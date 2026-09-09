@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Day Base - normalized daily prod'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType: {
  serviceQuality: #A,
  sizeCategory:   #L,
  dataClass:      #TRANSACTIONAL
}

/* ── Performance-oriented base layer ────────────────────────────────────────
 * Computes, ONCE per daily record (all pushed down to HANA, no joins except
 * the PI lookup):
 *   - QtyNative : signed daily figure -
 *                 Oil family : NET_PROD in BOPD, as-is
 *                 Gas        : GROSS_PROD minus GAS_INJ (gas has NO NET_PROD
 *                              rows in ZPRA_T_DLY_PRD - verified on the full
 *                              table dump), converted to MMSCMD
 *                              (MCM as-is, MCF/35.3, M3/1e6)
 *   - BoeFactor : 1 for oil family, 6290 for gas (BOEPD conversion)
 *   - PiPct     : participating interest valid on the date (0 if none)
 *   - FiscalYear/FiscalPeriod of the production date (FY April..March)
 *   - BusinessUnit derived from the asset-code country prefix (whitelist:
 *     anything unknown maps to OTHER and can be filtered by consumers -
 *     also screens out test entries like A001/PCB/SCB in the table)
 * The BOEPD cube joins targets on PLAIN EQUALITY over these derived columns,
 * so the expensive CASE logic is evaluated once here instead of inside join
 * conditions - keeps HANA join pruning effective on large date ranges.
 * ─────────────────────────────────────────────────────────────────────────── */
define view entity ZDPR_P_DAY_BASE
  as select from zpra_t_dly_prd as D

  left outer join zpra_t_prd_pi as PI
    on  D.asset            = PI.asset
    and D.block            = PI.block
    and D.production_date >= PI.vld_frm
    and D.production_date <= PI.vld_to

{
  key D.production_date                               as ProductionDate,
  key D.asset                                         as Asset,
  key D.block                                         as Block,
  key D.product                                       as Product,
  key D.prd_vl_type                                   as VolumeType,

      /* Oil-family vs Gas (Excel tab-3 column groups) */
      case D.product
        when '722000004' then 'GAS'
        else                  'OIL'
      end                                             as ProductGroup,

      /* Business Unit from the asset-code country prefix (dashboard groups).
         Unknown prefixes -> 'OTHER' (test/garbage rows filter). */
      case substring( D.asset, 1, 3 )
        when 'RUS' then 'BU-RUSSIA'
        when 'BRA' then 'BU-LAC'
        when 'COL' then 'BU-LAC'
        when 'VEN' then 'BU-LAC'
        when 'MMR' then 'BU-ASIA PACIFIC'
        when 'VNM' then 'BU-ASIA PACIFIC'
        when 'AZE' then 'BU-MENA CIS'
        when 'SSU' then 'BU-MENA CIS'
        when 'SUD' then 'BU-MENA CIS'
        when 'UAE' then 'BU-MENA CIS'
        else            'OTHER'
      end                                             as BusinessUnit,

      /* Fiscal year/period (FY = April..March): shifting the date back
         3 months makes its calendar year/month exactly gjahr/monat */
      cast( substring( dats_add_months( D.production_date, -3, 'INITIAL' ), 1, 4 )
            as abap.numc(4) )                         as FiscalYear,

      cast( substring( dats_add_months( D.production_date, -3, 'INITIAL' ), 5, 2 )
            as abap.numc(2) )                         as FiscalPeriod,

      /* Signed native daily figure: BOPD (oil family, NET_PROD) or MMSCMD
         (gas: +GROSS_PROD / -GAS_INJ, so SUM() nets automatically) */
      cast(
        ( case D.product
            when '722000004' then
              case D.prod_vl_uom1
                when 'MCF' then D.prod_vl_qty1 / cast( '35.3' as abap.dec( 4, 1 ) )
                when 'M3'  then D.prod_vl_qty1 / cast( 1000000 as abap.dec( 10, 0 ) )
                else            D.prod_vl_qty1
              end
            else D.prod_vl_qty1
          end )
        * ( case D.prd_vl_type
              when 'GAS_INJ' then cast( -1 as abap.dec( 2, 0 ) )
              else                cast(  1 as abap.dec( 2, 0 ) )
            end )
        as abap.dec( 23, 7 )
      )                                               as QtyNative,

      /* BOE factor: gas MMSCMD -> BOEPD */
      cast(
        case D.product
          when '722000004' then 6290
          else                  1
        end as abap.dec( 5, 0 )
      )                                               as BoeFactor,

      /* PI share % (0 when no PI row - OVL measures then contribute 0,
         matching the classic report's OVL-total behaviour) */
      cast( coalesce( PI.pi, cast( 0 as abap.dec( 5, 2 ) ) )
            as abap.dec( 5, 2 ) )                     as PiPct
}
/* Oil family counts NET_PROD; gas has no NET_PROD rows - net gas is
   GROSS_PROD - GAS_INJ, exactly as the classic DPR computes it.
   (GAS_FLARE / INT_CONS / WATER_INJ etc. are intentionally excluded.) */
where
     (  D.product <> '722000004'
    and D.prd_vl_type = 'NET_PROD' )
  or (  D.product =  '722000004'
    and D.prd_vl_type = 'GROSS_PROD' )
  or (  D.product =  '722000004'
    and D.prd_vl_type = 'GAS_INJ' )
