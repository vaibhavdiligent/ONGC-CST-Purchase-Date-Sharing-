@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Day Base - unit-normalized daily production'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType: {
  serviceQuality: #A,
  sizeCategory:   #L,
  dataClass:      #TRANSACTIONAL
}

/* ── Performance-oriented base layer ────────────────────────────────────────
 * Computes, ONCE per daily record (all pushed down to HANA, no joins except
 * the PI lookup):
 *   - QtyNative : BOPD for oil family, MMSCMD for gas (MCM as-is, MCF/35.3,
 *                 M3/1e6) - the unit logic of the classic DPR (v2.8 fix)
 *   - BoeFactor : 1 for oil family, 6290 for gas (BOEPD conversion)
 *   - PiPct     : participating interest valid on the date (0 if none)
 *   - FiscalYear/FiscalPeriod of the production date (FY April..March)
 * The BOEPD cube joins targets on PLAIN EQUALITY over these derived columns,
 * so the expensive CASE logic is evaluated once here instead of inside join
 * conditions - keeps HANA join pruning effective on large date ranges.
 * ─────────────────────────────────────────────────────────────────────────── */
define view entity ZPRA_P_DPR_DAY_BASE
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

      /* Oil-family vs Gas (Excel tab-3 column groups) */
      case D.product
        when '722000004' then 'GAS'
        else                  'OIL'
      end                                             as ProductGroup,

      /* Fiscal year/period of the date (FY = April..March) */
      case when month( D.production_date ) >= 4
           then cast( year( D.production_date )     as abap.numc(4) )
           else cast( year( D.production_date ) - 1 as abap.numc(4) )
      end                                             as FiscalYear,

      case when month( D.production_date ) >= 4
           then cast( month( D.production_date ) - 3 as abap.numc(2) )
           else cast( month( D.production_date ) + 9 as abap.numc(2) )
      end                                             as FiscalPeriod,

      /* Native daily figure: BOPD (oil family) / MMSCMD (gas) */
      cast(
        case D.product
          when '722000004' then
            case D.prod_vl_uom1
              when 'MCF' then D.prod_vl_qty1 / cast( '35.3' as abap.dec( 4, 1 ) )
              when 'M3'  then D.prod_vl_qty1 / cast( 1000000 as abap.dec( 10, 0 ) )
              else            D.prod_vl_qty1
            end
          else D.prod_vl_qty1
        end as abap.dec( 23, 7 )
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
where D.prd_vl_type = 'NET_PROD'
