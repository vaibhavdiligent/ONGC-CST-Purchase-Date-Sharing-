@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Production Targets - Interface View'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType: {
  serviceQuality: #A,
  sizeCategory:   #M,
  dataClass:      #MASTER
}

/* ── Production targets (ZPRA_T_PRD_TAR) ─────────────────────────────────────
 * tar_qty is stored per fiscal month as MMT (oil family) / BCM (gas).
 * The classic DPR report (FORM convert_target_units, BOPD/MMSCMD mode) scales
 * it to a volume before use:
 *   oil family : tar_qty * 1,000,000 * conv_factor   (MMT  -> barrels, the
 *                factor comes from ZPRA_T_TAR_CF for the target's fiscal year)
 *   gas        : tar_qty * 1,000                     (BCM  -> MMSCM)
 *                and * 6290 on top for the BOE grand total
 * Those scaled figures are exposed here as plain columns (TargetVolume /
 * TargetBoe) so the aggregation views above only SUM plain fields.
 * The fiscal year runs April..March: FiscalYearStart = 1 April gjahr and
 * DaysInFiscalYear = 365 / 366 - the divisor the classic report uses to turn
 * the annual target into the flat "BE Target" daily rate.
 * ─────────────────────────────────────────────────────────────────────────── */
define view entity ZDPR_I_TARGET
  as select from zpra_t_prd_tar as PrdTar

  /* conversion factor of the target's fiscal year (classic: gt_zpra_t_tar_cf
     read with gjahr / asset / block / product) */
  left outer join zpra_t_tar_cf as Cf
    on  Cf.gjahr   = PrdTar.gjahr
    and Cf.asset   = PrdTar.asset
    and Cf.block   = PrdTar.block
    and Cf.product = PrdTar.product

  association [0..1] to zoiu_pr_dn as _AssetText
    on $projection.Asset = _AssetText.dn_no

{
  key PrdTar.tar_code                     as TargetCode,
  key PrdTar.gjahr                        as FiscalYear,
  key PrdTar.monat                        as FiscalPeriod,
  key PrdTar.asset                        as Asset,
  key PrdTar.block                        as Block,
  key PrdTar.product                      as Product,
  key PrdTar.prod_vl_type_cd              as VolumeType,

      @Semantics.quantity.unitOfMeasure: 'TargetUom'
      PrdTar.tar_qty                      as TargetQty,
      PrdTar.uom                          as TargetUom,

      @Semantics.quantity.unitOfMeasure: 'TargetUom'
      PrdTar.tar_qty                     as TargetQty2,

      /* Target type description */
      case PrdTar.tar_code
        when 'TAR_BE' then 'Budget Estimate'
        when 'TAR_IN' then 'Internal Target'
        when 'TAR_EX' then 'MOU Excellent'
        when 'TAR_VG' then 'MOU Very Good'
        when 'TAR_PC' then 'Physical Control'
        when 'TAR_RE' then 'Revised Estimate'
        else PrdTar.tar_code
      end                                 as TargetTypeDescription,

      case PrdTar.product
        when '722000001' then 'Oil'
        when '722000003' then 'Condensate'
        when '722000004' then 'Gas'
        when '722000005' then 'LNG'
        else                  'Other'
      end                                 as ProductDescription,

      /* Plain columns so aggregation views need no CASE in sums/group by */
      case PrdTar.product
        when '722000004' then 'GAS'
        else                  'OIL'
      end                                 as ProductGroup,

      /* Conversion factor MMT -> barrels of the fiscal year (0 when no
         ZPRA_T_TAR_CF row exists - same result as the classic report) */
      cast( coalesce( cast( Cf.conv_factor as abap.dec( 15, 7 ) ),
                      cast( 0 as abap.dec( 15, 7 ) ) )
            as abap.dec( 15, 7 ) )        as ConversionFactor,

      /* Monthly target volume in native units:
         barrels (oil family) / MMSCM (gas) */
      cast( case PrdTar.product
              when '722000004' then PrdTar.tar_qty * cast( 1000 as abap.dec( 10, 0 ) )
              else                  PrdTar.tar_qty * cast( 1000000 as abap.dec( 10, 0 ) )
                                    * coalesce( cast( Cf.conv_factor as abap.dec( 15, 7 ) ),
                                                cast( 0 as abap.dec( 15, 7 ) ) )
            end as abap.dec( 23, 3 ) )    as TargetVolume,

      /* Monthly target volume in barrels of oil equivalent
         (gas: MMSCM x 6290) */
      cast( case PrdTar.product
              when '722000004' then PrdTar.tar_qty * cast( 1000 as abap.dec( 10, 0 ) )
                                    * cast( 6290 as abap.dec( 5, 0 ) )
              else                  PrdTar.tar_qty * cast( 1000000 as abap.dec( 10, 0 ) )
                                    * coalesce( cast( Cf.conv_factor as abap.dec( 15, 7 ) ),
                                                cast( 0 as abap.dec( 15, 7 ) ) )
            end as abap.dec( 23, 3 ) )    as TargetBoe,

      /* 1 April of the fiscal year (FY April..March) */
      cast( concat( cast( PrdTar.gjahr as abap.char( 4 ) ), '0401' )
            as abap.dats )                as FiscalYearStart,

      /* days in the fiscal year: 1 April gjahr .. 31 March gjahr+1 (365/366) */
      cast( dats_days_between(
              cast( concat( cast( PrdTar.gjahr as abap.char( 4 ) ), '0401' ) as abap.dats ),
              dats_add_months(
                cast( concat( cast( PrdTar.gjahr as abap.char( 4 ) ), '0401' ) as abap.dats ),
                12, 'INITIAL' ) )
            as abap.dec( 5, 0 ) )         as DaysInFiscalYear,

      _AssetText
}
