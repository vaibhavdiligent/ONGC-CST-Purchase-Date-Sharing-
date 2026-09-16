@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Target rows scaled (bbl/MMSCM/BOE)'
@Metadata.ignorePropagatedAnnotations: true

/* ── Production target rows, scaled like the classic DPR report ────────────
 * tar_qty in ZPRA_T_PRD_TAR is a MONTHLY figure in MMT (oil family) / BCM
 * (gas). FORM convert_target_units of the classic report (BOPD/MMSCMD mode)
 * scales it before use:
 *   oil family : tar_qty * 1,000,000 * conv_factor  (MMT -> barrels; the
 *                factor is read from ZPRA_T_TAR_CF by gjahr/asset/block/
 *                product; no row -> 0, as in the report)
 *   gas        : tar_qty * 1,000                    (BCM -> MMSCM)
 *                and * 6290 on top for barrels of oil equivalent
 * Also supplies the fiscal-year calendar (FY April..March):
 *   FiscalYearStart  = 1 April gjahr
 *   DaysInFiscalYear = 365 / 366 - the divisor that turns the annual target
 *                      into the flat "BE Target" daily rate of the graph
 * Plain columns only, so ZDPR_I_TARGET_FY can SUM / GROUP BY them without
 * CASE expressions (rejected by the target release inside aggregates).
 * Stand-alone on the tables on purpose: it does not depend on ZDPR_I_TARGET.
 * ─────────────────────────────────────────────────────────────────────────── */
define view entity ZDPR_P_TARGET_ROW
  as select from zpra_t_prd_tar as PrdTar

  left outer join zpra_t_tar_cf as Cf
    on  Cf.gjahr   = PrdTar.gjahr
    and Cf.asset   = PrdTar.asset
    and Cf.block   = PrdTar.block
    and Cf.product = PrdTar.product

{
  key PrdTar.tar_code                     as TargetCode,
  key PrdTar.gjahr                        as FiscalYear,
  key PrdTar.monat                        as FiscalPeriod,
  key PrdTar.asset                        as Asset,
  key PrdTar.block                        as Block,
  key PrdTar.product                      as Product,
  key PrdTar.prod_vl_type_cd              as VolumeType,

      PrdTar.tar_qty                      as TargetQty,
      PrdTar.uom                          as TargetUom,

      case PrdTar.product
        when '722000004' then 'GAS'
        else                  'OIL'
      end                                 as ProductGroup,

      /* MMT -> barrels factor of the fiscal year (0 when missing) */
      cast( coalesce( cast( Cf.conv_factor as abap.dec( 15, 7 ) ),
                      cast( 0 as abap.dec( 15, 7 ) ) )
            as abap.dec( 15, 7 ) )        as ConversionFactor,

      /* monthly target volume: barrels (oil family) / MMSCM (gas) */
      cast( case PrdTar.product
              when '722000004' then PrdTar.tar_qty * cast( 1000 as abap.dec( 10, 0 ) )
              else                  PrdTar.tar_qty * cast( 1000000 as abap.dec( 10, 0 ) )
                                    * coalesce( cast( Cf.conv_factor as abap.dec( 15, 7 ) ),
                                                cast( 0 as abap.dec( 15, 7 ) ) )
            end as abap.dec( 23, 3 ) )    as TargetVolume,

      /* monthly target in barrels of oil equivalent (gas: MMSCM x 6290) */
      cast( case PrdTar.product
              when '722000004' then PrdTar.tar_qty * cast( 1000 as abap.dec( 10, 0 ) )
                                    * cast( 6290 as abap.dec( 5, 0 ) )
              else                  PrdTar.tar_qty * cast( 1000000 as abap.dec( 10, 0 ) )
                                    * coalesce( cast( Cf.conv_factor as abap.dec( 15, 7 ) ),
                                                cast( 0 as abap.dec( 15, 7 ) ) )
            end as abap.dec( 23, 3 ) )    as TargetBoe,

      /* 1 April of the fiscal year */
      cast( concat( cast( PrdTar.gjahr as abap.char( 4 ) ), '0401' )
            as abap.dats )                as FiscalYearStart,

      /* days from 1 April gjahr to 1 April gjahr+1 = 365 / 366 */
      cast( dats_days_between(
              cast( concat( cast( PrdTar.gjahr as abap.char( 4 ) ), '0401' ) as abap.dats ),
              dats_add_months(
                cast( concat( cast( PrdTar.gjahr as abap.char( 4 ) ), '0401' ) as abap.dats ),
                12, 'INITIAL' ) )
            as abap.dec( 5, 0 ) )         as DaysInFiscalYear
}
