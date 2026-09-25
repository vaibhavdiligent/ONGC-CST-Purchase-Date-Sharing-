@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR BOEPD rows - actuals and BE targets'
@Metadata.ignorePropagatedAnnotations: true

/* ── Row source of the BOEPD cube ───────────────────────────────────────────
 * Union of two row kinds, distinguished by RowType:
 *   'A' actual rows  from ZDPR_P_DAY_BASE  (target measures = 0)
 *   'T' target rows  from ZDPR_P_TARGET_DAY (actual measures = 0)
 * Keeping actuals and targets as separate rows (instead of a join) gives
 * the same flat "BE Target" line as the classic DPR graph: the target is
 * present on every date whether or not an asset reported production, and it
 * is never multiplied by the number of volume-type rows of a day.
 * Both branches cast every element to identical types (union rule).
 * ─────────────────────────────────────────────────────────────────────────── */
define view entity ZDPR_P_BOEPD_ROWS
  as select from ZDPR_P_DAY_BASE as Day

{
  key cast( 'A' as abap.char( 1 ) )                   as RowType,
  key Day.ProductionDate                              as ProductionDate,
  key Day.Asset                                       as Asset,
  key Day.Block                                       as Block,
  key Day.Product                                     as Product,
  /* literal CASE gives a fixed char(10) in both branches, independent of
     the DB field length (union branches must have identical types) */
  key case Day.VolumeType
        when 'NET_PROD'   then 'NET_PROD'
        when 'GROSS_PROD' then 'GROSS_PROD'
        when 'GAS_INJ'    then 'GAS_INJ'
        else                   'OTHER'
      end                                             as VolumeType,

      cast( Day.ProductGroup  as abap.char( 3 ) )     as ProductGroup,
      cast( Day.BusinessUnit  as abap.char( 15 ) )    as BusinessUnit,
      Day.FiscalYear                                  as FiscalYear,
      Day.FiscalPeriod                                as FiscalPeriod,

      /* actual measures */
      Day.QtyNative                                   as ActualQtyJv,

      cast( Day.QtyNative * Day.PiPct / cast( 100 as abap.dec( 5, 2 ) )
            as abap.dec( 23, 7 ) )                    as ActualQtyOvl,

      cast( Day.QtyNative * Day.BoeFactor
            as abap.dec( 23, 3 ) )                    as ActualBoepdJv,

      cast( Day.QtyNative * Day.BoeFactor * Day.PiPct / cast( 100 as abap.dec( 5, 2 ) )
            as abap.dec( 23, 3 ) )                    as ActualBoepdOvl,

      /* target measures: none on actual rows */
      cast( 0 as abap.dec( 23, 7 ) )                  as TargetQty,
      cast( 0 as abap.dec( 23, 3 ) )                  as TargetBoepd
}

union all

  select from ZDPR_P_TARGET_DAY as Tar

{
  key cast( 'T' as abap.char( 1 ) )                   as RowType,
  key Tar.ProductionDate                              as ProductionDate,
  key Tar.Asset                                       as Asset,
  key Tar.Block                                       as Block,
  key Tar.Product                                     as Product,
  key cast( 'TARGET' as abap.char( 10 ) )             as VolumeType,

      cast( Tar.ProductGroup  as abap.char( 3 ) )     as ProductGroup,
      cast( Tar.BusinessUnit  as abap.char( 15 ) )    as BusinessUnit,
      Tar.FiscalYear                                  as FiscalYear,
      Tar.FiscalPeriod                                as FiscalPeriod,

      /* actual measures: none on target rows */
      cast( 0 as abap.dec( 23, 7 ) )                  as ActualQtyJv,
      cast( 0 as abap.dec( 23, 7 ) )                  as ActualQtyOvl,
      cast( 0 as abap.dec( 23, 3 ) )                  as ActualBoepdJv,
      cast( 0 as abap.dec( 23, 3 ) )                  as ActualBoepdOvl,

      /* target measures */
      Tar.TargetQty                                   as TargetQty,
      Tar.TargetBoepd                                 as TargetBoepd
}
