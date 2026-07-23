@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR BOEPD per Day - Actual vs BE Target Cube'
@Metadata.ignorePropagatedAnnotations: true

/* ── Analytical Cube (performance-first) ────────────────────────────────────
 * Data behind Excel tab 2 (graph: "Actual Production" vs "BE Target" lines)
 * and tab 3 (Production Performance). All unit logic lives in
 * ZPRA_P_DPR_DAY_BASE (computed once per row); this cube only does
 *   - one EQUALITY join to the BE target (TAR_BE / NET_PROD) on
 *     asset/block/product/fiscal year/fiscal period
 *   - plain multiplications for the measures
 * so aggregation fully pushes down to HANA. Queries on top take mandatory
 * date-range parameters, keeping scans bounded.
 * ─────────────────────────────────────────────────────────────────────────── */
@Analytics.dataCategory: #CUBE
@Analytics.internalName: #LOCAL

@OData.entityType.name: 'DPRBoepdDayCubeType'

define view entity ZPRA_C_DPR_BOEPD_DAY
  as select from ZPRA_P_DPR_DAY_BASE as Day

  /* BE Target daily rate (tar_qty2, OVL level) of the fiscal month */
  left outer join zpra_t_prd_tar as Tar
    on  Tar.asset           = Day.Asset
    and Tar.block           = Day.Block
    and Tar.product         = Day.Product
    and Tar.gjahr           = Day.FiscalYear
    and Tar.monat           = Day.FiscalPeriod
    and Tar.tar_code        = 'TAR_BE'
    and Tar.prod_vl_type_cd = 'NET_PROD'

  association [0..1] to zoiu_pr_dn as _AssetText
    on $projection.Asset = _AssetText.dn_no

{
  /* ── Dimensions ─────────────────────────────────────────────────────── */
  @AnalyticsDetails.query.axis: #ROWS
  @EndUserText.label: 'Production Date'
  @Semantics.calendar.date: true
  key Day.ProductionDate                              as ProductionDate,

  @AnalyticsDetails.query.axis: #FREE
  @ObjectModel.text.association: '_AssetText'
  key Day.Asset                                       as Asset,

  @AnalyticsDetails.query.axis: #FREE
  key Day.Block                                       as Block,

  @AnalyticsDetails.query.axis: #FREE
  key Day.Product                                     as Product,

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Product Group'
  Day.ProductGroup                                    as ProductGroup,

  @AnalyticsDetails.query.axis: #FREE
  Day.FiscalYear                                      as FiscalYear,

  @AnalyticsDetails.query.axis: #FREE
  Day.FiscalPeriod                                    as FiscalPeriod,

  /* ── Actual measures ────────────────────────────────────────────────── */
  @EndUserText.label: 'Actual Qty JV (BOPD / MMSCMD)'
  @Aggregation.default: #SUM
  Day.QtyNative                                       as ActualQtyJv,

  @EndUserText.label: 'Actual Qty OVL (BOPD / MMSCMD)'
  @Aggregation.default: #SUM
  cast( Day.QtyNative * Day.PiPct / cast( 100 as abap.dec( 5, 2 ) )
        as abap.dec( 23, 7 ) )                        as ActualQtyOvl,

  @EndUserText.label: 'Actual BOEPD JV'
  @Aggregation.default: #SUM
  cast( Day.QtyNative * Day.BoeFactor
        as abap.dec( 23, 3 ) )                        as ActualBoepdJv,

  /* "Actual Production" line of Excel tab 2 (OVL share, BOEPD) */
  @EndUserText.label: 'Actual BOEPD (OVL)'
  @Aggregation.default: #SUM
  cast( Day.QtyNative * Day.BoeFactor * Day.PiPct / cast( 100 as abap.dec( 5, 2 ) )
        as abap.dec( 23, 3 ) )                        as ActualBoepdOvl,

  /* ── BE Target measures ("BE Target" flat line / tab-3 rows) ────────── */
  @EndUserText.label: 'BE Target Qty (BOPD / MMSCMD)'
  @Aggregation.default: #SUM
  coalesce( cast( Tar.tar_qty2 as abap.dec( 23, 7 ) ),
            cast( 0            as abap.dec( 23, 7 ) ) )
                                                      as TargetQty,

  @EndUserText.label: 'BE Target BOEPD'
  @Aggregation.default: #SUM
  cast( coalesce( cast( Tar.tar_qty2 as abap.dec( 23, 7 ) ),
                  cast( 0            as abap.dec( 23, 7 ) ) )
        * Day.BoeFactor as abap.dec( 23, 3 ) )        as TargetBoepd,

  _AssetText
}
