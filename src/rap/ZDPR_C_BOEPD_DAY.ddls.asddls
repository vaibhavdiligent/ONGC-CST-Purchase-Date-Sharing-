@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR BOEPD Day Cube - Actual vs Target'
@Metadata.ignorePropagatedAnnotations: true

/* ── Analytical Cube (performance-first) ────────────────────────────────────
 * Data behind Excel tab 2 (graph: "Actual Production" vs "BE Target" lines)
 * and tab 3 (Production Performance). Plain select on ZDPR_P_BOEPD_ROWS:
 *   RowType 'A' - one row per daily production record (ZDPR_P_DAY_BASE:
 *                 unit/sign logic, gas = GROSS_PROD - GAS_INJ, PI %)
 *   RowType 'T' - one row per date x asset/block/product carrying the BE
 *                 target daily rate of the date's fiscal year
 *                 (ZDPR_P_TARGET_DAY: annual TAR_BE volume / days in FY,
 *                 scaled exactly like the classic DPR report)
 * SUM over a date therefore yields the "Actual Production" value and the
 * flat "BE Target" value of the DPR graph. No join, no CASE - aggregation
 * fully pushes down to HANA; queries on top take mandatory date-range
 * parameters, keeping scans bounded.
 * ─────────────────────────────────────────────────────────────────────────── */
@Analytics.dataCategory: #CUBE
@Analytics.internalName: #LOCAL

@OData.entityType.name: 'DPRBoepdDayCubeType'

define view entity ZDPR_C_BOEPD_DAY
  as select from ZDPR_P_BOEPD_ROWS as Row

  association [0..1] to zoiu_pr_dn as _AssetText
    on $projection.Asset = _AssetText.dn_no

{
  /* ── Dimensions ─────────────────────────────────────────────────────── */
  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Row Type (A=Actual, T=Target)'
  key Row.RowType                                     as RowType,

  @AnalyticsDetails.query.axis: #ROWS
  @EndUserText.label: 'Production Date'
  key Row.ProductionDate                              as ProductionDate,

  @AnalyticsDetails.query.axis: #FREE
  @ObjectModel.text.association: '_AssetText'
  key Row.Asset                                       as Asset,

  @AnalyticsDetails.query.axis: #FREE
  key Row.Block                                       as Block,

  @AnalyticsDetails.query.axis: #FREE
  key Row.Product                                     as Product,

  @AnalyticsDetails.query.axis: #FREE
  key Row.VolumeType                                  as VolumeType,

  /* Readable date for chart category axes (Fiori renders a DATS as the raw
     YYYYMMDD string). ISO layout keeps the alphabetical order = date order. */
  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Date'
  concat( substring( cast( Row.ProductionDate as abap.char( 8 ) ), 1, 4 ),
    concat( '-',
      concat( substring( cast( Row.ProductionDate as abap.char( 8 ) ), 5, 2 ),
        concat( '-',
          substring( cast( Row.ProductionDate as abap.char( 8 ) ), 7, 2 ) ) ) ) )
                                                      as ProductionDateText,

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Product Group'
  Row.ProductGroup                                    as ProductGroup,

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Business Unit'
  Row.BusinessUnit                                    as BusinessUnit,

  @AnalyticsDetails.query.axis: #FREE
  Row.FiscalYear                                      as FiscalYear,

  @AnalyticsDetails.query.axis: #FREE
  Row.FiscalPeriod                                    as FiscalPeriod,

  /* ── Actual measures (signed: gas GROSS positive, GAS_INJ negative) ─── */
  @EndUserText.label: 'Actual Qty JV (BOPD / MMSCMD)'
  @Aggregation.default: #SUM
  Row.ActualQtyJv                                     as ActualQtyJv,

  @EndUserText.label: 'Actual Qty OVL (BOPD / MMSCMD)'
  @Aggregation.default: #SUM
  Row.ActualQtyOvl                                    as ActualQtyOvl,

  @EndUserText.label: 'Actual BOEPD JV'
  @Aggregation.default: #SUM
  Row.ActualBoepdJv                                   as ActualBoepdJv,

  /* "Actual Production" line of Excel tab 2 (OVL share, BOEPD) */
  @EndUserText.label: 'Actual BOEPD (OVL)'
  @Aggregation.default: #SUM
  Row.ActualBoepdOvl                                  as ActualBoepdOvl,

  /* ── BE Target measures ("BE Target" flat line / tab-3 rows) ────────── */
  @EndUserText.label: 'BE Target Qty (BOPD / MMSCMD)'
  @Aggregation.default: #SUM
  Row.TargetQty                                       as TargetQty,

  @EndUserText.label: 'BE Target BOEPD'
  @Aggregation.default: #SUM
  Row.TargetBoepd                                     as TargetBoepd,

  _AssetText
}
