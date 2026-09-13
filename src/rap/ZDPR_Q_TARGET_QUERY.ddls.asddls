@AbapCatalog.sqlViewName: 'ZDPRQTARGETQRY'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Target vs Actual Query'
@Metadata.ignorePropagatedAnnotations: true

@Analytics.query: true
@OData.publish: true
@Metadata.allowExtensions: true

/*
 * This query joins monthly reconciled production with production targets,
 * enabling target vs. actual variance analysis.
 * Parameters filter by fiscal year and target type.
 */
define view ZDPR_Q_TARGET_QUERY
  with parameters
    P_FiscalYear : gjahr,
    P_TargetCode : char10    /* e.g. TAR_BE, TAR_RE */

  as select from ZDPR_I_MONTHLY as Actual

  left outer join ZDPR_I_TARGET as Target
    on  Actual.FiscalYear   = Target.FiscalYear
    and Actual.FiscalPeriod = Target.FiscalPeriod
    and Actual.Asset        = Target.Asset
    and Actual.Block        = Target.Block
    and Actual.Product      = Target.Product
    and Actual.VolumeType   = Target.VolumeType
    and Target.TargetCode   = $parameters.P_TargetCode

{
  /* ── Dimensions ──────────────────────────────────────────────────────── */

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Fiscal Year'
  key Actual.FiscalYear                           as FiscalYear,

  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #SHOW
  @EndUserText.label: 'Fiscal Period'
  key Actual.FiscalPeriod                         as FiscalPeriod,

  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #SHOW
  @EndUserText.label: 'Product'
  @ObjectModel.text.element: ['ProductDescription']
  key Actual.Product                              as Product,

  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #SHOW
  @EndUserText.label: 'Asset'
  @ObjectModel.text.element: ['AssetDescription']
  key Actual.Asset                                as Asset,

  @AnalyticsDetails.query.axis: #FREE
  key Actual.Block                                as Block,

  @AnalyticsDetails.query.axis: #FREE
  key Actual.VolumeType                           as VolumeType,

  /* Texts (after the keys - key fields must be contiguous at the top) */
  @EndUserText.label: 'Product Description'
  Actual.ProductDescription                       as ProductDescription,

  @EndUserText.label: 'Asset'
  cast( Actual._AssetText.dn_de as abap.char( 80 ) ) as AssetDescription,

  @AnalyticsDetails.query.axis: #FREE
  Actual.VolumeTypeDescription                    as VolumeTypeDescription,

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Target Type'
  Target.TargetCode                               as TargetCode,

  @AnalyticsDetails.query.axis: #FREE
  Target.TargetTypeDescription                    as TargetTypeDescription,

  /* ── Measures ─────────────────────────────────────────────────────────── */

  @AnalyticsDetails.query.axis: #COLUMNS
  @EndUserText.label: 'Actual Production'
  @Aggregation.default: #SUM
  @Semantics.quantity.unitOfMeasure: 'ActualUom'
  Actual.ProdQty1                                 as ActualQty,
  Actual.ProdUom1                                 as ActualUom,

  @AnalyticsDetails.query.axis: #COLUMNS
  @EndUserText.label: 'Target Quantity'
  @Aggregation.default: #SUM
  @Semantics.quantity.unitOfMeasure: 'TargetUom'
  Target.TargetQty                                as TargetQty,
  Target.TargetUom                                as TargetUom,

  @AnalyticsDetails.query.axis: #COLUMNS
  @EndUserText.label: 'Variance (Actual - Target)'
  @Aggregation.default: #SUM
  @Semantics.quantity.unitOfMeasure: 'ActualUom'
  cast(
    Actual.ProdQty1 - Target.TargetQty
    as abap.dec(23,3)
  )                                               as VarianceQty,

  @AnalyticsDetails.query.axis: #COLUMNS
  @EndUserText.label: 'Achievement %'
  /* ratio: AVG of row percentages (OData V4 has no FORMULA/NOP) */
  @Aggregation.default: #AVG
  cast(
    case
      when Target.TargetQty <> 0
      then Actual.ProdQty1 * cast( 100 as abap.dec(5,2) ) / Target.TargetQty
      else cast( 0 as abap.dec(5,2) )
    end
    as abap.dec(7,2)
  )                                               as AchievementPct
}
where Actual.FiscalYear = $parameters.P_FiscalYear
