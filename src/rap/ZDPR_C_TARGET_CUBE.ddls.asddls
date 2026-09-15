@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Target vs Actual - Analytical Cube'
@Metadata.ignorePropagatedAnnotations: true

/* ── Analytical cube for the Target vs Actual query ──────────────────────────
 * An analytical query may select from exactly ONE cube and may not join, so
 * the join of monthly reconciled production with the production targets
 * lives here. The target code is a cube parameter (used in the join
 * condition) which the query passes through from its own parameter.
 * Every DEC element is a measure: the analytic engine does not accept DEC
 * characteristics. */
@Analytics.dataCategory: #CUBE
@Analytics.internalName: #LOCAL

define view entity ZDPR_C_TARGET_CUBE
  with parameters
    P_TargetCode : char10

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
  /* ── Dimensions ─────────────────────────────────────────────────────── */
  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Fiscal Year'
  key Actual.FiscalYear                              as FiscalYear,

  @AnalyticsDetails.query.axis: #ROWS
  @EndUserText.label: 'Fiscal Period'
  key Actual.FiscalPeriod                            as FiscalPeriod,

  @AnalyticsDetails.query.axis: #ROWS
  @EndUserText.label: 'Product'
  @ObjectModel.text.element: ['ProductDescription']
  key Actual.Product                                 as Product,

  @AnalyticsDetails.query.axis: #ROWS
  @EndUserText.label: 'Asset'
  @ObjectModel.text.element: ['AssetDescription']
  key Actual.Asset                                   as Asset,

  @AnalyticsDetails.query.axis: #FREE
  key Actual.Block                                   as Block,

  @AnalyticsDetails.query.axis: #FREE
  key Actual.VolumeType                              as VolumeType,

  @EndUserText.label: 'Product Description'
  Actual.ProductDescription                          as ProductDescription,

  /* rtrim() yields a plain string without the OIUNM conversion exit */
  @EndUserText.label: 'Asset Description'
  rtrim( Actual._AssetText.dn_de, ' ' )              as AssetDescription,

  @AnalyticsDetails.query.axis: #FREE
  Actual.VolumeTypeDescription                       as VolumeTypeDescription,

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Target Type'
  $parameters.P_TargetCode                           as TargetCode,

  @AnalyticsDetails.query.axis: #FREE
  Target.TargetTypeDescription                       as TargetTypeDescription,

  /* ── Measures ───────────────────────────────────────────────────────── */
  @EndUserText.label: 'Actual Production'
  @Aggregation.default: #SUM
  @Semantics.quantity.unitOfMeasure: 'ActualUom'
  Actual.ProdQty1                                    as ActualQty,
  Actual.ProdUom1                                    as ActualUom,

  @EndUserText.label: 'Target Quantity'
  @Aggregation.default: #SUM
  @Semantics.quantity.unitOfMeasure: 'TargetUom'
  cast( coalesce( Target.TargetQty, cast( 0 as abap.dec( 23, 3 ) ) )
        as abap.dec( 23, 3 ) )                       as TargetQty,
  Target.TargetUom                                   as TargetUom,

  @EndUserText.label: 'Variance (Actual - Target)'
  @Aggregation.default: #SUM
  @Semantics.quantity.unitOfMeasure: 'ActualUom'
  cast( Actual.ProdQty1
        - coalesce( Target.TargetQty, cast( 0 as abap.dec( 23, 3 ) ) )
        as abap.dec( 23, 3 ) )                       as VarianceQty
}
/* Achievement % is NOT a cube measure: the analytic engine accepts only
   SUM/MIN/MAX in a cube. The ratio is a FORMULA in ZDPR_Q_TARGET_QUERY,
   computed after aggregation from ActualQty and TargetQty. */
