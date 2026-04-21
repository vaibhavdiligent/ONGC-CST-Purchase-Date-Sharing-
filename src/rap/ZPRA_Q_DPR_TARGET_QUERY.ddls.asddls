@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Target vs Actual Query'
@Metadata.ignorePropagatedAnnotations: true

@Analytics.query: true
@Analytics.settings.maxResultSize: #UNLIMITED

@OData.entityType.name: 'DPRTargetQueryType'

/*
 * This query joins monthly reconciled production with production targets,
 * enabling target vs. actual variance analysis.
 * Parameters filter by fiscal year and target type.
 */
define view entity ZPRA_Q_DPR_TARGET_QUERY
  with parameters
    P_FiscalYear : zpra_t_mrec_prd-gjahr,
    P_TargetCode : zpra_t_prd_tar-tar_code    /* e.g. TAR_BE, TAR_RE */

  as select from ZPRA_I_DPR_MONTHLY as Actual

  left outer join ZPRA_I_DPR_TARGET as Target
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

  @EndUserText.label: 'Product Description'
  Actual.ProductDescription                       as ProductDescription,

  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #SHOW
  @EndUserText.label: 'Asset'
  @ObjectModel.text.element: ['AssetDescription']
  key Actual.Asset                                as Asset,

  @EndUserText.label: 'Asset'
  Actual._AssetText.dn_de                         as AssetDescription,

  @AnalyticsDetails.query.axis: #FREE
  key Actual.Block                                as Block,

  @AnalyticsDetails.query.axis: #FREE
  key Actual.VolumeType                           as VolumeType,

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

  @Semantics.unitOfMeasure: true
  Actual.ProdUom1                                 as ActualUom,

  @AnalyticsDetails.query.axis: #COLUMNS
  @EndUserText.label: 'Target Quantity'
  @Aggregation.default: #SUM
  @Semantics.quantity.unitOfMeasure: 'TargetUom'
  Target.TargetQty                                as TargetQty,

  @Semantics.unitOfMeasure: true
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
  @Aggregation.default: #NOP
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
