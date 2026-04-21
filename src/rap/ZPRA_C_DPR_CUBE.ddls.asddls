@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Production - Analytical Cube'
@Metadata.ignorePropagatedAnnotations: true

/* ── Analytical Cube annotations ─────────────────────────────────────────── */
@Analytics.dataCategory: #CUBE
@Analytics.internalName: #LOCAL

/* ── OData exposure ──────────────────────────────────────────────────────── */
@OData.entityType.name: 'DPRProductionCubeType'

define view entity ZPRA_C_DPR_CUBE
  as select from ZPRA_I_DPR_DAILY as Daily

  /* Asset name text */
  left outer join zoiu_pr_dn as AssetTxt
    on Daily.Asset = AssetTxt.dn_no

  /* PI percentage — join on asset/block, filter in query */
  left outer join zpra_t_prd_pi as PI
    on  Daily.Asset           = PI.asset
    and Daily.Block           = PI.block
    and Daily.ProductionDate >= PI.vld_frm
    and Daily.ProductionDate <= PI.vld_to

{
  /* ── Dimensions ──────────────────────────────────────────────────────── */

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Production Date'
  @Semantics.calendar.date: true
  key Daily.ProductionDate                        as ProductionDate,

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Calendar Year'
  key Daily.CalendarYear                          as CalendarYear,

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Calendar Month'
  key Daily.CalendarMonth                         as CalendarMonth,

  @AnalyticsDetails.query.axis: #ROWS
  @EndUserText.label: 'Product Code'
  @ObjectModel.text.element: ['ProductDescription']
  key Daily.Product                               as Product,

  @EndUserText.label: 'Product'
  Daily.ProductDescription                        as ProductDescription,

  @AnalyticsDetails.query.axis: #ROWS
  @EndUserText.label: 'Asset'
  @ObjectModel.text.element: ['AssetDescription']
  key Daily.Asset                                 as Asset,

  @EndUserText.label: 'Asset Description'
  AssetTxt.dn_de                                  as AssetDescription,

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Block'
  key Daily.Block                                 as Block,

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Volume Type'
  @ObjectModel.text.element: ['VolumeTypeDescription']
  key Daily.VolumeType                            as VolumeType,

  @EndUserText.label: 'Volume Type Description'
  Daily.VolumeTypeDescription                     as VolumeTypeDescription,

  /* ── Measures ─────────────────────────────────────────────────────────── */

  @EndUserText.label: 'Production Qty (Primary UoM)'
  @Aggregation.default: #SUM
  @Semantics.quantity.unitOfMeasure: 'ProdUom1'
  Daily.ProdQty1                                  as ProdQty1,

  @EndUserText.label: 'Primary UoM'
  @Semantics.unitOfMeasure: true
  Daily.ProdUom1                                  as ProdUom1,

  @EndUserText.label: 'Production Qty (Secondary UoM)'
  @Aggregation.default: #SUM
  @Semantics.quantity.unitOfMeasure: 'ProdUom2'
  Daily.ProdQty2                                  as ProdQty2,

  @EndUserText.label: 'Secondary UoM'
  @Semantics.unitOfMeasure: true
  Daily.ProdUom2                                  as ProdUom2,

  /* ── PI (Participating Interest) ─────────────────────────────────────── */
  @EndUserText.label: 'Participating Interest %'
  @Aggregation.default: #NOP
  PI.pi                                           as ParticipatingInterest,

  /* OVL share = ProdQty1 × PI / 100 */
  @EndUserText.label: 'OVL Share Qty (Primary UoM)'
  @Aggregation.default: #SUM
  @Semantics.quantity.unitOfMeasure: 'ProdUom1'
  cast(
    Daily.ProdQty1 * PI.pi / cast( 100 as abap.dec(5,2) )
    as abap.dec(23,3)
  )                                               as OvlShareQty1,

  @EndUserText.label: 'OVL Share Qty (Secondary UoM)'
  @Aggregation.default: #SUM
  @Semantics.quantity.unitOfMeasure: 'ProdUom2'
  cast(
    Daily.ProdQty2 * PI.pi / cast( 100 as abap.dec(5,2) )
    as abap.dec(23,3)
  )                                               as OvlShareQty2
}
where Daily.VolumeType = 'NET_PROD'   /* Default: Net Production only */
