@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Production Query - By Product & Asset'
@Metadata.ignorePropagatedAnnotations: true

/* ── Analytical Query ─────────────────────────────────────────────────────── */
@Analytics.query: true
@Analytics.settings.maxResultSize: #UNLIMITED

/* ── OData V4 ────────────────────────────────────────────────────────────── */
@OData.entityType.name: 'DPRProductionQueryType'

define view entity ZPRA_Q_DPR_PROD_QUERY
  with parameters
    /* Filter to a date range; defaults to current month if omitted */
    P_DateFrom : zpra_t_dly_prd-production_date,
    P_DateTo   : zpra_t_dly_prd-production_date

  as select from ZPRA_C_DPR_CUBE

{
  /* ── Row dimensions ───────────────────────────────────────────────────── */

  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #SHOW
  Product,

  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #SHOW
  ProductDescription,

  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #SHOW
  Asset,

  @AnalyticsDetails.query.axis: #ROWS
  AssetDescription,

  @AnalyticsDetails.query.axis: #ROWS
  Block,

  /* ── Free (filter) dimensions ────────────────────────────────────────── */

  @AnalyticsDetails.query.axis: #FREE
  ProductionDate,

  @AnalyticsDetails.query.axis: #FREE
  CalendarYear,

  @AnalyticsDetails.query.axis: #FREE
  CalendarMonth,

  @AnalyticsDetails.query.axis: #FREE
  VolumeType,

  @AnalyticsDetails.query.axis: #FREE
  VolumeTypeDescription,

  /* ── Column measures ─────────────────────────────────────────────────── */

  @AnalyticsDetails.query.axis: #COLUMNS
  @EndUserText.label: 'JV Production (Primary UoM)'
  ProdQty1,

  ProdUom1,

  @AnalyticsDetails.query.axis: #COLUMNS
  @EndUserText.label: 'JV Production (Secondary UoM)'
  ProdQty2,

  ProdUom2,

  @AnalyticsDetails.query.axis: #COLUMNS
  @EndUserText.label: 'OVL Share (Primary UoM)'
  OvlShareQty1,

  @AnalyticsDetails.query.axis: #COLUMNS
  @EndUserText.label: 'OVL Share (Secondary UoM)'
  OvlShareQty2,

  @AnalyticsDetails.query.axis: #COLUMNS
  @EndUserText.label: 'PI %'
  ParticipatingInterest
}
where ProductionDate >= $parameters.P_DateFrom
  and ProductionDate <= $parameters.P_DateTo
