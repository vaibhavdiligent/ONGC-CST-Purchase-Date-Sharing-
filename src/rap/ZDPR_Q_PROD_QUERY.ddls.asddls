@AbapCatalog.sqlViewName: 'ZDPRQPRODQUERY'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Production by Product and Asset'
@Metadata.ignorePropagatedAnnotations: true

/* ── Analytical Query ─────────────────────────────────────────────────────── */
@Analytics.query: true
@OData.publish: true
@Metadata.allowExtensions: true
/* ── OData V4 ────────────────────────────────────────────────────────────── */

define view ZDPR_Q_PROD_QUERY
  with parameters
    /* Filter to a date range; defaults to current month if omitted */
    P_DateFrom : datum,
    P_DateTo   : datum

  as select from ZDPR_C_PROD_CUBE

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
