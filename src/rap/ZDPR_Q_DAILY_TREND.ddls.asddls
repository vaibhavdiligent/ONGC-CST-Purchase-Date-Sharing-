@AbapCatalog.sqlViewName: 'ZDPRQDAILYTREND'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Daily Production Trend Query'
@Metadata.ignorePropagatedAnnotations: true

/*
 * Analytical query for time-series trend charts.
 * Aggregates production by date + product, suitable for
 * line/bar chart rendering in Fiori Analytical List Page.
 */
@Analytics.query: true
@OData.publish: true
@Metadata.allowExtensions: true

define view ZDPR_Q_DAILY_TREND
  with parameters
    P_DateFrom : datum,
    P_DateTo   : datum

  as select from ZDPR_C_PROD_CUBE

{
  /* ── Time axis (X-axis for chart) ─────────────────────────────────────── */
  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #HIDE
  ProductionDate,

  @AnalyticsDetails.query.axis: #FREE
  CalendarYear,

  @AnalyticsDetails.query.axis: #FREE
  CalendarMonth,

  /* ── Product breakdown (series for chart) ────────────────────────────── */
  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #SHOW
  Product,

  @AnalyticsDetails.query.axis: #ROWS
  ProductDescription,

  /* ── Free filters ─────────────────────────────────────────────────────── */
  @AnalyticsDetails.query.axis: #FREE
  Asset,

  @AnalyticsDetails.query.axis: #FREE
  AssetDescription,

  @AnalyticsDetails.query.axis: #FREE
  Block,

  /* ── Measures (Y-axis for chart) ──────────────────────────────────────── */
  @AnalyticsDetails.query.axis: #COLUMNS
  @EndUserText.label: 'JV Daily Production'
  ProdQty1,

  ProdUom1,

  @AnalyticsDetails.query.axis: #COLUMNS
  @EndUserText.label: 'OVL Share Production'
  OvlShareQty1,

  @AnalyticsDetails.query.axis: #COLUMNS
  ProdQty2,

  ProdUom2,

  OvlShareQty2
}
where ProductionDate >= $parameters.P_DateFrom
  and ProductionDate <= $parameters.P_DateTo
