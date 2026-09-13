@AbapCatalog.sqlViewName: 'ZDPRQBOEPDTREND'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR BOEPD Trend - Actual vs BE Target'
@Metadata.ignorePropagatedAnnotations: true

/* ── The graph of the DPR Excel (tab 2) as an analytical query ──────────────
 * X-axis : ProductionDate (the report's daily window)
 * Lines  : ActualBoepdOvl  ("Actual Production")
 *          TargetBoepd     ("BE Target" - flat daily-rate line)
 * Mandatory date-range parameters bound the cube scan (performance);
 * optional asset filter for drill-down. Aggregation over assets/products is
 * pushed to HANA - the OData result is ~1 row per date.
 * ─────────────────────────────────────────────────────────────────────────── */
@Analytics.query: true
@OData.publish: true
@Metadata.allowExtensions: true

define view ZDPR_Q_BOEPD_TREND
  with parameters
    P_DateFrom : datum,
    P_DateTo   : datum,
    P_Asset    : char20              /* '' = all assets */

  as select from ZDPR_C_BOEPD_DAY

{
  /* ── X-axis ─────────────────────────────────────────────────────────── */
  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #HIDE
  ProductionDate,

  /* ── Free drill-down dimensions ─────────────────────────────────────── */
  @AnalyticsDetails.query.axis: #FREE
  Asset,

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Business Unit'
  BusinessUnit,

  @AnalyticsDetails.query.axis: #FREE
  ProductGroup,

  @AnalyticsDetails.query.axis: #FREE
  Product,

  /* ── Chart series (Y-axis) ──────────────────────────────────────────── */
  @AnalyticsDetails.query.axis: #COLUMNS
  @EndUserText.label: 'Actual Production (BOEPD)'
  ActualBoepdOvl,

  @AnalyticsDetails.query.axis: #COLUMNS
  @EndUserText.label: 'BE Target (BOEPD)'
  TargetBoepd,

  /* Supporting measures for table drill-down */
  @AnalyticsDetails.query.axis: #COLUMNS
  @EndUserText.label: 'Actual BOEPD (JV)'
  ActualBoepdJv,

  @AnalyticsDetails.query.axis: #COLUMNS
  @EndUserText.label: 'Actual Qty OVL (BOPD/MMSCMD)'
  ActualQtyOvl
}
where ProductionDate >= $parameters.P_DateFrom
  and ProductionDate <= $parameters.P_DateTo
  and ( $parameters.P_Asset = '' or Asset = $parameters.P_Asset )
  and BusinessUnit <> 'OTHER'   /* screen out test/garbage asset codes */
