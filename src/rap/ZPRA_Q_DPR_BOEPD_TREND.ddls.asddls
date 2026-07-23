@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR BOEPD Trend - Actual vs BE Target (Excel tab 2)'
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
@Analytics.settings.maxResultSize: #UNLIMITED

@OData.entityType.name: 'DPRBoepdTrendQueryType'

define view entity ZPRA_Q_DPR_BOEPD_TREND
  with parameters
    P_DateFrom : zpra_t_dly_prd-production_date,
    P_DateTo   : zpra_t_dly_prd-production_date,
    P_Asset    : zpra_t_dly_prd-asset              /* '' = all assets */

  as select from ZPRA_C_DPR_BOEPD_DAY

{
  /* ── X-axis ─────────────────────────────────────────────────────────── */
  @AnalyticsDetails.query.axis: #ROWS
  @AnalyticsDetails.query.totals: #HIDE
  ProductionDate,

  /* ── Free drill-down dimensions ─────────────────────────────────────── */
  @AnalyticsDetails.query.axis: #FREE
  Asset,

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
