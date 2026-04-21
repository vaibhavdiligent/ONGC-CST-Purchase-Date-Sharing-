@EndUserText.label: 'DPR Analytics - OData V4 Service Binding'

/*
 * Service Binding Type : OData V4 - UI
 * Binding Name         : ZPRA_SB_DPR_ANALYTICS_O4
 * Service Name         : ZPRA_DPR_ANALYTICS
 * Service Version      : 0001
 *
 * Activate in ABAP Development Tools (ADT):
 *   1. Open this file in ADT
 *   2. Click "Publish" to activate the OData V4 endpoint
 *   3. Service URL: /sap/opu/odata4/sap/zpra_dpr_analytics/srvd/sap/zpra_sd_dpr_analytics/0001/
 *
 * Fiori Elements Page Types supported:
 *   - Analytical List Page  : DPRProductionQuery
 *   - Overview Page         : DPRProductionCube
 *   - List Report           : DPRDailyProduction, DPRMonthlyProduction
 */

define service binding ZPRA_SB_DPR_ANALYTICS_O4
  {
    service definition ZPRA_SD_DPR_ANALYTICS;
    binding type       odata v4 ui;
  }
