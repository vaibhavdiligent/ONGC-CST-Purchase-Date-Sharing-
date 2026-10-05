@EndUserText.label: 'DPR Analytics - OData V4 Service Binding'

/*
 * Service Binding Type : OData V4 - UI
 * Binding Name         : ZDPR_SB_ANALYTICS_O4
 * Service Name         : ZDPR_ANALYTICS
 * Service Version      : 0001
 *
 * Activate in ABAP Development Tools (ADT):
 *   1. Open this file in ADT
 *   2. Click "Publish" to activate the OData V4 endpoint
 *   3. Service URL: /sap/opu/odata4/sap/zdpr_analytics/srvd/sap/zdpr_sd_analytics/0001/
 *
 * Fiori Elements Page Types supported:
 *   - Analytical List Page  : DPRProductionQuery
 *   - Overview Page         : DPRProductionCube
 *   - List Report           : DPRDailyProduction, DPRMonthlyProduction
 */

define service binding ZDPR_SB_ANALYTICS_O4
  {
    service definition ZDPR_SD_ANALYTICS;
    binding type       odata v4 ui;
  }
