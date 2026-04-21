@EndUserText.label: 'DPR Analytics - Service Definition'

define service ZPRA_SD_DPR_ANALYTICS {

  /* ── Analytical queries (built-in Fiori Export to Excel button) ───── */
  expose ZPRA_Q_DPR_PROD_QUERY   as DPRProductionQuery;
  expose ZPRA_Q_DPR_TARGET_QUERY as DPRTargetQuery;
  expose ZPRA_Q_DPR_DAILY_TREND  as DPRDailyTrend;

  /* ── Analytical cube ──────────────────────────────────────────────── */
  expose ZPRA_C_DPR_CUBE         as DPRProductionCube;

  /* ── Interface views ──────────────────────────────────────────────── */
  expose ZPRA_I_DPR_DAILY        as DPRDailyProduction;
  expose ZPRA_I_DPR_MONTHLY      as DPRMonthlyProduction;
  expose ZPRA_I_DPR_TARGET       as DPRProductionTargets;

  /* ── Excel download action entity + parameter/result abstract types ─ */
  expose ZPRA_I_DPR_EXCEL_DL     as DPRExcelDownload;

}
