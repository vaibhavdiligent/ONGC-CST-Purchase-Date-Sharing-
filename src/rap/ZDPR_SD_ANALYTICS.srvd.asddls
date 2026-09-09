@EndUserText.label: 'DPR Analytics - Service Definition'

define service ZDPR_SD_ANALYTICS {

  /* ── Analytical queries (built-in Fiori Export to Excel button) ───── */
  expose ZDPR_Q_PROD_QUERY   as DPRProductionQuery;
  expose ZDPR_Q_TARGET_QUERY as DPRTargetQuery;
  expose ZDPR_Q_DAILY_TREND  as DPRDailyTrend;

  /* ── DPR Excel replica (tab 2 graph + tab 3 Production Performance) ─ */
  expose ZDPR_Q_BOEPD_TREND  as DPRBoepdTrend;
  expose ZDPR_Q_PROD_PERF    as DPRProductionPerformance;

  /* ── Analytical cubes ─────────────────────────────────────────────── */
  expose ZDPR_C_PROD_CUBE         as DPRProductionCube;
  expose ZDPR_C_BOEPD_DAY    as DPRBoepdDayCube;

  /* ── Interface views ──────────────────────────────────────────────── */
  expose ZDPR_I_DAILY        as DPRDailyProduction;
  expose ZDPR_I_MONTHLY      as DPRMonthlyProduction;
  expose ZDPR_I_TARGET       as DPRProductionTargets;

  /* ── Excel download action entity + parameter/result abstract types ─ */
  expose ZDPR_I_EXCEL_DL     as DPRExcelDownload;

}
