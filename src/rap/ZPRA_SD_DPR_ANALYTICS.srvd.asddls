@EndUserText.label: 'DPR Analytics - Service Definition'

define service ZPRA_SD_DPR_ANALYTICS {

  /* ── Production analytical query (daily data) ─────────────────────────── */
  expose ZPRA_Q_DPR_PROD_QUERY   as DPRProductionQuery;

  /* ── Target vs Actual analytical query ───────────────────────────────── */
  expose ZPRA_Q_DPR_TARGET_QUERY as DPRTargetQuery;

  /* ── Cube (for direct OData $apply access) ───────────────────────────── */
  expose ZPRA_C_DPR_CUBE         as DPRProductionCube;

  /* ── Interface views (for drill-down navigation) ─────────────────────── */
  expose ZPRA_I_DPR_DAILY        as DPRDailyProduction;
  expose ZPRA_I_DPR_MONTHLY      as DPRMonthlyProduction;
  expose ZPRA_I_DPR_TARGET       as DPRProductionTargets;

}
