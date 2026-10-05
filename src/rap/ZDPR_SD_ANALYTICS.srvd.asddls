@EndUserText.label: 'DPR Analytics - Service Definition'
define service ZDPR_SD_ANALYTICS {
  expose ZDPR_Q_PROD_PERF    as DPRProductionPerformance;
  expose ZDPR_C_PROD_CUBE    as DPRProductionCube;
  expose ZDPR_C_BOEPD_DAY    as DPRBoepdDayCube;
  expose ZDPR_I_DAILY        as DPRDailyProduction;
  expose ZDPR_I_MONTHLY      as DPRMonthlyProduction;
  expose ZDPR_I_TARGET       as DPRProductionTargets;
  expose ZDPR_I_EXCEL_DL     as DPRExcelDownload;
}
