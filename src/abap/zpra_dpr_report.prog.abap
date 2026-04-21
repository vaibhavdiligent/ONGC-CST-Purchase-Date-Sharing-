*&
*& Report  ZPRA_DPR_REPORT
*&
*&
*&
*&
*&
REPORT ZPRA_DPR_REPORT.
INCLUDE zpra_dpr_report_top .
INCLUDE zpra_dpr_report_f01 .
START-OF-SELECTION .
  PERFORM clear_variables .
  PERFORM fetch_data .
  PERFORM process_gas_records  .
  PERFORM process_data .
