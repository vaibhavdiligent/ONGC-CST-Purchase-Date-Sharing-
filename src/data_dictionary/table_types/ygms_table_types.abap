*&---------------------------------------------------------------------*
*& ABAP Data Dictionary - Table Type Definitions
*& Package: YGMS
*& Description: Table Type definitions for ONGC CST Purchase Data Sharing
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Table Type: YGMS_TT_RECEIPT
* Line Type: YGMS_S_RECEIPT
* Key Type: Standard
* Description: Receipt data table type
*----------------------------------------------------------------------*
TYPES: ygms_tt_receipt TYPE STANDARD TABLE OF ygms_s_receipt
                        WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Table Type: YGMS_TT_ALLOCATION
* Line Type: YGMS_S_ALLOCATION
* Key Type: Sorted by GAS_DAY, LOCATION_ID, MATERIAL, STATE_CODE
* Description: Allocation data table type
*----------------------------------------------------------------------*
TYPES: ygms_tt_allocation TYPE SORTED TABLE OF ygms_s_allocation
                           WITH UNIQUE KEY gas_day location_id material state_code.

*----------------------------------------------------------------------*
* Table Type: YGMS_TT_VALIDATION
* Line Type: YGMS_S_VALIDATION
* Key Type: Standard
* Description: Validation results table type
*----------------------------------------------------------------------*
TYPES: ygms_tt_validation TYPE STANDARD TABLE OF ygms_s_validation
                           WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Table Type: YGMS_TT_PURCHASE
* Line Type: YGMS_CST_PUR
* Key Type: Standard
* Description: Purchase data table type
*----------------------------------------------------------------------*
TYPES: ygms_tt_purchase TYPE STANDARD TABLE OF ygms_cst_pur
                         WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Table Type: YGMS_TT_FNT_DATA
* Line Type: YGMS_CST_FNT_DATA
* Key Type: Standard
* Description: Fortnightly data table type
*----------------------------------------------------------------------*
TYPES: ygms_tt_fnt_data TYPE STANDARD TABLE OF ygms_cst_fnt_data
                         WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Table Type: YGMS_TT_AUDIT_LOG
* Line Type: YGMS_CST_AUDIT_LOG
* Key Type: Standard
* Description: Audit log table type
*----------------------------------------------------------------------*
TYPES: ygms_tt_audit_log TYPE STANDARD TABLE OF ygms_cst_audit_log
                          WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Table Type: YGMS_TT_SALES_DATA
* Line Type: YGMS_S_SALES_DATA
* Key Type: Standard
* Description: Sales data table type
*----------------------------------------------------------------------*
TYPES: ygms_tt_sales_data TYPE STANDARD TABLE OF ygms_s_sales_data
                           WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Table Type: YGMS_TT_ALV_OUTPUT
* Line Type: YGMS_S_ALV_OUTPUT
* Key Type: Standard
* Description: ALV output table type
*----------------------------------------------------------------------*
TYPES: ygms_tt_alv_output TYPE STANDARD TABLE OF ygms_s_alv_output
                           WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Range Table Types
*----------------------------------------------------------------------*
TYPES: ygms_rt_state_code TYPE RANGE OF ygms_state_cd.
TYPES: ygms_rt_location   TYPE RANGE OF ygms_loc_id.
TYPES: ygms_rt_material   TYPE RANGE OF matnr.
TYPES: ygms_rt_date       TYPE RANGE OF datum.
