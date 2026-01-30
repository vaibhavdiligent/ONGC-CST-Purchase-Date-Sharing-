*&---------------------------------------------------------------------*
*& ABAP Data Dictionary - Structure Definitions
*& Package: YGMS
*& Description: Structure definitions for ONGC CST Purchase Data Sharing
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Structure: YGMS_S_RECEIPT
* Description: Receipt Data Structure
*----------------------------------------------------------------------*
TYPES: BEGIN OF ygms_s_receipt,
         gas_day       TYPE ygms_gas_day,
         ctp_id        TYPE ygms_ctp_id,
         ongc_material TYPE ygms_ongc_mat,
         location_id   TYPE ygms_loc_id,
         material      TYPE matnr,
         qty_scm       TYPE ygms_qty_scm,
         qty_mbg       TYPE ygms_qty_mbg,
         ongc_id       TYPE ygms_ongc_id,
         created_by    TYPE ernam,
         created_on    TYPE erdat,
         created_at    TYPE erzet,
         changed_by    TYPE aenam,
         changed_on    TYPE aedat,
         changed_at    TYPE aezet,
       END OF ygms_s_receipt.

*----------------------------------------------------------------------*
* Structure: YGMS_S_ALLOCATION
* Description: Allocation Data Structure
*----------------------------------------------------------------------*
TYPES: BEGIN OF ygms_s_allocation,
         gas_day       TYPE ygms_gas_day,
         location_id   TYPE ygms_loc_id,
         material      TYPE matnr,
         state_code    TYPE ygms_state_cd,
         state         TYPE ygms_state,
         sales_qty     TYPE ygms_qty_mbg,
         alloc_qty_mbg TYPE ygms_qty_mbg,
         alloc_qty_scm TYPE ygms_qty_scm,
         gcv           TYPE ygms_gcv,
         ncv           TYPE ygms_ncv,
         tax_type      TYPE ygms_tax_type,
         excluded      TYPE ygms_excl_flg,
       END OF ygms_s_allocation.

*----------------------------------------------------------------------*
* Structure: YGMS_S_VALIDATION
* Description: Validation Result Structure
*----------------------------------------------------------------------*
TYPES: BEGIN OF ygms_s_validation,
         location_id TYPE ygms_loc_id,
         material    TYPE matnr,
         alloc_scm   TYPE ygms_qty_scm,
         alloc_mbg   TYPE ygms_qty_mbg,
         supply_scm  TYPE ygms_qty_scm,
         supply_mbg  TYPE ygms_qty_mbg,
         diff_scm    TYPE ygms_qty_scm,
         diff_mbg    TYPE ygms_qty_mbg,
         status      TYPE char2,  " VS=Valid, ER=Error
       END OF ygms_s_validation.

*----------------------------------------------------------------------*
* Structure: YGMS_S_AUDIT
* Description: Audit Log Entry Structure (NEW in v1.2)
*----------------------------------------------------------------------*
TYPES: BEGIN OF ygms_s_audit,
         log_id      TYPE ygms_log_id,
         table_name  TYPE tabname,
         operation   TYPE char1,  " I=Insert, U=Update, D=Delete
         key_field1  TYPE char100,
         key_field2  TYPE char100,
         key_field3  TYPE char100,
         field_name  TYPE fieldname,
         old_value   TYPE char255,
         new_value   TYPE char255,
         changed_by  TYPE ernam,
         changed_on  TYPE erdat,
         changed_at  TYPE erzet,
       END OF ygms_s_audit.

*----------------------------------------------------------------------*
* Structure: YGMS_S_SALES_DATA
* Description: Sales Data Structure for Allocation Calculation
*----------------------------------------------------------------------*
TYPES: BEGIN OF ygms_s_sales_data,
         gas_day      TYPE ygms_gas_day,
         location_id  TYPE ygms_loc_id,
         material     TYPE matnr,
         state_code   TYPE ygms_state_cd,
         state        TYPE ygms_state,
         sales_qty    TYPE ygms_qty_mbg,
         customer     TYPE kunnr,
       END OF ygms_s_sales_data.

*----------------------------------------------------------------------*
* Structure: YGMS_S_EMAIL_PARAMS
* Description: Email Parameters Structure
*----------------------------------------------------------------------*
TYPES: BEGIN OF ygms_s_email_params,
         recipient    TYPE ad_smtpadr,
         cc_recipient TYPE ad_smtpadr,
         subject      TYPE so_obj_des,
         body         TYPE string,
         attach_pdf   TYPE abap_bool,
         attach_excel TYPE abap_bool,
       END OF ygms_s_email_params.

*----------------------------------------------------------------------*
* Structure: YGMS_S_FORTNIGHT
* Description: Fortnight Date Range Structure
*----------------------------------------------------------------------*
TYPES: BEGIN OF ygms_s_fortnight,
         from_date TYPE datum,
         to_date   TYPE datum,
       END OF ygms_s_fortnight.

*----------------------------------------------------------------------*
* Structure: YGMS_S_ALV_OUTPUT
* Description: ALV Output Structure for Display
*----------------------------------------------------------------------*
TYPES: BEGIN OF ygms_s_alv_output,
         gas_day       TYPE ygms_gas_day,
         location_id   TYPE ygms_loc_id,
         material      TYPE matnr,
         state_code    TYPE ygms_state_cd,
         state         TYPE ygms_state,
         ctp_id        TYPE ygms_ctp_id,
         ongc_material TYPE ygms_ongc_mat,
         qty_mbg       TYPE ygms_qty_mbg,
         gcv           TYPE ygms_gcv,
         ncv           TYPE ygms_ncv,
         qty_scm       TYPE ygms_qty_scm,
         gail_id       TYPE ygms_gail_id,
         tax_type      TYPE ygms_tax_type,
         excluded      TYPE ygms_excl_flg,
         sent          TYPE ygms_sent_flg,
         status_icon   TYPE icon_d,
         message       TYPE bapi_msg,
       END OF ygms_s_alv_output.
