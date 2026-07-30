*&---------------------------------------------------------------------*
*& Report  YCIS_REB_APPROVE
*&---------------------------------------------------------------------*
*& Rebate (PSD / YRVU001) - 3-level approval workflow : LEVEL 2.
*&
*&   L1  : runs YRVU001_REB_CALC_M2_N1, reviews the rebate ALV and
*&         EXECUTEs -> rows saved to YCIS_APPRVL with SCHEME_TYPE 'U'
*&         (Upliftment Rebate) and WF_STATUS '20' (Pending L2); L2 mailed.
*&   L2  : THIS program. Lists the Pending-L2 rebate rows for the user's
*&         sales office(s); APPROVE -> WF_STATUS '30' (Pending L3), mail L3;
*&         REJECT -> WF_STATUS '10' (back to L1), mail L1.
*&   L3  : YCIS_REB_EXECUTE - creates the credit-memo request (BUS2094)
*&         and writes YRVA_REBATE.
*&
*& Reuses the shared approval infrastructure (YCIS_APPRVL / YCIS_WF_APPR),
*& separated from the QAIS queue by SCHEME_TYPE = 'U'.
*& GUI status 'STANDARD' (APPR, REJ, SELALL, DESEL, BACK/EXIT) - SE41.
*&---------------------------------------------------------------------*
REPORT  ycis_reb_approve.

TYPE-POOLS: slis.

TABLES: ycis_apprvl, ycis_wf_appr.

CONSTANTS: gc_level  TYPE ycis_wlevel VALUE '2',   " this program = Level 2
           gc_scheme TYPE ycis_apprvl-scheme_type VALUE 'U'. " Upliftment Rebate

TYPES: BEGIN OF ty_out,
         sel         TYPE flag,
         kunnr       TYPE ycis_apprvl-kunnr,
         cust_name   TYPE ycis_apprvl-cust_name,
         kvgr2       TYPE ycis_apprvl-kvgr2,
         sales_off   TYPE ycis_apprvl-sales_off,
         reb_cond    TYPE ycis_apprvl-reb_cond,
         lft_qty     TYPE ycis_apprvl-lft_qty,
         elig_qty    TYPE ycis_apprvl-elig_qty,
         rebate_val  TYPE ycis_apprvl-rebate_val,
         purch_no    TYPE ycis_apprvl-purch_no,
         wf_status   TYPE ycis_apprvl-wf_status,
         l1_user     TYPE ycis_apprvl-l1_user,
         l1_date     TYPE ycis_apprvl-l1_date,
         l1_time     TYPE ycis_apprvl-l1_time,
         remarks     TYPE ycis_apprvl-remarks,
       END OF ty_out.

DATA: gt_appr   TYPE STANDARD TABLE OF ycis_apprvl,
      gs_appr   TYPE ycis_apprvl,
      gt_out    TYPE STANDARD TABLE OF ty_out,
      gs_out    TYPE ty_out,
      gt_fcat   TYPE slis_t_fieldcat_alv,
      gs_fcat   TYPE slis_fieldcat_alv,
      gs_layout TYPE slis_layout_alv,
      gr_office TYPE RANGE OF vkbur,
      gs_office LIKE LINE OF gr_office.

*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_sptag FOR ycis_apprvl-period_from,
                s_vkbur FOR ycis_apprvl-sales_off,
                s_kunnr FOR ycis_apprvl-kunnr,
                s_kvgr2 FOR ycis_apprvl-kvgr2.
SELECTION-SCREEN END OF BLOCK b1.

*--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_auth_offices.
  IF gr_office IS INITIAL.
    MESSAGE 'You are not maintained as a Level-2 approver (YCIS_WF_APPR)' TYPE 'I'.
    RETURN.
  ENDIF.
  PERFORM get_pending.
  IF gt_appr IS INITIAL.
    MESSAGE 'No rebate records pending your (L2) approval' TYPE 'I'.
    RETURN.
  ENDIF.
  PERFORM build_out.
  PERFORM build_fieldcat.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
FORM get_auth_offices.
  DATA: lt_wf TYPE STANDARD TABLE OF ycis_wf_appr,
        ls_wf TYPE ycis_wf_appr.
  REFRESH gr_office.
  SELECT * FROM ycis_wf_appr INTO TABLE lt_wf
    WHERE wf_level = gc_level
      AND userid   = sy-uname.
  LOOP AT lt_wf INTO ls_wf.
    gs_office-sign = 'I'. gs_office-option = 'EQ'.
    gs_office-low  = ls_wf-sales_office.
    APPEND gs_office TO gr_office.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
FORM get_pending.
  SELECT * FROM ycis_apprvl INTO TABLE gt_appr
    WHERE wf_status   = '20'
      AND scheme_type = gc_scheme            " rebate queue only
      AND sales_off   IN gr_office
      AND sales_off   IN s_vkbur
      AND period_from IN s_sptag
      AND kunnr       IN s_kunnr
      AND kvgr2       IN s_kvgr2.
ENDFORM.

*&---------------------------------------------------------------------*
FORM build_out.
  REFRESH gt_out.
  LOOP AT gt_appr INTO gs_appr.
    CLEAR gs_out.
    MOVE-CORRESPONDING gs_appr TO gs_out.
    APPEND gs_out TO gt_out.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
FORM build_fieldcat.
  DATA: lv_pos TYPE i.
  DEFINE add_fc.
    CLEAR gs_fcat.
    ADD 1 TO lv_pos.
    gs_fcat-col_pos   = lv_pos.
    gs_fcat-fieldname = &1.
    gs_fcat-seltext_l = &2.
    gs_fcat-seltext_m = &2.
    gs_fcat-seltext_s = &2.
    gs_fcat-checkbox  = &3.
    gs_fcat-edit      = &3.
    APPEND gs_fcat TO gt_fcat.
  END-OF-DEFINITION.

  add_fc 'SEL'         'Select'          'X'.
  add_fc 'KUNNR'       'Customer'        ''.
  add_fc 'CUST_NAME'   'Customer Name'   ''.
  add_fc 'KVGR2'       'Grp Co.'         ''.
  add_fc 'SALES_OFF'   'Sales Office'    ''.
  add_fc 'REB_COND'    'Cond. Type'      ''.
  add_fc 'LFT_QTY'     'Lifted Qty'      ''.
  add_fc 'ELIG_QTY'    'Eligible Qty'    ''.
  add_fc 'REBATE_VAL'  'Rebate Value'    ''.
  add_fc 'PURCH_NO'    'Reference No'    ''.
  add_fc 'L1_USER'     'L1 Approved By'  ''.
  add_fc 'L1_DATE'     'L1 Approved On'  ''.
  add_fc 'L1_TIME'     'L1 Approved At'  ''.
  add_fc 'REMARKS'     'Remarks'         ''.
ENDFORM.

*&---------------------------------------------------------------------*
FORM display_alv.
  gs_layout-zebra          = 'X'.
  gs_layout-box_fieldname  = 'SEL'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = gs_layout
      it_fieldcat              = gt_fcat
    TABLES
      t_outtab                 = gt_out
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.

*&---------------------------------------------------------------------*
FORM set_status USING rt_extab TYPE slis_t_extab.            "#EC CALLED
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab.
ENDFORM.

*&---------------------------------------------------------------------*
FORM user_command USING r_ucomm     LIKE sy-ucomm            "#EC CALLED
                        rs_selfield TYPE slis_selfield.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.
  IF lr_grid IS NOT INITIAL.
    CALL METHOD lr_grid->check_changed_data.
  ENDIF.

  CASE r_ucomm.
    WHEN 'APPR'.
      PERFORM process_selected USING 'A'.
      rs_selfield-refresh = 'X'.
    WHEN 'REJ'.
      PERFORM process_selected USING 'R'.
      rs_selfield-refresh = 'X'.
    WHEN 'SELALL'.
      LOOP AT gt_out INTO gs_out.
        gs_out-sel = 'X'. MODIFY gt_out FROM gs_out.
      ENDLOOP.
      rs_selfield-refresh = 'X'.
    WHEN 'DESEL'.
      LOOP AT gt_out INTO gs_out.
        CLEAR gs_out-sel. MODIFY gt_out FROM gs_out.
      ENDLOOP.
      rs_selfield-refresh = 'X'.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
FORM process_selected USING p_action TYPE char1.
  DATA: lv_cnt    TYPE i,
        lv_remark TYPE ycis_apprvl-rej_remarks,
        lt_office TYPE STANDARD TABLE OF vkbur,
        lv_off    TYPE vkbur.

  READ TABLE gt_out INTO gs_out WITH KEY sel = 'X'.
  IF sy-subrc <> 0.
    MESSAGE 'Please select at least one line' TYPE 'I'.
    RETURN.
  ENDIF.

  IF p_action = 'R'.
    PERFORM get_reject_remark CHANGING lv_remark.
    IF lv_remark IS INITIAL.
      MESSAGE 'Reject remark is mandatory' TYPE 'I'.
      RETURN.
    ENDIF.
  ENDIF.

  LOOP AT gt_out INTO gs_out WHERE sel = 'X'.
    READ TABLE gt_appr INTO gs_appr
         WITH KEY kunnr     = gs_out-kunnr
                  kvgr2     = gs_out-kvgr2
                  sales_off = gs_out-sales_off.
    CHECK sy-subrc = 0.
    IF p_action = 'A'.
      gs_appr-wf_status = '30'.        " pending L3
      gs_appr-l2_user   = sy-uname.
      gs_appr-l2_date   = sy-datum.
      gs_appr-l2_time   = sy-uzeit.
      gs_appr-remarks   = 'L2 approved'.
    ELSE.
      gs_appr-wf_status   = '10'.      " back to L1
      gs_appr-rej_level   = gc_level.
      gs_appr-rej_by      = sy-uname.
      gs_appr-rej_date    = sy-datum.
      gs_appr-rej_time    = sy-uzeit.
      gs_appr-rej_remarks = lv_remark.
      gs_appr-remarks     = 'Rejected by L2'.
    ENDIF.
    MODIFY ycis_apprvl FROM gs_appr.
    COLLECT gs_appr-sales_off INTO lt_office.
    lv_cnt = lv_cnt + 1.
  ENDLOOP.

  IF lv_cnt > 0.
    COMMIT WORK.
    LOOP AT lt_office INTO lv_off.
      IF p_action = 'A'.
        PERFORM send_mail USING '3' '0001' lv_off 'Rebate (PSD) pending execution (L3)'.
      ELSE.
        PERFORM send_mail USING '1' lv_off lv_off 'Rebate (PSD) rejected by L2 - please review (L1)'.
      ENDIF.
    ENDLOOP.
    MESSAGE |{ lv_cnt } rebate line(s) processed| TYPE 'S'.
    DELETE gt_out WHERE sel = 'X'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
FORM get_reject_remark CHANGING p_remark TYPE ycis_apprvl-rej_remarks.
  DATA: lt_fields TYPE STANDARD TABLE OF sval,
        ls_field  TYPE sval,
        lv_ret    TYPE char1.
  ls_field-tabname   = 'YCIS_APPRVL'.
  ls_field-fieldname = 'REJ_REMARKS'.
  ls_field-field_obl = 'X'.
  APPEND ls_field TO lt_fields.
  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'Reject remark'
    IMPORTING
      returncode      = lv_ret
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  IF sy-subrc = 0 AND lv_ret <> 'A'.
    READ TABLE lt_fields INTO ls_field INDEX 1.
    p_remark = ls_field-value.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
FORM send_mail USING p_level  TYPE ycis_wlevel
                     p_office  TYPE vkbur
                     p_ctxoff  TYPE vkbur
                     p_subject TYPE string.
  DATA: lt_wf   TYPE STANDARD TABLE OF ycis_wf_appr,
        ls_wf   TYPE ycis_wf_appr,
        lo_send TYPE REF TO cl_bcs,
        lo_doc  TYPE REF TO cl_document_bcs,
        lo_rec  TYPE REF TO if_recipient_bcs,
        lt_text TYPE bcsy_text,
        ls_text TYPE soli,
        lv_addr TYPE ad_smtpadr,
        lv_sub  TYPE so_obj_des.

  SELECT * FROM ycis_wf_appr INTO TABLE lt_wf
    WHERE wf_level = p_level AND sales_office = p_office.
  CHECK lt_wf IS NOT INITIAL.

  TRY.
      lo_send = cl_bcs=>create_persistent( ).
      CLEAR lt_text.
      IF p_level = '3'.
        ls_text-line = |Dear Sir/Madam,|.                           APPEND ls_text TO lt_text.
        ls_text-line = ||.                                           APPEND ls_text TO lt_text.
        ls_text-line = |Rebate (PSD) lines have been verified and approved by L2.|.
        APPEND ls_text TO lt_text.
        ls_text-line = |Please log in to T-Code YRVU015_E and generate the credit-memo requests.|.
        APPEND ls_text TO lt_text.
        ls_text-line = ||.                                           APPEND ls_text TO lt_text.
        ls_text-line = |This is a system generated mail. Please do not reply.|.
        APPEND ls_text TO lt_text.
        lv_sub = 'Rebate (PSD) - approved by L2, pending L3 execution'.
      ELSE.
        ls_text-line = |Dear Sir/Madam,|.                           APPEND ls_text TO lt_text.
        ls_text-line = ||.                                           APPEND ls_text TO lt_text.
        ls_text-line = |The rebate (PSD) lines for Sales Office { p_ctxoff } have been returned by L2|.
        APPEND ls_text TO lt_text.
        ls_text-line = |for your review. Please log in to T-Code YRVU015_A and re-submit.|.
        APPEND ls_text TO lt_text.
        ls_text-line = ||.                                           APPEND ls_text TO lt_text.
        ls_text-line = |This is a system generated mail. Please do not reply.|.
        APPEND ls_text TO lt_text.
        lv_sub = 'Rebate (PSD) - returned by L2 for review'.
      ENDIF.
      lo_doc = cl_document_bcs=>create_document(
                 i_type    = 'RAW'
                 i_text    = lt_text
                 i_subject = lv_sub ).
      lo_send->set_document( lo_doc ).
      LOOP AT lt_wf INTO ls_wf.
        CHECK ls_wf-email IS NOT INITIAL.
        lv_addr = ls_wf-email.
        lo_rec  = cl_cam_address_bcs=>create_internet_address( lv_addr ).
        lo_send->add_recipient( i_recipient = lo_rec ).
      ENDLOOP.
      lo_send->set_send_immediately( 'X' ).
      lo_send->send( i_with_error_screen = 'X' ).
      COMMIT WORK.
    CATCH cx_bcs.
  ENDTRY.
ENDFORM.
