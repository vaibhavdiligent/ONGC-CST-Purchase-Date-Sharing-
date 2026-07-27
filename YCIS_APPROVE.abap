*&---------------------------------------------------------------------*
*& Report  YCIS_APPROVE
*&---------------------------------------------------------------------*
*& CIS 2026-27 - 3-level approval workflow : LEVEL 2 (PC MKTG-HOD).
*&
*&   L1 (PC MKTG)      : runs YRVG004_QAIS_EXECUTE_N1, reviews the discount
*&                       ALV and CONFIRMs -> rows saved to YCIS_APPRVL with
*&                       WF_STATUS '20' (Pending L2); L2 users are e-mailed.
*&   L2 (PC MKTG-HOD)  : THIS program. Lists the Pending-L2 rows for the
*&                       user's sales office(s), select (checkbox / Select
*&                       All) and:
*&                         APPROVE -> WF_STATUS '30' (Pending L3), e-mail L3.
*&                         REJECT  -> WF_STATUS '10' (back to L1), e-mail L1.
*&   L3 (CPC)          : YCIS_EXECUTE - executes and creates the rebate order.
*&
*& Approval hierarchy & e-mail recipients are read from table YCIS_WF_APPR
*& (Sales Office / Department / Level / Sequence / User / E-mail), SM30.
*&
*& GUI status 'STANDARD' (with function codes APPR, REJ, SELALL, DESEL and
*& BACK/EXIT) must exist in this program - create it in SE41 (see doc).
*&---------------------------------------------------------------------*
REPORT  ycis_approve.

TYPE-POOLS: slis.

TABLES: ycis_apprvl, ycis_wf_appr.

CONSTANTS: gc_level TYPE ycis_wlevel VALUE '2'.   " this program = Level 2

TYPES: BEGIN OF ty_out,
         sel         TYPE flag,
         qais_no     TYPE ycis_apprvl-qais_no,
         scheme_type TYPE ycis_apprvl-scheme_type,
         stype_txt   TYPE char20,
         kunnr       TYPE ycis_apprvl-kunnr,
         cust_name   TYPE ycis_apprvl-cust_name,
         kvgr2       TYPE ycis_apprvl-kvgr2,
         sales_off   TYPE ycis_apprvl-sales_off,
         mcq_qty     TYPE ycis_apprvl-mcq_qty,
         mcq_perc    TYPE ycis_apprvl-mcq_perc,
         elig_qty    TYPE ycis_apprvl-elig_qty,
         rebate_val  TYPE ycis_apprvl-rebate_val,
         purch_no    TYPE ycis_apprvl-purch_no,
         wf_status   TYPE ycis_apprvl-wf_status,
         l1_user     TYPE ycis_apprvl-l1_user,
         remarks     TYPE ycis_apprvl-remarks,
       END OF ty_out.

DATA: gt_appr  TYPE STANDARD TABLE OF ycis_apprvl,
      gs_appr  TYPE ycis_apprvl,
      gt_out   TYPE STANDARD TABLE OF ty_out,
      gs_out   TYPE ty_out,
      gt_fcat  TYPE slis_t_fieldcat_alv,
      gs_fcat  TYPE slis_fieldcat_alv,
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
    MESSAGE 'No records pending your (L2) approval' TYPE 'I'.
    RETURN.
  ENDIF.
  PERFORM build_out.
  PERFORM build_fieldcat.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
*&      Form  get_auth_offices   (sales offices this user handles at L2)
*&---------------------------------------------------------------------*
FORM get_auth_offices.
  DATA: lt_wf TYPE STANDARD TABLE OF ycis_wf_appr,
        ls_wf TYPE ycis_wf_appr.
  REFRESH gr_office.
  SELECT * FROM ycis_wf_appr INTO TABLE lt_wf
    WHERE wf_level  = gc_level
      AND userid = sy-uname.
  LOOP AT lt_wf INTO ls_wf.
    gs_office-sign = 'I'. gs_office-option = 'EQ'.
    gs_office-low  = ls_wf-sales_office.
    APPEND gs_office TO gr_office.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_pending   (rows pending L2 for this user's offices)
*&---------------------------------------------------------------------*
FORM get_pending.
  SELECT * FROM ycis_apprvl INTO TABLE gt_appr
    WHERE wf_status   = '20'
      AND sales_off   IN gr_office
      AND sales_off   IN s_vkbur
      AND period_from IN s_sptag
      AND kunnr       IN s_kunnr
      AND kvgr2       IN s_kvgr2.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  build_out
*&---------------------------------------------------------------------*
FORM build_out.
  REFRESH gt_out.
  LOOP AT gt_appr INTO gs_appr.
    CLEAR gs_out.
    MOVE-CORRESPONDING gs_appr TO gs_out.
    PERFORM scheme_text USING gs_appr-scheme_type CHANGING gs_out-stype_txt.
    APPEND gs_out TO gt_out.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  scheme_text   (readable CIS scheme type - GAIL 17.07.2026)
*&---------------------------------------------------------------------*
FORM scheme_text USING p_code TYPE any CHANGING p_txt TYPE char20.
  CASE p_code.
    WHEN 'M'. p_txt = 'Monthly'.
    WHEN 'Q'. p_txt = 'Quarterly'.
    WHEN 'A'. p_txt = 'Annual'.
    WHEN 'C'. p_txt = 'Annual Consistency'.
    WHEN OTHERS. p_txt = p_code.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
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

  add_fc 'SEL'         'Select'        'X'.
  add_fc 'QAIS_NO'     'CIS No.'       ''.
  add_fc 'STYPE_TXT'   'Scheme Type'   ''.
  add_fc 'KUNNR'       'Customer'      ''.
  add_fc 'CUST_NAME'   'Customer Name' ''.
  add_fc 'KVGR2'       'Cust Group'    ''.
  add_fc 'SALES_OFF'   'Sales Office'  ''.
  add_fc 'MCQ_QTY'     'MCQ Qty'       ''.
  add_fc 'MCQ_PERC'    'MCQ %'         ''.
  add_fc 'ELIG_QTY'    'Eligible Qty'  ''.
  add_fc 'REBATE_VAL'  'Rebate Value'  ''.
  add_fc 'PURCH_NO'    'Reference No'  ''.
  add_fc 'L1_USER'     'L1 Confirmed By' ''.
  add_fc 'REMARKS'     'Remarks'       ''.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
FORM display_alv.
  gs_layout-zebra          = 'X'.
  gs_layout-box_fieldname  = 'SEL'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_top_of_page   = 'TOP_OF_PAGE'
      is_layout                = gs_layout
      it_fieldcat              = gt_fcat
    TABLES
      t_outtab                 = gt_out
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  top_of_page   (confirmation statement header - L2)
*&---------------------------------------------------------------------*
FORM top_of_page.                                           "#EC CALLED
  DATA: lt_hdr TYPE slis_t_listheader,
        ls_hdr TYPE slis_listheader.
  CLEAR ls_hdr. ls_hdr-typ = 'H'.
  ls_hdr-info = 'CIS 2026-27 - Level-2 Approval. On approval you confirm:'.
  APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'.
  ls_hdr-info = 'Customer-wise, grade-wise sales quantities, along with'.
  APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'.
  ls_hdr-info = 'eligible PSD rates and amounts, have been verified and'.
  APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'.
  ls_hdr-info = 'confirmed after considering customer waivers, shortfall'.
  APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'.
  ls_hdr-info = 'waivers, sales return quantities, and Group/MLE details.'.
  APPEND ls_hdr TO lt_hdr.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_hdr.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SET_STATUS  (ALV GUI status callback)
*&---------------------------------------------------------------------*
FORM set_status USING rt_extab TYPE slis_t_extab.            "#EC CALLED
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command USING r_ucomm     LIKE sy-ucomm            "#EC CALLED
                        rs_selfield TYPE slis_selfield.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
*   sync the edited checkbox values back into gt_out
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
*&      Form  process_selected   (A = approve, R = reject)
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
         WITH KEY qais_no     = gs_out-qais_no
                  scheme_type = gs_out-scheme_type
                  kunnr       = gs_out-kunnr
                  kvgr2       = gs_out-kvgr2.
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
        PERFORM send_mail USING '3' '0001' lv_off 'CIS rebates pending execution (L3/CPC)'.
      ELSE.
        PERFORM send_mail USING '1' lv_off lv_off 'CIS rebates rejected by L2 - please review (L1)'.
      ENDIF.
    ENDLOOP.
    MESSAGE |{ lv_cnt } line(s) processed| TYPE 'S'.
*   drop the processed lines from the current list
    DELETE gt_out WHERE sel = 'X'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_reject_remark
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
*&      Form  send_mail   (notify next/previous level from YCIS_WF_APPR)
*&      p_level  = target level (1/2/3)
*&      p_office = sales office to read recipients for ('0001' for CPC/L3)
*&      p_ctxoff = the office the rebate belongs to (for the mail text)
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
*       L2 -> L3 (CPC) : exact wording requested by GAIL (17.07.2026)
        ls_text-line = |Dear Sir/Madam,|.                           APPEND ls_text TO lt_text.
        ls_text-line = ||.                                           APPEND ls_text TO lt_text.
        ls_text-line = |The rebates under the CIS 2026-27 Scheme have been successfully verified|.
        APPEND ls_text TO lt_text.
        ls_text-line = |and approved by L2.|.                        APPEND ls_text TO lt_text.
        ls_text-line = ||.                                           APPEND ls_text TO lt_text.
        ls_text-line = |Please log in to T-Code YRVG004_E and generate the rebate orders.|.
        APPEND ls_text TO lt_text.
        ls_text-line = ||.                                           APPEND ls_text TO lt_text.
        ls_text-line = |With warm regards,|.                         APPEND ls_text TO lt_text.
        ls_text-line = |GAIL (INDIA) LTD.|.                          APPEND ls_text TO lt_text.
        ls_text-line = ||.                                           APPEND ls_text TO lt_text.
        ls_text-line = |This is a system generated mail. Please do not reply.|.
        APPEND ls_text TO lt_text.
        lv_sub = 'CIS Scheme - Rebates reviewed & submitted by L2'.
      ELSE.
*       L2 reject -> back to L1
        ls_text-line = |Dear Sir/Madam,|.                           APPEND ls_text TO lt_text.
        ls_text-line = ||.                                           APPEND ls_text TO lt_text.
        ls_text-line = |The CIS 2026-27 rebates for Sales Office { p_ctxoff } have been returned by L2|.
        APPEND ls_text TO lt_text.
        ls_text-line = |for your review. Please log in to T-Code YRVG004 (Run CIS Scheme) and re-submit.|.
        APPEND ls_text TO lt_text.
        ls_text-line = ||.                                           APPEND ls_text TO lt_text.
        ls_text-line = |With warm regards,|.                         APPEND ls_text TO lt_text.
        ls_text-line = |GAIL (INDIA) LTD.|.                          APPEND ls_text TO lt_text.
        ls_text-line = ||.                                           APPEND ls_text TO lt_text.
        ls_text-line = |This is a system generated mail. Please do not reply.|.
        APPEND ls_text TO lt_text.
        lv_sub = 'CIS Scheme - Rebates returned by L2 for review'.
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
*     force immediate delivery (do not leave the mail waiting in the
*     SAPconnect / SOST queue) - GAIL 17.07.2026 "mail not going".
      lo_send->set_send_immediately( 'X' ).
      lo_send->send( i_with_error_screen = 'X' ).
      COMMIT WORK.
    CATCH cx_bcs.
*     mail failure must not block the approval; log silently
  ENDTRY.
ENDFORM.
