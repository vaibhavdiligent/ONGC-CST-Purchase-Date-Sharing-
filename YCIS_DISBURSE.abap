*&---------------------------------------------------------------------*
*& Report  YCIS_DISBURSE
*&---------------------------------------------------------------------*
*& CIS 2026-27 - 6-level approval workflow : LEVEL 6 (CPC Finance -
*& Disbursement).  THIS IS THE TERMINAL LEVEL.
*&
*&   L5 (CPC Head) grants final approval -> WF_STATUS '60' (Pending L6).
*&   THIS program lists those Pending-L6 rows for CPC Finance:
*&     DISBURSE -> WF_STATUS '70' (Completed / Disbursed), STATUS 'A'.
*&                 The rebate order created at L3 is now released for
*&                 payment/credit-note. A confirmation mail is sent to the
*&                 originating sales office (L1).
*&     REJECT   -> WF_STATUS '10' (back to L1 for reinitiation), mail L1.
*&
*&   L6 is central (maintained in YCIS_WF_APPR under sales office '0001',
*&   level 6) and sees the Pending-L6 rows of ALL sales offices.
*&
*&   DISBURSEMENT POSTING (CPC 24.09.2026 - full automation): on Disburse
*&   the program creates the G2 Credit Note from the L3 rebate order and
*&   POSTS it to accounting immediately (FORM post_disbursement). Both the
*&   billing document (CN_DOC) and the FI accounting document (ACC_DOC /
*&   ACC_YEAR) are stored against the workflow (WF_STATUS '70') and shown on
*&   the L6 screen; the credit-note PDF + customer e-mail fire via output
*&   determination. See post_disbursement for the G2 Customizing needed.
*&
*& GUI status 'STANDARD' (function codes APPR, REJ, SELALL, DESEL, BACK,
*& EXIT) must exist in this program - create it in SE41 (copy from the L2
*& program YCIS_APPROVE).  The APPR button here means "Disburse".
*&---------------------------------------------------------------------*
REPORT  ycis_disburse.

TYPE-POOLS: slis.

TABLES: ycis_apprvl, ycis_wf_appr.

CONSTANTS: gc_level TYPE ycis_wlevel VALUE '6'.   " this program = Level 6

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
         order_no    TYPE ycis_apprvl-order_no,
         cn_doc      TYPE ycis_apprvl-cn_doc,
         acc_doc     TYPE ycis_apprvl-acc_doc,
         acc_year    TYPE ycis_apprvl-acc_year,
         purch_no    TYPE ycis_apprvl-purch_no,
         l3_user     TYPE ycis_apprvl-l3_user,
         l4_user     TYPE ycis_apprvl-l4_user,
         l5_user     TYPE ycis_apprvl-l5_user,
         l5_date     TYPE ycis_apprvl-l5_date,
         remarks     TYPE ycis_apprvl-remarks,
       END OF ty_out.

DATA: gt_appr   TYPE STANDARD TABLE OF ycis_apprvl,
      gs_appr   TYPE ycis_apprvl,
      gt_out    TYPE STANDARD TABLE OF ty_out,
      gs_out    TYPE ty_out,
      gt_fcat   TYPE slis_t_fieldcat_alv,
      gs_fcat   TYPE slis_fieldcat_alv,
      gs_layout TYPE slis_layout_alv,
      gv_auth   TYPE flag,
      gr_stype  TYPE RANGE OF ycis_apprvl-scheme_type,
      gs_stype  LIKE LINE OF gr_stype.

*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_sptag FOR ycis_apprvl-period_from,
                s_vkbur FOR ycis_apprvl-sales_off,
                s_kunnr FOR ycis_apprvl-kunnr,
                s_kvgr2 FOR ycis_apprvl-kvgr2.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_mon  RADIOBUTTON GROUP g1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(25) c_mon.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_year RADIOBUTTON GROUP g1.
SELECTION-SCREEN COMMENT 3(25) c_year.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  c_mon  = 'Monthly'.
  c_year = 'Yearly (Annual)'.

*--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM build_stype_range.
  PERFORM check_auth.
  IF gv_auth IS INITIAL.
    MESSAGE 'You are not maintained as a Level-6 (CPC Finance - Disbursement) approver (YCIS_WF_APPR)' TYPE 'I'.
    RETURN.
  ENDIF.
  PERFORM get_pending.
  IF gt_appr IS INITIAL.
    MESSAGE 'No records pending your (L6) disbursement' TYPE 'I'.
    RETURN.
  ENDIF.
  PERFORM build_out.
  PERFORM build_fieldcat.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
FORM check_auth.
  DATA lv_cnt TYPE i.
  SELECT COUNT(*) INTO lv_cnt FROM ycis_wf_appr
    WHERE wf_level = gc_level AND userid = sy-uname.
  IF lv_cnt > 0.
    gv_auth = 'X'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
FORM get_pending.
  SELECT * FROM ycis_apprvl INTO TABLE gt_appr
    WHERE wf_status    = '60'                       " Pending L6
      AND scheme_type IN gr_stype
      AND sales_off   IN s_vkbur
      AND period_from IN s_sptag
      AND kunnr       IN s_kunnr
      AND kvgr2       IN s_kvgr2.
ENDFORM.

*&---------------------------------------------------------------------*
FORM build_stype_range.
  REFRESH gr_stype.
  gs_stype-sign = 'I'. gs_stype-option = 'EQ'.
  IF p_year = 'X'.
    gs_stype-low = 'A'. APPEND gs_stype TO gr_stype.
    gs_stype-low = 'C'. APPEND gs_stype TO gr_stype.
  ELSE.
    gs_stype-low = 'M'. APPEND gs_stype TO gr_stype.
  ENDIF.
ENDFORM.

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
  add_fc 'QAIS_NO'     'CIS No.'         ''.
  add_fc 'STYPE_TXT'   'Scheme Type'     ''.
  add_fc 'KUNNR'       'Customer'        ''.
  add_fc 'CUST_NAME'   'Customer Name'   ''.
  add_fc 'KVGR2'       'Cust Group'      ''.
  add_fc 'SALES_OFF'   'Sales Office'    ''.
  add_fc 'MCQ_QTY'     'Committed Qty'   ''.
  add_fc 'MCQ_PERC'    'MCQ %'           ''.
  add_fc 'ELIG_QTY'    'Eligible Qty'    ''.
  add_fc 'REBATE_VAL'  'Rebate Value'    ''.
  add_fc 'ORDER_NO'    'Rebate Order'    ''.
  add_fc 'CN_DOC'      'Credit Note'     ''.
  add_fc 'ACC_DOC'     'Accounting Doc'  ''.
  add_fc 'ACC_YEAR'    'Fiscal Year'     ''.
  add_fc 'PURCH_NO'    'Reference No'    ''.
  add_fc 'L3_USER'     'L3 Executed By'  ''.
  add_fc 'L4_USER'     'L4 Vetted By'    ''.
  add_fc 'L5_USER'     'L5 Approved By'  ''.
  add_fc 'L5_DATE'     'L5 Approved On'  ''.
  add_fc 'REMARKS'     'Remarks'         ''.
ENDFORM.

*&---------------------------------------------------------------------*
FORM display_alv.
  gs_layout-zebra         = 'X'.
  gs_layout-box_fieldname = 'SEL'.
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
FORM show_stmt_popup CHANGING p_ans TYPE c.
  CLEAR p_ans.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'CIS 2026-27 - L6 Disbursement'
      text_question         =
        'The selected record(s) have been finally approved at L5. Confirm ' &&
        'disbursement? This will mark the CIS discount as Completed / ' &&
        'Disbursed. This action cannot be undone.'
      text_button_1         = 'Yes'
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = 'No'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = ' '
    IMPORTING
      answer                = p_ans
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
ENDFORM.

*&---------------------------------------------------------------------*
FORM top_of_page.                                           "#EC CALLED
  DATA: lt_hdr TYPE slis_t_listheader,
        ls_hdr TYPE slis_listheader.
  CLEAR ls_hdr. ls_hdr-typ = 'H'.
  ls_hdr-info = 'CIS 2026-27 L-6 Disbursement (CPC Finance)'.
  APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'.
  ls_hdr-info = 'Finally approved rebate orders are presented for disbursement.'.
  APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'.
  ls_hdr-info = 'On disbursement, the CIS discount is marked Completed and the sales office is notified.'.
  APPEND ls_hdr TO lt_hdr.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_hdr.
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
    WHEN 'APPR'.                                     " Disburse
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
*&      Form  process_selected   (A = disburse/complete, R = reject)
*&---------------------------------------------------------------------*
FORM process_selected USING p_action TYPE char1.
  DATA: lv_disb   TYPE i,
        lv_rej    TYPE i,
        lv_remark TYPE ycis_apprvl-rej_remarks,
        lt_dofc   TYPE STANDARD TABLE OF vkbur,
        lt_rofc   TYPE STANDARD TABLE OF vkbur,
        lv_ans    TYPE c,
        lv_off    TYPE vkbur,
        lv_ok     TYPE flag,
        lv_cndoc  TYPE vbeln_vf,
        lv_accdoc TYPE belnr_d,
        lv_accyr  TYPE gjahr.

  READ TABLE gt_out INTO gs_out WITH KEY sel = 'X'.
  IF sy-subrc <> 0.
    MESSAGE 'Please select at least one line' TYPE 'I'.
    RETURN.
  ENDIF.

* CIS 2026-27: a remark is MANDATORY at every level for BOTH disburse and reject
  IF p_action = 'A'.
    PERFORM get_remark USING 'Disbursement remark (mandatory)' CHANGING lv_remark.
  ELSE.
    PERFORM get_remark USING 'Reject remark (mandatory)'       CHANGING lv_remark.
  ENDIF.
  IF lv_remark IS INITIAL.
    MESSAGE 'Remark is mandatory' TYPE 'I'.
    RETURN.
  ENDIF.

  IF p_action = 'A'.
    PERFORM show_stmt_popup CHANGING lv_ans.
    IF lv_ans <> '1'.
      MESSAGE 'Disbursement cancelled' TYPE 'S'.
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
*       CIS 2026-27 (CPC 24.09.2026): fully automatic - create the G2 Credit
*       Note from the ZP09 request, POST it to accounting immediately, and let
*       output determination send the PDF + customer e-mail. Failure to
*       create/post leaves the row Pending L6 (not marked disbursed).
      CLEAR: lv_ok, lv_cndoc, lv_accdoc, lv_accyr.
      PERFORM post_disbursement USING gs_appr
              CHANGING lv_ok lv_cndoc lv_accdoc lv_accyr.
      IF lv_ok IS INITIAL.
*         credit-note creation failed - skip this row, leave it Pending L6
        CONTINUE.
      ENDIF.
      IF lv_cndoc IS NOT INITIAL.
        gs_appr-cn_doc = lv_cndoc.         " G2 credit note (billing doc)
      ENDIF.
      gs_appr-acc_doc  = lv_accdoc.        " FI accounting document (posted)
      gs_appr-acc_year = lv_accyr.
      gs_appr-wf_status = '70'.            " Completed / Disbursed
      gs_appr-status    = 'A'.
      gs_appr-l6_user   = sy-uname.
      gs_appr-l6_date   = sy-datum.
      gs_appr-l6_time   = sy-uzeit.
      gs_appr-rem_l6    = lv_remark.           " L6 disbursement remark (prints on note)
      gs_appr-remarks   = 'Disbursed - CIS discount completed'.
      COLLECT gs_appr-sales_off INTO lt_dofc.
      lv_disb = lv_disb + 1.
    ELSE.
      gs_appr-wf_status   = '10'.          " back to L1 (reinitiation)
      gs_appr-status      = 'R'.
      gs_appr-rej_level   = gc_level.
      gs_appr-rej_by      = sy-uname.
      gs_appr-rej_date    = sy-datum.
      gs_appr-rej_time    = sy-uzeit.
      gs_appr-rej_remarks = lv_remark.
      gs_appr-rem_l6      = lv_remark.
      gs_appr-remarks     = 'Returned by L6'.
      COLLECT gs_appr-sales_off INTO lt_rofc.
      lv_rej = lv_rej + 1.
    ENDIF.
    MODIFY ycis_apprvl FROM gs_appr.
  ENDLOOP.

  IF lv_disb > 0 OR lv_rej > 0.
    COMMIT WORK.
  ENDIF.
*   disbursement complete -> notify originating sales office (L1)
  IF lv_disb > 0.
    LOOP AT lt_dofc INTO lv_off.
      PERFORM send_mail USING 'D' lv_off lv_off.
    ENDLOOP.
  ENDIF.
*   rejected -> back to L1 for reinitiation: notify each affected office
  IF lv_rej > 0.
    LOOP AT lt_rofc INTO lv_off.
      PERFORM send_mail USING '1' lv_off lv_off.
    ENDLOOP.
  ENDIF.
  DELETE gt_out WHERE sel = 'X'.
  MESSAGE |{ lv_disb } disbursed (Credit Note created & posted), { lv_rej } returned to L1| TYPE 'S'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  post_disbursement
*&---------------------------------------------------------------------*
*&  Credit-note creation + immediate posting at L6 disbursement.
*&---------------------------------------------------------------------*
*   CIS 2026-27 (CPC 24.09.2026): at L6 the system creates the Credit Note
*   (billing type G2) from the ZP09 credit-memo request created at L3
*   (order-related billing, copy control ZP09 -> G2), with billing/posting
*   date = disbursement date, and it is POSTED TO ACCOUNTING IMMEDIATELY.
*   Both numbers (billing doc CN_DOC and FI document ACC_DOC/ACC_YEAR) are
*   stored against the workflow and shown on the L6 screen.
*
*   Customizing prerequisites for full automation:
*     - Billing type G2: 'Posting block' flag OFF (VOFA) so FI posts at once.
*     - Output type on G2 (credit-note copy + customer e-mail): dispatch
*       time 4 (send immediately) so the PDF download + e-mail fire without
*       any manual step (VV31 / NACE).
FORM post_disbursement USING    ps_appr  TYPE ycis_apprvl
                       CHANGING p_ok     TYPE flag
                                p_cndoc  TYPE vbeln_vf
                                p_accdoc TYPE belnr_d
                                p_accyr  TYPE gjahr.
  DATA: lt_bill   TYPE STANDARD TABLE OF bapivbrk,
        ls_bill   TYPE bapivbrk,
        lt_succ   TYPE STANDARD TABLE OF bapivbrksuccess,
        ls_succ   TYPE bapivbrksuccess,
        lt_return TYPE STANDARD TABLE OF bapireturn1,
        ls_return TYPE bapireturn1,
        lv_err    TYPE flag.

  CLEAR: p_ok, p_cndoc, p_accdoc, p_accyr.
*   need the L3 rebate order (ZP09) as the billing reference
  IF ps_appr-order_no IS INITIAL.
    MESSAGE 'No rebate order (ZP09) found - Credit Note not created' TYPE 'I'.
    RETURN.
  ENDIF.
*   idempotent: a Credit Note already exists for this proposal -> keep it
  IF ps_appr-cn_doc IS NOT INITIAL.
    p_ok     = 'X'.
    p_cndoc  = ps_appr-cn_doc.
    p_accdoc = ps_appr-acc_doc.
    p_accyr  = ps_appr-acc_year.
    RETURN.
  ENDIF.

  ls_bill-salesorg   = ps_appr-sales_org.
  ls_bill-ref_doc    = ps_appr-order_no.    " ZP09 credit-memo request
  ls_bill-ref_doc_ca = 'C'.                 " order-related billing
  ls_bill-doc_number = ps_appr-order_no.
  ls_bill-bill_date  = sy-datum.            " billing/posting date = disbursement date
  APPEND ls_bill TO lt_bill.

*   CPC (24.09.2026): FULL automation - create the G2 Credit Note AND post it
*   to accounting IMMEDIATELY. Posting to FI happens automatically provided the
*   'Posting block' flag on billing type G2 is OFF (VOFA). The credit-note
*   output (PDF download + customer e-mail) is fired by output determination
*   on G2 set to dispatch time 4 (send immediately) - no manual step.
  CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'
    TABLES
      billingdatain = lt_bill
      return        = lt_return
      success       = lt_succ.

*   any error message -> rollback, signal failure, leave row Pending L6
  LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
    lv_err = 'X'.
  ENDLOOP.
  READ TABLE lt_succ INTO ls_succ INDEX 1.
  IF lv_err = 'X' OR sy-subrc <> 0 OR ls_succ-bill_doc IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Credit Note (G2) creation/posting failed for the selected proposal' TYPE 'I'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING wait = 'X'.
  p_cndoc = ls_succ-bill_doc.

*   read the accounting document posted for this billing document, so both
*   numbers can be shown against the workflow (CPC 24.09.2026 pt.2).
  SELECT SINGLE belnr gjahr INTO (p_accdoc, p_accyr)
    FROM bkpf
    WHERE awtyp = 'VBRK'
      AND awkey = p_cndoc.
  IF p_accdoc IS INITIAL.
*     billing posted but FI doc not found (e.g. posting still in the update
*     task or a posting block is set) - the billing doc is still created.
    MESSAGE 'Credit Note created; accounting document not yet posted - check billing type G2 posting block' TYPE 'I'.
  ENDIF.
  p_ok = 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
FORM get_remark USING p_title TYPE clike
                CHANGING p_remark TYPE ycis_apprvl-rej_remarks.
  DATA: lt_fields TYPE STANDARD TABLE OF sval,
        ls_field  TYPE sval,
        lv_ret    TYPE char1.
  ls_field-tabname   = 'YCIS_APPRVL'.
  ls_field-fieldname = 'REM_L6'.               " 100-char remark field
  ls_field-fieldtext = 'Remark'.
  ls_field-field_obl = 'X'.
  APPEND ls_field TO lt_fields.
  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = p_title
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
*&      Form  send_mail   (p_mode = 'D' disbursed-complete, '1' reject)
*&---------------------------------------------------------------------*
FORM send_mail USING p_mode   TYPE char1
                     p_office  TYPE vkbur
                     p_ctxoff  TYPE vkbur.
  DATA: lt_wf   TYPE STANDARD TABLE OF ycis_wf_appr,
        ls_wf   TYPE ycis_wf_appr,
        lo_send TYPE REF TO cl_bcs,
        lo_doc  TYPE REF TO cl_document_bcs,
        lo_rec  TYPE REF TO if_recipient_bcs,
        lt_text TYPE bcsy_text,
        ls_text TYPE soli,
        lv_addr TYPE ad_smtpadr,
        lv_sub  TYPE so_obj_des.

* both notifications go to the originating sales office (Level 1)
  SELECT * FROM ycis_wf_appr INTO TABLE lt_wf
    WHERE wf_level = '1' AND sales_office = p_office.
  CHECK lt_wf IS NOT INITIAL.

  TRY.
      lo_send = cl_bcs=>create_persistent( ).
      CLEAR lt_text.
      ls_text-line = |Dear Sir/Madam,|.                            APPEND ls_text TO lt_text.
      ls_text-line = ||.                                            APPEND ls_text TO lt_text.
      IF p_mode = 'D'.
        ls_text-line = |The CIS 2026-27 discount/rebate for Sales Office { p_ctxoff } has been disbursed by L6 (CPC Finance).|.
        APPEND ls_text TO lt_text.
        ls_text-line = |The workflow for these record(s) is now Completed. No further action is required.|.
        APPEND ls_text TO lt_text.
        lv_sub = 'CIS Discount Disbursed - Workflow Completed'.
      ELSE.
        ls_text-line = |The CIS 2026-27 rebates for Sales Office { p_ctxoff } have been returned by L6 (CPC Finance).|.
        APPEND ls_text TO lt_text.
        ls_text-line = |Please log in to T-Code YRVG004 (Run CIS Scheme) and reinitiate the process.|.
        APPEND ls_text TO lt_text.
        lv_sub = 'CIS Discount Request Returned by L6 - Reinitiation Required at L1'.
      ENDIF.
      ls_text-line = ||.                                            APPEND ls_text TO lt_text.
      ls_text-line = |With warm regards,|.                          APPEND ls_text TO lt_text.
      ls_text-line = |GAIL (INDIA) LTD.|.                           APPEND ls_text TO lt_text.
      ls_text-line = ||.                                            APPEND ls_text TO lt_text.
      ls_text-line = |This is a system generated mail. Please do not reply.|.
      APPEND ls_text TO lt_text.
      lo_doc = cl_document_bcs=>create_document(
                 i_type = 'RAW' i_text = lt_text i_subject = lv_sub ).
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
