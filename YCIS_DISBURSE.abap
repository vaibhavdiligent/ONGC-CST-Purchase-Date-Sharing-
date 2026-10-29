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
*& program YCIS_APPROVE).  The APPR button here means "Post": in SE41 set
*& the function text of APPR to 'Post' (CIS UAT 14.10.2026 pt.6).
*&
*& CIS UAT 14.10.2026 (T-code YRVG004_P):
*&   pt.5  Credit Note / Accounting Doc are stored and shown - use the
*&         'Executed' option to display posted records.
*&   pt.7  The accounting document is read again after posting (with a
*&         short wait for the update task); message 'Accounting document
*&         posted' when the credit note is posted (VBRK-RFBSK = 'C').
*&   pt.8  On posting, the credit note PDF (Smart Form YRV_CREDIT_NOTE, as
*&         in YRVR167) is e-mailed to the customer's e-mail IDs (partner
*&         logic of YRVR169) - FORM send_cn_mail.
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
*        CIS UAT 14.10.2026 pt.1/2: seasonal indicator + monthly waivers
*        (customer master YRVA_QAIS_DATA) shown at every level
         seas_ind    TYPE yrva_qais_data-p_r_indicator,
         sc_ind      TYPE yrva_qais_data-p_sc_indicator,
         waiver_1    TYPE yrva_qais_data-waiver_1,
         waiver_2    TYPE yrva_qais_data-waiver_2,
         wv_stat     TYPE ycis_apprvl-wv_stat,     " Shortfall/Customer Waiver status
         lft_qty     TYPE ycis_apprvl-lft_qty,       " group lifted qty
         ind_lft_qty TYPE ycis_apprvl-ind_lft_qty,   " individual lifted qty
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
*        CIS UAT 14.10.2026 pt.3: 'Executed' view - processing details
         wf_txt      TYPE char40,
         l6_user     TYPE ycis_apprvl-l6_user,
         l6_date     TYPE ycis_apprvl-l6_date,
         rem_l6      TYPE ycis_apprvl-rem_l6,
         rej_by      TYPE ycis_apprvl-rej_by,
         rej_date    TYPE ycis_apprvl-rej_date,
         rej_remarks TYPE ycis_apprvl-rej_remarks,
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
DATA: gt_qais TYPE STANDARD TABLE OF yrva_qais_data.   " seasonal ind. / waivers

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t_b1.
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
*   CIS UAT 14.10.2026 pt.3: Unexecuted = pending at this level (action
*   possible); Executed = already processed at this level (display only).
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE t_b2.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_unex RADIOBUTTON GROUP g2 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(45) c_unex.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_exec RADIOBUTTON GROUP g2.
SELECTION-SCREEN COMMENT 3(45) c_exec.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  c_mon  = 'Monthly'.
  c_year = 'Yearly (Annual)'.
  c_unex = 'Unexecuted (pending at L6)'.
  c_exec = 'Executed (already processed at L6)'.
  t_b1   = 'Selection Criteria'.
  t_b2   = 'Display'.
* CIS UAT (L4 obs.3): field names on the selection screen without
* depending on maintained selection texts
  %_s_sptag_%_app_%-text = 'Period From'.
  %_s_vkbur_%_app_%-text = 'Sales Office'.
  %_s_kunnr_%_app_%-text = 'Customer'.
  %_s_kvgr2_%_app_%-text = 'Customer Group'.

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
    IF p_exec = 'X'.
      MESSAGE 'No records already posted / processed at L6 for the selection' TYPE 'I'.
    ELSE.
      MESSAGE 'No records pending your (L6) posting' TYPE 'I'.
    ENDIF.
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
* CIS UAT 23.10.2026: zero-value 'GROUP OK' lines (no rebate order) are not
* vetted / approved / posted any more - lines forwarded before this change
* are closed here (L3 now closes them itself and does not forward them).
  IF p_exec IS INITIAL.
    UPDATE ycis_apprvl SET wf_status = '70'
                           status    = 'A'
                           remarks   = 'Group OK - no rebate order, closed'
      WHERE wf_status = '60'
        AND order_no  = 'GROUP OK'.
    IF sy-dbcnt > 0.
      COMMIT WORK.
    ENDIF.
  ENDIF.
  IF p_exec = 'X'.
    SELECT * FROM ycis_apprvl INTO TABLE gt_appr
      WHERE ( l6_user <> space OR rej_level = gc_level )   " processed at L6
        AND scheme_type IN gr_stype
        AND sales_off   IN s_vkbur
        AND period_from IN s_sptag
        AND kunnr       IN s_kunnr
        AND kvgr2       IN s_kvgr2.
  ELSE.
    SELECT * FROM ycis_apprvl INTO TABLE gt_appr
      WHERE wf_status    = '60'                       " Pending L6
        AND scheme_type IN gr_stype
        AND sales_off   IN s_vkbur
        AND period_from IN s_sptag
        AND kunnr       IN s_kunnr
        AND kvgr2       IN s_kvgr2.
  ENDIF.
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
    gs_stype-low = 'T'. APPEND gs_stype TO gr_stype.   " Strategic Monthly Discount
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
FORM build_out.
  DATA: lv_posted TYPE flag,
        lv_upd    TYPE flag,
        lv_txtok  TYPE flag.
  REFRESH gt_out.
  PERFORM read_qais_master.
  LOOP AT gt_appr INTO gs_appr.
    CLEAR gs_out.
    MOVE-CORRESPONDING gs_appr TO gs_out.
    PERFORM scheme_text USING gs_appr-scheme_type CHANGING gs_out-stype_txt.
    PERFORM fill_extra USING gs_appr CHANGING gs_out.
*   CIS UAT pt.5/7: credit note posted but accounting doc not captured yet
    IF gs_appr-cn_doc IS NOT INITIAL AND gs_appr-acc_doc IS INITIAL.
      PERFORM read_acc_doc USING gs_appr-cn_doc ' '
                           CHANGING gs_appr-acc_doc gs_appr-acc_year lv_posted.
      IF gs_appr-acc_doc IS NOT INITIAL.
        gs_out-acc_doc  = gs_appr-acc_doc.
        gs_out-acc_year = gs_appr-acc_year.
        MODIFY ycis_apprvl FROM gs_appr.
        lv_upd = 'X'.
      ENDIF.
    ENDIF.
*   GAIL UAT 29.10.2026: posted credit notes whose expense G/L line has no
*   text yet get 'INVOICE <reference>' when the Executed view is opened
    IF p_exec = 'X' AND gs_appr-cn_doc IS NOT INITIAL
                    AND gs_appr-cn_doc <> 'GROUP OK'.
      PERFORM set_gl_text USING gs_appr-cn_doc gs_appr-purch_no ' '
                          CHANGING lv_txtok.
    ENDIF.
    APPEND gs_out TO gt_out.
  ENDLOOP.
  IF lv_upd = 'X'.
    COMMIT WORK.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
FORM scheme_text USING p_code TYPE any CHANGING p_txt TYPE char20.
  CASE p_code.
    WHEN 'M'. p_txt = 'Monthly'.
    WHEN 'Q'. p_txt = 'Quarterly'.
    WHEN 'A'. p_txt = 'Annual'.
    WHEN 'C'. p_txt = 'Annual Consistency'.
    WHEN 'T'. p_txt = 'Strategic Monthly'.
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
*  P/R/S Indicator column removed (GAIL UAT 29.10.2026)
  add_fc 'SC_IND'      'Seasonal Indicator' ''.
  add_fc 'WAIVER_1'    'Waiver Month 1'     ''.
  add_fc 'WAIVER_2'    'Waiver Month 2'     ''.
  add_fc 'WV_STAT'     'Waiver Status'   ''.
  add_fc 'LFT_QTY'     'Group Lifted Qty'      ''.
  add_fc 'IND_LFT_QTY' 'Individual Lifted Qty' ''.
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
  IF p_exec = 'X'.
*   display only: no selection column, processing details instead
    DELETE gt_fcat WHERE fieldname = 'SEL'.
    add_fc 'WF_TXT'      'Workflow Status'  ''.
    add_fc 'L6_USER'     'L6 Posted By'  ''.
    add_fc 'L6_DATE'     'L6 Posted On'  ''.
    add_fc 'REM_L6'      'L6 Remark'     ''.
    add_fc 'REJ_BY'      'Rejected By'      ''.
    add_fc 'REJ_DATE'    'Rejected On'      ''.
    add_fc 'REJ_REMARKS' 'Reject Remark'    ''.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
FORM display_alv.
  gs_layout-zebra         = 'X'.
  IF p_exec IS INITIAL.
    gs_layout-box_fieldname = 'SEL'.
  ENDIF.
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
      titlebar              = 'CIS 2026-27 - L6 Posting'
      text_question         =
        'The selected record(s) have been finally approved at L5. Post ' &&
        'the Credit Note(s)? The credit note is posted to accounting, ' &&
        'e-mailed to the customer and the CIS discount is marked Completed.' &&
        ' This action cannot be undone.'
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
  ls_hdr-info = 'Finally approved rebate orders are presented for posting (Credit Note).'.
  APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'.
  ls_hdr-info = 'On posting, the Credit Note is posted & e-mailed and the CIS discount is marked Completed.'.
  APPEND ls_hdr TO lt_hdr.
  IF p_exec = 'X'.
    CLEAR ls_hdr. ls_hdr-typ = 'S'.
    ls_hdr-info = 'Display: records already processed at L6 (display only).'.
    APPEND ls_hdr TO lt_hdr.
  ENDIF.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_hdr.
ENDFORM.

*&---------------------------------------------------------------------*
FORM set_status USING rt_extab TYPE slis_t_extab.            "#EC CALLED
  DATA: lt_ex TYPE slis_t_extab,
        ls_ex TYPE slis_extab.
  lt_ex = rt_extab.
  IF p_exec = 'X'.                     " display only - no actions
    ls_ex-fcode = 'APPR'. APPEND ls_ex TO lt_ex.
    ls_ex-fcode = 'REJ'. APPEND ls_ex TO lt_ex.
    ls_ex-fcode = 'SELALL'. APPEND ls_ex TO lt_ex.
    ls_ex-fcode = 'DESEL'. APPEND ls_ex TO lt_ex.
  ENDIF.
  SET PF-STATUS 'STANDARD' EXCLUDING lt_ex.
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
  IF p_exec = 'X' AND ( r_ucomm = 'APPR' OR r_ucomm = 'REJ' ).
    MESSAGE 'Display only - already processed records' TYPE 'S'.
    RETURN.
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
  TYPES: BEGIN OF ty_cnref,
           cn_doc   TYPE vbeln_vf,
           purch_no TYPE ycis_apprvl-purch_no,
         END OF ty_cnref.
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
        lv_accyr  TYPE gjahr,
        lt_cn     TYPE STANDARD TABLE OF ty_cnref,
        ls_cn     TYPE ty_cnref,
        lv_mail   TYPE i,
        lv_nmail  TYPE i,
        lv_txtok  TYPE flag,
        lv_ntext  TYPE i.

  READ TABLE gt_out INTO gs_out WITH KEY sel = 'X'.
  IF sy-subrc <> 0.
    MESSAGE 'Please select at least one line' TYPE 'I'.
    RETURN.
  ENDIF.

* CIS 2026-27: a remark is MANDATORY at every level for BOTH disburse and reject
  IF p_action = 'A'.
    PERFORM get_remark USING 'Posting remark (mandatory)'      CHANGING lv_remark.
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
      MESSAGE 'Posting cancelled' TYPE 'S'.
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
      gs_appr-remarks   = 'Posted - Credit Note created, CIS discount completed'.
      COLLECT gs_appr-sales_off INTO lt_dofc.
      IF gs_appr-cn_doc IS NOT INITIAL.
        ls_cn-cn_doc   = gs_appr-cn_doc.
        ls_cn-purch_no = gs_appr-purch_no.
        APPEND ls_cn TO lt_cn.
      ENDIF.
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
  LOOP AT lt_cn INTO ls_cn.
*   CIS UAT 23.10.2026: text on the expense G/L line of the accounting
*   document ('INVOICE <reference>') - replaces the manual YRFR126 step
    CLEAR lv_txtok.
    PERFORM set_gl_text USING ls_cn-cn_doc ls_cn-purch_no 'X' CHANGING lv_txtok.
    IF lv_txtok = 'X'.
      lv_ntext = lv_ntext + 1.
    ENDIF.
*   CIS UAT 14.10.2026 pt.8: credit note e-mail to the customer with posting
    CLEAR lv_nmail.
    PERFORM send_cn_mail USING ls_cn-cn_doc CHANGING lv_nmail.
    IF lv_nmail > 0.
      lv_mail = lv_mail + 1.
    ENDIF.
  ENDLOOP.
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
  MESSAGE |{ lv_disb } posted (Credit Note created, Accounting document posted, { lv_ntext } G/L text set, { lv_mail } e-mailed), { lv_rej } returned to L1| TYPE 'S'.
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
        lv_err    TYPE flag,
        lv_posted TYPE flag.

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
*   ATC (S/4HANA, Note 2438131 - material number field length extension):
*   reviewed - no material number is passed or read here (billing data
*   BAPIVBRK only carries the reference order), so no adaptation needed.
  CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE'     "#EC CI_USAGE_OK[2438131]
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
*   CIS UAT 14.10.2026 pt.7: the FI document is written by the update task
*   after the commit, so it was often not yet visible and the program said
*   'not yet posted' although it was. Wait for it (max. 10 s) and treat the
*   billing posting status VBRK-RFBSK = 'C' as posted.
  PERFORM read_acc_doc USING p_cndoc 'X' CHANGING p_accdoc p_accyr lv_posted.
  IF lv_posted IS INITIAL.
    MESSAGE |Credit Note { p_cndoc } created; accounting document not posted - release it in VFX3| TYPE 'I'.
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
        ls_text-line = |The CIS 2026-27 discount/rebate for Sales Office { p_ctxoff } has been posted by L6 (CPC Finance) - Credit Note created and e-mailed to the customer.|.
        APPEND ls_text TO lt_text.
        ls_text-line = |The workflow for these record(s) is now Completed. No further action is required.|.
        APPEND ls_text TO lt_text.
        lv_sub = 'CIS Discount Posted - Workflow Completed'.
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

*&---------------------------------------------------------------------*
*&      Form  read_qais_master
*&---------------------------------------------------------------------*
*   CIS UAT 14.10.2026 pt.1/2: seasonal indicator (P_R_INDICATOR /
*   P_SC_INDICATOR) and monthly waivers (WAIVER_1 / WAIVER_2) of the listed
*   customers, read once from the CIS customer master YRVA_QAIS_DATA.
*&---------------------------------------------------------------------*
FORM read_qais_master.
  REFRESH gt_qais.
  CHECK gt_appr IS NOT INITIAL.
  SELECT * FROM yrva_qais_data INTO TABLE gt_qais
    FOR ALL ENTRIES IN gt_appr
    WHERE kunnr           = gt_appr-kunnr
      AND module_identity = space.
  SORT gt_qais BY kunnr mou_begda DESCENDING.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  fill_extra   (master data + workflow status of one row)
*&---------------------------------------------------------------------*
FORM fill_extra USING    ps_appr TYPE ycis_apprvl
                CHANGING ps_out  TYPE ty_out.
  DATA: ls_qais  TYPE yrva_qais_data,
        ls_first TYPE yrva_qais_data,
        lv_to    TYPE d.
  lv_to = ps_appr-period_to.
  IF lv_to IS INITIAL.
    lv_to = ps_appr-period_from.
  ENDIF.
* scheme record valid for the CIS period - same customer group preferred
  LOOP AT gt_qais INTO ls_qais WHERE kunnr     = ps_appr-kunnr
                                 AND mou_begda LE lv_to
                                 AND mou_endda GE ps_appr-period_from.
    IF ls_first IS INITIAL.
      ls_first = ls_qais.
    ENDIF.
    IF ls_qais-kvgr2 = ps_appr-kvgr2.
      ls_first = ls_qais.
      EXIT.
    ENDIF.
  ENDLOOP.
* not for the Strategic / Additional May-June discount (GAIL 26.10.2026)
  IF ps_appr-scheme_type <> 'T'.
    ps_out-seas_ind = ls_first-p_r_indicator.
    ps_out-sc_ind   = ls_first-p_sc_indicator.
    ps_out-waiver_1 = ls_first-waiver_1.
    ps_out-waiver_2 = ls_first-waiver_2.
  ENDIF.
  PERFORM wf_text USING ps_appr CHANGING ps_out-wf_txt.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  wf_text   (current workflow status as text)
*&---------------------------------------------------------------------*
FORM wf_text USING    ps_appr TYPE ycis_apprvl
             CHANGING p_txt   TYPE char40.
  DATA lv_lvl TYPE c LENGTH 1.
  lv_lvl = ps_appr-wf_status(1).
  CASE ps_appr-wf_status.
    WHEN '10'.
      IF ps_appr-status = 'R'.
        p_txt = |Returned to L1 (rejected at L{ ps_appr-rej_level })|.
      ELSE.
        p_txt = 'Pending L1'.
      ENDIF.
    WHEN '20' OR '30' OR '40' OR '50' OR '60'.
      p_txt = |Pending L{ lv_lvl }|.
    WHEN '70'.
      p_txt = 'Completed (Credit Note posted)'.
    WHEN OTHERS.
      p_txt = ps_appr-wf_status.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  read_acc_doc
*&---------------------------------------------------------------------*
*   FI accounting document of a billing document (BKPF AWTYP 'VBRK').
*   p_wait = 'X' : retry up to 10 s for the update task after posting.
*   p_posted     : 'X' when the FI document exists or the billing document
*                  has posting status 'C' (accounting document created).
*&---------------------------------------------------------------------*
FORM read_acc_doc USING    p_cndoc  TYPE vbeln_vf
                           p_wait   TYPE c
                  CHANGING p_accdoc TYPE belnr_d
                           p_accyr  TYPE gjahr
                           p_posted TYPE flag.
  DATA: lv_awkey TYPE awkey,
        lv_rfbsk TYPE vbrk-rfbsk.
  CLEAR: p_accdoc, p_accyr, p_posted.
  CHECK p_cndoc IS NOT INITIAL.
  lv_awkey = p_cndoc.
  DO 10 TIMES.
    SELECT belnr gjahr FROM bkpf INTO (p_accdoc, p_accyr) UP TO 1 ROWS
      WHERE awtyp = 'VBRK'
        AND awkey = lv_awkey
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF p_accdoc IS NOT INITIAL.
      p_posted = 'X'.
      RETURN.
    ENDIF.
    SELECT SINGLE rfbsk FROM vbrk INTO lv_rfbsk WHERE vbeln = p_cndoc.
    IF lv_rfbsk = 'C'.
      p_posted = 'X'.                  " posted - FI document still in update
    ENDIF.
    IF p_wait IS INITIAL.
      EXIT.
    ENDIF.
    WAIT UP TO 1 SECONDS.
  ENDDO.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  send_cn_mail   (CIS UAT 14.10.2026 pt.8)
*&---------------------------------------------------------------------*
*   E-mails the posted credit note to the customer, as done today with
*   YRVR167 (PDF download, Smart Form YRV_CREDIT_NOTE for rebate credit
*   notes) + YRVR169 (mail to the e-mail IDs of the customer's partners in
*   the billing sales area, sender EINVOICE@GAIL.CO.IN) - now in one step
*   at L6 posting. p_cnt = number of recipients the mail was sent to.
*&---------------------------------------------------------------------*
FORM send_cn_mail USING    p_cndoc TYPE vbeln_vf
                  CHANGING p_cnt   TYPE i.
  CONSTANTS lc_form TYPE tdsfname VALUE 'YRV_CREDIT_NOTE'.
  DATA: ls_vbrk         TYPE vbrk,
        lv_fm           TYPE rs38l_fnam,
        ls_ctrl         TYPE ssfctrlop,
        ls_opt          TYPE ssfcompop,
        ls_docinfo      TYPE ssfcrespd,
        ls_jobinfo      TYPE ssfcrescl,
        ls_jobopt       TYPE ssfcresop,
        lv_devtype      TYPE rspoptype,
        x_yrva_rebate   TYPE yrvs_rebate,
        w_bill          LIKE vbrk-vbeln,
        w_aubel         TYPE vbeln_va,
        w_fkdat         TYPE fkdat,
        w_fkimg         LIKE vbrp-fkimg,
        w_netwr         LIKE vbrk-netwr,
        w_vkbur         TYPE vkbur,
        w_bstkd         LIKE vbkd-bstkd,
        w_from_date     TYPE d,
        w_to_date       TYPE d,
        w_bezei         TYPE bezei,
        w_text          TYPE char200,
        w_kunnr         LIKE vbpa-kunnr,
        w_adrnr         LIKE kna1-adrnr,
        x_adrc          LIKE adrc,
        lt_lines        TYPE STANDARD TABLE OF tline,
        lv_size         TYPE i,
        lv_pdf          TYPE xstring,
        lt_hex          TYPE solix_tab,
        lt_knvp         TYPE STANDARD TABLE OF knvp,
        ls_knvp         TYPE knvp,
        lt_partner      TYPE STANDARD TABLE OF kunnr,
        lv_partner      TYPE kunnr,
        lt_adr6         TYPE STANDARD TABLE OF adr6,
        ls_adr6         TYPE adr6,
        lt_body         TYPE bcsy_text,
        ls_line         TYPE soli,
        lv_subj         TYPE so_obj_des,
        lv_att          TYPE so_obj_des,
        lv_cust         TYPE char10,
        lv_d1           TYPE char10,
        lv_d2           TYPE char10,
        lo_send         TYPE REF TO cl_bcs,
        lo_doc          TYPE REF TO cl_document_bcs,
        lo_rec          TYPE REF TO if_recipient_bcs,
        lo_sender       TYPE REF TO if_sender_bcs,
        lv_addr         TYPE ad_smtpadr.

  CLEAR p_cnt.
  SELECT SINGLE * FROM vbrk INTO ls_vbrk WHERE vbeln = p_cndoc.
  CHECK sy-subrc = 0.

* ---- credit note PDF (same data as YRVR167, Smart Form YRV_CREDIT_NOTE)
  w_bill  = p_cndoc.
  w_fkdat = ls_vbrk-fkdat.
  w_netwr = ls_vbrk-netwr.
  SELECT aubel fkimg vkbur INTO (w_aubel, w_fkimg, w_vkbur) FROM vbrp
    UP TO 1 ROWS WHERE vbeln = w_bill ORDER BY PRIMARY KEY.
  ENDSELECT.
  SELECT bstkd bstdk bstdk_e INTO (w_bstkd, w_from_date, w_to_date) FROM vbkd
    UP TO 1 ROWS WHERE vbeln = w_aubel ORDER BY PRIMARY KEY.
  ENDSELECT.
  SELECT * INTO CORRESPONDING FIELDS OF x_yrva_rebate FROM yrva_rebate
    UP TO 1 ROWS WHERE vbeln = w_aubel ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF sy-subrc <> 0.
    x_yrva_rebate-yy_per_start = w_from_date.
    x_yrva_rebate-yy_per_end   = w_to_date.
    x_yrva_rebate-vkbur        = w_vkbur.
  ENDIF.
  SELECT SINGLE bezei INTO w_bezei FROM tvkbt
    WHERE spras = 'EN' AND vkbur = x_yrva_rebate-vkbur.
  SELECT kunnr INTO w_kunnr FROM vbpa UP TO 1 ROWS
    WHERE vbeln = w_aubel AND parvw = 'WE' ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF w_kunnr IS NOT INITIAL.
    SELECT SINGLE adrnr INTO w_adrnr FROM kna1 WHERE kunnr = w_kunnr.
    SELECT * INTO x_adrc FROM adrc UP TO 1 ROWS
      WHERE addrnumber = w_adrnr ORDER BY PRIMARY KEY.
    ENDSELECT.
  ENDIF.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lc_form
    IMPORTING
      fm_name            = lv_fm
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
    MESSAGE |Credit Note { p_cndoc }: form { lc_form } not found - e-mail not sent| TYPE 'I'.
    RETURN.
  ENDIF.
  CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
    EXPORTING
      i_language    = 'E'
      i_application = 'SAPDEFAULT'
    IMPORTING
      e_devtype     = lv_devtype
    EXCEPTIONS
      OTHERS        = 1.
  ls_opt-tdprinter   = lv_devtype.
  ls_ctrl-no_dialog  = 'X'.
  ls_ctrl-getotf     = 'X'.
  CALL FUNCTION lv_fm
    EXPORTING
      control_parameters   = ls_ctrl
      output_options       = ls_opt
      x_yrva_rebate        = x_yrva_rebate
      w_vbeln              = w_bill
      w_fkdat              = w_fkdat
      x_adrc               = x_adrc
      w_netwr              = w_netwr
      w_fkimg              = w_fkimg
      w_bezei              = w_bezei
      w_text               = w_text
      w_bstkd              = w_bstkd
    IMPORTING
      document_output_info = ls_docinfo
      job_output_info      = ls_jobinfo
      job_output_options   = ls_jobopt
    EXCEPTIONS
      formatting_error     = 1
      internal_error       = 2
      send_error           = 3
      user_canceled        = 4
      OTHERS               = 5.
  IF sy-subrc <> 0.
    MESSAGE |Credit Note { p_cndoc }: PDF could not be generated - e-mail not sent| TYPE 'I'.
    RETURN.
  ENDIF.
  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format       = 'PDF'
    IMPORTING
      bin_filesize = lv_size
      bin_file     = lv_pdf
    TABLES
      otf          = ls_jobinfo-otfdata
      lines        = lt_lines
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0 OR lv_pdf IS INITIAL.
    MESSAGE |Credit Note { p_cndoc }: PDF conversion failed - e-mail not sent| TYPE 'I'.
    RETURN.
  ENDIF.
  lt_hex = cl_bcs_convert=>xstring_to_solix( iv_xstring = lv_pdf ).

* ---- recipients: e-mail IDs of the customer's partners (as YRVR169)
  SELECT * FROM knvp INTO TABLE lt_knvp
    WHERE kunnr = ls_vbrk-kunag
      AND vkorg = ls_vbrk-vkorg
      AND vtweg = ls_vbrk-vtweg
      AND spart = ls_vbrk-spart.
  LOOP AT lt_knvp INTO ls_knvp.
    lv_partner = ls_knvp-kunn2.
    IF lv_partner IS INITIAL.
      lv_partner = ls_knvp-lifnr.
    ENDIF.
    CHECK lv_partner IS NOT INITIAL.
    COLLECT lv_partner INTO lt_partner.
  ENDLOOP.
  IF lt_partner IS INITIAL.
    APPEND ls_vbrk-kunag TO lt_partner.
  ENDIF.
  SELECT b~addrnumber b~persnumber b~smtp_addr
    FROM kna1 AS a INNER JOIN adr6 AS b ON b~addrnumber = a~adrnr
    INTO CORRESPONDING FIELDS OF TABLE lt_adr6
    FOR ALL ENTRIES IN lt_partner
    WHERE a~kunnr = lt_partner-table_line.
  DELETE lt_adr6 WHERE persnumber IS NOT INITIAL OR smtp_addr IS INITIAL.
  SORT lt_adr6 BY smtp_addr.
  DELETE ADJACENT DUPLICATES FROM lt_adr6 COMPARING smtp_addr.
  IF lt_adr6 IS INITIAL.
    MESSAGE |Credit Note { p_cndoc }: no customer e-mail ID maintained - e-mail not sent| TYPE 'I'.
    RETURN.
  ENDIF.

* ---- mail (text as YRVR169)
  lv_d1 = |{ w_from_date+6(2) }.{ w_from_date+4(2) }.{ w_from_date(4) }|.
  lv_d2 = |{ w_to_date+6(2) }.{ w_to_date+4(2) }.{ w_to_date(4) }|.
  IF sy-mandt <> '300'.
    APPEND 'THIS IS A TEST MAIL, PLEASE IGNORE' TO lt_body.
  ENDIF.
  APPEND 'DEAR SIR/MADAM,' TO lt_body.
  APPEND ' ' TO lt_body.
  APPEND 'GREETINGS FROM GAIL (INDIA) LTD.' TO lt_body.
  APPEND ' ' TO lt_body.
  ls_line-line = |PLEASE FIND ATTACHED CREDIT NOTE NO. { p_cndoc } FOR { w_bstkd } REBATE PERIOD { lv_d1 } TO { lv_d2 }.|.
  APPEND ls_line TO lt_body.
  APPEND ' ' TO lt_body.
  APPEND 'WE ALSO WISH TO BRING TO YOUR NOTICE THAT THE DOMAIN NAME USED BY GAIL FOR ALL ITS MAIL COMMUNICATIONS IS GAIL.CO.IN ONLY.' TO lt_body.
  APPEND ' ' TO lt_body.
  APPEND 'THANKING YOU,' TO lt_body.
  APPEND 'GAIL (INDIA) LTD.' TO lt_body.
  APPEND ' ' TO lt_body.
  APPEND 'THIS IS A SYSTEM GENERATED MAIL, PLEASE DO NOT REPLY. FOR ANY CLARIFICATIONS PLEASE CONTACT THE CONCERNED GAIL MARKETING OFFICE.' TO lt_body.
  APPEND ' ' TO lt_body.
  ls_line-line = |SOURCE:YRVG004_P.{ sy-uname }.{ sy-datum DATE = USER }.{ sy-uzeit TIME = USER }|.
  APPEND ls_line TO lt_body.

  lv_cust = |{ ls_vbrk-kunag ALPHA = OUT }|.
  CONDENSE lv_cust.
  lv_subj = |CREDIT NOTE-{ p_cndoc }-{ lv_cust }|.
  lv_att  = |{ ls_vbrk-kunag }_{ p_cndoc }|.

  TRY.
      lo_send = cl_bcs=>create_persistent( ).
      lo_doc  = cl_document_bcs=>create_document(
                  i_type    = 'RAW'
                  i_text    = lt_body
                  i_subject = lv_subj ).
      lo_doc->add_attachment(
        i_attachment_type    = 'PDF'
        i_attachment_subject = lv_att
        i_att_content_hex    = lt_hex ).
      lo_send->set_document( lo_doc ).
      lo_sender = cl_cam_address_bcs=>create_internet_address( 'EINVOICE@GAIL.CO.IN' ).
      lo_send->set_sender( lo_sender ).
      LOOP AT lt_adr6 INTO ls_adr6.
        lv_addr = ls_adr6-smtp_addr.
        lo_rec  = cl_cam_address_bcs=>create_internet_address( lv_addr ).
        lo_send->add_recipient( i_recipient = lo_rec i_express = 'X' ).
        p_cnt = p_cnt + 1.
      ENDLOOP.
      lo_send->set_status_attributes( i_requested_status = 'N' i_status_mail = 'N' ).
      lo_send->set_send_immediately( 'X' ).
      lo_send->send( i_with_error_screen = 'X' ).
      COMMIT WORK.
    CATCH cx_bcs.
      CLEAR p_cnt.
      MESSAGE |Credit Note { p_cndoc }: e-mail could not be sent| TYPE 'I'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  set_gl_text   (CIS UAT 23.10.2026)
*&---------------------------------------------------------------------*
*   Writes the line-item text 'INVOICE <reference>' (e.g. 'INVOICE CIS 26-27
*   JUNE DISCO', reference = YCIS_APPRVL-PURCH_NO / order VBKD-BSTKD) on the
*   G/L lines (KOART 'S' - expense GL 'Discount on Sales - MOU Incentive
*   Scheme') of the accounting document of the credit note. Zonal F&A did
*   this manually with YRFR126 after posting. Lines that already carry a
*   text are left unchanged. Uses the standard document change
*   (FI_DOCUMENT_CHANGE, same rules as FB02 - BSEG-SGTXT must be changeable
*   in the document change rules, OB32).
*&---------------------------------------------------------------------*
FORM set_gl_text USING    p_cndoc TYPE vbeln_vf
                          p_ref   TYPE clike
                          p_show  TYPE c
                 CHANGING p_ok    TYPE flag.
  DATA: ls_bkpf   TYPE bkpf,
        lt_chg    TYPE STANDARD TABLE OF accchg,
        ls_chg    TYPE accchg,
        lv_awkey  TYPE awkey,
        lv_ref    TYPE bstkd,
        lv_text   TYPE sgtxt,
        lv_aubel  TYPE vbeln_va.

  CLEAR p_ok.
  CHECK p_cndoc IS NOT INITIAL.
  lv_awkey = p_cndoc.
  SELECT * FROM bkpf INTO ls_bkpf UP TO 1 ROWS
    WHERE awtyp = 'VBRK'
      AND awkey = lv_awkey
    ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF sy-subrc <> 0.
    RETURN.                              " accounting document not (yet) there
  ENDIF.

* reference = CIS reference no. (YRVG052 reference), else the order's BSTKD
  lv_ref = p_ref.
  IF lv_ref IS INITIAL.
    SELECT aubel FROM vbrp INTO lv_aubel UP TO 1 ROWS
      WHERE vbeln = p_cndoc ORDER BY PRIMARY KEY.
    ENDSELECT.
    SELECT SINGLE bstkd FROM vbkd INTO lv_ref
      WHERE vbeln = lv_aubel AND posnr = '000000'.
  ENDIF.
  CHECK lv_ref IS NOT INITIAL.
  lv_text = |INVOICE { lv_ref }|.

* G/L line items of the document - read through the released S/4HANA view
* I_OperationalAcctgDocItem instead of a direct BSEG select (ATC, S/4HANA)
  SELECT accountingdocumentitem, documentitemtext
    FROM i_operationalacctgdocitem
    WHERE companycode          = @ls_bkpf-bukrs
      AND accountingdocument   = @ls_bkpf-belnr
      AND fiscalyear           = @ls_bkpf-gjahr
      AND financialaccounttype = 'S'
    INTO TABLE @DATA(lt_item).
  LOOP AT lt_item INTO DATA(ls_item) WHERE documentitemtext IS INITIAL.
    REFRESH lt_chg.
    ls_chg-fdname = 'SGTXT'.
    ls_chg-oldval = ls_item-documentitemtext.
    ls_chg-newval = lv_text.
    APPEND ls_chg TO lt_chg.
*   GAIL UAT 29.10.2026: the text was still blank - the change is now
*   addressed to the FI document line itself (company code / document /
*   year / line item). The reference fields AWTYP/AWREF/AWORG/OBZEI are no
*   longer passed: OBZEI is the line of the ORIGINAL (billing) document, not
*   the FI line item, so the line was not found and nothing was changed.
    CALL FUNCTION 'FI_DOCUMENT_CHANGE'
      EXPORTING
        i_bukrs              = ls_bkpf-bukrs
        i_belnr              = ls_bkpf-belnr
        i_gjahr              = ls_bkpf-gjahr
        i_buzei              = ls_item-accountingdocumentitem
      TABLES
        t_accchg             = lt_chg
      EXCEPTIONS
        no_reference         = 1
        no_document          = 2
        many_documents       = 3
        wrong_input          = 4
        overwrite_creditcard = 5
        OTHERS               = 6.
    IF sy-subrc = 0.
      p_ok = 'X'.
    ELSEIF p_show = 'X'.
*     show why the text could not be set (e.g. document change rules OB32)
      IF sy-msgid IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        MESSAGE |G/L text not set on document { ls_bkpf-belnr } line { ls_item-accountingdocumentitem } (FI_DOCUMENT_CHANGE rc { sy-subrc })| TYPE 'I'.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF p_ok = 'X'.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDFORM.
