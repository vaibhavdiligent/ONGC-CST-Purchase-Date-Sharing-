*&---------------------------------------------------------------------*
*& Report  YCIS_APPRV5
*&---------------------------------------------------------------------*
*& CIS 2026-27 - 6-level approval workflow : LEVEL 5 (CPC Head - Final
*& Approval).
*&
*&   L4 (CPC Finance) vets the rebate order -> WF_STATUS '50' (Pending L5).
*&   THIS program lists those Pending-L5 rows for the CPC Head:
*&     APPROVE (Final) -> WF_STATUS '60' (Pending L6 - Disbursement), mail L6.
*&     REJECT          -> WF_STATUS '10' (back to L1 for reinitiation), mail L1.
*&
*&   L5 is central (maintained in YCIS_WF_APPR under sales office '0001',
*&   level 5) and sees the Pending-L5 rows of ALL sales offices.
*&
*& GUI status 'STANDARD' (function codes APPR, REJ, SELALL, DESEL, BACK,
*& EXIT) must exist in this program - create it in SE41 (copy from the L2
*& program YCIS_APPROVE).
*&---------------------------------------------------------------------*
REPORT  ycis_apprv5.

TYPE-POOLS: slis.

TABLES: ycis_apprvl, ycis_wf_appr.

CONSTANTS: gc_level TYPE ycis_wlevel VALUE '5'.   " this program = Level 5

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
         mcq_qty     TYPE ycis_apprvl-mcq_qty,
         lft_qty     TYPE ycis_apprvl-lft_qty,       " group lifted qty
         ind_lft_qty TYPE ycis_apprvl-ind_lft_qty,   " individual lifted qty
         mcq_perc    TYPE ycis_apprvl-mcq_perc,
         elig_qty    TYPE ycis_apprvl-elig_qty,
         rebate_val  TYPE ycis_apprvl-rebate_val,
         order_no    TYPE ycis_apprvl-order_no,
         purch_no    TYPE ycis_apprvl-purch_no,
         l1_user     TYPE ycis_apprvl-l1_user,
         l2_user     TYPE ycis_apprvl-l2_user,
         l3_user     TYPE ycis_apprvl-l3_user,
         l4_user     TYPE ycis_apprvl-l4_user,
         l4_date     TYPE ycis_apprvl-l4_date,
         remarks     TYPE ycis_apprvl-remarks,
*        CIS UAT 14.10.2026 pt.3: 'Executed' view - processing details
         wf_txt      TYPE char40,
         l5_user     TYPE ycis_apprvl-l5_user,
         l5_date     TYPE ycis_apprvl-l5_date,
         rem_l5      TYPE ycis_apprvl-rem_l5,
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
  c_unex = 'Unexecuted (pending at L5)'.
  c_exec = 'Executed (already processed at L5)'.
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
    MESSAGE 'You are not maintained as a Level-5 (CPC Head) approver (YCIS_WF_APPR)' TYPE 'I'.
    RETURN.
  ENDIF.
  PERFORM get_pending.
  IF gt_appr IS INITIAL.
    IF p_exec = 'X'.
      MESSAGE 'No records already processed at L5 for the selection' TYPE 'I'.
    ELSE.
      MESSAGE 'No records pending your (L5) final approval' TYPE 'I'.
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
  IF p_exec = 'X'.
    SELECT * FROM ycis_apprvl INTO TABLE gt_appr
      WHERE ( l5_user <> space OR rej_level = gc_level )   " processed at L5
        AND scheme_type IN gr_stype
        AND sales_off   IN s_vkbur
        AND period_from IN s_sptag
        AND kunnr       IN s_kunnr
        AND kvgr2       IN s_kvgr2.
  ELSE.
    SELECT * FROM ycis_apprvl INTO TABLE gt_appr
      WHERE wf_status    = '50'                       " Pending L5
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
  REFRESH gt_out.
  PERFORM read_qais_master.
  LOOP AT gt_appr INTO gs_appr.
    CLEAR gs_out.
    MOVE-CORRESPONDING gs_appr TO gs_out.
    PERFORM scheme_text USING gs_appr-scheme_type CHANGING gs_out-stype_txt.
    PERFORM fill_extra USING gs_appr CHANGING gs_out.
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
  add_fc 'SEAS_IND'    'P/R/S Indicator'    ''.
  add_fc 'SC_IND'      'Seasonal Indicator' ''.
  add_fc 'WAIVER_1'    'Waiver Month 1'     ''.
  add_fc 'WAIVER_2'    'Waiver Month 2'     ''.
  add_fc 'WV_STAT'     'Waiver Status'   ''.
  add_fc 'MCQ_QTY'     'Committed Qty'   ''.
  add_fc 'LFT_QTY'     'Group Lifted Qty'      ''.
  add_fc 'IND_LFT_QTY' 'Individual Lifted Qty' ''.
  add_fc 'MCQ_PERC'    'MCQ %'           ''.
  add_fc 'ELIG_QTY'    'Eligible Qty'    ''.
  add_fc 'REBATE_VAL'  'Rebate Value'    ''.
  add_fc 'ORDER_NO'    'Rebate Order'    ''.
  add_fc 'PURCH_NO'    'Reference No'    ''.
  add_fc 'L1_USER'     'L1 Verified By'  ''.
  add_fc 'L2_USER'     'L2 Verified By'  ''.
  add_fc 'L3_USER'     'L3 Executed By'  ''.
  add_fc 'L4_USER'     'L4 Vetted By'    ''.
  add_fc 'L4_DATE'     'L4 Vetted On'    ''.
  add_fc 'REMARKS'     'Remarks'         ''.
  IF p_exec = 'X'.
*   display only: no selection column, processing details instead
    DELETE gt_fcat WHERE fieldname = 'SEL'.
    add_fc 'WF_TXT'      'Workflow Status'  ''.
    add_fc 'L5_USER'     'L5 Approved By'  ''.
    add_fc 'L5_DATE'     'L5 Approved On'  ''.
    add_fc 'REM_L5'      'L5 Remark'     ''.
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
      titlebar              = 'CIS 2026-27 - L5 Final Approval'
      text_question         =
        'The rebate orders have been financially vetted at L4. Grant final ' &&
        'approval and forward the selected record(s) to L6 (CPC Finance) ' &&
        'for disbursement?'
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
  ls_hdr-info = 'CIS 2026-27 L-5 Final Approval (CPC Head)'.
  APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'.
  ls_hdr-info = 'Rebate orders vetted by L4 are presented for final approval.'.
  APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'.
  ls_hdr-info = 'On approval, the request is forwarded to L6 (CPC Finance) for disbursement.'.
  APPEND ls_hdr TO lt_hdr.
  IF p_exec = 'X'.
    CLEAR ls_hdr. ls_hdr-typ = 'S'.
    ls_hdr-info = 'Display: records already processed at L5 (display only).'.
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
*&      Form  process_selected   (A = final approve, R = reject)
*&---------------------------------------------------------------------*
FORM process_selected USING p_action TYPE char1.
  DATA: lv_appr   TYPE i,
        lv_rej    TYPE i,
        lv_remark TYPE ycis_apprvl-rej_remarks,
        lt_office TYPE STANDARD TABLE OF vkbur,
        lv_ans    TYPE c,
        lv_off    TYPE vkbur.

  READ TABLE gt_out INTO gs_out WITH KEY sel = 'X'.
  IF sy-subrc <> 0.
    MESSAGE 'Please select at least one line' TYPE 'I'.
    RETURN.
  ENDIF.

* CIS 2026-27: a remark is MANDATORY at every level for BOTH approve and reject
  IF p_action = 'A'.
    PERFORM get_remark USING 'Approval remark (mandatory)' CHANGING lv_remark.
  ELSE.
    PERFORM get_remark USING 'Reject remark (mandatory)'   CHANGING lv_remark.
  ENDIF.
  IF lv_remark IS INITIAL.
    MESSAGE 'Remark is mandatory' TYPE 'I'.
    RETURN.
  ENDIF.

  IF p_action = 'A'.
    PERFORM show_stmt_popup CHANGING lv_ans.
    IF lv_ans <> '1'.
      MESSAGE 'Final approval cancelled - nothing sent to L6' TYPE 'S'.
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
      gs_appr-wf_status = '60'.            " Pending L6 (CPC Finance - Disbursement)
      gs_appr-status    = 'P'.
      gs_appr-l5_user   = sy-uname.
      gs_appr-l5_date   = sy-datum.
      gs_appr-l5_time   = sy-uzeit.
      gs_appr-rem_l5    = lv_remark.                 " L5 approval remark (prints on note)
      gs_appr-remarks   = 'L5 final approved'.
      lv_appr = lv_appr + 1.
    ELSE.
      gs_appr-wf_status   = '10'.          " back to L1 (reinitiation)
      gs_appr-status      = 'R'.
      gs_appr-rej_level   = gc_level.
      gs_appr-rej_by      = sy-uname.
      gs_appr-rej_date    = sy-datum.
      gs_appr-rej_time    = sy-uzeit.
      gs_appr-rej_remarks = lv_remark.
      gs_appr-rem_l5      = lv_remark.
      gs_appr-remarks     = 'Returned by L5'.
      COLLECT gs_appr-sales_off INTO lt_office.
      lv_rej = lv_rej + 1.
    ENDIF.
    MODIFY ycis_apprvl FROM gs_appr.
  ENDLOOP.

  IF lv_appr > 0 OR lv_rej > 0.
    COMMIT WORK.
  ENDIF.
*   forwarded to L6 (CPC Finance) - central (office '0001', level 6): one mail
  IF lv_appr > 0.
    PERFORM send_mail USING '6' '0001' '0001'.
  ENDIF.
*   rejected -> back to L1 for reinitiation: notify each affected office
  IF lv_rej > 0.
    LOOP AT lt_office INTO lv_off.
      PERFORM send_mail USING '1' lv_off lv_off.
    ENDLOOP.
  ENDIF.
  DELETE gt_out WHERE sel = 'X'.
  MESSAGE |{ lv_appr } approved & forwarded to L6, { lv_rej } returned to L1| TYPE 'S'.
ENDFORM.

*&---------------------------------------------------------------------*
FORM get_remark USING p_title TYPE clike
                CHANGING p_remark TYPE ycis_apprvl-rej_remarks.
  DATA: lt_fields TYPE STANDARD TABLE OF sval,
        ls_field  TYPE sval,
        lv_ret    TYPE char1.
  ls_field-tabname   = 'YCIS_APPRVL'.
  ls_field-fieldname = 'REM_L5'.               " 100-char remark field
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
*&      Form  send_mail   (p_level = target level; '6' forward, '1' reject)
*&---------------------------------------------------------------------*
FORM send_mail USING p_level  TYPE ycis_wlevel
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

  SELECT * FROM ycis_wf_appr INTO TABLE lt_wf
    WHERE wf_level = p_level AND sales_office = p_office.
  CHECK lt_wf IS NOT INITIAL.

  TRY.
      lo_send = cl_bcs=>create_persistent( ).
      CLEAR lt_text.
      ls_text-line = |Dear Sir/Madam,|.                            APPEND ls_text TO lt_text.
      ls_text-line = ||.                                            APPEND ls_text TO lt_text.
      IF p_level = '6'.
        ls_text-line = |Final approval of the CIS 2026-27 rebate orders has been granted by L5 (CPC Head).|.
        APPEND ls_text TO lt_text.
        ls_text-line = |Please log in and process the disbursement (L6 - CPC Finance).|.
        APPEND ls_text TO lt_text.
        lv_sub = 'CIS Discount Approved by L5 - Action Required at L6'.
      ELSE.
        ls_text-line = |The CIS 2026-27 rebates for Sales Office { p_ctxoff } have been returned by L5 (CPC Head).|.
        APPEND ls_text TO lt_text.
        ls_text-line = |Please log in to T-Code YRVG004 (Run CIS Scheme) and reinitiate the process.|.
        APPEND ls_text TO lt_text.
        lv_sub = 'CIS Discount Request Rejected by L5 - Reinitiation Required at L1'.
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
  ps_out-seas_ind = ls_first-p_r_indicator.
  ps_out-sc_ind   = ls_first-p_sc_indicator.
  ps_out-waiver_1 = ls_first-waiver_1.
  ps_out-waiver_2 = ls_first-waiver_2.
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
