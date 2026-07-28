*&---------------------------------------------------------------------*
*& Report  YCIS_EXECUTE
*&---------------------------------------------------------------------*
*& CIS 2026-27 - 3-level approval workflow : LEVEL 3 (CPC - Execution).
*&
*&   L3 (CPC) is central (maintained in YCIS_WF_APPR under sales office
*&   '0001', level 3) and sees the Pending-L3 rows of ALL sales offices.
*&     EXECUTE -> create the rebate order (credit-memo request) via BAPI,
*&                WF_STATUS '40' (Completed), store the order number.
*&     REJECT  -> WF_STATUS '20' (back to L2 - PC MKTG-HOD), e-mail L2.
*&
*& GUI status 'STANDARD' (function codes EXEC, REJ, SELALL, DESEL, BACK,
*& EXIT) must exist in this program - create it in SE41 (see doc).
*&---------------------------------------------------------------------*
REPORT  ycis_execute.

TYPE-POOLS: slis.

TABLES: ycis_apprvl, ycis_wf_appr.

CONSTANTS: gc_level TYPE ycis_wlevel VALUE '3'.
*   Confirmation statement (verified & confirmed at L1 and L2), shown as the
*   L3 report header for onward processing.
CONSTANTS:
  gc_stmt1 TYPE string VALUE
    'Customer-wise, grade-wise sales quantities, along with',
  gc_stmt2 TYPE string VALUE
    'eligible PSD rates and amounts, have been verified and',
  gc_stmt3 TYPE string VALUE
    'confirmed after considering customer waivers, shortfall',
  gc_stmt4 TYPE string VALUE
    'waivers, sales return quantities, and Group/MLE details.'.

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
         lft_qty     TYPE ycis_apprvl-lft_qty,       " group lifted qty
         ind_lft_qty TYPE ycis_apprvl-ind_lft_qty,   " individual lifted qty
         mcq_perc    TYPE ycis_apprvl-mcq_perc,
         elig_qty    TYPE ycis_apprvl-elig_qty,
         rebate_val  TYPE ycis_apprvl-rebate_val,
         purch_no    TYPE ycis_apprvl-purch_no,
*        L1 & L2 verification status captured for L3 onward processing
         l1_user     TYPE ycis_apprvl-l1_user,
         l1_date     TYPE ycis_apprvl-l1_date,
         l1_time     TYPE ycis_apprvl-l1_time,
         l2_user     TYPE ycis_apprvl-l2_user,
         l2_date     TYPE ycis_apprvl-l2_date,
         l2_time     TYPE ycis_apprvl-l2_time,
         order_no    TYPE ycis_apprvl-order_no,
         remarks     TYPE ycis_apprvl-remarks,
       END OF ty_out.

DATA: gt_appr  TYPE STANDARD TABLE OF ycis_apprvl,
      gs_appr  TYPE ycis_apprvl,
      gt_out   TYPE STANDARD TABLE OF ty_out,
      gs_out   TYPE ty_out,
      gt_fcat  TYPE slis_t_fieldcat_alv,
      gs_fcat  TYPE slis_fieldcat_alv,
      gs_layout TYPE slis_layout_alv,
      gv_isl3  TYPE flag.

*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_sptag FOR ycis_apprvl-period_from,
                s_vkbur FOR ycis_apprvl-sales_off,
                s_kunnr FOR ycis_apprvl-kunnr,
                s_kvgr2 FOR ycis_apprvl-kvgr2.
SELECTION-SCREEN END OF BLOCK b1.

*--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM check_l3_auth.
  IF gv_isl3 IS INITIAL.
    MESSAGE 'You are not maintained as a Level-3 (CPC) executor (YCIS_WF_APPR)' TYPE 'I'.
    RETURN.
  ENDIF.
  PERFORM get_pending.
  IF gt_appr IS INITIAL.
    MESSAGE 'No records pending L3 execution' TYPE 'I'.
    RETURN.
  ENDIF.
  PERFORM build_out.
  PERFORM build_fieldcat.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
FORM check_l3_auth.
  DATA lv_cnt TYPE i.
  SELECT COUNT(*) INTO lv_cnt FROM ycis_wf_appr
    WHERE wf_level = gc_level AND userid = sy-uname.
  IF lv_cnt > 0.
    gv_isl3 = 'X'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
FORM get_pending.
*   CPC is central -> all offices' pending-L3 rows (narrowed by s_vkbur)
  SELECT * FROM ycis_apprvl INTO TABLE gt_appr
    WHERE wf_status   = '30'
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
  add_fc 'SALES_OFF'   'Sales Office'         ''.
  add_fc 'MCQ_QTY'     'Committed Qty'        ''.
  add_fc 'LFT_QTY'     'Group Lifted Qty'     ''.
  add_fc 'IND_LFT_QTY' 'Individual Lifted Qty' ''.
  add_fc 'MCQ_PERC'    'MCQ %'                ''.
  add_fc 'ELIG_QTY'    'Eligible Qty'         ''.
  add_fc 'REBATE_VAL'  'Rebate Value'    ''.
  add_fc 'PURCH_NO'    'Reference No'    ''.
  add_fc 'L1_USER'     'L1 Verified By'  ''.
  add_fc 'L1_DATE'     'L1 Verified On'  ''.
  add_fc 'L1_TIME'     'L1 Verified At'  ''.
  add_fc 'L2_USER'     'L2 Verified By'  ''.
  add_fc 'L2_DATE'     'L2 Verified On'  ''.
  add_fc 'L2_TIME'     'L2 Verified At'  ''.
  add_fc 'ORDER_NO'    'Rebate Order'    ''.
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
*&      Form  top_of_page   (confirmation statement header - L1/L2 verified)
*&---------------------------------------------------------------------*
FORM top_of_page.                                           "#EC CALLED
  DATA: lt_hdr TYPE slis_t_listheader,
        ls_hdr TYPE slis_listheader.
  CLEAR ls_hdr. ls_hdr-typ = 'H'.
  ls_hdr-info = 'CIS 2026-27 - Level-3 Execution (verified & confirmed at L1 and L2)'.
  APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'. ls_hdr-info = gc_stmt1. APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'. ls_hdr-info = gc_stmt2. APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'. ls_hdr-info = gc_stmt3. APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'. ls_hdr-info = gc_stmt4. APPEND ls_hdr TO lt_hdr.
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
    WHEN 'EXEC'.
      PERFORM process_selected USING 'E'.
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
*&      Form  process_selected   (E = execute/create order, R = reject)
*&---------------------------------------------------------------------*
FORM process_selected USING p_action TYPE char1.
  DATA: lv_cnt    TYPE i,
        lv_err    TYPE i,
        lv_dup    TYPE i,
        lv_remark TYPE ycis_apprvl-rej_remarks,
        lt_office TYPE STANDARD TABLE OF vkbur,
        lv_off    TYPE vkbur,
        lv_vbeln  TYPE vbeln,
        lv_exord  TYPE ycis_apprvl-order_no,
        lv_exstat TYPE ycis_apprvl-wf_status.

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

    IF p_action = 'E'.
*     Guard against a duplicate rebate order: re-read the current record
*     from the database (the in-memory copy can be stale after an earlier
*     Execute in the same session). If it already carries an order / is
*     Completed, tell the user and skip - do NOT create a second order.
      CLEAR: lv_exord, lv_exstat.
      SELECT SINGLE order_no wf_status
        INTO (lv_exord, lv_exstat)
        FROM ycis_apprvl
        WHERE qais_no     = gs_appr-qais_no
          AND scheme_type = gs_appr-scheme_type
          AND period_from = gs_appr-period_from
          AND period_to   = gs_appr-period_to
          AND kunnr       = gs_appr-kunnr
          AND kvgr2       = gs_appr-kvgr2.
      IF lv_exord IS NOT INITIAL OR lv_exstat = '40'.
        lv_dup = lv_dup + 1.
        gs_out-order_no = lv_exord.
        gs_out-remarks  = |Rebate order { lv_exord } already created|.
        CLEAR gs_out-sel.
        MODIFY gt_out FROM gs_out.
        CONTINUE.
      ENDIF.

      CLEAR lv_vbeln.
      PERFORM create_order USING gs_appr CHANGING lv_vbeln.
      IF lv_vbeln IS NOT INITIAL.
        gs_appr-wf_status = '40'.        " Completed
        gs_appr-status    = 'A'.         " Approved - order created (clears 'P' Pending)
        gs_appr-order_no  = lv_vbeln.
        gs_appr-l3_user   = sy-uname.
        gs_appr-l3_date   = sy-datum.
        gs_appr-l3_time   = sy-uzeit.
        gs_appr-remarks   = 'Executed - order created'.
        MODIFY ycis_apprvl FROM gs_appr.
        lv_cnt = lv_cnt + 1.
*       keep the row on the main grid and show the created rebate order
        gs_out-order_no = lv_vbeln.
        gs_out-remarks  = 'Executed - order created'.
        CLEAR gs_out-sel.
        MODIFY gt_out FROM gs_out.
      ELSE.
        lv_err = lv_err + 1.
        gs_out-remarks = 'Order creation failed'.
        CLEAR gs_out-sel.
        MODIFY gt_out FROM gs_out.
      ENDIF.
    ELSE.
      gs_appr-wf_status   = '20'.     " back to L2
      gs_appr-rej_level   = gc_level.
      gs_appr-rej_by      = sy-uname.
      gs_appr-rej_date    = sy-datum.
      gs_appr-rej_time    = sy-uzeit.
      gs_appr-rej_remarks = lv_remark.
      gs_appr-remarks     = 'Rejected by L3'.
      MODIFY ycis_apprvl FROM gs_appr.
      COLLECT gs_appr-sales_off INTO lt_office.
      lv_cnt = lv_cnt + 1.
    ENDIF.
  ENDLOOP.

  IF lv_cnt > 0.
    COMMIT WORK.
    IF p_action = 'R'.
      LOOP AT lt_office INTO lv_off.
        PERFORM send_mail USING '2' lv_off lv_off 'CIS rebates rejected by L3 - please review (L2)'.
      ENDLOOP.
*     rejected rows leave L3 - remove them from the grid
      DELETE gt_out WHERE sel = 'X'.
    ENDIF.
  ENDIF.
* executed rows stay on the main grid with their Rebate Order number, so the
* CPC user sees the full rebate-order list right after execution (visibility).
  IF p_action = 'E'.
    IF lv_dup > 0 AND lv_cnt = 0 AND lv_err = 0.
*     only already-created lines were selected
      MESSAGE 'Rebate order already created for the selected line(s)' TYPE 'I'.
    ELSE.
      MESSAGE |{ lv_cnt } rebate order(s) created, { lv_dup } already created, { lv_err } failed - see 'Rebate Order' column| TYPE 'S'.
    ENDIF.
*   confirmation pop-up after execution (GAIL 27.07.2026)
    IF lv_cnt > 0.
      PERFORM show_stmt_popup.
    ENDIF.
  ELSE.
    MESSAGE |{ lv_cnt } line(s) rejected and returned to L2| TYPE 'S'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  show_stmt_popup   (confirmation statement pop-up)
*&---------------------------------------------------------------------*
FORM show_stmt_popup.
  DATA: lv_ans TYPE c.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'CIS 2026-27 - Verification & Confirmation'
      text_question         =
        'Customer-wise, grade-wise sales quantities, along with eligible PSD ' &&
        'rates and amounts, have been verified and confirmed after considering ' &&
        'customer waivers, shortfall waivers, sales return quantities, and ' &&
        'Group/MLE details.'
      text_button_1         = 'OK'
      icon_button_1         = 'ICON_OKAY'
      display_cancel_button = ' '
    IMPORTING
      answer                = lv_ans
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  create_order   (build & post the rebate order from payload)
*&---------------------------------------------------------------------*
FORM create_order USING p_appr TYPE ycis_apprvl
                  CHANGING p_vbeln TYPE vbeln.
*   Interface types mirror the production create_sale_order in
*   YRVG004_QAIS_EXECUTE : SOLD_TO_PARTY is a structure (BAPISOLDTO) and
*   RETURN is a single IMPORTING structure (BAPIRETURN1) - NOT a table.
*   Passing a CHAR field / a table here caused the CALL_FUNCTION_CONFLICT_LENG
*   (CX_SY_DYN_CALL_ILLEGAL_TYPE) runtime error.
  DATA: x_header    LIKE bapisdhead,
        i_items     LIKE bapiitemin  OCCURS 0 WITH HEADER LINE,
        i_partner   LIKE bapipartnr  OCCURS 0 WITH HEADER LINE,
        x_sold_to   LIKE bapisoldto,
        x_return    LIKE bapireturn1,
        w_objtype   LIKE bapiusw01-objtype,
        w_vbeln     LIKE bapivbeln-vbeln.

  x_header-doc_type   = p_appr-doc_type.
  x_header-sales_org  = p_appr-sales_org.
  x_header-distr_chan = p_appr-distr_chan.
  x_header-division   = p_appr-division.
  x_header-ord_reason = p_appr-ord_reason.
  x_header-sales_off  = p_appr-sales_off.
  x_header-cd_type1   = p_appr-cd_type.
  x_header-cd_value1  = p_appr-cd_value.
  x_header-purch_no   = p_appr-purch_no.
  IF p_appr-bill_block IS NOT INITIAL.
    x_header-bill_block = p_appr-bill_block.
  ENDIF.

  CLEAR i_partner. REFRESH i_partner.
  i_partner-partn_role = 'AG'.
  i_partner-partn_numb = p_appr-kunnr.
  APPEND i_partner.

  CLEAR i_items. REFRESH i_items.
  i_items-material   = p_appr-material.
  i_items-target_qty = p_appr-target_qty.
  APPEND i_items.

  w_objtype = 'BUS2094'.

  CALL FUNCTION 'BAPI_SALESDOCU_CREATEFROMDATA'
    EXPORTING
      order_header_in = x_header
      business_object = w_objtype
      without_commit  = ' '
    IMPORTING
      salesdocument   = w_vbeln
      sold_to_party   = x_sold_to
      return          = x_return
    TABLES
      order_items_in  = i_items
      order_partners  = i_partner.

  IF w_vbeln IS NOT INITIAL.
    p_vbeln = w_vbeln.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
*   Post-order updates that the original YRVG004 did after order creation:
*   stamp the CIS period on the order (VBKD), log the rebate (YRVA_REBATE)
*   and write the sale order back into the CIS master (YRVA_QAIS_DATA / _M).
    PERFORM post_order_update USING p_appr w_vbeln.
    PERFORM post_master_update USING p_appr w_vbeln.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  post_order_update
*&      Mirrors the YRVG004 post-processing: set the CIS period dates on
*&      the created order (VBKD-BSTDK / BSTDK_E) and write the rebate log
*&      table YRVA_REBATE. In the maker/checker flow this happens here, at
*&      the final (L3 / CPC) approval - only after the order is created.
*&---------------------------------------------------------------------*
FORM post_order_update USING p_appr TYPE ycis_apprvl
                             p_vbeln TYPE vbeln.
  DATA: ls_reb TYPE yrva_rebate.

* CIS period on the order document (reference dates)
  UPDATE vbkd SET bstdk   = p_appr-period_from
                  bstdk_e = p_appr-period_to
            WHERE vbeln = p_vbeln.
  COMMIT WORK AND WAIT.

* rebate log
  CLEAR ls_reb.
  ls_reb-vbeln        = p_vbeln.
  ls_reb-kunnr        = p_appr-kunnr.
  ls_reb-vkbur        = p_appr-sales_off.
  ls_reb-lft_qty      = p_appr-lft_qty.
  ls_reb-rev_qty      = p_appr-elig_qty.
  ls_reb-reb_cond     = p_appr-reb_cond.
  ls_reb-ord_cond     = 'ZCMU'.
  ls_reb-value        = p_appr-rebate_val.
  ls_reb-yy_per_start = p_appr-period_from.
  ls_reb-yy_per_end   = p_appr-period_to.
  SELECT SINGLE kukla FROM kna1 INTO ls_reb-kukla
    WHERE kunnr = p_appr-kunnr.
  MODIFY yrva_rebate FROM ls_reb.

* stamp the created order onto the grade-wise detail captured at L1
  UPDATE ycis_apprvl_grd SET order_no = p_vbeln
    WHERE qais_no     = p_appr-qais_no
      AND scheme_type = p_appr-scheme_type
      AND period_from = p_appr-period_from
      AND period_to   = p_appr-period_to
      AND kunnr       = p_appr-kunnr
      AND kvgr2       = p_appr-kvgr2.

  COMMIT WORK AND WAIT.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  post_master_update
*&      Write the created sale order back into the CIS master, mirroring
*&      the original YRVG004:
*&        Q -> YRVA_QAIS_DATA-SALE_ORDER_Qn      (n from the period)
*&        A -> YRVA_QAIS_DATA-SO_ANNUAL
*&        C -> YRVA_QAIS_DATA-SO_ANNUAL_CONSIT
*&        M -> YRVA_QAIS_DATA-IND_ELGL_QTY_Mn and YRVA_QAIS_DATA_M
*&             (MON_SO_Mn / MON_VALUE_Mn / IND_ELGL_QTY_Mn / MON_REMARKS_Mn)
*&      Read-modify-write on the full row keeps every other field intact.
*&      Quarter / month index are derived from the stored period, exactly
*&      as the original derived them from the selection-screen period.
*&---------------------------------------------------------------------*
FORM post_master_update USING p_appr TYPE ycis_apprvl
                              p_vbeln TYPE vbeln.
  DATA: ls_q    TYPE yrva_qais_data,
        ls_qm   TYPE yrva_qais_data_m,
        lv_mm   TYPE i,
        lv_idx  TYPE i,
        lv_fld  TYPE string,
        lv_ok   TYPE flag.
  FIELD-SYMBOLS: <any> TYPE any.

* locate the CIS master row (same business key the original used)
  CLEAR: ls_q, lv_ok.
  IF p_appr-qais_no IS NOT INITIAL.
    SELECT SINGLE * FROM yrva_qais_data INTO ls_q
      WHERE qais_no = p_appr-qais_no.
    IF sy-subrc = 0.
      lv_ok = 'X'.
    ENDIF.
  ENDIF.
  IF lv_ok IS INITIAL.
    SELECT SINGLE * FROM yrva_qais_data INTO ls_q
      WHERE kunnr = p_appr-kunnr
        AND kvgr2 = p_appr-kvgr2
        AND vkbur = p_appr-sales_off.
    IF sy-subrc = 0.
      lv_ok = 'X'.
    ENDIF.
  ENDIF.
  CHECK lv_ok = 'X'.

  CASE p_appr-scheme_type.
    WHEN 'Q'.
      CASE p_appr-period_from+4(2).
        WHEN '04'. lv_fld = 'SALE_ORDER_Q1'.
        WHEN '07'. lv_fld = 'SALE_ORDER_Q2'.
        WHEN '10'. lv_fld = 'SALE_ORDER_Q3'.
        WHEN '01'. lv_fld = 'SALE_ORDER_Q4'.
      ENDCASE.
      ASSIGN COMPONENT lv_fld OF STRUCTURE ls_q TO <any>.
      IF sy-subrc = 0.
        <any> = p_vbeln.
      ENDIF.
      MODIFY yrva_qais_data FROM ls_q.

    WHEN 'A'.
      ASSIGN COMPONENT 'SO_ANNUAL' OF STRUCTURE ls_q TO <any>.
      IF sy-subrc = 0.
        <any> = p_vbeln.
      ENDIF.
      MODIFY yrva_qais_data FROM ls_q.

    WHEN 'C'.
      ASSIGN COMPONENT 'SO_ANNUAL_CONSIT' OF STRUCTURE ls_q TO <any>.
      IF sy-subrc = 0.
        <any> = p_vbeln.
      ENDIF.
      MODIFY yrva_qais_data FROM ls_q.

    WHEN 'M'.
*     financial-year month index (Apr=1 ... Jun=3 ... Mar=12)
      lv_mm = p_appr-period_to+4(2).
      IF lv_mm >= 4.
        lv_idx = lv_mm - 3.
      ELSE.
        lv_idx = lv_mm + 9.
      ENDIF.
*     main table : eligible qty for the month
      lv_fld = |IND_ELGL_QTY_M{ lv_idx }|.
      ASSIGN COMPONENT lv_fld OF STRUCTURE ls_q TO <any>.
      IF sy-subrc = 0.
        <any> = p_appr-elig_qty.
      ENDIF.
      MODIFY yrva_qais_data FROM ls_q.
*     detail table _M : order / value / qty / remarks for the month
      CLEAR ls_qm.
      SELECT SINGLE * FROM yrva_qais_data_m INTO ls_qm
        WHERE kunnr = p_appr-kunnr
          AND kvgr2 = p_appr-kvgr2
          AND vkbur = p_appr-sales_off.
      IF sy-subrc <> 0.
        CLEAR ls_qm.
        ls_qm-mandt   = sy-mandt.
        ls_qm-qais_no = p_appr-qais_no.
        ls_qm-kunnr   = p_appr-kunnr.
        ls_qm-kvgr2   = p_appr-kvgr2.
        ls_qm-vkbur   = p_appr-sales_off.
      ENDIF.
      lv_fld = |MON_SO_M{ lv_idx }|.
      ASSIGN COMPONENT lv_fld OF STRUCTURE ls_qm TO <any>.
      IF sy-subrc = 0.
        <any> = p_vbeln.
      ENDIF.
      lv_fld = |MON_VALUE_M{ lv_idx }|.
      ASSIGN COMPONENT lv_fld OF STRUCTURE ls_qm TO <any>.
      IF sy-subrc = 0.
        <any> = p_appr-rebate_val.
      ENDIF.
      lv_fld = |IND_ELGL_QTY_M{ lv_idx }|.
      ASSIGN COMPONENT lv_fld OF STRUCTURE ls_qm TO <any>.
      IF sy-subrc = 0.
        <any> = p_appr-elig_qty.
      ENDIF.
      lv_fld = |MON_REMARKS_M{ lv_idx }|.
      ASSIGN COMPONENT lv_fld OF STRUCTURE ls_qm TO <any>.
      IF sy-subrc = 0.
        <any> = p_appr-purch_no.
      ENDIF.
      MODIFY yrva_qais_data_m FROM ls_qm.
  ENDCASE.
  COMMIT WORK AND WAIT.
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
*     L3 (CPC) reject -> back to L2 (PC MKTG-HOD)
      ls_text-line = |Dear Sir/Madam,|.                            APPEND ls_text TO lt_text.
      ls_text-line = ||.                                            APPEND ls_text TO lt_text.
      ls_text-line = |The CIS 2026-27 rebates for Sales Office { p_ctxoff } have been returned by L3 (CPC)|.
      APPEND ls_text TO lt_text.
      ls_text-line = |for your review. Please log in to T-Code YRVG004_A and re-check the records.|.
      APPEND ls_text TO lt_text.
      ls_text-line = ||.                                            APPEND ls_text TO lt_text.
      ls_text-line = |With warm regards,|.                          APPEND ls_text TO lt_text.
      ls_text-line = |GAIL (INDIA) LTD.|.                           APPEND ls_text TO lt_text.
      ls_text-line = ||.                                            APPEND ls_text TO lt_text.
      ls_text-line = |This is a system generated mail. Please do not reply.|.
      APPEND ls_text TO lt_text.
      lv_sub = 'CIS Scheme - Rebates returned by L3 for review'.
      lo_doc = cl_document_bcs=>create_document(
                 i_type = 'RAW' i_text = lt_text i_subject = lv_sub ).
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
  ENDTRY.
ENDFORM.
