*&---------------------------------------------------------------------*
*& Report  YCIS_REB_EXECUTE
*&---------------------------------------------------------------------*
*& Rebate (PSD / YRVU001) - 3-level approval workflow : LEVEL 3 (CPC).
*&
*&   L1 : YRVU001_REB_CALC_M2_N1  -> stages rows to YCIS_APPRVL
*&        (SCHEME_TYPE 'U', WF_STATUS '20').
*&   L2 : YCIS_REB_APPROVE        -> APPROVE -> WF_STATUS '30' (Pending L3).
*&   L3 : THIS program (CPC, central). Lists the Pending-L3 rebate rows of
*&        ALL sales offices and, on EXECUTE, replays the stored payload into
*&        BAPI_SALESDOCU_CREATEFROMDATA (BUS2094 credit-memo request) exactly
*&        as the original YRVU001 FORM check / FORM sales_order did, then
*&        writes YRVA_REBATE and stamps WF_STATUS '40' (Completed) / STATUS
*&        'A'. REJECT -> WF_STATUS '20' (back to L2), mail L2.
*&
*& Reuses the shared approval infrastructure (YCIS_APPRVL / YCIS_WF_APPR),
*& separated from the QAIS queue by SCHEME_TYPE = 'U'.
*& GUI status 'STANDARD' (EXEC, REJ, SELALL, DESEL, BACK/EXIT) - SE41.
*&---------------------------------------------------------------------*
REPORT  ycis_reb_execute.

TYPE-POOLS: slis.

TABLES: ycis_apprvl, ycis_wf_appr, yrva_rebate.

CONSTANTS: gc_level  TYPE ycis_wlevel VALUE '3',
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
*        L1 & L2 verification captured for L3 onward processing
         l1_user     TYPE ycis_apprvl-l1_user,
         l1_date     TYPE ycis_apprvl-l1_date,
         l2_user     TYPE ycis_apprvl-l2_user,
         l2_date     TYPE ycis_apprvl-l2_date,
         order_no    TYPE ycis_apprvl-order_no,
         remarks     TYPE ycis_apprvl-remarks,
       END OF ty_out.

DATA: gt_appr   TYPE STANDARD TABLE OF ycis_apprvl,
      gs_appr   TYPE ycis_apprvl,
      gt_out    TYPE STANDARD TABLE OF ty_out,
      gs_out    TYPE ty_out,
      gt_fcat   TYPE slis_t_fieldcat_alv,
      gs_fcat   TYPE slis_fieldcat_alv,
      gs_layout TYPE slis_layout_alv,
      gv_isl3   TYPE flag.

*   Confirmation statement shown as the L3 report header (verified at L1/L2).
CONSTANTS:
  gc_stmt1 TYPE string VALUE
    'Customer-wise upliftment quantities, along with eligible rebate',
  gc_stmt2 TYPE string VALUE
    'rates and amounts, have been verified and confirmed after',
  gc_stmt3 TYPE string VALUE
    'considering sales return / reversal quantities and Group details.'.

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
    MESSAGE 'No rebate records pending L3 execution' TYPE 'I'.
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
*   CPC is central -> all offices' pending-L3 rebate rows (narrowed by s_vkbur)
  SELECT * FROM ycis_apprvl INTO TABLE gt_appr
    WHERE wf_status   = '30'
      AND scheme_type = gc_scheme              " rebate queue only
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
  add_fc 'L1_USER'     'L1 Verified By'  ''.
  add_fc 'L1_DATE'     'L1 Verified On'  ''.
  add_fc 'L2_USER'     'L2 Verified By'  ''.
  add_fc 'L2_DATE'     'L2 Verified On'  ''.
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
FORM top_of_page.                                           "#EC CALLED
  DATA: lt_hdr TYPE slis_t_listheader,
        ls_hdr TYPE slis_listheader.
  CLEAR ls_hdr. ls_hdr-typ = 'H'.
  ls_hdr-info = 'Rebate (PSD) - Level-3 Execution (verified & confirmed at L1 and L2)'.
  APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'. ls_hdr-info = gc_stmt1. APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'. ls_hdr-info = gc_stmt2. APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'. ls_hdr-info = gc_stmt3. APPEND ls_hdr TO lt_hdr.
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
*&      Form  process_selected   (E = execute/create CMR, R = reject)
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
         WITH KEY kunnr     = gs_out-kunnr
                  kvgr2     = gs_out-kvgr2
                  sales_off = gs_out-sales_off.
    CHECK sy-subrc = 0.

    IF p_action = 'E'.
*     Duplicate guard: re-read the current DB row (the in-memory copy may be
*     stale after an earlier Execute in the same session). If it already
*     carries an order / is Completed, skip - do NOT create a second CMR.
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
      PERFORM create_reb_order USING gs_appr CHANGING lv_vbeln.
      IF lv_vbeln IS NOT INITIAL.
        gs_appr-wf_status = '40'.        " Completed
        gs_appr-status    = 'A'.         " Approved - order created
        gs_appr-order_no  = lv_vbeln.
        gs_appr-l3_user   = sy-uname.
        gs_appr-l3_date   = sy-datum.
        gs_appr-l3_time   = sy-uzeit.
        gs_appr-remarks   = 'Executed - rebate order created'.
        MODIFY ycis_apprvl FROM gs_appr.
        lv_cnt = lv_cnt + 1.
        gs_out-order_no = lv_vbeln.
        gs_out-remarks  = 'Executed - rebate order created'.
        CLEAR gs_out-sel.
        MODIFY gt_out FROM gs_out.
      ELSE.
        lv_err = lv_err + 1.
        gs_out-remarks = 'Order creation failed'.
        CLEAR gs_out-sel.
        MODIFY gt_out FROM gs_out.
      ENDIF.
    ELSE.
      gs_appr-wf_status   = '20'.        " back to L2
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
        PERFORM send_mail USING '2' lv_off lv_off.
      ENDLOOP.
      DELETE gt_out WHERE sel = 'X'.
    ENDIF.
  ENDIF.

  IF p_action = 'E'.
    IF lv_dup > 0 AND lv_cnt = 0 AND lv_err = 0.
      MESSAGE 'Rebate order already created for the selected line(s)' TYPE 'I'.
    ELSE.
      MESSAGE |{ lv_cnt } rebate order(s) created, { lv_dup } already created, { lv_err } failed - see 'Rebate Order' column| TYPE 'S'.
    ENDIF.
    IF lv_cnt > 0.
      PERFORM show_stmt_popup.
    ENDIF.
  ELSE.
    MESSAGE |{ lv_cnt } line(s) rejected and returned to L2| TYPE 'S'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
FORM show_stmt_popup.
  DATA: lv_ans TYPE c.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Rebate (PSD) - Verification & Confirmation'
      text_question         =
        'Customer-wise upliftment quantities, along with eligible rebate ' &&
        'rates and amounts, have been verified and confirmed after ' &&
        'considering sales return / reversal quantities and Group details.'
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
*&      Form  map_cd_type   (rebate condition p_kschl -> order cd_type1)
*&      Mirrors the mapping in the original YRVU001 FORM check.
*&---------------------------------------------------------------------*
FORM map_cd_type USING p_kschl TYPE any
              CHANGING p_cdtype TYPE any.
  CLEAR p_cdtype.
  CASE p_kschl.
*   quarterly discount family -> ZCQT
    WHEN 'ZMQD' OR 'ZLQD' OR 'ZHHD' OR 'ZTQD' OR 'ZTLD' OR 'ZDCD'.
      p_cdtype = 'ZCQT'.
*   annual discount family -> ZAQT
    WHEN 'ZAQD' OR 'ZAD1' OR 'ZAD2' OR 'ZAD3' OR 'ZAD4' OR 'ZAD5'.
      p_cdtype = 'ZAQT'.
*   special-quantity discount family -> ZSQT
    WHEN 'ZSQD' OR 'ZBQD' OR 'ZPQD' OR 'ZIQD' OR 'ZEQD'
      OR 'ZRQD' OR 'ZMQR' OR 'ZPP9' OR 'ZCQD' OR 'ZQTY'.
      p_cdtype = 'ZSQT'.
    WHEN OTHERS.
*     polymer PE/ZP discount family (ZPRT/ZPPT/ZPP1..ZPP8/ZP10..ZP35/PE12..PE30)
*     -> ZAQT. Handled here to keep the CASE compact.
      IF p_kschl CP 'ZP*' OR p_kschl CP 'PE*'
      OR p_kschl = 'ZPRT' OR p_kschl = 'ZPPT'.
        p_cdtype = 'ZAQT'.
      ENDIF.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  create_reb_order   (build & post the credit-memo request)
*&      Replays the stored payload into BAPI_SALESDOCU_CREATEFROMDATA,
*&      mirroring the original YRVU001 FORM check / FORM sales_order.
*&---------------------------------------------------------------------*
FORM create_reb_order USING p_appr TYPE ycis_apprvl
                   CHANGING p_vbeln TYPE vbeln.
  DATA: x_header  LIKE bapisdhead,
        i_items   LIKE bapiitemin OCCURS 0 WITH HEADER LINE,
        i_partner LIKE bapipartnr OCCURS 0 WITH HEADER LINE,
        x_sold_to LIKE bapisoldto,
        x_return  LIKE bapireturn1,
        w_objtype LIKE bapiusw01-objtype,
        w_vbeln   LIKE bapivbeln-vbeln,
        lv_cdtype TYPE bapisdhead-cd_type1,
        lv_kunnr  TYPE kunnr.

  PERFORM map_cd_type USING p_appr-reb_cond CHANGING lv_cdtype.
  IF lv_cdtype IS INITIAL OR p_appr-rebate_val IS INITIAL
     OR p_appr-target_qty IS INITIAL.
*   nothing valid to post (unmapped condition / zero value) - leave unexecuted
    RETURN.
  ENDIF.

* HEADER (see original FORM check, lines ~1424-1537)
  x_header-doc_type   = p_appr-doc_type.        " 'ZP09'
  x_header-sales_org  = p_appr-sales_org.
  IF p_appr-werks = '5010'.
    x_header-distr_chan = '10'.
  ELSE.
    x_header-distr_chan = '20'.
  ENDIF.
  x_header-division   = '20'.
  x_header-ord_reason = p_appr-ord_reason.       " 'G21'
  x_header-sales_off  = p_appr-sales_off.
  x_header-cd_type1   = lv_cdtype.
  x_header-cd_value1  = p_appr-rebate_val / 10.  " original: value / 10
  x_header-purch_no   = p_appr-purch_no.
* consignment stockist reference (original x_order_header_in-ref_1 = lv_kunnr)
  IF p_appr-werks <> '5010' AND p_appr-werks IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_appr-werks
      IMPORTING
        output = lv_kunnr.
    x_header-ref_1 = lv_kunnr.
  ELSE.
    x_header-ref_1 = p_appr-ref_cust.
  ENDIF.

* PARTNER (AG = sold-to; for plants other than 5010 the plant-customer)
  CLEAR i_partner. REFRESH i_partner.
  i_partner-partn_role = 'AG'.
  IF p_appr-werks = '5010' OR p_appr-werks IS INITIAL.
    i_partner-partn_numb = p_appr-kunnr.
  ELSE.
    i_partner-partn_numb = lv_kunnr.
  ENDIF.
  APPEND i_partner.

* ITEM
  CLEAR i_items. REFRESH i_items.
  i_items-material   = p_appr-material.          " 'REBATE(POLYMER)'
  i_items-target_qty = p_appr-target_qty.        " ummenge1 * 1000
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
    PERFORM post_rebate_log USING p_appr w_vbeln lv_cdtype.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  post_rebate_log   (write YRVA_REBATE - as original FORM check)
*&---------------------------------------------------------------------*
FORM post_rebate_log USING p_appr   TYPE ycis_apprvl
                           p_vbeln  TYPE vbeln
                           p_cdtype TYPE any.
  CLEAR yrva_rebate.
  yrva_rebate-vbeln        = p_vbeln.
  yrva_rebate-kunnr        = p_appr-kunnr.
  yrva_rebate-vkbur        = p_appr-sales_off.
  yrva_rebate-lft_qty      = p_appr-lft_qty.
  yrva_rebate-werks        = p_appr-werks.
  yrva_rebate-rev_qty      = p_appr-rev_qty.
  yrva_rebate-reb_cond     = p_appr-reb_cond.
  yrva_rebate-ord_cond     = p_cdtype.
  yrva_rebate-value        = p_appr-rebate_val.
  yrva_rebate-yy_per_start = p_appr-period_from.
  yrva_rebate-yy_per_end   = p_appr-period_to.
  yrva_rebate-kukla        = p_appr-kukla.
  INSERT yrva_rebate FROM yrva_rebate.
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
      ls_text-line = |Dear Sir/Madam,|.                              APPEND ls_text TO lt_text.
      ls_text-line = ||.                                             APPEND ls_text TO lt_text.
      ls_text-line = |The rebate (PSD) lines for Sales Office { p_ctxoff } have been returned by L3 (CPC)|.
      APPEND ls_text TO lt_text.
      ls_text-line = |for your review. Please log in to T-Code YRVU015_E (L2) and re-check the records.|.
      APPEND ls_text TO lt_text.
      ls_text-line = ||.                                             APPEND ls_text TO lt_text.
      ls_text-line = |This is a system generated mail. Please do not reply.|.
      APPEND ls_text TO lt_text.
      lv_sub = 'Rebate (PSD) - returned by L3 for review'.
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
