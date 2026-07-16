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

TYPES: BEGIN OF ty_out,
         sel         TYPE flag,
         qais_no     TYPE ycis_apprvl-qais_no,
         scheme_type TYPE ycis_apprvl-scheme_type,
         kunnr       TYPE ycis_apprvl-kunnr,
         cust_name   TYPE ycis_apprvl-cust_name,
         kvgr2       TYPE ycis_apprvl-kvgr2,
         sales_off   TYPE ycis_apprvl-sales_off,
         elig_qty    TYPE ycis_apprvl-elig_qty,
         rebate_val  TYPE ycis_apprvl-rebate_val,
         purch_no    TYPE ycis_apprvl-purch_no,
         l2_user     TYPE ycis_apprvl-l2_user,
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
  add_fc 'QAIS_NO'     'CIS No.'         ''.
  add_fc 'SCHEME_TYPE' 'Type'            ''.
  add_fc 'KUNNR'       'Customer'        ''.
  add_fc 'CUST_NAME'   'Customer Name'   ''.
  add_fc 'KVGR2'       'Cust Group'      ''.
  add_fc 'SALES_OFF'   'Sales Office'    ''.
  add_fc 'ELIG_QTY'    'Eligible Qty'    ''.
  add_fc 'REBATE_VAL'  'Rebate Value'    ''.
  add_fc 'PURCH_NO'    'Reference No'    ''.
  add_fc 'L2_USER'     'L2 Approved By'  ''.
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
        lv_remark TYPE ycis_apprvl-rej_remarks,
        lt_office TYPE STANDARD TABLE OF vkbur,
        lv_off    TYPE vkbur,
        lv_vbeln  TYPE vbeln.

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
      CLEAR lv_vbeln.
      PERFORM create_order USING gs_appr CHANGING lv_vbeln.
      IF lv_vbeln IS NOT INITIAL.
        gs_appr-wf_status = '40'.
        gs_appr-order_no  = lv_vbeln.
        gs_appr-l3_user   = sy-uname.
        gs_appr-l3_date   = sy-datum.
        gs_appr-l3_time   = sy-uzeit.
        gs_appr-remarks   = 'Executed - order created'.
        MODIFY ycis_apprvl FROM gs_appr.
        lv_cnt = lv_cnt + 1.
      ELSE.
        lv_err = lv_err + 1.
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
    ENDIF.
    DELETE gt_out WHERE sel = 'X'.
  ENDIF.
  MESSAGE |Processed: { lv_cnt }  Failed: { lv_err }| TYPE 'I'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  create_order   (build & post the rebate order from payload)
*&---------------------------------------------------------------------*
FORM create_order USING p_appr TYPE ycis_apprvl
                  CHANGING p_vbeln TYPE vbeln.
  DATA: x_header   TYPE bapisdhead,
        i_items    TYPE STANDARD TABLE OF bapiitemin,
        wa_item    TYPE bapiitemin,
        i_partner  TYPE STANDARD TABLE OF bapipartnr,
        wa_partner TYPE bapipartnr,
        i_return   TYPE STANDARD TABLE OF bapireturn,
        lv_sold    TYPE kunnr.

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

  wa_partner-partn_role = 'AG'.
  wa_partner-partn_numb = p_appr-kunnr.
  APPEND wa_partner TO i_partner.

  wa_item-material   = p_appr-material.
  wa_item-target_qty = p_appr-target_qty.
  APPEND wa_item TO i_items.

  CALL FUNCTION 'BAPI_SALESDOCU_CREATEFROMDATA'
    EXPORTING
      order_header_in = x_header
      business_object = 'BUS2094'
      without_commit  = ' '
    IMPORTING
      salesdocument   = p_vbeln
      sold_to_party   = lv_sold
    TABLES
      order_items_in  = i_items
      order_partners  = i_partner
      return          = i_return.

  IF p_vbeln IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
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
      ls_text-line = |CIS 2026-27 : { p_subject }|.   APPEND ls_text TO lt_text.
      ls_text-line = |Sales Office : { p_ctxoff }|.    APPEND ls_text TO lt_text.
      ls_text-line = |Please open the relevant transaction to action the pending records.|.
      APPEND ls_text TO lt_text.
      lv_sub = p_subject.
      lo_doc = cl_document_bcs=>create_document(
                 i_type = 'RAW' i_text = lt_text i_subject = lv_sub ).
      lo_send->set_document( lo_doc ).
      LOOP AT lt_wf INTO ls_wf.
        CHECK ls_wf-email IS NOT INITIAL.
        lv_addr = ls_wf-email.
        lo_rec  = cl_cam_address_bcs=>create_internet_address( lv_addr ).
        lo_send->add_recipient( i_recipient = lo_rec ).
      ENDLOOP.
      lo_send->send( ).
      COMMIT WORK.
    CATCH cx_bcs.
  ENDTRY.
ENDFORM.
