*&---------------------------------------------------------------------*
*& Report  YCIS_APPROVE
*&---------------------------------------------------------------------*
*& CIS 2026-27 - Maker/Checker (R4) : CHECKER / APPROVER program.
*&
*& Flow:
*&   - The MAKER runs YRVG004_QAIS_EXECUTE_N1, reviews the ALV and submits
*&     the computed rebates. Instead of creating orders they are saved to
*&     table YCIS_APPRVL with status 'P' (Pending).
*&   - The CHECKER runs THIS program with the same selection parameters,
*&     sees only the Pending records, and either:
*&        APPROVE -> the rebate sales order (credit-memo request) is created
*&                   via BAPI and the row is marked 'A' with the order no.
*&        REJECT  -> the row is marked 'R' (no order created); the maker can
*&                   re-submit later.
*&
*& The checker narrows the pending set with the selection screen (period,
*& sales office, customer, group) and chooses the action. PREVIEW just lists
*& the pending records without acting.
*&---------------------------------------------------------------------*
REPORT  ycis_approve.

TYPE-POOLS: slis.

TABLES: ycis_apprvl.

*--------------------------------------------------------------------*
* Output structure (display of pending / processed records)
*--------------------------------------------------------------------*
TYPES: BEGIN OF ty_out,
         qais_no     TYPE ycis_apprvl-qais_no,
         scheme_type TYPE ycis_apprvl-scheme_type,
         kunnr       TYPE ycis_apprvl-kunnr,
         cust_name   TYPE ycis_apprvl-cust_name,
         kvgr2       TYPE ycis_apprvl-kvgr2,
         sales_off   TYPE ycis_apprvl-sales_off,
         elig_qty    TYPE ycis_apprvl-elig_qty,
         rebate_val  TYPE ycis_apprvl-rebate_val,
         purch_no    TYPE ycis_apprvl-purch_no,
         status      TYPE ycis_apprvl-status,
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
      gv_cnt_ok TYPE i,
      gv_cnt_er TYPE i.

*--------------------------------------------------------------------*
* Selection screen - same key parameters as the maker
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_sptag FOR ycis_apprvl-period_from OBLIGATORY,   " scheme period (from date)
                s_vkbur FOR ycis_apprvl-sales_off,                " sales office
                s_kunnr FOR ycis_apprvl-kunnr,                    " customer
                s_kvgr2 FOR ycis_apprvl-kvgr2.                    " customer group
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_prev RADIOBUTTON GROUP act DEFAULT 'X',            " preview only
            p_appr RADIOBUTTON GROUP act,                        " approve -> create order
            p_rej  RADIOBUTTON GROUP act.                        " reject
SELECTION-SCREEN END OF BLOCK b2.

*--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_pending.
  IF gt_appr IS INITIAL.
    MESSAGE 'No pending records for the given selection' TYPE 'I'.
    RETURN.
  ENDIF.

  IF p_appr = 'X'.
    PERFORM do_approve.
    MESSAGE |Approved: { gv_cnt_ok }  |
         && |Failed: { gv_cnt_er }| TYPE 'I'.
  ELSEIF p_rej = 'X'.
    PERFORM do_reject.
    MESSAGE |Rejected: { gv_cnt_ok } record(s)| TYPE 'I'.
  ENDIF.

  PERFORM build_out.
  PERFORM build_fieldcat.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
*&      Form  get_pending
*&---------------------------------------------------------------------*
FORM get_pending.
  SELECT * FROM ycis_apprvl INTO TABLE gt_appr
    WHERE period_from IN s_sptag
      AND sales_off   IN s_vkbur
      AND kunnr       IN s_kunnr
      AND kvgr2       IN s_kvgr2
      AND status      = 'P'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  do_approve   (create the rebate order for each pending row)
*&---------------------------------------------------------------------*
FORM do_approve.
  DATA: x_header   TYPE bapisdhead,
        i_items    TYPE STANDARD TABLE OF bapiitemin,
        wa_item    TYPE bapiitemin,
        i_partner  TYPE STANDARD TABLE OF bapipartnr,
        wa_partner TYPE bapipartnr,
        i_return   TYPE STANDARD TABLE OF bapireturn,
        wa_return  TYPE bapireturn,
        lv_vbeln   TYPE vbeln,
        lv_sold    TYPE bapikna1-customer.

  LOOP AT gt_appr INTO gs_appr.
    CLEAR: x_header, i_items, i_partner, i_return, lv_vbeln.
    REFRESH: i_items, i_partner, i_return.

    x_header-doc_type   = gs_appr-doc_type.
    x_header-sales_org  = gs_appr-sales_org.
    x_header-distr_chan = gs_appr-distr_chan.
    x_header-division   = gs_appr-division.
    x_header-ord_reason = gs_appr-ord_reason.
    x_header-sales_off  = gs_appr-sales_off.
    x_header-cd_type1   = gs_appr-cd_type.
    x_header-cd_value1  = gs_appr-cd_value.
    x_header-purch_no   = gs_appr-purch_no.
    IF gs_appr-bill_block IS NOT INITIAL.
      x_header-bill_block = gs_appr-bill_block.
    ENDIF.

    wa_partner-partn_role = 'AG'.
    wa_partner-partn_numb = gs_appr-kunnr.
    APPEND wa_partner TO i_partner.

    wa_item-material   = gs_appr-material.
    wa_item-target_qty = gs_appr-target_qty.
    APPEND wa_item TO i_items.

    CALL FUNCTION 'BAPI_SALESDOCU_CREATEFROMDATA'
      EXPORTING
        order_header_in = x_header
        business_object = 'BUS2094'
        without_commit  = ' '
      IMPORTING
        salesdocument   = lv_vbeln
        sold_to_party   = lv_sold
      TABLES
        order_items_in  = i_items
        order_partners  = i_partner
        return          = i_return.

    IF lv_vbeln IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      gs_appr-status       = 'A'.
      gs_appr-order_no     = lv_vbeln.
      gs_appr-checker      = sy-uname.
      gs_appr-checker_date = sy-datum.
      gs_appr-checker_time = sy-uzeit.
      gs_appr-remarks      = 'Approved - order created'.
      MODIFY ycis_apprvl FROM gs_appr.
      MODIFY gt_appr FROM gs_appr.
      gv_cnt_ok = gv_cnt_ok + 1.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      READ TABLE i_return INTO wa_return
           WITH KEY type = 'E'.
      IF sy-subrc <> 0.
        READ TABLE i_return INTO wa_return INDEX 1.
      ENDIF.
      gs_appr-remarks = wa_return-message.
      MODIFY gt_appr FROM gs_appr.
      gv_cnt_er = gv_cnt_er + 1.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  do_reject
*&---------------------------------------------------------------------*
FORM do_reject.
  LOOP AT gt_appr INTO gs_appr.
    gs_appr-status       = 'R'.
    gs_appr-checker      = sy-uname.
    gs_appr-checker_date = sy-datum.
    gs_appr-checker_time = sy-uzeit.
    gs_appr-remarks      = 'Rejected by checker'.
    MODIFY ycis_apprvl FROM gs_appr.
    MODIFY gt_appr FROM gs_appr.
    gv_cnt_ok = gv_cnt_ok + 1.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  build_out
*&---------------------------------------------------------------------*
FORM build_out.
  REFRESH gt_out.
  LOOP AT gt_appr INTO gs_appr.
    CLEAR gs_out.
    gs_out-qais_no     = gs_appr-qais_no.
    gs_out-scheme_type = gs_appr-scheme_type.
    gs_out-kunnr       = gs_appr-kunnr.
    gs_out-cust_name   = gs_appr-cust_name.
    gs_out-kvgr2       = gs_appr-kvgr2.
    gs_out-sales_off   = gs_appr-sales_off.
    gs_out-elig_qty    = gs_appr-elig_qty.
    gs_out-rebate_val  = gs_appr-rebate_val.
    gs_out-purch_no    = gs_appr-purch_no.
    gs_out-status      = gs_appr-status.
    gs_out-order_no    = gs_appr-order_no.
    gs_out-remarks     = gs_appr-remarks.
    APPEND gs_out TO gt_out.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  build_fieldcat
*&---------------------------------------------------------------------*
FORM build_fieldcat.
  DEFINE add_fc.
    CLEAR gs_fcat.
    gs_fcat-fieldname = &1.
    gs_fcat-seltext_l = &2.
    gs_fcat-seltext_m = &2.
    gs_fcat-seltext_s = &2.
    APPEND gs_fcat TO gt_fcat.
  END-OF-DEFINITION.

  add_fc 'QAIS_NO'     'CIS No.'.
  add_fc 'SCHEME_TYPE' 'Type'.
  add_fc 'KUNNR'       'Customer'.
  add_fc 'CUST_NAME'   'Customer Name'.
  add_fc 'KVGR2'       'Cust Group'.
  add_fc 'SALES_OFF'   'Sales Office'.
  add_fc 'ELIG_QTY'    'Eligible Qty'.
  add_fc 'REBATE_VAL'  'Rebate Value'.
  add_fc 'PURCH_NO'    'Reference No'.
  add_fc 'STATUS'      'Status'.
  add_fc 'ORDER_NO'    'Order No.'.
  add_fc 'REMARKS'     'Remarks'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
FORM display_alv.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra             = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fcat
    TABLES
      t_outtab           = gt_out
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
