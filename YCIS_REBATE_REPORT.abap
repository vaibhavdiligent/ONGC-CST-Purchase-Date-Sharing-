*&---------------------------------------------------------------------*
*& Report  YCIS_REBATE_REPORT
*&---------------------------------------------------------------------*
*& CIS 2026-27 - Requirement 5
*& Report to capture rebate order details created from the CIS program:
*&   Customer | Grade | Material | Discount Qty | Rebate amount | Reference
*&
*& GAIL observation 09.07.2026 (incorporated):
*&   1. Grade - capture the grade (KONDM) on which the discount is given.
*&   2. Qty   - capture the quantity on which the discount is given.
*&   3. Reference no - to identify the discount head. The CIS program stores
*&      it in the rebate order's Purchase-Order-Number (VBKD-BSTKD, from the
*&      "remarks"); the discount condition type is ZCMU.
*&
*& NOTE (to confirm with GAIL): the rebate order is created as a
*& credit-memo request from YRVG004 (PERFORM create_sale_order). The exact
*& document type is read from parameter p_auart below - adjust the default
*& to the actual CIS credit-memo-request document type before go-live.
*&---------------------------------------------------------------------*
REPORT  ycis_rebate_report.

TYPE-POOLS: slis.

TABLES: vbak, vbap.

*--------------------------------------------------------------------*
* Output structure
*--------------------------------------------------------------------*
TYPES: BEGIN OF ty_out,
         vbeln      TYPE vbeln_va,     " rebate order
         auart      TYPE auart,        " doc type
         erdat      TYPE erdat,        " created on
         bstkd      TYPE bstkd,        " reference no (identifies the discount head)
         kunnr      TYPE kunnr,        " customer
         name1      TYPE name1_gp,     " customer name
         kondm      TYPE kondm,        " grade (material pricing group)
         kondm_txt  TYPE t178t-vtext,  " grade name
         matnr      TYPE matnr,        " material
         arktx      TYPE arktx,        " material description
         kwmeng     TYPE kwmeng,       " discount quantity
         vrkme      TYPE vrkme,        " unit
         netwr      TYPE netwr,        " rebate amount (net value)
         waerk      TYPE waerk,        " currency
       END OF ty_out.

DATA: gt_out   TYPE STANDARD TABLE OF ty_out,
      gs_out   TYPE ty_out,
      gt_fcat  TYPE slis_t_fieldcat_alv,
      gs_fcat  TYPE slis_fieldcat_alv,
      gs_layout TYPE slis_layout_alv.

*--------------------------------------------------------------------*
* Selection screen
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_erdat FOR vbak-erdat,          " created-on / scheme period
                s_vkbur FOR vbak-vkbur,          " sales office
                s_kunnr FOR vbak-kunnr,          " customer
                s_vbeln FOR vbak-vbeln.          " rebate order
PARAMETERS:     p_auart TYPE auart DEFAULT 'ZCR'."credit-memo req. doc type
SELECTION-SCREEN END OF BLOCK b1.

*--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_data.
  PERFORM build_fieldcat.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
FORM get_data.
  DATA: lt_vbak TYPE STANDARD TABLE OF vbak,
        ls_vbak TYPE vbak,
        lt_vbap TYPE STANDARD TABLE OF vbap,
        ls_vbap TYPE vbap.

  SELECT * FROM vbak INTO TABLE lt_vbak
    WHERE vbeln IN s_vbeln
      AND erdat IN s_erdat
      AND vkbur IN s_vkbur
      AND kunnr IN s_kunnr
      AND auart  = p_auart.

  IF lt_vbak IS INITIAL.
    MESSAGE 'No rebate orders found for the selection' TYPE 'I'.
    RETURN.
  ENDIF.

  SELECT * FROM vbap INTO TABLE lt_vbap
    FOR ALL ENTRIES IN lt_vbak
    WHERE vbeln = lt_vbak-vbeln.

  LOOP AT lt_vbap INTO ls_vbap.
    READ TABLE lt_vbak INTO ls_vbak WITH KEY vbeln = ls_vbap-vbeln.
    CHECK sy-subrc = 0.
    CLEAR gs_out.
    gs_out-vbeln  = ls_vbap-vbeln.
    gs_out-auart  = ls_vbak-auart.
    gs_out-erdat  = ls_vbak-erdat.
    gs_out-kunnr  = ls_vbak-kunnr.
    gs_out-matnr  = ls_vbap-matnr.
    gs_out-arktx  = ls_vbap-arktx.
    gs_out-kondm  = ls_vbap-kondm.          " grade on which discount given
    gs_out-kwmeng = ls_vbap-kwmeng.         " discount quantity
    gs_out-vrkme  = ls_vbap-vrkme.
    gs_out-netwr  = ls_vbap-netwr.
    gs_out-waerk  = ls_vbak-waerk.
    SELECT SINGLE name1 FROM kna1 INTO gs_out-name1
      WHERE kunnr = gs_out-kunnr.
*   grade name (material pricing group text)
    IF ls_vbap-kondm IS NOT INITIAL.
      SELECT SINGLE vtext FROM t178t INTO gs_out-kondm_txt
        WHERE spras = sy-langu AND kondm = ls_vbap-kondm.
    ENDIF.
*   reference no that identifies the discount head - stored by YRVG004 in the
*   rebate order's PO number (VBKD-BSTKD, header business data posnr 000000)
    SELECT SINGLE bstkd FROM vbkd INTO gs_out-bstkd
      WHERE vbeln = ls_vbap-vbeln AND posnr = '000000'.
    APPEND gs_out TO gt_out.
  ENDLOOP.
ENDFORM.

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

  add_fc 'VBELN'    'Rebate Order'.
  add_fc 'AUART'    'Doc Type'.
  add_fc 'ERDAT'    'Created On'.
  add_fc 'BSTKD'    'Reference No'.
  add_fc 'KUNNR'    'Customer'.
  add_fc 'NAME1'    'Customer Name'.
  add_fc 'KONDM'    'Grade'.
  add_fc 'KONDM_TXT' 'Grade Name'.
  add_fc 'MATNR'    'Material'.
  add_fc 'ARKTX'    'Description'.
  add_fc 'KWMENG'   'Discount Qty'.
  add_fc 'VRKME'    'Unit'.
  add_fc 'NETWR'    'Rebate Amount'.
  add_fc 'WAERK'    'Currency'.
ENDFORM.

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
