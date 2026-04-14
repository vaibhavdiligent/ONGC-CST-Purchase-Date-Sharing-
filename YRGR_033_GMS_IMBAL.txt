*&---------------------------------------------------------------------*
*& Report YRGR_033_GMS_IMBAL
*&---------------------------------------------------------------------*
*
* Name of Program : YRGR_033_GMS_IMBAL
* T-Code          :
* Developed By    : Ravinder Singh
* Functional      : Pratibha Dangwal
* Date            : 08.05.2024
* DESCRIPTION     : CT/FN end invoicing - Report For Closing Imbalance
*                   Of Expired Contracts
* CHARM ID        : 2000000826
* TR NO           : DVRK9A19P3
*
*&---------------------------------------------------------------------*
REPORT yrgr_033_gms_imbal.

** Data Declarations **
DATA: dg_parent_grid TYPE REF TO cl_gui_container,
      dg_dyndoc_id   TYPE REF TO cl_dd_document,
      dg_splitter    TYPE REF TO cl_gui_splitter_container.

**====================================================================**
** SECTION: TOP INCLUDE - YRGR_033_GMS_IMBAL_TOP                     **
** (Data declarations, types, selection screen)                      **
**====================================================================**

TABLES : oijnomi, veda.

TYPES: BEGIN OF t_final,
  docnr    TYPE oijnomi-docnr,    "Nomination Reference Document
  matnr    TYPE oijnomi-matnr_i,  "Demand material
  locid    TYPE oijnomi-locid,    "Location ID
  partnr   TYPE oijnomi-partnr,   "Location Partner
  vbegdat  TYPE veda-vbegdat,     "Contract start date
  venddat  TYPE veda-venddat,     "CT End Date
  stat     TYPE char15,
  po_imbal TYPE yy_oij_cumimb,
  ne_imbal TYPE yy_oij_cumimb,
  ret_del  TYPE vbeln,            "Return Delivery
  crd_not  TYPE vbeln,            "Credit Note
  sal_ord  TYPE vbeln,            "Sales Order
  do       TYPE vbeln,            "DO
  pgi      TYPE vbeln,            "PGI
  invoic   TYPE vbeln,            "Invoice
  pur_ret  TYPE ebeln,            "Purchase Return
  po       TYPE ebeln,            "PO
  gr       TYPE ebeln,            "GR
END OF t_final.

DATA: lt_final TYPE TABLE OF yrx_imb_settle_qty,
      ls_final TYPE yrx_imb_settle_qty.

DATA: dg_html_cntrl  TYPE REF TO cl_gui_html_viewer,
      dg_parent_html TYPE REF TO cl_gui_container.

DATA: lt_exclude TYPE ui_functions,
      ls_exclude TYPE ui_func.

DATA: gs_fieldcat        TYPE lvc_s_fcat,
      gs_layout          TYPE lvc_s_layo,
      gt_fieldcat        TYPE lvc_t_fcat,
      grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      g_container        TYPE scrfname VALUE 'CUST'.

DATA: lv_begda   TYPE datum,
      lv_endda   TYPE datum,
      lv_fromdat TYPE datum,
      lv_todat   TYPE datum,
      lv_dat     TYPE char2,
      lv_dat1    TYPE datum,
      lv_dat2    TYPE datum.

DATA last_day_of_month LIKE sy-datum.
DATA: st_date TYPE sy-datum,
      ed_date TYPE sy-datum,
      lv_date TYPE sy-datum.

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-001.
  PARAMETERS: r1 RADIOBUTTON GROUP r1
                 USER-COMMAND abc DEFAULT 'X' MODIF ID m1.
  SELECT-OPTIONS: s_date FOR oijnomi-idate MODIF ID m2 OBLIGATORY.
  PARAMETERS: r2 RADIOBUTTON GROUP r1 MODIF ID m1.
SELECTION-SCREEN END OF BLOCK b.

INITIALIZATION.
  lv_date = sy-datum - 4.
  CALL FUNCTION 'YRX_PRVS_DATE_FM'
    EXPORTING
      s_date  = lv_date  " ABAP System Field: Current Date of Application Server
    IMPORTING
      st_date = st_date  " ABAP System Field: Current Date of Application Server
      ed_date = ed_date. " ABAP System Field: Current Date of Application Server
  REFRESH: s_date[].
  s_date-low  = st_date.
  s_date-high = ed_date.
  APPEND s_date.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'R2'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

**====================================================================**
** SECTION: CLASS INCLUDE - YRGR_033_GMS_IMBAL_CLASS                 **
** (Event handler class definition and implementation)               **
**====================================================================**

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS : get_data.
    CLASS-METHODS:
      get_data,
      button_click
        FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING
          es_col_id
          es_row_no
          sender,
      top_of_page
        FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING
          e_dyndoc_id
          table_index.
*     double_click
*       FOR EVENT double_click OF cl_gui_alv_grid
*       IMPORTING
*         e_row
*         e_column
*         es_row_no,
*     hotspot_click
*       FOR EVENT hotspot_click OF cl_gui_alv_grid
*       IMPORTING
*         e_column_id
*         e_row_id
*         es_row_no.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD get_data.
    PERFORM get_data.
  ENDMETHOD.

* METHOD button_click.
*   PERFORM button_click USING es_col_id
*                              es_row_no.
* ENDMETHOD.

  METHOD top_of_page.
    PERFORM top_of_page USING dg_dyndoc_id.
  ENDMETHOD.

* METHOD double_click.
*   PERFORM double_click.
* ENDMETHOD.

* METHOD hotspot_click.
*   PERFORM detailed_screen USING e_column_id
*                                 e_row_id
*                                 es_row_no.
* ENDMETHOD.
ENDCLASS.

**====================================================================**
** MAIN FLOW: START-OF-SELECTION                                     **
**====================================================================**

START-OF-SELECTION.
  IF r1 EQ 'X'.
    DATA: obj_rep TYPE REF TO lcl_event_handler.
    CREATE OBJECT obj_rep.
    obj_rep->get_data( ).
    PERFORM fill_fieldcat.
    PERFORM display.
  ENDIF.

**====================================================================**
** SECTION: GET_DATA INCLUDE - YRGR_033_GMS_IMBAL_GET_DATA           **
** Part A: FORM get_data                                             **
**====================================================================**

*&--------------------------------------------------------------------*
*& Form GET_DATA
*&--------------------------------------------------------------------*
*  Fetches imbalance settlement data via FM YRX_IMB_SETTLE_QTY_FM
*&--------------------------------------------------------------------*
FORM get_data.

  CALL FUNCTION 'YRX_IMB_SETTLE_QTY_FM'
    EXPORTING
      st_date  = s_date-low   " Date in CHAR Format
      ed_date  = s_date-high  " Scheduled date
    TABLES
      lt_final = lt_final.    " IMB SETTLE QTY STR

* --- Alternative logic (commented for reference) ---
** DATA: lv_date1 TYPE sy-datum,
**       lv_date2 TYPE sy-datum.
*** lv_date1 = s_date-low - 3.
*** lv_date2 = sy-datum - 3.
*** IF s_date-high LE lv_date2.
***   lv_date = s_date-high.
*** ELSE.
***   lv_date = lv_date2.
*** ENDIF.
*** FREE s_date.
*** s_date-low  = lv_date1.
*** s_date-high = lv_date.
*** APPEND s_date.

*  SELECT * FROM oijnomi
*    INTO TABLE @DATA(lt_oijnomi)
*    WHERE idate  IN @s_date
*    AND   sityp  =  'ZD'
*    AND   docind =  'G'
*    AND   delind NE 'X'.
*
*  IF lt_oijnomi[] IS NOT INITIAL.
*    SELECT vbeln, vbegdat, venddat
*      FROM veda
*      INTO TABLE @DATA(lt_veda)
*      FOR ALL ENTRIES IN @lt_oijnomi
*      WHERE vbeln   EQ @lt_oijnomi-docnr
*      AND   venddat IN @s_date.
*
*    SORT lt_oijnomi BY docnr.
*    DELETE ADJACENT DUPLICATES FROM lt_oijnomi COMPARING docnr.
*    SORT lt_veda BY vbeln.
*
**   LOOP AT lt_veda INTO DATA(ls_veda).
**     READ TABLE lt_oijnomi ...
**     DELETE lt_oijnomi WHERE docnr EQ ls_veda-vbeln.
**     CLEAR ls_veda.
**   ENDLOOP.
*
*    IF lt_veda[] IS NOT INITIAL.
*
*      SELECT * FROM yrva_gta_imb_sft
*        INTO TABLE @DATA(lt_gta_imb)
*        FOR ALL ENTRIES IN @lt_veda
*        WHERE contract_from = @lt_veda-vbeln.
*
*      SORT lt_veda    BY vbeln.
*      SORT lt_gta_imb BY contract_from.
*
*      LOOP AT lt_gta_imb INTO DATA(ls_gta_imb).
*        DELETE lt_veda WHERE vbeln = ls_gta_imb-contract_from.
*        CLEAR ls_gta_imb.
*      ENDLOOP.
*
*      SELECT * FROM vbak
*        INTO TABLE @DATA(lt_vbak)
*        FOR ALL ENTRIES IN @lt_veda
*        WHERE vbeln = @lt_veda-vbeln.
*
*      LOOP AT lt_vbak INTO DATA(ls_vbak).
*        IF ls_vbak-vbeln_grp IS NOT INITIAL.
*          DELETE lt_veda WHERE vbeln = ls_vbak-vbeln.
*          CLEAR ls_vbak.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.
*
*  LOOP AT lt_veda INTO DATA(ls_veda).
*    CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
*      EXPORTING
*        iv_date             = ls_veda-venddat
*      IMPORTING
*        ev_month_begin_date = lv_begda
*        ev_month_end_date   = lv_endda.
*
*    IF '01' <= ls_veda-venddat+6(2) AND ls_veda-venddat+6(2) <= '15'.
*      CONCATENATE lv_endda+0(4) lv_endda+4(2) '01' INTO lv_fromdat.
*    ELSEIF '16' <= ls_veda-venddat+6(2)
*       AND ls_veda-venddat+6(2) <= lv_endda+6(2).
*      CONCATENATE lv_endda+0(4) lv_endda+4(2) '16' INTO lv_fromdat.
*    ENDIF.
*
*    DATA: flg TYPE c.
*
*    SELECT SINGLE * FROM yrg_cumm_imb
*      INTO @DATA(ls_cumm_imb)
*      WHERE yy_contract = @ls_veda-vbeln
*      AND   begda       = @s_date-low    " lv_fromdat
*      AND   endda       = @s_date-high.  " ls_veda-venddat
*
*    IF ls_cumm_imb IS INITIAL.
*      ls_final-stat = 'Not Posted'.
*    ELSE.
*      ls_final-stat = 'Posted'.
**     IF flg = 'X'.
**       ls_final-ne_imbal = ls_cumm_imb-yy_oij_cumimb.
**     ELSE.
**       ls_final-po_imbal = ls_cumm_imb-yy_oij_cumimb.
**     ENDIF.
*      IF ls_cumm_imb-yy_oij_cumimb <= '0.00'.
*        ls_final-ne_imbal = ls_cumm_imb-yy_oij_cumimb.
*      ELSE.
*        ls_final-po_imbal = ls_cumm_imb-yy_oij_cumimb.
*      ENDIF.
*    ENDIF.
*
*    ls_final-vbegdat = ls_veda-vbegdat.
*    ls_final-venddat = ls_veda-venddat.
*
*    READ TABLE lt_oijnomi INTO DATA(ls_oijnomi)
*      WITH KEY docnr = ls_veda-vbeln.
*    IF sy-subrc EQ 0.
*      ls_final-docnr  = ls_oijnomi-docnr.
*      ls_final-matnr  = ls_oijnomi-matnr_i.
*      ls_final-partnr = ls_oijnomi-partnr.
*      ls_final-locid  = ls_oijnomi-locid.
*    ENDIF.
*
*    APPEND ls_final TO lt_final.
*    CLEAR: ls_oijnomi, lv_endda, lv_begda,
*           ls_veda, lv_fromdat, ls_cumm_imb, ls_final.
**   CLEAR flg.
**   flg = 'X'.
*  ENDLOOP.
*
*  DELETE lt_final WHERE po_imbal EQ '0.00'
*                    AND ne_imbal EQ '0.00'.

ENDFORM.
