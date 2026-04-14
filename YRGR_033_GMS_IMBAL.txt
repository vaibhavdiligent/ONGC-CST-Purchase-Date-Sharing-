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
