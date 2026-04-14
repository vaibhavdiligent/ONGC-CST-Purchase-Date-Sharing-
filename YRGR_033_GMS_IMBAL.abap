*&---------------------------------------------------------------------*
*& Report YRGR_033_GMS_IMBAL
*&---------------------------------------------------------------------*
* Name of Program : YRGR_033_GMS_IMBAL
* Developed By    : Ravinder Singh
* Functional      : Pratibha Dangwal
* Date            : 08.05.2024
* DESCRIPTION     : Report For Closing Imbalance Of Expired Contracts
* CHARM ID        : 2000000826 / TR NO : DVRK9A19P3
*&---------------------------------------------------------------------*
REPORT yrgr_033_gms_imbal.

DATA: dg_parent_grid TYPE REF TO cl_gui_container,
      dg_dyndoc_id   TYPE REF TO cl_dd_document,
      dg_splitter    TYPE REF TO cl_gui_splitter_container.

**====================================================================**
** TOP INCLUDE
**====================================================================**
TABLES : oijnomi, veda.

TYPES: BEGIN OF t_final,
  docnr    TYPE oijnomi-docnr,
  matnr    TYPE oijnomi-matnr_i,
  locid    TYPE oijnomi-locid,
  partnr   TYPE oijnomi-partnr,
  vbegdat  TYPE veda-vbegdat,
  venddat  TYPE veda-venddat,
  stat     TYPE char15,
  po_imbal TYPE yy_oij_cumimb,
  ne_imbal TYPE yy_oij_cumimb,
  ret_del  TYPE vbeln,
  crd_not  TYPE vbeln,
  sal_ord  TYPE vbeln,
  do       TYPE vbeln,
  pgi      TYPE vbeln,
  invoic   TYPE vbeln,
  pur_ret  TYPE ebeln,
  po       TYPE ebeln,
  gr       TYPE ebeln,
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
    EXPORTING  s_date  = lv_date
    IMPORTING  st_date = st_date
               ed_date = ed_date.
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
** CLASS INCLUDE
**====================================================================**
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS : get_data.
    CLASS-METHODS:
      get_data,
      button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no sender,
      top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING e_dyndoc_id table_index.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD get_data.
    PERFORM get_data.
  ENDMETHOD.
  METHOD top_of_page.
    PERFORM top_of_page USING dg_dyndoc_id.
  ENDMETHOD.
ENDCLASS.

**====================================================================**
** START-OF-SELECTION
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
** FORM get_data
**====================================================================**
FORM get_data.
  CALL FUNCTION 'YRX_IMB_SETTLE_QTY_FM'
    EXPORTING
      st_date  = s_date-low
      ed_date  = s_date-high
    TABLES
      lt_final = lt_final.
ENDFORM.

**====================================================================**
** FORM fill_fieldcat - all 19 cols
**====================================================================**
FORM fill_fieldcat.
  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'DOCNR'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '10'.
  gs_fieldcat-scrtext_l = 'Contract ID'.
  gs_fieldcat-scrtext_m = 'Contract ID'.
  gs_fieldcat-scrtext_s = 'Contract ID'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'MATNR'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '10'.
  gs_fieldcat-scrtext_l = 'Material'.
  gs_fieldcat-scrtext_m = 'Material'.
  gs_fieldcat-scrtext_s = 'Material'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'PARTNR'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '10'.
  gs_fieldcat-scrtext_l = 'Customer'.
  gs_fieldcat-scrtext_m = 'Customer'.
  gs_fieldcat-scrtext_s = 'Customer'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'LOCID'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '8'.
  gs_fieldcat-scrtext_l = 'Location ID'.
  gs_fieldcat-scrtext_m = 'Location ID'.
  gs_fieldcat-scrtext_s = 'Location ID'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'VBEGDAT'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '10'.
  gs_fieldcat-scrtext_l = 'CT Start Date'.
  gs_fieldcat-scrtext_m = 'CT Start Date'.
  gs_fieldcat-scrtext_s = 'CT Start Date'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'VENDDAT'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '10'.
  gs_fieldcat-scrtext_l = 'CT End Date'.
  gs_fieldcat-scrtext_m = 'CT End Date'.
  gs_fieldcat-scrtext_s = 'CT End Date'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'STAT'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '12'.
  gs_fieldcat-scrtext_l = 'Status'.
  gs_fieldcat-scrtext_m = 'Status'.
  gs_fieldcat-scrtext_s = 'Status'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'PO_IMBAL'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '15'.
  gs_fieldcat-no_zero   = 'X'.
  gs_fieldcat-scrtext_l = 'Positive Imbalance'.
  gs_fieldcat-scrtext_m = 'Positive Imbalance'.
  gs_fieldcat-scrtext_s = 'Pos Imbal'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'NE_IMBAL'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '15'.
  gs_fieldcat-no_zero   = 'X'.
  gs_fieldcat-scrtext_l = 'Negative Imbalance'.
  gs_fieldcat-scrtext_m = 'Negative Imbalance'.
  gs_fieldcat-scrtext_s = 'Neg Imbal'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'VKBUR'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '5'.
  gs_fieldcat-scrtext_l = 'Sales Office'.
  gs_fieldcat-scrtext_m = 'Sales Office'.
  gs_fieldcat-scrtext_s = 'Sales Office'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'RET_DEL'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '10'.
  gs_fieldcat-scrtext_l = 'Return Delivery (Rtrn Del)'.
  gs_fieldcat-scrtext_m = 'Return Delivery (Rtrn Del)'.
  gs_fieldcat-scrtext_s = 'Ret Del'.
  gs_fieldcat-no_out    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'CRD_NOT'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '10'.
  gs_fieldcat-scrtext_l = 'Credit Note'.
  gs_fieldcat-scrtext_m = 'Credit Note'.
  gs_fieldcat-scrtext_s = 'Crd Note'.
  gs_fieldcat-no_out    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'SAL_ORD'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '10'.
  gs_fieldcat-scrtext_l = 'Sales Order'.
  gs_fieldcat-scrtext_m = 'Sales Order'.
  gs_fieldcat-scrtext_s = 'Sal_Ord'.
  gs_fieldcat-no_out    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'DO'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '10'.
  gs_fieldcat-scrtext_l = 'DO'.
  gs_fieldcat-scrtext_m = 'DO'.
  gs_fieldcat-scrtext_s = 'DO'.
  gs_fieldcat-no_out    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'PGI'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '10'.
  gs_fieldcat-scrtext_l = 'PGI'.
  gs_fieldcat-scrtext_m = 'PGI'.
  gs_fieldcat-scrtext_s = 'PGI'.
  gs_fieldcat-no_out    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'INVOIC'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '10'.
  gs_fieldcat-scrtext_l = 'Invoice'.
  gs_fieldcat-scrtext_m = 'Invoice'.
  gs_fieldcat-scrtext_s = 'Invoice'.
  gs_fieldcat-no_out    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'PUR_RET'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '10'.
  gs_fieldcat-scrtext_l = 'Purchase Return (Pur Rtrn)'.
  gs_fieldcat-scrtext_m = 'Purchase Return (Pur Rtrn)'.
  gs_fieldcat-scrtext_s = 'Pur_Ret'.
  gs_fieldcat-no_out    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'PO'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '10'.
  gs_fieldcat-scrtext_l = 'PO'.
  gs_fieldcat-scrtext_m = 'PO'.
  gs_fieldcat-scrtext_s = 'PO'.
  gs_fieldcat-no_out    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname = 'GR'.
  gs_fieldcat-tabname   = 'LT_FINAL'.
  gs_fieldcat-outputlen = '10'.
  gs_fieldcat-scrtext_l = 'GR'.
  gs_fieldcat-scrtext_m = 'GR'.
  gs_fieldcat-scrtext_s = 'GR'.
  gs_fieldcat-no_out    = 'X'.
  APPEND gs_fieldcat TO gt_fieldcat.
ENDFORM.

**====================================================================**
** FORM display
**====================================================================**
FORM display.
  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING container_name = g_container.
    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.
    CALL METHOD dg_splitter->get_container
      EXPORTING row = 1 column = 1
      RECEIVING container = dg_parent_html.
    CALL METHOD dg_splitter->get_container
      EXPORTING row = 2 column = 1
      RECEIVING container = dg_parent_grid.
    CALL METHOD dg_splitter->set_row_height
      EXPORTING id = 1 height = 24.
    CREATE OBJECT grid
      EXPORTING i_parent = dg_parent_grid.
    gs_layout-stylefname = 'CELL'.
    SET HANDLER lcl_event_handler=>top_of_page FOR grid.
    CALL METHOD grid->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = lt_exclude
        is_layout            = gs_layout
      CHANGING
        it_fieldcatalog      = gt_fieldcat
        it_outtab            = lt_final[].
  ENDIF.
  CREATE OBJECT dg_dyndoc_id
    EXPORTING style = 'ALV_GRID'.
  CALL METHOD dg_dyndoc_id->initialize_document.
  CALL METHOD grid->list_processing_events
    EXPORTING
      i_event_name = 'TOP_OF_PAGE'
      i_dyndoc_id  = dg_dyndoc_id.
  CALL SCREEN 100.
ENDFORM.

**====================================================================**
** MODULE status_0100 OUTPUT (PBO)
**====================================================================**
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'SALV_STANDARD'.
  SET TITLEBAR 'GMS_TITLE'.
ENDMODULE.

**====================================================================**
** MODULE user_command_0100 INPUT (PAI)
**====================================================================**
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&F03' OR '&F12' OR '&F15'.
      SET SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
