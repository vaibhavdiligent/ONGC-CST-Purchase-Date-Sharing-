*&---------------------------------------------------------------------*
*& Report YRGR_033_GMS_IMBAL
*&---------------------------------------------------------------------*
* Name of Program : YRGR_033_GMS_IMBAL
* Developed By    : Ravinder Singh
* Functional      : Pratibha Dangwal
* Date            : 08.05.2024
* DESCRIPTION     : Report For Closing Imbalance Of Expired Contracts
* CHARM ID        : 2000000826 / TR NO : DVRK9A19P3
* Change  : Added Till Date radio button, Send Email checkbox, email logic
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
DATA: lv_subrc     TYPE sy-subrc,
      lv_send_date TYPE sy-datum,
      lv_send_time TYPE sy-uzeit.
DATA: lt_email_to  TYPE TABLE OF ad_smtpadr,
      lt_email_cc  TYPE TABLE OF ad_smtpadr,
      lv_email     TYPE ad_smtpadr.
DATA: lv_vkbur    TYPE vkbur,
      lv_locid    TYPE oijnomi-locid.
DATA: lv_has_role TYPE c LENGTH 1.

SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-001.
  PARAMETERS: r1 RADIOBUTTON GROUP r1
                 USER-COMMAND abc DEFAULT 'X' MODIF ID m1.  " FN Wise
  SELECT-OPTIONS: s_date FOR oijnomi-idate MODIF ID m2 OBLIGATORY.
  PARAMETERS: r2 RADIOBUTTON GROUP r1 MODIF ID m5.          " (deactivated)
  PARAMETERS: r3 RADIOBUTTON GROUP r1 MODIF ID m1.          " Till Date (from Sept 2025)
  PARAMETERS: p_email AS CHECKBOX MODIF ID m3.              " Send Email (role-restricted)
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
  SELECT SINGLE uname FROM agr_users INTO @DATA(lv_uname)
    WHERE uname    = @sy-uname
    AND   agr_name = 'ZO_CC_EHS.GMS_ROLE'.
  IF sy-subrc EQ 0.
    lv_has_role = 'X'.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'R2'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'P_EMAIL'.
      IF lv_has_role NE 'X' OR r3 NE 'X'.
        screen-active = 0.
      ELSE.
        screen-active = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'S_DATE-LOW' OR screen-name = 'S_DATE-HIGH'.
      IF r3 EQ 'X'.
        screen-active = 0.
      ELSE.
        screen-active = 1.
      ENDIF.
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
  IF r1 EQ 'X' OR r3 EQ 'X'.
    " For Till Date radio button: fix date range
    IF r3 EQ 'X'.
      lv_date = sy-datum - 3.
      CALL FUNCTION 'YRX_PRVS_DATE_FM'
        EXPORTING  s_date  = lv_date
        IMPORTING  st_date = st_date
                   ed_date = ed_date.
      REFRESH: s_date[].
      s_date-low  = '20250901'.  " Fixed start: 01.09.2025
      s_date-high = ed_date.     " End: previous FN end date
      APPEND s_date.
    ENDIF.
    DATA: obj_rep TYPE REF TO lcl_event_handler.
    CREATE OBJECT obj_rep.
    obj_rep->get_data( ).
    PERFORM fill_fieldcat.
    PERFORM display.
    " Send email if Till Date selected and checkbox active
    IF r3 EQ 'X' AND p_email EQ 'X'.
      PERFORM send_emails.
    ENDIF.
  ENDIF.
