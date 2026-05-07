*&---------------------------------------------------------------------*
*& Include  YRGR_033_GMS_IMBAL_TOP
*& Global types, data, and selection screen
*&---------------------------------------------------------------------*

TABLES: oijnomi, veda.

TYPE-POOLS: slis.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: lt_final TYPE TABLE OF yrx_imb_settle_qty,
      ls_final TYPE yrx_imb_settle_qty.

DATA: dg_html_cntrl   TYPE REF TO cl_gui_html_viewer,
      dg_parent_html  TYPE REF TO cl_gui_container.

DATA: lt_exclude TYPE ui_functions,
      ls_exclude TYPE ui_func.

DATA: gs_fieldcat TYPE lvc_s_fcat,
      gs_layout   TYPE lvc_s_layo,
      gt_fieldcat TYPE lvc_t_fcat,
      grid        TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      g_container        TYPE scrfname VALUE 'CUST'.

DATA: lv_begda   TYPE datum,
      lv_endda   TYPE datum,
      lv_fromdat TYPE datum,
      lv_todat   TYPE datum,
      lv_dat     TYPE char2,
      lv_dat1    TYPE datum,
      lv_dat2    TYPE datum.

DATA: last_day_of_month LIKE sy-datum.

DATA: st_date TYPE sy-datum,
      ed_date TYPE sy-datum,
      lv_date TYPE sy-datum.

DATA: lv_subrc     TYPE sy-subrc,
      lv_send_date TYPE sy-datum,
      lv_send_time TYPE sy-uzeit.

DATA: lt_email_to TYPE TABLE OF ad_smtpadr,
      lt_email_cc TYPE TABLE OF ad_smtpadr,
      lv_email    TYPE ad_smtpadr.

DATA: lv_vkbur TYPE vkbur,
      lv_locid TYPE oijnomi-locid.

DATA: lv_has_role TYPE c LENGTH 1.

*----------------------------------------------------------------------*
* Selection Screen
* r1 = Report for Closing Imbalance (uses s_date date range)
* r2 = (reserved – always hidden)
* r3 = Till Date (auto-calculates date range; hides Gas Day s_date)
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-001.
  PARAMETERS: r1 RADIOBUTTON GROUP r1 USER-COMMAND abc DEFAULT 'X' MODIF ID m1.
  SELECT-OPTIONS: s_date FOR oijnomi-idate MODIF ID m2 OBLIGATORY.
  PARAMETERS: r2 RADIOBUTTON GROUP r1 MODIF ID m5.   " Always hidden
  PARAMETERS: r3 RADIOBUTTON GROUP r1 MODIF ID m1.   " Till Date
  PARAMETERS: p_email AS CHECKBOX MODIF ID m3.       " Send Email (role-protected)
SELECTION-SCREEN END OF BLOCK b.
