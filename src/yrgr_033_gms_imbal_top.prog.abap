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

DATA: lv_cceml TYPE ad_smtpadr.   " Reference field for s_cceml SELECT-OPTIONS

*----------------------------------------------------------------------*
* Extended final type: yrx_imb_settle_qty + Action Taken columns
* NOTE: YRG_IMB_ACTION table must be created in SE11 before
*       the Action Taken save/read logic can be activated.
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_action_cols,
         at_chkbox  TYPE c LENGTH 1,   " Action Taken flag (X = action done)
         at_sal_ord TYPE vbeln,         " Sales Order (from YRG_IMB_ACTION)
         at_qty     TYPE menge,         " Quantity   (from YRG_IMB_ACTION)
         at_remarks TYPE char100,       " Remarks    (from YRG_IMB_ACTION)
       END OF ty_action_cols.

TYPES: BEGIN OF ty_final_ext.
         INCLUDE TYPE yrx_imb_settle_qty AS base.
         INCLUDE TYPE ty_action_cols     AS action.
       END OF ty_final_ext.

DATA: lt_final_ext TYPE STANDARD TABLE OF ty_final_ext WITH DEFAULT KEY,
      ls_final_ext TYPE ty_final_ext.

*----------------------------------------------------------------------*
* Selection Screen
* r1 = Report for Closing Imbalance (uses s_date date range)
*      SE38 text element for R1: 'Report for Closing Imbalance'
* r2 = (reserved – always hidden)
* r3 = Till Date (auto-calculates date range; hides Gas Day s_date)
*      SE38 text element for R3: 'Till Date'
* r4 = Action Taken (new radio button)
*      SE38 text element for R4: 'Action Taken'
*
* MODIF IDs:
*   m1 = radio buttons (r1, r2, r3, r4)
*   m2 = Gas Day s_date (hidden for r3/r4)
*   m3 = p_email Send Mail checkbox
*   m4 = s_cceml CC emails (shown when p_email checked + r3 + role)
*   m5 = r2 (always hidden)
*   m6 = s_vkbur Sales Office for r1/r3 (hidden for r4)
*   m7 = s_dat4/s_vk4 inputs for r4 (hidden for r1/r3)
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b WITH FRAME TITLE TEXT-001.
  PARAMETERS:    r1 RADIOBUTTON GROUP r1 USER-COMMAND abc DEFAULT 'X' MODIF ID m1.
  SELECT-OPTIONS: s_date  FOR oijnomi-idate MODIF ID m2 OBLIGATORY.
  SELECT-OPTIONS: s_vkbur FOR lv_vkbur      MODIF ID m6.
  PARAMETERS:    r2 RADIOBUTTON GROUP r1 MODIF ID m5.          " Always hidden
  PARAMETERS:    r3 RADIOBUTTON GROUP r1 MODIF ID m1.          " Till Date
  PARAMETERS:    r4 RADIOBUTTON GROUP r1 MODIF ID m1.          " Action Taken
  SELECT-OPTIONS: s_dat4  FOR oijnomi-idate MODIF ID m7.
  SELECT-OPTIONS: s_vk4   FOR lv_vkbur      MODIF ID m7.
  PARAMETERS:    p_email AS CHECKBOX MODIF ID m3 USER-COMMAND eml.
  SELECT-OPTIONS: s_cceml FOR lv_cceml NO INTERVALS MODIF ID m4.
SELECTION-SCREEN END OF BLOCK b.
