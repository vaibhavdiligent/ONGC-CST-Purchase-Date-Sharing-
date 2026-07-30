*&---------------------------------------------------------------------*
*& Include  YRGR_033_GMS_IMBAL_TOP
*& Global types, data, and selection screen
*&---------------------------------------------------------------------*

TABLES: oijnomi, veda.

TYPE-POOLS: slis, icon.

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

" Comment fields for the Notes block on the selection screen (block b2).
" Filled at INITIALIZATION so no SE38 text elements need maintaining.
" 79 is the maximum width of a SELECTION-SCREEN COMMENT.
DATA: cmt1 TYPE c LENGTH 79,
      cmt2 TYPE c LENGTH 79,
      cmt3 TYPE c LENGTH 79,
      cmt4 TYPE c LENGTH 79,
      cmt5 TYPE c LENGTH 79.

" Deferred grid refresh: set in DATA_CHANGED, consumed in DATA_CHANGED_FINISHED.
" Refreshing inside DATA_CHANGED dumps with OBJECTS_OBJREF_NOT_ASSIGNED_NO.
DATA: gv_refresh_grid TYPE c LENGTH 1,
      gs_stable       TYPE lvc_s_stbl.

" Number of alert mails actually sent – used to suppress the success
" message when there was nothing to report.
DATA: gv_mail_count TYPE i.

DATA: lv_fn_from_day TYPE c LENGTH 2,
      lv_fn_to_day   TYPE c LENGTH 2,
      lv_fn_next_day TYPE sy-datum,
      lv_fn_is_last  TYPE c LENGTH 1.

*----------------------------------------------------------------------*
* Extended final type: yrx_imb_settle_qty + Action Taken columns
* NOTE: YRG_IMB_ACTION table must be created in SE11 before
*       the Action Taken save/read logic can be activated.
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_action_cols,
         at_chkbox   TYPE c LENGTH 1,   " Action Taken flag (X = action done)
         at_sal_ord  TYPE vbeln,         " Sales Order (from YRG_IMB_ACTION)
         at_qty      TYPE char20,        " Quantity   (from YRG_IMB_ACTION)
         at_remarks  TYPE char255,       " Remarks    (from YRG_IMB_ACTION)
         at_changed  TYPE c LENGTH 1,   " 'X' = row was edited in R4 mode
       END OF ty_action_cols.

" Display-only fields: cell style (editability) + cell colour (SO error)
TYPES: BEGIN OF ty_display_cols,
         cell      TYPE lvc_t_styl,     " stylefname='CELL': Not Posted rows disabled
         cellcolor TYPE lvc_t_scol,     " ctab_fname='CELLCOLOR': SO error in red
       END OF ty_display_cols.

TYPES: BEGIN OF ty_final_ext.
  INCLUDE TYPE yrx_imb_settle_qty AS base.
  INCLUDE TYPE ty_action_cols     AS action.
  INCLUDE TYPE ty_display_cols    AS disp.
TYPES: END OF ty_final_ext.

DATA: lt_final_ext TYPE STANDARD TABLE OF ty_final_ext,
      ls_final_ext TYPE ty_final_ext.

*----------------------------------------------------------------------*
* Selection Screen Layout (top to bottom on screen):
*   r1  = Report for Closing Imbalance  [SE38 text: keep short; long desc in Note 4]
*   s_date / s_vkbur  (m2/m6) – Gas Day + Sales Office, hidden for r3/r4
*   r2  = (always hidden, m5)
*   r3  = Till Date (with effect after 2UoM Migration)  [SE38 text: update to full label]
*   p_email / s_cceml (m3/m4) – Send Mail + CC, shown directly below r3 when active
*   r4  = Action Taken  [SE38 text: 'Action Taken'] — last radio button
*   s_dat4 / s_vk4 (m7) – date + sales office for r4 mode
*
* MODIF IDs:
*   m1 = radio buttons (r1, r2, r3, r4)
*   m2 = Gas Day s_date (hidden for r3/r4)
*   m3 = p_email Send Mail checkbox (shown for r3 + role; positioned below r3)
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
  PARAMETERS:    r3 RADIOBUTTON GROUP r1 MODIF ID m1.          " Till Date (2UoM)
  PARAMETERS:    p_email AS CHECKBOX MODIF ID m3 USER-COMMAND eml.
  SELECT-OPTIONS: s_cceml FOR lv_cceml NO INTERVALS MODIF ID m4.
  PARAMETERS:    r4 RADIOBUTTON GROUP r1 MODIF ID m1.          " Action Taken (last)
  SELECT-OPTIONS: s_dat4  FOR oijnomi-idate MODIF ID m7.
  SELECT-OPTIONS: s_vk4   FOR lv_vkbur      MODIF ID m7.
SELECTION-SCREEN END OF BLOCK b.

*----------------------------------------------------------------------*
* Notes block (reference YRXR025N) – explains the data effectivity date
* of each radio button. Texts are assigned at INITIALIZATION.
* Maintain TEXT-002 in SE38 -> Goto -> Text Elements as 'Notes'.
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECTION-SCREEN COMMENT /1(79) cmt1.
  SELECTION-SCREEN COMMENT /1(79) cmt2.
  SELECTION-SCREEN COMMENT /1(79) cmt3.
  SELECTION-SCREEN COMMENT /1(79) cmt4.
  SELECTION-SCREEN COMMENT /1(79) cmt5.
SELECTION-SCREEN END OF BLOCK b2.
