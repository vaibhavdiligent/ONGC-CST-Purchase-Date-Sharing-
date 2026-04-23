*&---------------------------------------------------------------------*
*& Report YRGR_033_GMS_IMBAL
*&---------------------------------------------------------------------*
* Name of Program : YRGR_033_GMS_IMBAL
* T-Code          :
* Developed By    : Ravinder Singh
* Functional      : Pratibha Dangwal
* Date            : 08.05.2024
* DESCRIPTION     : Report For Closing Imbalance Of Expired Contracts
* CHARM ID        : 2000000826 / TR NO : DVRK9A19P3
* Change          : Added Till Date radio button, Send Email checkbox, email logic
*&---------------------------------------------------------------------*
REPORT yrgr_033_gms_imbal.

** Data Declarations **
DATA: dg_parent_grid TYPE REF TO cl_gui_container,
      dg_dyndoc_id   TYPE REF TO cl_dd_document,
      dg_splitter    TYPE REF TO cl_gui_splitter_container.

** Include Programs **
INCLUDE yrgr_033_gms_imbal_top.
INCLUDE yrgr_033_gms_imbal_class.
INCLUDE yrgr_033_gms_imbal_get_data.

INITIALIZATION.
  lv_date = sy-datum - 4.
  CALL FUNCTION 'YRX_PRVS_DATE_FM'
    EXPORTING
      s_date  = lv_date
    IMPORTING
      st_date = st_date
      ed_date = ed_date.
  REFRESH: s_date[].
  s_date-low = st_date. s_date-high = ed_date. APPEND s_date.
  SELECT SINGLE uname FROM agr_users INTO @DATA(lv_uname)
    WHERE uname = @sy-uname AND agr_name = 'ZO_CC_EHS.GMS_ROLE'.
  IF sy-subrc EQ 0. lv_has_role = 'X'. ENDIF.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = 'R2'. screen-active = 0. MODIFY SCREEN. ENDIF.
    IF screen-name = 'P_EMAIL'.
      IF lv_has_role NE 'X' OR r3 NE 'X'. screen-active = 0.
      ELSE. screen-active = 1. ENDIF.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'S_DATE-LOW' OR screen-name = 'S_DATE-HIGH'.
      IF r3 EQ 'X'. screen-active = 0. ELSE. screen-active = 1. ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

START-OF-SELECTION.
  IF r1 EQ 'X' OR r3 EQ 'X'.
    IF r3 EQ 'X'.
      lv_date = sy-datum - 3.
      CALL FUNCTION 'YRX_PRVS_DATE_FM'
        EXPORTING
          s_date  = lv_date
        IMPORTING
          st_date = st_date
          ed_date = ed_date.
      REFRESH: s_date[].
      s_date-low  = '20250901'.   " Fixed start: 01.09.2025
      s_date-high = ed_date.      " End: previous FN end date (sy-datum - 3)
      APPEND s_date.
    ENDIF.
** Declaring Object for Class
    DATA: obj_rep TYPE REF TO lcl_event_handler.
** Creating Object
    CREATE OBJECT obj_rep.
** Calling class Methods
    obj_rep->get_data( ).
** Calling fill_fieldcat
    PERFORM fill_fieldcat.
** Calling Display ALV
    PERFORM display.
    IF r3 EQ 'X' AND p_email EQ 'X'.
      PERFORM send_emails.
    ENDIF.
  ENDIF.
