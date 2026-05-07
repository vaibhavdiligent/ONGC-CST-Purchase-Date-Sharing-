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
* Change          : Added Till Date radio button, Send Email checkbox,
*                   email logic
*&---------------------------------------------------------------------*
* Modification Log:
* Date        Author      Req.No.    Change History
* 07.05.2026  DEVELOPER              Hide Gas Day when Till Date selected
*                                    CC: ngmc@gail.co.in
*                                    HTML tabular email body
*                                    Not Posted: omit Posted Imbal cols
*                                    Sender: GAIL PARTNER CARE GMS
*                                    Removed 'for' prefix from subject/body
*                                    CT dates formatted DD.MM.YYYY
*                                    Message: 'Emails sent successfully'
*                                    'Sales Office' label before office no.
*                                    Last Changed-by (YRVA_CON_WF_LOG
*                                    sort+dedup) same for Posted and
*                                    Not Posted
*&---------------------------------------------------------------------*
REPORT yrgr_033_gms_imbal.

DATA: dg_parent_grid TYPE REF TO cl_gui_container,
      dg_dyndoc_id   TYPE REF TO cl_dd_document,
      dg_splitter    TYPE REF TO cl_gui_splitter_container.

INCLUDE yrgr_033_gms_imbal_top.
INCLUDE yrgr_033_gms_imbal_class.
INCLUDE yrgr_033_gms_imbal_get_data.

*----------------------------------------------------------------------*
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
  SELECT SINGLE uname FROM agr_users
    INTO @DATA(lv_uname)
    WHERE uname    = @sy-uname
    AND   agr_name = 'ZO_CC_EHS.GMS_ROLE'.
  IF sy-subrc EQ 0. lv_has_role = 'X'. ENDIF.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    " Always hide r2 radio button
    IF screen-name = 'R2'.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
    " Hide Send Email checkbox unless user has role and Till Date selected
    IF screen-name = 'P_EMAIL'.
      IF lv_has_role NE 'X' OR r3 NE 'X'.
        screen-active = 0.
      ELSE.
        screen-active = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    " Hide Gas Day (s_date) label and fields when Till Date (r3) is selected
    " MODIF ID m2 covers both the select-option fields and its text label
    IF screen-group1 = 'M2'.
      IF r3 EQ 'X'.
        screen-active = 0.
      ELSE.
        screen-active = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*----------------------------------------------------------------------*
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
      s_date-low  = '20250901'.
      s_date-high = ed_date.
      APPEND s_date.
    ENDIF.
    DATA: obj_rep TYPE REF TO lcl_event_handler.
    CREATE OBJECT obj_rep.
    obj_rep->get_data( ).
    PERFORM fill_fieldcat.
    IF p_email IS INITIAL.
      PERFORM display.
    ELSE.
      IF r3 EQ 'X' AND p_email EQ 'X'.
        PERFORM send_emails.
        MESSAGE 'Emails sent successfully' TYPE 'S'.
      ENDIF.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'SALV_STANDARD'.
  SET TITLEBAR 'GMS_TITLE'.
ENDMODULE.

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&F03' OR '&F12' OR '&F15'.
      SET SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
