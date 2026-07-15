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
* 11.06.2026  DEVELOPER              Add Sales Office (s_vkbur) selection
*                                    Add CC email input (s_cceml) replacing
*                                    hardcoded ngmc@gail.co.in
*                                    Add Action Taken radio button (r4)
*                                    Validate r1 start date >= 01.01.2022
*                                    Add notes 3 & 4 to ALV header
*                                    Email: black border, column rename,
*                                    updated intro text, sort by abs imbal
*                                    Action Taken cols in ALV (r1/r3 r-o)
*                                    NOTE: YRG_IMB_ACTION needs SE11
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
AT SELECTION-SCREEN.
  " Skip all validation during USER-COMMAND events
  " (radio button click 'ABC', Send Mail checkbox 'EML').
  IF sy-ucomm EQ 'ABC' OR sy-ucomm EQ 'EML'.
    " Screen refresh only - no validation

  " R1: start date must be on or after 01.01.2022
  ELSEIF r1 EQ 'X' AND s_date IS NOT INITIAL.
    READ TABLE s_date INTO DATA(ls_date_chk) INDEX 1.
    IF sy-subrc = 0 AND ls_date_chk-low IS NOT INITIAL.
      IF ls_date_chk-low < '20220101'.
        MESSAGE 'From date should be on or after 01.01.2022' TYPE 'E'.
      ENDIF.
    ENDIF.

  " FN date validation for Action Taken (R4) - applied to s_dat4 inputs.
  " R3 dates are auto-calculated, no validation needed.
  ELSEIF r4 EQ 'X' AND s_dat4 IS NOT INITIAL.
    READ TABLE s_dat4 INTO DATA(ls_dat4_chk) INDEX 1.
    IF sy-subrc = 0 AND ls_dat4_chk-low IS NOT INITIAL.
      lv_fn_from_day = ls_dat4_chk-low+6(2).
      IF lv_fn_from_day NE '01' AND lv_fn_from_day NE '16'.
        MESSAGE 'From date must be 1st or 16th of the month (FN start date)' TYPE 'E'.
      ENDIF.
    ENDIF.
    IF sy-subrc = 0 AND ls_dat4_chk-high IS NOT INITIAL.
      lv_fn_to_day   = ls_dat4_chk-high+6(2).
      lv_fn_next_day = ls_dat4_chk-high + 1.
      CLEAR lv_fn_is_last.
      IF lv_fn_next_day(6) NE ls_dat4_chk-high(6). lv_fn_is_last = 'X'. ENDIF.
      IF lv_fn_to_day NE '15' AND lv_fn_is_last IS INITIAL.
        MESSAGE 'To date must be 15th or last day of the month (FN end date)' TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    " Always hide r2 radio button
    IF screen-name = 'R2'.
      screen-active = 0.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.

    " Hide Gas Day (s_date / m2) when Till Date (r3) or Action Taken (r4)
    IF screen-group1 = 'M2'.
      IF r3 EQ 'X' OR r4 EQ 'X'.
        screen-active = 0.
      ELSE.
        screen-active = 1.
      ENDIF.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.

    " Sales Office s_vkbur (m6): shown for r1/r3, hidden for r4
    IF screen-group1 = 'M6'.
      IF r4 EQ 'X'.
        screen-active = 0.
      ELSE.
        screen-active = 1.
      ENDIF.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.

    " R4-specific inputs s_dat4/s_vk4 (m7): shown only for r4
    IF screen-group1 = 'M7'.
      IF r4 EQ 'X'.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.

    " Send Email checkbox (m3): show only when user has role and r3 is selected
    IF screen-name = 'P_EMAIL'.
      IF lv_has_role NE 'X' OR r3 NE 'X'.
        screen-active = 0.
      ELSE.
        screen-active = 1.
      ENDIF.
      MODIFY SCREEN.
      CONTINUE.
    ENDIF.

    " CC Email Addresses s_cceml (m4): shown when p_email checked + r3 + role
    IF screen-group1 = 'M4'.
      IF p_email IS INITIAL OR lv_has_role NE 'X' OR r3 NE 'X'.
        screen-active = 0.
      ELSE.
        screen-active = 1.
      ENDIF.
      MODIFY SCREEN.
      CONTINUE.
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

  ELSEIF r4 EQ 'X'.
    " Action Taken mode: use s_dat4/s_vk4 as date/sales-office input.
    " Shows the same contract list as R1, filtered by the user-provided date range.
    REFRESH: s_date[].
    IF s_dat4 IS NOT INITIAL.
      LOOP AT s_dat4 INTO DATA(ls_dat4).
        CLEAR s_date.
        s_date-sign   = ls_dat4-sign.
        s_date-option = ls_dat4-option.
        s_date-low    = ls_dat4-low.
        s_date-high   = ls_dat4-high.
        APPEND s_date.
      ENDLOOP.
    ELSE.
      " Default: last full fortnight when no date entered
      lv_date = sy-datum - 3.
      CALL FUNCTION 'YRX_PRVS_DATE_FM'
        EXPORTING s_date = lv_date
        IMPORTING st_date = st_date ed_date = ed_date.
      CLEAR s_date.
      s_date-low = st_date. s_date-high = ed_date. APPEND s_date.
    ENDIF.
    DATA: obj_r4 TYPE REF TO lcl_event_handler.
    CREATE OBJECT obj_r4.
    obj_r4->get_data( ).
    PERFORM fill_fieldcat.
    PERFORM display.
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
