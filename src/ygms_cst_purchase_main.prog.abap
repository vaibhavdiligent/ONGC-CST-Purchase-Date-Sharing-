*&---------------------------------------------------------------------*
*& Report YGMS_CST_PURCHASE_MAIN
*&---------------------------------------------------------------------*
*& Description: ONGC CST Purchase Data Sharing - Main Program
*& Author:      [To be filled]
*& Date:        [Creation date]
*& Module:      FI/SD
*& WRICEF ID:   R-GAIL-CST-001
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*
REPORT ygms_cst_purchase_main MESSAGE-ID ygms_msg.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: go_controller  TYPE REF TO ygms_cl_cst_controller,
      go_alv_handler TYPE REF TO ygms_cl_cst_alv_handler.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
  PARAMETERS:
    rb_upld  RADIOBUTTON GROUP mode DEFAULT 'X' USER-COMMAND mode,
    rb_alloc RADIOBUTTON GROUP mode,
    rb_view  RADIOBUTTON GROUP mode,
    rb_send  RADIOBUTTON GROUP mode,
    rb_dwnld RADIOBUTTON GROUP mode,
    rb_nom   RADIOBUTTON GROUP mode.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
  PARAMETERS:
    p_loc    TYPE ygms_loc_id OBLIGATORY,
    p_frdat  TYPE datum,
    p_todat  TYPE datum.
  SELECT-OPTIONS:
    s_mat    FOR ygms_cst_pur-material.
  PARAMETERS:
    p_file   TYPE string LOWER CASE MODIF ID upl.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
  SELECT-OPTIONS:
    s_exst   FOR ygms_cst_pur-state_code NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* At Selection Screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_loc.
  PERFORM f4_location.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  " Set default dates to current fortnight
  DATA(lv_day) = sy-datum+6(2).
  IF lv_day <= 15.
    p_frdat = sy-datum(6) && '01'.
    p_todat = sy-datum(6) && '15'.
  ELSE.
    p_frdat = sy-datum(6) && '16'.
    " Calculate end of month
    DATA(lv_next_month) = sy-datum.
    lv_next_month+6(2) = '01'.
    lv_next_month = lv_next_month + 32.
    lv_next_month+6(2) = '01'.
    p_todat = lv_next_month - 1.
  ENDIF.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  " Initialize controller
  go_controller = NEW ygms_cl_cst_controller(
    iv_location_id = p_loc
    iv_from_date   = p_frdat
    iv_to_date     = p_todat
  ).

  TRY.
      DATA: lt_messages   TYPE bapiret2_t,
            lt_allocation TYPE ygms_tt_allocation.

      CASE abap_true.
        WHEN rb_upld.
          " Upload mode
          go_controller->process_upload(
            EXPORTING iv_file_path = p_file
            IMPORTING et_messages  = lt_messages
          ).
          PERFORM display_messages USING lt_messages.

        WHEN rb_alloc.
          " Allocation mode
          go_controller->execute_allocation(
            EXPORTING it_excluded_states = s_exst[]
            IMPORTING et_allocation      = lt_allocation
                      et_messages        = lt_messages
          ).

          " Display ALV for editing
          go_alv_handler = NEW ygms_cl_cst_alv_handler( ).
          go_alv_handler->display_allocation( lt_allocation ).

        WHEN rb_view.
          " View mode - Display existing data in ALV

        WHEN rb_send.
          " Send mode - Send data via email

        WHEN rb_dwnld.
          " Download mode - Download data to Excel

        WHEN rb_nom.
          " Nomination mode - Display purchase nominations

      ENDCASE.

    CATCH ygms_cx_cst_error INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.

*----------------------------------------------------------------------*
* Forms
*----------------------------------------------------------------------*
FORM f4_location.
  DATA: lt_return TYPE STANDARD TABLE OF ddshretval.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname    = 'YGMS_CST_LOC_MAP'
      fieldname  = 'LOCATION_ID'
    TABLES
      return_tab = lt_return.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
  IF sy-subrc = 0.
    p_loc = ls_return-fieldval.
  ENDIF.
ENDFORM.

FORM f4_file.
  DATA: lt_file TYPE filetable,
        lv_rc   TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      file_filter = '*.xlsx;*.xls'
    CHANGING
      file_table  = lt_file
      rc          = lv_rc
  ).

  READ TABLE lt_file INTO DATA(ls_file) INDEX 1.
  IF sy-subrc = 0.
    p_file = ls_file-filename.
  ENDIF.
ENDFORM.

FORM modify_screen.
  LOOP AT SCREEN.
    " Show file path only for upload mode
    IF screen-group1 = 'UPL' AND rb_upld <> abap_true.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM display_messages USING it_messages TYPE bapiret2_t.
  LOOP AT it_messages INTO DATA(ls_message).
    CASE ls_message-type.
      WHEN 'E'.
        WRITE: / icon_led_red AS ICON, ls_message-message.
      WHEN 'W'.
        WRITE: / icon_led_yellow AS ICON, ls_message-message.
      WHEN 'S' OR 'I'.
        WRITE: / icon_led_green AS ICON, ls_message-message.
    ENDCASE.
  ENDLOOP.
ENDFORM.
