*&---------------------------------------------------------------------*
*& Include YGMS_CST_PURCHASE_F01
*& Description: Forms and Subroutines
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& Set default values for selection screen
*&---------------------------------------------------------------------*
FORM initialization.
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
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_LOCATION
*&---------------------------------------------------------------------*
*& F4 Help for Location ID
*&---------------------------------------------------------------------*
FORM f4_location.
  DATA: lt_return TYPE STANDARD TABLE OF ddshretval.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname    = 'YGMS_CST_LOC_MAP'
      fieldname  = 'LOCATION_ID'
    TABLES
      return_tab = lt_return
    EXCEPTIONS
      OTHERS     = 1.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
  IF sy-subrc = 0.
    p_loc = ls_return-fieldval.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_FILE
*&---------------------------------------------------------------------*
*& F4 Help for File Path
*&---------------------------------------------------------------------*
FORM f4_file.
  DATA: lt_file TYPE filetable,
        lv_rc   TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      file_filter = '*.xlsx;*.xls'
    CHANGING
      file_table  = lt_file
      rc          = lv_rc
    EXCEPTIONS
      OTHERS      = 1
  ).

  READ TABLE lt_file INTO DATA(ls_file) INDEX 1.
  IF sy-subrc = 0.
    p_file = ls_file-filename.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form MODIFY_SCREEN
*&---------------------------------------------------------------------*
*& Modify screen based on selected mode
*&---------------------------------------------------------------------*
FORM modify_screen.
  LOOP AT SCREEN.
    " Show file path only for upload mode
    IF screen-group1 = 'UPL' AND rb_upld <> abap_true.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    " Show email field only for send mode
    IF screen-group1 = 'SND' AND rb_send <> abap_true.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    " Show download path only for download mode
    IF screen-group1 = 'DWN' AND rb_dwnld <> abap_true.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATE_SELECTION
*&---------------------------------------------------------------------*
*& Validate selection screen inputs
*&---------------------------------------------------------------------*
FORM validate_selection.
  " Validate file path for upload mode
  IF rb_upld = abap_true AND p_file IS INITIAL.
    MESSAGE e001(ygms_msg) WITH 'File path is required for upload'.
  ENDIF.

  " Validate email for send mode
  IF rb_send = abap_true AND p_email IS INITIAL.
    MESSAGE e014(ygms_msg) WITH 'Email address is required'.
  ENDIF.

  " Validate download path for download mode
  IF rb_dwnld = abap_true AND p_dwnfl IS INITIAL.
    MESSAGE e001(ygms_msg) WITH 'Download path is required'.
  ENDIF.

  " Validate date range
  IF p_frdat > p_todat.
    MESSAGE e009(ygms_msg) WITH 'From date' 'cannot be greater than To date'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_MAIN
*&---------------------------------------------------------------------*
*& Main processing logic
*&---------------------------------------------------------------------*
FORM process_main.
  " Initialize controller
  go_controller = NEW ygms_cl_cst_controller(
    iv_location_id = p_loc
    iv_from_date   = p_frdat
    iv_to_date     = p_todat
  ).

  TRY.
      CASE abap_true.
        WHEN rb_upld.
          " Upload mode
          PERFORM process_upload.

        WHEN rb_alloc.
          " Allocation mode
          PERFORM process_allocation.

        WHEN rb_view.
          " View mode
          PERFORM process_view.

        WHEN rb_send.
          " Send mode
          PERFORM process_send.

        WHEN rb_dwnld.
          " Download mode
          PERFORM process_download.

        WHEN rb_nom.
          " Nomination mode
          PERFORM process_nomination.

      ENDCASE.

    CATCH ygms_cx_cst_error INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_UPLOAD
*&---------------------------------------------------------------------*
*& Process file upload
*&---------------------------------------------------------------------*
FORM process_upload.
  go_controller->process_upload(
    EXPORTING iv_file_path = p_file
    IMPORTING et_messages  = gt_messages
  ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_ALLOCATION
*&---------------------------------------------------------------------*
*& Process state-wise allocation
*&---------------------------------------------------------------------*
FORM process_allocation.
  go_controller->execute_allocation(
    EXPORTING it_excluded_states = s_exst[]
    IMPORTING et_allocation      = gt_allocation
              et_messages        = gt_messages
  ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_VIEW
*&---------------------------------------------------------------------*
*& View existing data
*&---------------------------------------------------------------------*
FORM process_view.
  go_controller->ygms_if_cst_processor~process(
    EXPORTING iv_mode     = ygms_if_cst_constants=>co_mode_view
    IMPORTING et_messages = gt_messages
  ).

  gt_purchase = go_controller->ygms_if_cst_processor~get_results( ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_SEND
*&---------------------------------------------------------------------*
*& Send data via email
*&---------------------------------------------------------------------*
FORM process_send.
  go_controller->send_data(
    EXPORTING iv_email_address = p_email
    IMPORTING et_messages      = gt_messages
  ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_DOWNLOAD
*&---------------------------------------------------------------------*
*& Download data to file
*&---------------------------------------------------------------------*
FORM process_download.
  go_controller->download_data(
    EXPORTING iv_file_path = p_dwnfl
    IMPORTING et_messages  = gt_messages
  ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_NOMINATION
*&---------------------------------------------------------------------*
*& Display purchase nominations
*&---------------------------------------------------------------------*
FORM process_nomination.
  gt_purchase = go_controller->get_nominations( ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_RESULTS
*&---------------------------------------------------------------------*
*& Display processing results
*&---------------------------------------------------------------------*
FORM display_results.
  " Display messages
  PERFORM display_messages.

  " Display data based on mode
  go_alv_handler = NEW ygms_cl_cst_alv_handler( ).

  CASE abap_true.
    WHEN rb_alloc.
      " Display allocation in editable ALV
      IF gt_allocation IS NOT INITIAL.
        go_alv_handler->display_allocation( gt_allocation ).
      ENDIF.

    WHEN rb_view OR rb_nom.
      " Display purchase data in read-only ALV
      IF gt_purchase IS NOT INITIAL.
        go_alv_handler->display_purchase( gt_purchase ).
      ENDIF.

    WHEN OTHERS.
      " Just show messages for other modes

  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_MESSAGES
*&---------------------------------------------------------------------*
*& Display messages in list format
*&---------------------------------------------------------------------*
FORM display_messages.
  IF gt_messages IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT gt_messages INTO DATA(ls_message).
    " Build message text if not already built
    IF ls_message-message IS INITIAL.
      MESSAGE ID ls_message-id TYPE ls_message-type NUMBER ls_message-number
        WITH ls_message-message_v1 ls_message-message_v2
             ls_message-message_v3 ls_message-message_v4
        INTO ls_message-message.
    ENDIF.

    CASE ls_message-type.
      WHEN 'E'.
        WRITE: / icon_led_red AS ICON, ls_message-message.
      WHEN 'W'.
        WRITE: / icon_led_yellow AS ICON, ls_message-message.
      WHEN 'S' OR 'I'.
        WRITE: / icon_led_green AS ICON, ls_message-message.
    ENDCASE.
  ENDLOOP.

  SKIP 1.
ENDFORM.
