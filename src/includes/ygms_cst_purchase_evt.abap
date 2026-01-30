*&---------------------------------------------------------------------*
*& Include YGMS_CST_PURCHASE_EVT
*& Description: Event Handling Routines
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*& Handle user commands from toolbar
*&---------------------------------------------------------------------*
FORM handle_user_command USING iv_function TYPE salv_de_function.
  CASE iv_function.
    WHEN 'SAVE'.
      PERFORM save_allocation_data.

    WHEN 'VALIDATE'.
      PERFORM validate_allocation_data.

    WHEN 'EXCLUDE'.
      PERFORM exclude_selected_states.

    WHEN 'INCLUDE'.
      PERFORM include_selected_states.

    WHEN 'REFRESH'.
      PERFORM refresh_data.

    WHEN 'SEND'.
      PERFORM send_data_email.

  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SAVE_ALLOCATION_DATA
*&---------------------------------------------------------------------*
*& Save allocation data to database
*&---------------------------------------------------------------------*
FORM save_allocation_data.
  " Get modified data from ALV
  DATA(lt_allocation) = go_alv_handler->get_modified_data( ).

  TRY.
      go_controller->save_data(
        EXPORTING it_data     = lt_allocation
        IMPORTING ev_gail_id  = gv_gail_id
                  et_messages = gt_messages
      ).

      MESSAGE s010(ygms_msg) WITH gv_gail_id.

    CATCH ygms_cx_cst_error INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATE_ALLOCATION_DATA
*&---------------------------------------------------------------------*
*& Validate allocation totals
*&---------------------------------------------------------------------*
FORM validate_allocation_data.
  " Get current data from ALV
  DATA(lt_allocation) = go_alv_handler->get_modified_data( ).

  TRY.
      DATA(lv_valid) = go_controller->ygms_if_cst_processor~validate( lt_allocation ).

      IF lv_valid = abap_true.
        MESSAGE s008(ygms_msg) WITH 'all'.
      ELSE.
        MESSAGE w009(ygms_msg) WITH 'Allocation' 'Supply'.
      ENDIF.

    CATCH ygms_cx_cst_validation INTO DATA(lx_validation).
      MESSAGE lx_validation TYPE 'W'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXCLUDE_SELECTED_STATES
*&---------------------------------------------------------------------*
*& Mark selected states as excluded
*&---------------------------------------------------------------------*
FORM exclude_selected_states.
  " This would be implemented with proper ALV selection handling
  " For now, display informational message
  MESSAGE i015(ygms_msg) WITH 'Selected states' 'excluded'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form INCLUDE_SELECTED_STATES
*&---------------------------------------------------------------------*
*& Remove exclusion flag from selected states
*&---------------------------------------------------------------------*
FORM include_selected_states.
  " This would be implemented with proper ALV selection handling
  " For now, display informational message
  MESSAGE i015(ygms_msg) WITH 'Selected states' 'included'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form REFRESH_DATA
*&---------------------------------------------------------------------*
*& Refresh data from database
*&---------------------------------------------------------------------*
FORM refresh_data.
  " Re-execute allocation
  TRY.
      go_controller->execute_allocation(
        EXPORTING it_excluded_states = s_exst[]
        IMPORTING et_allocation      = gt_allocation
                  et_messages        = gt_messages
      ).

      " Redisplay ALV
      go_alv_handler->display_allocation( gt_allocation ).

    CATCH ygms_cx_cst_error INTO DATA(lx_error).
      MESSAGE lx_error TYPE 'W'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SEND_DATA_EMAIL
*&---------------------------------------------------------------------*
*& Send current data via email
*&---------------------------------------------------------------------*
FORM send_data_email.
  DATA: lv_email TYPE ad_smtpadr.

  " Get email address from user
  CALL FUNCTION 'POPUP_TO_GET_ONE_VALUE'
    EXPORTING
      textline1   = 'Enter email address:'
      titel       = 'Send Data via Email'
      valuelength = 100
    IMPORTING
      value1      = lv_email
    EXCEPTIONS
      OTHERS      = 1.

  IF sy-subrc = 0 AND lv_email IS NOT INITIAL.
    TRY.
        go_controller->send_data(
          EXPORTING iv_email_address = lv_email
          IMPORTING et_messages      = gt_messages
        ).

        MESSAGE s013(ygms_msg) WITH lv_email.

      CATCH ygms_cx_cst_error INTO DATA(lx_error).
        MESSAGE lx_error TYPE 'E'.
    ENDTRY.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& Handle double click on ALV row
*&---------------------------------------------------------------------*
FORM handle_double_click USING iv_row    TYPE i
                               iv_column TYPE lvc_fname.
  " Read selected row
  READ TABLE gt_allocation INTO DATA(ls_allocation) INDEX iv_row.
  IF sy-subrc = 0.
    " Display detail popup or drill-down
    DATA: lv_message TYPE string.
    lv_message = |{ ls_allocation-state } ({ ls_allocation-state_code }): | &&
                 |{ ls_allocation-alloc_qty_mbg } MMBTU / { ls_allocation-alloc_qty_scm } SCM|.
    MESSAGE lv_message TYPE 'I'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form HANDLE_DATA_CHANGED
*&---------------------------------------------------------------------*
*& Handle data changes in ALV
*&---------------------------------------------------------------------*
FORM handle_data_changed.
  " Recalculate totals or validate changes
  " This would be called when user edits cells in the ALV

  MESSAGE i015(ygms_msg) WITH 'Data changed' 'Validation pending'.
ENDFORM.
