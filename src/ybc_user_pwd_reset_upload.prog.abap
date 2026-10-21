*&---------------------------------------------------------------------*
*& Report YBC_USER_PWD_RESET_UPLOAD
*&---------------------------------------------------------------------*
*& Mass password reset of SAP users from an Excel file.
*&
*& Excel layout (one user per row):
*&   Column A : User ID
*&   Column B : New Password
*&   Column C : Email ID
*&
*& For every valid row:
*&   1. Password is reset using BAPI_USER_CHANGE (PASSWORD/PASSWORDX)
*&   2. If the user is locked (USR02-UFLAG), it is unlocked using
*&      BAPI_USER_UNLOCK and the lock status is re-checked
*&   3. A mail is sent (CL_BCS) to the Email ID given in the Excel
*&
*& Safety:
*&   - Test run (default) only validates, nothing is changed
*&   - Program refuses to run in a client with role "Production"
*&   - Own user and SAP standard users (SAP*, DDIC, ...) are blocked
*&   - Confirmation popup before update run
*&---------------------------------------------------------------------*
REPORT ybc_user_pwd_reset_upload.

TYPE-POOLS: icon.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_item,
         row        TYPE i,
         bname      TYPE xubname,
         password   TYPE bapipwd-bapipwd,
         email      TYPE ad_smtpadr,
         uflag      TYPE xuuflag,
         status     TYPE icon_d,
         pwd_msg    TYPE bapi_msg,
         unlock_msg TYPE bapi_msg,
         mail_msg   TYPE bapi_msg,
         error      TYPE abap_bool,
       END OF ty_item,
       tt_item TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
  PARAMETERS: p_file   TYPE rlgrap-filename OBLIGATORY,
              p_header AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
  PARAMETERS: p_test   AS CHECKBOX DEFAULT 'X',
              p_prod   AS CHECKBOX,
              p_unlock AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
  PARAMETERS: p_mail   AS CHECKBOX DEFAULT 'X',
              p_inpwd  AS CHECKBOX DEFAULT 'X',
              p_sender TYPE ad_smtpadr.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* Application class
*----------------------------------------------------------------------*
CLASS lcl_app DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS f4_file.
    METHODS run.

  PRIVATE SECTION.
    CONSTANTS: c_max_rows TYPE i VALUE 9999.

    DATA mt_item TYPE tt_item.

    METHODS check_environment RETURNING VALUE(rv_ok) TYPE abap_bool.
    METHODS upload_file RETURNING VALUE(rv_ok) TYPE abap_bool.
    METHODS validate.
    METHODS confirm RETURNING VALUE(rv_ok) TYPE abap_bool.
    METHODS process.
    METHODS reset_password CHANGING cs_item TYPE ty_item.
    METHODS unlock_user CHANGING cs_item TYPE ty_item.
    METHODS get_lock_text
      IMPORTING iv_uflag       TYPE xuuflag
      RETURNING VALUE(rv_text) TYPE string.
    METHODS send_mail CHANGING cs_item TYPE ty_item.
    METHODS get_error_text
      IMPORTING it_return      TYPE bapiret2_t
      RETURNING VALUE(rv_text) TYPE bapi_msg.
    METHODS display.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.

  METHOD f4_file.
    DATA: lt_files  TYPE filetable,
          lv_rc     TYPE i,
          lv_action TYPE i.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = 'Select User Password Excel'
        file_filter             = 'Excel (*.xlsx;*.xls)|*.xlsx;*.xls'
      CHANGING
        file_table              = lt_files
        rc                      = lv_rc
        user_action             = lv_action
      EXCEPTIONS
        OTHERS                  = 1 ).
    IF sy-subrc = 0 AND lv_action = cl_gui_frontend_services=>action_ok.
      READ TABLE lt_files INTO DATA(ls_file) INDEX 1.
      IF sy-subrc = 0.
        p_file = ls_file-filename.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD run.
    IF check_environment( ) = abap_false.
      RETURN.
    ENDIF.

    IF upload_file( ) = abap_false.
      RETURN.
    ENDIF.

    validate( ).

    IF p_test = abap_false AND confirm( ) = abap_false.
      MESSAGE 'Processing cancelled by user' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    process( ).
    display( ).
  ENDMETHOD.

  METHOD check_environment.
    rv_ok = abap_false.

    " Excel upload needs SAP GUI frontend
    IF sy-batch = abap_true.
      MESSAGE 'Program must be run in foreground (Excel upload needs SAP GUI)' TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " Never allow mass password reset in a production client
    SELECT SINGLE cccategory FROM t000 INTO @DATA(lv_category)
      WHERE mandt = @sy-mandt.
    IF lv_category = 'P'.
      MESSAGE 'Mass password reset is not allowed in a production client' TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    rv_ok = abap_true.
  ENDMETHOD.

  METHOD upload_file.
    DATA: lt_cells     TYPE STANDARD TABLE OF alsmex_tabline,
          lv_begin_row TYPE i,
          lv_row       TYPE i.

    rv_ok = abap_false.
    lv_begin_row = COND #( WHEN p_header = abap_true THEN 2 ELSE 1 ).

    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_file
        i_begin_col             = 1
        i_begin_row             = lv_begin_row
        i_end_col               = 3
        i_end_row               = c_max_rows
      TABLES
        intern                  = lt_cells
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE 'Error reading Excel file. Check that file exists and is closed.' TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF lt_cells IS INITIAL.
      MESSAGE 'Excel file contains no data' TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    SORT lt_cells BY row col.

    LOOP AT lt_cells INTO DATA(ls_cell).
      " ALSM row is relative to I_BEGIN_ROW -> convert to actual Excel row
      lv_row = ls_cell-row + lv_begin_row - 1.

      READ TABLE mt_item ASSIGNING FIELD-SYMBOL(<ls_item>) WITH KEY row = lv_row.
      IF sy-subrc <> 0.
        APPEND VALUE #( row = lv_row ) TO mt_item ASSIGNING <ls_item>.
      ENDIF.

      CASE ls_cell-col.
        WHEN 1.
          <ls_item>-bname = to_upper( condense( ls_cell-value ) ).
        WHEN 2.
          " Password is taken as-is (no condense, case sensitive)
          IF strlen( ls_cell-value ) > 40.
            <ls_item>-error   = abap_true.
            <ls_item>-pwd_msg = 'Password longer than 40 characters'.
          ELSE.
            <ls_item>-password = ls_cell-value.
          ENDIF.
        WHEN 3.
          <ls_item>-email = condense( ls_cell-value ).
      ENDCASE.
    ENDLOOP.

    rv_ok = abap_true.
  ENDMETHOD.

  METHOD validate.
    DATA lt_seen TYPE SORTED TABLE OF xubname WITH UNIQUE KEY table_line.

    " Users that must never be reset by this program
    DATA(lt_blocked) = VALUE string_table( ( `SAP*` ) ( `DDIC` ) ( `TMSADM` )
                                           ( `EARLYWATCH` ) ( `SAPCPIC` )
                                           ( CONV string( sy-uname ) ) ).

    LOOP AT mt_item ASSIGNING FIELD-SYMBOL(<ls_item>).
      IF <ls_item>-error = abap_true.
        CONTINUE.
      ENDIF.

      IF <ls_item>-bname IS INITIAL.
        <ls_item>-error = abap_true.
        <ls_item>-pwd_msg = 'User ID is blank'.
      ELSEIF <ls_item>-password IS INITIAL.
        <ls_item>-error = abap_true.
        <ls_item>-pwd_msg = 'Password is blank'.
      ELSEIF <ls_item>-email IS INITIAL.
        <ls_item>-error = abap_true.
        <ls_item>-pwd_msg = 'Email ID is blank'.
      ELSEIF NOT matches( val   = <ls_item>-email
                          regex = `^[A-Za-z0-9._%+\-]+@[A-Za-z0-9.\-]+\.[A-Za-z]{2,}$` ).
        <ls_item>-error = abap_true.
        <ls_item>-pwd_msg = 'Email ID format is invalid'.
      ELSEIF line_exists( lt_blocked[ table_line = <ls_item>-bname ] ).
        <ls_item>-error = abap_true.
        <ls_item>-pwd_msg = 'User is protected / own user - not allowed'.
      ELSE.
        INSERT <ls_item>-bname INTO TABLE lt_seen.
        IF sy-subrc <> 0.
          <ls_item>-error = abap_true.
          <ls_item>-pwd_msg = 'Duplicate User ID in file'.
        ELSE.
          SELECT SINGLE uflag FROM usr02 INTO @<ls_item>-uflag
            WHERE bname = @<ls_item>-bname.
          IF sy-subrc <> 0.
            <ls_item>-error = abap_true.
            <ls_item>-pwd_msg = 'User does not exist in this client'.
          ELSEIF <ls_item>-uflag <> 0.
            <ls_item>-unlock_msg = |Locked ({ get_lock_text( <ls_item>-uflag ) })|
                                && COND #( WHEN p_unlock = abap_true THEN ` - will be unlocked` ).
          ELSE.
            <ls_item>-unlock_msg = 'Not locked'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD confirm.
    DATA lv_answer TYPE c LENGTH 1.

    DATA(lv_valid) = REDUCE i( INIT n = 0 FOR ls IN mt_item WHERE ( error = abap_false ) NEXT n = n + 1 ).
    DATA(lv_text) = |Password will be reset for { lv_valid } user(s) in client { sy-mandt }. Continue?|.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Mass Password Reset'
        text_question         = lv_text
        text_button_1         = 'Yes'
        text_button_2         = 'No'
        default_button        = '2'
        display_cancel_button = abap_false
      IMPORTING
        answer                = lv_answer
      EXCEPTIONS
        OTHERS                = 1.

    rv_ok = xsdbool( sy-subrc = 0 AND lv_answer = '1' ).
  ENDMETHOD.

  METHOD process.
    LOOP AT mt_item ASSIGNING FIELD-SYMBOL(<ls_item>).
      IF <ls_item>-error = abap_true.
        <ls_item>-status = icon_led_red.
        CONTINUE.
      ENDIF.

      IF p_test = abap_true.
        <ls_item>-status  = icon_led_yellow.
        <ls_item>-pwd_msg = 'Test run - validation OK, no change done'.
        CONTINUE.
      ENDIF.

      reset_password( CHANGING cs_item = <ls_item> ).
      IF <ls_item>-error = abap_true.
        <ls_item>-status = icon_led_red.
        CONTINUE.
      ENDIF.

      IF p_unlock = abap_true.
        unlock_user( CHANGING cs_item = <ls_item> ).
      ENDIF.

      IF p_mail = abap_true.
        send_mail( CHANGING cs_item = <ls_item> ).
      ENDIF.

      <ls_item>-status = COND #( WHEN <ls_item>-error = abap_true THEN icon_led_yellow
                                 ELSE icon_led_green ).
    ENDLOOP.
  ENDMETHOD.

  METHOD reset_password.
    DATA: ls_password  TYPE bapipwd,
          ls_passwordx TYPE bapipwdx,
          lt_return    TYPE bapiret2_t,
          lv_prod      TYPE c LENGTH 1,
          lt_ptab      TYPE abap_func_parmbind_tab,
          lt_etab      TYPE abap_func_excpbind_tab.

    ls_password-bapipwd  = cs_item-password.
    ls_passwordx-bapipwd = abap_true.

    lt_ptab = VALUE #(
      ( name = 'USERNAME'  kind = abap_func_exporting value = REF #( cs_item-bname ) )
      ( name = 'PASSWORD'  kind = abap_func_exporting value = REF #( ls_password ) )
      ( name = 'PASSWORDX' kind = abap_func_exporting value = REF #( ls_passwordx ) )
      ( name = 'RETURN'    kind = abap_func_tables    value = REF #( lt_return ) ) ).

    " PRODUCTIVE_PWD is only available with SAP Note 1287410 - pass it
    " only when requested so the call also works on older releases
    IF p_prod = abap_true.
      lv_prod = abap_true.
      INSERT VALUE #( name = 'PRODUCTIVE_PWD' kind = abap_func_exporting
                      value = REF #( lv_prod ) ) INTO TABLE lt_ptab.
    ENDIF.

    TRY.
        CALL FUNCTION 'BAPI_USER_CHANGE'
          PARAMETER-TABLE lt_ptab
          EXCEPTION-TABLE lt_etab.
      CATCH cx_sy_dyn_call_param_not_found.
        cs_item-error   = abap_true.
        cs_item-pwd_msg = 'PRODUCTIVE_PWD not supported (SAP Note 1287410 missing)'.
        RETURN.
    ENDTRY.

    DATA(lv_error) = get_error_text( lt_return ).
    IF lv_error IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      cs_item-error   = abap_true.
      cs_item-pwd_msg = lv_error.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    cs_item-pwd_msg = COND #( WHEN p_prod = abap_true THEN 'Password reset (productive)'
                              ELSE 'Password reset (initial - change at first logon)' ).
  ENDMETHOD.

  METHOD unlock_user.
    DATA lt_return TYPE bapiret2_t.

    IF cs_item-uflag = 0.
      cs_item-unlock_msg = 'Not locked'.
      RETURN.
    ENDIF.

    DATA(lv_lock_before) = get_lock_text( cs_item-uflag ).

    CALL FUNCTION 'BAPI_USER_UNLOCK'
      EXPORTING
        username = cs_item-bname
      TABLES
        return   = lt_return.

    DATA(lv_error) = get_error_text( lt_return ).
    IF lv_error IS NOT INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      cs_item-error      = abap_true.
      cs_item-unlock_msg = |Unlock failed ({ lv_lock_before }): { lv_error }|.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    " Re-read lock status to confirm the user is really unlocked
    SELECT SINGLE uflag FROM usr02 INTO @cs_item-uflag
      WHERE bname = @cs_item-bname.

    IF cs_item-uflag = 0.
      cs_item-unlock_msg = |Unlocked (was: { lv_lock_before })|.
    ELSE.
      cs_item-error      = abap_true.
      cs_item-unlock_msg = |Still locked: { get_lock_text( cs_item-uflag ) }|.
    ENDIF.
  ENDMETHOD.

  METHOD get_lock_text.
    " USR02-UFLAG is a bit mask: 32 = global (CUA) admin lock,
    " 64 = local admin lock, 128 = too many incorrect logons
    DATA(lt_text) = VALUE string_table(
      ( COND #( WHEN iv_uflag MOD 64 >= 32  THEN `global admin lock (CUA)` ) )
      ( COND #( WHEN iv_uflag MOD 128 >= 64 THEN `admin lock` ) )
      ( COND #( WHEN iv_uflag >= 128        THEN `incorrect logons` ) ) ).
    DELETE lt_text WHERE table_line IS INITIAL.
    rv_text = COND #( WHEN lt_text IS INITIAL THEN |lock code { iv_uflag }|
                      ELSE concat_lines_of( table = lt_text sep = `, ` ) ).
  ENDMETHOD.

  METHOD send_mail.
    DATA: lt_html TYPE string_table,
          lv_html TYPE string.

    DATA(lv_user) = escape( val = CONV string( cs_item-bname )    format = cl_abap_format=>e_html_text ).
    DATA(lv_pwd)  = escape( val = CONV string( cs_item-password ) format = cl_abap_format=>e_html_text ).

    APPEND `<html><body style="font-family:Arial,sans-serif;font-size:10pt">` TO lt_html.
    APPEND `<p>Dear User,</p>` TO lt_html.
    APPEND |<p>Your password for SAP system <b>{ sy-sysid }</b> client <b>{ sy-mandt }</b> has been reset.</p>| TO lt_html.
    APPEND `<table border="1" cellpadding="4" style="border-collapse:collapse">` TO lt_html.
    APPEND |<tr><td><b>System</b></td><td>{ sy-sysid }</td></tr>| TO lt_html.
    APPEND |<tr><td><b>Client</b></td><td>{ sy-mandt }</td></tr>| TO lt_html.
    APPEND |<tr><td><b>User ID</b></td><td>{ lv_user }</td></tr>| TO lt_html.
    IF p_inpwd = abap_true.
      APPEND |<tr><td><b>Password</b></td><td>{ lv_pwd }</td></tr>| TO lt_html.
    ENDIF.
    APPEND `</table>` TO lt_html.
    IF p_prod = abap_false.
      APPEND `<p>You will be asked to change this password at your first logon.</p>` TO lt_html.
    ENDIF.
    APPEND `<p>Please do not share your password with anyone.</p>` TO lt_html.
    APPEND `<p>This is an auto-generated mail. Please do not reply.</p>` TO lt_html.
    APPEND `<p>Regards,<br>SAP Basis Team</p>` TO lt_html.
    APPEND `</body></html>` TO lt_html.

    CONCATENATE LINES OF lt_html INTO lv_html.

    TRY.
        DATA(lo_request) = cl_bcs=>create_persistent( ).

        DATA(lv_subject) = |SAP { sy-sysid } Client { sy-mandt } - Password Reset for User { cs_item-bname }|.

        DATA(lo_document) = cl_document_bcs=>create_document(
          i_type    = 'HTM'
          i_text    = cl_document_bcs=>string_to_soli( lv_html )
          i_subject = CONV so_obj_des( lv_subject ) ).
        lo_request->set_document( lo_document ).
        lo_request->set_message_subject( ip_subject = lv_subject ).

        IF p_sender IS NOT INITIAL.
          lo_request->set_sender( cl_cam_address_bcs=>create_internet_address( p_sender ) ).
        ELSE.
          lo_request->set_sender( cl_sapuser_bcs=>create( sy-uname ) ).
        ENDIF.

        lo_request->add_recipient( cl_cam_address_bcs=>create_internet_address( cs_item-email ) ).
        lo_request->set_send_immediately( abap_true ).

        IF lo_request->send( ) = abap_true.
          COMMIT WORK.
          cs_item-mail_msg = |Mail sent to { cs_item-email }|.
        ELSE.
          ROLLBACK WORK.
          cs_item-error    = abap_true.
          cs_item-mail_msg = 'Mail could not be sent'.
        ENDIF.

      CATCH cx_bcs INTO DATA(lx_bcs).
        ROLLBACK WORK.
        cs_item-error    = abap_true.
        cs_item-mail_msg = |Mail error: { lx_bcs->get_text( ) }|.
    ENDTRY.
  ENDMETHOD.

  METHOD get_error_text.
    LOOP AT it_return INTO DATA(ls_return) WHERE type CA 'EAX'.
      rv_text = ls_return-message.
      RETURN.
    ENDLOOP.
  ENDMETHOD.

  METHOD display.
    DATA: lo_alv TYPE REF TO cl_salv_table.

    DATA(lv_ok)  = REDUCE i( INIT n = 0 FOR ls IN mt_item WHERE ( status = icon_led_green )  NEXT n = n + 1 ).
    DATA(lv_wrn) = REDUCE i( INIT n = 0 FOR ls IN mt_item WHERE ( status = icon_led_yellow ) NEXT n = n + 1 ).
    DATA(lv_err) = REDUCE i( INIT n = 0 FOR ls IN mt_item WHERE ( status = icon_led_red )    NEXT n = n + 1 ).

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING  t_table      = mt_item ).

        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
        lo_alv->get_display_settings( )->set_list_header(
          CONV #( |{ COND string( WHEN p_test = abap_true THEN `TEST RUN - ` ) }Password Reset: |
               && |{ lv_ok } OK, { lv_wrn } Warning, { lv_err } Error| ) ).

        DATA(lo_columns) = lo_alv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        " Never show the password in the output
        lo_columns->get_column( 'PASSWORD' )->set_technical( abap_true ).
        lo_columns->get_column( 'ERROR' )->set_technical( abap_true ).
        lo_columns->get_column( 'UFLAG' )->set_technical( abap_true ).

        DATA(lo_col) = lo_columns->get_column( 'ROW' ).
        lo_col->set_long_text( 'Excel Row' ).
        lo_col->set_medium_text( 'Excel Row' ).
        lo_col->set_short_text( 'Row' ).

        lo_col = lo_columns->get_column( 'STATUS' ).
        CAST cl_salv_column_table( lo_col )->set_icon( abap_true ).
        lo_col->set_long_text( 'Status' ).
        lo_col->set_medium_text( 'Status' ).
        lo_col->set_short_text( 'Status' ).

        lo_col = lo_columns->get_column( 'PWD_MSG' ).
        lo_col->set_long_text( 'Password Reset Message' ).
        lo_col->set_medium_text( 'Password Message' ).
        lo_col->set_short_text( 'Pwd Msg' ).

        lo_col = lo_columns->get_column( 'UNLOCK_MSG' ).
        lo_col->set_long_text( 'Unlock Message' ).
        lo_col->set_medium_text( 'Unlock Message' ).
        lo_col->set_short_text( 'Unlock' ).

        lo_col = lo_columns->get_column( 'MAIL_MSG' ).
        lo_col->set_long_text( 'Mail Message' ).
        lo_col->set_medium_text( 'Mail Message' ).
        lo_col->set_short_text( 'Mail' ).

        lo_alv->display( ).

      CATCH cx_salv_msg cx_salv_not_found INTO DATA(lx_salv).
        MESSAGE lx_salv->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Events
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_app=>f4_file( ).

START-OF-SELECTION.
  NEW lcl_app( )->run( ).
