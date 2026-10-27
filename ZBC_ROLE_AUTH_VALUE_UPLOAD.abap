*&---------------------------------------------------------------------*
*& Report  ZBC_ROLE_AUTH_VALUE_UPLOAD
*&---------------------------------------------------------------------*
*& Program Title : Mass change of authorization field values in roles
*&                 (PFCG) from an Excel upload
*&
*& Description   : The user uploads an Excel file with the columns
*&
*&                   A: Role name            (AGR_NAME,  e.g. ZFI_AP_CLERK)
*&                   B: Authorization object (OBJECT,    e.g. F_BKPF_BUK)
*&                   C: Field name           (FIELD,     e.g. BUKRS)
*&                   D: Value (from)         (LOW,       e.g. 1000 or *)
*&                   E: Value to (optional)  (HIGH,      only for ranges)
*&
*&                 Row 1 is a header row and is skipped. Several rows with
*&                 the same Role / Object / Field give several values for
*&                 that field.
*&
*&                 For every role the program reads the authorization data
*&                 of the role, changes the field value in EVERY active
*&                 authorization instance of the object in that role and
*&                 saves it back exactly the way PFCG does it:
*&
*&                   PRGN_ACTIVITY_GROUP_ENQUEUE   lock the role
*&                   PRGN_1251_READ_FIELD_VALUES   read AGR_1251 data
*&                   PRGN_1251_SAVE_FIELD_VALUES   write changed values
*&                   PRGN_UPDATE_DATABASE          post to DB + change docs
*&                   PRGN_ACTIVITY_GROUP_DEQUEUE   unlock the role
*&                   PRGN_AUTO_GENERATE_PROFILE_NEW (optional) generate
*&                                                 the authorization profile
*&
*& Modes         : Replace - existing values of the field are removed and
*&                           replaced by the values from the Excel file.
*&                 Add     - values from the Excel file are added to the
*&                           existing values (duplicates are ignored).
*&                 Test run (default) only shows what would be changed.
*&
*& Restrictions  : - Organizational level fields (value $BUKRS etc.) are
*&                   not changed - maintain them via org. levels in PFCG.
*&                 - The object must already exist in the role (manual
*&                   insertion of new objects is not done).
*&                 - For derived roles a warning is given: the values are
*&                   overwritten by the next inheritance from the master.
*&                 - Do a test run first and test in a sandbox/dev client.
*&---------------------------------------------------------------------*
REPORT zbc_role_auth_value_upload.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_excel,
         agr_name TYPE agr_name,
         object   TYPE xuobject,
         field    TYPE tprfield,
         low      TYPE xuval,
         high     TYPE xuval,
       END OF ty_excel.

TYPES: BEGIN OF ty_log,
         agr_name TYPE agr_name,
         object   TYPE xuobject,
         auth     TYPE xuauth,
         field    TYPE tprfield,
         old_val  TYPE char255,
         new_val  TYPE char255,
         status   TYPE char10,             " Success / Test / Warning / Error
         message  TYPE char220,
       END OF ty_log.

TYPES: ty_t_pt1251 TYPE STANDARD TABLE OF pt1251 WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Global data
*----------------------------------------------------------------------*
DATA: gt_excel TYPE STANDARD TABLE OF ty_excel,
      gt_log   TYPE STANDARD TABLE OF ty_log.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
PARAMETERS: p_repl RADIOBUTTON GROUP mode DEFAULT 'X',
            p_add  RADIOBUTTON GROUP mode.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
PARAMETERS: p_gen  AS CHECKBOX DEFAULT 'X',
            p_test AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* F4 help for the file
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  PERFORM read_excel.
  PERFORM process_roles.
  PERFORM display_log.

*&---------------------------------------------------------------------*
*& Form read_excel  (standard FM TEXT_CONVERT_XLS_TO_SAP)
*&---------------------------------------------------------------------*
FORM read_excel.
  DATA: lt_raw  TYPE truxs_t_text_data,
        lv_file TYPE rlgrap-filename.

  lv_file = p_file.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_field_seperator    = 'X'
      i_line_header        = 'X'      " skip the header row
      i_tab_raw_data       = lt_raw
      i_filename           = lv_file
    TABLES
      i_tab_converted_data = gt_excel
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Could not read the Excel file' TYPE 'E'.
  ENDIF.

* Normalise: upper case, no leading blanks, drop empty rows
  LOOP AT gt_excel ASSIGNING FIELD-SYMBOL(<ls_excel>).
    CONDENSE: <ls_excel>-agr_name NO-GAPS, <ls_excel>-object NO-GAPS,
              <ls_excel>-field NO-GAPS.
    SHIFT: <ls_excel>-low  LEFT DELETING LEADING space,
           <ls_excel>-high LEFT DELETING LEADING space.
    TRANSLATE: <ls_excel>-agr_name TO UPPER CASE,
               <ls_excel>-object   TO UPPER CASE,
               <ls_excel>-field    TO UPPER CASE.
  ENDLOOP.
  DELETE gt_excel WHERE agr_name IS INITIAL
                    AND object   IS INITIAL
                    AND field    IS INITIAL.

  IF gt_excel IS INITIAL.
    MESSAGE 'The Excel file contains no data rows' TYPE 'E'.
  ENDIF.

  SORT gt_excel BY agr_name object field low high.
  DELETE ADJACENT DUPLICATES FROM gt_excel COMPARING ALL FIELDS.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form process_roles
*&---------------------------------------------------------------------*
FORM process_roles.
  DATA: lt_role_rows TYPE STANDARD TABLE OF ty_excel,
        lt_values    TYPE ty_t_pt1251,
        lv_changed   TYPE abap_bool,
        lv_error     TYPE abap_bool,
        lv_locked    TYPE abap_bool.

  LOOP AT gt_excel INTO DATA(ls_excel)
       GROUP BY ls_excel-agr_name INTO DATA(lv_role).

    CLEAR: lt_role_rows, lt_values, lv_changed, lv_error, lv_locked.
    LOOP AT GROUP lv_role INTO DATA(ls_member).
      APPEND ls_member TO lt_role_rows.
    ENDLOOP.

*   --- Basic checks on the role -------------------------------------
    PERFORM check_role USING lv_role lt_role_rows CHANGING lv_error.
    IF lv_error = abap_true.
      CONTINUE.
    ENDIF.

*   --- Lock the role (same lock as PFCG) ----------------------------
    IF p_test IS INITIAL.
      CALL FUNCTION 'PRGN_ACTIVITY_GROUP_ENQUEUE'
        EXPORTING
          activity_group          = lv_role
        EXCEPTIONS
          foreign_lock            = 1
          transport_check_problem = 2
          OTHERS                  = 3.
      IF sy-subrc <> 0.
        PERFORM add_role_log USING lv_role lt_role_rows 'Error'
                'Role is locked by another user or transport check failed'.
        CONTINUE.
      ENDIF.
      lv_locked = abap_true.
    ENDIF.

*   --- Read current authorization field values of the role ----------
    CALL FUNCTION 'PRGN_CLEAR_BUFFER'.
    CALL FUNCTION 'PRGN_1251_READ_FIELD_VALUES'
      EXPORTING
        activity_group    = lv_role
      TABLES
        field_values      = lt_values
      EXCEPTIONS
        no_data_available = 1
        OTHERS            = 2.
    IF sy-subrc <> 0 OR lt_values IS INITIAL.
      PERFORM add_role_log USING lv_role lt_role_rows 'Error'
              'Role has no authorization data (maintain it once in PFCG)'.
      PERFORM unlock_role USING lv_role lv_locked.
      CONTINUE.
    ENDIF.

*   --- Apply every Object/Field group of this role ------------------
    LOOP AT lt_role_rows INTO DATA(ls_row)
         GROUP BY ( object = ls_row-object field = ls_row-field )
         INTO DATA(ls_key).
      DATA(lt_new) = VALUE ty_t_pt1251( ).
      LOOP AT GROUP ls_key INTO DATA(ls_val).
        APPEND VALUE #( low = ls_val-low high = ls_val-high ) TO lt_new.
      ENDLOOP.
      PERFORM change_field USING    lv_role ls_key-object ls_key-field
                                    lt_new
                           CHANGING lt_values lv_changed.
    ENDLOOP.

*   --- Save ---------------------------------------------------------
    IF p_test IS INITIAL AND lv_changed = abap_true.
      PERFORM save_role USING lv_role lt_values.
    ELSE.
      CALL FUNCTION 'PRGN_CLEAR_BUFFER'.
    ENDIF.

    PERFORM unlock_role USING lv_role lv_locked.

*   --- Generate the profile (after the role lock is released) -------
    IF p_test IS INITIAL AND lv_changed = abap_true AND p_gen = abap_true.
      PERFORM generate_profile USING lv_role.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form check_role
*&---------------------------------------------------------------------*
FORM check_role USING    iv_role TYPE agr_name
                         it_rows TYPE STANDARD TABLE
                CHANGING cv_error TYPE abap_bool.
  DATA: ls_tobj TYPE tobj,
        lv_ok   TYPE abap_bool.

  SELECT SINGLE agr_name, parent_agr FROM agr_define
    WHERE agr_name = @iv_role
    INTO @DATA(ls_define).
  IF sy-subrc <> 0.
    PERFORM add_role_log USING iv_role it_rows 'Error' 'Role does not exist'.
    cv_error = abap_true.
    RETURN.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'S_USER_AGR'
    ID 'ACT_GROUP' FIELD iv_role
    ID 'ACTVT'     FIELD '02'.
  IF sy-subrc <> 0.
    PERFORM add_role_log USING iv_role it_rows 'Error'
            'No authorization to change this role (S_USER_AGR / 02)'.
    cv_error = abap_true.
    RETURN.
  ENDIF.

  IF ls_define-parent_agr IS NOT INITIAL.
    APPEND VALUE #( agr_name = iv_role status = 'Warning'
                    message  = |Derived role of { ls_define-parent_agr }: | &&
                               |values are overwritten by next inheritance| )
           TO gt_log.
  ENDIF.

* Every row: value present, object exists, field belongs to the object
  LOOP AT it_rows ASSIGNING FIELD-SYMBOL(<ls_row>).
    ASSIGN COMPONENT 'OBJECT' OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_obj>).
    ASSIGN COMPONENT 'FIELD'  OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_fld>).
    ASSIGN COMPONENT 'LOW'    OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_low>).

    IF <lv_low> IS INITIAL.
      APPEND VALUE #( agr_name = iv_role object = <lv_obj> field = <lv_fld>
                      status = 'Error' message = 'Value is empty' ) TO gt_log.
      cv_error = abap_true.
      CONTINUE.
    ENDIF.

    SELECT SINGLE * FROM tobj WHERE objct = @<lv_obj> INTO @ls_tobj.
    IF sy-subrc <> 0.
      APPEND VALUE #( agr_name = iv_role object = <lv_obj> field = <lv_fld>
                      status = 'Error'
                      message = 'Authorization object does not exist' ) TO gt_log.
      cv_error = abap_true.
      CONTINUE.
    ENDIF.

    lv_ok = abap_false.
    DO 10 TIMES.
      DATA(lv_comp) = |FIEL{ sy-index }|.
      IF sy-index = 10.
        lv_comp = 'FIEL0'.
      ENDIF.
      ASSIGN COMPONENT lv_comp OF STRUCTURE ls_tobj TO FIELD-SYMBOL(<lv_tobj_fld>).
      IF sy-subrc = 0 AND <lv_tobj_fld> = <lv_fld>.
        lv_ok = abap_true.
        EXIT.
      ENDIF.
    ENDDO.
    IF lv_ok = abap_false.
      APPEND VALUE #( agr_name = iv_role object = <lv_obj> field = <lv_fld>
                      status = 'Error'
                      message = 'Field is not a field of this authorization object' )
             TO gt_log.
      cv_error = abap_true.
    ENDIF.
  ENDLOOP.

  IF cv_error = abap_true.
    APPEND VALUE #( agr_name = iv_role status = 'Error'
                    message = 'Role skipped - correct the Excel rows above' )
           TO gt_log.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form change_field
*&  Changes field IV_FIELD of every active instance of IV_OBJECT
*&---------------------------------------------------------------------*
FORM change_field USING    iv_role    TYPE agr_name
                           iv_object  TYPE xuobject
                           iv_field   TYPE tprfield
                           it_new     TYPE ty_t_pt1251
                  CHANGING ct_values  TYPE ty_t_pt1251
                           cv_changed TYPE abap_bool.
  DATA: lt_auths    TYPE STANDARD TABLE OF xuauth,
        lt_result   TYPE ty_t_pt1251,
        lv_old      TYPE string,
        lv_new      TYPE string,
        lv_modified TYPE tpr_st_mod,
        lv_org      TYPE abap_bool,
        lv_index    TYPE i.

* Active authorization instances of the object in the role
  LOOP AT ct_values INTO DATA(ls_value)
       WHERE object = iv_object AND deleted IS INITIAL.
    COLLECT ls_value-auth INTO lt_auths.
  ENDLOOP.
  IF lt_auths IS INITIAL.
    APPEND VALUE #( agr_name = iv_role object = iv_object field = iv_field
                    status = 'Error'
                    message = 'Authorization object is not active in the role' )
           TO gt_log.
    RETURN.
  ENDIF.

  LOOP AT lt_auths INTO DATA(lv_auth).
    CLEAR: lv_old, lv_new, lt_result, lv_org.

*   Template row: existing row of the field, else any row of the instance
    READ TABLE ct_values INTO DATA(ls_template)
         WITH KEY object = iv_object auth = lv_auth field = iv_field.
    IF sy-subrc <> 0.
      READ TABLE ct_values INTO ls_template
           WITH KEY object = iv_object auth = lv_auth.
      ls_template-field = iv_field.
      CLEAR: ls_template-low, ls_template-high.
    ENDIF.

*   Current values of the field in this instance
    LOOP AT ct_values INTO ls_value
         WHERE object = iv_object AND auth = lv_auth AND field = iv_field.
      lv_old = COND #( WHEN lv_old IS INITIAL THEN ls_value-low
                       ELSE |{ lv_old }, { ls_value-low }| ).
      IF ls_value-high IS NOT INITIAL.
        lv_old = |{ lv_old }-{ ls_value-high }|.
      ENDIF.
      IF ls_value-low(1) = '$'.
        APPEND VALUE #( agr_name = iv_role object = iv_object auth = lv_auth
                        field = iv_field old_val = ls_value-low status = 'Error'
                        message = 'Organizational level field - change it via org. levels in PFCG' )
               TO gt_log.
        lv_org = abap_true.
        EXIT.
      ENDIF.
      IF p_add = abap_true AND ls_value-low IS NOT INITIAL.
        APPEND ls_value TO lt_result.     " Add mode keeps existing values
      ENDIF.
    ENDLOOP.
    IF lv_org = abap_true.
      CONTINUE.     " org. level field - instance is skipped
    ENDIF.

*   Status of the changed field like PFCG: Standard -> Changed,
*   empty -> Maintained, otherwise keep (Manually / Changed / ...)
    lv_modified = ls_template-modified.
    IF ls_template-low IS INITIAL.
      lv_modified = 'M'.
    ELSEIF lv_modified = 'S' OR lv_modified IS INITIAL.
      lv_modified = 'U'.
    ENDIF.

    LOOP AT it_new INTO DATA(ls_new).
      IF line_exists( lt_result[ low = ls_new-low high = ls_new-high ] ).
        CONTINUE.
      ENDIF.
      ls_value          = ls_template.
      ls_value-low      = ls_new-low.
      ls_value-high     = ls_new-high.
      ls_value-modified = lv_modified.
      APPEND ls_value TO lt_result.
    ENDLOOP.

    LOOP AT lt_result INTO ls_value.
      lv_new = COND #( WHEN lv_new IS INITIAL THEN ls_value-low
                       ELSE |{ lv_new }, { ls_value-low }| ).
      IF ls_value-high IS NOT INITIAL.
        lv_new = |{ lv_new }-{ ls_value-high }|.
      ENDIF.
    ENDLOOP.

    IF lv_new = lv_old.
      APPEND VALUE #( agr_name = iv_role object = iv_object auth = lv_auth
                      field = iv_field old_val = lv_old new_val = lv_new
                      status = 'Warning' message = 'No change - value already set' )
             TO gt_log.
      CONTINUE.
    ENDIF.

*   Replace the rows of this field in this instance at the same position
*   (a new field is placed after the last row of the instance)
    lv_index = line_index( ct_values[ object = iv_object auth = lv_auth
                                      field = iv_field ] ).
    IF lv_index = 0.
      LOOP AT ct_values TRANSPORTING NO FIELDS
           WHERE object = iv_object AND auth = lv_auth.
        lv_index = sy-tabix + 1.
      ENDLOOP.
    ENDIF.
    DELETE ct_values WHERE object = iv_object AND auth = lv_auth
                       AND field = iv_field.
    INSERT LINES OF lt_result INTO ct_values INDEX lv_index.
    cv_changed = abap_true.

    APPEND VALUE #( agr_name = iv_role object = iv_object auth = lv_auth
                    field = iv_field old_val = lv_old new_val = lv_new
                    status  = COND #( WHEN p_test = abap_true THEN 'Test' ELSE 'Success' )
                    message = COND #( WHEN p_test = abap_true
                                      THEN 'Test run - would be changed'
                                      ELSE 'Value changed' ) )
           TO gt_log.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form save_role
*&---------------------------------------------------------------------*
FORM save_role USING iv_role   TYPE agr_name
                     it_values TYPE ty_t_pt1251.
  DATA lt_values TYPE ty_t_pt1251.

  lt_values = it_values.
  CALL FUNCTION 'PRGN_1251_SAVE_FIELD_VALUES'
    EXPORTING
      activity_group = iv_role
    TABLES
      field_values   = lt_values.

* Writes the buffered role data to the DB and creates change documents
  CALL FUNCTION 'PRGN_UPDATE_DATABASE'.
  COMMIT WORK AND WAIT.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form generate_profile
*&---------------------------------------------------------------------*
FORM generate_profile USING iv_role TYPE agr_name.
  DATA lv_msg TYPE char220.

  CALL FUNCTION 'PRGN_AUTO_GENERATE_PROFILE_NEW'
    EXPORTING
      activity_group              = iv_role
      no_dialog                   = 'X'
      rebuild_auth_data           = space   " keep the uploaded values
      fill_empty_fields_with_star = space   " never grant * automatically
    EXCEPTIONS
      activity_group_does_not_exist = 1
      activity_group_enqueued       = 2
      profile_name_exists           = 3
      profile_not_in_namespace      = 4
      no_auth_for_prof_creation     = 5
      no_auth_for_role_change       = 6
      no_auth_for_auth_maint        = 7
      no_auth_for_gen               = 8
      no_auths                      = 9
      open_auths                    = 10
      too_many_auths                = 11
      profgen_tables_not_updated    = 12
      error_when_generating_profile = 13
      OTHERS                        = 14.
  CASE sy-subrc.
    WHEN 0.
      COMMIT WORK AND WAIT.
      APPEND VALUE #( agr_name = iv_role status = 'Success'
                      message = 'Profile generated' ) TO gt_log.
      RETURN.
    WHEN 2.  lv_msg = 'Profile not generated: role is locked'.
    WHEN 5 OR 6 OR 7 OR 8.
             lv_msg = 'Profile not generated: no authorization for generation'.
    WHEN 10. lv_msg = 'Profile not generated: role has open (empty) fields'.
    WHEN 11. lv_msg = 'Profile not generated: too many authorizations'.
    WHEN OTHERS.
      lv_msg = |Profile not generated (return code { sy-subrc }) - generate in PFCG|.
  ENDCASE.
  ROLLBACK WORK.
  APPEND VALUE #( agr_name = iv_role status = 'Warning' message = lv_msg ) TO gt_log.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form unlock_role
*&---------------------------------------------------------------------*
FORM unlock_role USING iv_role   TYPE agr_name
                       iv_locked TYPE abap_bool.
  IF iv_locked = abap_true.
    CALL FUNCTION 'PRGN_ACTIVITY_GROUP_DEQUEUE'
      EXPORTING
        activity_group = iv_role.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form add_role_log  - one log line per Excel row of the role
*&---------------------------------------------------------------------*
FORM add_role_log USING iv_role   TYPE agr_name
                        it_rows   TYPE STANDARD TABLE
                        iv_status TYPE clike
                        iv_msg    TYPE clike.
  LOOP AT it_rows ASSIGNING FIELD-SYMBOL(<ls_row>).
    ASSIGN COMPONENT 'OBJECT' OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_obj>).
    ASSIGN COMPONENT 'FIELD'  OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_fld>).
    ASSIGN COMPONENT 'LOW'    OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<lv_low>).
    APPEND VALUE #( agr_name = iv_role object = <lv_obj> field = <lv_fld>
                    new_val = <lv_low> status = iv_status message = iv_msg )
           TO gt_log.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form display_log  (ALV)
*&---------------------------------------------------------------------*
FORM display_log.
  DATA lo_alv TYPE REF TO cl_salv_table.

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                              CHANGING  t_table      = gt_log ).
      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_columns( )->set_optimize( abap_true ).

      DATA(lo_cols) = lo_alv->get_columns( ).
      PERFORM set_col_text USING lo_cols 'AGR_NAME' 'Role'.
      PERFORM set_col_text USING lo_cols 'OBJECT'   'Auth. Object'.
      PERFORM set_col_text USING lo_cols 'AUTH'     'Authorization'.
      PERFORM set_col_text USING lo_cols 'FIELD'    'Field'.
      PERFORM set_col_text USING lo_cols 'OLD_VAL'  'Old Value(s)'.
      PERFORM set_col_text USING lo_cols 'NEW_VAL'  'New Value(s)'.
      PERFORM set_col_text USING lo_cols 'STATUS'   'Status'.
      PERFORM set_col_text USING lo_cols 'MESSAGE'  'Message'.

      lo_alv->get_display_settings( )->set_list_header(
        COND #( WHEN p_test = abap_true
                THEN 'Role authorization value upload - TEST RUN'
                ELSE 'Role authorization value upload - UPDATE RUN' ) ).
      lo_alv->display( ).
    CATCH cx_salv_msg cx_salv_not_found.
      MESSAGE 'Error displaying the log' TYPE 'I'.
  ENDTRY.
ENDFORM.

FORM set_col_text USING io_cols TYPE REF TO cl_salv_columns_table
                        iv_col  TYPE lvc_fname
                        iv_text TYPE clike
                  RAISING cx_salv_not_found.
  DATA(lo_col) = io_cols->get_column( iv_col ).
  lo_col->set_short_text( CONV #( iv_text ) ).
  lo_col->set_medium_text( CONV #( iv_text ) ).
  lo_col->set_long_text( CONV #( iv_text ) ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f4_file
*&---------------------------------------------------------------------*
FORM f4_file.
  DATA: lt_files TYPE filetable,
        lv_rc    TYPE i,
        lv_act   TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title = 'Select Excel file'
      file_filter  = 'Excel Files (*.xls;*.xlsx)|*.xls;*.xlsx|All Files (*.*)|*.*'
    CHANGING
      file_table   = lt_files
      rc           = lv_rc
      user_action  = lv_act
    EXCEPTIONS
      OTHERS       = 1 ).
  IF lv_act = cl_gui_frontend_services=>action_ok.
    READ TABLE lt_files INTO DATA(ls_file) INDEX 1.
    IF sy-subrc = 0.
      p_file = ls_file-filename.
    ENDIF.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* Text elements (create in SE38 -> Goto -> Text elements)
*   TEXT-B01  File
*   TEXT-B02  Change mode
*   TEXT-B03  Options
* Selection texts
*   P_FILE    Excel file
*   P_REPL    Replace existing values
*   P_ADD     Add to existing values
*   P_GEN     Generate profile after change
*   P_TEST    Test run (no update)
*----------------------------------------------------------------------*
