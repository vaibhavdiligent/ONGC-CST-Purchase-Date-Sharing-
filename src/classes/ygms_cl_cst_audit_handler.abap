*&---------------------------------------------------------------------*
*& Class: YGMS_CL_CST_AUDIT_HANDLER
*& Package: YGMS
*& Description: Audit Log Management Class (NEW in v1.2)
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

CLASS ygms_cl_cst_audit_handler DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    METHODS constructor.

    "! <p class="shorttext synchronized" lang="en">Log insert operation</p>
    "! @parameter iv_table_name | Table being changed
    "! @parameter is_new_data | New data being inserted
    "! @parameter iv_key1 | Primary key value 1
    "! @parameter iv_key2 | Primary key value 2
    "! @parameter iv_key3 | Primary key value 3
    METHODS log_insert
      IMPORTING
        iv_table_name TYPE tabname
        is_new_data   TYPE any
        iv_key1       TYPE string OPTIONAL
        iv_key2       TYPE string OPTIONAL
        iv_key3       TYPE string OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Log update operation</p>
    "! @parameter iv_table_name | Table being changed
    "! @parameter is_old_data | Old data before change
    "! @parameter is_new_data | New data after change
    "! @parameter iv_key1 | Primary key value 1
    "! @parameter iv_key2 | Primary key value 2
    "! @parameter iv_key3 | Primary key value 3
    METHODS log_update
      IMPORTING
        iv_table_name TYPE tabname
        is_old_data   TYPE any
        is_new_data   TYPE any
        iv_key1       TYPE string OPTIONAL
        iv_key2       TYPE string OPTIONAL
        iv_key3       TYPE string OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Log delete operation</p>
    "! @parameter iv_table_name | Table being changed
    "! @parameter is_old_data | Data being deleted
    "! @parameter iv_key1 | Primary key value 1
    "! @parameter iv_key2 | Primary key value 2
    "! @parameter iv_key3 | Primary key value 3
    METHODS log_delete
      IMPORTING
        iv_table_name TYPE tabname
        is_old_data   TYPE any
        iv_key1       TYPE string OPTIONAL
        iv_key2       TYPE string OPTIONAL
        iv_key3       TYPE string OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Get audit log entries</p>
    "! @parameter iv_table_name | Filter by table name (optional)
    "! @parameter iv_from_date | Filter by start date (optional)
    "! @parameter iv_to_date | Filter by end date (optional)
    "! @parameter rt_log | Audit log entries
    METHODS get_audit_log
      IMPORTING
        iv_table_name TYPE tabname OPTIONAL
        iv_from_date  TYPE datum OPTIONAL
        iv_to_date    TYPE datum OPTIONAL
      RETURNING
        VALUE(rt_log) TYPE ygms_tt_audit_log.

    "! <p class="shorttext synchronized" lang="en">Display audit log in ALV</p>
    "! @parameter iv_table_name | Filter by table name (optional)
    "! @parameter iv_from_date | Filter by start date (optional)
    "! @parameter iv_to_date | Filter by end date (optional)
    METHODS display_audit_log
      IMPORTING
        iv_table_name TYPE tabname OPTIONAL
        iv_from_date  TYPE datum OPTIONAL
        iv_to_date    TYPE datum OPTIONAL.

  PRIVATE SECTION.
    DATA: mt_log_buffer TYPE ygms_tt_audit_log.

    "! <p class="shorttext synchronized" lang="en">Generate unique log ID</p>
    METHODS generate_log_id
      RETURNING
        VALUE(rv_log_id) TYPE ygms_log_id.

    "! <p class="shorttext synchronized" lang="en">Save log entry to database</p>
    METHODS save_log_entry
      IMPORTING
        is_log_entry TYPE ygms_s_audit.

    "! <p class="shorttext synchronized" lang="en">Convert any value to string</p>
    METHODS value_to_string
      IMPORTING
        iv_value         TYPE any
      RETURNING
        VALUE(rv_string) TYPE char255.

ENDCLASS.


CLASS ygms_cl_cst_audit_handler IMPLEMENTATION.

  METHOD constructor.
    " Initialize log buffer
    CLEAR mt_log_buffer.
  ENDMETHOD.


  METHOD log_insert.
    DATA(ls_log) = VALUE ygms_s_audit(
      log_id      = generate_log_id( )
      table_name  = iv_table_name
      operation   = ygms_if_cst_constants=>co_audit_insert
      key_field1  = iv_key1
      key_field2  = iv_key2
      key_field3  = iv_key3
      changed_by  = sy-uname
      changed_on  = sy-datum
      changed_at  = sy-uzeit
    ).

    save_log_entry( ls_log ).
  ENDMETHOD.


  METHOD log_update.
    " Compare old and new data and log field-level changes
    DATA: lo_old_descr TYPE REF TO cl_abap_structdescr.

    lo_old_descr ?= cl_abap_typedescr=>describe_by_data( is_old_data ).

    LOOP AT lo_old_descr->components INTO DATA(ls_component).
      " Skip audit fields themselves
      IF ls_component-name CP 'CREATED*' OR
         ls_component-name CP 'CHANGED*' OR
         ls_component-name = 'MANDT'.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT ls_component-name OF STRUCTURE is_old_data TO FIELD-SYMBOL(<fs_old>).
      ASSIGN COMPONENT ls_component-name OF STRUCTURE is_new_data TO FIELD-SYMBOL(<fs_new>).

      IF <fs_old> IS ASSIGNED AND <fs_new> IS ASSIGNED.
        IF <fs_old> <> <fs_new>.
          DATA(ls_log) = VALUE ygms_s_audit(
            log_id      = generate_log_id( )
            table_name  = iv_table_name
            operation   = ygms_if_cst_constants=>co_audit_update
            key_field1  = iv_key1
            key_field2  = iv_key2
            key_field3  = iv_key3
            field_name  = ls_component-name
            old_value   = value_to_string( <fs_old> )
            new_value   = value_to_string( <fs_new> )
            changed_by  = sy-uname
            changed_on  = sy-datum
            changed_at  = sy-uzeit
          ).

          save_log_entry( ls_log ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD log_delete.
    DATA(ls_log) = VALUE ygms_s_audit(
      log_id      = generate_log_id( )
      table_name  = iv_table_name
      operation   = ygms_if_cst_constants=>co_audit_delete
      key_field1  = iv_key1
      key_field2  = iv_key2
      key_field3  = iv_key3
      changed_by  = sy-uname
      changed_on  = sy-datum
      changed_at  = sy-uzeit
    ).

    save_log_entry( ls_log ).
  ENDMETHOD.


  METHOD get_audit_log.
    SELECT *
      FROM ygms_cst_audit_log
      WHERE ( @iv_table_name IS INITIAL OR table_name = @iv_table_name )
        AND ( @iv_from_date IS INITIAL OR changed_on >= @iv_from_date )
        AND ( @iv_to_date IS INITIAL OR changed_on <= @iv_to_date )
      ORDER BY changed_on DESCENDING, changed_at DESCENDING
      INTO TABLE @rt_log.
  ENDMETHOD.


  METHOD display_audit_log.
    " Get audit log data
    DATA(lt_log) = get_audit_log(
      iv_table_name = iv_table_name
      iv_from_date  = iv_from_date
      iv_to_date    = iv_to_date
    ).

    " Display using SALV
    TRY.
        DATA: lo_alv TYPE REF TO cl_salv_table.

        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = lt_log
        ).

        " Set up display options
        DATA(lo_display) = lo_alv->get_display_settings( ).
        lo_display->set_striped_pattern( abap_true ).

        " Set up column headings
        DATA(lo_columns) = lo_alv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        " Set title
        lo_alv->get_functions( )->set_all( abap_true ).

        " Display ALV
        lo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv).
        MESSAGE lx_salv TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD generate_log_id.
    " Generate GUID for log entry
    TRY.
        rv_log_id = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
        " Fallback to timestamp-based ID
        rv_log_id = |{ sy-datum }{ sy-uzeit }{ sy-uname(8) }|.
    ENDTRY.
  ENDMETHOD.


  METHOD save_log_entry.
    DATA(ls_db_entry) = VALUE ygms_cst_audit_log(
      mandt      = sy-mandt
      log_id     = is_log_entry-log_id
      table_name = is_log_entry-table_name
      operation  = is_log_entry-operation
      key_field1 = is_log_entry-key_field1
      key_field2 = is_log_entry-key_field2
      key_field3 = is_log_entry-key_field3
      field_name = is_log_entry-field_name
      old_value  = is_log_entry-old_value
      new_value  = is_log_entry-new_value
      changed_by = is_log_entry-changed_by
      changed_on = is_log_entry-changed_on
      changed_at = is_log_entry-changed_at
    ).

    INSERT ygms_cst_audit_log FROM @ls_db_entry.
  ENDMETHOD.


  METHOD value_to_string.
    " Convert any value to string representation
    TRY.
        rv_string = |{ iv_value }|.
      CATCH cx_root.
        rv_string = ''.
    ENDTRY.

    " Truncate if too long
    IF strlen( rv_string ) > 255.
      rv_string = rv_string(255).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
