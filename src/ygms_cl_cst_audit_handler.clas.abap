CLASS ygms_cl_cst_audit_handler DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Log insert operation</p>
    METHODS log_insert
      IMPORTING
        iv_table_name TYPE tabname
        iv_key1       TYPE clike OPTIONAL
        iv_key2       TYPE clike OPTIONAL
        iv_key3       TYPE clike OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Log update operation</p>
    METHODS log_update
      IMPORTING
        iv_table_name TYPE tabname
        iv_key1       TYPE clike OPTIONAL
        iv_field_name TYPE fieldname OPTIONAL
        iv_old_value  TYPE clike OPTIONAL
        iv_new_value  TYPE clike OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Log delete operation</p>
    METHODS log_delete
      IMPORTING
        iv_table_name TYPE tabname
        iv_key1       TYPE clike OPTIONAL
        iv_key2       TYPE clike OPTIONAL.

ENDCLASS.


CLASS ygms_cl_cst_audit_handler IMPLEMENTATION.

  METHOD log_insert.
    DATA: ls_audit TYPE ygms_cst_audit_log.

    ls_audit-mandt      = sy-mandt.
    TRY.
        ls_audit-log_id = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error.
        ls_audit-log_id = |{ sy-datum }{ sy-uzeit }{ sy-uname }|.
    ENDTRY.
    ls_audit-table_name = iv_table_name.
    ls_audit-operation  = ygms_if_cst_constants=>gc_operation-insert.
    ls_audit-key_field1 = iv_key1.
    ls_audit-key_field2 = iv_key2.
    ls_audit-key_field3 = iv_key3.
    ls_audit-changed_by = sy-uname.
    ls_audit-changed_on = sy-datum.
    ls_audit-changed_at = sy-uzeit.

    INSERT ygms_cst_audit_log FROM ls_audit.
  ENDMETHOD.


  METHOD log_update.
    DATA: ls_audit TYPE ygms_cst_audit_log.

    ls_audit-mandt      = sy-mandt.
    TRY.
        ls_audit-log_id = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error.
        ls_audit-log_id = |{ sy-datum }{ sy-uzeit }{ sy-uname }|.
    ENDTRY.
    ls_audit-table_name = iv_table_name.
    ls_audit-operation  = ygms_if_cst_constants=>gc_operation-update.
    ls_audit-key_field1 = iv_key1.
    ls_audit-field_name = iv_field_name.
    ls_audit-old_value  = iv_old_value.
    ls_audit-new_value  = iv_new_value.
    ls_audit-changed_by = sy-uname.
    ls_audit-changed_on = sy-datum.
    ls_audit-changed_at = sy-uzeit.

    INSERT ygms_cst_audit_log FROM ls_audit.
  ENDMETHOD.


  METHOD log_delete.
    DATA: ls_audit TYPE ygms_cst_audit_log.

    ls_audit-mandt      = sy-mandt.
    TRY.
        ls_audit-log_id = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error.
        ls_audit-log_id = |{ sy-datum }{ sy-uzeit }{ sy-uname }|.
    ENDTRY.
    ls_audit-table_name = iv_table_name.
    ls_audit-operation  = ygms_if_cst_constants=>gc_operation-delete.
    ls_audit-key_field1 = iv_key1.
    ls_audit-key_field2 = iv_key2.
    ls_audit-changed_by = sy-uname.
    ls_audit-changed_on = sy-datum.
    ls_audit-changed_at = sy-uzeit.

    INSERT ygms_cst_audit_log FROM ls_audit.
  ENDMETHOD.

ENDCLASS.
