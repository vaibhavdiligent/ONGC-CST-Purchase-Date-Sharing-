*&---------------------------------------------------------------------*
*& Class  ZCL_ZTABLE_INS_DPC  (Data Provider - code based, OData V2)
*&---------------------------------------------------------------------*
*& Runtime for the generic "insert into a customer table" service.
*&
*& CPI POSTs a target table + row(s) as JSON; the service writes them
*& and returns a per-request outcome:
*&
*&   POST /InsertSet
*&   { "Tabname":"ZFOO", "Mode":"MODIFY",
*&     "DataJson":"[ { ..row.. }, { ..row.. } ]" }
*&   -> { "Tabname":"ZFOO", "RowsProcessed":2, "Status":"S",
*&        "Message":"2 row(s) written with MODIFY." }
*&
*& DataJson uses the SAME shape the read service emits, so read-here /
*& insert-there is a pass-through. DataJson (and Tabname) may be
*& Base64URL-encoded with a 'B64:' prefix (CPI's OData adapter mangles
*& braces/quotes otherwise) - the same decode as the read service.
*&
*& SAFETY (hard rules, enforced below):
*&   * only CUSTOMER-NAMESPACE transparent tables: Z*, Y*, /CCBJI/*,
*&     /CCEJ/* (extend GC_ALLOWED_PREFIXES). Every standard SAP table
*&     is refused.
*&   * table must be a real transparent table (DD02L, tabclass TRANSP).
*&   * AUTHORITY-CHECK S_TABU_NAM ACTVT '02' (change) before any write.
*&   * one POST = one LUW: all rows commit together or roll back together.
*&   * TestRun = 'X' writes then ROLLBACKs (validation only).
*&
*& Default write mode is MODIFY (upsert) so a CPI message re-delivery is
*& idempotent (re-writing the same key does not dump).
*&
*& Fully code based -> extends /IWBEP/CL_MGW_ABS_DATA. The create flow
*& redefines /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY. Register with
*& /IWFND/MAINT_SERVICE together with ZCL_ZTABLE_INS_MPC.
*&---------------------------------------------------------------------*
CLASS zcl_ztable_ins_dpc DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_mgw_abs_data
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~create_entity REDEFINITION.

  PROTECTED SECTION.
    " Allowed customer-namespace prefixes. A target table name must start
    " with one of these (upper case) or the write is refused. Extend this
    " list to open further customer namespaces.
    CONSTANTS gc_max_rows_per_call TYPE i VALUE 50000.   " LUW size guard

    " Decode a value the caller Base64URL-encoded and prefixed 'B64:'.
    " (Identical logic to the read service.) Raw values pass through.
    METHODS decode_b64
      IMPORTING iv_value        TYPE string
      RETURNING VALUE(rv_value) TYPE string.

    " Enforce the customer-namespace rule + that the object is a real
    " transparent table. Raises a business exception otherwise.
    METHODS check_customer_table
      IMPORTING iv_tabname TYPE tabname
      RAISING   /iwbep/cx_mgw_busi_exception.

    " Display/change authorisation for the target table (ACTVT '02').
    METHODS check_write_authority
      IMPORTING iv_tabname TYPE tabname
      RAISING   /iwbep/cx_mgw_busi_exception.

    " Raise a business exception with a single message text.
    METHODS raise_error
      IMPORTING iv_text TYPE string
      RAISING   /iwbep/cx_mgw_busi_exception.
ENDCLASS.


CLASS zcl_ztable_ins_dpc IMPLEMENTATION.

  METHOD /iwbep/if_mgw_appl_srv_runtime~create_entity.

    DATA ls_req TYPE zcl_ztable_ins_mpc=>ty_insert_request.

    " Read the POSTed entity (Tabname / Mode / DataJson / TestRun).
    io_data_provider->read_entry_data( IMPORTING es_data = ls_req ).

    "--- resolve + normalise inputs ------------------------------------
    DATA(lv_tabname_str) = me->decode_b64( CONV string( ls_req-tabname ) ).
    TRANSLATE lv_tabname_str TO UPPER CASE.
    CONDENSE lv_tabname_str.
    IF lv_tabname_str IS INITIAL.
      me->raise_error( |Tabname is mandatory.| ).
    ENDIF.
    DATA lv_tabname TYPE tabname.
    lv_tabname = lv_tabname_str.

    DATA(lv_mode) = to_upper( CONV string( ls_req-mode ) ).
    CONDENSE lv_mode.
    IF lv_mode IS INITIAL.
      lv_mode = 'MODIFY'.                    " default: upsert (retry-safe)
    ENDIF.
    IF lv_mode <> 'INSERT' AND lv_mode <> 'MODIFY' AND lv_mode <> 'UPDATE'.
      me->raise_error( |Mode must be INSERT, MODIFY or UPDATE (got { lv_mode }).| ).
    ENDIF.

    DATA(lv_json) = me->decode_b64( ls_req-data_json ).
    IF lv_json IS INITIAL.
      me->raise_error( |DataJson is mandatory (one row object or a JSON array of rows).| ).
    ENDIF.

    "--- SAFETY GATES --------------------------------------------------
    me->check_customer_table( lv_tabname ).       " customer namespace + TRANSP
    me->check_write_authority( lv_tabname ).      " S_TABU_NAM ACTVT '02'

    "--- build a dynamic internal table of the target row type ---------
    DATA lr_tab TYPE REF TO data.
    FIELD-SYMBOLS <lt_rows> TYPE STANDARD TABLE.
    TRY.
        CREATE DATA lr_tab TYPE STANDARD TABLE OF (lv_tabname).
      CATCH cx_sy_create_data_error.
        me->raise_error( |Cannot build a work area for { lv_tabname }.| ).
    ENDTRY.
    ASSIGN lr_tab->* TO <lt_rows>.

    "--- deserialise DataJson into the typed table ---------------------
    " Accept both a single object { .. } and an array [ { .. }, .. ] by
    " wrapping a lone object in brackets. /ui2/cl_json maps JSON names to
    " field names case-insensitively (pretty_mode-none, like the reader).
    DATA(lv_payload) = lv_json.
    SHIFT lv_payload LEFT DELETING LEADING space.
    IF lv_payload(1) = '{'.
      lv_payload = |[{ lv_json }]|.
    ENDIF.
    TRY.
        /ui2/cl_json=>deserialize(
          EXPORTING json        = lv_payload
                    pretty_name = /ui2/cl_json=>pretty_mode-none
          CHANGING  data        = <lt_rows> ).
      CATCH cx_root INTO DATA(lx_json).
        me->raise_error( |DataJson is not valid JSON for { lv_tabname }: { lx_json->get_text( ) }| ).
    ENDTRY.

    DATA(lv_count) = lines( <lt_rows> ).
    IF lv_count = 0.
      me->raise_error( |DataJson contained no rows to write.| ).
    ENDIF.
    IF lv_count > gc_max_rows_per_call.
      me->raise_error( |Batch too large: { lv_count } rows (limit { gc_max_rows_per_call }). Split into smaller POSTs.| ).
    ENDIF.

    "--- write (one LUW, all-or-nothing) -------------------------------
    DATA lv_dbcnt TYPE i.
    TRY.
        CASE lv_mode.
          WHEN 'INSERT'.
            INSERT (lv_tabname) FROM TABLE <lt_rows>.
          WHEN 'MODIFY'.
            MODIFY (lv_tabname) FROM TABLE <lt_rows>.
          WHEN 'UPDATE'.
            UPDATE (lv_tabname) FROM TABLE <lt_rows>.
        ENDCASE.
        lv_dbcnt = sy-dbcnt.
      CATCH cx_sy_open_sql_db INTO DATA(lx_sql).
        ROLLBACK WORK.
        me->raise_error( |{ lv_mode } into { lv_tabname } failed (rolled back): { lx_sql->get_text( ) }| ).
    ENDTRY.

    IF sy-subrc <> 0.
      ROLLBACK WORK.
      me->raise_error( |{ lv_mode } into { lv_tabname } failed with subrc { sy-subrc } (rolled back). | &&
                       |For INSERT this is usually a duplicate key - consider Mode MODIFY.| ).
    ENDIF.

    "--- commit or (dry run) rollback ----------------------------------
    IF ls_req-test_run = 'X' OR ls_req-test_run = 'x'.
      ROLLBACK WORK.
      ls_req-status  = 'S'.
      ls_req-message = |TEST RUN: { lv_count } row(s) validated with { lv_mode }, nothing committed.|.
    ELSE.
      COMMIT WORK AND WAIT.
      IF sy-subrc <> 0.
        me->raise_error( |COMMIT failed for { lv_tabname }.| ).
      ENDIF.
      ls_req-status  = 'S'.
      ls_req-message = |{ lv_count } row(s) written into { lv_tabname } with { lv_mode }.|.
    ENDIF.

    "--- fill the response entity --------------------------------------
    ls_req-tabname        = lv_tabname.
    ls_req-mode           = lv_mode.
    ls_req-rows_processed = lv_count.
    ls_req-data_json      = ''.            " do not echo the (possibly large) payload

    copy_data_to_ref( EXPORTING is_data = ls_req
                      CHANGING  cr_data = er_entity ).
  ENDMETHOD.


  METHOD decode_b64.
    " 'B64:<base64url>' -> decoded text. Base64URL uses - and _ and no
    " padding; convert to standard Base64 and pad before decoding.
    rv_value = iv_value.
    IF iv_value NP 'B64:*'.
      RETURN.
    ENDIF.
    DATA(lv_enc) = substring( val = iv_value off = 4 ).
    REPLACE ALL OCCURRENCES OF '-' IN lv_enc WITH '+'.
    REPLACE ALL OCCURRENCES OF '_' IN lv_enc WITH '/'.
    DATA(lv_mod) = strlen( lv_enc ) MOD 4.
    IF lv_mod = 2.
      lv_enc = lv_enc && '=='.
    ELSEIF lv_mod = 3.
      lv_enc = lv_enc && '='.
    ENDIF.
    TRY.
        rv_value = cl_http_utility=>decode_base64( encoded = lv_enc ).
      CATCH cx_root.
        rv_value = iv_value.       " leave as-is if it is not valid Base64
    ENDTRY.
  ENDMETHOD.


  METHOD check_customer_table.
    " Rule 1: the name must be in a customer namespace.
    "   Z* / Y*        -> customer name range
    "   /XXX/*         -> a registered customer namespace; we allow the
    "                     project namespaces /CCBJI/ and /CCEJ/ explicitly.
    " Everything else (standard SAP tables, foreign namespaces) is refused.
    DATA(lv_name) = to_upper( CONV string( iv_tabname ) ).
    DATA lv_ok TYPE abap_bool.
    IF lv_name(1) = 'Z' OR lv_name(1) = 'Y'.
      lv_ok = abap_true.
    ELSEIF lv_name CP '/CCBJI/*' OR lv_name CP '/CCEJ/*'.
      lv_ok = abap_true.
    ENDIF.
    IF lv_ok = abap_false.
      me->raise_error( |Table { iv_tabname } is not a customer-namespace table. | &&
                       |Only Z*, Y*, /CCBJI/* and /CCEJ/* tables may be written.| ).
    ENDIF.

    " Rule 2: it must be a real transparent table (no views, no pool/cluster).
    SELECT SINGLE tabname FROM dd02l INTO @DATA(lv_dummy)
      WHERE tabname = @iv_tabname AND tabclass = 'TRANSP'.
    IF sy-subrc <> 0.
      me->raise_error( |{ iv_tabname } is not a transparent table (only transparent customer tables can be written).| ).
    ENDIF.
  ENDMETHOD.


  METHOD check_write_authority.
    AUTHORITY-CHECK OBJECT 'S_TABU_NAM'
      ID 'ACTVT' FIELD '02'
      ID 'TABLE' FIELD iv_tabname.
    IF sy-subrc <> 0.
      me->raise_error( |No change authorisation (S_TABU_NAM, ACTVT 02) for table { iv_tabname }.| ).
    ENDIF.
  ENDMETHOD.


  METHOD raise_error.
    " iv_msg_text is typed C(220) (BAPI_MSG) in the message container API,
    " so convert the string into a fixed-length field before passing it.
    DATA lv_msg TYPE bapi_msg.
    lv_msg = iv_text.
    DATA(lo_mc) = mo_context->get_message_container( ).
    lo_mc->add_message_text_only( iv_msg_type = 'E'
                                  iv_msg_text = lv_msg ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING message_container = lo_mc.
  ENDMETHOD.

ENDCLASS.
