*&---------------------------------------------------------------------*
*& Class  ZCL_YF01_EXCH_RATE_DPC   (Data Provider - code based, OData V2)
*&---------------------------------------------------------------------*
*& Service : YF01_EXCHANGE_RATE_SRV
*&
*& CPI performs ONE deep insert (POST) to ExchangeRatesSet with the nested
*& ExchangeRate collection. The whole batch lands in CREATE_DEEP_ENTITY and
*& is written to TCURR via BAPI_EXCHANGERATE_CREATEMULTIPLE (all-or-nothing).
*&
*& Mandatory fields (7) are validated before the BAPI call. VALID_FROM
*& arrives as DD.MM.YYYY and is converted to YYYYMMDD for the BAPI.
*&
*& Extends /IWBEP/CL_MGW_PUSH_ABS_DATA. Register together with
*& ZCL_YF01_EXCH_RATE_MPC.
*&---------------------------------------------------------------------*
CLASS zcl_yf01_exch_rate_dpc DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_mgw_push_abs_data
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~create_deep_entity REDEFINITION.
ENDCLASS.


CLASS zcl_yf01_exch_rate_dpc IMPLEMENTATION.

  METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.

    " Deep structure = header fields + table named after the nav property 'ExchangeRate'
    TYPES: BEGIN OF ty_deep.
             INCLUDE TYPE zcl_yf01_exch_rate_mpc=>ts_exchange_rates.
    TYPES:   exchangerate TYPE STANDARD TABLE OF zcl_yf01_exch_rate_mpc=>ts_exchange_rate
                            WITH DEFAULT KEY.
    TYPES: END OF ty_deep.

    DATA: ls_deep   TYPE ty_deep,
          lt_list   TYPE STANDARD TABLE OF bapi1093_0,
          ls_list   TYPE bapi1093_0,
          lt_return TYPE bapiret2_t,
          ls_return TYPE bapiret2,
          lv_valid  TYPE c LENGTH 8,
          lv_errors TYPE i.

    FIELD-SYMBOLS <ls_rate> TYPE zcl_yf01_exch_rate_mpc=>ts_exchange_rate.

    "--- 1. Read the deep payload posted by CPI
    io_data_provider->read_entry_data( IMPORTING es_data = ls_deep ).

    "--- 2. Validate MANDATORY fields + map each item into the BAPI table
    LOOP AT ls_deep-exchangerate ASSIGNING <ls_rate>.
      IF <ls_rate>-rate_type  IS INITIAL OR <ls_rate>-from_curr   IS INITIAL OR
         <ls_rate>-to_currncy IS INITIAL OR <ls_rate>-valid_from  IS INITIAL OR
         <ls_rate>-exch_rate  IS INITIAL OR <ls_rate>-from_factor IS INITIAL OR
         <ls_rate>-to_factor  IS INITIAL.
        lv_errors = lv_errors + 1.
        ls_return-type = 'E'. ls_return-id = 'YF01'. ls_return-number = '000'.
        ls_return-message = |Mandatory field missing for { <ls_rate>-from_curr }/{ <ls_rate>-to_currncy } { <ls_rate>-valid_from }|.
        APPEND ls_return TO lt_return.
        CONTINUE.
      ENDIF.

      CLEAR ls_list.
      MOVE-CORRESPONDING <ls_rate> TO ls_list.

      "--- Convert VALID_FROM DD.MM.YYYY -> YYYYMMDD for the BAPI
      IF <ls_rate>-valid_from CA '.'.
        lv_valid = <ls_rate>-valid_from+6(4)   " YYYY
                && <ls_rate>-valid_from+3(2)   " MM
                && <ls_rate>-valid_from+0(2).  " DD
      ELSE.
        lv_valid = <ls_rate>-valid_from.       " already YYYYMMDD
      ENDIF.
      ls_list-valid_from = lv_valid.

      APPEND ls_list TO lt_list.
    ENDLOOP.

    "--- 3. Post the whole batch in one BAPI call
    IF lv_errors = 0 AND lt_list IS NOT INITIAL.
      CALL FUNCTION 'BAPI_EXCHANGERATE_CREATEMULTIPLE'
        EXPORTING  upd_allowed    = abap_true
        TABLES     exch_rate_list = lt_list
                   return         = lt_return.
      LOOP AT lt_return INTO ls_return WHERE type CA 'EAX'.
        lv_errors = lv_errors + 1.
      ENDLOOP.
    ENDIF.

    "--- 4. Commit or roll back the whole batch (all-or-nothing)
    IF lv_errors > 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      DATA(lo_mc) = mo_context->get_message_container( ).
      lo_mc->add_messages_from_bapi( it_bapi_messages = lt_return ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING message_container = lo_mc.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = abap_true.
    ENDIF.

    "--- 5. Echo the header back to the caller
    DATA ls_head TYPE zcl_yf01_exch_rate_mpc=>ts_exchange_rates.
    MOVE-CORRESPONDING ls_deep TO ls_head.
    copy_data_to_ref( EXPORTING is_data = ls_head CHANGING cr_data = er_deep_entity ).

  ENDMETHOD.

ENDCLASS.
