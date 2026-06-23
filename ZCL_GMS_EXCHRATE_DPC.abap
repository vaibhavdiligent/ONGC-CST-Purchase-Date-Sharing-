*&---------------------------------------------------------------------*
*& Class  ZCL_GMS_EXCHRATE_DPC  (Data Provider - code based, OData V2)
*&---------------------------------------------------------------------*
*& Runtime for the inbound Exchange Rate upload service called by CPI.
*&
*& CPI performs ONE deep insert (POST) to the header entity set and
*& embeds the ExchangeRate collection via the navigation property:
*&
*&   POST /sap/opu/odata/sap/ZGMS_EXCHRATE_SRV/ExchangeRatesSet
*&   {
*&     "REQUEST_ID": "",
*&     "ExchangeRate": [
*&       { "RATE_TYPE":"M","FROM_CURR":"USD","TO_CURRNCY":"INR",
*&         "VALID_FROM":"20260623","EXCH_RATE":"83.25",
*&         "FROM_FACTOR":"1","TO_FACTOR":"1" },
*&       ...
*&     ]
*&   }
*&
*& The whole batch lands in ONE call in CREATE_DEEP_ENTITY and is written
*& to TCURR with BAPI_EXCHANGERATE_CREATEMULTIPLE in a single call
*& (all-or-nothing: any error rolls the whole batch back).
*&
*& The ABAP item field names match BAPI1093_0 1:1, so MOVE-CORRESPONDING
*& fills the BAPI table directly.
*&
*& Extends /IWBEP/CL_MGW_PUSH_ABS_DATA. Register together with
*& ZCL_GMS_EXCHRATE_MPC via /IWFND/MAINT_SERVICE (service
*& ZGMS_EXCHRATE_SRV) and activate.
*&---------------------------------------------------------------------*
CLASS zcl_gms_exchrate_dpc DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_mgw_push_abs_data
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~create_deep_entity REDEFINITION.

  PROTECTED SECTION.
    "! Mandatory-field check for one item. Returns an E message or empty.
    METHODS validate_rate
      IMPORTING
        is_rate          TYPE zcl_gms_exchrate_mpc=>ty_exchange_rate
      RETURNING
        VALUE(rs_return) TYPE bapiret2.
ENDCLASS.


CLASS zcl_gms_exchrate_dpc IMPLEMENTATION.

  METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.

    DATA: ls_deep    TYPE zcl_gms_exchrate_mpc=>ty_deep,
          lt_list    TYPE STANDARD TABLE OF bapi1093_0,   " BAPI input table
          ls_list    TYPE bapi1093_0,
          lt_return  TYPE bapiret2_t,
          ls_return  TYPE bapiret2,
          lv_errors  TYPE i.

    FIELD-SYMBOLS <ls_rate> TYPE zcl_gms_exchrate_mpc=>ty_exchange_rate.

    "--- 1. Read the deep payload posted by CPI ------------------------
    io_data_provider->read_entry_data( IMPORTING es_data = ls_deep ).

    "--- 2. Generate a request id if CPI did not send one --------------
    IF ls_deep-request_id IS INITIAL.
      TRY.
          ls_deep-request_id = cl_system_uuid=>create_uuid_c32_static( ).
        CATCH cx_uuid_error.
          GET TIME STAMP FIELD DATA(lv_ts).
          ls_deep-request_id = lv_ts.
      ENDTRY.
    ENDIF.

    "--- 3. Validate + map items into the BAPI table -------------------
    "    Field names of TY_EXCHANGE_RATE match BAPI1093_0 1:1, so a
    "    MOVE-CORRESPONDING transfers every field (RATE_TYPE, FROM_CURR,
    "    TO_CURRNCY, VALID_FROM, EXCH_RATE, FROM_FACTOR, TO_FACTOR and
    "    the optional *_V fields).
    "    Note: VALID_FROM is expected as YYYYMMDD from CPI.
    LOOP AT ls_deep-exchangerate ASSIGNING <ls_rate>.
      ls_return = me->validate_rate( <ls_rate> ).
      IF ls_return IS NOT INITIAL.
        APPEND ls_return TO lt_return.
        lv_errors = lv_errors + 1.
        CONTINUE.
      ENDIF.

      CLEAR ls_list.
      MOVE-CORRESPONDING <ls_rate> TO ls_list.
      APPEND ls_list TO lt_list.
    ENDLOOP.

    "--- 4. Post the whole batch in one BAPI call ----------------------
    IF lv_errors = 0 AND lt_list IS NOT INITIAL.
      CALL FUNCTION 'BAPI_EXCHANGERATE_CREATEMULTIPLE'
        EXPORTING
          upd_allowed    = abap_true     " update existing TCURR entries too
        TABLES
          exch_rate_list = lt_list
          return         = lt_return.

      LOOP AT lt_return INTO ls_return WHERE type CA 'EAX'.
        lv_errors = lv_errors + 1.
      ENDLOOP.
    ENDIF.

    "--- 5. Commit or roll back the whole batch ------------------------
    IF lv_errors > 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      DATA(lo_msg_container) = mo_context->get_message_container( ).
      lo_msg_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg_container.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ENDIF.

    "--- 6. Echo the processed header back to the caller ---------------
    DATA ls_head TYPE zcl_gms_exchrate_mpc=>ty_exchange_rates.
    ls_head-request_id = ls_deep-request_id.
    copy_data_to_ref( EXPORTING is_data = ls_head
                      CHANGING  cr_data = er_deep_entity ).
  ENDMETHOD.


  METHOD validate_rate.
    IF is_rate-rate_type  IS INITIAL OR is_rate-from_curr   IS INITIAL OR
       is_rate-to_currncy IS INITIAL OR is_rate-valid_from  IS INITIAL OR
       is_rate-exch_rate  IS INITIAL OR is_rate-from_factor IS INITIAL OR
       is_rate-to_factor  IS INITIAL.
      rs_return-type    = 'E'.
      rs_return-id      = 'ZGMS'.
      rs_return-number  = '000'.
      rs_return-message = |Mandatory field missing for { is_rate-from_curr }/{ is_rate-to_currncy } { is_rate-valid_from }|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
