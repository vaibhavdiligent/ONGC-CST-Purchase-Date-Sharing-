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
*& The whole batch lands in ONE call in CREATE_DEEP_ENTITY below.
*& >>> Add your SAP update logic in the marked section. <<<
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
    "! Per-item processing - returns a BAPIRET2 message for the item.
    "! Put the actual SAP table/BAPI update for ONE rate here.
    METHODS process_rate
      IMPORTING
        is_rate          TYPE zcl_gms_exchrate_mpc=>ty_exchange_rate
      RETURNING
        VALUE(rs_return) TYPE bapiret2.
ENDCLASS.


CLASS zcl_gms_exchrate_dpc IMPLEMENTATION.

  METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.

    DATA: ls_deep    TYPE zcl_gms_exchrate_mpc=>ty_deep,
          ls_return  TYPE bapiret2,
          lt_return  TYPE bapiret2_t,
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

    "--- 3. Process every exchange-rate item ---------------------------
    "*******************************************************************
    "*  >>> WRITE YOUR SAP UPDATE LOGIC INSIDE process_rate( ) <<<     *
    "*  e.g. BAPI_EXCHANGERATE_CREATEMULTIPLE / TCURR update / your    *
    "*       custom Z table. Each item is one rate from CPI.           *
    "*******************************************************************
    LOOP AT ls_deep-exchangerate ASSIGNING <ls_rate>.
      ls_return = me->process_rate( <ls_rate> ).
      APPEND ls_return TO lt_return.
      IF ls_return-type CA 'EAX'.
        lv_errors = lv_errors + 1.
      ENDIF.
    ENDLOOP.

    "--- 4. Roll back the whole batch on any error ---------------------
    IF lv_errors > 0.
      " surface the collected messages to CPI as the OData error body
      DATA(lo_msg_container) = mo_context->get_message_container( ).
      lo_msg_container->add_messages_from_bapi( it_bapi_messages = lt_return ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_msg_container.
    ENDIF.

    "--- 5. Echo the processed header back to the caller ---------------
    DATA ls_head TYPE zcl_gms_exchrate_mpc=>ty_exchange_rates.
    ls_head-request_id = ls_deep-request_id.
    copy_data_to_ref( EXPORTING is_data = ls_head
                      CHANGING  cr_data = er_deep_entity ).
  ENDMETHOD.


  METHOD process_rate.
    "*******************************************************************
    "* TODO (you): replace the body below with the real SAP update.   *
    "* Currently it only validates and returns a success message so   *
    "* the service is callable end-to-end from CPI before the update  *
    "* logic is wired in.                                              *
    "*                                                                 *
    "* Available fields (string based, as per the CPI XSD):           *
    "*   is_rate-rate_type / from_curr / to_currncy / valid_from      *
    "*   is_rate-exch_rate / from_factor / to_factor                  *
    "*   is_rate-exch_rate_v / from_factor_v / to_factor_v (optional) *
    "*******************************************************************

    " minimal mandatory-field validation
    IF is_rate-rate_type  IS INITIAL OR is_rate-from_curr   IS INITIAL OR
       is_rate-to_currncy IS INITIAL OR is_rate-valid_from  IS INITIAL OR
       is_rate-exch_rate  IS INITIAL OR is_rate-from_factor IS INITIAL OR
       is_rate-to_factor  IS INITIAL.
      rs_return-type    = 'E'.
      rs_return-id      = 'ZGMS'.
      rs_return-number  = '000'.
      rs_return-message = |Mandatory field missing for { is_rate-from_curr }/{ is_rate-to_currncy }|.
      RETURN.
    ENDIF.

    " <<< INSERT TCURR / BAPI_EXCHANGERATE_CREATEMULTIPLE UPDATE HERE >>>

    rs_return-type    = 'S'.
    rs_return-id      = 'ZGMS'.
    rs_return-number  = '001'.
    rs_return-message = |Rate { is_rate-from_curr }/{ is_rate-to_currncy } { is_rate-valid_from } accepted|.
  ENDMETHOD.

ENDCLASS.
