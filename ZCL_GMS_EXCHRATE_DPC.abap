*&---------------------------------------------------------------------*
*& Class  ZCL_GMS_EXCHRATE_DPC  (Data Provider - code based, OData V2)
*&---------------------------------------------------------------------*
*& Runtime for the inbound Exchange Rate upload service called by CPI.
*&
*& FLAT model: CPI calls Create(POST) on ExchangeRateSet. Multiple rows
*& are sent in ONE $batch call -> the framework runs CREATE_ENTITY once
*& per row (each row is one exchange rate).
*&
*& Each row is written to TCURR with BAPI_EXCHANGERATE_CREATEMULTIPLE.
*& VALID_FROM arrives as DD.MM.YYYY (e.g. 01.04.2025) and is converted
*& to YYYYMMDD for the BAPI.
*&
*& Field names of TS_EXCHANGERATE match BAPI1093_0 1:1, so
*& MOVE-CORRESPONDING fills the BAPI table directly.
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
    METHODS /iwbep/if_mgw_appl_srv_runtime~create_entity REDEFINITION.
ENDCLASS.


CLASS zcl_gms_exchrate_dpc IMPLEMENTATION.

  METHOD /iwbep/if_mgw_appl_srv_runtime~create_entity.

    DATA: ls_rate   TYPE zcl_gms_exchrate_mpc=>ty_exchange_rate,
          lt_list   TYPE STANDARD TABLE OF bapi1093_0,
          ls_list   TYPE bapi1093_0,
          lt_return TYPE bapiret2_t,
          ls_return TYPE bapiret2,
          lv_valid  TYPE c LENGTH 8.

    "--- 1. Read the single posted rate
    io_data_provider->read_entry_data( IMPORTING es_data = ls_rate ).

    "--- 2. Mandatory-field check
    IF ls_rate-rate_type  IS INITIAL OR ls_rate-from_curr   IS INITIAL OR
       ls_rate-to_currncy IS INITIAL OR ls_rate-valid_from  IS INITIAL OR
       ls_rate-exch_rate  IS INITIAL OR ls_rate-from_factor IS INITIAL OR
       ls_rate-to_factor  IS INITIAL.
      DATA(lv_msg1) = |Mandatory field missing for { ls_rate-from_curr }/{ ls_rate-to_currncy } { ls_rate-valid_from }|.
      DATA(lo_mc1)  = mo_context->get_message_container( ).
      lo_mc1->add_message_text_only( iv_msg_type = 'E' iv_msg_text = lv_msg1 ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING message_container = lo_mc1.
    ENDIF.

    "--- 3. Map to BAPI + convert VALID_FROM DD.MM.YYYY -> YYYYMMDD
    MOVE-CORRESPONDING ls_rate TO ls_list.
    IF ls_rate-valid_from CA '.'.
      lv_valid = ls_rate-valid_from+6(4)   " YYYY
              && ls_rate-valid_from+3(2)   " MM
              && ls_rate-valid_from+0(2).  " DD
    ELSE.
      lv_valid = ls_rate-valid_from.       " already YYYYMMDD
    ENDIF.
    ls_list-valid_from = lv_valid.
    APPEND ls_list TO lt_list.

    "--- 4. Post to TCURR
    CALL FUNCTION 'BAPI_EXCHANGERATE_CREATEMULTIPLE'
      EXPORTING
        upd_allowed    = abap_true
      TABLES
        exch_rate_list = lt_list
        return         = lt_return.

    "--- 5. Error handling / commit (per row within the $batch changeset)
    READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc <> 0.
      READ TABLE lt_return INTO ls_return WITH KEY type = 'A'.
    ENDIF.
    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      DATA(lo_mc2) = mo_context->get_message_container( ).
      lo_mc2->add_messages_from_bapi( it_bapi_messages = lt_return ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING message_container = lo_mc2.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING wait = abap_true.
    ENDIF.

    "--- 6. Echo the created entity back
    copy_data_to_ref( EXPORTING is_data = ls_rate
                      CHANGING  cr_data = er_entity ).
  ENDMETHOD.

ENDCLASS.
