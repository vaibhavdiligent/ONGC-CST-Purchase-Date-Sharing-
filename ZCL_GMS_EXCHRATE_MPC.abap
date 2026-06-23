*&---------------------------------------------------------------------*
*& Class  ZCL_GMS_EXCHRATE_MPC  (Model Provider - code based, OData V2)
*&---------------------------------------------------------------------*
*& Inbound OData V2 service for Exchange Rate upload from SAP CPI.
*&
*& Models the XSD provided by CPI:
*&   ExchangeRates (root) -> ExchangeRate (0..n)
*&     Mandatory : RATE_TYPE, FROM_CURR, TO_CURRNCY, VALID_FROM,
*&                 EXCH_RATE, FROM_FACTOR, TO_FACTOR
*&     Optional  : EXCH_RATE_V, FROM_FACTOR_V, TO_FACTOR_V
*&
*& Modelled as a DEEP entity so CPI can post the whole batch in one call:
*&   POST /ExchangeRatesSet     (header)  with nested
*&        ExchangeRate : [ {...}, {...} ]  (navigation -> items)
*&
*& Fully code based -> extends /IWBEP/CL_MGW_PUSH_ABS_MODEL, no SEGW
*& project required. Register with /IWFND/MAINT_SERVICE using this class
*& as the Model Provider and ZCL_GMS_EXCHRATE_DPC as the Data Provider.
*&---------------------------------------------------------------------*
CLASS zcl_gms_exchrate_mpc DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_mgw_push_abs_model
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! Technical model name / version exposed in $metadata
    CONSTANTS gc_model_name    TYPE /iwbep/med_model_name VALUE 'ZGMS_EXCHRATE_MDL'.
    CONSTANTS gc_model_version TYPE /iwbep/med_version     VALUE '0001'.

    "! ABAP type used by the Data Provider for a single exchange-rate item.
    "! Names match the OData property names 1:1 so /ui2/cl_json and the
    "! gateway runtime map them without extra name-mapping.
    TYPES: BEGIN OF ty_exchange_rate,
             rate_type     TYPE c LENGTH 4,    " RATE_TYPE  (key)
             from_curr     TYPE c LENGTH 5,    " FROM_CURR  (key)
             to_currncy    TYPE c LENGTH 5,    " TO_CURRNCY (key)
             valid_from    TYPE c LENGTH 8,    " VALID_FROM (key) YYYYMMDD
             exch_rate     TYPE c LENGTH 30,   " EXCH_RATE
             from_factor   TYPE c LENGTH 10,   " FROM_FACTOR
             to_factor     TYPE c LENGTH 10,   " TO_FACTOR
             exch_rate_v   TYPE c LENGTH 30,   " EXCH_RATE_V   (optional)
             from_factor_v TYPE c LENGTH 10,   " FROM_FACTOR_V (optional)
             to_factor_v   TYPE c LENGTH 10,   " TO_FACTOR_V   (optional)
           END OF ty_exchange_rate.
    TYPES tt_exchange_rate TYPE STANDARD TABLE OF ty_exchange_rate WITH DEFAULT KEY.

    "! Header entity - one row per upload batch.
    TYPES: BEGIN OF ty_exchange_rates,
             request_id TYPE c LENGTH 32,      " REQUEST_ID (key, generated/echoed)
           END OF ty_exchange_rates.

    "! Deep structure consumed by CREATE_DEEP_ENTITY in the DPC.
    TYPES: BEGIN OF ty_deep,
             request_id   TYPE c LENGTH 32,
             exchangerate TYPE tt_exchange_rate,   " navigation target collection
           END OF ty_deep.

    METHODS /iwbep/if_mgw_odata_fw_model~get_model REDEFINITION.
    METHODS define                                  REDEFINITION.

  PROTECTED SECTION.
    METHODS define_exchange_rate.   " item entity type + entity set
    METHODS define_exchange_rates.  " header entity type + entity set
    METHODS define_association.     " header -> items navigation
ENDCLASS.


CLASS zcl_gms_exchrate_mpc IMPLEMENTATION.

  METHOD /iwbep/if_mgw_odata_fw_model~get_model.
    model_name    = gc_model_name.
    model_version = gc_model_version.
  ENDMETHOD.


  METHOD define.
    me->define_exchange_rate( ).
    me->define_exchange_rates( ).
    me->define_association( ).
  ENDMETHOD.


  METHOD define_exchange_rate.
    DATA: lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ,
          lo_property    TYPE REF TO /iwbep/if_mgw_odata_property,
          lo_entity_set  TYPE REF TO /iwbep/if_mgw_odata_entity_set.

    lo_entity_type = model->create_entity_type( iv_entity_type_name = 'ExchangeRate'
                                                iv_def_entity_set   = abap_false ).

    "--- Mandatory key fields ------------------------------------------
    lo_property = lo_entity_type->create_property( iv_property_name = 'RATE_TYPE'
                                                   iv_abap_fieldname = 'RATE_TYPE' ).
    lo_property->set_is_key( ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 4 ).
    lo_property->set_nullable( abap_false ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'FROM_CURR'
                                                   iv_abap_fieldname = 'FROM_CURR' ).
    lo_property->set_is_key( ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 5 ).
    lo_property->set_nullable( abap_false ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'TO_CURRNCY'
                                                   iv_abap_fieldname = 'TO_CURRNCY' ).
    lo_property->set_is_key( ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 5 ).
    lo_property->set_nullable( abap_false ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'VALID_FROM'
                                                   iv_abap_fieldname = 'VALID_FROM' ).
    lo_property->set_is_key( ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 8 ).
    lo_property->set_nullable( abap_false ).

    "--- Mandatory value fields ----------------------------------------
    lo_property = lo_entity_type->create_property( iv_property_name = 'EXCH_RATE'
                                                   iv_abap_fieldname = 'EXCH_RATE' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 30 ).
    lo_property->set_nullable( abap_false ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'FROM_FACTOR'
                                                   iv_abap_fieldname = 'FROM_FACTOR' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 10 ).
    lo_property->set_nullable( abap_false ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'TO_FACTOR'
                                                   iv_abap_fieldname = 'TO_FACTOR' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 10 ).
    lo_property->set_nullable( abap_false ).

    "--- Optional value fields (nullable) ------------------------------
    lo_property = lo_entity_type->create_property( iv_property_name = 'EXCH_RATE_V'
                                                   iv_abap_fieldname = 'EXCH_RATE_V' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 30 ).
    lo_property->set_nullable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'FROM_FACTOR_V'
                                                   iv_abap_fieldname = 'FROM_FACTOR_V' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 10 ).
    lo_property->set_nullable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'TO_FACTOR_V'
                                                   iv_abap_fieldname = 'TO_FACTOR_V' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 10 ).
    lo_property->set_nullable( abap_true ).

    "--- Bind ABAP structure & expose the entity set -------------------
    lo_entity_type->bind_structure( iv_structure_name = 'ZCL_GMS_EXCHRATE_MPC=>TY_EXCHANGE_RATE' ).

    lo_entity_set = lo_entity_type->create_entity_set( iv_entity_set_name = 'ExchangeRateSet' ).
    lo_entity_set->set_creatable( abap_true ).
    lo_entity_set->set_updatable( abap_false ).
    lo_entity_set->set_deletable( abap_false ).
    lo_entity_set->set_pageable(  abap_false ).
    lo_entity_set->set_addressable( abap_true ).
  ENDMETHOD.


  METHOD define_exchange_rates.
    DATA: lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ,
          lo_property    TYPE REF TO /iwbep/if_mgw_odata_property,
          lo_entity_set  TYPE REF TO /iwbep/if_mgw_odata_entity_set.

    lo_entity_type = model->create_entity_type( iv_entity_type_name = 'ExchangeRates'
                                                iv_def_entity_set   = abap_false ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'REQUEST_ID'
                                                   iv_abap_fieldname = 'REQUEST_ID' ).
    lo_property->set_is_key( ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 32 ).
    lo_property->set_nullable( abap_true ).   " server can generate it

    lo_entity_type->bind_structure( iv_structure_name = 'ZCL_GMS_EXCHRATE_MPC=>TY_EXCHANGE_RATES' ).

    lo_entity_set = lo_entity_type->create_entity_set( iv_entity_set_name = 'ExchangeRatesSet' ).
    lo_entity_set->set_creatable( abap_true ).
    lo_entity_set->set_updatable( abap_false ).
    lo_entity_set->set_deletable( abap_false ).
    lo_entity_set->set_pageable(  abap_false ).
    lo_entity_set->set_addressable( abap_true ).
  ENDMETHOD.


  METHOD define_association.
    DATA: lo_assoc     TYPE REF TO /iwbep/if_mgw_odata_assoc,
          lo_nav       TYPE REF TO /iwbep/if_mgw_odata_nav_prop,
          lo_ref_const TYPE REF TO /iwbep/if_mgw_odata_referc_cons,
          lo_assoc_set TYPE REF TO /iwbep/if_mgw_odata_assoc_set,
          lo_entity    TYPE REF TO /iwbep/if_mgw_odata_entity_typ.

    "--- Association ExchangeRates (1) -> ExchangeRate (0..n) ----------
    lo_assoc = model->create_association(
                 iv_association_name  = 'ExchangeRates_ExchangeRate'
                 iv_left_type         = 'ExchangeRates'
                 iv_right_type        = 'ExchangeRate'
                 iv_left_card         = /iwbep/if_mgw_med_odata_types=>gcs_cardinality-card_1
                 iv_right_card        = /iwbep/if_mgw_med_odata_types=>gcs_cardinality-card_0_n ).

    "--- Navigation property on the header -----------------------------
    lo_entity = model->get_entity_type( iv_entity_name = 'ExchangeRates' ).
    lo_nav = lo_entity->create_navigation_property(
               iv_property_name   = 'ExchangeRate'
               iv_association_name = 'ExchangeRates_ExchangeRate' ).

    "--- Association set ------------------------------------------------
    lo_assoc_set = model->create_association_set(
                     iv_association_set_name = 'ExchangeRates_ExchangeRateSet'
                     iv_left_entity_set_name  = 'ExchangeRatesSet'
                     iv_right_entity_set_name = 'ExchangeRateSet'
                     iv_association_name      = 'ExchangeRates_ExchangeRate' ).
  ENDMETHOD.

ENDCLASS.
