*&---------------------------------------------------------------------*
*& Class  YCL_YF01_EXCH_RATE_MPC   (Model Provider - code based, OData V2)
*&---------------------------------------------------------------------*
*& Service : YF01_EXCHANGE_RATE_SRV   (inbound Exchange Rate upload from CPI)
*&
*& DEEP model (matches the CPI XSD: ExchangeRates -> ExchangeRate*):
*&   Header  ExchangeRates (key REQUEST_ID)  --nav 'ExchangeRate'-->  ExchangeRate (0..n)
*&
*& Mandatory (Nullable=false): RATE_TYPE, FROM_CURR, TO_CURRNCY, VALID_FROM,
*&                             EXCH_RATE, FROM_FACTOR, TO_FACTOR
*& Optional  (Nullable=true) : EXCH_RATE_V, FROM_FACTOR_V, TO_FACTOR_V
*&
*& All properties + entity sets are set creatable=true in code (no SEGW,
*& so no creatable/cache surprises). VALID_FROM length 10 for DD.MM.YYYY.
*&
*& Extends /IWBEP/CL_MGW_PUSH_ABS_MODEL. Register with /IWBEP/REG_SERVICE
*& (model YF01_EXCHANGE_RATE_MDL, service YF01_EXCHANGE_RATE_SRV) then add
*& it in /IWFND/MAINT_SERVICE.
*&---------------------------------------------------------------------*
CLASS ycl_yf01_exch_rate_mpc DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_mgw_push_abs_model
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! One exchange-rate line item. Field names match the OData properties
    "! and BAPI1093_0 1:1 so MOVE-CORRESPONDING works in the DPC.
    TYPES: BEGIN OF ts_exchange_rate,
             rate_type     TYPE c LENGTH 4,
             from_curr     TYPE c LENGTH 5,
             to_currncy    TYPE c LENGTH 5,
             valid_from    TYPE c LENGTH 10,   " DD.MM.YYYY
             exch_rate     TYPE c LENGTH 30,
             from_factor   TYPE c LENGTH 10,
             to_factor     TYPE c LENGTH 10,
             exch_rate_v   TYPE c LENGTH 30,
             from_factor_v TYPE c LENGTH 10,
             to_factor_v   TYPE c LENGTH 10,
           END OF ts_exchange_rate.
    TYPES tt_exchange_rate TYPE STANDARD TABLE OF ts_exchange_rate WITH DEFAULT KEY.

    "! Header (parent) - technical key only, CPI leaves it blank.
    TYPES: BEGIN OF ts_exchange_rates,
             request_id TYPE c LENGTH 32,
           END OF ts_exchange_rates.

    METHODS define REDEFINITION.

  PROTECTED SECTION.
    METHODS define_exchange_rate.    " child entity + set
    METHODS define_exchange_rates.   " header entity + set
    METHODS define_association.      " header -> child navigation
ENDCLASS.


CLASS ycl_yf01_exch_rate_mpc IMPLEMENTATION.

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

    "--- Mandatory KEY fields (Nullable=false) -------------------------
    lo_property = lo_entity_type->create_property( iv_property_name = 'RATE_TYPE'  iv_abap_fieldname = 'RATE_TYPE' ).
    lo_property->set_is_key( ). lo_property->set_type_edm_string( ). lo_property->set_maxlength( 4 ).
    lo_property->set_nullable( abap_false ). lo_property->set_creatable( abap_true ). lo_property->set_updatable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'FROM_CURR'  iv_abap_fieldname = 'FROM_CURR' ).
    lo_property->set_is_key( ). lo_property->set_type_edm_string( ). lo_property->set_maxlength( 5 ).
    lo_property->set_nullable( abap_false ). lo_property->set_creatable( abap_true ). lo_property->set_updatable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'TO_CURRNCY' iv_abap_fieldname = 'TO_CURRNCY' ).
    lo_property->set_is_key( ). lo_property->set_type_edm_string( ). lo_property->set_maxlength( 5 ).
    lo_property->set_nullable( abap_false ). lo_property->set_creatable( abap_true ). lo_property->set_updatable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'VALID_FROM' iv_abap_fieldname = 'VALID_FROM' ).
    lo_property->set_is_key( ). lo_property->set_type_edm_string( ). lo_property->set_maxlength( 10 ).
    lo_property->set_nullable( abap_false ). lo_property->set_creatable( abap_true ). lo_property->set_updatable( abap_true ).

    "--- Mandatory VALUE fields (Nullable=false) -----------------------
    lo_property = lo_entity_type->create_property( iv_property_name = 'EXCH_RATE'   iv_abap_fieldname = 'EXCH_RATE' ).
    lo_property->set_type_edm_string( ). lo_property->set_maxlength( 30 ).
    lo_property->set_nullable( abap_false ). lo_property->set_creatable( abap_true ). lo_property->set_updatable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'FROM_FACTOR' iv_abap_fieldname = 'FROM_FACTOR' ).
    lo_property->set_type_edm_string( ). lo_property->set_maxlength( 10 ).
    lo_property->set_nullable( abap_false ). lo_property->set_creatable( abap_true ). lo_property->set_updatable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'TO_FACTOR'   iv_abap_fieldname = 'TO_FACTOR' ).
    lo_property->set_type_edm_string( ). lo_property->set_maxlength( 10 ).
    lo_property->set_nullable( abap_false ). lo_property->set_creatable( abap_true ). lo_property->set_updatable( abap_true ).

    "--- Optional VALUE fields (Nullable=true) -------------------------
    lo_property = lo_entity_type->create_property( iv_property_name = 'EXCH_RATE_V'   iv_abap_fieldname = 'EXCH_RATE_V' ).
    lo_property->set_type_edm_string( ). lo_property->set_maxlength( 30 ).
    lo_property->set_nullable( abap_true ). lo_property->set_creatable( abap_true ). lo_property->set_updatable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'FROM_FACTOR_V' iv_abap_fieldname = 'FROM_FACTOR_V' ).
    lo_property->set_type_edm_string( ). lo_property->set_maxlength( 10 ).
    lo_property->set_nullable( abap_true ). lo_property->set_creatable( abap_true ). lo_property->set_updatable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name = 'TO_FACTOR_V'   iv_abap_fieldname = 'TO_FACTOR_V' ).
    lo_property->set_type_edm_string( ). lo_property->set_maxlength( 10 ).
    lo_property->set_nullable( abap_true ). lo_property->set_creatable( abap_true ). lo_property->set_updatable( abap_true ).

    lo_entity_type->bind_structure( iv_structure_name = 'YCL_YF01_EXCH_RATE_MPC=>TS_EXCHANGE_RATE' ).

    lo_entity_set = lo_entity_type->create_entity_set( 'ExchangeRateSet' ).
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

    lo_property = lo_entity_type->create_property( iv_property_name = 'REQUEST_ID' iv_abap_fieldname = 'REQUEST_ID' ).
    lo_property->set_is_key( ). lo_property->set_type_edm_string( ). lo_property->set_maxlength( 32 ).
    lo_property->set_nullable( abap_true ). lo_property->set_creatable( abap_true ).

    lo_entity_type->bind_structure( iv_structure_name = 'YCL_YF01_EXCH_RATE_MPC=>TS_EXCHANGE_RATES' ).

    lo_entity_set = lo_entity_type->create_entity_set( 'ExchangeRatesSet' ).
    lo_entity_set->set_creatable( abap_true ).
    lo_entity_set->set_updatable( abap_false ).
    lo_entity_set->set_deletable( abap_false ).
    lo_entity_set->set_pageable(  abap_false ).
    lo_entity_set->set_addressable( abap_true ).
  ENDMETHOD.


  METHOD define_association.
    DATA: lo_assoc  TYPE REF TO /iwbep/if_mgw_odata_assoc,
          lo_nav    TYPE REF TO /iwbep/if_mgw_odata_nav_prop,
          lo_entity TYPE REF TO /iwbep/if_mgw_odata_entity_typ.

    lo_assoc = model->create_association(
                 iv_association_name = 'ExchangeRates_ExchangeRate'
                 iv_left_type        = 'ExchangeRates'
                 iv_right_type       = 'ExchangeRate'
                 iv_left_card        = /iwbep/if_mgw_med_odata_types=>gcs_cardinality-card_1
                 iv_right_card       = /iwbep/if_mgw_med_odata_types=>gcs_cardinality-card_0_n ).

    lo_entity = model->get_entity_type( iv_entity_name = 'ExchangeRates' ).
    lo_nav = lo_entity->create_navigation_property(
               iv_property_name    = 'ExchangeRate'
               iv_association_name = 'ExchangeRates_ExchangeRate' ).

    model->create_association_set(
      iv_association_set_name  = 'ExchangeRates_ExchangeRateSet'
      iv_left_entity_set_name  = 'ExchangeRatesSet'
      iv_right_entity_set_name = 'ExchangeRateSet'
      iv_association_name      = 'ExchangeRates_ExchangeRate' ).
  ENDMETHOD.

ENDCLASS.
