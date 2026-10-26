*&---------------------------------------------------------------------*
*& Class  ZCL_ZTABLE_INS_MPC  (Model Provider - code based, OData V2)
*&---------------------------------------------------------------------*
*& Generic / dynamic "insert into a customer table" service for SAP CPI.
*&
*& Companion to the read service (ZCL_ZTABLE_META_*). CPI POSTs a target
*& table name plus the row(s) as JSON, and the service writes them:
*&
*&   POST /InsertSet
*&   { "Tabname":"ZFOO", "Mode":"MODIFY",
*&     "DataJson":"[ { ..row.. }, { ..row.. } ]" }
*&
*& DataJson uses the SAME shape the read service emits in TableDataSet,
*& so "read from system A -> insert into system B" is a straight
*& pass-through with no transformation on the CPI side.
*&
*& SAFETY: only CUSTOMER-NAMESPACE transparent tables may be written
*&         (Z*, Y*, /CCBJI/*, /CCEJ/* - enforced in the DPC). Every
*&         standard SAP table is refused. Auth is checked with
*&         S_TABU_NAM ACTVT '02' (change). Each POST is ONE LUW
*&         (all-or-nothing commit / rollback).
*&
*& The OData MODEL is static (one fixed entity type); only the CONTENT
*& is dynamic, so a single service inserts into any customer table.
*&
*& Fully code based -> extends /IWBEP/CL_MGW_ABS_MODEL (the standard
*& code-based model base). Register with /IWFND/MAINT_SERVICE using this
*& class as the Model Provider and ZCL_ZTABLE_INS_DPC as the Data
*& Provider.
*&---------------------------------------------------------------------*
CLASS zcl_ztable_ins_mpc DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_mgw_abs_model
  CREATE PUBLIC.

  PUBLIC SECTION.
    " One insert request (in: Tabname/Mode/DataJson/TestRun;
    " out: RowsProcessed/Status/Message). Same entity is used for the
    " POST body and the response, so the caller sees the outcome.
    TYPES: BEGIN OF ty_insert_request,
             tabname       TYPE c LENGTH 30,    " target customer table (key)
             mode          TYPE c LENGTH 10,    " INSERT / MODIFY / UPDATE
             test_run      TYPE c LENGTH 1,     " 'X' = validate + rollback
             data_json     TYPE string,         " row or JSON array of rows
             rows_processed TYPE i,             " rows actually written (out)
             status        TYPE c LENGTH 1,     " 'S' / 'E' (out)
             message       TYPE string,         " result / error text (out)
           END OF ty_insert_request.
    TYPES tt_insert_request TYPE STANDARD TABLE OF ty_insert_request WITH DEFAULT KEY.

    " Code-based model: only DEFINE must be redefined. The model
    " name/version are supplied at service registration
    " (/IWBEP/REG_SERVICE), so GET_MODEL is intentionally NOT redefined
    " (its owning interface name differs across Gateway releases).
    METHODS define REDEFINITION.

  PROTECTED SECTION.
    METHODS define_insert_entity.
ENDCLASS.


CLASS zcl_ztable_ins_mpc IMPLEMENTATION.

  METHOD define.
    me->define_insert_entity( ).
  ENDMETHOD.


  METHOD define_insert_entity.
    DATA: lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ,
          lo_property    TYPE REF TO /iwbep/if_mgw_odata_property,
          lo_entity_set  TYPE REF TO /iwbep/if_mgw_odata_entity_set.

    lo_entity_type = model->create_entity_type( iv_entity_type_name = 'InsertRequest'
                                                iv_def_entity_set   = abap_false ).

    "--- key: Tabname --------------------------------------------------
    lo_property = lo_entity_type->create_property( iv_property_name  = 'Tabname'
                                                   iv_abap_fieldname = 'TABNAME' ).
    lo_property->set_is_key( ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 30 ).
    lo_property->set_nullable( abap_false ).

    "--- input properties ----------------------------------------------
    " Write mode. Default (blank) = MODIFY (upsert, retry-safe).
    lo_property = lo_entity_type->create_property( iv_property_name  = 'Mode'
                                                   iv_abap_fieldname = 'MODE' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 10 ).
    lo_property->set_nullable( abap_true ).

    " 'X' = dry run: validate + write + ROLLBACK (nothing persisted).
    lo_property = lo_entity_type->create_property( iv_property_name  = 'TestRun'
                                                   iv_abap_fieldname = 'TEST_RUN' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 1 ).
    lo_property->set_nullable( abap_true ).

    " The payload: one row object, or a JSON array of row objects,
    " keyed by the target table's own field names (same shape the read
    " service returns in DataJson). May be Base64URL-encoded with a
    " 'B64:' prefix so CPI's adapter does not mangle the braces/quotes.
    lo_property = lo_entity_type->create_property( iv_property_name  = 'DataJson'
                                                   iv_abap_fieldname = 'DATA_JSON' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_nullable( abap_true ).

    "--- output properties (filled by the DPC in the response) ---------
    lo_property = lo_entity_type->create_property( iv_property_name  = 'RowsProcessed'
                                                   iv_abap_fieldname = 'ROWS_PROCESSED' ).
    lo_property->set_type_edm_int32( ).
    lo_property->set_nullable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Status'
                                                   iv_abap_fieldname = 'STATUS' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 1 ).
    lo_property->set_nullable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Message'
                                                   iv_abap_fieldname = 'MESSAGE' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_nullable( abap_true ).

    lo_entity_type->bind_structure( iv_structure_name = 'ZCL_ZTABLE_INS_MPC=>TY_INSERT_REQUEST' ).

    "--- entity set: creatable (POST) only -----------------------------
    lo_entity_set = lo_entity_type->create_entity_set( iv_entity_set_name = 'InsertSet' ).
    lo_entity_set->set_creatable(   abap_true  ).
    lo_entity_set->set_updatable(   abap_false ).
    lo_entity_set->set_deletable(   abap_false ).
    lo_entity_set->set_pageable(    abap_false ).
    lo_entity_set->set_addressable( abap_true  ).
  ENDMETHOD.

ENDCLASS.
