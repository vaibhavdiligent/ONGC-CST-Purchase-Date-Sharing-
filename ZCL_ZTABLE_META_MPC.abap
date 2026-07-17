*&---------------------------------------------------------------------*
*& Class  ZCL_ZTABLE_META_MPC  (Model Provider - code based, OData V2)
*&---------------------------------------------------------------------*
*& Generic / dynamic "table dictionary + data" service for SAP CPI.
*&
*& Input  : a table name (VBAP, EKPO, MARA, ... any transparent table
*&          or DDIC view) passed as a $filter on property Tabname.
*& Output : - TableStructureSet -> one entity per field of that table
*&            (field name, data element, domain, type, length, decimals,
*&             key flag, check table, description, ... = full DDIC
*&             field characteristics).
*&          - TableDataSet      -> one entity per record of that table,
*&            the whole record serialised to JSON in property DataJson
*&            (dynamic: works for ANY table without changing the model).
*&
*& The OData MODEL is static (two fixed entity types); only the CONTENT
*& is dynamic, so a single service serves every table.
*&
*& Fully code based -> extends /IWBEP/CL_MGW_ABS_MODEL (the standard
*& code-based model base, same one SEGW-generated MPCs use), no SEGW
*& project required. Register with /IWFND/MAINT_SERVICE using this class
*& as the Model Provider and ZCL_ZTABLE_META_DPC as the Data Provider.
*&---------------------------------------------------------------------*
CLASS zcl_ztable_meta_mpc DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_mgw_abs_model
  CREATE PUBLIC.

  PUBLIC SECTION.
    " One field of the requested table (mirrors DFIES characteristics).
    TYPES: BEGIN OF ty_field_info,
             tabname    TYPE dfies-tabname,     " table / view name
             fieldname  TYPE dfies-fieldname,   " field name
             position   TYPE i,                 " position in structure
             keyflag    TYPE dfies-keyflag,     " 'X' = key field
             rollname   TYPE dfies-rollname,    " data element
             domname    TYPE dfies-domname,     " domain
             datatype   TYPE dfies-datatype,    " DDIC type (CHAR/NUMC/DEC..)
             leng       TYPE i,                 " DDIC length
             decimals   TYPE i,                 " decimal places
             inttype    TYPE dfies-inttype,     " ABAP internal type (C/N/P/D..)
             intlen     TYPE i,                 " internal length in bytes
             lowercase  TYPE dfies-lowercase,   " 'X' = lower case allowed
             signflag   TYPE dfies-sign,        " 'X' = value can be negative
             checktable TYPE dfies-checktable,  " foreign-key check table
             reftable   TYPE dfies-reftable,    " reference table (CURR/QUAN)
             reffield   TYPE dfies-reffield,    " reference field
             convexit   TYPE dfies-convexit,    " conversion exit
             fieldtext  TYPE dfies-fieldtext,   " short field text
             scrtext_l  TYPE dfies-scrtext_l,   " long field label
           END OF ty_field_info.
    TYPES tt_field_info TYPE STANDARD TABLE OF ty_field_info WITH DEFAULT KEY.

    " One record of the requested table, serialised to JSON.
    TYPES: BEGIN OF ty_table_row,
             tabname  TYPE dfies-tabname,   " table / view name (key)
             row_no   TYPE i,               " 1-based record number (key)
             data_json TYPE string,         " whole record as JSON
           END OF ty_table_row.
    TYPES tt_table_row TYPE STANDARD TABLE OF ty_table_row WITH DEFAULT KEY.

    " Code-based push model: only DEFINE must be redefined. The model
    " name/version are supplied at service registration (/IWBEP/REG_SERVICE),
    " so GET_MODEL is intentionally NOT redefined (its owning interface name
    " differs across Gateway releases/landscapes).
    METHODS define REDEFINITION.

  PROTECTED SECTION.
    METHODS define_structure_entity.
    METHODS define_data_entity.
ENDCLASS.


CLASS zcl_ztable_meta_mpc IMPLEMENTATION.

  METHOD define.
    me->define_structure_entity( ).
    me->define_data_entity( ).
  ENDMETHOD.


  METHOD define_structure_entity.
    DATA: lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ,
          lo_property    TYPE REF TO /iwbep/if_mgw_odata_property,
          lo_entity_set  TYPE REF TO /iwbep/if_mgw_odata_entity_set.

    lo_entity_type = model->create_entity_type( iv_entity_type_name = 'TableFieldInfo'
                                                iv_def_entity_set   = abap_false ).

    "--- key: Tabname + Fieldname --------------------------------------
    lo_property = lo_entity_type->create_property( iv_property_name  = 'Tabname'
                                                   iv_abap_fieldname = 'TABNAME' ).
    lo_property->set_is_key( ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 30 ).
    lo_property->set_nullable( abap_false ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Fieldname'
                                                   iv_abap_fieldname = 'FIELDNAME' ).
    lo_property->set_is_key( ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 30 ).
    lo_property->set_nullable( abap_false ).

    "--- field characteristics -----------------------------------------
    lo_property = lo_entity_type->create_property( iv_property_name  = 'Position'
                                                   iv_abap_fieldname = 'POSITION' ).
    lo_property->set_type_edm_int32( ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Keyflag'
                                                   iv_abap_fieldname = 'KEYFLAG' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 1 ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Rollname'
                                                   iv_abap_fieldname = 'ROLLNAME' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 30 ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Domname'
                                                   iv_abap_fieldname = 'DOMNAME' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 30 ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Datatype'
                                                   iv_abap_fieldname = 'DATATYPE' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 4 ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Leng'
                                                   iv_abap_fieldname = 'LENG' ).
    lo_property->set_type_edm_int32( ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Decimals'
                                                   iv_abap_fieldname = 'DECIMALS' ).
    lo_property->set_type_edm_int32( ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Inttype'
                                                   iv_abap_fieldname = 'INTTYPE' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 1 ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Intlen'
                                                   iv_abap_fieldname = 'INTLEN' ).
    lo_property->set_type_edm_int32( ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Lowercase'
                                                   iv_abap_fieldname = 'LOWERCASE' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 1 ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Signflag'
                                                   iv_abap_fieldname = 'SIGNFLAG' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 1 ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Checktable'
                                                   iv_abap_fieldname = 'CHECKTABLE' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 30 ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Reftable'
                                                   iv_abap_fieldname = 'REFTABLE' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 30 ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Reffield'
                                                   iv_abap_fieldname = 'REFFIELD' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 30 ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Convexit'
                                                   iv_abap_fieldname = 'CONVEXIT' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 5 ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Fieldtext'
                                                   iv_abap_fieldname = 'FIELDTEXT' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 60 ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Scrtext_l'
                                                   iv_abap_fieldname = 'SCRTEXT_L' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 40 ).

    "--- bind + expose entity set --------------------------------------
    lo_entity_type->bind_structure( iv_structure_name = 'ZCL_ZTABLE_META_MPC=>TY_FIELD_INFO' ).

    lo_entity_set = lo_entity_type->create_entity_set( iv_entity_set_name = 'TableStructureSet' ).
    lo_entity_set->set_creatable(   abap_false ).
    lo_entity_set->set_updatable(   abap_false ).
    lo_entity_set->set_deletable(   abap_false ).
    lo_entity_set->set_pageable(    abap_true  ).
    lo_entity_set->set_addressable( abap_true  ).
  ENDMETHOD.


  METHOD define_data_entity.
    DATA: lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ,
          lo_property    TYPE REF TO /iwbep/if_mgw_odata_property,
          lo_entity_set  TYPE REF TO /iwbep/if_mgw_odata_entity_set.

    lo_entity_type = model->create_entity_type( iv_entity_type_name = 'TableRow'
                                                iv_def_entity_set   = abap_false ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'Tabname'
                                                   iv_abap_fieldname = 'TABNAME' ).
    lo_property->set_is_key( ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 30 ).
    lo_property->set_nullable( abap_false ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'RowNo'
                                                   iv_abap_fieldname = 'ROW_NO' ).
    lo_property->set_is_key( ).
    lo_property->set_type_edm_int32( ).
    lo_property->set_nullable( abap_false ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'DataJson'
                                                   iv_abap_fieldname = 'DATA_JSON' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_nullable( abap_true ).

    lo_entity_type->bind_structure( iv_structure_name = 'ZCL_ZTABLE_META_MPC=>TY_TABLE_ROW' ).

    lo_entity_set = lo_entity_type->create_entity_set( iv_entity_set_name = 'TableDataSet' ).
    lo_entity_set->set_creatable(   abap_false ).
    lo_entity_set->set_updatable(   abap_false ).
    lo_entity_set->set_deletable(   abap_false ).
    lo_entity_set->set_pageable(    abap_true  ).
    lo_entity_set->set_addressable( abap_true  ).
  ENDMETHOD.

ENDCLASS.
