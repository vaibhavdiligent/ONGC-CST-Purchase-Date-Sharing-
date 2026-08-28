*&---------------------------------------------------------------------*
*& Class  ZCL_ZCCBJI_JCTINVR_MPC  (Model Provider - code based, OData V2)
*&---------------------------------------------------------------------*
*& Inbound NTA invoice-registration-number service for SAP CPI (CCBJI).
*&
*& CPI posts ONE deep entity per call: a technical header
*& (InvoiceRegistrations) wrapping MULTIPLE line items
*& (InvoiceRegistration) - one item per NTA record, same field format
*& as table /CCBJI/T_JCTINVR:
*&
*&   POST /InvoiceRegistrationsSet
*&   { "InvoiceRegistration": [ { "INVOICE_CD": "T1234567890123", ... },
*&                              { ... }, ... ] }
*&
*& The DPC (ZCL_ZCCBJI_JCTINVR_DPC) compares each incoming INVOICE_CD
*& against /CCBJI/T_JCTINVR and writes matched records back (Option A
*& logic - see ZCCBJI_JCTINVR_ODATA_GUIDE.md).
*&
*& Fully code based -> extends /IWBEP/CL_MGW_ABS_MODEL (the standard
*& code-based model base, same one SEGW-generated MPCs use), no SEGW
*& project required. Register in /IWBEP/REG_SERVICE with this class as
*& Model Provider and ZCL_ZCCBJI_JCTINVR_DPC as Data Provider
*& (model ZCCBJI_JCTINVR_MDL / service ZCCBJI_JCTINVR_SRV).
*&---------------------------------------------------------------------*
CLASS zcl_zccbji_jctinvr_mpc DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_mgw_abs_model
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Technical header (parent). CPI leaves REQUEST_ID empty in the
    " request; the response echoes the processing summary in it,
    " e.g. 'U:1234 S:56' (updated / skipped counts).
    TYPES: BEGIN OF ty_header,
             request_id TYPE c LENGTH 32,
           END OF ty_header.
    TYPES tt_header TYPE STANDARD TABLE OF ty_header WITH DEFAULT KEY.

    " One NTA record (child) - business fields of /CCBJI/T_JCTINVR.
    " Dates travel as strings; the DPC normalises YYYYMMDD, YYYY-MM-DD
    " and DD.MM.YYYY to DATS. MANDT and the /CCEJ/MDM_DELTA audit fields
    " are filled by the backend, never sent by CPI.
    TYPES: BEGIN OF ty_item,
             invoice_cd      TYPE c LENGTH 14,  " registration number (key)
             process_kbn     TYPE c LENGTH 2,   " business processing class.
             correction_kbn  TYPE c LENGTH 1,   " correction classification
             personal_kbn    TYPE c LENGTH 1,   " personnel classification
             domestic_kbn    TYPE c LENGTH 1,   " domestic/foreign class.
             latest_kbn      TYPE c LENGTH 1,   " latest history flag
             create_date     TYPE c LENGTH 10,  " registration date
             update_date     TYPE c LENGTH 10,  " update date
             revocation_date TYPE c LENGTH 10,  " revocation date
             expiration_date TYPE c LENGTH 10,  " expiration date
           END OF ty_item.
    TYPES tt_item TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY.

    " Code-based model: only DEFINE must be redefined. Model name/version
    " are supplied at registration (/IWBEP/REG_SERVICE), so GET_MODEL is
    " intentionally NOT redefined (same as ZCL_ZTABLE_META_MPC).
    METHODS define REDEFINITION.

  PROTECTED SECTION.
    METHODS define_header_entity.
    METHODS define_item_entity.
    METHODS define_association.
ENDCLASS.


CLASS zcl_zccbji_jctinvr_mpc IMPLEMENTATION.

  METHOD define.
    me->define_header_entity( ).
    me->define_item_entity( ).
    me->define_association( ).
  ENDMETHOD.


  METHOD define_header_entity.
    DATA: lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ,
          lo_property    TYPE REF TO /iwbep/if_mgw_odata_property,
          lo_entity_set  TYPE REF TO /iwbep/if_mgw_odata_entity_set.

    lo_entity_type = model->create_entity_type( iv_entity_type_name = 'InvoiceRegistrations'
                                                iv_def_entity_set   = abap_false ).

    " Technical key only. Nullable + creatable so CPI can POST without
    " supplying it (server fills it in the response).
    lo_property = lo_entity_type->create_property( iv_property_name  = 'REQUEST_ID'
                                                   iv_abap_fieldname = 'REQUEST_ID' ).
    lo_property->set_is_key( ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 32 ).
    lo_property->set_nullable( abap_true ).
    lo_property->set_creatable( abap_true ).

    lo_entity_type->bind_structure( iv_structure_name = 'ZCL_ZCCBJI_JCTINVR_MPC=>TY_HEADER' ).

    lo_entity_set = lo_entity_type->create_entity_set( iv_entity_set_name = 'InvoiceRegistrationsSet' ).
    lo_entity_set->set_creatable(   abap_true  ).   " CPI POSTs here
    lo_entity_set->set_updatable(   abap_false ).
    lo_entity_set->set_deletable(   abap_false ).
    lo_entity_set->set_pageable(    abap_false ).
    lo_entity_set->set_addressable( abap_true  ).
  ENDMETHOD.


  METHOD define_item_entity.
    DATA: lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ,
          lo_property    TYPE REF TO /iwbep/if_mgw_odata_property,
          lo_entity_set  TYPE REF TO /iwbep/if_mgw_odata_entity_set.

    lo_entity_type = model->create_entity_type( iv_entity_type_name = 'InvoiceRegistration'
                                                iv_def_entity_set   = abap_false ).

    "--- key: the registration number ----------------------------------
    lo_property = lo_entity_type->create_property( iv_property_name  = 'INVOICE_CD'
                                                   iv_abap_fieldname = 'INVOICE_CD' ).
    lo_property->set_is_key( ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 14 ).
    lo_property->set_nullable( abap_false ).
    lo_property->set_creatable( abap_true ).
    lo_property->set_updatable( abap_true ).

    "--- classifications ------------------------------------------------
    lo_property = lo_entity_type->create_property( iv_property_name  = 'PROCESS_KBN'
                                                   iv_abap_fieldname = 'PROCESS_KBN' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 2 ).
    lo_property->set_nullable( abap_true ).
    lo_property->set_creatable( abap_true ).
    lo_property->set_updatable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'CORRECTION_KBN'
                                                   iv_abap_fieldname = 'CORRECTION_KBN' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 1 ).
    lo_property->set_nullable( abap_true ).
    lo_property->set_creatable( abap_true ).
    lo_property->set_updatable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'PERSONAL_KBN'
                                                   iv_abap_fieldname = 'PERSONAL_KBN' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 1 ).
    lo_property->set_nullable( abap_true ).
    lo_property->set_creatable( abap_true ).
    lo_property->set_updatable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'DOMESTIC_KBN'
                                                   iv_abap_fieldname = 'DOMESTIC_KBN' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 1 ).
    lo_property->set_nullable( abap_true ).
    lo_property->set_creatable( abap_true ).
    lo_property->set_updatable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'LATEST_KBN'
                                                   iv_abap_fieldname = 'LATEST_KBN' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 1 ).
    lo_property->set_nullable( abap_true ).
    lo_property->set_creatable( abap_true ).
    lo_property->set_updatable( abap_true ).

    "--- dates (strings, normalised in the DPC) -------------------------
    lo_property = lo_entity_type->create_property( iv_property_name  = 'CREATE_DATE'
                                                   iv_abap_fieldname = 'CREATE_DATE' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 10 ).
    lo_property->set_nullable( abap_true ).
    lo_property->set_creatable( abap_true ).
    lo_property->set_updatable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'UPDATE_DATE'
                                                   iv_abap_fieldname = 'UPDATE_DATE' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 10 ).
    lo_property->set_nullable( abap_true ).
    lo_property->set_creatable( abap_true ).
    lo_property->set_updatable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'REVOCATION_DATE'
                                                   iv_abap_fieldname = 'REVOCATION_DATE' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 10 ).
    lo_property->set_nullable( abap_true ).
    lo_property->set_creatable( abap_true ).
    lo_property->set_updatable( abap_true ).

    lo_property = lo_entity_type->create_property( iv_property_name  = 'EXPIRATION_DATE'
                                                   iv_abap_fieldname = 'EXPIRATION_DATE' ).
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = 10 ).
    lo_property->set_nullable( abap_true ).
    lo_property->set_creatable( abap_true ).
    lo_property->set_updatable( abap_true ).

    lo_entity_type->bind_structure( iv_structure_name = 'ZCL_ZCCBJI_JCTINVR_MPC=>TY_ITEM' ).

    lo_entity_set = lo_entity_type->create_entity_set( iv_entity_set_name = 'InvoiceRegistrationSet' ).
    lo_entity_set->set_creatable(   abap_true  ).
    lo_entity_set->set_updatable(   abap_false ).
    lo_entity_set->set_deletable(   abap_false ).
    lo_entity_set->set_pageable(    abap_false ).
    lo_entity_set->set_addressable( abap_true  ).
  ENDMETHOD.


  METHOD define_association.
    " Header 1 : 0..n items. The navigation property name
    " 'InvoiceRegistration' is the wrapper element CPI maps the repeating
    " line items into (deep insert, Sub Levels = 1) - it must match the
    " component name used in the DPC's deep structure.
    model->create_association(
        iv_association_name = 'InvoiceRegistrations_InvoiceRegistration'
        iv_left_type        = 'InvoiceRegistrations'
        iv_right_type       = 'InvoiceRegistration'
        iv_left_card        = /iwbep/if_mgw_med_odata_types=>gcs_cardinality-cardinality_1_1
        iv_right_card       = /iwbep/if_mgw_med_odata_types=>gcs_cardinality-cardinality_0_n
        iv_def_assoc_set    = abap_true ).

    DATA(lo_header_type) = model->get_entity_type( iv_entity_name = 'InvoiceRegistrations' ).
    lo_header_type->create_navigation_property(
        iv_property_name    = 'InvoiceRegistration'
        iv_association_name = 'InvoiceRegistrations_InvoiceRegistration' ).
  ENDMETHOD.

ENDCLASS.
