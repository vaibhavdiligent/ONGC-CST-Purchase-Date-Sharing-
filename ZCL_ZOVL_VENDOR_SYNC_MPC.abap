*&---------------------------------------------------------------------*
*& Class  ZCL_ZOVL_VENDOR_SYNC_MPC  (Model Provider - code based, OData V2)
*&---------------------------------------------------------------------*
*& Inbound Employee Vendor (VEMP) master sync ONGC -> OVL
*& (Interface OVL-INT-VEN-001, ONGC_OVL_Vendor_Data_Sync_Document.pdf).
*&
*& ONGC batch job -> ONGC PI/PO -> OVL CPI -> THIS SERVICE (OVL S/4HANA).
*& CPI posts ONE deep entity per vendor: the vendor general data (LFA1)
*& wrapping its company code data (LFB1) and bank details (LFBK):
*&
*&   POST /VendorSet
*&   { "LIFNR": "0000123456", "NAME1": "...", ...,
*&     "ToCompanyCode": [ { "BUKRS": "OVL", "AKONT": "...", ... } ],
*&     "ToBank":        [ { "BANKS": "IN", "BANKL": "SBIN0000001", ... } ] }
*&
*& The DPC (ZCL_ZOVL_VENDOR_SYNC_DPC) creates the Business Partner +
*& vendor with the SAME number as the ONGC vendor (LIFNR), or updates it
*& when it already exists (ZCL_OVL_VENDOR_SYNC).
*&
*& Fully code based -> extends /IWBEP/CL_MGW_ABS_MODEL, no SEGW project
*& (same process as ZCL_ZCCBJI_JCTINVR_MPC). Register in
*& /IWBEP/REG_SERVICE with this class as Model Provider and
*& ZCL_ZOVL_VENDOR_SYNC_DPC as Data Provider
*& (model ZOVL_VENDOR_SYNC_MDL / service ZOVL_VENDOR_SYNC_SRV).
*&---------------------------------------------------------------------*
CLASS zcl_zovl_vendor_sync_mpc DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_mgw_abs_model
  CREATE PUBLIC.

  PUBLIC SECTION.
    " Vendor general data (LFA1) - header entity Vendor.
    " STATUS / ACTION / MESSAGE are response fields: CPI leaves them
    " empty, the backend fills them (acknowledgement back to CPI).
    " NAME_FIRST / NAME_LAST / BP_CATEGORY are optional additions to the
    " document payload (ONGC BUT000) - when empty, NAME1 is used.
    TYPES: BEGIN OF ty_vendor,
             lifnr       TYPE c LENGTH 10,   " LIFNR  vendor = BP number (key)
             land1       TYPE c LENGTH 3,    " LAND1  country
             name1       TYPE c LENGTH 35,   " NAME1  name
             ort01       TYPE c LENGTH 35,   " ORT01  city
             ort02       TYPE c LENGTH 35,   " ORT02  district
             pstlz       TYPE c LENGTH 10,   " PSTLZ  postal code
             regio       TYPE c LENGTH 3,    " REGIO  region / state
             sortl       TYPE c LENGTH 10,   " SORTL  sort field
             stras       TYPE c LENGTH 60,   " STRAS  street / house no.
             adrnr       TYPE c LENGTH 10,   " ADRNR  ONGC address no. (info only)
             mcod1       TYPE c LENGTH 25,   " MCOD1  search term 1 (info only)
             mcod3       TYPE c LENGTH 25,   " MCOD3  search term 3 (info only)
             anred       TYPE c LENGTH 15,   " ANRED  title (text or key)
             konzs       TYPE c LENGTH 10,   " KONZS  group key
             spras       TYPE c LENGTH 2,    " SPRAS  language (SAP or ISO)
             telf1       TYPE c LENGTH 30,   " TELF1  telephone 1
             gbort       TYPE c LENGTH 25,   " GBORT  place of birth
             j_1ipanno   TYPE c LENGTH 40,   " J_1IPANNO PAN
             name_first  TYPE c LENGTH 40,   " optional - person first name
             name_last   TYPE c LENGTH 40,   " optional - person last name
             bp_category TYPE c LENGTH 1,    " optional - 1 person / 2 org.
             status      TYPE c LENGTH 1,    " response: S / E
             action      TYPE c LENGTH 10,   " response: CREATE / UPDATE
             message     TYPE c LENGTH 220,  " response: result text
           END OF ty_vendor.
    TYPES tt_vendor TYPE STANDARD TABLE OF ty_vendor WITH DEFAULT KEY.

    " Vendor company code data (LFB1) - item entity CompanyCode
    TYPES: BEGIN OF ty_company,
             lifnr TYPE c LENGTH 10,   " LIFNR (key)
             bukrs TYPE c LENGTH 4,    " BUKRS (key) - OVL
             pernr TYPE c LENGTH 8,    " PERNR personnel number
             zuawa TYPE c LENGTH 3,    " ZUAWA sort key
             akont TYPE c LENGTH 10,   " AKONT reconciliation account
             zwels TYPE c LENGTH 10,   " ZWELS payment methods
             zterm TYPE c LENGTH 4,    " ZTERM ignored - defaulted in OVL
             kverm TYPE c LENGTH 30,   " KVERM memo
           END OF ty_company.
    TYPES tt_company TYPE STANDARD TABLE OF ty_company WITH DEFAULT KEY.

    " Vendor bank details (LFBK) - item entity Bank
    TYPES: BEGIN OF ty_bank,
             lifnr TYPE c LENGTH 10,   " LIFNR (key)
             banks TYPE c LENGTH 3,    " BANKS bank country (key)
             bankl TYPE c LENGTH 15,   " BANKL bank key / IFSC (key)
             bankn TYPE c LENGTH 18,   " BANKN account number (key)
             bvtyp TYPE c LENGTH 4,    " BVTYP partner bank type
             koinh TYPE c LENGTH 60,   " KOINH account holder
           END OF ty_bank.
    TYPES tt_bank TYPE STANDARD TABLE OF ty_bank WITH DEFAULT KEY.

    " Code-based model: only DEFINE must be redefined. Model name/version
    " are supplied at registration (/IWBEP/REG_SERVICE), so GET_MODEL is
    " intentionally NOT redefined (same as ZCL_ZCCBJI_JCTINVR_MPC).
    METHODS define REDEFINITION.

  PROTECTED SECTION.
    METHODS define_vendor_entity.
    METHODS define_company_entity.
    METHODS define_bank_entity.
    METHODS define_associations.

    " Adds one Edm.String property (creatable + updatable)
    METHODS add_property
      IMPORTING io_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ
                iv_name        TYPE string
                iv_length      TYPE i
                iv_key         TYPE abap_bool DEFAULT abap_false.

    " Exposes the entity set of an entity type (CPI POSTs only)
    METHODS add_entity_set
      IMPORTING io_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ
                iv_set_name    TYPE string.
ENDCLASS.


CLASS zcl_zovl_vendor_sync_mpc IMPLEMENTATION.

  METHOD define.
    me->define_vendor_entity( ).
    me->define_company_entity( ).
    me->define_bank_entity( ).
    me->define_associations( ).
  ENDMETHOD.


  METHOD define_vendor_entity.
    DATA(lo_entity_type) = model->create_entity_type( iv_entity_type_name = 'Vendor'
                                                      iv_def_entity_set   = abap_false ).

    add_property( io_entity_type = lo_entity_type iv_name = 'LIFNR'       iv_length = 10 iv_key = abap_true ).
    add_property( io_entity_type = lo_entity_type iv_name = 'LAND1'       iv_length = 3 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'NAME1'       iv_length = 35 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'ORT01'       iv_length = 35 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'ORT02'       iv_length = 35 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'PSTLZ'       iv_length = 10 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'REGIO'       iv_length = 3 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'SORTL'       iv_length = 10 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'STRAS'       iv_length = 60 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'ADRNR'       iv_length = 10 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'MCOD1'       iv_length = 25 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'MCOD3'       iv_length = 25 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'ANRED'       iv_length = 15 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'KONZS'       iv_length = 10 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'SPRAS'       iv_length = 2 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'TELF1'       iv_length = 30 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'GBORT'       iv_length = 25 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'J_1IPANNO'   iv_length = 40 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'NAME_FIRST'  iv_length = 40 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'NAME_LAST'   iv_length = 40 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'BP_CATEGORY' iv_length = 1 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'STATUS'      iv_length = 1 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'ACTION'      iv_length = 10 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'MESSAGE'     iv_length = 220 ).

    lo_entity_type->bind_structure( iv_structure_name = 'ZCL_ZOVL_VENDOR_SYNC_MPC=>TY_VENDOR' ).
    add_entity_set( io_entity_type = lo_entity_type iv_set_name = 'VendorSet' ).
  ENDMETHOD.


  METHOD define_company_entity.
    DATA(lo_entity_type) = model->create_entity_type( iv_entity_type_name = 'CompanyCode'
                                                      iv_def_entity_set   = abap_false ).

    add_property( io_entity_type = lo_entity_type iv_name = 'LIFNR' iv_length = 10 iv_key = abap_true ).
    add_property( io_entity_type = lo_entity_type iv_name = 'BUKRS' iv_length = 4  iv_key = abap_true ).
    add_property( io_entity_type = lo_entity_type iv_name = 'PERNR' iv_length = 8 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'ZUAWA' iv_length = 3 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'AKONT' iv_length = 10 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'ZWELS' iv_length = 10 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'ZTERM' iv_length = 4 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'KVERM' iv_length = 30 ).

    lo_entity_type->bind_structure( iv_structure_name = 'ZCL_ZOVL_VENDOR_SYNC_MPC=>TY_COMPANY' ).
    add_entity_set( io_entity_type = lo_entity_type iv_set_name = 'CompanyCodeSet' ).
  ENDMETHOD.


  METHOD define_bank_entity.
    DATA(lo_entity_type) = model->create_entity_type( iv_entity_type_name = 'Bank'
                                                      iv_def_entity_set   = abap_false ).

    add_property( io_entity_type = lo_entity_type iv_name = 'LIFNR' iv_length = 10 iv_key = abap_true ).
    add_property( io_entity_type = lo_entity_type iv_name = 'BANKS' iv_length = 3  iv_key = abap_true ).
    add_property( io_entity_type = lo_entity_type iv_name = 'BANKL' iv_length = 15 iv_key = abap_true ).
    add_property( io_entity_type = lo_entity_type iv_name = 'BANKN' iv_length = 18 iv_key = abap_true ).
    add_property( io_entity_type = lo_entity_type iv_name = 'BVTYP' iv_length = 4 ).
    add_property( io_entity_type = lo_entity_type iv_name = 'KOINH' iv_length = 60 ).

    lo_entity_type->bind_structure( iv_structure_name = 'ZCL_ZOVL_VENDOR_SYNC_MPC=>TY_BANK' ).
    add_entity_set( io_entity_type = lo_entity_type iv_set_name = 'BankSet' ).
  ENDMETHOD.


  METHOD define_associations.
    " Vendor 1 : 0..n CompanyCode / Bank. The navigation property names
    " are the wrapper elements CPI maps the child rows into (deep insert,
    " Sub Levels = 1) - they must match the component names of the DPC's
    " deep structure (TOCOMPANYCODE / TOBANK).
    "
    " Association sets are created EXPLICITLY with the entity set names
    " (same as a SEGW-generated MPC). The entity types have no default
    " entity set, so iv_def_assoc_set = abap_true would not bind the
    " navigation to VendorSet / CompanyCodeSet / BankSet in $metadata, and
    " the CPI OData modeler would not offer ToCompanyCode / ToBank.
    DATA: lo_association TYPE REF TO /iwbep/if_mgw_odata_assoc,
          lo_ref_constr  TYPE REF TO /iwbep/if_mgw_odata_ref_constr.

    "--- Vendor -> CompanyCode ----------------------------------------
    lo_association = model->create_association(
        iv_association_name = 'Vendor_CompanyCode'
        iv_left_type        = 'Vendor'
        iv_right_type       = 'CompanyCode'
        iv_left_card        = /iwbep/if_mgw_med_odata_types=>gcs_cardinality-cardinality_1_1
        iv_right_card       = /iwbep/if_mgw_med_odata_types=>gcs_cardinality-cardinality_0_n
        iv_def_assoc_set    = abap_false ).
    lo_ref_constr = lo_association->create_ref_constraint( ).
    lo_ref_constr->add_property( iv_principal_property = 'LIFNR'
                                 iv_dependent_property = 'LIFNR' ).
    model->create_association_set( iv_association_set_name  = 'Vendor_CompanyCodeSet'
                                   iv_left_entity_set_name  = 'VendorSet'
                                   iv_right_entity_set_name = 'CompanyCodeSet'
                                   iv_association_name      = 'Vendor_CompanyCode' ).

    "--- Vendor -> Bank -----------------------------------------------
    lo_association = model->create_association(
        iv_association_name = 'Vendor_Bank'
        iv_left_type        = 'Vendor'
        iv_right_type       = 'Bank'
        iv_left_card        = /iwbep/if_mgw_med_odata_types=>gcs_cardinality-cardinality_1_1
        iv_right_card       = /iwbep/if_mgw_med_odata_types=>gcs_cardinality-cardinality_0_n
        iv_def_assoc_set    = abap_false ).
    lo_ref_constr = lo_association->create_ref_constraint( ).
    lo_ref_constr->add_property( iv_principal_property = 'LIFNR'
                                 iv_dependent_property = 'LIFNR' ).
    model->create_association_set( iv_association_set_name  = 'Vendor_BankSet'
                                   iv_left_entity_set_name  = 'VendorSet'
                                   iv_right_entity_set_name = 'BankSet'
                                   iv_association_name      = 'Vendor_Bank' ).

    "--- Navigation properties on Vendor ------------------------------
    DATA(lo_vendor_type) = model->get_entity_type( iv_entity_name = 'Vendor' ).
    lo_vendor_type->create_navigation_property(
        iv_property_name    = 'ToCompanyCode'
        iv_abap_fieldname   = 'TOCOMPANYCODE'
        iv_association_name = 'Vendor_CompanyCode' ).
    lo_vendor_type->create_navigation_property(
        iv_property_name    = 'ToBank'
        iv_abap_fieldname   = 'TOBANK'
        iv_association_name = 'Vendor_Bank' ).
  ENDMETHOD.


  METHOD add_property.
    DATA(lo_property) = io_entity_type->create_property( iv_property_name  = CONV #( iv_name )
                                                        iv_abap_fieldname = CONV #( iv_name ) ).
    IF iv_key = abap_true.
      lo_property->set_is_key( ).
    ENDIF.
    lo_property->set_type_edm_string( ).
    lo_property->set_maxlength( iv_max_length = iv_length ).
    " Keys may stay empty in the request (CompanyCode/Bank LIFNR is taken
    " from the vendor header), so everything is nullable.
    lo_property->set_nullable( abap_true ).
    lo_property->set_creatable( abap_true ).
    lo_property->set_updatable( abap_true ).
  ENDMETHOD.


  METHOD add_entity_set.
    DATA(lo_entity_set) = io_entity_type->create_entity_set( iv_entity_set_name = CONV #( iv_set_name ) ).
    lo_entity_set->set_creatable(   abap_true  ).
    lo_entity_set->set_updatable(   abap_false ).
    lo_entity_set->set_deletable(   abap_false ).
    lo_entity_set->set_pageable(    abap_false ).
    lo_entity_set->set_addressable( abap_true  ).
  ENDMETHOD.

ENDCLASS.
