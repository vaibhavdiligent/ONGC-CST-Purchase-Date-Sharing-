*&---------------------------------------------------------------------*
*& Class  ZCL_ZOVL_VENDOR_SYNC_DPC  (Data Provider - code based, OData V2)
*&---------------------------------------------------------------------*
*& Inbound Employee Vendor (VEMP) master sync ONGC -> OVL
*& (Interface OVL-INT-VEN-001).
*&
*& CPI POSTs ONE deep entity per vendor to VendorSet: Vendor (LFA1)
*& with navigation ToCompanyCode (LFB1) and ToBank (LFBK). The vendor is
*& created (same number as ONGC) or updated by ZCL_OVL_VENDOR_SYNC.
*&
*& Response (acknowledgement for CPI):
*&   success -> HTTP 201, the posted vendor echoed with
*&              STATUS = 'S', ACTION = CREATE / UPDATE, MESSAGE
*&   error   -> HTTP 400 with the CVI / validation messages; nothing is
*&              saved, CPI flags the record / raises the alert.
*& Every call is written to ZOVL_VEN_SYNCLOG (success and error).
*&
*& One vendor = one LUW (the business class commits), so CPI must send
*& one POST per vendor and NOT use $batch.
*&
*& Fully code based -> extends /IWBEP/CL_MGW_ABS_DATA (same base as
*& ZCL_ZCCBJI_JCTINVR_DPC). Register with ZCL_ZOVL_VENDOR_SYNC_MPC via
*& /IWBEP/REG_SERVICE + /IWFND/MAINT_SERVICE.
*&---------------------------------------------------------------------*
CLASS zcl_zovl_vendor_sync_dpc DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_mgw_abs_data
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~create_deep_entity REDEFINITION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~create_entity      REDEFINITION.

  PROTECTED SECTION.
    " Deep structure received from CPI: vendor fields + child tables.
    " Component names must equal the navigation properties' ABAP field
    " names defined in the MPC (TOCOMPANYCODE / TOBANK).
    TYPES: BEGIN OF ty_deep.
             INCLUDE TYPE zcl_zovl_vendor_sync_mpc=>ty_vendor.
    TYPES:   tocompanycode TYPE zcl_zovl_vendor_sync_mpc=>tt_company,
             tobank        TYPE zcl_zovl_vendor_sync_mpc=>tt_bank,
           END OF ty_deep.

    " Runs the upsert, logs it and raises on error
    METHODS process_vendor
      IMPORTING is_vendor        TYPE zcl_zovl_vendor_sync_mpc=>ty_vendor
                it_company       TYPE zcl_zovl_vendor_sync_mpc=>tt_company
                it_bank          TYPE zcl_zovl_vendor_sync_mpc=>tt_bank
      RETURNING VALUE(rs_vendor) TYPE zcl_zovl_vendor_sync_mpc=>ty_vendor
      RAISING   /iwbep/cx_mgw_busi_exception.

    " Raise a business exception with a single message text
    METHODS raise_error
      IMPORTING iv_text TYPE string
      RAISING   /iwbep/cx_mgw_busi_exception.
ENDCLASS.


CLASS zcl_zovl_vendor_sync_dpc IMPLEMENTATION.

  METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.
    DATA ls_deep TYPE ty_deep.

    IF iv_entity_set_name <> 'VendorSet'.
      me->raise_error( |Deep insert is only supported on VendorSet (called: { iv_entity_set_name }).| ).
    ENDIF.

    "--- 1. Read the vendor with its company code + bank rows
    io_data_provider->read_entry_data( IMPORTING es_data = ls_deep ).

    "--- 2. Create / update; raises (HTTP 400) on error
    DATA(ls_vendor) = me->process_vendor( is_vendor  = CORRESPONDING #( ls_deep )
                                          it_company = ls_deep-tocompanycode
                                          it_bank    = ls_deep-tobank ).

    "--- 3. Echo the posted vendor with the result fields
    ls_deep-lifnr   = ls_vendor-lifnr.
    ls_deep-status  = ls_vendor-status.
    ls_deep-action  = ls_vendor-action.
    ls_deep-message = ls_vendor-message.
    copy_data_to_ref( EXPORTING is_data = ls_deep
                      CHANGING  cr_data = er_deep_entity ).
  ENDMETHOD.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_entity.
    " Flat POST to VendorSet (no company code / bank rows) - e.g. a
    " general-data-only change. The children cannot be posted alone.
    DATA ls_vendor TYPE zcl_zovl_vendor_sync_mpc=>ty_vendor.

    IF iv_entity_set_name <> 'VendorSet'.
      me->raise_error( |{ iv_entity_set_name } cannot be posted alone - post it inside VendorSet.| ).
    ENDIF.

    io_data_provider->read_entry_data( IMPORTING es_data = ls_vendor ).

    ls_vendor = me->process_vendor( is_vendor  = ls_vendor
                                    it_company = VALUE #( )
                                    it_bank    = VALUE #( ) ).

    copy_data_to_ref( EXPORTING is_data = ls_vendor
                      CHANGING  cr_data = er_entity ).
  ENDMETHOD.


  METHOD process_vendor.
    DATA(lo_sync) = NEW zcl_ovl_vendor_sync( ).

    DATA(ls_result) = lo_sync->upsert( is_vendor  = is_vendor
                                       it_company = it_company
                                       it_bank    = it_bank ).
    lo_sync->write_log( ls_result ).

    IF ls_result-status = 'E'.
      DATA(lo_mc) = mo_context->get_message_container( ).
      lo_mc->add_messages_from_bapi( it_bapi_messages = ls_result-messages ).
      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          message_container = lo_mc.
    ENDIF.

    rs_vendor         = is_vendor.
    rs_vendor-lifnr   = ls_result-lifnr.
    rs_vendor-status  = ls_result-status.
    rs_vendor-action  = ls_result-action.
    rs_vendor-message = ls_result-message.
  ENDMETHOD.


  METHOD raise_error.
    " iv_msg_text is typed C(220) (BAPI_MSG) in the message container API,
    " so convert the string into a fixed-length field before passing it.
    DATA lv_msg TYPE bapi_msg.
    lv_msg = iv_text.
    DATA(lo_mc) = mo_context->get_message_container( ).
    lo_mc->add_message_text_only( iv_msg_type = 'E'
                                  iv_msg_text = lv_msg ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING message_container = lo_mc.
  ENDMETHOD.

ENDCLASS.
