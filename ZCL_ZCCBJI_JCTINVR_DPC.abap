*&---------------------------------------------------------------------*
*& Class  ZCL_ZCCBJI_JCTINVR_DPC  (Data Provider - code based, OData V2)
*&---------------------------------------------------------------------*
*& Inbound NTA invoice-registration-number service for SAP CPI (CCBJI).
*&
*& CPI POSTs ONE deep entity per call - header InvoiceRegistrations
*& wrapping MULTIPLE InvoiceRegistration items (one per NTA record).
*& The whole batch arrives in a single CREATE_DEEP_ENTITY call and is
*& processed in one LUW (all-or-nothing per POST).
*&
*& Compare logic (Option A - literal port of the AS-IS ABInitio job):
*&   - match field: incoming INVOICE_CD (registered number) against
*&     /CCBJI/T_JCTINVR-INVOICE_CD
*&   - FOUND      -> write the incoming record back over the existing
*&                   row (UPDATE: business fields from payload, original
*&                   created-by audit fields preserved, changed-by audit
*&                   fields stamped, ZUPDIND = 'U')
*&   - NOT found  -> skip (no insert), counted in the response summary
*&
*& Response: header REQUEST_ID echoes 'U:<updated> S:<skipped>'.
*& Errors (invalid date, empty batch, DB failure, missing authority)
*& roll back the whole chunk -> CPI can safely retry.
*&
*& Fully code based -> extends /IWBEP/CL_MGW_ABS_DATA (same base as
*& ZCL_ZTABLE_META_DPC). Register with ZCL_ZCCBJI_JCTINVR_MPC via
*& /IWBEP/REG_SERVICE + /IWFND/MAINT_SERVICE.
*&---------------------------------------------------------------------*
CLASS zcl_zccbji_jctinvr_dpc DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_mgw_abs_data
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~create_deep_entity REDEFINITION.

  PROTECTED SECTION.
    CONSTANTS gc_tabname TYPE tabname VALUE '/CCBJI/T_JCTINVR'.

    " Deep structure received from CPI: header fields + item table.
    " The component name INVOICEREGISTRATION must equal the navigation
    " property name defined in the MPC.
    TYPES: BEGIN OF ty_deep.
             INCLUDE TYPE zcl_zccbji_jctinvr_mpc=>ty_header.
    TYPES:   invoiceregistration TYPE zcl_zccbji_jctinvr_mpc=>tt_item.
    TYPES: END OF ty_deep.

    " Check change authorisation for the table (S_TABU_NAM, ACTVT 02).
    METHODS check_table_authority
      RAISING /iwbep/cx_mgw_busi_exception.

    " Normalise an incoming date string (YYYYMMDD / YYYY-MM-DD /
    " DD.MM.YYYY / empty) to DATS; raises on anything unparseable.
    METHODS conv_date
      IMPORTING iv_in          TYPE clike
                iv_field       TYPE string
                iv_invoice_cd  TYPE clike
      RETURNING VALUE(rv_date) TYPE dats
      RAISING   /iwbep/cx_mgw_busi_exception.

    " Raise a business exception with a single message text.
    METHODS raise_error
      IMPORTING iv_text TYPE string
      RAISING   /iwbep/cx_mgw_busi_exception.
ENDCLASS.


CLASS zcl_zccbji_jctinvr_dpc IMPLEMENTATION.

  METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.

    DATA: ls_deep    TYPE ty_deep,
          lt_items   TYPE zcl_zccbji_jctinvr_mpc=>tt_item,
          lt_update  TYPE STANDARD TABLE OF /ccbji/t_jctinvr,
          ls_row     TYPE /ccbji/t_jctinvr,
          lv_updated TYPE i,
          lv_skipped TYPE i,
          lv_tabix   TYPE sy-tabix.

    "--------------------------------------------------------------
    " 1. Read the whole batch (header + all items) in one call
    "--------------------------------------------------------------
    io_data_provider->read_entry_data( IMPORTING es_data = ls_deep ).

    IF ls_deep-invoiceregistration IS INITIAL.
      me->raise_error( |Empty batch: no InvoiceRegistration items in the request.| ).
    ENDIF.

    me->check_table_authority( ).

    "--------------------------------------------------------------
    " 2. Validate + normalise; duplicates in the batch: last wins
    "--------------------------------------------------------------
    LOOP AT ls_deep-invoiceregistration ASSIGNING FIELD-SYMBOL(<ls_in>).
      lv_tabix = sy-tabix.
      CONDENSE <ls_in>-invoice_cd NO-GAPS.
      TRANSLATE <ls_in>-invoice_cd TO UPPER CASE.
      IF <ls_in>-invoice_cd IS INITIAL.
        me->raise_error( |Record { lv_tabix }: INVOICE_CD is empty.| ).
      ENDIF.
      DELETE lt_items WHERE invoice_cd = <ls_in>-invoice_cd.
      APPEND <ls_in> TO lt_items.
    ENDLOOP.

    "--------------------------------------------------------------
    " 3. Fetch the existing rows for all incoming numbers (one SELECT)
    "--------------------------------------------------------------
    SELECT * FROM /ccbji/t_jctinvr
      INTO TABLE @DATA(lt_existing)
      FOR ALL ENTRIES IN @lt_items
      WHERE invoice_cd = @lt_items-invoice_cd.
    SORT lt_existing BY invoice_cd.

    "--------------------------------------------------------------
    " 4. Compare (Option A): FOUND -> write payload back over the row,
    "    NOT found -> skip. No field-by-field change detection.
    "--------------------------------------------------------------
    LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).

      READ TABLE lt_existing INTO DATA(ls_exist)
           WITH KEY invoice_cd = <ls_item>-invoice_cd BINARY SEARCH.
      IF sy-subrc <> 0.
        " Registered number not in /CCBJI/T_JCTINVR -> no insert (AS-IS logic)
        lv_skipped = lv_skipped + 1.
        CONTINUE.
      ENDIF.

      " Start from the existing row: keeps MANDT and the original
      " created-by audit fields (ZERNAM/ZERSDA/ZERZZT).
      ls_row = ls_exist.

      " Business fields from the payload ("same format" as the table)
      ls_row-process_kbn     = <ls_item>-process_kbn.
      ls_row-correction_kbn  = <ls_item>-correction_kbn.
      ls_row-personal_kbn    = <ls_item>-personal_kbn.
      ls_row-domestic_kbn    = <ls_item>-domestic_kbn.
      ls_row-latest_kbn      = <ls_item>-latest_kbn.
      ls_row-create_date     = me->conv_date( iv_in         = <ls_item>-create_date
                                              iv_field      = 'CREATE_DATE'
                                              iv_invoice_cd = <ls_item>-invoice_cd ).
      ls_row-update_date     = me->conv_date( iv_in         = <ls_item>-update_date
                                              iv_field      = 'UPDATE_DATE'
                                              iv_invoice_cd = <ls_item>-invoice_cd ).
      ls_row-revocation_date = me->conv_date( iv_in         = <ls_item>-revocation_date
                                              iv_field      = 'REVOCATION_DATE'
                                              iv_invoice_cd = <ls_item>-invoice_cd ).
      ls_row-expiration_date = me->conv_date( iv_in         = <ls_item>-expiration_date
                                              iv_field      = 'EXPIRATION_DATE'
                                              iv_invoice_cd = <ls_item>-invoice_cd ).

      " Changed-by audit fields (/CCEJ/MDM_DELTA include)
      ls_row-zaenam  = sy-uname.
      ls_row-zupdat  = sy-datum.
      ls_row-zuptim  = sy-uzeit.
      ls_row-zupdind = 'U'.

      APPEND ls_row TO lt_update.
      lv_updated = lv_updated + 1.
    ENDLOOP.

    "--------------------------------------------------------------
    " 5. Write in a single LUW; all-or-nothing per POST
    "--------------------------------------------------------------
    IF lt_update IS NOT INITIAL.
      MODIFY /ccbji/t_jctinvr FROM TABLE lt_update.
      IF sy-subrc <> 0.
        ROLLBACK WORK.
        me->raise_error( |Database update of { gc_tabname } failed.| ).
      ENDIF.
      COMMIT WORK AND WAIT.
    ENDIF.

    "--------------------------------------------------------------
    " 6. Response: echo the processing summary in the header key
    "--------------------------------------------------------------
    DATA ls_head TYPE zcl_zccbji_jctinvr_mpc=>ty_header.
    ls_head-request_id = |U:{ lv_updated } S:{ lv_skipped }|.
    copy_data_to_ref( EXPORTING is_data = ls_head
                      CHANGING  cr_data = er_deep_entity ).

  ENDMETHOD.


  METHOD check_table_authority.
    AUTHORITY-CHECK OBJECT 'S_TABU_NAM'
      ID 'ACTVT' FIELD '02'
      ID 'TABLE' FIELD gc_tabname.
    IF sy-subrc <> 0.
      me->raise_error( |No change authorisation (S_TABU_NAM) for table { gc_tabname }.| ).
    ENDIF.
  ENDMETHOD.


  METHOD conv_date.
    DATA lv_in TYPE string.

    lv_in = iv_in.
    CONDENSE lv_in NO-GAPS.

    " Empty / zero dates are valid (field not maintained by NTA)
    IF lv_in IS INITIAL OR lv_in = '00000000' OR lv_in = '0000-00-00'.
      rv_date = '00000000'.
      RETURN.
    ENDIF.

    IF lv_in CA '.'.
      " DD.MM.YYYY
      IF strlen( lv_in ) = 10.
        rv_date = lv_in+6(4) && lv_in+3(2) && lv_in+0(2).
      ENDIF.
    ELSE.
      " YYYY-MM-DD -> YYYYMMDD
      REPLACE ALL OCCURRENCES OF '-' IN lv_in WITH ''.
      IF strlen( lv_in ) = 8 AND lv_in CO '0123456789'.
        rv_date = lv_in.
      ENDIF.
    ENDIF.

    IF rv_date IS INITIAL.
      me->raise_error( |{ iv_invoice_cd }: { iv_field } '{ iv_in }' is not a valid date | &&
                       |(expected YYYYMMDD, YYYY-MM-DD or DD.MM.YYYY).| ).
    ENDIF.

    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = rv_date
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
    IF sy-subrc <> 0.
      me->raise_error( |{ iv_invoice_cd }: { iv_field } '{ iv_in }' is not a plausible date.| ).
    ENDIF.
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
