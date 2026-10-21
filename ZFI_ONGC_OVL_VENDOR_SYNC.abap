*&---------------------------------------------------------------------*
*& Report  ZFI_ONGC_OVL_VENDOR_SYNC
*&---------------------------------------------------------------------*
*& ONGC -> OVL Employee Vendor (VEMP) master sync - SENDER (ONGC side)
*&   (Interface OVL-INT-VEN-001, ONGC_OVL_Vendor_Data_Sync_Document.pdf)
*&
*&   Scheduled daily in background (SM36, e.g. 01:00). Selects the
*&   employee vendors created / changed on the previous day and sends
*&   them to the OVL CPI iFlow, which posts every vendor to the OVL
*&   OData service ZOVL_VENDOR_SYNC_SRV (VendorSet, deep insert).
*&
*&   Step 1 : BUT000 - BPs created / changed in the date range
*&            (CRDAT / CHDAT) + CDHDR OBJECTCLAS KRED (vendor changes,
*&            e.g. company code data) in the same range.
*&   Step 2 : account group VEMP only (CVI_VEND_LINK + LFA1-KTOKK,
*&            same result as I_BUSINESSPARTNERSUPPLIER).
*&   Step 3 : vendor must exist in company code OVL (LFB1-BUKRS = OVL).
*&   Step 4 : read LFA1, LFB1 (OVL), LFBK + BUT000 names and build the
*&            JSON payload. Field names = OData property names of
*&            ZOVL_VENDOR_SYNC_SRV, so CPI only splits and forwards.
*&   Send   : HTTP POST to SM59 destination (type G) -> CPI, in packages
*&            of P_PACK vendors. Retry 3 times with backoff on
*&            connection errors / HTTP 5xx.
*&   Log    : ZONGC_OVL_VENLOG (one row per vendor and run) + ALV list
*&            (spool in background). The job ends with an error message
*&            if a package could not be delivered.
*&
*&   Payload sent to CPI (one HTTP call per package):
*&     { "RUN_ID": "...", "PACKAGE": 1, "PACKAGES": 3,
*&       "Vendors": [ { "LIFNR": "...", ..., "ToCompanyCode": [ ... ],
*&                      "ToBank": [ ... ] }, ... ] }
*&
*& DDIC prerequisite : ZONGC_OVL_VENLOG
*& Basis prerequisite: SM59 destination ZOVL_CPI_VENDOR_SYNC (type G)
*&   (see ZFI_ONGC_OVL_VENDOR_SYNC_ONGC_Technical_Guide.docx)
*&---------------------------------------------------------------------*
REPORT zfi_ongc_ovl_vendor_sync.

TABLES: lfa1.

CONSTANTS: gc_objclas_vendor TYPE cdobjectcl VALUE 'KRED',
           gc_max_attempts   TYPE i          VALUE 3.

*----------------------------------------------------------------------*
* Payload structures - component names = OData property names of
* ZOVL_VENDOR_SYNC_SRV (see ZCL_ZOVL_VENDOR_SYNC_MPC).
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_company,
         lifnr TYPE c LENGTH 10,
         bukrs TYPE c LENGTH 4,
         pernr TYPE c LENGTH 8,
         zuawa TYPE c LENGTH 3,
         akont TYPE c LENGTH 10,
         zwels TYPE c LENGTH 10,
         zterm TYPE c LENGTH 4,
         kverm TYPE c LENGTH 30,
       END OF ty_company,
       tt_company TYPE STANDARD TABLE OF ty_company WITH DEFAULT KEY.

TYPES: BEGIN OF ty_bank,
         lifnr TYPE c LENGTH 10,
         banks TYPE c LENGTH 3,
         bankl TYPE c LENGTH 15,
         bankn TYPE c LENGTH 18,
         bvtyp TYPE c LENGTH 4,
         koinh TYPE c LENGTH 60,
       END OF ty_bank,
       tt_bank TYPE STANDARD TABLE OF ty_bank WITH DEFAULT KEY.

TYPES: BEGIN OF ty_vendor,
         lifnr         TYPE c LENGTH 10,
         land1         TYPE c LENGTH 3,
         name1         TYPE c LENGTH 35,
         ort01         TYPE c LENGTH 35,
         ort02         TYPE c LENGTH 35,
         pstlz         TYPE c LENGTH 10,
         regio         TYPE c LENGTH 3,
         sortl         TYPE c LENGTH 10,
         stras         TYPE c LENGTH 60,
         adrnr         TYPE c LENGTH 10,
         mcod1         TYPE c LENGTH 25,
         mcod3         TYPE c LENGTH 25,
         anred         TYPE c LENGTH 15,
         konzs         TYPE c LENGTH 10,
         spras         TYPE c LENGTH 2,
         telf1         TYPE c LENGTH 30,
         gbort         TYPE c LENGTH 25,
         j_1ipanno     TYPE c LENGTH 40,
         name_first    TYPE c LENGTH 40,
         name_last     TYPE c LENGTH 40,
         bp_category   TYPE c LENGTH 1,
         tocompanycode TYPE tt_company,
         tobank        TYPE tt_bank,
       END OF ty_vendor,
       tt_vendor TYPE STANDARD TABLE OF ty_vendor WITH DEFAULT KEY.

TYPES: BEGIN OF ty_package,
         run_id   TYPE sysuuid_c32,
         package  TYPE i,
         packages TYPE i,
         vendors  TYPE tt_vendor,
       END OF ty_package.

TYPES tt_lifnr TYPE SORTED TABLE OF lifnr WITH UNIQUE KEY table_line.

TYPES: BEGIN OF ty_out,
         status    TYPE icon_d,
         lifnr     TYPE lifnr,
         name1     TYPE name1_gp,
         packno    TYPE int4,
         http_code TYPE int4,
         message   TYPE bapi_msg,
       END OF ty_out,
       tt_out TYPE STANDARD TABLE OF ty_out WITH DEFAULT KEY.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  " R_PREV : daily background run - always the previous calendar day,
  "          independent of the dates saved in the variant
  " R_RANGE: manual re-run for a date range (e.g. job missed a day)
  " R_FULL : initial / full load - all VEMP vendors of company code OVL
  PARAMETERS: r_prev  RADIOBUTTON GROUP mode DEFAULT 'X' USER-COMMAND mode,
              r_range RADIOBUTTON GROUP mode,
              r_full  RADIOBUTTON GROUP mode,
              p_from  TYPE sy-datum MODIF ID rng,
              p_to    TYPE sy-datum MODIF ID rng.
  SELECT-OPTIONS s_lifnr FOR lfa1-lifnr.
  PARAMETERS: p_bukrs TYPE bukrs DEFAULT 'OVL' OBLIGATORY,
              p_ktokk TYPE ktokk DEFAULT 'VEMP' OBLIGATORY,
              p_skdel AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.
  PARAMETERS: p_dest TYPE rfcdest DEFAULT 'ZOVL_CPI_VENDOR_SYNC' OBLIGATORY,
              p_pack TYPE i DEFAULT 100 OBLIGATORY,
              p_test AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b02.

*----------------------------------------------------------------------*
CLASS lcl_sync DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS run.

  PRIVATE SECTION.
    DATA mv_run_id TYPE sysuuid_c32.
    DATA mt_out    TYPE tt_out.
    DATA mv_failed TYPE abap_bool.

    METHODS select_vendors
      RETURNING VALUE(rt_lifnr) TYPE tt_lifnr.
    METHODS build_payload
      IMPORTING it_lifnr          TYPE tt_lifnr
      RETURNING VALUE(rt_vendor) TYPE tt_vendor.
    METHODS to_json
      IMPORTING is_package     TYPE ty_package
      RETURNING VALUE(rv_json) TYPE string.
    METHODS send_package
      IMPORTING iv_json      TYPE string
                iv_packno    TYPE i
      EXPORTING ev_http_code TYPE i
                ev_message   TYPE bapi_msg.
    METHODS add_result
      IMPORTING it_vendor    TYPE tt_vendor
                iv_packno    TYPE i
                iv_http_code TYPE i
                iv_status    TYPE bapi_mtype
                iv_message   TYPE bapi_msg.
    METHODS write_log.
    METHODS display.
ENDCLASS.


CLASS lcl_sync IMPLEMENTATION.

  METHOD run.
    DATA: ls_package TYPE ty_package,
          lv_code    TYPE i,
          lv_msg     TYPE bapi_msg.

    TRY.
        mv_run_id = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error.
        mv_run_id = |{ sy-datum }{ sy-uzeit }|.
    ENDTRY.

    "--- Steps 1 - 3: which vendors
    DATA(lt_lifnr) = select_vendors( ).
    IF lt_lifnr IS INITIAL.
      MESSAGE 'No records to sync' TYPE 'S'.
      RETURN.
    ENDIF.

    "--- Step 4: payload
    DATA(lt_vendor) = build_payload( lt_lifnr ).

    "--- Send in packages
    DATA(lv_total) = lines( lt_vendor ).
    ls_package-run_id   = mv_run_id.
    ls_package-packages = ( lv_total + p_pack - 1 ) DIV p_pack.

    DATA(lv_from) = 1.
    WHILE lv_from <= lv_total.
      ls_package-package = ls_package-package + 1.
      CLEAR ls_package-vendors.
      DATA(lv_to) = nmin( val1 = lv_from + p_pack - 1 val2 = lv_total ).
      APPEND LINES OF lt_vendor FROM lv_from TO lv_to TO ls_package-vendors.
      lv_from = lv_to + 1.

      IF p_test = abap_true.
        add_result( it_vendor = ls_package-vendors iv_packno = ls_package-package
                    iv_http_code = 0 iv_status = 'W'
                    iv_message = 'Test run - selected, not sent' ).
        CONTINUE.
      ENDIF.

      send_package( EXPORTING iv_json      = to_json( ls_package )
                              iv_packno    = ls_package-package
                    IMPORTING ev_http_code = lv_code
                              ev_message   = lv_msg ).
      IF lv_code BETWEEN 200 AND 299.
        add_result( it_vendor = ls_package-vendors iv_packno = ls_package-package
                    iv_http_code = lv_code iv_status = 'S' iv_message = lv_msg ).
      ELSE.
        mv_failed = abap_true.
        add_result( it_vendor = ls_package-vendors iv_packno = ls_package-package
                    iv_http_code = lv_code iv_status = 'E' iv_message = lv_msg ).
      ENDIF.
    ENDWHILE.

    IF p_test = abap_false.
      write_log( ).
    ENDIF.
    display( ).

    IF mv_failed = abap_true.
      " Cancelled job -> visible in SM37 / job monitoring
      IF sy-batch = abap_true.
        MESSAGE 'Vendor sync to OVL CPI failed for at least one package - see ZONGC_OVL_VENLOG' TYPE 'E'.
      ELSE.
        MESSAGE 'Vendor sync to OVL CPI failed for at least one package' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD select_vendors.
    TYPES: BEGIN OF ty_bp,
             partner_guid TYPE bu_partner_guid,
           END OF ty_bp.
    DATA: lt_bp        TYPE STANDARD TABLE OF ty_bp WITH DEFAULT KEY,
          lt_candidate TYPE tt_lifnr,
          lv_lifnr     TYPE lifnr.

    IF r_full = abap_true.
      "--- Initial / full load: all VEMP vendors of the OVL company code
      SELECT a~lifnr
        FROM lfa1 AS a
        INNER JOIN lfb1 AS b ON b~lifnr = a~lifnr
        WHERE a~lifnr IN @s_lifnr
          AND a~ktokk =  @p_ktokk
          AND b~bukrs =  @p_bukrs
        INTO TABLE @DATA(lt_full).
      LOOP AT lt_full INTO DATA(ls_full).
        INSERT ls_full-lifnr INTO TABLE lt_candidate.
      ENDLOOP.
    ELSE.
      "--- Step 1a: BPs created / changed in the range (BUT000)
      SELECT partner_guid FROM but000
        WHERE ( chdat BETWEEN @p_from AND @p_to )
           OR ( crdat BETWEEN @p_from AND @p_to )
        INTO TABLE @lt_bp.

      IF lt_bp IS NOT INITIAL.
        SELECT vendor FROM cvi_vend_link
          FOR ALL ENTRIES IN @lt_bp
          WHERE partner_guid = @lt_bp-partner_guid
          INTO TABLE @DATA(lt_link).
        LOOP AT lt_link INTO DATA(ls_link).
          INSERT ls_link-vendor INTO TABLE lt_candidate.
        ENDLOOP.
      ENDIF.

      "--- Step 1b: vendor change documents (LFA1/LFB1/LFBK) in the range
      SELECT DISTINCT objectid FROM cdhdr
        WHERE objectclas = @gc_objclas_vendor
          AND udate BETWEEN @p_from AND @p_to
        INTO TABLE @DATA(lt_cdhdr).
      LOOP AT lt_cdhdr INTO DATA(ls_cdhdr).
        lv_lifnr = ls_cdhdr-objectid.
        INSERT lv_lifnr INTO TABLE lt_candidate.
      ENDLOOP.

      DELETE lt_candidate WHERE table_line NOT IN s_lifnr.
    ENDIF.

    IF lt_candidate IS INITIAL.
      RETURN.
    ENDIF.

    "--- Steps 2 + 3: account group VEMP and extended to company code OVL
    SELECT a~lifnr, a~loevm AS loevm_a, b~loevm AS loevm_b
      FROM lfa1 AS a
      INNER JOIN lfb1 AS b ON b~lifnr = a~lifnr
      FOR ALL ENTRIES IN @lt_candidate
      WHERE a~lifnr = @lt_candidate-table_line
        AND a~ktokk = @p_ktokk
        AND b~bukrs = @p_bukrs
      INTO TABLE @DATA(lt_valid).

    LOOP AT lt_valid INTO DATA(ls_valid).
      IF p_skdel = abap_true AND ( ls_valid-loevm_a = abap_true OR ls_valid-loevm_b = abap_true ).
        CONTINUE.   " marked for deletion - not sent
      ENDIF.
      INSERT ls_valid-lifnr INTO TABLE rt_lifnr.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_payload.
    DATA ls_vendor TYPE ty_vendor.

    "--- General data (LFA1) + BP name / category (BUT000)
    SELECT a~lifnr, a~land1, a~name1, a~ort01, a~ort02, a~pstlz, a~regio,
           a~sortl, a~stras, a~adrnr, a~mcod1, a~mcod3, a~anred, a~konzs,
           a~spras, a~telf1, a~gbort, a~j_1ipanno,
           p~name_first, p~name_last, p~type
      FROM lfa1 AS a
      LEFT OUTER JOIN cvi_vend_link AS l ON l~vendor = a~lifnr
      LEFT OUTER JOIN but000 AS p ON p~partner_guid = l~partner_guid
      FOR ALL ENTRIES IN @it_lifnr
      WHERE a~lifnr = @it_lifnr-table_line
      INTO TABLE @DATA(lt_lfa1).
    SORT lt_lfa1 BY lifnr.

    "--- Company code data (LFB1) - OVL only
    SELECT lifnr, bukrs, pernr, zuawa, akont, zwels, zterm, kverm
      FROM lfb1
      FOR ALL ENTRIES IN @it_lifnr
      WHERE lifnr = @it_lifnr-table_line
        AND bukrs = @p_bukrs
      INTO TABLE @DATA(lt_lfb1).
    SORT lt_lfb1 BY lifnr.

    "--- Bank details (LFBK)
    SELECT lifnr, banks, bankl, bankn, bvtyp, koinh
      FROM lfbk
      FOR ALL ENTRIES IN @it_lifnr
      WHERE lifnr = @it_lifnr-table_line
      INTO TABLE @DATA(lt_lfbk).
    SORT lt_lfbk BY lifnr bvtyp.

    LOOP AT lt_lfa1 INTO DATA(ls_lfa1).
      CLEAR ls_vendor.
      ls_vendor = CORRESPONDING #( ls_lfa1 ).
      " Language as ISO code (E -> EN), the OVL service accepts both
      CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
        EXPORTING
          input  = ls_lfa1-spras
        IMPORTING
          output = ls_vendor-spras.
      ls_vendor-bp_category = ls_lfa1-type.

      LOOP AT lt_lfb1 INTO DATA(ls_lfb1) WHERE lifnr = ls_lfa1-lifnr.
        APPEND CORRESPONDING ty_company( ls_lfb1 ) TO ls_vendor-tocompanycode.
      ENDLOOP.
      LOOP AT lt_lfbk INTO DATA(ls_lfbk) WHERE lifnr = ls_lfa1-lifnr.
        APPEND CORRESPONDING ty_bank( ls_lfbk ) TO ls_vendor-tobank.
      ENDLOOP.

      APPEND ls_vendor TO rt_vendor.
    ENDLOOP.
  ENDMETHOD.


  METHOD to_json.
    " Field names in upper case (= OData property names); only the
    " navigation properties and the package wrapper need mixed case.
    rv_json = /ui2/cl_json=>serialize( data        = is_package
                                       compress    = abap_false
                                       pretty_name = /ui2/cl_json=>pretty_mode-none ).
    REPLACE ALL OCCURRENCES OF `"TOCOMPANYCODE":` IN rv_json WITH `"ToCompanyCode":`.
    REPLACE ALL OCCURRENCES OF `"TOBANK":`        IN rv_json WITH `"ToBank":`.
    REPLACE ALL OCCURRENCES OF `"VENDORS":`       IN rv_json WITH `"Vendors":`.
  ENDMETHOD.


  METHOD send_package.
    DATA: lo_http   TYPE REF TO if_http_client,
          lv_reason TYPE string,
          lv_resp   TYPE string,
          lv_wait   TYPE i.

    DATA(lv_body) = cl_abap_codepage=>convert_to( source = iv_json codepage = `UTF-8` ).

    DO gc_max_attempts TIMES.
      DATA(lv_attempt) = sy-index.
      CLEAR: ev_http_code, ev_message, lv_reason, lv_resp.

      cl_http_client=>create_by_destination(
        EXPORTING
          destination              = p_dest
        IMPORTING
          client                   = lo_http
        EXCEPTIONS
          argument_not_found       = 1
          destination_not_found    = 2
          destination_no_authority = 3
          plugin_not_active        = 4
          internal_error           = 5
          OTHERS                   = 6 ).
      IF sy-subrc <> 0.
        ev_message = |SM59 destination { p_dest } not usable (create_by_destination rc { sy-subrc })|.
        RETURN.   " configuration error - retry does not help
      ENDIF.

      lo_http->request->set_method( if_http_request=>co_request_method_post ).
      lo_http->request->set_header_field( name = 'Content-Type' value = 'application/json; charset=utf-8' ).
      lo_http->request->set_header_field( name = 'Accept'       value = 'application/json' ).
      lo_http->request->set_header_field( name = 'X-Run-Id'     value = CONV #( mv_run_id ) ).
      lo_http->request->set_header_field( name = 'X-Package'    value = |{ iv_packno }| ).
      lo_http->request->set_data( lv_body ).

      lo_http->send( EXCEPTIONS http_communication_failure = 1
                                http_invalid_state         = 2
                                http_processing_failed     = 3
                                http_invalid_timeout       = 4
                                OTHERS                     = 5 ).
      IF sy-subrc = 0.
        lo_http->receive( EXCEPTIONS http_communication_failure = 1
                                     http_invalid_state         = 2
                                     http_processing_failed     = 3
                                     OTHERS                     = 4 ).
      ENDIF.

      IF sy-subrc = 0.
        lo_http->response->get_status( IMPORTING code = ev_http_code reason = lv_reason ).
        lv_resp = lo_http->response->get_cdata( ).
        ev_message = |HTTP { ev_http_code } { lv_reason } (attempt { lv_attempt }) { lv_resp }|.
      ELSE.
        lo_http->get_last_error( IMPORTING message = lv_reason ).
        ev_message = |Connection error (attempt { lv_attempt }): { lv_reason }|.
      ENDIF.
      lo_http->close( ).

      " Done on success and on 4xx (data / config error - retry does not help)
      IF ev_http_code BETWEEN 200 AND 499.
        RETURN.
      ENDIF.

      " Connection error / 5xx: exponential backoff 10s, 20s, ...
      IF lv_attempt < gc_max_attempts.
        lv_wait = 10 * ipow( base = 2 exp = lv_attempt - 1 ).
        WAIT UP TO lv_wait SECONDS.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD add_result.
    LOOP AT it_vendor INTO DATA(ls_vendor).
      APPEND VALUE #( status    = SWITCH #( iv_status WHEN 'S' THEN icon_green_light
                                                      WHEN 'W' THEN icon_yellow_light
                                                      ELSE icon_red_light )
                      lifnr     = ls_vendor-lifnr
                      name1     = ls_vendor-name1
                      packno    = iv_packno
                      http_code = iv_http_code
                      message   = iv_message ) TO mt_out.
    ENDLOOP.
  ENDMETHOD.


  METHOD write_log.
    DATA lt_log TYPE STANDARD TABLE OF zongc_ovl_venlog WITH DEFAULT KEY.

    LOOP AT mt_out INTO DATA(ls_out).
      APPEND VALUE #( run_id    = mv_run_id
                      lifnr     = ls_out-lifnr
                      erdat     = sy-datum
                      erzet     = sy-uzeit
                      packno    = ls_out-packno
                      http_code = ls_out-http_code
                      status    = COND #( WHEN ls_out-status = icon_green_light THEN 'S' ELSE 'E' )
                      message   = ls_out-message
                      date_from = COND #( WHEN r_full = abap_false THEN p_from )
                      date_to   = COND #( WHEN r_full = abap_false THEN p_to )
                      ernam     = sy-uname ) TO lt_log.
    ENDLOOP.
    MODIFY zongc_ovl_venlog FROM TABLE lt_log.
    COMMIT WORK.
  ENDMETHOD.


  METHOD display.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                                CHANGING  t_table      = mt_out ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->get_columns( )->set_optimize( abap_true ).
        lo_alv->get_display_settings( )->set_list_header(
          |ONGC -> OVL vendor sync  { COND string( WHEN p_test = abap_true THEN `(TEST RUN)` ) }  Run { mv_run_id }| ).
        lo_alv->display( ).
      CATCH cx_salv_msg.
        MESSAGE 'ALV display failed' TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
INITIALIZATION.
  p_from = sy-datum - 1.
  p_to   = sy-datum - 1.

AT SELECTION-SCREEN OUTPUT.
  " Date range is only input-ready in mode "date range"
  LOOP AT SCREEN.
    IF screen-group1 = 'RNG'.
      screen-input = COND #( WHEN r_range = abap_true THEN 1 ELSE 0 ).
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  IF r_range = abap_true.
    IF p_from IS INITIAL OR p_to IS INITIAL.
      MESSAGE 'Enter the change date range' TYPE 'E'.
    ENDIF.
    IF p_to < p_from.
      MESSAGE 'Date to must not be before date from' TYPE 'E'.
    ENDIF.
  ENDIF.
  IF p_pack < 1 OR p_pack > 1000.
    MESSAGE 'Package size must be between 1 and 1000' TYPE 'E'.
  ENDIF.

START-OF-SELECTION.
  " Daily mode: previous calendar day (document: CHDAT = SY-DATUM - 1).
  " Set here, so a variant saved on another day still takes "yesterday".
  IF r_prev = abap_true.
    p_from = sy-datum - 1.
    p_to   = sy-datum - 1.
  ENDIF.
  NEW lcl_sync( )->run( ).
