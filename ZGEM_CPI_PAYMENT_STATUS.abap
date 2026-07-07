*&---------------------------------------------------------------------*
*& Report ZGEM_CPI_PAYMENT_STATUS
*&---------------------------------------------------------------------*
*& Payment Status (3.11) - GeM CPI integration.
*& Same approach as ZGEM_CPI_ORDER_SUMMARY:
*&   1. Generate SEK token via proxy ZGEM_TOKENCO_SI_SECURITY_TOKEN.
*&   2. Call CPI through SM59 destination CPI_HTTP_GEM.
*&   3. Path -> CPI derives CamelHttpPath (sender endpoint must end with /*).
*&   4. POST JSON body; SEK token sent as header 'token' = Bearer <token>.
*&   5. Parse response and show it as an ALV grid.
*&      (ALV list header is taken from selection-screen field p_head.)
*&
*& Real response shape (confirmed from a live GeM call, 2026-07-07):
*&   {"transactionID":null,"status":"fail","paymentMode":null,"message":"Invalid transactionID"}
*&   - a FLAT object, NOT the generic Status/Iat/data{Sub,Aud,Iss} envelope
*&   used by the other Sync APIs. Field names are camelCase.
*&
*& Real request "paydata" shape confirmed from the GeM API spec (2026-07-07)
*& and a live sample - sent as a PLAIN (unencrypted) JSON object, same as the
*& other Sync APIs (no AES/encryption is applied here, per instruction):
*&   {"transactionID":..,"status":"Success","paymentBy":"NEFT",
*&    "contractNo":..,"gemInvoiceNo":..,"invoiceNo":..,"billNo":..,
*&    "billAmountPaid":10,"transactionDate":"2025-01-02",
*&    "deductionType":"NA","bankName":..,"bankTransactionNo":..,
*&    "sanctions":"10.00","sanctionDate":"2024-11-19"}
*&   deductedAmount/chequeNumber/demandDraftNo are also part of the spec but
*&   were absent from the confirmed live sample (likely only sent when
*&   applicable to the payment mode) - selection fields provided for them too.
*&---------------------------------------------------------------------*
REPORT zgem_cpi_payment_status.

CONSTANTS: c_dest TYPE rfcdest VALUE 'CPI_HTTP_GEM'.

PARAMETERS:
            p_head   TYPE char70 LOWER CASE DEFAULT 'Payment Status (3.11)', " ALV list header (editable)
            p_user   TYPE string LOWER CASE DEFAULT 'CLIENT_NAME',
            p_txnid  TYPE string LOWER CASE DEFAULT '173200477772237507927',
            p_status TYPE string LOWER CASE DEFAULT 'Success',
            p_paymby TYPE string LOWER CASE DEFAULT 'NEFT',
            p_contno TYPE string LOWER CASE DEFAULT 'GEMC-511687737507927',
            p_geminv TYPE string LOWER CASE DEFAULT 'GEM-950165',
            p_invno  TYPE string LOWER CASE DEFAULT 'sdf578',
            p_billno TYPE string LOWER CASE DEFAULT '511687737507927-2B1',
            p_bamt   TYPE p LENGTH 9 DECIMALS 0 DEFAULT 10,
            p_txndt  TYPE string LOWER CASE DEFAULT '2025-01-02',
            p_dedamt TYPE p LENGTH 9 DECIMALS 0,
            p_dedtyp TYPE string LOWER CASE DEFAULT 'NA',
            p_bank   TYPE string LOWER CASE DEFAULT 'ICICI BANK',
            p_chqno  TYPE string LOWER CASE,
            p_bktxno TYPE string LOWER CASE DEFAULT 'UTI657454690',
            p_ddno   TYPE string LOWER CASE,
            p_sanc   TYPE string LOWER CASE DEFAULT '10.00',
            p_sancdt TYPE string LOWER CASE DEFAULT '2024-11-19',
            p_path   TYPE string LOWER CASE DEFAULT '/http/GEM/Sync/PaymentStatus'.

*--- Token proxy objects (same pattern as the summary program)
DATA: lo_gem_token     TYPE REF TO zgem_tokenco_si_security_token,
      proxy_data       TYPE zgem_tokenmt_security_token_se,
      lt_input         TYPE zgem_tokenmt_security_token_re,
      lo_sys_exception TYPE REF TO cx_ai_system_fault,
      err_string       TYPE string,
      gv_token         TYPE string.

*--- paydata payload (Section Payment Status (3.11))
TYPES: BEGIN OF ty_paydata,
         transactionid     TYPE string,
         status            TYPE string,
         paymentby         TYPE string,
         contractno        TYPE string,
         geminvoiceno      TYPE string,
         invoiceno         TYPE string,
         billno            TYPE string,
         billamountpaid    TYPE p LENGTH 9 DECIMALS 0,
         transactiondate   TYPE string,
         deductedamount    TYPE p LENGTH 9 DECIMALS 0,
         deductiontype     TYPE string,
         bankname          TYPE string,
         chequenumber      TYPE string,
         banktransactionno TYPE string,
         demanddraftno     TYPE string,
         sanctions         TYPE string,
         sanctiondate      TYPE string,
       END OF ty_paydata.

*--- Request payload
TYPES: BEGIN OF ty_request,
         user    TYPE string,
         method  TYPE string,
         paydata TYPE ty_paydata,
       END OF ty_request.

*--- Response structure matching the ACTUAL payload (confirmed real response).
*   Component names equal JSON keys (case-insensitive match) -> no name_mappings.
TYPES: BEGIN OF ty_display,
         transactionid TYPE string,
         status        TYPE string,
         paymentmode   TYPE string,
         message       TYPE string,
         raw_response  TYPE string,   " full raw response, kept for diagnosis
       END OF ty_display,
       tt_display TYPE STANDARD TABLE OF ty_display WITH DEFAULT KEY.

DATA: lo_client    TYPE REF TO if_http_client,
      ls_request   TYPE ty_request,
      lv_json      TYPE string,
      lv_response  TYPE string,
      lv_code      TYPE i,
      lv_reason    TYPE string,
      lt_display   TYPE tt_display,
      ls_display   TYPE ty_display,
      lo_alv       TYPE REF TO cl_salv_table,
      lx_salv      TYPE REF TO cx_salv_msg,
      lt_name_maps TYPE /ui2/cl_json=>name_mappings.

START-OF-SELECTION.

*--- 1. Generate the SEK security token via the CPI token proxy
  proxy_data-mt_security_token_sender-username = 'ONGCVIDESH'.
  proxy_data-mt_security_token_sender-password = 'M8sQ3Zp2Xk7L1dT9V4bH6cW0YgF5nRJA'.
  TRY.
      CREATE OBJECT lo_gem_token.
      CALL METHOD lo_gem_token->si_security_token_ob
        EXPORTING output = proxy_data
        IMPORTING input  = lt_input.
    CATCH cx_ai_system_fault INTO lo_sys_exception.
      err_string = lo_sys_exception->get_text( ).
    CATCH cx_ai_application_fault.
  ENDTRY.
  gv_token = lt_input-mt_security_token_receiver-token.

*--- 2. Build the paydata payload from selection-screen values
  CLEAR ls_request.
  ls_request-user   = p_user.
  ls_request-method = 'payments'.
  ls_request-paydata-transactionid     = p_txnid.
  ls_request-paydata-status            = p_status.
  ls_request-paydata-paymentby         = p_paymby.
  ls_request-paydata-contractno        = p_contno.
  ls_request-paydata-geminvoiceno      = p_geminv.
  ls_request-paydata-invoiceno         = p_invno.
  ls_request-paydata-billno            = p_billno.
  ls_request-paydata-billamountpaid    = p_bamt.
  ls_request-paydata-transactiondate   = p_txndt.
  ls_request-paydata-deductedamount    = p_dedamt.
  ls_request-paydata-deductiontype     = p_dedtyp.
  ls_request-paydata-bankname          = p_bank.
  ls_request-paydata-chequenumber      = p_chqno.
  ls_request-paydata-banktransactionno = p_bktxno.
  ls_request-paydata-demanddraftno     = p_ddno.
  ls_request-paydata-sanctions         = p_sanc.
  ls_request-paydata-sanctiondate      = p_sancdt.

*--- 3. Serialize as plain JSON (no encryption) with the real camelCase keys
  lt_name_maps = VALUE #(
    ( abap = 'TRANSACTIONID'     json = 'transactionID' )
    ( abap = 'PAYMENTBY'         json = 'paymentBy' )
    ( abap = 'CONTRACTNO'        json = 'contractNo' )
    ( abap = 'GEMINVOICENO'      json = 'gemInvoiceNo' )
    ( abap = 'INVOICENO'         json = 'invoiceNo' )
    ( abap = 'BILLNO'            json = 'billNo' )
    ( abap = 'BILLAMOUNTPAID'    json = 'billAmountPaid' )
    ( abap = 'TRANSACTIONDATE'   json = 'transactionDate' )
    ( abap = 'DEDUCTEDAMOUNT'    json = 'deductedAmount' )
    ( abap = 'DEDUCTIONTYPE'     json = 'deductionType' )
    ( abap = 'BANKNAME'          json = 'bankName' )
    ( abap = 'CHEQUENUMBER'      json = 'chequeNumber' )
    ( abap = 'BANKTRANSACTIONNO' json = 'bankTransactionNo' )
    ( abap = 'DEMANDDRAFTNO'     json = 'demandDraftNo' )
    ( abap = 'SANCTIONDATE'      json = 'sanctionDate' ) ).

  lv_json = /ui2/cl_json=>serialize(
              data          = ls_request
              compress      = abap_true
              name_mappings = lt_name_maps ).

*--- 4. Create HTTP client from SM59 destination and set path/method
  cl_http_client=>create_by_destination(
    EXPORTING destination = c_dest
    IMPORTING client      = lo_client
    EXCEPTIONS OTHERS     = 1 ).
  IF sy-subrc <> 0.
    WRITE: / 'Error creating HTTP client for destination', c_dest. RETURN.
  ENDIF.

  lo_client->propertytype_logon_popup = if_http_client=>co_disabled.
  cl_http_utility=>set_request_uri( request = lo_client->request uri = p_path ).
  lo_client->request->set_method( if_http_request=>co_request_method_post ).

*--- 5. Headers: Content-Type + SEK token header 'token' = Bearer <token>
  lo_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
  IF gv_token IS NOT INITIAL.
    lo_client->request->set_header_field( name = 'token' value = |Bearer { gv_token }| ).
  ENDIF.

*--- 6. Body + send + receive
  lo_client->request->set_cdata( lv_json ).
  lo_client->send( EXCEPTIONS OTHERS = 1 ).
  IF sy-subrc <> 0.
    WRITE: / 'Error sending request to CPI'. lo_client->close( EXCEPTIONS OTHERS = 0 ). RETURN.
  ENDIF.
  lo_client->receive( EXCEPTIONS OTHERS = 1 ).
  lo_client->response->get_status( IMPORTING code = lv_code reason = lv_reason ).
  lv_response = lo_client->response->get_cdata( ).
  lo_client->close( EXCEPTIONS OTHERS = 0 ).

*--- 7. Parse the confirmed flat response into typed fields
  CLEAR ls_display.
  /ui2/cl_json=>deserialize( EXPORTING json = lv_response
                             CHANGING  data = ls_display ).
  ls_display-raw_response = lv_response.   " full raw response for reference
  APPEND ls_display TO lt_display.

*--- 8. Display as ALV grid with the (editable) list header from p_head
  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                              CHANGING  t_table      = lt_display ).
      lo_alv->get_columns( )->set_optimize( abap_true ).
      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_display_settings( )->set_list_header( p_head ).
      lo_alv->display( ).
    CATCH cx_salv_msg INTO lx_salv.
      WRITE: / 'ALV error:', lx_salv->get_text( ).
      WRITE: / 'HTTP', lv_code, lv_reason.
      WRITE: / lv_response.
  ENDTRY.
