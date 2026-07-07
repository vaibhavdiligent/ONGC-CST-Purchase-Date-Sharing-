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
*& STILL UNVERIFIED: the exact field names GeM expects INSIDE "paydata" for
*& the request (p_paydat below is passed through as an opaque string).
*&---------------------------------------------------------------------*
REPORT zgem_cpi_payment_status.

CONSTANTS: c_dest TYPE rfcdest VALUE 'CPI_HTTP_GEM'.

PARAMETERS:
            p_head  TYPE char70 LOWER CASE DEFAULT 'Payment Status (3.11)', " ALV list header (editable)
            p_user  TYPE string LOWER CASE DEFAULT 'clientname',
            p_paydat TYPE string LOWER CASE, " encrypted paydata blob
            p_path  TYPE string LOWER CASE DEFAULT '/http/GEM/Sync/PaymentStatus'.

*--- Token proxy objects (same pattern as the summary program)
DATA: lo_gem_token     TYPE REF TO zgem_tokenco_si_security_token,
      proxy_data       TYPE zgem_tokenmt_security_token_se,
      lt_input         TYPE zgem_tokenmt_security_token_re,
      lo_sys_exception TYPE REF TO cx_ai_system_fault,
      err_string       TYPE string,
      gv_token         TYPE string.

*--- Request payload (Section Payment Status (3.11))
TYPES: BEGIN OF ty_request,
         user          TYPE string,
         method        TYPE string,
         paydata       TYPE string,
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

DATA: lo_client   TYPE REF TO if_http_client,
      ls_request  TYPE ty_request,
      lv_json     TYPE string,
      lv_response TYPE string,
      lv_code     TYPE i,
      lv_reason   TYPE string,
      lt_display  TYPE tt_display,
      ls_display  TYPE ty_display,
      lo_alv      TYPE REF TO cl_salv_table,
      lx_salv     TYPE REF TO cx_salv_msg.

START-OF-SELECTION.

*--- 1. Validate input
  IF p_paydat IS INITIAL.
    WRITE: / 'Error: paydata is mandatory.'. RETURN.
  ENDIF.

*--- 1a. Generate the SEK security token via the CPI token proxy
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

*--- 2. Build the JSON request payload
  CLEAR ls_request.
  ls_request-user   = p_user.
  ls_request-method = 'payments'.
  ls_request-paydata = p_paydat.

  lv_json = /ui2/cl_json=>serialize(
              data        = ls_request
              compress    = abap_true
              pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

*--- 3. Create HTTP client from SM59 destination and set path/method
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

*--- 4. Headers: Content-Type + SEK token header 'token' = Bearer <token>
  lo_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
  IF gv_token IS NOT INITIAL.
    lo_client->request->set_header_field( name = 'token' value = |Bearer { gv_token }| ).
  ENDIF.

*--- 5. Body + send + receive
  lo_client->request->set_cdata( lv_json ).
  lo_client->send( EXCEPTIONS OTHERS = 1 ).
  IF sy-subrc <> 0.
    WRITE: / 'Error sending request to CPI'. lo_client->close( EXCEPTIONS OTHERS = 0 ). RETURN.
  ENDIF.
  lo_client->receive( EXCEPTIONS OTHERS = 1 ).
  lo_client->response->get_status( IMPORTING code = lv_code reason = lv_reason ).
  lv_response = lo_client->response->get_cdata( ).
  lo_client->close( EXCEPTIONS OTHERS = 0 ).

*--- 6. Parse the confirmed flat response into typed fields
  CLEAR ls_display.
  /ui2/cl_json=>deserialize( EXPORTING json = lv_response
                             CHANGING  data = ls_display ).
  ls_display-raw_response = lv_response.   " full raw response for reference
  APPEND ls_display TO lt_display.

*--- 7. Display as ALV grid with the (editable) list header from p_head
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
