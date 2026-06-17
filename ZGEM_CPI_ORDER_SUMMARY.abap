*&---------------------------------------------------------------------*
*& Report ZGEM_CPI_ORDER_SUMMARY
*&---------------------------------------------------------------------*
*& Calls the SAP CPI iFlow endpoint /http/GEM/Test using the
*& SM59 HTTP (type G) RFC destination "CPI".
*&
*& - The host / SSL / proxy settings are taken from SM59 destination CPI.
*& - Auth: an OAuth2 Bearer token is fetched (client credentials) from the
*&   XSUAA token endpoint and sent on the CPI call (SM59 Basic auth alone
*&   returns 401 against the OAuth-protected runtime).
*& - The path (e.g. /http/S4/GEM/OrderSummary) is set via set_request_uri;
*&   CPI derives CamelHttpPath from the URL path to route the interface.
*& - Request method POST, body = JSON payload (orderSummary).
*&---------------------------------------------------------------------*
REPORT zgem_cpi_order_summary.

CONSTANTS: c_dest TYPE rfcdest VALUE 'CPI'.

*--- OAuth2 (client credentials) details for the CPI runtime (hardcoded)
*   The CPI HTTPS sender adapter is protected by OAuth2; we fetch a Bearer
*   token from the XSUAA token endpoint and send it on the CPI call.
CONSTANTS:
  c_clientid TYPE string VALUE
    'sb-d50facfd-958c-4c12-8e8a-522fef205ada!b41507|it-rt-ovl-subaccount-non-prod-zmmj4s8r!b148',
  c_clientsecret TYPE string VALUE
    '23bbd1bb-96bc-440a-a418-54073f7ced3a$AnHre5W4lVFCSgmJxVXtw6S54yUB2RX9vfuCu-NoxDs=',
  c_tokenurl TYPE string VALUE
    'https://ovl-subaccount-non-prod-zmmj4s8r.authentication.in30.hana.ondemand.com/oauth/token'.

*--- Selection screen so the values from the spec can be entered/changed
PARAMETERS: p_user  TYPE string LOWER CASE DEFAULT 'clientname',
            p_buyer TYPE string LOWER CASE DEFAULT 'buyerID',   " optional
            p_ason  TYPE string LOWER CASE DEFAULT '2023-04-12', " single-date mode
            p_from  TYPE string LOWER CASE,                      " range mode (with p_to)
            p_to    TYPE string LOWER CASE,                      " range mode (needs p_from)
            p_token TYPE string LOWER CASE,   " Authentication SEK token (optional)
            p_path  TYPE string LOWER CASE       " request path -> CPI derives CamelHttpPath from it
              DEFAULT '/http/S4/GEM/OrderSummary'.

*--- Structure that mirrors the request payload from the spec
TYPES: BEGIN OF ty_request,
         user         TYPE string,
         method       TYPE string,
         buyer_user_id TYPE string,
         as_on        TYPE string,
         from_date    TYPE string,
         to_date      TYPE string,
       END OF ty_request.

*--- Structures that mirror the response payload (Section 3.2.5)
*   Body
*     |- Status, Iat
*     |- data (parent)
*          |- Sub, Aud, Iss
*          |- data (child)
*               |- Date, Count
*               |- orderIds [ { orderId } ]
TYPES: BEGIN OF ty_order,
         orderid TYPE string,            " ordereId (max 100)
       END OF ty_order,
       tt_order TYPE STANDARD TABLE OF ty_order WITH DEFAULT KEY.

TYPES: BEGIN OF ty_data_child,
         date     TYPE string,           " Date orders are requested
         count    TYPE i,                " Total number of orders
         orderids TYPE tt_order,         " All order ids per the count
       END OF ty_data_child.

TYPES: BEGIN OF ty_data_parent,
         sub  TYPE string,               " Service name
         aud  TYPE string,               " Entity name
         iss  TYPE string,               " Source identification (e.g. GeM)
         data TYPE ty_data_child,
       END OF ty_data_parent.

TYPES: BEGIN OF ty_response,
         status TYPE string,             " Succ / Fail
         iat    TYPE p LENGTH 8 DECIMALS 0, " Response timestamp (epoch)
         data   TYPE ty_data_parent,
       END OF ty_response.

*--- Flat structure for display on screen (one row per order id)
TYPES: BEGIN OF ty_display,
         status  TYPE string,
         sub     TYPE string,
         aud     TYPE string,
         iss     TYPE string,
         date    TYPE string,
         count   TYPE i,
         orderid TYPE string,
       END OF ty_display,
       tt_display TYPE STANDARD TABLE OF ty_display WITH DEFAULT KEY.

DATA: lo_client   TYPE REF TO if_http_client,
      ls_request  TYPE ty_request,
      ls_response TYPE ty_response,
      ls_order    TYPE ty_order,
      lt_maps     TYPE /ui2/cl_json=>name_mappings,
      lt_display  TYPE tt_display,
      ls_display  TYPE ty_display,
      lo_alv      TYPE REF TO cl_salv_table,
      lx_salv     TYPE REF TO cx_salv_msg,
      lv_json     TYPE string,
      lv_response TYPE string,
      lv_code     TYPE i,
      lv_reason   TYPE string,
      lv_token    TYPE string.   " OAuth2 access token fetched at runtime

START-OF-SELECTION.

*--- 1. Build the JSON payload per the spec (Section 3.2.4)
*   Mutually exclusive date selection:
*     - as_on  -> single date. from_date/to_date must NOT be sent.
*     - from_date/to_date -> range. as_on must NOT be sent.
*       to_date does not work without from_date.
*   buyer_user_id is optional.
*   Empty fields are dropped from the JSON via compress = abap_true.
  IF p_ason IS NOT INITIAL AND ( p_from IS NOT INITIAL OR p_to IS NOT INITIAL ).
    WRITE: / 'Error: provide either as_on (single date) OR from_date/to_date (range), not both.'.
    RETURN.
  ENDIF.
  IF p_ason IS INITIAL AND p_from IS INITIAL.
    WRITE: / 'Error: provide as_on, or from_date (with to_date).'.
    RETURN.
  ENDIF.
  IF p_from IS NOT INITIAL AND p_to IS INITIAL.
    WRITE: / 'Error: to_date is mandatory when from_date is set.'.
    RETURN.
  ENDIF.
  IF p_to IS NOT INITIAL AND p_from IS INITIAL.
    WRITE: / 'Error: to_date does not work without from_date.'.
    RETURN.
  ENDIF.

  CLEAR ls_request.
  ls_request-user          = p_user.
  ls_request-method        = 'orderSummary'.
  ls_request-buyer_user_id = p_buyer.   " optional - omitted if blank

  IF p_ason IS NOT INITIAL.
    ls_request-as_on = p_ason.          " single-date mode
  ELSE.
    ls_request-from_date = p_from.      " range mode
    ls_request-to_date   = p_to.
  ENDIF.

  lv_json = /ui2/cl_json=>serialize(
              data        = ls_request
              compress    = abap_true   " drop empty/initial fields
              pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

*--- 1b. Fetch the OAuth2 Bearer token (client credentials grant)
  PERFORM get_oauth_token CHANGING lv_token.
  IF lv_token IS INITIAL.
    WRITE: / 'Error: could not obtain OAuth token from', c_tokenurl.
    RETURN.
  ENDIF.

*--- 2. Create the HTTP client from the SM59 RFC destination "CPI"
  cl_http_client=>create_by_destination(
    EXPORTING
      destination              = c_dest
    IMPORTING
      client                   = lo_client
    EXCEPTIONS
      argument_not_found       = 1
      destination_not_found    = 2
      destination_no_authority = 3
      plugin_not_active        = 4
      internal_error           = 5
      OTHERS                   = 6 ).
  IF sy-subrc <> 0.
    WRITE: / 'Error creating HTTP client for destination', c_dest.
    RETURN.
  ENDIF.

*--- 3. Suppress logon popup and set the request line
  lo_client->propertytype_logon_popup = if_http_client=>co_disabled.

*   Set the request path. CPI routes the interface by deriving the internal
*   CamelHttpPath header from the URL path after the sender endpoint address
*   (e.g. address /http/S4/GEM/* called with /http/S4/GEM/OrderSummary ->
*   CamelHttpPath = OrderSummary). CamelHttpPath is a reserved Camel header
*   and is NOT honoured if sent as an HTTP request header - it must come
*   from the URL. Keep the SM59 destination Path Prefix EMPTY to avoid
*   duplicating the path.
  cl_http_utility=>set_request_uri(
    request = lo_client->request
    uri     = p_path ).

  lo_client->request->set_method( if_http_request=>co_request_method_post ).

*--- 4. Request headers (Section 3.2.2)
  lo_client->request->set_header_field(
    name  = 'Content-Type'
    value = 'application/json' ).

*  Authentication: send the OAuth2 Bearer token fetched in step 1b.
*  (A manual p_token, if supplied, overrides the fetched one.)
  IF p_token IS NOT INITIAL.
    lv_token = p_token.
  ENDIF.
  lo_client->request->set_header_field(
    name  = 'Authorization'
    value = |Bearer { lv_token }| ).

*--- 5. Set the JSON body (Section 3.2.3)
  lo_client->request->set_cdata( lv_json ).

*--- 6. Send
  lo_client->send(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4 ).
  IF sy-subrc <> 0.
    WRITE: / 'Error sending request to CPI'.
    lo_client->close( EXCEPTIONS OTHERS = 0 ).
    RETURN.
  ENDIF.

*--- 7. Receive
  lo_client->receive(
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4 ).
  IF sy-subrc <> 0.
    " Even on HTTP error status receive may raise; read status anyway
    lo_client->response->get_status(
      IMPORTING
        code   = lv_code
        reason = lv_reason ).
    WRITE: / 'Receive failed. HTTP status:', lv_code, lv_reason.
    lo_client->close( EXCEPTIONS OTHERS = 0 ).
    RETURN.
  ENDIF.

*--- 8. Read response
  lo_client->response->get_status(
    IMPORTING
      code   = lv_code
      reason = lv_reason ).

  lv_response = lo_client->response->get_cdata( ).

  lo_client->close( EXCEPTIONS OTHERS = 0 ).

*--- 9. Capture / parse the response payload (Section 3.2.5)
*   Map the mixed-case JSON keys onto the ABAP component names.
*   (One mapping per distinct name; applied at every nesting level.)
  lt_maps = VALUE #(
    ( abap = 'STATUS'   json = 'Status' )
    ( abap = 'IAT'      json = 'Iat' )
    ( abap = 'DATA'     json = 'data' )
    ( abap = 'SUB'      json = 'Sub' )
    ( abap = 'AUD'      json = 'Aud' )
    ( abap = 'ISS'      json = 'Iss' )
    ( abap = 'DATE'     json = 'Date' )
    ( abap = 'COUNT'    json = 'Count' )
    ( abap = 'ORDERIDS' json = 'orderIds' )
    ( abap = 'ORDERID'  json = 'orderId' ) ).

  /ui2/cl_json=>deserialize(
    EXPORTING
      json          = lv_response
      name_mappings = lt_maps
    CHANGING
      data          = ls_response ).

*--- 10. Move parsed response into a flat internal table (one row per order id)
  CLEAR lt_display.
  LOOP AT ls_response-data-data-orderids INTO ls_order.
    CLEAR ls_display.
    ls_display-status  = ls_response-status.
    ls_display-sub     = ls_response-data-sub.
    ls_display-aud     = ls_response-data-aud.
    ls_display-iss     = ls_response-data-iss.
    ls_display-date    = ls_response-data-data-date.
    ls_display-count   = ls_response-data-data-count.
    ls_display-orderid = ls_order-orderid.
    APPEND ls_display TO lt_display.
  ENDLOOP.

*--- 11. Display the internal table on screen as an ALV grid
  IF lt_display IS NOT INITIAL.
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = lt_display ).

        " Optimized column widths + standard ALV functions (sort, filter, export)
        lo_alv->get_columns( )->set_optimize( abap_true ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->get_display_settings( )->set_list_header(
          |orderSummary - { ls_response-status } - Count { ls_response-data-data-count }| ).

        lo_alv->display( ).
      CATCH cx_salv_msg INTO lx_salv.
        WRITE: / 'ALV display error:', lx_salv->get_text( ).
    ENDTRY.
  ELSE.
    " Nothing to tabulate - show status / raw payload for diagnosis
    WRITE: / 'HTTP Status :', lv_code, lv_reason.
    WRITE: / 'API Status  :', ls_response-status.
    WRITE: / 'No order ids returned. Raw response:'.
    WRITE: / lv_response.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form GET_OAUTH_TOKEN
*& Fetches an OAuth2 access token via client_credentials grant from the
*& XSUAA token endpoint. Client id/secret are passed as HTTP Basic auth
*& (recommended for XSUAA) and grant_type in the form body.
*&---------------------------------------------------------------------*
FORM get_oauth_token CHANGING cv_token TYPE string.

  DATA: lo_tok    TYPE REF TO if_http_client,
        lv_basic  TYPE string,
        lv_b64    TYPE string,
        lv_resp   TYPE string,
        lv_code   TYPE i,
        lv_reason TYPE string.

  CLEAR cv_token.

*--- Token endpoint is on a different host than CPI -> create by URL
  cl_http_client=>create_by_url(
    EXPORTING
      url                = c_tokenurl
    IMPORTING
      client             = lo_tok
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4 ).
  IF sy-subrc <> 0.
    WRITE: / 'Error creating HTTP client for token endpoint'.
    RETURN.
  ENDIF.

  lo_tok->propertytype_logon_popup = if_http_client=>co_disabled.

*--- HTTP Basic auth header = base64( clientid:clientsecret )
  lv_basic = |{ c_clientid }:{ c_clientsecret }|.
  lv_b64   = cl_http_utility=>encode_base64( unencoded = lv_basic ).

  lo_tok->request->set_method( if_http_request=>co_request_method_post ).
  lo_tok->request->set_header_field(
    name = 'Content-Type' value = 'application/x-www-form-urlencoded' ).
  lo_tok->request->set_header_field(
    name = 'Accept' value = 'application/json' ).
  lo_tok->request->set_header_field(
    name = 'Authorization' value = |Basic { lv_b64 }| ).
  lo_tok->request->set_cdata( 'grant_type=client_credentials' ).

  lo_tok->send( EXCEPTIONS http_communication_failure = 1
                           http_invalid_state         = 2
                           http_processing_failed     = 3
                           OTHERS                     = 4 ).
  IF sy-subrc <> 0.
    WRITE: / 'Error sending token request'.
    lo_tok->close( EXCEPTIONS OTHERS = 0 ).
    RETURN.
  ENDIF.

  lo_tok->receive( EXCEPTIONS http_communication_failure = 1
                              http_invalid_state         = 2
                              http_processing_failed     = 3
                              OTHERS                     = 4 ).
  lo_tok->response->get_status( IMPORTING code = lv_code reason = lv_reason ).
  lv_resp = lo_tok->response->get_cdata( ).
  lo_tok->close( EXCEPTIONS OTHERS = 0 ).

  IF lv_code <> 200.
    WRITE: / 'Token endpoint returned HTTP', lv_code, lv_reason.
    WRITE: / lv_resp.
    RETURN.
  ENDIF.

*--- Parse access_token from the JSON response
  DATA: BEGIN OF ls_tok,
          access_token TYPE string,
          token_type   TYPE string,
          expires_in   TYPE i,
        END OF ls_tok.
  /ui2/cl_json=>deserialize(
    EXPORTING json = lv_resp
    CHANGING  data = ls_tok ).

  cv_token = ls_tok-access_token.

ENDFORM.
