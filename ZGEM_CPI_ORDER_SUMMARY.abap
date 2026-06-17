*&---------------------------------------------------------------------*
*& Report ZGEM_CPI_ORDER_SUMMARY
*&---------------------------------------------------------------------*
*& Calls the SAP CPI iFlow endpoint /http/GEM/Test using the
*& SM59 HTTP (type G) RFC destination "CPI".
*&
*& - The host / SSL / proxy settings are taken from SM59 destination CPI.
*& - The relative path /http/GEM/Test is appended via set_uri.
*& - Request method POST, body = JSON payload (orderSummary).
*& - Headers: Content-Type application/json, authorization Bearer + SEK token.
*&---------------------------------------------------------------------*
REPORT zgem_cpi_order_summary.

CONSTANTS: c_dest TYPE rfcdest VALUE 'CPI',
           c_path TYPE string  VALUE '/http/GEM/Test'.

*--- Selection screen so the values from the spec can be entered/changed
PARAMETERS: p_user  TYPE string LOWER CASE DEFAULT 'clientname',
            p_buyer TYPE string LOWER CASE DEFAULT 'buyerID',
            p_ason  TYPE string LOWER CASE DEFAULT '2023-04-12',
            p_from  TYPE string LOWER CASE DEFAULT '2023-05-24',
            p_to    TYPE string LOWER CASE DEFAULT '2024-05-25',
            p_token TYPE string LOWER CASE.   " Authentication SEK token

*--- Structure that mirrors the request payload from the spec
TYPES: BEGIN OF ty_request,
         user         TYPE string,
         method       TYPE string,
         buyer_user_id TYPE string,
         as_on        TYPE string,
         from_date    TYPE string,
         to_date      TYPE string,
       END OF ty_request.

DATA: lo_client   TYPE REF TO if_http_client,
      ls_request  TYPE ty_request,
      lv_json     TYPE string,
      lv_response TYPE string,
      lv_code     TYPE i,
      lv_reason   TYPE string.

START-OF-SELECTION.

*--- 1. Build the JSON payload exactly as per the spec
  ls_request-user          = p_user.
  ls_request-method        = 'orderSummary'.
  ls_request-buyer_user_id = p_buyer.
  ls_request-as_on         = p_ason.
  ls_request-from_date     = p_from.
  ls_request-to_date       = p_to.

  lv_json = /ui2/cl_json=>serialize(
              data        = ls_request
              pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

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

  cl_http_utility=>set_request_uri(
    request = lo_client->request
    uri     = c_path ).

  lo_client->request->set_method( if_http_request=>co_request_method_post ).

*--- 4. Request headers (Section 3.2.2)
  lo_client->request->set_header_field(
    name  = 'Content-Type'
    value = 'application/json' ).

  lo_client->request->set_header_field(
    name  = 'authorization'
    value = |Bearer { p_token }| ).

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

*--- 9. Output
  WRITE: / 'HTTP Status :', lv_code, lv_reason.
  WRITE: / 'Request body:'.
  WRITE: / lv_json.
  SKIP.
  WRITE: / 'Response    :'.
  WRITE: / lv_response.
