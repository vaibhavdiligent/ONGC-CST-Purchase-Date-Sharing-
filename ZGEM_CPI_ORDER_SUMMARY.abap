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
            p_buyer TYPE string LOWER CASE DEFAULT 'buyerID',   " optional
            p_ason  TYPE string LOWER CASE DEFAULT '2023-04-12', " single-date mode
            p_from  TYPE string LOWER CASE,                      " range mode (with p_to)
            p_to    TYPE string LOWER CASE,                      " range mode (needs p_from)
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
      lv_reason   TYPE string.

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
