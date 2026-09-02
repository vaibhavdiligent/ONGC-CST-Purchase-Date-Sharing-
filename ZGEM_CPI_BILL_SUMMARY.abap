*&---------------------------------------------------------------------*
*& Report ZGEM_CPI_BILL_SUMMARY
*&---------------------------------------------------------------------*
*& Bill Summary (3.9) - GeM CPI integration.
*& Same approach as ZGEM_CPI_ORDER_SUMMARY:
*&   1. Generate SEK token via proxy ZGEM_TOKENCO_SI_SECURITY_TOKEN.
*&   2. Call CPI through SM59 destination CPI_HTTP_GEM.
*&   3. Path -> CPI derives CamelHttpPath (sender endpoint must end with /*).
*&   4. POST JSON body; SEK token sent as header 'token' = Bearer <token>.
*&   5. Parse response and display as ALV grid (one row per gemInvoiceNo).
*&      (ALV list header is taken from selection-screen field p_head.)
*&
*& Real response shape:
*&   {"sub":..,"aud":..,"iss":..,"data":[
*&     {"date":..,"count":..,"amount":..,"gemInvoiceNos":[{"gemInvoiceNo":..}]}
*&   ]}
*&---------------------------------------------------------------------*
REPORT zgem_cpi_bill_summary.

CONSTANTS: c_dest TYPE rfcdest VALUE 'CPI_HTTP_GEM'.

PARAMETERS:
            p_head  TYPE char70 LOWER CASE DEFAULT 'Bill Summary (3.9)', " ALV list header (editable)
            p_user  TYPE string LOWER CASE DEFAULT 'clientname',
            p_buyer TYPE string LOWER CASE DEFAULT 'buyerID',   " optional
            p_ason  TYPE string LOWER CASE DEFAULT '2023-04-12', " single-date mode
            p_from  TYPE string LOWER CASE,                      " range mode (with p_to)
            p_to    TYPE string LOWER CASE,                      " range mode (needs p_from)
            p_off   TYPE string LOWER CASE DEFAULT '0',
            p_limit TYPE string LOWER CASE DEFAULT '10',
            p_path  TYPE string LOWER CASE DEFAULT '/http/GEM/Sync/BillSummary'.

*--- Token proxy objects
DATA: lo_gem_token     TYPE REF TO zgem_tokenco_si_security_token,
      proxy_data       TYPE zgem_tokenmt_security_token_se,
      lt_input         TYPE zgem_tokenmt_security_token_re,
      lo_sys_exception TYPE REF TO cx_ai_system_fault,
      err_string       TYPE string,
      gv_token         TYPE string.

*--- Request payload
TYPES: BEGIN OF ty_request,
         user          TYPE string,
         method        TYPE string,
         buyer_user_id TYPE string,
         as_on         TYPE string,
         from_date     TYPE string,
         to_date       TYPE string,
         offset        TYPE string,
         limit         TYPE string,
       END OF ty_request.

*--- Response structures matching the ACTUAL payload.
*   Component names equal JSON keys (case-insensitive match) -> no name_mappings.
TYPES: BEGIN OF ty_invoice,
         geminvoiceno TYPE string,   " gemInvoiceNo
       END OF ty_invoice,
       tt_invoice TYPE STANDARD TABLE OF ty_invoice WITH DEFAULT KEY.

TYPES: BEGIN OF ty_data_block,
         date         TYPE string,
         count        TYPE i,
         amount       TYPE string,   " returned as string in JSON
         geminvoicenos TYPE tt_invoice,  " gemInvoiceNos array
       END OF ty_data_block,
       tt_data TYPE STANDARD TABLE OF ty_data_block WITH DEFAULT KEY.

TYPES: BEGIN OF ty_response,
         sub  TYPE string,
         aud  TYPE string,
         iss  TYPE string,
         data TYPE tt_data,
       END OF ty_response.

*--- Flat display: one row per gemInvoiceNo
TYPES: BEGIN OF ty_display,
         sub          TYPE string,
         aud          TYPE string,
         iss          TYPE string,
         date         TYPE string,
         count        TYPE i,
         amount       TYPE string,
         geminvoiceno TYPE string,
       END OF ty_display,
       tt_display TYPE STANDARD TABLE OF ty_display WITH DEFAULT KEY.

DATA: lo_client   TYPE REF TO if_http_client,
      ls_request  TYPE ty_request,
      ls_response TYPE ty_response,
      ls_block    TYPE ty_data_block,
      ls_inv      TYPE ty_invoice,
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
  IF p_ason IS NOT INITIAL AND ( p_from IS NOT INITIAL OR p_to IS NOT INITIAL ).
    WRITE: / 'Error: provide either as_on OR from_date/to_date, not both.'. RETURN.
  ENDIF.
  IF p_ason IS INITIAL AND p_from IS INITIAL.
    WRITE: / 'Error: provide as_on, or from_date (with to_date).'. RETURN.
  ENDIF.
  IF p_from IS NOT INITIAL AND p_to IS INITIAL.
    WRITE: / 'Error: to_date is mandatory when from_date is set.'. RETURN.
  ENDIF.

*--- 1a. Generate the SEK security token via the CPI token proxy
  proxy_data-mt_security_token_sender-username = 'NBCCServices'.
  proxy_data-mt_security_token_sender-password = '823090987ez07u8maz0z8789qn5a4a62'.
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
  ls_request-method = 'billSummary'.
  ls_request-buyer_user_id = p_buyer.
  IF p_ason IS NOT INITIAL.
    ls_request-as_on = p_ason.
  ELSE.
    ls_request-from_date = p_from.
    ls_request-to_date   = p_to.
  ENDIF.
  ls_request-offset = p_off.
  ls_request-limit  = p_limit.

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

*--- 6. Parse the full response into typed structures
  /ui2/cl_json=>deserialize( EXPORTING json = lv_response
                             CHANGING  data = ls_response ).

*--- 6a. Flatten to one row per gemInvoiceNo per date block
  CLEAR lt_display.
  LOOP AT ls_response-data INTO ls_block.
    LOOP AT ls_block-geminvoicenos INTO ls_inv.
      CLEAR ls_display.
      ls_display-sub          = ls_response-sub.
      ls_display-aud          = ls_response-aud.
      ls_display-iss          = ls_response-iss.
      ls_display-date         = ls_block-date.
      ls_display-count        = ls_block-count.
      ls_display-amount       = ls_block-amount.
      ls_display-geminvoiceno = ls_inv-geminvoiceno.
      APPEND ls_display TO lt_display.
    ENDLOOP.
  ENDLOOP.

*--- 7. Display as ALV grid
  IF lt_display IS NOT INITIAL.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING  t_table      = lt_display ).
        lo_alv->get_columns( )->set_optimize( abap_true ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->get_display_settings( )->set_list_header(
          |{ ls_response-sub } - { ls_response-aud } - { ls_response-iss } - { lines( lt_display ) } invoice(s)| ).
        lo_alv->display( ).
      CATCH cx_salv_msg INTO lx_salv.
        WRITE: / 'ALV error:', lx_salv->get_text( ).
    ENDTRY.
  ELSE.
    WRITE: / 'HTTP', lv_code, lv_reason.
    WRITE: / 'No invoice rows returned. Raw response:'.
    WRITE: / lv_response.
  ENDIF.
