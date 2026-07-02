*&---------------------------------------------------------------------*
*& Report ZGEM_CPI_SDAC_DETAILS
*&---------------------------------------------------------------------*
*& SDAC Details (3.8) - GeM CPI integration.
*&   1. Generate SEK token via proxy ZGEM_TOKENCO_SI_SECURITY_TOKEN.
*&   2. Call CPI through SM59 destination CPI_HTTP_GEM.
*&   3. Path -> CPI derives CamelHttpPath (sender endpoint must end with /*).
*&   4. POST JSON body; SEK token sent as header 'token' = Bearer <token>.
*&   5. Parse response and display as ALV grid (one row per SNDD entry).
*&
*& Real response shape:
*&   {"sub":"CracService","aud":..,"iss":..,"data":[{
*&     header fields (contractNumber, shipmentId, InvoiceNumber, ...),
*&     "items":[{
*&       "order_item_id":..,"total_base_cost":{currency,value},
*&       "addons":{"addons":{currency,value},"pots":{currency,value}},
*&       "sla_deductions":[],
*&       "service_non_delivery_deduction":[{
*&         "reason":null|{code,hint,value,...},"comment":..,"value":{currency,value},
*&         "sndd_files":..
*&       }]
*&     }],
*&     "sdac_url":..,"crac_id":..,"sdac_date":..
*&   }]}
*&
*& Note: value inside currency-amount objects may be a bare number (1.67) or
*&       a quoted string ("0.00"); stored as string to handle both.
*&       $$hashKey in reason object is ignored by deserializer.
*&---------------------------------------------------------------------*
REPORT zgem_cpi_sdac_details.

CONSTANTS: c_dest TYPE rfcdest VALUE 'CPI_HTTP_GEM'.

PARAMETERS:
            p_head  TYPE char70 LOWER CASE DEFAULT 'SDAC Details (3.8)', " ALV list header (editable)
            p_user  TYPE string LOWER CASE DEFAULT 'clientname',
            p_buyer TYPE string LOWER CASE DEFAULT 'buyerID',   " optional
            p_from  TYPE string LOWER CASE,                      " from_date (mandatory)
            p_to    TYPE string LOWER CASE,                      " to_date (mandatory)
            p_path  TYPE string LOWER CASE DEFAULT '/http/GEM/Sync/SdacDetails'.

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
         from_date     TYPE string,
         to_date       TYPE string,
       END OF ty_request.

*--- Currency-amount pair (value kept as string: JSON mixes bare numbers and quoted strings)
TYPES: BEGIN OF ty_amt,
         currency TYPE string,
         value    TYPE string,
       END OF ty_amt.

*--- reason object inside service_non_delivery_deduction
*   ($$hashKey is silently skipped by /ui2/cl_json)
TYPES: BEGIN OF ty_reason,
         code  TYPE string,
         hint  TYPE string,
         value TYPE string,
       END OF ty_reason.

*--- One service_non_delivery_deduction entry
TYPES: BEGIN OF ty_sndd,
         reason     TYPE ty_reason,
         sndd_files TYPE string,
         comment    TYPE string,
         value      TYPE ty_amt,
       END OF ty_sndd,
       tt_sndd TYPE STANDARD TABLE OF ty_sndd WITH DEFAULT KEY.

*--- addons section inside an item  (can be empty object {})
TYPES: BEGIN OF ty_addons,
         addons TYPE ty_amt,
         pots   TYPE ty_amt,
       END OF ty_addons.

*--- Dummy type for sla_deductions (always empty array in practice)
TYPES: tt_string TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

*--- One item inside a SDAC record
TYPES: BEGIN OF ty_item,
         order_item_id                 TYPE i,
         total_base_cost               TYPE ty_amt,
         addons                        TYPE ty_addons,
         sla_deductions                TYPE tt_string,
         service_non_delivery_deduction TYPE tt_sndd,
       END OF ty_item,
       tt_item TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY.

*--- One SDAC record (one element of data[])
TYPES: BEGIN OF ty_sdac,
         contractnumber          TYPE string,   " contractNumber
         consigneepostid         TYPE string,   " consigneePostId
         shipmentid              TYPE string,   " shipmentId
         invoicenumber           TYPE string,   " InvoiceNumber (capital I - case-insensitive)
         invoicebillingstartdate TYPE string,   " invoiceBillingStartDate
         invoicebillingenddate   TYPE string,   " invoiceBillingEndDate
         invoicecurrency         TYPE string,
         invoicevalue            TYPE string,
         paymentmode             TYPE string,
         requesttype             TYPE string,
         pgmode                  TYPE string,
         autocracflag            TYPE string,
         sdacid                  TYPE string,
         gemuid                  TYPE string,
         items                   TYPE tt_item,
         sdac_url                TYPE string,
         crac_id                 TYPE string,
         sdac_date               TYPE string,
       END OF ty_sdac,
       tt_sdac TYPE STANDARD TABLE OF ty_sdac WITH DEFAULT KEY.

TYPES: BEGIN OF ty_response,
         sub  TYPE string,
         aud  TYPE string,
         iss  TYPE string,
         data TYPE tt_sdac,
       END OF ty_response.

*--- Flat display: one row per (sdac -> item -> sndd entry)
TYPES: BEGIN OF ty_display,
         sub                     TYPE string,
         aud                     TYPE string,
         iss                     TYPE string,
         contractnumber          TYPE string,
         shipmentid              TYPE string,
         invoicenumber           TYPE string,
         invoicebillingstartdate TYPE string,
         invoicebillingenddate   TYPE string,
         invoicecurrency         TYPE string,
         invoicevalue            TYPE string,
         paymentmode             TYPE string,
         requesttype             TYPE string,
         pgmode                  TYPE string,
         autocracflag            TYPE string,
         sdacid                  TYPE string,
         gemuid                  TYPE string,
         crac_id                 TYPE string,
         sdac_date               TYPE string,
         sdac_url                TYPE string,
         consigneepostid         TYPE string,
         order_item_id           TYPE i,
         total_base_cost_val     TYPE string,
         addons_val              TYPE string,
         pots_val                TYPE string,
         sndd_comment            TYPE string,
         sndd_value              TYPE string,
         sndd_reason_code        TYPE string,
         sndd_reason_value       TYPE string,
         sndd_files              TYPE string,
       END OF ty_display,
       tt_display TYPE STANDARD TABLE OF ty_display WITH DEFAULT KEY.

DATA: lo_client   TYPE REF TO if_http_client,
      ls_request  TYPE ty_request,
      ls_response TYPE ty_response,
      ls_sdac     TYPE ty_sdac,
      ls_item     TYPE ty_item,
      ls_sndd     TYPE ty_sndd,
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
  IF p_from IS INITIAL OR p_to IS INITIAL.
    WRITE: / 'Error: from_date and to_date are both mandatory.'. RETURN.
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
  ls_request-user          = p_user.
  ls_request-method        = 'getcracservice'.
  ls_request-buyer_user_id = p_buyer.
  ls_request-from_date     = p_from.
  ls_request-to_date       = p_to.

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

*--- 6a. Flatten: one row per (sdac -> item -> service_non_delivery_deduction entry).
*   If sndd is empty for an item, still emit one row for the item.
  CLEAR lt_display.
  LOOP AT ls_response-data INTO ls_sdac.
    LOOP AT ls_sdac-items INTO ls_item.
      IF ls_item-service_non_delivery_deduction IS INITIAL.
        CLEAR ls_display.
        ls_display-sub                     = ls_response-sub.
        ls_display-aud                     = ls_response-aud.
        ls_display-iss                     = ls_response-iss.
        ls_display-contractnumber          = ls_sdac-contractnumber.
        ls_display-shipmentid              = ls_sdac-shipmentid.
        ls_display-invoicenumber           = ls_sdac-invoicenumber.
        ls_display-invoicebillingstartdate = ls_sdac-invoicebillingstartdate.
        ls_display-invoicebillingenddate   = ls_sdac-invoicebillingenddate.
        ls_display-invoicecurrency         = ls_sdac-invoicecurrency.
        ls_display-invoicevalue            = ls_sdac-invoicevalue.
        ls_display-paymentmode             = ls_sdac-paymentmode.
        ls_display-requesttype             = ls_sdac-requesttype.
        ls_display-pgmode                  = ls_sdac-pgmode.
        ls_display-autocracflag            = ls_sdac-autocracflag.
        ls_display-sdacid                  = ls_sdac-sdacid.
        ls_display-gemuid                  = ls_sdac-gemuid.
        ls_display-crac_id                 = ls_sdac-crac_id.
        ls_display-sdac_date               = ls_sdac-sdac_date.
        ls_display-sdac_url                = ls_sdac-sdac_url.
        ls_display-consigneepostid         = ls_sdac-consigneepostid.
        ls_display-order_item_id           = ls_item-order_item_id.
        ls_display-total_base_cost_val     = ls_item-total_base_cost-value.
        ls_display-addons_val              = ls_item-addons-addons-value.
        ls_display-pots_val                = ls_item-addons-pots-value.
        APPEND ls_display TO lt_display.
      ELSE.
        LOOP AT ls_item-service_non_delivery_deduction INTO ls_sndd.
          CLEAR ls_display.
          ls_display-sub                     = ls_response-sub.
          ls_display-aud                     = ls_response-aud.
          ls_display-iss                     = ls_response-iss.
          ls_display-contractnumber          = ls_sdac-contractnumber.
          ls_display-shipmentid              = ls_sdac-shipmentid.
          ls_display-invoicenumber           = ls_sdac-invoicenumber.
          ls_display-invoicebillingstartdate = ls_sdac-invoicebillingstartdate.
          ls_display-invoicebillingenddate   = ls_sdac-invoicebillingenddate.
          ls_display-invoicecurrency         = ls_sdac-invoicecurrency.
          ls_display-invoicevalue            = ls_sdac-invoicevalue.
          ls_display-paymentmode             = ls_sdac-paymentmode.
          ls_display-requesttype             = ls_sdac-requesttype.
          ls_display-pgmode                  = ls_sdac-pgmode.
          ls_display-autocracflag            = ls_sdac-autocracflag.
          ls_display-sdacid                  = ls_sdac-sdacid.
          ls_display-gemuid                  = ls_sdac-gemuid.
          ls_display-crac_id                 = ls_sdac-crac_id.
          ls_display-sdac_date               = ls_sdac-sdac_date.
          ls_display-sdac_url                = ls_sdac-sdac_url.
          ls_display-consigneepostid         = ls_sdac-consigneepostid.
          ls_display-order_item_id           = ls_item-order_item_id.
          ls_display-total_base_cost_val     = ls_item-total_base_cost-value.
          ls_display-addons_val              = ls_item-addons-addons-value.
          ls_display-pots_val                = ls_item-addons-pots-value.
          ls_display-sndd_comment            = ls_sndd-comment.
          ls_display-sndd_value              = ls_sndd-value-value.
          ls_display-sndd_reason_code        = ls_sndd-reason-code.
          ls_display-sndd_reason_value       = ls_sndd-reason-value.
          ls_display-sndd_files              = ls_sndd-sndd_files.
          APPEND ls_display TO lt_display.
        ENDLOOP.
      ENDIF.
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
          |{ ls_response-sub } - { ls_response-aud } - { ls_response-iss } - { lines( lt_display ) } item(s)| ).
        lo_alv->display( ).
      CATCH cx_salv_msg INTO lx_salv.
        WRITE: / 'ALV error:', lx_salv->get_text( ).
    ENDTRY.
  ELSE.
    WRITE: / 'HTTP', lv_code, lv_reason.
    WRITE: / 'No SDAC rows returned. Raw response:'.
    WRITE: / lv_response.
  ENDIF.
