*&---------------------------------------------------------------------*
*& Report ZGEM_CPI_CRAC_DETAILS
*&---------------------------------------------------------------------*
*& CRAC Details (3.8) - GeM CPI integration.
*&   1. Generate SEK token via proxy ZGEM_TOKENCO_SI_SECURITY_TOKEN.
*&   2. Call CPI through SM59 destination CPI_HTTP_GEM.
*&   3. Path -> CPI derives CamelHttpPath (sender endpoint must end with /*).
*&   4. POST JSON body; SEK token sent as header 'token' = Bearer <token>.
*&   5. Parse response and display as ALV grid (one row per cracShipmentitem).
*&
*& Real response shape:
*&   {"sub":..,"aud":..,"iss":..,"data":[{
*&     consignee/CRAC header fields,
*&     "cracShipmentitems":[{shipment item fields}],
*&     "prcShipmentItems":[{prc item fields}]
*&   }]}
*&---------------------------------------------------------------------*
REPORT zgem_cpi_crac_details.

CONSTANTS: c_dest TYPE rfcdest VALUE 'CPI_HTTP_GEM'.

PARAMETERS:
            p_head  TYPE char70 LOWER CASE DEFAULT 'CRAC Details (3.8)', " ALV list header (editable)
            p_user  TYPE string LOWER CASE DEFAULT 'clientname',
            p_buyer TYPE string LOWER CASE DEFAULT 'buyerID',   " optional
            p_from  TYPE string LOWER CASE,                      " from_date (mandatory)
            p_to    TYPE string LOWER CASE,                      " to_date (mandatory)
            p_path  TYPE string LOWER CASE DEFAULT '/http/GEM/Sync/CracDetails'.

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

*--- Response structures matching the ACTUAL payload.
*   Component names equal JSON keys (case-insensitive match) -> no name_mappings.
*   freightCharge in cracShipmentitems is a nested object {currency, value}.
TYPES: BEGIN OF ty_freight,
         currency TYPE string,
         value    TYPE p LENGTH 10 DECIMALS 2,
       END OF ty_freight.

TYPES: BEGIN OF ty_shipment_item,
         order_item_id     TYPE i,
         reason            TYPE string,
         receivedqty       TYPE i,
         agency_name       TYPE string,
         acceptedqty       TYPE i,
         inspected_by      TYPE string,
         shipmentitemid    TYPE i,
         inspection_cert_no TYPE string,
         rejectedqty       TYPE i,
         freightcharge     TYPE ty_freight,   " {"currency":..,"value":..}
         productname       TYPE string,
       END OF ty_shipment_item,
       tt_shipment_item TYPE STANDARD TABLE OF ty_shipment_item WITH DEFAULT KEY.

TYPES: BEGIN OF ty_prc_item,
         order_item_id TYPE i,
         reason        TYPE string,
         receivedqty   TYPE i,
         rejectedqty   TYPE i,
         productname   TYPE string,
       END OF ty_prc_item,
       tt_prc_item TYPE STANDARD TABLE OF ty_prc_item WITH DEFAULT KEY.

TYPES: BEGIN OF ty_crac,
         consigneeaddress     TYPE string,
         consigneedistrict    TYPE string,
         consigneename        TYPE string,
         consigneelastname    TYPE string,
         consigneemobile      TYPE string,
         consigneepin         TYPE string,
         consigneestate       TYPE string,
         consignmentamount    TYPE string,
         consignmentcurrency  TYPE string,
         autocracflag         TYPE string,
         cracamount           TYPE string,
         craccurrency         TYPE string,
         cracdocurl           TYPE string,
         cracnumber           TYPE string,
         cracshipmentitems    TYPE tt_shipment_item,   " cracShipmentitems
         cracverificationdate TYPE string,
         craccreateddate      TYPE string,             " CracCreatedDate
         demandid             TYPE string,
         invoicenumber        TYPE string,
         orderid              TYPE string,
         prcreceiveddate      TYPE string,
         prcshipmentitems     TYPE tt_prc_item,        " prcShipmentItems
       END OF ty_crac,
       tt_crac TYPE STANDARD TABLE OF ty_crac WITH DEFAULT KEY.

TYPES: BEGIN OF ty_response,
         sub  TYPE string,
         aud  TYPE string,
         iss  TYPE string,
         data TYPE tt_crac,
       END OF ty_response.

*--- Flat display: one row per cracShipmentitem
TYPES: BEGIN OF ty_display,
         sub                  TYPE string,
         aud                  TYPE string,
         iss                  TYPE string,
         cracnumber           TYPE string,
         orderid              TYPE string,
         demandid             TYPE string,
         invoicenumber        TYPE string,
         consigneename        TYPE string,
         consigneelastname    TYPE string,
         consigneeaddress     TYPE string,
         consigneedistrict    TYPE string,
         consigneepin         TYPE string,
         consigneestate       TYPE string,
         consigneemobile      TYPE string,
         consignmentamount    TYPE string,
         consignmentcurrency  TYPE string,
         autocracflag         TYPE string,
         cracamount           TYPE string,
         craccurrency         TYPE string,
         cracverificationdate TYPE string,
         craccreateddate      TYPE string,
         prcreceiveddate      TYPE string,
         cracdocurl           TYPE string,
         order_item_id        TYPE i,
         shipmentitemid       TYPE i,
         productname          TYPE string,
         receivedqty          TYPE i,
         acceptedqty          TYPE i,
         rejectedqty          TYPE i,
         inspected_by         TYPE string,
         agency_name          TYPE string,
         inspection_cert_no   TYPE string,
         reason               TYPE string,
         freightcurrency      TYPE string,
         freightvalue         TYPE p LENGTH 10 DECIMALS 2,
       END OF ty_display,
       tt_display TYPE STANDARD TABLE OF ty_display WITH DEFAULT KEY.

DATA: lo_client   TYPE REF TO if_http_client,
      ls_request  TYPE ty_request,
      ls_response TYPE ty_response,
      ls_crac     TYPE ty_crac,
      ls_item     TYPE ty_shipment_item,
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
  ls_request-method        = 'getCrac'.
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

*--- 6a. Flatten to one row per cracShipmentitem
  CLEAR lt_display.
  LOOP AT ls_response-data INTO ls_crac.
    LOOP AT ls_crac-cracshipmentitems INTO ls_item.
      CLEAR ls_display.
      ls_display-sub                  = ls_response-sub.
      ls_display-aud                  = ls_response-aud.
      ls_display-iss                  = ls_response-iss.
      ls_display-cracnumber           = ls_crac-cracnumber.
      ls_display-orderid              = ls_crac-orderid.
      ls_display-demandid             = ls_crac-demandid.
      ls_display-invoicenumber        = ls_crac-invoicenumber.
      ls_display-consigneename        = ls_crac-consigneename.
      ls_display-consigneelastname    = ls_crac-consigneelastname.
      ls_display-consigneeaddress     = ls_crac-consigneeaddress.
      ls_display-consigneedistrict    = ls_crac-consigneedistrict.
      ls_display-consigneepin         = ls_crac-consigneepin.
      ls_display-consigneestate       = ls_crac-consigneestate.
      ls_display-consigneemobile      = ls_crac-consigneemobile.
      ls_display-consignmentamount    = ls_crac-consignmentamount.
      ls_display-consignmentcurrency  = ls_crac-consignmentcurrency.
      ls_display-autocracflag         = ls_crac-autocracflag.
      ls_display-cracamount           = ls_crac-cracamount.
      ls_display-craccurrency         = ls_crac-craccurrency.
      ls_display-cracverificationdate = ls_crac-cracverificationdate.
      ls_display-craccreateddate      = ls_crac-craccreateddate.
      ls_display-prcreceiveddate      = ls_crac-prcreceiveddate.
      ls_display-cracdocurl           = ls_crac-cracdocurl.
      ls_display-order_item_id        = ls_item-order_item_id.
      ls_display-shipmentitemid       = ls_item-shipmentitemid.
      ls_display-productname          = ls_item-productname.
      ls_display-receivedqty          = ls_item-receivedqty.
      ls_display-acceptedqty          = ls_item-acceptedqty.
      ls_display-rejectedqty          = ls_item-rejectedqty.
      ls_display-inspected_by         = ls_item-inspected_by.
      ls_display-agency_name          = ls_item-agency_name.
      ls_display-inspection_cert_no   = ls_item-inspection_cert_no.
      ls_display-reason               = ls_item-reason.
      ls_display-freightcurrency      = ls_item-freightcharge-currency.
      ls_display-freightvalue         = ls_item-freightcharge-value.
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
          |{ ls_response-sub } - { ls_response-aud } - { ls_response-iss } - { lines( lt_display ) } item(s)| ).
        lo_alv->display( ).
      CATCH cx_salv_msg INTO lx_salv.
        WRITE: / 'ALV error:', lx_salv->get_text( ).
    ENDTRY.
  ELSE.
    WRITE: / 'HTTP', lv_code, lv_reason.
    WRITE: / 'No CRAC rows returned. Raw response:'.
    WRITE: / lv_response.
  ENDIF.
