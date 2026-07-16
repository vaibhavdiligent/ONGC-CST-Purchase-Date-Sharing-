*&---------------------------------------------------------------------*
*& Report ZGEM_CRAC_DETAILS
*&---------------------------------------------------------------------*
*& CRAC Details (3.8) - GeM CPI integration (customer version).
*& Selection screen: from-date / to-date only. All other request fields
*& are hard-coded. Fetched rows are saved to ZGEMC_CRACDET and shown in ALV.
*&---------------------------------------------------------------------*
REPORT zgem_crac_details.

PARAMETERS: datefrom TYPE sy-datum DEFAULT sy-datum,
            dateto   TYPE sy-datum DEFAULT sy-datum.

CONSTANTS: c_dest TYPE rfcdest VALUE 'CPI_HTTP_GEM',
           c_path TYPE string   VALUE '/http/GEM/Sync/CracDetails'.

DATA: lo_gem_token     TYPE REF TO zgem_tokenco_si_security_token,
      proxy_data       TYPE zgem_tokenmt_security_token_se,
      lt_input         TYPE zgem_tokenmt_security_token_re,
      lo_sys_exception TYPE REF TO cx_ai_system_fault,
      err_string       TYPE string,
      gv_token         TYPE string.

TYPES: BEGIN OF ty_request,
         user          TYPE string,
         method        TYPE string,
         buyer_user_id TYPE string,
         from_date     TYPE string,
         to_date       TYPE string,
       END OF ty_request.

TYPES: BEGIN OF ty_shipment_item,
         productname  TYPE string,
         receivedqty  TYPE string,
         acceptedqty  TYPE string,
         rejectedqty  TYPE string,
         inspected_by TYPE string,
       END OF ty_shipment_item,
       tt_shipment_item TYPE STANDARD TABLE OF ty_shipment_item WITH DEFAULT KEY.

TYPES: BEGIN OF ty_crac,
         cracnumber        TYPE string,
         orderid           TYPE string,
         invoicenumber     TYPE string,
         demandid          TYPE string,
         consigneename     TYPE string,
         consigneestate    TYPE string,
         cracamount        TYPE string,
         craccurrency      TYPE string,
         craccreateddate   TYPE string,
         cracshipmentitems TYPE tt_shipment_item,
       END OF ty_crac,
       tt_crac TYPE STANDARD TABLE OF ty_crac WITH DEFAULT KEY.

TYPES: BEGIN OF ty_response,
         sub  TYPE string,
         aud  TYPE string,
         iss  TYPE string,
         data TYPE tt_crac,
       END OF ty_response.

DATA: lo_client   TYPE REF TO if_http_client,
      ls_request  TYPE ty_request,
      ls_response TYPE ty_response,
      ls_crac     TYPE ty_crac,
      ls_item     TYPE ty_shipment_item,
      lv_json     TYPE string,
      lv_response TYPE string,
      lv_code     TYPE i,
      lv_reason   TYPE string,
      lv_item     TYPE posnr,
      it_out      TYPE STANDARD TABLE OF zgemc_cracdet,
      wa_out      TYPE zgemc_cracdet,
      lo_alv      TYPE REF TO cl_salv_table,
      lx_salv     TYPE REF TO cx_salv_msg.

START-OF-SELECTION.

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

  CLEAR ls_request.
  ls_request-user          = 'ONGCVIDESH'.
  ls_request-method        = 'getCrac'.
  ls_request-buyer_user_id = 'OVLMM'.
  ls_request-from_date     = |{ datefrom+0(4) }-{ datefrom+4(2) }-{ datefrom+6(2) }|.
  ls_request-to_date       = |{ dateto+0(4) }-{ dateto+4(2) }-{ dateto+6(2) }|.

  lv_json = /ui2/cl_json=>serialize(
              data        = ls_request
              compress    = abap_true
              pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

  cl_http_client=>create_by_destination(
    EXPORTING destination = c_dest
    IMPORTING client      = lo_client
    EXCEPTIONS OTHERS     = 1 ).
  IF sy-subrc <> 0.
    WRITE: / 'Error creating HTTP client for destination', c_dest. RETURN.
  ENDIF.

  lo_client->propertytype_logon_popup = if_http_client=>co_disabled.
  cl_http_utility=>set_request_uri( request = lo_client->request uri = c_path ).
  lo_client->request->set_method( if_http_request=>co_request_method_post ).
  lo_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
  IF gv_token IS NOT INITIAL.
    lo_client->request->set_header_field( name = 'token' value = |Bearer { gv_token }| ).
  ENDIF.

  lo_client->request->set_cdata( lv_json ).
  lo_client->send( EXCEPTIONS OTHERS = 1 ).
  IF sy-subrc <> 0.
    WRITE: / 'Error sending request to CPI'. lo_client->close( EXCEPTIONS OTHERS = 0 ). RETURN.
  ENDIF.
  lo_client->receive( EXCEPTIONS OTHERS = 1 ).
  lo_client->response->get_status( IMPORTING code = lv_code reason = lv_reason ).
  lv_response = lo_client->response->get_cdata( ).
  lo_client->close( EXCEPTIONS OTHERS = 0 ).

  /ui2/cl_json=>deserialize( EXPORTING json = lv_response
                             CHANGING  data = ls_response ).

  CLEAR it_out.
  LOOP AT ls_response-data INTO ls_crac.
    CLEAR lv_item.
    LOOP AT ls_crac-cracshipmentitems INTO ls_item.
      lv_item = lv_item + 1.
      CLEAR wa_out.
      wa_out-crac_number       = ls_crac-cracnumber.
      wa_out-item              = lv_item.
      wa_out-order_id          = ls_crac-orderid.
      wa_out-invoice_number    = ls_crac-invoicenumber.
      wa_out-demand_id         = ls_crac-demandid.
      wa_out-consignee_name    = ls_crac-consigneename.
      wa_out-consignee_state   = ls_crac-consigneestate.
      wa_out-product_name      = ls_item-productname.
      wa_out-received_qty      = ls_item-receivedqty.
      wa_out-accepted_qty      = ls_item-acceptedqty.
      wa_out-rejected_qty      = ls_item-rejectedqty.
      wa_out-crac_amount       = ls_crac-cracamount.
      wa_out-crac_currency     = ls_crac-craccurrency.
      wa_out-crac_created_date = ls_crac-craccreateddate.
      wa_out-inspected_by      = ls_item-inspected_by.
      wa_out-datefrom          = datefrom.
      wa_out-dateto            = dateto.
      wa_out-ernam             = sy-uname.
      wa_out-erdat             = sy-datum.
      MODIFY zgemc_cracdet FROM wa_out.
      APPEND wa_out TO it_out.
    ENDLOOP.
  ENDLOOP.

  IF it_out IS NOT INITIAL.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING  t_table      = it_out ).
        lo_alv->get_columns( )->set_optimize( abap_true ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->get_display_settings( )->set_list_header(
          |GeM CRAC Details - { lines( it_out ) } row(s)| ).
        lo_alv->display( ).
      CATCH cx_salv_msg INTO lx_salv.
        WRITE: / 'ALV error:', lx_salv->get_text( ).
    ENDTRY.
  ELSE.
    WRITE: / 'HTTP', lv_code, lv_reason.
    WRITE: / 'No CRAC rows returned. Raw response:'.
    WRITE: / lv_response.
  ENDIF.
