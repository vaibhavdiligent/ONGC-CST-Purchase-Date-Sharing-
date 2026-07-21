*&---------------------------------------------------------------------*
*& Report ZGEM_ORDER_DETAILS
*&---------------------------------------------------------------------*
*& Get Order Details (3.3) - GeM CPI integration (customer version).
*& Selection screen: from-date / to-date only. All other request fields
*& are hard-coded. Fetched rows are saved to ZGEMC_ORDERDET and shown in ALV.
*&---------------------------------------------------------------------*
REPORT zgem_order_details.

PARAMETERS: datefrom TYPE sy-datum DEFAULT sy-datum,
            dateto   TYPE sy-datum DEFAULT sy-datum.

CONSTANTS: c_dest TYPE rfcdest VALUE 'CPI_HTTP_GEM',
           c_path TYPE string   VALUE '/http/GEM/Sync/OrderDetails'.

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
         offset        TYPE string,
         limit         TYPE string,
       END OF ty_request.

*--- Response structures (component names match the JSON keys)
TYPES: BEGIN OF ty_product,
         productname      TYPE string,
         productcode      TYPE string,
         quantityordered  TYPE string,
         unitprice        TYPE string,
         totalvalue       TYPE string,
         offering_type    TYPE string,
       END OF ty_product,
       tt_product TYPE STANDARD TABLE OF ty_product WITH DEFAULT KEY.

TYPES: BEGIN OF ty_consignment,
         consigneefname TYPE string,
         consigneestate TYPE string,
         consigneepin   TYPE string,
         products       TYPE tt_product,
       END OF ty_consignment,
       tt_consignment TYPE STANDARD TABLE OF ty_consignment WITH DEFAULT KEY.

TYPES: BEGIN OF ty_order,
         orderid            TYPE string,
         orderdate          TYPE string,
         orderamount        TYPE string,
         buyername          TYPE string,
         buyeremail         TYPE string,
         buyerorg           TYPE string,
         vendorname         TYPE string,
         vendorcode         TYPE string,
         vendorgstn         TYPE string,
         vendorpan          TYPE string,
         consignmentdetails TYPE tt_consignment,
       END OF ty_order,
       tt_order TYPE STANDARD TABLE OF ty_order WITH DEFAULT KEY.

TYPES: BEGIN OF ty_response,
         sub  TYPE string,
         aud  TYPE string,
         iss  TYPE string,
         data TYPE tt_order,
       END OF ty_response.

DATA: lo_client   TYPE REF TO if_http_client,
      ls_request  TYPE ty_request,
      ls_response TYPE ty_response,
      ls_order    TYPE ty_order,
      ls_cons     TYPE ty_consignment,
      ls_prod     TYPE ty_product,
      lv_json     TYPE string,
      lv_response TYPE string,
      lv_code     TYPE i,
      lv_reason   TYPE string,
      lv_item     TYPE posnr,
      it_out      TYPE STANDARD TABLE OF zgemc_orderdet,
      wa_out      TYPE zgemc_orderdet,
      lo_alv      TYPE REF TO cl_salv_table,
      lx_salv     TYPE REF TO cx_salv_msg.

START-OF-SELECTION.

*--- 1. Generate the SEK security token via the CPI token proxy
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

*--- 2. Build the request payload (all non-date fields hard-coded)
  CLEAR ls_request.
  ls_request-user          = 'NBCCServices'.
  ls_request-method        = 'getOrders'.
  ls_request-buyer_user_id = 'OVLMM'.
  ls_request-from_date     = |{ datefrom+0(4) }-{ datefrom+4(2) }-{ datefrom+6(2) }|.
  ls_request-to_date       = |{ dateto+0(4) }-{ dateto+4(2) }-{ dateto+6(2) }|.
  ls_request-offset        = '0'.
  ls_request-limit         = '20'.

  lv_json = /ui2/cl_json=>serialize(
              data        = ls_request
              compress    = abap_true
              pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

*--- 3. Create HTTP client from SM59 destination and call CPI
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

*--- 4. Parse the response
  /ui2/cl_json=>deserialize( EXPORTING json = lv_response
                             CHANGING  data = ls_response ).

*--- 5. Flatten to one row per product, save to ZGEMC_ORDERDET, collect for ALV
  CLEAR it_out.
  LOOP AT ls_response-data INTO ls_order.
    CLEAR lv_item.
    LOOP AT ls_order-consignmentdetails INTO ls_cons.
      LOOP AT ls_cons-products INTO ls_prod.
        lv_item = lv_item + 1.
        CLEAR wa_out.
        wa_out-order_id         = ls_order-orderid.
        wa_out-item             = lv_item.
        wa_out-order_date       = ls_order-orderdate.
        wa_out-order_amount     = ls_order-orderamount.
        wa_out-buyer_name       = ls_order-buyername.
        wa_out-buyer_email      = ls_order-buyeremail.
        wa_out-buyer_org        = ls_order-buyerorg.
        wa_out-vendor_name      = ls_order-vendorname.
        wa_out-vendor_code      = ls_order-vendorcode.
        wa_out-vendor_gstn      = ls_order-vendorgstn.
        wa_out-vendor_pan       = ls_order-vendorpan.
        wa_out-product_name     = ls_prod-productname.
        wa_out-product_code     = ls_prod-productcode.
        wa_out-quantity_ordered = ls_prod-quantityordered.
        wa_out-unit_price       = ls_prod-unitprice.
        wa_out-total_value      = ls_prod-totalvalue.
        wa_out-offering_type    = ls_prod-offering_type.
        wa_out-consignee_name   = ls_cons-consigneefname.
        wa_out-consignee_state  = ls_cons-consigneestate.
        wa_out-consignee_pin    = ls_cons-consigneepin.
        wa_out-datefrom         = datefrom.
        wa_out-dateto           = dateto.
        wa_out-ernam            = sy-uname.
        wa_out-erdat            = sy-datum.
        MODIFY zgemc_orderdet FROM wa_out.
        APPEND wa_out TO it_out.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

*--- 6. Display as ALV grid
  IF it_out IS NOT INITIAL.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING  t_table      = it_out ).
        lo_alv->get_columns( )->set_optimize( abap_true ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->get_display_settings( )->set_list_header(
          |GeM Order Details - { lines( it_out ) } row(s)| ).
        lo_alv->display( ).
      CATCH cx_salv_msg INTO lx_salv.
        WRITE: / 'ALV error:', lx_salv->get_text( ).
    ENDTRY.
  ELSE.
    WRITE: / 'HTTP', lv_code, lv_reason.
    WRITE: / 'No order rows returned. Raw response:'.
    WRITE: / lv_response.
  ENDIF.
