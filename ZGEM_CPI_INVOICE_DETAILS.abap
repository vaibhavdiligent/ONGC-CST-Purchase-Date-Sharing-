*&---------------------------------------------------------------------*
*& Report ZGEM_CPI_INVOICE_DETAILS
*&---------------------------------------------------------------------*
*& Invoice Details (3.5) - GeM CPI integration.
*&   1. Generate SEK token via proxy ZGEM_TOKENCO_SI_SECURITY_TOKEN.
*&   2. Call CPI through SM59 destination CPI_HTTP_GEM.
*&   3. Path -> CPI derives CamelHttpPath (sender endpoint must end with /*).
*&   4. POST JSON body; SEK token sent as header 'token' = Bearer <token>.
*&   5. Parse response and display as ALV grid (one row per product).
*&
*& Real response shape:
*&   {"sub":..,"aud":..,"iss":..,"data":[{
*&     "consignmentDetails":{ consignee fields, "products":[{product fields}] },
*&     invoice header fields (gemInvoiceNumber, taxInvoiceNumber, vendor*, ...),
*&     "invoiceItems":[{ tax amount objects per product line }]
*&   }]}
*&
*& Note: consignmentDetails is a SINGLE object (not an array).
*&       invoiceItems are matched to products by order_item_id.
*&       Tax amount fields (sgst, cgst, igst, etc.) are nested objects
*&       {"currency":..,"value":..}; value is flattened into display columns.
*&---------------------------------------------------------------------*
REPORT zgem_cpi_invoice_details.

CONSTANTS: c_dest TYPE rfcdest VALUE 'CPI_HTTP_GEM'.

PARAMETERS:
            p_head  TYPE char70 LOWER CASE DEFAULT 'Invoice Details (3.5)', " ALV list header (editable)
            p_user  TYPE string LOWER CASE DEFAULT 'clientname',
            p_buyer TYPE string LOWER CASE DEFAULT 'buyerID',   " optional
            p_from  TYPE string LOWER CASE,                      " from_date (mandatory)
            p_to    TYPE string LOWER CASE,                      " to_date (mandatory)
            p_path  TYPE string LOWER CASE DEFAULT '/http/GEM/Sync/InvoiceDetails'.

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

*--- Reusable currency-amount nested object type (used by all tax fields).
TYPES: BEGIN OF ty_amt,
         currency TYPE string,
         value    TYPE p LENGTH 13 DECIMALS 2,
       END OF ty_amt.

*--- Product inside consignmentDetails
TYPES: BEGIN OF ty_product,
         totalvalue           TYPE p LENGTH 13 DECIMALS 2,
         order_item_id        TYPE i,
         unitprice            TYPE p LENGTH 13 DECIMALS 2,
         productcode          TYPE string,
         offering_type        TYPE string,
         expecteddeliverydate TYPE string,
         productbrand         TYPE string,
         quantityordered      TYPE i,
         quantityunittype     TYPE string,
         productname          TYPE string,
         tdsunderincometax    TYPE string,
         tdsundergst          TYPE string,
       END OF ty_product,
       tt_product TYPE STANDARD TABLE OF ty_product WITH DEFAULT KEY.

*--- consignmentDetails (single object, not array)
TYPES: BEGIN OF ty_consignment,
         consigneestate    TYPE string,
         consigneelastname TYPE string,
         consigneemobile   TYPE string,
         consigneefname    TYPE string,
         consigneepin      TYPE string,
         consigneedistrict TYPE string,
         consigneeaddress  TYPE string,
         products          TYPE tt_product,
       END OF ty_consignment.

*--- invoiceItems (one per product line, matched by order_item_id)
TYPES: BEGIN OF ty_invoice_item,
         sgst            TYPE ty_amt,
         quantity        TYPE i,
         freight_cgst    TYPE ty_amt,
         freight         TYPE ty_amt,
         rounding_off    TYPE string,
         cgst            TYPE ty_amt,
         cess            TYPE ty_amt,
         freight_utgst   TYPE ty_amt,
         igst            TYPE ty_amt,
         utgst           TYPE ty_amt,
         tax_rate        TYPE string,
         cess_amount     TYPE string,
         order_item_id   TYPE i,
         gst_uq_code     TYPE string,
         freight_igst    TYPE ty_amt,
         cess_in_quantum TYPE string,
         slot_id         TYPE string,
         freight_cess    TYPE ty_amt,
         gst_uq_name     TYPE string,
         cess_rate       TYPE string,
         taxable_amount  TYPE string,
         freight_sgst    TYPE ty_amt,
       END OF ty_invoice_item,
       tt_invoice_item TYPE STANDARD TABLE OF ty_invoice_item WITH DEFAULT KEY.

*--- One invoice (one element of data[])
TYPES: BEGIN OF ty_invoice,
         consignmentdetails     TYPE ty_consignment,
         dispatchdate           TYPE string,
         itceligible            TYPE string,
         geminvoiceamount       TYPE string,
         geminvoicecurrency     TYPE string,
         geminvoicenumber       TYPE string,
         geminvoicedate         TYPE string,
         invoicefile            TYPE string,
         iscompgstreg           TYPE string,
         isregenerated          TYPE string,
         orderdate              TYPE string,
         svcapplicable          TYPE string,
         excessapplicable       TYPE string,
         orderid                TYPE string,
         receiptnoreverscharge  TYPE string,
         revchg                 TYPE string,
         taxinvoicefile         TYPE string,
         taxinvoicenumber       TYPE string,
         taxinvoicedate         TYPE string,
         tdsapplicable          TYPE string,
         trackingurl            TYPE string,
         transactionid          TYPE string,
         vendoraddress          TYPE string,
         vendorcode             TYPE string,
         vendordistrict         TYPE string,
         vendoremail            TYPE string,
         vendorgstn             TYPE string,
         vendorgstnverfied      TYPE string,
         vendormobile           TYPE string,
         vendorname             TYPE string,
         vendorpincode          TYPE string,
         vendorstate            TYPE string,
         vendoruniqueid         TYPE string,
         vendorgststatus        TYPE string,
         invoiceitems           TYPE tt_invoice_item,
         placeofsupply          TYPE string,
         supplytype             TYPE string,
         placeofsupplystatecode TYPE string,
         placeofsupplystate     TYPE string,
         posgstin               TYPE string,
         einvoice_data          TYPE string,
       END OF ty_invoice,
       tt_invoice TYPE STANDARD TABLE OF ty_invoice WITH DEFAULT KEY.

TYPES: BEGIN OF ty_response,
         sub  TYPE string,
         aud  TYPE string,
         iss  TYPE string,
         data TYPE tt_invoice,
       END OF ty_response.

*--- Flat display: one row per product with key invoice + consignee + tax fields
TYPES: BEGIN OF ty_display,
         sub                    TYPE string,
         aud                    TYPE string,
         iss                    TYPE string,
         geminvoicenumber       TYPE string,
         geminvoicedate         TYPE string,
         geminvoiceamount       TYPE string,
         geminvoicecurrency     TYPE string,
         orderid                TYPE string,
         orderdate              TYPE string,
         transactionid          TYPE string,
         taxinvoicenumber       TYPE string,
         taxinvoicedate         TYPE string,
         vendorname             TYPE string,
         vendorgstn             TYPE string,
         vendorstate            TYPE string,
         vendoraddress          TYPE string,
         vendorcode             TYPE string,
         vendormobile           TYPE string,
         vendoremail            TYPE string,
         vendorpincode          TYPE string,
         vendordistrict         TYPE string,
         vendoruniqueid         TYPE string,
         vendorgststatus        TYPE string,
         vendorgstnverfied      TYPE string,
         placeofsupply          TYPE string,
         supplytype             TYPE string,
         placeofsupplystate     TYPE string,
         placeofsupplystatecode TYPE string,
         posgstin               TYPE string,
         itceligible            TYPE string,
         iscompgstreg           TYPE string,
         isregenerated          TYPE string,
         tdsapplicable          TYPE string,
         svcapplicable          TYPE string,
         excessapplicable       TYPE string,
         dispatchdate           TYPE string,
         consigneefname         TYPE string,
         consigneelastname      TYPE string,
         consigneestate         TYPE string,
         consigneedistrict      TYPE string,
         consigneepin           TYPE string,
         consigneemobile        TYPE string,
         consigneeaddress       TYPE string,
         productname            TYPE string,
         productbrand           TYPE string,
         productcode            TYPE string,
         order_item_id          TYPE i,
         offering_type          TYPE string,
         quantityordered        TYPE i,
         quantityunittype       TYPE string,
         unitprice              TYPE p LENGTH 13 DECIMALS 2,
         totalvalue             TYPE p LENGTH 13 DECIMALS 2,
         expecteddeliverydate   TYPE string,
         tdsundergst            TYPE string,
         tdsunderincometax      TYPE string,
         item_qty               TYPE i,
         tax_rate               TYPE string,
         taxable_amount         TYPE string,
         rounding_off           TYPE string,
         sgst_val               TYPE p LENGTH 13 DECIMALS 2,
         cgst_val               TYPE p LENGTH 13 DECIMALS 2,
         igst_val               TYPE p LENGTH 13 DECIMALS 2,
         utgst_val              TYPE p LENGTH 13 DECIMALS 2,
         cess_val               TYPE p LENGTH 13 DECIMALS 2,
         cess_amount            TYPE string,
         cess_rate              TYPE string,
         cess_in_quantum        TYPE string,
         freight_val            TYPE p LENGTH 13 DECIMALS 2,
         freight_sgst_val       TYPE p LENGTH 13 DECIMALS 2,
         freight_cgst_val       TYPE p LENGTH 13 DECIMALS 2,
         freight_igst_val       TYPE p LENGTH 13 DECIMALS 2,
         freight_utgst_val      TYPE p LENGTH 13 DECIMALS 2,
         freight_cess_val       TYPE p LENGTH 13 DECIMALS 2,
         gst_uq_code            TYPE string,
         gst_uq_name            TYPE string,
         slot_id                TYPE string,
       END OF ty_display,
       tt_display TYPE STANDARD TABLE OF ty_display WITH DEFAULT KEY.

DATA: lo_client   TYPE REF TO if_http_client,
      ls_request  TYPE ty_request,
      ls_response TYPE ty_response,
      ls_inv      TYPE ty_invoice,
      ls_prod     TYPE ty_product,
      ls_item     TYPE ty_invoice_item,
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
  ls_request-method        = 'getInvoice'.
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

*--- 6a. Flatten: one row per product per invoice.
*   invoiceItems are matched to products by order_item_id.
  CLEAR lt_display.
  LOOP AT ls_response-data INTO ls_inv.
    LOOP AT ls_inv-consignmentdetails-products INTO ls_prod.
*     Find the matching invoiceItem for this product line
      READ TABLE ls_inv-invoiceitems INTO ls_item
        WITH KEY order_item_id = ls_prod-order_item_id.
      IF sy-subrc <> 0. CLEAR ls_item. ENDIF.

      CLEAR ls_display.
      ls_display-sub                    = ls_response-sub.
      ls_display-aud                    = ls_response-aud.
      ls_display-iss                    = ls_response-iss.
      ls_display-geminvoicenumber       = ls_inv-geminvoicenumber.
      ls_display-geminvoicedate         = ls_inv-geminvoicedate.
      ls_display-geminvoiceamount       = ls_inv-geminvoiceamount.
      ls_display-geminvoicecurrency     = ls_inv-geminvoicecurrency.
      ls_display-orderid                = ls_inv-orderid.
      ls_display-orderdate              = ls_inv-orderdate.
      ls_display-transactionid          = ls_inv-transactionid.
      ls_display-taxinvoicenumber       = ls_inv-taxinvoicenumber.
      ls_display-taxinvoicedate         = ls_inv-taxinvoicedate.
      ls_display-vendorname             = ls_inv-vendorname.
      ls_display-vendorgstn             = ls_inv-vendorgstn.
      ls_display-vendorstate            = ls_inv-vendorstate.
      ls_display-vendoraddress          = ls_inv-vendoraddress.
      ls_display-vendorcode             = ls_inv-vendorcode.
      ls_display-vendormobile           = ls_inv-vendormobile.
      ls_display-vendoremail            = ls_inv-vendoremail.
      ls_display-vendorpincode          = ls_inv-vendorpincode.
      ls_display-vendordistrict         = ls_inv-vendordistrict.
      ls_display-vendoruniqueid         = ls_inv-vendoruniqueid.
      ls_display-vendorgststatus        = ls_inv-vendorgststatus.
      ls_display-vendorgstnverfied      = ls_inv-vendorgstnverfied.
      ls_display-placeofsupply          = ls_inv-placeofsupply.
      ls_display-supplytype             = ls_inv-supplytype.
      ls_display-placeofsupplystate     = ls_inv-placeofsupplystate.
      ls_display-placeofsupplystatecode = ls_inv-placeofsupplystatecode.
      ls_display-posgstin               = ls_inv-posgstin.
      ls_display-itceligible            = ls_inv-itceligible.
      ls_display-iscompgstreg           = ls_inv-iscompgstreg.
      ls_display-isregenerated          = ls_inv-isregenerated.
      ls_display-tdsapplicable          = ls_inv-tdsapplicable.
      ls_display-svcapplicable          = ls_inv-svcapplicable.
      ls_display-excessapplicable       = ls_inv-excessapplicable.
      ls_display-dispatchdate           = ls_inv-dispatchdate.
      ls_display-consigneefname         = ls_inv-consignmentdetails-consigneefname.
      ls_display-consigneelastname      = ls_inv-consignmentdetails-consigneelastname.
      ls_display-consigneestate         = ls_inv-consignmentdetails-consigneestate.
      ls_display-consigneedistrict      = ls_inv-consignmentdetails-consigneedistrict.
      ls_display-consigneepin           = ls_inv-consignmentdetails-consigneepin.
      ls_display-consigneemobile        = ls_inv-consignmentdetails-consigneemobile.
      ls_display-consigneeaddress       = ls_inv-consignmentdetails-consigneeaddress.
      ls_display-productname            = ls_prod-productname.
      ls_display-productbrand           = ls_prod-productbrand.
      ls_display-productcode            = ls_prod-productcode.
      ls_display-order_item_id          = ls_prod-order_item_id.
      ls_display-offering_type          = ls_prod-offering_type.
      ls_display-quantityordered        = ls_prod-quantityordered.
      ls_display-quantityunittype       = ls_prod-quantityunittype.
      ls_display-unitprice              = ls_prod-unitprice.
      ls_display-totalvalue             = ls_prod-totalvalue.
      ls_display-expecteddeliverydate   = ls_prod-expecteddeliverydate.
      ls_display-tdsundergst            = ls_prod-tdsundergst.
      ls_display-tdsunderincometax      = ls_prod-tdsunderincometax.
      ls_display-item_qty               = ls_item-quantity.
      ls_display-tax_rate               = ls_item-tax_rate.
      ls_display-taxable_amount         = ls_item-taxable_amount.
      ls_display-rounding_off           = ls_item-rounding_off.
      ls_display-sgst_val               = ls_item-sgst-value.
      ls_display-cgst_val               = ls_item-cgst-value.
      ls_display-igst_val               = ls_item-igst-value.
      ls_display-utgst_val              = ls_item-utgst-value.
      ls_display-cess_val               = ls_item-cess-value.
      ls_display-cess_amount            = ls_item-cess_amount.
      ls_display-cess_rate              = ls_item-cess_rate.
      ls_display-cess_in_quantum        = ls_item-cess_in_quantum.
      ls_display-freight_val            = ls_item-freight-value.
      ls_display-freight_sgst_val       = ls_item-freight_sgst-value.
      ls_display-freight_cgst_val       = ls_item-freight_cgst-value.
      ls_display-freight_igst_val       = ls_item-freight_igst-value.
      ls_display-freight_utgst_val      = ls_item-freight_utgst-value.
      ls_display-freight_cess_val       = ls_item-freight_cess-value.
      ls_display-gst_uq_code            = ls_item-gst_uq_code.
      ls_display-gst_uq_name            = ls_item-gst_uq_name.
      ls_display-slot_id                = ls_item-slot_id.
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
          |{ ls_response-sub } - { ls_response-aud } - { ls_response-iss } - { lines( lt_display ) } product(s)| ).
        lo_alv->display( ).
      CATCH cx_salv_msg INTO lx_salv.
        WRITE: / 'ALV error:', lx_salv->get_text( ).
    ENDTRY.
  ELSE.
    WRITE: / 'HTTP', lv_code, lv_reason.
    WRITE: / 'No invoice rows returned. Raw response:'.
    WRITE: / lv_response.
  ENDIF.
