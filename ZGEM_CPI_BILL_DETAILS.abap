*&---------------------------------------------------------------------*
*& Report ZGEM_CPI_BILL_DETAILS
*&---------------------------------------------------------------------*
*& Get Bill Details (3.10) - GeM CPI integration.
*& Same approach as ZGEM_CPI_ORDER_DETAILS but response includes bill-
*& specific fields at order level and extra tax/qty fields at product level.
*&   1. Generate SEK token via proxy ZGEM_TOKENCO_SI_SECURITY_TOKEN.
*&   2. Call CPI through SM59 destination CPI_HTTP_GEM.
*&   3. Path -> CPI derives CamelHttpPath (sender endpoint must end with /*).
*&   4. POST JSON body; SEK token sent as header 'token' = Bearer <token>.
*&   5. Parse response and display as ALV grid (one row per product).
*&      (ALV list header is taken from selection-screen field p_head.)
*&
*& Real response shape:
*&   {"sub":..,"aud":..,"iss":..,"data":[
*&     { order fields + bill fields,
*&       "consignmentDetails":[{ consignee fields,
*&         "products":[{ product fields + tax/qty fields }] }] }
*&   ]}
*&---------------------------------------------------------------------*
REPORT zgem_cpi_bill_details.

CONSTANTS: c_dest TYPE rfcdest VALUE 'CPI_HTTP_GEM'.

PARAMETERS:
            p_head  TYPE char70 LOWER CASE DEFAULT 'Get Bill Details (3.10)', " ALV list header (editable)
            p_user  TYPE string LOWER CASE DEFAULT 'clientname',
            p_buyer TYPE string LOWER CASE DEFAULT 'buyerID',   " optional
            p_ason  TYPE string LOWER CASE DEFAULT '2023-04-12', " single-date mode
            p_from  TYPE string LOWER CASE,                      " range mode (with p_to)
            p_to    TYPE string LOWER CASE,                      " range mode (needs p_from)
            p_off   TYPE string LOWER CASE DEFAULT '0',
            p_limit TYPE string LOWER CASE DEFAULT '10',
            p_path  TYPE string LOWER CASE DEFAULT '/http/GEM/Sync/BillDetails'.

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
*   NOTE: JSON key "frieghtCharge" is intentionally misspelled in the API response.
TYPES: BEGIN OF ty_product,
         totalvalue           TYPE p LENGTH 13 DECIMALS 2,
         unitprice            TYPE p LENGTH 13 DECIMALS 2,
         expecteddeliverydate TYPE string,
         productbrand         TYPE string,
         quantityordered      TYPE i,
         productname          TYPE string,
         productcode          TYPE string,
         quantityunittype     TYPE string,
         materialnumber       TYPE string,
         order_item_id        TYPE string,
         offering_type        TYPE string,
         tdsundergst          TYPE string,
         tdsunderincometax    TYPE string,
         sgst                 TYPE string,
         freightcgst          TYPE string,
         hsncode              TYPE string,
         actualdeliverydate   TYPE string,
         suppliedquantity     TYPE string,
         acceptedquantity     TYPE string,
         frieghtcharge        TYPE string,   " typo preserved from API
         cess                 TYPE string,
         utgst                TYPE string,
         cgst                 TYPE string,
         igst                 TYPE string,
         freightsgst          TYPE string,
         freightutgst         TYPE string,
         freightigst          TYPE string,
       END OF ty_product,
       tt_product TYPE STANDARD TABLE OF ty_product WITH DEFAULT KEY.

TYPES: BEGIN OF ty_consignment,
         consigneestate    TYPE string,
         consigneelastname TYPE string,
         consigneepostid   TYPE string,
         consigneemobile   TYPE string,
         consigneefname    TYPE string,
         consigneedistrict TYPE string,
         consigneepin      TYPE string,
         consigneeaddress  TYPE string,
         products          TYPE tt_product,
       END OF ty_consignment,
       tt_consignment TYPE STANDARD TABLE OF ty_consignment WITH DEFAULT KEY.

TYPES: BEGIN OF ty_order,
         pgmode               TYPE string,
         orderid              TYPE string,
         orderdate            TYPE string,
         accepteddate         TYPE string,
         orderamount          TYPE string,
         demandid             TYPE string,
         buyerorg             TYPE string,
         buyername            TYPE string,
         buyeremail           TYPE string,
         buyermobile          TYPE string,
         buyeraddress         TYPE string,
         buyerpincode         TYPE string,
         buyerdistrict        TYPE string,
         buyerstate           TYPE string,
         buyergstn            TYPE string,
         vendorname           TYPE string,
         vendoraddress        TYPE string,
         vendorcode           TYPE string,
         vendordistrict       TYPE string,
         vendorstate          TYPE string,
         vendorpin            TYPE string,
         vendorbankaccountno  TYPE string,
         vendorbankifsccode   TYPE string,
         vendorpan            TYPE string,
         vendorgstn           TYPE string,
         vendoruniqueid       TYPE string,
         sellerid             TYPE string,
         supplyorderno        TYPE string,
         supplyorderdate      TYPE string,
         designationfinancial TYPE string,
         ifdconcurrance       TYPE string,
         ifddiaryno           TYPE string,
         ifddiarydate         TYPE string,
         contractfile         TYPE string,
         amendedstatus        TYPE string,
         parentorderid        TYPE string,
         ismsmeverified       TYPE string,
         msesocialcategory    TYPE string,
         msegender            TYPE string,
         udyamnumber          TYPE string,
         buyeruserid          TYPE string,
         buyerdep             TYPE string,
         buyermin             TYPE string,
         buyeroffice          TYPE string,
         buyerorgtype         TYPE string,
         prnumber             TYPE string,
         prdate               TYPE string,
         billno               TYPE string,
         billdate             TYPE string,
         billamount           TYPE string,
         fafile               TYPE string,
         cracfile             TYPE string,
         receiptno            TYPE string,
         receiptdate          TYPE string,
         cracdate             TYPE string,
         billfile             TYPE string,
         invoicefile          TYPE string,
         invoicedate          TYPE string,
         invoiceno            TYPE string,
         geminvoiceno         TYPE string,
         paymentinitdate      TYPE string,
         deductions           TYPE string,
         createon             TYPE string,
         transactionid        TYPE string,
         svcapplicable        TYPE string,
         excessapplicable     TYPE string,
         consignmentdetails   TYPE tt_consignment,
       END OF ty_order,
       tt_order TYPE STANDARD TABLE OF ty_order WITH DEFAULT KEY.

TYPES: BEGIN OF ty_response,
         sub  TYPE string,
         aud  TYPE string,
         iss  TYPE string,
         data TYPE tt_order,
       END OF ty_response.

*--- Flat display: one row per product (order -> consignment -> product)
TYPES: BEGIN OF ty_display,
         sub                  TYPE string,
         aud                  TYPE string,
         iss                  TYPE string,
         pgmode               TYPE string,
         orderid              TYPE string,
         orderdate            TYPE string,
         accepteddate         TYPE string,
         orderamount          TYPE string,
         demandid             TYPE string,
         buyerorg             TYPE string,
         buyername            TYPE string,
         buyeremail           TYPE string,
         buyermobile          TYPE string,
         buyeraddress         TYPE string,
         buyerpincode         TYPE string,
         buyerdistrict        TYPE string,
         buyerstate           TYPE string,
         buyergstn            TYPE string,
         vendorname           TYPE string,
         vendoraddress        TYPE string,
         vendorcode           TYPE string,
         vendordistrict       TYPE string,
         vendorstate          TYPE string,
         vendorpin            TYPE string,
         vendorbankaccountno  TYPE string,
         vendorbankifsccode   TYPE string,
         vendorpan            TYPE string,
         vendorgstn           TYPE string,
         vendoruniqueid       TYPE string,
         sellerid             TYPE string,
         supplyorderno        TYPE string,
         supplyorderdate      TYPE string,
         designationfinancial TYPE string,
         ifdconcurrance       TYPE string,
         ifddiaryno           TYPE string,
         ifddiarydate         TYPE string,
         contractfile         TYPE string,
         amendedstatus        TYPE string,
         parentorderid        TYPE string,
         ismsmeverified       TYPE string,
         buyeruserid          TYPE string,
         buyerdep             TYPE string,
         buyermin             TYPE string,
         buyeroffice          TYPE string,
         buyerorgtype         TYPE string,
         prnumber             TYPE string,
         prdate               TYPE string,
         billno               TYPE string,
         billdate             TYPE string,
         billamount           TYPE string,
         fafile               TYPE string,
         cracfile             TYPE string,
         receiptno            TYPE string,
         receiptdate          TYPE string,
         cracdate             TYPE string,
         billfile             TYPE string,
         invoicefile          TYPE string,
         invoicedate          TYPE string,
         invoiceno            TYPE string,
         geminvoiceno         TYPE string,
         paymentinitdate      TYPE string,
         deductions           TYPE string,
         createon             TYPE string,
         transactionid        TYPE string,
         svcapplicable        TYPE string,
         excessapplicable     TYPE string,
         consigneestate       TYPE string,
         consigneefname       TYPE string,
         consigneelastname    TYPE string,
         consigneemobile      TYPE string,
         consigneedistrict    TYPE string,
         consigneepin         TYPE string,
         consigneeaddress     TYPE string,
         consigneepostid      TYPE string,
         productname          TYPE string,
         productbrand         TYPE string,
         productcode          TYPE string,
         quantityordered      TYPE i,
         quantityunittype     TYPE string,
         unitprice            TYPE p LENGTH 13 DECIMALS 2,
         totalvalue           TYPE p LENGTH 13 DECIMALS 2,
         expecteddeliverydate TYPE string,
         materialnumber       TYPE string,
         order_item_id        TYPE string,
         offering_type        TYPE string,
         tdsundergst          TYPE string,
         tdsunderincometax    TYPE string,
         sgst                 TYPE string,
         freightcgst          TYPE string,
         hsncode              TYPE string,
         actualdeliverydate   TYPE string,
         suppliedquantity     TYPE string,
         acceptedquantity     TYPE string,
         frieghtcharge        TYPE string,
         cess                 TYPE string,
         utgst                TYPE string,
         cgst                 TYPE string,
         igst                 TYPE string,
         freightsgst          TYPE string,
         freightutgst         TYPE string,
         freightigst          TYPE string,
       END OF ty_display,
       tt_display TYPE STANDARD TABLE OF ty_display WITH DEFAULT KEY.

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
  ls_request-method = 'getbills'.
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

*--- 6a. Flatten to one row per product (order -> consignment -> product)
  CLEAR lt_display.
  LOOP AT ls_response-data INTO ls_order.
    LOOP AT ls_order-consignmentdetails INTO ls_cons.
      LOOP AT ls_cons-products INTO ls_prod.
        CLEAR ls_display.
        ls_display-sub                  = ls_response-sub.
        ls_display-aud                  = ls_response-aud.
        ls_display-iss                  = ls_response-iss.
        ls_display-pgmode               = ls_order-pgmode.
        ls_display-orderid              = ls_order-orderid.
        ls_display-orderdate            = ls_order-orderdate.
        ls_display-accepteddate         = ls_order-accepteddate.
        ls_display-orderamount          = ls_order-orderamount.
        ls_display-demandid             = ls_order-demandid.
        ls_display-buyerorg             = ls_order-buyerorg.
        ls_display-buyername            = ls_order-buyername.
        ls_display-buyeremail           = ls_order-buyeremail.
        ls_display-buyermobile          = ls_order-buyermobile.
        ls_display-buyeraddress         = ls_order-buyeraddress.
        ls_display-buyerpincode         = ls_order-buyerpincode.
        ls_display-buyerdistrict        = ls_order-buyerdistrict.
        ls_display-buyerstate           = ls_order-buyerstate.
        ls_display-buyergstn            = ls_order-buyergstn.
        ls_display-vendorname           = ls_order-vendorname.
        ls_display-vendoraddress        = ls_order-vendoraddress.
        ls_display-vendorcode           = ls_order-vendorcode.
        ls_display-vendordistrict       = ls_order-vendordistrict.
        ls_display-vendorstate          = ls_order-vendorstate.
        ls_display-vendorpin            = ls_order-vendorpin.
        ls_display-vendorbankaccountno  = ls_order-vendorbankaccountno.
        ls_display-vendorbankifsccode   = ls_order-vendorbankifsccode.
        ls_display-vendorpan            = ls_order-vendorpan.
        ls_display-vendorgstn           = ls_order-vendorgstn.
        ls_display-vendoruniqueid       = ls_order-vendoruniqueid.
        ls_display-sellerid             = ls_order-sellerid.
        ls_display-supplyorderno        = ls_order-supplyorderno.
        ls_display-supplyorderdate      = ls_order-supplyorderdate.
        ls_display-designationfinancial = ls_order-designationfinancial.
        ls_display-ifdconcurrance       = ls_order-ifdconcurrance.
        ls_display-ifddiaryno           = ls_order-ifddiaryno.
        ls_display-ifddiarydate         = ls_order-ifddiarydate.
        ls_display-contractfile         = ls_order-contractfile.
        ls_display-amendedstatus        = ls_order-amendedstatus.
        ls_display-parentorderid        = ls_order-parentorderid.
        ls_display-ismsmeverified       = ls_order-ismsmeverified.
        ls_display-buyeruserid          = ls_order-buyeruserid.
        ls_display-buyerdep             = ls_order-buyerdep.
        ls_display-buyermin             = ls_order-buyermin.
        ls_display-buyeroffice          = ls_order-buyeroffice.
        ls_display-buyerorgtype         = ls_order-buyerorgtype.
        ls_display-prnumber             = ls_order-prnumber.
        ls_display-prdate               = ls_order-prdate.
        ls_display-billno               = ls_order-billno.
        ls_display-billdate             = ls_order-billdate.
        ls_display-billamount           = ls_order-billamount.
        ls_display-fafile               = ls_order-fafile.
        ls_display-cracfile             = ls_order-cracfile.
        ls_display-receiptno            = ls_order-receiptno.
        ls_display-receiptdate          = ls_order-receiptdate.
        ls_display-cracdate             = ls_order-cracdate.
        ls_display-billfile             = ls_order-billfile.
        ls_display-invoicefile          = ls_order-invoicefile.
        ls_display-invoicedate          = ls_order-invoicedate.
        ls_display-invoiceno            = ls_order-invoiceno.
        ls_display-geminvoiceno         = ls_order-geminvoiceno.
        ls_display-paymentinitdate      = ls_order-paymentinitdate.
        ls_display-deductions           = ls_order-deductions.
        ls_display-createon             = ls_order-createon.
        ls_display-transactionid        = ls_order-transactionid.
        ls_display-svcapplicable        = ls_order-svcapplicable.
        ls_display-excessapplicable     = ls_order-excessapplicable.
        ls_display-consigneestate       = ls_cons-consigneestate.
        ls_display-consigneefname       = ls_cons-consigneefname.
        ls_display-consigneelastname    = ls_cons-consigneelastname.
        ls_display-consigneemobile      = ls_cons-consigneemobile.
        ls_display-consigneedistrict    = ls_cons-consigneedistrict.
        ls_display-consigneepin         = ls_cons-consigneepin.
        ls_display-consigneeaddress     = ls_cons-consigneeaddress.
        ls_display-consigneepostid      = ls_cons-consigneepostid.
        ls_display-productname          = ls_prod-productname.
        ls_display-productbrand         = ls_prod-productbrand.
        ls_display-productcode          = ls_prod-productcode.
        ls_display-quantityordered      = ls_prod-quantityordered.
        ls_display-quantityunittype     = ls_prod-quantityunittype.
        ls_display-unitprice            = ls_prod-unitprice.
        ls_display-totalvalue           = ls_prod-totalvalue.
        ls_display-expecteddeliverydate = ls_prod-expecteddeliverydate.
        ls_display-materialnumber       = ls_prod-materialnumber.
        ls_display-order_item_id        = ls_prod-order_item_id.
        ls_display-offering_type        = ls_prod-offering_type.
        ls_display-tdsundergst          = ls_prod-tdsundergst.
        ls_display-tdsunderincometax    = ls_prod-tdsunderincometax.
        ls_display-sgst                 = ls_prod-sgst.
        ls_display-freightcgst          = ls_prod-freightcgst.
        ls_display-hsncode              = ls_prod-hsncode.
        ls_display-actualdeliverydate   = ls_prod-actualdeliverydate.
        ls_display-suppliedquantity     = ls_prod-suppliedquantity.
        ls_display-acceptedquantity     = ls_prod-acceptedquantity.
        ls_display-frieghtcharge        = ls_prod-frieghtcharge.
        ls_display-cess                 = ls_prod-cess.
        ls_display-utgst                = ls_prod-utgst.
        ls_display-cgst                 = ls_prod-cgst.
        ls_display-igst                 = ls_prod-igst.
        ls_display-freightsgst          = ls_prod-freightsgst.
        ls_display-freightutgst         = ls_prod-freightutgst.
        ls_display-freightigst          = ls_prod-freightigst.
        APPEND ls_display TO lt_display.
      ENDLOOP.
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
    WRITE: / 'No bill rows returned. Raw response:'.
    WRITE: / lv_response.
  ENDIF.
