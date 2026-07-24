*&---------------------------------------------------------------------*
*& Report ZGEM_ORDER_DETAILS
*&---------------------------------------------------------------------*
*& Get Order Details (3.3) - GeM CPI integration (customer version).
*& Selection screen: from-date / to-date only. All other request fields
*& are hard-coded.
*&
*& IMPORTANT: fetched rows are saved to the STANDARD ZGEM_ORDERDET table
*& (NOT a separate customer table) because the downstream PO creation
*& process (T-Code ZGEM_INT) reads ZGEM_ORDERDET. The item counter and the
*& vendor-code (LIFNR) update logic below are what ZGEM_INT relies on, so
*& they are carried over unchanged from the proven ZGEM_ORDER_SUMMARY logic.
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
TYPES: BEGIN OF ty_cpi_request,
         user          TYPE string,
         method        TYPE string,
         buyer_user_id TYPE string,
         from_date     TYPE string,
         to_date       TYPE string,
         offset        TYPE string,
         limit         TYPE string,
       END OF ty_cpi_request.

*--- Response structures (component names match the JSON keys)
TYPES: BEGIN OF ty_cpi_product,
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
       END OF ty_cpi_product,
       tt_cpi_product TYPE STANDARD TABLE OF ty_cpi_product WITH DEFAULT KEY.

TYPES: BEGIN OF ty_cpi_consignment,
         consigneestate    TYPE string,
         consigneelastname TYPE string,
         consigneepostid   TYPE string,
         consigneemobile   TYPE string,
         consigneefname    TYPE string,
         consigneedistrict TYPE string,
         consigneepin      TYPE string,
         consigneeaddress  TYPE string,
         products          TYPE tt_cpi_product,
       END OF ty_cpi_consignment,
       tt_cpi_consignment TYPE STANDARD TABLE OF ty_cpi_consignment WITH DEFAULT KEY.

TYPES: BEGIN OF ty_cpi_order,
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
         consignmentdetails   TYPE tt_cpi_consignment,
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
       END OF ty_cpi_order,
       tt_cpi_order TYPE STANDARD TABLE OF ty_cpi_order WITH DEFAULT KEY.

TYPES: BEGIN OF ty_cpi_response,
         sub  TYPE string,
         aud  TYPE string,
         iss  TYPE string,
         data TYPE tt_cpi_order,
       END OF ty_cpi_response.

*--- Flat working structure fed into ZGEM_ORDERDET (field names match the
*    ZGEM_ORDERDET DDIC structure so MOVE-CORRESPONDING maps them 1:1).
TYPES: BEGIN OF ty_final,
         order_id               TYPE string,
         buyer_email            TYPE string,
         ifd_diary_no           TYPE string,
         consigneestate         TYPE string,
         consigneelastname      TYPE string,
         consigneemobile        TYPE string,
         consigneefname         TYPE string,
         contract_start_date    TYPE string,
         contract_end_date      TYPE string,
         sgst                   TYPE string,
         product_category_name  TYPE string,
         productbrand           TYPE string,
         hsn_code               TYPE string,
         offering_type          TYPE string,
         unitprice              TYPE string,
         totalvalue             TYPE string,
         quantityordered        TYPE string,
         tdsundergst            TYPE string,
         tdsunderincometax      TYPE string,
         productcode            TYPE string,
         expecteddeliverydate   TYPE string,
         cgst                   TYPE string,
         cess                   TYPE string,
         igst                   TYPE string,
         utgst                  TYPE string,
         product_category_id    TYPE string,
         order_item_id          TYPE string,
         consigneedistrict      TYPE string,
         consigneepin           TYPE string,
         consigneeaddress       TYPE string,
         vendor_state           TYPE string,
         designation_financial  TYPE string,
         supply_order_no        TYPE string,
         mse_gender             TYPE string,
         vendor_code            TYPE string,
         buyer_pincode          TYPE string,
         buyer_address          TYPE string,
         vendor_gstn            TYPE string,
         demand_id              TYPE string,
         order_amount           TYPE string,
         seller_id              TYPE string,
         supply_order_date      TYPE string,
         pg_mode                TYPE string,
         parent_order_id        TYPE string,
         buyer_district         TYPE string,
         mse_socail_category    TYPE string,
         vendor_bank_account_no TYPE string,
         id                     TYPE string,
         buyer_org              TYPE string,
         buyer_org_type         TYPE string,
         buyer_office           TYPE string,
         vendor_pin_code        TYPE string,
         amended_status         TYPE string,
         buyer_state            TYPE string,
         ifd_concurrance        TYPE string,
         ifd_diary_date         TYPE string,
         buyer_name             TYPE string,
         vendor_name            TYPE string,
         vendor_pan             TYPE string,
         buyer_min              TYPE string,
         vendor_bank_ifsc_code  TYPE string,
         buyer_dep              TYPE string,
         buyer_mobile           TYPE string,
         is_msme_verified       TYPE string,
         order_date             TYPE string,
         vendor_address         TYPE string,
         vendor_unique_id       TYPE string,
       END OF ty_final.

DATA: lo_client      TYPE REF TO if_http_client,
      ls_cpi_request TYPE ty_cpi_request,
      ls_cpi_resp    TYPE ty_cpi_response,
      ls_cpi_order   TYPE ty_cpi_order,
      ls_cpi_cons    TYPE ty_cpi_consignment,
      ls_cpi_prod    TYPE ty_cpi_product,
      lv_json        TYPE string,
      lv_response    TYPE string,
      lv_code        TYPE i,
      lv_reason      TYPE string.

DATA: it_final TYPE TABLE OF ty_final,
      wa_final TYPE ty_final,
      it_order TYPE TABLE OF zgem_orderdet,
      wa_order TYPE zgem_orderdet,
      order    TYPE zgem_orderdet-order_id,
      cnt      TYPE sy-index.

DATA: v_regio TYPE t005u-bland,
      v_stcd3 TYPE lfa1-stcd3,
      v_panno TYPE j_1imovend-j_1ipanno.

DATA: lo_alv  TYPE REF TO cl_salv_table,
      lx_salv TYPE REF TO cx_salv_msg.

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
  CLEAR ls_cpi_request.
  ls_cpi_request-user          = 'NBCCServices'.
  ls_cpi_request-method        = 'getOrders'.
* buyer_user_id is OPTIONAL per GeM - intentionally omitted so GeM does not filter by an OVL buyer. Set a value here only if GeM requires it.
  ls_cpi_request-from_date     = |{ datefrom+0(4) }-{ datefrom+4(2) }-{ datefrom+6(2) }|.
  ls_cpi_request-to_date       = |{ dateto+0(4) }-{ dateto+4(2) }-{ dateto+6(2) }|.
  ls_cpi_request-offset        = '0'.
  ls_cpi_request-limit         = '20'.

  lv_json = /ui2/cl_json=>serialize(
              data        = ls_cpi_request
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
                             CHANGING  data = ls_cpi_resp ).

*--- 5. Flatten to one row per product into it_final
  LOOP AT ls_cpi_resp-data INTO ls_cpi_order.
    LOOP AT ls_cpi_order-consignmentdetails INTO ls_cpi_cons.
      LOOP AT ls_cpi_cons-products INTO ls_cpi_prod.
        CLEAR wa_final.
        MOVE-CORRESPONDING ls_cpi_prod TO wa_final.
        MOVE-CORRESPONDING ls_cpi_cons TO wa_final.

        wa_final-order_id               = ls_cpi_order-orderid.
        wa_final-buyer_email            = ls_cpi_order-buyeremail.
        wa_final-ifd_diary_no           = ls_cpi_order-ifddiaryno.
        wa_final-vendor_state           = ls_cpi_order-vendorstate.
        wa_final-designation_financial  = ls_cpi_order-designationfinancial.
        wa_final-supply_order_no        = ls_cpi_order-supplyorderno.
        wa_final-mse_gender             = ls_cpi_order-msegender.
        wa_final-vendor_code            = ls_cpi_order-vendorcode.
        wa_final-buyer_pincode          = ls_cpi_order-buyerpincode.
        wa_final-buyer_address          = ls_cpi_order-buyeraddress.
        wa_final-vendor_gstn            = ls_cpi_order-vendorgstn.
        wa_final-demand_id              = ls_cpi_order-demandid.
        wa_final-order_amount           = ls_cpi_order-orderamount.
        wa_final-seller_id              = ls_cpi_order-sellerid.
        wa_final-supply_order_date      = ls_cpi_order-supplyorderdate.
        wa_final-pg_mode                = ls_cpi_order-pgmode.
        wa_final-parent_order_id        = ls_cpi_order-parentorderid.
        wa_final-buyer_district         = ls_cpi_order-buyerdistrict.
        wa_final-mse_socail_category    = ls_cpi_order-msesocialcategory.
        wa_final-vendor_bank_account_no = ls_cpi_order-vendorbankaccountno.
        wa_final-buyer_org              = ls_cpi_order-buyerorg.
        wa_final-buyer_org_type         = ls_cpi_order-buyerorgtype.
        wa_final-buyer_office           = ls_cpi_order-buyeroffice.
        wa_final-vendor_pin_code        = ls_cpi_order-vendorpin.
        wa_final-amended_status         = ls_cpi_order-amendedstatus.
        wa_final-buyer_state            = ls_cpi_order-buyerstate.
        wa_final-ifd_concurrance        = ls_cpi_order-ifdconcurrance.
        wa_final-ifd_diary_date         = ls_cpi_order-ifddiarydate.
        wa_final-buyer_name             = ls_cpi_order-buyername.
        wa_final-vendor_name            = ls_cpi_order-vendorname.
        wa_final-vendor_pan             = ls_cpi_order-vendorpan.
        wa_final-buyer_min              = ls_cpi_order-buyermin.
        wa_final-vendor_bank_ifsc_code  = ls_cpi_order-vendorbankifsccode.
        wa_final-buyer_dep              = ls_cpi_order-buyerdep.
        wa_final-buyer_mobile           = ls_cpi_order-buyermobile.
        wa_final-is_msme_verified       = ls_cpi_order-ismsmeverified.
        wa_final-order_date             = ls_cpi_order-orderdate.
        wa_final-vendor_address         = ls_cpi_order-vendoraddress.
        wa_final-vendor_unique_id       = ls_cpi_order-vendoruniqueid.

        TRANSLATE wa_final-vendor_pan  TO UPPER CASE.
        TRANSLATE wa_final-vendor_gstn TO UPPER CASE.
        APPEND wa_final TO it_final.
        CLEAR wa_final.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

*--- 6. Store into the standard ZGEM_ORDERDET table with an item counter
*        per order (ZGEM_INT / PO creation reads this table).
  IF it_final IS NOT INITIAL.
    LOOP AT it_final INTO wa_final.
      TRANSLATE wa_final-vendor_state TO UPPER CASE.
      IF wa_final-vendor_state = 'NATIONAL CAPITAL TERRITORY OF DELHI'.
        wa_final-vendor_state = 'DELHI'.
      ENDIF.

      MOVE-CORRESPONDING wa_final TO wa_order. "#EC CI_FLDEXT_OK[2610650]
      IF order <> wa_order-order_id.
        CLEAR cnt.
        order = wa_order-order_id.
      ENDIF.
      cnt = cnt + 1.
      wa_order-item = cnt.

      MODIFY zgem_orderdet FROM wa_order.
      APPEND wa_order TO it_order.
      CLEAR wa_order.
    ENDLOOP.
  ENDIF.

*--- 7. Update vendor code (LIFNR) where a vendor with the same GSTIN or PAN
*        already exists (needed by PO creation in ZGEM_INT).
  SELECT * FROM zgem_orderdet INTO TABLE @DATA(it_ord)
    WHERE lifnr EQ ' '.
  DELETE it_ord WHERE lifnr IS NOT INITIAL.
  SORT it_ord BY order_id.
  DELETE ADJACENT DUPLICATES FROM it_ord COMPARING order_id.

  SELECT l~lifnr, l~regio, l~ktokk, l~stcd3, j~j_1ipanno INTO TABLE @DATA(it_lfa1)
    FROM lfa1 AS l INNER JOIN j_1imovend AS j
    ON l~lifnr = j~lifnr
    WHERE ktokk = 'GEMV'.

  LOOP AT it_ord INTO DATA(wa_ord).
    CLEAR: v_stcd3, v_panno, v_regio.

    CONDENSE: wa_ord-vendor_pan, wa_ord-vendor_gstn.
    TRANSLATE wa_ord-vendor_pan  TO UPPER CASE.
    TRANSLATE wa_ord-vendor_gstn TO UPPER CASE.
    MOVE wa_ord-vendor_gstn TO v_stcd3.
    MOVE wa_ord-vendor_pan  TO v_panno.
    IF wa_ord-vendor_gstn IS NOT INITIAL.
      READ TABLE it_lfa1 INTO DATA(wa_lfa1) WITH KEY stcd3 = v_stcd3.
      IF sy-subrc = 0.
        UPDATE zgem_orderdet SET lifnr = wa_lfa1-lifnr
             WHERE vendor_gstn = v_stcd3.
        COMMIT WORK.
      ENDIF.
    ENDIF.

    IF wa_ord-vendor_state IS NOT INITIAL.
      TRANSLATE wa_ord-vendor_state TO UPPER CASE.
      SELECT bland FROM t005u UP TO 1 ROWS INTO v_regio
        WHERE spras = sy-langu AND land1 = 'IN'
          AND bezei LIKE wa_ord-vendor_state
        ORDER BY PRIMARY KEY.
      ENDSELECT.
    ENDIF.

    IF v_regio IS NOT INITIAL AND v_panno IS NOT INITIAL.
      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY regio = v_regio
                                               j_1ipanno = v_panno.
      IF sy-subrc = 0 AND v_stcd3 IS INITIAL.
        UPDATE zgem_orderdet SET lifnr = wa_lfa1-lifnr
           WHERE vendor_pan = v_panno.
        COMMIT WORK.
      ENDIF.
    ENDIF.

    CLEAR wa_lfa1.
  ENDLOOP.

*--- 8. Display as ALV grid
  IF it_order IS NOT INITIAL.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING  t_table      = it_order ).
        lo_alv->get_columns( )->set_optimize( abap_true ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->get_display_settings( )->set_list_header(
          |GeM Order Details - { lines( it_order ) } row(s)| ).
        lo_alv->display( ).
      CATCH cx_salv_msg INTO lx_salv.
        WRITE: / 'ALV error:', lx_salv->get_text( ).
    ENDTRY.
  ELSE.
    WRITE: / 'HTTP', lv_code, lv_reason.
    WRITE: / 'No order rows returned. Raw response:'.
    WRITE: / lv_response.
  ENDIF.
