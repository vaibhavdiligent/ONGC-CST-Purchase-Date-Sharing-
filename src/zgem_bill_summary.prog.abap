*&---------------------------------------------------------------------*
*& Report  ZGEM_BILL_SUMMARY
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zgem_bill_summary.

PARAMETERS : datefrom TYPE sy-datum,
             dateto   TYPE sy-datum.
*             offset TYPE i,
*             limit  TYPE i.

PARAMETERS : r1 RADIOBUTTON GROUP b1,
             r2 RADIOBUTTON GROUP b1.

DATA: lo_gem_token     TYPE REF TO zgem_tokenco_si_security_token,
      proxy_data       TYPE zgem_tokenmt_security_token_se,
      lt_input         TYPE zgem_tokenmt_security_token_re,
      lo_sys_exception TYPE REF TO cx_ai_system_fault.
DATA: err_string       TYPE string.


DATA: lo_sys_exception1 TYPE REF TO cx_ai_system_fault,
      token             TYPE string.
DATA: err_string1       TYPE string.

*--- Bill Details (3.10) via CPI - real response shape confirmed by
*    ZGEM_CPI_BILL_DETAILS.abap. Replaces the old SI_BILLING_SUMMARY_OB proxy call.
CONSTANTS: c_dest TYPE rfcdest VALUE 'CPI_HTTP_GEM',
           c_path TYPE string VALUE '/http/GEM/Sync/BillSummary'.

TYPES: BEGIN OF ty_cpi_request,
         user          TYPE string,
         method        TYPE string,
         buyer_user_id TYPE string,
         from_date     TYPE string,
         to_date       TYPE string,
         offset        TYPE string,
         limit         TYPE string,
       END OF ty_cpi_request.

*   NOTE: JSON key "frieghtCharge" is intentionally misspelled in the API response.
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
         consignmentdetails   TYPE tt_cpi_consignment,
       END OF ty_cpi_order,
       tt_cpi_order TYPE STANDARD TABLE OF ty_cpi_order WITH DEFAULT KEY.

TYPES: BEGIN OF ty_cpi_response,
         sub  TYPE string,
         aud  TYPE string,
         iss  TYPE string,
         data TYPE tt_cpi_order,
       END OF ty_cpi_response.

DATA: lo_client      TYPE REF TO if_http_client,
      ls_cpi_request TYPE ty_cpi_request,
      ls_cpi_resp    TYPE ty_cpi_response,
      ls_cpi_order   TYPE ty_cpi_order,
      ls_cpi_cons    TYPE ty_cpi_consignment,
      ls_cpi_prod    TYPE ty_cpi_product,
      lv_json        TYPE string,
      lv_response    TYPE string,
      lv_code        TYPE i,
      lv_reason      TYPE string,
      gv_token       TYPE string.
FIELD-SYMBOLS :   <out_structure> TYPE any.
DATA: lr_alv TYPE REF TO cl_salv_table.
DATA: lr_colums TYPE REF TO cl_salv_columns_table.

TYPES : BEGIN OF ty_prod,
          totalvalue           TYPE string,
          unitprice            TYPE string,
          sgst                 TYPE string,
          actualdeliverydate   TYPE string,
          hsncode              TYPE string,
          freightcgst          TYPE string,
          freightutgst         TYPE string,
          expecteddeliverydate TYPE string,
          productbrand         TYPE string,
          suppliedquantity     TYPE string,
          quantityordered      TYPE string,
          freightsgst          TYPE string,
          cgst                 TYPE string,
          freightigst          TYPE string,
          cess                 TYPE string,
          productname          TYPE string,
          igst                 TYPE string,
          utgst                TYPE string,
          freightcess          TYPE string,
          productcode          TYPE string,
          acceptedquantity     TYPE string,
          offering_type        TYPE string,
          frieghtcharge        TYPE string,
          quantityunittype     TYPE string,


        END OF ty_prod.





TYPES : BEGIN OF ty_ded,
          reason            TYPE string,
          amount            TYPE string,
          name              TYPE string,
          additionaldetails TYPE string,
          type              TYPE string,
        END OF ty_ded.

TYPES : BEGIN OF ty_billcycle,
          unit  TYPE string,
          value TYPE string,
        END OF ty_billcycle.

TYPES : BEGIN OF ty_service,
          sgst                  TYPE string,
          product_category_name TYPE string,
          hsncode               TYPE string,
          offering_type         TYPE string,
          billing_cycle_details TYPE ty_billcycle,
          cgst                  TYPE string,
          cess                  TYPE string,
          igst                  TYPE string,
          utgst                 TYPE string,
          product_category_id   TYPE string,


        END OF ty_service.


TYPES : BEGIN OF ty_consignee,
          consigneestate    TYPE string,
          consigneelastname TYPE string,
          consigneemobile   TYPE string,
          consigneefname    TYPE string,
          consigneepin      TYPE string,
          products          TYPE TABLE OF ty_prod WITH NON-UNIQUE DEFAULT KEY,
          consigneedistrict TYPE string,
          consigneeaddress  TYPE string,
        END OF ty_consignee.

TYPES : BEGIN OF ty_consignees,
          consigneestate    TYPE string,
          consigneelastname TYPE string,
          consigneemobile   TYPE string,
          consigneefname    TYPE string,
          consigneepin      TYPE string,
          products          TYPE TABLE OF ty_service WITH NON-UNIQUE DEFAULT KEY,
          consigneedistrict TYPE string,
          consigneeaddress  TYPE string,
        END OF ty_consignees.

DATA: it_consignee  TYPE TABLE OF ty_consignee,
      wa_consignee  TYPE ty_consignee,
      it_consignees TYPE TABLE OF ty_consignees,
      wa_consignees TYPE ty_consignees,
      it_products   TYPE TABLE OF ty_prod,
      wa_products   TYPE ty_prod,
      it_deductions TYPE TABLE OF ty_ded,
      wa_deductions TYPE ty_ded,
      it_service    TYPE TABLE OF ty_service,
      wa_service    TYPE ty_service.

TYPES: BEGIN OF ty_final,
         gem_invoice_no         TYPE string,
         id_days                TYPE string,
         order_id               TYPE string,
         buyer_email            TYPE string,
         ifd_diary_no           TYPE string,

         consigneestate         TYPE string,
         consigneelastname      TYPE string,
         consigneemobile        TYPE string,
         consigneefname         TYPE string,
         consigneepin           TYPE string,
         offering_type          TYPE string,
         totalvalue             TYPE string,
         unitprice              TYPE string,
         sgst                   TYPE string,
         actualdeliverydate     TYPE string,
         hsncode                TYPE string,
         freightcgst            TYPE string,
         freightutgst           TYPE string,
         expecteddeliverydate   TYPE string,
         productbrand           TYPE string,
         suppliedquantity       TYPE string,
         quantityordered        TYPE string,
         freightsgst            TYPE string,
         cgst                   TYPE string,
         freightigst            TYPE string,
         cess                   TYPE string,
         productname            TYPE string,
         igst                   TYPE string,
         utgst                  TYPE string,
         freightcess            TYPE string,
         productcode            TYPE string,
         acceptedquantity       TYPE string,
         frieghtcharge          TYPE string,
         quantityunittype       TYPE string,
         consigneedistrict      TYPE string,
         consigneeaddress       TYPE string,
         vendor_state           TYPE string,
*        deductions             TYPE string,
         reason                 TYPE string,
         amount                 TYPE string,
         name                   TYPE string,
         additionaldetails      TYPE string,
         type                   TYPE string,
         contract_file          TYPE string,
         designation_financial  TYPE string,
         supply_order_no        TYPE string,
         vendor_code            TYPE string,
         buyer_pincode          TYPE string,
         fa_file                TYPE string,
         invoice_file           TYPE string,
         bill_amount            TYPE string,
         buyer_address          TYPE string,
         vendor_gstn            TYPE string,
         demand_id              TYPE string,
         order_amount           TYPE string,
         seller_id              TYPE string,
         supply_order_date      TYPE string,
         crac_file              TYPE string,
         buyer_district         TYPE string,
         vendor_bank_account_no TYPE string,
         id                     TYPE string,
         buyer_org              TYPE string,
         invoice_no             TYPE string,
         bill_no                TYPE string,
         bill_file              TYPE string,
         vendor_pin_code        TYPE string,
         receipt_date           TYPE string,
         buyer_state            TYPE string,
         ifd_concurrance        TYPE string,
         ifd_diary_date         TYPE string,
         bill_date              TYPE string,
         crac_date              TYPE string,
         buyer_name             TYPE string,
         invoice_date           TYPE string,
         vendor_name            TYPE string,
         vendor_pan             TYPE string,
         vendor_bank_ifsc_code  TYPE string,
         buyer_mobile           TYPE string,
         receipt_no             TYPE string,
         id_amount              TYPE string,
         order_date             TYPE string,
         vendor_address         TYPE string,

       END OF ty_final.
DATA : ls_data  TYPE ty_final,
       it_final TYPE TABLE OF ty_final,
       wa_final TYPE ty_final,
       it_inv   TYPE TABLE OF zgem_billdet,
       wa_inv   TYPE zgem_billdet,
       it_bill  TYPE zgem_bill,
       wa_bill  TYPE zgem_bill,
       inv      TYPE zgem_billdet-gem_invoice_no,
       cnt      TYPE sy-index.

DATA: lv_bill_amount TYPE decfloat34. "--------> Declared by Aparna on 15.12.2024


DATA: parsee TYPE REF TO cl_gis_json_parser.
DATA: out1 TYPE REF TO data.
DATA :it_fcat TYPE  slis_t_fieldcat_alv.
DATA :wa_fcat TYPE  slis_fieldcat_alv.
FIELD-SYMBOLS:    <fs1> TYPE any,
                  <fs2> TYPE any.
CREATE OBJECT: lo_gem_token.



IF r1 = 'X'.

  proxy_data-mt_security_token_sender-username  = 'NBCCServices'.
  proxy_data-mt_security_token_sender-password  = '823090987ez07u8maz0z8789qn5a4a62'.


* """"""""""""" UAT ID PASSWORD""""""""""""""""""
*  proxy_data-mt_security_token_sender-username  = 'SAHAY29062020'.
*  proxy_data-mt_security_token_sender-password  = 'Test@29062020'.
*  """""""""""" UAT ID PASSWORD""""""""""""""""""

*  proxy_data-mt_security_token_sender-username  = 'Alex123'.
*  proxy_data-mt_security_token_sender-password  = 'password'.
  TRY.
      CALL METHOD lo_gem_token->si_security_token_ob
        EXPORTING
          output = proxy_data
        IMPORTING
          input  = lt_input.
    CATCH cx_ai_system_fault INTO lo_sys_exception.
      err_string = lo_sys_exception->get_text( ).
    CATCH cx_ai_application_fault .
  ENDTRY.

  token = lt_input-mt_security_token_receiver-token.
  gv_token = token.

  IF token IS NOT INITIAL.
    ls_cpi_request-user          = 'ONGCVIDESH'.
    ls_cpi_request-method        = 'billSummary'.
    ls_cpi_request-buyer_user_id = 'OVLMM'.
    ls_cpi_request-from_date     = |{ datefrom+0(4) }-{ datefrom+4(2) }-{ datefrom+6(2) }|.
    ls_cpi_request-to_date       = |{ dateto+0(4) }-{ dateto+4(2) }-{ dateto+6(2) }|.
    ls_cpi_request-offset        = '0'.
    ls_cpi_request-limit         = '12'.
  ENDIF.

*--- Build the JSON request payload and call CPI directly (was: proxy call
*    to lo_gem_billsumm->si_billing_summary_ob). Path/shape per ZGEM_CPI_BILL_DETAILS.
  lv_json = /ui2/cl_json=>serialize(
              data        = ls_cpi_request
              compress    = abap_true
              pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

  cl_http_client=>create_by_destination(
    EXPORTING destination = c_dest
    IMPORTING client      = lo_client
    EXCEPTIONS OTHERS     = 1 ).
  IF sy-subrc <> 0.
    WRITE: / 'Error creating HTTP client for destination', c_dest.
  ELSE.
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
      WRITE: / 'Error sending request to CPI'.
      lo_client->close( EXCEPTIONS OTHERS = 0 ).
    ELSE.
      lo_client->receive( EXCEPTIONS OTHERS = 1 ).
      lo_client->response->get_status( IMPORTING code = lv_code reason = lv_reason ).
      lv_response = lo_client->response->get_cdata( ).
      lo_client->close( EXCEPTIONS OTHERS = 0 ).

      /ui2/cl_json=>deserialize( EXPORTING json = lv_response
                                 CHANGING  data = ls_cpi_resp ).
    ENDIF.
  ENDIF.

  DATA lv_bill_amt TYPE string.

*   Flatten the typed response straight into it_final (one row per product).
*   NOTE: the old code branched on raw '"offering_type":"goods"' / '"bill_amount"'
*   text searches to pick between a "goods" and a "services" sub-schema. The real
*   Bill Details API returns one product shape with its own offering_type field,
*   so that is used directly instead (per confirmed decision). A handful of
*   services-only fields the old "services" branch used (billing cycle,
*   product_category_name/id) and a few SOAP-only fields (id, id_days, id_amount,
*   freightcess) are NOT present on this confirmed API response and are
*   therefore left blank rather than guessed.
  LOOP AT ls_cpi_resp-data INTO ls_cpi_order.

    /ui2/cl_json=>deserialize( EXPORTING json = ls_cpi_order-deductions
                               CHANGING data = it_deductions ).

    LOOP AT ls_cpi_order-consignmentdetails INTO ls_cpi_cons.
      LOOP AT ls_cpi_cons-products INTO ls_cpi_prod.
        CLEAR wa_final.
        MOVE-CORRESPONDING ls_cpi_prod TO wa_final.
        MOVE-CORRESPONDING ls_cpi_cons TO wa_final.

        IF wa_final-totalvalue CA sy-abcde.
          wa_final-totalvalue = ls_cpi_order-billamount.
        ENDIF.

        wa_final-gem_invoice_no         = ls_cpi_order-geminvoiceno.
        wa_final-order_id               = ls_cpi_order-orderid.
        wa_final-buyer_email            = ls_cpi_order-buyeremail.
        wa_final-ifd_diary_no           = ls_cpi_order-ifddiaryno.
        wa_final-vendor_state           = ls_cpi_order-vendorstate.
        wa_final-contract_file          = ls_cpi_order-contractfile.
        wa_final-designation_financial  = ls_cpi_order-designationfinancial.
        wa_final-supply_order_no        = ls_cpi_order-supplyorderno.
        wa_final-vendor_code            = ls_cpi_order-vendorcode.
        wa_final-buyer_pincode          = ls_cpi_order-buyerpincode.
        wa_final-fa_file                = ls_cpi_order-fafile.
        wa_final-invoice_file           = ls_cpi_order-invoicefile.
        wa_final-bill_amount            = ls_cpi_order-billamount.
        wa_final-buyer_address          = ls_cpi_order-buyeraddress.
        wa_final-vendor_gstn            = ls_cpi_order-vendorgstn.
        wa_final-demand_id              = ls_cpi_order-demandid.
        wa_final-order_amount           = ls_cpi_order-orderamount.
        wa_final-seller_id              = ls_cpi_order-sellerid.
        wa_final-supply_order_date      = ls_cpi_order-supplyorderdate.
        wa_final-crac_file              = ls_cpi_order-cracfile.
        wa_final-buyer_district         = ls_cpi_order-buyerdistrict.
        wa_final-vendor_bank_account_no = ls_cpi_order-vendorbankaccountno.
        wa_final-buyer_org              = ls_cpi_order-buyerorg.
        wa_final-invoice_no             = ls_cpi_order-invoiceno.
        wa_final-bill_no                = ls_cpi_order-billno.
        wa_final-bill_file              = ls_cpi_order-billfile.
        wa_final-vendor_pin_code        = ls_cpi_order-vendorpin.
        wa_final-receipt_date           = ls_cpi_order-receiptdate.
        wa_final-buyer_state            = ls_cpi_order-buyerstate.
        wa_final-ifd_concurrance        = ls_cpi_order-ifdconcurrance.
        wa_final-ifd_diary_date         = ls_cpi_order-ifddiarydate.
        wa_final-bill_date              = ls_cpi_order-billdate.
        wa_final-crac_date              = ls_cpi_order-cracdate.
        wa_final-buyer_name             = ls_cpi_order-buyername.
        wa_final-invoice_date           = ls_cpi_order-invoicedate.
        wa_final-vendor_name            = ls_cpi_order-vendorname.
        wa_final-vendor_pan             = ls_cpi_order-vendorpan.
        wa_final-vendor_bank_ifsc_code  = ls_cpi_order-vendorbankifsccode.
        wa_final-buyer_mobile           = ls_cpi_order-buyermobile.
        wa_final-receipt_no             = ls_cpi_order-receiptno.
        wa_final-order_date             = ls_cpi_order-orderdate.
        wa_final-vendor_address         = ls_cpi_order-vendoraddress.

        LOOP AT it_deductions INTO wa_deductions.
          MOVE-CORRESPONDING wa_deductions TO wa_final.
        ENDLOOP.
        APPEND wa_final TO it_final.
        CLEAR: wa_final, wa_deductions.
      ENDLOOP.
    ENDLOOP.

    REFRESH it_deductions.
    CLEAR wa_final.
  ENDLOOP.
*
*

*BREAK-POINT.
  LOOP AT it_final INTO wa_final.
*--------------------------------" Changes begins by Aparna on 15.12.2024 "---------------------------*
    IF wa_final-bill_amount CA sy-abcde.
      CLEAR lv_bill_amt.
*      lv_bill_amount = wa_final-bill_amount.
      lv_bill_amount = wa_final-order_amount.   " add by rohit  on 06-06-2025
      CLEAR wa_final-bill_amount.
      wa_final-bill_amount = lv_bill_amount.
    ENDIF.
*--------------------------------" Changes ended by Aparna on 15.12.2024 "---------------------------*
" Code Remediation changes S4 2025_1_A Conversion **BEGIN OF CHANGE BY SAP_ABAP 09.06.2026  FOR ATC
*     MOVE-CORRESPONDING wa_final TO wa_inv.
    MOVE-CORRESPONDING wa_final TO wa_inv. "#EC CI_FLDEXT_OK[2610650]
" Code Remediation changes S4 2025_1_A Conversion **END OF CHANGE BY SAP_ABAP 09.06.2026 FOR ATC
    IF inv <> wa_inv-gem_invoice_no.
      CLEAR: cnt.
      inv = wa_inv-gem_invoice_no.
    ENDIF.
    cnt = cnt + 1.
    wa_inv-inv_id = cnt.
    MODIFY zgem_billdet FROM wa_inv.
    APPEND wa_inv TO it_inv.

    MOVE-CORRESPONDING wa_inv TO wa_bill.
    MODIFY zgem_bill FROM wa_bill.
  ENDLOOP.

  LOOP AT it_inv INTO wa_inv.
    ON CHANGE OF wa_inv-gem_invoice_no.

      SELECT * FROM EKKO INTO @DATA(WA_EKKO) UP TO 1 ROWS
 WHERE ZGEMPO = @WA_INV-ORDER_ID
 ORDER BY PRIMARY KEY .
 ENDSELECT.
      IF wa_ekko-ebeln IS NOT INITIAL.
        SELECT *
 FROM ZMM_IMS INTO @DATA(WA_IMS) UP TO 1 ROWS WHERE TRACKTYP IN ( 'M' , 'S' ) AND EBELN = @WA_EKKO-EBELN
 ORDER BY PRIMARY KEY .
 ENDSELECT.

        IF wa_ims-trackno IS NOT INITIAL.
          UPDATE zmm_ims SET gem_invoice_no = wa_inv-gem_invoice_no
          WHERE trackno = wa_ims-trackno.
          PERFORM send_mail.
        ENDIF.

      ENDIF.

    ENDON.

  ENDLOOP.


ELSE.

  SELECT * FROM zgem_billdet INTO TABLE it_inv.

ENDIF.




CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
  EXPORTING
    i_program_name         = sy-repid
*   I_INTERNAL_TABNAME     = 'IT_FINAL'
    i_structure_name       = 'ZGEM_BILLDET'
    i_client_never_display = 'X'
*   I_INCLNAME             =
*   I_BYPASSING_BUFFER     =
*   I_BUFFER_ACTIVE        =
  CHANGING
    ct_fieldcat            = it_fcat
*     EXCEPTIONS
*   INCONSISTENT_INTERFACE = 1
*   PROGRAM_ERROR          = 2
*   OTHERS                 = 3
  .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

LOOP AT it_fcat INTO wa_fcat.
*  IF wa_fcat-seltext_m is INITIAL.
  wa_fcat-seltext_l = wa_fcat-fieldname.
  wa_fcat-seltext_m = wa_fcat-fieldname.
  wa_fcat-seltext_s = wa_fcat-fieldname.
  wa_fcat-reptext_ddic = wa_fcat-fieldname.
  MODIFY it_fcat FROM wa_fcat.
*  ENDIF.

ENDLOOP.



CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
*   I_BUFFER_ACTIVE    =
*   I_INTERFACE_CHECK  = ' '
    i_callback_program = sy-repid
*   I_CALLBACK_PF_STATUS_SET    = ' '
*   I_CALLBACK_USER_COMMAND     = ' '
*   I_CALLBACK_TOP_OF_PAGE      = ' '
*   I_CALLBACK_HTML_TOP_OF_PAGE = ' '
*   I_CALLBACK_HTML_END_OF_LIST = ' '
*   I_STRUCTURE_NAME   = alv_tab
*   I_BACKGROUND_ID    = ' '
*   i_grid_title       = title
*   I_GRID_SETTINGS    = grid
*   IS_LAYOUT          =
    it_fieldcat        = it_fcat[]
*   IT_EXCLUDING       =
*   IT_SPECIAL_GROUPS  =
*   IT_SORT            =
*   IT_FILTER          =
*   IS_SEL_HIDE        =
*   I_DEFAULT          = 'X'
*   I_SAVE             = ' '
*   IS_VARIANT         =
*   IT_EVENTS          =
*   IT_EVENT_EXIT      =
*   IS_PRINT           =
*   IS_REPREP_ID       =
*   I_SCREEN_START_COLUMN       = 0
*   I_SCREEN_START_LINE         = 0
*   I_SCREEN_END_COLUMN         = 0
*   I_SCREEN_END_LINE  = 0
*    IMPORTING
*   E_EXIT_CAUSED_BY_CALLER     =
*   ES_EXIT_CAUSED_BY_USER      =
  TABLES
    t_outtab           = it_inv
  EXCEPTIONS
    program_error      = 1
    OTHERS             = 2.
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.






*    call METHOD cl_salv_table=>factory
**    EXPORTING
**      LIST_DISPLAY   = IF_SALV_C_BOOL_SAP=>FALSE
**      R_CONTAINER    =
**      CONTAINER_NAME =
*  IMPORTING
*    R_SALV_TABLE = LR_ALV
*  CHANGING
*    T_TABLE      = IT_inv.
*
**  TRY.
*  CALL METHOD lr_alv->get_columns
*    RECEIVING
*      value  = lr_colums
*      .
*
*   LR_alv->display( ).

*  DATA(out) = cl_demo_output=>new(
*     )->begin_section( `Order-Data` ).
*  out->write_data( it_final ).
*  out->display( ).



*ENDIF.

*DATA(out) = cl_demo_output=>new(
*     )->begin_section( `Bill-Data` ).
*  out->write_data( it_final ).
*  out->display( ).
*&---------------------------------------------------------------------*
*&      Form  SEND_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_mail .

  DATA : i_text          TYPE bcsy_text. "Table for body
  DATA : w_text          LIKE LINE OF i_text.
  DATA : i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE,
         i_record    LIKE solisti1 OCCURS 0 WITH HEADER LINE.
  DATA : lo_document     TYPE REF TO cl_document_bcs VALUE IS INITIAL. "document object

  DATA : p_sub TYPE char50. "email subject
  DATA : lo_send_request TYPE REF TO cl_bcs VALUE IS INITIAL.
  DATA : lv_data_string TYPE string.
  DATA : lv_len_in    LIKE sood-objlen.
  DATA : lv_filesize TYPE so_obj_len.

  DATA : l_attsubject TYPE sood-objdes.
  DATA : lo_sender       TYPE REF TO if_sender_bcs VALUE IS INITIAL. "sender
  DATA : lo_recipient    TYPE REF TO if_recipient_bcs VALUE IS INITIAL. "recipient
  DATA : p_email       TYPE adr6-smtp_addr,
         email_address TYPE adr6-smtp_addr,
         subrc         TYPE  sy-subrc,
         error_table   TYPE TABLE OF rpbenerr..

  DATA: t_header  TYPE STANDARD TABLE OF w3head WITH HEADER LINE, "Header
        t_fields  TYPE STANDARD TABLE OF w3fields WITH HEADER LINE, "Fields
        t_html    TYPE STANDARD TABLE OF w3html WITH HEADER LINE, "Html
        t_html1   TYPE STANDARD TABLE OF w3html WITH HEADER LINE, "Html
        wa_header TYPE w3head,
        w_head    TYPE w3head.

  CLEAR: p_sub, lo_send_request.
  p_sub = 'Post GeM Invoice'.
  lo_send_request = cl_bcs=>create_persistent( ).

  REFRESH t_html.
  CLEAR t_html.

  t_html-line = '<table>'.
  APPEND t_html.
  CLEAR t_html.
*t_html-line = '<thead>'.
* APPEND t_html.
* CLEAR t_html.
  t_html-line = '<tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<td>Dear Sir/Madam,</td></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<td><p>The GeM Invoice for the below Gem PO has been created. Please post Invoice.<p></td>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr></tr></table>'.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '<table border = "1">'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr>'.
  APPEND t_html.
  CLEAR t_html.
*    LOOP AT it_fcat.
  CONCATENATE '<th bgcolor = "#F0D086" fgcolor = "black">'
  'GeM PO Number'
  '</th>' INTO t_html-line.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '</tr>'.
  APPEND t_html.
  CLEAR t_html.

*   LOOP AT it_mail_buy INTO wa_mail_buy.
  t_html-line = '<tr>'.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '<td>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = wa_inv-order_id.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '</td>'.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '</tr>'.
  APPEND t_html.
  CLEAR t_html.
*   ENDLOOP.

  t_html-line = '</table>'.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '<table>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = ' <tr></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr><p>Regards<p></tr>'.
  APPEND t_html.
  CLEAR t_html.

*   t_html-line = '<tr><p>SAP Team - ONGC Videsh<p></tr>'.
*   APPEND t_html.
*   CLEAR t_html.
  t_html-line = '<tr><td></td></tr></table>'.
  APPEND t_html.
  CLEAR t_html.

  REFRESH i_text[].
  APPEND LINES OF t_html TO i_text.
  lo_document = cl_document_bcs=>create_document( "create document
  i_type = 'HTM' "Type of document HTM, TXT etc
  i_text =  i_text "email body internal table
  i_subject = p_sub ). "email subject here p_sub input parameter
* Pass the document to send request
  lo_send_request->set_document( lo_document ).

  TRY.
      lo_sender = cl_sapuser_bcs=>create( sy-uname ). "sender is the logged in user
* Set sender to send request
      lo_send_request->set_sender(
      EXPORTING
      i_sender = lo_sender ).
*    CATCH CX_ADDRESS_BCS.
****Catch exception here
  ENDTRY.

  SELECT * FROM ZGEM_INT_LOG INTO @DATA(WA_GINV) UP TO 1 ROWS
 WHERE GEMPO = @WA_INV-ORDER_ID
 ORDER BY PRIMARY KEY .
 ENDSELECT.
  CHECK sy-subrc = 0.
  IF wa_ginv-payau IS NOT INITIAL.
    CALL FUNCTION 'HR_FBN_GET_USER_EMAIL_ADDRESS'
      EXPORTING
        user_id       = wa_ginv-payau
        reaction      = ' '
      IMPORTING
        email_address = email_address
        subrc         = subrc
      TABLES
        error_table   = error_table.
    p_email = email_address.
    IF p_email IS NOT INITIAL.
      lo_recipient = cl_cam_address_bcs=>create_internet_address( p_email ). "Here Recipient is email input p_email
      TRY.
          lo_send_request->add_recipient(
              EXPORTING
              i_recipient = lo_recipient
              i_express = 'X' ).
*  CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
**Catch exception here
      ENDTRY.

      CLEAR : p_email.



      TRY.
** Send email
          lo_send_request->send(
          EXPORTING
          i_with_error_screen = 'X' ).
          COMMIT WORK.
          IF sy-subrc = 0. "mail sent successfully
*        CLEAR : WA_FINAL.
            MESSAGE 'Mail Sent Successfully' TYPE 'S'.
          ENDIF.
*    CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
*catch exception here
      ENDTRY.
    ENDIF.
  ENDIF.
ENDFORM.
