*&---------------------------------------------------------------------*
*& Report ZGEM_CPI_PAYMENT_STATUS
*&---------------------------------------------------------------------*
*& Payment Status (3.11) - GeM CPI integration.
*&   1. Generate SEK token via proxy ZGEM_TOKENCO_SI_SECURITY_TOKEN.
*&   2. Call CPI through SM59 destination CPI_HTTP_GEM.
*&   3. Path -> CPI derives CamelHttpPath (sender endpoint must end with /*).
*&   4. POST JSON body; SEK token sent as header 'token' = Bearer <token>.
*&   5. CPI encrypts paydata (AES/ECB/PKCS5Padding) before forwarding to GeM.
*&   6. Parse response and display raw output (tighten once real shape confirmed).
*&
*& Request shape:
*&   {"user":..,"method":"payments","paydata":{
*&     "transactionID":..,"status":..,"paymentBy":..,"contractNo":..,
*&     "gemInvoiceNo":..,"invoiceNo":..,"billNo":..,"billAmountPaid":..,
*&     "transactionDate":..,"deductedAmount":..,"deductionType":..,"bankName":..
*&     "chequeNumber":..,"bankTransactionNo":..,"demandDraftNo":..
*&     "sanctions":..,"sanctionDate":..
*&   }}
*&---------------------------------------------------------------------*
REPORT zgem_cpi_payment_status.

CONSTANTS: c_dest TYPE rfcdest VALUE 'CPI_HTTP_GEM'.

PARAMETERS:
  p_head  TYPE char70 LOWER CASE DEFAULT 'Payment Status (3.11)',
  p_user  TYPE string LOWER CASE DEFAULT 'clientname',
  p_path  TYPE string LOWER CASE DEFAULT '/http/GEM/Sync/PaymentStatus',
  p_txnid TYPE string LOWER CASE,   " transactionID  (40, Numeric)
  p_stat  TYPE string LOWER CASE,   " status         (Success/Fail)
  p_pyby  TYPE string LOWER CASE,   " paymentBy      (payment mode)
  p_cntno TYPE string LOWER CASE,   " contractNo     (GeM order no)
  p_ginvno TYPE string LOWER CASE,  " gemInvoiceNo
  p_invno TYPE string LOWER CASE,   " invoiceNo
  p_billno TYPE string LOWER CASE,  " billNo
  p_billamt TYPE string LOWER CASE, " billAmountPaid (INT)
  p_txndt TYPE string LOWER CASE,   " transactionDate (YYYY-MM-DD)
  p_dedamt TYPE string LOWER CASE,  " deductedAmount (INT)
  p_dedtyp TYPE string LOWER CASE,  " deductionType  (LD / NA)
  p_bkname TYPE string LOWER CASE,  " bankName
  p_chqno TYPE string LOWER CASE,   " chequeNumber
  p_bktnno TYPE string LOWER CASE,  " bankTransactionNo
  p_ddno  TYPE string LOWER CASE,   " demandDraftNo
  p_sanct TYPE string LOWER CASE,   " sanctions
  p_sancdt TYPE string LOWER CASE.  " sanctionDate   (YYYY-MM-DD)

*--- Token proxy objects
DATA: lo_gem_token     TYPE REF TO zgem_tokenco_si_security_token,
      proxy_data       TYPE zgem_tokenmt_security_token_se,
      lt_input         TYPE zgem_tokenmt_security_token_re,
      lo_sys_exception TYPE REF TO cx_ai_system_fault,
      err_string       TYPE string,
      gv_token         TYPE string.

*--- paydata nested object (CPI encrypts before sending to GeM)
TYPES: BEGIN OF ty_paydata,
         transactionid     TYPE string,   " transactionID
         status            TYPE string,
         paymentby         TYPE string,   " paymentBy
         contractno        TYPE string,   " contractNo
         geminvoiceno      TYPE string,   " gemInvoiceNo
         invoiceno         TYPE string,   " invoiceNo
         billno            TYPE string,   " billNo
         billamountpaid    TYPE string,   " billAmountPaid (INT in spec; string avoids decimal suffix)
         transactiondate   TYPE string,   " transactionDate YYYY-MM-DD
         deductedamount    TYPE string,   " deductedAmount  (INT in spec)
         deductiontype     TYPE string,   " deductionType
         bankname          TYPE string,   " bankName
         chequenumber      TYPE string,   " chequeNumber
         banktransactionno TYPE string,   " bankTransactionNo
         demanddraftno     TYPE string,   " demandDraftNo
         sanctions         TYPE string,
         sanctiondate      TYPE string,   " sanctionDate YYYY-MM-DD
       END OF ty_paydata.

*--- Top-level request
TYPES: BEGIN OF ty_request,
         user    TYPE string,
         method  TYPE string,
         paydata TYPE ty_paydata,
       END OF ty_request.

DATA: lo_client   TYPE REF TO if_http_client,
      ls_request  TYPE ty_request,
      ls_paydata  TYPE ty_paydata,
      lv_json     TYPE string,
      lv_response TYPE string,
      lv_code     TYPE i,
      lv_reason   TYPE string,
      lo_alv      TYPE REF TO cl_salv_table,
      lx_salv     TYPE REF TO cx_salv_msg.

*--- Generic single-row display (tighten once real response shape is confirmed)
TYPES: BEGIN OF ty_display,
         http_code TYPE i,
         reason    TYPE string,
         response  TYPE string,
       END OF ty_display,
       tt_display TYPE STANDARD TABLE OF ty_display WITH DEFAULT KEY.
DATA: lt_display TYPE tt_display,
      ls_display TYPE ty_display.

START-OF-SELECTION.

*--- 1. Validate: all paydata fields are mandatory
  IF p_txnid  IS INITIAL OR p_stat   IS INITIAL OR p_pyby   IS INITIAL OR
     p_cntno  IS INITIAL OR p_ginvno IS INITIAL OR p_invno  IS INITIAL OR
     p_billno IS INITIAL OR p_billamt IS INITIAL OR p_txndt  IS INITIAL OR
     p_dedamt IS INITIAL OR p_dedtyp IS INITIAL OR p_bkname IS INITIAL OR
     p_chqno  IS INITIAL OR p_bktnno IS INITIAL OR p_ddno   IS INITIAL OR
     p_sanct  IS INITIAL OR p_sancdt IS INITIAL.
    WRITE: / 'Error: all paydata fields are mandatory.'. RETURN.
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
  CLEAR ls_paydata.
  ls_paydata-transactionid     = p_txnid.
  ls_paydata-status            = p_stat.
  ls_paydata-paymentby         = p_pyby.
  ls_paydata-contractno        = p_cntno.
  ls_paydata-geminvoiceno      = p_ginvno.
  ls_paydata-invoiceno         = p_invno.
  ls_paydata-billno            = p_billno.
  ls_paydata-billamountpaid    = p_billamt.
  ls_paydata-transactiondate   = p_txndt.
  ls_paydata-deductedamount    = p_dedamt.
  ls_paydata-deductiontype     = p_dedtyp.
  ls_paydata-bankname          = p_bkname.
  ls_paydata-chequenumber      = p_chqno.
  ls_paydata-banktransactionno = p_bktnno.
  ls_paydata-demanddraftno     = p_ddno.
  ls_paydata-sanctions         = p_sanct.
  ls_paydata-sanctiondate      = p_sancdt.

  CLEAR ls_request.
  ls_request-user    = p_user.
  ls_request-method  = 'payments'.
  ls_request-paydata = ls_paydata.

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

*--- 6. Display raw response (tighten with typed structures once real shape is known)
  CLEAR ls_display.
  ls_display-http_code = lv_code.
  ls_display-reason    = lv_reason.
  ls_display-response  = lv_response.
  APPEND ls_display TO lt_display.

*--- 7. Show as ALV
  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                              CHANGING  t_table      = lt_display ).
      lo_alv->get_columns( )->set_optimize( abap_true ).
      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_display_settings( )->set_list_header( p_head ).
      lo_alv->display( ).
    CATCH cx_salv_msg INTO lx_salv.
      WRITE: / 'ALV error:', lx_salv->get_text( ).
      WRITE: / 'HTTP', lv_code, lv_reason.
      WRITE: / lv_response.
  ENDTRY.
