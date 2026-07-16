*&---------------------------------------------------------------------*
*& Report ZGEM_SDAC_SUMMARY
*&---------------------------------------------------------------------*
*& SDAC Summary - GeM CPI integration (customer version).
*& Selection screen: from-date / to-date only. All other request fields
*& are hard-coded. Fetched rows are saved to ZGEMC_SDACSUMM and shown in ALV.
*&---------------------------------------------------------------------*
REPORT zgem_sdac_summary.

PARAMETERS: datefrom TYPE sy-datum DEFAULT sy-datum,
            dateto   TYPE sy-datum DEFAULT sy-datum.

CONSTANTS: c_dest TYPE rfcdest VALUE 'CPI_HTTP_GEM',
           c_path TYPE string   VALUE '/http/GEM/Sync/SdacSummary'.

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

TYPES: BEGIN OF ty_sdac,
         sdacid        TYPE string,
         invoicenumber TYPE string,
       END OF ty_sdac,
       tt_sdac TYPE STANDARD TABLE OF ty_sdac WITH DEFAULT KEY.

TYPES: BEGIN OF ty_data_block,
         date  TYPE string,
         count TYPE i,
         sdacids TYPE tt_sdac,
       END OF ty_data_block,
       tt_data TYPE STANDARD TABLE OF ty_data_block WITH DEFAULT KEY.

TYPES: BEGIN OF ty_response,
         sub  TYPE string,
         aud  TYPE string,
         iss  TYPE string,
         data TYPE tt_data,
       END OF ty_response.

DATA: lo_client   TYPE REF TO if_http_client,
      ls_request  TYPE ty_request,
      ls_response TYPE ty_response,
      ls_block    TYPE ty_data_block,
      ls_sdac     TYPE ty_sdac,
      lv_json     TYPE string,
      lv_response TYPE string,
      lv_code     TYPE i,
      lv_reason   TYPE string,
      it_out      TYPE STANDARD TABLE OF zgemc_sdacsumm,
      wa_out      TYPE zgemc_sdacsumm,
      lo_alv      TYPE REF TO cl_salv_table,
      lx_salv     TYPE REF TO cx_salv_msg.

START-OF-SELECTION.

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

  CLEAR ls_request.
  ls_request-user          = 'ONGCVIDESH'.
  ls_request-method        = 'cracServiceSummary'.
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
  LOOP AT ls_response-data INTO ls_block.
    LOOP AT ls_block-sdacids INTO ls_sdac.
      CLEAR wa_out.
      wa_out-sdac_id        = ls_sdac-sdacid.
      wa_out-invoice_number = ls_sdac-invoicenumber.
      wa_out-sdate          = ls_block-date.
      wa_out-scount         = ls_block-count.
      wa_out-datefrom       = datefrom.
      wa_out-dateto         = dateto.
      wa_out-ernam          = sy-uname.
      wa_out-erdat          = sy-datum.
      MODIFY zgemc_sdacsumm FROM wa_out.
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
          |GeM SDAC Summary - { lines( it_out ) } row(s)| ).
        lo_alv->display( ).
      CATCH cx_salv_msg INTO lx_salv.
        WRITE: / 'ALV error:', lx_salv->get_text( ).
    ENDTRY.
  ELSE.
    WRITE: / 'HTTP', lv_code, lv_reason.
    WRITE: / 'No SDAC rows returned. Raw response:'.
    WRITE: / lv_response.
  ENDIF.
