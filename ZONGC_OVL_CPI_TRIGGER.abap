*&---------------------------------------------------------------------*
*& Report  ZONGC_OVL_CPI_TRIGGER
*&---------------------------------------------------------------------*
*& Purpose : Daily background job (ONGC) that TRIGGERS the SAP CPI
*&           integration flow which shares data with OVL
*&           (ONGC Videsh Limited).
*&
*& Architecture (pull via CPI):
*&   1. This ABAP job is scheduled daily in background (SM36).
*&   2. It builds a small trigger payload (run id + batch/delta date +
*&      optional interface list) and HTTP-POSTs it to the CPI iFlow
*&      sender endpoint (via an SM59 HTTP/HTTPS destination).
*&   3. CPI, on receiving the trigger, calls the read OData service
*&      (/CCBJI/TABLE_META_SRV -> TableDataSet) to PULL the delta data
*&      for that batch date, then sends it on to OVL.
*&   4. CPI returns an acknowledgement (HTTP 200/202 + a small JSON);
*&      this job logs it so SM37 shows success/failure.
*&
*&   This program does NOT read or push the business data itself - the
*&   data extraction stays in the OData service, called by CPI. The job
*&   only kicks the flow off and records the outcome, so SAP owns the
*&   schedule and the audit trail.
*&
*& Config  : The CPI endpoint host, path-prefix, authentication (Basic /
*&           OAuth / client cert) and SSL are all held in the SM59 HTTP
*&           destination (default ZCPI_OVL) - nothing sensitive is coded
*&           here. The iFlow's specific path is a parameter.
*&
*& Schedule: SM36 -> job ZONGC_OVL_CPI_DAILY -> step = this report with a
*&           variant -> daily (e.g. 02:00). Background user needs
*&           authorization for RFC destination ZCPI_OVL and S_ICF.
*&---------------------------------------------------------------------*
REPORT zongc_ovl_cpi_trigger.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_trigger,
         run_id        TYPE string,     " unique id for this run
         batch_date    TYPE string,     " delta date, YYYYMMDD
         triggered_by  TYPE syuname,    " background user
         triggered_at  TYPE string,     " YYYYMMDDHHMMSS
         source_system TYPE sysysid,    " SAP system id
         interfaces    TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
       END OF ty_trigger.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
PARAMETERS: p_date  TYPE dats DEFAULT sy-datum,              " batch/delta date
            p_dest  TYPE rfcdest DEFAULT 'ZCPI_OVL',         " SM59 HTTP dest to CPI
            p_path  TYPE string LOWER CASE
                      DEFAULT '/http/ongc/ovl/trigger'.      " CPI iFlow endpoint path
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
SELECT-OPTIONS s_intf FOR ('CHAR40') NO INTERVALS.           " optional interface/table list
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
PARAMETERS: p_test  AS CHECKBOX.                             " build payload, do NOT call CPI
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* Globals
*----------------------------------------------------------------------*
DATA: gs_trigger TYPE ty_trigger,
      gv_json    TYPE string,
      gv_code    TYPE i,
      gv_reason  TYPE string,
      gv_resp    TYPE string.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  PERFORM build_payload.

  WRITE: / '--- ONGC -> OVL CPI trigger ---'.
  WRITE: / 'Run id    :', gs_trigger-run_id.
  WRITE: / 'Batch date:', p_date.
  WRITE: / 'Destination:', p_dest, '   Path:', p_path.
  WRITE: / 'Payload   :'. WRITE: / gv_json.

  IF p_test = abap_true.
    WRITE: / 'TEST MODE - payload built, CPI NOT called.'.
    RETURN.
  ENDIF.

  PERFORM call_cpi CHANGING gv_code gv_reason gv_resp.

  IF gv_code BETWEEN 200 AND 299.
    WRITE: / 'CPI trigger OK. HTTP', gv_code, gv_reason.
    IF gv_resp IS NOT INITIAL.
      WRITE: / 'CPI response:', gv_resp.
    ENDIF.
    MESSAGE |CPI trigger sent for { p_date DATE = USER } (run { gs_trigger-run_id }).| TYPE 'S'.
  ELSE.
    WRITE: / 'CPI trigger FAILED. HTTP', gv_code, gv_reason.
    IF gv_resp IS NOT INITIAL.
      WRITE: / 'CPI response:', gv_resp.
    ENDIF.
    " Non-zero return code so SM37 shows the job as finished-with-error.
    MESSAGE |CPI trigger FAILED (HTTP { gv_code } { gv_reason }).| TYPE 'E'.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form build_payload
*&---------------------------------------------------------------------*
FORM build_payload.
  DATA lv_uuid TYPE string.

  " unique run id (UUID; fall back to date-time if UUID service fails)
  TRY.
      lv_uuid = cl_system_uuid=>create_uuid_c32_static( ).
    CATCH cx_uuid_error.
      lv_uuid = |{ sy-datum }{ sy-uzeit }{ sy-mandt }|.
  ENDTRY.

  CLEAR gs_trigger.
  gs_trigger-run_id        = lv_uuid.
  gs_trigger-batch_date    = p_date.                         " YYYYMMDD
  gs_trigger-triggered_by  = sy-uname.
  gs_trigger-triggered_at  = |{ sy-datum }{ sy-uzeit }|.
  gs_trigger-source_system = sy-sysid.

  " optional interface list from the select-option
  LOOP AT s_intf ASSIGNING FIELD-SYMBOL(<ls_i>) WHERE low IS NOT INITIAL.
    APPEND CONV string( <ls_i>-low ) TO gs_trigger-interfaces.
  ENDLOOP.

  " serialize to JSON (keys kept as-is, not camelCased)
  gv_json = /ui2/cl_json=>serialize(
              data        = gs_trigger
              compress    = abap_false
              pretty_name = /ui2/cl_json=>pretty_mode-none ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form call_cpi
*&---------------------------------------------------------------------*
FORM call_cpi CHANGING cv_code   TYPE i
                       cv_reason TYPE string
                       cv_resp   TYPE string.
  DATA: lo_http TYPE REF TO if_http_client.

  CLEAR: cv_code, cv_reason, cv_resp.

  " Create the HTTP client from the SM59 destination (host/auth/SSL there)
  cl_http_client=>create_by_destination(
    EXPORTING  destination              = p_dest
    IMPORTING  client                   = lo_http
    EXCEPTIONS argument_not_found        = 1
               destination_not_found     = 2
               destination_no_authority  = 3
               plugin_not_active         = 4
               internal_error            = 5
               OTHERS                    = 6 ).
  IF sy-subrc <> 0.
    cv_code = 0. cv_reason = |SM59 destination { p_dest } error (subrc { sy-subrc }).|.
    RETURN.
  ENDIF.

  " POST + JSON + the iFlow path
  lo_http->request->set_method( if_http_request=>co_request_method_post ).
  cl_http_utility=>set_request_uri( request = lo_http->request
                                    uri     = p_path ).
  lo_http->request->set_content_type( 'application/json' ).
  lo_http->request->set_cdata( gv_json ).
  " do not dump on http errors - we read the status ourselves
  lo_http->propertytype_logon_popup = if_http_client=>co_disabled.

  lo_http->send(
    EXCEPTIONS http_communication_failure = 1
               http_invalid_state         = 2
               http_processing_failed      = 3
               OTHERS                      = 4 ).
  IF sy-subrc <> 0.
    cv_code = 0. cv_reason = |HTTP send failed (subrc { sy-subrc }).|.
    lo_http->close( EXCEPTIONS OTHERS = 0 ).
    RETURN.
  ENDIF.

  lo_http->receive(
    EXCEPTIONS http_communication_failure = 1
               http_invalid_state         = 2
               http_processing_failed      = 3
               OTHERS                      = 4 ).
  IF sy-subrc <> 0.
    " still try to read the status/body for diagnostics
    lo_http->response->get_status( IMPORTING code = cv_code reason = cv_reason ).
    cv_resp = lo_http->response->get_cdata( ).
    IF cv_code = 0.
      cv_reason = |HTTP receive failed (subrc { sy-subrc }).|.
    ENDIF.
    lo_http->close( EXCEPTIONS OTHERS = 0 ).
    RETURN.
  ENDIF.

  lo_http->response->get_status( IMPORTING code = cv_code reason = cv_reason ).
  cv_resp = lo_http->response->get_cdata( ).
  lo_http->close( EXCEPTIONS OTHERS = 0 ).
ENDFORM.
