*&---------------------------------------------------------------------*
*& Report ZGEM_CRAC_DETAILS
*&---------------------------------------------------------------------*
*& CRAC Details (3.8) - GeM CPI integration (customer version).
*& Selection screen: from-date / to-date only. All other request fields
*& are hard-coded.
*&
*& IMPORTANT: fetched rows are saved to the STANDARD ZGEM_CRACDET table
*& (NOT a separate customer table), matching the proven CRAC flow, so any
*& downstream reporting/processing that reads ZGEM_CRACDET keeps working.
*&---------------------------------------------------------------------*
REPORT zgem_crac_details.

PARAMETERS: datefrom TYPE sy-datum DEFAULT sy-datum,
            dateto   TYPE sy-datum DEFAULT sy-datum.

DATA: lo_gem_token     TYPE REF TO zgem_tokenco_si_security_token,
      proxy_data       TYPE zgem_tokenmt_security_token_se,
      lt_input         TYPE zgem_tokenmt_security_token_re,
      lo_sys_exception TYPE REF TO cx_ai_system_fault.
DATA: err_string       TYPE string.

TYPES : BEGIN OF ty_frt,
          currency TYPE zgem_cracdet-currency,
          value    TYPE zgem_cracdet-value,
        END OF ty_frt.

TYPES: BEGIN OF ty_cracship,
         order_item_id      TYPE zgem_cracdet-order_item_id,
         reason             TYPE zgem_cracdet-reason,
         receivedqty        TYPE zgem_cracdet-receivedqty,
         agency_name        TYPE zgem_cracdet-agency_name,
         acceptedqty        TYPE zgem_cracdet-acceptedqty,
         shipmentitemid     TYPE zgem_cracdet-shipmentitemid,
         inspected_by       TYPE zgem_cracdet-inspected_by,
         productname        TYPE zgem_cracdet-productname,
         inspection_cert_no TYPE zgem_cracdet-inspection_cert_no,
         rejectedqty        TYPE zgem_cracdet-rejectedqty,
         freightcharges     TYPE ty_frt,
       END OF ty_cracship.

TYPES: BEGIN OF ty_prcship,
         order_item_id TYPE zgem_cracdet-prc_order_item_id,
         reason        TYPE zgem_cracdet-prc_reason,
         receivedqty   TYPE zgem_cracdet-prc_receivedqty,
         productname   TYPE zgem_cracdet-prc_productname,
         rejectedqty   TYPE zgem_cracdet-prc_rejectedqty,
       END OF ty_prcship.

DATA: lo_sys_exception1 TYPE REF TO cx_ai_system_fault,
      token             TYPE string,
      it_detail         TYPE zcracsummarydt_cracsummary_tab,
      wa_detail         LIKE LINE OF it_detail,
      it_cracship       TYPE TABLE OF ty_cracship,
      wa_cracship       TYPE ty_cracship,
      it_prcship        TYPE TABLE OF ty_prcship,
      wa_prcship        TYPE ty_prcship,
      it_final          TYPE TABLE OF zgem_cracdet,
      wa_final          TYPE zgem_cracdet.
DATA: err_string1       TYPE string.

*--- CRAC Details (3.8) via CPI. Path/method per ZGEM_CPI_CRAC_DETAILS.
CONSTANTS: c_dest TYPE rfcdest VALUE 'CPI_HTTP_GEM',
           c_path TYPE string VALUE '/http/GEM/Sync/CracDetails'.

TYPES: BEGIN OF ty_cpi_request,
         user          TYPE string,
         method        TYPE string,
         buyer_user_id TYPE string,
         from_date     TYPE string,
         to_date       TYPE string,
       END OF ty_cpi_request.

TYPES: BEGIN OF ty_cpi_freight,
         currency TYPE string,
         value    TYPE p LENGTH 10 DECIMALS 2,
       END OF ty_cpi_freight.

TYPES: BEGIN OF ty_cpi_shipitem,
         order_item_id      TYPE i,
         reason             TYPE string,
         receivedqty        TYPE i,
         agency_name        TYPE string,
         acceptedqty        TYPE i,
         inspected_by       TYPE string,
         shipmentitemid     TYPE i,
         inspection_cert_no TYPE string,
         rejectedqty        TYPE i,
         freightcharge      TYPE ty_cpi_freight,
         productname        TYPE string,
       END OF ty_cpi_shipitem,
       tt_cpi_shipitem TYPE STANDARD TABLE OF ty_cpi_shipitem WITH DEFAULT KEY.

TYPES: BEGIN OF ty_cpi_prcitem,
         order_item_id TYPE i,
         reason        TYPE string,
         receivedqty   TYPE i,
         rejectedqty   TYPE i,
         productname   TYPE string,
       END OF ty_cpi_prcitem,
       tt_cpi_prcitem TYPE STANDARD TABLE OF ty_cpi_prcitem WITH DEFAULT KEY.

TYPES: BEGIN OF ty_cpi_crac,
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
         cracshipmentitems    TYPE tt_cpi_shipitem,
         cracverificationdate TYPE string,
         craccreateddate      TYPE string,
         demandid             TYPE string,
         invoicenumber        TYPE string,
         orderid              TYPE string,
         prcreceiveddate      TYPE string,
         prcshipmentitems     TYPE tt_cpi_prcitem,
       END OF ty_cpi_crac,
       tt_cpi_crac TYPE STANDARD TABLE OF ty_cpi_crac WITH DEFAULT KEY.

TYPES: BEGIN OF ty_cpi_response,
         sub  TYPE string,
         aud  TYPE string,
         iss  TYPE string,
         data TYPE tt_cpi_crac,
       END OF ty_cpi_response.

DATA: lo_client      TYPE REF TO if_http_client,
      ls_cpi_request TYPE ty_cpi_request,
      ls_cpi_resp    TYPE ty_cpi_response,
      ls_cpi_crac    TYPE ty_cpi_crac,
      lv_json        TYPE string,
      lv_response    TYPE string,
      lv_code        TYPE i,
      lv_reason      TYPE string,
      gv_token       TYPE string,
      lt_name_maps   TYPE /ui2/cl_json=>name_mappings.

DATA: it_fcat TYPE slis_t_fieldcat_alv,
      wa_fcat TYPE slis_fieldcat_alv.
DATA: g_save    TYPE c VALUE 'A'.
DATA: gs_layout TYPE slis_layout_alv.

DATA: obj TYPE REF TO cl_trex_json_deserializer.

START-OF-SELECTION.

  CREATE OBJECT lo_gem_token.

*--- 1. Generate the SEK security token via the CPI token proxy
  proxy_data-mt_security_token_sender-username = 'NBCCServices'.
  proxy_data-mt_security_token_sender-password = '823090987ez07u8maz0z8789qn5a4a62'.
  TRY.
      CALL METHOD lo_gem_token->si_security_token_ob
        EXPORTING output = proxy_data
        IMPORTING input  = lt_input.
    CATCH cx_ai_system_fault INTO lo_sys_exception.
      err_string = lo_sys_exception->get_text( ).
    CATCH cx_ai_application_fault.
  ENDTRY.

  token    = lt_input-mt_security_token_receiver-token.
  gv_token = token.

  IF token IS NOT INITIAL.

*--- 2. Build the request payload (all non-date fields hard-coded)
    ls_cpi_request-user          = 'NBCCServices'.
    ls_cpi_request-method        = 'getCrac'.
* buyer_user_id is OPTIONAL per GeM - intentionally omitted so GeM does not filter by an OVL buyer. Set a value here only if GeM requires it.
    ls_cpi_request-from_date     = |{ datefrom+0(4) }-{ datefrom+4(2) }-{ datefrom+6(2) }|.
    ls_cpi_request-to_date       = |{ dateto+0(4) }-{ dateto+4(2) }-{ dateto+6(2) }|.

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

*--- 4. Re-hydrate it_detail so all downstream parsing and DB-update logic
*        below stays exactly as the proven CRAC flow. cracShipmentitems /
*        prcShipmentitems are re-serialized to the raw JSON string shape the
*        existing deserialize-based loop expects. (name_mappings renames
*        "freightcharge" -> "FREIGHTcharges" to match TY_CRACSHIP.)
        lt_name_maps = VALUE #( ( abap = 'FREIGHTCHARGES' json = 'freightcharge' ) ).
        REFRESH it_detail.
        LOOP AT ls_cpi_resp-data INTO ls_cpi_crac.
          CLEAR wa_detail.
          MOVE-CORRESPONDING ls_cpi_crac TO wa_detail.
          wa_detail-crac_shipmentitems = /ui2/cl_json=>serialize(
                                            data          = ls_cpi_crac-cracshipmentitems
                                            compress      = abap_true
                                            name_mappings = lt_name_maps ).
          wa_detail-prc_shipmentitems  = /ui2/cl_json=>serialize(
                                            data        = ls_cpi_crac-prcshipmentitems
                                            compress    = abap_true
                                            pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
          APPEND wa_detail TO it_detail.
        ENDLOOP.
      ENDIF.
    ENDIF.

    CREATE OBJECT obj.
    LOOP AT it_detail INTO wa_detail.

      REFRESH: it_cracship[], it_prcship[].

      /ui2/cl_json=>deserialize( EXPORTING json = wa_detail-crac_shipmentitems
                                 CHANGING data = it_cracship ).

      /ui2/cl_json=>deserialize( EXPORTING json = wa_detail-prc_shipmentitems
                                 CHANGING data = it_prcship ).

      LOOP AT it_cracship INTO wa_cracship.
        MOVE-CORRESPONDING wa_cracship TO wa_final.
        READ TABLE it_prcship INTO wa_prcship WITH KEY order_item_id = wa_cracship-order_item_id.
        IF sy-subrc = 0.
          wa_final-prc_order_item_id = wa_prcship-order_item_id.
          wa_final-prc_reason        = wa_prcship-reason.
          wa_final-prc_receivedqty   = wa_prcship-receivedqty.
          wa_final-prc_productname   = wa_prcship-productname.
          wa_final-prc_rejectedqty   = wa_prcship-rejectedqty.
        ENDIF.
        MOVE-CORRESPONDING wa_detail TO wa_final.
        APPEND wa_final TO it_final.
        CLEAR: wa_final, wa_cracship, wa_prcship.
      ENDLOOP.

      CLEAR: wa_detail, wa_final.

    ENDLOOP.

*--- 5. Store into the standard ZGEM_CRACDET table
    MODIFY zgem_cracdet FROM TABLE it_final.

  ENDIF.

*--- 6. Display as ALV grid
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_structure_name       = 'ZGEM_CRACDET'
      i_client_never_display = 'X'
    CHANGING
      ct_fieldcat            = it_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
  ENDIF.

  LOOP AT it_fcat INTO wa_fcat.
    wa_fcat-seltext_l    = wa_fcat-fieldname.
    wa_fcat-seltext_m    = wa_fcat-fieldname.
    wa_fcat-seltext_s    = wa_fcat-fieldname.
    wa_fcat-reptext_ddic = wa_fcat-fieldname.
    MODIFY it_fcat FROM wa_fcat.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = gs_layout
      it_fieldcat        = it_fcat[]
      i_save             = g_save
    TABLES
      t_outtab           = it_final[]
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
  ENDIF.
