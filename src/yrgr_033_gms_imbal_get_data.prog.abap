*&---------------------------------------------------------------------*
*& Include  YRGR_033_GMS_IMBAL_GET_DATA
*& Forms: get_data, fill_fieldcat, display, top_of_page,
*&        send_emails, send_email_posted, send_email_not_posted
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.
  CALL FUNCTION 'YRX_IMB_SETTLE_QTY_FM'
    EXPORTING
      st_date  = s_date-low
      ed_date  = s_date-high
    TABLES
      lt_final = lt_final.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FILL_FIELDCAT
*&---------------------------------------------------------------------*
FORM fill_fieldcat.
  DEFINE fcat.
    CLEAR gs_fieldcat.
    gs_fieldcat-fieldname = &1. gs_fieldcat-tabname   = 'LT_FINAL'.
    gs_fieldcat-outputlen = &2. gs_fieldcat-scrtext_l = &3.
    gs_fieldcat-scrtext_m = &3. gs_fieldcat-scrtext_s = &4.
    gs_fieldcat-no_out    = &5.
    APPEND gs_fieldcat TO gt_fieldcat.
  END-OF-DEFINITION.

  fcat 'DOCNR'    '10' 'Contract ID'                 'Cont ID'    ' '.
  fcat 'MATNR'    '10' 'Material'                    'Material'   ' '.
  fcat 'PARTNR'   '10' 'Customer'                    'Customer'   ' '.
  fcat 'LOCID'    '8'  'Location ID'                 'Loc ID'     ' '.
  fcat 'VBEGDAT'  '10' 'CT Start Date'               'CT Start'   ' '.
  fcat 'VENDDAT'  '10' 'CT End Date'                 'CT End'     ' '.
  fcat 'STAT'     '12' 'Status'                      'Status'     ' '.
  fcat 'PO_IMBAL' '15' 'Positive Imbalance'          'Pos Imbal'  ' '.
  fcat 'NE_IMBAL' '15' 'Negative Imbalance'          'Neg Imbal'  ' '.
  fcat 'VKBUR'    '5'  'Sales Office'                'Sales Off'  ' '.
  fcat 'RET_DEL'  '10' 'Return Delivery (Rtrn Del)'  'Ret Del'    'X'.
  fcat 'CRD_NOT'  '10' 'Credit Note'                 'Crd Note'   'X'.
  fcat 'SAL_ORD'  '10' 'Sales Order'                 'Sal_Ord'    'X'.
  fcat 'DO'       '10' 'DO'                           'DO'         'X'.
  fcat 'PGI'      '10' 'PGI'                          'PGI'        'X'.
  fcat 'INVOIC'   '10' 'Invoice'                     'Invoice'    'X'.
  fcat 'PUR_RET'  '10' 'Purchase Return (Pur Rtrn)'  'Pur_Ret'    'X'.
  fcat 'PO'       '10' 'PO'                           'PO'         'X'.
  fcat 'GR'       '10' 'GR'                           'GR'         'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY
*&---------------------------------------------------------------------*
FORM display.
  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING container_name = g_container.
    CREATE OBJECT dg_splitter
      EXPORTING parent  = g_custom_container
                rows    = 2
                columns = 1.
    CALL METHOD dg_splitter->get_container
      EXPORTING row = 1 column = 1
      RECEIVING container = dg_parent_html.
    CALL METHOD dg_splitter->get_container
      EXPORTING row = 2 column = 1
      RECEIVING container = dg_parent_grid.
    CALL METHOD dg_splitter->set_row_height
      EXPORTING id = 1 height = 24.
    CREATE OBJECT grid EXPORTING i_parent = dg_parent_grid.
    gs_layout-stylefname = 'CELL'.
    SET HANDLER lcl_event_handler=>top_of_page FOR grid.
    CALL METHOD grid->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = lt_exclude
        is_layout            = gs_layout
      CHANGING
        it_fieldcatalog = gt_fieldcat
        it_outtab       = lt_final[].
  ENDIF.
  CREATE OBJECT dg_dyndoc_id EXPORTING style = 'ALV_GRID'.
  CALL METHOD dg_dyndoc_id->initialize_document.
  CALL METHOD grid->list_processing_events
    EXPORTING
      i_event_name = 'TOP_OF_PAGE'
      i_dyndoc_id  = dg_dyndoc_id.
  CALL SCREEN 100.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form TOP_OF_PAGE
*&---------------------------------------------------------------------*
FORM top_of_page USING p_dyndoc_id TYPE REF TO cl_dd_document.
  DATA: dl_text(255) TYPE c,
        lv_text(30)  TYPE c,
        lv_date(10)  TYPE c.

  IF r1 EQ 'X'.
    dl_text = 'Report For Closing Imbalance Of Expired Contracts'.
  ELSEIF r3 EQ 'X'.
    dl_text = 'Report For Closing Imbalance Of Expired Contracts - Till Date'.
  ELSEIF r2 EQ 'X'.
    dl_text = 'SOP Due for Invoicing'.
  ENDIF.

  CALL METHOD p_dyndoc_id->add_text
    EXPORTING text         = dl_text
              sap_style    = cl_dd_area=>heading
              sap_emphasis = cl_dd_area=>strong.
  CALL METHOD p_dyndoc_id->new_line.
  CLEAR dl_text.
  CALL METHOD p_dyndoc_id->new_line.
  CALL METHOD p_dyndoc_id->add_gap.

  IF s_date IS NOT INITIAL.
    WRITE s_date-low DD/MM/YYYY TO lv_text.
    IF s_date-high IS NOT INITIAL.
      WRITE s_date-high DD/MM/YYYY TO lv_date.
      CONCATENATE lv_text ' to ' lv_date INTO lv_text SEPARATED BY space.
    ENDIF.
  ELSE.
    lv_text = 'Not Provided'.
  ENDIF.

  dl_text = 'Gas Date:'.
  CALL METHOD p_dyndoc_id->add_text EXPORTING text = dl_text sap_emphasis = cl_dd_area=>strong.
  CALL METHOD p_dyndoc_id->add_gap EXPORTING width = 0.
  CLEAR dl_text. dl_text = lv_text.
  CALL METHOD p_dyndoc_id->add_text EXPORTING text = dl_text sap_emphasis = cl_dd_area=>heading.
  CLEAR dl_text.
  CALL METHOD p_dyndoc_id->new_line.

  dl_text = 'Run Date:'.
  CALL METHOD p_dyndoc_id->add_text EXPORTING text = dl_text sap_emphasis = cl_dd_area=>strong.
  CLEAR dl_text.
  dl_text = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum(4) }|.
  CALL METHOD p_dyndoc_id->add_text EXPORTING text = dl_text sap_emphasis = cl_dd_area=>heading.
  CALL METHOD p_dyndoc_id->new_line.
  CLEAR dl_text.

  IF r1 EQ 'X' OR r3 EQ 'X'.
    dl_text = 'Note:'.
    CALL METHOD p_dyndoc_id->add_text EXPORTING text = dl_text sap_emphasis = cl_dd_area=>strong.
    CALL METHOD p_dyndoc_id->add_gap EXPORTING width = 6.
    CLEAR dl_text.
    dl_text = '1. Status pertains to Imbalance Posting done via YRG011N'.
    CALL METHOD p_dyndoc_id->add_text EXPORTING text = dl_text sap_emphasis = cl_dd_area=>heading.
    CALL METHOD p_dyndoc_id->new_line.
    CALL METHOD p_dyndoc_id->add_gap EXPORTING width = 16.
    CLEAR dl_text.
    dl_text = '2. Contracts for which imbalance has been shifted are not displayed'.
    CALL METHOD p_dyndoc_id->add_text EXPORTING text = dl_text sap_emphasis = cl_dd_area=>heading.
    CALL METHOD p_dyndoc_id->new_line.
    CLEAR dl_text.
    CALL METHOD p_dyndoc_id->new_line.
  ENDIF.

  DATA: dl_background_id TYPE sdydo_key VALUE space,
        dl_length        TYPE i.
  IF dg_html_cntrl IS INITIAL.
    CREATE OBJECT dg_html_cntrl EXPORTING parent = dg_parent_html.
  ENDIF.
  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING document = p_dyndoc_id bottom = space
    IMPORTING length = dl_length.
  CALL METHOD p_dyndoc_id->merge_document.
  p_dyndoc_id->html_control = dg_html_cntrl.
  CALL METHOD p_dyndoc_id->display_document
    EXPORTING reuse_control = 'X' parent = dg_parent_html
    EXCEPTIONS html_display_error = 1.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SEND_EMAILS
*& Orchestrates Posted and Not-Posted alert emails
*&---------------------------------------------------------------------*
FORM send_emails.
  PERFORM send_email_posted.
  PERFORM send_email_not_posted.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SEND_EMAIL_POSTED
*& Emails for Status = Posted (finite imbalance), grouped by Sales Office
*& TO  : Last Changed-by from YRVA_CON_WF_LOG (YY_LEVEL=0) via PA0105
*& CC  : YSD_WF_AGENT YLEVEL=4 + ngmc@gail.co.in
*& From: GAIL PARTNER CARE GMS
*& Body: HTML table; all columns including Posted Imbalance
*& Subject: Expired Contracts with finite Imbalance_SO [VKBUR]
*&---------------------------------------------------------------------*
FORM send_email_posted.
  TYPES: BEGIN OF ty_ep_vkbur, vkbur   TYPE vkbur,  END OF ty_ep_vkbur.
  TYPES: BEGIN OF ty_ep_cont,  cont_id TYPE vbeln,   END OF ty_ep_cont.
  TYPES: BEGIN OF ty_ep_pernr, pernr   TYPE persno,  END OF ty_ep_pernr.

  DATA: lt_ep_vkbur  TYPE TABLE OF ty_ep_vkbur,  ls_ep_vkbur  TYPE ty_ep_vkbur,
        lt_ep_cont   TYPE TABLE OF ty_ep_cont,    ls_ep_cont   TYPE ty_ep_cont,
        lt_ep_pernr  TYPE TABLE OF ty_ep_pernr,   ls_ep_pernr  TYPE ty_ep_pernr,
        lt_to_email  TYPE TABLE OF ad_smtpadr,
        lt_cc_email  TYPE TABLE OF ad_smtpadr,
        lv_ep_email  TYPE ad_smtpadr,
        lv_subject   TYPE c LENGTH 50,
        lv_html_body TYPE string,
        lt_soli      TYPE soli_tab,
        lv_ct_start  TYPE char10,
        lv_ct_end    TYPE char10,
        lv_po_str    TYPE c LENGTH 20,
        lv_ne_str    TYPE c LENGTH 20,
        lv_partnr    TYPE string,
        lo_send_req  TYPE REF TO cl_bcs,
        lo_document  TYPE REF TO cl_document_bcs,
        lo_sender_sap TYPE REF TO cl_sapuser_bcs,
        lo_recipient TYPE REF TO cl_cam_address_bcs.

  " Collect unique sales offices for Posted records
  LOOP AT lt_final INTO ls_final WHERE stat = 'Posted'.
    ls_ep_vkbur-vkbur = ls_final-vkbur.
    APPEND ls_ep_vkbur TO lt_ep_vkbur.
  ENDLOOP.
  SORT lt_ep_vkbur BY vkbur. DELETE ADJACENT DUPLICATES FROM lt_ep_vkbur COMPARING vkbur.

  LOOP AT lt_ep_vkbur INTO ls_ep_vkbur.
    CLEAR: lt_to_email, lt_cc_email, lt_ep_cont, lt_ep_pernr,
           lv_subject, lv_html_body, lt_soli.

    LOOP AT lt_final INTO ls_final WHERE stat = 'Posted' AND vkbur = ls_ep_vkbur-vkbur.
      ls_ep_cont-cont_id = ls_final-docnr.
      APPEND ls_ep_cont TO lt_ep_cont.
    ENDLOOP.
    SORT lt_ep_cont BY cont_id. DELETE ADJACENT DUPLICATES FROM lt_ep_cont COMPARING cont_id.

    " TO: latest CHANGED_BY from YRVA_CON_WF_LOG (YY_LEVEL=0) -> PA0105 email
    IF lt_ep_cont IS NOT INITIAL.
      SELECT vbeln, changed_by, changed_on, changed_time
        FROM yrva_con_wf_log
        INTO TABLE @DATA(lt_wf_ep)
        FOR ALL ENTRIES IN @lt_ep_cont
        WHERE vbeln    = @lt_ep_cont-cont_id
        AND   yy_level = 0.

      " Keep only last (most recent) changed_by per contract
      SORT lt_wf_ep BY vbeln ASCENDING changed_on DESCENDING changed_time DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_wf_ep COMPARING vbeln.

      LOOP AT lt_wf_ep INTO DATA(ls_wf_ep).
        ls_ep_pernr-pernr = ls_wf_ep-changed_by.
        APPEND ls_ep_pernr TO lt_ep_pernr.
      ENDLOOP.
      IF lt_ep_pernr IS NOT INITIAL.
        SELECT usrid_long FROM pa0105
          INTO TABLE @DATA(lt_pa_ep_to)
          FOR ALL ENTRIES IN @lt_ep_pernr
          WHERE pernr = @lt_ep_pernr-pernr AND subty = '0010' AND endda = '99991231'.
        LOOP AT lt_pa_ep_to INTO DATA(ls_pa_ep_to).
          IF ls_pa_ep_to-usrid_long IS NOT INITIAL.
            lv_ep_email = ls_pa_ep_to-usrid_long.
            APPEND lv_ep_email TO lt_to_email.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    " TO: YSD_WF_AGENT YLEVEL=1 for this sales office
    SELECT email FROM ysd_wf_agent INTO TABLE @DATA(lt_agt_ep_to)
      WHERE vkbur = @ls_ep_vkbur-vkbur AND ylevel = 1.
    LOOP AT lt_agt_ep_to INTO DATA(ls_agt_ep_to).
      IF ls_agt_ep_to-email IS NOT INITIAL.
        lv_ep_email = ls_agt_ep_to-email. APPEND lv_ep_email TO lt_to_email.
      ENDIF.
    ENDLOOP.
    SORT lt_to_email. DELETE ADJACENT DUPLICATES FROM lt_to_email.
    IF lt_to_email IS INITIAL. CONTINUE. ENDIF.

    " CC: YSD_WF_AGENT YLEVEL=4 + fixed ngmc address
    SELECT email FROM ysd_wf_agent INTO TABLE @DATA(lt_agt_ep_cc)
      WHERE vkbur = @ls_ep_vkbur-vkbur AND ylevel = 4.
    LOOP AT lt_agt_ep_cc INTO DATA(ls_agt_ep_cc).
      IF ls_agt_ep_cc-email IS NOT INITIAL.
        lv_ep_email = ls_agt_ep_cc-email. APPEND lv_ep_email TO lt_cc_email.
      ENDIF.
    ENDLOOP.
    lv_ep_email = 'ngmc@gail.co.in'. APPEND lv_ep_email TO lt_cc_email.
    SORT lt_cc_email. DELETE ADJACENT DUPLICATES FROM lt_cc_email.
    LOOP AT lt_to_email INTO lv_ep_email.
      DELETE lt_cc_email WHERE table_line = lv_ep_email.
    ENDLOOP.

    " Subject: SO [vkbur]
    lv_subject = |Expired Contracts with finite Imbalance_SO { ls_ep_vkbur-vkbur }|.

    " Body: HTML table (ref YRGR091 style), all columns retained for Posted
    lv_html_body =
      '<html><body style="font-family:Arial,sans-serif;font-size:12px;">' &&
      '<p>Dear Ma''am/ Sir,</p>' &&
      |<p>Please find below instances of Finite Imbalances in Expired Contracts | &&
      |for Sales Office { ls_ep_vkbur-vkbur }. | &&
      |Please take necessary action in this regard.</p>| &&
      '<table border="1" cellspacing="0" cellpadding="4" ' &&
      'style="border-collapse:collapse;font-size:12px;">' &&
      '<tr style="background-color:#c0c0c0;font-weight:bold;">' &&
      '<td>Contract ID</td>' &&
      '<td>Sales Office</td>' &&
      '<td>Business Location</td>' &&
      '<td>Customer</td>' &&
      '<td>CT Start Date</td>' &&
      '<td>CT End Date</td>' &&
      '<td>Posted Imbalance</td>' &&
      '<td>Posted Negative Imbalance</td>' &&
      '</tr>'.

    LOOP AT lt_final INTO ls_final WHERE stat = 'Posted' AND vkbur = ls_ep_vkbur-vkbur.
      " Date format: DD.MM.YYYY
      lv_ct_start = |{ ls_final-vbegdat+6(2) }.{ ls_final-vbegdat+4(2) }.{ ls_final-vbegdat(4) }|.
      lv_ct_end   = |{ ls_final-venddat+6(2) }.{ ls_final-venddat+4(2) }.{ ls_final-venddat(4) }|.
      " Strip leading zeros from customer
      lv_partnr = ls_final-partnr.
      SHIFT lv_partnr LEFT DELETING LEADING '0'.
      WRITE ls_final-po_imbal TO lv_po_str LEFT-JUSTIFIED.
      WRITE ls_final-ne_imbal TO lv_ne_str LEFT-JUSTIFIED.
      lv_html_body = lv_html_body &&
        |<tr>| &&
        |<td>{ ls_final-docnr }</td>| &&
        |<td>{ ls_final-vkbur }</td>| &&
        |<td>{ ls_final-locid }</td>| &&
        |<td>{ lv_partnr }</td>| &&
        |<td>{ lv_ct_start }</td>| &&
        |<td>{ lv_ct_end }</td>| &&
        |<td align="right">{ lv_po_str }</td>| &&
        |<td align="right">{ lv_ne_str }</td>| &&
        |</tr>|.
    ENDLOOP.

    lv_html_body = lv_html_body &&
      '</table>' &&
      '<p>For more details, please execute T-code YRGR105 with the required input.</p>' &&
      '<p>With warm regards,<br/>GAIL (INDIA) LTD.</p>' &&
      '<hr/><p>This is a system generated mail. Please do not reply.</p><hr/>' &&
      |<p>Source: YRGR105.{ sy-uname }.{ sy-datum }.{ sy-uzeit }</p>| &&
      '</body></html>'.

    " Send via CL_BCS for proper HTML rendering and sender display name
    TRY.
      lo_send_req = cl_bcs=>create_persistent( ).

      lt_soli = cl_document_bcs=>string_to_soli( ip_string = lv_html_body ).
      lo_document = cl_document_bcs=>create_document(
        i_type    = 'HTM'
        i_subject = lv_subject
        i_text    = lt_soli ).
      lo_send_req->set_document( lo_document ).

      lo_sender_sap = cl_sapuser_bcs=>create( sy-uname ).
      lo_send_req->set_sender( i_sender = lo_sender_sap ).

      LOOP AT lt_to_email INTO lv_ep_email.
        lo_recipient = cl_cam_address_bcs=>create_internet_address(
          i_address_string = lv_ep_email ).
        lo_send_req->add_recipient(
          i_recipient = lo_recipient
          i_express   = abap_true ).
      ENDLOOP.

      LOOP AT lt_cc_email INTO lv_ep_email.
        lo_recipient = cl_cam_address_bcs=>create_internet_address(
          i_address_string = lv_ep_email ).
        lo_send_req->add_recipient(
          i_recipient = lo_recipient
          i_copy      = abap_true ).
      ENDLOOP.

      lo_send_req->send( i_with_error_screen = abap_false ).
      COMMIT WORK.
    CATCH cx_bcs INTO DATA(lx_bcs_ep).
      MESSAGE lx_bcs_ep->get_text( ) TYPE 'W'.
    ENDTRY.

    CLEAR: lt_ep_cont, lt_ep_pernr.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SEND_EMAIL_NOT_POSTED
*& Emails for Status = Not Posted, grouped by Business Location (LOCID)
*& TO  : ERNAM from OIJ_EL_TICKET_I / OIJNOMI AENAM -> PA0105
*& CC  : PA0034/PA0001 (PERSK E7/E8/E9) + last Changed-by via
*&        YRVA_CON_WF_LOG (YY_LEVEL=0, same sort+dedup as Posted)
*&        + YSD_WF_AGENT YLEVEL=1 + ngmc@gail.co.in
*& From: GAIL PARTNER CARE GMS
*& Body: HTML table; last two imbalance columns OMITTED (Not Posted)
*& Subject: IMB Posting Pending [LOCID without C prefix]_Expired Contracts
*&---------------------------------------------------------------------*
FORM send_email_not_posted.
  TYPES: BEGIN OF ty_np_locid, locid   TYPE oijnomi-locid, END OF ty_np_locid.
  TYPES: BEGIN OF ty_np_ernam, ernam   TYPE pa0105-usrid,  END OF ty_np_ernam.
  TYPES: BEGIN OF ty_np_cont,  cont_id TYPE vbeln,         END OF ty_np_cont.
  TYPES: BEGIN OF ty_np_pernr, pernr   TYPE persno,        END OF ty_np_pernr.
  TYPES: BEGIN OF ty_np_ccrep,
           orig_pernr   TYPE persno,
           yy_pernr_rep TYPE persno,
           persk        TYPE persk,
         END OF ty_np_ccrep.

  DATA: lt_np_locid    TYPE TABLE OF ty_np_locid,  ls_np_locid    TYPE ty_np_locid,
        lt_np_cont     TYPE TABLE OF ty_np_cont,   ls_np_cont     TYPE ty_np_cont,
        lt_np_ernam    TYPE TABLE OF ty_np_ernam,  ls_np_ernam    TYPE ty_np_ernam,
        lt_np_pernr    TYPE TABLE OF ty_np_pernr,  ls_np_pernr    TYPE ty_np_pernr,
        lt_cc_rep      TYPE TABLE OF ty_np_ccrep,  ls_cc_rep      TYPE ty_np_ccrep,
        lt_wf_np_pernr TYPE TABLE OF ty_np_pernr,  ls_wf_np_pernr TYPE ty_np_pernr,
        lt_to_email    TYPE TABLE OF ad_smtpadr,
        lt_cc_email    TYPE TABLE OF ad_smtpadr,
        lv_np_email    TYPE ad_smtpadr,
        lv_subject     TYPE c LENGTH 50,
        lv_html_body   TYPE string,
        lt_soli        TYPE soli_tab,
        lv_ct_start    TYPE char10,
        lv_ct_end      TYPE char10,
        lv_np_vkbur    TYPE vkbur,
        lv_partnr      TYPE string,
        lv_locid_disp  TYPE string,
        lo_send_req    TYPE REF TO cl_bcs,
        lo_document    TYPE REF TO cl_document_bcs,
        lo_sender_sap  TYPE REF TO cl_sapuser_bcs,
        lo_recipient   TYPE REF TO cl_cam_address_bcs.

  " Collect unique business locations for Not Posted records
  LOOP AT lt_final INTO ls_final WHERE stat = 'Not Posted'.
    ls_np_locid-locid = ls_final-locid.
    APPEND ls_np_locid TO lt_np_locid.
  ENDLOOP.
  SORT lt_np_locid BY locid. DELETE ADJACENT DUPLICATES FROM lt_np_locid COMPARING locid.

  LOOP AT lt_np_locid INTO ls_np_locid.
    CLEAR: lt_to_email, lt_cc_email, lt_np_cont, lt_np_ernam, lt_np_pernr,
           lt_cc_rep, lt_wf_np_pernr, lv_subject, lv_html_body, lt_soli,
           lv_np_vkbur, lv_locid_disp, lv_partnr.

    LOOP AT lt_final INTO ls_final WHERE stat = 'Not Posted' AND locid = ls_np_locid-locid.
      ls_np_cont-cont_id = ls_final-docnr.
      APPEND ls_np_cont TO lt_np_cont.
      lv_np_vkbur = ls_final-vkbur.
    ENDLOOP.
    SORT lt_np_cont BY cont_id. DELETE ADJACENT DUPLICATES FROM lt_np_cont COMPARING cont_id.

    " TO: OIJ_EL_TICKET_I -> ERNAM (fallback: OIJNOMI -> AENAM)
    SELECT DISTINCT ernam FROM oij_el_ticket_i
      INTO TABLE @DATA(lt_ticket_en)
      WHERE locid          = @ls_np_locid-locid
        AND budat         IN @s_date
        AND ticket_purpose = '1'
        AND status         = 'C'
        AND substatus      = '6'
        AND tktsubrc       NE '1A'.
    IF lt_ticket_en IS NOT INITIAL.
      LOOP AT lt_ticket_en INTO DATA(ls_ticket_en).
        ls_np_ernam-ernam = ls_ticket_en-ernam.
        APPEND ls_np_ernam TO lt_np_ernam. CLEAR ls_np_ernam.
      ENDLOOP.
    ELSE.
      SELECT DISTINCT aenam FROM oijnomi
        INTO TABLE @DATA(lt_nomi_en)
        WHERE locid  = @ls_np_locid-locid
          AND idate IN @s_date
          AND delind NE 'X'.
      LOOP AT lt_nomi_en INTO DATA(ls_nomi_en).
        ls_np_ernam-ernam = ls_nomi_en-aenam.
        APPEND ls_np_ernam TO lt_np_ernam. CLEAR ls_np_ernam.
      ENDLOOP.
    ENDIF.
    SORT lt_np_ernam BY ernam. DELETE ADJACENT DUPLICATES FROM lt_np_ernam COMPARING ernam.

    " TO email derivation:
    " Step 1 - fill temp table with ernam as pernr (type conversion CHAR->NUMC),
    "           query PA0105 subty 0010 directly using those PERNRs
    " Step 2 - for ernam values not found in step 1, fallback:
    "           PA0105 usrid=ernam subty 0001 -> PERNR -> PA0105 subty 0010 -> email
    IF lt_np_ernam IS NOT INITIAL.
      " Fill temp PERNR table from ernam (implicit CHAR12->NUMC8 conversion)
      DATA: lt_try_pernr    TYPE TABLE OF ty_np_pernr,
            ls_try_pernr    TYPE ty_np_pernr,
            lt_found_pernr  TYPE TABLE OF ty_np_pernr,
            ls_found_pernr  TYPE ty_np_pernr,
            lt_np_ernam_rem TYPE TABLE OF ty_np_ernam,
            ls_np_ernam_rem TYPE ty_np_ernam,
            lv_try_pernr    TYPE persno.

      LOOP AT lt_np_ernam INTO DATA(ls_en_try).
        ls_try_pernr-pernr = ls_en_try-ernam.
        APPEND ls_try_pernr TO lt_try_pernr. CLEAR ls_try_pernr.
      ENDLOOP.

      " Step 1: query PA0105 subty 0010 using ernam as PERNR directly
      IF lt_try_pernr IS NOT INITIAL.
        SELECT pernr, usrid_long FROM pa0105
          INTO TABLE @DATA(lt_s1_result)
          FOR ALL ENTRIES IN @lt_try_pernr
          WHERE pernr = @lt_try_pernr-pernr AND subty = '0010' AND endda = '99991231'.
        LOOP AT lt_s1_result INTO DATA(ls_s1).
          IF ls_s1-usrid_long IS NOT INITIAL.
            lv_np_email = ls_s1-usrid_long.
            APPEND lv_np_email TO lt_to_email.
          ENDIF.
          ls_found_pernr-pernr = ls_s1-pernr.
          APPEND ls_found_pernr TO lt_found_pernr. CLEAR ls_found_pernr.
        ENDLOOP.
        SORT lt_found_pernr. DELETE ADJACENT DUPLICATES FROM lt_found_pernr COMPARING pernr.

        " Identify ernam values NOT resolved in step 1
        LOOP AT lt_np_ernam INTO DATA(ls_en_rem).
          lv_try_pernr = ls_en_rem-ernam.
          READ TABLE lt_found_pernr TRANSPORTING NO FIELDS WITH KEY pernr = lv_try_pernr.
          IF sy-subrc NE 0.
            ls_np_ernam_rem-ernam = ls_en_rem-ernam.
            APPEND ls_np_ernam_rem TO lt_np_ernam_rem. CLEAR ls_np_ernam_rem.
          ENDIF.
        ENDLOOP.
      ENDIF.

      " Step 2: fallback for remaining – usrid=ernam -> PERNR -> email
      IF lt_np_ernam_rem IS NOT INITIAL.
        SELECT pernr FROM pa0105
          INTO TABLE @DATA(lt_uid_pernr)
          FOR ALL ENTRIES IN @lt_np_ernam_rem
          WHERE usrid = @lt_np_ernam_rem-ernam AND subty = '0001'
            AND begda LE @sy-datum AND endda GE @sy-datum.
        IF lt_uid_pernr IS NOT INITIAL.
          SELECT usrid_long FROM pa0105
            INTO TABLE @DATA(lt_pa_np_to)
            FOR ALL ENTRIES IN @lt_uid_pernr
            WHERE pernr = @lt_uid_pernr-pernr AND subty = '0010' AND endda = '99991231'.
          LOOP AT lt_pa_np_to INTO DATA(ls_pa_np_to).
            IF ls_pa_np_to-usrid_long IS NOT INITIAL.
              lv_np_email = ls_pa_np_to-usrid_long.
              APPEND lv_np_email TO lt_to_email.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.

      SORT lt_to_email. DELETE ADJACENT DUPLICATES FROM lt_to_email.

      " CC supervisor chain: combine PERNRs from step 1 (lt_found_pernr)
      "                      and step 2 (lt_uid_pernr)
      LOOP AT lt_found_pernr INTO DATA(ls_fp).
        ls_np_pernr-pernr = ls_fp-pernr.
        APPEND ls_np_pernr TO lt_np_pernr.
      ENDLOOP.
      LOOP AT lt_uid_pernr INTO DATA(ls_uid_p).
        ls_np_pernr-pernr = ls_uid_p-pernr.
        APPEND ls_np_pernr TO lt_np_pernr.
      ENDLOOP.
      SORT lt_np_pernr. DELETE ADJACENT DUPLICATES FROM lt_np_pernr COMPARING pernr.

      " CC: PA0034 -> PA0001 (lowest PERSK E7/E8/E9) -> PA0105
      IF lt_np_pernr IS NOT INITIAL.
        SELECT pernr AS orig_pernr, yy_pernr_rep FROM pa0034
          INTO TABLE @DATA(lt_pa0034_np)
          FOR ALL ENTRIES IN @lt_np_pernr
          WHERE pernr  = @lt_np_pernr-pernr
            AND subty IN ('9001', '9002', '9003', '9113', '9114')
            AND begda LE @sy-datum AND endda GE @sy-datum.
        IF lt_pa0034_np IS NOT INITIAL.
          SELECT pernr, persk FROM pa0001
            INTO TABLE @DATA(lt_pa0001_np)
            FOR ALL ENTRIES IN @lt_pa0034_np
            WHERE pernr  = @lt_pa0034_np-yy_pernr_rep
              AND persk IN ('E7', 'E8', 'E9')
              AND begda LE @sy-datum AND endda GE @sy-datum.
          LOOP AT lt_pa0034_np INTO DATA(ls_pa0034_np).
            READ TABLE lt_pa0001_np INTO DATA(ls_pa0001_np)
              WITH KEY pernr = ls_pa0034_np-yy_pernr_rep.
            IF sy-subrc = 0.
              ls_cc_rep-orig_pernr   = ls_pa0034_np-orig_pernr.
              ls_cc_rep-yy_pernr_rep = ls_pa0034_np-yy_pernr_rep.
              ls_cc_rep-persk        = ls_pa0001_np-persk.
              APPEND ls_cc_rep TO lt_cc_rep. CLEAR ls_cc_rep.
            ENDIF.
          ENDLOOP.
          " Keep lowest grade per original PERNR
          SORT lt_cc_rep BY orig_pernr ASCENDING persk ASCENDING.
          DELETE ADJACENT DUPLICATES FROM lt_cc_rep COMPARING orig_pernr.
          IF lt_cc_rep IS NOT INITIAL.
            SELECT usrid_long FROM pa0105
              INTO TABLE @DATA(lt_cc_rep_email)
              FOR ALL ENTRIES IN @lt_cc_rep
              WHERE pernr = @lt_cc_rep-yy_pernr_rep AND subty = '0010' AND endda = '99991231'.
            LOOP AT lt_cc_rep_email INTO DATA(ls_cc_rep_em).
              IF ls_cc_rep_em-usrid_long IS NOT INITIAL.
                lv_np_email = ls_cc_rep_em-usrid_long. APPEND lv_np_email TO lt_cc_email.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.     " lt_np_ernam

    IF lt_to_email IS INITIAL. CONTINUE. ENDIF.

    " CC: latest CHANGED_BY from YRVA_CON_WF_LOG (YY_LEVEL=0) -> PA0105
    " Same sort+dedup logic as SEND_EMAIL_POSTED to keep last Changed-by entry
    IF lt_np_cont IS NOT INITIAL.
      SELECT vbeln, changed_by, changed_on, changed_time FROM yrva_con_wf_log
        INTO TABLE @DATA(lt_wf_np)
        FOR ALL ENTRIES IN @lt_np_cont
        WHERE vbeln = @lt_np_cont-cont_id AND yy_level = 0.

      SORT lt_wf_np BY vbeln ASCENDING changed_on DESCENDING changed_time DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_wf_np COMPARING vbeln.

      LOOP AT lt_wf_np INTO DATA(ls_wf_np).
        ls_wf_np_pernr-pernr = ls_wf_np-changed_by.
        APPEND ls_wf_np_pernr TO lt_wf_np_pernr.
      ENDLOOP.
      IF lt_wf_np_pernr IS NOT INITIAL.
        SELECT usrid_long FROM pa0105
          INTO TABLE @DATA(lt_wf_np_email)
          FOR ALL ENTRIES IN @lt_wf_np_pernr
          WHERE pernr = @lt_wf_np_pernr-pernr AND subty = '0010' AND endda = '99991231'.
        LOOP AT lt_wf_np_email INTO DATA(ls_wf_np_em).
          IF ls_wf_np_em-usrid_long IS NOT INITIAL.
            lv_np_email = ls_wf_np_em-usrid_long. APPEND lv_np_email TO lt_cc_email.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    " CC: YSD_WF_AGENT YLEVEL=1 (sales office of this location) + fixed address
    SELECT email FROM ysd_wf_agent INTO TABLE @DATA(lt_agt_np)
      WHERE vkbur = @lv_np_vkbur AND ylevel = 1.
    LOOP AT lt_agt_np INTO DATA(ls_agt_np).
      IF ls_agt_np-email IS NOT INITIAL.
        lv_np_email = ls_agt_np-email. APPEND lv_np_email TO lt_cc_email.
      ENDIF.
    ENDLOOP.
    lv_np_email = 'ngmc@gail.co.in'. APPEND lv_np_email TO lt_cc_email.
    SORT lt_cc_email. DELETE ADJACENT DUPLICATES FROM lt_cc_email.
    LOOP AT lt_to_email INTO lv_np_email.
      DELETE lt_cc_email WHERE table_line = lv_np_email.
    ENDLOOP.

    " Subject: strip leading 'C' from locid (C10551 -> 10551)
    lv_locid_disp = ls_np_locid-locid.
    SHIFT lv_locid_disp LEFT DELETING LEADING 'C'.
    lv_subject = |IMB Posting Pending { lv_locid_disp }_Expired Contracts|.

    " Get customer for body intro (strip leading zeros)
    READ TABLE lt_final INTO ls_final WITH KEY locid = ls_np_locid-locid
                                               stat  = 'Not Posted'.
    lv_partnr = ls_final-partnr.
    SHIFT lv_partnr LEFT DELETING LEADING '0'.

    " Body: HTML table – last two columns (Posted Imbalance cols) OMITTED
    lv_html_body =
      '<html><body style="font-family:Arial,sans-serif;font-size:12px;">' &&
      '<p>Dear Ma''am/ Sir,</p>' &&
      |<p>Please find below instances of missed Imbalances posting for Expired Contracts | &&
      |for Customer { lv_partnr }. Please take necessary action in this regard.</p>| &&
      '<table border="1" cellspacing="0" cellpadding="4" ' &&
      'style="border-collapse:collapse;font-size:12px;">' &&
      '<tr style="background-color:#c0c0c0;font-weight:bold;">' &&
      '<td>Contract ID</td>' &&
      '<td>Sales Office</td>' &&
      '<td>Business Location</td>' &&
      '<td>Customer</td>' &&
      '<td>CT Start Date</td>' &&
      '<td>CT End Date</td>' &&
      '</tr>'.

    LOOP AT lt_final INTO ls_final WHERE stat = 'Not Posted' AND locid = ls_np_locid-locid.
      " Date format: DD.MM.YYYY
      lv_ct_start = |{ ls_final-vbegdat+6(2) }.{ ls_final-vbegdat+4(2) }.{ ls_final-vbegdat(4) }|.
      lv_ct_end   = |{ ls_final-venddat+6(2) }.{ ls_final-venddat+4(2) }.{ ls_final-venddat(4) }|.
      " Strip leading zeros from customer
      lv_partnr = ls_final-partnr.
      SHIFT lv_partnr LEFT DELETING LEADING '0'.
      lv_html_body = lv_html_body &&
        |<tr>| &&
        |<td>{ ls_final-docnr }</td>| &&
        |<td>{ ls_final-vkbur }</td>| &&
        |<td>{ ls_final-locid }</td>| &&
        |<td>{ lv_partnr }</td>| &&
        |<td>{ lv_ct_start }</td>| &&
        |<td>{ lv_ct_end }</td>| &&
        |</tr>|.
    ENDLOOP.

    lv_html_body = lv_html_body &&
      '</table>' &&
      '<p>For more details, please execute T-code YRGR105/ YRGR102 with the required input.</p>' &&
      '<p>With warm regards,<br/>GAIL (INDIA) LTD.</p>' &&
      '<hr/><p>This is a system generated mail. Please do not reply.</p><hr/>' &&
      |<p>Source: YRGR105.{ sy-uname }.{ sy-datum }.{ sy-uzeit }</p>| &&
      '</body></html>'.

    " Send via CL_BCS for proper HTML rendering and sender display name
    TRY.
      lo_send_req = cl_bcs=>create_persistent( ).

      lt_soli = cl_document_bcs=>string_to_soli( ip_string = lv_html_body ).
      lo_document = cl_document_bcs=>create_document(
        i_type    = 'HTM'
        i_subject = lv_subject
        i_text    = lt_soli ).
      lo_send_req->set_document( lo_document ).

      lo_sender_sap = cl_sapuser_bcs=>create( sy-uname ).
      lo_send_req->set_sender( i_sender = lo_sender_sap ).

      LOOP AT lt_to_email INTO lv_np_email.
        lo_recipient = cl_cam_address_bcs=>create_internet_address(
          i_address_string = lv_np_email ).
        lo_send_req->add_recipient(
          i_recipient = lo_recipient
          i_express   = abap_true ).
      ENDLOOP.

      LOOP AT lt_cc_email INTO lv_np_email.
        lo_recipient = cl_cam_address_bcs=>create_internet_address(
          i_address_string = lv_np_email ).
        lo_send_req->add_recipient(
          i_recipient = lo_recipient
          i_copy      = abap_true ).
      ENDLOOP.

      lo_send_req->send( i_with_error_screen = abap_false ).
      COMMIT WORK.
    CATCH cx_bcs INTO DATA(lx_bcs_np).
      MESSAGE lx_bcs_np->get_text( ) TYPE 'W'.
    ENDTRY.

    CLEAR: lt_np_cont, lt_np_ernam, lt_np_pernr, lt_cc_rep, lt_wf_np_pernr.
  ENDLOOP.
ENDFORM.
