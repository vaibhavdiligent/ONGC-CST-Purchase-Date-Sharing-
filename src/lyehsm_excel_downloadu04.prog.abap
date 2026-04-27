FUNCTION YEHSM_ECEL_DATA_SAVE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(ES_AUDIT_HEADER) TYPE  YSROOT1_D
*"     REFERENCE(EV_TOT_OBS_NO) TYPE  YY_OBS_NO
*"     REFERENCE(EV_COORDINATOR_ROLE) TYPE  FLAG
*"     REFERENCE(EV_IMPLEMENTOR_ROLE) TYPE  FLAG
*"     REFERENCE(EV_CWC_ROLE) TYPE  FLAG
*"     REFERENCE(EV_VIEW_ROLE) TYPE  FLAG
*"     REFERENCE(MO_SVC_MNGR) TYPE REF TO  /BOBF/IF_TRA_SERVICE_MANAGER
*"  TABLES
*"      IT_AUDITORS TYPE  YTTAUDITORS_D
*"      IT_EXCEL_DATA TYPE  YTTEHS_EXCEL_D
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_lines,
           line(132) TYPE c,
         END OF ty_lines,

         BEGIN OF ty_text,
           obs_no TYPE yy_obs_no,
           type(5) TYPE c,
           text   TYPE string,
         END OF ty_text,

         BEGIN OF ty_message,
           Row     TYPE int8,
           Message TYPE string,
         END OF ty_message.

  DATA : lt_status       TYPE STANDARD TABLE OF dd07v,
         ls_status       TYPE dd07v,
         ls_audit_obs    TYPE ysaudit_observations_d,
         lv_rcdate       TYPE datum,
         lv_aufrom       TYPE datum,
         lv_tgt_date     TYPE datum,
         lv_delayed_text TYPE string,
         lt_delay_reason TYPE STANDARD TABLE OF ty_lines,
         ls_delay_reason TYPE ty_lines,
         lv_range_nr     TYPE inri-nrrangenr,
         lv_object       TYPE inri-object,
         lv_audit_number(10) TYPE n,
         lt_text         TYPE TABLE OF ty_text,
         ls_text         TYPE ty_text,
         lt_return       TYPE TABLE OF ty_message,
         ls_return       TYPE ty_message,
         lv_row          TYPE int8 VALUE IS INITIAL,
         ls_audit_header TYPE YSROOT1_D,
         lv_save_mode(1) TYPE c VALUE 'S'.

* GET NEW NUMBER FOR AUDIT
  DATA : lt_mod         TYPE /bobf/t_frw_modification,
         lr_audit       TYPE REF TO ysroot1,
         lr_auditors    TYPE REF TO ysauditors,
         lr_obs         TYPE REF TO ysaudit_observations,
         lr_aud_obs     TYPE REF TO ysaudit_obs_text,
         lo_change      TYPE REF TO /bobf/if_tra_change,
         lo_message     TYPE REF TO /bobf/if_frw_message,
         lt_messages    TYPE /bobf/t_frw_message_k,
         lt_audit_chang TYPE STANDARD TABLE OF yehs_audit_chang,
         ls_audit_chang TYPE yehs_audit_chang,
         mo_txn_mngr    TYPE REF TO /bobf/if_tra_transaction_mgr,
         lv_rejected    TYPE boole_d,
         lx_bopf_ex     TYPE REF TO /bobf/cx_frw,
         lv_err_msg     TYPE string.

  FIELD-SYMBOLS : <ls_message> LIKE LINE OF lt_messages,
                  <fs_auditors> TYPE ysauditors_d,
                  <ls_mod> LIKE LINE OF lt_mod.

  ls_audit_header = es_audit_header.

  ls_audit_obs-status = 'O'.
  ls_audit_obs-obs_no = 00001.

  lv_range_nr = '01'.
  lv_object   = 'YAUDIT'.

* Get Reference of BOBF Transaction Manager
  mo_txn_mngr = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

**********************************************Authorization Code
  IF lt_status[] IS INITIAL.
    CALL FUNCTION 'GET_DOMAIN_VALUES'
      EXPORTING
        domname    = 'YY_ENH_STATUS'
      TABLES
        values_tab = lt_status.
  ENDIF.

  READ TABLE lt_status INTO ls_status WITH KEY domvalue_l = ls_audit_obs-status.
  IF sy-subrc = 0.
    IF ev_cwc_role = 'X'.
      AUTHORITY-CHECK OBJECT 'YAUDIT_CWC'
        ID 'WERKS'     FIELD ls_audit_header-audit_plant
        ID 'YAUD_STAT' FIELD ls_audit_obs-status.
      IF sy-subrc NE 0.
        MESSAGE i030(ye01) WITH ls_status-ddtext ls_audit_header-audit_plant DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

    ELSEIF ev_implementor_role = 'X' AND ev_coordinator_role IS INITIAL.
      AUTHORITY-CHECK OBJECT 'YAUDIT_IM'
        ID 'WERKS'     FIELD ls_audit_header-audit_plant
        ID 'YAUD_STAT' FIELD ls_audit_obs-status.
      IF sy-subrc NE 0.
        MESSAGE i030(ye01) WITH ls_status-ddtext ls_audit_header-audit_plant DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

    ELSEIF ev_implementor_role = 'X' AND ev_coordinator_role = 'X'.
      AUTHORITY-CHECK OBJECT 'YAUDIT_IM'
        ID 'WERKS'     FIELD ls_audit_header-audit_plant
        ID 'YAUD_STAT' FIELD ls_audit_obs-status.
      IF sy-subrc NE 0.
        AUTHORITY-CHECK OBJECT 'YAUDIT_CO'
          ID 'WERKS'     FIELD ls_audit_header-audit_plant
          ID 'YAUD_STAT' FIELD ls_audit_obs-status.
        IF sy-subrc NE 0.
          MESSAGE i030(ye01) WITH ls_status-ddtext ls_audit_header-audit_plant DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ENDIF.

    ELSEIF ev_coordinator_role = 'X'.
      AUTHORITY-CHECK OBJECT 'YAUDIT_CO'
        ID 'WERKS'     FIELD ls_audit_header-audit_plant
        ID 'YAUD_STAT' FIELD ls_audit_obs-status.
      IF sy-subrc NE 0.
        MESSAGE i030(ye01) WITH ls_status-ddtext ls_audit_header-audit_plant DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.
  ELSE.
    EXIT.
  ENDIF.
*************************************End of Authorization Code*************************************

  DATA: lv_date TYPE datum.
  LOOP AT it_excel_data INTO DATA(ls_audit_data).

    CLEAR: lv_rcdate, lv_aufrom, lv_tgt_date, lv_delayed_text, lv_audit_number.

    PERFORM date_conv USING ls_audit_data-rcdate CHANGING lv_date.
    lv_rcdate = lv_date.
    CLEAR lv_date.

    PERFORM date_conv USING ls_audit_data-aufrom CHANGING lv_date.
    lv_aufrom = lv_date.
    CLEAR lv_date.

    CALL FUNCTION 'CONVERT_DATE_INPUT'
      EXPORTING
        input  = ls_audit_data-tgtdate
      IMPORTING
        output = lv_tgt_date
      EXCEPTIONS
        OTHERS = 1.

    IF ls_audit_header-audit_no IS INITIAL.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = lv_range_nr
          object                  = lv_object
        IMPORTING
          number                  = lv_audit_number
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7.
      IF sy-subrc NE 0.
        MESSAGE 'PLEASE MAINTAIN NUMBER RANGE' TYPE 'I'.
        EXIT.
      ELSE.
*— ASSIGN NUMBER AT HEADER LEVEL
        ls_audit_header-audit_no = lv_audit_number.
*— ASSIGN NUMBER AT OBSERVATION LEVEL
        ls_audit_obs-audit_no = lv_audit_number.
      ENDIF.
    ELSE.
      ls_audit_obs-audit_no = ls_audit_header-audit_no.
    ENDIF.

    CLEAR: lt_mod.

    TRY.
      CREATE DATA lr_audit.
      CLEAR: ls_text, lt_text.

      lr_audit->key            = /bobf/cl_frw_factory=>get_new_key( ).
      lr_audit->audit_plant    = ls_audit_data-plant.
      lr_audit->audit_no       = ls_audit_header-audit_no.
      lr_audit->audit_type     = ls_audit_data-autype.
      lr_audit->audit_date     = ls_audit_header-audit_date.
      lr_audit->audit_from     = lv_aufrom.
      lr_audit->audit_to       = ls_audit_header-audit_to.
      lr_audit->audit_rec_date = lv_rcdate.
      lr_audit->audit_scope    = ls_audit_header-audit_scope.
      lr_audit->audit_agency   = ls_audit_header-audit_agency.
      lr_audit->cname          = sy-uname.
      lr_audit->cdate          = sy-datum.
      lr_audit->ctime          = sy-uzeit.
      lr_audit->delay_reason   = lv_delayed_text.

      APPEND INITIAL LINE TO lt_mod ASSIGNING <ls_mod>.
      <ls_mod>-node        = yif_y_ehs_audit1_c=>sc_node-audit_header.
      <ls_mod>-change_mode = /bobf/if_frw_c=>sc_modify_create.
      <ls_mod>-key         = lr_audit->key.
      <ls_mod>-data        = lr_audit.

*— SET AUDITORS
      LOOP AT it_auditors ASSIGNING <fs_auditors>.
        FREE lr_auditors.
        CREATE DATA lr_auditors.
        lr_auditors->key     = /bobf/cl_frw_factory=>get_new_key( ).
        lr_auditors->auditor = <fs_auditors>-auditor.

        APPEND INITIAL LINE TO lt_mod ASSIGNING <ls_mod>.
        <ls_mod>-node        = yif_y_ehs_audit1_c=>sc_node-auditors.
        <ls_mod>-change_mode = /bobf/if_frw_c=>sc_modify_create.
        <ls_mod>-source_node = yif_y_ehs_audit1_c=>sc_node-audit_header.
        <ls_mod>-association = yif_y_ehs_audit1_c=>sc_association-audit_header-auditors.
        <ls_mod>-source_key  = lr_audit->key.
        <ls_mod>-key         = lr_auditors->key.
        <ls_mod>-data        = lr_auditors.
      ENDLOOP.

*— SET OBSERVATIONS
      IF ev_coordinator_role = 'X'.
        ls_text-obs_no = ls_audit_obs-obs_no.
        ls_text-type   = 'OBS'.
        ls_text-text   = ls_audit_data-obstext.
        APPEND ls_text TO lt_text.
      ENDIF.

      FREE lr_obs.
      CREATE DATA lr_obs.
      lr_obs->key           = /bobf/cl_frw_factory=>get_new_key( ).
      lr_obs->audit_no      = ls_audit_obs-audit_no.
      lr_obs->obs_no        = ls_audit_obs-obs_no.
      lr_obs->audit_ref_no  = ls_audit_data-refno.
      lr_obs->saved_status  = lv_save_mode.
      lr_obs->location      = ls_audit_data-location.
      lr_obs->sublocation   = ls_audit_data-subloc.
      lr_obs->priority      = ls_audit_data-priority.
      lr_obs->target_date   = lv_tgt_date.
      lr_obs->status        = ls_audit_obs-status.
      lr_obs->resp_person1  = ls_audit_data-impl01.
      lr_obs->resp_person2  = ls_audit_data-impl02.
      lr_obs->resp_person3  = ls_audit_data-impl03.
* SOC ADDED BY SHUBHAM DATE:15.06.2020 CHARM:4000002126
      lr_obs->resp_person4  = ls_audit_data-impl04.
      lr_obs->resp_person5  = ls_audit_data-impl05.
      lr_obs->resp_person6  = ls_audit_data-impl06.
      lr_obs->resp_person7  = ls_audit_data-impl07.
      lr_obs->resp_person8  = ls_audit_data-impl08.
      lr_obs->resp_person9  = ls_audit_data-impl09.
      lr_obs->resp_person10 = ls_audit_data-impl10.
      lr_obs->resp_person11 = ls_audit_data-impl11.
      lr_obs->resp_person12 = ls_audit_data-impl12.
      lr_obs->resp_person13 = ls_audit_data-impl13.
      lr_obs->resp_person14 = ls_audit_data-impl14.
      lr_obs->resp_person15 = ls_audit_data-impl15.
* EOC ADDED BY SHUBHAM DATE:15.06.2020 CHARM:4000002126
      lr_obs->cname         = sy-uname.
      lr_obs->cdate         = sy-datum.
      lr_obs->ctime         = sy-uzeit.

      APPEND INITIAL LINE TO lt_mod ASSIGNING <ls_mod>.
      <ls_mod>-node        = yif_y_ehs_audit1_c=>sc_node-audit_observations.
      <ls_mod>-change_mode = /bobf/if_frw_c=>sc_modify_create.
      <ls_mod>-source_node = yif_y_ehs_audit1_c=>sc_node-audit_header.
      <ls_mod>-association = yif_y_ehs_audit1_c=>sc_association-audit_header-audit_observations.
      <ls_mod>-source_key  = lr_audit->key.
      <ls_mod>-key         = lr_obs->key.
      <ls_mod>-data        = lr_obs.

*— SET AUDIT'S OBSERVATION TEXT
      FREE lr_aud_obs.
      CREATE DATA lr_aud_obs.
      READ TABLE lt_text INTO ls_text WITH KEY obs_no = ls_audit_obs-obs_no
                                               type   = 'OBS'.
      IF sy-subrc EQ 0.
        lr_aud_obs->key        = /bobf/cl_frw_factory=>get_new_key( ).
        lr_aud_obs->created_by = sy-uname.
        lr_aud_obs->created_on = sy-datum.
        lr_aud_obs->audit_text = ls_text-text.

        APPEND INITIAL LINE TO lt_mod ASSIGNING <ls_mod>.
        <ls_mod>-node        = yif_y_ehs_audit1_c=>sc_node-audit_obs_text.
        <ls_mod>-change_mode = /bobf/if_frw_c=>sc_modify_create.
        <ls_mod>-source_node = yif_y_ehs_audit1_c=>sc_node-audit_observations.
        <ls_mod>-association = yif_y_ehs_audit1_c=>sc_association-audit_observations-audit_obs_text.
        <ls_mod>-source_key  = lr_obs->key.
        <ls_mod>-root_key    = lr_audit->key.
        <ls_mod>-key         = lr_aud_obs->key.
        <ls_mod>-data        = lr_aud_obs.
      ENDIF.

*— CREATE RECORD
      CALL METHOD mo_svc_mngr->modify
        EXPORTING
          it_modification = lt_mod
        IMPORTING
          eo_change       = lo_change
          eo_message      = lo_message.

      lv_row += 1.

*— CHECK FOR ERRORS:
      IF lo_message IS BOUND.
        IF lo_message->check( ) EQ abap_true.
          lo_message->get_messages( IMPORTING et_message = lt_messages ).
          LOOP AT lt_messages ASSIGNING <ls_message>.
            IF sy-tabix = 1.
              ls_return-row     = lv_row.
              ls_return-message = <ls_message>-message->get_text( ).
              APPEND ls_return TO lt_return.
            ENDIF.
          ENDLOOP.
          CLEAR: lt_messages.
          EXIT.
        ENDIF.
      ENDIF.

*— APPLY THE TRANSACTIONAL CHANGES:
      CALL METHOD mo_txn_mngr->save
        IMPORTING
          eo_message  = lo_message
          ev_rejected = lv_rejected.

*— CHECK MESSAGES
      IF lv_rejected EQ abap_true.
        IF lo_message->check( ) EQ abap_true.
          lo_message->get_messages( IMPORTING et_message = lt_messages ).
          LOOP AT lt_messages ASSIGNING <ls_message>.
            IF sy-tabix = 1.
              ls_return-row     = lv_row.
              ls_return-message = <ls_message>-message->get_text( ).
              APPEND ls_return TO lt_return.
            ENDIF.
          ENDLOOP.
          CLEAR: lt_messages.
          EXIT.
        ENDIF.
      ELSE.
        ls_audit_chang-audit_no    = ls_audit_header-audit_no.
        ls_audit_chang-obs_no      = ls_audit_obs-obs_no.
        ls_audit_chang-status      = ls_audit_obs-status.
        ls_audit_chang-change_on   = sy-datum.
        ls_audit_chang-change_time = sy-uzeit.
        ls_audit_chang-change_by   = sy-uname.
        APPEND ls_audit_chang TO lt_audit_chang.
      ENDIF.

    CATCH /bobf/cx_frw INTO lx_bopf_ex.
      lv_err_msg = lx_bopf_ex->get_text( ).
      MESSAGE : lv_err_msg TYPE 'E'.
    ENDTRY.

    CLEAR: ls_audit_obs-audit_no, ls_audit_data, ls_return, ls_audit_chang,
           ls_audit_header-audit_no.

  ENDLOOP.

  MODIFY yehs_audit_chang FROM TABLE lt_audit_chang.

ENDFUNCTION.

FORM date_conv USING p_date TYPE char20 CHANGING lv_date TYPE datum.

  DATA: lv_day  TYPE string,
        lv_mon  TYPE string,
        lv_year TYPE string.

  SPLIT p_date AT '-' INTO lv_day lv_mon lv_year.
  CASE lv_mon.
    WHEN 'Jan'. lv_mon = '01'.
    WHEN 'Feb'. lv_mon = '02'.
    WHEN 'Mar'. lv_mon = '03'.
    WHEN 'Apr'. lv_mon = '04'.
    WHEN 'May'. lv_mon = '05'.
    WHEN 'Jun'. lv_mon = '06'.
    WHEN 'Jul'. lv_mon = '07'.
    WHEN 'Aug'. lv_mon = '08'.
    WHEN 'Sep'. lv_mon = '09'.
    WHEN 'Oct'. lv_mon = '10'.
    WHEN 'Nov'. lv_mon = '11'.
    WHEN 'Dec'. lv_mon = '12'.
  ENDCASE.
  IF lv_day < 10.
    CONCATENATE '0' lv_day INTO lv_day.
  ENDIF.
  CONCATENATE sy-datum+0(2) lv_year lv_mon lv_day INTO lv_date.
  CLEAR: lv_year, lv_mon, lv_day.
ENDFORM.
