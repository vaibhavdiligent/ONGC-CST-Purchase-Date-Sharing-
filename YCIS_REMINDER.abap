*&---------------------------------------------------------------------*
*& Report  YCIS_REMINDER
*&---------------------------------------------------------------------*
*& CIS 2026-27 - Reminder e-mail for pending L1 / L2 processing
*& (GAIL UAT observation 02.11.2026, point 4)
*&
*&   "The system should automatically trigger a reminder email to the Zonal
*&    PC Executive (L1) and Zonal PC Head (L2), with copies to L3, L4 and L5,
*&    if the discount processing at L1 and L2 is not completed within two
*&    working days from the date of release of the discount by PMG."
*&
*&   Release of a discount by PMG is recorded in table YCIS_DISC_REL
*&   (scheme type + CIS period + release date), maintained by CPC (SM30).
*&   This report is scheduled DAILY as a background job (SM36). For every
*&   release older than p_days working days (factory calendar p_fcal) it
*&   checks each zonal sales office that has CIS customers in the period:
*&     - no line confirmed at L1 yet (no YCIS_APPRVL row, or returned to
*&       L1 - WF_STATUS 10)                     -> reminder to L1 of office
*&     - lines confirmed by L1 but still Pending L2 (WF_STATUS 20)
*&                                              -> reminder to L2 of office
*&   Copies go to L3, L4 and L5 (central, sales office '0001' in
*&   YCIS_WF_APPR). One reminder per office and day (YCIS_DISC_REL-
*&   LAST_REMIND), repeated daily until L1/L2 is completed.
*&
*& DDIC prerequisite - table YCIS_DISC_REL (see CIS_2026_27_6LEVEL_IMPORT_
*& GUIDE.md): MANDT, SCHEME_TYPE, PERIOD_FROM, PERIOD_TO (key), REL_DATE,
*& LAST_REMIND, CREATED_BY, CREATED_ON, REMARKS.
*&---------------------------------------------------------------------*
REPORT  ycis_reminder.

TABLES: ycis_disc_rel.

TYPES: BEGIN OF ty_log,
         scheme_type TYPE ycis_disc_rel-scheme_type,
         period_from TYPE ycis_disc_rel-period_from,
         period_to   TYPE ycis_disc_rel-period_to,
         rel_date    TYPE ycis_disc_rel-rel_date,
         due_date    TYPE d,
         sales_off   TYPE vkbur,
         pend_level  TYPE char2,
         mail_sent   TYPE char3,
       END OF ty_log.

DATA: gt_log TYPE STANDARD TABLE OF ty_log,
      gs_log TYPE ty_log.

*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t_b1.
SELECT-OPTIONS: s_vkbur FOR gs_log-sales_off.
PARAMETERS: p_days TYPE i DEFAULT 2,
            p_fcal TYPE wfcid DEFAULT 'IN',
            p_test AS CHECKBOX DEFAULT space.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  t_b1 = 'CIS L1 / L2 reminder'.
  %_s_vkbur_%_app_%-text = 'Sales Office'.
  %_p_days_%_app_%-text  = 'Working days after release'.
  %_p_fcal_%_app_%-text  = 'Factory calendar'.
  %_p_test_%_app_%-text  = 'Test run (no e-mail)'.

*--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM process.
  PERFORM show_log.

*&---------------------------------------------------------------------*
*&      Form  process
*&---------------------------------------------------------------------*
FORM process.
  DATA: lt_rel    TYPE STANDARD TABLE OF ycis_disc_rel,
        ls_rel    TYPE ycis_disc_rel,
        lt_off    TYPE STANDARD TABLE OF vkbur,
        lv_off    TYPE vkbur,
        lv_due    TYPE d,
        lv_level  TYPE ycis_wlevel,
        lv_cnt    TYPE i,
        lv_sent   TYPE flag,
        lv_any    TYPE flag.

  SELECT * FROM ycis_disc_rel INTO TABLE lt_rel
    WHERE rel_date    <= sy-datum
      AND last_remind <> sy-datum.            " max. one reminder per day

* zonal offices with an L1 executive (central CPC office '0001' excluded)
  SELECT DISTINCT sales_office FROM ycis_wf_appr INTO TABLE lt_off
    WHERE wf_level     = '1'
      AND sales_office IN s_vkbur
      AND sales_office <> '0001'.

  LOOP AT lt_rel INTO ls_rel.
    PERFORM add_workdays USING ls_rel-rel_date p_days CHANGING lv_due.
    CHECK sy-datum > lv_due.                  " still within the time limit
    CLEAR lv_any.

    LOOP AT lt_off INTO lv_off.
*     only offices with CIS customers in this period
      SELECT COUNT(*) INTO lv_cnt FROM yrva_qais_data
        WHERE vkbur           = lv_off
          AND mou_begda       <= ls_rel-period_to
          AND mou_endda       >= ls_rel-period_from
          AND module_identity = space.
      CHECK lv_cnt > 0.

      PERFORM pending_level USING ls_rel lv_off CHANGING lv_level.
      CHECK lv_level IS NOT INITIAL.          " L1 and L2 completed

      CLEAR lv_sent.
      IF p_test IS INITIAL.
        PERFORM send_reminder USING ls_rel lv_off lv_level lv_due
                              CHANGING lv_sent.
      ENDIF.
      CLEAR gs_log.
      MOVE-CORRESPONDING ls_rel TO gs_log.
      gs_log-due_date   = lv_due.
      gs_log-sales_off  = lv_off.
      gs_log-pend_level = |L{ lv_level }|.
      IF lv_sent = 'X'.
        gs_log-mail_sent = 'Yes'.
        lv_any = 'X'.
      ELSE.
        gs_log-mail_sent = 'No'.
      ENDIF.
      APPEND gs_log TO gt_log.
    ENDLOOP.

    IF lv_any = 'X'.
      UPDATE ycis_disc_rel SET last_remind = sy-datum
        WHERE scheme_type = ls_rel-scheme_type
          AND period_from = ls_rel-period_from
          AND period_to   = ls_rel-period_to.
    ENDIF.
  ENDLOOP.
  COMMIT WORK.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  pending_level   ('1' = pending L1, '2' = pending L2, ' ' = done)
*&---------------------------------------------------------------------*
FORM pending_level USING    ps_rel   TYPE ycis_disc_rel
                            p_office TYPE vkbur
                   CHANGING p_level  TYPE ycis_wlevel.
  DATA: lt_st TYPE STANDARD TABLE OF ycis_apprvl-wf_status,
        lv_st TYPE ycis_apprvl-wf_status.

  CLEAR p_level.
  SELECT wf_status FROM ycis_apprvl INTO TABLE lt_st
    WHERE scheme_type = ps_rel-scheme_type
      AND period_from = ps_rel-period_from
      AND sales_off   = p_office.
  IF lt_st IS INITIAL.
    p_level = '1'.                     " nothing confirmed at L1 yet
    RETURN.
  ENDIF.
  LOOP AT lt_st INTO lv_st WHERE table_line = '10'.
    p_level = '1'.                     " returned to / pending at L1
    RETURN.
  ENDLOOP.
  LOOP AT lt_st INTO lv_st WHERE table_line = '20'.
    p_level = '2'.                     " confirmed by L1, pending at L2
    RETURN.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  add_workdays   (release date + n working days)
*&---------------------------------------------------------------------*
FORM add_workdays USING    p_date TYPE d
                           p_days TYPE i
                  CHANGING p_due  TYPE d.
  DATA: lv_fdate TYPE facdate,
        lv_wday  TYPE c LENGTH 1,           " 1 = Monday ... 7 = Sunday
        lv_n     TYPE i.

  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      correct_option      = '+'
      date                = p_date
      factory_calendar_id = p_fcal
    IMPORTING
      factorydate         = lv_fdate
    EXCEPTIONS
      OTHERS              = 1.
  IF sy-subrc = 0.
    lv_fdate = lv_fdate + p_days.
    CALL FUNCTION 'FACTORYDATE_CONVERT_TO_DATE'
      EXPORTING
        factorydate         = lv_fdate
        factory_calendar_id = p_fcal
      IMPORTING
        date                = p_due
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.
  ENDIF.

* fallback (calendar not maintained): skip Saturdays and Sundays
  p_due = p_date.
  WHILE lv_n < p_days.
    p_due = p_due + 1.
    CALL FUNCTION 'DATE_COMPUTE_DAY'
      EXPORTING
        date = p_due
      IMPORTING
        day  = lv_wday.
    IF lv_wday < '6'.
      lv_n = lv_n + 1.
    ENDIF.
  ENDWHILE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  send_reminder   (To: L1 or L2 of the office, Cc: L3/L4/L5)
*&---------------------------------------------------------------------*
FORM send_reminder USING    ps_rel   TYPE ycis_disc_rel
                            p_office TYPE vkbur
                            p_level  TYPE ycis_wlevel
                            p_due    TYPE d
                   CHANGING p_sent   TYPE flag.
  DATA: lt_to   TYPE STANDARD TABLE OF ycis_wf_appr,
        lt_cc   TYPE STANDARD TABLE OF ycis_wf_appr,
        ls_wf   TYPE ycis_wf_appr,
        lo_send TYPE REF TO cl_bcs,
        lo_doc  TYPE REF TO cl_document_bcs,
        lo_rec  TYPE REF TO if_recipient_bcs,
        lt_text TYPE bcsy_text,
        ls_text TYPE soli,
        lv_addr TYPE ad_smtpadr,
        lv_sub  TYPE so_obj_des,
        lv_sch  TYPE char40,
        lv_tcd  TYPE char20,
        lv_role TYPE char30.

  CLEAR p_sent.
  SELECT * FROM ycis_wf_appr INTO TABLE lt_to
    WHERE wf_level = p_level AND sales_office = p_office.
  DELETE lt_to WHERE email IS INITIAL.
  CHECK lt_to IS NOT INITIAL.
  SELECT * FROM ycis_wf_appr INTO TABLE lt_cc
    WHERE wf_level IN ('3','4','5') AND sales_office = '0001'.
  DELETE lt_cc WHERE email IS INITIAL.

  CASE ps_rel-scheme_type.
    WHEN 'M'. lv_sch = 'Monthly CIS discount'.
    WHEN 'T'. lv_sch = 'Strategic Monthly Discount'.
    WHEN 'A'. lv_sch = 'Annual CIS discount'.
    WHEN 'C'. lv_sch = 'Annual Consistency discount'.
    WHEN OTHERS. lv_sch = 'CIS discount'.
  ENDCASE.
  IF p_level = '1'.
    lv_role = 'L1 (Zonal PC Executive)'.
    lv_tcd  = 'YRVG004'.
  ELSE.
    lv_role = 'L2 (Zonal PC Head)'.
    lv_tcd  = 'YRVG004_A'.
  ENDIF.

  TRY.
      lo_send = cl_bcs=>create_persistent( ).
      ls_text-line = |Dear Sir/Madam,|.                      APPEND ls_text TO lt_text.
      ls_text-line = ||.                                      APPEND ls_text TO lt_text.
      ls_text-line = |REMINDER: The { lv_sch } for the period { ps_rel-period_from DATE = USER } to { ps_rel-period_to DATE = USER }|.
      APPEND ls_text TO lt_text.
      ls_text-line = |was released by PMG on { ps_rel-rel_date DATE = USER }. Processing for Sales Office { p_office }|.
      APPEND ls_text TO lt_text.
      ls_text-line = |is still pending at { lv_role }, beyond { p_days } working days (due { p_due DATE = USER }).|.
      APPEND ls_text TO lt_text.
      ls_text-line = ||.                                      APPEND ls_text TO lt_text.
      ls_text-line = |Please log in to T-Code { lv_tcd } and complete the processing at the earliest.|.
      APPEND ls_text TO lt_text.
      ls_text-line = ||.                                      APPEND ls_text TO lt_text.
      ls_text-line = |With warm regards,|.                    APPEND ls_text TO lt_text.
      ls_text-line = |GAIL (INDIA) LTD.|.                     APPEND ls_text TO lt_text.
      ls_text-line = ||.                                      APPEND ls_text TO lt_text.
      ls_text-line = |This is a system generated mail. Please do not reply.|.
      APPEND ls_text TO lt_text.
      lv_sub = |Reminder: CIS discount pending at L{ p_level } - Sales Office { p_office }|.

      lo_doc = cl_document_bcs=>create_document(
                 i_type    = 'RAW'
                 i_text    = lt_text
                 i_subject = lv_sub ).
      lo_send->set_document( lo_doc ).
      LOOP AT lt_to INTO ls_wf.
        lv_addr = ls_wf-email.
        lo_rec  = cl_cam_address_bcs=>create_internet_address( lv_addr ).
        lo_send->add_recipient( i_recipient = lo_rec ).
      ENDLOOP.
      LOOP AT lt_cc INTO ls_wf.
        lv_addr = ls_wf-email.
        lo_rec  = cl_cam_address_bcs=>create_internet_address( lv_addr ).
        lo_send->add_recipient( i_recipient = lo_rec i_copy = 'X' ).
      ENDLOOP.
      lo_send->set_send_immediately( 'X' ).
      lo_send->send( i_with_error_screen = space ).
      COMMIT WORK.
      p_sent = 'X'.
    CATCH cx_bcs.
      CLEAR p_sent.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  show_log   (job log / list of reminders)
*&---------------------------------------------------------------------*
FORM show_log.
  IF gt_log IS INITIAL.
    WRITE: / 'No CIS discount pending at L1 / L2 beyond the time limit.'.
    RETURN.
  ENDIF.
  WRITE: / 'Scheme', 8 'Period from', 21 'Period to', 34 'Released',
           47 'Due', 60 'Sales Off', 71 'Pending', 80 'Mail sent'.
  ULINE.
  LOOP AT gt_log INTO gs_log.
    WRITE: / gs_log-scheme_type, 8 gs_log-period_from DD/MM/YYYY,
             21 gs_log-period_to DD/MM/YYYY, 34 gs_log-rel_date DD/MM/YYYY,
             47 gs_log-due_date DD/MM/YYYY, 60 gs_log-sales_off,
             71 gs_log-pend_level, 80 gs_log-mail_sent.
  ENDLOOP.
ENDFORM.
