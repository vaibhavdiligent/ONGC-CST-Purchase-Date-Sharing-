*&---------------------------------------------------------------------*
*& Report  YCIS_EXECUTE
*&---------------------------------------------------------------------*
*& CIS 2026-27 - 6-level approval workflow : LEVEL 3 (CPC - Execution).
*&
*&   L3 (CPC) is central (maintained in YCIS_WF_APPR under sales office
*&   '0001', level 3) and sees the Pending-L3 rows of ALL sales offices.
*&     EXECUTE -> create the rebate order (credit-memo request) via BAPI,
*&                WF_STATUS '40' (Pending L4), store the order number,
*&                forward to L4 (CPC Finance - Financial Vetting), e-mail L4.
*&     REJECT  -> WF_STATUS '10' (back to L1 for reinitiation), e-mail L1.
*&   Status model (6-level): 10 L1 / 20 L2 / 30 L3 / 40 L4 / 50 L5 / 60 L6 /
*&   70 Completed (disbursed).  Any reject at any level returns to L1.
*&
*& GUI status 'STANDARD' (function codes EXEC, REJ, SELALL, DESEL, BACK,
*& EXIT) must exist in this program - create it in SE41 (see doc).
*&
*& GAIL 23.11.2026 (350 lines took 45 min): more than 20 ticked lines are
*&   executed by up to 5 parallel background jobs YCIS_L3_REBATE_ORDERS
*&   (SM37) - the L3 user needs authorisation S_BTCH_JOB / RELE to release
*&   own jobs, otherwise the lines are executed in dialog as before. Each
*&   line is locked while its order is created (no duplicate orders); a
*&   failed order keeps SAP's reason in Remarks; L4 is e-mailed once when
*&   the last job has finished.
*&---------------------------------------------------------------------*
REPORT  ycis_execute.

TYPE-POOLS: slis.

TABLES: ycis_apprvl, ycis_wf_appr.

CONSTANTS: gc_level TYPE ycis_wlevel VALUE '3'.
* PDR go-live (GAIL 17.11.2026): Strategic Monthly Discount (May/June'26)
* switched OFF - the option is NOT displayed on the selection screen of
* T-codes YRVG004 / YRVG004_A / _E / _V / _F / _P and cannot be run.
* To activate it again set the value to 'X' (no other change needed).
CONSTANTS  gc_smd_active TYPE c VALUE ' '.
*   Confirmation statement (verified & confirmed at L1 and L2), shown as the
*   L3 report header for onward processing.
CONSTANTS:
  gc_stmt1 TYPE string VALUE
    'Customer-wise, grade-wise sales quantities, along with',
  gc_stmt2 TYPE string VALUE
    'eligible PSD rates and amounts, have been verified and',
  gc_stmt3 TYPE string VALUE
    'confirmed after considering customer waivers, shortfall',
  gc_stmt4 TYPE string VALUE
    'waivers, sales return quantities, and Group/MLE details.'.

TYPES: BEGIN OF ty_out,
         sel         TYPE flag,
         qais_no     TYPE ycis_apprvl-qais_no,
         scheme_type TYPE ycis_apprvl-scheme_type,
         stype_txt   TYPE char20,
         kunnr       TYPE ycis_apprvl-kunnr,
         cust_name   TYPE ycis_apprvl-cust_name,
         kvgr2       TYPE ycis_apprvl-kvgr2,
         sales_off   TYPE ycis_apprvl-sales_off,
*        CIS UAT 14.10.2026 pt.1/2: seasonal indicator + monthly waivers
*        (customer master YRVA_QAIS_DATA) shown at every level
         seas_ind    TYPE yrva_qais_data-p_r_indicator,
         sc_ind      TYPE yrva_qais_data-p_sc_indicator,
         waiver_1    TYPE yrva_qais_data-waiver_1,
         waiver_2    TYPE yrva_qais_data-waiver_2,
         wv_stat     TYPE ycis_apprvl-wv_stat,     " Shortfall/Customer Waiver status
         mcq_qty     TYPE p LENGTH 13 DECIMALS 3,
         lft_qty     TYPE p LENGTH 13 DECIMALS 3,       " group lifted qty
         ind_lft_qty TYPE p LENGTH 13 DECIMALS 3,   " individual lifted qty
         mcq_perc    TYPE p LENGTH 7 DECIMALS 2,
         elig_qty    TYPE p LENGTH 13 DECIMALS 3,
         rebate_val  TYPE p LENGTH 15 DECIMALS 2,
*        Strategic Monthly Discount (scheme T) - GAIL QAR 13.11.2026
         cis_date    TYPE d,                          " CIS entry date
         raf_qty     TYPE p LENGTH 13 DECIMALS 3,     " Raffia grades qty
         raf_rate    TYPE p LENGTH 9 DECIMALS 2,      " Raffia rate (Rs./MT)
         oth_qty     TYPE p LENGTH 13 DECIMALS 3,     " other grades qty
         oth_rate    TYPE p LENGTH 9 DECIMALS 2,      " other rate (Rs./MT)
         purch_no    TYPE ycis_apprvl-purch_no,
*        GAIL 23.11.2026: period of the line (not displayed) - identifies the
*        line exactly when a customer has more than one period pending
         period_from TYPE ycis_apprvl-period_from,
         period_to   TYPE ycis_apprvl-period_to,
*        L1 & L2 verification status captured for L3 onward processing
         l1_user     TYPE ycis_apprvl-l1_user,
         l1_date     TYPE ycis_apprvl-l1_date,
         l1_time     TYPE ycis_apprvl-l1_time,
         l2_user     TYPE ycis_apprvl-l2_user,
         l2_date     TYPE ycis_apprvl-l2_date,
         l2_time     TYPE ycis_apprvl-l2_time,
         order_no    TYPE ycis_apprvl-order_no,
         remarks     TYPE ycis_apprvl-remarks,
*        CIS UAT 14.10.2026 pt.3: 'Executed' view - processing details
         wf_txt      TYPE char40,
         l3_user     TYPE ycis_apprvl-l3_user,
         l3_date     TYPE ycis_apprvl-l3_date,
         rem_l3      TYPE ycis_apprvl-rem_l3,
         rej_by      TYPE ycis_apprvl-rej_by,
         rej_date    TYPE ycis_apprvl-rej_date,
         rej_remarks TYPE ycis_apprvl-rej_remarks,
       END OF ty_out.

*   GAIL 23.11.2026 ('45 min for generation of rebate orders' - 350 lines):
*   more than GC_BG_MIN ticked lines are executed by up to GC_JOBS parallel
*   background jobs (name GC_JOBNAME, visible in SM37) instead of one by one
*   in dialog. Fewer lines are executed in dialog as before.
CONSTANTS: gc_jobs    TYPE i VALUE 5,
           gc_bg_min  TYPE i VALUE 20,
           gc_jobname TYPE btcjob VALUE 'YCIS_L3_REBATE_ORDERS'.

TYPES: BEGIN OF ty_key,                  " key of one YCIS_APPRVL line
         qais_no     TYPE ycis_apprvl-qais_no,
         scheme_type TYPE ycis_apprvl-scheme_type,
         period_from TYPE ycis_apprvl-period_from,
         period_to   TYPE ycis_apprvl-period_to,
         kunnr       TYPE ycis_apprvl-kunnr,
         kvgr2       TYPE ycis_apprvl-kvgr2,
       END OF ty_key,
       tt_key TYPE STANDARD TABLE OF ty_key WITH DEFAULT KEY.

DATA: gt_appr  TYPE STANDARD TABLE OF ycis_apprvl,
      gs_appr  TYPE ycis_apprvl,
      gt_out   TYPE STANDARD TABLE OF ty_out,
      gs_out   TYPE ty_out,
      gt_fcat  TYPE slis_t_fieldcat_alv,
      gs_fcat  TYPE slis_fieldcat_alv,
      gs_layout TYPE slis_layout_alv,
      gv_isl3  TYPE flag,
      gr_stype TYPE RANGE OF ycis_apprvl-scheme_type,
      gs_stype LIKE LINE OF gr_stype.

*--------------------------------------------------------------------*
DATA: gt_qais TYPE STANDARD TABLE OF yrva_qais_data.   " seasonal ind. / waivers

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE t_b1.
SELECT-OPTIONS: s_sptag FOR ycis_apprvl-period_from,
                s_vkbur FOR ycis_apprvl-sales_off,
                s_kunnr FOR ycis_apprvl-kunnr,
                s_kvgr2 FOR ycis_apprvl-kvgr2.
*   Scheme selection - same Monthly / Yearly choice as the L1 program
*   (YRVG004_QAIS_EXECUTE_N1). Monthly -> 'M', Yearly -> Annual 'A' +
*   Annual Consistency 'C'. GAIL 30.07.2026.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_mon  RADIOBUTTON GROUP g1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(25) c_mon.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_year RADIOBUTTON GROUP g1.
SELECTION-SCREEN COMMENT 3(25) c_year.
SELECTION-SCREEN END OF LINE.
*   GAIL QAR 10.11.2026: separate selection for the Strategic Monthly
*   Discount (scheme type 'T') - no longer mixed with the monthly CIS
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_smd  RADIOBUTTON GROUP g1.
SELECTION-SCREEN COMMENT 3(45) c_smd.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.
*   CIS UAT 14.10.2026 pt.3: Unexecuted = pending at this level (action
*   possible); Executed = already processed at this level (display only).
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE t_b2.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_unex RADIOBUTTON GROUP g2 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 3(45) c_unex.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_exec RADIOBUTTON GROUP g2.
SELECTION-SCREEN COMMENT 3(45) c_exec.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.
* background packet id (set only by the Execute button - FORM submit_background)
PARAMETERS p_bgid TYPE indx-srtfd NO-DISPLAY.

INITIALIZATION.
  c_mon  = 'Monthly'.
  c_year = 'Yearly (Annual)'.
  c_smd  = 'Strategic Monthly Discount (May/June''26)'.
  c_unex = 'Unexecuted (pending at L3)'.
  c_exec = 'Executed (already processed at L3)'.
  t_b1   = 'Selection Criteria'.
  t_b2   = 'Display'.
* CIS UAT (L4 obs.3): field names on the selection screen without
* depending on maintained selection texts
  %_s_sptag_%_app_%-text = 'Period From'.
  %_s_vkbur_%_app_%-text = 'Sales Office'.
  %_s_kunnr_%_app_%-text = 'Customer'.
  %_s_kvgr2_%_app_%-text = 'Customer Group'.

*--------------------------------------------------------------------*
* Strategic Monthly Discount option inactive (see GC_SMD_ACTIVE)
AT SELECTION-SCREEN OUTPUT.
  IF gc_smd_active IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name = 'P_SMD' OR screen-name = 'C_SMD'.   " button + its text
        screen-active    = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN.
  IF p_smd = 'X' AND gc_smd_active IS INITIAL.
    MESSAGE 'Strategic Monthly Discount (May''26 / June''26) is not active' TYPE 'E'.
  ENDIF.

*--------------------------------------------------------------------*
START-OF-SELECTION.
* background job started by the Execute button: create the rebate orders
* of its packet of lines, no list output
  IF p_bgid IS NOT INITIAL.
    PERFORM run_background.
    RETURN.
  ENDIF.
  PERFORM build_stype_range.
  PERFORM check_l3_auth.
  IF gv_isl3 IS INITIAL.
    MESSAGE 'You are not maintained as a Level-3 (CPC) executor (YCIS_WF_APPR)' TYPE 'I'.
    RETURN.
  ENDIF.
  PERFORM get_pending.
  IF gt_appr IS INITIAL.
    IF p_exec = 'X'.
      MESSAGE 'No records already processed at L3 for the selection' TYPE 'I'.
    ELSE.
      MESSAGE 'No records pending L3 execution' TYPE 'I'.
    ENDIF.
    RETURN.
  ENDIF.
  PERFORM build_out.
  PERFORM build_fieldcat.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
FORM check_l3_auth.
  DATA lv_cnt TYPE i.
  SELECT COUNT(*) INTO lv_cnt FROM ycis_wf_appr
    WHERE wf_level = gc_level AND userid = sy-uname.
  IF lv_cnt > 0.
    gv_isl3 = 'X'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
FORM get_pending.
*   CPC is central -> all offices' pending-L3 rows (narrowed by s_vkbur)
  IF p_exec = 'X'.
    SELECT * FROM ycis_apprvl INTO TABLE gt_appr
      WHERE ( l3_user <> space OR rej_level = gc_level )   " processed at L3
        AND scheme_type IN gr_stype               " Monthly / Yearly (excl. rebate 'U')
        AND sales_off   IN s_vkbur
        AND period_from IN s_sptag
        AND kunnr       IN s_kunnr
        AND kvgr2       IN s_kvgr2.
  ELSE.
    SELECT * FROM ycis_apprvl INTO TABLE gt_appr
      WHERE wf_status    = '30'
        AND scheme_type IN gr_stype               " Monthly / Yearly (excl. rebate 'U')
        AND sales_off   IN s_vkbur
        AND period_from IN s_sptag
        AND kunnr       IN s_kunnr
        AND kvgr2       IN s_kvgr2.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  build_stype_range   (Monthly / Yearly scheme selection)
*&---------------------------------------------------------------------*
*   Monthly -> 'M'; Yearly -> Annual 'A' + Annual Consistency 'C'. Mirrors
*   the L1 program's Monthly / Annual choice and keeps the rebate queue
*   ('U') out. GAIL 30.07.2026.
*&---------------------------------------------------------------------*
FORM build_stype_range.
  REFRESH gr_stype.
  gs_stype-sign = 'I'. gs_stype-option = 'EQ'.
  IF p_year = 'X'.
    gs_stype-low = 'A'. APPEND gs_stype TO gr_stype.
    gs_stype-low = 'C'. APPEND gs_stype TO gr_stype.
  ELSEIF p_smd = 'X'.
    gs_stype-low = 'T'. APPEND gs_stype TO gr_stype.   " Strategic Monthly Discount
  ELSE.
    gs_stype-low = 'M'. APPEND gs_stype TO gr_stype.   " monthly CIS only
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
FORM build_out.
  REFRESH gt_out.
  PERFORM read_qais_master.
  LOOP AT gt_appr INTO gs_appr.
    CLEAR gs_out.
    MOVE-CORRESPONDING gs_appr TO gs_out.
    PERFORM scheme_text USING gs_appr-scheme_type CHANGING gs_out-stype_txt.
    PERFORM fill_extra USING gs_appr CHANGING gs_out.
    APPEND gs_out TO gt_out.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  scheme_text   (readable CIS scheme type - GAIL 17.07.2026)
*&---------------------------------------------------------------------*
FORM scheme_text USING p_code TYPE any CHANGING p_txt TYPE char20.
  CASE p_code.
    WHEN 'M'. p_txt = 'Monthly'.
    WHEN 'Q'. p_txt = 'Quarterly'.
    WHEN 'A'. p_txt = 'Annual'.
    WHEN 'C'. p_txt = 'Annual Consistency'.
    WHEN 'T'. p_txt = 'Strategic Monthly'.
    WHEN OTHERS. p_txt = p_code.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
FORM build_fieldcat.
  DATA: lv_pos TYPE i.
  DEFINE add_fc.
    CLEAR gs_fcat.
    ADD 1 TO lv_pos.
    gs_fcat-col_pos   = lv_pos.
    gs_fcat-fieldname = &1.
    gs_fcat-seltext_l = &2.
    gs_fcat-seltext_m = &2.
    gs_fcat-seltext_s = &2.
    gs_fcat-checkbox  = &3.
    gs_fcat-edit      = &3.
    APPEND gs_fcat TO gt_fcat.
  END-OF-DEFINITION.

  add_fc 'SEL'         'Select'          'X'.
  add_fc 'QAIS_NO'     'CIS No.'         ''.
  add_fc 'STYPE_TXT'   'Scheme Type'     ''.
  add_fc 'KUNNR'       'Customer'        ''.
  add_fc 'CUST_NAME'   'Customer Name'   ''.
  add_fc 'KVGR2'       'Cust Group'      ''.
  add_fc 'SALES_OFF'   'Sales Office'         ''.
*  P/R/S Indicator column removed (GAIL UAT 29.10.2026)
* GAIL QAR 13.11.2026: Seasonal / Waiver columns not for the Strategic
* Monthly Discount; it shows the CIS entry date instead (as L1)
  IF p_smd IS INITIAL.
    add_fc 'SC_IND'      'Seasonal Indicator' ''.
    add_fc 'WAIVER_1'    'Waiver Month 1'     ''.
    add_fc 'WAIVER_2'    'Waiver Month 2'     ''.
    add_fc 'WV_STAT'     'Waiver Status'   ''.
  ELSE.
    add_fc 'CIS_DATE'    'CIS Entry Date'  ''.
  ENDIF.
  add_fc 'MCQ_QTY'     'Committed Qty'        ''.
  add_fc 'LFT_QTY'     'Group Lifted Qty'     ''.
  add_fc 'IND_LFT_QTY' 'Individual Lifted Qty' ''.
  add_fc 'MCQ_PERC'    'MCQ %'                ''.
  add_fc 'ELIG_QTY'    'Eligible Qty'         ''.
  IF p_smd = 'X'.
*   Strategic Monthly Discount - Raffia / Other bifurcation (as L1)
    add_fc 'RAF_QTY'     'Raffia Grades Qty'     ''.
    add_fc 'RAF_RATE'    'Raffia Rate (Rs./MT)'  ''.
    add_fc 'OTH_QTY'     'Other Grades Qty'      ''.
    add_fc 'OTH_RATE'    'Other Rate (Rs./MT)'   ''.
    add_fc 'REBATE_VAL'  'Total Amount (Rs.)'    ''.
  ELSE.
    add_fc 'REBATE_VAL'  'Rebate Value'    ''.
  ENDIF.
  add_fc 'PURCH_NO'    'Reference No'    ''.
  add_fc 'L1_USER'     'L1 Verified By'  ''.
  add_fc 'L1_DATE'     'L1 Verified On'  ''.
  add_fc 'L1_TIME'     'L1 Verified At'  ''.
  add_fc 'L2_USER'     'L2 Verified By'  ''.
  add_fc 'L2_DATE'     'L2 Verified On'  ''.
  add_fc 'L2_TIME'     'L2 Verified At'  ''.
  add_fc 'ORDER_NO'    'Rebate Order'    ''.
  add_fc 'REMARKS'     'Remarks'         ''.
  IF p_exec = 'X'.
*   display only: no selection column, processing details instead
    DELETE gt_fcat WHERE fieldname = 'SEL'.
    add_fc 'WF_TXT'      'Workflow Status'  ''.
    add_fc 'L3_USER'     'L3 Executed By'  ''.
    add_fc 'L3_DATE'     'L3 Executed On'  ''.
    add_fc 'REM_L3'      'L3 Remark'     ''.
    add_fc 'REJ_BY'      'Rejected By'      ''.
    add_fc 'REJ_DATE'    'Rejected On'      ''.
    add_fc 'REJ_REMARKS' 'Reject Remark'    ''.
  ENDIF.
  PERFORM num_format.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  num_format   (quantity / % / amount columns)
*&---------------------------------------------------------------------*
*   GAIL UAT 12.11.2026: MCQ Qty, Group / Individual Lifted Qty, MCQ % and
*   Rebate Value were shown in a wrong number format. In YCIS_APPRVL these
*   are quantity / amount fields that need a unit / currency field, which
*   the list does not carry, so the grid formatted them wrongly. The list
*   now holds them as plain numbers and the format is fixed here:
*   quantities in MT with 3 decimals, MCQ % and Rebate Value (Rs.) with 2
*   decimals, right-aligned; Rebate Value is totalled.
*&---------------------------------------------------------------------*
FORM num_format.
  DATA ls_fc TYPE slis_fieldcat_alv.
  LOOP AT gt_fcat INTO ls_fc.
    CASE ls_fc-fieldname.
      WHEN 'MCQ_QTY' OR 'LFT_QTY' OR 'IND_LFT_QTY' OR 'ELIG_QTY'
        OR 'RAF_QTY' OR 'OTH_QTY'.
        ls_fc-inttype      = 'P'.
        ls_fc-datatype     = 'DEC'.
        ls_fc-decimals_out = '3'.
        ls_fc-outputlen    = 15.
        ls_fc-just         = 'R'.
      WHEN 'MCQ_PERC' OR 'RAF_RATE' OR 'OTH_RATE'.
        ls_fc-inttype      = 'P'.
        ls_fc-datatype     = 'DEC'.
        ls_fc-decimals_out = '2'.
        ls_fc-outputlen    = 9.
        ls_fc-just         = 'R'.
      WHEN 'REBATE_VAL'.
        ls_fc-inttype      = 'P'.
        ls_fc-datatype     = 'DEC'.
        ls_fc-decimals_out = '2'.
        ls_fc-outputlen    = 18.
        ls_fc-just         = 'R'.
        ls_fc-do_sum       = 'X'.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.
    MODIFY gt_fcat FROM ls_fc.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
FORM display_alv.
  gs_layout-zebra         = 'X'.
* GAIL UAT 04.11.2026: only the 'Select' tick-box column is used - the extra
* ALV row-selection box (box_fieldname) showed a second selection box.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_top_of_page   = 'TOP_OF_PAGE'
      is_layout                = gs_layout
      it_fieldcat              = gt_fcat
    TABLES
      t_outtab                 = gt_out
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  top_of_page   (confirmation statement header - L1/L2 verified)
*&---------------------------------------------------------------------*
FORM top_of_page.                                           "#EC CALLED
  DATA: lt_hdr TYPE slis_t_listheader,
        ls_hdr TYPE slis_listheader.
  CLEAR ls_hdr. ls_hdr-typ = 'H'.
  ls_hdr-info = 'CIS 2026-27 L-3 Execution (Verified & Confirmed at L1 & L2)'.
  APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'. ls_hdr-info = gc_stmt1. APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'. ls_hdr-info = gc_stmt2. APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'. ls_hdr-info = gc_stmt3. APPEND ls_hdr TO lt_hdr.
  CLEAR ls_hdr. ls_hdr-typ = 'S'. ls_hdr-info = gc_stmt4. APPEND ls_hdr TO lt_hdr.
  IF p_exec = 'X'.
    CLEAR ls_hdr. ls_hdr-typ = 'S'.
    ls_hdr-info = 'Display: records already processed at L3 (display only).'.
    APPEND ls_hdr TO lt_hdr.
  ENDIF.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_hdr.
ENDFORM.

*&---------------------------------------------------------------------*
FORM set_status USING rt_extab TYPE slis_t_extab.            "#EC CALLED
  DATA: lt_ex TYPE slis_t_extab,
        ls_ex TYPE slis_extab.
  lt_ex = rt_extab.
  IF p_exec = 'X'.                     " display only - no actions
    ls_ex-fcode = 'EXEC'. APPEND ls_ex TO lt_ex.
    ls_ex-fcode = 'REJ'. APPEND ls_ex TO lt_ex.
    ls_ex-fcode = 'SELALL'. APPEND ls_ex TO lt_ex.
    ls_ex-fcode = 'DESEL'. APPEND ls_ex TO lt_ex.
  ENDIF.
  SET PF-STATUS 'STANDARD' EXCLUDING lt_ex.
ENDFORM.

*&---------------------------------------------------------------------*
FORM user_command USING r_ucomm     LIKE sy-ucomm            "#EC CALLED
                        rs_selfield TYPE slis_selfield.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.
  IF lr_grid IS NOT INITIAL.
    CALL METHOD lr_grid->check_changed_data.
  ENDIF.

  IF p_exec = 'X' AND ( r_ucomm = 'EXEC' OR r_ucomm = 'REJ' ).
    MESSAGE 'Display only - already processed records' TYPE 'S'.
    RETURN.
  ENDIF.
  CASE r_ucomm.
    WHEN 'EXEC'.
      PERFORM process_selected USING 'E'.
      rs_selfield-refresh = 'X'.
    WHEN 'REJ'.
      PERFORM process_selected USING 'R'.
      rs_selfield-refresh = 'X'.
    WHEN 'SELALL'.
      LOOP AT gt_out INTO gs_out.
        gs_out-sel = 'X'. MODIFY gt_out FROM gs_out.
      ENDLOOP.
      rs_selfield-refresh = 'X'.
    WHEN 'DESEL'.
      LOOP AT gt_out INTO gs_out.
        CLEAR gs_out-sel. MODIFY gt_out FROM gs_out.
      ENDLOOP.
      rs_selfield-refresh = 'X'.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  process_selected   (E = execute/create order, R = reject)
*&---------------------------------------------------------------------*
FORM process_selected USING p_action TYPE char1.
  DATA: lv_cnt    TYPE i,
        lv_err    TYPE i,
        lv_dup    TYPE i,
        lv_zero   TYPE i,
        lv_queued TYPE i,
        lv_text   TYPE string,
        lv_jobs   TYPE i,
        lv_remark TYPE ycis_apprvl-rej_remarks,
        lt_office TYPE STANDARD TABLE OF vkbur,
        lv_off    TYPE vkbur,
        lt_keys   TYPE tt_key,
        lt_rest   TYPE tt_key,
        ls_key    TYPE ty_key.

  READ TABLE gt_out INTO gs_out WITH KEY sel = 'X'.
  IF sy-subrc <> 0.
    MESSAGE 'Please select at least one line' TYPE 'I'.
    RETURN.
  ENDIF.

* CIS 2026-27: a remark is MANDATORY at every level for BOTH execute and reject
  IF p_action = 'E'.
    PERFORM get_remark USING 'Execution remark (mandatory)' CHANGING lv_remark.
  ELSE.
    PERFORM get_remark USING 'Reject remark (mandatory)'    CHANGING lv_remark.
  ENDIF.
  IF lv_remark IS INITIAL.
    MESSAGE 'Remark is mandatory' TYPE 'I'.
    RETURN.
  ENDIF.

  IF p_action = 'E'.
*   GAIL 23.11.2026 (350 lines took 45 min at L3): a larger selection is
*   handed to parallel background jobs - each job creates the rebate orders
*   of its share of the lines, so the user does not wait and the run is
*   several times faster. Lines that could not be scheduled (e.g. no
*   authorisation to release own background jobs) are executed here as before.
    LOOP AT gt_out INTO gs_out WHERE sel = 'X'.
      READ TABLE gt_appr INTO gs_appr
           WITH KEY qais_no     = gs_out-qais_no
                    scheme_type = gs_out-scheme_type
                    kunnr       = gs_out-kunnr
                    kvgr2       = gs_out-kvgr2
                    period_from = gs_out-period_from
                    period_to   = gs_out-period_to.
      CHECK sy-subrc = 0.
      MOVE-CORRESPONDING gs_appr TO ls_key.
      APPEND ls_key TO lt_keys.
    ENDLOOP.
    IF lines( lt_keys ) > gc_bg_min.
      PERFORM submit_background USING lt_keys lv_remark
                                CHANGING lt_rest lv_jobs.
      IF lv_jobs > 0.
*       lines now with a background job: show it on the grid, do not run them here
        LOOP AT gt_out INTO gs_out WHERE sel = 'X'.
          READ TABLE gt_appr INTO gs_appr
               WITH KEY qais_no     = gs_out-qais_no
                        scheme_type = gs_out-scheme_type
                        kunnr       = gs_out-kunnr
                        kvgr2       = gs_out-kvgr2
                        period_from = gs_out-period_from
                        period_to   = gs_out-period_to.
          CHECK sy-subrc = 0.
          READ TABLE lt_rest TRANSPORTING NO FIELDS
               WITH KEY qais_no     = gs_appr-qais_no
                        scheme_type = gs_appr-scheme_type
                        period_from = gs_appr-period_from
                        period_to   = gs_appr-period_to
                        kunnr       = gs_appr-kunnr
                        kvgr2       = gs_appr-kvgr2.
          IF sy-subrc <> 0.
            gs_out-remarks = 'Queued - rebate order being created in background'.
            CLEAR gs_out-sel.
            MODIFY gt_out FROM gs_out.
            lv_queued = lv_queued + 1.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    PERFORM execute_rows USING lv_remark
                         CHANGING lv_cnt lv_dup lv_zero lv_err.

*   executed rows keep their Rebate Order number on the grid; they are now
*   forwarded to L4 (CPC Finance - Financial Vetting). L4 is central in
*   YCIS_WF_APPR (sales office '0001', level 4) - notify once that rebate
*   orders are ready for financial vetting. Background jobs send this mail
*   themselves when the last job of the run has finished.
    IF lv_cnt > 0.                      " only real rebate orders go to L4
      PERFORM send_mail USING '4' '0001' '0001' space.
    ENDIF.
    IF lv_queued > 0.
      lv_text = |{ lv_queued } line(s) handed to { lv_jobs } background job(s) | &&
                |{ gc_jobname } - rebate orders are created in the background and | &&
                |forwarded to L4 (L4 is e-mailed when all jobs are finished). Run | &&
                |YRVG004_E again (Executed) to see the orders. Processed here: | &&
                |{ lv_cnt } created, { lv_zero } zero-value closed, | &&
                |{ lv_dup } already created/in process, { lv_err } failed.|.
      MESSAGE lv_text TYPE 'I'.
    ELSEIF lv_dup > 0 AND lv_cnt = 0 AND lv_err = 0 AND lv_zero = 0.
      MESSAGE 'Rebate order already created for the selected line(s)' TYPE 'I'.
    ELSE.
      MESSAGE |{ lv_cnt } rebate order(s) created & forwarded to L4, { lv_dup } already created, { lv_zero } zero-value line(s) closed at L3, { lv_err } failed - see 'Remarks'| TYPE 'S'.
    ENDIF.
    RETURN.
  ENDIF.

* reject
  LOOP AT gt_out INTO gs_out WHERE sel = 'X'.
    READ TABLE gt_appr INTO gs_appr
         WITH KEY qais_no     = gs_out-qais_no
                  scheme_type = gs_out-scheme_type
                  kunnr       = gs_out-kunnr
                  kvgr2       = gs_out-kvgr2
                  period_from = gs_out-period_from
                  period_to   = gs_out-period_to.
    CHECK sy-subrc = 0.
    gs_appr-wf_status   = '10'.     " back to L1 (6-level: any reject returns to L1)
    gs_appr-status      = 'R'.
    gs_appr-rej_level   = gc_level.
    gs_appr-rej_by      = sy-uname.
    gs_appr-rej_date    = sy-datum.
    gs_appr-rej_time    = sy-uzeit.
    gs_appr-rej_remarks = lv_remark.
    gs_appr-rem_l3      = lv_remark.
    gs_appr-remarks     = 'Rejected by L3'.
    MODIFY ycis_apprvl FROM gs_appr.
    COLLECT gs_appr-sales_off INTO lt_office.
    lv_cnt = lv_cnt + 1.
  ENDLOOP.
  IF lv_cnt > 0.
    COMMIT WORK.
*   6-level rule: any reject returns to L1 for reinitiation - notify L1.
    LOOP AT lt_office INTO lv_off.
      PERFORM send_mail USING '1' lv_off lv_off space.
    ENDLOOP.
    DELETE gt_out WHERE sel = 'X'.
  ENDIF.
  MESSAGE |{ lv_cnt } line(s) rejected and returned to L1| TYPE 'S'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  execute_rows   (create the rebate orders of the ticked lines)
*&---------------------------------------------------------------------*
*   Used by the dialog (Execute button) and by the background jobs.
*   Each line is locked (ENQUEUE_E_TABLE on its YCIS_APPRVL key) before the
*   duplicate check, so a dialog run and a background job - or two jobs -
*   can never create a second rebate order for the same line.
*&---------------------------------------------------------------------*
FORM execute_rows USING    p_remark TYPE ycis_apprvl-rej_remarks
                  CHANGING p_cnt    TYPE i
                           p_dup    TYPE i
                           p_zero   TYPE i
                           p_err    TYPE i.
  DATA: lv_vbeln  TYPE vbeln,
        lv_msg    TYPE bapi_msg,
        lv_varkey TYPE rstable-varkey,
        lv_exord  TYPE ycis_apprvl-order_no,
        lv_exstat TYPE ycis_apprvl-wf_status.

  LOOP AT gt_out INTO gs_out WHERE sel = 'X'.
    READ TABLE gt_appr INTO gs_appr
         WITH KEY qais_no     = gs_out-qais_no
                  scheme_type = gs_out-scheme_type
                  kunnr       = gs_out-kunnr
                  kvgr2       = gs_out-kvgr2
                  period_from = gs_out-period_from
                  period_to   = gs_out-period_to.
    CHECK sy-subrc = 0.

*   line lock - another L3 run / background job is working on this line
    lv_varkey = |{ sy-mandt }/{ gs_appr-qais_no }/{ gs_appr-scheme_type }/{ gs_appr-period_from }/{ gs_appr-period_to }/{ gs_appr-kunnr }/{ gs_appr-kvgr2 }|.
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        mode_rstable   = 'E'
        tabname        = 'YCIS_APPRVL'
        varkey         = lv_varkey
        _scope         = '1'
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      p_dup = p_dup + 1.
      gs_out-remarks = 'In process - another L3 run / background job'.
      CLEAR gs_out-sel.
      MODIFY gt_out FROM gs_out.
      CONTINUE.
    ENDIF.

*   Guard against a duplicate rebate order: re-read the current record
*   from the database (the in-memory copy can be stale after an earlier
*   Execute in the same session). If it already carries an order / is
*   Completed, tell the user and skip - do NOT create a second order.
    CLEAR: lv_exord, lv_exstat.
    SELECT SINGLE order_no wf_status
      INTO (lv_exord, lv_exstat)
      FROM ycis_apprvl
      WHERE qais_no     = gs_appr-qais_no
        AND scheme_type = gs_appr-scheme_type
        AND period_from = gs_appr-period_from
        AND period_to   = gs_appr-period_to
        AND kunnr       = gs_appr-kunnr
        AND kvgr2       = gs_appr-kvgr2.
    IF lv_exord IS NOT INITIAL OR lv_exstat <> '30'.
      p_dup = p_dup + 1.
      gs_out-order_no = lv_exord.
      IF lv_exord IS NOT INITIAL.
        gs_out-remarks  = |Rebate order { lv_exord } already created|.
      ELSE.
        gs_out-remarks  = 'Already processed at L3'.
      ENDIF.
      CLEAR gs_out-sel.
      MODIFY gt_out FROM gs_out.
      PERFORM unlock_row USING lv_varkey.
      CONTINUE.
    ENDIF.

*   Zero-amount group member (GAIL 05.08.2026): a row with no discount
*   value / no order quantity creates NO rebate order, but it has already
*   flowed L1 -> L2 as a legitimate 'Grp O.k' / waiver line (a pure zero
*   row with no waiver is blocked at L1). It must still be COMPLETED with
*   a dummy SD document ('GROUP OK') and Approved status, so the group is
*   treated as processed and can be run again next month - instead of
*   being left stuck at Pending with a blank SD document.
*   CIS UAT 23.10.2026 (L4 obs.): a row WITHOUT a rebate order (zero
*   discount value, e.g. a group member) is NOT forwarded to L4 any more -
*   only rebate orders created against the discount approved at L3 go on
*   to L4. The zero row is closed (Completed) here at L3.
    IF gs_appr-cd_value IS INITIAL OR gs_appr-target_qty IS INITIAL.
      p_zero = p_zero + 1.
      gs_appr-wf_status = '70'.          " Completed at L3 - not forwarded to L4
      gs_appr-status    = 'A'.
      gs_appr-order_no  = 'GROUP OK'.    " dummy SD document - no real order
      gs_appr-l3_user   = sy-uname.
      gs_appr-l3_date   = sy-datum.
      gs_appr-l3_time   = sy-uzeit.
      gs_appr-rem_l3    = p_remark.
      gs_appr-remarks   = 'Group OK - no discount, closed at L3 (not sent to L4)'.
      MODIFY ycis_apprvl FROM gs_appr.
      COMMIT WORK.
      gs_out-order_no = 'GROUP OK'.
      gs_out-remarks  = 'Group OK - no discount, closed at L3'.
      CLEAR gs_out-sel.
      MODIFY gt_out FROM gs_out.
      PERFORM unlock_row USING lv_varkey.
      CONTINUE.
    ENDIF.

    CLEAR: lv_vbeln, lv_msg.
    PERFORM create_order USING gs_appr CHANGING lv_vbeln lv_msg.
    IF lv_vbeln IS NOT INITIAL.
      gs_appr-wf_status = '40'.        " Pending L4 (Financial Vetting) - 6-level flow
      gs_appr-status    = 'P'.         " still in workflow (final only after L6)
      gs_appr-order_no  = lv_vbeln.
      gs_appr-l3_user   = sy-uname.
      gs_appr-l3_date   = sy-datum.
      gs_appr-l3_time   = sy-uzeit.
      gs_appr-rem_l3    = p_remark.    " L3 execution remark (prints on note)
      gs_appr-remarks   = 'Executed - rebate order created, forwarded to L4'.
      MODIFY ycis_apprvl FROM gs_appr.
*     one commit per row: persists this order's VBKD / YRVA_REBATE /
*     YCIS_APPRVL_GRD / master + the YCIS_APPRVL stamp together (direct
*     table updates only - the sales order itself was already written
*     synchronously by BAPI_TRANSACTION_COMMIT in create_order, so no
*     second synchronous wait is needed here).
      COMMIT WORK.
      p_cnt = p_cnt + 1.
*     keep the row on the main grid and show the created rebate order
      gs_out-order_no = lv_vbeln.
      gs_out-remarks  = 'Executed - order created'.
    ELSE.
      p_err = p_err + 1.
*     keep the SAP reason on the line (also visible after a background run)
      gs_appr-remarks = |Order creation failed: { lv_msg }|.
      MODIFY ycis_apprvl FROM gs_appr.
      COMMIT WORK.
      gs_out-remarks = gs_appr-remarks.
    ENDIF.
    CLEAR gs_out-sel.
    MODIFY gt_out FROM gs_out.
    PERFORM unlock_row USING lv_varkey.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
FORM unlock_row USING p_varkey TYPE rstable-varkey.
  CALL FUNCTION 'DEQUEUE_E_TABLE'
    EXPORTING
      mode_rstable = 'E'
      tabname      = 'YCIS_APPRVL'
      varkey       = p_varkey
      _scope       = '1'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  submit_background
*&---------------------------------------------------------------------*
*   GAIL 23.11.2026: split the selected lines into up to GC_JOBS packets
*   and start one background job per packet (this program, hidden
*   parameter P_BGID). The line keys + remark of a packet are handed over
*   in the standard cluster table INDX (area 'YC'). PT_REST returns the
*   lines that could not be scheduled - the caller executes them in dialog.
*&---------------------------------------------------------------------*
FORM submit_background USING    pt_keys  TYPE tt_key
                                p_remark TYPE ycis_apprvl-rej_remarks
                       CHANGING pt_rest  TYPE tt_key
                                p_jobs   TYPE i.
  TYPES: BEGIN OF ty_pk,
           id   TYPE indx-srtfd,
           keys TYPE tt_key,
         END OF ty_pk.
  DATA: lt_pack     TYPE tt_key,
        lt_pk       TYPE STANDARD TABLE OF ty_pk,
        ls_pk       TYPE ty_pk,
        ls_key      TYPE ty_key,
        lv_npack    TYPE i,
        lv_pack     TYPE i,
        lv_idx      TYPE i,
        lv_ts       TYPE timestampl,
        lv_tsc      TYPE c LENGTH 30,
        lv_dig      TYPE c LENGTH 17,
        lv_base     TYPE indx-srtfd,
        lv_id       TYPE indx-srtfd,
        lv_nn       TYPE n LENGTH 2,
        lv_jobcount TYPE tbtcjob-jobcount,
        lv_rel      TYPE btch0000-char1.

  CLEAR p_jobs.
  pt_rest = pt_keys.

* the user must be allowed to release his own background jobs
  AUTHORITY-CHECK OBJECT 'S_BTCH_JOB'
    ID 'JOBACTION' FIELD 'RELE'
    ID 'JOBGROUP'  DUMMY.
  IF sy-subrc <> 0.
    MESSAGE 'No authorisation to release background jobs (S_BTCH_JOB) - lines are executed in dialog' TYPE 'S'.
    RETURN.
  ENDIF.

* unique run id: 'L3' + time stamp (digits only, max. 19 characters)
  GET TIME STAMP FIELD lv_ts.
  lv_tsc = |{ lv_ts DECIMALS = 3 }|.
  REPLACE ALL OCCURRENCES OF '.' IN lv_tsc WITH ''.
  CONDENSE lv_tsc NO-GAPS.
  lv_dig  = lv_tsc(17).
  lv_base = |L3{ lv_dig }|.

  lv_npack = lines( pt_keys ).
  IF lv_npack > gc_jobs.
    lv_npack = gc_jobs.
  ENDIF.

* 1. store all packets first (so that no job can finish and report the run
*    complete before the other packets exist)
  DO lv_npack TIMES.
    lv_pack = sy-index.
*   round robin: line 1 -> job 1, line 2 -> job 2 ...
    CLEAR ls_pk.
    LOOP AT pt_keys INTO ls_key.
      lv_idx = ( sy-tabix - 1 ) MOD lv_npack + 1.
      IF lv_idx = lv_pack.
        APPEND ls_key TO ls_pk-keys.
      ENDIF.
    ENDLOOP.
    CHECK ls_pk-keys IS NOT INITIAL.
    lv_nn    = lv_pack.
    ls_pk-id = |{ lv_base }{ lv_nn }|.
    lt_pack  = ls_pk-keys.
    EXPORT keys = lt_pack remark = p_remark TO DATABASE indx(yc) ID ls_pk-id.
    APPEND ls_pk TO lt_pk.
  ENDDO.
  COMMIT WORK.                         " the jobs must find their packets

* 2. one background job per packet, started immediately
  LOOP AT lt_pk INTO ls_pk.
    lv_id = ls_pk-id.
    CLEAR lv_jobcount.
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = gc_jobname
      IMPORTING
        jobcount         = lv_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      DELETE FROM DATABASE indx(yc) ID lv_id.
      COMMIT WORK.
      CONTINUE.                        " these lines run in dialog
    ENDIF.

    SUBMIT (sy-repid) WITH p_bgid = lv_id
           VIA JOB gc_jobname NUMBER lv_jobcount
           AND RETURN.
    IF sy-subrc = 0.
      CLEAR lv_rel.
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = lv_jobcount
          jobname              = gc_jobname
          strtimmed            = 'X'
        IMPORTING
          job_was_released     = lv_rel
        EXCEPTIONS
          OTHERS               = 1.
    ENDIF.
    IF sy-subrc <> 0 OR lv_rel IS INITIAL.
*     job not started - remove it and run these lines in dialog
      CALL FUNCTION 'BP_JOB_DELETE'
        EXPORTING
          jobcount   = lv_jobcount
          jobname    = gc_jobname
          forcedmode = 'X'
        EXCEPTIONS
          OTHERS     = 1.
      DELETE FROM DATABASE indx(yc) ID lv_id.
      COMMIT WORK.
      CONTINUE.
    ENDIF.

    p_jobs = p_jobs + 1.
    LOOP AT ls_pk-keys INTO ls_key.
      DELETE pt_rest WHERE qais_no     = ls_key-qais_no
                       AND scheme_type = ls_key-scheme_type
                       AND period_from = ls_key-period_from
                       AND period_to   = ls_key-period_to
                       AND kunnr       = ls_key-kunnr
                       AND kvgr2       = ls_key-kvgr2.
    ENDLOOP.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  run_background   (one background packet - P_BGID)
*&---------------------------------------------------------------------*
FORM run_background.
  DATA: lt_keys   TYPE tt_key,
        lv_remark TYPE ycis_apprvl-rej_remarks,
        lv_cnt    TYPE i,
        lv_dup    TYPE i,
        lv_zero   TYPE i,
        lv_err    TYPE i,
        lv_like   TYPE indx-srtfd,
        lv_run    TYPE c LENGTH 19,
        lv_left   TYPE i,
        lt_ids    TYPE STANDARD TABLE OF indx-srtfd,
        lv_id     TYPE indx-srtfd,
        lv_pcnt   TYPE i,
        lv_tot    TYPE i.

  PERFORM check_l3_auth.
  IF gv_isl3 IS INITIAL.
    MESSAGE 'Background job user is not a Level-3 (CPC) executor (YCIS_WF_APPR)' TYPE 'S'.
    RETURN.
  ENDIF.

  IMPORT keys = lt_keys remark = lv_remark FROM DATABASE indx(yc) ID p_bgid.
  IF sy-subrc <> 0 OR lt_keys IS INITIAL.
    MESSAGE |No lines found for background packet { p_bgid }| TYPE 'S'.
    RETURN.
  ENDIF.

  SELECT * FROM ycis_apprvl INTO TABLE gt_appr
    FOR ALL ENTRIES IN lt_keys
    WHERE qais_no     = lt_keys-qais_no
      AND scheme_type = lt_keys-scheme_type
      AND period_from = lt_keys-period_from
      AND period_to   = lt_keys-period_to
      AND kunnr       = lt_keys-kunnr
      AND kvgr2       = lt_keys-kvgr2
      AND wf_status   = '30'.
  REFRESH gt_out.
  LOOP AT gt_appr INTO gs_appr.
    CLEAR gs_out.
    MOVE-CORRESPONDING gs_appr TO gs_out.
    gs_out-sel = 'X'.
    APPEND gs_out TO gt_out.
  ENDLOOP.

  PERFORM execute_rows USING lv_remark
                       CHANGING lv_cnt lv_dup lv_zero lv_err.
  MESSAGE |Packet { p_bgid }: { lv_cnt } rebate order(s) created & forwarded to L4, { lv_zero } zero-value line(s) closed, { lv_dup } already created / in process, { lv_err } failed (reason in Remarks)| TYPE 'S'.

* packet finished: remove it, leave the number of created orders for the
* last job of the run (area 'YD')
  DELETE FROM DATABASE indx(yc) ID p_bgid.
  EXPORT cnt = lv_cnt TO DATABASE indx(yd) ID p_bgid.
  COMMIT WORK.

* last job of this run (no packet of the run left) -> one mail to L4
  lv_run  = p_bgid(19).                " run id without the packet number
  lv_like = |{ lv_run }%|.
  SELECT COUNT(*) INTO lv_left FROM indx
    WHERE relid = 'YC' AND srtfd LIKE lv_like.
  CHECK lv_left = 0.
  SELECT DISTINCT srtfd FROM indx INTO TABLE lt_ids
    WHERE relid = 'YD' AND srtfd LIKE lv_like.
  LOOP AT lt_ids INTO lv_id.
    CLEAR lv_pcnt.
    IMPORT cnt = lv_pcnt FROM DATABASE indx(yd) ID lv_id.
    lv_tot = lv_tot + lv_pcnt.
  ENDLOOP.
  DELETE FROM indx WHERE relid = 'YD' AND srtfd LIKE lv_like.
  IF sy-dbcnt > 0.                     " only the job that removed them mails
    COMMIT WORK.
    IF lv_tot > 0.
      PERFORM send_mail USING '4' '0001' '0001' space.
    ENDIF.
    MESSAGE |Run { lv_run } completed: { lv_tot } rebate order(s) created in total| TYPE 'S'.
  ENDIF.
ENDFORM.

*   The Verification & Confirmation pop-up was removed from L3: per GAIL
*   (30.07.2026) it is shown only at L1 (maker) and L2 (approver), the
*   levels that verify and confirm the figures - L3 only executes.

*&---------------------------------------------------------------------*
*&      Form  create_order   (build & post the rebate order from payload)
*&---------------------------------------------------------------------*
FORM create_order USING p_appr TYPE ycis_apprvl
                  CHANGING p_vbeln TYPE vbeln
                           p_msg   TYPE bapi_msg.
*   Interface types mirror the production create_sale_order in
*   YRVG004_QAIS_EXECUTE : SOLD_TO_PARTY is a structure (BAPISOLDTO) and
*   RETURN is a single IMPORTING structure (BAPIRETURN1) - NOT a table.
*   Passing a CHAR field / a table here caused the CALL_FUNCTION_CONFLICT_LENG
*   (CX_SY_DYN_CALL_ILLEGAL_TYPE) runtime error.
  DATA: x_header    LIKE bapisdhead,
        i_items     LIKE bapiitemin  OCCURS 0 WITH HEADER LINE,
        i_partner   LIKE bapipartnr  OCCURS 0 WITH HEADER LINE,
        x_sold_to   LIKE bapisoldto,
        x_return    LIKE bapireturn1,
        w_objtype   LIKE bapiusw01-objtype,
        w_vbeln     LIKE bapivbeln-vbeln.

*   Do NOT create a zero-amount rebate order (GAIL 30.07.2026): if there is
*   no discount value or no order quantity, there is nothing to credit, so
*   skip creation and return with an empty document. Mirrors the guard in
*   the original YRVU001 (IF cd_value1 <> 0 AND target_qty <> 0 -> create).
  IF p_appr-cd_value IS INITIAL OR p_appr-target_qty IS INITIAL.
    p_msg = 'No discount value / quantity'.
    RETURN.
  ENDIF.

  x_header-doc_type   = p_appr-doc_type.
  x_header-sales_org  = p_appr-sales_org.
  x_header-distr_chan = p_appr-distr_chan.
  x_header-division   = p_appr-division.
  x_header-ord_reason = p_appr-ord_reason.
  x_header-sales_off  = p_appr-sales_off.
  x_header-cd_type1   = p_appr-cd_type.
  x_header-cd_value1  = p_appr-cd_value.
  x_header-purch_no   = p_appr-purch_no.
* GAIL QAR 16.11.2026: credit-note mail showed rebate period 00.00.0000 -
* pass the CIS period start with the order itself (VBKD-BSTDK); the period
* end (VBKD-BSTDK_E) is still set in post_order_update.
  x_header-purch_date = p_appr-period_from.
  IF p_appr-bill_block IS NOT INITIAL.
    x_header-bill_block = p_appr-bill_block.
  ENDIF.

  CLEAR i_partner. REFRESH i_partner.
  i_partner-partn_role = 'AG'.
  i_partner-partn_numb = p_appr-kunnr.
  APPEND i_partner.

  CLEAR i_items. REFRESH i_items.
  i_items-material   = p_appr-material.
  i_items-target_qty = p_appr-target_qty.
  APPEND i_items.

  w_objtype = 'BUS2094'.

  CALL FUNCTION 'BAPI_SALESDOCU_CREATEFROMDATA'
    EXPORTING
      order_header_in = x_header
      business_object = w_objtype
      without_commit  = ' '
    IMPORTING
      salesdocument   = w_vbeln
      sold_to_party   = x_sold_to
      return          = x_return
    TABLES
      order_items_in  = i_items
      order_partners  = i_partner.

  IF w_vbeln IS NOT INITIAL.
    p_vbeln = w_vbeln.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
*   Post-order updates that the original YRVG004 did after order creation:
*   stamp the CIS period on the order (VBKD), log the rebate (YRVA_REBATE)
*   and write the sale order back into the CIS master (YRVA_QAIS_DATA / _M).
    PERFORM post_order_update USING p_appr w_vbeln.
    PERFORM post_master_update USING p_appr w_vbeln.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*   GAIL 23.11.2026: keep SAP's reason (shown in Remarks / job log)
    p_msg = x_return-message.
    IF p_msg IS INITIAL.
      p_msg = |{ x_return-type } { x_return-id } { x_return-number }|.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  post_order_update
*&      Mirrors the YRVG004 post-processing: set the CIS period dates on
*&      the created order (VBKD-BSTDK / BSTDK_E) and write the rebate log
*&      table YRVA_REBATE. In the maker/checker flow this happens here, at
*&      the final (L3 / CPC) approval - only after the order is created.
*&---------------------------------------------------------------------*
FORM post_order_update USING p_appr TYPE ycis_apprvl
                             p_vbeln TYPE vbeln.
  DATA: ls_reb TYPE yrva_rebate.

* CIS period on the order document (reference dates)
  UPDATE vbkd SET bstdk   = p_appr-period_from
                  bstdk_e = p_appr-period_to
            WHERE vbeln = p_vbeln.
*   Performance (GAIL 30.07.2026 - L3 slow for group customers): the
*   per-form COMMIT WORK AND WAIT calls were removed. All direct-table
*   updates for one row (VBKD / YRVA_REBATE / YCIS_APPRVL_GRD / master /
*   YCIS_APPRVL) are now committed ONCE per row by the caller, so a group
*   of N members needs 1 (not 3) extra sync commit per member. The sales
*   order itself is already persisted by BAPI_TRANSACTION_COMMIT above,
*   which is why the VBKD update here still finds the document.

* rebate log
  CLEAR ls_reb.
  ls_reb-vbeln        = p_vbeln.
  ls_reb-kunnr        = p_appr-kunnr.
  ls_reb-vkbur        = p_appr-sales_off.
  ls_reb-lft_qty      = p_appr-lft_qty.
  ls_reb-rev_qty      = p_appr-elig_qty.
  ls_reb-reb_cond     = p_appr-reb_cond.
  ls_reb-ord_cond     = 'ZCMU'.
  ls_reb-value        = p_appr-rebate_val.
  ls_reb-yy_per_start = p_appr-period_from.
  ls_reb-yy_per_end   = p_appr-period_to.
  SELECT SINGLE kukla FROM kna1 INTO ls_reb-kukla
    WHERE kunnr = p_appr-kunnr.
  MODIFY yrva_rebate FROM ls_reb.

* stamp the created order onto the grade-wise detail captured at L1
  UPDATE ycis_apprvl_grd SET order_no = p_vbeln
    WHERE qais_no     = p_appr-qais_no
      AND scheme_type = p_appr-scheme_type
      AND period_from = p_appr-period_from
      AND period_to   = p_appr-period_to
      AND kunnr       = p_appr-kunnr
      AND kvgr2       = p_appr-kvgr2.
* commit consolidated in execute_rows (one COMMIT WORK per row)
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  post_master_update
*&      Write the created sale order back into the CIS master, mirroring
*&      the original YRVG004:
*&        Q -> YRVA_QAIS_DATA-SALE_ORDER_Qn      (n from the period)
*&        A -> YRVA_QAIS_DATA-SO_ANNUAL
*&        C -> YRVA_QAIS_DATA-SO_ANNUAL_CONSIT
*&        M -> YRVA_QAIS_DATA-IND_ELGL_QTY_Mn and YRVA_QAIS_DATA_M
*&             (MON_SO_Mn / MON_VALUE_Mn / IND_ELGL_QTY_Mn / MON_REMARKS_Mn)
*&      Read-modify-write on the full row keeps every other field intact.
*&      Quarter / month index are derived from the stored period, exactly
*&      as the original derived them from the selection-screen period.
*&---------------------------------------------------------------------*
FORM post_master_update USING p_appr TYPE ycis_apprvl
                              p_vbeln TYPE vbeln.
  DATA: ls_q    TYPE yrva_qais_data,
        ls_qm   TYPE yrva_qais_data_m,
        lv_mm   TYPE i,
        lv_idx  TYPE i,
        lv_fld  TYPE string,
        lv_ok   TYPE flag.
  FIELD-SYMBOLS: <any> TYPE any.

* locate the CIS master row (same business key the original used)
  CLEAR: ls_q, lv_ok.
  IF p_appr-qais_no IS NOT INITIAL.
    SELECT SINGLE * FROM yrva_qais_data INTO ls_q
      WHERE qais_no = p_appr-qais_no.
    IF sy-subrc = 0.
      lv_ok = 'X'.
    ENDIF.
  ENDIF.
  IF lv_ok IS INITIAL.
    SELECT SINGLE * FROM yrva_qais_data INTO ls_q
      WHERE kunnr = p_appr-kunnr
        AND kvgr2 = p_appr-kvgr2
        AND vkbur = p_appr-sales_off.
    IF sy-subrc = 0.
      lv_ok = 'X'.
    ENDIF.
  ENDIF.
  CHECK lv_ok = 'X'.

  CASE p_appr-scheme_type.
    WHEN 'Q'.
      CASE p_appr-period_from+4(2).
        WHEN '04'. lv_fld = 'SALE_ORDER_Q1'.
        WHEN '07'. lv_fld = 'SALE_ORDER_Q2'.
        WHEN '10'. lv_fld = 'SALE_ORDER_Q3'.
        WHEN '01'. lv_fld = 'SALE_ORDER_Q4'.
      ENDCASE.
      ASSIGN COMPONENT lv_fld OF STRUCTURE ls_q TO <any>.
      IF sy-subrc = 0.
        <any> = p_vbeln.
      ENDIF.
      MODIFY yrva_qais_data FROM ls_q.

    WHEN 'A'.
      ASSIGN COMPONENT 'SO_ANNUAL' OF STRUCTURE ls_q TO <any>.
      IF sy-subrc = 0.
        <any> = p_vbeln.
      ENDIF.
      MODIFY yrva_qais_data FROM ls_q.

    WHEN 'C'.
      ASSIGN COMPONENT 'SO_ANNUAL_CONSIT' OF STRUCTURE ls_q TO <any>.
      IF sy-subrc = 0.
        <any> = p_vbeln.
      ENDIF.
      MODIFY yrva_qais_data FROM ls_q.

    WHEN 'M'.
*     financial-year month index (Apr=1 ... Jun=3 ... Mar=12)
      lv_mm = p_appr-period_to+4(2).
      IF lv_mm >= 4.
        lv_idx = lv_mm - 3.
      ELSE.
        lv_idx = lv_mm + 9.
      ENDIF.
*     main table : eligible qty for the month
      lv_fld = |IND_ELGL_QTY_M{ lv_idx }|.
      ASSIGN COMPONENT lv_fld OF STRUCTURE ls_q TO <any>.
      IF sy-subrc = 0.
        <any> = p_appr-elig_qty.
      ENDIF.
      MODIFY yrva_qais_data FROM ls_q.
*     detail table _M : order / value / qty / remarks for the month
      CLEAR ls_qm.
      SELECT SINGLE * FROM yrva_qais_data_m INTO ls_qm
        WHERE kunnr = p_appr-kunnr
          AND kvgr2 = p_appr-kvgr2
          AND vkbur = p_appr-sales_off.
      IF sy-subrc <> 0.
        CLEAR ls_qm.
        ls_qm-mandt   = sy-mandt.
        ls_qm-qais_no = p_appr-qais_no.
        ls_qm-kunnr   = p_appr-kunnr.
        ls_qm-kvgr2   = p_appr-kvgr2.
        ls_qm-vkbur   = p_appr-sales_off.
      ENDIF.
      lv_fld = |MON_SO_M{ lv_idx }|.
      ASSIGN COMPONENT lv_fld OF STRUCTURE ls_qm TO <any>.
      IF sy-subrc = 0.
        <any> = p_vbeln.
      ENDIF.
      lv_fld = |MON_VALUE_M{ lv_idx }|.
      ASSIGN COMPONENT lv_fld OF STRUCTURE ls_qm TO <any>.
      IF sy-subrc = 0.
        <any> = p_appr-rebate_val.
      ENDIF.
      lv_fld = |IND_ELGL_QTY_M{ lv_idx }|.
      ASSIGN COMPONENT lv_fld OF STRUCTURE ls_qm TO <any>.
      IF sy-subrc = 0.
        <any> = p_appr-elig_qty.
      ENDIF.
      lv_fld = |MON_REMARKS_M{ lv_idx }|.
      ASSIGN COMPONENT lv_fld OF STRUCTURE ls_qm TO <any>.
      IF sy-subrc = 0.
        <any> = p_appr-purch_no.
      ENDIF.
      MODIFY yrva_qais_data_m FROM ls_qm.
  ENDCASE.
* commit consolidated in execute_rows (one COMMIT WORK per row)
ENDFORM.

*&---------------------------------------------------------------------*
FORM get_remark USING p_title TYPE clike
                CHANGING p_remark TYPE ycis_apprvl-rej_remarks.
  DATA: lt_fields TYPE STANDARD TABLE OF sval,
        ls_field  TYPE sval,
        lv_ret    TYPE char1.
  ls_field-tabname   = 'YCIS_APPRVL'.
  ls_field-fieldname = 'REM_L3'.               " 100-char remark field
  ls_field-fieldtext = 'Remark'.
  ls_field-field_obl = 'X'.
  APPEND ls_field TO lt_fields.
  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = p_title
    IMPORTING
      returncode      = lv_ret
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  IF sy-subrc = 0 AND lv_ret <> 'A'.
    READ TABLE lt_fields INTO ls_field INDEX 1.
    p_remark = ls_field-value.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
FORM send_mail USING p_level  TYPE ycis_wlevel
                     p_office  TYPE vkbur
                     p_ctxoff  TYPE vkbur
                     p_subject TYPE string.
  DATA: lt_wf   TYPE STANDARD TABLE OF ycis_wf_appr,
        ls_wf   TYPE ycis_wf_appr,
        lo_send TYPE REF TO cl_bcs,
        lo_doc  TYPE REF TO cl_document_bcs,
        lo_rec  TYPE REF TO if_recipient_bcs,
        lt_text TYPE bcsy_text,
        ls_text TYPE soli,
        lv_addr TYPE ad_smtpadr,
        lt_addr TYPE STANDARD TABLE OF ad_smtpadr,
        lv_sub  TYPE so_obj_des.

  SELECT * FROM ycis_wf_appr INTO TABLE lt_wf
    WHERE wf_level = p_level AND sales_office = p_office.
* GAIL 23.11.2026 (CPC: no mail to L6 on L5 approval): a central level
* (sales office '0001') is found under whatever sales office its approvers
* are maintained; a blank e-mail ID is taken from the user master (SU01).
  IF p_office = '0001'.
    SELECT * FROM ycis_wf_appr INTO TABLE lt_wf
      WHERE wf_level = p_level.
  ENDIF.
  CHECK lt_wf IS NOT INITIAL.

  TRY.
      lo_send = cl_bcs=>create_persistent( ).
      CLEAR lt_text.
      ls_text-line = |Dear Sir/Madam,|.                            APPEND ls_text TO lt_text.
      ls_text-line = ||.                                            APPEND ls_text TO lt_text.
      IF p_level = '4'.
*       L3 execution done -> forward to L4 (CPC Finance - Financial Vetting)
        ls_text-line = |Rebate orders under the CIS 2026-27 Scheme have been generated by L3 (CPC Marketing).|.
        APPEND ls_text TO lt_text.
        ls_text-line = |Please log in and Review & Vet the discounts (Financial Vetting - L4).|.
        APPEND ls_text TO lt_text.
        lv_sub = 'CIS Discount/Rebate Order Processed - Action Required at L4'.
      ELSE.
*       L3 reject -> back to L1 for reinitiation (6-level rule)
        ls_text-line = |The CIS 2026-27 rebates for Sales Office { p_ctxoff } have been rejected by L3 (CPC).|.
        APPEND ls_text TO lt_text.
        ls_text-line = |Please log in to T-Code YRVG004 (Run CIS Scheme) and reinitiate the process.|.
        APPEND ls_text TO lt_text.
        lv_sub = 'CIS Discount Request Rejected by L3 - Reinitiation Required at L1'.
      ENDIF.
      ls_text-line = ||.                                            APPEND ls_text TO lt_text.
      ls_text-line = |With warm regards,|.                          APPEND ls_text TO lt_text.
      ls_text-line = |GAIL (INDIA) LTD.|.                           APPEND ls_text TO lt_text.
      ls_text-line = ||.                                            APPEND ls_text TO lt_text.
      ls_text-line = |This is a system generated mail. Please do not reply.|.
      APPEND ls_text TO lt_text.
      lo_doc = cl_document_bcs=>create_document(
                 i_type = 'RAW' i_text = lt_text i_subject = lv_sub ).
      lo_send->set_document( lo_doc ).
      PERFORM set_mail_sender USING lo_send.
      LOOP AT lt_wf INTO ls_wf.
        lv_addr = ls_wf-email.
        IF lv_addr IS INITIAL.
          PERFORM user_email USING ls_wf-userid CHANGING lv_addr.
        ENDIF.
        CHECK lv_addr IS NOT INITIAL.
        TRANSLATE lv_addr TO LOWER CASE.
        READ TABLE lt_addr TRANSPORTING NO FIELDS WITH KEY table_line = lv_addr.
        CHECK sy-subrc <> 0.               " each address once
        APPEND lv_addr TO lt_addr.
        lo_rec  = cl_cam_address_bcs=>create_internet_address( lv_addr ).
        lo_send->add_recipient( i_recipient = lo_rec ).
      ENDLOOP.
*     force immediate delivery (do not leave the mail waiting in the
*     SAPconnect / SOST queue) - GAIL 17.07.2026 "mail not going".
      lo_send->set_send_immediately( 'X' ).
      lo_send->send( i_with_error_screen = 'X' ).
      COMMIT WORK.
    CATCH cx_bcs.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  read_qais_master
*&---------------------------------------------------------------------*
*   CIS UAT 14.10.2026 pt.1/2: seasonal indicator (P_R_INDICATOR /
*   P_SC_INDICATOR) and monthly waivers (WAIVER_1 / WAIVER_2) of the listed
*   customers, read once from the CIS customer master YRVA_QAIS_DATA.
*&---------------------------------------------------------------------*
FORM read_qais_master.
  REFRESH gt_qais.
  CHECK gt_appr IS NOT INITIAL.
  SELECT * FROM yrva_qais_data INTO TABLE gt_qais
    FOR ALL ENTRIES IN gt_appr
    WHERE kunnr           = gt_appr-kunnr
      AND module_identity = space.
  SORT gt_qais BY kunnr mou_begda DESCENDING.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  fill_extra   (master data + workflow status of one row)
*&---------------------------------------------------------------------*
FORM fill_extra USING    ps_appr TYPE ycis_apprvl
                CHANGING ps_out  TYPE ty_out.
  DATA: ls_qais  TYPE yrva_qais_data,
        ls_first TYPE yrva_qais_data,
        lv_to    TYPE d.
  lv_to = ps_appr-period_to.
  IF lv_to IS INITIAL.
    lv_to = ps_appr-period_from.
  ENDIF.
* scheme record valid for the CIS period - same customer group preferred
  LOOP AT gt_qais INTO ls_qais WHERE kunnr     = ps_appr-kunnr
                                 AND mou_begda LE lv_to
                                 AND mou_endda GE ps_appr-period_from.
    IF ls_first IS INITIAL.
      ls_first = ls_qais.
    ENDIF.
    IF ls_qais-kvgr2 = ps_appr-kvgr2.
      ls_first = ls_qais.
      EXIT.
    ENDIF.
  ENDLOOP.
* not for the Strategic / Additional May-June discount (GAIL 26.10.2026)
  IF ps_appr-scheme_type <> 'T'.
    ps_out-seas_ind = ls_first-p_r_indicator.
    ps_out-sc_ind   = ls_first-p_sc_indicator.
    ps_out-waiver_1 = ls_first-waiver_1.
    ps_out-waiver_2 = ls_first-waiver_2.
  ENDIF.
* GAIL QAR 13.11.2026: Strategic Monthly Discount - same columns as L1:
* CIS entry date (first CIS 2026-27 entry of the customer) and Raffia /
* Other grades qty and rate (Rs. 4000 / 3000 per MT); the total amount is
* the rebate value. The split follows exactly from the stored eligible qty
* and value: value = 4000 x raffia + 3000 x other, qty = raffia + other.
  IF ps_appr-scheme_type = 'T'.
    LOOP AT gt_qais INTO ls_qais WHERE kunnr     = ps_appr-kunnr
                                   AND mou_endda GE '20260601'.
      IF ps_out-cis_date IS INITIAL OR ls_qais-mou_begda < ps_out-cis_date.
        ps_out-cis_date = ls_qais-mou_begda.
      ENDIF.
    ENDLOOP.
    ps_out-raf_rate = 4000.
    ps_out-oth_rate = 3000.
    ps_out-raf_qty  = ( ps_appr-rebate_val - 3000 * ps_appr-elig_qty ) / 1000.
    IF ps_out-raf_qty < 0.
      ps_out-raf_qty = 0.
    ELSEIF ps_out-raf_qty > ps_appr-elig_qty.
      ps_out-raf_qty = ps_appr-elig_qty.
    ENDIF.
    ps_out-oth_qty  = ps_appr-elig_qty - ps_out-raf_qty.
  ENDIF.
  PERFORM wf_text USING ps_appr CHANGING ps_out-wf_txt.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  wf_text   (current workflow status as text)
*&---------------------------------------------------------------------*
FORM wf_text USING    ps_appr TYPE ycis_apprvl
             CHANGING p_txt   TYPE char40.
  DATA lv_lvl TYPE c LENGTH 1.
  lv_lvl = ps_appr-wf_status(1).
  CASE ps_appr-wf_status.
    WHEN '10'.
      IF ps_appr-status = 'R'.
        p_txt = |Returned to L1 (rejected at L{ ps_appr-rej_level })|.
      ELSE.
        p_txt = 'Pending L1'.
      ENDIF.
    WHEN '20' OR '30' OR '40' OR '50' OR '60'.
      p_txt = |Pending L{ lv_lvl }|.
    WHEN '70'.
      p_txt = 'Completed (Credit Note posted)'.
    WHEN OTHERS.
      p_txt = ps_appr-wf_status.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  user_email   (e-mail ID of a SAP user from SU01)
*&---------------------------------------------------------------------*
*   GAIL 23.11.2026 (CPC: no mail to L6 on L5 approval): used when the
*   e-mail ID of an approver is blank in YCIS_WF_APPR.
*&---------------------------------------------------------------------*
FORM user_email USING    p_user TYPE any
                CHANGING p_addr TYPE ad_smtpadr.
  DATA: lv_adrn TYPE usr21-addrnumber,
        lv_pers TYPE usr21-persnumber.
  CLEAR p_addr.
  CHECK p_user IS NOT INITIAL.
  SELECT SINGLE addrnumber persnumber FROM usr21 INTO (lv_adrn, lv_pers)
    WHERE bname = p_user.
  CHECK sy-subrc = 0.
  SELECT smtp_addr FROM adr6 INTO p_addr UP TO 1 ROWS
    WHERE addrnumber = lv_adrn
      AND persnumber = lv_pers
    ORDER BY flgdefault DESCENDING.
  ENDSELECT.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  set_mail_sender
*&---------------------------------------------------------------------*
*   A user without an e-mail ID in SU01 cannot be the sender of an
*   internet mail (it stops in SOST). Such a user's e-mail ID from
*   YCIS_WF_APPR is used as sender instead.
*&---------------------------------------------------------------------*
FORM set_mail_sender USING po_send TYPE REF TO cl_bcs.
  DATA: lv_addr   TYPE ad_smtpadr,
        lo_sender TYPE REF TO if_sender_bcs.
  PERFORM user_email USING sy-uname CHANGING lv_addr.
  CHECK lv_addr IS INITIAL.            " SU01 e-mail ID exists - default sender
  SELECT email FROM ycis_wf_appr INTO lv_addr UP TO 1 ROWS
    WHERE userid = sy-uname
      AND email <> space.
  ENDSELECT.
  CHECK lv_addr IS NOT INITIAL.
  TRY.
      lo_sender = cl_cam_address_bcs=>create_internet_address( lv_addr ).
      po_send->set_sender( lo_sender ).
    CATCH cx_bcs.
  ENDTRY.
ENDFORM.
