*&---------------------------------------------------------------------*
*& Report  ZJVA_EQUITY_ADJ_UPLOAD
*&---------------------------------------------------------------------*
*& Program Title : Excel upload for JVA Prior Period Equity Adjustment
*&                 (mass execution of transaction GJ19A)
*&
*& Description   : Drives the STANDARD equity adjustment repeatedly, once
*&                 per row of an Excel file, so that many venture /
*&                 equity-group changes can be processed in one go.
*&
*&                 Each row is ONE complete execution of the standard
*&                 transaction - its own venture, old and new equity
*&                 group, document date range, posting year / period /
*&                 date and object filters. Amounts, recovery indicators,
*&                 document types and account determination are NEVER
*&                 taken from the file; they are derived by standard
*&                 logic exactly as in GJ19A. That is what keeps the run
*&                 reconcilable with cutback and stops a re-run from
*&                 double posting.
*&
*&                 No standard logic is copied. For every row the program
*&                 instantiates the standard class CL_JVA_EQUITY_ADJUST
*&                 and calls the same interface methods that the standard
*&                 report RGJVEA10_ACD (transaction GJ19A) calls:
*&
*&                     set_run_parameters( )
*&                     check_screen_parameters( )
*&                     run_process( )
*&
*&                 Reversal, re-booking, cutback correction and posting
*&                 therefore stay 100% standard, and future SAP
*&                 corrections to that class flow through automatically.
*&
*& Reference     : RGJVEA10_ACD / RGJVEAS0_ACD (transaction GJ19A)
*&                 IF_JVA_EQUITY_ADJUST / CL_JVA_EQUITY_ADJUST
*&
*& IMPORTANT - why every row is pre-validated
*&                 CL_JVA_EQUITY_ADJUST reports invalid selection data
*&                 with hard MESSAGE ... TYPE 'E' statements (e234/e232/
*&                 e243/e235/e117/e237/e566/e674/e682 ...). An E message
*&                 raised in START-OF-SELECTION terminates the whole
*&                 event block - meaning one bad row would abandon every
*&                 row after it. PERFORM validate_row therefore repeats
*&                 the class's own checks BEFORE the class is called, so
*&                 a bad row is logged as Skipped and the run continues.
*&                 Keep validate_row in step with the class.
*&
*& Results       : The interface exposes no results getter, and the
*&                 class keeps its results (MS_EA_RESULTS) private. The
*&                 class does however write its messages to the
*&                 application log (object JVA). Each row is therefore
*&                 bracketed by a timestamp and its log entries are read
*&                 back afterwards - see FORM read_row_result.
*&
*& NOTE          : Not yet syntax-checked - written without access to an
*&                 SAP system. Check in SE38 before transporting.
*&---------------------------------------------------------------------*
REPORT zjva_equity_adj_upload.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
" Flat, all-character structure matching the upload template columns.
" TEXT_CONVERT_XLS_TO_SAP fills these BY POSITION, so the field order
" here MUST match the column order of the Excel template exactly.
TYPES: BEGIN OF ty_raw,
         rowno   TYPE c LENGTH 6,    " 01 Row No.
         bukrs   TYPE c LENGTH 4,    " 02 Company Code
         vname   TYPE c LENGTH 6,    " 03 Venture
         eqold   TYPE c LENGTH 3,    " 04 Old Equity Group
         eqnew   TYPE c LENGTH 3,    " 05 New Equity Group
         frdate  TYPE c LENGTH 10,   " 06 Document Date From  DD.MM.YYYY
         todate  TYPE c LENGTH 10,   " 07 Document Date To    DD.MM.YYYY
         gjahr   TYPE c LENGTH 4,    " 08 Posting Year
         monat   TYPE c LENGTH 2,    " 09 Posting Period
         budat   TYPE c LENGTH 10,   " 10 Posting Date        DD.MM.YYYY
         bktxt   TYPE c LENGTH 25,   " 11 Document Header Text
         excloi  TYPE c LENGTH 1,    " 12 Exclude OI-Managed Accounts X/' '
         selbudf TYPE c LENGTH 10,   " 13 Sel. Posting Date From
         selbudt TYPE c LENGTH 10,   " 14 Sel. Posting Date To
         acctf   TYPE c LENGTH 10,   " 15 Account From
         acctt   TYPE c LENGTH 10,   " 16 Account To
         cntr    TYPE c LENGTH 10,   " 17 Cost Center
         ordnr   TYPE c LENGTH 12,   " 18 Order
         projk   TYPE c LENGTH 24,   " 19 WBS Element (external key)
         nplnr   TYPE c LENGTH 12,   " 20 Network
         vornr   TYPE c LENGTH 4,    " 21 Network Activity
       END OF ty_raw.

TYPES: BEGIN OF ty_log,
         rowno   TYPE c LENGTH 6,
         bukrs   TYPE bukrs,
         vname   TYPE jv_name,
         eqold   TYPE jv_egroup,
         eqnew   TYPE jv_egroup,
         gjahr   TYPE gjahr,
         monat   TYPE monat,
         runtype TYPE c LENGTH 10,          " Analysis / Test / Update
         status  TYPE c LENGTH 10,          " Success / Warning / Error / Skipped
         errors  TYPE i,
         warns   TYPE i,
         message TYPE c LENGTH 200,
       END OF ty_log.

*----------------------------------------------------------------------*
* Global data
*----------------------------------------------------------------------*
" go_adjust must be GLOBAL: the standard class calls back into this
" report's ALV handler FORMs (that is what iv_repid_callback is for),
" and those FORMs need the object of the row being processed.
DATA: go_adjust TYPE REF TO if_jva_equity_adjust.

DATA: gt_raw TYPE STANDARD TABLE OF ty_raw,
      gt_log TYPE STANDARD TABLE OF ty_log.

" Typed exactly as IF_JVA_EQUITY_ADJUST~SET_RUN_PARAMETERS expects them.
DATA: gt_r_acct  TYPE jv_account_range_table,
      gt_r_budat TYPE fins_t_budat_range,
      gt_r_cntr  TYPE jv_cost_center_range_table,
      gt_r_nplnr TYPE jv_network_range_table,
      gt_r_ordnr TYPE jv_order_range_table,
      gt_r_projk TYPE jv_project_range_table,
      gt_r_vornr TYPE vornr_rang_t.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  PARAMETERS p_xls TYPE string OBLIGATORY.        " upload file (.xls/.xlsx)
SELECTION-SCREEN END OF BLOCK b1.

" Run-level options - they apply to every row, exactly as they would to
" a single GJ19A run. Names and defaults follow RGJVEAS0_ACD.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
  PARAMETERS: p_ivpeqg RADIOBUTTON GROUP pst1 DEFAULT 'X', " per Equity Group
              p_ivpptn RADIOBUTTON GROUP pst1,             " per Partner
              p_ivpexp RADIOBUTTON GROUP pst1.             " per Partner/Expense
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
  PARAMETERS: p_displ  RADIOBUTTON GROUP runt,             " Analysis only
              p_test   RADIOBUTTON GROUP runt DEFAULT 'X', " Test run
              p_update RADIOBUTTON GROUP runt.             " Update run
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* F4 help
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_xls.
  PERFORM f4_file CHANGING p_xls.

*----------------------------------------------------------------------*
* Main
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM read_upload_file.

  IF gt_raw IS INITIAL.
    MESSAGE 'No data rows found in the upload file' TYPE 'E'.
  ENDIF.

  PERFORM process_all_rows.

  PERFORM show_log.

*&---------------------------------------------------------------------*
*& Form f4_file
*&---------------------------------------------------------------------*
FORM f4_file CHANGING cv_file TYPE string.

  DATA: lt_files TYPE filetable,
        lv_rc    TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title = 'Select equity adjustment upload file'
      file_filter  = 'Excel files (*.xlsx;*.xls)|*.xlsx;*.xls|All files (*.*)|*.*'
    CHANGING
      file_table   = lt_files
      rc           = lv_rc
    EXCEPTIONS
      OTHERS       = 1 ).

  IF sy-subrc = 0 AND lv_rc > 0.
    READ TABLE lt_files INTO DATA(ls_file) INDEX 1.
    IF sy-subrc = 0.
      cv_file = ls_file-filename.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form read_upload_file  (standard FM TEXT_CONVERT_XLS_TO_SAP)
*&---------------------------------------------------------------------*
FORM read_upload_file.

  DATA: lt_tab_raw TYPE truxs_t_text_data,
        lv_file    TYPE rlgrap-filename.

  lv_file = p_xls.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_field_seperator    = 'X'        " tab-separated cells
      i_line_header        = 'X'        " skip the header row
      i_tab_raw_data       = lt_tab_raw
      i_filename           = lv_file
    TABLES
      i_tab_converted_data = gt_raw
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Could not read the upload file' TYPE 'E'.
  ENDIF.

  DELETE gt_raw WHERE bukrs IS INITIAL
                  AND vname IS INITIAL
                  AND eqold IS INITIAL
                  AND eqnew IS INITIAL.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form process_all_rows
*&---------------------------------------------------------------------*
FORM process_all_rows.

  DATA: lv_total TYPE i,
        lv_done  TYPE i.

  lv_total = lines( gt_raw ).

  LOOP AT gt_raw INTO DATA(ls_raw).
    lv_done = lv_done + 1.
    PERFORM progress USING lv_done lv_total ls_raw-vname.
    PERFORM process_one_row USING ls_raw.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form process_one_row
*&---------------------------------------------------------------------*
*& Runs the STANDARD equity adjustment once, for one uploaded row.
*&---------------------------------------------------------------------*
FORM process_one_row USING ps_raw TYPE ty_raw.

  DATA: ls_log     TYPE ty_log,
        lv_bukrs   TYPE bukrs,
        lv_vname   TYPE jv_name,
        lv_eqold   TYPE jv_egroup,
        lv_eqnew   TYPE jv_egroup,
        lv_year    TYPE gjahr,
        lv_period  TYPE monat,
        lv_frdate  TYPE syst_datum,
        lv_todate  TYPE syst_datum,
        lv_budat   TYPE syst_datum,
        lv_bktxt   TYPE bktxt,
        lv_excloi  TYPE char1,
        lv_ok      TYPE abap_bool,
        lv_time_fr TYPE sy-uzeit,
        lv_date_fr TYPE sy-datum.

  " ---- log line first, so every exit path reports something ---------
  ls_log-rowno = ps_raw-rowno.
  ls_log-bukrs = ps_raw-bukrs.
  ls_log-vname = ps_raw-vname.
  ls_log-eqold = ps_raw-eqold.
  ls_log-eqnew = ps_raw-eqnew.
  ls_log-gjahr = ps_raw-gjahr.
  ls_log-monat = ps_raw-monat.

  IF p_displ = 'X'.
    ls_log-runtype = 'Analysis'.
  ELSEIF p_test = 'X'.
    ls_log-runtype = 'Test'.
  ELSE.
    ls_log-runtype = 'Update'.
  ENDIF.

  " ---- convert the character cells ----------------------------------
  PERFORM convert_row USING    ps_raw
                      CHANGING lv_bukrs lv_vname lv_eqold lv_eqnew
                               lv_year  lv_period
                               lv_frdate lv_todate lv_budat
                               lv_bktxt lv_excloi
                               ls_log   lv_ok.
  IF lv_ok = abap_false.
    ls_log-status = 'Skipped'.
    APPEND ls_log TO gt_log.
    RETURN.
  ENDIF.

  " ---- repeat the class's own checks, so it never raises a hard E ---
  PERFORM validate_row USING    lv_bukrs lv_vname lv_eqold lv_eqnew
                                lv_year  lv_period lv_budat
                       CHANGING ls_log   lv_ok.
  IF lv_ok = abap_false.
    ls_log-status = 'Skipped'.
    APPEND ls_log TO gt_log.
    RETURN.
  ENDIF.

  " ---- object filters for this row ----------------------------------
  PERFORM build_ranges USING ps_raw.

  " ---- run the STANDARD equity adjustment ---------------------------
  " Mirrors FORM check_parameters / FORM run of RGJVEA10_ACD.
  CLEAR go_adjust.
  lv_date_fr = sy-datum.
  lv_time_fr = sy-uzeit.

  TRY.
      go_adjust = NEW cl_jva_equity_adjust( ).

      go_adjust->set_run_parameters(
        iv_bukrs          = lv_bukrs
        iv_vname          = lv_vname
        iv_year           = lv_year
        iv_period         = lv_period
        iv_frdate         = lv_frdate
        iv_todate         = lv_todate
        iv_eqnew          = lv_eqnew
        iv_eqold          = lv_eqold
        iv_excloi         = lv_excloi
        iv_budat          = lv_budat
        it_acct           = gt_r_acct
        it_budat          = gt_r_budat
        it_cntr           = gt_r_cntr
        it_ordnr          = gt_r_ordnr
        it_projk          = gt_r_projk
        it_nplnr          = gt_r_nplnr
        it_vornr          = gt_r_vornr
        iv_bktxt          = lv_bktxt
        iv_ivpeqg         = p_ivpeqg
        iv_ivpptn         = p_ivpptn
        iv_ivpexp         = p_ivpexp
        iv_analysis_only  = p_displ
        iv_test           = p_test
        iv_repid_callback = sy-repid
        iv_farm_in_out    = abap_false ).

      " Standard validation. It also derives the date range defaults,
      " exactly as it does on the GJ19A screen.
      go_adjust->check_screen_parameters( CHANGING cv_frdate = lv_frdate
                                                   cv_todate = lv_todate ).

      " Reversal, re-booking and cutback correction - all standard.
      go_adjust->run_process( ).

    CATCH cx_root INTO DATA(lx_root).
      ls_log-status  = 'Error'.
      ls_log-message = lx_root->get_text( ).
      APPEND ls_log TO gt_log.
      RETURN.
  ENDTRY.

  " ---- read what the run wrote to the application log ---------------
  PERFORM read_row_result USING    lv_date_fr lv_time_fr
                          CHANGING ls_log.

  APPEND ls_log TO gt_log.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form convert_row
*&---------------------------------------------------------------------*
FORM convert_row USING    ps_raw    TYPE ty_raw
                 CHANGING cv_bukrs  TYPE bukrs
                          cv_vname  TYPE jv_name
                          cv_eqold  TYPE jv_egroup
                          cv_eqnew  TYPE jv_egroup
                          cv_year   TYPE gjahr
                          cv_period TYPE monat
                          cv_frdate TYPE syst_datum
                          cv_todate TYPE syst_datum
                          cv_budat  TYPE syst_datum
                          cv_bktxt  TYPE bktxt
                          cv_excloi TYPE char1
                          cs_log    TYPE ty_log
                          cv_ok     TYPE abap_bool.

  cv_ok = abap_true.

  cv_bukrs  = ps_raw-bukrs.
  cv_vname  = ps_raw-vname.
  cv_eqold  = ps_raw-eqold.
  cv_eqnew  = ps_raw-eqnew.
  cv_bktxt  = ps_raw-bktxt.
  cv_excloi = ps_raw-excloi.

  IF cv_bukrs IS INITIAL OR cv_vname IS INITIAL
     OR cv_eqold IS INITIAL OR cv_eqnew IS INITIAL.
    cs_log-message = 'Company code, venture and both equity groups are mandatory'.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  IF ps_raw-gjahr CO '0123456789' AND ps_raw-gjahr IS NOT INITIAL.
    cv_year = ps_raw-gjahr.
  ELSE.
    cs_log-message = 'Posting year is missing or not numeric'.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  IF ps_raw-monat CO '0123456789 ' AND ps_raw-monat IS NOT INITIAL.
    cv_period = ps_raw-monat.
  ELSE.
    cs_log-message = 'Posting period is missing or not numeric'.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  PERFORM to_date USING ps_raw-frdate CHANGING cv_frdate.
  PERFORM to_date USING ps_raw-todate CHANGING cv_todate.
  PERFORM to_date USING ps_raw-budat  CHANGING cv_budat.

  IF cv_frdate IS INITIAL OR cv_todate IS INITIAL.
    cs_log-message = 'Document date from / to are mandatory (format DD.MM.YYYY)'.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  IF cv_frdate > cv_todate.
    cs_log-message = 'Document date from is later than document date to'.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_row
*&---------------------------------------------------------------------*
*& Repeats the checks CL_JVA_EQUITY_ADJUST performs in
*& CHECK_COMPANY_CODE / CHECK_VENTURE / CHECK_EQUITY_GROUP /
*& CHECK_POSTING_DATE / CHECK_AUTHORITY.
*&
*& The class raises these as MESSAGE TYPE 'E', which would terminate the
*& whole event block and abandon every remaining row. Catching them here
*& keeps the rows independent. Only the class's ERROR cases are repeated -
*& its warnings are left to the class.
*&---------------------------------------------------------------------*
FORM validate_row USING    pv_bukrs  TYPE bukrs
                           pv_vname  TYPE jv_name
                           pv_eqold  TYPE jv_egroup
                           pv_eqnew  TYPE jv_egroup
                           pv_year   TYPE gjahr
                           pv_period TYPE monat
                           pv_budat  TYPE syst_datum
                  CHANGING cs_log    TYPE ty_log
                           cv_ok     TYPE abap_bool.

  DATA: ls_t001      TYPE t001,
        ls_t8jv      TYPE t8jv,
        ls_t8jg_old  TYPE t8jg,
        ls_t8jg_new  TYPE t8jg,
        lt_periods   TYPE STANDARD TABLE OF periods,
        ls_periods   TYPE periods,
        lv_poper     TYPE periods-buper,
        lv_last      TYPE periods-buper,
        lv_same_ven  TYPE abap_bool,
        lv_same_type TYPE abap_bool,
        lv_same_curr TYPE abap_bool,
        lv_activity  TYPE c LENGTH 2,
        lv_count     TYPE i.

  cv_ok = abap_true.

  " ---- company code (class: CHECK_COMPANY_CODE, e234) ---------------
  SELECT SINGLE * FROM t001 INTO ls_t001 WHERE bukrs = pv_bukrs.
  IF sy-subrc <> 0 OR ls_t001-xjvaa <> 'X'.
    cs_log-message = |Company code { pv_bukrs } is not active for Joint Venture Accounting|.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  SELECT COUNT( * ) FROM t8jz WHERE bukrs = pv_bukrs.
  IF sy-subrc <> 0.
    cs_log-message = |No JVA company code settings for { pv_bukrs }|.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  " ---- venture (class: CHECK_VENTURE, e232 / e243) ------------------
  SELECT SINGLE * FROM t8jv INTO ls_t8jv
                  WHERE bukrs = pv_bukrs AND vname = pv_vname.
  IF sy-subrc <> 0.
    cs_log-message = |Venture { pv_vname } does not exist in company code { pv_bukrs }|.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  IF ls_t8jv-vtype = '2' OR ls_t8jv-vtype = '5'.
    cs_log-message = |Venture { pv_vname } has a venture type that cannot be equity adjusted|.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  " ---- equity groups (class: CHECK_EQUITY_GROUP, e235/e117/e237/e566)
  SELECT * FROM t8jg INTO ls_t8jg_old UP TO 1 ROWS
           WHERE bukrs = pv_bukrs AND vname = pv_vname AND egrup = pv_eqold.
  ENDSELECT.
  IF sy-subrc <> 0.
    cs_log-message = |Old equity group { pv_eqold } does not exist for venture { pv_vname }|.
    cv_ok = abap_false.
    RETURN.
  ENDIF.
  IF ls_t8jg_old-egroupact <> 'X'.
    cs_log-message = |Old equity group { pv_eqold } is not active|.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  SELECT * FROM t8jg INTO ls_t8jg_new UP TO 1 ROWS
           WHERE bukrs = pv_bukrs AND vname = pv_vname AND egrup = pv_eqnew.
  ENDSELECT.
  IF sy-subrc <> 0.
    cs_log-message = |New equity group { pv_eqnew } does not exist for venture { pv_vname }|.
    cv_ok = abap_false.
    RETURN.
  ENDIF.
  IF ls_t8jg_new-egroupact <> 'X'.
    cs_log-message = |New equity group { pv_eqnew } is not active|.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  IF pv_eqold = pv_eqnew.
    cs_log-message = 'Old and new equity group must be different'.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  CALL FUNCTION 'COMPARE_2_EQUITY_GROUPS'
    EXPORTING
      bukrs        = pv_bukrs
      vname        = pv_vname
      eg_old       = pv_eqold
      eg_new       = pv_eqnew
    IMPORTING
      same_venture = lv_same_ven
      same_type    = lv_same_type
      equal_curr   = lv_same_curr
    EXCEPTIONS
      not_found    = 1
      OTHERS       = 2.
  IF sy-subrc = 0 AND lv_same_ven IS INITIAL.
    cs_log-message = |Equity groups { pv_eqold } and { pv_eqnew } do not belong to the same venture|.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  " ---- period and posting date (class: CHECK_POSTING_DATE) ----------
  CALL FUNCTION 'G_PERIODS_OF_YEAR_GET'
    EXPORTING
      variant             = ls_t001-periv
      year                = pv_year
    IMPORTING
      last_normal_period  = lv_last
    TABLES
      i_periods           = lt_periods
    EXCEPTIONS
      variant_not_defined = 1
      year_not_defined    = 2
      OTHERS              = 3.
  IF sy-subrc <> 0.
    cs_log-message = |Fiscal year variant { ls_t001-periv } is not defined for { pv_year }|.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  lv_poper = pv_period.
  READ TABLE lt_periods INTO ls_periods WITH KEY buper = lv_poper.
  IF sy-subrc <> 0.
    cs_log-message = |Posting period { pv_period } is not allowed (special periods are not permitted)|.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  " The ACDOCA class rejects a posting date outside the period
  " unconditionally - there is no "allow previous periods" option.
  IF pv_budat IS NOT INITIAL.
    IF pv_budat < ls_periods-datab OR pv_budat > ls_periods-datbi.
      cs_log-message = |Posting date { pv_budat DATE = USER } is outside posting period { pv_period }/{ pv_year }|.
      cv_ok = abap_false.
      RETURN.
    ENDIF.
  ENDIF.

  " ---- authorisation (class: CHECK_AUTHORITY) -----------------------
  IF p_update = 'X'.
    lv_activity = '16'.
  ELSE.
    lv_activity = '48'.
  ENDIF.

  CALL FUNCTION 'JV_AUTHORITY_CHECK_PROCESS'
    EXPORTING
      process_code = 'EQUITY-ADJ'
      activity     = lv_activity
      bukrs        = pv_bukrs
    EXCEPTIONS
      OTHERS       = 1.
  IF sy-subrc <> 0.
    cs_log-message = |No authorisation for equity adjustment in company code { pv_bukrs }|.
    cv_ok = abap_false.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form to_date  - DD.MM.YYYY (or YYYYMMDD) text cell to a real date
*&---------------------------------------------------------------------*
FORM to_date USING pv_text TYPE c
             CHANGING cv_date TYPE syst_datum.

  DATA lv_text TYPE c LENGTH 10.

  CLEAR cv_date.
  lv_text = pv_text.
  CONDENSE lv_text NO-GAPS.

  IF lv_text IS INITIAL.
    RETURN.
  ENDIF.

  IF lv_text CS '.'.
    cv_date+6(2) = lv_text+0(2).      " day
    cv_date+4(2) = lv_text+3(2).      " month
    cv_date+0(4) = lv_text+6(4).      " year
  ELSEIF strlen( lv_text ) = 8 AND lv_text CO '0123456789'.
    cv_date = lv_text.                " YYYYMMDD
  ENDIF.

  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                      = cv_date
    EXCEPTIONS
      plausibility_check_failed = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    CLEAR cv_date.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form build_ranges
*&---------------------------------------------------------------------*
*& A blank cell means no restriction, matching an empty field on the
*& GJ19A screen.
*&---------------------------------------------------------------------*
FORM build_ranges USING ps_raw TYPE ty_raw.

  DATA: lv_from  TYPE syst_datum,
        lv_to    TYPE syst_datum,
        lv_acctf TYPE racct,
        lv_acctt TYPE racct,
        lv_cntr  TYPE kostl,
        lv_ordnr TYPE aufnr,
        lv_nplnr TYPE aufnr,
        lv_pspnr TYPE ps_posnr.

  CLEAR: gt_r_budat, gt_r_acct, gt_r_cntr, gt_r_ordnr,
         gt_r_projk, gt_r_nplnr, gt_r_vornr.

  " Selection posting-date range
  PERFORM to_date USING ps_raw-selbudf CHANGING lv_from.
  PERFORM to_date USING ps_raw-selbudt CHANGING lv_to.
  IF lv_from IS NOT INITIAL.
    IF lv_to IS INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_from ) TO gt_r_budat.
    ELSE.
      APPEND VALUE #( sign = 'I' option = 'BT' low = lv_from high = lv_to ) TO gt_r_budat.
    ENDIF.
  ENDIF.

  " Account range - accounts are stored with leading zeros
  IF ps_raw-acctf IS NOT INITIAL.
    lv_acctf = ps_raw-acctf.
    PERFORM alpha_in CHANGING lv_acctf.
    IF ps_raw-acctt IS INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_acctf ) TO gt_r_acct.
    ELSE.
      lv_acctt = ps_raw-acctt.
      PERFORM alpha_in CHANGING lv_acctt.
      APPEND VALUE #( sign = 'I' option = 'BT' low = lv_acctf high = lv_acctt ) TO gt_r_acct.
    ENDIF.
  ENDIF.

  IF ps_raw-cntr IS NOT INITIAL.
    lv_cntr = ps_raw-cntr.
    PERFORM alpha_in CHANGING lv_cntr.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_cntr ) TO gt_r_cntr.
  ENDIF.

  IF ps_raw-ordnr IS NOT INITIAL.
    lv_ordnr = ps_raw-ordnr.
    PERFORM alpha_in CHANGING lv_ordnr.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_ordnr ) TO gt_r_ordnr.
  ENDIF.

  IF ps_raw-nplnr IS NOT INITIAL.
    lv_nplnr = ps_raw-nplnr.
    PERFORM alpha_in CHANGING lv_nplnr.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_nplnr ) TO gt_r_nplnr.
  ENDIF.

  IF ps_raw-vornr IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = ps_raw-vornr ) TO gt_r_vornr.
  ENDIF.

  " WBS element: the file carries the EXTERNAL key, the class expects the
  " internal number - same conversion RGJVEA10_ACD does in
  " FORM convert_project_range.
  IF ps_raw-projk IS NOT INITIAL.
    CALL FUNCTION 'CJPN_EXTERN_TO_INTERN_CONV'
      EXPORTING
        ext_num       = ps_raw-projk
      IMPORTING
        int_num       = lv_pspnr
      EXCEPTIONS
        not_found     = 1
        error_message = 2
        OTHERS        = 4.
    IF sy-subrc = 0 AND lv_pspnr IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_pspnr ) TO gt_r_projk.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form alpha_in  - apply the ALPHA conversion (leading zeros)
*&---------------------------------------------------------------------*
FORM alpha_in CHANGING cv_value TYPE any.

  IF cv_value IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = cv_value
    IMPORTING
      output = cv_value.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form read_row_result
*&---------------------------------------------------------------------*
*& IF_JVA_EQUITY_ADJUST exposes no results getter and the class keeps
*& MS_EA_RESULTS private, so the run's outcome is read back from the
*& application log the class writes to (object JVA, saved to the
*& database - see CL_JVA_EQUITY_ADJUST, method INITIALIZE_OBJECTS).
*&
*& The log is identified by the time window the row ran in, plus user.
*&---------------------------------------------------------------------*
FORM read_row_result USING    pv_date_fr TYPE sy-datum
                              pv_time_fr TYPE sy-uzeit
                     CHANGING cs_log     TYPE ty_log.

  DATA: ls_filter    TYPE bal_s_lfil,
        lt_header    TYPE balhdr_t,
        lt_msg_hndl  TYPE bal_t_msgh,
        ls_msg       TYPE bal_s_msg,
        lv_txt       TYPE string.

  CLEAR: cs_log-errors, cs_log-warns.

  APPEND VALUE #( sign = 'I' option = 'EQ'
                  low  = if_jva_message_output=>gc_bal_object-jva )
         TO ls_filter-object.
  APPEND VALUE #( sign = 'I' option = 'EQ'
                  low  = if_jva_message_output=>gc_bal_subobject-general )
         TO ls_filter-subobject.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = pv_date_fr ) TO ls_filter-aldate.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = sy-uname )   TO ls_filter-aluser.
  APPEND VALUE #( sign = 'I' option = 'BT'
                  low  = pv_time_fr high = sy-uzeit )         TO ls_filter-altime.

  CALL FUNCTION 'BAL_DB_SEARCH'
    EXPORTING
      i_s_log_filter = ls_filter
    IMPORTING
      e_t_log_header = lt_header
    EXCEPTIONS
      log_not_found  = 1
      OTHERS         = 2.

  IF sy-subrc <> 0 OR lt_header IS INITIAL.
    " No log written - the run had nothing to say.
    cs_log-status = 'Success'.
    RETURN.
  ENDIF.

  " Severity counts come straight off the log headers.
  LOOP AT lt_header INTO DATA(ls_header).
    cs_log-errors = cs_log-errors + ls_header-msg_cnt_a + ls_header-msg_cnt_e.
    cs_log-warns  = cs_log-warns  + ls_header-msg_cnt_w.
  ENDLOOP.

  IF cs_log-errors > 0.
    cs_log-status = 'Error'.
  ELSEIF cs_log-warns > 0.
    cs_log-status = 'Warning'.
  ELSE.
    cs_log-status = 'Success'.
    RETURN.
  ENDIF.

  " Load the logs and pull the first message that explains the status.
  CALL FUNCTION 'BAL_DB_LOAD'
    EXPORTING
      i_t_log_header = lt_header
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
    EXPORTING
      i_s_msg_filter = VALUE bal_s_mfil(
                         msgty = COND #( WHEN cs_log-errors > 0
                                         THEN VALUE #( ( sign = 'I' option = 'EQ' low = 'E' )
                                                       ( sign = 'I' option = 'EQ' low = 'A' ) )
                                         ELSE VALUE #( ( sign = 'I' option = 'EQ' low = 'W' ) ) ) )
    IMPORTING
      e_t_msg_handle = lt_msg_hndl
    EXCEPTIONS
      msg_not_found  = 1
      OTHERS         = 2.
  IF sy-subrc <> 0 OR lt_msg_hndl IS INITIAL.
    RETURN.
  ENDIF.

  READ TABLE lt_msg_hndl INTO DATA(ls_hndl) INDEX 1.
  IF sy-subrc = 0.
    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = ls_hndl
      IMPORTING
        e_s_msg        = ls_msg
        e_txt_msg      = lv_txt
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc = 0.
      cs_log-message = lv_txt.
    ENDIF.
  ENDIF.

  " Free the loaded logs so the next row starts clean.
  CALL FUNCTION 'BAL_GLB_MEMORY_REFRESH'
    EXCEPTIONS
      OTHERS = 1.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form progress
*&---------------------------------------------------------------------*
FORM progress USING pv_done  TYPE i
                    pv_total TYPE i
                    pv_text  TYPE any.

  DATA lv_pct TYPE i.

  IF pv_total > 0.
    lv_pct = pv_done * 100 / pv_total.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = lv_pct
      text       = |Equity adjustment { pv_text } ({ pv_done }/{ pv_total })|.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form show_log  - ALV summary, one line per uploaded row
*&---------------------------------------------------------------------*
FORM show_log.

  DATA lo_alv TYPE REF TO cl_salv_table.

  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_alv
        CHANGING  t_table      = gt_log ).
      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_columns( )->set_optimize( abap_true ).
      lo_alv->display( ).
    CATCH cx_salv_msg INTO DATA(lx).
      MESSAGE lx->get_text( ) TYPE 'I'.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& ALV callback handlers
*&---------------------------------------------------------------------*
*& The standard class writes its own result lists through callbacks into
*& the calling report - that is what iv_repid_callback = sy-repid is for.
*& These seven FORMs mirror the ones in RGJVEA10_ACD one for one; without
*& them the class cannot produce its output.
*&---------------------------------------------------------------------*
FORM top_of_page_handler_ea ##CALLED.
  IF go_adjust IS BOUND.
    go_adjust->handle_ea_event_top_of_page( ).
  ENDIF.
ENDFORM.

FORM top_of_list_handler_ea ##CALLED.
  IF go_adjust IS BOUND.
    go_adjust->handle_ea_event_top_of_list( ).
  ENDIF.
ENDFORM.

FORM before_line_output_handler_ea USING rs_lineinfo TYPE slis_lineinfo ##CALLED.
  IF go_adjust IS BOUND.
    go_adjust->handle_ea_event_befor_line_out( rs_lineinfo ).
  ENDIF.
ENDFORM.

FORM after_line_output_handler_ea USING rs_lineinfo TYPE slis_lineinfo ##CALLED.
  IF go_adjust IS BOUND.
    go_adjust->handle_ea_event_after_line_out( rs_lineinfo ).
  ENDIF.
ENDFORM.

FORM top_of_page_handler_cb ##CALLED.
  IF go_adjust IS BOUND.
    go_adjust->handle_cb_event_top_of_page( ).
  ENDIF.
ENDFORM.

FORM top_of_list_handler_cb ##CALLED.
  IF go_adjust IS BOUND.
    go_adjust->handle_cb_event_top_of_list( ).
  ENDIF.
ENDFORM.

FORM after_line_output_handler_cb USING rs_lineinfo TYPE slis_lineinfo ##CALLED.
  IF go_adjust IS BOUND.
    go_adjust->handle_cb_event_after_line_out( rs_lineinfo ).
  ENDIF.
ENDFORM.
