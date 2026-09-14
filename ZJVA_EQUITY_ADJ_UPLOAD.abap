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
*&                 Each row of the Excel file is ONE complete execution
*&                 of the standard transaction - it carries its own
*&                 venture, old and new equity group, document date
*&                 range, posting year / period / date and object
*&                 filters. Amounts, recovery indicators, document types
*&                 and account determination are NEVER taken from the
*&                 file; they are derived by standard SAP logic exactly
*&                 as they are in GJ19A. That is deliberate - it is what
*&                 keeps the run reconcilable with cutback and stops a
*&                 re-run from double posting.
*&
*&                 No standard logic is copied. For every row the program
*&                 instantiates the standard class CL_JVA_EQUITY_ADJUST
*&                 and calls the same three methods that the standard
*&                 report RGJVEA10_ACD (transaction GJ19A) calls:
*&
*&                     set_run_parameters( )
*&                     check_screen_parameters( )
*&                     run_process( )
*&
*&                 Reversal, re-booking, cutback correction and posting
*&                 therefore remain 100% standard, and future SAP
*&                 corrections to that class flow through automatically.
*&
*&                 Excel is read with the standard FM
*&                 TEXT_CONVERT_XLS_TO_SAP. Results are shown as an ALV
*&                 log, one line per uploaded row.
*&
*& Reference     : RGJVEA10_ACD / RGJVEAS0_ACD (transaction GJ19A)
*&
*& NOTE          : Rows are processed independently. A row that fails
*&                 does not stop the rows after it - the failure is
*&                 reported in the log. Run type defaults to Test Run.
*&---------------------------------------------------------------------*
REPORT zjva_equity_adj_upload.

TABLES acdoca.

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
         projk   TYPE c LENGTH 24,   " 19 WBS Element (external)
         nplnr   TYPE c LENGTH 12,   " 20 Network
         vornr   TYPE c LENGTH 4,    " 21 Network Activity
       END OF ty_raw.

TYPES: BEGIN OF ty_log,
         rowno   TYPE c LENGTH 6,
         bukrs   TYPE c LENGTH 4,
         vname   TYPE c LENGTH 6,
         eqold   TYPE c LENGTH 3,
         eqnew   TYPE c LENGTH 3,
         gjahr   TYPE c LENGTH 4,
         monat   TYPE c LENGTH 2,
         runtype TYPE c LENGTH 12,          " Analysis / Test / Update
         status  TYPE c LENGTH 10,          " Success / Error / Skipped
         msgid   TYPE sy-msgid,
         msgno   TYPE sy-msgno,
         message TYPE c LENGTH 200,
       END OF ty_log.

*----------------------------------------------------------------------*
* Global data
*----------------------------------------------------------------------*
" go_adjust must be GLOBAL: the standard class calls back into this
" report's ALV handler FORMs (see iv_repid_callback below), and those
" FORMs need the object reference of the row currently being processed.
DATA: go_adjust TYPE REF TO if_jva_equity_adjust.

DATA: gt_raw TYPE STANDARD TABLE OF ty_raw,
      gt_log TYPE STANDARD TABLE OF ty_log.

RANGES: gr_budat FOR sy-datum,
        gr_acct  FOR acdoca-racct,
        gr_cntr  FOR acdoca-rcntr,
        gr_ordnr FOR acdoca-aufnr,
        gr_projk FOR acdoca-ps_psp_pnr,
        gr_nplnr FOR acdoca-nplnr,
        gr_vornr FOR acdoca-vornr.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  PARAMETERS p_xls TYPE string OBLIGATORY.        " upload file (.xls/.xlsx)
SELECTION-SCREEN END OF BLOCK b1.

" Run-level processing options. These are NOT per row - they apply to
" every row in the file, exactly as they would to a single GJ19A run.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
  PARAMETERS: p_single RADIOBUTTON GROUP proc DEFAULT 'X', " Single Item
              p_aggreg RADIOBUTTON GROUP proc.             " Aggregated
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
  PARAMETERS: p_ivpeqg RADIOBUTTON GROUP pst1 DEFAULT 'X', " per Equity Group
              p_ivpptn RADIOBUTTON GROUP pst1,             " per Partner
              p_ivpexp RADIOBUTTON GROUP pst1.             " per Partner/Expense
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-004.
  PARAMETERS: p_displ  RADIOBUTTON GROUP runt,             " Analysis only
              p_test   RADIOBUTTON GROUP runt DEFAULT 'X', " Test run
              p_update RADIOBUTTON GROUP runt.             " Update run
SELECTION-SCREEN END OF BLOCK b4.

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

  " Drop completely empty trailing rows and any sample rows left behind.
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

  DATA: ls_log    TYPE ty_log,
        lv_bukrs  TYPE jva_search_help_ts-bukrs,
        lv_vname  TYPE jva_search_help_ts-vname,
        lv_eqold  TYPE jvto1-regrou,
        lv_eqnew  TYPE jvto1-regrou,
        lv_year   TYPE bkpf-gjahr,
        lv_period TYPE bkpf-monat,
        lv_frdate TYPE sy-datum,
        lv_todate TYPE sy-datum,
        lv_budat  TYPE sy-datum,
        lv_bktxt  TYPE bkpf-bktxt,
        lv_excloi TYPE c LENGTH 1,
        lv_ok     TYPE abap_bool.

  " ---- build the log line up front so every exit path reports it -----
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

  " ---- convert and validate the row ---------------------------------
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

  " ---- build the object-filter ranges for this row ------------------
  PERFORM build_ranges USING ps_raw.

  " ---- authorisation, same process code the standard uses -----------
  PERFORM check_authority USING    lv_bukrs
                          CHANGING ls_log lv_ok.
  IF lv_ok = abap_false.
    ls_log-status = 'Error'.
    APPEND ls_log TO gt_log.
    RETURN.
  ENDIF.

  " ---- run the STANDARD equity adjustment for this row --------------
  " Mirrors FORM check_parameters / FORM run of RGJVEA10_ACD exactly.
  CLEAR go_adjust.

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
        it_acct           = gr_acct[]
        it_budat          = gr_budat[]
        it_cntr           = gr_cntr[]
        it_ordnr          = gr_ordnr[]
        it_projk          = gr_projk[]
        it_nplnr          = gr_nplnr[]
        it_vornr          = gr_vornr[]
        iv_bktxt          = lv_bktxt
        iv_ivpeqg         = p_ivpeqg
        iv_ivpptn         = p_ivpptn
        iv_ivpexp         = p_ivpexp
        iv_ec_aggregated  = p_aggreg
        iv_analysis_only  = p_displ
        iv_test           = p_test
        iv_repid_callback = sy-repid
        iv_farm_in_out    = abap_false ).

      " Standard validation (authority + selection data). It may adjust
      " the date range, exactly as it does on the GJ19A screen.
      go_adjust->check_screen_parameters( CHANGING cv_frdate = lv_frdate
                                                   cv_todate = lv_todate ).

      " Reversal, re-booking and cutback correction - all standard.
      go_adjust->run_process( ).

      ls_log-status = 'Success'.

    CATCH cx_root INTO DATA(lx_root).
      ls_log-status  = 'Error'.
      ls_log-message = lx_root->get_text( ).
  ENDTRY.

  " A message raised inside the run is the most specific thing we have.
  IF ls_log-message IS INITIAL AND sy-msgid IS NOT INITIAL.
    ls_log-msgid = sy-msgid.
    ls_log-msgno = sy-msgno.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
            INTO ls_log-message.
    IF sy-msgty CA 'EAX'.
      ls_log-status = 'Error'.
    ENDIF.
  ENDIF.

  APPEND ls_log TO gt_log.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form convert_row
*&---------------------------------------------------------------------*
*& Converts the character cells into typed fields and validates what can
*& be validated before the standard checks take over.
*&---------------------------------------------------------------------*
FORM convert_row USING    ps_raw    TYPE ty_raw
                 CHANGING cv_bukrs  TYPE jva_search_help_ts-bukrs
                          cv_vname  TYPE jva_search_help_ts-vname
                          cv_eqold  TYPE jvto1-regrou
                          cv_eqnew  TYPE jvto1-regrou
                          cv_year   TYPE bkpf-gjahr
                          cv_period TYPE bkpf-monat
                          cv_frdate TYPE sy-datum
                          cv_todate TYPE sy-datum
                          cv_budat  TYPE sy-datum
                          cv_bktxt  TYPE bkpf-bktxt
                          cv_excloi TYPE c
                          cs_log    TYPE ty_log
                          cv_ok     TYPE abap_bool.

  cv_ok = abap_true.

  cv_bukrs  = ps_raw-bukrs.
  cv_vname  = ps_raw-vname.
  cv_eqold  = ps_raw-eqold.
  cv_eqnew  = ps_raw-eqnew.
  cv_bktxt  = ps_raw-bktxt.
  cv_excloi = ps_raw-excloi.

  " ---- mandatory fields ---------------------------------------------
  IF cv_bukrs IS INITIAL OR cv_vname IS INITIAL
     OR cv_eqold IS INITIAL OR cv_eqnew IS INITIAL.
    cs_log-message = 'Company code, venture and both equity groups are mandatory'.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  IF cv_eqold = cv_eqnew.
    cs_log-message = 'Old and new equity group must be different'.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  " ---- year / period -------------------------------------------------
  IF ps_raw-gjahr CO ' 0123456789' AND ps_raw-gjahr IS NOT INITIAL.
    cv_year = ps_raw-gjahr.
  ELSE.
    cs_log-message = 'Posting year is missing or not numeric'.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  IF ps_raw-monat CO ' 0123456789' AND ps_raw-monat IS NOT INITIAL.
    cv_period = ps_raw-monat.
  ELSE.
    cs_log-message = 'Posting period is missing or not numeric'.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  IF cv_period < '01' OR cv_period > '12'.
    cs_log-message = 'Posting period must be between 01 and 12 (no special periods)'.
    cv_ok = abap_false.
    RETURN.
  ENDIF.

  " ---- dates ---------------------------------------------------------
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
*& Form to_date  - DD.MM.YYYY (or YYYYMMDD) text cell to a real date
*&---------------------------------------------------------------------*
FORM to_date USING pv_text TYPE c
             CHANGING cv_date TYPE sy-datum.

  DATA lv_text TYPE c LENGTH 10.

  CLEAR cv_date.
  lv_text = pv_text.
  CONDENSE lv_text NO-GAPS.

  IF lv_text IS INITIAL.
    RETURN.
  ENDIF.

  IF lv_text CS '.'.
    " DD.MM.YYYY
    cv_date+6(2) = lv_text+0(2).      " day
    cv_date+4(2) = lv_text+3(2).      " month
    cv_date+0(4) = lv_text+6(4).      " year
  ELSEIF strlen( lv_text ) = 8 AND lv_text CO '0123456789'.
    " YYYYMMDD, as Excel sometimes hands it over
    cv_date = lv_text.
  ENDIF.

  " Reject anything that is not a real calendar date.
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
*& Builds the object-filter ranges for one row. A blank cell means no
*& restriction, which matches leaving the field empty on the GJ19A screen.
*&---------------------------------------------------------------------*
FORM build_ranges USING ps_raw TYPE ty_raw.

  DATA: lv_from TYPE sy-datum,
        lv_to   TYPE sy-datum.

  REFRESH: gr_budat, gr_acct, gr_cntr, gr_ordnr,
           gr_projk, gr_nplnr, gr_vornr.

  " Selection posting-date range
  PERFORM to_date USING ps_raw-selbudf CHANGING lv_from.
  PERFORM to_date USING ps_raw-selbudt CHANGING lv_to.
  IF lv_from IS NOT INITIAL OR lv_to IS NOT INITIAL.
    gr_budat-sign = 'I'.
    IF lv_to IS INITIAL.
      gr_budat-option = 'EQ'.
      gr_budat-low    = lv_from.
    ELSE.
      gr_budat-option = 'BT'.
      gr_budat-low    = lv_from.
      gr_budat-high   = lv_to.
    ENDIF.
    APPEND gr_budat.
  ENDIF.

  " Account range
  IF ps_raw-acctf IS NOT INITIAL.
    gr_acct-sign = 'I'.
    IF ps_raw-acctt IS INITIAL.
      gr_acct-option = 'EQ'.
      gr_acct-low    = ps_raw-acctf.
    ELSE.
      gr_acct-option = 'BT'.
      gr_acct-low    = ps_raw-acctf.
      gr_acct-high   = ps_raw-acctt.
    ENDIF.
    " Accounts are stored with leading zeros.
    PERFORM alpha_in CHANGING gr_acct-low.
    PERFORM alpha_in CHANGING gr_acct-high.
    APPEND gr_acct.
  ENDIF.

  " Single-value object filters
  IF ps_raw-cntr IS NOT INITIAL.
    gr_cntr-sign = 'I'. gr_cntr-option = 'EQ'. gr_cntr-low = ps_raw-cntr.
    PERFORM alpha_in CHANGING gr_cntr-low.
    APPEND gr_cntr.
  ENDIF.

  IF ps_raw-ordnr IS NOT INITIAL.
    gr_ordnr-sign = 'I'. gr_ordnr-option = 'EQ'. gr_ordnr-low = ps_raw-ordnr.
    PERFORM alpha_in CHANGING gr_ordnr-low.
    APPEND gr_ordnr.
  ENDIF.

  IF ps_raw-nplnr IS NOT INITIAL.
    gr_nplnr-sign = 'I'. gr_nplnr-option = 'EQ'. gr_nplnr-low = ps_raw-nplnr.
    PERFORM alpha_in CHANGING gr_nplnr-low.
    APPEND gr_nplnr.
  ENDIF.

  IF ps_raw-vornr IS NOT INITIAL.
    gr_vornr-sign = 'I'. gr_vornr-option = 'EQ'. gr_vornr-low = ps_raw-vornr.
    APPEND gr_vornr.
  ENDIF.

  " WBS element: the file carries the EXTERNAL key, the class expects the
  " internal number - same conversion FORM convert_project_range does in
  " the standard report.
  IF ps_raw-projk IS NOT INITIAL.
    DATA lv_pspnr TYPE ps_posnr.
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
      gr_projk-sign = 'I'. gr_projk-option = 'EQ'. gr_projk-low = lv_pspnr.
      APPEND gr_projk.
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
*& Form check_authority
*&---------------------------------------------------------------------*
*& Same process code the standard equity adjustment checks against.
*& Activity 16 = execute (update), 48 = simulate (test / analysis).
*&---------------------------------------------------------------------*
FORM check_authority USING    pv_bukrs TYPE jva_search_help_ts-bukrs
                     CHANGING cs_log   TYPE ty_log
                              cv_ok    TYPE abap_bool.

  DATA lv_activity TYPE c LENGTH 2.

  cv_ok = abap_true.

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
