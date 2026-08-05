*&---------------------------------------------------------------------*
*& Report  YGMS_CGD_GAS_SCHEDULING
*&---------------------------------------------------------------------*
*& NAME OF PROGRAM : YGMS_CGD_GAS_SCHEDULING
*& DESCRIPTION     : System based Gas Scheduling to CGD Gas (FMS/GAIL)
*&                   Stage 1: GA ID wise Demanded Nomination
*&                            (Upload File / Calculate / Download)
*&                   Stage 2: Contract wise Scheduled Nomination
*&                            (Upload File / Calculate / Validate /
*&                             Nomination Create via YRXR006)
*& REFERENCE       : FS "System based Gas Scheduling to CGD Gas.docx"
*&                   Nomination creation via program
*&                   YRXR028_NOMINATION_NEW (T-Code YRXR001 / YRXR006)
*&---------------------------------------------------------------------*
REPORT ygms_cgd_gas_scheduling.

TABLES: kna1.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
* GA ID master mapping (from T176T: VTEXT = "<GAID>_<Description>")
TYPES: BEGIN OF ty_gamap,
         upl_gaid TYPE ygms_cgd_gaid,   " GA ID token, e.g. 99.01
         bsark    TYPE bsark,           " SAP GA ID (PO type)
         vtext    TYPE bezei20,         " full description
       END OF ty_gamap.

* Parsed matrix cell of the upload file
TYPES: BEGIN OF ty_upl,
         gas_day  TYPE datum,
         upl_gaid TYPE ygms_cgd_gaid,
         kunnr    TYPE kunnr,
         bsark    TYPE bsark,
         vtext    TYPE bezei20,
         qty      TYPE ygms_cgd_qty,
         uom      TYPE meins,
       END OF ty_upl.

* Stage 1 Calculate ALV
TYPES: BEGIN OF ty_alv1,
         sap_gaid  TYPE bsark,
         upl_gaid  TYPE ygms_cgd_gaid,
         gaid_desc TYPE bezei20,
         gas_day   TYPE datum,
         upl_qty   TYPE ygms_cgd_qty,
         cal_qty   TYPE ygms_cgd_qty,
         sum_dcq   TYPE ygms_cgd_qty,
         sum_max   TYPE ygms_cgd_qty,
         sum_min   TYPE ygms_cgd_qty,
       END OF ty_alv1.

* Contract level result of Stage 1 Calculate (for YRXA_GAID_DCQ)
TYPES: BEGIN OF ty_cdcq,
         contract  TYPE vbeln_va,
         kunnr     TYPE kunnr,
         sap_gaid  TYPE bsark,
         upl_gaid  TYPE ygms_cgd_gaid,
         gaid_desc TYPE bezei20,
         gas_day   TYPE datum,
         cal_qty   TYPE ygms_cgd_qty,
         dcq       TYPE ygms_cgd_qty,
         dcq_max   TYPE ygms_cgd_qty,
         dcq_min   TYPE ygms_cgd_qty,
       END OF ty_cdcq.

* Stage 2 Calculate ALV
TYPES: BEGIN OF ty_alv2,
         contract  TYPE vbeln_va,
         kunnr     TYPE kunnr,
         sap_gaid  TYPE bsark,
         upl_gaid  TYPE ygms_cgd_gaid,
         gaid_desc TYPE bezei20,
         gas_day   TYPE datum,
         dcq       TYPE ygms_cgd_qty,
         dcq_max   TYPE ygms_cgd_qty,
         dcq_min   TYPE ygms_cgd_qty,
         cnf_qty   TYPE ygms_cgd_qty,
       END OF ty_alv2.

* Error / message ALVs
TYPES: BEGIN OF ty_err,
         gas_day  TYPE datum,
         upl_gaid TYPE ygms_cgd_gaid,
         kunnr    TYPE kunnr,
         message  TYPE bapi_msg,
       END OF ty_err.

TYPES: BEGIN OF ty_err2,
         contract TYPE vbeln_va,
         gas_day  TYPE datum,
         message  TYPE bapi_msg,
       END OF ty_err2.

* Download matrix (max 30 gas days as per FS)
TYPES: BEGIN OF ty_matrix,
         upl_gaid TYPE ygms_cgd_gaid,
         d01      TYPE ygms_cgd_qty,
         d02      TYPE ygms_cgd_qty,
         d03      TYPE ygms_cgd_qty,
         d04      TYPE ygms_cgd_qty,
         d05      TYPE ygms_cgd_qty,
         d06      TYPE ygms_cgd_qty,
         d07      TYPE ygms_cgd_qty,
         d08      TYPE ygms_cgd_qty,
         d09      TYPE ygms_cgd_qty,
         d10      TYPE ygms_cgd_qty,
         d11      TYPE ygms_cgd_qty,
         d12      TYPE ygms_cgd_qty,
         d13      TYPE ygms_cgd_qty,
         d14      TYPE ygms_cgd_qty,
         d15      TYPE ygms_cgd_qty,
         d16      TYPE ygms_cgd_qty,
         d17      TYPE ygms_cgd_qty,
         d18      TYPE ygms_cgd_qty,
         d19      TYPE ygms_cgd_qty,
         d20      TYPE ygms_cgd_qty,
         d21      TYPE ygms_cgd_qty,
         d22      TYPE ygms_cgd_qty,
         d23      TYPE ygms_cgd_qty,
         d24      TYPE ygms_cgd_qty,
         d25      TYPE ygms_cgd_qty,
         d26      TYPE ygms_cgd_qty,
         d27      TYPE ygms_cgd_qty,
         d28      TYPE ygms_cgd_qty,
         d29      TYPE ygms_cgd_qty,
         d30      TYPE ygms_cgd_qty,
       END OF ty_matrix.

* Helper structures for DB reads
TYPES: BEGIN OF ty_kna1,
         kunnr  TYPE kunnr,
         yygaid TYPE bsark,
       END OF ty_kna1.

TYPES: BEGIN OF ty_cont,
         vbeln TYPE vbeln_va,
         kunnr TYPE kunnr,
       END OF ty_cont.

TYPES: BEGIN OF ty_vbap,
         vbeln TYPE vbeln_va,
         posnr TYPE posnr_va,
         zieme TYPE dzieme,
       END OF ty_vbap.

TYPES: BEGIN OF ty_t176t,
         bsark TYPE bsark,
         vtext TYPE bezei20,
       END OF ty_t176t.

TYPES: BEGIN OF ty_nomi,
         docnr TYPE oij_docnr,
         idate TYPE oij_idate,
       END OF ty_nomi.

* Structures for memory hand-off to YRXR028_NOMINATION_NEW (YRXR006).
* MUST match TY_NOMN / TY_NOMN_R / TY_NOMN_ERROR of that program 1:1.
TYPES: BEGIN OF ty_nomn,
         vbeln      TYPE vbeln,
         idate      TYPE aedat,
         menge      TYPE oij_menge,
         unit       TYPE oij_uniti,
         gujdnq     TYPE yyoij_gujdnq,
         gujdnq_uom TYPE yyoij_gujdnq_uom,
         dqty       TYPE oij_menge,
       END OF ty_nomn.

TYPES: BEGIN OF ty_nomn_r,
         vbeln       TYPE vbeln,
         idate       TYPE aedat,
         yy_bnreason TYPE yy_bnreason,
       END OF ty_nomn_r.

TYPES: BEGIN OF ty_nomn_error,
         vbeln TYPE vbeln,
         idate TYPE aedat,
         menge TYPE oij_menge,
         unit  TYPE oij_uniti,
         error(255),
       END OF ty_nomn_error.

*----------------------------------------------------------------------*
* Global data
*----------------------------------------------------------------------*
DATA: gv_gaid  TYPE ygms_cgd_gaid,
      gv_augru TYPE augru.

DATA: gt_gamap TYPE SORTED TABLE OF ty_gamap WITH UNIQUE KEY upl_gaid.

DATA: gt_cells TYPE TABLE OF alsmex_tabline,
      gt_dates TYPE TABLE OF datum,           " index = excel col - 6
      gt_upl   TYPE TABLE OF ty_upl.

DATA: gt_alv1 TYPE TABLE OF ty_alv1,
      gt_cdcq TYPE TABLE OF ty_cdcq,
      gt_alv2 TYPE TABLE OF ty_alv2.

DATA: gt_err  TYPE TABLE OF ty_err,
      gt_err2 TYPE TABLE OF ty_err2.

DATA: gv_seqno TYPE ygms_cgd_seqno.

* Tables with header lines - names must match YRXR028 IMPORT statements
DATA: it_nomn         TYPE TABLE OF ty_nomn       WITH HEADER LINE,
      it_nomn_r       TYPE TABLE OF ty_nomn_r     WITH HEADER LINE,
      itab_nomination TYPE TABLE OF ty_nomn_error WITH HEADER LINE.

DATA: gt_bdc TYPE TABLE OF bdcdata.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: rb_dmup  RADIOBUTTON GROUP rad1 DEFAULT 'X'
                     USER-COMMAND ucom,
            rb_dmcal RADIOBUTTON GROUP rad1,
            rb_dmdwn RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: rb_cfup  RADIOBUTTON GROUP rad1,
            rb_cfcal RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK b2.

* File upload block (Stage 1 + Stage 2 upload)
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_file TYPE rlgrap-filename MODIF ID fil.
SELECTION-SCREEN PUSHBUTTON /1(30) btn_tmpl USER-COMMAND tmpl
                     MODIF ID fil.
SELECTION-SCREEN END OF BLOCK b3.

* Stage 1 Calculate: Gas day single mandatory, GA ID multiple no range
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
PARAMETERS: p_gday TYPE sy-datum MODIF ID cal.
SELECT-OPTIONS: s_gaid FOR gv_gaid NO INTERVALS MODIF ID cal.
SELECTION-SCREEN END OF BLOCK b4.

* Stage 1 Download: Gas day range (within one FN), GA ID multiple
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.
SELECT-OPTIONS: s_gday  FOR sy-datum MODIF ID dwn,
                s_gaid2 FOR gv_gaid NO INTERVALS MODIF ID dwn.
SELECTION-SCREEN END OF BLOCK b5.

* Stage 2 Calculate: Gas day multiple mandatory, Customer multiple
SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-006.
SELECT-OPTIONS: s_gday2 FOR sy-datum NO INTERVALS MODIF ID cf2,
                s_kunnr FOR kna1-kunnr NO INTERVALS MODIF ID cf2.
SELECTION-SCREEN END OF BLOCK b6.

*----------------------------------------------------------------------*
INITIALIZATION.
  btn_tmpl = 'Template Download'.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'FIL'.
        IF rb_dmup = 'X' OR rb_cfup = 'X'.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.
      WHEN 'CAL'.
        IF rb_dmcal = 'X'.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.
      WHEN 'DWN'.
        IF rb_dmdwn = 'X'.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.
      WHEN 'CF2'.
        IF rb_cfcal = 'X'.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f_f4_file CHANGING p_file.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF sy-ucomm = 'TMPL'.
    PERFORM f_template_download.
  ENDIF.

*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM f_get_augru.
  PERFORM f_load_gaid_map.

  CASE 'X'.
    WHEN rb_dmup.
      PERFORM f_dm_upload.
    WHEN rb_dmcal.
      PERFORM f_dm_calculate.
    WHEN rb_dmdwn.
      PERFORM f_dm_download.
    WHEN rb_cfup.
      PERFORM f_cf_upload.
    WHEN rb_cfcal.
      PERFORM f_cf_calculate.
  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  F_GET_AUGRU
*&---------------------------------------------------------------------*
* Order reason code whose description is 'CGD POOL' (TVAUT)
*----------------------------------------------------------------------*
FORM f_get_augru.
  SELECT SINGLE augru FROM tvaut INTO gv_augru
    WHERE spras = 'E'
      AND bezei = 'CGD POOL'.
  IF sy-subrc <> 0.
    MESSAGE 'Order reason with description CGD POOL not found in TVAUT'
      TYPE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_LOAD_GAID_MAP
*&---------------------------------------------------------------------*
* GA ID -> BSARK mapping from T176T. VTEXT format: <GAID>_<Description>
*----------------------------------------------------------------------*
FORM f_load_gaid_map.
  DATA: lt_t176t TYPE TABLE OF ty_t176t,
        ls_t176t TYPE ty_t176t,
        ls_map   TYPE ty_gamap,
        lv_token TYPE ygms_cgd_gaid,
        lv_rest  TYPE string.

  SELECT bsark vtext FROM t176t
    INTO TABLE lt_t176t
    WHERE spras = 'E'.

  LOOP AT lt_t176t INTO ls_t176t.
    CLEAR: ls_map, lv_token, lv_rest.
    SPLIT ls_t176t-vtext AT '_' INTO lv_token lv_rest.
*   Only entries following the GA ID naming pattern (token with '.')
    IF lv_token CA '.'.
      ls_map-upl_gaid = lv_token.
      ls_map-bsark    = ls_t176t-bsark.
      ls_map-vtext    = ls_t176t-vtext.
      INSERT ls_map INTO TABLE gt_gamap.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_F4_FILE
*&---------------------------------------------------------------------*
FORM f_f4_file CHANGING cv_file TYPE rlgrap-filename.
  DATA: lt_files TYPE filetable,
        ls_file  TYPE file_table,
        lv_rc    TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      file_filter             = 'Excel Files (*.xlsx;*.xls)|*.xlsx;*.xls'
    CHANGING
      file_table              = lt_files
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.
  IF sy-subrc = 0 AND lv_rc > 0.
    READ TABLE lt_files INTO ls_file INDEX 1.
    IF sy-subrc = 0.
      cv_file = ls_file-filename.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_READ_EXCEL
*&---------------------------------------------------------------------*
* Read the upload file. Fixed columns: A PAYER, B CUSTOMER CODE,
* C GA ID, D CGD ENTITY, E UPDATED DATE&TIME, F UOM, G.. gas days.
* Highlighted cells = GA ID / UOM / date columns. Max 30 date columns.
*----------------------------------------------------------------------*
FORM f_read_excel USING iv_file TYPE rlgrap-filename.
  CLEAR gt_cells[].
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = iv_file
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 36        "6 fixed + 30 gas day columns
      i_end_row               = 9999
    TABLES
      intern                  = gt_cells
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE 'Error reading the Excel file' TYPE 'E'.
  ENDIF.
  IF gt_cells[] IS INITIAL.
    MESSAGE 'Excel file is empty' TYPE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_PARSE_DATE
*&---------------------------------------------------------------------*
* Convert DD.MM.YYYY text to internal date. cv_ok = 'X' when valid.
*----------------------------------------------------------------------*
FORM f_parse_date USING iv_text TYPE clike
                  CHANGING cv_date TYPE datum
                           cv_ok   TYPE char1.
  DATA: lv_d(2)    TYPE c,
        lv_m(2)    TYPE c,
        lv_y(4)    TYPE c,
        lv_txt(50) TYPE c.

  CLEAR: cv_date, cv_ok.
  lv_txt = iv_text.
  CONDENSE lv_txt NO-GAPS.
  SPLIT lv_txt AT '.' INTO lv_d lv_m lv_y.
  IF lv_d CO '0123456789 ' AND lv_m CO '0123456789 '
     AND lv_y CO '0123456789 ' AND lv_y <> ''
     AND lv_d <> '' AND lv_m <> ''.
    UNPACK lv_d TO lv_d.
    UNPACK lv_m TO lv_m.
    CONCATENATE lv_y lv_m lv_d INTO cv_date.
    CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
      EXPORTING
        date                      = cv_date
      EXCEPTIONS
        plausibility_check_failed = 1
        OTHERS                    = 2.
    IF sy-subrc = 0.
      cv_ok = 'X'.
    ELSE.
      CLEAR cv_date.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_TO_QTY
*&---------------------------------------------------------------------*
FORM f_to_qty USING iv_text TYPE clike
              CHANGING cv_qty TYPE ygms_cgd_qty.
  DATA lv_txt(50) TYPE c.

  CLEAR cv_qty.
  lv_txt = iv_text.
  REPLACE ALL OCCURRENCES OF ',' IN lv_txt WITH ''.
  CONDENSE lv_txt NO-GAPS.
  IF lv_txt IS INITIAL.
    RETURN.
  ENDIF.
  TRY.
      cv_qty = lv_txt.
    CATCH cx_sy_conversion_no_number cx_sy_arithmetic_overflow.
      CLEAR cv_qty.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_FN_BOUNDS
*&---------------------------------------------------------------------*
* Fortnight of a date: 1st-15th or 16th-last day of month
*----------------------------------------------------------------------*
FORM f_fn_bounds USING iv_date TYPE datum
                 CHANGING cv_from TYPE datum
                          cv_to   TYPE datum.
  DATA lv_last TYPE datum.

  IF iv_date+6(2) <= '15'.
    CONCATENATE iv_date(6) '01' INTO cv_from.
    CONCATENATE iv_date(6) '15' INTO cv_to.
  ELSE.
    CONCATENATE iv_date(6) '16' INTO cv_from.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = iv_date
      IMPORTING
        last_day_of_month = lv_last
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
    cv_to = lv_last.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_PARSE_MATRIX
*&---------------------------------------------------------------------*
* Parse the excel matrix. iv_mode 'G' = Stage 1 (GA ID rows)
*                         iv_mode 'C' = Stage 2 (Customer rows)
* Fills gt_dates and gt_upl. Date format errors go to gt_err.
*----------------------------------------------------------------------*
FORM f_parse_matrix USING iv_mode TYPE char1.
  DATA: ls_cell    TYPE alsmex_tabline,
        ls_err     TYPE ty_err,
        lt_rows    TYPE TABLE OF alsmex_tabline,
        lv_date    TYPE datum,
        lv_ok      TYPE char1,
        lv_col     TYPE i,
        lv_idx     TYPE i,
        lv_gaid    TYPE ygms_cgd_gaid,
        lv_kunnr   TYPE kunnr,
        lv_uom     TYPE meins,
        lv_curow   TYPE i,
        lv_ctxt(50) TYPE c.

  CLEAR: gt_dates[], gt_upl[].

* ---- Header row: gas day columns start at column 7 -------------------
  LOOP AT gt_cells INTO ls_cell WHERE row = 1 AND col >= 7.
    lv_col = ls_cell-col.
    PERFORM f_parse_date USING ls_cell-value
                         CHANGING lv_date lv_ok.
    IF lv_ok = 'X'.
      lv_idx = lv_col - 6.
*     keep the date at its column position
      DO.
        IF lines( gt_dates ) >= lv_idx.
          EXIT.
        ENDIF.
        APPEND INITIAL LINE TO gt_dates.
      ENDDO.
      MODIFY gt_dates FROM lv_date INDEX lv_idx.
    ELSE.
      CLEAR ls_err.
      ls_err-message = |Invalid Gas Day format in header column|
                    && | { lv_col }: { ls_cell-value }|.
      APPEND ls_err TO gt_err.
    ENDIF.
  ENDLOOP.

  DELETE gt_dates WHERE table_line IS INITIAL.
  IF gt_dates[] IS INITIAL.
    CLEAR ls_err.
    ls_err-message = 'No valid Gas Day columns found in the file'.
    APPEND ls_err TO gt_err.
    RETURN.
  ENDIF.

* ---- Data rows -------------------------------------------------------
  lt_rows = gt_cells.
  SORT lt_rows BY row col.
  DELETE lt_rows WHERE row = 1.

  lv_curow = 0.
  LOOP AT lt_rows INTO ls_cell.
    IF ls_cell-row <> lv_curow.
*     flush previous row
      IF lv_curow > 0.
        PERFORM f_append_matrix_row USING iv_mode lv_gaid lv_kunnr
                                          lv_uom lv_curow.
      ENDIF.
      lv_curow = ls_cell-row.
      CLEAR: lv_gaid, lv_kunnr, lv_uom.
    ENDIF.
    CASE ls_cell-col.
      WHEN 2.  "Customer code
        IF iv_mode = 'C'.
          lv_ctxt = ls_cell-value.
          CONDENSE lv_ctxt NO-GAPS.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_ctxt
            IMPORTING
              output = lv_kunnr.
        ENDIF.
      WHEN 3.  "GA ID
        lv_gaid = ls_cell-value.
        CONDENSE lv_gaid NO-GAPS.
      WHEN 6.  "UoM
        lv_uom = ls_cell-value.
        CONDENSE lv_uom NO-GAPS.
        TRANSLATE lv_uom TO UPPER CASE.
    ENDCASE.
  ENDLOOP.
  IF lv_curow > 0.
    PERFORM f_append_matrix_row USING iv_mode lv_gaid lv_kunnr
                                      lv_uom lv_curow.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_APPEND_MATRIX_ROW
*&---------------------------------------------------------------------*
FORM f_append_matrix_row USING iv_mode  TYPE char1
                               iv_gaid  TYPE ygms_cgd_gaid
                               iv_kunnr TYPE kunnr
                               iv_uom   TYPE meins
                               iv_row   TYPE i.
  DATA: ls_upl    TYPE ty_upl,
        ls_cell   TYPE alsmex_tabline,
        lv_qty    TYPE ygms_cgd_qty,
        lv_col    TYPE i,
        lv_idx    TYPE i,
        lv_ndates TYPE i.

  IF ( iv_mode = 'G' AND iv_gaid IS INITIAL ) OR
     ( iv_mode = 'C' AND iv_kunnr IS INITIAL ).
    RETURN.  "empty row
  ENDIF.

  lv_ndates = lines( gt_dates ).
  DO lv_ndates TIMES.
    lv_idx = sy-index.
    lv_col = lv_idx + 6.
    CLEAR: lv_qty, ls_cell.
    READ TABLE gt_cells INTO ls_cell
         WITH KEY row = iv_row col = lv_col.
    IF sy-subrc = 0.
      PERFORM f_to_qty USING ls_cell-value CHANGING lv_qty.
    ENDIF.
    CLEAR ls_upl.
    READ TABLE gt_dates INTO ls_upl-gas_day INDEX lv_idx.
    ls_upl-upl_gaid = iv_gaid.
    ls_upl-kunnr    = iv_kunnr.
    ls_upl-uom      = iv_uom.
    ls_upl-qty      = lv_qty.
    APPEND ls_upl TO gt_upl.
  ENDDO.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CHECK_SINGLE_FN
*&---------------------------------------------------------------------*
FORM f_check_single_fn USING it_dates LIKE gt_dates
                       CHANGING cv_ok TYPE char1.
  DATA: lv_first TYPE datum,
        lv_date  TYPE datum,
        lv_from  TYPE datum,
        lv_to    TYPE datum,
        lv_from2 TYPE datum,
        lv_to2   TYPE datum.

  cv_ok = 'X'.
  READ TABLE it_dates INTO lv_first INDEX 1.
  IF sy-subrc <> 0.
    CLEAR cv_ok.
    RETURN.
  ENDIF.
  PERFORM f_fn_bounds USING lv_first CHANGING lv_from lv_to.
  LOOP AT it_dates INTO lv_date.
    PERFORM f_fn_bounds USING lv_date CHANGING lv_from2 lv_to2.
    IF lv_from2 <> lv_from OR lv_to2 <> lv_to.
      CLEAR cv_ok.
      RETURN.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_NEXT_SEQNO
*&---------------------------------------------------------------------*
FORM f_next_seqno CHANGING cv_seqno TYPE ygms_cgd_seqno.
  gv_seqno = gv_seqno + 1.
  cv_seqno = gv_seqno.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_POPUP_CONFIRM
*&---------------------------------------------------------------------*
FORM f_popup_confirm USING iv_question TYPE clike
                     CHANGING cv_answer TYPE char1.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'CGD Gas Scheduling'
      text_question         = iv_question
      text_button_1         = 'Yes'
      text_button_2         = 'No'
      default_button        = '2'
      display_cancel_button = ' '
    IMPORTING
      answer                = cv_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    cv_answer = '2'.
  ENDIF.
* answer '1' = Yes, '2' = No
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SHOW_ERR_POPUP
*&---------------------------------------------------------------------*
FORM f_show_err_popup USING it_tab TYPE STANDARD TABLE
                            iv_title TYPE clike.
  DATA: lo_salv  TYPE REF TO cl_salv_table,
        lv_title TYPE lvc_title.

  lv_title = iv_title.
  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_salv
        CHANGING  t_table      = it_tab ).
      lo_salv->get_columns( )->set_optimize( abap_true ).
      lo_salv->get_functions( )->set_all( abap_true ).
      lo_salv->get_display_settings( )->set_list_header( lv_title ).
      lo_salv->set_screen_popup( start_column = 5
                                 end_column   = 140
                                 start_line   = 3
                                 end_line     = 20 ).
      lo_salv->display( ).
    CATCH cx_salv_msg.
      MESSAGE 'Error displaying ALV' TYPE 'I'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SHOW_ALV
*&---------------------------------------------------------------------*
FORM f_show_alv USING it_tab TYPE STANDARD TABLE
                      iv_title TYPE clike.
  DATA: lo_salv  TYPE REF TO cl_salv_table,
        lv_title TYPE lvc_title.

  lv_title = iv_title.
  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_salv
        CHANGING  t_table      = it_tab ).
      lo_salv->get_columns( )->set_optimize( abap_true ).
      lo_salv->get_functions( )->set_all( abap_true ).
      lo_salv->get_display_settings( )->set_list_header( lv_title ).
      lo_salv->display( ).
    CATCH cx_salv_msg.
      MESSAGE 'Error displaying ALV' TYPE 'I'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_TEMPLATE_DOWNLOAD
*&---------------------------------------------------------------------*
* Tab delimited template in the agreed matrix format
*----------------------------------------------------------------------*
FORM f_template_download.
  DATA: lt_data TYPE TABLE OF string,
        lv_line TYPE string,
        lv_file TYPE string,
        lv_path TYPE string,
        lv_full TYPE string,
        lv_sep  TYPE c.

  lv_sep = cl_abap_char_utilities=>horizontal_tab.

  CONCATENATE 'PAYER' 'CUSTOMER CODE' 'GA ID' 'CGD ENTITY'
              'UPDATED DATE & TIME' 'UOM'
              'DD.MM.YYYY' 'DD.MM.YYYY' '...'
    INTO lv_line SEPARATED BY lv_sep.
  APPEND lv_line TO lt_data.
  CONCATENATE '' '' '' '' '' 'SM3' INTO lv_line SEPARATED BY lv_sep.
  APPEND lv_line TO lt_data.

  cl_gui_frontend_services=>file_save_dialog(
    EXPORTING
      default_file_name = 'CGD_Upload_Template.xls'
      default_extension = 'xls'
    CHANGING
      filename          = lv_file
      path              = lv_path
      fullpath          = lv_full
    EXCEPTIONS
      cntl_error        = 1
      error_no_gui      = 2
      OTHERS            = 3 ).
  IF sy-subrc <> 0 OR lv_full IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename = lv_full
      filetype = 'ASC'
    TABLES
      data_tab = lt_data
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc = 0.
    MESSAGE 'Template downloaded' TYPE 'S'.
  ELSE.
    MESSAGE 'Template download failed' TYPE 'I'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_DM_UPLOAD
*&---------------------------------------------------------------------*
* Stage 1 - GA ID wise Demanded Nomination : Upload File
*----------------------------------------------------------------------*
FORM f_dm_upload.
  DATA: ls_err    TYPE ty_err,
        ls_map    TYPE ty_gamap,
        lv_ok     TYPE char1,
        lv_answer TYPE char1,
        lt_dmd    TYPE TABLE OF yrxa_cgd_gaid_dmd,
        ls_dmd    TYPE yrxa_cgd_gaid_dmd,
        lt_log    TYPE TABLE OF yrxa_gaid_dmd_log,
        ls_log    TYPE yrxa_gaid_dmd_log,
        lt_exist  TYPE TABLE OF yrxa_cgd_gaid_dmd,
        lt_dup    TYPE TABLE OF ty_upl,
        ls_upl    TYPE ty_upl,
        lv_lines  TYPE i,
        lv_warn   TYPE bapi_msg.
  FIELD-SYMBOLS <ls_upl> TYPE ty_upl.

  IF p_file IS INITIAL.
    MESSAGE 'Please provide the file path' TYPE 'E'.
  ENDIF.

  CLEAR: gt_err[].
  PERFORM f_read_excel USING p_file.
  PERFORM f_parse_matrix USING 'G'.

* Gas days must belong to a single FN
  IF gt_dates[] IS NOT INITIAL.
    PERFORM f_check_single_fn USING gt_dates CHANGING lv_ok.
    IF lv_ok IS INITIAL.
      CLEAR ls_err.
      ls_err-message = 'Gas Days should belong to a single FN'.
      APPEND ls_err TO gt_err.
    ENDIF.
  ENDIF.

* Duplicate GA ID / Gas Day check
  lt_dup = gt_upl.
  SORT lt_dup BY upl_gaid gas_day.
  lv_lines = lines( lt_dup ).
  DELETE ADJACENT DUPLICATES FROM lt_dup COMPARING upl_gaid gas_day.
  IF lines( lt_dup ) <> lv_lines.
    CLEAR ls_err.
    ls_err-message =
      'Duplicate GA ID / Gas Day combination found in the excel file'.
    APPEND ls_err TO gt_err.
  ENDIF.

* Row level validations
  LOOP AT gt_upl ASSIGNING <ls_upl>.
*   GA ID must exist in T176 (via T176T text)
    READ TABLE gt_gamap INTO ls_map
         WITH TABLE KEY upl_gaid = <ls_upl>-upl_gaid.
    IF sy-subrc <> 0.
      CLEAR ls_err.
      ls_err-gas_day  = <ls_upl>-gas_day.
      ls_err-upl_gaid = <ls_upl>-upl_gaid.
      ls_err-message  = |GA ID { <ls_upl>-upl_gaid } doesn't exists|.
      APPEND ls_err TO gt_err.
    ELSE.
      <ls_upl>-bsark = ls_map-bsark.
      <ls_upl>-vtext = ls_map-vtext.
    ENDIF.
*   UoM check
    IF <ls_upl>-uom <> 'SM3'.
      CLEAR ls_err.
      ls_err-gas_day  = <ls_upl>-gas_day.
      ls_err-upl_gaid = <ls_upl>-upl_gaid.
      ls_err-message  =
        |UoM can be 'SM3' only for GA ID { <ls_upl>-upl_gaid }|.
      APPEND ls_err TO gt_err.
    ENDIF.
  ENDLOOP.

  SORT gt_err BY gas_day upl_gaid message.
  DELETE ADJACENT DUPLICATES FROM gt_err COMPARING ALL FIELDS.

* On error: show ALV, log, stop
  IF gt_err[] IS NOT INITIAL.
    LOOP AT gt_err INTO ls_err.
      CLEAR ls_log.
      ls_log-mandt    = sy-mandt.
      ls_log-gas_day  = ls_err-gas_day.
      ls_log-upl_gaid = ls_err-upl_gaid.
      ls_log-erdat    = sy-datum.
      ls_log-erzet    = sy-uzeit.
      PERFORM f_next_seqno CHANGING ls_log-seqno.
      ls_log-message  = ls_err-message.
      ls_log-ernam    = sy-uname.
      APPEND ls_log TO lt_log.
    ENDLOOP.
    MODIFY yrxa_gaid_dmd_log FROM TABLE lt_log.
    COMMIT WORK.
    PERFORM f_show_err_popup USING gt_err 'Upload Errors'.
    RETURN.
  ENDIF.

* Existing data check (re-upload)
  IF gt_upl[] IS NOT INITIAL.
    SELECT * FROM yrxa_cgd_gaid_dmd
      INTO TABLE lt_exist
      FOR ALL ENTRIES IN gt_upl
      WHERE gas_day  = gt_upl-gas_day
        AND sap_gaid = gt_upl-bsark.
  ENDIF.

  IF lt_exist[] IS NOT INITIAL.
    PERFORM f_show_err_popup USING lt_exist 'Existing Data'.
    PERFORM f_popup_confirm
      USING 'Data exists for some of the GA ID/ Gas Day combination. Do you want to proceed?'
      CHANGING lv_answer.
    IF lv_answer = '1'.
      lv_warn =
   'Data exists for some of the GA ID/ Gas Day combination - Yes'.
    ELSE.
      lv_warn =
   'Data exists for some of the GA ID/ Gas Day combination - No'.
    ENDIF.
*   log the warning + decision
    LOOP AT gt_upl INTO ls_upl.
      CLEAR ls_log.
      ls_log-mandt     = sy-mandt.
      ls_log-gas_day   = ls_upl-gas_day.
      ls_log-upl_gaid  = ls_upl-upl_gaid.
      ls_log-erdat     = sy-datum.
      ls_log-erzet     = sy-uzeit.
      PERFORM f_next_seqno CHANGING ls_log-seqno.
      ls_log-sap_gaid  = ls_upl-bsark.
      ls_log-gaid_desc = ls_upl-vtext.
      ls_log-dmd_qty   = ls_upl-qty.
      ls_log-uom       = ls_upl-uom.
      ls_log-message   = lv_warn.
      ls_log-ernam     = sy-uname.
      APPEND ls_log TO lt_log.
    ENDLOOP.
    MODIFY yrxa_gaid_dmd_log FROM TABLE lt_log.
    COMMIT WORK.
    IF lv_answer <> '1'.
      MESSAGE 'Upload cancelled by user' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.
    CLEAR lt_log[].
  ENDIF.

* Stamp data
  LOOP AT gt_upl INTO ls_upl.
    CLEAR ls_dmd.
    ls_dmd-mandt     = sy-mandt.
    ls_dmd-gas_day   = ls_upl-gas_day.
    ls_dmd-sap_gaid  = ls_upl-bsark.
    ls_dmd-gaid_desc = ls_upl-vtext.
    ls_dmd-upl_gaid  = ls_upl-upl_gaid.
    ls_dmd-dmd_qty   = ls_upl-qty.
    ls_dmd-uom       = ls_upl-uom.
    ls_dmd-ernam     = sy-uname.
    ls_dmd-erdat     = sy-datum.
    ls_dmd-erzet     = sy-uzeit.
    APPEND ls_dmd TO lt_dmd.

    CLEAR ls_log.
    MOVE-CORRESPONDING ls_dmd TO ls_log.
    PERFORM f_next_seqno CHANGING ls_log-seqno.
    ls_log-message = 'Success'.
    APPEND ls_log TO lt_log.
  ENDLOOP.

  MODIFY yrxa_cgd_gaid_dmd FROM TABLE lt_dmd.
  MODIFY yrxa_gaid_dmd_log FROM TABLE lt_log.
  COMMIT WORK.
  MESSAGE 'Successfully uploaded' TYPE 'S'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_DM_CALCULATE
*&---------------------------------------------------------------------*
* Stage 1 - Calculate demanded schedule per GA ID for one Gas Day
*----------------------------------------------------------------------*
FORM f_dm_calculate.
  DATA: lt_bsark  TYPE RANGE OF bsark,
        ls_rng    LIKE LINE OF lt_bsark,
        lt_map    LIKE gt_gamap,
        ls_map    TYPE ty_gamap,
        lt_kna1   TYPE TABLE OF ty_kna1,
        ls_kna1   TYPE ty_kna1,
        lt_cont   TYPE TABLE OF ty_cont,
        ls_cont   TYPE ty_cont,
        lt_vbap   TYPE TABLE OF ty_vbap,
        ls_vbap   TYPE ty_vbap,
        lt_dcq    TYPE TABLE OF yrvt_cont_dcq,
        ls_dcq    TYPE yrvt_cont_dcq,
        lt_agg    TYPE TABLE OF ty_cdcq,
        ls_cdcq   TYPE ty_cdcq,
        ls_alv    TYPE ty_alv1,
        ls_err2   TYPE ty_err2,
        lv_answer TYPE char1.
  FIELD-SYMBOLS <ls_c> TYPE ty_cdcq.

  IF p_gday IS INITIAL.
    MESSAGE 'Gas Day is mandatory' TYPE 'E'.
  ENDIF.

  CLEAR: gt_err2[], gt_alv1[], gt_cdcq[].

* GA ID selection -> BSARK set
  lt_map = gt_gamap.
  IF s_gaid[] IS NOT INITIAL.
    DELETE lt_map WHERE upl_gaid NOT IN s_gaid.
    IF lt_map[] IS INITIAL.
      MESSAGE 'No GA ID found for the given selection' TYPE 'E'.
    ENDIF.
  ENDIF.
  LOOP AT lt_map INTO ls_map.
    CLEAR ls_rng.
    ls_rng-sign   = 'I'.
    ls_rng-option = 'EQ'.
    ls_rng-low    = ls_map-bsark.
    APPEND ls_rng TO lt_bsark.
  ENDLOOP.

* Customers of the GA IDs
  SELECT kunnr yygaid FROM kna1
    INTO TABLE lt_kna1
    WHERE yygaid IN lt_bsark
      AND yygaid <> space.
  IF lt_kna1[] IS INITIAL.
    MESSAGE 'No customer found for the selected GA ID(s)' TYPE 'E'.
  ENDIF.

* Valid CGD POOL contracts of these customers on the gas day
  SELECT a~vbeln a~kunnr
    INTO TABLE lt_cont
    FROM vbak AS a INNER JOIN veda AS b
      ON b~vbeln = a~vbeln
    FOR ALL ENTRIES IN lt_kna1
    WHERE a~kunnr   = lt_kna1-kunnr
      AND a~augru   = gv_augru
      AND b~vposn   = '000000'
      AND b~vbegdat <= p_gday
      AND b~venddat >= p_gday.
  IF lt_cont[] IS INITIAL.
    MESSAGE 'No valid CGD POOL contract found for the Gas Day' TYPE 'E'.
  ENDIF.

* Target UoM check of item 0010
  SELECT vbeln posnr zieme FROM vbap
    INTO TABLE lt_vbap
    FOR ALL ENTRIES IN lt_cont
    WHERE vbeln = lt_cont-vbeln
      AND posnr = '000010'.
  LOOP AT lt_vbap INTO ls_vbap WHERE zieme <> 'SM3'.
    CLEAR ls_err2.
    ls_err2-contract = ls_vbap-vbeln.
    ls_err2-gas_day  = p_gday.
    ls_err2-message  = |Contract { ls_vbap-vbeln } has Target UoM as|
                    && | { ls_vbap-zieme }. Only SM3 is allowed|.
    APPEND ls_err2 TO gt_err2.
  ENDLOOP.
  IF gt_err2[] IS NOT INITIAL.
    PERFORM f_show_err_popup USING gt_err2 'UoM Errors'.
    RETURN.
  ENDIF.

* Latest DCQ per contract for the gas day
  SELECT * FROM yrvt_cont_dcq
    INTO TABLE lt_dcq
    FOR ALL ENTRIES IN lt_cont
    WHERE vbeln = lt_cont-vbeln
      AND ydate = p_gday.
  SORT lt_dcq BY vbeln ASCENDING creon DESCENDING time DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_dcq COMPARING vbeln.

* Build contract level table
  SORT lt_kna1 BY kunnr.
  SORT lt_dcq BY vbeln.
  LOOP AT lt_cont INTO ls_cont.
    CLEAR ls_cdcq.
    ls_cdcq-contract = ls_cont-vbeln.
    ls_cdcq-kunnr    = ls_cont-kunnr.
    ls_cdcq-gas_day  = p_gday.
    READ TABLE lt_kna1 INTO ls_kna1
         WITH KEY kunnr = ls_cont-kunnr BINARY SEARCH.
    IF sy-subrc = 0.
      ls_cdcq-sap_gaid = ls_kna1-yygaid.
    ENDIF.
    READ TABLE lt_map INTO ls_map WITH KEY bsark = ls_cdcq-sap_gaid.
    IF sy-subrc = 0.
      ls_cdcq-upl_gaid  = ls_map-upl_gaid.
      ls_cdcq-gaid_desc = ls_map-vtext.
    ENDIF.
    READ TABLE lt_dcq INTO ls_dcq
         WITH KEY vbeln = ls_cont-vbeln BINARY SEARCH.
    IF sy-subrc = 0.
      ls_cdcq-dcq     = ls_dcq-dcq_qty.
*     MAX_QTY / MIN_QTY are percentages of DCQ (YZPERCENT)
      ls_cdcq-dcq_max = ls_dcq-dcq_qty * ls_dcq-max_qty / 100.
      ls_cdcq-dcq_min = ls_dcq-dcq_qty * ls_dcq-min_qty / 100.
    ENDIF.
    APPEND ls_cdcq TO gt_cdcq.
  ENDLOOP.

* Aggregate per GA ID and apply demand clamping
  lt_agg = gt_cdcq.
  SORT lt_agg BY sap_gaid.
  LOOP AT lt_agg INTO ls_cdcq
       GROUP BY ( sap_gaid = ls_cdcq-sap_gaid ) INTO DATA(lo_grp).
    CLEAR ls_alv.
    ls_alv-sap_gaid = lo_grp-sap_gaid.
    ls_alv-gas_day  = p_gday.
    LOOP AT GROUP lo_grp INTO DATA(ls_member).
      ls_alv-sum_dcq   = ls_alv-sum_dcq + ls_member-dcq.
      ls_alv-sum_max   = ls_alv-sum_max + ls_member-dcq_max.
      ls_alv-sum_min   = ls_alv-sum_min + ls_member-dcq_min.
      ls_alv-upl_gaid  = ls_member-upl_gaid.
      ls_alv-gaid_desc = ls_member-gaid_desc.
    ENDLOOP.
*   Uploaded demand of the GA ID
    SELECT SINGLE upl_gaid gaid_desc dmd_qty
      FROM yrxa_cgd_gaid_dmd
      INTO (ls_alv-upl_gaid, ls_alv-gaid_desc, ls_alv-upl_qty)
      WHERE gas_day  = p_gday
        AND sap_gaid = lo_grp-sap_gaid.
    IF sy-subrc = 0.
      IF ls_alv-upl_qty > ls_alv-sum_max.
        ls_alv-cal_qty = ls_alv-sum_max.
      ELSEIF ls_alv-upl_qty < ls_alv-sum_min.
        ls_alv-cal_qty = ls_alv-sum_min.
      ELSE.
        ls_alv-cal_qty = ls_alv-upl_qty.
      ENDIF.
    ELSE.
*     no excel value -> sum of DCQ
      ls_alv-cal_qty = ls_alv-sum_dcq.
    ENDIF.
    APPEND ls_alv TO gt_alv1.

*   carry calculated GA level qty to the contract lines
    LOOP AT gt_cdcq ASSIGNING <ls_c>
         WHERE sap_gaid = lo_grp-sap_gaid.
      <ls_c>-cal_qty = ls_alv-cal_qty.
    ENDLOOP.
  ENDLOOP.

  PERFORM f_show_alv USING gt_alv1
    'GA ID wise Calculated Demanded Schedule'.

  PERFORM f_popup_confirm
    USING 'Save the calculated data?'
    CHANGING lv_answer.
  IF lv_answer = '1'.
    PERFORM f_dm_save.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_DM_SAVE
*&---------------------------------------------------------------------*
FORM f_dm_save.
  DATA: lt_cal    TYPE TABLE OF yrxa_gaid_cal_dmd,
        ls_cal    TYPE yrxa_gaid_cal_dmd,
        lt_exist  TYPE TABLE OF yrxa_gaid_cal_dmd,
        lt_old    TYPE TABLE OF yrxa_gaid_dcq,
        ls_old    TYPE yrxa_gaid_dcq,
        ls_dcqdb  TYPE yrxa_gaid_dcq,
        ls_alv    TYPE ty_alv1,
        ls_cdcq   TYPE ty_cdcq,
        lv_answer TYPE char1.

* Existing data check
  IF gt_alv1[] IS NOT INITIAL.
    SELECT * FROM yrxa_gaid_cal_dmd
      INTO TABLE lt_exist
      FOR ALL ENTRIES IN gt_alv1
      WHERE gas_day  = gt_alv1-gas_day
        AND sap_gaid = gt_alv1-sap_gaid.
    IF lt_exist[] IS NOT INITIAL.
      PERFORM f_popup_confirm
        USING 'Data exists for some of the GA ID/ Gas Day combination. Do you want to proceed?'
        CHANGING lv_answer.
      IF lv_answer <> '1'.
        MESSAGE 'Save cancelled by user' TYPE 'S' DISPLAY LIKE 'W'.
        RETURN.
      ENDIF.
    ENDIF.
  ENDIF.

* Append calculated GA ID level data
  LOOP AT gt_alv1 INTO ls_alv.
    CLEAR ls_cal.
    ls_cal-mandt       = sy-mandt.
    ls_cal-gas_day     = ls_alv-gas_day.
    ls_cal-sap_gaid    = ls_alv-sap_gaid.
    ls_cal-erdat       = sy-datum.
    ls_cal-erzet       = sy-uzeit.
    PERFORM f_next_seqno CHANGING ls_cal-seqno.
    ls_cal-gaid_desc   = ls_alv-gaid_desc.
    ls_cal-upl_gaid    = ls_alv-upl_gaid.
    ls_cal-upl_dmd_qty = ls_alv-upl_qty.
    ls_cal-cal_dmd_qty = ls_alv-cal_qty.
    ls_cal-dcq_qty     = ls_alv-sum_dcq.
    ls_cal-dcq_max     = ls_alv-sum_max.
    ls_cal-dcq_min     = ls_alv-sum_min.
    ls_cal-uom         = 'SM3'.
    ls_cal-ernam       = sy-uname.
    APPEND ls_cal TO lt_cal.
  ENDLOOP.
  INSERT yrxa_gaid_cal_dmd FROM TABLE lt_cal ACCEPTING DUPLICATE KEYS.

* Upsert contract level data (key: Contract / Gas Day)
  IF gt_cdcq[] IS NOT INITIAL.
    SELECT * FROM yrxa_gaid_dcq
      INTO TABLE lt_old
      FOR ALL ENTRIES IN gt_cdcq
      WHERE contract = gt_cdcq-contract
        AND gas_day  = gt_cdcq-gas_day.
    SORT lt_old BY contract gas_day.
  ENDIF.
  LOOP AT gt_cdcq INTO ls_cdcq.
    CLEAR ls_dcqdb.
    READ TABLE lt_old INTO ls_old
         WITH KEY contract = ls_cdcq-contract
                  gas_day  = ls_cdcq-gas_day BINARY SEARCH.
    IF sy-subrc = 0.
      ls_dcqdb = ls_old.
      ls_dcqdb-aenam = sy-uname.
      ls_dcqdb-aedat = sy-datum.
      ls_dcqdb-aezet = sy-uzeit.
    ELSE.
      ls_dcqdb-mandt    = sy-mandt.
      ls_dcqdb-contract = ls_cdcq-contract.
      ls_dcqdb-gas_day  = ls_cdcq-gas_day.
      ls_dcqdb-ernam    = sy-uname.
      ls_dcqdb-erdat    = sy-datum.
      ls_dcqdb-erzet    = sy-uzeit.
    ENDIF.
    ls_dcqdb-kunnr       = ls_cdcq-kunnr.
    ls_dcqdb-sap_gaid    = ls_cdcq-sap_gaid.
    ls_dcqdb-gaid_desc   = ls_cdcq-gaid_desc.
    ls_dcqdb-upl_gaid    = ls_cdcq-upl_gaid.
    ls_dcqdb-cal_dmd_qty = ls_cdcq-cal_qty.
    ls_dcqdb-dcq_qty     = ls_cdcq-dcq.
    ls_dcqdb-dcq_max     = ls_cdcq-dcq_max.
    ls_dcqdb-dcq_min     = ls_cdcq-dcq_min.
    ls_dcqdb-uom         = 'SM3'.
    MODIFY yrxa_gaid_dcq FROM ls_dcqdb.
  ENDLOOP.

  COMMIT WORK.
  MESSAGE 'Calculated data saved successfully' TYPE 'S'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_DM_DOWNLOAD
*&---------------------------------------------------------------------*
* Stage 1 - Download the latest calculated demanded schedule (matrix)
*----------------------------------------------------------------------*
FORM f_dm_download.
  DATA: lt_cal    TYPE TABLE OF yrxa_gaid_cal_dmd,
        ls_cal    TYPE yrxa_gaid_cal_dmd,
        lt_days   LIKE gt_dates,
        lt_matrix TYPE TABLE OF ty_matrix,
        ls_matrix TYPE ty_matrix,
        lv_idx    TYPE i,
        lv_comp   TYPE string,
        lv_ok     TYPE char1,
        lv_answer TYPE char1.
  FIELD-SYMBOLS: <ls_mx>  TYPE ty_matrix,
                 <lv_qty> TYPE ygms_cgd_qty.

  IF s_gday[] IS INITIAL.
    MESSAGE 'Gas Day is mandatory' TYPE 'E'.
  ENDIF.

  SELECT * FROM yrxa_gaid_cal_dmd
    INTO TABLE lt_cal
    WHERE gas_day  IN s_gday
      AND upl_gaid IN s_gaid2.
  IF lt_cal[] IS INITIAL.
    MESSAGE 'No data found for the selection' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

* Latest version per Gas Day / GA ID
  SORT lt_cal BY gas_day sap_gaid erdat DESCENDING erzet DESCENDING
                 seqno DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_cal COMPARING gas_day sap_gaid.

* Distinct gas days -> must lie in one FN, max 30
  LOOP AT lt_cal INTO ls_cal.
    APPEND ls_cal-gas_day TO lt_days.
  ENDLOOP.
  SORT lt_days.
  DELETE ADJACENT DUPLICATES FROM lt_days.
  PERFORM f_check_single_fn USING lt_days CHANGING lv_ok.
  IF lv_ok IS INITIAL.
    MESSAGE 'Gas Days should belong to a single FN' TYPE 'E'.
  ENDIF.
  IF lines( lt_days ) > 30.
    MESSAGE 'Maximum 30 Gas Days supported' TYPE 'E'.
  ENDIF.

* Pivot into the matrix
  SORT lt_cal BY upl_gaid gas_day.
  LOOP AT lt_cal INTO ls_cal.
    READ TABLE lt_matrix ASSIGNING <ls_mx>
         WITH KEY upl_gaid = ls_cal-upl_gaid.
    IF sy-subrc <> 0.
      CLEAR ls_matrix.
      ls_matrix-upl_gaid = ls_cal-upl_gaid.
      APPEND ls_matrix TO lt_matrix.
      READ TABLE lt_matrix ASSIGNING <ls_mx>
           WITH KEY upl_gaid = ls_cal-upl_gaid.
    ENDIF.
    READ TABLE lt_days TRANSPORTING NO FIELDS
         WITH KEY table_line = ls_cal-gas_day.
    IF sy-subrc = 0.
      lv_idx  = sy-tabix.
      lv_comp = |D{ lv_idx WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
      ASSIGN COMPONENT lv_comp OF STRUCTURE <ls_mx> TO <lv_qty>.
      IF sy-subrc = 0.
        <lv_qty> = ls_cal-cal_dmd_qty.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Show ALV with gas day column headers
  PERFORM f_show_matrix_alv USING lt_matrix lt_days.

* Download
  PERFORM f_popup_confirm USING 'Download the data to a file?'
                          CHANGING lv_answer.
  IF lv_answer = '1'.
    PERFORM f_download_matrix USING lt_matrix lt_days.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SHOW_MATRIX_ALV
*&---------------------------------------------------------------------*
FORM f_show_matrix_alv USING it_matrix TYPE STANDARD TABLE
                             it_days   LIKE gt_dates.
  DATA: lo_salv  TYPE REF TO cl_salv_table,
        lo_cols  TYPE REF TO cl_salv_columns_table,
        lo_col   TYPE REF TO cl_salv_column,
        lv_idx   TYPE i,
        lv_comp  TYPE lvc_fname,
        lv_date  TYPE datum,
        lv_txt_s TYPE scrtext_s,
        lv_txt_m TYPE scrtext_m,
        lv_txt_l TYPE scrtext_l.

  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_salv
        CHANGING  t_table      = it_matrix ).
      lo_cols = lo_salv->get_columns( ).
      lo_cols->set_optimize( abap_true ).
      lo_salv->get_functions( )->set_all( abap_true ).
      DO 30 TIMES.
        lv_idx  = sy-index.
        lv_comp = |D{ lv_idx WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
        lo_col  = lo_cols->get_column( lv_comp ).
        READ TABLE it_days INTO lv_date INDEX lv_idx.
        IF sy-subrc = 0.
          lv_txt_l = |{ lv_date+6(2) }.{ lv_date+4(2) }.{ lv_date(4) }|.
          lv_txt_s = lv_txt_l.
          lv_txt_m = lv_txt_l.
          lo_col->set_short_text( lv_txt_s ).
          lo_col->set_medium_text( lv_txt_m ).
          lo_col->set_long_text( lv_txt_l ).
        ELSE.
          lo_col->set_technical( abap_true ).
        ENDIF.
      ENDDO.
      lo_salv->get_display_settings( )->set_list_header(
        'CGD Pool SQ (SM3) - Latest Calculated Demanded Schedule' ).
      lo_salv->display( ).
    CATCH cx_salv_msg cx_salv_not_found.
      MESSAGE 'Error displaying ALV' TYPE 'I'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_MATRIX
*&---------------------------------------------------------------------*
FORM f_download_matrix USING it_matrix TYPE STANDARD TABLE
                             it_days   LIKE gt_dates.
  DATA: lt_data  TYPE TABLE OF string,
        lv_line  TYPE string,
        lv_cell  TYPE string,
        lv_file  TYPE string,
        lv_path  TYPE string,
        lv_full  TYPE string,
        lv_date  TYPE datum,
        lv_comp  TYPE string,
        lv_ndays TYPE i,
        lv_sep   TYPE c.
  FIELD-SYMBOLS: <ls_row> TYPE ty_matrix,
                 <ls_any> TYPE any,
                 <lv_qty> TYPE ygms_cgd_qty.

  lv_sep = cl_abap_char_utilities=>horizontal_tab.

* Header row 1: GA ID + dates
  lv_line = 'GA ID'.
  LOOP AT it_days INTO lv_date.
    lv_cell = |{ lv_date+6(2) }-{ lv_date+4(2) }-{ lv_date(4) }|.
    CONCATENATE lv_line lv_cell INTO lv_line SEPARATED BY lv_sep.
  ENDLOOP.
  APPEND lv_line TO lt_data.
* Header row 2: CGD Pool SQ
  CLEAR lv_line.
  LOOP AT it_days INTO lv_date.
    CONCATENATE lv_line 'CGD Pool SQ' INTO lv_line SEPARATED BY lv_sep.
  ENDLOOP.
  APPEND lv_line TO lt_data.
* Header row 3: UoM
  CLEAR lv_line.
  LOOP AT it_days INTO lv_date.
    CONCATENATE lv_line 'SM3' INTO lv_line SEPARATED BY lv_sep.
  ENDLOOP.
  APPEND lv_line TO lt_data.

* Data rows
  lv_ndays = lines( it_days ).
  LOOP AT it_matrix ASSIGNING <ls_any>.
    ASSIGN <ls_any> TO <ls_row> CASTING.
    lv_line = <ls_row>-upl_gaid.
    DO lv_ndays TIMES.
      lv_comp = |D{ sy-index WIDTH = 2 PAD = '0' ALIGN = RIGHT }|.
      ASSIGN COMPONENT lv_comp OF STRUCTURE <ls_row> TO <lv_qty>.
      IF sy-subrc = 0.
        lv_cell = |{ <lv_qty> }|.
        CONDENSE lv_cell NO-GAPS.
      ELSE.
        CLEAR lv_cell.
      ENDIF.
      CONCATENATE lv_line lv_cell INTO lv_line SEPARATED BY lv_sep.
    ENDDO.
    APPEND lv_line TO lt_data.
  ENDLOOP.

  cl_gui_frontend_services=>file_save_dialog(
    EXPORTING
      default_file_name = 'CGD_Demanded_Schedule.xls'
      default_extension = 'xls'
    CHANGING
      filename          = lv_file
      path              = lv_path
      fullpath          = lv_full
    EXCEPTIONS
      cntl_error        = 1
      error_no_gui      = 2
      OTHERS            = 3 ).
  IF sy-subrc <> 0 OR lv_full IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename = lv_full
      filetype = 'ASC'
    TABLES
      data_tab = lt_data
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc = 0.
    MESSAGE 'File downloaded successfully' TYPE 'S'.
  ELSE.
    MESSAGE 'File download failed' TYPE 'I'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CF_UPLOAD
*&---------------------------------------------------------------------*
* Stage 2 - Contract wise Confirmed Qty : Upload File (customer matrix)
*----------------------------------------------------------------------*
FORM f_cf_upload.
  DATA: ls_err     TYPE ty_err,
        ls_map     TYPE ty_gamap,
        lv_answer  TYPE char1,
        lt_cnf     TYPE TABLE OF yrxa_cgd_cnf_qty,
        ls_cnf     TYPE yrxa_cgd_cnf_qty,
        lt_log     TYPE TABLE OF yrxa_cnf_qty_log,
        ls_log     TYPE yrxa_cnf_qty_log,
        lt_cust    TYPE TABLE OF ty_upl,
        lt_kna1    TYPE TABLE OF ty_kna1,
        ls_kna1    TYPE ty_kna1,
        lt_gaids   TYPE TABLE OF ty_upl,
        lt_allcust TYPE TABLE OF ty_kna1,
        ls_ac      TYPE ty_kna1,
        lt_bygaid  TYPE TABLE OF ty_upl,
        lt_infile  TYPE TABLE OF kunnr,
        lt_cald    TYPE TABLE OF yrxa_gaid_cal_dmd,
        ls_cald    TYPE yrxa_gaid_cal_dmd,
        lt_contr   TYPE TABLE OF yrxa_gaid_dcq,
        lt_nomi    TYPE TABLE OF ty_nomi,
        ls_upl     TYPE ty_upl,
        lv_other   TYPE ygms_cgd_qty,
        lv_total   TYPE ygms_cgd_qty,
        lv_limit   TYPE ygms_cgd_qty,
        lv_nomfnd  TYPE char1,
        lv_warn    TYPE bapi_msg.
  FIELD-SYMBOLS <ls_upl> TYPE ty_upl.

  IF p_file IS INITIAL.
    MESSAGE 'Please provide the file path' TYPE 'E'.
  ENDIF.

  CLEAR: gt_err[].
  PERFORM f_read_excel USING p_file.
  PERFORM f_parse_matrix USING 'C'.

  IF gt_upl[] IS INITIAL AND gt_err[] IS INITIAL.
    MESSAGE 'No data rows found in the file' TYPE 'E'.
  ENDIF.

* Customer validation
  lt_cust = gt_upl.
  SORT lt_cust BY kunnr.
  DELETE ADJACENT DUPLICATES FROM lt_cust COMPARING kunnr.
  IF lt_cust[] IS NOT INITIAL.
    SELECT kunnr yygaid FROM kna1
      INTO TABLE lt_kna1
      FOR ALL ENTRIES IN lt_cust
      WHERE kunnr = lt_cust-kunnr.
    SORT lt_kna1 BY kunnr.
  ENDIF.

  LOOP AT gt_upl ASSIGNING <ls_upl>.
    READ TABLE lt_kna1 INTO ls_kna1
         WITH KEY kunnr = <ls_upl>-kunnr BINARY SEARCH.
    IF sy-subrc <> 0.
      CLEAR ls_err.
      ls_err-gas_day = <ls_upl>-gas_day.
      ls_err-kunnr   = <ls_upl>-kunnr.
      ls_err-message = |Customer { <ls_upl>-kunnr } is not valid|.
      APPEND ls_err TO gt_err.
    ELSEIF ls_kna1-yygaid IS INITIAL.
      CLEAR ls_err.
      ls_err-gas_day = <ls_upl>-gas_day.
      ls_err-kunnr   = <ls_upl>-kunnr.
      ls_err-message =
        |Customer { <ls_upl>-kunnr } is not mapped to any GA ID|.
      APPEND ls_err TO gt_err.
    ELSE.
      <ls_upl>-bsark = ls_kna1-yygaid.
      READ TABLE gt_gamap INTO ls_map
           WITH KEY bsark = ls_kna1-yygaid.
      IF sy-subrc = 0.
        <ls_upl>-upl_gaid = ls_map-upl_gaid.
        <ls_upl>-vtext    = ls_map-vtext.
      ENDIF.
    ENDIF.
  ENDLOOP.

* GA ID wise sum check against calculated demanded qty
  IF gt_err[] IS INITIAL.
*   all customers of the involved GA IDs
    lt_gaids = gt_upl.
    SORT lt_gaids BY bsark.
    DELETE ADJACENT DUPLICATES FROM lt_gaids COMPARING bsark.
    IF lt_gaids[] IS NOT INITIAL.
      SELECT kunnr yygaid FROM kna1
        INTO TABLE lt_allcust
        FOR ALL ENTRIES IN lt_gaids
        WHERE yygaid = lt_gaids-bsark.
      SORT lt_allcust BY yygaid kunnr.
    ENDIF.

    lt_bygaid = gt_upl.
    SORT lt_bygaid BY gas_day bsark kunnr.
    LOOP AT lt_bygaid INTO DATA(ls_first)
         GROUP BY ( gas_day = ls_first-gas_day
                    bsark   = ls_first-bsark ) INTO DATA(lo_g).
      CLEAR: lv_total, lv_limit, lt_infile[].
      LOOP AT GROUP lo_g INTO DATA(ls_m).
        lv_total = lv_total + ls_m-qty.
        APPEND ls_m-kunnr TO lt_infile.
      ENDLOOP.
      SORT lt_infile.
*     confirmed qty of the remaining customers of the GA ID
      LOOP AT lt_allcust INTO ls_ac WHERE yygaid = lo_g-bsark.
        READ TABLE lt_infile TRANSPORTING NO FIELDS
             WITH KEY table_line = ls_ac-kunnr BINARY SEARCH.
        IF sy-subrc <> 0.
          CLEAR lv_other.
          SELECT SINGLE cnf_qty FROM yrxa_cgd_cnf_qty
            INTO lv_other
            WHERE gas_day = lo_g-gas_day
              AND kunnr   = ls_ac-kunnr.
          IF sy-subrc = 0.
            lv_total = lv_total + lv_other.
          ENDIF.
        ENDIF.
      ENDLOOP.
*     latest calculated demanded qty of the GA ID
      CLEAR lt_cald[].
      SELECT * FROM yrxa_gaid_cal_dmd
        INTO TABLE lt_cald
        WHERE gas_day  = lo_g-gas_day
          AND sap_gaid = lo_g-bsark.
      SORT lt_cald BY erdat DESCENDING erzet DESCENDING
                      seqno DESCENDING.
      READ TABLE lt_cald INTO ls_cald INDEX 1.
      IF sy-subrc = 0.
        lv_limit = ls_cald-cal_dmd_qty.
      ENDIF.
      IF lv_total > lv_limit.
        READ TABLE gt_gamap INTO ls_map WITH KEY bsark = lo_g-bsark.
        CLEAR ls_err.
        ls_err-gas_day  = lo_g-gas_day.
        ls_err-upl_gaid = ls_map-upl_gaid.
        ls_err-message  = |Uploaded qty is more than the calculated|
          && | demanded qty for GAID { ls_map-upl_gaid }|
          && | { ls_map-vtext }|.
        APPEND ls_err TO gt_err.
      ENDIF.
    ENDLOOP.
  ENDIF.

* On error: show, log, stop
  IF gt_err[] IS NOT INITIAL.
    LOOP AT gt_err INTO ls_err.
      CLEAR ls_log.
      ls_log-mandt   = sy-mandt.
      ls_log-gas_day = ls_err-gas_day.
      ls_log-kunnr   = ls_err-kunnr.
      ls_log-erdat   = sy-datum.
      ls_log-erzet   = sy-uzeit.
      PERFORM f_next_seqno CHANGING ls_log-seqno.
      ls_log-message = ls_err-message.
      ls_log-ernam   = sy-uname.
      APPEND ls_log TO lt_log.
    ENDLOOP.
    MODIFY yrxa_cnf_qty_log FROM TABLE lt_log.
    COMMIT WORK.
    PERFORM f_show_err_popup USING gt_err 'Upload Errors'.
    RETURN.
  ENDIF.

* Existing nomination check via YRXA_GAID_DCQ contracts -> OIJNOMI
  IF gt_upl[] IS NOT INITIAL.
    SELECT * FROM yrxa_gaid_dcq
      INTO TABLE lt_contr
      FOR ALL ENTRIES IN gt_upl
      WHERE kunnr   = gt_upl-kunnr
        AND gas_day = gt_upl-gas_day.
  ENDIF.
  CLEAR lv_nomfnd.
  IF lt_contr[] IS NOT INITIAL.
    SELECT docnr idate FROM oijnomi
      INTO TABLE lt_nomi
      FOR ALL ENTRIES IN lt_contr
      WHERE docnr  = lt_contr-contract
        AND idate  = lt_contr-gas_day
        AND delind <> 'X'.
    IF lt_nomi[] IS NOT INITIAL.
      lv_nomfnd = 'X'.
    ENDIF.
  ENDIF.

  IF lv_nomfnd = 'X'.
    PERFORM f_popup_confirm
      USING 'Nomination exists against some of the CGD Contracts of customer/ Gas Day combination and will be re-calculated. Do you want to proceed?'
      CHANGING lv_answer.
    IF lv_answer = '1'.
      lv_warn = 'Nomination exists against some of the CGD Contracts'
             && ' of customer/ Gas Day combination and will be'
             && ' re-calculated - Yes'.
    ELSE.
      lv_warn = 'Nomination exists against some of the CGD Contracts'
             && ' of customer/ Gas Day combination and will be'
             && ' re-calculated - No'.
    ENDIF.
    LOOP AT gt_upl INTO ls_upl.
      CLEAR ls_log.
      ls_log-mandt   = sy-mandt.
      ls_log-gas_day = ls_upl-gas_day.
      ls_log-kunnr   = ls_upl-kunnr.
      ls_log-erdat   = sy-datum.
      ls_log-erzet   = sy-uzeit.
      PERFORM f_next_seqno CHANGING ls_log-seqno.
      ls_log-cnf_qty = ls_upl-qty.
      ls_log-message = lv_warn.
      ls_log-ernam   = sy-uname.
      APPEND ls_log TO lt_log.
    ENDLOOP.
    MODIFY yrxa_cnf_qty_log FROM TABLE lt_log.
    COMMIT WORK.
    IF lv_answer <> '1'.
      MESSAGE 'Upload cancelled by user' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.
    CLEAR lt_log[].
  ENDIF.

* Save: latest data in YRXA_CGD_CNF_QTY, append log
  LOOP AT gt_upl INTO ls_upl.
    CLEAR ls_cnf.
    ls_cnf-mandt   = sy-mandt.
    ls_cnf-gas_day = ls_upl-gas_day.
    ls_cnf-kunnr   = ls_upl-kunnr.
    ls_cnf-cnf_qty = ls_upl-qty.
    ls_cnf-uom     = 'SM3'.
    ls_cnf-ernam   = sy-uname.
    ls_cnf-erdat   = sy-datum.
    ls_cnf-erzet   = sy-uzeit.
    APPEND ls_cnf TO lt_cnf.

    CLEAR ls_log.
    ls_log-mandt   = sy-mandt.
    ls_log-gas_day = ls_upl-gas_day.
    ls_log-kunnr   = ls_upl-kunnr.
    ls_log-erdat   = sy-datum.
    ls_log-erzet   = sy-uzeit.
    PERFORM f_next_seqno CHANGING ls_log-seqno.
    ls_log-cnf_qty = ls_upl-qty.
    ls_log-message = 'Success'.
    ls_log-ernam   = sy-uname.
    APPEND ls_log TO lt_log.
  ENDLOOP.

  MODIFY yrxa_cgd_cnf_qty FROM TABLE lt_cnf.
  MODIFY yrxa_cnf_qty_log FROM TABLE lt_log.
  COMMIT WORK.
  MESSAGE 'Successfully uploaded' TYPE 'S'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CF_CALCULATE
*&---------------------------------------------------------------------*
* Stage 2 - Distribute customer confirmed qty to contracts (DCQ ratio)
*----------------------------------------------------------------------*
FORM f_cf_calculate.
  DATA: lt_dcq    TYPE TABLE OF yrxa_gaid_dcq,
        lt_cnf    TYPE TABLE OF yrxa_cgd_cnf_qty,
        ls_c      TYPE yrxa_cgd_cnf_qty,
        lt_dmd    TYPE TABLE OF yrxa_cgd_gaid_dmd,
        ls_d      TYPE yrxa_cgd_gaid_dmd,
        ls_alv    TYPE ty_alv2,
        lv_sum    TYPE ygms_cgd_qty,
        lv_cnf    TYPE ygms_cgd_qty,
        lv_dist   TYPE ygms_cgd_qty,
        lv_diff   TYPE ygms_cgd_qty,
        lv_answer TYPE char1,
        lv_rc     TYPE char1.
  FIELD-SYMBOLS <ls_adj> TYPE ty_alv2.

  IF s_gday2[] IS INITIAL.
    MESSAGE 'Gas Day is mandatory' TYPE 'E'.
  ENDIF.

  SELECT * FROM yrxa_gaid_dcq
    INTO TABLE lt_dcq
    WHERE gas_day IN s_gday2
      AND kunnr   IN s_kunnr.
  IF lt_dcq[] IS INITIAL.
    MESSAGE 'No data found in YRXA_GAID_DCQ for the selection'
      TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

* Confirmed qty per customer / gas day
  SELECT * FROM yrxa_cgd_cnf_qty
    INTO TABLE lt_cnf
    FOR ALL ENTRIES IN lt_dcq
    WHERE gas_day = lt_dcq-gas_day
      AND kunnr   = lt_dcq-kunnr.
  SORT lt_cnf BY gas_day kunnr.

* GA ID text from the demanded upload (per FS)
  SELECT * FROM yrxa_cgd_gaid_dmd
    INTO TABLE lt_dmd
    FOR ALL ENTRIES IN lt_dcq
    WHERE gas_day  = lt_dcq-gas_day
      AND sap_gaid = lt_dcq-sap_gaid.
  SORT lt_dmd BY gas_day sap_gaid.

  CLEAR gt_alv2[].
  SORT lt_dcq BY gas_day kunnr contract.
  LOOP AT lt_dcq INTO DATA(ls_first)
       GROUP BY ( gas_day = ls_first-gas_day
                  kunnr   = ls_first-kunnr ) INTO DATA(lo_g).
    CLEAR: lv_sum, lv_cnf, lv_dist.
    LOOP AT GROUP lo_g INTO DATA(ls_m).
      lv_sum = lv_sum + ls_m-dcq_qty.
    ENDLOOP.
    READ TABLE lt_cnf INTO ls_c
         WITH KEY gas_day = lo_g-gas_day kunnr = lo_g-kunnr
         BINARY SEARCH.
    IF sy-subrc = 0.
      lv_cnf = ls_c-cnf_qty.
    ENDIF.

    LOOP AT GROUP lo_g INTO ls_m.
      CLEAR ls_alv.
      ls_alv-contract  = ls_m-contract.
      ls_alv-kunnr     = ls_m-kunnr.
      ls_alv-sap_gaid  = ls_m-sap_gaid.
      ls_alv-gas_day   = ls_m-gas_day.
      ls_alv-dcq       = ls_m-dcq_qty.
      ls_alv-dcq_max   = ls_m-dcq_max.
      ls_alv-dcq_min   = ls_m-dcq_min.
      READ TABLE lt_dmd INTO ls_d
           WITH KEY gas_day = ls_m-gas_day sap_gaid = ls_m-sap_gaid
           BINARY SEARCH.
      IF sy-subrc = 0.
        ls_alv-upl_gaid  = ls_d-upl_gaid.
        ls_alv-gaid_desc = ls_d-gaid_desc.
      ELSE.
        ls_alv-upl_gaid  = ls_m-upl_gaid.
        ls_alv-gaid_desc = ls_m-gaid_desc.
      ENDIF.
      IF lv_sum > 0.
        ls_alv-cnf_qty = ls_m-dcq_qty / lv_sum * lv_cnf.
      ELSE.
        ls_alv-cnf_qty = 0.
      ENDIF.
      lv_dist = lv_dist + ls_alv-cnf_qty.
      APPEND ls_alv TO gt_alv2.
    ENDLOOP.

*   rounding difference adjustment on a contract with qty <> 0
    lv_diff = lv_cnf - lv_dist.
    IF lv_diff <> 0.
      LOOP AT gt_alv2 ASSIGNING <ls_adj>
           WHERE gas_day = lo_g-gas_day AND kunnr = lo_g-kunnr
             AND cnf_qty <> 0.
        <ls_adj>-cnf_qty = <ls_adj>-cnf_qty + lv_diff.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0 AND lv_cnf <> 0.
*       all shares zero (sum DCQ = 0): put full qty on first contract
        LOOP AT gt_alv2 ASSIGNING <ls_adj>
             WHERE gas_day = lo_g-gas_day AND kunnr = lo_g-kunnr.
          <ls_adj>-cnf_qty = <ls_adj>-cnf_qty + lv_diff.
          EXIT.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM f_show_alv USING gt_alv2
    'Contract wise Confirmed Qty (Calculated)'.

* Validate
  PERFORM f_popup_confirm USING 'Run validation now?'
                          CHANGING lv_answer.
  IF lv_answer <> '1'.
    RETURN.
  ENDIF.
  PERFORM f_cf_validate CHANGING lv_rc.
  IF lv_rc <> 'X'.
    RETURN.
  ENDIF.
  MESSAGE 'All validations passed' TYPE 'S'.

* Nomination create (enabled only after successful validation)
  PERFORM f_popup_confirm USING 'Create Nominations now?'
                          CHANGING lv_answer.
  IF lv_answer = '1'.
    PERFORM f_cf_nomcreate.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CF_VALIDATE
*&---------------------------------------------------------------------*
* Stage 2 validations 1-5 per FS. cv_ok = 'X' when all passed.
*----------------------------------------------------------------------*
FORM f_cf_validate CHANGING cv_ok TYPE char1.
  DATA: ls_err2    TYPE ty_err2,
        lt_g       TYPE TABLE OF ty_alv2,
        lt_alvcust TYPE TABLE OF kunnr,
        lv_cust    TYPE kunnr,
        lt_syscust TYPE TABLE OF kunnr,
        lt_alvcont TYPE TABLE OF vbeln_va,
        lt_syscont TYPE TABLE OF vbeln_va,
        lv_vbeln_x TYPE vbeln_va,
        lt_cur     TYPE TABLE OF yrvt_cont_dcq,
        ls_cur     TYPE yrvt_cont_dcq,
        lt_cald    TYPE TABLE OF yrxa_gaid_cal_dmd,
        ls_cald    TYPE yrxa_gaid_cal_dmd,
        lv_qsum    TYPE ygms_cgd_qty,
        lv_dsum    TYPE ygms_cgd_qty,
        lv_dmax    TYPE ygms_cgd_qty,
        lv_dmin    TYPE ygms_cgd_qty,
        lv_cal     TYPE ygms_cgd_qty.

  CLEAR: cv_ok, gt_err2[].

  lt_g = gt_alv2.
  SORT lt_g BY gas_day sap_gaid kunnr contract.
  LOOP AT lt_g INTO DATA(ls_f)
       GROUP BY ( gas_day  = ls_f-gas_day
                  sap_gaid = ls_f-sap_gaid ) INTO DATA(lo_grp).

*   customers in the ALV for this GA ID
    CLEAR lt_alvcust[].
    LOOP AT GROUP lo_grp INTO DATA(ls_m).
      APPEND ls_m-kunnr TO lt_alvcust.
    ENDLOOP.
    SORT lt_alvcust.
    DELETE ADJACENT DUPLICATES FROM lt_alvcust.

*   1. customer set change check
    CLEAR lt_syscust[].
    SELECT kunnr FROM kna1
      INTO TABLE lt_syscust
      WHERE yygaid = lo_grp-sap_gaid.
    SORT lt_syscust.
    IF lines( lt_syscust ) < lines( lt_alvcust ).
      CLEAR ls_err2.
      ls_err2-gas_day = lo_grp-gas_day.
      ls_err2-message = |No of Customer attached with GA ID|
                     && | ({ ls_m-gaid_desc }) has changed|.
      APPEND ls_err2 TO gt_err2.
    ELSEIF lines( lt_syscust ) > lines( lt_alvcust ).
*     extra customers: error only if they hold a valid CGD contract
      LOOP AT lt_syscust INTO lv_cust.
        READ TABLE lt_alvcust TRANSPORTING NO FIELDS
             WITH KEY table_line = lv_cust BINARY SEARCH.
        IF sy-subrc <> 0.
          CLEAR lv_vbeln_x.
          SELECT SINGLE a~vbeln
            INTO lv_vbeln_x
            FROM vbak AS a INNER JOIN veda AS b
              ON b~vbeln = a~vbeln
            WHERE a~kunnr   = lv_cust
              AND a~augru   = gv_augru
              AND b~vposn   = '000000'
              AND b~vbegdat <= lo_grp-gas_day
              AND b~venddat >= lo_grp-gas_day.
          IF sy-subrc = 0.
            CLEAR ls_err2.
            ls_err2-gas_day = lo_grp-gas_day.
            ls_err2-message = |No of Customer attached with GA ID|
                           && | ({ ls_m-gaid_desc }) has changed|.
            APPEND ls_err2 TO gt_err2.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

*   2. contract set change per customer
    LOOP AT lt_alvcust INTO lv_cust.
      CLEAR: lt_alvcont[], lt_syscont[].
      LOOP AT GROUP lo_grp INTO ls_m WHERE kunnr = lv_cust.
        APPEND ls_m-contract TO lt_alvcont.
      ENDLOOP.
      SORT lt_alvcont.
      SELECT a~vbeln
        INTO TABLE lt_syscont
        FROM vbak AS a INNER JOIN veda AS b
          ON b~vbeln = a~vbeln
        WHERE a~kunnr   = lv_cust
          AND a~augru   = gv_augru
          AND b~vposn   = '000000'
          AND b~vbegdat <= lo_grp-gas_day
          AND b~venddat >= lo_grp-gas_day.
      SORT lt_syscont.
      IF lt_syscont[] <> lt_alvcont[].
        CLEAR ls_err2.
        ls_err2-gas_day = lo_grp-gas_day.
        ls_err2-message =
          |Contracts mapped with Customer { lv_cust } changed|.
        APPEND ls_err2 TO gt_err2.
      ENDIF.
    ENDLOOP.

*   3. DCQ change + collect current sums for check 5
    CLEAR: lv_qsum, lv_dsum, lv_dmax, lv_dmin.
    LOOP AT GROUP lo_grp INTO ls_m.
      lv_qsum = lv_qsum + ls_m-cnf_qty.
      CLEAR lt_cur[].
      SELECT * FROM yrvt_cont_dcq
        INTO TABLE lt_cur
        WHERE vbeln = ls_m-contract
          AND ydate = lo_grp-gas_day.
      SORT lt_cur BY creon DESCENDING time DESCENDING.
      READ TABLE lt_cur INTO ls_cur INDEX 1.
      IF sy-subrc = 0.
        IF ls_cur-dcq_qty <> ls_m-dcq.
          CLEAR ls_err2.
          ls_err2-contract = ls_m-contract.
          ls_err2-gas_day  = lo_grp-gas_day.
          ls_err2-message  = |DCQ changed for Contract|
            && | { ls_m-contract } and Gas Day { lo_grp-gas_day }|.
          APPEND ls_err2 TO gt_err2.
        ENDIF.
        lv_dsum = lv_dsum + ls_cur-dcq_qty.
        lv_dmax = lv_dmax + ls_cur-dcq_qty * ls_cur-max_qty / 100.
        lv_dmin = lv_dmin + ls_cur-dcq_qty * ls_cur-min_qty / 100.
      ELSE.
        IF ls_m-dcq <> 0.
          CLEAR ls_err2.
          ls_err2-contract = ls_m-contract.
          ls_err2-gas_day  = lo_grp-gas_day.
          ls_err2-message  = |DCQ changed for Contract|
            && | { ls_m-contract } and Gas Day { lo_grp-gas_day }|.
          APPEND ls_err2 TO gt_err2.
        ENDIF.
      ENDIF.
    ENDLOOP.

*   latest calculated demanded qty of the GA ID
    CLEAR: lv_cal, lt_cald[].
    SELECT * FROM yrxa_gaid_cal_dmd
      INTO TABLE lt_cald
      WHERE gas_day  = lo_grp-gas_day
        AND sap_gaid = lo_grp-sap_gaid.
    SORT lt_cald BY erdat DESCENDING erzet DESCENDING seqno DESCENDING.
    READ TABLE lt_cald INTO ls_cald INDEX 1.
    IF sy-subrc = 0.
      lv_cal = ls_cald-cal_dmd_qty.
    ENDIF.

*   4. sum of confirmed vs calculated demanded qty
    IF lv_qsum <> lv_cal.
      CLEAR ls_err2.
      ls_err2-gas_day = lo_grp-gas_day.
      ls_err2-message = |Confirmed Qty against all the customer|
        && | doesn't match with the calculated demanded qty of GA ID|
        && | { ls_m-upl_gaid }|.
      APPEND ls_err2 TO gt_err2.
    ENDIF.

*   5. demanded qty range check with current DCQ sums
    SELECT COUNT(*) FROM yrxa_cgd_gaid_dmd
      WHERE gas_day  = lo_grp-gas_day
        AND sap_gaid = lo_grp-sap_gaid.
    IF sy-dbcnt > 0.
      IF lv_cal < lv_dmin OR lv_cal > lv_dmax.
        CLEAR ls_err2.
        ls_err2-gas_day = lo_grp-gas_day.
        ls_err2-message = |GA ID { ls_m-upl_gaid }|
          && | ({ ls_m-gaid_desc }) confirmed qty no longer lies|
          && | between the DCQ range for the related contracts|.
        APPEND ls_err2 TO gt_err2.
      ENDIF.
    ELSE.
      IF lv_cal <> lv_dsum.
        CLEAR ls_err2.
        ls_err2-gas_day = lo_grp-gas_day.
        ls_err2-message = |GA ID ({ ls_m-gaid_desc }) confirmed qty|
          && | is not same as the sum of DCQ of the related contracts|.
        APPEND ls_err2 TO gt_err2.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Log the validation run
  PERFORM f_cf_log USING 'V'.

  IF gt_err2[] IS NOT INITIAL.
    PERFORM f_show_err_popup USING gt_err2 'Validation Errors'.
    RETURN.
  ENDIF.
  cv_ok = 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CF_LOG
*&---------------------------------------------------------------------*
* Append YRXA_CONT_CNF_LOG rows. iv_mode 'V' validate / 'N' nomination
*----------------------------------------------------------------------*
FORM f_cf_log USING iv_mode TYPE char1.
  DATA: lt_log TYPE TABLE OF yrxa_cont_cnf_log,
        ls_log TYPE yrxa_cont_cnf_log,
        ls_alv TYPE ty_alv2,
        ls_e   TYPE ty_err2,
        ls_n   TYPE ty_nomn_error.

  LOOP AT gt_alv2 INTO ls_alv.
    CLEAR ls_log.
    ls_log-mandt     = sy-mandt.
    ls_log-gas_day   = ls_alv-gas_day.
    ls_log-contract  = ls_alv-contract.
    ls_log-erdat     = sy-datum.
    ls_log-erzet     = sy-uzeit.
    PERFORM f_next_seqno CHANGING ls_log-seqno.
    ls_log-kunnr     = ls_alv-kunnr.
    ls_log-sap_gaid  = ls_alv-sap_gaid.
    ls_log-gaid_desc = ls_alv-gaid_desc.
    ls_log-upl_gaid  = ls_alv-upl_gaid.
    ls_log-dcq_qty   = ls_alv-dcq.
    ls_log-dcq_max   = ls_alv-dcq_max.
    ls_log-dcq_min   = ls_alv-dcq_min.
    ls_log-cnf_qty   = ls_alv-cnf_qty.
    ls_log-uom       = 'SM3'.
    ls_log-ernam     = sy-uname.
    IF iv_mode = 'V'.
      ls_log-val_uname = sy-uname.
      ls_log-val_datum = sy-datum.
      ls_log-val_uzeit = sy-uzeit.
      READ TABLE gt_err2 INTO ls_e
           WITH KEY contract = ls_alv-contract
                    gas_day  = ls_alv-gas_day.
      IF sy-subrc = 0.
        ls_log-message = ls_e-message.
      ELSE.
        ls_log-message = 'Validation passed'.
      ENDIF.
    ELSE.
      ls_log-nom_uname = sy-uname.
      ls_log-nom_datum = sy-datum.
      ls_log-nom_uzeit = sy-uzeit.
      READ TABLE itab_nomination INTO ls_n
           WITH KEY vbeln = ls_alv-contract
                    idate = ls_alv-gas_day.
      IF sy-subrc = 0.
        ls_log-message = ls_n-error.
      ELSE.
        ls_log-message = 'Nomination processed'.
      ENDIF.
    ENDIF.
    APPEND ls_log TO lt_log.
  ENDLOOP.
  MODIFY yrxa_cont_cnf_log FROM TABLE lt_log.
  COMMIT WORK.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_CF_NOMCREATE
*&---------------------------------------------------------------------*
* Nomination creation through the Excel Sheet Upload logic of YRXR001
* (program YRXR028_NOMINATION_NEW) using the YRXR006 memory interface:
*   EXPORT it_nomn[]   -> memory id 'NOMI_CHANGE_YRXR006'
*   EXPORT it_nomn_r[] -> memory id 'NOMI_REASON_YRXR006'
*   CALL TRANSACTION 'YRXR006' (RB_EX set via batch input)
*   IMPORT itab_nomination[] <- memory id 'NOMI_ERROR_YRXR006'
*----------------------------------------------------------------------*
FORM f_cf_nomcreate.
  DATA: ls_bdc     TYPE bdcdata,
        ls_alv     TYPE ty_alv2,
        lt_days    TYPE TABLE OF datum,
        lv_day     TYPE datum,
        lt_err_all TYPE TABLE OF ty_nomn_error.

* distinct gas days
  LOOP AT gt_alv2 INTO ls_alv.
    APPEND ls_alv-gas_day TO lt_days.
  ENDLOOP.
  SORT lt_days.
  DELETE ADJACENT DUPLICATES FROM lt_days.

  LOOP AT lt_days INTO lv_day.
    CLEAR: it_nomn[], it_nomn_r[], itab_nomination[].

*   date wise data: Contract ID | Gas Day | Confirmed Qty | UoM (SM3)
    LOOP AT gt_alv2 INTO ls_alv WHERE gas_day = lv_day.
      CLEAR it_nomn.
      it_nomn-vbeln = ls_alv-contract.
      it_nomn-idate = ls_alv-gas_day.
      it_nomn-menge = ls_alv-cnf_qty.
      it_nomn-unit  = 'SM3'.
      APPEND it_nomn.
    ENDLOOP.
    IF it_nomn[] IS INITIAL.
      CONTINUE.
    ENDIF.

    EXPORT it_nomn[]   TO MEMORY ID 'NOMI_CHANGE_YRXR006'.
    EXPORT it_nomn_r[] TO MEMORY ID 'NOMI_REASON_YRXR006'.

*   call the nomination program via T-Code YRXR006 (Excel Upload path)
    CLEAR gt_bdc[].
    CLEAR ls_bdc.
    ls_bdc-program  = 'YRXR028_NOMINATION_NEW'.
    ls_bdc-dynpro   = '1000'.
    ls_bdc-dynbegin = 'X'.
    APPEND ls_bdc TO gt_bdc.
    CLEAR ls_bdc.
    ls_bdc-fnam = 'RB_EX'.
    ls_bdc-fval = 'X'.
    APPEND ls_bdc TO gt_bdc.
    CLEAR ls_bdc.
    ls_bdc-fnam = 'BDC_OKCODE'.
    ls_bdc-fval = '=ONLI'.
    APPEND ls_bdc TO gt_bdc.

    CALL TRANSACTION 'YRXR006' USING gt_bdc
         MODE 'N' UPDATE 'S'.

*   errors reported back by YRXR028
    CLEAR itab_nomination[].
    IMPORT itab_nomination[] FROM MEMORY ID 'NOMI_ERROR_YRXR006'.
    IF sy-subrc = 0 AND itab_nomination[] IS NOT INITIAL.
      APPEND LINES OF itab_nomination TO lt_err_all.
    ENDIF.
    FREE MEMORY ID 'NOMI_CHANGE_YRXR006'.
    FREE MEMORY ID 'NOMI_REASON_YRXR006'.
    FREE MEMORY ID 'NOMI_ERROR_YRXR006'.
    FREE MEMORY ID 'NOMI_SERROR_YRXR006'.
  ENDLOOP.

* keep the complete error list for logging and display
  CLEAR itab_nomination[].
  itab_nomination[] = lt_err_all[].

  PERFORM f_cf_log USING 'N'.

  IF lt_err_all[] IS NOT INITIAL.
*   show errors of YRXR001 but return to this program (per FS)
    PERFORM f_show_err_popup USING lt_err_all 'Nomination Errors'.
    MESSAGE 'Nomination creation finished with errors' TYPE 'S'
      DISPLAY LIKE 'W'.
  ELSE.
    MESSAGE 'Nominations created successfully' TYPE 'S'.
  ENDIF.
ENDFORM.
