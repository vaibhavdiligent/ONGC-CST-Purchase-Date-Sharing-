CLASS zcl_zpra_dpr_pdf DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    "-- Reuse the row types defined in the Excel class so both helpers
    "   work on the same SELECT result and stay in sync.
    TYPES:
      tt_prod_rows   TYPE zcl_zpra_dpr_excel=>tt_prod_rows,
      tt_target_rows TYPE zcl_zpra_dpr_excel=>tt_target_rows.

    CLASS-METHODS:
      "! Fetch production data and return as PDF binary + page count.
      "! Internally selects the same data that the Excel exporter uses
      "! (so both buttons produce a faithful snapshot of identical content)
      "! and renders it via Adobe Document Services / SmartForms
      "! (whichever is configured as default in the system).
      fetch_and_export_production
        IMPORTING
          iv_date_from TYPE sy-datum
          iv_date_to   TYPE sy-datum
        EXPORTING
          ev_pdf       TYPE xstring
          ev_pages     TYPE i
        RAISING
          cx_ai_system_error,

      "! Fetch target vs actual data and return as PDF binary + page count.
      fetch_and_export_targets
        IMPORTING
          iv_fiscal_year TYPE gjahr
          iv_target_code TYPE zpra_t_prd_tar-tar_code
        EXPORTING
          ev_pdf         TYPE xstring
          ev_pages       TYPE i
        RAISING
          cx_ai_system_error,

      "! Render an arbitrary production row table as PDF.
      "! Form name configured via the constants below — change once
      "! the Adobe form ZPRA_FRM_DPR_PRODUCTION is created in SFP.
      generate_production_pdf
        IMPORTING
          it_data      TYPE tt_prod_rows
          iv_date_from TYPE sy-datum
          iv_date_to   TYPE sy-datum
        EXPORTING
          ev_pdf       TYPE xstring
          ev_pages     TYPE i
        RAISING
          cx_ai_system_error,

      "! Render a target vs actual table as PDF.
      generate_target_pdf
        IMPORTING
          it_data        TYPE tt_target_rows
          iv_fiscal_year TYPE gjahr
          iv_target_code TYPE zpra_t_prd_tar-tar_code
        EXPORTING
          ev_pdf         TYPE xstring
          ev_pages       TYPE i
        RAISING
          cx_ai_system_error.

  PRIVATE SECTION.

    " ── Adobe Form names (create via SFP) ─────────────────────────────────
    CONSTANTS:
      c_form_production TYPE fpname VALUE 'ZPRA_FRM_DPR_PRODUCTION',
      c_form_target     TYPE fpname VALUE 'ZPRA_FRM_DPR_TARGETS'.

    " ── Helper: select production data (same query as Excel exporter) ─────
    CLASS-METHODS select_production_data
      IMPORTING iv_date_from TYPE sy-datum
                iv_date_to   TYPE sy-datum
      RETURNING VALUE(rt_data) TYPE tt_prod_rows
      RAISING   cx_ai_system_error.

    CLASS-METHODS select_target_data
      IMPORTING iv_fiscal_year TYPE gjahr
                iv_target_code TYPE zpra_t_prd_tar-tar_code
      RETURNING VALUE(rt_data) TYPE tt_target_rows
      RAISING   cx_ai_system_error.

    " ── Adobe Forms wrapper: open job, run form, close job, return xstring
    CLASS-METHODS render_pdf
      IMPORTING iv_form_name   TYPE fpname
                iv_title       TYPE string
      EXPORTING ev_pdf         TYPE xstring
                ev_pages       TYPE i
                eo_function    TYPE REF TO data
      RAISING   cx_ai_system_error.

ENDCLASS.



CLASS zcl_zpra_dpr_pdf IMPLEMENTATION.

  "-------------------------------------------------------------------------
  " Public: production data → PDF (xstring + page count)
  "-------------------------------------------------------------------------
  METHOD fetch_and_export_production.
    DATA(lt_data) = select_production_data(
                      iv_date_from = iv_date_from
                      iv_date_to   = iv_date_to ).

    generate_production_pdf(
      EXPORTING it_data      = lt_data
                iv_date_from = iv_date_from
                iv_date_to   = iv_date_to
      IMPORTING ev_pdf       = ev_pdf
                ev_pages     = ev_pages ).
  ENDMETHOD.


  METHOD fetch_and_export_targets.
    DATA(lt_data) = select_target_data(
                      iv_fiscal_year = iv_fiscal_year
                      iv_target_code = iv_target_code ).

    generate_target_pdf(
      EXPORTING it_data        = lt_data
                iv_fiscal_year = iv_fiscal_year
                iv_target_code = iv_target_code
      IMPORTING ev_pdf         = ev_pdf
                ev_pages       = ev_pages ).
  ENDMETHOD.


  "-------------------------------------------------------------------------
  " Render production PDF via Adobe Document Services
  "-------------------------------------------------------------------------
  METHOD generate_production_pdf.
    DATA: lv_fm_name        TYPE rs38l_fnam,
          ls_outputparams   TYPE sfpoutputparams,
          ls_docparams      TYPE sfpdocparams,
          ls_formoutput     TYPE fpformoutput,
          lv_fp_function    TYPE REF TO data.

    TRY.
        " 1. Resolve generated function module for the Adobe form
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING  i_name     = c_form_production
          IMPORTING  e_funcname = lv_fm_name
          EXCEPTIONS OTHERS     = 1.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_ai_system_error
            EXPORTING textid = cx_ai_system_error=>cx_ai_system_error.
        ENDIF.

        " 2. Open job — request raw PDF, no printer dialog
        ls_outputparams-nodialog  = abap_true.
        ls_outputparams-getpdf    = abap_true.
        ls_outputparams-preview   = abap_false.
        ls_outputparams-reqnew    = abap_true.

        CALL FUNCTION 'FP_JOB_OPEN'
          CHANGING ie_outputparams = ls_outputparams
          EXCEPTIONS OTHERS         = 1.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_ai_system_error
            EXPORTING textid = cx_ai_system_error=>cx_ai_system_error.
        ENDIF.

        " 3. Call generated FM with form data
        ls_docparams-langu   = sy-langu.
        ls_docparams-country = 'IN'.

        CALL FUNCTION lv_fm_name
          EXPORTING /1bcdwb/docparams  = ls_docparams
                    iv_date_from       = iv_date_from
                    iv_date_to         = iv_date_to
                    it_data            = it_data
          IMPORTING /1bcdwb/formoutput = ls_formoutput
          EXCEPTIONS OTHERS            = 1.
        IF sy-subrc <> 0.
          CALL FUNCTION 'FP_JOB_CLOSE'.
          RAISE EXCEPTION TYPE cx_ai_system_error
            EXPORTING textid = cx_ai_system_error=>cx_ai_system_error.
        ENDIF.

        " 4. Close job
        CALL FUNCTION 'FP_JOB_CLOSE'
          EXCEPTIONS OTHERS = 1.

        ev_pdf   = ls_formoutput-pdf.
        ev_pages = ls_formoutput-pages.

      CATCH cx_ai_system_error.
        RAISE.
      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION TYPE cx_ai_system_error
          EXPORTING textid = cx_ai_system_error=>cx_ai_system_error
                    previous = lx.
    ENDTRY.
  ENDMETHOD.


  "-------------------------------------------------------------------------
  " Render target vs actual PDF
  "-------------------------------------------------------------------------
  METHOD generate_target_pdf.
    DATA: lv_fm_name      TYPE rs38l_fnam,
          ls_outputparams TYPE sfpoutputparams,
          ls_docparams    TYPE sfpdocparams,
          ls_formoutput   TYPE fpformoutput.

    TRY.
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING  i_name     = c_form_target
          IMPORTING  e_funcname = lv_fm_name
          EXCEPTIONS OTHERS     = 1.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_ai_system_error
            EXPORTING textid = cx_ai_system_error=>cx_ai_system_error.
        ENDIF.

        ls_outputparams-nodialog = abap_true.
        ls_outputparams-getpdf   = abap_true.
        ls_outputparams-preview  = abap_false.
        ls_outputparams-reqnew   = abap_true.

        CALL FUNCTION 'FP_JOB_OPEN'
          CHANGING   ie_outputparams = ls_outputparams
          EXCEPTIONS OTHERS           = 1.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_ai_system_error
            EXPORTING textid = cx_ai_system_error=>cx_ai_system_error.
        ENDIF.

        ls_docparams-langu   = sy-langu.
        ls_docparams-country = 'IN'.

        CALL FUNCTION lv_fm_name
          EXPORTING /1bcdwb/docparams  = ls_docparams
                    iv_fiscal_year     = iv_fiscal_year
                    iv_target_code     = iv_target_code
                    it_data            = it_data
          IMPORTING /1bcdwb/formoutput = ls_formoutput
          EXCEPTIONS OTHERS            = 1.
        IF sy-subrc <> 0.
          CALL FUNCTION 'FP_JOB_CLOSE'.
          RAISE EXCEPTION TYPE cx_ai_system_error
            EXPORTING textid = cx_ai_system_error=>cx_ai_system_error.
        ENDIF.

        CALL FUNCTION 'FP_JOB_CLOSE'
          EXCEPTIONS OTHERS = 1.

        ev_pdf   = ls_formoutput-pdf.
        ev_pages = ls_formoutput-pages.

      CATCH cx_ai_system_error.
        RAISE.
      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION TYPE cx_ai_system_error
          EXPORTING textid = cx_ai_system_error=>cx_ai_system_error
                    previous = lx.
    ENDTRY.
  ENDMETHOD.


  "-------------------------------------------------------------------------
  " Helper: select production rows (mirrors Excel exporter)
  "-------------------------------------------------------------------------
  METHOD select_production_data.
    SELECT
        d~production_date,
        d~product,
        ''                           AS product_desc,
        d~asset,
        a~dn_de                      AS asset_desc,
        d~block,
        d~prd_vl_type                AS volume_type,
        d~prod_vl_qty1               AS prod_qty1,
        d~prod_vl_uom1               AS prod_uom1,
        d~prod_vl_qty2               AS prod_qty2,
        d~prod_vl_uom2               AS prod_uom2,
        p~pi                         AS pi_pct,
        CAST( d~prod_vl_qty1 * p~pi / 100 AS d34_3 ) AS ovl_qty1,
        CAST( d~prod_vl_qty2 * p~pi / 100 AS d34_3 ) AS ovl_qty2
      FROM zpra_t_dly_prd AS d
        LEFT OUTER JOIN zoiu_pr_dn  AS a ON d~asset = a~dn_no
        LEFT OUTER JOIN zpra_t_prd_pi AS p
          ON  d~asset           = p~asset
          AND d~block           = p~block
          AND d~production_date BETWEEN p~vld_frm AND p~vld_to
      WHERE d~production_date BETWEEN @iv_date_from AND @iv_date_to
        AND d~prd_vl_type = 'NET_PROD'
      INTO TABLE @rt_data.
  ENDMETHOD.


  METHOD select_target_data.
    SELECT
        t~gjahr                      AS fiscal_year,
        t~monat                      AS fiscal_period,
        t~product,
        ''                           AS product_desc,
        t~asset,
        a~dn_de                      AS asset_desc,
        t~block,
        t~tar_code                   AS target_code,
        ''                           AS target_type_desc,
        CAST( 0 AS d34_3 )           AS actual_qty,
        CAST( t~tar_qty AS d34_3 )   AS target_qty,
        CAST( 0 - t~tar_qty AS d34_3 ) AS variance_qty,
        CAST( 0 AS d9_2 )            AS achievement_pct
      FROM zpra_t_prd_tar AS t
        LEFT OUTER JOIN zoiu_pr_dn AS a ON t~asset = a~dn_no
      WHERE t~gjahr    = @iv_fiscal_year
        AND t~tar_code = @iv_target_code
      INTO TABLE @rt_data.
  ENDMETHOD.


  METHOD render_pdf.
    " Reserved for future shared rendering helper. Currently each
    " generate_*_pdf method handles open/run/close inline because the
    " function module signature differs per form.
    CLEAR: ev_pdf, ev_pages, eo_function.
  ENDMETHOD.

ENDCLASS.
