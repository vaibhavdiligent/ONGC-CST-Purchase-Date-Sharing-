CLASS zbp_zpra_dpr_excel_dl DEFINITION
  PUBLIC ABSTRACT FINAL
  FOR BEHAVIOR OF zpra_i_dpr_excel_dl.

  PUBLIC SECTION.
ENDCLASS.

CLASS zbp_zpra_dpr_excel_dl IMPLEMENTATION.
ENDCLASS.

"-- Handler class for static download actions (Excel + PDF) -----------------
CLASS lhc_dpr_excel DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS:
      "! Excel — daily production export
      download_production FOR MODIFY
        IMPORTING it_action_import FOR ACTION
                  dprexceldownload~downloadproduction RESULT et_action_result,

      "! Excel — targets vs actual export
      download_targets FOR MODIFY
        IMPORTING it_action_import FOR ACTION
                  dprexceldownload~downloadtargets RESULT et_action_result,

      "! PDF — daily production export
      download_pdf_production FOR MODIFY
        IMPORTING it_action_import FOR ACTION
                  dprexceldownload~downloadpdfproduction RESULT et_action_result,

      "! PDF — targets vs actual export
      download_pdf_targets FOR MODIFY
        IMPORTING it_action_import FOR ACTION
                  dprexceldownload~downloadpdftargets RESULT et_action_result.

ENDCLASS.

CLASS lhc_dpr_excel IMPLEMENTATION.

  "-------------------------------------------------------------------------
  " Excel: daily production
  "-------------------------------------------------------------------------
  METHOD download_production.
    DATA: ls_result TYPE STRUCTURE FOR ACTION RESULT zpra_i_dpr_excel_dl~downloadProduction,
          lv_xdata  TYPE xstring,
          lv_b64    TYPE string.

    READ TABLE it_action_import INTO DATA(ls_param) INDEX 1.
    IF sy-subrc <> 0.
      RAISE SHORTDUMP TYPE cx_rap_query_provider
        MESSAGE e001(00) WITH 'No parameters provided'.
    ENDIF.

    TRY.
        lv_xdata = zcl_zpra_dpr_excel=>fetch_and_export_production(
          iv_date_from = ls_param-%param-date_from
          iv_date_to   = ls_param-%param-date_to
        ).

        lv_b64 = cl_http_utility=>encode_x_base64( lv_xdata ).

        ls_result-%param-excel_base64 = lv_b64.
        ls_result-%param-file_name    =
          |DPR_Production_{ ls_param-%param-date_from }_{ ls_param-%param-date_to }.xlsx|.
        ls_result-%param-mime_type    = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'.
        ls_result-%param-message      = 'Excel generated successfully'.
        APPEND ls_result TO et_action_result.

      CATCH cx_ai_system_error INTO DATA(lx_excel).
        RAISE SHORTDUMP TYPE cx_rap_query_provider
          MESSAGE e001(00) WITH lx_excel->get_text( ).
    ENDTRY.
  ENDMETHOD.


  "-------------------------------------------------------------------------
  " Excel: targets vs actual
  "-------------------------------------------------------------------------
  METHOD download_targets.
    DATA: ls_result TYPE STRUCTURE FOR ACTION RESULT zpra_i_dpr_excel_dl~downloadTargets,
          lv_xdata  TYPE xstring,
          lv_b64    TYPE string.

    READ TABLE it_action_import INTO DATA(ls_param) INDEX 1.
    IF sy-subrc <> 0.
      RAISE SHORTDUMP TYPE cx_rap_query_provider
        MESSAGE e001(00) WITH 'No parameters provided'.
    ENDIF.

    TRY.
        lv_xdata = zcl_zpra_dpr_excel=>fetch_and_export_targets(
          iv_fiscal_year = ls_param-%param-fiscal_year
          iv_target_code = ls_param-%param-target_code
        ).

        lv_b64 = cl_http_utility=>encode_x_base64( lv_xdata ).

        ls_result-%param-excel_base64 = lv_b64.
        ls_result-%param-file_name    =
          |DPR_Targets_{ ls_param-%param-fiscal_year }_{ ls_param-%param-target_code }.xlsx|.
        ls_result-%param-mime_type    = 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'.
        ls_result-%param-message      = 'Excel generated successfully'.
        APPEND ls_result TO et_action_result.

      CATCH cx_ai_system_error INTO DATA(lx_targets).
        RAISE SHORTDUMP TYPE cx_rap_query_provider
          MESSAGE e001(00) WITH lx_targets->get_text( ).
    ENDTRY.
  ENDMETHOD.


  "-------------------------------------------------------------------------
  " PDF: daily production
  "-------------------------------------------------------------------------
  METHOD download_pdf_production.
    DATA: ls_result TYPE STRUCTURE FOR ACTION RESULT zpra_i_dpr_excel_dl~downloadPdfProduction,
          lv_xdata  TYPE xstring,
          lv_pages  TYPE i,
          lv_b64    TYPE string.

    READ TABLE it_action_import INTO DATA(ls_param) INDEX 1.
    IF sy-subrc <> 0.
      RAISE SHORTDUMP TYPE cx_rap_query_provider
        MESSAGE e001(00) WITH 'No parameters provided'.
    ENDIF.

    TRY.
        zcl_zpra_dpr_pdf=>fetch_and_export_production(
          EXPORTING iv_date_from = ls_param-%param-date_from
                    iv_date_to   = ls_param-%param-date_to
          IMPORTING ev_pdf       = lv_xdata
                    ev_pages     = lv_pages
        ).

        lv_b64 = cl_http_utility=>encode_x_base64( lv_xdata ).

        ls_result-%param-pdf_base64 = lv_b64.
        ls_result-%param-file_name  =
          |DPR_Production_{ ls_param-%param-date_from }_{ ls_param-%param-date_to }.pdf|.
        ls_result-%param-mime_type  = 'application/pdf'.
        ls_result-%param-page_count = lv_pages.
        ls_result-%param-message    = |PDF generated ({ lv_pages } page(s))|.
        APPEND ls_result TO et_action_result.

      CATCH cx_ai_system_error INTO DATA(lx_pdf_p).
        RAISE SHORTDUMP TYPE cx_rap_query_provider
          MESSAGE e001(00) WITH lx_pdf_p->get_text( ).
    ENDTRY.
  ENDMETHOD.


  "-------------------------------------------------------------------------
  " PDF: targets vs actual
  "-------------------------------------------------------------------------
  METHOD download_pdf_targets.
    DATA: ls_result TYPE STRUCTURE FOR ACTION RESULT zpra_i_dpr_excel_dl~downloadPdfTargets,
          lv_xdata  TYPE xstring,
          lv_pages  TYPE i,
          lv_b64    TYPE string.

    READ TABLE it_action_import INTO DATA(ls_param) INDEX 1.
    IF sy-subrc <> 0.
      RAISE SHORTDUMP TYPE cx_rap_query_provider
        MESSAGE e001(00) WITH 'No parameters provided'.
    ENDIF.

    TRY.
        zcl_zpra_dpr_pdf=>fetch_and_export_targets(
          EXPORTING iv_fiscal_year = ls_param-%param-fiscal_year
                    iv_target_code = ls_param-%param-target_code
          IMPORTING ev_pdf         = lv_xdata
                    ev_pages       = lv_pages
        ).

        lv_b64 = cl_http_utility=>encode_x_base64( lv_xdata ).

        ls_result-%param-pdf_base64 = lv_b64.
        ls_result-%param-file_name  =
          |DPR_Targets_{ ls_param-%param-fiscal_year }_{ ls_param-%param-target_code }.pdf|.
        ls_result-%param-mime_type  = 'application/pdf'.
        ls_result-%param-page_count = lv_pages.
        ls_result-%param-message    = |PDF generated ({ lv_pages } page(s))|.
        APPEND ls_result TO et_action_result.

      CATCH cx_ai_system_error INTO DATA(lx_pdf_t).
        RAISE SHORTDUMP TYPE cx_rap_query_provider
          MESSAGE e001(00) WITH lx_pdf_t->get_text( ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
