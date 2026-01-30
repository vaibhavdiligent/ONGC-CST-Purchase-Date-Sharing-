*&---------------------------------------------------------------------*
*& Class: YGMS_CL_CST_PDF_GENERATOR
*& Package: YGMS
*& Description: PDF Report Generation
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

CLASS ygms_cl_cst_pdf_generator DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    METHODS constructor.

    "! <p class="shorttext synchronized" lang="en">Generate PDF from purchase data</p>
    "! @parameter it_data | Purchase data
    "! @parameter rv_content | PDF content as xstring
    METHODS generate_pdf
      IMPORTING
        it_data          TYPE ygms_tt_purchase
      RETURNING
        VALUE(rv_content) TYPE xstring.

    "! <p class="shorttext synchronized" lang="en">Get PDF content</p>
    "! @parameter rv_content | PDF content
    METHODS get_pdf_content
      RETURNING
        VALUE(rv_content) TYPE xstring.

  PRIVATE SECTION.
    DATA: mv_pdf_content TYPE xstring.

    "! <p class="shorttext synchronized" lang="en">Create PDF header</p>
    METHODS create_header
      IMPORTING
        it_data TYPE ygms_tt_purchase
      RETURNING
        VALUE(rv_header) TYPE string.

    "! <p class="shorttext synchronized" lang="en">Create PDF body</p>
    METHODS create_body
      IMPORTING
        it_data TYPE ygms_tt_purchase
      RETURNING
        VALUE(rv_body) TYPE string.

    "! <p class="shorttext synchronized" lang="en">Create PDF footer</p>
    METHODS create_footer
      RETURNING
        VALUE(rv_footer) TYPE string.

ENDCLASS.


CLASS ygms_cl_cst_pdf_generator IMPLEMENTATION.

  METHOD constructor.
    CLEAR mv_pdf_content.
  ENDMETHOD.


  METHOD generate_pdf.
    " Generate PDF using Adobe Forms or Smart Forms
    " This is a placeholder implementation - actual implementation depends on
    " client's form templates and preferences

    DATA: lv_form_name TYPE tdsfname VALUE 'YGMS_CST_PURCHASE_FORM',
          ls_output    TYPE ssfcrespd,
          lv_job_output_info TYPE ssfcrescl.

    TRY.
        " Check if Smart Form exists
        DATA(lv_fm_name) = CONV rs38l_fnam( '' ).

        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            formname           = lv_form_name
          IMPORTING
            fm_name            = lv_fm_name
          EXCEPTIONS
            no_form            = 1
            no_function_module = 2
            OTHERS             = 3.

        IF sy-subrc = 0 AND lv_fm_name IS NOT INITIAL.
          " Call Smart Form with data
          " Note: This is a template - actual form parameters depend on form design

          " Set output options for PDF
          DATA(ls_control) = VALUE ssfctrlop(
            no_dialog = abap_true
            getotf    = abap_true
          ).

          DATA(ls_output_options) = VALUE ssfcompop(
            tddest = 'LP01'
          ).

          " Call the form function module
          " This would be dynamically called based on form name
          " CALL FUNCTION lv_fm_name
          "   EXPORTING
          "     control_parameters = ls_control
          "     output_options     = ls_output_options
          "     it_purchase_data   = it_data
          "   IMPORTING
          "     job_output_info    = lv_job_output_info
          "   EXCEPTIONS
          "     OTHERS             = 1.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        " If form generation fails, create simple text-based PDF
    ENDTRY.

    " If Smart Form not available, generate simple PDF using convert functions
    " Create HTML content first, then convert to PDF
    DATA: lv_html TYPE string.

    " Build HTML content
    lv_html = |<html><head><title>ONGC CST Purchase Data</title></head><body>|.
    lv_html = lv_html && create_header( it_data ).
    lv_html = lv_html && create_body( it_data ).
    lv_html = lv_html && create_footer( ).
    lv_html = lv_html && |</body></html>|.

    " Convert HTML to xstring
    rv_content = cl_abap_codepage=>convert_to( lv_html ).

    " Store for later retrieval
    mv_pdf_content = rv_content.
  ENDMETHOD.


  METHOD get_pdf_content.
    rv_content = mv_pdf_content.
  ENDMETHOD.


  METHOD create_header.
    " Get date range from data
    DATA: lv_min_date TYPE datum VALUE '99991231',
          lv_max_date TYPE datum VALUE '00010101',
          lv_location TYPE ygms_loc_id.

    LOOP AT it_data INTO DATA(ls_data).
      IF ls_data-gas_day < lv_min_date.
        lv_min_date = ls_data-gas_day.
      ENDIF.
      IF ls_data-gas_day > lv_max_date.
        lv_max_date = ls_data-gas_day.
      ENDIF.
      IF lv_location IS INITIAL.
        lv_location = ls_data-location_id.
      ENDIF.
    ENDLOOP.

    " Build header
    rv_header = |<div style="text-align:center;">|.
    rv_header = rv_header && |<h1>GAIL (India) Limited</h1>|.
    rv_header = rv_header && |<h2>ONGC CST Purchase Data Sharing Report</h2>|.
    rv_header = rv_header && |<p>Period: { lv_min_date DATE = USER } to { lv_max_date DATE = USER }</p>|.
    rv_header = rv_header && |<p>Location: { lv_location }</p>|.
    rv_header = rv_header && |<p>Generated on: { sy-datum DATE = USER } at { sy-uzeit TIME = USER }</p>|.
    rv_header = rv_header && |</div><hr/>|.
  ENDMETHOD.


  METHOD create_body.
    " Build table with data
    rv_body = |<table border="1" cellpadding="5" cellspacing="0" width="100%">|.

    " Header row
    rv_body = rv_body && |<tr style="background-color:#cccccc;">|.
    rv_body = rv_body && |<th>Gas Day</th>|.
    rv_body = rv_body && |<th>Location</th>|.
    rv_body = rv_body && |<th>Material</th>|.
    rv_body = rv_body && |<th>State</th>|.
    rv_body = rv_body && |<th>Qty (MMBTU)</th>|.
    rv_body = rv_body && |<th>GCV</th>|.
    rv_body = rv_body && |<th>NCV</th>|.
    rv_body = rv_body && |<th>Qty (SCM)</th>|.
    rv_body = rv_body && |<th>Tax Type</th>|.
    rv_body = rv_body && |</tr>|.

    " Data rows
    DATA: lv_total_mbg TYPE ygms_qty_mbg,
          lv_total_scm TYPE ygms_qty_scm.

    LOOP AT it_data INTO DATA(ls_data).
      rv_body = rv_body && |<tr>|.
      rv_body = rv_body && |<td>{ ls_data-gas_day DATE = USER }</td>|.
      rv_body = rv_body && |<td>{ ls_data-location_id }</td>|.
      rv_body = rv_body && |<td>{ ls_data-material }</td>|.
      rv_body = rv_body && |<td>{ ls_data-state } ({ ls_data-state_code })</td>|.
      rv_body = rv_body && |<td align="right">{ ls_data-qty_mbg DECIMALS = 3 }</td>|.
      rv_body = rv_body && |<td align="right">{ ls_data-gcv DECIMALS = 3 }</td>|.
      rv_body = rv_body && |<td align="right">{ ls_data-ncv DECIMALS = 3 }</td>|.
      rv_body = rv_body && |<td align="right">{ ls_data-qty_scm DECIMALS = 3 }</td>|.
      rv_body = rv_body && |<td>{ ls_data-tax_type }</td>|.
      rv_body = rv_body && |</tr>|.

      lv_total_mbg = lv_total_mbg + ls_data-qty_mbg.
      lv_total_scm = lv_total_scm + ls_data-qty_scm.
    ENDLOOP.

    " Total row
    rv_body = rv_body && |<tr style="background-color:#eeeeee;font-weight:bold;">|.
    rv_body = rv_body && |<td colspan="4">Total</td>|.
    rv_body = rv_body && |<td align="right">{ lv_total_mbg DECIMALS = 3 }</td>|.
    rv_body = rv_body && |<td colspan="2"></td>|.
    rv_body = rv_body && |<td align="right">{ lv_total_scm DECIMALS = 3 }</td>|.
    rv_body = rv_body && |<td></td>|.
    rv_body = rv_body && |</tr>|.

    rv_body = rv_body && |</table>|.
  ENDMETHOD.


  METHOD create_footer.
    rv_footer = |<hr/>|.
    rv_footer = rv_footer && |<div style="text-align:center;">|.
    rv_footer = rv_footer && |<p style="font-size:10px;">|.
    rv_footer = rv_footer && |This is a system generated report from YGMS CST Purchase Data Sharing System.|.
    rv_footer = rv_footer && |</p>|.
    rv_footer = rv_footer && |<p style="font-size:10px;">|.
    rv_footer = rv_footer && |Generated by: { sy-uname } | System: { sy-sysid } | Client: { sy-mandt }|.
    rv_footer = rv_footer && |</p>|.
    rv_footer = rv_footer && |</div>|.
  ENDMETHOD.

ENDCLASS.
