*&---------------------------------------------------------------------*
*& Class: YGMS_CL_CST_EXCEL_HANDLER
*& Package: YGMS
*& Description: Excel File Upload and Parsing Handler
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

CLASS ygms_cl_cst_excel_handler DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    METHODS constructor.

    "! <p class="shorttext synchronized" lang="en">Upload Excel file</p>
    "! @parameter iv_file_path | Path to Excel file
    "! @parameter rt_data | Parsed receipt data
    "! @raising ygms_cx_cst_error | Upload failed
    METHODS upload_file
      IMPORTING
        iv_file_path   TYPE string
      RETURNING
        VALUE(rt_data) TYPE ygms_tt_receipt
      RAISING
        ygms_cx_cst_error.

    "! <p class="shorttext synchronized" lang="en">Validate single fortnight</p>
    "! @parameter it_data | Data to validate
    "! @raising ygms_cx_cst_error | Multiple fortnights found
    METHODS validate_single_fortnight
      IMPORTING
        it_data TYPE ygms_tt_receipt
      RAISING
        ygms_cx_cst_error.

    "! <p class="shorttext synchronized" lang="en">Generate Excel from purchase data</p>
    "! @parameter it_data | Purchase data
    "! @parameter rv_content | Excel file content as xstring
    METHODS generate_excel
      IMPORTING
        it_data          TYPE ygms_tt_purchase
      RETURNING
        VALUE(rv_content) TYPE xstring.

    "! <p class="shorttext synchronized" lang="en">Save content to file</p>
    "! @parameter iv_file_path | File path
    "! @parameter iv_content | File content
    "! @raising ygms_cx_cst_error | Save failed
    METHODS save_to_file
      IMPORTING
        iv_file_path TYPE string
        iv_content   TYPE xstring
      RAISING
        ygms_cx_cst_error.

  PRIVATE SECTION.
    CONSTANTS: co_header_row TYPE i VALUE 1.

    "! <p class="shorttext synchronized" lang="en">Read Excel file content</p>
    METHODS read_excel_file
      IMPORTING
        iv_file_path     TYPE string
      RETURNING
        VALUE(rv_content) TYPE xstring
      RAISING
        ygms_cx_cst_error.

    "! <p class="shorttext synchronized" lang="en">Parse Excel content to internal table</p>
    METHODS parse_excel_content
      IMPORTING
        iv_content     TYPE xstring
      RETURNING
        VALUE(rt_data) TYPE ygms_tt_receipt
      RAISING
        ygms_cx_cst_error.

    "! <p class="shorttext synchronized" lang="en">Determine fortnight from date</p>
    METHODS get_fortnight
      IMPORTING
        iv_date             TYPE datum
      RETURNING
        VALUE(rs_fortnight) TYPE ygms_if_cst_constants=>ty_fortnight.

ENDCLASS.


CLASS ygms_cl_cst_excel_handler IMPLEMENTATION.

  METHOD constructor.
    " No initialization required
  ENDMETHOD.


  METHOD upload_file.
    " Read Excel file content
    DATA(lv_content) = read_excel_file( iv_file_path ).

    " Parse Excel content
    rt_data = parse_excel_content( lv_content ).
  ENDMETHOD.


  METHOD validate_single_fortnight.
    DATA: ls_first_fortnight TYPE ygms_if_cst_constants=>ty_fortnight,
          ls_fortnight       TYPE ygms_if_cst_constants=>ty_fortnight.

    " Get fortnight from first record
    READ TABLE it_data INTO DATA(ls_first) INDEX 1.
    IF sy-subrc = 0.
      ls_first_fortnight = get_fortnight( ls_first-gas_day ).
    ENDIF.

    " Check all records are in same fortnight
    LOOP AT it_data INTO DATA(ls_data) FROM 2.
      ls_fortnight = get_fortnight( ls_data-gas_day ).

      IF ls_fortnight <> ls_first_fortnight.
        RAISE EXCEPTION TYPE ygms_cx_cst_error
          EXPORTING
            textid = ygms_cx_cst_error=>multi_fortnight.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD generate_excel.
    " Generate Excel file using cl_fdt_xl_spreadsheet or similar

    TRY.
        " Create spreadsheet
        DATA: lo_excel     TYPE REF TO cl_fdt_xl_spreadsheet,
              lt_data      TYPE REF TO data.

        " Convert to table reference
        GET REFERENCE OF it_data INTO lt_data.

        " Create Excel
        CREATE OBJECT lo_excel
          EXPORTING
            document_name = 'ONGC_CST_Purchase_Data'.

        " Get workbook as xstring
        lo_excel->if_fdt_doc_spreadsheet~get_raw_data(
          IMPORTING
            r_data = rv_content
        ).

      CATCH cx_root INTO DATA(lx_error).
        " If cl_fdt_xl_spreadsheet fails, use alternative method
        " Create simple CSV format as fallback
        DATA: lv_csv TYPE string.

        " Header row
        lv_csv = |GAS_DAY,LOCATION_ID,MATERIAL,STATE_CODE,STATE,QTY_MBG,GCV,NCV,QTY_SCM,TAX_TYPE|.

        LOOP AT it_data INTO DATA(ls_data).
          lv_csv = lv_csv && cl_abap_char_utilities=>cr_lf &&
                   |{ ls_data-gas_day },{ ls_data-location_id },{ ls_data-material },| &&
                   |{ ls_data-state_code },{ ls_data-state },{ ls_data-qty_mbg },| &&
                   |{ ls_data-gcv },{ ls_data-ncv },{ ls_data-qty_scm },{ ls_data-tax_type }|.
        ENDLOOP.

        " Convert to xstring
        rv_content = cl_abap_codepage=>convert_to( lv_csv ).
    ENDTRY.
  ENDMETHOD.


  METHOD save_to_file.
    DATA: lt_data    TYPE solix_tab,
          lv_size    TYPE i.

    " Convert xstring to table
    lt_data = cl_bcs_convert=>xstring_to_solix( iv_content ).
    lv_size = xstrlen( iv_content ).

    " Download file
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        bin_filesize = lv_size
        filename     = iv_file_path
        filetype     = 'BIN'
      CHANGING
        data_tab     = lt_data
      EXCEPTIONS
        OTHERS       = 1
    ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ygms_cx_cst_error
        EXPORTING
          textid    = ygms_cx_cst_error=>upload_failed
          mv_param1 = |File save error: { sy-subrc }|.
    ENDIF.
  ENDMETHOD.


  METHOD read_excel_file.
    DATA: lt_data TYPE solix_tab,
          lv_size TYPE i.

    " Upload file from presentation server
    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename   = iv_file_path
        filetype   = 'BIN'
      IMPORTING
        filelength = lv_size
      CHANGING
        data_tab   = lt_data
      EXCEPTIONS
        OTHERS     = 1
    ).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ygms_cx_cst_error
        EXPORTING
          textid    = ygms_cx_cst_error=>upload_failed
          mv_param1 = |File read error: { sy-subrc }|.
    ENDIF.

    " Convert to xstring
    rv_content = cl_bcs_convert=>solix_to_xstring( lt_data ).
  ENDMETHOD.


  METHOD parse_excel_content.
    " Parse Excel content using cl_fdt_xl_spreadsheet or similar

    TRY.
        DATA: lo_excel TYPE REF TO cl_fdt_xl_spreadsheet.

        " Create spreadsheet object from content
        lo_excel = NEW cl_fdt_xl_spreadsheet(
          document_name = 'Upload'
          xdocument     = iv_content
        ).

        " Get worksheet names
        DATA(lt_worksheets) = lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names( ).

        " Read first worksheet
        READ TABLE lt_worksheets INTO DATA(lv_worksheet) INDEX 1.
        IF sy-subrc = 0.
          " Get worksheet data
          DATA: lr_data TYPE REF TO data.

          lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
            EXPORTING
              worksheet_name = lv_worksheet
            IMPORTING
              itab           = lr_data
          ).

          " Process data
          FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.

          ASSIGN lr_data->* TO <lt_data>.
          IF sy-subrc = 0.
            LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_row>) FROM 2.  " Skip header
              " Extract fields from row
              DATA(ls_receipt) = VALUE ygms_s_receipt( ).

              ASSIGN COMPONENT 1 OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<fv_gas_day>).
              ASSIGN COMPONENT 2 OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<fv_ctp_id>).
              ASSIGN COMPONENT 3 OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<fv_ongc_mat>).
              ASSIGN COMPONENT 4 OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<fv_qty_scm>).
              ASSIGN COMPONENT 5 OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<fv_qty_mbg>).
              ASSIGN COMPONENT 6 OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<fv_ongc_id>).

              IF <fv_gas_day> IS ASSIGNED.
                ls_receipt-gas_day = <fv_gas_day>.
              ENDIF.
              IF <fv_ctp_id> IS ASSIGNED.
                ls_receipt-ctp_id = <fv_ctp_id>.
              ENDIF.
              IF <fv_ongc_mat> IS ASSIGNED.
                ls_receipt-ongc_material = <fv_ongc_mat>.
              ENDIF.
              IF <fv_qty_scm> IS ASSIGNED.
                ls_receipt-qty_scm = <fv_qty_scm>.
              ENDIF.
              IF <fv_qty_mbg> IS ASSIGNED.
                ls_receipt-qty_mbg = <fv_qty_mbg>.
              ENDIF.
              IF <fv_ongc_id> IS ASSIGNED.
                ls_receipt-ongc_id = <fv_ongc_id>.
              ENDIF.

              IF ls_receipt-gas_day IS NOT INITIAL.
                APPEND ls_receipt TO rt_data.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE ygms_cx_cst_error
          EXPORTING
            textid    = ygms_cx_cst_error=>upload_failed
            mv_param1 = lx_error->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_fortnight.
    DATA(lv_day) = iv_date+6(2).

    IF lv_day <= 15.
      " First fortnight: Day 1-15
      rs_fortnight-from_date = iv_date(6) && '01'.
      rs_fortnight-to_date   = iv_date(6) && '15'.
    ELSE.
      " Second fortnight: Day 16 - End of month
      rs_fortnight-from_date = iv_date(6) && '16'.

      " Calculate end of month
      DATA(lv_next_month) = iv_date.
      lv_next_month+6(2) = '01'.
      lv_next_month = lv_next_month + 32.
      lv_next_month+6(2) = '01'.
      rs_fortnight-to_date = lv_next_month - 1.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
