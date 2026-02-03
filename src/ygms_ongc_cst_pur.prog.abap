*&---------------------------------------------------------------------*
*& Report YGMS_ONGC_CST_PUR
*& Description: ONGC CST Purchase Data Upload Program (Excel)
*& Version: 3.0 - Excel upload support
*&---------------------------------------------------------------------*
REPORT ygms_ongc_cst_pur.

*----------------------------------------------------------------------*
* Type Definitions
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_upload_data,
         gas_day       TYPE datum,
         location_id   TYPE ygms_de_loc_id,
         material      TYPE ygms_de_gail_mat,
         qty_scm       TYPE ygms_de_qty_scm,
       END OF ty_upload_data.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_header AS CHECKBOX DEFAULT 'X'.
  PARAMETERS: p_sheet  TYPE i DEFAULT 1.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_disp RADIOBUTTON GROUP rb1 DEFAULT 'X',
              p_save RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: gt_upload_data TYPE TABLE OF ty_upload_data,
      gt_excel_data  TYPE TABLE OF alsmex_tabline,
      gv_file        TYPE string,
      gv_records     TYPE i,
      gv_errors      TYPE i.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  " Text symbols should be maintained via SE38 -> Text Elements

*----------------------------------------------------------------------*
* At Selection Screen - F4 Help for File
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file_browse.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  " Read Excel file
  PERFORM read_excel_file.

  " Parse Excel data
  PERFORM parse_excel_data.

  " Validate data
  PERFORM validate_data.

  " Process based on option
  IF p_disp = abap_true.
    PERFORM display_alv.
  ELSE.
    PERFORM save_data.
    PERFORM display_alv.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form F4_FILE_BROWSE
*&---------------------------------------------------------------------*
FORM f4_file_browse.
  DATA: lt_filetable TYPE filetable,
        lv_rc        TYPE i,
        lv_action    TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title            = 'Select Excel File'
      file_filter             = 'Excel Files (*.xlsx;*.xls)|*.xlsx;*.xls|All Files (*.*)|*.*'
      default_extension       = 'xlsx'
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
      user_action             = lv_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5
  ).

  IF sy-subrc = 0 AND lv_action = cl_gui_frontend_services=>action_ok.
    READ TABLE lt_filetable INTO DATA(ls_file) INDEX 1.
    IF sy-subrc = 0.
      p_file = ls_file-filename.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form READ_EXCEL_FILE
*&---------------------------------------------------------------------*
FORM read_excel_file.
  DATA: lv_start_row TYPE i VALUE 1,
        lv_start_col TYPE i VALUE 1,
        lv_end_row   TYPE i VALUE 9999,
        lv_end_col   TYPE i VALUE 4.

  " Skip header row if checkbox is selected
  IF p_header = abap_true.
    lv_start_row = 2.
  ENDIF.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = lv_start_col
      i_begin_row             = lv_start_row
      i_end_col               = lv_end_col
      i_end_row               = lv_end_row
    TABLES
      intern                  = gt_excel_data
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    MESSAGE e001(ygms_msg) WITH 'Error reading Excel file' p_file.
  ENDIF.

  " Count unique rows
  DATA(lt_rows) = VALUE rseloption( ).
  LOOP AT gt_excel_data INTO DATA(ls_excel).
    COLLECT VALUE rsdsselopt( sign = 'I' option = 'EQ' low = CONV #( ls_excel-row ) ) INTO lt_rows.
  ENDLOOP.
  gv_records = lines( lt_rows ).

  MESSAGE s000(ygms_msg) WITH gv_records 'rows read from Excel file'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PARSE_EXCEL_DATA
*&---------------------------------------------------------------------*
FORM parse_excel_data.
  DATA: ls_upload    TYPE ty_upload_data,
        lv_curr_row  TYPE i,
        lv_prev_row  TYPE i,
        lv_date_str  TYPE string.

  SORT gt_excel_data BY row col.

  LOOP AT gt_excel_data INTO DATA(ls_excel).
    " Check if new row
    IF ls_excel-row <> lv_prev_row.
      " Save previous row if exists
      IF lv_prev_row > 0 AND ls_upload IS NOT INITIAL.
        APPEND ls_upload TO gt_upload_data.
      ENDIF.
      CLEAR ls_upload.
      lv_prev_row = ls_excel-row.
    ENDIF.

    " Parse based on column
    TRY.
        CASE ls_excel-col.
          WHEN 1.  " Gas Day
            lv_date_str = ls_excel-value.
            PERFORM convert_date USING lv_date_str CHANGING ls_upload-gas_day.
          WHEN 2.  " Location ID
            ls_upload-location_id = ls_excel-value.
            CONDENSE ls_upload-location_id.
          WHEN 3.  " Material
            ls_upload-material = ls_excel-value.
            CONDENSE ls_upload-material.
          WHEN 4.  " Quantity in SCM
            DATA(lv_qty) = ls_excel-value.
            REPLACE ALL OCCURRENCES OF ',' IN lv_qty WITH '.'.
            ls_upload-qty_scm = lv_qty.
        ENDCASE.
      CATCH cx_root.
        gv_errors = gv_errors + 1.
    ENDTRY.
  ENDLOOP.

  " Append last row
  IF ls_upload IS NOT INITIAL.
    APPEND ls_upload TO gt_upload_data.
  ENDIF.

  IF gv_errors > 0.
    MESSAGE w000(ygms_msg) WITH gv_errors 'cells had parsing errors'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CONVERT_DATE
*&---------------------------------------------------------------------*
FORM convert_date USING pv_date_str TYPE string
                  CHANGING pv_date TYPE datum.
  DATA: lv_day      TYPE string,
        lv_month    TYPE string,
        lv_year     TYPE string,
        lv_date_out TYPE string,
        lt_parts    TYPE TABLE OF string.

  " Check if already in internal format (YYYYMMDD or number)
  IF strlen( pv_date_str ) = 8 AND pv_date_str CO '0123456789'.
    pv_date = pv_date_str.
    RETURN.
  ENDIF.

  " Check for Excel serial date number
  IF pv_date_str CO '0123456789'.
    DATA(lv_serial) = CONV i( pv_date_str ).
    IF lv_serial > 0.
      " Excel date serial: days since 1899-12-30
      pv_date = '18991230'.
      pv_date = pv_date + lv_serial.
      RETURN.
    ENDIF.
  ENDIF.

  " Try DD-MM-YYYY format
  SPLIT pv_date_str AT '-' INTO TABLE lt_parts.
  IF lines( lt_parts ) <> 3.
    " Try DD.MM.YYYY format
    SPLIT pv_date_str AT '.' INTO TABLE lt_parts.
  ENDIF.
  IF lines( lt_parts ) <> 3.
    " Try DD/MM/YYYY format
    SPLIT pv_date_str AT '/' INTO TABLE lt_parts.
  ENDIF.

  IF lines( lt_parts ) = 3.
    READ TABLE lt_parts INTO lv_day INDEX 1.
    READ TABLE lt_parts INTO lv_month INDEX 2.
    READ TABLE lt_parts INTO lv_year INDEX 3.

    CONDENSE: lv_day, lv_month, lv_year.

    " Pad with zeros if needed
    IF strlen( lv_day ) = 1.
      CONCATENATE '0' lv_day INTO lv_day.
    ENDIF.
    IF strlen( lv_month ) = 1.
      CONCATENATE '0' lv_month INTO lv_month.
    ENDIF.

    " Convert to YYYYMMDD
    CONCATENATE lv_year lv_month lv_day INTO lv_date_out.
    pv_date = lv_date_out.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATE_DATA
*&---------------------------------------------------------------------*
FORM validate_data.
  DATA: lv_valid_count TYPE i,
        lv_error_count TYPE i.

  LOOP AT gt_upload_data INTO DATA(ls_data).
    DATA(lv_valid) = abap_true.

    " Validate Gas Day
    IF ls_data-gas_day IS INITIAL OR ls_data-gas_day < '19000101'.
      lv_valid = abap_false.
    ENDIF.

    " Validate Location ID
    IF ls_data-location_id IS INITIAL.
      lv_valid = abap_false.
    ENDIF.

    " Validate Material
    IF ls_data-material IS INITIAL.
      lv_valid = abap_false.
    ENDIF.

    " Validate Quantity
    IF ls_data-qty_scm IS INITIAL OR ls_data-qty_scm <= 0.
      lv_valid = abap_false.
    ENDIF.

    IF lv_valid = abap_true.
      lv_valid_count = lv_valid_count + 1.
    ELSE.
      lv_error_count = lv_error_count + 1.
    ENDIF.
  ENDLOOP.

  MESSAGE s000(ygms_msg) WITH lv_valid_count 'valid records,' lv_error_count 'invalid'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SAVE_DATA
*&---------------------------------------------------------------------*
FORM save_data.
  DATA: lt_purchase TYPE TABLE OF ygms_cst_pur,
        ls_purchase TYPE ygms_cst_pur,
        lv_gail_id  TYPE ygms_de_gail_id.

  " Generate a GAIL ID for this upload batch
  CONCATENATE 'UPLOAD-' sy-datum '-' sy-uzeit INTO lv_gail_id.

  LOOP AT gt_upload_data INTO DATA(ls_data).
    CLEAR ls_purchase.

    ls_purchase-mandt       = sy-mandt.
    ls_purchase-gail_id     = lv_gail_id.
    ls_purchase-gas_day     = ls_data-gas_day.
    ls_purchase-location_id = ls_data-location_id.
    ls_purchase-material    = ls_data-material.
    ls_purchase-qty_scm     = ls_data-qty_scm.
    ls_purchase-ernam       = sy-uname.
    ls_purchase-erdat       = sy-datum.
    ls_purchase-erzet       = sy-uzeit.

    APPEND ls_purchase TO lt_purchase.
  ENDLOOP.

  IF lt_purchase IS NOT INITIAL.
    MODIFY ygms_cst_pur FROM TABLE lt_purchase.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      DATA(lv_count) = lines( lt_purchase ).
      MESSAGE s000(ygms_msg) WITH lv_count 'records saved successfully'.
    ELSE.
      ROLLBACK WORK.
      MESSAGE e001(ygms_msg) WITH 'Error saving data to database'.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM display_alv.
  DATA: lo_alv       TYPE REF TO cl_salv_table,
        lo_functions TYPE REF TO cl_salv_functions_list,
        lo_columns   TYPE REF TO cl_salv_columns_table,
        lo_column    TYPE REF TO cl_salv_column.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = gt_upload_data
      ).

      " Enable all functions
      lo_functions = lo_alv->get_functions( ).
      lo_functions->set_all( abap_true ).

      " Set column texts
      lo_columns = lo_alv->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      TRY.
          lo_column = lo_columns->get_column( 'GAS_DAY' ).
          lo_column->set_short_text( 'Gas Day' ).
          lo_column->set_medium_text( 'Gas Day' ).
          lo_column->set_long_text( 'Gas Day' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          lo_column = lo_columns->get_column( 'LOCATION_ID' ).
          lo_column->set_short_text( 'Location' ).
          lo_column->set_medium_text( 'Location ID' ).
          lo_column->set_long_text( 'Location ID' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          lo_column = lo_columns->get_column( 'MATERIAL' ).
          lo_column->set_short_text( 'Material' ).
          lo_column->set_medium_text( 'Material' ).
          lo_column->set_long_text( 'Material' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          lo_column = lo_columns->get_column( 'QTY_SCM' ).
          lo_column->set_short_text( 'Qty SCM' ).
          lo_column->set_medium_text( 'Quantity SCM' ).
          lo_column->set_long_text( 'Quantity in SCM' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      " Display
      lo_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx_salv).
      MESSAGE lx_salv TYPE 'E'.
  ENDTRY.
ENDFORM.
