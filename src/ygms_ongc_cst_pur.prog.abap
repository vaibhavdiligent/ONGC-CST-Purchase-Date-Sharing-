*&---------------------------------------------------------------------*
*& Report YGMS_ONGC_CST_PUR
*& Description: ONGC CST Purchase Data Upload Program
*& Version: 2.0 - Fixed field names and text elements
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

TYPES: BEGIN OF ty_file_raw,
         line TYPE string,
       END OF ty_file_raw.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_header AS CHECKBOX DEFAULT 'X'.
  PARAMETERS: p_sep    TYPE c LENGTH 1 DEFAULT ';'.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_disp RADIOBUTTON GROUP rb1 DEFAULT 'X',
              p_save RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: gt_raw_data    TYPE TABLE OF ty_file_raw,
      gt_upload_data TYPE TABLE OF ty_upload_data,
      gv_file        TYPE string,
      gv_records     TYPE i,
      gv_errors      TYPE i.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  " Text symbols TEXT-001, TEXT-002, TEXT-003 should be maintained
  " via SE38 -> Goto -> Text Elements -> Selection Texts

*----------------------------------------------------------------------*
* At Selection Screen - F4 Help for File
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file_browse.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  " Read file
  PERFORM read_file.

  " Parse data
  PERFORM parse_data.

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
      window_title            = 'Select Upload File'
      file_filter             = 'CSV Files (*.csv)|*.csv|Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
      default_extension       = 'csv'
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
*& Form READ_FILE
*&---------------------------------------------------------------------*
FORM read_file.
  DATA: lt_data TYPE TABLE OF string.

  gv_file = p_file.

  cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename                = gv_file
      filetype                = 'ASC'
    CHANGING
      data_tab                = lt_data
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19
  ).

  IF sy-subrc <> 0.
    MESSAGE e001(ygms_msg) WITH 'Error reading file' gv_file.
  ENDIF.

  " Convert to raw data structure
  LOOP AT lt_data INTO DATA(lv_line).
    APPEND VALUE ty_file_raw( line = lv_line ) TO gt_raw_data.
  ENDLOOP.

  " Remove header if specified
  IF p_header = abap_true AND lines( gt_raw_data ) > 0.
    DELETE gt_raw_data INDEX 1.
  ENDIF.

  gv_records = lines( gt_raw_data ).
  MESSAGE s000(ygms_msg) WITH gv_records 'records read from file'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PARSE_DATA
*&---------------------------------------------------------------------*
FORM parse_data.
  DATA: lt_fields TYPE TABLE OF string,
        ls_upload TYPE ty_upload_data,
        lv_date   TYPE string.

  LOOP AT gt_raw_data INTO DATA(ls_raw).
    CLEAR: lt_fields, ls_upload.

    " Split line by separator
    SPLIT ls_raw-line AT p_sep INTO TABLE lt_fields.

    " Check minimum fields
    IF lines( lt_fields ) < 4.
      gv_errors = gv_errors + 1.
      CONTINUE.
    ENDIF.

    " Parse fields
    TRY.
        " Gas Day (convert from DD-MM-YYYY or DD.MM.YYYY to internal format)
        READ TABLE lt_fields INTO lv_date INDEX 1.
        PERFORM convert_date USING lv_date CHANGING ls_upload-gas_day.

        " Location ID
        READ TABLE lt_fields INTO ls_upload-location_id INDEX 2.
        CONDENSE ls_upload-location_id.

        " Material
        READ TABLE lt_fields INTO ls_upload-material INDEX 3.
        CONDENSE ls_upload-material.

        " Quantity in SCM
        DATA(lv_qty) = VALUE string( ).
        READ TABLE lt_fields INTO lv_qty INDEX 4.
        CONDENSE lv_qty.
        REPLACE ALL OCCURRENCES OF ',' IN lv_qty WITH '.'.
        ls_upload-qty_scm = lv_qty.

        APPEND ls_upload TO gt_upload_data.

      CATCH cx_root.
        gv_errors = gv_errors + 1.
    ENDTRY.
  ENDLOOP.

  IF gv_errors > 0.
    MESSAGE w000(ygms_msg) WITH gv_errors 'records had parsing errors'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CONVERT_DATE
*&---------------------------------------------------------------------*
FORM convert_date USING pv_date_str TYPE string
                  CHANGING pv_date TYPE datum.
  DATA: lv_day   TYPE string,
        lv_month TYPE string,
        lv_year  TYPE string,
        lt_parts TYPE TABLE OF string.

  " Try DD-MM-YYYY format
  SPLIT pv_date_str AT '-' INTO TABLE lt_parts.
  IF lines( lt_parts ) <> 3.
    " Try DD.MM.YYYY format
    SPLIT pv_date_str AT '.' INTO TABLE lt_parts.
  ENDIF.

  IF lines( lt_parts ) = 3.
    READ TABLE lt_parts INTO lv_day INDEX 1.
    READ TABLE lt_parts INTO lv_month INDEX 2.
    READ TABLE lt_parts INTO lv_year INDEX 3.

    CONDENSE: lv_day, lv_month, lv_year.

    " Pad with zeros if needed
    IF strlen( lv_day ) = 1.
      lv_day = '0' && lv_day.
    ENDIF.
    IF strlen( lv_month ) = 1.
      lv_month = '0' && lv_month.
    ENDIF.

    " Convert to YYYYMMDD
    pv_date = lv_year && lv_month && lv_day.
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

  " Generate a simple GAIL ID for this upload batch
  lv_gail_id = |UPLOAD-{ sy-datum }-{ sy-uzeit }|.

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
      MESSAGE s000(ygms_msg) WITH lines( lt_purchase ) 'records saved successfully'.
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
