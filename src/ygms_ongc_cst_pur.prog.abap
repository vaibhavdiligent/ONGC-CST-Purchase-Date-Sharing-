*&---------------------------------------------------------------------*
*& Report YGMS_ONGC_CST_PUR
*& Description: ONGC CST Purchase Data Upload Program (Excel)
*& Version: 5.2 - Added GCV/NCV fetch from YRXA_CMDATA
*&---------------------------------------------------------------------*
REPORT ygms_ongc_cst_pur.

*----------------------------------------------------------------------*
* Type Definitions
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_excel_data,
         gas_day       TYPE datum,
         location_id   TYPE ygms_de_loc_id,
         material      TYPE ygms_de_gail_mat,
         qty_scm       TYPE ygms_de_qty_scm,
       END OF ty_excel_data.

TYPES: BEGIN OF ty_upload_data,
         gas_day       TYPE datum,
         location_id   TYPE ygms_de_loc_id,
         ctp_id        TYPE ygms_de_ongc_ctp,
         material      TYPE ygms_de_gail_mat,
         ongc_material TYPE ygms_de_ongc_mat,
         qty_scm       TYPE ygms_de_qty_scm,
         gcv           TYPE ygms_de_gcv,
         ncv           TYPE ygms_de_ncv,
       END OF ty_upload_data.

TYPES: BEGIN OF ty_view_data,
         gas_day       TYPE datum,
         ctp_id        TYPE ygms_de_ongc_ctp,
         ongc_material TYPE ygms_de_ongc_mat,
         qty_scm       TYPE ygms_de_qty_scm,
         gcv           TYPE ygms_de_gcv,
         ncv           TYPE ygms_de_ncv,
       END OF ty_view_data.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_upload RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND mode,
              p_view   RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_file TYPE rlgrap-filename MODIF ID upl.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS:     p_ctp    TYPE ygms_de_ongc_ctp MODIF ID viw.
  SELECT-OPTIONS: s_gasday FOR sy-datum MODIF ID viw.
  PARAMETERS:     p_mat    TYPE ygms_de_ongc_mat MODIF ID viw.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: gt_excel_data  TYPE TABLE OF ty_excel_data,
      gt_upload_data TYPE TABLE OF ty_upload_data,
      gt_view_data   TYPE TABLE OF ty_view_data,
      gt_alsmex_data TYPE TABLE OF alsmex_tabline,
      gv_fn_start    TYPE datum,
      gv_fn_end      TYPE datum,
      gv_records     TYPE i,
      gv_errors      TYPE i,
      gv_b2b_mode    TYPE abap_bool VALUE abap_false.

*----------------------------------------------------------------------*
* At Selection Screen Output
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_upload = abap_true.
      IF screen-group1 = 'VIW'.
        screen-active = 0.
      ENDIF.
    ELSE.
      IF screen-group1 = 'UPL'.
        screen-active = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

*----------------------------------------------------------------------*
* At Selection Screen - F4 Help for File
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file_browse.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF p_upload = abap_true.
    PERFORM process_upload.
  ELSE.
    PERFORM process_view.
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
*& Form PROCESS_UPLOAD
*&---------------------------------------------------------------------*
FORM process_upload.
  " Step 1: Read Excel file
  PERFORM read_excel_file.
  CHECK gt_excel_data IS NOT INITIAL.

  " Step 2: Validate fortnight (single fortnight only)
  PERFORM validate_fortnight.
  CHECK gv_fn_start IS NOT INITIAL.

  " Step 3: Map Location ID to CTP ID
  PERFORM map_location_to_ctp.
  CHECK gv_errors = 0.

  " Step 4: Map Material to ONGC Material
  PERFORM map_material_to_ongc.
  CHECK gv_errors = 0.

  " Step 5: Fetch GCV/NCV from YRXA_CMDATA
  PERFORM fetch_gcv_ncv.
  CHECK gv_errors = 0.

  " Step 6: Check existing B2B data
  PERFORM check_existing_b2b_data.

  " Step 7: Save data
  PERFORM save_data.

  " Step 8: Display results
  PERFORM display_alv.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_VIEW
*&---------------------------------------------------------------------*
FORM process_view.
  " Select from YRGA_CST_B2B_1 with available columns
  IF p_ctp IS NOT INITIAL AND s_gasday[] IS NOT INITIAL AND p_mat IS NOT INITIAL.
    SELECT gas_day ctp_id ongc_material qty_scm gcv ncv
      FROM yrga_cst_b2b_1
      INTO TABLE gt_view_data
      WHERE ctp_id = p_ctp
        AND gas_day IN s_gasday
        AND ongc_material = p_mat.
  ELSEIF p_ctp IS NOT INITIAL AND s_gasday[] IS NOT INITIAL.
    SELECT gas_day ctp_id ongc_material qty_scm gcv ncv
      FROM yrga_cst_b2b_1
      INTO TABLE gt_view_data
      WHERE ctp_id = p_ctp
        AND gas_day IN s_gasday.
  ELSEIF p_ctp IS NOT INITIAL AND p_mat IS NOT INITIAL.
    SELECT gas_day ctp_id ongc_material qty_scm gcv ncv
      FROM yrga_cst_b2b_1
      INTO TABLE gt_view_data
      WHERE ctp_id = p_ctp
        AND ongc_material = p_mat.
  ELSEIF s_gasday[] IS NOT INITIAL AND p_mat IS NOT INITIAL.
    SELECT gas_day ctp_id ongc_material qty_scm gcv ncv
      FROM yrga_cst_b2b_1
      INTO TABLE gt_view_data
      WHERE gas_day IN s_gasday
        AND ongc_material = p_mat.
  ELSEIF p_ctp IS NOT INITIAL.
    SELECT gas_day ctp_id ongc_material qty_scm gcv ncv
      FROM yrga_cst_b2b_1
      INTO TABLE gt_view_data
      WHERE ctp_id = p_ctp.
  ELSEIF s_gasday[] IS NOT INITIAL.
    SELECT gas_day ctp_id ongc_material qty_scm gcv ncv
      FROM yrga_cst_b2b_1
      INTO TABLE gt_view_data
      WHERE gas_day IN s_gasday.
  ELSEIF p_mat IS NOT INITIAL.
    SELECT gas_day ctp_id ongc_material qty_scm gcv ncv
      FROM yrga_cst_b2b_1
      INTO TABLE gt_view_data
      WHERE ongc_material = p_mat.
  ELSE.
    MESSAGE s000(ygms_msg) WITH 'Please enter at least one selection criterion'.
    RETURN.
  ENDIF.

  IF gt_view_data IS INITIAL.
    MESSAGE s000(ygms_msg) WITH 'No data found for selection criteria'.
    RETURN.
  ENDIF.

  PERFORM display_view_alv.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form READ_EXCEL_FILE
*&---------------------------------------------------------------------*
FORM read_excel_file.
  DATA: lv_start_row TYPE i VALUE 2,
        lv_start_col TYPE i VALUE 1,
        lv_end_row   TYPE i VALUE 9999,
        lv_end_col   TYPE i VALUE 4.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = lv_start_col
      i_begin_row             = lv_start_row
      i_end_col               = lv_end_col
      i_end_row               = lv_end_row
    TABLES
      intern                  = gt_alsmex_data
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    MESSAGE e001(ygms_msg) WITH 'Error reading Excel file' p_file.
    RETURN.
  ENDIF.

  " Parse Excel data
  PERFORM parse_excel_data.

  gv_records = lines( gt_excel_data ).
  MESSAGE s000(ygms_msg) WITH gv_records 'rows read from Excel file'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PARSE_EXCEL_DATA
*&---------------------------------------------------------------------*
FORM parse_excel_data.
  DATA: ls_excel     TYPE ty_excel_data,
        lv_prev_row  TYPE i,
        lv_date_str  TYPE string,
        lv_qty_str   TYPE string.

  SORT gt_alsmex_data BY row col.

  LOOP AT gt_alsmex_data INTO DATA(ls_alsmex).
    IF ls_alsmex-row <> lv_prev_row.
      IF lv_prev_row > 0 AND ls_excel IS NOT INITIAL.
        APPEND ls_excel TO gt_excel_data.
      ENDIF.
      CLEAR ls_excel.
      lv_prev_row = ls_alsmex-row.
    ENDIF.

    TRY.
        CASE ls_alsmex-col.
          WHEN 1.
            lv_date_str = ls_alsmex-value.
            PERFORM convert_date USING lv_date_str CHANGING ls_excel-gas_day.
          WHEN 2.
            ls_excel-location_id = ls_alsmex-value.
            CONDENSE ls_excel-location_id.
          WHEN 3.
            ls_excel-material = ls_alsmex-value.
            CONDENSE ls_excel-material.
          WHEN 4.
            lv_qty_str = ls_alsmex-value.
            REPLACE ALL OCCURRENCES OF ',' IN lv_qty_str WITH '.'.
            ls_excel-qty_scm = lv_qty_str.
        ENDCASE.
      CATCH cx_root.
        gv_errors = gv_errors + 1.
    ENDTRY.
  ENDLOOP.

  IF ls_excel IS NOT INITIAL.
    APPEND ls_excel TO gt_excel_data.
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
        lt_parts    TYPE TABLE OF string,
        lv_serial   TYPE i.

  IF strlen( pv_date_str ) = 8 AND pv_date_str CO '0123456789'.
    pv_date = pv_date_str.
    RETURN.
  ENDIF.

  IF pv_date_str CO '0123456789'.
    lv_serial = pv_date_str.
    IF lv_serial > 0.
      pv_date = '18991230'.
      pv_date = pv_date + lv_serial.
      RETURN.
    ENDIF.
  ENDIF.

  SPLIT pv_date_str AT '-' INTO TABLE lt_parts.
  IF lines( lt_parts ) <> 3.
    SPLIT pv_date_str AT '.' INTO TABLE lt_parts.
  ENDIF.
  IF lines( lt_parts ) <> 3.
    SPLIT pv_date_str AT '/' INTO TABLE lt_parts.
  ENDIF.

  IF lines( lt_parts ) = 3.
    READ TABLE lt_parts INTO lv_day INDEX 1.
    READ TABLE lt_parts INTO lv_month INDEX 2.
    READ TABLE lt_parts INTO lv_year INDEX 3.

    CONDENSE: lv_day, lv_month, lv_year.

    IF strlen( lv_day ) = 1.
      CONCATENATE '0' lv_day INTO lv_day.
    ENDIF.
    IF strlen( lv_month ) = 1.
      CONCATENATE '0' lv_month INTO lv_month.
    ENDIF.

    CONCATENATE lv_year lv_month lv_day INTO lv_date_out.
    pv_date = lv_date_out.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATE_FORTNIGHT
*&---------------------------------------------------------------------*
FORM validate_fortnight.
  DATA: lv_min_date   TYPE datum,
        lv_max_date   TYPE datum,
        lv_fn1_start  TYPE datum,
        lv_fn1_end    TYPE datum,
        lv_next_month TYPE datum,
        lv_day        TYPE c LENGTH 2.

  " Find min and max dates in uploaded data
  LOOP AT gt_excel_data INTO DATA(ls_excel).
    IF lv_min_date IS INITIAL OR ls_excel-gas_day < lv_min_date.
      lv_min_date = ls_excel-gas_day.
    ENDIF.
    IF ls_excel-gas_day > lv_max_date.
      lv_max_date = ls_excel-gas_day.
    ENDIF.
  ENDLOOP.

  " Calculate fortnight dates
  " First fortnight: 1st to 15th of month
  " Second fortnight: 16th to end of month
  lv_day = lv_min_date+6(2).

  IF lv_day <= '15'.
    " First fortnight
    CONCATENATE lv_min_date(6) '01' INTO lv_fn1_start.
    CONCATENATE lv_min_date(6) '15' INTO lv_fn1_end.
  ELSE.
    " Second fortnight
    CONCATENATE lv_min_date(6) '16' INTO lv_fn1_start.
    " Get last day of month
    lv_next_month = lv_min_date.
    lv_next_month+6(2) = '01'.
    lv_next_month = lv_next_month + 32.
    lv_next_month+6(2) = '01'.
    lv_fn1_end = lv_next_month - 1.
  ENDIF.

  " Check if max date is within the same fortnight
  IF lv_max_date < lv_fn1_start OR lv_max_date > lv_fn1_end.
    MESSAGE e001(ygms_msg) WITH 'Data spans multiple fortnights. Only one fortnight allowed.'.
    CLEAR: gv_fn_start, gv_fn_end.
    RETURN.
  ENDIF.

  gv_fn_start = lv_fn1_start.
  gv_fn_end   = lv_fn1_end.

  MESSAGE s000(ygms_msg) WITH 'Fortnight:' gv_fn_start 'to' gv_fn_end.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form MAP_LOCATION_TO_CTP
*&---------------------------------------------------------------------*
FORM map_location_to_ctp.
  DATA: lt_loc_map   TYPE TABLE OF yrga_cst_loc_map,
        lt_locations TYPE TABLE OF ygms_de_loc_id,
        ls_upload    TYPE ty_upload_data,
        lv_error     TYPE abap_bool.

  " Get unique location IDs
  LOOP AT gt_excel_data INTO DATA(ls_excel).
    COLLECT ls_excel-location_id INTO lt_locations.
  ENDLOOP.

  " Fetch location mappings using FOR ALL ENTRIES
  IF lt_locations IS NOT INITIAL.
    SELECT * FROM yrga_cst_loc_map
      INTO TABLE lt_loc_map
      FOR ALL ENTRIES IN lt_locations
      WHERE gail_loc_id = lt_locations-table_line
        AND valid_from <= gv_fn_start
        AND valid_to   >= gv_fn_end
        AND deleted    = abap_false.
  ENDIF.

  " Map each record
  LOOP AT gt_excel_data INTO ls_excel.
    CLEAR ls_upload.
    ls_upload-gas_day     = ls_excel-gas_day.
    ls_upload-location_id = ls_excel-location_id.
    ls_upload-material    = ls_excel-material.
    ls_upload-qty_scm     = ls_excel-qty_scm.

    READ TABLE lt_loc_map INTO DATA(ls_loc)
      WITH KEY gail_loc_id = ls_excel-location_id.
    IF sy-subrc = 0.
      ls_upload-ctp_id = ls_loc-ongc_ctp_id.
    ELSE.
      MESSAGE e001(ygms_msg) WITH 'No CTP mapping for Location:' ls_excel-location_id.
      lv_error = abap_true.
      gv_errors = gv_errors + 1.
      CONTINUE.
    ENDIF.

    APPEND ls_upload TO gt_upload_data.
  ENDLOOP.

  IF lv_error = abap_true.
    MESSAGE e001(ygms_msg) WITH 'Location mapping errors found. Upload aborted.'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form MAP_MATERIAL_TO_ONGC
*&---------------------------------------------------------------------*
FORM map_material_to_ongc.
  DATA: lt_mat_map TYPE TABLE OF yrga_cst_mat_map,
        lv_error   TYPE abap_bool.

  DATA: BEGIN OF ls_loc_mat,
          location_id TYPE ygms_de_loc_id,
          material    TYPE ygms_de_gail_mat,
        END OF ls_loc_mat,
        lt_loc_mat LIKE TABLE OF ls_loc_mat.

  LOOP AT gt_upload_data INTO DATA(ls_data).
    ls_loc_mat-location_id = ls_data-location_id.
    ls_loc_mat-material    = ls_data-material.
    COLLECT ls_loc_mat INTO lt_loc_mat.
  ENDLOOP.

  " Fetch material mappings
  IF lt_loc_mat IS NOT INITIAL.
    SELECT * FROM yrga_cst_mat_map
      INTO TABLE lt_mat_map
      FOR ALL ENTRIES IN lt_loc_mat
      WHERE location_id    = lt_loc_mat-location_id
        AND gail_material  = lt_loc_mat-material
        AND valid_from    <= gv_fn_start
        AND valid_to      >= gv_fn_end
        AND deleted        = abap_false.
  ENDIF.

  " Map materials
  LOOP AT gt_upload_data ASSIGNING FIELD-SYMBOL(<fs_upload>).
    READ TABLE lt_mat_map INTO DATA(ls_mat)
      WITH KEY location_id   = <fs_upload>-location_id
               gail_material = <fs_upload>-material.
    IF sy-subrc = 0.
      <fs_upload>-ongc_material = ls_mat-ongc_material.
    ELSE.
      MESSAGE e001(ygms_msg) WITH 'No ONGC Material mapping for:' <fs_upload>-material.
      lv_error = abap_true.
      gv_errors = gv_errors + 1.
    ENDIF.
  ENDLOOP.

  IF lv_error = abap_true.
    MESSAGE e001(ygms_msg) WITH 'Material mapping errors found. Upload aborted.'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FETCH_GCV_NCV
*&---------------------------------------------------------------------*
FORM fetch_gcv_ncv.
  DATA: lv_error    TYPE abap_bool,
        lv_gcv      TYPE ygms_de_gcv,
        lv_ncv      TYPE ygms_de_ncv,
        lv_err_cnt  TYPE i.

  DATA: BEGIN OF ls_loc_day,
          location_id TYPE ygms_de_loc_id,
          gas_day     TYPE datum,
        END OF ls_loc_day,
        lt_loc_day LIKE TABLE OF ls_loc_day.

  DATA: BEGIN OF ls_cmdata,
          location_id TYPE ygms_de_loc_id,
          gas_day     TYPE datum,
          gcv         TYPE ygms_de_gcv,
          ncv         TYPE ygms_de_ncv,
        END OF ls_cmdata,
        lt_cmdata LIKE TABLE OF ls_cmdata.

  " Get unique Location ID - Gas Day combinations
  LOOP AT gt_upload_data INTO DATA(ls_data).
    ls_loc_day-location_id = ls_data-location_id.
    ls_loc_day-gas_day     = ls_data-gas_day.
    COLLECT ls_loc_day INTO lt_loc_day.
  ENDLOOP.

  " Fetch GCV/NCV from YRXA_CMDATA
  IF lt_loc_day IS NOT INITIAL.
    SELECT location_id gas_day gcv ncv
      FROM yrxa_cmdata
      INTO TABLE lt_cmdata
      FOR ALL ENTRIES IN lt_loc_day
      WHERE location_id = lt_loc_day-location_id
        AND gas_day     = lt_loc_day-gas_day.
  ENDIF.

  " Update GCV/NCV in upload data and validate
  LOOP AT gt_upload_data ASSIGNING FIELD-SYMBOL(<fs_upload>).
    CLEAR: lv_gcv, lv_ncv.

    READ TABLE lt_cmdata INTO ls_cmdata
      WITH KEY location_id = <fs_upload>-location_id
               gas_day     = <fs_upload>-gas_day.

    IF sy-subrc = 0.
      lv_gcv = ls_cmdata-gcv.
      lv_ncv = ls_cmdata-ncv.
    ENDIF.

    " Validate GCV
    IF lv_gcv IS INITIAL OR lv_gcv <= 0.
      MESSAGE e001(ygms_msg) WITH 'GCV not available for' <fs_upload>-location_id <fs_upload>-gas_day.
      lv_error = abap_true.
      lv_err_cnt = lv_err_cnt + 1.
      CONTINUE.
    ENDIF.

    " Validate NCV
    IF lv_ncv IS INITIAL OR lv_ncv <= 0.
      MESSAGE e001(ygms_msg) WITH 'NCV not available for' <fs_upload>-location_id <fs_upload>-gas_day.
      lv_error = abap_true.
      lv_err_cnt = lv_err_cnt + 1.
      CONTINUE.
    ENDIF.

    " Update GCV/NCV
    <fs_upload>-gcv = lv_gcv.
    <fs_upload>-ncv = lv_ncv.
  ENDLOOP.

  IF lv_error = abap_true.
    gv_errors = gv_errors + lv_err_cnt.
    MESSAGE e001(ygms_msg) WITH 'GCV/NCV errors found. Upload aborted.'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_EXISTING_B2B_DATA
*&---------------------------------------------------------------------*
FORM check_existing_b2b_data.
  DATA: lv_count   TYPE i,
        lv_answer  TYPE c,
        lt_ctp_ids TYPE TABLE OF ygms_de_ongc_ctp.

  " Get unique CTP IDs
  LOOP AT gt_upload_data INTO DATA(ls_data).
    COLLECT ls_data-ctp_id INTO lt_ctp_ids.
  ENDLOOP.

  " Check if B2B data exists for this fortnight
  IF lt_ctp_ids IS NOT INITIAL.
    SELECT COUNT(*)
      FROM yrga_cst_b2b_1
      INTO lv_count
      FOR ALL ENTRIES IN lt_ctp_ids
      WHERE ctp_id  = lt_ctp_ids-table_line
        AND gas_day BETWEEN gv_fn_start AND gv_fn_end.
  ENDIF.

  IF lv_count > 0.
    IF gv_b2b_mode = abap_false.
      " Interactive mode - ask for confirmation
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Warning'
          text_question         = 'B2B data exists for this fortnight. Delete and proceed?'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '2'
          display_cancel_button = ''
        IMPORTING
          answer                = lv_answer.

      IF lv_answer <> '1'.
        MESSAGE s000(ygms_msg) WITH 'Upload cancelled by user.'.
        CLEAR gt_upload_data.
        RETURN.
      ENDIF.
    ENDIF.

    " Delete existing data
    PERFORM delete_existing_b2b_data.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DELETE_EXISTING_B2B_DATA
*&---------------------------------------------------------------------*
FORM delete_existing_b2b_data.
  DATA: lt_ctp_ids TYPE TABLE OF ygms_de_ongc_ctp.

  LOOP AT gt_upload_data INTO DATA(ls_data).
    COLLECT ls_data-ctp_id INTO lt_ctp_ids.
  ENDLOOP.

  " Delete from YRGA_CST_B2B_1 for each CTP ID
  LOOP AT lt_ctp_ids INTO DATA(lv_ctp).
    DELETE FROM yrga_cst_b2b_1
      WHERE ctp_id  = lv_ctp
        AND gas_day BETWEEN gv_fn_start AND gv_fn_end.
  ENDLOOP.

  COMMIT WORK AND WAIT.

  MESSAGE s000(ygms_msg) WITH 'Existing B2B data deleted.'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SAVE_DATA
*&---------------------------------------------------------------------*
FORM save_data.
  DATA: lt_b2b     TYPE TABLE OF yrga_cst_b2b_1,
        ls_b2b     TYPE yrga_cst_b2b_1,
        lv_count   TYPE i.

  CHECK gt_upload_data IS NOT INITIAL.

  LOOP AT gt_upload_data INTO DATA(ls_data).
    CLEAR ls_b2b.

    ls_b2b-mandt         = sy-mandt.
    ls_b2b-gas_day       = ls_data-gas_day.
    ls_b2b-ctp_id        = ls_data-ctp_id.
    ls_b2b-ongc_material = ls_data-ongc_material.
    ls_b2b-qty_scm       = ls_data-qty_scm.
    ls_b2b-gcv           = ls_data-gcv.
    ls_b2b-ncv           = ls_data-ncv.
    ls_b2b-received_on   = sy-datum.
    ls_b2b-received_at   = sy-uzeit.
    ls_b2b-data_source   = 'EXCEL'.
    ls_b2b-created_by    = sy-uname.

    APPEND ls_b2b TO lt_b2b.
  ENDLOOP.

  IF lt_b2b IS NOT INITIAL.
    MODIFY yrga_cst_b2b_1 FROM TABLE lt_b2b.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      lv_count = lines( lt_b2b ).
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

  CHECK gt_upload_data IS NOT INITIAL.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = gt_upload_data
      ).

      lo_functions = lo_alv->get_functions( ).
      lo_functions->set_all( abap_true ).

      lo_columns = lo_alv->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      TRY.
          lo_column = lo_columns->get_column( 'GAS_DAY' ).
          lo_column->set_short_text( 'Gas Day' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          lo_column = lo_columns->get_column( 'LOCATION_ID' ).
          lo_column->set_short_text( 'Location' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          lo_column = lo_columns->get_column( 'CTP_ID' ).
          lo_column->set_short_text( 'CTP ID' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          lo_column = lo_columns->get_column( 'MATERIAL' ).
          lo_column->set_short_text( 'GAIL Mat' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          lo_column = lo_columns->get_column( 'ONGC_MATERIAL' ).
          lo_column->set_short_text( 'ONGC Mat' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          lo_column = lo_columns->get_column( 'QTY_SCM' ).
          lo_column->set_short_text( 'Qty SCM' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          lo_column = lo_columns->get_column( 'GCV' ).
          lo_column->set_short_text( 'GCV' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          lo_column = lo_columns->get_column( 'NCV' ).
          lo_column->set_short_text( 'NCV' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      lo_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx_salv).
      MESSAGE lx_salv TYPE 'E'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_VIEW_ALV
*&---------------------------------------------------------------------*
FORM display_view_alv.
  DATA: lo_alv       TYPE REF TO cl_salv_table,
        lo_functions TYPE REF TO cl_salv_functions_list,
        lo_columns   TYPE REF TO cl_salv_columns_table,
        lo_column    TYPE REF TO cl_salv_column.

  CHECK gt_view_data IS NOT INITIAL.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = gt_view_data
      ).

      lo_functions = lo_alv->get_functions( ).
      lo_functions->set_all( abap_true ).

      lo_columns = lo_alv->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      TRY.
          lo_column = lo_columns->get_column( 'GAS_DAY' ).
          lo_column->set_short_text( 'Gas Day' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          lo_column = lo_columns->get_column( 'CTP_ID' ).
          lo_column->set_short_text( 'CTP ID' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          lo_column = lo_columns->get_column( 'ONGC_MATERIAL' ).
          lo_column->set_short_text( 'ONGC Mat' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          lo_column = lo_columns->get_column( 'QTY_SCM' ).
          lo_column->set_short_text( 'Qty SCM' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          lo_column = lo_columns->get_column( 'GCV' ).
          lo_column->set_short_text( 'GCV' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      TRY.
          lo_column = lo_columns->get_column( 'NCV' ).
          lo_column->set_short_text( 'NCV' ).
        CATCH cx_salv_not_found.
      ENDTRY.

      lo_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx_salv).
      MESSAGE lx_salv TYPE 'E'.
  ENDTRY.
ENDFORM.
