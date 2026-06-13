*&---------------------------------------------------------------------*
*& Report  ZATC_PROG_UPLOAD
*&---------------------------------------------------------------------*
*& Program Title : Upload object source from text files using a manifest
*&
*& Description   : Companion uploader for ZATC_PROG_DOWNLOAD.
*&                 Reads the Excel manifest produced by the download
*&                 program (Object Name | Object Type | File Name), then
*&                 for every entry reads the matching .txt file from the
*&                 chosen folder and writes the source back into the
*&                 object, recording it in the transport request given on
*&                 the selection screen.
*&
*&                 Excel is read with the standard FM
*&                 TEXT_CONVERT_XLS_TO_SAP, the .txt files with the
*&                 standard FM GUI_UPLOAD, and the source is updated with
*&                 RPY_PROGRAM_UPDATE (same API used by
*&                 ZATC_RESULT_CORRECTION).
*&---------------------------------------------------------------------*
REPORT zatc_prog_upload.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
" Flat structure matching the manifest columns. The columns are read by
" position, so the field order here must match the download program.
TYPES: BEGIN OF ty_manifest,
         objname  TYPE c LENGTH 40,
         objtype  TYPE c LENGTH 10,
         filename TYPE c LENGTH 128,
       END OF ty_manifest.

TYPES: BEGIN OF ty_log,
         objname  TYPE c LENGTH 40,
         objtype  TYPE c LENGTH 10,
         filename TYPE c LENGTH 128,
         status   TYPE char10,           " Success / Error / Skipped
         message  TYPE char120,
       END OF ty_log.

*----------------------------------------------------------------------*
* Global data
*----------------------------------------------------------------------*
DATA: gt_manifest TYPE STANDARD TABLE OF ty_manifest,
      gt_source   TYPE STANDARD TABLE OF abaptxt255,
      gt_log      TYPE STANDARD TABLE OF ty_log,
      gv_sep      TYPE c LENGTH 1.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  PARAMETERS p_dir TYPE string OBLIGATORY.            " folder with .txt files
  PARAMETERS p_xls TYPE string OBLIGATORY.            " manifest Excel file
  PARAMETERS p_req TYPE trkorr OBLIGATORY.            " transport request
  PARAMETERS p_test TYPE flag AS CHECKBOX DEFAULT 'X'." simulate (no update)
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* F4 helps
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dir.
  PERFORM f4_folder CHANGING p_dir.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_xls.
  PERFORM f4_file CHANGING p_xls.

*----------------------------------------------------------------------*
INITIALIZATION.
  cl_gui_frontend_services=>get_file_separator(
    CHANGING  file_separator = gv_sep
    EXCEPTIONS OTHERS = 1 ).
  IF gv_sep IS INITIAL.
    gv_sep = '\'.
  ENDIF.

*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF p_test IS INITIAL.
    PERFORM validate_transport.
  ENDIF.

*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM run_in_gui.
  PERFORM normalize_folder CHANGING p_dir.

  PERFORM read_manifest.
  IF gt_manifest IS INITIAL.
    MESSAGE 'Manifest is empty or could not be read' TYPE 'E'.
  ENDIF.

  DATA lv_total TYPE i.
  DATA lv_done  TYPE i.
  lv_total = lines( gt_manifest ).

  LOOP AT gt_manifest INTO DATA(ls_man).
    lv_done = lv_done + 1.
    PERFORM progress USING lv_done lv_total ls_man-objname.
    PERFORM upload_object USING ls_man.
  ENDLOOP.

  PERFORM show_log.

*&---------------------------------------------------------------------*
*& Form read_manifest  (standard FM TEXT_CONVERT_XLS_TO_SAP)
*&---------------------------------------------------------------------*
FORM read_manifest.
  DATA: lt_raw   TYPE truxs_t_text_data,
        lv_file  TYPE rlgrap-filename.

  lv_file = p_xls.
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_field_seperator    = 'X'      " tab-separated cells
      i_line_header        = 'X'      " skip the header row
      i_tab_raw_data       = lt_raw
      i_filename           = lv_file
    TABLES
      i_tab_converted_data = gt_manifest
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Could not read the manifest Excel file' TYPE 'E'.
  ENDIF.

  " Drop blank rows.
  DELETE gt_manifest WHERE objname IS INITIAL AND filename IS INITIAL.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form upload_object
*&---------------------------------------------------------------------*
FORM upload_object USING ps_man TYPE ty_manifest.

  DATA: ls_log      TYPE ty_log,
        lv_fullpath TYPE string,
        lv_name     TYPE programm,
        lv_exists   TYPE abap_bool.

  ls_log-objname  = ps_man-objname.
  ls_log-objtype  = ps_man-objtype.
  ls_log-filename = ps_man-filename.

  IF ps_man-filename IS INITIAL.
    ls_log-status  = 'Skipped'.
    ls_log-message = 'No file name in manifest'.
    APPEND ls_log TO gt_log.
    RETURN.
  ENDIF.

  " Read the .txt file from the folder.
  CONCATENATE p_dir ps_man-filename INTO lv_fullpath.
  REFRESH gt_source.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = lv_fullpath
      filetype = 'ASC'
    TABLES
      data_tab = gt_source
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc <> 0 OR gt_source IS INITIAL.
    ls_log-status  = 'Error'.
    ls_log-message = 'Could not read source text file'.
    APPEND ls_log TO gt_log.
    RETURN.
  ENDIF.

  lv_name = ps_man-objname.

  " Object must already exist (we update, we do not create).
  SELECT SINGLE name FROM trdir INTO @DATA(lv_dummy) WHERE name = @lv_name.
  lv_exists = boolc( sy-subrc = 0 ).
  IF lv_exists = abap_false.
    ls_log-status  = 'Skipped'.
    ls_log-message = 'Object does not exist in this system'.
    APPEND ls_log TO gt_log.
    RETURN.
  ENDIF.

  IF p_test = 'X'.
    ls_log-status  = 'Success'.
    ls_log-message = |Simulation: { lines( gt_source ) } lines ready to upload|.
    APPEND ls_log TO gt_log.
    RETURN.
  ENDIF.

  " Determine the program type (subc) and write the source back, recording
  " the change in the transport request.
  SELECT SINGLE subc FROM trdir INTO @DATA(lv_subc) WHERE name = @lv_name.

  CALL FUNCTION 'RPY_PROGRAM_UPDATE'
    EXPORTING
      program_name     = lv_name
      program_type     = lv_subc
      transport_number = p_req
    TABLES
      source_extended  = gt_source
    EXCEPTIONS
      cancelled        = 1
      permission_error = 2
      not_found        = 3
      OTHERS           = 4.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    ls_log-status  = 'Success'.
    ls_log-message = |Uploaded to { p_req }|.
  ELSE.
    ls_log-status  = 'Error'.
    ls_log-message = |RPY_PROGRAM_UPDATE failed (subrc { sy-subrc })|.
  ENDIF.
  APPEND ls_log TO gt_log.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_transport  - modifiable workbench request only
*&---------------------------------------------------------------------*
FORM validate_transport.
  SELECT SINGLE trfunction, trstatus FROM e070 INTO @DATA(ls_e070)
    WHERE trkorr = @p_req.
  IF sy-subrc <> 0.
    MESSAGE 'Transport request does not exist' TYPE 'E'.
  ENDIF.
  " Customizing (W/T) or other non-workbench types are rejected.
  IF ls_e070-trfunction = 'W' OR ls_e070-trfunction = 'T'
     OR ls_e070-trfunction = 'G' OR ls_e070-trfunction = 'R'.
    MESSAGE 'Please select a Workbench transport request' TYPE 'E'.
  ENDIF.
  " Status must be modifiable (D = modifiable, L = modifiable protected).
  IF ls_e070-trstatus <> 'D' AND ls_e070-trstatus <> 'L'.
    MESSAGE 'Transport request is already released / not modifiable' TYPE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f4_folder
*&---------------------------------------------------------------------*
FORM f4_folder CHANGING pv_dir TYPE string.
  DATA lv_folder TYPE string.
  cl_gui_frontend_services=>directory_browse(
    EXPORTING
      window_title    = 'Select folder containing the text files'
    CHANGING
      selected_folder = lv_folder
    EXCEPTIONS
      OTHERS          = 1 ).
  IF lv_folder IS NOT INITIAL.
    pv_dir = lv_folder.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form f4_file  - pick the manifest Excel file
*&---------------------------------------------------------------------*
FORM f4_file CHANGING pv_file TYPE string.
  DATA: lt_files TYPE filetable,
        lv_rc    TYPE i,
        lv_act   TYPE i.
  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title = 'Select manifest Excel file'
      file_filter  = 'Excel Files (*.xls;*.xlsx)|*.xls;*.xlsx|All Files (*.*)|*.*'
    CHANGING
      file_table   = lt_files
      rc           = lv_rc
      user_action  = lv_act
    EXCEPTIONS
      OTHERS       = 1 ).
  IF lv_act = cl_gui_frontend_services=>action_ok.
    READ TABLE lt_files INTO DATA(ls_file) INDEX 1.
    IF sy-subrc = 0.
      pv_file = ls_file-filename.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form normalize_folder  - ensure a trailing path separator
*&---------------------------------------------------------------------*
FORM normalize_folder CHANGING pv_dir TYPE string.
  DATA lv_len TYPE i.
  lv_len = strlen( pv_dir ).
  IF lv_len > 0
     AND substring( val = pv_dir off = lv_len - 1 len = 1 ) <> gv_sep.
    CONCATENATE pv_dir gv_sep INTO pv_dir.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form run_in_gui
*&---------------------------------------------------------------------*
FORM run_in_gui.
  DATA l_url TYPE string.
  CALL FUNCTION 'WEBGUI_GET_FLP_URL'
    IMPORTING
      url = l_url.
  IF l_url IS NOT INITIAL.
    MESSAGE 'Program can only be executed in SAP GUI' TYPE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form progress
*&---------------------------------------------------------------------*
FORM progress USING pv_done TYPE i pv_total TYPE i pv_text TYPE clike.
  DATA(lv_pct) = COND i( WHEN pv_total > 0 THEN pv_done * 100 / pv_total ELSE 0 ).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = lv_pct
      text       = |Uploading { pv_text } ({ pv_done }/{ pv_total })|.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form show_log  - ALV summary
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
