*&---------------------------------------------------------------------*
*& Report  ZATC_PROG_DOWNLOAD
*&---------------------------------------------------------------------*
*& Program Title : Download object source as text files + manifest Excel
*&
*& Description   : Companion downloader for ZATC_RESULT_CORRECTION.
*&                 For every object entered on the selection screen the
*&                 active source is read and written to the chosen folder
*&                 as a .txt file (one file per object). After all objects
*&                 are downloaded an Excel manifest is written to the same
*&                 folder containing  Object Name | Object Type | File Name.
*&                 The manifest is the input for the upload program
*&                 ZATC_PROG_UPLOAD, which restores the source and records
*&                 the objects in a transport request.
*&
*& Object types  : PROG / INCLUDE / FUGR-includes (anything readable via
*&                 READ REPORT). Other types are reported as skipped.
*&---------------------------------------------------------------------*
REPORT zatc_prog_download.

TABLES: trdir, tadir.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_manifest,
         objname  TYPE tadir-obj_name,   " object / program / include name
         objtype  TYPE tadir-object,     " R3TR object type (PROG, FUGR, ...)
         filename TYPE string,           " name of the downloaded .txt file
       END OF ty_manifest.

TYPES: BEGIN OF ty_log,
         objname  TYPE tadir-obj_name,
         objtype  TYPE tadir-object,
         filename TYPE string,
         status   TYPE char10,           " Success / Skipped / Error
         message  TYPE char100,
       END OF ty_log.

*----------------------------------------------------------------------*
* Global data
*----------------------------------------------------------------------*
DATA: gt_source   TYPE STANDARD TABLE OF abaptxt255,
      gt_manifest TYPE STANDARD TABLE OF ty_manifest,
      gt_log      TYPE STANDARD TABLE OF ty_log,
      gv_sep      TYPE c LENGTH 1.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  SELECT-OPTIONS s_obj FOR trdir-name OBLIGATORY.          " objects to download
  PARAMETERS     p_dir TYPE string OBLIGATORY.             " target folder
  PARAMETERS     p_man TYPE flag AS CHECKBOX DEFAULT 'X'.  " also write manifest
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* F4 help for the folder
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dir.
  PERFORM f4_folder CHANGING p_dir.

*----------------------------------------------------------------------*
INITIALIZATION.
  cl_gui_frontend_services=>get_file_separator(
    CHANGING  file_separator = gv_sep
    EXCEPTIONS OTHERS = 1 ).
  IF gv_sep IS INITIAL.
    gv_sep = '\'.
  ENDIF.

*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM run_in_gui.
  PERFORM normalize_folder CHANGING p_dir.

  DATA lv_total TYPE i.
  DATA lv_done  TYPE i.

  " Resolve the list of object names from the select-options.
  DATA gt_names TYPE STANDARD TABLE OF trdir-name.
  SELECT name FROM trdir INTO TABLE @gt_names WHERE name IN @s_obj.
  IF gt_names IS INITIAL.
    MESSAGE 'No objects found for the given selection' TYPE 'E'.
  ENDIF.
  lv_total = lines( gt_names ).

  LOOP AT gt_names INTO DATA(lv_name).
    lv_done = lv_done + 1.
    PERFORM progress USING lv_done lv_total lv_name.
    PERFORM download_object USING lv_name.
  ENDLOOP.

  " Write the Excel manifest once all objects are downloaded.
  IF p_man = 'X' AND gt_manifest IS NOT INITIAL.
    PERFORM write_manifest.
  ENDIF.

  PERFORM show_log.

*&---------------------------------------------------------------------*
*& Form download_object
*&---------------------------------------------------------------------*
FORM download_object USING pv_name TYPE trdir-name.

  DATA: ls_log      TYPE ty_log,
        ls_manifest TYPE ty_manifest,
        lv_type     TYPE tadir-object,
        lv_file     TYPE string,
        lv_fullpath TYPE string,
        lv_subrc    TYPE sy-subrc.

  ls_log-objname = pv_name.

  " Read the active source.
  REFRESH gt_source.
  READ REPORT pv_name INTO gt_source STATE 'A'.
  IF sy-subrc <> 0 OR gt_source IS INITIAL.
    ls_log-status  = 'Skipped'.
    ls_log-message = 'No active source found (not a program/include)'.
    APPEND ls_log TO gt_log.
    RETURN.
  ENDIF.

  PERFORM get_obj_type USING pv_name CHANGING lv_type.

  " Build a file-system safe file name (namespace '/' -> '#').
  lv_file = pv_name.
  REPLACE ALL OCCURRENCES OF '/' IN lv_file WITH '#'.
  CONCATENATE lv_file '.txt' INTO lv_file.
  CONCATENATE p_dir lv_file INTO lv_fullpath.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename = lv_fullpath
      filetype = 'ASC'
    TABLES
      data_tab = gt_source
    EXCEPTIONS
      OTHERS   = 1.
  lv_subrc = sy-subrc.

  ls_log-objtype  = lv_type.
  ls_log-filename = lv_file.
  IF lv_subrc = 0.
    ls_log-status  = 'Success'.
    ls_log-message = 'Downloaded'.
    ls_manifest-objname  = pv_name.
    ls_manifest-objtype  = lv_type.
    ls_manifest-filename = lv_file.
    APPEND ls_manifest TO gt_manifest.
  ELSE.
    ls_log-status  = 'Error'.
    ls_log-message = 'GUI_DOWNLOAD failed'.
  ENDIF.
  APPEND ls_log TO gt_log.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_obj_type
*&  Returns the R3TR object type from TADIR; falls back to the TRDIR
*&  program class (executable -> PROG, otherwise INCLUDE).
*&---------------------------------------------------------------------*
FORM get_obj_type USING pv_name TYPE trdir-name
                  CHANGING pv_type TYPE tadir-object.

  CLEAR pv_type.
  SELECT SINGLE object FROM tadir INTO pv_type
    WHERE pgmid = 'R3TR' AND obj_name = pv_name.
  IF sy-subrc = 0 AND pv_type IS NOT INITIAL.
    RETURN.
  ENDIF.

  SELECT SINGLE subc FROM trdir INTO @DATA(lv_subc) WHERE name = @pv_name.
  IF sy-subrc = 0 AND lv_subc = '1'.
    pv_type = 'PROG'.
  ELSE.
    pv_type = 'INCL'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form write_manifest
*&  Writes the manifest as a real Excel file using the standard function
*&  module SAP_CONVERT_TO_XLS_FORMAT. The first row is a header that the
*&  upload program skips (TEXT_CONVERT_XLS_TO_SAP with i_line_header = 'X').
*&---------------------------------------------------------------------*
FORM write_manifest.

  TYPES: BEGIN OF ty_flat,
           objname  TYPE c LENGTH 40,
           objtype  TYPE c LENGTH 10,
           filename TYPE c LENGTH 128,
         END OF ty_flat.

  DATA: lt_flat  TYPE STANDARD TABLE OF ty_flat,
        ls_flat  TYPE ty_flat,
        lv_file  TYPE string,
        lv_full  TYPE string,
        lv_stamp TYPE char14.

  " Header row (skipped on upload).
  ls_flat-objname  = 'Object Name'.
  ls_flat-objtype  = 'Object Type'.
  ls_flat-filename = 'File Name'.
  APPEND ls_flat TO lt_flat.

  LOOP AT gt_manifest INTO DATA(ls_man).
    CLEAR ls_flat.
    ls_flat-objname  = ls_man-objname.
    ls_flat-objtype  = ls_man-objtype.
    ls_flat-filename = ls_man-filename.
    APPEND ls_flat TO lt_flat.
  ENDLOOP.

  CONCATENATE sy-datum sy-uzeit INTO lv_stamp.
  CONCATENATE 'ZATC_DOWNLOAD_MANIFEST_' lv_stamp '.xls' INTO lv_file.
  CONCATENATE p_dir lv_file INTO lv_full.

  CALL FUNCTION 'SAP_CONVERT_TO_XLS_FORMAT'
    EXPORTING
      i_filename        = lv_full
    TABLES
      i_tab_sap_data    = lt_flat
    EXCEPTIONS
      conversion_failed = 1
      OTHERS            = 2.
  IF sy-subrc = 0.
    MESSAGE |Manifest written: { lv_file }| TYPE 'S'.
  ELSE.
    MESSAGE 'Manifest download failed' TYPE 'I'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f4_folder
*&---------------------------------------------------------------------*
FORM f4_folder CHANGING pv_dir TYPE string.
  DATA lv_folder TYPE string.
  cl_gui_frontend_services=>directory_browse(
    EXPORTING
      window_title    = 'Select target folder'
    CHANGING
      selected_folder = lv_folder
    EXCEPTIONS
      OTHERS          = 1 ).
  IF lv_folder IS NOT INITIAL.
    pv_dir = lv_folder.
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
*& Form run_in_gui  - this tool only works in SAP GUI (frontend services)
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
FORM progress USING pv_done TYPE i pv_total TYPE i pv_text TYPE trdir-name.
  DATA(lv_pct) = COND i( WHEN pv_total > 0 THEN pv_done * 100 / pv_total ELSE 0 ).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = lv_pct
      text       = |Downloading { pv_text } ({ pv_done }/{ pv_total })|.
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
