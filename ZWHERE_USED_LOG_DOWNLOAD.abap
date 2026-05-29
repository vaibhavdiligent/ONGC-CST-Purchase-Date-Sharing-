*&---------------------------------------------------------------------*
*& Program: ZWHERE_USED_LOG_DOWNLOAD
*& Purpose: Download high-volume data from ZWHERE_USED_LOG to local file
*&          Handles 3M+ records via chunked SELECT with package cursor
*&          Written in classic ABAP syntax for ECC 6.0 compatibility
*&---------------------------------------------------------------------*
REPORT zwhere_used_log_download.

TABLES: zwhere_used_log.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE text-001.
  SELECT-OPTIONS: s_objnm  FOR zwhere_used_log-src_obj_name,
                  s_objtp  FOR zwhere_used_log-src_obj_type,
                  s_trkorr FOR zwhere_used_log-src_trkorr,
                  s_uobjnm FOR zwhere_used_log-used_in_obj_name,
                  s_uobjtp FOR zwhere_used_log-used_in_obj_type,
                  s_erdat  FOR zwhere_used_log-erdat,
                  s_ernam  FOR zwhere_used_log-ernam,
                  s_status FOR zwhere_used_log-status.
SELECTION-SCREEN END OF BLOCK sel.

SELECTION-SCREEN BEGIN OF BLOCK opt WITH FRAME TITLE text-002.
  PARAMETERS: p_pkg    TYPE i DEFAULT 50000  OBLIGATORY,  " Package size per fetch
              p_path   TYPE string LOWER CASE,             " File path (blank = dialog)
              p_delim  TYPE c DEFAULT '|',                 " Field delimiter
              p_head   AS CHECKBOX DEFAULT 'X'.            " Include header row
SELECTION-SCREEN END OF BLOCK opt.

*----------------------------------------------------------------------*
* Data Declarations
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_log,
         src_obj_name        TYPE zwhere_used_log-src_obj_name,
         src_obj_type        TYPE zwhere_used_log-src_obj_type,
         src_counter         TYPE zwhere_used_log-src_counter,
         src_trkorr          TYPE zwhere_used_log-src_trkorr,
         src_package         TYPE zwhere_used_log-src_package,
         used_in_obj_name    TYPE zwhere_used_log-used_in_obj_name,
         used_in_obj_type    TYPE zwhere_used_log-used_in_obj_type,
         used_in_trkorr      TYPE zwhere_used_log-used_in_trkorr,
         used_in_obj_package TYPE zwhere_used_log-used_in_obj_package,
         detection_type      TYPE zwhere_used_log-detection_type,
         exec_mode           TYPE zwhere_used_log-exec_mode,
         erdat               TYPE zwhere_used_log-erdat,
         erzet               TYPE zwhere_used_log-erzet,
         ernam               TYPE zwhere_used_log-ernam,
         status              TYPE zwhere_used_log-status,
         message             TYPE zwhere_used_log-message,
       END OF ty_log.

DATA: lt_data     TYPE STANDARD TABLE OF ty_log,
      lt_output   TYPE STANDARD TABLE OF string,
      lv_line     TYPE string,
      lv_field    TYPE string,
      lv_file     TYPE string,
      lv_path     TYPE string,
      lv_fullpath TYPE string,
      lv_total    TYPE i,
      lv_fetched  TYPE i,
      lv_pct      TYPE i,
      lv_append   TYPE c,
      lv_bom      TYPE c,
      lv_cursor   TYPE cursor,
      lv_count    TYPE i,
      lv_last     TYPE c,
      lv_len      TYPE i,
      lv_off      TYPE i,
      lv_msg      TYPE string.

FIELD-SYMBOLS: <fs> TYPE ty_log.

*----------------------------------------------------------------------*
* F4 help for output file path
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  DATA: lv_f4_file TYPE string,
        lv_f4_path TYPE string,
        lv_f4_full TYPE string.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Select output file'
      default_file_name = 'ZWHERE_USED_LOG_EXPORT.txt'
      default_extension = 'txt'
      file_filter       = 'Text Files (*.txt)|*.txt|CSV Files (*.csv)|*.csv|All Files (*.*)|*.*|'
    CHANGING
      filename          = lv_f4_file
      path              = lv_f4_path
      fullpath          = lv_f4_full
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc = 0 AND lv_f4_full IS NOT INITIAL.
    p_path = lv_f4_full.
  ENDIF.

*----------------------------------------------------------------------*
* Macro to build delimited line (classic, ECC-safe)
*----------------------------------------------------------------------*
DEFINE _append_field.
  lv_field = &1.
  IF lv_line IS INITIAL.
    lv_line = lv_field.
  ELSE.
    CONCATENATE lv_line p_delim lv_field INTO lv_line.
  ENDIF.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Determine output file path
  IF p_path IS INITIAL.
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        default_file_name = 'ZWHERE_USED_LOG_EXPORT.txt'
        default_extension = 'txt'
        file_filter       = 'Text Files (*.txt)|*.txt|CSV Files (*.csv)|*.csv|All Files (*.*)|*.*|'
      CHANGING
        filename          = lv_file
        path              = lv_path
        fullpath          = lv_fullpath
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.
    IF sy-subrc <> 0 OR lv_fullpath IS INITIAL.
      MESSAGE 'No file selected. Program cancelled.' TYPE 'I'.
      LEAVE PROGRAM.
    ENDIF.
    lv_file = lv_fullpath.
  ELSE.
    lv_file = p_path.
  ENDIF.

* Safeguard: if a folder was given (ends with \ or /), append a filename
  lv_len = strlen( lv_file ).
  IF lv_len > 0.
    lv_off = lv_len - 1.
    lv_last = lv_file+lv_off(1).
    IF lv_last = '\' OR lv_last = '/'.
      CONCATENATE lv_file 'ZWHERE_USED_LOG_EXPORT.txt' INTO lv_file.
    ENDIF.
  ENDIF.

* Count matching records first (for progress display)
  SELECT COUNT(*) INTO lv_total
    FROM zwhere_used_log
    WHERE src_obj_name     IN s_objnm
      AND src_obj_type     IN s_objtp
      AND src_trkorr       IN s_trkorr
      AND used_in_obj_name IN s_uobjnm
      AND used_in_obj_type IN s_uobjtp
      AND erdat            IN s_erdat
      AND ernam            IN s_ernam
      AND status           IN s_status.

  IF lv_total = 0.
    MESSAGE 'No records found matching selection criteria.' TYPE 'I'.
    LEAVE PROGRAM.
  ENDIF.

  WRITE: / 'Total records to export:', lv_total.
  ULINE.

* Write header row
  IF p_head = 'X'.
    CLEAR lv_line.
    _append_field 'SRC_OBJ_NAME'.
    _append_field 'SRC_OBJ_TYPE'.
    _append_field 'SRC_COUNTER'.
    _append_field 'SRC_TRKORR'.
    _append_field 'SRC_PACKAGE'.
    _append_field 'USED_IN_OBJ_NAME'.
    _append_field 'USED_IN_OBJ_TYPE'.
    _append_field 'USED_IN_TRKORR'.
    _append_field 'USED_IN_OBJ_PACKAGE'.
    _append_field 'DETECTION_TYPE'.
    _append_field 'EXEC_MODE'.
    _append_field 'ERDAT'.
    _append_field 'ERZET'.
    _append_field 'ERNAM'.
    _append_field 'STATUS'.
    _append_field 'MESSAGE'.
    APPEND lv_line TO lt_output.
  ENDIF.

* Fetch data in packages using cursor to avoid memory overflow
  lv_fetched = 0.

  OPEN CURSOR WITH HOLD lv_cursor FOR
    SELECT src_obj_name src_obj_type src_counter src_trkorr src_package
           used_in_obj_name used_in_obj_type used_in_trkorr used_in_obj_package
           detection_type exec_mode erdat erzet ernam status message
      FROM zwhere_used_log
      WHERE src_obj_name     IN s_objnm
        AND src_obj_type     IN s_objtp
        AND src_trkorr       IN s_trkorr
        AND used_in_obj_name IN s_uobjnm
        AND used_in_obj_type IN s_uobjtp
        AND erdat            IN s_erdat
        AND ernam            IN s_ernam
        AND status           IN s_status
      ORDER BY src_obj_name src_obj_type erdat.

  DO.
    CLEAR lt_data.
    FETCH NEXT CURSOR lv_cursor
      INTO TABLE lt_data
      PACKAGE SIZE p_pkg.

    IF sy-subrc <> 0 AND lt_data IS INITIAL.
      EXIT.
    ENDIF.

*   Convert internal table to delimited strings
    LOOP AT lt_data ASSIGNING <fs>.
      CLEAR lv_line.
      _append_field <fs>-src_obj_name.
      _append_field <fs>-src_obj_type.
      _append_field <fs>-src_counter.
      _append_field <fs>-src_trkorr.
      _append_field <fs>-src_package.
      _append_field <fs>-used_in_obj_name.
      _append_field <fs>-used_in_obj_type.
      _append_field <fs>-used_in_trkorr.
      _append_field <fs>-used_in_obj_package.
      _append_field <fs>-detection_type.
      _append_field <fs>-exec_mode.
      _append_field <fs>-erdat.
      _append_field <fs>-erzet.
      _append_field <fs>-ernam.
      _append_field <fs>-status.
      _append_field <fs>-message.
      APPEND lv_line TO lt_output.
    ENDLOOP.

    lv_count = lines( lt_data ).
    lv_fetched = lv_fetched + lv_count.

*   Determine append / BOM flags (first write creates file, rest append)
    IF lv_fetched > lv_count.
      lv_append = 'X'.
      lv_bom    = ' '.
    ELSE.
      lv_append = ' '.
      lv_bom    = 'X'.
    ENDIF.

*   Flush buffer to file every package to control memory usage
    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename         = lv_file
        filetype         = 'ASC'
        append           = lv_append
        codepage         = '4110'    " UTF-8
        write_bom        = lv_bom
      CHANGING
        data_tab         = lt_output
      EXCEPTIONS
        file_write_error = 1
        OTHERS           = 2.

    IF sy-subrc <> 0.
      CLOSE CURSOR lv_cursor.
      CONCATENATE 'File write error at record' lv_fetched 'Path:' lv_file
                  INTO lv_msg SEPARATED BY space.
      MESSAGE lv_msg TYPE 'E'.
    ENDIF.

    CLEAR lt_output.

*   Progress indicator
    lv_pct = ( lv_fetched * 100 ) / lv_total.
    WRITE: / 'Exported', lv_fetched, '/', lv_total, 'records (', lv_pct, '% )'.

    IF sy-subrc <> 0.   " FETCH returned non-zero = end of data
      EXIT.
    ENDIF.
  ENDDO.

  CLOSE CURSOR lv_cursor.

  ULINE.
  WRITE: / 'Export complete.', lv_fetched, 'records written to:', lv_file.
  CONCATENATE 'Export complete.' lv_fetched 'records written.'
              INTO lv_msg SEPARATED BY space.
  MESSAGE lv_msg TYPE 'S'.
