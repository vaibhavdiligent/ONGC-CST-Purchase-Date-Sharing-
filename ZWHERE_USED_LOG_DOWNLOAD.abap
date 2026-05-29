*&---------------------------------------------------------------------*
*& Program: ZWHERE_USED_LOG_DOWNLOAD
*& Purpose: Download high-volume data from ZWHERE_USED_LOG straight to a
*&          local PC file (e.g. the Desktop).
*&          Classic ABAP syntax for ECC 6.0 compatibility.
*&
*& WHY KEY-PAGING (not OPEN CURSOR + GUI_DOWNLOAD):
*&   GUI_DOWNLOAD opens an RFC link to the SAP GUI; that roundtrip causes
*&   an IMPLICIT DATABASE COMMIT. If a cursor is open (OPEN CURSOR/FETCH)
*&   the commit invalidates it -> dump DBIF_RSQL_INVALID_CURSOR.
*&   Instead we read the table in pages using independent
*&   "SELECT ... UP TO p_pkg ROWS" statements ordered by the primary key.
*&   Each SELECT fully completes (no cursor stays open), so the commit
*&   triggered by GUI_DOWNLOAD between pages is harmless. We remember the
*&   last key of each page and fetch the next rows greater than it.
*&
*& OUTPUT: a delimited UTF-8 text file on the user's PC. Memory stays
*&         bounded because only one page is held at a time.
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
  PARAMETERS: p_path  TYPE string LOWER CASE,            " PC file (blank = dialog/desktop)
              p_pkg   TYPE i DEFAULT 50000 OBLIGATORY,   " Rows per page
              p_delim TYPE c DEFAULT '|',                " Field delimiter
              p_head  AS CHECKBOX DEFAULT 'X'.           " Include header row
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

DATA: lt_data    TYPE STANDARD TABLE OF ty_log,
      lt_output  TYPE STANDARD TABLE OF string,
      lv_line    TYPE string,
      lv_field   TYPE string,
      lv_file    TYPE string,
      lv_total   TYPE i,
      lv_fetched TYPE i,
      lv_pct     TYPE i,
      lv_count   TYPE i,
      lv_first   TYPE c VALUE 'X',     " first page flag
      lv_append  TYPE c,
      lv_bom     TYPE c,
      lv_msg     TYPE string.

* Last-key holders for paging (primary key after MANDT)
DATA: lv_k_name TYPE zwhere_used_log-src_obj_name,
      lv_k_type TYPE zwhere_used_log-src_obj_type,
      lv_k_cnt  TYPE zwhere_used_log-src_counter.

* For F4 / desktop default
DATA: lv_desktop TYPE string,
      lv_f4_file TYPE string,
      lv_f4_path TYPE string,
      lv_f4_full TYPE string.

FIELD-SYMBOLS: <fs> TYPE ty_log.

*----------------------------------------------------------------------*
* Macro to build a delimited line (classic, ECC-safe)
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
* F4 help for output file path
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
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
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Determine output file on the PC
  IF p_path IS INITIAL.
*   Default to the user's Desktop folder
    CALL METHOD cl_gui_frontend_services=>get_desktop_directory
      CHANGING
        desktop_directory = lv_desktop
      EXCEPTIONS
        OTHERS            = 1.
    CALL METHOD cl_gui_cfw=>flush.
    IF lv_desktop IS NOT INITIAL.
      CONCATENATE lv_desktop '\ZWHERE_USED_LOG_EXPORT.txt' INTO lv_file.
    ELSE.
      lv_file = 'C:\ZWHERE_USED_LOG_EXPORT.txt'.
    ENDIF.
  ELSE.
    lv_file = p_path.
  ENDIF.

* Count matching records (for progress display)
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

* Header row goes in the very first output buffer
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

*----------------------------------------------------------------------*
* Page through the data. Each SELECT is independent (no open cursor),
* so the implicit commit caused by GUI_DOWNLOAD is harmless.
*----------------------------------------------------------------------*
  DO.
    CLEAR lt_data.

    IF lv_first = 'X'.
*     First page: no key lower bound
      SELECT src_obj_name src_obj_type src_counter src_trkorr src_package
             used_in_obj_name used_in_obj_type used_in_trkorr used_in_obj_package
             detection_type exec_mode erdat erzet ernam status message
        FROM zwhere_used_log
        INTO TABLE lt_data
        UP TO p_pkg ROWS
        WHERE src_obj_name     IN s_objnm
          AND src_obj_type     IN s_objtp
          AND src_trkorr       IN s_trkorr
          AND used_in_obj_name IN s_uobjnm
          AND used_in_obj_type IN s_uobjtp
          AND erdat            IN s_erdat
          AND ernam            IN s_ernam
          AND status           IN s_status
        ORDER BY src_obj_name src_obj_type src_counter.
    ELSE.
*     Next pages: only rows with key strictly greater than the last key.
*     Composite-key comparison expanded into OR-conditions.
      SELECT src_obj_name src_obj_type src_counter src_trkorr src_package
             used_in_obj_name used_in_obj_type used_in_trkorr used_in_obj_package
             detection_type exec_mode erdat erzet ernam status message
        FROM zwhere_used_log
        INTO TABLE lt_data
        UP TO p_pkg ROWS
        WHERE ( src_obj_name > lv_k_name
             OR ( src_obj_name = lv_k_name AND src_obj_type > lv_k_type )
             OR ( src_obj_name = lv_k_name AND src_obj_type = lv_k_type
                                          AND src_counter  > lv_k_cnt ) )
          AND src_obj_name     IN s_objnm
          AND src_obj_type     IN s_objtp
          AND src_trkorr       IN s_trkorr
          AND used_in_obj_name IN s_uobjnm
          AND used_in_obj_type IN s_uobjtp
          AND erdat            IN s_erdat
          AND ernam            IN s_ernam
          AND status           IN s_status
        ORDER BY src_obj_name src_obj_type src_counter.
    ENDIF.

    lv_count = lines( lt_data ).
    IF lv_count = 0.
      EXIT.
    ENDIF.

*   Remember the key of the last row for the next page
    READ TABLE lt_data ASSIGNING <fs> INDEX lv_count.
    lv_k_name = <fs>-src_obj_name.
    lv_k_type = <fs>-src_obj_type.
    lv_k_cnt  = <fs>-src_counter.

*   Build delimited lines
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

*   First write creates the file (with BOM); later writes append.
    IF lv_first = 'X'.
      lv_append = ' '.
      lv_bom    = 'X'.
    ELSE.
      lv_append = 'X'.
      lv_bom    = ' '.
    ENDIF.

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
      CONCATENATE 'File write error. Check path:' lv_file
                  INTO lv_msg SEPARATED BY space.
      MESSAGE lv_msg TYPE 'E'.
    ENDIF.

    CLEAR lt_output.
    lv_fetched = lv_fetched + lv_count.
    lv_first = ' '.

*   Progress
    lv_pct = ( lv_fetched * 100 ) / lv_total.
    WRITE: / 'Exported', lv_fetched, '/', lv_total, 'records (', lv_pct, '% )'.

    IF lv_count < p_pkg.   " last (partial) page -> done
      EXIT.
    ENDIF.
  ENDDO.

  ULINE.
  WRITE: / 'Export complete.', lv_fetched, 'records written to:'.
  WRITE: / lv_file.
  CONCATENATE 'Export complete.' lv_fetched 'records written.'
              INTO lv_msg SEPARATED BY space.
  MESSAGE lv_msg TYPE 'S'.
