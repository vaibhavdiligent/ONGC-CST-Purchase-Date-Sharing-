*&---------------------------------------------------------------------*
*& Program: ZWHERE_USED_LOG_DOWNLOAD
*& Purpose: Download high-volume data from ZWHERE_USED_LOG.
*&          Handles 3M+ records via package cursor + OPEN DATASET.
*&          Classic ABAP syntax for ECC 6.0 compatibility.
*&
*& IMPORTANT - why OPEN DATASET (not GUI_DOWNLOAD):
*&   GUI_DOWNLOAD opens an RFC connection to the SAP GUI frontend.
*&   That roundtrip triggers an IMPLICIT DATABASE COMMIT, which
*&   invalidates an open cursor -> runtime error DBIF_RSQL_INVALID_CURSOR.
*&   OPEN DATASET / TRANSFER write to the application server WITHOUT any
*&   commit, so the cursor stays valid across all FETCH packages.
*&
*& OUTPUT: A file on the SAP application server. Retrieve it afterwards
*&         with transaction CG3Y (server -> PC) or view via AL11.
*&         Best run in background (SM36 / F9) for this data volume.
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
  PARAMETERS: p_file  TYPE string LOWER CASE OBLIGATORY
                      DEFAULT '/tmp/ZWHERE_USED_LOG_EXPORT.txt',
              p_pkg   TYPE i DEFAULT 50000 OBLIGATORY,  " Package size per fetch
              p_delim TYPE c DEFAULT '|',               " Field delimiter
              p_head  AS CHECKBOX DEFAULT 'X'.          " Include header row
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
      lv_line    TYPE string,
      lv_field   TYPE string,
      lv_total   TYPE i,
      lv_fetched TYPE i,
      lv_pct     TYPE i,
      lv_cursor  TYPE cursor,
      lv_count   TYPE i,
      lv_msg     TYPE string.

FIELD-SYMBOLS: <fs> TYPE ty_log.

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

* Open the application-server file for output (text mode, UTF-8).
* OPEN DATASET does NOT cause a database commit -> cursor stays valid.
  OPEN DATASET p_file FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
  IF sy-subrc <> 0.
    CONCATENATE 'Cannot open file on application server:' p_file
                INTO lv_msg SEPARATED BY space.
    MESSAGE lv_msg TYPE 'E'.
  ENDIF.

* Count matching records first (for progress display).
* This SELECT completes fully before the cursor is opened, so no conflict.
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
    CLOSE DATASET p_file.
    MESSAGE 'No records found matching selection criteria.' TYPE 'I'.
    LEAVE PROGRAM.
  ENDIF.

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
    TRANSFER lv_line TO p_file.
  ENDIF.

* Fetch data in packages using a cursor to avoid memory overflow.
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
        AND status           IN s_status.

  DO.
    CLEAR lt_data.
    FETCH NEXT CURSOR lv_cursor
      INTO TABLE lt_data
      PACKAGE SIZE p_pkg.

    IF sy-subrc <> 0 AND lt_data IS INITIAL.
      EXIT.
    ENDIF.

*   Convert each row to a delimited line and transfer to the dataset.
*   TRANSFER does not commit, so the cursor remains valid.
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
      TRANSFER lv_line TO p_file.
    ENDLOOP.

    lv_count = lines( lt_data ).
    lv_fetched = lv_fetched + lv_count.

*   Progress indicator (safe: no DB / RFC interruption of the cursor).
    lv_pct = ( lv_fetched * 100 ) / lv_total.
    WRITE: / 'Exported', lv_fetched, '/', lv_total, 'records (', lv_pct, '% )'.

    IF lv_count < p_pkg.   " last (partial) package -> done
      EXIT.
    ENDIF.
  ENDDO.

  CLOSE CURSOR lv_cursor.
  CLOSE DATASET p_file.

  ULINE.
  WRITE: / 'Export complete.', lv_fetched, 'records written to server file:'.
  WRITE: / p_file.
  WRITE: / 'Retrieve it with transaction CG3Y (or view via AL11).'.
  CONCATENATE 'Export complete.' lv_fetched 'records written to' p_file
              INTO lv_msg SEPARATED BY space.
  MESSAGE lv_msg TYPE 'S'.
