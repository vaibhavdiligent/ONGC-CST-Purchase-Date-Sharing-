*&---------------------------------------------------------------------*
*& Program: ZTADIR_DOWNLOAD
*& Purpose: Download high-volume data from TADIR to local PC file.
*&          Uses key-paging (SELECT UP TO n ROWS) so gui_download can
*&          run between pages without causing DBIF_RSQL_INVALID_CURSOR.
*&          Classic ABAP syntax - ECC 6.0 compatible.
*&---------------------------------------------------------------------*
REPORT ztadir_download.

TABLES: tadir.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE text-001.
  SELECT-OPTIONS: s_pgmid  FOR tadir-pgmid,
                  s_object FOR tadir-object,
                  s_objnm  FOR tadir-obj_name,
                  s_author FOR tadir-author,
                  s_devcl  FOR tadir-devclass,
                  s_comp   FOR tadir-component,
                  s_crel   FOR tadir-crelease,
                  s_crdat  FOR tadir-created_on,
                  s_delfg  FOR tadir-delflag.
SELECTION-SCREEN END OF BLOCK sel.

SELECTION-SCREEN BEGIN OF BLOCK opt WITH FRAME TITLE text-002.
  PARAMETERS: p_path  TYPE string LOWER CASE,           " PC file (blank = Desktop)
              p_pkg   TYPE i DEFAULT 10000 OBLIGATORY,  " Rows per page
              p_delim TYPE c DEFAULT '|',               " Field delimiter
              p_head  AS CHECKBOX DEFAULT 'X'.          " Include header row
SELECTION-SCREEN END OF BLOCK opt.

*----------------------------------------------------------------------*
* Data Declarations
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_tadir,
         pgmid      TYPE tadir-pgmid,
         object     TYPE tadir-object,
         obj_name   TYPE tadir-obj_name,
         korrnum    TYPE tadir-korrnum,
         srcsystem  TYPE tadir-srcsystem,
         author     TYPE tadir-author,
         srcdep     TYPE tadir-srcdep,
         devclass   TYPE tadir-devclass,
         genflag    TYPE tadir-genflag,
         edtflag    TYPE tadir-edtflag,
         cproject   TYPE tadir-cproject,
         masterlang TYPE tadir-masterlang,
         versid     TYPE tadir-versid,
         paknocheck TYPE tadir-paknocheck,
         objstablty TYPE tadir-objstablty,
         component  TYPE tadir-component,
         crelease   TYPE tadir-crelease,
         delflag    TYPE tadir-delflag,
         translttxt TYPE tadir-translttxt,
         created_on TYPE tadir-created_on,
         check_date TYPE tadir-check_date,
         check_cfg  TYPE tadir-check_cfg,
       END OF ty_tadir.

DATA: lt_data    TYPE STANDARD TABLE OF ty_tadir,
      lt_output  TYPE STANDARD TABLE OF string,
      lv_line    TYPE string,
      lv_field   TYPE string,
      lv_file    TYPE string,
      lv_total   TYPE i,
      lv_fetched TYPE i,
      lv_pct     TYPE i,
      lv_count   TYPE i,
      lv_first   TYPE c VALUE 'X',
      lv_append  TYPE c,
      lv_bom     TYPE c,
      lv_msg     TYPE string,
      lv_desktop TYPE string,
      lv_f4_file TYPE string,
      lv_f4_path TYPE string,
      lv_f4_full TYPE string.

* Last-key holders for paging (primary key: PGMID, OBJECT, OBJ_NAME)
DATA: lv_k_pgmid  TYPE tadir-pgmid,
      lv_k_object TYPE tadir-object,
      lv_k_objnm  TYPE tadir-obj_name.

FIELD-SYMBOLS: <fs> TYPE ty_tadir.

*----------------------------------------------------------------------*
* Macro to build a delimited line
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
      default_file_name = 'TADIR_EXPORT.txt'
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
    CALL METHOD cl_gui_frontend_services=>get_desktop_directory
      CHANGING
        desktop_directory = lv_desktop
      EXCEPTIONS
        OTHERS            = 1.
    CALL METHOD cl_gui_cfw=>flush.
    IF lv_desktop IS NOT INITIAL.
      CONCATENATE lv_desktop '\TADIR_EXPORT.txt' INTO lv_file.
    ELSE.
      lv_file = 'C:\TADIR_EXPORT.txt'.
    ENDIF.
  ELSE.
    lv_file = p_path.
  ENDIF.

* Count records
  SELECT COUNT(*) INTO lv_total
    FROM tadir
    WHERE pgmid      IN s_pgmid
      AND object     IN s_object
      AND obj_name   IN s_objnm
      AND author     IN s_author
      AND devclass   IN s_devcl
      AND component  IN s_comp
      AND crelease   IN s_crel
      AND created_on IN s_crdat
      AND delflag    IN s_delfg.

  IF lv_total = 0.
    MESSAGE 'No records found matching selection criteria.' TYPE 'I'.
    LEAVE PROGRAM.
  ENDIF.

  WRITE: / 'Total records to export:', lv_total.
  ULINE.

* Header row
  IF p_head = 'X'.
    CLEAR lv_line.
    _append_field 'PGMID'.
    _append_field 'OBJECT'.
    _append_field 'OBJ_NAME'.
    _append_field 'KORRNUM'.
    _append_field 'SRCSYSTEM'.
    _append_field 'AUTHOR'.
    _append_field 'SRCDEP'.
    _append_field 'DEVCLASS'.
    _append_field 'GENFLAG'.
    _append_field 'EDTFLAG'.
    _append_field 'CPROJECT'.
    _append_field 'MASTERLANG'.
    _append_field 'VERSID'.
    _append_field 'PAKNOCHECK'.
    _append_field 'OBJSTABLTY'.
    _append_field 'COMPONENT'.
    _append_field 'CRELEASE'.
    _append_field 'DELFLAG'.
    _append_field 'TRANSLTTXT'.
    _append_field 'CREATED_ON'.
    _append_field 'CHECK_DATE'.
    _append_field 'CHECK_CFG'.
    APPEND lv_line TO lt_output.
  ENDIF.

*----------------------------------------------------------------------*
* Key-paging loop - each SELECT completes fully so gui_download between
* pages does NOT cause DBIF_RSQL_INVALID_CURSOR.
*----------------------------------------------------------------------*
  DO.
    CLEAR lt_data.

    IF lv_first = 'X'.
      SELECT pgmid object obj_name korrnum srcsystem author srcdep
             devclass genflag edtflag cproject masterlang versid
             paknocheck objstablty component crelease delflag
             translttxt created_on check_date check_cfg
        FROM tadir
        INTO TABLE lt_data
        UP TO p_pkg ROWS
        WHERE pgmid      IN s_pgmid
          AND object     IN s_object
          AND obj_name   IN s_objnm
          AND author     IN s_author
          AND devclass   IN s_devcl
          AND component  IN s_comp
          AND crelease   IN s_crel
          AND created_on IN s_crdat
          AND delflag    IN s_delfg
        ORDER BY pgmid object obj_name.
    ELSE.
      SELECT pgmid object obj_name korrnum srcsystem author srcdep
             devclass genflag edtflag cproject masterlang versid
             paknocheck objstablty component crelease delflag
             translttxt created_on check_date check_cfg
        FROM tadir
        INTO TABLE lt_data
        UP TO p_pkg ROWS
        WHERE ( pgmid > lv_k_pgmid
             OR ( pgmid = lv_k_pgmid AND object > lv_k_object )
             OR ( pgmid = lv_k_pgmid AND object = lv_k_object
                                     AND obj_name > lv_k_objnm ) )
          AND pgmid      IN s_pgmid
          AND object     IN s_object
          AND obj_name   IN s_objnm
          AND author     IN s_author
          AND devclass   IN s_devcl
          AND component  IN s_comp
          AND crelease   IN s_crel
          AND created_on IN s_crdat
          AND delflag    IN s_delfg
        ORDER BY pgmid object obj_name.
    ENDIF.

    lv_count = lines( lt_data ).
    IF lv_count = 0.
      EXIT.
    ENDIF.

*   Save last key for next page
    READ TABLE lt_data ASSIGNING <fs> INDEX lv_count.
    lv_k_pgmid  = <fs>-pgmid.
    lv_k_object = <fs>-object.
    lv_k_objnm  = <fs>-obj_name.

*   Build delimited output lines
    LOOP AT lt_data ASSIGNING <fs>.
      CLEAR lv_line.
      _append_field <fs>-pgmid.
      _append_field <fs>-object.
      _append_field <fs>-obj_name.
      _append_field <fs>-korrnum.
      _append_field <fs>-srcsystem.
      _append_field <fs>-author.
      _append_field <fs>-srcdep.
      _append_field <fs>-devclass.
      _append_field <fs>-genflag.
      _append_field <fs>-edtflag.
      _append_field <fs>-cproject.
      _append_field <fs>-masterlang.
      _append_field <fs>-versid.
      _append_field <fs>-paknocheck.
      _append_field <fs>-objstablty.
      _append_field <fs>-component.
      _append_field <fs>-crelease.
      _append_field <fs>-delflag.
      _append_field <fs>-translttxt.
      _append_field <fs>-created_on.
      _append_field <fs>-check_date.
      _append_field <fs>-check_cfg.
      APPEND lv_line TO lt_output.
    ENDLOOP.

*   First write creates file with BOM; subsequent writes append
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
        codepage         = '4110'
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

    lv_pct = ( lv_fetched * 100 ) / lv_total.
    WRITE: / 'Exported', lv_fetched, '/', lv_total, 'records (', lv_pct, '% )'.

    IF lv_count < p_pkg.
      EXIT.
    ENDIF.
  ENDDO.

  ULINE.
  WRITE: / 'Export complete.', lv_fetched, 'records written to:'.
  WRITE: / lv_file.
  CONCATENATE 'Export complete.' lv_fetched 'records written.'
              INTO lv_msg SEPARATED BY space.
  MESSAGE lv_msg TYPE 'S'.
