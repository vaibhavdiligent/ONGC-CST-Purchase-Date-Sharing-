*&---------------------------------------------------------------------*
*& Report ZATC_PROG_VER_ROLLBACK
*&---------------------------------------------------------------------*
*& Restores objects from version management (table VRSD):
*&   - REPS objtype : program source code  -> RPY_PROGRAM_UPDATE
*&   - REPT objtype : text elements/pool   -> INSERT TEXTPOOL
*&   - TRAN objtype : transaction code     -> RPY_TRANSACTION_UPDATE
*& For each objtype the latest version is taken (falling back to INDEX 1
*& when fewer than 2 versions exist) and re-applied, linked to transport.
*& Loops over distinct object names found in VRSD – no TRDIR dependency.
*&---------------------------------------------------------------------*
REPORT zatc_program_rollback.

TABLES: trdir.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECT-OPTIONS: s_prog FOR trdir-name OBLIGATORY.
PARAMETERS:     lv_req TYPE trkorr OBLIGATORY.

*======================================================================*
START-OF-SELECTION.
*======================================================================*

  TYPES: BEGIN OF ty_obj,
           objname TYPE vrsd-objname,
         END OF ty_obj.

  DATA: lt_objects TYPE STANDARD TABLE OF ty_obj,
        wa_obj     TYPE ty_obj.

  " Fetch all distinct object names that have any version in VRSD
  SELECT DISTINCT objname INTO CORRESPONDING FIELDS OF TABLE @lt_objects
    FROM vrsd WHERE objname IN @s_prog.

  IF lt_objects IS INITIAL.
    WRITE: / 'No version history found in VRSD for selected objects.'.
    STOP.
  ENDIF.

  LOOP AT lt_objects INTO wa_obj.

    DATA: lv_obj_name  TYPE vrsd-objname,
          lv_prog_name TYPE programm.

    lv_obj_name  = wa_obj-objname.
    lv_prog_name = wa_obj-objname.

    WRITE: / '======================================================'.
    WRITE: / |Processing: { lv_obj_name }|.
    WRITE: / '======================================================'.
    ULINE.

    " ----------------------------------------------------------------
    " Part A: Restore REPS (program source code)
    " ----------------------------------------------------------------
    WRITE: / '=== REPS (Source Code) ==='.
    ULINE.

    DATA: lv_versno_reps TYPE vrsd-versno,
          lt_source      TYPE STANDARD TABLE OF abaptxt255,
          lv_found_reps  TYPE abap_bool.

    CLEAR: lv_versno_reps, lt_source, lv_found_reps.

    PERFORM get_latest_version USING lv_obj_name 'REPS'
                               CHANGING lv_versno_reps lv_found_reps.

    IF lv_found_reps = abap_true.

      " Create program if it does not already exist
      SELECT SINGLE name INTO @DATA(lv_existing)
        FROM trdir WHERE name = @lv_prog_name.

      IF sy-subrc <> 0.
        DATA: lt_init_src TYPE STANDARD TABLE OF abaptxt255,
              wa_init_src TYPE abaptxt255.
        CLEAR: lt_init_src, wa_init_src.
        CONCATENATE 'REPORT ' lv_prog_name '.' INTO wa_init_src-line SEPARATED BY space.
        APPEND wa_init_src TO lt_init_src.
        INSERT REPORT lv_prog_name FROM lt_init_src.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
          WRITE: / |Program { lv_prog_name } created successfully.|.
        ELSE.
          WRITE: / |ERROR: INSERT REPORT failed (SY-SUBRC: { sy-subrc }) – skipping REPS.|.
          CLEAR lv_found_reps.
        ENDIF.
      ELSE.
        WRITE: / |Program { lv_prog_name } already exists – skipping creation.|.
      ENDIF.

    ENDIF.

    IF lv_found_reps = abap_true.
      CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
        EXPORTING
          object_name           = lv_obj_name
          versno                = lv_versno_reps
        TABLES
          repos_tab             = lt_source
        EXCEPTIONS
          no_version            = 1
          system_failure        = 2
          communication_failure = 3.

      IF sy-subrc = 0 AND lt_source IS NOT INITIAL.
        " Repair broken "SORT <tab> BY" statements left by S/4 ATC
        " remediation (sort fields stripped, period swallowed into a
        " comment) which otherwise leave the SORT statement unterminated.
        PERFORM fix_broken_sort CHANGING lt_source.

        SELECT SINGLE * INTO @DATA(l_trdir) FROM trdir WHERE name = @lv_prog_name.
        CALL FUNCTION 'RPY_PROGRAM_UPDATE'
          EXPORTING
            program_name     = lv_prog_name
            program_type     = l_trdir-subc
            transport_number = lv_req
          TABLES
            source_extended  = lt_source
          EXCEPTIONS
            cancelled        = 1
            permission_error = 2
            not_found        = 3
            OTHERS           = 4.

        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
          WRITE: / |REPS restored from version { lv_versno_reps } and linked to { lv_req }.|.
        ELSE.
          WRITE: / |ERROR: RPY_PROGRAM_UPDATE failed (SY-SUBRC: { sy-subrc }).|.
        ENDIF.
      ELSE.
        WRITE: / |ERROR: Could not fetch REPS version { lv_versno_reps } (SY-SUBRC: { sy-subrc }).|.
      ENDIF.
    ELSE.
      WRITE: / |No REPS version found in VRSD for { lv_obj_name }.|.
    ENDIF.

    SKIP.

    " ----------------------------------------------------------------
    " Part B: Restore REPT (text elements / text pool)
    " ----------------------------------------------------------------
    WRITE: / '=== REPT (Text Elements) ==='.
    ULINE.

    DATA: lv_versno_rept TYPE vrsd-versno,
          lt_textpoolt   TYPE STANDARD TABLE OF textpoolt,
          lt_textpool    TYPE STANDARD TABLE OF textpool,
          wa_textpoolt   TYPE textpoolt,
          wa_textpool    TYPE textpool,
          lv_found_rept  TYPE abap_bool.

    CLEAR: lv_versno_rept, lt_textpoolt, lt_textpool, lv_found_rept.

    PERFORM get_latest_version USING lv_obj_name 'REPT'
                               CHANGING lv_versno_rept lv_found_rept.

    IF lv_found_rept = abap_true.
      CALL FUNCTION 'SVRS_GET_VERSION_REPT_40'
        EXPORTING
          object_name           = lv_obj_name
          versno                = lv_versno_rept
        TABLES
          repot_tab             = lt_textpoolt
        EXCEPTIONS
          no_version            = 1
          system_failure        = 2
          communication_failure = 3.

      IF sy-subrc = 0 AND lt_textpoolt IS NOT INITIAL.
        REFRESH lt_textpool.
        LOOP AT lt_textpoolt INTO wa_textpoolt.
          MOVE-CORRESPONDING wa_textpoolt TO wa_textpool.
          APPEND wa_textpool TO lt_textpool.
          CLEAR wa_textpool.
        ENDLOOP.
        " Delete existing text pool first so INSERT overwrites, not skips
        DELETE TEXTPOOL lv_prog_name LANGUAGE sy-langu.
        INSERT TEXTPOOL lv_prog_name FROM lt_textpool LANGUAGE sy-langu.
        IF sy-subrc = 0.
          COMMIT WORK AND WAIT.
          WRITE: / |REPT (text pool) restored from version { lv_versno_rept }.|.
        ELSE.
          WRITE: / |ERROR: INSERT TEXTPOOL failed (SY-SUBRC: { sy-subrc }).|.
        ENDIF.
      ELSE.
        WRITE: / |No text-pool data fetched for REPT version { lv_versno_rept } (SY-SUBRC: { sy-subrc }).|.
      ENDIF.
    ELSE.
      WRITE: / |No REPT version found in VRSD for { lv_obj_name }.|.
    ENDIF.

    SKIP.

    " ----------------------------------------------------------------
    " Part C: Restore TRAN (transaction code)
    " ----------------------------------------------------------------
    WRITE: / '=== TRAN (Transaction Code) ==='.
    ULINE.

    DATA: lv_versno_tran TYPE vrsd-versno,
          lv_found_tran  TYPE abap_bool,
          lt_tcodes      TYPE STANDARD TABLE OF tstc,
          lt_gui_attr    TYPE STANDARD TABLE OF tstcc,
          wa_tstc        TYPE tstc,
          wa_tstct       TYPE tstct,
          lv_tcode       TYPE tstc-tcode.

    CLEAR: lv_versno_tran, lv_found_tran, lt_tcodes, lt_gui_attr, lv_tcode.

    PERFORM get_latest_version USING lv_obj_name 'TRAN'
                               CHANGING lv_versno_tran lv_found_tran.

    IF lv_found_tran = abap_true.

      lv_tcode = lv_obj_name.

      " Read current transaction definition
      CALL FUNCTION 'RPY_TRANSACTION_READ'
        EXPORTING
          transaction    = lv_tcode
        TABLES
          tcodes         = lt_tcodes
          gui_attributes = lt_gui_attr
        EXCEPTIONS
          permission_error = 1
          cancelled        = 2
          not_found        = 3
          object_not_found = 4
          OTHERS           = 5.

      IF sy-subrc = 0 AND lt_tcodes IS NOT INITIAL.
        READ TABLE lt_tcodes INTO wa_tstc INDEX 1.

        " Read transaction short text
        SELECT SINGLE * INTO @wa_tstct FROM tstct
          WHERE sprsl = @sy-langu AND tcode = @lv_tcode.

        " Re-insert transaction linked to transport request
        CALL FUNCTION 'RPY_TRANSACTION_INSERT'
          EXPORTING
            transaction      = lv_tcode
            program          = wa_tstc-pgmna
            dynpro           = wa_tstc-dypno
            language         = sy-langu
            transport_number = lv_req
            shorttext        = wa_tstct-ttext
          EXCEPTIONS
            cancelled        = 1
            already_exist    = 2
            permission_error = 3
            name_not_allowed = 4
            name_conflict    = 5
            illegal_type     = 6
            object_inconsistent = 7
            db_access_error  = 8
            OTHERS           = 9.

        IF sy-subrc = 0 OR sy-subrc = 2.
          COMMIT WORK AND WAIT.
          IF sy-subrc = 2.
            WRITE: / |TRAN { lv_obj_name } already exists – linked to transport { lv_req }.|.
          ELSE.
            WRITE: / |TRAN { lv_obj_name } inserted and linked to { lv_req }.|.
          ENDIF.
          WRITE: / |  (Version info: { lv_versno_tran } found in VRSD)|.
        ELSE.
          WRITE: / |ERROR: RPY_TRANSACTION_INSERT failed (SY-SUBRC: { sy-subrc }).|.
        ENDIF.
      ELSE.
        WRITE: / |ERROR: RPY_TRANSACTION_READ failed (SY-SUBRC: { sy-subrc }).|.
      ENDIF.
    ELSE.
      WRITE: / |No TRAN version found in VRSD for { lv_obj_name }.|.
    ENDIF.

    SKIP.

    " ----------------------------------------------------------------
    " Final Step: Clear TADIR deletion flag
    " ----------------------------------------------------------------
    WRITE: / '=== TADIR Deletion Flag ==='.
    ULINE.

    UPDATE tadir SET delfag = space
      WHERE pgmid   = 'R3TR'
        AND obj_name = lv_obj_name
        AND delfag   = 'X'.

    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      WRITE: / |TADIR deletion flag cleared for { lv_obj_name }.|.
    ELSE.
      WRITE: / |TADIR: no deletion flag found or already cleared for { lv_obj_name }.|.
    ENDIF.

    SKIP.
    WRITE: / |Restore complete for { lv_obj_name }.|.
    ULINE.
    SKIP.

  ENDLOOP.

  WRITE: / 'All objects processed.'.

*&---------------------------------------------------------------------*
*& Form get_latest_version
*&  Uses SVRS_GET_VERSION_DIRECTORY_40 to fetch all versions.
*&  Sorts by date/time descending and returns INDEX 2 (latest-1).
*&  Falls back to INDEX 1 if fewer than 2 versions exist.
*&---------------------------------------------------------------------*
FORM get_latest_version USING    p_objname LIKE vrsd-objname
                                 p_objtype LIKE vrsd-objtype
                        CHANGING p_versno  LIKE vrsd-versno
                                 p_found   TYPE abap_bool.

  DATA: lt_version_list  TYPE STANDARD TABLE OF vrsd_40a,
        lt_lversno_list  TYPE STANDARD TABLE OF vrsn,
        wa_version       TYPE vrsd_40a,
        lv_fm_objname    LIKE vrsd_40a-objname,
        lv_fm_objtype    LIKE vrsd_40a-objtype.

  CLEAR: p_versno, p_found.

  lv_fm_objname = p_objname.
  lv_fm_objtype = p_objtype.

  CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_40'
    EXPORTING
      objname               = lv_fm_objname
      objtype               = lv_fm_objtype
    TABLES
      lversno_list          = lt_lversno_list
      version_list          = lt_version_list
    EXCEPTIONS
      no_entry              = 1
      communication_failure = 2
      system_failure        = 3.

  IF sy-subrc <> 0 OR lt_version_list IS INITIAL.
    WRITE: / |{ p_objtype }: no versions found in version directory.|.
    RETURN.
  ENDIF.

  " Sort most recent first
  SORT lt_version_list BY datum DESCENDING zeit DESCENDING.

  " Take index 2 (latest - 1)
  READ TABLE lt_version_list INTO wa_version INDEX 2.
  IF sy-subrc = 0.
    p_versno = wa_version-versno.
    p_found  = abap_true.
    WRITE: / |{ p_objtype }: version { wa_version-versno } | &&
             |dated { wa_version-datum } { wa_version-zeit } by { wa_version-author }|.
  ELSE.
    " Only 1 version exists – fall back to index 1
    READ TABLE lt_version_list INTO wa_version INDEX 1.
    IF sy-subrc = 0.
      p_versno = wa_version-versno.
      p_found  = abap_true.
      WRITE: / |{ p_objtype }: only 1 version found – using { wa_version-versno } | &&
               |dated { wa_version-datum } { wa_version-zeit } by { wa_version-author }|.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form fix_broken_sort
*&  Repairs "SORT <tab> BY" statements that the S/4 ATC remediation
*&  tool left broken: the sort fields were stripped and the closing
*&  period was swallowed into a trailing comment (e.g.
*&        SORT L_I_PAY BY "Update.
*&  which leaves the SORT statement unterminated and causes the parser
*&  to consume the following lines (LOOP / IF / FORM) as sort fields.
*&  Such lines are rewritten to: <indent>SORT <tab>.
*&---------------------------------------------------------------------*
FORM fix_broken_sort CHANGING ct_source TYPE STANDARD TABLE.

  FIELD-SYMBOLS <ls_line> TYPE abaptxt255.

  DATA: lv_code   TYPE string,
        lv_chk    TYPE string,
        lv_indent TYPE string,
        lv_tab    TYPE string,
        lv_w1     TYPE string,
        lv_rest   TYPE string,
        lv_pos    TYPE i,
        lv_off    TYPE i.

  LOOP AT ct_source ASSIGNING <ls_line>.

    " Skip full-line comments
    IF <ls_line>-line(1) = '*'.
      CONTINUE.
    ENDIF.

    " A broken remediation line has the period swallowed in a comment,
    " so it must contain a '"' quote
    FIND '"' IN <ls_line>-line MATCH OFFSET lv_pos.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    " Code portion before the comment
    lv_code = <ls_line>-line(lv_pos).

    " Normalise for checking
    lv_chk = lv_code.
    CONDENSE lv_chk.
    TRANSLATE lv_chk TO UPPER CASE.

    " Match: SORT <name> BY  with nothing (no fields) after BY
    FIND REGEX '^SORT\s+(\S+)\s+BY$' IN lv_chk.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    " Second token = the internal table name
    SPLIT lv_chk AT space INTO lv_w1 lv_tab lv_rest.

    " Preserve original leading indentation
    lv_off = 0.
    WHILE lv_off < strlen( <ls_line>-line )
      AND <ls_line>-line+lv_off(1) = space.
      lv_off = lv_off + 1.
    ENDWHILE.
    IF lv_off > 0.
      lv_indent = <ls_line>-line(lv_off).
    ELSE.
      CLEAR lv_indent.
    ENDIF.

    " Rebuild as: <indent>SORT <table>.
    <ls_line>-line = lv_indent && 'SORT ' && lv_tab && '.'.

  ENDLOOP.

ENDFORM.
