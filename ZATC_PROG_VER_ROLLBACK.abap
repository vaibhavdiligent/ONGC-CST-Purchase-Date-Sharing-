*&---------------------------------------------------------------------*
*& Report ZATC_PROG_VER_ROLLBACK
*&---------------------------------------------------------------------*
*& Restores a program from version management (table VRSD):
*&   - REPS objtype : program source code  -> RPY_PROGRAM_UPDATE
*&   - REPT objtype : text elements/pool   -> INSERT TEXTPOOL
*& For each objtype the latest version is taken (falling back to 00000
*& when no numbered version exists) and re-applied to the program,
*& linked to the supplied transport request.
*&---------------------------------------------------------------------*
REPORT zatc_program_rollback.

TABLES: trdir.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
PARAMETERS: p_prog TYPE programm OBLIGATORY.
PARAMETERS: lv_req TYPE trkorr   OBLIGATORY.

*----------------------------------------------------------------------*
* F4 help for program name
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_prog.
  TYPES: BEGIN OF ty_prog,
           name TYPE trdir-name,
         END OF ty_prog.
  DATA lt_prog_list TYPE STANDARD TABLE OF ty_prog.
  SELECT name INTO CORRESPONDING FIELDS OF TABLE @lt_prog_list
    FROM trdir WHERE name LIKE 'Z%' OR name LIKE 'Y%'.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'NAME'
      dynpprog        = sy-cprog
      dynpnr          = sy-dynnr
      dynprofield     = 'P_PROG'
      stepl           = 0
      value_org       = 'S'
    TABLES
      value_tab       = lt_prog_list
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

*======================================================================*
START-OF-SELECTION.
*======================================================================*

  DATA: lv_obj_name  TYPE vrsd-objname,
        lv_prog_name TYPE programm.

  lv_obj_name  = p_prog.
  lv_prog_name = p_prog.

  " ----------------------------------------------------------------
  " Step 1: Create the program (if it does not already exist)
  " ----------------------------------------------------------------
  WRITE: / '=== STEP 1: CREATE PROGRAM ==='.
  ULINE.

  DATA: lt_init_src TYPE STANDARD TABLE OF abaptxt255,
        wa_init_src TYPE abaptxt255.

  " Check if program already exists
  SELECT SINGLE name INTO @DATA(lv_existing)
    FROM trdir WHERE name = @p_prog.

  IF sy-subrc <> 0.
    " Program does not exist – create it with a minimal stub
    CONCATENATE 'REPORT ' p_prog '.' INTO wa_init_src-line SEPARATED BY space.
    APPEND wa_init_src TO lt_init_src.
    INSERT REPORT p_prog FROM lt_init_src.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      WRITE: / |Program { p_prog } created successfully.|.
    ELSE.
      WRITE: / |ERROR: INSERT REPORT failed (SY-SUBRC: { sy-subrc }).|.
      STOP.
    ENDIF.
  ELSE.
    WRITE: / |Program { p_prog } already exists – skipping creation.|.
  ENDIF.

  SKIP.

  SELECT SINGLE * INTO @DATA(l_trdir) FROM trdir WHERE name = @p_prog.

  " ----------------------------------------------------------------
  " Part A: Restore REPS (program source code)
  " ----------------------------------------------------------------
  WRITE: / '=== REPS (Source Code) ==='.
  ULINE.

  DATA: lv_versno_reps TYPE vrsd-versno,
        lt_source      TYPE STANDARD TABLE OF abaptxt255,
        lv_found_reps  TYPE abap_bool.

  PERFORM get_latest_version USING lv_obj_name 'REPS'
                             CHANGING lv_versno_reps lv_found_reps.

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
    WRITE: / |No REPS version found in VRSD for { p_prog }.|.
  ENDIF.

  SKIP.

  " ----------------------------------------------------------------
  " Part B: Restore REPT (text elements / text pool)
  " ----------------------------------------------------------------
  WRITE: / '=== REPT (Text Elements) ==='.
  ULINE.

  DATA: lv_versno_rept  TYPE vrsd-versno,
        lt_textpoolt    TYPE STANDARD TABLE OF textpoolt,
        lt_textpool     TYPE STANDARD TABLE OF textpool,
        wa_textpoolt    TYPE textpoolt,
        wa_textpool     TYPE textpool,
        lv_found_rept   TYPE abap_bool.

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
      " Convert TEXTPOOLT -> TEXTPOOL (INSERT TEXTPOOL requires TEXTPOOL type)
      REFRESH lt_textpool.
      LOOP AT lt_textpoolt INTO wa_textpoolt.
        MOVE-CORRESPONDING wa_textpoolt TO wa_textpool.
        APPEND wa_textpool TO lt_textpool.
        CLEAR wa_textpool.
      ENDLOOP.
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
    WRITE: / |No REPT version found in VRSD for { p_prog }.|.
  ENDIF.

  SKIP.

  WRITE: / 'Restore complete.'.

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

  DATA: lt_version_list  TYPE STANDARD TABLE OF vrsd,
        lt_lversno_list  TYPE STANDARD TABLE OF vrsn,
        wa_version       TYPE vrsd.

  CLEAR: p_versno, p_found.

  CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_40'
    EXPORTING
      objname               = p_objname
      objtype               = p_objtype
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
