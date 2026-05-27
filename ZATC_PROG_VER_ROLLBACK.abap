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

  SELECT SINGLE * INTO @DATA(l_trdir) FROM trdir WHERE name = @p_prog.

  " ----------------------------------------------------------------
  " Part A: Restore REPS (program source code)
  " ----------------------------------------------------------------
  WRITE: / '=== REPS (Source Code) ==='.
  ULINE.

  DATA: lv_versno_reps TYPE vrsd-versno,
        lt_source      TYPE STANDARD TABLE OF abaptxt255.

  PERFORM get_latest_version USING p_prog 'REPS'
                             CHANGING lv_versno_reps.

  IF lv_versno_reps IS NOT INITIAL.
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

  DATA: lv_versno_rept TYPE vrsd-versno,
        lt_textpool    TYPE STANDARD TABLE OF textpool.

  PERFORM get_latest_version USING p_prog 'REPT'
                             CHANGING lv_versno_rept.

  IF lv_versno_rept IS NOT INITIAL.
    CALL FUNCTION 'SVRS_GET_VERSION_REPT_40'
      EXPORTING
        object_name           = lv_obj_name
        versno                = lv_versno_rept
      TABLES
        ptab                  = lt_textpool
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3.

    IF sy-subrc = 0 AND lt_textpool IS NOT INITIAL.
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
*&  Returns the latest version number for the given objname/objtype.
*&  Falls back to '00000' when no numbered versions exist.
*&  Returns blank versno when the objtype is not present at all.
*&---------------------------------------------------------------------*
FORM get_latest_version USING    p_objname TYPE vrsd-objname
                                 p_objtype TYPE vrsd-objtype
                        CHANGING p_versno  TYPE vrsd-versno.

  DATA: lt_vers TYPE TABLE OF vrsd,
        wa_vers TYPE vrsd.

  CLEAR p_versno.

  SELECT *
    INTO TABLE @lt_vers
    FROM vrsd
    WHERE objname = @p_objname
      AND objtype = @p_objtype
      AND versno  <> '00000'.

  IF sy-subrc = 0 AND lt_vers IS NOT INITIAL.
    " Numbered versions exist – take the most recent
    SORT lt_vers BY datum DESCENDING zeit DESCENDING.
    READ TABLE lt_vers INTO wa_vers INDEX 1.
    p_versno = wa_vers-versno.
    WRITE: / |{ p_objtype }: latest version { wa_vers-versno } | &&
             |dated { wa_vers-datum } { wa_vers-zeit } by { wa_vers-author }|.
  ELSE.
    " No numbered version – fall back to 00000 if it exists
    SELECT SINGLE *
      INTO @wa_vers
      FROM vrsd
      WHERE objname = @p_objname
        AND objtype = @p_objtype
        AND versno  = '00000'.
    IF sy-subrc = 0.
      p_versno = '00000'.
      WRITE: / |{ p_objtype }: no numbered version – using 00000 | &&
               |dated { wa_vers-datum } { wa_vers-zeit } by { wa_vers-author }|.
    ENDIF.
  ENDIF.

ENDFORM.
