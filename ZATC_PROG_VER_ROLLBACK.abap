*&---------------------------------------------------------------------*
*& Report ZATC_PROG_VER_ROLLBACK
*&---------------------------------------------------------------------*
REPORT zatc_prog_ver_rollback.

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
  DATA lt_prog_list TYPE TABLE OF rspoplist.
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

  DATA: lt_versions  TYPE TABLE OF vrsd,
        wa_version   TYPE vrsd,
        lt_source    TYPE STANDARD TABLE OF abaptxt255,
        lv_obj_name  TYPE vrsd-objname,
        lv_obj_type  TYPE vrsd-objtype,
        lv_prog_name TYPE programm.

  " ----------------------------------------------------------------
  " Step 1: Determine object type from VRSD for this program
  " ----------------------------------------------------------------
  SELECT SINGLE objtype
    INTO @lv_obj_type
    FROM vrsd
    WHERE objname = @p_prog.

  IF sy-subrc <> 0.
    WRITE: / |ERROR: { p_prog } not found in VRSD at all.|.
    STOP.
  ENDIF.

  WRITE: / |Object type from VRSD: { lv_obj_type }|.

  " ----------------------------------------------------------------
  " Step 2: Read all numbered versions from VRSD using fetched objtype
  " ----------------------------------------------------------------
  SELECT *
    INTO TABLE @lt_versions
    FROM vrsd
    WHERE objname = @p_prog
      AND objtype = @lv_obj_type
      AND versno  <> '00000'.

  IF sy-subrc <> 0 OR lt_versions IS INITIAL.
    " No numbered versions – fall back to version 00000
    WRITE: / |No numbered versions found – falling back to version 00000.|.

    SELECT SINGLE *
      INTO @wa_version
      FROM vrsd
      WHERE objname = @p_prog
        AND objtype = @lv_obj_type
        AND versno  = '00000'.

    IF sy-subrc <> 0.
      WRITE: / |ERROR: No version data found in VRSD for { p_prog }.|.
      STOP.
    ENDIF.
  ELSE.
    " Sort descending – index 1 = latest version
    SORT lt_versions BY datum DESCENDING zeit DESCENDING.
    READ TABLE lt_versions INTO wa_version INDEX 1.
  ENDIF.

  WRITE: / |Program    : { p_prog }|.
  WRITE: / |Object Type: { wa_version-objtype }|.
  WRITE: / |Version    : { wa_version-versno }|.
  WRITE: / |Date/Time  : { wa_version-datum } { wa_version-zeit }|.
  WRITE: / |Author     : { wa_version-author }|.
  SKIP.

  " ----------------------------------------------------------------
  " Step 3: Fetch source code of the selected version
  " ----------------------------------------------------------------
  lv_obj_name = p_prog.

  CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
    EXPORTING
      object_name           = lv_obj_name
      versno                = wa_version-versno
    TABLES
      repos_tab             = lt_source
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3.

  IF sy-subrc <> 0.
    WRITE: / |ERROR: Could not fetch version { wa_version-versno } (SY-SUBRC: { sy-subrc }).|.
    STOP.
  ENDIF.

  IF lt_source IS INITIAL.
    WRITE: / |ERROR: Version { wa_version-versno } returned empty source.|.
    STOP.
  ENDIF.

  DATA lv_lines TYPE i.
  DESCRIBE TABLE lt_source LINES lv_lines.
  WRITE: / |Source lines fetched: { lv_lines }|.
  SKIP.

  " ----------------------------------------------------------------
  " Step 4: Insert source into the program and link to transport
  " ----------------------------------------------------------------
  SELECT SINGLE * INTO @DATA(l_trdir) FROM trdir WHERE name = @p_prog.
  lv_prog_name = p_prog.

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
    WRITE: / |SUCCESS: { p_prog } (type: { wa_version-objtype }) updated from version { wa_version-versno } and linked to { lv_req }.|.

    " Syntax check
    DATA: lt_errors TYPE syn_error,
          wa_error  TYPE syner_str.

    CALL FUNCTION 'RS_PROGRAM_CHECK_SYNTAX'
      EXPORTING
        program_name = lv_prog_name
        program_type = l_trdir-subc
      TABLES
        error_table  = lt_errors
      EXCEPTIONS
        OTHERS       = 0.

    SKIP.
    IF lt_errors IS INITIAL.
      WRITE: / 'Syntax check: PASSED'.
    ELSE.
      WRITE: / 'Syntax check: ERRORS found – review before activating!'.
      LOOP AT lt_errors INTO wa_error.
        WRITE: /5 'Line:', wa_error-zeile, '|', wa_error-mtext.
      ENDLOOP.
    ENDIF.

  ELSE.
    WRITE: / |ERROR: RPY_PROGRAM_UPDATE failed (SY-SUBRC: { sy-subrc })|.
    CASE sy-subrc.
      WHEN 1. WRITE: / 'Reason: Cancelled by user.'.
      WHEN 2. WRITE: / 'Reason: Permission error – check authorization.'.
      WHEN 3. WRITE: / 'Reason: Program not found.'.
      WHEN OTHERS. WRITE: / 'Reason: Unknown error.'.
    ENDCASE.
  ENDIF.
