*&---------------------------------------------------------------------*
*& Report ZATC_PROG_VER_ROLLBACK
*&---------------------------------------------------------------------*
*& Step 1 : Create the program given on selection screen and commit.
*& Step 2 : Fetch all versions from VRSD, sort (most recent first),
*&           pick latest-1 (index 2), read its source via
*&           SVRS_GET_VERSION_REPS_40, and update the program via
*&           RPY_PROGRAM_UPDATE.
*&---------------------------------------------------------------------*
REPORT zatc_prog_ver_rollback.

TABLES: trdir.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_version_info,
         versno TYPE vrsd-versno,
         datum  TYPE vrsd-datum,
         zeit   TYPE vrsd-zeit,
         author TYPE vrsd-author,
       END OF ty_version_info.

*----------------------------------------------------------------------*
* Global data
*----------------------------------------------------------------------*
DATA: lt_versions  TYPE TABLE OF ty_version_info,
      wa_version   TYPE ty_version_info,
      lt_source    TYPE STANDARD TABLE OF abaptxt255,
      lv_obj_name  TYPE vrsd-objname,
      lv_versno    TYPE vrsd-versno,
      lv_ver_count TYPE i,
      lv_prog_name TYPE programm.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
PARAMETERS: p_prog TYPE programm   OBLIGATORY.
PARAMETERS: p_type TYPE trdir-subc DEFAULT '1'.   " 1=Exec, M=Module pool, I=Include, F=Func.grp
PARAMETERS: lv_req TYPE trkorr     OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* F4 help for program name (Z*/Y* objects from TRDIR)
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_prog.
  DATA lt_prog_list TYPE TABLE OF rspoplist.

  SELECT name INTO CORRESPONDING FIELDS OF TABLE @lt_prog_list
    FROM trdir
    WHERE name LIKE 'Z%' OR name LIKE 'Y%'.

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
* START OF SELECTION
*======================================================================*
START-OF-SELECTION.

*----------------------------------------------------------------------*
* STEP 1: Create the program
*----------------------------------------------------------------------*
  WRITE: / '=== STEP 1: CREATE PROGRAM ==='.
  ULINE.

  DATA: lt_init_src TYPE STANDARD TABLE OF abaptxt255,
        wa_init_src TYPE abaptxt255.

  CONCATENATE 'REPORT ' p_prog '.' INTO wa_init_src-line SEPARATED BY space.
  APPEND wa_init_src TO lt_init_src.

  CALL FUNCTION 'RPY_PROGRAM_INSERT'
    EXPORTING
      program_name     = p_prog
      program_type     = p_type
      title            = p_prog
      language         = sy-langu
      transport_number = lv_req
    TABLES
      source_extended  = lt_init_src
    EXCEPTIONS
      already_exists   = 1
      permission_error = 2
      OTHERS           = 3.

  CASE sy-subrc.
    WHEN 0.
      COMMIT WORK AND WAIT.
      WRITE: / |Program { p_prog } created and committed.|.
    WHEN 1.
      WRITE: / |Program { p_prog } already exists – skipping creation.|.
    WHEN 2.
      WRITE: / |ERROR: Permission denied creating { p_prog }. Check authorization.|.
      STOP.
    WHEN OTHERS.
      WRITE: / |ERROR: RPY_PROGRAM_INSERT failed (SY-SUBRC: { sy-subrc }).|.
      STOP.
  ENDCASE.

  SKIP.

*----------------------------------------------------------------------*
* STEP 2: Fetch all versions from VRSD and sort
*----------------------------------------------------------------------*
  WRITE: / '=== STEP 2: GET VERSIONS ==='.
  ULINE.

  SELECT versno datum zeit author
    INTO CORRESPONDING FIELDS OF TABLE @lt_versions
    FROM vrsd
    WHERE objname = @p_prog
      AND objtype = 'REPS'
      AND versno  <> '00000'.

  IF sy-subrc <> 0.
    WRITE: / |No version history found in VRSD for { p_prog }.|.
    STOP.
  ENDIF.

  " Sort: most recent date/time first
  SORT lt_versions BY datum DESCENDING
                      zeit  DESCENDING.

  DESCRIBE TABLE lt_versions LINES lv_ver_count.

  " Display full version list
  WRITE: /3 'Idx', 12 'VersionNo', 24 'Date', 36 'Time', 49 'Author'.
  ULINE.
  DATA lv_idx TYPE i VALUE 1.
  LOOP AT lt_versions INTO wa_version.
    WRITE: /3 lv_idx, 12 wa_version-versno, 24 wa_version-datum,
           36 wa_version-zeit, 49 wa_version-author.
    lv_idx = lv_idx + 1.
  ENDLOOP.
  ULINE.
  WRITE: / |Total versions found: { lv_ver_count }|.
  SKIP.

  IF lv_ver_count < 2.
    WRITE: / |Only { lv_ver_count } version(s) found. Need at least 2 to pick latest-1.|.
    STOP.
  ENDIF.

  " Latest version (index 1) – for display only
  READ TABLE lt_versions INTO wa_version INDEX 1.
  WRITE: / |Latest version   : { wa_version-versno } | &&
           |dated { wa_version-datum } at { wa_version-zeit } by { wa_version-author }|.

  " Latest - 1 (index 2) – this is the target version to restore
  READ TABLE lt_versions INTO wa_version INDEX 2.
  lv_versno = wa_version-versno.
  WRITE: / |Target (latest-1): { wa_version-versno } | &&
           |dated { wa_version-datum } at { wa_version-zeit } by { wa_version-author }|.
  SKIP.

*----------------------------------------------------------------------*
* STEP 3: Read source of latest-1 version
*----------------------------------------------------------------------*
  WRITE: / '=== STEP 3: READ VERSION SOURCE ==='.
  ULINE.

  lv_obj_name = p_prog.
  REFRESH lt_source.

  CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
    EXPORTING
      object_name           = lv_obj_name
      versno                = lv_versno
    TABLES
      repos_tab             = lt_source
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3.

  IF sy-subrc <> 0.
    WRITE: / |ERROR: Could not read version { lv_versno } (SY-SUBRC: { sy-subrc }).|.
    STOP.
  ENDIF.

  IF lt_source IS INITIAL.
    WRITE: / |ERROR: Version { lv_versno } returned empty source.|.
    STOP.
  ENDIF.

  DATA lv_line_count TYPE i.
  DESCRIBE TABLE lt_source LINES lv_line_count.
  WRITE: / |Read { lv_line_count } lines from version { lv_versno }.|.
  SKIP.

*----------------------------------------------------------------------*
* STEP 4: Update the program with the latest-1 version source
*----------------------------------------------------------------------*
  WRITE: / '=== STEP 4: UPDATE PROGRAM ==='.
  ULINE.

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
    WRITE: / |SUCCESS: { p_prog } updated with version { lv_versno }|.
    WRITE: / |Dated { wa_version-datum } at { wa_version-zeit } by { wa_version-author }|.

    " Syntax check on restored source
    DATA: lt_syn_errors TYPE syn_error,
          wa_syn_error  TYPE syner_str.

    CALL FUNCTION 'RS_PROGRAM_CHECK_SYNTAX'
      EXPORTING
        program_name = lv_prog_name
        program_type = l_trdir-subc
      TABLES
        error_table  = lt_syn_errors
      EXCEPTIONS
        OTHERS       = 0.

    SKIP.
    IF lt_syn_errors IS INITIAL.
      WRITE: / 'Syntax check: PASSED'.
    ELSE.
      WRITE: / 'Syntax check: ERRORS – review before activating!'.
      LOOP AT lt_syn_errors INTO wa_syn_error.
        WRITE: /5 'Line:', wa_syn_error-zeile, '|', wa_syn_error-mtext.
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
