*&---------------------------------------------------------------------*
*& Report ZATC_PROG_VER_ROLLBACK
*&---------------------------------------------------------------------*
*& Purpose : Given a program name, fetch all its version history,
*&           sort versions (latest first), pick the "latest - 1"
*&           (i.e., the second-most-recent) version, read its source
*&           via SVRS_GET_VERSION_REPS_40, and restore the active
*&           program to that previous version via RPY_PROGRAM_UPDATE.
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
* Data declarations
*----------------------------------------------------------------------*
DATA: lt_versions   TYPE TABLE OF ty_version_info,
      wa_version    TYPE ty_version_info,
      lt_source     TYPE STANDARD TABLE OF abaptxt255,
      lv_obj_name   TYPE vrsd-objname,
      lv_versno     TYPE vrsd-versno,
      lv_ver_count  TYPE i,
      lv_prog_name  TYPE programm.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
PARAMETERS: p_prog TYPE programm OBLIGATORY.
PARAMETERS: lv_req TYPE trkorr   OBLIGATORY.
PARAMETERS: p_sim  TYPE flag     AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* F4 Help for program name
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_prog.
  DATA: lt_prog_list TYPE TABLE OF rspoplist,
        wa_prog_list TYPE rspoplist.

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

*----------------------------------------------------------------------*
* Main processing
*----------------------------------------------------------------------*
START-OF-SELECTION.

  " ----------------------------------------------------------------
  " Step 1: Validate the program exists in TRDIR
  " ----------------------------------------------------------------
  SELECT SINGLE * INTO @DATA(l_trdir)
    FROM trdir
    WHERE name = @p_prog.

  IF sy-subrc <> 0.
    MESSAGE |Program { p_prog } does not exist| TYPE 'E'.
  ENDIF.

  " ----------------------------------------------------------------
  " Step 2: Fetch all versions of the program from VRSD
  "         OBJTYPE = 'REPS' for report/program source text
  "         Exclude versno '00000' which represents the active copy
  " ----------------------------------------------------------------
  SELECT versno datum zeit author
    INTO CORRESPONDING FIELDS OF TABLE @lt_versions
    FROM vrsd
    WHERE objname = @p_prog
      AND objtype = 'REPS'
      AND versno  <> '00000'.

  IF sy-subrc <> 0.
    WRITE: / |No version history found for program { p_prog }|.
    STOP.
  ENDIF.

  " ----------------------------------------------------------------
  " Step 3: Sort versions – most recent first (by date then time)
  " ----------------------------------------------------------------
  SORT lt_versions BY datum DESCENDING
                      zeit  DESCENDING.

  DESCRIBE TABLE lt_versions LINES lv_ver_count.

  " Display full version history for reference
  WRITE: / |Version history for program: { p_prog }|.
  ULINE.
  WRITE: /5 'No.', 10 'VersionNo', 20 'Date',       32 'Time',    45 'Author'.
  ULINE.
  DATA lv_idx TYPE i VALUE 1.
  LOOP AT lt_versions INTO wa_version.
    WRITE: /5 lv_idx, 10 wa_version-versno, 20 wa_version-datum,
           32 wa_version-zeit, 45 wa_version-author.
    lv_idx = lv_idx + 1.
  ENDLOOP.
  ULINE.
  WRITE: / |Total versions found: { lv_ver_count }|.
  SKIP.

  " ----------------------------------------------------------------
  " Step 4: We need at least 2 versions to pick "latest - 1"
  " ----------------------------------------------------------------
  IF lv_ver_count < 2.
    WRITE: / 'ERROR: At least 2 historical versions are required.'.
    WRITE: / |Only { lv_ver_count } version(s) found for { p_prog }. Cannot roll back.|.
    STOP.
  ENDIF.

  " INDEX 1 = latest version (most recent)
  " INDEX 2 = latest - 1 version (target for rollback)
  READ TABLE lt_versions INTO wa_version INDEX 1.
  WRITE: / |Latest version    : { wa_version-versno } dated { wa_version-datum } at { wa_version-zeit } by { wa_version-author }|.

  READ TABLE lt_versions INTO wa_version INDEX 2.
  lv_versno = wa_version-versno.
  WRITE: / |Target version    : { wa_version-versno } dated { wa_version-datum } at { wa_version-zeit } by { wa_version-author }|.
  SKIP.

  " ----------------------------------------------------------------
  " Step 5: Read source code of the target version (latest - 1)
  "         using SVRS_GET_VERSION_REPS_40
  " ----------------------------------------------------------------
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
    WRITE: / |ERROR: Failed to read version { lv_versno } for { p_prog } (SY-SUBRC: { sy-subrc })|.
    STOP.
  ENDIF.

  IF lt_source IS INITIAL.
    WRITE: / |ERROR: Version { lv_versno } returned empty source.|.
    STOP.
  ENDIF.

  DATA lv_line_count TYPE i.
  DESCRIBE TABLE lt_source LINES lv_line_count.
  WRITE: / |Source lines read : { lv_line_count }|.
  SKIP.

  " ----------------------------------------------------------------
  " Step 6a: SIMULATION – write source into a temporary test program
  " ----------------------------------------------------------------
  IF p_sim = 'X'.
    DATA lv_test_prog TYPE programm.
    CONCATENATE 'ZTEST_VER_' sy-uname INTO lv_test_prog.
    CONDENSE lv_test_prog NO-GAPS.
    lv_prog_name = lv_test_prog.

    INSERT REPORT lv_prog_name FROM lt_source.

    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      WRITE: / '[SIMULATION] Backup test program created : ', lv_test_prog.
      WRITE: / '[SIMULATION] Would restore program       : ', p_prog.
      WRITE: / '[SIMULATION] To version                  : ', lv_versno.
      WRITE: / 'To perform actual rollback, uncheck the Simulation flag and re-run.'.
    ELSE.
      WRITE: / '[SIMULATION] ERROR: Could not create test program'.
    ENDIF.

  " ----------------------------------------------------------------
  " Step 6b: REAL MODE – update the active program to the target version
  " ----------------------------------------------------------------
  ELSE.
    " First take a backup of the current active version
    DATA: lt_current_source TYPE STANDARD TABLE OF abaptxt255,
          lv_backup_prog    TYPE programm.

    CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
      EXPORTING
        object_name           = lv_obj_name
        versno                = '00000'
      TABLES
        repos_tab             = lt_current_source
      EXCEPTIONS
        no_version            = 1
        system_failure        = 2
        communication_failure = 3.

    IF sy-subrc = 0 AND lt_current_source IS NOT INITIAL.
      CONCATENATE 'ZBAK_' sy-uname INTO lv_backup_prog.
      CONDENSE lv_backup_prog NO-GAPS.
      lv_prog_name = lv_backup_prog.
      INSERT REPORT lv_prog_name FROM lt_current_source.
      IF sy-subrc = 0.
        COMMIT WORK.
        WRITE: / |Active-version backup saved as: { lv_backup_prog }|.
      ENDIF.
      REFRESH lt_current_source.
    ENDIF.

    " Now update the actual program with the previous version source
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
      WRITE: / |SUCCESS: Program { p_prog } restored to version { lv_versno }|.
      WRITE: / |Version dated { wa_version-datum } at { wa_version-zeit } by { wa_version-author }|.

      " Syntax check on the restored program
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
        WRITE: / 'Syntax check: ERRORS FOUND – review before activating!'.
        LOOP AT lt_syn_errors INTO wa_syn_error.
          WRITE: /5 'Line:', wa_syn_error-zeile, '|', wa_syn_error-mtext.
        ENDLOOP.
      ENDIF.

    ELSE.
      WRITE: / |ERROR: RPY_PROGRAM_UPDATE failed (SY-SUBRC: { sy-subrc })|.
      CASE sy-subrc.
        WHEN 1. WRITE: / 'Reason: Cancelled by user.'.
        WHEN 2. WRITE: / 'Reason: Permission error – check authorization.'.
        WHEN 3. WRITE: / 'Reason: Program not found in transport.'.
        WHEN OTHERS. WRITE: / 'Reason: Unknown error.'.
      ENDCASE.
    ENDIF.

  ENDIF. " p_sim check
