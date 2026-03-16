*&---------------------------------------------------------------------*
*& Report YGMS_CST_EXCLUDE
*&---------------------------------------------------------------------*
*& Exclusion of Material for Allocation - Create, Edit and View
*& Table: YRGA_CST_EXCLUDE
*&---------------------------------------------------------------------*
REPORT ygms_cst_exclude.
*----------------------------------------------------------------------*
* TYPE-POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: slis, icon.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: yrga_cst_exclude, oifspbl, mara, marc, t005u.
*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_exclude,
         sel          TYPE c LENGTH 1,
         state_code   TYPE regio,
         state        TYPE bezei20,
         location     TYPE yrga_cst_exclude-location,
         material     TYPE yrga_cst_exclude-material,
         valid_from   TYPE yrga_cst_exclude-valid_from,
         valid_to     TYPE yrga_cst_exclude-valid_to,
         created_by   TYPE yrga_cst_exclude-created_by,
         created_on   TYPE yrga_cst_exclude-created_on,
         created_time TYPE yrga_cst_exclude-created_time,
         changed_by   TYPE yrga_cst_exclude-changed_by,
         changed_on   TYPE yrga_cst_exclude-changed_on,
         changed_time TYPE yrga_cst_exclude-changed_time,
         deleted      TYPE yrga_cst_exclude-deleted,
       END OF ty_exclude.
*----------------------------------------------------------------------*
* DATA DECLARATIONS
*----------------------------------------------------------------------*
DATA: gt_exclude   TYPE STANDARD TABLE OF ty_exclude,
      gs_exclude   TYPE ty_exclude,
      gt_fieldcat  TYPE slis_t_fieldcat_alv,
      gs_fieldcat  TYPE slis_fieldcat_alv,
      gs_layout    TYPE slis_layout_alv,
      gv_repid     TYPE sy-repid.
*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
* Block B00 - Mode selection (Create / Edit / View)
SELECTION-SCREEN BEGIN OF BLOCK b00 WITH FRAME TITLE TEXT-b00.
  PARAMETERS: p_create RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND mode,
              p_edit   RADIOBUTTON GROUP rb1,
              p_view   RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b00.
* Block B01 - Create Exclusion Entry
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  PARAMETERS: p_state  TYPE regio MODIF ID CRE,
              p_stname TYPE bezei20 MODIF ID SNM,
              p_loc    TYPE yrga_cst_exclude-location MODIF ID CRE,
              p_mat    TYPE yrga_cst_exclude-material MODIF ID CRE,
              p_vfrom  TYPE yrga_cst_exclude-valid_from MODIF ID CRE,
              p_vto    TYPE yrga_cst_exclude-valid_to MODIF ID CRE.
SELECTION-SCREEN END OF BLOCK b01.
* Block B02 - Edit / View Exclusion Entry
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.
  PARAMETERS: p_state2 TYPE regio MODIF ID EVW,
              p_loc2   TYPE yrga_cst_exclude-location MODIF ID EVW,
              p_mat2   TYPE yrga_cst_exclude-material MODIF ID EVW.
SELECTION-SCREEN END OF BLOCK b02.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT - Dynamic screen control
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_create = 'X'.
*     Create mode: show Create block, hide Edit/View block
      IF screen-group1 = 'EVW'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
*     State name field is always display-only
      IF screen-group1 = 'SNM'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
*     Edit/View mode: show Edit/View block, hide Create block
      IF screen-group1 = 'CRE' OR screen-group1 = 'SNM'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST - F4 help for state codes
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_state.
  PERFORM f4_help_state USING 'P_STATE'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_state2.
  PERFORM f4_help_state USING 'P_STATE2'.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON p_state - Derive state name on state code change
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON p_state.
  IF sy-ucomm = 'MODE'.
    EXIT.
  ENDIF.
  IF p_state IS NOT INITIAL.
    PERFORM get_state_name USING p_state CHANGING p_stname.
  ELSE.
    CLEAR p_stname.
  ENDIF.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN - Input validations
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
* Skip validation when switching radio buttons
  CHECK sy-ucomm <> 'MODE'.
  IF p_create = 'X'.
*   Mandatory field checks for Create mode
    IF p_state IS INITIAL.
      MESSAGE 'State Code is mandatory.' TYPE 'E'.
    ENDIF.
    IF p_loc IS INITIAL.
      MESSAGE 'Supply Location is mandatory.' TYPE 'E'.
    ENDIF.
    IF p_mat IS INITIAL.
      MESSAGE 'Material is mandatory.' TYPE 'E'.
    ENDIF.
    IF p_vfrom IS INITIAL.
      MESSAGE 'Valid From date is mandatory.' TYPE 'E'.
    ENDIF.
    IF p_vto IS INITIAL.
      MESSAGE 'Valid To date is mandatory.' TYPE 'E'.
    ENDIF.
*   Validate Valid From is start of a fortnight (1st or 16th)
    IF p_vfrom IS NOT INITIAL AND
       p_vfrom+6(2) <> '01' AND p_vfrom+6(2) <> '16'.
      MESSAGE 'Valid From can only be the start date of a fortnight (1st or 16th).' TYPE 'E'.
    ENDIF.
*   Validate Valid To is end of a fortnight (15th or last day of month)
    IF p_vto IS NOT INITIAL.
      PERFORM validate_fortnight_end USING p_vto.
    ENDIF.
  ELSEIF p_edit = 'X' OR p_view = 'X'.
*   Mandatory field check for Edit/View mode
    IF p_state2 IS INITIAL.
      MESSAGE 'State Code is mandatory.' TYPE 'E'.
    ENDIF.
  ENDIF.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  gv_repid = sy-repid.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF p_create = 'X'.
    PERFORM create_exclusion.
  ELSEIF p_edit = 'X'.
    PERFORM edit_exclusion.
  ELSEIF p_view = 'X'.
    PERFORM view_exclusion.
  ENDIF.
*&---------------------------------------------------------------------*
*& Form F4_HELP_STATE
*&---------------------------------------------------------------------*
*& Provide F4 help for state code using T005U
*&---------------------------------------------------------------------*
FORM f4_help_state USING pv_field TYPE string.
  DATA: lt_return TYPE STANDARD TABLE OF ddshretval,
        ls_return TYPE ddshretval,
        lt_t005u  TYPE STANDARD TABLE OF t005u,
        ls_t005u  TYPE t005u.
  DATA: lt_dynpfields TYPE STANDARD TABLE OF dynpread,
        ls_dynpfield  TYPE dynpread.
* Fetch Indian state codes from T005U
  SELECT * FROM t005u
    INTO TABLE lt_t005u
    WHERE spras = sy-langu
      AND land1 = 'IN'.
  IF lt_t005u IS INITIAL.
    MESSAGE 'No state codes found.' TYPE 'I'.
    RETURN.
  ENDIF.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'BLAND'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = pv_field
      value_org       = 'S'
    TABLES
      value_tab       = lt_t005u
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_STATE_NAME
*&---------------------------------------------------------------------*
*& Derive state name from state code via T005U
*&---------------------------------------------------------------------*
FORM get_state_name USING    pv_state_code TYPE regio
                    CHANGING pv_state_name TYPE bezei20.
  CLEAR pv_state_name.
  SELECT SINGLE bezei FROM t005u
    INTO pv_state_name
    WHERE spras = sy-langu
      AND land1 = 'IN'
      AND bland = pv_state_code.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form VALIDATE_FORTNIGHT_END
*&---------------------------------------------------------------------*
*& Validate that a date is the end of a fortnight (15th or last day)
*&---------------------------------------------------------------------*
FORM validate_fortnight_end USING pv_date TYPE datum.
  DATA: lv_day       TYPE numc2,
        lv_last_day  TYPE datum,
        lv_year      TYPE numc4,
        lv_month     TYPE numc2.
  lv_day   = pv_date+6(2).
  lv_year  = pv_date+0(4).
  lv_month = pv_date+4(2).
* Valid To must be 15th or last day of the month
  IF lv_day = '15'.
    RETURN. "Valid
  ENDIF.
* Check if it is the last day of the month
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = pv_date
    IMPORTING
      last_day_of_month = lv_last_day
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
  IF sy-subrc = 0 AND pv_date = lv_last_day.
    RETURN. "Valid - last day of month
  ENDIF.
  MESSAGE 'Valid To can only be the end date of a fortnight (15th or last day of month).' TYPE 'E'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form VALIDATE_LOCATION
*&---------------------------------------------------------------------*
*& 9.1.1 - Validate location ID against OIFSPBL (skip for wildcard)
*&---------------------------------------------------------------------*
FORM validate_location USING pv_location TYPE yrga_cst_exclude-location
                       CHANGING pv_valid TYPE abap_bool.
  DATA: lv_pblnr TYPE oifspbl-pblnr,
        lv_msg   TYPE char200.
  pv_valid = abap_true.
* Allow wildcard
  IF pv_location = '*'.
    RETURN.
  ENDIF.
  SELECT SINGLE pblnr FROM oifspbl
    INTO lv_pblnr
    WHERE pblnr = pv_location
      AND pbltyp = 'YDVN'.
  IF sy-subrc <> 0.
    CONCATENATE pv_location ' is not a valid purchase location.' INTO lv_msg.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Error'
        txt1  = lv_msg
        txt2  = space.
    pv_valid = abap_false.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form VALIDATE_MATERIAL
*&---------------------------------------------------------------------*
*& 9.1.2 to 9.1.4 - Validate material (GMS check, MARA, MARC)
*&---------------------------------------------------------------------*
FORM validate_material USING pv_material  TYPE yrga_cst_exclude-material
                              pv_valid_to TYPE datum
                       CHANGING pv_valid  TYPE abap_bool.
  DATA: lv_matnr TYPE mara-matnr,
        lv_mstde TYPE mara-mstde,
        lv_uomgr TYPE marc-uomgr,
        lv_msg   TYPE char200.
  pv_valid = abap_true.
* Allow wildcard
  IF pv_material = '*'.
    RETURN.
  ENDIF.
* 9.1.2 - Validate material begins with GMS
  IF pv_material(3) <> 'GMS'.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Error'
        txt1  = 'Please enter a valid GMS material.'
        txt2  = space.
    pv_valid = abap_false.
    RETURN.
  ENDIF.
* 9.1.3 - Check material block status in MARA
  SELECT SINGLE matnr mstde FROM mara
    INTO (lv_matnr, lv_mstde)
    WHERE matnr = pv_material
      AND mstae = 'Z1'.
  IF sy-subrc = 0.
    IF pv_valid_to >= lv_mstde.
      CONCATENATE 'Material ' pv_material ' has been blocked with effect from ' lv_mstde '.'
        INTO lv_msg.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Error'
          txt1  = lv_msg
          txt2  = space.
      pv_valid = abap_false.
      RETURN.
    ENDIF.
  ENDIF.
* 9.1.4 - Check UoM group in MARC
  SELECT SINGLE uomgr FROM marc
    INTO lv_uomgr
    WHERE matnr = pv_material
      AND uomgr = '6'.
  IF sy-subrc <> 0.
    CONCATENATE pv_material ' does not belong to 2 UoM group.' INTO lv_msg.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Error'
        txt1  = lv_msg
        txt2  = space.
    pv_valid = abap_false.
    RETURN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_EXCLUSION
*&---------------------------------------------------------------------*
*& 1.1 - Create a new exclusion entry
*&---------------------------------------------------------------------*
FORM create_exclusion.
  DATA: lv_valid     TYPE abap_bool,
        lt_existing  TYPE STANDARD TABLE OF yrga_cst_exclude,
        ls_existing  TYPE yrga_cst_exclude,
        ls_new       TYPE yrga_cst_exclude,
        lv_timestamp TYPE char14.
* 1.1.1 / 9.1.1 - Validate location
  PERFORM validate_location USING p_loc CHANGING lv_valid.
  IF lv_valid = abap_false.
    RETURN.
  ENDIF.
* 1.1.2 / 9.1.2 to 9.1.4 - Validate material
  PERFORM validate_material USING p_mat p_vto CHANGING lv_valid.
  IF lv_valid = abap_false.
    RETURN.
  ENDIF.
* 1.1.3 - Fetch existing entries where DELETED is not X
  SELECT * FROM yrga_cst_exclude
    INTO TABLE lt_existing
    WHERE state_code = p_state
      AND location   = p_loc
      AND material   = p_mat
      AND deleted   <> 'X'.
* 1.1.4 - If entries exist, mark them as deleted first
  IF lt_existing IS NOT INITIAL.
    LOOP AT lt_existing INTO ls_existing.
      UPDATE yrga_cst_exclude
        SET deleted      = 'X'
            changed_by   = sy-uname
            changed_on   = sy-datum
            changed_time = sy-uzeit
        WHERE state_code  = ls_existing-state_code
          AND location    = ls_existing-location
          AND material    = ls_existing-material
          AND time_stamp  = ls_existing-time_stamp.
    ENDLOOP.
  ENDIF.
* Generate timestamp for new record
  CONCATENATE sy-datum sy-uzeit INTO lv_timestamp.
* Create new record
  CLEAR ls_new.
  ls_new-mandt        = sy-mandt.
  ls_new-state_code   = p_state.
  ls_new-location     = p_loc.
  ls_new-material     = p_mat.
  ls_new-time_stamp   = lv_timestamp.
  ls_new-valid_from   = p_vfrom.
  ls_new-valid_to     = p_vto.
  ls_new-created_by   = sy-uname.
  ls_new-created_on   = sy-datum.
  ls_new-created_time = sy-uzeit.
  ls_new-deleted      = space.
  INSERT yrga_cst_exclude FROM ls_new.
  IF sy-subrc = 0.
    COMMIT WORK.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Success'
        txt1  = 'Exclusion entry created successfully.'
        txt2  = space.
  ELSE.
    ROLLBACK WORK.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Error'
        txt1  = 'Error creating exclusion entry. Please try again.'
        txt2  = space.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form EDIT_EXCLUSION
*&---------------------------------------------------------------------*
*& 1.2 - Edit exclusion entries
*&---------------------------------------------------------------------*
FORM edit_exclusion.
  DATA: lv_valid   TYPE abap_bool,
        lt_db      TYPE STANDARD TABLE OF yrga_cst_exclude,
        ls_db      TYPE yrga_cst_exclude,
        ls_result  TYPE ty_exclude,
        lv_state   TYPE bezei20.
* 1.2.2 / 9.1.1 to 9.1.4 - Validate location and material if entered
  IF p_loc2 IS NOT INITIAL AND p_loc2 <> '*'.
    PERFORM validate_location USING p_loc2 CHANGING lv_valid.
    IF lv_valid = abap_false.
      RETURN.
    ENDIF.
  ENDIF.
  IF p_mat2 IS NOT INITIAL AND p_mat2 <> '*'.
    DATA: lv_dummy_date TYPE datum VALUE '99991231'.
    PERFORM validate_material USING p_mat2 lv_dummy_date CHANGING lv_valid.
    IF lv_valid = abap_false.
      RETURN.
    ENDIF.
  ENDIF.
* 1.2.3 - Fetch entries from YRGA_CST_EXCLUDE where DELETED <> X
  IF p_loc2 IS NOT INITIAL AND p_mat2 IS NOT INITIAL.
    SELECT * FROM yrga_cst_exclude
      INTO TABLE lt_db
      WHERE state_code = p_state2
        AND location   = p_loc2
        AND material   = p_mat2
        AND deleted   <> 'X'.
  ELSEIF p_loc2 IS NOT INITIAL.
    SELECT * FROM yrga_cst_exclude
      INTO TABLE lt_db
      WHERE state_code = p_state2
        AND location   = p_loc2
        AND deleted   <> 'X'.
  ELSEIF p_mat2 IS NOT INITIAL.
    SELECT * FROM yrga_cst_exclude
      INTO TABLE lt_db
      WHERE state_code = p_state2
        AND material   = p_mat2
        AND deleted   <> 'X'.
  ELSE.
    SELECT * FROM yrga_cst_exclude
      INTO TABLE lt_db
      WHERE state_code = p_state2
        AND deleted   <> 'X'.
  ENDIF.
  IF lt_db IS INITIAL.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Information'
        txt1  = 'No such mapping exists.'
        txt2  = space.
    RETURN.
  ENDIF.
* Get state name
  PERFORM get_state_name USING p_state2 CHANGING lv_state.
* Move to ALV display structure
  CLEAR gt_exclude.
  LOOP AT lt_db INTO ls_db.
    CLEAR ls_result.
    ls_result-state_code   = ls_db-state_code.
    ls_result-state        = lv_state.
    ls_result-location     = ls_db-location.
    ls_result-material     = ls_db-material.
    ls_result-valid_from   = ls_db-valid_from.
    ls_result-valid_to     = ls_db-valid_to.
    ls_result-created_by   = ls_db-created_by.
    ls_result-created_on   = ls_db-created_on.
    ls_result-created_time = ls_db-created_time.
    ls_result-changed_by   = ls_db-changed_by.
    ls_result-changed_on   = ls_db-changed_on.
    ls_result-changed_time = ls_db-changed_time.
    ls_result-deleted      = ls_db-deleted.
    APPEND ls_result TO gt_exclude.
  ENDLOOP.
* 1.2.3/1.2.4 - Display ALV with editable fields
  PERFORM build_fieldcat_edit.
  gs_layout-zebra             = 'X'.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-window_titlebar   = 'Edit Exclusion Entry'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = gv_repid
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      i_default                = 'X'
      i_save                   = 'A'
      i_callback_user_command  = 'USER_COMMAND_EDIT'
      i_callback_pf_status_set = 'SET_PF_STATUS_EDIT'
    TABLES
      t_outtab                 = gt_exclude
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form VIEW_EXCLUSION
*&---------------------------------------------------------------------*
*& 1.3 - View exclusion entries
*&---------------------------------------------------------------------*
FORM view_exclusion.
  DATA: lt_db      TYPE STANDARD TABLE OF yrga_cst_exclude,
        ls_db      TYPE yrga_cst_exclude,
        ls_result  TYPE ty_exclude,
        lt_result  TYPE STANDARD TABLE OF ty_exclude,
        lv_state   TYPE bezei20.
* Fetch entries from YRGA_CST_EXCLUDE
  IF p_loc2 IS NOT INITIAL AND p_mat2 IS NOT INITIAL.
    SELECT * FROM yrga_cst_exclude
      INTO TABLE lt_db
      WHERE state_code = p_state2
        AND location   = p_loc2
        AND material   = p_mat2.
  ELSEIF p_loc2 IS NOT INITIAL.
    SELECT * FROM yrga_cst_exclude
      INTO TABLE lt_db
      WHERE state_code = p_state2
        AND location   = p_loc2.
  ELSEIF p_mat2 IS NOT INITIAL.
    SELECT * FROM yrga_cst_exclude
      INTO TABLE lt_db
      WHERE state_code = p_state2
        AND material   = p_mat2.
  ELSE.
    SELECT * FROM yrga_cst_exclude
      INTO TABLE lt_db
      WHERE state_code = p_state2.
  ENDIF.
  IF lt_db IS INITIAL.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Information'
        txt1  = 'No exclusion entries found for the given criteria.'
        txt2  = space.
    RETURN.
  ENDIF.
* Get state name
  PERFORM get_state_name USING p_state2 CHANGING lv_state.
* Move to ALV display structure (exclude timestamp as per requirement)
  LOOP AT lt_db INTO ls_db.
    CLEAR ls_result.
    ls_result-state_code   = ls_db-state_code.
    ls_result-state        = lv_state.
    ls_result-location     = ls_db-location.
    ls_result-material     = ls_db-material.
    ls_result-valid_from   = ls_db-valid_from.
    ls_result-valid_to     = ls_db-valid_to.
    ls_result-created_by   = ls_db-created_by.
    ls_result-created_on   = ls_db-created_on.
    ls_result-created_time = ls_db-created_time.
    ls_result-changed_by   = ls_db-changed_by.
    ls_result-changed_on   = ls_db-changed_on.
    ls_result-changed_time = ls_db-changed_time.
    ls_result-deleted      = ls_db-deleted.
    APPEND ls_result TO lt_result.
  ENDLOOP.
* Display ALV (read-only)
  PERFORM build_fieldcat_view.
  gs_layout-zebra             = 'X'.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-window_titlebar   = 'Exclusion Entries - View'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = gv_repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat
      i_default          = 'X'
      i_save             = 'A'
    TABLES
      t_outtab           = lt_result
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_FIELDCAT_VIEW
*&---------------------------------------------------------------------*
*& Build field catalog for View ALV (all fields except timestamp)
*&---------------------------------------------------------------------*
FORM build_fieldcat_view.
  DATA: lv_col TYPE i.
  CLEAR gt_fieldcat.
  lv_col = 0.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'STATE_CODE'.
  gs_fieldcat-seltext_l = 'State Code'.
  gs_fieldcat-seltext_m = 'State Code'.
  gs_fieldcat-seltext_s = 'State'.
  gs_fieldcat-tabname   = 'LT_RESULT'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'STATE'.
  gs_fieldcat-seltext_l = 'State'.
  gs_fieldcat-seltext_m = 'State'.
  gs_fieldcat-seltext_s = 'State'.
  gs_fieldcat-tabname   = 'LT_RESULT'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'LOCATION'.
  gs_fieldcat-seltext_l = 'Supply Location'.
  gs_fieldcat-seltext_m = 'Location'.
  gs_fieldcat-seltext_s = 'Loc'.
  gs_fieldcat-tabname   = 'LT_RESULT'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'MATERIAL'.
  gs_fieldcat-seltext_l = 'Material'.
  gs_fieldcat-seltext_m = 'Material'.
  gs_fieldcat-seltext_s = 'Mat'.
  gs_fieldcat-tabname   = 'LT_RESULT'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'VALID_FROM'.
  gs_fieldcat-seltext_l = 'Valid From'.
  gs_fieldcat-seltext_m = 'Valid From'.
  gs_fieldcat-seltext_s = 'From'.
  gs_fieldcat-tabname   = 'LT_RESULT'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'VALID_TO'.
  gs_fieldcat-seltext_l = 'Valid To'.
  gs_fieldcat-seltext_m = 'Valid To'.
  gs_fieldcat-seltext_s = 'To'.
  gs_fieldcat-tabname   = 'LT_RESULT'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CREATED_BY'.
  gs_fieldcat-seltext_l = 'Created By'.
  gs_fieldcat-seltext_m = 'Created By'.
  gs_fieldcat-seltext_s = 'Cr. By'.
  gs_fieldcat-tabname   = 'LT_RESULT'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CREATED_ON'.
  gs_fieldcat-seltext_l = 'Created On'.
  gs_fieldcat-seltext_m = 'Created On'.
  gs_fieldcat-seltext_s = 'Cr. On'.
  gs_fieldcat-tabname   = 'LT_RESULT'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CREATED_TIME'.
  gs_fieldcat-seltext_l = 'Created At'.
  gs_fieldcat-seltext_m = 'Created At'.
  gs_fieldcat-seltext_s = 'Cr. At'.
  gs_fieldcat-tabname   = 'LT_RESULT'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CHANGED_BY'.
  gs_fieldcat-seltext_l = 'Changed By'.
  gs_fieldcat-seltext_m = 'Changed By'.
  gs_fieldcat-seltext_s = 'Ch. By'.
  gs_fieldcat-tabname   = 'LT_RESULT'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CHANGED_ON'.
  gs_fieldcat-seltext_l = 'Changed On'.
  gs_fieldcat-seltext_m = 'Changed On'.
  gs_fieldcat-seltext_s = 'Ch. On'.
  gs_fieldcat-tabname   = 'LT_RESULT'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CHANGED_TIME'.
  gs_fieldcat-seltext_l = 'Changed At'.
  gs_fieldcat-seltext_m = 'Changed At'.
  gs_fieldcat-seltext_s = 'Ch. At'.
  gs_fieldcat-tabname   = 'LT_RESULT'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'DELETED'.
  gs_fieldcat-seltext_l = 'Deletion Flag'.
  gs_fieldcat-seltext_m = 'Del Flag'.
  gs_fieldcat-seltext_s = 'Del'.
  gs_fieldcat-tabname   = 'LT_RESULT'.
  APPEND gs_fieldcat TO gt_fieldcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_FIELDCAT_EDIT
*&---------------------------------------------------------------------*
*& Build field catalog for Edit ALV with editable fields
*& 1.2.4 - Location, Material, Valid From, Valid To are editable
*&---------------------------------------------------------------------*
FORM build_fieldcat_edit.
  DATA: lv_col TYPE i.
  CLEAR gt_fieldcat.
  lv_col = 0.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'STATE_CODE'.
  gs_fieldcat-seltext_l = 'State Code'.
  gs_fieldcat-seltext_m = 'State Code'.
  gs_fieldcat-seltext_s = 'State'.
  gs_fieldcat-tabname   = 'GT_EXCLUDE'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'STATE'.
  gs_fieldcat-seltext_l = 'State'.
  gs_fieldcat-seltext_m = 'State'.
  gs_fieldcat-seltext_s = 'State'.
  gs_fieldcat-tabname   = 'GT_EXCLUDE'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'LOCATION'.
  gs_fieldcat-seltext_l = 'Supply Location'.
  gs_fieldcat-seltext_m = 'Location'.
  gs_fieldcat-seltext_s = 'Loc'.
  gs_fieldcat-edit      = 'X'.
  gs_fieldcat-tabname   = 'GT_EXCLUDE'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'MATERIAL'.
  gs_fieldcat-seltext_l = 'Material'.
  gs_fieldcat-seltext_m = 'Material'.
  gs_fieldcat-seltext_s = 'Mat'.
  gs_fieldcat-edit      = 'X'.
  gs_fieldcat-tabname   = 'GT_EXCLUDE'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'VALID_FROM'.
  gs_fieldcat-seltext_l = 'Valid From'.
  gs_fieldcat-seltext_m = 'Valid From'.
  gs_fieldcat-seltext_s = 'From'.
  gs_fieldcat-edit      = 'X'.
  gs_fieldcat-tabname   = 'GT_EXCLUDE'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'VALID_TO'.
  gs_fieldcat-seltext_l = 'Valid To'.
  gs_fieldcat-seltext_m = 'Valid To'.
  gs_fieldcat-seltext_s = 'To'.
  gs_fieldcat-edit      = 'X'.
  gs_fieldcat-tabname   = 'GT_EXCLUDE'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CREATED_BY'.
  gs_fieldcat-seltext_l = 'Created By'.
  gs_fieldcat-seltext_m = 'Created By'.
  gs_fieldcat-seltext_s = 'Cr. By'.
  gs_fieldcat-tabname   = 'GT_EXCLUDE'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CREATED_ON'.
  gs_fieldcat-seltext_l = 'Created On'.
  gs_fieldcat-seltext_m = 'Created On'.
  gs_fieldcat-seltext_s = 'Cr. On'.
  gs_fieldcat-tabname   = 'GT_EXCLUDE'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CREATED_TIME'.
  gs_fieldcat-seltext_l = 'Created At'.
  gs_fieldcat-seltext_m = 'Created At'.
  gs_fieldcat-seltext_s = 'Cr. At'.
  gs_fieldcat-tabname   = 'GT_EXCLUDE'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CHANGED_BY'.
  gs_fieldcat-seltext_l = 'Changed By'.
  gs_fieldcat-seltext_m = 'Changed By'.
  gs_fieldcat-seltext_s = 'Ch. By'.
  gs_fieldcat-tabname   = 'GT_EXCLUDE'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CHANGED_ON'.
  gs_fieldcat-seltext_l = 'Changed On'.
  gs_fieldcat-seltext_m = 'Changed On'.
  gs_fieldcat-seltext_s = 'Ch. On'.
  gs_fieldcat-tabname   = 'GT_EXCLUDE'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CHANGED_TIME'.
  gs_fieldcat-seltext_l = 'Changed At'.
  gs_fieldcat-seltext_m = 'Changed At'.
  gs_fieldcat-seltext_s = 'Ch. At'.
  gs_fieldcat-tabname   = 'GT_EXCLUDE'.
  APPEND gs_fieldcat TO gt_fieldcat.

  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'DELETED'.
  gs_fieldcat-seltext_l = 'Deletion Flag'.
  gs_fieldcat-seltext_m = 'Del Flag'.
  gs_fieldcat-seltext_s = 'Del'.
  gs_fieldcat-tabname   = 'GT_EXCLUDE'.
  APPEND gs_fieldcat TO gt_fieldcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PF_STATUS_EDIT
*&---------------------------------------------------------------------*
*& Set PF-STATUS with Update button for Edit ALV
*&---------------------------------------------------------------------*
FORM set_pf_status_edit USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'EDIT_STATUS' EXCLUDING rt_extab.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form USER_COMMAND_EDIT
*&---------------------------------------------------------------------*
*& 1.2.5 - Handle Update user command for Edit ALV
*&---------------------------------------------------------------------*
FORM user_command_edit USING r_ucomm     LIKE sy-ucomm
                             rs_selfield TYPE slis_selfield.
  DATA: lv_valid      TYPE abap_bool,
        lv_answer     TYPE char1,
        lv_timestamp  TYPE char14,
        lv_day        TYPE numc2,
        lv_last_day   TYPE datum,
        ls_new        TYPE yrga_cst_exclude,
        lv_msg        TYPE char200,
        lv_error      TYPE abap_bool.
  FIELD-SYMBOLS: <ls_excl> TYPE ty_exclude.
  CASE r_ucomm.
    WHEN 'UPDATE' OR '&DATA_SAVE'.
*     Confirmation popup
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirm Update'
          text_question         = 'Are you sure you want to update the exclusion entries?'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = lv_answer.
      IF lv_answer <> '1'.
        RETURN.
      ENDIF.
*     Validate all edited records before updating
      lv_error = abap_false.
      LOOP AT gt_exclude ASSIGNING <ls_excl>.
*       Mandatory checks
        IF <ls_excl>-location IS INITIAL.
          MESSAGE 'Supply Location is mandatory for all entries.' TYPE 'I'.
          lv_error = abap_true.
          EXIT.
        ENDIF.
        IF <ls_excl>-material IS INITIAL.
          MESSAGE 'Material is mandatory for all entries.' TYPE 'I'.
          lv_error = abap_true.
          EXIT.
        ENDIF.
        IF <ls_excl>-valid_from IS INITIAL.
          MESSAGE 'Valid From is mandatory for all entries.' TYPE 'I'.
          lv_error = abap_true.
          EXIT.
        ENDIF.
        IF <ls_excl>-valid_to IS INITIAL.
          MESSAGE 'Valid To is mandatory for all entries.' TYPE 'I'.
          lv_error = abap_true.
          EXIT.
        ENDIF.
*       Validate Valid From is start of fortnight
        IF <ls_excl>-valid_from+6(2) <> '01' AND <ls_excl>-valid_from+6(2) <> '16'.
          CONCATENATE 'Valid From must be 1st or 16th for entry with location'
            <ls_excl>-location '.' INTO lv_msg SEPARATED BY space.
          MESSAGE lv_msg TYPE 'I'.
          lv_error = abap_true.
          EXIT.
        ENDIF.
*       Validate Valid To is end of fortnight
        lv_day = <ls_excl>-valid_to+6(2).
        IF lv_day <> '15'.
          CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
            EXPORTING
              day_in            = <ls_excl>-valid_to
            IMPORTING
              last_day_of_month = lv_last_day
            EXCEPTIONS
              day_in_no_date    = 1
              OTHERS            = 2.
          IF sy-subrc <> 0 OR <ls_excl>-valid_to <> lv_last_day.
            CONCATENATE 'Valid To must be 15th or last day of month for entry with location'
              <ls_excl>-location '.' INTO lv_msg SEPARATED BY space.
            MESSAGE lv_msg TYPE 'I'.
            lv_error = abap_true.
            EXIT.
          ENDIF.
        ENDIF.
*       Validate location (skip wildcard)
        IF <ls_excl>-location <> '*'.
          PERFORM validate_location USING <ls_excl>-location CHANGING lv_valid.
          IF lv_valid = abap_false.
            lv_error = abap_true.
            EXIT.
          ENDIF.
        ENDIF.
*       Validate material (skip wildcard)
        IF <ls_excl>-material <> '*'.
          PERFORM validate_material USING <ls_excl>-material <ls_excl>-valid_to
            CHANGING lv_valid.
          IF lv_valid = abap_false.
            lv_error = abap_true.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF lv_error = abap_true.
        RETURN.
      ENDIF.
*     1.2.5 - Process each record: mark old as deleted, create new
      LOOP AT gt_exclude ASSIGNING <ls_excl>.
*       Mark existing record as deleted using original state_code, location, material
        UPDATE yrga_cst_exclude
          SET deleted      = 'X'
              changed_by   = sy-uname
              changed_on   = sy-datum
              changed_time = sy-uzeit
          WHERE state_code  = <ls_excl>-state_code
            AND location    = <ls_excl>-location
            AND material    = <ls_excl>-material
            AND deleted    <> 'X'.
*       Create new entry with user-entered data
        CONCATENATE sy-datum sy-uzeit INTO lv_timestamp.
        CLEAR ls_new.
        ls_new-mandt        = sy-mandt.
        ls_new-state_code   = <ls_excl>-state_code.
        ls_new-location     = <ls_excl>-location.
        ls_new-material     = <ls_excl>-material.
        ls_new-time_stamp   = lv_timestamp.
        ls_new-valid_from   = <ls_excl>-valid_from.
        ls_new-valid_to     = <ls_excl>-valid_to.
        ls_new-created_by   = sy-uname.
        ls_new-created_on   = sy-datum.
        ls_new-created_time = sy-uzeit.
        ls_new-deleted      = space.
        INSERT yrga_cst_exclude FROM ls_new.
      ENDLOOP.
      COMMIT WORK.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Success'
          txt1  = 'Exclusion entries updated successfully.'
          txt2  = space.
      rs_selfield-refresh = 'X'.
  ENDCASE.
ENDFORM.
