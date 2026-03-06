*&---------------------------------------------------------------------*
*& Report YGMS_CST_MAT_MAP
*& Description: Material Mapping Master Data - Create, View, Delete
*&---------------------------------------------------------------------*
REPORT ygms_cst_mat_map.

TYPE-POOLS: slis, icon.

TABLES: yrga_cst_mat_map, oifspbl, mara, marc.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_mat_map,
         sel           TYPE c LENGTH 1,
         location_id   TYPE yrga_cst_mat_map-location_id,
         ongc_material TYPE yrga_cst_mat_map-ongc_material,
         gail_material TYPE yrga_cst_mat_map-gail_material,
         valid_from    TYPE yrga_cst_mat_map-valid_from,
         valid_to      TYPE yrga_cst_mat_map-valid_to,
         deleted       TYPE yrga_cst_mat_map-deleted,
         created_by    TYPE yrga_cst_mat_map-created_by,
         created_on    TYPE yrga_cst_mat_map-created_on,
         created_time  TYPE yrga_cst_mat_map-created_time,
         changed_by    TYPE yrga_cst_mat_map-changed_by,
         changed_on    TYPE yrga_cst_mat_map-changed_on,
         changed_time  TYPE yrga_cst_mat_map-changed_time,
       END OF ty_mat_map.

*----------------------------------------------------------------------*
* Data Declarations
*----------------------------------------------------------------------*
DATA: gt_mat_map   TYPE STANDARD TABLE OF ty_mat_map,
      gs_mat_map   TYPE ty_mat_map,
      gt_fieldcat  TYPE slis_t_fieldcat_alv,
      gs_fieldcat  TYPE slis_fieldcat_alv,
      gs_layout    TYPE slis_layout_alv,
      gv_repid     TYPE sy-repid,
      gv_valid     TYPE c LENGTH 1.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b00 WITH FRAME TITLE TEXT-b00.
  PARAMETERS: p_create RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND mode,
              p_view   RADIOBUTTON GROUP rb1,
              p_delete RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b00.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  PARAMETERS: p_locid  TYPE yrga_cst_mat_map-location_id,
              p_ongcmt TYPE yrga_cst_mat_map-ongc_material,
              p_gailmt TYPE yrga_cst_mat_map-gail_material,
              p_vfrom  TYPE datum,
              p_vto    TYPE datum DEFAULT '99991231'.
SELECTION-SCREEN END OF BLOCK b01.

*----------------------------------------------------------------------*
* At Selection Screen Output - Dynamic screen control
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_create = 'X'.
      " Create mode: all fields active, Valid To disabled
      IF screen-name = 'P_LOCID' OR screen-name = 'P_ONGCMT' OR
         screen-name = 'P_GAILMT' OR screen-name = 'P_VFROM'.
        screen-input = 1.
        screen-required = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'P_VTO'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF p_view = 'X' OR p_delete = 'X'.
      " View/Delete mode: only location, ongc material, gail material; none mandatory
      IF screen-name = 'P_LOCID' OR screen-name = 'P_ONGCMT' OR
         screen-name = 'P_GAILMT'.
        screen-input = 1.
        screen-required = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'P_VFROM' OR screen-name = 'P_VTO'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

*----------------------------------------------------------------------*
* At Selection Screen - Input validations
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  " Skip mandatory validation when user is switching radio buttons
  CHECK sy-ucomm <> 'MODE'.

  IF p_create = 'X'.
    " Mandatory checks for Create mode
    IF p_locid IS INITIAL.
      MESSAGE 'GAIL Location ID is mandatory.' TYPE 'E'.
    ENDIF.
    IF p_ongcmt IS INITIAL.
      MESSAGE 'ONGC Material Name is mandatory.' TYPE 'E'.
    ENDIF.
    IF p_gailmt IS INITIAL.
      MESSAGE 'GAIL Material Name is mandatory.' TYPE 'E'.
    ENDIF.
    IF p_vfrom IS INITIAL.
      MESSAGE 'Valid From date is mandatory.' TYPE 'E'.
    ENDIF.
    " Validate Valid From is start of a fortnight (1st or 16th)
    IF p_vfrom IS NOT INITIAL AND
       p_vfrom+6(2) <> '01' AND p_vfrom+6(2) <> '16'.
      MESSAGE 'Valid From can only be the start date of a fortnight (1st or 16th).' TYPE 'E'.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  gv_repid = sy-repid.

  IF p_create = 'X'.
    PERFORM create_mapping.
  ELSEIF p_view = 'X'.
    PERFORM view_mapping.
  ELSEIF p_delete = 'X'.
    PERFORM delete_mapping.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form CREATE_MAPPING
*&---------------------------------------------------------------------*
FORM create_mapping.

  DATA: lv_pblnr   TYPE oifspbl-pblnr,
        lv_matnr   TYPE mara-matnr,
        lv_mstde   TYPE mara-mstde,
        lv_uomgr   TYPE marc-uomgr,
        ls_existing TYPE yrga_cst_mat_map,
        lv_s_from   TYPE datum,
        lv_s_to     TYPE datum,
        lv_msg      TYPE string,
        lv_answer   TYPE c LENGTH 1.

  lv_s_from = p_vfrom.
  lv_s_to   = p_vto.

  " 9.1.1 - Validate GAIL Location ID against OIFSPBL
  SELECT SINGLE pblnr FROM oifspbl
    INTO lv_pblnr
    WHERE pblnr = p_locid
      AND pbltyp = 'YDVN'.
  IF sy-subrc <> 0.
    CONCATENATE p_locid ' is not a valid purchase location.' INTO lv_msg.
    MESSAGE lv_msg TYPE 'I'.
    RETURN.
  ENDIF.

  " 9.1.2 - Validate GAIL Material Name begins with GMS
  IF p_gailmt(3) <> 'GMS'.
    MESSAGE 'Please enter a valid GMS material.' TYPE 'I'.
    RETURN.
  ENDIF.

  " 9.1.3 - Check material block status in MARA
  SELECT SINGLE matnr mstde FROM mara
    INTO (lv_matnr, lv_mstde)
    WHERE matnr = p_gailmt
      AND mstae = 'Z1'.
  IF sy-subrc = 0.
    " Material found with block status Z1
    IF lv_s_to >= lv_mstde.
      CONCATENATE 'Material ' p_gailmt ' has been blocked with effect from ' lv_mstde '.' INTO lv_msg.
      MESSAGE lv_msg TYPE 'I'.
      RETURN.
    ENDIF.
  ENDIF.

  " 9.1.4 - Check UoM group in MARC
  SELECT SINGLE uomgr FROM marc
    INTO lv_uomgr
    WHERE matnr = p_gailmt
      AND uomgr = '6'.
  IF sy-subrc <> 0.
    CONCATENATE p_gailmt ' does not belong to 2 UoM group.' INTO lv_msg.
    MESSAGE lv_msg TYPE 'I'.
    RETURN.
  ENDIF.

  " 9.1.5 - Check if active mapping already exists for same primary keys (Location ID + ONGC Material)
  SELECT SINGLE * FROM yrga_cst_mat_map
    INTO ls_existing
    WHERE location_id   = p_locid
      AND ongc_material = p_ongcmt
      AND deleted       <> 'X'.
  IF sy-subrc = 0.
    " Existing active mapping found - confirm overwrite
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Mapping Exists'
        text_question         = 'An active mapping already exists for this Location ID and ONGC Material. Do you want to overwrite it?'
        text_button_1         = 'Yes'
        text_button_2         = 'No'
        default_button        = '2'
        display_cancel_button = ' '
      IMPORTING
        answer                = lv_answer.

    IF lv_answer <> '1'.
      RETURN.
    ENDIF.
  ENDIF.

  " 9.1.6 - Create / Overwrite the mapping (MODIFY upserts on primary key)
  PERFORM insert_new_mapping.
  MESSAGE 'Material mapping created successfully.' TYPE 'S'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form INSERT_NEW_MAPPING
*&---------------------------------------------------------------------*
FORM insert_new_mapping.

  DATA: ls_new TYPE yrga_cst_mat_map.

  ls_new-mandt         = sy-mandt.
  ls_new-location_id   = p_locid.
  ls_new-gail_material = p_gailmt.
  ls_new-valid_from    = p_vfrom.
  ls_new-valid_to      = p_vto.
  ls_new-ongc_material = p_ongcmt.
  ls_new-created_by    = sy-uname.
  ls_new-created_on    = sy-datum.
  ls_new-created_time  = sy-uzeit.
  ls_new-deleted       = ' '.

  MODIFY yrga_cst_mat_map FROM ls_new.
  IF sy-subrc <> 0.
    MESSAGE 'Error creating material mapping.' TYPE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form VIEW_MAPPING
*&---------------------------------------------------------------------*
FORM view_mapping.

  DATA: lt_result  TYPE STANDARD TABLE OF ty_mat_map,
        ls_result  TYPE ty_mat_map,
        lt_db      TYPE STANDARD TABLE OF yrga_cst_mat_map,
        ls_db      TYPE yrga_cst_mat_map,
        lv_pblnr   TYPE oifspbl-pblnr,
        lv_msg     TYPE string.

  " 9.2.1 - Validate Location ID if entered
  IF p_locid IS NOT INITIAL.
    SELECT SINGLE pblnr FROM oifspbl
      INTO lv_pblnr
      WHERE pblnr = p_locid
        AND pbltyp = 'YDVN'.
    IF sy-subrc <> 0.
      CONCATENATE p_locid ' is not a valid purchase location.' INTO lv_msg.
      MESSAGE lv_msg TYPE 'I'.
      RETURN.
    ENDIF.
  ENDIF.

  " 9.2.2 - Validate GAIL Material Name if entered
  IF p_gailmt IS NOT INITIAL.
    IF p_gailmt(3) <> 'GMS'.
      MESSAGE 'Please enter a valid GMS material.' TYPE 'I'.
      RETURN.
    ENDIF.
  ENDIF.

  " 9.2.3 / 9.2.4 - Fetch mappings based on inputs
  IF p_locid IS INITIAL AND p_ongcmt IS INITIAL AND p_gailmt IS INITIAL.
    SELECT * FROM yrga_cst_mat_map INTO TABLE lt_db.
  ELSE.
    SELECT * FROM yrga_cst_mat_map
      INTO TABLE lt_db
      WHERE ( location_id   = p_locid   OR p_locid   IS INITIAL )
        AND ( ongc_material = p_ongcmt  OR p_ongcmt  IS INITIAL )
        AND ( gail_material = p_gailmt  OR p_gailmt  IS INITIAL ).
  ENDIF.

  IF lt_db IS INITIAL.
    MESSAGE 'No material mappings found.' TYPE 'I'.
    RETURN.
  ENDIF.

  " Move to ALV display structure
  LOOP AT lt_db INTO ls_db.
    CLEAR ls_result.
    ls_result-location_id   = ls_db-location_id.
    ls_result-ongc_material = ls_db-ongc_material.
    ls_result-gail_material = ls_db-gail_material.
    ls_result-valid_from    = ls_db-valid_from.
    ls_result-valid_to      = ls_db-valid_to.
    ls_result-deleted       = ls_db-deleted.
    ls_result-created_by    = ls_db-created_by.
    ls_result-created_on    = ls_db-created_on.
    ls_result-created_time  = ls_db-created_time.
    ls_result-changed_by    = ls_db-changed_by.
    ls_result-changed_on    = ls_db-changed_on.
    ls_result-changed_time  = ls_db-changed_time.
    APPEND ls_result TO lt_result.
  ENDLOOP.

  " Display ALV
  PERFORM build_fieldcatalog_view.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra             = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = gv_repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat
      i_save             = 'A'
    TABLES
      t_outtab           = lt_result
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Error displaying ALV.' TYPE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DELETE_MAPPING
*&---------------------------------------------------------------------*
FORM delete_mapping.

  DATA: lt_result  TYPE STANDARD TABLE OF ty_mat_map,
        ls_result  TYPE ty_mat_map,
        lt_db      TYPE STANDARD TABLE OF yrga_cst_mat_map,
        ls_db      TYPE yrga_cst_mat_map,
        lv_pblnr   TYPE oifspbl-pblnr,
        lv_msg     TYPE string.

  " 9.3.1 - Validate Location ID if entered
  IF p_locid IS NOT INITIAL.
    SELECT SINGLE pblnr FROM oifspbl
      INTO lv_pblnr
      WHERE pblnr = p_locid
        AND pbltyp = 'YDVN'.
    IF sy-subrc <> 0.
      CONCATENATE p_locid ' is not a valid purchase location.' INTO lv_msg.
      MESSAGE lv_msg TYPE 'I'.
      RETURN.
    ENDIF.
  ENDIF.

  " 9.3.2 - Validate GAIL Material Name if entered
  IF p_gailmt IS NOT INITIAL.
    IF p_gailmt(3) <> 'GMS'.
      MESSAGE 'Please enter a valid GMS material.' TYPE 'I'.
      RETURN.
    ENDIF.
  ENDIF.

  " 9.3.3 - Fetch mappings where DELETED <> X
  IF p_locid IS INITIAL AND p_ongcmt IS INITIAL AND p_gailmt IS INITIAL.
    SELECT * FROM yrga_cst_mat_map
      INTO TABLE lt_db
      WHERE deleted <> 'X'.
  ELSE.
    SELECT * FROM yrga_cst_mat_map
      INTO TABLE lt_db
      WHERE ( location_id   = p_locid   OR p_locid   IS INITIAL )
        AND ( ongc_material = p_ongcmt  OR p_ongcmt  IS INITIAL )
        AND ( gail_material = p_gailmt  OR p_gailmt  IS INITIAL )
        AND deleted <> 'X'.
  ENDIF.

  IF lt_db IS INITIAL.
    MESSAGE 'No active material mappings found.' TYPE 'I'.
    RETURN.
  ENDIF.

  " Move to ALV display structure with checkbox
  LOOP AT lt_db INTO ls_db.
    CLEAR ls_result.
    ls_result-sel           = ' '.
    ls_result-location_id   = ls_db-location_id.
    ls_result-ongc_material = ls_db-ongc_material.
    ls_result-gail_material = ls_db-gail_material.
    ls_result-valid_from    = ls_db-valid_from.
    ls_result-valid_to      = ls_db-valid_to.
    ls_result-deleted       = ls_db-deleted.
    ls_result-created_by    = ls_db-created_by.
    ls_result-created_on    = ls_db-created_on.
    ls_result-created_time  = ls_db-created_time.
    ls_result-changed_by    = ls_db-changed_by.
    ls_result-changed_on    = ls_db-changed_on.
    ls_result-changed_time  = ls_db-changed_time.
    APPEND ls_result TO lt_result.
  ENDLOOP.

  gt_mat_map = lt_result.

  " Display ALV with checkbox and delete button
  PERFORM build_fieldcatalog_delete.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra             = 'X'.
  gs_layout-box_fieldname     = 'SEL'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = gv_repid
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      i_callback_user_command  = 'USER_COMMAND_DELETE'
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_mat_map
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Error displaying ALV.' TYPE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZDELETE'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form USER_COMMAND_DELETE
*&---------------------------------------------------------------------*
FORM user_command_delete USING r_ucomm     LIKE sy-ucomm
                               rs_selfield TYPE slis_selfield.

  DATA: ls_map    TYPE ty_mat_map,
        ls_db     TYPE yrga_cst_mat_map,
        lv_answer TYPE c LENGTH 1,
        lv_count  TYPE i.

  CASE r_ucomm.
    WHEN 'DELETE' OR '&IC1'.

      " Count selected records
      lv_count = 0.
      LOOP AT gt_mat_map INTO ls_map WHERE sel = 'X'.
        lv_count = lv_count + 1.
      ENDLOOP.

      IF lv_count = 0.
        MESSAGE 'Please select at least one record to delete.' TYPE 'I'.
        RETURN.
      ENDIF.

      " 9.3.5 - Confirmation popup
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirm Deletion'
          text_question         = 'Are you sure you want to delete the mapping?'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = lv_answer.

      IF lv_answer = '1'.
        " Set deletion flag on selected records
        LOOP AT gt_mat_map INTO ls_map WHERE sel = 'X'.
          " 9.3.5 / 9.3.6 - Update deletion flag and changed fields
          UPDATE yrga_cst_mat_map
            SET deleted      = 'X'
                changed_by   = sy-uname
                changed_on   = sy-datum
                changed_time = sy-uzeit
            WHERE location_id   = ls_map-location_id
              AND ongc_material = ls_map-ongc_material.
        ENDLOOP.

        IF sy-subrc = 0.
          MESSAGE 'Selected mappings have been deleted successfully.' TYPE 'S'.
        ENDIF.

        " Refresh the display
        rs_selfield-refresh = 'X'.
      ENDIF.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_FIELDCATALOG_VIEW
*&---------------------------------------------------------------------*
FORM build_fieldcatalog_view.

  CLEAR gt_fieldcat.

  PERFORM add_field USING 'LOCATION_ID'   'YRGA_CST_MAT_MAP' 'LOCATION_ID'   'Location ID'.
  PERFORM add_field USING 'ONGC_MATERIAL' 'YRGA_CST_MAT_MAP' 'ONGC_MATERIAL' 'ONGC Material'.
  PERFORM add_field USING 'GAIL_MATERIAL' 'YRGA_CST_MAT_MAP' 'GAIL_MATERIAL' 'GAIL Material'.
  PERFORM add_field USING 'VALID_FROM'    'YRGA_CST_MAT_MAP' 'VALID_FROM'    'Valid From'.
  PERFORM add_field USING 'VALID_TO'      'YRGA_CST_MAT_MAP' 'VALID_TO'      'Valid To'.
  PERFORM add_field USING 'DELETED'       'YRGA_CST_MAT_MAP' 'DELETED'       'Deleted'.
  PERFORM add_field USING 'CREATED_BY'    'YRGA_CST_MAT_MAP' 'CREATED_BY'    'Created By'.
  PERFORM add_field USING 'CREATED_ON'    'YRGA_CST_MAT_MAP' 'CREATED_ON'    'Created On'.
  PERFORM add_field USING 'CREATED_TIME'  'YRGA_CST_MAT_MAP' 'CREATED_TIME'  'Created Time'.
  PERFORM add_field USING 'CHANGED_BY'    'YRGA_CST_MAT_MAP' 'CHANGED_BY'    'Changed By'.
  PERFORM add_field USING 'CHANGED_ON'    'YRGA_CST_MAT_MAP' 'CHANGED_ON'    'Changed On'.
  PERFORM add_field USING 'CHANGED_TIME'  'YRGA_CST_MAT_MAP' 'CHANGED_TIME'  'Changed Time'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_FIELDCATALOG_DELETE
*&---------------------------------------------------------------------*
FORM build_fieldcatalog_delete.

  CLEAR gt_fieldcat.

  PERFORM add_field USING 'LOCATION_ID'   'YRGA_CST_MAT_MAP' 'LOCATION_ID'   'Location ID'.
  PERFORM add_field USING 'ONGC_MATERIAL' 'YRGA_CST_MAT_MAP' 'ONGC_MATERIAL' 'ONGC Material'.
  PERFORM add_field USING 'GAIL_MATERIAL' 'YRGA_CST_MAT_MAP' 'GAIL_MATERIAL' 'GAIL Material'.
  PERFORM add_field USING 'VALID_FROM'    'YRGA_CST_MAT_MAP' 'VALID_FROM'    'Valid From'.
  PERFORM add_field USING 'VALID_TO'      'YRGA_CST_MAT_MAP' 'VALID_TO'      'Valid To'.
  PERFORM add_field USING 'CREATED_BY'    'YRGA_CST_MAT_MAP' 'CREATED_BY'    'Created By'.
  PERFORM add_field USING 'CREATED_ON'    'YRGA_CST_MAT_MAP' 'CREATED_ON'    'Created On'.
  PERFORM add_field USING 'CREATED_TIME'  'YRGA_CST_MAT_MAP' 'CREATED_TIME'  'Created Time'.
  PERFORM add_field USING 'CHANGED_BY'    'YRGA_CST_MAT_MAP' 'CHANGED_BY'    'Changed By'.
  PERFORM add_field USING 'CHANGED_ON'    'YRGA_CST_MAT_MAP' 'CHANGED_ON'    'Changed On'.
  PERFORM add_field USING 'CHANGED_TIME'  'YRGA_CST_MAT_MAP' 'CHANGED_TIME'  'Changed Time'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ADD_FIELD
*&---------------------------------------------------------------------*
FORM add_field USING pv_fieldname TYPE slis_fieldcat_alv-fieldname
                     pv_ref_tab   TYPE slis_fieldcat_alv-ref_tabname
                     pv_ref_field TYPE slis_fieldcat_alv-ref_fieldname
                     pv_seltext   TYPE slis_fieldcat_alv-seltext_m.

  CLEAR gs_fieldcat.
  gs_fieldcat-fieldname     = pv_fieldname.
  gs_fieldcat-ref_tabname   = pv_ref_tab.
  gs_fieldcat-ref_fieldname = pv_ref_field.
  gs_fieldcat-seltext_m     = pv_seltext.
  APPEND gs_fieldcat TO gt_fieldcat.

ENDFORM.
