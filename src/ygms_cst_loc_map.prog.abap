*&---------------------------------------------------------------------*
*& Report YGMS_CST_LOC_MAP
*&---------------------------------------------------------------------*
*& Location Mapping Master Data - Create, View and Delete
*& Table: YRGA_CST_LOC_MAP
*&---------------------------------------------------------------------*
REPORT ygms_cst_loc_map.
*----------------------------------------------------------------------*
* TYPE-POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: slis, icon.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: yrga_cst_loc_map, oifspbl.
*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_loc_map,
         sel         TYPE char1,
         ongc_ctp_id TYPE yrga_cst_loc_map-ongc_ctp_id,
         gail_loc_id TYPE yrga_cst_loc_map-gail_loc_id,
         valid_from  TYPE yrga_cst_loc_map-valid_from,
         valid_to    TYPE yrga_cst_loc_map-valid_to,
         created_by  TYPE yrga_cst_loc_map-created_by,
         created_on  TYPE yrga_cst_loc_map-created_on,
         created_time TYPE yrga_cst_loc_map-created_time,
         deleted     TYPE yrga_cst_loc_map-deleted,
         changed_by  TYPE yrga_cst_loc_map-changed_by,
         changed_on  TYPE yrga_cst_loc_map-changed_on,
         changed_time TYPE yrga_cst_loc_map-changed_time,
       END OF ty_loc_map.
TYPES: BEGIN OF ty_error,
         msgty TYPE symsgty,
         msgtx TYPE char200,
       END OF ty_error.
*----------------------------------------------------------------------*
* DATA DECLARATIONS
*----------------------------------------------------------------------*
DATA: gt_loc_map   TYPE STANDARD TABLE OF ty_loc_map,
      gt_errors    TYPE STANDARD TABLE OF ty_error,
      gt_fieldcat  TYPE slis_t_fieldcat_alv,
      gt_events    TYPE slis_t_event.
DATA: gs_loc_map   TYPE ty_loc_map,
      gs_error     TYPE ty_error,
      gs_fieldcat  TYPE slis_fieldcat_alv,
      gs_layout    TYPE slis_layout_alv,
      gs_event     TYPE slis_alv_event.
DATA: gv_repid     TYPE sy-repid,
      gv_valid     TYPE abap_bool.
*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b00 WITH FRAME TITLE TEXT-b00.
  PARAMETERS: p_create RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND rb,
              p_view   RADIOBUTTON GROUP rb1,
              p_delete RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b00.
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  PARAMETERS: p_ctp_id TYPE yrga_cst_loc_map-ongc_ctp_id MODIF ID CRE,
              p_loc_id TYPE yrga_cst_loc_map-gail_loc_id MODIF ID CRE,
              p_vfrom  TYPE yrga_cst_loc_map-valid_from   MODIF ID CRE,
              p_vto    TYPE yrga_cst_loc_map-valid_to     MODIF ID CRE.
SELECTION-SCREEN END OF BLOCK b01.
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.
  PARAMETERS: p_ctp_v  TYPE yrga_cst_loc_map-ongc_ctp_id MODIF ID VDL,
              p_loc_v  TYPE yrga_cst_loc_map-gail_loc_id MODIF ID VDL.
SELECTION-SCREEN END OF BLOCK b02.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_create = 'X'.
*     Show Create block, hide View/Delete block
      IF screen-group1 = 'VDL'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
*     Make Valid To disabled (always 31.12.9999)
      IF screen-name = 'P_VTO'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
*     Show View/Delete block, hide Create block
      IF screen-group1 = 'CRE'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  gv_repid = sy-repid.
  p_vto    = '99991231'.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
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
*& Create a new location mapping record
*&---------------------------------------------------------------------*
FORM create_mapping.
  DATA: lv_pblnr      TYPE oifspbl-pblnr,
        lv_msg        TYPE char200,
        lv_valid_day  TYPE numc2,
        lt_existing   TYPE STANDARD TABLE OF yrga_cst_loc_map,
        ls_existing   TYPE yrga_cst_loc_map,
        ls_new        TYPE yrga_cst_loc_map,
        lv_validation TYPE char1,
        lv_s_from     TYPE datum,
        lv_s_to       TYPE datum,
        lv_prev_day   TYPE datum,
        lv_answer     TYPE char1.
* Mandatory field checks
  IF p_ctp_id IS INITIAL.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Error'
        txt1  = 'ONGC CTP ID is mandatory.'
        txt2  = space.
    RETURN.
  ENDIF.
  IF p_loc_id IS INITIAL.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Error'
        txt1  = 'GAIL Location ID is mandatory.'
        txt2  = space.
    RETURN.
  ENDIF.
  IF p_vfrom IS INITIAL.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Error'
        txt1  = 'Valid From date is mandatory.'
        txt2  = space.
    RETURN.
  ENDIF.
* Validate Valid From is start of a fortnight (1st or 16th)
  lv_valid_day = p_vfrom+6(2).
  IF lv_valid_day NE '01' AND lv_valid_day NE '16'.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Error'
        txt1  = 'Valid From can only be the start date of a fortnight (1st or 16th).'
        txt2  = space.
    RETURN.
  ENDIF.
  lv_s_from = p_vfrom.
  lv_s_to   = p_vto.
* 8.1.1 Validate GAIL Location ID against OIFSPBL
  CLEAR lv_pblnr.
  SELECT SINGLE pblnr FROM oifspbl
    INTO lv_pblnr
    WHERE pblnr = p_loc_id
      AND pbltyp = 'YDVN'.
  IF sy-subrc NE 0.
    CONCATENATE p_loc_id 'is not a valid purchase location.'
      INTO lv_msg SEPARATED BY space.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Error'
        txt1  = lv_msg
        txt2  = space.
    RETURN.
  ENDIF.
* 8.1.2 Check if mapping already exists
  DATA: lv_deleted    TYPE yrga_cst_loc_map-deleted,
        lv_reactivate TYPE char1.
  CLEAR lv_reactivate.
  SELECT SINGLE deleted FROM yrga_cst_loc_map
    INTO lv_deleted
    WHERE ongc_ctp_id = p_ctp_id
      AND gail_loc_id = p_loc_id.
  IF sy-subrc = 0.
    IF lv_deleted NE 'X'.
*     Active mapping exists - error
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Error'
          txt1  = 'This mapping already exists.'
          txt2  = space.
      RETURN.
    ELSE.
*     Deleted mapping exists - mark for reactivation (don't return yet)
      lv_reactivate = 'X'.
    ENDIF.
  ENDIF.
* 8.1.3 Fetch all active records for ONGC CTP ID (exclude the record being reactivated)
  SELECT * FROM yrga_cst_loc_map
    INTO TABLE lt_existing
    WHERE ongc_ctp_id = p_ctp_id
      AND gail_loc_id NE p_loc_id
      AND deleted NE 'X'.
* 8.1.4 If no other active records, create/reactivate directly
  IF lt_existing IS INITIAL.
    IF lv_reactivate = 'X'.
      PERFORM reactivate_mapping_record.
    ELSE.
      PERFORM insert_mapping_record.
    ENDIF.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Success'
        txt1  = 'Location mapping created successfully.'
        txt2  = space.
    RETURN.
  ENDIF.
* 8.1.5 Validate against existing records
  lv_validation = 'P'. "P = Passed
  LOOP AT lt_existing INTO ls_existing.
*   8.1.5.b: S_FROM inside existing range AND S_TO inside existing range
    IF ( lv_s_from > ls_existing-valid_from AND lv_s_from < ls_existing-valid_to )
       AND ( lv_s_to < ls_existing-valid_to ).
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Error'
          txt1  = 'The entered validity period falls between the validity'
          txt2  = 'period of an existing mapping. Please delete the existing mapping.'.
      lv_validation = 'F'. "F = Failed
      EXIT.
    ENDIF.
*   8.1.5.c: S_FROM before existing start AND S_TO overlaps existing start
    IF ( lv_s_from <= ls_existing-valid_from )
       AND ( lv_s_to >= ls_existing-valid_from AND lv_s_to < ls_existing-valid_to ).
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Error'
          txt1  = 'The entered validity period will shift the validity start date'
          txt2  = 'of an existing mapping forward. Please delete the existing mapping.'.
      lv_validation = 'F'.
      EXIT.
    ENDIF.
*   8.1.5.d: S_FROM before existing start AND S_TO after existing end
    IF ( lv_s_from <= ls_existing-valid_from )
       AND ( lv_s_to >= ls_existing-valid_to ).
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Error'
          txt1  = 'The entered validity completely covers the validity of an'
          txt2  = 'existing mapping. Please delete the existing mapping.'.
      lv_validation = 'F'.
      EXIT.
    ENDIF.
  ENDLOOP.
* 8.1.5.e: If validation failed, exit
  IF lv_validation = 'F'.
    RETURN.
  ENDIF.
* 8.1.6 Adjust overlapping existing records
  LOOP AT lt_existing INTO ls_existing.
*   8.1.6.b: No overlap - skip
    IF lv_s_from > ls_existing-valid_to OR lv_s_to < ls_existing-valid_from.
      CONTINUE.
    ENDIF.
*   8.1.6.c: S_FROM inside existing range (tail overlap) - truncate existing
    IF ( lv_s_from > ls_existing-valid_from AND lv_s_from <= ls_existing-valid_to )
       AND ( lv_s_to >= ls_existing-valid_to ).
      lv_prev_day = lv_s_from - 1.
      UPDATE yrga_cst_loc_map
        SET valid_to     = lv_prev_day
            changed_by   = sy-uname
            changed_on   = sy-datum
            changed_time = sy-uzeit
        WHERE ongc_ctp_id = ls_existing-ongc_ctp_id
          AND gail_loc_id = ls_existing-gail_loc_id.
    ENDIF.
  ENDLOOP.
* 8.1.7 Create or reactivate the mapping record
  IF lv_reactivate = 'X'.
    PERFORM reactivate_mapping_record.
  ELSE.
    PERFORM insert_mapping_record.
  ENDIF.
  CALL FUNCTION 'POPUP_TO_INFORM'
    EXPORTING
      titel = 'Success'
      txt1  = 'Location mapping created successfully.'
      txt2  = space.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form INSERT_MAPPING_RECORD
*&---------------------------------------------------------------------*
*& Insert a new record into YRGA_CST_LOC_MAP
*&---------------------------------------------------------------------*
FORM insert_mapping_record.
  DATA: ls_new TYPE yrga_cst_loc_map.
  ls_new-mandt        = sy-mandt.
  ls_new-ongc_ctp_id  = p_ctp_id.
  ls_new-gail_loc_id  = p_loc_id.
  ls_new-valid_from   = p_vfrom.
  ls_new-valid_to     = p_vto.
  ls_new-created_by   = sy-uname.
  ls_new-created_on   = sy-datum.
  ls_new-created_time = sy-uzeit.
  ls_new-deleted      = space.
  INSERT yrga_cst_loc_map FROM ls_new.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Error'
        txt1  = 'Error creating the mapping record. Please try again.'
        txt2  = space.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REACTIVATE_MAPPING_RECORD
*&---------------------------------------------------------------------*
*& Reactivate a previously deleted record in YRGA_CST_LOC_MAP
*&---------------------------------------------------------------------*
FORM reactivate_mapping_record.
  UPDATE yrga_cst_loc_map
    SET valid_from    = p_vfrom
        valid_to      = p_vto
        deleted       = space
        changed_by    = sy-uname
        changed_on    = sy-datum
        changed_time  = sy-uzeit
    WHERE ongc_ctp_id = p_ctp_id
      AND gail_loc_id = p_loc_id.
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Error'
        txt1  = 'Error creating the mapping record. Please try again.'
        txt2  = space.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form VIEW_MAPPING
*&---------------------------------------------------------------------*
*& View location mapping records
*&---------------------------------------------------------------------*
FORM view_mapping.
  DATA: lv_pblnr TYPE oifspbl-pblnr,
        lv_msg   TYPE char200.
* 8.2.1 Validate GAIL Location ID if provided
  IF p_loc_v IS NOT INITIAL.
    CLEAR lv_pblnr.
    SELECT SINGLE pblnr FROM oifspbl
      INTO lv_pblnr
      WHERE pblnr = p_loc_v
        AND pbltyp = 'YDVN'.
    IF sy-subrc NE 0.
      CONCATENATE p_loc_v 'is not a valid purchase location.'
        INTO lv_msg SEPARATED BY space.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Error'
          txt1  = lv_msg
          txt2  = space.
      RETURN.
    ENDIF.
  ENDIF.
* 8.2.2 / 8.2.3 Fetch mappings based on input
  CLEAR gt_loc_map.
  IF p_ctp_v IS NOT INITIAL AND p_loc_v IS NOT INITIAL.
    SELECT ongc_ctp_id gail_loc_id valid_from valid_to
           created_by created_on created_time
           deleted changed_by changed_on changed_time
      FROM yrga_cst_loc_map
      INTO CORRESPONDING FIELDS OF TABLE gt_loc_map
      WHERE ongc_ctp_id = p_ctp_v
        AND gail_loc_id = p_loc_v.
  ELSEIF p_ctp_v IS NOT INITIAL.
    SELECT ongc_ctp_id gail_loc_id valid_from valid_to
           created_by created_on created_time
           deleted changed_by changed_on changed_time
      FROM yrga_cst_loc_map
      INTO CORRESPONDING FIELDS OF TABLE gt_loc_map
      WHERE ongc_ctp_id = p_ctp_v.
  ELSEIF p_loc_v IS NOT INITIAL.
    SELECT ongc_ctp_id gail_loc_id valid_from valid_to
           created_by created_on created_time
           deleted changed_by changed_on changed_time
      FROM yrga_cst_loc_map
      INTO CORRESPONDING FIELDS OF TABLE gt_loc_map
      WHERE gail_loc_id = p_loc_v.
  ELSE.
*   8.2.3 No inputs - show all mappings
    SELECT ongc_ctp_id gail_loc_id valid_from valid_to
           created_by created_on created_time
           deleted changed_by changed_on changed_time
      FROM yrga_cst_loc_map
      INTO CORRESPONDING FIELDS OF TABLE gt_loc_map.
  ENDIF.
  IF gt_loc_map IS INITIAL.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Information'
        txt1  = 'No mappings found for the given criteria.'
        txt2  = space.
    RETURN.
  ENDIF.
  PERFORM build_fieldcat_view.
  PERFORM display_view_alv.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DELETE_MAPPING
*&---------------------------------------------------------------------*
*& Delete (soft) location mapping records
*&---------------------------------------------------------------------*
FORM delete_mapping.
  DATA: lv_pblnr TYPE oifspbl-pblnr,
        lv_msg   TYPE char200.
* 8.3.1 Validate GAIL Location ID if provided
  IF p_loc_v IS NOT INITIAL.
    CLEAR lv_pblnr.
    SELECT SINGLE pblnr FROM oifspbl
      INTO lv_pblnr
      WHERE pblnr = p_loc_v
        AND pbltyp = 'YDVN'.
    IF sy-subrc NE 0.
      CONCATENATE p_loc_v 'is not a valid purchase location.'
        INTO lv_msg SEPARATED BY space.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Error'
          txt1  = lv_msg
          txt2  = space.
      RETURN.
    ENDIF.
  ENDIF.
* 8.3.2 Fetch mappings where DELIND ≠ X
  CLEAR gt_loc_map.
  IF p_ctp_v IS NOT INITIAL AND p_loc_v IS NOT INITIAL.
    SELECT ongc_ctp_id gail_loc_id valid_from valid_to
           created_by created_on created_time
           deleted changed_by changed_on changed_time
      FROM yrga_cst_loc_map
      INTO CORRESPONDING FIELDS OF TABLE gt_loc_map
      WHERE ongc_ctp_id = p_ctp_v
        AND gail_loc_id = p_loc_v
        AND deleted NE 'X'.
  ELSEIF p_ctp_v IS NOT INITIAL.
    SELECT ongc_ctp_id gail_loc_id valid_from valid_to
           created_by created_on created_time
           deleted changed_by changed_on changed_time
      FROM yrga_cst_loc_map
      INTO CORRESPONDING FIELDS OF TABLE gt_loc_map
      WHERE ongc_ctp_id = p_ctp_v
        AND deleted NE 'X'.
  ELSEIF p_loc_v IS NOT INITIAL.
    SELECT ongc_ctp_id gail_loc_id valid_from valid_to
           created_by created_on created_time
           deleted changed_by changed_on changed_time
      FROM yrga_cst_loc_map
      INTO CORRESPONDING FIELDS OF TABLE gt_loc_map
      WHERE gail_loc_id = p_loc_v
        AND deleted NE 'X'.
  ELSE.
    SELECT ongc_ctp_id gail_loc_id valid_from valid_to
           created_by created_on created_time
           deleted changed_by changed_on changed_time
      FROM yrga_cst_loc_map
      INTO CORRESPONDING FIELDS OF TABLE gt_loc_map
      WHERE deleted NE 'X'.
  ENDIF.
  IF gt_loc_map IS INITIAL.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Information'
        txt1  = 'No active mappings found for the given criteria.'
        txt2  = space.
    RETURN.
  ENDIF.
  PERFORM build_fieldcat_delete.
  PERFORM display_delete_alv.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_FIELDCAT_VIEW
*&---------------------------------------------------------------------*
*& Build field catalog for View ALV
*&---------------------------------------------------------------------*
FORM build_fieldcat_view.
  DATA: lv_col TYPE i.
  CLEAR gt_fieldcat.
  lv_col = 0.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'ONGC_CTP_ID'.
  gs_fieldcat-seltext_l = 'ONGC CTP ID'.
  gs_fieldcat-seltext_m = 'ONGC CTP ID'.
  gs_fieldcat-seltext_s = 'CTP ID'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'GAIL_LOC_ID'.
  gs_fieldcat-seltext_l = 'GAIL Location ID'.
  gs_fieldcat-seltext_m = 'GAIL Loc ID'.
  gs_fieldcat-seltext_s = 'Loc ID'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'VALID_FROM'.
  gs_fieldcat-seltext_l = 'Valid From'.
  gs_fieldcat-seltext_m = 'Valid From'.
  gs_fieldcat-seltext_s = 'From'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'VALID_TO'.
  gs_fieldcat-seltext_l = 'Valid To'.
  gs_fieldcat-seltext_m = 'Valid To'.
  gs_fieldcat-seltext_s = 'To'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CREATED_BY'.
  gs_fieldcat-seltext_l = 'Created By'.
  gs_fieldcat-seltext_m = 'Created By'.
  gs_fieldcat-seltext_s = 'Cr. By'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CREATED_ON'.
  gs_fieldcat-seltext_l = 'Created On'.
  gs_fieldcat-seltext_m = 'Created On'.
  gs_fieldcat-seltext_s = 'Cr. On'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CREATED_TIME'.
  gs_fieldcat-seltext_l = 'Created At'.
  gs_fieldcat-seltext_m = 'Created At'.
  gs_fieldcat-seltext_s = 'Cr. At'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'DELETED'.
  gs_fieldcat-seltext_l = 'Deletion Flag'.
  gs_fieldcat-seltext_m = 'Del Flag'.
  gs_fieldcat-seltext_s = 'Del'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CHANGED_BY'.
  gs_fieldcat-seltext_l = 'Changed By'.
  gs_fieldcat-seltext_m = 'Changed By'.
  gs_fieldcat-seltext_s = 'Ch. By'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CHANGED_ON'.
  gs_fieldcat-seltext_l = 'Changed On'.
  gs_fieldcat-seltext_m = 'Changed On'.
  gs_fieldcat-seltext_s = 'Ch. On'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CHANGED_TIME'.
  gs_fieldcat-seltext_l = 'Changed At'.
  gs_fieldcat-seltext_m = 'Changed At'.
  gs_fieldcat-seltext_s = 'Ch. At'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_FIELDCAT_DELETE
*&---------------------------------------------------------------------*
*& Build field catalog for Delete ALV (with checkbox)
*&---------------------------------------------------------------------*
FORM build_fieldcat_delete.
  DATA: lv_col TYPE i.
  CLEAR gt_fieldcat.
  lv_col = 0.
* 8.3.3 Checkbox for selection
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'SEL'.
  gs_fieldcat-seltext_l = 'Select'.
  gs_fieldcat-seltext_m = 'Select'.
  gs_fieldcat-seltext_s = 'Sel'.
  gs_fieldcat-checkbox  = 'X'.
  gs_fieldcat-edit      = 'X'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'ONGC_CTP_ID'.
  gs_fieldcat-seltext_l = 'ONGC CTP ID'.
  gs_fieldcat-seltext_m = 'ONGC CTP ID'.
  gs_fieldcat-seltext_s = 'CTP ID'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'GAIL_LOC_ID'.
  gs_fieldcat-seltext_l = 'GAIL Location ID'.
  gs_fieldcat-seltext_m = 'GAIL Loc ID'.
  gs_fieldcat-seltext_s = 'Loc ID'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'VALID_FROM'.
  gs_fieldcat-seltext_l = 'Valid From'.
  gs_fieldcat-seltext_m = 'Valid From'.
  gs_fieldcat-seltext_s = 'From'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'VALID_TO'.
  gs_fieldcat-seltext_l = 'Valid To'.
  gs_fieldcat-seltext_m = 'Valid To'.
  gs_fieldcat-seltext_s = 'To'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CREATED_BY'.
  gs_fieldcat-seltext_l = 'Created By'.
  gs_fieldcat-seltext_m = 'Created By'.
  gs_fieldcat-seltext_s = 'Cr. By'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CREATED_ON'.
  gs_fieldcat-seltext_l = 'Created On'.
  gs_fieldcat-seltext_m = 'Created On'.
  gs_fieldcat-seltext_s = 'Cr. On'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CREATED_TIME'.
  gs_fieldcat-seltext_l = 'Created At'.
  gs_fieldcat-seltext_m = 'Created At'.
  gs_fieldcat-seltext_s = 'Cr. At'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CHANGED_BY'.
  gs_fieldcat-seltext_l = 'Changed By'.
  gs_fieldcat-seltext_m = 'Changed By'.
  gs_fieldcat-seltext_s = 'Ch. By'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CHANGED_ON'.
  gs_fieldcat-seltext_l = 'Changed On'.
  gs_fieldcat-seltext_m = 'Changed On'.
  gs_fieldcat-seltext_s = 'Ch. On'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
  lv_col = lv_col + 1.
  CLEAR gs_fieldcat.
  gs_fieldcat-col_pos   = lv_col.
  gs_fieldcat-fieldname = 'CHANGED_TIME'.
  gs_fieldcat-seltext_l = 'Changed At'.
  gs_fieldcat-seltext_m = 'Changed At'.
  gs_fieldcat-seltext_s = 'Ch. At'.
  gs_fieldcat-tabname   = 'GT_LOC_MAP'.
  APPEND gs_fieldcat TO gt_fieldcat.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_VIEW_ALV
*&---------------------------------------------------------------------*
*& Display ALV for View mode
*&---------------------------------------------------------------------*
FORM display_view_alv.
  gs_layout-zebra             = 'X'.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-window_titlebar   = 'Location Mapping - View'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = gv_repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat
      i_default          = 'X'
      i_save             = 'A'
    TABLES
      t_outtab           = gt_loc_map
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_DELETE_ALV
*&---------------------------------------------------------------------*
*& Display ALV for Delete mode with user command callback
*&---------------------------------------------------------------------*
FORM display_delete_alv.
  gs_layout-zebra             = 'X'.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-window_titlebar   = 'Location Mapping - Select records to delete'.
  gs_layout-box_fieldname     = 'SEL'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = gv_repid
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      i_default                = 'X'
      i_save                   = 'A'
      i_callback_user_command  = 'USER_COMMAND_DELETE'
      i_callback_pf_status_set = 'SET_PF_STATUS_DELETE'
    TABLES
      t_outtab                 = gt_loc_map
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PF_STATUS_DELETE
*&---------------------------------------------------------------------*
*& Set PF-STATUS with custom Delete button for delete ALV
*&---------------------------------------------------------------------*
FORM set_pf_status_delete USING rt_extab TYPE slis_t_extab.
* Custom PF-STATUS with Delete button
* Create in SE41: Program YGMS_CST_LOC_MAP, Status DELETE_STATUS
* Copy from SAPLKKBL status STANDARD_FULLSCREEN
* Add pushbutton in application toolbar: FCode = DELETE, Icon = ICON_DELETE, Text = Delete
  SET PF-STATUS 'DELETE_STATUS' EXCLUDING rt_extab.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form USER_COMMAND_DELETE
*&---------------------------------------------------------------------*
*& Handle user command for delete ALV
*&---------------------------------------------------------------------*
FORM user_command_delete USING r_ucomm     LIKE sy-ucomm
                               rs_selfield TYPE slis_selfield.
  DATA: lv_answer   TYPE char1,
        lv_count    TYPE i,
        lv_msg      TYPE char200.
  FIELD-SYMBOLS: <ls_map> TYPE ty_loc_map.
  CASE r_ucomm.
    WHEN 'DELETE' OR '&IC1' OR '&DATA_SAVE'.
*     Count selected records
      lv_count = 0.
      LOOP AT gt_loc_map ASSIGNING <ls_map> WHERE sel = 'X'.
        lv_count = lv_count + 1.
      ENDLOOP.
      IF lv_count = 0.
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = 'Information'
            txt1  = 'Please select at least one record to delete.'
            txt2  = space.
        RETURN.
      ENDIF.
*     8.3.4 Confirmation pop-up
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar       = 'Confirm Deletion'
          text_question  = 'Are you sure you want to delete the mapping?'
          text_button_1  = 'Yes'
          icon_button_1  = 'ICON_OKAY'
          text_button_2  = 'No'
          icon_button_2  = 'ICON_CANCEL'
          default_button = '2'
        IMPORTING
          answer         = lv_answer.
      IF lv_answer = '1'.
*       8.3.4 / 8.3.5 Set deletion flag and update changed fields
        LOOP AT gt_loc_map ASSIGNING <ls_map> WHERE sel = 'X'.
          UPDATE yrga_cst_loc_map
            SET deleted      = 'X'
                changed_by   = sy-uname
                changed_on   = sy-datum
                changed_time = sy-uzeit
            WHERE ongc_ctp_id = <ls_map>-ongc_ctp_id
              AND gail_loc_id = <ls_map>-gail_loc_id.
          IF sy-subrc = 0.
            <ls_map>-deleted      = 'X'.
            <ls_map>-changed_by   = sy-uname.
            <ls_map>-changed_on   = sy-datum.
            <ls_map>-changed_time = sy-uzeit.
            <ls_map>-sel          = space.
          ENDIF.
        ENDLOOP.
        COMMIT WORK.
        CALL FUNCTION 'POPUP_TO_INFORM'
          EXPORTING
            titel = 'Success'
            txt1  = 'Selected mappings have been deleted successfully.'
            txt2  = space.
        rs_selfield-refresh = 'X'.
      ENDIF.
  ENDCASE.
ENDFORM.
