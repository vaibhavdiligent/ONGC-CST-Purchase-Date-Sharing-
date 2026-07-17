*&---------------------------------------------------------------------*
*& Program: ZDELETE_WB_OBJECTS
*& Purpose: Mass-delete Workbench / DDIC objects entered on the selection
*&          screen and record every deletion in a given transport request.
*&
*&          Supported object types:
*&            CLAS - Class
*&            DOMA - Domain
*&            DTEL - Data element
*&            FUGR - Function group
*&            INTF - Interface
*&            SHLP - Search help
*&            TABL - Table / Structure
*&            TTYP - Table type
*&
*&          Inputs (selection screen):
*&            * Object name(s) to delete (select-option, supports ranges
*&              and patterns; resolved against TADIR for the ticked types)
*&            * One checkbox per object type (process only the ticked ones)
*&            * Transport request number - the deletions are SAVED /
*&              recorded in this request (passed to each delete FM via
*&              its own CORRNUM / CORRNR parameter)
*&            * Test run flag (simulation, ON by default for safety)
*&
*&          Delete APIs used (verified to exist):
*&            RS_DD_DELETE_OBJ              DOMA/DTEL/TABL/TTYP/SHLP
*&            SEO_CLASS_DELETE_COMPLETE     CLAS
*&            SEO_INTERFACE_DELETE_COMPLETE INTF
*&            RS_FUNCTION_POOL_DELETE       FUGR
*&
*&          Written in classic ABAP syntax for ECC 6.0 compatibility.
*&---------------------------------------------------------------------*
REPORT zdelete_wb_objects.

TYPE-POOLS: slis.                                         " ALV grid types

TABLES: tadir.                                            " for SELECT-OPTIONS reference

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
  SELECT-OPTIONS: s_objnm  FOR  tadir-obj_name.           " Object name(s) to delete
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t02.
  PARAMETERS: p_clas AS CHECKBOX DEFAULT 'X',             " CLAS - Class
              p_doma AS CHECKBOX DEFAULT 'X',             " DOMA - Domain
              p_dtel AS CHECKBOX DEFAULT 'X',             " DTEL - Data element
              p_fugr AS CHECKBOX DEFAULT 'X',             " FUGR - Function group
              p_intf AS CHECKBOX DEFAULT 'X',             " INTF - Interface
              p_shlp AS CHECKBOX DEFAULT 'X',             " SHLP - Search help
              p_tabl AS CHECKBOX DEFAULT 'X',             " TABL - Table / Structure
              p_ttyp AS CHECKBOX DEFAULT 'X'.             " TTYP - Table type
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-t03.
  PARAMETERS: p_trkorr TYPE e070-trkorr OBLIGATORY,       " Request to save deletions in
              p_test   AS CHECKBOX DEFAULT 'X'.           " Test run (no deletion)
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* Data Declarations
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_obj,
         object   TYPE e071-object,
         obj_name TYPE e071-obj_name,
       END OF ty_obj.

TYPES: BEGIN OF ty_log,
         object   TYPE e071-object,
         obj_name TYPE e071-obj_name,
         status   TYPE c LENGTH 10,   " OK / SKIPPED / ERROR / TEST
         message  TYPE c LENGTH 255,  " char (not STRING) so ALV can display it
       END OF ty_log.

DATA: gt_obj    TYPE STANDARD TABLE OF ty_obj,
      gt_log    TYPE STANDARD TABLE OF ty_log,
      gs_obj    TYPE ty_obj,
      gs_log    TYPE ty_log,
      gr_type   TYPE RANGE OF e071-object,
      gs_type   LIKE LINE OF gr_type,
      gv_answer TYPE c,
      gv_ok     TYPE i,
      gv_err    TYPE i,
      gv_skip   TYPE i.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN - validate the transport request exists
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  DATA: lv_exists TYPE e070-trkorr.
  SELECT SINGLE trkorr FROM e070 INTO lv_exists
         WHERE trkorr = p_trkorr.
  IF sy-subrc <> 0.
    MESSAGE e398(00) WITH 'Transport request' p_trkorr 'does not exist'
                          space.
  ENDIF.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM build_type_range.

  IF gr_type IS INITIAL.
    MESSAGE 'Please select at least one object type' TYPE 'S'
            DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

* Object name is the primary input - never allow an unrestricted run,
* otherwise every object of the selected types would be deleted.
  IF s_objnm[] IS INITIAL.
    MESSAGE 'Please enter at least one object name' TYPE 'S'
            DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  PERFORM collect_objects.

  IF gt_obj IS INITIAL.
    MESSAGE 'No matching objects found for the selected names/types'
            TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF p_test IS INITIAL.
    PERFORM confirm_deletion CHANGING gv_answer.
    IF gv_answer <> '1'.
      MESSAGE 'Deletion cancelled by user' TYPE 'S'.
      RETURN.
    ENDIF.
  ENDIF.

  PERFORM process_objects.

END-OF-SELECTION.
  PERFORM display_log.

*&---------------------------------------------------------------------*
*& Form BUILD_TYPE_RANGE
*&   Builds a selection range from the ticked object-type checkboxes.
*&---------------------------------------------------------------------*
FORM build_type_range.
  CLEAR gr_type.
  gs_type-sign   = 'I'.
  gs_type-option = 'EQ'.

  IF p_clas = 'X'. gs_type-low = 'CLAS'. APPEND gs_type TO gr_type. ENDIF.
  IF p_doma = 'X'. gs_type-low = 'DOMA'. APPEND gs_type TO gr_type. ENDIF.
  IF p_dtel = 'X'. gs_type-low = 'DTEL'. APPEND gs_type TO gr_type. ENDIF.
  IF p_fugr = 'X'. gs_type-low = 'FUGR'. APPEND gs_type TO gr_type. ENDIF.
  IF p_intf = 'X'. gs_type-low = 'INTF'. APPEND gs_type TO gr_type. ENDIF.
  IF p_shlp = 'X'. gs_type-low = 'SHLP'. APPEND gs_type TO gr_type. ENDIF.
  IF p_tabl = 'X'. gs_type-low = 'TABL'. APPEND gs_type TO gr_type. ENDIF.
  IF p_ttyp = 'X'. gs_type-low = 'TTYP'. APPEND gs_type TO gr_type. ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form COLLECT_OBJECTS
*&   Resolves the object name selection against TADIR, keeping only the
*&   R3TR objects whose type is in the selected range and that actually
*&   exist in the system (not already flagged for deletion).
*&---------------------------------------------------------------------*
FORM collect_objects.

  SELECT object obj_name
         FROM tadir
         INTO CORRESPONDING FIELDS OF TABLE gt_obj
         WHERE pgmid    = 'R3TR'
           AND object   IN gr_type
           AND obj_name IN s_objnm
           AND delflag  = space.

  SORT gt_obj BY object obj_name.
  DELETE ADJACENT DUPLICATES FROM gt_obj COMPARING object obj_name.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CONFIRM_DELETION
*&   Popup safety confirmation before real deletion.
*&---------------------------------------------------------------------*
FORM confirm_deletion CHANGING cv_answer TYPE c.
  DATA: lv_lines TYPE i,
        lv_count TYPE n LENGTH 10,     " char-numeric so it can be concatenated
        lv_text  TYPE string.

  DESCRIBE TABLE gt_obj LINES lv_lines.
  lv_count = lv_lines.
  CONCATENATE 'You are about to DELETE' lv_count
              'object(s). This is irreversible. Continue?'
         INTO lv_text SEPARATED BY space.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirm object deletion'
      text_question         = lv_text
      text_button_1         = 'Delete'
      text_button_2         = 'Cancel'
      default_button        = '2'
      display_cancel_button = ' '
    IMPORTING
      answer                = cv_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    cv_answer = '2'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_OBJECTS
*&   Dispatches each object to the correct deletion routine.
*&---------------------------------------------------------------------*
FORM process_objects.

  LOOP AT gt_obj INTO gs_obj.

    CLEAR gs_log.
    gs_log-object   = gs_obj-object.
    gs_log-obj_name = gs_obj-obj_name.

    IF p_test = 'X'.
      gs_log-status  = 'TEST'.
      gs_log-message = 'Simulation only - not deleted'.
      APPEND gs_log TO gt_log.
      CONTINUE.
    ENDIF.

    CASE gs_obj-object.
      WHEN 'DOMA' OR 'DTEL' OR 'TABL' OR 'TTYP' OR 'SHLP'.
        PERFORM delete_ddic USING gs_obj CHANGING gs_log.
      WHEN 'CLAS'.
        PERFORM delete_class USING gs_obj 'CLAS' CHANGING gs_log.
      WHEN 'INTF'.
        PERFORM delete_class USING gs_obj 'INTF' CHANGING gs_log.
      WHEN 'FUGR'.
        PERFORM delete_fugr USING gs_obj CHANGING gs_log.
      WHEN OTHERS.
        gs_log-status  = 'SKIPPED'.
        gs_log-message = 'Object type not supported'.
    ENDCASE.

    APPEND gs_log TO gt_log.

  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DELETE_DDIC
*&   Deletes DDIC objects (DOMA, DTEL, TABL, TTYP, SHLP) via
*&   RS_DD_DELETE_OBJ. The object is recorded in the supplied request.
*&---------------------------------------------------------------------*
FORM delete_ddic USING us_obj TYPE ty_obj
                 CHANGING cs_log TYPE ty_log.

  DATA: lv_ddtype(4)  TYPE c,          " DDIC object type code (4 char)
        lv_name(30)   TYPE c,          " DDIC object name
        lv_corr       TYPE trkorr.

  lv_name = us_obj-obj_name.
  lv_corr = p_trkorr.

* DDIC type code (DDOBJTYPE) matches the transport object type 1:1
  CASE us_obj-object.
    WHEN 'DOMA'. lv_ddtype = 'DOMA'.
    WHEN 'DTEL'. lv_ddtype = 'DTEL'.
    WHEN 'TABL'. lv_ddtype = 'TABL'.
    WHEN 'TTYP'. lv_ddtype = 'TTYP'.
    WHEN 'SHLP'. lv_ddtype = 'SHLP'.
  ENDCASE.

* NO_ASK = 'X' suppresses the transport popup; the deletion is recorded
* in the request passed via the CHANGING parameter CORRNUM (= P_TRKORR).
  CALL FUNCTION 'RS_DD_DELETE_OBJ'
    EXPORTING
      objname = lv_name
      objtype = lv_ddtype
      no_ask  = 'X'
    CHANGING
      corrnum = lv_corr
    EXCEPTIONS
      OTHERS  = 1.

  PERFORM set_result USING sy-subrc CHANGING cs_log.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DELETE_CLASS
*&   Deletes classes (CLAS) and interfaces (INTF) via the SEO API.
*&---------------------------------------------------------------------*
FORM delete_class USING us_obj  TYPE ty_obj
                        uv_kind TYPE c
                  CHANGING cs_log TYPE ty_log.

  DATA: ls_clskey TYPE seoclskey,
        lv_corr   TYPE trkorr.

  ls_clskey-clsname = us_obj-obj_name.
  lv_corr           = p_trkorr.

  IF uv_kind = 'INTF'.
    CALL FUNCTION 'SEO_INTERFACE_DELETE_COMPLETE'
      EXPORTING
        intkey          = ls_clskey
        suppress_dialog = 'X'
      CHANGING
        corrnr          = lv_corr
      EXCEPTIONS
        OTHERS          = 1.
  ELSE.
    CALL FUNCTION 'SEO_CLASS_DELETE_COMPLETE'
      EXPORTING
        clskey          = ls_clskey
        suppress_dialog = 'X'
      CHANGING
        corrnr          = lv_corr
      EXCEPTIONS
        OTHERS          = 1.
  ENDIF.

  PERFORM set_result USING sy-subrc CHANGING cs_log.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DELETE_FUGR
*&   Deletes a function group (FUGR) via RS_FUNCTION_POOL_DELETE.
*&---------------------------------------------------------------------*
FORM delete_fugr USING us_obj TYPE ty_obj
                 CHANGING cs_log TYPE ty_log.

  DATA: lv_area TYPE rs38l-area.

  lv_area = us_obj-obj_name.

* The function group name is passed via AREA (not FUNCTION_POOL);
* CORRNUM records the deletion in the request, WITH_KORR forces the
* correction entry and SUPPRESS_POPUPS runs it without dialogs.
  CALL FUNCTION 'RS_FUNCTION_POOL_DELETE'
    EXPORTING
      area              = lv_area
      corrnum           = p_trkorr
      with_korr         = 'X'
      suppress_popups   = 'X'
      skip_progress_ind = 'X'
    EXCEPTIONS
      OTHERS            = 1.

  PERFORM set_result USING sy-subrc CHANGING cs_log.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_RESULT
*&   Translates SY-SUBRC / SY-MSG into a log line.
*&---------------------------------------------------------------------*
FORM set_result USING uv_subrc TYPE sy-subrc
                CHANGING cs_log TYPE ty_log.

  IF uv_subrc = 0.
    cs_log-status  = 'OK'.
    cs_log-message = 'Deleted'.
  ELSE.
    cs_log-status = 'ERROR'.
    IF sy-msgid IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO cs_log-message.
    ELSE.
      cs_log-message = 'Deletion failed (see SLG1 / SY-SUBRC)'.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_LOG
*&   Shows the result log as an ALV grid (REUSE_ALV_GRID_DISPLAY) with a
*&   header (request / mode / summary) via the TOP_OF_PAGE callback.
*&---------------------------------------------------------------------*
FORM display_log.

  DATA: lt_fcat  TYPE slis_t_fieldcat_alv,
        ls_layo  TYPE slis_layout_alv,
        lv_repid TYPE sy-repid.

  lv_repid = sy-repid.

* Summary counters (also shown in the header)
  CLEAR: gv_ok, gv_err, gv_skip.
  LOOP AT gt_log INTO gs_log.
    IF     gs_log-status = 'OK'.
      gv_ok = gv_ok + 1.
    ELSEIF gs_log-status = 'ERROR'.
      gv_err = gv_err + 1.
    ELSE.
      gv_skip = gv_skip + 1.
    ENDIF.
  ENDLOOP.

* Field catalog
  PERFORM add_fcat USING 'OBJECT'   'Type'        4  CHANGING lt_fcat.
  PERFORM add_fcat USING 'OBJ_NAME' 'Object name' 40 CHANGING lt_fcat.
  PERFORM add_fcat USING 'STATUS'   'Status'      10 CHANGING lt_fcat.
  PERFORM add_fcat USING 'MESSAGE'  'Message'     60 CHANGING lt_fcat.

  ls_layo-zebra             = 'X'.
  ls_layo-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = lv_repid
      i_callback_top_of_page = 'TOP_OF_PAGE'
      is_layout              = ls_layo
      it_fieldcat            = lt_fcat
      i_save                 = 'A'
    TABLES
      t_outtab               = gt_log
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
  IF sy-subrc <> 0.
    MESSAGE 'ALV display error' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form ADD_FCAT
*&   Appends one column definition to the ALV field catalog.
*&---------------------------------------------------------------------*
FORM add_fcat USING uv_field TYPE c
                    uv_text  TYPE c
                    uv_len   TYPE i
              CHANGING ct_fcat TYPE slis_t_fieldcat_alv.

  DATA: ls_fcat TYPE slis_fieldcat_alv.

  ls_fcat-fieldname    = uv_field.
  ls_fcat-seltext_l    = uv_text.
  ls_fcat-seltext_m    = uv_text.
  ls_fcat-seltext_s    = uv_text.
  ls_fcat-reptext_ddic = uv_text.
  ls_fcat-outputlen    = uv_len.
  APPEND ls_fcat TO ct_fcat.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form TOP_OF_PAGE  (ALV callback)
*&   Header block: request, run mode and result summary.
*&---------------------------------------------------------------------*
FORM top_of_page.

  DATA: lt_head TYPE slis_t_listheader,
        ls_head TYPE slis_listheader,
        lv_ok   TYPE c LENGTH 10,
        lv_err  TYPE c LENGTH 10,
        lv_skip TYPE c LENGTH 10,
        lv_sum  TYPE c LENGTH 60.

  ls_head-typ  = 'H'.
  ls_head-info = 'Workbench Object Deletion Log'.
  APPEND ls_head TO lt_head.

  CLEAR ls_head.
  ls_head-typ  = 'S'.
  ls_head-key  = 'Request:'.
  ls_head-info = p_trkorr.
  APPEND ls_head TO lt_head.

  CLEAR ls_head.
  ls_head-typ  = 'S'.
  ls_head-key  = 'Mode:'.
  IF p_test = 'X'.
    ls_head-info = 'TEST RUN - nothing was deleted'.
  ELSE.
    ls_head-info = 'PRODUCTIVE - objects deleted'.
  ENDIF.
  APPEND ls_head TO lt_head.

  WRITE gv_ok   TO lv_ok   LEFT-JUSTIFIED.
  WRITE gv_err  TO lv_err  LEFT-JUSTIFIED.
  WRITE gv_skip TO lv_skip LEFT-JUSTIFIED.
  CONCATENATE 'Deleted' lv_ok '/ Errors' lv_err '/ Other' lv_skip
         INTO lv_sum SEPARATED BY space.

  CLEAR ls_head.
  ls_head-typ  = 'S'.
  ls_head-key  = 'Summary:'.
  ls_head-info = lv_sum.
  APPEND ls_head TO lt_head.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_head.
ENDFORM.
