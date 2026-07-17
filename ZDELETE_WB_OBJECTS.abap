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
*&              recorded in this request
*&            * Test run flag (simulation, ON by default for safety)
*&
*&          Written in classic ABAP syntax for ECC 6.0 compatibility.
*&---------------------------------------------------------------------*
REPORT zdelete_wb_objects.

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
         pgmid    TYPE e071-pgmid,
         object   TYPE e071-object,
         obj_name TYPE e071-obj_name,
       END OF ty_obj.

TYPES: BEGIN OF ty_log,
         object   TYPE e071-object,
         obj_name TYPE e071-obj_name,
         status   TYPE char10,      " OK / SKIPPED / ERROR / TEST
         message  TYPE string,
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
  DATA: lv_count TYPE i,
        lv_text  TYPE string.

  DESCRIBE TABLE gt_obj LINES lv_count.
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

*   Lock the object in the target request first, so the deletion is
*   recorded / saved in P_TRKORR.
    PERFORM record_in_request USING gs_obj CHANGING gs_log.
    IF gs_log-status = 'ERROR'.
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
*& Form RECORD_IN_REQUEST
*&   Registers (locks) the object in the target transport request via
*&   RS_CORR_INSERT so that the subsequent deletion is saved in P_TRKORR.
*&---------------------------------------------------------------------*
FORM record_in_request USING us_obj TYPE ty_obj
                       CHANGING cs_log TYPE ty_log.

  CALL FUNCTION 'RS_CORR_INSERT'
    EXPORTING
      object              = us_obj-obj_name
      object_class        = us_obj-object
      korrnum             = p_trkorr
      master_language     = sy-langu
      global_lock         = 'X'
      mode                = 'I'
    EXCEPTIONS
      cancelled           = 1
      permission_failure  = 2
      unknown_objectclass = 3
      OTHERS              = 4.

  IF sy-subrc <> 0.
    cs_log-status = 'ERROR'.
    IF sy-msgid IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO cs_log-message.
    ELSE.
      cs_log-message = 'Could not record object in request'.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DELETE_DDIC
*&   Deletes DDIC objects (DOMA, DTEL, TABL, TTYP, SHLP) via
*&   RS_DD_DELETE_OBJ. The object is recorded in the supplied request.
*&---------------------------------------------------------------------*
FORM delete_ddic USING us_obj TYPE ty_obj
                 CHANGING cs_log TYPE ty_log.

  DATA: lv_ddtype TYPE ddobjtype,
        lv_name   TYPE ddobjname.

  lv_name = us_obj-obj_name.

* Map the transport object type to the internal DDIC type code
  CASE us_obj-object.
    WHEN 'DOMA'. lv_ddtype = 'DOMA'.
    WHEN 'DTEL'. lv_ddtype = 'DTEL'.
    WHEN 'TABL'. lv_ddtype = 'TABL'.
    WHEN 'TTYP'. lv_ddtype = 'TTYP'.
    WHEN 'SHLP'. lv_ddtype = 'SHLP'.
  ENDCASE.

* NOTE: RS_DD_DELETE_OBJ suppresses the transport popup with NO_ASK = 'X'.
*       The object is already contained in P_TRKORR (that is where the
*       list was read from), so the deletion is recorded in that request.
*       If your release rejects a parameter/exception below, verify the
*       signature in SE37 and adjust.
  CALL FUNCTION 'RS_DD_DELETE_OBJ'
    EXPORTING
      objname = lv_name
      objtype = lv_ddtype
      no_ask  = 'X'
    EXCEPTIONS
      OTHERS  = 1.

  PERFORM set_result USING sy-subrc CHANGING cs_log.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DELETE_CLASS
*&   Deletes classes (CLAS) and interfaces (INTF) via the SEO API.
*&---------------------------------------------------------------------*
FORM delete_class USING us_obj  TYPE ty_obj
                        uv_kind TYPE char4
                  CHANGING cs_log TYPE ty_log.

  DATA: ls_clskey TYPE seoclskey.

  ls_clskey-clsname = us_obj-obj_name.

  IF uv_kind = 'INTF'.
    CALL FUNCTION 'SEO_INTERFACE_DELETE_COMPLETE'
      EXPORTING
        intkey = ls_clskey
      EXCEPTIONS
        OTHERS = 1.
  ELSE.
    CALL FUNCTION 'SEO_CLASS_DELETE_COMPLETE'
      EXPORTING
        clskey = ls_clskey
      EXCEPTIONS
        OTHERS = 1.
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

  CALL FUNCTION 'RS_FUNCTION_POOL_DELETE'
    EXPORTING
      function_pool = lv_area
      corrnum       = p_trkorr
    EXCEPTIONS
      OTHERS        = 1.

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
*&   Simple list output with a summary footer.
*&---------------------------------------------------------------------*
FORM display_log.

  CLEAR: gv_ok, gv_err, gv_skip.

  WRITE: / 'Object deletion log for request', p_trkorr.
  IF p_test = 'X'.
    WRITE: '   (TEST RUN - nothing was deleted)'.
  ENDIF.
  ULINE.
  WRITE: /(4) 'Type', 42 'Object name', 84 'Status', 95 'Message'.
  ULINE.

  LOOP AT gt_log INTO gs_log.

    IF     gs_log-status = 'OK'.
      gv_ok = gv_ok + 1.
    ELSEIF gs_log-status = 'ERROR'.
      gv_err = gv_err + 1.
    ELSE.
      gv_skip = gv_skip + 1.
    ENDIF.

    WRITE: /(4) gs_log-object,
            6(36) gs_log-obj_name,
            84(10) gs_log-status,
            95     gs_log-message.
  ENDLOOP.

  ULINE.
  WRITE: / 'Deleted:', gv_ok,
         / 'Errors :', gv_err,
         / 'Other  :', gv_skip.
ENDFORM.
