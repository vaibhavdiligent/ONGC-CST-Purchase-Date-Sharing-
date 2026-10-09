*&---------------------------------------------------------------------*
*& Report  ZTR_OBJ_SYNC
*&---------------------------------------------------------------------*
*& Program Title : Transport object comparison between an OLD (remote)
*&                 system and THIS (new) system  -  PHASE 1: compare only
*&
*& Description   : The user enters one or more workbench transport
*&                 requests that exist in the OLD system plus an RFC
*&                 destination pointing to that system. The program:
*&
*&                 1. Reads the request headers (E070), their tasks and
*&                    the object lists (E071) from the OLD system via
*&                    the standard RFC-enabled FM RFC_READ_TABLE.
*&                 2. Resolves every entry to its R3TR main object with
*&                    TR_CHECK_TYPE and checks existence in THIS system
*&                    (TADIR).
*&                 3. For code-carrying entries it fetches the real
*&                    source from both systems and compares it:
*&                      - local  read : READ REPORT / SEO method include
*&                      - remote read : SVRS_GET_VERSION_REPS_40
*&                                      DESTINATION (versno 00000 =
*&                                      active source), include names
*&                                      resolved via TFDIR / TMDIR read
*&                                      with RFC_READ_TABLE
*&                 4. Classifies every object:
*&                      NEW        - does not exist in this system
*&                      DIFFERENT  - exists here but source differs
*&                      IDENTICAL  - exists here with identical source
*&                      EXISTS     - exists here, type not code-compared
*&                      ERROR      - source could not be read
*&                 5. Shows everything in a SALV grid with LED icons and
*&                    a summary header. Read-only: NOTHING is written.
*&
*&                 Phase 2 (separate step) will add "deploy selected
*&                 NEW objects" using the mechanisms already proven in
*&                 ZATC_PROG_UPLOAD (INSERT REPORT / RPY_PROGRAM_UPDATE
*&                 + CTS_WBO_API_INSERT_OBJECTS).
*&
*& Source-read patterns reused from ZATC_PROG_DOWNLOAD /
*& ZATC_RESULT_CORRECTION; RFC/authority handling modelled on the SAP
*& standard report /SDF/CMO_TR_CHECK (see analysis documents in repo).
*&---------------------------------------------------------------------*
REPORT ztr_obj_sync.

TABLES: e070.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_req,
         trkorr     TYPE trkorr,
         trfunction TYPE trfunction,
         trstatus   TYPE trstatus,
         as4user    TYPE tr_as4user,
         strkorr    TYPE strkorr,
         as4text    TYPE as4text,
         found      TYPE flag,
       END OF ty_req.

TYPES: BEGIN OF ty_e071,
         trkorr   TYPE trkorr,
         pgmid    TYPE pgmid,
         object   TYPE trobjtype,
         obj_name TYPE trobj_name,
       END OF ty_e071.

TYPES: BEGIN OF ty_result,
         icon      TYPE char4,
         status    TYPE char20,     " NEW / DIFFERENT / IDENTICAL ...
         pgmid     TYPE pgmid,
         object    TYPE trobjtype,
         obj_name  TYPE trobj_name,
         mainobj   TYPE trobjtype,  " resolved R3TR object type
         mainname  TYPE trobj_name, " resolved R3TR object name
         exists    TYPE char3,      " YES / NO
         include   TYPE progname,   " compared include (if any)
         lines_old TYPE i,          " source lines in OLD system
         lines_new TYPE i,          " source lines in THIS system
         trkorrs   TYPE char120,    " transports containing the entry
         remark    TYPE char120,
       END OF ty_result.

TYPES: ty_t_result TYPE STANDARD TABLE OF ty_result WITH EMPTY KEY.

TYPES: ty_srcline TYPE c LENGTH 255,
       ty_t_src   TYPE STANDARD TABLE OF ty_srcline WITH EMPTY KEY.

" generic parsed row coming back from RFC_READ_TABLE
TYPES: ty_t_vals TYPE STANDARD TABLE OF string WITH EMPTY KEY.
TYPES: BEGIN OF ty_row,
         vals TYPE ty_t_vals,
       END OF ty_row.
TYPES: ty_t_rows TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

*----------------------------------------------------------------------*
* Constants  (status icons)
*----------------------------------------------------------------------*
CONSTANTS: gc_icon_new   TYPE char4 VALUE '@5C@',  " red    LED
           gc_icon_diff  TYPE char4 VALUE '@5D@',  " yellow LED
           gc_icon_same  TYPE char4 VALUE '@5B@',  " green  LED
           gc_icon_info  TYPE char4 VALUE '@0S@',  " blue   info
           gc_icon_error TYPE char4 VALUE '@0A@'.  " red cross

*----------------------------------------------------------------------*
* Global data
*----------------------------------------------------------------------*
DATA: gt_req     TYPE STANDARD TABLE OF ty_req,
      gt_e071    TYPE STANDARD TABLE OF ty_e071,
      gt_result  TYPE ty_t_result,
      gv_old_sid TYPE sysysid,
      gv_error   TYPE flag.

DATA: gv_cnt_new  TYPE i,
      gv_cnt_diff TYPE i,
      gv_cnt_same TYPE i,
      gv_cnt_exi  TYPE i,
      gv_cnt_err  TYPE i.

DATA: lv_ori_tr TYPE e070-trkorr.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS p_rfc TYPE rfcdes-rfcdest OBLIGATORY.      " RFC to OLD system
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS s_tr FOR lv_ori_tr NO INTERVALS OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS p_tasks TYPE flag AS CHECKBOX DEFAULT 'X'. " incl. task objects
  PARAMETERS p_code  TYPE flag AS CHECKBOX DEFAULT 'X'. " compare source code
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* Start
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM authority_check.

  PERFORM validate_rfc CHANGING gv_error.
  IF gv_error = 'X'.
    RETURN.
  ENDIF.

  PERFORM read_requests.
  PERFORM read_object_lists.

  IF gt_e071[] IS INITIAL.
    MESSAGE i398(00) WITH 'No objects found in the given requests'
                          'in the old system' '' ''.
    RETURN.
  ENDIF.

  PERFORM build_results.
  PERFORM display_results.

*&---------------------------------------------------------------------*
*& Form authority_check
*&---------------------------------------------------------------------*
FORM authority_check.
  AUTHORITY-CHECK OBJECT 'S_TRANSPRT'
    ID 'ACTVT' FIELD '03'.
  IF sy-subrc <> 0.
    MESSAGE e398(00) WITH 'Missing authorization S_TRANSPRT'
                          '(display transports)' '' ''.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_rfc - destination must exist and answer
*&---------------------------------------------------------------------*
FORM validate_rfc CHANGING pv_error TYPE flag.

  DATA: ls_rfcsi TYPE rfcsi.

  CLEAR pv_error.

  CALL FUNCTION 'RFC_SYSTEM_INFO'
    DESTINATION p_rfc
    IMPORTING
      rfcsi_export          = ls_rfcsi
    EXCEPTIONS
      communication_failure = 1 MESSAGE sy-msgv1
      system_failure        = 2 MESSAGE sy-msgv1
      OTHERS                = 3.
  IF sy-subrc <> 0.
    MESSAGE i398(00) WITH 'RFC destination not reachable:' p_rfc
                          sy-msgv1 ''.
    pv_error = 'X'.
    RETURN.
  ENDIF.

  gv_old_sid = ls_rfcsi-rfcsysid.

  IF gv_old_sid = sy-sysid.
    MESSAGE i398(00) WITH 'Warning: RFC points to THIS system'
                          '- old and new system are identical' '' ''.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form read_requests - E070/E07T of the entered requests + their tasks
*&---------------------------------------------------------------------*
FORM read_requests.

  DATA: lt_rows TYPE ty_t_rows,
        ls_req  TYPE ty_req,
        lv_opt  TYPE string.

  LOOP AT s_tr.

    CLEAR ls_req.
    ls_req-trkorr = s_tr-low.

    " ---- header of the request itself --------------------------------
    CLEAR lt_rows.
    CONCATENATE 'TRKORR EQ ''' s_tr-low '''' INTO lv_opt.
    PERFORM rfc_read_table
      USING p_rfc 'E070'
            'TRKORR,TRFUNCTION,TRSTATUS,AS4USER,STRKORR'
            lv_opt
      CHANGING lt_rows.

    READ TABLE lt_rows INTO DATA(ls_row) INDEX 1.
    IF sy-subrc = 0.
      ls_req-found = 'X'.
      READ TABLE ls_row-vals INTO DATA(lv_val) INDEX 2.
      ls_req-trfunction = lv_val.
      READ TABLE ls_row-vals INTO lv_val INDEX 3.
      ls_req-trstatus = lv_val.
      READ TABLE ls_row-vals INTO lv_val INDEX 4.
      ls_req-as4user = lv_val.
      READ TABLE ls_row-vals INTO lv_val INDEX 5.
      ls_req-strkorr = lv_val.
    ELSE.
      " request does not exist in the old system -> error line in output
      DATA(ls_res) = VALUE ty_result(
        icon     = gc_icon_error
        status   = 'ERROR'
        obj_name = s_tr-low
        remark   = 'Request not found in old system' ).
      APPEND ls_res TO gt_result.
      gv_cnt_err = gv_cnt_err + 1.
      CONTINUE.
    ENDIF.

    " ---- description --------------------------------------------------
    CLEAR lt_rows.
    CONCATENATE 'TRKORR EQ ''' s_tr-low ''' AND LANGU EQ ''E'''
      INTO lv_opt.
    PERFORM rfc_read_table
      USING p_rfc 'E07T' 'AS4TEXT' lv_opt
      CHANGING lt_rows.
    READ TABLE lt_rows INTO ls_row INDEX 1.
    IF sy-subrc = 0.
      READ TABLE ls_row-vals INTO lv_val INDEX 1.
      ls_req-as4text = lv_val.
    ENDIF.

    APPEND ls_req TO gt_req.

    " ---- tasks below the request (objects usually sit in tasks) -------
    IF p_tasks = 'X'.
      CLEAR lt_rows.
      CONCATENATE 'STRKORR EQ ''' s_tr-low '''' INTO lv_opt.
      PERFORM rfc_read_table
        USING p_rfc 'E070'
              'TRKORR,TRFUNCTION,TRSTATUS,AS4USER,STRKORR'
              lv_opt
        CHANGING lt_rows.
      LOOP AT lt_rows INTO ls_row.
        CLEAR ls_req.
        ls_req-found = 'X'.
        READ TABLE ls_row-vals INTO lv_val INDEX 1.
        ls_req-trkorr = lv_val.
        READ TABLE ls_row-vals INTO lv_val INDEX 2.
        ls_req-trfunction = lv_val.
        READ TABLE ls_row-vals INTO lv_val INDEX 5.
        ls_req-strkorr = lv_val.
        APPEND ls_req TO gt_req.
      ENDLOOP.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form read_object_lists - E071 rows of requests and tasks (old system)
*&---------------------------------------------------------------------*
FORM read_object_lists.

  DATA: lt_rows TYPE ty_t_rows,
        ls_071  TYPE ty_e071,
        lv_opt  TYPE string.

  LOOP AT gt_req INTO DATA(ls_req) WHERE found = 'X'.

    CLEAR lt_rows.
    CONCATENATE 'TRKORR EQ ''' ls_req-trkorr '''' INTO lv_opt.
    PERFORM rfc_read_table
      USING p_rfc 'E071' 'TRKORR,PGMID,OBJECT,OBJ_NAME' lv_opt
      CHANGING lt_rows.

    LOOP AT lt_rows INTO DATA(ls_row).
      CLEAR ls_071.
      READ TABLE ls_row-vals INTO DATA(lv_val) INDEX 1.
      ls_071-trkorr = lv_val.
      READ TABLE ls_row-vals INTO lv_val INDEX 2.
      ls_071-pgmid = lv_val.
      READ TABLE ls_row-vals INTO lv_val INDEX 3.
      ls_071-object = lv_val.
      READ TABLE ls_row-vals INTO lv_val INDEX 4.
      ls_071-obj_name = lv_val.

      " only real repository entries; ignore CORR/RELE bookkeeping rows
      IF ls_071-pgmid = 'R3TR' OR ls_071-pgmid = 'LIMU'.
        " report the parent request, not the task, in the output
        IF ls_req-strkorr IS NOT INITIAL.
          ls_071-trkorr = ls_req-strkorr.
        ENDIF.
        APPEND ls_071 TO gt_e071.
      ENDIF.
    ENDLOOP.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form build_results - dedupe, resolve, check existence, compare code
*&---------------------------------------------------------------------*
FORM build_results.

  DATA: ls_res     TYPE ty_result,
        ls_e071_in TYPE e071,
        ls_tadir   TYPE tadir,
        lv_exists  TYPE flag,
        lv_delflag TYPE tadir-delflag.

  " ---- de-duplicate object entries, collect their transports ----------
  TYPES: BEGIN OF ty_uni,
           pgmid    TYPE pgmid,
           object   TYPE trobjtype,
           obj_name TYPE trobj_name,
           trkorrs  TYPE char120,
         END OF ty_uni.
  DATA: lt_uni TYPE STANDARD TABLE OF ty_uni,
        ls_uni TYPE ty_uni.

  SORT gt_e071 BY pgmid object obj_name trkorr.
  DELETE ADJACENT DUPLICATES FROM gt_e071
    COMPARING pgmid object obj_name trkorr.

  LOOP AT gt_e071 INTO DATA(ls_071).
    READ TABLE lt_uni ASSIGNING FIELD-SYMBOL(<uni>)
      WITH KEY pgmid    = ls_071-pgmid
               object   = ls_071-object
               obj_name = ls_071-obj_name.
    IF sy-subrc = 0.
      IF <uni>-trkorrs NS ls_071-trkorr.
        CONCATENATE <uni>-trkorrs ls_071-trkorr
          INTO <uni>-trkorrs SEPARATED BY ','.
      ENDIF.
    ELSE.
      CLEAR ls_uni.
      ls_uni-pgmid    = ls_071-pgmid.
      ls_uni-object   = ls_071-object.
      ls_uni-obj_name = ls_071-obj_name.
      ls_uni-trkorrs  = ls_071-trkorr.
      APPEND ls_uni TO lt_uni.
    ENDIF.
  ENDLOOP.

  " ---- classify every unique entry ------------------------------------
  LOOP AT lt_uni INTO ls_uni.

    CLEAR: ls_res, ls_tadir, lv_exists, lv_delflag.
    ls_res-pgmid    = ls_uni-pgmid.
    ls_res-object   = ls_uni-object.
    ls_res-obj_name = ls_uni-obj_name.
    ls_res-trkorrs  = ls_uni-trkorrs.

    " resolve LIMU sub-object to its R3TR main object
    CLEAR ls_e071_in.
    ls_e071_in-pgmid    = ls_uni-pgmid.
    ls_e071_in-object   = ls_uni-object.
    ls_e071_in-obj_name = ls_uni-obj_name.
    CALL FUNCTION 'TR_CHECK_TYPE'
      EXPORTING
        wi_e071  = ls_e071_in
      IMPORTING
        we_tadir = ls_tadir.
    IF ls_tadir-object IS NOT INITIAL.
      ls_res-mainobj  = ls_tadir-object.
      ls_res-mainname = ls_tadir-obj_name.
    ELSE.
      ls_res-mainobj  = ls_uni-object.
      ls_res-mainname = ls_uni-obj_name.
    ENDIF.

    " does the main object exist in THIS system?
    SELECT SINGLE delflag FROM tadir INTO lv_delflag
      WHERE pgmid    = 'R3TR'
        AND object   = ls_res-mainobj
        AND obj_name = ls_res-mainname.
    IF sy-subrc = 0 AND lv_delflag IS INITIAL.
      lv_exists = 'X'.
      ls_res-exists = 'YES'.
    ELSE.
      ls_res-exists = 'NO'.
    ENDIF.

    IF lv_exists IS INITIAL.
      ls_res-icon   = gc_icon_new.
      ls_res-status = 'NEW'.
      ls_res-remark = 'Not in this system - deploy candidate (Phase 2)'.
      gv_cnt_new = gv_cnt_new + 1.
      APPEND ls_res TO gt_result.
      CONTINUE.
    ENDIF.

    " object exists here -> compare source where we can
    IF p_code = 'X'.
      PERFORM compare_code CHANGING ls_res.
    ELSE.
      ls_res-icon   = gc_icon_info.
      ls_res-status = 'EXISTS'.
      ls_res-remark = 'Code comparison switched off'.
    ENDIF.

    CASE ls_res-status.
      WHEN 'NEW'.       gv_cnt_new  = gv_cnt_new  + 1.
      WHEN 'DIFFERENT'. gv_cnt_diff = gv_cnt_diff + 1.
      WHEN 'IDENTICAL'. gv_cnt_same = gv_cnt_same + 1.
      WHEN 'ERROR'.     gv_cnt_err  = gv_cnt_err  + 1.
      WHEN OTHERS.      gv_cnt_exi  = gv_cnt_exi  + 1.
    ENDCASE.

    APPEND ls_res TO gt_result.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form compare_code - decide the include to compare and do it
*&---------------------------------------------------------------------*
*& Comparable entries and how their include name is found:
*&   R3TR PROG / LIMU REPS : include = obj_name
*&   LIMU CINC             : obj_name is already the include name
*&   LIMU CPUB/CPRO/CPRI   : class section include (deterministic name)
*&   R3TR INTF             : interface pool include (deterministic)
*&   LIMU FUNC             : include from TFDIR (local + remote)
*&   LIMU METH             : include from SEO (local) / TMDIR (remote)
*&   everything else       : existence only ("EXISTS")
*&---------------------------------------------------------------------*
FORM compare_code CHANGING ps_res TYPE ty_result.

  DATA: lv_inc_new TYPE progname,   " include in THIS system
        lv_inc_old TYPE progname,   " include in OLD system
        lt_new     TYPE ty_t_src,
        lt_old     TYPE ty_t_src,
        lv_subrc   TYPE sy-subrc,
        lv_cls     TYPE seoclsname,
        lv_meth    TYPE c LENGTH 61.

  CASE ps_res-object.

    WHEN 'PROG' OR 'REPS'.
      lv_inc_new = ps_res-obj_name.
      lv_inc_old = lv_inc_new.

    WHEN 'CINC'.
      lv_inc_new = ps_res-obj_name.
      lv_inc_old = lv_inc_new.

    WHEN 'CPUB' OR 'CPRO' OR 'CPRI'.
      lv_cls = ps_res-obj_name.
      PERFORM class_section_include
        USING lv_cls ps_res-object
        CHANGING lv_inc_new.
      lv_inc_old = lv_inc_new.               " deterministic name

    WHEN 'INTF'.
      lv_cls = ps_res-obj_name.
      PERFORM class_section_include
        USING lv_cls 'INTF'
        CHANGING lv_inc_new.
      lv_inc_old = lv_inc_new.

    WHEN 'FUNC'.
      PERFORM func_include_local
        USING ps_res-obj_name CHANGING lv_inc_new.
      PERFORM func_include_remote
        USING ps_res-obj_name CHANGING lv_inc_old.
      IF lv_inc_new IS INITIAL OR lv_inc_old IS INITIAL.
        ps_res-icon   = gc_icon_error.
        ps_res-status = 'ERROR'.
        ps_res-remark = 'Function include not resolvable'.
        RETURN.
      ENDIF.

    WHEN 'METH'.
      lv_cls  = ps_res-obj_name(30).
      lv_meth = ps_res-obj_name+30.
      CONDENSE: lv_cls, lv_meth.
      PERFORM meth_include_local
        USING lv_cls lv_meth CHANGING lv_inc_new.
      PERFORM meth_include_remote
        USING lv_cls lv_meth CHANGING lv_inc_old.
      IF lv_inc_new IS INITIAL OR lv_inc_old IS INITIAL.
        ps_res-icon   = gc_icon_error.
        ps_res-status = 'ERROR'.
        ps_res-remark = 'Method include not resolvable'.
        RETURN.
      ENDIF.

    WHEN 'CLAS'.
      ps_res-icon   = gc_icon_info.
      ps_res-status = 'EXISTS'.
      ps_res-remark =
        'Whole class - methods/sections are compared as own entries'.
      RETURN.

    WHEN 'FUGR' OR 'FUGS'.
      ps_res-icon   = gc_icon_info.
      ps_res-status = 'EXISTS'.
      ps_res-remark =
        'Whole function group - FUNC entries compared individually'.
      RETURN.

    WHEN OTHERS.
      ps_res-icon   = gc_icon_info.
      ps_res-status = 'EXISTS'.
      ps_res-remark = 'Type not code-comparable (DDIC/screen/other)'.
      RETURN.

  ENDCASE.

  ps_res-include = lv_inc_new.

  " ---- local (new system) source --------------------------------------
  PERFORM read_source_local
    USING lv_inc_new CHANGING lt_new lv_subrc.
  IF lv_subrc <> 0.
    " main object exists but this piece does not -> the piece is new
    ps_res-icon   = gc_icon_new.
    ps_res-status = 'NEW'.
    ps_res-remark = 'Sub-object missing in this system'.
    ps_res-lines_new = 0.
  ENDIF.

  " ---- remote (old system) source --------------------------------------
  PERFORM read_source_remote
    USING lv_inc_old CHANGING lt_old lv_subrc.
  IF lv_subrc <> 0.
    ps_res-icon   = gc_icon_error.
    ps_res-status = 'ERROR'.
    ps_res-remark = 'Source not readable in old system via RFC'.
    RETURN.
  ENDIF.

  ps_res-lines_old = lines( lt_old ).
  IF ps_res-status = 'NEW'.
    RETURN.
  ENDIF.
  ps_res-lines_new = lines( lt_new ).

  " ---- compare ----------------------------------------------------------
  PERFORM sources_equal USING lt_old lt_new CHANGING lv_subrc.
  IF lv_subrc = 0.
    ps_res-icon   = gc_icon_same.
    ps_res-status = 'IDENTICAL'.
    ps_res-remark = 'Source code identical in both systems'.
  ELSE.
    ps_res-icon   = gc_icon_diff.
    ps_res-status = 'DIFFERENT'.
    ps_res-remark = 'Source code differs between the systems'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form class_section_include - deterministic class/interface includes
*&---------------------------------------------------------------------*
FORM class_section_include USING pv_cls  TYPE seoclsname
                                 pv_kind TYPE trobjtype
                           CHANGING pv_inc TYPE progname.

  DATA lv_pool TYPE c LENGTH 30.

  lv_pool = pv_cls.
  TRANSLATE lv_pool USING ' ='.               " pad with '='

  CASE pv_kind.
    WHEN 'CPUB'. CONCATENATE lv_pool 'CU' INTO pv_inc.
    WHEN 'CPRO'. CONCATENATE lv_pool 'CO' INTO pv_inc.
    WHEN 'CPRI'. CONCATENATE lv_pool 'CI' INTO pv_inc.
    WHEN 'INTF'. CONCATENATE lv_pool 'IU' INTO pv_inc.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form func_include_local - TFDIR of this system
*&---------------------------------------------------------------------*
FORM func_include_local USING pv_func TYPE trobj_name
                        CHANGING pv_inc TYPE progname.

  DATA: lv_pname   TYPE tfdir-pname,
        lv_incno   TYPE tfdir-include,
        lv_funcname TYPE tfdir-funcname.

  CLEAR pv_inc.
  lv_funcname = pv_func.

  SELECT SINGLE pname include FROM tfdir
    INTO (lv_pname, lv_incno)
    WHERE funcname = lv_funcname.
  IF sy-subrc = 0.
    PERFORM func_include_build
      USING lv_pname lv_incno CHANGING pv_inc.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form func_include_remote - TFDIR of the old system via RFC_READ_TABLE
*&---------------------------------------------------------------------*
FORM func_include_remote USING pv_func TYPE trobj_name
                         CHANGING pv_inc TYPE progname.

  DATA: lt_rows TYPE ty_t_rows,
        lv_opt  TYPE string,
        lv_pname TYPE tfdir-pname,
        lv_incno TYPE tfdir-include.

  CLEAR pv_inc.
  CONCATENATE 'FUNCNAME EQ ''' pv_func '''' INTO lv_opt.
  PERFORM rfc_read_table
    USING p_rfc 'TFDIR' 'PNAME,INCLUDE' lv_opt
    CHANGING lt_rows.

  READ TABLE lt_rows INTO DATA(ls_row) INDEX 1.
  IF sy-subrc = 0.
    READ TABLE ls_row-vals INTO DATA(lv_val) INDEX 1.
    lv_pname = lv_val.
    READ TABLE ls_row-vals INTO lv_val INDEX 2.
    lv_incno = lv_val.
    PERFORM func_include_build
      USING lv_pname lv_incno CHANGING pv_inc.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form func_include_build - SAPLGRP + nn -> LGRPUnn (namespace-aware)
*&---------------------------------------------------------------------*
FORM func_include_build USING pv_pname TYPE tfdir-pname
                              pv_incno TYPE tfdir-include
                        CHANGING pv_inc TYPE progname.

  DATA: lv_ns   TYPE string,
        lv_rest TYPE string,
        lv_stem TYPE string,
        lv_no   TYPE c LENGTH 3.

  CLEAR pv_inc.
  lv_no = pv_incno.
  CONDENSE lv_no NO-GAPS.
  IF pv_pname(1) = '/'.
    " /NAMESPC/SAPLGROUP -> /NAMESPC/LGROUPU<nn>
    SPLIT pv_pname+1 AT '/' INTO lv_ns lv_rest.
    IF strlen( lv_rest ) > 4 AND lv_rest(4) = 'SAPL'.
      lv_stem = lv_rest+4.
      CONCATENATE '/' lv_ns '/L' lv_stem 'U' lv_no INTO pv_inc.
    ENDIF.
  ELSE.
    IF pv_pname(4) = 'SAPL'.
      lv_stem = pv_pname+4.
      CONDENSE lv_stem NO-GAPS.
      CONCATENATE 'L' lv_stem 'U' lv_no INTO pv_inc.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form meth_include_local - SEO API of this system
*&---------------------------------------------------------------------*
FORM meth_include_local USING pv_cls  TYPE seoclsname
                              pv_meth TYPE c
                        CHANGING pv_inc TYPE progname.

  DATA: ls_clskey   TYPE seoclskey,
        lt_includes TYPE seop_methods_w_include.

  CLEAR pv_inc.
  ls_clskey-clsname = pv_cls.

  CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
    EXPORTING
      clskey                       = ls_clskey
    IMPORTING
      includes                     = lt_includes
    EXCEPTIONS
      _internal_class_not_existing = 1
      OTHERS                       = 2.
  IF sy-subrc = 0.
    READ TABLE lt_includes INTO DATA(ls_inc)
      WITH KEY cpdkey-cpdname = pv_meth.
    IF sy-subrc = 0.
      pv_inc = ls_inc-incname.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form meth_include_remote - TMDIR of the old system via RFC_READ_TABLE
*&---------------------------------------------------------------------*
*& Method includes are generated as <class padded to 30 with '='>CM<nnn>
*& where <nnn> comes from TMDIR-METHODINDX. The numbering is per system,
*& so the OLD system's TMDIR must be read - the local include name must
*& NOT be assumed to match.
*&---------------------------------------------------------------------*
FORM meth_include_remote USING pv_cls  TYPE seoclsname
                               pv_meth TYPE c
                         CHANGING pv_inc TYPE progname.

  DATA: lt_rows  TYPE ty_t_rows,
        lv_opt   TYPE string,
        lv_indx  TYPE n LENGTH 5,
        lv_pool  TYPE c LENGTH 30,
        lv_sfx   TYPE c LENGTH 3.

  CLEAR pv_inc.
  CONCATENATE 'CLSNAME EQ ''' pv_cls ''' AND METHODNAME EQ '''
              pv_meth '''' INTO lv_opt.
  PERFORM rfc_read_table
    USING p_rfc 'TMDIR' 'METHODINDX' lv_opt
    CHANGING lt_rows.

  READ TABLE lt_rows INTO DATA(ls_row) INDEX 1.
  IF sy-subrc = 0.
    READ TABLE ls_row-vals INTO DATA(lv_val) INDEX 1.
    lv_indx = lv_val.
    lv_sfx  = lv_indx+2(3).
    lv_pool = pv_cls.
    TRANSLATE lv_pool USING ' ='.
    CONCATENATE lv_pool 'CM' lv_sfx INTO pv_inc.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form read_source_local - active source of an include in THIS system
*&---------------------------------------------------------------------*
FORM read_source_local USING pv_inc TYPE progname
                       CHANGING pt_src   TYPE ty_t_src
                                pv_subrc TYPE sy-subrc.

  CLEAR pt_src.
  READ REPORT pv_inc INTO pt_src.
  pv_subrc = sy-subrc.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form read_source_remote - active source from the OLD system
*&---------------------------------------------------------------------*
*& Same FM the download program uses locally, called with DESTINATION.
*& versno 00000 = the active version.
*&---------------------------------------------------------------------*
FORM read_source_remote USING pv_inc TYPE progname
                        CHANGING pt_src   TYPE ty_t_src
                                 pv_subrc TYPE sy-subrc.

  DATA: lt_repos TYPE STANDARD TABLE OF abaptxt255,
        lv_name  TYPE vrsd-objname.

  CLEAR pt_src.
  lv_name = pv_inc.

  CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
    DESTINATION p_rfc
    EXPORTING
      object_name           = lv_name
      versno                = '00000'
    TABLES
      repos_tab             = lt_repos
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2 MESSAGE sy-msgv1
      communication_failure = 3 MESSAGE sy-msgv1
      OTHERS                = 4.
  pv_subrc = sy-subrc.
  IF pv_subrc = 0.
    LOOP AT lt_repos INTO DATA(ls_repos).
      APPEND ls_repos-line TO pt_src.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form sources_equal - 0 = identical (trailing blanks ignored)
*&---------------------------------------------------------------------*
FORM sources_equal USING pt_old TYPE ty_t_src
                         pt_new TYPE ty_t_src
                   CHANGING pv_subrc TYPE sy-subrc.

  DATA: lt_old TYPE ty_t_src,
        lt_new TYPE ty_t_src.

  lt_old = pt_old.
  lt_new = pt_new.

  " normalise: remove trailing spaces and trailing empty lines
  LOOP AT lt_old ASSIGNING FIELD-SYMBOL(<l1>).
    SHIFT <l1> RIGHT DELETING TRAILING space.
    SHIFT <l1> LEFT DELETING LEADING space.
  ENDLOOP.
  LOOP AT lt_new ASSIGNING FIELD-SYMBOL(<l2>).
    SHIFT <l2> RIGHT DELETING TRAILING space.
    SHIFT <l2> LEFT DELETING LEADING space.
  ENDLOOP.
  DELETE lt_old WHERE table_line IS INITIAL.
  DELETE lt_new WHERE table_line IS INITIAL.

  IF lt_old = lt_new.
    pv_subrc = 0.
  ELSE.
    pv_subrc = 4.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form rfc_read_table - generic remote table read + offset parsing
*&---------------------------------------------------------------------*
*& pv_fields: comma-separated field list, pv_where: one WHERE line.
*& Result: one ty_row per data row, vals in field-list order.
*&---------------------------------------------------------------------*
FORM rfc_read_table USING pv_dest   TYPE rfcdes-rfcdest
                          pv_table  TYPE string
                          pv_fields TYPE string
                          pv_where  TYPE string
                    CHANGING pt_rows TYPE ty_t_rows.

  DATA: lt_fields  TYPE STANDARD TABLE OF rfc_db_fld,
        ls_field   TYPE rfc_db_fld,
        lt_options TYPE STANDARD TABLE OF rfc_db_opt,
        ls_option  TYPE rfc_db_opt,
        lt_data    TYPE STANDARD TABLE OF tab512,
        lt_names   TYPE STANDARD TABLE OF string,
        lv_table   TYPE dd02l-tabname,
        ls_row     TYPE ty_row,
        lv_off     TYPE i,
        lv_len     TYPE i,
        lv_val     TYPE string.

  CLEAR pt_rows.
  lv_table = pv_table.

  SPLIT pv_fields AT ',' INTO TABLE lt_names.
  LOOP AT lt_names INTO DATA(lv_name).
    CLEAR ls_field.
    ls_field-fieldname = lv_name.
    APPEND ls_field TO lt_fields.
  ENDLOOP.

  IF pv_where IS NOT INITIAL.
    " split the WHERE condition into option lines; break only at spaces
    " so that quoted literals are never cut in the middle
    DATA: lv_rest TYPE string,
          lv_pos  TYPE i.
    lv_rest = pv_where.
    WHILE strlen( lv_rest ) > 72.
      lv_pos = 72.
      WHILE lv_pos > 0 AND lv_rest+lv_pos(1) <> ' '.
        lv_pos = lv_pos - 1.
      ENDWHILE.
      IF lv_pos = 0.                 " no space found - hard split
        lv_pos = 72.
      ENDIF.
      ls_option-text = lv_rest(lv_pos).
      APPEND ls_option TO lt_options.
      lv_rest = lv_rest+lv_pos.
      SHIFT lv_rest LEFT DELETING LEADING space.
    ENDWHILE.
    IF lv_rest IS NOT INITIAL.
      ls_option-text = lv_rest.
      APPEND ls_option TO lt_options.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'RFC_READ_TABLE'
    DESTINATION pv_dest
    EXPORTING
      query_table           = lv_table
      delimiter             = ''
    TABLES
      options               = lt_options
      fields                = lt_fields
      data                  = lt_data
    EXCEPTIONS
      table_not_available   = 1
      table_without_data    = 2
      option_not_valid      = 3
      field_not_valid       = 4
      not_authorized        = 5
      data_buffer_exceeded  = 6
      system_failure        = 7 MESSAGE sy-msgv1
      communication_failure = 8 MESSAGE sy-msgv1
      OTHERS                = 9.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  " parse fixed-offset rows using the offsets RFC_READ_TABLE returned
  LOOP AT lt_data INTO DATA(ls_data).
    CLEAR ls_row.
    LOOP AT lt_fields INTO ls_field.
      lv_off = ls_field-offset.
      lv_len = ls_field-length.
      IF lv_off + lv_len <= 512.
        lv_val = ls_data-wa+lv_off(lv_len).
      ELSE.
        CLEAR lv_val.
      ENDIF.
      CONDENSE lv_val.
      APPEND lv_val TO ls_row-vals.
    ENDLOOP.
    APPEND ls_row TO pt_rows.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form display_results - SALV grid with summary header
*&---------------------------------------------------------------------*
FORM display_results.

  DATA: lo_alv     TYPE REF TO cl_salv_table,
        lo_columns TYPE REF TO cl_salv_columns_table,
        lo_col     TYPE REF TO cl_salv_column_table,
        lo_grid    TYPE REF TO cl_salv_form_layout_grid,
        lv_text    TYPE string.

  SORT gt_result BY status pgmid object obj_name.

  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_alv
        CHANGING  t_table      = gt_result ).
    CATCH cx_salv_msg.
      MESSAGE i398(00) WITH 'ALV could not be created' '' '' ''.
      RETURN.
  ENDTRY.

  lo_alv->get_functions( )->set_all( abap_true ).
  lo_columns = lo_alv->get_columns( ).
  lo_columns->set_optimize( abap_true ).

  TRY.
      lo_col ?= lo_columns->get_column( 'ICON' ).
      lo_col->set_icon( if_salv_c_bool_sap=>true ).
      lo_col->set_short_text( 'Light' ).
      lo_col->set_medium_text( 'Light' ).
      lo_col->set_long_text( 'Light' ).

      lo_col ?= lo_columns->get_column( 'STATUS' ).
      lo_col->set_medium_text( 'Result' ).
      lo_col ?= lo_columns->get_column( 'OBJ_NAME' ).
      lo_col->set_medium_text( 'Object Name' ).
      lo_col ?= lo_columns->get_column( 'MAINOBJ' ).
      lo_col->set_medium_text( 'Main Type' ).
      lo_col ?= lo_columns->get_column( 'MAINNAME' ).
      lo_col->set_medium_text( 'Main Object' ).
      lo_col ?= lo_columns->get_column( 'EXISTS' ).
      lo_col->set_medium_text( 'In this sys' ).
      lo_col ?= lo_columns->get_column( 'INCLUDE' ).
      lo_col->set_medium_text( 'Compared Incl.' ).
      lo_col ?= lo_columns->get_column( 'LINES_OLD' ).
      lo_col->set_medium_text( 'Lines old sys' ).
      lo_col ?= lo_columns->get_column( 'LINES_NEW' ).
      lo_col->set_medium_text( 'Lines this sys' ).
      lo_col ?= lo_columns->get_column( 'TRKORRS' ).
      lo_col->set_medium_text( 'Transport(s)' ).
      lo_col ?= lo_columns->get_column( 'REMARK' ).
      lo_col->set_medium_text( 'Remark' ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  " summary header
  CREATE OBJECT lo_grid.
  lo_grid->create_label( row = 1 column = 1
    text = 'Transport Object Comparison (Phase 1 - read only)' ).
  lv_text = |Old system: { gv_old_sid } via { p_rfc } | &&
            |/ This system: { sy-sysid }|.
  lo_grid->create_text( row = 2 column = 1 text = lv_text ).
  lv_text = |NEW: { gv_cnt_new } / | &&
            |DIFFERENT: { gv_cnt_diff } / | &&
            |IDENTICAL: { gv_cnt_same } / | &&
            |EXISTS (not compared): { gv_cnt_exi } / | &&
            |ERRORS: { gv_cnt_err }|.
  lo_grid->create_text( row = 3 column = 1 text = lv_text ).
  lo_alv->set_top_of_list( lo_grid ).

  lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
  lo_alv->get_display_settings( )->set_list_header(
    'ZTR_OBJ_SYNC - Object Comparison Old vs New System' ).

  lo_alv->display( ).

ENDFORM.

*----------------------------------------------------------------------*
* Text symbols (create in SE38 -> Goto -> Text Elements):
*   TEXT-001  Old (source) system connection
*   TEXT-002  Workbench requests in the old system
*   TEXT-003  Options
* Selection texts:
*   P_RFC     RFC to old system
*   S_TR      Transport request
*   P_TASKS   Include objects of tasks
*   P_CODE    Compare source code
*----------------------------------------------------------------------*
