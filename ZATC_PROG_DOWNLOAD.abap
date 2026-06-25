*&---------------------------------------------------------------------*
*& Report  ZATC_PROG_DOWNLOAD
*&---------------------------------------------------------------------*
*& Program Title : Download the exact ATC-flagged sub-objects as text
*&                 files + an Excel manifest
*&
*& Description   : Companion downloader for ZATC_RESULT_CORRECTION.
*&                 The user supplies an ATC result (run series) name on
*&                 the selection screen. The program reads the ATC
*&                 findings and, exactly like the correction program,
*&                 resolves each finding to the precise sub-object that
*&                 carries the code (finding-sobjname): a program include,
*&                 a class method include, or a function-group include.
*&                 Only those sub-objects are downloaded - one .txt file
*&                 each - into the chosen folder.
*&
*&                 After all downloads an Excel manifest is written with
*&                 Object Type | Object Name | Sub Object | File Name,
*&                 using the standard FM SAP_CONVERT_TO_XLS_FORMAT. The
*&                 manifest is the input for ZATC_PROG_UPLOAD.
*&
*& Source read   : PROG / FUGR / FUGS / SFPF / SSFO -> SVRS_GET_VERSION_REPS_40
*&                 CLAS                              -> SEO method/section source
*&                 (identical to ZATC_RESULT_CORRECTION).
*&---------------------------------------------------------------------*
REPORT zatc_prog_download.

TABLES: tadir, trnspace.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_obj,
         objtype  TYPE char4,    " PROG / CLAS / FUGR / SSFO ...
         objname  TYPE char40,   " main object (program / class / fgr)
         sobjname TYPE char40,   " exact include / method / function include
       END OF ty_obj.

TYPES: BEGIN OF ty_manifest,
         objtype  TYPE char4,
         objname  TYPE char40,
         sobjname TYPE char40,
         filename TYPE string,
       END OF ty_manifest.

TYPES: BEGIN OF ty_log,
         objtype  TYPE char4,
         objname  TYPE char40,
         sobjname TYPE char40,
         filename TYPE string,
         status   TYPE char10,           " Success / Skipped / Error
         message  TYPE char100,
       END OF ty_log.

*----------------------------------------------------------------------*
* Global data
*----------------------------------------------------------------------*
DATA: gt_obj           TYPE STANDARD TABLE OF ty_obj,
      gt_manifest      TYPE STANDARD TABLE OF ty_manifest,
      gt_findings_full TYPE scit_rest,    " full ATC findings (all fields)
      gt_log           TYPE STANDARD TABLE OF ty_log,
      gv_sep           TYPE c LENGTH 1.

" Source read work areas (same kinds the correction program uses)
DATA: repos_tab          TYPE STANDARD TABLE OF abaptxt255,
      wa_blank           TYPE abaptxt255,
      object_name        TYPE vrsd-objname,
      l_seoclskey        TYPE seoclskey,
      it_includes        TYPE seop_methods_w_include,
      ls_mtdkey          TYPE seocpdkey,
      lt_source          TYPE seop_source,
      ex_source_code_tab TYPE seop_source_string,
      lt_source_seo      TYPE seo_section_source,
      l_program_sec      TYPE program,
      l_clkey            TYPE seoclskey.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  PARAMETERS     p_id  TYPE satc_d_ac_title OBLIGATORY.        " ATC result name
  SELECT-OPTIONS s_obj FOR tadir-obj_name.                     " object filter
  SELECT-OPTIONS s_nsp FOR trnspace-namespace.                 " namespace filter
  PARAMETERS     p_dir TYPE string OBLIGATORY.                 " target folder
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* F4 help: ATC result (run series) name
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_id.
  SELECT DISTINCT run_series_name INTO TABLE @DATA(it_run_series_name)
    FROM satc_ac_resulth.
  IF sy-subrc = 0.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'RUN_SERIES_NAME'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = 'P_ID'
        value_org       = 'S'
      TABLES
        value_tab       = it_run_series_name
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
  ENDIF.

*----------------------------------------------------------------------*
* F4 help: target folder
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_dir.
  PERFORM f4_folder CHANGING p_dir.

*----------------------------------------------------------------------*
INITIALIZATION.
  cl_gui_frontend_services=>get_file_separator(
    CHANGING  file_separator = gv_sep
    EXCEPTIONS OTHERS = 1 ).
  IF gv_sep IS INITIAL.
    gv_sep = '\'.
  ENDIF.

*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM run_in_gui.
  PERFORM normalize_folder CHANGING p_dir.

  PERFORM collect_findings.
  IF gt_obj IS INITIAL.
    MESSAGE 'No matching ATC findings / sub-objects found' TYPE 'E'.
  ENDIF.

  DATA lv_total TYPE i.
  DATA lv_done  TYPE i.
  lv_total = lines( gt_obj ).

  LOOP AT gt_obj INTO DATA(gs_obj).
    lv_done = lv_done + 1.
    PERFORM progress USING lv_done lv_total gs_obj-sobjname.
    PERFORM download_subobject USING gs_obj.
  ENDLOOP.

  IF gt_manifest IS NOT INITIAL.
    PERFORM write_manifest.
  ENDIF.

  " Export the full ATC findings (all fields) as a separate Excel file.
  IF gt_findings_full IS NOT INITIAL.
    PERFORM write_atc_result.
  ENDIF.

  PERFORM show_log.

*&---------------------------------------------------------------------*
*& Form collect_findings
*&  Reads the ATC result and builds the unique list of sub-objects to
*&  download (object type / object name / sub-object name).
*&---------------------------------------------------------------------*
FORM collect_findings.

  DATA: i_result_id          TYPE satc_d_ac_display_id,
        e_findings_extension TYPE satc_ci_findings_extension,
        e_ext_field_list     TYPE satc_ci_finding_ext_field_list.

  " Resolve the latest display id for the given run series name.
  SELECT * INTO TABLE @DATA(it_resulth)
    FROM satc_ac_resulth
    WHERE run_series_name = @p_id.
  IF sy-subrc <> 0.
    MESSAGE 'Wrong ATC variant / result name selected' TYPE 'E'.
  ENDIF.
  SORT it_resulth BY update_on DESCENDING.
  READ TABLE it_resulth INTO DATA(wa_resulth) INDEX 1.
  IF sy-subrc = 0.
    i_result_id = wa_resulth-display_id.
  ENDIF.

  TRY.
      DATA(result_access) = NEW cl_satc_api_factory( )->create_result_access( i_result_id ).
      result_access->get_findings(
        IMPORTING
          e_findings           = DATA(findings)
          e_findings_extension = e_findings_extension
          e_ext_field_list     = e_ext_field_list ).
    CATCH cx_satc_failure INTO DATA(lx_satc).
      MESSAGE lx_satc->get_text( ) TYPE 'E'.
  ENDTRY.

  " Check titles / message texts (same source the correction program uses)
  " - needed to recognise the "Prerequisites for the test / Syntax error"
  "   pseudo-findings, which must NOT be downloaded.
  SELECT * INTO TABLE @DATA(it_chmmt) FROM satc_ac_chm_msgt_ddlv.
  SELECT * INTO TABLE @DATA(it_cmmmt) FROM satc_ac_cmm_msgt_ddlv.
  DATA wa_chmmt LIKE LINE OF it_chmmt.
  DATA wa_cmmmt LIKE LINE OF it_cmmmt.

  DATA gs_obj TYPE ty_obj.
  DATA lv_ok  TYPE abap_bool.
  LOOP AT findings INTO DATA(finding)
       WHERE objname IN s_obj OR enhname IN s_obj.

    " Skip pseudo-findings raised when the test could not run because the
    " object has a syntax error ("Prerequisites for the test"). There is
    " nothing to correct, so nothing to download. A sub-object that only
    " has such findings is therefore never collected; one that also has a
    " real finding is still downloaded via that finding.
    CLEAR: wa_chmmt, wa_cmmmt.
    READ TABLE it_chmmt INTO wa_chmmt WITH KEY ci_id = finding-test.
    IF sy-subrc = 0 AND wa_chmmt-title CS 'Prerequisites for the test'.
      CONTINUE.
    ENDIF.
    READ TABLE it_cmmmt INTO wa_cmmmt WITH KEY message_id = finding-code.
    IF sy-subrc = 0 AND wa_cmmmt-title CS 'Syntax error'.
      CONTINUE.
    ENDIF.

    CLEAR gs_obj.
    gs_obj-objtype  = finding-objtype.
    gs_obj-objname  = finding-objname.
    gs_obj-sobjname = finding-sobjname.
    IF gs_obj-sobjname IS INITIAL.
      gs_obj-sobjname = finding-objname.   " fall back to the object itself
    ENDIF.

    " Namespace filter: when namespaces / prefixes are entered, download
    " only sub-objects whose name matches one of them at the FIRST or the
    " SECOND character. The second-character check allows function-group
    " and module-pool includes whose generated prefix letter (e.g. 'L' or
    " 'M') pushes the namespace/customer prefix to position 2 - e.g.
    " 'MZMMCODU' belongs to prefix 'Z'. Excludes standard SAP code.
    IF s_nsp[] IS NOT INITIAL.
      PERFORM in_namespace USING gs_obj-sobjname CHANGING lv_ok.
      IF lv_ok = abap_false.
        CONTINUE.
      ENDIF.
    ENDIF.

    APPEND gs_obj TO gt_obj.
  ENDLOOP.

  " Keep one entry per unique sub-object (sub-objects come from the ATC findings).
  SORT gt_obj BY objname sobjname.
  DELETE ADJACENT DUPLICATES FROM gt_obj COMPARING objname sobjname.

  " Export ALL findings of the sub-objects that are actually downloaded:
  " every finding row (all fields - note, line, col, params, ...) whose
  " sub-object is in the downloaded set.
  LOOP AT findings INTO finding
       WHERE objname IN s_obj OR enhname IN s_obj.
    DATA(lv_sobj) = finding-sobjname.
    IF lv_sobj IS INITIAL.
      lv_sobj = finding-objname.
    ENDIF.
    READ TABLE gt_obj TRANSPORTING NO FIELDS
      WITH KEY objname = finding-objname sobjname = lv_sobj.
    IF sy-subrc = 0.
      APPEND finding TO gt_findings_full.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form in_namespace
*&  Returns 'X' if the object name matches one of the selected namespaces
*&  / prefixes (s_nsp) at the FIRST or the SECOND character. The second
*&  position covers function-group / module-pool includes whose generated
*&  prefix letter precedes the customer prefix (e.g. 'MZMMCODU' -> 'Z').
*&---------------------------------------------------------------------*
FORM in_namespace USING pv_name TYPE clike
                  CHANGING pv_ok TYPE abap_bool.
  DATA: lv_low TYPE string,
        lv_pat TYPE string,
        lv_2nd TYPE string.
  pv_ok = abap_false.
  lv_2nd = pv_name+1.
  LOOP AT s_nsp INTO DATA(ls_nsp).
    lv_low = ls_nsp-low.
    CONDENSE lv_low.
    IF lv_low IS INITIAL.
      CONTINUE.
    ENDIF.
    lv_pat = |{ lv_low }*|.
    IF pv_name CP lv_pat OR lv_2nd CP lv_pat.
      pv_ok = abap_true.
      RETURN.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form download_subobject
*&  Reads the source of one sub-object (per object type) and downloads
*&  it as a .txt file. Mirrors the read logic of ZATC_RESULT_CORRECTION.
*&---------------------------------------------------------------------*
FORM download_subobject USING ps_obj TYPE ty_obj.

  DATA: ls_log      TYPE ty_log,
        ls_manifest TYPE ty_manifest,
        lv_file     TYPE string,
        lv_fullpath TYPE string.

  ls_log-objtype  = ps_obj-objtype.
  ls_log-objname  = ps_obj-objname.
  ls_log-sobjname = ps_obj-sobjname.

  REFRESH repos_tab.
  PERFORM read_source USING ps_obj.

  IF repos_tab IS INITIAL.
    ls_log-status  = 'Skipped'.
    ls_log-message = 'No source could be read for this sub-object'.
    APPEND ls_log TO gt_log.
    RETURN.
  ENDIF.

  " File name from the sub-object (namespace '/' -> '#').
  lv_file = ps_obj-sobjname.
  REPLACE ALL OCCURRENCES OF '/' IN lv_file WITH '#'.
  CONCATENATE lv_file '.txt' INTO lv_file.
  CONCATENATE p_dir lv_file INTO lv_fullpath.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename = lv_fullpath
      filetype = 'ASC'
    TABLES
      data_tab = repos_tab
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc = 0.
    ls_log-filename = lv_file.
    ls_log-status   = 'Success'.
    ls_log-message  = 'Downloaded'.
    ls_manifest-objtype  = ps_obj-objtype.
    ls_manifest-objname  = ps_obj-objname.
    ls_manifest-sobjname = ps_obj-sobjname.
    ls_manifest-filename = lv_file.
    APPEND ls_manifest TO gt_manifest.
  ELSE.
    ls_log-status  = 'Error'.
    ls_log-message = 'GUI_DOWNLOAD failed'.
  ENDIF.
  APPEND ls_log TO gt_log.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form read_source  - fills REPOS_TAB for one sub-object
*&---------------------------------------------------------------------*
FORM read_source USING ps_obj TYPE ty_obj.

  CASE ps_obj-objtype.
    WHEN 'PROG' OR 'FUGR' OR 'FUGS' OR 'SFPF' OR 'SSFO'.
      " Program / function-group / form includes: read the include directly.
      object_name = ps_obj-sobjname.
      CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
        EXPORTING
          object_name           = object_name
          versno                = '00000'
        TABLES
          repos_tab             = repos_tab
        EXCEPTIONS
          no_version            = 1
          system_failure        = 2
          communication_failure = 3.

    WHEN 'CLAS'.
      " Class: resolve the method include and read its source.
      CLEAR l_seoclskey.
      l_seoclskey = ps_obj-objname.
      CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
        EXPORTING
          clskey                       = l_seoclskey
        IMPORTING
          includes                     = it_includes
        EXCEPTIONS
          _internal_class_not_existing = 1
          OTHERS                       = 2.
      IF sy-subrc = 0.
        READ TABLE it_includes INTO DATA(wa_includes)
          WITH KEY incname = ps_obj-sobjname.
        IF sy-subrc = 0.
          CLEAR ls_mtdkey.
          REFRESH ex_source_code_tab.
          ls_mtdkey-clsname = wa_includes-cpdkey-clsname.
          ls_mtdkey-cpdname = wa_includes-cpdkey-cpdname.
          CALL FUNCTION 'SEO_METHOD_GET_SOURCE'
            EXPORTING
              mtdkey                        = ls_mtdkey
            IMPORTING
              source                        = lt_source
              source_expanded               = ex_source_code_tab
            EXCEPTIONS
              _internal_method_not_existing = 1
              _internal_class_not_existing  = 2
              version_not_existing          = 3
              inactive_new                  = 4
              inactive_deleted              = 5
              OTHERS                        = 6.
          IF sy-subrc = 0.
            LOOP AT ex_source_code_tab INTO DATA(ls_source).
              wa_blank-line = ls_source.
              APPEND wa_blank TO repos_tab.
              CLEAR wa_blank.
            ENDLOOP.
          ENDIF.
        ELSE.
          " Not a method include -> try the class sections (public/protected/private).
          DATA l_limu TYPE c LENGTH 4.
          DO 3 TIMES.
            CASE sy-index.
              WHEN 1. l_limu = 'CPUB'.
              WHEN 2. l_limu = 'CPRO'.
              WHEN 3. l_limu = 'CPRI'.
            ENDCASE.
            l_clkey-clsname = ps_obj-objname.
            CALL FUNCTION 'SEO_SECTION_GET_SOURCE'
              EXPORTING
                cifkey               = l_clkey
                limu                 = l_limu
                state                = 'A'
              IMPORTING
                source               = lt_source_seo
                incname              = l_program_sec
              EXCEPTIONS
                class_not_existing   = 1
                version_not_existing = 2
                OTHERS               = 3.
            IF lt_source_seo[] IS NOT INITIAL AND l_program_sec = ps_obj-sobjname.
              LOOP AT lt_source_seo INTO DATA(ls_seo).
                wa_blank-line = ls_seo.
                APPEND wa_blank TO repos_tab.
                CLEAR wa_blank.
              ENDLOOP.
              EXIT.
            ENDIF.
          ENDDO.
        ENDIF.
      ENDIF.

    WHEN OTHERS.
      " Fallback: try reading the sub-object as a plain include/report.
      object_name = ps_obj-sobjname.
      CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
        EXPORTING
          object_name           = object_name
          versno                = '00000'
        TABLES
          repos_tab             = repos_tab
        EXCEPTIONS
          no_version            = 1
          system_failure        = 2
          communication_failure = 3.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form write_manifest  (standard FM SAP_CONVERT_TO_XLS_FORMAT)
*&---------------------------------------------------------------------*
FORM write_manifest.

  TYPES: BEGIN OF ty_flat,
           objtype  TYPE c LENGTH 12,
           objname  TYPE c LENGTH 40,
           sobjname TYPE c LENGTH 40,
           filename TYPE c LENGTH 128,
         END OF ty_flat.

  DATA: lt_flat  TYPE STANDARD TABLE OF ty_flat,
        ls_flat  TYPE ty_flat,
        lv_file  TYPE string,
        lv_full  TYPE rlgrap-filename,   " FM I_FILENAME needs fixed-length char
        lv_stamp TYPE char14.

  " Header row (skipped on upload via i_line_header = 'X').
  ls_flat-objtype  = 'Object Type'.
  ls_flat-objname  = 'Object Name'.
  ls_flat-sobjname = 'Sub Object'.
  ls_flat-filename = 'File Name'.
  APPEND ls_flat TO lt_flat.

  LOOP AT gt_manifest INTO DATA(ls_man).
    CLEAR ls_flat.
    ls_flat-objtype  = ls_man-objtype.
    ls_flat-objname  = ls_man-objname.
    ls_flat-sobjname = ls_man-sobjname.
    ls_flat-filename = ls_man-filename.
    APPEND ls_flat TO lt_flat.
  ENDLOOP.

  CONCATENATE sy-datum sy-uzeit INTO lv_stamp.
  CONCATENATE 'ZATC_DOWNLOAD_MANIFEST_' lv_stamp '.xls' INTO lv_file.
  CONCATENATE p_dir lv_file INTO lv_full.

  CALL FUNCTION 'SAP_CONVERT_TO_XLS_FORMAT'
    EXPORTING
      i_filename        = lv_full
    TABLES
      i_tab_sap_data    = lt_flat
    EXCEPTIONS
      conversion_failed = 1
      OTHERS            = 2.
  IF sy-subrc = 0.
    MESSAGE |Manifest written: { lv_file }| TYPE 'S'.
  ELSE.
    MESSAGE 'Manifest download failed' TYPE 'I'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form write_atc_result
*&  Downloads the COMPLETE ATC findings table (every field captured by
*&  the result access - priority/kind, note, line, col, params, check
*&  test/code, object/sub-object, ...). The raw findings structure is too
*&  complex for SAP_CONVERT_TO_XLS_FORMAT (it crashes the GUI), so the
*&  rows are flattened generically (RTTI) into a tab-delimited file with a
*&  header row and written with the robust standard FM GUI_DOWNLOAD. The
*&  .xls file opens directly in Excel as columns.
*&---------------------------------------------------------------------*
FORM write_atc_result.

  DATA: lt_out   TYPE STANDARD TABLE OF string,
        lv_line  TYPE string,
        lv_val   TYPE string,
        lv_file  TYPE string,
        lv_full  TYPE string,
        lv_stamp TYPE char14,
        lv_tab   TYPE c LENGTH 1,
        lv_idx   TYPE i.
  DATA: lo_struct TYPE REF TO cl_abap_structdescr,
        ls_find   LIKE LINE OF gt_findings_full.
  FIELD-SYMBOLS: <row> TYPE any,
                 <val> TYPE any.

  lv_tab    = cl_abap_char_utilities=>horizontal_tab.
  lo_struct ?= cl_abap_typedescr=>describe_by_data( ls_find ).

  " Header row: field names of the findings structure.
  CLEAR lv_line.
  LOOP AT lo_struct->components INTO DATA(ls_comp).
    IF sy-tabix > 1.
      CONCATENATE lv_line lv_tab INTO lv_line.
    ENDIF.
    CONCATENATE lv_line ls_comp-name INTO lv_line.
  ENDLOOP.
  APPEND lv_line TO lt_out.

  " Data rows: every component converted to character, tab-separated.
  LOOP AT gt_findings_full ASSIGNING <row>.
    CLEAR lv_line.
    LOOP AT lo_struct->components INTO ls_comp.
      lv_idx = sy-tabix.
      CLEAR lv_val.
      " Skip complex components (structure / table / reference / xstring).
      IF ls_comp-type_kind CA 'uvhrly'.
        CLEAR lv_val.
      ELSE.
        ASSIGN COMPONENT lv_idx OF STRUCTURE <row> TO <val>.
        IF sy-subrc = 0.
          lv_val = <val>.
        ENDIF.
      ENDIF.
      IF lv_idx > 1.
        CONCATENATE lv_line lv_tab INTO lv_line.
      ENDIF.
      CONCATENATE lv_line lv_val INTO lv_line.
    ENDLOOP.
    APPEND lv_line TO lt_out.
  ENDLOOP.

  CONCATENATE sy-datum sy-uzeit INTO lv_stamp.
  CONCATENATE 'ZATC_RESULT_' lv_stamp '.xls' INTO lv_file.
  CONCATENATE p_dir lv_file INTO lv_full.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename = lv_full
      filetype = 'ASC'
    TABLES
      data_tab = lt_out
    EXCEPTIONS
      OTHERS   = 1.
  IF sy-subrc = 0.
    MESSAGE |ATC result written: { lv_file }| TYPE 'S'.
  ELSE.
    MESSAGE 'ATC result download failed' TYPE 'I'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f4_folder
*&---------------------------------------------------------------------*
FORM f4_folder CHANGING pv_dir TYPE string.
  DATA lv_folder TYPE string.
  cl_gui_frontend_services=>directory_browse(
    EXPORTING
      window_title    = 'Select target folder'
    CHANGING
      selected_folder = lv_folder
    EXCEPTIONS
      OTHERS          = 1 ).
  IF lv_folder IS NOT INITIAL.
    pv_dir = lv_folder.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form normalize_folder  - ensure a trailing path separator
*&---------------------------------------------------------------------*
FORM normalize_folder CHANGING pv_dir TYPE string.
  DATA lv_len TYPE i.
  lv_len = strlen( pv_dir ).
  IF lv_len > 0
     AND substring( val = pv_dir off = lv_len - 1 len = 1 ) <> gv_sep.
    CONCATENATE pv_dir gv_sep INTO pv_dir.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form run_in_gui
*&---------------------------------------------------------------------*
FORM run_in_gui.
  DATA l_url TYPE string.
  CALL FUNCTION 'WEBGUI_GET_FLP_URL'
    IMPORTING
      url = l_url.
  IF l_url IS NOT INITIAL.
    MESSAGE 'Program can only be executed in SAP GUI' TYPE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form progress
*&---------------------------------------------------------------------*
FORM progress USING pv_done TYPE i pv_total TYPE i pv_text TYPE clike.
  DATA(lv_pct) = COND i( WHEN pv_total > 0 THEN pv_done * 100 / pv_total ELSE 0 ).
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = lv_pct
      text       = |Downloading { pv_text } ({ pv_done }/{ pv_total })|.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form show_log  - ALV summary
*&---------------------------------------------------------------------*
FORM show_log.
  DATA lo_alv TYPE REF TO cl_salv_table.
  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_alv
        CHANGING  t_table      = gt_log ).
      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->get_columns( )->set_optimize( abap_true ).
      lo_alv->display( ).
    CATCH cx_salv_msg INTO DATA(lx).
      MESSAGE lx->get_text( ) TYPE 'I'.
  ENDTRY.
ENDFORM.
