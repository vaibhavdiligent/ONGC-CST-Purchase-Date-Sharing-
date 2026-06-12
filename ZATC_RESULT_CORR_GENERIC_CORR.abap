REPORT zatc_result_corr_generic_corr.
TYPE-POOLS seop.
TABLES : tadir,SCIREST_AD.
TYPES: ty_swastrtab TYPE TABLE OF swastrtab.
DATA : lt_recording_entries TYPE cts_recording_entries,
       ls_recording_entry   TYPE cts_recording_entry.
DATA object_name TYPE vrsd-objname.
DATA repos_tab TYPE STANDARD TABLE OF abaptxt255.
DATA repos_tab_new TYPE STANDARD TABLE OF abaptxt255.
DATA l_tabix TYPE sy-tabix.
DATA l_tabix1 TYPE sy-tabix.
DATA l_tab TYPE sy-tabix.
DATA l_for TYPE flag.
DATA l_alv TYPE char50.
DATA l_seoclskey TYPE seoclskey.
DATA it_includes TYPE seop_methods_w_include.
DATA ls_mtdkey TYPE seocpdkey.
DATA : lt_source          TYPE seop_source,
       lt_source_expanded TYPE seop_source_string,
       ex_source_code_tab TYPE seop_source_string,
       lt_source_seo      TYPE seo_section_source,
       l_program_sec      TYPE program,
       l_clkey            TYPE seoclskey.
DATA l_datum TYPE char10.
DATA l_note TYPE char40.
DATA it_error_table TYPE syn_error.
DATA l_repid(5) TYPE n.
TYPES: BEGIN OF ty_final ,
         priority      TYPE char3,
         check_title   TYPE string,
         check_message TYPE string,
         objtype       TYPE char4,
         objname       TYPE char40,
         line(6)       TYPE n,
         program_name  TYPE char40,
         sobjname      TYPE char40,
         param1        TYPE char50,
         param2        TYPE char50,
         param3        TYPE char50,
         param4        TYPE char50,
         message       TYPE string,
         message1      TYPE string,
         note          TYPE char20,
         note_corr     TYPE char40,
         enhname       TYPE enhname,
       END OF ty_final.
DATA : it_final TYPE TABLE OF ty_final,
       wa_final TYPE ty_final.
TYPES : BEGIN OF ty_output,
          program_name  TYPE char40,
          subobj        TYPE char40,
          check_title   TYPE char100,
          check_message TYPE char100,
          line          TYPE char6,
          new_program   TYPE char40,
          backup        TYPE char40,
          run_status    TYPE char15,
          status        TYPE char10,
        END OF ty_output.
DATA it_output TYPE TABLE OF ty_output.
DATA wa_output TYPE ty_output.
DATA: include   TYPE program,
      statement TYPE sychar80.
DATA g_error_table TYPE syn_error.
CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS start IMPORTING p_program       TYPE program
                                  p_incl_date     TYPE sy-datum
                                  p_keyword       TYPE trmsg_keyw
                                  p_msgnumber     TYPE trmsg_num3
                                  p_syntax_trace  TYPE abap_bool
                                  p_include_info  TYPE abap_bool
                                  p_rfc           TYPE abap_bool
                                  p_debug         TYPE abap_bool
                                  p_test1         TYPE abap_bool
                                  p_test2         TYPE abap_bool
                                  p_test3         TYPE abap_bool
                                  p_test4         TYPE abap_bool
                                  p_prehdr_mode   TYPE sychar01
                                  p_excl_includes TYPE bgrfc_cprog_range
                                  p_src_includes  TYPE bgrfc_cprog_range.
  PRIVATE SECTION.
    CLASS-METHODS write_error
      IMPORTING p_error TYPE cl_abap_error_analyze=>t_error.
    CLASS-METHODS write_source
      IMPORTING p_includes TYPE sreptab.
    CLASS-METHODS write_trace
      IMPORTING p_trctab TYPE syn_trctab.
ENDCLASS.
CLASS lcl_main IMPLEMENTATION.
  METHOD start.
    DATA: l_include_names TYPE scr_programs,
          l_includes       TYPE sreptab,
          l_trctab         TYPE syn_trctab,
          l_error          TYPE cl_abap_error_analyze=>t_error,
          l_exp            TYPE REF TO cx_abap_error_analyze,
          l_incl_dates     TYPE cl_abap_error_analyze=>t_dates,
          l_incl_date      LIKE LINE OF l_incl_dates.
    IF p_src_includes IS NOT INITIAL.
      SELECT name FROM trdir INTO TABLE l_include_names
        WHERE name IN p_src_includes ORDER BY name.
    ENDIF.
    IF p_incl_date IS NOT INITIAL.
      l_incl_date-low    = p_incl_date.
      l_incl_date-option = 'GE'.
      l_incl_date-sign   = 'I'.
      APPEND l_incl_date TO l_incl_dates.
    ENDIF.
    TRY.
      CATCH cx_abap_error_analyze INTO l_exp.
    ENDTRY.
  ENDMETHOD.
  METHOD write_error.
    WRITE  / 'Error Message:' COLOR COL_HEADING.
    WRITE: / 'KEYWORD = ', p_error-error-keyword,
           / 'MESSAGE = ', p_error-error-message,
           / 'INCLUDE = ', p_error-error-incname,
           / 'LINE    = ', p_error-error-line.
  ENDMETHOD.
  METHOD write_source.
    FIELD-SYMBOLS: <l_reptab> LIKE LINE OF p_includes,
                   <l_source> LIKE LINE OF <l_reptab>-source->*.
    LOOP AT p_includes ASSIGNING <l_reptab>.
      WRITE / <l_reptab>-name COLOR COL_GROUP.
      LOOP AT <l_reptab>-source->* ASSIGNING <l_source>.
        WRITE / <l_source>.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD write_trace.
    DATA: l_incl  TYPE sychar01,
          l_dummy TYPE string ##NEEDED,
          l_skip  TYPE abap_bool.
    SET BLANK LINES OFF.
    WRITE / 'Syntax Trace' COLOR COL_HEADING.
    LOOP AT p_trctab ASSIGNING FIELD-SYMBOL(<l_trcwa>).
      NEW-LINE.
      l_incl = ' '.
      CASE <l_trcwa>-cc(1).
        WHEN '#'. WRITE /.
        WHEN '$'. l_incl = 'X'. SPLIT <l_trcwa>-line AT ' ' INTO include l_dummy.
                  l_skip = abap_false.
        WHEN space. CHECK l_skip = abap_false.
        WHEN OTHERS. l_skip = abap_false.
      ENDCASE.
      IF l_incl = 'X'. FORMAT INTENSIFIED ON COLOR OFF.
      ELSE.            FORMAT INTENSIFIED OFF COLOR OFF.
      ENDIF.
      WRITE : / <l_trcwa>-cc, <l_trcwa>-ex, <l_trcwa>-line.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
PARAMETERS  p_id TYPE satc_d_ac_title.
SELECT-OPTIONS s_obj FOR tadir-obj_name.
SELECT-OPTIONS s_name FOR SCIREST_AD-sobjname OBLIGATORY.
PARAMETERS p_rem TYPE char50.
PARAMETERS lv_req TYPE trkorr OBLIGATORY.
PARAMETERS p_begin TYPE char50 DEFAULT '**begin of change by'.
PARAMETERS p_end TYPE char50 DEFAULT '* *End of change by'.
PARAMETERS p_sim TYPE flag AS CHECKBOX DEFAULT 'X'.
INITIALIZATION.
  DATA: lv_hostname TYPE string,
        lv_port     TYPE string,
        lv_protocol TYPE string.
  DATA l_url TYPE string.
  DATA l_find TYPE i.
  DATA l_find1 TYPE i.
  DATA l_find2 TYPE char30.
  CALL FUNCTION 'WEBGUI_GET_FLP_URL'
    IMPORTING
      url = l_url.
  IF l_url IS NOT INITIAL.
    MESSAGE 'Program Cannot be Executed outside SAP GUI' TYPE 'E'.
  ENDIF.
  CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.'
  sy-datum(4) INTO l_datum.
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
        stepl           = 0
        value_org       = 'S'
      TABLES
        value_tab       = it_run_series_name
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
    ENDIF.
  ENDIF.
START-OF-SELECTION.
  DATA: lv_memento TYPE xstring.
  DATA i_result_id        TYPE satc_d_ac_display_id.
  DATA : e_findings            TYPE scit_rest,
         e_findings_extension TYPE satc_ci_findings_extension,
         e_ext_field_list     TYPE satc_ci_finding_ext_field_list.
  SELECT * INTO TABLE @DATA(it_SATC_AC_RESULTH)
    FROM SATC_AC_RESULTH
    WHERE RUN_SERIES_NAME = @P_ID.
  IF sy-subrc = 0.
    SORT it_SATC_AC_RESULTH BY UPDATE_ON DESCENDING.
    READ TABLE it_SATC_AC_RESULTH INTO DATA(wa_SATC_AC_RESULTH) INDEX 1.
    IF sy-subrc = 0.
      i_result_id = wa_SATC_AC_RESULTH-display_id.
    ENDIF.
  ELSE.
    MESSAGE 'WRONG ATC VARIANT SELECTED' TYPE 'E'.
  ENDIF.
  SELECT SINGLE * INTO @DATA(l_e070)
    FROM e070
    WHERE trkorr = @lv_req.
  IF sy-subrc <> 0.
    MESSAGE 'Wrong Transport request selected' TYPE 'E'.
  ENDIF.
  IF l_e070-trfunction = 'T' OR l_e070-trfunction = 'G' OR l_e070-trfunction = 'R'.
    MESSAGE 'Please select a Workbench transport request, not a Customizing transport' TYPE 'E'.
  ENDIF.
  DATA(result_access) = NEW cl_satc_api_factory( )->create_result_access( i_result_id ).
  result_access->get_findings( IMPORTING e_findings           = DATA(findings)
                                         e_findings_extension = e_findings_extension
                                         e_ext_field_list     = e_ext_field_list ).
  DATA test TYPE REF TO cl_ci_test_root.
  SELECT * INTO TABLE @DATA(it_satc_ac_chmmt_dv)
    FROM satc_ac_chm_msgt_ddlv.
  SELECT * INTO TABLE @DATA(it_satc_ac_cmmmt_dv)
    FROM satc_ac_cmm_msgt_ddlv.
  LOOP AT findings INTO DATA(finding) WHERE objname IN s_obj OR enhname IN s_obj.
    READ TABLE e_findings_extension INTO DATA(finding_ext) INDEX sy-tabix.
    IF sy-subrc = 0.
      LOOP AT finding_ext-description_lines INTO DATA(wa_desc_note).
        CLEAR: l_find, l_find1.
        IF wa_desc_note CS '#EC'.
          l_find1 = sy-fdpos.
          l_find = strlen( wa_desc_note ).
          l_find = l_find - l_find1.
          wa_final-note_corr = wa_desc_note+l_find1(l_find).
        ENDIF.
      ENDLOOP.
    ENDIF.
    CLEAR l_repid.
    CREATE OBJECT test TYPE (finding-test).
    DATA(message) = test->scimessages[ test = finding-test code = finding-code ]-text.
    wa_final-message1 = message.
    wa_final-message1 = replace( val = wa_final-message1 sub = '&1' with = space ).
    wa_final-message1 = replace( val = wa_final-message1 sub = '&2' with = space ).
    wa_final-message1 = replace( val = wa_final-message1 sub = '&3' with = space ).
    wa_final-message1 = replace( val = wa_final-message1 sub = '&4' with = space ).
    REPLACE ALL OCCURRENCES OF '(' IN wa_final-message1 WITH space IGNORING CASE.
    REPLACE ALL OCCURRENCES OF ')' IN wa_final-message1 WITH space IGNORING CASE.
    REPLACE ALL OCCURRENCES OF ', see' IN wa_final-message1 WITH space IGNORING CASE.
    CONDENSE wa_final-message1.
    message = replace( val = message sub = '&1' with = finding-param1 ).
    message = replace( val = message sub = '&2' with = finding-param2 ).
    message = replace( val = message sub = '&3' with = finding-param3 ).
    message = replace( val = message sub = '&4' with = finding-param4 ).
    CASE finding-kind.
      WHEN 'E'. wa_final-priority = '1'.
      WHEN 'W'. wa_final-priority = '2'.
      WHEN 'N'. wa_final-priority = '3'.
    ENDCASE.
    READ TABLE it_satc_ac_chmmt_dv INTO DATA(wa_satc_ac_chmmt_dv)
      WITH KEY ci_id = finding-test.
    IF sy-subrc = 0.
      wa_final-check_title = wa_satc_ac_chmmt_dv-title.
    ENDIF.
    READ TABLE it_satc_ac_cmmmt_dv INTO DATA(wa_satc_ac_cmmmt_dv)
      WITH KEY message_id = finding-code.
    IF sy-subrc = 0.
      wa_final-check_message = wa_satc_ac_cmmmt_dv-title.
    ENDIF.
    wa_final-objtype      = finding-objtype.
    wa_final-objname      = finding-objname.
    wa_final-line         = finding-line.
    wa_final-program_name = finding-program_name.
    wa_final-param2       = finding-param2.
    wa_final-param3       = finding-param3.
    wa_final-param4       = finding-param4.
    wa_final-param1       = finding-param1.
    wa_final-message      = message.
    wa_final-sobjname     = finding-sobjname.
    wa_final-enhname      = finding-enhname.
    IF wa_final-note IS INITIAL AND finding-param1 IS NOT INITIAL.
      DATA lv_p1 TYPE string.
      DATA lv_note_extracted TYPE string.
      lv_p1 = finding-param1.
      CLEAR lv_note_extracted.
      DO strlen( lv_p1 ) TIMES.
        DATA(lv_idx) = sy-index - 1.
        DATA(lv_char) = lv_p1+lv_idx(1).
        IF lv_char CA '0123456789'.
          CONCATENATE lv_note_extracted lv_char INTO lv_note_extracted.
        ENDIF.
      ENDDO.
      lv_note_extracted = condense( val = lv_note_extracted ).
      IF lv_note_extracted IS NOT INITIAL.
        wa_final-note = lv_note_extracted.
      ENDIF.
    ENDIF.
    APPEND wa_final TO it_final.
  ENDLOOP.
  DATA l_text TYPE char255.
  DATA l_table TYPE char50.
  DATA l_where TYPE char100.
  DATA l_v TYPE i.
  DATA l_new TYPE char255.
  DATA wa_blank TYPE abaptxt255.
  SORT it_final BY priority check_title check_message program_name sobjname line.
  DELETE it_final WHERE priority = '1'.
  DATA(it_final_p) = it_final.
  SORT it_final_p BY program_name sobjname.
  DELETE ADJACENT DUPLICATES FROM it_final_p COMPARING program_name sobjname.
  SORT it_final BY program_name sobjname line ASCENDING.
  DELETE ADJACENT DUPLICATES FROM it_final COMPARING line objname sobjname.
  REFRESH it_output.
  CLEAR l_repid.
  DATA lv_total_objects TYPE i.
  DESCRIBE TABLE it_final_p LINES lv_total_objects.
  LOOP AT it_final_p INTO DATA(wa_final_p)
     WHERE sobjname IN s_name OR objname IN s_name.
    l_repid = l_repid + 1.
    DATA(lv_pct) = CONV i( l_repid * 100 / lv_total_objects ).
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_pct
        text       = wa_final_p-sobjname.
    CLEAR wa_output.
    CLEAR l_tab.
    REFRESH repos_tab.
    REFRESH repos_tab_new.
    object_name = wa_final_p-sobjname.
    CASE wa_final_p-objtype.
      WHEN 'PROG' OR 'FUGR' OR 'FUGS'.
        object_name = wa_final_p-sobjname.
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
        object_name = wa_final_p-objname.
        CLEAR l_seoclskey.
        l_seoclskey = wa_final_p-objname.
        CALL FUNCTION 'SEO_CLASS_GET_METHOD_INCLUDES'
          EXPORTING
            clskey                       = l_seoclskey
          IMPORTING
            includes                     = it_includes
          EXCEPTIONS
            _internal_class_not_existing = 1
            OTHERS                       = 2.
        IF sy-subrc = 0.
          READ TABLE it_includes INTO DATA(wa_includes) WITH KEY
            incname = wa_final_p-sobjname.
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
            DO 3 TIMES.
              CASE sy-index.
                WHEN '1'. DATA(l_limu) = 'CPUB'.
                WHEN '2'. l_limu = 'CPRO'.
                WHEN '3'. l_limu = 'CPRI'.
              ENDCASE.
              l_clkey-clsname = wa_final_p-objname.
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
              IF lt_source_seo[] IS NOT INITIAL AND l_program_sec = wa_final_p-sobjname.
                wa_includes-incname = l_program_sec.
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
    ENDCASE.
    IF repos_tab[] IS NOT INITIAL.
      LOOP AT repos_tab INTO DATA(wa_repos_tab).
        l_tabix = sy-tabix.
        IF l_tab IS NOT INITIAL.
          IF l_tabix < l_tab.
            CONTINUE.
          ELSEIF l_tabix = l_tab.
            CLEAR l_tab.
          ENDIF.
        ENDIF.
        READ TABLE it_final INTO wa_final WITH KEY
          program_name = wa_final_p-program_name
          sobjname     = wa_final_p-sobjname
          line         = l_tabix.
        IF sy-subrc <> 0.
          APPEND wa_repos_tab TO repos_tab_new.
        ELSE.
          TRANSLATE wa_final-check_message TO UPPER CASE.
          TRANSLATE wa_final-message1 TO UPPER CASE.
          DATA(lv_msg) = wa_final-check_message.
          IF lv_msg IS INITIAL.
            lv_msg = wa_final-message1.
          ENDIF.
          IF wa_final-priority <> '2' AND wa_final-priority <> '3'.
            APPEND wa_repos_tab TO repos_tab_new.
            CONTINUE.
          ENDIF.
          DATA(lv_is_target) = abap_false.
          DATA(lv_title_match) = abap_false.
          PERFORM check_target_title USING wa_final-check_title CHANGING lv_title_match.
          IF lv_title_match = abap_true.
            PERFORM check_target_msg USING lv_msg CHANGING lv_is_target.
            IF lv_is_target = abap_false.
              PERFORM check_target_msg USING wa_final-message1 CHANGING lv_is_target.
            ENDIF.
          ENDIF.
          IF lv_is_target = abap_false.
            APPEND wa_repos_tab TO repos_tab_new.
            CONTINUE.
          ENDIF.
          CLEAR : l_find , l_find1.
          IF ( wa_repos_tab CS '"#EC CI_USAGE_OK[' OR wa_repos_tab CS '"#EC CI_FLDEXT_OK[' ) AND wa_repos_tab(1) <> '*'.
            l_find = sy-fdpos.
            l_find1 = strlen( wa_repos_tab-line ).
            l_find1 = l_find1 - l_find.
            l_find2 = wa_repos_tab-line+l_find(l_find1).
            REPLACE ALL OCCURRENCES OF '"#EC CI_USAGE_OK[' IN l_find2 WITH space.
            REPLACE ALL OCCURRENCES OF '"#EC CI_FLDEXT_OK[' IN l_find2 WITH space.
            REPLACE ALL OCCURRENCES OF ']' IN l_find2 WITH space.
            CONDENSE l_find2.
            READ TABLE it_final INTO wa_final WITH KEY note = l_find2.
            IF sy-subrc = 0.
              CONCATENATE wa_repos_tab(l_find) text-001 wa_final-note_corr INTO wa_repos_tab
                SEPARATED BY space.
              APPEND wa_repos_tab TO repos_tab_new.
            ELSE.
              APPEND wa_repos_tab TO repos_tab_new.
            ENDIF.
          ELSE.
            CONCATENATE wa_repos_tab-line text-001 wa_final-note_corr INTO wa_repos_tab-line SEPARATED BY space.
            APPEND wa_repos_tab TO repos_tab_new.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF wa_output-program_name IS INITIAL AND wa_output-check_title IS INITIAL.
      wa_output-program_name = wa_final_p-objname.
      wa_output-subobj       = wa_final_p-sobjname.
      wa_output-run_status   = 'Not processed'.
      APPEND wa_output TO it_output.
      CLEAR wa_output.
    ELSEIF repos_tab_new[] IS INITIAL.
      wa_output-run_status = 'Processed'.
      wa_output-status     = 'No change'.
      APPEND wa_output TO it_output.
      CLEAR wa_output.
    ENDIF.
    IF repos_tab_new[] IS NOT INITIAL.
      IF wa_final_p-enhname IS INITIAL.
        CASE wa_final_p-objtype.
          WHEN 'PROG' OR 'FUGR' OR 'FUGS'.
            SELECT SINGLE * INTO @DATA(l_trdir)
              FROM trdir WHERE name = @wa_final_p-sobjname.
            wa_output-program_name = wa_final_p-objname.
            wa_output-subobj = wa_final_p-sobjname.
            IF p_sim = 'X'.
              CLEAR wa_final_p-sobjname.
              CONCATENATE 'ZTEST_CHECK' l_repid '_' sy-uname INTO wa_final_p-sobjname.
              INSERT REPORT wa_final_p-sobjname FROM repos_tab_new.
              REFRESH repos_tab_new.
              COMMIT WORK AND WAIT.
            ELSE.
              DATA lv_prog_name TYPE programm.
              lv_prog_name = wa_final_p-sobjname.
              CALL FUNCTION 'RPY_PROGRAM_UPDATE'
                EXPORTING
                  program_name     = lv_prog_name
                  program_type     = l_trdir-subc
                  transport_number = lv_req
                TABLES
                  source_extended  = repos_tab_new
                EXCEPTIONS
                  cancelled        = 1
                  permission_error = 2
                  not_found        = 3
                  OTHERS           = 4.
              IF sy-subrc = 0.
                COMMIT WORK AND WAIT.
                CLEAR wa_output-backup.
                CONCATENATE 'ZTEST_CHECK' l_repid '_' sy-uname INTO wa_output-backup.
                INSERT REPORT wa_output-backup FROM repos_tab.
                REFRESH repos_tab.
                COMMIT WORK.
              ENDIF.
            ENDIF.
            REFRESH repos_tab_new.
            wa_output-new_program = wa_final_p-sobjname.
            CLEAR it_error_table.
            PERFORM syntax_check USING wa_final_p-objname wa_final_p-objtype
                                 CHANGING it_error_table.
            wa_output-run_status = 'Processed'.
            IF it_error_table IS INITIAL.
              wa_output-status = 'Success'.
            ELSE.
              wa_output-status = 'Syn.error'.
            ENDIF.
            APPEND wa_output TO it_output.
            CLEAR wa_output.
          WHEN 'CLAS'.
            SELECT SINGLE * INTO @DATA(wa_tadir)
              FROM tadir WHERE obj_name = @object_name.
            wa_output-program_name = wa_final_p-objname.
            wa_output-subobj = wa_includes-incname.
            IF p_sim = 'X'.
              CLEAR wa_includes-incname.
              CONCATENATE 'ZTEST_CHECK' l_repid '_' sy-uname INTO wa_includes-incname.
            ELSE.
              CLEAR wa_output-backup.
              CONCATENATE 'ZTEST_CHECK' l_repid '_' sy-uname INTO wa_output-backup.
              INSERT REPORT wa_output-backup FROM repos_tab.
              COMMIT WORK.
              REFRESH repos_tab.
            ENDIF.
            INSERT REPORT wa_includes-incname FROM repos_tab_new.
            REFRESH repos_tab_new.
            REFRESH lt_recording_entries.
            ls_recording_entry-object_entry-object_key-pgmid    = 'R3TR'.
            ls_recording_entry-object_entry-object_key-object   = wa_tadir-object.
            ls_recording_entry-object_entry-object_key-obj_name = object_name.
            ls_recording_entry-author      = wa_tadir-author.
            ls_recording_entry-devclass    = wa_tadir-devclass.
            ls_recording_entry-masterlang  = wa_tadir-masterlang.
            APPEND ls_recording_entry TO lt_recording_entries.
            CALL FUNCTION 'CTS_WBO_API_INSERT_OBJECTS'
              EXPORTING
                recording_entries = lt_recording_entries
                trkorr            = lv_req.
            COMMIT WORK.
            wa_output-new_program = wa_includes-incname.
            PERFORM syntax_check USING wa_final_p-objname wa_final_p-objtype
                                 CHANGING it_error_table.
            wa_output-run_status = 'Processed'.
            IF it_error_table IS INITIAL.
              wa_output-status = 'Success'.
            ELSE.
              wa_output-status = 'Syn.error'.
            ENDIF.
            APPEND wa_output TO it_output.
            CLEAR wa_output.
        ENDCASE.
      ELSE.
        DATA l_enh_tool  TYPE REF TO if_enh_tool.
        DATA l_enh       TYPE enhname.
        DATA l_enh_tool_hook TYPE REF TO cl_enh_tool_hook_impl.
        l_enh = wa_final_p-enhname.
        CALL METHOD cl_enh_factory=>get_enhancement
          EXPORTING
            lock           = 'X'
            enhancement_id = l_enh
          RECEIVING
            enhancement    = l_enh_tool.
        l_enh_tool_hook ?= l_enh_tool.
        DATA it_impl TYPE enh_hook_impl_it.
        CALL METHOD l_enh_tool_hook->get_hook_impls
          RECEIVING enhancements = it_impl.
        READ TABLE it_impl INTO DATA(wa_impl) INDEX 1.
        DATA it_source TYPE rswsourcet.
        DATA wa_source TYPE string.
        LOOP AT repos_tab_new INTO DATA(wa_repos).
          IF wa_repos-line CS 'ENHANCEMENT ' OR wa_repos-line CS 'ENDENHANCEMENT'.
          ELSE.
            wa_source = wa_repos-line.
            APPEND wa_source TO it_source.
          ENDIF.
        ENDLOOP.
        CALL METHOD l_enh_tool_hook->if_enh_object~set_locked
          EXPORTING is_locked = 'X'.
        CALL METHOD l_enh_tool_hook->modify_hook_impl
          EXPORTING
            overwrite        = ' '
            method           = wa_impl-method
            enhmode          = wa_impl-enhmode
            full_name        = wa_impl-parent_full_name
            source           = it_source
            extid            = wa_impl-extid
            spot             = wa_impl-spotname
            parent_full_name = wa_impl-parent_full_name.
        CALL METHOD l_enh_tool_hook->if_enh_object~save
          EXPORTING run_dark = 'X'
          CHANGING  trkorr   = lv_req.
        CALL METHOD l_enh_tool_hook->if_enh_object~activate
          EXPORTING run_dark = 'X'
          CHANGING  trkorr   = lv_req.
        l_enh_tool_hook->if_enh_object~unlock( ).
        COMMIT WORK AND WAIT.
        REFRESH repos_tab_new.
        wa_output-new_program = wa_final_p-enhname.
        CLEAR it_error_table.
        wa_output-run_status = 'Processed'.
        IF it_error_table IS INITIAL.
          wa_output-status = 'Success'.
        ELSE.
          wa_output-status = 'Syn.error'.
        ENDIF.
        APPEND wa_output TO it_output.
        CLEAR wa_output.
      ENDIF.
    ENDIF.
    REFRESH repos_tab_new.
    REFRESH repos_tab.
  ENDLOOP.
  cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_table)
                          CHANGING  t_table      = it_output ).
  lo_table->get_columns( )->get_column( columnname = 'PROGRAM_NAME'  )->set_long_text( 'Main Program Name' ).
  lo_table->get_columns( )->get_column( columnname = 'SUBOBJ'        )->set_long_text( 'Sub Object Name' ).
  lo_table->get_columns( )->get_column( columnname = 'CHECK_TITLE'   )->set_long_text( 'Check Title' ).
  lo_table->get_columns( )->get_column( columnname = 'CHECK_MESSAGE' )->set_long_text( 'Check Message' ).
  lo_table->get_columns( )->get_column( columnname = 'LINE'          )->set_long_text( 'Line No' ).
  lo_table->get_columns( )->get_column( columnname = 'NEW_PROGRAM'   )->set_long_text( 'New Program Name' ).
  lo_table->get_columns( )->get_column( columnname = 'BACKUP'        )->set_long_text( 'Back Up Program Name' ).
  lo_table->get_columns( )->get_column( columnname = 'RUN_STATUS'    )->set_long_text( 'Run Status' ).
  lo_table->get_columns( )->get_column( columnname = 'STATUS'        )->set_long_text( 'Status' ).
  lo_table->display( ).

FORM check_target_msg
  USING    iv_msg     TYPE string
  CHANGING cv_match   TYPE abap_bool.

  DATA lv_up TYPE string.
  lv_up = iv_msg.
  TRANSLATE lv_up TO UPPER CASE.

  IF lv_up CS 'NON-STRATEGIC-FUNCTION'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'ARITHMETIC OPERATION'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'OLD ARITHMETIC TYPE CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'COMPARE LENGTH CONFLICT' AND NOT lv_up CS 'OLD COMPARE LENGTH CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'OLD COMPARE LENGTH CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'OLD COMPARE TYPE CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'COMPARE <-> GENERIC'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'CONCATENATE LENGTH CONFLICT' AND NOT lv_up CS 'OLD CONCATENATE LENGTH CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'OLD CONCATENATE LENGTH CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'CONCATENATION DETECTED'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'IS-INITIAL-CHECK FOR TYPE'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'IS-INITIAL-CHECK FOR COMPONENT' AND NOT lv_up CS 'CLEARED FIELD'
                                               AND NOT lv_up CS 'OPTIONAL PARAMETER'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'IS-INITIAL-CHECK FOR COMPONENT' AND lv_up CS 'CLEARED FIELD'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'IS-INITIAL-CHECK FOR COMPONENT' AND lv_up CS 'OPTIONAL PARAMETER'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'MOVE LENGTH CONFLICT' AND NOT lv_up CS 'OLD MOVE LENGTH CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'OLD MOVE LENGTH CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'OLD MOVE TYPE CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'MOVE -> GENERIC'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'MOVE GENERIC ->'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'STRUCTURE-COMPONENT LENGTH CONFLICT' AND NOT lv_up CS 'OLD STRUCTURE-COMPONENT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'OLD STRUCTURE-COMPONENT LENGTH CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'STRUCTURE-COMPONENT TYPE CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'SELECT TYPE CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'OLD SELECT TYPE CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'SET PARAMETER ISSUE'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'TYPE-CONFLICT' AND NOT lv_up CS 'OLD TYPE-CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'OLD TYPE-CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'TYPE USED BY RFC-FUNCTION PARAMETER'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'TYPE COMPONENT' AND lv_up CS 'RFC-FUNCTION PARAMETER'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'OFFSET/LENGTH-ACCESS'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'TRANSFER ISSUE'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'WRITE ISSUE' AND NOT lv_up CS 'OLD WRITE-LENGTH ISSUE'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'OLD WRITE-LENGTH ISSUE'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'ASSIGN COMPONENT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'ASSIGN GENERIC'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'DYNAMIC ASSIGN'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'DYNAMIC DB-ACCESS'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'CASTING FROM'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'CALL FUNCTION GENERIC PARAMETER'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'CALL FUNCTION GENERIC OPERAND'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'CALL METHOD GENERIC PARAMETER'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'CALL METHOD GENERIC OPERAND'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'CONSTANT COMPARE CONFLICT' AND NOT lv_up CS 'OLD CONSTANT COMPARE CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'OLD CONSTANT COMPARE CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'DESCRIBE FIELD ISSUE'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'EXPORT ISSUE'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'GENERIC SOURCE CODE ISSUE'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'GENERIC DESTINATION CODE ISSUE'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'IMPORT ISSUE'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'OLD MESSAGE-INTO LENGTH CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'NO IMPLEMENTATION FOR CURRENT STATEMENT'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'PERFORM GENERIC PARAMETER'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'PERFORM GENERIC OPERAND'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'OLD SPLIT LENGTH CONFLICT'.
    cv_match = abap_true. RETURN.
  ENDIF.

ENDFORM.

FORM check_target_title
  USING    iv_title   TYPE string
  CHANGING cv_match   TYPE abap_bool.

  DATA lv_up TYPE string.
  lv_up = iv_title.
  TRANSLATE lv_up TO UPPER CASE.

  IF lv_up CS 'FIELD LENGTH EXTENSIONS'.
    cv_match = abap_true. RETURN.
  ENDIF.
  IF lv_up CS 'SIMPLIFIED OBJECTS'.
    cv_match = abap_true. RETURN.
  ENDIF.

ENDFORM.

FORM syntax_check USING    program    TYPE program
                           objecttype TYPE trobjtype
                  CHANGING error_table TYPE syn_error.
  DATA: lv_classname TYPE char32,
        lv_msg       TYPE string,
        lv_line      TYPE i,
        lv_word      TYPE string,
        ls_errtbl    TYPE syn_error,
        ls_errstr    TYPE rslinlmsg.
  IF objecttype IS INITIAL AND program IS NOT INITIAL.
    SELECT SINGLE FROM tadir FIELDS object
      WHERE obj_name = @program INTO @objecttype.
  ENDIF.
  CASE objecttype.
    WHEN 'CLAS'.
      lv_classname = program.
      TRANSLATE lv_classname USING ' ='.
      lv_classname+30(2) = 'CP'.
      program = lv_classname.
    WHEN 'FUGR'.
      program = 'SAPL'&& program.
    WHEN 'ENHO'.
      SELECT SINGLE programname FROM enhincinx
        WHERE enhname = @program INTO @DATA(lv_enh_main_prog).
      IF sy-subrc = 0.
        TRANSLATE program USING ' ='.
        program+30 = 'E'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
  CASE objecttype.
    WHEN 'PROG' OR 'CLASS' OR 'FUGR'.
      CALL FUNCTION 'RS_ABAP_SYNTAX_CHECK_E'
        EXPORTING
          p_program  = program
          p_langu    = sy-langu
        IMPORTING
          p_errors   = ls_errtbl-errors
          p_warnings = ls_errtbl-warnings
          p_subrc    = ls_errtbl-subrc.
      IF ls_errtbl-subrc <> 0.
        IF ls_errtbl-errors IS NOT INITIAL.
          MOVE ls_errtbl TO error_table.
        ENDIF.
      ENDIF.
    WHEN 'ENHO'.
      CALL FUNCTION 'RS_ABAP_SYNTAX_CHECK_E'
        EXPORTING
          p_program  = lv_enh_main_prog
          p_langu    = sy-langu
        IMPORTING
          p_errors   = ls_errtbl-errors
          p_warnings = ls_errtbl-warnings
          p_subrc    = ls_errtbl-subrc.
      IF ls_errtbl-subrc <> 0.
        DELETE ls_errtbl-errors WHERE incname <> program.
        IF ls_errtbl-errors IS NOT INITIAL.
          MOVE ls_errtbl TO error_table.
        ENDIF.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
