*&---------------------------------------------------------------------*
*& Report ZATC_RESULT_CORR_GENERIC
*& Handles the following check messages (pseudo-code only):
*&   Non-strategic-function
*&   ASSIGN COMPONENT
*&   ASSIGN Generic -> Field-Symbol
*&   CALL FUNCTION Generic Operand
*&   CALL FUNCTION Generic Parameter
*&   CALL METHOD Generic Operand
*&   CALL METHOD Generic Parameter
*&   Casting from/to
*&   Compare <-> Generic
*&   Concatenation detected
*&   Constant Compare conflict
*&   DESCRIBE FIELD issue
*&   No implementation for current statement
*&---------------------------------------------------------------------*
REPORT zatc_result_corr_generic.
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
TYPES : BEGIN OF ty_zatc_process_all,
          mandt            TYPE mandt,
          sno              TYPE int4,
          priority         TYPE char3,
          description      TYPE   char255,
          check_title      TYPE   char255,
          note(10)         TYPE c,
          srch_tem         TYPE  char35,
          ref_obj_type     TYPE  char4,
          message          TYPE   char120,
          look             TYPE  char50,
          look2            TYPE   char50,
          zcomment         TYPE  char80,
          fix_by           TYPE  char120,
          solution         TYPE  char1,
          correction_value TYPE char30,
        END OF ty_zatc_process_all.
TYPES : BEGIN OF ty_zatc_process1,
          mandt            TYPE mandt,
          priority         TYPE char3,
          description      TYPE   char255,
          check_title      TYPE   char255,
          note(10)         TYPE c,
          srch_tem         TYPE  char35,
          ref_obj_type     TYPE  char4,
          message          TYPE   char120,
          look             TYPE  char50,
          look2            TYPE   char50,
          zcomment         TYPE  char80,
          fix_by           TYPE  char120,
          solution         TYPE  char1,
          correction_value TYPE char30,
        END OF ty_zatc_process1.
TYPES : BEGIN OF ty_zatc_process_dte,
          mandt            TYPE mandt,
          sno(8)           TYPE n,
          priority         TYPE char3,
          description      TYPE char255,
          check_title      TYPE char255,
          note(10)         TYPE c,
          srch_tem         TYPE char35,
          ref_obj_type     TYPE char4,
          message          TYPE char120,
          look             TYPE char50,
          look2            TYPE char50,
          zcomment         TYPE char80,
          fix_by           TYPE char120,
          solution         TYPE char1,
          correction_value TYPE char30,
        END OF ty_zatc_process_dte.
DATA it_zatc_process_all TYPE TABLE OF ty_zatc_process_all.
DATA it_zatc_process_dte TYPE TABLE OF ty_zatc_process_dte.
DATA it_zatc_process1 TYPE TABLE OF ty_zatc_process1.
DATA wa_zatc_process_all TYPE ty_zatc_process_all.
DATA wa_zatc_process_dte TYPE ty_zatc_process_dte.
DATA wa_zatc_process1 TYPE ty_zatc_process1.
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
         enhname       TYPE enhname,
       END OF ty_final.
DATA : it_final TYPE TABLE OF ty_final,
       wa_final TYPE ty_final.
TYPES : BEGIN OF ty_output,
          program_name TYPE char40,
          subobj       TYPE char40,
          new_program  TYPE char40,
          backup       TYPE char40,
          status       TYPE char10,
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
    APPEND wa_final TO it_final.
  ENDLOOP.
  DATA l_text TYPE char255.
  DATA l_table TYPE char50.
  DATA l_where TYPE char100.
  DATA l_v TYPE i.
  DATA l_new TYPE char255.
  DATA wa_blank TYPE abaptxt255.
  SORT it_final BY priority check_title check_message program_name sobjname line.
  DATA(it_final_p) = it_final.
  SORT it_final_p BY program_name sobjname.
  DELETE ADJACENT DUPLICATES FROM it_final_p COMPARING program_name sobjname.
  SORT it_final BY priority line ASCENDING.
  DELETE ADJACENT DUPLICATES FROM it_final COMPARING line objname sobjname.
  PERFORM zatc_process_all.
  PERFORM zatc_process_dte.
  PERFORM zatc_process1.
  REFRESH it_output.
  CLEAR l_repid.
  DATA lv_total_objects TYPE i.
  DESCRIBE TABLE it_final_p LINES lv_total_objects.
  LOOP AT it_final_p INTO DATA(wa_final_p)
     WHERE sobjname IN s_name.
    l_repid = l_repid + 1.
    DATA(lv_pct) = CONV i( l_repid * 100 / lv_total_objects ).
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_pct
        text       = wa_final_p-sobjname.
    REFRESH repos_tab.
    object_name = wa_final_p-sobjname.
    CASE wa_final_p-objtype.
      WHEN 'PROG' OR 'FUGR' OR 'FUGS' OR 'SFPF'.
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
      WHEN 'SSFO'.
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
        IF p_rem IS NOT INITIAL.
          IF wa_repos_tab-line CS p_rem.
            REFRESH repos_tab_new.
            EXIT.
          ENDIF.
        ENDIF.
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
          CLEAR wa_blank.
          TRANSLATE wa_final-check_message TO UPPER CASE.
          TRANSLATE wa_final-message1 TO UPPER CASE.

          " Combine check_message and message1 for matching
          DATA(lv_msg) = wa_final-check_message.
          IF lv_msg IS INITIAL.
            lv_msg = wa_final-message1.
          ENDIF.

          " Only process priority 2 and 3 findings.
          IF wa_final-priority <> '2' AND wa_final-priority <> '3'.
            APPEND wa_repos_tab TO repos_tab_new.
            CONTINUE.
          ENDIF.

          " Both check_title AND check_message must match before any change.
          DATA(lv_is_target) = abap_false.
          DATA(lv_title_match) = abap_false.
          PERFORM check_target_title USING wa_final-check_title CHANGING lv_title_match.
          IF lv_title_match = abap_true.
            PERFORM check_target_msg USING lv_msg CHANGING lv_is_target.
            IF lv_is_target = abap_false.
              PERFORM check_target_msg USING wa_final-message1 CHANGING lv_is_target.
            ENDIF.
          ENDIF.

          IF lv_is_target = abap_true.
            " Pragma depends on check_title
            CLEAR l_note.
            DATA(lv_title_up) = wa_final-check_title.
            TRANSLATE lv_title_up TO UPPER CASE.
            IF lv_title_up CS 'SIMPLIFIED OBJECTS'.
              CONCATENATE '"#EC CI_USAGE_OK[' wa_final-note ']' INTO l_note.
            ELSE.
              " S/4HANA: Field Length Extensions
              CONCATENATE '"#EC CI_FLDEXT_OK[' wa_final-note ']' INTO l_note.
            ENDIF.
            " BEGIN marker
            CLEAR wa_blank.
            CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
              INTO wa_blank-line SEPARATED BY space.
            APPEND wa_blank TO repos_tab_new.
            CLEAR wa_blank.
            " Star-comment the original line
            CONCATENATE '*' wa_repos_tab-line INTO wa_blank-line SEPARATED BY space.
            APPEND wa_blank TO repos_tab_new.
            CLEAR wa_blank.
            " Strip any existing inline comment before appending pragma
            IF wa_repos_tab-line CS '"'.
              wa_repos_tab-line = wa_repos_tab-line(sy-fdpos).
              REPLACE ALL OCCURRENCES OF '"' IN wa_repos_tab-line WITH space.
              CONDENSE wa_repos_tab-line.
            ENDIF.
            CONCATENATE wa_repos_tab-line l_note INTO wa_repos_tab-line SEPARATED BY space.
            APPEND wa_repos_tab TO repos_tab_new.
            CLEAR wa_blank.
            " END marker
            CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
              INTO wa_blank-line SEPARATED BY space.
            APPEND wa_blank TO repos_tab_new.
            CLEAR wa_blank.
          ELSE.
            APPEND wa_repos_tab TO repos_tab_new.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    DESCRIBE TABLE repos_tab LINES DATA(l_repos_old).
    DESCRIBE TABLE repos_tab_new LINES DATA(l_repos_new).
    IF repos_tab_new[] IS NOT INITIAL AND l_repos_old <> l_repos_new.
      IF wa_final_p-enhname IS INITIAL.
        CASE wa_final_p-objtype.
          WHEN 'SFPF'.
            PERFORM adobe_form_procee.
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
                IF p_sim IS INITIAL.
                  CLEAR wa_output-backup.
                  CONCATENATE 'ZTEST_CHECK' l_repid '_' sy-uname INTO wa_output-backup.
                  INSERT REPORT wa_output-backup FROM repos_tab.
                  REFRESH repos_tab.
                  COMMIT WORK.
                ENDIF.
              ENDIF.
            ENDIF.
            REFRESH repos_tab_new.
            wa_output-new_program = wa_final_p-sobjname.
            CLEAR it_error_table.
            PERFORM syntax_check USING wa_final_p-objname wa_final_p-objtype
                                 CHANGING it_error_table.
            IF it_error_table IS INITIAL.
              wa_output-status = 'Success'.
            ELSE.
              wa_output-status = 'Syntax error'.
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
            IF it_error_table IS INITIAL.
              wa_output-status = 'Success'.
            ELSE.
              wa_output-status = 'Syntax error'.
            ENDIF.
            APPEND wa_output TO it_output.
            CLEAR wa_output.
          WHEN 'SSFO'.
            PERFORM smartform_procee.
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
        IF it_error_table IS INITIAL.
          wa_output-status = 'Success'.
        ELSE.
          wa_output-status = 'Syntax error'.
        ENDIF.
        APPEND wa_output TO it_output.
        CLEAR wa_output.
      ENDIF.
    ENDIF.
    REFRESH repos_tab_new.
    REFRESH repos_tab.
    CLEAR : l_repos_new,l_repos_old.
  ENDLOOP.
  cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_table)
                          CHANGING  t_table      = it_output ).
  lo_table->get_columns( )->get_column( columnname = 'PROGRAM_NAME' )->set_long_text( 'Main Program Name' ).
  lo_table->get_columns( )->get_column( columnname = 'SUBOBJ' )->set_long_text( 'Sub Object Name' ).
  lo_table->get_columns( )->get_column( columnname = 'NEW_PROGRAM' )->set_long_text( 'New Program Name' ).
  lo_table->get_columns( )->get_column( columnname = 'BACKUP' )->set_long_text( 'Back Up Program Name' ).
  lo_table->get_columns( )->get_column( columnname = 'STATUS' )->set_long_text( 'Status' ).
  lo_table->display( ).

*&---------------------------------------------------------------------*
*& Form check_target_msg
*& Returns abap_true if the given message text matches one of the
*& P3 field-length / generic ATC check messages or the P2
*& Non-strategic-function message that this program handles.
*&---------------------------------------------------------------------*
FORM check_target_msg
  USING    iv_msg     TYPE string
  CHANGING cv_match   TYPE abap_bool.

  DATA lv_up TYPE string.
  lv_up = iv_msg.
  TRANSLATE lv_up TO UPPER CASE.

  " S/4HANA: Search for Usages of Simplified Objects ---------------
  IF lv_up CS 'NON-STRATEGIC-FUNCTION'.
    cv_match = abap_true. RETURN.
  ENDIF.

  " S/4HANA: Field Length Extensions – 49 check messages -----------
  IF lv_up CS 'ARITHMETIC OPERATION'.
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
*&---------------------------------------------------------------------*
*& Form check_target_title
*& Returns abap_true if check_title is one of the ATC categories
*& handled by this program.
*&---------------------------------------------------------------------*
FORM check_target_title
  USING    iv_title   TYPE string
  CHANGING cv_match   TYPE abap_bool.

  DATA lv_up TYPE string.
  lv_up = iv_title.
  TRANSLATE lv_up TO UPPER CASE.

  " S/4HANA: Field Length Extensions
  IF lv_up CS 'FIELD LENGTH EXTENSIONS'.
    cv_match = abap_true. RETURN.
  ENDIF.
  " S/4HANA: Search for Usages of Simplified Objects
  IF lv_up CS 'SIMPLIFIED OBJECTS'.
    cv_match = abap_true. RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form zatc_process_all
*&---------------------------------------------------------------------*
FORM zatc_process_all.
  SELECT * INTO TABLE it_zatc_process_all FROM zatc_process_all.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form zatc_process_dte
*&---------------------------------------------------------------------*
FORM zatc_process_dte.
  SELECT * INTO TABLE it_zatc_process_dte FROM zatc_process_dte.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form zatc_process1
*&---------------------------------------------------------------------*
FORM zatc_process1.
  SELECT * INTO TABLE it_zatc_process1 FROM zatc_process1.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form syntax_check
*&---------------------------------------------------------------------*
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
    WHEN 'SFPF'.
      SELECT SINGLE master FROM d010inc
        WHERE include = @program INTO @DATA(lv_prog).
      IF sy-subrc = 0. program = lv_prog. ENDIF.
    WHEN OTHERS.
  ENDCASE.
  CASE objecttype.
    WHEN 'PROG' OR 'CLASS' OR 'SFPF' OR 'FUGR'.
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
*&---------------------------------------------------------------------*
*& Form adobe_form_procee
*& Permanently updates Adobe Form (SFPF) ABAP code sections.
*&---------------------------------------------------------------------*
FORM adobe_form_procee.
  DATA: lv_fpname    TYPE fpname,
        lv_fm_name   TYPE rs38l_fnam,
        lv_prog      TYPE program,
        lv_fugr_name TYPE rs38l_fnam,
        lv_incl_top  TYPE program,
        lv_incl_init TYPE program,
        lv_incl_form TYPE program,
        lv_changed   TYPE flag.
  lv_fpname = wa_final_p-objname.
  CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING
      i_name     = lv_fpname
    IMPORTING
      e_funcname = lv_fm_name
    EXCEPTIONS
      not_found  = 1
      OTHERS     = 2.
  IF sy-subrc <> 0. RETURN. ENDIF.
  lv_fugr_name = lv_fm_name.
  lv_prog = 'SAPL' && lv_fugr_name(26).
  CALL FUNCTION 'SEO_FUGR_INCLUDE_GET'
    EXPORTING
      fugrname  = lv_fugr_name(26)
    IMPORTING
      top_incl  = lv_incl_top
      init_incl = lv_incl_init
      form_incl = lv_incl_form
    EXCEPTIONS
      OTHERS    = 1.
  DATA: lv_incl   TYPE program,
        lt_source_afpf TYPE STANDARD TABLE OF abaptxt255,
        lt_repos_new   TYPE STANDARD TABLE OF abaptxt255.
  LOOP AT repos_tab_new INTO DATA(wa_rn).
    APPEND wa_rn TO lt_repos_new.
  ENDLOOP.
  DO 3 TIMES.
    CASE sy-index.
      WHEN 1. lv_incl = lv_incl_top.
      WHEN 2. lv_incl = lv_incl_init.
      WHEN 3. lv_incl = lv_incl_form.
    ENDCASE.
    IF lv_incl IS INITIAL. CONTINUE. ENDIF.
    REFRESH lt_source_afpf.
    CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
      EXPORTING
        object_name = lv_incl
        versno      = '00000'
      TABLES
        repos_tab   = lt_source_afpf
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc <> 0. CONTINUE. ENDIF.
    lv_changed = abap_false.
    DATA lt_merged TYPE STANDARD TABLE OF abaptxt255.
    REFRESH lt_merged.
    LOOP AT lt_source_afpf INTO DATA(wa_orig_afpf).
      READ TABLE lt_repos_new INTO DATA(wa_rn2) INDEX sy-tabix.
      IF sy-subrc = 0 AND wa_orig_afpf-line <> wa_rn2-line.
        APPEND wa_rn2 TO lt_merged.
        lv_changed = abap_true.
      ELSE.
        APPEND wa_orig_afpf TO lt_merged.
      ENDIF.
    ENDLOOP.
    IF lv_changed = abap_true.
      DATA lv_trdir_afpf TYPE trdir.
      SELECT SINGLE * INTO @lv_trdir_afpf FROM trdir WHERE name = @lv_incl.
      CALL FUNCTION 'RPY_PROGRAM_UPDATE'
        EXPORTING
          program_name     = lv_incl
          program_type     = lv_trdir_afpf-subc
          transport_number = lv_req
        TABLES
          source_extended  = lt_merged
        EXCEPTIONS
          OTHERS           = 1.
      IF sy-subrc = 0. COMMIT WORK AND WAIT. ENDIF.
    ENDIF.
  ENDDO.
  DATA lv_sfpf_fm TYPE rs38l_fnam.
  CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING
      i_name     = lv_fpname
    IMPORTING
      e_funcname = lv_sfpf_fm
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc = 0.
    CALL FUNCTION 'SAPSCRIPT_GENERATE'
      EXPORTING
        object   = lv_fpname
        language = sy-langu
      EXCEPTIONS
        OTHERS   = 1.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form bdc_transaction
*&---------------------------------------------------------------------*
FORM bdc_transaction USING p_tcode TYPE any.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form bdc_dynpro
*&---------------------------------------------------------------------*
FORM bdc_dynpro USING p_program TYPE any p_dynpro TYPE any.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form bdc_field
*&---------------------------------------------------------------------*
FORM bdc_field USING p_fnam TYPE any p_fval TYPE any.
ENDFORM.
