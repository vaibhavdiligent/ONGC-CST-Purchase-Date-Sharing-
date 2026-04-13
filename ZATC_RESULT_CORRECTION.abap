*&---------------------------------------------------------------------*
*& Report ZATC_RESULT_CORRECTION
*&---------------------------------------------------------------------*
REPORT zatc_result_correction.
TYPE-POOLS seop.
TABLES : tadir.
TYPES: ty_swastrtab TYPE TABLE OF   swastrtab.
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
DATA l_cl_ddl TYPE REF TO cl_dd_ddl_field_tracker.
DATA l_ddlname TYPE ddlname.
DATA l_param1_n TYPE char50.
DATA it_query TYPE TABLE OF swastrtab.
DATA wa_query TYPE swastrtab.
DATA it_query_new TYPE TABLE OF swastrtab.
DATA it_query_new_temp TYPE TABLE OF swastrtab.
DATA l_seoclskey TYPE seoclskey.
DATA it_includes TYPE seop_methods_w_include.
DATA ls_mtdkey TYPE seocpdkey.
DATA : lt_source          TYPE seop_source,
       lt_source_expanded TYPE seop_source_string,
       ex_source_code_tab TYPE seop_source_string,
       lt_source_seo      TYPE seo_section_source,
       l_program_sec      TYPE program,
       l_clkey            TYPE seoclskey.
DATA : l_v1 TYPE char255,
       l_v2 TYPE char255,
       l_v3 TYPE char255.
DATA l_datum TYPE char10.
DATA l_note TYPE char40.
TYPES:
  BEGIN OF ts_base_field,
    entity_name   TYPE dd_cds_entity_name,
    element_name  TYPE fieldname,
    base_object   TYPE objectname,
    base_field    TYPE fieldname,
    is_calculated TYPE dd_cds_calculated,
  END OF ts_base_field,
  tt_base_fields TYPE SORTED TABLE OF ts_base_field WITH UNIQUE KEY entity_name element_name.
DATA it_error_table TYPE  syn_error.
DATA: l_varr TYPE char255.
DATA l_varr1 TYPE char255.
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
DATA it_zatc_process_all TYPE TABLE OF ty_zatc_process_all.
DATA it_zatc_process1 TYPE TABLE OF ty_zatc_process1.
DATA wa_zatc_process_all TYPE  ty_zatc_process_all.
DATA wa_zatc_process1 TYPE  ty_zatc_process1.
DATA l_repid(5) TYPE n.
DATA it_rt_base_fields TYPE tt_base_fields.
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
    MESSAGE 'Program Cannot be Excuted outside SAP GUI' TYPE 'E'.
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
  SELECT SINGLE display_id INTO i_result_id
    FROM satc_ac_resulth
    WHERE run_series_name = p_id.
  IF sy-subrc <> 0.
    MESSAGE 'Wrong ATC Varaint Selected' TYPE 'E'.
  ENDIF.
  SELECT SINGLE * INTO @DATA(l_e070)
    FROM e070
    WHERE trkorr = @lv_req.
  IF sy-subrc <> 0.
    MESSAGE 'Wrong Transport request selected' TYPE 'E'.
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
    CLEAR l_param1_n.
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
    wa_final-objtype     = finding-objtype.
    wa_final-objname     = finding-objname.
    wa_final-line        = finding-line.
    wa_final-program_name = finding-program_name.
    wa_final-param2      = finding-param2.
    wa_final-param3      = finding-param3.
    wa_final-param4      = finding-param4.
    wa_final-param1      = finding-param1.
    wa_final-message     = message.
    wa_final-sobjname    = finding-sobjname.
    wa_final-enhname     = finding-enhname.
    IF wa_final-param1 CS 'Note'.
      l_param1_n = wa_final-param1.
    ELSEIF wa_final-param2 CS 'Note'.
      l_param1_n = wa_final-param2.
    ELSEIF wa_final-param3 CS 'Note'.
      l_param1_n = wa_final-param3.
    ELSEIF wa_final-param4 CS 'Note'.
      l_param1_n = wa_final-param4.
    ENDIF.
    IF l_param1_n IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF 'Note' IN l_param1_n WITH space IGNORING CASE.
      DATA(l_n5) = strlen( l_param1_n ).
      l_n5 = l_n5 - sy-fdpos.
      l_param1_n = l_param1_n+sy-fdpos(l_n5).
      IF l_param1_n CA '1234567890'.
        l_n5 = strlen( l_param1_n ).
        l_n5 = l_n5 - sy-fdpos.
        wa_final-note = l_param1_n+sy-fdpos(l_n5).
        CONDENSE wa_final-note.
      ENDIF.
    ENDIF.
    SHIFT wa_final-note LEFT DELETING LEADING '0'.
    CONDENSE wa_final-note.
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
  PERFORM zatc_process1.
  REFRESH it_output.
  CLEAR l_repid.
  LOOP AT it_final_p INTO DATA(wa_final_p)
     WHERE ( sobjname(1) = 'Z' OR sobjname(1) = 'Y' ).
    l_repid = l_repid + 1.
    REFRESH repos_tab.
    object_name = wa_final_p-sobjname.
    CASE wa_final_p-objtype.
      WHEN 'PROG' OR 'FUGR' OR 'FUGS' OR 'SSFO'.
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
          CLEAR l_tabix1.
          l_tabix1 = l_tabix.
          CLEAR wa_blank.
          CLEAR l_text.
          CLEAR l_table.
          CLEAR l_where.
          CLEAR l_v.
          TRANSLATE wa_final-check_message TO UPPER CASE.
          TRANSLATE wa_final-check_title TO UPPER CASE.
          TRANSLATE wa_final-message1 TO UPPER CASE.
          CASE wa_final-check_title.
            WHEN 'S/4HANA: SEARCH FOR DATABASE OPERATIONS'.
              IF wa_final-message1 = 'DB OPERATION SELECT FOUND'
                OR wa_final-message1 = 'DB OPERATION JOIN FOUND'.
                CLEAR wa_zatc_process1.
                SELECT SINGLE * INTO @DATA(l_ars_api_succssr1)
                  FROM ars_api_succssr
                  WHERE object_key  = @wa_final-param1
                    AND object_type = 'TABL'.
                IF sy-subrc = 0.
                  wa_zatc_process1-correction_value = l_ars_api_succssr1-successor_tadir_obj_name.
                ENDIF.
                IF wa_final-param1 = 'KONV'.
                  wa_zatc_process1-correction_value = 'V_KONV_CDS'.
                ENDIF.
                IF wa_zatc_process1 IS NOT INITIAL.
                  l_ddlname = wa_zatc_process1-correction_value.
                  SELECT SINGLE * INTO @DATA(l_tadir)
                    FROM tadir
                    WHERE obj_name = @l_ddlname.
                  IF sy-subrc = 0.
                    IF wa_zatc_process1-correction_value IS NOT INITIAL.
                      CREATE OBJECT l_cl_ddl
                        EXPORTING iv_ddlname = l_ddlname.
                      REFRESH it_rt_base_fields.
                      TRY.
                          CALL METHOD l_cl_ddl->get_base_field_information
                            RECEIVING rt_base_fields = it_rt_base_fields.
                        CATCH cx_dd_ddl_read.
                      ENDTRY.
                      IF it_rt_base_fields[] IS INITIAL.
                        CLEAR wa_blank.
                        CONCATENATE '"' '"' p_rem p_begin sy-uname l_datum ' for ATC '
                          INTO wa_blank-line SEPARATED BY space.
                        APPEND wa_blank TO repos_tab_new.
                        CLEAR wa_blank.
                        CONCATENATE '*' wa_repos_tab-line
                          INTO wa_blank-line SEPARATED BY space.
                        APPEND wa_blank TO repos_tab_new.
                        CLEAR wa_blank.
                        CONCATENATE 'FROM' wa_final-param1
                          INTO wa_final-param1 SEPARATED BY space.
                        CONCATENATE 'FROM' wa_zatc_process1-correction_value
                          INTO wa_zatc_process1-correction_value SEPARATED BY space.
                        REPLACE ALL OCCURRENCES OF wa_final-param1 IN wa_repos_tab-line
                          WITH wa_zatc_process1-correction_value IGNORING CASE.
                        IF sy-subrc <> 0.
                          LOOP AT repos_tab INTO DATA(wa_repos_tab_d) FROM l_tabix.
                            l_tab = sy-tabix + 1.
                            CONDENSE wa_repos_tab_d-line.
                            REPLACE ALL OCCURRENCES OF wa_final-param1 IN wa_repos_tab_d-line
                              WITH wa_zatc_process1-correction_value IGNORING CASE.
                            IF sy-subrc <> 0.
                              APPEND wa_repos_tab_d TO repos_tab_new.
                            ELSE.
                              APPEND wa_repos_tab_d TO repos_tab_new.
                              EXIT.
                            ENDIF.
                          ENDLOOP.
                        ELSE.
                          APPEND wa_repos_tab TO repos_tab_new.
                        ENDIF.
                        CLEAR wa_blank.
                        CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
                          INTO wa_blank-line SEPARATED BY space.
                        APPEND wa_blank TO repos_tab_new.
                        CLEAR wa_blank.
                        l_tabix1 = l_tabix + 1.
                      ELSE.
                        REFRESH it_query.
                        REFRESH it_query_new.
                        DATA l_table_string TYPE string.
                        CLEAR l_table_string.
                        READ TABLE repos_tab INTO wa_repos_tab_d INDEX l_tabix.
                        IF sy-subrc = 0.
                          IF wa_repos_tab_d-line CS 'SELECT'.
                          ELSE.
                            DATA l_tabix_del TYPE sy-tabix.
                            CLEAR l_tabix_del.
                            DO 10 TIMES.
                              l_tabix = l_tabix - 1.
                              l_tabix_del = l_tabix_del + 1.
                              READ TABLE repos_tab INTO wa_repos_tab_d INDEX l_tabix.
                              IF sy-subrc = 0.
                                IF wa_repos_tab_d-line CS 'SELECT'.
                                  EXIT.
                                ENDIF.
                              ELSE.
                                EXIT.
                              ENDIF.
                            ENDDO.
                            IF l_tabix_del > 0.
                              DESCRIBE TABLE repos_tab_new LINES DATA(l_line_del).
                              DO l_tabix_del TIMES.
                                DELETE repos_tab_new INDEX l_line_del.
                                l_line_del = l_line_del - 1.
                              ENDDO.
                            ENDIF.
                          ENDIF.
                        ENDIF.
                        LOOP AT repos_tab INTO wa_repos_tab_d FROM l_tabix.
                          IF wa_repos_tab_d-line CS '"'.
                            DATA(l_fypos) = sy-fdpos.
                            wa_repos_tab_d-line = wa_repos_tab_d-line+0(l_fypos).
                          ENDIF.
                          IF wa_repos_tab_d-line CS '.'.
                            l_tab = sy-tabix + 1.
                            wa_query-str = wa_repos_tab_d-line.
                            APPEND wa_query TO it_query.
                            EXIT.
                          ELSE.
                            IF wa_repos_tab_d-line(1) <> '*'.
                              wa_query-str = wa_repos_tab_d-line.
                              APPEND wa_query TO it_query.
                            ENDIF.
                          ENDIF.
                        ENDLOOP.
                        IF it_query[] IS NOT INITIAL.
                          PERFORM change_table.
                        ENDIF.
                        IF it_query_new[] IS NOT INITIAL.
                          IF it_query_new[] = it_query[].
                            CLEAR l_tab.
                            CLEAR wa_blank.
                            CONCATENATE '"' '"' p_rem p_begin sy-uname l_datum ' for ATC '
                              INTO wa_blank-line SEPARATED BY space.
                            APPEND wa_blank TO repos_tab_new.
                            CLEAR l_note.
                            CONCATENATE '"#EC CI_DB_OPERATION_OK[' wa_final-note ']'
                              INTO l_note.
                            CONCATENATE wa_repos_tab-line l_note
                              INTO wa_repos_tab-line SEPARATED BY space.
                            APPEND wa_repos_tab TO repos_tab_new.
                            CLEAR wa_blank.
                            CONCATENATE '"' '"' p_rem p_end sy-uname l_datum 'for ATC'
                              INTO wa_blank-line SEPARATED BY space.
                            APPEND wa_blank TO repos_tab_new.
                            CLEAR wa_blank.
                          ELSE.
                            CLEAR wa_blank.
                            CONCATENATE '"' '"' p_rem p_begin sy-uname l_datum ' for ATC '
                              INTO wa_blank-line SEPARATED BY space.
                            APPEND wa_blank TO repos_tab_new.
                            CLEAR wa_blank.
                            LOOP AT repos_tab INTO wa_repos_tab_d FROM l_tabix.
                              IF wa_repos_tab_d-line CS '.'.
                                CONCATENATE '*' wa_repos_tab_d-line INTO wa_blank-line.
                                APPEND wa_blank TO repos_tab_new.
                                CLEAR wa_blank.
                                EXIT.
                              ELSE.
                                CONCATENATE '*' wa_repos_tab_d-line INTO wa_blank-line.
                                APPEND wa_blank TO repos_tab_new.
                                CLEAR wa_blank.
                              ENDIF.
                            ENDLOOP.
                            LOOP AT it_query_new INTO wa_query.
                              wa_blank-line = wa_query-str.
                              APPEND wa_blank TO repos_tab_new.
                              CLEAR wa_blank.
                            ENDLOOP.
                            CLEAR wa_blank.
                            CONCATENATE '"' '"' p_rem p_end sy-uname l_datum 'for ATC'
                              INTO wa_blank-line SEPARATED BY space.
                            APPEND wa_blank TO repos_tab_new.
                            CLEAR wa_blank.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ELSE.
                      APPEND wa_repos_tab TO repos_tab_new.
                    ENDIF.
                  ELSE.
                    APPEND wa_repos_tab TO repos_tab_new.
                  ENDIF.
                ELSE.
                  APPEND wa_repos_tab TO repos_tab_new.
                ENDIF.
              ENDIF.
              REFRESH : it_query,it_query_new.
            WHEN 'S/4HANA: FIELD LENGTH EXTENSIONS'.
              CASE wa_final-check_message.
                WHEN 'CALL METHOD GENERIC PARAMETER' OR 'OLD STRUCTURE-COMPONENT TYPE CONFLICT'
                  OR 'CALL FUNCTION GENERIC PARAMETER' OR 'WRITE ISSUE' OR 'WRITE-LENGTH ISSUE'
                  OR 'SET PARAMETER ISSUE' OR 'OLD SELECT TYPE CONFLICT' OR 'MOVE GENERIC ->'
                  OR 'MOVE -> GENERIC' OR ' REPLACE ISSUE' OR 'OFFSET/LENGTH-ACCESS'
                  OR 'OLD MOVE LENGTH CONFLICT' OR 'GENERIC SOURCE CODE ISSUE'.
                  IF ( wa_final-check_message = 'OFFSET/LENGTH-ACCESS' OR
                       wa_final-check_message = 'OLD MOVE LENGTH CONFLICT' ).
                    IF wa_final-priority > 1.
                      SELECT SINGLE * INTO @DATA(l_fle_topic_switch)
                        FROM fle_topic_switch WHERE matnr_target_mode = 'X'.
                      IF sy-subrc = 0.
                        APPEND wa_repos_tab TO repos_tab_new.
                        CONTINUE.
                      ENDIF.
                    ELSE.
                      APPEND wa_repos_tab TO repos_tab_new.
                      CONTINUE.
                    ENDIF.
                  ENDIF.
                  CLEAR wa_blank.
                  CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
                    INTO wa_blank-line SEPARATED BY space.
                  APPEND wa_blank TO repos_tab_new.
                  CLEAR l_note.
                  CONCATENATE '"#EC CI_FLDEXT_OK[' wa_final-note ']' INTO l_note.
                  CONCATENATE '*' wa_repos_tab-line INTO wa_blank-line SEPARATED BY space.
                  APPEND wa_blank TO repos_tab_new.
                  CLEAR wa_blank.
                  IF wa_repos_tab-line CS '"'.
                    wa_repos_tab-line = wa_repos_tab-line(sy-fdpos).
                    REPLACE ALL OCCURRENCES OF '"' IN wa_repos_tab-line WITH space.
                    CONDENSE wa_repos_tab-line.
                  ENDIF.
                  CONCATENATE wa_repos_tab-line l_note INTO wa_repos_tab-line SEPARATED BY space.
                  APPEND wa_repos_tab TO repos_tab_new.
                  CLEAR wa_blank.
                  CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
                    INTO wa_blank-line SEPARATED BY space.
                  APPEND wa_blank TO repos_tab_new.
                  CLEAR wa_blank.
                WHEN 'IMPORT ISSUE'.
                  CLEAR wa_blank.
                  CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
                    INTO wa_blank-line SEPARATED BY space.
                  APPEND wa_blank TO repos_tab_new.
                  CLEAR wa_blank.
                  CONCATENATE '*' wa_repos_tab-line INTO wa_blank-line SEPARATED BY space.
                  APPEND wa_blank TO repos_tab_new.
                  CLEAR wa_blank.
                  IF wa_repos_tab-line CS '"'.
                    DATA(l_import_fdpos) = sy-fdpos - 1.
                    wa_repos_tab-line = wa_repos_tab-line(l_import_fdpos).
                  ENDIF.
                  REPLACE ALL OCCURRENCES OF '.' IN wa_repos_tab-line WITH space.
                  CONDENSE wa_repos_tab-line.
                  CONCATENATE wa_repos_tab-line ' accepting padding .'
                    INTO wa_blank-line SEPARATED BY space.
                  APPEND wa_blank TO repos_tab_new.
                  CLEAR wa_blank.
                  CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
                    INTO wa_blank-line SEPARATED BY space.
                  APPEND wa_blank TO repos_tab_new.
                  CLEAR wa_blank.
                WHEN 'MOVE TYPE CONFLICT' OR 'MOVE LENGTH CONFLICT'
                  OR 'OLD ARITHMETIC TYPE CONFLICT' OR 'OLD MOVE TYPE CONFLICT'.
                  IF wa_repos_tab-line CS '='.
                    DATA(l_fpos) = sy-fdpos.
                    DATA(l_i) = strlen( wa_repos_tab-line ).
                    l_fpos = l_fpos + 1.
                    IF l_fpos <> l_i.
                      CLEAR wa_blank.
                      CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
                        INTO wa_blank-line SEPARATED BY space.
                      APPEND wa_blank TO repos_tab_new.
                      CLEAR wa_blank.
                      CONCATENATE '*' wa_repos_tab-line INTO wa_blank-line.
                      APPEND wa_blank TO repos_tab_new.
                      CLEAR wa_blank.
                      l_i = l_i - l_fpos.
                      CLEAR: l_v1,l_v2,l_v3.
                      l_v1 = wa_repos_tab-line+0(l_fpos).
                      l_v2 = wa_repos_tab-line+l_fpos(l_i).
                      CONDENSE l_v1. CONDENSE l_v2.
                      IF l_v2 CS '"'.
                        l_fpos = sy-fdpos.
                        l_v2 = l_v2+0(l_fpos).
                      ENDIF.
                      REPLACE ALL OCCURRENCES OF '.' IN l_v2 WITH space.
                      CONDENSE l_v2.
                      CLEAR wa_repos_tab-line.
                      CONCATENATE 'CONV' '#(' INTO l_v3 SEPARATED BY space.
                      CONCATENATE l_v1 l_v3 l_v2 ')' '.'
                        INTO wa_repos_tab-line SEPARATED BY space.
                      APPEND wa_repos_tab TO repos_tab_new.
                      CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
                        INTO wa_blank-line SEPARATED BY space.
                      APPEND wa_blank TO repos_tab_new.
                      CLEAR wa_blank.
                    ELSE.
                      APPEND wa_repos_tab TO repos_tab_new.
                    ENDIF.
                  ELSE.
                    APPEND wa_repos_tab TO repos_tab_new.
                  ENDIF.
                WHEN OTHERS.
                  APPEND wa_repos_tab TO repos_tab_new.
              ENDCASE.
            WHEN 'S/4HANA: IDOC CHECK' OR 'S/4HANA: SEARCH FOR ABAP DICTIONARY ENHANCEMENTS'
              OR 'S/4HANA: SEARCH FOR DATABASE OPERATIONS'
              OR 'S/4HANA: READINESS CHECK FOR SAP QUERIES'
              OR 'S/4HANA: SEARCH FOR BASE TABLES OF ABAP DICTIONARY VIEWS'.
              APPEND wa_repos_tab TO repos_tab_new.
            WHEN 'S/4HANA: SEARCH FOR USAGES OF SIMPLIFIED OBJECTS'.
              CASE wa_final-check_message.
                WHEN 'SYNTACTICALLY INCOMPATIBLE CHANGE OF EXISTING FUNCTIONALITY'
                  OR 'FUNCTIONALITY UNAVAILABLE'
                  OR 'FUNCTIONALITY NOT AVAILABLE: EQUIVALENT FUNCTION ON ROADMAP'.
                  IF wa_final-param1 CS '0002628704' OR wa_final-param1 CS '0002438131'.
                    IF wa_final-param1 CS '0002628704'.
                      PERFORM amount_conv.
                    ELSE.
                      PERFORM material_conv.
                    ENDIF.
                    CLEAR wa_blank.
                    CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
                      INTO wa_blank-line SEPARATED BY space.
                    APPEND wa_blank TO repos_tab_new.
                    CLEAR l_note.
                    CONCATENATE '"#EC CI_USAGE_OK[' wa_final-note ']' INTO l_note.
                    CONCATENATE wa_repos_tab-line l_note INTO wa_repos_tab-line.
                    APPEND wa_repos_tab TO repos_tab_new.
                    CLEAR wa_blank.
                    CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
                      INTO wa_blank-line SEPARATED BY space.
                    APPEND wa_blank TO repos_tab_new.
                    CLEAR wa_blank.
                  ELSE.
                    READ TABLE it_zatc_process_all INTO wa_zatc_process_all WITH KEY
                      srch_tem     = wa_final-param2
                      ref_obj_type = wa_final-param3.
                    IF sy-subrc = 0 AND wa_zatc_process_all-solution = 'X'
                      AND wa_zatc_process_all-fix_by <> 'FIT GAP'.
                      CASE wa_final-param3.
                        WHEN 'DTEL'.
                          CLEAR wa_blank.
                          CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
                            INTO wa_blank-line SEPARATED BY space.
                          APPEND wa_blank TO repos_tab_new.
                          CLEAR wa_blank.
                          CONCATENATE '*' wa_repos_tab-line INTO wa_blank-line SEPARATED BY space.
                          APPEND wa_blank TO repos_tab_new.
                          CLEAR wa_blank.
                          IF wa_repos_tab-line CS 'TYPE'.
                            DATA(l_dy) = sy-fdpos.
                            DATA(l_n) = strlen( wa_repos_tab-line ).
                            l_n = l_n - l_dy + 1.
                          ENDIF.
                          REPLACE ALL OCCURRENCES OF wa_final-param2 IN wa_repos_tab-line+l_dy(l_n)
                            WITH wa_zatc_process_all-correction_value IGNORING CASE.
                          APPEND wa_repos_tab TO repos_tab_new.
                          CLEAR wa_blank.
                          CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
                            INTO wa_blank-line SEPARATED BY space.
                          APPEND wa_blank TO repos_tab_new.
                          CLEAR wa_blank.
                        WHEN OTHERS.
                          APPEND wa_repos_tab TO repos_tab_new.
                      ENDCASE.
                    ELSEIF wa_final-priority = '1' AND wa_final-param3 = 'TRAN'.
                      IF wa_final-param2+3(1) = '3'.
                        SELECT SINGLE * INTO @DATA(l_prgn_corr2)
                          FROM prgn_corr2
                          WHERE s_tcode = @wa_final-param2.
                        IF sy-subrc = 0.
                          IF l_prgn_corr2-t_tcode = 'BP'.
                            PERFORM replace_bp.
                          ELSEIF l_prgn_corr2-t_tcode = 'MIGO'.
                            PERFORM replace_migo.
                          ELSE.
                            APPEND wa_repos_tab TO repos_tab_new.
                          ENDIF.
                        ELSE.
                          APPEND wa_repos_tab TO repos_tab_new.
                        ENDIF.
                      ELSE.
                        APPEND wa_repos_tab TO repos_tab_new.
                      ENDIF.
                    ELSE.
                      APPEND wa_repos_tab TO repos_tab_new.
                    ENDIF.
                  ENDIF.
                WHEN 'FUNCTIONALITY NOT AVAILABLE: FUNCTIONAL EQUIVALENT AVAILABLE'.
                  IF wa_final-priority = '1' AND wa_final-param3 = 'TRAN'.
                    IF wa_final-param2+3(1) = '3'.
                      SELECT SINGLE * INTO @l_prgn_corr2
                        FROM prgn_corr2 WHERE s_tcode = @wa_final-param2.
                      IF sy-subrc = 0.
                        IF l_prgn_corr2-t_tcode = 'BP'.
                          PERFORM replace_bp.
                        ELSEIF l_prgn_corr2-t_tcode = 'MIGO'.
                          PERFORM replace_migo.
                        ELSE.
                          APPEND wa_repos_tab TO repos_tab_new.
                        ENDIF.
                      ELSE.
                        APPEND wa_repos_tab TO repos_tab_new.
                      ENDIF.
                    ELSE.
                      APPEND wa_repos_tab TO repos_tab_new.
                    ENDIF.
                  ELSE.
                    APPEND wa_repos_tab TO repos_tab_new.
                  ENDIF.
                WHEN 'NON-STRATEGIC-FUNCTION: FUNCTIONAL EQUIVALENT AVAILABLE'.
                  IF wa_final-priority = '2' OR wa_final-priority = '3'.
                    CLEAR wa_blank.
                    CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
                      INTO wa_blank-line SEPARATED BY space.
                    APPEND wa_blank TO repos_tab_new.
                    CLEAR wa_blank.
                    CLEAR l_note.
                    CONCATENATE '"#EC CI_USAGE_OK[' wa_final-note ']' INTO l_note.
                    CONCATENATE '*' wa_repos_tab-line INTO wa_blank-line SEPARATED BY space.
                    APPEND wa_blank TO repos_tab_new.
                    CLEAR wa_blank.
                    IF wa_repos_tab-line CS '"'.
                      wa_repos_tab-line = wa_repos_tab-line(sy-fdpos).
                      REPLACE ALL OCCURRENCES OF '"' IN wa_repos_tab-line WITH space.
                      CONDENSE wa_repos_tab-line.
                    ENDIF.
                    CONCATENATE wa_repos_tab-line l_note INTO wa_repos_tab-line SEPARATED BY space.
                    APPEND wa_repos_tab TO repos_tab_new.
                    CLEAR wa_blank.
                    CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
                      INTO wa_blank-line SEPARATED BY space.
                    APPEND wa_blank TO repos_tab_new.
                    CLEAR wa_blank.
                  ELSE.
                    APPEND wa_repos_tab TO repos_tab_new.
                  ENDIF.
                WHEN OTHERS.
                  APPEND wa_repos_tab TO repos_tab_new.
              ENDCASE.
            WHEN 'S/4HANA: SEARCH FOR SIMPLIFIED TRANSACTIONS IN LITERALS'.
              IF wa_final-priority = '3'.
                CASE wa_final-check_message.
                  WHEN 'FUNCTIONALITY NOT AVAILABLE: NO FUNCTIONAL EQUIVALENT'
                    OR 'FUNCTIONALITY NOT AVAILABLE: EQUIVALENT FUNCTION ON ROADMAP'
                    OR 'FUNCTIONALITY UNAVAILABLE'
                    OR 'FUNCTIONALITY NOT AVAILABLE: FUNCTIONAL EQUIVALENT AVAILABLE'.
                    IF wa_final-param3 = 'TRAN'.
                      CLEAR wa_blank.
                      CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
                        INTO wa_blank-line SEPARATED BY space.
                      APPEND wa_blank TO repos_tab_new.
                      CLEAR wa_blank.
                      CLEAR l_note.
                      CONCATENATE '"#EC CI_USAGE_OK[' wa_final-note ']' INTO l_note.
                      IF wa_repos_tab-line CS wa_final-param2.
                        DATA(l_method)  = sy-fdpos.
                        DATA(l_method1) = strlen( wa_repos_tab-line ).
                        DATA l_method3 TYPE char100.
                        l_method  = l_method - 1.
                        l_method1 = l_method1 - l_method.
                        CLEAR wa_blank.
                        wa_blank-line = wa_repos_tab-line(l_method).
                        APPEND wa_blank TO repos_tab_new.
                        CLEAR wa_blank.
                        l_method3 = wa_repos_tab-line+l_method(l_method1).
                        CONCATENATE TEXT-001 wa_final-param2 TEXT-001 INTO wa_blank-line.
                        REPLACE ALL OCCURRENCES OF wa_blank-line IN l_method3 WITH space IGNORING CASE.
                        CONCATENATE wa_blank-line l_note INTO wa_blank-line SEPARATED BY space.
                        APPEND wa_blank TO repos_tab_new.
                        CLEAR wa_blank.
                        wa_blank-line = l_method3.
                        APPEND wa_blank TO repos_tab_new.
                        CLEAR wa_blank.
                        CLEAR l_method3.
                      ENDIF.
                      CLEAR wa_blank.
                      CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
                        INTO wa_blank-line SEPARATED BY space.
                      APPEND wa_blank TO repos_tab_new.
                      CLEAR wa_blank.
                    ELSE.
                      APPEND wa_repos_tab TO repos_tab_new.
                    ENDIF.
                  WHEN OTHERS.
                    APPEND wa_repos_tab TO repos_tab_new.
                ENDCASE.
              ELSE.
                APPEND wa_repos_tab TO repos_tab_new.
              ENDIF.
            WHEN 'SEARCH PROBLEMATIC STATEMENTS FOR RESULT OF SELECT/OPEN CURSOR WITHOUT ORDER BY'.
              CASE wa_final-check_message.
                WHEN 'DELETE ADJACENT DUPLICATES FOR RESULT OF STATEMENT AT ... LINE ...'.
                  l_text = wa_repos_tab-line.
                  TRANSLATE l_text TO UPPER CASE.
                  REPLACE ALL OCCURRENCES OF 'DELETE ADJACENT DUPLICATES' IN l_text WITH space IGNORING CASE.
                  REPLACE ALL OCCURRENCES OF 'FROM' IN l_text WITH space IGNORING CASE.
                  CONDENSE l_text.
                  CLEAR l_i.
                  l_i = strlen( l_text ).
                  DO l_i TIMES.
                    DATA(l_var) = l_text+l_v(1).
                    IF l_var = ' '.
                      EXIT.
                    ELSE.
                      CONCATENATE l_table l_var INTO l_table.
                    ENDIF.
                    l_v = l_v + 1.
                  ENDDO.
                  CONDENSE l_table.
                  REPLACE ALL OCCURRENCES OF l_table IN l_text WITH space IGNORING CASE.
                  REPLACE ALL OCCURRENCES OF 'COMPARING' IN l_text WITH space IGNORING CASE.
                  CONDENSE l_text.
                  l_where = l_text.
                  IF l_where CS 'ALL FIELDS'.
                    CONCATENATE 'SORT' l_table '.' INTO l_new SEPARATED BY space.
                  ELSE.
                    CONCATENATE 'SORT' l_table 'BY' l_where INTO l_new SEPARATED BY space.
                  ENDIF.
                  CLEAR wa_blank.
                  CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
                    INTO wa_blank-line SEPARATED BY space.
                  APPEND wa_blank TO repos_tab_new.
                  CLEAR wa_blank.
                  wa_blank-line = l_new.
                  APPEND wa_blank TO repos_tab_new.
                  CLEAR wa_blank.
                  CONCATENATE '"' p_rem p_end sy-uname l_datum ' for ATC'
                    INTO wa_blank-line SEPARATED BY space.
                  APPEND wa_blank TO repos_tab_new.
                  CLEAR wa_blank.
                  APPEND wa_repos_tab TO repos_tab_new.
                WHEN 'LOOP AT ITAB. EXIT/RETURN/LEAVE ... FOR RESULT OF STATEMENT AT ... LINE ...'
                  OR 'LOOP AT EMPTY ITAB. ... FOR RESULT OF STATEMENT AT ... LINE ...'
                  OR 'WRITE IN LOOP FOR RESULT OF STATEMENT AT ... LINE ...'.
                  PERFORM loop_exit.
                  APPEND wa_repos_tab TO repos_tab_new.
                WHEN 'READ TABLE ... INDEX 1 FOR RESULT OF STATEMENT AT ... LINE ...'
                  OR 'LOOP AT ITAB FROM/TO FOR RESULT OF STATEMENT AT ... LINE ...'
                  OR 'MODIFY ... INDEX FOR RESULT OF SELECT STATEMENT AT ... LINE ...'
                  OR 'READ TABLE ... INDEX FOR RESULT OF SELECT STATEMENT AT ... LINE ...'
                  OR 'READ TABLE ... INDEX C_CONST FOR RESULT OF STATEMENT AT ... LINE ...'
                  OR 'DELETE ... INDEX FOR RESULT OF SELECT STATEMENT AT ... LINE ...'.
                  CLEAR wa_blank.
                  CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
                    INTO wa_blank-line SEPARATED BY space.
                  APPEND wa_blank TO repos_tab_new.
                  CLEAR wa_blank.
                  CLEAR : l_varr,l_varr1.
                  CLEAR wa_blank.
                  IF wa_repos_tab-line CS '"'.
                    DATA(l_ty)  = sy-fdpos.
                    DATA(l_len) = strlen( wa_repos_tab-line ).
                    l_len  = l_len - l_ty.
                    l_varr = wa_repos_tab-line(l_ty).
                    l_varr1 = wa_repos_tab-line+l_ty(l_len).
                    CONCATENATE l_varr '"#EC CI_NOORDER' l_varr1
                      INTO wa_blank-line SEPARATED BY space.
                    APPEND wa_blank TO repos_tab_new.
                    CLEAR wa_blank.
                  ELSE.
                    CONCATENATE wa_repos_tab-line '"#EC CI_NOORDER'
                      INTO wa_blank-line SEPARATED BY space.
                    APPEND wa_blank TO repos_tab_new.
                    CLEAR wa_blank.
                  ENDIF.
                  CONCATENATE '"' p_rem p_end sy-uname l_datum ' for ATC'
                    INTO wa_blank-line SEPARATED BY space.
                  APPEND wa_blank TO repos_tab_new.
                  CLEAR wa_blank.
                WHEN 'SELECT ... FOR (FORMER) CLUSTER TABLE ... WITHOUT ORDER BY FOUND'
                  OR 'SELECT ... FOR (FORMER) POOL TABLE ... WITHOUT ORDER BY FOUND'
                  OR 'SELECT .. UP TO .. ROWS WITHOUT ORDER BY FOUND'.
                  CLEAR l_for.
                  LOOP AT repos_tab INTO DATA(wa_repos_tab1) FROM l_tabix1.
                    l_tab = sy-tabix.
                    IF wa_repos_tab1-line CS 'FOR ALL ENTRIES'.
                      l_for = 'X'.
                    ENDIF.
                    IF wa_repos_tab1-line CS '.' AND l_for IS INITIAL.
                      REPLACE ALL OCCURRENCES OF '.' IN wa_repos_tab1-line WITH space IGNORING CASE.
                      CONCATENATE wa_repos_tab1-line 'ORDER BY PRIMARY KEY.'
                        INTO wa_repos_tab1-line SEPARATED BY space.
                      APPEND wa_repos_tab1 TO repos_tab_new.
                      l_tab = l_tab + 1.
                      EXIT.
                    ELSE.
                      IF l_for = 'X' AND wa_repos_tab1-line CS '.'.
                        CONCATENATE wa_repos_tab1-line '"#EC CI_NOORDER'
                          INTO wa_repos_tab1-line SEPARATED BY space.
                        APPEND wa_repos_tab1 TO repos_tab_new.
                        l_tab = l_tab + 1.
                        EXIT.
                      ELSE.
                        APPEND wa_repos_tab1 TO repos_tab_new.
                      ENDIF.
                    ENDIF.
                  ENDLOOP.
                WHEN 'SELECT SINGLE IS POSSIBLY NOT UNIQUE'.
                  LOOP AT repos_tab INTO DATA(wa_repos_tab_d) FROM l_tabix.
                    IF wa_repos_tab_d-line CS '"'.
                      DATA(l_fdpos) = sy-fdpos.
                      wa_repos_tab_d-line = wa_repos_tab_d-line+0(l_fdpos).
                    ENDIF.
                    IF wa_repos_tab_d-line(1) = '*'.
                      CONTINUE.
                    ENDIF.
                    IF wa_repos_tab_d-line CS '.'.
                      l_tab = sy-tabix + 1.
                      wa_query-str = wa_repos_tab_d-line.
                      APPEND wa_query TO it_query.
                      EXIT.
                    ELSE.
                      IF wa_repos_tab_d-line(1) <> '*'.
                        wa_query-str = wa_repos_tab_d-line.
                        APPEND wa_query TO it_query.
                      ENDIF.
                    ENDIF.
                  ENDLOOP.
                  PERFORM change_single.
                  IF it_query_new[] IS NOT INITIAL.
                    CLEAR wa_blank.
                    CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
                      INTO wa_blank-line SEPARATED BY space.
                    APPEND wa_blank TO repos_tab_new.
                    CLEAR wa_blank.
                    LOOP AT repos_tab INTO wa_repos_tab_d FROM l_tabix.
                      IF wa_repos_tab_d-line CS '.'.
                        CONCATENATE '*' wa_repos_tab_d-line INTO wa_blank-line.
                        APPEND wa_blank TO repos_tab_new.
                        CLEAR wa_blank.
                        EXIT.
                      ELSE.
                        CONCATENATE '*' wa_repos_tab_d-line INTO wa_blank-line.
                        APPEND wa_blank TO repos_tab_new.
                        CLEAR wa_blank.
                      ENDIF.
                    ENDLOOP.
                    LOOP AT it_query_new INTO wa_query.
                      wa_blank-line = wa_query-str.
                      APPEND wa_blank TO repos_tab_new.
                      CLEAR wa_blank.
                    ENDLOOP.
                    CLEAR wa_blank.
                    CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
                      INTO wa_blank-line SEPARATED BY space.
                    APPEND wa_blank TO repos_tab_new.
                    CLEAR wa_blank.
                  ENDIF.
                  REFRESH : it_query,it_query_new.
                WHEN 'READ .. BINARY SEARCH FOR RESULT OF STATEMENT AT ... LINE ...'.
                  PERFORM process_read.
                  APPEND wa_repos_tab TO repos_tab_new.
                WHEN 'LOOP AT ITAB. AT ... ENDAT. FOR RESULT OF STATEMENT AT ... LINE ...'.
                  PERFORM endat.
                  APPEND wa_repos_tab TO repos_tab_new.
                WHEN 'LOOP AT ITAB. ON CHANGE OF ... ENDON. FOR RESULT OF STATEMENT AT ... LINE ...'.
                  PERFORM process_change_loop.
                  APPEND wa_repos_tab TO repos_tab_new.
                WHEN 'EMPTY SELECT/ENDSELECT AT ... LINE ...'.
                  CLEAR wa_blank.
                  CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
                    INTO wa_blank-line SEPARATED BY space.
                  APPEND wa_blank TO repos_tab_new.
                  CONCATENATE wa_repos_tab-line '"#EC CI_NOORDER'
                    INTO wa_repos_tab-line SEPARATED BY space.
                  APPEND wa_repos_tab TO repos_tab_new.
                  CLEAR wa_blank.
                  CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
                    INTO wa_blank-line SEPARATED BY space.
                  APPEND wa_blank TO repos_tab_new.
                  CLEAR wa_blank.
                WHEN 'ALV CALL AT ... LINE ...'.
                  IF wa_repos_tab-line CS 'CALL METHOD cl_salv_table=>factory'.
                    CLEAR l_alv.
                    LOOP AT repos_tab INTO wa_repos_tab_d FROM l_tabix.
                      IF wa_repos_tab_d-line CS 't_table'.
                        DATA(l_line_alv) = wa_repos_tab_d-line.
                        REPLACE ALL OCCURRENCES OF 'T_TABLE' IN l_line_alv WITH space IGNORING CASE.
                        REPLACE ALL OCCURRENCES OF '=' IN l_line_alv WITH space IGNORING CASE.
                        REPLACE ALL OCCURRENCES OF '.' IN l_line_alv WITH space IGNORING CASE.
                        CONDENSE l_line_alv.
                        l_alv = l_line_alv.
                      ENDIF.
                      IF wa_repos_tab_d-line CS '.'. EXIT. ENDIF.
                    ENDLOOP.
                    IF l_alv IS INITIAL.
                      APPEND wa_repos_tab TO repos_tab_new.
                    ELSE.
                      CLEAR wa_blank.
                      CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
                        INTO wa_blank-line SEPARATED BY space.
                      APPEND wa_blank TO repos_tab_new.
                      CLEAR wa_blank.
                      CONCATENATE 'SORT ' l_alv '.' INTO wa_blank-line SEPARATED BY space.
                      APPEND wa_blank TO repos_tab_new.
                      CLEAR wa_blank.
                      CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
                        INTO wa_blank-line SEPARATED BY space.
                      APPEND wa_blank TO repos_tab_new.
                      CLEAR wa_blank.
                      APPEND wa_repos_tab TO repos_tab_new.
                    ENDIF.
                  ELSE.
                    APPEND wa_repos_tab TO repos_tab_new.
                  ENDIF.
                WHEN OTHERS.
                  APPEND wa_repos_tab TO repos_tab_new.
              ENDCASE.
            WHEN 'USE OF ADBC INTERFACE'.
              CASE wa_final-check_message.
                WHEN 'ADBC CLASS ... USED'.
                  CLEAR wa_blank.
                  CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
                    INTO wa_blank-line SEPARATED BY space.
                  APPEND wa_blank TO repos_tab_new.
                  CONCATENATE wa_repos_tab-line '"#EC CI_ADBC_US'
                    INTO wa_repos_tab-line SEPARATED BY space.
                  APPEND wa_repos_tab TO repos_tab_new.
                  CLEAR wa_blank.
                  CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
                    INTO wa_blank-line SEPARATED BY space.
                  APPEND wa_blank TO repos_tab_new.
                  CLEAR wa_blank.
                WHEN OTHERS.
                  APPEND wa_repos_tab TO repos_tab_new.
              ENDCASE.
            WHEN OTHERS.
              APPEND wa_repos_tab TO repos_tab_new.
          ENDCASE.
        ENDIF.
      ENDLOOP.
    ENDIF.
    DESCRIBE TABLE repos_tab LINES DATA(l_repos_old).
    DESCRIBE TABLE repos_tab_new LINES DATA(l_repos_new).
    IF repos_tab_new[] IS NOT INITIAL AND l_repos_old <> l_repos_new.
      IF wa_final_p-enhname IS INITIAL.
        CASE wa_final_p-objtype.
          WHEN 'PROG' OR 'FUGR' OR 'FUGS'.
            SELECT SINGLE * INTO @DATA(l_trdir)
              FROM trdir WHERE name = @wa_final_p-sobjname.
            wa_output-program_name = wa_final_p-objname.
            wa_output-subobj = wa_final_p-sobjname.
            IF p_sim = 'X'.
              CLEAR wa_final_p-sobjname.
              CONCATENATE 'ZTEST_CHECK' l_repid INTO wa_final_p-sobjname.
              INSERT REPORT wa_final_p-sobjname FROM repos_tab_new.
              REFRESH repos_tab_new.
              COMMIT WORK AND WAIT.
            ELSE.
              CALL FUNCTION 'RPY_PROGRAM_UPDATE'
                EXPORTING
                  program_name     = wa_final_p-sobjname
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
                  CONCATENATE 'ZTEST_CHECK' l_repid INTO wa_output-backup.
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
              wa_output-status = 'Syyntax error'.
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
              CONCATENATE 'ZTEST_CHECK' l_repid INTO wa_includes-incname.
            ELSE.
              CLEAR wa_output-backup.
              CONCATENATE 'ZTEST_CHECK' l_repid INTO wa_output-backup.
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
              wa_output-status = 'Syyntax error'.
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
          wa_output-status = 'Syyntax error'.
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
*& Form change_table
*&---------------------------------------------------------------------*
FORM change_table.
  TYPES:
    BEGIN OF ts_base_field,
      entity_name   TYPE dd_cds_entity_name,
      element_name  TYPE fieldname,
      base_object   TYPE objectname,
      base_field    TYPE fieldname,
      is_calculated TYPE dd_cds_calculated,
    END OF ts_base_field,
    tt_base_fields TYPE TABLE OF ts_base_field.
  DATA l_string TYPE string.
  DATA l_from   TYPE i.
  DATA l_where  TYPE i.
  DATA l_len    TYPE i.
  DATA l_i      TYPE i.
  DATA l_into   TYPE i.
  DATA l_bras   TYPE c.
  DATA l_value  TYPE char1.
  TYPES: BEGIN OF ty_table,    value TYPE char72,    END OF ty_table.
  TYPES: BEGIN OF ty_table_q,
           value     TYPE char72,
           symbol    TYPE char10,
           as        TYPE char10,
           new_table TYPE char100,
         END OF ty_table_q.
  TYPES: BEGIN OF ty_value,    value TYPE char72,    END OF ty_value.
  DATA l_tab_i    TYPE n.
  DATA it_value   TYPE TABLE OF ty_value.
  DATA wa_value   TYPE ty_value.
  DATA it_table   TYPE TABLE OF ty_table.
  DATA it_table_q TYPE TABLE OF ty_table_q.
  DATA wa_table_q TYPE ty_table_q.
  DATA wa_table   TYPE ty_table.
  DATA it_fields_new   TYPE tt_base_fields.
  DATA it_fields_new_t TYPE tt_base_fields.
  DATA l_cl_dd_ddl_field_tracker TYPE REF TO cl_dd_ddl_field_tracker.
  CLEAR l_string.
  LOOP AT it_query INTO DATA(wa_q).
    IF wa_q-str CS '"'. wa_q-str = wa_q-str(sy-fdpos). ENDIF.
    CONCATENATE l_string wa_q-str INTO l_string.
  ENDLOOP.
  TRANSLATE l_string TO UPPER CASE.
  l_len = strlen( l_string ).
  CLEAR l_i.
  DATA l_table TYPE char50.
  DO l_len TIMES.
    l_value = l_string+l_i(1).
    IF l_value = ',' OR l_value = ' ' OR l_value = ')' OR l_value = '(' OR l_value = '.'.
      APPEND wa_table TO it_table. CLEAR wa_table.
      IF l_value = ')' OR l_value = '('.
        wa_table-value = l_value. APPEND wa_table TO it_table. CLEAR wa_table.
      ENDIF.
    ELSE.
      CONCATENATE wa_table-value l_value INTO wa_table-value.
    ENDIF.
    l_i = l_i + 1.
  ENDDO.
  IF wa_table-value IS NOT INITIAL. APPEND wa_table TO it_table. ENDIF.
  DELETE it_table WHERE value = ' '.
  DATA l_exit TYPE flag.
  DATA l_symbol TYPE char10.
  DATA l_as     TYPE char10.
  DATA l_q      TYPE c.
  DATA l_q1     TYPE i.
  DATA l_bras1  TYPE i.
  DATA l_ind    TYPE i.
  DATA l_bet    TYPE i.
  IF l_string CS 'JOIN'.
    READ TABLE it_table INTO wa_table WITH KEY value = 'FROM'.
    IF sy-subrc = 0. l_from = sy-tabix. ENDIF.
    READ TABLE it_table INTO wa_table WITH KEY value = 'INTO'.
    IF sy-subrc = 0. l_into = sy-tabix. ENDIF.
    READ TABLE it_table INTO wa_table WITH KEY value = 'WHERE'.
    IF sy-subrc = 0. l_where = sy-tabix. ENDIF.
    l_from = l_from + 1.
    READ TABLE it_table INTO wa_table INDEX l_from.
    IF sy-subrc = 0. l_table = wa_table-value. wa_table_q-value = l_table. ENDIF.
    l_from = l_from + 1.
    READ TABLE it_table INTO wa_table INDEX l_from.
    IF sy-subrc = 0. l_as = wa_table-value. wa_table_q-as = l_as. ENDIF.
    l_from = l_from + 1.
    READ TABLE it_table INTO wa_table INDEX l_from.
    IF sy-subrc = 0.
      l_symbol = wa_table-value. CONCATENATE l_symbol '~' INTO l_symbol.
      wa_table_q-symbol = l_symbol.
    ENDIF.
    APPEND wa_table_q TO it_table_q.
    LOOP AT it_table INTO wa_table WHERE value CS 'JOIN'.
      DATA(l_t) = sy-tabix + 1.
      READ TABLE it_table INTO DATA(wa_t) INDEX l_t.
      IF sy-subrc = 0. wa_table_q-value = wa_t-value. ENDIF.
      l_t = l_t + 1.
      READ TABLE it_table INTO wa_t INDEX l_t.
      IF sy-subrc = 0. wa_table_q-as = wa_t-value. ENDIF.
      l_t = l_t + 1.
      READ TABLE it_table INTO wa_t INDEX l_t.
      IF sy-subrc = 0.
        wa_table_q-symbol = wa_t-value.
        CONCATENATE wa_table_q-symbol '~' INTO wa_table_q-symbol.
      ENDIF.
      APPEND wa_table_q TO it_table_q.
    ENDLOOP.
    l_from = l_from - 3.
    LOOP AT it_table_q ASSIGNING FIELD-SYMBOL(<fs_table_q>).
      CLEAR l_cl_dd_ddl_field_tracker.
      REFRESH it_fields_new_t.
      SELECT SINGLE * INTO @DATA(l_ars)
        FROM ars_api_succssr
        WHERE object_key = @<fs_table_q>-value AND object_type = 'TABL'.
      IF sy-subrc = 0.
        <fs_table_q>-new_table = l_ars-successor_tadir_obj_name.
        CREATE OBJECT l_cl_dd_ddl_field_tracker
          EXPORTING iv_ddlname = l_ars-successor_tadir_obj_name.
        TRY.
            CALL METHOD l_cl_dd_ddl_field_tracker->get_base_field_information
              RECEIVING rt_base_fields = it_fields_new_t.
          CATCH cx_dd_ddl_read.
        ENDTRY.
        LOOP AT it_fields_new_t INTO DATA(wa_f). APPEND wa_f TO it_fields_new. ENDLOOP.
      ENDIF.
    ENDLOOP.
    DATA l_query TYPE string.
    CLEAR l_query. CLEAR l_q. REFRESH it_value.
    l_q1 = 1.
    LOOP AT it_table INTO wa_table WHERE value CS 'SELECT' OR value CS 'SINGLE'.
      l_q1 = l_q1 + 1.
    ENDLOOP.
    LOOP AT it_table INTO wa_table.
      IF sy-tabix > l_q1. l_q = ','. ENDIF.
      IF sy-tabix = l_from OR sy-tabix = l_into OR sy-tabix = l_where. EXIT. ENDIF.
      IF wa_table-value = 'SELECT' OR wa_table-value = 'SINGLE'.
        CONCATENATE l_query wa_table-value INTO l_query SEPARATED BY space. CONTINUE.
      ENDIF.
      IF wa_table-value = '*'.
        IF l_table <> 'BSEG' AND l_table <> 'SKA1' AND l_table <> 'SKB1'.
          LOOP AT it_fields_new INTO DATA(wa_fn) WHERE is_calculated IS INITIAL.
            CONCATENATE l_query l_q wa_fn-element_name 'AS' wa_fn-base_field
              INTO l_query SEPARATED BY space.
          ENDLOOP.
        ELSE.
          MOVE it_query[] TO it_query_new[]. l_exit = 'X'. EXIT.
        ENDIF.
      ELSE.
        IF wa_table-value CS '~'.
          LOOP AT it_table_q INTO wa_table_q WHERE symbol CS wa_table-value(sy-fdpos).
            l_symbol = wa_table_q-symbol. l_table = wa_table_q-value. EXIT.
          ENDLOOP.
        ENDIF.
        IF wa_table-value CS l_symbol.
          REPLACE l_symbol IN wa_table-value WITH space IGNORING CASE. CONDENSE wa_table-value.
          READ TABLE it_fields_new INTO DATA(wa_fn2) WITH KEY
            base_field = wa_table-value base_object = l_table.
          IF sy-subrc = 0.
            CLEAR l_tab_i.
            READ TABLE it_value INTO wa_value WITH KEY value = wa_table-value.
            IF sy-subrc = 0. l_tab_i = sy-tabix. CONCATENATE wa_table-value l_tab_i INTO wa_table-value. ENDIF.
            CONCATENATE l_symbol wa_fn2-element_name INTO wa_fn2-element_name.
            CONCATENATE l_query l_q wa_fn2-element_name 'AS' wa_table-value
              INTO l_query SEPARATED BY space.
            wa_value = wa_table-value. APPEND wa_value TO it_value.
          ELSE.
            MOVE it_query[] TO it_query_new[]. l_exit = 'X'.
          ENDIF.
        ELSE.
          CONCATENATE l_query l_q wa_table-value INTO l_query SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF l_exit = 'X'. EXIT. ENDIF.
    CONCATENATE l_query 'FROM' INTO l_query SEPARATED BY space.
    LOOP AT it_table INTO wa_table FROM l_from.
      IF sy-tabix = l_into OR sy-tabix = l_where. EXIT. ENDIF.
      READ TABLE it_table_q INTO wa_table_q WITH KEY value = wa_table-value.
      IF sy-subrc = 0. wa_table-value = wa_table_q-new_table.
      ELSE.
        IF wa_table-value CS '~'.
          LOOP AT it_table_q INTO wa_table_q WHERE symbol CS wa_table-value(sy-fdpos).
            l_symbol = wa_table_q-symbol. l_table = wa_table_q-value. EXIT.
          ENDLOOP.
        ENDIF.
      ENDIF.
      IF wa_table-value = 'FROM'. CONTINUE. ENDIF.
      IF wa_table-value CS l_symbol.
        REPLACE ALL OCCURRENCES OF l_symbol IN wa_table-value WITH space IGNORING CASE. CONDENSE wa_table-value.
        READ TABLE it_fields_new INTO DATA(wa_fn3) WITH KEY base_field = wa_table-value base_object = l_table.
        IF sy-subrc = 0. CLEAR wa_table-value. CONCATENATE l_symbol wa_fn3-element_name INTO wa_table-value. ENDIF.
      ENDIF.
      CONCATENATE l_query wa_table-value INTO l_query SEPARATED BY space.
    ENDLOOP.
    CONCATENATE l_query 'INTO' INTO l_query SEPARATED BY space.
    IF l_into > 0.
      CLEAR l_bras.
      LOOP AT it_table INTO wa_table FROM l_into.
        IF sy-tabix = l_from OR sy-tabix = l_where. EXIT. ENDIF.
        IF wa_table-value = 'INTO'. CONTINUE. ENDIF.
        IF wa_table-value <> 'TABLE' AND wa_table-value <> 'FOR' AND wa_table-value <> 'ALL'
          AND wa_table-value <> 'ENTRIES' AND wa_table-value <> 'IN' AND wa_table-value <> 'INTO'
          AND wa_table-value <> 'CORRESPONDING' AND wa_table-value <> 'FIELDS'
          AND wa_table-value <> 'OF' AND wa_table-value <> ')' AND wa_table-value <> '('.
          IF wa_table-value(1) <> '@'. CONCATENATE '@' wa_table-value INTO wa_table-value. ENDIF.
        ENDIF.
        IF wa_table-value = '('. l_bras = ','. ELSEIF wa_table-value = ')'. CLEAR l_bras. ENDIF.
        l_bras1 = sy-tabix + 1.
        READ TABLE it_table INTO DATA(wa_table_b) INDEX l_bras1.
        IF sy-subrc = 0. IF wa_table_b-value = ')'. CLEAR l_bras. ENDIF. ENDIF.
        IF wa_table-value <> '(' AND wa_table-value <> ')'.
          CONCATENATE l_query wa_table-value l_bras INTO l_query SEPARATED BY space.
        ELSE.
          CONCATENATE l_query wa_table-value INTO l_query SEPARATED BY space.
        ENDIF.
      ENDLOOP.
    ENDIF.
    CONCATENATE l_query '.' INTO l_query.
    PERFORM split_string USING l_query '72' ' ' ' ' CHANGING it_query_new.
  ELSE.
    READ TABLE it_table INTO wa_table WITH KEY value = 'FROM'.
    IF sy-subrc = 0. l_from = sy-tabix. ENDIF.
    READ TABLE it_table INTO wa_table WITH KEY value = 'INTO'.
    IF sy-subrc = 0. l_into = sy-tabix. ENDIF.
    READ TABLE it_table INTO wa_table WITH KEY value = 'WHERE'.
    IF sy-subrc = 0. l_where = sy-tabix. ENDIF.
    l_from = l_from + 1.
    READ TABLE it_table INTO wa_table INDEX l_from.
    IF sy-subrc = 0. l_table = wa_table-value. ENDIF.
    l_from = l_from - 1.
    SELECT SINGLE * INTO @DATA(l_ars2)
      FROM ars_api_succssr WHERE object_key = @l_table AND object_type = 'TABL'.
    IF sy-subrc = 0.
      CREATE OBJECT l_cl_dd_ddl_field_tracker EXPORTING iv_ddlname = l_ars2-successor_tadir_obj_name.
      TRY.
          CALL METHOD l_cl_dd_ddl_field_tracker->get_base_field_information
            RECEIVING rt_base_fields = it_fields_new.
        CATCH cx_dd_ddl_read.
      ENDTRY.
    ENDIF.
    CLEAR l_query. CLEAR l_q.
    l_q1 = 1.
    LOOP AT it_table INTO wa_table WHERE value CS 'SELECT' OR value CS 'SINGLE'.
      l_q1 = l_q1 + 1.
    ENDLOOP.
    LOOP AT it_table INTO wa_table.
      IF sy-tabix > l_q1. l_q = ','. ENDIF.
      IF sy-tabix = l_from OR sy-tabix = l_into OR sy-tabix = l_where. EXIT. ENDIF.
      IF wa_table-value = 'SELECT' OR wa_table-value = 'SINGLE'.
        CONCATENATE l_query wa_table-value INTO l_query SEPARATED BY space. CONTINUE.
      ENDIF.
      IF wa_table-value = '*'.
        IF l_table <> 'BSEG' AND l_table <> 'SKA1' AND l_table <> 'SKB1'.
          LOOP AT it_fields_new INTO DATA(wa_fn4) WHERE is_calculated IS INITIAL.
            CONCATENATE l_query l_q wa_fn4-element_name 'AS' wa_fn4-base_field
              INTO l_query SEPARATED BY space.
          ENDLOOP.
        ELSE.
          MOVE it_query[] TO it_query_new[]. l_exit = 'X'. EXIT.
        ENDIF.
      ELSE.
        READ TABLE it_fields_new INTO DATA(wa_fn5) WITH KEY base_field = wa_table-value.
        IF sy-subrc = 0.
          CONCATENATE l_query l_q wa_fn5-element_name 'AS' wa_table-value INTO l_query SEPARATED BY space.
        ELSE.
          LOOP AT it_fields_new INTO DATA(wa_fn6) WHERE base_field CS wa_table-value. EXIT. ENDLOOP.
          IF sy-subrc = 0.
            CONCATENATE l_query l_q wa_fn6-element_name 'AS' wa_table-value INTO l_query SEPARATED BY space.
          ELSE.
            MOVE it_query[] TO it_query_new[]. l_exit = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF l_exit = 'X'. EXIT. ENDIF.
    CONCATENATE l_query 'FROM' l_ars2-successor_tadir_obj_name INTO l_query SEPARATED BY space.
    CONCATENATE l_query 'INTO' INTO l_query SEPARATED BY space.
    IF l_into > 0.
      CLEAR l_bras.
      LOOP AT it_table INTO wa_table FROM l_into.
        IF sy-tabix = l_from OR sy-tabix = l_where. EXIT. ENDIF.
        IF wa_table-value = 'INTO'. CONTINUE. ENDIF.
        IF wa_table-value <> 'TABLE' AND wa_table-value <> 'FOR' AND wa_table-value <> 'ALL'
          AND wa_table-value <> 'ENTRIES' AND wa_table-value <> 'IN' AND wa_table-value <> 'INTO'
          AND wa_table-value <> 'CORRESPONDING' AND wa_table-value <> 'FIELDS'
          AND wa_table-value <> 'OF' AND wa_table-value <> ')' AND wa_table-value <> '('.
          IF wa_table-value(1) <> '@'. CONCATENATE '@' wa_table-value INTO wa_table-value. ENDIF.
        ENDIF.
        IF wa_table-value = '('. l_bras = ','. ELSEIF wa_table-value = ')'. CLEAR l_bras. ENDIF.
        l_bras1 = sy-tabix + 1.
        READ TABLE it_table INTO wa_table_b INDEX l_bras1.
        IF sy-subrc = 0. IF wa_table_b-value = ')'. CLEAR l_bras. ENDIF. ENDIF.
        IF wa_table-value <> '(' AND wa_table-value <> ')'.
          CONCATENATE l_query wa_table-value l_bras INTO l_query SEPARATED BY space.
        ELSE.
          CONCATENATE l_query wa_table-value INTO l_query SEPARATED BY space.
        ENDIF.
      ENDLOOP.
    ENDIF.
    CONCATENATE l_query '.' INTO l_query.
    PERFORM split_string USING l_query '72' ' ' ' ' CHANGING it_query_new.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form change_single
*&---------------------------------------------------------------------*
FORM change_single.
  TYPES: BEGIN OF ty_table, value TYPE char72, END OF ty_table.
  DATA it_table TYPE TABLE OF ty_table.
  DATA wa_table TYPE ty_table.
  DATA l_table  TYPE char50.
  DATA l_string TYPE string.
  DATA l_from   TYPE i.
  DATA l_where  TYPE i.
  DATA l_len    TYPE i.
  DATA l_i      TYPE i.
  DATA l_into   TYPE i.
  DATA l_value  TYPE char1.
  DATA l_value2 TYPE char2.
  CLEAR l_string.
  LOOP AT it_query INTO DATA(wa_q).
    CONCATENATE l_string wa_q-str INTO l_string.
  ENDLOOP.
  TRANSLATE l_string TO UPPER CASE.
  l_len = strlen( l_string ).
  CLEAR l_i.
  DO l_len TIMES.
    l_value = l_string+l_i(1).
    IF l_i < ( l_len - 1 ).
      IF l_value = ')'.
        DATA(l_i1) = l_i - 1.
        l_value2 = l_string+l_i1(2).
        IF l_value2+1(1) <> ' '.
          CONCATENATE wa_table-value l_value INTO wa_table-value.
          l_i = l_i + 1. CONTINUE.
        ENDIF.
      ELSEIF l_value = '('.
        l_value2 = l_string+l_i(2).
        IF l_value2+1(1) <> ' '.
          CONCATENATE wa_table-value l_value INTO wa_table-value.
          l_i = l_i + 1. CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.
    IF l_value = ',' OR l_value = ' ' OR l_value = ')' OR l_value = '('.
      APPEND wa_table TO it_table. CLEAR wa_table.
      IF l_value = ')' OR l_value = '(' OR l_value = ','.
        wa_table-value = l_value. APPEND wa_table TO it_table. CLEAR wa_table.
      ENDIF.
    ELSE.
      CONCATENATE wa_table-value l_value INTO wa_table-value.
    ENDIF.
    l_i = l_i + 1.
  ENDDO.
  IF wa_table-value IS NOT INITIAL. APPEND wa_table TO it_table. ENDIF.
  DELETE it_table WHERE value = ' '.
  READ TABLE it_table INTO wa_table WITH KEY value = 'FROM'.
  IF sy-subrc = 0. l_from = sy-tabix. ENDIF.
  READ TABLE it_table INTO wa_table WITH KEY value = 'INTO'.
  IF sy-subrc = 0. l_into = sy-tabix. ENDIF.
  READ TABLE it_table INTO wa_table WITH KEY value = 'WHERE'.
  IF sy-subrc = 0. l_where = sy-tabix. ENDIF.
  l_from = l_from + 1.
  READ TABLE it_table INTO wa_table INDEX l_from.
  IF sy-subrc = 0. l_table = wa_table-value. ENDIF.
  l_from = l_from - 1.
  DATA l_query TYPE string.
  CLEAR l_query.
  LOOP AT it_table INTO wa_table.
    REPLACE ALL OCCURRENCES OF '.' IN wa_table-value WITH space.
    IF sy-tabix = l_from OR sy-tabix = l_into OR sy-tabix = l_where. EXIT. ENDIF.
    IF wa_table-value = 'SINGLE' OR wa_table-value = 'single'. CONTINUE. ENDIF.
    CONCATENATE l_query wa_table-value INTO l_query SEPARATED BY space.
  ENDLOOP.
  LOOP AT it_table INTO wa_table FROM l_from.
    REPLACE ALL OCCURRENCES OF '.' IN wa_table-value WITH space.
    IF sy-tabix = l_into OR sy-tabix = l_where. EXIT. ENDIF.
    CONCATENATE l_query wa_table-value INTO l_query SEPARATED BY space.
  ENDLOOP.
  CONCATENATE l_query '  UP TO 1 rows' INTO l_query SEPARATED BY space.
  IF l_into > 0.
    LOOP AT it_table INTO wa_table FROM l_into.
      REPLACE ALL OCCURRENCES OF '.' IN wa_table-value WITH space.
      IF sy-tabix = l_from OR sy-tabix = l_where. EXIT. ENDIF.
      CONCATENATE l_query wa_table-value INTO l_query SEPARATED BY space.
    ENDLOOP.
  ENDIF.
  IF l_where > 0.
    LOOP AT it_table INTO wa_table FROM l_where.
      REPLACE ALL OCCURRENCES OF '.' IN wa_table-value WITH space.
      IF sy-tabix = l_from OR sy-tabix = l_into. EXIT. ENDIF.
      CONCATENATE l_query wa_table-value INTO l_query SEPARATED BY space.
    ENDLOOP.
  ENDIF.
  CONCATENATE l_query ' ORDER BY PRIMARY KEY.' INTO l_query SEPARATED BY space.
  CONCATENATE l_query '  ENDSELECT.' INTO l_query SEPARATED BY space.
  PERFORM split_string USING l_query '72' ' ' ' ' CHANGING it_query_new.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form process_read
*&---------------------------------------------------------------------*
FORM process_read.
  TYPES: BEGIN OF ty_table, value TYPE char72, END OF ty_table.
  DATA it_table TYPE TABLE OF ty_table.
  DATA wa_table TYPE ty_table.
  DATA l_table    TYPE char100.
  DATA l_line_new TYPE text255.
  DATA l_fp       TYPE i.
  DATA l_read     TYPE flag.
  DATA(l_line1) = wa_repos_tab-line.
  CONDENSE l_line1.
  CLEAR l_read.
  LOOP AT repos_tab INTO DATA(wa_repos_read) FROM l_tabix.
    IF wa_repos_read-line CS '"'.
      wa_repos_read-line = wa_repos_read-line(sy-fdpos).
      REPLACE ALL OCCURRENCES OF '"' IN wa_repos_read-line WITH space.
      CONDENSE wa_repos_read-line.
    ENDIF.
    l_line1 = wa_repos_read-line.
    CONDENSE l_line1.
    DATA(l_len) = strlen( l_line1 ).
    IF l_line1 CS 'KEY' OR l_line1 CS 'key'.
      l_len = l_len - sy-fdpos.
      l_read = 'X'.
      l_line1 = l_line1+sy-fdpos(l_len).
    ENDIF.
    CONDENSE l_line1.
    REPLACE ALL OCCURRENCES OF 'KEY' IN l_line1 WITH space IGNORING CASE.
    IF l_line1 IS NOT INITIAL.
      DO.
        IF l_line1 CS '='.
          DATA(l_fdpos) = sy-fdpos.
          wa_table-value = l_line1(l_fdpos).
          REPLACE ALL OCCURRENCES OF '@' IN wa_table-value WITH space.
          CONDENSE wa_table-value.
          APPEND wa_table TO it_table.
          REPLACE FIRST OCCURRENCE OF wa_table-value IN l_line1 WITH space IGNORING CASE.
          REPLACE FIRST OCCURRENCE OF '=' IN l_line1 WITH space.
          CONDENSE l_line1.
          REPLACE FIRST OCCURRENCE OF ' ' IN l_line1 WITH '@'.
          CLEAR wa_table-value.
          l_fp = l_fdpos + 1.
          IF l_line1 CA ' '. l_fp = sy-fdpos + 1. ENDIF.
          l_len = strlen( l_line1 ).
          l_len = l_len - l_fp.
          IF l_len < 1. EXIT. ENDIF.
          l_line1 = l_line1+l_fp(l_len).
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.
    IF wa_repos_read-line CS '.'. EXIT. ENDIF.
  ENDLOOP.
  l_line1 = wa_repos_tab-line.
  DATA l_tab2 TYPE sy-index.
  REPLACE ALL OCCURRENCES OF 'READ TABLE' IN l_line1 WITH space IGNORING CASE.
  CONDENSE l_line1.
  l_len = strlen( l_line1 ).
  DO l_len TIMES.
    DATA(l_value) = l_line1+l_tab2(1).
    IF l_value = ' '. EXIT.
    ELSE. CONCATENATE l_table l_value INTO l_table.
    ENDIF.
    l_tab2 = l_tab2 + 1.
  ENDDO.
  CONCATENATE 'SORT' l_table 'BY' INTO l_line_new SEPARATED BY space.
  LOOP AT it_table INTO wa_table.
    CONCATENATE l_line_new wa_table-value INTO l_line_new SEPARATED BY space.
  ENDLOOP.
  CONCATENATE l_line_new '.' INTO l_line_new SEPARATED BY space.
  CLEAR l_tab2.
  DESCRIBE TABLE repos_tab_new LINES DATA(l_ind).
  l_fp = l_ind.
  DATA l_cont TYPE flag.
  DO l_ind TIMES.
    READ TABLE repos_tab_new INTO DATA(wa_rep) INDEX l_fp.
    IF sy-subrc = 0.
      TRANSLATE wa_rep-line TO UPPER CASE.
      IF wa_rep-line CS 'ENDLOOP'. DATA(l_cont2) = 'X'. l_fp = l_fp - 1. CONTINUE. ENDIF.
      IF wa_rep-line CS 'LOOP'.
        IF l_cont IS INITIAL. EXIT.
        ELSE. CLEAR l_cont. ENDIF.
      ENDIF.
    ELSE. EXIT.
    ENDIF.
    l_fp = l_fp - 1.
  ENDDO.
  DATA it_repos TYPE STANDARD TABLE OF abaptxt255.
  MOVE repos_tab_new[] TO it_repos.
  REFRESH repos_tab_new.
  LOOP AT it_repos INTO wa_rep.
    IF sy-tabix = l_fp.
      CLEAR wa_blank.
      CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
        INTO wa_blank-line SEPARATED BY space.
      APPEND wa_blank TO repos_tab_new.
      APPEND l_line_new TO repos_tab_new.
      CLEAR wa_blank.
      CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
        INTO wa_blank-line SEPARATED BY space.
      APPEND wa_blank TO repos_tab_new.
      CLEAR wa_blank.
      APPEND wa_rep TO repos_tab_new.
    ELSE.
      APPEND wa_rep TO repos_tab_new.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form process_change_loop
*&---------------------------------------------------------------------*
FORM process_change_loop.
  TYPES: BEGIN OF ty_table, value TYPE char72, END OF ty_table.
  DATA it_table   TYPE TABLE OF ty_table.
  DATA wa_table   TYPE ty_table.
  DATA l_table    TYPE char100.
  DATA l_line_new TYPE text255.
  DATA l_line1    TYPE text255.
  DATA l_field    TYPE text255.
  DATA l_value    TYPE c.
  DATA l_ind      TYPE sy-tabix VALUE 1.
  DATA l_fp       TYPE sy-fdpos.
  DATA l_tab      TYPE sy-index.
  DATA l_cont     TYPE flag.
  DATA l_cont1    TYPE i.
  l_line1 = wa_repos_tab-line.
  REPLACE ALL OCCURRENCES OF 'ON CHANGE OF' IN l_line1 WITH space IGNORING CASE.
  REPLACE ALL OCCURRENCES OF '.' IN l_line1 WITH space.
  CONDENSE l_line1.
  CLEAR: l_tab,l_value.
  DATA(l_len) = strlen( l_line1 ).
  CLEAR l_cont.
  DO l_len TIMES.
    l_value = l_line1+l_tab(1).
    IF l_value = '-'. l_cont = 'X'. l_tab = l_tab + 1. CONTINUE. ENDIF.
    IF l_cont = 'X'.
      IF l_value = ' '.
        CLEAR l_cont. wa_table-value = l_field. APPEND wa_table TO it_table.
        CLEAR l_field. l_tab = l_tab + 1. CONTINUE.
      ELSE.
        CONCATENATE l_field l_value INTO l_field.
      ENDIF.
    ENDIF.
    l_tab = l_tab + 1.
  ENDDO.
  IF l_field IS NOT INITIAL. wa_table-value = l_field. APPEND wa_table TO it_table. ENDIF.
  CLEAR: l_tab,l_value.
  CLEAR l_ind.
  DESCRIBE TABLE repos_tab_new LINES l_ind.
  l_fp = l_ind.
  DO l_ind TIMES.
    READ TABLE repos_tab_new INTO DATA(wa_rep) INDEX l_fp.
    IF sy-subrc = 0.
      TRANSLATE wa_rep-line TO UPPER CASE.
      IF wa_rep-line CS 'ENDLOOP'. l_cont1 = l_cont1 + 1. l_fp = l_fp - 1. CONTINUE. ENDIF.
      IF wa_rep-line CS 'LOOP'.
        IF l_cont1 IS INITIAL. EXIT.
        ELSE. l_cont1 = l_cont1 - 1. ENDIF.
      ENDIF.
    ELSE. EXIT.
    ENDIF.
    l_fp = l_fp - 1.
  ENDDO.
  CLEAR: l_tab,l_value.
  REPLACE ALL OCCURRENCES OF 'LOOP AT' IN wa_rep-line WITH space IGNORING CASE.
  CONDENSE wa_rep.
  l_len = strlen( wa_rep-line ).
  DO l_len TIMES.
    l_value = wa_rep-line+l_tab(1).
    IF l_value = ' '. EXIT.
    ELSE. CONCATENATE l_table l_value INTO l_table. ENDIF.
    l_tab = l_tab + 1.
  ENDDO.
  CONCATENATE 'SORT' l_table 'BY' INTO l_line_new SEPARATED BY space.
  LOOP AT it_table INTO wa_table.
    CONCATENATE l_line_new wa_table-value INTO l_line_new SEPARATED BY space.
  ENDLOOP.
  CONCATENATE l_line_new '.' INTO l_line_new SEPARATED BY space.
  DATA it_repos TYPE STANDARD TABLE OF abaptxt255.
  MOVE repos_tab_new[] TO it_repos.
  REFRESH repos_tab_new.
  LOOP AT it_repos INTO wa_rep.
    IF sy-tabix = l_fp.
      CLEAR wa_blank.
      CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
        INTO wa_blank-line SEPARATED BY space.
      APPEND wa_blank TO repos_tab_new.
      APPEND l_line_new TO repos_tab_new.
      CLEAR wa_blank.
      CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
        INTO wa_blank-line SEPARATED BY space.
      APPEND wa_blank TO repos_tab_new.
      CLEAR wa_blank.
      APPEND wa_rep TO repos_tab_new.
    ELSE.
      APPEND wa_rep TO repos_tab_new.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form endat
*&---------------------------------------------------------------------*
FORM endat.
  TYPES: BEGIN OF ty_table, value TYPE char72, END OF ty_table.
  DATA it_table   TYPE TABLE OF ty_table.
  DATA wa_table   TYPE ty_table.
  DATA l_table    TYPE char100.
  DATA l_line_new TYPE text255.
  DATA l_line1    TYPE text255.
  DATA l_field    TYPE text255.
  DATA l_value    TYPE c.
  DATA l_ind      TYPE sy-tabix VALUE 1.
  DATA l_fp       TYPE sy-fdpos.
  DATA l_tab      TYPE sy-index.
  DATA l_cont     TYPE i.
  l_line1 = wa_repos_tab-line.
  REPLACE ALL OCCURRENCES OF 'AT NEW' IN l_line1 WITH space IGNORING CASE.
  REPLACE ALL OCCURRENCES OF 'AT END OF' IN l_line1 WITH space IGNORING CASE.
  REPLACE ALL OCCURRENCES OF '.' IN l_line1 WITH space.
  CONDENSE l_line1.
  CLEAR: l_tab,l_value.
  DATA(l_len) = strlen( l_line1 ).
  DO l_len TIMES.
    l_value = l_line1+l_tab(1).
    IF l_value = ' '. EXIT.
    ELSE. CONCATENATE l_field l_value INTO l_field. ENDIF.
    l_tab = l_tab + 1.
  ENDDO.
  CLEAR: l_tab,l_value. CLEAR l_ind.
  DESCRIBE TABLE repos_tab_new LINES l_ind.
  l_fp = l_ind.
  DO l_ind TIMES.
    READ TABLE repos_tab_new INTO DATA(wa_rep) INDEX l_fp.
    IF sy-subrc = 0.
      TRANSLATE wa_rep-line TO UPPER CASE.
      IF wa_rep-line CS 'ENDLOOP'. l_cont = l_cont + 1. l_fp = l_fp - 1. CONTINUE. ENDIF.
      IF wa_rep-line CS 'LOOP'.
        IF l_cont IS INITIAL. EXIT.
        ELSE. l_cont = l_cont - 1. ENDIF.
      ENDIF.
    ELSE. EXIT.
    ENDIF.
    l_fp = l_fp - 1.
  ENDDO.
  CLEAR: l_tab,l_value.
  REPLACE ALL OCCURRENCES OF 'LOOP AT' IN wa_rep-line WITH space IGNORING CASE.
  CONDENSE wa_rep.
  l_len = strlen( wa_rep-line ).
  DO l_len TIMES.
    l_value = wa_rep-line+l_tab(1).
    IF l_value = ' '. EXIT.
    ELSE. CONCATENATE l_table l_value INTO l_table. ENDIF.
    l_tab = l_tab + 1.
  ENDDO.
  CONCATENATE 'SORT' l_table 'BY' l_field '.' INTO l_line_new SEPARATED BY space.
  DATA it_repos TYPE STANDARD TABLE OF abaptxt255.
  MOVE repos_tab_new[] TO it_repos.
  REFRESH repos_tab_new.
  LOOP AT it_repos INTO wa_rep.
    IF sy-tabix = l_fp.
      CLEAR wa_blank.
      CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
        INTO wa_blank-line SEPARATED BY space.
      APPEND wa_blank TO repos_tab_new.
      APPEND l_line_new TO repos_tab_new.
      CLEAR wa_blank.
      CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
        INTO wa_blank-line SEPARATED BY space.
      APPEND wa_blank TO repos_tab_new.
      CLEAR wa_blank.
      APPEND wa_rep TO repos_tab_new.
    ELSE.
      APPEND wa_rep TO repos_tab_new.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form loop_exit
*&---------------------------------------------------------------------*
FORM loop_exit.
  TYPES: BEGIN OF ty_table, value TYPE char72, END OF ty_table.
  DATA l_table    TYPE char100.
  DATA l_line_new TYPE text255.
  DATA l_value    TYPE c.
  DATA l_fp       TYPE sy-fdpos.
  DATA l_tab      TYPE sy-index.
  DATA l_cont1    TYPE i.
  DATA l_exit     TYPE flag.
  CLEAR: l_tab,l_value.
  CLEAR l_fp.
  DESCRIBE TABLE repos_tab_new LINES DATA(l_ind).
  l_fp = l_ind.
  DO l_ind TIMES.
    READ TABLE repos_tab_new INTO DATA(wa_rep) INDEX l_fp.
    IF sy-subrc = 0.
      TRANSLATE wa_rep-line TO UPPER CASE.
      IF wa_rep-line CS 'ENDLOOP'. l_cont1 = l_cont1 + 1. l_fp = l_fp - 1. CONTINUE. ENDIF.
      IF wa_rep-line CS 'LOOP'.
        IF l_cont1 IS INITIAL. EXIT.
        ELSE. l_cont1 = l_cont1 - 1. ENDIF.
      ENDIF.
    ELSE. EXIT.
    ENDIF.
    l_fp = l_fp - 1.
  ENDDO.
  CLEAR: l_tab,l_value.
  REPLACE ALL OCCURRENCES OF 'LOOP AT' IN wa_rep-line WITH space IGNORING CASE.
  CONDENSE wa_rep.
  DATA(l_len) = strlen( wa_rep-line ).
  DO l_len TIMES.
    l_value = wa_rep-line+l_tab(1).
    IF l_value = ' '. EXIT.
    ELSE. CONCATENATE l_table l_value INTO l_table. ENDIF.
    l_tab = l_tab + 1.
  ENDDO.
  CONCATENATE 'SORT' l_table '.' INTO l_line_new SEPARATED BY space.
  DATA(l_fp1) = l_fp - 1.
  DO 20 TIMES.
    READ TABLE repos_tab_new INTO wa_rep INDEX l_fp1.
    IF sy-subrc = 0.
      IF wa_rep-line = l_line_new. l_exit = 'X'. EXIT. ENDIF.
    ELSE. EXIT.
    ENDIF.
    l_fp1 = l_fp1 - 1.
  ENDDO.
  IF l_exit = 'X'. EXIT. ENDIF.
  DATA it_repos TYPE STANDARD TABLE OF abaptxt255.
  MOVE repos_tab_new[] TO it_repos.
  REFRESH repos_tab_new.
  LOOP AT it_repos INTO wa_rep.
    IF sy-tabix = l_fp.
      CLEAR wa_blank.
      CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
        INTO wa_blank-line SEPARATED BY space.
      APPEND wa_blank TO repos_tab_new.
      APPEND l_line_new TO repos_tab_new.
      CLEAR wa_blank.
      CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
        INTO wa_blank-line SEPARATED BY space.
      APPEND wa_blank TO repos_tab_new.
      CLEAR wa_blank.
      APPEND wa_rep TO repos_tab_new.
    ELSE.
      APPEND wa_rep TO repos_tab_new.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form split_string
*&---------------------------------------------------------------------*
FORM split_string
  USING input_string           TYPE string
        max_component_length   TYPE i
        terminating_separators TYPE string
        opening_separators     TYPE string
  CHANGING
        string_components      TYPE ty_swastrtab.
  DATA: total_length     TYPE i,
        remaining_length TYPE i,
        current_offset   TYPE i,
        current_char     TYPE c,
        search_position  TYPE i,
        component_length TYPE i.
  DATA: current_string TYPE swastrtab.
  DATA l_curr TYPE i.
  CHECK NOT input_string IS INITIAL.
  DESCRIBE FIELD current_string-str LENGTH total_length IN CHARACTER MODE.
  IF max_component_length > total_length.
    MESSAGE ID 'W8' TYPE 'S' NUMBER 602
      WITH total_length RAISING max_component_length_invalid.
  ELSEIF max_component_length <= 0.
    max_component_length = 100.
  ENDIF.
  IF terminating_separators IS INITIAL.
    terminating_separators = ' >=)].,;:?!'.
  ENDIF.
  IF opening_separators IS INITIAL.
    opening_separators = '<(['.
  ENDIF.
  total_length     = strlen( input_string ).
  remaining_length = total_length.
  current_offset   = 0.
  CLEAR string_components[].
  WHILE remaining_length >= max_component_length.
    search_position  = current_offset + max_component_length - 1.
    component_length = max_component_length.
    WHILE search_position >= current_offset.
      current_char = input_string+search_position(1).
      IF terminating_separators CA current_char. EXIT. ENDIF.
      SUBTRACT 1 FROM component_length.
      IF ( opening_separators CA current_char ).
        l_curr = search_position + 1.
        DATA(l_var) = input_string+l_curr(1).
        IF l_var IS INITIAL. EXIT. ENDIF.
      ENDIF.
      SUBTRACT 1 FROM search_position.
    ENDWHILE.
    IF component_length = 0. component_length = max_component_length. ENDIF.
    current_string-len = component_length.
    current_string-str = input_string+current_offset(component_length).
    APPEND current_string TO string_components.
    ADD component_length TO current_offset.
    SUBTRACT component_length FROM remaining_length.
  ENDWHILE.
  IF remaining_length > 0.
    CLEAR current_string.
    current_string-len = remaining_length.
    current_string-str = input_string+current_offset(remaining_length).
    APPEND current_string TO string_components.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form zatc_process_all
*&---------------------------------------------------------------------*
FORM zatc_process_all.
  SELECT * INTO TABLE it_zatc_process_all FROM zatc_process_all.
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
*& Form amount_conv
*&---------------------------------------------------------------------*
FORM amount_conv.
  IF wa_final-param3 = 'FUNC'.
    DATA l_flag  TYPE flag.
    DATA l_v     TYPE c.
    DATA l_param TYPE char200.
    DATA l_flag1 TYPE flag.
    DATA l_t     TYPE i.
    SELECT * INTO TABLE @DATA(it_fupararef)
      FROM fupararef WHERE funcname = @wa_final-param2.
    IF sy-subrc = 0.
      LOOP AT it_fupararef INTO DATA(wa_fupararef).
        IF wa_fupararef-structure CS '-'. CONTINUE. ENDIF.
        SELECT * INTO TABLE @DATA(it_dd03l)
          FROM dd03l
          WHERE tabname  = @wa_fupararef-structure
            AND datatype = 'DEC'.
        IF sy-subrc = 0.
          CLEAR l_flag.
          LOOP AT repos_tab INTO DATA(l_repos) WHERE line CS wa_fupararef-parameter.
            IF l_repos-line CS '='. l_flag = 'X'. EXIT. ENDIF.
          ENDLOOP.
          IF l_flag = 'X'.
            REPLACE ALL OCCURRENCES OF wa_fupararef-parameter IN l_repos-line WITH space IGNORING CASE.
            REPLACE ALL OCCURRENCES OF '=' IN l_repos-line WITH space IGNORING CASE.
          ENDIF.
          CONDENSE l_repos-line.
          DATA(l_line) = strlen( l_repos-line ).
          CLEAR : l_t,l_v,l_param.
          DO l_line TIMES.
            l_v = l_repos-line+l_t(1).
            IF l_v = ' '. EXIT.
            ELSE. CONCATENATE l_param l_v INTO l_param. ENDIF.
            l_t = l_t + 1.
          ENDDO.
          CLEAR l_flag.
          LOOP AT repos_tab_new INTO DATA(l_repos1) WHERE line CS l_param.
            IF l_repos1-line CS 'append' OR l_repos1-line CS 'APPEND'.
              l_flag = 'X'. EXIT.
            ENDIF.
          ENDLOOP.
          IF l_flag = 'X'.
            REPLACE ALL OCCURRENCES OF 'APPEND' IN l_repos1-line WITH space IGNORING CASE.
            CONDENSE l_repos1-line.
            l_line = strlen( l_repos1-line ).
            CLEAR : l_t,l_v,l_param.
            DO l_line TIMES.
              l_v = l_repos1-line+l_t(1).
              IF l_v = ' '. EXIT.
              ELSE. CONCATENATE l_param l_v INTO l_param. ENDIF.
              l_t = l_t + 1.
            ENDDO.
            DATA(l_param2) = l_param.
            CLEAR l_param.
            LOOP AT it_dd03l INTO DATA(wa_dd03l).
              CONCATENATE l_param2 '-' wa_dd03l-fieldname INTO DATA(l_param1).
              DESCRIBE TABLE repos_tab_new LINES DATA(l_line1).
              DATA(l_tabix) = l_line1.
              DO l_line1 TIMES.
                READ TABLE repos_tab_new INTO l_repos1 INDEX l_tabix.
                IF l_repos1-line CS l_param1 AND l_repos1-line CS '='.
                  IF l_repos1-line CS 'CONV'. ELSE. EXIT. ENDIF.
                ENDIF.
                l_tabix = l_tabix - 1.
              ENDDO.
              IF l_repos1-line CS 'CONV'. EXIT. ENDIF.
              IF l_tabix IS NOT INITIAL.
                l_line = strlen( l_repos1-line ).
                CLEAR : l_t,l_v,l_param,l_flag1.
                DO l_line TIMES.
                  l_v = l_repos1-line+l_t(1).
                  IF l_v = '='.
                    CONCATENATE l_param l_v INTO l_param SEPARATED BY space.
                    CONCATENATE l_param 'CONV' INTO l_param SEPARATED BY space.
                    CONCATENATE wa_dd03l-domname '(' INTO wa_dd03l-fieldname.
                    CONCATENATE l_param wa_dd03l-fieldname INTO l_param SEPARATED BY space.
                    DO.
                      l_t = l_t + 1. l_v = l_repos1-line+l_t(1).
                      CONCATENATE l_param l_v INTO l_param SEPARATED BY space.
                      IF l_v <> ' '. EXIT. ENDIF.
                    ENDDO.
                    l_flag1 = 'X'.
                  ELSEIF ( l_v = ' ' OR l_v = '.' ) AND l_flag1 = 'X'.
                    IF l_v = ' '.
                      CONCATENATE l_param l_v INTO l_param.
                      CONCATENATE l_param ')' INTO l_param SEPARATED BY space.
                    ELSE.
                      CONCATENATE l_param ')' INTO l_param SEPARATED BY space.
                      CONCATENATE l_param '.' INTO l_param SEPARATED BY space.
                    ENDIF.
                    CLEAR l_flag1.
                  ELSE.
                    CONCATENATE l_param l_v INTO l_param.
                  ENDIF.
                  l_t = l_t + 1.
                ENDDO.
                DATA it_repos TYPE STANDARD TABLE OF abaptxt255.
                MOVE repos_tab_new[] TO it_repos.
                REFRESH repos_tab_new.
                LOOP AT it_repos INTO DATA(wa_rep).
                  IF sy-tabix = l_tabix.
                    CLEAR wa_blank.
                    CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
                      INTO wa_blank-line SEPARATED BY space.
                    APPEND wa_blank TO repos_tab_new.
                    APPEND l_param TO repos_tab_new.
                    CLEAR wa_blank.
                    CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
                      INTO wa_blank-line SEPARATED BY space.
                    APPEND wa_blank TO repos_tab_new.
                    CLEAR wa_blank.
                  ELSE.
                    APPEND wa_rep TO repos_tab_new.
                  ENDIF.
                ENDLOOP.
                CLEAR l_param. CLEAR l_param1.
              ENDIF.
              CLEAR l_tabix.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form material_conv
*&---------------------------------------------------------------------*
FORM material_conv.
  IF wa_final-param3 = 'FUNC'.
    DATA l_flag  TYPE flag.
    DATA l_v     TYPE c.
    DATA l_param TYPE char200.
    DATA l_flag1 TYPE flag.
    DATA l_t     TYPE i.
    SELECT * INTO TABLE @DATA(it_fupararef)
      FROM fupararef WHERE funcname = @wa_final-param2.
    IF sy-subrc = 0.
      LOOP AT it_fupararef INTO DATA(wa_fupararef).
        IF wa_fupararef-structure CS '-'. CONTINUE. ENDIF.
        SELECT * INTO TABLE @DATA(it_dd03l)
          FROM dd03l
          WHERE tabname  = @wa_fupararef-structure
            AND ( domname = 'MATNR40' OR domname = 'MATNR18' ).
        IF sy-subrc = 0.
          CLEAR l_flag.
          LOOP AT repos_tab INTO DATA(l_repos) WHERE line CS wa_fupararef-parameter.
            IF l_repos-line CS '='. l_flag = 'X'. EXIT. ENDIF.
          ENDLOOP.
          IF l_flag = 'X'.
            REPLACE ALL OCCURRENCES OF wa_fupararef-parameter IN l_repos-line WITH space IGNORING CASE.
            REPLACE ALL OCCURRENCES OF '=' IN l_repos-line WITH space IGNORING CASE.
          ENDIF.
          CONDENSE l_repos-line.
          DATA(l_line) = strlen( l_repos-line ).
          CLEAR : l_t,l_v,l_param.
          DO l_line TIMES.
            l_v = l_repos-line+l_t(1).
            IF l_v = ' '. EXIT.
            ELSE. CONCATENATE l_param l_v INTO l_param. ENDIF.
            l_t = l_t + 1.
          ENDDO.
          CLEAR l_flag.
          LOOP AT repos_tab_new INTO DATA(l_repos1) WHERE line CS l_param.
            IF l_repos1-line CS 'append' OR l_repos1-line CS 'APPEND'.
              l_flag = 'X'. EXIT.
            ENDIF.
          ENDLOOP.
          IF l_flag = 'X'.
            REPLACE ALL OCCURRENCES OF 'APPEND' IN l_repos1-line WITH space IGNORING CASE.
            CONDENSE l_repos1-line.
            l_line = strlen( l_repos1-line ).
            CLEAR : l_t,l_v,l_param.
            DO l_line TIMES.
              l_v = l_repos1-line+l_t(1).
              IF l_v = ' '. EXIT.
              ELSE. CONCATENATE l_param l_v INTO l_param. ENDIF.
              l_t = l_t + 1.
            ENDDO.
            DATA(l_param2) = l_param.
            CLEAR l_param.
            LOOP AT it_dd03l INTO DATA(wa_dd03l).
              CONCATENATE l_param2 '-' wa_dd03l-fieldname INTO DATA(l_param1).
              DESCRIBE TABLE repos_tab_new LINES DATA(l_line1).
              DATA(l_tabix) = l_line1.
              DO l_line1 TIMES.
                READ TABLE repos_tab_new INTO l_repos1 INDEX l_tabix.
                IF l_repos1-line CS l_param1 AND l_repos1-line CS '='.
                  IF l_repos1-line CS 'CONV'. ELSE. EXIT. ENDIF.
                ENDIF.
                l_tabix = l_tabix - 1.
              ENDDO.
              IF l_repos1-line CS 'CONV'. EXIT. ENDIF.
              IF l_tabix IS NOT INITIAL.
                l_line = strlen( l_repos1-line ).
                CLEAR : l_t,l_v,l_param,l_flag1.
                DO l_line TIMES.
                  l_v = l_repos1-line+l_t(1).
                  IF l_v = '='.
                    CONCATENATE l_param l_v INTO l_param SEPARATED BY space.
                    CONCATENATE l_param 'CONV' INTO l_param SEPARATED BY space.
                    CONCATENATE wa_dd03l-domname '(' INTO wa_dd03l-fieldname.
                    CONCATENATE l_param wa_dd03l-fieldname INTO l_param SEPARATED BY space.
                    DO.
                      l_t = l_t + 1. l_v = l_repos1-line+l_t(1).
                      CONCATENATE l_param l_v INTO l_param SEPARATED BY space.
                      IF l_v <> ' '. EXIT. ENDIF.
                    ENDDO.
                    l_flag1 = 'X'.
                  ELSEIF ( l_v = ' ' OR l_v = '.' ) AND l_flag1 = 'X'.
                    IF l_v = ' '.
                      CONCATENATE l_param l_v INTO l_param.
                      CONCATENATE l_param ')' INTO l_param SEPARATED BY space.
                    ELSE.
                      CONCATENATE l_param ')' INTO l_param SEPARATED BY space.
                      CONCATENATE l_param '.' INTO l_param SEPARATED BY space.
                    ENDIF.
                    CLEAR l_flag1.
                  ELSE.
                    CONCATENATE l_param l_v INTO l_param.
                  ENDIF.
                  l_t = l_t + 1.
                ENDDO.
                DATA it_repos TYPE STANDARD TABLE OF abaptxt255.
                MOVE repos_tab_new[] TO it_repos.
                REFRESH repos_tab_new.
                LOOP AT it_repos INTO DATA(wa_rep).
                  IF sy-tabix = l_tabix.
                    CLEAR wa_blank.
                    CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
                      INTO wa_blank-line SEPARATED BY space.
                    APPEND wa_blank TO repos_tab_new.
                    APPEND l_param TO repos_tab_new.
                    CLEAR wa_blank.
                    CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
                      INTO wa_blank-line SEPARATED BY space.
                    APPEND wa_blank TO repos_tab_new.
                    CLEAR wa_blank.
                  ELSE.
                    APPEND wa_rep TO repos_tab_new.
                  ENDIF.
                ENDLOOP.
                CLEAR l_param. CLEAR l_param1.
              ENDIF.
              CLEAR l_tabix.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form replace_bp
*&---------------------------------------------------------------------*
FORM replace_bp.
  DATA l_value TYPE char30.
  DESCRIBE TABLE repos_tab_new LINES DATA(l_line).
  DATA(l_line1) = l_line.
  DO 10 TIMES.
    READ TABLE repos_tab_new INTO DATA(wa_repos) INDEX l_line1.
    IF sy-subrc = 0.
      IF wa_repos-line CS 'SET'.
        REPLACE ALL OCCURRENCES OF 'SET PARAMETER ID' IN wa_repos-line WITH space IGNORING CASE.
        CONDENSE wa_repos-line.
        DATA(l_line2) = strlen( wa_repos-line ).
        DATA(l_v) = l_line2.
        l_v = 1.
        CLEAR l_value.
        DO l_line2 TIMES.
          DATA(l_c) = wa_repos-line+l_v(1).
          IF l_c = ' '. EXIT.
          ELSE. CONCATENATE l_value l_c INTO l_value. ENDIF.
          l_v = l_v + 1.
        ENDDO.
        REPLACE ALL OCCURRENCES OF TEXT-001 IN l_value WITH space.
        CONDENSE l_value.
        SELECT SINGLE * INTO @DATA(l_tparat)
          FROM tparat
          WHERE sprache = @sy-langu AND paramid = @l_value.
        IF sy-subrc = 0.
          IF l_tparat-partext CS 'VENDOR' OR l_tparat-partext CS 'CUSTOMER'.
            REPLACE ALL OCCURRENCES OF l_value IN wa_repos-line WITH space IGNORING CASE.
            REPLACE ALL OCCURRENCES OF 'FIELD' IN wa_repos-line WITH space IGNORING CASE.
            DATA(l_field) = wa_repos-line.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE. EXIT.
    ENDIF.
    l_line1 = l_line1 - 1.
  ENDDO.
  REPLACE ALL OCCURRENCES OF TEXT-001 IN l_field WITH space.
  REPLACE ALL OCCURRENCES OF '.' IN l_field WITH space.
  CONDENSE l_field.
  IF l_tparat IS NOT INITIAL AND l_field IS NOT INITIAL.
    CLEAR wa_blank.
    CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
      INTO wa_blank-line SEPARATED BY space.
    APPEND wa_blank TO repos_tab_new.
    CLEAR wa_blank.
    CONCATENATE '*' wa_repos_tab-line INTO wa_blank-line SEPARATED BY space.
    APPEND wa_blank TO repos_tab_new.
    CLEAR wa_blank.
    IF l_tparat-partext CS 'VENDOR'.
      wa_blank-line = 'SELECT  partner INTO @DATA(l_partner)'.
      APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
      wa_blank-line = 'UP TO 1 ROWS'. APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
      wa_blank-line = 'FROM v_cvi_vend_link'. APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
      wa_blank-line = 'WHERE'. APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
      IF l_field(1) <> '@'. CONCATENATE '@' l_field INTO l_field. ENDIF.
      CONCATENATE 'LIFNR' '=' l_field 'ORDER BY PRIMARY KEY.'
        INTO wa_blank-line SEPARATED BY space.
      APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    ELSE.
      wa_blank-line = 'SELECT  partner INTO @DATA(l_partner)'.
      APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
      wa_blank-line = 'UP TO 1 ROWS'. APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
      wa_blank-line = 'FROM V_CVI_CUST_LINK'. APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
      wa_blank-line = 'WHERE'. APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
      IF l_field <> '@'. CONCATENATE '@' l_field INTO l_field. ENDIF.
      CONCATENATE 'KUNNR' '=' l_field 'ORDER BY PRIMARY KEY.'
        INTO wa_blank-line SEPARATED BY space.
      APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    ENDIF.
    wa_blank-line = 'DATA(request) = NEW cl_bupa_navigation_request( ).'.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = 'request->set_partner_number( L_PARTNER ).'.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = 'CALL METHOD request->set_bupa_activity'.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = 'EXPORTING'. APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = 'iv_value = request->gc_activity_display.'.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = 'DATA(options) = NEW cl_bupa_dialog_joel_options( ).'.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = 'options->set_navigation_disabled( abap_true ).'.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = 'cl_bupa_dialog_joel=>start_with_navigation( iv_request = request'.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = '                                             iv_options = options ).'.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = 'ENDSELECT.'. APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
      INTO wa_blank-line SEPARATED BY space.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
  ELSE.
    APPEND wa_repos_tab TO repos_tab_new.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form replace_migo
*&---------------------------------------------------------------------*
FORM replace_migo.
  DATA l_gjahr TYPE gjahr.
  DATA l_mblnr TYPE char100.
  DATA l_value TYPE char100.
  DESCRIBE TABLE repos_tab_new LINES DATA(l_line).
  DATA(l_line1) = l_line.
  DO 10 TIMES.
    READ TABLE repos_tab_new INTO DATA(wa_repos) INDEX l_line1.
    IF sy-subrc = 0.
      IF wa_repos-line CS 'SET'.
        REPLACE ALL OCCURRENCES OF 'SET PARAMETER ID' IN wa_repos-line WITH space IGNORING CASE.
        CONDENSE wa_repos-line.
        DATA(l_line2) = strlen( wa_repos-line ).
        DATA(l_v) = l_line2. l_v = 1. CLEAR l_value.
        DO l_line2 TIMES.
          DATA(l_c) = wa_repos-line+l_v(1).
          IF l_c = ' '. EXIT.
          ELSE. CONCATENATE l_value l_c INTO l_value. ENDIF.
          l_v = l_v + 1.
        ENDDO.
        REPLACE ALL OCCURRENCES OF TEXT-001 IN l_value WITH space. CONDENSE l_value.
        SELECT SINGLE * INTO @DATA(l_tparat)
          FROM tparat WHERE sprache = @sy-langu AND paramid = @l_value.
        IF sy-subrc = 0.
          IF l_tparat-partext CS 'Material doc. year'.
            REPLACE ALL OCCURRENCES OF l_value IN wa_repos-line WITH space IGNORING CASE.
            REPLACE ALL OCCURRENCES OF 'FIELD' IN wa_repos-line WITH space IGNORING CASE.
            l_gjahr = wa_repos-line.
          ELSEIF l_tparat-partext CS 'Material document number'.
            REPLACE ALL OCCURRENCES OF l_value IN wa_repos-line WITH space IGNORING CASE.
            REPLACE ALL OCCURRENCES OF 'FIELD' IN wa_repos-line WITH space IGNORING CASE.
            l_mblnr = wa_repos-line.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE. EXIT.
    ENDIF.
    IF l_mblnr IS NOT INITIAL AND l_gjahr IS NOT INITIAL. EXIT. ENDIF.
    l_line1 = l_line1 - 1.
  ENDDO.
  IF l_mblnr IS NOT INITIAL AND l_gjahr IS NOT INITIAL.
    CLEAR wa_blank.
    CONCATENATE '"' p_rem p_begin sy-uname l_datum ' for ATC '
      INTO wa_blank-line SEPARATED BY space.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    CONCATENATE '*' wa_repos_tab-line INTO wa_blank-line SEPARATED BY space.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    CONCATENATE 'data l_gjahr type majhr' '.' INTO wa_blank-line SEPARATED BY space.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    CONCATENATE 'data l_mblnr TYPE mblnr value' '.' INTO wa_blank-line SEPARATED BY space.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    CONCATENATE 'l_mblnr' '=' l_mblnr INTO wa_blank-line SEPARATED BY space.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    CONCATENATE 'l_gjahr' '=' l_gjahr INTO wa_blank-line SEPARATED BY space.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    CONCATENATE TEXT-001 'MIGO_DIALOG' TEXT-001 INTO DATA(l_func).
    CONCATENATE 'CALL FUNCTION' l_func INTO wa_blank-line SEPARATED BY space.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = 'EXPORTING'. APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = '   I_MBLNR = l_mblnr'. APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = '   I_MJAHR = l_gjahr'. APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = ' EXCEPTIONS'. APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = '   ILLEGAL_COMBINATION = 1'. APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = '   OTHERS = 2'. APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = 'IF sy-subrc <> 0.'. APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    wa_blank-line = 'ENDIF.'. APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
    CONCATENATE '"' p_rem p_end sy-uname l_datum 'for ATC'
      INTO wa_blank-line SEPARATED BY space.
    APPEND wa_blank TO repos_tab_new. CLEAR wa_blank.
  ELSE.
    APPEND wa_repos_tab TO repos_tab_new.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form smartform_procee
*&---------------------------------------------------------------------*
FORM smartform_procee.
  BREAK-POINT.
  DATA : i_caption TYPE tdtext,
         i_vartext TYPE tsfvtext,
         i_header  TYPE ssfhead,
         i_admin   TYPE stxfadm,
         p_form    TYPE tdsfname.
  p_form = wa_final_p-objname.
  CALL FUNCTION 'SSF_READ_FORM'
    EXPORTING
      i_formname             = p_form
    IMPORTING
      o_caption              = i_caption
      o_vartext              = i_vartext
      o_admdata              = i_admin
    EXCEPTIONS
      no_form                = 1
      no_active_source       = 2
      no_source              = 3
      OTHERS                 = 4.
  IF sy-subrc = 0.
    ASSIGN ('(SAPLSTXBX)T_NTOKENS') TO FIELD-SYMBOL(<fs_code>).
    LOOP AT <fs_code> ASSIGNING FIELD-SYMBOL(<fs_code1>).
      ASSIGN COMPONENT 'NTYPE' OF STRUCTURE <fs_code1> TO FIELD-SYMBOL(<fs_code2>).
      IF <fs_code2> = 'CO'.
        ASSIGN COMPONENT 'T_TOKEN' OF STRUCTURE <fs_code1> TO FIELD-SYMBOL(<fs_code3>).
        LOOP AT <fs_code3> ASSIGNING FIELD-SYMBOL(<fs_code4>).
          ASSIGN COMPONENT 'TNAME' OF STRUCTURE <fs_code4> TO FIELD-SYMBOL(<fs_tname>).
          IF <fs_tname> = 'OUTIN' OR <fs_tname> = 'CODE'.
          ELSE.
            LOOP AT repos_tab_new INTO DATA(l_repos) WHERE line CS <fs_tname>.
              BREAK-POINT.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& LCL_MAIN IMPLEMENTATION
*&---------------------------------------------------------------------*
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
*&---------------------------------------------------------------------*
*& Unit Test Class
*&---------------------------------------------------------------------*
CLASS lcl_test DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION MEDIUM.
  PUBLIC SECTION.
    METHODS test_1 FOR TESTING.
ENDCLASS.
CLASS lcl_test IMPLEMENTATION.
  METHOD test_1.
    PERFORM bdc_dynpro      USING 'SAPMS38M' '0101'.
    PERFORM bdc_field       USING 'BDC_CURSOR' 'RS38M-PROGRAMM'.
    PERFORM bdc_field       USING 'BDC_OKCODE' '=STRT'.
    PERFORM bdc_field       USING 'RS38M-PROGRAMM' 'RS_ABAP_ERROR_ANALYZE'.
    PERFORM bdc_dynpro      USING 'RS_ABAP_ERROR_ANALYZE' '1000'.
    PERFORM bdc_field       USING 'BDC_CURSOR' 'PROGRAM'.
    PERFORM bdc_field       USING 'BDC_OKCODE' '=ONLI'.
    PERFORM bdc_field       USING 'PROGRAM' 'RS_ABAP_ERROR_ANALYZE_ERR'.
    PERFORM bdc_field       USING 'EXCLINCL-LOW' '<*'.
    PERFORM bdc_field       USING 'SYNTRC' 'X'.
    PERFORM bdc_dynpro      USING 'SAPMSSY0' '0120'.
    PERFORM bdc_field       USING 'BDC_OKCODE' '=PZ 3'.
    PERFORM bdc_dynpro      USING 'SAPMSSY0' '0120'.
    PERFORM bdc_field       USING 'BDC_OKCODE' '=BACK'.
    PERFORM bdc_dynpro      USING 'RS_ABAP_ERROR_ANALYZE' '1000'.
    PERFORM bdc_field       USING 'BDC_OKCODE' '/EE'.
    PERFORM bdc_field       USING 'BDC_CURSOR' 'PROGRAM'.
    PERFORM bdc_transaction USING 'SA38'.
  ENDMETHOD.
ENDCLASS.
DATA bdcdata TYPE TABLE OF bdcdata WITH HEADER LINE.    "#EC NEEDED
DATA messtab TYPE TABLE OF bdcmsgcoll WITH HEADER LINE. "#EC NEEDED
FORM bdc_transaction USING p_tcode TYPE any.
  DATA: l_subrc LIKE sy-subrc.
  DATA: l_mode  TYPE sychar01.
  REFRESH messtab. l_mode = 'N'.
  TRY.
      CALL TRANSACTION p_tcode WITH AUTHORITY-CHECK
                       USING    bdcdata
                       MODE     l_mode
                       MESSAGES INTO messtab.
      l_subrc = sy-subrc.
      cl_aunit_assert=>assert_equals( act = l_subrc exp = 0 ).
      REFRESH bdcdata.
    CATCH cx_sy_authorization_error ##NO_HANDLER.
  ENDTRY.
ENDFORM.
FORM bdc_dynpro USING p_program TYPE any p_dynpro TYPE any.
  CLEAR bdcdata.
  bdcdata-program  = p_program.
  bdcdata-dynpro   = p_dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.
FORM bdc_field USING p_fnam TYPE any p_fval TYPE any.
  CLEAR bdcdata.
  bdcdata-fnam = p_fnam.
  bdcdata-fval = p_fval.
  APPEND bdcdata.
ENDFORM.
