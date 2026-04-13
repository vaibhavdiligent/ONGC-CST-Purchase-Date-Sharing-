*&---------------------------------------------------------------------*
*& Report ZBDC_FM_MAPPER
*& BDC to Function Module Field Mapping Analyzer
*&
*& Purpose:
*&   Given a source program containing CALL TRANSACTION (BDC) code
*&   and a replacement Function Module name, this program:
*&   1. Scans the source program for BDC FNAM/FVAL pairs
*&   2. Looks up rollnames (data elements) from DD03L for each BDC field
*&   3. Gets the FM parameter structures from FUPARAREF + DD03L
*&   4. Matches BDC fields to FM parameters via rollname
*&   5. Displays mapping result and generated code in ALV
*&
*& Usage:
*&   P_PROG  = Source program containing BDC code (e.g. ZVENDOR_CREATE)
*&   P_TCODE = Transaction being called via BDC (e.g. ME21)
*&   P_FM    = Replacement Function Module (e.g. BAPI_PO_CREATE1)
*&---------------------------------------------------------------------*
REPORT zbdc_fm_mapper.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
PARAMETERS: p_prog  TYPE program    OBLIGATORY,       " Source program
            p_tcode TYPE tcode      OBLIGATORY,       " BDC transaction
            p_fm    TYPE rs38l_fnam OBLIGATORY.       " Replacement FM

*----------------------------------------------------------------------*
* TYPE DEFINITIONS
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_bdc_map,
         fnam      TYPE char50,    " Full BDC field name e.g. EKKO-LIFNR
         fval_var  TYPE char100,   " Variable/value used for FVAL
         tabname   TYPE dd03l-tabname,   " Table part of FNAM
         fieldname TYPE dd03l-fieldname, " Field part of FNAM
         rollname  TYPE dd03l-rollname,  " Data element (bridge for matching)
         fm_param  TYPE char50,    " Matched FM parameter name
         fm_struct TYPE char50,    " Matched FM structure type name
         fm_field  TYPE dd03l-fieldname, " Matched field in FM structure
         matched   TYPE flag,      " X = matched, space = no match
         src_line  TYPE i,         " Source line number in program
       END OF ty_bdc_map.

TYPES: BEGIN OF ty_fm_dd,
         parameter TYPE fupararef-parameter,
         paramtype TYPE fupararef-paramtype, " I=Import E=Export T=Table X=Exception
         structure TYPE fupararef-structure,
         fieldname TYPE dd03l-fieldname,
         rollname  TYPE dd03l-rollname,
       END OF ty_fm_dd.

TYPES: BEGIN OF ty_output,
         src_line    TYPE i,
         bdc_fnam    TYPE char50,
         bdc_fval    TYPE char100,
         bdc_roll    TYPE dd03l-rollname,
         status      TYPE char15,
         fm_param    TYPE char50,
         fm_struct   TYPE char50,
         fm_field    TYPE dd03l-fieldname,
         gen_code    TYPE char200,
         remark      TYPE char100,
       END OF ty_output.

TYPES: BEGIN OF ty_code_preview,
         lineno TYPE i,
         code   TYPE char200,
       END OF ty_code_preview.

*----------------------------------------------------------------------*
* DATA DECLARATIONS
*----------------------------------------------------------------------*
DATA: lt_bdc_map     TYPE TABLE OF ty_bdc_map,
      wa_bdc_map     TYPE ty_bdc_map,
      lt_fm_dd       TYPE TABLE OF ty_fm_dd,
      wa_fm_dd       TYPE ty_fm_dd,
      lt_output      TYPE TABLE OF ty_output,
      wa_output      TYPE ty_output,
      lt_code        TYPE TABLE OF ty_code_preview,
      wa_code        TYPE ty_code_preview.

DATA: lt_source      TYPE TABLE OF abaptxt255,
      wa_source      TYPE abaptxt255.

DATA: lv_bdc_start   TYPE i,
      lv_bdc_end     TYPE i,
      lv_in_bdc      TYPE flag,
      lv_lineno      TYPE i,
      lv_code_ctr    TYPE i.

*----------------------------------------------------------------------*
* START OF SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  " Step 1: Read source program
  CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
    EXPORTING
      object_name           = p_prog
      versno                = '00000'
    TABLES
      repos_tab             = lt_source
    EXCEPTIONS
      no_version            = 1
      system_failure        = 2
      communication_failure = 3.
  IF sy-subrc <> 0 OR lt_source IS INITIAL.
    MESSAGE |Program { p_prog } not found or has no source| TYPE 'E'.
  ENDIF.

  " Step 2: Scan source for BDC block belonging to p_tcode
  PERFORM find_bdc_block.

  IF lt_bdc_map IS INITIAL.
    MESSAGE |No BDC fields found for transaction { p_tcode } in { p_prog }| TYPE 'I'.
    RETURN.
  ENDIF.

  " Step 3: Lookup DD03L rollnames for all BDC fields
  PERFORM enrich_rollnames.

  " Step 4: Get FM parameter structure fields with rollnames
  PERFORM get_fm_fields.

  " Step 5: Match BDC fields to FM parameters via rollname
  PERFORM match_fields.

  " Step 6: Build output table
  PERFORM build_output.

  " Step 7: Generate replacement code preview
  PERFORM generate_code_preview.

  " Step 8: Display results
  PERFORM display_results.

*----------------------------------------------------------------------*
*& Form find_bdc_block
*& Scan source program and extract all FNAM/FVAL pairs
*----------------------------------------------------------------------*
FORM find_bdc_block.
  DATA: lv_fnam_found  TYPE flag,
        lv_last_fnam   TYPE char50,
        lv_tcode_found TYPE flag.

  LOOP AT lt_source INTO wa_source.
    lv_lineno = sy-tabix.
    DATA(lv_line) = wa_source-line.
    TRANSLATE lv_line TO UPPER CASE.
    CONDENSE lv_line.

    " Detect start of BDC block — look for BDCDATA table declaration
    " or first wa_bdc-PROGRAM assignment near our transaction
    IF lv_line CS 'BDCDATA' OR lv_line CS 'BDC_OPEN_GROUP'.
      lv_in_bdc = 'X'.
      IF lv_bdc_start = 0.
        lv_bdc_start = lv_lineno.
      ENDIF.
    ENDIF.

    " Detect CALL TRANSACTION for our target tcode
    IF lv_line CS 'CALL TRANSACTION' AND lv_line CS p_tcode.
      lv_tcode_found = 'X'.
      lv_bdc_end     = lv_lineno.
      lv_in_bdc      = space.
    ENDIF.

    " Extract FNAM value — pattern: -FNAM = 'TABLE-FIELD'
    IF lv_in_bdc = 'X' AND lv_line CS '-FNAM'.
      " Find the quoted string value
      DATA(lv_raw) = wa_source-line.
      IF lv_raw CS '='.
        DATA(lv_pos) = sy-fdpos + 1.
        DATA(lv_rest) = lv_raw+lv_pos.
        CONDENSE lv_rest.
        " Extract value between quotes
        IF lv_rest CS ''''.
          DATA(lv_q1) = sy-fdpos + 1.
          lv_rest = lv_rest+lv_q1.
          IF lv_rest CS ''''.
            DATA(lv_q2) = sy-fdpos.
            lv_last_fnam = lv_rest(lv_q2).
            lv_fnam_found = 'X'.
            " Create map entry
            CLEAR wa_bdc_map.
            wa_bdc_map-fnam     = lv_last_fnam.
            wa_bdc_map-src_line = lv_lineno.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    " Extract FVAL value — pattern: -FVAL = <variable>
    IF lv_in_bdc = 'X' AND lv_line CS '-FVAL' AND lv_fnam_found = 'X'.
      DATA(lv_raw2) = wa_source-line.
      IF lv_raw2 CS '='.
        DATA(lv_pos2) = sy-fdpos + 1.
        DATA(lv_fval) = lv_raw2+lv_pos2.
        CONDENSE lv_fval.
        " Remove trailing dot or period
        REPLACE ALL OCCURRENCES OF '.' IN lv_fval WITH space.
        CONDENSE lv_fval.
        wa_bdc_map-fval_var = lv_fval.
      ENDIF.
      " Skip BDC navigation fields
      TRANSLATE wa_bdc_map-fnam TO UPPER CASE.
      IF wa_bdc_map-fnam = 'BDC_OKCODE'
        OR wa_bdc_map-fnam = 'BDC_CURSOR'.
        CLEAR wa_bdc_map.
        lv_fnam_found = space.
        CONTINUE.
      ENDIF.
      " Split FNAM into table and field
      IF wa_bdc_map-fnam CS '-'.
        DATA(lv_sp) = sy-fdpos.
        wa_bdc_map-tabname   = wa_bdc_map-fnam(lv_sp).
        DATA(lv_rest2) = wa_bdc_map-fnam+lv_sp.
        SHIFT lv_rest2 LEFT BY 1 PLACES.
        wa_bdc_map-fieldname = lv_rest2.
      ENDIF.
      APPEND wa_bdc_map TO lt_bdc_map.
      CLEAR: wa_bdc_map, lv_fnam_found, lv_last_fnam.
    ENDIF.
  ENDLOOP.

  IF lv_tcode_found = space.
    MESSAGE |CALL TRANSACTION '{ p_tcode }' not found in { p_prog }| TYPE 'I'.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*& Form enrich_rollnames
*& Get rollname (data element) for each BDC field from DD03L
*----------------------------------------------------------------------*
FORM enrich_rollnames.
  LOOP AT lt_bdc_map ASSIGNING FIELD-SYMBOL(<fs_map>).
    IF <fs_map>-tabname IS INITIAL OR <fs_map>-fieldname IS INITIAL.
      CONTINUE.
    ENDIF.
    SELECT SINGLE rollname INTO @<fs_map>-rollname
      FROM dd03l
      WHERE tabname   = @<fs_map>-tabname
        AND fieldname = @<fs_map>-fieldname.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
*& Form get_fm_fields
*& Get all parameter structure fields + rollnames for the FM
*----------------------------------------------------------------------*
FORM get_fm_fields.
  " Get FM parameters
  DATA lt_fupararef TYPE TABLE OF fupararef.
  SELECT * INTO TABLE @lt_fupararef
    FROM fupararef
    WHERE funcname  = @p_fm
      AND paramtype IN ('I', 'E', 'T').

  " For each parameter with a structure, get DD03L fields
  LOOP AT lt_fupararef INTO DATA(wa_fup).
    IF wa_fup-structure IS INITIAL. CONTINUE. ENDIF.

    SELECT fieldname, rollname
      INTO TABLE @DATA(lt_struct_fields)
      FROM dd03l
      WHERE tabname  = @wa_fup-structure
        AND rollname IS NOT INITIAL
        AND fieldname NOT LIKE '.%'.    " skip internal fields

    LOOP AT lt_struct_fields INTO DATA(wa_sf).
      CLEAR wa_fm_dd.
      wa_fm_dd-parameter = wa_fup-parameter.
      wa_fm_dd-paramtype = wa_fup-paramtype.
      wa_fm_dd-structure = wa_fup-structure.
      wa_fm_dd-fieldname = wa_sf-fieldname.
      wa_fm_dd-rollname  = wa_sf-rollname.
      APPEND wa_fm_dd TO lt_fm_dd.
    ENDLOOP.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
*& Form match_fields
*& Match BDC fields to FM parameters using rollname as bridge
*----------------------------------------------------------------------*
FORM match_fields.
  LOOP AT lt_bdc_map ASSIGNING FIELD-SYMBOL(<fs_m>).
    IF <fs_m>-rollname IS INITIAL. CONTINUE. ENDIF.

    READ TABLE lt_fm_dd INTO wa_fm_dd
      WITH KEY rollname = <fs_m>-rollname.
    IF sy-subrc = 0.
      <fs_m>-fm_param  = wa_fm_dd-parameter.
      <fs_m>-fm_struct = wa_fm_dd-structure.
      <fs_m>-fm_field  = wa_fm_dd-fieldname.
      <fs_m>-matched   = 'X'.
    ENDIF.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
*& Form build_output
*& Build ALV output table from mapping results
*----------------------------------------------------------------------*
FORM build_output.
  DATA lv_match_cnt  TYPE i.
  DATA lv_total_cnt  TYPE i.

  LOOP AT lt_bdc_map INTO wa_bdc_map.
    CLEAR wa_output.
    lv_total_cnt = lv_total_cnt + 1.

    wa_output-src_line  = wa_bdc_map-src_line.
    wa_output-bdc_fnam  = wa_bdc_map-fnam.
    wa_output-bdc_fval  = wa_bdc_map-fval_var.
    wa_output-bdc_roll  = wa_bdc_map-rollname.

    IF wa_bdc_map-matched = 'X'.
      lv_match_cnt = lv_match_cnt + 1.
      wa_output-status    = 'MATCHED'.
      wa_output-fm_param  = wa_bdc_map-fm_param.
      wa_output-fm_struct = wa_bdc_map-fm_struct.
      wa_output-fm_field  = wa_bdc_map-fm_field.
      CONCATENATE 'ls_' wa_bdc_map-fm_param '-' wa_bdc_map-fm_field
                  ' = ' wa_bdc_map-fval_var '.'
        INTO wa_output-gen_code SEPARATED BY space.
      wa_output-remark = wa_bdc_map-rollname.
    ELSE.
      wa_output-status = 'NO MATCH'.
      IF wa_bdc_map-rollname IS INITIAL.
        wa_output-remark = 'Field not found in DD03L'.
      ELSE.
        CONCATENATE 'Rollname' wa_bdc_map-rollname 'not in FM interface'
          INTO wa_output-remark SEPARATED BY space.
      ENDIF.
      CONCATENATE '" TODO: map' wa_bdc_map-fnam '(' wa_bdc_map-fval_var ')'
        INTO wa_output-gen_code SEPARATED BY space.
    ENDIF.
    APPEND wa_output TO lt_output.
  ENDLOOP.

  " Summary message
  MESSAGE |Mapping complete: { lv_match_cnt } of { lv_total_cnt } fields matched| TYPE 'I'.
ENDFORM.

*----------------------------------------------------------------------*
*& Form generate_code_preview
*& Generate the replacement ABAP code as a preview
*----------------------------------------------------------------------*
FORM generate_code_preview.
  DATA: lt_params   TYPE TABLE OF fupararef,
        lt_structs  TYPE SORTED TABLE OF char50 WITH UNIQUE KEY table_line.

  lv_code_ctr = 1.

  " Helper macro to add a code line
  DEFINE add_line.
    wa_code-lineno = lv_code_ctr.
    wa_code-code   = &1.
    APPEND wa_code TO lt_code.
    lv_code_ctr = lv_code_ctr + 1.
  END-OF-DEFINITION.

  add_line: |" ** begin of change - BDC → FM replacement **|.
  add_line: |" TODO: Replace CALL TRANSACTION '{ p_tcode }' with { p_fm }|.
  add_line: ''.

  " Comment out BDC lines
  add_line: |" --- Original BDC block (lines { lv_bdc_start } - { lv_bdc_end }) commented out ---|.
  LOOP AT lt_source INTO wa_source FROM lv_bdc_start TO lv_bdc_end.
    wa_code-lineno = lv_code_ctr.
    CONCATENATE '*' wa_source-line INTO wa_code-code.
    APPEND wa_code TO lt_code.
    lv_code_ctr = lv_code_ctr + 1.
  ENDLOOP.
  add_line: ''.

  " DATA declarations for FM structures
  add_line: '" --- Data declarations for FM structures ---'.
  SELECT DISTINCT parameter, structure, paramtype
    INTO TABLE @DATA(lt_distinct_params)
    FROM fupararef
    WHERE funcname  = @p_fm
      AND paramtype IN ('I', 'E', 'T')
      AND structure IS NOT INITIAL.

  LOOP AT lt_distinct_params INTO DATA(wa_dp).
    DATA lv_decl TYPE char200.
    CASE wa_dp-paramtype.
      WHEN 'I' OR 'E'.
        CONCATENATE 'DATA ls_' wa_dp-parameter ' TYPE '
          wa_dp-structure '.' INTO lv_decl SEPARATED BY space.
      WHEN 'T'.
        CONCATENATE 'DATA lt_' wa_dp-parameter ' TYPE TABLE OF '
          wa_dp-structure '.' INTO lv_decl SEPARATED BY space.
        DATA lv_wa_decl TYPE char200.
        CONCATENATE 'DATA ls_' wa_dp-parameter '_wa TYPE '
          wa_dp-structure '.' INTO lv_wa_decl SEPARATED BY space.
        add_line: lv_wa_decl.
    ENDCASE.
    add_line: lv_decl.
  ENDLOOP.
  add_line: 'DATA lt_return TYPE TABLE OF bapiret2.'.
  add_line: ''.

  " Field assignments — matched fields
  add_line: '" --- Auto-mapped field assignments ---'.
  LOOP AT lt_bdc_map INTO wa_bdc_map WHERE matched = 'X'.
    DATA lv_assign TYPE char200.
    CONCATENATE 'ls_' wa_bdc_map-fm_param
                '-' wa_bdc_map-fm_field
                ' = ' wa_bdc_map-fval_var '.'
      INTO lv_assign SEPARATED BY space.
    DATA lv_comment TYPE char200.
    CONCATENATE '" BDC:' wa_bdc_map-fnam
                '→ FM:' wa_bdc_map-fm_param '-' wa_bdc_map-fm_field
                '(' wa_bdc_map-rollname ')'
      INTO lv_comment SEPARATED BY space.
    add_line: lv_comment.
    add_line: lv_assign.
  ENDLOOP.
  add_line: ''.

  " TODO comments for unmatched fields
  DATA lv_has_todo TYPE flag.
  LOOP AT lt_bdc_map INTO wa_bdc_map WHERE matched = space.
    IF lv_has_todo = space.
      add_line: '" --- TODO: These BDC fields could not be auto-mapped ---'.
      lv_has_todo = 'X'.
    ENDIF.
    DATA lv_todo TYPE char200.
    CONCATENATE '" TODO:' wa_bdc_map-fnam
                '(val=' wa_bdc_map-fval_var
                'roll=' wa_bdc_map-rollname ')'
      INTO lv_todo SEPARATED BY space.
    add_line: lv_todo.
  ENDLOOP.
  add_line: ''.

  " FM call
  add_line: |CALL FUNCTION '{ p_fm }'|.
  DATA lv_curr_section TYPE char10.
  LOOP AT lt_distinct_params INTO wa_dp.
    CASE wa_dp-paramtype.
      WHEN 'I'.
        IF lv_curr_section <> 'EXPORTING'.
          add_line: '  EXPORTING'.
          lv_curr_section = 'EXPORTING'.
        ENDIF.
        CONCATENATE '    ' wa_dp-parameter ' = ls_' wa_dp-parameter
          INTO lv_decl SEPARATED BY space.
      WHEN 'E'.
        IF lv_curr_section <> 'IMPORTING'.
          add_line: '  IMPORTING'.
          lv_curr_section = 'IMPORTING'.
        ENDIF.
        CONCATENATE '    ' wa_dp-parameter ' = ls_' wa_dp-parameter
          INTO lv_decl SEPARATED BY space.
      WHEN 'T'.
        IF lv_curr_section <> 'TABLES'.
          add_line: '  TABLES'.
          lv_curr_section = 'TABLES'.
        ENDIF.
        CONCATENATE '    ' wa_dp-parameter ' = lt_' wa_dp-parameter
          INTO lv_decl SEPARATED BY space.
    ENDCASE.
    add_line: lv_decl.
  ENDLOOP.
  add_line: '  TABLES'.
  add_line: '    return = lt_return'.
  add_line: '  EXCEPTIONS'.
  add_line: '    OTHERS = 99.'.

  " BAPI commit if it is a BAPI
  IF p_fm CS 'BAPI'.
    add_line: 'IF sy-subrc = 0.'.
    add_line: '  CALL FUNCTION ''BAPI_TRANSACTION_COMMIT'''.
    add_line: '    EXPORTING wait = ''X''.'.
    add_line: 'ENDIF.'.
  ENDIF.
  add_line: ''.
  add_line: '" ** end of change - BDC → FM replacement **'.
ENDFORM.

*----------------------------------------------------------------------*
*& Form display_results
*& Show ALV for mapping + separate ALV for code preview
*----------------------------------------------------------------------*
FORM display_results.
  " --- ALV 1: Field Mapping Table ---
  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = DATA(lo_map)
        CHANGING  t_table      = lt_output ).

      lo_map->get_columns( )->set_optimize( 'X' ).

      DATA(lo_cols) = lo_map->get_columns( ).
      lo_cols->get_column( 'SRC_LINE'  )->set_long_text( 'Source Line' ).
      lo_cols->get_column( 'BDC_FNAM'  )->set_long_text( 'BDC Field Name' ).
      lo_cols->get_column( 'BDC_FVAL'  )->set_long_text( 'BDC Value Variable' ).
      lo_cols->get_column( 'BDC_ROLL'  )->set_long_text( 'Data Element (Rollname)' ).
      lo_cols->get_column( 'STATUS'    )->set_long_text( 'Match Status' ).
      lo_cols->get_column( 'FM_PARAM'  )->set_long_text( 'FM Parameter' ).
      lo_cols->get_column( 'FM_STRUCT' )->set_long_text( 'FM Structure Type' ).
      lo_cols->get_column( 'FM_FIELD'  )->set_long_text( 'FM Field Name' ).
      lo_cols->get_column( 'GEN_CODE'  )->set_long_text( 'Generated Code Line' ).
      lo_cols->get_column( 'REMARK'    )->set_long_text( 'Remark' ).

      " Color MATCHED rows green, NO MATCH rows red
      DATA(lo_disp) = lo_map->get_display_settings( ).
      lo_disp->set_list_header( |BDC → FM Field Mapping: { p_tcode } → { p_fm }| ).

      lo_map->display( ).
    CATCH cx_salv_msg.
      MESSAGE 'Error displaying mapping ALV' TYPE 'I'.
  ENDTRY.

  " --- ALV 2: Generated Code Preview ---
  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = DATA(lo_code)
        CHANGING  t_table      = lt_code ).

      lo_code->get_columns( )->set_optimize( 'X' ).
      lo_code->get_columns( )->get_column( 'LINENO' )->set_long_text( 'Line' ).
      lo_code->get_columns( )->get_column( 'CODE'   )->set_long_text( 'Generated ABAP Code' ).

      DATA(lo_disp2) = lo_code->get_display_settings( ).
      lo_disp2->set_list_header( |Generated Replacement Code Preview for { p_fm }| ).

      lo_code->display( ).
    CATCH cx_salv_msg.
      MESSAGE 'Error displaying code preview ALV' TYPE 'I'.
  ENDTRY.
ENDFORM.
