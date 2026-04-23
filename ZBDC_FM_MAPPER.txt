*&---------------------------------------------------------------------*
*& Report ZBDC_FM_MAPPER
*& BDC / FM to Function Module or Class Method Mapping Analyzer
*&
*& Purpose:
*&   Analyzes source code and maps old BDC/FM calls to a replacement
*&   Function Module OR Class Method using DD03L rollname matching.
*&
*& Modes:
*&   Mode 1 - BDC  → Function Module  : RB_FM  selected
*&   Mode 2 - BDC  → Class Method     : RB_CLS selected, P_TCODE filled
*&   Mode 3 - FM Call → Class Method  : RB_CLS selected, P_OLDFM filled
*&
*& Usage:
*&   P_PROG   = Source program to scan (e.g. ZVENDOR_CREATE)
*&   P_TCODE  = BDC transaction (e.g. ME21)  — for BDC modes
*&   P_OLDFM  = Old FM being called (e.g. OLD_FM) — for FM→Class mode
*&   P_FM     = Replacement FM     (RB_FM mode)
*&   P_CLASS  = Replacement Class  (RB_CLS mode, e.g. CL_MM_PO_FACTORY)
*&   P_METH   = Replacement Method (RB_CLS mode, e.g. CREATE_PO)
*&   P_STATIC = X = static method call (class=>method)
*&              space = instance method call (obj->method)
*&---------------------------------------------------------------------*
REPORT zbdc_fm_mapper.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
PARAMETERS: p_prog   TYPE program    OBLIGATORY.       " Source program

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_tcode  TYPE tcode,                       " BDC transaction
            p_oldfm  TYPE rs38l_fnam.                  " Old FM (for FM→Class)
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: rb_fm    RADIOBUTTON GROUP rg1 DEFAULT 'X'
                     USER-COMMAND ucomm,               " Replace with FM
            rb_cls   RADIOBUTTON GROUP rg1.            " Replace with Class Method
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_fm     TYPE rs38l_fnam.                  " Replacement FM (RB_FM mode)
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
PARAMETERS: p_class  TYPE seoclsname,                  " Replacement Class
            p_meth   TYPE seocpdname,                  " Replacement Method
            p_static TYPE flag AS CHECKBOX DEFAULT 'X'." X=static, space=instance
SELECTION-SCREEN END OF BLOCK b4.


*----------------------------------------------------------------------*
* TYPE DEFINITIONS
*----------------------------------------------------------------------*
" Class method parameter type
TYPES: BEGIN OF ty_cls_param,
         param_name TYPE seocpdname,     " Method parameter name
         param_dir  TYPE char1,          " I=Import E=Export R=Return C=Change
         type_name  TYPE char50,         " Type e.g. LIFNR / BAPIMEPOHEADER
         typtype    TYPE char1,          " 1=type 2=obj ref 3=data ref
         rollname   TYPE dd03l-rollname, " Data element for matching
         is_struct  TYPE flag,           " X = structure type
       END OF ty_cls_param.

" Class method field-level type (for structures)
TYPES: BEGIN OF ty_cls_field,
         param_name TYPE seocpdname,
         param_dir  TYPE char1,
         type_name  TYPE char50,
         fieldname  TYPE dd03l-fieldname,
         rollname   TYPE dd03l-rollname,
         field_path TYPE char200,           " full access path for deep structures
       END OF ty_cls_field.

TYPES: BEGIN OF ty_bdc_map,
         fnam      TYPE char50,    " Full BDC field name e.g. EKKO-LIFNR
         fval_var  TYPE char100,   " Variable/value used for FVAL
         tabname   TYPE dd03l-tabname,   " Table part of FNAM
         fieldname TYPE dd03l-fieldname, " Field part of FNAM
         rollname  TYPE dd03l-rollname,  " Data element (bridge for matching)
         fm_param  TYPE char50,          " Matched FM parameter name
         fm_struct TYPE char50,          " Matched FM structure type name
         fm_field  TYPE dd03l-fieldname, " Matched field in FM structure
         cls_param TYPE seocpdname,      " Matched class method parameter
         cls_field TYPE dd03l-fieldname, " Matched field in class param structure
         cls_type  TYPE char50,          " Class parameter type name
         cls_path  TYPE char200,         " Full deep-path for nested structures
         matched   TYPE flag,            " X = matched, space = no match
         src_line  TYPE i,               " Source line number in program
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
         cls_param   TYPE seocpdname,
         cls_field   TYPE dd03l-fieldname,
         gen_code    TYPE char200,
         remark      TYPE char100,
       END OF ty_output.

TYPES: BEGIN OF ty_code_preview,
         lineno TYPE i,
         code   TYPE char200,
       END OF ty_code_preview.

" Work item for iterative deep-structure expansion
TYPES: BEGIN OF ty_ex_item,
         struct_name TYPE char50,
         path_prefix TYPE char200,
         param_name  TYPE seocpdname,
         param_dir   TYPE char1,
       END OF ty_ex_item.

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

" Class method data
DATA: lt_cls_params  TYPE TABLE OF ty_cls_param,
      wa_cls_param   TYPE ty_cls_param,
      lt_cls_fields  TYPE TABLE OF ty_cls_field,
      wa_cls_field   TYPE ty_cls_field.

" FM call block (for FM→Class mode)
DATA: lt_fm_params   TYPE TABLE OF ty_bdc_map, " reuse ty_bdc_map for FM params
      lv_fm_start    TYPE i,
      lv_fm_end      TYPE i.

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

  " Validate inputs
  IF rb_fm = 'X' AND p_fm IS INITIAL.
    MESSAGE 'Enter Replacement FM name' TYPE 'E'.
  ENDIF.
  IF rb_cls = 'X' AND ( p_class IS INITIAL OR p_meth IS INITIAL ).
    MESSAGE 'Enter Replacement Class and Method name' TYPE 'E'.
  ENDIF.
  IF p_tcode IS INITIAL AND p_oldfm IS INITIAL.
    MESSAGE 'Enter either BDC Transaction or Old FM name' TYPE 'E'.
  ENDIF.

  " Step 1: Read source program
  DATA lv_obj_name TYPE versobjnam.
  lv_obj_name = p_prog.
  CALL FUNCTION 'SVRS_GET_VERSION_REPS_40'
    EXPORTING
      object_name           = lv_obj_name
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

  " Step 2: Scan source — BDC block or FM call block
  IF p_tcode IS NOT INITIAL.
    PERFORM find_bdc_block.
    IF lt_bdc_map IS INITIAL.
      MESSAGE |No BDC fields found for { p_tcode } in { p_prog }| TYPE 'I'.
      RETURN.
    ENDIF.
  ELSE.
    " FM call mode — scan for CALL FUNCTION 'p_oldfm'
    PERFORM find_fm_call_block.
    IF lt_bdc_map IS INITIAL.
      MESSAGE |No CALL FUNCTION '{ p_oldfm }' found in { p_prog }| TYPE 'I'.
      RETURN.
    ENDIF.
  ENDIF.

  " Step 3: Enrich with rollnames from DD03L
  PERFORM enrich_rollnames.

  " Step 4a: FM replacement mode
  IF rb_fm = 'X'.
    PERFORM get_fm_fields.
    PERFORM match_fields.
    PERFORM build_output.
    PERFORM generate_code_preview.

  " Step 4b: Class method replacement mode
  ELSEIF rb_cls = 'X'.
    PERFORM get_class_method_params.
    PERFORM match_to_class.
    PERFORM build_output_class.
    PERFORM generate_class_code_preview.
  ENDIF.

  " Step 5: Display results
  PERFORM display_results.

*----------------------------------------------------------------------*
*& Form find_bdc_block
*& Scan source program and extract all FNAM/FVAL pairs
*----------------------------------------------------------------------*
FORM find_bdc_block.
  DATA: lv_tcode_found TYPE flag,
        lv_pend_val    TYPE flag.   " X = waiting for value on next non-comment line

  LOOP AT lt_source INTO wa_source.
    lv_lineno = sy-tabix.
    DATA(lv_line) = wa_source-line.
    CONDENSE lv_line.

    " Skip pure comment lines
    IF lv_line IS INITIAL. CONTINUE. ENDIF.
    IF lv_line(1) = '*'. CONTINUE. ENDIF.

    DATA(lv_line_u) = lv_line.
    TRANSLATE lv_line_u TO UPPER CASE.

    " Detect CALL TRANSACTION for our target tcode
    IF lv_line_u CS 'CALL TRANSACTION' AND lv_line_u CS p_tcode.
      lv_tcode_found = 'X'.
      lv_bdc_end  = lv_lineno.
      lv_in_bdc   = space.
      lv_pend_val = space.
      CONTINUE.
    ENDIF.

    " Detect start of BDC block via PERFORM bdc_dynpro / bdc_field
    IF lv_line_u CS 'PERFORM BDC_DYNPRO' OR lv_line_u CS 'PERFORM BDC_FIELD'.
      lv_in_bdc = 'X'.
      IF lv_bdc_start = 0. lv_bdc_start = lv_lineno. ENDIF.
    ENDIF.

    IF lv_in_bdc <> 'X'. CONTINUE. ENDIF.

    " ── Handle pending value: value variable was on next line after field name ──
    IF lv_pend_val = 'X'.
      DATA lv_val TYPE string.
      lv_val = lv_line.
      REPLACE ALL OCCURRENCES OF '.' IN lv_val WITH space.
      " Strip surrounding quotes if value is a literal e.g. 'X'
      IF lv_val CS ''''.
        DATA(lv_vq1) = sy-fdpos + 1.
        DATA lv_vinner TYPE string.
        lv_vinner = substring( val = lv_val off = lv_vq1 ).
        IF lv_vinner CS ''''.
          DATA(lv_vq2) = sy-fdpos.
          lv_val = lv_vinner(lv_vq2).
        ENDIF.
      ENDIF.
      CONDENSE lv_val.
      wa_bdc_map-fval_var = lv_val.
      lv_pend_val = space.
      PERFORM append_bdc_map.
      CONTINUE.
    ENDIF.

    " ── Detect: PERFORM bdc_field USING 'FNAM' [value] ──
    IF lv_line_u CS 'PERFORM BDC_FIELD' AND lv_line_u CS 'USING'.
      DATA(lv_raw) = lv_line.
      " Find first quote — start of field name
      IF lv_raw CS ''''.
        DATA(lv_q1) = sy-fdpos + 1.
        DATA lv_rest TYPE string.
        lv_rest = substring( val = lv_raw off = lv_q1 ).
        " Find closing quote — end of field name
        IF lv_rest CS ''''.
          DATA(lv_q2) = sy-fdpos.
          CLEAR wa_bdc_map.
          wa_bdc_map-fnam     = lv_rest(lv_q2).
          wa_bdc_map-src_line = lv_lineno.

          " Check for value on same line after closing quote
          DATA lv_after TYPE string.
          lv_after = substring( val = lv_rest off = lv_q2 + 1 ).
          CONDENSE lv_after.
          REPLACE ALL OCCURRENCES OF '.' IN lv_after WITH space.
          " Strip quotes from literal value e.g. '/00'
          IF lv_after CS ''''.
            DATA(lv_aq1) = sy-fdpos + 1.
            DATA lv_ainner TYPE string.
            lv_ainner = substring( val = lv_after off = lv_aq1 ).
            IF lv_ainner CS ''''.
              DATA(lv_aq2) = sy-fdpos.
              lv_after = lv_ainner(lv_aq2).
            ENDIF.
          ENDIF.
          CONDENSE lv_after.

          IF lv_after IS NOT INITIAL.
            wa_bdc_map-fval_var = lv_after.
            PERFORM append_bdc_map.
          ELSE.
            " Value is on the next non-comment line
            lv_pend_val = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    " ── Also support direct -FNAM = '...' / -FVAL = ... assignments ──
    IF lv_line_u CS '-FNAM' AND lv_line_u CS '='.
      DATA(lv_raw_fn) = lv_line.
      IF lv_raw_fn CS '='.
        DATA(lv_fn_pos) = sy-fdpos + 1.
        DATA lv_fn_rest TYPE string.
        lv_fn_rest = substring( val = lv_raw_fn off = lv_fn_pos ).
        IF lv_fn_rest CS ''''.
          DATA(lv_fnq1) = sy-fdpos + 1.
          lv_fn_rest = substring( val = lv_fn_rest off = lv_fnq1 ).
          IF lv_fn_rest CS ''''.
            DATA(lv_fnq2) = sy-fdpos.
            CLEAR wa_bdc_map.
            wa_bdc_map-fnam     = lv_fn_rest(lv_fnq2).
            wa_bdc_map-src_line = lv_lineno.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_line_u CS '-FVAL' AND lv_line_u CS '=' AND wa_bdc_map-fnam IS NOT INITIAL.
      DATA(lv_raw_fv) = lv_line.
      IF lv_raw_fv CS '='.
        DATA(lv_fv_pos) = sy-fdpos + 1.
        DATA lv_fval TYPE string.
        lv_fval = substring( val = lv_raw_fv off = lv_fv_pos ).
        REPLACE ALL OCCURRENCES OF '.' IN lv_fval WITH space.
        CONDENSE lv_fval.
        wa_bdc_map-fval_var = lv_fval.
        PERFORM append_bdc_map.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF lv_tcode_found = space.
    MESSAGE |CALL TRANSACTION '{ p_tcode }' not found in { p_prog }| TYPE 'I'.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*& Helper: validate, split and append wa_bdc_map to lt_bdc_map
*----------------------------------------------------------------------*
FORM append_bdc_map.
  " Skip BDC navigation fields
  DATA(lv_fn_u) = wa_bdc_map-fnam.
  TRANSLATE lv_fn_u TO UPPER CASE.
  IF lv_fn_u = 'BDC_OKCODE' OR lv_fn_u = 'BDC_CURSOR'.
    CLEAR wa_bdc_map. RETURN.
  ENDIF.
  " Split FNAM into table and field (e.g. LFBW-WITHT(01) -> LFBW / WITHT(01))
  IF wa_bdc_map-fnam CS '-'.
    DATA(lv_sp) = sy-fdpos.
    wa_bdc_map-tabname = wa_bdc_map-fnam(lv_sp).
    DATA lv_fld TYPE string.
    lv_fld = substring( val = wa_bdc_map-fnam off = lv_sp ).
    SHIFT lv_fld LEFT BY 1 PLACES.
    wa_bdc_map-fieldname = lv_fld.
  ENDIF.
  APPEND wa_bdc_map TO lt_bdc_map.
  CLEAR wa_bdc_map.
ENDFORM.

*----------------------------------------------------------------------*
*& Form find_fm_call_block
*& Scan source for CALL FUNCTION 'p_oldfm' and extract parameters
*& Reuses lt_bdc_map: fnam=param name, fval_var=variable passed
*----------------------------------------------------------------------*
FORM find_fm_call_block.
  DATA: lv_in_fm    TYPE flag,
        lv_section  TYPE char15.   " EXPORTING/IMPORTING/TABLES

  LOOP AT lt_source INTO wa_source.
    lv_lineno = sy-tabix.
    DATA(lv_line_u) = wa_source-line.
    TRANSLATE lv_line_u TO UPPER CASE.
    CONDENSE lv_line_u.

    " Detect CALL FUNCTION line for our target FM
    IF lv_line_u CS 'CALL FUNCTION' AND lv_line_u CS p_oldfm.
      lv_in_fm    = 'X'.
      lv_fm_start = lv_lineno.
      lv_section  = space.
      CONTINUE.
    ENDIF.

    IF lv_in_fm = 'X'.
      " Track section
      IF lv_line_u CS 'EXPORTING'.  lv_section = 'EXPORTING'. CONTINUE. ENDIF.
      IF lv_line_u CS 'IMPORTING'.  lv_section = 'IMPORTING'. CONTINUE. ENDIF.
      IF lv_line_u CS 'CHANGING'.   lv_section = 'CHANGING'.  CONTINUE. ENDIF.
      IF lv_line_u CS 'TABLES'.     lv_section = 'TABLES'.    CONTINUE. ENDIF.
      IF lv_line_u CS 'EXCEPTIONS'. lv_section = 'EXCEPTION'. CONTINUE. ENDIF.

      " End of CALL FUNCTION block
      IF lv_line_u(1) = '.' OR ( lv_line_u CS '.' AND NOT ( lv_line_u CS '=' ) ).
        lv_fm_end = lv_lineno.
        lv_in_fm  = space.
        EXIT.
      ENDIF.

      " Skip EXCEPTIONS section entries
      IF lv_section = 'EXCEPTION'. CONTINUE. ENDIF.

      " Extract  param_name = variable
      DATA(lv_raw_fm) = wa_source-line.
      CONDENSE lv_raw_fm.
      IF lv_raw_fm CS '='.
        DATA(lv_eq) = sy-fdpos.
        DATA lv_pname TYPE string.
        lv_pname = lv_raw_fm(lv_eq).
        CONDENSE lv_pname.
        DATA lv_pval TYPE string.
        lv_pval = substring( val = lv_raw_fm off = lv_eq ).
        SHIFT lv_pval LEFT BY 1 PLACES.
        REPLACE ALL OCCURRENCES OF '.' IN lv_pval WITH space.
        CONDENSE lv_pval.

        IF lv_pname IS NOT INITIAL AND lv_pval IS NOT INITIAL.
          CLEAR wa_bdc_map.
          wa_bdc_map-fnam     = lv_pname.   " old FM parameter name
          wa_bdc_map-fval_var = lv_pval.    " variable passed
          wa_bdc_map-src_line = lv_lineno.
          " Get rollname for this FM parameter from FUPARAREF + DD03L
          SELECT SINGLE structure INTO @DATA(lv_struct)
            FROM fupararef
            WHERE funcname  = @p_oldfm
              AND parameter = @lv_pname.
          IF sy-subrc = 0 AND lv_struct IS NOT INITIAL.
            " structured param — store structure as tabname for DD03L lookup
            wa_bdc_map-tabname   = lv_struct.
            wa_bdc_map-fieldname = lv_pname.
          ELSE.
            " scalar param — get rollname directly
            SELECT SINGLE type INTO @DATA(lv_ptype)
              FROM fupararef
              WHERE funcname  = @p_oldfm
                AND parameter = @lv_pname.
            wa_bdc_map-rollname = lv_ptype.
          ENDIF.
          APPEND wa_bdc_map TO lt_bdc_map.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
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
      FROM dd03l
      WHERE tabname  = @wa_fup-structure
        AND rollname IS NOT INITIAL
        AND fieldname NOT LIKE '.%'    " skip internal fields
      INTO TABLE @DATA(lt_struct_fields).

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
*& Form get_class_method_params
*& Read class method parameters from SEOPAR + DD03L rollnames
*& SEOPAR fields:
*&   CLSNAME   = class name
*&   CPDNAME   = method name
*&   SCONAME   = parameter name
*&   PARDECLTYP= I/E/R/C (Importing/Exporting/Returning/Changing)
*&   TYPTYPE   = 1=type 2=obj ref 3=data ref
*&   TYPE      = type name (rollname or structure)
*----------------------------------------------------------------------*
FORM get_class_method_params.
  DATA: lo_desc     TYPE REF TO cl_abap_classdescr,
        lv_typename TYPE string.

  TRY.
      lo_desc = CAST cl_abap_classdescr(
                  cl_abap_typedescr=>describe_by_name( p_class ) ).
    CATCH cx_root.
      MESSAGE |Class { p_class } not found or is not a class| TYPE 'I'.
      RETURN.
  ENDTRY.

  READ TABLE lo_desc->methods
    WITH KEY name = to_upper( p_meth )
    INTO DATA(wa_meth).
  IF sy-subrc <> 0.
    MESSAGE |Method { p_meth } not found in { p_class }| TYPE 'I'.
    RETURN.
  ENDIF.

  IF wa_meth-parameters IS INITIAL.
    MESSAGE |No parameters found for { p_class }->{ p_meth }| TYPE 'I'.
    RETURN.
  ENDIF.

  LOOP AT wa_meth-parameters INTO DATA(wa_parm).
    " Extract type name via RTTI (ABAP_PARMDESCR has no type_descr in this release)
    TRY.
        DATA(lo_ptype) = lo_desc->get_method_parameter_type(
                           p_method_name    = to_upper( p_meth )
                           p_parameter_name = wa_parm-name ).
        lv_typename = lo_ptype->absolute_name.
      CATCH cx_root.
        lv_typename = wa_parm-name.
    ENDTRY.
    " Extract type name: absolute_name format is \TYPE=NAME or \POOL=X\TYPE=NAME
    IF lv_typename CS '='.
      DATA lt_tn_parts TYPE TABLE OF string.
      SPLIT lv_typename AT '=' INTO TABLE lt_tn_parts.
      READ TABLE lt_tn_parts INDEX lines( lt_tn_parts ) INTO lv_typename.
    ENDIF.
    IF lv_typename IS INITIAL.
      lv_typename = wa_parm-name.
    ENDIF.

    CLEAR wa_cls_param.
    wa_cls_param-param_name = wa_parm-name.
    wa_cls_param-param_dir  = wa_parm-parm_kind.
    wa_cls_param-type_name  = lv_typename.
    wa_cls_param-typtype    = '1'.

    " Check if the type is a structure (exists in DD02L)
    SELECT SINGLE tabname INTO @DATA(lv_tabchk)
      FROM dd02l WHERE tabname = @lv_typename.
    IF sy-subrc = 0.
      " Structure type — expand all fields
      wa_cls_param-is_struct = 'X'.
      wa_cls_param-rollname  = space.
      APPEND wa_cls_param TO lt_cls_params.

      " Recursively expand structure (handles nested structures and table types)
      DATA lv_px_path  TYPE char200.
      DATA lv_px_param TYPE seocpdname.
      DATA lv_px_dir   TYPE char1.
      CLEAR lv_px_path.
      lv_px_param = wa_parm-name.
      lv_px_dir   = wa_parm-parm_kind.
      PERFORM expand_struct_fields
        USING lv_typename lv_px_path lv_px_param lv_px_dir.
    ELSE.
      " Scalar type — rollname = type itself (it IS the data element)
      wa_cls_param-is_struct = space.
      wa_cls_param-rollname  = lv_typename.
      APPEND wa_cls_param TO lt_cls_params.

      " Also add as field entry for matching
      CLEAR wa_cls_field.
      wa_cls_field-param_name = wa_parm-name.
      wa_cls_field-param_dir  = wa_parm-parm_kind.
      wa_cls_field-type_name  = lv_typename.
      wa_cls_field-fieldname  = wa_parm-name.
      wa_cls_field-rollname   = lv_typename.
      APPEND wa_cls_field TO lt_cls_fields.
    ENDIF.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
*& Form expand_struct_fields
*& Iterative deep-structure expansion using RTTI.
*& RTTI reliably identifies whether a component is a structure, table
*& type or elementary field — regardless of how DD03L stores the type.
*& Structure → expand components recursively
*& Table type → get row type, append [1] to path, expand row structure
*& Elementary → read rollname from DD03L and record in lt_cls_fields
*----------------------------------------------------------------------*
FORM expand_struct_fields
  USING    p_struct  TYPE string
           p_path    TYPE char200
           p_param   TYPE seocpdname
           p_dir     TYPE char1.

  " Work stack and work areas (all pre-declared — no DATA inside loops)
  DATA lt_ex_stack  TYPE TABLE OF ty_ex_item.
  DATA wa_ex_item   TYPE ty_ex_item.
  DATA wa_ex_new    TYPE ty_ex_item.
  DATA lv_ex_depth  TYPE i.
  DATA lv_ex_path   TYPE char200.
  DATA lv_ex_tpath  TYPE char200.
  DATA lv_ex_tname  TYPE string.
  DATA lt_ex_parts  TYPE TABLE OF string.

  " RTTI references
  DATA lo_ex_tdesc  TYPE REF TO cl_abap_typedescr.
  DATA lo_ex_strdsc TYPE REF TO cl_abap_structdescr.
  DATA lo_ex_tabdsc TYPE REF TO cl_abap_tabledescr.
  DATA lo_ex_row    TYPE REF TO cl_abap_typedescr.
  DATA lt_ex_comps  TYPE abap_component_tab.

  " Seed the stack with the top-level type
  CLEAR wa_ex_new.
  wa_ex_new-struct_name = p_struct.
  wa_ex_new-path_prefix = p_path.
  wa_ex_new-param_name  = p_param.
  wa_ex_new-param_dir   = p_dir.
  APPEND wa_ex_new TO lt_ex_stack.

  WHILE lt_ex_stack IS NOT INITIAL.
    READ TABLE lt_ex_stack INTO wa_ex_item INDEX 1.
    DELETE lt_ex_stack INDEX 1.

    " Safety: stop if path depth exceeds 10 nesting levels
    FIND ALL OCCURRENCES OF '-' IN wa_ex_item-path_prefix MATCH COUNT lv_ex_depth.
    IF lv_ex_depth > 10. CONTINUE. ENDIF.

    " Get RTTI descriptor for this type name
    TRY.
        lo_ex_tdesc = cl_abap_typedescr=>describe_by_name( wa_ex_item-struct_name ).
      CATCH cx_root.
        CONTINUE.   " type not found in this system
    ENDTRY.

    " ── Table type: resolve row type and push with [1] suffix ──────────
    IF lo_ex_tdesc->kind = cl_abap_typedescr=>kind_table.
      TRY.
          lo_ex_tabdsc = CAST cl_abap_tabledescr( lo_ex_tdesc ).
          lo_ex_row    = lo_ex_tabdsc->get_table_line_type( ).
          lv_ex_tname  = lo_ex_row->absolute_name.
          CLEAR lt_ex_parts.
          IF lv_ex_tname CS '='.
            SPLIT lv_ex_tname AT '=' INTO TABLE lt_ex_parts.
            READ TABLE lt_ex_parts INDEX lines( lt_ex_parts ) INTO lv_ex_tname.
          ENDIF.
          IF wa_ex_item-path_prefix IS INITIAL.
            lv_ex_tpath = '[1]'.
          ELSE.
            CONCATENATE wa_ex_item-path_prefix '[1]' INTO lv_ex_tpath.
          ENDIF.
          CLEAR wa_ex_new.
          wa_ex_new-struct_name = lv_ex_tname.
          wa_ex_new-path_prefix = lv_ex_tpath.
          wa_ex_new-param_name  = wa_ex_item-param_name.
          wa_ex_new-param_dir   = wa_ex_item-param_dir.
          APPEND wa_ex_new TO lt_ex_stack.
        CATCH cx_root.
      ENDTRY.
      CONTINUE.
    ENDIF.

    " ── Structure type: iterate components ─────────────────────────────
    IF lo_ex_tdesc->kind = cl_abap_typedescr=>kind_struct.
      TRY.
          lo_ex_strdsc = CAST cl_abap_structdescr( lo_ex_tdesc ).
          lt_ex_comps  = lo_ex_strdsc->get_components( ).
        CATCH cx_root.
          CONTINUE.
      ENDTRY.

      LOOP AT lt_ex_comps INTO DATA(wa_ex_comp).
        IF wa_ex_comp-name IS INITIAL. CONTINUE. ENDIF.

        " Build access path for this component
        IF wa_ex_item-path_prefix IS INITIAL.
          lv_ex_path = wa_ex_comp-name.
        ELSE.
          CONCATENATE wa_ex_item-path_prefix '-' wa_ex_comp-name INTO lv_ex_path.
        ENDIF.

        CASE wa_ex_comp-type->kind.

          WHEN cl_abap_typedescr=>kind_struct.
            " Nested structure → push for further expansion
            lv_ex_tname = wa_ex_comp-type->absolute_name.
            CLEAR lt_ex_parts.
            IF lv_ex_tname CS '='.
              SPLIT lv_ex_tname AT '=' INTO TABLE lt_ex_parts.
              READ TABLE lt_ex_parts INDEX lines( lt_ex_parts ) INTO lv_ex_tname.
            ENDIF.
            CLEAR wa_ex_new.
            wa_ex_new-struct_name = lv_ex_tname.
            wa_ex_new-path_prefix = lv_ex_path.
            wa_ex_new-param_name  = wa_ex_item-param_name.
            wa_ex_new-param_dir   = wa_ex_item-param_dir.
            APPEND wa_ex_new TO lt_ex_stack.

          WHEN cl_abap_typedescr=>kind_table.
            " Table component → get row type, append [1], push row structure
            TRY.
                lo_ex_tabdsc = CAST cl_abap_tabledescr( wa_ex_comp-type ).
                lo_ex_row    = lo_ex_tabdsc->get_table_line_type( ).
                lv_ex_tname  = lo_ex_row->absolute_name.
                CLEAR lt_ex_parts.
                IF lv_ex_tname CS '='.
                  SPLIT lv_ex_tname AT '=' INTO TABLE lt_ex_parts.
                  READ TABLE lt_ex_parts INDEX lines( lt_ex_parts ) INTO lv_ex_tname.
                ENDIF.
                CONCATENATE lv_ex_path '[1]' INTO lv_ex_tpath.
                CLEAR wa_ex_new.
                wa_ex_new-struct_name = lv_ex_tname.
                wa_ex_new-path_prefix = lv_ex_tpath.
                wa_ex_new-param_name  = wa_ex_item-param_name.
                wa_ex_new-param_dir   = wa_ex_item-param_dir.
                APPEND wa_ex_new TO lt_ex_stack.
              CATCH cx_root.
            ENDTRY.

          WHEN cl_abap_typedescr=>kind_elem.
            " Primitive field → get rollname from DD03L for matching
            SELECT SINGLE rollname
              FROM dd03l
              WHERE tabname   = @wa_ex_item-struct_name
                AND fieldname = @wa_ex_comp-name
              INTO @DATA(lv_ex_roll).
            IF sy-subrc = 0 AND lv_ex_roll IS NOT INITIAL.
              CLEAR wa_cls_field.
              wa_cls_field-param_name = wa_ex_item-param_name.
              wa_cls_field-param_dir  = wa_ex_item-param_dir.
              wa_cls_field-type_name  = wa_ex_item-struct_name.
              wa_cls_field-fieldname  = wa_ex_comp-name.
              wa_cls_field-rollname   = lv_ex_roll.
              wa_cls_field-field_path = lv_ex_path.
              APPEND wa_cls_field TO lt_cls_fields.
            ENDIF.

        ENDCASE.
      ENDLOOP.
    ENDIF.
  ENDWHILE.
ENDFORM.

*----------------------------------------------------------------------*
*& Form match_to_class
*& Match BDC/FM fields to class method parameters via rollname
*----------------------------------------------------------------------*
FORM match_to_class.
  LOOP AT lt_bdc_map ASSIGNING FIELD-SYMBOL(<fs_c>).
    IF <fs_c>-rollname IS INITIAL. CONTINUE. ENDIF.

    READ TABLE lt_cls_fields INTO wa_cls_field
      WITH KEY rollname = <fs_c>-rollname.
    IF sy-subrc = 0.
      <fs_c>-cls_param = wa_cls_field-param_name.
      <fs_c>-cls_field = wa_cls_field-fieldname.
      <fs_c>-cls_type  = wa_cls_field-type_name.
      <fs_c>-cls_path  = wa_cls_field-field_path.
      <fs_c>-matched   = 'X'.
    ENDIF.
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
*& Form build_output_class
*& Build ALV output for class method mapping
*----------------------------------------------------------------------*
FORM build_output_class.
  DATA lv_match_cnt    TYPE i.
  DATA lv_total_cnt    TYPE i.
  DATA lv_path_to_use  TYPE char200.

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
      wa_output-cls_param = wa_bdc_map-cls_param.
      wa_output-cls_field = wa_bdc_map-cls_field.

      " Use deep path if available, else simple field name
      IF wa_bdc_map-cls_path IS NOT INITIAL.
        lv_path_to_use = wa_bdc_map-cls_path.
      ELSE.
        lv_path_to_use = wa_bdc_map-cls_field.
      ENDIF.
      " Check if param is a structure
      READ TABLE lt_cls_params INTO wa_cls_param
        WITH KEY param_name = wa_bdc_map-cls_param
                 is_struct  = 'X'.
      IF sy-subrc = 0.
        CONCATENATE 'ls_' wa_bdc_map-cls_param
                    '-' lv_path_to_use
                    ' = ' wa_bdc_map-fval_var '.'
          INTO wa_output-gen_code SEPARATED BY space.
      ELSE.
        CONCATENATE wa_bdc_map-cls_param
                    ' = ' wa_bdc_map-fval_var '.'
          INTO wa_output-gen_code SEPARATED BY space.
      ENDIF.
      wa_output-remark = wa_bdc_map-rollname.
    ELSE.
      wa_output-status = 'NO MATCH'.
      IF wa_bdc_map-rollname IS INITIAL.
        wa_output-remark = 'Rollname not found in DD03L'.
      ELSE.
        CONCATENATE 'Rollname' wa_bdc_map-rollname
          'not in class method interface'
          INTO wa_output-remark SEPARATED BY space.
      ENDIF.
      CONCATENATE '" TODO: map' wa_bdc_map-fnam
        '(' wa_bdc_map-fval_var ')'
        INTO wa_output-gen_code SEPARATED BY space.
    ENDIF.
    APPEND wa_output TO lt_output.
  ENDLOOP.

  MESSAGE |Class mapping: { lv_match_cnt } of { lv_total_cnt } fields matched| TYPE 'I'.
ENDFORM.

*----------------------------------------------------------------------*
*& Form generate_class_code_preview
*& Generate replacement Class Method call code
*----------------------------------------------------------------------*
FORM generate_class_code_preview.
  lv_code_ctr = 1.
  DATA lv_ln TYPE string.

  DEFINE add_line.
    wa_code-lineno = lv_code_ctr.
    wa_code-code   = &1.
    APPEND wa_code TO lt_code.
    lv_code_ctr = lv_code_ctr + 1.
  END-OF-DEFINITION.

  " Header
  IF p_tcode IS NOT INITIAL.
    lv_ln = |" ** begin of change - BDC -> Class Method replacement **|. add_line: lv_ln.
    lv_ln = |" Replace CALL TRANSACTION '{ p_tcode }' with { p_class }=>{ p_meth }|. add_line: lv_ln.
  ELSE.
    lv_ln = |" ** begin of change - FM -> Class Method replacement **|. add_line: lv_ln.
    lv_ln = |" Replace CALL FUNCTION '{ p_oldfm }' with { p_class }=>{ p_meth }|. add_line: lv_ln.
  ENDIF.
  add_line: ''.

  " Comment out original block
  DATA lv_blk_start TYPE i.
  DATA lv_blk_end   TYPE i.
  IF p_tcode IS NOT INITIAL.
    lv_blk_start = lv_bdc_start. lv_blk_end = lv_bdc_end.
  ELSE.
    lv_blk_start = lv_fm_start.  lv_blk_end = lv_fm_end.
  ENDIF.
  lv_ln = |" --- Original block (lines { lv_blk_start }-{ lv_blk_end }) commented out ---|. add_line: lv_ln.
  LOOP AT lt_source INTO wa_source FROM lv_blk_start TO lv_blk_end.
    wa_code-lineno = lv_code_ctr.
    CONCATENATE '*' wa_source-line INTO wa_code-code.
    APPEND wa_code TO lt_code.
    lv_code_ctr = lv_code_ctr + 1.
  ENDLOOP.
  add_line: ''.

  " DATA declarations for structured parameters
  add_line: '" --- Data declarations ---'.
  LOOP AT lt_cls_params INTO wa_cls_param WHERE is_struct = 'X'.
    DATA lv_cls_decl TYPE char200.
    CONCATENATE 'DATA ls_' wa_cls_param-param_name
      ' TYPE ' wa_cls_param-type_name '.'
      INTO lv_cls_decl SEPARATED BY space.
    add_line: lv_cls_decl.
  ENDLOOP.
  add_line: ''.

  " Matched field assignments
  add_line: '" --- Auto-mapped field assignments (via DD03L rollname) ---'.
  DATA lv_fpath      TYPE char200.
  LOOP AT lt_bdc_map INTO wa_bdc_map WHERE matched = 'X'.
    DATA lv_cls_assign TYPE char200.
    DATA lv_cls_cmt    TYPE char200.
    " Use deep path if available (e.g. COMPANY_DATA[1]-DATA-WITHT)
    IF wa_bdc_map-cls_path IS NOT INITIAL.
      lv_fpath = wa_bdc_map-cls_path.
    ELSE.
      lv_fpath = wa_bdc_map-cls_field.
    ENDIF.
    " Check if param is a structure
    READ TABLE lt_cls_params INTO wa_cls_param
      WITH KEY param_name = wa_bdc_map-cls_param is_struct = 'X'.
    IF sy-subrc = 0.
      CONCATENATE 'ls_' wa_bdc_map-cls_param
                  '-' lv_fpath
                  ' = ' wa_bdc_map-fval_var '.'
        INTO lv_cls_assign SEPARATED BY space.
    ELSE.
      CONCATENATE wa_bdc_map-cls_param
                  ' = ' wa_bdc_map-fval_var '.'
        INTO lv_cls_assign SEPARATED BY space.
    ENDIF.
    CONCATENATE '" BDC/FM:' wa_bdc_map-fnam
                '→ Param:' wa_bdc_map-cls_param
                'Field:' wa_bdc_map-cls_field
                '(' wa_bdc_map-rollname ')'
      INTO lv_cls_cmt SEPARATED BY space.
    add_line: lv_cls_cmt.
    add_line: lv_cls_assign.
  ENDLOOP.
  add_line: ''.

  " TODO for unmatched
  DATA lv_todo_hdr TYPE flag.
  LOOP AT lt_bdc_map INTO wa_bdc_map WHERE matched = space.
    IF lv_todo_hdr = space.
      add_line: '" --- TODO: Could not auto-map these fields ---'.
      lv_todo_hdr = 'X'.
    ENDIF.
    DATA lv_cls_todo TYPE char200.
    CONCATENATE '" TODO: map' wa_bdc_map-fnam
      '(val=' wa_bdc_map-fval_var
      'roll=' wa_bdc_map-rollname ')'
      INTO lv_cls_todo SEPARATED BY space.
    add_line: lv_cls_todo.
  ENDLOOP.
  add_line: ''.

  " Method call — static or instance
  DATA lv_call_stmt TYPE char200.
  IF p_static = 'X'.
    CONCATENATE p_class '=>' p_meth '('
      INTO lv_call_stmt.
  ELSE.
    CONCATENATE 'lo_obj->' p_meth '('
      INTO lv_call_stmt.
    lv_ln = |DATA lo_obj TYPE REF TO { p_class }.|. add_line: lv_ln.
    lv_ln = |CREATE OBJECT lo_obj.|. add_line: lv_ln.
  ENDIF.
  add_line: lv_call_stmt.

  " Parameters in call
  LOOP AT lt_cls_params INTO wa_cls_param.
    DATA lv_prm TYPE char200.
    CASE wa_cls_param-param_dir.
      WHEN 'I' OR 'C'.  " Importing / Changing
        IF wa_cls_param-is_struct = 'X'.
          CONCATENATE '  EXPORTING ' wa_cls_param-param_name
            '= ls_' wa_cls_param-param_name
            INTO lv_prm SEPARATED BY space.
        ELSE.
          CONCATENATE '  EXPORTING ' wa_cls_param-param_name
            '= " TODO: fill value'
            INTO lv_prm SEPARATED BY space.
        ENDIF.
      WHEN 'E'.  " Exporting
        CONCATENATE '  IMPORTING ' wa_cls_param-param_name
          '= lv_' wa_cls_param-param_name
          INTO lv_prm SEPARATED BY space.
      WHEN 'R'.  " Returning
        CONCATENATE '  RECEIVING result = lv_result "'
          wa_cls_param-type_name
          INTO lv_prm SEPARATED BY space.
    ENDCASE.
    add_line: lv_prm.
  ENDLOOP.
  add_line: ').'.
  add_line: ''.
  add_line: '" ** end of change **'.
ENDFORM.

*----------------------------------------------------------------------*
*& Form generate_code_preview
*& Generate the replacement ABAP code as a preview
*----------------------------------------------------------------------*
FORM generate_code_preview.
  DATA: lt_params   TYPE TABLE OF fupararef,
        lt_structs  TYPE SORTED TABLE OF char50 WITH UNIQUE KEY table_line.
  DATA lv_ln TYPE string.

  lv_code_ctr = 1.

  " Helper macro to add a code line
  DEFINE add_line.
    wa_code-lineno = lv_code_ctr.
    wa_code-code   = &1.
    APPEND wa_code TO lt_code.
    lv_code_ctr = lv_code_ctr + 1.
  END-OF-DEFINITION.

  lv_ln = |" ** begin of change - BDC -> FM replacement **|. add_line: lv_ln.
  lv_ln = |" TODO: Replace CALL TRANSACTION '{ p_tcode }' with { p_fm }|. add_line: lv_ln.
  add_line: ''.

  " Comment out BDC lines
  lv_ln = |" --- Original BDC block (lines { lv_bdc_start } - { lv_bdc_end }) commented out ---|. add_line: lv_ln.
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
     FROM fupararef
    WHERE funcname  = @p_fm
      AND paramtype IN ('I', 'E', 'T')
      AND structure IS NOT INITIAL
       INTO TABLE @DATA(lt_distinct_params).

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
  lv_ln = |CALL FUNCTION '{ p_fm }'|. add_line: lv_ln.
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
      lo_cols->get_column( 'CLS_PARAM' )->set_long_text( 'Class Method Parameter' ).
      lo_cols->get_column( 'CLS_FIELD' )->set_long_text( 'Class Param Field' ).
      lo_cols->get_column( 'GEN_CODE'  )->set_long_text( 'Generated Code Line' ).
      lo_cols->get_column( 'REMARK'    )->set_long_text( 'Remark' ).

      " Hide columns not relevant to current mode
      IF rb_fm = 'X'.
        lo_cols->get_column( 'CLS_PARAM' )->set_visible( if_salv_c_bool_sap=>false ).
        lo_cols->get_column( 'CLS_FIELD' )->set_visible( if_salv_c_bool_sap=>false ).
      ELSE.
        lo_cols->get_column( 'FM_PARAM'  )->set_visible( if_salv_c_bool_sap=>false ).
        lo_cols->get_column( 'FM_STRUCT' )->set_visible( if_salv_c_bool_sap=>false ).
        lo_cols->get_column( 'FM_FIELD'  )->set_visible( if_salv_c_bool_sap=>false ).
      ENDIF.

      DATA(lo_disp) = lo_map->get_display_settings( ).
      IF rb_fm = 'X'.
        lo_disp->set_list_header( |BDC/FM → FM Mapping: → { p_fm }| ).
      ELSE.
        lo_disp->set_list_header( |BDC/FM → Class Method Mapping: { p_class }=>{ p_meth }| ).
      ENDIF.

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
