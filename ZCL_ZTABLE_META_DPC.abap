*&---------------------------------------------------------------------*
*& Class  ZCL_ZTABLE_META_DPC  (Data Provider - code based, OData V2)
*&---------------------------------------------------------------------*
*& Runtime for the generic "table dictionary + data" service.
*&
*& CPI (or any client) sends a table name and gets back:
*&   GET /TableStructureSet?$filter=Tabname eq 'MARA'
*&       -> field-level DDIC metadata (one entity per field)
*&   GET /TableDataSet?$filter=Tabname eq 'MARA'&$top=100
*&       -> table records, each serialised to JSON in DataJson
*&
*& The table name is MANDATORY and is read from the $filter on Tabname.
*& $top / $skip are honoured for paging; if no $top is given a safe
*& default cap is applied to TableDataSet so a client cannot dump a
*& whole large table by accident.
*&
*& Structure  : function module DDIF_FIELDINFO_GET (DFIES characteristics).
*& Data       : dynamic SELECT * FROM (tabname), each row -> JSON via
*&              /ui2/cl_json=>serialize.
*& Security   : AUTHORITY-CHECK on S_TABU_NAM (display) before any read.
*&
*& Fully code based -> extends /IWBEP/CL_MGW_ABS_DATA (the standard
*& code-based data base, same one SEGW-generated DPCs use). The "push"
*& base /IWBEP/CL_MGW_PUSH_ABS_DATA is NOT used because it declares
*& abstract subscription methods that a plain read service must not have
*& to implement. Register with /IWFND/MAINT_SERVICE together with
*& ZCL_ZTABLE_META_MPC.
*&---------------------------------------------------------------------*
CLASS zcl_ztable_meta_dpc DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_mgw_abs_data
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~get_entityset REDEFINITION.

  PROTECTED SECTION.
    CONSTANTS gc_default_max_rows TYPE i VALUE 1000.   " cap when no $top sent
    CONSTANTS gc_hard_max_rows    TYPE i VALUE 50000.  " absolute ceiling

    " Extract the requested table name (or comma list) from $filter.
    METHODS get_tabname_from_filter
      IMPORTING it_filter_select_options TYPE /iwbep/t_mgw_select_option
      RETURNING VALUE(rv_tabname)        TYPE string.

    " Extract the optional WhereClause filter (Open SQL restriction).
    METHODS get_where_from_filter
      IMPORTING it_filter_select_options TYPE /iwbep/t_mgw_select_option
      RETURNING VALUE(rv_where)          TYPE string.

    " Extract the optional Fields filter (comma-separated column list).
    METHODS get_fields_from_filter
      IMPORTING it_filter_select_options TYPE /iwbep/t_mgw_select_option
      RETURNING VALUE(rv_fields)         TYPE string.

    " Extract the optional Join filter (join FROM expression).
    METHODS get_join_from_filter
      IMPORTING it_filter_select_options TYPE /iwbep/t_mgw_select_option
      RETURNING VALUE(rv_join)           TYPE string.

    " Build a reduced internal table containing only the requested fields
    " (validated against the table's DDIC columns) plus the column list.
    METHODS build_field_projection
      IMPORTING iv_tabname TYPE tabname
                iv_fields  TYPE string
      EXPORTING et_cols    TYPE string_table
                er_table   TYPE REF TO data
      RAISING   /iwbep/cx_mgw_busi_exception.

    " Build the result table for a join from qualified TAB~FIELD AS ALIAS
    " specs, resolving each field's type from its source table's DDIC.
    METHODS build_join_projection
      IMPORTING iv_tables TYPE string
                iv_fields TYPE string
      EXPORTING et_cols   TYPE string_table
                er_table  TYPE REF TO data
      RAISING   /iwbep/cx_mgw_busi_exception.

    " Raise a business exception with a single message text.
    METHODS raise_error
      IMPORTING iv_text TYPE string
      RAISING   /iwbep/cx_mgw_busi_exception.

    " Check display authorisation for each table in the (comma) list.
    METHODS check_table_authority
      IMPORTING iv_tables TYPE string
      RAISING   /iwbep/cx_mgw_busi_exception.

    " Read DDIC field metadata for the table.
    METHODS read_structure
      IMPORTING iv_tabname       TYPE tabname
      RETURNING VALUE(rt_fields) TYPE zcl_ztable_meta_mpc=>tt_field_info
      RAISING   /iwbep/cx_mgw_busi_exception.

    " Read table data dynamically and serialise each row to JSON.
    METHODS read_data
      IMPORTING iv_tabname     TYPE string
                iv_top         TYPE i
                iv_skip        TYPE i
                iv_where       TYPE string OPTIONAL
                iv_fields      TYPE string OPTIONAL
                iv_join        TYPE string OPTIONAL
      RETURNING VALUE(rt_rows) TYPE zcl_ztable_meta_mpc=>tt_table_row
      RAISING   /iwbep/cx_mgw_busi_exception.
ENDCLASS.


CLASS zcl_ztable_meta_dpc IMPLEMENTATION.

  METHOD /iwbep/if_mgw_appl_srv_runtime~get_entityset.

    DATA(lv_tabname) = me->get_tabname_from_filter( it_filter_select_options ).

    IF lv_tabname IS INITIAL.
      me->raise_error( |Table name is mandatory. Use $filter=Tabname eq 'MARA'.| ).
    ENDIF.

    " Normalise (DDIC names are upper case)
    TRANSLATE lv_tabname TO UPPER CASE.

    me->check_table_authority( lv_tabname ).

    CASE iv_entity_set_name.

      WHEN 'TableStructureSet'.
        " read_structure expects a single table name (C30); the structure
        " endpoint is always a single table, so convert from the string.
        DATA lv_struct_tab TYPE tabname.
        lv_struct_tab = lv_tabname.
        DATA(lt_fields) = me->read_structure( lv_struct_tab ).
        copy_data_to_ref( EXPORTING is_data = lt_fields
                          CHANGING  cr_data = er_entityset ).

      WHEN 'TableDataSet'.
        " Paging: honour $top / $skip, else apply the default cap.
        DATA lv_top  TYPE i.
        DATA lv_skip TYPE i.
        lv_skip = is_paging-skip.
        IF is_paging-top > 0.
          lv_top = is_paging-top.
        ELSE.
          lv_top = gc_default_max_rows.
        ENDIF.

        DATA(lv_where)  = me->get_where_from_filter( it_filter_select_options ).
        DATA(lv_fields) = me->get_fields_from_filter( it_filter_select_options ).
        DATA(lv_join)   = me->get_join_from_filter( it_filter_select_options ).

        DATA(lt_rows) = me->read_data( iv_tabname = lv_tabname
                                       iv_top     = lv_top
                                       iv_skip    = lv_skip
                                       iv_where   = lv_where
                                       iv_fields  = lv_fields
                                       iv_join    = lv_join ).
        copy_data_to_ref( EXPORTING is_data = lt_rows
                          CHANGING  cr_data = er_entityset ).

      WHEN OTHERS.
        me->raise_error( |Unknown entity set { iv_entity_set_name }.| ).

    ENDCASE.

  ENDMETHOD.


  METHOD get_tabname_from_filter.
    " Look for a select option on property Tabname / TABNAME and take its low value.
    LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<ls_filter>).
      IF <ls_filter>-property = 'TABNAME' OR <ls_filter>-property = 'Tabname'.
        READ TABLE <ls_filter>-select_options ASSIGNING FIELD-SYMBOL(<ls_range>) INDEX 1.
        IF sy-subrc = 0.
          rv_tabname = <ls_range>-low.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_where_from_filter.
    " Optional property WhereClause carries a raw Open SQL restriction,
    " e.g. WhereClause eq 'EBELN = ''4500000001'''. String literals must
    " be single-quoted with the quotes doubled by the caller.
    LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<ls_filter>).
      IF <ls_filter>-property = 'WHERECLAUSE' OR <ls_filter>-property = 'WhereClause'.
        READ TABLE <ls_filter>-select_options ASSIGNING FIELD-SYMBOL(<ls_range>) INDEX 1.
        IF sy-subrc = 0.
          rv_where = <ls_range>-low.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_fields_from_filter.
    " Optional property Fields carries a comma-separated column list,
    " e.g. Fields eq 'MATNR,MTART,MEINS'.
    LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<ls_filter>).
      IF <ls_filter>-property = 'FIELDS' OR <ls_filter>-property = 'Fields'.
        READ TABLE <ls_filter>-select_options ASSIGNING FIELD-SYMBOL(<ls_range>) INDEX 1.
        IF sy-subrc = 0.
          rv_fields = <ls_range>-low.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_field_projection.
    " Validate every requested field and resolve its type through DDIC via
    " 'TABLE-FIELD'. This also resolves fields that come from a .INCLUDE or
    " append structure (which get_components does NOT flatten). Build a
    " reduced result table with just those fields, in the requested order.
    DATA lt_names TYPE STANDARD TABLE OF string.
    DATA lt_comp  TYPE cl_abap_structdescr=>component_table.
    DATA ls_comp  TYPE abap_componentdescr.
    DATA lo_type  TYPE REF TO cl_abap_datadescr.

    DATA lv_tab TYPE string.
    lv_tab = iv_tabname.
    CONDENSE lv_tab.

    SPLIT iv_fields AT ',' INTO TABLE lt_names.
    LOOP AT lt_names INTO DATA(lv_name).
      CONDENSE lv_name.
      IF lv_name IS INITIAL.
        CONTINUE.
      ENDIF.
      DATA(lv_up) = to_upper( lv_name ).
      TRY.
          lo_type ?= cl_abap_typedescr=>describe_by_name( |{ lv_tab }-{ lv_up }| ).
        CATCH cx_root.
          me->raise_error( |Field { lv_name } does not exist in { iv_tabname }.| ).
      ENDTRY.
      CLEAR ls_comp.
      ls_comp-name = lv_up.
      ls_comp-type = lo_type.
      APPEND ls_comp TO lt_comp.
      APPEND lv_up TO et_cols.
    ENDLOOP.

    IF lt_comp IS INITIAL.
      me->raise_error( |No valid fields supplied in the Fields parameter.| ).
    ENDIF.

    TRY.
        DATA(lo_row_struct) = cl_abap_structdescr=>get( lt_comp ).
        DATA(lo_row_table)  = cl_abap_tabledescr=>create( lo_row_struct ).
        CREATE DATA er_table TYPE HANDLE lo_row_table.
      CATCH cx_root.
        me->raise_error( |Cannot build the projection (duplicate field?).| ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_join_from_filter.
    " Optional property Join carries the join FROM expression, e.g.
    " Join eq 'VBAK INNER JOIN VBAP ON VBAK~VBELN = VBAP~VBELN'.
    LOOP AT it_filter_select_options ASSIGNING FIELD-SYMBOL(<ls_filter>).
      IF <ls_filter>-property = 'JOIN' OR <ls_filter>-property = 'Join'.
        READ TABLE <ls_filter>-select_options ASSIGNING FIELD-SYMBOL(<ls_range>) INDEX 1.
        IF sy-subrc = 0.
          rv_join = <ls_range>-low.
          RETURN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_join_projection.
    " Fields are qualified specs: TAB~FIELD [AS ALIAS], comma separated.
    " Every referenced table must be in the Tabname list; every field must
    " exist in its table. Build a flat result structure keyed by the alias.
    DATA lt_specs   TYPE STANDARD TABLE OF string.
    DATA lt_allowed TYPE STANDARD TABLE OF string.
    DATA lt_comp    TYPE cl_abap_structdescr=>component_table.
    DATA ls_comp    TYPE abap_componentdescr.
    DATA lv_off     TYPE i.

    SPLIT iv_tables AT ',' INTO TABLE lt_allowed.
    LOOP AT lt_allowed ASSIGNING FIELD-SYMBOL(<lv_a>).
      CONDENSE <lv_a>.
      <lv_a> = to_upper( <lv_a> ).
    ENDLOOP.

    SPLIT iv_fields AT ',' INTO TABLE lt_specs.
    LOOP AT lt_specs INTO DATA(lv_spec).
      CONDENSE lv_spec.
      IF lv_spec IS INITIAL.
        CONTINUE.
      ENDIF.

      " Separate the optional  ' AS <alias>'  suffix.
      DATA lv_src   TYPE string.
      DATA lv_alias TYPE string.
      CLEAR: lv_src, lv_alias.
      DATA(lv_up) = to_upper( lv_spec ).
      FIND FIRST OCCURRENCE OF ` AS ` IN lv_up MATCH OFFSET lv_off.
      IF sy-subrc = 0.
        lv_src   = lv_spec(lv_off).
        lv_alias = lv_spec+lv_off.
        SHIFT lv_alias LEFT BY 4 PLACES.       " drop ' AS '
      ELSE.
        lv_src = lv_spec.
      ENDIF.
      CONDENSE lv_src.
      CONDENSE lv_alias.

      " Split TAB~FIELD.
      DATA lv_tab   TYPE string.
      DATA lv_field TYPE string.
      IF lv_src CS '~'.
        SPLIT lv_src AT '~' INTO lv_tab lv_field.
        CONDENSE lv_tab.
        CONDENSE lv_field.
      ELSE.
        me->raise_error( |Join field '{ lv_spec }' must be qualified as TABLE~FIELD.| ).
      ENDIF.

      " Table must be one of the declared (authorised) tables.
      DATA(lv_tab_up) = to_upper( lv_tab ).
      READ TABLE lt_allowed TRANSPORTING NO FIELDS WITH KEY table_line = lv_tab_up.
      IF sy-subrc <> 0.
        me->raise_error( |Table { lv_tab } (field { lv_field }) is not in the Tabname list.| ).
      ENDIF.

      " Resolve the field's type through DDIC via TABLE-FIELD (also resolves
      " .INCLUDE / append fields, which get_components does not flatten).
      DATA(lv_field_up) = to_upper( lv_field ).
      DATA lo_ftype TYPE REF TO cl_abap_datadescr.
      TRY.
          lo_ftype ?= cl_abap_typedescr=>describe_by_name( |{ lv_tab_up }-{ lv_field_up }| ).
        CATCH cx_root.
          me->raise_error( |Field { lv_field } does not exist in { lv_tab }.| ).
      ENDTRY.

      IF lv_alias IS INITIAL.
        lv_alias = lv_field.
      ENDIF.
      DATA(lv_alias_up) = to_upper( lv_alias ).

      CLEAR ls_comp.
      ls_comp-name = lv_alias_up.
      ls_comp-type = lo_ftype.
      APPEND ls_comp TO lt_comp.

      APPEND |{ lv_tab }~{ lv_field } AS { lv_alias_up }| TO et_cols.
    ENDLOOP.

    IF lt_comp IS INITIAL.
      me->raise_error( |No valid fields supplied for the join.| ).
    ENDIF.

    TRY.
        DATA(lo_row_struct) = cl_abap_structdescr=>get( lt_comp ).
        DATA(lo_row_table)  = cl_abap_tabledescr=>create( lo_row_struct ).
        CREATE DATA er_table TYPE HANDLE lo_row_table.
      CATCH cx_root.
        me->raise_error( |Cannot build the join result (duplicate alias in Fields?).| ).
    ENDTRY.
  ENDMETHOD.


  METHOD check_table_authority.
    " iv_tables may be a single table or a comma-separated list (join).
    DATA lt_tab TYPE STANDARD TABLE OF string.
    SPLIT iv_tables AT ',' INTO TABLE lt_tab.
    LOOP AT lt_tab INTO DATA(lv_t).
      CONDENSE lv_t.
      IF lv_t IS INITIAL.
        CONTINUE.
      ENDIF.
      DATA lv_tabnam TYPE tabname.
      lv_tabnam = to_upper( lv_t ).
      AUTHORITY-CHECK OBJECT 'S_TABU_NAM'
        ID 'ACTVT' FIELD '03'
        ID 'TABLE' FIELD lv_tabnam.
      IF sy-subrc <> 0.
        me->raise_error( |No display authorisation (S_TABU_NAM) for table { lv_tabnam }.| ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD read_structure.
    DATA: lt_dfies TYPE STANDARD TABLE OF dfies,
          ls_field TYPE zcl_ztable_meta_mpc=>ty_field_info.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = iv_tabname
        langu          = sy-langu
      TABLES
        dfies_tab      = lt_dfies
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.

    IF sy-subrc <> 0 OR lt_dfies IS INITIAL.
      me->raise_error( |Table / structure { iv_tabname } not found in the dictionary.| ).
    ENDIF.

    LOOP AT lt_dfies ASSIGNING FIELD-SYMBOL(<ls_dfies>).
      CLEAR ls_field.
      ls_field-tabname    = <ls_dfies>-tabname.
      ls_field-fieldname  = <ls_dfies>-fieldname.
      ls_field-position   = <ls_dfies>-position.
      ls_field-keyflag    = <ls_dfies>-keyflag.
      ls_field-rollname   = <ls_dfies>-rollname.
      ls_field-domname    = <ls_dfies>-domname.
      ls_field-datatype   = <ls_dfies>-datatype.
      ls_field-leng       = <ls_dfies>-leng.
      ls_field-decimals   = <ls_dfies>-decimals.
      ls_field-inttype    = <ls_dfies>-inttype.
      ls_field-intlen     = <ls_dfies>-intlen.
      ls_field-lowercase  = <ls_dfies>-lowercase.
      ls_field-signflag   = <ls_dfies>-sign.
      ls_field-checktable = <ls_dfies>-checktable.
      ls_field-reftable   = <ls_dfies>-reftable.
      ls_field-reffield   = <ls_dfies>-reffield.
      ls_field-convexit   = <ls_dfies>-convexit.
      ls_field-fieldtext  = <ls_dfies>-fieldtext.
      ls_field-scrtext_l  = <ls_dfies>-scrtext_l.
      APPEND ls_field TO rt_fields.
    ENDLOOP.
  ENDMETHOD.


  METHOD read_data.
    DATA: lr_table TYPE REF TO data,
          lv_rowno TYPE i,
          ls_row   TYPE zcl_ztable_meta_mpc=>ty_table_row.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE,
                   <ls_data> TYPE any.

    " Paging cap. UP TO n ROWS caps the result set; skipping is done in
    " ABAP after fetch (portable across DBs).
    DATA lv_fetch TYPE i.
    lv_fetch = iv_top + iv_skip.
    IF lv_fetch <= 0 OR lv_fetch > gc_hard_max_rows.
      lv_fetch = gc_hard_max_rows.
    ENDIF.

    " Optional dynamic WHERE (table form avoids length limits). An empty
    " table means no restriction (all rows).
    " Convenience: a double quote (") is accepted as the string-literal
    " delimiter and translated to a single quote, so the caller can write
    "   WhereClause eq 'MANDT = "100"'
    " instead of doubling single quotes for OData
    "   WhereClause eq 'MANDT = ''100'''
    " (the doubled form still works - single quotes are left untouched).
    DATA lt_where TYPE STANDARD TABLE OF string.
    IF iv_where IS NOT INITIAL.
      DATA lv_where TYPE string.
      lv_where = iv_where.
      REPLACE ALL OCCURRENCES OF '"' IN lv_where WITH ''''.
      APPEND lv_where TO lt_where.
    ENDIF.

    DATA lt_cols TYPE string_table.   " selected column list (empty = all)
    DATA lv_from TYPE string.         " single table OR the join expression

    IF iv_join IS NOT INITIAL.
      "--- JOIN mode: Fields is mandatory and must be TAB~FIELD [AS ALIAS];
      "    the join expression itself becomes the dynamic FROM clause.
      IF iv_fields IS INITIAL.
        me->raise_error( |Fields (qualified TAB~FIELD AS ALIAS) is mandatory for a join.| ).
      ENDIF.
      me->build_join_projection(
        EXPORTING iv_tables = iv_tabname
                  iv_fields = iv_fields
        IMPORTING et_cols   = lt_cols
                  er_table  = lr_table ).
      lv_from = iv_join.
      " Same convenience: allow " as string-literal delimiter in the join.
      REPLACE ALL OCCURRENCES OF '"' IN lv_from WITH ''''.
    ELSE.
      "--- Single-table mode (behaviour unchanged).
      DATA lv_single TYPE tabname.
      lv_single = iv_tabname.
      SELECT SINGLE tabname FROM dd02l INTO @DATA(lv_dummy)
        WHERE tabname  = @lv_single
          AND tabclass IN ( 'TRANSP', 'POOL', 'CLUSTER', 'VIEW' ).
      IF sy-subrc <> 0.
        SELECT SINGLE viewname FROM dd25l INTO @DATA(lv_vdummy)
          WHERE viewname = @lv_single.
        IF sy-subrc <> 0.
          me->raise_error( |{ lv_single } is not a selectable table or view.| ).
        ENDIF.
      ENDIF.
      IF iv_fields IS NOT INITIAL.
        me->build_field_projection(
          EXPORTING iv_tabname = lv_single
                    iv_fields  = iv_fields
          IMPORTING et_cols    = lt_cols
                    er_table   = lr_table ).
      ELSE.
        TRY.
            CREATE DATA lr_table TYPE STANDARD TABLE OF (lv_single).
          CATCH cx_sy_create_data_error.
            me->raise_error( |Cannot build a work area for { lv_single }.| ).
        ENDTRY.
      ENDIF.
      lv_from = lv_single.
    ENDIF.

    ASSIGN lr_table->* TO <lt_data>.

    " Dynamic read. Empty lt_cols = all columns; empty lt_where = all rows.
    TRY.
        SELECT (lt_cols) FROM (lv_from)
          INTO CORRESPONDING FIELDS OF TABLE <lt_data>
          UP TO lv_fetch ROWS
          WHERE (lt_where).
      CATCH cx_sy_dynamic_osql_error INTO DATA(lx_sql).
        me->raise_error( |Read failed: { lx_sql->get_text( ) }| ).
    ENDTRY.

    LOOP AT <lt_data> ASSIGNING <ls_data>.
      lv_rowno = sy-tabix.
      " apply $skip
      IF lv_rowno <= iv_skip.
        CONTINUE.
      ENDIF.
      CLEAR ls_row.
      ls_row-tabname     = iv_tabname.
      ls_row-row_no      = lv_rowno.
      ls_row-whereclause = iv_where.          " echo the applied filter
      ls_row-fields      = iv_fields.         " echo the selected fields
      ls_row-join        = iv_join.           " echo the join expression
      ls_row-data_json   = /ui2/cl_json=>serialize(
                             data        = <ls_data>
                             compress    = abap_false
                             pretty_name = /ui2/cl_json=>pretty_mode-none ).
      APPEND ls_row TO rt_rows.
    ENDLOOP.
  ENDMETHOD.


  METHOD raise_error.
    " iv_msg_text is typed C(220) (BAPI_MSG) in the message container API,
    " so convert the string into a fixed-length field before passing it.
    DATA lv_msg TYPE bapi_msg.
    lv_msg = iv_text.
    DATA(lo_mc) = mo_context->get_message_container( ).
    lo_mc->add_message_text_only( iv_msg_type = 'E'
                                  iv_msg_text = lv_msg ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING message_container = lo_mc.
  ENDMETHOD.

ENDCLASS.
