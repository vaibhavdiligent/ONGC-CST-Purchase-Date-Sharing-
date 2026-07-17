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
*& Fully code based -> extends /IWBEP/CL_MGW_PUSH_ABS_DATA. Register with
*& /IWFND/MAINT_SERVICE together with ZCL_ZTABLE_META_MPC.
*&---------------------------------------------------------------------*
CLASS zcl_ztable_meta_dpc DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_mgw_push_abs_data
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS /iwbep/if_mgw_appl_srv_runtime~get_entityset REDEFINITION.

  PROTECTED SECTION.
    CONSTANTS gc_default_max_rows TYPE i VALUE 1000.   " cap when no $top sent
    CONSTANTS gc_hard_max_rows    TYPE i VALUE 50000.  " absolute ceiling

    " Extract the requested table name from the $filter select options.
    METHODS get_tabname_from_filter
      IMPORTING it_filter_select_options TYPE /iwbep/t_mgw_select_option
      RETURNING VALUE(rv_tabname)        TYPE tabname.

    " Extract the optional WhereClause filter (Open SQL restriction).
    METHODS get_where_from_filter
      IMPORTING it_filter_select_options TYPE /iwbep/t_mgw_select_option
      RETURNING VALUE(rv_where)          TYPE string.

    " Raise a business exception with a single message text.
    METHODS raise_error
      IMPORTING iv_text TYPE string
      RAISING   /iwbep/cx_mgw_busi_exception.

    " Check display authorisation for the table (S_TABU_NAM).
    METHODS check_table_authority
      IMPORTING iv_tabname TYPE tabname
      RAISING   /iwbep/cx_mgw_busi_exception.

    " Read DDIC field metadata for the table.
    METHODS read_structure
      IMPORTING iv_tabname       TYPE tabname
      RETURNING VALUE(rt_fields) TYPE zcl_ztable_meta_mpc=>tt_field_info
      RAISING   /iwbep/cx_mgw_busi_exception.

    " Read table data dynamically and serialise each row to JSON.
    METHODS read_data
      IMPORTING iv_tabname     TYPE tabname
                iv_top         TYPE i
                iv_skip        TYPE i
                iv_where       TYPE string OPTIONAL
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
        DATA(lt_fields) = me->read_structure( lv_tabname ).
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

        DATA(lv_where) = me->get_where_from_filter( it_filter_select_options ).

        DATA(lt_rows) = me->read_data( iv_tabname = lv_tabname
                                       iv_top     = lv_top
                                       iv_skip    = lv_skip
                                       iv_where   = lv_where ).
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


  METHOD check_table_authority.
    AUTHORITY-CHECK OBJECT 'S_TABU_NAM'
      ID 'ACTVT' FIELD '03'
      ID 'TABLE' FIELD iv_tabname.
    IF sy-subrc <> 0.
      me->raise_error( |No display authorisation (S_TABU_NAM) for table { iv_tabname }.| ).
    ENDIF.
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
    DATA: lr_table   TYPE REF TO data,
          lr_line    TYPE REF TO data,
          lv_rowno   TYPE i,
          ls_row     TYPE zcl_ztable_meta_mpc=>ty_table_row.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE,
                   <ls_data> TYPE any.

    " Make sure the object exists and is readable via Open SQL
    " (transparent / pooled / cluster table or a DDIC view).
    SELECT SINGLE tabname FROM dd02l INTO @DATA(lv_dummy)
      WHERE tabname  = @iv_tabname
        AND tabclass IN ( 'TRANSP', 'POOL', 'CLUSTER', 'VIEW' ).
    IF sy-subrc <> 0.
      SELECT SINGLE viewname FROM dd25l INTO @DATA(lv_vdummy)
        WHERE viewname = @iv_tabname.
      IF sy-subrc <> 0.
        me->raise_error( |{ iv_tabname } is not a selectable table or view.| ).
      ENDIF.
    ENDIF.

    " Build a dynamic internal table of the table's row type.
    TRY.
        CREATE DATA lr_table TYPE STANDARD TABLE OF (iv_tabname).
        CREATE DATA lr_line  TYPE (iv_tabname).
      CATCH cx_sy_create_data_error.
        me->raise_error( |Cannot build a work area for { iv_tabname }.| ).
    ENDTRY.

    ASSIGN lr_table->* TO <lt_data>.
    ASSIGN lr_line->*  TO <ls_data>.

    " Dynamic read with paging. UP TO n ROWS caps the result set;
    " skipping is done in ABAP after fetch (portable across DBs).
    DATA lv_fetch TYPE i.
    lv_fetch = iv_top + iv_skip.
    " Never fetch more than the hard ceiling in a single call.
    IF lv_fetch <= 0 OR lv_fetch > gc_hard_max_rows.
      lv_fetch = gc_hard_max_rows.
    ENDIF.

    " Optional dynamic WHERE (table form avoids length limits).
    DATA lt_where TYPE STANDARD TABLE OF string.
    IF iv_where IS NOT INITIAL.
      APPEND iv_where TO lt_where.
    ENDIF.

    TRY.
        IF lt_where IS INITIAL.
          SELECT * FROM (iv_tabname)
            INTO TABLE <lt_data>
            UP TO lv_fetch ROWS.
        ELSE.
          SELECT * FROM (iv_tabname)
            INTO TABLE <lt_data>
            UP TO lv_fetch ROWS
            WHERE (lt_where).
        ENDIF.
      CATCH cx_sy_dynamic_osql_error INTO DATA(lx_sql).
        me->raise_error( |Read failed for { iv_tabname }: { lx_sql->get_text( ) }| ).
    ENDTRY.

    LOOP AT <lt_data> ASSIGNING <ls_data>.
      lv_rowno = sy-tabix.
      " apply $skip
      IF lv_rowno <= iv_skip.
        CONTINUE.
      ENDIF.
      CLEAR ls_row.
      ls_row-tabname   = iv_tabname.
      ls_row-row_no    = lv_rowno.
      ls_row-data_json = /ui2/cl_json=>serialize(
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
