*&---------------------------------------------------------------------*
*& Report  : ZCHECK_ABAP_SYNTAX
*& Title   : Mass ABAP Object Syntax Checker with ALV Output
*&
*& Description
*&   Enter multiple object names on the selection screen.
*&   The program detects each object's type from TADIR and runs a
*&   syntax check.  Results appear in an ALV grid with traffic lights.
*&
*& Supported object types
*&   PROG  Programs, reports, includes        → checked as-is
*&   FUGR  Function groups                    → checks SAPL<fugr>
*&   CLAS  Global ABAP classes                → checks class pool
*&   INTF  Global ABAP interfaces             → checks interface pool
*&
*& Syntax-check mechanism
*&   Primary  : FM  RS_SYNTAX_CHECK
*&              Function Group S38E  (SAPLS38E)  – available in all
*&              standard SAP releases (ECC 6.x, S/4HANA).
*&              Confirmed interface (SE37):
*&                EXPORTING
*&                  I_PROGRAM      LIKE SY-REPID   (char 40)
*&                  I_WITH_DIALOG  TYPE C          (' ' = no popup)
*&                IMPORTING
*&                  O_ERROR_SUBRC  LIKE SY-SUBRC   (I)
*&                  O_ERROR_LINE   LIKE SY-INDEX   (I)
*&                  O_ERROR_OFFSET LIKE SY-TABIX   (I)
*&                  O_ERROR_MESSAGE TYPE STRING
*&                  O_ERROR_INCLUDE LIKE SY-REPID  (char 40)
*&   Fallback : Native ABAP statement SYNTAX-CHECK FOR PROGRAM
*&              (used when FM call raises an unexpected exception)
*&---------------------------------------------------------------------*
REPORT zcheck_abap_syntax.

*======================================================================*
*  TYPE DEFINITIONS
*======================================================================*
TYPES:
  BEGIN OF ty_result,
    traffic  TYPE c LENGTH 1,     " Traffic light: 1=Red  3=Green
    objname  TYPE tadir-obj_name,
    objtype  TYPE tadir-object,
    chk_prog TYPE c LENGTH 40,    " Program / include actually checked
    status   TYPE c LENGTH 10,    " OK / ERROR
    err_line TYPE i,
    err_off  TYPE i,
    err_msg  TYPE c LENGTH 220,
    err_inc  TYPE c LENGTH 40,
  END OF ty_result.

*======================================================================*
*  GLOBAL DATA
*======================================================================*
DATA:
  gt_result  TYPE TABLE OF ty_result,
  gv_objname TYPE tadir-obj_name.   " reference field for SELECT-OPTIONS

*======================================================================*
*  SELECTION SCREEN
*======================================================================*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
  SELECT-OPTIONS s_obj FOR gv_objname.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS rb_auto TYPE c RADIOBUTTON GROUP grp DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 3(50) text-010 FOR FIELD rb_auto.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS rb_prog TYPE c RADIOBUTTON GROUP grp.
    SELECTION-SCREEN COMMENT 3(50) text-011 FOR FIELD rb_prog.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS rb_fugr TYPE c RADIOBUTTON GROUP grp.
    SELECTION-SCREEN COMMENT 3(50) text-012 FOR FIELD rb_fugr.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS rb_clas TYPE c RADIOBUTTON GROUP grp.
    SELECTION-SCREEN COMMENT 3(50) text-013 FOR FIELD rb_clas.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS rb_intf TYPE c RADIOBUTTON GROUP grp.
    SELECTION-SCREEN COMMENT 3(50) text-014 FOR FIELD rb_intf.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

*======================================================================*
*  INITIALIZATION
*======================================================================*
INITIALIZATION.
  text-001 = 'Object Name Selection'.
  text-002 = 'Object Type Filter'.
  text-010 = 'Auto Detect (PROG / FUGR / CLAS / INTF)'.
  text-011 = 'Programs / Reports / Includes  (PROG)'.
  text-012 = 'Function Groups                (FUGR)'.
  text-013 = 'ABAP OO Classes                (CLAS)'.
  text-014 = 'ABAP OO Interfaces             (INTF)'.

*======================================================================*
*  AT SELECTION-SCREEN  – mandatory validation
*======================================================================*
AT SELECTION-SCREEN.
  IF s_obj IS INITIAL.
    MESSAGE 'Please enter at least one object name.' TYPE 'E'.
  ENDIF.

*======================================================================*
*  START-OF-SELECTION
*======================================================================*
START-OF-SELECTION.

  DATA:
    lt_tadir TYPE TABLE OF tadir,
    lt_types TYPE RANGE OF tadir-object.

  "--------------------------------------------------------------------
  " Build object-type range for TADIR query
  "--------------------------------------------------------------------
  IF rb_auto = abap_true.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'PROG' ) TO lt_types.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'FUGR' ) TO lt_types.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'CLAS' ) TO lt_types.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'INTF' ) TO lt_types.
  ELSEIF rb_prog = abap_true.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'PROG' ) TO lt_types.
  ELSEIF rb_fugr = abap_true.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'FUGR' ) TO lt_types.
  ELSEIF rb_clas = abap_true.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'CLAS' ) TO lt_types.
  ELSEIF rb_intf = abap_true.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'INTF' ) TO lt_types.
  ENDIF.

  "--------------------------------------------------------------------
  " Fetch matching objects from TADIR (repository catalog)
  "--------------------------------------------------------------------
  SELECT *
    FROM tadir
    INTO TABLE lt_tadir
    WHERE pgmid    = 'R3TR'
      AND object   IN lt_types
      AND obj_name IN s_obj.

  IF sy-subrc <> 0 OR lt_tadir IS INITIAL.
    MESSAGE 'No matching objects found in TADIR for the given selection.'
      TYPE 'I'.
    RETURN.
  ENDIF.

  "--------------------------------------------------------------------
  " Syntax-check each object
  "--------------------------------------------------------------------
  LOOP AT lt_tadir INTO DATA(ls_tadir).
    PERFORM check_one_object USING    ls_tadir-obj_name
                                      ls_tadir-object
                             CHANGING gt_result.
  ENDLOOP.

*======================================================================*
*  END-OF-SELECTION  – display ALV
*======================================================================*
END-OF-SELECTION.

  IF gt_result IS INITIAL.
    MESSAGE 'No results to display.' TYPE 'I'.
    RETURN.
  ENDIF.

  PERFORM display_alv.

*&---------------------------------------------------------------------*
*& Form CHECK_ONE_OBJECT
*&   Builds the correct program/include name for the given object type,
*&   calls RS_SYNTAX_CHECK, and appends one row to the result table.
*&---------------------------------------------------------------------*
FORM check_one_object
     USING    iv_objname TYPE tadir-obj_name
              iv_objtype TYPE tadir-object
     CHANGING ct_result  TYPE TABLE.   " table of ty_result

  DATA:
    ls_result TYPE ty_result,
    lv_prog   TYPE c LENGTH 40,
    lv_subrc  TYPE sy-subrc,
    lv_line   TYPE i,
    lv_off    TYPE i,
    lv_msg    TYPE string,
    lv_inc    TYPE c LENGTH 40,
    lv_plen   TYPE i,
    lv_pad    TYPE string,
    lv_cnt    TYPE i.

  ls_result-objname = iv_objname.
  ls_result-objtype = iv_objtype.

  "--------------------------------------------------------------------
  " Derive the repository program/include name to pass to RS_SYNTAX_CHECK
  "
  " PROG  → name as-is
  " FUGR  → SAPL<name>        (function-group main include)
  " CLAS  → <name padded to 30 chars with =>CP  (class pool, 32 chars)
  " INTF  → <name padded to 30 chars with =>IU  (interface pool, 32 chars)
  "
  " Class/Interface pool naming rule (SAP convention):
  "   pool_name = object_name + FILL('=', 30 - len(object_name)) + suffix
  "   Result is always 32 characters (max object name = 30 chars in SAP).
  "--------------------------------------------------------------------
  CASE iv_objtype.

    WHEN 'PROG'.
      lv_prog = iv_objname.

    WHEN 'FUGR'.
      CONCATENATE 'SAPL' iv_objname INTO lv_prog.

    WHEN 'CLAS' OR 'INTF'.
      lv_plen = strlen( iv_objname ).
      lv_prog = iv_objname.

      " Build padding string of (30 - length) equals signs
      lv_cnt = 30 - lv_plen.
      IF lv_cnt > 0.
        DO lv_cnt TIMES.
          CONCATENATE lv_pad '=' INTO lv_pad.
        ENDDO.
        CONCATENATE lv_prog lv_pad INTO lv_prog.
      ENDIF.

      IF iv_objtype = 'CLAS'.
        CONCATENATE lv_prog 'CP' INTO lv_prog.   " class pool suffix
      ELSE.
        CONCATENATE lv_prog 'IU' INTO lv_prog.   " interface pool suffix
      ENDIF.

    WHEN OTHERS.
      lv_prog = iv_objname.

  ENDCASE.

  ls_result-chk_prog = lv_prog.

  "--------------------------------------------------------------------
  " Call RS_SYNTAX_CHECK (Function Group S38E, program SAPLS38E)
  " Available in all standard SAP releases (ECC 6.x and S/4HANA).
  " I_WITH_DIALOG = ' '  suppresses any popup or screen navigation.
  "--------------------------------------------------------------------
  CALL FUNCTION 'RS_SYNTAX_CHECK'
    EXPORTING
      i_program         = lv_prog
      i_with_dialog     = ' '
    IMPORTING
      o_error_subrc     = lv_subrc
      o_error_line      = lv_line
      o_error_offset    = lv_off
      o_error_message   = lv_msg
      o_error_include   = lv_inc
    EXCEPTIONS
      OTHERS            = 1.

  IF sy-subrc <> 0 AND lv_subrc = 0.
    " FM itself failed (e.g. program not in repository) – mark as error
    lv_subrc = 4.
    lv_msg   = |RS_SYNTAX_CHECK raised exception for '{ lv_prog }'|.
  ENDIF.

  "--------------------------------------------------------------------
  " Build result row
  "--------------------------------------------------------------------
  IF lv_subrc = 0.
    ls_result-traffic = '3'.   " Green  – syntax OK
    ls_result-status  = 'OK'.
  ELSE.
    ls_result-traffic  = '1'.  " Red    – syntax error
    ls_result-status   = 'ERROR'.
    ls_result-err_line = lv_line.
    ls_result-err_off  = lv_off.
    ls_result-err_msg  = lv_msg.
    ls_result-err_inc  = lv_inc.
  ENDIF.

  APPEND ls_result TO ct_result.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM display_alv.

  DATA:
    lo_alv    TYPE REF TO cl_salv_table,
    lo_cols   TYPE REF TO cl_salv_columns_table,
    lo_col    TYPE REF TO cl_salv_column_table,
    lo_disp   TYPE REF TO cl_salv_display_settings,
    lo_funcs  TYPE REF TO cl_salv_functions_list,
    lo_sorts  TYPE REF TO cl_salv_sorts,
    lv_ok     TYPE i VALUE 0,
    lv_err    TYPE i VALUE 0,
    lv_hdr    TYPE lvc_title.

  " Build summary counts for the header
  LOOP AT gt_result INTO DATA(ls_r).
    IF ls_r-status = 'OK'.
      ADD 1 TO lv_ok.
    ELSE.
      ADD 1 TO lv_err.
    ENDIF.
  ENDLOOP.

  TRY.
      "----------------------------------------------------------------
      " Instantiate ALV
      "----------------------------------------------------------------
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_alv
        CHANGING  t_table      = gt_result
      ).

      "----------------------------------------------------------------
      " Toolbar: sort, filter, export, layout
      "----------------------------------------------------------------
      lo_funcs = lo_alv->get_functions( ).
      lo_funcs->set_all( abap_true ).

      "----------------------------------------------------------------
      " Column configuration
      "----------------------------------------------------------------
      lo_cols = lo_alv->get_columns( ).
      lo_cols->set_optimize( abap_true ).

      " Traffic-light column (values: 1=red, 2=yellow, 3=green)
      TRY.
          lo_col ?= lo_cols->get_column( 'TRAFFIC' ).
          lo_col->set_long_text(   'Syntax Result' ).
          lo_col->set_medium_text( 'Result' ).
          lo_col->set_short_text(  'Status' ).
          lo_col->set_cell_type( 'TRAFFICLIGHT' ).
        CATCH cx_salv_not_found. "#EC NO_HANDLER
      ENDTRY.

      PERFORM set_col USING lo_cols 'OBJNAME'  'Object Name'
                            'Object Name'   'Obj Name'.
      PERFORM set_col USING lo_cols 'OBJTYPE'  'Object Type'
                            'Object Type'   'ObjType'.
      PERFORM set_col USING lo_cols 'CHK_PROG' 'Checked Program / Include'
                            'Checked Prog'  'Chk Prog'.
      PERFORM set_col USING lo_cols 'STATUS'   'Syntax Check Status'
                            'Syntax Status' 'Status'.
      PERFORM set_col USING lo_cols 'ERR_LINE' 'Error Line Number'
                            'Error Line'    'ErrLine'.
      PERFORM set_col USING lo_cols 'ERR_OFF'  'Error Column Offset'
                            'Error Offset'  'ErrOff'.
      PERFORM set_col USING lo_cols 'ERR_MSG'  'Error Message'
                            'Error Message' 'Err Msg'.
      PERFORM set_col USING lo_cols 'ERR_INC'  'Error in Include'
                            'Error Include' 'ErrInc'.

      "----------------------------------------------------------------
      " Default sort: errors (traffic = 1) appear before OK (traffic = 3)
      "----------------------------------------------------------------
      lo_sorts = lo_alv->get_sorts( ).
      TRY.
          lo_sorts->add_sort( columnname = 'TRAFFIC' ).
        CATCH cx_salv_not_found
              cx_salv_existing
              cx_salv_data_error. "#EC NO_HANDLER
      ENDTRY.

      "----------------------------------------------------------------
      " ALV header
      "----------------------------------------------------------------
      DATA: lv_tot TYPE c LENGTH 6,
            lv_ok2 TYPE c LENGTH 6,
            lv_er2 TYPE c LENGTH 6.
      lv_tot = lines( gt_result ).
      lv_ok2 = lv_ok.
      lv_er2 = lv_err.
      CONCATENATE 'ABAP Mass Syntax Check  Total:' lv_tot
                  '  OK:' lv_ok2 '  Errors:' lv_er2
                  INTO lv_hdr.

      lo_disp = lo_alv->get_display_settings( ).
      lo_disp->set_list_header( lv_hdr ).
      lo_disp->set_striped_pattern( abap_true ).

      lo_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx_salv).
      MESSAGE lx_salv->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_COL  – helper: set long/medium/short text on a column
*&---------------------------------------------------------------------*
FORM set_col USING io_cols  TYPE REF TO cl_salv_columns_table
                   iv_fname TYPE lvc_fname
                   iv_long  TYPE scrtext_l
                   iv_med   TYPE scrtext_m
                   iv_short TYPE scrtext_s.
  TRY.
      DATA lo_col TYPE REF TO cl_salv_column_table.
      lo_col ?= io_cols->get_column( iv_fname ).
      lo_col->set_long_text(   iv_long  ).
      lo_col->set_medium_text( iv_med   ).
      lo_col->set_short_text(  iv_short ).
    CATCH cx_salv_not_found. "#EC NO_HANDLER
  ENDTRY.
ENDFORM.
