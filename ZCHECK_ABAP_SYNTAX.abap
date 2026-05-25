*&---------------------------------------------------------------------*
*& Report  : ZCHECK_ABAP_SYNTAX
*& Title   : Mass ABAP Object Syntax Checker with ALV Output
*&
*& Supported object types
*&   PROG  Programs / Reports / Includes   → checked as-is
*&   FUGR  Function Groups                 → checks SAPL<fugr>
*&   CLAS  Global ABAP Classes             → checks class pool
*&   INTF  Global ABAP Interfaces          → checks interface pool
*&   TRAN  Transaction Codes               → looks up program in TSTC,
*&                                           then checks that program
*&
*& Syntax-check mechanism: native ABAP SYNTAX-CHECK FOR PROGRAM statement
*&   Reads source directly from the repository; handles all program
*&   types (E, F, K, J, I) without requiring source to be supplied.
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
    err_msg  TYPE c LENGTH 220,
  END OF ty_result.

*======================================================================*
*  GLOBAL DATA
*======================================================================*
DATA:
  gt_result  TYPE TABLE OF ty_result,
  gv_objname TYPE tadir-obj_name.   " reference field for SELECT-OPTIONS
" Note: gv_blk1/2 and gv_t010-014 are auto-declared by SELECTION-SCREEN

*======================================================================*
*  SELECTION SCREEN
*======================================================================*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE gv_blk1.
  SELECT-OPTIONS s_obj FOR gv_objname.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE gv_blk2.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS rb_auto TYPE c RADIOBUTTON GROUP grp DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 3(52) gv_t010 FOR FIELD rb_auto.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS rb_prog TYPE c RADIOBUTTON GROUP grp.
    SELECTION-SCREEN COMMENT 3(52) gv_t011 FOR FIELD rb_prog.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS rb_fugr TYPE c RADIOBUTTON GROUP grp.
    SELECTION-SCREEN COMMENT 3(52) gv_t012 FOR FIELD rb_fugr.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS rb_clas TYPE c RADIOBUTTON GROUP grp.
    SELECTION-SCREEN COMMENT 3(52) gv_t013 FOR FIELD rb_clas.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS rb_intf TYPE c RADIOBUTTON GROUP grp.
    SELECTION-SCREEN COMMENT 3(52) gv_t014 FOR FIELD rb_intf.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS rb_tran TYPE c RADIOBUTTON GROUP grp.
    SELECTION-SCREEN COMMENT 3(52) gv_t015 FOR FIELD rb_tran.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.

*======================================================================*
*  INITIALIZATION
*======================================================================*
INITIALIZATION.
  gv_blk1 = 'Object Name Selection'.
  gv_blk2 = 'Object Type Filter'.
  gv_t010 = 'Auto Detect (PROG / FUGR / CLAS / INTF)'.
  gv_t011 = 'Programs / Reports / Includes  (PROG)'.
  gv_t012 = 'Function Groups                (FUGR)'.
  gv_t013 = 'ABAP OO Classes                (CLAS)'.
  gv_t014 = 'ABAP OO Interfaces             (INTF)'.
  gv_t015 = 'Transaction Codes              (TRAN)'.

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
    lt_types TYPE RANGE OF tadir-object,
    lt_tstc  TYPE TABLE OF tstc,
    ls_tstc  TYPE tstc.

  "--------------------------------------------------------------------
  " TRAN: look up program behind each transaction code in TSTC
  "--------------------------------------------------------------------
  IF rb_tran = abap_true.
    SELECT tcode pgmna
      FROM tstc
      INTO CORRESPONDING FIELDS OF TABLE lt_tstc
      WHERE tcode IN s_obj.

    IF sy-subrc <> 0 OR lt_tstc IS INITIAL.
      MESSAGE 'No matching transaction codes found in TSTC.' TYPE 'I'.
      RETURN.
    ENDIF.

    LOOP AT lt_tstc INTO ls_tstc.
      IF ls_tstc-pgmna IS INITIAL.
        " Skip tcodes with no program (e.g. parameter transactions)
        CONTINUE.
      ENDIF.
      PERFORM check_one_object USING    ls_tstc-tcode
                                        'TRAN'
                                        ls_tstc-pgmna
                               CHANGING gt_result.
    ENDLOOP.

  ELSE.
    "--------------------------------------------------------------------
    " PROG / FUGR / CLAS / INTF: build type range and query TADIR
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

    LOOP AT lt_tadir INTO DATA(ls_tadir).
      PERFORM check_one_object USING    ls_tadir-obj_name
                                        ls_tadir-object
                                        ''
                               CHANGING gt_result.
    ENDLOOP.
  ENDIF.

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
     USING    iv_objname TYPE tadir-obj_name  " Object name shown in result
              iv_objtype TYPE tadir-object    " PROG/FUGR/CLAS/INTF/TRAN
              iv_prog    TYPE c               " Program to check (TRAN only)
     CHANGING ct_result  TYPE TABLE.          " table of ty_result

  DATA:
    ls_result   TYPE ty_result,
    lv_prog     TYPE c LENGTH 40,
    lv_subrc    TYPE sy-subrc,
    lv_line     TYPE i,
    lv_plen     TYPE i,
    lv_pad      TYPE string,
    lv_cnt      TYPE i,
    lv_chk_msg  TYPE c LENGTH 220,
    lv_chk_word TYPE c LENGTH 72.

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

    WHEN 'TRAN'.
      " Program name comes directly from TSTC-PGMNA (passed as iv_prog)
      lv_prog = iv_prog.

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
  " Native ABAP SYNTAX-CHECK FOR PROGRAM
  " Works correctly for ALL program types:
  "   E  Executable (REPORT)    F  Function Group
  "   K  Class Pool             J  Interface Pool
  "   I  Include
  " RS_SYNTAX_CHECK was replaced because passing empty i_source caused
  " it to check empty code and return "REPORT/PROGRAM missing" for all
  " object types, instead of reading from the repository.
  "--------------------------------------------------------------------
  SYNTAX-CHECK FOR PROGRAM lv_prog
    MESSAGE lv_chk_msg
    LINE    lv_line
    WORD    lv_chk_word.

  lv_subrc = sy-subrc.

  "--------------------------------------------------------------------
  " Build result row
  "--------------------------------------------------------------------
  IF lv_subrc = 0.
    ls_result-traffic = '3'.
    ls_result-status  = 'OK'.
  ELSE.
    ls_result-traffic  = '1'.
    ls_result-status   = 'ERROR'.
    ls_result-err_line = lv_line.
    ls_result-err_msg  = lv_chk_msg.
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

      " TRAFFIC field (1/3) is internal only – hide it; STATUS shows OK/ERROR
      TRY.
          lo_col ?= lo_cols->get_column( 'TRAFFIC' ).
          lo_col->set_technical( abap_true ).
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
      PERFORM set_col USING lo_cols 'ERR_MSG'  'Error Message'
                            'Error Message' 'Err Msg'.

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
