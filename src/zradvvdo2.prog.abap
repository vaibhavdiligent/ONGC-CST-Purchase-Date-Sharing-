*&---------------------------------------------------------------------*
*& Report ZRADVVDO2
*&---------------------------------------------------------------------*
*& Z Copy of Standard Program RADVVDO2
*& Title: Version Display/Comparison of Domains
*& Original Author: SAP
*& Development Class: SDBT
*& RS_VERS function modules replaced with local PERFORMs
*&---------------------------------------------------------------------*
REPORT zradvvdo2
  MESSAGE-ID da
  NO STANDARD PAGE HEADING.

***********************************************************************
*       V E R S I O N S V E R G L E I C H E R
*       für Domänen
***********************************************************************

INCLUDE zradvvhdr.
"Parameter-Definition für alle Vergleichsreports

* Makro, der zu eine Struktur X die Tabellen X_OBJ1, X_OBJ2, X_LIST
* X_BLOCK definiert.
DEFINE_VERSTABLES: DD01VD, DD07VD.
DEFINE_VERSTABLES: RSVRSHEAD.

*----------------------------------------------------------------------*
* Global Data Declarations (from Function Group SVEU - LSVEUTOP)
*----------------------------------------------------------------------*
TYPE-POOLS: seuvs, kkblo.

INCLUDE <icon>.

* Display stack for version comparison blocks
DATA: gv_disp_stack TYPE seuvs_t_disp_stack.

* Hide-Info for user command to determine display block
DATA: gv_blocknum TYPE i.

* Global data for a display block, for title display etc.
DATA: gv_block_info TYPE seuvs_block_info.

* Flag for technical fields display (set by "tech. display" CTRL F1)
DATA: gv_raw_display.

* Language info popup
DATA: gt_it002t LIKE t002t OCCURS 0 WITH HEADER LINE.
DATA: gv_list_type(10).
CONSTANTS: gc_lt_info LIKE gv_list_type VALUE 'INFO'.

* Color constants for display
CONSTANTS: gc_color_old   VALUE '3',  "Old version
           gc_color_new   VALUE '3',  "New version
           gc_color_equ   VALUE '2',  "Both versions equal
           gc_color_dif   VALUE '6',  "Differences in field
           gc_color_tit   VALUE '2',  "Title
           gc_color_foot  VALUE '2',  "Statistics output
           gc_color_null  VALUE '0',  "No color
           gc_intensiv_old  VALUE ' ',
           gc_intensiv_new  VALUE '1',
           gc_intensiv_equ  VALUE '0',
           gc_intensiv_tit  VALUE '0',
           gc_inverse_tit   VALUE '0',
           gc_intensiv_foot VALUE '0',
           gc_inverse_foot  VALUE '0'.

* Color values for version display
CONSTANTS: gc_col_vers1 TYPE seuvs_colors VALUE 'C31',  "Version 1
           gc_col_vers2 TYPE seuvs_colors VALUE 'C30',  "Version 2
           gc_col_null  TYPE seuvs_colors VALUE 'C00',  "Missing lines
           gc_col_equal TYPE seuvs_colors VALUE 'C20'.  "Unchanged lines

* Standard widths for attribute blocks
CONSTANTS: gc_attr_keyword_l TYPE i VALUE 18.
CONSTANTS: gc_attr_text_l    TYPE i VALUE 60.

* Minimum width for statistics output
CONSTANTS: gc_stat_width TYPE i VALUE 25.

* Index table for comparing two internal tables
DATA: BEGIN OF gs_index_struct.
        INCLUDE STRUCTURE ddcompare.
DATA:   op    TYPE i,     "Operation for Miller algorithm
        tabix TYPE i,     "Index for direct access
      END OF gs_index_struct.

* Global display mode status
TYPES: BEGIN OF ty_disp_mode,
         cmpmode,        "(F)ull, (D)elta display, ' ': Pure display
         colnum,         "1: single-column, 2: two-column, SPACE: no toggle
         maxwidth TYPE i, "Maximum width of all blocks
         tecdisp(1) TYPE c, "Offer "tech. display" function
         repeat(1) TYPE c,  "Display again from start
       END OF ty_disp_mode.

DATA: gs_disp_mode TYPE ty_disp_mode.

* Global tables for header display
DATA: gt_rsvrshead_fields TYPE slis_t_fieldcat_alv.
DATA: gs_rsvrshead_block  TYPE seuvs_block_info.

* Comparison operation constants
CONSTANTS: gc_insert TYPE i VALUE 1,
           gc_delete TYPE i VALUE 2,
           gc_update TYPE i VALUE 3.

* MACRO for icon with quick-info
DEFINE set_icon_and_quickinfo.
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name       = &1
      info       = &2
      add_stdinf = ' '
    IMPORTING
      result     = <attr>-status_icon
    EXCEPTIONS
      outputfield_too_short = 1
      OTHERS                = 0.
  IF sy-subrc = 1.
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name       = &1
        add_stdinf = ' '
      IMPORTING
        result     = <attr>-status_icon
      EXCEPTIONS
        OTHERS     = 0.
  ENDIF.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* Original report data declarations
*----------------------------------------------------------------------*
DATA rc LIKE sy-subrc.
DATA tech_fields TYPE seuvs_t_fieldnames.  " technical fields
DATA key_fields TYPE seuvs_t_fieldnames.   " key fields
DATA cb_fields TYPE seuvs_t_fieldnames.    " checkbox fields
DATA opt_fields TYPE seuvs_t_fieldnames.   " optional fields

*----------------------------------------------------------------------*
* Include with all converted FORM subroutines
*----------------------------------------------------------------------*
INCLUDE zradvvdo2_forms.

***********************************************************************
* MAIN PROGRAM LOGIC
***********************************************************************
START-OF-SELECTION.

* Authority-Check auf Anzeigeberechtigung
  PERFORM auth_check(radvvutl) USING 'DOMA' objname objnam2.

* Die nicht versorgten Parameter füllen.
  PERFORM fill_emtpy_par(radvvutl)
    CHANGING objname objnam2
             versno1 versno2
             log_dest compare.

* Zuerst die beiden Objekte lesen.
  PERFORM obj_read
    TABLES dd01vd_obj1 dd07vd_obj1
    USING objname versno1 SPACE CHANGING rc.

  IF compare = 'X'.
    PERFORM obj_read
      TABLES dd01vd_obj2 dd07vd_obj2
      USING objnam2 versno2 log_dest CHANGING rc.
  ELSE.
    dd01vd_obj2[] = dd01vd_obj1[].
    dd07vd_obj2[] = dd07vd_obj1[].
  ENDIF.

  DO.
*   Initialize display (replaces RS_VERS_DISPLAY_INIT)
    PERFORM z_vers_display_init.

*   Define title block (replaces RS_VERS_TITEL_DEF)
    PERFORM z_vers_titel_def
      USING objname objnam2 versno1 versno2
            infoln1b infoln2b log_dest rem_syst.

***********************************************************************
*   Header
***********************************************************************
    CLEAR: block_info.
    REFRESH: key_fields, tech_fields, cb_fields, opt_fields.

    " technical fields
    SPLIT 'VALEXI' AT ' ' INTO TABLE tech_fields.
    " OUTPUTSTYLETXT is displayed instead of OUTPUTSTYLE:
    APPEND 'OUTPUTSTYLE' TO tech_fields.

    " checkbox fields
    SPLIT 'LOWERCASE SIGNFLAG PROXYTYPE AMPMFORMAT' AT ' '
      INTO TABLE cb_fields.

    block_info-typ = 'A'.
    block_info-title = 'Allgemeine Eigenschaften'(s01).
    block_info-tabname = 'DD01VD'.

*   Define block (replaces RS_VERS_BLOCK_DEF)
    PERFORM z_vers_block_def
      USING block_info
      TABLES dd01vd_obj1 dd01vd_obj2 dd01vd_list
             key_fields tech_fields cb_fields opt_fields.

***********************************************************************
*   Festwerte
***********************************************************************
    READ TABLE dd01vd_obj1 INDEX 1.
    READ TABLE dd01vd_obj2 INDEX 1.

    IF dd01vd_obj1-valexi <> space OR dd01vd_obj2-valexi <> space.

      CLEAR block_info.
      REFRESH: key_fields, tech_fields, cb_fields, opt_fields.

      block_info-typ = 'T'.
      block_info-title = 'Festwerte:'(s02).
      block_info-tabname = 'DD07VD'.

*     Define block (replaces RS_VERS_BLOCK_DEF)
      PERFORM z_vers_block_def
        USING block_info
        TABLES dd07vd_obj1 dd07vd_obj2 dd07vd_list
               key_fields tech_fields cb_fields opt_fields.

    ENDIF.

*   Process display (replaces RS_VERS_DISPLAY_PROCESS)
    PERFORM z_vers_display_process
      USING compare
      CHANGING repeat_display.

    IF repeat_display = space.
      EXIT.
    ENDIF.
  ENDDO.

*----------------------------------------------------------------------*
*  FORM OBJ_READ
*----------------------------------------------------------------------*
FORM obj_read
  TABLES dd01vd_tab STRUCTURE dd01vd
         dd07vd_tab STRUCTURE dd07vd
  USING  objname versno log_dest
  CHANGING rc.

  DATA: dd07v_tab  LIKE dd07v  OCCURS 0 WITH HEADER LINE.
  DATA: dd01v_tab  LIKE dd01v  OCCURS 0 WITH HEADER LINE.
  DATA: dd07tv_tab LIKE dd07tv OCCURS 0 WITH HEADER LINE.
  DATA: dd01tv_tab LIKE dd01tv OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'SVRS_GET_VERSION_DOMD_40'
    EXPORTING
      destination  = log_dest
      object_name  = objname
      versno       = versno
    TABLES
      dd01v_tab    = dd01v_tab
      dd07v_tab    = dd07v_tab
      dd01tv_tab   = dd01tv_tab
      dd07tv_tab   = dd07tv_tab
    EXCEPTIONS
      no_version   = 01.

  IF sy-subrc <> 0.
    MESSAGE e401 WITH versno objname.
  ENDIF.

  fill_sy_langu_text1 01 domname.
  fill_sy_langu_text2 07 domname domvalue_l.

  READ TABLE dd01v_tab INDEX 1.
  IF dd01v_tab-valexi = space.
    REFRESH dd07v_tab.
  ENDIF.

  LOOP AT dd01v_tab.
    CLEAR dd01vd_tab.
    MOVE-CORRESPONDING dd01v_tab TO dd01vd_tab.
    IF dd01v_tab-datatype(3) = 'D34'
    OR dd01v_tab-datatype(3) = 'D16'.
      PERFORM get_fv_text(radvvutl)
        USING
          'OUTPUTSTYLE' dd01vd_tab-outputstyle
        CHANGING dd01vd_tab-outputstyletxt rc.
    ENDIF.
    APPEND dd01vd_tab.
  ENDLOOP.

  LOOP AT dd07v_tab.
    MOVE-CORRESPONDING dd07v_tab TO dd07vd_tab.
    APPEND dd07vd_tab.
  ENDLOOP.

ENDFORM.                    "OBJ_READ
