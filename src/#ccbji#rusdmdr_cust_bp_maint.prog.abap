*&---------------------------------------------------------------------*
*& Report  /CCBJI/RUSDMDR_CUST_BP_MAINT
*&---------------------------------------------------------------------*
*& Title       : Customer Master - Dynamic Download / Upload
*& Module      : SD                       Transaction : /CCBJI/RUSDMD_CUSBP
*&
*& What it does
*&   One program for every change to, and every creation of, a
*&   customer - in place of the LSMW objects that did this one layout at a
*&   time. The user chooses the fields, the program writes a file with
*&   exactly those fields, and reads the same file back:
*&
*&   Download  Row 1 holds the field names (TABLE-FIELD, e.g. KNVV-KDGRP),
*&             row 2 onwards the data. With no numbers given the file has
*&             the headings only; with numbers, one row per record holding
*&             what SAP has today. The fields the system needs are added
*&             to the user's choice automatically.
*&   Upload    Change: each row is compared with SAP and only the fields
*&             that differ are sent. Create: a business partner is created
*&             with the customer role and the data in its rows.
*&
*&   Every value is checked before anything is posted: type, length,
*&   check table and fixed values from the ABAP Dictionary. Everything is
*&   written through the Business Partner API (CL_MD_BP_MAINTAIN) - no
*&   table is updated directly. A test run shows what would change.
*&
*& Clean core positioning (SAP S/4HANA 2025 / ABAP_CLOUD_READINESS)
*&   Classic ABAP, deliberately: CL_MD_BP_MAINTAIN is not released for
*&   ABAP Cloud and no released API maintains the customer and supplier
*&   parts of a business partner from a GUI mass upload. Reads of the
*&   master tables, the ABAP Dictionary (DDIF_FK_GET, DD07L) and the text IDs
*&   (TTXID) are confined to LCL_CAT, LCL_CHK, LCL_CFG and LCL_DB.
*&
*& Generated - do not edit. Source: tools/ccej/bp_maint/*.abap,
*& generator tools/ccej/gen_bp_maint.py.
*&---------------------------------------------------------------------*
REPORT /ccbji/rusdmdr_cust_bp_maint.

" C = customer program, V = supplier program. The two programs are one
" source; this constant is what tells them apart at runtime.
CONSTANTS gc_side TYPE c LENGTH 1 VALUE 'C'.

TABLES sscrfields.

" 255 is the widest a screen field goes; the file dialog returns a STRING
" and a shorter field would cut a long path short.
TYPES ty_path  TYPE c LENGTH 255.
" A field name in the file: TABLE-FIELD, e.g. KNVV-/CCEJ/KNVV_ZKZBLCREKBN.
TYPES ty_fname TYPE c LENGTH 60.
TYPES ty_dec   TYPE p LENGTH 15 DECIMALS 2.
TYPES ty_grp   TYPE c LENGTH 4.
TYPES ty_msgty TYPE c LENGTH 1.

TYPES: tt_cell TYPE string_table.
TYPES: BEGIN OF ty_row,
         row   TYPE i,
         cells TYPE tt_cell,
       END OF ty_row,
       tt_row TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

" One field the program can download and upload.
TYPES: BEGIN OF ty_fld,
         name     TYPE string,     " heading in the file: TABLE-FIELD
         grp      TYPE ty_grp,     " which part of the master record
         tab      TYPE tabname,    " table the current value is read from
         fld      TYPE fieldname,  " field in that table
         api      TYPE fieldname,  " component in the API structure
         key      TYPE abap_bool,  " key of its group
         must     TYPE abap_bool,  " always in the file (key / required)
         text     TYPE string,     " description
         rollname TYPE rollname,
         convexit TYPE convexit,
         inttype  TYPE inttype,
         leng     TYPE i,
         decimals TYPE i,
         lower    TYPE abap_bool,
         checktab TYPE tabname,
         domname  TYPE domname,
         valexi   TYPE abap_bool,
         tdobj    TYPE tdobject,   " texts only
         tdid     TYPE tdid,       " texts only
       END OF ty_fld,
       tt_fld TYPE STANDARD TABLE OF ty_fld WITH EMPTY KEY.

" One part of the master record: the general data, a company code, a sales
" area, the partner functions of a sales area and so on.
"   REP     the part repeats - one row in the file per entry, with ACTION
"   PARENT  the part it hangs under (a partner function under a sales area)
"   KEYS    its own key fields, blank-separated
"   API     the ABAP Dictionary structure the API takes its data in
TYPES: BEGIN OF ty_gdef,
         grp    TYPE ty_grp,
         tab    TYPE tabname,
         text   TYPE string,
         rep    TYPE abap_bool,
         parent TYPE ty_grp,
         keys   TYPE string,
         api    TYPE string,
         numfld TYPE fieldname,
       END OF ty_gdef,
       tt_gdef TYPE STANDARD TABLE OF ty_gdef WITH EMPTY KEY.

" One line of the result list.
TYPES: BEGIN OF ty_log,
         status TYPE icon_d,
         row    TYPE i,
         obj    TYPE string,
         part   TYPE string,
         field  TYPE string,
         old    TYPE string,
         new    TYPE string,
         type   TYPE ty_msgty,
         msg    TYPE string,
       END OF ty_log,
       tt_log TYPE STANDARD TABLE OF ty_log WITH EMPTY KEY.

CONSTANTS:
  " A cell holding this is left as it is in SAP.
  gc_keep   TYPE string VALUE '#',
  " Used by the reused helpers for "nothing in the cell".
  gc_clear  TYPE string VALUE '#BLANK#',
  " Column that says what to do with a repeating entry: I, U or D.
  gc_action TYPE string VALUE 'ACTION',
  " The API's tasks.
  gc_ins    TYPE c LENGTH 1 VALUE 'I',
  gc_upd    TYPE c LENGTH 1 VALUE 'U',
  gc_del    TYPE c LENGTH 1 VALUE 'D',
  " A mobile number is a telephone entry with R_3_USER 3. It is not a flag:
  " anything but space, 1, 2 or 3 ends the transaction with a type X message.
  gc_mobile TYPE c LENGTH 1 VALUE '3',
  " Business partner category: organisation.
  gc_org    TYPE bu_type VALUE '2'.

" Data objects the SELECT-OPTIONS are built over.
DATA: gv_num   TYPE kunnr,
      gv_bp    TYPE bu_partner,
      gv_bukrs TYPE bukrs,
      gv_org   TYPE vkorg,
      gv_fname TYPE ty_fname.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_crt RADIOBUTTON GROUP g0 USER-COMMAND md,
            p_chg RADIOBUTTON GROUP g0 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_down RADIOBUTTON GROUP g1 USER-COMMAND ac DEFAULT 'X',
            p_up   RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.

" The account group decides the number range and which fields a new record
" needs. Only creation asks for it.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_ktogr TYPE ktokd,
            p_bpgrp TYPE bu_group.
SELECTION-SCREEN END OF BLOCK b2.

" The fields the user picked. S_FLD is not on the screen but it is part of
" it, so a variant saved with Goto > Variants keeps the choice and brings it
" back.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-004.
SELECTION-SCREEN PUSHBUTTON /1(24) bt_fld USER-COMMAND fld.
SELECTION-SCREEN COMMENT 27(50) cm_fld.
SELECT-OPTIONS s_fld FOR gv_fname NO-DISPLAY.
PARAMETERS p_tlang TYPE spras DEFAULT sy-langu.
SELECTION-SCREEN END OF BLOCK b3.

" Which records a download fills the file with. Left empty, the file has
" the headings only.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-005.
SELECT-OPTIONS: s_num   FOR gv_num,
                s_bp    FOR gv_bp,
                s_bukrs FOR gv_bukrs,
                s_org   FOR gv_org.
PARAMETERS:     p_max   TYPE i DEFAULT 1000.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-006.
PARAMETERS p_test AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b5.

" Not OBLIGATORY: a required field left empty stops every radio button click
" with "Fill in all required entry fields". It is checked on execution.
SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-007.
PARAMETERS: p_file TYPE ty_path LOWER CASE,
            p_xlsx RADIOBUTTON GROUP g2 USER-COMMAND ft DEFAULT 'X',
            p_txt  RADIOBUTTON GROUP g2.
SELECTION-SCREEN END OF BLOCK b6.

*----------------------------------------------------------------------*
* Exception
*----------------------------------------------------------------------*
CLASS lcx_dl DEFINITION INHERITING FROM cx_static_check FINAL.
  PUBLIC SECTION.
    DATA text TYPE string.
    METHODS constructor IMPORTING iv_text TYPE string.
    METHODS get_text REDEFINITION.
ENDCLASS.

CLASS lcx_dl IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    text = iv_text.
  ENDMETHOD.
  METHOD get_text.
    result = text.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* Reused unchanged from the Cipla programs ZSDS_CUST_TMPL_DOWNLOAD and
* ZMMS_BP_MASS_UPLOAD, where they were tested in the system:
*   LCL_UTIL   value formatting and parsing helpers
*   LCL_XLSX   writes an .xlsx (OpenXML parts zipped with CL_ABAP_ZIP)
*   LCL_EXCEL  reads an .xlsx through CL_FDT_XL_SPREADSHEET
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* LCL_UTIL - writing a stored value the way the upload reads it back
*----------------------------------------------------------------------*
CLASS lcl_util DEFINITION FINAL.
  PUBLIC SECTION.
    " IV_FMT is the conversion the upload program applies on the way in:
    "   DT date   NM whole number   AL / GL leading zeros   TT title key
    CLASS-METHODS text
      IMPORTING iv_value  TYPE any
                iv_fmt    TYPE clike DEFAULT ''
      RETURNING VALUE(rv) TYPE string.

    CLASS-METHODS xml_escape
      IMPORTING iv_in     TYPE string
      RETURNING VALUE(rv) TYPE string.

    " 1 -> A, 27 -> AA, as the spreadsheet format wants it.
    CLASS-METHODS col_letter
      IMPORTING iv_col    TYPE i
      RETURNING VALUE(rv) TYPE string.

    " Reads cell IV_COL of IS_ROW. Returns an empty string when the column
    " is beyond the end of the row, which is normal for short rows.
    CLASS-METHODS cell
      IMPORTING is_row    TYPE ty_row
                iv_col    TYPE i
      RETURNING VALUE(rv) TYPE string.

    " Accepts DD.MM.YYYY, DD/MM/YYYY, DD-MM-YYYY and YYYYMMDD. Returns
    " initial for anything it cannot parse - the caller decides whether an
    " unparsable date is an error.
    CLASS-METHODS to_date
      IMPORTING iv_in     TYPE string
      RETURNING VALUE(rv) TYPE d.

    CLASS-METHODS to_dec
      IMPORTING iv_in     TYPE string
      RETURNING VALUE(rv) TYPE ty_dec.

    CLASS-METHODS to_int
      IMPORTING iv_in     TYPE string
      RETURNING VALUE(rv) TYPE i.


    CLASS-METHODS alpha
      IMPORTING iv_in     TYPE string
                iv_len    TYPE i DEFAULT 0
      RETURNING VALUE(rv) TYPE string.


    " Upper-cases and strips everything except letters and digits, so tab
    " names and column headings can be compared without being defeated by
    " spacing, punctuation or capitalisation.
    CLASS-METHODS squash
      IMPORTING iv_in     TYPE clike
      RETURNING VALUE(rv) TYPE string.

    " A one character flag written as a word. Excel turns a tick into TRUE
    " and some files carry YES or 1, all of which would land in a CHAR 1
    " field as its first letter - T, Y, 1 - none of which SAP reads as set.
    CLASS-METHODS flag
      IMPORTING iv_in     TYPE clike
      RETURNING VALUE(rv) TYPE string.

    CLASS-METHODS is_empty
      IMPORTING is_row    TYPE ty_row
      RETURNING VALUE(rv) TYPE abap_bool.

    " Copies the components that were actually filled - those whose flag is
    " set in IS_FROMX - into the like-named components of CS_TO, raising the
    " same flags there. Used to give the business partner the address the
    " customer was given, without writing the mapping out twice.
    CLASS-METHODS copy_like
      IMPORTING is_from  TYPE any
                is_fromx TYPE any
      CHANGING  cs_to    TYPE any
                cs_tox   TYPE any.
ENDCLASS.

CLASS lcl_util IMPLEMENTATION.

  METHOD text.
    FIELD-SYMBOLS <lv> TYPE any.
    ASSIGN iv_value TO <lv>.
    IF <lv> IS NOT ASSIGNED OR <lv> IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_kind) = cl_abap_typedescr=>describe_by_data( <lv> )->type_kind.

    " A column name can land on a table or a structure inside the master
    " data - there is no text for those, and assigning one to a string
    " would terminate the program.
    IF lv_kind = cl_abap_typedescr=>typekind_table
    OR lv_kind = cl_abap_typedescr=>typekind_struct1
    OR lv_kind = cl_abap_typedescr=>typekind_struct2
    OR lv_kind = cl_abap_typedescr=>typekind_oref
    OR lv_kind = cl_abap_typedescr=>typekind_dref.
      RETURN.
    ENDIF.

    IF lv_kind = cl_abap_typedescr=>typekind_date.
      DATA lv_d TYPE d.
      lv_d = <lv>.
      IF lv_d IS INITIAL.
        RETURN.
      ENDIF.
      rv = |{ lv_d+6(2) }.{ lv_d+4(2) }.{ lv_d(4) }|.
      RETURN.
    ENDIF.

    IF lv_kind = cl_abap_typedescr=>typekind_packed
    OR lv_kind = cl_abap_typedescr=>typekind_float
    OR lv_kind = cl_abap_typedescr=>typekind_int
    OR lv_kind = cl_abap_typedescr=>typekind_int1
    OR lv_kind = cl_abap_typedescr=>typekind_int2.
      DATA lv_p TYPE p LENGTH 16 DECIMALS 4.
      lv_p = <lv>.
      rv = |{ lv_p NUMBER = RAW }|.
      rv = condense( rv ).
      " trailing zeros after the point say nothing on a template
      IF rv CS '.'.
        WHILE substring( val = rv off = strlen( rv ) - 1 len = 1 ) = '0'.
          rv = substring( val = rv len = strlen( rv ) - 1 ).
        ENDWHILE.
        IF substring( val = rv off = strlen( rv ) - 1 len = 1 ) = '.'.
          rv = substring( val = rv len = strlen( rv ) - 1 ).
        ENDIF.
      ENDIF.
      RETURN.
    ENDIF.

    " Everything else is character-like: a plain assignment converts it.
    DATA lv_c TYPE string.
    lv_c = <lv>.
    rv   = condense( lv_c ).

    " Leading zeros come off: the upload program puts them back, and a
    " file full of 0000147341 is harder to read and to edit.
    IF ( iv_fmt = 'AL' OR iv_fmt = 'GL' ) AND rv CO '0123456789'.
      SHIFT rv LEFT DELETING LEADING '0'.
    ENDIF.
  ENDMETHOD.

  METHOD xml_escape.
    rv = iv_in.
    REPLACE ALL OCCURRENCES OF '&'  IN rv WITH '&amp;'.
    REPLACE ALL OCCURRENCES OF '<'  IN rv WITH '&lt;'.
    REPLACE ALL OCCURRENCES OF '>'  IN rv WITH '&gt;'.
    REPLACE ALL OCCURRENCES OF '"'  IN rv WITH '&quot;'.
    REPLACE ALL OCCURRENCES OF `'`  IN rv WITH '&apos;'.
    " Tabs and line breaks inside a cell would break the sheet.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf   IN rv WITH ` `.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN rv WITH ` `.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN rv WITH ` `.
  ENDMETHOD.

  METHOD col_letter.
    DATA lv_n TYPE i.
    DATA lv_r TYPE i.
    lv_n = iv_col.
    WHILE lv_n > 0.
      lv_r = ( lv_n - 1 ) MOD 26.
      rv   = |{ sy-abcde+lv_r(1) }{ rv }|.
      lv_n = ( lv_n - 1 ) DIV 26.
    ENDWHILE.
  ENDMETHOD.


  METHOD cell.
    IF iv_col > 0 AND iv_col <= lines( is_row-cells ).
      rv = condense( is_row-cells[ iv_col ] ).
    ENDIF.
  ENDMETHOD.

  METHOD to_date.
    DATA(lv) = condense( iv_in ).
    IF lv IS INITIAL.
      RETURN.
    ENDIF.

    " Excel hands a date over in whatever shape the cell had: with a time
    " behind it, with slashes or dashes, the year in front, or - when the
    " cell was a real date and the format was lost - as its serial number.
    IF lv CS ` `.
      " Excel hands a date over with the time behind it. Only the date is
      " wanted; SPLIT needed somewhere to put the rest, and that somewhere
      " was a variable nothing ever read.
      lv = substring_before( val = lv sub = ` ` ).
    ENDIF.
    REPLACE ALL OCCURRENCES OF '/' IN lv WITH '.'.
    REPLACE ALL OCCURRENCES OF '-' IN lv WITH '.'.

    IF lv CS '.'.
      SPLIT lv AT '.' INTO DATA(lv_1) DATA(lv_2) DATA(lv_3).
      IF lv_1 IS INITIAL OR lv_2 IS INITIAL OR lv_3 IS INITIAL
         OR lv_1 CN '0123456789' OR lv_2 CN '0123456789' OR lv_3 CN '0123456789'.
        RETURN.
      ENDIF.

      DATA: lv_d TYPE string,
            lv_m TYPE string,
            lv_y TYPE string.
      IF strlen( lv_1 ) = 4.
        lv_y = lv_1. lv_m = lv_2. lv_d = lv_3.        " 2026-12-31
      ELSEIF CONV i( lv_1 ) > 12 OR CONV i( lv_2 ) <= 12.
        lv_d = lv_1. lv_m = lv_2. lv_y = lv_3.        " 31.12.2026
      ELSE.
        lv_m = lv_1. lv_d = lv_2. lv_y = lv_3.        " 12/31/2026
      ENDIF.
      IF strlen( lv_y ) = 2.
        " Two-digit years in these templates are always this century.
        lv_y = |20{ lv_y }|.
      ENDIF.
      IF strlen( lv_y ) <> 4.
        RETURN.
      ENDIF.
      rv = |{ lv_y }{ lv_m ALPHA = IN WIDTH = 2 }{ lv_d ALPHA = IN WIDTH = 2 }|.

    ELSEIF strlen( lv ) = 8 AND lv CO '0123456789'.
      rv = lv.

    ELSEIF strlen( lv ) BETWEEN 4 AND 6 AND lv CO '0123456789'.
      " A spreadsheet serial: day 1 is 01.01.1900, and the sheet counts a
      " 29.02.1900 that never existed, which is why the epoch is the 30th
      " of December 1899. Only a sensible range is taken.
      DATA(lv_ser) = CONV i( lv ).
      IF lv_ser >= 20000 AND lv_ser <= 80000.
        DATA lv_base TYPE d VALUE '18991230'.
        DATA lv_dat  TYPE d.
        lv_dat = lv_base + lv_ser.
        rv = lv_dat.
      ENDIF.
    ENDIF.

    " Guard against 20260231 and friends: a real date survives a round trip
    " through a date field, an invalid one does not.
    IF rv IS NOT INITIAL.
      DATA lv_chk TYPE d.
      DATA lv_days TYPE i.
      lv_chk = rv.
      lv_days = lv_chk - 1.
      lv_chk = lv_days + 1.
      IF lv_chk <> rv.
        CLEAR rv.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD to_dec.
    DATA(lv) = condense( iv_in ).
    IF lv IS INITIAL.
      RETURN.
    ENDIF.
    CONDENSE lv NO-GAPS.

    " A trailing minus is how SAP writes a negative number, and Excel hands
    " it over that way too.
    DATA(lv_neg) = abap_false.
    IF substring( val = lv off = strlen( lv ) - 1 len = 1 ) = '-'.
      lv_neg = abap_true.
      lv = substring( val = lv len = strlen( lv ) - 1 ).
    ENDIF.

    " Which separator is the decimal one: the LAST of the two. 500,000.00
    " and 500.000,00 are the same number written in two conventions, and
    " taking the comma out of both would turn the second into 500.00.
    DATA(lv_dot) = 0.
    DATA(lv_com) = 0.
    FIND ALL OCCURRENCES OF '.' IN lv MATCH COUNT DATA(lv_ndot).
    FIND ALL OCCURRENCES OF ',' IN lv MATCH COUNT DATA(lv_ncom).
    IF lv_ndot > 0.
      FIND ALL OCCURRENCES OF '.' IN lv RESULTS DATA(lt_dot).
      lv_dot = lt_dot[ lines( lt_dot ) ]-offset + 1.
    ENDIF.
    IF lv_ncom > 0.
      FIND ALL OCCURRENCES OF ',' IN lv RESULTS DATA(lt_com).
      lv_com = lt_com[ lines( lt_com ) ]-offset + 1.
    ENDIF.

    IF lv_dot > 0 AND lv_com > 0.
      IF lv_com > lv_dot.
        REPLACE ALL OCCURRENCES OF '.' IN lv WITH ``.
        REPLACE ALL OCCURRENCES OF ',' IN lv WITH '.'.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv WITH ``.
      ENDIF.
    ELSEIF lv_ncom = 1.
      " One comma and only two digits behind it is a decimal comma;
      " anything else is a thousands separator.
      IF strlen( lv ) - lv_com = 2.
        REPLACE ALL OCCURRENCES OF ',' IN lv WITH '.'.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv WITH ``.
      ENDIF.
    ELSEIF lv_ncom > 1.
      REPLACE ALL OCCURRENCES OF ',' IN lv WITH ``.
    ENDIF.

    TRY.
        rv = lv.
      CATCH cx_sy_conversion_error.
        " The superclass, so an overflow is caught as well as a value that
        " is not a number at all.
        CLEAR rv.
        RETURN.
    ENDTRY.
    IF lv_neg = abap_true.
      rv = rv * -1.
    ENDIF.
  ENDMETHOD.

  METHOD to_int.
    DATA(lv_p) = to_dec( iv_in ).
    rv = round( val = lv_p dec = 0 ).
  ENDMETHOD.

  METHOD alpha.
    " Leading-zero conversion, done here rather than through
    " CONVERSION_EXIT_ALPHA_INPUT.
    "
    " That function module pads to the length of its OUTPUT parameter. This
    " method used to hand it a STRING, which has no fixed length, so it had
    " nothing to pad to and the zeros were never added: a reconciliation
    " account keyed as 1120001 stayed 1120001 instead of becoming
    " 0001120001, and every SKB1 lookup missed.
    "
    " Padding to IV_LEN here removes the dependency on that behaviour.
    " Only purely numeric values are padded, which is what ALPHA does.
    CLEAR rv.
    rv = condense( iv_in ).
    IF rv IS INITIAL OR rv = gc_clear.
      RETURN.
    ENDIF.
    IF iv_len > 0 AND rv CO '0123456789' AND strlen( rv ) < iv_len.
      rv = repeat( val = '0' occ = iv_len - strlen( rv ) ) && rv.
    ENDIF.
  ENDMETHOD.

  METHOD squash.
    rv = to_upper( CONV string( iv_in ) ).
    REPLACE ALL OCCURRENCES OF PCRE '[^A-Z0-9]' IN rv WITH ''.
    " The key is kept in a 40 character field, so a longer heading has to be
    " cut to the same length here - otherwise the file's key is 41 characters
    " long, the map's is 40, and a column with a long heading could never
    " match. "Key for sorting according to assignment numbers" is one.
    IF strlen( rv ) > 40.
      rv = rv(40).
    ENDIF.
  ENDMETHOD.

  METHOD copy_like.
    FIELD-SYMBOLS: <lv_fx> TYPE any, <lv_f>  TYPE any,
                   <lv_tx> TYPE any, <lv_t>  TYPE any.
    DATA lo_str TYPE REF TO cl_abap_structdescr.
    lo_str ?= cl_abap_typedescr=>describe_by_data( is_fromx ).
    LOOP AT lo_str->components INTO DATA(ls_cmp).
      ASSIGN COMPONENT ls_cmp-name OF STRUCTURE is_fromx TO <lv_fx>.
      IF sy-subrc <> 0 OR <lv_fx> IS INITIAL.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT ls_cmp-name OF STRUCTURE cs_tox TO <lv_tx>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT ls_cmp-name OF STRUCTURE is_from TO <lv_f>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT ls_cmp-name OF STRUCTURE cs_to TO <lv_t>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      <lv_t>  = <lv_f>.
      <lv_tx> = <lv_fx>.
    ENDLOOP.
  ENDMETHOD.

  METHOD flag.
    DATA(lv) = to_upper( condense( CONV string( iv_in ) ) ).
    rv = lv.
    IF strlen( lv ) <= 1.
      RETURN.
    ENDIF.
    CASE lv.
      WHEN 'TRUE' OR 'YES' OR 'JA' OR 'Y' OR 'J' OR '1' OR 'SET' OR 'CHECKED'.
        rv = 'X'.
      WHEN 'FALSE' OR 'NO' OR 'NEIN' OR 'N' OR '0' OR 'UNCHECKED'.
        CLEAR rv.
    ENDCASE.
  ENDMETHOD.

  METHOD is_empty.
    " IS INITIAL takes a data object, not an expression, so the result of
    " CONDENSE is put in a variable first.
    DATA lv_c TYPE string.
    rv = abap_true.
    LOOP AT is_row-cells INTO DATA(lv).
      lv_c = condense( lv ).
      IF lv_c IS NOT INITIAL.
        rv = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_XLSX - writes the workbook
*   An .xlsx is a zip of OpenXML parts. CL_FDT_XL_SPREADSHEET, which the
*   upload program reads it back with, is not a general reader: it takes
*   the text of a cell from the shared string table and nowhere else, and
*   it expects the parts Excel itself writes. So every part below is
*   written and every text cell points into xl/sharedStrings.xml.
*----------------------------------------------------------------------*

CLASS lcl_xlsx DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS build
      IMPORTING iv_sheet  TYPE clike
                it_head   TYPE tt_cell
                it_row    TYPE tt_row
      RETURNING VALUE(rv) TYPE xstring
      RAISING   lcx_dl.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_si,
             text TYPE string,
             idx  TYPE i,
           END OF ty_si.
    CLASS-DATA mt_si  TYPE HASHED TABLE OF ty_si WITH UNIQUE KEY text.
    CLASS-DATA mt_txt TYPE string_table.
    CLASS-DATA mv_use TYPE i.

    CLASS-METHODS si
      IMPORTING iv_text   TYPE string
      RETURNING VALUE(rv) TYPE i.
    CLASS-METHODS row_xml
      IMPORTING it_cells  TYPE tt_cell
                iv_row    TYPE i
      RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS to_x
      IMPORTING iv_in     TYPE string
      RETURNING VALUE(rv) TYPE xstring
      RAISING   lcx_dl.
ENDCLASS.

CLASS lcl_xlsx IMPLEMENTATION.

  METHOD to_x.
    TRY.
        rv = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( iv_in ).
      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION NEW lcx_dl( |The workbook could not be encoded: { lx->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

  METHOD si.
    " The index of a text in the shared string table, adding it if it is
    " not there yet. MV_USE counts the cells that point at one, which is
    " what the sst element's "count" attribute means.
    mv_use = mv_use + 1.
    READ TABLE mt_si WITH TABLE KEY text = iv_text INTO DATA(ls_si).
    IF sy-subrc = 0.
      rv = ls_si-idx.
      RETURN.
    ENDIF.
    APPEND iv_text TO mt_txt.
    rv = lines( mt_txt ) - 1.
    INSERT VALUE ty_si( text = iv_text idx = rv ) INTO TABLE mt_si.
  ENDMETHOD.

  METHOD row_xml.
    rv = |<row r="{ iv_row }">|.
    LOOP AT it_cells INTO DATA(lv_cell).
      " Taken before anything else runs: READ TABLE inside SI( ) sets
      " SY-TABIX, and the column number is wanted, not that.
      DATA(lv_col) = sy-tabix.
      " Column A is always written, empty or not. CL_FDT_XL_SPREADSHEET
      " builds its table from the cells it finds, so a row that starts at
      " B comes back one column short and every value sits one place to
      " the left of where the template says it is.
      IF lv_cell IS INITIAL AND lv_col > 1.
        CONTINUE.                              " an empty cell is left out
      ENDIF.
      DATA(lv_ix) = si( lv_cell ).
      rv = rv && |<c r="{ lcl_util=>col_letter( lv_col ) }{ iv_row }" t="s">| &&
                 |<v>{ lv_ix }</v></c>|.
    ENDLOOP.
    rv = rv && |</row>|.
  ENDMETHOD.

  METHOD build.
    CLEAR: mt_si, mt_txt, mv_use.

    " Excel limits a sheet name to 31 characters and forbids : \ / ? * [ ]
    DATA(lv_name) = condense( CONV string( iv_sheet ) ).
    REPLACE ALL OCCURRENCES OF PCRE '[:\\\\/?*\[\]]' IN lv_name WITH ` `.
    IF strlen( lv_name ) > 31.
      lv_name = lv_name(31).
    ENDIF.
    IF lv_name IS INITIAL.
      lv_name = 'Sheet1'.
    ENDIF.

    " ---- the sheet, and with it the shared string table ----------------
    DATA(lv_body) = row_xml( it_cells = it_head iv_row = 1 ).
    DATA(lv_wide) = lines( it_head ).
    LOOP AT it_row INTO DATA(ls_row).
      lv_body = lv_body && row_xml( it_cells = ls_row-cells iv_row = sy-tabix + 1 ).
      IF lines( ls_row-cells ) > lv_wide.
        lv_wide = lines( ls_row-cells ).
      ENDIF.
    ENDLOOP.
    IF lv_wide < 1.
      lv_wide = 1.
    ENDIF.
    DATA(lv_dim) = |A1:{ lcl_util=>col_letter( lv_wide ) }{ lines( it_row ) + 1 }|.

    DATA(lv_sheet) =
      |<?xml version="1.0" encoding="UTF-8" standalone="yes"?>| &&
      |<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" | &&
      |xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">| &&
      |<dimension ref="{ lv_dim }"/>| &&
      |<sheetViews><sheetView tabSelected="1" workbookViewId="0"/></sheetViews>| &&
      |<sheetFormatPr defaultRowHeight="15"/>| &&
      |<sheetData>| && lv_body && |</sheetData>| &&
      |</worksheet>|.

    " ---- shared strings ------------------------------------------------
    DATA(lv_sst) =
      |<?xml version="1.0" encoding="UTF-8" standalone="yes"?>| &&
      |<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" | &&
      |count="{ mv_use }" uniqueCount="{ lines( mt_txt ) }">|.
    LOOP AT mt_txt INTO DATA(lv_t).
      lv_sst = lv_sst && |<si><t xml:space="preserve">{ lcl_util=>xml_escape( lv_t ) }</t></si>|.
    ENDLOOP.
    lv_sst = lv_sst && |</sst>|.

    " ---- styles: one font, one format, which is all that is referenced -
    DATA(lv_sty) =
      |<?xml version="1.0" encoding="UTF-8" standalone="yes"?>| &&
      |<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">| &&
      |<fonts count="1"><font><sz val="11"/><name val="Calibri"/><family val="2"/></font></fonts>| &&
      |<fills count="2"><fill><patternFill patternType="none"/></fill>| &&
      |<fill><patternFill patternType="gray125"/></fill></fills>| &&
      |<borders count="1"><border><left/><right/><top/><bottom/><diagonal/></border></borders>| &&
      |<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>| &&
      |<cellXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/></cellXfs>| &&
      |<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>| &&
      |</styleSheet>|.

    DATA(lv_types) =
      |<?xml version="1.0" encoding="UTF-8" standalone="yes"?>| &&
      |<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">| &&
      |<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>| &&
      |<Default Extension="xml" ContentType="application/xml"/>| &&
      |<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>| &&
      |<Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>| &&
      |<Override PartName="/xl/sharedStrings.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/>| &&
      |<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>| &&
      |<Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>| &&
      |<Override PartName="/docProps/app.xml" ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>| &&
      |</Types>|.

    DATA(lv_rels) =
      |<?xml version="1.0" encoding="UTF-8" standalone="yes"?>| &&
      |<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">| &&
      |<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>| &&
      |<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>| &&
      |<Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties" Target="docProps/app.xml"/>| &&
      |</Relationships>|.

    DATA(lv_wb) =
      |<?xml version="1.0" encoding="UTF-8" standalone="yes"?>| &&
      |<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" | &&
      |xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">| &&
      |<bookViews><workbookView/></bookViews>| &&
      |<sheets><sheet name="{ lcl_util=>xml_escape( lv_name ) }" sheetId="1" r:id="rId1"/></sheets>| &&
      |</workbook>|.

    DATA(lv_wbrels) =
      |<?xml version="1.0" encoding="UTF-8" standalone="yes"?>| &&
      |<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">| &&
      |<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>| &&
      |<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>| &&
      |<Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings" Target="sharedStrings.xml"/>| &&
      |</Relationships>|.

    DATA(lv_core) =
      |<?xml version="1.0" encoding="UTF-8" standalone="yes"?>| &&
      |<cp:coreProperties | &&
      |xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" | &&
      |xmlns:dc="http://purl.org/dc/elements/1.1/" | &&
      |xmlns:dcterms="http://purl.org/dc/terms/" | &&
      |xmlns:dcmitype="http://purl.org/dc/dcmitype/" | &&
      |xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">| &&
      |<dc:creator>ZSDS_CUST_TMPL_DOWNLOAD</dc:creator>| &&
      |<cp:lastModifiedBy>ZSDS_CUST_TMPL_DOWNLOAD</cp:lastModifiedBy>| &&
      |</cp:coreProperties>|.

    DATA(lv_app) =
      |<?xml version="1.0" encoding="UTF-8" standalone="yes"?>| &&
      |<Properties | &&
      |xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties" | &&
      |xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">| &&
      |<Application>SAP</Application>| &&
      |</Properties>|.

    DATA(lo_zip) = NEW cl_abap_zip( ).
    lo_zip->add( name = '[Content_Types].xml'        content = to_x( lv_types ) ).
    lo_zip->add( name = '_rels/.rels'                content = to_x( lv_rels ) ).
    lo_zip->add( name = 'docProps/core.xml'          content = to_x( lv_core ) ).
    lo_zip->add( name = 'docProps/app.xml'           content = to_x( lv_app ) ).
    lo_zip->add( name = 'xl/workbook.xml'            content = to_x( lv_wb ) ).
    lo_zip->add( name = 'xl/_rels/workbook.xml.rels' content = to_x( lv_wbrels ) ).
    lo_zip->add( name = 'xl/styles.xml'              content = to_x( lv_sty ) ).
    lo_zip->add( name = 'xl/sharedStrings.xml'       content = to_x( lv_sst ) ).
    lo_zip->add( name = 'xl/worksheets/sheet1.xml'   content = to_x( lv_sheet ) ).
    rv = lo_zip->save( ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_excel DEFINITION FINAL.
  PUBLIC SECTION.
    " Returns the data rows of the tab that carries the columns of this
    " scenario. IT_WANT holds the headings the scenario expects; the tab is
    " picked by how many of them it has, so the tab NAME does not matter -
    " a renamed tab, a single-sheet copy or the master workbook all load the
    " same way. IV_SHEET is only the tie-breaker and the fallback.
    METHODS read
      IMPORTING iv_file    TYPE string
                iv_from_pc TYPE abap_bool
                iv_sheet   TYPE string
                iv_skip    TYPE i DEFAULT 1
                it_want    TYPE string_table OPTIONAL
      EXPORTING et_head    TYPE string_table
                et_row     TYPE tt_row
                ev_sheet   TYPE string
      RAISING   lcx_dl.
  PRIVATE SECTION.
    METHODS load_bin
      IMPORTING iv_file    TYPE string
                iv_from_pc TYPE abap_bool
      RETURNING VALUE(rv)  TYPE xstring
      RAISING   lcx_dl.

    " One worksheet as a table of rows, heading rows included.
    METHODS sheet_rows
      IMPORTING io_xl     TYPE REF TO cl_fdt_xl_spreadsheet
                iv_name   TYPE string
      RETURNING VALUE(rt) TYPE tt_row
      RAISING   lcx_dl.

    " How many of the expected headings this heading row carries.
    METHODS score
      IMPORTING it_head   TYPE string_table
                it_want   TYPE string_table
      RETURNING VALUE(rv) TYPE i.
ENDCLASS.


CLASS lcl_excel IMPLEMENTATION.

  METHOD load_bin.
    DATA lt_bin  TYPE solix_tab.
    DATA lv_len  TYPE i.

    IF iv_from_pc = abap_true.
      cl_gui_frontend_services=>gui_upload(
        EXPORTING filename   = iv_file
                  filetype   = 'BIN'
        IMPORTING filelength = lv_len
        CHANGING  data_tab   = lt_bin
        EXCEPTIONS OTHERS    = 1 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_dl
          EXPORTING iv_text = |Cannot read { iv_file } from the PC|.
      ENDIF.
      " SCMS_BINARY_TO_XSTRING rather than a utility class, so no method
      " signature outside this program has to be right for it to compile.
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING input_length = lv_len
        IMPORTING buffer       = rv
        TABLES    binary_tab   = lt_bin
        EXCEPTIONS failed      = 1
                   OTHERS      = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_dl
          EXPORTING iv_text = |{ iv_file } could not be converted|.
      ENDIF.
    ELSE.
      DATA lv_msg TYPE string.
      DATA lv_x   TYPE xstring.
      " MESSAGE addition is mandatory - without it a failed open dumps.
      OPEN DATASET iv_file FOR INPUT IN BINARY MODE MESSAGE lv_msg.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_dl
          EXPORTING iv_text = |Cannot open { iv_file } on the server: { lv_msg }|.
      ENDIF.
      READ DATASET iv_file INTO lv_x.
      CLOSE DATASET iv_file.
      rv = lv_x.
    ENDIF.

    IF rv IS INITIAL.
      RAISE EXCEPTION TYPE lcx_dl EXPORTING iv_text = |{ iv_file } is empty|.
    ENDIF.
  ENDMETHOD.

  METHOD sheet_rows.
    DATA(lo_data) = io_xl->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                      worksheet_name = iv_name ).
    FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.
    ASSIGN lo_data->* TO <lt_tab>.
    IF <lt_tab> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE lcx_dl
        EXPORTING iv_text = |Tab "{ iv_name }" could not be converted|.
    ENDIF.

    DATA lv_idx TYPE i.
    LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_line>).
      lv_idx = sy-tabix.
      DATA ls_row TYPE ty_row.
      CLEAR ls_row.
      ls_row-row = lv_idx.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_c>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        DATA lv_cellv TYPE string.
        lv_cellv = <lv_c>.
        APPEND lv_cellv TO ls_row-cells.
      ENDDO.
      APPEND ls_row TO rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD score.
    DATA lt_k TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line.
    LOOP AT it_head INTO DATA(lv_h).
      DATA(lv_k) = lcl_util=>squash( lv_h ).
      IF lv_k IS NOT INITIAL.
        INSERT lv_k INTO TABLE lt_k.
      ENDIF.
    ENDLOOP.
    LOOP AT it_want INTO DATA(lv_w).
      IF line_exists( lt_k[ table_line = lv_w ] ).
        rv = rv + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD read.
    CLEAR: et_head, et_row, ev_sheet.
    DATA(lv_bin) = load_bin( iv_file = iv_file iv_from_pc = iv_from_pc ).

    DATA lo_xl TYPE REF TO cl_fdt_xl_spreadsheet.
    TRY.
        lo_xl = NEW cl_fdt_xl_spreadsheet(
                      document_name = iv_file
                      xdocument     = lv_bin ).
      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION TYPE lcx_dl
          EXPORTING iv_text = |Workbook cannot be parsed: { lx->get_text( ) }|.
    ENDTRY.

    DATA lt_names TYPE if_fdt_doc_spreadsheet=>t_worksheet_names.
    lo_xl->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = lt_names ).
    IF lt_names IS INITIAL.
      RAISE EXCEPTION TYPE lcx_dl
        EXPORTING iv_text = |{ iv_file } contains no worksheet|.
    ENDIF.

    " The tab whose name matches, if there is one. Names are compared on
    " letters and digits only, so trailing blanks, capitalisation and
    " spaces against underscores make no difference.
    DATA lv_named TYPE string.
    DATA(lv_wnm) = lcl_util=>squash( iv_sheet ).
    LOOP AT lt_names INTO DATA(lv_nm).
      IF lcl_util=>squash( lv_nm ) = lv_wnm.
        lv_named = lv_nm.
        EXIT.
      ENDIF.
    ENDLOOP.

    " What actually decides is the heading row: over every tab, and over the
    " first few lines of each, the line carrying the most of the columns this
    " scenario expects wins. So the tab name does not matter, and neither
    " does a title line above the headings. Where two tabs are equally good
    " the one named for the scenario is taken.
    CONSTANTS lc_scan TYPE i VALUE 10.
    DATA lv_hit  TYPE string.
    DATA lt_hit  TYPE tt_row.
    DATA lv_best TYPE i.
    DATA lv_hrow TYPE i.
    IF it_want IS NOT INITIAL.
      LOOP AT lt_names INTO DATA(lv_n2).
        DATA(lt_r) = sheet_rows( io_xl = lo_xl iv_name = lv_n2 ).
        DATA(lv_max) = COND i( WHEN lines( lt_r ) < lc_scan THEN lines( lt_r )
                               ELSE lc_scan ).
        DO lv_max TIMES.
          DATA(lv_r)  = sy-index.
          DATA(lv_sc) = score( it_head = lt_r[ lv_r ]-cells it_want = it_want ).
          IF lv_sc > lv_best
          OR ( lv_sc > 0 AND lv_sc = lv_best AND lv_n2 = lv_named AND lv_hit <> lv_named ).
            lv_best = lv_sc.
            lv_hit  = lv_n2.
            lt_hit  = lt_r.
            lv_hrow = lv_r.
          ENDIF.
        ENDDO.
      ENDLOOP.
    ENDIF.

    " No tab recognisable by its headings - fall back to the name, then to
    " the only tab there is, and to IV_SKIP for the heading row.
    IF lv_best = 0.
      CLEAR lt_hit.
      lv_hit  = lv_named.
      lv_hrow = iv_skip.
      " A workbook this program did not write may name its tab anything:
      " the first tab is the one read.
      IF lv_hit IS INITIAL.
        lv_hit = lt_names[ 1 ].
      ENDIF.
    ENDIF.

    IF lv_hit IS INITIAL.
      DATA lv_have TYPE string.
      LOOP AT lt_names INTO DATA(lv_n3).
        lv_have = COND string( WHEN lv_have IS INITIAL THEN lv_n3
                               ELSE |{ lv_have }, { lv_n3 }| ).
      ENDLOOP.
      RAISE EXCEPTION TYPE lcx_dl
        EXPORTING iv_text = |No tab in this workbook carries the columns of "{ iv_sheet }". | &&
                            |Tabs found: { lv_have }|.
    ENDIF.

    IF lt_hit IS INITIAL.
      lt_hit = sheet_rows( io_xl = lo_xl iv_name = lv_hit ).
    ENDIF.
    ev_sheet = lv_hit.

    " Everything down to and including the heading line is dropped; the
    " heading line itself is handed back, because it is what the columns are
    " matched on. Whether CL_FDT_XL_SPREADSHEET returns the heading row as
    " its first line or consumes it as the column names is release-dependent,
    " which is exactly why the heading is located rather than assumed.
    LOOP AT lt_hit INTO DATA(ls_l).
      IF ls_l-row < lv_hrow.
        CONTINUE.
      ELSEIF ls_l-row = lv_hrow.
        et_head = ls_l-cells.
        CONTINUE.
      ENDIF.
      IF ls_l-row = lv_hrow + 1.
        " Some tabs spread the headings over two lines - technical names on
        " one line and, for the columns that have no technical name, the
        " description on the next. A blank
        " heading is therefore filled from the neighbouring line, but only
        " from a line that is itself part of the heading block: a line that
        " carries none of this scenario's headings is data and is left
        " alone.
        IF score( it_head = ls_l-cells it_want = it_want ) > 0.
          DATA lv_hc TYPE i.
          LOOP AT ls_l-cells INTO DATA(lv_fill).
            lv_hc = sy-tabix.
            IF lv_fill IS INITIAL.
              CONTINUE.
            ENDIF.
            IF lv_hc > lines( et_head ).
              APPEND INITIAL LINE TO et_head.
            ENDIF.
            READ TABLE et_head ASSIGNING FIELD-SYMBOL(<lv_hd>) INDEX lv_hc.
            IF sy-subrc = 0 AND <lv_hd> IS INITIAL.
              <lv_hd> = lv_fill.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
      IF lcl_util=>is_empty( ls_l ) = abap_false.
        APPEND ls_l TO et_row.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_CAT - the fields the program knows
*   Built at runtime, not typed in: for every part of the master record
*   the fields of its database table that the API structure also carries.
*   A field added to KNA1 and to the API structure (an append for a new
*   custom field) is therefore picked up without a change here.
*----------------------------------------------------------------------*
CLASS lcl_cat DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS groups RETURNING VALUE(rt) TYPE tt_gdef.
    CLASS-METHODS grp
      IMPORTING iv_grp    TYPE ty_grp
      RETURNING VALUE(rs) TYPE ty_gdef.
    CLASS-METHODS all RETURNING VALUE(rt) TYPE tt_fld.
    CLASS-METHODS by_name
      IMPORTING iv_name   TYPE string
      RETURNING VALUE(rs) TYPE ty_fld.
    " A heading from a file: TABLE-FIELD, a field name alone where it is
    " unique, or one of the names the LSMW files used.
    CLASS-METHODS resolve
      IMPORTING iv_head TYPE string
      EXPORTING es_fld  TYPE ty_fld
                ev_err  TYPE string.
    " The number column: KNA1-KUNNR or LFA1-LIFNR.
    CLASS-METHODS num_name RETURNING VALUE(rv) TYPE string.
    " The headings that identify an entry of a group: its parent's keys
    " and its own (the number column is not included).
    CLASS-METHODS key_names
      IMPORTING iv_grp    TYPE ty_grp
      RETURNING VALUE(rt) TYPE string_table.
    " The group a key belongs to and the groups a repeating group hangs on.
    CLASS-METHODS root_of
      IMPORTING iv_grp    TYPE ty_grp
      RETURNING VALUE(rv) TYPE ty_grp.
  PRIVATE SECTION.
    CLASS-DATA mt_fld  TYPE tt_fld.
    CLASS-DATA mt_gdef TYPE tt_gdef.
    TYPES: BEGIN OF ty_alias,
             alias TYPE string,
             name  TYPE string,
           END OF ty_alias.
    CLASS-DATA mt_alias TYPE SORTED TABLE OF ty_alias WITH UNIQUE KEY alias.

    CLASS-METHODS build.
    CLASS-METHODS add_table IMPORTING is_g TYPE ty_gdef.
    CLASS-METHODS add_addr.
    CLASS-METHODS add_comm.
    CLASS-METHODS add_text.
    CLASS-METHODS add_alias.
    CLASS-METHODS ddic
      IMPORTING iv_tab    TYPE clike
      RETURNING VALUE(rt) TYPE ddfields.
    CLASS-METHODS comps
      IMPORTING iv_struc  TYPE clike
      RETURNING VALUE(rt) TYPE string_table.
    CLASS-METHODS from_dfies
      IMPORTING is_d      TYPE dfies
                iv_grp    TYPE ty_grp
                iv_api    TYPE clike
      RETURNING VALUE(rs) TYPE ty_fld.
    CLASS-METHODS add
      IMPORTING is_f TYPE ty_fld.
ENDCLASS.


CLASS lcl_cat IMPLEMENTATION.

  METHOD groups.
    IF mt_gdef IS INITIAL.
      IF gc_side = 'C'.
        mt_gdef = VALUE #(
          ( grp = 'CENT' tab = 'KNA1' text = 'General data' api = 'CMDS_EI_VMD_CENTRAL_DATA' numfld = 'KUNNR' )
          ( grp = 'ADDR' tab = 'ADRC' text = 'Name and address' )
          ( grp = 'COMM' tab = 'ADR2' text = 'Telephone, fax, e-mail' )
          ( grp = 'TXTC' tab = 'STXH' text = 'Texts - general' )
          ( grp = 'COMP' tab = 'KNB1' text = 'Company code' keys = 'BUKRS' api = 'CMDS_EI_COMPANY_DATA' numfld = 'KUNNR' )
          ( grp = 'TXTB' tab = 'STXH' text = 'Texts - company code' parent = 'COMP' )
          ( grp = 'SALE' tab = 'KNVV' text = 'Sales area' keys = 'VKORG VTWEG SPART' api = 'CMDS_EI_SALES_DATA' numfld = 'KUNNR' )
          ( grp = 'TXTS' tab = 'STXH' text = 'Texts - sales area' parent = 'SALE' )
          ( grp = 'TAXI' tab = 'KNVI' text = 'Tax classification' rep = abap_true keys = 'ALAND TATYP' api = 'CMDS_EI_TAX_IND_DATA' numfld = 'KUNNR' )
          ( grp = 'PART' tab = 'KNVP' text = 'Partner functions' rep = abap_true parent = 'SALE' keys = 'PARVW PARZA' api = 'CMDS_EI_FUNCTIONS_DATA' numfld = 'KUNNR' )
          ( grp = 'UNLD' tab = 'KNVA' text = 'Unloading points' rep = abap_true keys = 'ABLAD' api = 'CMDS_EI_LOADING_DATA' numfld = 'KUNNR' )
          ( grp = 'BANK' tab = 'KNBK' text = 'Bank details' rep = abap_true keys = 'BANKS BANKL BANKN' api = 'CVIS_EI_BANKDETAIL_DATA' numfld = 'KUNNR' )
          ( grp = 'CONT' tab = 'KNVK' text = 'Contact persons' rep = abap_true keys = 'PARNR' api = 'CMDS_EI_CONTACTS_DATA' numfld = 'KUNNR' )
          ( grp = 'DUNN' tab = 'KNB5' text = 'Dunning data' rep = abap_true parent = 'COMP' keys = 'MABER' api = 'CMDS_EI_DUNNING_DATA' numfld = 'KUNNR' )
          ( grp = 'WTAX' tab = 'KNBW' text = 'Withholding tax' rep = abap_true parent = 'COMP' keys = 'WITHT' api = 'CMDS_EI_WTAX_DATA' numfld = 'KUNNR' ) ).
      ELSE.
        mt_gdef = VALUE #(
          ( grp = 'CENT' tab = 'LFA1' text = 'General data' api = 'VMDS_EI_VMD_CENTRAL_DATA' numfld = 'LIFNR' )
          ( grp = 'ADDR' tab = 'ADRC' text = 'Name and address' )
          ( grp = 'COMM' tab = 'ADR2' text = 'Telephone, fax, e-mail' )
          ( grp = 'TXTC' tab = 'STXH' text = 'Texts - general' )
          ( grp = 'COMP' tab = 'LFB1' text = 'Company code' keys = 'BUKRS' api = 'VMDS_EI_COMPANY_DATA' numfld = 'LIFNR' )
          ( grp = 'TXTB' tab = 'STXH' text = 'Texts - company code' parent = 'COMP' )
          ( grp = 'PURC' tab = 'LFM1' text = 'Purchasing organization' keys = 'EKORG' api = 'VMDS_EI_PURCHASING_DATA' numfld = 'LIFNR' )
          ( grp = 'TXTS' tab = 'STXH' text = 'Texts - purchasing organization' parent = 'PURC' )
          ( grp = 'PART' tab = 'WYT3' text = 'Partner functions' rep = abap_true parent = 'PURC' keys = 'LTSNR WERKS PARVW PARZA' api = 'VMDS_EI_FUNCTIONS_DATA' numfld = 'LIFNR' )
          ( grp = 'BANK' tab = 'LFBK' text = 'Bank details' rep = abap_true keys = 'BANKS BANKL BANKN' api = 'CVIS_EI_BANKDETAIL_DATA' numfld = 'LIFNR' )
          ( grp = 'CONT' tab = 'KNVK' text = 'Contact persons' rep = abap_true keys = 'PARNR' api = 'VMDS_EI_CONTACTS_DATA' numfld = 'LIFNR' )
          ( grp = 'DUNN' tab = 'LFB5' text = 'Dunning data' rep = abap_true parent = 'COMP' keys = 'MABER' api = 'VMDS_EI_DUNNING_DATA' numfld = 'LIFNR' )
          ( grp = 'WTAX' tab = 'LFBW' text = 'Withholding tax' rep = abap_true parent = 'COMP' keys = 'WITHT' api = 'VMDS_EI_WTAX_DAT' numfld = 'LIFNR' ) ).
      ENDIF.
    ENDIF.
    rt = mt_gdef.
  ENDMETHOD.

  METHOD grp.
    DATA(lt_g) = groups( ).
    READ TABLE lt_g INTO rs WITH KEY grp = iv_grp.
  ENDMETHOD.

  METHOD root_of.
    " The single-valued part an entry belongs to: a partner function to its
    " sales area, a dunning entry to its company code, everything without a
    " parent to the general data.
    DATA(ls_g) = grp( iv_grp ).
    rv = COND #( WHEN ls_g-parent IS NOT INITIAL THEN ls_g-parent
                 WHEN ls_g-rep = abap_true    THEN 'CENT'
                 ELSE iv_grp ).
  ENDMETHOD.

  METHOD num_name.
    rv = COND #( WHEN gc_side = 'C' THEN `KNA1-KUNNR` ELSE `LFA1-LIFNR` ).
  ENDMETHOD.

  METHOD key_names.
    DATA(ls_g) = grp( iv_grp ).
    IF ls_g-parent IS NOT INITIAL.
      rt = key_names( ls_g-parent ).
    ENDIF.
    IF ls_g-keys IS NOT INITIAL.
      SPLIT ls_g-keys AT ` ` INTO TABLE DATA(lt_k).
      LOOP AT lt_k INTO DATA(lv_k).
        APPEND |{ ls_g-tab }-{ lv_k }| TO rt.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD all.
    IF mt_fld IS INITIAL.
      build( ).
    ENDIF.
    rt = mt_fld.
  ENDMETHOD.

  METHOD by_name.
    IF mt_fld IS INITIAL.
      build( ).
    ENDIF.
    READ TABLE mt_fld INTO rs WITH KEY name = iv_name.
  ENDMETHOD.

  METHOD add.
    IF line_exists( mt_fld[ name = is_f-name ] ).
      RETURN.
    ENDIF.
    APPEND is_f TO mt_fld.
  ENDMETHOD.

  METHOD ddic.
    DATA lo_t TYPE REF TO cl_abap_typedescr.
    cl_abap_typedescr=>describe_by_name(
      EXPORTING  p_name         = iv_tab
      RECEIVING  p_descr_ref    = lo_t
      EXCEPTIONS type_not_found = 1
                 OTHERS         = 2 ).
    IF sy-subrc <> 0 OR lo_t->kind <> cl_abap_typedescr=>kind_struct.
      RETURN.
    ENDIF.
    DATA(lo_s) = CAST cl_abap_structdescr( lo_t ).
    lo_s->get_ddic_field_list(
      EXPORTING  p_langu      = sy-langu
      RECEIVING  p_field_list = rt
      EXCEPTIONS not_found    = 1
                 no_ddic_type = 2
                 OTHERS       = 3 ).
    IF sy-subrc <> 0.
      CLEAR rt.
    ENDIF.
  ENDMETHOD.

  METHOD comps.
    DATA lo_t TYPE REF TO cl_abap_typedescr.
    cl_abap_typedescr=>describe_by_name(
      EXPORTING  p_name         = iv_struc
      RECEIVING  p_descr_ref    = lo_t
      EXCEPTIONS type_not_found = 1
                 OTHERS         = 2 ).
    IF sy-subrc <> 0 OR lo_t->kind <> cl_abap_typedescr=>kind_struct.
      RETURN.
    ENDIF.
    " The included view: the components of an append or include structure
    " are listed as components of their own, which is how a custom field
    " added through an append shows up.
    DATA(lt_c) = CAST cl_abap_structdescr( lo_t )->get_included_view( ).
    LOOP AT lt_c INTO DATA(ls_c).
      APPEND CONV string( ls_c-name ) TO rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD from_dfies.
    rs-name     = |{ is_d-tabname }-{ is_d-fieldname }|.
    rs-grp      = iv_grp.
    rs-tab      = is_d-tabname.
    rs-fld      = is_d-fieldname.
    rs-api      = iv_api.
    rs-text     = COND #( WHEN is_d-scrtext_l IS NOT INITIAL THEN is_d-scrtext_l
                          ELSE is_d-fieldtext ).
    rs-rollname = is_d-rollname.
    rs-convexit = is_d-convexit.
    rs-inttype  = is_d-inttype.
    rs-leng     = is_d-leng.
    rs-decimals = is_d-decimals.
    rs-lower    = xsdbool( is_d-lowercase IS NOT INITIAL ).
    rs-checktab = is_d-checktable.
    rs-domname  = is_d-domname.
    rs-valexi   = xsdbool( is_d-valexi IS NOT INITIAL ).
  ENDMETHOD.

  METHOD add_table.
    DATA(lt_db) = ddic( is_g-tab ).
    IF lt_db IS INITIAL.
      RETURN.
    ENDIF.
    DATA lt_api TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    DATA(lt_c) = comps( is_g-api ).
    LOOP AT lt_c INTO DATA(lv_c).
      INSERT lv_c INTO TABLE lt_api.
    ENDLOOP.

    " ---- keys: the number on the general data, the group's own keys here
    DATA lt_key TYPE SORTED TABLE OF fieldname WITH UNIQUE KEY table_line.
    IF is_g-keys IS NOT INITIAL.
      SPLIT is_g-keys AT ` ` INTO TABLE DATA(lt_k).
      LOOP AT lt_k INTO DATA(lv_k).
        INSERT CONV fieldname( lv_k ) INTO TABLE lt_key.
      ENDLOOP.
    ENDIF.

    LOOP AT lt_db INTO DATA(ls_d) WHERE datatype <> 'CLNT'.
      DATA(ls_f) = from_dfies( is_d = ls_d iv_grp = is_g-grp iv_api = ls_d-fieldname ).
      IF ls_d-fieldname = is_g-numfld.
        IF is_g-grp = 'CENT'.
          ls_f-key  = abap_true.
          ls_f-must = abap_true.
          add( ls_f ).
        ENDIF.
        CONTINUE.
      ENDIF.
      IF line_exists( lt_key[ table_line = ls_d-fieldname ] ).
        ls_f-key  = abap_true.
        ls_f-must = abap_true.
        add( ls_f ).
        CONTINUE.
      ENDIF.
      IF ls_d-keyflag = abap_true.
        " A key the API does not take as data (the vendor partner
        " function's plant, for instance) stays out of the data fields.
        CONTINUE.
      ENDIF.
      IF line_exists( lt_api[ table_line = CONV string( ls_d-fieldname ) ] ).
        add( ls_f ).
      ENDIF.
    ENDLOOP.

    " The customer's partner function names its partner in KUNN2, LIFNR,
    " PERNR or PARNR depending on the function; the API takes one number.
    IF is_g-grp = 'PART' AND gc_side = 'C'.
      DATA(ls_p) = VALUE ty_fld( name = `KNVP-PARTNER` grp = 'PART' tab = 'KNVP'
                                 api = 'PARTNER' rollname = 'GPANR' inttype = 'C' leng = 10
                                 text = `Partner number (customer, supplier, personnel or contact)` ).
      add( ls_p ).
    ENDIF.
  ENDMETHOD.

  METHOD add_addr.
    " ADRC field -> component of the API's address structure (BAPIAD1VL).
    TYPES: BEGIN OF ty_m, adrc TYPE fieldname, api TYPE fieldname, END OF ty_m.
    DATA lt_m TYPE STANDARD TABLE OF ty_m WITH EMPTY KEY.
    lt_m = VALUE #(
      ( adrc = 'NAME1'      api = 'NAME' )       ( adrc = 'NAME2'      api = 'NAME_2' )
      ( adrc = 'NAME3'      api = 'NAME_3' )     ( adrc = 'NAME4'      api = 'NAME_4' )
      ( adrc = 'NAME_CO'    api = 'C_O_NAME' )   ( adrc = 'SORT1'      api = 'SORT1' )
      ( adrc = 'SORT2'      api = 'SORT2' )      ( adrc = 'TITLE'      api = 'TITLE' )
      ( adrc = 'STREET'     api = 'STREET' )     ( adrc = 'HOUSE_NUM1' api = 'HOUSE_NO' )
      ( adrc = 'HOUSE_NUM2' api = 'HOUSE_NO2' )  ( adrc = 'STR_SUPPL1' api = 'STR_SUPPL1' )
      ( adrc = 'STR_SUPPL2' api = 'STR_SUPPL2' ) ( adrc = 'STR_SUPPL3' api = 'STR_SUPPL3' )
      ( adrc = 'LOCATION'   api = 'LOCATION' )   ( adrc = 'BUILDING'   api = 'BUILDING' )
      ( adrc = 'FLOOR'      api = 'FLOOR' )      ( adrc = 'ROOMNUMBER' api = 'ROOM_NO' )
      ( adrc = 'CITY1'      api = 'CITY' )       ( adrc = 'CITY2'      api = 'DISTRICT' )
      ( adrc = 'POST_CODE1' api = 'POSTL_COD1' ) ( adrc = 'POST_CODE2' api = 'POSTL_COD2' )
      ( adrc = 'PO_BOX'     api = 'PO_BOX' )     ( adrc = 'COUNTRY'    api = 'COUNTRY' )
      ( adrc = 'REGION'     api = 'REGION' )     ( adrc = 'LANGU'      api = 'LANGU' )
      ( adrc = 'TRANSPZONE' api = 'TRANSPZONE' ) ( adrc = 'TIME_ZONE'  api = 'TIME_ZONE' )
      ( adrc = 'TAXJURCODE' api = 'TAXJURCODE' ) ( adrc = 'DEFLT_COMM' api = 'COMM_TYPE' )
      ( adrc = 'EXTENSION1' api = 'EXTENS_1' )   ( adrc = 'EXTENSION2' api = 'EXTENS_2' ) ).
    DATA(lt_db) = ddic( 'ADRC' ).
    DATA lt_api TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    DATA(lt_c) = comps( 'BAPIAD1VL' ).
    LOOP AT lt_c INTO DATA(lv_c).
      INSERT lv_c INTO TABLE lt_api.
    ENDLOOP.
    LOOP AT lt_m INTO DATA(ls_m).
      READ TABLE lt_db INTO DATA(ls_d) WITH KEY fieldname = ls_m-adrc.
      IF sy-subrc <> 0 OR NOT line_exists( lt_api[ table_line = CONV string( ls_m-api ) ] ).
        CONTINUE.
      ENDIF.
      add( from_dfies( is_d = ls_d iv_grp = 'ADDR' iv_api = ls_m-api ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD add_comm.
    " The standard telephone, mobile, fax and e-mail. API component names
    " are those of BAPIADTEL, BAPIADFAX and BAPIADSMTP.
    DATA(lt_2) = ddic( 'ADR2' ).
    DATA(lt_3) = ddic( 'ADR3' ).
    DATA(lt_6) = ddic( 'ADR6' ).
    READ TABLE lt_2 INTO DATA(ls_d) WITH KEY fieldname = 'TEL_NUMBER'.
    IF sy-subrc = 0.
      add( from_dfies( is_d = ls_d iv_grp = 'COMM' iv_api = 'TELEPHONE' ) ).
      DATA(ls_mob) = from_dfies( is_d = ls_d iv_grp = 'COMM' iv_api = 'MOBILE' ).
      ls_mob-name = `ADR2-MOB_NUMBER`.
      ls_mob-text = `Mobile number`.
      add( ls_mob ).
    ENDIF.
    READ TABLE lt_2 INTO ls_d WITH KEY fieldname = 'TEL_EXTENS'.
    IF sy-subrc = 0.
      add( from_dfies( is_d = ls_d iv_grp = 'COMM' iv_api = 'EXTENSION' ) ).
    ENDIF.
    READ TABLE lt_3 INTO ls_d WITH KEY fieldname = 'FAX_NUMBER'.
    IF sy-subrc = 0.
      add( from_dfies( is_d = ls_d iv_grp = 'COMM' iv_api = 'FAX' ) ).
    ENDIF.
    READ TABLE lt_3 INTO ls_d WITH KEY fieldname = 'FAX_EXTENS'.
    IF sy-subrc = 0.
      add( from_dfies( is_d = ls_d iv_grp = 'COMM' iv_api = 'EXTENSION' ) ).
    ENDIF.
    READ TABLE lt_6 INTO ls_d WITH KEY fieldname = 'SMTP_ADDR'.
    IF sy-subrc = 0.
      add( from_dfies( is_d = ls_d iv_grp = 'COMM' iv_api = 'E_MAIL' ) ).
    ENDIF.
  ENDMETHOD.

  METHOD add_text.
    " One column per text ID: TEXT-KNVV-Z911 and so on.
    DATA lt_obj TYPE RANGE OF tdobject.
    IF gc_side = 'C'.
      lt_obj = VALUE #( sign = 'I' option = 'EQ' ( low = 'KNA1' ) ( low = 'KNB1' ) ( low = 'KNVV' ) ).
    ELSE.
      lt_obj = VALUE #( sign = 'I' option = 'EQ' ( low = 'LFA1' ) ( low = 'LFB1' ) ( low = 'LFM1' ) ).
    ENDIF.
    SELECT tdobject, tdid FROM ttxid
      WHERE tdobject IN @lt_obj
      ORDER BY tdobject, tdid
      INTO TABLE @DATA(lt_id).
    IF lt_id IS INITIAL.
      RETURN.
    ENDIF.
    SELECT tdobject, tdid, tdspras, tdtext FROM ttxit
      FOR ALL ENTRIES IN @lt_id
      WHERE tdobject = @lt_id-tdobject AND tdid = @lt_id-tdid
      INTO TABLE @DATA(lt_tx).
    LOOP AT lt_id INTO DATA(ls_id).
      " READ TABLE rather than VALUE string( lt_tx[ ... ]-tdtext OPTIONAL ):
      " TDTEXT is a character field, and a VALUE with a table expression
      " takes only a compatible type.
      DATA lv_txt TYPE string.
      CLEAR lv_txt.
      READ TABLE lt_tx INTO DATA(ls_tx)
           WITH KEY tdobject = ls_id-tdobject tdid = ls_id-tdid tdspras = sy-langu.
      IF sy-subrc <> 0.
        READ TABLE lt_tx INTO ls_tx
             WITH KEY tdobject = ls_id-tdobject tdid = ls_id-tdid tdspras = 'E'.
      ENDIF.
      IF sy-subrc = 0.
        lv_txt = ls_tx-tdtext.
      ENDIF.
      DATA(lv_grp) = COND ty_grp( WHEN ls_id-tdobject = 'KNA1' OR ls_id-tdobject = 'LFA1' THEN 'TXTC'
                                  WHEN ls_id-tdobject = 'KNB1' OR ls_id-tdobject = 'LFB1' THEN 'TXTB'
                                  ELSE 'TXTS' ).
      add( VALUE #( name = |TEXT-{ ls_id-tdobject }-{ ls_id-tdid }| grp = lv_grp
                    tab = 'STXH' inttype = 'g' lower = abap_true leng = 0
                    text = |Text { ls_id-tdid }: { lv_txt }|
                    tdobj = ls_id-tdobject tdid = ls_id-tdid ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD add_alias.
    " The customer's own column names - KNA1 address fields, which in
    " S/4HANA belong to the business partner's address, and the names the
    " LSMW files used.
    DATA(lv_g) = COND string( WHEN gc_side = 'C' THEN `KNA1` ELSE `LFA1` ).
    DATA(lv_b) = COND string( WHEN gc_side = 'C' THEN `KNB1` ELSE `LFB1` ).
    DATA(lv_s) = COND string( WHEN gc_side = 'C' THEN `KNVV` ELSE `LFM1` ).
    DATA lt_a TYPE STANDARD TABLE OF ty_alias WITH EMPTY KEY.
    lt_a = VALUE #(
      ( alias = |{ lv_g }-NAME1| name = `ADRC-NAME1` ) ( alias = |{ lv_g }-NAME2| name = `ADRC-NAME2` )
      ( alias = |{ lv_g }-NAME3| name = `ADRC-NAME3` ) ( alias = |{ lv_g }-NAME4| name = `ADRC-NAME4` )
      ( alias = |{ lv_g }-SORTL| name = `ADRC-SORT1` ) ( alias = |{ lv_g }-STRAS| name = `ADRC-STREET` )
      ( alias = |{ lv_g }-ORT01| name = `ADRC-CITY1` ) ( alias = |{ lv_g }-ORT02| name = `ADRC-CITY2` )
      ( alias = |{ lv_g }-PSTLZ| name = `ADRC-POST_CODE1` ) ( alias = |{ lv_g }-PSTL2| name = `ADRC-POST_CODE2` )
      ( alias = |{ lv_g }-PFACH| name = `ADRC-PO_BOX` ) ( alias = |{ lv_g }-REGIO| name = `ADRC-REGION` )
      ( alias = |{ lv_g }-LAND1| name = `ADRC-COUNTRY` ) ( alias = |{ lv_g }-SPRAS| name = `ADRC-LANGU` )
      ( alias = |{ lv_g }-LZONE| name = `ADRC-TRANSPZONE` ) ( alias = |{ lv_g }-TELF1| name = `ADR2-TEL_NUMBER` )
      ( alias = |{ lv_g }-TELFX| name = `ADR3-FAX_NUMBER` )
      ( alias = `POST` name = `ADRC-POST_CODE1` ) ( alias = `POST_CODE` name = `ADRC-POST_CODE1` )
      ( alias = `SUPPL2` name = `ADRC-STR_SUPPL2` ) ( alias = `SUPPL3` name = `ADRC-STR_SUPPL3` )
      ( alias = `TEL` name = `ADR2-TEL_NUMBER` ) ( alias = `FAX` name = `ADR3-FAX_NUMBER` )
      ( alias = `MOB_NUMBER` name = `ADR2-MOB_NUMBER` )
      ( alias = `ZTERM_B` name = |{ lv_b }-ZTERM| ) ( alias = `ZTERMB` name = |{ lv_b }-ZTERM| )
      ( alias = `ZTERM_V` name = |{ lv_s }-ZTERM| ) ( alias = `ZTERMV` name = |{ lv_s }-ZTERM| )
      ( alias = |{ lv_g }_LOEVM| name = |{ lv_g }-LOEVM| ) ( alias = |{ lv_b }_LOEVM| name = |{ lv_b }-LOEVM| ) ).
    IF gc_side = 'C'.
      APPEND VALUE #( alias = `KTONR` name = `KNVP-PARTNER` ) TO lt_a.
    ENDIF.
    LOOP AT lt_a INTO DATA(ls_a).
      IF line_exists( mt_fld[ name = ls_a-name ] ).
        INSERT ls_a INTO TABLE mt_alias.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD build.
    CLEAR: mt_fld, mt_alias.
    LOOP AT groups( ) INTO DATA(ls_g).
      CASE ls_g-grp.
        WHEN 'ADDR'.
          add_addr( ).
        WHEN 'COMM'.
          add_comm( ).
        WHEN 'TXTC' OR 'TXTB' OR 'TXTS'.
          IF ls_g-grp = 'TXTC'.
            add_text( ).
          ENDIF.
        WHEN OTHERS.
          add_table( ls_g ).
      ENDCASE.
    ENDLOOP.
    add_alias( ).
  ENDMETHOD.

  METHOD resolve.
    CLEAR: es_fld, ev_err.
    IF mt_fld IS INITIAL.
      build( ).
    ENDIF.
    DATA(lv_h) = to_upper( condense( val = iv_head del = ` ` ) ).
    REPLACE ALL OCCURRENCES OF ` ` IN lv_h WITH ``.

    READ TABLE mt_fld INTO es_fld WITH KEY name = lv_h.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.
    READ TABLE mt_alias INTO DATA(ls_a) WITH KEY alias = lv_h.
    IF sy-subrc = 0.
      READ TABLE mt_fld INTO es_fld WITH KEY name = ls_a-name.
      RETURN.
    ENDIF.

    " A field name on its own - only where exactly one field has it.
    IF lv_h NS '-'.
      DATA lv_n   TYPE i.
      DATA lv_alt TYPE string.
      LOOP AT mt_fld INTO DATA(ls_f) WHERE fld = lv_h.
        lv_n = lv_n + 1.
        es_fld = ls_f.
        lv_alt = COND #( WHEN lv_alt IS INITIAL THEN ls_f-name ELSE |{ lv_alt }, { ls_f-name }| ).
      ENDLOOP.
      IF lv_n = 1.
        RETURN.
      ELSEIF lv_n > 1.
        CLEAR es_fld.
        ev_err = |Column "{ iv_head }" is in more than one table - name it as one of: { lv_alt }|.
        RETURN.
      ENDIF.
    ENDIF.
    ev_err = |Column "{ iv_head }" is not a field this program knows|.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* LCL_VAL - a value as the file shows it and as SAP holds it
*   File values are text. Each is converted with the field's own rules
*   from the ABAP Dictionary: its type, its length, its conversion routine
*   (leading zeros, language keys) and whether it may be lower case.
*----------------------------------------------------------------------*
CLASS lcl_val DEFINITION FINAL.
  PUBLIC SECTION.
    " A data object of the field's own type.
    CLASS-METHODS new_data
      IMPORTING is_f      TYPE ty_fld
      RETURNING VALUE(rr) TYPE REF TO data.
    " File text -> value of the field's type. EV_ERR says why it cannot be.
    CLASS-METHODS to_int
      IMPORTING is_f    TYPE ty_fld
                iv_in   TYPE string
      EXPORTING er_val  TYPE REF TO data
                ev_err  TYPE string.
    " Value of the field's type -> file text.
    CLASS-METHODS to_ext
      IMPORTING is_f      TYPE ty_fld
                iv_val    TYPE any
      RETURNING VALUE(rv) TYPE string.
  PRIVATE SECTION.
    CLASS-METHODS to_time
      IMPORTING iv_in     TYPE string
      RETURNING VALUE(rv) TYPE string.
ENDCLASS.


CLASS lcl_val IMPLEMENTATION.

  METHOD new_data.
    TRY.
        IF is_f-inttype = 'g'.
          CREATE DATA rr TYPE string.
        ELSEIF is_f-fld IS NOT INITIAL AND is_f-tab <> 'STXH'.
          DATA(lv_t) = |{ is_f-tab }-{ is_f-fld }|.
          CREATE DATA rr TYPE (lv_t).
        ELSEIF is_f-rollname IS NOT INITIAL.
          CREATE DATA rr TYPE (is_f-rollname).
        ELSE.
          CREATE DATA rr TYPE string.
        ENDIF.
      CATCH cx_sy_create_data_error.
        CREATE DATA rr TYPE string.
    ENDTRY.
  ENDMETHOD.

  METHOD to_time.
    " 9:00, 09:00, 0900, 09:00:00, 090000 - and an Excel time, which is a
    " fraction of a day (0.375 is 09:00).
    DATA(lv) = condense( iv_in ).
    IF lv CO '0123456789.' AND lv CS '.'.
      TRY.
          DATA(lv_f) = CONV decfloat34( lv ).
          IF lv_f < 1.
            DATA(lv_s) = CONV i( round( val = lv_f * 86400 dec = 0 ) ).
            rv = |{ lv_s DIV 3600 WIDTH = 2 ALIGN = RIGHT PAD = '0' }| &&
                 |{ ( lv_s MOD 3600 ) DIV 60 WIDTH = 2 ALIGN = RIGHT PAD = '0' }| &&
                 |{ lv_s MOD 60 WIDTH = 2 ALIGN = RIGHT PAD = '0' }|.
            RETURN.
          ENDIF.
        CATCH cx_sy_conversion_error.
          RETURN.
      ENDTRY.
    ENDIF.
    IF lv CS ':'.
      SPLIT lv AT ':' INTO DATA(lv_h) DATA(lv_m) DATA(lv_x).
    ELSEIF strlen( lv ) = 4 OR strlen( lv ) = 6.
      lv_h = lv(2).
      lv_m = lv+2(2).
      lv_x = COND #( WHEN strlen( lv ) = 6 THEN lv+4(2) ).
    ELSE.
      RETURN.
    ENDIF.
    IF lv_x IS INITIAL.
      lv_x = `00`.
    ENDIF.
    IF lv_h CN '0123456789' OR lv_m CN '0123456789' OR lv_x CN '0123456789'
       OR strlen( lv_h ) > 2 OR strlen( lv_m ) <> 2 OR strlen( lv_x ) <> 2.
      RETURN.
    ENDIF.
    IF CONV i( lv_h ) > 23 OR CONV i( lv_m ) > 59 OR CONV i( lv_x ) > 59.
      RETURN.
    ENDIF.
    rv = |{ lv_h WIDTH = 2 ALIGN = RIGHT PAD = '0' }{ lv_m }{ lv_x }|.
  ENDMETHOD.

  METHOD to_int.
    CLEAR ev_err.
    er_val = new_data( is_f ).
    ASSIGN er_val->* TO FIELD-SYMBOL(<lv>).
    DATA(lv_in) = COND string( WHEN is_f-inttype = 'g' THEN iv_in ELSE condense( iv_in ) ).
    IF lv_in IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        CASE is_f-inttype.
          WHEN 'g'.
            <lv> = lv_in.

          WHEN 'D'.
            DATA(lv_d) = lcl_util=>to_date( lv_in ).
            IF lv_d IS INITIAL.
              ev_err = |"{ lv_in }" is not a date - write it as YYYY/MM/DD|.
              RETURN.
            ENDIF.
            <lv> = lv_d.

          WHEN 'T'.
            DATA(lv_t) = to_time( lv_in ).
            IF lv_t IS INITIAL.
              ev_err = |"{ lv_in }" is not a time - write it as HH:MM|.
              RETURN.
            ENDIF.
            <lv> = lv_t.

          WHEN 'P' OR 'I' OR 'F' OR 'b' OR 's' OR '8' OR 'a' OR 'e'.
            DATA(lv_num) = lv_in.
            REPLACE ALL OCCURRENCES OF `,` IN lv_num WITH ``.
            IF lv_num CN '0123456789.-+ E'.
              ev_err = |"{ lv_in }" is not a number|.
              RETURN.
            ENDIF.
            <lv> = lv_num.

          WHEN 'N'.
            IF lv_in CN '0123456789'.
              ev_err = |"{ lv_in }" may only contain digits|.
              RETURN.
            ENDIF.
            IF is_f-leng > 0 AND strlen( lv_in ) > is_f-leng.
              ev_err = |"{ lv_in }" is longer than { is_f-leng } digits|.
              RETURN.
            ENDIF.
            <lv> = lv_in.

          WHEN OTHERS.
            DATA(lv_c) = lv_in.
            " A word in a one character field is a flag written out in full:
            " Yes, True, X.
            IF is_f-leng = 1 AND strlen( lv_c ) > 1 AND is_f-convexit IS INITIAL.
              lv_c = lcl_util=>flag( lv_c ).
            ENDIF.
            IF is_f-lower = abap_false.
              lv_c = to_upper( lv_c ).
            ENDIF.
            IF is_f-convexit IS NOT INITIAL.
              DATA(lv_fm) = |CONVERSION_EXIT_{ is_f-convexit }_INPUT|.
              TRY.
                  CALL FUNCTION lv_fm
                    EXPORTING  input  = lv_c
                    IMPORTING  output = <lv>
                    EXCEPTIONS OTHERS = 1.
                  IF sy-subrc <> 0.
                    ev_err = |"{ lv_in }" is not a valid value for { is_f-name }|.
                    RETURN.
                  ENDIF.
                CATCH cx_sy_dyn_call_error.
                  <lv> = lv_c.
              ENDTRY.
            ELSE.
              IF is_f-leng > 0 AND strlen( lv_c ) > is_f-leng.
                ev_err = |"{ lv_in }" is longer than the { is_f-leng } characters { is_f-name } holds|.
                RETURN.
              ENDIF.
              <lv> = lv_c.
            ENDIF.
        ENDCASE.
      CATCH cx_sy_conversion_error cx_sy_arithmetic_error.
        ev_err = |"{ lv_in }" does not fit { is_f-name }|.
    ENDTRY.
  ENDMETHOD.

  METHOD to_ext.
    IF iv_val IS INITIAL.
      RETURN.
    ENDIF.
    CASE is_f-inttype.
      WHEN 'g'.
        rv = iv_val.
      WHEN 'D'.
        DATA(lv_d) = CONV d( iv_val ).
        rv = |{ lv_d(4) }/{ lv_d+4(2) }/{ lv_d+6(2) }|.
      WHEN 'T'.
        DATA(lv_t) = CONV t( iv_val ).
        rv = |{ lv_t(2) }:{ lv_t+2(2) }|.
        IF lv_t+4(2) <> '00'.
          rv = |{ rv }:{ lv_t+4(2) }|.
        ENDIF.
      WHEN 'P' OR 'I' OR 'F' OR 'b' OR 's' OR '8' OR 'a' OR 'e'.
        DATA lv_num TYPE decfloat34.
        lv_num = iv_val.
        rv = |{ lv_num NUMBER = RAW }|.
      WHEN OTHERS.
        IF is_f-convexit IS NOT INITIAL.
          DATA(lv_fm) = |CONVERSION_EXIT_{ is_f-convexit }_OUTPUT|.
          DATA lv_out TYPE c LENGTH 255.
          TRY.
              CALL FUNCTION lv_fm
                EXPORTING  input  = iv_val
                IMPORTING  output = lv_out
                EXCEPTIONS OTHERS = 1.
              IF sy-subrc = 0.
                rv = condense( lv_out ).
                RETURN.
              ENDIF.
            CATCH cx_sy_dyn_call_error.
              CLEAR lv_out.
          ENDTRY.
        ENDIF.
        rv = condense( CONV string( iv_val ) ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* LCL_CHK - is the value allowed?
*   The field's check table, read through its foreign key in the ABAP
*   Dictionary (DD05S) so that a key with more than one field - a region
*   checked together with its country - is checked as SAP checks it; and
*   the fixed values of its domain. Answers are kept, so a value used on a
*   thousand rows is looked up once.
*----------------------------------------------------------------------*
CLASS lcl_chk DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_ctx,
             fld TYPE fieldname,
             val TYPE string,
           END OF ty_ctx,
           tt_ctx TYPE HASHED TABLE OF ty_ctx WITH UNIQUE KEY fld.
    " IT_CTX holds the other fields of the same record (internal values),
    " for foreign keys with more than one field.
    CLASS-METHODS check
      IMPORTING is_f          TYPE ty_fld
                VALUE(iv_val) TYPE string
                it_ctx        TYPE tt_ctx
      RETURNING VALUE(rv) TYPE string.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_fk,
             checktable TYPE tabname,
             checkfield TYPE fieldname,
             fortable   TYPE tabname,
             forkey     TYPE fieldname,
           END OF ty_fk,
           tt_fk TYPE STANDARD TABLE OF ty_fk WITH EMPTY KEY.
    TYPES: BEGIN OF ty_fkc,
             tab TYPE tabname,
             fld TYPE fieldname,
             fk  TYPE tt_fk,
           END OF ty_fkc.
    CLASS-DATA mt_fkc TYPE HASHED TABLE OF ty_fkc WITH UNIQUE KEY tab fld.
    TYPES: BEGIN OF ty_hit,
             key TYPE string,
             ok  TYPE abap_bool,
           END OF ty_hit.
    CLASS-DATA mt_hit TYPE HASHED TABLE OF ty_hit WITH UNIQUE KEY key.
    TYPES: BEGIN OF ty_dv,
             domname TYPE domname,
             low     TYPE string,
             high    TYPE string,
           END OF ty_dv.
    CLASS-DATA mt_dv  TYPE SORTED TABLE OF ty_dv WITH NON-UNIQUE KEY domname.
    CLASS-DATA mt_dvr TYPE SORTED TABLE OF domname WITH UNIQUE KEY table_line.
    CLASS-METHODS fk
      IMPORTING iv_tab    TYPE tabname
                iv_fld    TYPE fieldname
      RETURNING VALUE(rt) TYPE tt_fk.
ENDCLASS.


CLASS lcl_chk IMPLEMENTATION.

  METHOD fk.
    READ TABLE mt_fkc INTO DATA(ls_c) WITH TABLE KEY tab = iv_tab fld = iv_fld.
    IF sy-subrc = 0.
      rt = ls_c-fk.
      RETURN.
    ENDIF.
    " Through the dictionary's own API, not a SELECT on DD05S: the check
    " table is not a column of DD05S on S/4HANA 2025 ("Unknown column name
    " CHECKTABLE"). DD05M, which DDIF_FK_GET fills, carries both sides of
    " every key field pair together with the check table.
    DATA: lv_tab TYPE ddobjname,
          lv_fld TYPE dfies-fieldname,
          lt_05m TYPE STANDARD TABLE OF dd05m,
          lt_08v TYPE STANDARD TABLE OF dd08v.
    lv_tab = iv_tab.
    lv_fld = iv_fld.
    CALL FUNCTION 'DDIF_FK_GET'
      EXPORTING
        tabname       = lv_tab
        fieldname     = lv_fld
        state         = 'A'
      TABLES
        dd08v_tab     = lt_08v
        dd05m_tab     = lt_05m
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc = 0.
      SORT lt_05m BY primpos.
      LOOP AT lt_05m INTO DATA(ls_05m).
        APPEND VALUE #( checktable = ls_05m-checktable checkfield = ls_05m-checkfield
                        fortable   = ls_05m-fortable   forkey     = ls_05m-forkey ) TO rt.
      ENDLOOP.
    ENDIF.
    INSERT VALUE #( tab = iv_tab fld = iv_fld fk = rt ) INTO TABLE mt_fkc.
  ENDMETHOD.

  METHOD check.
    IF iv_val IS INITIAL OR is_f-inttype = 'g'.
      RETURN.
    ENDIF.

    " ---- fixed values of the domain
    IF is_f-valexi = abap_true AND is_f-domname IS NOT INITIAL.
      IF NOT line_exists( mt_dvr[ table_line = is_f-domname ] ).
        SELECT domvalue_l, domvalue_h FROM dd07l
          WHERE domname = @is_f-domname AND as4local = 'A'
          INTO TABLE @DATA(lt_dv).
        LOOP AT lt_dv INTO DATA(ls_dv).
          INSERT VALUE #( domname = is_f-domname low = condense( CONV string( ls_dv-domvalue_l ) )
                          high = condense( CONV string( ls_dv-domvalue_h ) ) ) INTO TABLE mt_dv.
        ENDLOOP.
        INSERT is_f-domname INTO TABLE mt_dvr.
      ENDIF.
      DATA lv_found TYPE abap_bool.
      DATA lv_list  TYPE string.
      DATA(lv_v) = condense( iv_val ).
      LOOP AT mt_dv INTO DATA(ls_v) WHERE domname = is_f-domname.
        IF ( ls_v-high IS INITIAL AND lv_v = ls_v-low )
           OR ( ls_v-high IS NOT INITIAL AND lv_v >= ls_v-low AND lv_v <= ls_v-high ).
          lv_found = abap_true.
          EXIT.
        ENDIF.
        lv_list = COND #( WHEN lv_list IS INITIAL THEN ls_v-low ELSE |{ lv_list }, { ls_v-low }| ).
      ENDLOOP.
      IF lv_found = abap_false AND line_exists( mt_dv[ domname = is_f-domname ] ).
        rv = |"{ lv_v }" is not allowed for { is_f-name } - allowed: { lv_list }|.
        RETURN.
      ENDIF.
    ENDIF.

    " ---- check table, through the foreign key
    IF is_f-checktab IS INITIAL OR is_f-fld IS INITIAL OR is_f-tab = 'STXH'
       OR is_f-inttype NA 'CN'.
      RETURN.
    ENDIF.
    DATA(lt_fk) = fk( iv_tab = is_f-tab iv_fld = is_f-fld ).
    IF lt_fk IS INITIAL.
      RETURN.
    ENDIF.
    DATA lt_where TYPE string_table.
    DATA lv_key   TYPE string.
    LOOP AT lt_fk INTO DATA(ls_fk).
      DATA lv_cv TYPE string.
      CLEAR lv_cv.
      IF ls_fk-checkfield = 'MANDT' OR ls_fk-checkfield = 'CLIENT' OR ls_fk-forkey = 'MANDT'.
        CONTINUE.
      ELSEIF ls_fk-fortable = '*'.
        " A constant in the foreign key, written with its quotes.
        lv_cv = ls_fk-forkey.
        REPLACE ALL OCCURRENCES OF `'` IN lv_cv WITH ``.
      ELSEIF ls_fk-forkey = is_f-fld.
        lv_cv = iv_val.
      ELSE.
        READ TABLE it_ctx INTO DATA(ls_ctx) WITH TABLE KEY fld = ls_fk-forkey.
        IF sy-subrc <> 0.
          " The other half of the key is not known here - SAP checks it
          " when the record is posted.
          RETURN.
        ENDIF.
        lv_cv = ls_ctx-val.
      ENDIF.
      REPLACE ALL OCCURRENCES OF `'` IN lv_cv WITH `''`.
      APPEND |{ ls_fk-checkfield } = '{ lv_cv }'| TO lt_where.
      lv_key = |{ lv_key }/{ ls_fk-checkfield }={ lv_cv }|.
    ENDLOOP.
    IF lt_where IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lv_tab) = lt_fk[ 1 ]-checktable.
    lv_key = |{ lv_tab }{ lv_key }|.
    READ TABLE mt_hit INTO DATA(ls_hit) WITH TABLE KEY key = lv_key.
    IF sy-subrc <> 0.
      DATA lv_cond TYPE string.
      CLEAR lv_cond.
      LOOP AT lt_where INTO DATA(lv_w).
        lv_cond = COND #( WHEN lv_cond IS INITIAL THEN lv_w ELSE |{ lv_cond } AND { lv_w }| ).
      ENDLOOP.
      ls_hit-key = lv_key.
      TRY.
          SELECT SINGLE @abap_true FROM (lv_tab) WHERE (lv_cond) INTO @ls_hit-ok.
        CATCH cx_sy_dynamic_osql_error.
          ls_hit-ok = abap_true.
      ENDTRY.
      INSERT ls_hit INTO TABLE mt_hit.
    ENDIF.
    IF ls_hit-ok = abap_false.
      rv = |"{ iv_val }" is not allowed for { is_f-name } - it is not in { lv_tab }|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* LCL_CFG - what the system says about a number, a group or a role
*----------------------------------------------------------------------*
CLASS lcl_cfg DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES tt_role TYPE STANDARD TABLE OF bu_role WITH EMPTY KEY.
    CLASS-METHODS guid_of
      IMPORTING VALUE(iv_num) TYPE kunnr
      RETURNING VALUE(rv)     TYPE bu_partner_guid.
    CLASS-METHODS bp_of
      IMPORTING VALUE(iv_num) TYPE kunnr
      RETURNING VALUE(rv)     TYPE bu_partner.
    " Business partners given on the screen -> their customer / supplier.
    CLASS-METHODS nums_of_bp
      IMPORTING it_bp         TYPE STANDARD TABLE
      RETURNING VALUE(rt)     TYPE string_table.
    CLASS-METHODS num_by_guid
      IMPORTING VALUE(iv_guid) TYPE bu_partner_guid
      RETURNING VALUE(rv)      TYPE string.
    CLASS-METHODS bp_by_guid
      IMPORTING VALUE(iv_guid) TYPE bu_partner_guid
      RETURNING VALUE(rv)      TYPE bu_partner.
    CLASS-METHODS ok_ktogr
      IMPORTING iv            TYPE clike
      RETURNING VALUE(rv)     TYPE abap_bool.
    " CVI customising: the grouping and the roles an account group creates.
    CLASS-METHODS bp_group
      IMPORTING iv_ktogr      TYPE clike
      RETURNING VALUE(rv)     TYPE bu_group.
    CLASS-METHODS bp_roles
      IMPORTING iv_ktogr      TYPE clike
      RETURNING VALUE(rt)     TYPE tt_role.
    CLASS-METHODS has_role
      IMPORTING VALUE(iv_bp)   TYPE bu_partner
                VALUE(iv_role) TYPE bu_role
      RETURNING VALUE(rv)      TYPE abap_bool.
    " Partner type of a partner function: KU, LI, PE, AP.
    CLASS-METHODS nrart
      IMPORTING VALUE(iv_parvw) TYPE parvw
      RETURNING VALUE(rv)       TYPE nrart.
ENDCLASS.


CLASS lcl_cfg IMPLEMENTATION.

  METHOD guid_of.
    IF gc_side = 'C'.
      SELECT SINGLE partner_guid FROM cvi_cust_link WHERE customer = @iv_num INTO @rv.
    ELSE.
      SELECT SINGLE partner_guid FROM cvi_vend_link WHERE vendor = @iv_num INTO @rv.
    ENDIF.
  ENDMETHOD.

  METHOD bp_of.
    DATA(lv_g) = guid_of( iv_num ).
    IF lv_g IS NOT INITIAL.
      SELECT SINGLE partner FROM but000 WHERE partner_guid = @lv_g INTO @rv.
    ENDIF.
  ENDMETHOD.

  METHOD nums_of_bp.
    IF it_bp IS INITIAL.
      RETURN.
    ENDIF.
    DATA lt_bp TYPE STANDARD TABLE OF bu_partner WITH EMPTY KEY.
    LOOP AT it_bp ASSIGNING FIELD-SYMBOL(<lv_bp>).
      APPEND CONV bu_partner( <lv_bp> ) TO lt_bp.
    ENDLOOP.
    SELECT partner_guid FROM but000
      FOR ALL ENTRIES IN @lt_bp WHERE partner = @lt_bp-table_line
      INTO TABLE @DATA(lt_g).
    IF lt_g IS INITIAL.
      RETURN.
    ENDIF.
    IF gc_side = 'C'.
      " A typed table, not @DATA( ): one column read into an inline table
      " gives a structured line, which a string template cannot take.
      DATA lt_c TYPE STANDARD TABLE OF kunnr WITH EMPTY KEY.
      SELECT customer FROM cvi_cust_link
        FOR ALL ENTRIES IN @lt_g WHERE partner_guid = @lt_g-partner_guid
        INTO TABLE @lt_c.
      LOOP AT lt_c INTO DATA(lv_cust).
        APPEND |{ lv_cust }| TO rt.
      ENDLOOP.
    ELSE.
      DATA lt_v TYPE STANDARD TABLE OF lifnr WITH EMPTY KEY.
      SELECT vendor FROM cvi_vend_link
        FOR ALL ENTRIES IN @lt_g WHERE partner_guid = @lt_g-partner_guid
        INTO TABLE @lt_v.
      LOOP AT lt_v INTO DATA(lv_vend).
        APPEND |{ lv_vend }| TO rt.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD num_by_guid.
    IF gc_side = 'C'.
      SELECT SINGLE customer FROM cvi_cust_link WHERE partner_guid = @iv_guid INTO @DATA(lv_c).
      rv = lv_c.
    ELSE.
      SELECT SINGLE vendor FROM cvi_vend_link WHERE partner_guid = @iv_guid INTO @DATA(lv_v).
      rv = lv_v.
    ENDIF.
  ENDMETHOD.

  METHOD bp_by_guid.
    SELECT SINGLE partner FROM but000 WHERE partner_guid = @iv_guid INTO @rv.
  ENDMETHOD.

  METHOD ok_ktogr.
    IF gc_side = 'C'.
      SELECT SINGLE @abap_true FROM t077d WHERE ktokd = @iv INTO @rv.
    ELSE.
      SELECT SINGLE @abap_true FROM t077k WHERE ktokk = @iv INTO @rv.
    ENDIF.
  ENDMETHOD.

  METHOD bp_group.
    IF gc_side = 'C'.
      SELECT SINGLE grouping FROM cvic_cust_to_bp1 WHERE account_group = @iv_ktogr INTO @rv.
    ELSE.
      SELECT SINGLE grouping FROM cvic_vend_to_bp1 WHERE account_group = @iv_ktogr INTO @rv.
    ENDIF.
  ENDMETHOD.

  METHOD bp_roles.
    IF gc_side = 'C'.
      SELECT role FROM cvic_cust_to_bp2 WHERE account_group = @iv_ktogr INTO TABLE @rt.
    ELSE.
      SELECT role FROM cvic_vend_to_bp2 WHERE account_group = @iv_ktogr INTO TABLE @rt.
    ENDIF.
  ENDMETHOD.

  METHOD has_role.
    IF iv_bp IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE @abap_true FROM but100
      WHERE partner = @iv_bp AND rltyp = @iv_role INTO @rv.
  ENDMETHOD.

  METHOD nrart.
    SELECT SINGLE nrart FROM tpar WHERE parvw = @iv_parvw INTO @rv.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_DB - what SAP holds today
*   Every record of every part asked for, read for all numbers at once
*   (in packages), and kept under the key the file uses: the parent's keys
*   followed by the part's own - a partner function of sales area
*   0801/Z1/Z0 is found under "0801|Z1|Z0|ZR|000".
*----------------------------------------------------------------------*
CLASS lcl_db DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_rec,
             grp TYPE ty_grp,
             num TYPE string,
             key TYPE string,
             rec TYPE REF TO data,
           END OF ty_rec,
           tt_rec TYPE SORTED TABLE OF ty_rec WITH NON-UNIQUE KEY grp num key.

    " IT_NUM: internal numbers (with their leading zeros).
    METHODS constructor
      IMPORTING it_num TYPE string_table
                it_grp TYPE string_table.
    METHODS records
      IMPORTING iv_grp    TYPE ty_grp
                iv_num    TYPE string
      RETURNING VALUE(rt) TYPE tt_rec.
    METHODS rec
      IMPORTING iv_grp    TYPE ty_grp
                iv_num    TYPE string
                iv_key    TYPE string
      RETURNING VALUE(rr) TYPE REF TO data.
    " The current value of a field, typed as the field. Unbound when the
    " record does not exist.
    METHODS value
      IMPORTING is_f      TYPE ty_fld
                iv_num    TYPE string
                iv_key    TYPE string
      RETURNING VALUE(rr) TYPE REF TO data.
    " The consecutive number of the communication entry a value lives in -
    " a change has to name it.
    METHODS consnumber
      IMPORTING is_f      TYPE ty_fld
                iv_num    TYPE string
      RETURNING VALUE(rv) TYPE ad_consnum.
    " The key a record is filed under, built from the record itself.
    CLASS-METHODS key_of
      IMPORTING iv_grp    TYPE ty_grp
                is_rec    TYPE any
      RETURNING VALUE(rv) TYPE string.
    " Text name (TDNAME) of a text object for a number and a key.
    CLASS-METHODS tdname
      IMPORTING iv_obj    TYPE tdobject
                iv_num    TYPE string
                iv_key    TYPE string
      RETURNING VALUE(rv) TYPE tdobname.
  PRIVATE SECTION.
    DATA mt_rec TYPE tt_rec.
    TYPES: BEGIN OF ty_txt,
             obj  TYPE tdobject,
             name TYPE tdobname,
             id   TYPE tdid,
             text TYPE string,
           END OF ty_txt.
    DATA mt_txt TYPE HASHED TABLE OF ty_txt WITH UNIQUE KEY obj name id.
    METHODS load_tab
      IMPORTING is_g   TYPE ty_gdef
                it_num TYPE string_table.
    METHODS load_addr
      IMPORTING it_num TYPE string_table.
    METHODS put
      IMPORTING iv_grp TYPE ty_grp
                iv_num TYPE string
                iv_key TYPE string
                ir_rec TYPE REF TO data.
    METHODS text
      IMPORTING iv_obj    TYPE tdobject
                iv_name   TYPE tdobname
                iv_id     TYPE tdid
      RETURNING VALUE(rv) TYPE string.
ENDCLASS.


CLASS lcl_db IMPLEMENTATION.

  METHOD constructor.
    IF it_num IS INITIAL.
      RETURN.
    ENDIF.
    LOOP AT lcl_cat=>groups( ) INTO DATA(ls_g).
      IF NOT line_exists( it_grp[ table_line = CONV string( ls_g-grp ) ] ).
        CONTINUE.
      ENDIF.
      CASE ls_g-grp.
        WHEN 'ADDR' OR 'COMM'.
          " Read together, below.
        WHEN 'TXTC' OR 'TXTB' OR 'TXTS'.
          " Read when asked for.
        WHEN OTHERS.
          load_tab( is_g = ls_g it_num = it_num ).
      ENDCASE.
    ENDLOOP.
    IF line_exists( it_grp[ table_line = `ADDR` ] ) OR line_exists( it_grp[ table_line = `COMM` ] ).
      load_addr( it_num ).
    ENDIF.
  ENDMETHOD.

  METHOD put.
    INSERT VALUE #( grp = iv_grp num = iv_num key = iv_key rec = ir_rec ) INTO TABLE mt_rec.
  ENDMETHOD.

  METHOD key_of.
    DATA(lt_k) = lcl_cat=>key_names( iv_grp ).
    DATA lv_n TYPE i.
    LOOP AT lt_k INTO DATA(lv_k).
      lv_n = lv_n + 1.
      DATA(lv_fld) = substring_after( val = lv_k sub = `-` ).
      ASSIGN COMPONENT lv_fld OF STRUCTURE is_rec TO FIELD-SYMBOL(<lv>).
      DATA(lv_v) = COND string( WHEN sy-subrc = 0 THEN condense( CONV string( <lv> ) ) ).
      rv = COND #( WHEN lv_n = 1 THEN lv_v ELSE |{ rv }\|{ lv_v }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD load_tab.
    " Read in packages of numbers: FOR ALL ENTRIES with a few thousand
    " entries is split by the database interface anyway, and a package
    " keeps one statement from holding too much in memory.
    CONSTANTS lc_pack TYPE i VALUE 500.
    DATA lr_tab TYPE REF TO data.
    FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.
    DATA lt_pack TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA(lv_cond) = |{ is_g-numfld } = @LT_PACK-TABLE_LINE|.

    DATA lv_from TYPE i.
    DATA lv_to   TYPE i.
    DATA(lv_i) = 0.
    WHILE lv_i < lines( it_num ).
      CLEAR lt_pack.
      lv_from = lv_i + 1.
      lv_to   = lv_i + lc_pack.
      LOOP AT it_num INTO DATA(lv_n) FROM lv_from TO lv_to.
        APPEND lv_n TO lt_pack.
      ENDLOOP.
      lv_i = lv_to.

      CREATE DATA lr_tab TYPE STANDARD TABLE OF (is_g-tab).
      ASSIGN lr_tab->* TO <lt_tab>.
      TRY.
          SELECT * FROM (is_g-tab)
            FOR ALL ENTRIES IN @lt_pack
            WHERE (lv_cond)
            INTO TABLE @<lt_tab>.
        CATCH cx_sy_dynamic_osql_error.
          RETURN.
      ENDTRY.
      LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_r>).
        ASSIGN COMPONENT is_g-numfld OF STRUCTURE <ls_r> TO FIELD-SYMBOL(<lv_num>).
        IF sy-subrc <> 0 OR <lv_num> IS INITIAL.
          CONTINUE.
        ENDIF.
        DATA lr_r TYPE REF TO data.
        CREATE DATA lr_r LIKE <ls_r>.
        ASSIGN lr_r->* TO FIELD-SYMBOL(<ls_copy>).
        <ls_copy> = <ls_r>.
        put( iv_grp = is_g-grp iv_num = CONV string( <lv_num> )
             iv_key = key_of( iv_grp = is_g-grp is_rec = <ls_r> ) ir_rec = lr_r ).
      ENDLOOP.
    ENDWHILE.
  ENDMETHOD.

  METHOD load_addr.
    " The address number comes from the general data. Read it here when the
    " general data itself was not asked for.
    TYPES: BEGIN OF ty_an,
             num   TYPE string,
             adrnr TYPE ad_addrnum,
           END OF ty_an.
    DATA lt_an TYPE STANDARD TABLE OF ty_an WITH EMPTY KEY.
    DATA lt_nr TYPE STANDARD TABLE OF ad_addrnum WITH EMPTY KEY.
    DATA lt_k  TYPE STANDARD TABLE OF kunnr WITH EMPTY KEY.
    LOOP AT it_num INTO DATA(lv_n).
      APPEND CONV kunnr( lv_n ) TO lt_k.
    ENDLOOP.
    IF gc_side = 'C'.
      SELECT kunnr AS num, adrnr FROM kna1
        FOR ALL ENTRIES IN @lt_k WHERE kunnr = @lt_k-table_line
        INTO CORRESPONDING FIELDS OF TABLE @lt_an.
    ELSE.
      SELECT lifnr AS num, adrnr FROM lfa1
        FOR ALL ENTRIES IN @lt_k WHERE lifnr = @lt_k-table_line
        INTO CORRESPONDING FIELDS OF TABLE @lt_an.
    ENDIF.
    DELETE lt_an WHERE adrnr IS INITIAL.
    IF lt_an IS INITIAL.
      RETURN.
    ENDIF.
    LOOP AT lt_an INTO DATA(ls_an).
      APPEND ls_an-adrnr TO lt_nr.
    ENDLOOP.

    SELECT * FROM adrc FOR ALL ENTRIES IN @lt_nr
      WHERE addrnumber = @lt_nr-table_line AND nation = @space
      INTO TABLE @DATA(lt_adrc).
    SELECT * FROM adr2 FOR ALL ENTRIES IN @lt_nr
      WHERE addrnumber = @lt_nr-table_line AND persnumber = @space
      INTO TABLE @DATA(lt_adr2).
    SELECT * FROM adr3 FOR ALL ENTRIES IN @lt_nr
      WHERE addrnumber = @lt_nr-table_line AND persnumber = @space
      INTO TABLE @DATA(lt_adr3).
    SELECT * FROM adr6 FOR ALL ENTRIES IN @lt_nr
      WHERE addrnumber = @lt_nr-table_line AND persnumber = @space
      INTO TABLE @DATA(lt_adr6).
    " The standard entry first, so the first one found is the one shown.
    SORT lt_adr2 BY addrnumber flgdefault DESCENDING consnumber.
    SORT lt_adr3 BY addrnumber flgdefault DESCENDING consnumber.
    SORT lt_adr6 BY addrnumber flgdefault DESCENDING consnumber.

    LOOP AT lt_an INTO ls_an.
      " The address valid today.
      LOOP AT lt_adrc INTO DATA(ls_adrc) WHERE addrnumber = ls_an-adrnr
                                           AND date_from <= sy-datum
                                           AND date_to   >= sy-datum.
        put( iv_grp = 'ADDR' iv_num = ls_an-num iv_key = `` ir_rec = NEW adrc( ls_adrc ) ).
        EXIT.
      ENDLOOP.
      " Landline: R_3_USER space or 1. Mobile: 2 or 3.
      LOOP AT lt_adr2 INTO DATA(ls_2) WHERE addrnumber = ls_an-adrnr AND ( r3_user = space OR r3_user = '1' ).
        put( iv_grp = 'COMM' iv_num = ls_an-num iv_key = `ADR2` ir_rec = NEW adr2( ls_2 ) ).
        EXIT.
      ENDLOOP.
      LOOP AT lt_adr2 INTO ls_2 WHERE addrnumber = ls_an-adrnr AND ( r3_user = '2' OR r3_user = '3' ).
        put( iv_grp = 'COMM' iv_num = ls_an-num iv_key = `MOB` ir_rec = NEW adr2( ls_2 ) ).
        EXIT.
      ENDLOOP.
      LOOP AT lt_adr3 INTO DATA(ls_3) WHERE addrnumber = ls_an-adrnr.
        put( iv_grp = 'COMM' iv_num = ls_an-num iv_key = `ADR3` ir_rec = NEW adr3( ls_3 ) ).
        EXIT.
      ENDLOOP.
      LOOP AT lt_adr6 INTO DATA(ls_6) WHERE addrnumber = ls_an-adrnr.
        put( iv_grp = 'COMM' iv_num = ls_an-num iv_key = `ADR6` ir_rec = NEW adr6( ls_6 ) ).
        EXIT.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD records.
    LOOP AT mt_rec INTO DATA(ls_r) WHERE grp = iv_grp AND num = iv_num.
      INSERT ls_r INTO TABLE rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD rec.
    READ TABLE mt_rec INTO DATA(ls_r) WITH KEY grp = iv_grp num = iv_num key = iv_key.
    IF sy-subrc = 0.
      rr = ls_r-rec.
    ENDIF.
  ENDMETHOD.

  METHOD tdname.
    " KNA1: the number. KNB1: number + company code. KNVV: number + sales
    " area. The same for the supplier objects.
    DATA(lv_num) = iv_num.
    IF strlen( lv_num ) < 10.
      lv_num = |{ lv_num WIDTH = 10 }|.
    ENDIF.
    DATA(lv_key) = iv_key.
    REPLACE ALL OCCURRENCES OF `|` IN lv_key WITH ``.
    CASE iv_obj.
      WHEN 'KNA1' OR 'LFA1'.
        rv = lv_num.
      WHEN OTHERS.
        rv = |{ lv_num }{ lv_key }|.
    ENDCASE.
  ENDMETHOD.

  METHOD text.
    READ TABLE mt_txt INTO DATA(ls_t) WITH TABLE KEY obj = iv_obj name = iv_name id = iv_id.
    IF sy-subrc = 0.
      rv = ls_t-text.
      RETURN.
    ENDIF.
    DATA lt_line TYPE STANDARD TABLE OF tline WITH EMPTY KEY.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING  id       = iv_id
                 language = p_tlang
                 name     = iv_name
                 object   = iv_obj
      TABLES     lines    = lt_line
      EXCEPTIONS OTHERS   = 1.
    IF sy-subrc = 0.
      LOOP AT lt_line INTO DATA(ls_l).
        rv = COND #( WHEN sy-tabix = 1 THEN CONV string( ls_l-tdline )
                     ELSE |{ rv }{ cl_abap_char_utilities=>newline }{ ls_l-tdline }| ).
      ENDLOOP.
    ENDIF.
    INSERT VALUE #( obj = iv_obj name = iv_name id = iv_id text = rv ) INTO TABLE mt_txt.
  ENDMETHOD.

  METHOD value.
    IF is_f-grp = 'TXTC' OR is_f-grp = 'TXTB' OR is_f-grp = 'TXTS'.
      rr = NEW string( text( iv_obj  = is_f-tdobj
                             iv_name = tdname( iv_obj = is_f-tdobj iv_num = iv_num iv_key = iv_key )
                             iv_id   = is_f-tdid ) ).
      RETURN.
    ENDIF.

    DATA lr_rec TYPE REF TO data.
    DATA lv_fld TYPE fieldname.
    lv_fld = is_f-fld.
    CASE is_f-grp.
      WHEN 'COMM'.
        DATA(lv_ck) = COND string( WHEN is_f-api = 'MOBILE' THEN `MOB` ELSE CONV string( is_f-tab ) ).
        lr_rec = rec( iv_grp = 'COMM' iv_num = iv_num iv_key = lv_ck ).
      WHEN OTHERS.
        lr_rec = rec( iv_grp = is_f-grp iv_num = iv_num iv_key = iv_key ).
    ENDCASE.
    IF lr_rec IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN lr_rec->* TO FIELD-SYMBOL(<ls_rec>).

    rr = lcl_val=>new_data( is_f ).
    ASSIGN rr->* TO FIELD-SYMBOL(<lv_out>).

    " The customer's partner function holds its partner in the field its
    " partner type uses.
    IF is_f-name = `KNVP-PARTNER`.
      DATA(lt_pf) = VALUE string_table( ( `KUNN2` ) ( `LIFNR` ) ( `PERNR` ) ( `PARNR` ) ).
      LOOP AT lt_pf INTO DATA(lv_pf).
        ASSIGN COMPONENT lv_pf OF STRUCTURE <ls_rec> TO FIELD-SYMBOL(<lv_pv>).
        IF sy-subrc = 0 AND <lv_pv> IS NOT INITIAL.
          <lv_out> = <lv_pv>.
          RETURN.
        ENDIF.
      ENDLOOP.
      RETURN.
    ENDIF.

    ASSIGN COMPONENT lv_fld OF STRUCTURE <ls_rec> TO FIELD-SYMBOL(<lv_in>).
    IF sy-subrc = 0.
      <lv_out> = <lv_in>.
    ENDIF.
  ENDMETHOD.

  METHOD consnumber.
    DATA(lv_ck) = COND string( WHEN is_f-api = 'MOBILE' THEN `MOB` ELSE CONV string( is_f-tab ) ).
    DATA(lr_rec) = rec( iv_grp = 'COMM' iv_num = iv_num iv_key = lv_ck ).
    IF lr_rec IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN lr_rec->* TO FIELD-SYMBOL(<ls_rec>).
    ASSIGN COMPONENT 'CONSNUMBER' OF STRUCTURE <ls_rec> TO FIELD-SYMBOL(<lv_c>).
    IF sy-subrc = 0.
      rv = <lv_c>.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* LCL_FILE - the file on the PC: .xlsx, or text separated by tabs
*----------------------------------------------------------------------*
CLASS lcl_file DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS f4.
    CLASS-METHODS write
      IMPORTING it_head TYPE tt_cell
                it_rows TYPE tt_row
      RAISING   lcx_dl.
    CLASS-METHODS read
      EXPORTING et_head TYPE tt_cell
                et_rows TYPE tt_row
      RAISING   lcx_dl.
  PRIVATE SECTION.
    CLASS-METHODS save_bin
      IMPORTING iv_x TYPE xstring
      RAISING   lcx_dl.
    CLASS-METHODS load_bin
      RETURNING VALUE(rv) TYPE xstring
      RAISING   lcx_dl.
    CLASS-METHODS decode
      IMPORTING iv_x      TYPE xstring
      RETURNING VALUE(rv) TYPE string
      RAISING   lcx_dl.
ENDCLASS.


CLASS lcl_file IMPLEMENTATION.

  METHOD f4.
    DATA(lv_filter) = COND string( WHEN p_txt = abap_true
                                   THEN `Text, tab-separated (*.txt)|*.txt|`
                                   ELSE `Excel workbook (*.xlsx)|*.xlsx|` ).
    IF p_up = abap_true.
      DATA lt_ft TYPE filetable.
      DATA lv_rc TYPE i.
      DATA lv_ua TYPE i.
      cl_gui_frontend_services=>file_open_dialog(
        EXPORTING  window_title = 'Select the file to upload'
                   file_filter  = lv_filter
        CHANGING   file_table   = lt_ft
                   rc           = lv_rc
                   user_action  = lv_ua
        EXCEPTIONS OTHERS       = 1 ).
      IF sy-subrc = 0 AND lv_ua = cl_gui_frontend_services=>action_ok AND lv_rc >= 1.
        DATA(lv_pick) = lt_ft[ 1 ]-filename.
        IF strlen( lv_pick ) > 255.
          MESSAGE 'That path is longer than 255 characters - move the file to a shorter path'
                  TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          p_file = lv_pick.
        ENDIF.
      ENDIF.
      RETURN.
    ENDIF.

    DATA: lv_name TYPE string,
          lv_path TYPE string,
          lv_full TYPE string.
    DATA(lv_ext) = COND string( WHEN p_txt = abap_true THEN `txt` ELSE `xlsx` ).
    DATA(lv_def) = |CUST_BP_MAINT_{ sy-datum }.{ lv_ext }|.
    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING  window_title      = 'Save the file'
                 default_extension = lv_ext
                 default_file_name = lv_def
                 file_filter       = lv_filter
      CHANGING   filename          = lv_name
                 path              = lv_path
                 fullpath          = lv_full
      EXCEPTIONS OTHERS            = 1 ).
    IF sy-subrc = 0 AND lv_full IS NOT INITIAL.
      IF strlen( lv_full ) > 255.
        MESSAGE 'That path is longer than 255 characters - pick a shorter folder'
                TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        p_file = lv_full.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD save_bin.
    DATA(lt_bin) = cl_bcs_convert=>xstring_to_solix( iv_x ).
    DATA(lv_len) = xstrlen( iv_x ).
    DATA(lv_file) = CONV string( p_file ).
    cl_gui_frontend_services=>gui_download(
      EXPORTING  bin_filesize = lv_len
                 filename     = lv_file
                 filetype     = 'BIN'
      CHANGING   data_tab     = lt_bin
      EXCEPTIONS OTHERS       = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW lcx_dl( |The file { p_file } could not be written - is it open in Excel?| ).
    ENDIF.
  ENDMETHOD.

  METHOD load_bin.
    DATA lt_bin TYPE solix_tab.
    DATA lv_len TYPE i.
    DATA(lv_file) = CONV string( p_file ).
    cl_gui_frontend_services=>gui_upload(
      EXPORTING  filename   = lv_file
                 filetype   = 'BIN'
      IMPORTING  filelength = lv_len
      CHANGING   data_tab   = lt_bin
      EXCEPTIONS OTHERS     = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW lcx_dl( |The file { p_file } could not be read| ).
    ENDIF.
    rv = cl_bcs_convert=>solix_to_xstring( it_solix = lt_bin iv_size = lv_len ).
  ENDMETHOD.

  METHOD decode.
    " A byte order mark says how the text is encoded. Without one, UTF-8 is
    " tried first and then Shift-JIS, the usual encoding of a Japanese
    " Windows text file.
    DATA lv_cp TYPE string.
    DATA lv_x  TYPE xstring.
    lv_x = iv_x.
    IF xstrlen( lv_x ) >= 3 AND lv_x(3) = 'EFBBBF'.
      lv_cp = `UTF-8`.
      lv_x = lv_x+3.
    ELSEIF xstrlen( lv_x ) >= 2 AND lv_x(2) = 'FFFE'.
      lv_cp = `UTF-16LE`.
      lv_x = lv_x+2.
    ELSEIF xstrlen( lv_x ) >= 2 AND lv_x(2) = 'FEFF'.
      lv_cp = `UTF-16BE`.
      lv_x = lv_x+2.
    ENDIF.
    IF lv_cp IS NOT INITIAL.
      TRY.
          rv = cl_abap_conv_codepage=>create_in( codepage = lv_cp )->convert( lv_x ).
          RETURN.
        CATCH cx_root INTO DATA(lx1).
          RAISE EXCEPTION NEW lcx_dl( |The file could not be read as { lv_cp }: { lx1->get_text( ) }| ).
      ENDTRY.
    ENDIF.
    TRY.
        rv = cl_abap_conv_codepage=>create_in( codepage = `UTF-8` )->convert( lv_x ).
      CATCH cx_root.
        TRY.
            rv = cl_abap_conv_codepage=>create_in( codepage = `Shift_JIS` )->convert( lv_x ).
          CATCH cx_root INTO DATA(lx2).
            RAISE EXCEPTION NEW lcx_dl( |The file's encoding is not recognised - save it as | &&
                                        |"Unicode Text" or UTF-8 ({ lx2->get_text( ) })| ).
        ENDTRY.
    ENDTRY.
  ENDMETHOD.

  METHOD write.
    IF p_xlsx = abap_true.
      save_bin( lcl_xlsx=>build( iv_sheet = 'Data' it_head = it_head it_row = it_rows ) ).
      RETURN.
    ENDIF.
    " Text: one line per row, cells separated by tabs. A line break inside
    " a cell - a text of several lines - is written as \n.
    DATA(lv_tab) = cl_abap_char_utilities=>horizontal_tab.
    DATA(lv_nl)  = cl_abap_char_utilities=>cr_lf.
    DATA(lv_out) = concat_lines_of( table = it_head sep = lv_tab ) && lv_nl.
    LOOP AT it_rows INTO DATA(ls_r).
      DATA lt_c TYPE tt_cell.
      lt_c = ls_r-cells.
      LOOP AT lt_c ASSIGNING FIELD-SYMBOL(<lv_c>).
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN <lv_c> WITH `\n`.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN <lv_c> WITH `\n`.
        REPLACE ALL OCCURRENCES OF lv_tab IN <lv_c> WITH ` `.
      ENDLOOP.
      lv_out = lv_out && concat_lines_of( table = lt_c sep = lv_tab ) && lv_nl.
    ENDLOOP.
    DATA lv_x TYPE xstring.
    TRY.
        lv_x = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( lv_out ).
      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION NEW lcx_dl( |The text file could not be encoded: { lx->get_text( ) }| ).
    ENDTRY.
    " The byte order mark makes Excel open the file as UTF-8.
    DATA lv_bom TYPE xstring VALUE 'EFBBBF'.
    CONCATENATE lv_bom lv_x INTO lv_x IN BYTE MODE.
    save_bin( lv_x ).
  ENDMETHOD.

  METHOD read.
    CLEAR: et_head, et_rows.
    DATA(lv_low) = to_lower( CONV string( p_file ) ).
    IF lv_low CP '*.xlsx'.
      DATA(lo_x) = NEW lcl_excel( ).
      DATA lv_sheet TYPE string.
      lo_x->read( EXPORTING iv_file    = CONV string( p_file )
                            iv_from_pc = abap_true
                            iv_sheet   = `Data`
                            iv_skip    = 1
                  IMPORTING et_head    = et_head
                            et_row     = et_rows
                            ev_sheet   = lv_sheet ).
      RETURN.
    ENDIF.

    DATA(lv_txt) = decode( load_bin( ) ).
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_txt WITH cl_abap_char_utilities=>newline.
    SPLIT lv_txt AT cl_abap_char_utilities=>newline INTO TABLE DATA(lt_lines).
    LOOP AT lt_lines INTO DATA(lv_line).
      DATA(lv_no) = sy-tabix.
      SPLIT lv_line AT cl_abap_char_utilities=>horizontal_tab INTO TABLE DATA(lt_c).
      LOOP AT lt_c ASSIGNING FIELD-SYMBOL(<lv_c>).
        REPLACE ALL OCCURRENCES OF `\n` IN <lv_c> WITH cl_abap_char_utilities=>newline.
      ENDLOOP.
      IF lv_no = 1.
        et_head = lt_c.
        CONTINUE.
      ENDIF.
      " IS INITIAL takes a data object, not a built-in function.
      DATA(lv_trim) = condense( lv_line ).
      IF lv_trim IS INITIAL.
        CONTINUE.
      ENDIF.
      APPEND VALUE #( row = lv_no cells = lt_c ) TO et_rows.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* LCL_LOG - the result list
*----------------------------------------------------------------------*
CLASS lcl_log DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS add
      IMPORTING iv_row   TYPE i DEFAULT 0
                iv_obj   TYPE clike OPTIONAL
                iv_part  TYPE clike OPTIONAL
                iv_field TYPE clike OPTIONAL
                iv_old   TYPE clike OPTIONAL
                iv_new   TYPE clike OPTIONAL
                iv_type  TYPE ty_msgty
                iv_msg   TYPE clike.
    METHODS show
      IMPORTING iv_title TYPE clike.
  PRIVATE SECTION.
    DATA mt_log TYPE tt_log.
ENDCLASS.


CLASS lcl_log IMPLEMENTATION.

  METHOD add.
    DATA(lv_icon) = SWITCH icon_d( iv_type
                                   WHEN 'E' THEN icon_red_light
                                   WHEN 'W' THEN icon_yellow_light
                                   WHEN 'S' THEN icon_green_light
                                   ELSE icon_information ).
    APPEND VALUE #( status = lv_icon row = iv_row obj = iv_obj part = iv_part field = iv_field
                    old = iv_old new = iv_new type = iv_type msg = iv_msg ) TO mt_log.
  ENDMETHOD.

  METHOD show.
    IF mt_log IS INITIAL.
      MESSAGE 'Nothing to report' TYPE 'S'.
      RETURN.
    ENDIF.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                                CHANGING  t_table      = mt_log ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->get_columns( )->set_optimize( abap_true ).
        DATA(lo_cols) = lo_alv->get_columns( ).
        DATA(lt_txt) = VALUE string_table( ( `STATUS` ) ( `Status` ) ( `ROW` ) ( `Row` )
                                           ( `OBJ` ) ( `Number` ) ( `PART` ) ( `Part` )
                                           ( `FIELD` ) ( `Field` ) ( `OLD` ) ( `Value in SAP` )
                                           ( `NEW` ) ( `Value in file` ) ( `TYPE` ) ( `Type` )
                                           ( `MSG` ) ( `Message` ) ).
        DATA lv_i TYPE i VALUE 1.
        WHILE lv_i < lines( lt_txt ).
          TRY.
              DATA(lo_c) = lo_cols->get_column( CONV lvc_fname( lt_txt[ lv_i ] ) ).
              DATA(lv_h) = lt_txt[ lv_i + 1 ].
              lo_c->set_long_text( CONV scrtext_l( lv_h ) ).
              lo_c->set_medium_text( CONV scrtext_m( lv_h ) ).
              lo_c->set_short_text( CONV scrtext_s( lv_h ) ).
            CATCH cx_salv_not_found.
              CLEAR lv_h.
          ENDTRY.
          lv_i = lv_i + 2.
        ENDWHILE.
        lo_alv->get_display_settings( )->set_list_header( CONV lvc_title( iv_title ) ).
        lo_alv->display( ).
      CATCH cx_salv_msg INTO DATA(lx).
        MESSAGE lx TYPE 'I'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_SEL - which fields go into the file
*   The user ticks fields in a list; the keys and the fields the system
*   needs are added. The choice lives in S_FLD, so a screen variant keeps
*   it.
*----------------------------------------------------------------------*
CLASS lcl_sel DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS choose.
    CLASS-METHODS chosen RETURNING VALUE(rt) TYPE string_table.
    " The complete list of columns, in file order: the number, the keys,
    " ACTION where a repeating part is chosen, then the data fields.
    " EV_ERR when the choice cannot go into one file.
    CLASS-METHODS complete
      IMPORTING it_names  TYPE string_table
                iv_create TYPE abap_bool
      EXPORTING et_fld    TYPE tt_fld
                ev_action TYPE abap_bool
                ev_err    TYPE string.
    " The one repeating part among these fields, if there is one.
    CLASS-METHODS rep_of
      IMPORTING it_fld    TYPE tt_fld
      RETURNING VALUE(rv) TYPE ty_grp.
    " Parts that may stand in a file next to a repeating part.
    CLASS-METHODS allowed_with
      IMPORTING iv_rep    TYPE ty_grp
      RETURNING VALUE(rt) TYPE string_table.
ENDCLASS.


CLASS lcl_sel IMPLEMENTATION.

  METHOD chosen.
    LOOP AT s_fld INTO DATA(ls_s) WHERE sign = 'I' AND option = 'EQ'.
      APPEND condense( CONV string( ls_s-low ) ) TO rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD choose.
    TYPES: BEGIN OF ty_pick,
             sel  TYPE c LENGTH 1,
             part TYPE c LENGTH 30,
             name TYPE c LENGTH 60,
             text TYPE c LENGTH 70,
           END OF ty_pick.
    DATA lt_pick TYPE STANDARD TABLE OF ty_pick WITH EMPTY KEY.
    DATA(lt_now) = chosen( ).
    DATA(lt_all) = lcl_cat=>all( ).
    LOOP AT lt_all INTO DATA(ls_f) WHERE key = abap_false.
      APPEND VALUE #( sel  = COND #( WHEN line_exists( lt_now[ table_line = ls_f-name ] ) THEN 'X' )
                      part = lcl_cat=>grp( ls_f-grp )-text
                      name = ls_f-name
                      text = ls_f-text ) TO lt_pick.
    ENDLOOP.

    DATA lt_fcat TYPE slis_t_fieldcat_alv.
    lt_fcat = VALUE #( ( fieldname = 'PART' tabname = '1' seltext_l = 'Part'        outputlen = 26 )
                       ( fieldname = 'NAME' tabname = '1' seltext_l = 'Field'       outputlen = 34 )
                       ( fieldname = 'TEXT' tabname = '1' seltext_l = 'Description' outputlen = 50 ) ).
    DATA lv_exit TYPE c LENGTH 1.
    CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
      EXPORTING  i_title              = 'Choose the fields for the file'
                 i_selection          = 'X'
                 i_zebra              = 'X'
                 i_checkbox_fieldname = 'SEL'
                 i_tabname            = '1'
                 it_fieldcat          = lt_fcat
                 i_screen_start_column = 5
                 i_screen_start_line   = 3
                 i_screen_end_column   = 125
                 i_screen_end_line     = 25
      IMPORTING  e_exit               = lv_exit
      TABLES     t_outtab             = lt_pick
      EXCEPTIONS OTHERS               = 1.
    IF sy-subrc <> 0 OR lv_exit = 'X'.
      RETURN.
    ENDIF.
    CLEAR s_fld[].
    LOOP AT lt_pick INTO DATA(ls_p) WHERE sel = 'X'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_p-name ) TO s_fld.
    ENDLOOP.
  ENDMETHOD.

  METHOD rep_of.
    LOOP AT it_fld INTO DATA(ls_f).
      IF lcl_cat=>grp( ls_f-grp )-rep = abap_true.
        rv = ls_f-grp.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD allowed_with.
    rt = VALUE #( ( `CENT` ) ( `ADDR` ) ( `COMM` ) ( `TXTC` ) ( CONV string( iv_rep ) ) ).
    DATA(lv_p) = lcl_cat=>grp( iv_rep )-parent.
    IF lv_p IS NOT INITIAL.
      APPEND CONV string( lv_p ) TO rt.
      " the parent's texts
      LOOP AT lcl_cat=>groups( ) INTO DATA(ls_g) WHERE parent = lv_p AND rep = abap_false.
        APPEND CONV string( ls_g-grp ) TO rt.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD complete.
    CLEAR: et_fld, ev_action, ev_err.
    DATA(lt_all) = lcl_cat=>all( ).
    DATA lt_pick TYPE tt_fld.
    LOOP AT it_names INTO DATA(lv_n).
      READ TABLE lt_all INTO DATA(ls_f) WITH KEY name = lv_n.
      IF sy-subrc = 0.
        APPEND ls_f TO lt_pick.
      ENDIF.
    ENDLOOP.

    " ---- at most one repeating part, and only what belongs next to it
    DATA lt_rep TYPE SORTED TABLE OF ty_grp WITH UNIQUE KEY table_line.
    LOOP AT lt_pick INTO ls_f.
      IF lcl_cat=>grp( ls_f-grp )-rep = abap_true.
        INSERT ls_f-grp INTO TABLE lt_rep.
      ENDIF.
    ENDLOOP.
    IF lines( lt_rep ) > 1.
      DATA lv_r TYPE string.
      LOOP AT lt_rep INTO DATA(lv_g).
        lv_r = COND #( WHEN lv_r IS INITIAL THEN lcl_cat=>grp( lv_g )-text
                       ELSE |{ lv_r }, { lcl_cat=>grp( lv_g )-text }| ).
      ENDLOOP.
      ev_err = |Fields of one repeating part per file only - this choice has { lv_r }|.
      RETURN.
    ENDIF.
    DATA(lv_rep) = VALUE ty_grp( lt_rep[ 1 ] OPTIONAL ).
    IF lv_rep IS NOT INITIAL.
      DATA(lt_ok) = allowed_with( lv_rep ).
      LOOP AT lt_pick INTO ls_f.
        IF NOT line_exists( lt_ok[ table_line = CONV string( ls_f-grp ) ] ).
          ev_err = |{ lcl_cat=>grp( lv_rep )-text } can only be combined with general data | &&
                   |and { lcl_cat=>grp( lcl_cat=>root_of( lv_rep ) )-text } - remove { ls_f-name }|.
          RETURN.
        ENDIF.
      ENDLOOP.
      ev_action = abap_true.
    ENDIF.

    " ---- the columns the system needs
    DATA lt_need TYPE string_table.
    APPEND lcl_cat=>num_name( ) TO lt_need.
    DATA lt_grp TYPE SORTED TABLE OF ty_grp WITH UNIQUE KEY table_line.
    LOOP AT lt_pick INTO ls_f.
      INSERT ls_f-grp INTO TABLE lt_grp.
    ENDLOOP.
    LOOP AT lt_grp INTO lv_g.
      APPEND LINES OF lcl_cat=>key_names( lv_g ) TO lt_need.
      " A text of a company code or a sales area needs that key too.
      DATA(lv_par) = lcl_cat=>grp( lv_g )-parent.
      IF lv_par IS NOT INITIAL.
        APPEND LINES OF lcl_cat=>key_names( lv_par ) TO lt_need.
      ENDIF.
    ENDLOOP.
    IF iv_create = abap_true.
      APPEND LINES OF VALUE string_table( ( `ADRC-NAME1` ) ( `ADRC-SORT1` )
                                          ( `ADRC-COUNTRY` ) ( `ADRC-LANGU` ) ) TO lt_need.
      DATA(lv_b) = COND string( WHEN gc_side = 'C' THEN `KNB1` ELSE `LFB1` ).
      IF line_exists( lt_grp[ table_line = 'COMP' ] ) OR line_exists( lt_grp[ table_line = 'DUNN' ] )
         OR line_exists( lt_grp[ table_line = 'WTAX' ] ) OR line_exists( lt_grp[ table_line = 'TXTB' ] ).
        APPEND |{ lv_b }-AKONT| TO lt_need.
      ENDIF.
      IF line_exists( lt_grp[ table_line = 'SALE' ] ) OR line_exists( lt_grp[ table_line = 'PURC' ] )
         OR line_exists( lt_grp[ table_line = 'PART' ] ) OR line_exists( lt_grp[ table_line = 'TXTS' ] ).
        APPEND COND string( WHEN gc_side = 'C' THEN `KNVV-WAERS` ELSE `LFM1-WAERS` ) TO lt_need.
      ENDIF.
    ENDIF.

    " ---- the number, then keys in part order, then the data fields
    DATA lt_names TYPE string_table.
    LOOP AT lt_need INTO lv_n.
      IF NOT line_exists( lt_names[ table_line = lv_n ] ) AND line_exists( lt_all[ name = lv_n ] )
         AND lt_all[ name = lv_n ]-key = abap_true.
        APPEND lv_n TO lt_names.
      ENDIF.
    ENDLOOP.
    LOOP AT lt_need INTO lv_n.
      IF NOT line_exists( lt_names[ table_line = lv_n ] ) AND line_exists( lt_all[ name = lv_n ] ).
        APPEND lv_n TO lt_names.
      ENDIF.
    ENDLOOP.
    LOOP AT lt_all INTO ls_f.
      IF line_exists( lt_pick[ name = ls_f-name ] ) AND NOT line_exists( lt_names[ table_line = ls_f-name ] ).
        APPEND ls_f-name TO lt_names.
      ENDIF.
    ENDLOOP.
    LOOP AT lt_names INTO lv_n.
      READ TABLE lt_all INTO ls_f WITH KEY name = lv_n.
      IF line_exists( lt_need[ table_line = lv_n ] ).
        ls_f-must = abap_true.
      ENDIF.
      APPEND ls_f TO et_fld.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* LCL_DL - the download
*----------------------------------------------------------------------*
CLASS lcl_dl DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS run.
  PRIVATE SECTION.
    CLASS-METHODS numbers RETURNING VALUE(rt) TYPE string_table.
    " The first N parts of a key: "0801|Z1|Z0|ZR|000", 3 -> "0801|Z1|Z0".
    CLASS-METHODS prefix
      IMPORTING iv_key    TYPE string
                iv_n      TYPE i
      RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS in_scope
      IMPORTING iv_grp    TYPE ty_grp
                iv_key    TYPE string
      RETURNING VALUE(rv) TYPE abap_bool.
ENDCLASS.


CLASS lcl_dl IMPLEMENTATION.

  METHOD numbers.
    DATA lt_k TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    IF s_num[] IS NOT INITIAL.
      IF gc_side = 'C'.
        " Typed tables: one column read into an inline table gives a
        " structured line, which a string template cannot take.
        DATA lt_c TYPE STANDARD TABLE OF kunnr WITH EMPTY KEY.
        SELECT kunnr FROM kna1 WHERE kunnr IN @s_num ORDER BY kunnr
          INTO TABLE @lt_c UP TO @p_max ROWS.
        LOOP AT lt_c INTO DATA(lv_cust).
          INSERT |{ lv_cust }| INTO TABLE lt_k.
        ENDLOOP.
      ELSE.
        DATA lt_v TYPE STANDARD TABLE OF lifnr WITH EMPTY KEY.
        SELECT lifnr FROM lfa1 WHERE lifnr IN @s_num ORDER BY lifnr
          INTO TABLE @lt_v UP TO @p_max ROWS.
        LOOP AT lt_v INTO DATA(lv_vend).
          INSERT |{ lv_vend }| INTO TABLE lt_k.
        ENDLOOP.
      ENDIF.
    ENDIF.
    IF s_bp[] IS NOT INITIAL.
      DATA lt_bp TYPE STANDARD TABLE OF bu_partner WITH EMPTY KEY.
      SELECT partner FROM but000 WHERE partner IN @s_bp
        INTO TABLE @lt_bp UP TO @p_max ROWS.
      DATA(lt_n) = lcl_cfg=>nums_of_bp( lt_bp ).
      LOOP AT lt_n INTO DATA(lv_n).
        INSERT lv_n INTO TABLE lt_k.
      ENDLOOP.
    ENDIF.
    LOOP AT lt_k INTO DATA(lv_k).
      IF lines( rt ) >= p_max.
        EXIT.
      ENDIF.
      APPEND lv_k TO rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD prefix.
    SPLIT iv_key AT `|` INTO TABLE DATA(lt_p).
    LOOP AT lt_p INTO DATA(lv_p) TO iv_n.
      rv = COND #( WHEN sy-tabix = 1 THEN lv_p ELSE |{ rv }\|{ lv_p }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD in_scope.
    " The company code and sales area / purchasing organisation ranges on
    " the screen limit which entries are written.
    DATA(lv_root) = lcl_cat=>root_of( iv_grp ).
    SPLIT iv_key AT `|` INTO TABLE DATA(lt_p).
    " The first key part, moved into a field of the range's own type. Not
    " VALUE bukrs( lt_p[ 1 ] OPTIONAL ): the lines of LT_P are strings, and
    " a VALUE with a table expression takes only a compatible type.
    DATA: lv_first TYPE string,
          lv_bukrs TYPE bukrs,
          lv_org   TYPE vkorg.
    READ TABLE lt_p INTO lv_first INDEX 1.
    rv = abap_true.
    CASE lv_root.
      WHEN 'COMP'.
        IF s_bukrs[] IS NOT INITIAL.
          lv_bukrs = lv_first.
          rv = xsdbool( lv_bukrs IN s_bukrs ).
        ENDIF.
      WHEN 'SALE' OR 'PURC'.
        IF s_org[] IS NOT INITIAL.
          lv_org = lv_first.
          rv = xsdbool( lv_org IN s_org ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD run.
    DATA lt_fld TYPE tt_fld.
    DATA lv_act TYPE abap_bool.
    DATA lv_err TYPE string.
    lcl_sel=>complete( EXPORTING it_names  = lcl_sel=>chosen( )
                                 iv_create = p_crt
                       IMPORTING et_fld    = lt_fld
                                 ev_action = lv_act
                                 ev_err    = lv_err ).
    IF lv_err IS NOT INITIAL.
      MESSAGE lv_err TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " ---- headings
    DATA lt_head TYPE tt_cell.
    DATA(lv_num) = lcl_cat=>num_name( ).
    DATA(lv_rep) = lcl_sel=>rep_of( lt_fld ).
    " ACTION follows the last key column.
    DATA lv_last_key TYPE i.
    LOOP AT lt_fld INTO DATA(ls_f) WHERE key = abap_true.
      lv_last_key = sy-tabix.
    ENDLOOP.
    DATA lv_ix TYPE i.
    LOOP AT lt_fld INTO ls_f.
      lv_ix = lv_ix + 1.
      APPEND ls_f-name TO lt_head.
      IF lv_act = abap_true AND lv_ix = lv_last_key.
        APPEND gc_action TO lt_head.
      ENDIF.
    ENDLOOP.

    " ---- rows: none without numbers, the records of each number otherwise
    DATA lt_rows TYPE tt_row.
    DATA(lt_nums) = numbers( ).
    IF lt_nums IS NOT INITIAL.
      DATA lt_g TYPE string_table.
      LOOP AT lt_fld INTO ls_f.
        IF NOT line_exists( lt_g[ table_line = CONV string( ls_f-grp ) ] ).
          APPEND CONV string( ls_f-grp ) TO lt_g.
        ENDIF.
        DATA(lv_root) = lcl_cat=>root_of( ls_f-grp ).
        IF NOT line_exists( lt_g[ table_line = CONV string( lv_root ) ] ).
          APPEND CONV string( lv_root ) TO lt_g.
        ENDIF.
      ENDLOOP.
      DATA(lo_db) = NEW lcl_db( it_num = lt_nums it_grp = lt_g ).

      " Which parts carry keys: a company code part, a sales part.
      DATA(lv_cgrp) = VALUE ty_grp( ).
      DATA(lv_sgrp) = VALUE ty_grp( ).
      LOOP AT lt_fld INTO ls_f.
        DATA(lv_r) = lcl_cat=>root_of( ls_f-grp ).
        IF lv_r = 'COMP'.
          lv_cgrp = 'COMP'.
        ELSEIF lv_r = 'SALE' OR lv_r = 'PURC'.
          lv_sgrp = lv_r.
        ENDIF.
      ENDLOOP.

      LOOP AT lt_nums INTO DATA(lv_n).
        " The entries one row stands for.
        TYPES: BEGIN OF ty_slot,
                 ckey TYPE string,   " company code key
                 skey TYPE string,   " sales / purchasing key
                 rkey TYPE string,   " repeating entry key
               END OF ty_slot.
        DATA lt_slot TYPE STANDARD TABLE OF ty_slot WITH EMPTY KEY.
        CLEAR lt_slot.
        IF lv_rep IS NOT INITIAL.
          DATA(lv_pn) = lines( lcl_cat=>key_names( lcl_cat=>grp( lv_rep )-parent ) ).
          LOOP AT lo_db->records( iv_grp = lv_rep iv_num = lv_n ) INTO DATA(ls_e).
            IF in_scope( iv_grp = lv_rep iv_key = ls_e-key ) = abap_false.
              CONTINUE.
            ENDIF.
            DATA(lv_pk) = COND string( WHEN lv_pn > 0 THEN prefix( iv_key = ls_e-key iv_n = lv_pn ) ).
            APPEND VALUE #( ckey = COND #( WHEN lv_cgrp IS NOT INITIAL THEN lv_pk )
                            skey = COND #( WHEN lv_sgrp IS NOT INITIAL THEN lv_pk )
                            rkey = ls_e-key ) TO lt_slot.
          ENDLOOP.
        ELSE.
          DATA lt_ck TYPE string_table.
          DATA lt_sk TYPE string_table.
          CLEAR: lt_ck, lt_sk.
          IF lv_cgrp IS NOT INITIAL.
            LOOP AT lo_db->records( iv_grp = lv_cgrp iv_num = lv_n ) INTO ls_e.
              IF in_scope( iv_grp = lv_cgrp iv_key = ls_e-key ) = abap_true.
                APPEND ls_e-key TO lt_ck.
              ENDIF.
            ENDLOOP.
          ENDIF.
          IF lv_sgrp IS NOT INITIAL.
            LOOP AT lo_db->records( iv_grp = lv_sgrp iv_num = lv_n ) INTO ls_e.
              IF in_scope( iv_grp = lv_sgrp iv_key = ls_e-key ) = abap_true.
                APPEND ls_e-key TO lt_sk.
              ENDIF.
            ENDLOOP.
          ENDIF.
          IF lt_ck IS INITIAL.
            APPEND `` TO lt_ck.
          ENDIF.
          IF lt_sk IS INITIAL.
            APPEND `` TO lt_sk.
          ENDIF.
          LOOP AT lt_ck INTO DATA(lv_ck).
            LOOP AT lt_sk INTO DATA(lv_sk).
              APPEND VALUE #( ckey = lv_ck skey = lv_sk ) TO lt_slot.
            ENDLOOP.
          ENDLOOP.
        ENDIF.
        IF lt_slot IS INITIAL.
          APPEND INITIAL LINE TO lt_slot.
        ENDIF.

        LOOP AT lt_slot INTO DATA(ls_slot).
          DATA ls_row TYPE ty_row.
          CLEAR ls_row.
          CLEAR lv_ix.
          LOOP AT lt_fld INTO ls_f.
            lv_ix = lv_ix + 1.
            DATA lv_cell TYPE string.
            CLEAR lv_cell.
            IF ls_f-name = lv_num.
              " A new record gets its number when it is created.
              lv_cell = COND #( WHEN p_crt = abap_false THEN lcl_val=>to_ext( is_f = ls_f iv_val = CONV kunnr( lv_n ) ) ).
            ELSE.
              DATA(lv_g2) = lcl_cat=>root_of( ls_f-grp ).
              DATA(lv_key) = COND string(
                WHEN lcl_cat=>grp( ls_f-grp )-rep = abap_true THEN ls_slot-rkey
                WHEN lv_g2 = 'COMP'                           THEN ls_slot-ckey
                WHEN lv_g2 = 'SALE' OR lv_g2 = 'PURC'         THEN ls_slot-skey
                ELSE `` ).
              DATA(lr_v) = lo_db->value( is_f = ls_f iv_num = lv_n iv_key = lv_key ).
              IF lr_v IS BOUND.
                ASSIGN lr_v->* TO FIELD-SYMBOL(<lv_v>).
                lv_cell = lcl_val=>to_ext( is_f = ls_f iv_val = <lv_v> ).
              ENDIF.
            ENDIF.
            APPEND lv_cell TO ls_row-cells.
            IF lv_act = abap_true AND lv_ix = lv_last_key.
              APPEND `` TO ls_row-cells.
            ENDIF.
          ENDLOOP.
          ls_row-row = lines( lt_rows ) + 2.
          APPEND ls_row TO lt_rows.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    TRY.
        lcl_file=>write( it_head = lt_head it_rows = lt_rows ).
      CATCH lcx_dl INTO DATA(lx).
        DATA(lv_xt) = lx->get_text( ).
        MESSAGE lv_xt TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    IF lt_rows IS INITIAL.
      MESSAGE |Template with { lines( lt_head ) } columns written to { p_file }| TYPE 'S'.
    ELSE.
      MESSAGE |{ lines( lt_rows ) } rows written to { p_file }| TYPE 'S'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_UL - the upload
*   1 read the file and match every heading to a field
*   2 convert and check every value, row by row
*   3 change: compare with SAP; create: take what is filled
*   4 one popup for the cells that would clear a field
*   5 build the API's structures with only what changes, and post - one
*     business partner at a time, so one bad record stops nobody else
*----------------------------------------------------------------------*
CLASS lcl_ul DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS run.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_col,
             idx TYPE i,
             fld TYPE ty_fld,
           END OF ty_col,
           tt_col TYPE STANDARD TABLE OF ty_col WITH EMPTY KEY.
    " One value going to SAP.
    TYPES: BEGIN OF ty_chg,
             row   TYPE i,
             grp   TYPE ty_grp,
             key   TYPE string,
             fld   TYPE ty_fld,
             new   TYPE REF TO data,
             oldx  TYPE string,
             newx  TYPE string,
             clear TYPE abap_bool,
           END OF ty_chg,
           tt_chg TYPE STANDARD TABLE OF ty_chg WITH EMPTY KEY.
    " One company code, sales area or repeating entry the record touches.
    TYPES: BEGIN OF ty_ent,
             grp  TYPE ty_grp,
             key  TYPE string,
             task TYPE c LENGTH 1,
             row  TYPE i,
           END OF ty_ent,
           tt_ent TYPE STANDARD TABLE OF ty_ent WITH EMPTY KEY.
    TYPES: BEGIN OF ty_obj,
             tag    TYPE string,
             num    TYPE string,
             row    TYPE i,
             exists TYPE abap_bool,
             err    TYPE abap_bool,
             rows   TYPE tt_row,
             chg    TYPE tt_chg,
             ent    TYPE tt_ent,
           END OF ty_obj,
           tt_obj TYPE STANDARD TABLE OF ty_obj WITH EMPTY KEY.
    " A line already placed in a table of the API structure.
    TYPES: BEGIN OF ty_line,
             key TYPE string,
             ref TYPE REF TO data,
           END OF ty_line.

    CLASS-DATA mo_log  TYPE REF TO lcl_log.
    CLASS-DATA mt_col  TYPE tt_col.
    CLASS-DATA mv_act  TYPE i.
    CLASS-DATA mv_rep  TYPE ty_grp.
    CLASS-DATA mo_db   TYPE REF TO lcl_db.
    CLASS-DATA mt_line TYPE HASHED TABLE OF ty_line WITH UNIQUE KEY key.

    CLASS-METHODS map_cols
      IMPORTING it_head   TYPE tt_cell
                it_rows   TYPE tt_row
      RETURNING VALUE(rv) TYPE abap_bool.
    CLASS-METHODS check_cols RETURNING VALUE(rv) TYPE abap_bool.
    CLASS-METHODS col_of
      IMPORTING iv_name   TYPE string
      RETURNING VALUE(rv) TYPE i.
    CLASS-METHODS cell
      IMPORTING is_row    TYPE ty_row
                iv_idx    TYPE i
      RETURNING VALUE(rv) TYPE string.
    " The key of part IV_GRP in this row, as LCL_DB files it.
    CLASS-METHODS row_key
      IMPORTING is_row    TYPE ty_row
                iv_grp    TYPE ty_grp
      RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS gather
      IMPORTING it_rows   TYPE tt_row
      RETURNING VALUE(rt) TYPE tt_obj.
    CLASS-METHODS compare CHANGING cs_obj TYPE ty_obj.
    CLASS-METHODS add_chg
      IMPORTING is_chg TYPE ty_chg
      CHANGING  cs_obj TYPE ty_obj.
    CLASS-METHODS validate CHANGING cs_obj TYPE ty_obj.
    CLASS-METHODS confirm_clears
      IMPORTING it_obj    TYPE tt_obj
      RETURNING VALUE(rv) TYPE abap_bool.
    CLASS-METHODS build
      IMPORTING is_obj  TYPE ty_obj
      EXPORTING es_ext  TYPE cvis_ei_extern
                ev_guid TYPE bu_partner_guid.
    CLASS-METHODS post
      IMPORTING is_ext    TYPE cvis_ei_extern
                is_obj    TYPE ty_obj
      RETURNING VALUE(rv) TYPE abap_bool.
    CLASS-METHODS reset_bp.

    " ---- generic access to the API structure
    CLASS-METHODS sub
      IMPORTING ir        TYPE REF TO data
                iv_path   TYPE string
      RETURNING VALUE(rr) TYPE REF TO data.
    CLASS-METHODS put
      IMPORTING ir      TYPE REF TO data
                iv_path TYPE string
                iv_val  TYPE any.
    CLASS-METHODS set_field
      IMPORTING ir_line TYPE REF TO data
                is_chg  TYPE ty_chg.
    CLASS-METHODS new_line
      IMPORTING ir_tab    TYPE REF TO data
      RETURNING VALUE(rr) TYPE REF TO data.
    " The line of a company code / sales area / repeating entry, created on
    " first use.
    CLASS-METHODS line_of
      IMPORTING ir_obj    TYPE REF TO data
                is_ent    TYPE ty_ent
      RETURNING VALUE(rr) TYPE REF TO data.
    CLASS-METHODS text_lines
      IMPORTING iv_text   TYPE string
      RETURNING VALUE(rt) TYPE tline_tab.
ENDCLASS.


CLASS lcl_ul IMPLEMENTATION.

  METHOD cell.
    READ TABLE is_row-cells INTO rv INDEX iv_idx.
    IF sy-subrc <> 0.
      CLEAR rv.
    ENDIF.
  ENDMETHOD.

  METHOD col_of.
    READ TABLE mt_col INTO DATA(ls_c) WITH KEY fld-name = iv_name.
    IF sy-subrc = 0.
      rv = ls_c-idx.
    ENDIF.
  ENDMETHOD.

  METHOD map_cols.
    rv = abap_true.
    CLEAR: mt_col, mv_act.
    LOOP AT it_head INTO DATA(lv_h).
      DATA(lv_i) = sy-tabix.
      DATA(lv_hc) = condense( lv_h ).
      IF lv_hc IS INITIAL.
        " A column without a heading is only a problem if it has data.
        LOOP AT it_rows INTO DATA(ls_r).
          " IS NOT INITIAL takes a data object, not a built-in function.
          DATA(lv_cv) = condense( cell( is_row = ls_r iv_idx = lv_i ) ).
          IF lv_cv IS NOT INITIAL.
            mo_log->add( iv_row = ls_r-row iv_type = 'E'
                         iv_msg = |Column { lcl_util=>col_letter( lv_i ) } has data but no field name in row 1| ).
            rv = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.
        CONTINUE.
      ENDIF.
      IF to_upper( lv_hc ) = gc_action.
        mv_act = lv_i.
        CONTINUE.
      ENDIF.
      DATA ls_f TYPE ty_fld.
      DATA lv_err TYPE string.
      lcl_cat=>resolve( EXPORTING iv_head = lv_hc IMPORTING es_fld = ls_f ev_err = lv_err ).
      IF lv_err IS NOT INITIAL.
        mo_log->add( iv_field = lv_hc iv_type = 'E' iv_msg = lv_err ).
        rv = abap_false.
        CONTINUE.
      ENDIF.
      IF line_exists( mt_col[ fld-name = ls_f-name ] ).
        mo_log->add( iv_field = lv_hc iv_type = 'E'
                     iv_msg = |{ ls_f-name } is in the file twice - columns { lcl_util=>col_letter( col_of( ls_f-name ) ) } | &&
                              |and { lcl_util=>col_letter( lv_i ) }| ).
        rv = abap_false.
        CONTINUE.
      ENDIF.
      APPEND VALUE #( idx = lv_i fld = ls_f ) TO mt_col.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_cols.
    rv = abap_true.
    DATA(lv_num) = lcl_cat=>num_name( ).
    IF p_chg = abap_true AND col_of( lv_num ) = 0.
      mo_log->add( iv_field = lv_num iv_type = 'E'
                   iv_msg = |A change needs the number column { lv_num }| ).
      rv = abap_false.
    ENDIF.

    DATA lt_fld TYPE tt_fld.
    LOOP AT mt_col INTO DATA(ls_c).
      APPEND ls_c-fld TO lt_fld.
    ENDLOOP.

    " ---- one repeating part at most, and only next to what it belongs to
    DATA lt_names TYPE string_table.
    LOOP AT lt_fld INTO DATA(ls_f) WHERE key = abap_false.
      APPEND ls_f-name TO lt_names.
    ENDLOOP.
    DATA lt_dummy TYPE tt_fld.
    DATA lv_dact TYPE abap_bool.
    DATA lv_err TYPE string.
    lcl_sel=>complete( EXPORTING it_names  = lt_names
                                 iv_create = p_crt
                       IMPORTING et_fld    = lt_dummy
                                 ev_action = lv_dact
                                 ev_err    = lv_err ).
    IF lv_err IS NOT INITIAL.
      mo_log->add( iv_type = 'E' iv_msg = lv_err ).
      rv = abap_false.
      RETURN.
    ENDIF.
    mv_rep = lcl_sel=>rep_of( lt_dummy ).

    " ---- every column the system needs must be there
    LOOP AT lt_dummy INTO ls_f WHERE must = abap_true.
      IF ls_f-name = lv_num.
        CONTINUE.
      ENDIF.
      IF col_of( ls_f-name ) = 0.
        mo_log->add( iv_field = ls_f-name iv_type = 'E'
                     iv_msg = |Column { ls_f-name } is missing - { COND string( WHEN ls_f-key = abap_true
                                THEN `it identifies the record` ELSE `a new record needs it` ) }| ).
        rv = abap_false.
      ENDIF.
    ENDLOOP.
    IF mv_rep IS INITIAL AND mv_act > 0.
      mo_log->add( iv_field = gc_action iv_type = 'W'
                   iv_msg = 'The ACTION column is only used with a repeating part - it is ignored' ).
    ENDIF.
  ENDMETHOD.

  METHOD row_key.
    DATA(lt_k) = lcl_cat=>key_names( iv_grp ).
    DATA lv_n TYPE i.
    LOOP AT lt_k INTO DATA(lv_k).
      lv_n = lv_n + 1.
      DATA(lv_i) = col_of( lv_k ).
      DATA lv_v TYPE string.
      CLEAR lv_v.
      IF lv_i > 0.
        DATA(ls_f) = lcl_cat=>by_name( lv_k ).
        DATA lr_v TYPE REF TO data.
        DATA lv_e TYPE string.
        lcl_val=>to_int( EXPORTING is_f = ls_f iv_in = cell( is_row = is_row iv_idx = lv_i )
                         IMPORTING er_val = lr_v ev_err = lv_e ).
        IF lr_v IS BOUND.
          ASSIGN lr_v->* TO FIELD-SYMBOL(<lv>).
          lv_v = condense( CONV string( <lv> ) ).
        ENDIF.
      ENDIF.
      rv = COND #( WHEN lv_n = 1 THEN lv_v ELSE |{ rv }\|{ lv_v }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD gather.
    " Rows are grouped by number. On a creation a row with no number is a
    " record of its own.
    DATA(lv_ncol) = col_of( lcl_cat=>num_name( ) ).
    DATA(ls_nf)   = lcl_cat=>by_name( lcl_cat=>num_name( ) ).
    LOOP AT it_rows INTO DATA(ls_r).
      DATA lv_num TYPE string.
      CLEAR lv_num.
      IF lv_ncol > 0.
        DATA(lv_raw) = condense( cell( is_row = ls_r iv_idx = lv_ncol ) ).
        IF lv_raw IS NOT INITIAL.
          DATA lr_n TYPE REF TO data.
          DATA lv_e TYPE string.
          lcl_val=>to_int( EXPORTING is_f = ls_nf iv_in = lv_raw IMPORTING er_val = lr_n ev_err = lv_e ).
          IF lv_e IS NOT INITIAL.
            mo_log->add( iv_row = ls_r-row iv_field = ls_nf-name iv_new = lv_raw iv_type = 'E' iv_msg = lv_e ).
            CONTINUE.
          ENDIF.
          ASSIGN lr_n->* TO FIELD-SYMBOL(<lv_n>).
          lv_num = <lv_n>.
        ENDIF.
      ENDIF.
      IF lv_num IS INITIAL AND p_chg = abap_true.
        mo_log->add( iv_row = ls_r-row iv_field = ls_nf-name iv_type = 'E'
                     iv_msg = 'No number in this row - a change needs one' ).
        CONTINUE.
      ENDIF.
      DATA(lv_tag) = COND string( WHEN lv_num IS INITIAL THEN |#NEW{ ls_r-row }| ELSE lv_num ).
      READ TABLE rt ASSIGNING FIELD-SYMBOL(<ls_o>) WITH KEY tag = lv_tag.
      IF sy-subrc <> 0.
        APPEND VALUE #( tag = lv_tag num = lv_num row = ls_r-row ) TO rt ASSIGNING <ls_o>.
      ENDIF.
      APPEND ls_r TO <ls_o>-rows.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_chg.
    " The same value from two rows of one record is fine; two different
    " values are a contradiction the program cannot settle.
    READ TABLE cs_obj-chg INTO DATA(ls_old)
         WITH KEY grp = is_chg-grp key = is_chg-key fld-name = is_chg-fld-name.
    IF sy-subrc = 0.
      IF ls_old-newx <> is_chg-newx.
        mo_log->add( iv_row = is_chg-row iv_obj = cs_obj-tag iv_field = is_chg-fld-name
                     iv_old = ls_old-newx iv_new = is_chg-newx iv_type = 'E'
                     iv_msg = |Row { ls_old-row } gives "{ ls_old-newx }" for the same field| ).
        cs_obj-err = abap_true.
      ENDIF.
      RETURN.
    ENDIF.
    APPEND is_chg TO cs_obj-chg.
  ENDMETHOD.

  METHOD compare.
    DATA(lv_create) = p_crt.
    LOOP AT cs_obj-rows INTO DATA(ls_r).
      " ---- the entries this row stands for
      DATA lv_act TYPE c LENGTH 1.
      CLEAR lv_act.
      IF mv_act > 0 AND mv_rep IS NOT INITIAL.
        lv_act = to_upper( condense( cell( is_row = ls_r iv_idx = mv_act ) ) ).
        IF lv_act NA ' IUD'.
          mo_log->add( iv_row = ls_r-row iv_obj = cs_obj-tag iv_field = gc_action iv_new = lv_act
                       iv_type = 'E' iv_msg = 'ACTION must be I (insert), U (update), D (delete) or empty' ).
          cs_obj-err = abap_true.
          CONTINUE.
        ENDIF.
      ENDIF.

      DATA lt_grp TYPE SORTED TABLE OF ty_grp WITH UNIQUE KEY table_line.
      CLEAR lt_grp.
      LOOP AT mt_col INTO DATA(ls_c).
        INSERT ls_c-fld-grp INTO TABLE lt_grp.
        DATA(lv_root) = lcl_cat=>root_of( ls_c-fld-grp ).
        INSERT lv_root INTO TABLE lt_grp.
      ENDLOOP.

      LOOP AT lt_grp INTO DATA(lv_g).
        DATA(ls_g) = lcl_cat=>grp( lv_g ).
        IF ls_g-keys IS INITIAL AND ls_g-rep = abap_false.
          CONTINUE.
        ENDIF.
        DATA(lv_k) = row_key( is_row = ls_r iv_grp = lv_g ).
        IF lv_k CO '|'.
          " no key in this row for this part
          CONTINUE.
        ENDIF.
        DATA(lv_have) = xsdbool( cs_obj-exists = abap_true AND mo_db IS BOUND
                                 AND mo_db->rec( iv_grp = lv_g iv_num = cs_obj-num iv_key = lv_k ) IS BOUND ).
        DATA lv_task TYPE c LENGTH 1.
        IF ls_g-rep = abap_true.
          lv_task = lv_act.
          IF lv_task = space.
            lv_task = COND #( WHEN lv_have = abap_true THEN gc_upd ELSE gc_ins ).
          ENDIF.
          IF ( lv_task = gc_upd OR lv_task = gc_del ) AND lv_have = abap_false.
            mo_log->add( iv_row = ls_r-row iv_obj = cs_obj-tag iv_part = ls_g-text iv_type = 'E'
                         iv_msg = |Entry { lv_k } does not exist - it cannot be { COND string( WHEN lv_task = gc_del
                                  THEN `deleted` ELSE `updated` ) }| ).
            cs_obj-err = abap_true.
            CONTINUE.
          ENDIF.
          IF lv_task = gc_ins AND lv_have = abap_true.
            mo_log->add( iv_row = ls_r-row iv_obj = cs_obj-tag iv_part = ls_g-text iv_type = 'E'
                         iv_msg = |Entry { lv_k } exists already - use U to change it| ).
            cs_obj-err = abap_true.
            CONTINUE.
          ENDIF.
          APPEND VALUE #( grp = lv_g key = lv_k task = lv_task row = ls_r-row ) TO cs_obj-ent.
        ELSEIF NOT line_exists( cs_obj-ent[ grp = lv_g key = lv_k ] ).
          APPEND VALUE #( grp = lv_g key = lv_k row = ls_r-row
                          task = COND #( WHEN lv_have = abap_true THEN gc_upd ELSE gc_ins ) ) TO cs_obj-ent.
        ENDIF.
      ENDLOOP.

      " ---- the values
      LOOP AT mt_col INTO ls_c WHERE fld-key = abap_false.
        DATA(ls_f) = ls_c-fld.
        DATA(lv_raw) = cell( is_row = ls_r iv_idx = ls_c-idx ).
        IF condense( lv_raw ) = gc_keep.
          CONTINUE.
        ENDIF.
        DATA(lv_rg) = lcl_cat=>root_of( ls_f-grp ).
        DATA(lv_key) = COND string( WHEN lcl_cat=>grp( ls_f-grp )-rep = abap_true
                                    THEN row_key( is_row = ls_r iv_grp = ls_f-grp )
                                    WHEN lv_rg = 'CENT' THEN ``
                                    ELSE row_key( is_row = ls_r iv_grp = lv_rg ) ).
        " A deleted entry takes no values.
        IF lcl_cat=>grp( ls_f-grp )-rep = abap_true
           AND line_exists( cs_obj-ent[ grp = ls_f-grp key = lv_key task = gc_del ] ).
          CONTINUE.
        ENDIF.

        DATA lr_new TYPE REF TO data.
        DATA lv_err TYPE string.
        lcl_val=>to_int( EXPORTING is_f = ls_f iv_in = lv_raw IMPORTING er_val = lr_new ev_err = lv_err ).
        IF lv_err IS NOT INITIAL.
          mo_log->add( iv_row = ls_r-row iv_obj = cs_obj-tag iv_field = ls_f-name iv_new = lv_raw
                       iv_type = 'E' iv_msg = lv_err ).
          cs_obj-err = abap_true.
          CONTINUE.
        ENDIF.
        ASSIGN lr_new->* TO FIELD-SYMBOL(<lv_new>).

        DATA lr_cur TYPE REF TO data.
        CLEAR lr_cur.
        IF lv_create = abap_false AND mo_db IS BOUND.
          lr_cur = mo_db->value( is_f = ls_f iv_num = cs_obj-num iv_key = lv_key ).
        ENDIF.
        DATA(lv_oldx) = ``.
        DATA(lv_same) = abap_false.
        IF lr_cur IS BOUND.
          ASSIGN lr_cur->* TO FIELD-SYMBOL(<lv_cur>).
          lv_oldx = lcl_val=>to_ext( is_f = ls_f iv_val = <lv_cur> ).
          lv_same = xsdbool( <lv_new> = <lv_cur> ).
        ELSE.
          lv_same = xsdbool( <lv_new> IS INITIAL ).
        ENDIF.
        IF lv_same = abap_true.
          CONTINUE.
        ENDIF.
        add_chg( EXPORTING is_chg = VALUE #( row = ls_r-row grp = ls_f-grp key = lv_key fld = ls_f new = lr_new
                                             oldx = lv_oldx newx = lcl_val=>to_ext( is_f = ls_f iv_val = <lv_new> )
                                             clear = xsdbool( <lv_new> IS INITIAL ) )
                 CHANGING  cs_obj = cs_obj ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD validate.
    " Every value that goes to SAP is checked against its check table and
    " fixed values, with the other fields of the same record as context.
    LOOP AT cs_obj-chg INTO DATA(ls_c) WHERE clear = abap_false.
      DATA lt_ctx TYPE lcl_chk=>tt_ctx.
      CLEAR lt_ctx.
      " the record as it is
      IF mo_db IS BOUND AND cs_obj-exists = abap_true.
        DATA lr_rec TYPE REF TO data.
        lr_rec = mo_db->rec( iv_grp = ls_c-fld-grp iv_num = cs_obj-num iv_key = ls_c-key ).
        IF lr_rec IS BOUND.
          ASSIGN lr_rec->* TO FIELD-SYMBOL(<ls_rec>).
          DATA(lo_s) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <ls_rec> ) ).
          LOOP AT lo_s->components INTO DATA(ls_comp).
            ASSIGN COMPONENT ls_comp-name OF STRUCTURE <ls_rec> TO FIELD-SYMBOL(<lv_rc>).
            IF sy-subrc = 0 AND ls_comp-type_kind CA 'CNDTg'.
              INSERT VALUE #( fld = ls_comp-name val = condense( CONV string( <lv_rc> ) ) ) INTO TABLE lt_ctx.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
      " what the file sends, on top
      LOOP AT cs_obj-chg INTO DATA(ls_o) WHERE grp = ls_c-grp AND key = ls_c-key.
        ASSIGN ls_o-new->* TO FIELD-SYMBOL(<lv_o>).
        DELETE lt_ctx WHERE fld = ls_o-fld-fld.
        INSERT VALUE #( fld = ls_o-fld-fld val = condense( CONV string( <lv_o> ) ) ) INTO TABLE lt_ctx.
      ENDLOOP.
      " and the keys of the record
      DATA(lt_kn) = lcl_cat=>key_names( ls_c-grp ).
      SPLIT ls_c-key AT `|` INTO TABLE DATA(lt_kv).
      DATA lv_kx TYPE i.
      CLEAR lv_kx.
      LOOP AT lt_kn INTO DATA(lv_kn).
        lv_kx = lv_kx + 1.
        DATA(lv_kf) = CONV fieldname( substring_after( val = lv_kn sub = `-` ) ).
        IF NOT line_exists( lt_ctx[ fld = lv_kf ] ).
          INSERT VALUE #( fld = lv_kf val = VALUE #( lt_kv[ lv_kx ] OPTIONAL ) ) INTO TABLE lt_ctx.
        ENDIF.
      ENDLOOP.

      ASSIGN ls_c-new->* TO FIELD-SYMBOL(<lv_v>).
      DATA(lv_msg) = lcl_chk=>check( is_f = ls_c-fld iv_val = condense( CONV string( <lv_v> ) ) it_ctx = lt_ctx ).
      IF lv_msg IS NOT INITIAL.
        mo_log->add( iv_row = ls_c-row iv_obj = cs_obj-tag iv_field = ls_c-fld-name iv_new = ls_c-newx
                     iv_type = 'E' iv_msg = lv_msg ).
        cs_obj-err = abap_true.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD confirm_clears.
    rv = abap_true.
    DATA lv_n TYPE i.
    LOOP AT it_obj INTO DATA(ls_o) WHERE err = abap_false.
      LOOP AT ls_o-chg TRANSPORTING NO FIELDS WHERE clear = abap_true.
        lv_n = lv_n + 1.
      ENDLOOP.
    ENDLOOP.
    IF lv_n = 0.
      RETURN.
    ENDIF.
    DATA lv_ans TYPE c LENGTH 1.
    DATA lv_q   TYPE c LENGTH 400.
    lv_q = |{ lv_n } field(s) are empty in the file but filled in SAP. Empty cells clear the field. | &&
           |Clear them? To leave a field as it is, put # in its cell instead.|.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING  titlebar              = 'Empty cells'
                 text_question         = lv_q
                 text_button_1         = 'Clear them'
                 icon_button_1         = 'ICON_DELETE'
                 text_button_2         = 'Cancel'
                 icon_button_2         = 'ICON_CANCEL'
                 default_button        = '2'
                 display_cancel_button = ' '
      IMPORTING  answer                = lv_ans
      EXCEPTIONS OTHERS                = 1.
    rv = xsdbool( sy-subrc = 0 AND lv_ans = '1' ).
  ENDMETHOD.

  " ---- generic access to the API structure -----------------------------

  METHOD sub.
    IF ir IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN ir->* TO FIELD-SYMBOL(<ls_s>).
    SPLIT iv_path AT `-` INTO TABLE DATA(lt_p).
    LOOP AT lt_p INTO DATA(lv_p).
      ASSIGN COMPONENT lv_p OF STRUCTURE <ls_s> TO FIELD-SYMBOL(<lv_c>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      ASSIGN <lv_c> TO <ls_s>.
    ENDLOOP.
    GET REFERENCE OF <ls_s> INTO rr.
  ENDMETHOD.

  METHOD put.
    DATA(lr) = sub( ir = ir iv_path = iv_path ).
    IF lr IS BOUND.
      ASSIGN lr->* TO FIELD-SYMBOL(<lv>).
      <lv> = iv_val.
    ENDIF.
  ENDMETHOD.

  METHOD new_line.
    FIELD-SYMBOLS <lt> TYPE STANDARD TABLE.
    ASSIGN ir_tab->* TO <lt>.
    IF sy-subrc = 0.
      DATA lr_new TYPE REF TO data.
      APPEND INITIAL LINE TO <lt> REFERENCE INTO lr_new.
      rr = lr_new.
    ENDIF.
  ENDMETHOD.

  METHOD set_field.
    " DATA-<component> takes the value, DATAX-<component> says "change it".
    DATA(lr_d) = sub( ir = ir_line iv_path = |DATA-{ is_chg-fld-api }| ).
    IF lr_d IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN lr_d->* TO FIELD-SYMBOL(<lv_d>).
    ASSIGN is_chg-new->* TO FIELD-SYMBOL(<lv_v>).
    TRY.
        <lv_d> = <lv_v>.
      CATCH cx_sy_conversion_error.
        RETURN.
    ENDTRY.
    put( ir = ir_line iv_path = |DATAX-{ is_chg-fld-api }| iv_val = abap_true ).
  ENDMETHOD.

  METHOD line_of.
    DATA(ls_g) = lcl_cat=>grp( is_ent-grp ).
    DATA(lv_id) = |{ is_ent-grp }/{ is_ent-key }|.
    READ TABLE mt_line INTO DATA(ls_l) WITH TABLE KEY key = lv_id.
    IF sy-subrc = 0.
      rr = ls_l-ref.
      RETURN.
    ENDIF.

    " Where the part's table sits: on the record itself, or inside the line
    " of its company code / sales area / purchasing organisation.
    DATA lr_owner TYPE REF TO data.
    DATA lv_path TYPE string.
    lr_owner = ir_obj.
    CASE is_ent-grp.
      WHEN 'COMP'. lv_path = `COMPANY_DATA-COMPANY`.
      WHEN 'SALE'. lv_path = `SALES_DATA-SALES`.
      WHEN 'PURC'. lv_path = `PURCHASING_DATA-PURCHASING`.
      WHEN 'TAXI'. lv_path = `CENTRAL_DATA-TAX_IND-TAX_IND`.
      WHEN 'UNLD'. lv_path = `CENTRAL_DATA-LOADING-LOADING`.
      WHEN 'BANK'. lv_path = `CENTRAL_DATA-BANKDETAIL-BANKDETAILS`.
      WHEN 'CONT'. lv_path = `CENTRAL_DATA-CONTACT-CONTACTS`.
      WHEN 'PART' OR 'DUNN' OR 'WTAX'.
        " Inside the parent's line.
        DATA(lv_pn) = lines( lcl_cat=>key_names( ls_g-parent ) ).
        SPLIT is_ent-key AT `|` INTO TABLE DATA(lt_kp).
        DATA lv_pk TYPE string.
        CLEAR lv_pk.
        LOOP AT lt_kp INTO DATA(lv_kp) TO lv_pn.
          lv_pk = COND #( WHEN sy-tabix = 1 THEN lv_kp ELSE |{ lv_pk }\|{ lv_kp }| ).
        ENDLOOP.
        " The parent's own entry sets its real task when it is placed.
        lr_owner = line_of( ir_obj = ir_obj is_ent = VALUE #( grp = ls_g-parent key = lv_pk task = gc_upd ) ).
        lv_path = SWITCH #( is_ent-grp WHEN 'PART' THEN `FUNCTIONS-FUNCTIONS`
                                       WHEN 'DUNN' THEN `DUNNING-DUNNING`
                                       ELSE `WTAX_TYPE-WTAX_TYPE` ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    rr = new_line( sub( ir = lr_owner iv_path = lv_path ) ).
    IF rr IS NOT BOUND.
      RETURN.
    ENDIF.
    put( ir = rr iv_path = `TASK` iv_val = is_ent-task ).
    " DATA_KEY: the part's own keys, the last parts of the entry key.
    SPLIT ls_g-keys AT ` ` INTO TABLE DATA(lt_own).
    SPLIT is_ent-key AT `|` INTO TABLE DATA(lt_kv).
    DATA(lv_off) = lines( lt_kv ) - lines( lt_own ).
    DATA lv_ki TYPE i.
    LOOP AT lt_own INTO DATA(lv_own).
      lv_ki = lv_off + sy-tabix.
      DATA(lv_val) = VALUE string( lt_kv[ lv_ki ] OPTIONAL ).
      put( ir = rr iv_path = |DATA_KEY-{ lv_own }| iv_val = lv_val ).
    ENDLOOP.
    INSERT VALUE #( key = lv_id ref = rr ) INTO TABLE mt_line.
  ENDMETHOD.

  METHOD text_lines.
    " A cell may hold several lines; a line longer than a text line is
    " carried on in the next.
    DATA(lv) = iv_text.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv WITH cl_abap_char_utilities=>newline.
    SPLIT lv AT cl_abap_char_utilities=>newline INTO TABLE DATA(lt_p).
    LOOP AT lt_p INTO DATA(lv_p).
      DATA(lv_first) = abap_true.
      WHILE lv_first = abap_true OR lv_p IS NOT INITIAL.
        DATA ls_l TYPE tline.
        ls_l-tdformat = COND #( WHEN lv_first = abap_true THEN '*' ELSE '=' ).
        ls_l-tdline   = lv_p.
        APPEND ls_l TO rt.
        lv_first = abap_false.
        lv_p = COND #( WHEN strlen( lv_p ) > 132 THEN substring( val = lv_p off = 132 ) ELSE `` ).
      ENDWHILE.
    ENDLOOP.
  ENDMETHOD.

  METHOD build.
    CLEAR: es_ext, ev_guid, mt_line.
    DATA(lv_create) = xsdbool( is_obj-exists = abap_false ).
    DATA(lv_task) = COND #( WHEN lv_create = abap_true THEN gc_ins ELSE gc_upd ).
    DATA lr_ext TYPE REF TO data.
    GET REFERENCE OF es_ext INTO lr_ext.
    DATA(lr_obj) = sub( ir = lr_ext iv_path = COND #( WHEN gc_side = 'C' THEN `CUSTOMER` ELSE `VENDOR` ) ).

    " ---- the customer / supplier
    put( ir = lr_obj iv_path = `HEADER-OBJECT_TASK` iv_val = lv_task ).
    IF is_obj-num IS NOT INITIAL.
      put( ir = lr_obj iv_path = |HEADER-OBJECT_INSTANCE-{ lcl_cat=>grp( 'CENT' )-numfld }| iv_val = is_obj-num ).
    ENDIF.
    DATA(lr_cd)  = sub( ir = lr_obj iv_path = `CENTRAL_DATA-CENTRAL` ).
    IF lv_create = abap_true.
      DATA(lv_ktf) = COND string( WHEN gc_side = 'C' THEN `KTOKD` ELSE `KTOKK` ).
      put( ir = lr_cd iv_path = |DATA-{ lv_ktf }| iv_val = p_ktogr ).
      put( ir = lr_cd iv_path = |DATAX-{ lv_ktf }| iv_val = abap_true ).
    ENDIF.

    " ---- the entries: company codes, sales areas, repeating entries
    LOOP AT is_obj-ent INTO DATA(ls_e).
      DATA(lr_l) = line_of( ir_obj = lr_obj is_ent = ls_e ).
      " The task of a line created as a parent is the parent's own.
      IF lr_l IS BOUND.
        put( ir = lr_l iv_path = `TASK` iv_val = ls_e-task ).
      ENDIF.
    ENDLOOP.

    " ---- the values
    DATA lv_addr TYPE abap_bool.
    TYPES: BEGIN OF ty_com,
             kind TYPE string,
             cons TYPE ad_consnum,
             val  TYPE string,
             ext  TYPE string,
             xval TYPE abap_bool,
             xext TYPE abap_bool,
             del  TYPE abap_bool,
           END OF ty_com.
    DATA lt_com TYPE STANDARD TABLE OF ty_com WITH EMPTY KEY.
    LOOP AT is_obj-chg INTO DATA(ls_c).
      ASSIGN ls_c-new->* TO FIELD-SYMBOL(<lv_new>).
      CASE ls_c-fld-grp.
        WHEN 'CENT'.
          set_field( ir_line = lr_cd is_chg = ls_c ).

        WHEN 'ADDR'.
          set_field( ir_line = sub( ir = lr_obj iv_path = `CENTRAL_DATA-ADDRESS-POSTAL` ) is_chg = ls_c ).
          lv_addr = abap_true.

        WHEN 'COMM'.
          " One line per kind of number: the standard telephone, the
          " mobile, the fax, the e-mail.
          DATA(lv_kind) = COND string( WHEN ls_c-fld-api = 'MOBILE' THEN `MOB`
                                       WHEN ls_c-fld-tab = 'ADR2'   THEN `TEL`
                                       WHEN ls_c-fld-tab = 'ADR3'   THEN `FAX`
                                       ELSE `SMTP` ).
          READ TABLE lt_com ASSIGNING FIELD-SYMBOL(<ls_com>) WITH KEY kind = lv_kind.
          IF sy-subrc <> 0.
            APPEND VALUE #( kind = lv_kind cons = COND #( WHEN mo_db IS BOUND
                                                          THEN mo_db->consnumber( is_f = ls_c-fld iv_num = is_obj-num ) ) )
                   TO lt_com ASSIGNING <ls_com>.
          ENDIF.
          IF ls_c-fld-api = 'EXTENSION'.
            <ls_com>-ext  = <lv_new>.
            <ls_com>-xext = abap_true.
          ELSE.
            <ls_com>-val  = <lv_new>.
            <ls_com>-xval = abap_true.
            <ls_com>-del  = ls_c-clear.
          ENDIF.
          lv_addr = abap_true.

        WHEN 'TXTC' OR 'TXTB' OR 'TXTS'.
          DATA lr_tt TYPE REF TO data.
          IF ls_c-fld-grp = 'TXTC'.
            lr_tt = sub( ir = lr_obj iv_path = `CENTRAL_DATA-TEXT-TEXTS` ).
          ELSE.
            DATA(lv_pg) = lcl_cat=>grp( ls_c-fld-grp )-parent.
            DATA(lr_pl) = line_of( ir_obj = lr_obj is_ent = VALUE #( grp = lv_pg key = ls_c-key
                                     task = VALUE #( is_obj-ent[ grp = lv_pg key = ls_c-key ]-task DEFAULT gc_upd ) ) ).
            lr_tt = sub( ir = lr_pl iv_path = `TEXTS-TEXTS` ).
          ENDIF.
          DATA(lr_tl) = new_line( lr_tt ).
          IF lr_tl IS BOUND.
            DATA lr_old TYPE REF TO data.
            CLEAR lr_old.
            IF mo_db IS BOUND.
              lr_old = mo_db->value( is_f = ls_c-fld iv_num = is_obj-num iv_key = ls_c-key ).
            ENDIF.
            DATA(lv_had) = abap_false.
            IF lr_old IS BOUND.
              ASSIGN lr_old->* TO FIELD-SYMBOL(<lv_old>).
              lv_had = xsdbool( <lv_old> IS NOT INITIAL ).
            ENDIF.
            put( ir = lr_tl iv_path = `TASK`
                 iv_val = COND string( WHEN ls_c-clear = abap_true THEN gc_del
                                       WHEN lv_had = abap_true    THEN gc_upd
                                       ELSE gc_ins ) ).
            put( ir = lr_tl iv_path = `DATA_KEY-TEXT_ID` iv_val = ls_c-fld-tdid ).
            put( ir = lr_tl iv_path = `DATA_KEY-LANGU`   iv_val = p_tlang ).
            IF ls_c-clear = abap_false.
              put( ir = lr_tl iv_path = `DATA` iv_val = text_lines( CONV string( <lv_new> ) ) ).
            ENDIF.
          ENDIF.

        WHEN OTHERS.
          " A company code, a sales area or a repeating entry.
          READ TABLE is_obj-ent INTO DATA(ls_en) WITH KEY grp = ls_c-fld-grp key = ls_c-key.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          DATA(lr_el) = line_of( ir_obj = lr_obj is_ent = ls_en ).
          IF ls_c-fld-name = `KNVP-PARTNER`.
            " The partner number has the length its partner type uses:
            " a personnel number has 8 digits, the others 10.
            DATA(lv_parvw) = CONV parvw( segment( val = ls_c-key index = lines( lcl_cat=>key_names( ls_c-fld-grp ) ) - 1 sep = `|` ) ).
            DATA(lv_pv) = condense( CONV string( <lv_new> ) ).
            IF lv_pv CO '0123456789'.
              DATA(lv_len) = COND i( WHEN lcl_cfg=>nrart( lv_parvw ) = 'PE' THEN 8 ELSE 10 ).
              lv_pv = lcl_util=>alpha( iv_in = lv_pv iv_len = lv_len ).
            ENDIF.
            put( ir = lr_el iv_path = `DATA-PARTNER`  iv_val = lv_pv ).
            put( ir = lr_el iv_path = `DATAX-PARTNER` iv_val = abap_true ).
          ELSE.
            set_field( ir_line = lr_el is_chg = ls_c ).
          ENDIF.
      ENDCASE.
    ENDLOOP.

    " ---- telephone, mobile, fax, e-mail
    LOOP AT lt_com INTO DATA(ls_cm).
      DATA(lv_tpath) = SWITCH string( ls_cm-kind WHEN `TEL` OR `MOB` THEN `CENTRAL_DATA-ADDRESS-COMMUNICATION-PHONE-PHONE`
                                                 WHEN `FAX`          THEN `CENTRAL_DATA-ADDRESS-COMMUNICATION-FAX-FAX`
                                                 ELSE                     `CENTRAL_DATA-ADDRESS-COMMUNICATION-SMTP-SMTP` ).
      DATA(lv_vfld) = SWITCH string( ls_cm-kind WHEN `TEL` OR `MOB` THEN `TELEPHONE`
                                                WHEN `FAX`          THEN `FAX`
                                                ELSE                     `E_MAIL` ).
      DATA(lr_cl) = new_line( sub( ir = lr_obj iv_path = lv_tpath ) ).
      IF lr_cl IS NOT BOUND.
        CONTINUE.
      ENDIF.
      DATA(lv_ctask) = COND string( WHEN ls_cm-cons IS INITIAL                 THEN gc_ins
                                    WHEN ls_cm-del = abap_true                 THEN gc_del
                                    ELSE gc_upd ).
      put( ir = lr_cl iv_path = `CONTACT-TASK` iv_val = lv_ctask ).
      IF ls_cm-cons IS NOT INITIAL.
        put( ir = lr_cl iv_path = `CONTACT-DATA-CONSNUMBER`  iv_val = ls_cm-cons ).
        put( ir = lr_cl iv_path = `CONTACT-DATAX-CONSNUMBER` iv_val = abap_true ).
      ELSE.
        put( ir = lr_cl iv_path = `CONTACT-DATA-STD_NO`  iv_val = abap_true ).
        put( ir = lr_cl iv_path = `CONTACT-DATAX-STD_NO` iv_val = abap_true ).
      ENDIF.
      IF ls_cm-xval = abap_true.
        put( ir = lr_cl iv_path = |CONTACT-DATA-{ lv_vfld }|  iv_val = ls_cm-val ).
        put( ir = lr_cl iv_path = |CONTACT-DATAX-{ lv_vfld }| iv_val = abap_true ).
      ENDIF.
      IF ls_cm-xext = abap_true.
        put( ir = lr_cl iv_path = `CONTACT-DATA-EXTENSION`  iv_val = ls_cm-ext ).
        put( ir = lr_cl iv_path = `CONTACT-DATAX-EXTENSION` iv_val = abap_true ).
      ENDIF.
      IF ls_cm-kind = `MOB`.
        put( ir = lr_cl iv_path = `CONTACT-DATA-R_3_USER`  iv_val = gc_mobile ).
        put( ir = lr_cl iv_path = `CONTACT-DATAX-R_3_USER` iv_val = abap_true ).
      ENDIF.
    ENDLOOP.

    IF lv_addr = abap_true.
      put( ir = lr_obj iv_path = `CENTRAL_DATA-ADDRESS-TASK` iv_val = lv_task ).
      " A postal code is checked against its country: a row that changes
      " the one without the other sends the country SAP already has.
      DATA(lr_px) = sub( ir = lr_obj iv_path = `CENTRAL_DATA-ADDRESS-POSTAL-DATAX` ).
      FIELD-SYMBOLS: <lv_xp> TYPE any,
                     <lv_xc> TYPE any.
      IF lr_px IS BOUND.
        ASSIGN lr_px->* TO FIELD-SYMBOL(<ls_px>).
        ASSIGN COMPONENT 'POSTL_COD1' OF STRUCTURE <ls_px> TO <lv_xp>.
        ASSIGN COMPONENT 'COUNTRY' OF STRUCTURE <ls_px> TO <lv_xc>.
      ENDIF.
      IF <lv_xp> IS ASSIGNED AND <lv_xc> IS ASSIGNED AND <lv_xp> = abap_true AND <lv_xc> <> abap_true
         AND mo_db IS BOUND.
        DATA(lr_cty) = mo_db->value( is_f = lcl_cat=>by_name( `ADRC-COUNTRY` ) iv_num = is_obj-num iv_key = `` ).
        IF lr_cty IS BOUND.
          ASSIGN lr_cty->* TO FIELD-SYMBOL(<lv_cty>).
          put( ir = lr_obj iv_path = `CENTRAL_DATA-ADDRESS-POSTAL-DATA-COUNTRY` iv_val = <lv_cty> ).
          <lv_xc> = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

    " ---- the business partner
    es_ext-partner-header-object_task = lv_task.
    IF lv_create = abap_true.
      TRY.
          ev_guid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
        CATCH cx_uuid_error.
          CLEAR ev_guid.
      ENDTRY.
    ELSE.
      ev_guid = lcl_cfg=>guid_of( CONV kunnr( is_obj-num ) ).
    ENDIF.
    es_ext-partner-header-object_instance-bpartnerguid = ev_guid.

    DATA(lv_bp) = COND bu_partner( WHEN lv_create = abap_false
                                   THEN lcl_cfg=>bp_of( CONV kunnr( is_obj-num ) ) ).
    DATA lt_role TYPE lcl_cfg=>tt_role.
    DATA(lv_area) = abap_false.
    LOOP AT is_obj-ent INTO ls_e WHERE grp = 'SALE' OR grp = 'PURC'.
      lv_area = abap_true.
    ENDLOOP.
    IF lv_create = abap_true.
      es_ext-partner-central_data-common-data-bp_control-category = gc_org.
      es_ext-partner-central_data-common-data-bp_control-grouping =
        COND #( WHEN p_bpgrp IS NOT INITIAL THEN p_bpgrp ELSE lcl_cfg=>bp_group( p_ktogr ) ).
      lt_role = lcl_cfg=>bp_roles( p_ktogr ).
      IF lt_role IS INITIAL.
        APPEND COND bu_role( WHEN gc_side = 'C' THEN 'FLCU00' ELSE 'FLVN00' ) TO lt_role.
      ENDIF.
    ENDIF.
    IF lv_area = abap_true.
      DATA(lv_arole) = COND bu_role( WHEN gc_side = 'C' THEN 'FLCU01' ELSE 'FLVN01' ).
      IF NOT line_exists( lt_role[ table_line = lv_arole ] ).
        APPEND lv_arole TO lt_role.
      ENDIF.
    ENDIF.
    LOOP AT lt_role INTO DATA(lv_role).
      IF lv_create = abap_false AND lcl_cfg=>has_role( iv_bp = lv_bp iv_role = lv_role ) = abap_true.
        CONTINUE.
      ENDIF.
      APPEND VALUE bus_ei_bupa_roles( task = gc_ins data_key = lv_role ) TO es_ext-partner-central_data-role-roles.
    ENDLOOP.

    " A new partner needs its name and address of its own; on a change CVI
    " keeps the partner in step with the customer / supplier.
    IF lv_create = abap_true.
      DATA(lr_pd) = sub( ir = lr_obj iv_path = `CENTRAL_DATA-ADDRESS-POSTAL-DATA` ).
      DATA(lr_pdx) = sub( ir = lr_obj iv_path = `CENTRAL_DATA-ADDRESS-POSTAL-DATAX` ).
      ASSIGN lr_pd->* TO FIELD-SYMBOL(<ls_pd>).
      ASSIGN lr_pdx->* TO FIELD-SYMBOL(<ls_pdx>).
      DATA(lt_nm) = VALUE string_table( ( `NAME` ) ( `NAME1` ) ( `NAME_2` ) ( `NAME2` ) ( `NAME_3` ) ( `NAME3` )
                                        ( `NAME_4` ) ( `NAME4` ) ).
      DATA lv_i TYPE i VALUE 1.
      DATA lv_from TYPE string.
      DATA lv_to   TYPE string.
      WHILE lv_i < lines( lt_nm ).
        lv_from = lt_nm[ lv_i ].
        lv_to   = lt_nm[ lv_i + 1 ].
        ASSIGN COMPONENT lv_from OF STRUCTURE <ls_pd> TO FIELD-SYMBOL(<lv_nm>).
        IF sy-subrc = 0 AND <lv_nm> IS NOT INITIAL.
          ASSIGN COMPONENT lv_to OF STRUCTURE es_ext-partner-central_data-common-data-bp_organization
                 TO FIELD-SYMBOL(<lv_bn>).
          IF sy-subrc = 0.
            <lv_bn> = <lv_nm>.
            ASSIGN COMPONENT lv_to OF STRUCTURE es_ext-partner-central_data-common-datax-bp_organization
                   TO FIELD-SYMBOL(<lv_bx>).
            IF sy-subrc = 0.
              <lv_bx> = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
        lv_i = lv_i + 2.
      ENDWHILE.
      ASSIGN COMPONENT 'SORT1' OF STRUCTURE <ls_pd> TO FIELD-SYMBOL(<lv_s1>).
      IF sy-subrc = 0 AND <lv_s1> IS NOT INITIAL.
        es_ext-partner-central_data-common-data-bp_centraldata-searchterm1  = <lv_s1>.
        es_ext-partner-central_data-common-datax-bp_centraldata-searchterm1 = abap_true.
      ENDIF.
      DATA ls_adr TYPE bus_ei_bupa_address.
      CLEAR ls_adr.
      ls_adr-task = gc_ins.
      lcl_util=>copy_like( EXPORTING is_from  = <ls_pd>
                                     is_fromx = <ls_pdx>
                           CHANGING  cs_to    = ls_adr-data-postal-data
                                     cs_tox   = ls_adr-data-postal-datax ).
      IF ls_adr-data-postal-datax IS NOT INITIAL.
        APPEND ls_adr TO es_ext-partner-central_data-address-addresses.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD reset_bp.
    " The business partner keeps a global memory for the unit of work just
    " closed - the save mode included - and a COMMIT does not clear it.
    " Without this the next record is refused with "Parameter IV_X_SAVE".
    TRY.
        CALL FUNCTION 'BUP_MEMORY_CENTRAL_INIT'.
      CATCH cx_sy_dyn_call_illegal_func.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD post.
    rv = abap_true.
    DATA(lv_obj) = COND string( WHEN is_obj-num IS NOT INITIAL THEN is_obj-num ELSE |new (row { is_obj-row })| ).

    DATA lt_map TYPE mdg_bs_bp_msgmap_t.
    TRY.
        cl_md_bp_maintain=>validate_single( EXPORTING i_data        = is_ext
                                            IMPORTING et_return_map = lt_map ).
      CATCH cx_root INTO DATA(lx1).
        mo_log->add( iv_row = is_obj-row iv_obj = lv_obj iv_type = 'E'
                     iv_msg = |Validation failed: { lx1->get_text( ) }| ).
        rv = abap_false.
        RETURN.
    ENDTRY.
    LOOP AT lt_map INTO DATA(ls_map) WHERE type CA 'EAX'.
      mo_log->add( iv_row = is_obj-row iv_obj = lv_obj iv_type = 'E'
                   iv_field = |{ ls_map-bapistrucname }-{ ls_map-bapifldnm }| iv_msg = ls_map-message ).
      rv = abap_false.
    ENDLOOP.
    IF rv = abap_false.
      RETURN.
    ENDIF.

    DATA: lt_data TYPE cvis_ei_extern_t,
          lt_ret  TYPE bapiretm.
    APPEND is_ext TO lt_data.
    TRY.
        cl_md_bp_maintain=>maintain( EXPORTING i_data     = lt_data
                                               i_test_run = p_test
                                     IMPORTING e_return   = lt_ret ).
      CATCH cx_root INTO DATA(lx2).
        ROLLBACK WORK.
        reset_bp( ).
        mo_log->add( iv_row = is_obj-row iv_obj = lv_obj iv_type = 'E'
                     iv_msg = |Maintain failed: { lx2->get_text( ) }| ).
        rv = abap_false.
        RETURN.
    ENDTRY.

    FIELD-SYMBOLS: <lt_sub> TYPE ANY TABLE,
                   <ls_sub> TYPE any.
    LOOP AT lt_ret ASSIGNING FIELD-SYMBOL(<ls_ret>).
      ASSIGN COMPONENT 'OBJECT_MSG' OF STRUCTURE <ls_ret> TO <lt_sub>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      LOOP AT <lt_sub> ASSIGNING <ls_sub>.
        DATA ls_r2 TYPE bapiret2.
        CLEAR ls_r2.
        MOVE-CORRESPONDING <ls_sub> TO ls_r2.
        IF ls_r2-type CA 'EAX'.
          mo_log->add( iv_row = is_obj-row iv_obj = lv_obj iv_type = 'E'
                       iv_field = ls_r2-field iv_msg = ls_r2-message ).
          rv = abap_false.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    IF rv = abap_false OR p_test = abap_true.
      ROLLBACK WORK.
      reset_bp( ).
      RETURN.
    ENDIF.

    " BAPI_TRANSACTION_COMMIT, not a bare COMMIT WORK: the business partner
    " hangs its own end of the unit of work on it, and its RETURN is the only
    " thing that says whether the update actually ran.
    DATA ls_cret TYPE bapiret2.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait   = abap_true
      IMPORTING return = ls_cret.
    reset_bp( ).
    IF ls_cret-type CA 'EAX'.
      mo_log->add( iv_row = is_obj-row iv_obj = lv_obj iv_type = 'E'
                   iv_msg = |The update did not go through - nothing was saved: { ls_cret-message }| ).
      rv = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD run.
    mo_log = NEW lcl_log( ).
    CLEAR mo_db.

    DATA lt_head TYPE tt_cell.
    DATA lt_rows TYPE tt_row.
    TRY.
        lcl_file=>read( IMPORTING et_head = lt_head et_rows = lt_rows ).
      CATCH lcx_dl INTO DATA(lx).
        DATA(lv_xt) = lx->get_text( ).
        MESSAGE lv_xt TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    IF lt_head IS INITIAL OR lt_rows IS INITIAL.
      MESSAGE 'The file has no data rows - row 1 holds the field names, the data starts in row 2'
              TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " ---- 1. headings
    IF map_cols( it_head = lt_head it_rows = lt_rows ) = abap_false OR check_cols( ) = abap_false.
      mo_log->show( 'The file cannot be read - nothing was posted' ).
      RETURN.
    ENDIF.

    " ---- 2. records
    DATA(lt_obj) = gather( lt_rows ).
    DATA lt_num TYPE string_table.
    LOOP AT lt_obj INTO DATA(ls_o) WHERE num IS NOT INITIAL.
      APPEND ls_o-num TO lt_num.
    ENDLOOP.
    IF lt_num IS NOT INITIAL.
      DATA lt_g TYPE string_table.
      LOOP AT mt_col INTO DATA(ls_c).
        IF NOT line_exists( lt_g[ table_line = CONV string( ls_c-fld-grp ) ] ).
          APPEND CONV string( ls_c-fld-grp ) TO lt_g.
        ENDIF.
        DATA(lv_rt) = lcl_cat=>root_of( ls_c-fld-grp ).
        IF NOT line_exists( lt_g[ table_line = CONV string( lv_rt ) ] ).
          APPEND CONV string( lv_rt ) TO lt_g.
        ENDIF.
      ENDLOOP.
      " The general data always, to know whether the record exists.
      IF NOT line_exists( lt_g[ table_line = `CENT` ] ).
        APPEND `CENT` TO lt_g.
      ENDIF.
      mo_db = NEW lcl_db( it_num = lt_num it_grp = lt_g ).
    ENDIF.

    LOOP AT lt_obj ASSIGNING FIELD-SYMBOL(<ls_o>).
      <ls_o>-exists = xsdbool( <ls_o>-num IS NOT INITIAL AND mo_db IS BOUND
                               AND mo_db->rec( iv_grp = 'CENT' iv_num = <ls_o>-num iv_key = `` ) IS BOUND ).
      IF p_chg = abap_true AND <ls_o>-exists = abap_false.
        mo_log->add( iv_row = <ls_o>-row iv_obj = <ls_o>-tag iv_type = 'E'
                     iv_msg = |{ <ls_o>-num } does not exist - choose Create to create it| ).
        <ls_o>-err = abap_true.
        CONTINUE.
      ENDIF.
      IF p_crt = abap_true AND <ls_o>-exists = abap_true.
        mo_log->add( iv_row = <ls_o>-row iv_obj = <ls_o>-tag iv_type = 'E'
                     iv_msg = |{ <ls_o>-num } exists already - choose Change to change it| ).
        <ls_o>-err = abap_true.
        CONTINUE.
      ENDIF.
      compare( CHANGING cs_obj = <ls_o> ).
      validate( CHANGING cs_obj = <ls_o> ).
    ENDLOOP.

    " ---- 3. one question for the cells that clear a field
    IF p_chg = abap_true AND confirm_clears( lt_obj ) = abap_false.
      mo_log->add( iv_type = 'W' iv_msg = 'Cancelled - nothing was posted. Put # in the cells to leave unchanged.' ).
      mo_log->show( 'Cancelled' ).
      RETURN.
    ENDIF.

    " ---- 4. post
    DATA lv_ok TYPE i.
    DATA lv_bad TYPE i.
    DATA lv_none TYPE i.
    LOOP AT lt_obj INTO ls_o.
      DATA(lv_obj) = COND string( WHEN ls_o-num IS NOT INITIAL THEN ls_o-num ELSE |new (row { ls_o-row })| ).
      IF ls_o-err = abap_true.
        lv_bad = lv_bad + 1.
        CONTINUE.
      ENDIF.
      IF ls_o-chg IS INITIAL AND NOT line_exists( ls_o-ent[ task = gc_del ] )
         AND NOT line_exists( ls_o-ent[ task = gc_ins ] ) AND ls_o-exists = abap_true.
        mo_log->add( iv_row = ls_o-row iv_obj = lv_obj iv_type = 'I' iv_msg = 'No change - the file matches SAP' ).
        lv_none = lv_none + 1.
        CONTINUE.
      ENDIF.

      DATA ls_ext TYPE cvis_ei_extern.
      DATA lv_guid TYPE bu_partner_guid.
      build( EXPORTING is_obj = ls_o IMPORTING es_ext = ls_ext ev_guid = lv_guid ).
      DATA(lv_posted) = post( is_ext = ls_ext is_obj = ls_o ).

      " The fields, with what they were and what they become.
      LOOP AT ls_o-chg INTO DATA(ls_ch).
        mo_log->add( iv_row = ls_ch-row iv_obj = lv_obj iv_part = lcl_cat=>grp( ls_ch-grp )-text
                     iv_field = ls_ch-fld-name iv_old = ls_ch-oldx iv_new = ls_ch-newx
                     iv_type = COND #( WHEN lv_posted = abap_false THEN 'W'
                                       WHEN p_test = abap_true    THEN 'I'
                                       ELSE 'S' )
                     iv_msg = COND string( WHEN lv_posted = abap_false THEN `Not saved - see the error above`
                                           WHEN p_test = abap_true    THEN `Would change (test run)`
                                           WHEN ls_ch-clear = abap_true THEN `Cleared`
                                           ELSE `Changed` ) ).
      ENDLOOP.
      LOOP AT ls_o-ent INTO DATA(ls_en) WHERE task = gc_del OR ( task = gc_ins AND grp <> 'COMP' AND grp <> 'SALE' AND grp <> 'PURC' ).
        mo_log->add( iv_row = ls_en-row iv_obj = lv_obj iv_part = lcl_cat=>grp( ls_en-grp )-text
                     iv_field = ls_en-key iv_type = COND #( WHEN lv_posted = abap_true THEN 'I' ELSE 'W' )
                     iv_msg = COND string( WHEN ls_en-task = gc_del THEN `Entry deleted` ELSE `Entry added` ) ).
      ENDLOOP.

      IF lv_posted = abap_false.
        lv_bad = lv_bad + 1.
        CONTINUE.
      ENDIF.
      lv_ok = lv_ok + 1.
      IF p_test = abap_true.
        mo_log->add( iv_row = ls_o-row iv_obj = lv_obj iv_type = 'S'
                     iv_msg = 'Test run OK - nothing was posted' ).
      ELSEIF ls_o-exists = abap_false.
        DATA(lv_new) = lcl_cfg=>num_by_guid( lv_guid ).
        mo_log->add( iv_row = ls_o-row iv_obj = lv_new iv_type = 'S'
                     iv_msg = |Created { lv_new } - business partner { lcl_cfg=>bp_by_guid( lv_guid ) }| ).
      ELSE.
        mo_log->add( iv_row = ls_o-row iv_obj = lv_obj iv_type = 'S' iv_msg = 'Saved' ).
      ENDIF.
    ENDLOOP.

    mo_log->show( |{ COND string( WHEN p_test = abap_true THEN `TEST RUN - ` ) }| &&
                  |{ lv_ok } record(s) { COND string( WHEN p_test = abap_true THEN `would be posted` ELSE `posted` ) }, | &&
                  |{ lv_bad } with errors, { lv_none } unchanged| ).
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_APP - the selection screen
*----------------------------------------------------------------------*
CLASS lcl_app DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS init.
    CLASS-METHODS output.
    CLASS-METHODS pai.
    CLASS-METHODS run.
ENDCLASS.


CLASS lcl_app IMPLEMENTATION.

  METHOD init.
    bt_fld = 'Choose fields'.
  ENDMETHOD.

  METHOD output.
    cm_fld = |{ lines( s_fld ) } field(s) chosen - the keys are added automatically|.
    LOOP AT SCREEN.
      " Account group and grouping: creation only.
      IF screen-name = 'P_KTOGR' OR screen-name = 'P_BPGRP'.
        screen-input = COND #( WHEN p_crt = abap_true THEN '1' ELSE '0' ).
        MODIFY SCREEN.
      ENDIF.
      " Which fields and which records: download only. An upload takes its
      " fields from the file's first row.
      IF screen-name = 'BT_FLD' OR screen-name = 'P_MAX'
         OR screen-name CP 'S_NUM*' OR screen-name CP '%_S_NUM_*'
         OR screen-name CP 'S_BP*' OR screen-name CP '%_S_BP_*'
         OR screen-name CP 'S_BUKRS*' OR screen-name CP '%_S_BUKRS_*'
         OR screen-name CP 'S_ORG*' OR screen-name CP '%_S_ORG_*'.
        screen-input = COND #( WHEN p_down = abap_true THEN '1' ELSE '0' ).
        MODIFY SCREEN.
      ENDIF.
      " Test run: upload only.
      IF screen-name = 'P_TEST'.
        screen-input = COND #( WHEN p_up = abap_true THEN '1' ELSE '0' ).
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD pai.
    CASE sscrfields-ucomm.
      WHEN 'FLD'.
        lcl_sel=>choose( ).
        RETURN.
      WHEN 'ONLI' OR 'PRIN' OR 'SJOB'.
        " checked below
      WHEN OTHERS.
        " A radio button or Enter: nothing to complain about yet.
        RETURN.
    ENDCASE.

    IF p_crt = abap_true.
      IF p_ktogr IS INITIAL.
        MESSAGE 'Creation needs an account group' TYPE 'E'.
      ENDIF.
      IF lcl_cfg=>ok_ktogr( p_ktogr ) = abap_false.
        MESSAGE |Account group { p_ktogr } does not exist| TYPE 'E'.
      ENDIF.
      IF p_bpgrp IS INITIAL AND lcl_cfg=>bp_group( p_ktogr ) IS INITIAL AND p_up = abap_true.
        MESSAGE |Account group { p_ktogr } has no business partner grouping in CVI customising - give one| TYPE 'E'.
      ENDIF.
    ENDIF.
    IF p_down = abap_true AND s_fld[] IS INITIAL.
      MESSAGE 'Choose the fields for the file first' TYPE 'E'.
    ENDIF.
    IF p_file IS INITIAL.
      MESSAGE 'Give a file name - F4 opens the file dialog' TYPE 'E'.
    ENDIF.
    DATA(lv_name) = to_upper( CONV string( p_file ) ).
    IF p_xlsx = abap_true AND lv_name NP '*.XLSX'.
      MESSAGE 'Excel chosen - the file name must end in .xlsx' TYPE 'E'.
    ELSEIF p_txt = abap_true AND lv_name NP '*.TXT'.
      MESSAGE 'Text chosen - the file name must end in .txt' TYPE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD run.
    IF p_down = abap_true.
      lcl_dl=>run( ).
    ELSE.
      lcl_ul=>run( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Events
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_app=>init( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_app=>output( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_file=>f4( ).

AT SELECTION-SCREEN.
  lcl_app=>pai( ).

START-OF-SELECTION.
  lcl_app=>run( ).
