*&---------------------------------------------------------------------*
*& Report  ZSDS_CUST_TMPL_DOWNLOAD
*&---------------------------------------------------------------------*
*& Customer master templates, in both directions: DOWNLOAD writes a
*& template - empty, or filled with the data of the customers asked for -
*& and UPLOAD reads a filled one back and creates or changes the customers
*& in it through the business partner interface.
*&
*& LSMW is gone in S/4HANA - XD01 no longer exists and the customer is
*& maintained as a business partner - so the templates Cipla used to load
*& through it are now loaded here.
*&
*& Why one program and not two: the two directions read the same column
*& map. The download writes a template's heading row from it; the upload
*& reads the same rows to decide which column feeds which field, through
*& the conversion the map carries (CNV) - a date, a number, a language key,
*& leading zeros. With one map a file this program writes is a file it can
*& read, by construction. tools/cipla/sim_roundtrip.py walks all 1986
*& columns of all 24 templates to prove it.
*&
*& Pick a region and an account group and the template follows: the
*& workbook Cipla supplied holds 24 layouts over 79 region and account
*& group combinations, and a region and an account group name exactly one
*& of them. It is a region and not a country because the two do not line
*& up - Europe is four countries on one template, and the United States is
*& two entities, Exelan and Invagen, with account groups in common.
*&
*& The upload replaces ZSDS_CUST_MASS_UPLOAD, which is retired. That
*& program's seven hand-written layouts belong to the earlier LSMW workbook
*& and none of them matches a current template. Its one other tab, the
*& FSCM credit limit, is not carried over: Cipla has not asked for credit
*& limits to be loaded, and none of the 24 templates carries them.
*&
*& The data is read through the same interface the upload program writes
*& through - CMD_EI_API_EXTRACT=>GET_DATA - so a column that can be loaded
*& is a column that can be read back, in the same structures. Two things
*& the interface does not carry are read directly, as the upload program
*& writes them directly: ZSD_LICENSE_CHK for the licence, bank guarantee
*& and routing data, and BUT0ID for the Aadhaar number. A download changes
*& nothing. An upload writes through CL_MD_BP_MAINTAIN, with the one
*& authorised direct write the earlier upload also made - MODIFY on
*& ZSD_LICENSE_CHK, which has no API - and checks F_KNA1_BUK itself first,
*& because one transaction cannot ask S_TCODE a read question and a change
*& question at once.
*&
*& The workbook is written as a real .xlsx - a zip of OpenXML parts built
*& with CL_ABAP_ZIP - with every text cell pointing into the shared string
*& table and column A always written, because that is what
*& CL_FDT_XL_SPREADSHEET needs to read it back.
*&
*& Naming convention
*&   Z<MODULE>_ pattern from Cipla_Checklist Part 1.1.
*&
*& S/4HANA
*&   Nothing here is obsolete in S/4HANA. The program reads the customer
*&   through CMD_EI_API_EXTRACT, which is the interface the customer is
*&   maintained through as a business partner, and the tables it reads
*&   besides - BUT000, BUT0ID and CVI_CUST_LINK for the partner, KNVK for
*&   the contact person, T005T, T077X and TSAD3T for texts - are all
*&   current. The program writes nothing at all: no INSERT, no UPDATE, no
*&   MODIFY, no COMMIT, no BAPI, no CALL TRANSACTION and no batch input.
*&
*&   XD01 and XD05 are obsolete in S/4HANA - a customer is maintained as a
*&   business partner - and they appear here only as the contents of a
*&   template column, never as a call. The transaction code column is taken
*&   from each template's own sample row, so it says what the template says.
*&   The block and unblock template has no such column at all; its blocking
*&   flags are read from the general data, the company code and the sales
*&   area, which is where a business partner keeps them.
*&
*& Clean core positioning (S/4HANA 2502 / ABAP Cloud)
*&   Deliberately TIER 2. CMD_EI_API_EXTRACT and CL_MD_BP_MAINTAIN are
*&   "Not released", and the released alternatives (the CDS views behind
*&   I_Customer, the Business Partner RAP BO) do not carry the licence
*&   record or the identification numbers the templates need.
*&   CL_GUI_FRONTEND_SERVICES is classic GUI only, which is what a file on
*&   a user's PC means. Every read is confined to LCL_SRC, so it can be
*&   swapped for released CDS views without touching the engine.
*&   Run ATC with variant ABAP_CLOUD_READINESS and record those exemptions
*&   and the one direct write, MODIFY ZSD_LICENSE_CHK. A download changes
*&   no data at all.
*&
*& The column map below is generated from the template workbook by
*& tools/cipla/gen_download_program.py, so a change to a template is a
*& regeneration rather than an edit.
*&---------------------------------------------------------------------*
REPORT zsds_cust_tmpl_download.

" One row of a sheet, in either direction. The download builds these to
" write; the upload reads them, and needs the Excel row number to report
" against. Both halves hand CELLS to by-reference parameters - the writer
" typed TT_CELL, the reader typed STRING_TABLE - and a by-reference
" parameter takes nothing but its own type, so TT_CELL IS STRING_TABLE.
TYPES: tt_cell TYPE string_table.

TYPES: BEGIN OF ty_row,
         row   TYPE i,
         cells TYPE tt_cell,
       END OF ty_row,
       tt_row TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

" One line of the upload's result list: which row, which customer, which
" field, and what SAP or the program said about it. The download keeps a
" record of its own (TY_MSG), much slimmer, for what it read.
TYPES: BEGIN OF ty_ulog,
         icon    TYPE icon_d,
         xlsrow  TYPE i,
         kunnr   TYPE char16,
         bukrs   TYPE char4,
         vkorg   TYPE char4,
         msgty   TYPE bapi_mtype,
         msgid   TYPE symsgid,
         msgno   TYPE symsgno,
         struc   TYPE char30,
         fldnm   TYPE char30,
         message TYPE bapi_msg,
       END OF ty_ulog,
       tt_ulog TYPE STANDARD TABLE OF ty_ulog WITH EMPTY KEY.

" RETURNING parameters must be fully typed, so the packed type used for
" amounts, rates and day counts read from the file is declared here.
TYPES ty_dec TYPE p LENGTH 15 DECIMALS 2.

" One line per column of one template: where it sits, what it is called,
" which part of the master record holds it, and how to write it.
" One column of one template, read in both directions. FMT says how a stored
" value is written into the file; CNV says how a cell is read back out of it.
" They differ because writing only has to undo the exits - leading zeros, the
" title key - while reading has to turn a cell into a date, a number or a
" language key as well.
TYPES: BEGIN OF ty_col,
         tmpl TYPE char8,
         col  TYPE i,
         hdr  TYPE char60,
         node TYPE char1,
         fld  TYPE char30,
         fmt  TYPE char2,
         cnv  TYPE char2,
       END OF ty_col,
       tt_col TYPE STANDARD TABLE OF ty_col WITH EMPTY KEY.

" Which template a region and an account group resolve to.
"
" The workbook is organised by REGION, not by country, and the two do not
" line up. Europe is one template for four countries; the United States has
" two entities, Exelan and Invagen, sharing three account groups between
" them. A country and an account group therefore do not name one template,
" which is why the screen asks for the region. The country is carried along
" for the file name and the heading - it is derived, never typed.
TYPES: BEGIN OF ty_combi,
         regn  TYPE char2,
         land  TYPE land1,
         ktokd TYPE ktokd,
         tmpl  TYPE char8,
       END OF ty_combi,
       tt_combi TYPE STANDARD TABLE OF ty_combi WITH EMPTY KEY.

" The regions the dropdown offers, each with its countries in brackets.
TYPES: BEGIN OF ty_regn,
         regn TYPE char2,
         text TYPE char40,
       END OF ty_regn,
       tt_regn TYPE STANDARD TABLE OF ty_regn WITH EMPTY KEY.

" The file path. RLGRAP-FILENAME, which this parameter used to have, is
" CHAR 128, and the file dialog hands the chosen path back as a STRING. A
" path longer than 128 characters - which a OneDrive or Teams synchronised
" folder reaches on its own - was cut short on the way into the parameter,
" taking the ".xlsx" at the end of it with it. 255 is the widest a screen
" field goes.
TYPES ty_path TYPE c LENGTH 255.

TYPES: tt_land  TYPE STANDARD TABLE OF land1 WITH EMPTY KEY,
       tt_ktokd TYPE STANDARD TABLE OF ktokd WITH EMPTY KEY.

TYPES: BEGIN OF ty_msg,
         icon    TYPE icon_d,
         objkey  TYPE char20,
         message TYPE string,
       END OF ty_msg,
       tt_msg TYPE STANDARD TABLE OF ty_msg WITH EMPTY KEY.

" The identification category the customer programs keep the Aadhaar
" number in. Created by Cipla, not delivered by SAP.
CONSTANTS gc_id_aadhaar TYPE bu_id_type VALUE 'X90003'.

" The task the extract interface expects on a read request. It is not the
" task of a change - the maintain interface takes I or U - it is what
" tells the extractor which record to assemble.
CONSTANTS gc_task_read TYPE cmd_ei_object_task VALUE 'M'.

" Which node of the customer record a column belongs to. The same letters
" the map carries, read now in both directions.
CONSTANTS:
  gc_n_key  TYPE char1 VALUE 'K',
  gc_n_addr TYPE char1 VALUE 'A',
  gc_n_comm TYPE char1 VALUE 'M',
  gc_n_cent TYPE char1 VALUE 'C',
  gc_n_comp TYPE char1 VALUE 'B',
  gc_n_sale TYPE char1 VALUE 'S',
  gc_n_tax  TYPE char1 VALUE 'T',
  gc_n_lic  TYPE char1 VALUE 'Z',
  gc_n_iden TYPE char1 VALUE 'I',
  gc_n_cont TYPE char1 VALUE 'P',
  gc_n_const TYPE char1 VALUE 'X'.

CONSTANTS:
  gc_i     TYPE cmd_ei_object_task VALUE 'I',   " insert
  gc_u     TYPE cmd_ei_object_task VALUE 'U',   " update
  gc_clear TYPE string             VALUE '#BLANK#'.

CONSTANTS:
  gc_role_fi TYPE bu_role VALUE 'FLCU00',
  gc_role_sd TYPE bu_role VALUE 'FLCU01',
  gc_org     TYPE bu_type VALUE '2'.

" What R_3_USER holds for a mobile number. Not a flag: SAP reads space and
" 1 as a landline, 2 and 3 as a mobile, and brings the run down on anything
" else.
CONSTANTS gc_mobile TYPE c LENGTH 1 VALUE '3'.

" The two templates that do not belong to a country: the extension
" template and the XD05 block / unblock template.
CONSTANTS: gc_any       TYPE land1 VALUE '*',
           gc_tmpl_extn TYPE char8 VALUE 'EXTN',
           gc_tmpl_blk  TYPE char8 VALUE 'BLOCK'.

TABLES sscrfields.

DATA: gv_bp    TYPE bu_partner,
      gv_kunnr TYPE kunnr,
      gv_key   TYPE string.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
" Download writes the template, upload reads a filled one back. One map
" serves both, which is the point of doing them in one program: a file this
" program writes is a file it can read, by construction rather than by
" agreement between two maps.
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-006.
PARAMETERS: p_down RADIOBUTTON GROUP g0 USER-COMMAND md DEFAULT 'X',
            p_up   RADIOBUTTON GROUP g0.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_crt RADIOBUTTON GROUP g1 USER-COMMAND rb DEFAULT 'X',
            p_ext RADIOBUTTON GROUP g1,
            p_blk RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
" A dropdown, not a free text country key. Half the workbook's sheets are
" named for something that is not a country - Dubai is a city, Europe is
" four countries, SAGA and QCIL are entities - so a user who had to type a
" country key had to know it, and the ones who did not got "Country key not
" found" from the dynpro before the program was ever reached.
PARAMETERS: p_regn  TYPE char2 AS LISTBOX VISIBLE LENGTH 40
                    USER-COMMAND rg,
            p_ktokd TYPE ktokd.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS: s_bp    FOR gv_bp    NO INTERVALS,
                s_kunnr FOR gv_kunnr NO INTERVALS.
PARAMETERS:     p_max   TYPE i DEFAULT 100.
SELECTION-SCREEN END OF BLOCK b3.

" What an upload run does with what it reads. Ignored on a download.
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-005.
PARAMETERS: p_test  AS CHECKBOX DEFAULT 'X',
            p_stop  AS CHECKBOX,
            p_bpgrp TYPE bu_group,
            p_skip  TYPE i DEFAULT 1.
SELECTION-SCREEN END OF BLOCK b5.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
PARAMETERS: p_file  TYPE ty_path LOWER CASE,
            p_pc    RADIOBUTTON GROUP g2 DEFAULT 'X',
            p_srv   RADIOBUTTON GROUP g2,
            p_empty AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b4.

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

  PUBLIC SECTION.
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

    " Conversion exits live on the DOMAIN, not on the field, so they are
    " applied here by target field rather than read from DD03L-CONROUT.
    "   KUNNR / LIFNR / VBUND -> ALPHA        AKONT -> ALPHA (SAKNR)
    "   FDGRV -> ALPHA (domain FDGRP)
    " IV_LEN is the length of the field the value is going into. Without it
    " nothing is padded - see the comment in the implementation.
    " The character length of a field, or 0 when it has none. DESCRIBE
    " FIELD ... IN CHARACTER MODE only takes a character-like operand: a
    " packed field such as KNVV-ANTLF, an integer, or a STRING terminates
    " the program with OBJECTS_NOT_CHAR, and a dynamically assigned field
    " symbol can be any of those.
    CLASS-METHODS char_len
      IMPORTING iv_any    TYPE any
      RETURNING VALUE(rv) TYPE i.

    CLASS-METHODS alpha
      IMPORTING iv_in     TYPE string
                iv_len    TYPE i DEFAULT 0
      RETURNING VALUE(rv) TYPE string.

    " A language. The templates carry the two letter ISO code - EN, ES, NL -
    " and SAP's own language key is one character, which is NOT the first
    " letter of the ISO code: Spanish is ES but S, Swedish SV but V, Danish
    " DA but K. Cutting the code to one character therefore files a Spanish
    " address under English and a Swedish one under Spanish, without a word.
    " Domain SPRAS carries conversion exit ISOLA for exactly this.
    " An unknown code comes back empty so the caller can say so.
    CLASS-METHODS lang
      IMPORTING iv_in     TYPE string
      RETURNING VALUE(rv) TYPE spras.

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

  METHOD char_len.
    rv = 0.
    DATA(lv_kind) = cl_abap_typedescr=>describe_by_data( iv_any )->type_kind.
    IF lv_kind = cl_abap_typedescr=>typekind_char
    OR lv_kind = cl_abap_typedescr=>typekind_num
    OR lv_kind = cl_abap_typedescr=>typekind_date
    OR lv_kind = cl_abap_typedescr=>typekind_time.
      DESCRIBE FIELD iv_any LENGTH rv IN CHARACTER MODE.
    ENDIF.
  ENDMETHOD.

  METHOD lang.
    CLEAR rv.
    DATA(lv) = to_upper( condense( iv_in ) ).
    IF lv IS INITIAL.
      RETURN.
    ENDIF.

    " One character is already the internal key - a file downloaded from
    " this system carries it that way.
    IF strlen( lv ) = 1.
      rv = lv.
      RETURN.
    ENDIF.

    DATA lv_out TYPE spras.
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
      EXPORTING  input            = lv
      IMPORTING  output           = lv_out
      EXCEPTIONS unknown_language = 1
                 OTHERS           = 2.
    IF sy-subrc = 0.
      rv = lv_out.
    ENDIF.
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

*----------------------------------------------------------------------*
* LCL_TMPL - the templates, and which combination uses which
*   Generated from "customer code templates.xlsx". The workbook is LSMW
*   shaped: every sheet is a country or a legal entity and the templates
*   are stacked inside it, one per account group. 61 blocks collapse to
*   24 distinct column lists over 81 combinations, and a country with an
*   account group names exactly one of them.
*----------------------------------------------------------------------*
CLASS lcl_tmpl DEFINITION FINAL.
  PUBLIC SECTION.
    " The template a region and an account group resolve to, empty when the
    " workbook does not cover the combination.
    CLASS-METHODS resolve
      IMPORTING iv_regn   TYPE char2
                iv_ktokd  TYPE ktokd
      RETURNING VALUE(rv) TYPE char8.

    " The country a region stands for - the first of them where a region
    " covers several, which is only Europe. Used for the file name and the
    " heading; nothing is selected by it.
    CLASS-METHODS land_of
      IMPORTING iv_regn   TYPE char2
      RETURNING VALUE(rv) TYPE land1.

    " Every region the workbook holds, in the order the dropdown shows them.
    CLASS-METHODS regions RETURNING VALUE(rt) TYPE tt_regn.
    CLASS-METHODS region_text
      IMPORTING iv_regn   TYPE char2
      RETURNING VALUE(rv) TYPE char40.

    CLASS-METHODS cols
      IMPORTING iv_tmpl   TYPE char8
      RETURNING VALUE(rt) TYPE tt_col.

    " Every account group the workbook covers for one region - what the F4 on
    " the selection screen offers once a region is chosen.
    CLASS-METHODS groups
      IMPORTING iv_regn   TYPE char2
      RETURNING VALUE(rt) TYPE tt_ktokd.

  PRIVATE SECTION.
    CLASS-DATA mt_col   TYPE tt_col.
    CLASS-DATA mt_combi TYPE tt_combi.
    CLASS-DATA mt_regn  TYPE tt_regn.
    CLASS-METHODS load.
    CLASS-METHODS map_1 RETURNING VALUE(rt) TYPE tt_col.
    CLASS-METHODS map_2 RETURNING VALUE(rt) TYPE tt_col.
    CLASS-METHODS map_3 RETURNING VALUE(rt) TYPE tt_col.
    CLASS-METHODS map_4 RETURNING VALUE(rt) TYPE tt_col.
    CLASS-METHODS map_5 RETURNING VALUE(rt) TYPE tt_col.
    CLASS-METHODS map_6 RETURNING VALUE(rt) TYPE tt_col.
ENDCLASS.

CLASS lcl_tmpl IMPLEMENTATION.

  METHOD load.
    IF mt_col IS NOT INITIAL.
      RETURN.
    ENDIF.
    APPEND LINES OF map_1( ) TO mt_col.
    APPEND LINES OF map_2( ) TO mt_col.
    APPEND LINES OF map_3( ) TO mt_col.
    APPEND LINES OF map_4( ) TO mt_col.
    APPEND LINES OF map_5( ) TO mt_col.
    APPEND LINES OF map_6( ) TO mt_col.
    mt_combi = VALUE tt_combi(
      ( regn = 'AE' land = 'AE' ktokd = 'ZEXP' tmpl = 'ab38ead5' )
      ( regn = 'AE' land = 'AE' ktokd = 'ZSHP' tmpl = '80d23dd8' )
      ( regn = 'AU' land = 'AU' ktokd = 'ZCDP' tmpl = '9c914100' )
      ( regn = 'AU' land = 'AU' ktokd = 'ZDOM' tmpl = 'f7e2b95a' )
      ( regn = 'AU' land = 'AU' ktokd = 'ZEXP' tmpl = 'ab38ead5' )
      ( regn = 'AU' land = 'AU' ktokd = 'ZPLN' tmpl = '42895e0e' )
      ( regn = 'AU' land = 'AU' ktokd = 'ZSHP' tmpl = '8a74041a' )
      ( regn = 'EU' land = 'BE' ktokd = 'YSHP' tmpl = '43156406' )
      ( regn = 'EU' land = 'ES' ktokd = 'YSHP' tmpl = '43156406' )
      ( regn = 'EU' land = 'GB' ktokd = 'YSHP' tmpl = '43156406' )
      ( regn = 'EU' land = 'NL' ktokd = 'YSHP' tmpl = '43156406' )
      ( regn = 'EU' land = 'BE' ktokd = 'ZCDP' tmpl = '9c914100' )
      ( regn = 'EU' land = 'ES' ktokd = 'ZCDP' tmpl = '9c914100' )
      ( regn = 'EU' land = 'GB' ktokd = 'ZCDP' tmpl = '9c914100' )
      ( regn = 'EU' land = 'NL' ktokd = 'ZCDP' tmpl = '9c914100' )
      ( regn = 'EU' land = 'BE' ktokd = 'ZDOM' tmpl = 'e10ec770' )
      ( regn = 'EU' land = 'ES' ktokd = 'ZDOM' tmpl = 'e10ec770' )
      ( regn = 'EU' land = 'GB' ktokd = 'ZDOM' tmpl = 'e10ec770' )
      ( regn = 'EU' land = 'NL' ktokd = 'ZDOM' tmpl = 'e10ec770' )
      ( regn = 'EU' land = 'BE' ktokd = 'ZEXP' tmpl = 'ab38ead5' )
      ( regn = 'EU' land = 'ES' ktokd = 'ZEXP' tmpl = 'ab38ead5' )
      ( regn = 'EU' land = 'GB' ktokd = 'ZEXP' tmpl = 'ab38ead5' )
      ( regn = 'EU' land = 'NL' ktokd = 'ZEXP' tmpl = 'ab38ead5' )
      ( regn = 'EU' land = 'BE' ktokd = 'ZPLN' tmpl = '42895e0e' )
      ( regn = 'EU' land = 'ES' ktokd = 'ZPLN' tmpl = '42895e0e' )
      ( regn = 'EU' land = 'GB' ktokd = 'ZPLN' tmpl = '42895e0e' )
      ( regn = 'EU' land = 'NL' ktokd = 'ZPLN' tmpl = '42895e0e' )
      ( regn = 'EU' land = 'BE' ktokd = 'ZSHP' tmpl = '8a74041a' )
      ( regn = 'EU' land = 'ES' ktokd = 'ZSHP' tmpl = '8a74041a' )
      ( regn = 'EU' land = 'GB' ktokd = 'ZSHP' tmpl = '8a74041a' )
      ( regn = 'EU' land = 'NL' ktokd = 'ZSHP' tmpl = '8a74041a' )
      ( regn = 'EX' land = 'US' ktokd = 'YSHP' tmpl = '43156406' )
      ( regn = 'EX' land = 'US' ktokd = 'YVMI' tmpl = '6e6467eb' )
      ( regn = 'EX' land = 'US' ktokd = 'YVSP' tmpl = '6e6467eb' )
      ( regn = 'EX' land = 'US' ktokd = 'YVTO' tmpl = '6e6467eb' )
      ( regn = 'EX' land = 'US' ktokd = 'ZCDP' tmpl = '9c914100' )
      ( regn = 'EX' land = 'US' ktokd = 'ZPLN' tmpl = '42895e0e' )
      ( regn = 'IN' land = 'IN' ktokd = 'ZBMR' tmpl = '647cd8ca' )
      ( regn = 'IN' land = 'IN' ktokd = 'ZCDP' tmpl = '9c914100' )
      ( regn = 'IN' land = 'IN' ktokd = 'ZDOC' tmpl = '1f487b03' )
      ( regn = 'IN' land = 'IN' ktokd = 'ZDOD' tmpl = '1f487b03' )
      ( regn = 'IN' land = 'IN' ktokd = 'ZDOF' tmpl = '1f487b03' )
      ( regn = 'IN' land = 'IN' ktokd = 'ZDOM' tmpl = 'fb40b09b' )
      ( regn = 'IN' land = 'IN' ktokd = 'ZEXP' tmpl = 'da47e4b3' )
      ( regn = 'IN' land = 'IN' ktokd = 'ZMPC' tmpl = '42895e0e' )
      ( regn = 'IN' land = 'IN' ktokd = 'ZNOT' tmpl = '84513e29' )
      ( regn = 'IN' land = 'IN' ktokd = 'ZOTC' tmpl = '42895e0e' )
      ( regn = 'IN' land = 'IN' ktokd = 'ZPLN' tmpl = '42895e0e' )
      ( regn = 'IN' land = 'IN' ktokd = 'ZPY1' tmpl = '32831d38' )
      ( regn = 'IN' land = 'IN' ktokd = 'ZREM' tmpl = '06551ad0' )
      ( regn = 'IN' land = 'IN' ktokd = 'ZSHM' tmpl = '67849f38' )
      ( regn = 'IN' land = 'IN' ktokd = 'ZSHP' tmpl = '6c94ea65' )
      ( regn = 'IN' land = 'IN' ktokd = 'ZSUB' tmpl = 'f10a66f5' )
      ( regn = 'IV' land = 'US' ktokd = 'YVSP' tmpl = '6e6467eb' )
      ( regn = 'IV' land = 'US' ktokd = 'ZCDP' tmpl = '9c914100' )
      ( regn = 'IV' land = 'US' ktokd = 'ZDOM' tmpl = '6d2ec22f' )
      ( regn = 'IV' land = 'US' ktokd = 'ZEXP' tmpl = 'ab38ead5' )
      ( regn = 'IV' land = 'US' ktokd = 'ZPLN' tmpl = '42895e0e' )
      ( regn = 'IV' land = 'US' ktokd = 'ZSHP' tmpl = '80d23dd8' )
      ( regn = 'KE' land = 'KE' ktokd = 'YDOM' tmpl = '08585a5a' )
      ( regn = 'KE' land = 'KE' ktokd = 'YVTO' tmpl = '6e6467eb' )
      ( regn = 'KE' land = 'KE' ktokd = 'ZEXP' tmpl = 'ab38ead5' )
      ( regn = 'MA' land = 'MA' ktokd = 'ZCDP' tmpl = '9c914100' )
      ( regn = 'MA' land = 'MA' ktokd = 'ZDOM' tmpl = 'f7e2b95a' )
      ( regn = 'MA' land = 'MA' ktokd = 'ZOTC' tmpl = '9c914100' )
      ( regn = 'MA' land = 'MA' ktokd = 'ZPLN' tmpl = '42895e0e' )
      ( regn = 'UG' land = 'UG' ktokd = 'YSHP' tmpl = '43156406' )
      ( regn = 'UG' land = 'UG' ktokd = 'ZCDP' tmpl = '9c914100' )
      ( regn = 'UG' land = 'UG' ktokd = 'ZDOM' tmpl = 'd7ee33bb' )
      ( regn = 'UG' land = 'UG' ktokd = 'ZEXP' tmpl = 'd7ee33bb' )
      ( regn = 'UG' land = 'UG' ktokd = 'ZOTC' tmpl = '9c914100' )
      ( regn = 'UG' land = 'UG' ktokd = 'ZSHP' tmpl = '8a74041a' )
      ( regn = 'UG' land = 'UG' ktokd = 'ZSUB' tmpl = 'f10a66f5' )
      ( regn = 'ZA' land = 'ZA' ktokd = 'YDOM' tmpl = '08585a5a' )
      ( regn = 'ZA' land = 'ZA' ktokd = 'YINT' tmpl = '08585a5a' )
      ( regn = 'ZA' land = 'ZA' ktokd = 'YTDR' tmpl = '08585a5a' )
      ( regn = 'ZA' land = 'ZA' ktokd = 'ZDOM' tmpl = 'e10ec770' )
      ( regn = 'ZA' land = 'ZA' ktokd = 'ZEXP' tmpl = 'ab38ead5' )
      ( regn = 'ZA' land = 'ZA' ktokd = 'ZPLN' tmpl = '42895e0e' )
    ).

    mt_regn = VALUE tt_regn(
      ( regn = 'AU' text = 'Australia (AU)' )
      ( regn = 'AE' text = 'Dubai (AE)' )
      ( regn = 'EU' text = 'Europe (GB/BE/ES/NL)' )
      ( regn = 'EX' text = 'Exelan (US)' )
      ( regn = 'IN' text = 'India (IN)' )
      ( regn = 'IV' text = 'Invagen (US)' )
      ( regn = 'KE' text = 'Kenya (KE)' )
      ( regn = 'MA' text = 'Morocco (MA)' )
      ( regn = 'UG' text = 'QCIL (UG)' )
      ( regn = 'ZA' text = 'SAGA (ZA)' )
    ).
  ENDMETHOD.

  METHOD resolve.
    load( ).
    READ TABLE mt_combi INTO DATA(ls_cb)
         WITH KEY regn = iv_regn ktokd = iv_ktokd.
    IF sy-subrc = 0.
      rv = ls_cb-tmpl.
    ENDIF.
  ENDMETHOD.

  METHOD land_of.
    load( ).
    READ TABLE mt_combi INTO DATA(ls_cb) WITH KEY regn = iv_regn.
    IF sy-subrc = 0.
      rv = ls_cb-land.
    ENDIF.
  ENDMETHOD.

  METHOD regions.
    load( ).
    rt = mt_regn.
  ENDMETHOD.

  METHOD region_text.
    load( ).
    READ TABLE mt_regn INTO DATA(ls_rg) WITH KEY regn = iv_regn.
    IF sy-subrc = 0.
      rv = ls_rg-text.
    ENDIF.
  ENDMETHOD.

  METHOD cols.
    load( ).
    LOOP AT mt_col INTO DATA(ls_col) WHERE tmpl = iv_tmpl.
      APPEND ls_col TO rt.
    ENDLOOP.
    SORT rt BY col.
  ENDMETHOD.


  METHOD groups.
    load( ).
    LOOP AT mt_combi INTO DATA(ls_cb) WHERE regn = iv_regn.
      READ TABLE rt TRANSPORTING NO FIELDS WITH KEY table_line = ls_cb-ktokd.
      IF sy-subrc <> 0.
        APPEND ls_cb-ktokd TO rt.
      ENDIF.
    ENDLOOP.
    SORT rt.
  ENDMETHOD.

  METHOD map_1.
    rt = VALUE tt_col(
    "  06551ad0 - 64 columns - IN/ZREM
      ( tmpl = '06551ad0' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 2   hdr = 'Ref Customer Code  as sample' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '06551ad0' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 8   hdr = 'Number of contact person' node = 'P' fld = 'PARNR' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 9   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 10  hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 11  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 12  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 13  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 14  hdr = 'aLWAYS x' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 15  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = '06551ad0' col = 16  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 17  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 18  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 19  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 20  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 21  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 22  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 23  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 24  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 25  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 26  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 27  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 28  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 29  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 30  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 31  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 32  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 33  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 34  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = '06551ad0' col = 35  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 36  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 37  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 38  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 39  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 40  hdr = 'Exch. Rate Type M' node = 'S' fld = 'KURST' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 41  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 42  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 43  hdr = 'Price List' node = 'S' fld = 'PLTYP' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 44  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 45  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 46  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 47  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 48  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 49  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = '06551ad0' col = 50  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 51  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 52  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 53  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 54  hdr = 'JOIG IN:Central GST - OP' node = 'T' fld = 'JOCG' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 55  hdr = 'JTC1 IN: 206C(1H) Goods' node = 'T' fld = 'JTC1' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 56  hdr = 'JTX1 Tax Jurisdict.Code d' node = 'T' fld = 'JTX1' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 57  hdr = 'JTX2 Tax Jurisdict.Code d' node = 'T' fld = 'JTX2' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 58  hdr = 'JTX3 Tax Jurisdict.Code d' node = 'T' fld = 'JTX3' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 59  hdr = 'JTX4 Tax Jurisdict.Code d' node = 'T' fld = 'JTX4' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 60  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 61  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 62  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 63  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = '06551ad0' col = 64  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
    "  08585a5a - 83 columns - KE/YDOM, ZA/YDOM, ZA/YINT, ZA/YTDR
      ( tmpl = '08585a5a' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 2   hdr = 'Customer number' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '08585a5a' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 8   hdr = 'Always X' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 9   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = '08585a5a' col = 10  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 11  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 12  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 13  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 14  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 15  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 16  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 17  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 18  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 19  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 20  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 21  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 22  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 23  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 24  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 25  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = '08585a5a' col = 26  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 27  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 28  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 29  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 30  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 31  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '08585a5a' col = 32  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '08585a5a' col = 33  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 34  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 35  hdr = 'VAT Registration Number' node = 'C' fld = 'STCEG' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 36  hdr = 'Tax Number 5' node = 'C' fld = 'STCD5' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 37  hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 38  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' cnv = 'GL' )
      ( tmpl = '08585a5a' col = 39  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 40  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '08585a5a' col = 41  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 42  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 43  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 44  hdr = 'Memo' node = 'B' fld = 'KVERM' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 45  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 46  hdr = 'Order probability of the item' node = 'S' fld = 'AWAHR' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 47  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 48  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 49  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 50  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 51  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 52  hdr = 'Exchange Rate Type' node = 'S' fld = 'KURST' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 53  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 54  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 55  hdr = 'Price list type' node = 'S' fld = 'PLTYP' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 56  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 57  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 58  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 59  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 60  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 61  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = '08585a5a' col = 62  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 63  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 64  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 65  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 66  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 67  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 68  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 69  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 70  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 71  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 72  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 73  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 74  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' cnv = 'NM' )
      ( tmpl = '08585a5a' col = 75  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 76  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 77  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '08585a5a' col = 78  hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' cnv = 'DT' )
      ( tmpl = '08585a5a' col = 79  hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' cnv = '' )
      ( tmpl = '08585a5a' col = 80  hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '08585a5a' col = 81  hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '08585a5a' col = 82  hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '08585a5a' col = 83  hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' cnv = 'DT' )
    "  1f487b03 - 136 columns - IN/ZDOC, IN/ZDOD, IN/ZDOF
      ( tmpl = '1f487b03' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 2   hdr = 'New Customer Code' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '1f487b03' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 9   hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 10  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 11  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 12  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 13  hdr = 'ALWAYS X' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = '1f487b03' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 25  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 26  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 27  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 28  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 29  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 30  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 31  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 32  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 33  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = '1f487b03' col = 34  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 35  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 36  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 37  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 38  hdr = 'Attribute 1' node = 'C' fld = 'KATR1' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 39  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 40  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 41  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '1f487b03' col = 42  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '1f487b03' col = 43  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 44  hdr = 'Tax Number 3 ( GST Number)' node = 'C' fld = 'STCD3' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 45  hdr = 'Permanent Account Number' node = 'C' fld = 'J_1IPANNO' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 46  hdr = 'GST TDS Registration' node = 'C' fld = 'GST_TDS' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 47  hdr = 'Aadhaar Number' node = 'I' fld = 'X90003' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 48  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' cnv = 'GL' )
      ( tmpl = '1f487b03' col = 49  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 50  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '1f487b03' col = 51  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 52  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 53  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '1f487b03' col = 54  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 55  hdr = 'Tolerance group for the business partner/G/L account' node = 'B' fld = 'TOGRU' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 56  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 57  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 58  hdr = 'Block Key for Payment' node = 'B' fld = 'ZAHLS' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 59  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 60  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 61  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 62  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 63  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 64  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 65  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 66  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 67  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 68  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 69  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 70  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 71  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 72  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = '1f487b03' col = 73  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 74  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 75  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 76  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 77  hdr = 'JOIG IN:Central GST - OP' node = 'T' fld = 'JOCG' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 78  hdr = 'JTC1 IN: 206C(1H) Goods' node = 'T' fld = 'JTC1' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 79  hdr = 'JTX1 Tax Jurisdict.Code d' node = 'T' fld = 'JTX1' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 80  hdr = 'JTX2 Tax Jurisdict.Code d' node = 'T' fld = 'JTX2' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 81  hdr = 'JTX3 Tax Jurisdict.Code d' node = 'T' fld = 'JTX3' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 82  hdr = 'JTX4 Tax Jurisdict.Code d' node = 'T' fld = 'JTX4' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 83  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 84  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 85  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 86  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 87  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 88  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 89  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 90  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' cnv = 'NM' )
      ( tmpl = '1f487b03' col = 91  hdr = '20B. Lic. No' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 92  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 93  hdr = '21B. Lic. No' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 94  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 95  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '1f487b03' col = 96  hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 97  hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '1f487b03' col = 98  hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 99  hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '1f487b03' col = 100 hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 101 hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '1f487b03' col = 102 hdr = 'Retails Lic No (20 and 21 )' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 103 hdr = 'SC_EXEMPT' node = 'Z' fld = 'SC_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 104 hdr = 'Retails Lic Exp date' node = 'Z' fld = 'RETAIL_EXP' fmt = '' cnv = 'DT' )
      ( tmpl = '1f487b03' col = 105 hdr = 'Mfg License (Gen) Number' node = 'Z' fld = 'MFGLIC1NO' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 106 hdr = 'Mfg License (Nar) Number' node = 'Z' fld = 'MFGLIC2NO' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 107 hdr = 'Mfg License (CC) Number' node = 'Z' fld = 'MFGLIC3NO' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 108 hdr = 'Bank Guarantee(Y/N)' node = 'Z' fld = 'BGYN' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 109 hdr = 'Bank Guarantee No' node = 'Z' fld = 'BG_NO' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 110 hdr = 'BG Amount' node = 'Z' fld = 'BG_AMT' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 111 hdr = 'SD Document Currency' node = 'Z' fld = 'CURRENCY' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 112 hdr = 'BG Issue Date' node = 'Z' fld = 'BG_ISS_DT' fmt = '' cnv = 'DT' )
      ( tmpl = '1f487b03' col = 113 hdr = 'BG Expiry Date' node = 'Z' fld = 'BG_EXP_DT' fmt = '' cnv = 'DT' )
      ( tmpl = '1f487b03' col = 114 hdr = 'BG Issuing Bank' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 115 hdr = 'Agreement Expiry Date' node = 'Z' fld = 'AGGR_EXPDT' fmt = '' cnv = 'DT' )
      ( tmpl = '1f487b03' col = 116 hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' cnv = 'DT' )
      ( tmpl = '1f487b03' col = 117 hdr = 'Customer group' node = 'Z' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 118 hdr = 'AIOCD Code' node = 'Z' fld = 'AIOCD_CODE' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 119 hdr = 'Customer Bank Name' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 120 hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 121 hdr = 'Route Code' node = 'Z' fld = 'ZTROUT' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 122 hdr = 'Extension' node = 'Z' fld = 'EXTENSION' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 123 hdr = 'Route' node = 'Z' fld = 'ZCROUT' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 124 hdr = 'GLN URI Format' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 125 hdr = 'DUNS_Number' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 126 hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '1f487b03' col = 127 hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '1f487b03' col = 128 hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '1f487b03' col = 129 hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '1f487b03' col = 130 hdr = 'Import_License/MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 131 hdr = 'IMPL/MIA_From_Date' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = '' cnv = 'DT' )
      ( tmpl = '1f487b03' col = 132 hdr = 'IMPL/MIA_Valid_Date' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = '' cnv = 'DT' )
      ( tmpl = '1f487b03' col = 133 hdr = 'Check Digit' node = 'Z' fld = 'CHECK_DIGIT' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 134 hdr = 'Global Company Prefix' node = 'Z' fld = 'GLOBAL_COM' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 135 hdr = 'Backorder Days' node = 'Z' fld = 'BO_DAYS' fmt = '' cnv = '' )
      ( tmpl = '1f487b03' col = 136 hdr = 'Location Number' node = 'Z' fld = 'LOCATION_NUMBER' fmt = '' cnv = '' )
    "  28573fa5 - 43 columns - */ZDOM
      ( tmpl = 'EXTN' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 2   hdr = 'Customer Account Number' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'EXTN' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 9   hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 10  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 11  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 12  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 13  hdr = 'Always X' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 14  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 15  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 16  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 17  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 18  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 19  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 20  hdr = 'Pricing procedure assigned to this custome' node = 'S' fld = 'KALKS' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 21  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 22  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 23  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 24  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 25  hdr = 'Maximum Number of Partial Deliveries Allow' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = 'EXTN' col = 26  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 27  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 28  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 29  hdr = 'Tax classification for customer' node = 'T' fld = '#2' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 30  hdr = 'Tax classification for customer' node = 'T' fld = '#3' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 31  hdr = 'Tax classification for customer' node = 'T' fld = '#4' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 32  hdr = 'Tax classification for customer' node = 'T' fld = '#5' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 33  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 34  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 35  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 36  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 37  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 38  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 39  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 40  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' cnv = 'NM' )
      ( tmpl = 'EXTN' col = 41  hdr = '20B. Lic. No' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 42  hdr = '21B. Lic. No' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' cnv = '' )
      ( tmpl = 'EXTN' col = 43  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' cnv = 'DT' )
    "  32831d38 - 88 columns - IN/ZPY1
      ( tmpl = '32831d38' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 2   hdr = 'Ref Customer Code  as sample' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '32831d38' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
    ).
  ENDMETHOD.

  METHOD map_2.
    rt = VALUE tt_col(
      ( tmpl = '32831d38' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 9   hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 10  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 11  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 12  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 13  hdr = 'aLWAYS x' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = '32831d38' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 25  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 26  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 27  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 28  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 29  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 30  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 31  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 32  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 33  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = '32831d38' col = 34  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 35  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 36  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 37  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 38  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '32831d38' col = 39  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '32831d38' col = 40  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 41  hdr = 'Tax Number 3 ( GST Number)' node = 'C' fld = 'STCD3' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 42  hdr = 'Permanent Account Number' node = 'C' fld = 'J_1IPANNO' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 43  hdr = 'GST TDS Registration' node = 'C' fld = 'GST_TDS' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 44  hdr = 'Aadhaar Number' node = 'I' fld = 'X90003' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 45  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' cnv = 'GL' )
      ( tmpl = '32831d38' col = 46  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 47  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '32831d38' col = 48  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 49  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 50  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '32831d38' col = 51  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 52  hdr = 'Tolerance group for the business partner/G/L account' node = 'B' fld = 'TOGRU' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 53  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 54  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 55  hdr = 'Block Key for Payment' node = 'B' fld = 'ZAHLS' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 56  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 57  hdr = 'Order probab.' node = 'S' fld = 'AWAHR' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 58  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 59  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 60  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 61  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 62  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 63  hdr = 'Exch. Rate Type M' node = 'S' fld = 'KURST' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 64  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 65  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 66  hdr = 'Price List' node = 'S' fld = 'PLTYP' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 67  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 68  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 69  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 70  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 71  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 72  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = '32831d38' col = 73  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 74  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 75  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 76  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 77  hdr = 'JOIG IN:Central GST - OP' node = 'T' fld = 'JOCG' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 78  hdr = 'JTC1 IN: 206C(1H) Goods' node = 'T' fld = 'JTC1' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 79  hdr = 'JTX1 Tax Jurisdict.Code d' node = 'T' fld = 'JTX1' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 80  hdr = 'JTX2 Tax Jurisdict.Code d' node = 'T' fld = 'JTX2' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 81  hdr = 'JTX3 Tax Jurisdict.Code d' node = 'T' fld = 'JTX3' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 82  hdr = 'JTX4 Tax Jurisdict.Code d' node = 'T' fld = 'JTX4' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 83  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 84  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 85  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 86  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 87  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = '32831d38' col = 88  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
    "  42895e0e - 139 columns - AU/ZPLN, BE/ZPLN, ES/ZPLN, GB/ZPLN, IN/ZMPC, IN/ZOTC, IN/ZPLN, MA/ZPLN, NL/ZPLN, US/ZPLN, ZA/ZPLN
      ( tmpl = '42895e0e' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 2   hdr = 'Ref Customer Code  as sample' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '42895e0e' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 9   hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 10  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 11  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 12  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 13  hdr = 'aLWAYS x' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = '42895e0e' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 25  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 26  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 27  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 28  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 29  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 30  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 31  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 32  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 33  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = '42895e0e' col = 34  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 35  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 36  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 37  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 38  hdr = 'Attribute 1' node = 'C' fld = 'KATR1' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 39  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 40  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 41  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '42895e0e' col = 42  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '42895e0e' col = 43  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 44  hdr = 'Tax Number 3 ( GST Number)' node = 'C' fld = 'STCD3' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 45  hdr = 'Permanent Account Number' node = 'C' fld = 'J_1IPANNO' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 46  hdr = 'GST TDS Registration' node = 'C' fld = 'GST_TDS' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 47  hdr = 'Aadhaar Number' node = 'I' fld = 'X90003' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 48  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' cnv = 'GL' )
      ( tmpl = '42895e0e' col = 49  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 50  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '42895e0e' col = 51  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 52  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 53  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '42895e0e' col = 54  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 55  hdr = 'Tolerance group for the business partner/G/L account' node = 'B' fld = 'TOGRU' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 56  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 57  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 58  hdr = 'Block Key for Payment' node = 'B' fld = 'ZAHLS' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 59  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 60  hdr = 'Order probab.' node = 'S' fld = 'AWAHR' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 61  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 62  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 63  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 64  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 65  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 66  hdr = 'Exch. Rate Type M' node = 'S' fld = 'KURST' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 67  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 68  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 69  hdr = 'Price List' node = 'S' fld = 'PLTYP' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 70  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 71  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 72  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 73  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 74  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 75  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = '42895e0e' col = 76  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 77  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 78  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 79  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 80  hdr = 'JOIG IN:Central GST - OP' node = 'T' fld = 'JOCG' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 81  hdr = 'JTC1 IN: 206C(1H) Goods' node = 'T' fld = 'JTC1' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 82  hdr = 'JTX1 Tax Jurisdict.Code d' node = 'T' fld = 'JTX1' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 83  hdr = 'JTX2 Tax Jurisdict.Code d' node = 'T' fld = 'JTX2' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 84  hdr = 'JTX3 Tax Jurisdict.Code d' node = 'T' fld = 'JTX3' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 85  hdr = 'JTX4 Tax Jurisdict.Code d' node = 'T' fld = 'JTX4' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 86  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 87  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 88  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 89  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 90  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 91  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 92  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 93  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' cnv = 'NM' )
      ( tmpl = '42895e0e' col = 94  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 95  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 96  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 97  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 98  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '42895e0e' col = 99  hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 100 hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '42895e0e' col = 101 hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 102 hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '42895e0e' col = 103 hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 104 hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '42895e0e' col = 105 hdr = 'Retails Lic No (20 and 21 )' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 106 hdr = 'SC_EXEMPT' node = 'Z' fld = 'SC_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 107 hdr = 'Retails Lic Exp date' node = 'Z' fld = 'RETAIL_EXP' fmt = '' cnv = 'DT' )
      ( tmpl = '42895e0e' col = 108 hdr = 'Mfg License (Gen) Number' node = 'Z' fld = 'MFGLIC1NO' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 109 hdr = 'Mfg License (Nar) Number' node = 'Z' fld = 'MFGLIC2NO' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 110 hdr = 'Mfg License (CC) Number' node = 'Z' fld = 'MFGLIC3NO' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 111 hdr = 'Bank Guarantee(Y/N)' node = 'Z' fld = 'BGYN' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 112 hdr = 'Bank Guarantee No' node = 'Z' fld = 'BG_NO' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 113 hdr = 'BG Amount' node = 'Z' fld = 'BG_AMT' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 114 hdr = 'SD Document Currency' node = 'Z' fld = 'CURRENCY' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 115 hdr = 'BG Issue Date' node = 'Z' fld = 'BG_ISS_DT' fmt = '' cnv = 'DT' )
      ( tmpl = '42895e0e' col = 116 hdr = 'BG Expiry Date' node = 'Z' fld = 'BG_EXP_DT' fmt = '' cnv = 'DT' )
      ( tmpl = '42895e0e' col = 117 hdr = 'BG Issuing Bank' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 118 hdr = 'Agreement Expiry Date' node = 'Z' fld = 'AGGR_EXPDT' fmt = '' cnv = 'DT' )
      ( tmpl = '42895e0e' col = 119 hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' cnv = 'DT' )
      ( tmpl = '42895e0e' col = 120 hdr = 'Customer group' node = 'Z' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 121 hdr = 'AIOCD Code' node = 'Z' fld = 'AIOCD_CODE' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 122 hdr = 'Customer Bank Name' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 123 hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 124 hdr = 'Route Code' node = 'Z' fld = 'ZTROUT' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 125 hdr = 'Extension' node = 'Z' fld = 'EXTENSION' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 126 hdr = 'Route' node = 'Z' fld = 'ZCROUT' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 127 hdr = 'GLN URI Format' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 128 hdr = 'DUNS_Number' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 129 hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '42895e0e' col = 130 hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '42895e0e' col = 131 hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '42895e0e' col = 132 hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '42895e0e' col = 133 hdr = 'Import_License/MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 134 hdr = 'IMPL/MIA_From_Date' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = '' cnv = 'DT' )
      ( tmpl = '42895e0e' col = 135 hdr = 'IMPL/MIA_Valid_Date' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = '' cnv = 'DT' )
      ( tmpl = '42895e0e' col = 136 hdr = 'Check Digit' node = 'Z' fld = 'CHECK_DIGIT' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 137 hdr = 'Global Company Prefix' node = 'Z' fld = 'GLOBAL_COM' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 138 hdr = 'Backorder Days' node = 'Z' fld = 'BO_DAYS' fmt = '' cnv = '' )
      ( tmpl = '42895e0e' col = 139 hdr = 'Location Number' node = 'Z' fld = 'LOCATION_NUMBER' fmt = '' cnv = '' )
    "  43156406 - 63 columns - BE/YSHP, ES/YSHP, GB/YSHP, NL/YSHP, UG/YSHP, US/YSHP
      ( tmpl = '43156406' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 2   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 3   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 4   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 5   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 6   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 7   hdr = 'Always X' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 8   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = '43156406' col = 9   hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 10  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 11  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 12  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 13  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 14  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 15  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 16  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 17  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 18  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 19  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 20  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 21  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 22  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 23  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 24  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 25  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 26  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = '43156406' col = 27  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 28  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 29  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 30  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 31  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 32  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 33  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 34  hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 35  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 36  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 37  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 38  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 39  hdr = 'ABC class' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 40  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 41  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 42  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 43  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 44  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 45  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = '43156406' col = 46  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 47  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 48  hdr = 'Customer Account Assignment Group' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 49  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 50  hdr = 'Tax classification for customer' node = 'T' fld = '#2' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 51  hdr = 'Tax classification for customer' node = 'T' fld = '#3' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 52  hdr = 'Tax classification for customer' node = 'T' fld = '#4' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 53  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 54  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 55  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 56  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 57  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 58  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 59  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 60  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' cnv = 'NM' )
      ( tmpl = '43156406' col = 61  hdr = '20B. Lic. No' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 62  hdr = '21B. Lic. No' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' cnv = '' )
      ( tmpl = '43156406' col = 63  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' cnv = 'DT' )
    "  647cd8ca - 24 columns - IN/ZBMR
      ( tmpl = '647cd8ca' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 2   hdr = 'Customer Code' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '647cd8ca' col = 3   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 4   hdr = 'aLWAYS x' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 5   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = '647cd8ca' col = 6   hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 7   hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 8   hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 9   hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 10  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 11  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 12  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 13  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 14  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 15  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 16  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 17  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 18  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 19  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 20  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 21  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 22  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 23  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = '647cd8ca' col = 24  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
    "  67849f38 - 108 columns - IN/ZSHM
      ( tmpl = '67849f38' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 2   hdr = 'Customer Account Number' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '67849f38' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 9   hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 10  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 11  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 12  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 13  hdr = 'Always X' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = '67849f38' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
    ).
  ENDMETHOD.

  METHOD map_3.
    rt = VALUE tt_col(
      ( tmpl = '67849f38' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 25  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 26  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 27  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 28  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 29  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 30  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 31  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 32  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = '67849f38' col = 33  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 34  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 35  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 36  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 37  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 38  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 39  hdr = 'Permanent Account Number' node = 'C' fld = 'J_1IPANNO' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 40  hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 41  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 42  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 43  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 44  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 45  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 46  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 47  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = '67849f38' col = 48  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 49  hdr = 'Tax classification for customer' node = 'T' fld = 'JOCG' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 50  hdr = 'Tax classification for customer' node = 'T' fld = 'JTC1' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 51  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX1' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 52  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX2' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 53  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX3' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 54  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX4' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 55  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 56  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 57  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 58  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 59  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 60  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 61  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 62  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' cnv = 'NM' )
      ( tmpl = '67849f38' col = 63  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 64  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 65  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 66  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 67  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '67849f38' col = 68  hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 69  hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '67849f38' col = 70  hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 71  hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '67849f38' col = 72  hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 73  hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '67849f38' col = 74  hdr = 'Retails Lic No (20 and 21 )' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 75  hdr = 'SC_EXEMPT' node = 'Z' fld = 'SC_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 76  hdr = 'Retails Lic Exp date' node = 'Z' fld = 'RETAIL_EXP' fmt = '' cnv = 'DT' )
      ( tmpl = '67849f38' col = 77  hdr = 'Mfg License (Gen) Number' node = 'Z' fld = 'MFGLIC1NO' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 78  hdr = 'Mfg License (Nar) Number' node = 'Z' fld = 'MFGLIC2NO' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 79  hdr = 'Mfg License (CC) Number' node = 'Z' fld = 'MFGLIC3NO' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 80  hdr = 'Bank Guarantee(Y/N)' node = 'Z' fld = 'BGYN' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 81  hdr = 'Bank Guarantee No' node = 'Z' fld = 'BG_NO' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 82  hdr = 'BG Amount' node = 'Z' fld = 'BG_AMT' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 83  hdr = 'SD Document Currency' node = 'Z' fld = 'CURRENCY' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 84  hdr = 'BG Issue Date' node = 'Z' fld = 'BG_ISS_DT' fmt = '' cnv = 'DT' )
      ( tmpl = '67849f38' col = 85  hdr = 'BG Expiry Date' node = 'Z' fld = 'BG_EXP_DT' fmt = '' cnv = 'DT' )
      ( tmpl = '67849f38' col = 86  hdr = 'BG Issuing Bank' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 87  hdr = 'Agreement Expiry Date' node = 'Z' fld = 'AGGR_EXPDT' fmt = '' cnv = 'DT' )
      ( tmpl = '67849f38' col = 88  hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' cnv = 'DT' )
      ( tmpl = '67849f38' col = 89  hdr = 'Customer group' node = 'Z' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 90  hdr = 'AIOCD Code' node = 'Z' fld = 'AIOCD_CODE' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 91  hdr = 'Customer Bank Name' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 92  hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 93  hdr = 'Route Code' node = 'Z' fld = 'ZTROUT' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 94  hdr = 'Extension' node = 'Z' fld = 'EXTENSION' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 95  hdr = 'Route' node = 'Z' fld = 'ZCROUT' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 96  hdr = 'GLN URI Format' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 97  hdr = 'DUNS_Number' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 98  hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '67849f38' col = 99  hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '67849f38' col = 100 hdr = 'Import_License/MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 101 hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '67849f38' col = 102 hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '67849f38' col = 103 hdr = 'IMPL/MIA_From_Date' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = '' cnv = 'DT' )
      ( tmpl = '67849f38' col = 104 hdr = 'IMPL/MIA_Valid_Date' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = '' cnv = 'DT' )
      ( tmpl = '67849f38' col = 105 hdr = 'Check Digit' node = 'Z' fld = 'CHECK_DIGIT' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 106 hdr = 'Global Company Prefix' node = 'Z' fld = 'GLOBAL_COM' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 107 hdr = 'Backorder Days' node = 'Z' fld = 'BO_DAYS' fmt = '' cnv = '' )
      ( tmpl = '67849f38' col = 108 hdr = 'Location Number' node = 'Z' fld = 'LOCATION_NUMBER' fmt = '' cnv = '' )
    "  6c94ea65 - 77 columns - IN/ZSHP
      ( tmpl = '6c94ea65' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 2   hdr = 'Customer code' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '6c94ea65' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 9   hdr = 'Ref Company Code' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 10  hdr = 'Ref Sales Organization' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 11  hdr = 'Ref Distribution Channel' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 12  hdr = 'Ref Division' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 13  hdr = 'Always X' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = '6c94ea65' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 25  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 26  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 27  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 28  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 29  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 30  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 31  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 32  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = '6c94ea65' col = 33  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 34  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 35  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 36  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 37  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 38  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 39  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '6c94ea65' col = 40  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '6c94ea65' col = 41  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 42  hdr = 'Tax Number 2' node = 'C' fld = 'STCD2' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 43  hdr = 'Tax Number 1' node = 'C' fld = 'STCD1' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 44  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 45  hdr = 'VAT Registration Number' node = 'C' fld = 'STCEG' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 46  hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 47  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 48  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 49  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 50  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 51  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 52  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 53  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 54  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 55  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 56  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 57  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = '6c94ea65' col = 58  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 59  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 60  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 61  hdr = 'Tax classification for customer' node = 'T' fld = 'JOCG' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 62  hdr = 'Tax classification for customer' node = 'T' fld = 'JTC1' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 63  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX1' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 64  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX2' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 65  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX3' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 66  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 67  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 68  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 69  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 70  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 71  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 72  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 73  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 74  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 75  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 76  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '6c94ea65' col = 77  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' cnv = 'DT' )
    "  6d2ec22f - 132 columns - US/ZDOM
      ( tmpl = '6d2ec22f' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 2   hdr = 'Customer Code' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '6d2ec22f' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 9   hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 10  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 11  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 12  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 13  hdr = 'aLWAYS x' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = '6d2ec22f' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 25  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 26  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 27  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 28  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 29  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 30  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 31  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 32  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 33  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = '6d2ec22f' col = 34  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 35  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 36  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 37  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 38  hdr = 'Attribute 1' node = 'C' fld = 'KATR1' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 39  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 40  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 41  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '6d2ec22f' col = 42  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '6d2ec22f' col = 43  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 44  hdr = 'GST TDS Registration' node = 'C' fld = 'GST_TDS' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 45  hdr = 'Aadhaar Number' node = 'I' fld = 'X90003' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 46  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' cnv = 'GL' )
      ( tmpl = '6d2ec22f' col = 47  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 48  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '6d2ec22f' col = 49  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 50  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 51  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '6d2ec22f' col = 52  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 53  hdr = 'Tolerance group for the business partner/G/L account' node = 'B' fld = 'TOGRU' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 54  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 55  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 56  hdr = 'Block Key for Payment' node = 'B' fld = 'ZAHLS' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 57  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 58  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 59  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 60  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 61  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 62  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 63  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 64  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 65  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 66  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 67  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 68  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 69  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 70  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = '6d2ec22f' col = 71  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 72  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 73  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 74  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 75  hdr = 'JOIG IN:Central GST - OP' node = 'T' fld = 'JOCG' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 76  hdr = 'JTC1 IN: 206C(1H) Goods' node = 'T' fld = 'JTC1' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 77  hdr = 'JTX1 Tax Jurisdict.Code d' node = 'T' fld = 'JTX1' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 78  hdr = 'JTX2 Tax Jurisdict.Code d' node = 'T' fld = 'JTX2' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 79  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 80  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 81  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 82  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 83  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 84  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 85  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 86  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' cnv = 'NM' )
      ( tmpl = '6d2ec22f' col = 87  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 88  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 89  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 90  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 91  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '6d2ec22f' col = 92  hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 93  hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '6d2ec22f' col = 94  hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 95  hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '6d2ec22f' col = 96  hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 97  hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '6d2ec22f' col = 98  hdr = 'Retails Lic No (20 and 21 )' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 99  hdr = 'SC_EXEMPT' node = 'Z' fld = 'SC_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 100 hdr = 'Retails Lic Exp date' node = 'Z' fld = 'RETAIL_EXP' fmt = '' cnv = 'DT' )
      ( tmpl = '6d2ec22f' col = 101 hdr = 'Mfg License (Gen) Number' node = 'Z' fld = 'MFGLIC1NO' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 102 hdr = 'Mfg License (Nar) Number' node = 'Z' fld = 'MFGLIC2NO' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 103 hdr = 'Mfg License (CC) Number' node = 'Z' fld = 'MFGLIC3NO' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 104 hdr = 'Bank Guarantee(Y/N)' node = 'Z' fld = 'BGYN' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 105 hdr = 'Bank Guarantee No' node = 'Z' fld = 'BG_NO' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 106 hdr = 'BG Amount' node = 'Z' fld = 'BG_AMT' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 107 hdr = 'SD Document Currency' node = 'Z' fld = 'CURRENCY' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 108 hdr = 'BG Issue Date' node = 'Z' fld = 'BG_ISS_DT' fmt = '' cnv = 'DT' )
      ( tmpl = '6d2ec22f' col = 109 hdr = 'BG Expiry Date' node = 'Z' fld = 'BG_EXP_DT' fmt = '' cnv = 'DT' )
      ( tmpl = '6d2ec22f' col = 110 hdr = 'BG Issuing Bank' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 111 hdr = 'Agreement Expiry Date' node = 'Z' fld = 'AGGR_EXPDT' fmt = '' cnv = 'DT' )
      ( tmpl = '6d2ec22f' col = 112 hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' cnv = 'DT' )
      ( tmpl = '6d2ec22f' col = 113 hdr = 'Customer group' node = 'Z' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 114 hdr = 'AIOCD Code' node = 'Z' fld = 'AIOCD_CODE' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 115 hdr = 'Customer Bank Name' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 116 hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 117 hdr = 'Route Code' node = 'Z' fld = 'ZTROUT' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 118 hdr = 'Extension' node = 'Z' fld = 'EXTENSION' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 119 hdr = 'Route' node = 'Z' fld = 'ZCROUT' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 120 hdr = 'GLN URI Format' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 121 hdr = 'DUNS_Number' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 122 hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '6d2ec22f' col = 123 hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '6d2ec22f' col = 124 hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '6d2ec22f' col = 125 hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '6d2ec22f' col = 126 hdr = 'Import_License/MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 127 hdr = 'IMPL/MIA_From_Date' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = '' cnv = 'DT' )
      ( tmpl = '6d2ec22f' col = 128 hdr = 'IMPL/MIA_Valid_Date' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = '' cnv = 'DT' )
      ( tmpl = '6d2ec22f' col = 129 hdr = 'Check Digit' node = 'Z' fld = 'CHECK_DIGIT' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 130 hdr = 'Global Company Prefix' node = 'Z' fld = 'GLOBAL_COM' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 131 hdr = 'Backorder Days' node = 'Z' fld = 'BO_DAYS' fmt = '' cnv = '' )
      ( tmpl = '6d2ec22f' col = 132 hdr = 'Location Number' node = 'Z' fld = 'LOCATION_NUMBER' fmt = '' cnv = '' )
    "  6e6467eb - 74 columns - KE/YVTO, US/YVMI, US/YVSP, US/YVTO
      ( tmpl = '6e6467eb' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 2   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 3   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 4   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 5   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 6   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 7   hdr = 'Always X' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 8   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = '6e6467eb' col = 9   hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 10  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 11  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 12  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 13  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 14  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 15  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 16  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 17  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 18  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 19  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 20  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 21  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 22  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 23  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 24  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 25  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = '6e6467eb' col = 26  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 27  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 28  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 29  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 30  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 31  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 32  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 33  hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 34  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' cnv = 'GL' )
      ( tmpl = '6e6467eb' col = 35  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 36  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = 'AL' cnv = 'AL' )
    ).
  ENDMETHOD.

  METHOD map_4.
    rt = VALUE tt_col(
      ( tmpl = '6e6467eb' col = 37  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 38  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 39  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 40  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 41  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 42  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 43  hdr = 'Order probability of the item' node = 'S' fld = 'AWAHR' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 44  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 45  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 46  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 47  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 48  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 49  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 50  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 51  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 52  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 53  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 54  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 55  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = '6e6467eb' col = 56  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 57  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 58  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 59  hdr = 'Customer Account Assignment Group' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 60  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 61  hdr = 'Tax classification for customer' node = 'T' fld = '#2' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 62  hdr = 'Tax classification for customer' node = 'T' fld = '#3' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 63  hdr = 'Tax classification for customer' node = 'T' fld = '#4' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 64  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 65  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 66  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 67  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 68  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 69  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 70  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 71  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' cnv = 'NM' )
      ( tmpl = '6e6467eb' col = 72  hdr = '20B. Lic. No' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 73  hdr = '21B. Lic. No' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' cnv = '' )
      ( tmpl = '6e6467eb' col = 74  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' cnv = 'DT' )
    "  80d23dd8 - 76 columns - AE/ZSHP, US/ZSHP
      ( tmpl = '80d23dd8' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 2   hdr = 'Customer code' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '80d23dd8' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 9   hdr = 'Ref Company Code' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 10  hdr = 'Ref Sales Organization' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 11  hdr = 'Ref Distribution Channel' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 12  hdr = 'Ref Division' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 13  hdr = 'Always X' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = '80d23dd8' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 25  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 26  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 27  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 28  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 29  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 30  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 31  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 32  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = '80d23dd8' col = 33  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 34  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 35  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 36  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 37  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 38  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 39  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '80d23dd8' col = 40  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '80d23dd8' col = 41  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 42  hdr = 'Tax Number 2' node = 'C' fld = 'STCD2' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 43  hdr = 'Tax Number 1' node = 'C' fld = 'STCD1' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 44  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 45  hdr = 'VAT Registration Number' node = 'C' fld = 'STCEG' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 46  hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 47  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 48  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 49  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 50  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 51  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 52  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 53  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 54  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 55  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 56  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 57  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = '80d23dd8' col = 58  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 59  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 60  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 61  hdr = 'Tax classification for customer' node = 'T' fld = 'MWST' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 62  hdr = 'UTX2' node = 'T' fld = 'UTX2' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 63  hdr = 'UTX3' node = 'T' fld = 'UTX3' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 64  hdr = 'UTXJ' node = 'T' fld = 'UTXJ' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 65  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 66  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 67  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 68  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 69  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 70  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 71  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 72  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 73  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 74  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 75  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '80d23dd8' col = 76  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' cnv = 'DT' )
    "  84513e29 - 38 columns - IN/ZNOT
      ( tmpl = '84513e29' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 2   hdr = 'Ref Customer Code  as sample' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '84513e29' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 8   hdr = 'Number of contact person' node = 'P' fld = 'PARNR' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 9   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 10  hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 11  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 12  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 13  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 14  hdr = 'aLWAYS x' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 15  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = '84513e29' col = 16  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 17  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 18  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 19  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 20  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 21  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 22  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 23  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 24  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 25  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 26  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 27  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 28  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 29  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 30  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 31  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 32  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 33  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 34  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = '84513e29' col = 35  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 36  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 37  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = '84513e29' col = 38  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
    "  8a74041a - 73 columns - AU/ZSHP, BE/ZSHP, ES/ZSHP, GB/ZSHP, NL/ZSHP, UG/ZSHP
      ( tmpl = '8a74041a' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 2   hdr = 'Customer code' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '8a74041a' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 9   hdr = 'Ref Company Code' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 10  hdr = 'Ref Sales Organization' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 11  hdr = 'Ref Distribution Channel' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 12  hdr = 'Ref Division' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 13  hdr = 'Always X' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = '8a74041a' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 25  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 26  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 27  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 28  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 29  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 30  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 31  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 32  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = '8a74041a' col = 33  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 34  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 35  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 36  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 37  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 38  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 39  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '8a74041a' col = 40  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '8a74041a' col = 41  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 42  hdr = 'Tax Number 2' node = 'C' fld = 'STCD2' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 43  hdr = 'Tax Number 1' node = 'C' fld = 'STCD1' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 44  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 45  hdr = 'VAT Registration Number' node = 'C' fld = 'STCEG' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 46  hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 47  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 48  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 49  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 50  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 51  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 52  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 53  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 54  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 55  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 56  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 57  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = '8a74041a' col = 58  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 59  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 60  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 61  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 62  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 63  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 64  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 65  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 66  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 67  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 68  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 69  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 70  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 71  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 72  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '8a74041a' col = 73  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' cnv = 'DT' )
    "  9c914100 - 140 columns - AU/ZCDP, BE/ZCDP, ES/ZCDP, GB/ZCDP, IN/ZCDP, MA/ZCDP, MA/ZOTC, NL/ZCDP, UG/ZCDP, UG/ZOTC, US/ZCDP
      ( tmpl = '9c914100' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 2   hdr = 'Ref Customer Code  as sample' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '9c914100' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 8   hdr = 'Number of contact person' node = 'P' fld = 'PARNR' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 9   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 10  hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 11  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 12  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 13  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 14  hdr = 'aLWAYS x' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 15  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = '9c914100' col = 16  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 17  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 18  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 19  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 20  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 21  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 22  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 23  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 24  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 25  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 26  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 27  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 28  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 29  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 30  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 31  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 32  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 33  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 34  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = '9c914100' col = 35  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 36  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 37  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 38  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 39  hdr = 'Attribute 1' node = 'C' fld = 'KATR1' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 40  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 41  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 42  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '9c914100' col = 43  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '9c914100' col = 44  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 45  hdr = 'Tax Number 3 ( GST Number)' node = 'C' fld = 'STCD3' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 46  hdr = 'Permanent Account Number' node = 'C' fld = 'J_1IPANNO' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 47  hdr = 'GST TDS Registration' node = 'C' fld = 'GST_TDS' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 48  hdr = 'Aadhaar Number' node = 'I' fld = 'X90003' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 49  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' cnv = 'GL' )
      ( tmpl = '9c914100' col = 50  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 51  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '9c914100' col = 52  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 53  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 54  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' cnv = 'AL' )
      ( tmpl = '9c914100' col = 55  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 56  hdr = 'Tolerance group for the business partner/G/L account' node = 'B' fld = 'TOGRU' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 57  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 58  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 59  hdr = 'Block Key for Payment' node = 'B' fld = 'ZAHLS' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 60  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 61  hdr = 'Order probab.' node = 'S' fld = 'AWAHR' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 62  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 63  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 64  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 65  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 66  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 67  hdr = 'Exch. Rate Type M' node = 'S' fld = 'KURST' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 68  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 69  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 70  hdr = 'Price List' node = 'S' fld = 'PLTYP' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 71  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 72  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 73  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 74  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 75  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 76  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = '9c914100' col = 77  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 78  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 79  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 80  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 81  hdr = 'JOIG IN:Central GST - OP' node = 'T' fld = 'JOCG' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 82  hdr = 'JTC1 IN: 206C(1H) Goods' node = 'T' fld = 'JTC1' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 83  hdr = 'JTX1 Tax Jurisdict.Code d' node = 'T' fld = 'JTX1' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 84  hdr = 'JTX2 Tax Jurisdict.Code d' node = 'T' fld = 'JTX2' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 85  hdr = 'JTX3 Tax Jurisdict.Code d' node = 'T' fld = 'JTX3' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 86  hdr = 'JTX4 Tax Jurisdict.Code d' node = 'T' fld = 'JTX4' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 87  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 88  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 89  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 90  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 91  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 92  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 93  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 94  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' cnv = 'NM' )
      ( tmpl = '9c914100' col = 95  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 96  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 97  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 98  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 99  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '9c914100' col = 100 hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 101 hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '9c914100' col = 102 hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 103 hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '9c914100' col = 104 hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 105 hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = '9c914100' col = 106 hdr = 'Retails Lic No (20 and 21 )' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' cnv = '' )
    ).
  ENDMETHOD.

  METHOD map_5.
    rt = VALUE tt_col(
      ( tmpl = '9c914100' col = 107 hdr = 'SC_EXEMPT' node = 'Z' fld = 'SC_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 108 hdr = 'Retails Lic Exp date' node = 'Z' fld = 'RETAIL_EXP' fmt = '' cnv = 'DT' )
      ( tmpl = '9c914100' col = 109 hdr = 'Mfg License (Gen) Number' node = 'Z' fld = 'MFGLIC1NO' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 110 hdr = 'Mfg License (Nar) Number' node = 'Z' fld = 'MFGLIC2NO' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 111 hdr = 'Mfg License (CC) Number' node = 'Z' fld = 'MFGLIC3NO' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 112 hdr = 'Bank Guarantee(Y/N)' node = 'Z' fld = 'BGYN' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 113 hdr = 'Bank Guarantee No' node = 'Z' fld = 'BG_NO' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 114 hdr = 'BG Amount' node = 'Z' fld = 'BG_AMT' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 115 hdr = 'SD Document Currency' node = 'Z' fld = 'CURRENCY' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 116 hdr = 'BG Issue Date' node = 'Z' fld = 'BG_ISS_DT' fmt = '' cnv = 'DT' )
      ( tmpl = '9c914100' col = 117 hdr = 'BG Expiry Date' node = 'Z' fld = 'BG_EXP_DT' fmt = '' cnv = 'DT' )
      ( tmpl = '9c914100' col = 118 hdr = 'BG Issuing Bank' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 119 hdr = 'Agreement Expiry Date' node = 'Z' fld = 'AGGR_EXPDT' fmt = '' cnv = 'DT' )
      ( tmpl = '9c914100' col = 120 hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' cnv = 'DT' )
      ( tmpl = '9c914100' col = 121 hdr = 'Customer group' node = 'Z' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 122 hdr = 'AIOCD Code' node = 'Z' fld = 'AIOCD_CODE' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 123 hdr = 'Customer Bank Name' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 124 hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 125 hdr = 'Route Code' node = 'Z' fld = 'ZTROUT' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 126 hdr = 'Extension' node = 'Z' fld = 'EXTENSION' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 127 hdr = 'Route' node = 'Z' fld = 'ZCROUT' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 128 hdr = 'GLN URI Format' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 129 hdr = 'DUNS_Number' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 130 hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '9c914100' col = 131 hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '9c914100' col = 132 hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '9c914100' col = 133 hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = '9c914100' col = 134 hdr = 'Import_License/MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 135 hdr = 'IMPL/MIA_From_Date' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = '' cnv = 'DT' )
      ( tmpl = '9c914100' col = 136 hdr = 'IMPL/MIA_Valid_Date' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = '' cnv = 'DT' )
      ( tmpl = '9c914100' col = 137 hdr = 'Check Digit' node = 'Z' fld = 'CHECK_DIGIT' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 138 hdr = 'Global Company Prefix' node = 'Z' fld = 'GLOBAL_COM' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 139 hdr = 'Backorder Days' node = 'Z' fld = 'BO_DAYS' fmt = '' cnv = '' )
      ( tmpl = '9c914100' col = 140 hdr = 'Location Number' node = 'Z' fld = 'LOCATION_NUMBER' fmt = '' cnv = '' )
    "  ab38ead5 - 79 columns - AE/ZEXP, AU/ZEXP, BE/ZEXP, ES/ZEXP, GB/ZEXP, KE/ZEXP, NL/ZEXP, US/ZEXP, ZA/ZEXP
      ( tmpl = 'ab38ead5' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 2   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 3   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 4   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 5   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 6   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 7   hdr = 'Always X' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 8   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = 'ab38ead5' col = 9   hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 10  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 11  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 12  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 13  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 14  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 15  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 16  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 17  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 18  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 19  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 20  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 21  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 22  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 23  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 24  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 25  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 26  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = 'ab38ead5' col = 27  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 28  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 29  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 30  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 31  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 32  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 33  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'ab38ead5' col = 34  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'ab38ead5' col = 35  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 36  hdr = 'Permanent Account Number' node = 'C' fld = 'J_1IPANNO' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 37  hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 38  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' cnv = 'GL' )
      ( tmpl = 'ab38ead5' col = 39  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 40  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'ab38ead5' col = 41  hdr = 'Value Adjustment Key' node = 'B' fld = 'WBRSL' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 42  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 43  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 44  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 45  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 46  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 47  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 48  hdr = 'Order probability of the item' node = 'S' fld = 'AWAHR' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 49  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 50  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 51  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 52  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 53  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 54  hdr = 'Exchange Rate Type' node = 'S' fld = 'KURST' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 55  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 56  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 57  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 58  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 59  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 60  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 61  hdr = 'Partial delivery at item level' node = 'S' fld = 'KZTLF' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 62  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = 'ab38ead5' col = 63  hdr = 'Underdelivery Tolerance Limit' node = 'S' fld = 'UNTTO' fmt = '' cnv = 'NM' )
      ( tmpl = 'ab38ead5' col = 64  hdr = 'Overdelivery Tolerance Limit' node = 'S' fld = 'UEBTO' fmt = '' cnv = 'NM' )
      ( tmpl = 'ab38ead5' col = 65  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 66  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 67  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 68  hdr = 'Customer Account Assignment Group' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 69  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 70  hdr = 'Tax classification for customer' node = 'T' fld = '#2' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 71  hdr = 'Tax classification for customer' node = 'T' fld = '#3' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 72  hdr = 'Tax classification for customer' node = 'T' fld = '#4' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 73  hdr = 'Tax classification for customer' node = 'T' fld = '#5' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 74  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 75  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 76  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 77  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 78  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = 'ab38ead5' col = 79  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
    "  c9c1a8dd - 16 columns - */*
      ( tmpl = 'BLOCK' col = 1   hdr = 'Blocking or unblocking' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'BLOCK' col = 2   hdr = 'Customer Account Number' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'BLOCK' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = 'BLOCK' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = 'BLOCK' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = 'BLOCK' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = 'BLOCK' col = 7   hdr = 'Central posting block' node = 'C' fld = 'SPERR' fmt = '' cnv = '' )
      ( tmpl = 'BLOCK' col = 8   hdr = 'Posting block for company code' node = 'B' fld = 'SPERR' fmt = '' cnv = '' )
      ( tmpl = 'BLOCK' col = 9   hdr = 'Central order block for customer' node = 'C' fld = 'AUFSD' fmt = '' cnv = '' )
      ( tmpl = 'BLOCK' col = 10  hdr = 'Customer order block (sales area)' node = 'S' fld = 'AUFSD' fmt = '' cnv = '' )
      ( tmpl = 'BLOCK' col = 11  hdr = 'Central delivery block for the customer' node = 'C' fld = 'LIFSD' fmt = '' cnv = '' )
      ( tmpl = 'BLOCK' col = 12  hdr = 'Customer delivery block (sales area)' node = 'S' fld = 'LIFSD' fmt = '' cnv = '' )
      ( tmpl = 'BLOCK' col = 13  hdr = 'Central billing block for customer' node = 'C' fld = 'FAKSD' fmt = '' cnv = '' )
      ( tmpl = 'BLOCK' col = 14  hdr = 'Billing block for customer (sales and distribution)' node = 'S' fld = 'FAKSD' fmt = '' cnv = '' )
      ( tmpl = 'BLOCK' col = 15  hdr = 'Central sales block for customer' node = 'C' fld = 'CASSD' fmt = '' cnv = '' )
      ( tmpl = 'BLOCK' col = 16  hdr = 'Sales block for customer (sales area)' node = 'S' fld = 'CASSD' fmt = '' cnv = '' )
    "  d7ee33bb - 65 columns - UG/ZDOM, UG/ZEXP
      ( tmpl = 'd7ee33bb' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 2   hdr = 'Customer code' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'd7ee33bb' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 8   hdr = 'Always X' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 9   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = 'd7ee33bb' col = 10  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 11  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 12  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 13  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 14  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 15  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 16  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 17  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 18  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 19  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 20  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 21  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 22  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 23  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 24  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 25  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 26  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 27  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 28  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 29  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 30  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 31  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' cnv = 'GL' )
      ( tmpl = 'd7ee33bb' col = 32  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 33  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'd7ee33bb' col = 34  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 35  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 36  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 37  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 38  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 39  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 40  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 41  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 42  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 43  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 44  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 45  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 46  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 47  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 48  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 49  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 50  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 51  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = 'd7ee33bb' col = 52  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 53  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 54  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 55  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 56  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 57  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 58  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 59  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 60  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 61  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 62  hdr = 'TIN No' node = 'Z' fld = 'TIN' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 63  hdr = 'Taxpayer Type' node = 'Z' fld = 'TAXP_TYPE' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 64  hdr = 'NINBRN' node = 'Z' fld = 'NINBRN' fmt = '' cnv = '' )
      ( tmpl = 'd7ee33bb' col = 65  hdr = 'Legal Name' node = 'Z' fld = 'LEGL_NAME' fmt = '' cnv = '' )
    "  da47e4b3 - 76 columns - IN/ZEXP
      ( tmpl = 'da47e4b3' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 2   hdr = 'Customer code' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'da47e4b3' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 8   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = 'da47e4b3' col = 9   hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 10  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 11  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 12  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 13  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 14  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 15  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 16  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 17  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 18  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 19  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 20  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 21  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 22  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 23  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 24  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 25  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 26  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 27  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = 'da47e4b3' col = 28  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 29  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 30  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 31  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 32  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 33  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' cnv = 'GL' )
      ( tmpl = 'da47e4b3' col = 34  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 35  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 36  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 37  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 38  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 39  hdr = 'Order probability of the item' node = 'S' fld = 'AWAHR' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 40  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 41  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 42  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 43  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 44  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 45  hdr = 'Exchange Rate Type' node = 'S' fld = 'KURST' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 46  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 47  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 48  hdr = 'Price list type' node = 'S' fld = 'PLTYP' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 49  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 50  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 51  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 52  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 53  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 54  hdr = 'Partial delivery at item level' node = 'S' fld = 'KZTLF' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 55  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = 'da47e4b3' col = 56  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 57  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 58  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 59  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 60  hdr = 'Tax classification for customer' node = 'T' fld = 'JOCG' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 61  hdr = 'Tax classification for customer' node = 'T' fld = 'JTC1' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 62  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX1' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 63  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX2' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 64  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX3' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 65  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX4' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 66  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 67  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 68  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 69  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 70  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 71  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 72  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 73  hdr = 'TIN No' node = 'Z' fld = 'TIN' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 74  hdr = 'Taxpayer Type' node = 'Z' fld = 'TAXP_TYPE' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 75  hdr = 'NINBRN' node = 'Z' fld = 'NINBRN' fmt = '' cnv = '' )
      ( tmpl = 'da47e4b3' col = 76  hdr = 'Legal Name' node = 'Z' fld = 'LEGL_NAME' fmt = '' cnv = '' )
    "  e10ec770 - 137 columns - BE/ZDOM, ES/ZDOM, GB/ZDOM, NL/ZDOM, ZA/ZDOM
      ( tmpl = 'e10ec770' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 2   hdr = 'Customer Account Number' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'e10ec770' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 9   hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 10  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 11  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 12  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 13  hdr = 'Always X' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = 'e10ec770' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 25  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 26  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 27  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 28  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 29  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 30  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 31  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 32  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 33  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = 'e10ec770' col = 34  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 35  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 36  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 37  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 38  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 39  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 40  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'e10ec770' col = 41  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'e10ec770' col = 42  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 43  hdr = 'Tax Number 1' node = 'C' fld = 'STCD1' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 44  hdr = 'Tax Number 2' node = 'C' fld = 'STCD2' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 45  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 46  hdr = 'Liable for VAT' node = 'C' fld = 'STKZU' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 47  hdr = 'Account number of the master record with the fiscal address' node = 'C' fld = 'FISKN' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'e10ec770' col = 48  hdr = 'VAT Registration Number' node = 'C' fld = 'STCEG' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 49  hdr = 'First name' node = 'P' fld = 'NAMEV' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 50  hdr = 'Name 1' node = 'P' fld = 'NAME1' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 51  hdr = 'Contact person department' node = 'P' fld = 'ABTNR' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 52  hdr = 'Contact person function' node = 'P' fld = 'PAFKT' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 53  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' cnv = 'GL' )
      ( tmpl = 'e10ec770' col = 54  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 55  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'e10ec770' col = 56  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 57  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 58  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'e10ec770' col = 59  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 60  hdr = 'Tolerance group for the business partner/G/L account' node = 'B' fld = 'TOGRU' fmt = '' cnv = '' )
    ).
  ENDMETHOD.

  METHOD map_6.
    rt = VALUE tt_col(
      ( tmpl = 'e10ec770' col = 61  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 62  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 63  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 64  hdr = 'Order probab.' node = 'S' fld = 'AWAHR' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 65  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 66  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 67  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 68  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 69  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 70  hdr = 'Exch. Rate Type' node = 'S' fld = 'KURST' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 71  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 72  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 73  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 74  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 75  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 76  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 77  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 78  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = 'e10ec770' col = 79  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 80  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 81  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 82  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 83  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 84  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 85  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 86  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 87  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 88  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 89  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 90  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 91  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' cnv = 'NM' )
      ( tmpl = 'e10ec770' col = 92  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 93  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 94  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 95  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 96  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = 'e10ec770' col = 97  hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 98  hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = 'e10ec770' col = 99  hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 100 hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = 'e10ec770' col = 101 hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 102 hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = 'e10ec770' col = 103 hdr = 'Retails Lic No (20 and 21 )' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 104 hdr = 'SC_EXEMPT' node = 'Z' fld = 'SC_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 105 hdr = 'Retails Lic Exp date' node = 'Z' fld = 'RETAIL_EXP' fmt = '' cnv = 'DT' )
      ( tmpl = 'e10ec770' col = 106 hdr = 'Mfg License (Gen) Number' node = 'Z' fld = 'MFGLIC1NO' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 107 hdr = 'Mfg License (Nar) Number' node = 'Z' fld = 'MFGLIC2NO' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 108 hdr = 'Mfg License (CC) Number' node = 'Z' fld = 'MFGLIC3NO' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 109 hdr = 'Bank Guarantee(Y/N)' node = 'Z' fld = 'BGYN' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 110 hdr = 'Bank Guarantee No' node = 'Z' fld = 'BG_NO' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 111 hdr = 'BG Amount' node = 'Z' fld = 'BG_AMT' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 112 hdr = 'SD Document Currency' node = 'Z' fld = 'CURRENCY' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 113 hdr = 'BG Issue Date' node = 'Z' fld = 'BG_ISS_DT' fmt = '' cnv = 'DT' )
      ( tmpl = 'e10ec770' col = 114 hdr = 'BG Expiry Date' node = 'Z' fld = 'BG_EXP_DT' fmt = '' cnv = 'DT' )
      ( tmpl = 'e10ec770' col = 115 hdr = 'BG Issuing Bank' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 116 hdr = 'Agreement Expiry Date' node = 'Z' fld = 'AGGR_EXPDT' fmt = '' cnv = 'DT' )
      ( tmpl = 'e10ec770' col = 117 hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' cnv = 'DT' )
      ( tmpl = 'e10ec770' col = 118 hdr = 'Customer group' node = 'Z' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 119 hdr = 'AIOCD Code' node = 'Z' fld = 'AIOCD_CODE' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 120 hdr = 'Customer Bank Name' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 121 hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 122 hdr = 'Route Code' node = 'Z' fld = 'ZTROUT' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 123 hdr = 'Extension' node = 'Z' fld = 'EXTENSION' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 124 hdr = 'Route' node = 'Z' fld = 'ZCROUT' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 125 hdr = 'GLN URI Format' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 126 hdr = 'DUNS_Number' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 127 hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = 'e10ec770' col = 128 hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = 'e10ec770' col = 129 hdr = 'Import_License/MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 130 hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = 'e10ec770' col = 131 hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = 'e10ec770' col = 132 hdr = 'IMPL/MIA_From_Date' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = '' cnv = 'DT' )
      ( tmpl = 'e10ec770' col = 133 hdr = 'IMPL/MIA_Valid_Date' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = '' cnv = 'DT' )
      ( tmpl = 'e10ec770' col = 134 hdr = 'Check Digit' node = 'Z' fld = 'CHECK_DIGIT' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 135 hdr = 'Global Company Prefix' node = 'Z' fld = 'GLOBAL_COM' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 136 hdr = 'Backorder Days' node = 'Z' fld = 'BO_DAYS' fmt = '' cnv = '' )
      ( tmpl = 'e10ec770' col = 137 hdr = 'Location Number' node = 'Z' fld = 'LOCATION_NUMBER' fmt = '' cnv = '' )
    "  f10a66f5 - 36 columns - IN/ZSUB, UG/ZSUB
      ( tmpl = 'f10a66f5' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 2   hdr = 'Customer Code  as sample' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'f10a66f5' col = 3   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 4   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 5   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 6   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 7   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 8   hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 9   hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 10  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 11  hdr = 'aLWAYS x' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 12  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = 'f10a66f5' col = 13  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 14  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 15  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 16  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 17  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 18  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 19  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 20  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 21  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 22  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 23  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 24  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 25  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 26  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 27  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 28  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 29  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 30  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 31  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = 'f10a66f5' col = 32  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 33  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 34  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 35  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = 'f10a66f5' col = 36  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' cnv = '' )
    "  f7e2b95a - 83 columns - AU/ZDOM, MA/ZDOM
      ( tmpl = 'f7e2b95a' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 2   hdr = 'Customer Account Number' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'f7e2b95a' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 8   hdr = 'Always X' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 9   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = 'f7e2b95a' col = 10  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 11  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 12  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 13  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 14  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 15  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 16  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 17  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 18  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 19  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 20  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 21  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 22  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 23  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 24  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 25  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 26  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 27  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = 'f7e2b95a' col = 28  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 29  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 30  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 31  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 32  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 33  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 34  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'f7e2b95a' col = 35  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 36  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' cnv = 'GL' )
      ( tmpl = 'f7e2b95a' col = 37  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 38  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'f7e2b95a' col = 39  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 40  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 41  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 42  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 43  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 44  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 45  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 46  hdr = 'ABC class' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 47  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 48  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 49  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 50  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 51  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 52  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 53  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 54  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = 'f7e2b95a' col = 55  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 56  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 57  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 58  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 59  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 60  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 61  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 62  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 63  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 64  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 65  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 66  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 67  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' cnv = 'NM' )
      ( tmpl = 'f7e2b95a' col = 68  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 69  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 70  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 71  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 72  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = 'f7e2b95a' col = 73  hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 74  hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = 'f7e2b95a' col = 75  hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 76  hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = 'f7e2b95a' col = 77  hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' cnv = '' )
      ( tmpl = 'f7e2b95a' col = 78  hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = 'f7e2b95a' col = 79  hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' cnv = 'DT' )
      ( tmpl = 'f7e2b95a' col = 80  hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = 'f7e2b95a' col = 81  hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = 'f7e2b95a' col = 82  hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = 'f7e2b95a' col = 83  hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' cnv = 'DT' )
    "  fb40b09b - 136 columns - IN/ZDOM
      ( tmpl = 'fb40b09b' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 2   hdr = 'New Customer Code' node = 'K' fld = 'KUNNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'fb40b09b' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 9   hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 10  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 11  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 12  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 13  hdr = 'ALWAYS X' node = 'X' fld = 'X' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' cnv = 'TT' )
      ( tmpl = 'fb40b09b' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 25  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 26  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 27  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 28  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 29  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 30  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 31  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 32  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 33  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )
      ( tmpl = 'fb40b09b' col = 34  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 35  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 36  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 37  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 38  hdr = 'Attribute 1' node = 'C' fld = 'KATR1' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 39  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 40  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 41  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'fb40b09b' col = 42  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'fb40b09b' col = 43  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 44  hdr = 'Tax Number 3 ( GST Number)' node = 'C' fld = 'STCD3' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 45  hdr = 'Permanent Account Number' node = 'C' fld = 'J_1IPANNO' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 46  hdr = 'GST TDS Registration' node = 'C' fld = 'GST_TDS' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 47  hdr = 'Aadhaar Number' node = 'I' fld = 'X90003' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 48  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' cnv = 'GL' )
      ( tmpl = 'fb40b09b' col = 49  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 50  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'fb40b09b' col = 51  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 52  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 53  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' cnv = 'AL' )
      ( tmpl = 'fb40b09b' col = 54  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 55  hdr = 'Tolerance group for the business partner/G/L account' node = 'B' fld = 'TOGRU' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 56  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 57  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 58  hdr = 'Block Key for Payment' node = 'B' fld = 'ZAHLS' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 59  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 60  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 61  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 62  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 63  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 64  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 65  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 66  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 67  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 68  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 69  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 70  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 71  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 72  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' cnv = 'NM' )
      ( tmpl = 'fb40b09b' col = 73  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 74  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 75  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 76  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 77  hdr = 'JOIG IN:Central GST - OP' node = 'T' fld = 'JOCG' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 78  hdr = 'JTC1 IN: 206C(1H) Goods' node = 'T' fld = 'JTC1' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 79  hdr = 'JTX1 Tax Jurisdict.Code d' node = 'T' fld = 'JTX1' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 80  hdr = 'JTX2 Tax Jurisdict.Code d' node = 'T' fld = 'JTX2' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 81  hdr = 'JTX3 Tax Jurisdict.Code d' node = 'T' fld = 'JTX3' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 82  hdr = 'JTX4 Tax Jurisdict.Code d' node = 'T' fld = 'JTX4' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 83  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 84  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 85  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 86  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 87  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 88  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 89  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 90  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' cnv = 'NM' )
      ( tmpl = 'fb40b09b' col = 91  hdr = '20B. Lic. No' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 92  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 93  hdr = '21B. Lic. No' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 94  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 95  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = 'fb40b09b' col = 96  hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 97  hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = 'fb40b09b' col = 98  hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 99  hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = 'fb40b09b' col = 100 hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 101 hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = '' cnv = 'DT' )
      ( tmpl = 'fb40b09b' col = 102 hdr = 'Retails Lic No (20 and 21 )' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 103 hdr = 'SC_EXEMPT' node = 'Z' fld = 'SC_EXEMPT' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 104 hdr = 'Retails Lic Exp date' node = 'Z' fld = 'RETAIL_EXP' fmt = '' cnv = 'DT' )
      ( tmpl = 'fb40b09b' col = 105 hdr = 'Mfg License (Gen) Number' node = 'Z' fld = 'MFGLIC1NO' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 106 hdr = 'Mfg License (Nar) Number' node = 'Z' fld = 'MFGLIC2NO' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 107 hdr = 'Mfg License (CC) Number' node = 'Z' fld = 'MFGLIC3NO' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 108 hdr = 'Bank Guarantee(Y/N)' node = 'Z' fld = 'BGYN' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 109 hdr = 'Bank Guarantee No' node = 'Z' fld = 'BG_NO' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 110 hdr = 'BG Amount' node = 'Z' fld = 'BG_AMT' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 111 hdr = 'SD Document Currency' node = 'Z' fld = 'CURRENCY' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 112 hdr = 'BG Issue Date' node = 'Z' fld = 'BG_ISS_DT' fmt = '' cnv = 'DT' )
      ( tmpl = 'fb40b09b' col = 113 hdr = 'BG Expiry Date' node = 'Z' fld = 'BG_EXP_DT' fmt = '' cnv = 'DT' )
      ( tmpl = 'fb40b09b' col = 114 hdr = 'BG Issuing Bank' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 115 hdr = 'Agreement Expiry Date' node = 'Z' fld = 'AGGR_EXPDT' fmt = '' cnv = 'DT' )
      ( tmpl = 'fb40b09b' col = 116 hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' cnv = 'DT' )
      ( tmpl = 'fb40b09b' col = 117 hdr = 'Customer group' node = 'Z' fld = 'KDGRP' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 118 hdr = 'AIOCD Code' node = 'Z' fld = 'AIOCD_CODE' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 119 hdr = 'Customer Bank Name' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 120 hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 121 hdr = 'Route Code' node = 'Z' fld = 'ZTROUT' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 122 hdr = 'Extension' node = 'Z' fld = 'EXTENSION' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 123 hdr = 'Route' node = 'Z' fld = 'ZCROUT' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 124 hdr = 'GLN URI Format' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 125 hdr = 'DUNS_Number' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 126 hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = 'fb40b09b' col = 127 hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = 'fb40b09b' col = 128 hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = 'fb40b09b' col = 129 hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' cnv = 'DT' )
      ( tmpl = 'fb40b09b' col = 130 hdr = 'Import_License/MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 131 hdr = 'IMPL/MIA_From_Date' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = '' cnv = 'DT' )
      ( tmpl = 'fb40b09b' col = 132 hdr = 'IMPL/MIA_Valid_Date' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = '' cnv = 'DT' )
      ( tmpl = 'fb40b09b' col = 133 hdr = 'Check Digit' node = 'Z' fld = 'CHECK_DIGIT' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 134 hdr = 'Global Company Prefix' node = 'Z' fld = 'GLOBAL_COM' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 135 hdr = 'Backorder Days' node = 'Z' fld = 'BO_DAYS' fmt = '' cnv = '' )
      ( tmpl = 'fb40b09b' col = 136 hdr = 'Location Number' node = 'Z' fld = 'LOCATION_NUMBER' fmt = '' cnv = '' )
    ).
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_SRC - everything the program reads
*   The master data comes through the extract interface, which is the
*   mirror of the interface the upload program writes through. The three
*   things that interface does not carry are read directly.
*----------------------------------------------------------------------*
CLASS lcl_src DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_key,
             kunnr   TYPE kunnr,
             partner TYPE bu_partner,
           END OF ty_key,
           tt_key TYPE STANDARD TABLE OF ty_key WITH EMPTY KEY.

    " The customers asked for, whether they were given as a customer
    " number or as a business partner number.
    CLASS-METHODS keys
      IMPORTING it_bp     TYPE STANDARD TABLE
                it_kunnr  TYPE STANDARD TABLE
      RETURNING VALUE(rt) TYPE tt_key.

    CLASS-METHODS customer
      IMPORTING iv_kunnr TYPE kunnr
      EXPORTING es_data  TYPE cmds_ei_extern
      RAISING   lcx_dl.

    " The medium title text behind a title key, which is what the
    " template carries and what the upload program looks up on the way in.
    CLASS-METHODS title_text
      IMPORTING iv_title  TYPE any
      RETURNING VALUE(rv) TYPE string.

    CLASS-METHODS licence
      IMPORTING iv_kunnr  TYPE kunnr
      RETURNING VALUE(rs) TYPE zsd_license_chk.

    CLASS-METHODS aadhaar
      IMPORTING iv_partner TYPE bu_partner
      RETURNING VALUE(rv)  TYPE string.

    CLASS-METHODS contact
      IMPORTING iv_kunnr  TYPE kunnr
      RETURNING VALUE(rs) TYPE knvk.

  PRIVATE SECTION.
    CLASS-METHODS first_error
      IMPORTING is_err    TYPE cvis_message
      RETURNING VALUE(rv) TYPE string.
ENDCLASS.

CLASS lcl_src IMPLEMENTATION.

  METHOD keys.
    DATA ls_key TYPE ty_key.

    " A customer number given outright.
    LOOP AT it_kunnr ASSIGNING FIELD-SYMBOL(<ls_k>).
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_k> TO FIELD-SYMBOL(<lv_low>).
      IF <lv_low> IS NOT ASSIGNED OR <lv_low> IS INITIAL.
        CONTINUE.
      ENDIF.
      CLEAR ls_key.
      ls_key-kunnr = <lv_low>.
      APPEND ls_key TO rt.
    ENDLOOP.

    " A business partner number, resolved through the CVI link table -
    " the same resolution the upload program does, so a user may give
    " either number.
    LOOP AT it_bp ASSIGNING FIELD-SYMBOL(<ls_b>).
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_b> TO FIELD-SYMBOL(<lv_bp>).
      IF <lv_bp> IS NOT ASSIGNED OR <lv_bp> IS INITIAL.
        CONTINUE.
      ENDIF.
      DATA lv_partner TYPE bu_partner.
      lv_partner = <lv_bp>.
      SELECT SINGLE partner_guid FROM but000
        WHERE partner = @lv_partner INTO @DATA(lv_guid).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      SELECT SINGLE customer FROM cvi_cust_link
        WHERE partner_guid = @lv_guid INTO @DATA(lv_kunnr).
      IF sy-subrc <> 0 OR lv_kunnr IS INITIAL.
        CONTINUE.
      ENDIF.
      CLEAR ls_key.
      ls_key-kunnr   = lv_kunnr.
      ls_key-partner = lv_partner.
      APPEND ls_key TO rt.
    ENDLOOP.

    SORT rt BY kunnr.
    DELETE ADJACENT DUPLICATES FROM rt COMPARING kunnr.

    " The partner behind a customer given by customer number, for the
    " Aadhaar read.
    LOOP AT rt ASSIGNING FIELD-SYMBOL(<ls_r>) WHERE partner IS INITIAL.
      SELECT SINGLE partner_guid FROM cvi_cust_link
        WHERE customer = @<ls_r>-kunnr INTO @DATA(lv_g2).
      IF sy-subrc = 0.
        SELECT SINGLE partner FROM but000
          WHERE partner_guid = @lv_g2 INTO @<ls_r>-partner.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD customer.
    CLEAR es_data.
    " OBJECT_TASK has to be there as well as the number. Without it the
    " extractor answers with an entry that carries no data at all.
    DATA ls_in TYPE cmds_ei_main.
    APPEND VALUE cmds_ei_extern( header-object_task           = gc_task_read
                                 header-object_instance-kunnr = iv_kunnr ) TO ls_in-customers.

    DATA ls_out TYPE cmds_ei_main.
    DATA ls_err TYPE cvis_message.
    TRY.
        cmd_ei_api_extract=>get_data( EXPORTING is_master_data = ls_in
                                      IMPORTING es_master_data = ls_out
                                                es_error       = ls_err ).
      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION NEW lcx_dl( |Customer { iv_kunnr } could not be read: { lx->get_text( ) }| ).
    ENDTRY.

    IF ls_err-is_error = abap_true.
      RAISE EXCEPTION NEW lcx_dl( |Customer { iv_kunnr } could not be read: { first_error( ls_err ) }| ).
    ENDIF.
    IF ls_out-customers IS INITIAL.
      RAISE EXCEPTION NEW lcx_dl( |Customer { iv_kunnr } does not exist| ).
    ENDIF.
    es_data = ls_out-customers[ 1 ].
  ENDMETHOD.

  METHOD first_error.
    FIELD-SYMBOLS <lt_msg> TYPE ANY TABLE.
    ASSIGN COMPONENT 'MESSAGES' OF STRUCTURE is_err TO <lt_msg>.
    IF <lt_msg> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    LOOP AT <lt_msg> ASSIGNING FIELD-SYMBOL(<ls_msg>).
      DATA ls_ret TYPE bapiret2.
      CLEAR ls_ret.
      MOVE-CORRESPONDING <ls_msg> TO ls_ret.
      IF ls_ret-message IS NOT INITIAL.
        rv = ls_ret-message.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD title_text.
    DATA lv_title TYPE ad_title.
    lv_title = iv_title.
    IF lv_title IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE title_medi FROM tsad3t
      WHERE title = @lv_title AND langu = @sy-langu INTO @rv.
    IF sy-subrc <> 0.
      rv = lv_title.
    ENDIF.
  ENDMETHOD.

  METHOD licence.
    " ZSD_LICENSE_CHK is keyed on the customer alone - WERKS is a field of
    " the record, not part of its key - so there is one row per customer.
    SELECT SINGLE * FROM zsd_license_chk
      WHERE kunnr = @iv_kunnr INTO @rs.
  ENDMETHOD.

  METHOD aadhaar.
    IF iv_partner IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE idnumber FROM but0id
      WHERE partner = @iv_partner AND type = @gc_id_aadhaar INTO @rv.
  ENDMETHOD.

  METHOD contact.
    " The templates carry one contact person, so the first is the one.
    SELECT * FROM knvk
      WHERE kunnr = @iv_kunnr
      ORDER BY parnr
      INTO @rs UP TO 1 ROWS.
    ENDSELECT.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_ENG - builds the rows
*----------------------------------------------------------------------*
CLASS lcl_eng DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor IMPORTING iv_tmpl TYPE char8.
    METHODS run.
    METHODS head RETURNING VALUE(rt) TYPE tt_cell.
    METHODS rows RETURNING VALUE(rt) TYPE tt_row.
    METHODS log  RETURNING VALUE(rt) TYPE tt_msg.
  PRIVATE SECTION.
    DATA mv_tmpl TYPE char8.
    DATA mt_col  TYPE tt_col.
    DATA mt_row  TYPE tt_row.
    DATA mt_msg  TYPE tt_msg.
    DATA mv_wide TYPE i.

    METHODS add_msg   IMPORTING iv_key TYPE clike iv_type TYPE char1 iv_text TYPE clike.
    METHODS put       IMPORTING iv_col TYPE i iv_val TYPE clike CHANGING cs_row TYPE ty_row.
    METHODS comp      IMPORTING is_any TYPE any iv_fld TYPE clike iv_fmt TYPE clike
                      RETURNING VALUE(rv) TYPE string.
    METHODS empty_row RETURNING VALUE(rs) TYPE ty_row.
    METHODS cust      IMPORTING is_key TYPE lcl_src=>ty_key.
ENDCLASS.

CLASS lcl_eng IMPLEMENTATION.

  METHOD constructor.
    mv_tmpl = iv_tmpl.
    mt_col  = lcl_tmpl=>cols( iv_tmpl ).
    LOOP AT mt_col INTO DATA(ls_cl).
      IF ls_cl-col > mv_wide.
        mv_wide = ls_cl-col.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD head.
    DO mv_wide TIMES.
      APPEND INITIAL LINE TO rt.
    ENDDO.
    LOOP AT mt_col INTO DATA(ls_cl).
      READ TABLE rt ASSIGNING FIELD-SYMBOL(<lv>) INDEX ls_cl-col.
      IF sy-subrc = 0.
        <lv> = ls_cl-hdr.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD rows.
    rt = mt_row.
  ENDMETHOD.

  METHOD log.
    rt = mt_msg.
  ENDMETHOD.

  METHOD add_msg.
    " Green is an outcome and nothing else - the same rule the upload's list
    " follows. A remark the read makes on the way is information.
    APPEND VALUE ty_msg(
      icon    = COND #( WHEN iv_type = 'E' THEN icon_red_light
                        WHEN iv_type = 'W' THEN icon_yellow_light
                        WHEN iv_type = 'S' THEN icon_green_light
                        ELSE                    icon_information )
      objkey  = iv_key
      message = iv_text ) TO mt_msg.
  ENDMETHOD.

  METHOD empty_row.
    DO mv_wide TIMES.
      APPEND INITIAL LINE TO rs-cells.
    ENDDO.
  ENDMETHOD.

  METHOD put.
    READ TABLE cs_row-cells ASSIGNING FIELD-SYMBOL(<lv>) INDEX iv_col.
    IF sy-subrc = 0.
      <lv> = iv_val.
    ENDIF.
  ENDMETHOD.

  METHOD comp.
    " A column name that is not a component of the structure it was mapped
    " to is a fault in the map, not in the data - it is reported once and
    " the cell is left empty rather than terminating the run.
    FIELD-SYMBOLS <lv> TYPE any.
    ASSIGN COMPONENT iv_fld OF STRUCTURE is_any TO <lv>.
    IF <lv> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    rv = lcl_util=>text( iv_value = <lv> iv_fmt = iv_fmt ).
  ENDMETHOD.

  METHOD run.
    DATA(lt_key) = lcl_src=>keys( it_bp = s_bp[] it_kunnr = s_kunnr[] ).
    IF lt_key IS INITIAL.
      add_msg( iv_key = '' iv_type = 'W'
               iv_text = 'Nothing was selected - the file carries the headings only' ).
      RETURN.
    ENDIF.
    LOOP AT lt_key INTO DATA(ls_key).
      IF lines( mt_row ) >= p_max.
        add_msg( iv_key = '' iv_type = 'W'
                 iv_text = |Stopped at { p_max } row(s) - raise "Rows at most" for more| ).
        EXIT.
      ENDIF.
      cust( ls_key ).
    ENDLOOP.
  ENDMETHOD.

  METHOD cust.
    DATA ls_c TYPE cmds_ei_extern.
    TRY.
        lcl_src=>customer( EXPORTING iv_kunnr = is_key-kunnr IMPORTING es_data = ls_c ).
      CATCH lcx_dl INTO DATA(lx).
        DATA(lv_t) = lx->get_text( ).
        add_msg( iv_key = is_key-kunnr iv_type = 'E' iv_text = lv_t ).
        RETURN.
    ENDTRY.

    " The template is chosen by account group, the customers by number, and
    " nothing forces the two to agree. A customer whose own account group is
    " not the one the template is for still downloads - the file simply has
    " that template's layout - but the user is told, because a file whose
    " account group column disagrees with its layout is easy to misread.
    IF p_crt = abap_true AND p_ktokd IS NOT INITIAL.
      DATA(lv_ktokd) = comp( is_any = ls_c-central_data-central-data
                             iv_fld = 'KTOKD' iv_fmt = '' ).
      IF lv_ktokd IS NOT INITIAL AND lv_ktokd <> p_ktokd.
        add_msg( iv_key = is_key-kunnr iv_type = 'W'
                 iv_text = |Account group { lv_ktokd }, but the template is | &&
                           |for { p_ktokd } - the file has the { p_ktokd } layout| ).
      ENDIF.
    ENDIF.

    DATA(ls_lic) = lcl_src=>licence( is_key-kunnr ).
    DATA(lv_adh) = lcl_src=>aadhaar( is_key-partner ).
    DATA(ls_knvk) = lcl_src=>contact( is_key-kunnr ).

    DATA(lt_comp) = ls_c-company_data-company.
    DATA(lt_sale) = ls_c-sales_data-sales.
    DATA ls_comp TYPE cmds_ei_company.
    DATA ls_sale TYPE cmds_ei_sales.

    " One row per company code and sales area the customer has, because
    " that is how the templates are keyed. Nothing there - one row anyway,
    " with those columns empty.
    DATA lv_ci TYPE i.
    DATA lv_si TYPE i.
    DATA(lv_cn) = COND i( WHEN lt_comp IS INITIAL THEN 1 ELSE lines( lt_comp ) ).
    DATA(lv_sn) = COND i( WHEN lt_sale IS INITIAL THEN 1 ELSE lines( lt_sale ) ).

    lv_ci = 1.
    WHILE lv_ci <= lv_cn.
      CLEAR ls_comp.
      READ TABLE lt_comp INTO ls_comp INDEX lv_ci.
      lv_si = 1.
      WHILE lv_si <= lv_sn.
        CLEAR ls_sale.
        READ TABLE lt_sale INTO ls_sale INDEX lv_si.

        IF lines( mt_row ) >= p_max.
          lv_si = lv_sn + 1.
          lv_ci = lv_cn + 1.
          EXIT.
        ENDIF.

        DATA(ls_row) = empty_row( ).
        LOOP AT mt_col INTO DATA(ls_col).
          DATA lv_val TYPE string.
          CLEAR lv_val.

          CASE ls_col-node.
              " A column the template fills with a constant: the
              " transaction code, and the flag that is always X.
            WHEN 'X'.
              lv_val = ls_col-fld.

              " Copying from a reference is an XD01 feature. A customer
              " that exists has no reference, so the column stays empty.
            WHEN '-'.
              CLEAR lv_val.

            WHEN 'K'.
              CASE ls_col-fld.
                WHEN 'KUNNR'. lv_val = lcl_util=>text( iv_value = is_key-kunnr iv_fmt = 'AL' ).
                WHEN 'BUKRS'. lv_val = ls_comp-data_key-bukrs.
                WHEN 'VKORG'. lv_val = ls_sale-data_key-vkorg.
                WHEN 'VTWEG'. lv_val = ls_sale-data_key-vtweg.
                WHEN 'SPART'. lv_val = ls_sale-data_key-spart.
                WHEN 'KTOKD'. lv_val = comp( is_any = ls_c-central_data-central-data
                                             iv_fld = 'KTOKD' iv_fmt = '' ).
              ENDCASE.

            WHEN 'C'.
              lv_val = comp( is_any = ls_c-central_data-central-data
                             iv_fld = ls_col-fld iv_fmt = ls_col-fmt ).

            WHEN 'A'.
              lv_val = comp( is_any = ls_c-central_data-address-postal-data
                             iv_fld = ls_col-fld iv_fmt = ls_col-fmt ).
              " The template carries the title text, not the title key.
              IF ls_col-fmt = 'TT' AND lv_val IS NOT INITIAL.
                lv_val = lcl_src=>title_text( lv_val ).
              ENDIF.

            WHEN 'M'.
              CASE ls_col-fld.
                WHEN 'TEL' OR 'MOB'.
                  " R_3_USER is not a flag: SAP reads space and 1 as a
                  " landline and 2 and 3 as a mobile, and refuses anything
                  " else. Testing it against X never matched, so a mobile
                  " came back as a landline.
                  LOOP AT ls_c-central_data-address-communication-phone-phone INTO DATA(ls_ph).
                    DATA(lv_mob) = xsdbool( ls_ph-contact-data-r_3_user CA '23' ).
                    IF xsdbool( ls_col-fld = 'MOB' ) = lv_mob.
                      lv_val = ls_ph-contact-data-telephone.
                      EXIT.
                    ENDIF.
                  ENDLOOP.
                WHEN 'FAX'.
                  LOOP AT ls_c-central_data-address-communication-fax-fax INTO DATA(ls_fx).
                    lv_val = ls_fx-contact-data-fax.
                    EXIT.
                  ENDLOOP.
                WHEN 'SMT'.
                  LOOP AT ls_c-central_data-address-communication-smtp-smtp INTO DATA(ls_sm).
                    lv_val = ls_sm-contact-data-e_mail.
                    EXIT.
                  ENDLOOP.
              ENDCASE.

            WHEN 'B'.
              lv_val = comp( is_any = ls_comp-data iv_fld = ls_col-fld iv_fmt = ls_col-fmt ).

            WHEN 'S'.
              lv_val = comp( is_any = ls_sale-data iv_fld = ls_col-fld iv_fmt = ls_col-fmt ).

            WHEN 'T'.
              " A template that names the tax category is read by
              " category; one that does not is read by position, which is
              " the nth tax classification the customer carries.
              DATA lv_nth TYPE i.
              CLEAR lv_nth.
              IF ls_col-fld(1) = '#'.
                lv_nth = CONV i( ls_col-fld+1 ).
                READ TABLE ls_c-central_data-tax_ind-tax_ind INTO DATA(ls_tx) INDEX lv_nth.
                IF sy-subrc = 0.
                  lv_val = ls_tx-data-taxkd.
                ENDIF.
              ELSE.
                LOOP AT ls_c-central_data-tax_ind-tax_ind INTO ls_tx.
                  IF ls_tx-data_key-tatyp = ls_col-fld.
                    lv_val = ls_tx-data-taxkd.
                    EXIT.
                  ENDIF.
                ENDLOOP.
              ENDIF.

            WHEN 'Z'.
              lv_val = comp( is_any = ls_lic iv_fld = ls_col-fld iv_fmt = ls_col-fmt ).

            WHEN 'P'.
              lv_val = comp( is_any = ls_knvk iv_fld = ls_col-fld iv_fmt = ls_col-fmt ).

            WHEN 'I'.
              lv_val = lv_adh.
          ENDCASE.

          put( EXPORTING iv_col = ls_col-col iv_val = lv_val CHANGING cs_row = ls_row ).
        ENDLOOP.

        APPEND ls_row TO mt_row.
        add_msg( iv_key = is_key-kunnr iv_type = 'S'
                 iv_text = |Row { lines( mt_row ) }: { ls_comp-data_key-bukrs } | &&
                           |{ ls_sale-data_key-vkorg }/{ ls_sale-data_key-vtweg }/{ ls_sale-data_key-spart }| ).
        lv_si = lv_si + 1.
      ENDWHILE.
      lv_ci = lv_ci + 1.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_MAIN - the selection screen, the file, and the log
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* The upload half
*   The same template map read the other way round: a cell becomes the
*   field the map names, through the conversion the map carries. Brought
*   over from ZSDS_CUST_MASS_UPLOAD, whose seven hand-written layouts this
*   replaces - none of them matched the workbook Cipla now uses.
*----------------------------------------------------------------------*
CLASS lcl_log DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS add
      IMPORTING iv_row   TYPE i
                iv_kunnr TYPE clike OPTIONAL
                iv_bukrs TYPE clike OPTIONAL
                iv_vkorg TYPE clike OPTIONAL
                iv_type  TYPE bapi_mtype
                iv_text  TYPE clike
                iv_struc TYPE clike OPTIONAL
                iv_fld   TYPE clike OPTIONAL.

    " A creation is logged before its number exists; this puts the number
    " on the lines already written for that row so the list shows it.
    METHODS set_key
      IMPORTING iv_row   TYPE i
                iv_kunnr TYPE clike.

    METHODS has_error
      IMPORTING iv_row    TYPE i
      RETURNING VALUE(rv) TYPE abap_bool.

    METHODS counts
      EXPORTING ev_ok   TYPE i
                ev_err  TYPE i
                ev_skip TYPE i.

    METHODS display.
  PRIVATE SECTION.
    DATA mt_msg TYPE tt_ulog.
ENDCLASS.


CLASS lcl_log IMPLEMENTATION.

  METHOD add.
    APPEND VALUE ty_ulog(
      " A green light is an OUTCOME and nothing else. The lines that say what
      " the program noticed on the way used to wear one too, so a row that
      " failed showed green and red together and read as though half of it
      " had worked.
      icon    = COND #( WHEN iv_type = 'E' OR iv_type = 'A' THEN icon_red_light
                        WHEN iv_type = 'W'                  THEN icon_yellow_light
                        WHEN iv_type = 'S'                  THEN icon_green_light
                        ELSE                                     icon_information )
      xlsrow  = iv_row
      kunnr   = iv_kunnr
      bukrs   = iv_bukrs
      vkorg   = iv_vkorg
      msgty   = iv_type
      struc   = iv_struc
      fldnm   = iv_fld
      message = iv_text ) TO mt_msg.
  ENDMETHOD.

  METHOD set_key.
    IF iv_kunnr IS INITIAL.
      RETURN.
    ENDIF.
    LOOP AT mt_msg ASSIGNING FIELD-SYMBOL(<ls_m>) WHERE xlsrow = iv_row.
      IF <ls_m>-kunnr IS INITIAL.
        <ls_m>-kunnr = iv_kunnr.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD has_error.
    rv = xsdbool( line_exists( mt_msg[ xlsrow = iv_row msgty = 'E' ] )
               OR line_exists( mt_msg[ xlsrow = iv_row msgty = 'A' ] ) ).
  ENDMETHOD.

  METHOD counts.
    " Three outcomes, not two. A row is OK because something was DONE to it
    " and said so, not merely because nothing went wrong, and a line written
    " against row 0 is about the run rather than about a row.
    DATA lt_bad TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    DATA lt_win TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    DATA lt_all TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    LOOP AT mt_msg INTO DATA(ls).
      IF ls-xlsrow <= 0.
        CONTINUE.
      ENDIF.
      INSERT ls-xlsrow INTO TABLE lt_all.
      IF ls-msgty = 'E' OR ls-msgty = 'A'.
        INSERT ls-xlsrow INTO TABLE lt_bad.
      ELSEIF ls-msgty = 'S'.
        INSERT ls-xlsrow INTO TABLE lt_win.
      ENDIF.
    ENDLOOP.
    ev_err = lines( lt_bad ).
    LOOP AT lt_bad INTO DATA(lv_b).
      DELETE lt_win WHERE table_line = lv_b.
    ENDLOOP.
    ev_ok   = lines( lt_win ).
    ev_skip = lines( lt_all ) - ev_err - ev_ok.
  ENDMETHOD.

  METHOD display.
    IF mt_msg IS INITIAL.
      " Every way a run can end writes something here first, so this says
      " only what it knows rather than diagnosing a file it never read.
      MESSAGE 'The run ended without a single message - please send the file in.'
              TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.
    DATA lo_alv TYPE REF TO cl_salv_table.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING  t_table      = mt_msg ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->get_columns( )->set_optimize( abap_true ).
        DATA(lo_cols) = lo_alv->get_columns( ).
        lo_cols->get_column( 'ICON' )->set_short_text( 'Status' ).
        lo_cols->get_column( 'XLSROW' )->set_short_text( 'Excel row' ).
        lo_cols->get_column( 'STRUC' )->set_short_text( 'Structure' ).
        lo_cols->get_column( 'FLDNM' )->set_short_text( 'Field' ).
        lo_alv->display( ).
      CATCH cx_salv_msg cx_salv_not_found.
        LOOP AT mt_msg INTO DATA(ls_m).
          WRITE: / ls_m-xlsrow, ls_m-msgty, ls_m-message.
        ENDLOOP.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_cfg DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS get RETURNING VALUE(ro) TYPE REF TO lcl_cfg.

    METHODS cust_exists IMPORTING VALUE(iv_kunnr) TYPE kunnr
                        RETURNING VALUE(rv)       TYPE abap_bool.

    " The Business Partner behind a customer, through the CVI link. A change
    " has to name the partner it is changing, otherwise the API reads the
    " request as a creation and asks for a number.
    " The country of the address the customer already has. A postal code is
    " checked against its country's rules, so a row that changes the one
    " without naming the other has to carry it.
    METHODS cust_land1  IMPORTING VALUE(iv_kunnr) TYPE kunnr
                        RETURNING VALUE(rv)       TYPE land1.

    METHODS cust_guid   IMPORTING VALUE(iv_kunnr) TYPE kunnr
                        RETURNING VALUE(rv)       TYPE bu_partner_guid.
    METHODS cust_bp     IMPORTING VALUE(iv_kunnr) TYPE kunnr
                        RETURNING VALUE(rv)       TYPE bu_partner.

    " CVI customising: which business partner grouping and which BP roles a
    " customer account group creates. Maintained with SM30, views
    " CVIV_CUST_TO_BP1 and CVIV_CUST_TO_BP2. A creation must state the
    " grouping - it is what gives the new partner its number range.
    " The customer number that ended up behind a GUID we created.
    METHODS cust_by_guid IMPORTING VALUE(iv_guid) TYPE bu_partner_guid
                         RETURNING VALUE(rv)      TYPE kunnr.
    " The business partner behind the same GUID. Unless the grouping is
    " flagged for the same number in CVIC_CUST_TO_BP1, the partner is
    " numbered from its own range and does not match the customer - so the
    " run has to say which one it is.
    METHODS bp_by_guid   IMPORTING VALUE(iv_guid) TYPE bu_partner_guid
                         RETURNING VALUE(rv)      TYPE bu_partner.

    " The key column asks for a customer, but a user working in BP sees the
    " partner number and the two are only the same where the grouping is
    " flagged for it. A number that is not a customer is therefore tried as
    " a partner, and the customer behind it is what the row is applied to.
    METHODS cust_of
      IMPORTING VALUE(iv_in) TYPE kunnr
      EXPORTING ev_kunnr     TYPE kunnr
                ev_from_bp   TYPE bu_partner.

    METHODS bp_group    IMPORTING iv_ktokd  TYPE clike
                        RETURNING VALUE(rv) TYPE bu_group.
    TYPES tt_role TYPE STANDARD TABLE OF bu_role WITH EMPTY KEY.
    METHODS bp_roles    IMPORTING iv_ktokd  TYPE clike
                        RETURNING VALUE(rt) TYPE tt_role.

    " The API does not take a "modify" task on the customer side, so every
    " node has to say insert or update. These answer which one it is.
    " By value, not by reference: a by-reference parameter demands an actual
    " parameter of exactly the same type, and these are called with whatever
    " the row happened to give.
    METHODS has_knb1    IMPORTING VALUE(iv_kunnr) TYPE kunnr
                                  VALUE(iv_bukrs) TYPE bukrs
                        RETURNING VALUE(rv)       TYPE abap_bool.
    METHODS has_knvv    IMPORTING VALUE(iv_kunnr) TYPE kunnr
                                  VALUE(iv_vkorg) TYPE vkorg
                                  VALUE(iv_vtweg) TYPE vtweg
                                  VALUE(iv_spart) TYPE spart
                        RETURNING VALUE(rv)       TYPE abap_bool.
    METHODS has_knvi    IMPORTING VALUE(iv_kunnr) TYPE kunnr
                                  VALUE(iv_aland) TYPE land1
                                  VALUE(iv_tatyp) TYPE tatyp
                        RETURNING VALUE(rv)       TYPE abap_bool.
    METHODS has_role    IMPORTING VALUE(iv_partner) TYPE bu_partner
                                  VALUE(iv_role)    TYPE bu_role
                        RETURNING VALUE(rv)         TYPE abap_bool.
    METHODS has_ident   IMPORTING VALUE(iv_partner) TYPE bu_partner
                                  VALUE(iv_cat)     TYPE bu_id_type
                        RETURNING VALUE(rv)         TYPE abap_bool.

    " Title text -> title key (ADRC-TITLE). The templates carry the text
    " ("Company", "Mr."), the API wants the key.
    METHODS title_key   IMPORTING iv_text   TYPE clike
                        RETURNING VALUE(rv) TYPE ad_title.

    " Departure country for the tax classification. KNVI-ALAND is the
    " country of the sales organisation's company code, not the customer's.
    METHODS aland_of    IMPORTING iv_vkorg  TYPE vkorg
                        RETURNING VALUE(rv) TYPE land1.

    " Nth configured tax category for a country, in TSTL-LFDNR order. This
    " is what TAXKD_01..TAXKD_05 mean on the positional tabs.
    METHODS tax_cat_nth IMPORTING iv_aland  TYPE land1
                                  iv_nth    TYPE i
                        RETURNING VALUE(rv) TYPE tatyp.

    METHODS tax_cat_ok  IMPORTING iv_aland  TYPE land1
                                  iv_tatyp  TYPE tatyp
                        RETURNING VALUE(rv) TYPE abap_bool.

    " Check table of customer group 3. The API reports a failed check as a
    " bare "Entry X does not exist in TVV3", which says neither which field
    " nor where the value came from - so the value is checked here first.
    METHODS ok_kvgr3    IMPORTING iv        TYPE clike
                        RETURNING VALUE(rv) TYPE abap_bool.

    " Sales areas of a customer whose STORED customer group 3 is no longer
    " in TVV3. The API validates the whole customer, so one such row blocks
    " every update of that customer until it is corrected.
    TYPES: BEGIN OF ty_bad_sa,
             vkorg TYPE vkorg,
             vtweg TYPE vtweg,
             spart TYPE spart,
             kvgr3 TYPE kvgr3,
           END OF ty_bad_sa,
           tt_bad_sa TYPE STANDARD TABLE OF ty_bad_sa WITH EMPTY KEY.
    METHODS bad_kvgr3   IMPORTING VALUE(iv_kunnr) TYPE kunnr
                        RETURNING VALUE(rt)       TYPE tt_bad_sa.

    METHODS ok_ktokd    IMPORTING iv        TYPE clike
                        RETURNING VALUE(rv) TYPE abap_bool.

  PRIVATE SECTION.
    CLASS-DATA mo TYPE REF TO lcl_cfg.

    TYPES: BEGIN OF ty_tstl,
             talnd TYPE land1,
             lfdnr TYPE n LENGTH 3,
             tatyp TYPE tatyp,
           END OF ty_tstl.

    DATA mt_tstl  TYPE SORTED TABLE OF ty_tstl
                       WITH NON-UNIQUE KEY talnd lfdnr.
    DATA mt_ktokd TYPE SORTED TABLE OF ktokd WITH UNIQUE KEY table_line.
    DATA mt_kvgr3 TYPE SORTED TABLE OF kvgr3 WITH UNIQUE KEY table_line.

    TYPES: BEGIN OF ty_g2b, ktokd TYPE ktokd, grouping TYPE bu_group, END OF ty_g2b,
           BEGIN OF ty_r2b, ktokd TYPE ktokd, role     TYPE bu_role,  END OF ty_r2b.
    DATA mt_g2b TYPE SORTED TABLE OF ty_g2b WITH UNIQUE KEY ktokd.
    DATA mt_r2b TYPE SORTED TABLE OF ty_r2b WITH NON-UNIQUE KEY ktokd.

    METHODS constructor.
ENDCLASS.


CLASS lcl_cfg IMPLEMENTATION.

  METHOD get.
    IF mo IS INITIAL.
      mo = NEW lcl_cfg( ).
    ENDIF.
    ro = mo.
  ENDMETHOD.

  METHOD constructor.
    " MT_TSTL is NON-UNIQUE, so it can be filled directly.
    SELECT talnd, lfdnr, tatyp FROM tstl
      INTO CORRESPONDING FIELDS OF TABLE @mt_tstl.

    " Everything below is declared WITH UNIQUE KEY. Filling such a table
    " from a result set that contains duplicates raises ITAB_DUPLICATE_KEY,
    " which is a short dump and not catchable - so duplicates are removed
    " before the move, never after.
    "
    " These config tables are keyed on the code today, so DISTINCT is
    " belt and braces. The supplier program dumped on exactly this pattern
    " against T052, which does hold several rows per code.
    SELECT DISTINCT ktokd FROM t077d INTO TABLE @mt_ktokd.
    SELECT DISTINCT kvgr3 FROM tvv3 INTO TABLE @mt_kvgr3.

    " Keyed on the account group alone. Should the customising ever map one
    " account group to two groupings, INSERT reports it with SY-SUBRC 4 and
    " the first wins, instead of dumping on a duplicate key.
    SELECT account_group AS ktokd, grouping
      FROM cvic_cust_to_bp1 INTO TABLE @DATA(lt_g2b).
    LOOP AT lt_g2b INTO DATA(ls_g2b).
      INSERT VALUE ty_g2b( ktokd    = ls_g2b-ktokd
                           grouping = ls_g2b-grouping ) INTO TABLE mt_g2b.
    ENDLOOP.

    SELECT account_group AS ktokd, role
      FROM cvic_cust_to_bp2 INTO CORRESPONDING FIELDS OF TABLE @mt_r2b.
  ENDMETHOD.

  METHOD cust_exists.
    SELECT SINGLE @abap_true FROM kna1 WHERE kunnr = @iv_kunnr INTO @rv.
  ENDMETHOD.

  METHOD cust_land1.
    IF iv_kunnr IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE land1 FROM kna1 WHERE kunnr = @iv_kunnr INTO @rv.
  ENDMETHOD.

  METHOD cust_guid.
    SELECT SINGLE partner_guid FROM cvi_cust_link
      WHERE customer = @iv_kunnr INTO @rv.
  ENDMETHOD.

  METHOD bp_by_guid.
    IF iv_guid IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE partner FROM but000
      WHERE partner_guid = @iv_guid INTO @rv.
  ENDMETHOD.

  METHOD cust_of.
    CLEAR: ev_kunnr, ev_from_bp.
    IF iv_in IS INITIAL.
      RETURN.
    ENDIF.

    " A customer number wins - that is what the column asks for, and the
    " same digits can be a customer and, separately, someone else's partner.
    IF cust_exists( iv_in ) = abap_true.
      ev_kunnr = iv_in.
      RETURN.
    ENDIF.

    " BUT000-PARTNER and KNA1-KUNNR are both CHAR 10 with the ALPHA exit,
    " so the number as it stands can be looked up either way round.
    SELECT SINGLE partner_guid FROM but000
      WHERE partner = @iv_in INTO @DATA(lv_guid).
    IF sy-subrc <> 0 OR lv_guid IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE customer FROM cvi_cust_link
      WHERE partner_guid = @lv_guid INTO @ev_kunnr.
    IF ev_kunnr IS NOT INITIAL.
      ev_from_bp = iv_in.
    ENDIF.
  ENDMETHOD.

  METHOD cust_bp.
    SELECT SINGLE b~partner FROM but000 AS b
      INNER JOIN cvi_cust_link AS l ON l~partner_guid = b~partner_guid
      WHERE l~customer = @iv_kunnr INTO @rv.
  ENDMETHOD.

  METHOD has_knb1.
    SELECT SINGLE @abap_true FROM knb1
      WHERE kunnr = @iv_kunnr AND bukrs = @iv_bukrs INTO @rv.
  ENDMETHOD.

  METHOD has_knvv.
    SELECT SINGLE @abap_true FROM knvv
      WHERE kunnr = @iv_kunnr AND vkorg = @iv_vkorg
        AND vtweg = @iv_vtweg AND spart = @iv_spart INTO @rv.
  ENDMETHOD.

  METHOD has_knvi.
    SELECT SINGLE @abap_true FROM knvi
      WHERE kunnr = @iv_kunnr AND aland = @iv_aland AND tatyp = @iv_tatyp INTO @rv.
  ENDMETHOD.

  METHOD has_role.
    IF iv_partner IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE @abap_true FROM but100
      WHERE partner = @iv_partner AND rltyp = @iv_role INTO @rv.
  ENDMETHOD.

  METHOD has_ident.
    IF iv_partner IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE @abap_true FROM but0id
      WHERE partner = @iv_partner AND type = @iv_cat INTO @rv.
  ENDMETHOD.

  METHOD title_key.
    DATA(lv_t) = to_upper( condense( CONV string( iv_text ) ) ).
    IF lv_t IS INITIAL.
      RETURN.
    ENDIF.
    " Already a key (numeric, e.g. 0003) - take it as given.
    IF lv_t CO '0123456789'.
      rv = |{ lv_t ALPHA = IN WIDTH = 4 }|.
      RETURN.
    ENDIF.
    SELECT SINGLE title FROM tsad3t
      WHERE langu = @sy-langu AND title_medi = @lv_t
      INTO @rv.
    IF sy-subrc <> 0.
      SELECT SINGLE title FROM tsad3t
        WHERE title_medi = @lv_t
        INTO @rv.
    ENDIF.
  ENDMETHOD.

  METHOD aland_of.
    DATA lv_bukrs TYPE bukrs.
    SELECT SINGLE bukrs FROM tvko WHERE vkorg = @iv_vkorg INTO @lv_bukrs.
    IF sy-subrc = 0.
      SELECT SINGLE land1 FROM t001 WHERE bukrs = @lv_bukrs INTO @rv.
    ENDIF.
  ENDMETHOD.

  METHOD tax_cat_nth.
    DATA lv_n TYPE i.
    LOOP AT mt_tstl INTO DATA(ls) WHERE talnd = iv_aland.
      lv_n = lv_n + 1.
      IF lv_n = iv_nth.
        rv = ls-tatyp.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD tax_cat_ok.
    rv = xsdbool( line_exists( mt_tstl[ talnd = iv_aland tatyp = iv_tatyp ] ) ).
  ENDMETHOD.

  METHOD cust_by_guid.
    IF iv_guid IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE customer FROM cvi_cust_link
      WHERE partner_guid = @iv_guid INTO @rv.
  ENDMETHOD.

  METHOD bp_group.
    rv = VALUE #( mt_g2b[ ktokd = CONV ktokd( iv_ktokd ) ]-grouping OPTIONAL ).
  ENDMETHOD.

  METHOD bp_roles.
    LOOP AT mt_r2b INTO DATA(ls_r) WHERE ktokd = iv_ktokd.
      APPEND ls_r-role TO rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD ok_kvgr3.
    rv = xsdbool( line_exists( mt_kvgr3[ table_line = iv ] ) ).
  ENDMETHOD.

  METHOD bad_kvgr3.
    SELECT vkorg, vtweg, spart, kvgr3 FROM knvv
      WHERE kunnr = @iv_kunnr AND kvgr3 <> @space
      INTO TABLE @DATA(lt_sa).
    LOOP AT lt_sa INTO DATA(ls_sa).
      IF ok_kvgr3( ls_sa-kvgr3 ) = abap_false.
        APPEND CORRESPONDING #( ls_sa ) TO rt.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD ok_ktokd.
    rv = xsdbool( line_exists( mt_ktokd[ table_line = iv ] ) ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_cvis DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_log TYPE REF TO lcl_log.
    METHODS post
      IMPORTING is_data   TYPE cvis_ei_extern
                iv_row    TYPE i
                iv_kunnr  TYPE clike
      RETURNING VALUE(rv) TYPE abap_bool.
  PRIVATE SECTION.
    DATA mo_log TYPE REF TO lcl_log.

    " The business partner keeps a global memory for the logical unit of work
    " that has just been closed - including the save mode. A COMMIT does not
    " clear it, and the next row is then refused with "Parameter IV_X_SAVE is
    " ' ' for FM BUPA_CREATE_FROM_DATA. It should be 'A'". Initialising the
    " memory gives every row a clean start.
    METHODS reset_bp.
ENDCLASS.


CLASS lcl_cvis IMPLEMENTATION.

  METHOD constructor.
    mo_log = io_log.
  ENDMETHOD.

  METHOD reset_bp.
    TRY.
        CALL FUNCTION 'BUP_MEMORY_CENTRAL_INIT'.
      CATCH cx_sy_dyn_call_illegal_func.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD post.
    rv = abap_true.

    " ---- 1. validate. ET_RETURN_MAP carries BAPISTRUCNAME / BAPIFLDNM so
    "         the user can be pointed at the offending template column.
    DATA lt_map TYPE mdg_bs_bp_msgmap_t.
    TRY.
        cl_md_bp_maintain=>validate_single(
          EXPORTING i_data        = is_data
          IMPORTING et_return_map = lt_map ).
      CATCH cx_root INTO DATA(lx1).
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                     iv_text = |Validation failed: { lx1->get_text( ) }| ).
        rv = abap_false.
        RETURN.
    ENDTRY.

    LOOP AT lt_map INTO DATA(ls_map) WHERE type CA 'EAX'.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr
                   iv_type  = ls_map-type
                   iv_text  = ls_map-message
                   iv_struc = ls_map-bapistrucname
                   iv_fld   = ls_map-bapifldnm ).
      rv = abap_false.
    ENDLOOP.
    IF rv = abap_false.
      RETURN.
    ENDIF.

    " ---- 2. maintain. I_TEST_RUN is honoured by the API itself. --------
    DATA: lt_data TYPE cvis_ei_extern_t,
          lt_ret  TYPE bapiretm.
    APPEND is_data TO lt_data.

    TRY.
        cl_md_bp_maintain=>maintain(
          EXPORTING i_data     = lt_data
                    i_test_run = p_test
          IMPORTING e_return   = lt_ret ).
      CATCH cx_root INTO DATA(lx2).
        ROLLBACK WORK.
        reset_bp( ).
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                     iv_text = |Maintain failed: { lx2->get_text( ) }| ).
        rv = abap_false.
        RETURN.
    ENDTRY.

    " BAPIRETM lines carry a nested message table. Read it generically so a
    " release-dependent component name cannot break the program.
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
          mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr
                       iv_type = ls_r2-type iv_text = ls_r2-message
                       iv_fld  = ls_r2-field ).
          rv = abap_false.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF rv = abap_false.
      ROLLBACK WORK.
      reset_bp( ).
      RETURN.
    ENDIF.

    IF p_test = abap_true.
      ROLLBACK WORK.
      reset_bp( ).
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'S'
                   iv_text = 'Test run OK - customer would be posted' ).
    ELSE.
      " BAPI_TRANSACTION_COMMIT, not a bare COMMIT WORK: the business
      " partner hangs its own end-of-LUW processing off it. Its RETURN is
      " read, because it is the only thing that says whether the update
      " actually ran - without it "Customer posted" was a claim, not a fact.
      DATA ls_cret TYPE bapiret2.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING wait   = abap_true
        IMPORTING return = ls_cret.
      reset_bp( ).
      IF ls_cret-type CA 'EAX'.
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                     iv_text = |The update did not go through - nothing was saved | &&
                               |for this row: { ls_cret-message }| ).
        rv = abap_false.
        RETURN.
      ENDIF.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'S'
                   iv_text = 'Customer posted' ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_lic DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_log TYPE REF TO lcl_log.

    METHODS set
      IMPORTING iv_fld  TYPE clike
                iv_val  TYPE clike
                iv_cnv  TYPE clike
                iv_row  TYPE i.

    METHODS save
      IMPORTING iv_kunnr TYPE kunnr
                iv_row   TYPE i.

    METHODS reset.
    METHODS touched RETURNING VALUE(rv) TYPE abap_bool.
  PRIVATE SECTION.
    DATA mo_log    TYPE REF TO lcl_log.
    DATA ms_new    TYPE zsd_license_chk.
    DATA mt_fld    TYPE SORTED TABLE OF fieldname WITH UNIQUE KEY table_line.
ENDCLASS.


CLASS lcl_lic IMPLEMENTATION.

  METHOD constructor.
    mo_log = io_log.
  ENDMETHOD.

  METHOD reset.
    CLEAR ms_new.
    CLEAR mt_fld.
  ENDMETHOD.

  METHOD touched.
    rv = xsdbool( mt_fld IS NOT INITIAL ).
  ENDMETHOD.

  METHOD set.
    ASSIGN COMPONENT iv_fld OF STRUCTURE ms_new TO FIELD-SYMBOL(<lv>).
    IF sy-subrc <> 0.
      mo_log->add( iv_row = iv_row iv_type = 'E'
                   iv_struc = 'ZSD_LICENSE_CHK' iv_fld = iv_fld
                   iv_text = |Field { iv_fld } does not exist on ZSD_LICENSE_CHK| ).
      RETURN.
    ENDIF.

    DATA(lv_in) = condense( CONV string( iv_val ) ).
    IF lv_in = gc_clear.
      CLEAR <lv>.
      INSERT CONV fieldname( iv_fld ) INTO TABLE mt_fld.
      RETURN.
    ENDIF.
    IF lv_in IS INITIAL.
      RETURN.
    ENDIF.

    " The licence table has amounts and day counts in it, so the same guard
    " applies here: a cell that will not convert is logged, not dumped.
    TRY.
        CASE iv_cnv.
          WHEN 'DT'.
            DATA(lv_d) = lcl_util=>to_date( lv_in ).
            IF lv_d IS INITIAL.
              mo_log->add( iv_row = iv_row iv_type = 'E'
                           iv_struc = 'ZSD_LICENSE_CHK' iv_fld = iv_fld
                           iv_text = |"{ lv_in }" is not a valid date| ).
              RETURN.
            ENDIF.
            <lv> = lv_d.
          WHEN 'NM'.
            <lv> = lcl_util=>to_int( lv_in ).
          WHEN OTHERS.
            <lv> = lv_in.
        ENDCASE.
      CATCH cx_sy_conversion_error.
        mo_log->add( iv_row = iv_row iv_type = 'E'
                     iv_struc = 'ZSD_LICENSE_CHK' iv_fld = iv_fld
                     iv_text = |"{ lv_in }" does not fit { iv_fld }| ).
        RETURN.
    ENDTRY.
    INSERT CONV fieldname( iv_fld ) INTO TABLE mt_fld.
  ENDMETHOD.

  METHOD save.
    IF touched( ) = abap_false.
      RETURN.
    ENDIF.

    " ---- read the current row -----------------------------------------
    DATA ls_db TYPE zsd_license_chk.
    SELECT SINGLE * FROM zsd_license_chk
      WHERE kunnr = @iv_kunnr
      INTO @ls_db.
    DATA(lv_exists) = xsdbool( sy-subrc = 0 ).

    " ---- merge: only the columns this template carries -----------------
    ls_db-kunnr = iv_kunnr.
    LOOP AT mt_fld INTO DATA(lv_f).
      ASSIGN COMPONENT lv_f OF STRUCTURE ms_new TO FIELD-SYMBOL(<lv_s>).
      CHECK sy-subrc = 0.
      ASSIGN COMPONENT lv_f OF STRUCTURE ls_db  TO FIELD-SYMBOL(<lv_t>).
      CHECK sy-subrc = 0.
      <lv_t> = <lv_s>.
    ENDLOOP.

    IF p_test = abap_true.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'S'
                   iv_struc = 'ZSD_LICENSE_CHK'
                   iv_text = COND string(
                     WHEN lv_exists = abap_true
                     THEN |Test run OK - would update { lines( mt_fld ) } licence field(s)|
                     ELSE |Test run OK - would create the licence record| ) ).
      RETURN.
    ENDIF.

    " Authorised direct write - see the header comment of this report.
    MODIFY zsd_license_chk FROM ls_db.
    IF sy-subrc = 0.
      " The MODIFY succeeding is not the same as the record being there: the
      " COMMIT is what makes it so, and its SY-SUBRC is what says the work
      " went through.
      COMMIT WORK AND WAIT.
      DATA(lv_csub) = sy-subrc.
      IF lv_csub <> 0.
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                     iv_struc = 'ZSD_LICENSE_CHK'
                     iv_text = |The update was terminated (COMMIT WORK returned { lv_csub }) | &&
                               |- the licence record was not written| ).
        RETURN.
      ENDIF.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'S'
                   iv_struc = 'ZSD_LICENSE_CHK'
                   iv_text = COND string(
                     WHEN lv_exists = abap_true
                     THEN |Licence record updated ({ lines( mt_fld ) } field(s))|
                     ELSE 'Licence record created' ) ).
    ELSE.
      ROLLBACK WORK.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                   iv_struc = 'ZSD_LICENSE_CHK'
                   iv_text = 'Licence record could not be written' ).
    ENDIF.
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
      IF lv_hit IS INITIAL AND lines( lt_names ) = 1.
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


CLASS lcl_engine DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_tmpl TYPE char8
                io_log  TYPE REF TO lcl_log.
    METHODS run   IMPORTING it_row TYPE tt_row.

    " The headings this scenario expects, as matching keys. Used to find the
    " right tab whatever it is called.
    METHODS headings RETURNING VALUE(rt) TYPE string_table.

    " Re-points every map entry at the column that actually carries its
    " heading in this file. Entries whose heading is blank, duplicated or
    " absent keep the position they were built with.
    METHODS bind_columns IMPORTING it_head TYPE string_table.

  PRIVATE SECTION.
    DATA mv_tmpl  TYPE char8.
    DATA mo_log   TYPE REF TO lcl_log.
    DATA mo_cvis  TYPE REF TO lcl_cvis.
    DATA mo_lic   TYPE REF TO lcl_lic.
    " The template's columns, the same rows the download writes from.
    DATA mt_map   TYPE tt_col.

    " The customer a key cell names - see the method for what it resolves.
    " TYPE STRING, not CLIKE: LCL_UTIL=>ALPHA takes a string by reference
    " and a generically typed actual cannot reach it. Both callers read the
    " cell with LCL_UTIL=>CELL( ), which returns a string.
    METHODS key_kunnr
      IMPORTING iv_cell   TYPE string
                iv_row    TYPE i
      RETURNING VALUE(rv) TYPE kunnr.

    " Moves IV_VAL into component IV_FLD of CS_DATA and flags the matching
    " component of CS_DATAX. RETURNING cannot be combined with CHANGING, so
    " this reports problems through the log instead of a return code.
    METHODS set_comp
      IMPORTING iv_fld   TYPE clike
                iv_val   TYPE clike
                iv_cnv   TYPE clike
                iv_row   TYPE i
                iv_struc TYPE clike
      CHANGING  cs_data  TYPE any
                cs_datax TYPE any.

    " Moves one component of the customer's address into a differently
    " named component of the business partner, when it was filled.
    METHODS bp_copy
      IMPORTING iv_from  TYPE clike
                iv_to    TYPE clike
                iv_row   TYPE i
                is_post  TYPE any
      CHANGING  cs_data  TYPE any
                cs_datax TYPE any.

    METHODS master IMPORTING is_row TYPE ty_row.

    " The API validates the customer as a whole, so a value already stored
    " against one of its sales areas can reject an update that has nothing
    " to do with it. This says which one, instead of leaving the user with
    " a bare "Entry X does not exist in TVV3".
    METHODS warn_stored
      IMPORTING iv_kunnr TYPE kunnr
                iv_row   TYPE i
                is_sale  TYPE cmds_ei_sales.

    " Technical field names that occur only once in this scenario, and so
    " can identify a column on a file headed with field names.
    METHODS fld_keys RETURNING VALUE(rt) TYPE string_table.
ENDCLASS.


CLASS lcl_engine IMPLEMENTATION.

  METHOD constructor.
    mv_tmpl = iv_tmpl.
    mo_log  = io_log.
    mo_cvis = NEW lcl_cvis( io_log ).
    mo_lic  = NEW lcl_lic( io_log ).

    " The template's own columns, the same rows the download writes from.
    " HDR is squashed here rather than at every use: the download keeps the
    " workbook's heading as it stands, because it writes it into the file,
    " and the upload compares it with what a file carries, which is the same
    " heading typed by hand, re-cased, or with a space moved.
    "
    " A heading the template uses more than once - the five "Tax
    " classification for customer" columns - is kept too. BIND_COLUMNS
    " matches the nth in the file to the nth in the template, so a column
    " inserted or deleted in front of them cannot shift one tax
    " classification into the next.
    LOOP AT lcl_tmpl=>cols( iv_tmpl ) INTO DATA(ls_col).
      DATA ls_m TYPE ty_col.
      ls_m = ls_col.
      ls_m-hdr = lcl_util=>squash( ls_col-hdr ).
      APPEND ls_m TO mt_map.
    ENDLOOP.
  ENDMETHOD.

  METHOD headings.
    " Both spellings count: the heading the template carries ("Company
    " Code") and the technical field name ("BUKRS"). Files arrive headed
    " either way, and a field name is only usable where it occurs once.
    DATA lt_k TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    LOOP AT mt_map INTO DATA(ls_m).
      IF ls_m-hdr IS NOT INITIAL.
        INSERT CONV string( ls_m-hdr ) INTO TABLE lt_k.
      ENDIF.
    ENDLOOP.
    DATA(lt_fk) = fld_keys( ).
    LOOP AT lt_fk INTO DATA(lv_f).
      INSERT lv_f INTO TABLE lt_k.
    ENDLOOP.
    rt = VALUE #( FOR lv IN lt_k ( lv ) ).
  ENDMETHOD.

  METHOD fld_keys.
    TYPES: BEGIN OF ty_f, key TYPE string, n TYPE i, END OF ty_f.
    DATA lt_f TYPE SORTED TABLE OF ty_f WITH UNIQUE KEY key.
    LOOP AT mt_map INTO DATA(ls_m).
      DATA(lv_k) = lcl_util=>squash( ls_m-fld ).
      IF lv_k IS INITIAL.
        CONTINUE.
      ENDIF.
      READ TABLE lt_f ASSIGNING FIELD-SYMBOL(<ls_f>) WITH KEY key = lv_k.
      IF sy-subrc = 0.
        <ls_f>-n = <ls_f>-n + 1.
      ELSE.
        INSERT VALUE ty_f( key = lv_k n = 1 ) INTO TABLE lt_f.
      ENDIF.
    ENDLOOP.
    LOOP AT lt_f INTO DATA(ls_f) WHERE n = 1.
      APPEND ls_f-key TO rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD bind_columns.
    IF it_head IS INITIAL.
      RETURN.
    ENDIF.

    " ---- what the file's heading line holds ---------------------------
    " Every occurrence of every heading, in file order. A heading that
    " appears more than once is not thrown away: the second "Terms of
    " payment key" in the file belongs to the second one in the template.
    TYPES: BEGIN OF ty_h,   key TYPE string, col TYPE i, n TYPE i, END OF ty_h.
    TYPES: BEGIN OF ty_occ, key TYPE string, seq TYPE i, col TYPE i, END OF ty_occ.
    TYPES: BEGIN OF ty_bc,  col TYPE i,      key TYPE string, END OF ty_bc.
    DATA lt_cnt   TYPE SORTED TABLE OF ty_h   WITH UNIQUE KEY key.
    DATA lt_occ   TYPE SORTED TABLE OF ty_occ WITH UNIQUE KEY key seq.
    DATA lt_bycol TYPE SORTED TABLE OF ty_bc  WITH UNIQUE KEY col.

    LOOP AT it_head INTO DATA(lv_h).
      " The column number has to be taken here, before anything else runs.
      " READ TABLE on a sorted table is a binary search, and SAP sets
      " SY-TABIX to the position the key WOULD be inserted at when it finds
      " nothing - so reading SY-TABIX after it gives the heading's place in
      " the alphabet instead of its place in the file.
      DATA(lv_col) = sy-tabix.
      DATA(lv_k)   = lcl_util=>squash( lv_h ).
      IF lv_k IS INITIAL.
        CONTINUE.
      ENDIF.
      DATA lv_seq TYPE i.
      READ TABLE lt_cnt ASSIGNING FIELD-SYMBOL(<ls_c>) WITH KEY key = lv_k.
      IF sy-subrc = 0.
        <ls_c>-n = <ls_c>-n + 1.
        lv_seq   = <ls_c>-n.
      ELSE.
        INSERT VALUE ty_h( key = lv_k col = lv_col n = 1 ) INTO TABLE lt_cnt.
        lv_seq = 1.
      ENDIF.
      INSERT VALUE ty_occ( key = lv_k seq = lv_seq col = lv_col ) INTO TABLE lt_occ.
      INSERT VALUE ty_bc( col = lv_col key = lv_k ) INTO TABLE lt_bycol.
    ENDLOOP.

    " ---- and how often the template uses each heading ------------------
    DATA lt_mcnt TYPE SORTED TABLE OF ty_h WITH UNIQUE KEY key.
    LOOP AT mt_map INTO DATA(ls_c1) WHERE hdr IS NOT INITIAL.
      DATA(lv_ck) = CONV string( ls_c1-hdr ).
      READ TABLE lt_mcnt ASSIGNING FIELD-SYMBOL(<ls_mc>) WITH KEY key = lv_ck.
      IF sy-subrc = 0.
        <ls_mc>-n = <ls_mc>-n + 1.
      ELSE.
        INSERT VALUE ty_h( key = lv_ck n = 1 ) INTO TABLE lt_mcnt.
      ENDIF.
    ENDLOOP.

    DATA lt_done TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY table_line.
    DATA lt_used TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY table_line.
    DATA lt_seen TYPE SORTED TABLE OF ty_h WITH UNIQUE KEY key.
    DATA lv_moved TYPE i.
    DATA lv_namb  TYPE i.

    " ---- first pass: the heading the template carries above the column --
    " A repeated heading is matched by its occurrence, and only when the
    " file repeats it exactly as often as the template does - otherwise
    " there is no way to tell which is which and the column stays put.
    LOOP AT mt_map ASSIGNING FIELD-SYMBOL(<ls_m>) WHERE hdr IS NOT INITIAL.
      DATA(lv_ix) = sy-tabix.
      DATA(lv_key) = CONV string( <ls_m>-hdr ).

      DATA lv_mseq TYPE i.
      READ TABLE lt_seen ASSIGNING FIELD-SYMBOL(<ls_s>) WITH KEY key = lv_key.
      IF sy-subrc = 0.
        <ls_s>-n = <ls_s>-n + 1.
        lv_mseq  = <ls_s>-n.
      ELSE.
        INSERT VALUE ty_h( key = lv_key n = 1 ) INTO TABLE lt_seen.
        lv_mseq = 1.
      ENDIF.

      READ TABLE lt_cnt  INTO DATA(ls_fc) WITH KEY key = lv_key.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      READ TABLE lt_mcnt INTO DATA(ls_mc) WITH KEY key = lv_key.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      " The file must repeat the heading at least as often as the template
      " does; the nth in the template is then the nth in the file. Fewer in
      " the file than in the template means one of them was removed and
      " there is no telling which. Reading them by position would load each
      " with its neighbour's value, so they are left empty instead.
      IF ls_fc-n < ls_mc-n.
        <ls_m>-col = 0.
        lv_namb = lv_namb + 1.
        INSERT lv_ix INTO TABLE lt_done.
        CONTINUE.
      ENDIF.
      READ TABLE lt_occ INTO DATA(ls_o) WITH KEY key = lv_key seq = lv_mseq.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF ls_o-col <> <ls_m>-col.
        lv_moved = lv_moved + 1.
      ENDIF.
      <ls_m>-col = ls_o-col.
      INSERT lv_ix   INTO TABLE lt_done.
      INSERT ls_o-col INTO TABLE lt_used.
    ENDLOOP.

    " ---- second pass: the technical field name -------------------------
    " For files headed with field names rather than the template wording.
    " Only names that occur once in this scenario, once in the file, and
    " only columns no heading has already claimed.
    DATA(lt_fk) = fld_keys( ).
    LOOP AT mt_map ASSIGNING <ls_m>.
      DATA(lv_ix2) = sy-tabix.
      IF line_exists( lt_done[ table_line = lv_ix2 ] ).
        CONTINUE.
      ENDIF.
      DATA(lv_fk) = lcl_util=>squash( <ls_m>-fld ).
      IF lv_fk IS INITIAL OR NOT line_exists( lt_fk[ table_line = lv_fk ] ).
        CONTINUE.
      ENDIF.
      READ TABLE lt_cnt INTO ls_fc WITH KEY key = lv_fk.
      IF sy-subrc <> 0 OR ls_fc-n <> 1
         OR line_exists( lt_used[ table_line = ls_fc-col ] ).
        CONTINUE.
      ENDIF.
      IF ls_fc-col <> <ls_m>-col.
        lv_moved = lv_moved + 1.
      ENDIF.
      <ls_m>-col = ls_fc-col.
      INSERT lv_ix2   INTO TABLE lt_done.
      INSERT ls_fc-col INTO TABLE lt_used.
    ENDLOOP.

    IF lt_done IS INITIAL.
      " No heading in this file was recognised at all, so there is nothing
      " to say and nothing to protect against - the file is read exactly as
      " the template is laid out.
      RETURN.
    ENDIF.

    " ---- what is left is read by position ------------------------------
    " And a position another field has already been found at cannot be
    " read: on a file with a column inserted or removed it holds the
    " neighbour's value, and a wrong value is worse than none.
    DATA lv_miss  TYPE string.
    DATA lv_nmiss TYPE i.
    DATA lv_nblank TYPE i.
    LOOP AT mt_map ASSIGNING <ls_m>.
      DATA(lv_ix3) = sy-tabix.
      IF line_exists( lt_done[ table_line = lv_ix3 ] ).
        CONTINUE.
      ENDIF.
      lv_nmiss = lv_nmiss + 1.
      IF lv_nmiss <= 12.
        lv_miss = COND string( WHEN lv_miss IS INITIAL
                               THEN |{ <ls_m>-fld }({ <ls_m>-col })|
                               ELSE |{ lv_miss }, { <ls_m>-fld }({ <ls_m>-col })| ).
      ENDIF.
      " Two reasons not to read the position after all. Either another
      " field has already been found there, or the heading sitting there is
      " one this scenario knows and it belongs to a different field. Both
      " mean the column has moved and the position now holds someone else's
      " value, which is worse than none.
      DATA(lv_blank) = xsdbool( line_exists( lt_used[ table_line = <ls_m>-col ] ) ).
      IF lv_blank = abap_false.
        READ TABLE lt_bycol INTO DATA(ls_bc) WITH KEY col = <ls_m>-col.
        IF sy-subrc = 0
           AND ls_bc-key <> CONV string( <ls_m>-hdr )
           AND ls_bc-key <> lcl_util=>squash( <ls_m>-fld )
           AND ( line_exists( lt_mcnt[ key = ls_bc-key ] )
              OR line_exists( lt_fk[ table_line = ls_bc-key ] ) ).
          lv_blank = abap_true.
        ENDIF.
      ENDIF.
      IF lv_blank = abap_true.
        <ls_m>-col = 0.
        lv_nblank = lv_nblank + 1.
      ENDIF.
    ENDLOOP.

    " One line each, not one per column.
    IF lv_moved > 0.
      mo_log->add( iv_row = 0 iv_type = 'I'
                   iv_text = |{ lv_moved } column(s) sit elsewhere in this file than in | &&
                             |the template - each was read from where its heading is| ).
    ENDIF.
    IF lv_nmiss > 0.
      mo_log->add( iv_row = 0 iv_type = 'I'
                   iv_text = |{ lv_nmiss } column(s) carry no heading this program recognises | &&
                             |and were read by position: { lv_miss }| ).
    ENDIF.
    IF lv_nblank > 0.
      mo_log->add( iv_row = 0 iv_type = 'W'
                   iv_text = |{ lv_nblank } of those sit where another field was found, so they | &&
                             |were left empty rather than loaded with a neighbour's value - | &&
                             |give those columns their template heading| ).
    ENDIF.
    IF lv_namb > 0.
      mo_log->add( iv_row = 0 iv_type = 'W'
                   iv_text = |{ lv_namb } column(s) carry a heading the template repeats, and this | &&
                             |file repeats it fewer times - which is which cannot be told, so | &&
                             |they were left empty. Restore the template's columns| ).
    ENDIF.
  ENDMETHOD.

  METHOD run.
    LOOP AT it_row INTO DATA(ls_row).
      master( ls_row ).
      IF p_stop = abap_true AND mo_log->has_error( ls_row-row ) = abap_true.
        mo_log->add( iv_row = ls_row-row iv_type = 'W'
                     iv_text = 'Stopped at the first faulty row' ).
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD key_kunnr.
    " The customer a key cell names, whether it holds the customer number
    " or the business partner number. An unknown number is handed back as
    " it stands, so the caller's own "does not exist" check still speaks.
    rv = lcl_util=>alpha( iv_in = iv_cell iv_len = 10 ).
    IF rv IS INITIAL.
      RETURN.
    ENDIF.

    DATA: lv_kunnr TYPE kunnr,
          lv_bp    TYPE bu_partner.
    lcl_cfg=>get( )->cust_of( EXPORTING iv_in      = rv
                              IMPORTING ev_kunnr   = lv_kunnr
                                        ev_from_bp = lv_bp ).
    IF lv_bp IS INITIAL OR lv_kunnr IS INITIAL.
      RETURN.                            " a customer number, or neither
    ENDIF.

    mo_log->add( iv_row = iv_row iv_kunnr = lv_kunnr iv_type = 'I'
                 iv_text = |{ rv ALPHA = OUT } is business partner | &&
                           |{ lv_bp ALPHA = OUT } - customer | &&
                           |{ lv_kunnr ALPHA = OUT } is used| ).
    rv = lv_kunnr.
  ENDMETHOD.

  METHOD set_comp.
    ASSIGN COMPONENT iv_fld OF STRUCTURE cs_data TO FIELD-SYMBOL(<lv_t>).
    IF sy-subrc <> 0.
      mo_log->add( iv_row = iv_row iv_type = 'E'
                   iv_struc = iv_struc iv_fld = iv_fld
                   iv_text = |Field { iv_fld } does not exist on { iv_struc }| ).
      RETURN.
    ENDIF.

    DATA(lv_in) = condense( CONV string( iv_val ) ).

    IF lv_in = gc_clear.
      CLEAR <lv_t>.
    ELSEIF lv_in IS INITIAL.
      " A blank cell means "leave alone", so no DATAX flag is set.
      RETURN.
    ELSE.
      " Every branch below writes into a field the caller named at runtime,
      " so the target can be a number or a date. A cell that is not one
      " belongs in the log, not in a short dump.
      TRY.
        CASE iv_cnv.
          WHEN 'AL' OR 'GL'.
            " GL was never handled here, so the seven AKONT columns fell
            " through to WHEN OTHERS and reached the API with no conversion
            " at all. Both codes now take the same path.
            "
            " The padding length is read from the target field itself, so it
            " is always the real DDIC length - 10 for KUNNR, LIFNR, AKONT and
            " FDGRV, 6 for VBUND - and cannot drift out of step with a table.
            DATA(lv_len) = lcl_util=>char_len( <lv_t> ).
            IF lv_len > 0.
              <lv_t> = lcl_util=>alpha( iv_in = lv_in iv_len = lv_len ).
            ELSE.
              " not a character field, so there is nothing to pad
              <lv_t> = lv_in.
            ENDIF.
          WHEN 'DT'.
            DATA(lv_d) = lcl_util=>to_date( lv_in ).
            IF lv_d IS INITIAL.
              mo_log->add( iv_row = iv_row iv_type = 'E'
                           iv_struc = iv_struc iv_fld = iv_fld
                           iv_text = |"{ lv_in }" is not a valid date| ).
              RETURN.
            ENDIF.
            <lv_t> = lv_d.
          WHEN 'NM'.
            <lv_t> = lcl_util=>to_int( lv_in ).
          WHEN 'TT'.
            <lv_t> = lcl_cfg=>get( )->title_key( lv_in ).
          WHEN 'LG'.
            " Never through WHEN OTHERS: LANGU is one character, so a two
            " letter ISO code would be read there as a flag written out in
            " full - NO, the code for Norwegian, clears the field and JA,
            " the code for Japanese, sets it to X - and what survived that
            " would be cut to its first letter, filing ES under English.
            DATA(lv_lg) = lcl_util=>lang( lv_in ).
            IF lv_lg IS INITIAL.
              mo_log->add( iv_row = iv_row iv_type = 'W'
                           iv_struc = iv_struc iv_fld = iv_fld
                           iv_text = |"{ lv_in }" is not a language key - the field is left alone| ).
              RETURN.
            ENDIF.
            <lv_t> = lv_lg.
          WHEN OTHERS.
            " A word in a one character field is a flag written out in full.
            DATA(lv_w) = lcl_util=>char_len( <lv_t> ).
            IF lv_w = 1 AND strlen( lv_in ) > 1.
              <lv_t> = lcl_util=>flag( lv_in ).
            ELSE.
              <lv_t> = lv_in.
            ENDIF.
        ENDCASE.
        CATCH cx_sy_conversion_error.
          mo_log->add( iv_row = iv_row iv_type = 'E'
                       iv_struc = iv_struc iv_fld = iv_fld
                       iv_text = |"{ lv_in }" does not fit { iv_fld }| ).
          RETURN.
      ENDTRY.
    ENDIF.

    " DATAX carries the same component names as DATA.
    ASSIGN COMPONENT iv_fld OF STRUCTURE cs_datax TO FIELD-SYMBOL(<lv_x>).
    IF sy-subrc = 0.
      <lv_x> = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD bp_copy.
    FIELD-SYMBOLS <lv_v> TYPE any.
    ASSIGN COMPONENT iv_from OF STRUCTURE is_post TO <lv_v>.
    IF sy-subrc <> 0 OR <lv_v> IS INITIAL.
      RETURN.
    ENDIF.
    set_comp( EXPORTING iv_fld = iv_to iv_val = <lv_v>
                        iv_cnv = '' iv_row = iv_row iv_struc = 'BP'
              CHANGING  cs_data  = cs_data
                        cs_datax = cs_datax ).
  ENDMETHOD.

  METHOD master.
    DATA(lo_cfg) = lcl_cfg=>get( ).
    mo_lic->reset( ).

    " ---- 1. keys -------------------------------------------------------
    DATA: lv_kunnr TYPE kunnr,
          lv_bukrs TYPE bukrs,
          lv_vkorg TYPE vkorg,
          lv_vtweg TYPE vtweg,
          lv_spart TYPE spart,
          lv_ktokd TYPE ktokd.
    DATA: lv_kt   TYPE string,
          lv_code TYPE string,
          lv_rest TYPE string.

    LOOP AT mt_map INTO DATA(ls_k) WHERE node = gc_n_key.
      DATA(lv_v) = lcl_util=>cell( is_row = is_row iv_col = ls_k-col ).
      CASE ls_k-fld.
        WHEN 'KUNNR'. lv_kunnr = key_kunnr( iv_cell = lv_v iv_row = is_row-row ).
        WHEN 'BUKRS'. lv_bukrs = lv_v.
        WHEN 'VKORG'. lv_vkorg = lv_v.
        WHEN 'VTWEG'. lv_vtweg = lv_v.
        WHEN 'SPART'. lv_spart = lv_v.
        WHEN 'KTOKD'.
          " Some tabs put a picking list in this cell, for example
          " "ZDOM - Sold-to Customer" followed by more lines. Keep only the
          " code in front, and do it on the string before the value is
          " truncated into a CHAR4 field.
          lv_kt = lv_v.
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
                  IN lv_kt WITH ` `.
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
                  IN lv_kt WITH ` `.
          IF lv_kt CA ` `.
            SPLIT lv_kt AT ` ` INTO lv_code lv_rest.
            lv_kt = lv_code.
          ENDIF.
          lv_ktokd = lv_kt.
      ENDCASE.
    ENDLOOP.

    IF lv_ktokd IS NOT INITIAL AND lo_cfg->ok_ktokd( lv_ktokd ) = abap_false.
      mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'E'
                   iv_fld = 'KTOKD'
                   iv_text = |Account group { lv_ktokd } does not exist| ).
      RETURN.
    ENDIF.

    DATA(lv_exists) = COND abap_bool(
      WHEN lv_kunnr IS INITIAL THEN abap_false
      ELSE lo_cfg->cust_exists( lv_kunnr ) ).
    DATA(lv_task) = COND cmd_ei_object_task(
      WHEN lv_exists = abap_true THEN gc_u ELSE gc_i ).

    IF lv_kunnr IS INITIAL AND lv_ktokd IS INITIAL.
      mo_log->add( iv_row = is_row-row iv_type = 'E'
                   iv_text = 'Neither a customer number nor an account group is given' ).
      RETURN.
    ENDIF.

    " ---- 2. build the customer node ------------------------------------
    DATA ls_cust TYPE cmds_ei_extern.
    CLEAR ls_cust.
    ls_cust-header-object_instance-kunnr = lv_kunnr.
    ls_cust-header-object_task           = lv_task.
    ls_cust-central_data-central-data-ktokd  = lv_ktokd.
    ls_cust-central_data-central-datax-ktokd = abap_true.
    ls_cust-central_data-address-task        = lv_task.

    DATA ls_comp TYPE cmds_ei_company.
    DATA ls_sale TYPE cmds_ei_sales.
    CLEAR: ls_comp, ls_sale.
    " "Modify" is not a task the customer API accepts - every node has to
    " say whether it is an insert or an update, so each one is asked for.
    ls_comp-task = COND #( WHEN lv_exists = abap_true
                            AND lo_cfg->has_knb1( iv_kunnr = lv_kunnr
                                                  iv_bukrs = lv_bukrs ) = abap_true
                           THEN gc_u ELSE gc_i ).
    ls_comp-data_key-bukrs = lv_bukrs.
    ls_sale-task = COND #( WHEN lv_exists = abap_true
                            AND lo_cfg->has_knvv( iv_kunnr = lv_kunnr
                                                  iv_vkorg = lv_vkorg
                                                  iv_vtweg = lv_vtweg
                                                  iv_spart = lv_spart ) = abap_true
                           THEN gc_u ELSE gc_i ).
    ls_sale-data_key-vkorg = lv_vkorg.
    ls_sale-data_key-vtweg = lv_vtweg.
    ls_sale-data_key-spart = lv_spart.

    DATA(lv_aland) = lo_cfg->aland_of( lv_vkorg ).

    DATA: lv_tel TYPE string,
          lv_mob TYPE string,
          lv_fax TYPE string,
          lv_smt TYPE string,
          lv_adh TYPE string.

    LOOP AT mt_map INTO DATA(ls_m) WHERE node <> gc_n_key.
      DATA(lv_cell) = lcl_util=>cell( is_row = is_row iv_col = ls_m-col ).
      IF lv_cell IS INITIAL.
        CONTINUE.
      ENDIF.

      CASE ls_m-node.

        WHEN gc_n_cent.
          set_comp( EXPORTING iv_fld = ls_m-fld iv_val = lv_cell
                              iv_cnv = ls_m-cnv iv_row = is_row-row
                              iv_struc = 'KNA1'
                    CHANGING  cs_data  = ls_cust-central_data-central-data
                              cs_datax = ls_cust-central_data-central-datax ).

        WHEN gc_n_addr.
          set_comp( EXPORTING iv_fld = ls_m-fld iv_val = lv_cell
                              iv_cnv = ls_m-cnv iv_row = is_row-row
                              iv_struc = 'ADDRESS'
                    CHANGING  cs_data  = ls_cust-central_data-address-postal-data
                              cs_datax = ls_cust-central_data-address-postal-datax ).

        WHEN gc_n_comp.
          set_comp( EXPORTING iv_fld = ls_m-fld iv_val = lv_cell
                              iv_cnv = ls_m-cnv iv_row = is_row-row
                              iv_struc = 'KNB1'
                    CHANGING  cs_data  = ls_comp-data
                              cs_datax = ls_comp-datax ).

        WHEN gc_n_sale.
          IF ls_m-fld = 'KVGR3' AND lo_cfg->ok_kvgr3( lv_cell ) = abap_false.
            mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'E'
                         iv_struc = 'KNVV' iv_fld = 'KVGR3'
                         iv_text = |Customer group 3 "{ lv_cell }" (column { ls_m-col }) is not in TVV3 | &&
                                   |- maintain it with SM30 view V_TVV3 or correct the file| ).
            CONTINUE.
          ENDIF.
          set_comp( EXPORTING iv_fld = ls_m-fld iv_val = lv_cell
                              iv_cnv = ls_m-cnv iv_row = is_row-row
                              iv_struc = 'KNVV'
                    CHANGING  cs_data  = ls_sale-data
                              cs_datax = ls_sale-datax ).

        WHEN gc_n_comm.
          CASE ls_m-fld.
            WHEN 'TEL'. lv_tel = lv_cell.
            WHEN 'MOB'. lv_mob = lv_cell.
            WHEN 'FAX'. lv_fax = lv_cell.
            WHEN 'SMT'. lv_smt = lv_cell.
          ENDCASE.

        WHEN gc_n_tax.
          " The named tabs give the tax category outright; the positional
          " tabs give an ordinal (#1 .. #5) resolved against TSTL.
          DATA lv_tatyp TYPE tatyp.
          CLEAR lv_tatyp.
          IF ls_m-fld(1) = '#'.
            lv_tatyp = lo_cfg->tax_cat_nth(
                         iv_aland = lv_aland
                         iv_nth   = CONV i( ls_m-fld+1 ) ).
          ELSE.
            lv_tatyp = ls_m-fld.
          ENDIF.
          IF lv_tatyp IS INITIAL.
            mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'W'
                         iv_struc = 'KNVI' iv_fld = ls_m-fld
                         iv_text = |No tax category configured for country { lv_aland } - column { ls_m-col } skipped| ).
            CONTINUE.
          ENDIF.
          IF lo_cfg->tax_cat_ok( iv_aland = lv_aland iv_tatyp = lv_tatyp ) = abap_false.
            mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'E'
                         iv_struc = 'KNVI' iv_fld = lv_tatyp
                         iv_text = |Tax category { lv_tatyp } is not configured for country { lv_aland }| ).
            CONTINUE.
          ENDIF.
          DATA lv_ttask TYPE cmd_ei_object_task.
          lv_ttask = gc_i.
          IF lv_exists = abap_true
             AND lo_cfg->has_knvi( iv_kunnr = lv_kunnr
                                   iv_aland = lv_aland
                                   iv_tatyp = lv_tatyp ) = abap_true.
            lv_ttask = gc_u.
          ENDIF.
          APPEND VALUE cmds_ei_tax_ind(
            task              = lv_ttask
            data_key-aland    = lv_aland
            data_key-tatyp    = lv_tatyp
            data-taxkd        = lv_cell
            datax-taxkd       = abap_true ) TO ls_cust-central_data-tax_ind-tax_ind.

        WHEN gc_n_lic.
          mo_lic->set( iv_fld = ls_m-fld iv_val = lv_cell
                       iv_cnv = ls_m-cnv iv_row = is_row-row ).

        WHEN gc_n_iden.
          lv_adh = lv_cell.

        WHEN gc_n_cont.
          " A contact person is a business partner of its own in S/4HANA,
          " tied to the customer by a relationship - not a field on the
          " customer record. The ZDOC, ZDOD, ZDOF and European ZDOM
          " templates carry contact columns, and the program that this
          " replaces never loaded them either. They are reported rather than
          " dropped, so the file's owner knows to maintain them in BP.
          mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'W'
                       iv_struc = 'KNVK' iv_fld = ls_m-fld
                       iv_text = |Column { ls_m-col } ({ ls_m-fld }) is a contact person | &&
                                 |field - contact persons are separate business partners and | &&
                                 |are not loaded here; maintain them in transaction BP| ).

        WHEN gc_n_const OR '-'.
          " The transaction code the old recording carried, the "always X"
          " screen flags, and columns the map knows are not fields: the
          " download writes them so the file looks as the workbook does,
          " and there is nothing on the customer for them to go into.

      ENDCASE.
    ENDLOOP.

    " ---- 2a. a postal code is checked against its country ---------------
    " India's postal codes are six digits, Spain's five, and SAP tests the
    " one against the other. A row that gives a postal code but leaves the
    " country cell empty sends no country at all, and the code is then
    " measured against the wrong country - "Postal code has the wrong
    " length" on a perfectly good six digit code. The country the customer
    " already has is sent with it.
    IF ls_cust-central_data-address-postal-datax-postl_cod1 = abap_true
       AND ls_cust-central_data-address-postal-datax-country <> abap_true.
      DATA(lv_land) = lo_cfg->cust_land1( lv_kunnr ).
      IF lv_land IS INITIAL.
        mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'E'
                     iv_struc = 'ADDRESS' iv_fld = 'COUNTRY'
                     iv_text = 'A postal code needs its country - fill the country key column' ).
        RETURN.
      ENDIF.
      ls_cust-central_data-address-postal-data-country  = lv_land.
      ls_cust-central_data-address-postal-datax-country = abap_true.
      mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'I'
                   iv_struc = 'ADDRESS' iv_fld = 'COUNTRY'
                   iv_text = |No country in this row - the postal code is checked against { lv_land }| ).
    ENDIF.

    " ---- 3. communication ----------------------------------------------
    " The line type of CVIS_EI_PHONE_T is CVIS_EI_PHONE_STR, which wraps
    " CVIS_EI_PHONE in a component called CONTACT (plus a REMARK). The same
    " holds for fax and e-mail, so every field goes through CONTACT-.
    IF lv_tel IS NOT INITIAL.
      APPEND VALUE cvis_ei_phone_str(
               contact-task            = gc_i
               contact-data-telephone  = lv_tel
               contact-datax-telephone = abap_true
             ) TO ls_cust-central_data-address-communication-phone-phone.
    ENDIF.
    IF lv_mob IS NOT INITIAL.
      " A mobile number is a telephone entry marked as mobile in R_3_USER.
      " That field is not a flag: SAP's CL_ADDR_MAP=>CONVERT_ADTEL_TO_TELEPHONE
      " takes space or 1 as a landline, 2 or 3 as a mobile, and answers
      " anything else - an X included - with a type X message that brings the
      " transaction down before anything is written.
      APPEND VALUE cvis_ei_phone_str(
               contact-task            = gc_i
               contact-data-telephone  = lv_mob
               contact-data-r_3_user   = gc_mobile
               contact-datax-telephone = abap_true
               contact-datax-r_3_user  = abap_true
             ) TO ls_cust-central_data-address-communication-phone-phone.
    ENDIF.
    IF lv_fax IS NOT INITIAL.
      APPEND VALUE cvis_ei_fax_str(
               contact-task      = gc_i
               contact-data-fax  = lv_fax
               contact-datax-fax = abap_true
             ) TO ls_cust-central_data-address-communication-fax-fax.
    ENDIF.
    IF lv_smt IS NOT INITIAL.
      APPEND VALUE cvis_ei_smtp_str(
               contact-task         = gc_i
               contact-data-e_mail  = lv_smt
               contact-datax-e_mail = abap_true
             ) TO ls_cust-central_data-address-communication-smtp-smtp.
    ENDIF.

    " ---- 4. company code and sales area --------------------------------
    IF lv_bukrs IS NOT INITIAL.
      APPEND ls_comp TO ls_cust-company_data-company.
    ENDIF.
    IF lv_vkorg IS NOT INITIAL.
      APPEND ls_sale TO ls_cust-sales_data-sales.
    ENDIF.

    " ---- 5. the BP node: category, grouping, roles, Aadhaar ------------
    DATA ls_bp TYPE bus_ei_extern.
    CLEAR ls_bp.
    ls_bp-header-object_task     = lv_task.

    " The partner has to be identified in the message either way - the API
    " answers "Specify at least one number for the business partner"
    " otherwise (message R11 123). A change names the partner it changes by
    " its GUID. A creation has no number yet, because the number comes from
    " the grouping's range, so it is identified by a GUID generated here:
    " that GUID becomes the new partner's PARTNER_GUID.
    DATA lv_guid TYPE bu_partner_guid.
    IF lv_task = gc_i.
      TRY.
          lv_guid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
        CATCH cx_uuid_error INTO DATA(lx_uuid).
          DATA(lv_ut) = lx_uuid->get_text( ).
          mo_log->add( iv_row = is_row-row iv_type = 'E' iv_text = lv_ut ).
          RETURN.
      ENDTRY.
    ELSEIF lv_kunnr IS NOT INITIAL.
      lv_guid = lo_cfg->cust_guid( lv_kunnr ).
    ENDIF.
    IF lv_guid IS NOT INITIAL.
      ls_bp-header-object_instance-bpartnerguid = lv_guid.
    ENDIF.
    ls_bp-central_data-common-data-bp_control-category = gc_org.

    " A creation has to state the grouping: it is what gives the new partner
    " its number range, and without it the API answers "Specify at least one
    " number for the business partner". It is not derived from the account
    " group on its own - the mapping is CVI customising, SM30 view
    " CVIV_CUST_TO_BP1. The selection screen overrides it.
    IF lv_task = gc_i.
      DATA lv_grp TYPE bu_group.
      lv_grp = p_bpgrp.
      IF lv_grp IS INITIAL.
        lv_grp = lo_cfg->bp_group( lv_ktokd ).
      ENDIF.
      IF lv_grp IS INITIAL.
        mo_log->add( iv_row = is_row-row iv_type = 'E' iv_fld = 'KTOKD'
                     iv_text = |Account group { lv_ktokd } has no business partner grouping in | &&
                               |CVIC_CUST_TO_BP1 - maintain it with SM30 view CVIV_CUST_TO_BP1, | &&
                               |or give a grouping on the selection screen| ).
        RETURN.
      ENDIF.
      ls_bp-central_data-common-data-bp_control-grouping = lv_grp.
    ELSEIF p_bpgrp IS NOT INITIAL.
      ls_bp-central_data-common-data-bp_control-grouping = p_bpgrp.
    ENDIF.

    " The roles the account group creates are CVI customising too, SM30 view
    " CVIV_CUST_TO_BP2. Where that is not maintained, the two standard
    " customer roles are used: FI, plus SD when the row carries a sales area.
    DATA(lv_bp) = COND bu_partner( WHEN lv_task <> gc_i AND lv_kunnr IS NOT INITIAL
                                   THEN lo_cfg->cust_bp( lv_kunnr ) ).
    DATA(lt_roles) = lo_cfg->bp_roles( lv_ktokd ).
    IF lt_roles IS INITIAL.
      APPEND gc_role_fi TO lt_roles.
      IF lv_vkorg IS NOT INITIAL.
        APPEND gc_role_sd TO lt_roles.
      ENDIF.
    ENDIF.

    " A role the partner already has is an update, a new one an insert.
    DATA lv_rtask TYPE cmd_ei_object_task.
    LOOP AT lt_roles INTO DATA(lv_role).
      lv_rtask = COND #( WHEN lo_cfg->has_role( iv_partner = lv_bp
                                                iv_role    = lv_role ) = abap_true
                         THEN gc_u ELSE gc_i ).
      APPEND VALUE bus_ei_bupa_roles(
        task     = lv_rtask
        data_key = lv_role ) TO ls_bp-central_data-role-roles.
    ENDLOOP.

    " A new business partner needs a name and an address of its own. The
    " customer's are what they are, so they are copied across rather than
    " mapped a second time. On a change the partner already has both, and
    " CVI keeps them in step with the customer.
    IF lv_task = gc_i.
      " NAME -> NAME1 and so on: the customer address structure and the BP
      " organisation structure name the same things differently.
      bp_copy( EXPORTING iv_from = 'NAME'   iv_to = 'NAME1' iv_row = is_row-row
                         is_post = ls_cust-central_data-address-postal-data
               CHANGING  cs_data = ls_bp-central_data-common-data-bp_organization
                         cs_datax = ls_bp-central_data-common-datax-bp_organization ).
      bp_copy( EXPORTING iv_from = 'NAME_2' iv_to = 'NAME2' iv_row = is_row-row
                         is_post = ls_cust-central_data-address-postal-data
               CHANGING  cs_data = ls_bp-central_data-common-data-bp_organization
                         cs_datax = ls_bp-central_data-common-datax-bp_organization ).
      bp_copy( EXPORTING iv_from = 'NAME_3' iv_to = 'NAME3' iv_row = is_row-row
                         is_post = ls_cust-central_data-address-postal-data
               CHANGING  cs_data = ls_bp-central_data-common-data-bp_organization
                         cs_datax = ls_bp-central_data-common-datax-bp_organization ).
      bp_copy( EXPORTING iv_from = 'NAME_4' iv_to = 'NAME4' iv_row = is_row-row
                         is_post = ls_cust-central_data-address-postal-data
               CHANGING  cs_data = ls_bp-central_data-common-data-bp_organization
                         cs_datax = ls_bp-central_data-common-datax-bp_organization ).
      bp_copy( EXPORTING iv_from = 'SORT1' iv_to = 'SEARCHTERM1' iv_row = is_row-row
                         is_post = ls_cust-central_data-address-postal-data
               CHANGING  cs_data = ls_bp-central_data-common-data-bp_centraldata
                         cs_datax = ls_bp-central_data-common-datax-bp_centraldata ).
      bp_copy( EXPORTING iv_from = 'SORT2' iv_to = 'SEARCHTERM2' iv_row = is_row-row
                         is_post = ls_cust-central_data-address-postal-data
               CHANGING  cs_data = ls_bp-central_data-common-data-bp_centraldata
                         cs_datax = ls_bp-central_data-common-datax-bp_centraldata ).

      DATA ls_adr TYPE bus_ei_bupa_address.
      CLEAR ls_adr.
      ls_adr-task = gc_i.
      lcl_util=>copy_like(
        EXPORTING is_from  = ls_cust-central_data-address-postal-data
                  is_fromx = ls_cust-central_data-address-postal-datax
        CHANGING  cs_to    = ls_adr-data-postal-data
                  cs_tox   = ls_adr-data-postal-datax ).
      IF ls_adr-data-postal-datax IS NOT INITIAL.
        APPEND ls_adr TO ls_bp-central_data-address-addresses.
      ENDIF.
    ENDIF.

    IF lv_adh IS NOT INITIAL.
      DATA lv_itask TYPE cmd_ei_object_task.
      lv_itask = COND #( WHEN lo_cfg->has_ident( iv_partner = lv_bp
                                                 iv_cat     = CONV bu_id_type( gc_id_aadhaar ) ) = abap_true
                         THEN gc_u ELSE gc_i ).
      APPEND VALUE bus_ei_bupa_identification(
        task                            = lv_itask
        data_key-identificationcategory = gc_id_aadhaar
        data_key-identificationnumber   = lv_adh
      ) TO ls_bp-central_data-ident_number-ident_numbers.
    ENDIF.

    " ---- 6. post --------------------------------------------------------
    DATA ls_cvis TYPE cvis_ei_extern.
    CLEAR ls_cvis.
    ls_cvis-partner  = ls_bp.
    ls_cvis-customer = ls_cust.

    IF lv_exists = abap_true.
      warn_stored( iv_kunnr = lv_kunnr iv_row = is_row-row is_sale = ls_sale ).
    ENDIF.

    DATA(lv_ok) = mo_cvis->post( is_data  = ls_cvis
                                 iv_row   = is_row-row
                                 iv_kunnr = lv_kunnr ).

    " ---- 7. the licence record, only once the BP is safely in ----------
    " A customer created with internal numbering has its number only after
    " the save, and the GUID is what leads to it.
    IF lv_ok = abap_true AND lv_task = gc_i AND lv_kunnr IS INITIAL
       AND p_test = abap_false.
      lv_kunnr = lo_cfg->cust_by_guid( lv_guid ).
      IF lv_kunnr IS INITIAL.
        mo_log->add( iv_row = is_row-row iv_type = 'W'
                     iv_text = 'Posted, but the new customer number could not be read back from CVI_CUST_LINK' ).
      ELSE.
        mo_log->set_key( iv_row = is_row-row iv_kunnr = lv_kunnr ).
        DATA(lv_bp2)  = lo_cfg->bp_by_guid( lv_guid ).
        DATA(lv_made) = |Customer { lv_kunnr ALPHA = OUT } created|.
        IF lv_bp2 IS INITIAL.
          lv_made = lv_made && ' - but BUT000 holds no partner for its GUID; check the CVI link'.
        ELSE.
          lv_made = lv_made && | as business partner { lv_bp2 ALPHA = OUT }|.
        ENDIF.
        mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr
                     iv_type = COND #( WHEN lv_bp2 IS INITIAL THEN 'W' ELSE 'S' )
                     iv_text = lv_made ).
      ENDIF.
    ENDIF.

    IF lv_ok = abap_true AND mo_lic->touched( ) = abap_true.
      IF lv_kunnr IS INITIAL.
        mo_log->add( iv_row = is_row-row iv_type = 'W'
                     iv_struc = 'ZSD_LICENSE_CHK'
                     iv_text = 'Licence data skipped - the customer number is assigned internally and is not known in a test run; the productive run writes it' ).
      ELSE.
        mo_lic->save( iv_kunnr = lv_kunnr iv_row = is_row-row ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD warn_stored.
    DATA(lt_bad) = lcl_cfg=>get( )->bad_kvgr3( iv_kunnr ).
    LOOP AT lt_bad INTO DATA(ls_bad).
      " Corrected by this run if the file writes that same sales area.
      IF is_sale-datax-kvgr3     = abap_true
     AND is_sale-data_key-vkorg = ls_bad-vkorg
     AND is_sale-data_key-vtweg = ls_bad-vtweg
     AND is_sale-data_key-spart = ls_bad-spart.
        CONTINUE.
      ENDIF.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'W'
                   iv_struc = 'KNVV' iv_fld = 'KVGR3'
                   iv_text = |Sales area { ls_bad-vkorg }/{ ls_bad-vtweg }/{ ls_bad-spart } of this | &&
                             |customer already holds Customer group 3 "{ ls_bad-kvgr3 }", which is not | &&
                             |in TVV3. The API checks the whole customer, so it rejects the update until | &&
                             |that value is maintained in SM30 view V_TVV3 or replaced from the file| ).
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.


CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
    " The template the selection screen currently points at, empty when
    " the country and account group given are not covered.
    CLASS-METHODS chosen RETURNING VALUE(rv) TYPE char8.
    " The name the file and the sheet carry. LABEL reads the program's own
    " values, which is what PBO and PAI want; LABEL_SCREEN reads the screen,
    " which is what a value help wants.
    CLASS-METHODS label RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS label_screen RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS propose_file.
    " Fills the region dropdown. A listbox is filled once, before the screen
    " is shown, and the key it stores is the region - never a country key.
    CLASS-METHODS fill_regions.
    " The account groups of the region on the screen this moment.
    CLASS-METHODS f4_ktokd.
    " The region as it stands on the screen this moment.
    CLASS-METHODS screen_regn RETURNING VALUE(rv) TYPE char2.
    CLASS-METHODS screen_val IMPORTING iv_field  TYPE clike
                             RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS label_of IMPORTING iv_regn   TYPE clike
                                     iv_ktokd  TYPE clike
                           RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS validate.
    CLASS-METHODS run.
  PRIVATE SECTION.
    CLASS-METHODS download.
    CLASS-METHODS upload.
    CLASS-DATA mv_last TYPE string.
    CLASS-METHODS write IMPORTING iv_x TYPE xstring.
    CLASS-METHODS show  IMPORTING it_msg TYPE tt_msg.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD chosen.
    CASE abap_true.
      WHEN p_ext. rv = gc_tmpl_extn.
      WHEN p_blk. rv = gc_tmpl_blk.
      WHEN OTHERS.
        rv = lcl_tmpl=>resolve( iv_regn = p_regn iv_ktokd = p_ktokd ).
    ENDCASE.
  ENDMETHOD.

  METHOD label_of.
    CASE abap_true.
      WHEN p_ext. rv = 'CUST_EXTN'.
      WHEN p_blk. rv = 'BLOCK_UNBLOCK'.
      WHEN OTHERS.
        " The file keeps the country in its name, which is what the MDM
        " team files it under. Europe's four countries share one template,
        " so the region stands in its own name there.
        DATA(lv_land) = lcl_tmpl=>land_of( CONV char2( iv_regn ) ).
        rv = |{ lv_land }_{ iv_ktokd }|.
        IF iv_regn = 'EU'.
          rv = |EUROPE_{ iv_ktokd }|.
        ENDIF.
        IF iv_regn IS INITIAL OR iv_ktokd IS INITIAL.
          rv = 'CUSTOMER'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD label.
    rv = label_of( iv_regn = p_regn iv_ktokd = p_ktokd ).
  ENDMETHOD.

  METHOD label_screen.
    " The template radio buttons carry USER-COMMAND, so a click on one has
    " already been through PAI and the program holds the current choice. The
    " country and the account group are plain input fields with no round trip
    " of their own, so those two are read off the screen.
    rv = label_of( iv_regn = screen_regn( ) iv_ktokd = screen_val( 'P_KTOKD' ) ).
  ENDMETHOD.

  METHOD propose_file.
    " Keep the folder the user chose and swap the file name, so that
    " changing the country or the account group does not quietly leave
    " the previous template's name on the file.
    "
    " Not on an upload: there the file is the one the user picked, and
    " renaming it after the region would point the run at a file that
    " does not exist.
    IF p_up = abap_true.
      RETURN.
    ENDIF.
    DATA(lv_now) = label( ).
    IF lv_now = mv_last AND p_file IS NOT INITIAL.
      RETURN.
    ENDIF.
    mv_last = lv_now.

    DATA(lv_old) = CONV string( p_file ).
    DATA(lv_dir) = `C:\temp\`.
    DATA(lv_i)   = strlen( lv_old ).
    WHILE lv_i > 0.
      lv_i = lv_i - 1.
      IF lv_old+lv_i(1) = '\' OR lv_old+lv_i(1) = '/'.
        lv_dir = lv_old(lv_i) && lv_old+lv_i(1).
        EXIT.
      ENDIF.
    ENDWHILE.
    p_file = |{ lv_dir }{ lv_now }.xlsx|.
  ENDMETHOD.

  METHOD fill_regions.
    " The dropdown is filled once, before the screen is shown. Its key is the
    " region and its text carries the country in brackets, so the user picks
    " "Europe (GB/BE/ES/NL)" and never has to know that Dubai is AE.
    DATA lt_vrm TYPE vrm_values.
    LOOP AT lcl_tmpl=>regions( ) INTO DATA(ls_rg).
      APPEND VALUE vrm_value( key = ls_rg-regn text = ls_rg-text ) TO lt_vrm.
    ENDLOOP.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING  id     = 'P_REGN'
                 values = lt_vrm
      EXCEPTIONS OTHERS = 1.
  ENDMETHOD.

  METHOD f4_ktokd.
    " Only the account groups the chosen region actually has. With the region
    " picked from a list there is one field left to fill, so the value comes
    " back the ordinary way and no second field has to be written behind it.
    DATA(lv_regn) = screen_regn( ).
    IF lv_regn IS INITIAL.
      MESSAGE 'Choose a region first' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    DATA(lt_grp) = lcl_tmpl=>groups( lv_regn ).
    IF lt_grp IS INITIAL.
      MESSAGE |No customer template exists for { lcl_tmpl=>region_text( lv_regn ) }|
              TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    TYPES: BEGIN OF ty_f4,
             ktokd TYPE ktokd,
             txt30 TYPE text30,
             cols  TYPE char5,
           END OF ty_f4.
    DATA lt_f4 TYPE STANDARD TABLE OF ty_f4 WITH EMPTY KEY.

    " The whole (small) text table rather than FOR ALL ENTRIES: LT_GRP is a
    " table of a single field, and FOR ALL ENTRIES has no component to drive
    " itself from there.
    SELECT ktokd, txt30 FROM t077x
      WHERE spras = @sy-langu
      INTO TABLE @DATA(lt_txt).

    LOOP AT lt_grp INTO DATA(lv_k).
      DATA ls_f4 TYPE ty_f4.
      CLEAR ls_f4.
      ls_f4-ktokd = lv_k.
      READ TABLE lt_txt INTO DATA(ls_t) WITH KEY ktokd = lv_k.
      IF sy-subrc = 0.
        ls_f4-txt30 = ls_t-txt30.
      ENDIF.
      ls_f4-cols = lines( lcl_tmpl=>cols(
        lcl_tmpl=>resolve( iv_regn = lv_regn iv_ktokd = lv_k ) ) ).
      SHIFT ls_f4-cols LEFT DELETING LEADING '0'.
      APPEND ls_f4 TO lt_f4.
    ENDLOOP.
    SORT lt_f4 BY ktokd.

    " WINDOW_TITLE is a C field on this function module, and a classic
    " function module takes nothing else there - a string template builds a
    " STRING and the call dies with CALL_FUNCTION_CONFLICT_GEN_TYP before it
    " has done anything. The title is built into a character field first.
    DATA lv_title TYPE char70.
    lv_title = |Account groups for { lcl_tmpl=>region_text( lv_regn ) }|.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING  retfield     = 'KTOKD'
                 dynpprog     = sy-repid
                 dynpnr       = sy-dynnr
                 dynprofield  = 'P_KTOKD'
                 window_title = lv_title
                 value_org    = 'S'
      TABLES     value_tab    = lt_f4
      EXCEPTIONS OTHERS       = 1.
  ENDMETHOD.

  METHOD screen_regn.
    rv = screen_val( 'P_REGN' ).
    IF rv IS INITIAL.
      rv = p_regn.
    ENDIF.
  ENDMETHOD.

  METHOD screen_val.
    " A value help runs before the screen has been handed to the program, so
    " the program variable still holds whatever the last round trip left in
    " it - nothing at all the first time the screen is used. The value has to
    " be read off the screen itself.
    DATA lt_dynp TYPE TABLE OF dynpread.
    DATA ls_dynp TYPE dynpread.
    ls_dynp-fieldname = iv_field.
    APPEND ls_dynp TO lt_dynp.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING  dyname             = sy-repid
                 dynumb             = sy-dynnr
                 translate_to_upper = abap_true
      TABLES     dynpfields         = lt_dynp
      EXCEPTIONS OTHERS             = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    READ TABLE lt_dynp INTO ls_dynp INDEX 1.
    IF sy-subrc = 0.
      rv = condense( ls_dynp-fieldvalue ).
    ENDIF.
  ENDMETHOD.

  METHOD validate.
    IF p_crt = abap_true.
      IF p_regn IS INITIAL.
        MESSAGE 'Choose a region' TYPE 'E'.
      ENDIF.
      IF p_ktokd IS INITIAL.
        MESSAGE 'Give a customer account group' TYPE 'E'.
      ENDIF.
      " A region the workbook does not cover, and a region it does cover but
      " not with this account group, are two different messages - the user
      " should not have to guess which of the two is wrong.
      IF lcl_tmpl=>groups( p_regn ) IS INITIAL.
        MESSAGE |No customer template exists for { lcl_tmpl=>region_text( p_regn ) }|
                TYPE 'E'.
      ENDIF.
      IF lcl_tmpl=>resolve( iv_regn = p_regn iv_ktokd = p_ktokd ) IS INITIAL.
        MESSAGE |No customer template exists for { lcl_tmpl=>region_text( p_regn ) } | &&
                |with account group { p_ktokd }| TYPE 'E'.
      ENDIF.
    ENDIF.

    " Which customers go into the file is a download question only - an
    " upload takes whatever rows the file holds.
    IF p_down = abap_true.
      IF p_empty = abap_false AND s_bp[] IS INITIAL AND s_kunnr[] IS INITIAL.
        MESSAGE 'Give a business partner or a customer - or tick "Template only"' TYPE 'E'.
      ENDIF.
      IF p_max < 1.
        MESSAGE 'Rows at most must be 1 or more' TYPE 'E'.
      ENDIF.
    ENDIF.
    IF p_up = abap_true AND p_skip < 1.
      MESSAGE 'Heading rows to skip must be 1 or more' TYPE 'E'.
    ENDIF.
    IF p_file IS INITIAL.
      " MESSAGE takes a data object, not an expression.
      DATA(lv_nofile) = COND string( WHEN p_up = abap_true
                                     THEN `Pick the filled template to upload`
                                     ELSE `Give a file name` ).
      MESSAGE lv_nofile TYPE 'E'.
    ENDIF.
    " Only .xlsx can be read - CL_FDT_XL_SPREADSHEET reads the OpenXML
    " package and nothing else - and the end of the name is what says so.
    IF p_up = abap_true.
      DATA(lv_ext) = to_upper( CONV string( p_file ) ).
      IF lv_ext NP '*.XLSX'.
        MESSAGE 'Only .xlsx can be uploaded - open the file in Excel and save it as "Excel Workbook (*.xlsx)"'
                TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD write.
    DATA lt_bin TYPE solix_tab.
    DATA lv_len TYPE i.
    lv_len = xstrlen( iv_x ).
    lt_bin = cl_bcs_convert=>xstring_to_solix( iv_x ).

    IF p_pc = abap_true.
      DATA(lv_name) = CONV string( p_file ).
      cl_gui_frontend_services=>gui_download(
        EXPORTING bin_filesize = lv_len
                  filename     = lv_name
                  filetype     = 'BIN'
        CHANGING  data_tab     = lt_bin
        EXCEPTIONS OTHERS      = 1 ).
      IF sy-subrc <> 0.
        MESSAGE |The file could not be written to { p_file }| TYPE 'E'.
      ENDIF.
    ELSE.
      OPEN DATASET p_file FOR OUTPUT IN BINARY MODE.
      IF sy-subrc <> 0.
        MESSAGE |The file could not be opened on the server: { p_file }| TYPE 'E'.
      ENDIF.
      TRANSFER iv_x TO p_file.
      CLOSE DATASET p_file.
    ENDIF.
  ENDMETHOD.

  METHOD show.
    IF it_msg IS INITIAL.
      RETURN.
    ENDIF.
    DATA lt_msg TYPE tt_msg.
    lt_msg = it_msg.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                                CHANGING  t_table      = lt_msg ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->get_columns( )->set_optimize( abap_true ).
        lo_alv->get_columns( )->get_column( 'OBJKEY' )->set_short_text( 'Customer' ).
        lo_alv->display( ).
      CATCH cx_salv_msg cx_salv_not_found.
        LOOP AT lt_msg INTO DATA(ls_m).
          WRITE: / ls_m-objkey, ls_m-message.
        ENDLOOP.
    ENDTRY.
  ENDMETHOD.

  METHOD run.
    IF p_up = abap_true.
      upload( ).
    ELSE.
      download( ).
    ENDIF.
  ENDMETHOD.

  METHOD upload.
    DATA(lv_tmpl) = chosen( ).
    IF lv_tmpl IS INITIAL.
      MESSAGE |No customer template exists for { lcl_tmpl=>region_text( p_regn ) } | &&
              |with account group { p_ktokd }| TYPE 'E'.
      RETURN.
    ENDIF.

    " The run's own authorisation. Downloading is a read; uploading changes
    " master data, and one transaction cannot ask S_TCODE two different
    " questions, so the program asks this one itself.
    AUTHORITY-CHECK OBJECT 'F_KNA1_BUK' ID 'BUKRS' DUMMY ID 'ACTVT' FIELD '02'.
    IF sy-subrc <> 0.
      MESSAGE 'No authorisation to change customer master data (F_KNA1_BUK)' TYPE 'E'.
      RETURN.
    ENDIF.

    DATA(lo_log) = NEW lcl_log( ).
    DATA(lo_eng) = NEW lcl_engine( iv_tmpl = lv_tmpl io_log = lo_log ).

    DATA lt_row  TYPE tt_row.
    DATA lt_head TYPE string_table.
    DATA gv_tab  TYPE string.
    TRY.
        NEW lcl_excel( )->read(
          EXPORTING iv_file    = CONV string( p_file )
                    iv_from_pc = p_pc
                    iv_sheet   = label( )
                    iv_skip    = p_skip
                    it_want    = lo_eng->headings( )
          IMPORTING et_head    = lt_head
                    et_row     = lt_row
                    ev_sheet   = gv_tab ).
      CATCH lcx_dl INTO DATA(lx_up).
        " The reason goes in the list, where it stays on screen. A message
        " here would be overwritten by whatever the end of the run says.
        lo_log->add( iv_row = 0 iv_type = 'E' iv_text = lx_up->get_text( ) ).
        lo_log->display( ).
        RETURN.
    ENDTRY.

    IF lt_row IS INITIAL.
      lo_log->add( iv_row = 0 iv_type = 'E'
                   iv_text = |Tab "{ gv_tab }" holds no data below its heading row| ).
      lo_log->display( ).
      RETURN.
    ENDIF.

    lo_eng->bind_columns( lt_head ).
    lo_eng->run( lt_row ).

    DATA lv_ok   TYPE i.
    DATA lv_err  TYPE i.
    DATA lv_skip TYPE i.
    lo_log->counts( IMPORTING ev_ok = lv_ok ev_err = lv_err ev_skip = lv_skip ).
    " MESSAGE takes a data object, not an expression.
    DATA(lv_sum) = |{ label( ) }: { lines( lt_row ) } row(s) read, { lv_ok } processed, | &&
                   |{ lv_err } with errors| &&
                   COND string( WHEN lv_skip > 0 THEN |, { lv_skip } skipped| ELSE `` ).
    MESSAGE lv_sum TYPE 'S'.
    lo_log->display( ).
  ENDMETHOD.

  METHOD download.
    DATA(lv_tmpl) = chosen( ).
    IF lv_tmpl IS INITIAL.
      MESSAGE |No customer template exists for { lcl_tmpl=>region_text( p_regn ) } | &&
              |with account group { p_ktokd }| TYPE 'E'.
      RETURN.
    ENDIF.

    DATA(lo_eng) = NEW lcl_eng( lv_tmpl ).

    DATA lt_row TYPE tt_row.
    IF p_empty = abap_false.
      lo_eng->run( ).
      lt_row = lo_eng->rows( ).
    ENDIF.

    DATA(lt_head) = lo_eng->head( ).

    TRY.
        DATA(lv_x) = lcl_xlsx=>build( iv_sheet = label( )
                                      it_head  = lt_head
                                      it_row   = lt_row ).
        write( lv_x ).
      CATCH lcx_dl INTO DATA(lx).
        DATA(lv_t) = lx->get_text( ).
        MESSAGE lv_t TYPE 'E'.
    ENDTRY.

    MESSAGE |{ label( ) }: { lines( lt_head ) } column(s), | &&
            |{ lines( lt_row ) } data row(s) written to { p_file }| TYPE 'S'.
    show( lo_eng->log( ) ).
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Selection-screen events
*----------------------------------------------------------------------*
INITIALIZATION.
  " A listbox is filled before the screen is shown, not while it is on it.
  lcl_main=>fill_regions( ).
  lcl_main=>propose_file( ).

AT SELECTION-SCREEN OUTPUT.
  " The region and the account group only apply to the customer create
  " template; the extension and the block / unblock templates are the same
  " everywhere.
  LOOP AT SCREEN.
    IF screen-name = 'P_REGN' OR screen-name = 'P_KTOKD'.
      IF p_crt = abap_true.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    " Which customers to put in the file is a download question; how to
    " post what is read is an upload one. Each block is closed while the
    " other direction is chosen, so nobody fills in options that do nothing.
    CASE screen-name.
      WHEN 'S_BP-LOW' OR 'S_KUNNR-LOW' OR 'P_MAX' OR 'P_EMPTY'
        OR '%_S_BP_%_APP_%-VALU_PUSH' OR '%_S_KUNNR_%_APP_%-VALU_PUSH'.
        screen-input = COND #( WHEN p_down = abap_true THEN 1 ELSE 0 ).
        MODIFY SCREEN.
      WHEN 'P_TEST' OR 'P_STOP' OR 'P_BPGRP' OR 'P_SKIP'.
        screen-input = COND #( WHEN p_up = abap_true THEN 1 ELSE 0 ).
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.
  " Show the file name that belongs to the template now selected. The
  " radio button group carries USER-COMMAND, so a click comes back here.
  lcl_main=>propose_file( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ktokd.
  lcl_main=>f4_ktokd( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: lv_path TYPE string,
        lv_name TYPE string,
        lv_full TYPE string.
  " An upload reads a file that is already there, so it is picked, not
  " named. The mode is read off the screen: the radio button carries a
  " USER-COMMAND, so the program already holds the current choice.
  IF p_up = abap_true.
    DATA lt_ft TYPE filetable.
    DATA lv_rc TYPE i.
    DATA lv_ua TYPE i.
    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING window_title = 'Select the filled template'
                file_filter  = |Excel workbook (*.xlsx)\|*.xlsx\||
      CHANGING  file_table   = lt_ft
                rc           = lv_rc
                user_action  = lv_ua
      EXCEPTIONS OTHERS      = 1 ).
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
  cl_gui_frontend_services=>file_save_dialog(
    EXPORTING window_title      = 'Save the template'
              default_extension = 'xlsx'
              default_file_name = |{ lcl_main=>label_screen( ) }.xlsx|
              file_filter       = |Excel workbook (*.xlsx)\|*.xlsx\||
    CHANGING  filename          = lv_name
              path              = lv_path
              fullpath          = lv_full
    EXCEPTIONS OTHERS           = 1 ).
  IF sy-subrc = 0 AND lv_full IS NOT INITIAL.
    " A path longer than the parameter is refused rather than silently cut
    " short - a cut short path writes the file somewhere else, or not at all.
    IF strlen( lv_full ) > 255.
      MESSAGE 'That path is longer than 255 characters - pick a shorter folder' TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      p_file = lv_full.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN.
  " Runs before START-OF-SELECTION as well, so the file name is right even
  " when the user picks a template and presses F8 without a refresh.
  lcl_main=>propose_file( ).

  " A radio button click is only that refresh - the user has not asked for
  " anything yet, so there is nothing to complain about.
  CHECK sscrfields-ucomm <> 'RB' AND sscrfields-ucomm <> 'MD' AND sscrfields-ucomm <> 'RG'.
  lcl_main=>validate( ).

*----------------------------------------------------------------------*
* Main
*----------------------------------------------------------------------*
START-OF-SELECTION.
  lcl_main=>run( ).
