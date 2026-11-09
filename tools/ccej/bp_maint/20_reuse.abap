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


