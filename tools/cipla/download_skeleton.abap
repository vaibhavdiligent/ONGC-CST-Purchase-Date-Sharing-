*&---------------------------------------------------------------------*
*& Report  ZSDS_CUST_TMPL_DOWNLOAD
*&---------------------------------------------------------------------*
*& Downloads a customer master template, empty or filled with the data of
*& the customers asked for.
*&
*& LSMW is gone in S/4HANA - XD01 no longer exists and the customer is
*& maintained as a business partner - so the templates Cipla used to load
*& through it are now loaded through an Excel upload program. This report
*& is the other half: it writes the same workbook out, so a user can take
*& a template, or take the data that is already in the system, edit it and
*& load it back.
*&
*& Pick a country and an account group and the template follows: the
*& workbook Cipla supplied holds 24 layouts over 81 country and account
*& group combinations, and a country and an account group name exactly one
*& of them. A combination the workbook does not cover is refused on the
*& selection screen rather than producing an empty file.
*&
*& The data is read through the same interface the upload program writes
*& through - CMD_EI_API_EXTRACT=>GET_DATA - so a column that can be loaded
*& is a column that can be read back, in the same structures. Two things
*& the interface does not carry are read directly, as the upload program
*& writes them directly: ZSD_LICENSE_CHK for the licence, bank guarantee
*& and routing data, and BUT0ID for the Aadhaar number. Nothing is
*& changed: the program only reads.
*&
*& The workbook is written as a real .xlsx - a zip of OpenXML parts built
*& with CL_ABAP_ZIP - with every text cell pointing into the shared string
*& table and column A always written, because that is what
*& CL_FDT_XL_SPREADSHEET needs to read it back.
*&
*& Naming convention
*&   Z<MODULE>_ pattern from Cipla_Checklist Part 1.1.
*&
*& The column map below is generated from the template workbook by
*& tools/cipla/gen_download_program.py, so a change to a template is a
*& regeneration rather than an edit.
*&---------------------------------------------------------------------*
REPORT zsds_cust_tmpl_download.

TYPES: tt_cell TYPE STANDARD TABLE OF string WITH EMPTY KEY.

TYPES: BEGIN OF ty_row,
         cells TYPE tt_cell,
       END OF ty_row,
       tt_row TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

" One line per column of one template: where it sits, what it is called,
" which part of the master record holds it, and how to write it.
TYPES: BEGIN OF ty_col,
         tmpl TYPE char8,
         col  TYPE i,
         hdr  TYPE char60,
         node TYPE char1,
         fld  TYPE char30,
         fmt  TYPE char2,
       END OF ty_col,
       tt_col TYPE STANDARD TABLE OF ty_col WITH EMPTY KEY.

" Which template a country and an account group resolve to.
TYPES: BEGIN OF ty_combi,
         land  TYPE land1,
         ktokd TYPE ktokd,
         tmpl  TYPE char8,
       END OF ty_combi,
       tt_combi TYPE STANDARD TABLE OF ty_combi WITH EMPTY KEY.

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
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_crt RADIOBUTTON GROUP g1 USER-COMMAND rb DEFAULT 'X',
            p_ext RADIOBUTTON GROUP g1,
            p_blk RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_land  TYPE land1,
            p_ktokd TYPE ktokd.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS: s_bp    FOR gv_bp    NO INTERVALS,
                s_kunnr FOR gv_kunnr NO INTERVALS.
PARAMETERS:     p_max   TYPE i DEFAULT 100.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
PARAMETERS: p_file  TYPE rlgrap-filename LOWER CASE,
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
    " The template a country and an account group resolve to, empty when
    " the workbook does not cover the combination.
    CLASS-METHODS resolve
      IMPORTING iv_land   TYPE land1
                iv_ktokd  TYPE ktokd
      RETURNING VALUE(rv) TYPE char8.

    CLASS-METHODS cols
      IMPORTING iv_tmpl   TYPE char8
      RETURNING VALUE(rt) TYPE tt_col.

    " Every country the workbook covers, and every account group it covers
    " for one country - what the F4 on the selection screen offers.
    CLASS-METHODS countries RETURNING VALUE(rt) TYPE tt_land.
    CLASS-METHODS groups
      IMPORTING iv_land   TYPE land1
      RETURNING VALUE(rt) TYPE tt_ktokd.

    CLASS-METHODS combis RETURNING VALUE(rt) TYPE tt_combi.

  PRIVATE SECTION.
    CLASS-DATA mt_col   TYPE tt_col.
    CLASS-DATA mt_combi TYPE tt_combi.
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
*<<COMBI>>
    ).
  ENDMETHOD.

  METHOD combis.
    load( ).
    rt = mt_combi.
  ENDMETHOD.

  METHOD resolve.
    load( ).
    READ TABLE mt_combi INTO DATA(ls_cb)
         WITH KEY land = iv_land ktokd = iv_ktokd.
    IF sy-subrc = 0.
      rv = ls_cb-tmpl.
    ENDIF.
  ENDMETHOD.

  METHOD cols.
    load( ).
    LOOP AT mt_col INTO DATA(ls_col) WHERE tmpl = iv_tmpl.
      APPEND ls_col TO rt.
    ENDLOOP.
    SORT rt BY col.
  ENDMETHOD.

  METHOD countries.
    load( ).
    LOOP AT mt_combi INTO DATA(ls_cb).
      IF ls_cb-land = gc_any.
        CONTINUE.
      ENDIF.
      READ TABLE rt TRANSPORTING NO FIELDS WITH KEY table_line = ls_cb-land.
      IF sy-subrc <> 0.
        APPEND ls_cb-land TO rt.
      ENDIF.
    ENDLOOP.
    SORT rt.
  ENDMETHOD.

  METHOD groups.
    load( ).
    LOOP AT mt_combi INTO DATA(ls_cb) WHERE land = iv_land.
      READ TABLE rt TRANSPORTING NO FIELDS WITH KEY table_line = ls_cb-ktokd.
      IF sy-subrc <> 0.
        APPEND ls_cb-ktokd TO rt.
      ENDIF.
    ENDLOOP.
    SORT rt.
  ENDMETHOD.

*<<MAP>>

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
    APPEND VALUE ty_msg(
      icon    = COND #( WHEN iv_type = 'E' THEN icon_red_light
                        WHEN iv_type = 'W' THEN icon_yellow_light
                        ELSE                    icon_green_light )
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
                  " R_3_USER is the flag that marks a number as a mobile.
                  LOOP AT ls_c-central_data-address-communication-phone-phone INTO DATA(ls_ph).
                    DATA(lv_mob) = xsdbool( ls_ph-contact-data-r_3_user = abap_true ).
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
    CLASS-METHODS f4_land.
    CLASS-METHODS f4_ktokd.
    " One list of every template the workbook holds. Whichever of the two
    " fields the help is called from, it fills both.
    CLASS-METHODS f4_template IMPORTING iv_return TYPE clike.
    " The country as it stands on the screen this moment.
    CLASS-METHODS screen_land RETURNING VALUE(rv) TYPE land1.
    CLASS-METHODS screen_val IMPORTING iv_field  TYPE clike
                             RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS label_of IMPORTING iv_land   TYPE clike
                                     iv_ktokd  TYPE clike
                           RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS validate.
    CLASS-METHODS run.
  PRIVATE SECTION.
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
        rv = lcl_tmpl=>resolve( iv_land = p_land iv_ktokd = p_ktokd ).
    ENDCASE.
  ENDMETHOD.

  METHOD label_of.
    CASE abap_true.
      WHEN p_ext. rv = 'CUST_EXTN'.
      WHEN p_blk. rv = 'BLOCK_UNBLOCK'.
      WHEN OTHERS.
        rv = |{ iv_land }_{ iv_ktokd }|.
        IF iv_land IS INITIAL OR iv_ktokd IS INITIAL.
          rv = 'CUSTOMER'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD label.
    rv = label_of( iv_land = p_land iv_ktokd = p_ktokd ).
  ENDMETHOD.

  METHOD label_screen.
    " The template radio buttons carry USER-COMMAND, so a click on one has
    " already been through PAI and the program holds the current choice. The
    " country and the account group are plain input fields with no round trip
    " of their own, so those two are read off the screen.
    rv = label_of( iv_land = screen_land( ) iv_ktokd = screen_val( 'P_KTOKD' ) ).
  ENDMETHOD.

  METHOD propose_file.
    " Keep the folder the user chose and swap the file name, so that
    " changing the country or the account group does not quietly leave
    " the previous template's name on the file.
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

  METHOD f4_land.
    f4_template( 'LAND1' ).
  ENDMETHOD.

  METHOD f4_ktokd.
    f4_template( 'KTOKD' ).
  ENDMETHOD.

  METHOD f4_template.
    " Every template the workbook holds, as one list: the country and its
    " name, the account group and its text, and how wide the template is. A
    " country already typed narrows the list to that country; an empty one
    " shows all of them, because a user who does not yet know which
    " combinations exist is exactly the one who needs the list.
    TYPES: BEGIN OF ty_f4,
             land1 TYPE land1,
             landx TYPE landx,
             ktokd TYPE ktokd,
             txt30 TYPE text30,
             cols  TYPE char5,
           END OF ty_f4.
    DATA lt_f4 TYPE STANDARD TABLE OF ty_f4 WITH EMPTY KEY.

    DATA(lv_land) = screen_land( ).
    DATA(lt_combi) = lcl_tmpl=>combis( ).
    DELETE lt_combi WHERE land = gc_any.
    IF lv_land IS NOT INITIAL.
      DELETE lt_combi WHERE land <> lv_land.
      IF lt_combi IS INITIAL.
        MESSAGE |No customer template exists for country { lv_land }| TYPE 'S' DISPLAY LIKE 'W'.
        RETURN.
      ENDIF.
    ENDIF.
    IF lt_combi IS INITIAL.
      RETURN.
    ENDIF.

    SELECT land1, landx FROM t005t
      FOR ALL ENTRIES IN @lt_combi
      WHERE spras = @sy-langu AND land1 = @lt_combi-land
      INTO TABLE @DATA(lt_ctry).
    SELECT ktokd, txt30 FROM t077x
      FOR ALL ENTRIES IN @lt_combi
      WHERE spras = @sy-langu AND ktokd = @lt_combi-ktokd
      INTO TABLE @DATA(lt_grp).

    LOOP AT lt_combi INTO DATA(ls_cb).
      DATA ls_f4 TYPE ty_f4.
      CLEAR ls_f4.
      ls_f4-land1 = ls_cb-land.
      ls_f4-ktokd = ls_cb-ktokd.
      READ TABLE lt_ctry INTO DATA(ls_ct) WITH KEY land1 = ls_cb-land.
      IF sy-subrc = 0.
        ls_f4-landx = ls_ct-landx.
      ENDIF.
      READ TABLE lt_grp INTO DATA(ls_gr) WITH KEY ktokd = ls_cb-ktokd.
      IF sy-subrc = 0.
        ls_f4-txt30 = ls_gr-txt30.
      ENDIF.
      ls_f4-cols = lines( lcl_tmpl=>cols( ls_cb-tmpl ) ).
      SHIFT ls_f4-cols LEFT DELETING LEADING '0'.
      APPEND ls_f4 TO lt_f4.
    ENDLOOP.
    SORT lt_f4 BY land1 ktokd.

    " Picking a row fills the country as well as the account group, so the
    " two fields cannot be left disagreeing with each other.
    DATA lt_map TYPE STANDARD TABLE OF dselc WITH EMPTY KEY.
    APPEND VALUE dselc( fldname = 'LAND1' dyfldname = 'P_LAND'  ) TO lt_map.
    APPEND VALUE dselc( fldname = 'KTOKD' dyfldname = 'P_KTOKD' ) TO lt_map.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING retfield         = iv_return
                dynpprog         = sy-repid
                dynpnr           = sy-dynnr
                window_title     = 'Customer templates'
                value_org        = 'S'
      TABLES    value_tab        = lt_f4
                dynpfld_mapping  = lt_map
      EXCEPTIONS OTHERS          = 1.
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

  METHOD screen_land.
    rv = screen_val( 'P_LAND' ).
    IF rv IS INITIAL.
      rv = p_land.
    ENDIF.
  ENDMETHOD.

  METHOD validate.
    IF p_crt = abap_true.
      IF p_land IS INITIAL.
        MESSAGE 'Give a country' TYPE 'E'.
      ENDIF.
      IF p_ktokd IS INITIAL.
        MESSAGE 'Give a customer account group' TYPE 'E'.
      ENDIF.
      " A country the workbook does not cover, and a country it does cover
      " but not with this account group, are two different messages - the
      " user should not have to guess which of the two is wrong.
      IF lcl_tmpl=>groups( p_land ) IS INITIAL.
        MESSAGE |No customer template exists for country { p_land }| TYPE 'E'.
      ENDIF.
      IF lcl_tmpl=>resolve( iv_land = p_land iv_ktokd = p_ktokd ) IS INITIAL.
        MESSAGE |No customer template exists for country { p_land } | &&
                |with account group { p_ktokd }| TYPE 'E'.
      ENDIF.
    ENDIF.

    IF p_empty = abap_false AND s_bp[] IS INITIAL AND s_kunnr[] IS INITIAL.
      MESSAGE 'Give a business partner or a customer - or tick "Template only"' TYPE 'E'.
    ENDIF.
    IF p_max < 1.
      MESSAGE 'Rows at most must be 1 or more' TYPE 'E'.
    ENDIF.
    IF p_file IS INITIAL.
      MESSAGE 'Give a file name' TYPE 'E'.
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
    DATA(lv_tmpl) = chosen( ).
    IF lv_tmpl IS INITIAL.
      MESSAGE |No customer template exists for country { p_land } | &&
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
  lcl_main=>propose_file( ).

AT SELECTION-SCREEN OUTPUT.
  " The country and the account group only apply to the customer create
  " template; the extension and the block / unblock templates are the same
  " for every country.
  LOOP AT SCREEN.
    IF screen-name = 'P_LAND' OR screen-name = 'P_KTOKD'.
      IF p_crt = abap_true.
        screen-input = 1.
      ELSE.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
  " Show the file name that belongs to the template now selected. The
  " radio button group carries USER-COMMAND, so a click comes back here.
  lcl_main=>propose_file( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_land.
  lcl_main=>f4_land( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_ktokd.
  lcl_main=>f4_ktokd( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: lv_path TYPE string,
        lv_name TYPE string,
        lv_full TYPE string.
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
    p_file = lv_full.
  ENDIF.

AT SELECTION-SCREEN.
  " Runs before START-OF-SELECTION as well, so the file name is right even
  " when the user picks a template and presses F8 without a refresh.
  lcl_main=>propose_file( ).

  " A radio button click is only that refresh - the user has not asked for
  " anything yet, so there is nothing to complain about.
  CHECK sscrfields-ucomm <> 'RB'.
  lcl_main=>validate( ).

*----------------------------------------------------------------------*
* Main
*----------------------------------------------------------------------*
START-OF-SELECTION.
  lcl_main=>run( ).
