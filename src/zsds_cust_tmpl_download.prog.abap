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
*&   Deliberately TIER 2, for the same reason ZSDS_CUST_MASS_UPLOAD is.
*&   CMD_EI_API_EXTRACT is "Not released", and the released alternative
*&   (the CDS views behind I_Customer) does not carry the licence record or
*&   the identification numbers the templates need. CL_GUI_FRONTEND_SERVICES
*&   is classic GUI only, which is what a download to a user's PC means.
*&   Every read is confined to LCL_SRC, so it can be swapped for released
*&   CDS views without touching the engine.
*&   Run ATC with variant ABAP_CLOUD_READINESS and record those two
*&   exemptions. There is nothing else to record - the program changes no
*&   data at all.
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

    CLASS-METHODS combis RETURNING VALUE(rt) TYPE tt_combi.

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

  METHOD combis.
    load( ).
    rt = mt_combi.
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
      ( tmpl = '06551ad0' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = '06551ad0' col = 2   hdr = 'Ref Customer Code  as sample' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = '06551ad0' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = '06551ad0' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = '06551ad0' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = '06551ad0' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = '06551ad0' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = '06551ad0' col = 8   hdr = 'Number of contact person' node = 'P' fld = 'PARNR' fmt = '' )
      ( tmpl = '06551ad0' col = 9   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' )
      ( tmpl = '06551ad0' col = 10  hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' )
      ( tmpl = '06551ad0' col = 11  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' )
      ( tmpl = '06551ad0' col = 12  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' )
      ( tmpl = '06551ad0' col = 13  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' )
      ( tmpl = '06551ad0' col = 14  hdr = 'aLWAYS x' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = '06551ad0' col = 15  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = '06551ad0' col = 16  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = '06551ad0' col = 17  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = '06551ad0' col = 18  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = '06551ad0' col = 19  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = '06551ad0' col = 20  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = '06551ad0' col = 21  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = '06551ad0' col = 22  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = '06551ad0' col = 23  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = '06551ad0' col = 24  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = '06551ad0' col = 25  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = '06551ad0' col = 26  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' )
      ( tmpl = '06551ad0' col = 27  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = '06551ad0' col = 28  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = '06551ad0' col = 29  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = '06551ad0' col = 30  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = '06551ad0' col = 31  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = '06551ad0' col = 32  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = '06551ad0' col = 33  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = '06551ad0' col = 34  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = '06551ad0' col = 35  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = '06551ad0' col = 36  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = '06551ad0' col = 37  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = '06551ad0' col = 38  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = '06551ad0' col = 39  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( tmpl = '06551ad0' col = 40  hdr = 'Exch. Rate Type M' node = 'S' fld = 'KURST' fmt = '' )
      ( tmpl = '06551ad0' col = 41  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' )
      ( tmpl = '06551ad0' col = 42  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' )
      ( tmpl = '06551ad0' col = 43  hdr = 'Price List' node = 'S' fld = 'PLTYP' fmt = '' )
      ( tmpl = '06551ad0' col = 44  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = '06551ad0' col = 45  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = '06551ad0' col = 46  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = '06551ad0' col = 47  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = '06551ad0' col = 48  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = '06551ad0' col = 49  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = '06551ad0' col = 50  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = '06551ad0' col = 51  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = '06551ad0' col = 52  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( tmpl = '06551ad0' col = 53  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = '06551ad0' col = 54  hdr = 'JOIG IN:Central GST - OP' node = 'T' fld = 'JOCG' fmt = '' )
      ( tmpl = '06551ad0' col = 55  hdr = 'JTC1 IN: 206C(1H) Goods' node = 'T' fld = 'JTC1' fmt = '' )
      ( tmpl = '06551ad0' col = 56  hdr = 'JTX1 Tax Jurisdict.Code d' node = 'T' fld = 'JTX1' fmt = '' )
      ( tmpl = '06551ad0' col = 57  hdr = 'JTX2 Tax Jurisdict.Code d' node = 'T' fld = 'JTX2' fmt = '' )
      ( tmpl = '06551ad0' col = 58  hdr = 'JTX3 Tax Jurisdict.Code d' node = 'T' fld = 'JTX3' fmt = '' )
      ( tmpl = '06551ad0' col = 59  hdr = 'JTX4 Tax Jurisdict.Code d' node = 'T' fld = 'JTX4' fmt = '' )
      ( tmpl = '06551ad0' col = 60  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = '06551ad0' col = 61  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = '06551ad0' col = 62  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = '06551ad0' col = 63  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = '06551ad0' col = 64  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
    "  08585a5a - 83 columns - KE/YDOM, ZA/YDOM, ZA/YINT, ZA/YTDR
      ( tmpl = '08585a5a' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = '08585a5a' col = 2   hdr = 'Customer number' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = '08585a5a' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = '08585a5a' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = '08585a5a' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = '08585a5a' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = '08585a5a' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = '08585a5a' col = 8   hdr = 'Always X' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = '08585a5a' col = 9   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = '08585a5a' col = 10  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = '08585a5a' col = 11  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = '08585a5a' col = 12  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = '08585a5a' col = 13  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = '08585a5a' col = 14  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = '08585a5a' col = 15  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = '08585a5a' col = 16  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = '08585a5a' col = 17  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = '08585a5a' col = 18  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' )
      ( tmpl = '08585a5a' col = 19  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = '08585a5a' col = 20  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = '08585a5a' col = 21  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = '08585a5a' col = 22  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = '08585a5a' col = 23  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = '08585a5a' col = 24  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = '08585a5a' col = 25  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = '08585a5a' col = 26  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = '08585a5a' col = 27  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = '08585a5a' col = 28  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = '08585a5a' col = 29  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = '08585a5a' col = 30  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( tmpl = '08585a5a' col = 31  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' )
      ( tmpl = '08585a5a' col = 32  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = '' )
      ( tmpl = '08585a5a' col = 33  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' )
      ( tmpl = '08585a5a' col = 34  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' )
      ( tmpl = '08585a5a' col = 35  hdr = 'VAT Registration Number' node = 'C' fld = 'STCEG' fmt = '' )
      ( tmpl = '08585a5a' col = 36  hdr = 'Tax Number 5' node = 'C' fld = 'STCD5' fmt = '' )
      ( tmpl = '08585a5a' col = 37  hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' )
      ( tmpl = '08585a5a' col = 38  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( tmpl = '08585a5a' col = 39  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( tmpl = '08585a5a' col = 40  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' )
      ( tmpl = '08585a5a' col = 41  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( tmpl = '08585a5a' col = 42  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' )
      ( tmpl = '08585a5a' col = 43  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' )
      ( tmpl = '08585a5a' col = 44  hdr = 'Memo' node = 'B' fld = 'KVERM' fmt = '' )
      ( tmpl = '08585a5a' col = 45  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = '08585a5a' col = 46  hdr = 'Order probability of the item' node = 'S' fld = 'AWAHR' fmt = '' )
      ( tmpl = '08585a5a' col = 47  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = '08585a5a' col = 48  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = '08585a5a' col = 49  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = '08585a5a' col = 50  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = '08585a5a' col = 51  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( tmpl = '08585a5a' col = 52  hdr = 'Exchange Rate Type' node = 'S' fld = 'KURST' fmt = '' )
      ( tmpl = '08585a5a' col = 53  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' )
      ( tmpl = '08585a5a' col = 54  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' )
      ( tmpl = '08585a5a' col = 55  hdr = 'Price list type' node = 'S' fld = 'PLTYP' fmt = '' )
      ( tmpl = '08585a5a' col = 56  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = '08585a5a' col = 57  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = '08585a5a' col = 58  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = '08585a5a' col = 59  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = '08585a5a' col = 60  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = '08585a5a' col = 61  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = '08585a5a' col = 62  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = '08585a5a' col = 63  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = '08585a5a' col = 64  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( tmpl = '08585a5a' col = 65  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = '08585a5a' col = 66  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' )
      ( tmpl = '08585a5a' col = 67  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = '08585a5a' col = 68  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = '08585a5a' col = 69  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = '08585a5a' col = 70  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = '08585a5a' col = 71  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = '08585a5a' col = 72  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = '08585a5a' col = 73  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = '08585a5a' col = 74  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' )
      ( tmpl = '08585a5a' col = 75  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( tmpl = '08585a5a' col = 76  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( tmpl = '08585a5a' col = 77  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' )
      ( tmpl = '08585a5a' col = 78  hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' )
      ( tmpl = '08585a5a' col = 79  hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' )
      ( tmpl = '08585a5a' col = 80  hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' )
      ( tmpl = '08585a5a' col = 81  hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' )
      ( tmpl = '08585a5a' col = 82  hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' )
      ( tmpl = '08585a5a' col = 83  hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' )
    "  1f487b03 - 136 columns - IN/ZDOC, IN/ZDOD, IN/ZDOF
      ( tmpl = '1f487b03' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = '1f487b03' col = 2   hdr = 'New Customer Code' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = '1f487b03' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = '1f487b03' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = '1f487b03' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = '1f487b03' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = '1f487b03' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = '1f487b03' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' )
      ( tmpl = '1f487b03' col = 9   hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' )
      ( tmpl = '1f487b03' col = 10  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' )
      ( tmpl = '1f487b03' col = 11  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' )
      ( tmpl = '1f487b03' col = 12  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' )
      ( tmpl = '1f487b03' col = 13  hdr = 'ALWAYS X' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = '1f487b03' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = '1f487b03' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = '1f487b03' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = '1f487b03' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = '1f487b03' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = '1f487b03' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = '1f487b03' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = '1f487b03' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = '1f487b03' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = '1f487b03' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = '1f487b03' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = '1f487b03' col = 25  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' )
      ( tmpl = '1f487b03' col = 26  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = '1f487b03' col = 27  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = '1f487b03' col = 28  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = '1f487b03' col = 29  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = '1f487b03' col = 30  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = '1f487b03' col = 31  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = '1f487b03' col = 32  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = '1f487b03' col = 33  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = '1f487b03' col = 34  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = '1f487b03' col = 35  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = '1f487b03' col = 36  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = '1f487b03' col = 37  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = '1f487b03' col = 38  hdr = 'Attribute 1' node = 'C' fld = 'KATR1' fmt = '' )
      ( tmpl = '1f487b03' col = 39  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( tmpl = '1f487b03' col = 40  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
      ( tmpl = '1f487b03' col = 41  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' )
      ( tmpl = '1f487b03' col = 42  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = '' )
      ( tmpl = '1f487b03' col = 43  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' )
      ( tmpl = '1f487b03' col = 44  hdr = 'Tax Number 3 ( GST Number)' node = 'C' fld = 'STCD3' fmt = '' )
      ( tmpl = '1f487b03' col = 45  hdr = 'Permanent Account Number' node = 'C' fld = 'J_1IPANNO' fmt = '' )
      ( tmpl = '1f487b03' col = 46  hdr = 'GST TDS Registration' node = 'C' fld = 'GST_TDS' fmt = '' )
      ( tmpl = '1f487b03' col = 47  hdr = 'Aadhaar Number' node = 'I' fld = 'X90003' fmt = '' )
      ( tmpl = '1f487b03' col = 48  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( tmpl = '1f487b03' col = 49  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( tmpl = '1f487b03' col = 50  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = '' )
      ( tmpl = '1f487b03' col = 51  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' )
      ( tmpl = '1f487b03' col = 52  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' )
      ( tmpl = '1f487b03' col = 53  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' )
      ( tmpl = '1f487b03' col = 54  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( tmpl = '1f487b03' col = 55  hdr = 'Tolerance group for the business partner/G/L account' node = 'B' fld = 'TOGRU' fmt = '' )
      ( tmpl = '1f487b03' col = 56  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' )
      ( tmpl = '1f487b03' col = 57  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' )
      ( tmpl = '1f487b03' col = 58  hdr = 'Block Key for Payment' node = 'B' fld = 'ZAHLS' fmt = '' )
      ( tmpl = '1f487b03' col = 59  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = '1f487b03' col = 60  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = '1f487b03' col = 61  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = '1f487b03' col = 62  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = '1f487b03' col = 63  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = '1f487b03' col = 64  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( tmpl = '1f487b03' col = 65  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' )
      ( tmpl = '1f487b03' col = 66  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' )
      ( tmpl = '1f487b03' col = 67  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = '1f487b03' col = 68  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = '1f487b03' col = 69  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = '1f487b03' col = 70  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = '1f487b03' col = 71  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = '1f487b03' col = 72  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = '1f487b03' col = 73  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = '1f487b03' col = 74  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = '1f487b03' col = 75  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( tmpl = '1f487b03' col = 76  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = '1f487b03' col = 77  hdr = 'JOIG IN:Central GST - OP' node = 'T' fld = 'JOCG' fmt = '' )
      ( tmpl = '1f487b03' col = 78  hdr = 'JTC1 IN: 206C(1H) Goods' node = 'T' fld = 'JTC1' fmt = '' )
      ( tmpl = '1f487b03' col = 79  hdr = 'JTX1 Tax Jurisdict.Code d' node = 'T' fld = 'JTX1' fmt = '' )
      ( tmpl = '1f487b03' col = 80  hdr = 'JTX2 Tax Jurisdict.Code d' node = 'T' fld = 'JTX2' fmt = '' )
      ( tmpl = '1f487b03' col = 81  hdr = 'JTX3 Tax Jurisdict.Code d' node = 'T' fld = 'JTX3' fmt = '' )
      ( tmpl = '1f487b03' col = 82  hdr = 'JTX4 Tax Jurisdict.Code d' node = 'T' fld = 'JTX4' fmt = '' )
      ( tmpl = '1f487b03' col = 83  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = '1f487b03' col = 84  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = '1f487b03' col = 85  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = '1f487b03' col = 86  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = '1f487b03' col = 87  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = '1f487b03' col = 88  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = '1f487b03' col = 89  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = '1f487b03' col = 90  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' )
      ( tmpl = '1f487b03' col = 91  hdr = '20B. Lic. No' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( tmpl = '1f487b03' col = 92  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' )
      ( tmpl = '1f487b03' col = 93  hdr = '21B. Lic. No' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( tmpl = '1f487b03' col = 94  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' )
      ( tmpl = '1f487b03' col = 95  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' )
      ( tmpl = '1f487b03' col = 96  hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' )
      ( tmpl = '1f487b03' col = 97  hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = '' )
      ( tmpl = '1f487b03' col = 98  hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' )
      ( tmpl = '1f487b03' col = 99  hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = '' )
      ( tmpl = '1f487b03' col = 100 hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' )
      ( tmpl = '1f487b03' col = 101 hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = '' )
      ( tmpl = '1f487b03' col = 102 hdr = 'Retails Lic No (20 and 21 )' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' )
      ( tmpl = '1f487b03' col = 103 hdr = 'SC_EXEMPT' node = 'Z' fld = 'SC_EXEMPT' fmt = '' )
      ( tmpl = '1f487b03' col = 104 hdr = 'Retails Lic Exp date' node = 'Z' fld = 'RETAIL_EXP' fmt = '' )
      ( tmpl = '1f487b03' col = 105 hdr = 'Mfg License (Gen) Number' node = 'Z' fld = 'MFGLIC1NO' fmt = '' )
      ( tmpl = '1f487b03' col = 106 hdr = 'Mfg License (Nar) Number' node = 'Z' fld = 'MFGLIC2NO' fmt = '' )
      ( tmpl = '1f487b03' col = 107 hdr = 'Mfg License (CC) Number' node = 'Z' fld = 'MFGLIC3NO' fmt = '' )
      ( tmpl = '1f487b03' col = 108 hdr = 'Bank Guarantee(Y/N)' node = 'Z' fld = 'BGYN' fmt = '' )
      ( tmpl = '1f487b03' col = 109 hdr = 'Bank Guarantee No' node = 'Z' fld = 'BG_NO' fmt = '' )
      ( tmpl = '1f487b03' col = 110 hdr = 'BG Amount' node = 'Z' fld = 'BG_AMT' fmt = '' )
      ( tmpl = '1f487b03' col = 111 hdr = 'SD Document Currency' node = 'Z' fld = 'CURRENCY' fmt = '' )
      ( tmpl = '1f487b03' col = 112 hdr = 'BG Issue Date' node = 'Z' fld = 'BG_ISS_DT' fmt = '' )
      ( tmpl = '1f487b03' col = 113 hdr = 'BG Expiry Date' node = 'Z' fld = 'BG_EXP_DT' fmt = '' )
      ( tmpl = '1f487b03' col = 114 hdr = 'BG Issuing Bank' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' )
      ( tmpl = '1f487b03' col = 115 hdr = 'Agreement Expiry Date' node = 'Z' fld = 'AGGR_EXPDT' fmt = '' )
      ( tmpl = '1f487b03' col = 116 hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' )
      ( tmpl = '1f487b03' col = 117 hdr = 'Customer group' node = 'Z' fld = 'KDGRP' fmt = '' )
      ( tmpl = '1f487b03' col = 118 hdr = 'AIOCD Code' node = 'Z' fld = 'AIOCD_CODE' fmt = '' )
      ( tmpl = '1f487b03' col = 119 hdr = 'Customer Bank Name' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' )
      ( tmpl = '1f487b03' col = 120 hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' )
      ( tmpl = '1f487b03' col = 121 hdr = 'Route Code' node = 'Z' fld = 'ZTROUT' fmt = '' )
      ( tmpl = '1f487b03' col = 122 hdr = 'Extension' node = 'Z' fld = 'EXTENSION' fmt = '' )
      ( tmpl = '1f487b03' col = 123 hdr = 'Route' node = 'Z' fld = 'ZCROUT' fmt = '' )
      ( tmpl = '1f487b03' col = 124 hdr = 'GLN URI Format' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' )
      ( tmpl = '1f487b03' col = 125 hdr = 'DUNS_Number' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' )
      ( tmpl = '1f487b03' col = 126 hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' )
      ( tmpl = '1f487b03' col = 127 hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' )
      ( tmpl = '1f487b03' col = 128 hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' )
      ( tmpl = '1f487b03' col = 129 hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' )
      ( tmpl = '1f487b03' col = 130 hdr = 'Import_License/MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' )
      ( tmpl = '1f487b03' col = 131 hdr = 'IMPL/MIA_From_Date' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = '' )
      ( tmpl = '1f487b03' col = 132 hdr = 'IMPL/MIA_Valid_Date' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = '' )
      ( tmpl = '1f487b03' col = 133 hdr = 'Check Digit' node = 'Z' fld = 'CHECK_DIGIT' fmt = '' )
      ( tmpl = '1f487b03' col = 134 hdr = 'Global Company Prefix' node = 'Z' fld = 'GLOBAL_COM' fmt = '' )
      ( tmpl = '1f487b03' col = 135 hdr = 'Backorder Days' node = 'Z' fld = 'BO_DAYS' fmt = '' )
      ( tmpl = '1f487b03' col = 136 hdr = 'Location Number' node = 'Z' fld = 'LOCATION_NUMBER' fmt = '' )
    "  28573fa5 - 43 columns - */ZDOM
      ( tmpl = 'EXTN' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = 'EXTN' col = 2   hdr = 'Customer Account Number' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = 'EXTN' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = 'EXTN' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = 'EXTN' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = 'EXTN' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = 'EXTN' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = 'EXTN' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' )
      ( tmpl = 'EXTN' col = 9   hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' )
      ( tmpl = 'EXTN' col = 10  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' )
      ( tmpl = 'EXTN' col = 11  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' )
      ( tmpl = 'EXTN' col = 12  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' )
      ( tmpl = 'EXTN' col = 13  hdr = 'Always X' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = 'EXTN' col = 14  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = 'EXTN' col = 15  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = 'EXTN' col = 16  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = 'EXTN' col = 17  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = 'EXTN' col = 18  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = 'EXTN' col = 19  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( tmpl = 'EXTN' col = 20  hdr = 'Pricing procedure assigned to this custome' node = 'S' fld = 'KALKS' fmt = '' )
      ( tmpl = 'EXTN' col = 21  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = 'EXTN' col = 22  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = 'EXTN' col = 23  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = 'EXTN' col = 24  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = 'EXTN' col = 25  hdr = 'Maximum Number of Partial Deliveries Allow' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = 'EXTN' col = 26  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( tmpl = 'EXTN' col = 27  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = 'EXTN' col = 28  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' )
      ( tmpl = 'EXTN' col = 29  hdr = 'Tax classification for customer' node = 'T' fld = '#2' fmt = '' )
      ( tmpl = 'EXTN' col = 30  hdr = 'Tax classification for customer' node = 'T' fld = '#3' fmt = '' )
      ( tmpl = 'EXTN' col = 31  hdr = 'Tax classification for customer' node = 'T' fld = '#4' fmt = '' )
      ( tmpl = 'EXTN' col = 32  hdr = 'Tax classification for customer' node = 'T' fld = '#5' fmt = '' )
      ( tmpl = 'EXTN' col = 33  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = 'EXTN' col = 34  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = 'EXTN' col = 35  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = 'EXTN' col = 36  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = 'EXTN' col = 37  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = 'EXTN' col = 38  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = 'EXTN' col = 39  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = 'EXTN' col = 40  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' )
      ( tmpl = 'EXTN' col = 41  hdr = '20B. Lic. No' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( tmpl = 'EXTN' col = 42  hdr = '21B. Lic. No' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( tmpl = 'EXTN' col = 43  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' )
    "  32831d38 - 88 columns - IN/ZPY1
      ( tmpl = '32831d38' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = '32831d38' col = 2   hdr = 'Ref Customer Code  as sample' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = '32831d38' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = '32831d38' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
    ).
  ENDMETHOD.

  METHOD map_2.
    rt = VALUE tt_col(
      ( tmpl = '32831d38' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = '32831d38' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = '32831d38' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = '32831d38' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' )
      ( tmpl = '32831d38' col = 9   hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' )
      ( tmpl = '32831d38' col = 10  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' )
      ( tmpl = '32831d38' col = 11  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' )
      ( tmpl = '32831d38' col = 12  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' )
      ( tmpl = '32831d38' col = 13  hdr = 'aLWAYS x' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = '32831d38' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = '32831d38' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = '32831d38' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = '32831d38' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = '32831d38' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = '32831d38' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = '32831d38' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = '32831d38' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = '32831d38' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = '32831d38' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = '32831d38' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = '32831d38' col = 25  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' )
      ( tmpl = '32831d38' col = 26  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = '32831d38' col = 27  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = '32831d38' col = 28  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = '32831d38' col = 29  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = '32831d38' col = 30  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = '32831d38' col = 31  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = '32831d38' col = 32  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = '32831d38' col = 33  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = '32831d38' col = 34  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = '32831d38' col = 35  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = '32831d38' col = 36  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = '32831d38' col = 37  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = '32831d38' col = 38  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' )
      ( tmpl = '32831d38' col = 39  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = '' )
      ( tmpl = '32831d38' col = 40  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' )
      ( tmpl = '32831d38' col = 41  hdr = 'Tax Number 3 ( GST Number)' node = 'C' fld = 'STCD3' fmt = '' )
      ( tmpl = '32831d38' col = 42  hdr = 'Permanent Account Number' node = 'C' fld = 'J_1IPANNO' fmt = '' )
      ( tmpl = '32831d38' col = 43  hdr = 'GST TDS Registration' node = 'C' fld = 'GST_TDS' fmt = '' )
      ( tmpl = '32831d38' col = 44  hdr = 'Aadhaar Number' node = 'I' fld = 'X90003' fmt = '' )
      ( tmpl = '32831d38' col = 45  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( tmpl = '32831d38' col = 46  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( tmpl = '32831d38' col = 47  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = '' )
      ( tmpl = '32831d38' col = 48  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' )
      ( tmpl = '32831d38' col = 49  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' )
      ( tmpl = '32831d38' col = 50  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' )
      ( tmpl = '32831d38' col = 51  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( tmpl = '32831d38' col = 52  hdr = 'Tolerance group for the business partner/G/L account' node = 'B' fld = 'TOGRU' fmt = '' )
      ( tmpl = '32831d38' col = 53  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' )
      ( tmpl = '32831d38' col = 54  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' )
      ( tmpl = '32831d38' col = 55  hdr = 'Block Key for Payment' node = 'B' fld = 'ZAHLS' fmt = '' )
      ( tmpl = '32831d38' col = 56  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = '32831d38' col = 57  hdr = 'Order probab.' node = 'S' fld = 'AWAHR' fmt = '' )
      ( tmpl = '32831d38' col = 58  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = '32831d38' col = 59  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = '32831d38' col = 60  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = '32831d38' col = 61  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = '32831d38' col = 62  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( tmpl = '32831d38' col = 63  hdr = 'Exch. Rate Type M' node = 'S' fld = 'KURST' fmt = '' )
      ( tmpl = '32831d38' col = 64  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' )
      ( tmpl = '32831d38' col = 65  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' )
      ( tmpl = '32831d38' col = 66  hdr = 'Price List' node = 'S' fld = 'PLTYP' fmt = '' )
      ( tmpl = '32831d38' col = 67  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = '32831d38' col = 68  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = '32831d38' col = 69  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = '32831d38' col = 70  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = '32831d38' col = 71  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = '32831d38' col = 72  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = '32831d38' col = 73  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = '32831d38' col = 74  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = '32831d38' col = 75  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( tmpl = '32831d38' col = 76  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = '32831d38' col = 77  hdr = 'JOIG IN:Central GST - OP' node = 'T' fld = 'JOCG' fmt = '' )
      ( tmpl = '32831d38' col = 78  hdr = 'JTC1 IN: 206C(1H) Goods' node = 'T' fld = 'JTC1' fmt = '' )
      ( tmpl = '32831d38' col = 79  hdr = 'JTX1 Tax Jurisdict.Code d' node = 'T' fld = 'JTX1' fmt = '' )
      ( tmpl = '32831d38' col = 80  hdr = 'JTX2 Tax Jurisdict.Code d' node = 'T' fld = 'JTX2' fmt = '' )
      ( tmpl = '32831d38' col = 81  hdr = 'JTX3 Tax Jurisdict.Code d' node = 'T' fld = 'JTX3' fmt = '' )
      ( tmpl = '32831d38' col = 82  hdr = 'JTX4 Tax Jurisdict.Code d' node = 'T' fld = 'JTX4' fmt = '' )
      ( tmpl = '32831d38' col = 83  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = '32831d38' col = 84  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = '32831d38' col = 85  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = '32831d38' col = 86  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = '32831d38' col = 87  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = '32831d38' col = 88  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
    "  42895e0e - 139 columns - AU/ZPLN, BE/ZPLN, ES/ZPLN, GB/ZPLN, IN/ZMPC, IN/ZOTC, IN/ZPLN, MA/ZPLN, NL/ZPLN, US/ZPLN, ZA/ZPLN
      ( tmpl = '42895e0e' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = '42895e0e' col = 2   hdr = 'Ref Customer Code  as sample' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = '42895e0e' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = '42895e0e' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = '42895e0e' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = '42895e0e' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = '42895e0e' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = '42895e0e' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' )
      ( tmpl = '42895e0e' col = 9   hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' )
      ( tmpl = '42895e0e' col = 10  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' )
      ( tmpl = '42895e0e' col = 11  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' )
      ( tmpl = '42895e0e' col = 12  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' )
      ( tmpl = '42895e0e' col = 13  hdr = 'aLWAYS x' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = '42895e0e' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = '42895e0e' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = '42895e0e' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = '42895e0e' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = '42895e0e' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = '42895e0e' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = '42895e0e' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = '42895e0e' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = '42895e0e' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = '42895e0e' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = '42895e0e' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = '42895e0e' col = 25  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' )
      ( tmpl = '42895e0e' col = 26  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = '42895e0e' col = 27  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = '42895e0e' col = 28  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = '42895e0e' col = 29  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = '42895e0e' col = 30  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = '42895e0e' col = 31  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = '42895e0e' col = 32  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = '42895e0e' col = 33  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = '42895e0e' col = 34  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = '42895e0e' col = 35  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = '42895e0e' col = 36  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = '42895e0e' col = 37  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = '42895e0e' col = 38  hdr = 'Attribute 1' node = 'C' fld = 'KATR1' fmt = '' )
      ( tmpl = '42895e0e' col = 39  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( tmpl = '42895e0e' col = 40  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
      ( tmpl = '42895e0e' col = 41  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' )
      ( tmpl = '42895e0e' col = 42  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = '' )
      ( tmpl = '42895e0e' col = 43  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' )
      ( tmpl = '42895e0e' col = 44  hdr = 'Tax Number 3 ( GST Number)' node = 'C' fld = 'STCD3' fmt = '' )
      ( tmpl = '42895e0e' col = 45  hdr = 'Permanent Account Number' node = 'C' fld = 'J_1IPANNO' fmt = '' )
      ( tmpl = '42895e0e' col = 46  hdr = 'GST TDS Registration' node = 'C' fld = 'GST_TDS' fmt = '' )
      ( tmpl = '42895e0e' col = 47  hdr = 'Aadhaar Number' node = 'I' fld = 'X90003' fmt = '' )
      ( tmpl = '42895e0e' col = 48  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( tmpl = '42895e0e' col = 49  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( tmpl = '42895e0e' col = 50  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = '' )
      ( tmpl = '42895e0e' col = 51  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' )
      ( tmpl = '42895e0e' col = 52  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' )
      ( tmpl = '42895e0e' col = 53  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' )
      ( tmpl = '42895e0e' col = 54  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( tmpl = '42895e0e' col = 55  hdr = 'Tolerance group for the business partner/G/L account' node = 'B' fld = 'TOGRU' fmt = '' )
      ( tmpl = '42895e0e' col = 56  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' )
      ( tmpl = '42895e0e' col = 57  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' )
      ( tmpl = '42895e0e' col = 58  hdr = 'Block Key for Payment' node = 'B' fld = 'ZAHLS' fmt = '' )
      ( tmpl = '42895e0e' col = 59  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = '42895e0e' col = 60  hdr = 'Order probab.' node = 'S' fld = 'AWAHR' fmt = '' )
      ( tmpl = '42895e0e' col = 61  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = '42895e0e' col = 62  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = '42895e0e' col = 63  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = '42895e0e' col = 64  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = '42895e0e' col = 65  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( tmpl = '42895e0e' col = 66  hdr = 'Exch. Rate Type M' node = 'S' fld = 'KURST' fmt = '' )
      ( tmpl = '42895e0e' col = 67  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' )
      ( tmpl = '42895e0e' col = 68  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' )
      ( tmpl = '42895e0e' col = 69  hdr = 'Price List' node = 'S' fld = 'PLTYP' fmt = '' )
      ( tmpl = '42895e0e' col = 70  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = '42895e0e' col = 71  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = '42895e0e' col = 72  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = '42895e0e' col = 73  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = '42895e0e' col = 74  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = '42895e0e' col = 75  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = '42895e0e' col = 76  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = '42895e0e' col = 77  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = '42895e0e' col = 78  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( tmpl = '42895e0e' col = 79  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = '42895e0e' col = 80  hdr = 'JOIG IN:Central GST - OP' node = 'T' fld = 'JOCG' fmt = '' )
      ( tmpl = '42895e0e' col = 81  hdr = 'JTC1 IN: 206C(1H) Goods' node = 'T' fld = 'JTC1' fmt = '' )
      ( tmpl = '42895e0e' col = 82  hdr = 'JTX1 Tax Jurisdict.Code d' node = 'T' fld = 'JTX1' fmt = '' )
      ( tmpl = '42895e0e' col = 83  hdr = 'JTX2 Tax Jurisdict.Code d' node = 'T' fld = 'JTX2' fmt = '' )
      ( tmpl = '42895e0e' col = 84  hdr = 'JTX3 Tax Jurisdict.Code d' node = 'T' fld = 'JTX3' fmt = '' )
      ( tmpl = '42895e0e' col = 85  hdr = 'JTX4 Tax Jurisdict.Code d' node = 'T' fld = 'JTX4' fmt = '' )
      ( tmpl = '42895e0e' col = 86  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = '42895e0e' col = 87  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = '42895e0e' col = 88  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = '42895e0e' col = 89  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = '42895e0e' col = 90  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = '42895e0e' col = 91  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = '42895e0e' col = 92  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = '42895e0e' col = 93  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' )
      ( tmpl = '42895e0e' col = 94  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( tmpl = '42895e0e' col = 95  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' )
      ( tmpl = '42895e0e' col = 96  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( tmpl = '42895e0e' col = 97  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' )
      ( tmpl = '42895e0e' col = 98  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' )
      ( tmpl = '42895e0e' col = 99  hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' )
      ( tmpl = '42895e0e' col = 100 hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = '' )
      ( tmpl = '42895e0e' col = 101 hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' )
      ( tmpl = '42895e0e' col = 102 hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = '' )
      ( tmpl = '42895e0e' col = 103 hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' )
      ( tmpl = '42895e0e' col = 104 hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = '' )
      ( tmpl = '42895e0e' col = 105 hdr = 'Retails Lic No (20 and 21 )' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' )
      ( tmpl = '42895e0e' col = 106 hdr = 'SC_EXEMPT' node = 'Z' fld = 'SC_EXEMPT' fmt = '' )
      ( tmpl = '42895e0e' col = 107 hdr = 'Retails Lic Exp date' node = 'Z' fld = 'RETAIL_EXP' fmt = '' )
      ( tmpl = '42895e0e' col = 108 hdr = 'Mfg License (Gen) Number' node = 'Z' fld = 'MFGLIC1NO' fmt = '' )
      ( tmpl = '42895e0e' col = 109 hdr = 'Mfg License (Nar) Number' node = 'Z' fld = 'MFGLIC2NO' fmt = '' )
      ( tmpl = '42895e0e' col = 110 hdr = 'Mfg License (CC) Number' node = 'Z' fld = 'MFGLIC3NO' fmt = '' )
      ( tmpl = '42895e0e' col = 111 hdr = 'Bank Guarantee(Y/N)' node = 'Z' fld = 'BGYN' fmt = '' )
      ( tmpl = '42895e0e' col = 112 hdr = 'Bank Guarantee No' node = 'Z' fld = 'BG_NO' fmt = '' )
      ( tmpl = '42895e0e' col = 113 hdr = 'BG Amount' node = 'Z' fld = 'BG_AMT' fmt = '' )
      ( tmpl = '42895e0e' col = 114 hdr = 'SD Document Currency' node = 'Z' fld = 'CURRENCY' fmt = '' )
      ( tmpl = '42895e0e' col = 115 hdr = 'BG Issue Date' node = 'Z' fld = 'BG_ISS_DT' fmt = '' )
      ( tmpl = '42895e0e' col = 116 hdr = 'BG Expiry Date' node = 'Z' fld = 'BG_EXP_DT' fmt = '' )
      ( tmpl = '42895e0e' col = 117 hdr = 'BG Issuing Bank' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' )
      ( tmpl = '42895e0e' col = 118 hdr = 'Agreement Expiry Date' node = 'Z' fld = 'AGGR_EXPDT' fmt = '' )
      ( tmpl = '42895e0e' col = 119 hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' )
      ( tmpl = '42895e0e' col = 120 hdr = 'Customer group' node = 'Z' fld = 'KDGRP' fmt = '' )
      ( tmpl = '42895e0e' col = 121 hdr = 'AIOCD Code' node = 'Z' fld = 'AIOCD_CODE' fmt = '' )
      ( tmpl = '42895e0e' col = 122 hdr = 'Customer Bank Name' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' )
      ( tmpl = '42895e0e' col = 123 hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' )
      ( tmpl = '42895e0e' col = 124 hdr = 'Route Code' node = 'Z' fld = 'ZTROUT' fmt = '' )
      ( tmpl = '42895e0e' col = 125 hdr = 'Extension' node = 'Z' fld = 'EXTENSION' fmt = '' )
      ( tmpl = '42895e0e' col = 126 hdr = 'Route' node = 'Z' fld = 'ZCROUT' fmt = '' )
      ( tmpl = '42895e0e' col = 127 hdr = 'GLN URI Format' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' )
      ( tmpl = '42895e0e' col = 128 hdr = 'DUNS_Number' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' )
      ( tmpl = '42895e0e' col = 129 hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' )
      ( tmpl = '42895e0e' col = 130 hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' )
      ( tmpl = '42895e0e' col = 131 hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' )
      ( tmpl = '42895e0e' col = 132 hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' )
      ( tmpl = '42895e0e' col = 133 hdr = 'Import_License/MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' )
      ( tmpl = '42895e0e' col = 134 hdr = 'IMPL/MIA_From_Date' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = '' )
      ( tmpl = '42895e0e' col = 135 hdr = 'IMPL/MIA_Valid_Date' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = '' )
      ( tmpl = '42895e0e' col = 136 hdr = 'Check Digit' node = 'Z' fld = 'CHECK_DIGIT' fmt = '' )
      ( tmpl = '42895e0e' col = 137 hdr = 'Global Company Prefix' node = 'Z' fld = 'GLOBAL_COM' fmt = '' )
      ( tmpl = '42895e0e' col = 138 hdr = 'Backorder Days' node = 'Z' fld = 'BO_DAYS' fmt = '' )
      ( tmpl = '42895e0e' col = 139 hdr = 'Location Number' node = 'Z' fld = 'LOCATION_NUMBER' fmt = '' )
    "  43156406 - 63 columns - BE/YSHP, ES/YSHP, GB/YSHP, NL/YSHP, UG/YSHP, US/YSHP
      ( tmpl = '43156406' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = '43156406' col = 2   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = '43156406' col = 3   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = '43156406' col = 4   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = '43156406' col = 5   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = '43156406' col = 6   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = '43156406' col = 7   hdr = 'Always X' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = '43156406' col = 8   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = '43156406' col = 9   hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = '43156406' col = 10  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = '43156406' col = 11  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = '43156406' col = 12  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = '43156406' col = 13  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = '43156406' col = 14  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = '43156406' col = 15  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = '43156406' col = 16  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = '43156406' col = 17  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = '43156406' col = 18  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = '43156406' col = 19  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = '43156406' col = 20  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = '43156406' col = 21  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = '43156406' col = 22  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = '43156406' col = 23  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = '43156406' col = 24  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = '43156406' col = 25  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = '43156406' col = 26  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = '43156406' col = 27  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = '43156406' col = 28  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = '43156406' col = 29  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = '43156406' col = 30  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = '43156406' col = 31  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' )
      ( tmpl = '43156406' col = 32  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( tmpl = '43156406' col = 33  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
      ( tmpl = '43156406' col = 34  hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' )
      ( tmpl = '43156406' col = 35  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = '43156406' col = 36  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = '43156406' col = 37  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = '43156406' col = 38  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = '43156406' col = 39  hdr = 'ABC class' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = '43156406' col = 40  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = '43156406' col = 41  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = '43156406' col = 42  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = '43156406' col = 43  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = '43156406' col = 44  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = '43156406' col = 45  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = '43156406' col = 46  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = '43156406' col = 47  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = '43156406' col = 48  hdr = 'Customer Account Assignment Group' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = '43156406' col = 49  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' )
      ( tmpl = '43156406' col = 50  hdr = 'Tax classification for customer' node = 'T' fld = '#2' fmt = '' )
      ( tmpl = '43156406' col = 51  hdr = 'Tax classification for customer' node = 'T' fld = '#3' fmt = '' )
      ( tmpl = '43156406' col = 52  hdr = 'Tax classification for customer' node = 'T' fld = '#4' fmt = '' )
      ( tmpl = '43156406' col = 53  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = '43156406' col = 54  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = '43156406' col = 55  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = '43156406' col = 56  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = '43156406' col = 57  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = '43156406' col = 58  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = '43156406' col = 59  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = '43156406' col = 60  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' )
      ( tmpl = '43156406' col = 61  hdr = '20B. Lic. No' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( tmpl = '43156406' col = 62  hdr = '21B. Lic. No' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( tmpl = '43156406' col = 63  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' )
    "  647cd8ca - 24 columns - IN/ZBMR
      ( tmpl = '647cd8ca' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = '647cd8ca' col = 2   hdr = 'Customer Code' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = '647cd8ca' col = 3   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = '647cd8ca' col = 4   hdr = 'aLWAYS x' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = '647cd8ca' col = 5   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = '647cd8ca' col = 6   hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = '647cd8ca' col = 7   hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = '647cd8ca' col = 8   hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = '647cd8ca' col = 9   hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = '647cd8ca' col = 10  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = '647cd8ca' col = 11  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = '647cd8ca' col = 12  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = '647cd8ca' col = 13  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = '647cd8ca' col = 14  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = '647cd8ca' col = 15  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = '647cd8ca' col = 16  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' )
      ( tmpl = '647cd8ca' col = 17  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = '647cd8ca' col = 18  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = '647cd8ca' col = 19  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = '647cd8ca' col = 20  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = '647cd8ca' col = 21  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = '647cd8ca' col = 22  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = '647cd8ca' col = 23  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = '647cd8ca' col = 24  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
    "  67849f38 - 108 columns - IN/ZSHM
      ( tmpl = '67849f38' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = '67849f38' col = 2   hdr = 'Customer Account Number' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = '67849f38' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = '67849f38' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = '67849f38' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = '67849f38' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = '67849f38' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = '67849f38' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' )
      ( tmpl = '67849f38' col = 9   hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' )
      ( tmpl = '67849f38' col = 10  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' )
      ( tmpl = '67849f38' col = 11  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' )
      ( tmpl = '67849f38' col = 12  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' )
      ( tmpl = '67849f38' col = 13  hdr = 'Always X' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = '67849f38' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = '67849f38' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = '67849f38' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = '67849f38' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = '67849f38' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = '67849f38' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = '67849f38' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = '67849f38' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
    ).
  ENDMETHOD.

  METHOD map_3.
    rt = VALUE tt_col(
      ( tmpl = '67849f38' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = '67849f38' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = '67849f38' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = '67849f38' col = 25  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = '67849f38' col = 26  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = '67849f38' col = 27  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = '67849f38' col = 28  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = '67849f38' col = 29  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = '67849f38' col = 30  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = '67849f38' col = 31  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = '67849f38' col = 32  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = '67849f38' col = 33  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = '67849f38' col = 34  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = '67849f38' col = 35  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = '67849f38' col = 36  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = '67849f38' col = 37  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( tmpl = '67849f38' col = 38  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' )
      ( tmpl = '67849f38' col = 39  hdr = 'Permanent Account Number' node = 'C' fld = 'J_1IPANNO' fmt = '' )
      ( tmpl = '67849f38' col = 40  hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' )
      ( tmpl = '67849f38' col = 41  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = '67849f38' col = 42  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = '67849f38' col = 43  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = '67849f38' col = 44  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = '67849f38' col = 45  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = '67849f38' col = 46  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = '67849f38' col = 47  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = '67849f38' col = 48  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = '67849f38' col = 49  hdr = 'Tax classification for customer' node = 'T' fld = 'JOCG' fmt = '' )
      ( tmpl = '67849f38' col = 50  hdr = 'Tax classification for customer' node = 'T' fld = 'JTC1' fmt = '' )
      ( tmpl = '67849f38' col = 51  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX1' fmt = '' )
      ( tmpl = '67849f38' col = 52  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX2' fmt = '' )
      ( tmpl = '67849f38' col = 53  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX3' fmt = '' )
      ( tmpl = '67849f38' col = 54  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX4' fmt = '' )
      ( tmpl = '67849f38' col = 55  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = '67849f38' col = 56  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = '67849f38' col = 57  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = '67849f38' col = 58  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = '67849f38' col = 59  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = '67849f38' col = 60  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = '67849f38' col = 61  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = '67849f38' col = 62  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' )
      ( tmpl = '67849f38' col = 63  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( tmpl = '67849f38' col = 64  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' )
      ( tmpl = '67849f38' col = 65  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( tmpl = '67849f38' col = 66  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' )
      ( tmpl = '67849f38' col = 67  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' )
      ( tmpl = '67849f38' col = 68  hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' )
      ( tmpl = '67849f38' col = 69  hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = '' )
      ( tmpl = '67849f38' col = 70  hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' )
      ( tmpl = '67849f38' col = 71  hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = '' )
      ( tmpl = '67849f38' col = 72  hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' )
      ( tmpl = '67849f38' col = 73  hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = '' )
      ( tmpl = '67849f38' col = 74  hdr = 'Retails Lic No (20 and 21 )' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' )
      ( tmpl = '67849f38' col = 75  hdr = 'SC_EXEMPT' node = 'Z' fld = 'SC_EXEMPT' fmt = '' )
      ( tmpl = '67849f38' col = 76  hdr = 'Retails Lic Exp date' node = 'Z' fld = 'RETAIL_EXP' fmt = '' )
      ( tmpl = '67849f38' col = 77  hdr = 'Mfg License (Gen) Number' node = 'Z' fld = 'MFGLIC1NO' fmt = '' )
      ( tmpl = '67849f38' col = 78  hdr = 'Mfg License (Nar) Number' node = 'Z' fld = 'MFGLIC2NO' fmt = '' )
      ( tmpl = '67849f38' col = 79  hdr = 'Mfg License (CC) Number' node = 'Z' fld = 'MFGLIC3NO' fmt = '' )
      ( tmpl = '67849f38' col = 80  hdr = 'Bank Guarantee(Y/N)' node = 'Z' fld = 'BGYN' fmt = '' )
      ( tmpl = '67849f38' col = 81  hdr = 'Bank Guarantee No' node = 'Z' fld = 'BG_NO' fmt = '' )
      ( tmpl = '67849f38' col = 82  hdr = 'BG Amount' node = 'Z' fld = 'BG_AMT' fmt = '' )
      ( tmpl = '67849f38' col = 83  hdr = 'SD Document Currency' node = 'Z' fld = 'CURRENCY' fmt = '' )
      ( tmpl = '67849f38' col = 84  hdr = 'BG Issue Date' node = 'Z' fld = 'BG_ISS_DT' fmt = '' )
      ( tmpl = '67849f38' col = 85  hdr = 'BG Expiry Date' node = 'Z' fld = 'BG_EXP_DT' fmt = '' )
      ( tmpl = '67849f38' col = 86  hdr = 'BG Issuing Bank' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' )
      ( tmpl = '67849f38' col = 87  hdr = 'Agreement Expiry Date' node = 'Z' fld = 'AGGR_EXPDT' fmt = '' )
      ( tmpl = '67849f38' col = 88  hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' )
      ( tmpl = '67849f38' col = 89  hdr = 'Customer group' node = 'Z' fld = 'KDGRP' fmt = '' )
      ( tmpl = '67849f38' col = 90  hdr = 'AIOCD Code' node = 'Z' fld = 'AIOCD_CODE' fmt = '' )
      ( tmpl = '67849f38' col = 91  hdr = 'Customer Bank Name' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' )
      ( tmpl = '67849f38' col = 92  hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' )
      ( tmpl = '67849f38' col = 93  hdr = 'Route Code' node = 'Z' fld = 'ZTROUT' fmt = '' )
      ( tmpl = '67849f38' col = 94  hdr = 'Extension' node = 'Z' fld = 'EXTENSION' fmt = '' )
      ( tmpl = '67849f38' col = 95  hdr = 'Route' node = 'Z' fld = 'ZCROUT' fmt = '' )
      ( tmpl = '67849f38' col = 96  hdr = 'GLN URI Format' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' )
      ( tmpl = '67849f38' col = 97  hdr = 'DUNS_Number' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' )
      ( tmpl = '67849f38' col = 98  hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' )
      ( tmpl = '67849f38' col = 99  hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' )
      ( tmpl = '67849f38' col = 100 hdr = 'Import_License/MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' )
      ( tmpl = '67849f38' col = 101 hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' )
      ( tmpl = '67849f38' col = 102 hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' )
      ( tmpl = '67849f38' col = 103 hdr = 'IMPL/MIA_From_Date' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = '' )
      ( tmpl = '67849f38' col = 104 hdr = 'IMPL/MIA_Valid_Date' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = '' )
      ( tmpl = '67849f38' col = 105 hdr = 'Check Digit' node = 'Z' fld = 'CHECK_DIGIT' fmt = '' )
      ( tmpl = '67849f38' col = 106 hdr = 'Global Company Prefix' node = 'Z' fld = 'GLOBAL_COM' fmt = '' )
      ( tmpl = '67849f38' col = 107 hdr = 'Backorder Days' node = 'Z' fld = 'BO_DAYS' fmt = '' )
      ( tmpl = '67849f38' col = 108 hdr = 'Location Number' node = 'Z' fld = 'LOCATION_NUMBER' fmt = '' )
    "  6c94ea65 - 77 columns - IN/ZSHP
      ( tmpl = '6c94ea65' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = '6c94ea65' col = 2   hdr = 'Customer code' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = '6c94ea65' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = '6c94ea65' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = '6c94ea65' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = '6c94ea65' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = '6c94ea65' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = '6c94ea65' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' )
      ( tmpl = '6c94ea65' col = 9   hdr = 'Ref Company Code' node = '-' fld = '' fmt = '' )
      ( tmpl = '6c94ea65' col = 10  hdr = 'Ref Sales Organization' node = '-' fld = '' fmt = '' )
      ( tmpl = '6c94ea65' col = 11  hdr = 'Ref Distribution Channel' node = '-' fld = '' fmt = '' )
      ( tmpl = '6c94ea65' col = 12  hdr = 'Ref Division' node = '-' fld = '' fmt = '' )
      ( tmpl = '6c94ea65' col = 13  hdr = 'Always X' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = '6c94ea65' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = '6c94ea65' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = '6c94ea65' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = '6c94ea65' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = '6c94ea65' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = '6c94ea65' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = '6c94ea65' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = '6c94ea65' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = '6c94ea65' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = '6c94ea65' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = '6c94ea65' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = '6c94ea65' col = 25  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = '6c94ea65' col = 26  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = '6c94ea65' col = 27  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = '6c94ea65' col = 28  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = '6c94ea65' col = 29  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = '6c94ea65' col = 30  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = '6c94ea65' col = 31  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = '6c94ea65' col = 32  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = '6c94ea65' col = 33  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = '6c94ea65' col = 34  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = '6c94ea65' col = 35  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = '6c94ea65' col = 36  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = '6c94ea65' col = 37  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( tmpl = '6c94ea65' col = 38  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
      ( tmpl = '6c94ea65' col = 39  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' )
      ( tmpl = '6c94ea65' col = 40  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = '' )
      ( tmpl = '6c94ea65' col = 41  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' )
      ( tmpl = '6c94ea65' col = 42  hdr = 'Tax Number 2' node = 'C' fld = 'STCD2' fmt = '' )
      ( tmpl = '6c94ea65' col = 43  hdr = 'Tax Number 1' node = 'C' fld = 'STCD1' fmt = '' )
      ( tmpl = '6c94ea65' col = 44  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' )
      ( tmpl = '6c94ea65' col = 45  hdr = 'VAT Registration Number' node = 'C' fld = 'STCEG' fmt = '' )
      ( tmpl = '6c94ea65' col = 46  hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' )
      ( tmpl = '6c94ea65' col = 47  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = '6c94ea65' col = 48  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = '6c94ea65' col = 49  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = '6c94ea65' col = 50  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = '6c94ea65' col = 51  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = '6c94ea65' col = 52  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = '6c94ea65' col = 53  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = '6c94ea65' col = 54  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = '6c94ea65' col = 55  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = '6c94ea65' col = 56  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = '6c94ea65' col = 57  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = '6c94ea65' col = 58  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = '6c94ea65' col = 59  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = '6c94ea65' col = 60  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = '6c94ea65' col = 61  hdr = 'Tax classification for customer' node = 'T' fld = 'JOCG' fmt = '' )
      ( tmpl = '6c94ea65' col = 62  hdr = 'Tax classification for customer' node = 'T' fld = 'JTC1' fmt = '' )
      ( tmpl = '6c94ea65' col = 63  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX1' fmt = '' )
      ( tmpl = '6c94ea65' col = 64  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX2' fmt = '' )
      ( tmpl = '6c94ea65' col = 65  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX3' fmt = '' )
      ( tmpl = '6c94ea65' col = 66  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = '6c94ea65' col = 67  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = '6c94ea65' col = 68  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = '6c94ea65' col = 69  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = '6c94ea65' col = 70  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = '6c94ea65' col = 71  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = '6c94ea65' col = 72  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = '6c94ea65' col = 73  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( tmpl = '6c94ea65' col = 74  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' )
      ( tmpl = '6c94ea65' col = 75  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( tmpl = '6c94ea65' col = 76  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' )
      ( tmpl = '6c94ea65' col = 77  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' )
    "  6d2ec22f - 132 columns - US/ZDOM
      ( tmpl = '6d2ec22f' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = '6d2ec22f' col = 2   hdr = 'Customer Code' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = '6d2ec22f' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = '6d2ec22f' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = '6d2ec22f' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = '6d2ec22f' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = '6d2ec22f' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = '6d2ec22f' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' )
      ( tmpl = '6d2ec22f' col = 9   hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' )
      ( tmpl = '6d2ec22f' col = 10  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' )
      ( tmpl = '6d2ec22f' col = 11  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' )
      ( tmpl = '6d2ec22f' col = 12  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' )
      ( tmpl = '6d2ec22f' col = 13  hdr = 'aLWAYS x' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = '6d2ec22f' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = '6d2ec22f' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = '6d2ec22f' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = '6d2ec22f' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = '6d2ec22f' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = '6d2ec22f' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = '6d2ec22f' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = '6d2ec22f' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = '6d2ec22f' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = '6d2ec22f' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = '6d2ec22f' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = '6d2ec22f' col = 25  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' )
      ( tmpl = '6d2ec22f' col = 26  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = '6d2ec22f' col = 27  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = '6d2ec22f' col = 28  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 29  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = '6d2ec22f' col = 30  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = '6d2ec22f' col = 31  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = '6d2ec22f' col = 32  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = '6d2ec22f' col = 33  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = '6d2ec22f' col = 34  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = '6d2ec22f' col = 35  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = '6d2ec22f' col = 36  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = '6d2ec22f' col = 37  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 38  hdr = 'Attribute 1' node = 'C' fld = 'KATR1' fmt = '' )
      ( tmpl = '6d2ec22f' col = 39  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( tmpl = '6d2ec22f' col = 40  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
      ( tmpl = '6d2ec22f' col = 41  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' )
      ( tmpl = '6d2ec22f' col = 42  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = '' )
      ( tmpl = '6d2ec22f' col = 43  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' )
      ( tmpl = '6d2ec22f' col = 44  hdr = 'GST TDS Registration' node = 'C' fld = 'GST_TDS' fmt = '' )
      ( tmpl = '6d2ec22f' col = 45  hdr = 'Aadhaar Number' node = 'I' fld = 'X90003' fmt = '' )
      ( tmpl = '6d2ec22f' col = 46  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( tmpl = '6d2ec22f' col = 47  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( tmpl = '6d2ec22f' col = 48  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = '' )
      ( tmpl = '6d2ec22f' col = 49  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' )
      ( tmpl = '6d2ec22f' col = 50  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 51  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' )
      ( tmpl = '6d2ec22f' col = 52  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( tmpl = '6d2ec22f' col = 53  hdr = 'Tolerance group for the business partner/G/L account' node = 'B' fld = 'TOGRU' fmt = '' )
      ( tmpl = '6d2ec22f' col = 54  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' )
      ( tmpl = '6d2ec22f' col = 55  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' )
      ( tmpl = '6d2ec22f' col = 56  hdr = 'Block Key for Payment' node = 'B' fld = 'ZAHLS' fmt = '' )
      ( tmpl = '6d2ec22f' col = 57  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = '6d2ec22f' col = 58  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = '6d2ec22f' col = 59  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = '6d2ec22f' col = 60  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = '6d2ec22f' col = 61  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = '6d2ec22f' col = 62  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( tmpl = '6d2ec22f' col = 63  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' )
      ( tmpl = '6d2ec22f' col = 64  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' )
      ( tmpl = '6d2ec22f' col = 65  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = '6d2ec22f' col = 66  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = '6d2ec22f' col = 67  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = '6d2ec22f' col = 68  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = '6d2ec22f' col = 69  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = '6d2ec22f' col = 70  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = '6d2ec22f' col = 71  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = '6d2ec22f' col = 72  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = '6d2ec22f' col = 73  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( tmpl = '6d2ec22f' col = 74  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = '6d2ec22f' col = 75  hdr = 'JOIG IN:Central GST - OP' node = 'T' fld = 'JOCG' fmt = '' )
      ( tmpl = '6d2ec22f' col = 76  hdr = 'JTC1 IN: 206C(1H) Goods' node = 'T' fld = 'JTC1' fmt = '' )
      ( tmpl = '6d2ec22f' col = 77  hdr = 'JTX1 Tax Jurisdict.Code d' node = 'T' fld = 'JTX1' fmt = '' )
      ( tmpl = '6d2ec22f' col = 78  hdr = 'JTX2 Tax Jurisdict.Code d' node = 'T' fld = 'JTX2' fmt = '' )
      ( tmpl = '6d2ec22f' col = 79  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = '6d2ec22f' col = 80  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = '6d2ec22f' col = 81  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = '6d2ec22f' col = 82  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = '6d2ec22f' col = 83  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = '6d2ec22f' col = 84  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = '6d2ec22f' col = 85  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = '6d2ec22f' col = 86  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' )
      ( tmpl = '6d2ec22f' col = 87  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( tmpl = '6d2ec22f' col = 88  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 89  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( tmpl = '6d2ec22f' col = 90  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 91  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 92  hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' )
      ( tmpl = '6d2ec22f' col = 93  hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 94  hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' )
      ( tmpl = '6d2ec22f' col = 95  hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 96  hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' )
      ( tmpl = '6d2ec22f' col = 97  hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 98  hdr = 'Retails Lic No (20 and 21 )' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' )
      ( tmpl = '6d2ec22f' col = 99  hdr = 'SC_EXEMPT' node = 'Z' fld = 'SC_EXEMPT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 100 hdr = 'Retails Lic Exp date' node = 'Z' fld = 'RETAIL_EXP' fmt = '' )
      ( tmpl = '6d2ec22f' col = 101 hdr = 'Mfg License (Gen) Number' node = 'Z' fld = 'MFGLIC1NO' fmt = '' )
      ( tmpl = '6d2ec22f' col = 102 hdr = 'Mfg License (Nar) Number' node = 'Z' fld = 'MFGLIC2NO' fmt = '' )
      ( tmpl = '6d2ec22f' col = 103 hdr = 'Mfg License (CC) Number' node = 'Z' fld = 'MFGLIC3NO' fmt = '' )
      ( tmpl = '6d2ec22f' col = 104 hdr = 'Bank Guarantee(Y/N)' node = 'Z' fld = 'BGYN' fmt = '' )
      ( tmpl = '6d2ec22f' col = 105 hdr = 'Bank Guarantee No' node = 'Z' fld = 'BG_NO' fmt = '' )
      ( tmpl = '6d2ec22f' col = 106 hdr = 'BG Amount' node = 'Z' fld = 'BG_AMT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 107 hdr = 'SD Document Currency' node = 'Z' fld = 'CURRENCY' fmt = '' )
      ( tmpl = '6d2ec22f' col = 108 hdr = 'BG Issue Date' node = 'Z' fld = 'BG_ISS_DT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 109 hdr = 'BG Expiry Date' node = 'Z' fld = 'BG_EXP_DT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 110 hdr = 'BG Issuing Bank' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' )
      ( tmpl = '6d2ec22f' col = 111 hdr = 'Agreement Expiry Date' node = 'Z' fld = 'AGGR_EXPDT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 112 hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 113 hdr = 'Customer group' node = 'Z' fld = 'KDGRP' fmt = '' )
      ( tmpl = '6d2ec22f' col = 114 hdr = 'AIOCD Code' node = 'Z' fld = 'AIOCD_CODE' fmt = '' )
      ( tmpl = '6d2ec22f' col = 115 hdr = 'Customer Bank Name' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' )
      ( tmpl = '6d2ec22f' col = 116 hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' )
      ( tmpl = '6d2ec22f' col = 117 hdr = 'Route Code' node = 'Z' fld = 'ZTROUT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 118 hdr = 'Extension' node = 'Z' fld = 'EXTENSION' fmt = '' )
      ( tmpl = '6d2ec22f' col = 119 hdr = 'Route' node = 'Z' fld = 'ZCROUT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 120 hdr = 'GLN URI Format' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 121 hdr = 'DUNS_Number' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' )
      ( tmpl = '6d2ec22f' col = 122 hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' )
      ( tmpl = '6d2ec22f' col = 123 hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' )
      ( tmpl = '6d2ec22f' col = 124 hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' )
      ( tmpl = '6d2ec22f' col = 125 hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' )
      ( tmpl = '6d2ec22f' col = 126 hdr = 'Import_License/MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' )
      ( tmpl = '6d2ec22f' col = 127 hdr = 'IMPL/MIA_From_Date' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = '' )
      ( tmpl = '6d2ec22f' col = 128 hdr = 'IMPL/MIA_Valid_Date' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = '' )
      ( tmpl = '6d2ec22f' col = 129 hdr = 'Check Digit' node = 'Z' fld = 'CHECK_DIGIT' fmt = '' )
      ( tmpl = '6d2ec22f' col = 130 hdr = 'Global Company Prefix' node = 'Z' fld = 'GLOBAL_COM' fmt = '' )
      ( tmpl = '6d2ec22f' col = 131 hdr = 'Backorder Days' node = 'Z' fld = 'BO_DAYS' fmt = '' )
      ( tmpl = '6d2ec22f' col = 132 hdr = 'Location Number' node = 'Z' fld = 'LOCATION_NUMBER' fmt = '' )
    "  6e6467eb - 74 columns - KE/YVTO, US/YVMI, US/YVSP, US/YVTO
      ( tmpl = '6e6467eb' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = '6e6467eb' col = 2   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = '6e6467eb' col = 3   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = '6e6467eb' col = 4   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = '6e6467eb' col = 5   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = '6e6467eb' col = 6   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = '6e6467eb' col = 7   hdr = 'Always X' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = '6e6467eb' col = 8   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = '6e6467eb' col = 9   hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = '6e6467eb' col = 10  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = '6e6467eb' col = 11  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = '6e6467eb' col = 12  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = '6e6467eb' col = 13  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = '6e6467eb' col = 14  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = '6e6467eb' col = 15  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = '6e6467eb' col = 16  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = '6e6467eb' col = 17  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = '6e6467eb' col = 18  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = '6e6467eb' col = 19  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = '6e6467eb' col = 20  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = '6e6467eb' col = 21  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = '6e6467eb' col = 22  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = '6e6467eb' col = 23  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = '6e6467eb' col = 24  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = '6e6467eb' col = 25  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = '6e6467eb' col = 26  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = '6e6467eb' col = 27  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = '6e6467eb' col = 28  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = '6e6467eb' col = 29  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = '6e6467eb' col = 30  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' )
      ( tmpl = '6e6467eb' col = 31  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( tmpl = '6e6467eb' col = 32  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
      ( tmpl = '6e6467eb' col = 33  hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' )
      ( tmpl = '6e6467eb' col = 34  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( tmpl = '6e6467eb' col = 35  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( tmpl = '6e6467eb' col = 36  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = '' )
    ).
  ENDMETHOD.

  METHOD map_4.
    rt = VALUE tt_col(
      ( tmpl = '6e6467eb' col = 37  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' )
      ( tmpl = '6e6467eb' col = 38  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' )
      ( tmpl = '6e6467eb' col = 39  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( tmpl = '6e6467eb' col = 40  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' )
      ( tmpl = '6e6467eb' col = 41  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' )
      ( tmpl = '6e6467eb' col = 42  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = '6e6467eb' col = 43  hdr = 'Order probability of the item' node = 'S' fld = 'AWAHR' fmt = '' )
      ( tmpl = '6e6467eb' col = 44  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = '6e6467eb' col = 45  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = '6e6467eb' col = 46  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = '6e6467eb' col = 47  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = '6e6467eb' col = 48  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( tmpl = '6e6467eb' col = 49  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' )
      ( tmpl = '6e6467eb' col = 50  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = '6e6467eb' col = 51  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = '6e6467eb' col = 52  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = '6e6467eb' col = 53  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = '6e6467eb' col = 54  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = '6e6467eb' col = 55  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = '6e6467eb' col = 56  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = '6e6467eb' col = 57  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = '6e6467eb' col = 58  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( tmpl = '6e6467eb' col = 59  hdr = 'Customer Account Assignment Group' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = '6e6467eb' col = 60  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' )
      ( tmpl = '6e6467eb' col = 61  hdr = 'Tax classification for customer' node = 'T' fld = '#2' fmt = '' )
      ( tmpl = '6e6467eb' col = 62  hdr = 'Tax classification for customer' node = 'T' fld = '#3' fmt = '' )
      ( tmpl = '6e6467eb' col = 63  hdr = 'Tax classification for customer' node = 'T' fld = '#4' fmt = '' )
      ( tmpl = '6e6467eb' col = 64  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = '6e6467eb' col = 65  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = '6e6467eb' col = 66  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = '6e6467eb' col = 67  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = '6e6467eb' col = 68  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = '6e6467eb' col = 69  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = '6e6467eb' col = 70  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = '6e6467eb' col = 71  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' )
      ( tmpl = '6e6467eb' col = 72  hdr = '20B. Lic. No' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( tmpl = '6e6467eb' col = 73  hdr = '21B. Lic. No' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( tmpl = '6e6467eb' col = 74  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' )
    "  80d23dd8 - 76 columns - AE/ZSHP, US/ZSHP
      ( tmpl = '80d23dd8' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = '80d23dd8' col = 2   hdr = 'Customer code' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = '80d23dd8' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = '80d23dd8' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = '80d23dd8' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = '80d23dd8' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = '80d23dd8' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = '80d23dd8' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' )
      ( tmpl = '80d23dd8' col = 9   hdr = 'Ref Company Code' node = '-' fld = '' fmt = '' )
      ( tmpl = '80d23dd8' col = 10  hdr = 'Ref Sales Organization' node = '-' fld = '' fmt = '' )
      ( tmpl = '80d23dd8' col = 11  hdr = 'Ref Distribution Channel' node = '-' fld = '' fmt = '' )
      ( tmpl = '80d23dd8' col = 12  hdr = 'Ref Division' node = '-' fld = '' fmt = '' )
      ( tmpl = '80d23dd8' col = 13  hdr = 'Always X' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = '80d23dd8' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = '80d23dd8' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = '80d23dd8' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = '80d23dd8' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = '80d23dd8' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = '80d23dd8' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = '80d23dd8' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = '80d23dd8' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = '80d23dd8' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = '80d23dd8' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = '80d23dd8' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = '80d23dd8' col = 25  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = '80d23dd8' col = 26  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = '80d23dd8' col = 27  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = '80d23dd8' col = 28  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = '80d23dd8' col = 29  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = '80d23dd8' col = 30  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = '80d23dd8' col = 31  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = '80d23dd8' col = 32  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = '80d23dd8' col = 33  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = '80d23dd8' col = 34  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = '80d23dd8' col = 35  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = '80d23dd8' col = 36  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = '80d23dd8' col = 37  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( tmpl = '80d23dd8' col = 38  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
      ( tmpl = '80d23dd8' col = 39  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' )
      ( tmpl = '80d23dd8' col = 40  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = '' )
      ( tmpl = '80d23dd8' col = 41  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' )
      ( tmpl = '80d23dd8' col = 42  hdr = 'Tax Number 2' node = 'C' fld = 'STCD2' fmt = '' )
      ( tmpl = '80d23dd8' col = 43  hdr = 'Tax Number 1' node = 'C' fld = 'STCD1' fmt = '' )
      ( tmpl = '80d23dd8' col = 44  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' )
      ( tmpl = '80d23dd8' col = 45  hdr = 'VAT Registration Number' node = 'C' fld = 'STCEG' fmt = '' )
      ( tmpl = '80d23dd8' col = 46  hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' )
      ( tmpl = '80d23dd8' col = 47  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = '80d23dd8' col = 48  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = '80d23dd8' col = 49  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = '80d23dd8' col = 50  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = '80d23dd8' col = 51  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = '80d23dd8' col = 52  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = '80d23dd8' col = 53  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = '80d23dd8' col = 54  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = '80d23dd8' col = 55  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = '80d23dd8' col = 56  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = '80d23dd8' col = 57  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = '80d23dd8' col = 58  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = '80d23dd8' col = 59  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = '80d23dd8' col = 60  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = '80d23dd8' col = 61  hdr = 'Tax classification for customer' node = 'T' fld = 'MWST' fmt = '' )
      ( tmpl = '80d23dd8' col = 62  hdr = 'UTX2' node = 'T' fld = 'UTX2' fmt = '' )
      ( tmpl = '80d23dd8' col = 63  hdr = 'UTX3' node = 'T' fld = 'UTX3' fmt = '' )
      ( tmpl = '80d23dd8' col = 64  hdr = 'UTXJ' node = 'T' fld = 'UTXJ' fmt = '' )
      ( tmpl = '80d23dd8' col = 65  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = '80d23dd8' col = 66  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = '80d23dd8' col = 67  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = '80d23dd8' col = 68  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = '80d23dd8' col = 69  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = '80d23dd8' col = 70  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = '80d23dd8' col = 71  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = '80d23dd8' col = 72  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( tmpl = '80d23dd8' col = 73  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' )
      ( tmpl = '80d23dd8' col = 74  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( tmpl = '80d23dd8' col = 75  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' )
      ( tmpl = '80d23dd8' col = 76  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' )
    "  84513e29 - 38 columns - IN/ZNOT
      ( tmpl = '84513e29' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = '84513e29' col = 2   hdr = 'Ref Customer Code  as sample' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = '84513e29' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = '84513e29' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = '84513e29' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = '84513e29' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = '84513e29' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = '84513e29' col = 8   hdr = 'Number of contact person' node = 'P' fld = 'PARNR' fmt = '' )
      ( tmpl = '84513e29' col = 9   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' )
      ( tmpl = '84513e29' col = 10  hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' )
      ( tmpl = '84513e29' col = 11  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' )
      ( tmpl = '84513e29' col = 12  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' )
      ( tmpl = '84513e29' col = 13  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' )
      ( tmpl = '84513e29' col = 14  hdr = 'aLWAYS x' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = '84513e29' col = 15  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = '84513e29' col = 16  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = '84513e29' col = 17  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = '84513e29' col = 18  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = '84513e29' col = 19  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = '84513e29' col = 20  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = '84513e29' col = 21  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = '84513e29' col = 22  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = '84513e29' col = 23  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = '84513e29' col = 24  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = '84513e29' col = 25  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = '84513e29' col = 26  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' )
      ( tmpl = '84513e29' col = 27  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = '84513e29' col = 28  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = '84513e29' col = 29  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = '84513e29' col = 30  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = '84513e29' col = 31  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = '84513e29' col = 32  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = '84513e29' col = 33  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = '84513e29' col = 34  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = '84513e29' col = 35  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = '84513e29' col = 36  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = '84513e29' col = 37  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = '84513e29' col = 38  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
    "  8a74041a - 73 columns - AU/ZSHP, BE/ZSHP, ES/ZSHP, GB/ZSHP, NL/ZSHP, UG/ZSHP
      ( tmpl = '8a74041a' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = '8a74041a' col = 2   hdr = 'Customer code' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = '8a74041a' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = '8a74041a' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = '8a74041a' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = '8a74041a' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = '8a74041a' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = '8a74041a' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' )
      ( tmpl = '8a74041a' col = 9   hdr = 'Ref Company Code' node = '-' fld = '' fmt = '' )
      ( tmpl = '8a74041a' col = 10  hdr = 'Ref Sales Organization' node = '-' fld = '' fmt = '' )
      ( tmpl = '8a74041a' col = 11  hdr = 'Ref Distribution Channel' node = '-' fld = '' fmt = '' )
      ( tmpl = '8a74041a' col = 12  hdr = 'Ref Division' node = '-' fld = '' fmt = '' )
      ( tmpl = '8a74041a' col = 13  hdr = 'Always X' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = '8a74041a' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = '8a74041a' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = '8a74041a' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = '8a74041a' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = '8a74041a' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = '8a74041a' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = '8a74041a' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = '8a74041a' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = '8a74041a' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = '8a74041a' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = '8a74041a' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = '8a74041a' col = 25  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = '8a74041a' col = 26  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = '8a74041a' col = 27  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = '8a74041a' col = 28  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = '8a74041a' col = 29  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = '8a74041a' col = 30  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = '8a74041a' col = 31  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = '8a74041a' col = 32  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = '8a74041a' col = 33  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = '8a74041a' col = 34  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = '8a74041a' col = 35  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = '8a74041a' col = 36  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = '8a74041a' col = 37  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( tmpl = '8a74041a' col = 38  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
      ( tmpl = '8a74041a' col = 39  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' )
      ( tmpl = '8a74041a' col = 40  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = '' )
      ( tmpl = '8a74041a' col = 41  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' )
      ( tmpl = '8a74041a' col = 42  hdr = 'Tax Number 2' node = 'C' fld = 'STCD2' fmt = '' )
      ( tmpl = '8a74041a' col = 43  hdr = 'Tax Number 1' node = 'C' fld = 'STCD1' fmt = '' )
      ( tmpl = '8a74041a' col = 44  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' )
      ( tmpl = '8a74041a' col = 45  hdr = 'VAT Registration Number' node = 'C' fld = 'STCEG' fmt = '' )
      ( tmpl = '8a74041a' col = 46  hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' )
      ( tmpl = '8a74041a' col = 47  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = '8a74041a' col = 48  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = '8a74041a' col = 49  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = '8a74041a' col = 50  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = '8a74041a' col = 51  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = '8a74041a' col = 52  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = '8a74041a' col = 53  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = '8a74041a' col = 54  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = '8a74041a' col = 55  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = '8a74041a' col = 56  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = '8a74041a' col = 57  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = '8a74041a' col = 58  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = '8a74041a' col = 59  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = '8a74041a' col = 60  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = '8a74041a' col = 61  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' )
      ( tmpl = '8a74041a' col = 62  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = '8a74041a' col = 63  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = '8a74041a' col = 64  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = '8a74041a' col = 65  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = '8a74041a' col = 66  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = '8a74041a' col = 67  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = '8a74041a' col = 68  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = '8a74041a' col = 69  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( tmpl = '8a74041a' col = 70  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' )
      ( tmpl = '8a74041a' col = 71  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( tmpl = '8a74041a' col = 72  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' )
      ( tmpl = '8a74041a' col = 73  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' )
    "  9c914100 - 140 columns - AU/ZCDP, BE/ZCDP, ES/ZCDP, GB/ZCDP, IN/ZCDP, MA/ZCDP, MA/ZOTC, NL/ZCDP, UG/ZCDP, UG/ZOTC, US/ZCDP
      ( tmpl = '9c914100' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = '9c914100' col = 2   hdr = 'Ref Customer Code  as sample' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = '9c914100' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = '9c914100' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = '9c914100' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = '9c914100' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = '9c914100' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = '9c914100' col = 8   hdr = 'Number of contact person' node = 'P' fld = 'PARNR' fmt = '' )
      ( tmpl = '9c914100' col = 9   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' )
      ( tmpl = '9c914100' col = 10  hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' )
      ( tmpl = '9c914100' col = 11  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' )
      ( tmpl = '9c914100' col = 12  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' )
      ( tmpl = '9c914100' col = 13  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' )
      ( tmpl = '9c914100' col = 14  hdr = 'aLWAYS x' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = '9c914100' col = 15  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = '9c914100' col = 16  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = '9c914100' col = 17  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = '9c914100' col = 18  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = '9c914100' col = 19  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = '9c914100' col = 20  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = '9c914100' col = 21  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = '9c914100' col = 22  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = '9c914100' col = 23  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = '9c914100' col = 24  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = '9c914100' col = 25  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = '9c914100' col = 26  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' )
      ( tmpl = '9c914100' col = 27  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = '9c914100' col = 28  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = '9c914100' col = 29  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = '9c914100' col = 30  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = '9c914100' col = 31  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = '9c914100' col = 32  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = '9c914100' col = 33  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = '9c914100' col = 34  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = '9c914100' col = 35  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = '9c914100' col = 36  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = '9c914100' col = 37  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = '9c914100' col = 38  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = '9c914100' col = 39  hdr = 'Attribute 1' node = 'C' fld = 'KATR1' fmt = '' )
      ( tmpl = '9c914100' col = 40  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( tmpl = '9c914100' col = 41  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
      ( tmpl = '9c914100' col = 42  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' )
      ( tmpl = '9c914100' col = 43  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = '' )
      ( tmpl = '9c914100' col = 44  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' )
      ( tmpl = '9c914100' col = 45  hdr = 'Tax Number 3 ( GST Number)' node = 'C' fld = 'STCD3' fmt = '' )
      ( tmpl = '9c914100' col = 46  hdr = 'Permanent Account Number' node = 'C' fld = 'J_1IPANNO' fmt = '' )
      ( tmpl = '9c914100' col = 47  hdr = 'GST TDS Registration' node = 'C' fld = 'GST_TDS' fmt = '' )
      ( tmpl = '9c914100' col = 48  hdr = 'Aadhaar Number' node = 'I' fld = 'X90003' fmt = '' )
      ( tmpl = '9c914100' col = 49  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( tmpl = '9c914100' col = 50  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( tmpl = '9c914100' col = 51  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = '' )
      ( tmpl = '9c914100' col = 52  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' )
      ( tmpl = '9c914100' col = 53  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' )
      ( tmpl = '9c914100' col = 54  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' )
      ( tmpl = '9c914100' col = 55  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( tmpl = '9c914100' col = 56  hdr = 'Tolerance group for the business partner/G/L account' node = 'B' fld = 'TOGRU' fmt = '' )
      ( tmpl = '9c914100' col = 57  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' )
      ( tmpl = '9c914100' col = 58  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' )
      ( tmpl = '9c914100' col = 59  hdr = 'Block Key for Payment' node = 'B' fld = 'ZAHLS' fmt = '' )
      ( tmpl = '9c914100' col = 60  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = '9c914100' col = 61  hdr = 'Order probab.' node = 'S' fld = 'AWAHR' fmt = '' )
      ( tmpl = '9c914100' col = 62  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = '9c914100' col = 63  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = '9c914100' col = 64  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = '9c914100' col = 65  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = '9c914100' col = 66  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( tmpl = '9c914100' col = 67  hdr = 'Exch. Rate Type M' node = 'S' fld = 'KURST' fmt = '' )
      ( tmpl = '9c914100' col = 68  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' )
      ( tmpl = '9c914100' col = 69  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' )
      ( tmpl = '9c914100' col = 70  hdr = 'Price List' node = 'S' fld = 'PLTYP' fmt = '' )
      ( tmpl = '9c914100' col = 71  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = '9c914100' col = 72  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = '9c914100' col = 73  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = '9c914100' col = 74  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = '9c914100' col = 75  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = '9c914100' col = 76  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = '9c914100' col = 77  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = '9c914100' col = 78  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = '9c914100' col = 79  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( tmpl = '9c914100' col = 80  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = '9c914100' col = 81  hdr = 'JOIG IN:Central GST - OP' node = 'T' fld = 'JOCG' fmt = '' )
      ( tmpl = '9c914100' col = 82  hdr = 'JTC1 IN: 206C(1H) Goods' node = 'T' fld = 'JTC1' fmt = '' )
      ( tmpl = '9c914100' col = 83  hdr = 'JTX1 Tax Jurisdict.Code d' node = 'T' fld = 'JTX1' fmt = '' )
      ( tmpl = '9c914100' col = 84  hdr = 'JTX2 Tax Jurisdict.Code d' node = 'T' fld = 'JTX2' fmt = '' )
      ( tmpl = '9c914100' col = 85  hdr = 'JTX3 Tax Jurisdict.Code d' node = 'T' fld = 'JTX3' fmt = '' )
      ( tmpl = '9c914100' col = 86  hdr = 'JTX4 Tax Jurisdict.Code d' node = 'T' fld = 'JTX4' fmt = '' )
      ( tmpl = '9c914100' col = 87  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = '9c914100' col = 88  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = '9c914100' col = 89  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = '9c914100' col = 90  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = '9c914100' col = 91  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = '9c914100' col = 92  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = '9c914100' col = 93  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = '9c914100' col = 94  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' )
      ( tmpl = '9c914100' col = 95  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( tmpl = '9c914100' col = 96  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' )
      ( tmpl = '9c914100' col = 97  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( tmpl = '9c914100' col = 98  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' )
      ( tmpl = '9c914100' col = 99  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' )
      ( tmpl = '9c914100' col = 100 hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' )
      ( tmpl = '9c914100' col = 101 hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = '' )
      ( tmpl = '9c914100' col = 102 hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' )
      ( tmpl = '9c914100' col = 103 hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = '' )
      ( tmpl = '9c914100' col = 104 hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' )
      ( tmpl = '9c914100' col = 105 hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = '' )
      ( tmpl = '9c914100' col = 106 hdr = 'Retails Lic No (20 and 21 )' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' )
    ).
  ENDMETHOD.

  METHOD map_5.
    rt = VALUE tt_col(
      ( tmpl = '9c914100' col = 107 hdr = 'SC_EXEMPT' node = 'Z' fld = 'SC_EXEMPT' fmt = '' )
      ( tmpl = '9c914100' col = 108 hdr = 'Retails Lic Exp date' node = 'Z' fld = 'RETAIL_EXP' fmt = '' )
      ( tmpl = '9c914100' col = 109 hdr = 'Mfg License (Gen) Number' node = 'Z' fld = 'MFGLIC1NO' fmt = '' )
      ( tmpl = '9c914100' col = 110 hdr = 'Mfg License (Nar) Number' node = 'Z' fld = 'MFGLIC2NO' fmt = '' )
      ( tmpl = '9c914100' col = 111 hdr = 'Mfg License (CC) Number' node = 'Z' fld = 'MFGLIC3NO' fmt = '' )
      ( tmpl = '9c914100' col = 112 hdr = 'Bank Guarantee(Y/N)' node = 'Z' fld = 'BGYN' fmt = '' )
      ( tmpl = '9c914100' col = 113 hdr = 'Bank Guarantee No' node = 'Z' fld = 'BG_NO' fmt = '' )
      ( tmpl = '9c914100' col = 114 hdr = 'BG Amount' node = 'Z' fld = 'BG_AMT' fmt = '' )
      ( tmpl = '9c914100' col = 115 hdr = 'SD Document Currency' node = 'Z' fld = 'CURRENCY' fmt = '' )
      ( tmpl = '9c914100' col = 116 hdr = 'BG Issue Date' node = 'Z' fld = 'BG_ISS_DT' fmt = '' )
      ( tmpl = '9c914100' col = 117 hdr = 'BG Expiry Date' node = 'Z' fld = 'BG_EXP_DT' fmt = '' )
      ( tmpl = '9c914100' col = 118 hdr = 'BG Issuing Bank' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' )
      ( tmpl = '9c914100' col = 119 hdr = 'Agreement Expiry Date' node = 'Z' fld = 'AGGR_EXPDT' fmt = '' )
      ( tmpl = '9c914100' col = 120 hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' )
      ( tmpl = '9c914100' col = 121 hdr = 'Customer group' node = 'Z' fld = 'KDGRP' fmt = '' )
      ( tmpl = '9c914100' col = 122 hdr = 'AIOCD Code' node = 'Z' fld = 'AIOCD_CODE' fmt = '' )
      ( tmpl = '9c914100' col = 123 hdr = 'Customer Bank Name' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' )
      ( tmpl = '9c914100' col = 124 hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' )
      ( tmpl = '9c914100' col = 125 hdr = 'Route Code' node = 'Z' fld = 'ZTROUT' fmt = '' )
      ( tmpl = '9c914100' col = 126 hdr = 'Extension' node = 'Z' fld = 'EXTENSION' fmt = '' )
      ( tmpl = '9c914100' col = 127 hdr = 'Route' node = 'Z' fld = 'ZCROUT' fmt = '' )
      ( tmpl = '9c914100' col = 128 hdr = 'GLN URI Format' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' )
      ( tmpl = '9c914100' col = 129 hdr = 'DUNS_Number' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' )
      ( tmpl = '9c914100' col = 130 hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' )
      ( tmpl = '9c914100' col = 131 hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' )
      ( tmpl = '9c914100' col = 132 hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' )
      ( tmpl = '9c914100' col = 133 hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' )
      ( tmpl = '9c914100' col = 134 hdr = 'Import_License/MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' )
      ( tmpl = '9c914100' col = 135 hdr = 'IMPL/MIA_From_Date' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = '' )
      ( tmpl = '9c914100' col = 136 hdr = 'IMPL/MIA_Valid_Date' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = '' )
      ( tmpl = '9c914100' col = 137 hdr = 'Check Digit' node = 'Z' fld = 'CHECK_DIGIT' fmt = '' )
      ( tmpl = '9c914100' col = 138 hdr = 'Global Company Prefix' node = 'Z' fld = 'GLOBAL_COM' fmt = '' )
      ( tmpl = '9c914100' col = 139 hdr = 'Backorder Days' node = 'Z' fld = 'BO_DAYS' fmt = '' )
      ( tmpl = '9c914100' col = 140 hdr = 'Location Number' node = 'Z' fld = 'LOCATION_NUMBER' fmt = '' )
    "  ab38ead5 - 79 columns - AE/ZEXP, AU/ZEXP, BE/ZEXP, ES/ZEXP, GB/ZEXP, KE/ZEXP, NL/ZEXP, US/ZEXP, ZA/ZEXP
      ( tmpl = 'ab38ead5' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = 'ab38ead5' col = 2   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = 'ab38ead5' col = 3   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = 'ab38ead5' col = 4   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = 'ab38ead5' col = 5   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = 'ab38ead5' col = 6   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = 'ab38ead5' col = 7   hdr = 'Always X' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = 'ab38ead5' col = 8   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = 'ab38ead5' col = 9   hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = 'ab38ead5' col = 10  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = 'ab38ead5' col = 11  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = 'ab38ead5' col = 12  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = 'ab38ead5' col = 13  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = 'ab38ead5' col = 14  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = 'ab38ead5' col = 15  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = 'ab38ead5' col = 16  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = 'ab38ead5' col = 17  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = 'ab38ead5' col = 18  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = 'ab38ead5' col = 19  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = 'ab38ead5' col = 20  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = 'ab38ead5' col = 21  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = 'ab38ead5' col = 22  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = 'ab38ead5' col = 23  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = 'ab38ead5' col = 24  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = 'ab38ead5' col = 25  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = 'ab38ead5' col = 26  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = 'ab38ead5' col = 27  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = 'ab38ead5' col = 28  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = 'ab38ead5' col = 29  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = 'ab38ead5' col = 30  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = 'ab38ead5' col = 31  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( tmpl = 'ab38ead5' col = 32  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
      ( tmpl = 'ab38ead5' col = 33  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' )
      ( tmpl = 'ab38ead5' col = 34  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = '' )
      ( tmpl = 'ab38ead5' col = 35  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' )
      ( tmpl = 'ab38ead5' col = 36  hdr = 'Permanent Account Number' node = 'C' fld = 'J_1IPANNO' fmt = '' )
      ( tmpl = 'ab38ead5' col = 37  hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' )
      ( tmpl = 'ab38ead5' col = 38  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( tmpl = 'ab38ead5' col = 39  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( tmpl = 'ab38ead5' col = 40  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = '' )
      ( tmpl = 'ab38ead5' col = 41  hdr = 'Value Adjustment Key' node = 'B' fld = 'WBRSL' fmt = '' )
      ( tmpl = 'ab38ead5' col = 42  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' )
      ( tmpl = 'ab38ead5' col = 43  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' )
      ( tmpl = 'ab38ead5' col = 44  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( tmpl = 'ab38ead5' col = 45  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' )
      ( tmpl = 'ab38ead5' col = 46  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' )
      ( tmpl = 'ab38ead5' col = 47  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = 'ab38ead5' col = 48  hdr = 'Order probability of the item' node = 'S' fld = 'AWAHR' fmt = '' )
      ( tmpl = 'ab38ead5' col = 49  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = 'ab38ead5' col = 50  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = 'ab38ead5' col = 51  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = 'ab38ead5' col = 52  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = 'ab38ead5' col = 53  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( tmpl = 'ab38ead5' col = 54  hdr = 'Exchange Rate Type' node = 'S' fld = 'KURST' fmt = '' )
      ( tmpl = 'ab38ead5' col = 55  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' )
      ( tmpl = 'ab38ead5' col = 56  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = 'ab38ead5' col = 57  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = 'ab38ead5' col = 58  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = 'ab38ead5' col = 59  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = 'ab38ead5' col = 60  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = 'ab38ead5' col = 61  hdr = 'Partial delivery at item level' node = 'S' fld = 'KZTLF' fmt = '' )
      ( tmpl = 'ab38ead5' col = 62  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = 'ab38ead5' col = 63  hdr = 'Underdelivery Tolerance Limit' node = 'S' fld = 'UNTTO' fmt = '' )
      ( tmpl = 'ab38ead5' col = 64  hdr = 'Overdelivery Tolerance Limit' node = 'S' fld = 'UEBTO' fmt = '' )
      ( tmpl = 'ab38ead5' col = 65  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = 'ab38ead5' col = 66  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = 'ab38ead5' col = 67  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( tmpl = 'ab38ead5' col = 68  hdr = 'Customer Account Assignment Group' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = 'ab38ead5' col = 69  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' )
      ( tmpl = 'ab38ead5' col = 70  hdr = 'Tax classification for customer' node = 'T' fld = '#2' fmt = '' )
      ( tmpl = 'ab38ead5' col = 71  hdr = 'Tax classification for customer' node = 'T' fld = '#3' fmt = '' )
      ( tmpl = 'ab38ead5' col = 72  hdr = 'Tax classification for customer' node = 'T' fld = '#4' fmt = '' )
      ( tmpl = 'ab38ead5' col = 73  hdr = 'Tax classification for customer' node = 'T' fld = '#5' fmt = '' )
      ( tmpl = 'ab38ead5' col = 74  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = 'ab38ead5' col = 75  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = 'ab38ead5' col = 76  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = 'ab38ead5' col = 77  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = 'ab38ead5' col = 78  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = 'ab38ead5' col = 79  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
    "  c9c1a8dd - 16 columns - */*
      ( tmpl = 'BLOCK' col = 1   hdr = 'Blocking or unblocking' node = '-' fld = '' fmt = '' )
      ( tmpl = 'BLOCK' col = 2   hdr = 'Customer Account Number' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = 'BLOCK' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = 'BLOCK' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = 'BLOCK' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = 'BLOCK' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = 'BLOCK' col = 7   hdr = 'Central posting block' node = 'C' fld = 'SPERR' fmt = '' )
      ( tmpl = 'BLOCK' col = 8   hdr = 'Posting block for company code' node = 'B' fld = 'SPERR' fmt = '' )
      ( tmpl = 'BLOCK' col = 9   hdr = 'Central order block for customer' node = 'C' fld = 'AUFSD' fmt = '' )
      ( tmpl = 'BLOCK' col = 10  hdr = 'Customer order block (sales area)' node = 'S' fld = 'AUFSD' fmt = '' )
      ( tmpl = 'BLOCK' col = 11  hdr = 'Central delivery block for the customer' node = 'C' fld = 'LIFSD' fmt = '' )
      ( tmpl = 'BLOCK' col = 12  hdr = 'Customer delivery block (sales area)' node = 'S' fld = 'LIFSD' fmt = '' )
      ( tmpl = 'BLOCK' col = 13  hdr = 'Central billing block for customer' node = 'C' fld = 'FAKSD' fmt = '' )
      ( tmpl = 'BLOCK' col = 14  hdr = 'Billing block for customer (sales and distribution)' node = 'S' fld = 'FAKSD' fmt = '' )
      ( tmpl = 'BLOCK' col = 15  hdr = 'Central sales block for customer' node = 'C' fld = 'CASSD' fmt = '' )
      ( tmpl = 'BLOCK' col = 16  hdr = 'Sales block for customer (sales area)' node = 'S' fld = 'CASSD' fmt = '' )
    "  d7ee33bb - 65 columns - UG/ZDOM, UG/ZEXP
      ( tmpl = 'd7ee33bb' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 2   hdr = 'Customer code' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = 'd7ee33bb' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 8   hdr = 'Always X' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 9   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = 'd7ee33bb' col = 10  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 11  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 12  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 13  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 14  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 15  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 16  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 17  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 18  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 19  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 20  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 21  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 22  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 23  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 24  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 25  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 26  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 27  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 28  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 29  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 30  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 31  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( tmpl = 'd7ee33bb' col = 32  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 33  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 34  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 35  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 36  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 37  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 38  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 39  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 40  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 41  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 42  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 43  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 44  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 45  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 46  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 47  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 48  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 49  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 50  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 51  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 52  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 53  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 54  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 55  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 56  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 57  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 58  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 59  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 60  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 61  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 62  hdr = 'TIN No' node = 'Z' fld = 'TIN' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 63  hdr = 'Taxpayer Type' node = 'Z' fld = 'TAXP_TYPE' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 64  hdr = 'NINBRN' node = 'Z' fld = 'NINBRN' fmt = '' )
      ( tmpl = 'd7ee33bb' col = 65  hdr = 'Legal Name' node = 'Z' fld = 'LEGL_NAME' fmt = '' )
    "  da47e4b3 - 76 columns - IN/ZEXP
      ( tmpl = 'da47e4b3' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = 'da47e4b3' col = 2   hdr = 'Customer code' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = 'da47e4b3' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = 'da47e4b3' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = 'da47e4b3' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = 'da47e4b3' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = 'da47e4b3' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = 'da47e4b3' col = 8   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = 'da47e4b3' col = 9   hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = 'da47e4b3' col = 10  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = 'da47e4b3' col = 11  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = 'da47e4b3' col = 12  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = 'da47e4b3' col = 13  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = 'da47e4b3' col = 14  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = 'da47e4b3' col = 15  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = 'da47e4b3' col = 16  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = 'da47e4b3' col = 17  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = 'da47e4b3' col = 18  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = 'da47e4b3' col = 19  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' )
      ( tmpl = 'da47e4b3' col = 20  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = 'da47e4b3' col = 21  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = 'da47e4b3' col = 22  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = 'da47e4b3' col = 23  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = 'da47e4b3' col = 24  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = 'da47e4b3' col = 25  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = 'da47e4b3' col = 26  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = 'da47e4b3' col = 27  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = 'da47e4b3' col = 28  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = 'da47e4b3' col = 29  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = 'da47e4b3' col = 30  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = 'da47e4b3' col = 31  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = 'da47e4b3' col = 32  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' )
      ( tmpl = 'da47e4b3' col = 33  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( tmpl = 'da47e4b3' col = 34  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( tmpl = 'da47e4b3' col = 35  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( tmpl = 'da47e4b3' col = 36  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' )
      ( tmpl = 'da47e4b3' col = 37  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' )
      ( tmpl = 'da47e4b3' col = 38  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = 'da47e4b3' col = 39  hdr = 'Order probability of the item' node = 'S' fld = 'AWAHR' fmt = '' )
      ( tmpl = 'da47e4b3' col = 40  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = 'da47e4b3' col = 41  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = 'da47e4b3' col = 42  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = 'da47e4b3' col = 43  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = 'da47e4b3' col = 44  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( tmpl = 'da47e4b3' col = 45  hdr = 'Exchange Rate Type' node = 'S' fld = 'KURST' fmt = '' )
      ( tmpl = 'da47e4b3' col = 46  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' )
      ( tmpl = 'da47e4b3' col = 47  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' )
      ( tmpl = 'da47e4b3' col = 48  hdr = 'Price list type' node = 'S' fld = 'PLTYP' fmt = '' )
      ( tmpl = 'da47e4b3' col = 49  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = 'da47e4b3' col = 50  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = 'da47e4b3' col = 51  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = 'da47e4b3' col = 52  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = 'da47e4b3' col = 53  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = 'da47e4b3' col = 54  hdr = 'Partial delivery at item level' node = 'S' fld = 'KZTLF' fmt = '' )
      ( tmpl = 'da47e4b3' col = 55  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = 'da47e4b3' col = 56  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = 'da47e4b3' col = 57  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = 'da47e4b3' col = 58  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( tmpl = 'da47e4b3' col = 59  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = 'da47e4b3' col = 60  hdr = 'Tax classification for customer' node = 'T' fld = 'JOCG' fmt = '' )
      ( tmpl = 'da47e4b3' col = 61  hdr = 'Tax classification for customer' node = 'T' fld = 'JTC1' fmt = '' )
      ( tmpl = 'da47e4b3' col = 62  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX1' fmt = '' )
      ( tmpl = 'da47e4b3' col = 63  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX2' fmt = '' )
      ( tmpl = 'da47e4b3' col = 64  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX3' fmt = '' )
      ( tmpl = 'da47e4b3' col = 65  hdr = 'Tax classification for customer' node = 'T' fld = 'JTX4' fmt = '' )
      ( tmpl = 'da47e4b3' col = 66  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = 'da47e4b3' col = 67  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = 'da47e4b3' col = 68  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = 'da47e4b3' col = 69  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = 'da47e4b3' col = 70  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = 'da47e4b3' col = 71  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = 'da47e4b3' col = 72  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = 'da47e4b3' col = 73  hdr = 'TIN No' node = 'Z' fld = 'TIN' fmt = '' )
      ( tmpl = 'da47e4b3' col = 74  hdr = 'Taxpayer Type' node = 'Z' fld = 'TAXP_TYPE' fmt = '' )
      ( tmpl = 'da47e4b3' col = 75  hdr = 'NINBRN' node = 'Z' fld = 'NINBRN' fmt = '' )
      ( tmpl = 'da47e4b3' col = 76  hdr = 'Legal Name' node = 'Z' fld = 'LEGL_NAME' fmt = '' )
    "  e10ec770 - 137 columns - BE/ZDOM, ES/ZDOM, GB/ZDOM, NL/ZDOM, ZA/ZDOM
      ( tmpl = 'e10ec770' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = 'e10ec770' col = 2   hdr = 'Customer Account Number' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = 'e10ec770' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = 'e10ec770' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = 'e10ec770' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = 'e10ec770' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = 'e10ec770' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = 'e10ec770' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' )
      ( tmpl = 'e10ec770' col = 9   hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' )
      ( tmpl = 'e10ec770' col = 10  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' )
      ( tmpl = 'e10ec770' col = 11  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' )
      ( tmpl = 'e10ec770' col = 12  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' )
      ( tmpl = 'e10ec770' col = 13  hdr = 'Always X' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = 'e10ec770' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = 'e10ec770' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = 'e10ec770' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = 'e10ec770' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = 'e10ec770' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = 'e10ec770' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = 'e10ec770' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = 'e10ec770' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = 'e10ec770' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = 'e10ec770' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = 'e10ec770' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = 'e10ec770' col = 25  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' )
      ( tmpl = 'e10ec770' col = 26  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = 'e10ec770' col = 27  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = 'e10ec770' col = 28  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = 'e10ec770' col = 29  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = 'e10ec770' col = 30  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = 'e10ec770' col = 31  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = 'e10ec770' col = 32  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = 'e10ec770' col = 33  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = 'e10ec770' col = 34  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = 'e10ec770' col = 35  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = 'e10ec770' col = 36  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = 'e10ec770' col = 37  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = 'e10ec770' col = 38  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( tmpl = 'e10ec770' col = 39  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
      ( tmpl = 'e10ec770' col = 40  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' )
      ( tmpl = 'e10ec770' col = 41  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = '' )
      ( tmpl = 'e10ec770' col = 42  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' )
      ( tmpl = 'e10ec770' col = 43  hdr = 'Tax Number 1' node = 'C' fld = 'STCD1' fmt = '' )
      ( tmpl = 'e10ec770' col = 44  hdr = 'Tax Number 2' node = 'C' fld = 'STCD2' fmt = '' )
      ( tmpl = 'e10ec770' col = 45  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' )
      ( tmpl = 'e10ec770' col = 46  hdr = 'Liable for VAT' node = 'C' fld = 'STKZU' fmt = '' )
      ( tmpl = 'e10ec770' col = 47  hdr = 'Account number of the master record with the fiscal address' node = 'C' fld = 'FISKN' fmt = 'AL' )
      ( tmpl = 'e10ec770' col = 48  hdr = 'VAT Registration Number' node = 'C' fld = 'STCEG' fmt = '' )
      ( tmpl = 'e10ec770' col = 49  hdr = 'First name' node = 'P' fld = 'NAMEV' fmt = '' )
      ( tmpl = 'e10ec770' col = 50  hdr = 'Name 1' node = 'P' fld = 'NAME1' fmt = '' )
      ( tmpl = 'e10ec770' col = 51  hdr = 'Contact person department' node = 'P' fld = 'ABTNR' fmt = '' )
      ( tmpl = 'e10ec770' col = 52  hdr = 'Contact person function' node = 'P' fld = 'PAFKT' fmt = '' )
      ( tmpl = 'e10ec770' col = 53  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( tmpl = 'e10ec770' col = 54  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( tmpl = 'e10ec770' col = 55  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = '' )
      ( tmpl = 'e10ec770' col = 56  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' )
      ( tmpl = 'e10ec770' col = 57  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' )
      ( tmpl = 'e10ec770' col = 58  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' )
      ( tmpl = 'e10ec770' col = 59  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( tmpl = 'e10ec770' col = 60  hdr = 'Tolerance group for the business partner/G/L account' node = 'B' fld = 'TOGRU' fmt = '' )
    ).
  ENDMETHOD.

  METHOD map_6.
    rt = VALUE tt_col(
      ( tmpl = 'e10ec770' col = 61  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' )
      ( tmpl = 'e10ec770' col = 62  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' )
      ( tmpl = 'e10ec770' col = 63  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = 'e10ec770' col = 64  hdr = 'Order probab.' node = 'S' fld = 'AWAHR' fmt = '' )
      ( tmpl = 'e10ec770' col = 65  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = 'e10ec770' col = 66  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = 'e10ec770' col = 67  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = 'e10ec770' col = 68  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = 'e10ec770' col = 69  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( tmpl = 'e10ec770' col = 70  hdr = 'Exch. Rate Type' node = 'S' fld = 'KURST' fmt = '' )
      ( tmpl = 'e10ec770' col = 71  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' )
      ( tmpl = 'e10ec770' col = 72  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' )
      ( tmpl = 'e10ec770' col = 73  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = 'e10ec770' col = 74  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = 'e10ec770' col = 75  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = 'e10ec770' col = 76  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = 'e10ec770' col = 77  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = 'e10ec770' col = 78  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = 'e10ec770' col = 79  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = 'e10ec770' col = 80  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = 'e10ec770' col = 81  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( tmpl = 'e10ec770' col = 82  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = 'e10ec770' col = 83  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' )
      ( tmpl = 'e10ec770' col = 84  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = 'e10ec770' col = 85  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = 'e10ec770' col = 86  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = 'e10ec770' col = 87  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = 'e10ec770' col = 88  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = 'e10ec770' col = 89  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = 'e10ec770' col = 90  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = 'e10ec770' col = 91  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' )
      ( tmpl = 'e10ec770' col = 92  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( tmpl = 'e10ec770' col = 93  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' )
      ( tmpl = 'e10ec770' col = 94  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( tmpl = 'e10ec770' col = 95  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' )
      ( tmpl = 'e10ec770' col = 96  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' )
      ( tmpl = 'e10ec770' col = 97  hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' )
      ( tmpl = 'e10ec770' col = 98  hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = '' )
      ( tmpl = 'e10ec770' col = 99  hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' )
      ( tmpl = 'e10ec770' col = 100 hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = '' )
      ( tmpl = 'e10ec770' col = 101 hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' )
      ( tmpl = 'e10ec770' col = 102 hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = '' )
      ( tmpl = 'e10ec770' col = 103 hdr = 'Retails Lic No (20 and 21 )' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' )
      ( tmpl = 'e10ec770' col = 104 hdr = 'SC_EXEMPT' node = 'Z' fld = 'SC_EXEMPT' fmt = '' )
      ( tmpl = 'e10ec770' col = 105 hdr = 'Retails Lic Exp date' node = 'Z' fld = 'RETAIL_EXP' fmt = '' )
      ( tmpl = 'e10ec770' col = 106 hdr = 'Mfg License (Gen) Number' node = 'Z' fld = 'MFGLIC1NO' fmt = '' )
      ( tmpl = 'e10ec770' col = 107 hdr = 'Mfg License (Nar) Number' node = 'Z' fld = 'MFGLIC2NO' fmt = '' )
      ( tmpl = 'e10ec770' col = 108 hdr = 'Mfg License (CC) Number' node = 'Z' fld = 'MFGLIC3NO' fmt = '' )
      ( tmpl = 'e10ec770' col = 109 hdr = 'Bank Guarantee(Y/N)' node = 'Z' fld = 'BGYN' fmt = '' )
      ( tmpl = 'e10ec770' col = 110 hdr = 'Bank Guarantee No' node = 'Z' fld = 'BG_NO' fmt = '' )
      ( tmpl = 'e10ec770' col = 111 hdr = 'BG Amount' node = 'Z' fld = 'BG_AMT' fmt = '' )
      ( tmpl = 'e10ec770' col = 112 hdr = 'SD Document Currency' node = 'Z' fld = 'CURRENCY' fmt = '' )
      ( tmpl = 'e10ec770' col = 113 hdr = 'BG Issue Date' node = 'Z' fld = 'BG_ISS_DT' fmt = '' )
      ( tmpl = 'e10ec770' col = 114 hdr = 'BG Expiry Date' node = 'Z' fld = 'BG_EXP_DT' fmt = '' )
      ( tmpl = 'e10ec770' col = 115 hdr = 'BG Issuing Bank' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' )
      ( tmpl = 'e10ec770' col = 116 hdr = 'Agreement Expiry Date' node = 'Z' fld = 'AGGR_EXPDT' fmt = '' )
      ( tmpl = 'e10ec770' col = 117 hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' )
      ( tmpl = 'e10ec770' col = 118 hdr = 'Customer group' node = 'Z' fld = 'KDGRP' fmt = '' )
      ( tmpl = 'e10ec770' col = 119 hdr = 'AIOCD Code' node = 'Z' fld = 'AIOCD_CODE' fmt = '' )
      ( tmpl = 'e10ec770' col = 120 hdr = 'Customer Bank Name' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' )
      ( tmpl = 'e10ec770' col = 121 hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' )
      ( tmpl = 'e10ec770' col = 122 hdr = 'Route Code' node = 'Z' fld = 'ZTROUT' fmt = '' )
      ( tmpl = 'e10ec770' col = 123 hdr = 'Extension' node = 'Z' fld = 'EXTENSION' fmt = '' )
      ( tmpl = 'e10ec770' col = 124 hdr = 'Route' node = 'Z' fld = 'ZCROUT' fmt = '' )
      ( tmpl = 'e10ec770' col = 125 hdr = 'GLN URI Format' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' )
      ( tmpl = 'e10ec770' col = 126 hdr = 'DUNS_Number' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' )
      ( tmpl = 'e10ec770' col = 127 hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' )
      ( tmpl = 'e10ec770' col = 128 hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' )
      ( tmpl = 'e10ec770' col = 129 hdr = 'Import_License/MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' )
      ( tmpl = 'e10ec770' col = 130 hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' )
      ( tmpl = 'e10ec770' col = 131 hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' )
      ( tmpl = 'e10ec770' col = 132 hdr = 'IMPL/MIA_From_Date' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = '' )
      ( tmpl = 'e10ec770' col = 133 hdr = 'IMPL/MIA_Valid_Date' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = '' )
      ( tmpl = 'e10ec770' col = 134 hdr = 'Check Digit' node = 'Z' fld = 'CHECK_DIGIT' fmt = '' )
      ( tmpl = 'e10ec770' col = 135 hdr = 'Global Company Prefix' node = 'Z' fld = 'GLOBAL_COM' fmt = '' )
      ( tmpl = 'e10ec770' col = 136 hdr = 'Backorder Days' node = 'Z' fld = 'BO_DAYS' fmt = '' )
      ( tmpl = 'e10ec770' col = 137 hdr = 'Location Number' node = 'Z' fld = 'LOCATION_NUMBER' fmt = '' )
    "  f10a66f5 - 36 columns - IN/ZSUB, UG/ZSUB
      ( tmpl = 'f10a66f5' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = 'f10a66f5' col = 2   hdr = 'Customer Code  as sample' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = 'f10a66f5' col = 3   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = 'f10a66f5' col = 4   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = 'f10a66f5' col = 5   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = 'f10a66f5' col = 6   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = 'f10a66f5' col = 7   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' )
      ( tmpl = 'f10a66f5' col = 8   hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' )
      ( tmpl = 'f10a66f5' col = 9   hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' )
      ( tmpl = 'f10a66f5' col = 10  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' )
      ( tmpl = 'f10a66f5' col = 11  hdr = 'aLWAYS x' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = 'f10a66f5' col = 12  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = 'f10a66f5' col = 13  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = 'f10a66f5' col = 14  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = 'f10a66f5' col = 15  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = 'f10a66f5' col = 16  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = 'f10a66f5' col = 17  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = 'f10a66f5' col = 18  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = 'f10a66f5' col = 19  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = 'f10a66f5' col = 20  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = 'f10a66f5' col = 21  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = 'f10a66f5' col = 22  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = 'f10a66f5' col = 23  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' )
      ( tmpl = 'f10a66f5' col = 24  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = 'f10a66f5' col = 25  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = 'f10a66f5' col = 26  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = 'f10a66f5' col = 27  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = 'f10a66f5' col = 28  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = 'f10a66f5' col = 29  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = 'f10a66f5' col = 30  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = 'f10a66f5' col = 31  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = 'f10a66f5' col = 32  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = 'f10a66f5' col = 33  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = 'f10a66f5' col = 34  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = 'f10a66f5' col = 35  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = 'f10a66f5' col = 36  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
    "  f7e2b95a - 83 columns - AU/ZDOM, MA/ZDOM
      ( tmpl = 'f7e2b95a' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 2   hdr = 'Customer Account Number' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = 'f7e2b95a' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 8   hdr = 'Always X' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 9   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = 'f7e2b95a' col = 10  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 11  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 12  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 13  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 14  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 15  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 16  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 17  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 18  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 19  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 20  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 21  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 22  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 23  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 24  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 25  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 26  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 27  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 28  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 29  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 30  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 31  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 32  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 33  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 34  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 35  hdr = 'Tax Number 3' node = 'C' fld = 'STCD3' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 36  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( tmpl = 'f7e2b95a' col = 37  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 38  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' )
      ( tmpl = 'f7e2b95a' col = 39  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 40  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 41  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 42  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 43  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 44  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 45  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 46  hdr = 'ABC class' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 47  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 48  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 49  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 50  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 51  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 52  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 53  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 54  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 55  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 56  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 57  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 58  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 59  hdr = 'Tax classification for customer' node = 'T' fld = '#1' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 60  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 61  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 62  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 63  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 64  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 65  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 66  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 67  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 68  hdr = '20B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 69  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 70  hdr = '21B. Lic. No.' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 71  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 72  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 73  hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 74  hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 75  hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 76  hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 77  hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 78  hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 79  hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 80  hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 81  hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 82  hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' )
      ( tmpl = 'f7e2b95a' col = 83  hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' )
    "  fb40b09b - 136 columns - IN/ZDOM
      ( tmpl = 'fb40b09b' col = 1   hdr = 'Transaction Code' node = 'X' fld = 'XD01' fmt = '' )
      ( tmpl = 'fb40b09b' col = 2   hdr = 'New Customer Code' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( tmpl = 'fb40b09b' col = 3   hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( tmpl = 'fb40b09b' col = 4   hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( tmpl = 'fb40b09b' col = 5   hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( tmpl = 'fb40b09b' col = 6   hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( tmpl = 'fb40b09b' col = 7   hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( tmpl = 'fb40b09b' col = 8   hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' )
      ( tmpl = 'fb40b09b' col = 9   hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' )
      ( tmpl = 'fb40b09b' col = 10  hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' )
      ( tmpl = 'fb40b09b' col = 11  hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' )
      ( tmpl = 'fb40b09b' col = 12  hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' )
      ( tmpl = 'fb40b09b' col = 13  hdr = 'ALWAYS X' node = 'X' fld = 'X' fmt = '' )
      ( tmpl = 'fb40b09b' col = 14  hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( tmpl = 'fb40b09b' col = 15  hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( tmpl = 'fb40b09b' col = 16  hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( tmpl = 'fb40b09b' col = 17  hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( tmpl = 'fb40b09b' col = 18  hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( tmpl = 'fb40b09b' col = 19  hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( tmpl = 'fb40b09b' col = 20  hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( tmpl = 'fb40b09b' col = 21  hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( tmpl = 'fb40b09b' col = 22  hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( tmpl = 'fb40b09b' col = 23  hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( tmpl = 'fb40b09b' col = 24  hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( tmpl = 'fb40b09b' col = 25  hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' )
      ( tmpl = 'fb40b09b' col = 26  hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( tmpl = 'fb40b09b' col = 27  hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( tmpl = 'fb40b09b' col = 28  hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 29  hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( tmpl = 'fb40b09b' col = 30  hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( tmpl = 'fb40b09b' col = 31  hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( tmpl = 'fb40b09b' col = 32  hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( tmpl = 'fb40b09b' col = 33  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( tmpl = 'fb40b09b' col = 34  hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( tmpl = 'fb40b09b' col = 35  hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( tmpl = 'fb40b09b' col = 36  hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( tmpl = 'fb40b09b' col = 37  hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 38  hdr = 'Attribute 1' node = 'C' fld = 'KATR1' fmt = '' )
      ( tmpl = 'fb40b09b' col = 39  hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( tmpl = 'fb40b09b' col = 40  hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
      ( tmpl = 'fb40b09b' col = 41  hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' )
      ( tmpl = 'fb40b09b' col = 42  hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = '' )
      ( tmpl = 'fb40b09b' col = 43  hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' )
      ( tmpl = 'fb40b09b' col = 44  hdr = 'Tax Number 3 ( GST Number)' node = 'C' fld = 'STCD3' fmt = '' )
      ( tmpl = 'fb40b09b' col = 45  hdr = 'Permanent Account Number' node = 'C' fld = 'J_1IPANNO' fmt = '' )
      ( tmpl = 'fb40b09b' col = 46  hdr = 'GST TDS Registration' node = 'C' fld = 'GST_TDS' fmt = '' )
      ( tmpl = 'fb40b09b' col = 47  hdr = 'Aadhaar Number' node = 'I' fld = 'X90003' fmt = '' )
      ( tmpl = 'fb40b09b' col = 48  hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( tmpl = 'fb40b09b' col = 49  hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( tmpl = 'fb40b09b' col = 50  hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = '' )
      ( tmpl = 'fb40b09b' col = 51  hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' )
      ( tmpl = 'fb40b09b' col = 52  hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 53  hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = 'AL' )
      ( tmpl = 'fb40b09b' col = 54  hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( tmpl = 'fb40b09b' col = 55  hdr = 'Tolerance group for the business partner/G/L account' node = 'B' fld = 'TOGRU' fmt = '' )
      ( tmpl = 'fb40b09b' col = 56  hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' )
      ( tmpl = 'fb40b09b' col = 57  hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' )
      ( tmpl = 'fb40b09b' col = 58  hdr = 'Block Key for Payment' node = 'B' fld = 'ZAHLS' fmt = '' )
      ( tmpl = 'fb40b09b' col = 59  hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( tmpl = 'fb40b09b' col = 60  hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( tmpl = 'fb40b09b' col = 61  hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( tmpl = 'fb40b09b' col = 62  hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( tmpl = 'fb40b09b' col = 63  hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( tmpl = 'fb40b09b' col = 64  hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( tmpl = 'fb40b09b' col = 65  hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' )
      ( tmpl = 'fb40b09b' col = 66  hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' )
      ( tmpl = 'fb40b09b' col = 67  hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( tmpl = 'fb40b09b' col = 68  hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( tmpl = 'fb40b09b' col = 69  hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( tmpl = 'fb40b09b' col = 70  hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( tmpl = 'fb40b09b' col = 71  hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( tmpl = 'fb40b09b' col = 72  hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( tmpl = 'fb40b09b' col = 73  hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( tmpl = 'fb40b09b' col = 74  hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( tmpl = 'fb40b09b' col = 75  hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( tmpl = 'fb40b09b' col = 76  hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( tmpl = 'fb40b09b' col = 77  hdr = 'JOIG IN:Central GST - OP' node = 'T' fld = 'JOCG' fmt = '' )
      ( tmpl = 'fb40b09b' col = 78  hdr = 'JTC1 IN: 206C(1H) Goods' node = 'T' fld = 'JTC1' fmt = '' )
      ( tmpl = 'fb40b09b' col = 79  hdr = 'JTX1 Tax Jurisdict.Code d' node = 'T' fld = 'JTX1' fmt = '' )
      ( tmpl = 'fb40b09b' col = 80  hdr = 'JTX2 Tax Jurisdict.Code d' node = 'T' fld = 'JTX2' fmt = '' )
      ( tmpl = 'fb40b09b' col = 81  hdr = 'JTX3 Tax Jurisdict.Code d' node = 'T' fld = 'JTX3' fmt = '' )
      ( tmpl = 'fb40b09b' col = 82  hdr = 'JTX4 Tax Jurisdict.Code d' node = 'T' fld = 'JTX4' fmt = '' )
      ( tmpl = 'fb40b09b' col = 83  hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( tmpl = 'fb40b09b' col = 84  hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( tmpl = 'fb40b09b' col = 85  hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( tmpl = 'fb40b09b' col = 86  hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( tmpl = 'fb40b09b' col = 87  hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( tmpl = 'fb40b09b' col = 88  hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( tmpl = 'fb40b09b' col = 89  hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = '' )
      ( tmpl = 'fb40b09b' col = 90  hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = '' )
      ( tmpl = 'fb40b09b' col = 91  hdr = '20B. Lic. No' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( tmpl = 'fb40b09b' col = 92  hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 93  hdr = '21B. Lic. No' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( tmpl = 'fb40b09b' col = 94  hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 95  hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 96  hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' )
      ( tmpl = 'fb40b09b' col = 97  hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 98  hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' )
      ( tmpl = 'fb40b09b' col = 99  hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 100 hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' )
      ( tmpl = 'fb40b09b' col = 101 hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 102 hdr = 'Retails Lic No (20 and 21 )' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' )
      ( tmpl = 'fb40b09b' col = 103 hdr = 'SC_EXEMPT' node = 'Z' fld = 'SC_EXEMPT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 104 hdr = 'Retails Lic Exp date' node = 'Z' fld = 'RETAIL_EXP' fmt = '' )
      ( tmpl = 'fb40b09b' col = 105 hdr = 'Mfg License (Gen) Number' node = 'Z' fld = 'MFGLIC1NO' fmt = '' )
      ( tmpl = 'fb40b09b' col = 106 hdr = 'Mfg License (Nar) Number' node = 'Z' fld = 'MFGLIC2NO' fmt = '' )
      ( tmpl = 'fb40b09b' col = 107 hdr = 'Mfg License (CC) Number' node = 'Z' fld = 'MFGLIC3NO' fmt = '' )
      ( tmpl = 'fb40b09b' col = 108 hdr = 'Bank Guarantee(Y/N)' node = 'Z' fld = 'BGYN' fmt = '' )
      ( tmpl = 'fb40b09b' col = 109 hdr = 'Bank Guarantee No' node = 'Z' fld = 'BG_NO' fmt = '' )
      ( tmpl = 'fb40b09b' col = 110 hdr = 'BG Amount' node = 'Z' fld = 'BG_AMT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 111 hdr = 'SD Document Currency' node = 'Z' fld = 'CURRENCY' fmt = '' )
      ( tmpl = 'fb40b09b' col = 112 hdr = 'BG Issue Date' node = 'Z' fld = 'BG_ISS_DT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 113 hdr = 'BG Expiry Date' node = 'Z' fld = 'BG_EXP_DT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 114 hdr = 'BG Issuing Bank' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' )
      ( tmpl = 'fb40b09b' col = 115 hdr = 'Agreement Expiry Date' node = 'Z' fld = 'AGGR_EXPDT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 116 hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 117 hdr = 'Customer group' node = 'Z' fld = 'KDGRP' fmt = '' )
      ( tmpl = 'fb40b09b' col = 118 hdr = 'AIOCD Code' node = 'Z' fld = 'AIOCD_CODE' fmt = '' )
      ( tmpl = 'fb40b09b' col = 119 hdr = 'Customer Bank Name' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' )
      ( tmpl = 'fb40b09b' col = 120 hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' )
      ( tmpl = 'fb40b09b' col = 121 hdr = 'Route Code' node = 'Z' fld = 'ZTROUT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 122 hdr = 'Extension' node = 'Z' fld = 'EXTENSION' fmt = '' )
      ( tmpl = 'fb40b09b' col = 123 hdr = 'Route' node = 'Z' fld = 'ZCROUT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 124 hdr = 'GLN URI Format' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 125 hdr = 'DUNS_Number' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' )
      ( tmpl = 'fb40b09b' col = 126 hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = '' )
      ( tmpl = 'fb40b09b' col = 127 hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = '' )
      ( tmpl = 'fb40b09b' col = 128 hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = '' )
      ( tmpl = 'fb40b09b' col = 129 hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = '' )
      ( tmpl = 'fb40b09b' col = 130 hdr = 'Import_License/MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' )
      ( tmpl = 'fb40b09b' col = 131 hdr = 'IMPL/MIA_From_Date' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = '' )
      ( tmpl = 'fb40b09b' col = 132 hdr = 'IMPL/MIA_Valid_Date' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = '' )
      ( tmpl = 'fb40b09b' col = 133 hdr = 'Check Digit' node = 'Z' fld = 'CHECK_DIGIT' fmt = '' )
      ( tmpl = 'fb40b09b' col = 134 hdr = 'Global Company Prefix' node = 'Z' fld = 'GLOBAL_COM' fmt = '' )
      ( tmpl = 'fb40b09b' col = 135 hdr = 'Backorder Days' node = 'Z' fld = 'BO_DAYS' fmt = '' )
      ( tmpl = 'fb40b09b' col = 136 hdr = 'Location Number' node = 'Z' fld = 'LOCATION_NUMBER' fmt = '' )
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

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING  retfield     = 'KTOKD'
                 dynpprog     = sy-repid
                 dynpnr       = sy-dynnr
                 dynprofield  = 'P_KTOKD'
                 window_title = |Account groups for { lcl_tmpl=>region_text( lv_regn ) }|
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
  CHECK sscrfields-ucomm <> 'RB'.
  lcl_main=>validate( ).

*----------------------------------------------------------------------*
* Main
*----------------------------------------------------------------------*
START-OF-SELECTION.
  lcl_main=>run( ).
