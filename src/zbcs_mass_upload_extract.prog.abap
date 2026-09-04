*&---------------------------------------------------------------------*
*& Report  ZBCS_MASS_UPLOAD_EXTRACT
*&---------------------------------------------------------------------*
*& Builds a ready-to-upload workbook out of master data that already
*& exists, so the two mass upload programs can be tested against real
*& values instead of hand-typed ones.
*&
*& Give it a business partner (or a customer / supplier number) and pick
*& the scenario you want to test. The file that comes back carries the
*& headings of that scenario's tab and one row per company code / sales
*& area (or purchasing organisation) the record has, filled from the
*& system. Feed it straight back into:
*&
*&   ZSDS_CUST_MASS_UPLOAD   scenarios C1 - C7
*&   ZMMS_BP_MASS_UPLOAD     scenarios V1 - V9
*&
*& The data is read through the same interfaces the upload programs
*& write through - CMD_EI_API_EXTRACT=>GET_DATA for customers and
*& VMD_EI_API_EXTRACT=>GET_DATA for suppliers - so a column that can be
*& loaded is a column that can be read back, in the same structures.
*& Nothing is changed: the program only reads.
*&
*& The workbook is written as a real .xlsx (a zip of OpenXML parts, built
*& with CL_ABAP_ZIP), because that is what the upload programs read.
*&---------------------------------------------------------------------*
REPORT zbcs_mass_upload_extract.

TYPES: tt_cell TYPE STANDARD TABLE OF string WITH EMPTY KEY.

TYPES: BEGIN OF ty_row,
         cells TYPE tt_cell,
       END OF ty_row,
       tt_row TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

" One line per column of one scenario's tab: where it sits, what it is
" called, which part of the master record holds it, and how to write it.
TYPES: BEGIN OF ty_col,
         scen TYPE char2,
         col  TYPE i,
         hdr  TYPE char60,
         node TYPE char1,
         fld  TYPE char30,
         fmt  TYPE char2,
       END OF ty_col,
       tt_col TYPE STANDARD TABLE OF ty_col WITH EMPTY KEY.

TYPES: BEGIN OF ty_msg,
         icon    TYPE icon_d,
         objkey  TYPE char20,
         message TYPE string,
       END OF ty_msg,
       tt_msg TYPE STANDARD TABLE OF ty_msg WITH EMPTY KEY.

" The identification category the customer program loads the Aadhaar
" number into.
CONSTANTS gc_id_aadhaar TYPE bu_id_type VALUE 'X90003'.

" The task the extract interface expects on a read request. It is not the
" task of a change - the maintain interface takes I or U - it is what tells
" the extractor which record to assemble.
CONSTANTS gc_task_read TYPE cmd_ei_object_task VALUE 'M'.

" Data objects the SELECT-OPTIONS are built over, and the scenario the
" proposed file name follows.
" The selection screen's own function code, so a radio button click can be
" told apart from the user asking to run the extract.
TABLES sscrfields.

DATA: gv_bp    TYPE bu_partner,
      gv_kunnr TYPE kunnr,
      gv_lifnr TYPE lifnr,
      gv_scen  TYPE char2.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
" All sixteen scenarios are one radio button group, and a group cannot be
" split over two blocks - so they share a block, with a heading line above
" each half.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN COMMENT /1(60) TEXT-002.
PARAMETERS: p_c1 RADIOBUTTON GROUP g1 USER-COMMAND rb DEFAULT 'X',
            p_c2 RADIOBUTTON GROUP g1,
            p_c3 RADIOBUTTON GROUP g1,
            p_c4 RADIOBUTTON GROUP g1,
            p_c5 RADIOBUTTON GROUP g1,
            p_c6 RADIOBUTTON GROUP g1,
            p_c7 RADIOBUTTON GROUP g1.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT /1(60) TEXT-003.
PARAMETERS: p_v1 RADIOBUTTON GROUP g1,
            p_v2 RADIOBUTTON GROUP g1,
            p_v3 RADIOBUTTON GROUP g1,
            p_v4 RADIOBUTTON GROUP g1,
            p_v5 RADIOBUTTON GROUP g1,
            p_v6 RADIOBUTTON GROUP g1,
            p_v7 RADIOBUTTON GROUP g1,
            p_v8 RADIOBUTTON GROUP g1,
            p_v9 RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-004.
SELECT-OPTIONS: s_bp    FOR gv_bp    NO INTERVALS,
                s_kunnr FOR gv_kunnr NO INTERVALS,
                s_lifnr FOR gv_lifnr NO INTERVALS.
PARAMETERS:     p_max   TYPE i DEFAULT 20.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-005.
PARAMETERS: p_file TYPE rlgrap-filename LOWER CASE,
            p_pc   RADIOBUTTON GROUP g2 DEFAULT 'X',
            p_srv  RADIOBUTTON GROUP g2,
            p_blank AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* Exception
*----------------------------------------------------------------------*
CLASS lcx_ext DEFINITION INHERITING FROM cx_static_check FINAL.
  PUBLIC SECTION.
    DATA text TYPE string.
    METHODS constructor IMPORTING iv_text TYPE string.
    METHODS get_text REDEFINITION.
ENDCLASS.

CLASS lcx_ext IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    text = iv_text.
  ENDMETHOD.
  METHOD get_text.
    result = text.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* LCL_UTIL - formatting a stored value into something the upload
*            programs read back the same way
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

    " Leading zeros come off: the upload programs put them back, and a
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
* LCL_XLSX - writes a workbook
*   An .xlsx is a zip of OpenXML parts. Only four are needed for a plain
*   sheet, and every value is written as an inline string so no shared
*   string table or styles are required - which is also what keeps the
*   file readable by CL_FDT_XL_SPREADSHEET on the way back in.
*----------------------------------------------------------------------*
CLASS lcl_xlsx DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS build
      IMPORTING iv_sheet  TYPE clike
                it_head   TYPE tt_cell
                it_row    TYPE tt_row
      RETURNING VALUE(rv) TYPE xstring
      RAISING   lcx_ext.
  PRIVATE SECTION.
    " The workbook is read back by CL_FDT_XL_SPREADSHEET, which is not a
    " general .xlsx reader: it takes the text of a cell from the shared
    " string table and nowhere else, and it expects the parts Excel itself
    " writes. A four-part package of inline strings loads in Excel and is
    " refused by it, so every part below is written and every text cell
    " points into xl/sharedStrings.xml.
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
      RAISING   lcx_ext.
ENDCLASS.

CLASS lcl_xlsx IMPLEMENTATION.

  METHOD to_x.
    TRY.
        rv = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( iv_in ).
      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION NEW lcx_ext( |The workbook could not be encoded: { lx->get_text( ) }| ).
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
      " builds its table from the cells it finds, so a row that starts at B
      " comes back one column short and every value sits one place to the
      " left of where the template says it is. The templates whose first
      " column is a label leave A empty, which is exactly that case.
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

    " ---- shared strings -------------------------------------------------
    DATA(lv_sst) =
      |<?xml version="1.0" encoding="UTF-8" standalone="yes"?>| &&
      |<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" | &&
      |count="{ mv_use }" uniqueCount="{ lines( mt_txt ) }">|.
    LOOP AT mt_txt INTO DATA(lv_t).
      lv_sst = lv_sst && |<si><t xml:space="preserve">{ lcl_util=>xml_escape( lv_t ) }</t></si>|.
    ENDLOOP.
    lv_sst = lv_sst && |</sst>|.

    " ---- styles: one font, one format, which is all that is referenced --
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
      |<dc:creator>ZBCS_MASS_UPLOAD_EXTRACT</dc:creator>| &&
      |<cp:lastModifiedBy>ZBCS_MASS_UPLOAD_EXTRACT</cp:lastModifiedBy>| &&
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
* LCL_MAP - the columns of each scenario's tab
*   Generated from the two upload programs, so the file that comes out
*   is laid out exactly like the file they read.
*     C1-C7  ZSDS_CUST_MASS_UPLOAD   R1-R7
*     V1-V9  ZMMS_BP_MASS_UPLOAD     R1-R9
*----------------------------------------------------------------------*
CLASS lcl_map DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS for   IMPORTING iv_scen   TYPE char2
                        RETURNING VALUE(rt) TYPE tt_col.
    CLASS-METHODS sheet IMPORTING iv_scen   TYPE char2
                        RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS name  IMPORTING iv_scen   TYPE char2
                        RETURNING VALUE(rv) TYPE string.
  PRIVATE SECTION.
    CLASS-DATA mt TYPE tt_col.
    CLASS-METHODS build RETURNING VALUE(rt) TYPE tt_col.
ENDCLASS.

CLASS lcl_map IMPLEMENTATION.

  METHOD for.
    IF mt IS INITIAL.
      mt = build( ).
    ENDIF.
    rt = VALUE #( FOR ls IN mt WHERE ( scen = iv_scen ) ( ls ) ).
    SORT rt BY col.
  ENDMETHOD.

  METHOD sheet.
    rv = SWITCH string( iv_scen
           WHEN 'C1' THEN 'domestic customer IND'
           WHEN 'C2' THEN 'Export customer'
           WHEN 'C3' THEN 'Morocco customer'
           WHEN 'C4' THEN 'SAGA customer'
           WHEN 'C5' THEN 'credit Limit'
           WHEN 'C6' THEN 'domestic customer US'
           WHEN 'C7' THEN 'ship to party US'
           WHEN 'V1' THEN 'Vendor creation for All CC'
           WHEN 'V2' THEN 'TDS upload'
           WHEN 'V3' THEN 'TAN details'
           WHEN 'V4' THEN 'BANK Key creation'
           WHEN 'V5' THEN 'Bank details update'
           WHEN 'V6' THEN 'Vendor extension'
           WHEN 'V7' THEN 'CIN details'
           WHEN 'V8' THEN 'Patner function'
           WHEN 'V9' THEN 'Block_Unblocked'
           ELSE           'Sheet1' ).
  ENDMETHOD.

  METHOD name.
    rv = |{ sheet( iv_scen ) }_sample|.
    " Backquotes, not quotes: a text field literal drops its trailing
    " blanks, so ' ' is an empty search pattern and REPLACE terminates with
    " CX_SY_REPLACE_INFINITE_LOOP.
    REPLACE ALL OCCURRENCES OF ` ` IN rv WITH `_`.
  ENDMETHOD.

  METHOD build.
    APPEND LINES OF VALUE tt_col(
      ( scen = 'C1' col = 1    hdr = 'Transaction Code' node = '-' fld = '' fmt = '' )
      ( scen = 'C1' col = 2    hdr = 'New Customer Code' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( scen = 'C1' col = 3    hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( scen = 'C1' col = 4    hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( scen = 'C1' col = 5    hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( scen = 'C1' col = 6    hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( scen = 'C1' col = 7    hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( scen = 'C1' col = 13   hdr = 'ALWAYS X' node = '-' fld = '' fmt = '' )
      ( scen = 'C1' col = 12   hdr = 'Division that is used as a reference' node = '-' fld = '' fmt = '' )
      ( scen = 'C1' col = 11   hdr = 'Reference distribution channel' node = '-' fld = '' fmt = '' )
      ( scen = 'C1' col = 10   hdr = 'Reference sales organization' node = '-' fld = '' fmt = '' )
      ( scen = 'C1' col = 9    hdr = 'Reference Company Code' node = '-' fld = '' fmt = '' )
      ( scen = 'C1' col = 8    hdr = 'Reference for customer (matchcode field)' node = '-' fld = '' fmt = '' )
      ( scen = 'C1' col = 14   hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( scen = 'C1' col = 15   hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( scen = 'C1' col = 16   hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( scen = 'C1' col = 17   hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( scen = 'C1' col = 18   hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( scen = 'C1' col = 19   hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( scen = 'C1' col = 20   hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( scen = 'C1' col = 21   hdr = 'c/o name' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( scen = 'C1' col = 22   hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( scen = 'C1' col = 23   hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( scen = 'C1' col = 24   hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( scen = 'C1' col = 25   hdr = 'House Number' node = 'A' fld = 'HOUSE_NO' fmt = '' )
      ( scen = 'C1' col = 26   hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( scen = 'C1' col = 27   hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( scen = 'C1' col = 28   hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( scen = 'C1' col = 29   hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( scen = 'C1' col = 30   hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( scen = 'C1' col = 31   hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( scen = 'C1' col = 32   hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( scen = 'C1' col = 33   hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( scen = 'C1' col = 34   hdr = 'First telephone no.: dialling code+number' node = 'M' fld = 'TEL' fmt = '' )
      ( scen = 'C1' col = 35   hdr = 'First Mobile Telephone No.: Dialing Code + Number' node = 'M' fld = 'MOB' fmt = '' )
      ( scen = 'C1' col = 36   hdr = 'First fax no.: dialling code+number' node = 'M' fld = 'FAX' fmt = '' )
      ( scen = 'C1' col = 37   hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( scen = 'C1' col = 38   hdr = 'Attribute 1' node = 'C' fld = 'KATR1' fmt = '' )
      ( scen = 'C1' col = 39   hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( scen = 'C1' col = 40   hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
      ( scen = 'C1' col = 41   hdr = 'Account Number of Vendor or Creditor' node = 'C' fld = 'LIFNR' fmt = 'AL' )
      ( scen = 'C1' col = 42   hdr = 'Company ID of Trading Partner' node = 'C' fld = 'VBUND' fmt = 'AL' )
      ( scen = 'C1' col = 43   hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' )
      ( scen = 'C1' col = 44   hdr = 'Tax Number 3 ( GST Number)' node = 'C' fld = 'STCD3' fmt = '' )
      ( scen = 'C1' col = 45   hdr = 'Permanent Account Number' node = 'C' fld = 'J_1IPANNO' fmt = '' )
      ( scen = 'C1' col = 46   hdr = 'GST TDS Registration' node = 'C' fld = 'GST_TDS' fmt = '' )
      ( scen = 'C1' col = 47   hdr = 'Aadhaar Number' node = 'I' fld = 'X90003' fmt = '' )
      ( scen = 'C1' col = 48   hdr = 'Reconciliation Account in General Ledger' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( scen = 'C1' col = 49   hdr = 'Key for sorting according to assignment numbers' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( scen = 'C1' col = 50   hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = 'AL' )
      ( scen = 'C1' col = 51   hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' )
      ( scen = 'C1' col = 52   hdr = 'Interest calculation frequency in months' node = 'B' fld = 'ZINRT' fmt = '' )
      ( scen = 'C1' col = 53   hdr = 'Previous Master Record Number' node = 'B' fld = 'ALTKN' fmt = '' )
      ( scen = 'C1' col = 54   hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( scen = 'C1' col = 55   hdr = 'Tolerance group for the business partner/G/L account' node = 'B' fld = 'TOGRU' fmt = '' )
      ( scen = 'C1' col = 56   hdr = 'Indicator: Record Payment History ?' node = 'B' fld = 'XZVER' fmt = '' )
      ( scen = 'C1' col = 57   hdr = 'List of the Payment Methods to be Considered' node = 'B' fld = 'ZWELS' fmt = '' )
      ( scen = 'C1' col = 58   hdr = 'Block Key for Payment' node = 'B' fld = 'ZAHLS' fmt = '' )
      ( scen = 'C1' col = 59   hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( scen = 'C1' col = 60   hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( scen = 'C1' col = 61   hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( scen = 'C1' col = 62   hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( scen = 'C1' col = 63   hdr = 'Customer classification (ABC analysis)' node = 'S' fld = 'KLABC' fmt = '' )
      ( scen = 'C1' col = 64   hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( scen = 'C1' col = 65   hdr = 'Price group (customer)' node = 'S' fld = 'KONDA' fmt = '' )
      ( scen = 'C1' col = 66   hdr = 'Pricing procedure assigned to this customer' node = 'S' fld = 'KALKS' fmt = '' )
      ( scen = 'C1' col = 67   hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( scen = 'C1' col = 68   hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( scen = 'C1' col = 69   hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( scen = 'C1' col = 70   hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( scen = 'C1' col = 71   hdr = 'Delivering Plant (Own or External)' node = 'S' fld = 'VWERK' fmt = '' )
      ( scen = 'C1' col = 72   hdr = 'Maximum Number of Partial Deliveries Allowed Per Item' node = 'S' fld = 'ANTLF' fmt = '' )
      ( scen = 'C1' col = 73   hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( scen = 'C1' col = 74   hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( scen = 'C1' col = 75   hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( scen = 'C1' col = 76   hdr = 'Account Assignment Group for Customer' node = 'S' fld = 'KTGRD' fmt = '' )
      ( scen = 'C1' col = 77   hdr = 'JOIG IN:Central GST - OP' node = 'T' fld = 'JOCG' fmt = '' )
      ( scen = 'C1' col = 78   hdr = 'JTC1 IN: 206C(1H) Goods' node = 'T' fld = 'JTC1' fmt = '' )
      ( scen = 'C1' col = 79   hdr = 'JTX1 Tax Jurisdict.Code d' node = 'T' fld = 'JTX1' fmt = '' )
      ( scen = 'C1' col = 80   hdr = 'JTX2 Tax Jurisdict.Code d' node = 'T' fld = 'JTX2' fmt = '' )
      ( scen = 'C1' col = 81   hdr = 'JTX3 Tax Jurisdict.Code d' node = 'T' fld = 'JTX3' fmt = '' )
      ( scen = 'C1' col = 82   hdr = 'JTX4 Tax Jurisdict.Code d' node = 'T' fld = 'JTX4' fmt = '' )
      ( scen = 'C1' col = 83   hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( scen = 'C1' col = 84   hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( scen = 'C1' col = 85   hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( scen = 'C1' col = 86   hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( scen = 'C1' col = 87   hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( scen = 'C1' col = 88   hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( scen = 'C1' col = 89   hdr = 'Transit Day' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = 'NM' )
      ( scen = 'C1' col = 90   hdr = 'Distance in kms.' node = 'Z' fld = 'KMSUM' fmt = 'NM' )
      ( scen = 'C1' col = 91   hdr = '20B. Lic. No' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( scen = 'C1' col = 92   hdr = 'DEA_exempt' node = 'Z' fld = 'DEA_EXEMPT' fmt = '' )
      ( scen = 'C1' col = 93   hdr = '21B. Lic. No' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( scen = 'C1' col = 94   hdr = 'SL_EXEMPT' node = 'Z' fld = 'SL_EXEMPT' fmt = '' )
      ( scen = 'C1' col = 95   hdr = '20B and 21B Expiry Date' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = 'DT' )
      ( scen = 'C1' col = 96   hdr = 'Food Lic' node = 'Z' fld = 'FOODSLICENSE' fmt = '' )
      ( scen = 'C1' col = 97   hdr = 'Food Lic Valid Date' node = 'Z' fld = 'FL_VALIDDT' fmt = 'DT' )
      ( scen = 'C1' col = 98   hdr = 'Sch. X Wh.Sale Lic No' node = 'Z' fld = 'SCHXNO' fmt = '' )
      ( scen = 'C1' col = 99   hdr = 'Schedule-X Wh.Sale Lic. Exp. Date' node = 'Z' fld = 'SCHX_VALIDDT' fmt = 'DT' )
      ( scen = 'C1' col = 100  hdr = 'Sch. X Retail Lic No' node = 'Z' fld = 'SCHXRNO' fmt = '' )
      ( scen = 'C1' col = 101  hdr = 'Sch. X Retail Lic Exp. Date' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = 'DT' )
      ( scen = 'C1' col = 102  hdr = 'Retails Lic No (20 and 21 )' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' )
      ( scen = 'C1' col = 103  hdr = 'SC_EXEMPT' node = 'Z' fld = 'SC_EXEMPT' fmt = '' )
      ( scen = 'C1' col = 104  hdr = 'Retails Lic Exp date' node = 'Z' fld = 'RETAIL_EXP' fmt = 'DT' )
      ( scen = 'C1' col = 105  hdr = 'Mfg License (Gen) Number' node = 'Z' fld = 'MFGLIC1NO' fmt = '' )
      ( scen = 'C1' col = 106  hdr = 'Mfg License (Nar) Number' node = 'Z' fld = 'MFGLIC2NO' fmt = '' )
      ( scen = 'C1' col = 107  hdr = 'Mfg License (CC) Number' node = 'Z' fld = 'MFGLIC3NO' fmt = '' )
      ( scen = 'C1' col = 108  hdr = 'Bank Guarantee(Y/N)' node = 'Z' fld = 'BGYN' fmt = '' )
      ( scen = 'C1' col = 109  hdr = 'Bank Guarantee No' node = 'Z' fld = 'BG_NO' fmt = '' )
      ( scen = 'C1' col = 110  hdr = 'BG Amount' node = 'Z' fld = 'BG_AMT' fmt = 'NM' )
      ( scen = 'C1' col = 111  hdr = 'SD Document Currency' node = 'Z' fld = 'CURRENCY' fmt = '' )
      ( scen = 'C1' col = 112  hdr = 'BG Issue Date' node = 'Z' fld = 'BG_ISS_DT' fmt = 'DT' )
      ( scen = 'C1' col = 113  hdr = 'BG Expiry Date' node = 'Z' fld = 'BG_EXP_DT' fmt = 'DT' )
      ( scen = 'C1' col = 114  hdr = 'BG Issuing Bank' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' )
      ( scen = 'C1' col = 115  hdr = 'Agreement Expiry Date' node = 'Z' fld = 'AGGR_EXPDT' fmt = 'DT' )
      ( scen = 'C1' col = 116  hdr = 'Appointment Date' node = 'Z' fld = 'APPOINT_DT' fmt = 'DT' )
      ( scen = 'C1' col = 117  hdr = 'Customer group' node = 'Z' fld = 'KDGRP' fmt = '' )
      ( scen = 'C1' col = 118  hdr = 'AIOCD Code' node = 'Z' fld = 'AIOCD_CODE' fmt = '' )
      ( scen = 'C1' col = 119  hdr = 'Customer Bank Name' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' )
      ( scen = 'C1' col = 120  hdr = 'Destination of Booking' node = 'Z' fld = 'DST_BOOKING' fmt = '' )
      ( scen = 'C1' col = 121  hdr = 'Route Code' node = 'Z' fld = 'ZTROUT' fmt = '' )
      ( scen = 'C1' col = 122  hdr = 'Extension' node = 'Z' fld = 'EXTENSION' fmt = '' )
      ( scen = 'C1' col = 123  hdr = 'Route' node = 'Z' fld = 'ZCROUT' fmt = '' )
      ( scen = 'C1' col = 124  hdr = 'GLN URI Format' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' )
      ( scen = 'C1' col = 125  hdr = 'DUNS_Number' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' )
      ( scen = 'C1' col = 126  hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = 'DT' )
      ( scen = 'C1' col = 127  hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = 'DT' )
      ( scen = 'C1' col = 128  hdr = 'State From Date' node = 'Z' fld = 'STATE_FROM_DATE' fmt = 'DT' )
      ( scen = 'C1' col = 129  hdr = 'State To Date' node = 'Z' fld = 'STATE_TO_DATE' fmt = 'DT' )
      ( scen = 'C1' col = 130  hdr = 'Import_License/MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' )
      ( scen = 'C1' col = 131  hdr = 'IMPL/MIA_From_Date' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = 'DT' )
      ( scen = 'C1' col = 132  hdr = 'IMPL/MIA_Valid_Date' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = 'DT' )
      ( scen = 'C1' col = 133  hdr = 'Check Digit' node = 'Z' fld = 'CHECK_DIGIT' fmt = '' )
      ( scen = 'C1' col = 134  hdr = 'Global Company Prefix' node = 'Z' fld = 'GLOBAL_COM' fmt = '' )
      ( scen = 'C1' col = 135  hdr = 'Backorder Days' node = 'Z' fld = 'BO_DAYS' fmt = '' )
      ( scen = 'C1' col = 136  hdr = 'Location Number' node = 'Z' fld = 'LOCATION_NUMBER' fmt = '' )
      ( scen = 'C2' col = 1    hdr = 'ID' node = '-' fld = '' fmt = '' )
      ( scen = 'C2' col = 2    hdr = 'BUKRS' node = 'K' fld = 'BUKRS' fmt = '' )
      ( scen = 'C2' col = 3    hdr = 'VKORG' node = 'K' fld = 'VKORG' fmt = '' )
      ( scen = 'C2' col = 4    hdr = 'VTWEG' node = 'K' fld = 'VTWEG' fmt = '' )
      ( scen = 'C2' col = 5    hdr = 'SPART' node = 'K' fld = 'SPART' fmt = '' )
      ( scen = 'C2' col = 6    hdr = 'KTOKD' node = 'K' fld = 'KTOKD' fmt = '' )
      ( scen = 'C2' col = 7    hdr = 'ADDRESSMGTM' node = '-' fld = '' fmt = '' )
      ( scen = 'C2' col = 8    hdr = 'TITLE_MEDI' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( scen = 'C2' col = 9    hdr = 'NAME1' node = 'A' fld = 'NAME' fmt = '' )
      ( scen = 'C2' col = 10   hdr = 'NAME2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( scen = 'C2' col = 11   hdr = 'NAME3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( scen = 'C2' col = 12   hdr = 'NAME4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( scen = 'C2' col = 13   hdr = 'SORT1' node = 'A' fld = 'SORT1' fmt = '' )
      ( scen = 'C2' col = 14   hdr = 'SORT2' node = 'A' fld = 'SORT2' fmt = '' )
      ( scen = 'C2' col = 15   hdr = 'NAME_CO' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( scen = 'C2' col = 16   hdr = 'STR_SUPPL1' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( scen = 'C2' col = 17   hdr = 'STR_SUPPL2' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( scen = 'C2' col = 18   hdr = 'STREET' node = 'A' fld = 'STREET' fmt = '' )
      ( scen = 'C2' col = 19   hdr = 'STR_SUPPL3' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( scen = 'C2' col = 20   hdr = 'LOCATION' node = 'A' fld = 'LOCATION' fmt = '' )
      ( scen = 'C2' col = 21   hdr = 'CITY2' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( scen = 'C2' col = 22   hdr = 'POST_CODE1' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( scen = 'C2' col = 23   hdr = 'CITY1' node = 'A' fld = 'CITY' fmt = '' )
      ( scen = 'C2' col = 24   hdr = 'COUNTRY' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( scen = 'C2' col = 25   hdr = 'REGION' node = 'A' fld = 'REGION' fmt = '' )
      ( scen = 'C2' col = 26   hdr = 'LANGU' node = 'A' fld = 'LANGU' fmt = '' )
      ( scen = 'C2' col = 27   hdr = 'TEL_NUMBER' node = 'M' fld = 'TEL' fmt = '' )
      ( scen = 'C2' col = 28   hdr = 'MOB_NUMBER' node = 'M' fld = 'MOB' fmt = '' )
      ( scen = 'C2' col = 29   hdr = 'FAX_NUMBER' node = 'M' fld = 'FAX' fmt = '' )
      ( scen = 'C2' col = 30   hdr = 'SMTP_ADDR' node = 'M' fld = 'SMT' fmt = '' )
      ( scen = 'C2' col = 31   hdr = 'DEFLT_COMM' node = '-' fld = '' fmt = '' )
      ( scen = 'C2' col = 32   hdr = 'LIFNR' node = 'C' fld = 'LIFNR' fmt = 'AL' )
      ( scen = 'C2' col = 33   hdr = 'KUKLA' node = 'C' fld = 'KUKLA' fmt = '' )
      ( scen = 'C2' col = 34   hdr = 'UMSA1' node = 'C' fld = 'UMSA1' fmt = '' )
      ( scen = 'C2' col = 35   hdr = 'UWAER' node = 'C' fld = 'UWAER' fmt = '' )
      ( scen = 'C2' col = 36   hdr = 'UMJAH' node = 'C' fld = 'UMJAH' fmt = '' )
      ( scen = 'C2' col = 37   hdr = 'AKONT' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( scen = 'C2' col = 38   hdr = 'ZUAWA' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( scen = 'C2' col = 39   hdr = 'XZVER' node = 'B' fld = 'XZVER' fmt = '' )
      ( scen = 'C2' col = 40   hdr = 'BZIRK' node = 'S' fld = 'BZIRK' fmt = '' )
      ( scen = 'C2' col = 41   hdr = 'AWAHR' node = 'S' fld = 'AWAHR' fmt = '' )
      ( scen = 'C2' col = 42   hdr = 'VKBUR' node = 'S' fld = 'VKBUR' fmt = '' )
      ( scen = 'C2' col = 43   hdr = 'VKGRP' node = 'S' fld = 'VKGRP' fmt = '' )
      ( scen = 'C2' col = 44   hdr = 'KDGRP' node = 'S' fld = 'KDGRP' fmt = '' )
      ( scen = 'C2' col = 45   hdr = 'KLABC' node = 'S' fld = 'KLABC' fmt = '' )
      ( scen = 'C2' col = 46   hdr = 'WAERS' node = 'S' fld = 'WAERS' fmt = '' )
      ( scen = 'C2' col = 47   hdr = 'KURST' node = 'S' fld = 'KURST' fmt = '' )
      ( scen = 'C2' col = 48   hdr = 'KALKS' node = 'S' fld = 'KALKS' fmt = '' )
      ( scen = 'C2' col = 49   hdr = 'VERSG' node = 'S' fld = 'VERSG' fmt = '' )
      ( scen = 'C2' col = 50   hdr = 'LPRIO' node = 'S' fld = 'LPRIO' fmt = '' )
      ( scen = 'C2' col = 51   hdr = 'KZAZU' node = 'S' fld = 'KZAZU' fmt = '' )
      ( scen = 'C2' col = 52   hdr = 'VSBED' node = 'S' fld = 'VSBED' fmt = '' )
      ( scen = 'C2' col = 53   hdr = 'VWERK' node = 'S' fld = 'VWERK' fmt = '' )
      ( scen = 'C2' col = 54   hdr = 'ANTLF' node = 'S' fld = 'ANTLF' fmt = '' )
      ( scen = 'C2' col = 55   hdr = 'INCO1' node = 'S' fld = 'INCO1' fmt = '' )
      ( scen = 'C2' col = 56   hdr = 'INCO2' node = 'S' fld = 'INCO2' fmt = '' )
      ( scen = 'C2' col = 57   hdr = 'ZTERM' node = 'S' fld = 'ZTERM' fmt = '' )
      ( scen = 'C2' col = 58   hdr = 'KTGRD' node = 'S' fld = 'KTGRD' fmt = '' )
      ( scen = 'C2' col = 59   hdr = 'TAXKD_01' node = 'T' fld = '#1' fmt = '' )
      ( scen = 'C2' col = 60   hdr = 'TAXKD_02' node = 'T' fld = '#2' fmt = '' )
      ( scen = 'C2' col = 61   hdr = 'TAXKD_03' node = 'T' fld = '#3' fmt = '' )
      ( scen = 'C2' col = 62   hdr = 'TAXKD_04' node = 'T' fld = '#4' fmt = '' )
      ( scen = 'C2' col = 63   hdr = 'KVGR1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( scen = 'C2' col = 64   hdr = 'KVGR3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( scen = 'C2' col = 65   hdr = 'KVGR4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( scen = 'C2' col = 66   hdr = 'KVGR5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( scen = 'C2' col = 67   hdr = 'PAN No' node = 'C' fld = 'J_1IPANNO' fmt = '' )
      ( scen = 'C3' col = 2    hdr = 'TCODE' node = '-' fld = '' fmt = '' )
      ( scen = 'C3' col = 1    hdr = 'TABNAME' node = '-' fld = '' fmt = '' )
      ( scen = 'C3' col = 3    hdr = 'BUKRS' node = 'K' fld = 'BUKRS' fmt = '' )
      ( scen = 'C3' col = 4    hdr = 'VKORG' node = 'K' fld = 'VKORG' fmt = '' )
      ( scen = 'C3' col = 5    hdr = 'VTWEG' node = 'K' fld = 'VTWEG' fmt = '' )
      ( scen = 'C3' col = 6    hdr = 'SPART' node = 'K' fld = 'SPART' fmt = '' )
      ( scen = 'C3' col = 7    hdr = 'KTOKD' node = 'K' fld = 'KTOKD' fmt = '' )
      ( scen = 'C3' col = 8    hdr = 'USE_ZAV' node = '-' fld = '' fmt = '' )
      ( scen = 'C3' col = 9    hdr = 'TITLE_MEDI' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( scen = 'C3' col = 10   hdr = 'NAME1' node = 'A' fld = 'NAME' fmt = '' )
      ( scen = 'C3' col = 11   hdr = 'NAME2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( scen = 'C3' col = 12   hdr = 'NAME3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( scen = 'C3' col = 13   hdr = 'NAME4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( scen = 'C3' col = 14   hdr = 'SORT1' node = 'A' fld = 'SORT1' fmt = '' )
      ( scen = 'C3' col = 15   hdr = 'SORT2' node = 'A' fld = 'SORT2' fmt = '' )
      ( scen = 'C3' col = 16   hdr = 'NAME_CO' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( scen = 'C3' col = 17   hdr = 'STR_SUPPL1' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( scen = 'C3' col = 18   hdr = 'STR_SUPPL2' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( scen = 'C3' col = 19   hdr = 'STREET' node = 'A' fld = 'STREET' fmt = '' )
      ( scen = 'C3' col = 20   hdr = 'STR_SUPPL3' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( scen = 'C3' col = 21   hdr = 'LOCATION' node = 'A' fld = 'LOCATION' fmt = '' )
      ( scen = 'C3' col = 22   hdr = 'CITY2' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( scen = 'C3' col = 23   hdr = 'POST_CODE1' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( scen = 'C3' col = 24   hdr = 'CITY1' node = 'A' fld = 'CITY' fmt = '' )
      ( scen = 'C3' col = 25   hdr = 'COUNTRY' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( scen = 'C3' col = 26   hdr = 'REGION' node = 'A' fld = 'REGION' fmt = '' )
      ( scen = 'C3' col = 27   hdr = 'LANGU' node = 'A' fld = 'LANGU' fmt = '' )
      ( scen = 'C3' col = 28   hdr = 'TEL_NUMBER' node = 'M' fld = 'TEL' fmt = '' )
      ( scen = 'C3' col = 29   hdr = 'MOB_NUMBER' node = 'M' fld = 'MOB' fmt = '' )
      ( scen = 'C3' col = 30   hdr = 'FAX_NUMBER' node = 'M' fld = 'FAX' fmt = '' )
      ( scen = 'C3' col = 31   hdr = 'SMTP_ADDR' node = 'M' fld = 'SMT' fmt = '' )
      ( scen = 'C3' col = 32   hdr = 'KATR3' node = 'C' fld = 'KATR3' fmt = '' )
      ( scen = 'C3' col = 33   hdr = 'TIME_ZONE' node = 'A' fld = 'TIME_ZONE' fmt = '' )
      ( scen = 'C3' col = 34   hdr = 'J_1IPANNO' node = 'C' fld = 'J_1IPANNO' fmt = '' )
      ( scen = 'C3' col = 35   hdr = 'STCD3' node = 'C' fld = 'STCD3' fmt = '' )
      ( scen = 'C3' col = 36   hdr = 'AKONT' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( scen = 'C3' col = 37   hdr = 'ZUAWA' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( scen = 'C3' col = 38   hdr = 'FDGRV' node = 'B' fld = 'FDGRV' fmt = 'AL' )
      ( scen = 'C3' col = 39   hdr = 'VZSKZ' node = 'B' fld = 'VZSKZ' fmt = '' )
      ( scen = 'C3' col = 40   hdr = 'ZINRT' node = 'B' fld = 'ZINRT' fmt = '' )
      ( scen = 'C3' col = 41   hdr = 'ZTERM' node = 'B' fld = 'ZTERM' fmt = '' )
      ( scen = 'C3' col = 42   hdr = 'XZVER' node = 'B' fld = 'XZVER' fmt = '' )
      ( scen = 'C3' col = 43   hdr = 'ZWELS' node = 'B' fld = 'ZWELS' fmt = '' )
      ( scen = 'C3' col = 44   hdr = 'BZIRK' node = 'S' fld = 'BZIRK' fmt = '' )
      ( scen = 'C3' col = 45   hdr = 'VKBUR' node = 'S' fld = 'VKBUR' fmt = '' )
      ( scen = 'C3' col = 46   hdr = 'VKGRP' node = 'S' fld = 'VKGRP' fmt = '' )
      ( scen = 'C3' col = 47   hdr = 'KDGRP' node = 'S' fld = 'KDGRP' fmt = '' )
      ( scen = 'C3' col = 48   hdr = 'KLABC' node = 'S' fld = 'KLABC' fmt = '' )
      ( scen = 'C3' col = 49   hdr = 'WAERS' node = 'S' fld = 'WAERS' fmt = '' )
      ( scen = 'C3' col = 50   hdr = 'KONDA' node = 'S' fld = 'KONDA' fmt = '' )
      ( scen = 'C3' col = 51   hdr = 'KALKS' node = 'S' fld = 'KALKS' fmt = '' )
      ( scen = 'C3' col = 52   hdr = 'VERSG' node = 'S' fld = 'VERSG' fmt = '' )
      ( scen = 'C3' col = 53   hdr = 'LPRIO' node = 'S' fld = 'LPRIO' fmt = '' )
      ( scen = 'C3' col = 54   hdr = 'KZAZU' node = 'S' fld = 'KZAZU' fmt = '' )
      ( scen = 'C3' col = 55   hdr = 'VSBED' node = 'S' fld = 'VSBED' fmt = '' )
      ( scen = 'C3' col = 56   hdr = 'VWERK' node = 'S' fld = 'VWERK' fmt = '' )
      ( scen = 'C3' col = 57   hdr = 'ANTLF' node = 'S' fld = 'ANTLF' fmt = '' )
      ( scen = 'C3' col = 58   hdr = 'ZTERM1' node = 'S' fld = 'ZTERM' fmt = '' )
      ( scen = 'C3' col = 59   hdr = 'KTGRD' node = 'S' fld = 'KTGRD' fmt = '' )
      ( scen = 'C3' col = 60   hdr = 'TAXKD_01' node = 'T' fld = '#1' fmt = '' )
      ( scen = 'C3' col = 64   hdr = 'TAXKD_05' node = '-' fld = '' fmt = '' )
      ( scen = 'C3' col = 63   hdr = 'TAXKD_04' node = '-' fld = '' fmt = '' )
      ( scen = 'C3' col = 62   hdr = 'TAXKD_03' node = '-' fld = '' fmt = '' )
      ( scen = 'C3' col = 61   hdr = 'TAXKD_02' node = '-' fld = '' fmt = '' )
      ( scen = 'C3' col = 65   hdr = 'KVGR1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( scen = 'C3' col = 66   hdr = 'KVGR2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( scen = 'C3' col = 67   hdr = 'KVGR3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( scen = 'C3' col = 68   hdr = 'KVGR4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( scen = 'C3' col = 69   hdr = 'KVGR5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( scen = 'C3' col = 70   hdr = 'WERKS' node = 'Z' fld = 'WERKS' fmt = '' )
      ( scen = 'C3' col = 71   hdr = 'CUST_TRNST_DAYS' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = 'NM' )
      ( scen = 'C3' col = 72   hdr = 'KMSUM' node = 'Z' fld = 'KMSUM' fmt = 'NM' )
      ( scen = 'C3' col = 73   hdr = 'DRUGLICENSE1' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( scen = 'C3' col = 74   hdr = 'DRUGLICENSE2' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( scen = 'C3' col = 75   hdr = 'DL1_DL2_VALIDDT' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = 'DT' )
      ( scen = 'C3' col = 76   hdr = 'FOODSLICENSE' node = 'Z' fld = 'FOODSLICENSE' fmt = '' )
      ( scen = 'C3' col = 77   hdr = 'FL_VALIDDT' node = 'Z' fld = 'FL_VALIDDT' fmt = 'DT' )
      ( scen = 'C3' col = 78   hdr = 'SCHXNO' node = 'Z' fld = 'SCHXNO' fmt = '' )
      ( scen = 'C3' col = 79   hdr = 'SCHX_VALIDDT' node = 'Z' fld = 'SCHX_VALIDDT' fmt = 'DT' )
      ( scen = 'C3' col = 80   hdr = 'SCHXRNO' node = 'Z' fld = 'SCHXRNO' fmt = '' )
      ( scen = 'C3' col = 81   hdr = 'SCHXR_VALIDDT' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = 'DT' )
      ( scen = 'C3' col = 82   hdr = 'RETAIL_LIC_NO' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' )
      ( scen = 'C3' col = 83   hdr = 'RETAIL_EXP' node = 'Z' fld = 'RETAIL_EXP' fmt = 'DT' )
      ( scen = 'C3' col = 84   hdr = 'MFGLIC1NO' node = 'Z' fld = 'MFGLIC1NO' fmt = '' )
      ( scen = 'C3' col = 85   hdr = 'MFGLIC2NO' node = 'Z' fld = 'MFGLIC2NO' fmt = '' )
      ( scen = 'C3' col = 86   hdr = 'MFGLIC3NO' node = 'Z' fld = 'MFGLIC3NO' fmt = '' )
      ( scen = 'C3' col = 87   hdr = 'BGYN' node = 'Z' fld = 'BGYN' fmt = '' )
      ( scen = 'C3' col = 88   hdr = 'BG_NO' node = 'Z' fld = 'BG_NO' fmt = '' )
      ( scen = 'C3' col = 89   hdr = 'BG_AMT' node = 'Z' fld = 'BG_AMT' fmt = 'NM' )
      ( scen = 'C3' col = 90   hdr = 'CURRENCY' node = 'Z' fld = 'CURRENCY' fmt = '' )
      ( scen = 'C3' col = 91   hdr = 'BG_ISS_DT' node = 'Z' fld = 'BG_ISS_DT' fmt = 'DT' )
      ( scen = 'C3' col = 92   hdr = 'BG_EXP_DT' node = 'Z' fld = 'BG_EXP_DT' fmt = 'DT' )
      ( scen = 'C3' col = 93   hdr = 'BG_ISS_BANK' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' )
      ( scen = 'C3' col = 94   hdr = 'AGGR_EXPDT' node = 'Z' fld = 'AGGR_EXPDT' fmt = 'DT' )
      ( scen = 'C3' col = 95   hdr = 'APPOINT_DT' node = 'Z' fld = 'APPOINT_DT' fmt = 'DT' )
      ( scen = 'C3' col = 96   hdr = 'KDGRP1' node = 'Z' fld = 'KDGRP' fmt = '' )
      ( scen = 'C3' col = 97   hdr = 'AIOCD_CODE' node = 'Z' fld = 'AIOCD_CODE' fmt = '' )
      ( scen = 'C3' col = 98   hdr = 'CUST_BNK_NAME' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' )
      ( scen = 'C3' col = 99   hdr = 'DST_BOOKING' node = 'Z' fld = 'DST_BOOKING' fmt = '' )
      ( scen = 'C3' col = 100  hdr = 'ZTROUT' node = 'Z' fld = 'ZTROUT' fmt = '' )
      ( scen = 'C3' col = 101  hdr = 'EXTENSION' node = 'Z' fld = 'EXTENSION' fmt = '' )
      ( scen = 'C3' col = 102  hdr = 'ZCROUT' node = 'Z' fld = 'ZCROUT' fmt = '' )
      ( scen = 'C3' col = 103  hdr = 'GLN_URI_FORMAT' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' )
      ( scen = 'C3' col = 104  hdr = 'DUNS_NUMBER' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' )
      ( scen = 'C3' col = 105  hdr = 'DEA_FROM_DATE' node = 'Z' fld = 'DEA_FROM_DATE' fmt = 'DT' )
      ( scen = 'C3' col = 106  hdr = 'DEA_TO_DATE' node = 'Z' fld = 'DEA_TO_DATE' fmt = 'DT' )
      ( scen = 'C3' col = 107  hdr = 'ZIMP_LIC_MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' )
      ( scen = 'C3' col = 108  hdr = 'STATE_FROM_DATE' node = 'Z' fld = 'STATE_FROM_DATE' fmt = 'DT' )
      ( scen = 'C3' col = 109  hdr = 'STATE_TO_DATE' node = 'Z' fld = 'STATE_TO_DATE' fmt = 'DT' )
      ( scen = 'C3' col = 110  hdr = 'ZIMP_FROMDT_MIA' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = 'DT' )
      ( scen = 'C3' col = 111  hdr = 'ZIMP_VALIDDT_MIA' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = 'DT' )
      ( scen = 'C4' col = 3    hdr = 'TABNAME' node = '-' fld = '' fmt = '' )
      ( scen = 'C4' col = 2    hdr = 'TCODE' node = '-' fld = '' fmt = '' )
      ( scen = 'C4' col = 1    hdr = 'INSTRUCTIONS' node = '-' fld = '' fmt = '' )
      ( scen = 'C4' col = 4    hdr = 'BUKRS' node = 'K' fld = 'BUKRS' fmt = '' )
      ( scen = 'C4' col = 5    hdr = 'VKORG' node = 'K' fld = 'VKORG' fmt = '' )
      ( scen = 'C4' col = 6    hdr = 'VTWEG' node = 'K' fld = 'VTWEG' fmt = '' )
      ( scen = 'C4' col = 7    hdr = 'SPART' node = 'K' fld = 'SPART' fmt = '' )
      ( scen = 'C4' col = 8    hdr = 'KTOKD' node = 'K' fld = 'KTOKD' fmt = '' )
      ( scen = 'C4' col = 9    hdr = 'USE_ZAV' node = '-' fld = '' fmt = '' )
      ( scen = 'C4' col = 10   hdr = 'TITLE_MEDI' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( scen = 'C4' col = 11   hdr = 'NAME1' node = 'A' fld = 'NAME' fmt = '' )
      ( scen = 'C4' col = 12   hdr = 'NAME2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( scen = 'C4' col = 13   hdr = 'NAME3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( scen = 'C4' col = 14   hdr = 'NAME4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( scen = 'C4' col = 15   hdr = 'SORT1' node = 'A' fld = 'SORT1' fmt = '' )
      ( scen = 'C4' col = 16   hdr = 'SORT2' node = 'A' fld = 'SORT2' fmt = '' )
      ( scen = 'C4' col = 17   hdr = 'NAME_CO' node = 'A' fld = 'C_O_NAME' fmt = '' )
      ( scen = 'C4' col = 18   hdr = 'STR_SUPPL1' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( scen = 'C4' col = 19   hdr = 'STR_SUPPL2' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( scen = 'C4' col = 20   hdr = 'STREET' node = 'A' fld = 'STREET' fmt = '' )
      ( scen = 'C4' col = 21   hdr = 'STR_SUPPL3' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( scen = 'C4' col = 22   hdr = 'LOCATION' node = 'A' fld = 'LOCATION' fmt = '' )
      ( scen = 'C4' col = 23   hdr = 'CITY2' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( scen = 'C4' col = 24   hdr = 'POST_CODE1' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( scen = 'C4' col = 25   hdr = 'CITY1' node = 'A' fld = 'CITY' fmt = '' )
      ( scen = 'C4' col = 26   hdr = 'COUNTRY' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( scen = 'C4' col = 27   hdr = 'REGION' node = 'A' fld = 'REGION' fmt = '' )
      ( scen = 'C4' col = 28   hdr = 'TIME_ZONE' node = 'A' fld = 'TIME_ZONE' fmt = '' )
      ( scen = 'C4' col = 29   hdr = 'LANGU' node = 'A' fld = 'LANGU' fmt = '' )
      ( scen = 'C4' col = 30   hdr = 'TEL_NUMBER' node = 'M' fld = 'TEL' fmt = '' )
      ( scen = 'C4' col = 31   hdr = 'MOB_NUMBER' node = 'M' fld = 'MOB' fmt = '' )
      ( scen = 'C4' col = 32   hdr = 'FAX_NUMBER' node = 'M' fld = 'FAX' fmt = '' )
      ( scen = 'C4' col = 33   hdr = 'SMTP_ADDR' node = 'M' fld = 'SMT' fmt = '' )
      ( scen = 'C4' col = 34   hdr = 'KATR3' node = 'C' fld = 'KATR3' fmt = '' )
      ( scen = 'C4' col = 35   hdr = 'KATR4' node = 'C' fld = 'KATR4' fmt = '' )
      ( scen = 'C4' col = 36   hdr = 'LIFNR' node = 'C' fld = 'LIFNR' fmt = 'AL' )
      ( scen = 'C4' col = 37   hdr = 'VBUND' node = 'C' fld = 'VBUND' fmt = 'AL' )
      ( scen = 'C4' col = 38   hdr = 'KONZS' node = 'C' fld = 'KONZS' fmt = '' )
      ( scen = 'C4' col = 39   hdr = 'STCD3' node = 'C' fld = 'STCD3' fmt = '' )
      ( scen = 'C4' col = 40   hdr = 'STCD4' node = 'C' fld = 'STCD4' fmt = '' )
      ( scen = 'C4' col = 41   hdr = 'STCD4' node = 'C' fld = 'STCD5' fmt = '' )
      ( scen = 'C4' col = 42   hdr = 'STCEG' node = 'C' fld = 'STCEG' fmt = '' )
      ( scen = 'C4' col = 43   hdr = 'J_1IPANNO' node = 'C' fld = 'J_1IPANNO' fmt = '' )
      " Column 44 is headed STCD3, which column 39 already is; the upload
      " program reads nothing from it until the customer confirms what it
      " is, so nothing is written into it here either.
      ( scen = 'C4' col = 44   hdr = 'STCD3' node = '-' fld = '' fmt = '' )
      ( scen = 'C4' col = 45   hdr = 'AKONT' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( scen = 'C4' col = 46   hdr = 'ZUAWA' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( scen = 'C4' col = 47   hdr = 'VZSKZ' node = 'B' fld = 'VZSKZ' fmt = '' )
      ( scen = 'C4' col = 48   hdr = 'ZINRT' node = 'B' fld = 'ZINRT' fmt = '' )
      ( scen = 'C4' col = 49   hdr = 'ZTERM' node = 'B' fld = 'ZTERM' fmt = '' )
      ( scen = 'C4' col = 50   hdr = 'XZVER' node = 'B' fld = 'XZVER' fmt = '' )
      ( scen = 'C4' col = 51   hdr = 'ZWELS' node = 'B' fld = 'ZWELS' fmt = '' )
      ( scen = 'C4' col = 52   hdr = 'BZIRK' node = 'S' fld = 'BZIRK' fmt = '' )
      ( scen = 'C4' col = 53   hdr = 'AWAHR' node = 'S' fld = 'AWAHR' fmt = '' )
      ( scen = 'C4' col = 54   hdr = 'VKBUR' node = 'S' fld = 'VKBUR' fmt = '' )
      ( scen = 'C4' col = 55   hdr = 'VKGRP' node = 'S' fld = 'VKGRP' fmt = '' )
      ( scen = 'C4' col = 56   hdr = 'KDGRP' node = 'S' fld = 'KDGRP' fmt = '' )
      ( scen = 'C4' col = 57   hdr = 'KLABC' node = 'S' fld = 'KLABC' fmt = '' )
      ( scen = 'C4' col = 58   hdr = 'WAERS' node = 'S' fld = 'WAERS' fmt = '' )
      ( scen = 'C4' col = 59   hdr = 'KURST' node = 'S' fld = 'KURST' fmt = '' )
      ( scen = 'C4' col = 60   hdr = 'KALKS' node = 'S' fld = 'KALKS' fmt = '' )
      ( scen = 'C4' col = 61   hdr = 'VERSG' node = 'S' fld = 'VERSG' fmt = '' )
      ( scen = 'C4' col = 62   hdr = 'LPRIO' node = 'S' fld = 'LPRIO' fmt = '' )
      ( scen = 'C4' col = 63   hdr = 'KZAZU' node = 'S' fld = 'KZAZU' fmt = '' )
      ( scen = 'C4' col = 64   hdr = 'VSBED' node = 'S' fld = 'VSBED' fmt = '' )
      ( scen = 'C4' col = 65   hdr = 'VWERK' node = 'S' fld = 'VWERK' fmt = '' )
      ( scen = 'C4' col = 66   hdr = 'ANTLF' node = 'S' fld = 'ANTLF' fmt = '' )
      ( scen = 'C4' col = 67   hdr = 'INCO1' node = 'S' fld = 'INCO1' fmt = '' )
      ( scen = 'C4' col = 68   hdr = 'INCO2' node = 'S' fld = 'INCO2' fmt = '' )
      ( scen = 'C4' col = 69   hdr = 'ZTERM1' node = 'S' fld = 'ZTERM' fmt = '' )
      ( scen = 'C4' col = 70   hdr = 'KTGRD' node = 'S' fld = 'KTGRD' fmt = '' )
      ( scen = 'C4' col = 71   hdr = 'TAXKD_01' node = 'T' fld = '#1' fmt = '' )
      ( scen = 'C4' col = 75   hdr = 'TAXKD_05' node = '-' fld = '' fmt = '' )
      ( scen = 'C4' col = 74   hdr = 'TAXKD_04' node = '-' fld = '' fmt = '' )
      ( scen = 'C4' col = 73   hdr = 'TAXKD_03' node = '-' fld = '' fmt = '' )
      ( scen = 'C4' col = 72   hdr = 'TAXKD_02' node = '-' fld = '' fmt = '' )
      ( scen = 'C4' col = 76   hdr = 'KVGR1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( scen = 'C4' col = 77   hdr = 'KVGR2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( scen = 'C4' col = 78   hdr = 'KVGR3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( scen = 'C4' col = 79   hdr = 'KVGR4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( scen = 'C4' col = 80   hdr = 'KVGR5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( scen = 'C4' col = 81   hdr = 'WERKS' node = 'Z' fld = 'WERKS' fmt = '' )
      ( scen = 'C4' col = 82   hdr = 'CUST_TRNST_DAYS' node = 'Z' fld = 'CUST_TRNST_DAYS' fmt = 'NM' )
      ( scen = 'C4' col = 83   hdr = 'KMSUM' node = 'Z' fld = 'KMSUM' fmt = 'NM' )
      ( scen = 'C4' col = 84   hdr = 'DRUGLICENSE1' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( scen = 'C4' col = 85   hdr = 'DRUGLICENSE2' node = 'Z' fld = 'DRUGLICENSE2' fmt = '' )
      ( scen = 'C4' col = 86   hdr = 'DL1_DL2_VALIDDT' node = 'Z' fld = 'DL1_DL2_VALIDDT' fmt = 'DT' )
      ( scen = 'C4' col = 87   hdr = 'FOODSLICENSE' node = 'Z' fld = 'FOODSLICENSE' fmt = '' )
      ( scen = 'C4' col = 88   hdr = 'FL_VALIDDT' node = 'Z' fld = 'FL_VALIDDT' fmt = 'DT' )
      ( scen = 'C4' col = 89   hdr = 'SCHXNO' node = 'Z' fld = 'SCHXNO' fmt = '' )
      ( scen = 'C4' col = 90   hdr = 'SCHX_VALIDDT' node = 'Z' fld = 'SCHX_VALIDDT' fmt = 'DT' )
      ( scen = 'C4' col = 91   hdr = 'SCHXRNO' node = 'Z' fld = 'SCHXRNO' fmt = '' )
      ( scen = 'C4' col = 92   hdr = 'SCHXR_VALIDDT' node = 'Z' fld = 'SCHXR_VALIDDT' fmt = 'DT' )
      ( scen = 'C4' col = 93   hdr = 'RETAIL_LIC_NO' node = 'Z' fld = 'RETAIL_LIC_NO' fmt = '' )
      ( scen = 'C4' col = 94   hdr = 'RETAIL_EXP' node = 'Z' fld = 'RETAIL_EXP' fmt = 'DT' )
      ( scen = 'C4' col = 95   hdr = 'MFGLIC1NO' node = 'Z' fld = 'MFGLIC1NO' fmt = '' )
      ( scen = 'C4' col = 96   hdr = 'MFGLIC2NO' node = 'Z' fld = 'MFGLIC2NO' fmt = '' )
      ( scen = 'C4' col = 97   hdr = 'MFGLIC3NO' node = 'Z' fld = 'MFGLIC3NO' fmt = '' )
      ( scen = 'C4' col = 98   hdr = 'BGYN' node = 'Z' fld = 'BGYN' fmt = '' )
      ( scen = 'C4' col = 99   hdr = 'BG_NO' node = 'Z' fld = 'BG_NO' fmt = '' )
      ( scen = 'C4' col = 100  hdr = 'BG_AMT' node = 'Z' fld = 'BG_AMT' fmt = 'NM' )
      ( scen = 'C4' col = 101  hdr = 'CURRENCY' node = 'Z' fld = 'CURRENCY' fmt = '' )
      ( scen = 'C4' col = 102  hdr = 'BG_ISS_DT' node = 'Z' fld = 'BG_ISS_DT' fmt = 'DT' )
      ( scen = 'C4' col = 103  hdr = 'BG_EXP_DT' node = 'Z' fld = 'BG_EXP_DT' fmt = 'DT' )
      ( scen = 'C4' col = 104  hdr = 'BG_ISS_BANK' node = 'Z' fld = 'BG_ISS_BANK' fmt = '' )
      ( scen = 'C4' col = 105  hdr = 'AGGR_EXPDT' node = 'Z' fld = 'AGGR_EXPDT' fmt = 'DT' )
      ( scen = 'C4' col = 106  hdr = 'APPOINT_DT' node = 'Z' fld = 'APPOINT_DT' fmt = 'DT' )
      ( scen = 'C4' col = 107  hdr = 'KDGRP1' node = 'Z' fld = 'KDGRP' fmt = '' )
      ( scen = 'C4' col = 108  hdr = 'AIOCD_CODE' node = 'Z' fld = 'AIOCD_CODE' fmt = '' )
      ( scen = 'C4' col = 109  hdr = 'CUST_BNK_NAME' node = 'Z' fld = 'CUST_BNK_NAME' fmt = '' )
      ( scen = 'C4' col = 110  hdr = 'DST_BOOKING' node = 'Z' fld = 'DST_BOOKING' fmt = '' )
      ( scen = 'C4' col = 111  hdr = 'ZTROUT' node = 'Z' fld = 'ZTROUT' fmt = '' )
      ( scen = 'C4' col = 112  hdr = 'EXTENSION' node = 'Z' fld = 'EXTENSION' fmt = '' )
      ( scen = 'C4' col = 113  hdr = 'ZCROUT' node = 'Z' fld = 'ZCROUT' fmt = '' )
      ( scen = 'C4' col = 114  hdr = 'GLN_URI_FORMAT' node = 'Z' fld = 'GLN_URI_FORMAT' fmt = '' )
      ( scen = 'C4' col = 115  hdr = 'DUNS_NUMBER' node = 'Z' fld = 'DUNS_NUMBER' fmt = '' )
      ( scen = 'C4' col = 116  hdr = 'DEA_FROM_DATE' node = 'Z' fld = 'DEA_FROM_DATE' fmt = 'DT' )
      ( scen = 'C4' col = 117  hdr = 'DEA_TO_DATE' node = 'Z' fld = 'DEA_TO_DATE' fmt = 'DT' )
      ( scen = 'C4' col = 118  hdr = 'ZIMP_LIC_MIA' node = 'Z' fld = 'ZIMP_LIC_MIA' fmt = '' )
      ( scen = 'C4' col = 119  hdr = 'STATE_FROM_DATE' node = 'Z' fld = 'STATE_FROM_DATE' fmt = 'DT' )
      ( scen = 'C4' col = 120  hdr = 'STATE_TO_DATE' node = 'Z' fld = 'STATE_TO_DATE' fmt = 'DT' )
      ( scen = 'C4' col = 121  hdr = 'ZIMP_FROMDT_MIA' node = 'Z' fld = 'ZIMP_FROMDT_MIA' fmt = 'DT' )
      ( scen = 'C4' col = 122  hdr = 'ZIMP_VALIDDT_MIA' node = 'Z' fld = 'ZIMP_VALIDDT_MIA' fmt = 'DT' )
      ( scen = 'C5' col = 1    hdr = 'Customer code' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( scen = 'C5' col = 2    hdr = 'KKBER' node = 'U' fld = 'SEGMENT' fmt = '' )
      ( scen = 'C5' col = 7    hdr = 'D0220' node = '-' fld = '' fmt = '' )
      ( scen = 'C5' col = 6    hdr = 'D0210' node = '-' fld = '' fmt = '' )
      ( scen = 'C5' col = 5    hdr = 'D0120' node = '-' fld = '' fmt = '' )
      ( scen = 'C5' col = 4    hdr = 'D0110' node = '-' fld = '' fmt = '' )
      ( scen = 'C5' col = 3    hdr = 'D0105' node = '-' fld = '' fmt = '' )
      ( scen = 'C5' col = 8    hdr = 'KLIMG' node = 'U' fld = 'LIMIT_MAIN' fmt = 'NM' )
      ( scen = 'C5' col = 9    hdr = 'KLIME' node = 'U' fld = 'LIMIT_SGM' fmt = 'NM' )
      ( scen = 'C5' col = 10   hdr = 'WAERS' node = 'U' fld = 'CURRENCY' fmt = '' )
      ( scen = 'C5' col = 11   hdr = 'KLIMK' node = 'U' fld = 'LIMIT_SGM' fmt = 'NM' )
      ( scen = 'C5' col = 12   hdr = 'CTLPC' node = 'U' fld = 'RISK_CLASS' fmt = '' )
      ( scen = 'C5' col = 13   hdr = 'CRBLB' node = 'U' fld = 'XBLOCKED' fmt = '' )
      ( scen = 'C5' col = 14   hdr = 'SBGRP' node = '-' fld = '' fmt = '' )
      ( scen = 'C5' col = 16   hdr = 'Payment Terms' node = 'B' fld = 'ZTERM' fmt = '' )
      ( scen = 'C5' col = 17   hdr = 'Cust Grp 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( scen = 'C5' col = 18   hdr = 'Z1 Interest Indicator (cycle -> ZINRT, see handler)' node = 'B' fld = 'VZSKZ' fmt = '' )
      ( scen = 'C6' col = 1    hdr = 'Customer code' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( scen = 'C6' col = 2    hdr = 'Transaction Code' node = '-' fld = '' fmt = '' )
      ( scen = 'C6' col = 3    hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( scen = 'C6' col = 4    hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( scen = 'C6' col = 5    hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( scen = 'C6' col = 6    hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( scen = 'C6' col = 7    hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( scen = 'C6' col = 8    hdr = 'Always X' node = '-' fld = '' fmt = '' )
      ( scen = 'C6' col = 9    hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( scen = 'C6' col = 10   hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( scen = 'C6' col = 11   hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( scen = 'C6' col = 12   hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( scen = 'C6' col = 13   hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( scen = 'C6' col = 14   hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( scen = 'C6' col = 15   hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( scen = 'C6' col = 16   hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( scen = 'C6' col = 17   hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( scen = 'C6' col = 18   hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( scen = 'C6' col = 19   hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( scen = 'C6' col = 20   hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( scen = 'C6' col = 21   hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( scen = 'C6' col = 22   hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( scen = 'C6' col = 23   hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( scen = 'C6' col = 24   hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( scen = 'C6' col = 25   hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( scen = 'C6' col = 26   hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( scen = 'C6' col = 27   hdr = 'First telephone no.: dialling cod' node = 'M' fld = 'TEL' fmt = '' )
      ( scen = 'C6' col = 28   hdr = 'First Mobile Telephone No.: Diali' node = 'M' fld = 'MOB' fmt = '' )
      ( scen = 'C6' col = 29   hdr = 'First fax no.: dialling code+numb' node = 'M' fld = 'FAX' fmt = '' )
      ( scen = 'C6' col = 30   hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( scen = 'C6' col = 31   hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' )
      ( scen = 'C6' col = 32   hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( scen = 'C6' col = 33   hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
      ( scen = 'C6' col = 34   hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' )
      ( scen = 'C6' col = 35   hdr = 'Reconciliation Account in General' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( scen = 'C6' col = 36   hdr = 'Key for sorting according to assi' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( scen = 'C6' col = 37   hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = 'AL' )
      ( scen = 'C6' col = 38   hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' )
      ( scen = 'C6' col = 39   hdr = 'Interest calculation frequency in' node = 'B' fld = 'ZINRT' fmt = '' )
      ( scen = 'C6' col = 40   hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( scen = 'C6' col = 41   hdr = 'Indicator: Record Payment History' node = 'B' fld = 'XZVER' fmt = '' )
      ( scen = 'C6' col = 42   hdr = 'List of the Payment Methods to be' node = 'B' fld = 'ZWELS' fmt = '' )
      ( scen = 'C6' col = 43   hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( scen = 'C6' col = 44   hdr = 'Order probability of the item' node = 'S' fld = 'AWAHR' fmt = '' )
      ( scen = 'C6' col = 45   hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( scen = 'C6' col = 46   hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( scen = 'C6' col = 47   hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( scen = 'C6' col = 48   hdr = 'Customer classification (ABC anal' node = 'S' fld = 'KLABC' fmt = '' )
      ( scen = 'C6' col = 49   hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( scen = 'C6' col = 50   hdr = 'Pricing procedure assigned to thi' node = 'S' fld = 'KALKS' fmt = '' )
      ( scen = 'C6' col = 51   hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( scen = 'C6' col = 52   hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( scen = 'C6' col = 53   hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( scen = 'C6' col = 54   hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( scen = 'C6' col = 55   hdr = 'Delivering Plant (Own or External' node = 'S' fld = 'VWERK' fmt = '' )
      ( scen = 'C6' col = 56   hdr = 'Maximum Number of Partial Deliver' node = 'S' fld = 'ANTLF' fmt = '' )
      ( scen = 'C6' col = 57   hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( scen = 'C6' col = 58   hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( scen = 'C6' col = 59   hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( scen = 'C6' col = 60   hdr = 'Customer Account Assignment Group' node = 'S' fld = 'KTGRD' fmt = '' )
      ( scen = 'C6' col = 61   hdr = 'Tax classification for customer' node = 'T' fld = 'UTXJ' fmt = '' )
      ( scen = 'C6' col = 62   hdr = 'Tax classification for customer' node = 'T' fld = 'UTX2' fmt = '' )
      ( scen = 'C6' col = 63   hdr = 'Tax classification for customer' node = 'T' fld = 'UTX3' fmt = '' )
      ( scen = 'C6' col = 64   hdr = 'Tax classification for customer' node = 'T' fld = 'MWST' fmt = '' )
      ( scen = 'C6' col = 65   hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( scen = 'C6' col = 66   hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( scen = 'C6' col = 67   hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( scen = 'C6' col = 68   hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( scen = 'C6' col = 69   hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( scen = 'C6' col = 70   hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( scen = 'C6' col = 71   hdr = '20B. Lic. No' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( scen = 'C6' col = 72   hdr = '20B Issue Date' node = 'Z' fld = 'DL1_ISSUEDT' fmt = 'DT' )
      ( scen = 'C6' col = 73   hdr = '20B Expiry Date' node = 'Z' fld = 'DL1_VALIDDT' fmt = 'DT' )
      ( scen = 'C6' col = 74   hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = 'DT' )
      ( scen = 'C6' col = 75   hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = 'DT' )
      ( scen = 'C7' col = 1    hdr = 'Customer code' node = 'K' fld = 'KUNNR' fmt = 'AL' )
      ( scen = 'C7' col = 2    hdr = 'Transaction Code' node = '-' fld = '' fmt = '' )
      ( scen = 'C7' col = 3    hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( scen = 'C7' col = 4    hdr = 'Sales Organization' node = 'K' fld = 'VKORG' fmt = '' )
      ( scen = 'C7' col = 5    hdr = 'Distribution Channel' node = 'K' fld = 'VTWEG' fmt = '' )
      ( scen = 'C7' col = 6    hdr = 'Division' node = 'K' fld = 'SPART' fmt = '' )
      ( scen = 'C7' col = 7    hdr = 'Customer Account Group' node = 'K' fld = 'KTOKD' fmt = '' )
      ( scen = 'C7' col = 8    hdr = 'Always X' node = '-' fld = '' fmt = '' )
      ( scen = 'C7' col = 9    hdr = 'Title text' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( scen = 'C7' col = 10   hdr = 'Name 1' node = 'A' fld = 'NAME' fmt = '' )
      ( scen = 'C7' col = 11   hdr = 'Name 2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( scen = 'C7' col = 12   hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( scen = 'C7' col = 13   hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( scen = 'C7' col = 14   hdr = 'Search Term 1' node = 'A' fld = 'SORT1' fmt = '' )
      ( scen = 'C7' col = 15   hdr = 'Search Term 2' node = 'A' fld = 'SORT2' fmt = '' )
      ( scen = 'C7' col = 16   hdr = 'Street 2' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( scen = 'C7' col = 17   hdr = 'Street 3' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( scen = 'C7' col = 18   hdr = 'Street' node = 'A' fld = 'STREET' fmt = '' )
      ( scen = 'C7' col = 19   hdr = 'Street 4' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( scen = 'C7' col = 20   hdr = 'Street 5' node = 'A' fld = 'LOCATION' fmt = '' )
      ( scen = 'C7' col = 21   hdr = 'District' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( scen = 'C7' col = 22   hdr = 'City postal code' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( scen = 'C7' col = 23   hdr = 'City' node = 'A' fld = 'CITY' fmt = '' )
      ( scen = 'C7' col = 24   hdr = 'Country Key' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( scen = 'C7' col = 25   hdr = 'Region (State, Province, County)' node = 'A' fld = 'REGION' fmt = '' )
      ( scen = 'C7' col = 26   hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' )
      ( scen = 'C7' col = 27   hdr = 'First telephone no.: dialling cod' node = 'M' fld = 'TEL' fmt = '' )
      ( scen = 'C7' col = 28   hdr = 'First Mobile Telephone No.: Diali' node = 'M' fld = 'MOB' fmt = '' )
      ( scen = 'C7' col = 29   hdr = 'First fax no.: dialling code+numb' node = 'M' fld = 'FAX' fmt = '' )
      ( scen = 'C7' col = 30   hdr = 'E-Mail Address' node = 'M' fld = 'SMT' fmt = '' )
      ( scen = 'C7' col = 31   hdr = 'Group key' node = 'C' fld = 'KONZS' fmt = '' )
      ( scen = 'C7' col = 32   hdr = 'Attribute 3' node = 'C' fld = 'KATR3' fmt = '' )
      ( scen = 'C7' col = 33   hdr = 'Attribute 4' node = 'C' fld = 'KATR4' fmt = '' )
      ( scen = 'C7' col = 34   hdr = 'ID for mainly non-military use' node = 'C' fld = 'CIVVE' fmt = '' )
      ( scen = 'C7' col = 35   hdr = 'Reconciliation Account in General' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( scen = 'C7' col = 36   hdr = 'Key for sorting according to assi' node = 'B' fld = 'ZUAWA' fmt = '' )
      ( scen = 'C7' col = 37   hdr = 'Planning group' node = 'B' fld = 'FDGRV' fmt = 'AL' )
      ( scen = 'C7' col = 38   hdr = 'Interest calculation indicator' node = 'B' fld = 'VZSKZ' fmt = '' )
      ( scen = 'C7' col = 39   hdr = 'Interest calculation frequency in' node = 'B' fld = 'ZINRT' fmt = '' )
      ( scen = 'C7' col = 40   hdr = 'Terms of Payment Key' node = 'B' fld = 'ZTERM' fmt = '' )
      ( scen = 'C7' col = 41   hdr = 'Indicator: Record Payment History' node = 'B' fld = 'XZVER' fmt = '' )
      ( scen = 'C7' col = 42   hdr = 'List of the Payment Methods to be' node = 'B' fld = 'ZWELS' fmt = '' )
      ( scen = 'C7' col = 43   hdr = 'Sales district' node = 'S' fld = 'BZIRK' fmt = '' )
      ( scen = 'C7' col = 44   hdr = 'Order probability of the item' node = 'S' fld = 'AWAHR' fmt = '' )
      ( scen = 'C7' col = 45   hdr = 'Sales Office' node = 'S' fld = 'VKBUR' fmt = '' )
      ( scen = 'C7' col = 46   hdr = 'Sales Group' node = 'S' fld = 'VKGRP' fmt = '' )
      ( scen = 'C7' col = 47   hdr = 'Customer group' node = 'S' fld = 'KDGRP' fmt = '' )
      ( scen = 'C7' col = 48   hdr = 'Customer classification (ABC anal' node = 'S' fld = 'KLABC' fmt = '' )
      ( scen = 'C7' col = 49   hdr = 'Currency' node = 'S' fld = 'WAERS' fmt = '' )
      ( scen = 'C7' col = 50   hdr = 'Pricing procedure assigned to thi' node = 'S' fld = 'KALKS' fmt = '' )
      ( scen = 'C7' col = 51   hdr = 'Customer Statistics Group' node = 'S' fld = 'VERSG' fmt = '' )
      ( scen = 'C7' col = 52   hdr = 'Delivery Priority' node = 'S' fld = 'LPRIO' fmt = '' )
      ( scen = 'C7' col = 53   hdr = 'Order Combination Indicator' node = 'S' fld = 'KZAZU' fmt = '' )
      ( scen = 'C7' col = 54   hdr = 'Shipping Conditions' node = 'S' fld = 'VSBED' fmt = '' )
      ( scen = 'C7' col = 55   hdr = 'Delivering Plant (Own or External' node = 'S' fld = 'VWERK' fmt = '' )
      ( scen = 'C7' col = 56   hdr = 'Maximum Number of Partial Deliver' node = 'S' fld = 'ANTLF' fmt = '' )
      ( scen = 'C7' col = 57   hdr = 'Incoterms (Part 1)' node = 'S' fld = 'INCO1' fmt = '' )
      ( scen = 'C7' col = 58   hdr = 'Incoterms (Part 2)' node = 'S' fld = 'INCO2' fmt = '' )
      ( scen = 'C7' col = 59   hdr = 'Terms of Payment Key' node = 'S' fld = 'ZTERM' fmt = '' )
      ( scen = 'C7' col = 60   hdr = 'Customer Account Assignment Group' node = 'S' fld = 'KTGRD' fmt = '' )
      ( scen = 'C7' col = 61   hdr = 'Tax classification for customer' node = 'T' fld = 'UTXJ' fmt = '' )
      ( scen = 'C7' col = 62   hdr = 'Tax classification for customer' node = 'T' fld = 'UTX2' fmt = '' )
      ( scen = 'C7' col = 63   hdr = 'Tax classification for customer' node = 'T' fld = 'UTX3' fmt = '' )
      ( scen = 'C7' col = 64   hdr = 'Tax classification for customer' node = 'T' fld = 'MWST' fmt = '' )
      ( scen = 'C7' col = 65   hdr = 'Customer group 1' node = 'S' fld = 'KVGR1' fmt = '' )
      ( scen = 'C7' col = 66   hdr = 'Customer group 2' node = 'S' fld = 'KVGR2' fmt = '' )
      ( scen = 'C7' col = 67   hdr = 'Customer group 3' node = 'S' fld = 'KVGR3' fmt = '' )
      ( scen = 'C7' col = 68   hdr = 'Customer group 4' node = 'S' fld = 'KVGR4' fmt = '' )
      ( scen = 'C7' col = 69   hdr = 'Customer group 5' node = 'S' fld = 'KVGR5' fmt = '' )
      ( scen = 'C7' col = 70   hdr = 'Plant' node = 'Z' fld = 'WERKS' fmt = '' )
      ( scen = 'C7' col = 71   hdr = '20B. Lic. No' node = 'Z' fld = 'DRUGLICENSE1' fmt = '' )
      ( scen = 'C7' col = 72   hdr = '20B Issue Date' node = 'Z' fld = 'DL1_ISSUEDT' fmt = 'DT' )
      ( scen = 'C7' col = 73   hdr = '20B Expiry Date' node = 'Z' fld = 'DL1_VALIDDT' fmt = 'DT' )
      ( scen = 'C7' col = 74   hdr = 'DEA From Date' node = 'Z' fld = 'DEA_FROM_DATE' fmt = 'DT' )
      ( scen = 'C7' col = 75   hdr = 'DEA To Date' node = 'Z' fld = 'DEA_TO_DATE' fmt = 'DT' )
    ) TO rt.

    " V1 - Vendor creation for All CC (64 columns)
    APPEND LINES OF VALUE tt_col(
      ( scen = 'V1' col = 1    hdr = 'Field Tech name' node = '-' fld = '' fmt = '' )
      ( scen = 'V1' col = 2    hdr = 'LIFNR' node = 'K' fld = 'LIFNR' fmt = 'AL' )
      ( scen = 'V1' col = 3    hdr = 'BUKRS' node = 'K' fld = 'BUKRS' fmt = '' )
      ( scen = 'V1' col = 4    hdr = 'EKORG' node = 'K' fld = 'EKORG' fmt = '' )
      ( scen = 'V1' col = 5    hdr = 'KTOKK' node = 'V' fld = 'KTOKK' fmt = '' )
      ( scen = 'V1' col = 6    hdr = 'TITLE_MEDI' node = 'A' fld = 'TITLE' fmt = 'TT' )
      ( scen = 'V1' col = 7    hdr = 'NAME1' node = 'A' fld = 'NAME' fmt = '' )
      ( scen = 'V1' col = 8    hdr = 'NAME2' node = 'A' fld = 'NAME_2' fmt = '' )
      ( scen = 'V1' col = 9    hdr = 'Name 3' node = 'A' fld = 'NAME_3' fmt = '' )
      ( scen = 'V1' col = 10   hdr = 'Name 4' node = 'A' fld = 'NAME_4' fmt = '' )
      ( scen = 'V1' col = 11   hdr = 'SORT1' node = 'A' fld = 'SORT1' fmt = '' )
      ( scen = 'V1' col = 12   hdr = 'SORT2' node = 'A' fld = 'SORT2' fmt = '' )
      ( scen = 'V1' col = 13   hdr = 'STR_SUPPL1' node = 'A' fld = 'STR_SUPPL1' fmt = '' )
      ( scen = 'V1' col = 14   hdr = 'STR_SUPPL2' node = 'A' fld = 'STR_SUPPL2' fmt = '' )
      ( scen = 'V1' col = 15   hdr = 'STREET' node = 'A' fld = 'STREET' fmt = '' )
      ( scen = 'V1' col = 16   hdr = 'STR_SUPPL3' node = 'A' fld = 'STR_SUPPL3' fmt = '' )
      ( scen = 'V1' col = 17   hdr = 'CITY2' node = 'A' fld = 'DISTRICT' fmt = '' )
      ( scen = 'V1' col = 18   hdr = 'POST_CODE1' node = 'A' fld = 'POSTL_COD1' fmt = '' )
      ( scen = 'V1' col = 19   hdr = 'CITY1' node = 'A' fld = 'CITY' fmt = '' )
      ( scen = 'V1' col = 20   hdr = 'COUNTRY' node = 'A' fld = 'COUNTRY' fmt = '' )
      ( scen = 'V1' col = 21   hdr = 'REGION' node = 'A' fld = 'REGION' fmt = '' )
      ( scen = 'V1' col = 22   hdr = 'LANGU' node = 'A' fld = 'LANGU' fmt = '' )
      ( scen = 'V1' col = 23   hdr = 'TEL_NUMBER' node = 'M' fld = 'TEL' fmt = '' )
      ( scen = 'V1' col = 24   hdr = 'TEL_EXTENS' node = 'M' fld = 'TELX' fmt = '' )
      ( scen = 'V1' col = 25   hdr = 'TEL_NUMBER2' node = 'M' fld = 'TEL2' fmt = '' )
      ( scen = 'V1' col = 26   hdr = 'TEL_EXTENS2' node = 'M' fld = 'TELX2' fmt = '' )
      ( scen = 'V1' col = 27   hdr = 'MOB_NUMBER' node = 'M' fld = 'MOB' fmt = '' )
      ( scen = 'V1' col = 28   hdr = 'MOB_NUMBER2' node = 'M' fld = 'MOB2' fmt = '' )
      ( scen = 'V1' col = 29   hdr = 'FAX_NUMBER' node = 'M' fld = 'FAX' fmt = '' )
      ( scen = 'V1' col = 30   hdr = 'SMTP_ADDR' node = 'M' fld = 'SMT' fmt = '' )
      ( scen = 'V1' col = 31   hdr = 'SMTP_ADDR2' node = 'M' fld = 'SMT2' fmt = '' )
      ( scen = 'V1' col = 32   hdr = 'KUNNR' node = 'V' fld = 'KUNNR' fmt = 'AL' )
      ( scen = 'V1' col = 33   hdr = 'VBUND' node = 'V' fld = 'VBUND' fmt = 'AL' )
      ( scen = 'V1' col = 34   hdr = 'KONZS' node = 'V' fld = 'KONZS' fmt = '' )
      ( scen = 'V1' col = 35   hdr = 'STCD3' node = 'V' fld = 'STCD3' fmt = '' )
      ( scen = 'V1' col = 36   hdr = 'STCD5' node = 'V' fld = 'STCD5' fmt = '' )
      ( scen = 'V1' col = 37   hdr = 'STCEG' node = 'V' fld = 'STCEG' fmt = '' )
      ( scen = 'V1' col = 38   hdr = 'J_1KFTBUS' node = 'V' fld = 'J_1KFTBUS' fmt = '' )
      ( scen = 'V1' col = 39   hdr = 'STENR' node = 'V' fld = 'STENR' fmt = '' )
      ( scen = 'V1' col = 40   hdr = 'BRSCH' node = 'V' fld = 'BRSCH' fmt = '' )
      ( scen = 'V1' col = 41   hdr = 'BANKS_01' node = 'N' fld = 'BANKS#1' fmt = '' )
      ( scen = 'V1' col = 42   hdr = 'BANKL_01' node = 'N' fld = 'BANKL#1' fmt = '' )
      ( scen = 'V1' col = 43   hdr = 'BANKN_01' node = 'N' fld = 'BANKN#1' fmt = '' )
      ( scen = 'V1' col = 44   hdr = 'KOINH_01' node = 'N' fld = 'KOINH#1' fmt = '' )
      ( scen = 'V1' col = 45   hdr = 'BKONT' node = 'N' fld = 'BKONT#1' fmt = '' )
      ( scen = 'V1' col = 46   hdr = 'IBAN' node = 'N' fld = 'IBAN#1' fmt = '' )
      ( scen = 'V1' col = 47   hdr = 'AKONT' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( scen = 'V1' col = 48   hdr = 'FDGRV' node = 'B' fld = 'FDGRV' fmt = '' )
      ( scen = 'V1' col = 49   hdr = 'ALTKN' node = 'B' fld = 'ALTKN' fmt = '' )
      ( scen = 'V1' col = 50   hdr = 'ZTERM company code' node = 'B' fld = 'ZTERM' fmt = '' )
      ( scen = 'V1' col = 51   hdr = 'REPRF' node = 'B' fld = 'REPRF' fmt = '' )
      ( scen = 'V1' col = 52   hdr = 'ZWELS' node = 'B' fld = 'ZWELS' fmt = '' )
      ( scen = 'V1' col = 53   hdr = 'ZAHLS' node = 'B' fld = 'ZAHLS' fmt = '' )
      ( scen = 'V1' col = 54   hdr = 'HBKID' node = 'B' fld = 'HBKID' fmt = '' )
      ( scen = 'V1' col = 55   hdr = 'VEN_CLASS' node = 'V' fld = 'VEN_CLASS' fmt = '' )
      ( scen = 'V1' col = 56   hdr = 'J_1ISSIST' node = 'V' fld = 'J_1ISSIST' fmt = '' )
      ( scen = 'V1' col = 57   hdr = 'J_1IPANNO' node = 'V' fld = 'J_1IPANNO' fmt = '' )
      ( scen = 'V1' col = 58   hdr = 'QLAND' node = 'B' fld = 'QLAND' fmt = '' )
      ( scen = 'V1' col = 59   hdr = 'WITHT' node = 'W' fld = 'WITHT#1' fmt = '' )
      ( scen = 'V1' col = 60   hdr = 'WT_WITHCD' node = 'W' fld = 'WT_WITHCD#1' fmt = '' )
      ( scen = 'V1' col = 61   hdr = 'WAERS' node = 'P' fld = 'WAERS' fmt = '' )
      ( scen = 'V1' col = 62   hdr = 'ZTERM purch. org' node = 'P' fld = 'ZTERM' fmt = '' )
      ( scen = 'V1' col = 63   hdr = 'KALSK' node = 'P' fld = 'KALSK' fmt = '' )
      ( scen = 'V1' col = 64   hdr = 'WEBRE' node = 'P' fld = 'WEBRE' fmt = '' )
      ( scen = 'V1' col = 65   hdr = 'INCO1' node = 'P' fld = 'INCO1' fmt = '' )
      ( scen = 'V1' col = 66   hdr = 'INCO2' node = 'P' fld = 'INCO2' fmt = '' )
    ) TO rt.

    " V2 - TDS upload (64 columns)
    APPEND LINES OF VALUE tt_col(
      ( scen = 'V2' col = 2    hdr = 'LIFNR' node = 'K' fld = 'LIFNR' fmt = 'AL' )
      ( scen = 'V2' col = 3    hdr = 'BUKRS' node = 'K' fld = 'BUKRS' fmt = '' )
      ( scen = 'V2' col = 4    hdr = 'D0610' node = '-' fld = '' fmt = '' )
      ( scen = 'V2' col = 5    hdr = 'QLAND' node = 'B' fld = 'QLAND' fmt = '' )
      ( scen = 'V2' col = 6    hdr = 'WITHT_01' node = 'W' fld = 'WITHT#1' fmt = '' )
      ( scen = 'V2' col = 7    hdr = 'WITHT_02' node = 'W' fld = 'WITHT#2' fmt = '' )
      ( scen = 'V2' col = 8    hdr = 'WITHT_03' node = 'W' fld = 'WITHT#3' fmt = '' )
      ( scen = 'V2' col = 9    hdr = 'WITHT_04' node = 'W' fld = 'WITHT#4' fmt = '' )
      ( scen = 'V2' col = 10   hdr = 'WITHT_05' node = 'W' fld = 'WITHT#5' fmt = '' )
      ( scen = 'V2' col = 11   hdr = 'WITHT_06' node = 'W' fld = 'WITHT#6' fmt = '' )
      ( scen = 'V2' col = 12   hdr = 'WT_WITHCD_01' node = 'W' fld = 'WT_WITHCD#1' fmt = '' )
      ( scen = 'V2' col = 13   hdr = 'WT_WITHCD_02' node = 'W' fld = 'WT_WITHCD#2' fmt = '' )
      ( scen = 'V2' col = 14   hdr = 'WT_WITHCD_03' node = 'W' fld = 'WT_WITHCD#3' fmt = '' )
      ( scen = 'V2' col = 15   hdr = 'WT_WITHCD_04' node = 'W' fld = 'WT_WITHCD#4' fmt = '' )
      ( scen = 'V2' col = 16   hdr = 'WT_WITHCD_05' node = 'W' fld = 'WT_WITHCD#5' fmt = '' )
      ( scen = 'V2' col = 17   hdr = 'WT_WITHCD_06' node = 'W' fld = 'WT_WITHCD#6' fmt = '' )
      ( scen = 'V2' col = 18   hdr = 'WT_SUBJCT_01' node = 'W' fld = 'WT_SUBJCT#1' fmt = '' )
      ( scen = 'V2' col = 19   hdr = 'WT_SUBJCT_02' node = 'W' fld = 'WT_SUBJCT#2' fmt = '' )
      ( scen = 'V2' col = 20   hdr = 'WT_SUBJCT_03' node = 'W' fld = 'WT_SUBJCT#3' fmt = '' )
      ( scen = 'V2' col = 21   hdr = 'WT_SUBJCT_04' node = 'W' fld = 'WT_SUBJCT#4' fmt = '' )
      ( scen = 'V2' col = 22   hdr = 'WT_SUBJCT_05' node = 'W' fld = 'WT_SUBJCT#5' fmt = '' )
      ( scen = 'V2' col = 23   hdr = 'WT_SUBJCT_06' node = 'W' fld = 'WT_SUBJCT#6' fmt = '' )
      ( scen = 'V2' col = 24   hdr = 'QSREC_01' node = 'W' fld = 'QSREC#1' fmt = '' )
      ( scen = 'V2' col = 25   hdr = 'QSREC_02' node = 'W' fld = 'QSREC#2' fmt = '' )
      ( scen = 'V2' col = 26   hdr = 'QSREC_03' node = 'W' fld = 'QSREC#3' fmt = '' )
      ( scen = 'V2' col = 27   hdr = 'QSREC_04' node = 'W' fld = 'QSREC#4' fmt = '' )
      ( scen = 'V2' col = 28   hdr = 'QSREC_05' node = 'W' fld = 'QSREC#5' fmt = '' )
      ( scen = 'V2' col = 29   hdr = 'QSREC_06' node = 'W' fld = 'QSREC#6' fmt = '' )
      ( scen = 'V2' col = 30   hdr = 'WT_WTSTCD_01' node = 'W' fld = 'WT_WTSTCD#1' fmt = '' )
      ( scen = 'V2' col = 31   hdr = 'WT_WTSTCD_02' node = 'W' fld = 'WT_WTSTCD#2' fmt = '' )
      ( scen = 'V2' col = 32   hdr = 'WT_WTSTCD_03' node = 'W' fld = 'WT_WTSTCD#3' fmt = '' )
      ( scen = 'V2' col = 33   hdr = 'WT_WTSTCD_04' node = 'W' fld = 'WT_WTSTCD#4' fmt = '' )
      ( scen = 'V2' col = 34   hdr = 'WT_WTSTCD_05' node = 'W' fld = 'WT_WTSTCD#5' fmt = '' )
      ( scen = 'V2' col = 35   hdr = 'WT_WTSTCD_06' node = 'W' fld = 'WT_WTSTCD#6' fmt = '' )
      ( scen = 'V2' col = 36   hdr = 'WT_EXNR_01' node = 'W' fld = 'WT_EXNR#1' fmt = '' )
      ( scen = 'V2' col = 37   hdr = 'WT_EXNR_02' node = 'W' fld = 'WT_EXNR#2' fmt = '' )
      ( scen = 'V2' col = 38   hdr = 'WT_EXNR_03' node = 'W' fld = 'WT_EXNR#3' fmt = '' )
      ( scen = 'V2' col = 39   hdr = 'WT_EXNR_04' node = 'W' fld = 'WT_EXNR#4' fmt = '' )
      ( scen = 'V2' col = 40   hdr = 'WT_EXNR_05' node = 'W' fld = 'WT_EXNR#5' fmt = '' )
      ( scen = 'V2' col = 41   hdr = 'WT_EXNR_06' node = 'W' fld = 'WT_EXNR#6' fmt = '' )
      ( scen = 'V2' col = 42   hdr = 'WT_EXRT_01' node = 'W' fld = 'WT_EXRT#1' fmt = '' )
      ( scen = 'V2' col = 43   hdr = 'WT_EXRT_02' node = 'W' fld = 'WT_EXRT#2' fmt = '' )
      ( scen = 'V2' col = 44   hdr = 'WT_EXRT_03' node = 'W' fld = 'WT_EXRT#3' fmt = '' )
      ( scen = 'V2' col = 45   hdr = 'WT_EXRT_04' node = 'W' fld = 'WT_EXRT#4' fmt = '' )
      ( scen = 'V2' col = 46   hdr = 'WT_EXRT_05' node = 'W' fld = 'WT_EXRT#5' fmt = '' )
      ( scen = 'V2' col = 47   hdr = 'WT_EXRT_06' node = 'W' fld = 'WT_EXRT#6' fmt = '' )
      ( scen = 'V2' col = 48   hdr = 'WT_WTEXRS_01' node = 'W' fld = 'WT_WTEXRS#1' fmt = '' )
      ( scen = 'V2' col = 49   hdr = 'WT_WTEXRS_02' node = 'W' fld = 'WT_WTEXRS#2' fmt = '' )
      ( scen = 'V2' col = 50   hdr = 'WT_WTEXRS_03' node = 'W' fld = 'WT_WTEXRS#3' fmt = '' )
      ( scen = 'V2' col = 51   hdr = 'WT_WTEXRS_04' node = 'W' fld = 'WT_WTEXRS#4' fmt = '' )
      ( scen = 'V2' col = 52   hdr = 'WT_WTEXRS_05' node = 'W' fld = 'WT_WTEXRS#5' fmt = '' )
      ( scen = 'V2' col = 53   hdr = 'WT_WTEXRS_06' node = 'W' fld = 'WT_WTEXRS#6' fmt = '' )
      ( scen = 'V2' col = 54   hdr = 'WT_EXDF_01' node = 'W' fld = 'WT_EXDF#1' fmt = '' )
      ( scen = 'V2' col = 55   hdr = 'WT_EXDF_02' node = 'W' fld = 'WT_EXDF#2' fmt = '' )
      ( scen = 'V2' col = 56   hdr = 'WT_EXDF_03' node = 'W' fld = 'WT_EXDF#3' fmt = '' )
      ( scen = 'V2' col = 57   hdr = 'WT_EXDF_04' node = 'W' fld = 'WT_EXDF#4' fmt = '' )
      ( scen = 'V2' col = 58   hdr = 'WT_EXDF_05' node = 'W' fld = 'WT_EXDF#5' fmt = '' )
      ( scen = 'V2' col = 59   hdr = 'WT_EXDF_06' node = 'W' fld = 'WT_EXDF#6' fmt = '' )
      ( scen = 'V2' col = 60   hdr = 'WT_EXDT_01' node = 'W' fld = 'WT_EXDT#1' fmt = '' )
      ( scen = 'V2' col = 61   hdr = 'WT_EXDT_02' node = 'W' fld = 'WT_EXDT#2' fmt = '' )
      ( scen = 'V2' col = 62   hdr = 'WT_EXDT_03' node = 'W' fld = 'WT_EXDT#3' fmt = '' )
      ( scen = 'V2' col = 63   hdr = 'WT_EXDT_04' node = 'W' fld = 'WT_EXDT#4' fmt = '' )
      ( scen = 'V2' col = 64   hdr = 'WT_EXDT_05' node = 'W' fld = 'WT_EXDT#5' fmt = '' )
      ( scen = 'V2' col = 65   hdr = 'WT_EXDT_06' node = 'W' fld = 'WT_EXDT#6' fmt = '' )
    ) TO rt.

    " V3 - TAN details (21 columns)
    APPEND LINES OF VALUE tt_col(
      ( scen = 'V3' col = 1    hdr = 'Vendor' node = 'K' fld = 'LIFNR' fmt = 'AL' )
      ( scen = 'V3' col = 2    hdr = 'Company' node = 'K' fld = 'BUKRS' fmt = '' )
      ( scen = 'V3' col = 3    hdr = 'Address' node = '-' fld = '' fmt = '' )
      ( scen = 'V3' col = 4    hdr = 'Section_code_1' node = 'X' fld = 'SECCODE#1' fmt = '' )
      ( scen = 'V3' col = 5    hdr = 'Section_code_2' node = 'X' fld = 'SECCODE#2' fmt = '' )
      ( scen = 'V3' col = 6    hdr = 'Certificate_1' node = 'X' fld = 'WT_EXNR#1' fmt = '' )
      ( scen = 'V3' col = 7    hdr = 'Certificate_2' node = 'X' fld = 'WT_EXNR#2' fmt = '' )
      ( scen = 'V3' col = 8    hdr = 'Exemption_rate_1' node = 'X' fld = 'WT_EXRT#1' fmt = '' )
      ( scen = 'V3' col = 9    hdr = 'Exemption_rate_2' node = 'X' fld = 'WT_EXRT#2' fmt = '' )
      ( scen = 'V3' col = 10   hdr = 'Validfrom_1' node = 'X' fld = 'WT_EXDF#1' fmt = '' )
      ( scen = 'V3' col = 11   hdr = 'Validfrom2' node = 'X' fld = 'WT_EXDF#2' fmt = '' )
      ( scen = 'V3' col = 12   hdr = 'Validto_1' node = 'X' fld = 'WT_EXDT#1' fmt = '' )
      ( scen = 'V3' col = 13   hdr = 'Validto_2' node = 'X' fld = 'WT_EXDT#2' fmt = '' )
      ( scen = 'V3' col = 14   hdr = 'taxtype_1' node = 'X' fld = 'WITHT#1' fmt = '' )
      ( scen = 'V3' col = 15   hdr = 'Taxtype_2' node = 'X' fld = 'WITHT#2' fmt = '' )
      ( scen = 'V3' col = 16   hdr = 'taxcode_1' node = 'X' fld = 'WT_WITHCD#1' fmt = '' )
      ( scen = 'V3' col = 17   hdr = 'Taxcode_2' node = 'X' fld = 'WT_WITHCD#2' fmt = '' )
      ( scen = 'V3' col = 18   hdr = 'threshold_1' node = 'X' fld = 'FIWTIN_EXEM_THR#1' fmt = '' )
      ( scen = 'V3' col = 19   hdr = 'threshold_2' node = 'X' fld = 'FIWTIN_EXEM_THR#2' fmt = '' )
      ( scen = 'V3' col = 20   hdr = 'Currency_1' node = 'X' fld = 'WAERS#1' fmt = '' )
      ( scen = 'V3' col = 21   hdr = 'Currency_2' node = 'X' fld = 'WAERS#2' fmt = '' )
    ) TO rt.

    " V4 - BANK Key creation (9 columns)
    APPEND LINES OF VALUE tt_col(
      ( scen = 'V4' col = 1    hdr = 'Field Technical Name' node = '-' fld = '' fmt = '' )
      ( scen = 'V4' col = 2    hdr = 'BANKS' node = 'N' fld = 'BANKS#1' fmt = '' )
      ( scen = 'V4' col = 3    hdr = 'BANKL' node = 'N' fld = 'BANKL#1' fmt = '' )
      ( scen = 'V4' col = 4    hdr = 'BANKA' node = 'Y' fld = 'BANKA' fmt = '' )
      ( scen = 'V4' col = 5    hdr = 'PROVZ' node = 'Y' fld = 'PROVZ' fmt = '' )
      ( scen = 'V4' col = 6    hdr = 'STRAS' node = 'Y' fld = 'STRAS' fmt = '' )
      ( scen = 'V4' col = 7    hdr = 'ORT01' node = 'Y' fld = 'ORT01' fmt = '' )
      ( scen = 'V4' col = 8    hdr = 'BRNCH' node = 'Y' fld = 'BRNCH' fmt = '' )
      ( scen = 'V4' col = 9    hdr = 'SWIFT' node = 'Y' fld = 'SWIFT' fmt = '' )
    ) TO rt.

    " V5 - Bank details update (8 columns)
    APPEND LINES OF VALUE tt_col(
      ( scen = 'V5' col = 1    hdr = 'Field Technical Name' node = '-' fld = '' fmt = '' )
      ( scen = 'V5' col = 2    hdr = 'LIFNR' node = 'K' fld = 'LIFNR' fmt = 'AL' )
      ( scen = 'V5' col = 3    hdr = 'BUKRS' node = 'K' fld = 'BUKRS' fmt = '' )
      ( scen = 'V5' col = 4    hdr = 'BANKS' node = 'N' fld = 'BANKS#1' fmt = '' )
      ( scen = 'V5' col = 5    hdr = 'BANKL' node = 'N' fld = 'BANKL#1' fmt = '' )
      ( scen = 'V5' col = 6    hdr = 'BANKN' node = 'N' fld = 'BANKN#1' fmt = '' )
      ( scen = 'V5' col = 7    hdr = 'KOINH' node = 'N' fld = 'KOINH#1' fmt = '' )
      ( scen = 'V5' col = 8    hdr = 'IBAN' node = 'N' fld = 'IBAN#1' fmt = '' )
    ) TO rt.

    " V6 - Vendor extension (12 columns)
    APPEND LINES OF VALUE tt_col(
      ( scen = 'V6' col = 1    hdr = 'Field Technical Name' node = '-' fld = '' fmt = '' )
      ( scen = 'V6' col = 2    hdr = 'LIFNR' node = 'K' fld = 'LIFNR' fmt = 'AL' )
      ( scen = 'V6' col = 3    hdr = 'BUKRS' node = 'K' fld = 'BUKRS' fmt = '' )
      ( scen = 'V6' col = 4    hdr = 'EKORG' node = 'K' fld = 'EKORG' fmt = '' )
      ( scen = 'V6' col = 5    hdr = 'REF LIFNR' node = '-' fld = '' fmt = '' )
      ( scen = 'V6' col = 6    hdr = 'REF BUKRS' node = 'K' fld = 'RBUKRS' fmt = '' )
      ( scen = 'V6' col = 7    hdr = 'REF EKORG' node = 'K' fld = 'REKORG' fmt = '' )
      ( scen = 'V6' col = 8    hdr = 'Char' node = '-' fld = '' fmt = '' )
      ( scen = 'V6' col = 9    hdr = 'AKONT' node = 'B' fld = 'AKONT' fmt = 'GL' )
      ( scen = 'V6' col = 10   hdr = 'ZWELS' node = 'B' fld = 'ZWELS' fmt = '' )
      ( scen = 'V6' col = 11   hdr = 'REPRF' node = 'B' fld = 'REPRF' fmt = '' )
      ( scen = 'V6' col = 12   hdr = 'WAERS' node = 'P' fld = 'WAERS' fmt = '' )
      ( scen = 'V6' col = 13   hdr = 'KALSK' node = 'P' fld = 'KALSK' fmt = '' )
      ( scen = 'V6' col = 14   hdr = 'WEBRE' node = 'P' fld = 'WEBRE' fmt = '' )
    ) TO rt.

    " V7 - CIN details (15 columns)
    APPEND LINES OF VALUE tt_col(
      ( scen = 'V7' col = 1    hdr = 'Vendor Account Number' node = 'K' fld = 'LIFNR' fmt = 'AL' )
      ( scen = 'V7' col = 2    hdr = 'Company Code' node = 'K' fld = 'BUKRS' fmt = '' )
      ( scen = 'V7' col = 3    hdr = 'Address View' node = '-' fld = '' fmt = '' )
      ( scen = 'V7' col = 4    hdr = 'ECC Number' node = 'V' fld = 'J_1IEXCD' fmt = '' )
      ( scen = 'V7' col = 5    hdr = 'Excise Registration Number' node = 'V' fld = 'J_1IEXRN' fmt = '' )
      ( scen = 'V7' col = 6    hdr = 'Excise Range' node = 'V' fld = 'J_1IEXRG' fmt = '' )
      ( scen = 'V7' col = 7    hdr = 'Excise Division' node = 'V' fld = 'J_1IEXDI' fmt = '' )
      ( scen = 'V7' col = 8    hdr = 'Excise Commissionerate' node = 'V' fld = 'J_1IEXCO' fmt = '' )
      ( scen = 'V7' col = 9    hdr = 'Central Sales Tax Number' node = 'V' fld = 'J_1ICSTNO' fmt = '' )
      ( scen = 'V7' col = 10   hdr = 'Local Sales Tax Number' node = 'V' fld = 'J_1ILSTNO' fmt = '' )
      ( scen = 'V7' col = 11   hdr = 'Service Tax Registration Number' node = 'V' fld = 'J_1ISERN' fmt = '' )
      ( scen = 'V7' col = 12   hdr = 'Permanent Account Number' node = 'V' fld = 'J_1IPANNO' fmt = '' )
      ( scen = 'V7' col = 13   hdr = 'SSI status' node = 'V' fld = 'J_1ISSIST' fmt = '' )
      ( scen = 'V7' col = 14   hdr = 'Exc.Tax Ind. Vendor' node = 'V' fld = 'J_1IEXCIVE' fmt = '' )
      ( scen = 'V7' col = 15   hdr = 'Type of Vendor' node = 'V' fld = 'J_1IVTYP' fmt = '' )
    ) TO rt.

    " V8 - Patner function (35 columns)
    APPEND LINES OF VALUE tt_col(
      ( scen = 'V8' col = 1    hdr = 'LIFNR' node = 'K' fld = 'LIFNR' fmt = 'AL' )
      ( scen = 'V8' col = 2    hdr = 'BUKRS' node = 'K' fld = 'BUKRS' fmt = '' )
      ( scen = 'V8' col = 3    hdr = 'EKORG' node = 'K' fld = 'EKORG' fmt = '' )
      ( scen = 'V8' col = 4    hdr = 'D0320' node = '-' fld = '' fmt = '' )
      ( scen = 'V8' col = 5    hdr = 'USE_ZAV' node = '-' fld = '' fmt = '' )
      ( scen = 'V8' col = 6    hdr = 'PARVW_05' node = 'F' fld = 'PARVW#5' fmt = '' )
      ( scen = 'V8' col = 7    hdr = 'PARVW_06' node = 'F' fld = 'PARVW#6' fmt = '' )
      ( scen = 'V8' col = 8    hdr = 'PARVW_07' node = 'F' fld = 'PARVW#7' fmt = '' )
      ( scen = 'V8' col = 9    hdr = 'PARVW_08' node = 'F' fld = 'PARVW#8' fmt = '' )
      ( scen = 'V8' col = 10   hdr = 'PARVW_09' node = 'F' fld = 'PARVW#9' fmt = '' )
      ( scen = 'V8' col = 11   hdr = 'PARVW_10' node = 'F' fld = 'PARVW#10' fmt = '' )
      ( scen = 'V8' col = 12   hdr = 'PARVW_11' node = 'F' fld = 'PARVW#11' fmt = '' )
      ( scen = 'V8' col = 13   hdr = 'PARVW_12' node = 'F' fld = 'PARVW#12' fmt = '' )
      ( scen = 'V8' col = 14   hdr = 'PARVW_13' node = 'F' fld = 'PARVW#13' fmt = '' )
      ( scen = 'V8' col = 15   hdr = 'PARVW_14' node = 'F' fld = 'PARVW#14' fmt = '' )
      ( scen = 'V8' col = 16   hdr = 'PARVW_15' node = 'F' fld = 'PARVW#15' fmt = '' )
      ( scen = 'V8' col = 17   hdr = 'GPARN_05' node = 'F' fld = 'PARTNER#5' fmt = '' )
      ( scen = 'V8' col = 18   hdr = 'GPARN_06' node = 'F' fld = 'PARTNER#6' fmt = '' )
      ( scen = 'V8' col = 19   hdr = 'GPARN_07' node = 'F' fld = 'PARTNER#7' fmt = '' )
      ( scen = 'V8' col = 20   hdr = 'GPARN_08' node = 'F' fld = 'PARTNER#8' fmt = '' )
      ( scen = 'V8' col = 21   hdr = 'GPARN_09' node = 'F' fld = 'PARTNER#9' fmt = '' )
      ( scen = 'V8' col = 22   hdr = 'GPARN_10' node = 'F' fld = 'PARTNER#10' fmt = '' )
      ( scen = 'V8' col = 23   hdr = 'GPARN_11' node = 'F' fld = 'PARTNER#11' fmt = '' )
      ( scen = 'V8' col = 24   hdr = 'GPARN_12' node = 'F' fld = 'PARTNER#12' fmt = '' )
      ( scen = 'V8' col = 25   hdr = 'GPARN_13' node = 'F' fld = 'PARTNER#13' fmt = '' )
      ( scen = 'V8' col = 26   hdr = 'GPARN_14' node = 'F' fld = 'PARTNER#14' fmt = '' )
      ( scen = 'V8' col = 27   hdr = 'GPARN_15' node = 'F' fld = 'PARTNER#15' fmt = '' )
      ( scen = 'V8' col = 28   hdr = 'PARVW_01' node = 'F' fld = 'PARVW#1' fmt = '' )
      ( scen = 'V8' col = 29   hdr = 'PARVW_02' node = 'F' fld = 'PARVW#2' fmt = '' )
      ( scen = 'V8' col = 30   hdr = 'PARVW_03' node = 'F' fld = 'PARVW#3' fmt = '' )
      ( scen = 'V8' col = 31   hdr = 'PARVW_04' node = 'F' fld = 'PARVW#4' fmt = '' )
      ( scen = 'V8' col = 32   hdr = 'GPARN_01' node = 'F' fld = 'PARTNER#1' fmt = '' )
      ( scen = 'V8' col = 33   hdr = 'GPARN_02' node = 'F' fld = 'PARTNER#2' fmt = '' )
      ( scen = 'V8' col = 34   hdr = 'GPARN_03' node = 'F' fld = 'PARTNER#3' fmt = '' )
      ( scen = 'V8' col = 35   hdr = 'GPARN_04' node = 'F' fld = 'PARTNER#4' fmt = '' )
    ) TO rt.

    " V9 - Block_Unblocked (9 columns)
    APPEND LINES OF VALUE tt_col(
      ( scen = 'V9' col = 1    hdr = 'Tech name' node = '-' fld = '' fmt = '' )
      ( scen = 'V9' col = 2    hdr = 'LIFNR' node = 'K' fld = 'LIFNR' fmt = 'AL' )
      ( scen = 'V9' col = 3    hdr = 'BUKRS' node = 'K' fld = 'BUKRS' fmt = '' )
      ( scen = 'V9' col = 4    hdr = 'EKORG' node = 'K' fld = 'EKORG' fmt = '' )
      ( scen = 'V9' col = 5    hdr = 'SPERR' node = 'V' fld = 'SPERR' fmt = '' )
      ( scen = 'V9' col = 6    hdr = 'SPERR_1' node = 'B' fld = 'SPERR' fmt = '' )
      ( scen = 'V9' col = 7    hdr = 'SPERM' node = 'V' fld = 'SPERM' fmt = '' )
      ( scen = 'V9' col = 8    hdr = 'SPERM_1' node = 'P' fld = 'SPERM' fmt = '' )
      ( scen = 'V9' col = 9    hdr = 'SPERQ' node = 'V' fld = 'SPERQ' fmt = '' )
    ) TO rt.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_SRC - reads the master data
*   Through the same external interfaces the upload programs write
*   through, so the structures line up field for field. Read only.
*----------------------------------------------------------------------*
CLASS lcl_src DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_key,
             kunnr   TYPE kunnr,
             lifnr   TYPE lifnr,
             partner TYPE bu_partner,
           END OF ty_key,
           tt_key TYPE STANDARD TABLE OF ty_key WITH EMPTY KEY.

    " What the selection screen asked for: customers, or suppliers, plus
    " whatever sits behind the business partners it names.
    CLASS-METHODS keys
      IMPORTING iv_cust   TYPE abap_bool
      RETURNING VALUE(rt) TYPE tt_key.

    CLASS-METHODS customer
      IMPORTING iv_kunnr TYPE kunnr
      EXPORTING es_data  TYPE cmds_ei_extern
      RAISING   lcx_ext.

    CLASS-METHODS vendor
      IMPORTING iv_lifnr TYPE lifnr
      EXPORTING es_data  TYPE vmds_ei_extern
      RAISING   lcx_ext.

    CLASS-METHODS title_text
      IMPORTING iv_title  TYPE clike
      RETURNING VALUE(rv) TYPE string.

    " Two things the supplier interface does not carry: the bank master
    " behind the vendor's own bank details, and the TAN exemption blocks.
    " Passed by value with the dictionary type they are read with: an Open
    " SQL host variable needs a type the compiler knows, and a by-reference
    " parameter of a fixed type will not take anything else.
    CLASS-METHODS bank_master
      IMPORTING VALUE(iv_banks) TYPE banks
                VALUE(iv_bankl) TYPE bankl
      EXPORTING es_bnka         TYPE bnka.

    " Component names are the table's own, so a column can be looked up by
    " the same field name the upload program writes.
    TYPES: BEGIN OF ty_tan,
             seccode         TYPE string,
             witht           TYPE string,
             wt_withcd       TYPE string,
             wt_exnr         TYPE string,
             wt_exrt         TYPE string,
             wt_exdf         TYPE string,
             wt_exdt         TYPE string,
             fiwtin_exem_thr TYPE string,
             waers           TYPE string,
           END OF ty_tan,
           tt_tan TYPE STANDARD TABLE OF ty_tan WITH EMPTY KEY.

    CLASS-METHODS tan_exem
      IMPORTING VALUE(iv_lifnr) TYPE lifnr
                VALUE(iv_bukrs) TYPE bukrs
      RETURNING VALUE(rt)       TYPE tt_tan.

    " Withholding tax, read from the table the interface writes to. The
    " company-code node of the extract carries it, but only when the
    " interface chooses to fill that node - and a sample with no tax type
    " in it looks exactly like a vendor with no tax type. Reading LFBW
    " settles which of the two it is.
    TYPES: BEGIN OF ty_wtax,
             witht     TYPE string,
             wt_withcd TYPE string,
             wt_subjct TYPE string,
             qsrec     TYPE string,
             wt_wtstcd TYPE string,
             wt_exnr   TYPE string,
             wt_exrt   TYPE string,
             wt_wtexrs TYPE string,
             wt_exdf   TYPE string,
             wt_exdt   TYPE string,
           END OF ty_wtax,
           tt_wtax TYPE STANDARD TABLE OF ty_wtax WITH EMPTY KEY.

    CLASS-METHODS wtax
      IMPORTING VALUE(iv_lifnr) TYPE lifnr
                VALUE(iv_bukrs) TYPE bukrs
      RETURNING VALUE(rt)       TYPE tt_wtax.
  PRIVATE SECTION.
    CLASS-METHODS first_error
      IMPORTING is_error  TYPE cvis_message
      RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS pick
      IMPORTING is_row    TYPE any
                iv_fld    TYPE clike
      RETURNING VALUE(rv) TYPE string.
ENDCLASS.

CLASS lcl_src IMPLEMENTATION.

  METHOD keys.
    DATA ls_key TYPE ty_key.

    IF iv_cust = abap_true.
      IF s_kunnr[] IS NOT INITIAL.
        SELECT kunnr FROM kna1 WHERE kunnr IN @s_kunnr
          ORDER BY kunnr INTO TABLE @DATA(lt_c) UP TO @p_max ROWS.
        LOOP AT lt_c INTO DATA(lv_c).
          CLEAR ls_key.
          ls_key-kunnr = lv_c-kunnr.
          APPEND ls_key TO rt.
        ENDLOOP.
      ENDIF.
      IF s_bp[] IS NOT INITIAL.
        " Business partner -> customer, through the CVI link.
        SELECT b~partner, l~customer FROM but000 AS b
          INNER JOIN cvi_cust_link AS l ON l~partner_guid = b~partner_guid
          WHERE b~partner IN @s_bp
          ORDER BY b~partner INTO TABLE @DATA(lt_bc) UP TO @p_max ROWS.
        LOOP AT lt_bc INTO DATA(ls_bc).
          IF line_exists( rt[ kunnr = ls_bc-customer ] ).
            CONTINUE.
          ENDIF.
          CLEAR ls_key.
          ls_key-kunnr   = ls_bc-customer.
          ls_key-partner = ls_bc-partner.
          APPEND ls_key TO rt.
        ENDLOOP.
      ENDIF.
    ELSE.
      IF s_lifnr[] IS NOT INITIAL.
        SELECT lifnr FROM lfa1 WHERE lifnr IN @s_lifnr
          ORDER BY lifnr INTO TABLE @DATA(lt_v) UP TO @p_max ROWS.
        LOOP AT lt_v INTO DATA(lv_v).
          CLEAR ls_key.
          ls_key-lifnr = lv_v-lifnr.
          APPEND ls_key TO rt.
        ENDLOOP.
      ENDIF.
      IF s_bp[] IS NOT INITIAL.
        SELECT b~partner, l~vendor FROM but000 AS b
          INNER JOIN cvi_vend_link AS l ON l~partner_guid = b~partner_guid
          WHERE b~partner IN @s_bp
          ORDER BY b~partner INTO TABLE @DATA(lt_bv) UP TO @p_max ROWS.
        LOOP AT lt_bv INTO DATA(ls_bv).
          IF line_exists( rt[ lifnr = ls_bv-vendor ] ).
            CONTINUE.
          ENDIF.
          CLEAR ls_key.
          ls_key-lifnr   = ls_bv-vendor.
          ls_key-partner = ls_bv-partner.
          APPEND ls_key TO rt.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD first_error.
    FIELD-SYMBOLS <lt_msg> TYPE ANY TABLE.
    ASSIGN COMPONENT 'MESSAGES' OF STRUCTURE is_error TO <lt_msg>.
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

  METHOD customer.
    CLEAR es_data.
    " OBJECT_TASK has to be there as well as the number. Without it the
    " extractor answers with an entry that carries no data at all - which is
    " what produced a sample file holding only the key and the licence
    " record, both of which are read here rather than through the interface.
    DATA ls_in TYPE cmds_ei_main.
    APPEND VALUE cmds_ei_extern( header-object_task            = gc_task_read
                                 header-object_instance-kunnr  = iv_kunnr ) TO ls_in-customers.

    DATA ls_out TYPE cmds_ei_main.
    DATA ls_err TYPE cvis_message.
    TRY.
        cmd_ei_api_extract=>get_data( EXPORTING is_master_data = ls_in
                                      IMPORTING es_master_data = ls_out
                                                es_error       = ls_err ).
      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION NEW lcx_ext( |Customer { iv_kunnr } could not be read: { lx->get_text( ) }| ).
    ENDTRY.

    IF ls_err-is_error = abap_true.
      DATA(lv_t) = first_error( ls_err ).
      RAISE EXCEPTION NEW lcx_ext( |Customer { iv_kunnr } could not be read: { lv_t }| ).
    ENDIF.
    IF ls_out-customers IS INITIAL.
      RAISE EXCEPTION NEW lcx_ext( |Customer { iv_kunnr } does not exist| ).
    ENDIF.
    es_data = ls_out-customers[ 1 ].
  ENDMETHOD.

  METHOD vendor.
    CLEAR es_data.
    DATA ls_in TYPE vmds_ei_main.
    APPEND VALUE vmds_ei_extern( header-object_task           = gc_task_read
                                 header-object_instance-lifnr = iv_lifnr ) TO ls_in-vendors.

    DATA ls_out TYPE vmds_ei_main.
    DATA ls_err TYPE cvis_message.
    TRY.
        vmd_ei_api_extract=>get_data( EXPORTING is_master_data = ls_in
                                      IMPORTING es_master_data = ls_out
                                                es_error       = ls_err ).
      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION NEW lcx_ext( |Supplier { iv_lifnr } could not be read: { lx->get_text( ) }| ).
    ENDTRY.

    IF ls_err-is_error = abap_true.
      DATA(lv_t) = first_error( ls_err ).
      RAISE EXCEPTION NEW lcx_ext( |Supplier { iv_lifnr } could not be read: { lv_t }| ).
    ENDIF.
    IF ls_out-vendors IS INITIAL.
      RAISE EXCEPTION NEW lcx_ext( |Supplier { iv_lifnr } does not exist| ).
    ENDIF.
    es_data = ls_out-vendors[ 1 ].
  ENDMETHOD.

  METHOD bank_master.
    CLEAR es_bnka.
    IF iv_banks IS INITIAL OR iv_bankl IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE * FROM bnka
      WHERE banks = @iv_banks AND bankl = @iv_bankl
      INTO @es_bnka.
  ENDMETHOD.

  METHOD tan_exem.
    " FIWTIN_TAN_EXEM is the table behind J_1ITAN_EXEM_SAVE, which the
    " upload program writes through. It belongs to the India localisation,
    " so the table is looked up before it is read - a system without it
    " gets an empty result rather than a short dump.
    SELECT SINGLE @abap_true FROM dd02l
      WHERE tabname = 'FIWTIN_TAN_EXEM' AND tabclass = 'TRANSP' AND as4local = 'A'
      INTO @DATA(lv_there).
    IF lv_there <> abap_true.
      RETURN.
    ENDIF.

    DATA lr_tab TYPE REF TO data.
    FIELD-SYMBOLS <lt> TYPE STANDARD TABLE.

    TRY.
        CREATE DATA lr_tab TYPE STANDARD TABLE OF ('FIWTIN_TAN_EXEM').
        ASSIGN lr_tab->* TO <lt>.
        IF <lt> IS NOT ASSIGNED.
          RETURN.
        ENDIF.
        SELECT * FROM ('FIWTIN_TAN_EXEM')
          WHERE koart = 'K' AND accno = @iv_lifnr AND bukrs = @iv_bukrs
          INTO CORRESPONDING FIELDS OF TABLE @<lt> UP TO 2 ROWS.
      CATCH cx_root.
        RETURN.
    ENDTRY.

    FIELD-SYMBOLS <ls> TYPE any.
    LOOP AT <lt> ASSIGNING <ls>.
      DATA ls_t TYPE ty_tan.
      CLEAR ls_t.
      ls_t-seccode         = pick( is_row = <ls> iv_fld = 'SECCODE' ).
      ls_t-witht           = pick( is_row = <ls> iv_fld = 'WITHT' ).
      ls_t-wt_withcd       = pick( is_row = <ls> iv_fld = 'WT_WITHCD' ).
      ls_t-wt_exnr         = pick( is_row = <ls> iv_fld = 'WT_EXNR' ).
      ls_t-wt_exrt         = pick( is_row = <ls> iv_fld = 'WT_EXRT' ).
      ls_t-wt_exdf         = pick( is_row = <ls> iv_fld = 'WT_EXDF' ).
      ls_t-wt_exdt         = pick( is_row = <ls> iv_fld = 'WT_EXDT' ).
      ls_t-fiwtin_exem_thr = pick( is_row = <ls> iv_fld = 'FIWTIN_EXEM_THR' ).
      ls_t-waers           = pick( is_row = <ls> iv_fld = 'WAERS' ).
      APPEND ls_t TO rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD wtax.
    SELECT witht, wt_withcd, wt_subjct, qsrec, wt_wtstcd,
           wt_exnr, wt_exrt, wt_wtexrs, wt_exdf, wt_exdt
      FROM lfbw
      WHERE lifnr = @iv_lifnr AND bukrs = @iv_bukrs
      ORDER BY witht
      INTO TABLE @DATA(lt_w) UP TO 6 ROWS.

    LOOP AT lt_w INTO DATA(ls_w).
      DATA ls_r TYPE ty_wtax.
      CLEAR ls_r.
      ls_r-witht     = lcl_util=>text( iv_value = ls_w-witht     iv_fmt = '' ).
      ls_r-wt_withcd = lcl_util=>text( iv_value = ls_w-wt_withcd iv_fmt = '' ).
      ls_r-wt_subjct = lcl_util=>text( iv_value = ls_w-wt_subjct iv_fmt = '' ).
      ls_r-qsrec     = lcl_util=>text( iv_value = ls_w-qsrec     iv_fmt = '' ).
      ls_r-wt_wtstcd = lcl_util=>text( iv_value = ls_w-wt_wtstcd iv_fmt = '' ).
      ls_r-wt_exnr   = lcl_util=>text( iv_value = ls_w-wt_exnr   iv_fmt = '' ).
      ls_r-wt_exrt   = lcl_util=>text( iv_value = ls_w-wt_exrt   iv_fmt = '' ).
      ls_r-wt_wtexrs = lcl_util=>text( iv_value = ls_w-wt_wtexrs iv_fmt = '' ).
      ls_r-wt_exdf   = lcl_util=>text( iv_value = ls_w-wt_exdf   iv_fmt = '' ).
      ls_r-wt_exdt   = lcl_util=>text( iv_value = ls_w-wt_exdt   iv_fmt = '' ).
      APPEND ls_r TO rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD pick.
    FIELD-SYMBOLS <lv> TYPE any.
    ASSIGN COMPONENT iv_fld OF STRUCTURE is_row TO <lv>.
    IF sy-subrc = 0.
      rv = lcl_util=>text( iv_value = <lv> iv_fmt = '' ).
    ENDIF.
  ENDMETHOD.

  METHOD title_text.
    " The templates carry the title as text, the master data as a key.
    DATA(lv_key) = CONV ad_title( iv_title ).
    IF lv_key IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE title_medi FROM tsad3t
      WHERE langu = @sy-langu AND title = @lv_key INTO @rv.
    IF sy-subrc <> 0.
      rv = lv_key.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_ENG - turns master data into rows of the scenario's tab
*----------------------------------------------------------------------*
CLASS lcl_eng DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor IMPORTING iv_scen TYPE char2.
    METHODS run.
    METHODS head RETURNING VALUE(rt) TYPE tt_cell.
    METHODS rows RETURNING VALUE(rt) TYPE tt_row.
    METHODS log  RETURNING VALUE(rt) TYPE tt_msg.
  PRIVATE SECTION.
    DATA mv_scen TYPE char2.
    DATA mt_col  TYPE tt_col.
    DATA mt_row  TYPE tt_row.
    DATA mt_msg  TYPE tt_msg.
    DATA mv_wide TYPE i.

    METHODS add_msg IMPORTING iv_key TYPE clike iv_type TYPE char1 iv_text TYPE clike.
    METHODS put     IMPORTING iv_col TYPE i iv_val TYPE clike CHANGING cs_row TYPE ty_row.
    METHODS comp    IMPORTING is_any TYPE any iv_fld TYPE clike iv_fmt TYPE clike
                    RETURNING VALUE(rv) TYPE string.
    METHODS empty_row RETURNING VALUE(rs) TYPE ty_row.

    METHODS cust IMPORTING is_key TYPE lcl_src=>ty_key.
    METHODS cred IMPORTING is_key TYPE lcl_src=>ty_key.
    METHODS vend IMPORTING is_key TYPE lcl_src=>ty_key.
    METHODS split_occ IMPORTING iv_in  TYPE clike
                      EXPORTING ev_fld TYPE string
                                ev_occ TYPE i.
    METHODS partner_of IMPORTING is_key TYPE lcl_src=>ty_key
                       RETURNING VALUE(rv) TYPE bu_partner.
ENDCLASS.

CLASS lcl_eng IMPLEMENTATION.

  METHOD constructor.
    mv_scen = iv_scen.
    mt_col  = lcl_map=>for( iv_scen ).
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
    IF iv_col < 1 OR iv_col > lines( cs_row-cells ) OR iv_val IS INITIAL.
      RETURN.
    ENDIF.
    READ TABLE cs_row-cells ASSIGNING FIELD-SYMBOL(<lv>) INDEX iv_col.
    IF sy-subrc = 0.
      <lv> = iv_val.
    ENDIF.
  ENDMETHOD.

  METHOD comp.
    FIELD-SYMBOLS <lv> TYPE any.
    ASSIGN COMPONENT iv_fld OF STRUCTURE is_any TO <lv>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    rv = lcl_util=>text( iv_value = <lv> iv_fmt = iv_fmt ).
  ENDMETHOD.

  METHOD partner_of.
    rv = is_key-partner.
    IF rv IS NOT INITIAL.
      RETURN.
    ENDIF.
    IF is_key-kunnr IS NOT INITIAL.
      SELECT SINGLE b~partner FROM but000 AS b
        INNER JOIN cvi_cust_link AS l ON l~partner_guid = b~partner_guid
        WHERE l~customer = @is_key-kunnr INTO @rv.
    ELSEIF is_key-lifnr IS NOT INITIAL.
      SELECT SINGLE b~partner FROM but000 AS b
        INNER JOIN cvi_vend_link AS l ON l~partner_guid = b~partner_guid
        WHERE l~vendor = @is_key-lifnr INTO @rv.
    ENDIF.
  ENDMETHOD.

  METHOD run.
    CLEAR mt_row.
    DATA(lv_cust) = xsdbool( mv_scen(1) = 'C' ).
    DATA(lt_key)  = lcl_src=>keys( lv_cust ).

    IF lt_key IS INITIAL.
      add_msg( iv_key = '' iv_type = 'W'
               iv_text = 'Nothing was found for the numbers given - the file has headings only' ).
      RETURN.
    ENDIF.

    LOOP AT lt_key INTO DATA(ls_key).
      IF lines( mt_row ) >= p_max.
        add_msg( iv_key = '' iv_type = 'W'
                 iv_text = |Stopped at { p_max } row(s) - raise "Rows at most" for more| ).
        EXIT.
      ENDIF.
      CASE mv_scen.
        WHEN 'C5'. cred( ls_key ).
        WHEN OTHERS.
          IF lv_cust = abap_true.
            cust( ls_key ).
          ELSE.
            vend( ls_key ).
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD cust.
    DATA ls_c TYPE cmds_ei_extern.
    TRY.
        lcl_src=>customer( EXPORTING iv_kunnr = is_key-kunnr IMPORTING es_data = ls_c ).
      CATCH lcx_ext INTO DATA(lx).
        DATA(lv_t) = lx->get_text( ).
        add_msg( iv_key = is_key-kunnr iv_type = 'E' iv_text = lv_t ).
        RETURN.
    ENDTRY.

    " Two things the customer interface does not carry: the licence record
    " and the business partner's identification numbers.
    SELECT SINGLE * FROM zsd_license_chk
      WHERE kunnr = @is_key-kunnr INTO @DATA(ls_lic).
    DATA lv_adh TYPE string.
    DATA(lv_bp) = partner_of( is_key ).
    IF lv_bp IS NOT INITIAL.
      SELECT SINGLE idnumber FROM but0id
        WHERE partner = @lv_bp AND type = @gc_id_aadhaar INTO @lv_adh.
    ENDIF.

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
              IF ls_col-fmt = 'TT' AND lv_val IS NOT INITIAL.
                lv_val = lcl_src=>title_text( lv_val ).
              ENDIF.

            WHEN 'M'.
              CASE ls_col-fld.
                WHEN 'TEL' OR 'MOB'.
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
              " The named tabs give the tax category outright, the
              " positional ones an ordinal - which here is simply the nth
              " tax classification the customer has.
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

  METHOD cred.
    DATA(lv_bp) = partner_of( is_key ).
    IF lv_bp IS INITIAL.
      add_msg( iv_key = is_key-kunnr iv_type = 'E'
               iv_text = 'No business partner behind this customer - no credit data to read' ).
      RETURN.
    ENDIF.

    SELECT credit_sgmnt, credit_limit, xblocked FROM ukmbp_cms_sgm
      WHERE partner = @lv_bp
      ORDER BY credit_sgmnt INTO TABLE @DATA(lt_sgm).
    IF lt_sgm IS INITIAL.
      add_msg( iv_key = is_key-kunnr iv_type = 'W'
               iv_text = 'The customer has no credit segment data' ).
      RETURN.
    ENDIF.

    SELECT SINGLE risk_class FROM ukmbp_cms
      WHERE partner = @lv_bp INTO @DATA(lv_risk).

    " The main segment carries the total limit across all control areas.
    DATA lv_main TYPE string.
    LOOP AT lt_sgm INTO DATA(ls_m) WHERE credit_sgmnt = '0000'.
      lv_main = lcl_util=>text( iv_value = ls_m-credit_limit iv_fmt = 'NM' ).
      EXIT.
    ENDLOOP.

    " The three master fields the credit tab also carries.
    SELECT SINGLE zterm, vzskz FROM knb1
      WHERE kunnr = @is_key-kunnr INTO @DATA(ls_b1).
    SELECT SINGLE kvgr3 FROM knvv
      WHERE kunnr = @is_key-kunnr INTO @DATA(lv_kvgr3).

    LOOP AT lt_sgm INTO DATA(ls_s) WHERE credit_sgmnt <> '0000'.
      IF lines( mt_row ) >= p_max.
        EXIT.
      ENDIF.

      SELECT SINGLE kkber FROM ukm_kkber2sgm
        WHERE credit_sgmnt = @ls_s-credit_sgmnt INTO @DATA(lv_kkber).
      SELECT SINGLE currency FROM ukmcred_sgm0c
        WHERE credit_sgmnt = @ls_s-credit_sgmnt INTO @DATA(lv_cur).

      DATA(ls_row) = empty_row( ).
      LOOP AT mt_col INTO DATA(ls_col).
        DATA lv_val TYPE string.
        CLEAR lv_val.
        CASE ls_col-node.
          WHEN 'K'.
            IF ls_col-fld = 'KUNNR'.
              lv_val = lcl_util=>text( iv_value = is_key-kunnr iv_fmt = 'AL' ).
            ENDIF.
          WHEN 'U'.
            CASE ls_col-fld.
              WHEN 'SEGMENT'.    lv_val = lv_kkber.
              WHEN 'LIMIT_MAIN'. lv_val = lv_main.
              WHEN 'LIMIT_SGM'.  lv_val = lcl_util=>text( iv_value = ls_s-credit_limit iv_fmt = 'NM' ).
              WHEN 'CURRENCY'.   lv_val = lv_cur.
              WHEN 'RISK_CLASS'. lv_val = lv_risk.
              WHEN 'XBLOCKED'.   lv_val = ls_s-xblocked.
            ENDCASE.
          WHEN 'B'.
            CASE ls_col-fld.
              WHEN 'ZTERM'. lv_val = ls_b1-zterm.
              WHEN 'VZSKZ'. lv_val = ls_b1-vzskz.
            ENDCASE.
          WHEN 'S'.
            IF ls_col-fld = 'KVGR3'.
              lv_val = lv_kvgr3.
            ENDIF.
        ENDCASE.
        put( EXPORTING iv_col = ls_col-col iv_val = lv_val CHANGING cs_row = ls_row ).
      ENDLOOP.

      APPEND ls_row TO mt_row.
      add_msg( iv_key = is_key-kunnr iv_type = 'S'
               iv_text = |Row { lines( mt_row ) }: credit control area { lv_kkber } | &&
                         |(segment { ls_s-credit_sgmnt })| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD vend.
    DATA ls_v TYPE vmds_ei_extern.
    TRY.
        lcl_src=>vendor( EXPORTING iv_lifnr = is_key-lifnr IMPORTING es_data = ls_v ).
      CATCH lcx_ext INTO DATA(lx).
        DATA(lv_t) = lx->get_text( ).
        add_msg( iv_key = is_key-lifnr iv_type = 'E' iv_text = lv_t ).
        RETURN.
    ENDTRY.

    DATA(lt_comp) = ls_v-company_data-company.
    DATA(lt_pur)  = ls_v-purchasing_data-purchasing.
    DATA(lt_bank) = ls_v-central_data-bankdetail-bankdetails.
    DATA ls_comp TYPE vmds_ei_company.
    DATA ls_pur  TYPE vmds_ei_purchasing.

    " The bank master behind the vendor's first bank detail - the only
    " source for the bank-key tab, which the supplier interface does not
    " carry at all.
    DATA ls_bnka TYPE bnka.
    DATA ls_bk1  TYPE cvis_ei_cvi_bankdetail.
    READ TABLE lt_bank INTO ls_bk1 INDEX 1.
    IF sy-subrc = 0.
      lcl_src=>bank_master( EXPORTING iv_banks = CONV banks( ls_bk1-data_key-banks )
                                      iv_bankl = CONV bankl( ls_bk1-data_key-bankl )
                            IMPORTING es_bnka  = ls_bnka ).
    ENDIF.

    " Telephone, mobile, fax and e-mail, in the order they are held, so a
    " column asking for "the second one" gets the second one.
    DATA: lt_teln TYPE string_table,
          lt_telx TYPE string_table,
          lt_mobn TYPE string_table,
          lt_faxn TYPE string_table,
          lt_smtp TYPE string_table.
    LOOP AT ls_v-central_data-address-communication-phone-phone INTO DATA(ls_ph).
      " R_3_USER carries data element AD_FLGMOB - a flag, not an ordinal.
      IF ls_ph-contact-data-r_3_user = abap_true.
        APPEND CONV string( ls_ph-contact-data-telephone ) TO lt_mobn.
      ELSE.
        APPEND CONV string( ls_ph-contact-data-telephone ) TO lt_teln.
        APPEND CONV string( ls_ph-contact-data-extension ) TO lt_telx.
      ENDIF.
    ENDLOOP.
    LOOP AT ls_v-central_data-address-communication-fax-fax INTO DATA(ls_fx).
      APPEND CONV string( ls_fx-contact-data-fax ) TO lt_faxn.
    ENDLOOP.
    LOOP AT ls_v-central_data-address-communication-smtp-smtp INTO DATA(ls_sm).
      APPEND CONV string( ls_sm-contact-data-e_mail ) TO lt_smtp.
    ENDLOOP.

    DATA lv_ci TYPE i.
    DATA lv_pi TYPE i.
    DATA(lv_cn) = COND i( WHEN lt_comp IS INITIAL THEN 1 ELSE lines( lt_comp ) ).
    DATA(lv_pn) = COND i( WHEN lt_pur  IS INITIAL THEN 1 ELSE lines( lt_pur ) ).

    lv_ci = 1.
    WHILE lv_ci <= lv_cn.
      CLEAR ls_comp.
      READ TABLE lt_comp INTO ls_comp INDEX lv_ci.
      DATA(lt_wt)  = ls_comp-wtax_type-wtax_type.
      DATA(lt_lfbw) = lcl_src=>wtax( iv_lifnr = is_key-lifnr
                                     iv_bukrs = CONV bukrs( ls_comp-data_key-bukrs ) ).
      DATA(lt_tan) = lcl_src=>tan_exem( iv_lifnr = is_key-lifnr
                                        iv_bukrs = CONV bukrs( ls_comp-data_key-bukrs ) ).
      lv_pi = 1.
      WHILE lv_pi <= lv_pn.
        CLEAR ls_pur.
        READ TABLE lt_pur INTO ls_pur INDEX lv_pi.
        DATA(lt_fn) = ls_pur-functions-functions.

        IF lines( mt_row ) >= p_max.
          lv_pi = lv_pn + 1.
          lv_ci = lv_cn + 1.
          EXIT.
        ENDIF.

        DATA(ls_row) = empty_row( ).
        LOOP AT mt_col INTO DATA(ls_col).
          DATA lv_val TYPE string.
          DATA lv_fld TYPE string.
          DATA lv_occ TYPE i.
          CLEAR lv_val.
          split_occ( EXPORTING iv_in  = ls_col-fld
                     IMPORTING ev_fld = lv_fld ev_occ = lv_occ ).

          CASE ls_col-node.
            WHEN 'K'.
              CASE lv_fld.
                WHEN 'LIFNR'.  lv_val = lcl_util=>text( iv_value = is_key-lifnr iv_fmt = 'AL' ).
                WHEN 'BUKRS' OR 'RBUKRS'. lv_val = ls_comp-data_key-bukrs.
                WHEN 'EKORG' OR 'REKORG'. lv_val = ls_pur-data_key-ekorg.
              ENDCASE.

            WHEN 'V'.
              lv_val = comp( is_any = ls_v-central_data-central-data
                             iv_fld = lv_fld iv_fmt = ls_col-fmt ).

            WHEN 'A'.
              lv_val = comp( is_any = ls_v-central_data-address-postal-data
                             iv_fld = lv_fld iv_fmt = ls_col-fmt ).
              IF ls_col-fmt = 'TT' AND lv_val IS NOT INITIAL.
                lv_val = lcl_src=>title_text( lv_val ).
              ENDIF.

            WHEN 'M'.
              CASE lv_fld.
                WHEN 'TEL'.   READ TABLE lt_teln INTO lv_val INDEX 1.
                WHEN 'TELX'.  READ TABLE lt_telx INTO lv_val INDEX 1.
                WHEN 'TEL2'.  READ TABLE lt_teln INTO lv_val INDEX 2.
                WHEN 'TELX2'. READ TABLE lt_telx INTO lv_val INDEX 2.
                WHEN 'MOB'.   READ TABLE lt_mobn INTO lv_val INDEX 1.
                WHEN 'MOB2'.  READ TABLE lt_mobn INTO lv_val INDEX 2.
                WHEN 'FAX'.   READ TABLE lt_faxn INTO lv_val INDEX 1.
                WHEN 'SMT'.   READ TABLE lt_smtp INTO lv_val INDEX 1.
                WHEN 'SMT2'.  READ TABLE lt_smtp INTO lv_val INDEX 2.
              ENDCASE.
              IF sy-subrc <> 0.
                CLEAR lv_val.
              ENDIF.

            WHEN 'B'.
              lv_val = comp( is_any = ls_comp-data iv_fld = lv_fld iv_fmt = ls_col-fmt ).

            WHEN 'P'.
              lv_val = comp( is_any = ls_pur-data iv_fld = lv_fld iv_fmt = ls_col-fmt ).

            WHEN 'N'.
              DATA ls_bk TYPE cvis_ei_cvi_bankdetail.
              CLEAR ls_bk.
              READ TABLE lt_bank INTO ls_bk INDEX lv_occ.
              IF sy-subrc = 0.
                lv_val = comp( is_any = ls_bk-data_key iv_fld = lv_fld iv_fmt = ls_col-fmt ).
                IF lv_val IS INITIAL.
                  lv_val = comp( is_any = ls_bk-data iv_fld = lv_fld iv_fmt = ls_col-fmt ).
                ENDIF.
              ENDIF.

            WHEN 'W'.
              DATA ls_wt TYPE vmds_ei_wtax_type.
              CLEAR ls_wt.
              READ TABLE lt_wt INTO ls_wt INDEX lv_occ.
              IF sy-subrc = 0.
                lv_val = comp( is_any = ls_wt-data_key iv_fld = lv_fld iv_fmt = ls_col-fmt ).
                IF lv_val IS INITIAL.
                  lv_val = comp( is_any = ls_wt-data iv_fld = lv_fld iv_fmt = ls_col-fmt ).
                ENDIF.
              ELSE.
                " The interface returned no tax types - LFBW says whether
                " there are none or the node simply was not filled.
                DATA ls_lfbw TYPE lcl_src=>ty_wtax.
                CLEAR ls_lfbw.
                READ TABLE lt_lfbw INTO ls_lfbw INDEX lv_occ.
                IF sy-subrc = 0.
                  lv_val = comp( is_any = ls_lfbw iv_fld = lv_fld iv_fmt = ls_col-fmt ).
                ENDIF.
              ENDIF.

            WHEN 'F'.
              DATA ls_fn TYPE vmds_ei_functions.
              CLEAR ls_fn.
              READ TABLE lt_fn INTO ls_fn INDEX lv_occ.
              IF sy-subrc = 0.
                lv_val = comp( is_any = ls_fn-data_key iv_fld = lv_fld iv_fmt = ls_col-fmt ).
                IF lv_val IS INITIAL.
                  lv_val = comp( is_any = ls_fn-data iv_fld = lv_fld iv_fmt = 'AL' ).
                ENDIF.
              ENDIF.

            WHEN 'X'.
              DATA ls_tan TYPE lcl_src=>ty_tan.
              CLEAR ls_tan.
              READ TABLE lt_tan INTO ls_tan INDEX lv_occ.
              IF sy-subrc = 0.
                lv_val = comp( is_any = ls_tan iv_fld = lv_fld iv_fmt = ls_col-fmt ).
              ENDIF.

            WHEN 'Y'.
              lv_val = comp( is_any = ls_bnka iv_fld = lv_fld iv_fmt = ls_col-fmt ).
          ENDCASE.

          put( EXPORTING iv_col = ls_col-col iv_val = lv_val CHANGING cs_row = ls_row ).
        ENDLOOP.

        APPEND ls_row TO mt_row.
        add_msg( iv_key = is_key-lifnr iv_type = 'S'
                 iv_text = |Row { lines( mt_row ) }: { ls_comp-data_key-bukrs } { ls_pur-data_key-ekorg }| ).
        lv_pi = lv_pi + 1.
      ENDWHILE.
      lv_ci = lv_ci + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD split_occ.
    " A repeating node is addressed as FIELD#n in the map - the field name
    " on its own means the first occurrence.
    ev_fld = iv_in.
    ev_occ = 1.
    IF iv_in CS '#'.
      SPLIT iv_in AT '#' INTO DATA(lv_f) DATA(lv_n).
      ev_fld = lv_f.
      ev_occ = CONV i( lv_n ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_MAIN - which scenario, build the workbook, write it out
*----------------------------------------------------------------------*
CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS scenario RETURNING VALUE(rv) TYPE char2.
    CLASS-METHODS propose_file.
    CLASS-METHODS run.
  PRIVATE SECTION.
    CLASS-METHODS write
      IMPORTING iv_xstring TYPE xstring
      RAISING   lcx_ext.
    CLASS-METHODS show
      IMPORTING it_msg TYPE tt_msg.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD scenario.
    rv = COND char2(
      WHEN p_c1 = abap_true THEN 'C1' WHEN p_c2 = abap_true THEN 'C2'
      WHEN p_c3 = abap_true THEN 'C3' WHEN p_c4 = abap_true THEN 'C4'
      WHEN p_c5 = abap_true THEN 'C5' WHEN p_c6 = abap_true THEN 'C6'
      WHEN p_c7 = abap_true THEN 'C7'
      WHEN p_v1 = abap_true THEN 'V1' WHEN p_v2 = abap_true THEN 'V2'
      WHEN p_v3 = abap_true THEN 'V3' WHEN p_v4 = abap_true THEN 'V4'
      WHEN p_v5 = abap_true THEN 'V5' WHEN p_v6 = abap_true THEN 'V6'
      WHEN p_v7 = abap_true THEN 'V7' WHEN p_v8 = abap_true THEN 'V8'
      ELSE                       'V9' ).
  ENDMETHOD.

  METHOD propose_file.
    " The file name follows the radio button, so two scenarios never land in
    " the same workbook. Only a change of scenario rewrites it - a folder the
    " user picked in the file dialog is kept, and so is a name they typed,
    " until they move to a different scenario.
    DATA(lv_now) = scenario( ).
    IF lv_now = gv_scen AND p_file IS NOT INITIAL.
      RETURN.
    ENDIF.
    gv_scen = lv_now.

    " Whatever stands in front of the last separator is the folder, and the
    " separator is a backslash on the PC and a slash on the server.
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

    p_file = |{ lv_dir }{ lcl_map=>name( gv_scen ) }.xlsx|.
  ENDMETHOD.

  METHOD write.
    IF p_pc = abap_true.
      DATA(lt_bin) = cl_bcs_convert=>xstring_to_solix( iv_xstring ).
      cl_gui_frontend_services=>gui_download(
        EXPORTING bin_filesize            = xstrlen( iv_xstring )
                  filename                = CONV string( p_file )
                  filetype                = 'BIN'
        CHANGING  data_tab                = lt_bin
        EXCEPTIONS file_write_error        = 1
                   no_batch                = 2
                   gui_refuse_filetransfer = 3
                   invalid_type            = 4
                   no_authority            = 5
                   access_denied           = 6
                   disk_full               = 7
                   file_not_found          = 8
                   not_supported_by_gui    = 9
                   error_no_gui            = 10
                   OTHERS                  = 11 ).
      IF sy-subrc <> 0.
        DATA(lv_why) = SWITCH string( sy-subrc
          WHEN 1  THEN 'the file could not be written - it is open in Excel, or read-only'
          WHEN 2  THEN 'the program is running in the background, where there is no PC to write to'
          WHEN 5  THEN 'no authorisation to write there'
          WHEN 6  THEN 'access denied - the folder does not allow it'
          WHEN 7  THEN 'the disk is full'
          WHEN 8  THEN 'the folder does not exist'
          WHEN 10 THEN 'there is no SAP GUI - write to the application server instead'
          ELSE         |the download failed with reason { sy-subrc }| ).
        RAISE EXCEPTION NEW lcx_ext( |{ p_file }: { lv_why }| ).
      ENDIF.
    ELSE.
      DATA lv_msg TYPE string.
      OPEN DATASET p_file FOR OUTPUT IN BINARY MODE MESSAGE lv_msg.
      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW lcx_ext( |{ p_file } could not be opened on the server: { lv_msg }| ).
      ENDIF.
      TRANSFER iv_xstring TO p_file.
      CLOSE DATASET p_file.
    ENDIF.
  ENDMETHOD.

  METHOD show.
    IF it_msg IS INITIAL.
      RETURN.
    ENDIF.
    DATA lt_msg TYPE tt_msg.
    lt_msg = it_msg.
    DATA lo_alv TYPE REF TO cl_salv_table.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING  t_table      = lt_msg ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->get_columns( )->set_optimize( abap_true ).
        lo_alv->get_columns( )->get_column( 'OBJKEY' )->set_short_text( 'Master rec' ).
        lo_alv->display( ).
      CATCH cx_salv_msg cx_salv_not_found.
        LOOP AT lt_msg INTO DATA(ls_m).
          WRITE: / ls_m-objkey, ls_m-message.
        ENDLOOP.
    ENDTRY.
  ENDMETHOD.

  METHOD run.
    DATA(lv_scen) = scenario( ).
    DATA(lo_eng)  = NEW lcl_eng( lv_scen ).

    DATA lt_row TYPE tt_row.
    IF p_blank = abap_false.
      lo_eng->run( ).
      lt_row = lo_eng->rows( ).
    ENDIF.

    DATA(lt_head) = lo_eng->head( ).

    TRY.
        DATA(lv_x) = lcl_xlsx=>build( iv_sheet = lcl_map=>sheet( lv_scen )
                                      it_head  = lt_head
                                      it_row   = lt_row ).
        write( lv_x ).
      CATCH lcx_ext INTO DATA(lx).
        DATA(lv_t) = lx->get_text( ).
        MESSAGE lv_t TYPE 'E'.
    ENDTRY.

    DATA lv_sum TYPE string.
    lv_sum = |{ lcl_map=>sheet( lv_scen ) }: { lines( lt_head ) } column(s), | &&
             |{ lines( lt_row ) } data row(s) written to { p_file }|.
    MESSAGE lv_sum TYPE 'S'.
    show( lo_eng->log( ) ).
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Selection-screen events
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_main=>propose_file( ).

AT SELECTION-SCREEN OUTPUT.
  " Show the file name that belongs to the scenario now selected. The radio
  " button group carries USER-COMMAND, so a click comes straight back here.
  lcl_main=>propose_file( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: lv_path TYPE string,
        lv_name TYPE string,
        lv_full TYPE string.
  cl_gui_frontend_services=>file_save_dialog(
    EXPORTING window_title      = 'Save the sample workbook'
              default_extension = 'xlsx'
              default_file_name = |{ lcl_map=>name( lcl_main=>scenario( ) ) }.xlsx|
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
  " when the user picks a scenario and presses F8 without a screen refresh.
  lcl_main=>propose_file( ).

  " A radio button click is only that refresh - the user has not asked for
  " anything to be extracted yet, so there is nothing to complain about.
  CHECK sscrfields-ucomm <> 'RB'.

  IF s_bp[] IS INITIAL AND s_kunnr[] IS INITIAL AND s_lifnr[] IS INITIAL
     AND p_blank = abap_false.
    MESSAGE 'Give a business partner, a customer or a supplier - or tick "Headings only"' TYPE 'E'.
  ENDIF.
  IF p_max < 1.
    MESSAGE 'Rows at most must be 1 or more' TYPE 'E'.
  ENDIF.

*----------------------------------------------------------------------*
* Main
*----------------------------------------------------------------------*
START-OF-SELECTION.
  lcl_main=>run( ).
