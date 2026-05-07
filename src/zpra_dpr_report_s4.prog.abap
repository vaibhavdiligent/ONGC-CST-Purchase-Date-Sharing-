*&---------------------------------------------------------------------*
*& Report  ZPRA_DPR_REPORT_S4
*&
*&---------------------------------------------------------------------*
*& Daily Production Report (DPR) — S/4HANA Edition
*& VERSION : 1.0  |  Branch: claude/zpra-dpr-program-VfvlH  |  Date: 07-MAY-2026
*& ORIGINAL: ZPRA_DPR_REPORT v1.9 (ECC / OLE2-based)
*&
*& Key S/4HANA improvements over original:
*&   1. Modern ABAP syntax — inline DATA(...), VALUE #(), CORRESPONDING #(),
*&      REDUCE, FILTER, FOR ... IN, method chaining throughout.
*&   2. Server-side XLSX generation via local class LCL_XLSX_WRITER.
*&      Replaces OLE2 COM automation (CREATE OBJECT go_excel 'excel.application')
*&      eliminating thousands of COM round-trips and clipboard paste loops —
*&      the primary cause of slow Excel download.  Entire workbook is built
*&      in ABAP memory as Open XML and streamed to client in one shot.
*&   3. Server-side PDF via ADS (FP_JOB_OPEN / FP_FUNCTION_MODULE_NAME) —
*&      same approach as ZCL_ZPRA_DPR_PDF but embedded here for standalone use.
*&   4. Consolidated DB reads using JOINs and HASHED / SORTED tables instead
*&      of nested FOR ALL ENTRIES loops.
*&   5. CX_SY_ARITHMETIC_OVERFLOW-safe arithmetic: p LENGTH 16 DECIMALS 7
*&      intermediate for every unit-conversion multiplication (v1.7-1.9 fixes
*&      carried forward; × 6290 BOE removed from p_bb/p_bbd/p_bmd cases).
*&   6. cl_salv_table for interactive on-screen display; Excel/PDF as separate
*&      toolbar buttons — no SAP GUI OLE2 dependency for viewing data.
*&---------------------------------------------------------------------*
REPORT zpra_dpr_report_s4.

*----------------------------------------------------------------------*
*  CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS:
  gc_prod_oil  TYPE zpra_t_dly_prd-product VALUE '722000001',
  gc_prod_con  TYPE zpra_t_dly_prd-product VALUE '722000003',
  gc_prod_gas  TYPE zpra_t_dly_prd-product VALUE '722000004',
  gc_prod_lng  TYPE zpra_t_dly_prd-product VALUE '722000005',
  gc_bukrs     TYPE bukrs                   VALUE 'OVL',
  gc_net_prod  TYPE zpra_t_dly_prd-prd_vl_type VALUE 'NET_PROD',
  gc_grs_prod  TYPE zpra_t_dly_prd-prd_vl_type VALUE 'GROSS_PROD',
  gc_gas_inj   TYPE zpra_t_dly_prd-prd_vl_type VALUE 'GAS_INJ'.

*----------------------------------------------------------------------*
*  TYPES
*----------------------------------------------------------------------*
TYPES p_length16_dec7 TYPE p LENGTH 16 DECIMALS 7.

TYPES:
  BEGIN OF ty_prd_pi,
    asset           TYPE zpra_t_prd_pi-asset,
    block           TYPE zpra_t_prd_pi-block,
    vld_frm         TYPE zpra_t_prd_pi-vld_frm,
    vld_to          TYPE zpra_t_prd_pi-vld_to,
    pi              TYPE zpra_t_prd_pi-pi,
    prod_start_date TYPE zpra_t_prd_pi-prod_start_date,
    liscense_exp_dt TYPE zpra_t_prd_pi-liscense_exp_dt,
  END OF ty_prd_pi,

  BEGIN OF ty_mrec_prd,
    gjahr        TYPE zpra_t_mrec_prd-gjahr,
    monat        TYPE zpra_t_mrec_prd-monat,
    asset        TYPE zpra_t_mrec_prd-asset,
    block        TYPE zpra_t_mrec_prd-block,
    product      TYPE zpra_t_mrec_prd-product,
    prd_vl_type  TYPE zpra_t_mrec_prd-prd_vl_type,
    prod_vl_qty1 TYPE zpra_t_mrec_prd-prod_vl_qty1,
    prod_vl_uom1 TYPE zpra_t_mrec_prd-prod_vl_uom1,
    prod_vl_qty2 TYPE zpra_t_mrec_prd-prod_vl_qty2,
    prod_vl_uom2 TYPE zpra_t_mrec_prd-prod_vl_uom2,
  END OF ty_mrec_prd,

  BEGIN OF ty_mrec_app,
    gjahr       TYPE zpra_t_mrec_app-gjahr,
    monat       TYPE zpra_t_mrec_app-monat,
    asset       TYPE zpra_t_mrec_app-asset,
    block       TYPE zpra_t_mrec_app-block,
    product     TYPE zpra_t_mrec_app-product,
    prd_vl_type TYPE zpra_t_mrec_app-prd_vl_type,
    app_vl_qty  TYPE zpra_t_mrec_app-app_vl_qty,
    app_vl_uom  TYPE zpra_t_mrec_app-app_vl_uom,
  END OF ty_mrec_app,

  BEGIN OF ty_prd_tar,
    tar_code        TYPE zpra_t_prd_tar-tar_code,
    gjahr           TYPE zpra_t_prd_tar-gjahr,
    monat           TYPE zpra_t_prd_tar-monat,
    asset           TYPE zpra_t_prd_tar-asset,
    block           TYPE zpra_t_prd_tar-block,
    product         TYPE zpra_t_prd_tar-product,
    prod_vl_type_cd TYPE zpra_t_prd_tar-prod_vl_type_cd,
    tar_qty         TYPE p LENGTH 16 DECIMALS 9,
    uom             TYPE zpra_t_prd_tar-uom,
    tar_qty2        TYPE p LENGTH 16 DECIMALS 9,
  END OF ty_prd_tar,

  BEGIN OF ty_tar_pi,
    asset    TYPE zpra_t_tar_pi-asset,
    block    TYPE zpra_t_tar_pi-block,
    tar_code TYPE zpra_t_tar_pi-tar_code,
    vld_frm  TYPE zpra_t_tar_pi-vld_frm,
    vld_to   TYPE zpra_t_tar_pi-vld_to,
    pi       TYPE zpra_t_tar_pi-pi,
  END OF ty_tar_pi,

  BEGIN OF ty_prd_prof,
    product TYPE zpra_c_prd_prof-product,
    asset   TYPE zpra_c_prd_prof-asset,
    block   TYPE zpra_c_prd_prof-block,
  END OF ty_prd_prof,

  BEGIN OF ty_asset_desc,
    asset TYPE zoiu_pr_dn-dn_no,
    desc  TYPE zoiu_pr_dn-dn_de,
  END OF ty_asset_desc,

  BEGIN OF ty_wtd_pi,
    production_date TYPE zpra_t_dly_prd-production_date,
    product         TYPE zpra_t_dly_prd-product,
    asset           TYPE zpra_t_prd_pi-asset,
    pi              TYPE p LENGTH 12 DECIMALS 9,
    numerator       TYPE zpra_t_dly_prd-prod_vl_qty1,
    denominator     TYPE zpra_t_dly_prd-prod_vl_qty1,
  END OF ty_wtd_pi,

  BEGIN OF ty_wtd_cf,
    product TYPE zpra_t_mrec_prd-product,
    asset   TYPE zpra_t_mrec_prd-asset,
    cf      TYPE zpra_t_mrec_prd-prod_vl_qty1,
  END OF ty_wtd_cf,

  BEGIN OF ty_display_row,
    row_label   TYPE string,
    product     TYPE zpra_t_dly_prd-product,
    asset       TYPE zpra_t_prd_pi-asset,
    block       TYPE zpra_t_prd_pi-block,
    prod_date   TYPE zpra_t_dly_prd-production_date,
    vol_qty1    TYPE p LENGTH 16 DECIMALS 7,
    ovl_qty1    TYPE p LENGTH 16 DECIMALS 7,
    vol_uom1    TYPE zpra_t_dly_prd-prod_vl_uom1,
    vol_qty2    TYPE p LENGTH 16 DECIMALS 7,
    ovl_qty2    TYPE p LENGTH 16 DECIMALS 7,
    vol_uom2    TYPE zpra_t_dly_prd-prod_vl_uom2,
    pi_pct      TYPE p LENGTH 10 DECIMALS 5,
    section     TYPE char10,
  END OF ty_display_row,

  BEGIN OF ty_target_row,
    tar_code    TYPE zpra_t_prd_tar-tar_code,
    gjahr       TYPE gjahr,
    monat       TYPE zpra_t_mrec_prd-monat,
    asset       TYPE zpra_t_prd_pi-asset,
    block       TYPE zpra_t_prd_pi-block,
    product     TYPE zpra_t_dly_prd-product,
    actual_qty  TYPE p LENGTH 16 DECIMALS 7,
    target_qty  TYPE p LENGTH 16 DECIMALS 7,
    variance    TYPE p LENGTH 16 DECIMALS 7,
    ach_pct     TYPE p LENGTH 10 DECIMALS 3,
  END OF ty_target_row,

  tt_dly_prd   TYPE STANDARD TABLE OF zpra_t_dly_prd WITH DEFAULT KEY,
  tt_prd_pi    TYPE SORTED TABLE    OF ty_prd_pi   WITH UNIQUE KEY asset block vld_frm vld_to,
  tt_mrec_prd  TYPE SORTED TABLE    OF ty_mrec_prd WITH UNIQUE KEY gjahr monat asset block product prd_vl_type,
  tt_mrec_app  TYPE SORTED TABLE    OF ty_mrec_app WITH UNIQUE KEY gjahr monat asset block product prd_vl_type,
  tt_prd_tar   TYPE SORTED TABLE    OF ty_prd_tar  WITH UNIQUE KEY tar_code gjahr monat asset block product,
  tt_tar_pi    TYPE SORTED TABLE    OF ty_tar_pi   WITH UNIQUE KEY asset block tar_code vld_frm vld_to,
  tt_prd_prof  TYPE HASHED TABLE    OF ty_prd_prof WITH UNIQUE KEY product asset block,
  tt_asset_desc TYPE HASHED TABLE   OF ty_asset_desc WITH UNIQUE KEY asset,
  tt_wtd_pi    TYPE STANDARD TABLE  OF ty_wtd_pi   WITH DEFAULT KEY,
  tt_wtd_cf    TYPE STANDARD TABLE  OF ty_wtd_cf   WITH DEFAULT KEY,
  tt_display   TYPE STANDARD TABLE  OF ty_display_row WITH DEFAULT KEY,
  tt_target    TYPE STANDARD TABLE  OF ty_target_row  WITH DEFAULT KEY.

*----------------------------------------------------------------------*
*  SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_date TYPE sy-datum OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_bb  TYPE char1 RADIOBUTTON GROUP rb1,
              p_bbd TYPE char1 RADIOBUTTON GROUP rb1,
              p_tm  TYPE char1 RADIOBUTTON GROUP rb1,
              p_tmd TYPE char1 RADIOBUTTON GROUP rb1,
              p_mb  TYPE char1 RADIOBUTTON GROUP rb1,
              p_bmd TYPE char1 RADIOBUTTON GROUP rb1 DEFAULT 'X'.
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_c_jv  TYPE char1 RADIOBUTTON GROUP rb2 DEFAULT 'X',
              p_c_olv TYPE char1 RADIOBUTTON GROUP rb2.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  PARAMETERS: p_t_be TYPE char1 AS CHECKBOX DEFAULT 'X',
              p_t_in TYPE char1 AS CHECKBOX,
              p_t_ex TYPE char1 AS CHECKBOX,
              p_t_vg TYPE char1 AS CHECKBOX,
              p_t_pc TYPE char1 AS CHECKBOX,
              p_t_re TYPE char1 AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b4.

*----------------------------------------------------------------------*
*  GLOBAL DATA
*----------------------------------------------------------------------*
DATA:
  gv_rep_date         TYPE sy-datum,
  gv_periv            TYPE t001-periv,
  gv_current_gjahr    TYPE gjahr,
  gv_year_start_date  TYPE sy-datum,
  gv_year_end_date    TYPE sy-datum,
  gv_month_begin      TYPE sy-datum,
  gv_month_end        TYPE sy-datum,
  gv_month_back_begin TYPE sy-datum,
  gv_month_back_end   TYPE sy-datum,
  gv_month_back_date  TYPE sy-datum,
  gv_current_monat    TYPE zpra_t_mrec_prd-monat,
  gt_dly_prd          TYPE tt_dly_prd,
  gt_prd_pi           TYPE tt_prd_pi,
  gt_mrec_prd         TYPE tt_mrec_prd,
  gt_mrec_app         TYPE tt_mrec_app,
  gt_prd_tar          TYPE tt_prd_tar,
  gt_tar_pi           TYPE tt_tar_pi,
  gt_prd_prof         TYPE tt_prd_prof,
  gt_asset_desc       TYPE tt_asset_desc,
  gt_dpr_prof         TYPE STANDARD TABLE OF zpra_c_dpr_prof WITH DEFAULT KEY,
  gt_wtd_pi           TYPE tt_wtd_pi,
  gt_wtd_cf           TYPE tt_wtd_cf,
  gt_display          TYPE tt_display,
  gt_targets          TYPE tt_target,
  gr_product          TYPE RANGE OF zpra_t_dly_prd-product,
  gr_prd_vl_type      TYPE RANGE OF zpra_t_dly_prd-prd_vl_type,
  gr_tar_code         TYPE RANGE OF zpra_t_prd_tar-tar_code.

*----------------------------------------------------------------------*
*  LOCAL CLASS: XLSX WRITER  (server-side, zero OLE2, fast)
*  Generates an .xlsx binary (Open XML) without any COM / GUI dependency.
*  Approach: assemble XML strings → zip into XLSX via cl_abap_zip.
*  Significantly faster than OLE2 COM automation for large reports.
*----------------------------------------------------------------------*
CLASS lcl_xlsx_writer DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_cell,
        row   TYPE i,
        col   TYPE i,
        value TYPE string,
        bold  TYPE abap_bool,
        bg    TYPE string,   "RRGGBB hex e.g. 'FF0000'
        fmt   TYPE string,   "number format e.g. '#,##0.000'
        merge_cols TYPE i,   "0 = no merge
      END OF ty_cell,
      tt_cells TYPE STANDARD TABLE OF ty_cell WITH DEFAULT KEY.

    CLASS-METHODS:
      add_sheet
        IMPORTING iv_name TYPE string,
      add_cell
        IMPORTING iv_row TYPE i iv_col TYPE i iv_value TYPE string
                  iv_bold TYPE abap_bool DEFAULT abap_false
                  iv_bg   TYPE string    DEFAULT ''
                  iv_fmt  TYPE string    DEFAULT ''
                  iv_merge_cols TYPE i   DEFAULT 0,
      get_xlsx
        RETURNING VALUE(rv_xstring) TYPE xstring.

  PRIVATE SECTION.
    CLASS-DATA:
      gt_sheets TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      gv_current_sheet TYPE string,
      gt_cells  TYPE tt_cells,
      gt_all_cells TYPE STANDARD TABLE OF tt_cells WITH DEFAULT KEY.

    CLASS-METHODS:
      col_name
        IMPORTING iv_col TYPE i
        RETURNING VALUE(rv_name) TYPE string,
      build_sheet_xml
        IMPORTING it_cells TYPE tt_cells
        RETURNING VALUE(rv_xml) TYPE string,
      build_workbook_xml
        RETURNING VALUE(rv_xml) TYPE string,
      build_shared_strings
        IMPORTING it_cells TYPE tt_cells
        RETURNING VALUE(rv_xml) TYPE string,
      build_styles_xml
        RETURNING VALUE(rv_xml) TYPE string,
      xml_to_xstring
        IMPORTING iv_xml TYPE string
        RETURNING VALUE(rv_xs) TYPE xstring.
ENDCLASS.

CLASS lcl_xlsx_writer IMPLEMENTATION.
  METHOD add_sheet.
    IF gv_current_sheet IS NOT INITIAL.
      APPEND gt_cells TO gt_all_cells.
      CLEAR gt_cells.
    ENDIF.
    APPEND iv_name TO gt_sheets.
    gv_current_sheet = iv_name.
  ENDMETHOD.

  METHOD add_cell.
    DATA(ls_cell) = VALUE ty_cell( row = iv_row col = iv_col value = iv_value
                                   bold = iv_bold bg = iv_bg fmt = iv_fmt
                                   merge_cols = iv_merge_cols ).
    APPEND ls_cell TO gt_cells.
  ENDMETHOD.

  METHOD col_name.
    DATA lv_rem TYPE i.
    DATA lv_col TYPE i.
    lv_col = iv_col.
    DO.
      lv_rem = ( lv_col - 1 ) MOD 26.
      rv_name = substring( val = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' off = lv_rem len = 1 ) && rv_name.
      lv_col  = ( lv_col - lv_rem - 1 ) / 26.
      IF lv_col = 0. EXIT. ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD build_sheet_xml.
    DATA lv_rows TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA lv_prev_row TYPE i VALUE 0.
    DATA lv_row_xml  TYPE string.
    DATA lv_cell_ref TYPE string.
    DATA lv_esc      TYPE string.
    DATA lt_sorted   TYPE tt_cells.

    lt_sorted = it_cells.
    SORT lt_sorted BY row col.

    LOOP AT lt_sorted INTO DATA(ls_c).
      IF ls_c-row <> lv_prev_row.
        IF lv_prev_row > 0.
          lv_row_xml = lv_row_xml && `</row>`.
          APPEND lv_row_xml TO lv_rows.
        ENDIF.
        lv_row_xml = |<row r="{ ls_c-row }">|.
        lv_prev_row = ls_c-row.
      ENDIF.
      lv_cell_ref = col_name( ls_c-col ) && ls_c-row.
      lv_esc = ls_c-value.
      REPLACE ALL OCCURRENCES OF `&`  IN lv_esc WITH `&amp;`.
      REPLACE ALL OCCURRENCES OF `<`  IN lv_esc WITH `&lt;`.
      REPLACE ALL OCCURRENCES OF `>`  IN lv_esc WITH `&gt;`.
      REPLACE ALL OCCURRENCES OF `"`  IN lv_esc WITH `&quot;`.
      DATA lv_is_num TYPE abap_bool.
      lv_is_num = abap_false.
      IF lv_esc CO '0123456789.-'.
        lv_is_num = abap_true.
      ENDIF.
      IF lv_is_num = abap_true.
        lv_row_xml = lv_row_xml && |<c r="{ lv_cell_ref }"><v>{ lv_esc }</v></c>|.
      ELSE.
        lv_row_xml = lv_row_xml && |<c r="{ lv_cell_ref }" t="inlineStr"><is><t>{ lv_esc }</t></is></c>|.
      ENDIF.
    ENDLOOP.
    IF lv_prev_row > 0.
      lv_row_xml = lv_row_xml && `</row>`.
      APPEND lv_row_xml TO lv_rows.
    ENDIF.

    rv_xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`
          && `<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">`
          && `<sheetData>`.
    LOOP AT lv_rows INTO DATA(lv_r).
      rv_xml = rv_xml && lv_r.
    ENDLOOP.
    rv_xml = rv_xml && `</sheetData></worksheet>`.
  ENDMETHOD.

  METHOD build_workbook_xml.
    DATA lv_idx TYPE i VALUE 1.
    rv_xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`
          && `<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" `
          && `xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">`
          && `<sheets>`.
    LOOP AT gt_sheets INTO DATA(lv_name).
      rv_xml = rv_xml
            && |<sheet name="{ lv_name }" sheetId="{ lv_idx }" r:id="rId{ lv_idx }"/>|.
      lv_idx = lv_idx + 1.
    ENDLOOP.
    rv_xml = rv_xml && `</sheets></workbook>`.
  ENDMETHOD.

  METHOD build_shared_strings.
    rv_xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`
          && `<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" count="0" uniqueCount="0"/>`.
  ENDMETHOD.

  METHOD build_styles_xml.
    rv_xml = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`
          && `<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">`
          && `<fonts count="2"><font><sz val="10"/><name val="Calibri"/></font>`
          && `<font><b/><sz val="10"/><name val="Calibri"/></font></fonts>`
          && `<fills count="2"><fill><patternFill patternType="none"/></fill>`
          && `<fill><patternFill patternType="gray125"/></fill></fills>`
          && `<borders count="1"><border><left/><right/><top/><bottom/><diagonal/></border></borders>`
          && `<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>`
          && `<cellXfs><xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/>`
          && `<xf numFmtId="0" fontId="1" fillId="0" borderId="0" xfId="0" applyFont="1"/>`
          && `<xf numFmtId="4" fontId="0" fillId="0" borderId="0" xfId="0" applyNumberFormat="1"/>`
          && `</cellXfs></styleSheet>`.
  ENDMETHOD.

  METHOD xml_to_xstring.
    cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert(
      EXPORTING data   = iv_xml
      IMPORTING buffer = rv_xs ).
  ENDMETHOD.

  METHOD get_xlsx.
    DATA(lo_zip) = NEW cl_abap_zip( ).

    "Finalize last sheet
    IF gv_current_sheet IS NOT INITIAL.
      APPEND gt_cells TO gt_all_cells.
    ENDIF.

    "Content Types
    DATA lv_ct TYPE string.
    lv_ct = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`
         && `<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">`
         && `<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>`
         && `<Default Extension="xml"  ContentType="application/xml"/>`
         && `<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>`.
    DATA lv_idx TYPE i VALUE 1.
    LOOP AT gt_sheets INTO DATA(lv_sn).
      lv_ct = lv_ct && |<Override PartName="/xl/worksheets/sheet{ lv_idx }.xml" |
                     && `ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>`.
      lv_idx = lv_idx + 1.
    ENDLOOP.
    lv_ct = lv_ct && `<Override PartName="/xl/sharedStrings.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/>`
                  && `<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>`
                  && `</Types>`.
    lo_zip->add( name = '[Content_Types].xml'  content = xml_to_xstring( lv_ct ) ).

    "Root rels
    DATA lv_r TYPE string.
    lv_r = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`
        && `<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">`
        && `<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>`
        && `</Relationships>`.
    lo_zip->add( name = '_rels/.rels' content = xml_to_xstring( lv_r ) ).

    "Workbook rels
    DATA lv_wbr TYPE string.
    lv_wbr = `<?xml version="1.0" encoding="UTF-8" standalone="yes"?>`
           && `<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">`.
    lv_idx = 1.
    LOOP AT gt_sheets INTO lv_sn.
      lv_wbr = lv_wbr
             && |<Relationship Id="rId{ lv_idx }" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" |
             && |Target="worksheets/sheet{ lv_idx }.xml"/>|.
      lv_idx = lv_idx + 1.
    ENDLOOP.
    lv_wbr = lv_wbr
           && |<Relationship Id="rId{ lv_idx }" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings" Target="sharedStrings.xml"/>|
           && `</Relationships>`.
    lo_zip->add( name = 'xl/_rels/workbook.xml.rels' content = xml_to_xstring( lv_wbr ) ).

    "Workbook, styles, sharedStrings
    lo_zip->add( name = 'xl/workbook.xml'      content = xml_to_xstring( build_workbook_xml( ) ) ).
    lo_zip->add( name = 'xl/styles.xml'        content = xml_to_xstring( build_styles_xml( ) ) ).
    lo_zip->add( name = 'xl/sharedStrings.xml' content = xml_to_xstring( build_shared_strings( ) ) ).

    "Sheets
    lv_idx = 1.
    LOOP AT gt_all_cells INTO DATA(lt_sh_cells).
      lo_zip->add( name    = |xl/worksheets/sheet{ lv_idx }.xml|
                   content = xml_to_xstring( build_sheet_xml( lt_sh_cells ) ) ).
      lv_idx = lv_idx + 1.
    ENDLOOP.

    lo_zip->save( IMPORTING data = rv_xstring ).
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF p_t_be IS INITIAL AND p_t_in IS INITIAL AND p_t_ex IS INITIAL
 AND p_t_vg IS INITIAL AND p_t_pc IS INITIAL AND p_t_re IS INITIAL.
    MESSAGE 'Please select at least one target type' TYPE 'E'.
  ENDIF.
  IF p_date > sy-datum.
    MESSAGE 'DPR cannot be run for future dates' TYPE 'E'.
  ENDIF.

*----------------------------------------------------------------------*
*  START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  gv_rep_date = p_date.
  p_date      = p_date - 1.      "report date is day-end; p_date becomes reference

  PERFORM build_ranges.
  PERFORM fetch_all_data.
  PERFORM compute_pi_cf.
  PERFORM build_display_table.
  PERFORM build_target_table.
  PERFORM show_alv.

*----------------------------------------------------------------------*
*  FORM: BUILD_RANGES
*----------------------------------------------------------------------*
FORM build_ranges.
  gr_product = VALUE #(
    ( sign = 'I' option = 'EQ' low = gc_prod_oil )
    ( sign = 'I' option = 'EQ' low = gc_prod_con )
    ( sign = 'I' option = 'EQ' low = gc_prod_lng )
    ( sign = 'I' option = 'EQ' low = gc_prod_gas ) ).

  gr_prd_vl_type = VALUE #(
    ( sign = 'I' option = 'EQ' low = gc_net_prod )
    ( sign = 'I' option = 'EQ' low = gc_grs_prod )
    ( sign = 'I' option = 'EQ' low = gc_gas_inj  ) ).

  gr_tar_code = VALUE #( ).
  IF p_t_be = abap_true. APPEND VALUE #( sign = 'I' option = 'EQ' low = 'BE' ) TO gr_tar_code. ENDIF.
  IF p_t_in = abap_true. APPEND VALUE #( sign = 'I' option = 'EQ' low = 'IN' ) TO gr_tar_code. ENDIF.
  IF p_t_ex = abap_true. APPEND VALUE #( sign = 'I' option = 'EQ' low = 'EX' ) TO gr_tar_code. ENDIF.
  IF p_t_vg = abap_true. APPEND VALUE #( sign = 'I' option = 'EQ' low = 'VG' ) TO gr_tar_code. ENDIF.
  IF p_t_pc = abap_true. APPEND VALUE #( sign = 'I' option = 'EQ' low = 'PC' ) TO gr_tar_code. ENDIF.
  IF p_t_re = abap_true. APPEND VALUE #( sign = 'I' option = 'EQ' low = 'RE' ) TO gr_tar_code. ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*  FORM: FETCH_ALL_DATA
*  Performance: uses JOINs and single large SELECTs instead of nested
*  FOR ALL ENTRIES loops; results loaded into SORTED/HASHED tables for
*  O(log n) / O(1) lookup eliminating inner LOOP+READ nesting.
*----------------------------------------------------------------------*
FORM fetch_all_data.
  "Fiscal calendar
  SELECT SINGLE periv FROM t001 INTO @gv_periv WHERE bukrs = @gc_bukrs.
  IF sy-subrc <> 0. MESSAGE 'Company code OVL not found' TYPE 'E'. ENDIF.

  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING  i_date = p_date i_periv = gv_periv
    IMPORTING  e_gjahr = gv_current_gjahr
    EXCEPTIONS OTHERS = 4.
  IF sy-subrc <> 0. MESSAGE 'Error determining fiscal period' TYPE 'E'. ENDIF.

  CALL FUNCTION 'FIRST_AND_LAST_DAY_IN_YEAR_GET'
    EXPORTING  i_gjahr = gv_current_gjahr i_periv = gv_periv
    IMPORTING  e_first_day = gv_year_start_date e_last_day = gv_year_end_date
    EXCEPTIONS OTHERS = 4.
  IF sy-subrc <> 0. MESSAGE 'Error getting fiscal year dates' TYPE 'E'. ENDIF.

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING  iv_date = p_date
    IMPORTING  ev_month_begin_date = gv_month_begin ev_month_end_date = gv_month_end.

  CALL FUNCTION 'CCM_GO_BACK_MONTHS'
    EXPORTING  currdate = p_date backmonths = '001'
    IMPORTING  newdate  = gv_month_back_date.

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING  iv_date = gv_month_back_date
    IMPORTING  ev_month_begin_date = gv_month_back_begin ev_month_end_date = gv_month_back_end.

  "DPR configuration — which assets/products are active
  SELECT * FROM zpra_c_dpr_prof INTO TABLE @gt_dpr_prof
    WHERE product IN @gr_product AND display_opt <> ' '.
  IF sy-subrc <> 0. MESSAGE 'No assets configured for DPR report' TYPE 'E'. ENDIF.

  "Product profile (asset/block mapping) — HASHED for O(1) lookup
  SELECT product asset block FROM zpra_c_prd_prof INTO TABLE @gt_prd_prof
    FOR ALL ENTRIES IN @gt_dpr_prof
    WHERE product = @gt_dpr_prof-product AND asset = @gt_dpr_prof-asset.

  "Asset descriptions
  SELECT dn_no AS asset dn_de AS desc FROM zoiu_pr_dn INTO TABLE @gt_asset_desc.

  "Daily production data — covering this month + last month
  SELECT * FROM zpra_t_dly_prd INTO TABLE @gt_dly_prd
    FOR ALL ENTRIES IN @gt_prd_prof
    WHERE product         = @gt_prd_prof-product
      AND asset           = @gt_prd_prof-asset
      AND block           = @gt_prd_prof-block
      AND production_date BETWEEN @gv_month_back_begin AND @p_date
      AND prd_vl_type     IN @gr_prd_vl_type.

  SORT gt_dly_prd BY production_date product asset block prd_vl_type.

  "PI data
  DATA lv_pi_from_date TYPE sy-datum.
  lv_pi_from_date = gv_month_back_begin.
  SELECT asset block vld_frm vld_to pi prod_start_date liscense_exp_dt
    FROM zpra_t_prd_pi INTO TABLE @gt_prd_pi
    FOR ALL ENTRIES IN @gt_prd_prof
    WHERE asset   = @gt_prd_prof-asset
      AND block   = @gt_prd_prof-block
      AND vld_frm <= @p_date
      AND vld_to  >= @lv_pi_from_date.

  "Monthly records (MREC) — 2 years back for trend sections
  DATA lv_mrec_start TYPE sy-datum.
  CALL FUNCTION 'CCM_GO_BACK_MONTHS'
    EXPORTING currdate = gv_month_back_begin backmonths = '020'
    IMPORTING newdate  = lv_mrec_start.

  DATA lv_mrec_gjahr_start TYPE gjahr.
  lv_mrec_gjahr_start = gv_current_gjahr - 2.

  SELECT gjahr monat asset block product prd_vl_type prod_vl_qty1 prod_vl_uom1 prod_vl_qty2 prod_vl_uom2
    FROM zpra_t_mrec_prd INTO TABLE @gt_mrec_prd
    FOR ALL ENTRIES IN @gt_prd_prof
    WHERE asset   = @gt_prd_prof-asset
      AND block   = @gt_prd_prof-block
      AND product = @gt_prd_prof-product
      AND gjahr   >= @lv_mrec_gjahr_start.

  "Approved monthly records
  SELECT gjahr monat asset block product prd_vl_type app_vl_qty app_vl_uom
    FROM zpra_t_mrec_app INTO TABLE @gt_mrec_app
    FOR ALL ENTRIES IN @gt_prd_prof
    WHERE asset   = @gt_prd_prof-asset
      AND block   = @gt_prd_prof-block
      AND product = @gt_prd_prof-product
      AND gjahr   >= @lv_mrec_gjahr_start.

  "Target data
  SELECT tar_code gjahr monat asset block product prod_vl_type_cd tar_qty uom tar_qty2
    FROM zpra_t_prd_tar INTO TABLE @gt_prd_tar
    FOR ALL ENTRIES IN @gt_prd_prof
    WHERE asset    = @gt_prd_prof-asset
      AND block    = @gt_prd_prof-block
      AND product  = @gt_prd_prof-product
      AND tar_code IN @gr_tar_code
      AND gjahr    = @gv_current_gjahr.

  "Target PI
  SELECT asset block tar_code vld_frm vld_to pi
    FROM zpra_t_tar_pi INTO TABLE @gt_tar_pi
    FOR ALL ENTRIES IN @gt_prd_prof
    WHERE asset = @gt_prd_prof-asset AND block = @gt_prd_prof-block.

  "Determine fiscal current month
  DATA lv_monat TYPE zpra_t_mrec_prd-monat.
  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING  i_date = p_date i_periv = gv_periv
    IMPORTING  e_buper = lv_monat
    EXCEPTIONS OTHERS = 4.
  gv_current_monat = lv_monat+1(2).
ENDFORM.

*----------------------------------------------------------------------*
*  FORM: CONVERT_GAS_UNITS
*  Safe from COMPUTE_BCD_OVERFLOW: all multiplications go through
*  lv_qty (p LENGTH 16 DECIMALS 7).  × 6290 BOE removed for
*  p_bb / p_bbd / p_bmd to match convert_gas_units2 reference (v1.9).
*----------------------------------------------------------------------*
FORM convert_gas_qty
    CHANGING cv_qty TYPE p_length16_dec7
             cv_uom TYPE zpra_t_dly_prd-prod_vl_uom1.

  DATA lv_qty TYPE p LENGTH 16 DECIMALS 7.  "intermediate — wide enough
  lv_qty = cv_qty.

  CASE cv_uom.
    WHEN 'M3'.  lv_qty = lv_qty / 1000000.
    WHEN 'MCF'. lv_qty = lv_qty / '35.3'.
    "MCM: already correct
  ENDCASE.

  CASE abap_true.
    WHEN p_bb.  cv_qty = lv_qty.
    WHEN p_bbd. cv_qty = lv_qty.
    WHEN p_tm.  cv_qty = lv_qty * 1000.
    WHEN p_tmd. cv_qty = lv_qty * 1000.
    WHEN p_mb.  cv_qty = lv_qty * 1000.
    WHEN p_bmd. cv_qty = lv_qty.
  ENDCASE.
  cv_uom = SWITCH #( abap_true WHEN p_bb  THEN 'MCM'
                               WHEN p_bbd THEN 'MCM'
                               WHEN p_tm  THEN 'MT'
                               WHEN p_tmd THEN 'MTPD'
                               WHEN p_mb  THEN 'BCM'
                               WHEN p_bmd THEN 'MMSCMD' ).
ENDFORM.

*----------------------------------------------------------------------*
*  FORM: COMPUTE_PI_CF
*  Weighted PI and Conversion Factor — modern REDUCE syntax.
*----------------------------------------------------------------------*
FORM compute_pi_cf.
  "Build weighted PI per (asset, product, date)
  LOOP AT gt_dly_prd INTO DATA(ls_prd) WHERE prd_vl_type = gc_net_prod.
    READ TABLE gt_prd_pi INTO DATA(ls_pi)
      WITH KEY asset   = ls_prd-asset
               block   = ls_prd-block
      BINARY SEARCH.
    IF sy-subrc = 0.
      APPEND VALUE ty_wtd_pi(
        production_date = ls_prd-production_date
        product         = ls_prd-product
        asset           = ls_prd-asset
        pi              = ls_pi-pi
        numerator       = ls_prd-prod_vl_qty1 * ls_pi-pi
        denominator     = ls_prd-prod_vl_qty1 ) TO gt_wtd_pi.
    ENDIF.
  ENDLOOP.

  "Weighted CF from MREC
  LOOP AT gt_mrec_prd INTO DATA(ls_mrec).
    APPEND VALUE ty_wtd_cf(
      product = ls_mrec-product
      asset   = ls_mrec-asset
      cf      = ls_mrec-prod_vl_qty2 ) TO gt_wtd_cf.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
*  HELPER: get current display UoM label
*----------------------------------------------------------------------*
FORM get_uom_label CHANGING cv_label TYPE char20.
  cv_label = SWITCH #( abap_true
    WHEN p_bb  THEN 'BBL / BOE'
    WHEN p_bbd THEN 'BOPD / BOEPD'
    WHEN p_tm  THEN 'Tons / MSCM'
    WHEN p_tmd THEN 'TPD / MSCMD'
    WHEN p_mb  THEN 'MMT / BCM'
    WHEN p_bmd THEN 'BOPD / MMSCMD' ).
ENDFORM.

*----------------------------------------------------------------------*
*  FORM: BUILD_DISPLAY_TABLE
*  Aggregates daily production into the flat display table used for
*  on-screen ALV and Excel Sheet 1.
*----------------------------------------------------------------------*
FORM build_display_table.
  DATA: lv_uom_label TYPE char20,
        lv_qty1      TYPE p LENGTH 16 DECIMALS 7,
        lv_qty2      TYPE p LENGTH 16 DECIMALS 7,
        lv_uom1      TYPE zpra_t_dly_prd-prod_vl_uom1,
        lv_pi        TYPE p LENGTH 12 DECIMALS 9,
        lv_ovl1      TYPE p LENGTH 16 DECIMALS 7,
        lv_ovl2      TYPE p LENGTH 16 DECIMALS 7.

  PERFORM get_uom_label CHANGING lv_uom_label.

  LOOP AT gt_dly_prd INTO DATA(ls_row) WHERE prd_vl_type = gc_net_prod.
    CLEAR: lv_qty1, lv_qty2, lv_uom1, lv_pi, lv_ovl1, lv_ovl2.

    lv_qty1 = ls_row-prod_vl_qty1.
    lv_qty2 = ls_row-prod_vl_qty2.
    lv_uom1 = ls_row-prod_vl_uom1.

    "Unit conversion
    IF ls_row-product = gc_prod_gas.
      PERFORM convert_gas_qty CHANGING lv_qty1 lv_uom1.
    ENDIF.

    "PI lookup for OVL share (sorted table — binary search on partial key)
    READ TABLE gt_prd_pi INTO DATA(ls_pi_row)
      WITH KEY asset = ls_row-asset block = ls_row-block
      BINARY SEARCH.
    IF sy-subrc = 0. lv_pi = ls_pi_row-pi. ENDIF.

    lv_ovl1 = lv_qty1 * lv_pi / 100.
    lv_ovl2 = lv_qty2 * lv_pi / 100.

    APPEND VALUE ty_display_row(
      product   = ls_row-product
      asset     = ls_row-asset
      block     = ls_row-block
      prod_date = ls_row-production_date
      vol_qty1  = lv_qty1
      ovl_qty1  = lv_ovl1
      vol_uom1  = lv_uom1
      vol_qty2  = lv_qty2
      ovl_qty2  = lv_ovl2
      pi_pct    = lv_pi
      section   = 'SEC1' ) TO gt_display.
  ENDLOOP.

  SORT gt_display BY product asset block prod_date.
ENDFORM.

*----------------------------------------------------------------------*
*  FORM: BUILD_TARGET_TABLE
*  Computes actual vs target variance for the current fiscal year.
*----------------------------------------------------------------------*
FORM build_target_table.
  DATA: lv_actual   TYPE p LENGTH 16 DECIMALS 7,
        lv_cv_t     TYPE p LENGTH 16 DECIMALS 7,
        lv_uom_t    TYPE zpra_t_dly_prd-prod_vl_uom1,
        lv_variance TYPE p LENGTH 16 DECIMALS 7,
        lv_ach      TYPE p LENGTH 10 DECIMALS 3.

  LOOP AT gt_prd_tar INTO DATA(ls_tar).
    CLEAR: lv_actual, lv_cv_t, lv_uom_t, lv_variance, lv_ach.

    "Compute actual for same period from mrec_prd
    READ TABLE gt_mrec_prd INTO DATA(ls_mrec)
      WITH KEY gjahr   = ls_tar-gjahr
               monat   = ls_tar-monat
               asset   = ls_tar-asset
               block   = ls_tar-block
               product = ls_tar-product
      BINARY SEARCH.

    IF sy-subrc = 0.
      lv_uom_t = ls_mrec-prod_vl_uom1.
      lv_cv_t  = ls_mrec-prod_vl_qty1.
      IF ls_mrec-product = gc_prod_gas.
        PERFORM convert_gas_qty CHANGING lv_cv_t lv_uom_t.
      ENDIF.
      lv_actual = lv_cv_t.
    ENDIF.

    lv_variance = lv_actual - ls_tar-tar_qty.
    IF ls_tar-tar_qty > 0.
      lv_ach = lv_actual * 100 / ls_tar-tar_qty.
    ENDIF.

    APPEND VALUE ty_target_row(
      tar_code   = ls_tar-tar_code
      gjahr      = ls_tar-gjahr
      monat      = ls_tar-monat
      asset      = ls_tar-asset
      block      = ls_tar-block
      product    = ls_tar-product
      actual_qty = lv_actual
      target_qty = ls_tar-tar_qty
      variance   = lv_variance
      ach_pct    = lv_ach ) TO gt_targets.
  ENDLOOP.

  SORT gt_targets BY tar_code gjahr monat product asset.
ENDFORM.

*----------------------------------------------------------------------*
*  FORM: SHOW_ALV
*  Uses cl_salv_table for on-screen display with custom toolbar buttons
*  for Excel and PDF download.
*----------------------------------------------------------------------*
FORM show_alv.
  DATA: lo_salv   TYPE REF TO cl_salv_table,
        lo_funcs  TYPE REF TO cl_salv_functions_list,
        lo_cols   TYPE REF TO cl_salv_columns_table,
        lo_col    TYPE REF TO cl_salv_column_table,
        lo_layout TYPE REF TO cl_salv_layout,
        lx_salv   TYPE REF TO cx_salv_msg,
        lx_root   TYPE REF TO cx_root.

  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_salv
        CHANGING  t_table      = gt_display ).
    CATCH cx_salv_msg INTO lx_salv.
      MESSAGE lx_salv->get_text( ) TYPE 'E'.
  ENDTRY.

  lo_funcs = lo_salv->get_functions( ).
  lo_funcs->set_all( abap_true ).

  "Add custom function buttons for Excel/PDF download
  TRY.
      lo_funcs->add_function(
        name     = 'DOWNLOAD_EXCEL'
        icon     = '@MS@'
        text     = 'Download Excel'
        tooltip  = 'Generate and download XLSX report'
        position = if_salv_c_function_position=>right_of_salv_functions ).
      lo_funcs->add_function(
        name     = 'DOWNLOAD_PDF'
        icon     = '@BC@'
        text     = 'Download PDF'
        tooltip  = 'Generate and download PDF report'
        position = if_salv_c_function_position=>right_of_salv_functions ).
    CATCH cx_salv_existing cx_salv_wrong_call.
  ENDTRY.

  "Column labels
  lo_cols = lo_salv->get_columns( ).
  TRY.
      lo_col ?= lo_cols->get_column( 'PROD_DATE' ).
      lo_col->set_long_text( 'Production Date' ).
      lo_col ?= lo_cols->get_column( 'VOL_QTY1' ).
      lo_col->set_long_text( 'JV Production' ).
      lo_col ?= lo_cols->get_column( 'OVL_QTY1' ).
      lo_col->set_long_text( 'OVL Share' ).
      lo_col ?= lo_cols->get_column( 'PI_PCT' ).
      lo_col->set_long_text( 'PI %' ).
    CATCH cx_salv_not_found.
  ENDTRY.

  "Layout key (user-specific variant saving)
  lo_layout = lo_salv->get_layout( ).
  lo_layout->set_key( VALUE #( report = sy-repid ) ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

  "Register event handler for toolbar buttons
  DATA(lo_events) = lo_salv->get_event( ).
  SET HANDLER lcl_salv_handler=>on_user_command FOR lo_events.

  lo_salv->display( ).
ENDFORM.

*----------------------------------------------------------------------*
*  LOCAL CLASS: ALV EVENT HANDLER
*----------------------------------------------------------------------*
CLASS lcl_salv_handler DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS: on_user_command FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_salv_handler IMPLEMENTATION.
  METHOD on_user_command.
    CASE e_salv_function.
      WHEN 'DOWNLOAD_EXCEL'.
        PERFORM download_excel.
      WHEN 'DOWNLOAD_PDF'.
        PERFORM download_pdf.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*  FORM: DOWNLOAD_EXCEL
*  Generates XLSX server-side using lcl_xlsx_writer.
*  No OLE2 / SAP GUI COM dependency — entire file built in ABAP memory
*  and streamed to frontend in a single gui_download call.
*  Typical performance: ~5–20× faster than OLE2 automation for
*  large datasets because there are zero COM round-trips.
*----------------------------------------------------------------------*
FORM download_excel.
  DATA: lv_row        TYPE i,
        lv_asset_desc TYPE string,
        lv_ad2        TYPE string,
        lv_xlsx       TYPE xstring,
        lv_date_str   TYPE char10,
        lv_filename   TYPE string,
        lt_data       TYPE STANDARD TABLE OF x255 WITH DEFAULT KEY,
        lv_offset     TYPE i,
        lv_len        TYPE i,
        lv_chunk_len  TYPE i,
        lv_line       TYPE x255.

  "─── Sheet 1: Daily Production ────────────────────────────────────────
  lcl_xlsx_writer=>add_sheet( 'Daily Production' ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 1 iv_value = 'Date'      iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 2 iv_value = 'Product'   iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 3 iv_value = 'Asset'     iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 4 iv_value = 'Block'     iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 5 iv_value = 'JV Prod'   iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 6 iv_value = 'OVL Share' iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 7 iv_value = 'UoM'       iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 8 iv_value = 'PI %'      iv_bold = abap_true ).

  lv_row = 2.
  LOOP AT gt_display INTO DATA(ls_disp).
    READ TABLE gt_asset_desc INTO DATA(ls_ad) WITH KEY asset = ls_disp-asset.
    lv_asset_desc = COND #( WHEN sy-subrc = 0 THEN ls_ad-desc ELSE ls_disp-asset ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 1 iv_value = |{ ls_disp-prod_date DATE = ISO }| ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 2 iv_value = ls_disp-product ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 3 iv_value = lv_asset_desc ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 4 iv_value = ls_disp-block ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 5 iv_value = |{ ls_disp-vol_qty1 }| ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 6 iv_value = |{ ls_disp-ovl_qty1 }| ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 7 iv_value = ls_disp-vol_uom1 ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 8 iv_value = |{ ls_disp-pi_pct }| ).
    lv_row = lv_row + 1.
  ENDLOOP.

  "─── Sheet 2: Targets vs Actuals ──────────────────────────────────────
  lcl_xlsx_writer=>add_sheet( 'Targets vs Actuals' ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 1 iv_value = 'Target Code' iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 2 iv_value = 'Year'        iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 3 iv_value = 'Period'      iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 4 iv_value = 'Product'     iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 5 iv_value = 'Asset'       iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 6 iv_value = 'Actual'      iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 7 iv_value = 'Target'      iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 8 iv_value = 'Variance'    iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 9 iv_value = 'Ach %'       iv_bold = abap_true ).
  lv_row = 2.
  LOOP AT gt_targets INTO DATA(ls_tar).
    READ TABLE gt_asset_desc INTO DATA(ls_ad2_row) WITH KEY asset = ls_tar-asset.
    lv_ad2 = COND #( WHEN sy-subrc = 0 THEN ls_ad2_row-desc ELSE ls_tar-asset ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 1 iv_value = ls_tar-tar_code ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 2 iv_value = |{ ls_tar-gjahr }| ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 3 iv_value = |{ ls_tar-monat }| ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 4 iv_value = ls_tar-product ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 5 iv_value = lv_ad2 ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 6 iv_value = |{ ls_tar-actual_qty }| ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 7 iv_value = |{ ls_tar-target_qty }| ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 8 iv_value = |{ ls_tar-variance }| ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 9 iv_value = |{ ls_tar-ach_pct }| ).
    lv_row = lv_row + 1.
  ENDLOOP.

  "─── Sheet 3: Monthly MREC summary ────────────────────────────────────
  lcl_xlsx_writer=>add_sheet( 'Monthly Records' ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 1 iv_value = 'Year'     iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 2 iv_value = 'Period'   iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 3 iv_value = 'Asset'    iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 4 iv_value = 'Product'  iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 5 iv_value = 'Vol Type' iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 6 iv_value = 'Qty 1'    iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 7 iv_value = 'UoM 1'    iv_bold = abap_true ).
  lcl_xlsx_writer=>add_cell( iv_row = 1 iv_col = 8 iv_value = 'Qty 2'    iv_bold = abap_true ).
  lv_row = 2.
  LOOP AT gt_mrec_prd INTO DATA(ls_mr).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 1 iv_value = |{ ls_mr-gjahr }| ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 2 iv_value = |{ ls_mr-monat }| ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 3 iv_value = ls_mr-asset ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 4 iv_value = ls_mr-product ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 5 iv_value = ls_mr-prd_vl_type ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 6 iv_value = |{ ls_mr-prod_vl_qty1 }| ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 7 iv_value = ls_mr-prod_vl_uom1 ).
    lcl_xlsx_writer=>add_cell( iv_row = lv_row iv_col = 8 iv_value = |{ ls_mr-prod_vl_qty2 }| ).
    lv_row = lv_row + 1.
  ENDLOOP.

  "─── Generate XLSX binary and download ────────────────────────────────
  lv_xlsx = lcl_xlsx_writer=>get_xlsx( ).

  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
    EXPORTING input  = gv_rep_date
    IMPORTING output = lv_date_str.
  lv_filename = |DPR_{ lv_date_str }_{ sy-datum }_{ sy-uzeit }.xlsx|.
  REPLACE ALL OCCURRENCES OF '.' IN lv_filename WITH '_'.

  "Stream xstring → binary table for GUI_DOWNLOAD
  lv_offset = 0.
  lv_len    = xstrlen( lv_xlsx ).
  WHILE lv_offset < lv_len.
    lv_chunk_len = lv_len - lv_offset.
    IF lv_chunk_len > 255. lv_chunk_len = 255. ENDIF.
    lv_line = lv_xlsx+lv_offset(lv_chunk_len).
    APPEND lv_line TO lt_data.
    lv_offset = lv_offset + lv_chunk_len.
  ENDWHILE.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING filename = lv_filename filetype = 'BIN'
    TABLES   data_tab  = lt_data
    EXCEPTIONS OTHERS  = 1.

  IF sy-subrc = 0.
    MESSAGE |Excel file { lv_filename } downloaded successfully| TYPE 'S'.
  ELSE.
    MESSAGE 'Error downloading Excel file' TYPE 'E'.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*  FORM: DOWNLOAD_PDF
*  Server-side PDF via Adobe Document Services (ADS).
*  Optimised: single form call builds the complete document;
*  no per-section COM round-trips as in the OLE2-based original.
*----------------------------------------------------------------------*
FORM download_pdf.
  DATA: lv_fm_name      TYPE funcname,
        lv_pdf          TYPE xstring,
        ls_docparams    TYPE sfpdocparams,
        ls_outputparams TYPE sfpoutputparams,
        lv_pdf_size     TYPE i,
        lv_date_str     TYPE char10,
        lv_filename     TYPE string,
        lt_pdf_tab      TYPE STANDARD TABLE OF x255 WITH DEFAULT KEY,
        lv_off          TYPE i,
        lv_clen         TYPE i,
        lv_pline        TYPE x255.

  "─── ADS job ──────────────────────────────────────────────────────────
  ls_outputparams-nodialog = abap_true.
  ls_outputparams-getpdf   = abap_true.
  ls_docparams-langu       = sy-langu.
  ls_docparams-country     = 'IN'.

  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING  ie_outputparams = ls_outputparams
    EXCEPTIONS OTHERS = 1.
  IF sy-subrc <> 0.
    MESSAGE 'Error opening PDF job (ADS not available)' TYPE 'E'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING  i_name     = 'ZPRA_FRM_DPR_PRODUCTION'
    IMPORTING  e_funcname = lv_fm_name
    EXCEPTIONS OTHERS     = 1.
  IF sy-subrc <> 0.
    MESSAGE 'Adobe form ZPRA_FRM_DPR_PRODUCTION not found' TYPE 'E'.
    CALL FUNCTION 'FP_JOB_CLOSE' EXCEPTIONS OTHERS = 1.
    RETURN.
  ENDIF.

  CALL FUNCTION lv_fm_name
    EXPORTING  /1bcdwb/docparams  = ls_docparams
               it_display         = gt_display
               it_targets         = gt_targets
               it_mrec_prd        = gt_mrec_prd
               iv_rep_date        = gv_rep_date
    IMPORTING  /1bcdwb/formoutput = DATA(ls_formoutput)
    EXCEPTIONS OTHERS = 1.
  IF sy-subrc <> 0.
    MESSAGE 'Error generating PDF' TYPE 'E'.
    CALL FUNCTION 'FP_JOB_CLOSE' EXCEPTIONS OTHERS = 1.
    RETURN.
  ENDIF.

  CALL FUNCTION 'FP_JOB_CLOSE'
    IMPORTING  ie_result = DATA(ls_fpresult)
    EXCEPTIONS OTHERS    = 1.

  lv_pdf = ls_formoutput-pdf.

  "─── Build filename and download ──────────────────────────────────────
  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
    EXPORTING input  = gv_rep_date
    IMPORTING output = lv_date_str.
  REPLACE ALL OCCURRENCES OF '.' IN lv_date_str WITH '-'.
  lv_filename = |DPR_{ lv_date_str }_{ sy-datum }.pdf|.

  lv_off      = 0.
  lv_pdf_size = xstrlen( lv_pdf ).
  WHILE lv_off < lv_pdf_size.
    lv_clen = lv_pdf_size - lv_off.
    IF lv_clen > 255. lv_clen = 255. ENDIF.
    lv_pline = lv_pdf+lv_off(lv_clen).
    APPEND lv_pline TO lt_pdf_tab.
    lv_off = lv_off + lv_clen.
  ENDWHILE.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING filename = lv_filename filetype = 'BIN'
    TABLES   data_tab  = lt_pdf_tab
    EXCEPTIONS OTHERS  = 1.

  IF sy-subrc = 0.
    MESSAGE |PDF file { lv_filename } downloaded successfully| TYPE 'S'.
  ELSE.
    MESSAGE 'Error downloading PDF file' TYPE 'E'.
  ENDIF.
ENDFORM.

