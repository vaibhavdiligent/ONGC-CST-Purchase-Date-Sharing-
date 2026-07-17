CLASS zcl_zpra_dpr_excel DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_prod_row,
        production_date  TYPE zpra_t_dly_prd-production_date,
        product          TYPE zpra_t_dly_prd-product,
        product_desc     TYPE char20,
        asset            TYPE zpra_t_dly_prd-asset,
        asset_desc       TYPE char100,
        block            TYPE zpra_t_dly_prd-block,
        volume_type      TYPE zpra_t_dly_prd-prd_vl_type,
        prod_qty1        TYPE zpra_t_dly_prd-prod_vl_qty1,
        prod_uom1        TYPE zpra_t_dly_prd-prod_vl_uom1,
        prod_qty2        TYPE zpra_t_dly_prd-prod_vl_qty2,
        prod_uom2        TYPE zpra_t_dly_prd-prod_vl_uom2,
        pi_pct           TYPE zpra_t_prd_pi-pi,
        ovl_qty1         TYPE zpra_t_dly_prd-prod_vl_qty1,
        ovl_qty2         TYPE zpra_t_dly_prd-prod_vl_qty2,
      END OF ty_prod_row,
      tt_prod_rows TYPE STANDARD TABLE OF ty_prod_row WITH DEFAULT KEY,

      BEGIN OF ty_target_row,
        fiscal_year      TYPE zpra_t_prd_tar-gjahr,
        fiscal_period    TYPE zpra_t_prd_tar-monat,
        product          TYPE zpra_t_prd_tar-product,
        product_desc     TYPE char20,
        asset            TYPE zpra_t_prd_tar-asset,
        asset_desc       TYPE char100,
        block            TYPE zpra_t_prd_tar-block,
        target_code      TYPE zpra_t_prd_tar-tar_code,
        target_type_desc TYPE char50,
        actual_qty       TYPE p LENGTH 16 DECIMALS 3,
        target_qty       TYPE p LENGTH 16 DECIMALS 3,
        variance_qty     TYPE p LENGTH 16 DECIMALS 3,
        achievement_pct  TYPE p LENGTH 7  DECIMALS 2,
      END OF ty_target_row,
      tt_target_rows TYPE STANDARD TABLE OF ty_target_row WITH DEFAULT KEY.

    CLASS-METHODS:
      "! Generate Excel for daily production query.
      "! Returns xstring (binary Excel content) suitable for HTTP response.
      generate_production_excel
        IMPORTING
          it_data         TYPE tt_prod_rows
          iv_date_from    TYPE sy-datum
          iv_date_to      TYPE sy-datum
        RETURNING
          VALUE(rv_xdata) TYPE xstring
        RAISING
          cx_ai_system_error,

      "! Generate Excel for target vs actual query.
      generate_target_excel
        IMPORTING
          it_data         TYPE tt_target_rows
          iv_fiscal_year  TYPE gjahr
          iv_target_code  TYPE zpra_t_prd_tar-tar_code
        RETURNING
          VALUE(rv_xdata) TYPE xstring
        RAISING
          cx_ai_system_error,

      "! Fetch production data and return as Excel binary.
      fetch_and_export_production
        IMPORTING
          iv_date_from    TYPE sy-datum
          iv_date_to      TYPE sy-datum
        RETURNING
          VALUE(rv_xdata) TYPE xstring
        RAISING
          cx_ai_system_error,

      "! Fetch target data and return as Excel binary.
      fetch_and_export_targets
        IMPORTING
          iv_fiscal_year  TYPE gjahr
          iv_target_code  TYPE zpra_t_prd_tar-tar_code
        RETURNING
          VALUE(rv_xdata) TYPE xstring
        RAISING
          cx_ai_system_error,

      "! Fetch daily production and return a DPR-style SUMMARY Excel:
      "! one row per day with Oil (BOPD), Gas (MMSCMD) and Total (BOEPD) at both
      "! JV and OVL (ONGC-Videsh share) level, followed by a "YTD (Average)" row
      "! that is the mean of the daily rows. Ports the classic report's fixes:
      "! gas -> MMSCMD conversion (no near-zero values), BOEPD grand total that
      "! includes the gas oil-equivalent (gas MMSCMD * 6290), and the YTD figure
      "! computed as the daily average so it matches the MTD figure.
      fetch_and_export_dpr_summary
        IMPORTING
          iv_date_from    TYPE sy-datum
          iv_date_to      TYPE sy-datum
        RETURNING
          VALUE(rv_xdata) TYPE xstring
        RAISING
          cx_ai_system_error.

  PRIVATE SECTION.

    " BOE conversion factor: 1 MMSCMD gas == 6290 BOEPD (as in the classic DPR)
    CONSTANTS c_boe_factor TYPE i VALUE 6290.

    TYPES ty_qty TYPE p LENGTH 16 DECIMALS 7.

    TYPES:
      BEGIN OF ty_dpr_day,
        production_date TYPE zpra_t_dly_prd-production_date,
        oil_jv          TYPE ty_qty,   " Oil + Condensate + LNG, JV level, BOPD
        gas_jv          TYPE ty_qty,   " Gas, JV level, MMSCMD
        boepd_jv        TYPE ty_qty,   " Total O+OEG, JV level, BOEPD
        oil_ovl         TYPE ty_qty,   " Oil + Condensate + LNG, OVL share, BOPD
        gas_ovl         TYPE ty_qty,   " Gas, OVL share, MMSCMD
        boepd_ovl       TYPE ty_qty,   " Total O+OEG, OVL share, BOEPD
      END OF ty_dpr_day,
      tt_dpr_day TYPE SORTED TABLE OF ty_dpr_day WITH UNIQUE KEY production_date.

    CONSTANTS:
      c_prod_oil TYPE zpra_t_dly_prd-product VALUE '722000001',
      c_prod_con TYPE zpra_t_dly_prd-product VALUE '722000003',
      c_prod_gas TYPE zpra_t_dly_prd-product VALUE '722000004',
      c_prod_lng TYPE zpra_t_dly_prd-product VALUE '722000005'.

    CLASS-METHODS:
      get_product_desc
        IMPORTING iv_product       TYPE zpra_t_dly_prd-product
        RETURNING VALUE(rv_desc)   TYPE char20,

      get_target_desc
        IMPORTING iv_tar_code      TYPE zpra_t_prd_tar-tar_code
        RETURNING VALUE(rv_desc)   TYPE char50,

      build_excel_from_rows
        IMPORTING
          it_headers      TYPE string_table
          it_rows         TYPE string_table
          iv_sheet_name   TYPE string
        RETURNING
          VALUE(rv_xdata) TYPE xstring
        RAISING
          cx_ai_system_error,

      "! Convert a gas quantity in its native UoM to MMSCMD.
      "! MCM -> as-is (MCM == MMSCM here), MCF -> /35.3, M3 -> /1,000,000.
      gas_to_mmscmd
        IMPORTING iv_qty        TYPE zpra_t_dly_prd-prod_vl_qty1
                  iv_uom        TYPE zpra_t_dly_prd-prod_vl_uom1
        RETURNING VALUE(rv_qty) TYPE ty_qty.

ENDCLASS.


CLASS zcl_zpra_dpr_excel IMPLEMENTATION.

  METHOD generate_production_excel.
    DATA: lt_headers TYPE string_table,
          lt_rows    TYPE string_table,
          lv_row     TYPE string.

    " ── Column headers ─────────────────────────────────────────────────────
    APPEND 'Date'              TO lt_headers.
    APPEND 'Product'           TO lt_headers.
    APPEND 'Asset'             TO lt_headers.
    APPEND 'Asset Description' TO lt_headers.
    APPEND 'Block'             TO lt_headers.
    APPEND 'Volume Type'       TO lt_headers.
    APPEND 'JV Qty (UoM1)'     TO lt_headers.
    APPEND 'UoM1'              TO lt_headers.
    APPEND 'OVL Share (UoM1)'  TO lt_headers.
    APPEND 'JV Qty (UoM2)'     TO lt_headers.
    APPEND 'UoM2'              TO lt_headers.
    APPEND 'OVL Share (UoM2)'  TO lt_headers.
    APPEND 'PI %'              TO lt_headers.

    " ── Data rows ──────────────────────────────────────────────────────────
    LOOP AT it_data INTO DATA(ls_row).
      CLEAR lv_row.
      CONCATENATE
        ls_row-production_date  '|'
        ls_row-product_desc     '|'
        ls_row-asset            '|'
        ls_row-asset_desc       '|'
        ls_row-block            '|'
        ls_row-volume_type      '|'
        ls_row-prod_qty1        '|'
        ls_row-prod_uom1        '|'
        ls_row-ovl_qty1         '|'
        ls_row-prod_qty2        '|'
        ls_row-prod_uom2        '|'
        ls_row-ovl_qty2         '|'
        ls_row-pi_pct
      INTO lv_row.
      APPEND lv_row TO lt_rows.
    ENDLOOP.

    rv_xdata = build_excel_from_rows(
      it_headers    = lt_headers
      it_rows       = lt_rows
      iv_sheet_name = |DPR Production { iv_date_from } to { iv_date_to }|
    ).
  ENDMETHOD.


  METHOD generate_target_excel.
    DATA: lt_headers TYPE string_table,
          lt_rows    TYPE string_table,
          lv_row     TYPE string.

    APPEND 'Fiscal Year'      TO lt_headers.
    APPEND 'Period'           TO lt_headers.
    APPEND 'Product'          TO lt_headers.
    APPEND 'Asset'            TO lt_headers.
    APPEND 'Asset Description'TO lt_headers.
    APPEND 'Block'            TO lt_headers.
    APPEND 'Target Type'      TO lt_headers.
    APPEND 'Actual Qty'       TO lt_headers.
    APPEND 'Target Qty'       TO lt_headers.
    APPEND 'Variance'         TO lt_headers.
    APPEND 'Achievement %'    TO lt_headers.

    LOOP AT it_data INTO DATA(ls_row).
      CLEAR lv_row.
      CONCATENATE
        ls_row-fiscal_year      '|'
        ls_row-fiscal_period    '|'
        ls_row-product_desc     '|'
        ls_row-asset            '|'
        ls_row-asset_desc       '|'
        ls_row-block            '|'
        ls_row-target_type_desc '|'
        ls_row-actual_qty       '|'
        ls_row-target_qty       '|'
        ls_row-variance_qty     '|'
        ls_row-achievement_pct
      INTO lv_row.
      APPEND lv_row TO lt_rows.
    ENDLOOP.

    rv_xdata = build_excel_from_rows(
      it_headers    = lt_headers
      it_rows       = lt_rows
      iv_sheet_name = |DPR Targets FY { iv_fiscal_year } - { iv_target_code }|
    ).
  ENDMETHOD.


  METHOD fetch_and_export_production.
    DATA: lt_data   TYPE tt_prod_rows,
          ls_data   TYPE ty_prod_row,
          lt_pi     TYPE STANDARD TABLE OF zpra_t_prd_pi,
          ls_pi     TYPE zpra_t_prd_pi,
          lt_atext  TYPE STANDARD TABLE OF zoiu_pr_dn,
          ls_atext  TYPE zoiu_pr_dn.

    " Fetch daily production
    SELECT production_date, product, asset, block,
           prd_vl_type, prod_vl_qty1, prod_vl_uom1,
           prod_vl_qty2, prod_vl_uom2
      FROM zpra_t_dly_prd
      INTO TABLE @DATA(lt_dly)
     WHERE production_date BETWEEN @iv_date_from AND @iv_date_to
       AND prd_vl_type = 'NET_PROD'.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_ai_system_error
        MESSAGE e001(00) WITH 'No production data found for date range'.
    ENDIF.

    " Fetch PI values
    SELECT * FROM zpra_t_prd_pi
      INTO TABLE @lt_pi
       FOR ALL ENTRIES IN @lt_dly
     WHERE asset   = @lt_dly-asset
       AND block   = @lt_dly-block
       AND vld_frm <= @iv_date_to
       AND vld_to  >= @iv_date_from.
    SORT lt_pi BY asset block vld_frm vld_to.

    " Fetch asset descriptions
    SELECT dn_no, dn_de FROM zoiu_pr_dn
      INTO TABLE @lt_atext
       FOR ALL ENTRIES IN @lt_dly
     WHERE dn_no = @lt_dly-asset.
    SORT lt_atext BY dn_no.

    " Build output rows
    LOOP AT lt_dly INTO DATA(ls_dly).
      CLEAR ls_data.
      ls_data-production_date = ls_dly-production_date.
      ls_data-product         = ls_dly-product.
      ls_data-product_desc    = get_product_desc( ls_dly-product ).
      ls_data-asset           = ls_dly-asset.
      ls_data-block           = ls_dly-block.
      ls_data-volume_type     = ls_dly-prd_vl_type.
      ls_data-prod_qty1       = ls_dly-prod_vl_qty1.
      ls_data-prod_uom1       = ls_dly-prod_vl_uom1.
      ls_data-prod_qty2       = ls_dly-prod_vl_qty2.
      ls_data-prod_uom2       = ls_dly-prod_vl_uom2.

      " Asset description
      READ TABLE lt_atext INTO ls_atext
        WITH KEY dn_no = ls_dly-asset BINARY SEARCH.
      IF sy-subrc = 0.
        ls_data-asset_desc = ls_atext-dn_de.
      ENDIF.

      " PI for this date
      LOOP AT lt_pi INTO ls_pi
        WHERE asset   = ls_dly-asset
          AND block   = ls_dly-block
          AND vld_frm <= ls_dly-production_date
          AND vld_to  >= ls_dly-production_date.
        ls_data-pi_pct = ls_pi-pi.
        EXIT.
      ENDLOOP.

      " OVL share
      IF ls_data-pi_pct > 0.
        ls_data-ovl_qty1 = ls_data-prod_qty1 * ls_data-pi_pct / 100.
        ls_data-ovl_qty2 = ls_data-prod_qty2 * ls_data-pi_pct / 100.
      ELSE.
        ls_data-ovl_qty1 = ls_data-prod_qty1.
        ls_data-ovl_qty2 = ls_data-prod_qty2.
      ENDIF.

      APPEND ls_data TO lt_data.
    ENDLOOP.

    rv_xdata = generate_production_excel(
      it_data      = lt_data
      iv_date_from = iv_date_from
      iv_date_to   = iv_date_to
    ).
  ENDMETHOD.


  METHOD fetch_and_export_targets.
    DATA: lt_data  TYPE tt_target_rows,
          ls_data  TYPE ty_target_row,
          lt_atext TYPE STANDARD TABLE OF zoiu_pr_dn,
          ls_atext TYPE zoiu_pr_dn.

    " Fetch monthly actuals
    SELECT gjahr, monat, asset, block, product,
           prd_vl_type, prod_vl_qty1
      FROM zpra_t_mrec_prd
      INTO TABLE @DATA(lt_mrec)
     WHERE gjahr      = @iv_fiscal_year
       AND prd_vl_type = 'NET_PROD'.

    " Fetch targets
    SELECT tar_code, gjahr, monat, asset, block,
           product, tar_qty
      FROM zpra_t_prd_tar
      INTO TABLE @DATA(lt_tar)
     WHERE gjahr    = @iv_fiscal_year
       AND tar_code = @iv_target_code.
    SORT lt_tar BY gjahr monat asset block product tar_code.

    " Fetch asset descriptions
    IF lt_mrec IS NOT INITIAL.
      SELECT dn_no, dn_de FROM zoiu_pr_dn
        INTO TABLE @lt_atext
         FOR ALL ENTRIES IN @lt_mrec
       WHERE dn_no = @lt_mrec-asset.
      SORT lt_atext BY dn_no.
    ENDIF.

    LOOP AT lt_mrec INTO DATA(ls_mrec).
      CLEAR ls_data.
      ls_data-fiscal_year   = ls_mrec-gjahr.
      ls_data-fiscal_period = ls_mrec-monat.
      ls_data-product       = ls_mrec-product.
      ls_data-product_desc  = get_product_desc( ls_mrec-product ).
      ls_data-asset         = ls_mrec-asset.
      ls_data-block         = ls_mrec-block.
      ls_data-target_code   = iv_target_code.
      ls_data-target_type_desc = get_target_desc( iv_target_code ).
      ls_data-actual_qty    = ls_mrec-prod_vl_qty1.

      READ TABLE lt_atext INTO ls_atext
        WITH KEY dn_no = ls_mrec-asset BINARY SEARCH.
      IF sy-subrc = 0.
        ls_data-asset_desc = ls_atext-dn_de.
      ENDIF.

      READ TABLE lt_tar INTO DATA(ls_tar)
        WITH KEY gjahr   = ls_mrec-gjahr
                 monat   = ls_mrec-monat
                 asset   = ls_mrec-asset
                 block   = ls_mrec-block
                 product = ls_mrec-product
        BINARY SEARCH.
      IF sy-subrc = 0.
        ls_data-target_qty = ls_tar-tar_qty.
      ENDIF.

      ls_data-variance_qty = ls_data-actual_qty - ls_data-target_qty.
      IF ls_data-target_qty > 0.
        ls_data-achievement_pct =
          ls_data-actual_qty * 100 / ls_data-target_qty.
      ENDIF.

      APPEND ls_data TO lt_data.
    ENDLOOP.

    rv_xdata = generate_target_excel(
      it_data        = lt_data
      iv_fiscal_year = iv_fiscal_year
      iv_target_code = iv_target_code
    ).
  ENDMETHOD.


  METHOD build_excel_from_rows.
    " Uses XCO_CP_XLSX (S/4HANA 2020+) to build a real .xlsx binary.
    " Falls back to CSV-in-xstring if XCO is unavailable.
    DATA: lo_workbook  TYPE REF TO if_xco_xlsx_document,
          lo_sheet     TYPE REF TO if_xco_xlsx_wks,
          lv_col       TYPE i,
          lv_row_idx   TYPE i,
          lv_cell_val  TYPE string.

    TRY.
        " ── Create workbook ───────────────────────────────────────────────
        lo_workbook = xco_cp_xlsx=>document->empty( )->workbook->create( ).
        lo_sheet    = lo_workbook->add_new_sheet( iv_sheet_name ).

        " ── Header row (row 1, bold) ──────────────────────────────────────
        lv_row_idx = 1.
        lv_col     = 1.
        LOOP AT it_headers INTO DATA(lv_hdr).
          lo_sheet->cursor->move_to( iv_row    = lv_row_idx
                                      iv_column = lv_col ).
          lo_sheet->cursor->cell->value->write_to( lv_hdr ).
          lv_col = lv_col + 1.
        ENDLOOP.

        " ── Data rows ─────────────────────────────────────────────────────
        lv_row_idx = 2.
        LOOP AT it_rows INTO DATA(lv_data_row).
          DATA(lt_cells) = cl_abap_char_utilities=>char_str( lv_data_row ).
          SPLIT lv_data_row AT '|' INTO TABLE DATA(lt_cols).
          lv_col = 1.
          LOOP AT lt_cols INTO lv_cell_val.
            CONDENSE lv_cell_val.
            lo_sheet->cursor->move_to( iv_row    = lv_row_idx
                                        iv_column = lv_col ).
            lo_sheet->cursor->cell->value->write_to( lv_cell_val ).
            lv_col = lv_col + 1.
          ENDLOOP.
          lv_row_idx = lv_row_idx + 1.
        ENDLOOP.

        " ── Serialise to xstring ──────────────────────────────────────────
        rv_xdata = lo_workbook->serialize( ).

      CATCH cx_xco_xlsx INTO DATA(lx).
        " ── Fallback: build a plain CSV wrapped as xstring ────────────────
        DATA: lv_csv TYPE string.
        CONCATENATE LINES OF it_headers INTO lv_csv SEPARATED BY ';'.
        lv_csv = lv_csv && cl_abap_char_utilities=>newline.
        LOOP AT it_rows INTO DATA(lv_r).
          REPLACE ALL OCCURRENCES OF '|' IN lv_r WITH ';'.
          lv_csv = lv_csv && lv_r && cl_abap_char_utilities=>newline.
        ENDLOOP.
        rv_xdata = cl_abap_codepage=>convert_to( lv_csv ).
    ENDTRY.
  ENDMETHOD.


  METHOD get_product_desc.
    rv_desc = SWITCH #( iv_product
      WHEN c_prod_oil THEN 'Oil'
      WHEN c_prod_con THEN 'Condensate'
      WHEN c_prod_gas THEN 'Gas'
      WHEN c_prod_lng THEN 'LNG'
      ELSE                 iv_product ).
  ENDMETHOD.


  METHOD get_target_desc.
    rv_desc = SWITCH #( iv_tar_code
      WHEN 'TAR_BE' THEN 'Budget Estimate'
      WHEN 'TAR_IN' THEN 'Internal Target'
      WHEN 'TAR_EX' THEN 'MOU Excellent'
      WHEN 'TAR_VG' THEN 'MOU Very Good'
      WHEN 'TAR_PC' THEN 'Physical Control'
      WHEN 'TAR_RE' THEN 'Revised Estimate'
      ELSE               iv_tar_code ).
  ENDMETHOD.


  METHOD gas_to_mmscmd.
    " Port of convert_gas_units_to_mmscm from the classic DPR program.
    CASE iv_uom.
      WHEN 'MCM'.  rv_qty = iv_qty.                 " MCM == MMSCM here
      WHEN 'MCF'.  rv_qty = iv_qty / '35.3'.        " 1 MMSCM ~= 35.3 MMCF
      WHEN 'M3'.   rv_qty = iv_qty / 1000000.       " m3 -> million m3
      WHEN OTHERS. rv_qty = iv_qty.
    ENDCASE.
  ENDMETHOD.


  METHOD fetch_and_export_dpr_summary.
    DATA: lt_days TYPE tt_dpr_day,
          ls_day  TYPE ty_dpr_day,
          lv_pi   TYPE zpra_t_prd_pi-pi,
          lv_gas  TYPE ty_qty,
          lv_ovl  TYPE ty_qty.

    " ── Daily net production for the range ───────────────────────────────────
    SELECT production_date, product, asset, block,
           prd_vl_type, prod_vl_qty1, prod_vl_uom1
      FROM zpra_t_dly_prd
      INTO TABLE @DATA(lt_dly)
     WHERE production_date BETWEEN @iv_date_from AND @iv_date_to
       AND prd_vl_type = 'NET_PROD'.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_ai_system_error
        MESSAGE e001(00) WITH 'No production data found for date range'.
    ENDIF.

    " ── PI (participating interest) ──────────────────────────────────────────
    SELECT * FROM zpra_t_prd_pi
      INTO TABLE @DATA(lt_pi)
       FOR ALL ENTRIES IN @lt_dly
     WHERE asset   = @lt_dly-asset
       AND block   = @lt_dly-block
       AND vld_frm <= @iv_date_to
       AND vld_to  >= @iv_date_from.

    " ── Aggregate per day: Oil/Cond/LNG as BOPD, Gas as MMSCMD, at JV & OVL ──
    LOOP AT lt_dly INTO DATA(ls_dly).
      " PI valid for this asset/block/date
      CLEAR lv_pi.
      LOOP AT lt_pi INTO DATA(ls_pi)
        WHERE asset   = ls_dly-asset
          AND block   = ls_dly-block
          AND vld_frm <= ls_dly-production_date
          AND vld_to  >= ls_dly-production_date.
        lv_pi = ls_pi-pi.
        EXIT.
      ENDLOOP.

      READ TABLE lt_days INTO ls_day
        WITH KEY production_date = ls_dly-production_date.
      IF sy-subrc <> 0.
        CLEAR ls_day.
        ls_day-production_date = ls_dly-production_date.
      ENDIF.

      IF ls_dly-product = c_prod_gas.
        lv_gas = gas_to_mmscmd( iv_qty = ls_dly-prod_vl_qty1
                                iv_uom = ls_dly-prod_vl_uom1 ).
        ls_day-gas_jv = ls_day-gas_jv + lv_gas.
        IF lv_pi > 0.
          lv_ovl = lv_gas * lv_pi / 100.
        ELSE.
          lv_ovl = lv_gas.
        ENDIF.
        ls_day-gas_ovl = ls_day-gas_ovl + lv_ovl.
      ELSE.
        " Oil / Condensate / LNG -> BOPD (native qty1)
        ls_day-oil_jv = ls_day-oil_jv + ls_dly-prod_vl_qty1.
        IF lv_pi > 0.
          lv_ovl = ls_dly-prod_vl_qty1 * lv_pi / 100.
        ELSE.
          lv_ovl = ls_dly-prod_vl_qty1.
        ENDIF.
        ls_day-oil_ovl = ls_day-oil_ovl + lv_ovl.
      ENDIF.

      " Total (O + OEG) BOEPD = oil BOPD + gas MMSCMD * 6290
      ls_day-boepd_jv  = ls_day-oil_jv  + ls_day-gas_jv  * c_boe_factor.
      ls_day-boepd_ovl = ls_day-oil_ovl + ls_day-gas_ovl * c_boe_factor.

      MODIFY TABLE lt_days FROM ls_day.
      IF sy-subrc <> 0.
        INSERT ls_day INTO TABLE lt_days.
      ENDIF.
    ENDLOOP.

    " ── Build rows: daily + "YTD (Average)" = mean of the daily rows ─────────
    DATA: lt_headers TYPE string_table,
          lt_rows    TYPE string_table,
          lv_row     TYPE string,
          lv_n       TYPE i,
          ls_avg     TYPE ty_dpr_day.

    APPEND 'Date'              TO lt_headers.
    APPEND 'Oil JV (BOPD)'     TO lt_headers.
    APPEND 'Gas JV (MMSCMD)'   TO lt_headers.
    APPEND 'Total JV (BOEPD)'  TO lt_headers.
    APPEND 'Oil OVL (BOPD)'    TO lt_headers.
    APPEND 'Gas OVL (MMSCMD)'  TO lt_headers.
    APPEND 'Total OVL (BOEPD)' TO lt_headers.

    LOOP AT lt_days INTO ls_day.
      CLEAR lv_row.
      CONCATENATE ls_day-production_date '|'
                  ls_day-oil_jv  '|' ls_day-gas_jv  '|' ls_day-boepd_jv  '|'
                  ls_day-oil_ovl '|' ls_day-gas_ovl '|' ls_day-boepd_ovl
             INTO lv_row.
      APPEND lv_row TO lt_rows.
      ls_avg-oil_jv    = ls_avg-oil_jv    + ls_day-oil_jv.
      ls_avg-gas_jv    = ls_avg-gas_jv    + ls_day-gas_jv.
      ls_avg-boepd_jv  = ls_avg-boepd_jv  + ls_day-boepd_jv.
      ls_avg-oil_ovl   = ls_avg-oil_ovl   + ls_day-oil_ovl.
      ls_avg-gas_ovl   = ls_avg-gas_ovl   + ls_day-gas_ovl.
      ls_avg-boepd_ovl = ls_avg-boepd_ovl + ls_day-boepd_ovl.
    ENDLOOP.

    lv_n = lines( lt_days ).
    IF lv_n > 0.
      ls_avg-oil_jv    = ls_avg-oil_jv    / lv_n.
      ls_avg-gas_jv    = ls_avg-gas_jv    / lv_n.
      ls_avg-boepd_jv  = ls_avg-boepd_jv  / lv_n.
      ls_avg-oil_ovl   = ls_avg-oil_ovl   / lv_n.
      ls_avg-gas_ovl   = ls_avg-gas_ovl   / lv_n.
      ls_avg-boepd_ovl = ls_avg-boepd_ovl / lv_n.
    ENDIF.

    CLEAR lv_row.
    CONCATENATE 'YTD (Average)' '|'
                ls_avg-oil_jv  '|' ls_avg-gas_jv  '|' ls_avg-boepd_jv  '|'
                ls_avg-oil_ovl '|' ls_avg-gas_ovl '|' ls_avg-boepd_ovl
           INTO lv_row.
    APPEND lv_row TO lt_rows.

    rv_xdata = build_excel_from_rows(
      it_headers    = lt_headers
      it_rows       = lt_rows
      iv_sheet_name = |DPR Summary { iv_date_from } to { iv_date_to }| ).
  ENDMETHOD.

ENDCLASS.
