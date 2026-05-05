# ZPRA_DPR_REPORT — Developer & Functional Consultant Guide

**Program:** `ZPRA_DPR_REPORT` (deployed as `ZPRA_DPR_REPORT_NEW1` in SAP)  
**Version:** 1.6  
**Language:** SAP ABAP ECC 6.0  
**Output:** Excel workbook with 3 sheets  

---

## Table of Contents
1. [Overview](#overview)
2. [Input Parameters](#input-parameters)
3. [Key SAP Tables Used](#key-sap-tables-used)
4. [Excel Output Structure](#excel-output-structure)
5. [Sheet 1: DPR — Row-by-Row Breakdown](#sheet-1-dpr--row-by-row-breakdown)
6. [Sheet 2: Trend Chart Data](#sheet-2-trend-chart-data)
7. [Sheet 3: Production Performance](#sheet-3-production-performance)
8. [Data Retrieval Priority Logic (All Sections)](#data-retrieval-priority-logic-all-sections)
9. [Unit Conversion Logic](#unit-conversion-logic)
10. [PI (Participating Interest) Logic](#pi-participating-interest-logic)
11. [Known Data Issues and Fixes](#known-data-issues-and-fixes)

---

## Overview

This program generates the **Daily Production Report (DPR)** for ONGC Videsh's international upstream assets. For each date and asset, it shows:
- Daily production (JV and ONGC Videsh OVL share)
- Month-to-date and year-to-date actuals
- Annual targets vs actuals
- 5-year historical production (Section 3f)
- An Excel chart of current-year production performance

The output uses **dynamic columns** — the number of asset columns is driven at runtime from configuration table `ZPRA_C_PRD_PROF`. Columns are identified internally by the key `{product_code}-{asset_code}`, e.g., `722000001-RUS_SK1`.

---

## Input Parameters

| Parameter | Type | Meaning |
|-----------|------|---------|
| `P_DATE` | Date | Report date (production date to report up to) |
| `P_BB` | Radio | Unit mode: Barrels / BOE (cumulative) |
| `P_BBD` | Radio | Unit mode: BOPD / BOEPD (per day — **most common**) |
| `P_TM` | Radio | Unit mode: Tonnes / MSCM |
| `P_BMD` | Radio | Unit mode: BOEPD with MMSCMD gas |
| `P_MB` | Radio | Unit mode: MMT / BCM |
| `P_C_JV` | Radio | Consortium level: JV (joint venture total) — **default** |
| `P_C_OLV` | Radio | Consortium level: OVL (ONGC Videsh share only) |

> **Note:** "Consortium Level" on the report header row reflects `P_C_JV` / `P_C_OLV`. The daily individual-asset columns show JV values by default. Product totals always show ONGC Videsh OVL share.

---

## Key SAP Tables Used

| Table | Purpose |
|-------|---------|
| `ZPRA_C_PRD_PROF` | Master list of assets/products to include in report |
| `ZPRA_T_DLY_PRD` | Daily production (raw, field-level) — primary source for current data |
| `ZPRA_T_DLY_RPRD` | Daily reconciled production — used when available (overrides dly_prd) |
| `ZPRA_T_MREC_APP` | Monthly approved production — highest priority, used when present |
| `ZPRA_T_MREC_PRD` | Monthly reconciled production — used in some older sections |
| `ZPRA_T_PRD_PI` | Participating Interest (PI) by asset/block and validity period |
| `ZPRA_T_TAR_CF` | Conversion Factor (bbl/tonne) by product/asset/block/fiscal year |
| `ZPRA_C_DPR_PROF` | Asset-level display config (e.g., gas combine flag) |
| `ZPRA_T_PRD_TAR` | Annual production targets by asset/product |
| `ZPRA_T_MREC_REM` | Operational remarks per asset/date |

---

## Excel Output Structure

```
Workbook
├── Sheet 1: "DPR ( DD-MON-YYYY )"   ← Main report
├── Sheet 2: "2"                       ← Daily trend data for chart
└── Sheet 3: "Production Performance"  ← Annual summary chart
```

---

## Sheet 1: DPR — Row-by-Row Breakdown

### Header Section (Rows 4–10)

| Row | Col 1 | Other Columns | Source |
|-----|-------|---------------|--------|
| 4 | `"Daily Production Report : DD-MON-YYYY"` | — | `P_DATE` parameter |
| 5 | `"Product"` | Product groups: `Oil`, `Total Oil`, `Condensate`, `Total Condensate`, `Gas`, `Total Gas`, `Grand Total` | Static header; columns built from `ZPRA_C_PRD_PROF` product list |
| 6 | `"Asset"` | Asset display names (e.g., `"Russia, Sakhalin-1"`) | `ZPRA_C_DPR_PROF-asset_desc` |
| 7 | `"ONGC Videsh Share (%)"` | PI% per asset | `ZPRA_T_PRD_PI-pi` (latest valid entry per asset) |
| 8 | `"Conversion Factor (bbl/tonnes)"` | CF per asset | `ZPRA_T_TAR_CF-conv_factor` (latest entry for current FY) |
| 9 | `"Consortium Level"` | `JV` or `ONGC Videsh` per column group | Derived from `P_C_JV` / `P_C_OLV` parameter |
| 10 | `"Date/Unit"` | Unit label per product group (e.g., `BOPD`, `MMSCMD`, `BCPD`) | Derived from `P_BBD`/`P_MB`/etc. parameter |

**Column layout (Sheet 1):**
- Col 1: Row label / Date
- Col 2: Secondary label (e.g., FY year for historical rows)
- Cols 3+: One column per asset per product (dynamic, in order from `ZPRA_C_PRD_PROF`)
- After individual asset columns: Product Total column (ONGC Videsh share sum)
- After all products: Grand Total column (Oil+Gas combined, ONGC Videsh share)

---

### Section 1 — Daily Production (Rows 11–42)

**What it shows:** One row per production date, from `P_DATE - 32 days` to `P_DATE`.

**Data source (priority order per asset per date):**
1. `ZPRA_T_DLY_PRD` — raw daily production (main source)

**Individual asset columns (JV):**
```
value = prod_vl_qty1 × gv_individual_mul
```
- If `P_C_JV` (default): `gv_individual_mul = 1` → shows raw JV field value  
- If `P_C_OLV`: `gv_individual_mul = PI / 100` → shows ONGC Videsh share

**Product Total column (OVL):**
```
value = prod_vl_qty1 × gv_total_mul
gv_total_mul = PI / 100  (always shows ONGC Videsh share)
```

**Grand Total column:**
```
value = prod_vl_qty1 × gv_grand_total_mul   (same as total_mul)
```

**Unit conversion (oil/condensate):** `convert_non_gas_units_2a2` divides `prod_vl_qty1` by days to get per-day rate if unit mode is per-day.

**Gas assets** with combine flag (`ZPRA_C_DPR_PROF`): multiple blocks aggregated into one `COMBINE` column.

---

### Section 2 — MTD / Monthly Actuals (Rows 43–46)

| Row | Label | Content |
|-----|-------|---------|
| 43 | `Target : <Month> <Year>` | Annual target for current month (from `ZPRA_T_PRD_TAR`) |
| 44 | `Prod. : MTD Actual` with current month start date | Month-to-date average daily production (sum of daily values ÷ days elapsed) |
| 45 | Same row, prior year date | Prior year same-month MTD actual for comparison |
| 46 | `Prod. : Monthly Actual` with prior month start date | Full prior month average daily production |

**Data source for rows 44–46:** Same as Section 1 — `ZPRA_T_DLY_PRD`, summed over the relevant date range then divided by day count.

---

### Section 3 — Annual Summary (Rows 47–62, Consortium Level = ONGC Videsh, Unit = MMT/BCM)

At row 53 the consortium level label changes to `"ONGC Videsh"` and units switch to MMT (oil) / BCM (gas). All values below are **ONGC Videsh OVL share**.

| Row | Label | Content | Data Source |
|-----|-------|---------|-------------|
| 47 | `Target : 2025-26` | Full-year annual target in MMT/BCM | `ZPRA_T_PRD_TAR` × `conv_factor` |
| 48 | `YTD Target : 2025-26` | Year-to-date target (prorated to `P_DATE`) | Same, prorated by elapsed days |
| 49 | `YTD Actual Prod. 2025-2026` | YTD actual OVL production in MMT/BCM | `ZPRA_T_DLY_PRD` / `ZPRA_T_DLY_RPRD` summed since Apr 1 |
| 50 | `(blank) 2024-2025` | Prior year YTD actual (same date last year) | Same tables, prior year date range |
| 51 | `Asking Rate : 2025-26` | Required daily rate to meet annual target | `(Annual Target - YTD Actual) ÷ remaining days` |
| 52 | `Actual Prod. : 2024-25` | Full prior fiscal year actual in BOPD/BOEPD | `ZPRA_T_DLY_RPRD` or `ZPRA_T_DLY_PRD`, divided by 365 |
| 53 | `Consortium Level` | Label row: `ONGC Videsh` | Static |
| 54 | `Unit` | `MMT` (oil), `BCM` (gas) | Derived from `P_MB` / unit parameter |
| 55 | `Target : 2025-26` | Annual target in MMT/BCM (OVL) | `ZPRA_T_PRD_TAR` |
| 56 | `YTD Target : 2025-26` | YTD target in MMT/BCM | Prorated from above |
| 57 | `YTD Actual Prod. : 2025-26` | YTD actual in MMT/BCM | Sum of daily production ÷ 1,000,000 |

---

### Section 3f — Historical Actual Production (Rows 58–62)

**What it shows:** 5 past fiscal years (5 years back from current FY). Unit: MMT (oil) / BCM (gas). OVL share only.

| Row | Col 1 | Col 2 | Asset columns |
|-----|-------|-------|---------------|
| 58 | `Actual Production` | `2020 - 21` | OVL production in MMT per asset |
| 59 | *(blank)* | `2021 - 22` | OVL production in MMT per asset |
| 60 | *(blank)* | `2022 - 23` | OVL production in MMT per asset |
| 61 | *(blank)* | `2023 - 24` | OVL production in MMT per asset |
| 62 | *(blank)* | `2024 - 25` | OVL production in MMT per asset |

**Data retrieval — three priority paths per asset per month (highest to lowest):**

```
Path 1 (MREC_APP — highest priority):
  Table: ZPRA_T_MREC_APP
  Key:   GJAHR + MONAT + ASSET + BLOCK + PRODUCT
  Value: app_vl_qty × 1,000,000
  Note:  Already in OVL MMT. No CF or PI applied.

Path 2 (MREC_APP not found → ZPRA_T_DLY_RPRD):
  Table: ZPRA_T_DLY_RPRD
  Key:   PRODUCT + ASSET + BLOCK + PRODUCTION_DATE (in month range)
  Value: ovl_rcn_vl_qty3  (copied to ovl_prd_vl_qty1 by convert_rprd_units_mb)
  Note:  For most assets this is already OVL share.
         EXCEPTION: Some historical SK (Sakhalin-type) records store JV in this field.
         FIX (v1.6): If asset contains 'SK' or 'SAKH', multiply by PI/100.

Path 3 (RPRD not found → ZPRA_T_DLY_PRD):
  Table: ZPRA_T_DLY_PRD
  Key:   PRODUCT + ASSET + BLOCK + PRODUCTION_DATE (in month range)
  Value: prod_vl_qty1
  Steps:
    1. Call convert_non_gas_units3f → divides prod_vl_qty1 by CF (bbl/tonne)
       CF source: ZPRA_T_TAR_CF for production date's fiscal year
       Fallback: latest CF from same table if year not found
       SK override (v1.6): if asset CS 'SK' or 'SAKH', CF = 7.39
    2. For gas: multiply by PI/100 (gas in dly_prd stores JV)
    3. For SK non-gas: also multiply by PI/100 (v1.6 fix — SK oil in dly_prd also stores JV)
```

**CF (Conversion Factor) lookup:** `get_cf_from_date` → reads `ZPRA_T_TAR_CF` filtered by product/asset/block and the FY containing the production date. If not found, takes the last entry in `GT_CF` for that product/asset/block (highest GJAHR wins since table is sorted ASC).

**PI lookup for Section 3f:** Uses `GT_ZPRA_T_PRD_PI_3F` (loaded from `ZPRA_T_PRD_PI`). First tries date-range match (`vld_frm ≤ date ≤ vld_to`), then falls back to any entry for the asset.

---

### Section 4 — Remarks (Rows 64–71)

| Row | Content | Source |
|-----|---------|--------|
| 64 | `"Remarks"` header | Static |
| 65 | Column headers: Asset / Date / Comments | Static |
| 66+ | One row per remark | `ZPRA_T_MREC_REM` — comments entered by users in the input transaction |

---

### Footer (Row 74)
`"Report Generated On: DD.MON.YYYY At HH:MM:SS"` — system date/time at report run.

---

## Sheet 2: Trend Chart Data

**Sheet name:** `"2"`

Provides the underlying data for the trend chart displayed alongside Sheet 3.

| Row | Col 1 | Cols 3+ |
|-----|-------|---------|
| 2 | *(blank)* | Production dates (one per column, from FY start to `P_DATE`) |
| 3 | `"Actual Production"` | Total ONGC Videsh daily production (BOEPD) per date |
| 4 | `"BE Target"` | BE budget target per date (if available) |

**Source:** `fill_dynamic_table_sec5a` → reads `ZPRA_T_DLY_RPRD` / `ZPRA_T_DLY_PRD` for each day, aggregates all assets to Grand Total column.

---

## Sheet 3: Production Performance

**Sheet name:** `"Production Performance"`

A summary table feeding the Excel bar chart.

| Row | Col 1 | Col 2 | Col 3 | Col 4 | Col 5 | Col 6 | Col 7 |
|-----|-------|-------|-------|-------|-------|-------|-------|
| 45 | `"ONGC Videsh YYYY-YY"` | `"Oil, LNG & Condensate (BOPD)"` | *(merged)* | `"Gas (MMSCMD)"` | *(merged)* | `"Total (O+OEG) (BOEPD)"` | *(merged)* |
| 46 | *(blank)* | `Annual` | `YTD` | `Annual` | `YTD` | `Annual` | `YTD` |
| 47+ | Row label | Oil annual | Oil YTD | Gas annual | Gas YTD | Total annual | Total YTD |

**Rows 47+ content:**

| Row | Label | Source |
|-----|-------|--------|
| 47 | `Actual` | Current FY actual from `<gfs_sec2d_table>` row 1 (= `fill_dynamic_table_sec6a`) |
| 48+ | `YYYY-YY Target` | Annual target rows from `<gfs_sec2b_table>` (= `fill_dynamic_table_sec6b`) |

**How `fill_dynamic_table_sec6a` (Actual row) is built:**
1. Reads first row of `<gfs_sec2d_table>` (current FY actuals from Section 2d)
2. Sums Oil products: `722000001-TOTAL + 722000003-TOTAL + 722000005-TOTAL`
3. Sums Gas product: `722000004-TOTAL`
4. Reads `GRAND-TOTAL` for combined Total
5. Writes same value to both Annual and YTD columns (they equal each other for current year actuals)

**Section 2d data (source for graph Actual row):**
- Covers current FY and prior FY (2 iterations of `DO 2 TIMES`)
- Same three-path priority: MREC_APP → dly_rprd → dly_prd
- Uses `ovl_prd_vl_qty1` for Product/Grand Total columns
- Divided by total days elapsed to get BOPD

---

## Data Retrieval Priority Logic (All Sections)

Every section follows the same priority cascade:

```
FOR EACH month in range:
  FOR EACH asset/product:
    Step 1 → READ ZPRA_T_MREC_APP (gjahr + monat + asset + block + product)
             If found: use app_vl_qty × 1,000,000 as-is (OVL, no conversion needed)

    Step 2 → If not found: READ/LOOP ZPRA_T_DLY_RPRD (production_date range)
             If found: call convert_rprd_units_mb → copies ovl_rcn_vl_qty3 to ovl_prd_vl_qty1
             Use ovl_prd_vl_qty1 (normally = OVL share)

    Step 3 → If still not found: LOOP ZPRA_T_DLY_PRD (production_date range)
             Call convert_non_gas_units3f → apply CF to get tonnes
             Apply PI if gas (or if SK asset in v1.6)
             Use prod_vl_qty1 as OVL share
```

---

## Unit Conversion Logic

### Oil/Condensate (Section 3f — `convert_non_gas_units3f`)

```
prod_vl_qty1 (in barrels or bbl/day) ÷ conv_factor (bbl/tonne) = MMT value
```

CF is fetched from `ZPRA_T_TAR_CF`:
1. Exact FY match (`gjahr` = FY of production date)
2. Fallback: highest available GJAHR for that product/asset/block

### Gas (all sections)
Gas remains in its native unit (MMSCMD or BCM). No CF applied. PI multiplication converts JV → OVL.

### RPRD path (`convert_rprd_units_mb`)
Simply copies `ovl_rcn_vl_qty3` → `ovl_prd_vl_qty1` (no arithmetic for oil in MB mode). No CF or PI applied at this step (data is assumed pre-converted).

---

## PI (Participating Interest) Logic

PI is stored in `ZPRA_T_PRD_PI` with validity dates (`vld_frm`, `vld_to`).

### When PI is applied
| Section | Product | Path | PI Applied? |
|---------|---------|------|-------------|
| Section 1 (daily) | Oil | dly_prd | Via `gv_total_mul = PI/100` |
| Section 1 (daily) | Gas | dly_prd | Via `gv_total_mul = PI/100` |
| Section 3f (historical) | Gas | dly_prd | Yes — gas stores JV in dly_prd |
| Section 3f (historical) | Oil (SK assets) | dly_rprd | Yes — SK historical dly_rprd stores JV (v1.6 fix) |
| Section 3f (historical) | Oil (SK assets) | dly_prd | Yes — SK oil in dly_prd also stores JV (v1.6 fix) |
| Section 3f (historical) | Oil (other assets) | dly_rprd | No — dly_rprd stores OVL for non-SK assets |
| MREC_APP path | All | mrec_app | No — already OVL |

### PI Lookup Order (Section 3f)
```abap
1. LOOP gt_zpra_t_prd_pi_3f WHERE asset = X AND vld_frm <= date AND vld_to >= date
   → EXIT on first match (date-range match)
2. If not found: LOOP gt_zpra_t_prd_pi_3f WHERE asset = X
   → EXIT on first match (any PI for asset, ignoring dates)
```

---

## Known Data Issues and Fixes

### Issue: Sakhalin-1 (RUS_SK1) Historical Oil FY2020-21 and FY2021-22 Wrong

**Symptom:** Section 3f showed ~12 MMT for FY2020-21 and ~10.9 MMT instead of expected ~2.45 MMT and ~2.19 MMT.

**Root cause:** For Sakhalin-1 oil, the `ZPRA_T_DLY_RPRD` records for FY2020-21 and FY2021-22 store **JV production** (whole-field) in `ovl_rcn_vl_qty3` instead of the ONGC Videsh OVL share. This is a **data inconsistency** specific to those years — other assets and other years store OVL as expected.

**Fix applied (v1.6):** After `convert_rprd_units_mb`, check if asset contains `'SK'` or `'SAKH'` (substring match). If yes and product is non-gas, multiply `ovl_prd_vl_qty1` by `PI/100` to convert JV → OVL.

```abap
" Code location: fill_dynamic_table_sec3f, dly_rprd loop (~line 7828)
IF ( gs_zpra_t_dly_rprd-asset EQ 'RUS_SK1' OR
     gs_zpra_t_dly_rprd-asset CS 'SK' OR
     gs_zpra_t_dly_rprd-asset CS 'SAKH' ) AND
   gs_zpra_t_dly_rprd-product NE c_prod_gas .
  " ... PI lookup and multiply ...
  gs_zpra_t_dly_rprd-ovl_prd_vl_qty1 = gs_zpra_t_dly_rprd-ovl_prd_vl_qty1 * PI / 100
ENDIF.
```

**Why substring match (CS) instead of exact EQ 'RUS_SK1':** Version 1.5 used `EQ 'RUS_SK1'` but had no effect — the actual asset code stored in `ZPRA_C_PRD_PROF` / `ZPRA_T_DLY_RPRD` may differ from `'RUS_SK1'`. Using `CS 'SK'` ensures the fix applies regardless of the exact internal code.

**Permanent fix recommendation:** Check the actual asset code in `ZPRA_C_PRD_PROF` SE16 for Sakhalin-1. Once confirmed, replace the `CS 'SK'` guard with the exact `EQ 'ACTUAL_CODE'` for safety. Alternatively, correct the historical data in `ZPRA_T_DLY_RPRD` so `ovl_rcn_vl_qty3` stores the OVL share for those years.

---

## Section-to-Form Mapping (Quick Reference)

| Excel Section | Fill Form | Display Form |
|---------------|-----------|--------------|
| Sheet 1 daily rows | `fill_dynamic_table_sec1` | `display_section1` |
| MTD/Monthly actuals | `fill_dynamic_table_sec2d` | `display_section2d` |
| Annual OVL summary (MMT) | `fill_dynamic_table_sec2a1/2/3` | `display_section2a1/2/3` |
| Historical 5-year (3f) | `fill_dynamic_table_sec3f` | `display_section3f` |
| Remarks section | N/A | `display_section4b` → `prepare_section4b_paste_data` |
| Sheet 2 trend data | `fill_dynamic_table_sec5a` | `display_section5a` |
| Sheet 3 chart table | `fill_dynamic_table_sec6` (→ `sec6a` + `sec6b`) | `display_section6` |

---

*Document prepared: May 2026. For issues contact the ABAP team maintaining ZPRA_DPR_REPORT_NEW1.*
