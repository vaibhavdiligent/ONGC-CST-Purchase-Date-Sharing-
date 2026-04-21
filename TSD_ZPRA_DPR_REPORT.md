# Technical Specification Document
## Program: ZPRA_DPR_REPORT — Daily Production Report (DPR)

| Field | Details |
|---|---|
| Program Name | ZPRA_DPR_REPORT |
| Title | DPR Report |
| Type | Executable Report (Type 1) |
| Author | VIVEK |
| Created | 10.04.2019 |
| Development Class | ZABAP |
| Includes | ZPRA_DPR_REPORT_TOP, ZPRA_DPR_REPORT_F01 |

---

## 1. Purpose

This program generates a **Daily Production Report (DPR)** for ONGC Videsh Limited (OVL). It reads hydrocarbon production data from SAP custom tables and produces a colour-coded, multi-sheet **Microsoft Excel workbook** showing production quantities for Oil, Condensate, LNG, and Gas across all configured overseas assets.

> **SAP Beginner Note:** SAP stores all company data in "tables" — think of them like sheets in a large database spreadsheet. This program reads those tables and formats the data into an Excel file that management can use for daily reporting.

---

## 2. Selection Screen (What the User Sees Before Running)

When the user runs transaction `ZPRA_DPR_REPORT`, they see five input blocks:

---

### Block 1 — Date Selection

| Field | Label | Type | Mandatory | Rule |
|---|---|---|---|---|
| P_DATE | Date | Date | Yes | Cannot be a future date |

The user enters the date for which the DPR is to be generated. The program internally works with **(date − 1)** as the data cutoff, and uses **P_DATE** as the report date shown on the output.

---

### Block 2 — Unit of Measurement (UoM) Selection

**Radio buttons — exactly one must be selected.**

| Parameter | Label | Default | Oil Unit | Condensate Unit | LNG Unit | Gas Unit | Grand Total Unit |
|---|---|---|---|---|---|---|---|
| P_BB | BBL / BOE | | BBL | BBL | BOE | BOE | BOE |
| P_BBD | BOPD / BOEPD | | BOPD | BCPD | BOEPD | BOEPD | BOEPD |
| P_TM | Tons / MSCM | | Tons | Tons | Tons | MSCM | TOE |
| P_TMD | TPD / MSCMD | | TPD | TPD | TPD | MSCMD | TOEPD |
| P_MB | MMT / BCM | | MMT | MMT | MMT | BCM | MMTOE |
| **P_BMD** | **BOPD / MMSCMD** | **✓ (default)** | BOPD | BCPD | MMSCMD | MMSCMD | BOEPD |

> **Plain English:** This controls the unit in which production numbers are displayed in the Excel report. For example, selecting "BOPD/MMSCMD" shows oil in Barrels Per Day and gas in Million Standard Cubic Metres Per Day.

**Important behaviour:** Section 2e (daily rate table) is only included in the output when **BOPD/BOEPD**, **TPD/MSCMD**, or **BOPD/MMSCMD** is selected.

---

### Block 3 — Consortium Level

**Radio buttons — exactly one must be selected.**

| Parameter | Label | Default | Meaning |
|---|---|---|---|
| **P_C_JV** | JV | **✓ (default)** | Joint Venture level — shows full consortium production, then multiplies by OVL's Participating Interest (PI%) to derive OVL's share |
| P_C_OLV | OVL | | ONGC Videsh level — multiplier is 1, no PI applied, shows OVL's direct share only |

> **Plain English:** OVL often holds only a partial share (e.g., 25%) in an overseas block. "JV" mode shows total field production and OVL's share side-by-side. "OVL" mode shows only OVL's share.

---

### Block 4 — Targets Selection

**Checkboxes — at least one must be checked (validated).**

| Parameter | Label | Default | Internal Code | Meaning |
|---|---|---|---|---|
| P_T_BE | BE | **✓ (default)** | TAR_BE | Budget Estimate |
| P_T_IN | Internal | | TAR_IN | Internal Target |
| P_T_EX | MOU Excellent | | TAR_EX | MOU Excellent Target |
| P_T_VG | MOU Very Good | | TAR_VG | MOU Very Good Target |
| P_T_PC | PC | | TAR_PC | Physical Control |
| P_T_RE | RE | | TAR_RE | Revised Estimate |

> **Plain English:** These are the different types of production targets set for the year. Checking "BE" will show Budget Estimate targets in the report alongside actual production. Multiple targets can be selected and they appear as separate rows.

**Validation:** If all checkboxes are unchecked, the program shows an error: *"Please select atleast one target"* and does not proceed.

---

### Block 5 — Directory Path

| Field | Label | Type | Mandatory |
|---|---|---|---|
| P_FNAME | Directory Path | String | Yes |

The user selects the local folder (on their PC) where the Excel file will be saved. A folder-browse popup appears when the user clicks the search icon.

---

## 3. Database Tables Used

> **SAP Beginner Note:** All table names starting with "Z" are custom tables built specifically for this project. Tables starting with "T" are standard SAP system tables.

### Configuration Tables (Static Setup Data)

| Table | Description | What It Stores |
|---|---|---|
| `ZPRA_C_DPR_PROF` | DPR Profile | Which assets and products are enabled for the DPR report, and how gas assets should be displayed (individual or combined) |
| `ZPRA_C_PRD_PROF` | Product Profile | All valid Asset–Block–Product combinations for the system |
| `ZOIU_PR_DN` | Asset Descriptions | Human-readable names/descriptions for each asset code |

### Transaction / Production Data Tables

| Table | Description | What It Stores |
|---|---|---|
| `ZPRA_T_DLY_PRD` | Daily Production | Actual production quantity per day per asset per product (used for current and previous month) |
| `ZPRA_T_DLY_RPRD` | Daily Revised Production | Revised/corrected daily production figures for gas assets |
| `ZPRA_T_MREC_PRD` | Monthly Reconciled Production | Finalized monthly production totals (used for historical months) |
| `ZPRA_T_MREC_APP` | Monthly Approved Production | Approved monthly production — used to identify months where daily data needs to be substituted |

### PI and Conversion Factor Tables

| Table | Description | What It Stores |
|---|---|---|
| `ZPRA_T_PRD_PI` | Production PI | OVL's Participating Interest (%) per asset-block with validity dates |
| `ZPRA_T_TAR_PI` | Target PI | PI (%) applicable to target calculations |
| `ZPRA_T_TAR_CF` | Target Conversion Factors | Conversion factors used to convert gas volumes to equivalent BOE/TOE units |

### Target Tables

| Table | Description | What It Stores |
|---|---|---|
| `ZPRA_T_PRD_TAR` | Production Targets | Annual/monthly target quantities per asset per product per target type (BE, RE, etc.) |

### Standard SAP Tables

| Table | Description | Used For |
|---|---|---|
| `T001` | Company Codes | Reads fiscal year variant (periv) for company code OVL |
| `T247` | Month Names | Gets the text name of a month number (e.g., 4 → "April") |

---

## 4. Key Concepts

### Participating Interest (PI)
OVL holds a percentage stake in each overseas block. For example, if OVL's PI in a block is 25%, and the field produced 1,000 BBL/day, OVL's share is 250 BBL/day.

- PI values are stored per asset-block in `ZPRA_T_PRD_PI` with validity date ranges.
- When **JV mode** is selected, the program calculates a **weighted average PI** for the period based on daily production volumes, then applies it to totals.
- When **OVL mode** is selected, no PI multiplication occurs (multiplier = 1).

### Conversion Factor (CF)
Gas is measured in volume units (MSCM, MMSCM, etc.) but needs to be expressed in energy-equivalent units (BOE, TOE) for cross-product comparison. CFs are stored in `ZPRA_T_TAR_CF`.

### Fiscal Year
OVL follows the **Indian fiscal year (April–March)**. The SAP function `DATE_TO_PERIOD_CONVERT` converts any calendar date to a fiscal period using the fiscal year variant stored in company code OVL in table `T001`.

### Combined Gas Assets
Some gas assets are marked as "combined" in `ZPRA_C_DPR_PROF` (display_opt = 'C'). These assets' gas production is **aggregated** into a single "Combined" column in the output rather than shown individually.

---

---

## 5. Program Flow

### High-Level Execution Steps

```
User runs ZPRA_DPR_REPORT
        │
        ▼
 AT SELECTION-SCREEN (Validation)
   • At least one target checkbox selected?
   • Date not in the future?
        │
        ▼
 START-OF-SELECTION
        │
        ├─ CLEAR_VARIABLES       ← Reset all global variables and tables
        ├─ FETCH_DATA            ← Read all required data from database
        ├─ PROCESS_GAS_RECORDS   ← Convert gas units, apply PI for main dataset
        └─ PROCESS_DATA          ← Build Excel workbook and write all sections
```

---

### Step 1: CLEAR_VARIABLES
Resets all internal tables (gt_*) and global variables (gv_*) to initial state.

---

### Step 2: FETCH_DATA

```
FETCH_DATA
├─ Set report date (gv_rep_date = P_DATE, working date = P_DATE − 1)
├─ Build product range: Oil(722000001), Condensate(722000003), LNG(722000005), Gas(722000004)
├─ Build volume type range: NET_PROD, GROSS_PROD, GAS_INJ
├─ Build target code range: from checked checkboxes (TAR_BE, TAR_IN, etc.)
│
├─ SELECT ZPRA_C_DPR_PROF  → assets configured for DPR
├─ SELECT ZPRA_C_PRD_PROF  → asset-block-product combinations
├─ SELECT T001             → fiscal year variant for company OVL
├─ Call DATE_TO_PERIOD_CONVERT    → current fiscal year and period
├─ Call FIRST_AND_LAST_DAY_IN_YEAR_GET → fiscal year start/end
│
├─ Calculate date windows (current month, previous month, YTD, days remaining)
├─ Build combined gas asset list (ZPRA_C_DPR_PROF where display_opt = 'C')
│
├─ SELECT ZPRA_T_DLY_PRD   → daily production (prev month to P_DATE−1)
├─ SELECT ZPRA_T_PRD_PI    → PI% per asset-block (with validity dates)
├─ SELECT ZPRA_T_MREC_PRD  → monthly reconciled production (20 months back, gas excluded)
├─ SELECT ZOIU_PR_DN       → asset description texts
│
├─ If targets selected:
│   ├─ SELECT ZPRA_T_TAR_PI  → target PI values
│   ├─ SELECT ZPRA_T_PRD_TAR → target quantities (current year/month)
│   └─ SELECT ZPRA_T_TAR_CF  → conversion factors (current year)
│
└─ SELECT ZPRA_T_TAR_CF (all years) → conversion factors for unit conversion
```

---

### Step 3: PROCESS_GAS_RECORDS
Converts raw gas volumes in the main daily production table to the user-selected unit.
Handles combined gas assets aggregation.

---

### Step 4: PROCESS_DATA — Excel Generation

```
PROCESS_DATA
├─ START_EXCEL              → Launch Excel via OLE, create 3 worksheets
├─ PROCESS_SEC1_DATA        → Section 1: Daily Production Table
├─ PROCESS_SEC2_DATA        → Sections 2a–2f: Monthly Summary panels
├─ PROCESS_SEC3_DATA        → Section 3: Historical/Trend data
├─ PROCESS_SEC4_DATA        → Section 4: Combined Gas Assets detail
├─ [Switch to Worksheet 2]
├─ PROCESS_SEC5_DATA        → Section 5: Long-term Production Trend
├─ PROCESS_SEC6_DATA        → Section 6: Management Summary Dashboard
├─ [Switch back to Worksheet 1]
├─ SET_COLUMNS_WIDTH / BORDER_CELLS / SET_CELL_FORMATS
└─ FINALIZE_WORKSHEET       → Save Excel file to selected directory
```

---

## 6. Excel Output — Section-by-Section

The output workbook has **3 sheets**: Main DPR (Sections 1–4, 6), Trend (Section 5), Chart.

---

### Section 1 — Daily Production Table

**Purpose:** Day-by-day production for current month + tail of previous month.

**Columns:** Date | Asset-1 | Asset-2 | ... | Product Total | Grand Total

**Header rows:** Logo, Report Date, Product Names, Asset Names, PI%, Conversion Factors, Consortium Level, Unit of Measurement

**Data source:** `ZPRA_T_DLY_PRD`

---

### Section 2 — Monthly Summary Panels

| Sub-Section | Label in Excel | Source Table | Period |
|---|---|---|---|
| 2a1 | Target rows (per target type) | ZPRA_T_PRD_TAR | Current year Annual + YTD |
| 2a2 | Prod.: MTD Actual | ZPRA_T_DLY_PRD | Current month sum |
| 2a3 | Last month actuals | ZPRA_T_MREC_PRD | Previous month reconciled |
| 2b | Full year monthly actuals | ZPRA_T_MREC_PRD | All months current fiscal year |
| 2c | YTD monthly actuals | ZPRA_T_MREC_PRD | April → current month |
| 2d | Last 2 months daily | ZPRA_T_DLY_PRD | Current + previous month |
| 2e | Per-day rates *(BOPD/BOEPD, TPD/MSCMD, BOPD/MMSCMD only)* | Derived from 2d | Same as 2d |
| 2f | Last month daily detail | ZPRA_T_DLY_PRD | Previous month only |

> Section 2e is **skipped** when BBL/BOE, Tons/MSCM, or MMT/BCM is selected (cumulative units, not daily rates).

---

### Section 3 — Historical Trend Data

| Sub-Section | Content | Source | Period |
|---|---|---|---|
| 3a | YTD actuals month-by-month | ZPRA_T_MREC_PRD | Current FY April → now |
| 3b | Last year YTD (same months) | ZPRA_T_MREC_PRD | Previous FY same period |
| 3c | Monthly actuals with gap-filling | ZPRA_T_DLY_PRD + ZPRA_T_MREC_PRD + ZPRA_T_MREC_APP | Current FY (daily substituted where monthly not approved) |
| 3d | Prior year monthly | ZPRA_T_MREC_PRD | Previous FY monthly |
| 3e | Gas injection monthly | ZPRA_T_MREC_PRD | Current FY (GAS_INJ) |
| 3f | 5-year monthly history | ZPRA_T_MREC_PRD | Last 5 fiscal years |

**Gap-filling rule (3c):** For each month in current FY, if no record exists in `ZPRA_T_MREC_APP`, daily rows from `ZPRA_T_DLY_PRD` are aggregated as the substitute monthly value.

---

### Section 4 — Combined Gas Assets

**Purpose:** Detailed daily and monthly gas data for assets marked as "combined" (display_opt = 'C').

| Sub-Section | Content | Source |
|---|---|---|
| 4a | Monthly gas (last year + current year) for combined assets | ZPRA_T_DLY_RPRD + ZPRA_T_DLY_PRD + ZPRA_T_MREC_PRD |
| 4b | Remarks / Comments | Static |

Run date and time stamp printed at bottom of this section.

---

### Section 5 — Production Trend (Worksheet 2)

| Content | Source | Period |
|---|---|---|
| Daily actuals (current FY) | ZPRA_T_DLY_PRD | FY start → P_DATE |
| Historical monthly actuals | ZPRA_T_MREC_PRD | Last 3 fiscal years |

Provides a continuous production view from 3 years back through today.

---

### Section 6 — Management Summary Dashboard

| Row | Content |
|---|---|
| Actual | MTD actual Oil+LNG+Condensate, Gas, Total (from Section 2d) |
| Target rows | One per selected target type — Annual and YTD values (from Section 2b) |
| Achievement % | Actual ÷ Target × 100 (Annual and YTD) |

**Columns:** FY Label | Oil Annual | Oil YTD | Gas Annual | Gas YTD | Total Annual | Total YTD

---

### Chart (Worksheet 3)

Auto-generated Excel column chart: **"Production Performance YYYY-YY"**
- Y-axis = selected unit (e.g., BOEPD, MMTOE)
- Source = Section 5 trend data
- Legend positioned at bottom-left

---

## 7. Radio Button Impact Summary

### UoM Radio Buttons — Unit Labels Applied Per Product

| UoM Option | Oil | Condensate | LNG | Gas | Grand Total |
|---|---|---|---|---|---|
| BBL / BOE | BBL | BBL | BOE | BOE | BOE |
| BOPD / BOEPD | BOPD | BCPD | BOEPD | BOEPD | BOEPD |
| Tons / MSCM | Tons | Tons | Tons | MSCM | TOE |
| TPD / MSCMD | TPD | TPD | TPD | MSCMD | TOEPD |
| MMT / BCM | MMT | MMT | MMT | BCM | MMTOE |
| BOPD / MMSCMD *(default)* | BOPD | BCPD | MMSCMD | MMSCMD | BOEPD |

The selected unit appears in the column header row of every section of the report. All numeric values are converted to the corresponding unit before being written to Excel.

### Consortium Radio Buttons — Effect on PI Application

| Option | PI Multiplication | Header Label |
|---|---|---|
| JV *(default)* | Weighted average PI% applied per asset | "JV" |
| OVL | Multiplier = 1.0 (no PI applied) | "ONGC Videsh" |

When JV is selected, the PI weighted average is calculated as:
```
Weighted PI = Sum(daily_quantity × PI%) / Sum(daily_quantity)
```
The weighted PI is then applied to each total cell.

### Target Checkboxes — How Targets Appear in Output

Each checked target type results in:
1. One header row in **Section 2a1** labelled with the target name ("BE Target", "Internal Target", etc.)
2. Annual target value and YTD target value columns for each asset and product
3. One row in **Section 6** showing target vs. actual comparison

---

## 8. Error Handling

| Condition | Error Message | Type |
|---|---|---|
| No assets in DPR profile | "No assets configured for DPR Report" | E (Abend) |
| No product profile entries | "No Assets found in product profile" | E (Abend) |
| No production data found | "No data found for the selection" | E (Abend) |
| Company code OVL missing | "Company Code OVL does not exist" | E (Abend) |
| Fiscal year conversion error | "Error getting Period" | E (Abend) |
| All target checkboxes blank | "Please select atleast one target" | E (stops at selection screen) |
| Future date entered | "DPR cannot be run for future dates" | E (stops at selection screen) |
| Folder browse fails | "Error getting Directory Name" | E (Abend) |

> **E type messages** abort the program immediately and show the message to the user.

---

## 9. Performance Notes

- All database SELECTs use `FOR ALL ENTRIES IN` to avoid full table scans — only fetching data for the configured assets.
- The program uses **dynamic internal tables** (created at runtime) so the number of columns in the Excel output adapts automatically to the number of configured assets and products.
- Data is written to Excel using **OLE Automation** — Excel must be installed on the SAP GUI client machine.
- A clipboard-based paste mechanism (`cl_gui_frontend_services=>clipboard_export`) is used for bulk data transfer to Excel for performance.

---

*Document generated from source code analysis of ZPRA_DPR_REPORT extracted from DPR_REPORT.pdf*
