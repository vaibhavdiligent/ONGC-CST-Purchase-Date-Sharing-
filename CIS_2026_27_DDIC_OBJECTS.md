# CIS 2026-27 — DDIC / Repository Objects to Create in SAP

These objects must be **created in the SAP system (SE11 / SE80 / SE93)** before the
enhanced `YRVG004_QAIS_EXECUTE_N1` and the new programs can be activated.
The ABAP code already references these exact table and field names.

> Naming (`ZCIS_*`) is a proposal — align with GAIL standards, and if you rename a
> table/field, tell me and I'll update the code to match.

---

## R1 — Customer Waiver

### Table `ZCIS_CUST_TYPE` (transparent, master)
Customer → type classification (Actual User vs AUT/Trader).

| Field | Key | Data element / type | Description |
|---|---|---|---|
| `MANDT` | ✔ | MANDT | Client |
| `KUNNR` | ✔ | KUNNR | Customer |
| `CUST_TYPE` |   | `ZCIS_CUST_TYPE` (CHAR1) | `A` = Actual User, `T` = AUT/Trader |

*Data element `ZCIS_CUST_TYPE` — domain CHAR1, fixed values A / T.*
⚠️ If customer type is already derivable from an existing field (e.g. `KNVV-KDGRP`,
a `KVGR*`, or a BP role), this table can be replaced by that field — please confirm.

### Table `ZCIS_WAIVER_RULE` (transparent, config)
Waiver rule by customer type and CIS signing-month band.

| Field | Key | Data element / type | Description |
|---|---|---|---|
| `MANDT` | ✔ | MANDT | Client |
| `SCHEME_YEAR` | ✔ | `ZCIS_YEAR` (CHAR7, e.g. 2026-27) | Scheme year |
| `CUST_TYPE` | ✔ | `ZCIS_CUST_TYPE` | A / T |
| `SIGN_FROM` | ✔ | `ZCIS_MONTH` (NUMC2) | Signing month band from (MM, 01–12) |
| `SIGN_TO` |   | `ZCIS_MONTH` (NUMC2) | Signing month band to (MM) |
| `MIN_LIFT_PERC` |   | `ZCIS_PERC` (DEC3) | Min lifting % in a waiver month (25 / 50) |
| `WV_COUNT` |   | INT1 | No. of monthly waivers allowed |
| `MAX_PER_QTR` |   | INT1 | Max waivers per quarter (default 1) |
| `VALID_FROM` |   | BEGDA (DATS) | Rule valid from |
| `VALID_TO` |   | ENDDA (DATS) | Rule valid to |

**Seed data for 2026-27:**
| SCHEME_YEAR | CUST_TYPE | SIGN_FROM | SIGN_TO | MIN_LIFT_PERC | WV_COUNT | MAX_PER_QTR |
|---|---|---|---|---|---|---|
| 2026-27 | A | 06 | 07 | 25 | 2 | 1 |
| 2026-27 | A | 08 | 09 | 25 | 1 | 1 |
| 2026-27 | A | 10 | 03 | 25 | 0 | 0 |
| 2026-27 | T | 06 | 09 | 50 | 1 | 1 |
| 2026-27 | T | 10 | 03 | 50 | 0 | 0 |

*(“10–03” = Oct→Mar; maintain as two rows 10–12 and 01–03 if wrap-around is not desired.)*

---

## R2 — Shortfall Grade Waivers

### Table `ZCIS_SHORTFALL_GRD` (transparent, config)
Month/period-wise grades declared shortfall by the process owner.

| Field | Key | Data element / type | Description |
|---|---|---|---|
| `MANDT` | ✔ | MANDT | Client |
| `PERIOD_FROM` | ✔ | BEGDA (DATS) | Period from (month start) |
| `PERIOD_TO` | ✔ | ENDDA (DATS) | Period to (month end) |
| `GRADE` | ✔ | `YY_GRADE` (as in YRVA_PRS_GRADES) | Grade declared shortfall |
| `PRS_IND` |   | CHAR1 | P / R / S indicator (optional) |
| `CREATED_BY` |   | UNAME | Process owner |
| `CREATED_ON` |   | DATS | Entry date |

### Maintenance transaction (R2 T-code)
- Generate **table maintenance (SM30)** for `ZCIS_SHORTFALL_GRD` (SE11 → Utilities → Table Maintenance Generator, one-step, function group `ZCIS`).
- Create a **parameter transaction** (SE93) e.g. `ZCIS_SHORTFALL` → calls `SM30` with `VIEWNAME = ZCIS_SHORTFALL_GRD`, `UPDATE = X` (skip first screen).
- Add an **authorization group** so only the process owner can maintain it.

*(A custom module-pool screen can replace SM30 if a richer UI is required — advise if needed.)*

---

## R5 — Rebate Order Report

### Program `ZCIS_REBATE_REPORT` (already written — file `ZCIS_REBATE_REPORT.abap`)
- Create executable program `ZCIS_REBATE_REPORT` (SE38), paste the source.
- Create transaction (SE93) e.g. `ZCIS_REBATE_RPT` → program `ZCIS_REBATE_REPORT`.
- ⚠️ Adjust default `p_auart` to the actual CIS credit-memo-request document type.

---

## Dev-Form point 5 — Non-discount grades (PS / GS / Powder / Polyfines)

### Table `ZCIS_NODISC_GRADE` (transparent, config)
Grades that count for eligibility / MCQ but receive **no** monthly/annual discount.

| Field | Key | Data element / type | Description |
|---|---|---|---|
| `MANDT` | ✔ | MANDT | Client |
| `KONDM` | ✔ | KONDM | Material pricing group / grade (S922-KONDM) |
| `DESCR` |   | CHAR40 | Description (PS / GS / Powder / Polyfines) |

**Seed data (confirmed by GAIL, mail 02.07.2026):**
| KONDM | Description |
|---|---|
| I2 | PS GLN HDPE |
| I3 | PS GLN LLDPE |
| I4 | PS GLX HDPE |
| I5 | PS GLX HDPE-2 |
| I6 | PS HDPE PC-II |
| I7 | PS LLDPE PC-II |
| I8 | GS GLN HDPE |
| I9 | GS GLN LLDPE |
| J0 | GS GLX HDPE |
| J1 | GS GLX HDPE-2 |
| J2 | GS HDPE PC-II |
| J3 | GS LLDPE PC-II |
| 74 | Poly Fine GL / Poly Fine GL PC-II |
| 75 | Powder GLX / GLX-2 / HDPE PC-II / LLDPE PC-II |

### Table `ZCIS_SCHEME_PARAM` (transparent, config)
Generic scheme numeric parameters (avoids hard-coding, e.g. 200 MTM cap).

| Field | Key | Data element / type | Description |
|---|---|---|---|
| `MANDT` | ✔ | MANDT | Client |
| `PARAM_KEY` | ✔ | CHAR20 | e.g. `TRADER_CAP_MT` |
| `PARAM_VAL` |   | DEC15.3 | Value (e.g. 200) |
| `DESCR` |   | CHAR40 | Description |

**Seed:** `TRADER_CAP_MT = 200` (Trader/AUT monthly cap in MT).

---

## R3 — Group / MLE  *(logic received — BP User Manual)*
No new **table** needed — mapping is standard **BP relationships**, read from **`BUT050`**:
- Group  → relationship category **`TZGPGRP`** ("Has Group Customer")
- MLE    → relationship category **`TZGPMLL`** ("Has Multi Location Entity")
- BP role **`ZCUSBPX`**, with Valid-From / Valid-To.

Code side: `FORM get_group_mle_members` (added to `N1`) reads `BUT050` for the flagship
BP and returns members valid on the scheme date. **Action for business:** maintain the
Group/MLE relationships in BP as per the User Manual.

---

## R4 — Workflow  *(excluded from this build, per instruction)*
Objects listed in `CIS_2026_27_Change_Plan.docx`; not built here.
