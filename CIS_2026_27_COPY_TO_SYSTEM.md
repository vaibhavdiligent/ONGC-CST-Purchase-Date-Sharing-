# CIS 2026-27 — What to Copy / Create in the SAP System

This is the definitive checklist to move everything into SAP. Do the steps
**in order** — the program will not activate until the DDIC objects exist.

---

## STEP 1 — Create DDIC objects (SE11)  *(do first)*
Field-level details are in `CIS_2026_27_DDIC_OBJECTS.md`.

| # | Object | Type | Requirement |
|---|---|---|---|
| 1 | `ZCIS_CUST_TYPE` | Transparent table (KUNNR → A/T) | R1 waiver, 200 MT cap |
| 2 | `ZCIS_WAIVER_RULE` | Transparent table (+ seed rows) | R1 waiver counts/floor |
| 3 | `ZCIS_SHORTFALL_GRD` | Transparent table | R2 shortfall auto |
| 4 | `ZCIS_NODISC_GRADE` | Transparent table | Non-discount grades (PS/GS/Powder/Polyfines) |
| 5 | `ZCIS_SCHEME_PARAM` | Transparent table (+ seed `TRADER_CAP_MT=200`) | 200 MT cap & scheme params |
| 6 | Data elements/domains | `ZCIS_CUST_TYPE, ZCIS_YEAR, ZCIS_MONTH, ZCIS_PERC` | typing |

## STEP 2 — Create program source (SE38)
| # | Program | Action |
|---|---|---|
| 1 | `YRVG004_QAIS_EXECUTE_N1` | Create (if not already) and paste `YRVG004_QAIS_EXECUTE_N1.abap` |
| 2 | `ZCIS_REBATE_REPORT` | Create and paste `ZCIS_REBATE_REPORT.abap` (R5 report) |

## STEP 3 — GUI status (SE41)
| # | Object | Action |
|---|---|---|
| 1 | GUI status `STANDARD` in `YRVG004_QAIS_EXECUTE_N1` | Copy from original `YRVG004_QAIS_EXECUTE` |

## STEP 4 — Transactions (SE93)
| # | T-code | Target |
|---|---|---|
| 1 | e.g. `ZCIS_EXECUTE` | Program `YRVG004_QAIS_EXECUTE_N1` |
| 2 | e.g. `ZCIS_SHORTFALL` | Parameter txn → `SM30`, view `ZCIS_SHORTFALL_GRD`, Update=X |
| 3 | e.g. `ZCIS_REBATE_RPT` | Program `ZCIS_REBATE_REPORT` |

## STEP 5 — Table maintenance generators (SE11 → Utilities)
Generate SM30 maintenance (function group `ZCIS`) for:
`ZCIS_CUST_TYPE`, `ZCIS_WAIVER_RULE`, `ZCIS_SHORTFALL_GRD`, `ZCIS_NODISC_GRADE`, `ZCIS_SCHEME_PARAM`.

## STEP 6 — Master / config data
| # | Data | Where |
|---|---|---|
| 1 | Waiver rules (seed table in DDIC doc) | `ZCIS_WAIVER_RULE` |
| 2 | Customer type A/T per customer | `ZCIS_CUST_TYPE` |
| 3 | Non-discount grades (PS/GS/Powder/Polyfines) — KONDM: **I2,I3,I4,I5,I6,I7 (PS), I8,I9,J0,J1,J2,J3 (GS), 74 (Polyfine), 75 (Powder)** [confirmed by GAIL 02.07.2026] | `ZCIS_NODISC_GRADE` |
| 4 | `TRADER_CAP_MT = 200` | `ZCIS_SCHEME_PARAM` |
| 5 | New seasonal grade **B63HM0003** (+ existing seasonal grades) | `YRVA_PRS_GRADES` (indicator S) |
| 6 | Group / MLE relationships (`ZGPGRP` / `ZGPMLL`, role `ZCUSBPX`) | **BP** per BP User Manual |

---

## STATUS of code delivered in `YRVG004_QAIS_EXECUTE_N1`

### Done & wired in code (✅)
- Period gating 01.06.2026–31.03.2027; Monthly 75% / Annual 80%.
- Error messages; Quarterly & Annual-Consistency radio buttons removed.
- Divide-by-zero guards; self-contained GUI status + `PF_STATUS_SET`.
- R1 waiver **floor 25%/50%** — config-driven, applied in all 12 monthly forms.
- Config loads for all Z tables; scheme param 200 MT read.
- Helper forms added & ready: `get_cust_wv_floor`, `get_group_mle_members`,
  `is_nodisc_grade`, `is_shortfall_grade`.

### Built as helpers — integration point to be placed & TESTED by developer (⚠️)
These affect financial calculations and/or depend on fields to be confirmed, so the
call is staged (helper ready) rather than wired blind:

| Item | Helper to call | Where to place | Needs |
|---|---|---|---|
| 200 MTM cap (Trader/AUT) | use `lv_trader_cap_mt` + `lv_cust_type` | where monthly MCQ / `w_month_max` is set | confirm cap applies to MCQ |
| ~~Non-discount grades no discount (pt.5)~~ **DONE** | via `r_nodisc` range | wired into existing `lv_no_dis_qty` exclusion (40 blocks) | KONDM list confirmed 02.07.2026 |
| ~~R2 shortfall auto-apply~~ **DONE** | `build_cis_shortfall` + `it_cis_shortfall` | wired at the monthly `w_waive_month` set (line ~7829) | ⚠️ verify `YRVA_QAIS_TNTLFT` grade field name |
| R1 waiver-count / max-1-per-qtr | `lv_wv_allowed` | monthly waiver grant point | confirm counter reset per quarter |
| R3 Group/MLE combined lifting | `get_group_mle_members` | group aggregation (currently `KVGR2`) | confirm replace vs add to KVGR2 |

### Not started — needs external input (🔴)
- Tentative lifting → MCQ/ACQ link (dev-form pt.9) — needs field definition.
- CIS Discount Structure (clause 11) — needs circular text.
- PP grades — Phase 2 (after PP plant starts).

### Excluded (agreed)
- R4 Maker–checker workflow + email (zonal/CPC).

---

## Confirmations still required from GAIL
1. **Customer type** (AU / Trader-AUT) source — new `ZCIS_CUST_TYPE`, or an existing field (`KDGRP`/`KVGR*`/BP role)?
2. **Clause 11** (discount structure) & **clause 8.I/8.II** (waiver) text.
3. **PS/GS/Powder/Polyfines** grade identification (`KONDM` values).
4. **Tentative-lifting** field for MCQ/ACQ.
5. Confirm `BUT050` `ZGPGRP` / `ZGPMLL` are the correct relationship categories.
