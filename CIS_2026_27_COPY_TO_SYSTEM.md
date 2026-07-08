# CIS 2026-27 — What to Copy / Create in the SAP System

This is the definitive checklist to move everything into SAP. Do the steps
**in order** — the program will not activate until the DDIC objects exist.

---

## STEP 1 — Create DDIC objects (SE11)  *(do first)*
Field-level details are in `CIS_2026_27_DDIC_OBJECTS.md`.

| # | Object | Type | Requirement |
|---|---|---|---|
| 1 | `YCIS_WAIVER_RULE` | Transparent table (+ seed rows) | R1 waiver counts/floor |
| 2 | `YCIS_SHORTFALL` | Transparent table | R2 shortfall auto |
| 3 | `YCIS_NODISC_GRD` | Transparent table | Non-discount grades (PS/GS/Powder/Polyfines) |
| 4 | Data elements/domains | `YCIS_CTYPE, YCIS_YEAR, YCIS_MONTH, YCIS_PERC, YCIS_COUNT` | typing |

> Dropped per GAIL 07.07.2026: `YCIS_CUST_TYPE` (customer type read from existing
> `YRVA_QAIS_DATA-YY_CUSCLASS`) and `YCIS_SCH_PARAM` (200 MT cap already in YRVG004).

## STEP 2 — Create program source (SE38)
| # | Program | Action |
|---|---|---|
| 1 | `YRVG004_QAIS_EXECUTE_N1` | Create (if not already) and paste `YRVG004_QAIS_EXECUTE_N1.abap` |
| 2 | `YCIS_REBATE_REPORT` | Create and paste `YCIS_REBATE_REPORT.abap` (R5 report) |

## STEP 3 — GUI status (SE41)
| # | Object | Action |
|---|---|---|
| 1 | GUI status `STANDARD` in `YRVG004_QAIS_EXECUTE_N1` | Copy from original `YRVG004_QAIS_EXECUTE` |

## STEP 4 — Transactions (SE93)
| # | T-code | Target |
|---|---|---|
| 1 | e.g. `YCIS_EXECUTE` | Program `YRVG004_QAIS_EXECUTE_N1` |
| 2 | e.g. `YCIS_SHORTFALL` | Parameter txn → `SM30`, view `YCIS_SHORTFALL`, Update=X |
| 3 | e.g. `YCIS_REBATE_RPT` | Program `YCIS_REBATE_REPORT` |

## STEP 5 — Table maintenance generators (SE11 → Utilities)
Generate SM30 maintenance (function group `YCIS`) for:
`YCIS_WAIVER_RULE`, `YCIS_SHORTFALL`, `YCIS_NODISC_GRD`.

## STEP 6 — Master / config data
| # | Data | Where |
|---|---|---|
| 1 | Waiver rules (seed table in DDIC doc) | `YCIS_WAIVER_RULE` |
| 2 | Customer type A/T per customer | *existing field* `YRVA_QAIS_DATA-YY_CUSCLASS` (no new table) |
| 3 | Non-discount grades (PS/GS/Powder/Polyfines) — KONDM: **I2,I3,I4,I5,I6,I7 (PS), I8,I9,J0,J1,J2,J3 (GS), 74 (Polyfine), 75 (Powder)** [confirmed by GAIL 02.07.2026] | `YCIS_NODISC_GRD` |
| 4 | 200 MT cap | *already in `YRVG004` at CIS creation (no new table)* |
| 5 | New seasonal grade **B63HM0003** (+ existing seasonal grades) | `YRVA_PRS_GRADES` (indicator S) |
| 6 | Group / MLE relationships (`ZGPGRP` / `ZGPMLL`, role `ZCUSBPX`) | **BP** per BP User Manual |

---

## STATUS of code delivered in `YRVG004_QAIS_EXECUTE_N1`

### Done & wired in code (✅)
- Period gating 01.06.2026–31.03.2027; Monthly 75% / Annual 80%.
- Error messages; Quarterly & Annual-Consistency radio buttons removed.
- Divide-by-zero guards; self-contained GUI status + `PF_STATUS_SET`.
- R1 waiver **floor 25%/50%** — config-driven, applied in all 12 monthly forms.
- Customer type (A/T) read from existing `YRVA_QAIS_DATA-YY_CUSCLASS` (`'TRADER'` ⇒ T).
- Config loads for the 3 `YCIS_*` tables.
- Helper forms added & ready: `get_cust_wv_floor`, `get_group_mle_members`,
  `is_nodisc_grade`, `build_cis_shortfall`.

### Built as helpers — integration point to be placed & TESTED by developer (⚠️)
These affect financial calculations and/or depend on fields to be confirmed, so the
call is staged (helper ready) rather than wired blind:

| Item | Helper to call | Where to place | Needs |
|---|---|---|---|
| ~~200 MTM cap (Trader/AUT)~~ **Not needed** | — | already handled in `YRVG004` at CIS creation (GAIL 07.07.2026) | — |
| ~~Non-discount grades no discount (pt.5)~~ **DONE** | via `r_nodisc` range | wired into existing `lv_no_dis_qty` exclusion (40 blocks) | KONDM list confirmed 02.07.2026 |
| ~~R2 shortfall auto-apply~~ **DONE** | `build_cis_shortfall` + `it_cis_shortfall` | wired at the monthly `w_waive_month` set (line ~7829) | ⚠️ verify `YRVA_QAIS_TNTLFT` grade field name |
| R1 waiver-count / max-1-per-qtr | `lv_wv_allowed` | monthly waiver grant point | confirm counter reset per quarter |
| R3 Group/MLE clubbing | — | **NO aggregation code change** — clubbing stays on `KVGR2` (Pankaj: "not going to replace KVGR2"). BP Group/MLE (`ZGPGRP`/`ZGPMLL`) is the governance/validity layer; BIS keeps the group's `KVGR2` per SOP. `get_group_mle_members` retained for reporting/validation only. | ⚠️ confirm no cross-KVGR2 clubbing required in the program |

### Not started — needs external input (🔴)
- Tentative lifting → MCQ/ACQ link (dev-form pt.9) — needs field definition.
- CIS Discount Structure (clause 11) — needs circular text.
- PP grades — Phase 2 (after PP plant starts).

### Excluded (agreed)
- R4 Maker–checker workflow + email (zonal/CPC).

---

## Confirmations still required from GAIL
1. ~~**Customer type** source~~ **Resolved 07.07.2026** — read from existing `YRVA_QAIS_DATA-YY_CUSCLASS` (`'TRADER'` ⇒ T, else A).
2. **Clause 11** (discount structure) & **clause 8.I/8.II** (waiver) text.
3. **PS/GS/Powder/Polyfines** grade identification (`KONDM` values).
4. **Tentative-lifting** field for MCQ/ACQ.
5. Confirm `BUT050` `ZGPGRP` / `ZGPMLL` are the correct relationship categories.
