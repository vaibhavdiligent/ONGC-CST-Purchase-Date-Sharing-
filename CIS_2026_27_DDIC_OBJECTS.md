# CIS 2026-27 — DDIC / Repository Objects to Create in SAP

These objects must be **created in the SAP system (SE11 / SE80 / SE93)** before the
enhanced `YRVG004_QAIS_EXECUTE_N1` and the new programs can be activated.
The ABAP code already references these exact table and field names.

> Naming (`YCIS_*`) is a proposal — align with GAIL standards, and if you rename a
> table/field, tell me and I'll update the code to match.

---

## R1 — Customer Waiver

> **No customer-type mapping table is needed.** GAIL confirmed (07.07.2026) that the
> customer classification is already captured in the existing CIS table
> **`YRVA_QAIS_DATA-YY_CUSCLASS`** (Customer Classification). The program reads that
> field directly: `YY_CUSCLASS = 'TRADER'` ⇒ **T** (AUT / Trader); anything else ⇒ **A**
> (Actual User). *(The earlier `YCIS_CUST_TYPE` table is dropped.)*
> The `YCIS_CTYPE` data element/domain (CHAR1, fixed values A/T) is retained because it
> types the `CUST_TYPE` key of `YCIS_WAIVER_RULE` below.

### Table `YCIS_WAIVER_RULE` (transparent, config)
Waiver rule by customer type and CIS signing-month band.

| Field | Key | Data element / type | Description |
|---|---|---|---|
| `MANDT` | ✔ | MANDT | Client |
| `SCHEME_YEAR` | ✔ | `YCIS_YEAR` (CHAR7, e.g. 2026-27) | Scheme year |
| `CUST_TYPE` | ✔ | `YCIS_CTYPE` | A / T |
| `SIGN_FROM` | ✔ | `YCIS_MONTH` (NUMC2) | Signing month band from (MM, 01–12) |
| `SIGN_TO` |   | `YCIS_MONTH` (NUMC2) | Signing month band to (MM) |
| `MIN_LIFT_PERC` |   | `YCIS_PERC` (DEC3) | Min lifting % in a waiver month (25 / 50) |
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

## R2 — Shortfall Waivers (declared by material)

### Table `YCIS_SHORTFALL` (transparent, config)
Month/period-wise **materials** declared shortfall by the process owner.

> **Key field is `MATNR` (material number), not a grade code.** The signed-grade
> table `YRVA_QAIS_TNTLFT` stores the signed item as `MATNR` (CHAR 40) — confirmed
> from DDIC 06.07.2026 — so shortfall is declared and matched at material level.
> (Original plan used a grade/KONDM field; changed to `MATNR` per customer approval.)

| Field | Key | Data element / type | Description |
|---|---|---|---|
| `MANDT` | ✔ | MANDT | Client |
| `PERIOD_FROM` | ✔ | BEGDA (DATS) | Period from (month start) |
| `PERIOD_TO` | ✔ | ENDDA (DATS) | Period to (month end) |
| `MATNR` | ✔ | MATNR (CHAR 40) | Material declared shortfall (matches YRVA_QAIS_TNTLFT-MATNR) |
| `CREATED_BY` |   | UNAME | Process owner |
| `CREATED_ON` |   | DATS | Entry date |

### Maintenance transaction (R2 T-code)
- Generate **table maintenance (SM30)** for `YCIS_SHORTFALL` (SE11 → Utilities → Table Maintenance Generator, one-step, function group `YCIS`).
- Create a **parameter transaction** (SE93) e.g. `YCIS_SHORTFALL` → calls `SM30` with `VIEWNAME = YCIS_SHORTFALL`, `UPDATE = X` (skip first screen).
- Add an **authorization group** so only the process owner can maintain it.

*(A custom module-pool screen can replace SM30 if a richer UI is required — advise if needed.)*

---

## R5 — Rebate Order Report

### Program `YCIS_REBATE_REPORT` (already written — file `YCIS_REBATE_REPORT.abap`)
- Create executable program `YCIS_REBATE_REPORT` (SE38), paste the source.
- Create transaction (SE93) e.g. `YCIS_REBATE_RPT` → program `YCIS_REBATE_REPORT`.
- ⚠️ Adjust default `p_auart` to the actual CIS credit-memo-request document type.

---

## Dev-Form point 5 — Non-discount grades (PS / GS / Powder / Polyfines)

### Table `YCIS_NODISC_GRD` (transparent, config)
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

### ~~Table `YCIS_SCH_PARAM`~~ — not required
> GAIL confirmed (07.07.2026) that the **200 MT upper-capping is already handled inside
> `YRVG004` at CIS creation** (first radio button). No scheme-parameter table is needed,
> so `YCIS_SCH_PARAM` (and the `YCIS_PARAM_KEY` / `YCIS_PARAM_VAL` data elements) are dropped.

---

## R3 — Group / MLE  *(logic confirmed — Mr. Pankaj Wadhwa)*
No new **table** needed — all standard CVI / BP tables. Relationship categories:
- Group → **`ZGPGRP`**  ("Has Group Customer")
- MLE   → **`ZGPMLL`**  ("Has Multi Location Entity")

**Derivation (flagship customer code → member customer codes):**
1. `CVI_CUST_LINK-CUSTOMER` = flagship KUNNR → `CVI_CUST_LINK-PARTNER_GUID`
2. `BUT000-PARTNER_GUID` = that GUID → `BUT000-PARTNER` (BP number)
3. `BUT050-PARTNER1` = BP number, `RELTYP = ZGPGRP` / `ZGPMLL` → `BUT050-PARTNER2` (member BPs), valid on scheme date (`DATE_FROM`/`DATE_TO`)
4. For each member BP: `BUT000-PARTNER` → `PARTNER_GUID` → `CVI_CUST_LINK-CUSTOMER` (member KUNNR)

Code side: `FORM get_group_mle_members` (in `N1`) implements exactly this and returns the
member customer codes (flagship included). **Action for business:** maintain Group/MLE
relationships in BP (T-code BP, role `ZCUSBPX`) per the User Manual, with validity dates —
quantity clubbing is governed by the relationship validity period.

---

## R4 — Workflow  *(excluded from this build, per instruction)*
Objects listed in `CIS_2026_27_Change_Plan.docx`; not built here.
