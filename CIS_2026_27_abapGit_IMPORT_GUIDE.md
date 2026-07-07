# CIS 2026-27 — abapGit ZIP Import Guide

Import file: **`CIS_2026_27_abapGit.zip`** (upload via `ZABAPGIT_STANDALONE` → *Import ZIP*).

The ZIP is a standard offline abapGit repository (`.abapgit.xml` + `/src/`), so it
imports the same way as any abapGit offline project.

---

## What's inside (20 objects)

All objects are in the **`Y`** customer namespace.

**Domains (7):** `YCIS_CTYPE` (fixed values A/T), `YCIS_YEAR`, `YCIS_MONTH`,
`YCIS_PERC`, `YCIS_COUNT`, `YCIS_PARAM_KEY`, `YCIS_PARAM_VAL`

**Data elements (7):** same names as the domains above.

**Tables (5):**
| Table | Key fields |
|---|---|
| `YCIS_CUST_TYPE` | MANDT, KATR2 |
| `YCIS_WAIVER_RULE` | MANDT, SCHEME_YEAR, CUST_TYPE, SIGN_FROM |
| `YCIS_SHORTFALL` | MANDT, PERIOD_FROM, PERIOD_TO, **MATNR** |
| `YCIS_NODISC_GRD` | MANDT, KONDM |
| `YCIS_SCH_PARAM` | MANDT, PARAM_KEY |

**Program (1):** `YCIS_REBATE_REPORT`

> ⚠️ **`YRVG004_QAIS_EXECUTE_N1` is NOT in this ZIP** — it already exists in the
> system, so it is copied in manually (paste the source from the repo file
> `YRVG004_QAIS_EXECUTE_N1.abap`, which already references the new `YCIS_*` tables).

> **Table names were shortened to fit SAP's 16-character limit.** Original (first
> proposed) name → final table name now used in the ABAP source:
> | First proposed | Final table (SAP) |
> |---|---|
> | `ZCIS_SHORTFALL_GRD` (18) | `YCIS_SHORTFALL` (14) |
> | `ZCIS_NODISC_GRADE` (17) | `YCIS_NODISC_GRD` (15) |
> | `ZCIS_SCHEME_PARAM` (17) | `YCIS_SCH_PARAM` (14) |
>
> All objects were also moved from the `Z` to the `Y` namespace. The customer-type
> **data element** is `YCIS_CTYPE` (not `YCIS_CUST_TYPE`) — SAP does not allow a
> table and a data element to share the same name.

> Standard data elements referenced (MANDT, MATNR, KATR2, KONDM, BEGDA, ENDDA,
> UNAME, DATUM, TEXT40) already exist in every system — not included, by design.

---

## Import steps

1. Run **`ZABAPGIT_STANDALONE`** → **Import ZIP** (or *+ new offline* → *Import ZIP*).
2. Choose/create the **target package** (e.g. `YCIS`) and upload `CIS_2026_27_abapGit.zip`.
3. **Pull / Import** all objects.
4. **Activate** in this order (abapGit's mass-activate usually handles it):
   Domains → Data elements → Tables → Programs.

---

## Manual steps required AFTER import

These cannot be carried in the ZIP and must be done in the system:

1. **Create `YRVG004_QAIS_EXECUTE_N1` manually** (not in the ZIP).
   SE38 → create program `YRVG004_QAIS_EXECUTE_N1` (type 1 / executable,
   message class `YV01`) → paste the source from `YRVG004_QAIS_EXECUTE_N1.abap`.
   Do this **after** the DDIC objects are active, so it activates cleanly.

2. **GUI status `STANDARD` for `YRVG004_QAIS_EXECUTE_N1`** ⚠️ *required to run*
   The program sets `PF-STATUS 'STANDARD'` (its own status). Copy the GUI status
   `STANDARD` from the original **`YRVG004_QAIS_EXECUTE`** into the new program:
   SE41 → *Status* → *Copy from program* `YRVG004_QAIS_EXECUTE`, status `STANDARD`.
   (A GUI status is a CUA object and is not part of the ABAP source.)

2. **Table maintenance (SM30)** — SE11 → each `YCIS_*` table → *Utilities → Table
   Maintenance Generator* (one-step, function group `YCIS`, auth group as needed).

3. **Transactions (SE93)** — e.g. `YCIS_EXECUTE` → `YRVG004_QAIS_EXECUTE_N1`;
   `YCIS_SHORTFALL` → parameter txn to `SM30` on `YCIS_SHORTFALL`;
   `YCIS_REBATE_RPT` → `YCIS_REBATE_REPORT`.

4. **Seed data** (SM30/SE16) — see `CIS_2026_27_DDIC_OBJECTS.md` for the rows:
   - `YCIS_WAIVER_RULE` — 5 approved rows (2026-27).
   - `YCIS_NODISC_GRD` — KONDM I2–I7, I8–J3, 74, 75 (confirmed 02.07.2026).
   - `YCIS_SCH_PARAM` — `TRADER_CAP_MT = 200`.
   - `YCIS_CUST_TYPE` — KATR2 → A/T mapping (values to be confirmed by GAIL).
   - `YCIS_SHORTFALL` — **material numbers** declared shortfall per period.

5. **`YCIS_REBATE_REPORT`** — set default `p_auart` to the actual CIS
   credit-memo-request document type before go-live.

---

## Notes

- **Delivery class** of the tables is `C` (customizing) — adjust if your standards differ.
- **`YCIS_SHORTFALL` is keyed on `MATNR`** (material), matching how signed items
  are stored in `YRVA_QAIS_TNTLFT` (confirmed from DDIC). The program's
  `build_cis_shortfall` matches TNTLFT-MATNR against this table directly.
- If you rename any table/field, tell me and I'll re-sync the ABAP + regenerate the ZIP.
