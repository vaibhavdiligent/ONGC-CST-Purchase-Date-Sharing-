# CIS 2026-27 — abapGit ZIP Import Guide

Import file: **`CIS_2026_27_abapGit.zip`** (upload via `ZABAPGIT_STANDALONE` → *Import ZIP*).

The ZIP is a standard offline abapGit repository (`.abapgit.xml` + `/src/`), so it
imports the same way as any abapGit offline project.

---

## What's inside (24 objects)

**Domains (7):** `ZCIS_CUST_TYPE` (fixed values A/T), `ZCIS_YEAR`, `ZCIS_MONTH`,
`ZCIS_PERC`, `ZCIS_COUNT`, `ZCIS_PARAM_KEY`, `ZCIS_PARAM_VAL`

**Data elements (7):** same names as the domains above.

**Tables (5):**
| Table | Key fields |
|---|---|
| `ZCIS_CUST_TYPE` | MANDT, KATR2 |
| `ZCIS_WAIVER_RULE` | MANDT, SCHEME_YEAR, CUST_TYPE, SIGN_FROM |
| `ZCIS_SHORTFALL_GRD` | MANDT, PERIOD_FROM, PERIOD_TO, **MATNR** |
| `ZCIS_NODISC_GRADE` | MANDT, KONDM |
| `ZCIS_SCHEME_PARAM` | MANDT, PARAM_KEY |

**Programs (2):** `YRVG004_QAIS_EXECUTE_N1`, `ZCIS_REBATE_REPORT`

> Standard data elements referenced (MANDT, MATNR, KATR2, KONDM, BEGDA, ENDDA,
> UNAME, DATUM, TEXT40) already exist in every system — not included, by design.

---

## Import steps

1. Run **`ZABAPGIT_STANDALONE`** → **Import ZIP** (or *+ new offline* → *Import ZIP*).
2. Choose/create the **target package** (e.g. `ZCIS`) and upload `CIS_2026_27_abapGit.zip`.
3. **Pull / Import** all objects.
4. **Activate** in this order (abapGit's mass-activate usually handles it):
   Domains → Data elements → Tables → Programs.

---

## Manual steps required AFTER import

These cannot be carried in the ZIP and must be done in the system:

1. **GUI status `STANDARD` for `YRVG004_QAIS_EXECUTE_N1`** ⚠️ *required to run*
   The program sets `PF-STATUS 'STANDARD'` (its own status). Copy the GUI status
   `STANDARD` from the original **`YRVG004_QAIS_EXECUTE`** into the new program:
   SE41 → *Status* → *Copy from program* `YRVG004_QAIS_EXECUTE`, status `STANDARD`.
   (A GUI status is a CUA object and is not part of the ABAP source.)

2. **Table maintenance (SM30)** — SE11 → each `ZCIS_*` table → *Utilities → Table
   Maintenance Generator* (one-step, function group `ZCIS`, auth group as needed).

3. **Transactions (SE93)** — e.g. `ZCIS_EXECUTE` → `YRVG004_QAIS_EXECUTE_N1`;
   `ZCIS_SHORTFALL` → parameter txn to `SM30` on `ZCIS_SHORTFALL_GRD`;
   `ZCIS_REBATE_RPT` → `ZCIS_REBATE_REPORT`.

4. **Seed data** (SM30/SE16) — see `CIS_2026_27_DDIC_OBJECTS.md` for the rows:
   - `ZCIS_WAIVER_RULE` — 5 approved rows (2026-27).
   - `ZCIS_NODISC_GRADE` — KONDM I2–I7, I8–J3, 74, 75 (confirmed 02.07.2026).
   - `ZCIS_SCHEME_PARAM` — `TRADER_CAP_MT = 200`.
   - `ZCIS_CUST_TYPE` — KATR2 → A/T mapping (values to be confirmed by GAIL).
   - `ZCIS_SHORTFALL_GRD` — **material numbers** declared shortfall per period.

5. **`ZCIS_REBATE_REPORT`** — set default `p_auart` to the actual CIS
   credit-memo-request document type before go-live.

---

## Notes

- **Delivery class** of the tables is `C` (customizing) — adjust if your standards differ.
- **`ZCIS_SHORTFALL_GRD` is keyed on `MATNR`** (material), matching how signed items
  are stored in `YRVA_QAIS_TNTLFT` (confirmed from DDIC). The program's
  `build_cis_shortfall` matches TNTLFT-MATNR against this table directly.
- If you rename any table/field, tell me and I'll re-sync the ABAP + regenerate the ZIP.
