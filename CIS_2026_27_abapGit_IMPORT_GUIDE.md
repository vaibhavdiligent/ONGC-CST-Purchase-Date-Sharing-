# CIS 2026-27 — abapGit ZIP Import Guide

Import file: **`CIS_2026_27_abapGit.zip`** (upload via `ZABAPGIT_STANDALONE` → *Import ZIP*).

The ZIP is a standard offline abapGit repository (`.abapgit.xml` + `/src/`), so it
imports the same way as any abapGit offline project.

---

## What's inside (39 objects)

All objects are in the **`Y`** customer namespace.

**Domains (15):** `YCIS_CTYPE`, `YCIS_YEAR`, `YCIS_MONTH`, `YCIS_PERC`, `YCIS_COUNT`,
`YCIS_QAIS`, `YCIS_STYPE`, `YCIS_STATUS`, `YCIS_AMT`, `YCIS_QTY`, `YCIS_REMARK`,
and (3-level workflow) `YCIS_WFSTAT`, `YCIS_DEPT`, `YCIS_WLEVEL`, `YCIS_SEQNO`

**Data elements (15):** same names as the domains above.

**Tables (5):**
| Table | Key fields |
|---|---|
| `YCIS_WAIVER_RULE` | MANDT, SCHEME_YEAR, CUST_TYPE, SIGN_FROM |
| `YCIS_SHORTFALL` | MANDT, PERIOD_FROM, PERIOD_TO, **MATNR** |
| `YCIS_NODISC_GRD` | MANDT, KONDM |
| `YCIS_APPRVL` | MANDT, QAIS_NO, SCHEME_TYPE, PERIOD_FROM, PERIOD_TO, KUNNR, KVGR2 (+ workflow status/audit) |
| `YCIS_WF_APPR` | MANDT, SALES_OFFICE, LEVEL, SEQUENCE (approval hierarchy) |

**Programs (4):** `YRVG004_QAIS_EXECUTE_N1` (L1), `YCIS_REBATE_REPORT`,
`YCIS_APPROVE` (L2), `YCIS_EXECUTE` (L3)

### 3-level approval workflow (R4)
- **L1 – PC MKTG** runs `YRVG004_QAIS_EXECUTE_N1`, reviews the ALV, and **Confirm** saves the
  computed rebates to `YCIS_APPRVL` as **Pending-L2** (no order) and e-mails L2.
- **L2 – PC MKTG-HOD** runs `YCIS_APPROVE`, sees Pending-L2 rows for their office, **Approve**
  (→ Pending-L3, e-mail L3) or **Reject** (→ back to L1, e-mail L1).
- **L3 – CPC** runs `YCIS_EXECUTE`, sees Pending-L3 rows (all offices), **Execute** (creates the
  rebate order, status Completed) or **Reject** (→ back to L2, e-mail L2).
- Hierarchy & e-mail recipients are maintained in **`YCIS_WF_APPR`** (SM30). See
  `CIS_2026_27_Approval_Workflow.docx` for the full design and config sample.

### Extra manual steps for the workflow
- **GUI status `STANDARD`** in `YCIS_APPROVE` (fn codes `APPR`,`REJ`,`SELALL`,`DESEL`,Back,Exit)
  and in `YCIS_EXECUTE` (`EXEC`,`REJ`,`SELALL`,`DESEL`,Back,Exit) — small SE41 status per program.
- **SM30** generators for `YCIS_WF_APPR` and `YCIS_APPRVL`.
- **Transactions (SE93):** `YCIS_L1` → `YRVG004_QAIS_EXECUTE_N1`, `YCIS_L2` → `YCIS_APPROVE`,
  `YCIS_L3` → `YCIS_EXECUTE`.
- **SCOT/SMTP** configured for the e-mail notifications.

> **Two tables dropped per GAIL feedback (07.07.2026):**
> - `YCIS_CUST_TYPE` — customer type is read from the existing
>   `YRVA_QAIS_DATA-YY_CUSCLASS` field (`'TRADER'` ⇒ T, else A), so no mapping table is needed.
> - `YCIS_SCH_PARAM` — the 200 MT cap is already handled in `YRVG004` at CIS creation.

> **Table names were shortened to fit SAP's 16-character limit.** Original (first
> proposed) name → final table name now used in the ABAP source:
> | First proposed | Final table (SAP) |
> |---|---|
> | `ZCIS_SHORTFALL_GRD` (18) | `YCIS_SHORTFALL` (14) |
> | `ZCIS_NODISC_GRADE` (17) | `YCIS_NODISC_GRD` (15) |
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
