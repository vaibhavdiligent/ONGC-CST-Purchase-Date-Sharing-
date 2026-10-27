# CIS 2026-27 — 6-Level Approval Workflow (L4/L5/L6) — Import & Setup

Deliverable: **`CIS_2026_27_6LEVEL_abapGit.zip`** (abapGit offline package).

This package extends the existing 3-level CIS maker-checker flow to 6 levels for
CPC (Central Processing Cell) processing of PSD/CIS discounts.

## New level map

| Level | Program        | T-code role                    | Reads WF_STATUS | On approve → | On reject → |
|-------|----------------|--------------------------------|-----------------|--------------|-------------|
| L1    | YRVG004_QAIS_EXECUTE_N1 | Executive – Run/Verify | 10              | 20           | —           |
| L2    | YCIS_APPROVE   | PC Head – Verify               | 20              | 30           | 10          |
| L3    | YCIS_EXECUTE   | CPC Mktg – Execute (rebate SO) | 30              | **40**       | 10          |
| **L4**| **YCIS_VET**   | CPC Finance – Financial Vetting| **40**          | **50**       | 10          |
| **L5**| **YCIS_APPRV5**| CPC Head – Final Approval      | **50**          | **60**       | 10          |
| **L6**| **YCIS_DISBURSE**| CPC Finance – Disbursement   | **60**          | **70 (Completed)** | 10   |

Rejection at **any** level sends the row back to **L1 (WF_STATUS 10)** for
re-initiation, and an e-mail alert goes to the originating sales office.

## What changed in DDIC

1. **Domain `YCIS_WFSTAT`** — fixed values extended:
   `40 = Pending L4`, `50 = Pending L5`, `60 = Pending L6`, `70 = Completed`
   (previously `40 = Completed`).
2. **Table `YCIS_APPRVL`** — 12 new fields (positions 0057–0068):
   `L4_USER/L4_DATE/L4_TIME`, `L5_USER/L5_DATE/L5_TIME`,
   `L6_USER/L6_DATE/L6_TIME`, `REM_L4`, `REM_L5`, `REM_L6`
   (data elements UNAME / DATUM / UZEIT / YCIS_REMARK — all already in the package).

## What changed in code

- **YCIS_EXECUTE (L3)** — on Execute now forwards to **WF_STATUS 40 (Pending L4)**
  and e-mails L4; "Group OK" zero-lifting rows also go to 40; reject → 10.
- **YRVG004_QAIS_EXECUTE_N1 (L1)** — status ranges widened so already-forwarded
  rows (20–70) are hidden from re-staging.
- **YCIS_VET / YCIS_APPRV5 / YCIS_DISBURSE** — new central (office `0001`)
  approval programs, one per level, ALV grid with Approve/Reject/Select-All.

## Import order (important)

1. Pull the package with abapGit.
2. **Activate DDIC first**: domain `YCIS_WFSTAT`, then table `YCIS_APPRVL`
   (adjust-and-activate; the 12 new columns are appended, existing data is kept).
3. Activate the programs (YCIS_EXECUTE, YCIS_VET, YCIS_APPRV5, YCIS_DISBURSE,
   YRVG004_QAIS_EXECUTE_N1).

## Manual step — GUI status (cannot be shipped in offline abapGit)

Each new program needs a GUI status named **`STANDARD`** with function codes
`APPR`, `REJ`, `SELALL`, `DESEL`, `BACK`, `EXIT` (same as YCIS_APPROVE / YCIS_EXECUTE).
Create via **SE41** for each of: `YCIS_VET`, `YCIS_APPRV5`, `YCIS_DISBURSE`
(easiest: copy status `STANDARD` from `YCIS_APPROVE`). For L6 the `APPR` button
label reads "Disburse".

## Master data — approver hierarchy (`YCIS_WF_APPR`)

Maintain the new central approvers (sales office `0001`) with `WF_LEVEL` = 4, 5, 6,
their `USERID` and `EMAIL`. Levels 4–6 are central: they see the pending rows of
**all** sales offices. Level 1 rows (per office) are used for reject / completion
e-mail routing.

## Disbursement posting (L6)

`YCIS_DISBURSE` records workflow completion (WF_STATUS 70, STATUS A) against the
rebate order created at L3. The actual credit-note / G-L / payment posting is a
finance-config decision and is **not** performed automatically — the single hook
`FORM post_disbursement` is provided as the place to plug in the posting BAPI/FM
once CPC Finance confirms the design. Until then it is a controlled no-op.

## Create the transaction codes (SE93)

Create a dialog/report transaction for each new program if CPC wants direct
T-code access: e.g. `YCIS_L4` → YCIS_VET, `YCIS_L5` → YCIS_APPRV5,
`YCIS_L6` → YCIS_DISBURSE.

## UAT observations 14.10.2026 (L1–L6) – changes

| # | Observation | Change | Program(s) |
|---|---|---|---|
| 1 | Seasonal indicator at all levels | Columns *P/R/S Indicator* (YRVA_QAIS_DATA-P_R_INDICATOR) and *Seasonal Indicator* (P_SC_INDICATOR) | N1 (L1 monthly), YCIS_APPROVE, YCIS_EXECUTE, YCIS_VET, YCIS_APPRV5, YCIS_DISBURSE |
| 2 | Monthly waiver at all levels | Columns *Waiver Month 1/2* (WAIVER_1 / WAIVER_2) | same as 1 |
| 3 | Already processed records at L2–L6 | Radio buttons *Unexecuted* (pending, default) / *Executed* (processed at this level – display only, with workflow status, processed by/on, remark, reject details) | L2–L6 |
| 4 | Grp / Ind Lifted Qty at L2–L6 | Already at L2–L5; added to L6 | YCIS_DISBURSE |
| 5 | Credit note details not captured (YRVG004_P) | Credit Note / Accounting Doc stored and shown; *Executed* view lists posted rows; missing accounting doc is completed automatically | YCIS_DISBURSE |
| 6 | Rename Approve → Post | Texts changed in program; **SE41: status STANDARD of YCIS_DISBURSE, function APPR, function text = `Post`** | YCIS_DISBURSE |
| 7 | "Accounting document not yet posted" although posted | Accounting doc read again with a wait for the update task; billing posting status RFBSK = 'C' counts as posted; message "Accounting document posted" | YCIS_DISBURSE |
| 8 | Credit note e-mail with L6 posting | On posting, the credit note PDF (Smart Form YRV_CREDIT_NOTE, as YRVR167) is e-mailed to the customer (partner e-mail logic of YRVR169, sender EINVOICE@GAIL.CO.IN) | YCIS_DISBURSE |
| L4-2 | Lifting qty breakup on L4 screen | Click Group / Individual Lifted Qty → popup with the S922 sales lines (customer, date, grade, channel, qty, totals); group = BP Group/MLE members | YCIS_VET |
| L4-3 | Field names on L4 selection screen | Selection texts set in the program (Period From, Sales Office, Customer, Customer Group) – all levels | L2–L6 |
| L4-4 | Display already vetted data | = obs. 3 (*Executed*) | YCIS_VET |

## UAT observations 23.10.2026 – changes

| # | Observation | Change | Object(s) |
|---|---|---|---|
| 1 | Maker & checker for shortfall grade entry – T-code | Program `YCIS_SHORTFALL_MC` (already delivered). Create report transaction in SE93 (suggested `YCIS_SHORTFALL`) and assign to the PMG maker and CPC L3 checker roles | YCIS_SHORTFALL_MC |
| 2 | MCQ Qty in YRVG052 | New column *MCQ Qty* (YCIS_APPRVL-MCQ_QTY) | YCIS_REBATE_REPORT |
| 3 | Zero-value / no rebate order lines must not go to L4 | L3 closes such lines (`GROUP OK`, WF_STATUS 70) instead of forwarding; L4 is e-mailed only for real rebate orders. Lines forwarded earlier are closed automatically when L4/L5/L6 is opened | YCIS_EXECUTE, YCIS_VET, YCIS_APPRV5, YCIS_DISBURSE |
| 4 | Text on expense GL line | After L6 posting the G/L lines of the accounting document get text `INVOICE <reference>` (e.g. `INVOICE CIS 26-27 JUNE DISCO`) via FI_DOCUMENT_CHANGE – no manual YRFR126 step. BSEG-SGTXT must be changeable (OB32) | YCIS_DISBURSE |
| 5 | Approval note: no L6 details; fixed statements L1–L5 | Layout: L1/L2 statements above the Sales-Office table, L3/L4/L5 statement above each block, L6 block removed; driver no longer fills L6 | ZCIS_PSD_APPR_NOTE_layout.XDP, YCIS_REBATE_REPORT |
