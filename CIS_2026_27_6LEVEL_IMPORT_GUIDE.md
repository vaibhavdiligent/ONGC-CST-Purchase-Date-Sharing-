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
