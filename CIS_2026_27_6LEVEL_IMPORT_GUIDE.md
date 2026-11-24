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

## UAT observations 02.11.2026 – changes

| # | Observation | Change | Object(s) |
|---|---|---|---|
| SMD-1 | Name as per scheme | Option renamed *Strategic Monthly Discount for May'26 and June'26* | YRVG004_QAIS_EXECUTE_N1 |
| SMD-2 | Raffia / other bifurcation | Output columns Raffia Grades Qty, Raffia Rate, Other Grades Qty, Other Rate, Total Amount (Rs.) | YRVG004_QAIS_EXECUTE_N1 |
| WF-1 | Approval note amount ≠ YRVG052 | Note showed 10× the value (WRITE … CURRENCY on a 3-decimal field) – now written with 2 decimals | YCIS_REBATE_REPORT |
| WF-2 | Note shows cases pending at L3 | Only cases finally approved at L5 (WF_STATUS 60/70, L5 user filled) are printed | YCIS_REBATE_REPORT |
| WF-4 | Reminder to L1/L2 after 2 working days | New report YCIS_REMINDER (daily job) + table YCIS_DISC_REL | YCIS_REMINDER |

### New table YCIS_DISC_REL – CIS Discount Release by PMG (SE11)

Delivery class **C**, Display/Maintenance allowed, data class **APPL2**, size category 0, buffering not allowed.

| Field | Key | Initial values | Data element | Type | Description |
|---|---|---|---|---|---|
| MANDT | X | X | MANDT | CLNT 3 | Client |
| SCHEME_TYPE | X | X | YCIS_STYPE | CHAR 1 | M = Monthly, T = Strategic Monthly, A = Annual, C = Annual Consistency |
| PERIOD_FROM | X | X | BEGDA | DATS 8 | CIS period from |
| PERIOD_TO | X | X | ENDDA | DATS 8 | CIS period to |
| REL_DATE | | | DATUM | DATS 8 | Release date by PMG |
| LAST_REMIND | | | DATUM | DATS 8 | Last reminder sent (filled by YCIS_REMINDER) |
| CREATED_BY | | | UNAME | CHAR 12 | Created by |
| CREATED_ON | | | DATUM | DATS 8 | Created on |
| REMARKS | | | YCIS_REMARK | CHAR | Remarks |

Table maintenance generator: function group e.g. `YCIS_DISC_REL`, one-step; T-code (SE93) e.g. `YCIS_DISC_REL` for SM30.
CPC enters one row per PMG release, e.g. `M | 01.10.2026 | 31.10.2026 | 05.11.2026`. LAST_REMIND is left blank.

### New report YCIS_REMINDER – L1/L2 reminder (daily job)

* Create the table above first, then program `YCIS_REMINDER` (type 1, executable) and activate.
* Selection: Sales Office (optional), Working days after release (default 2), Factory calendar (GAIL calendar ID), Test run (no e-mail).
* Logic: for every release in YCIS_DISC_REL older than the working-day limit, each zonal sales office with CIS customers in the period is checked in YCIS_APPRVL:
  * nothing confirmed at L1 / returned to L1 (WF_STATUS 10) → reminder **to L1** of the office;
  * pending at L2 (WF_STATUS 20) → reminder **to L2** of the office;
  * copies to **L3, L4, L5** (sales office `0001` in YCIS_WF_APPR).
* One reminder per office per day (LAST_REMIND), repeated daily until L1/L2 is completed; the job log lists the reminders sent.
* Schedule in SM36 daily (e.g. 09:00) with a variant: working days 2, factory calendar = GAIL calendar.

## UAT observations 04.11.2026 – changes

| # | Observation | Change | Program |
|---|---|---|---|
| SMD-1 | Group selection not working | Strategic run now respects Customer Group 2 (and Sales Office) selection | YRVG004_QAIS_EXECUTE_N1 |
| SMD-2 | Group MCQ % split per member | One MCQ % per group = group lifting / group MCQ, shown on every member; Grp Lifted Qty = group total | YRVG004_QAIS_EXECUTE_N1 |
| SMD-3 | CIS entry month | New column *CIS Entry Month* (MoU begin, e.g. JUNE 2026) before MCQ | YRVG004_QAIS_EXECUTE_N1 |
| SMD-4 | OG grades not counted (W55 OG – 32906, Y50 OG – 33870) | Raffia MPGs always applicable; A / B / OG / NA variants of an applicable grade (same material family in MVKE) are applicable too | YRVG004_QAIS_EXECUTE_N1 |
| WF-6 | Two selection boxes (YRVG004_F, YRVG052) | Extra ALV row-selection box removed – only the Select / Print tick-box column | YCIS_APPROVE, YCIS_EXECUTE, YCIS_VET, YCIS_APPRV5, YCIS_DISBURSE, YCIS_REBATE_REPORT |
| WF-7 | Approval note clubbed for two L5 batches | Note printed per L5 approval batch (same L5 user/date/time); L5 stamps one time for the whole batch | YCIS_REBATE_REPORT, YCIS_APPRV5 |

## UAT observations 06.11.2026 – changes (mail of 10.11.2026)

| # | Observation | Change | Program |
|---|---|---|---|
| WF-8 | Rebate order 29486381 returned by L5 not visible on the L1 screen | The order is logged in YRVA_REBATE at L3, and L1 dropped every customer with a rebate-log entry as "order already created". L1 now ignores rebate-log entries whose order is returned to L1 (YCIS_APPRVL WF_STATUS 10) – on the list and on Execute. When L1 re-submits, the earlier order's YRVA_REBATE entry is removed (L3 creates a new order). | YRVG004_QAIS_EXECUTE_N1, YRVG004_QAIS_EXECUTE_N1_old |
| WF-9 | YRVG052 shows "Returned by L2" for an order returned by L5 | Status text for all 6 levels: *Returned by L<n> - Pending L1* (n = level that returned it), Pending L2 … Pending L6, *Completed - Credit Note posted*, *Closed at L3 - no discount* | YCIS_REBATE_REPORT |
| WF-10 | (found during the fix) PDR program re-staged rows already at L4–L6 / completed | In-flight check in YRVG004_QAIS_EXECUTE_N1_old now covers WF_STATUS 20–70 (as in N1) | YRVG004_QAIS_EXECUTE_N1_old |

Note: the earlier rebate order of a returned row (e.g. 29486381) stays open in SAP. Set a reason for rejection on it in VA02 so it is not used again; the credit note is created only from the new order created at L3.

## Strategic Monthly Discount – QAR observations 10.11.2026

| # | Observation | Change | Program |
|---|---|---|---|
| SMD-5 | Customer still shown at L1 after L1 executed it | Submitted Strategic rows (scheme type T) are hidden at L1 – the check looked only at scheme M | YRVG004_QAIS_EXECUTE_N1 |
| SMD-6 | Sales Order column empty – show "VERIFIED" | After Execute the submitted rows show **VERIFIED** in the Sales Order column (order itself is created at L3) | YRVG004_QAIS_EXECUTE_N1 |
| SMD-7 | No separate selection for SMD at L2 / L3 | New option **Strategic Monthly Discount (May/June'26)** at L2–L6; *Monthly* now shows only the monthly CIS (scheme M) | YCIS_APPROVE, YCIS_EXECUTE, YCIS_VET, YCIS_APPRV5, YCIS_DISBURSE |
| SMD-8 | One row ticked, Execute submitted all rows | Strategic option added to the Execute check – only ticked rows are submitted | YRVG004_QAIS_EXECUTE_N1 |
| SMD-9 | June output given while May still in L1–L6 | June'26 run for a sales office is blocked while any May'26 Strategic record of that office is pending (WF_STATUS 10–60) | YRVG004_QAIS_EXECUTE_N1 |
| SMD-10 | CIS entry with MCQ 0 (33866) in June output | Customers with MCQ 0 (after the group MCQ) are not eligible | YRVG004_QAIS_EXECUTE_N1 |

Test data note: the 11 June'26 rows of office 3000 submitted on 10.11.2026 by the single-tick Execute are already Pending L2 (scheme T). To retest point 8, either approve/return them at L2 or delete those YCIS_APPRVL rows (SCHEME_TYPE T, PERIOD_FROM 01.06.2026, SALES_OFF 3000) in QAR.

## UAT 11.11.2026 – Approval Note: amount in words

The note now prints the discount total value in words under *Discount total value (Rs.)*, Indian numbering (crore / lakh), e.g. 2,05,800.00 → *Rupees Two Lakh Five Thousand Eight Hundred Only*.

1. **SE11 – structure `ZCIS_APPR_NOTE_S`**: add field `AMT_WORDS`, data type CHAR, length 255 (predefined type, or data element `TEXT255`). Activate.
2. **SFP – interface `ZCIS_PSD_APPR_NOTE`**: open, check, activate (WA_NOTE now includes AMT_WORDS).
3. **SFP – form `ZCIS_PSD_APPR_NOTE`**: on the *Context* tab make sure `WA_NOTE-AMT_WORDS` is active (if WA_NOTE was dragged in as a whole it appears automatically; otherwise drag the field in). In the *Layout* re-import `ZCIS_PSD_APPR_NOTE_layout.XDP` – or add a text field `AMT_WORDS` (caption "Amount in words", multi-line, width 180 mm) below `DISC_TOTAL_VALUE`, binding `$.WA_NOTE.AMT_WORDS`. Activate.
4. **YCIS_REBATE_REPORT** (YRVG052): copy the latest program and activate (new FORMs `amount_in_words`, `num_words`, `below_hundred`).

## UAT 12.11.2026 – number format at L2–L6

| Observation | Change | Programs |
|---|---|---|
| MCQ Qty, Group Lifted Qty, Individual Lifted Qty, MCQ % and Rebate Value shown in a wrong number format (L2 screen) | These YCIS_APPRVL fields are quantity / amount types that need a unit / currency field, which the list does not have, so the grid formatted them wrongly. The list now holds them as plain numbers with a fixed format: quantities (MT) 3 decimals, MCQ % 2 decimals, Rebate Value (Rs.) 2 decimals, right-aligned, Rebate Value totalled (new FORM `num_format`). | YCIS_APPROVE, YCIS_EXECUTE, YCIS_VET, YCIS_APPRV5, YCIS_DISBURSE |
| Same check on all other quantity / amount columns | Same fixed format applied to: rebate workflow L2–L6 (Lifted Qty, Eligible Qty, Rebate Value); YRVG052 (MCQ Qty, Lifted Qty, Discount Qty, Rebate Amount); L4 lifting break-up pop-up (Sales Qty (MT)) | YCIS_REB_APPROVE, YCIS_REB_EXECUTE, YCIS_REB_VET, YCIS_REB_APPRV5, YCIS_REB_DISBURSE, YCIS_REBATE_REPORT, YCIS_VET |

## Strategic Monthly Discount – QAR observations 13.11.2026

| # | Observation | Change | Program |
|---|---|---|---|
| SMD-11 | L2–L6: Seasonal, Waiver Month 1/2, Waiver Status not needed; show CIS Entry Date, Raffia Grades Qty, Raffia Rate, Other Grades Qty, Other Rate, Total Amount (as L1) | With the option *Strategic Monthly Discount (May/June'26)*: Seasonal / Waiver columns hidden; new columns CIS Entry Date (first CIS 2026-27 entry), Raffia Grades Qty, Raffia Rate (4000), Other Grades Qty, Other Rate (3000); Rebate Value headed *Total Amount (Rs.)*. Raffia/Other qty derived from the stored qty and value (value = 4000 × Raffia + 3000 × Other). | YCIS_APPROVE, YCIS_EXECUTE, YCIS_VET, YCIS_APPRV5, YCIS_DISBURSE |
| SMD-12 | Credit-note e-mail: "REBATE PERIOD 00.00.0000 TO 00.00.0000" | E-mail takes the period from the approval record (then the rebate log) when the order's period dates are blank; L3 also passes the period start with the order (VBKD-BSTDK) | YCIS_DISBURSE, YCIS_EXECUTE |
| SMD-13 | Applicable to customers who entered CIS in June / July 2026 and remain active at least up to Q2 (30.09.2026) | Per customer: CIS entry = earliest MoU begin of its CIS 2026-27 records, must be in June/July 2026; the customer's current (latest) record must be active and valid at least to 30.09.2026. An older version no longer qualifies a customer whose current record is terminated / inactive or ends before 30.09.2026. | YRVG004_QAIS_EXECUTE_N1 |

## PDR go-live (17.11.2026)

### Strategic Monthly Discount switched off
Constant `GC_SMD_ACTIVE` (value blank) in YRVG004_QAIS_EXECUTE_N1, YCIS_APPROVE, YCIS_EXECUTE, YCIS_VET, YCIS_APPRV5, YCIS_DISBURSE: the Strategic Monthly Discount radio button (and its text) is not displayed on T-codes YRVG004, YRVG004_A, YRVG004_E, YRVG004_V, YRVG004_F, YRVG004_P and cannot be run (also not through a saved variant). To activate later set `GC_SMD_ACTIVE` to `'X'` in these six programs.

### Objects in the transport (check before release)
* Programs: YRVG004_QAIS_EXECUTE_N1, YRVG004_QAIS_EXECUTE_N1_old, YCIS_APPROVE, YCIS_EXECUTE, YCIS_VET, YCIS_APPRV5, YCIS_DISBURSE, YCIS_REBATE_REPORT, YCIS_SHORTFALL_MC, YCIS_REMINDER, YCIS_MASTER_CHECK (if used) – each with its GUI status STANDARD (SE41: L6 function APPR text "Post"; YRVG052 button PRNT).
* DDIC: domains / data elements YCIS_*, tables YCIS_APPRVL, YCIS_APPRVL_GRD, YCIS_WF_APPR, YCIS_SHORTFALL, YCIS_WAIVER_RULE, YCIS_NODISC_GRD, YCIS_DISC_REL (+ table maintenance generators / function groups), structures ZCIS_APPR_NOTE_S (with AMT_WORDS), ZCIS_APPR_NOTE_ROW and its table type.
* Adobe form ZCIS_PSD_APPR_NOTE and interface ZCIS_PSD_APPR_NOTE.
* Transactions (SE93): L1–L6, YRVG052, YGCIS_SHORTFALL (SM30), YCIS_SHORTFALL (checker), YCIS_DISC_REL (SM30).
* Customizing (separate customizing request): OB32 – BSEG-SGTXT changeable after posting (G/L line text at L6).

### Data to maintain in PDR (client-specific – not in the workbench transport)
| Table / item | How | What |
|---|---|---|
| YCIS_WF_APPR | SM30 | Approvers: L1 and L2 per zonal sales office; L3, L4, L5, L6 under sales office 0001 – user ID and e-mail of each |
| YCIS_WAIVER_RULE | SM30 | Waiver rules valid for FY 2026-27 (minimum lifting in a waiver month AU 0.25 / AUT-Trader 0.50, number of waivers by CIS signing month) – same as tested in QAR |
| YCIS_NODISC_GRD | SM30 | Non-discount grades (PS / GS / Powder / Polyfines MPGs) – same as QAR |
| YCIS_SHORTFALL | YGCIS_SHORTFALL (maker) + YCIS_SHORTFALL (checker) | Shortfall-waiver grades per period – entered by PMG and approved by CPC L3 when needed |
| YCIS_DISC_REL | SM30 | One row per PMG discount release (scheme, period, release date) – drives the L1/L2 reminder |
| Existing CIS masters | check only | YRVA_QAIS_DATA (CIS 2026-27 customers, MCQ, waivers), grade tables YRVA_HDPE_GRADES / YRVA_LLDPE_GRADE / YRVA_OTHER_GRADE / YRVA_OTHER_GRAD1 / YRVA_GRADE_CISD, customer e-mail IDs (credit-note mail) |
| Job YCIS_REMINDER | SM36 | Daily (e.g. 08:45), variant: working days 2, GAIL factory calendar |
| Roles | PFCG | T-codes of each level to the respective users; YV_VKBUR sales-office authorisation for L1/L2 |
| Mail | SCOT / SOST (Basis) | SAPconnect sending active in PDR (workflow and credit-note e-mails) |

Do not move QAR test data (YCIS_APPRVL / YCIS_APPRVL_GRD rows) to PDR.

## PDR issues 23.11.2026 (CPC L6 issues / 45 min for rebate orders)

| # | Issue | Change | Program |
|---|-------|--------|---------|
| PDR-1 | L5 approval – no e-mail to L6 | Mail to a central level (L3–L6) reads the approvers of that level under **any** sales office (not only `0001`); a blank e-mail ID in YCIS_WF_APPR is taken from SU01 (USR21/ADR6); duplicates removed; if the approving user has no SU01 e-mail, his YCIS_WF_APPR e-mail is the sender. L5 message now says *e-mail sent to n L6 address(es)* or *e-mail to L6 NOT sent: <reason>*. | YCIS_APPRV5 (same recipient logic in YCIS_APPROVE, YCIS_EXECUTE, YCIS_VET, YCIS_DISBURSE) |
| PDR-2 | L6 POST – "Credit Note (G2) creation/posting failed" for every line, no entry posted | SAP's own message from billing (BAPI_BILLINGDOC_CREATEMULTIPLE, with message ID/number) is kept in **Remarks** of the line (line stays on the screen, Pending L6) and shown in a result list after POST; no pop-up per line. Billing block on the rebate order (VBAK-FAKSK) or a missing order is reported by name. | YCIS_DISBURSE |
| PDR-3 | L3 Execute – 350 lines took 45 min | More than 20 ticked lines are split into up to 5 packets and created by parallel background jobs **YCIS_L3_REBATE_ORDERS** (SM37), started immediately; the user gets the screen back at once. Each line is locked while its order is created (no duplicate order from two runs); a failed order keeps SAP's reason in Remarks; L4 is e-mailed once, by the last job. One synchronous commit per line instead of two. Up to 20 lines – dialog as before. | YCIS_EXECUTE |

Prerequisites / checks in PDR:
* PDR-3: L3 users need **S_BTCH_JOB** (JOBACTION `RELE`, JOBGROUP `*`) to release their own jobs – without it the lines are executed in dialog as before. Enough background work processes (SM50, type BGD) for 5 parallel jobs. Packets are handed over in cluster table INDX (areas `YC`/`YD`) – nothing to create.
* PDR-2: after the new version, POST one line in YRVG004_P – the Remarks / result list give the SAP reason (typical: authorisation V_VBRK_FKA for billing type G2 / V_VBRK_VKO for sales org. 5000, billing block on ZP09, copy control ZP09 → G2 (VTFA), G2 number range, posting period, account determination).
* PDR-1: YCIS_WF_APPR level 6 entry with user ID; e-mail ID in YCIS_WF_APPR or SU01. Mails that still do not arrive: SOST.
