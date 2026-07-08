# CIS Scheme 2026-27 — Change Plan & New Object List
**Program in scope:** `YRVG004_QAIS_EXECUTE` (standalone build: `YRVG004_QAIS_EXECUTE_N1`)
**Status:** Planning only — no code changed yet.
**Legend:** 🟢 exists / reusable · 🔴 new object · ⚠️ open point needing confirmation

---

## 0. Foundation already done (for reference)
- Monthly min lifting % `80 → 75`, Annual `85 → 80`, effective `01.06.2026` (period-gated), plus test enablement for FY 2025-26.
- Quarterly & Annual-Consistency radio buttons removed.
- Divide-by-zero guards in `monthly_discount`.
- Self-contained GUI status (`PF_STATUS_SET` + status copy).

---

## 1. Customer Waiver (Requirement 1)

### 1.1 Minimum lifting criteria by customer type
- AU (Actual User) → must lift ≥ **25%** MCQ to avail waiver.
- AUT / Trader → must lift ≥ **50%** MCQ to avail waiver.

### 1.2 Monthly waiver entitlement — Actual User (AU)
| CIS signed in | No. of monthly waivers |
|---|---|
| Jun'26 & Jul'26 | 2 |
| Aug'26 – Sep'26 | 1 |
| Oct'26 onwards | 0 |
- Max **one** waiver in any quarter of FY 2026-27.

### 1.3 Monthly waiver entitlement — AUT / Trader
| CIS signed in | No. of monthly waivers |
|---|---|
| Jun'26 – Sep'26 | 1 |
| Oct'26 onwards | 0 |
- Waiver allowed only after minimum MCQ (25% / 50%) is met.

### Design approach
- Derive **customer type** (AU / AUT / Trader) — not available today.
- Derive **CIS signing month** from `mou_begda` (already available in QAIS data).
- Drive waiver entitlement + minimum % from a **new configuration table** (no hard-coding), and enforce the "max 1 waiver per quarter" counter in `get_data` / `monthly_discount`.

### New objects
| # | Type | Proposed name | Purpose |
|---|---|---|---|
| 1 | 🔴 Z-Table (config) | `YCIS_WAIVER_RULE` | Keyed by scheme year + customer type + signing-month range → min-lifting %, no. of monthly waivers, max waiver per quarter |
| 2 | 🔴 Table maint. gen. + T-code | `YCIS_WAIVER_RULE` (SM30) | Business maintenance of the above |
| 3 | 🔴 Customer-type source | field on BP / `YCIS_CUST_TYPE` map | AU / AUT / Trader classification per customer ⚠️ |
| 4 | 🔴 Data element / domain | `YCIS_CUST_TYPE`, `YCIS_WV_COUNT` | Typing for the above |
| 5 | 🟢→ enhance | `YRVG004..._N1` (`get_data`, `monthly_discount`) | Apply type-wise min %, waiver count, per-quarter cap |

### ⚠️ Open points
- Where is **AU / AUT / Trader** classification maintained today? (BP role, `KDGRP`, `KVGR*`, or new map.)
- Confirm **CIS signing date** = `mou_begda`, or a separate signing date is required.
- "Quarter of FY 2026-27" = Apr-Jun / Jul-Sep / Oct-Dec / Jan-Mar — confirm.

---

## 2. Shortfall Grade Waivers (Requirement 2)

### Requirement
- New T-code for the process owner to enter grades declared **shortfall** for a given month.
- Signed CIS grades are in `YRVA_QAIS_TNTLFT`.
- Today shortfall is entered per CIS no. via `YRVG018` → stored in `YRVA_QAIS_ADD_WV`.
- Enhance `YRVG004` to **auto-apply** the month-wise shortfall grades (instead of manual per-CIS entry).

### Design approach
- New month-wise shortfall-grade master (grade + month/period, not per CIS no.).
- New maintenance transaction for the process owner.
- `YRVG004` reads this master and auto-flags shortfall for all CIS having that grade in that month, replacing the manual `YRVG018` step.

### New objects
| # | Type | Proposed name | Purpose |
|---|---|---|---|
| 1 | 🔴 Z-Table | `YCIS_SHORTFALL_GRD` | Month/period + grade (+ P/R/S) declared shortfall |
| 2 | 🔴 Maintenance program + T-code | `YCIS_SHORTFALL_MAINT` / `YRVG0xx` | Process owner enters shortfall grades per month (validation, authorization) |
| 3 | 🟢 reuse | `YRVA_QAIS_TNTLFT`, `YRVA_QAIS_ADD_WV` | Signed grades / existing waiver capture |
| 4 | 🟢→ enhance | `YRVG004..._N1` | Auto-derive shortfall from `YCIS_SHORTFALL_GRD` (new logic block) |

### ⚠️ Open points
- Keep writing to `YRVA_QAIS_ADD_WV` (compatibility) or read the new table directly at runtime?
- Grade granularity: material grade vs P/R/S category.

---

## 3. Group / MLE Incorporation (Requirement 3)

### Requirement
- Group & MLE to be maintained in **BP** by users.
- Detailed logic to be shared by **Mr. Pankaj Wadhwa** in a subsequent email.

### Design approach (provisional)
- Read group / MLE relationships from **BP** (business partner relationships / grouping) instead of relying only on `KVGR2`.
- Blocked pending the logic from Mr. Wadhwa.

### New objects (provisional — to finalize on receipt of logic)
| # | Type | Proposed name | Purpose |
|---|---|---|---|
| 1 | ⚠️ TBD | BP config / relationship | Group & MLE definition in BP |
| 2 | 🔴 (likely) helper | `YCIS_GET_GROUP` (FM/method) | Resolve group/MLE for a customer at runtime |
| 3 | 🟢→ enhance | `YRVG004..._N1` grouping (`KVGR2` logic) | Use BP-based group instead of/along with `KVGR2` |

### ⚠️ Open point
- **Awaiting logic from Mr. Pankaj Wadhwa** — cannot finalize objects until received.

---

## 4. Workflow — Maker & Checker + Email (Requirement 4)

### Requirement
1. PC Executive checks & **saves** → email trigger to **PC Head** (cc **CPC** dept).
2. On **approval by PC Head** → **CPC** creates the rebate orders.

### Design approach
- Introduce an **approval status** on the CIS result (Draft → Submitted → Approved → Orders Created).
- Email notification on Submit (to PC Head, cc CPC) and on Approve (to CPC).
- Restrict rebate-order creation to CPC and only after approval.
- Implement as a **status-driven approval with email** (simpler, recommended) **or** SAP Business Workflow ⚠️ (decision needed).

### New objects
| # | Type | Proposed name | Purpose |
|---|---|---|---|
| 1 | 🔴 Z-Table | `YCIS_APPROVAL` | CIS no. + period → status, maker, checker, timestamps, remarks |
| 2 | 🔴 Data element/domain | `YCIS_APPR_STATUS` | Status values (Draft/Submitted/Approved/Rejected/Order-created) |
| 3 | 🔴 Class / FM | `ZCL_CIS_EMAIL` (uses `CL_BCS`) | Build & send emails to PC Head / CPC |
| 4 | 🔴 Config table | `YCIS_APPR_RECIPIENT` | PC Head / CPC email or org-role mapping (no hard-coded IDs) |
| 5 | 🔴 Authorization object | `Z_CIS_ROLE` | Distinguish PC Executive / PC Head / CPC actions |
| 6 | 🔴 GUI status functions | on `YRVG004..._N1` STANDARD status | New buttons: Submit, Approve, Reject |
| 7 | 🟢→ enhance | `YRVG004..._N1` (`on_selection`, `create_sale_order`) | Save→submit, approval gate before order creation |
| 8 | ⚠️ optional | Workflow `WS9xxxxxxx` + tasks | If SAP Business Workflow route chosen instead of status+email |

### ⚠️ Open points
- **Approach:** custom status + email (recommended) vs full SAP Business Workflow?
- Recipient determination: fixed distribution list, org-unit (HR-ORG), or role-based?
- Is rejection / rework needed, or approve-only?

---

## 5. Report — Rebate Order Details (Requirement 5)

### Requirement
- Report to capture rebate order details: **customer, material, quantity, rebate amount**.

### Design approach
- New ALV report reading the rebate (credit-memo-request) orders created from `YRVG004`, joined to CIS no. / period, with selection by period / sales office / customer.

### New objects
| # | Type | Proposed name | Purpose |
|---|---|---|---|
| 1 | 🔴 Report program + T-code | `YCIS_REBATE_REPORT` / `YRVG0xx` | ALV: customer, material, qty, rebate amount, status |
| 2 | 🟢 reuse | `VBAK/VBAP` (or `VBRK/VBRP`), `YCIS_APPROVAL` | Rebate order data + approval status |
| 3 | 🔴 GUI status/title | for the new report | ALV toolbar |

### ⚠️ Open point
- Rebate order = credit-memo request (`VBAK` doc type ?) — confirm the exact document type / table to read.

---

## Consolidated New-Object Summary

| Area | New tables | New programs/T-codes | Classes/FM | Workflow/Auth | Data elements |
|---|---|---|---|---|---|
| 1 Waiver | `YCIS_WAIVER_RULE`, (cust-type map) | SM30 maint. | – | – | `YCIS_CUST_TYPE`, `YCIS_WV_COUNT` |
| 2 Shortfall | `YCIS_SHORTFALL_GRD` | `YCIS_SHORTFALL_MAINT` + T-code | – | auth check | – |
| 3 Group/MLE | (BP config) ⚠️ | – | `YCIS_GET_GROUP` | – | – |
| 4 Workflow | `YCIS_APPROVAL`, `YCIS_APPR_RECIPIENT` | GUI functions on N1 | `ZCL_CIS_EMAIL` | `Z_CIS_ROLE` (+opt. WF) | `YCIS_APPR_STATUS` |
| 5 Report | – | `YCIS_REBATE_REPORT` + T-code | – | – | – |

**Enhancements to existing program `YRVG004_QAIS_EXECUTE_N1`:** waiver logic (R1), auto-shortfall (R2), BP group (R3), approval gate + Submit/Approve buttons (R4).

---

## Suggested Sequencing
1. **R1 Customer Waiver** (core scheme correctness) — needs cust-type source + config table.
2. **R2 Shortfall automation** — new T-code + auto-derive.
3. **R5 Rebate report** — independent, quick win.
4. **R4 Workflow** — after R1/R2 stabilize (touches order creation).
5. **R3 Group/MLE** — on receipt of logic from Mr. Wadhwa.

## Key confirmations needed before build
1. Source of **AU / AUT / Trader** classification.
2. **CIS signing date** field (`mou_begda` or other).
3. **Workflow approach**: status+email vs SAP Business Workflow, and recipient determination.
4. **Rebate order** document type/table for R5.
5. **Group/MLE logic** from Mr. Pankaj Wadhwa.
