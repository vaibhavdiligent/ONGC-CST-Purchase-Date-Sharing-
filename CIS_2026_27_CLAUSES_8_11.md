# CIS 2026-27 — Clause 8 (Shortfall Waiver) & Clause 11 (Discount Structure)
Captured from GAIL circular images (mail 02.07.2026). Source of truth for these two clauses.

## Clause 11 — Discount Structure under CIS 2026-27
Discount disbursement is on **Monthly & Annual** basis.

| CIS Qty | Monthly (Rs/MT) | Annual (Rs/MT) | Total (Rs/MT) |
|---|---|---|---|
| **≥ 5 MT/Month** | **500** (pre-Sales as MIS discount) **+ 600** (Post-Sale Discount) = 1100 | **400** | **1500** |

- Monthly CIS discount disbursed to customers fulfilling the **monthly** commitment.
- Annual CIS discount disbursed to customers fulfilling the **annual** commitment.

**Mapping in SAP:** these rates are **condition records (VK11)** — the program reads
`i_cond-kbetr` from `A350 → KONM` (condition types **ZMIS** monthly / **ZAIS** annual).
➡️ **Action: GAIL to maintain the CIS 2026-27 rates (500 / 600 / 400) in VK11.** No code
change for the base rate. ⚠️ The program has a legacy `w_kbetr = i_cond-kbetr + 500`
add-on in some waiver paths — to be reconciled against the "500 MIS" component with GAIL.

## Clause 8 — Shortfall Grade Waiver (8.I / 8.II)
- **(single grade)** If a customer signed for a **single grade** in their grade-wise monthly
  lifting plan and that grade is **declared shortfall**, the customer is **straightaway
  eligible** for the monthly shortfall waiver.
- **(c)** The resulting **MCQ shortfall must be completed within the FY** — ACQ to be lifted
  by **31 March 2027**.
- **(d)** **Not eligible** for shortfall waiver if the customer **fails to submit the grade-wise
  monthly lifting plan** (Annexure III).
- **(e)** Eligible for monthly shortfall grade waiver **only if** a grade is **declared shortfall**
  **AND** customer **monthly lifting < 75% of MCQ**.

**Mapping in SAP (R2 auto-apply):**
- PMG enters shortfall grades per month in `ZCIS_SHORTFALL_GRD` (YRVG018 obsolete).
- Program auto-determines the monthly shortfall waiver for a CIS when:
  `is_shortfall_grade(signed grade) = X` **AND** `monthly lifting < 75% MCQ`
  **AND** grade-wise lifting plan exists (Annexure III / `YRVA_QAIS_TNTLFT`).
- Signed grade source: `YRVA_QAIS_TNTLFT` (per Q4). Annual make-up (c) via existing ACQ logic.

⚠️ Integration point: set the monthly waiver flag in the monthly discount flow when the above
condition holds; to be placed & tested in-system (touches the waiver-grant logic).
