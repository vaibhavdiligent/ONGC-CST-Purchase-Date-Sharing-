# Rebate (PSD / YRVU001) — extension to a 6-level workflow

Package: **`CIS_2026_27_REBATE_6LEVEL_abapGit.zip`**

The Upliftment-Rebate (PSD) flow (`SCHEME_TYPE = 'U'` in `YCIS_APPRVL`) is
extended from 3 levels to 6, on the same lines as the CIS flow.

## Level map (rebate queue, SCHEME_TYPE 'U')

| Level | Program | Action | WF_STATUS after | On reject → |
|-------|---------|--------|-----------------|-------------|
| L1 | YRVU001_REB_CALC_M2_N1 | Calc & stage | 20 (Pending L2) | — |
| L2 | YCIS_REB_APPROVE | Verify | 30 | 10 (L1) |
| L3 | YCIS_REB_EXECUTE | Execute – create CMR | **40** (Pending L4) | 20 (L2) |
| **L4** | **YCIS_REB_VET** | CPC Finance – Vet | **50** | **30 (L3)** |
| **L5** | **YCIS_REB_APPRV5** | CPC Head – Final Approval | **60** | **40 (L4)** |
| **L6** | **YCIS_REB_DISBURSE** | CPC Finance – Disburse | **70 (Completed)** | **50 (L5)** |

Reject at any level returns the row **one level back** (the rebate flow's own
pattern — same as L2→L1, L3→L2), and e-mails that level. L3–L6 are **central**
(sales office `0001`); L1/L2 are office-specific. On disbursement (L6) a
completion mail goes to the originating sales office (L1).

## What changed

- **YCIS_REB_EXECUTE (L3)** — on Execute now forwards the created rebate order
  to **Pending L4** (was "Completed") and e-mails L4; "Group OK" zero rows also
  forwarded to L4; duplicate guard widened to skip any row already at 40+.
- **YCIS_REB_VET / YCIS_REB_APPRV5 / YCIS_REB_DISBURSE** — new, central,
  scheme-'U' approval programs (mail content mirrors the existing rebate mails).
- **YRVU004_REB_CALC_COND_VARIANTM** — now calls **YRVU001_REB_CALC_M2_N1**
  (the new calc program) instead of the old `YRVU001_REB_CALC_M2`
  (constant `c_rep_name`, `RS_VARIANT_CONTENTS` report, and all three `SUBMIT`s).

## Prerequisites (already done for the CIS flow — same shared objects)

- `YCIS_APPRVL` must carry the L4/L5/L6 fields (L*_USER/DATE/TIME, REM_L4/L5/L6).
- Domain `YCIS_WFSTAT` must carry statuses 40=Pending L4 … 70=Completed.
Both come from the CIS 6-level DDIC changes — nothing extra here.

## Manual steps

1. **SE41 GUI status `STANDARD`** on `YCIS_REB_VET`, `YCIS_REB_APPRV5`,
   `YCIS_REB_DISBURSE` — copy from `YCIS_REB_APPROVE`
   (function codes APPR, REJ, SELALL, DESEL, BACK, EXIT; L6 APPR button = "Disburse").
2. **YCIS_WF_APPR** — central approvers under office `0001` for WF_LEVEL 4, 5, 6
   (USERID + EMAIL). These are shared with the CIS flow, so if you already added
   L4/L5/L6 approvers for CIS, the same users serve the rebate flow.
3. **Variants** — `YRVU004` now submits `YRVU001_REB_CALC_M2_N1`; make sure the
   report variants used by `YRVU004` exist on `YRVU001_REB_CALC_M2_N1`
   (copy them from `YRVU001_REB_CALC_M2` via SE38 → Variants, or recreate).
4. T-codes for the three new programs (SE93) if direct access is wanted.
