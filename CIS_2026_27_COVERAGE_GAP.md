# CIS 2026-27 — Coverage vs SAP Development Form (Gap Analysis)
Source of truth: **"SAP Development Form – CIS 2026-27" (Process Owner, 03.06.2026)** + **"User Manual for Mapping Group and Multi-Location Customers in BP"**.
Legend: ✅ Done · 🟡 Partly done · 🔴 Not started / pending · ⚪ Excluded (agreed)

| # | Dev-Form requirement | Status | Where / what remains |
|---|---|---|---|
| 1 | Scheme period **01.06.2026 – 31.03.2027** | ✅ | Period-gated in `N1` (monthly & annual) |
| 2 | Min **Monthly 75% MCQ**, **Annual 80% ACQ** | ✅ | Done in `N1` |
| 3 | Monthly qty **capped at 200 MTM** for **Trader/AUT** (PE) | 🔴 | No 200 MT cap found in code — must add, keyed to customer type (AU vs Trader/AUT) |
| 4 | Applicable PE grades: **A, B, OG, PS, GS, Powder, Polyfines** (PP grades later) | 🔴 | Grade-applicability filter for PE; PP is a later phase |
| 5 | **PS, GS, Powder, Polyfines** count for **eligibility** but **NO monthly/annual discount** | 🔴 | New exclusion rule — include in lifting/MCQ, exclude from discount value |
| 6 | Seasonal grades list incl **new B63HM0003** | 🟡 | Seasonal grades are **master-data driven** (`YRVA_PRS_GRADES` / range_s) — add `B63HM0003` + others in master, no code change |
| 7 | **Customer / Shortfall Waiver** – clause 8.I & II (AU 25% / Trader-AUT 50%; waiver counts by signing month; max 1/qtr) | 🟡 | R1: floor 25/50 config-driven **done**; **waiver-count + max-1/quarter enforcement pending** |
| 8 | **CIS Discount Structure** – clause 11 | 🔴 | Need clause-11 detail from circular to confirm rates/structure |
| 9 | **Tentative lifting** of grade linked to **MCQ & ACQ** calc (incl upward revision) | 🔴 | New calc rule — not yet implemented |
| 10 | **Multi-Location Entity** (single entity) → **combined lifting** across units | 🔴 | Logic now available (BP `TZGPMLL`); implement aggregation |
| 11 | **Group entity**: CIS 2025-26 mapping prevails; new CIS from date of **group approval** | 🔴 | Logic available (BP `TZGPGRP`); implement group aggregation + date rule |
| 12 | Shortfall grade waiver **automation** (replace manual YRVG018; PMG enters monthly/annual shortfall grades; auto-apply where lifting <75%/<80%) | 🟡 | R2: table `ZCIS_SHORTFALL_GRD` + load **done**; **auto-application into waiver flow pending** |
| 13 | **Zonal checking + CPC processing** of CIS output | ⚪ | R4 workflow — excluded from current build by instruction |
| 14 | **Report** of CIS checking / rebate order details | 🟡 | R5 `ZCIS_REBATE_REPORT` created; refine once workflow/doc-type confirmed |
| 15 | **PP grades** mapping after PP plant production starts | 🔴 | Future phase (Phase 2) |

---

## R3 — Group / MLE mapping logic (from the BP User Manual)
Now unblocked. Mapping is maintained in **BP** (T-code BP, role **`ZCUSBPX`**, tab Relationships):

- **Group customers** → relationship category **`TZGPGRP` – "Has Group Customer"** (flagship BP → member BPs, Valid From/To).
- **Multi-Location Entity (MLE)** → relationship category **`TZGPMLL` – "Has Multi Location Entity"** (flagship BP → member BPs, Valid From/To).
- Group/MLE codes still created by BIS; mapping in BP by zones (existing SOP).

**Implementation approach for `N1`:**
- Read BP relationships from **`BUT050`** (`RELTYP = 'TZGPGRP' / 'TZGPMLL'`, `PARTNER1` = flagship, `PARTNER2` = member, valid on scheme date) to build group/MLE membership.
- Aggregate lifting across members (like the current `KVGR2` group logic, but sourced from BP).
- Group: retain CIS 2025-26 mapping; apply new CIS from group-approval date.
- MLE: treat approved units as a single entity for combined lifting/eligibility.

---

## Summary
- **Fully done:** period, 75/80 minimums, error msgs, radio removal, dumps fixed, R5 report skeleton.
- **Partly done:** R1 waiver (floor yes; counts pending), R2 shortfall (load yes; apply pending).
- **Not started (new from Dev Form):** #3 200 MT cap, #4 grade applicability, #5 non-discount grades, #9 tentative-lifting link, #10 MLE, #11 Group, #15 PP phase-2, #8 discount clause-11 confirmation.
- **Master data only:** #6 seasonal grade `B63HM0003`.
- **Excluded (agreed):** #13 workflow (R4).

## New confirmations needed
1. **Clause 11** (discount structure) and **clause 8.I/8.II** (waiver) exact text from the circular.
2. Source of **customer type** AU vs Trader/AUT (for #3 cap and #7 floor).
3. How **PS/GS/Powder/Polyfines** grades are identified (via `YRVA_PRS_GRADES` indicator?) for the #5 no-discount rule.
4. **Tentative vs firm** lifting fields for #9.
5. Confirm `BUT050` relationship categories `TZGPGRP` / `TZGPMLL` for #10/#11.
