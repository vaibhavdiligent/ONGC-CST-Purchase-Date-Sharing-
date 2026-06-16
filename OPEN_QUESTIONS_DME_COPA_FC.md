# DME Month-End CO-PA — Clarifications Log (Business · Functional · Technical)

**Program:** `ZRDME_MONTH_END_COPA` (S/4HANA transformation of `/CCBJI/RUFIAPR_ACCURAL_DME`)
**FS:** FS - DME_Month_End_COPA posting_V1.0 (18-May-2026)
**Raised by:** Development team — Phase 1 (Spot Pay)
**Status:** Open — single consolidated list of all open points

This one document holds **all** open items, grouped by who owns the answer:
- **Business** — decisions on rules, values and process.
- **Functional** — FS interpretation, mapping and configuration decisions (FC / CO consultant).
- **Technical** — development / Basis / DDIC build details.

Severity: 🔴 Blocking (Phase-1 production posting) · 🟠 Needed for full Spot-Pay parity · 🟢 Nice-to-have / later phase.
Each item maps to a `TODO-FS` marker in the code and to `TSD_ZRDME_MONTH_END_COPA.md`.

---

## 1. BUSINESS CLARIFICATIONS

| # | Sev | Question | Currently assumed | Decision needed from business |
|---|---|---|---|---|
| B1 | 🔴 | **Account-Based GL accounts** (`GL_ACCT_ACCTBSD`) per `BOART`/`KVSL1` row, incl. the tax GL. | New column read but not populated. | Finance/CO to provide the GL list to maintain in `ZDME_GL_MAPPING`. |
| B2 | 🟠 | **Dummy material number(s)** to use when no actual sales exist (per company 7827 / 7828?). | Placeholder `DME_DUMMY`. | Provide real material(s). |
| B3 | 🟠 | Is **cross-territory / cross-company** allocation (sub-company, sales org 0803, dual dealer/bottler) still in scope for S/4? | Only main-company path coded. | Confirm scope; provide a test contract. |
| B4 | 🟠 | Are **multiple tax rates per contract** (per usage code) still applicable? | Single tax path coded. | Confirm + rules. |
| B5 | 🟠 | Is **catch-up over missed periods** (post un-posted prior months at month-end) still required? | Current period only. | Confirm. |
| B6 | 🟢 | **Reversal** trigger & timing (FS 4.5) — Phase 1 or later? | Deferred. | Confirm phase. |
| B7 | 🟢 | **Build order / timeline** for Phase-2 pay types (Pre / Post / Regular / Secondary). | Routed stubs. | Prioritise. |

---

## 2. FUNCTIONAL CLARIFICATIONS (FC / CO consultant)

| # | Sev | Question | Currently assumed | Decision needed |
|---|---|---|---|---|
| F1 | 🔴 | Can `BAPI_ACC_DOCUMENT_POST` post **both CB and AB CO-PA in one call** when both are active on op. concern JP00? (FS open item #1) | Single call posts both. | Confirm with CO + sandbox test. |
| F2 | 🔴 | Are custom characteristics **VKAUS, WW228, WW229, KUNWE** present in the **account-based** op. concern and passable via `CRITERIA`? | They exist and are passed. | Confirm KEA0 + KEDR derivation simulation. |
| F3 | 🔴 | **FS date contradiction** for sales source cut-over: section 3 says *"ACDOCA from Jan-2026"*, section 4.2.4 says *"CE2JP00 till Nov-2027 / ACDOCA from Dec-2027"*. Which is correct? | Coded Nov/Dec-2027 boundary. | Resolve the date. |
| F4 | 🟠 | Which **CCM status/type** maps to AS-IS "Approved (05)" / "Calculated (06)" — does status stay on the DME Z-header or move to `WCOCOH-STATUS`? | DME Z-header status reused. | Confirm. |
| F5 | 🟠 | Are the **DME Z-tables** (`/CCBJI/T_DME_HDR`/`_ITM`) still populated in S/4, or do attributes now come fully from CCM? | Z-tables retained; only office/bottler/dealer/validity from WCOCOH. | Confirm data ownership split. |
| F6 | 🟠 | For `RB_SALE` vs `RB_OTHR`: is the **record-type / value-field** split (CE1 'F' vs CE2 '5') still required, or fully replaced by ACDOCA actuals? | Unified via CDS; split dropped. | Confirm. |
| F7 | 🟠 | **Customer/material include-exclude conditions** (KONP `YJ12/13/14/15`) — how represented in CCM? | Replaced by direct CCM filters. | Confirm condition-type mapping. |
| F8 | 🟠 | **Tax rounding / difference adjustment** rule to tie total CO-PA to the contract amount (J_1I6 rounding; last-line vs proportional). | Single last-line correction. | Confirm expected behaviour. |
| F9 | 🟠 | **Header completion / LSA-stop** update at settlement (`f_upd_htab`) — still required for Spot Pay? | Item update only; header LSA-stop omitted. | Confirm. |
| F10 | 🟢 | Pre-Pay monthly **amortization FI + CO-PA** handling and Post-Pay **accrual frequency** rules (FS 5–8 overview only). | Not built. | Detailed logic for Phase 2. |
| F11 | 🟢 | **KEDR derivation rules / COPA0002 exits** confirmed to write to ACDOCA extension fields (FS 9.4)? | Assumed carried over. | Confirm. |

---

## 3. TECHNICAL CLARIFICATIONS (Dev / Basis / DDIC)

| # | Sev | Question | Currently assumed | Decision needed |
|---|---|---|---|---|
| T1 | 🔴 | Exact **WCOCOH append field names** for bottler account / dealer (FS: `ZZBOTACC` / `CUST_OWNER`). | Used `ZZBOTACC`, `CUST_OWNER`. | Confirm actual append names. |
| T2 | 🔴 | Exact **CI_ACDOCA append field names** holding WW207/WW214/WW228/WW229/VKAUS — needed to finalise the ACDOCA branch of CDS `ZC_DME_SALES_COPA`. | Same-named placeholders. | CO/Basis to confirm. |
| T3 | 🟠 | DDIC: add column **`GL_ACCT_ACCTBSD`** to `ZDME_GL_MAPPING` (table change + transport). | Coded as if present. | Confirm DDIC change owner. |
| T4 | 🟢 | New **transaction code** name (AS-IS was `/CCBJI/RURAP_ACC_DME`). | None set. | Confirm. |
| T5 | 🟢 | **Authorization object** — keep `F_BKPF_BUK` activity `01`? | Kept. | Confirm. |
| T6 | 🟢 | **Message class** — reuse `/CCBJI/RTR` or new Z class? | Reused `/CCBJI/RTR`. | Confirm. |
| T7 | 🟢 | **Package / naming / transport** standards for the S/4 build. | Root dev dump. | Confirm. |

---

### How to use this log
Respond per row: **Confirm** / **Clarify** / **Provide value**. The 🔴 rows (**B1, F1, F2, F3, T1, T2**) are prerequisites — Spot-Pay production posting cannot be finalised until they are answered.

---

## 4. CLIENT RESPONSES & RESOLUTION (received 16-Jun-2026)

| # | Client response | Dev action | Status |
|---|---|---|---|
| B1 | No separate AB GL yet. Use the same GLs maintained in `/CCBJI/T_DME_GL` until a separate account-based mapping is confirmed. | GL read switched to `/CCBJI/T_DME_GL`; AB GL = CB GL (`SAKN1`). `GL_ACCT_ACCTBSD` kept reserved. | ✅ Implemented |
| B2 | Dummy material = **9651030000**. | `c_dummy_mat = '9651030000'`. | ✅ Implemented |
| B3 | In scope — test case **5000442076**. | Cross-territory / cross-company logged for Phase-1 parity build. | ⏳ Backlog |
| F1 | Assume single BAPI call posts both CB + AB; must be validated in sandbox with CO (test cases shared). | Design retained. | ✅ Coded · pending sandbox |
| F2 | Assume AS-IS; custom chars expected available. To be confirmed by **Gaurav-san**. | CRITERIA passes VKAUS/WW228/WW229/KUNWE. | ✅ Coded · pending confirm |
| F3 | **Testing:** ACDOCA from Jan-2026. **Production:** CE2JP00 till Nov-2027, ACDOCA from Dec-2027. | Cut-over made configurable: TVARVC `/CCBJI/DME_ACDOCA_FROM` → CDS parameter `p_cutover`. | ✅ Implemented |
| F4–F11 | Not yet answered. | Current assumptions retained. | ⏳ Open |
| T1 | WCOCOH append field names to be confirmed with **Pankaj-san**. | Placeholders `ZZBOTACC` / `CUST_OWNER` retained. | ⏳ Pending |
| T2 | Assume CI_ACDOCA field names unchanged; confirm with **Gaurav-san**. | Placeholders retained in CDS. | ⏳ Pending |
| T3–T7 | Not yet answered. | Current assumptions retained. | ⏳ Open |

**Remaining blockers before production posting:** F1 sandbox validation (CO), F2 + T2 (Gaurav-san), T1 (Pankaj-san).
