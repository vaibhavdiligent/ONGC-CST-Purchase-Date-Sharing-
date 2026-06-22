# Build Readiness Checklist — `ZRDME_MONTH_END_COPA`

**Question this answers:** *Can we build the program now, or do we need more input?*
**Short answer:** Yes to DEV build + **test runs**, after the 2 field-name confirmations (T1, T2). Production-correct posting needs the functional sign-offs below.

Legend: 🔴 blocks activation · 🟠 blocks correct/production posting (not activation) · ✅ done.

---

## 1. Prerequisites to ACTIVATE in DEV (🔴 — compile/DDIC)

| ✔ | Item | Owner | Where to fix |
|---|---|---|---|
| ☐ | **T1** — confirm WCOCOH append names `ZZBOTACC` / `CUST_OWNER` **(only remaining blocker)** | Pankaj-san | `ZRDME_MONTH_END_COPA` → `F_GET_DATA` "T1 EDIT POINT" |
| ✅ | **T2** — CI_ACDOCA characteristics `WW207/WW214/WW228/WW229/VKAUS` — **CONFIRMED present in ACDOCA** (16-Jun-2026) | Functional | no change needed |
| ☐ | DDIC objects present in target: `/CCBJI/T_DME_GL`, `/CCBJI/T_DME_HDR`, `/CCBJI/T_DME_ITM`, `WCOCOH`, `CE2JP00`, `ACDOCA`, message class `/CCBJI/RTR` | Basis | system import |
| ☐ | Create CDS view `ZC_DME_SALES_COPA` (parameter `p_cutover`) | Dev | new object |
| ☐ | Create report `ZRDME_MONTH_END_COPA` + text symbols (text-001/002/012/016/027) | Dev | new object |

> All external field-name dependencies are isolated to the **two EDIT POINT
> banners** — see the mapping block at the top of the program. Changing them
> there is the only edit needed if names differ.

---

## 2. Prerequisites to RUN a TEST run (config)

| ✔ | Item | Owner |
|---|---|---|
| ☐ | TVARVC `/CCBJI/DME_ACDOCA_FROM` = `2026001` (test) | Functional/Basis |
| ☐ | TVARVC `/CCBJI/RTR_DME_CRGL` (credit GL) maintained | Functional |
| ☐ | `/CCBJI/T_DME_GL` has rows for the test company / BOART / KVSL1 | Finance/CO |
| ☐ | Dummy material `9651030000` exists in the sales org/plant | Master data |
| ☐ | Test contract(s) incl. cross-territory `5000442076` available | Functional |

Run with **RB_TEST = X** (uses `BAPI_ACC_DOCUMENT_CHECK`, no DB update).

---

## 3. Prerequisites for PRODUCTION posting (🟠 — functional sign-off)

| ✔ | Ref | Item | Owner |
|---|---|---|---|
| ☐ | **F1** | Sandbox proof that `BAPI_ACC_DOCUMENT_POST` writes **both** CB + AB CO-PA on JP00 | CO |
| ✅ | **F2** | Custom characteristics in AB operating concern — **CONFIRMED available & passable** (16-Jun-2026) | — |
| ☐ | **F4** | CCM vs DME-Z-header status mapping (Approved 05 / Calculated 06) | Functional |
| ☐ | **F5** | DME Z-table vs CCM data-ownership split confirmed | Functional |
| ☐ | **F6** | RB_SALE/RB_OTHR record-type split still required? | Functional |
| ☐ | **F8** | Tax rounding / difference rule confirmed | Functional |
| ☐ | **F9** | Header completion / LSA-stop update required for Spot Pay? | Functional |
| ☐ | **B1** | Separate AB GL accounts (only if the interim single-GL is not acceptable) | Finance/CO |
| ☐ | TVARVC `/CCBJI/DME_ACDOCA_FROM` switched to `2027012` for production | Functional |

---

## 4. Already settled — no action

| Ref | Decision | Status |
|---|---|---|
| B1 | GL from `/CCBJI/T_DME_GL`, AB = CB GL (interim) | ✅ coded |
| B2 | Dummy material `9651030000` | ✅ coded |
| B3 | Cross-territory: company range + per-company dummy + cost-center split | ✅ core coded |
| F1/F2 | Dual-COPA via `BAPI_ACC_DOCUMENT_POST` + CRITERIA | ✅ coded (pending proof) |
| F3 | Configurable cut-over (TVARVC + CDS param) | ✅ coded |

---

## 5. Verdict (updated 16-Jun-2026, round 2)

- **Build + activate in DEV:** ready as soon as **T1** is confirmed (single one-line edit; T2 now confirmed).
- **Test runs:** ready once section 2 config is in place.
- **Production posting:** gated on **F1** sandbox proof (F2 now confirmed) plus the F4–F9 functional confirmations.
- **Still on the backlog:** fine-grained cross-territory customer include/exclude split (F7), reversal (E1/E2), Phase-2 payment types (F10).
