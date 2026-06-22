# Handover — DME Tool: S/4HANA Month-End CO-PA Posting

**Purpose:** everything a developer/consultant needs to take this object forward.
**Branch:** `claude/dreamy-davinci-8vnt1y`  · **PR:** #207
**Last updated:** 16-Jun-2026 (client round-2 responses processed)

---

## 1. What this is

S/4HANA transformation of the ECC program **`/CCBJI/RUFIAPR_ACCURAL_DME`**
(Tcode `/CCBJI/RURAP_ACC_DME`) per **FS - DME_Month_End_COPA posting_V1.0**.
It posts month-end CO-PA allocations for DME contracts.

- **Phase 1 (built): Spot Pay** — end to end.
- **Phase 2 (not built): Pre-Pay, Post-Pay, Regular Pay, Post-Pay Secondary** — routed stubs.

Key TO-BE changes vs ECC: contract data from CCM (`WCOCOH`) instead of rebate
(`KONA`); sales via a CDS view (CE2JP00 → ACDOCA switch); **both Costing-Based
and Account-Based CO-PA** posted in one run via `BAPI_ACC_DOCUMENT_POST`.

---

## 2. Objects to create (names)

| Object | Type | Name | Source file |
|---|---|---|---|
| Report | Executable program | `ZRDME_MONTH_END_COPA` | `ZRDME_MONTH_END_COPA.abap` |
| CDS view | DDL source | `ZC_DME_SALES_COPA` (SQL view `ZCDMESALESCOPA`) | `ZC_DME_SALES_COPA.ddls.asddls` |
| TVARVC | Config (STVARV) | `/CCBJI/DME_ACDOCA_FROM`, `/CCBJI/RTR_DME_CRGL` | — |
| Tcode | SE93 | e.g. `ZRDME_ACC_DME` (to confirm) | — |

**Create the CDS view first** (the program selects from it).

---

## 3. Files in this repo (what to read)

| File | What it is |
|---|---|
| `ZRDME_MONTH_END_COPA.abap` | The program source — copy into SE38/ADT |
| `ZC_DME_SALES_COPA.ddls.asddls` | The CDS view source — copy into ADT |
| `DEPLOYMENT_GUIDE_DME_COPA.md` | Step-by-step: how to create the CDS + program, TVARVC, tcode, smoke test |
| `BUILD_READINESS_DME_COPA.md` | Checklist: what gates activation / test / production |
| `OPEN_QUESTIONS_DME_COPA_FC.md` / `.docx` | All open items + client answers (the clarifications log) |
| `TSD_ZRDME_MONTH_END_COPA.md` | Technical design: flow, mappings, config, status |
| `HANDOVER_DME_COPA.md` | **This file** |

Reference inputs (originals): `FS -DME_Month_End_COPA posting_V1.0.docx`,
`dme1.pdf` (stub), `dme2.pdf` (full 406-page AS-IS listing).

---

## 4. Current status

✅ **Settled & coded:** B1 (GL from `/CCBJI/T_DME_GL`, AB=CB GL), B2 (dummy
material `9651030000`), B3 (cross-territory core), F2 (custom characteristics
confirmed), F3 (cut-over configurable), T2 (ACDOCA characteristics confirmed).

⏳ **To ACTIVATE — 1 item left:**
- **T1** — confirm WCOCOH append names `ZZBOTACC` / `CUST_OWNER` with
  **Pankaj-san**, then edit the "T1 EDIT POINT" in `F_GET_DATA`. One-line change.

⏳ **To POST IN PRODUCTION — 1 item left:**
- **F1** — sandbox proof (with CO) that `BAPI_ACC_DOCUMENT_POST` writes **both**
  CB and AB CO-PA on operating concern JP00.

⏳ **Functional confirmations still open:** F4 (status mapping), F5 (Z-table vs
CCM ownership), F6 (record-type split), F8 (tax rounding rule), F9 (LSA-stop).

📋 **Backlog (not started):** F7 fine-grained cross-territory customer
include/exclude split; reversal flow (FS 4.5); Phase-2 payment types.

---

## 5. Next steps for whoever picks this up

1. Read `DEPLOYMENT_GUIDE_DME_COPA.md` and create the **CDS view**, then the **program**.
2. Get **T1** (WCOCOH field names) from Pankaj-san → apply at the T1 edit point → activate.
3. Maintain TVARVC `/CCBJI/DME_ACDOCA_FROM = 2026001` and the credit GL.
4. Run a **test run** (`RB_TEST = X`) for Spot-Pay test contract; cross-territory test case **5000442076**.
5. Drive **F1** sandbox validation with CO; once green, enable production posting.
6. Pick up the backlog (reversal, Phase-2) and remaining functional items (F4–F9).

---

## 6. How to find things in the code

- All external/unconfirmed field names are isolated under the banner
  **"EXTERNAL FIELD-NAME MAPPING — SINGLE EDIT POINT"** at the top of the program.
- Search the program for **`TODO-FS`** to see every open/feasibility item inline.
- Form routines follow the FS 13-step flow: `F_CLEAR` → `F_VALIDATIONS` →
  `F_GET_DATA` → `F_PROCESS_ROUTE` → `F_PROCESS_SPDATA` (Spot Pay) →
  `F_POST_COPA_ACC` (dual CB+AB BAPI) → `F_UPD_ITEM` → `F_DISPLAY_ALV`.

---

## 7. Contacts (open items)

| Item | Owner |
|---|---|
| T1 — WCOCOH append field names | Pankaj-san |
| F1 — BAPI dual-posting sandbox proof | CO team |
| F4–F9 — functional confirmations | Functional consultant |
| AB GL accounts (if B1 interim not acceptable) | Finance / CO |
