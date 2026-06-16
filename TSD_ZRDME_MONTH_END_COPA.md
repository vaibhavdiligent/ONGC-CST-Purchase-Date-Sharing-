# Technical Specification — DME Tool S/4HANA Month-End CO-PA Posting

| | |
|---|---|
| **New program** | `ZRDME_MONTH_END_COPA` (report) |
| **Reference (ECC)** | `/CCBJI/RUFIAPR_ACCURAL_DME` (Tcode `/CCBJI/RURAP_ACC_DME`) |
| **Functional Spec** | FS - DME_Month_End_COPA posting_V1.0 (18-May-2026) |
| **AS-IS source** | `dme2.pdf` (program listing — 406 pages) |
| **Scope** | ECC 6.0 → S/4HANA Transformation, *Lift Phase* |
| **Phase 1 (this delivery)** | **Spot Pay** — full logic |
| **Phase 2 (deferred)** | Pre-Pay, Post-Pay, Regular Pay, Post-Pay Secondary |

This document describes the technical realisation of the TO-BE program. It is
derived from the FS and a full read of the AS-IS source.

---

## 1. Objects delivered

| Object | Type | Purpose |
|---|---|---|
| `ZRDME_MONTH_END_COPA.abap` | Report | New month-end CO-PA posting program (Spot Pay implemented; other types routed) |
| `ZC_DME_SALES_COPA.ddls.asddls` | CDS view | Normalised sales source switching CE2JP00 (≤Nov-2027) ↔ ACDOCA (≥Dec-2027) |
| `TSD_ZRDME_MONTH_END_COPA.md` | Doc | This technical specification |

---

## 2. Program flow (FS section 3.1)

| Step | Routine | AS-IS → TO-BE change |
|---|---|---|
| 1 INITIALIZATION | `F_CLEAR` | No change |
| 2 VALIDATION | `F_VALIDATIONS`, `F_AUTHORIZATION_BUKRS` | No change |
| 3 GET contract hdr/item | `F_GET_DATA` | **NEW** — contract attributes from CCM `WCOCOH` (was rebate `KONA`) |
| 4 GET GL mapping | `F_GET_DATA` | **Extended** — `ZDME_GL_MAPPING` + new `GL_ACCT_ACCTBSD` column |
| 5 GET BSEG | `F_GET_DATA` | No change |
| 6 GET sales | `F_GET_SALES` | **NEW** — CDS view `ZC_DME_SALES_COPA` (was direct CE1/CE2 SELECT) |
| 7 Route by pay type | `F_PROCESS_ROUTE` | No change (Spot Pay implemented) |
| 8 Calc amount/tax | `F_PROCESS_SPDATA`, `F_CAL_CTAX`, `F_CAL_STAX`, `F_PRORATE` | Logic preserved |
| 9–10 POST CB + AB CO-PA | `F_POST_COPA_ACC` | **NEW** — single `BAPI_ACC_DOCUMENT_POST` posts both ledgers (was `BAPI_COPAACTUALS_POSTCOSTDATA` for CB only) |
| 11 COMMIT | `F_PROCESS_SPDATA` | No change |
| 12 UPDATE item table | `F_UPD_ITEM` | Key references CCM contract |
| 13 DISPLAY ALV | `F_DISPLAY_ALV` | Reworked to `CL_SALV_TABLE` |

---

## 3. Selection screen (FS section 2.1)

Field **names and layout are retained** from AS-IS; only labels change to
reference the CCM contract.

| Field | Type | Description | Change in S/4 |
|---|---|---|---|
| `P_BUKRS` | Parameter | Company Code | No change |
| `P_MONAT` | Parameter | Fiscal Period | No change |
| `P_GJAHR` | Parameter | Fiscal Year | No change |
| `S_KNUMA` | Select-Option | CCM Contract No. (`WCOCOH-NUM`) | Label only |
| `S_VKBUR` | Select-Option | Sales Office (`WCOCOH-VKBUR`) | Now sourced from CCM |
| `S_BOTACC` | Select-Option | Bottler Account (`WCOCOH-ZZBOTACC`) | Now sourced from CCM |
| `S_DEALER` | Select-Option | Dealer (`WCOCOH-CUST_OWNER`) | Now sourced from CCM |
| `P_BUDAT` | Parameter | Posting Date Override | No change |
| `RB_SALE / RB_OTHR` | Radio group | Sales Update / Other | Controls sales source |
| `RB_PRE/POS/SPOT/REG/SEC` | Radio group | Payment Type | No change |
| `RB_TEST / RB_POST` | Radio group | Test / Production | No change |

---

## 4. Contract object migration — KONA → WCOCOH

The contract management backend is migrated from ECC Rebate Management
(VBO1/VBO2, tables `KONA`/`KONP`) to S/4 CCM Condition Contract Management
(`WCOCOH`/`KONP`). In the program the AS-IS `ty_kona` read is replaced by
`ty_ccm`, populated from `WCOCOH`:

| AS-IS (KONA) | TO-BE (WCOCOH) |
|---|---|
| `KNUMA` | `NUM` |
| `VKBUR` | `VKBUR` |
| — | `ZZBOTACC` (bottler account append) |
| — | `CUST_OWNER` (contract owner / dealer) |
| `DATAB / DATBI` | `DATAB / DATBI` |
| `BOART` | `BOART` |

> The FI document posting at contract approval is **unchanged in the lift
> phase**; only contract creation moves to CCM.

---

## 5. GL determination (FS 4.2.2 / 9.3) — updated per client (B1)

**Client decision (B1):** a separate Account-Based CO-PA GL mapping is *not*
yet confirmed. Until it is, GLs are read from the **existing** `/CCBJI/T_DME_GL`
table and the **same GL is used for CB and AB** CO-PA.

`/CCBJI/T_DME_GL` is read by `BOART` + `KVSL1`:

* `SAKN1` → CB CO-PA debit GL (`gv_dbgl`) **and** AB CO-PA GL (`gv_abgl`) — interim
* `VFIELD` → CB CO-PA value field

The `GL_ACCT_ACCTBSD` field is retained (reserved) in the data model. When a
separate AB GL mapping is confirmed, point the GL read at `ZDME_GL_MAPPING`
and `gv_abgl` automatically switches to the new column.

---

## 6. CO-PA posting (FS 4.3) — TO-BE

`F_POST_COPA_ACC` builds and calls `BAPI_ACC_DOCUMENT_POST`:

* `DOCUMENTHEADER` — posting date, company code, doc type `GR`, contract ref
* `ACCOUNTGL` — one debit line per allocation row (AB GL when mapped, else CB
  GL) + offsetting credit line
* `CURRENCYAMOUNT` — proportional amounts in document currency (JPY)
* `CRITERIA` — profitability segment characteristics (FS 4.2.7 / 4.3.3):
  `KNDNR, ARTNR, WERKS, VKORG, VTWEG, PRCTR, KMVKBU` and custom
  `VKAUS, WW228, WW229, KUNWE`
* `ACCOUNTTAX` / `RETURN` — tax lines and messages

On a **test run** `BAPI_ACC_DOCUMENT_CHECK` is used; production run posts and
commits via `BAPI_TRANSACTION_COMMIT` (rollback on error).

---

## 7. Dummy sales (FS 4.2.5)

The AS-IS `1.01` write-back to `CE2JP00` (`F_DUMMY_SALES`) is **removed**. When
no actual sales exist for a contract, a single allocation line on the **dummy
material `9651030000`** (confirmed by client — B2) carries the full contract
amount.

---

## 8. Configuration prerequisites

| # | Item | Owner |
|---|---|---|
| 8.1 | Operating concern **JP00**: activate Account-Based CO-PA in parallel with Costing-Based (`KEA0`) | CO |
| 8.2 | Add custom characteristics (`VKAUS`, `WW228`, `WW229`, `KUNWE`) to the AB operating concern; `CI_ACDOCA` extension fields | CO / Basis |
| 8.3 | Extend `ZDME_GL_MAPPING` with `GL_ACCT_ACCTBSD` and maintain values | Finance/CO |
| 8.4 | Review `KEDR` derivation + `COPA0002` exits write to ACDOCA extension fields | CO |
| 8.5 | Create CDS view `ZC_DME_SALES_COPA` (parameter `p_cutover`); adjust ACDOCA extension field names | Dev |
| 8.6 | Create transaction code for `ZRDME_MONTH_END_COPA` | Dev |
| 8.7 | Maintain TVARVC `/CCBJI/DME_ACDOCA_FROM` = cut-over period (test `2026001`, prod `2027012`) — F3 | Functional/Basis |

---

## 9. Open / feasibility items — status after client response (16-Jun-2026)

Full responses are tracked in `OPEN_QUESTIONS_DME_COPA_FC.docx`.

| Ref | Item | Client response | Dev action |
|---|---|---|---|
| B1 | Separate AB GL accounts | Not confirmed — use `/CCBJI/T_DME_GL` (same GL for CB+AB) for now | ✅ Implemented |
| B2 | Dummy material | `9651030000` | ✅ Implemented |
| B3 | Cross-territory scope | In scope — test case `5000442076` | ⏳ Phase-1 parity backlog |
| F1 | `BAPI_ACC_DOCUMENT_POST` dual CB+AB posting | Assumed yes; validate in sandbox with CO (test cases shared) | ✅ Coded; **pending sandbox** |
| F2 | Custom characteristics in AB op. concern | Assumed AS-IS; **pending Gaurav-san** | ✅ Coded as assumed |
| F3 | CE2JP00 → ACDOCA cut-over | Test: ACDOCA from Jan-2026; Prod: CE2 till Nov-2027 / ACDOCA from Dec-2027 | ✅ Made configurable (TVARVC + CDS param) |
| T1 | WCOCOH append field names | **Pending Pankaj-san** | ⏳ Placeholder retained |
| T2 | CI_ACDOCA append field names | Assumed unchanged; **pending Gaurav-san** | ⏳ Placeholder retained |

---

## 10. Reversal (FS 4.5) — Phase 1 note

Reversal logic (FI reversal → re-post → CB CO-PA reverse → **AB CO-PA reverse
(NEW)**) is **not** in this Phase-1 delivery; it is a follow-up once the
forward posting feasibility (item #1) is confirmed. The document flow chain to
maintain is: CCM contract → FI document → CB CO-PA → AB CO-PA.

---

## 11. Out of scope (FS section 12)

CCM settlement engine for FI posting, real-time CO-PA at FI posting, Fiori
changes, CE2JP00 direct-read decommission (until Nov-2027), CB CO-PA
switch-off (planned Jan-2028), and the Internal Payment Statement report.
