# System findings — verified against system CRS

Source: `vendor_master.zip` (`cvers.txt`, `CL_MD_BP_MAINTAIN_1.pdf`,
`CL_MD_BP_MAINTAIN-MAINTAIN.pdf`, `TFKTAXNUMTYPE.xlsx`, `tier_2.xlsx` = `DD03L`
extract of 26 tables/structures, 5 687 rows).

Everything below is now **verified**, not assumed.

---

## 1. Release

| Component | Release | SP |
|---|---|---|
| `S4CORE` / `S4COREOP` / `S4FND` | **109** | 0001 |
| `SAP_BASIS` / `SAP_ABA` | **816** | 0001 |
| `MDG_APPL` / `MDG_FND` | 809 | 0001 |
| `IS-OIL`, `IS-PRA` | 809 | 0001 |

System ID **CRS**, client 500. Comfortably past the 1709 FPS02 cut-off, so
`CL_MD_BP_MAINTAIN` is the correct API and `RFC_CVI_EI_INBOUND_MAIN` is not needed.

## 2. `XK01` / `XK02` / `XK05` — confirmed gone

Confirmed by the project. The BDC/LSMW rework described in §2 of the plan stands.

## 3. `CL_MD_BP_MAINTAIN` — signature confirmed, and better than assumed

```abap
" Static, public
CL_MD_BP_MAINTAIN=>MAINTAIN(
  EXPORTING i_data     = lt_data      " TYPE cvis_ei_extern_t   <-- a TABLE
            i_test_run = abap_true    " TYPE boole_d  OPTIONAL
  IMPORTING e_return   = lt_return ). " TYPE bapiretm

CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE(
  EXPORTING i_data                   = ls_data   " TYPE cvis_ei_extern
            i_data_db                = ls_db     " TYPE cvis_ei_extern  OPTIONAL
            iv_test_run_mode         = abap_true
            iv_suppress_taxjur_check = abap_false
  IMPORTING et_return_map            = lt_map ). " TYPE mdg_bs_bp_msgmap_t
```

Four things this changes in our favour:

1. **`I_DATA` is a table type (`CVIS_EI_EXTERN_T`), not a single structure.** The API is
   natively mass-capable — we can pass a block of rows in one call rather than looping.
2. **`I_TEST_RUN` is built in.** The method drives `BUP_SET_GET_TESTRUN`,
   `ADDR_SET_GET_TESTRUN`, `CMD_SET_GET_TESTRUN`, `VMD_SET_GET_TESTRUN` and
   `PARTNER_SET_GET_TESTRUN` itself. We do **not** hand-roll simulation — the `p_test`
   checkbox maps straight onto this parameter.
3. **`E_RETURN TYPE BAPIRETM`** carries a per-object index (`lv_object_idx = sy-tabix`
   in the source), so messages come back already attributed to the input row.
4. **`VALIDATE_SINGLE` returns `MDG_BS_BP_MSGMAP_T` — a message-to-field map.** This
   largely retires risk **R2**: we can point the user at the offending column instead of
   relaying a generic BAPI message.

**Task constants (public):** `GC_TASK_CREATE = 'I'`, `GC_TASK_CHANGE = 'U'`,
`GC_TASK_MODIFY = 'M'`, `GC_OBJECT_TASK_CURRENT_STATUS = 'C'`.

**The class does its own authorisation checks** — `AUTHORITY_CHECK`,
`SUPPLIER_AUTHORITY_CHECK`, `AUTHORITY_CHECK_LFB1_SINGLE`, `AUTHORITY_CHECK_LFM1_SINGLE`.
Our program still checks up front for a clean error message, but we are not the only line
of defence.

**BP grouping is derived by the class** via `DETERMINE_BP_GROUP` / `GET_ACCOUNT_GROUP` /
`SET_ACCOUNT_GROUP`, reading `TB033` / `TB035`. That partly answers **Q2** — but SAP's own
documentation warns:

> *"To get all the relevant validation checks the BP grouping in the BP complex structure
> and the account grouping in the customer/vendor complex structure must be transferred
> along with all the other key data."*

So we should still pass both explicitly rather than rely on derivation.

### Two contract details that change the design

**(a) `VALIDATE_SINGLE` expects *gross* data, not a delta.** Its own comment:

> *"Expects gross per segment. Example 'Bank Details': Some data provided → provided data
> checked, also deletion of bank detail not provided."*

**This answers Q5, and not the way I proposed.** For bank details the API semantics are
**replace, not append** — supplying a partial list *deletes* the accounts you left out.
Scenario 4 must therefore **read the existing `LFBK` set, merge the uploaded rows in, and
pass the complete set**. Shipping the naive "append" design would have silently deleted
payment-relevant bank data. The same gross-per-segment rule applies to every table-like
segment (withholding tax, partner functions), so each of those handlers reads-then-merges.

**(b) `I_DATA_DB`** should carry the current DB state on change scenarios, which fits the
read-then-merge pattern above.

**Non-core / append fields** have a sanctioned path: `MAINTAIN_NON_CORE_VALUE` →
`CVIS_EI_EXTERN-EXT_APPL_DATA`. Not needed for this build (see §5), but worth recording.

## 4. India tax number categories — `IN3` does not exist here

`TFKTAXNUMTYPE` (client 500) contains only: **`IN0`, `IN1`, `IN2`** (*India: Tax
Identification Number (TIN)*), **`IN4`, `INS3`**. There is **no `IN3`**.

So the GST tax-number category was never activated in this system, and:

- **GSTIN → `LFA1-STCD3` only.** My planned dual write to a BP tax number is wrong and has
  been removed.
- **PAN → `LFA1-J_1IPANNO`** (see §5), not a BP tax number.
- **`BUS_EI_BUPA_TAXNUMBER` is not used at all** in this build.

*Answers Q3.*

## 5. The big one — CIN fields are in `LFA1` **and** in the CVI API structure

`LFA1` carries all of them natively:

`J_1IPANNO`, `J_1IPANREF`, `J_1IPANVALDT`, `J_1ISSIST`, `VEN_CLASS`, `J_1IEXCD`,
`J_1IEXRN`, `J_1IEXRG`, `J_1IEXDI`, `J_1IEXCO`, `J_1ICSTNO`, `J_1ILSTNO`, `J_1ISERN`,
`J_1IEXCIVE`, `J_1IVTYP`, `J_1IDEDREF`, `J_1IVENCRE`, `J_1I_CUSTOMS`, `STCD1`–`STCD6`.

Critically, **`VMDS_EI_VMD_CENTRAL` carries every one of them twice** — once in `DATA`,
once in `DATAX`. That means they are fully maintainable through
`CL_MD_BP_MAINTAIN`, with the normal `DATAX` flag discipline.

**Consequence: class C drops from 18 columns to zero.** No direct table update, no BDC on
`BP`, no unsupported `MODIFY` on `J_1IMOVEND`, and **risk R3 is closed**. Scenario 7
becomes an ordinary API scenario like the rest.

(`J_1IMOVEND` still exists with 26 fields and mirrors the same list, plus `CORP_IDENT_NO`
and `PSU_VEN`. We write through `LFA1` via the API and leave `J_1IMOVEND` to CVI.)

The *business* question — are the pre-GST excise fields still used? — is unaffected and
still open. But it is now a scoping question, not a technical blocker.

## 6. Structure paths — three corrections

| Was (in the first Annex A) | Correct |
|---|---|
| `COMPANY[]-WTAX_TYPE[]-…` | **`COMPANY[]-WTAX_TYPE-WTAX_TYPE[]-…`** (`VMDS_EI_WTAX_TYPE_S` wraps `VMDS_EI_WTAX_TYPE_T`) |
| `PURCHASING[]-FUNCTIONS[]-…` | **`PURCHASING[]-FUNCTIONS-FUNCTIONS[]-…`** (`VMDS_EI_VMD_FUNCTIONS` wraps `VMDS_EI_FUNCTIONS_T`) |
| `FUNCTIONS[]-DATA-LIFN2` | **`FUNCTIONS-FUNCTIONS[]-DATA-PARTNER`** (type `GPANR`; `LIFN2` is the `WYT3` column name, not the API field) |

Confirmed as correct: `VMDS_EI_EXTERN-HEADER-OBJECT_INSTANCE` / `-OBJECT_TASK`,
`-CENTRAL_DATA-CENTRAL-DATA` / `-DATAX`, `-COMPANY_DATA-COMPANY[]-DATA_KEY` / `-DATA` /
`-DATAX` / `-DUNNING` / `-WTAX_TYPE`, `-PURCHASING_DATA-PURCHASING[]-DATA_KEY` / `-DATA` /
`-DATAX`. `CVIS_EI_EXTERN` top level: `PARTNER`, `CUSTOMER`, `VENDOR`, `PARTNER_RELATION`,
`EXT_APPL_DATA`, `ADDITIONAL_VENDORS`, `ENSURE_CREATE`.

Vendor bank details hang off `CENTRAL_DATA-BANKDETAIL` of type **`CVIS_EI_BANKDETAIL`**
(not `VMDS_EI_BANKDETAIL`, which does not exist — that was the one name missing from the
extract).

## 7. `LFM1` — Q7 answered, my assumption was right

| Field | Type | Check table |
|---|---|---|
| `BSTAE` (Confirmation Control) | CHAR 4 | **`T163L`** |
| `KZABS` (Check Acknowledgement) | CHAR 1 | — |
| `KALSK` | CHAR 2 | `TMKK` |
| `INCO1` | CHAR 3 | `TINC` |
| `WAERS` | CUKY 5 | `TCURC` |

`LFBW` = `BUKRS`, `LIFNR`, `WITHT`, `WT_WITHCD`, `WT_SUBJCT`, `QSREC`, `WT_WTSTCD`,
`WT_EXNR`, `WT_EXRT`, `WT_WTEXRS`, `WT_EXDF`, `WT_EXDT` — matches the TDS template exactly.

`WYT3` = `LIFNR`, `EKORG`, `PARVW`, `PARZA`, `LIFN2`, `LTSNR`, `WERKS`, `PERNR`, `PARNR`,
`DEFPA` — matches the partner-function template.

---

## Revised coverage

| Class | Meaning | Before | **Now** |
|:---:|---|---:|---:|
| **A** | `CL_MD_BP_MAINTAIN` | 220 | **238** |
| **B** | `BAPI_BANK_*` | 17 | 17 |
| **C** | No standard API | 18 | **0** |
| **D** | Dead artifact, ignored | 11 | 11 |
| **E** | Unmapped | 8 | 8 |
| | **Total** | 274 | 274 |

**All 11 scenarios are now technically buildable.** The only columns that are not
API-addressable are the 17 bank-master fields (a different API by design) and the 8 in the
TAN template that need a functional answer.

## Risks closed / changed

| Risk | Status |
|---|---|
| **R2** — generic API messages | **Largely closed** by `ET_RETURN_MAP` |
| **R3** — no API for `J_1IMOVEND` | **Closed** — fields are in `LFA1` + CVI structure |
| **R4** — row-level commit performance | **Reduced** — `I_DATA` is a table; commit per block |
| *new* — **gross-segment semantics silently delete data** | **Open, high.** Every table-like segment must be read-then-merged, never sent partial |
