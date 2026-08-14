# ZF01_EXCHANGE_RATE — Full Solution & Handover

Knowledge-transfer doc for the **Exchange Rate mass-upload interface**: a custom SAP
Gateway **OData V2** service that receives foreign-exchange rates from **SAP CPI** and
writes them into **TCURR** via `BAPI_EXCHRATE_CREATEMULTIPLE`. Read this to understand
the whole solution, the final working code, the CPI setup, and every error we hit + fix.

---

## 1. Goal & data flow

A partner/source system produces a file of exchange rates. **CPI** sends them to our
**OData service**, which validates and posts them to `TCURR`.

```
Source XML (<ExchangeRates><ExchangeRate>xN)
        |  SAP CPI  (splitter/mapping -> $batch envelope)
        v
OData V2 service  ZF01_EXCHANGE_RATE_SRV   (SAP Gateway, backend)
        |  CREATE_ENTITY per record  ->  BAPI_EXCHRATE_CREATEMULTIPLE
        v
TCURR  (view in OB08)
```

- **System / client** seen in debug: `vhovlocqci` / OCQ 500.
- **Service**: `ZF01_EXCHANGE_RATE_SRV` (SEGW project `ZF01_EXCHANGE_RATE`).
- **Classes**: `ZCL_ZF01_EXCHANGE_RATE_MPC(_EXT)`, `ZCL_ZF01_EXCHANGE_RATE_DPC(_EXT)`.

---

## 2. OData model (FINAL = FLAT)

We tried deep (header+navigation) and flat; **the deployed model is FLAT**:

- **Entity type** `ExchangeRate` — the 10 business fields.
- **Entity set** `ExchangeRates` (a flat collection of `ExchangeRate`).
- **No** header entity, **no** navigation, **no** REQUEST_ID.
- Composite key: `RATE_TYPE`, `FROM_CURR`, `TO_CURRNCY`, `VALID_FROM`.

A flat Create accepts **one** `ExchangeRate` per call. **Mass upload is done with OData
`$batch`** (many creates in one HTTP call) — see Section 6.

### Fields (OData property = XSD element = BAPI1093_0 field, 1:1)

| # | Field | Type | Len | Mandatory | Notes |
|---|-------|------|-----|:---------:|-------|
| 1 | RATE_TYPE | Edm.String | 4 | yes (key) | e.g. `M` |
| 2 | FROM_CURR | Edm.String | 5 | yes (key) | source currency |
| 3 | TO_CURRNCY | Edm.String | 5 | yes (key) | target currency |
| 4 | VALID_FROM | Edm.String | 10 | yes (key) | **`DD.MM.YYYY`** -> converted to `YYYYMMDD` |
| 5 | EXCH_RATE | Edm.String | 30 | yes | indirect rate |
| 6 | FROM_FACTOR | Edm.String | 10 | yes | ratio from (>0) |
| 7 | TO_FACTOR | Edm.String | 10 | yes | ratio to (>0) |
| 8 | EXCH_RATE_V | Edm.String | 30 | no | direct rate — **we always clear it** |
| 9 | FROM_FACTOR_V | Edm.String | 10 | no | — |
| 10 | TO_FACTOR_V | Edm.String | 10 | no | — |

BAPI: `BAPI_EXCHRATE_CREATEMULTIPLE`, params `UPD_ALLOW='X'`, table `EXCHRATE_LIST`
(type `BAPI1093_0`), table `RETURN`. **Value fields are DEC9 (packed).**

---

## 3. DPC (Data Provider) — the runtime logic

Class `ZCL_ZF01_EXCHANGE_RATE_DPC_EXT`. Three redefined methods:

### 3a. `EXCHANGERATESSET_CREATE_ENTITY` — one rate per call

Uses a **local structure** (`ty_rate`, all char) so it never depends on the generated
MPC type names. Order of logic:
1. `read_entry_data` -> `ls_rate`.
2. Mandatory-field check (the 7 mandatory).
3. **Config pre-check** — currencies in `TCURC` + translation ratios in `TCURF`
   (either direction). If missing -> raise clean message "Currency/exchange-rate
   settings not maintained for M XXX/YYY" (skips the BAPI). Table check avoids the
   raw BAPI error `E!/015`.
4. Map to `BAPI1093_0` with `CONDENSE` (strip the trailing spaces the source sends).
5. Convert `VALID_FROM` `DD.MM.YYYY` -> `YYYYMMDD`.
6. **Always CLEAR the `_V` block** (never send direct-quote values) + default
   `FROM_FACTOR`/`TO_FACTOR` to 1 if 0.
7. Call BAPI; on error rollback + raise with messages; else commit.
8. Echo `er_entity`.

```abap
METHOD exchangeratesset_create_entity.

  TYPES: BEGIN OF ty_rate,
           rate_type     TYPE c LENGTH 4,
           from_curr     TYPE c LENGTH 5,
           to_currncy    TYPE c LENGTH 5,
           valid_from    TYPE c LENGTH 10,
           exch_rate     TYPE c LENGTH 30,
           from_factor   TYPE c LENGTH 10,
           to_factor     TYPE c LENGTH 10,
           exch_rate_v   TYPE c LENGTH 30,
           from_factor_v TYPE c LENGTH 10,
           to_factor_v   TYPE c LENGTH 10,
         END OF ty_rate.

  DATA: ls_rate   TYPE ty_rate,
        lt_list   TYPE STANDARD TABLE OF bapi1093_0,
        ls_list   TYPE bapi1093_0,
        lt_return TYPE bapiret2_t,
        ls_return TYPE bapiret2,
        lv_valid  TYPE c LENGTH 8,
        lv_msg    TYPE bapi_msg,
        lv_from   TYPE abap_bool,
        lv_to     TYPE abap_bool,
        lv_ratio  TYPE abap_bool,
        lv_n      TYPE string.

  io_data_provider->read_entry_data( IMPORTING es_data = ls_rate ).

  IF ls_rate-rate_type  IS INITIAL OR ls_rate-from_curr   IS INITIAL OR
     ls_rate-to_currncy IS INITIAL OR ls_rate-valid_from  IS INITIAL OR
     ls_rate-exch_rate  IS INITIAL OR ls_rate-from_factor IS INITIAL OR
     ls_rate-to_factor  IS INITIAL.
    lv_msg = |Mandatory field missing for { ls_rate-from_curr }/{ ls_rate-to_currncy } { ls_rate-valid_from }|.
    DATA(lo_mc1) = mo_context->get_message_container( ).
    lo_mc1->add_message_text_only( iv_msg_type = 'E' iv_msg_text = lv_msg ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception EXPORTING message_container = lo_mc1.
  ENDIF.

  " Config pre-check (currencies + translation ratios)
  SELECT SINGLE @abap_true FROM tcurc WHERE waers = @ls_rate-from_curr  INTO @lv_from.
  SELECT SINGLE @abap_true FROM tcurc WHERE waers = @ls_rate-to_currncy INTO @lv_to.
  SELECT SINGLE @abap_true FROM tcurf
    WHERE kurst = @ls_rate-rate_type
      AND ( ( fcurr = @ls_rate-from_curr  AND tcurr = @ls_rate-to_currncy )
         OR ( fcurr = @ls_rate-to_currncy AND tcurr = @ls_rate-from_curr ) )
    INTO @lv_ratio.
  IF lv_from = abap_false OR lv_to = abap_false OR lv_ratio = abap_false.
    lv_msg = |Currency/exchange-rate settings not maintained for { ls_rate-rate_type } { ls_rate-from_curr }/{ ls_rate-to_currncy }|.
    DATA(lo_mc0) = mo_context->get_message_container( ).
    lo_mc0->add_message_text_only( iv_msg_type = 'E' iv_msg_text = lv_msg ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception EXPORTING message_container = lo_mc0.
  ENDIF.

  CLEAR ls_list.
  MOVE-CORRESPONDING ls_rate TO ls_list.

  " strip trailing spaces from the numeric strings
  lv_n = ls_rate-exch_rate.   CONDENSE lv_n NO-GAPS. ls_list-exch_rate   = lv_n.
  lv_n = ls_rate-from_factor. CONDENSE lv_n NO-GAPS. ls_list-from_factor = lv_n.
  lv_n = ls_rate-to_factor.   CONDENSE lv_n NO-GAPS. ls_list-to_factor   = lv_n.

  " VALID_FROM DD.MM.YYYY -> YYYYMMDD
  IF ls_rate-valid_from CA '.'.
    lv_valid = ls_rate-valid_from+6(4) && ls_rate-valid_from+3(2) && ls_rate-valid_from+0(2).
  ELSE.
    lv_valid = ls_rate-valid_from.
  ENDIF.
  ls_list-valid_from = lv_valid.

  " never send direct-quotation (_V); factors must be > 0
  CLEAR: ls_list-exch_rate_v, ls_list-from_factor_v, ls_list-to_factor_v.
  IF ls_list-from_factor IS INITIAL. ls_list-from_factor = 1. ENDIF.
  IF ls_list-to_factor   IS INITIAL. ls_list-to_factor   = 1. ENDIF.

  APPEND ls_list TO lt_list.

  CALL FUNCTION 'BAPI_EXCHRATE_CREATEMULTIPLE'
    EXPORTING  upd_allow     = abap_true
    TABLES     exchrate_list = lt_list
               return        = lt_return.

  READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
  IF sy-subrc <> 0. READ TABLE lt_return INTO ls_return WITH KEY type = 'A'. ENDIF.
  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    DATA(lo_mc2) = mo_context->get_message_container( ).
    lo_mc2->add_messages_from_bapi( it_bapi_messages = lt_return ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception EXPORTING message_container = lo_mc2.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = abap_true.
  ENDIF.

  MOVE-CORRESPONDING ls_rate TO er_entity.

ENDMETHOD.
```

### 3b. `CHANGESET_BEGIN` — REQUIRED for $batch with many records

Without this, a `$batch` changeset with >1 operation fails with
**"Default changeset implementation allows only one operation"**.

```abap
METHOD /iwbep/if_mgw_appl_srv_runtime~changeset_begin.
  " Allow more than one operation per $batch changeset;
  " each op is processed individually by CREATE_ENTITY.
  cv_defer_mode = abap_false.
ENDMETHOD.
```

### 3c. `CREATE_DEEP_ENTITY` (optional / legacy)

Only relevant if the model is switched back to DEEP (header + navigation). It loops the
nested collection into `lt_list` and calls the BAPI once. Not used by the current FLAT
+ `$batch` deployment. Same per-row logic (CONDENSE, date, clear `_V`, factor defaults,
config pre-check) applies inside the loop using `<ls_rate>`.

---

## 4. MPC (Model Provider)

Class `ZCL_ZF01_EXCHANGE_RATE_MPC(_EXT)`. Generated from SEGW. Key points learned:
- Each property MUST have an **Edm Core Type** (`Edm.String`) or generation fails with
  *"Property X must define a Data Type"*.
- `VALID_FROM` MaxLength = **10** (for `DD.MM.YYYY`).
- Entity-set/property **`sap:creatable`** matters: if `false`, CPI hides the fields and
  POST is blocked. In this flat model the sets default to creatable=true (no annotation).
  Historic issue: SEGW generated `creatable="false"`; the fix was to tick Creatable on
  the properties (SEGW) or set it in `MPC_EXT->DEFINE` (guard with `IS BOUND` /`TRY`).
- After any model change: `/IWBEP/CACHE_CLEANUP` + `/IWFND/CACHE_CLEANUP` + **Load
  Metadata**, and bust the browser cache (`$metadata?x=1`).

---

## 5. Registration & cache

- Register: `/IWFND/MAINT_SERVICE` -> Add Service (alias `LOCAL`) -> `ZF01_EXCHANGE_RATE_SRV`.
- Metadata URL: `/sap/opu/odata/sap/ZF01_EXCHANGE_RATE_SRV/$metadata`
- After every change: `/IWBEP/CACHE_CLEANUP` + `/IWFND/CACHE_CLEANUP` + Load Metadata.

---

## 6. CPI integration (mass upload via $batch)

The service is flat (one create per call). To send many rates in ONE HTTP call, CPI uses
OData `$batch`.

**Two payload files exist (do not confuse):**
- `ExchangeRates_mass_sample.xml` — the **source** data into CPI (`<ExchangeRates><ExchangeRate>xN`).
- `ExchangeRates_CPI_batch_sample.xml` — what **CPI sends to SAP** (the `$batch` envelope).

**CPI OData V2 receiver:**
- Entity set `ExchangeRates`, Operation **Create (POST)**, **Batch Processing = ON**, CSRF ON.
- Use **Model Operation -> Generate XML Schema Definition** (Batch ON) to get the EXACT
  batch structure; the entity node name comes from there.

**Batch envelope actually accepted (from the working run):**
```xml
<batchParts>
  <batchChangeSet>
    <batchChangeSetPart>
      <method>POST</method>
      <ExchangeRates>
        <ExchangeRate>
          <RATE_TYPE>M</RATE_TYPE><FROM_CURR>USD</FROM_CURR><TO_CURRNCY>INR</TO_CURRNCY>
          <VALID_FROM>01.04.2025</VALID_FROM><EXCH_RATE>83.25000</EXCH_RATE>
          <FROM_FACTOR>1</FROM_FACTOR><TO_FACTOR>1</TO_FACTOR>
          <EXCH_RATE_V>0</EXCH_RATE_V><FROM_FACTOR_V>0</FROM_FACTOR_V><TO_FACTOR_V>0</TO_FACTOR_V>
        </ExchangeRate>
      </ExchangeRates>
    </batchChangeSetPart>
    <!-- repeat batchChangeSetPart per rate -->
  </batchChangeSet>
</batchParts>
```
- Message Mapping: source repeating `ExchangeRate` -> target repeating `batchChangeSetPart`.
- Result: HTTP 202; each `batchChangeSetPartResponse` = 201 or a per-record message.

**Alternative not deployed:** a splitter (one `<ExchangeRate>` per message + mapping to the
single-entity structure) — kept failing because the splitter wasn't feeding single records
and no mapping to the adapter structure existed. `$batch` is the chosen approach.

---

## 7. SAP config PREREQUISITE (FI / Basis) — not code

Rates only post if the currency pair is configured:
- **Currencies valid** in `TCURC` (OY03). NB: `AZM` (old Azerbaijani Manat) is obsolete ->
  replaced by `AZN` in 2006; unmaintained.
- **Translation ratios** maintained in **`OBBS`** (table `TCURF`) for the rate type +
  currency pair. Missing -> BAPI error `E!/015` "Valid system settings have not been made".
- Rate type exists in `OB07`.
The DPC pre-check (3a) returns a clean "not maintained" message for pairs missing this.

---

## 8. DATA rules for the sending system

1. `VALID_FROM` = **`DD.MM.YYYY`** (e.g. `01.04.2025`).
2. **No trailing/leading spaces** in values (source sent `14.95000 ` -> we CONDENSE, but
   ideally send clean).
3. 7 mandatory fields always; 3 `_V` optional -> send as `0`.
4. **Only the indirect `EXCH_RATE`** is used; never send a direct `EXCH_RATE_V` value
   (sending both caused `E!/033`). We clear `_V` in the DPC regardless.
5. Factors `> 0` (use `1`).
6. Currency pair must be maintained (Section 7).
7. Very old dates (e.g. `01.01.1800`) may be rejected by SAP.

---

## 9. Testing

- `/IWFND/GW_CLIENT`: single POST to `.../ZF01_EXCHANGE_RATE_SRV/ExchangeRates`,
  header `Content-Type: application/json`, body = ONE JSON entity `{ "RATE_TYPE":"M", ... }`.
  (Body must be JSON only — no `POST`/URL lines pasted in.)
- Mass: send the `$batch` envelope (Section 6) or from CPI.
- Verify in **`OB08`** / table `TCURR`.
- Errors: `/IWFND/ERROR_LOG` (hub) and `/IWBEP/ERROR_LOG` (backend), search by timestamp.

---

## 10. Error log we hit -> fix (fastest way to learn this interface)

| Error | Meaning | Fix |
|-------|---------|-----|
| `Method 'CREATE_DEEP_ENTITY' not implemented` | deep POST but method not redefined | redefine it (or use flat + CREATE_ENTITY) |
| `The type "TS_EXCHANGERATE" is unknown` | wrong generated type name | use a **local** `ty_rate` structure (no MPC type dependency) |
| `Property X must define a Data Type` (SEGW) | Edm Core Type blank | set `Edm.String` on every property |
| `could not find function BAPI_EXCHANGERATE_CREATEMULTIPLE` | wrong FM name | use **`BAPI_EXCHRATE_CREATEMULTIPLE`**, param `UPD_ALLOW`, table `EXCHRATE_LIST` |
| `Resource not found for segment` / 405 | wrong URL (service root / wrong set name) | POST to the exact entity set `.../ExchangeRates` |
| `value expected at 'POST /sap/'` | HTTP request line pasted into the body | body must be JSON only, first char `{` |
| `E!/033 EXCH_RATE or EXCH_RATE_V must be > 0` | both indirect+direct sent | **clear the `_V` block**; send only `EXCH_RATE` |
| `E!/015 Valid system settings have not been made` | translation ratios missing | maintain **OBBS** (TCURF); or pre-check returns clean msg |
| `SY/530 Currency/... not maintained` | our pre-check fired | expected; maintain OBBS for that pair |
| `payload must contain a SINGLE valid entity` | flat Create got multiple records | use `$batch` (envelope) or split to single records |
| `Default changeset implementation allows only one operation` | >1 op in a changeset | redefine **`CHANGESET_BEGIN`** -> `cv_defer_mode = abap_false` |

---

## 11. Current status (as of handover)

- OData service (flat) built, registered, cache-clean. Single create works (verified in
  debugger: BAPI committed, rate in TCURR).
- DPC: `CREATE_ENTITY` (with CONDENSE, date convert, clear `_V`, factor defaults, TCURC/TCURF
  pre-check) + `CHANGESET_BEGIN` (`cv_defer_mode = abap_false`) for `$batch`.
- CPI: `$batch` envelope accepted (HTTP 202); after the `CHANGESET_BEGIN` fix each part
  runs `CREATE_ENTITY`.
- Remaining = **config**: FI must maintain OBBS translation ratios for every currency pair
  the source sends; unmaintained pairs return the "not maintained" message per record.

## 12. Repo reference files

- `ZF01_EXCHANGE_RATE_API_SPEC.md` / `.docx` — customer interface spec.
- `ExchangeRates_mass_sample.xml` — source data sample.
- `ExchangeRates_CPI_batch_sample.xml` — `$batch` envelope sample.
- `ZCL_ZF01_EXCH_RATE_*.abap` / `ZCL_YF01_*` — code-based reference classes.
- This file — full handover.
