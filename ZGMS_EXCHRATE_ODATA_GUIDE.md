# ZGMS Exchange Rate – Inbound OData V2 Service (CPI → SAP)

Inbound OData V2 service so **SAP CPI posts exchange rates into SAP**. CPI
sends the headerless `ExchangeRates` → `ExchangeRate*` payload (per the agreed
XSD); the service exposes a **flat `ExchangeRate` entity** and writes each row
to **TCURR** via `BAPI_EXCHANGERATE_CREATEMULTIPLE`.

> Design: **flat** – no header entity, no `REQUEST_ID`. This matches the
> headerless XSD 1:1. CPI sends all rows in one `$batch` POST; the framework
> runs `CREATE_ENTITY` once per row.

---

## 1. Artifacts

| Object | Type | Purpose |
|--------|------|---------|
| `ZCL_GMS_EXCHRATE_MPC` (SEGW: `ZCL_ZGMS_EXCHRATE_MPC`) | Class (model) | Flat model: entity `ExchangeRate` / set `ExchangeRateSet`. |
| `ZCL_GMS_EXCHRATE_DPC` (SEGW: `ZCL_ZGMS_EXCHRATE_DPC_EXT`) | Class (data) | `CREATE_ENTITY` maps one row → `BAPI1093_0` and calls the BAPI. |
| `ZGMS_EXCHRATE_SRV` | Service | External (registered) service name. |

The `.abap` files in this repo are the **code-based** reference (no SEGW
project needed). If you build in SEGW instead, use Section 6 – the model is
identical; only the class names differ (`ZCL_ZGMS_EXCHRATE_*`).

---

## 2. Field & BAPI mapping

OData `ExchangeRate` property names = XSD element names = `BAPI1093_0` field
names (1:1), so the DPC fills the BAPI table with `MOVE-CORRESPONDING`.

| OData / XSD field | BAPI1093_0 | Req. | Notes |
|-------------------|------------|------|-------|
| `RATE_TYPE`   | RATE_TYPE   | ✔ key | e.g. `M` |
| `FROM_CURR`   | FROM_CURR   | ✔ key | source currency |
| `TO_CURRNCY`  | TO_CURRNCY  | ✔ key | target currency |
| `VALID_FROM`  | VALID_FROM  | ✔ key | **arrives `DD.MM.YYYY`, converted to `YYYYMMDD`** – model MaxLength **10** |
| `EXCH_RATE`   | EXCH_RATE   | ✔ | indirect-quote rate |
| `FROM_FACTOR` | FROM_FACTOR | ✔ | from ratio |
| `TO_FACTOR`   | TO_FACTOR   | ✔ | to ratio |
| `EXCH_RATE_V` | EXCH_RATE_V | – | direct-quote rate |
| `FROM_FACTOR_V` | FROM_FACTOR_V | – | direct from ratio |
| `TO_FACTOR_V` | TO_FACTOR_V | – | direct to ratio |

BAPI options: `UPD_ALLOWED = 'X'` (existing TCURR entries are updated, not
rejected). The DPC commits/rolls back per row inside the `$batch` changeset.

---

## 3. Create the service (code-based)

1. SE24 → `ZCL_GMS_EXCHRATE_MPC`, superclass `/IWBEP/CL_MGW_PUSH_ABS_MODEL`,
   paste `ZCL_GMS_EXCHRATE_MPC.abap`, activate.
2. SE24 → `ZCL_GMS_EXCHRATE_DPC`, superclass `/IWBEP/CL_MGW_PUSH_ABS_DATA`,
   paste `ZCL_GMS_EXCHRATE_DPC.abap`, activate.
3. `/IWFND/MAINT_SERVICE` → Add Service (alias `LOCAL`), MPC =
   `ZCL_GMS_EXCHRATE_MPC`, DPC = `ZCL_GMS_EXCHRATE_DPC`, external name
   `ZGMS_EXCHRATE_SRV`, activate.
4. Verify: `GET /sap/opu/odata/sap/ZGMS_EXCHRATE_SRV/$metadata` – you should
   see `ExchangeRateSet` (flat, no navigation).

---

## 4. How CPI calls it

CPI **OData V2 receiver** channel:
- Address: `/sap/opu/odata/sap/ZGMS_EXCHRATE_SRV`
- Resource Path / Entity Set: **`ExchangeRateSet`**
- Operation: **Create (POST)**  ← must be POST, not Query(GET)
- **Enable Batch Processing** → all rows go in one `$batch` HTTP call; the
  service runs `CREATE_ENTITY` once per row.
- CSRF: enable CSRF protection in the channel (it fetches the token then POSTs).
- Mapping: flat 1:1, source `ExchangeRate` → `ExchangeRateSet`. **No
  REQUEST_ID, no nesting.**

Single-entry POST body:
```json
{ "RATE_TYPE":"M","FROM_CURR":"INR","TO_CURRNCY":"BRL","VALID_FROM":"01.04.2025",
  "EXCH_RATE":"1.209","FROM_FACTOR":"1","TO_FACTOR":"1",
  "EXCH_RATE_V":"0","FROM_FACTOR_V":"0","TO_FACTOR_V":"0" }
```

Response: the created entity is echoed back. Errors return HTTP 4xx/5xx with
the BAPI messages in the OData error body (that row rolls back).

---

## 5. Test before wiring CPI
- `/IWFND/GW_CLIENT`: POST to
  `/sap/opu/odata/sap/ZGMS_EXCHRATE_SRV/ExchangeRateSet` with the JSON above
  (the client handles CSRF). Confirm the rate in **TCURR** / transaction
  `OB08`.

---

## 6. SEGW step-by-step (graphical modeler)

### 6.1 Create the project
1. Transaction **`SEGW`** → **Create Project** `ZGMS_EXCHRATE`, assign
   package/transport.

### 6.2 Create the entity type `ExchangeRate`
1. Right-click **Data Model → Create → Entity Type**, name `ExchangeRate`,
   tick **Create Related Entity Set** → `ExchangeRateSet`.
2. Add the properties, all **Edm.String** — set the **Edm Core Type** on every
   property (leaving it blank causes *"Property X must define a Data Type"* at
   generation):

   | Property | Is Key | Nullable | MaxLength |
   |----------|:------:|:--------:|:---------:|
   | `RATE_TYPE`     | ✔ | – | 4 |
   | `FROM_CURR`     | ✔ | – | 5 |
   | `TO_CURRNCY`    | ✔ | – | 5 |
   | `VALID_FROM`    | ✔ | – | **10** |
   | `EXCH_RATE`     | – | – | 30 |
   | `FROM_FACTOR`   | – | – | 10 |
   | `TO_FACTOR`     | – | – | 10 |
   | `EXCH_RATE_V`   | – | ✔ | 30 |
   | `FROM_FACTOR_V` | – | ✔ | 10 |
   | `TO_FACTOR_V`   | – | ✔ | 10 |

   > `VALID_FROM` is length **10** to hold `DD.MM.YYYY`. No header entity,
   > no association, no `REQUEST_ID` — the flat entity is all you need.

### 6.3 Generate runtime objects
Click **Generate Runtime Objects** → SEGW creates
`ZCL_ZGMS_EXCHRATE_MPC/_MPC_EXT/_DPC/_DPC_EXT`.

### 6.4 Implement `CREATE_ENTITY`
1. SEGW → **Service Implementation** → `ExchangeRateSet` → right-click
   **Create → Go to ABAP Workbench** (or open `ZCL_ZGMS_EXCHRATE_DPC_EXT` in
   SE24 and redefine `/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY`).
2. Paste the method body below (SEGW type name is `ts_exchangerate`):

```abap
METHOD /iwbep/if_mgw_appl_srv_runtime~create_entity.

  DATA: ls_rate   TYPE zcl_zgms_exchrate_mpc=>ts_exchangerate,
        lt_list   TYPE STANDARD TABLE OF bapi1093_0,
        ls_list   TYPE bapi1093_0,
        lt_return TYPE bapiret2_t,
        ls_return TYPE bapiret2,
        lv_valid  TYPE c LENGTH 8.

  io_data_provider->read_entry_data( IMPORTING es_data = ls_rate ).

  IF ls_rate-rate_type  IS INITIAL OR ls_rate-from_curr   IS INITIAL OR
     ls_rate-to_currncy IS INITIAL OR ls_rate-valid_from  IS INITIAL OR
     ls_rate-exch_rate  IS INITIAL OR ls_rate-from_factor IS INITIAL OR
     ls_rate-to_factor  IS INITIAL.
    DATA(lv_msg1) = |Mandatory field missing for { ls_rate-from_curr }/{ ls_rate-to_currncy } { ls_rate-valid_from }|.
    DATA(lo_mc1)  = mo_context->get_message_container( ).
    lo_mc1->add_message_text_only( iv_msg_type = 'E' iv_msg_text = lv_msg1 ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING message_container = lo_mc1.
  ENDIF.

  MOVE-CORRESPONDING ls_rate TO ls_list.
  IF ls_rate-valid_from CA '.'.
    lv_valid = ls_rate-valid_from+6(4) && ls_rate-valid_from+3(2) && ls_rate-valid_from+0(2).
  ELSE.
    lv_valid = ls_rate-valid_from.
  ENDIF.
  ls_list-valid_from = lv_valid.
  APPEND ls_list TO lt_list.

  CALL FUNCTION 'BAPI_EXCHANGERATE_CREATEMULTIPLE'
    EXPORTING
      upd_allowed    = abap_true
    TABLES
      exch_rate_list = lt_list
      return         = lt_return.

  READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
  IF sy-subrc <> 0.
    READ TABLE lt_return INTO ls_return WITH KEY type = 'A'.
  ENDIF.
  IF sy-subrc = 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    DATA(lo_mc2) = mo_context->get_message_container( ).
    lo_mc2->add_messages_from_bapi( it_bapi_messages = lt_return ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING message_container = lo_mc2.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = abap_true.
  ENDIF.

  copy_data_to_ref( EXPORTING is_data = ls_rate CHANGING cr_data = er_entity ).

ENDMETHOD.
```

### 6.5 Register & activate
`/IWFND/MAINT_SERVICE` → Add Service → `ZGMS_EXCHRATE_SRV` → activate. Test per
Sections 4–5.

---

## Notes
- **VALID_FROM**: API sends `DD.MM.YYYY` (e.g. `01.04.2025`); the DPC converts
  to `YYYYMMDD` for the BAPI. Model MaxLength must be **10**, else the date is
  truncated.
- **Batch atomicity**: with `$batch`, each row commits independently. If you
  need strict all-or-nothing across the whole file, move the BAPI call/commit
  into `CHANGESET_END` (deferred processing) instead of per-row `CREATE_ENTITY`
  — ask and this can be added.
