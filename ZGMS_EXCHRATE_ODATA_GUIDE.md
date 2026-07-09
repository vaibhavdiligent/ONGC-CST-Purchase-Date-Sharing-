# ZGMS Exchange Rate – Inbound OData V2 Service (CPI → SAP) — DEEP model

Clean build for the inbound Exchange Rate upload service called by CPI.

Consumer payload (one call, parent wraps repeating line items):
```xml
<ExchangeRates>
  <ExchangeRate>
    <RATE_TYPE>M</RATE_TYPE><FROM_CURR>INR</FROM_CURR><TO_CURRNCY>BRL</TO_CURRNCY>
    <VALID_FROM>01.04.2025</VALID_FROM><EXCH_RATE>1.209</EXCH_RATE>
    <FROM_FACTOR>1</FROM_FACTOR><TO_FACTOR>1</TO_FACTOR>
    <EXCH_RATE_V>0</EXCH_RATE_V><FROM_FACTOR_V>0</FROM_FACTOR_V><TO_FACTOR_V>0</TO_FACTOR_V>
  </ExchangeRate>
  <ExchangeRate> ... </ExchangeRate>
</ExchangeRates>
```

Design = **deep entity**: header `ExchangeRates` (parent, technical key `REQUEST_ID`) →
navigation `ExchangeRate` → child `ExchangeRate` (the 10 fields). All rows arrive in
ONE `CREATE_DEEP_ENTITY` call and are written to `TCURR` via
`BAPI_EXCHANGERATE_CREATEMULTIPLE` (all-or-nothing).

Fields (per XSD): mandatory = RATE_TYPE, FROM_CURR, TO_CURRNCY, VALID_FROM,
EXCH_RATE, FROM_FACTOR, TO_FACTOR; optional = EXCH_RATE_V, FROM_FACTOR_V, TO_FACTOR_V.
`VALID_FROM` arrives as `DD.MM.YYYY` and is converted to `YYYYMMDD` for the BAPI.

---

## 0. Clean slate (avoid the `_1_` duplicate class problem)
Before creating the new project, in SE24 delete any leftover classes so the names are free:
`ZCL_ZGMS_EXCHRATE_MPC / _MPC_EXT / _DPC / _DPC_EXT` and any `ZCL_ZGMS_EXCHRATE_1_*`.
Also delete old SEGW projects and the `/IWFND/MAINT_SERVICE` registration if still present.

## 1. SEGW — create the project
- `SEGW` → Create Project → name **`ZGMS_EXCHRATE`**, package `ZGMS` (+ transport).

## 2. Create the CHILD entity type `ExchangeRate`
- Data Model → Create → Entity Type → `ExchangeRate`, tick **Create Related Entity Set** → `ExchangeRateSet`.
- Add 10 properties. For EACH: set **Edm Core Type = Edm.String**, MaxLength, and tick
  **Creatable + Updatable** (so metadata is `sap:creatable="true"` and CPI shows the fields).

  | Property | Key | Nullable | MaxLength |
  |----------|:---:|:--------:|:---------:|
  | RATE_TYPE     | ✔ | – | 4  |
  | FROM_CURR     | ✔ | – | 5  |
  | TO_CURRNCY    | ✔ | – | 5  |
  | VALID_FROM    | ✔ | – | 10 |
  | EXCH_RATE     | – | – | 30 |
  | FROM_FACTOR   | – | – | 10 |
  | TO_FACTOR     | – | – | 10 |
  | EXCH_RATE_V   | – | ✔ | 30 |
  | FROM_FACTOR_V | – | ✔ | 10 |
  | TO_FACTOR_V   | – | ✔ | 10 |

## 3. Create the HEADER (parent) entity type `ExchangeRates`
- Data Model → Create → Entity Type → `ExchangeRates`, tick Create Entity Set → `ExchangeRatesSet`.
- Add ONE property **`REQUEST_ID`** — Edm.String, **Key ✔**, **Nullable ✔**, MaxLength 32,
  Creatable ✔. (Technical key only; CPI leaves it blank, server generates it.)

## 4. Create the association + navigation
- Data Model → Create → Association → `ExchangeRates_ExchangeRate`
  - Principal: `ExchangeRates`, cardinality **1**
  - Dependent: `ExchangeRate`, cardinality **0..n** (`*`)
  - Navigation property (on `ExchangeRates`): **`ExchangeRate`**  ← must be this exact name
  - Referential constraint: leave empty / continue. (Association Set auto-created.)

## 5. Generate Runtime Objects
- Generates `ZCL_ZGMS_EXCHRATE_MPC/_MPC_EXT/_DPC/_DPC_EXT`.

## 6. DPC — redefine `CREATE_DEEP_ENTITY`
`ZCL_ZGMS_EXCHRATE_DPC_EXT` → redefine `/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY`:

```abap
METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.

  TYPES: BEGIN OF ty_deep.
           INCLUDE TYPE zcl_zgms_exchrate_mpc=>ts_exchangerates.
  TYPES:   exchangerate TYPE STANDARD TABLE OF zcl_zgms_exchrate_mpc=>ts_exchangerate
                          WITH DEFAULT KEY.
  TYPES: END OF ty_deep.

  DATA: ls_deep   TYPE ty_deep,
        lt_list   TYPE STANDARD TABLE OF bapi1093_0,
        ls_list   TYPE bapi1093_0,
        lt_return TYPE bapiret2_t,
        ls_return TYPE bapiret2,
        lv_valid  TYPE c LENGTH 8,
        lv_errors TYPE i.

  FIELD-SYMBOLS <ls_rate> TYPE zcl_zgms_exchrate_mpc=>ts_exchangerate.

  io_data_provider->read_entry_data( IMPORTING es_data = ls_deep ).

  LOOP AT ls_deep-exchangerate ASSIGNING <ls_rate>.
    IF <ls_rate>-rate_type  IS INITIAL OR <ls_rate>-from_curr   IS INITIAL OR
       <ls_rate>-to_currncy IS INITIAL OR <ls_rate>-valid_from  IS INITIAL OR
       <ls_rate>-exch_rate  IS INITIAL OR <ls_rate>-from_factor IS INITIAL OR
       <ls_rate>-to_factor  IS INITIAL.
      lv_errors = lv_errors + 1.
      ls_return-type = 'E'. ls_return-id = 'ZGMS'. ls_return-number = '000'.
      ls_return-message = |Mandatory field missing for { <ls_rate>-from_curr }/{ <ls_rate>-to_currncy } { <ls_rate>-valid_from }|.
      APPEND ls_return TO lt_return.
      CONTINUE.
    ENDIF.

    CLEAR ls_list.
    MOVE-CORRESPONDING <ls_rate> TO ls_list.
    IF <ls_rate>-valid_from CA '.'.
      lv_valid = <ls_rate>-valid_from+6(4) && <ls_rate>-valid_from+3(2) && <ls_rate>-valid_from+0(2).
    ELSE.
      lv_valid = <ls_rate>-valid_from.
    ENDIF.
    ls_list-valid_from = lv_valid.
    APPEND ls_list TO lt_list.
  ENDLOOP.

  IF lv_errors = 0 AND lt_list IS NOT INITIAL.
    CALL FUNCTION 'BAPI_EXCHANGERATE_CREATEMULTIPLE'
      EXPORTING  upd_allowed    = abap_true
      TABLES     exch_rate_list = lt_list
                 return         = lt_return.
    LOOP AT lt_return INTO ls_return WHERE type CA 'EAX'.
      lv_errors = lv_errors + 1.
    ENDLOOP.
  ENDIF.

  IF lv_errors > 0.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    DATA(lo_mc) = mo_context->get_message_container( ).
    lo_mc->add_messages_from_bapi( it_bapi_messages = lt_return ).
    RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
      EXPORTING message_container = lo_mc.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = abap_true.
  ENDIF.

  DATA ls_head TYPE zcl_zgms_exchrate_mpc=>ts_exchangerates.
  MOVE-CORRESPONDING ls_deep TO ls_head.
  copy_data_to_ref( EXPORTING is_data = ls_head CHANGING cr_data = er_deep_entity ).

ENDMETHOD.
```
If `TS_EXCHANGERATE` doesn't exist yet, activate the MPC first, or adjust the class
name to your generated MPC (check bind_structure in the base MPC's DEFINE).

## 7. MPC_EXT — force creatable on both entity sets
`ZCL_ZGMS_EXCHRATE_MPC_EXT` → redefine `DEFINE`:
```abap
METHOD define.
  super->define( ).
  model->get_entity_set( iv_entity_set_name = 'ExchangeRatesSet' )->set_creatable( abap_true ).
  model->get_entity_set( iv_entity_set_name = 'ExchangeRateSet'  )->set_creatable( abap_true ).
ENDMETHOD.
```
(Only needed because SEGW hardcodes the entity-set flag to false. Property flags come
from the Creatable checkboxes in step 2.)

## 8. Register the service
- `/IWFND/MAINT_SERVICE` → **Add Service** → alias `LOCAL` → filter `ZGMS_EXCHRATE_SRV`
  → Get Services → select → **Add Selected Services** → package/Local Object → confirm.

## 9. Cache + verify
- `/IWBEP/CACHE_CLEANUP` + `/IWFND/CACHE_CLEANUP`
- `/IWFND/MAINT_SERVICE` → select service → **Load Metadata**
- Browser: `/sap/opu/odata/sap/ZGMS_EXCHRATE_SRV/$metadata?x=1`
  - Two entity sets `ExchangeRatesSet`, `ExchangeRateSet`
  - Navigation `ExchangeRate`
  - Properties `sap:creatable="true"`

## 10. Test in Gateway Client
- `/IWFND/GW_CLIENT` → POST `/sap/opu/odata/sap/ZGMS_EXCHRATE_SRV/ExchangeRatesSet`
  ```json
  { "ExchangeRate": [
      { "RATE_TYPE":"M","FROM_CURR":"INR","TO_CURRNCY":"BRL","VALID_FROM":"01.04.2025",
        "EXCH_RATE":"1.209","FROM_FACTOR":"1","TO_FACTOR":"1",
        "EXCH_RATE_V":"0","FROM_FACTOR_V":"0","TO_FACTOR_V":"0" } ] }
  ```
- Check `OB08` / `TCURR`.

## 11. CPI receiver channel
- Operation **Create (POST)**, Select Entity **`ExchangeRatesSet`**, **Sub Levels = 1**
  (pulls in the nested `ExchangeRate` items → parent `ExchangeRates` + child `ExchangeRate`).
- Enable CSRF. Map source `ExchangeRate` fields → nested child; leave `REQUEST_ID` unmapped.
- POST URL: `/sap/opu/odata/sap/ZGMS_EXCHRATE_SRV/ExchangeRatesSet`

---

## Checklist
| # | Where | Action |
|---|-------|--------|
| 0 | SE24/SEGW | delete old ZCL_ZGMS_EXCHRATE(_1)_* classes, old project, old service |
| 1 | SEGW | create project `ZGMS_EXCHRATE` |
| 2 | SEGW | child `ExchangeRate` (10 props, Edm.String, Creatable+Updatable ticked) |
| 3 | SEGW | header `ExchangeRates` (key `REQUEST_ID`, nullable) |
| 4 | SEGW | association 1:0..n, nav prop `ExchangeRate` |
| 5 | SEGW | Generate Runtime Objects |
| 6 | DPC_EXT | redefine `CREATE_DEEP_ENTITY` (code above) |
| 7 | MPC_EXT | redefine `DEFINE` → set both sets creatable |
| 8 | /IWFND/MAINT_SERVICE | Add Service |
| 9 | cache | `/IWBEP/` + `/IWFND/CACHE_CLEANUP` + Load Metadata, verify `$metadata?x=1` |
| 10 | /IWFND/GW_CLIENT | POST test → check OB08 |
| 11 | CPI | Create(POST), `ExchangeRatesSet`, Sub Levels 1, map, deploy |
