# YF01_EXCHANGE_RATE — Final Build Steps (flat OData + CPI wrapper)

Goal: CPI receives `<ExchangeRates>` wrapping many `<ExchangeRate>` (your schema.xsd)
and posts the rates into SAP TCURR. **No REQUEST_ID.** The `<ExchangeRates>` wrapper
lives in CPI (as the source message type); SAP exposes a **flat** `ExchangeRate` entity.

```
CPI source (schema.xsd)          CPI mapping         SAP OData (flat)
<ExchangeRates>                                       $batch:
  <ExchangeRate>...</...>   ── repeating map ──►         create ExchangeRateSet (row1)
  <ExchangeRate>...</...>   ─────────────────►           create ExchangeRateSet (row2)
</ExchangeRates>
```

Fields: 7 mandatory (RATE_TYPE, FROM_CURR, TO_CURRNCY, VALID_FROM, EXCH_RATE,
FROM_FACTOR, TO_FACTOR) + 3 optional (EXCH_RATE_V, FROM_FACTOR_V, TO_FACTOR_V).
VALID_FROM arrives DD.MM.YYYY, converted to YYYYMMDD for the BAPI.

=====================================================================
PART A — SAP (SEGW flat service)
=====================================================================

## A1. SEGW project
- `SEGW` → Create Project → `ZF01_EXCHANGE_RATE` → package + transport.

## A2. One entity: ExchangeRate
- Data Model → Create → Entity Type → `ExchangeRate`, tick Create Entity Set → `ExchangeRateSet`.
- Add 10 properties. Each: Edm.String, MaxLength, tick Creatable + Updatable,
  set Nullable per the table:

  | Property | Key | Nullable | Len |
  |----------|:---:|:--------:|:---:|
  | RATE_TYPE     | X | untick | 4  |
  | FROM_CURR     | X | untick | 5  |
  | TO_CURRNCY    | X | untick | 5  |
  | VALID_FROM    | X | untick | 10 |
  | EXCH_RATE     | - | untick | 30 |
  | FROM_FACTOR   | - | untick | 10 |
  | TO_FACTOR     | - | untick | 10 |
  | EXCH_RATE_V   | - | tick   | 30 |
  | FROM_FACTOR_V | - | tick   | 10 |
  | TO_FACTOR_V   | - | tick   | 10 |

  (untick Nullable = mandatory) — NO header, NO association, NO REQUEST_ID.

## A3. Generate Runtime Objects
- Name classes: `ZCL_YF01_EXCH_RATE_MPC/_MPC_EXT/_DPC/_DPC_EXT`, service `YF01_EXCHANGE_RATE_SRV`.

## A4. DPC_EXT → redefine EXCHANGERATESET_CREATE_ENTITY
```abap
METHOD exchangerateset_create_entity.

  DATA: ls_rate   TYPE zcl_yf01_exch_rate_mpc=>ts_exchangerate,
        lt_list   TYPE STANDARD TABLE OF bapi1093_0,
        ls_list   TYPE bapi1093_0,
        lt_return TYPE bapiret2_t,
        ls_return TYPE bapiret2,
        lv_valid  TYPE c LENGTH 8,
        lv_msg    TYPE bapi_msg.

  io_data_provider->read_entry_data( IMPORTING es_data = ls_rate ).

  IF ls_rate-rate_type  IS INITIAL OR ls_rate-from_curr   IS INITIAL OR
     ls_rate-to_currncy IS INITIAL OR ls_rate-valid_from  IS INITIAL OR
     ls_rate-exch_rate  IS INITIAL OR ls_rate-from_factor IS INITIAL OR
     ls_rate-to_factor  IS INITIAL.
    lv_msg = |Mandatory field missing for { ls_rate-from_curr }/{ ls_rate-to_currncy } { ls_rate-valid_from }|.
    DATA(lo_mc1) = mo_context->get_message_container( ).
    lo_mc1->add_message_text_only( iv_msg_type = 'E' iv_msg_text = lv_msg ).
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
    EXPORTING  upd_allowed    = abap_true
    TABLES     exch_rate_list = lt_list
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

  er_entity = ls_rate.

ENDMETHOD.
```
(Match `zcl_yf01_exch_rate_mpc` to the generated MPC name in the base bind_structure.)

## A5. MPC_EXT → redefine DEFINE (force entity set creatable)
```abap
METHOD define.
  super->define( ).
  model->get_entity_set( iv_entity_set_name = 'ExchangeRateSet' )->set_creatable( abap_true ).
ENDMETHOD.
```

## A6. Register + cache
- `/IWFND/MAINT_SERVICE` → Add Service → `YF01_EXCHANGE_RATE_SRV`.
- `/IWBEP/CACHE_CLEANUP` + `/IWFND/CACHE_CLEANUP` + Load Metadata.
- Verify `$metadata?x=1`: one entity set `ExchangeRateSet`, creatable=true,
  7 fields Nullable=false, 3 _V Nullable=true.

## A7. Test
- `/IWFND/GW_CLIENT` → POST `/sap/opu/odata/sap/YF01_EXCHANGE_RATE_SRV/ExchangeRateSet`
  with one JSON row → check OB08.

=====================================================================
PART B — CPI (this is where the <ExchangeRates> wrapper lives)
=====================================================================

## B1. Source message type = your schema.xsd
- Add `schema.xsd` (the `<ExchangeRates>`/`<ExchangeRate>` structure) to the iFlow
  Resources and use it as the inbound message type. THIS provides the
  `<ExchangeRates>` wrapper before each `<ExchangeRate>`.

## B2. OData V2 receiver channel
- Address: `/sap/opu/odata/sap/YF01_EXCHANGE_RATE_SRV`
- Operation: Create (POST)
- Entity Set: `ExchangeRateSet`  (flat), Sub Levels: 0
- Batch Processing: ON  (all rows in one $batch call)
- CSRF: ON

## B3. Message Mapping (source XSD -> OData target)
- Map repeating source `ExchangeRates/ExchangeRate`  ->  target `ExchangeRateSet` (repeating).
- Fields 1:1 (same names): RATE_TYPE, FROM_CURR, TO_CURRNCY, VALID_FROM, EXCH_RATE,
  FROM_FACTOR, TO_FACTOR, EXCH_RATE_V, FROM_FACTOR_V, TO_FACTOR_V.
- Nothing maps to REQUEST_ID (it does not exist).

## B4. Deploy + test end to end.

=====================================================================
Summary of what YOU create
=====================================================================
| # | Where | Object |
|---|-------|--------|
| 1 | SAP SEGW | project `ZF01_EXCHANGE_RATE` |
| 2 | SAP SEGW | entity `ExchangeRate` (+ set `ExchangeRateSet`), 10 props |
| 3 | SAP SEGW | generate → MPC/DPC classes + service `YF01_EXCHANGE_RATE_SRV` |
| 4 | SAP DPC_EXT | redefine `EXCHANGERATESET_CREATE_ENTITY` (BAPI code) |
| 5 | SAP MPC_EXT | redefine `DEFINE` (entity set creatable) |
| 6 | SAP /IWFND/MAINT_SERVICE | register + activate + clear cache |
| 7 | CPI | source message type from schema.xsd (the wrapper) |
| 8 | CPI | OData V2 receiver: Create(POST) ExchangeRateSet, Batch ON |
| 9 | CPI | message mapping XSD ExchangeRate -> ExchangeRateSet |

No REQUEST_ID anywhere. Wrapper = CPI source (schema.xsd). SAP = flat entity.
