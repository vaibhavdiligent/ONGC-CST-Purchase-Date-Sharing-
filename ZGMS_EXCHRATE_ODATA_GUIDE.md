# ZGMS Exchange Rate – Inbound OData V2 Service (CPI → SAP)

Inbound OData V2 service so **SAP CPI posts exchange rates into SAP**. CPI
sends the `ExchangeRates` → `ExchangeRate*` payload (per the agreed XSD); the
service maps it to a deep OData entity and writes the rates to **TCURR** via
`BAPI_EXCHANGERATE_CREATEMULTIPLE` in a single all-or-nothing call.

---

## 1. Artifacts

| Object | Type | Purpose |
|--------|------|---------|
| `ZCL_GMS_EXCHRATE_MPC` | Class (model) | Code-based model: `ExchangeRates` (header) + `ExchangeRate` (item) + navigation. |
| `ZCL_GMS_EXCHRATE_DPC` | Class (data) | Runtime. `CREATE_DEEP_ENTITY` maps the batch → `BAPI1093_0` and calls the BAPI. |
| `ZGMS_EXCHRATE_SRV` | Service | External (registered) service name. |

It is **fully code-based** – no SEGW project required. Section 2 below is the
recommended path. Section 6 gives the SEGW alternative if you prefer the
graphical modeler.

---

## 2. Create the service (code-based – recommended)

### Step 1 – Create the Model Provider class
1. SE24 → create `ZCL_GMS_EXCHRATE_MPC`.
2. Superclass: `/IWBEP/CL_MGW_PUSH_ABS_MODEL`.
3. Paste the source from `ZCL_GMS_EXCHRATE_MPC.abap`, activate.
   - It redefines `DEFINE` (builds the EDM model) and `GET_MODEL`
     (model name/version).

### Step 2 – Create the Data Provider class
1. SE24 → create `ZCL_GMS_EXCHRATE_DPC`.
2. Superclass: `/IWBEP/CL_MGW_PUSH_ABS_DATA`.
3. Paste the source from `ZCL_GMS_EXCHRATE_DPC.abap`, activate.
   - It redefines `CREATE_DEEP_ENTITY` (the deep insert handler).

### Step 3 – Register & publish the service
1. `/IWFND/MAINT_SERVICE` (SAP Gateway hub / embedded).
2. **Add Service** → System Alias `LOCAL` (embedded) → search your service,
   or use **Service Registration** if it is not auto-discovered.
   - Technical Service Name: `ZGMS_EXCHRATE_SRV`
   - External Service Name:  `ZGMS_EXCHRATE_SRV`
   - Model Provider Class:   `ZCL_GMS_EXCHRATE_MPC`
   - Data Provider Class:    `ZCL_GMS_EXCHRATE_DPC`
3. Assign a package/transport, then **Activate**.

> If MPC/DPC are not offered automatically in MAINT_SERVICE, first register
> the model & service in `/IWBEP/REG_SERVICE` (Service Builder backend
> registration): create a Technical Model `ZGMS_EXCHRATE_MDL` v1 → MPC class,
> and Technical Service `ZGMS_EXCHRATE_SRV` v1 → DPC class. Then add it in
> `/IWFND/MAINT_SERVICE`.

### Step 4 – Verify
```
GET /sap/opu/odata/sap/ZGMS_EXCHRATE_SRV/$metadata
```
You should see `ExchangeRatesSet`, `ExchangeRateSet`, and the navigation
property `ExchangeRate`.

---

## 3. Field & BAPI mapping

OData `ExchangeRate` property names = XSD element names = `BAPI1093_0` field
names (1:1), so the DPC fills the BAPI table with `MOVE-CORRESPONDING`.

| OData / XSD field | BAPI1093_0 | Req. | Notes |
|-------------------|------------|------|-------|
| `RATE_TYPE`   | RATE_TYPE   | ✔ key | e.g. `M` |
| `FROM_CURR`   | FROM_CURR   | ✔ key | source currency |
| `TO_CURRNCY`  | TO_CURRNCY  | ✔ key | target currency |
| `VALID_FROM`  | VALID_FROM  | ✔ key | **format `YYYYMMDD`** |
| `EXCH_RATE`   | EXCH_RATE   | ✔ | indirect-quote rate |
| `FROM_FACTOR` | FROM_FACTOR | ✔ | from ratio |
| `TO_FACTOR`   | TO_FACTOR   | ✔ | to ratio |
| `EXCH_RATE_V` | EXCH_RATE_V | – | direct-quote rate |
| `FROM_FACTOR_V` | FROM_FACTOR_V | – | direct from ratio |
| `TO_FACTOR_V` | TO_FACTOR_V | – | direct to ratio |

BAPI call options used in the DPC:
- `UPD_ALLOWED = 'X'` → existing TCURR entries for the same key are updated
  (not rejected as duplicates).
- Whole batch is passed in one `EXCH_RATE_LIST` call, then
  `BAPI_TRANSACTION_COMMIT`/`ROLLBACK` once → **all-or-nothing**.

---

## 4. How CPI calls it (deep insert)

```
POST /sap/opu/odata/sap/ZGMS_EXCHRATE_SRV/ExchangeRatesSet
Content-Type: application/json
X-CSRF-Token: <token>          # see CSRF note below

{
  "REQUEST_ID": "",
  "ExchangeRate": [
    { "RATE_TYPE":"M","FROM_CURR":"USD","TO_CURRNCY":"INR",
      "VALID_FROM":"20260623","EXCH_RATE":"83.2500",
      "FROM_FACTOR":"1","TO_FACTOR":"1" },
    { "RATE_TYPE":"M","FROM_CURR":"EUR","TO_CURRNCY":"INR",
      "VALID_FROM":"20260623","EXCH_RATE":"90.1000",
      "FROM_FACTOR":"1","TO_FACTOR":"1" }
  ]
}
```

CPI configuration notes:
- **CSRF**: OData V2 writes need a token. In the CPI OData/HTTP receiver
  enable **CSRF protection** (it does the `GET ...?$top=0` with
  `X-CSRF-Token: Fetch` and reuses the token+cookies on the POST), or do it
  manually with two calls on the same session.
- The nested array property is **`ExchangeRate`** (the navigation/child name),
  matching the XSD child element.
- `REQUEST_ID` may be sent empty; the service generates one and echoes it back.
- Response on success: the header entity with the filled `REQUEST_ID`.
- Response on failure: HTTP 4xx/5xx with the BAPI messages in the OData error
  body; nothing is committed.

---

## 5. Test before wiring CPI
- **Gateway Client** `/IWFND/GW_CLIENT`: method POST, URI
  `/sap/opu/odata/sap/ZGMS_EXCHRATE_SRV/ExchangeRatesSet`, paste the JSON above
  (the client handles CSRF for you). Confirm rows appear in **TCURR**
  (transaction `OB08`).

---

## 6. SEGW alternative (graphical modeler)

If you prefer SEGW instead of the code-based model:
1. SEGW → create project `ZGMS_EXCHRATE`.
2. Import the XSD or create entity types `ExchangeRates` (key `REQUEST_ID`) and
   `ExchangeRate` (keys `RATE_TYPE/FROM_CURR/TO_CURRNCY/VALID_FROM`, plus the
   value + optional `_V` properties, all `Edm.String`).
3. Create an Association `ExchangeRates → ExchangeRate` (1 : 0..n) and the
   navigation property `ExchangeRate`.
4. Generate runtime objects → SEGW creates `..._MPC/_MPC_EXT/_DPC/_DPC_EXT`.
5. In `..._DPC_EXT` redefine `CREATE_DEEP_ENTITY` and paste the body from
   `ZCL_GMS_EXCHRATE_DPC.abap` (adjust the deep-structure type name to the
   SEGW-generated one).
6. Register/activate via `/IWFND/MAINT_SERVICE` as in Step 3.

The code-based classes in this repo already do everything SEGW would
generate, so Section 2 is the faster route.
