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

## 6. SEGW step-by-step (graphical modeler)

Use this if you want to build the service with the Gateway Service Builder
instead of the code-based classes in Section 2. The end result is the same
model; you only hand-code the `CREATE_DEEP_ENTITY` body.

### 6.1 Create the project
1. Transaction **`SEGW`**.
2. **Create Project** (the white-page icon).
   - Project: `ZGMS_EXCHRATE`
   - Description: `Inbound Exchange Rate upload from CPI`
   - Type: `Service with SAP Annotations` (default)
   - Assign package (e.g. `ZGMS`) + transport.
   - The tree appears: **Data Model / Service Implementation / Runtime
     Artifacts / Service Maintenance**.

### 6.2 Create the ITEM entity type `ExchangeRate`
1. Right-click **Data Model → Create → Entity Type**.
   - Name: `ExchangeRate`
   - Tick **Create Related Entity Set** → set name `ExchangeRateSet`.
2. Expand `ExchangeRate` → right-click **Properties → Create** (or use
   "Create" repeatedly) and add these, all **Edm.String**:

   | Property | Is Key | Nullable | MaxLength |
   |----------|:------:|:--------:|:---------:|
   | `RATE_TYPE`     | ✔ | – | 4 |
   | `FROM_CURR`     | ✔ | – | 5 |
   | `TO_CURRNCY`    | ✔ | – | 5 |
   | `VALID_FROM`    | ✔ | – | 8 |
   | `EXCH_RATE`     | – | – | 30 |
   | `FROM_FACTOR`   | – | – | 10 |
   | `TO_FACTOR`     | – | – | 10 |
   | `EXCH_RATE_V`   | – | ✔ | 30 |
   | `FROM_FACTOR_V` | – | ✔ | 10 |
   | `TO_FACTOR_V`   | – | ✔ | 10 |

   (For each property double-click it → set Edm Core Type = `Edm.String`,
   Maxlength, and the **Key**/**Nullable** flags. Uncheck *Nullable* on the
   seven mandatory fields.)

   > Tip: instead of typing each property, you can **import the XSD** —
   > right-click `ExchangeRate` → *Import → Data Model from File* and pick the
   > XSD. Review the generated properties against the table above afterwards.

### 6.3 Create the HEADER entity type `ExchangeRates`
1. Right-click **Data Model → Create → Entity Type**.
   - Name: `ExchangeRates`
   - Tick **Create Related Entity Set** → `ExchangeRatesSet`.
2. Add one property: `REQUEST_ID` — `Edm.String`, **Key = ✔**, MaxLength 32,
   Nullable ✔ (the server can generate it).

### 6.4 Create the association + navigation
1. Right-click **Data Model → Create → Association**.
   - Association Name: `ExchangeRates_ExchangeRate`
   - Principal Entity: `ExchangeRates`, Cardinality **1**
   - Dependent Entity: `ExchangeRate`, Cardinality **0..n** (`*`)
   - Navigation Property Name (created on `ExchangeRates`): **`ExchangeRate`**
     ← this name must match the JSON array key CPI sends.
2. Finish the wizard. On the "referential constraint" step you can leave it
   empty (the header `REQUEST_ID` is not a foreign key in the items) — just
   confirm/continue. SEGW also auto-creates the Association Set.

### 6.5 Generate runtime objects
1. Click **Generate Runtime Objects** (the red/black "lorry" / generate icon).
2. Accept the proposed class/model names (or rename), assign transport:
   - Model Provider: `ZCL_ZGMS_EXCHRATE_MPC` + `..._MPC_EXT`
   - Data Provider:  `ZCL_ZGMS_EXCHRATE_DPC` + `..._DPC_EXT`
   - Technical Model + Technical Service names.
3. SEGW generates and activates the four classes.

### 6.6 Implement the deep insert
1. In SEGW tree → **Service Implementation** → expand `ExchangeRatesSet` →
   right-click **CreateDeepEntity → Go to ABAP Workbench** (opens the
   `..._DPC_EXT` class).
   *(If CreateDeepEntity is not listed, open `..._DPC_EXT` in SE24, →
   Redefine the method `/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY`.)*
2. Paste the method body from `ZCL_GMS_EXCHRATE_DPC.abap`, with these
   adjustments to the SEGW-generated names:
   - The deep structure: SEGW generates a structure for the deep type. Either
     declare your own local types (as in the repo class) **or** use the
     generated `..._MPC=>TS_*` / deep type. Simplest: copy the
     `TY_EXCHANGE_RATE` / `TY_DEEP` `TYPES` from `ZCL_GMS_EXCHRATE_MPC.abap`
     into your `..._DPC_EXT` (or a type pool) and keep the body unchanged.
   - Keep the BAPI call, validation, commit/rollback exactly as provided.
3. Activate the class.

> Why a deep structure is still needed: SEGW models the navigation, but the
> `read_entry_data( )` target must be a flat ABAP structure containing the
> header fields **and** an internal table named after the navigation property
> (`EXCHANGERATE`). The `TY_DEEP` type in the repo already matches this.

### 6.7 Register & activate the service
Same as Section 2, Step 3: **`/IWFND/MAINT_SERVICE`** → Add Service → pick
`ZGMS_EXCHRATE_SRV` (it appears once generated) → activate. Then test per
Sections 4–5.

---

Either path (Section 2 code-based, or Section 6 SEGW) produces the identical
OData V2 service. Section 2 is faster to transport; Section 6 is friendlier if
your team maintains models in the SEGW modeler.
