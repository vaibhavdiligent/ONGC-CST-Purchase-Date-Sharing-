# ZTABLE_META – Dynamic Table Structure + Data OData V2 Service (SAP → CPI)

A **generic** OData V2 service. You give it a table name (VBAP, EKPO, MARA, …
any transparent table or DDIC view) and it returns:

1. **The structure** of that table – one entity per field with the full DDIC
   characteristics (field name, data element, domain, type, length, decimals,
   key flag, check table, description …).
2. **The data** of that table – one entity per record, the whole record
   serialised to JSON.

The OData **model is fixed** (two entity types); only the **content is dynamic**,
so one service covers every table – no new development per table.

Fully **code based**: a Model Provider class extending `/IWBEP/CL_MGW_ABS_MODEL`
and a Data Provider class extending `/IWBEP/CL_MGW_ABS_DATA` (the standard
code-based bases SEGW-generated MPC/DPC use). **No SEGW project required.**

| Object | Name |
|--------|------|
| Model Provider Class (MPC) | `ZCL_ZTABLE_META_MPC` |
| Data Provider Class (DPC)  | `ZCL_ZTABLE_META_DPC` |
| Technical model name       | `ZTABLE_META_MDL` (version 0001) |
| Technical service name     | `ZTABLE_META_SRV` |
| Package                    | `ZGMS` (or your Z package) + transport |

---

## Entity model

### `TableStructureSet` (entity type `TableFieldInfo`)
Key = `Tabname` + `Fieldname`. One entity per field.

| Property | Edm type | Source (DFIES) | Meaning |
|----------|----------|----------------|---------|
| Tabname    | String (key) | TABNAME    | table / view name |
| Fieldname  | String (key) | FIELDNAME  | field name |
| Position   | Int32        | POSITION   | position in the structure |
| Keyflag    | String(1)    | KEYFLAG    | `X` = key field |
| Rollname   | String       | ROLLNAME   | **data element** |
| Domname    | String       | DOMNAME    | domain |
| Datatype   | String(4)    | DATATYPE   | DDIC type (CHAR/NUMC/DEC/DATS…) |
| Leng       | Int32        | LENG       | DDIC length |
| Decimals   | Int32        | DECIMALS   | decimal places |
| Inttype    | String(1)    | INTTYPE    | ABAP internal type (C/N/P/D/T/X…) |
| Intlen     | Int32        | INTLEN     | internal length (bytes) |
| Lowercase  | String(1)    | LOWERCASE  | `X` = lower case allowed |
| Signflag   | String(1)    | SIGN       | `X` = value can be negative |
| Checktable | String       | CHECKTABLE | foreign-key check table |
| Reftable   | String       | REFTABLE   | reference table (CURR/QUAN) |
| Reffield   | String       | REFFIELD   | reference field |
| Convexit   | String(5)    | CONVEXIT   | conversion exit |
| Fieldtext  | String(60)   | FIELDTEXT  | short field text |
| Scrtext_l  | String(40)   | SCRTEXT_L  | long field label |

### `TableDataSet` (entity type `TableRow`)
Key = `Tabname` + `RowNo`. One entity per record.

| Property | Edm type | Meaning |
|----------|----------|---------|
| Tabname  | String (key) | table / view name |
| RowNo    | Int32 (key)  | 1-based record number |
| DataJson | String       | the whole record serialised to JSON |

> **Why JSON for the data?** OData entity types are statically typed, but the
> table is chosen at runtime. Returning the record as a JSON string is what keeps
> the service truly generic across VBAP / EKPO / MARA without changing the model.
> In CPI you parse `DataJson` (JSON → XML converter, or a Groovy/JSON-to-XML step)
> to get the real columns of whatever table was requested.

---

## How the client calls it

The table name is **mandatory** and is passed as a `$filter` on `Tabname`.

Structure of MARA:
```
GET /sap/opu/odata/sap/ZTABLE_META_SRV/TableStructureSet?$filter=Tabname eq 'MARA'&$format=json
```

Data of MARA (first 100 rows):
```
GET /sap/opu/odata/sap/ZTABLE_META_SRV/TableDataSet?$filter=Tabname eq 'MARA'&$top=100&$format=json
```

Paging is supported with `$top` / `$skip`. If `$top` is omitted on
`TableDataSet` a safe default cap (`GC_DEFAULT_MAX_ROWS = 1000`) is applied so a
client cannot dump an entire large table by accident. An absolute ceiling
(`GC_HARD_MAX_ROWS = 50000`) is enforced regardless of `$top`. Raise/lower both
in the DPC.

**Optional row restriction** – pass a raw Open SQL condition in a `WhereClause`
filter property (string literals single-quoted, quotes doubled):
```
GET /sap/opu/odata/sap/ZTABLE_META_SRV/TableDataSet?$filter=Tabname eq 'EKPO' and WhereClause eq 'EBELN = ''4500000001'''&$format=json
```
This is display-only and the table is still protected by the `S_TABU_NAM` check,
but treat `WhereClause` as a trusted-caller feature (potential SQL surface); omit
it or add a whitelist if untrusted clients can call the service.

Example `TableDataSet` response (one row of MARA):
```json
{ "d": { "results": [
  { "Tabname": "MARA", "RowNo": 1,
    "DataJson": "{\"MANDT\":\"100\",\"MATNR\":\"000000000000000023\",\"MTART\":\"FERT\",...}" }
] } }
```

---

## Build steps

### 1. Create the two classes
Create `ZCL_ZTABLE_META_MPC` and `ZCL_ZTABLE_META_DPC` in SE24 (or ADT) and paste
the source from:
- `ZCL_ZTABLE_META_MPC.abap`
- `ZCL_ZTABLE_META_DPC.abap`

Activate both. (`DDIF_FIELDINFO_GET`, `/ui2/cl_json`, `DD02L`/`DD25L` are all
standard – no extra dependencies.)

### 2. Register the service — `/IWFND/MAINT_SERVICE`
- **Add Service** → System Alias `LOCAL` → Technical Service Name filter
  `ZTABLE_META_SRV`.
- If it is **not** found (code-based services are not auto-discovered), register
  the model first in **`/IWBEP/REG_SERVICE`** (SAP Gateway Service Builder →
  *Register* on the back-end):
  - Technical Model Name  : `ZTABLE_META_MDL`, Version `0001`,
    Model Provider Class `ZCL_ZTABLE_META_MPC`
  - Technical Service Name : `ZTABLE_META_SRV`, Version `0001`,
    Data Provider Class `ZCL_ZTABLE_META_DPC`
  - Then in `/IWFND/MAINT_SERVICE` **Add Service** and pick `ZTABLE_META_SRV`,
    assign package / Local Object.

### 3. Clear cache + load metadata
- `/IWBEP/CACHE_CLEANUP` and `/IWFND/CACHE_CLEANUP`
- `/IWFND/MAINT_SERVICE` → select the service → **Load Metadata**
- Browser check:
  `/sap/opu/odata/sap/ZTABLE_META_SRV/$metadata?x=1`
  → two entity sets `TableStructureSet`, `TableDataSet`.

### 4. Test in Gateway Client — `/IWFND/GW_CLIENT`
- GET `/sap/opu/odata/sap/ZTABLE_META_SRV/TableStructureSet?$filter=Tabname eq 'MARA'&$format=json`
- GET `/sap/opu/odata/sap/ZTABLE_META_SRV/TableDataSet?$filter=Tabname eq 'MARA'&$top=10&$format=json`

---

## CPI configuration (OData V2 sender/receiver adapter)

Use an **OData V2 receiver** channel (SAP calls SAP-Gateway; CPI is the caller):

1. **Connection**
   - Address: `https://<host>:<port>/sap/opu/odata/sap/ZTABLE_META_SRV`
   - Authentication: Basic / OAuth as per landscape.
2. **Processing – get the structure**
   - Operation **Query (GET)**, Resource Path / Entity Set `TableStructureSet`
   - Query Options: `$filter=Tabname eq '{table}'` (set `{table}` from your input)
   - Select all fields.
3. **Processing – get the data**
   - Second call: Operation **Query (GET)**, Entity Set `TableDataSet`
   - Query Options: `$filter=Tabname eq '{table}'&$top=1000`
   - After the call, add a **JSON to XML Converter** (or Groovy) on `DataJson`
     of each row to expand the record into real columns.
4. Drive the table name from your iFlow input (Content Modifier / property
   `table`), so the same flow works for VBAP, EKPO, MARA, … by changing one value.

---

## Security

- The DPC does `AUTHORITY-CHECK OBJECT 'S_TABU_NAM'` (activity `03` = display)
  before reading, so the CPI technical user must be authorised for the tables it
  reads. Add the required `S_TABU_NAM` values (or the relevant `S_TABU_DIS`
  authorisation groups) to that user's role.
- Consider restricting which tables are allowed (e.g. a whitelist Z-table) if the
  service must not expose arbitrary tables. A whitelist check would go in
  `ZCL_ZTABLE_META_DPC->get_entityset` right after `check_table_authority`.
- The default row cap (`GC_DEFAULT_MAX_ROWS`) protects against accidental full
  dumps; keep it sensible for your volumes.

---

## Checklist
| # | Where | Action |
|---|-------|--------|
| 1 | SE24/ADT | create + activate `ZCL_ZTABLE_META_MPC`, `ZCL_ZTABLE_META_DPC` |
| 2 | /IWBEP/REG_SERVICE | register model `ZTABLE_META_MDL` + service `ZTABLE_META_SRV` (if not auto-found) |
| 3 | /IWFND/MAINT_SERVICE | Add Service `ZTABLE_META_SRV` |
| 4 | cache | `/IWBEP/` + `/IWFND/CACHE_CLEANUP`, Load Metadata, verify `$metadata?x=1` |
| 5 | /IWFND/GW_CLIENT | GET TableStructureSet + TableDataSet with `$filter=Tabname eq 'MARA'` |
| 6 | Role | grant `S_TABU_NAM` (display) for the CPI user on the needed tables |
| 7 | CPI | two GET calls (structure + data), JSON→XML on `DataJson`, drive `Tabname` from input |
