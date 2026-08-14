# ZCCBJI_JCTINVR – Inbound OData V2 Service (CPI → S/4) for /CCBJI/T_JCTINVR

> **ACTUAL SYSTEM NAMES (namespaced deployment):** the productive import goes into
> package `/CCBJI/ODATA_DYNAMIC`, so the objects use the `/CCBJI/` namespace
> (zip `CCBJI_JCTINVR_ODATA_NS_abapGit.zip`):
> classes **`/CCBJI/CL_JCTINVR_MPC`** + **`/CCBJI/CL_JCTINVR_DPC`**,
> model **`/CCBJI/JCTINVR_MDL`**, service **`/CCBJI/JCTINVR_SRV`**,
> URL **`/sap/opu/odata/CCBJI/JCTINVR_SRV/`**. The `Z*` names below document the
> generic template — substitute the namespaced names when executing the steps.

Inbound service for the NTA invoice-registration-number feed (CCBJI /
qualified invoicing business). CPI posts the NTA records; the backend runs
the compare logic that used to live in ABInitio and writes the matched
records back to `/CCBJI/T_JCTINVR`.

**Multiple records per call**: CPI sends ONE deep POST — a technical header
wrapping the repeating line items (OData receiver adapter, Sub Levels = 1).
The whole batch is processed in a single `CREATE_DEEP_ENTITY` call, one LUW,
one commit — all-or-nothing per POST.

Consumer payload (one call, parent wraps repeating items):
```json
POST /sap/opu/odata/sap/ZCCBJI_JCTINVR_SRV/InvoiceRegistrationsSet
{
  "InvoiceRegistration": [
    { "INVOICE_CD": "T1234567890123", "PROCESS_KBN": "01", "CORRECTION_KBN": "0",
      "PERSONAL_KBN": "1", "DOMESTIC_KBN": "1", "LATEST_KBN": "1",
      "CREATE_DATE": "20260401", "UPDATE_DATE": "20260801",
      "REVOCATION_DATE": "00000000", "EXPIRATION_DATE": "00000000" },
    { "INVOICE_CD": "T9876543210987", "...": "..." }
  ]
}
```

Fully **code based** (same process as the dynamic read service
`ZTABLE_META_SRV`): a Model Provider extending `/IWBEP/CL_MGW_ABS_MODEL` and
a Data Provider extending `/IWBEP/CL_MGW_ABS_DATA`. **No SEGW project.**

| Object | Name |
|--------|------|
| Model Provider Class (MPC) | `ZCL_ZCCBJI_JCTINVR_MPC` |
| Data Provider Class (DPC)  | `ZCL_ZCCBJI_JCTINVR_DPC` |
| Technical model name       | `ZCCBJI_JCTINVR_MDL` (version 0001) |
| Technical service name     | `ZCCBJI_JCTINVR_SRV` (version 0001) |
| Package                    | CCBJI Z package + transport |

---

## Entity model

### `InvoiceRegistrationsSet` (header entity `InvoiceRegistrations`)
Technical parent — one per POST. Key `REQUEST_ID` (Edm.String 32, nullable,
creatable): CPI leaves it **blank** in the request; the **response** echoes
the processing summary in it, e.g. `U:1234 S:56` (updated / skipped counts).

Navigation property **`InvoiceRegistration`** (1 : 0..n) → the line items.

### `InvoiceRegistrationSet` (item entity `InvoiceRegistration`)
One entity per NTA record — same field format as `/CCBJI/T_JCTINVR`:

| Property | Key | Nullable | MaxLen | Table field |
|----------|:---:|:--------:|-------:|-------------|
| INVOICE_CD | ✔ | – | 14 | INVOICE_CD |
| PROCESS_KBN | – | ✔ | 2 | PROCESS_KBN |
| CORRECTION_KBN | – | ✔ | 1 | CORRECTION_KBN |
| PERSONAL_KBN | – | ✔ | 1 | PERSONAL_KBN |
| DOMESTIC_KBN | – | ✔ | 1 | DOMESTIC_KBN |
| LATEST_KBN | – | ✔ | 1 | LATEST_KBN |
| CREATE_DATE | – | ✔ | 10 | CREATE_DATE |
| UPDATE_DATE | – | ✔ | 10 | UPDATE_DATE |
| REVOCATION_DATE | – | ✔ | 10 | REVOCATION_DATE |
| EXPIRATION_DATE | – | ✔ | 10 | EXPIRATION_DATE |

Dates are strings; the backend accepts `YYYYMMDD`, `YYYY-MM-DD` and
`DD.MM.YYYY` (empty / `00000000` = not maintained). `MANDT` and the
`/CCEJ/MDM_DELTA` audit fields (ZERNAM, ZERSDA, ZERZZT, ZAENAM, ZUPDAT,
ZUPTIM, ZUPDIND) are **never sent** — the backend fills them.

---

## Processing logic (Option A — literal port of the AS-IS diagram)

Compare field: incoming **registered number** = **`INVOICE_CD`**. Single-field
match, no field-by-field change detection.

| Case | Action |
|------|--------|
| INVOICE_CD **found** in `/CCBJI/T_JCTINVR` | Incoming record written back over the row: 9 business fields from the payload; original ZERNAM/ZERSDA/ZERZZT preserved; ZAENAM/ZUPDAT/ZUPTIM = current user/date/time; **ZUPDIND = 'U'** |
| INVOICE_CD **not found** | **Skipped** (no insert), counted in the `S:` part of the response summary |
| Validation error (empty INVOICE_CD, bad date), DB error, no authority | Whole batch rejected, ROLLBACK, HTTP error with message — CPI retries the chunk |
| Duplicate INVOICE_CD within one batch | Last occurrence wins |

The write is one `MODIFY /ccbji/t_jctinvr FROM TABLE` + `COMMIT WORK AND WAIT`.
Security: `AUTHORITY-CHECK OBJECT 'S_TABU_NAM'` with **ACTVT 02 (change)** on
`/CCBJI/T_JCTINVR` before anything is written.

---

## Build steps

### 1. Create the two classes
Create `ZCL_ZCCBJI_JCTINVR_MPC` and `ZCL_ZCCBJI_JCTINVR_DPC` in SE24 (or ADT)
and paste the source from:
- `ZCL_ZCCBJI_JCTINVR_MPC.abap`
- `ZCL_ZCCBJI_JCTINVR_DPC.abap`

Activate both (only standard dependencies: `/IWBEP/*` bases,
`DATE_CHECK_PLAUSIBILITY`, the `/CCBJI/T_JCTINVR` table itself).

### 2. Register the service — backend
`/IWBEP/REG_SERVICE` (code-based services are not auto-discovered):
- Technical Model Name `ZCCBJI_JCTINVR_MDL`, Version `0001`,
  Model Provider Class `ZCL_ZCCBJI_JCTINVR_MPC`
- Technical Service Name `ZCCBJI_JCTINVR_SRV`, Version `0001`,
  Data Provider Class `ZCL_ZCCBJI_JCTINVR_DPC`

### 3. Activate on the hub — `/IWFND/MAINT_SERVICE`
- **Add Service** → System Alias `LOCAL` → filter `ZCCBJI_JCTINVR_SRV`
  → Add Selected Services → assign package / transport.

### 4. Clear cache + verify metadata
- `/IWBEP/CACHE_CLEANUP` and `/IWFND/CACHE_CLEANUP`
- `/IWFND/MAINT_SERVICE` → select the service → **Load Metadata**
- Browser: `/sap/opu/odata/sap/ZCCBJI_JCTINVR_SRV/$metadata?x=1`
  → entity sets `InvoiceRegistrationsSet` + `InvoiceRegistrationSet`,
  navigation `InvoiceRegistration`, `sap:creatable="true"`.

### 5. Test — `/IWFND/GW_CLIENT`
1. GET `/sap/opu/odata/sap/ZCCBJI_JCTINVR_SRV/$metadata` (sanity)
2. POST the payload above to `.../InvoiceRegistrationsSet`
   (X-CSRF-Token: GET with `Fetch` first, or use the client's *Use as Request*).
3. Check the response `REQUEST_ID` = `U:n S:m` and the rows in SE16
   `/CCBJI/T_JCTINVR` (ZUPDIND = 'U', ZAENAM/ZUPDAT/ZUPTIM stamped).

Test cases:
| # | Case | Expected |
|---|------|----------|
| 1 | Existing INVOICE_CD | row overwritten, ZUPDIND 'U', counted in U: |
| 2 | Unknown INVOICE_CD | untouched DB, counted in S: |
| 3 | Invalid date | HTTP 400, nothing written |
| 4 | Duplicate in batch | last occurrence wins |
| 5 | Empty batch | HTTP 400 "Empty batch" |
| 6 | User without S_TABU_NAM 02 | error, nothing written |
| 7 | 5k records | single call OK, response summary consistent |

---

## CPI receiver channel

- Adapter **OData V2**, Operation **Create (POST)**,
  Resource Path **`InvoiceRegistrationsSet`**, **Sub Levels = 1**
  (pulls the nested `InvoiceRegistration` items into the deep payload).
- CSRF enabled. Map the NTA fields → child `InvoiceRegistration`;
  leave `REQUEST_ID` unmapped.
- **Chunk large NTA files** (e.g. 5–10k records per POST). Each chunk is
  all-or-nothing: on HTTP 4xx/5xx nothing of that chunk was written → safe
  to retry the same chunk.
- Log the response `REQUEST_ID` (`U:n S:m`) per chunk for reconciliation.

## Security

- CPI technical user needs `S_TABU_NAM` **ACTVT 02** for `/CCBJI/T_JCTINVR`
  (plus the standard Gateway service authorization for `ZCCBJI_JCTINVR_SRV`).
- The service writes only this one table; the table name is a constant in the
  DPC (`GC_TABNAME`) — no dynamic table access.

## Checklist
| # | Where | Action |
|---|-------|--------|
| 1 | SE24/ADT | create + activate `ZCL_ZCCBJI_JCTINVR_MPC`, `ZCL_ZCCBJI_JCTINVR_DPC` |
| 2 | /IWBEP/REG_SERVICE | register model `ZCCBJI_JCTINVR_MDL` + service `ZCCBJI_JCTINVR_SRV` |
| 3 | /IWFND/MAINT_SERVICE | Add Service `ZCCBJI_JCTINVR_SRV` |
| 4 | cache | `/IWBEP/` + `/IWFND/CACHE_CLEANUP`, Load Metadata, verify `$metadata?x=1` |
| 5 | /IWFND/GW_CLIENT | POST deep payload → check SE16 + response summary |
| 6 | Role | grant `S_TABU_NAM` ACTVT 02 on `/CCBJI/T_JCTINVR` to the CPI user |
| 7 | CPI | Create (POST), `InvoiceRegistrationsSet`, Sub Levels 1, map items, chunk, deploy |
