# Plan — Inbound OData Service for /CCBJI/T_JCTINVR (NTA Invoice Registration Numbers)

**Status:** APPROVED — Option A confirmed 13.08.2026 (build in progress)
**Customer:** Coca-Cola Bottlers Japan (CCBJI), S/4HANA
**Interface:** NTA file → SAP CPI → **OData (this development)** → S/4HANA
**Replaces (AS-IS):** ABInitio compare logic that read `/CCBJI/T_JCTINVR` from ECC, compared the
incoming NTA registered numbers, and inserted back into the same table.
**TO-BE (per flow diagram):** CPI posts the NTA records to a new OData service; the
**compare + insert logic moves into the S/4 backend** (inside the OData data-provider class).

**Build approach:** fully **code-based OData V2** — MPC + DPC classes only, **no SEGW project** —
i.e. the exact same process already used for the dynamic table read service
(`ZTABLE_META` — see `ZTABLE_META_ODATA_GUIDE.md`, `ZCL_ZTABLE_META_MPC/DPC.abap`).
This is the write-direction counterpart of that development.

---

## 1. Target table (from CCBJI_T_JCTINVR.pdf)

`/CCBJI/T_JCTINVR` — *Invoice registration number (qualified invoicing business)*,
transparent table, key = `MANDT` + `INVOICE_CD`.

| # | Field | Key | Type | Len | Data Element | Description |
|---|-------|:---:|------|----:|--------------|-------------|
| 1 | MANDT | X | CLNT | 3 | MANDT | Client |
| 2 | INVOICE_CD | X | CHAR | 14 | /CCBJI/INVREG | Invoice registration number |
| 3 | PROCESS_KBN | | CHAR | 2 | /CCBJI/BPC | Business processing classification |
| 4 | CORRECTION_KBN | | CHAR | 1 | /CCBJI/CORR_KBN | Correction classification |
| 5 | PERSONAL_KBN | | CHAR | 1 | /CCBJI/PER_KBN | Personnel classification |
| 6 | DOMESTIC_KBN | | CHAR | 1 | /CCBJI/DOM_KBN | Domestic/foreign classification |
| 7 | LATEST_KBN | | CHAR | 1 | /CCBJI/LAT_KBN | Latest history |
| 8 | CREATE_DATE | | DATS | 8 | /CCBJI/REGDAT | Registration date |
| 9 | UPDATE_DATE | | DATS | 8 | /CCBJI/UPDDAT | Update date |
| 10 | REVOCATION_DATE | | DATS | 8 | /CCBJI/REVDAT | Revocation date |
| 11 | EXPIRATION_DATE | | DATS | 8 | /CCBJI/EXPDAT | Expiration date |
| — | *INCLUDE /CCEJ/MDM_DELTA (audit fields — filled by backend, NOT by CPI):* | | | | | |
| 12 | ZERNAM | | CHAR | 12 | ERNAM | Created by |
| 13 | ZERSDA | | DATS | 8 | ERSDA | Created on |
| 14 | ZERZZT | | TIMS | 6 | ERZZT | Created at |
| 15 | ZAENAM | | CHAR | 12 | AENAM | Changed by |
| 16 | ZUPDAT | | DATS | 8 | UPDAT | Changed on |
| 17 | ZUPTIM | | TIMS | 6 | UPTIM | Changed at |
| 18 | ZUPDIND | | CHAR | 1 | /CCEJ/MDM_UPD_IND | Update indicator — fixed values **D**elete / **I**nsert / **U**pdate |

## 2. Technology choice — code-based OData V2 (same process as ZTABLE_META dynamic read)

Same recipe as the dynamic read service already built for this landscape:

- **OData V2**, CPI OData receiver adapter, CSRF handled by the adapter.
- **Code-based model + data provider — no SEGW project:**
  - MPC class inheriting `/IWBEP/CL_MGW_ABS_MODEL`, entity model built in a
    redefined `DEFINE` method (`create_entity_type` / `create_property` /
    `bind_structure` / `create_entity_set`), payload structures declared as
    `TYPES` in the MPC public section.
  - DPC class inheriting `/IWBEP/CL_MGW_ABS_DATA`, the CPI POST handled in a
    redefined `/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY`.
  - Registration on the backend in `/IWBEP/REG_SERVICE` (model + service),
    then `/IWFND/MAINT_SERVICE` Add Service — exactly like `ZTABLE_META_SRV`.
- Differences vs. the read service, because this one is **inbound (write)**:
  - Direction is POST/Create, so the DPC redefines `CREATE_DEEP_ENTITY`
    instead of `GET_ENTITYSET`.
  - The model needs **two entity types + an association/navigation** (header →
    items) so CPI can send the whole NTA batch in ONE deep POST (Sub Levels = 1).
  - Entity sets are flagged `set_creatable( abap_true )` directly in the MPC
    `DEFINE` — being code-based, no MPC_EXT workaround is needed.

All objects in customer namespace `Z` (the `/CCBJI/` namespace is add-on-owned; we only
write to its table, we don't create objects in it).

## 3. Objects to create

| Object | Name | Notes |
|--------|------|-------|
| Model Provider Class | `ZCL_ZCCBJI_JCTINVR_MPC` | inherits `/IWBEP/CL_MGW_ABS_MODEL`; `DEFINE` redefined |
| Data Provider Class | `ZCL_ZCCBJI_JCTINVR_DPC` | inherits `/IWBEP/CL_MGW_ABS_DATA`; `CREATE_DEEP_ENTITY` redefined |
| Technical model name | `ZCCBJI_JCTINVR_MDL` (version 0001) | via `/IWBEP/REG_SERVICE` |
| Technical service name | `ZCCBJI_JCTINVR_SRV` (version 0001) | via `/IWBEP/REG_SERVICE` + `/IWFND/MAINT_SERVICE` |
| Package | CCBJI Z package convention + transport | open question #5 |

### Entity model (defined in code in the MPC `DEFINE`)

**Header entity `InvoiceRegistrations`** / set `InvoiceRegistrationsSet` —
technical parent only, key `REQUEST_ID` (Edm.String 32, nullable, creatable);
CPI leaves it blank; the response echoes the processing summary in it.

**Item entity `InvoiceRegistration`** / set `InvoiceRegistrationSet` —
one entity per NTA record, "same format" as the table's business fields:

| Property | Key | Nullable | MaxLen | Maps to table field |
|----------|:---:|:--------:|-------:|---------------------|
| INVOICE_CD | ✔ | – | 14 | INVOICE_CD |
| PROCESS_KBN | – | ✔ | 2 | PROCESS_KBN |
| CORRECTION_KBN | – | ✔ | 1 | CORRECTION_KBN |
| PERSONAL_KBN | – | ✔ | 1 | PERSONAL_KBN |
| DOMESTIC_KBN | – | ✔ | 1 | DOMESTIC_KBN |
| LATEST_KBN | – | ✔ | 1 | LATEST_KBN |
| CREATE_DATE | – | ✔ | 10 | CREATE_DATE (DATS) |
| UPDATE_DATE | – | ✔ | 10 | UPDATE_DATE (DATS) |
| REVOCATION_DATE | – | ✔ | 10 | REVOCATION_DATE (DATS) |
| EXPIRATION_DATE | – | ✔ | 10 | EXPIRATION_DATE (DATS) |

All properties Edm.String (dates normalized in the backend — accept `YYYYMMDD`,
`YYYY-MM-DD`, `DD.MM.YYYY`). `MANDT` and the 7 audit fields are **not** in the
payload — the backend fills them.

**Association** `InvoiceRegistrations_InvoiceRegistration`, cardinality 1 : 0..n,
navigation property **`InvoiceRegistration`** on the header (exact name matters —
it is the wrapper element CPI maps the line items into). Both entity sets
`set_creatable( abap_true )`.

## 4. Processing logic — `CREATE_DEEP_ENTITY` — **Option A (literal AS-IS diagram, CONFIRMED)**

Compare field: incoming **Registered Number** (NTA) vs **`INVOICE_CD`** in
`/CCBJI/T_JCTINVR` — single-field match, exactly as in the flow diagram
("If Registered Number found in T_JCTINVR Table then Insert into T_JCTINVR").

```
1. read_entry_data → header + item table (whole NTA batch in one call)
2. VALIDATE each item:
      - INVOICE_CD not initial (upper-cased, condensed)
      - dates parse to valid DATS → else reject the whole batch (all-or-nothing)
      - duplicate INVOICE_CD inside the batch → keep last occurrence
3. AUTHORITY: S_TABU_NAM activity 02 (change) on /CCBJI/T_JCTINVR
4. FETCH: SELECT rows from /ccbji/t_jctinvr FOR ALL ENTRIES on incoming INVOICE_CDs
5. COMPARE each incoming record (Option A — literal image logic):
      a) INVOICE_CD FOUND      → write the incoming record back over the existing
                                 row (physically an UPDATE, key already exists):
                                 all 9 business fields taken from the payload,
                                 original ZERNAM/ZERSDA/ZERZZT preserved,
                                 ZAENAM/ZUPDAT/ZUPTIM = sy-uname/sy-datum/sy-uzeit,
                                 ZUPDIND = 'U'
      b) INVOICE_CD NOT found  → SKIP (no insert), counted and reported
      (No field-by-field change detection — every matched record is written back,
       exactly as the AS-IS diagram states.)
6. WRITE: MODIFY /ccbji/t_jctinvr FROM TABLE lt_update (single LUW)
      - failure → ROLLBACK WORK + /iwbep/cx_mgw_busi_exception (message container)
7. COMMIT WORK AND WAIT; response header REQUEST_ID echoes the summary,
      e.g. "U:1234 S:56" (updated / skipped-not-found counts)
```

Notes:
- No standard BAPI exists for this add-on table → direct `MODIFY` of the transparent
  table inside the DPC, wrapped in explicit commit/rollback (acceptable: add-on master
  data, APPL0, no number ranges or change documents involved).
- Error raising via the same `raise_error` / message-container pattern as
  `ZCL_ZTABLE_META_DPC`.
- Batch sizes: NTA full files can be large — CPI should chunk (e.g. 5–10k records per
  POST); the service is stateless per call so chunking is safe.

## 5. Build & activation steps (same checklist shape as ZTABLE_META)

| # | Where | Action |
|---|-------|--------|
| 1 | SE24/ADT | create + activate `ZCL_ZCCBJI_JCTINVR_MPC` (source: `ZCL_ZCCBJI_JCTINVR_MPC.abap`) |
| 2 | SE24/ADT | create + activate `ZCL_ZCCBJI_JCTINVR_DPC` (source: `ZCL_ZCCBJI_JCTINVR_DPC.abap`) |
| 3 | /IWBEP/REG_SERVICE | register model `ZCCBJI_JCTINVR_MDL` 0001 (MPC) + service `ZCCBJI_JCTINVR_SRV` 0001 (DPC) |
| 4 | /IWFND/MAINT_SERVICE | Add Service, alias LOCAL, filter `ZCCBJI_JCTINVR_SRV`, assign package |
| 5 | Caches | `/IWBEP/CACHE_CLEANUP` + `/IWFND/CACHE_CLEANUP`, Load Metadata, verify `$metadata?x=1` (2 entity sets, nav. property `InvoiceRegistration`, `sap:creatable="true"`) |
| 6 | /IWFND/GW_CLIENT | POST test (payload below) → verify in SE16 `/CCBJI/T_JCTINVR` |
| 7 | Role | grant the CPI user change authorization for `/CCBJI/T_JCTINVR` (`S_TABU_NAM` ACTVT 02) |

### Gateway Client test payload

```json
POST /sap/opu/odata/sap/ZCCBJI_JCTINVR_SRV/InvoiceRegistrationsSet
{
  "InvoiceRegistration": [
    { "INVOICE_CD": "T1234567890123", "PROCESS_KBN": "01", "CORRECTION_KBN": "0",
      "PERSONAL_KBN": "1", "DOMESTIC_KBN": "1", "LATEST_KBN": "1",
      "CREATE_DATE": "20260401", "UPDATE_DATE": "20260801",
      "REVOCATION_DATE": "00000000", "EXPIRATION_DATE": "00000000" }
  ]
}
```

Test cases: existing number (→ updated, ZUPDIND 'U', changed-by audit stamped),
unknown number (→ skipped, reported in summary), invalid date (→ 400 + nothing written),
duplicate in batch (last wins), empty batch (→ 400), missing authorization (→ 403-style
error), 5k-record volume test.

## 6. CPI receiver channel (for the CPI team)

- Adapter: **OData V2**, Operation **Create (POST)**, Resource Path `InvoiceRegistrationsSet`,
  **Sub Levels = 1**, CSRF enabled.
- Map NTA fields → child `InvoiceRegistration`; leave `REQUEST_ID` unmapped.
- Chunk large NTA files; on HTTP 4xx/5xx the whole chunk is rolled back → safe to retry.
- Response `REQUEST_ID` carries "U:n S:m" (updated / skipped counts) for iFlow logging.

## 7. Repo deliverables (this branch)

1. `ZCL_ZCCBJI_JCTINVR_MPC.abap` — code-based Model Provider (TYPES + DEFINE)
2. `ZCL_ZCCBJI_JCTINVR_DPC.abap` — code-based Data Provider (CREATE_DEEP_ENTITY, Option A logic)
3. `ZCCBJI_JCTINVR_ODATA_GUIDE.md` — build/registration/test guide in the same format as
   `ZTABLE_META_ODATA_GUIDE.md`

## 8. Decisions & open questions

1. **Compare scope — DECIDED (Option A, 13.08.2026):** single-field match on
   INVOICE_CD; found → write payload back (update); not found → skip + count.
   No field-by-field change detection.
2. **Deletes:** NTA feed assumed to carry revocations only as `REVOCATION_DATE`
   values, never physical deletes (`ZUPDIND = 'D'` not produced by this service).
3. **LATEST_KBN:** passed through from CPI as-is.
4. **Error contract:** all-or-nothing per POST (validation or DB error rolls back the
   whole chunk); "not found → skipped" is NOT an error, it is reported in the summary.
5. Confirm package + transport naming convention for the CCBJI objects (build uses a
   placeholder package prompt in the guide).
