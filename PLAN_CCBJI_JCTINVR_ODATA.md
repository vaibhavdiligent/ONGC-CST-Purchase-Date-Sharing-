# Plan — Inbound OData Service for /CCBJI/T_JCTINVR (NTA Invoice Registration Numbers)

**Status:** PLAN — for review before build
**Interface:** NTA file → SAP CPI → **OData (this development)** → S/4HANA
**Replaces (AS-IS):** ABInitio compare logic that read `/CCBJI/T_JCTINVR` from ECC, compared the
incoming NTA registered numbers, and inserted back into the same table.
**TO-BE:** CPI posts the NTA records to a new OData service; the **compare + insert logic moves
into the S/4 backend** (inside the OData data-provider class).

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

Key observation: the table key is only `INVOICE_CD`, so there is exactly **one row per
registration number** — "insert back" in the AS-IS diagram translates to
**INSERT for new numbers / UPDATE for existing ones** (`MODIFY`), with `ZUPDIND`
recording which one happened and `LATEST_KBN` flagging the record as current.

## 2. Technology choice — OData V2 via SEGW (Gateway), deep-entity pattern

Chosen to match the existing, working CPI-inbound service in this repo
(`ZGMS_EXCHRATE_ODATA_GUIDE.md`, `ZCL_GMS_EXCHRATE_DPC/MPC.abap`):

- **OData V2** — CPI's OData receiver adapter default, POST with CSRF handling out of the box.
- **SEGW project** (not RAP) — the processing is a batch "compare and write" on a namespaced
  table with no BO semantics; a `CREATE_DEEP_ENTITY` redefinition is the simplest robust fit
  and mirrors the ZGMS_EXCHRATE precedent already proven with this CPI landscape.
- **Deep entity (header + items)** — CPI sends the whole NTA delta in ONE POST
  (Sub Levels = 1), so the full batch is processed in a single LUW: all-or-nothing,
  one commit, one response.

All objects in customer namespace `Z` (the `/CCBJI/` namespace is add-on-owned; we only
write to its table, we don't create objects in it).

## 3. Objects to create

| Object | Name (proposed) | Notes |
|--------|-----------------|-------|
| SEGW project | `ZCCBJI_JCTINVR` | package as per system convention + workbench transport |
| Header entity | `InvoiceRegistrations` / set `InvoiceRegistrationsSet` | technical key `REQUEST_ID` (Edm.String 32, nullable, creatable) — CPI leaves blank |
| Item entity | `InvoiceRegistration` / set `InvoiceRegistrationSet` | 10 business fields, all Edm.String, Creatable+Updatable ticked |
| Association | `InvoiceRegistrations_InvoiceRegistration`, 1 : 0..n, nav. property **`InvoiceRegistration`** | exact name matters for CPI mapping |
| Generated classes | `ZCL_ZCCBJI_JCTINVR_MPC/_MPC_EXT/_DPC/_DPC_EXT` | via Generate Runtime Objects |
| Service | `ZCCBJI_JCTINVR_SRV` | registered in `/IWFND/MAINT_SERVICE`, alias LOCAL |

### Item entity properties (the payload CPI sends — "same format" as the table)

| Property | Key | Nullable | MaxLen | Maps to |
|----------|:---:|:--------:|-------:|---------|
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

Dates travel as Edm.String and are normalized in the backend (accept `YYYYMMDD`,
`YYYY-MM-DD` and `DD.MM.YYYY`); MANDT and the 7 audit fields are **not** in the payload —
the backend fills them.

## 4. Processing logic — `CREATE_DEEP_ENTITY` (the "Compare Logic" box from the diagram)

```
1. read_entry_data → header + item table (whole NTA batch in one call)
2. VALIDATE each item:
      - INVOICE_CD not initial, length ≤ 14 (pattern T + 13 digits if NTA standard)
      - dates parse to valid DATS; collect per-record errors
      - duplicate INVOICE_CD inside the batch → keep last occurrence (log warning)
3. FETCH: SELECT the matching rows from /ccbji/t_jctinvr
      FOR ALL ENTRIES / IN range of incoming INVOICE_CDs  → lt_existing (sorted table)
4. COMPARE each incoming record against lt_existing:
      a) NOT found  → new record:      ZUPDIND = 'I',
                                       ZERNAM/ZERSDA/ZERZZT = sy-uname/sy-datum/sy-uzeit
      b) Found & any business field differs
                    → changed record:  ZUPDIND = 'U',
                                       keep original ZERNAM/ZERSDA/ZERZZT,
                                       ZAENAM/ZUPDAT/ZUPTIM = sy-uname/sy-datum/sy-uzeit
      c) Found & identical → SKIP (no DB touch, counted as "unchanged")
5. WRITE: MODIFY /ccbji/t_jctinvr FROM TABLE lt_upsert   (single LUW)
      - sy-subrc ≠ 0 → ROLLBACK WORK + /iwbep/cx_mgw_busi_exception with message container
      - any validation errors collected in step 2 → ROLLBACK + error response
        (all-or-nothing, same contract as the AS-IS full-batch job)
6. COMMIT WORK AND WAIT; return header entity + summary message
      (inserted / updated / unchanged counts via message container)
```

Notes:
- **`MODIFY` (upsert)**, not `INSERT`, because the single-field key makes
  "found → insert" from the AS-IS diagram physically an update on S/4.
- No standard BAPI exists for this add-on table → direct `MODIFY` of the transparent
  table inside the DPC, wrapped in explicit commit/rollback (acceptable: the table is
  add-on master data, APPL0, no number ranges or change documents involved).
- Batch sizes: NTA full files can be large — CPI should chunk (e.g. 5–10k records per
  POST); the service is stateless per call so chunking is safe.

## 5. Build & activation steps (system work, follows the ZGMS_EXCHRATE checklist)

| # | Where | Action |
|---|-------|--------|
| 1 | SEGW | Create project `ZCCBJI_JCTINVR` |
| 2 | SEGW | Item entity `InvoiceRegistration` (10 props per table above, Creatable+Updatable) |
| 3 | SEGW | Header entity `InvoiceRegistrations` (key `REQUEST_ID`, nullable) |
| 4 | SEGW | Association 1 : 0..n, navigation property `InvoiceRegistration` |
| 5 | SEGW | Generate Runtime Objects |
| 6 | SE24 | `..._DPC_EXT` → redefine `CREATE_DEEP_ENTITY` (logic in §4) |
| 7 | SE24 | `..._MPC_EXT` → redefine `DEFINE` → `set_creatable( abap_true )` on both entity sets |
| 8 | /IWFND/MAINT_SERVICE | Add service `ZCCBJI_JCTINVR_SRV`, alias LOCAL |
| 9 | Caches | `/IWBEP/CACHE_CLEANUP`, `/IWFND/CACHE_CLEANUP`, Load Metadata, check `$metadata` |
| 10 | /IWFND/GW_CLIENT | POST test (payload below) → verify in SE16 `/CCBJI/T_JCTINVR` |

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

Test cases: new number (→ I), existing number changed (→ U), existing identical (→ skip),
invalid date (→ 400 + rollback), duplicate in batch, empty batch, 5k-record volume test.

## 6. CPI receiver channel (for the CPI team)

- Adapter: **OData V2**, Operation **Create (POST)**, Resource Path `InvoiceRegistrationsSet`,
  **Sub Levels = 1**, CSRF enabled.
- Map NTA fields → child `InvoiceRegistration`; leave `REQUEST_ID` unmapped.
- Chunk large NTA files; on HTTP 4xx/5xx the whole chunk is rolled back → safe to retry.

## 7. Repo deliverables (this branch)

1. `zcl_zccbji_jctinvr_dpc_ext.clas.abap` — DPC_EXT with `CREATE_DEEP_ENTITY` compare/upsert logic
2. `zcl_zccbji_jctinvr_mpc_ext.clas.abap` — MPC_EXT `DEFINE` redefinition
3. `ZCCBJI_JCTINVR_ODATA_GUIDE.md` — step-by-step SEGW build/registration/test guide
   (same format as `ZGMS_EXCHRATE_ODATA_GUIDE.md`)

## 8. Open questions (answers refine the build, defaults are safe)

1. **Compare scope:** AS-IS text says "if registered number FOUND then insert". Default
   assumption: standard delta upsert — insert when new, update when changed (§4). If NTA
   deltas must instead only refresh already-known numbers, step 4a becomes "skip + log".
2. **Deletes:** does the NTA feed ever carry revocations as physical deletes (`ZUPDIND = 'D'`),
   or only `REVOCATION_DATE` updates? Default: no physical deletes.
3. **LATEST_KBN:** sent by CPI as-is (current default) or forced to '1' by the backend?
4. **Error contract:** all-or-nothing per POST (default, matches AS-IS batch) vs.
   accept-partial with an error list returned to CPI.
5. Confirm package + transport naming convention for the SEGW project.
