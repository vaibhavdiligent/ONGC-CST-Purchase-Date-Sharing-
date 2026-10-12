# ZOVL_VENDOR_SYNC_SRV: inbound OData V2, ONGC → OVL Employee Vendor (VEMP) sync

Source: `ONGC_OVL_Vendor_Data_Sync_Document.pdf` (OVL-INT-VEN-001 v1.0, 07.10.2026).
Decisions confirmed by the user on 12.10.2026:

| # | Decision |
|---|---|
| 1 | Vendors that do not exist in OVL are **created**. Vendors that exist are updated (upsert). This replaces the document's "new vendor creation out of scope" rule. |
| 2 | ONGC calls the **OVL CPI** iFlow, and CPI calls this OData service in **OVL S/4HANA**. |
| 3 | The OVL BP number and vendor number are the **same as the ONGC LIFNR**. |
| 4 | Company code **OVL**, BP grouping **VEMP**. |
| 5 | Payment terms are **defaulted in OVL**. The ONGC ZTERM is ignored. |
| 6 | **OData V2**, code-based (same build process as the Coca-Cola service `ZCCBJI_JCTINVR_SRV`). |

| Deliverable | File |
|---|---|
| Model Provider class | `ZCL_ZOVL_VENDOR_SYNC_MPC.abap` |
| Data Provider class | `ZCL_ZOVL_VENDOR_SYNC_DPC.abap` |
| Business logic (BP upsert via CVI) | `ZCL_OVL_VENDOR_SYNC.abap` |
| Log table | `ZOVL_VEN_SYNCLOG` (in the zip) |
| abapGit package (3 classes + table) | `ZOVL_VENDOR_SYNC_abapGit.zip` |
| This document | `ZOVL_VENDOR_SYNC_TECH_SPEC.md` |

---

## 1. Flow

```
ONGC S/4 batch job (delta BUT000-CHDAT = SY-DATUM-1, VEMP, LFB1-BUKRS = OVL)
   → ONGC PI/PO → OVL CPI iFlow (validation, mapping, 1 message per vendor)
      → POST /sap/opu/odata/sap/ZOVL_VENDOR_SYNC_SRV/VendorSet   (deep insert)
          → ZCL_ZOVL_VENDOR_SYNC_DPC → ZCL_OVL_VENDOR_SYNC=>UPSERT
              → CVI_EI_INBOUND_MAIN (BP + vendor in one call) → COMMIT
          ← 201 + STATUS/ACTION/MESSAGE   or   400 + error messages
      ← CPI logs the result; on error, alert / manual resolution
```

The ONGC side (selection program, steps 1–6 of the document) and the CPI iFlow are not part of this delivery. This delivery covers the OVL receiver only.

## 2. Objects

| Object | Name | Notes |
|---|---|---|
| Model Provider Class | `ZCL_ZOVL_VENDOR_SYNC_MPC` | inherits `/IWBEP/CL_MGW_ABS_MODEL`; `DEFINE` is redefined; payload types are in the public section |
| Data Provider Class | `ZCL_ZOVL_VENDOR_SYNC_DPC` | inherits `/IWBEP/CL_MGW_ABS_DATA`; redefines `CREATE_DEEP_ENTITY` and `CREATE_ENTITY` |
| Business class | `ZCL_OVL_VENDOR_SYNC` | `UPSERT` (one vendor = one LUW) and `WRITE_LOG`; can be reused for the initial load |
| Table | `ZOVL_VEN_SYNCLOG` | log of every call; delivery class A; display only |
| Technical model | `ZOVL_VENDOR_SYNC_MDL` version 0001 | `/IWBEP/REG_SERVICE` |
| Technical service | `ZOVL_VENDOR_SYNC_SRV` version 0001 | `/IWBEP/REG_SERVICE` + `/IWFND/MAINT_SERVICE` |

### 2.1 `ZOVL_VEN_SYNCLOG`
| Field | Key | Data element | Meaning |
|---|---|---|---|
| MANDT | ✔ | MANDT | |
| LIFNR | ✔ | LIFNR | vendor / BP number |
| ERDAT | ✔ | ERDAT | date of the call |
| ERZET | ✔ | ERZET | time of the call |
| ACTION | | CHAR10 | CREATE / UPDATE |
| STATUS | | BAPI_MTYPE | S / E |
| MESSAGE | | BAPI_MSG | leading message |
| ERNAM | | ERNAM | API user (CPI) |

## 3. Entity model

**`VendorSet`** (entity `Vendor`, LFA1). Navigation `ToCompanyCode` (1:0..n) → `CompanyCodeSet` (LFB1), and `ToBank` (1:0..n) → `BankSet` (LFBK).
All properties are `Edm.String`, nullable and creatable. CPI uses Sub Levels = 1.

### 3.1 Field mapping, LFA1 → BP / vendor

| Property | Len | Target in OVL | Rule |
|---|---|---|---|
| LIFNR | 10 | BP number = vendor number | ALPHA-converted. Determines create or update. |
| LAND1 | 3 | BP address country | mandatory |
| NAME1 | 35 | person: last name (if NAME_LAST is empty); organisation: NAME1 | mandatory |
| ORT01 | 35 | address city | |
| ORT02 | 35 | address district | |
| PSTLZ | 10 | address postal code | |
| REGIO | 3 | address region | |
| SORTL | 10 | BP search term 1 → LFA1-SORTL | NAME1 is used if empty |
| STRAS | 60 | address street | |
| ADRNR, MCOD1, MCOD3 | | — | Accepted but ignored. OVL generates these itself. |
| ANRED | 15 | BP title key | text (e.g. `Mr.`) → key via TSAD3T, or a key directly via TSAD3 |
| KONZS | 10 | LFA1-KONZS (vendor node) | |
| SPRAS | 2 | BP language, address language | SAP key (`E`) or ISO code (`EN`); default is the logon language |
| TELF1 | 30 | address telephone | the list sent replaces the OVL list |
| GBORT | 25 | person place of birth | persons only |
| J_1IPANNO | 40 | LFA1-J_1IPANNO (vendor node) | same as `ZGEM_CREATE_VENDOR` |
| NAME_FIRST / NAME_LAST | 40 | person first / last name | **optional addition** to the document payload (ONGC BUT000) |
| BP_CATEGORY | 1 | BP category on create | **optional**; default from STVARV (`1` = person) |
| STATUS / ACTION / MESSAGE | | response only | CPI leaves these empty |

### 3.2 LFB1 → company code data (company code OVL only)
The row with BUKRS = OVL is used. If there is none, the first row is used.

| Property | Rule |
|---|---|
| AKONT | Used if it exists in SKB1 for OVL. Otherwise the STVARV default `ZOVL_VENSYNC_AKONT` is used. If neither exists, the call fails with an error. |
| ZTERM | **Ignored.** The OVL default `ZOVL_VENSYNC_ZTERM` is used (fallback `0001`). |
| ZUAWA, ZWELS, KVERM | passed through when filled |
| PERNR | Set only if it exists in PA0003 of OVL. Otherwise a warning is logged and the vendor is still created. |

### 3.3 LFBK → BP bank details (CVI derives LFBK)
| Property | Rule |
|---|---|
| BANKS / BANKL / BANKN | bank country, key and account. **BANKL must exist in OVL BNKA (FI01).** Otherwise the call fails with a clear error. |
| BVTYP | bank detail ID. On update, an existing ID or the same account is updated; anything else is inserted. Left blank, CVI assigns the ID. |
| KOINH | account holder; NAME1 is used if empty |

## 4. Processing logic (`ZCL_OVL_VENDOR_SYNC=>UPSERT`)

1. Validate: LIFNR, NAME1 and LAND1 are mandatory, and the bank keys must exist. Authority check `F_LFA1_BUK` (ACTVT 01 for create, 02 for change, BUKRS OVL).
2. Check BUT000 for the LIFNR.
   - **Not found → CREATE.** Partner node task `I` with `bpartner` = LIFNR, a new GUID, category, grouping VEMP, role FLVN00, address with usage XXDEFAULT, bank details. Vendor node task `I` with `lifnr` = LIFNR, KTOKK VEMP, PAN, KONZS, and company code OVL (task `M`).
   - **Found → UPDATE.** Partner node task `U` (existing GUID). Name, search term, language and title are overwritten. The standard address is updated (task `U` with its GUID). Bank details are updated or inserted. Role FLVN00 is added only if it is missing. Vendor node task `U` (or `I` if the BP has no vendor yet). Company code OVL uses task `M`.
3. Call `CVI_EI_INBOUND_MAIN` for BP and vendor together. This is the same pattern as `ZGEM_CREATE_VENDOR`, which already runs in OVL.
4. If there is an E/A message, `BAPI_TRANSACTION_ROLLBACK` runs and the call returns HTTP 400 with the CVI messages.
   Otherwise `BAPI_TRANSACTION_COMMIT WAIT`, then a check that LFB1 exists for OVL. The call returns HTTP 201 with STATUS `S`.
5. The result is written to `ZOVL_VEN_SYNCLOG` in both cases.

Because the logic is an upsert, CPI can safely resend a failed or duplicated message.

## 5. OVL prerequisites (functional / Basis), required before the first test

| # | Prerequisite | Where |
|---|---|---|
| P1 | BP grouping **VEMP** with an **external** number range that covers the ONGC VEMP number interval | `BUCF` / SPRO BP number ranges + groupings |
| P2 | Vendor account group **VEMP** with an external number range of the same interval, and CVI "same number" assignment of BP grouping VEMP → account group VEMP | SPRO → Cross-Application Components → Master Data Synchronization → CVI → Define Number Assignment for Direction BP to Vendor |
| P3 | Role FLVN00 allowed for grouping VEMP | `BUSD` / role grouping |
| P4 | Field status of VEMP / company code OVL allows the fields sent, and no other mandatory field is left unfilled (e.g. cash management group FDGRV) | `OBD3`, `OBC5` |
| P5 | Reconciliation accounts used by ONGC exist in OVL, or `ZOVL_VENSYNC_AKONT` is maintained | `FS00` / `STVARV` |
| P6 | Bank keys of employee banks exist in OVL | `FI01` (or `BAUP` mass load) |
| P7 | API user for CPI (system user) with S_SERVICE for `ZOVL_VENDOR_SYNC_SRV`, `F_LFA1_BUK` 01/02 for OVL, plus the BP authorizations `B_BUPA_*` and `F_LFA1_APP`/`F_LFA1_GRP` | `PFCG` |

### 5.1 STVARV parameters (type P, all optional)
| Name | Default | Meaning |
|---|---|---|
| ZOVL_VENSYNC_BUKRS | OVL | target company code |
| ZOVL_VENSYNC_GROUPING | VEMP | BP grouping |
| ZOVL_VENSYNC_KTOKK | VEMP | vendor account group |
| ZOVL_VENSYNC_ZTERM | 0001 | **default payment terms in OVL** (confirm the value) |
| ZOVL_VENSYNC_AKONT | — | reconciliation account if the ONGC account does not exist in OVL |
| ZOVL_VENSYNC_BPCAT | 1 | BP category when ONGC does not send it (1 = person, 2 = organisation) |

## 6. Build and registration steps

1. **Import:** pull `ZOVL_VENDOR_SYNC_abapGit.zip` with abapGit (offline repo) into the OVL Z package, or create the 3 classes in SE24/ADT and the table in SE11 manually. Activate in this order: table → MPC → business class → DPC.
2. **`/IWBEP/REG_SERVICE`:**
   - Model `ZOVL_VENDOR_SYNC_MDL` 0001, Model Provider Class `ZCL_ZOVL_VENDOR_SYNC_MPC`.
   - Service `ZOVL_VENDOR_SYNC_SRV` 0001, Data Provider Class `ZCL_ZOVL_VENDOR_SYNC_DPC`. Assign the model.
3. **`/IWFND/MAINT_SERVICE`:** Add Service → System Alias `LOCAL` → `ZOVL_VENDOR_SYNC_SRV` → package / transport.
4. Run `/IWBEP/CACHE_CLEANUP` and `/IWFND/CACHE_CLEANUP`. Then open `/sap/opu/odata/sap/ZOVL_VENDOR_SYNC_SRV/$metadata` and check for `VendorSet`, `CompanyCodeSet`, `BankSet`, navigation `ToCompanyCode`/`ToBank`, and `sap:creatable="true"`.
5. Maintain the STVARV parameters (§5.1) and the prerequisites P1–P7.

## 7. Payload: CPI → OVL

```json
POST /sap/opu/odata/sap/ZOVL_VENDOR_SYNC_SRV/VendorSet
Content-Type: application/json
Accept: application/json
X-CSRF-Token: <fetched>

{
  "LIFNR": "0000712345", "LAND1": "IN", "NAME1": "RAJESH KUMAR",
  "NAME_FIRST": "RAJESH", "NAME_LAST": "KUMAR",
  "ORT01": "NEW DELHI", "ORT02": "VASANT KUNJ", "PSTLZ": "110070", "REGIO": "07",
  "SORTL": "RKUMAR", "STRAS": "PLOT 5A, NELSON MANDELA MARG",
  "ANRED": "Mr.", "KONZS": "", "SPRAS": "EN", "TELF1": "9810000000",
  "GBORT": "DEHRADUN", "J_1IPANNO": "ABCDE1234F",
  "ToCompanyCode": [
    { "BUKRS": "OVL", "PERNR": "00712345", "ZUAWA": "009",
      "AKONT": "0000190101", "ZWELS": "8", "ZTERM": "0001", "KVERM": "" }
  ],
  "ToBank": [
    { "BANKS": "IN", "BANKL": "SBIN0000691", "BANKN": "12345678901",
      "BVTYP": "0001", "KOINH": "RAJESH KUMAR" }
  ]
}
```
LIFNR inside the child rows can be left empty because the header LIFNR is used. The XML (Atom) format works the same way.

**Response 201:** the posted vendor is returned with `"STATUS": "S"`, `"ACTION": "CREATE"` (or `"UPDATE"`), and `"MESSAGE": "Vendor 712345 created in company code OVL."`.
**Response 400:** the standard Gateway error body. `error.message.value` holds the leading message, and `innererror.errordetails` lists all CVI messages.

## 8. CPI receiver channel (OVL CPI)

- Adapter **OData V2**, operation **Create (POST)**, resource `VendorSet`, Sub Levels **1** (deep insert).
- **One vendor per call.** Use a splitter before the receiver and **do not enable batch processing**. Each vendor is its own LUW (it commits), so `$batch` changesets are not supported.
- CSRF token handling is done by the adapter. Authentication is Basic or certificate for the API user (P7).
- Error handling (document §10): on HTTP 5xx or connection failures, retry 3 times with exponential backoff and then raise an alert. On HTTP 400 (data errors), don't retry. Log the error, flag the record and alert the interface monitor.
- **Initial / full load:** ONGC sends all VEMP vendors of company code OVL through the same iFlow once. The upsert creates missing vendors and updates existing ones.

## 9. Test cases (`/IWFND/GW_CLIENT` or CPI)

| # | Case | Expected |
|---|---|---|
| 1 | New LIFNR, full payload | 201 CREATE. BP = LIFNR, role FLVN00, LFA1 KTOKK VEMP, LFB1 OVL with default ZTERM, LFBK, PAN. Log row S. |
| 2 | Same payload again | 201 UPDATE, no duplicate bank or address |
| 3 | Changed address / bank account / phone | 201 UPDATE, new values in BP and LFA1/LFBK |
| 4 | Bank key not in OVL BNKA | 400 "Bank key … does not exist in OVL", nothing saved, log row E |
| 5 | AKONT not in OVL and no STVARV default | 400 recon account message |
| 6 | PERNR not in OVL | 201, warning in the log, PERNR empty |
| 7 | LIFNR outside the VEMP external number range | 400 CVI number range message |
| 8 | Missing NAME1 / LAND1 | 400 validation message |
| 9 | API user without F_LFA1_BUK | 400 authorization message |
| 10 | Flat POST to VendorSet without children | 201. Company code data is created with the recon account default. |

## 10. Open points and risks

| # | Point | Proposed handling |
|---|---|---|
| O1 | Is the ONGC employee vendor a **person** (category 1) or an **organisation** (category 2)? | Default is person (GBORT is person-only). ONGC should send NAME_FIRST / NAME_LAST / BP_CATEGORY from BUT000. Otherwise NAME1 goes into the last name. |
| O2 | Value of the **OVL default payment term** | Maintain `ZOVL_VENSYNC_ZTERM` (code fallback `0001`, as used for GeM vendors in OVL) |
| O3 | Is the reconciliation account the same in ONGC and OVL? | If not, maintain `ZOVL_VENSYNC_AKONT` |
| O4 | Does OVL require more LFB1 fields for VEMP (e.g. FDGRV, REPRF)? | Check the field status (P4). If needed, add defaults in `FILL_VENDOR`. |
| O5 | The document has no step 5, and step 4 (CDHDR) is not used by the payload | ONGC side, no impact on OVL |
| R1 | The code was written outside an SAP system and has **not been syntax-checked**. CVI field names follow the working `ZGEM_CREATE_VENDOR`; the new nodes (person, address update, telephone) follow the standard `CVIS_EI_EXTERN` structures. | Activate in DEV and fix any component-name differences for the release |
