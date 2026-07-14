# ASRS / TrackWise Interfaces — S/4HANA API Migration Specification

**Prepared for:** Client review + CPI (SAP Integration Suite) consultant
**Subject:** Replacing direct database (`EXEC SQL`) integration with API / HTTP calls
**Programs covered:** 10 (reconstructed from the PDF listings in `interface/`)

---

## Part A — Executive summary

These 10 interface programs exchange data between SAP and two external, non-SAP systems:

- **ASRS / WMS** — the automated warehouse system
- **TrackWise** — the Sparta/Honeywell TrackWise Quality Management system (+ RA/MDM material feed)

**Current integration mechanism (as-is):**
> **None of the 10 programs use any API, HTTP, OData, REST, SOAP or RFC call today.**
> **100% of the external integration is done with Native SQL (`EXEC SQL … ENDEXEC`) over a
> DBCON secondary database connection** — i.e. SAP opens a direct connection to the external
> system's database and runs `INSERT` / `SELECT` straight against its tables.

**Why it must change (to-be):**
> On S/4HANA a program **cannot** open a direct connection to a foreign database and execute SQL
> against it (native SQL over DBCON to a 3rd‑party DB is not allowed under clean‑core / S/4HANA
> Cloud, and is deprecated on‑premise). **Every `EXEC SQL` block must be replaced by an API call**
> to an endpoint that the external system (ASRS, TrackWise) exposes — this is where CPI comes in.

**What CPI must build:** a set of APIs / iFlows (detailed in Part C) that accept the same data that
is currently being `INSERT`ed into, or `SELECT`ed from, the external databases. SAP will call these
APIs instead of touching the foreign DB directly.

**Scale of change (all `EXEC SQL`, nothing else, needs to change):**

| Metric | Count |
|--------|------:|
| Programs using external DB via `EXEC SQL` | 10 of 10 |
| Programs already using an API/HTTP call | **0** |
| Total `EXEC SQL` blocks to replace | 82 |
| → external `INSERT` (push to ASRS/TrackWise) | 19 |
| → external `SELECT` (read status/existence) | 3 |
| → connection ops (`CONNECT`/`SET`/`GET`/`DISCONNECT`) | 59 |
| Open-SQL statements on SAP tables (stay unchanged) | 31 |

---

## Part B — Program-by-program: what is API/HTTP vs. what `EXEC SQL` changes

For each program: **"API/HTTP today?"** = does it already call any API (all say No), and the exact
`EXEC SQL` parts that must be converted. Line numbers refer to the `.abap` files in `interface/`.

### B.1  ZMM_SQL_ASRS_SAP_PUSH  *(+ 6 identical siblings — see note)*
- **Purpose:** push pending SAP messages to the ASRS/WMS system.
- **API/HTTP today?** ❌ No — uses `EXEC SQL` only.
- **Parts that STAY (Open SQL on SAP tables):**
  - `SELECT * FROM zmm_dbcon_asrs` (read connection config)
  - `SELECT * FROM zmm_asrs WHERE msg_trans_type IN s_type AND trf_status = 'N'` (pick pending)
  - `MODIFY zmm_asrs FROM TABLE gt_asrs` (write back `trf_status = 'Y'`)
- **Parts that CHANGE (`EXEC SQL` → API):**
  | Line(s) | `EXEC SQL` today | Replace with |
  |---------|------------------|--------------|
  | 64–77 | `DISCONNECT` / `CONNECT TO` / `GET CONNECTION` / `SET CONNECTION` :DBCON | (removed — handled by destination) |
  | 91–150 | `INSERT INTO host_to_wms (30 cols) VALUES (…)` | **POST** to *ASRS – Push Message* API (see C-1) |
  | 167–169 | `DISCONNECT` | (removed) |

> **Note — the 7 `ZMM_SQL_ASRS_SAP_PUSH*` variants are structurally identical.** `_COR`
> (correction), `_DISPENSE`, `_IN` (inbound), `_MAT` (material), `_OUT` (outbound), `_PARFULL`
> (partial/full) differ only in the `MSG_TRANS_TYPE` they select and minor field mapping. **All 7
> insert into the same `HOST_TO_WMS` table** → all 7 call the **same** *ASRS – Push Message* API.

### B.2  ZMM_ASRS_SAP_INTERFACE
- **Purpose:** read message status back from ASRS and show it in an ALV monitor.
- **API/HTTP today?** ❌ No.
- **Parts that STAY:** `SELECT … FROM zmm_asrs`, `SELECT * FROM zmm_dbcon_asrs`, the ALV display.
- **Parts that CHANGE:**
  | Line(s) | `EXEC SQL` today | Replace with |
  |---------|------------------|--------------|
  | 133–143 | `DISCONNECT` / `CONNECT TO` / `SET CONNECTION` | (removed) |
  | 149–160 | `OPEN dbcur FOR SELECT MSG_ERR, MSG_STAT FROM HOST_TO_WMS WHERE MSG_REC_ID = :…` → `FETCH` → `CLOSE` | **GET** *ASRS – Get Message Status* API (see C-2) |
  | 189–191 | `DISCONNECT` | (removed) |

### B.3  ZMDM_RA_TRACKWISE
- **Purpose:** push new API-relevant material numbers to the RA-TrackWise system.
- **API/HTTP today?** ❌ No.
- **Parts that STAY:** `SELECT * FROM zcon_mdm`, `SELECT field1 FROM zmm_param`, `SELECT * FROM mara …`.
- **Parts that CHANGE:**
  | Line(s) | `EXEC SQL` today | Replace with |
  |---------|------------------|--------------|
  | 96–112 | `DISCONNECT` / `CONNECT TO` / `GET CONNECTION` / `SET CONNECTION` | (removed) |
  | 126–130 | `INSERT INTO MARA (MATNR) VALUES ( :gs_mara-matnr )` | **POST** *TrackWise – Push Material* API (see C-3) |
  | 143–145 | `DISCONNECT` | (removed) |

### B.4  ZQM_TRACKWISE  *(largest — 27 `EXEC SQL` blocks)*
- **Purpose:** push plant, material-detail and product-detail records to TrackWise for deviation processing.
- **API/HTTP today?** ❌ No.
- **Parts that STAY:** `SELECT * FROM zcon_mdm`, `SELECT SINGLE name1 FROM t001w`, the local
  `INSERT ztw_prod_det/ztw_mat_det FROM TABLE …` into SAP mirror tables.
- **Parts that CHANGE:**
  | Line(s) | `EXEC SQL` today | Replace with |
  |---------|------------------|--------------|
  | multiple | `DISCONNECT`/`CONNECT`/`GET`/`SET CONNECTION` (15 ops) | (removed) |
  | 131–173, 182–222 | `INSERT INTO ZTW_MAT_DET (…)` (2 branches) | **POST** *TrackWise – Push Material Detail* (C-4) |
  | 302–307 | `SELECT Plant_Code, Plant_Name FROM ZTW_PLNT_DET WHERE Plant_Code = :…` | **GET** *TrackWise – Check Plant* (C-5) |
  | 320–326 | `INSERT INTO ZTW_PLNT_DET (Plant_Code, Plant_Name) VALUES (…)` | **POST** *TrackWise – Push Plant* (C-6) |
  | 410–442, 450–480, 488–516, 525–555, 563–595, 603–637, 645–675, 683–715 | 8 × `INSERT INTO ZTW_PROD_DET (…)` | **POST** *TrackWise – Push Product Detail* (C-7) |

---

## Part C — API interface catalogue (for the CPI consultant)

Seven logical APIs cover all 82 `EXEC SQL` blocks. Method/verb is a recommendation; the exact
protocol (REST/JSON vs SOAP vs OData) is for the CPI + external-system teams to finalise. Payload
field structures are in **Part D**.

| # | API / iFlow name | Verb | Direction | Target | Called by | Payload (Part D) |
|---|------------------|------|-----------|--------|-----------|------------------|
| C-1 | ASRS – Push Message | POST | SAP → ASRS | `HOST_TO_WMS` | all 7 `ZMM_SQL_ASRS_SAP_PUSH*` | **D-1** |
| C-2 | ASRS – Get Message Status | GET | ASRS → SAP | `HOST_TO_WMS` | `ZMM_ASRS_SAP_INTERFACE` | **D-2** |
| C-3 | TrackWise – Push Material (RA/MDM) | POST | SAP → TrackWise | `MARA` (TW side) | `ZMDM_RA_TRACKWISE` | **D-3** |
| C-4 | TrackWise – Push Material Detail | POST | SAP → TrackWise | `ZTW_MAT_DET` | `ZQM_TRACKWISE` | **D-4** |
| C-5 | TrackWise – Check Plant | GET | TrackWise → SAP | `ZTW_PLNT_DET` | `ZQM_TRACKWISE` | **D-5** |
| C-6 | TrackWise – Push Plant | POST | SAP → TrackWise | `ZTW_PLNT_DET` | `ZQM_TRACKWISE` | **D-5** |
| C-7 | TrackWise – Push Product Detail | POST | SAP → TrackWise | `ZTW_PROD_DET` | `ZQM_TRACKWISE` | **D-6** |

**Cross-cutting requirements for CPI to design:**
- **Endpoints & auth** — base URLs per system + per plant (today the connection is chosen per
  `WERKS` from `ZMM_DBCON_ASRS`; the API layer must preserve that plant routing). Auth method
  (Basic / OAuth2 / client-cert) to be confirmed.
- **Error handling** — SAP currently wraps each `EXEC SQL` in `TRY … CATCH cx_sy_native_sql_error`;
  the API must return a clear success/failure so SAP can keep `trf_status = 'N'` for retry on
  failure and set `'Y'` only on success (see open item O-5).
- **Idempotency / dedup key** — e.g. `MSG_REC_ID` for ASRS messages, so re-sends don't duplicate.
- **Batch vs. single** — the push programs loop per record; CPI may offer a bulk endpoint to reduce
  round-trips (optional).

---

## Part D — Data structures (field-level) for API design

These are the exact fields moved in each `EXEC SQL` statement, taken from the program source. Where a
field maps to a SAP DDIC table, the SAP source is given so the client/CPI can pull the **exact data
type & length** from that table. **Items marked "confirm" need the client to supply the DDIC type.**

### D-1 — ASRS "Push Message" payload  *(external table `HOST_TO_WMS` — 30 fields)*
Source SAP staging table: **`ZMM_ASRS`** (structure `gs_asrs`). Types below are from the program's
own type declaration; confirm final lengths against `ZMM_ASRS`.

| # | External column (HOST_TO_WMS) | SAP source field | ABAP type / DDIC |
|---|-------------------------------|------------------|------------------|
| 1 | MSG_SRC | gs_asrs-MSG_SRC | ZMSG_SRC |
| 2 | MSG_REC_ID *(dedup key)* | gs_asrs-MSG_REC_ID | ZMSG_REC_ID |
| 3 | MSG_TRANS_TYPE | gs_asrs-MSG_TRANS_TYPE | ZMSG_TRANS_TYPE |
| 4 | MSG_ACTION | gs_asrs-MSG_ACTION | ZMSG_ACTION |
| 5 | MSG_RET_SRC | gs_asrs-MSG_RET_SRC | ZMSG_RET_SRC |
| 6 | MSG_RET_REC_ID | gs_asrs-MSG_RET_REC_ID | ZMSG_RET_REC_ID |
| 7 | MSG_RET_TRANS_ID | gs_asrs-MSG_RET_TRANS_ID | ZMSG_RET_TRANS_ID |
| 8 | MSG_DT_DEF | lv_date (from gs_asrs-MSG_DT_DEF) | Date, char10 formatted |
| 9 | MSG_DT_TRM | lv_date1 (from gs_asrs-MSG_DT_TRM) | Date, char10 formatted |
| 10 | MSG_ERR | gs_asrs-MSG_ERR | confirm (ZMM_ASRS) |
| 11 | MSG_ERR_DESC | gs_asrs-MSG_ERR_DESC | confirm (ZMM_ASRS) |
| 12 | MSG_STAT | gs_asrs-MSG_STAT | confirm (ZMM_ASRS) |
| 13 | GR_NO | gs_asrs-GR_NO | MBLNR |
| 14 | REQ_ID | gs_asrs-REQ_ID | ZREQ_ID |
| 15 | REQ_TYPE | gs_asrs-REQ_TYPE | ZREQ_TYPE |
| 16 | MAT_CODE | gs_asrs-MAT_CODE | MATNR |
| 17 | DESCRIPTION | gs_asrs-DESCRIPTION | confirm (ZMM_ASRS) |
| 18 | UOM | gs_asrs-UOM | confirm (ZMM_ASRS) |
| 19 | ITEM_TYPE | gs_asrs-ITEM_TYPE | confirm (ZMM_ASRS) |
| 20 | SAP_BATCH | gs_asrs-SAP_BATCH | CHARG_D |
| 21 | QTY | gs_asrs-QTY | ERFMG (quantity) |
| 22 | STATUS | gs_asrs-STATUS | ZSTAT |
| 23 | TOTAL_PACK | gs_asrs-TOTAL_PACK | QANZGEB |
| 24 | MFG_DATE | lv_date2 (from gs_asrs-MFG_DATE) | Date, char10 formatted |
| 25 | MANUFACTURER | gs_asrs-MANUFACTURER | ZMANUFACTURER |
| 26 | MFG_BATCH | gs_asrs-MFG_BATCH | ZMFG_BATCH |
| 27 | LINE_ITEM | gs_asrs-LINE_ITEM | MBLPO |
| 28 | PLANT | gs_asrs-PLANT | WERKS_D |
| 29 | OLD_STATUS | gs_asrs-OLD_STATUS | ZOLD_STATUS |
| — | *(MSG_RET_TRANS_ID etc. as above)* | | |

> Note: dates are sent as `char10` (`DD-MM-YYYY`-style) after ABAP formatting, not raw `DATS`.

### D-2 — ASRS "Get Message Status" (response)
- **Request:** `MSG_REC_ID` (key, = D-1 #2).
- **Response fields (read back into `ZMM_ASRS`):** `MSG_ERR`, `MSG_STAT`.

### D-3 — TrackWise "Push Material (RA/MDM)"
- **Single field:** `MATNR` (SAP `MARA-MATNR`, leading zeros stripped before send).

### D-4 — TrackWise "Push Material Detail" payload  *(external table `ZTW_MAT_DET` — 20 fields)*
Source SAP mirror table: **`ZTW_MAT_DET`** (structure `gs_mat_det`) — client to supply DDIC types.

| # | Column | Source field |
|---|--------|--------------|
| 1 | Mat_Code | gs_mat_det-Mat_Code |
| 2 | Mat_Desc | gs_mat_det-Mat_Desc |
| 3 | Batch_No | gs_mat_det-Batch_No |
| 4 | Plant_Code | gs_mat_det-Plant_Code |
| 5 | Mfg_Batch_No | gs_mat_det-Mfg_Batch_No |
| 6 | Ins_Lot_No | gs_mat_det-Ins_Lot_No |
| 7 | AR_No | gs_mat_det-AR_No |
| 8 | Vendor_Code | gs_mat_det-Vendor_Code |
| 9 | Vendor_Name | gs_mat_det-Vendor_Name |
| 10 | Mfg_Code | gs_mat_det-Mfg_Code |
| 11 | Mfg_Name | gs_mat_det-Mfg_Name |
| 12 | Date_of_Supply | lv_date (formatted `DD-MM-YYYY`; sent empty when blank) |
| 13 | Qty_Supplied | gs_mat_det-Qty_Supplied |
| 14 | UoM | gs_mat_det-UoM |
| 15 | Impacted_Prod_Code | gs_mat_det-Impacted_Prod_Code |
| 16 | Impa_Prod_Batch_No | gs_mat_det-Impa_Prod_Batch_No |
| 17 | Impacted_Prod_Name | gs_mat_det-Impacted_Prod_Name |
| 18 | Impa_Prod_Ver_No | gs_mat_det-Impa_Prod_Ver_No |
| 19 | Impacted_Ref_Doc_No | gs_mat_det-Impacted_Ref_Doc_No |
| 20 | Impa_Mfg_Batch_No | gs_mat_det-Impa_Mfg_Batch_No |

> The two `INSERT` branches (with / without `Date_of_Supply`) map to the **same** API — send
> `Date_of_Supply` empty/null when the date is blank.

### D-5 — TrackWise "Plant" (Check + Push)  *(external table `ZTW_PLNT_DET` — 2 fields)*
- **Check (GET):** request `Plant_Code` → response `Plant_Code`, `Plant_Name`.
- **Push (POST):** `Plant_Code` (= `gs_plnt_comb-werks`), `Plant_Name` (= `gv_name`, from `T001W-NAME1`).

### D-6 — TrackWise "Push Product Detail" payload  *(external table `ZTW_PROD_DET` — 15 fields)*
Source SAP mirror table: **`ZTW_PROD_DET`** (structure `gs_prod_det`) — client to supply DDIC types.

| # | Column | Source field |
|---|--------|--------------|
| 1 | Prod_Code | gs_prod_det-Prod_Code |
| 2 | Prod_Name | gs_prod_det-Prod_Name |
| 3 | Prod_Batch_No | gs_prod_det-Prod_Batch_No |
| 4 | Plant_Code | gs_prod_det-Plant_Code |
| 5 | Production_Ver_No | gs_prod_det-Production_Ver_No |
| 6 | Ref_Doc_No | gs_prod_det-Ref_Doc_No |
| 7 | Mfg_Batch_No | gs_prod_det-Mfg_Batch_No |
| 8 | Expiry_Date | lv_date1 (formatted; empty when blank) |
| 9 | Retest_Date | lv_date2 (formatted; empty when blank) |
| 10 | Impacted_Mat_Code | gs_prod_det-Impacted_Mat_Code |
| 11 | Impacted_Mat_Desc | gs_prod_det-Impacted_Mat_Desc |
| 12 | Impacted_Batch_No | gs_prod_det-Impacted_Batch_No |
| 13 | Impa_Mat_Insp_Lot | gs_prod_det-Impa_Mat_Insp_Lot |
| 14 | Impa_Mfg_Batch_No | gs_prod_det-Impa_Mfg_Batch_No |
| 15 | Impacted_Mat_AR_No | gs_prod_det-Impacted_Mat_AR_No |

> The **8** `INSERT INTO ZTW_PROD_DET` branches all use these **same 15 columns**; they differ only
> in whether `Expiry_Date` / `Retest_Date` are populated. → **one** API, dates optional.

---

## Part E — Open items to confirm (client / external-system teams)

| # | Item | Needed from |
|---|------|-------------|
| O-1 | Do ASRS/WMS and TrackWise already expose APIs? If yes, provide OpenAPI/WSDL + base URLs. If no, they must be built first. | External-system vendors |
| O-2 | Auth method per system (Basic / OAuth2 / cert) and per-plant endpoint routing (replaces per-`WERKS` `DBCON`). | Client / vendors |
| O-3 | S/4HANA target — on-premise or Cloud (clean-core)? Decides the ABAP HTTP client & whether a Communication Arrangement is required. | Client |
| O-4 | Exact DDIC types for fields marked "confirm" in D-1, and for tables `ZTW_MAT_DET`, `ZTW_PLNT_DET`, `ZTW_PROD_DET`, `ZMM_ASRS`, `ZMM_DBCON_ASRS`, `ZCON_MDM`. | Client (SAP DDIC) |
| O-5 | Retry/idempotency: on API failure keep `trf_status = 'N'`? Dedup keys (`MSG_REC_ID`, plant code)? | Client + CPI |
| O-6 | Payload format — JSON/REST, SOAP, or OData — and whether a bulk/batch endpoint is wanted. | CPI + vendors |

---

*Source programs and this spec are on branch `claude/abap-sql-api-migration-rgc6ht`, folder `interface/`.
Field lists are extracted directly from the program source; data types must be confirmed against the
live SAP DDIC where marked.*
