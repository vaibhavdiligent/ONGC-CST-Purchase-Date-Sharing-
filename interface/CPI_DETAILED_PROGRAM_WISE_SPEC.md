# Detailed Program-wise Specification — SQL → API Conversion (ASRS / TrackWise)

**Audience:** SAP ABAP team + CPI (SAP Integration Suite) team
**Purpose:** For each program, show the **current SQL**, the **exact part to convert to an API**, and
the **field-level structure (name / type / length)** the CPI team needs to build the interface.
**Programs:** 10 (source `.abap` files in `interface/`).

> **How to read this document**
> - **Current SQL** = the code as it runs today (copied from the program source, line numbers shown).
> - **Convert to API** = the `EXEC SQL` block that must become an HTTP/API call.
> - **Field length** column: standard SAP fields are pre-filled; fields on custom (Z) tables are marked
>   **⟨CONFIRM⟩** — these are filled once the DDIC structures are supplied (see §2 and §6).

---

## 1. The SQL connection — which tables define it (please send these)

All 10 programs connect to the external DB the same way: they read a **connection name** from a
custom config table, then issue `EXEC SQL. CONNECT TO :dbcon`. That connection name is defined in the
**standard SAP `DBCON` table** (maintained via transaction **DBCO / SM59 secondary DB**). To convert
this to an API layer, the CPI/BASIS team needs the contents of these:

| Table | Type | Role in the SQL connection | Used by |
|-------|------|----------------------------|---------|
| **`ZMM_DBCON_ASRS`** | Custom Z-table | Holds the ASRS DB connection name **per plant (`WERKS`)**. Program reads `gs_con-DBCON`. | `ZMM_ASRS_SAP_INTERFACE`, all 7 `ZMM_SQL_ASRS_SAP_PUSH*` |
| **`ZCON_MDM`** | Custom Z-table | Holds the TrackWise/MDM DB connection name. Program reads `gs_con-DBCON`. | `ZMDM_RA_TRACKWISE`, `ZQM_TRACKWISE` |
| **`DBCON`** (standard) | SAP system table (DBCO) | The **actual** secondary-connection definition the name above points to: DB type, server, user, password. | (referenced as `dbcon-con_name`) |

> **✅ KEY FACT (from the DBCON entries below): the external systems are Microsoft SQL Server
> databases (`DBMS = MSS`).** Today SAP writes directly into these SQL Server DBs via native SQL.
> The API layer must front these same SQL Server databases.

**1.1 `ZMM_DBCON_ASRS` — structure (received)** — *"SAP-ASRS VS Plant connection Table"*

| Field | Key | Data element | Type | Len | Description |
|-------|:---:|--------------|------|----:|-------------|
| MANDT | ✔ | MANDT | CLNT | 3 | Client |
| WERKS | ✔ | WERKS_D | CHAR | 4 | Plant |
| DBCON | | DBCON_NAME | CHAR | 30 | Logical DB connection name (→ `DBCON.CON_NAME`) |
| DESCRIPTION | | /GRCPI/GRIA_DESC | CHAR | 132 | Description |
| VALUE1 | | CHAR50 | CHAR | 50 | Comment |
| VALUE2 | | CHAR50 | CHAR | 50 | Comment |
| ERFUSER / ERFDATE / ERFTIME | | UNAME/DATUM/UZEIT | CHAR/DATS/TIMS | 12/8/6 | Created-by (audit) |
| AENUSER / AENDATE / AENTIME | | UNAME/DATUM/UZEIT | CHAR/DATS/TIMS | 12/8/6 | Changed-by (audit) |

Sample rows: `1047 → CON_1047` (CIPLA/GOA-VII PD II/PHARMA – ASRS, prod plant) · `1048 → CON_1048` (Sikkim ASRS).

**1.2 `ZCON_MDM` — structure (received)** — TrackWise/MDM connection (single connection, no plant key)

| Field | Key | Data element | Type | Len | Description |
|-------|:---:|--------------|------|----:|-------------|
| MANDT | ✔ | MANDT | CLNT | 3 | Client |
| DBCON | ✔ | DBCON_NAME | CHAR | 30 | Logical DB connection name (→ `DBCON.CON_NAME`) |
| DESCRIPTION | | /GRCPI/GRIA_DESC | CHAR | 132 | Description |
| VALUE1 / VALUE2 | | CHAR50 | CHAR | 50 | Comment |
| ERFUSER…AENTIME | | (audit) | | | Created/Changed-by |

Sample row: `CON_MDM` → "Database Connection name for RA TrackWise Master Integration".

**1.3 `DBCON` (standard) — the actual connection definitions (received)**

| Field | Key | Data element | Type | Len | Description |
|-------|:---:|--------------|------|----:|-------------|
| CON_NAME | ✔ | DBCON_NAME | CHAR | 30 | Logical DB connection name |
| DBMS | | DBCON_DBMS | CHAR | 3 | Database system (**MSS** = MS SQL Server) |
| USER_NAME | | DBCON_UID | CHAR | 64 | DB user |
| PASSWORD | | DBCON_PWD | CHAR | 255 | DB password |
| CON_ENV | | DBCON_ENV | CHAR | 1024 | Connection info (server / DB name / port) |
| DB_RECO | | DBCON_RECO | CHAR | 1 | Availability type |
| MAX_CONNECTIONS / OPT_CONNECTIONS | | INT1 | INT1 | 3 | Pool sizing |

**Connection entries in use (→ become the API endpoints/destinations):**

| CON_NAME | Plant | DBMS | SQL Server (host\instance,port) | Database | System |
|----------|-------|------|----------------------------------|----------|--------|
| CON_1047 | 1047 (Goa) | MSS | tcp:172.18.11.120 | CIG_EFAWMS | ASRS/WMS |
| CON_1048 | 1048 (Sikkim) | MSS | tcp:10.27.1.27 | CIS_EFAWMS | ASRS/WMS |
| CON_MDM | (all) | MSS | tcp:INCPLTWPRDDB01\TWPRD,55619 | TW_SAP | TrackWise |

**In the target design these tables are replaced by:** an **HTTP/RFC destination** (on-prem:
SM59 + `CL_HTTP_DESTINATION_PROVIDER`) or a **Communication Arrangement / Outbound Service**
(S/4HANA Cloud). The **per-plant routing stays** — the program still reads `ZMM_DBCON_ASRS` by
`WERKS`, but the value now resolves to a destination (e.g. `CON_1047` → ASRS-Goa endpoint) instead
of a SQL Server connection. `ZCON_MDM` resolves to the single TrackWise endpoint.

---

## 2. What I still need to fill in all field lengths

Field lengths for **standard SAP** data elements are filled below. For **custom** tables I have the
field *names* from the code but not the DDIC type/length. Please send the DDIC structure (field,
data element, type, length, decimals) of:

- ~~`ZMM_ASRS`~~ — ✅ **RECEIVED** (§3.1c complete; `interface/DDIC_ZMM_ASRS.pdf`)
- ~~`ZTW_MAT_DET`~~ — ✅ **RECEIVED** (§3.5c complete; `interface/DDIC_ZTW_MAT_DET.pdf`)
- ~~`ZTW_PROD_DET`~~ — ✅ **RECEIVED** (§3.5e complete; `interface/DDIC_ZTW_PROD_DET.pdf`)
- ~~`ZMM_DBCON_ASRS`, `ZCON_MDM`, `DBCON`~~ — ✅ **RECEIVED** (§1)
- ~~`ZMM_PARAM`~~ — ✅ **RECEIVED**: `SRNO` NUMC 4 (key), `PARAM_TYPE` CHAR 100, `FIELD1..FIELD10` CHAR 300 each. → the `field1` read in ZMDM_RA_TRACKWISE is **CHAR 300**. (Open-SQL read; stays as-is.)
- `ZTW_PLNT_DET` — **not a SAP table** (external TrackWise DB only; §3.5d). SAP-side source lengths are known; external column lengths are owned by TrackWise/CPI.

**All SAP-side structures are now in.** Nothing further is needed to finalise the field lengths.

### 2.1 Where each external table lives (important — do not hunt for these in SAP DDIC)
| External table (via `EXEC SQL`) | Exists in SAP DDIC? | Structure source |
|---------------------------------|:-------------------:|------------------|
| `HOST_TO_WMS` (ASRS) | ❌ No | External ASRS/WMS SQL Server DB. SAP-side payload mirrors `ZMM_ASRS` (§3.1c). |
| `MARA` (TrackWise side) | ❌ (this is the TW copy) | External TrackWise DB. SAP sends only `MATNR`. |
| `ZTW_PLNT_DET` | ❌ No | External TrackWise DB only. |
| `ZTW_MAT_DET` | ✅ Yes (also a SAP mirror) | SAP DDIC = payload source (§3.5c). |
| `ZTW_PROD_DET` | ✅ Yes (also a SAP mirror) | SAP DDIC = payload source (§3.5e). |

---

## 3. Program details

Each program below: purpose → connection used → **current SQL** → **convert-to-API** → **field structure**.

---

### 3.1  ZMM_SQL_ASRS_SAP_PUSH  → API **C-1 "ASRS Push Message"**
**Purpose:** send pending SAP messages to the ASRS/WMS system.
**Connection table:** `ZMM_DBCON_ASRS` (per plant) → `DBCON`.

#### (a) Stays as-is — Open SQL on SAP tables (no change)
```abap
26  SELECT * FROM zmm_dbcon_asrs INTO TABLE gt_con.              " read connection config
43  SELECT * FROM zmm_asrs INTO CORRESPONDING FIELDS OF TABLE gt_asrs
46      WHERE msg_trans_type IN s_type AND trf_status = 'N'.     " pick pending messages
171 MODIFY zmm_asrs FROM TABLE gt_asrs.                          " write back trf_status = 'Y'
```

#### (b) Current SQL to CONVERT — connection + insert into external DB
```abap
64  EXEC SQL. DISCONNECT :con        ENDEXEC.   " ← remove (destination handles session)
69  EXEC SQL. CONNECT TO :gs_con-DBCON ENDEXEC. " ← remove
72  EXEC SQL. GET CONNECTION :CON      ENDEXEC. " ← remove
75  EXEC SQL. SET CONNECTION :gs_con-DBCON ENDEXEC. " ← remove
91  EXEC SQL.
92    INSERT INTO host_to_wms ( MSG_SRC, MSG_REC_ID, MSG_TRANS_TYPE, MSG_ACTION,
        MSG_RET_SRC, MSG_RET_REC_ID, MSG_RET_TRANS_ID, MSG_DT_DEF, MSG_DT_TRM,
        MSG_ERR, MSG_ERR_DESC, MSG_STAT, GR_NO, REQ_ID, REQ_TYPE, MAT_CODE,
        DESCRIPTION, UOM, ITEM_TYPE, SAP_BATCH, QTY, STATUS, TOTAL_PACK, MFG_DATE,
        MANUFACTURER, MFG_BATCH, LINE_ITEM, PLANT, OLD_STATUS )
      VALUES ( :gs_asrs-MSG_SRC, :gs_asrs-MSG_REC_ID, ... :gs_asrs-OLD_STATUS )
150 ENDEXEC.                                     " ← replace with POST to C-1
167 EXEC SQL. DISCONNECT :con ENDEXEC.           " ← remove
```
**Convert to:** `POST` **C-1 ASRS Push Message** with the body in §3.1(c), once per record in the loop.
On HTTP 2xx → set `trf_status = 'Y'`; on error → leave `'N'` for retry (today handled by
`CATCH cx_sy_native_sql_error`).

#### (c) Field structure — API C-1 payload (external table `HOST_TO_WMS`, **29 fields**) ✅ lengths final
SAP source structure: **`ZMM_ASRS`** (`gs_asrs`, DDIC in `interface/DDIC_ZMM_ASRS.pdf`). The INSERT
sends 29 of the 32 `ZMM_ASRS` fields (excludes `MANDT`, `TRF_STATUS`, `DATUM` which are SAP-side only).

| # | API field / ext column | SAP source | Data element | Type | Length | Notes |
|---|------------------------|-----------|--------------|------|--------|-------|
| 1 | MSG_SRC | gs_asrs-MSG_SRC | ZMSG_SRC | CHAR | 20 | "HOST" |
| 2 | MSG_REC_ID | gs_asrs-MSG_REC_ID | ZMSG_REC_ID | CHAR | 20 | **key / dedup — unique number** |
| 3 | MSG_TRANS_TYPE | gs_asrs-MSG_TRANS_TYPE | ZMSG_TRANS_TYPE | CHAR | 20 | domain values ↓ |
| 4 | MSG_ACTION | gs_asrs-MSG_ACTION | ZMSG_ACTION | CHAR | 3 | action |
| 5 | MSG_RET_SRC | gs_asrs-MSG_RET_SRC | ZMSG_RET_SRC | CHAR | 20 | return source |
| 6 | MSG_RET_REC_ID | gs_asrs-MSG_RET_REC_ID | ZMSG_RET_REC_ID | NUMC | 9 | numeric |
| 7 | MSG_RET_TRANS_ID | gs_asrs-MSG_RET_TRANS_ID | ZMSG_RET_TRANS_ID | NUMC | 9 | numeric |
| 8 | MSG_DT_DEF | lv_date ← gs_asrs-MSG_DT_DEF | DATUM (DATS 8) | CHAR | 10 | date sent as `DD-MM-YYYY` string |
| 9 | MSG_DT_TRM | lv_date1 ← gs_asrs-MSG_DT_TRM | DATUM (DATS 8) | CHAR | 10 | date string; empty if blank |
| 10 | MSG_ERR | gs_asrs-MSG_ERR | ZMSG_ERR | NUMC | 3 | numeric error code |
| 11 | MSG_ERR_DESC | gs_asrs-MSG_ERR_DESC | ZMSG_ERR_DESC | CHAR | 80 | |
| 12 | MSG_STAT | gs_asrs-MSG_STAT | ZMSG_STAT | CHAR | 3 | message state |
| 13 | GR_NO | gs_asrs-GR_NO | MBLNR | CHAR | 10 | material doc no. (ALPHA) |
| 14 | REQ_ID | gs_asrs-REQ_ID | ZREQ_ID | CHAR | 12 | |
| 15 | REQ_TYPE | gs_asrs-REQ_TYPE | ZREQ_TYPE | CHAR | 50 | domain: F/H/M/O/P/R |
| 16 | MAT_CODE | gs_asrs-MAT_CODE | MATNR | CHAR | 40 | material (MATN1) |
| 17 | DESCRIPTION | gs_asrs-DESCRIPTION | CHAR200 | CHAR | 200 | |
| 18 | UOM | gs_asrs-UOM | MEINS | UNIT | 3 | base UoM (CUNIT) |
| 19 | ITEM_TYPE | gs_asrs-ITEM_TYPE | ZITEM_TYPE | CHAR | 3 | |
| 20 | SAP_BATCH | gs_asrs-SAP_BATCH | CHARG_D | CHAR | 10 | batch |
| 21 | QTY | gs_asrs-QTY | ERFMG | QUAN | 13 (3 dec) | qty in entry unit (ref field = UOM) |
| 22 | STATUS | gs_asrs-STATUS | ZSTAT | CHAR | 30 | domain ZSTATS: APP/BLK/QUA |
| 23 | TOTAL_PACK | gs_asrs-TOTAL_PACK | QANZGEB | QUAN | 6 (0 dec) | no. of containers |
| 24 | MFG_DATE | lv_date2 ← gs_asrs-MFG_DATE | ZMFG_DATE (DATS 8) | CHAR | 10 | date string |
| 25 | MANUFACTURER | gs_asrs-MANUFACTURER | ZMANUFACTURER | CHAR | 30 | |
| 26 | MFG_BATCH | gs_asrs-MFG_BATCH | ZMFG_BATCH | CHAR | 30 | |
| 27 | LINE_ITEM | gs_asrs-LINE_ITEM | MBLPO | NUMC | 4 | item no. |
| 28 | PLANT | gs_asrs-PLANT | WERKS_D | CHAR | 4 | plant |
| 29 | OLD_STATUS | gs_asrs-OLD_STATUS | ZOLD_STATUS | CHAR | 30 | domain ZSTATS: APP/BLK/QUA |

> **`MSG_TRANS_TYPE` allowed values (domain `ZMSG_TRANS_TYPE`):** `COR` Correction · `DIS` Dispense ·
> `FSCR` Full Status Change · `IN` Store IN · `MASTER` Master List · `OUT` Store OUT ·
> `PSCR` Partial Status Change · `PSCR_IN` Partial Status Store In. **These map 1:1 to the
> `ZMM_SQL_ASRS_SAP_PUSH*` variants** — the CPI API should accept/validate this enumerated set.
> **`REQ_TYPE` domain `ZREQ_TYPE`:** F Full doc · H Partial doc · M Material doc · O OBD · P Process order · R Reservation.

---

### 3.2  ZMM_SQL_ASRS_SAP_PUSH — the 6 sibling variants
`_COR`, `_DISPENSE`, `_IN`, `_MAT`, `_OUT`, `_PARFULL` are **structurally identical** to §3.1: same
config table, same `HOST_TO_WMS` insert, same connect/disconnect. **They all call the same API C-1.**
Only the selected `MSG_TRANS_TYPE` and small field-mapping details differ.

| Variant | Business meaning (transaction type) | INSERT target | API |
|---------|--------------------------------------|---------------|-----|
| ZMM_SQL_ASRS_SAP_PUSH | Generic push | HOST_TO_WMS | C-1 |
| ZMM_SQL_ASRS_SAP_PUSH_COR | Correction | HOST_TO_WMS | C-1 |
| ZMM_SQL_ASRS_SAP_PUSH_DISPENSE | Dispense | HOST_TO_WMS | C-1 |
| ZMM_SQL_ASRS_SAP_PUSH_IN | Inbound / goods receipt | HOST_TO_WMS | C-1 |
| ZMM_SQL_ASRS_SAP_PUSH_MAT | Material | HOST_TO_WMS | C-1 |
| ZMM_SQL_ASRS_SAP_PUSH_OUT | Outbound / goods issue | HOST_TO_WMS | C-1 |
| ZMM_SQL_ASRS_SAP_PUSH_PARFULL | Partial / full | HOST_TO_WMS | C-1 |

*(No separate field tables needed — payload = §3.1(c).)*

---

### 3.3  ZMM_ASRS_SAP_INTERFACE  → API **C-2 "ASRS Get Message Status"**
**Purpose:** read message status back from ASRS and show it in an ALV monitor.
**Connection table:** `ZMM_DBCON_ASRS` (per plant).

#### (a) Stays as-is
```abap
75  SELECT msg_src ... FROM zmm_asrs INTO CORRESPONDING FIELDS OF TABLE gt_asrs
98      WHERE msg_dt_def IN p_date AND plant = s_werks.
101 SELECT * FROM zmm_dbcon_asrs INTO TABLE gt_con WHERE werks = s_werks.
    " + all ALV display logic (FORM display_alv)
```

#### (b) Current SQL to CONVERT — read status from external DB via cursor
```abap
133 EXEC SQL. DISCONNECT :con           ENDEXEC.  " ← remove
138 EXEC SQL. CONNECT TO :gs_con-DBCON  ENDEXEC.  " ← remove
141 EXEC SQL. SET CONNECTION :gs_con-DBCON ENDEXEC." ← remove
149 EXEC SQL.
150   OPEN dbcur FOR
151   SELECT MSG_ERR, MSG_STAT FROM HOST_TO_WMS
153     WHERE MSG_REC_ID = :ls_asrs-MSG_REC_ID
154 ENDEXEC.
155 EXEC SQL. FETCH NEXT dbcur INTO :ls_asrs-MSG_ERR, :ls_asrs-MSG_STAT ENDEXEC.
158 EXEC SQL. close dbcur ENDEXEC.               " ← the OPEN/FETCH/CLOSE triple → one GET (C-2)
189 EXEC SQL. DISCONNECT :con ENDEXEC.           " ← remove
```
**Convert to:** `GET` **C-2** for each `MSG_REC_ID` in the loop; map the response back into
`ls_asrs-MSG_ERR` / `ls_asrs-MSG_STAT`, then the existing ALV logic is unchanged.

#### (c) Field structure — API C-2 ✅ lengths final
| Direction | Field | SAP target | Data element | Type | Length |
|-----------|-------|-----------|--------------|------|--------|
| **Request** | MSG_REC_ID | ls_asrs-MSG_REC_ID | ZMSG_REC_ID | CHAR | 20 |
| **Response** | MSG_ERR | ls_asrs-MSG_ERR | ZMSG_ERR | NUMC | 3 |
| **Response** | MSG_STAT | ls_asrs-MSG_STAT | ZMSG_STAT | CHAR | 3 |

---

### 3.4  ZMDM_RA_TRACKWISE  → API **C-3 "TrackWise Push Material"**
**Purpose:** push new API-relevant material numbers to RA-TrackWise.
**Connection table:** `ZCON_MDM` → `DBCON`.

#### (a) Stays as-is
```abap
47  SELECT * FROM zcon_mdm CLIENT SPECIFIED INTO TABLE gt_con WHERE mandt = sy-mandt.
57  SELECT field1 FROM zmm_param CLIENT SPECIFIED UP TO 1 ROWS INTO g_field1
        WHERE mandt = sy-mandt AND param_type = 'ZZ9' ORDER BY srno. ENDSELECT.
62  SELECT * FROM mara INTO TABLE gt_mara
        WHERE ersda IN s_ersda AND mtart = 'ZAPI' AND mstae NOT IN (g_field1).
```

#### (b) Current SQL to CONVERT
```abap
96  EXEC SQL. DISCONNECT :con           ENDEXEC.  " ← remove
102 EXEC SQL. CONNECT TO :gs_con-DBCON  ENDEXEC.  " ← remove
106 EXEC SQL. GET CONNECTION :CON       ENDEXEC.  " ← remove
110 EXEC SQL. SET CONNECTION :gs_con-DBCON ENDEXEC." ← remove
123 SHIFT gs_mara-matnr LEFT DELETING LEADING '0'. " keep (payload prep)
126 EXEC SQL.
127   INSERT INTO MARA (MATNR) VALUES ( :gs_mara-matnr )
130 ENDEXEC.                                       " ← replace with POST to C-3
143 EXEC SQL. DISCONNECT :con ENDEXEC.             " ← remove
```
**Convert to:** `POST` **C-3** per material.

#### (c) Field structure — API C-3
| API field | SAP source | Data element | Type | Length | Notes |
|-----------|-----------|--------------|------|--------|-------|
| MATNR | gs_mara-MATNR | MATNR | CHAR | 40 | leading zeros stripped before send |

---

### 3.5  ZQM_TRACKWISE  → APIs **C-4, C-5, C-6, C-7**
**Purpose:** push plant, material-detail and product-detail records to TrackWise.
**Connection table:** `ZCON_MDM` → `DBCON`. (27 `EXEC SQL` blocks — the substantive ones below.)

#### (a) Stays as-is
```abap
71/267 SELECT * FROM zcon_mdm CLIENT SPECIFIED INTO TABLE gt_con WHERE mandt = sy-mandt.
317    SELECT SINGLE name1 FROM t001w INTO gv_name WHERE werks = p_gv_qmel-mawerk.
89/245 INSERT ztw_prod_det / ztw_mat_det FROM TABLE ...   " Open SQL into SAP mirror tables (keep)
```

#### (b) Current SQL to CONVERT
```abap
" material detail (2 branches, with/without Date_of_Supply) → API C-4
131 EXEC SQL. INSERT INTO ZTW_MAT_DET (Mat_Code, ... Impa_Mfg_Batch_No) VALUES (...) ENDEXEC.
182 EXEC SQL. INSERT INTO ZTW_MAT_DET (... same, minus Date_of_Supply ...) VALUES (...) ENDEXEC.

" plant existence check → API C-5
302 EXEC SQL. SELECT Plant_Code, Plant_Name INTO :wa_plant FROM ZTW_PLNT_DET
              WHERE Plant_Code = :gv_plant ENDEXEC.

" plant create → API C-6
320 EXEC SQL. INSERT INTO ZTW_PLNT_DET (Plant_Code, Plant_Name) VALUES (:gs_plnt_comb-werks, :gv_Name) ENDEXEC.

" product detail (8 branches by date combination) → API C-7
410/450/488/525/563/603/645/683  EXEC SQL. INSERT INTO ZTW_PROD_DET (Prod_Code, ...) VALUES (...) ENDEXEC.

" connection mgmt (15 ops) → all removed
    EXEC SQL. CONNECT TO / SET CONNECTION / GET CONNECTION / DISCONNECT :con ENDEXEC.
```

#### (c) Field structure — API **C-4 "Push Material Detail"** (`ZTW_MAT_DET`, **20 fields**) ✅ lengths final
Source: `gs_mat_det` (DDIC in `interface/DDIC_ZTW_MAT_DET.pdf`). The INSERT sends 20 of the 26 fields
(excludes `MANDT`, `SERIAL_NO` key, and `STATUS`/`CREATEDATE`/`CREATETIME`/`CREATEDBY` audit fields).

| # | API field | Source field | Data element | Type | Length |
|---|-----------|--------------|--------------|------|--------|
| 1 | Mat_Code | gs_mat_det-Mat_Code | MATNR | CHAR | 40 |
| 2 | Mat_Desc | gs_mat_det-Mat_Desc | ZMAKTX | CHAR | 200 |
| 3 | Batch_No | gs_mat_det-Batch_No | CHARG_D | CHAR | 10 |
| 4 | Plant_Code | gs_mat_det-Plant_Code | CHAR5 | CHAR | 5 |
| 5 | Mfg_Batch_No | gs_mat_det-Mfg_Batch_No | — | CHAR | 30 |
| 6 | Ins_Lot_No | gs_mat_det-Ins_Lot_No | QPLOS | NUMC | 12 |
| 7 | AR_No | gs_mat_det-AR_No | ZAR_NO | CHAR | 15 |
| 8 | Vendor_Code | gs_mat_det-Vendor_Code | LIFNR | CHAR | 10 |
| 9 | Vendor_Name | gs_mat_det-Vendor_Name | — | CHAR | 72 |
| 10 | Mfg_Code | gs_mat_det-Mfg_Code | ZMANUFACTURER | CHAR | 30 |
| 11 | Mfg_Name | gs_mat_det-Mfg_Name | — | CHAR | 72 |
| 12 | Date_of_Supply | lv_date ← gs_mat_det-Date_of_Supply | CPUDT (DATS 8) | CHAR | 10 (date string; empty when blank) |
| 13 | Qty_Supplied | gs_mat_det-Qty_Supplied | MENGE_D | QUAN | 13 (3 dec) |
| 14 | UoM | gs_mat_det-UoM | MEINS | UNIT | 3 |
| 15 | Impacted_Prod_Code | gs_mat_det-Impacted_Prod_Code | MATNR | CHAR | 40 |
| 16 | Impa_Prod_Batch_No | gs_mat_det-Impa_Prod_Batch_No | — | CHAR | 30 |
| 17 | Impacted_Prod_Name | gs_mat_det-Impacted_Prod_Name | ZMAKTX | CHAR | 200 |
| 18 | Impa_Prod_Ver_No | gs_mat_det-Impa_Prod_Ver_No | VERID | CHAR | 4 |
| 19 | Impacted_Ref_Doc_No | gs_mat_det-Impacted_Ref_Doc_No | — | CHAR | 300 |
| 20 | Impa_Mfg_Batch_No | gs_mat_det-Impa_Mfg_Batch_No | — | CHAR | 30 |

> Both material-detail INSERT branches → **one** API C-4; send `Date_of_Supply` empty when blank.

#### (d) Field structure — APIs **C-5 (check) / C-6 (create)** plant (`ZTW_PLNT_DET`, 2 fields)
> ⚠️ **`ZTW_PLNT_DET` is NOT a SAP table** — it exists **only on the external TrackWise SQL Server
> database**. The program reaches it exclusively via `EXEC SQL`. So its column lengths are owned by the
> **TrackWise/CPI side**, not SAP DDIC. The **SAP-side source** lengths (what the program supplies) are
> known and shown below: `Plant_Code` is typed `ztw_prod_det-plant_code` = **CHAR 5**; `Plant_Name`
> comes from `T001W-NAME1` = **CHAR 30**.

| API | Direction | Field | SAP source | Type | Length (SAP side) |
|-----|-----------|-------|------------|------|-------------------|
| C-5 | request | Plant_Code | gv_plant (ztw_prod_det-plant_code) | CHAR | 5 |
| C-5 | response | Plant_Code / Plant_Name | wa_plant | CHAR | 5 / (TrackWise-defined) |
| C-6 | body | Plant_Code | gs_plnt_comb-werks | CHAR | 5 |
| C-6 | body | Plant_Name | gv_Name (← T001W-NAME1) | CHAR | 30 |

#### (e) Field structure — API **C-7 "Push Product Detail"** (`ZTW_PROD_DET`, **15 fields**) ✅ lengths final
Source: `gs_prod_det` (DDIC in `interface/DDIC_ZTW_PROD_DET.pdf`). The INSERT sends 15 of the 22 fields
(excludes `MANDT`, `SERIAL_NO` key, `MFG_DATE`, and `STATUS`/`CREATEDATE`/`CREATETIME`/`CREATEDBY`).

| # | API field | Source field | Data element | Type | Length |
|---|-----------|--------------|--------------|------|--------|
| 1 | Prod_Code | gs_prod_det-Prod_Code | MATNR | CHAR | 40 |
| 2 | Prod_Name | gs_prod_det-Prod_Name | ZMAKTX | CHAR | 200 |
| 3 | Prod_Batch_No | gs_prod_det-Prod_Batch_No | CHARG_D | CHAR | 10 |
| 4 | Plant_Code | gs_prod_det-Plant_Code | CHAR5 | CHAR | 5 |
| 5 | Production_Ver_No | gs_prod_det-Production_Ver_No | VERID | CHAR | 4 |
| 6 | Ref_Doc_No | gs_prod_det-Ref_Doc_No | — | CHAR | 300 |
| 7 | Mfg_Batch_No | gs_prod_det-Mfg_Batch_No | — | CHAR | 30 |
| 8 | Expiry_Date | lv_date1 ← Expiry_Date | VFDAT (DATS 8) | CHAR | 10 (date string; empty when blank) |
| 9 | Retest_Date | lv_date2 ← Retest_Date | ZRETEST_TRACK_DATE | CHAR | 10 (already char; empty when blank) |
| 10 | Impacted_Mat_Code | gs_prod_det-Impacted_Mat_Code | IDNRK | CHAR | 40 |
| 11 | Impacted_Mat_Desc | gs_prod_det-Impacted_Mat_Desc | ZMAKTX | CHAR | 200 |
| 12 | Impacted_Batch_No | gs_prod_det-Impacted_Batch_No | CHARG_D | CHAR | 10 |
| 13 | Impa_Mat_Insp_Lot | gs_prod_det-Impa_Mat_Insp_Lot | QPLOS | NUMC | 12 |
| 14 | Impa_Mfg_Batch_No | gs_prod_det-Impa_Mfg_Batch_No | — | CHAR | 30 |
| 15 | Impacted_Mat_AR_No | gs_prod_det-Impacted_Mat_AR_No | ZAR_NO | CHAR | 15 |

> All 8 product-detail INSERT branches → **one** API C-7; dates optional.

---

## 4. API summary for the CPI team

| API | Verb | Direction | External object | Payload (fields) | Called by |
|-----|------|-----------|-----------------|------------------|-----------|
| C-1 ASRS Push Message | POST | SAP → ASRS | HOST_TO_WMS | 29 (§3.1c) | 7 × ZMM_SQL_ASRS_SAP_PUSH* |
| C-2 ASRS Get Msg Status | GET | ASRS → SAP | HOST_TO_WMS | 1 req / 2 resp (§3.3c) | ZMM_ASRS_SAP_INTERFACE |
| C-3 TrackWise Push Material | POST | SAP → TrackWise | MARA (TW) | 1 (§3.4c) | ZMDM_RA_TRACKWISE |
| C-4 TrackWise Push Material Detail | POST | SAP → TrackWise | ZTW_MAT_DET | 20 (§3.5c) | ZQM_TRACKWISE |
| C-5 TrackWise Check Plant | GET | TrackWise → SAP | ZTW_PLNT_DET | 1 req / 2 resp (§3.5d) | ZQM_TRACKWISE |
| C-6 TrackWise Push Plant | POST | SAP → TrackWise | ZTW_PLNT_DET | 2 (§3.5d) | ZQM_TRACKWISE |
| C-7 TrackWise Push Product Detail | POST | SAP → TrackWise | ZTW_PROD_DET | 15 (§3.5e) | ZQM_TRACKWISE |

**Also needed from the external-system / CPI side:** endpoint URLs (per plant), auth method
(Basic/OAuth2/cert), payload format (JSON/SOAP/OData), success/error response contract, and any
idempotency key (`MSG_REC_ID`, `Plant_Code`).

### 4.1 Does an API already exist on the ASRS / TrackWise side? (evidence from code)
**From the SAP code: today there is NO API — the integration is pure database-level.** Verified:
- The 10 programs contain **zero** HTTP/OData/REST/SOAP/RFC calls (no `cl_http*`, `if_web_http*`,
  `CALL FUNCTION … DESTINATION`, proxy, etc.). 100% is `EXEC SQL` (see each program in §3).
- The `DBCON` entries (§1) connect at the **raw MS SQL Server level**
  (`MSSQL_SERVER=tcp:…, MSSQL_DBNAME=…`) — SAP writes directly into the external DB tables, which is
  a classic "database-drop" integration, not an API integration.
- Whether the ASRS/WMS and TrackWise **applications** *also* expose a REST/OData/SOAP API cannot be
  known from SAP code (SAP never used one) — that must be confirmed with the WMS vendor and the
  TrackWise (Sparta/Honeywell) administrator.

**Two viable target designs (recommend option A — no change on the external side):**

| | Option A — CPI JDBC (recommended) | Option B — native API on external side |
|---|-----------------------------------|----------------------------------------|
| Flow | ABAP → OData/REST → **CPI iFlow** → **JDBC adapter** → same SQL Server tables (`HOST_TO_WMS`, `ZTW_*`) | ABAP → OData/REST → CPI → **new REST/SOAP API the ASRS/TrackWise vendor builds** |
| External system change | **None** — CPI writes the same rows SAP used to write | Vendor must build & host an API |
| SAP change | Remove `EXEC SQL`, call CPI (clean-core ✅) | Same |
| Best when | The external DBs stay as the integration point | The vendors already have / will build APIs |

> **Bottom line:** you do **not** need the ASRS/TrackWise vendors to build APIs to get SAP clean-core.
> CPI's **MS SQL Server JDBC adapter** can perform the exact same inserts/reads into
> `HOST_TO_WMS` / `ZTW_MAT_DET` / `ZTW_PROD_DET` / `ZTW_PLNT_DET`, so only the SAP↔CPI hop becomes an
> API. Confirm with the CPI team which option the client prefers.

---

## 5. Common conversion rules (apply to every program)
1. `SELECT ... FROM <config/connection table>` → **keep** (Open SQL on SAP tables).
2. `CONNECT TO` / `SET CONNECTION` / `GET CONNECTION` / `DISCONNECT` → **delete** (destination handles it).
3. `EXEC SQL. INSERT INTO <ext_table> ...` → **POST** to the matching API.
4. `EXEC SQL. OPEN/FETCH/CLOSE` or `SELECT FROM <ext_table>` → **GET** the matching API.
5. Keep the surrounding `TRY … CATCH` — map non-2xx HTTP to the existing error branch; only set
   `trf_status = 'Y'` / commit on success.
6. Date fields are already converted to `char10` strings in ABAP before the DB call — same in the API.

---

## 6. Checklist — tables/structures to send so I can finalise field lengths
- [x] ~~`ZMM_ASRS`~~ — ✅ received; §3.1c & §3.3c complete (`interface/DDIC_ZMM_ASRS.pdf`)
- [x] ~~`ZTW_MAT_DET`~~ — ✅ received; §3.5c complete (`interface/DDIC_ZTW_MAT_DET.pdf`)
- [x] ~~`ZTW_PROD_DET`~~ — ✅ received; §3.5e complete (`interface/DDIC_ZTW_PROD_DET.pdf`)
- [x] ~~`ZMM_DBCON_ASRS` / `ZCON_MDM` / `DBCON` + entries~~ — ✅ received (§1); external DBs confirmed as **MS SQL Server**
- [x] ~~`ZMM_PARAM`~~ — ✅ received (FIELD1 CHAR 300)
- [x] ~~`ZTW_PLNT_DET`~~ — ✅ resolved: **not a SAP table** (external TrackWise DB only, §3.5d); SAP-side lengths known
- [x] ~~Does an API already exist?~~ — ✅ answered from code (§4.1): **no — pure DB integration today**; recommend CPI JDBC (Option A)
- [ ] **Decision:** Option A (CPI JDBC to the same SQL Server tables) vs Option B (vendor builds APIs) — §4.1
- [ ] **S/4HANA target:** on-premise or Cloud (clean-core)? — decides the ABAP HTTP client

*Send items above and I will replace every ⟨CONFIRM⟩ with exact type/length and reissue this document.*
