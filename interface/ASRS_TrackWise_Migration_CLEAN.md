% ASRS & TrackWise Interfaces — SQL-to-API Migration
% Program-by-program specification (SAP → CPI)
% 14 July 2026

# Document control

| | |
|---|---|
| **Title** | ASRS & TrackWise Interfaces — SQL-to-API Migration |
| **Purpose** | Program-by-program analysis of the existing SAP↔ASRS/TrackWise interfaces and the plan to replace direct database (`EXEC SQL`) access with API calls via SAP CPI |
| **Audience** | Client IT / SAP team and CPI (SAP Integration Suite) consultant |
| **Target system** | SAP S/4HANA **Private Cloud Edition (RISE with SAP)** |
| **Scope** | 10 ABAP programs, 3 connection tables, 7 target interfaces |
| **Status** | For review |
| **Date** | 14 July 2026 |

# Executive summary

Ten SAP interface programs currently exchange data with two external systems — the **ASRS** automated
warehouse and the **TrackWise** quality system — by opening a **direct connection to those systems'
Microsoft SQL Server databases** and running SQL against them (`EXEC SQL`). No API or web service is
involved today.

The target landscape is **SAP S/4HANA Private Cloud Edition (PCE) under RISE with SAP** — an SAP-managed
system that follows the **clean-core** model. In this managed environment, **native-SQL secondary
connections (`DBCON`/`EXEC SQL`) to external third-party databases are not part of the supported setup and
conflict with clean core**, so each of these database calls must be replaced. The recommended and agreed
approach (**Option A**) is to route them through **SAP Integration Suite (CPI)**: SAP calls a CPI interface
over HTTPS, and CPI writes to / reads from the **same SQL Server tables** using its database (JDBC) adapter.
This makes SAP compliant with **no change required on the ASRS/TrackWise side**.

Because PCE is the S/4HANA on-premise stack (delivered as a managed private cloud), the **SAP-side outbound
call is standard, supported ABAP** — an HTTP(S) call through a configured destination — so no Public-Cloud
development restrictions apply to building the consumer programs.

This document lists, **for each program**: what it does, the SQL it uses (what stays vs. what must be
replaced), the SAP tables it reads (with sample data where available), and the structure of each table sent
to the external systems — everything the CPI team needs to build the interfaces.

# 1. Overview

Ten ABAP programs move data between SAP and two external, non-SAP systems:

- **ASRS / WMS** — the automated warehouse system (Microsoft SQL Server database).
- **TrackWise** — the Sparta/Honeywell TrackWise Quality system, plus an RA/MDM material feed (also Microsoft SQL Server).

**Today (as-is):** every program talks to these systems with **Native SQL** — `EXEC SQL … ENDEXEC` over a
DBCON secondary connection — writing/reading rows *directly* in the external database. There is **no API**
of any kind in the code.

**Problem:** the target system is **SAP S/4HANA Private Cloud Edition (RISE with SAP)** — SAP-managed and
clean-core. Native-SQL secondary connections (`DBCON`/`EXEC SQL`) to external third-party databases are not
supported in this managed landscape and are contrary to clean core. So **every `EXEC SQL` block must be
removed** and replaced by a call to an interface.

**Target design (Option A — chosen):**

> **SAP (ABAP)  →  HTTPS (OData/REST)  →  CPI iFlow  →  JDBC  →  the same SQL Server tables**

CPI uses its **Microsoft SQL Server JDBC adapter** to write/read the very same tables SAP writes today
(`HOST_TO_WMS`, `ZTW_MAT_DET`, `ZTW_PROD_DET`, `ZTW_PLNT_DET`, TrackWise `MARA`). Nothing changes on the
ASRS/TrackWise side; only the SAP→CPI hop becomes an API, and SAP becomes clean-core.

**Connectivity notes (RISE PCE):**

- **SAP → CPI:** the ABAP programs call CPI over HTTPS through a configured **destination / Communication
  Arrangement**. On PCE this uses the standard ABAP HTTP client (recommended: `if_web_http_client` with a
  destination) — fully supported managed ABAP.
- **CPI → SQL Server:** the ASRS/TrackWise SQL Servers sit on plant/on-premise networks
  (e.g. `172.18.11.120`, `10.27.1.27`), so CPI reaches them via the **SAP Cloud Connector / on-premise JDBC**
  connectivity. This is CPI-side configuration and needs no SAP application change.

**What stays vs. what changes (rule for every program):**

| Statement type | Runs against | Action |
|---|---|---|
| Open SQL (`SELECT`/`INSERT`/`MODIFY` on SAP tables) | SAP HANA DB | **Keep unchanged** |
| Native SQL `EXEC SQL … ENDEXEC` (`CONNECT`/`INSERT`/`SELECT`/`DISCONNECT`) | External SQL Server DB | **Replace with the CPI API call** |

---

# 2. Connection tables (used by all programs)

Each program reads a **connection name** from a config table, then `CONNECT TO` that name. The name is defined
in the standard SAP `DBCON` table (transaction DBCO). In the target design these become **CPI JDBC
data-sources** — the per-plant selection stays the same.

**2.1  ZMM_DBCON_ASRS** — ASRS connection per plant *(SAP-ASRS VS Plant connection table)*

| Field | Type | Len | Note |
|---|---|---|---|
| MANDT | CLNT | 3 | Client (key) |
| WERKS | CHAR | 4 | Plant (key) |
| DBCON | CHAR | 30 | Connection name → `DBCON.CON_NAME` |
| DESCRIPTION | CHAR | 132 | Description |
| VALUE1 / VALUE2 | CHAR | 50 | Comments |

*Data:*

| WERKS | DBCON | Description |
|---|---|---|
| 1047 | CON_1047 | CIPLA / GOA-VII PD II / PHARMA – ASRS (prod plant) |
| 1048 | CON_1048 | SIKKIM ASRS |

**2.2  ZCON_MDM** — TrackWise/MDM connection (single, no plant key)

| Field | Type | Len | Note |
|---|---|---|---|
| MANDT | CLNT | 3 | Client (key) |
| DBCON | CHAR | 30 | Connection name → `DBCON.CON_NAME` (key) |
| DESCRIPTION | CHAR | 132 | Description |
| VALUE1 / VALUE2 | CHAR | 50 | Comments |

*Data:*

| DBCON | Description |
|---|---|
| CON_MDM | Database Connection name for RA TrackWise Master Integration |

**2.3  DBCON** (standard SAP) — the real connection definitions these names point to

| CON_NAME | DBMS | SQL Server (host / instance, port) | Database |
|---|---|---|---|
| CON_1047 | MSS | tcp:172.18.11.120 | CIG_EFAWMS |
| CON_1048 | MSS | tcp:10.27.1.27 | CIS_EFAWMS |
| CON_MDM | MSS | tcp:INCPLTWPRDDB01\TWPRD,55619 | TW_SAP |

`DBMS = MSS` confirms all three are **Microsoft SQL Server**. Credentials (`USER_NAME`, `PASSWORD`) live in
`DBCON` today; in the target design they move into the **CPI JDBC data-source** and SAP no longer holds them.

---

# 3. Program — ZMM_SQL_ASRS_SAP_PUSH  *(and 6 identical variants)*

**Variants (same logic, different transaction type):** `_COR` correction · `_DISPENSE` · `_IN` inbound ·
`_MAT` material · `_OUT` outbound · `_PARFULL` partial/full. All 7 insert into the **same** `HOST_TO_WMS`
table, so all 7 use the **same** API.

### 3.1  What it does
1. Reads the ASRS connection per plant from **ZMM_DBCON_ASRS**.
2. Reads pending messages from **ZMM_ASRS** (`trf_status = 'N'`, of the requested transaction type).
3. Connects to the plant's external ASRS database.
4. For each message, **inserts a row into the external `HOST_TO_WMS`** table (the WMS inbound queue).
5. Marks the message as sent (`trf_status = 'Y'`) back in **ZMM_ASRS**.

### 3.2  SQL statements

*Keep (Open SQL on SAP tables):*

```abap
SELECT * FROM zmm_dbcon_asrs INTO TABLE gt_con.                       "connection config
SELECT * FROM zmm_asrs INTO CORRESPONDING FIELDS OF TABLE gt_asrs
        WHERE msg_trans_type IN s_type AND trf_status = 'N'.          "pending messages
MODIFY zmm_asrs FROM TABLE gt_asrs.                                   "write back status 'Y'
```

*Replace (Native SQL → API C-1):*

```abap
EXEC SQL. CONNECT TO :gs_con-DBCON      ENDEXEC.   "→ removed (CPI JDBC data-source)
EXEC SQL. SET CONNECTION :gs_con-DBCON  ENDEXEC.   "→ removed
EXEC SQL. GET CONNECTION :CON           ENDEXEC.   "→ removed
EXEC SQL.                                          "→ POST to CPI: ASRS Push Message
  INSERT INTO host_to_wms ( MSG_SRC, MSG_REC_ID, … 29 columns … )
  VALUES ( :gs_asrs-MSG_SRC, :gs_asrs-MSG_REC_ID, … )
ENDEXEC.
EXEC SQL. DISCONNECT :con               ENDEXEC.   "→ removed
```

### 3.3  Tables the data comes from (SAP)
- **ZMM_DBCON_ASRS** — see §2.1 (+ data).
- **ZMM_ASRS** — the message staging table; structure below. *(No sample data provided.)*

### 3.4  Table being sent out — `HOST_TO_WMS` (external ASRS DB)
`HOST_TO_WMS` is **not a SAP table** — it lives on the ASRS SQL Server. The 29 inserted columns mirror
**ZMM_ASRS**, so the payload structure is exactly the ZMM_ASRS fields below (excluding `MANDT`, `TRF_STATUS`,
`DATUM`, which are SAP-side only). Dates are sent as 10-char strings.

**ZMM_ASRS structure (source table = sent payload):**

| Field | Type | Len | Description |
|---|---|---|---|
| MANDT | CLNT | 3 | Client *(not sent)* |
| MSG_REC_ID | CHAR | 20 | Unique number **(key / dedup)** |
| MSG_SRC | CHAR | 20 | Host |
| MSG_TRANS_TYPE | CHAR | 20 | Transaction type (values below) |
| MSG_ACTION | CHAR | 3 | Action |
| MSG_RET_SRC | CHAR | 20 | Return source |
| MSG_RET_REC_ID | NUMC | 9 | Record ID |
| MSG_RET_TRANS_ID | NUMC | 9 | Return transaction ID |
| MSG_DT_DEF | DATS | 8 | Date (sent as 10-char) |
| MSG_DT_TRM | DATS | 8 | Date (sent as 10-char) |
| MSG_ERR | NUMC | 3 | Message error code |
| MSG_ERR_DESC | CHAR | 80 | Message description |
| MSG_STAT | CHAR | 3 | Message state |
| GR_NO | CHAR | 10 | Material document number |
| REQ_ID | CHAR | 12 | Requirement ID |
| REQ_TYPE | CHAR | 50 | Requirement type (F/H/M/O/P/R) |
| MAT_CODE | CHAR | 40 | Material number |
| DESCRIPTION | CHAR | 200 | Text |
| UOM | UNIT | 3 | Base unit of measure |
| ITEM_TYPE | CHAR | 3 | Item type |
| SAP_BATCH | CHAR | 10 | Batch number |
| QTY | QUAN | 13 | Quantity (3 dec) |
| STATUS | CHAR | 30 | Status (APP/BLK/QUA) |
| OLD_STATUS | CHAR | 30 | Old status (APP/BLK/QUA) |
| TOTAL_PACK | QUAN | 6 | No. of containers |
| MFG_DATE | DATS | 8 | Manufacturing date (sent as 10-char) |
| MANUFACTURER | CHAR | 30 | Manufacturer |
| MFG_BATCH | CHAR | 30 | Manufacturer batch |
| LINE_ITEM | NUMC | 4 | Item in material document |
| PLANT | CHAR | 4 | Plant |
| TRF_STATUS | CHAR | 1 | Transfer status Y/N *(not sent)* |
| DATUM | DATS | 8 | Record created on *(not sent)* |

**MSG_TRANS_TYPE allowed values** (these map 1:1 to the 7 variants): `COR` Correction · `DIS` Dispense ·
`FSCR` Full Status Change · `IN` Store IN · `MASTER` Master List · `OUT` Store OUT · `PSCR` Partial Status
Change · `PSCR_IN` Partial Status Store In.

---

# 4. Program — ZMM_ASRS_SAP_INTERFACE

### 4.1  What it does
A monitor report. For a plant + date range it reads the messages sent to ASRS and **reads their processing
status back** from the warehouse, then shows an ALV grid (sent / received / processed icons).

### 4.2  SQL statements

*Keep (Open SQL):*

```abap
SELECT … FROM zmm_asrs INTO CORRESPONDING FIELDS OF TABLE gt_asrs
        WHERE msg_dt_def IN p_date AND plant = s_werks.
SELECT * FROM zmm_dbcon_asrs INTO TABLE gt_con WHERE werks = s_werks.
```

*Replace (Native SQL → API C-2, read-back):*

```abap
EXEC SQL. CONNECT TO / SET CONNECTION :gs_con-DBCON ENDEXEC.   "→ removed
EXEC SQL.                                                      "→ GET from CPI: ASRS Get Status
  OPEN dbcur FOR
  SELECT MSG_ERR, MSG_STAT FROM host_to_wms
         WHERE MSG_REC_ID = :ls_asrs-MSG_REC_ID
ENDEXEC.
EXEC SQL. FETCH NEXT dbcur INTO :ls_asrs-MSG_ERR, :ls_asrs-MSG_STAT ENDEXEC.
EXEC SQL. CLOSE dbcur       ENDEXEC.
EXEC SQL. DISCONNECT :con   ENDEXEC.                           "→ removed
```

### 4.3  Tables the data comes from (SAP)
- **ZMM_ASRS** (§3.4 structure), **ZMM_DBCON_ASRS** (§2.1).

### 4.4  Data read from external system — `HOST_TO_WMS`
API **C-2** returns two fields per `MSG_REC_ID`:

| Direction | Field | Type | Len |
|---|---|---|---|
| Request | MSG_REC_ID | CHAR | 20 |
| Response | MSG_ERR | NUMC | 3 |
| Response | MSG_STAT | CHAR | 3 |

---

# 5. Program — ZMDM_RA_TRACKWISE

### 5.1  What it does
1. Reads the TrackWise connection from **ZCON_MDM**.
2. Reads an exclusion parameter (`FIELD1` for `PARAM_TYPE = 'ZZ9'`) from **ZMM_PARAM**.
3. Selects new API materials from **MARA** (`MTART = 'ZAPI'`, status not excluded, created-date range).
4. Connects to the TrackWise database and **inserts each material number** into the TrackWise-side `MARA`.

### 5.2  SQL statements

*Keep (Open SQL):*

```abap
SELECT * FROM zcon_mdm CLIENT SPECIFIED INTO TABLE gt_con WHERE mandt = sy-mandt.
SELECT field1 FROM zmm_param CLIENT SPECIFIED UP TO 1 ROWS INTO g_field1
        WHERE mandt = sy-mandt AND param_type = 'ZZ9' ORDER BY srno.
SELECT * FROM mara INTO TABLE gt_mara
        WHERE ersda IN s_ersda AND mtart = 'ZAPI' AND mstae NOT IN (g_field1).
```

*Replace (Native SQL → API C-3):*

```abap
EXEC SQL. CONNECT TO / GET / SET CONNECTION :gs_con-DBCON ENDEXEC.  "→ removed
EXEC SQL. INSERT INTO mara (MATNR) VALUES ( :gs_mara-matnr ) ENDEXEC. "→ POST TrackWise Material
EXEC SQL. DISCONNECT :con ENDEXEC.                                  "→ removed
```

### 5.3  Tables the data comes from (SAP)
- **ZCON_MDM** — §2.2 (+ data).
- **MARA** (standard SAP material master). Fields used: `MATNR`, `MTART` (= 'ZAPI'), `ERSDA` (created date),
  `MSTAE` (material status). Only `MATNR` is sent onward.
- **ZMM_PARAM** — parameter master; structure and sample data below.

**ZMM_PARAM structure** *(MM : Parameter master ztable)*

| Field | Type | Len | Note |
|---|---|---|---|
| MANDT | CLNT | 3 | Client (key) |
| SRNO | NUMC | 4 | Sequence number (key) |
| PARAM_TYPE | CHAR | 100 | Parameter type |
| FIELD1 … FIELD10 | CHAR | 300 | Values (program reads `FIELD1` where `PARAM_TYPE='ZZ9'`) |

**ZMM_PARAM sample data** (representative rows; the program itself uses only `PARAM_TYPE = 'ZZ9'`):

| SRNO | PARAM_TYPE | FIELD1 | FIELD2 |
|---|---|---|---|
| 0113 | REASON | ZINT | REGULATORY DEFICIENCY |
| 0119 | REASON | ZFGS | FIRST TIME SUPPLY TO THE CUSTOMER |
| 0123 | REASON | ZAPI | NEW MOLECULE |
| 0129 | VENDOR_CAT | ZNB | 109 / RAW MATERIAL |
| 0143 | VENDOR_CAT | ZTRD | 110 / FG-TRADING |
| 0174 | VENDOR_CAT | ZCER | 144 / CER-DIRECT |

### 5.4  Table being sent out — TrackWise `MARA`
External TrackWise DB. One field:

| Field | Type | Len | Note |
|---|---|---|---|
| MATNR | CHAR | 40 | Material number (leading zeros stripped before send) |

---

# 6. Program — ZQM_TRACKWISE  *(largest)*

### 6.1  What it does
Triggered from a QM BAdI. Depending on a memory flag (`P` = product, `M` = material) it takes product- or
material-detail records and pushes them to TrackWise for deviation processing:
1. Reads the TrackWise connection from **ZCON_MDM**.
2. Imports the detail records from memory; writes a local copy into the SAP mirror tables
   **ZTW_PROD_DET** / **ZTW_MAT_DET** (Open SQL — kept).
3. Connects to TrackWise and **inserts** material-detail / product-detail rows into the external
   `ZTW_MAT_DET` / `ZTW_PROD_DET`.
4. For the plant, **checks** `ZTW_PLNT_DET`; if missing, reads the plant name from **T001W** and **inserts**
   the plant into `ZTW_PLNT_DET`.

### 6.2  SQL statements

*Keep (Open SQL):*

```abap
SELECT * FROM zcon_mdm CLIENT SPECIFIED INTO TABLE gt_con WHERE mandt = sy-mandt.
SELECT SINGLE name1 FROM t001w INTO gv_name WHERE werks = p_gv_qmel-mawerk.
INSERT ztw_prod_det FROM TABLE gt_prod_det.     "SAP mirror
INSERT ztw_mat_det  FROM TABLE gt_mat_det.      "SAP mirror
```

*Replace (Native SQL → APIs C-4/5/6/7):*

```abap
EXEC SQL. CONNECT / SET / GET / DISCONNECT :con ENDEXEC.            "→ removed (×15)
EXEC SQL. INSERT INTO ZTW_MAT_DET ( … 20 cols … )  VALUES ( … ) ENDEXEC.  "→ POST Material Detail (C-4)
EXEC SQL. SELECT Plant_Code, Plant_Name FROM ZTW_PLNT_DET
                 WHERE Plant_Code = :gv_plant ENDEXEC.             "→ GET Check Plant (C-5)
EXEC SQL. INSERT INTO ZTW_PLNT_DET (Plant_Code, Plant_Name) VALUES ( … ) ENDEXEC. "→ POST Plant (C-6)
EXEC SQL. INSERT INTO ZTW_PROD_DET ( … 15 cols … ) VALUES ( … ) ENDEXEC.  "→ POST Product Detail (C-7)
```
*(The product-detail INSERT appears 8 times for different date combinations — all one API.)*

### 6.3  Tables the data comes from (SAP)
- **ZCON_MDM** — §2.2 (+ data).
- **T001W** (standard) — plant; field `NAME1` (CHAR 30) used as the plant name.
- **ZTW_MAT_DET / ZTW_PROD_DET** — SAP mirror tables; also the payload structure (below).

### 6.4  Tables being sent out (external TrackWise DB)

**(a) ZTW_MAT_DET — material detail (20 fields sent)** *(also a SAP table)*

| Field | Type | Len | Description |
|---|---|---|---|
| Mat_Code | CHAR | 40 | Material number |
| Mat_Desc | CHAR | 200 | Material description |
| Batch_No | CHAR | 10 | Batch number |
| Plant_Code | CHAR | 5 | Plant code |
| Mfg_Batch_No | CHAR | 30 | Manufacturer batch |
| Ins_Lot_No | NUMC | 12 | Inspection lot |
| AR_No | CHAR | 15 | A.R. number |
| Vendor_Code | CHAR | 10 | Supplier account |
| Vendor_Name | CHAR | 72 | Vendor name |
| Mfg_Code | CHAR | 30 | Manufacturer |
| Mfg_Name | CHAR | 72 | Manufacturer name |
| Date_of_Supply | CHAR | 10 | Date string (empty if blank) |
| Qty_Supplied | QUAN | 13 | Quantity (3 dec) |
| UoM | UNIT | 3 | Base unit of measure |
| Impacted_Prod_Code | CHAR | 40 | Material number |
| Impa_Prod_Batch_No | CHAR | 30 | Impacted product batch |
| Impacted_Prod_Name | CHAR | 200 | Material description |
| Impa_Prod_Ver_No | CHAR | 4 | Production version |
| Impacted_Ref_Doc_No | CHAR | 300 | Impacted reference no. |
| Impa_Mfg_Batch_No | CHAR | 30 | Impacted batch number |

**(b) ZTW_PROD_DET — product detail (15 fields sent)** *(also a SAP table)*

| Field | Type | Len | Description |
|---|---|---|---|
| Prod_Code | CHAR | 40 | Material number |
| Prod_Name | CHAR | 200 | Material description |
| Prod_Batch_No | CHAR | 10 | Batch number |
| Plant_Code | CHAR | 5 | Plant code |
| Production_Ver_No | CHAR | 4 | Production version |
| Ref_Doc_No | CHAR | 300 | Reference document no. |
| Mfg_Batch_No | CHAR | 30 | Manufacturer batch |
| Expiry_Date | CHAR | 10 | Date string (empty if blank) |
| Retest_Date | CHAR | 10 | Date string (empty if blank) |
| Impacted_Mat_Code | CHAR | 40 | BOM component |
| Impacted_Mat_Desc | CHAR | 200 | Material description |
| Impacted_Batch_No | CHAR | 10 | Batch number |
| Impa_Mat_Insp_Lot | NUMC | 12 | Inspection lot |
| Impa_Mfg_Batch_No | CHAR | 30 | Impacted manufacturer batch |
| Impacted_Mat_AR_No | CHAR | 15 | A.R. number |

**(c) ZTW_PLNT_DET — plant (2 fields)** — **external TrackWise DB only (not a SAP table)**

| Field | Type | Len | Source |
|---|---|---|---|
| Plant_Code | CHAR | 5 | Plant (from `ztw_prod_det-plant_code`) |
| Plant_Name | CHAR | 30 | From `T001W-NAME1` |

*External column lengths for `ZTW_PLNT_DET` are owned by the TrackWise side; the SAP-side values sent are as
above.*

---

# 7. API list for CPI (summary)

Under Option A, each API is a CPI iFlow: SAP calls it over HTTPS; the iFlow performs the identical JDBC
INSERT/SELECT on the SQL Server table shown.

| API | Verb (SAP→CPI) | External table (JDBC) | Fields | Called by |
|---|---|---|---|---|
| C-1 ASRS Push Message | POST | HOST_TO_WMS | 29 | 7 × ZMM_SQL_ASRS_SAP_PUSH* |
| C-2 ASRS Get Status | GET | HOST_TO_WMS | 1 in / 2 out | ZMM_ASRS_SAP_INTERFACE |
| C-3 TrackWise Push Material | POST | MARA (TW) | 1 | ZMDM_RA_TRACKWISE |
| C-4 TrackWise Push Material Detail | POST | ZTW_MAT_DET | 20 | ZQM_TRACKWISE |
| C-5 TrackWise Check Plant | GET | ZTW_PLNT_DET | 1 in / 2 out | ZQM_TRACKWISE |
| C-6 TrackWise Push Plant | POST | ZTW_PLNT_DET | 2 | ZQM_TRACKWISE |
| C-7 TrackWise Push Product Detail | POST | ZTW_PROD_DET | 15 | ZQM_TRACKWISE |

**Target confirmed: SAP S/4HANA Private Cloud Edition (RISE with SAP).** The SAP-side consumer is standard
managed ABAP — an HTTPS call to CPI via a destination / Communication Arrangement (recommended
`if_web_http_client`). No open points remain on the SAP side; the CPI team's remaining task is the SQL Server
connectivity (Cloud Connector / on-premise JDBC) and the seven iFlows in §7.
