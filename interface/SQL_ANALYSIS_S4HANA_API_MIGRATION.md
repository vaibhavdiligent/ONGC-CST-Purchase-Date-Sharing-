# SQL Analysis & S/4HANA API-Migration Assessment — ASRS / TrackWise Interfaces

**Scope:** 10 ABAP interface programs reconstructed from the PDF listings in `interface/`.
**Goal:** identify every SQL statement, separate what stays from what must change, and map the
Native-SQL (`EXEC SQL … ENDEXEC`) database calls to **API calls**, because in S/4HANA a
program may **not** open a direct connection to an external database and run SQL against it.

---

## 1. The core finding (one pattern, repeated)

Every program has the **same two-layer data access**:

| Layer | Technology | Runs against | S/4HANA verdict |
|-------|-----------|--------------|-----------------|
| **A. SAP data** | **Open SQL** (`SELECT`/`INSERT`/`MODIFY` on SAP tables) | The SAP HANA DB (tables `ZMM_ASRS`, `ZMM_DBCON_ASRS`, `ZCON_MDM`, `ZMM_PARAM`, `MARA`, `T001W`, `ZTW_*`) | ✅ **Keep** — Open SQL is fully supported. Minor ATC/clean-core tweaks only. |
| **B. External-system data** | **Native SQL** (`EXEC SQL`) over a **DBCON secondary connection** | A **non-SAP database** belonging to the ASRS/WMS system and the TrackWise QMS | ❌ **Must be replaced by an API call** — this is the "SQL EXE" to remove. |

**Why layer B breaks on S/4HANA:** the programs read a connection name from a Z-config table
(`ZMM_DBCON_ASRS` / `ZCON_MDM` → field `DBCON`), then do
`EXEC SQL. CONNECT TO :dbcon … SET CONNECTION … INSERT INTO <foreign_table> … ENDEXEC.`
i.e. SAP writes **directly into the warehouse/quality system's own database tables**.
On S/4HANA (clean-core / Cloud, and discouraged even on-prem) native SQL against a
secondary DBCON connection to a 3rd-party database is not allowed. The supported design is:
the external system exposes a **REST/OData/SOAP API**, and SAP **calls that API** with the same
payload it used to `INSERT`/`SELECT`.

So the migration is essentially: **every `EXEC SQL` block → an HTTP/API interaction**, and the
DBCON connection-config tables → **HTTP/RFC destinations or Communication Arrangements**.

---

## 2. The two external target systems

| Target system | Connection source (Z-table) | External tables the program touches | Direction |
|---------------|------------------------------|--------------------------------------|-----------|
| **ASRS / WMS** (Automated Storage & Retrieval / warehouse) | `ZMM_DBCON_ASRS.DBCON` (per plant `WERKS`) | `HOST_TO_WMS` | Push (INSERT) + status read (SELECT) |
| **TrackWise** (Sparta/Honeywell TrackWise QMS + RA-MDM) | `ZCON_MDM.DBCON` | `MARA` (on TW side), `ZTW_MAT_DET`, `ZTW_PLNT_DET`, `ZTW_PROD_DET` | Push (INSERT) + existence check (SELECT) |

---

## 3. Per-program SQL inventory

Legend: **[OPEN]** = Open SQL on SAP tables (keep). **[NATIVE]** = `EXEC SQL` on the external DB (replace with API).
Line numbers refer to the extracted `.abap` files in `interface/`.

### 3.1 `ZMM_SQL_ASRS_SAP_PUSH` (and 6 near-identical siblings)
Siblings: `_COR` (correction), `_DISPENSE`, `_IN` (inbound), `_MAT` (material), `_OUT` (outbound), `_PARFULL` (partial/full).
They differ only by which `MSG_TRANS_TYPE` they select and minor field mapping; the DB pattern is identical.

| Line(s) | Kind | Statement | Target |
|---------|------|-----------|--------|
| ~26–28 | **[OPEN]** | `SELECT * FROM zmm_dbcon_asrs INTO TABLE gt_con` | SAP — read connection config |
| ~43–47 | **[OPEN]** | `SELECT * FROM zmm_asrs … WHERE msg_trans_type IN s_type AND trf_status = 'N'` | SAP — pending messages to send |
| ~64–66 | **[NATIVE]** | `EXEC SQL. DISCONNECT :con` | ASRS DB — teardown |
| ~69–71 | **[NATIVE]** | `EXEC SQL. CONNECT TO :gs_con-DBCON` | ASRS DB — open connection |
| ~72–74 | **[NATIVE]** | `EXEC SQL. GET CONNECTION :CON` | ASRS DB |
| ~75–77 | **[NATIVE]** | `EXEC SQL. SET CONNECTION :gs_con-DBCON` | ASRS DB |
| ~91–150 | **[NATIVE]** | `EXEC SQL. INSERT INTO host_to_wms ( … 30 columns … ) VALUES ( :gs_asrs-… )` | **ASRS DB — push message** ← main call |
| ~167–169 | **[NATIVE]** | `EXEC SQL. DISCONNECT :con` | ASRS DB |
| ~158 | **[OPEN]** | `COMMIT WORK AND WAIT` | (LUW) |
| ~171 | **[OPEN]** | `MODIFY zmm_asrs FROM TABLE gt_asrs` | SAP — write back `trf_status = 'Y'` |

`HOST_TO_WMS` columns inserted: `MSG_SRC, MSG_REC_ID, MSG_TRANS_TYPE, MSG_ACTION, MSG_RET_SRC,
MSG_RET_REC_ID, MSG_RET_TRANS_ID, MSG_DT_DEF, MSG_DT_TRM, MSG_ERR, MSG_ERR_DESC, MSG_STAT,
GR_NO, REQ_ID, REQ_TYPE, MAT_CODE, DESCRIPTION, UOM, ITEM_TYPE, SAP_BATCH, QTY, STATUS,
TOTAL_PACK, MFG_DATE, MANUFACTURER, MFG_BATCH, LINE_ITEM, PLANT, OLD_STATUS`.

**→ API replacement:** `POST /asrs/host-to-wms` with a body carrying those 30 fields, once per
message. The connection loop (`CONNECT/SET/GET/DISCONNECT`) collapses into destination handling;
`MODIFY zmm_asrs` (status write-back) stays as Open SQL after a successful API response.

### 3.2 `ZMM_ASRS_SAP_INTERFACE` (status read-back / ALV monitor)
| Line(s) | Kind | Statement | Target |
|---------|------|-----------|--------|
| 75–99 | **[OPEN]** | `SELECT … FROM zmm_asrs … WHERE msg_dt_def IN p_date AND plant = s_werks` | SAP |
| 101–104 | **[OPEN]** | `SELECT * FROM zmm_dbcon_asrs WHERE werks = s_werks` | SAP — connection config |
| 133–135 | **[NATIVE]** | `DISCONNECT :con` | ASRS DB |
| 138–140 | **[NATIVE]** | `CONNECT TO :gs_con-DBCON` | ASRS DB |
| 141–143 | **[NATIVE]** | `SET CONNECTION :gs_con-DBCON` | ASRS DB |
| 149–154 | **[NATIVE]** | `EXEC SQL. OPEN dbcur FOR SELECT MSG_ERR, MSG_STAT FROM HOST_TO_WMS WHERE MSG_REC_ID = :ls_asrs-MSG_REC_ID` | **ASRS DB — read status** |
| 155–157 | **[NATIVE]** | `FETCH NEXT dbcur INTO :ls_asrs-MSG_ERR, :ls_asrs-MSG_STAT` | ASRS DB — cursor fetch |
| 158–160 | **[NATIVE]** | `CLOSE dbcur` | ASRS DB |
| 189–191 | **[NATIVE]** | `DISCONNECT :con` | ASRS DB |

**→ API replacement:** `GET /asrs/host-to-wms/{MSG_REC_ID}` → returns `{ MSG_ERR, MSG_STAT }`.
The whole open-cursor/fetch/close triple becomes a single GET; the ALV display logic is unchanged.

### 3.3 `ZMDM_RA_TRACKWISE` (push material numbers to RA-TrackWise)
| Line(s) | Kind | Statement | Target |
|---------|------|-----------|--------|
| 47–50 | **[OPEN]** | `SELECT * FROM zcon_mdm CLIENT SPECIFIED … WHERE mandt = sy-mandt` | SAP — connection config |
| 57–59 | **[OPEN]** | `SELECT field1 FROM zmm_param … UP TO 1 ROWS … ORDER BY srno` | SAP — parameter (excluded statuses) |
| 62–68 | **[OPEN]** | `SELECT * FROM mara WHERE ersda IN s_ersda AND mtart = 'ZAPI' AND mstae NOT IN (g_field1)` | SAP — materials |
| 96–98 | **[NATIVE]** | `DISCONNECT :con` | TW DB |
| 102–104 | **[NATIVE]** | `CONNECT TO :gs_con-DBCON` | TW DB |
| 106–108 | **[NATIVE]** | `GET CONNECTION :CON` | TW DB |
| 110–112 | **[NATIVE]** | `SET CONNECTION :gs_con-DBCON` | TW DB |
| 126–130 | **[NATIVE]** | `EXEC SQL. INSERT INTO MARA (MATNR) VALUES ( :gs_mara-matnr )` | **TW DB — push material** |
| 143–145 | **[NATIVE]** | `DISCONNECT :con` | TW DB |

**→ API replacement:** `POST /trackwise/material { "matnr": … }` per material.

### 3.4 `ZQM_TRACKWISE` (largest — pushes plant / material / product details)
27 `EXEC SQL` blocks; the substantive ones:

| Line(s) | Kind | Statement | Target |
|---------|------|-----------|--------|
| 71–74, 266–269 | **[OPEN]** | `SELECT * FROM zcon_mdm CLIENT SPECIFIED WHERE mandt = sy-mandt` | SAP — connection config |
| 89, 245 | **[OPEN]** | `INSERT ztw_prod_det / ztw_mat_det FROM TABLE …` | SAP — local mirror tables |
| 317 | **[OPEN]** | `SELECT SINGLE name1 FROM t001w WHERE werks = …` | SAP — plant name |
| 131–173 | **[NATIVE]** | `INSERT INTO ZTW_MAT_DET ( … ~20 cols … )` | **TW DB — material details** |
| 182–222 | **[NATIVE]** | `INSERT INTO ZTW_MAT_DET ( … )` (2nd branch) | TW DB |
| 302–307 | **[NATIVE]** | `SELECT Plant_Code, Plant_Name FROM ZTW_PLNT_DET WHERE Plant_Code = :gv_plant` | **TW DB — existence check** |
| 320–326 | **[NATIVE]** | `INSERT INTO ZTW_PLNT_DET (Plant_Code, Plant_Name) VALUES (…)` | TW DB — create plant |
| 410–442, 450–480, 488–516, 525–555, 563–595, 603–637, 645–675, 683–715 | **[NATIVE]** | 8 × `INSERT INTO ZTW_PROD_DET ( … )` (one per date-condition branch) | **TW DB — product details** |
| plus 15 conn ops | **[NATIVE]** | `CONNECT/SET/GET/DISCONNECT :con` | TW DB |

**→ API replacement:**
- `GET /trackwise/plant/{code}` (existence) + `POST /trackwise/plant`
- `POST /trackwise/material-detail`
- `POST /trackwise/product-detail` (the 8 INSERT branches all hit the same endpoint with different payloads)

---

## 4. Consolidated counts

| Program | `EXEC SQL` blocks | External INSERT | External SELECT | Conn ops | Open-SQL stmts (keep) |
|---------|:---:|:---:|:---:|:---:|:---:|
| ZMM_SQL_ASRS_SAP_PUSH | 6 | 1 | 0 | 5 | 3 |
| ZMM_SQL_ASRS_SAP_PUSH_COR | 6 | 1 | 0 | 5 | 3 |
| ZMM_SQL_ASRS_SAP_PUSH_DISPENSE | 6 | 1 | 0 | 5 | 3 |
| ZMM_SQL_ASRS_SAP_PUSH_IN | 6 | 1 | 0 | 5 | 3 |
| ZMM_SQL_ASRS_SAP_PUSH_MAT | 6 | 1 | 0 | 5 | 3 |
| ZMM_SQL_ASRS_SAP_PUSH_OUT | 6 | 1 | 0 | 5 | 3 |
| ZMM_SQL_ASRS_SAP_PUSH_PARFULL | 6 | 1 | 0 | 5 | 3 |
| ZMM_ASRS_SAP_INTERFACE | 7 | 0 | 1 (cursor) | 4 | 2 |
| ZMDM_RA_TRACKWISE | 6 | 1 | 0 | 5 | 3 |
| ZQM_TRACKWISE | 27 | 11 | 1 | 15 | 5 |
| **Total** | **82** | **19** | **3** | **59** | **31** |

Every one of the **82 `EXEC SQL` blocks** is a candidate for API replacement; the **31 Open-SQL
statements stay** (they already run on the S/4HANA DB).

---

## 5. Recommended S/4HANA replacement pattern

1. **Connection config → destination.** Replace the `DBCON` name in `ZMM_DBCON_ASRS` / `ZCON_MDM`
   with an **HTTP destination** (on-prem: SM59 / `CL_HTTP_DESTINATION_PROVIDER`; Cloud: a
   **Communication Arrangement / Outbound Service**). The per-plant lookup stays, but it now
   yields a destination/endpoint instead of a DB connection.
2. **`CONNECT / SET / GET / DISCONNECT` → nothing** (or a single "get client for destination").
   Connection lifecycle disappears; there is no session to open/close.
3. **`INSERT INTO <ext_table>` → `POST`** to the system's create endpoint, body = the inserted
   columns. Wrap in the existing `TRY … CATCH` so a non-2xx response is handled where
   `cx_sy_native_sql_error` is handled today.
4. **`SELECT/OPEN/FETCH/CLOSE FROM <ext_table>` → `GET`** (or a query POST) returning the same
   fields the cursor fetched.
5. **Keep** the Open-SQL reads and the `MODIFY zmm_asrs` / `COMMIT WORK` status write-back —
   they just move to *after* a successful API response instead of after a successful DB commit.
6. Suggested encapsulation: **one class per target system** (e.g. `ZCL_ASRS_WMS_API`,
   `ZCL_TRACKWISE_API`) with methods `push_message`, `get_status`, `push_material`,
   `push_product_detail`, `push_plant`, `plant_exists`. All 7 `ZMM_SQL_ASRS_SAP_PUSH*` variants
   then call the same `ZCL_ASRS_WMS_API->push_message( )`.

---

## 6. Open items — information needed to build the actual API calls

To turn this analysis into working replacement code I need, from you, whichever of these apply:

1. **Target-system API specs** — do the ASRS/WMS and TrackWise systems already expose REST/OData/SOAP
   endpoints? If so: base URLs, operations, auth (basic / OAuth / cert), and request/response schemas.
   If **no API exists yet**, that is a prerequisite (SAP can't be made to call one that isn't there).
2. **S/4HANA flavour** — on-premise or Cloud (clean-core)? This decides the mandated client
   (`if_web_http_client` + Communication Arrangement vs. classic `CL_HTTP_CLIENT`/SM59) and whether
   any `EXEC SQL` is even syntactically allowed during a transition.
3. **DDIC structures** for the SAP config/data tables — `ZMM_ASRS`, `ZMM_DBCON_ASRS`, `ZCON_MDM`,
   `ZMM_PARAM`, and the local mirror tables `ZTW_PROD_DET` / `ZTW_MAT_DET` — field names + types, so
   the payload mapping is exact.
4. **External table layouts** — column data types for `HOST_TO_WMS`, `ZTW_MAT_DET`, `ZTW_PLNT_DET`,
   `ZTW_PROD_DET`, and the TrackWise-side `MARA` (I have the column *names* from the code, not types).
5. **Error/idempotency expectations** — should a failed API call keep `trf_status = 'N'` for retry
   (current code sets `'Y'` only after success)? Any dedup key on the API side (e.g. `MSG_REC_ID`)?

Give me #1 and #2 first — with those I can scaffold the API classes and rewrite one program
end-to-end as a reference, then roll the same pattern across the remaining nine.
