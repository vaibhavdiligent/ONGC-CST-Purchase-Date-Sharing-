# Technical Specification Document
## Accounts PL Support Fee Calculation & Posting — S/4HANA Re-implementation

| | |
|---|---|
| Program | `/CCBJI/RUFIGLR_REPORTING_SUPP` |
| Transaction | `/CCBJI/RURGL_REPSUPP` (new, to be created in S/4) |
| RICEF | GAP-1000002273 |
| Company Code | 7827 (BJI); TVARVC also shows 7830 |
| Based on | FS_SupportFee_S4HANA_v2.0.docx + legacy BW program ZBW_RUFIGLR_REPORTING_SUPP + copa FS.zip system extracts + copa_add_input.docx + ECC posting program /CCBJI/RUFIGLR_SUPPFI_POST |
| Status | v1.2 — Design for review. No code written yet. v1.1: posting design aligned to actual ECC program. v1.2: FS v1.3 + client review points absorbed (existing /CCBJI/T_SUP_COS reused — new CE table cancelled; updated §8.5 priority diagram; KAM upload pending business decision); performance architecture (CDS + AMDP) added; BW HANA source findings incorporated (RJ/AB doc types, 3-mechanism dedup — see bw_sources/README). |
| Author | Claude (Diligent Consulting) — reviewed by Vaibhav Maheshwari |

---

## 1. Architecture Summary

The BW pipeline (HANA calc views → AMDP → ABAP → AL11 TXT file → ECC posting job)
collapses into a single S/4 report that reads ACDOCA on the primary DB connection,
calculates the incidence-% based support fee in ABAP, and posts FI (and account-based
COPA) documents directly with `BAPI_ACC_DOCUMENT_POST`. No files, no staging tables,
no RFC, no secondary connection.

Verified system facts driving this design:

* ACDOCA carries the JP00 COPA characteristics as `*_PA` fields:
  `WW207_PA` (dealer), `WW214_PA` (BAC), `WW220_PA` (RPP channel), `WW223_PA`
  (RPP category), `WW226_PA` (packaging code), `WW235_PA` (KAM rep), `WW236_PA`
  (RPP category ACPL), `WW237_PA` (DDIS brand). Cost center = `RCNTR`,
  sender cost center = `SCNTR`, cost element = `RACCT`. No ZZ fields.
* Volume (RBUKRS=7827, RYEAR=2026, RLDNR=0L): period 001 ≈ 118.4M rows,
  000 ≈ 4.8M, 002/003 ≈ 1.1M each. **All selections aggregate in the DB.**
* `/CCBJI/T_MAP_GL`, `/CCBJI/T_SUP_FEE` (KOSTL in key), `/CCBJI/T_DOC_TYP`,
  `/CCBJI/T_CCJCACC`, `/CCEJ/TEJACC`, `/CCEJ/TKAMREP`, `/CCEJ/TRPPCATE`,
  `/CCEJ/TZKTSD0023`, `/CCEJ/TZKTSD0004` exist in S/4.
* `/CCEJ/T_PROFCTRPH` exists nowhere (BW, ECC, S/4) → fallback redesigned (§7.4).
* `/BIC/AZJSUFEECE2` = 6 fields, 16 data rows → migrated to new S/4 table (§3.2).
* `/BIC/AZKAMORGL42` = 3 fields (KAMORGL4CD, RECORDMODE, KOSTL), 115 rows →
  migrated to new S/4 table (§3.1). FS §5.1's PRCTR/BUKRS/DATBI/DATAB fields do
  not exist in the real BW table.

## 2. Object List (all new objects in new package, namespace /CCBJI/)

| # | Object | Type | Purpose |
|---|--------|------|---------|
| 1 | `/CCBJI/RUFIGLR_REPORTING_SUPP` | Report | Main program |
| 2 | `/CCBJI/RUFIGLI_REP_TOP` | Include | Types, data, constants |
| 3 | `/CCBJI/RUFIGLI_REP_SEL` | Include | Selection screen |
| 4 | `/CCBJI/RUFIGLI_REP_FORM` | Include | Logic |
| 5 | `/CCBJI/RURGL_REPSUPP` | TCode | Launch report |
| 6 | `/CCBJI/T_KAM_L4_CC` | Table | KAM L4 → cost center mapping (§3.1) |
| 7 | `/CCBJI/T_SUP_COS` (EXISTING in S/4 — reused, no new table) | Table | Cost element mapping for COPA posting (§3.2) |
| 8 | `/CCBJI/T_GL_TYPE` | Table | GL type classification (§3.3) |
| 9 | `/CCBJI/T_RPPCAT_TY` | Table | RPP category type classification (§3.4) |
| 10 | `/CCBJI/RUFIGLR_KAML4_UPLOAD` | Report | KAM L4 CSV upload (§9) |
| 11 | `/CCBJI/RURGL_KAML4UP` | TCode | Launch upload |
| 12 | `/CCBJI/S_SUPFEE_RESULT` | Structure | = ZSRTR_RESULT (51 fields, S/4 types) |
| 13 | `/CCBJI/S_SUPFEE_MONTH` | Structure | = ZSRTR_MONTHRES |
| 14 | `/CCBJI/S_SUPFEE_AUDIT` | Structure | = ZSRTR_AUDITRES |
| 15 | `/CCBJI/S_SUPFEE_GLDATA` | Structure | ACDOCA FI extraction result |
| 16 | `/CCBJI/S_SUPFEE_COPA` | Structure | ACDOCA COPA extraction result |
| 17 | `/CCBJI/MC_SUPFEE` | Message class | All messages (replaces TEXT-Exx pattern) |
| 18 | `/CCBJI/RTR_TOT` | Data element | DEC 15 total (replaces Z_RTR_TOT) |
| 19 | SM30 maintenance views/dialogs for tables 6–9 | TMG | User maintenance |
| 20 | Number range object (reuse `Z_SUPP_FEE` copied to S/4, interval Z1 1000000001–1999999999) | SNRO | COPA doc grouping id |
| 21 | BAdI implementation `ACC_DOCUMENT` (`/CCBJI/` impl.) | BAdI | Map EXTENSION1 row (XREF1_HD) to BKPF-XREF1_HD, as the ECC program did |

TVARVC entries (STVARV, new in S/4): see §8.

## 3. New Tables

### 3.1 `/CCBJI/T_KAM_L4_CC` — KAM L4 vs Cost Center
Mirrors the real BW table plus minimal governance fields.

| Field | Key | Type | Notes |
|---|---|---|---|
| MANDT | X | MANDT | |
| KAMORGL4CD | X | CHAR 8 | KAM L4 org code |
| KOSTL | | KOSTL (CHAR 10) | Cost center |
| ERNAM/ERDAT/AENAM/AEDAT | | std | audit |

Initial load: BIC_AZKAMORGL42.XLSX (115 rows). DECISION D-01: validity dates
(DATAB/DATBI) are NOT added — the BW original has none; can be added later if
business asks.

### 3.2 Cost element mapping (COPA posting) — EXISTING table `/CCBJI/T_SUP_COS`
Per client review (30.07.2026): the S/4 equivalent of `/BIC/AZJSUFEECE2` already
exists as `/CCBJI/T_SUP_COS` ("Support Fee Cost Element", FS v1.3 §9.3) and is
reused — the previously planned new table is CANCELLED. Verify at build start
(O-09): SE11 structure and content vs the 16 BW rows (GLs 0893201522,
0893309312 × WW214/WW207/WW237 flags → KSTAR). Additionally per review point 1:
`/CCBJI/T_MAP_GL`-VALUE_FIELD content is repurposed — costing-based value-field
names are replaced by GL/cost-element values for account-based COPA (config
data activity, owner Functional).

### 3.3 `/CCBJI/T_GL_TYPE` — GL type classification (DECISION D-02)
The FS decision tree needs each expense GL classed as 50% Support / CokeON / Other.
No such data exists anywhere today, so it becomes a new config table:

| Field | Key | Type | Values |
|---|---|---|---|
| MANDT | X | MANDT | |
| RACCT | X | CHAR 10 | expense GL |
| GL_TYPE | | CHAR 6 | `50PCT` / `COKEON` / `OTHER` |

Runtime default when a GL has no row: **OTHER** + warning in log (matches FS §17
"GL type classification returns blank → treated as Other GL"). Business fills the
50PCT/COKEON lists when available; until then the program behaves like the legacy
cascade (no fixed-rate shortcuts).

### 3.4 `/CCBJI/T_RPPCAT_TY` — RPP category type (DECISION D-03)

| Field | Key | Type | Values |
|---|---|---|---|
| MANDT | X | MANDT | |
| RPPCAT | X | CHAR 3 | |
| RPPCAT_TYPE | | CHAR 8 | `OTHER` / `ADJACENT` / `SPECIFIC` |

Runtime default when missing: **OTHER** (out of scope) + warning, per FS §17.

## 4. Selection Screen

Block B1 (unchanged from FS §6 unless noted):
* P_RYEAR, P_POPER, P_BUKRS (obligatory, defaults as FS)
* S_BLART (no intervals, F4 from set Z_RTR_DOC_TYPE), S_RACCT, S_CHANNL,
  S_RPPCAT, S_BAC
* S_KOSTL — NEW select-option on ACDOCA-RCNTR, validated against CSKS
* P_SOURCE is DROPPED (BW memory workaround; DB-side aggregation removes the need)

Block B2 (execution mode): RB_SUMM (default) / RB_DET / RB_MNTH / RB_AUDT /
RB_POST / **RB_SIM (NEW — Simulation)**.

Block B3 (technical):
* P_MAXREC max records (default 20,000, C_MAXREC) — applies to ALV display only,
  never truncates posting/simulation totals
* **P_PTASK (NEW)** number of parallel sessions, default 4, max 20 — user-choosable
  per your instruction; used with `CALL FUNCTION ... STARTING NEW TASK` on the
  default (unnamed) server group. No RZ12 group required (your point 9).

## 5. Data Retrieval (F_GET_DATA) — performance design

All ACDOCA access is **aggregated in the database**; the 118M-row period must never
be transferred row-wise.

Flow 2 (FI source) select — one shot, DB aggregation:
```
SELECT rbukrs, ryear, poper, blart, racct, rcntr, prctr, belnr, drcrk,
       SUM( hsl ) AS hsl
  FROM acdoca
  WHERE rldnr = '0L' AND rbukrs = @p_bukrs AND ryear = @p_ryear
    AND poper = @p_poper AND racct IN @lr_expensegl AND blart IN @lr_blart_fi
    AND rcntr IN @s_kostl
  GROUP BY rbukrs, ryear, poper, blart, racct, rcntr, prctr, belnr, drcrk
```
Flow 1 (COPA source) select — same pattern over the CO-segment fields, grouped by
blart, racct, belnr, ww207_pa, ww214_pa, ww220_pa, ww223_pa, ww235_pa, ww236_pa,
ww237_pa, rcntr, scntr, prctr, matnr, awtyp, bttype (+ SUM(hsl)).
Record-type scoping per TVARVC `/CCBJI/RTR/RECTYPE` translated to AWTYP/BLART sets
(validated against query_1: VBRK/SD00 = billing (F), BKPF/BKPFF/RFBU = FI-originated,
COBK/RKIU = assessment, etc.).

Package/parallel strategy (RB_POST / RB_SIM / large display runs):
* Work units = distinct BLART × expense-GL bucket (same unit the legacy program
  looped on). Units are distributed over P_PTASK parallel tasks
  (`STARTING NEW TASK ... CALLING f_task_done ON END OF TASK`, default group).
* Each task runs the aggregated selects + calculation for its bucket and returns
  the result table; the main process merges, then posts serially (posting stays
  in the main process to keep number-range + commit control deterministic).
* Free/failed tasks: RESOURCE_FAILURE → retry inline in main process; task results
  tracked with a pending counter and WAIT UNTIL.

Master data reads: one `FOR ALL ENTRIES`-free SELECT per table on the distinct key
sets harvested from the extraction result (MARA, KNA1, CEPCT, SKAT, /CCEJ/*,
/CCBJI/* tables), all into sorted internal tables / hashed where key-unique.

Double-count prevention (ZFLAG): after both flows load, FI lines whose
(BLART, expense GL, BELNR) also appear in the COPA flow with equal amount are
zeroed (DEB_CRE_LC_CHECK logic from the HANA views, now in ABAP).


## 5a. Performance architecture (v1.2)

Three layers, code-to-data:
1. **CDS views** (Layer 1): parameterized `/CCBJI/` DDLs for the ACDOCA FI slice
   and COPA-segment slice (field projection, GL scope via association to
   T_MAP_GL/T_DOC_TYP).
2. **AMDP** (Layer 2): class `/CCBJI/CL_SUPFEE_DATA` — near-1:1 port of the
   proven BW procedure SUPPORT_FEE_STATGING_CALCULATION (6-branch UNION incl.
   RJ/AB, flag-based exclusions, ROW_NUMBER dedup, document-level SUM), reading
   the CDS views, returning a table parameter (~10-50k rows). One DB round trip;
   118M-row periods never leave HANA.
3. **ABAP** (Layer 3): incidence cascade (sorted tables/binary search), fee
   calc, ALV, posting — small data only.

P_PTASK parallel sessions apply to the ABAP-bound work: posting/simulation
BAPI fan-out (biggest wall-clock win), optional master-data enrichment, and an
extraction safety valve (AMDP split by BLART bucket) activated by config only.
HANA parallelizes the extraction internally — no aRFC needed there.
Targets: Simulation normal month < 2 min; worst month < 15 min; posting < 10
min. Prove with ST05/SQLM/PlanViz on the reconciliation month.

## 6. Calculation (F_CLASSIFY_LINES + F_PROCESS_DATA)

Per FS v2.0 §8, two flows selected via `/CCBJI/T_DOC_TYP`-SOURCE by expense GL.

FS v1.3 §8.5 replaced the priority table with a decision-flow diagram carrying
TWO numberings: business priority (GL=1, CC=2, RPPCat=3, Customer=4, Channel=5)
and PROGRAM ORDER (RPPCat=1, CC=2, Customer=3, Channel=4, GL=5). The program
order matches the legacy ABAP cascade exactly (RPP-Cat lookup -> GL+PRCTR ->
BAC+Channel -> Channel -> GL-only), with the GL fixed-rate routing (50% Support
GL / CokeON GL) as an up-front branch. Two interpretation points to confirm
(O-10): (a) diagram shows "Adjacent Category + Specific GL (Rebate, Spot Only)
-> Apply 50%" as one combined condition — AND or two rules? (b) "Other
Category" now flows on through CC/Customer/Channel checks (v2.0 said skip) —
confirm out-of-scope vs continue.

Priority cascade (both flows), implemented over sorted tables with BINARY SEARCH:

| Prio | Rule | Rate source | CAL_RULES text |
|---|---|---|---|
| 0 | GL_TYPE = 50PCT | TVARVC `/CCBJI/RTR_50PCT` | 50%_FIXED |
| 0 | GL_TYPE = COKEON (Flow 1) | TVARVC `/CCBJI/RTR_VENDPCT` | VENDING_CH |
| 1 | RPPCAT type OTHER (Flow 1) | — skip line (out of scope) | — |
| 1 | RPPCAT type ADJACENT (Flow 1) | TVARVC `/CCBJI/RTR_50PCT` | 50%_FIXED |
| 2 | SOURCE+RPPCAT+RACCT | T_SUP_FEE | TEXT-027 RPP Category-GL |
| 3 | SOURCE+RACCT+KOSTL(=PRCTR) | T_SUP_FEE | TEXT-049/048 GL-CC |
| 4 | SOURCE+RACCT+BAC+CHANNEL | T_SUP_FEE | TEXT-028 BAC & Channel |
| 5 | RACCT+CHANNEL | T_SUP_FEE | TEXT-029 Channel |
| 6 | SOURCE+RACCT | T_SUP_FEE | TEXT-026/030 FI-GL / COPA-GL |

Notes:
* Priorities 2–6 replicate the legacy binary-search cascade exactly (MOD-002/003/006
  behavior: the T_SUP_FEE "KOSTL" key column is matched against the line's PRCTR).
* Priorities 0–1 are the FS v2.0 additions; they activate only when
  `/CCBJI/T_GL_TYPE` / `/CCBJI/T_RPPCAT_TY` rows exist (D-02/D-03), so cutover
  can go live matching legacy results 1:1 and business logic switches on by config.
* Fee = `ROUND(cost × incidence% / 100)` via `J_1I6_ROUND_TO_NEAREST_AMT`, JPY.
* Cost = 0 after aggregation → skip (CONTINUE), as legacy.

## 7. Derivations

1. **Brand WW237_New**: COPA WW237_PA if filled, else SCNTR-based brand fallback
   (legacy SKOST). **Packaging**: WW226_PA (FS §4.2 wrongly says WW223; WW223_PA
   is RPP Category — confirmed in KEA5).
2. **Revenue mgmt plant (KMVKBU)**: `/CCEJ/TZKTSD0004`-ZKZSMWERKS by WW207 dealer.
3. **KAM chain**: WW235_PA (fallback `/CCEJ/TEJACC` by BAC) → `/CCEJ/TKAMREP` →
   KAMORGL4CD → `/CCBJI/T_KAM_L4_CC` → KOSTL.
4. **Profit center (F_DERIVE_PROFIT_CENTER)** — DECISION D-04 (replaces the
   nonexistent /CCEJ/T_PROFCTRPH fallback):
   1. PC-reposting types (BTTYPE/AWTYP per §5 mapping) → PRCTR = CSKS-PRCTR of
      SCNTR (sender cost center), else SCNTR value itself if no CSKS row.
   2. KAM L4 match → KOSTL from `/CCBJI/T_KAM_L4_CC` → PRCTR from CSKS
      (KOKRS = JP00 equivalent, date-valid row).
   3. Else **ACDOCA-PRCTR of the line itself** (in S/4 every journal line already
      carries a profit center — this replaces the old top-1 fallback table).
   4. PRCTR still empty → TVARVC `/CCBJI/RTR_DEFPC` if maintained, else keep blank
      and log warning. No hard error.
5. **Dummy cost center**: TVARVC `/CCBJI/RTR_KOSTL` (LOW = BUKRS, HIGH = dummy
   `ZZZZZZZZZZ`) is kept read-compatible but only used where the legacy MOD-004
   path still applies (COPA display of alphanumeric cost centers).

## 8. TVARVC entries (new/retained in S/4)

| Name | Type | Content | Status |
|---|---|---|---|
| `/CCBJI/RTR/RECTYPE` | S | 4,6,7,8,9,B (from BW) | migrate as-is |
| `/CCBJI/RTR_KOSTL` | S | 7827→ZZZZZZZZZZ; 7830→ZZZZZZZZZZ | migrate as-is |
| `/CCBJI/RTR_50PCT` | P | `50.00` | NEW (D-05: fixed rate configurable, not hardcoded) |
| `/CCBJI/RTR_VENDPCT` | P | vending-channel % for CokeON GLs | NEW (value TBD by business; empty = rule inactive) |
| `/CCBJI/RTR_DEFPC` | P | default profit center (optional) | NEW (D-04 step 4) |
| `/CCBJI/RTR_DEBITGL` | P | debit GL of the aggregated debit line | **RETAINED — value to be copied from ECC TVARVC** |
| `/CCBJI/RTR_KOSTL` (ECC variant) | P/S | profit center for the debit line | **RETAINED — value to be copied from ECC TVARVC** (note: BW entry of same name = dummy cost center; ECC entry = debit-line PRCTR — two different meanings, ECC one is the posting-relevant one) |
| `/CCEJ/RTR/HANA_CONN`, `/CCBJI/RTR_SFEE_AL11`, `RTR_BSCHL`, `RTR_COUNT`, `RTR/POSTING_GL`, `RTR_SFEE_TAXGL` | | | RETIRED (file/HANA-era; BSCHL replaced by DRCRK; TAXGL was dead code in ECC) |

DECISION D-06 (cost center scope, Flow 2): **scope = `/CCBJI/T_SUP_FEE` itself** —
a GL+PRCTR(/KOSTL-key) combination present in the incidence master is in scope;
no separate scope table. S_KOSTL additionally filters at selection when entered.
This is exactly how the legacy cascade behaved and keeps one source of truth.

## 9. KAM L4 Upload Program `/CCBJI/RUFIGLR_KAML4_UPLOAD`

Modeled on ZJRTR_RUCOPBR_HANA_ALLOC_UPLD (KAM_L4 branch) with the gaps fixed:
* CSV col1 = KAMORGL4CD, col2 = KOSTL (same file layout users know today)
* NEW validations: KOSTL exists in CSKS (date-valid), KAMORGL4CD not blank,
  duplicate check within file, row-level error report (ALV) instead of
  first-error-abort; nothing written unless file is error-free (all-or-nothing)
* Actions: Upload (upsert) / Display / Delete-all (with confirm popup)
* AUTHORITY-CHECK S_TCODE + F_BKPF_BUK-style display/change split (legacy had none)

## 10. Posting Mode (RB_POST) & Simulation (RB_SIM)

Replaces the two-step file chain (BW `F_BAPI_PROCESS` file → ECC
`/CCBJI/RUFIGLR_SUPPFI_POST`) with direct posting. The document construction
below replicates the **actual ECC program** (analyzed from source, closing O-01):

1. **Document type `YE` (confirmed — hard-coded in ECC program).** Header:
   BUS_ACT blank (defaults RFBU), COMP_CODE = P_BUKRS, DOC_DATE = sy-datlo,
   PSTNG_DATE = last day of selected period, FISC_YEAR/FIS_PERIOD from selection,
   HEADER_TXT = "<JP support-fee text> YYYY/PP nn/NN", USERNAME = sy-uname,
   CURRENCY_ISO `JPY` on every CURRENCYAMOUNT row.
   **XREF1_HD** (= COPA docnr + BUKRS + GJAHR; docnr from number range
   Z1/Z_SUPP_FEE for COPA source, = SOURCE literal for FI) passed via
   **EXTENSION1 + `ACC_DOCUMENT` BAdI implementation** (object #21) — exactly as
   ECC did. XBLNR stays empty (ECC parsed ref_doc_no but never mapped it).
2. **Line structure (per ECC program, corrects FS §10):**
   * ONE aggregated **debit line per document**: GL from TVARVC
     `/CCBJI/RTR_DEBITGL`, PROFIT_CTR from TVARVC `/CCBJI/RTR_KOSTL`
     (ECC meaning: debit-line profit center), amount = document gross total
     (positive).
   * One **credit line per result record**: POSTING_GL_ACC, PRCTR of the record,
     TAX_CODE, ITEM_TEXT = fixed JP text + channel, amount = supp_fee × −1.
     COSTCENTER filled only when the credit GL is a cost element (CSKB check,
     JP00) and the cost center exists in CSKS — else the record errors (as ECC
     MOD-002).
   * **Tax lines — explicit, not BAPI auto-calc**: per credit record
     `CALCULATE_TAX_FROM_NET_AMOUNT` (BUKRS, tax code, JPY, net fee), each
     condition rounded via `J_1I6_ROUND_TO_NEAREST_AMT`; tax ≠ 0 → extra
     CURRENCYAMOUNT row (tax × −1) + ACCOUNTTAX row (COND_KEY, ACCT_KEY,
     tax code).
   * Split at **400 ACCOUNTGL lines per document** (ECC MOD-001 limit; the 800
     was only the BW file-record split and is obsolete).
3. **Simulation (RB_SIM)**: identical build, `BAPI_ACC_DOCUMENT_CHECK` per
   document, no commit; ALV of would-be documents (doc count, per-doc totals,
   per-line messages). Mirrors the ECC program's own simulation radiobutton.
4. **Posting**: per document — POST then `BAPI_TRANSACTION_COMMIT` (WAIT 'X'),
   ROLLBACK on error; continue with next document; summary ALV at end. Amounts
   passed verbatim (no ×100 scaling — confirmed in ECC code).
5. **Re-run protection (DECISION D-07)**: ECC had NONE in-system (file archiving
   only), which no longer exists → mandatory here: before posting, SELECT
   existing BKPF/ACDOCA docs BLART `YE`, RBUKRS/RYEAR/POPER (+ XREF1_HD pattern);
   if found, hard stop with list unless checkbox P_FORCE is set.
6. COPA-source documents additionally carry the profitability-segment CRITERIA
   table (WW-characteristics per §7 + KSTAR from `/CCBJI/T_SUP_COS` flag
   lookup) for account-based COPA — one BAPI for both flows (FS v2.0). Note the
   ECC program posted NO profitability segment (COPA went via RKEVEXT3 in BW
   era); this is the one deliberate functional addition — flag for CO functional
   validation (O-08).

## 11. ALV Output

Summary / Detail / Monthly / JC-Audit views unchanged in content
(FS §12 + legacy structures, now `/CCBJI/S_SUPFEE_*`); new columns GL_TYPE,
RPPCAT_TYPE, AWTYP, BTTYPE in Detail; CAL_RULES texts from the migrated text
symbols. SALV OM (`cl_salv_table`) instead of REUSE_ALV* (S/4 standard), hotspot
FB03 on FI doc number retained.

## 12. Error handling / messages

Message class `/CCBJI/MC_SUPFEE`; all FS §17 scenarios covered; new: parallel-task
failure (retry note), simulation summary, re-run protection block, GL-type /
RPPCAT-type defaulting warnings, KAM-L4-not-found info.

## 13. Open items

| # | Item | Owner | Blocking? |
|---|---|---|---|
| O-01 | ~~ECC consumer program field mapping~~ **CLOSED** — /CCBJI/RUFIGLR_SUPPFI_POST source analyzed; §10 updated | — | Closed |
| O-02 | Reconciliation benchmark: migrated line-level history FY2024–2025 exists in ACDOCA (BA/SB docs, AWTYP=BKPFF). Export one month (RYEAR 2025, POPER 10, BLART BA, all RACCT) for the totals target | Vaibhav | Needed before go-live sign-off, not for build |
| O-03 | Business values for `/CCBJI/T_GL_TYPE`, `/CCBJI/T_RPPCAT_TY`, `RTR_VENDPCT` | Functional | No — rules dormant until config filled (D-02/D-03) |
| O-04 | Package name + transport | Basis | Before object creation |
| O-05 | Confirm KOKRS value for CSKS reads (assumed JP00-equivalent controlling area) | Functional | Minor |
| O-06 | Doc type: ECC program hard-codes YE, but migrated ACDOCA history shows BA/SB on the posting GLs — likely migration remapping or adjacent process. Verify one BA doc in FB03; design proceeds with YE | Vaibhav/Functional | No |
| O-07 | Copy ECC TVARVC values `/CCBJI/RTR_DEBITGL` + `/CCBJI/RTR_KOSTL` (debit GL + debit-line profit center) from ECC STVARV | Vaibhav (ECC) | Before first posting test |
| O-08 | ECC never posted a profitability segment; account-based COPA CRITERIA on credit lines is the S/4 addition per FS v2.0 — CO functional to validate characteristic list | CO Functional | Before posting go-live |
| O-09 | Verify existing `/CCBJI/T_SUP_COS` structure + content vs BW /BIC/AZJSUFEECE2 (16 rows) | Vaibhav (S/4 SE11/SE16) | Before COPA posting build |
| O-10 | Confirm two §8.5 diagram readings: Adjacent+SpecificGL combined condition (AND vs two rules); Other Category continue-vs-skip | Functional | Before cascade build |
| O-11 | Target S/4 ABAP release + Basis level (CDS/Open-SQL syntax level) | Basis | Before CDS build |
| O-12 | KAM L4 upload program: business checking if mapping can be derived from system directly — upload program (+TCode) built only if manual upload stays | Business/Functional | Phase 6 |

## 14. Decisions taken (for the record)

* D-01 KAM L4 table without validity dates (mirrors real BW table)
* D-02 GL type = new table `/CCBJI/T_GL_TYPE`, default OTHER + warning
* D-03 RPP category type = new table `/CCBJI/T_RPPCAT_TY`, default OTHER + warning
* D-04 Profit center: SCNTR override → KAM L4 → line's own ACDOCA-PRCTR → TVARVC default (replaces nonexistent /CCEJ/T_PROFCTRPH)
* D-05 50% and Vending rates in TVARVC (no hardcoding)
* D-06 Flow-2 cost center scope = presence in `/CCBJI/T_SUP_FEE` (no extra scope table)
* D-07 Re-run protection via YE-doc + XREF1 check with explicit override checkbox
* D-08 Parallelization: STARTING NEW TASK, default server group, user-selectable session count (P_PTASK); posting serialized in main task
* D-09 P_SOURCE dropped; Simulation mode added (RB_SIM)
* D-10 Packaging code = WW226_PA (FS §4.2 correction; WW223_PA is RPP Category)
* D-11 FI document layout = replica of ECC /CCBJI/RUFIGLR_SUPPFI_POST: doc type YE, one aggregated debit line (TVARVC GL + PRCTR), per-record credit lines, explicit tax lines (CALCULATE_TAX_FROM_NET_AMOUNT + ACCOUNTTAX), 400-line split, EXTENSION1/BAdI for XREF1_HD, per-document commit
