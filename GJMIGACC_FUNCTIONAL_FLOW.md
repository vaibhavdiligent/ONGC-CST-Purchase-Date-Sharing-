# GJMIGACC – Functional Flow and Z-Copy Approach

**Source analysed:** `GJMIGACC_code.pdf` (report `RGJV_UPDATE_ACC_DATA_IN_ACDOCA`) and
`GJMIGACC_c_lass.pdf` / `GJMIGACC_c_lass_1.pdf` (class `CL_JVA_MIG_CORRECT_ACDOCA`, package `GJVA_MIG`,
system OCP, release 816). The two class PDFs have identical content; only the print time differs.

**How to read this document:** steps marked *(inferred)* come from the interface methods
`EXECUTE_PROCESS` and `PARALL_*`, which are not in the class print. They were reconstructed from
the private methods that only these interface methods call (`GET_GLOBAL_SETTINGS`,
`PROCESS_DATA_PARALLELLY`, `SAVE_RESULTS`, `SAVE_RESULTS_GLOBAL`, `DISPLAY_RESULTS`). Everything
else comes directly from the printed source.

---

## 1. Purpose (business view)

GJMIGACC is an SAP-delivered **Joint Venture Accounting (JVA) reconciliation and correction tool** for S/4HANA.

In S/4HANA, JVA data (venture, equity group, partner, recovery indicator, billing indicator,
production period, cost objects) is carried on the Universal Journal **ACDOCA**. The classic JV
ledger tables **JVSO1** (ledger 4A – JV line items) and **JVSO2** (ledger 4B – operator/billing
items) are the "source of truth" from the JVA side.

The program:

1. Reads JV documents from JVSO1/JVSO2 for the selected company codes and periods.
2. Finds the matching ACDOCA lines (by FI doc, CO doc, assignment, preceding-document reference or migration source).
3. Compares the two line by line (amounts and JVA attributes) and, optionally, compares balances.
4. Classifies every document into a **correction type**.
5. In analysis or test mode it only reports. In update mode it **corrects ACDOCA** by updating lines,
   reversing and reposting lines or documents, or posting missing lines or documents.
6. Writes every correction to the log table **JVA_ACD_UPD_LOG** and shows an ALV result or a spool list.

Typical use is after S/4HANA conversion, or when ACDOCA's JV fields have drifted from the JV ledger.

---

## 2. Objects involved

| Object | Type | Role |
|---|---|---|
| `GJMIGACC` | Transaction | Starts the report |
| `RGJV_UPDATE_ACC_DATA_IN_ACDOCA` | Report | Selection screen, maps parameters, parallel-processing callbacks |
| `IF_JVA_MIG_CORRECT_ACDOCA` | Interface | Parameter structure `GTY_PROGRAM_PARAMETERS`, `EXECUTE_PROCESS`, `PARALL_*` |
| `CL_JVA_MIG_CORRECT_ACDOCA` | Class (final, private instantiation, singleton) | All selection, comparison and correction logic |
| `CL_JVA_MIG_POST_ACDOCA` | Class | Does the postings: `UPDATE_ACDOCA_LINE`, `REV_AND_REP_ACDOCA_LINE`, `POST_MISSING_ACDOCA_LINE`, `REVERSE_ACDOCA_DOC`, `POST_NEW_ACDOCA_DOC` |
| `CL_JVA_MIG_POST_FI_OP` | Class | Posts missing cutback and cash-call operator documents |
| `CL_JVA_MIG_ALV_OUTPUT` | Class | ALV and spool output, drill-down |
| `CL_JVA_SETTINGS`, `CL_JVA_UTIL`, `CL_JVA_OUTPUT` | Classes | JV company code settings, utilities, popups and SPTA wrapper |
| `JV_AUTHORITY_CHECK_PROCESS`, `JV_GET_LEDGER`, `FI_SPLIT_ACTIVATION`, `JV_UPDATE_ACD_CORR_LOG` | Function modules | Authority check, leading ledger, NewGL split check, log update (update task) |
| `JVSO1`, `JVSO2`, `T8JZ`, `T8JZ_FAGL`, `FINSC_LD_CMP`, `FAGL_SPLIT_FIELD`, `COBK` | Tables (read) | JV ledger and configuration |
| `ACDOCA` | Table (read and **changed**) | Universal Journal |
| `JVA_ACD_UPD_LOG` | Table (written) | Correction log; also used to find documents already reposted in another year |

---

## 3. Selection screen

| Block | Fields | Meaning |
|---|---|---|
| Selection | `S_BUKRS`, `S_RYEAR`, `S_POPER`, `S_BELNR` (FI doc), `S_REFNR` (CO doc) | Main selection on JVSO1 |
| Additional selection (collapsed) | Posting date, document date, G/L account, venture, equity group, recovery indicator, billing indicator, production period, cost center, order, WBS, network, operation | More JVSO1 filters |
| Document types | `P_FIDOCS` (FI, default on), `P_CODOCS` (CO), `P_PPPER` / `P_PPDAY` (production-period handling), `P_EXCLBL` (exclude balancing lines, default on) | Which JV line types to check |
| Ledger | `P_ALLLDS` (all ledgers, default) / `P_JVLEDG` (JV leading ledger only) | ACDOCA ledgers to correct |
| Accounting entities to check | `P_VNAME`, `P_EGRUP`, `P_VPTNR`, `P_RECID`, `P_BILID`, `P_PRODP`, `P_COBS` (CO objects on balance-sheet lines), `P_COCO` (CO objects on CO lines) | Which fields are compared |
| Update options (collapsed) | For each field: **Update** the line in place (`*UPD`) or **Reverse and repost** (`*REP`) | How to correct a field difference. Defaults: update in place, except CO-on-CO lines, which default to repost |
| Posting date | `P_ORIPD` (original date, default) / `P_NEWPD` + `P_BUDAT` | Posting date for correction documents |
| Processing | `P_ANALYS` (analysis only) / `P_TEST` (test run, default) / `P_UPDATE` (update) | Run mode |
| | `P_BALOV` (balance check overrides line check), `P_HEUR` (heuristic line assignment) | Matching behaviour |
| Bulk | `P_BLDOCH` (doc headers in memory, 500,000), `P_BLDLPR` (docs per line-processing bulk, 500) | Memory and performance |
| Parallel | `P_PARALL`, `P_RFCGRP`, `P_MXTSKS` (20), `P_DOCTSK` (1,000,000), `P_SPTASK` | SPTA parallel processing |
| Log | `P_LUPTSK` (update task), `P_LBULK` (1,000), `P_LGLTMS` / `P_LUPTMS` (timestamp) | How `JVA_ACD_UPD_LOG` is written |
| Output | `P_SHWALL` / `P_UPDERR` (default) / `P_ERRONL`, `P_SPFOR`, `P_NODET`, `P_SE16N` | Output detail |

The report hard-codes all `POST_*` flags (equity change, suspense, equity adjustment,
farm-in/out, cutback, cash call) and `update_existing_lines` / `post_new_documents` to `abap_false`.
So **GJMIGACC only processes FI- and CO-referenced JV documents**. The internal JV document path
(equity change, cutback and so on) is used by other SAP reports that call the same class.

---

## 4. Process flow

### 4.0 Prerequisites checked at run time

The run stops with an error if any of these is not met:

| Check | Where | Message |
|---|---|---|
| At least one selected company code is a JV company code (T8JZ) | `CHECK_COCODES_AND_AUTHORITIES` | G5 802 |
| User has JV process authorisation CUTBACK2, activity 16 (update) or 48 (test/analysis). Company codes without authority are dropped; the run stops only if none are left | `CHECK_COCODES_AND_AUTHORITIES` (`JV_AUTHORITY_CHECK_PROCESS`) | G4 292 |
| **Document splitting is active** for the company codes | `GET_SPLIT_CRITERIA` (`CL_FAGL_SPLIT_SERVICES=>GET_ACTIVE_CC`) | G5 826 |
| **Venture (VNAME) is a document-splitting characteristic** (FAGL_SPLIT_FIELD) | `GET_SPLIT_CRITERIA` | G5 827 |
| A server group is entered when parallel processing is on | `GET_GLOBAL_SETTINGS` | SPTA 002 |
| JV settings can be read for the company code | `GET_COMPANY_CODE_SETTINGS` (`CL_JVA_SETTINGS`) | GI 701 (that company code is skipped) |
| More than 100,000 JV documents in a dialog run | `GET_GLOBAL_SETTINGS` | Confirmation popup; "No" cancels |

### 4.1 Overall call flow

```
GJMIGACC
 └─ RGJV_UPDATE_ACC_DATA_IN_ACDOCA
     INITIALIZATION / AT SELECTION-SCREEN   → collapse/expand blocks, range checks on bulk/parallel fields
     END-OF-SELECTION → FORM run_process
        ├─ build GTY_PROGRAM_PARAMETERS from the screen
        └─ CL_JVA_MIG_CORRECT_ACDOCA=>get_instance( )->execute_process( params )   (inferred body)
              │   store params in MS_PROGPARS
              │
              ├─ GET_GLOBAL_SETTINGS              (error → skip to DISPLAY_RESULTS with the messages)
              │    ├─ CHECK_COCODES_AND_AUTHORITIES
              │    │     • company codes = T8JZ ∩ S_BUKRS (none → error G5 802)
              │    │     • JV_AUTHORITY_CHECK_PROCESS, process CUTBACK2,
              │    │       activity 16 (update) or 48 (test/analysis); drops unauthorised company codes
              │    │     • JV_GET_LEDGER → JV leading ledger
              │    ├─ GET_SPLIT_CRITERIA (doc splitting active? VNAME a split field? else G5 826/827)
              │    ├─ default/validate bulk & parallel sizes
              │    ├─ PREPARE_PERIOD_RANGE (no period entered → 001–012)
              │    ├─ PREPARE_ACTIVITY_RANGE_FI_CO (FI: fi_doc + fi_doc_mm, CO: co_doc)
              │    ├─ dialog only: COUNT_JV_DOCUMENTS_TOTAL > 100,000 → "run in foreground?" popup
              │    └─ decide spool output (batch, or spool per task)
              │
              ├─ Parallel?  PROCESS_DATA_PARALLELLY
              │      • CALCULATE_TASK_PACKAGES per company code (packages by year/period, ~P_DOCTSK docs each)
              │      • ≤ 1 package → falls back to synchronous
              │      • SPTA_PARA_PROCESS_START_2 → FORM BEFORE_RFC / IN_RFC / AFTER_RFC in the report
              │        (each task runs PROCESS_COMPANY_CODE for its periods; see 4.4)
              │  else PROCESS_DATA_SYNCHRONOUSLY → loop company codes → PROCESS_COMPANY_CODE
              │        (print a spool per company code in background; SAVE_RESULTS)
              │
              │  PROCESS_COMPANY_CODE (per company code or task)
              │     ├─ GET_COMPANY_CODE_SETTINGS
              │     │     • CL_JVA_SETTINGS (T8JZ), NewGL split active? (T8JZ_FAGL + FI_SPLIT_ACTIVATION)
              │     │     • GET_LEDGERS: JV ledger, or all ledgers in FINSC_LD_CMP
              │     │       (also UPDATE JVA_ACD_UPD_LOG set RLDNR_ACD + COMMIT)
              │     │     • currencies (GL and classic JV)
              │     │     • migrated docs present? (ACDOCA-MIG_SOURCE; 'J' = JV migrated)
              │     ├─ RETRIEVE_AND_PROCESS_DATA        (see 4.2)
              │     └─ UPDATE_LOG_DB_TABLE              (flush JVA_ACD_UPD_LOG + COMMIT)
              │
              ├─ SAVE_RESULTS_GLOBAL      (inferred call) totals over all company codes and tasks,
              │                            CALCULATE_GLOBAL_RUNTIMES, overall spool if printing
              └─ DISPLAY_RESULTS          (inferred call) nothing found → message GI 708;
                                           spool mode → DISPLAY_SPOOLS; else ALV for 1 or n company codes
        Output: CL_JVA_MIG_ALV_OUTPUT (ALV, or spool; AT LINE-SELECTION shows a spool)
```

### 4.2 RETRIEVE_AND_PROCESS_DATA (per company code, in bulks)

1. **Read JV document keys.** `RETRIEVE_JV_DOCUMENTS` reads `SELECT DISTINCT rldnr, rbukrs, ryear,
   docnr, reffidoc, refdocnr, activ FROM jvso1 WHERE rldnr='4A'`, filtered by company code,
   year/period, FI/CO reference and the additional selections. It reads up to `P_BLDOCH` rows at a
   time (OFFSET paging) and repeats until the last bulk.
2. **Group.** Lines without the required reference are skipped: FI lines need REFFIDOC, CO lines need
   REFDOCNR. JV documents with the same FI or CO reference are counted once. Every `P_BLDLPR`
   documents form a processing bulk.
3. **Per bulk:**
   * `RETRIEVE_FI_DOC_HEADER_DATA` reads BKPF (AWKEY, GLVOR, TCODE, BSTAT, BKTXT) and COBK, then
     decides what each JV document really is:
     * Documents created by JV internal transactions (TCODE or BKTXT starting with **GJEC, GJ19, GJ17,
       GJFARM**: equity change, cutback, cash call, farm-in/out) are **not** treated as normal FI documents.
       For FI-line types they are dropped from this run.
     * A JV CO line whose FI header has GLVOR = **COFI** has a wrong REFFIDOC and is dropped.
       Otherwise it is re-classified as an FI document.
     * If the controlling area is missing, it is taken from TKA02.
   * `RETRIEVE_JV_DOC_LINES` reads the full JVSO1 lines (ledger 4A, plus 4C when JV classic currencies
     3/4 are used) and merges 4A with 4C (`COMBINE_AND_ANALYZE_JV_LINES`). It also collects the
     preceding-document references used for the `IN` and `MI` searches below.
   * `RETRIEVE_LOG_ITEMS_DIFF_YEARS` finds documents this tool already reposted in another fiscal year, from `JVA_ACD_UPD_LOG`
     (`GJAHR_NEW` ≠ JV year). These are re-read so a second run does not correct the same document twice.
   * `RETRIEVE_ACDOCA_LINES` (ledger = JV ledger, or all ledgers), by type:
     * `FI`: `belnr = reffidoc`
     * `CO`: `co_belnr = refdocnr`
     * `IN`: lines posted by ACDOCA-only postings (`vorgn='RFRA'`, `prec_aw*` = JV reference)
     * `MI`: lines migrated during the S/4 conversion (`mig_source='J'` or `awtyp` JVAME/JVAM)
     * `LG`: correction documents this tool posted earlier (from the log)
     * Totals-correction migration lines (`mig_source` S/T/U/V) are excluded.
   * → `COMPARE_AND_CORRECT_DOCUMENTS`

### 4.3 COMPARE_AND_CORRECT_DOCUMENTS (per JV document group × ledger)

1. **Build the JV line set.** The program ignores:
   * zero-amount lines
   * balancing lines, if `P_EXCLBL` is set
   * FI/CO lines without a reference

   For FI/CO documents it also accumulates JV balances (`PREPARE_JVSO_BALANCES`).
   `DETERMINE_JV_DOC_TYPE` sets the document type (FI doc / CO doc) from ACTIV.
2. **Per ledger** (JV ledger first, then the other ledgers that have the document):
   * `GET_ACDOCA_DOC_LINES` / `PREPARE_ACDOCA_DOCUMENT` build the matching ACDOCA document and ACDOCA balances:
     * irrelevant migration lines (MIG_SOURCE set, not 'J'/'G', no venture) are dropped
     * with `P_EXCLBL`, venture balancing lines of a real FI document (BSTAT initial) are dropped:
       lines without BUZEI/CO_BUZEI/AWITEM, or without AWITEM for migrated documents
     * zero-amount lines are dropped
     * the JV amounts are derived per ACDOCA line (`DETERMINE_JV_AMNTS_IN_ACD_LINE`)
   * The program handles migrated documents whose balancing lines were dropped, and repostings
     (`CHECK_FOR_REPOSTINGS_JV/ACDOCA`).
3. **`COMPARE_LINES`** assigns every JV line to an ACDOCA line:
   * **By reference** (`GET_ACDOCA_LINE_BY_REF`): BUZEI = REFFIDLN, then AWITEM, CO_BUZEI = REFDOCLN, the log table, then migrated lines.
   * **By accounting data** (`GET_ACDOCA_LINE_BY_ACC_DATA` / `MULTIPLE_ACD_LINE_PROCESSING`):
     amount, then profit center, venture, equity group, recovery indicator, cost objects, until exactly one line matches.
     ACDOCA lines may be aggregated (`COMPRESS_JV_DOC`).
   * **Heuristic retry** (`P_HEUR`): if the result is "correction not possible", the method calls itself
     up to 3 times with other assignments and keeps the result with the fewest errors.
4. **`COMPARE_FIELD_VALUES`** compares each assigned pair and records issues:

   | Check | ACDOCA field | JVSO field | Issue |
   |---|---|---|---|
   | Amount / sign | WSL, HSL, KSL, HSL_4C, KSL_4C | TSL, HSL, KSL … | wrong_amount / wrong_amount_sign |
   | Venture | VNAME | RJVNAM | wrong_venture |
   | Equity group | EGRUP | REGROU | wrong_equity_group |
   | Billing indicator | BTYPE | BILID | wrong_billing_indicator |
   | Partner (4A only) | VPTNR | RPARTN | wrong_partner |
   | Recovery indicator (4A) | RECID (also the CO sub-key) | RRECIN | wrong_recovery_indicator |
   | Production period (4A) | PRODPER | PRODPER | wrong_prodper |
   | Cost objects (4A) | cost center, order, WBS, network, operation | RCNTR, RORDNR, RPROJK, NPLNR, VORNR | wrong_cost_center / order / wbs / network |

   A field is only checked if its check box is set on the screen.
5. **`DETERMINE_CORRECTION_TYPE`** maps each issue to a correction. The most severe one wins for the document:

   | Issue | Correction |
   |---|---|
   | Line missing in ACDOCA (FI/CO doc) | **Correction not possible** (reported only) |
   | Balancing-line issue | Correction not required |
   | Wrong amount / sign | **Reverse and repost document** |
   | Wrong JVA attribute (venture, equity group, partner, recovery indicator, billing indicator, production period) | `*REP` option → **Reverse and repost line**; `*UPD` option → **Update line** |
   | Wrong CO object | Same, using `P_COCO`/`P_COREP` for CO lines and `P_COBS`/`P_BSREP` for balance-sheet lines |
   | Lines to reverse, or only some lines missing, on an FI/CO doc | Correction not possible |

   If `P_BALOV` is set and JV and GL balances match, `COMPARE_BALANCES` downgrades the result to
   "Correction not required – line assignment failures occurred, but the balances match".
6. **Execute the correction**, depending on the correction type:

   | Correction type | Method | Posting class call |
   |---|---|---|
   | Update line | `CORRECT_ACDOCA_LINES` → `CORRECT_ACDOCA_LINE` | `CL_JVA_MIG_POST_ACDOCA->UPDATE_ACDOCA_LINE` (updates the existing ACDOCA line in place) |
   | Reverse and repost line | same | `->REV_AND_REP_ACDOCA_LINE` (reversal line plus new line, at the original or `P_BUDAT` date) |
   | Post missing line | same | `->POST_MISSING_ACDOCA_LINE` |
   | Reverse and repost document | `REV_AND_REP_ACDOCA_DOC` | `->REVERSE_ACDOCA_DOC`, then `->POST_NEW_ACDOCA_DOC` (stops if one JV doc maps to more than one ACDOCA document) |
   | Post new document | `POST_MISSING_DOCUMENT` | `->POST_NEW_ACDOCA_DOC`, or `CL_JVA_MIG_POST_FI_OP` for cutback/cash call (not reached from GJMIGACC) |
   | Correction not possible / not required / do not correct | — | Reported only |

   * **Analysis only:** nothing is called; the issues are listed as warnings.
   * **Test run:** the posting classes are called with `iv_test = 'X'` and the log shows `BELNR_NEW = 'TEST'`.
   * **Update:** successful corrections go to `JVA_ACD_UPD_LOG` with the old and new BELNR/GJAHR/DOCLN,
     the reversal document, the issues and the message. The log is written in bulks of `P_LBULK`,
     directly or through `JV_UPDATE_ACD_CORR_LOG` in update task, followed by COMMIT WORK.
7. **Statistics and output.** Document and line statistics are counted: processed, issues, updates and
   alignment failures. Item info is collected for the ALV output (all lines, updates and errors, or
   errors only). The results go to an ALV list in dialog, or to a spool per company code or task in
   background.

### 4.4 Parallel processing (SPTA)

When `P_PARALL` is set and there is more than one work package:

1. **Packages.** `CALCULATE_TASK_PACKAGES` counts JV documents per company code, year and period
   (`COUNT_JV_DOCUMENTS_BY_PERIOD`). Periods are added to a package until it holds about `P_DOCTSK`
   documents, with a minimum of 10,000 per package. A task never splits a period. Each package
   becomes one task with ID `<company code>_<n>`.
2. **`FORM BEFORE_RFC` → `PARALL_BEFORE_RFC`** *(inferred)*: takes the next task from `MT_TASKDATA` and
   passes its parameters (company code and periods) to the RFC.
3. **`FORM IN_RFC` → `PARALL_IN_RFC`** *(inferred)*: runs in a separate work process.
   `RETRIEVE_JV_DOCUMENTS` limits the selection to the package's year/period list.
   `PROCESS_COMPANY_CODE` then runs the full retrieve, compare and correct cycle and writes
   `JVA_ACD_UPD_LOG`. Results and runtimes are returned.
4. **`FORM AFTER_RFC` → `PARALL_AFTER_RFC`** *(inferred)*: `SAVE_RESULTS` merges the task result into its
   company code (`UPDATE_TASK_INFO` marks the task returned). Once all tasks of a company code are back,
   a spool can be printed for it.
5. SPTA errors (server group not found, no resources) are reported as SHDB_PFW 719, SPTA 005 or FCMLACC 160.

Each task commits its own postings and log entries. A failed task does not roll back the other tasks.

---

## 5. Where GJMIGACC fits – related SAP reports and notes

GJMIGACC is one of a set of SAP correction reports for moving to, or switching on, JVA on ACDOCA.
Public SAP sources describe it this way:

| Report / transaction | Purpose |
|---|---|
| `RGJV_UPDATE_INT_DOCS_IN_ACDOCA` / **GJMIGINT** | Creates missing **internal** JV documents in ACDOCA (equity change and adjustment, suspense/unsuspense, operator documents such as cutback and cash call). SAP recommends running this **first**. |
| `RGJV_UPDATE_ACC_DATA_IN_ACDOCA` / **GJMIGACC** | Updates ACDOCA with accounting data from the JV ledgers for FI/CO documents, where cost object and JV information differ (this document) |
| `RGJV_UPDATE_CBRUNID_IN_ACDOCA` | Updates historic ACDOCA data so that cutback does not pick it up again |

This matches the code: GJMIGINT and GJMIGACC share the same class. GJMIGACC switches off the
internal-document flags (`POST_*`) that GJMIGINT uses.

Prerequisites stated by SAP: New G/L with the G/L splitter and New G/L–JVA integration. The code
enforces this with messages G5 826/827 (section 4.0).

SAP Notes to read in SAP for Me (login required; not readable from here):
* **3058813** – JVA ACDOCA table update reports (documentation of these reports)
* **2941622** – Disclaimers for migration and switch scenarios to JVA on ACDOCA

Public sources:
* [SAP blog – Migration and Switching to Joint Venture Accounting on ACDOCA (23.03.2022)](https://blogs.sap.com/2022/03/23/migration-and-switching-to-joint-venture-accounting-on-acdoca/)
* [SAP Help – JVA on ACDOCA](https://help.sap.com/docs/SAP_S4HANA_ON-PREMISE/f049a59301a94f1c9bb60ca8394db217/06421bf5a2ad4af28c8d1f6904a214ec.html)

---

## 6. Creating a Z version – recommendation

### 6.1 What is copyable and what is not

* The **report** is a thin shell: selection screen, parameter mapping and three SPTA callback FORMs.
  It is easy to copy to `ZRGJV_UPDATE_ACC_DATA_IN_ACDOCA` plus a Z transaction (for example `ZGJMIGACC`).
* `CL_JVA_MIG_CORRECT_ACDOCA` is **FINAL, private instantiation, not released**, and it calls further
  SAP-internal, non-released classes (`CL_JVA_MIG_POST_ACDOCA`, `CL_JVA_MIG_POST_FI_OP`,
  `CL_JVA_MIG_ALV_OUTPUT`, `CL_JVA_SETTINGS`, `CL_JVA_UTIL`). These classes **change ACDOCA directly**.
* **The class PDF is incomplete.** It only prints the private methods. The interface method
  implementations are missing:
  * `IF_JVA_MIG_CORRECT_ACDOCA~EXECUTE_PROCESS` (main entry; calls `GET_GLOBAL_SETTINGS`, decides
    parallel or synchronous, `SAVE_RESULTS_GLOBAL`, `DISPLAY_RESULTS`)
  * `~PARALL_BEFORE_RFC`, `~PARALL_IN_RFC`, `~PARALL_AFTER_RFC`
  * the constant values (`GC_CORR_TYPE`, `GC_ISSUE_TYPE`, `GC_JVLINETYPE`, `GC_JVDOCTYPE`, `GC_JVPOSTTYPE`)
  * the private types and text symbols

  A full class copy needs these taken from SE24/SE80 in OCP (or a full abapGit export), not from the PDF.
  `GJMIGACC_c_lass_1.pdf` is a re-print of the same class and also lacks them. The SE24 print
  function does not output interface method implementations. To get them:
  * SE24 → `CL_JVA_MIG_CORRECT_ACDOCA` → **Source Code-Based** button → copy all, then save as `.txt`; or
  * SE38 → display include `CL_JVA_MIG_CORRECT_ACDOCA=====CU`/`CO`/`CI` (definitions) and the method
    includes `...CM0nn`. The class pool is `CL_JVA_MIG_CORRECT_ACDOCA=====CP`; or
  * export the class with abapGit.

  Also needed: interface `IF_JVA_MIG_CORRECT_ACDOCA` (parameter structure, package types), the class
  constants (`GC_*`) and the text pool.

### 6.2 Options

| Option | What to copy | When to use | Effort / risk |
|---|---|---|---|
| **A. Z report only (recommended)** | Copy the report to `ZRGJV_UPDATE_ACC_DATA_IN_ACDOCA` and a Z transaction. Keep calling the standard `CL_JVA_MIG_CORRECT_ACDOCA`. Change only the screen: defaults, hidden or forced options (for example, force test run for some users), extra authority check, variants. | The requirement is a different screen, defaults, restrictions or wrapper logic (pre/post steps, e-mail, extra log) | Low. SAP corrections to the class (SAP Notes) still apply. Copy the text symbols and selection texts too. |
| **B. Z report + Z class** | Copy the report and copy `CL_JVA_MIG_CORRECT_ACDOCA` to `ZCL_JVA_MIG_CORRECT_ACDOCA`. The Z class implements `IF_JVA_MIG_CORRECT_ACDOCA`, or a Z copy of the interface if the parameter structure must change. Keep calling the standard posting classes. | The comparison or decision logic must change: extra fields, different correction rules, ONGC-specific filters, an extra ledger rule | Medium. The class is about 8,000 lines and is no longer updated by SAP Notes. It needs regression tests against standard GJMIGACC output in test mode. |
| **C. Full copy including posting classes** | Also copy `CL_JVA_MIG_POST_ACDOCA` and the others | Only if the posting itself must change | **Not recommended.** This means custom code that writes ACDOCA directly, which affects audit, consistency and support. Raise an SAP incident instead. |

### 6.3 Steps for option A (and the base for option B)

1. SE38 → copy `RGJV_UPDATE_ACC_DATA_IN_ACDOCA` → `ZRGJV_UPDATE_ACC_DATA_IN_ACDOCA` (with text elements, documentation and variants).
2. SE93 → `ZGJMIGACC`, report transaction for the Z report, selection screen 1000.
3. Keep `repid_callback = sy-repid`. The class calls `BEFORE_RFC`/`IN_RFC`/`AFTER_RFC` back in the
   calling program by this name, so the three FORMs must stay in the Z report with the same names
   and signatures.
4. Apply the customer-specific changes (screen defaults, restrictions, extra checks).
5. Authorisation: the class already checks `JV_AUTHORITY_CHECK_PROCESS` (process CUTBACK2,
   activity 16 or 48). Add an `S_TCODE` / Z-object check if required.
6. Two small issues in the standard report that can be fixed in the copy:
   * `P_COBS` and `P_COCO` reuse `MEMORY ID vpt` (the same as `P_VPTNR`), so their values affect each other through SPA/GPA.
   * `SET CURSOR FIELD 'P_VNCORR'` points to a field that does not exist (it should be `P_VNUPD`).
   * Keep the system prerequisites in 4.0 in mind for the test system: document splitting with VNAME
     must be active, otherwise the Z report stops with G5 826/827 just like the standard one.
7. Test in this order: analysis → test run → update on a small company code and period, using the
   **same selections as standard GJMIGACC**. Compare the ALV results and `JVA_ACD_UPD_LOG`.

### 6.4 Open points for the customer

1. What exactly must the Z version do differently from standard? This decides between option A and option B.
2. If option B: provide the full class source (abapGit, or the SE24 source-code-based view) including
   the interface methods, constants and text pool. Neither class PDF contains them.
3. Confirm whether GJMIGINT (`RGJV_UPDATE_INT_DOCS_IN_ACDOCA`) has already been run in OCP.
   SAP recommends running it before GJMIGACC.
4. Confirm who may run update mode. Update mode posts or changes ACDOCA, so it should be restricted.
