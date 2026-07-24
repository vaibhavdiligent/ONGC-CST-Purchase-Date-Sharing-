# ZATC_RESULT_CORRECTION - Program Capabilities

## Purpose

`ZATC_RESULT_CORRECTION` is an automated ATC (ABAP Test Cockpit) result
remediation utility. It reads ATC findings from a selected run, opens each
target repository object, applies the standard corrective source-code change
that matches the ATC finding, activates the modified object, and reports a
syntax-check status per object in an ALV grid.

The program supports simulation mode (`p_sim = 'X'`) so all corrections can
be previewed on temporary copies (`ZTEST_CHECK<program>_<user>`) before real
objects are updated in a transport.

---

## Supported Object Types

The program can correct the following repository object types:

| Object type | Handling |
| --- | --- |
| **PROG** | Reports, executable programs, module pools, includes |
| **FUGR** | Function group main includes and function-module includes |
| **CLAS** | Global ABAP OO classes (public / protected / private sections, methods) |
| **INTF** | Global ABAP OO interfaces |
| **SSFO** | Smart Forms (form logic maintained via `SSF_READ_FORM` / `SSF_WRITE_FORM`) |
| **SFPF** | Adobe / SAP Interactive Forms (Form Builder) |
| **ENHO** | Enhancement Implementations (BAdIs, method / source-code plug-ins) |

For each fix the program writes a change header/footer block containing the
user, date, and ATC reference — matching the client's coding-guideline
"Begin of change / End of change" convention (parameters `p_begin` / `p_end`).

---

## ATC Findings that are Automatically Corrected

### 1. S/4HANA Simplification — Database / Table Migration

**Finding:** `S/4HANA: SEARCH FOR DATABASE OPERATIONS`

* `SELECT ... FROM <table>` where `<table>` is a table whose successor is a
  CDS view (per SAP table `ARS_API_SUCCESSOR`, including special-case
  `KONV → V_KONV_CDS`).
* Rewrites the SELECT to consume the successor CDS view.
* Rebuilds the field list (fetching base-field information from
  `CL_DD_DDL_HANDLER_FACTORY`) — including implicit `SELECT *` expansion,
  join field mapping, and `FOR ALL ENTRIES` targets.
* Adds the `@` host-variable prefix on `WHERE`, `INTO`, and `APPENDING`
  targets so the rewrite is standards-compliant with S/4HANA syntax.
* Preserves `UPDATE`, `INSERT`, and `DELETE` statements unchanged when they
  are hit by this check (the finding is informational for DML).
* Handles both `SELECT SINGLE *` (rewritten to `SELECT * ... UP TO 1 ROWS
  ORDER BY <primary key>`) and full multi-line SELECT blocks.
* Detects if the successor is a view (from `DD02L`) and emits
  `"#EC CI_NOORDER` instead of `ORDER BY PRIMARY KEY` because views do
  not accept that clause.

### 2. S/4HANA Simplification — Field Length Extensions (MATNR / GLACCOUNT)

**Finding:** `S/4HANA: FIELD LENGTH EXTENSIONS`

Auto-appends the standard "false positive" pseudo comment
`"#EC CI_FLDEXT_OK[<note>]` for confirmed-safe statements and leaves the
original line commented for traceability. Covers all messages:

* `CALL METHOD GENERIC PARAMETER`
* `OLD STRUCTURE-COMPONENT TYPE CONFLICT`
* `CALL FUNCTION GENERIC PARAMETER`
* `WRITE ISSUE` / `WRITE-LENGTH ISSUE`
* `SET PARAMETER ISSUE` / `GET PARAMETER ISSUE`
* `OLD SELECT TYPE CONFLICT`
* `MOVE GENERIC ->` / `MOVE -> GENERIC`
* `REPLACE ISSUE`
* `OFFSET/LENGTH-ACCESS` (respects `FLE_TOPIC_SWITCH-MATNR_TARGET_MODE`)
* `OLD MOVE LENGTH CONFLICT`
* `GENERIC SOURCE CODE ISSUE`
* `MESSAGE-WITH LENGTH CONFLICT`
* `STRUCTURE-COMPONENT LENGTH CONFLICT`
* `EXPORT ISSUE` / `IMPORT ISSUE` (multi-line IMPORT statements are
  detected via backward scan so the full statement is processed).

**Also handled with real transformation:**

* `MOVE TYPE CONFLICT`
* `MOVE LENGTH CONFLICT`
* `OLD ARITHMETIC TYPE CONFLICT`
* `OLD MOVE TYPE CONFLICT`

For these the assignment is split at `=`, the RHS is inspected for helper
conversion routines (e.g. `AMOUNT_CONV` for currency amount fields on
`DD03L`-typed CURR/DEC/QUAN columns), and the modified assignment is
emitted after the pseudo comment.

### 3. S/4HANA Simplification — Simplified Objects (Function Modules & Reports)

**Finding:** `S/4HANA: SEARCH FOR USAGES OF SIMPLIFIED OBJECTS`

* `SYNTACTICALLY INCOMPATIBLE CHANGE OF EXISTING FUNCTIONALITY`
* `NON-STRATEGIC-FUNCTION: FUNCTIONAL EQUIVALENT AVAILABLE`
* `FUNCTIONALITY NOT AVAILABLE: FUNCTIONAL EQUIVALENT AVAILABLE`

Uses the `ARS_API_SUCCESSOR` mapping to obtain the S/4HANA-strategic
successor for the deprecated FM/report and re-wires the `CALL FUNCTION`
signature (parameter list preserved). Special-case handling for
`FUNCTIONALITY NOT AVAILABLE: FUNCTIONAL EQUIVALENT AVAILABLE` on
transactions:

* `MIGO`-family transactions → wired to correct successor (via `PRGN_CORR2`)
* Business Partner replacements (`XD01/XD02/...` → BP) via
  `PERFORM replace_bp`
* Generic MIGO replacements via `PERFORM replace_migo`

### 4. S/4HANA Simplification — Simplified Transactions (as literals in code)

**Finding:** `S/4HANA: SEARCH FOR SIMPLIFIED TRANSACTIONS IN LITERALS`

For hard-coded transaction-code literals inside programs (e.g. `'MB01'`,
`'ME21'`), the program:

* Locates the literal (search is quote-anchored to avoid substring matches
  on identifiers such as `IT_MCHA`).
* Appends the pseudo comment `"#EC CI_USAGE_OK[<note>]`.
* Preserves any existing trailing comment.

### 5. Order By / Sort Corrections (Robust Loop Result)

**Finding:** `SEARCH PROBLEMATIC STATEMENTS FOR RESULT OF SELECT/OPEN CURSOR WITHOUT ORDER BY`

This check has **twelve** normalized sub-cases, each with a distinct fix:

| Sub-finding | Auto-fix applied |
| --- | --- |
| `DELETE ADJACENT DUPLICATES FOR RESULT OF STATEMENT AT ... LINE ...` | Inserts `SORT <itab> BY <compare fields>.` before the DELETE (or `SORT <itab>.` when `ALL FIELDS` is used) |
| `LOOP AT ITAB. EXIT/RETURN/LEAVE ... FOR RESULT OF STATEMENT AT ... LINE ...` | Inserts `SORT <itab>.` before the enclosing `LOOP AT` |
| `LOOP AT EMPTY ITAB. ...` | Same as above |
| `WRITE IN LOOP FOR RESULT OF STATEMENT AT ... LINE ...` | Same as above |
| `READ TABLE ... INDEX 1 FOR RESULT OF STATEMENT AT ... LINE ...` | Adds `SORT <itab> BY <key>.` before the READ TABLE |
| `LOOP AT ITAB FROM/TO ... FOR RESULT OF STATEMENT AT ... LINE ...` | Adds `SORT <itab>.` before the LOOP |
| `MODIFY ... INDEX FOR RESULT OF SELECT STATEMENT AT ... LINE ...` | Adds `SORT <itab> BY <key>.` before the MODIFY |
| `DELETE ... INDEX FOR RESULT OF SELECT STATEMENT AT ... LINE ...` | Adds `SORT <itab> BY <key>.` before the DELETE |
| `SELECT ... FOR (FORMER) CLUSTER TABLE ... WITHOUT ORDER BY FOUND` | Comments original SELECT, emits new SELECT with `ORDER BY PRIMARY KEY` (or `"#EC CI_NOORDER` if target is a view) |
| `SELECT ... FOR (FORMER) POOL TABLE ... WITHOUT ORDER BY FOUND` | Same as above |
| `SELECT .. UP TO .. ROWS WITHOUT ORDER BY FOUND` | Same as above |
| `SELECT SINGLE IS POSSIBLY NOT UNIQUE` | Rewrites `SELECT SINGLE *` to `SELECT * ... UP TO 1 ROWS ORDER BY <full primary key>` from `DD03L` |
| `READ TABLE ... BINARY SEARCH FOR RESULT OF STATEMENT AT ... LINE ...` | Inserts `SORT <itab> BY <key list>.` derived from the READ TABLE's WITH KEY clause |
| `LOOP AT ITAB. AT ... ENDAT. FOR RESULT OF STATEMENT AT ... LINE ...` | Adds `SORT <itab>.` before the loop |
| `LOOP AT ITAB. ON CHANGE OF ... ENDON. FOR RESULT OF STATEMENT AT ... LINE ...` | Adds `SORT <itab>.` before the loop |
| `EMPTY SELECT/ENDSELECT AT ... LINE ...` | Rewrites into an internal-table SELECT + LOOP construct |
| `ALV CALL AT ... LINE ...` | For `cl_salv_table=>factory( ... )`, emits a `SORT` on the ALV output table |

Duplicate protection: the program will not emit a `SORT` if one already
exists above the target statement for the same internal table.

### 6. IDoc Segment / DDIC Enhancement Findings

**Findings:**

* `S/4HANA: IDOC CHECK`
* `S/4HANA: SEARCH FOR ABAP DICTIONARY ENHANCEMENTS`

Auto-appends the `"#EC CI_USAGE_OK[<note>]` pseudo comment to the
flagged line so the finding is suppressed after review sign-off.

### 7. ADBC Interface Usage

**Finding:** `USE OF ADBC INTERFACE / ADBC CLASS ... USED`

Auto-appends the `"#EC CI_USAGE_OK[<note>]` pseudo comment.

---

## Post-Correction Quality Gate

After each object is patched, the program runs a **live syntax check**
using SAP's function module `RS_SYNTAX_CHECK` (the same FM used by the
ZCHECK_ABAP_SYNTAX utility). The ALV output includes:

* A traffic-light column (green = compiled cleanly, red = error found).
* Error line number.
* Full error message text.
* Include where the error occurs (relevant for CLAS/FUGR/ENHO objects).

Errors are automatically sorted to the top of the ALV.

The full SALV toolbar is enabled — including **Export → Spreadsheet** —
so results can be downloaded to Excel for QA sign-off.

---

## Operating Modes

| Parameter | Behaviour |
| --- | --- |
| `p_id` | ATC run series ID (F4-enabled) |
| `s_obj` | Object-name filter |
| `s_name` | Sub-object / include filter (mandatory) |
| `p_rem` | Free-text remark stamped on every change block |
| `lv_req` | Target transport request (mandatory) |
| `p_begin` / `p_end` | Change-block comment prefixes (client convention) |
| `p_sim = 'X'` | Simulation only: writes to a `ZTEST_CHECK<program>_<user>` copy, no changes to original |
| `p_sim = ' '` | Live mode: activates through `RPY_PROGRAM_UPDATE`, `CTS_WBO_API_INSERT_OBJECTS`, and per-object activation APIs |

Backups of the original source are always kept as
`ZTEST_CHECK<original_program>_<user>` before any live update, so the
change is fully reversible.

---

## Runtime Behaviour Notes

* The program refuses to run outside SAPGUI (Fiori Launchpad / WebGUI) to
  prevent accidental modifications from browser sessions.
* All updates are transport-recorded through the request supplied in
  `lv_req` (no direct-modifiable object writes).
* Every corrected block is bracketed by the exact "Begin of change / End
  of change" comments the client already uses in ONGC coding guidelines.
* Comment-only lines (`*` and `"`) are ignored by the change engine, so
  historical annotations are never treated as code.

---

## Deliverables per Run

For each object processed, the ALV grid shows:

1. Original program / class / form / include name.
2. Sub-object name.
3. New (activated) program name.
4. Backup program name.
5. Correction status: `Success` / `Syntax error`.
6. Traffic light indicator.
7. Syntax error line, message, and include (only for errors).
