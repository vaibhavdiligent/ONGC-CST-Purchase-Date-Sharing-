# Technical Specification Document
## Program: ZATC_RESULT_CORRECTION
### S/4HANA ATC Issue Auto-Correction Tool

---

| Field            | Details                                      |
|------------------|----------------------------------------------|
| **Document No.** | TSD-ZATC-001                                 |
| **Version**      | V1.1                                         |
| **Program Name** | ZATC_RESULT_CORRECTION                       |
| **Program Type** | Executable Program (Report)                  |
| **Package**      | Z-Custom Development                         |
| **Author**       | ONGC CST Development Team                    |
| **Date**         | April 2026                                   |
| **Status**       | Active                                       |

---

## 1. Overview

### 1.1 Purpose
`ZATC_RESULT_CORRECTION` is an automated ABAP correction tool designed to resolve ATC (ABAP Test Cockpit) findings generated during S/4HANA migration readiness analysis. It reads ATC results from a specific run series and automatically applies code fixes to ABAP programs, function groups, classes, and enhancement objects.

### 1.2 Background
During S/4HANA migration, SAP's ATC tool identifies hundreds or thousands of code issues that must be resolved before go-live. Manual correction of each finding is time-consuming and error-prone. This program automates the most common correction patterns, significantly reducing migration effort.

### 1.3 Scope
The program handles the following SAP ATC check categories:
- S/4HANA: Search for Database Operations
- S/4HANA: Field Length Extensions
- S/4HANA: Search for Usages of Simplified Objects
- S/4HANA: Search for Simplified Transactions in Literals
- Search for Problematic Statements (SELECT without ORDER BY)
- Use of ADBC Interface

---

## 2. Selection Screen Parameters

| Parameter | Type | Description | Mandatory |
|-----------|------|-------------|-----------|
| `P_ID` | `SATC_D_AC_TITLE` | ATC Run Series Name (F4 help available) | Yes |
| `S_OBJ` | `TADIR-OBJ_NAME` | Object Name filter (select-options) | No |
| `P_REM` | `CHAR50` | Remark/Identifier string to skip already-processed programs | No |
| `LV_REQ` | `TRKORR` | Transport Request Number | Yes |
| `P_BEGIN` | `CHAR50` | Begin-of-change comment text | No (default: `**begin of change by`) |
| `P_END` | `CHAR50` | End-of-change comment text | No (default: `* *End of change by`) |
| `P_SIM` | `FLAG` | Simulation mode checkbox | No (default: `X` = ON) |

### 2.1 Simulation Mode
When `P_SIM = 'X'` (checked), the program:
- Creates test copies of corrected programs with names `ZTEST_CHECKnnnnn`
- Does **not** modify the original programs
- Allows safe preview of changes before actual correction

When `P_SIM` is unchecked:
- Modifies original programs directly via `RPY_PROGRAM_UPDATE`
- Creates backup copies as `ZTEST_CHECKnnnnn`
- Assigns changes to the specified transport request

---

## 3. Program Flow

```
START
  |
  +--> Validate ATC Run Series (P_ID) --> Get Display ID
  |
  +--> Validate Transport Request (LV_REQ)
  |
  +--> Fetch ATC Findings via CL_SATC_API_FACTORY
  |
  +--> Build IT_FINAL (filtered findings with message details)
  |
  +--> Load correction config tables (ZATC_PROCESS_ALL, ZATC_PROCESS1)
  |
  +--> LOOP AT unique program/subobject combinations
  |      |
  |      +--> Read current source (SVRS_GET_VERSION_REPS_40 / SEO_METHOD_GET_SOURCE)
  |      |
  |      +--> LOOP AT source lines
  |             |
  |             +--> Match finding at line number
  |             |
  |             +--> Apply correction based on CHECK_TITLE + CHECK_MESSAGE
  |             |
  |             +--> Write corrected line to REPOS_TAB_NEW
  |
  +--> If source changed --> Update program (RPY_PROGRAM_UPDATE / INSERT REPORT)
  |
  +--> Syntax check corrected program
  |
  +--> Log result in IT_OUTPUT
  |
  +--> Display ALV output (CL_SALV_TABLE)
END
```

---

## 4. Supported Object Types

| Object Type | TADIR Key | Handling Method |
|-------------|-----------|-----------------|
| `PROG` | Program | `SVRS_GET_VERSION_REPS_40` + `RPY_PROGRAM_UPDATE` |
| `FUGR` | Function Group | `SVRS_GET_VERSION_REPS_40` + `RPY_PROGRAM_UPDATE` |
| `FUGS` | Function Group (include) | `SVRS_GET_VERSION_REPS_40` + `RPY_PROGRAM_UPDATE` |
| `CLAS` | ABAP Class | `SEO_METHOD_GET_SOURCE` / `SEO_SECTION_GET_SOURCE` + `INSERT REPORT` |
| `SSFO` | Smart Form | `SSF_READ_FORM` (partial - under development) |
| Enhancement (`ENHO`) | Hook Enhancement | `CL_ENH_FACTORY` + `CL_ENH_TOOL_HOOK_IMPL` |


---

## 5. ATC Check Categories and Correction Logic

### 5.1 S/4HANA: Search for Database Operations

**Messages Handled:**
- `DB OPERATION SELECT FOUND`
- `DB OPERATION JOIN FOUND`

**Correction Logic:**
1. Look up successor CDS view in table `ARS_API_SUCCSSR` for the deprecated database table (PARAM1).
2. Special case: `KONV` → replaced with `V_KONV_CDS`.
3. If the CDS view has **no base field mappings** (flat replacement):
   - Comment out original SELECT statement
   - Replace table name with CDS view name
   - Wrap with begin/end change comments
4. If the CDS view has **field mappings** (structural change):
   - Parse full SELECT statement into tokens
   - Remap field names to CDS element names using `CL_DD_DDL_FIELD_TRACKER->GET_BASE_FIELD_INFORMATION`
   - Reconstruct SELECT with aliased CDS fields (`element_name AS base_field`)
   - For JOINs: remap all joined tables and their field aliases
5. If no successor found and table has no CDS replacement:
   - Add pragma comment `"#EC CI_DB_OPERATION_OK[note_number]`

**Key Function Modules / Classes Used:**
| Object | Purpose |
|--------|---------|
| `ARS_API_SUCCSSR` | Table: maps deprecated tables to CDS successor views |
| `CL_DD_DDL_FIELD_TRACKER` | Gets base field mappings of a CDS view |
| `FORM change_table` | Parses and rewrites SELECT with CDS field mapping |

---

### 5.2 S/4HANA: Field Length Extensions

**Messages Handled:**

| Message | Correction Applied |
|---------|-------------------|
| `CALL METHOD GENERIC PARAMETER` | Add pragma `"#EC CI_FLDEXT_OK[note]` |
| `OLD STRUCTURE-COMPONENT TYPE CONFLICT` | Add pragma `"#EC CI_FLDEXT_OK[note]` |
| `CALL FUNCTION GENERIC PARAMETER` | Add pragma `"#EC CI_FLDEXT_OK[note]` |
| `WRITE ISSUE` | Add pragma `"#EC CI_FLDEXT_OK[note]` |
| `WRITE-LENGTH ISSUE` | Add pragma `"#EC CI_FLDEXT_OK[note]` |
| `SET PARAMETER ISSUE` | Add pragma `"#EC CI_FLDEXT_OK[note]` |
| `OLD SELECT TYPE CONFLICT` | Add pragma `"#EC CI_FLDEXT_OK[note]` |
| `MOVE GENERIC ->` | Add pragma `"#EC CI_FLDEXT_OK[note]` |
| `MOVE -> GENERIC` | Add pragma `"#EC CI_FLDEXT_OK[note]` |
| `OFFSET/LENGTH-ACCESS` | Add pragma (Priority 2/3 only, if FLE switch active) |
| `OLD MOVE LENGTH CONFLICT` | Add pragma (Priority 2/3 only, if FLE switch active) |
| `IMPORT ISSUE` | Add `ACCEPTING PADDING` to IMPORT statement |
| `MOVE TYPE CONFLICT` | Wrap RHS with `CONV #( ... )` operator |
| `MOVE LENGTH CONFLICT` | Wrap RHS with `CONV #( ... )` operator |
| `OLD ARITHMETIC TYPE CONFLICT` | Wrap RHS with `CONV #( ... )` operator |
| `OLD MOVE TYPE CONFLICT` | Wrap RHS with `CONV #( ... )` operator |

**CONV Operator Logic:**
- Parses `lhs = rhs.` statement
- Reconstructs as `lhs = CONV #( rhs ).`
- Wraps original line as comment, adds new corrected line

---

### 5.3 S/4HANA: Search for Usages of Simplified Objects

**Messages Handled:**

| Message | Condition | Note(s) | Correction |
|---------|-----------|---------|------------|
| `SYNTACTICALLY INCOMPATIBLE CHANGE` | Amount FLE - DEC fields | **2628704** | `PERFORM amount_conv` – wrap DEC field assignment with `CONV domname(...)` |
| `SYNTACTICALLY INCOMPATIBLE CHANGE` | Amount FLE - CURR fields | **2628699** | `PERFORM amount_conv` – extended to handle CURR datatype |
| `SYNTACTICALLY INCOMPATIBLE CHANGE` | Amount FLE - QUAN fields | **2628706** | `PERFORM amount_conv` – extended to handle QUAN datatype |
| `SYNTACTICALLY INCOMPATIBLE CHANGE` | Material FLE - MATNR40/18 | **2438131** | `PERFORM material_conv` – wrap MATNR40/18 field with `CONV domname(...)` |
| `SYNTACTICALLY INCOMPATIBLE CHANGE` | Material FLE - MATNR base | **2438110** | `PERFORM material_conv` – extended to handle base MATNR domain |
| `FUNCTIONALITY UNAVAILABLE` | Legal report → SAP DRC | **2480067** | `PERFORM drc_report_note` – insert migration banner comment to adopt DRC statutory report |
| `SYNTACTICALLY INCOMPATIBLE CHANGE` | DTEL ref in config table | Config-driven | Replace old data element TYPE reference with new one |
| `SYNTACTICALLY INCOMPATIBLE CHANGE` | TRAN, mapped to BP | Config-driven | `PERFORM replace_bp` – replace SET PARAM + CALL TRAN with CL_BUPA_DIALOG_JOEL |
| `SYNTACTICALLY INCOMPATIBLE CHANGE` | TRAN, mapped to MIGO | Config-driven | `PERFORM replace_migo` – replace with MIGO_DIALOG function module call |
| `FUNCTIONALITY NOT AVAILABLE` | LIKE reference | Config-driven | Convert LIKE to TYPE using DD03L rollname lookup |
| `NON-STRATEGIC-FUNCTION` | Priority 2 or 3 | Generic | Add pragma `"#EC CI_USAGE_OK[note]` |

#### Notes Classification — Simplified Objects

| Note Group | Note Numbers | Field/Area | Handled By |
|------------|-------------|-----------|------------|
| AFLE (Amount Field Length Extension) | 2628704, 2628699, 2628706 | DEC / CURR / QUAN amount fields | `FORM amount_conv` |
| MFLE (Material Field Length Extension) | 2438131, 2438110 | MATNR40 / MATNR18 / MATNR domains | `FORM material_conv` |
| DRC (Document & Reporting Compliance) | 2480067 | Legacy statutory/legal reports | `FORM drc_report_note` |
| FLE - Generic (other field extensions) | 2xxx range (140+ notes) | Various CURR/QUAN/DEC fields | CI_FLDEXT_OK pragma |
| Simplified Objects - Generic | Various 2xxx/3xxx | Simplified APIs, BAPIs | CI_USAGE_OK pragma |

---

### 5.4 S/4HANA: Search for Simplified Transactions in Literals

**Messages Handled:**
- `FUNCTIONALITY NOT AVAILABLE: NO FUNCTIONAL EQUIVALENT`
- `FUNCTIONALITY NOT AVAILABLE: EQUIVALENT FUNCTION ON ROADMAP`
- `FUNCTIONALITY UNAVAILABLE`

**Condition:** Priority 3 AND object type TRAN

**Correction:** Inserts pragma `"#EC CI_USAGE_OK[note_number]` next to the transaction literal

---

### 5.5 Search Problematic Statements (SELECT without ORDER BY)

| Message | Correction |
|---------|-----------|
| `SELECT ... CLUSTER/POOL TABLE ... WITHOUT ORDER BY` | Appends `ORDER BY PRIMARY KEY` to SELECT |
| `SELECT .. UP TO .. ROWS WITHOUT ORDER BY` | Appends `ORDER BY PRIMARY KEY` to SELECT (or `"#EC CI_NOORDER` if FOR ALL ENTRIES) |
| `SELECT SINGLE IS POSSIBLY NOT UNIQUE` | Converts `SELECT SINGLE` → `SELECT ... UP TO 1 ROWS ... ORDER BY PRIMARY KEY. ENDSELECT.` via `FORM change_single` |
| `DELETE ADJACENT DUPLICATES FOR RESULT` | Inserts `SORT <table> [BY <fields>].` before the DELETE statement |
| `LOOP AT ITAB. EXIT/RETURN/LEAVE` | Inserts `SORT <table>.` before LOOP via `FORM loop_exit` |
| `LOOP AT ITAB. AT ... ENDAT` | Inserts `SORT <table> BY <field>.` before LOOP via `FORM endat` |
| `LOOP AT ITAB. ON CHANGE OF` | Inserts `SORT <table> BY <fields>.` before LOOP via `FORM process_change_loop` |
| `READ .. BINARY SEARCH` | Inserts `SORT <table> BY <key_fields>.` before LOOP via `FORM process_read` |
| `LOOP AT ITAB FROM/TO` | Adds `"#EC CI_NOORDER` pragma |
| `MODIFY/DELETE/READ ... INDEX` | Adds `"#EC CI_NOORDER` pragma |
| `EMPTY SELECT/ENDSELECT` | Adds `"#EC CI_NOORDER` pragma |
| `ALV CALL AT ... LINE` | Inserts `SORT <table>.` before `CALL METHOD cl_salv_table=>factory` |
| `WRITE IN LOOP` | Inserts `SORT <table>.` before LOOP |

---

### 5.6 Use of ADBC Interface

| Message | Correction |
|---------|-----------|
| `ADBC CLASS ... USED` | Adds pragma `"#EC CI_ADBC_US` to the statement |


---

## 6. Internal Data Structures

### 6.1 Key Internal Tables

| Table | Type | Description |
|-------|------|-------------|
| `IT_FINAL` | `TY_FINAL[]` | Consolidated ATC findings with parsed message text and parameters |
| `IT_FINAL_P` | `TY_FINAL[]` | Unique program/subobject pairs to process |
| `REPOS_TAB` | `ABAPTXT255[]` | Original source code of current program |
| `REPOS_TAB_NEW` | `ABAPTXT255[]` | Corrected source code being built |
| `IT_QUERY` | `SWASTRTAB[]` | Tokenized SELECT statement lines for parsing |
| `IT_QUERY_NEW` | `SWASTRTAB[]` | Reconstructed SELECT statement after correction |
| `IT_ZATC_PROCESS_ALL` | `TY_ZATC_PROCESS_ALL[]` | Correction config from custom table ZATC_PROCESS_ALL |
| `IT_ZATC_PROCESS1` | `TY_ZATC_PROCESS1[]` | Correction config from custom table ZATC_PROCESS1 |
| `IT_OUTPUT` | `TY_OUTPUT[]` | Processing result log for ALV display |

### 6.2 TY_FINAL Structure

| Field | Type | Description |
|-------|------|-------------|
| `PRIORITY` | CHAR3 | 1=Error, 2=Warning, 3=Note |
| `CHECK_TITLE` | STRING | ATC check category name |
| `CHECK_MESSAGE` | STRING | Specific message within the check |
| `OBJTYPE` | CHAR4 | Object type (PROG/FUGR/CLAS etc.) |
| `OBJNAME` | CHAR40 | Main object name |
| `LINE` | N(6) | Source line number of finding |
| `PROGRAM_NAME` | CHAR40 | Program/include name |
| `SOBJNAME` | CHAR40 | Sub-object name (include, class section) |
| `PARAM1-4` | CHAR50 | ATC finding parameters (table name, note number etc.) |
| `MESSAGE` | STRING | Resolved message text with parameters substituted |
| `MESSAGE1` | STRING | Message template text (parameters replaced with blanks) |
| `NOTE` | CHAR20 | SAP Note number extracted from parameters |
| `ENHNAME` | ENHNAME | Enhancement name (if object is an enhancement) |

### 6.3 TY_OUTPUT Structure

| Field | Type | Description |
|-------|------|-------------|
| `PROGRAM_NAME` | CHAR40 | Main program/object name |
| `SUBOBJ` | CHAR40 | Sub-object processed (include, method include) |
| `NEW_PROGRAM` | CHAR40 | Name of corrected program / test copy name |
| `BACKUP` | CHAR40 | Backup program name (`ZTEST_CHECKnnnnn`) |
| `STATUS` | CHAR10 | `Success` or `Syyntax error` |

---

## 7. Custom Configuration Tables

### 7.1 ZATC_PROCESS_ALL

Stores correction configuration for simplified object usages.

| Key Field | Description |
|-----------|-------------|
| `SRCH_TEM` | Search term (e.g. deprecated object name) |
| `REF_OBJ_TYPE` | Reference object type (DTEL, TRAN, FUNC, TABL) |
| `SOLUTION` | `X` = solution available |
| `FIX_BY` | Method of fix (e.g. `FIT GAP`, program change) |
| `CORRECTION_VALUE` | Replacement value (new data element, CDS view etc.) |

### 7.2 ZATC_PROCESS1

Stores additional correction mappings at message level.

| Key Field | Description |
|-----------|-------------|
| `CHECK_TITLE` | ATC check category |
| `SRCH_TEM` | Search parameter value |
| `CORRECTION_VALUE` | Replacement CDS view or object name |

---

## 8. Key Function Modules and APIs Used

| FM / Class / Method | Purpose |
|---------------------|---------|
| `CL_SATC_API_FACTORY->CREATE_RESULT_ACCESS` | Initialize ATC result reader |
| `result_access->GET_FINDINGS` | Fetch all ATC findings for the run series |
| `SVRS_GET_VERSION_REPS_40` | Read active source of PROG/FUGR include |
| `SEO_CLASS_GET_METHOD_INCLUDES` | Get class method include names |
| `SEO_METHOD_GET_SOURCE` | Read source of a specific class method |
| `SEO_SECTION_GET_SOURCE` | Read class section source (public/protected/private) |
| `RPY_PROGRAM_UPDATE` | Update ABAP program source with transport assignment |
| `INSERT REPORT` | Insert source into program repository |
| `CTS_WBO_API_INSERT_OBJECTS` | Add object to transport request |
| `CL_DD_DDL_FIELD_TRACKER->GET_BASE_FIELD_INFORMATION` | Get CDS view field-to-base-table mapping |
| `RS_ABAP_SYNTAX_CHECK_E` | Perform syntax check after correction |
| `CL_ENH_FACTORY=>GET_ENHANCEMENT` | Get enhancement object for modification |
| `CL_ENH_TOOL_HOOK_IMPL->MODIFY_HOOK_IMPL` | Update enhancement source code |
| `CL_ENH_TOOL_HOOK_IMPL->IF_ENH_OBJECT~SAVE` | Save enhancement changes |
| `CL_ENH_TOOL_HOOK_IMPL->IF_ENH_OBJECT~ACTIVATE` | Activate enhancement |
| `SSF_READ_FORM` | Read Smart Form definition |
| `PRGN_CORR2` | Table: old → new transaction code mapping |
| `ARS_API_SUCCSSR` | Table: deprecated table → CDS successor mapping |
| `FLE_TOPIC_SWITCH` | Table: field length extension activation switch |
| `FUPARAREF` | Table: function module parameter-to-structure mapping |
| `DD03L` | DDIC table: field definitions (rollname, datatype, domain) |
| `CL_BUPA_DIALOG_JOEL` | New BP transaction API (replaces old vendor/customer transactions) |


---

## 9. Sub-Routine (FORM) Descriptions

| Form Name | Description |
|-----------|-------------|
| `CHANGE_TABLE` | Parses a SELECT or SELECT with JOIN statement, remaps deprecated table/field references to CDS view equivalents, and reconstructs the statement. Handles both simple SELECTs and multi-table JOINs. |
| `CHANGE_SINGLE` | Converts a `SELECT SINGLE ... INTO` statement into `SELECT ... UP TO 1 ROWS INTO ... ORDER BY PRIMARY KEY. ENDSELECT.` |
| `PROCESS_READ` | For `READ TABLE ... BINARY SEARCH` findings, finds the enclosing LOOP statement, extracts key fields, and inserts a `SORT <table> BY <fields>.` statement before the loop. |
| `PROCESS_CHANGE_LOOP` | For `ON CHANGE OF` findings, extracts the change field, finds the enclosing LOOP, and inserts appropriate SORT statement. |
| `ENDAT` | For `AT ... ENDAT` findings, extracts the AT field name, finds enclosing LOOP, and inserts `SORT <table> BY <field>.` |
| `LOOP_EXIT` | For `LOOP AT ... EXIT` findings, finds the enclosing LOOP and inserts `SORT <table>.` before it (deduplication guard included). |
| `AMOUNT_CONV` | Converts amount/currency/quantity field assignments to use `CONV domname(...)` operator. Handles **DEC** (note 2628704), **CURR** (note 2628699), and **QUAN** (note 2628706) datatypes by querying DD03L for the function module parameter structure fields. |
| `MATERIAL_CONV` | Converts material number field assignments to use `CONV domname(...)` operator. Handles **MATNR40/MATNR18** domains (note 2438131) and base **MATNR** domain (note 2438110). |
| `DRC_REPORT_NOTE` | Handles SAP note **2480067** — legacy ABAP legal/statutory reports replaced by SAP Document and Reporting Compliance (DRC) framework. Inserts begin/end change comments and a developer migration banner: `*** NOTE 2480067: Replace this usage with SAP DRC statutory report. ***` |
| `REPLACE_BP` | Replaces old vendor/customer transaction calls (via SET PARAMETER + CALL TRANSACTION) with the new `CL_BUPA_DIALOG_JOEL` API. |
| `REPLACE_MIGO` | Replaces direct MIGO transaction calls with `CALL FUNCTION 'MIGO_DIALOG'` including document number and year parameters. |
| `SMARTFORM_PROCEE` | Stub handler for Smart Form corrections (partially implemented, contains BREAK-POINT for analysis). |
| `SPLIT_STRING` | Splits a long ABAP statement string into multiple lines of max 72 characters, breaking at suitable separator characters. |
| `SYNTAX_CHECK` | Performs ABAP syntax check on the corrected program using `RS_ABAP_SYNTAX_CHECK_E`. Handles PROG, FUGR, CLAS, ENHO, and SFPF object types with appropriate program name derivation. |
| `ZATC_PROCESS_ALL` | Reads all records from custom config table `ZATC_PROCESS_ALL` into internal table. |
| `ZATC_PROCESS1` | Reads all records from custom config table `ZATC_PROCESS1` into internal table. |

---

## 10. Change Comment Format

The program wraps every correction with standardized change comments for traceability:

**Begin comment:**
```abap
"" <P_REM> **begin of change by <SY-UNAME> <DD.MM.YYYY> for ATC
```

**Original line (commented out):**
```abap
* <original source line>
```

**Corrected line:**
```abap
<corrected statement>
```

**End comment:**
```abap
" <P_REM> * *End of change by <SY-UNAME> <DD.MM.YYYY> for ATC
```

**Example output:**
```abap
"" ZZ **begin of change by BASIS01 13.04.2026 for ATC
* SELECT * FROM BSEG INTO TABLE @lt_bseg WHERE bukrs = @lv_bukrs.
SELECT belnr AS belnr, gjahr AS gjahr FROM I_JournalEntry
  INTO TABLE @lt_bseg WHERE bukrs = @lv_bukrs.
" ZZ * *End of change by BASIS01 13.04.2026 for ATC
```

---

## 11. Output ALV Report

After processing, the program displays an ALV list with the following columns:

| Column | Description |
|--------|-------------|
| Main Program Name | Original program/class/function group name |
| Sub Object Name | Include or method include that was modified |
| New Program Name | Corrected program name (or test copy in simulation mode) |
| Back Up Program Name | Backup copy name (`ZTEST_CHECKnnnnn`) |
| Status | `Success` = syntax check passed, `Syyntax error` = syntax errors found |

---

## 12. Prerequisites and Dependencies

### 12.1 Required Custom Tables
| Table | Description |
|-------|-------------|
| `ZATC_PROCESS_ALL` | Correction configuration for simplified objects |
| `ZATC_PROCESS1` | Additional correction mappings |

### 12.2 Required Authorizations
| Authorization Object | Field | Value |
|---------------------|-------|-------|
| `S_DEVELOP` | `DEVCLASS` | Target development class |
| `S_DEVELOP` | `OBJTYPE` | PROG, CLAS, FUGR, ENHO |
| `S_DEVELOP` | `ACTVT` | 02 (Change) |
| `S_TRANSPRT` | `TTYPE` | Workbench request |
| `S_CTS_ADMI` | `CTS_ADMFCT` | TABL |
| `S_SATC` | - | ATC result read access |

### 12.3 Execution Restrictions
- Must be executed **within SAP GUI** (not via WebGUI/Fiori). The program checks `WEBGUI_GET_FLP_URL` at initialization and raises error if executed outside SAP GUI.
- Requires an active, unlocked transport request.
- ATC Run Series must exist in `SATC_AC_RESULTH`.

---

## 13. Limitations and Known Issues

| # | Limitation |
|---|-----------|
| 1 | Smart Form correction (`SSFO` type) is not fully implemented — currently contains BREAK-POINT for analysis only |
| 2 | The program does not handle all possible SELECT syntax variations (e.g. nested SELECTs, dynamic WHERE clauses) |
| 3 | For JOIN statements with more than 3 tables, field remapping may not be complete |
| 4 | The status field shows `Syyntax error` (note typo — intentional in original code) |
| 5 | `FORM amount_conv` handles DEC/CURR/QUAN fields in function module parameter structures only; direct inline assignments in PROG/CLAS are handled by generic CI_FLDEXT_OK pragma. `FORM material_conv` handles MATNR40, MATNR18, and MATNR domain fields in function module parameters. |
| 6 | Objects where `sobjname` does not start with Y or Z are skipped (custom namespace filter) |

---

## 14. Testing Procedure

### 14.1 Simulation Mode Testing
1. Run with `P_SIM = X` (checked)
2. Verify test copies `ZTEST_CHECKnnnnn` are created
3. Review test copies for correctness of applied changes
4. Check ALV output — all statuses should be `Success`

### 14.2 Production Mode Execution
1. Ensure valid transport request exists and is open
2. Uncheck `P_SIM`
3. Run for a subset of objects first using `S_OBJ` filter
4. Verify modified programs in SE38/SE80
5. Validate syntax and activate objects
6. Release transport and test in target system

### 14.3 Re-run Protection
Use `P_REM` parameter with a unique identifier string. If this string is found in the source code of a program, the program skips it (prevents double-correction).

---

## 15. Change History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| V1.0 | April 2026 | ONGC CST Team | Initial version — covers major S/4HANA ATC correction scenarios |
| V1.1 | April 2026 | ONGC CST Team | Added handling for 4 missing SAP notes: 2628699 (AFLE-CURR), 2628706 (AFLE-QUAN), 2438110 (MFLE-MATNR base domain), 2480067 (DRC legal report replacement). Enhanced `amount_conv` for CURR/QUAN datatypes. Enhanced `material_conv` for MATNR domain. Added new `drc_report_note` FORM for DRC migration. |

---

*End of Technical Specification Document*
