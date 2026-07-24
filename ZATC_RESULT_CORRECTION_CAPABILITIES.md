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

---

## Appendix A — Classic Tables Auto-Converted to S/4HANA Successors

The program uses SAP's standard `ARS_API_SUCCESSOR` mapping to determine
the S/4HANA successor (released CDS view or API) for each classic
database table encountered by the ATC finding
`S/4HANA: SEARCH FOR DATABASE OPERATIONS` (messages `DB OPERATION SELECT
FOUND` / `DB OPERATION JOIN FOUND`).

Coverage below reflects the current `ARS_API_SUCCESSOR.xlsx` reference
snapshot shipped with the tool:

* **272 distinct SAP classic tables** are mapped to a successor.
* **312 total mappings** (some tables have multiple valid CDS successors
  — the program selects the correct one from the finding context and,
  where applicable, the base-field information returned by
  `CL_DD_DDL_HANDLER_FACTORY`).
* Special-case override maintained in the program's code:
  `KONV` → `V_KONV_CDS` (in addition to the generic
  `KONV → I_SLSPRCGCONDITIONRECORD` from ARS_API_SUCCESSOR).

### Full Predecessor → Successor List

| SAP Classic Table | S/4HANA Successor (CDS View / API) |
|---|---|
| `ACDOCA` | `I_GLACCOUNTLINEITEM`, `I_GLACCOUNTLINEITEMRAWDATA`, `I_JOURNALENTRYITEM` |
| `AFKO` | `I_MANUFACTURINGORDER` |
| `AFPO` | `I_MANUFACTURINGORDERITEM` |
| `AFRU` | `I_MFGORDERCONFIRMATION` |
| `AFVC` | `I_MANUFACTURINGORDEROPERATION` |
| `AFVU` | `I_MANUFACTURINGORDEROPERATION` |
| `AFVV` | `I_MANUFACTURINGORDEROPERATION` |
| `AUFK` | `I_ORDER` |
| `BKPF` | `I_JOURNALENTRY` |
| `BNKA` | `I_BANK_2` |
| `BSEC` | `I_ONETIMEACCOUNTCUSTOMER` |
| `BSED` | `I_BILLOFEXCHANGE` |
| `BSEG` | `I_OPERATIONALACCTGDOCITEM` |
| `BSEG_ADD` | `I_ADDLLEDGEROPLACCTGDOCITEM` |
| `BUT000` | `I_BUSINESSPARTNER` |
| `CDHDR` | `CL_CHDO_READ_TOOLS`, `CL_CHDO_WRITE_TOOLS` |
| `CDPOS` | `CL_CHDO_READ_TOOLS`, `CL_CHDO_WRITE_TOOLS` |
| `CEPC` | `I_PROFITCENTER` |
| `CEPCT` | `I_PROFITCENTERTEXT` |
| `CRHD` | `I_WORKCENTER` |
| `CRTX` | `I_WORKCENTERTEXT` |
| `CSKA` | `I_GLACCOUNT` |
| `CSKB` | `I_GLACCOUNTINCOMPANYCODE` |
| `CSKS` | `I_COSTCENTER` |
| `CSKT` | `I_COSTCENTERTEXT` |
| `CSKU` | `I_GLACCOUNTTEXT` |
| `CSLA` | `I_COSTCENTERACTIVITYTYPE` |
| `CSLT` | `I_COSTCENTERACTIVITYTYPETEXT` |
| `DD07L` | `DDCDS_CUSTOMER_DOMAIN_VALUE` |
| `DD07T` | `DDCDS_CUSTOMER_DOMAIN_VALUE_T` |
| `DFKKKO` | `I_CADOCUMENTHEADER` |
| `DFKKOP` | `I_CADOCUMENTBPITEMPHYSICAL` |
| `DFKKOPK` | `I_CADOCUMENTGLITEM` |
| `EBAN` | `I_PURCHASEREQUISITIONITEMAPI01` |
| `EBKN` | `I_PURREQNACCTASSGMTAPI01` |
| `EHEWAD_WA_TRRQLA` | `I_WASTETRANSFERREQUESTHISTORY` |
| `EINA` | `I_PURCHASINGINFORECORDAPI01` |
| `EINE` | `I_PURCHASINGINFORECORDAPI01` |
| `EKBE` | `I_PURCHASEORDERHISTORYAPI01` |
| `EKES` | `I_POSUPPLIERCONFIRMATIONAPI01` |
| `EKET` | `I_PURORDSCHEDULELINEAPI01` |
| `EKKN` | `I_PURORDACCOUNTASSIGNMENTAPI01` |
| `EKKO` | `I_PURCHASEORDERAPI01` |
| `EKPA` | `I_PURCHASEORDERPARTNERAPI01` |
| `EKPO` | `I_PURCHASEORDERITEMAPI01` |
| `EORD` | `I_MPPURCHASINGSOURCEITEM` |
| `EQUK` | `I_PURGQUOTAARRGMTAPI01` |
| `EQUP` | `I_PURGQUOTAARRGMTITEMAPI01` |
| `FKKINV_UNIT_PUBLIC_DATA_BADI` | `FKKINV_UNIT_PUBLIC_DATA_GFN` |
| `FKKVK` | `I_CONTRACTACCOUNTHEADER` |
| `FKKVKP` | `I_CONTRACTACCOUNTPARTNER` |
| `GLO_BUP_CTX_S_ACCDOC_HDR_FI` | `FINS_ACDOC_HEADER_IN` |
| `GLO_BUP_CTX_S_ACCDOC_ITM_FI` | `FINS_ACDOC_ITEM_IN` |
| `GLO_BUP_CTX_S_ACCHD_FI` | `FINS_ACDOC_HEADER_IN` |
| `GLO_BUP_CTX_S_ACCINTF_FI` | `FINS_ACDOC_HEADER_IN`, `FINS_ACDOC_ITEM_IN` |
| `GLO_BUP_CTX_S_ACCIT_FI` | `FINS_ACDOC_ITEM_IN` |
| `GLO_BUP_CTX_S_FIORI_FI` | `FINS_ACDOC_HEADER_IN`, `FINS_ACDOC_ITEM_IN` |
| `GLO_BUP_CTX_S_FIORI_ITEM_FI` | `FINS_ACDOC_ITEM_IN` |
| `GLO_BUP_CTX_S_HEADER_FI` | `FINS_ACDOC_HEADER_IN` |
| `GLO_BUP_CTX_S_ITEM_FI` | `FINS_ACDOC_ITEM_IN` |
| `GLO_BUP_CTX_S_TRANS_FI` | `FINS_ACDOC_HEADER_IN`, `FINS_ACDOC_ITEM_IN` |
| `KBED` | `I_CAPACITYREQUIREMENTITEM` |
| `KBLK` | `I_EARMARKEDFUNDSDOCUMENT` |
| `KBLP` | `I_EARMARKEDFUNDSDOCUMENTITEM` |
| `KNA1` | `I_CUSTOMER` |
| `KNB1` | `I_CUSTOMERCOMPANY` |
| `KNB5` | `I_CUSTOMERDUNNING` |
| `KNBK` | `I_BUSINESSPARTNERBANK` |
| `KNVI` | `I_CUSTSALESAREATAX` |
| `KNVK` | `I_CONTACTPERSON` |
| `KNVP` | `I_CUSTSALESPARTNERFUNC` |
| `KNVV` | `I_CUSTOMERSALESAREA` |
| `KONH` | `I_SLSPRCGCONDITIONRECORD` |
| `KONM` | `I_SLSPRCGCNDNRECORDSCALE` |
| `KONP` | `I_SLSPRCGCONDITIONRECORD` |
| `KONV` | `I_SLSPRCGCONDITIONRECORD` |
| `LFA1` | `I_SUPPLIER` |
| `LFAS` | `I_BUSINESSPARTNERTAXNUMBER` |
| `LFB1` | `I_SUPPLIERCOMPANY` |
| `LFBK` | `I_BUSINESSPARTNERBANK` |
| `LFM1` | `I_SUPPLIERPURCHASINGORG` |
| `LIKP` | `I_DELIVERYDOCUMENT` |
| `LIPS` | `I_DELIVERYDOCUMENTITEM` |
| `MARA` | `I_PRODUCT`, `I_PRODUCTPROCUREMENT`, `I_PRODUCTQM`, `I_PRODUCTSALES`, `I_PRODUCTSTORAGE_2` |
| `MARC` | `I_PRODUCTPLANTBASIC`, `I_PRODUCTPLANTCOSTING`, `I_PRODUCTPLANTFORECAST`, `I_PRODUCTPLANTINTLTRD`, `I_PRODUCTPLANTPROCUREMENT`, `I_PRODUCTPLANTQTMANAGEMENT`, `I_PRODUCTPLANTSALES`, `I_PRODUCTSUPPLYPLANNING`, `I_PRODUCTWORKSCHEDULING` |
| `MARD` | `I_PRODUCTSTORAGELOCATIONBASIC` |
| `MARM` | `I_PRODUCTUNITSOFMEASURE` |
| `MATDOC` | `I_MATERIALDOCUMENTHEADER_2` |
| `MCH1` | `I_BATCH` |
| `MCHA` | `I_BATCH` |
| `MKAL` | `I_PRODUCTIONVERSION` |
| `MKPF` | `I_MATERIALDOCUMENTHEADER_2` |
| `MSEG` | `I_MATERIALDOCUMENTITEM_2` |
| `MSKA` | `I_MATERIALSTOCK` |
| `MSKU` | `I_MATERIALSTOCK` |
| `MSLB` | `I_MATERIALSTOCK` |
| `MSLBH` | `I_MATERIALSTOCK` |
| `MSPR` | `I_MATERIALSTOCK` |
| `PACKKP` | `I_PACKINGINSTRUCTIONHEADER` |
| `PACKPO` | `I_PACKINGINSTRUCTIONCOMPONENT` |
| `PKHD` | `I_KANBANCONTROLCYCLE` |
| `PLFH` | `I_MFGBOOOPPRODNRSCETOOLCHGST` |
| `PLKO` | `I_BILLOFOPERATIONSGROUP` |
| `PLPO` | `I_MFGBILLOFOPERATIONSOPERATION` |
| `PRCD_ELEMENTS` | `I_SLSPRCGCONDITIONRECORD` |
| `QALS` | `I_INSPECTIONLOT` |
| `QPCT` | `I_CHARCATTRIBUTECODETEXT`, `I_DEFECTCODETEXT`, `I_USAGEDECISIONCODETEXT` |
| `QPMK` | `I_INSPSPECIFICATIONVERSION` |
| `RBCO` | `I_SUPLRINVOICEITEMGLACCTAPI01` |
| `RBKP` | `I_SUPPLIERINVOICEAPI01` |
| `RBSELBEST` | `I_SUPLRINVCSELDPURGDOCAPI01` |
| `RESB` | `I_RESERVATIONDOCUMENTITEM` |
| `RKWA` | `I_SUPCSGNMTPPLINEWTHDRWLAPI01` |
| `RSEG` | `I_SUPLRINVCITEMPURORDREFAPI01` |
| `SETHEADER` | `I_COSTCENTERHIERARCHY`, `I_COSTCTRACTIVITYTYPEHIERARCHY`, `I_FUNCTIONALAREAHIERARCHY`, `I_PROFITCENTERHIERARCHY`, `I_STSTCLKEYFIGUREHIERARCHY` |
| `SETHEADERT` | `I_COSTCENTERHIERARCHY`, `I_COSTCTRACTIVITYTYPEHIERARCHY`, `I_FUNCTIONALAREAHIERARCHY`, `I_PROFITCENTERHIERARCHY`, `I_STSTCLKEYFIGUREHIERARCHY` |
| `SETLEAF` | `I_COSTCENTERHIERARCHYNODE`, `I_COSTCTRACTIVITYTYPEHIERNODE`, `I_FUNCTIONALAREAHIERNODE`, `I_PROFITCENTERHIERARCHYNODE`, `I_STSTCLKEYFIGUREHIERNODE` |
| `SETNODE` | `I_COSTCENTERHIERARCHYNODE`, `I_COSTCTRACTIVITYTYPEHIERNODE`, `I_FUNCTIONALAREAHIERNODE`, `I_PROFITCENTERHIERARCHYNODE`, `I_STSTCLKEYFIGUREHIERNODE` |
| `SKA1` | `I_GLACCOUNTINCHARTOFACCOUNTS` |
| `SKAT` | `I_GLACCOUNTTEXT` |
| `SKB1` | `I_GLACCOUNTINCOMPANYCODE` |
| `T001` | `I_COMPANYCODE` |
| `T002` | `I_LANGUAGE` |
| `T002T` | `I_LANGUAGETEXT` |
| `T003` | `I_ACCOUNTINGDOCUMENTTYPE` |
| `T004` | `I_CHARTOFACCOUNTS` |
| `T005` | `I_COUNTRY` |
| `T005T` | `I_COUNTRYTEXT` |
| `T006` | `I_UNITOFMEASURE` |
| `T006A` | `I_UNITOFMEASURETEXT` |
| `T006B` | `I_UNITOFMEASURECOMMERCIALNAME` |
| `T006C` | `I_UNITOFMEASURETECHNICALNAME` |
| `T006D` | `I_UNITOFMEASUREDIMENSION` |
| `T006I` | `I_UNITOFMEASUREISOCODE` |
| `T006J` | `I_UNITOFMEASUREISOCODETEXT` |
| `T006T` | `I_UNITOFMEASUREDIMENSIONTEXT` |
| `T008` | `I_PAYMENTBLOCKINGREASON` |
| `T008T` | `I_PAYMENTBLOCKINGREASON` |
| `T011` | `I_FINANCIALSTATEMENTHIER` |
| `T011T` | `I_FINANCIALSTATEMENTHIERT` |
| `T014` | `I_CREDITCONTROLAREA` |
| `T014T` | `I_CREDITCONTROLAREATEXT` |
| `T024` | `I_PURCHASINGGROUP` |
| `T024D` | `I_MRPCONTROLLER` |
| `T024E` | `I_PURCHASINGORGANIZATION` |
| `T027A` | `I_SHIPPINGINSTRUCTION` |
| `T027B` | `I_SHIPPINGINSTRUCTIONTEXT` |
| `T040S` | `I_DUNNINGBLOCKINGREASONCODE` |
| `T048` | `I_CORRESPONDENCETYPE` |
| `T074T` | `I_SPECIALGLCODETEXT` |
| `T074U` | `I_SPECIALGLCODE` |
| `T077X` | `I_CUSTOMERACCOUNTGROUPTEXT` |
| `T151` | `I_CUSTOMERGROUP` |
| `T151T` | `I_CUSTOMERGROUP` |
| `T156` | `I_GOODSMOVEMENTTYPE` |
| `T161` | `I_PURCHASINGDOCUMENTTYPE` |
| `T161T` | `I_PURCHASINGDOCUMENTTYPETEXT` |
| `T163D` | `I_SUPLRCONFCATINTERNALASSGMT` |
| `T163I` | `I_ACCTASSIGNMENTCATEGORYTEXT` |
| `T163K` | `I_ACCOUNTASSIGNMENTCATEGORY` |
| `T166C` | `I_PURGDOCOUTPUTRELEVANTCHANGE` |
| `T166K` | `I_PURGOBJTEXTOBJECTOUTPRLVNCE` |
| `T166P` | `I_PURGOBJITEMTXTOBJOUTPRLVNCE` |
| `T166T` | `I_PURGDOCOUTPUTCHANGETEXT` |
| `T171T` | `I_SALESDISTRICT` |
| `T178` | `I_MATERIALPRICINGGROUP` |
| `T188` | `I_CUSTOMERPRICEGROUP` |
| `T189` | `I_PRICELISTTYPE` |
| `T459A` | `I_PLNDINDEPRQMTTYPE` |
| `T683` | `I_SLSPRICINGPROCEDURE` |
| `T683S` | `I_SLSPRICINGPROCEDUREITEM` |
| `T683T` | `I_SLSPRICINGPROCEDUREITEM` |
| `T685` | `I_CONDITIONTYPE` |
| `T685A` | `I_PRICINGCONDITIONTYPE` |
| `T685T` | `I_CONDITIONTYPE` |
| `TADIR` | `I_CUSTABAPOBJDIRECTORYENTRY` |
| `TBSL` | `I_POSTINGKEY` |
| `TCURC` | `I_CURRENCY` |
| `TCURF` | `I_EXCHANGERATEFACTORSRAWDATA` |
| `TCURN` | `I_EXCHRATEQTANTYPERAWDATA` |
| `TCURR` | `I_EXCHANGERATERAWDATA` |
| `TCURT` | `I_CURRENCYTEXT` |
| `TCURV` | `I_EXCHANGERATETYPE` |
| `TCURX` | `I_CURRENCY` |
| `TDEVC` | `I_CUSTABAPPACKAGE` |
| `TINC` | `I_INCOTERMSCLASSIFICATION` |
| `TINCT` | `I_INCOTERMSCLASSIFICATION` |
| `TKA01` | `I_CONTROLLINGAREA` |
| `TKA02` | `I_COMPANYCODE` |
| `TPAR` | `I_INCOTERMSCLASSIFICATION`, `I_PARTNERFUNCTION` |
| `TPART` | `I_PARTNERFUNCTIONTEXT` |
| `TPRIO` | `I_DELIVERYPRIORITY` |
| `TSKD` | `I_CUSTOMERTAXCLASSIFICATION` |
| `TSKDT` | `I_CUSTOMERTAXCLASSIFICATION` |
| `TSPA` | `I_DIVISION` |
| `TSPAT` | `I_DIVISION` |
| `TSTL` | `I_PRODUCTSALESTAX` |
| `TTYPT` | `I_REFERENCEDOCUMENTTYPETEXT` |
| `TVAG` | `I_SALESDOCUMENTRJCNREASON` |
| `TVAGT` | `I_SALESDOCUMENTRJCNREASON` |
| `TVAK` | `I_SALESDOCUMENTTYPE` |
| `TVAKT` | `I_SALESDOCUMENTTYPE` |
| `TVAP` | `I_SALESDOCUMENTITEMCATEGORY` |
| `TVAPT` | `I_SALESDOCUMENTITEMCATEGORY` |
| `TVAU` | `I_SDDOCUMENTREASON` |
| `TVAUT` | `I_SDDOCUMENTREASONTEXT` |
| `TVBUR` | `I_SALESOFFICE` |
| `TVBVK` | `I_SALESOFFICESALESGROUP` |
| `TVCIN` | `I_SDDOCUMENTPAYMENTCARDTYPE` |
| `TVCINT` | `I_SDDOCUMENTPAYMENTCARDTYPE` |
| `TVEPT` | `I_SCHEDULELINECATEGORY` |
| `TVFK` | `I_BILLINGDOCUMENTTYPE` |
| `TVFKT` | `I_BILLINGDOCUMENTTYPE` |
| `TVFS` | `I_BILLINGBLOCKREASON` |
| `TVFST` | `I_BILLINGBLOCKREASON` |
| `TVGRT` | `I_SALESGROUP` |
| `TVKBT` | `I_SALESOFFICE` |
| `TVKBZ` | `I_SALESAREASALESOFFICE` |
| `TVKGG` | `I_CUSTOMERCONDITIONGROUPVH` |
| `TVKGGT` | `I_CUSTCNDNGROUPVALUEHELPTEXT` |
| `TVKGR` | `I_SALESGROUP` |
| `TVKO` | `I_SALESORGANIZATION` |
| `TVKOT` | `I_SALESORGANIZATION` |
| `TVKOV` | `I_SLSORGANIZATIONDISTRCHNL` |
| `TVKT` | `I_CUSTOMERACCOUNTASSGMTGROUP` |
| `TVKWZ` | `I_CUSTSALESAREATAX` |
| `TVLK` | `I_DELIVERYDOCUMENTTYPE` |
| `TVLKT` | `I_DELIVERYDOCUMENTTYPETEXT` |
| `TVLS` | `I_DELIVERYBLOCKREASON` |
| `TVLST` | `I_DELIVERYBLOCKREASONTEXT` |
| `TVM1` | `I_ADDITIONALMATERIALGROUP1` |
| `TVM1T` | `I_ADDITIONALMATERIALGROUP1` |
| `TVM2` | `I_ADDITIONALMATERIALGROUP2` |
| `TVM2T` | `I_ADDITIONALMATERIALGROUP2` |
| `TVM3` | `I_ADDITIONALMATERIALGROUP3` |
| `TVM3T` | `I_ADDITIONALMATERIALGROUP3` |
| `TVM4` | `I_ADDITIONALMATERIALGROUP4` |
| `TVM4T` | `I_ADDITIONALMATERIALGROUP4` |
| `TVM5` | `I_ADDITIONALMATERIALGROUP5` |
| `TVM5T` | `I_ADDITIONALMATERIALGROUP5` |
| `TVPT` | `I_SALESDOCUMENTITEMCATEGORY` |
| `TVSB` | `I_SHIPPINGCONDITION` |
| `TVSBT` | `I_SHIPPINGCONDITIONTEXT` |
| `TVST` | `I_SHIPPINGPOINT` |
| `TVSTT` | `I_SHIPPINGPOINT` |
| `TVTA` | `I_SALESAREA` |
| `TVTW` | `I_DISTRIBUTIONCHANNEL` |
| `TVTWT` | `I_DISTRIBUTIONCHANNEL` |
| `TVV1` | `I_ADDITIONALCUSTOMERGROUP1` |
| `TVV1T` | `I_ADDITIONALCUSTOMERGROUP1` |
| `TVV2` | `I_ADDITIONALCUSTOMERGROUP2` |
| `TVV2T` | `I_ADDITIONALCUSTOMERGROUP2` |
| `TVV3` | `I_ADDITIONALCUSTOMERGROUP3` |
| `TVV3T` | `I_ADDITIONALCUSTOMERGROUP3` |
| `TVV4` | `I_ADDITIONALCUSTOMERGROUP4` |
| `TVV4T` | `I_ADDITIONALCUSTOMERGROUP4` |
| `TVV5` | `I_ADDITIONALCUSTOMERGROUP5` |
| `TVV5T` | `I_ADDITIONALCUSTOMERGROUP5` |
| `TVZBT` | `I_CUSTOMERPAYMENTTERMS` |
| `UKMBP_CMS_SGM` | `I_CREDITMANAGEMENTACCOUNT` |
| `UKMCRED_SGM0T` | `I_CREDITMANAGEMENTSEGMENT` |
| `VBAK` | `I_SALESDOCUMENT` |
| `VBAP` | `I_SALESDOCUMENTITEM` |
| `VBEP` | `I_SALESDOCUMENTSCHEDULELINE` |
| `VBFA` | `I_SDDOCUMENTMULTILEVELPROCFLOW` |
| `VBFS` | `I_COLLECTIVEPROCESSINGERRORLOG` |
| `VBKD` | `I_SALESDOCUMENT` |
| `VBRK` | `I_BILLINGDOCUMENTBASIC` |
| `VBRP` | `I_BILLINGDOCUMENTITEMBASIC` |
| `VEDA` | `I_SALESDOCUMENT` |
| `VEKP` | `I_HANDLINGUNITHEADER`, `I_HANDLINGUNITTP` |
| `VEPO` | `I_HANDLINGUNITITEM`, `I_HANDLINGUNITITEMTP` |

_Source: `ARS_API_SUCCESSOR.xlsx` (312 rows, snapshot from repository)._

_The successor prefix `I_` denotes a released S/4HANA CDS view; entries
beginning with `FINS_ACDOC_*` are RAP-released structure APIs; `CL_*`
entries are API-released classes (e.g. change-document read/write
tools). CDS views ending in `_2` are the strategic S/4HANA-native
version; the program prefers the highest-version successor available._
