# Plan — Single Program for Vendor/Business Partner Mass Create & Change (S/4HANA)

**Source input:** `revendorandcustomermastercreationmassuploadtempla.zip` (11 workbooks, 17 usable sheets)
**Proposed program:** `YGMS_BP_MASS_UPLOAD` (namespace follows existing `src/ygms_*` convention)
**Status:** PLAN ONLY — no code written yet, per instruction.

---

## 1. What is actually in the ZIP

| # | Workbook | Sheet(s) used | Rows of real data | What it does |
|---|----------|---------------|-------------------|--------------|
| 1 | `Vendor Creation Template_All CC_Sample.xlsx` | `Sheet1` (66 cols) | sample only | Full vendor create: general + CC + purch org + bank + CIN + WHT |
| 2 | `Vendor Extend Template.Ver1.xlsx` | `Vendor Extend` (14 cols) + `Config` | sample only | Extend existing vendor to new company code / purchasing org using a reference |
| 3 | `Bank Details and Bank Key Template_V1.0.xlsx` | `Bank Details` (8 cols), `Bank Key` (9 cols), `Config` | ~50 | Vendor bank accounts + bank master |
| 4 | `Bank key format_Uploadable File.xlsx` | `Sheet1` (11 cols) | ~22 | Bank master only (LSMW project `ZLSMW_BNK`) |
| 5 | `TDS for upload Format_V.01.xlsx` | `Sheet1` (65 cols) | ~178 | Withholding tax types/codes, up to 6 lines per vendor+CC |
| 6 | `TAN details update.xlsx` | `CIN TAN EXEMPTION` (22 cols) | ~11 | TDS lower-deduction certificate / exemption, 2 lines per vendor |
| 7 | `CIN_Email_MSME upload.xlsx` | `CIN MSME` (15 cols), `PAN Update` (2 cols), `Email` (8 cols) | 664 / 52 / 20 | CIN details, PAN, e-mail addresses |
| 8 | `Payment Term and Method update_V.01.xlsx` | `Payment Term` (4 cols), `Payment Method` (3 cols) | ~21 each | ZTERM at CC/POrg, ZWELS at CC |
| 9 | `Purchase Org Data update.xlsx` | `Sheet1` (5 cols) | ~32 | Confirmation control + acknowledgement flag per purch org |
| 10 | `Vendor Partner Function Template.xlsx` | `Sheet1` (35 cols) | sample only | Partner functions, up to 15 per vendor+CC+POrg (LSMW `ZXD01_PFADD_VND`) |
| 11 | `Vendor Block_Unblocked.xlsx` | `Total Block` (9 cols), `Payment block` (3 cols), `Deletion Flag` (3 cols) | 1201 / 13 | Central/CC/POrg block, payment block, deletion flag (LSMW `ZSD_XK05`) |

---

## 2. Critical finding — these templates are ECC-era, not S/4HANA-ready

Every one of these templates is a **BDC/LSMW recording of an ECC transaction**, not an API layout. The evidence is in the columns themselves:

- `Vendor Block_Unblocked.xlsx` carries `Transaction Code = XK05` and LSMW project `ZSD_XK05`.
- `Vendor Partner Function Template.xlsx` carries LSMW project `ZXD01_PFADD_VND` and **screen-control fields** `D0320` ("Process data?") and `USE_ZAV` ("Always X") — these exist only in the XK01/XK02 dynpro flow.
- `TDS for upload Format_V.01.xlsx` carries `D0610` ("Edit withholding tax data?") — again an XK02 screen flag, not a database field.
- `Bank key format_Uploadable File.xlsx` is LSMW project `ZLSMW_BNK`.

In SAP S/4HANA the Business Partner is the **single point of entry**. Per SAP Note 2265093 (*S4TWL – Business Partner Approach*), `XK01`, `XK02`, `XK03`, `MK01/02/03`, `FK01/02/03`, `XD01/02/03`, `VD01/02/03` are either redirected to transaction `BP` or removed outright. A BDC recording against them **will not run** on this system, and even where a redirect exists, the dynpro sequence is different, so screen numbers such as `D0610` / `D0320` are meaningless.

**Consequence for this build:** the templates are re-usable as *layouts* (the business fields and the operational split are correct and well-understood by ONGC users), but the **execution engine must be replaced** with the CVI/BP APIs. Columns that are pure screen-control artefacts (`D0610`, `D0320`, `USE_ZAV`, `Transaction Code`) will be read and ignored — kept in the layout only so users' existing files still load without re-keying.

This is the single biggest scope item and should be signed off before build starts.

---

## 3. Chosen technical approach

### 3.1 Primary API

**`CL_MD_BP_MAINTAIN=>MAINTAIN( )`** with `CVIS_EI_EXTERN`, preceded by `CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE( )`.

Rationale:
- It is the API SAP itself points to after `RFC_CVI_EI_INBOUND_MAIN` was de-supported (1709 FPS02 onward).
- One call maintains **BP header + roles + addresses + bank + tax numbers + supplier general/CC/purchasing data** in a single LUW, which is exactly what template #1 (Vendor Creation) needs.
- CVI synchronisation to `LFA1/LFB1/LFM1` happens automatically on save, so no separate sync step.

`CVIS_EI_EXTERN` decomposes as:

```
CVIS_EI_EXTERN
├── PARTNER   TYPE BUS_EI_EXTERN     "BP side
│   ├── HEADER-OBJECT_INSTANCE-BPARTNER / -BPARTNERGUID
│   ├── HEADER-OBJECT_TASK           " I = insert, U = update, M = modify
│   └── CENTRAL_DATA
│       ├── COMMON-DATA (BP_CENTRALDATA / BP_ORGANIZATION) + COMMON-DATAX
│       ├── ADDRESS-ADDRESSES[]      " postal + communication (phone/mobile/fax/SMTP)
│       ├── ROLE-ROLES[]             " FLVN00 supplier, FLVN01 FI supplier
│       ├── TAXNUMBER-TAXNUMBERS[]   " taxtype + taxnumber (e.g. IN3 = GSTIN)
│       ├── BANKDETAIL-BANKDETAILS[]
│       └── INDUSTRYSECTOR-INDUSTRYSECTORS[]
├── CUSTOMER  TYPE CMDS_EI_EXTERN    " not used in this build
└── VENDOR    TYPE VMDS_EI_EXTERN    " supplier side
    ├── HEADER-OBJECT_INSTANCE-LIFNR, HEADER-OBJECT_TASK
    ├── CENTRAL_DATA-CENTRAL-DATA / -DATAX          → LFA1
    ├── CENTRAL_DATA-BANKDETAIL-BANKDETAILS[]       → LFBK
    ├── COMPANY_DATA-COMPANY[]                      → LFB1
    │   ├── DATA_KEY-BUKRS, DATA / DATAX
    │   ├── DUNNING[]                               → LFB5
    │   └── W_TAX / WTAX_TYPE[]                     → LFBW
    └── PURCHASING_DATA-PURCHASING[]                → LFM1
        ├── DATA_KEY-EKORG, DATA / DATAX
        └── FUNCTIONS[]                             → WYT3 (partner functions)
```

Every `DATA` structure has a parallel `DATAX` structure of `'X'` flags. **Nothing is written unless its `DATAX` flag is set** — this is what makes safe partial updates possible and is the mechanism the change scenarios (5–11 below) rely on.

### 3.2 Fallback / secondary APIs

| Purpose | API | Note |
|---|---|---|
| Supplier-only changes where BP header is untouched | `VMD_EI_API=>MAINTAIN( )` / `MAINTAIN_BAPI( )` | Same `VMDS_EI_EXTERN` payload; lighter, but bypasses BP-side validation — use only for scenarios 5, 8, 9, 10 if `CL_MD_BP_MAINTAIN` proves too heavy |
| Bank master (BNKA) | `BAPI_BANK_CREATE` / `BAPI_BANK_CHANGE`, existence check via `BAPI_BANK_GETDETAIL` | Bank keys are **not** BP objects; separate path |
| Address/communication only | covered by `CVIS_EI_EXTERN` address node | `BAPI_BUPA_ADDRESS_CHANGE` as fallback for the Email sheet |
| CIN legacy fields (`J_1IMOVEND`) | **open — see §7** | No standard API; see risk R3 |

### 3.3 What we deliberately do *not* use

- **LSMW** — not supported for BP in S/4HANA.
- **LTMC / Migration Cockpit** — is SAP's own answer for one-off migrations, but ONGC needs a repeatable, authorisation-controlled, in-system tool with a log, so a custom program is correct here. Worth stating in the FS as the "considered and rejected" alternative.
- **BDC on `BP`** — technically possible but brittle (BP dynpros change with every FPS) and cannot be run in background reliably. Reserved only as last resort for CIN fields.

---

## 4. Program design

### 4.1 Selection screen

```
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.  " Scenario
  PARAMETERS: p_r01 RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND scen,  " Vendor / BP Create
              p_r02 RADIOBUTTON GROUP g1,   " Vendor Extend (CC / Purch Org)
              p_r03 RADIOBUTTON GROUP g1,   " Bank Key (BNKA) create/change
              p_r04 RADIOBUTTON GROUP g1,   " Vendor Bank Details
              p_r05 RADIOBUTTON GROUP g1,   " Withholding Tax / TDS
              p_r06 RADIOBUTTON GROUP g1,   " TDS Exemption / TAN details
              p_r07 RADIOBUTTON GROUP g1,   " CIN / PAN / MSME / E-mail
              p_r08 RADIOBUTTON GROUP g1,   " Payment Terms & Payment Method
              p_r09 RADIOBUTTON GROUP g1,   " Purchasing Org Data
              p_r10 RADIOBUTTON GROUP g1,   " Partner Functions
              p_r11 RADIOBUTTON GROUP g1.   " Block / Unblock / Deletion Flag
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.  " File
  PARAMETERS: p_file  TYPE rlgrap-filename OBLIGATORY,
              p_sheet TYPE string,           " sub-sheet, only shown for multi-sheet scenarios
              p_hdr   TYPE i DEFAULT 1.      " first data row
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.  " Run control
  PARAMETERS: p_test AS CHECKBOX DEFAULT 'X',   " simulate only, no COMMIT
              p_stop AS CHECKBOX,               " stop on first error
              p_dtmpl AS CHECKBOX.              " download blank template instead of running
SELECTION-SCREEN END OF BLOCK b3.
```

- `USER-COMMAND scen` + `AT SELECTION-SCREEN OUTPUT` drives dynamic screen behaviour: `p_sheet` is only visible/mandatory for scenarios 3, 7, 8 and 11 (the multi-sheet workbooks), and is filled with an F4 list of that scenario's valid sheet names.
- `p_dtmpl` lets a user download the correct blank template for the selected radio button — this removes the "which file do I use?" support load, and guarantees column order matches what the program expects.
- Test run is **on by default**. Real posting requires deliberately unticking it.

### 4.2 Object model

```
YGMS_BP_MASS_UPLOAD            (report – selection screen, dispatch, ALV)
YGMS_CL_BP_UPL_FACTORY         (maps radio button → handler instance)
YGMS_IF_BP_UPL_HANDLER         (interface: GET_LAYOUT, MAP, VALIDATE, EXECUTE)
  YGMS_CL_BP_UPL_CREATE        (scenario 1)
  YGMS_CL_BP_UPL_EXTEND        (scenario 2)
  YGMS_CL_BP_UPL_BANKKEY       (scenario 3)
  YGMS_CL_BP_UPL_BANKDET       (scenario 4)
  YGMS_CL_BP_UPL_WHT           (scenario 5)
  YGMS_CL_BP_UPL_TDSEXEM       (scenario 6)
  YGMS_CL_BP_UPL_CIN           (scenario 7)
  YGMS_CL_BP_UPL_PAYMENT       (scenario 8)
  YGMS_CL_BP_UPL_PURORG        (scenario 9)
  YGMS_CL_BP_UPL_PARTFN        (scenario 10)
  YGMS_CL_BP_UPL_BLOCK         (scenario 11)
YGMS_CL_BP_UPL_EXCEL           (xls via ALSM_EXCEL_TO_INTERNAL_TABLE, xlsx via CL_FDT_XL_SPREADSHEET)
YGMS_CL_BP_UPL_CVIS            (shared CVIS_EI_EXTERN builder + CL_MD_BP_MAINTAIN wrapper)
YGMS_CL_BP_UPL_LOG             (per-row message collection, ALV, APPL_LOG persistence)
YGMS_BP_UPL_LOG                (transparent table – audit trail)
YGMS_BP_UPL_LAYOUT             (customising table – column position → field name, per scenario)
```

Adding a 12th template later = one new handler class + rows in `YGMS_BP_UPL_LAYOUT`. No change to the report.

**Note on the Excel reader:** the existing `src/ygms_ks01_upload.prog.abap` already implements the dual `.xls`/`.xlsx` reader pattern in FORMs `UPLOAD_XLS` / `UPLOAD_XLSX`. `YGMS_CL_BP_UPL_EXCEL` should be a straight OO port of that logic so behaviour is consistent with the cost-centre uploads users already know.

### 4.3 Processing flow (identical for every scenario)

1. **Read** — file → raw cell table (row, col, value).
2. **Skip header block** — templates have 1–6 header rows (see §5, "Data starts") plus, in several files, `Sample`-marked rows in column A. Rows whose column A contains `Sample`/`Sample data` are dropped.
3. **Map** — raw cells → typed structure, using `YGMS_BP_UPL_LAYOUT` so a column insertion is customising, not a code change.
4. **Convert** — dates `DD.MM.YYYY` → `YYYYMMDD`; amounts with separators → packed; `CONVERSION_EXIT_ALPHA_INPUT` on `LIFNR`/`KUNNR`/`BANKL`/`AKONT`; upper-case on key fields; currency codes normalised (the TAN template contains lower-case `inr`).
5. **Validate** — three layers:
   - *Structural*: mandatory columns present, lengths within DDIC.
   - *Existence*: check `LFA1`/`BUT000` for the BP, `T001`/`T024E`/`T077K`/`BNKA`/`T059P`/`T042Z`/`T052` for config values. All read once into hashed tables before the loop.
   - *Business*: rules from the templates' own guideline rows — e.g. PAN mandatory for India vendors, `WEBRE = 'X'` for domestic / blank for import, bank fields all-or-nothing, `SPERQ`/`SPERR` mutual exclusivity.
6. **Build payload** — per scenario, per §5. Set `DATAX` only for columns actually supplied.
7. **Simulate** — `CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE( )` always runs, in test *and* productive mode.
8. **Post** — only if not test run: `MAINTAIN( )`, then `COMMIT WORK AND WAIT` per row (row-level commit so one bad record does not roll back the batch). On error: `ROLLBACK WORK` for that row, `MESSAGE_STORE` the messages, continue.
9. **Log** — ALV with traffic-light icon, key fields, message type/id/number/text; plus write to `YGMS_BP_UPL_LOG` and SLG1 (object `YGMS_BP_UPL`).
10. **Error file** — button on the ALV to download failed rows in the *original template layout* plus an appended error column, so the user corrects and re-uploads the same file.

### 4.4 Authorisation

- `S_TCODE` on the new transaction (propose `YBPUPL`).
- `B_BUPA_RLT` (BP role) and `B_BUPA_GRP` for BP-side.
- `F_LFA1_BUK` (ACTVT 01/02, BUKRS) and `M_LIEF_EKO` (EKORG) — checked per row *before* the API call, against the company code / purchasing org in that row, not just at program start.
- `F_BNKA_MAN` for scenario 3.
- Separate check for productive (non-test) execution so a wider group can simulate than can post.

---

## 5. Scenario-by-scenario field mapping

Legend: `→` target field. `[X]` = corresponding `DATAX` flag must be set.

### Scenario 1 — Vendor / BP Create (`Vendor Creation Template_All CC_Sample.xlsx`, `Sheet1`, 66 cols)
Header rows 1–3 (tech name / description / mandatory-rule). **Data starts row 4**; rows marked `Sample data` in col A are skipped.

| Col | Template | Target |
|---|---|---|
| 2 | `LIFNR` | must be **blank** for create (internal numbering). If filled → treat as change. |
| 3 | `BUKRS` | `VENDOR-COMPANY_DATA-COMPANY-DATA_KEY-BUKRS` |
| 4 | `EKORG` | `VENDOR-PURCHASING_DATA-PURCHASING-DATA_KEY-EKORG` |
| 5 | `KTOKK` | `VENDOR-CENTRAL_DATA-CENTRAL-DATA-KTOKK` `[X]` + drives BP grouping & role |
| 6 | `TITLE_MEDI` | `PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-TITLE_KEY` |
| 7–10 | `NAME1..NAME4` | `PARTNER-…-BP_ORGANIZATION-NAME1..NAME4` `[X]` |
| 11–12 | `SORT1`, `SORT2` | `PARTNER-…-BP_CENTRALDATA-SEARCHTERM1/2` `[X]` |
| 13,14,16 | `STR_SUPPL1/2/3` | address `POSTAL-DATA-STR_SUPPL1/2/3` |
| 15 | `STREET` | address `POSTAL-DATA-STREET` |
| 17 | `CITY2` | address `POSTAL-DATA-DISTRICT` |
| 18 | `POST_CODE1` | address `POSTAL-DATA-POSTL_COD1` |
| 19 | `CITY1` | address `POSTAL-DATA-CITY` |
| 20 | `COUNTRY` | address `POSTAL-DATA-COUNTRY` |
| 21 | `REGION` | address `POSTAL-DATA-REGION` |
| 22 | `LANGU` | address `POSTAL-DATA-LANGU` |
| 23–26 | `TEL_NUMBER`/`_EXTENS`(1,2) | address `COMMUNICATION-PHONE-PHONE[]` (`TELEPHONE`, `EXTENSION`, `STD_NO`) |
| 27–28 | `MOB_NUMBER`(1,2) | same `PHONE[]` table with `R_3_USER = '3'` (mobile) |
| 29 | `FAX_NUMBER` | address `COMMUNICATION-FAX-FAX[]` |
| 30–31 | `SMTP_ADDR`(1,2) | address `COMMUNICATION-SMTP-SMTP[]` — first entry `STD_NO = 'X'` |
| 32 | `KUNNR` | `VENDOR-CENTRAL_DATA-CENTRAL-DATA-KUNNR` `[X]` |
| 33 | `VBUND` | `…-CENTRAL-DATA-VBUND` `[X]` |
| 34 | `KONZS` | `…-CENTRAL-DATA-KONZS` `[X]` |
| 35 | `STCD3` | `…-CENTRAL-DATA-STCD3` `[X]` **and** BP tax number, category `IN3` (GSTIN) |
| 36 | `STCD5` | `…-CENTRAL-DATA-STCD5` `[X]` |
| 37 | `STCEG` | `…-CENTRAL-DATA-STCEG` `[X]` |
| 38–39 | `J_1KFTBUS`, `STENR` | `…-CENTRAL-DATA-J_1KFTBUS`, `-STENR` `[X]` (Argentina/US fields — likely unused at ONGC) |
| 40 | `BRSCH` | `…-CENTRAL-DATA-BRSCH` `[X]` + `PARTNER-…-INDUSTRYSECTOR` |
| 41–46 | `BANKS_01`,`BANKL_01`,`BANKN_01`,`KOINH_01`,`BKONT`,`IBAN` | `VENDOR-CENTRAL_DATA-BANKDETAIL-BANKDETAILS[]` → `LFBK`; mirrored to `PARTNER-…-BANKDETAIL`. **Pre-check `BNKA`** — fail the row with a clear message if the bank key does not exist (do *not* auto-create; that is scenario 3) |
| 47 | `AKONT` | `COMPANY-DATA-AKONT` `[X]` |
| 48 | `FDGRV` | `COMPANY-DATA-FDGRV` `[X]` |
| 49 | `ALTKN` | `COMPANY-DATA-ALTKN` `[X]` |
| 50 | `ZTERM` (CC) | `COMPANY-DATA-ZTERM` `[X]` |
| 51 | `REPRF` | `COMPANY-DATA-REPRF` `[X]` |
| 52 | `ZWELS` | `COMPANY-DATA-ZWELS` `[X]` |
| 53 | `ZAHLS` | `COMPANY-DATA-ZAHLS` `[X]` |
| 54 | `HBKID` | `COMPANY-DATA-HBKID` `[X]` |
| 55 | `VEN_CLASS` | CIN — `J_1IMOVEND-VEN_CLASS`, see §7 Q4 |
| 56 | `J_1ISSIST` | CIN — SSI/MSME status, see §7 Q4 |
| 57 | `J_1IPANNO` | CIN PAN — see §7 Q3 (BP tax number vs `J_1IMOVEND`) |
| 58–60 | `QLAND`, `WITHT`, `WT_WITHCD` | `COMPANY-W_TAX-WTAX_TYPE[]` → `LFBW`; `QLAND` → `COMPANY-DATA-QLAND` `[X]` |
| 61 | `WAERS` | `PURCHASING-DATA-WAERS` `[X]` |
| 62 | `ZTERM` (POrg) | `PURCHASING-DATA-ZTERM` `[X]` — note: **same technical name as col 50, different org level.** Layout table must key on column position, not field name |
| 63 | `KALSK` | `PURCHASING-DATA-KALSK` `[X]` |
| 64 | `WEBRE` | `PURCHASING-DATA-WEBRE` `[X]` |
| 65–66 | `INCO1`, `INCO2` | `PURCHASING-DATA-INCO1`, `-INCO2` `[X]` |

Roles set automatically: `FLVN00` (Supplier) always; `FLVN01` (FI Supplier) when a company code is supplied. BP category = `2` (Organisation) unless `KTOKK` is the employee-vendor group (then `1` = Person — **confirm the group, §7 Q2**).

### Scenario 2 — Vendor Extend (`Vendor Extend Template.Ver1.xlsx`, `Vendor Extend`)
Header rows 1–6, **data starts row 7**. Columns 2–4 = target `LIFNR`/`BUKRS`/`EKORG`; columns 5–7 = **reference** `LIFNR`/`BUKRS`/`EKORG`; column 8 = "always X" (ignored); columns 9–14 = overrides `AKONT`, payment method, double-invoice check, `WAERS`, `KALSK`, `WEBRE`.

Logic: read the reference vendor's `LFB1`/`LFM1` via `VMD_EI_API` read (or direct select), copy into a new `COMPANY[]` / `PURCHASING[]` entry keyed on the *target* org units, then overlay any of columns 9–14 that are filled. Reject if the target CC/POrg combination already exists (that is a change, not an extend).

> Note: the sheet's `max_column` reads as 16380 because of stray formatting; only columns 1–14 carry meaning. The reader must bound itself at the last non-empty header cell.

### Scenario 3 — Bank Key (`Bank key format_Uploadable File.xlsx` `Sheet1`, or `Bank Details…` `Bank Key` sheet)
Not a BP object. `BAPI_BANK_GETDETAIL` → if not found `BAPI_BANK_CREATE`, else `BAPI_BANK_CHANGE`, then `BAPI_TRANSACTION_COMMIT`.

`BANKS`→`BANK_CTRY`, `BANKL`→`BANK_KEY`, `BANKA`→`BANK_NAME`, `PROVZ`→`REGION`, `STRAS`→`STREET`, `ORT01`→`CITY`, `BRNCH`→`BANK_BRANCH`, `SWIFT`→`SWIFT_CODE`, `BNKLZ`→`BANK_NO` (template says keep blank).

The two source files differ (11 vs 9 columns, different header depth) — support **both**, distinguished by the `p_sheet` value.

### Scenario 4 — Vendor Bank Details (`Bank Details and Bank Key Template_V1.0.xlsx`, `Bank Details`)
Header rows 1–6, **data starts row 7**. `LIFNR`, `BUKRS`, `BANKS`, `BANKL`, `BANKN`, `KOINH`, `IBAN` → `VENDOR-CENTRAL_DATA-BANKDETAIL-BANKDETAILS[]` and `PARTNER-CENTRAL_DATA-BANKDETAIL`.

Key decision needed (§7 Q5): **append vs replace**. Multiple rows for one `LIFNR` = multiple bank accounts; `BVTYP` (partner bank type) is *not* in the template, so the program must generate it (`0001`, `0002`, …) or leave blank. `BUKRS` is in the template but `LFBK` is client-level — it is used only for the authorisation check.

### Scenario 5 — Withholding Tax / TDS (`TDS for upload Format_V.01.xlsx`, `Sheet1`, 65 cols)
Header rows 1–2, **data starts row 3**; `Sample` rows in col A skipped. Six repeating blocks (`_01` … `_06`), each producing one `LFBW` line:

| Template suffix group | `VMDS_EI_WTAX_TYPE` / `LFBW` |
|---|---|
| `WITHT_nn` | `DATA_KEY-WITHT` |
| `WT_WITHCD_nn` | `DATA-WT_WITHCD` `[X]` |
| `WT_SUBJCT_nn` | `DATA-WT_SUBJCT` `[X]` |
| `QSREC_nn` | `DATA-QSREC` `[X]` |
| `WT_WTSTCD_nn` | `DATA-WT_WTSTCD` `[X]` |
| `WT_EXNR_nn` | `DATA-WT_EXNR` `[X]` |
| `WT_EXRT_nn` | `DATA-WT_EXRT` `[X]` |
| `WT_WTEXRS_nn` | `DATA-WT_WTEXRS` `[X]` |
| `WT_EXDF_nn` / `WT_EXDT_nn` | `DATA-WT_EXDF` / `-WT_EXDT` `[X]` (`DD.MM.YYYY` → `YYYYMMDD`) |

`QLAND` → `COMPANY-DATA-QLAND` `[X]`. `D0610` ignored (screen flag). A block is generated only where `WITHT_nn` is non-blank.

### Scenario 6 — TDS Exemption / TAN details (`TAN details update.xlsx`, `CIN TAN EXEMPTION`)
Header row 1, **data starts row 2**. Two repeating blocks (`_1`, `_2`). Business fields → `LFBW`: `taxtype_n`→`WITHT`, `taxcode_n`→`WT_WITHCD`, `Certificate_n`→`WT_EXNR`, `Exemption_rate_n`→`WT_EXRT`, `Validfrom_n`→`WT_EXDF`, `Validto_n`→`WT_EXDT`.

`Section_code_n`, `threshold_n`, `Currency_n` have **no `LFBW` home** — these belong to India-specific TDS threshold/section configuration, not vendor master. `J_1IEWT_CERT` is a *form/section-indicator config* table (`BUKRS`, `BUPLA`, `QSCOD`, `J_1ISECTYP`, `TDFORM`, `J_1INOGROUP`), not vendor exemption data. **This scenario needs functional clarification before build — §7 Q6.** Note also that in the sample data `Section_code_1/2` equal the company code, which suggests the column is being used as Business Place / Section Code.

### Scenario 7 — CIN / PAN / MSME / E-mail (`CIN_Email_MSME upload.xlsx`) — three sub-sheets
- **`Email`** (no header row — data from row 1; cols: tcode?, `LIFNR`, 4 × `X` view flags, e-mail 1, e-mail 2) → address `COMMUNICATION-SMTP-SMTP[]`. Straightforward and safe. **A header row must be added to the template.**
- **`PAN Update`** (`Sap V Code`, `PAN No.`; header row 1, sample row 2) → see §7 Q3.
- **`CIN MSME`** (15 cols: ECC number, excise reg no., range, division, commissionerate, CST no., LST no., service tax reg no., PAN, SSI status, excise tax indicator, vendor type) → all `J_1IMOVEND` fields. **Post-GST these are largely dead fields.** 664 rows exist in the file, which suggests this was a historical one-time load. Recommend **descoping** unless ONGC confirms active use — §7 Q4.

### Scenario 8 — Payment Terms & Payment Method (`Payment Term and Method update_V.01.xlsx`)
- `Payment Term` sheet (`LIFNR`, `BUKRS`, `EKORG`, `ZTERM`) → sets `COMPANY-DATA-ZTERM` `[X]` **and** `PURCHASING-DATA-ZTERM` `[X]` (both org levels, since both keys are supplied).
- `Payment Method` sheet (`LIFNR`, `BUKRS`, `ZWELS`) → `COMPANY-DATA-ZWELS` `[X]`.

Both sheets: header row 1, data from row 2. The simplest scenarios; good candidates for the first build increment.

### Scenario 9 — Purchasing Org Data (`Purchase Org Data update.xlsx`, `Sheet1`)
Header row 1, data from row 2, `Sample` rows in col A skipped. `LIFNR`(c2), `EKORG`(c3), `CONFIRMATION CONTROL`(c4)→`PURCHASING-DATA-BSTAE` `[X]`, `CHECK ACKNOWLEDGEMENT`(c5)→`PURCHASING-DATA-KZABS` `[X]`.

> `BSTAE` / `KZABS` are the assumed technical names — the template gives descriptions only. **Confirm against `LFM1` in the target system — §7 Q7.**

### Scenario 10 — Partner Functions (`Vendor Partner Function Template.xlsx`, `Sheet1`)
Header rows 1–9, **data starts row 10**. Columns 4–5 (`D0320`, `USE_ZAV`) are **screen-control artefacts — ignore**. Fifteen `PARVW_nn` / `GPARN_nn` pairs (note the file's odd ordering: `_05`–`_15` appear at cols 6–27, then `_01`–`_04` at cols 28–35 — the mapper must key on the header text, not sequential position).

Each non-blank pair → `PURCHASING_DATA-PURCHASING-FUNCTIONS[]` (`WYT3`): `DATA_KEY-PARVW`, `DATA-LIFN2 = GPARN_nn`, `DATA_KEY-PARZA` generated sequentially per `PARVW`. Validate each `PARVW` against `TPAR` and against the partner schema assigned to the vendor's account group.

### Scenario 11 — Block / Unblock / Deletion Flag (`Vendor Block_Unblocked.xlsx`) — three sub-sheets
- **`Total Block`** (header rows 1–6, data from row 7; `Transaction Code` col ignored):
  `SPERR`→`CENTRAL-DATA-SPERR` (central posting block) `[X]`;
  `SPERR_1`→`COMPANY-DATA-SPERR` `[X]`;
  `SPERM`→`CENTRAL-DATA-SPERM` (central purchasing block) `[X]`;
  `SPERM_1`→`PURCHASING-DATA-SPERM` `[X]`;
  `SPERQ`→`CENTRAL-DATA-SPERQ` `[X]`.
  Validation: `SPERR_1` requires `BUKRS`; `SPERM_1` requires `EKORG`; `SPERQ` must be blank when a CC/POrg-level block is set (stated in the template's own guideline).
- **`Payment block`** (1201 rows) → `COMPANY-DATA-ZAHLS` `[X]`. Header says `B` = block, blank = unblock, but the sample data holds `X` — **normalise and confirm, §7 Q8.**
- **`Deletion Flag`** → `COMPANY-DATA-LOEVM` `[X]` (and `CENTRAL-DATA-LOEVM` if no `BUKRS` given).

**Unblocking must work.** Clearing a field through this API means setting `DATA-<field> = space` *with* `DATAX-<field> = 'X'`. The mapper therefore has to distinguish "cell left empty = don't touch" from "cell explicitly cleared = blank it out". Proposal: an explicit sentinel (`#BLANK#` or a dedicated `Action` column with values `BLOCK`/`UNBLOCK`) added to the template. Without this, unblock is impossible — this is a **template change**, not just a code decision.

---

## 6. Delivery phases

| Phase | Content | Why this order |
|---|---|---|
| **0** | Sign-off on §2 (BDC→API rework), §7 open questions, and the template changes flagged in scenarios 7 and 11 | Everything downstream depends on these answers |
| **1** | Framework: report, radio group, dynamic screen, Excel reader, layout table, log table, ALV, error-file download, authorisation. Plus scenarios **8 and 9** as the proving ground | Smallest scenarios; validates the whole pipeline end-to-end with low risk |
| **2** | Scenarios **4, 3, 11** (bank details, bank key, block/unblock) | Highest immediate operational volume (1201 payment-block rows waiting) |
| **3** | Scenario **1** (vendor create) + **2** (extend) | Largest and most complex; benefits from a proven framework |
| **4** | Scenarios **5, 10** (TDS, partner functions) | Repeating-block mapper is reusable between them |
| **5** | Scenarios **6, 7** (TAN exemption, CIN/PAN/MSME) | Blocked on functional answers; may be descoped |

Each phase ends with a template download (`p_dtmpl`) matching exactly what that phase's code reads.

---

## 7. Open questions — need answers before build

| # | Question | Why it matters | Suggested default |
|---|---|---|---|
| **Q1** | Which S/4HANA release/FPS is the target system? | Determines whether `CL_MD_BP_MAINTAIN` or the older `RFC_CVI_EI_INBOUND_MAIN` is available, and whether `XK02` still redirects | Assume 2021+ → `CL_MD_BP_MAINTAIN` |
| **Q2** | BP grouping ↔ vendor account group mapping, and internal vs external numbering per group. Which `KTOKK` is the employee-vendor group? | Scenario 1 cannot set BP category/grouping without it | Read from `TB001`/`CVI_VEND_LINK` config at runtime |
| **Q3** | PAN: BP tax number category (which `IN*` code?) or `J_1IMOVEND-J_1IPANNO`, or both? GSTIN is `IN3` — PAN's category must be confirmed from `TFKTAXNUMTYPE` in the target system | Scenario 1 col 57, Scenario 7 `PAN Update` sheet | Check `TFKTAXNUMTYPE` / SAP Note 775919 in-system |
| **Q4** | Are the legacy CIN excise fields (ECC no., excise reg./range/division/commissionerate, CST, LST, service tax) still in use post-GST? Is `J_1IMOVEND` still maintained? Where does MSME/Udyam status live? | Decides whether Scenario 7's `CIN MSME` sheet is built at all. There is **no standard API** for `J_1IMOVEND` — options are direct `MODIFY` (needs explicit sign-off) or BDC on `BP` (fragile) | Recommend **descope** the excise fields; keep PAN + MSME only |
| **Q5** | Vendor bank details: append new accounts, or replace the whole set? How is `BVTYP` (partner bank type) determined — it is absent from the template | Scenario 4 correctness; wrong choice silently deletes payment-relevant data | Recommend **append**, auto-generate `BVTYP` `0001…n`, add `BVTYP` as a template column |
| **Q6** | Scenario 6: what do `Section_code`, `threshold`, `Currency` actually drive? They have no `LFBW` field. Is this really vendor master, or TDS *configuration* (thresholds are normally config, not master data)? | Scenario 6 cannot be specified without this | Park Scenario 6 until answered |
| **Q7** | Confirm `LFM1` technical names for "Confirmation Control" and "Check Acknowledgement" (assumed `BSTAE` / `KZABS`) | Scenario 9 | Verify in SE11 on `LFM1` |
| **Q8** | Block/unblock templates: indicator value is documented as `B` but sample data uses `X`. And how is **unblock** signalled — empty cell, or explicit marker? | Scenario 11; without an explicit marker, unblock is not possible (see §5 note) | Add an explicit `Action` column (`BLOCK`/`UNBLOCK`) to the template |
| **Q9** | Is customer-side (`CMDS_EI_EXTERN`) creation in scope? The ZIP is named "vendor **and customer** master creation" but contains only vendor templates | Affects whether the handler interface reserves a customer branch | Assume **vendor only**; design keeps the CVIS customer node available |
| **Q10** | Foreground/background: must this run in background (no GUI)? | `CL_GUI_FRONTEND_SERVICES` and `ALSM_EXCEL_TO_INTERNAL_TABLE` are GUI-only; background needs the file on the app server (`OPEN DATASET`) | Support both: `p_loc` radio (PC / server) |
| **Q11** | Are there ONGC-specific mandatory fields, validations, or approval workflow (e.g. maker–checker) on vendor creation? | Could turn "post immediately" into "stage and release" | Assume direct post; log every row |

---

## 8. Risks

| ID | Risk | Impact | Mitigation |
|---|---|---|---|
| **R1** | Existing user files are the ECC layouts; users will keep sending files with `D0610`/`USE_ZAV`/tcode columns | Confusion, failed loads | Read and ignore the dead columns; ship `p_dtmpl` blank-template download; keep column *positions* identical to the originals wherever possible |
| **R2** | `CL_MD_BP_MAINTAIN` returns generic messages that do not identify which sub-object failed | Users cannot self-correct | Always run `VALIDATE_SINGLE` first; map `BAPIRET2` to the source column via the layout table; include Excel row + column in every log line |
| **R3** | No standard API for `J_1IMOVEND` (Scenario 7 CIN fields) | Either an unsupported direct `MODIFY`, or a fragile BDC | Descope if possible (Q4). If mandatory, direct `MODIFY` with explicit written sign-off, full change log, and a documented restriction to non-key fields |
| **R4** | Row-level `COMMIT WORK AND WAIT` on 1200+ rows is slow, and CVI synchronisation adds overhead | Long runtimes, possible dialog timeout | Support background execution; add an optional commit-block size; recommend batches ≤ 500 rows |
| **R5** | Partial success mid-row — BP created but company-code data rejected | Orphaned BP with no CC data | Single LUW per row via `CVIS_EI_EXTERN` (BP + CC + POrg in one `MAINTAIN` call); `ROLLBACK WORK` on any error in that row |
| **R6** | Unblock/clear-field cannot be expressed with an empty cell | Scenario 11 half-functional | Explicit `Action` column (Q8) — must be agreed as a template change |
| **R7** | Duplicate vendor creation on re-upload of the same file | Duplicate BPs | Pre-check on `NAME1` + `POST_CODE1` + `STCD3`/PAN before create; warn and skip, with an override checkbox |
| **R8** | Number ranges and BP groupings differ between DEV/QAS/PRD | Works in test, fails in production | Read all config at runtime; never hard-code; UAT in a client with production config copy |

---

## 9. Test plan (outline)

1. **Unit** — Excel reader against all 11 workbooks in both `.xls` and `.xlsx`, including the malformed `Vendor Extend` sheet (`max_column` 16380) and the header-less `Email` sheet.
2. **Negative** — for every scenario: missing mandatory column, invalid config value (bad `BUKRS`/`EKORG`/`AKONT`/`BANKL`), wrong date format, duplicate key, no authorisation. Every one must produce a readable ALV message, not a dump.
3. **Test-run parity** — a test run and a productive run of the same file must produce identical message sets (test run just skips `COMMIT`).
4. **Round trip** — create a vendor (Sc. 1) → extend it (Sc. 2) → add bank (Sc. 4) → add TDS (Sc. 5) → add partner functions (Sc. 10) → block (Sc. 11) → unblock (Sc. 11) → verify in `BP`, `LFA1`, `LFB1`, `LFM1`, `LFBK`, `LFBW`, `WYT3`, `BUT000`, `BUT0BK`.
5. **Volume** — the real 1201-row payment-block sheet and the 664-row CIN sheet, timed, in background.
6. **Regression** — confirm CVI sync fired: every created BP has matching `LFA1` and `BUT000` entries and a `CVI_VEND_LINK` row.

---

## 10. Deliverables

- `YGMS_BP_MASS_UPLOAD` report + transaction `YBPUPL`
- 13 classes / 1 interface per §4.2
- 2 tables: `YGMS_BP_UPL_LOG` (audit), `YGMS_BP_UPL_LAYOUT` (column mapping, with SM30 maintenance view)
- Application log object `YGMS_BP_UPL`
- 11 blank templates, regenerated to match the code, downloadable from the program
- Technical spec (TSD) + user manual, in the style of the existing `TSD_*.docx` in this repo
