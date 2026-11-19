# Creating customers and vendors in SAP S/4HANA (business partner) — working guide

This guide records how customer and vendor programs are converted from ECC to
S/4HANA in this repository. Most of the work so far is for Cipla. Read it
before converting or writing any program that creates or changes a customer or
vendor. Everything here is taken from the programs in `src/`. Where a point was
checked against SAP sources, it says so.

---

## 1. The rules

1. **In S/4HANA a customer or vendor *is* a business partner (BP).**
   - XD01/02/05, XK01/02/05, FD01/02, FK01/02/05, VD01/02, MK01/02 are gone.
   - A `CALL TRANSACTION` or batch input (BDC) against them fails.
   - Every BDC has to be replaced.
2. **Post only through the BP API `CL_MD_BP_MAINTAIN`,** with structure
   `CVIS_EI_EXTERN`, which holds partner, customer and vendor parts. CVI
   (Customer/Vendor Integration) then writes KNA1/KNB1/KNVV or
   LFA1/LFB1/LFBK/LFM1. SAP Note 2417298 names this API for external creation
   and change.
3. **Never write into standard master data tables yourself** (no
   INSERT/UPDATE/MODIFY on LFA1, KNA1, BUT000 …).
   - Data goes in only through FMs/classes.
   - A program's *own* Z log table (for example `ZFI_HONC_VEND`) may stay as
     it is.
4. **Don't use `VMD_EI_API=>MAINTAIN` / `CMD_EI_API=>MAINTAIN` on their own.**
   They bypass the BP.
   - The read-only extractors (`CMD_EI_API_EXTRACT`, `VMD_EI_API_EXTRACT`)
     are fine.
5. **The tables J_1IMOVEND / J_1IMOCUST are gone.** PAN, GST class (VEN_CLASS)
   and MSME status are fields of LFA1/KNA1 now.
6. **ABAP Cloud:** `CL_MD_BP_MAINTAIN`, `CVI_*_LINK` and the `CVIC_*` tables
   are not released for ABAP Cloud, so this is classic ABAP (tier 2).
   - Put this line in the program header:
     `ATC variant ABAP_CLOUD_READINESS - known exemption`.
   - The audit `tools/audit_s4_ready.py` checks for it.

---

## 2. The save sequence (always the same)

```abap
DATA: lt_map  TYPE mdg_bs_bp_msgmap_t,
      lt_in   TYPE cvis_ei_extern_t,
      lt_ret  TYPE bapiretm,
      ls_cret TYPE bapiret2.

" 1. validate
cl_md_bp_maintain=>validate_single( EXPORTING i_data = ls_data
                                    IMPORTING et_return_map = lt_map ).
" any line of LT_MAP with TYPE CA 'EAX' -> stop, report it

" 2. maintain
APPEND ls_data TO lt_in.
cl_md_bp_maintain=>maintain( EXPORTING i_data = lt_in IMPORTING e_return = lt_ret ).
" LT_RET (BAPIRETM) lines hold a nested table OBJECT_MSG - read it with
" ASSIGN COMPONENT 'OBJECT_MSG' (the name is release-dependent), MOVE-CORRESPONDING
" each line to BAPIRET2; TYPE CA 'EAX' -> ROLLBACK WORK, reset, stop

" 3. commit with WAIT, and read the verdict
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = abap_true
                                        IMPORTING return = ls_cret.
" LS_CRET-TYPE CA 'EAX' -> the update terminated; tell the user to look in SM13

" 4. reset the BP buffers before the next record
CALL FUNCTION 'BUP_MEMORY_CENTRAL_INIT'.   " in TRY ... CATCH cx_sy_dyn_call_illegal_func
```

Then read back what CVI made:

- Vendor: `SELECT SINGLE vendor FROM cvi_vend_link WHERE partner_guid = @lv_guid`
- Customer: `cvi_cust_link`, field `customer`
- Partner number: `BUT000` via `partner_guid`

If the partner exists but no vendor or customer does, the supplier/customer
roles of the account group are missing.

Use `BAPI_TRANSACTION_COMMIT` with `wait`, not a bare `COMMIT WORK`: the BP
hangs its end-of-LUW processing on it.

A worked, reusable example is the `SAVE` method in
`src/zfi_cimmra_bp.prog.abap` or `src/zfi_emp_upld.prog.abap`.

---

## 3. Filling `CVIS_EI_EXTERN` — create

### 3.1 Partner header and control

```abap
lv_guid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
ls_data-partner-header-object_task                  = 'I'.
ls_data-partner-header-object_instance-bpartnerguid = lv_guid.
ls_data-partner-header-object_instance-bpartner     = lv_lifnr. " ONLY with external numbering
ls_data-partner-central_data-common-data-bp_control-category = '1'.  " 1 person, 2 organisation
ls_data-partner-central_data-common-data-bp_control-grouping = lv_grp.
```

**Grouping and roles come from the account group:**

| | Vendor | Customer |
|---|---|---|
| Grouping | `CVIC_VEND_TO_BP1` (account_group → grouping) | `CVIC_CUST_TO_BP1` |
| Roles | `CVIC_VEND_TO_BP2` (account_group, role) | `CVIC_CUST_TO_BP2` |

Append each role:

```abap
ls_role-task     = 'I';
ls_role-data_key = <role>;
APPEND ls_role TO ls_data-partner-central_data-role-roles.
```

If either mapping is missing, stop with a message naming the table.

### 3.2 Person or organisation

- **Organisation (category 2):** name in
  `partner-central_data-common-data-bp_organization`, fields NAME1–NAME4.
- **Person (category 1):** name in `...-bp_person`, fields FIRSTNAME,
  MIDDLENAME, LASTNAME (LASTNAME is required), CORRESPONDLANGUAGE.
  - **Employees and individuals are persons.** In ZFI_EMP_UPLD the user
    explicitly asked for this: "employee … organization will not be created".
  - If only a full name is known, take the last word as LASTNAME.
- **Changing an existing partner:** read `BUT000-TYPE` first. Never send
  NAME1–4 to a person partner, which would fail. Leave its name alone and say
  so in the log.
- **Title:** `bp_centraldata-title_key`. Files carry the text (`Mr.`, `Dr.`);
  translate it through `TSAD3T-TITLE_MEDI` to the key.
- **Search term:** `bp_centraldata-searchterm1` (and searchterm2).

Every `data-xxx` needs its `datax-xxx = abap_true`. A generic helper `SET`
(ASSIGN COMPONENT on data and datax) avoids typos and compile breaks across
releases; see `SET` in either class.

### 3.3 Address and communication

```abap
ls_adr-task = 'I'.                                     " change: 'U' and data_key-operation = 'XXDFLT'
ls_adr-data-postal-data-country / region / city / district / postl_cod1 /
                       street / str_suppl1..3 / langu  (+ datax)
APPEND ls_adr TO ls_data-partner-central_data-address-addresses.
```

**Telephone / mobile** (`...-communication-phone-phone`, `BUS_EI_BUPA_TELEPHONE`):

```abap
ls_tel-contact-task           = 'I'.
ls_tel-contact-data-telephone = ...
ls_tel-contact-data-std_no    = abap_true.   " first one only
```

- **`R_3_USER` is a code, not a flag:** space = landline, 1 = standard
  landline, 2 = mobile, 3 = standard mobile (SAP Help). Any other value
  (e.g. `X`) causes `MESSAGE_TYPE_X` in the save.
- Fax: `...-fax-fax` with `contact-data-fax`.
- E-mail: `...-smtp-smtp` with `contact-data-e_mail`.
- On a **change**, read ADR2/ADR3/ADR6 for the partner's address
  (`addrnumber = LFA1-ADRNR`, `persnumber = space`). Add only numbers that
  are not there.
- To **replace** a number as FK02 did, update the existing entry:
  `contact-task = 'U'`, `contact-data-consnumber = <its CONSNUMBER>`, new
  value + datax. Setting `STD_NO` on a new entry does *not* move the standard
  flag (KBA 3565137).
- Put communication in the address node, not the address-independent node
  (KBA 3366047).

### 3.4 Tax numbers (GSTIN etc.)

GSTIN and other tax numbers belong to the partner (DFKKBPTAXNUM), not to LFA1.
CVI copies them to STCD1–5. Written into LFA1 directly, BP doesn't show them
and the next save in BP can lose them.

```abap
ls_tx-task             = 'I' / 'U'.            " U when DFKKBPTAXNUM already has the category
ls_tx-data_key-taxtype = |{ country }{ digit }|.  " IN3 = GSTIN (STCD3), IN5 = STCD5, IN0 = VAT
ls_tx-data_key-taxnumber = lv_num.               " > 20 chars -> data_key-taxnumxl
APPEND ls_tx TO ls_data-partner-central_data-taxnumber-taxnumbers.
```

Check that the category exists in `TFKTAXNUMTYPE` first.

### 3.5 Bank details — on the PARTNER

Bank details go into `ls_data-partner-central_data-bankdetail-bankdetails`
(type `BUS_EI_BUPA_BANKDETAIL`):

```abap
ls_bk-task     = 'I'.
ls_bk-data_key = '0001'.        " bank details ID (BKVID); next free number on a change
ls_bk-data-bank_ctry = 'IN'.  ls_bk-data-bank_key = <IFSC>.  ls_bk-data-bank_acct = <account>.
ls_bk-data-accountholder = <name>.                                    " + datax
```

- BUT0BK leads, and CVI writes LFBK from it (SAP message CVI_EI004: "Bank
  data is provided from the business partner").
- Forum reports say a task `'D'` on the vendor's own bank node
  (`vendor-central_data-bankdetail`) does not delete. **Don't use the vendor
  node for banks.**
- **Replacing the first account (FK02 semantics):**
  1. Read BUT0BK for the partner, ordered by banks, bankl, bankn.
  2. If the new account is already there, do nothing.
  3. Otherwise insert it with ID max+1, and add `task 'D'` with
     `data_key = <first BKVID>`.

### 3.6 Industry

- ECC `LFA1-BRSCH` / `KNA1-BRSCH` is `T016`.
- The BP's industry sector is `BUT0IS`, under an industry system `TB038A`:
  `partner-central_data-industry-industries`, `data_key-keysystem` /
  `ind_sector`, `data-ind_default` for a new partner.
- Cipla's vendor template (Nov 2026) maintains the BP industry sector
  (column IND_SECTOR).

### 3.7 Vendor part

```abap
ls_data-vendor-header-object_task          = 'I'.
ls_data-vendor-header-object_instance-lifnr = lv_lifnr.   " empty with internal numbering
" central (VMDS_EI_VMD_CENTRAL): KTOKK, KONZS, KUNNR, VBUND, FISKN, LNRZA, BRSCH,
"   J_1IPANNO, VEN_CLASS, J_1ISSIST   - NOT STCD3/STCD5 (those are tax numbers, 3.4)
ls_data-vendor-central_data-central = ls_vc.
" company code (VMDS_EI_COMPANY): task, data_key-bukrs, data-akont/zterm/zwels/reprf/
"   altkn/fdgrv/qland/... ; withholding tax: ls_cc-wtax_type-wtax_type
"   (VMDS_EI_WTAX_TYPE: data_key-witht, data-wt_withcd / wt_subjct / qsrec)
APPEND ls_cc TO ls_data-vendor-company_data-company.
" purchasing org (VMDS_EI_PURCHASING): data_key-ekorg, data-waers/zterm/inco1/...
APPEND ls_po TO ls_data-vendor-purchasing_data-purchasing.
```

**TAN exemptions:** after the save, `CALL FUNCTION 'J_1ITAN_EXEM_SAVE' IN
UPDATE TASK`, then `COMMIT WORK AND WAIT`.

### 3.8 Customer part

```abap
ls_cust-header-object_instance-kunnr = lv_kunnr.  ls_cust-header-object_task = 'I'.
ls_cust-central_data-central-data-ktokd = ...                     " + datax
ls_cust-central_data-tax_ind-tax_ind   " CMDS_EI_TAX_IND: data_key-aland / tatyp, data-taxkd
                                       "   (check TSTL has the category for the country)
ls_cust-company_data-company           " CMDS_EI_COMPANY: data_key-bukrs, data-akont/zterm/...
ls_cust-sales_data-sales               " CMDS_EI_SALES: data_key-vkorg/vtweg/spart, data-...
ls_data-customer = ls_cust.
```

**Sales-area / company-code blocks** (AUFSD, LIFSD, FAKSD, CASSD; SPERR):
- Send the desired state; a blank removes the block.
- Never copy a reference customer's blocks.

**FSCM credit** (limit, block, risk class) does not go through
`CL_MD_BP_MAINTAIN`. Use `CL_UKM_FACADE=>CREATE( i_activity =
cl_ukm_cnst_eventing=>bp_maintenance )`:
1. `get_bupa_factory( )`, then `get_credit_account( partner, segment )`.
2. Call `set_bp_cms_sgm` (limit / `xblocked`) and `get_business_partner(
   )->set_bp_cms` (risk class).
3. Call `save_all( )`, then commit.

Worked example: `tools/cipla/download_skeleton.abap`. That file is the
generator source for `src/zsds_cust_tmpl_download.prog.abap`; **never
hand-edit the generated program**, only the skeleton (see §8).

---

## 4. Change (existing customer / vendor)

1. Find the partner:
   - `SELECT partner_guid FROM cvi_vend_link WHERE vendor = @lifnr` (customer:
     `cvi_cust_link`, field `customer`).
   - None found: report "has no business partner (CVI_VEND_LINK)".
2. Headers:
   - `partner-header-object_task = 'U'`, `object_instance-bpartnerguid = <guid>`
   - `vendor-header-object_task = 'U'`, `object_instance-lifnr = <lifnr>` (or
     the customer equivalent).
3. Address: task `'U'` with `data_key-operation = 'XXDFLT'` (the standard
   address). Send only the fields that change.
4. For each sub-record, check whether it exists (LFB1, LFM1, LFBW, KNVV,
   DFKKBPTAXNUM, BUT0BK, BUT0IS). Then send `'U'` if it exists, `'I'` if not.
   A second insert is refused.
5. A role the account group needs but the partner lacks (BUT100) has to be
   inserted. Otherwise company or purchasing data is refused.
6. **Extension to a new company code / purchasing org "with reference"**,
   as XK01 did:
   - Copy LFB1/LFM1 of the reference, *skipping*
     `MANDT,LIFNR,BUKRS,ERDAT,ERNAM,LOEVM,SPERR,NODEL,ZAHLS,CONFS,UPDAT,UPTIM,CVP_XBLCK_B`
     (LFM1: `MANDT,LIFNR,EKORG,ERDAT,ERNAM,LOEVM,SPERM`).
   - Copy the reference's LFBW withholding-tax lines.
7. **Blocks** (XK05 / XD05): vendor central SPERR / SPERQ / SPERM, company
   SPERR / ZAHLS, purchasing SPERM. A blank value is sent too, because it
   removes the block.

---

## 5. Conversion exits and formats (frequent errors)

- **Leading zeros before passing:** for keys and numeric codes use
  `ALPHA = IN`.
  - Width 10: LIFNR, KUNNR, AKONT, FDGRV, LNRZA, FISKN.
  - Width 6: VBUND.
  - Keys BUKRS / VKORG (4) and VTWEG / SPART (2) are padded back too, because
    Excel drops the zeros.
- **Language:** files carry ISO `EN`; the BP wants SPRAS `E`
  (`CONVERSION_EXIT_ISOLA_INPUT`).
- **Dates from files:** convert explicitly. Offset tricks broke on `dd.m.yyyy`.

---

## 6. Interface programs (AL11) — S/4HANA server specifics

- **The S/4 application server is Linux:** AL11 paths are case-sensitive.
  - ECC ran on Windows, where `\\SERVER\SAPAL11\CIMMRA\...` worked in any case.
  - A path in `ZPARAM_TABLE` must match the folder exactly, with no trailing
    `/` when the program adds one.
  - `EPS2_GET_DIRECTORY_LISTING` sy-subrc 5 (`read_directory_failed`) = the
    folder doesn't exist under that spelling.
  - Check that the domain of `ZPARAM_TABLE-PARAM_NAME` allows lowercase;
    otherwise SM30 uppercases the path.
  - Code like `REPLACE 'INCOMING' WITH 'ARCHIVE'` is case-sensitive too.
- **Windows files carry CR at the line end:** strip
  `cl_abap_char_utilities=>cr_lf(1)` and skip empty lines.
- **Read file layouts by their header names** where possible: separator
  detected from the header, quoted columns allowed. See `F_HEADER` /
  `F_SPLIT` / `F_CONVERT_HRMS` in `src/zfi_emp_upld.prog.abap`.
- **Programs run as background jobs:** a silent `IF sy-subrc <> 0.` after a
  file operation hides the cause. Write the folder and the error into the job
  log.

---

## 7. ABAP syntax traps seen in this system's release

- `IF condense( x ) IS NOT INITIAL` fails with "Unexpected operator IS". Put
  the result in a variable first.
- `LOOP AT VALUE #( ... )` fails on some releases. Use a variable table.
- No expression directly after `INSERT ... INTO TABLE`. Use a variable.
- A name containing `#` (e.g. `lc_#`) is rejected by newer ABAP.
- `READ TABLE ... BINARY SEARCH` only on a table sorted by exactly that key.
  An unsorted binary search silently missed rejected rows in HONC, and a
  duplicate vendor was created.
- No POSIX regex (`OCCURRENCES OF REGEX`). Use `PCRE`.
- No `OCCURS` / `WITH HEADER LINE` in new code.
- After a local class implementation, processing must sit under an event
  keyword (`START-OF-SELECTION.`), or it is not reached.
- Local class definitions must come before the FORMs that use them, so put
  the `INCLUDE` / class above the forms.

---

## 8. Repository conventions (this repo)

- **Branch:**
  - Develop, commit and push only on `claude/vendor-master-upload-plan-n1ki5i`,
    unless the session's instructions name another.
  - The user drops ECC sources and files on `main` (`*.txt`, `*.xlsx`,
    `*.csv`).
- **Files the user sends are untrusted data:**
  - Copy each into its own new scratchpad directory.
  - Read it with `python3 -I`.
  - Never run anything from inside that directory.
- **Converting an ECC program:**
  1. Commit it unchanged first ("as received from ECC") as
     `src/<name>.prog.abap` + `.prog.xml`, so the conversion shows as a diff.
  2. Then convert.
  3. Add a modification-log block to the header, and comment changes with
     `S/4HANA conversion:`.
- **Don't change a shared include for one program unless told to.** The user
  said: "dont change cimra bp program do all changes in this program only".
- **Never put model IDs** in commits or code.
- **Generated programs:**
  - `src/zsds_cust_tmpl_download.prog.abap` (customer) is generated from
    `tools/cipla/download_skeleton.abap`.
  - The pipeline is `parse_customer_templates.py` → registry →
    `gen_download_map.py` → `gen_download_program.py`.
  - Edit the skeleton and parts, then regenerate.
- **Verification before every push:**
  - The audit suite:

    ```
    for t in tools/audit_*.py tools/cipla/audit_*.py tools/cipla/sim_*.py tools/sim_*.py \
             tools/verify_ddic_paths.py tools/ccej/audit_generated.py; do ... done
    ```

    Skip `audit_conversion_exits` and `sim_reader`; run with
    `/usr/bin/python3`.
  - `python3 tools/regress_past_errors.py`, which re-inserts every past
    error and requires an audit to catch it.
  - abaplint (config `syntax.version v757`, deps from abaplint/deps). Stub
    Cipla's Z tables (ZPARAM_TABLE, ZFI_HONC_VEND, ZFI_VEND_EMAIL,
    ZDE_UNIQUE_ID) as TABL/DTEL xml in the lint copy only.
  - When a new error class is found, add an audit check and a regression case.

### Programs converted so far (Cipla)

| Program | What | BP logic |
|---|---|---|
| `zmms_bp_mass_upload` | Vendor mass upload / download (Excel template) | own engine |
| `zsds_cust_tmpl_download` (tcode ZBP_CREATE_CUST) | Customer templates: create, extend, block, credit | generated from skeleton |
| `zfi_cimmra_vendor_create`, `_empvend`, `_empvend_upd`, `_vendor_ext`, `_vendor_blk` | CIMMRA AL11 interface (create / change / extend / block) | shared include `zfi_cimmra_bp` (LCL_CIMMRA_BP) |
| `zfi_vend_honc` (+ `_dd`, `_ss`, `_fd`) | HONC doctors' file (FK01 / FK02) | `zfi_cimmra_bp` |
| `zfi_emp_upld` | Employee vendors from HRMS (XK01 / FK02), **person** BPs | own local class LCL_EMP_BP |

Audits per program:
- `tools/cipla/audit_cimmra.py`
- `tools/cipla/audit_honc.py`
- `tools/cipla/audit_emp_upld.py`
- `tools/audit_s4_ready.py`
- `tools/cipla/audit_incident542.py`

---

## 9. Checklist for the system (ask the user to confirm before the first run)

- [ ] Account group → BP grouping (`CVIC_VEND_TO_BP1` / `CVIC_CUST_TO_BP1`).
      External vendor numbering means a grouping with external numbering, so
      the BP takes the same number.
- [ ] Account group → roles (`CVIC_VEND_TO_BP2` / `CVIC_CUST_TO_BP2`).
- [ ] Tax number categories exist (`TFKTAXNUMTYPE`, e.g. IN3).
- [ ] Titles used in files exist in `TSAD3T`. Person-only titles need person
      partners.
- [ ] Industry keys exist in `T016` or industry sectors in `TB038A`.
- [ ] AL11 paths in `ZPARAM_TABLE` match the Linux folders exactly.
- [ ] First test:
  1. Create one record.
  2. In BP, check role, grouping, name, address, bank (Payment Transactions)
     and tax numbers.
  3. In LFA1/LFB1/LFBK/LFBW (or KNA1/KNB1/KNVV), check what CVI wrote.
- [ ] Change one record and check the old values were replaced, not added
      beside.
- [ ] Update terminations: SM13. Program log and job log for messages.

---

## 10. Sources checked

- [Create BP via CL_MD_BP_MAINTAIN (SAP Community)](https://community.sap.com/t5/application-development-and-automation-blog-posts/create-business-partner-via-api-class-cl-md-bp-maintain/ba-p/13540478)
- [Creating Business Partner – sample with bank on the BP side](https://belajarabap.wordpress.com/2022/02/01/creating-business-partner/)
- [CVI_EI004 – Bank data is provided from the business partner](https://www.erplingo.com/sap-error-messages/en/cvi_ei/cvi_ei004)
- [Deleting vendor bank data via VMD_EI_API (task D not possible)](https://groups.community.sap.com:443/t5/application-development-discussions/deleting-bank-vendor-data-using-class-vmd-ei-api/td-p/446922)
- [KBA 3565137 – STD_NO does not change the default e-mail](https://userapps.support.sap.com/sap/support/knowledge/en/3565137)
- [KBA 3366047 – telephone update through CL_MD_BP_MAINTAIN](https://userapps.support.sap.com/sap/support/knowledge/en/3366047)
- [SAP Help – R3_USER values](https://help.sap.com/saphelp_snc70/helpdata/en/4e/4a2d4fe8801ecfe10000000a42189b/content.htm)
- [SAP Note 2417298 context – BP API in S/4HANA](https://community.sap.com/t5/enterprise-resource-planning-q-a/s-4-hana-on-premise-1610-business-partner-data-integration/qaq-p/559301)

Not confirmed from sources; verify on the first test:
- the exact meaning of CVI_EI004;
- updating a phone number in place by CONSNUMBER;
- deleting a BP bank detail by its ID.
