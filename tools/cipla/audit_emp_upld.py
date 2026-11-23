"""Cipla's employee vendor upload (ZFI_EMP_UPLD) creates employees as business partners.

ECC: the report read HRMS's active employee file from AL11 and created the
employee vendor with an XK01 batch input, changed its bank with FK02. Both
transactions are gone in S/4HANA. Since November 2026 the report posts through
its own local class LCL_EMP_BP (CL_MD_BP_MAINTAIN) - ZFI_CIMMRA_BP is not used
and not changed for it. This keeps it that way:

  - no batch input is left, and the report does not include ZFI_CIMMRA_BP;
  - the employee is a PERSON (BP category 1) with first / middle / last name,
    never an organisation;
  - grouping and supplier roles come from the account group, bank details go
    on the business partner, the save is validated, maintained and committed
    with its verdict read;
  - the HRMS layout is read by its column headings and gives the vendor the
    values of sheet "expected output file for upload" of
    "Employee vendor creation - mapping HRMS.xlsx";
  - every file's header is recognised, not only the first file's.

    python3 tools/cipla/audit_emp_upld.py
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
RAW = open(os.path.join(ROOT, 'src/zfi_emp_upld.prog.abap'), encoding='utf-8').read()
SRC = '\n'.join(re.sub(r'".*$', '', l) for l in RAW.split('\n') if not l.lstrip().startswith('*'))

fail = []


def need(what, pat, flags=re.I | re.S):
    if not re.search(pat, SRC, flags):
        fail.append(what)


if re.search(r'CALL TRANSACTION|\bBDCDATA\b|\bBDCMSGCOLL\b|\bbdc_(field|dynpro)\b|\bf_bdc_', SRC, re.I):
    fail.append('ZFI_EMP_UPLD still drives XK01 / FK02 by batch input - gone in S/4HANA')
if re.search(r'^\s*INCLUDE\s+zfi_cimmra_bp', SRC, re.I | re.M) or 'lcl_cimmra_bp' in SRC.lower():
    fail.append('ZFI_EMP_UPLD uses ZFI_CIMMRA_BP - the employee upload has its own BP logic (LCL_EMP_BP)')

need('the employee is not created as a person (BP category 1)',
     r"bp_control-category = '1'\.")
if re.search(r"bp_control-category = '2'", SRC):
    fail.append('an employee is created as an organisation (BP category 2)')
need('the person gets no first / last name',
     r"iv_comp = 'FIRSTNAME'.*iv_comp = 'LASTNAME'")
need('the partner\'s grouping does not come from the account group', r'FROM cvic_vend_to_bp1')
need('the partner gets no supplier roles from the account group', r'FROM cvic_vend_to_bp2')
need('bank details are not put on the business partner',
     r'APPEND ls_bk TO ls_data-partner-central_data-bankdetail-bankdetails\.')
if re.search(r'vendor-central_data-bankdetail', SRC, re.I):
    fail.append('bank details are sent on the supplier - CVI takes them from the business partner')
need('the save is not validated first', r'cl_md_bp_maintain=>validate_single\(')
need('the save does not go through the BP API', r'cl_md_bp_maintain=>maintain\(')
need('the commit\'s verdict is not read',
     r"CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'\s*\n\s*EXPORTING wait\s*=\s*abap_true\s*\n\s*IMPORTING return = ls_cret\.")
need('the bank change does not overwrite the currently valid bank details ID in place, as FK02 typed over line 1',
     r'ls_bk-task\s*= c_update\.\s*\n\s*ls_bk-data_key = lv_bkvid\.')
need('lines that are no longer valid are not left out - an ended bank line was taken as line 1',
     r'DELETE lt_old WHERE \( bk_valid_from IS NOT INITIAL AND bk_valid_from > lv_now \)\s*\n\s*'
     r'OR \( bk_valid_to\s+IS NOT INITIAL AND bk_valid_to\s+< lv_now \)\.')
need('the bank change is not sent as time-dependent for the line\'s own validity - SAP applied it as of today, '
     'ended the line and opened a second ID (BP 60451, CRQ, 23.11.2026)',
     r"bankdetail-time_dependent = abap_true\..*iv_comp = 'BANKDETAILVALIDFROM'.*iv_comp = 'BANKDETAILVALIDTO'")
need('the bank change is not read back - a second bank details ID would go unreported',
     r"NOT line_exists\( lt_new\[ bkvid = lv_bkvid banks = lv_banks bankl = lv_bankl bankn = lv_bankn \] \)")
need('a vendor that is already created is created again instead of getting its bank account and IFSC '
     'updated - the save was refused with "Business partner 123518 already exists" (CRQ, 23.11.2026)',
     r"SELECT SINGLE lifnr, land1 FROM lfa1 WHERE lifnr = @iv_lifnr INTO @DATA\(ls_is_vendor\)\.\s*\n\s*"
     r"IF ls_is_vendor-lifnr IS NOT INITIAL\.\s*\n\s*ev_existed = abap_true\..*?change_bank\(.*?RETURN\.")
need('an existing vendor whose bank account was updated is not logged as Modified',
     r"IF lv_existed = abap_true\.\s*\n(?:\s*\".*\n)*\s*PERFORM f_bp_log USING co_update lt_error lt_ok CHANGING ta_msg_update\.")
if re.search(r'METHOD extend\.', SRC, re.I):
    fail.append('an existing vendor is added to the company code - the rule is: update its bank account and IFSC only')
need('a business partner number that is already taken is not explained (whose it is)',
     r"SELECT SINGLE partner FROM but000 WHERE partner = @lv_bpno INTO @DATA\(lv_taken\)\..*who_is\( lv_bpno \)")
need('a business partner saved without a supplier is not diagnosed - the log must show the system\'s customer / '
     'vendor integration settings (production, BP 141883: supplier created in quality, not in production)',
     r"gv_cvi_checked = abap_true\.\s*\n\s*cvi_check\(.*METHOD cvi_check\..*MDSC_CTRL_OPT_A.*TBC002.*TB003.*TBC001.*MDS_PPO2")
need('the run\'s log is not printed in the job spool - when the mail does not go out (SOST 826) nobody sees '
     'why no vendor was created (CRQ, 23.11.2026)',
     r'PERFORM f_read_n_run\.\s*\n\s*PERFORM f_write_log\.')
need('the log is copied into the mail table on every file row instead of once after the loop',
     r"ENDLOOP\.\s*\n(?:\s*\n|\s*\".*\n)*\s*APPEND LINES OF ta_msg TO ta_msg_new\.")
need('the mail\'s serial number is a one-character field - "*" at 10 and CONVT_NO_NUMBER at 11 (CRQ, 23.11.2026)',
     r'lv_srno\s+TYPE i,')
need('the payment method is not CNT for the 50-column file - it sends only C (CRQ, 23.11.2026)',
     r'cwa_file-zwels\s+= co_hrms_zwels\.(?:(?!ENDFORM).)*ENDFORM\.(?:(?!FORM f_convert_hrms).)*FORM f_convert_hrms')
if re.search(r'task\s*= c_delete', SRC):
    fail.append('a bank detail is deleted - with time-dependent bank details the old account stays and the '
                'partner shows two (HONC, CRS, 20.11.2026); overwrite line 1 instead')
need('what the validation left in the buffers is not discarded before posting (BAPI_TRANSACTION_ROLLBACK)',
     r"ENDTRY\.\s*\n(?:\s*\n)*\s*discard\( \)\.")
need('DISCARD does not roll back through BAPI_TRANSACTION_ROLLBACK (which refreshes the registered buffers)',
     r"METHOD discard\.\s*\n(?:\s*\n)*\s*CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'\.")
need('XK01 is not replaced by LCL_EMP_BP=>CREATE', r'lcl_emp_bp=>create\(')
need('FK02 is not replaced by LCL_EMP_BP=>CHANGE_BANK', r'lcl_emp_bp=>change_bank\(')

# the HRMS layout
need('the header line does not decide the layout', r'PERFORM f_header USING lwa_line\.')
need('the HRMS columns are not found by their headings',
     r"WHEN 'EMPLOYEECODE' OR 'EMPCODE'\..*WHEN 'BANKACCTNO' OR 'BANKACCNO'\..*WHEN 'BANKIFSC'\.")
need('the 50-column file is taken for the HRMS layout - its template header line also says Emp_Code '
     '("Emp_Code#1000##Z008#Salutation#fname mname lname#..."); the file was refused for a missing name '
     'column and no employee vendor was created (CRQ, 23.11.2026)',
     r"IF gs_col-emp IS INITIAL\s*\n\s*OR lines\( lt_col \) >= 40\s*\n\s*"
     r"OR \( gs_col-name IS INITIAL AND gs_col-lname IS INITIAL \)\.")
need('employee codes with leading zeros (01047) are not found among the DPR / tax invoice vendors, which '
     'have none',
     r"lv_key = wa_file-lifnr\.\s*\n\s*SHIFT lv_key LEFT DELETING LEADING '0'\..*"
     r"READ TABLE ta_create INTO wa_create WITH KEY lifnr = lv_key BINARY SEARCH\..*"
     r"READ TABLE ta_update INTO wa_update WITH KEY lifnr = lv_key BINARY SEARCH\.")
need('only the first file\'s header is recognised - the flag is not reset per file',
     r"ta_msg, ta_file\.\s*\n\s*lv_header_skip = '0'\.")
for what, pat in (('company code 1000', r"co_hrms_bukrs TYPE bukrs\s+VALUE '1000'"),
                  ('account group Z008', r"co_hrms_ktokk TYPE ktokk\s+VALUE 'Z008'"),
                  ('country IN', r"co_hrms_land\s+TYPE land1\s+VALUE 'IN'"),
                  ('language EN', r"co_hrms_langu TYPE c LENGTH 2 VALUE 'EN'"),
                  ('reconciliation account 1200013', r"co_hrms_akont TYPE akont\s+VALUE '1200013'"),
                  ('payment method CNT', r"co_hrms_zwels TYPE dzwels\s+VALUE 'CNT'"),
                  ('previous account number = employee code', r'cwa_file-altkn\s*= cwa_file-lifnr\.'),
                  ('double-invoice check', r'cwa_file-reprf\s*= abap_true\.'),
                  ('account holder = employee name', r'cwa_file-koinh\s*= cwa_file-name1\.'),
                  ('search term = employee name', r'cwa_file-sort1\s*= cwa_file-name1\.'),
                  ('mobile into the telephone (LFA1-TELF1)',
                   r'gs_col-mob\s+CHANGING cwa_file-tel_number\.'),
                  ('function into the corporate group (LFA1-KONZS)',
                   r'gs_col-func\s+CHANGING cwa_file-konzs\.')):
    if not re.search(pat, SRC):
        fail.append(f'the HRMS layout no longer gives the vendor {what} (mapping sheet 2)')

if fail:
    print(f'{len(fail)} problem(s):')
    for f in fail:
        print('  -', f)
    sys.exit(1)
print('clean - ZFI_EMP_UPLD creates employee vendors as person business partners through its own '
      'LCL_EMP_BP, from the HRMS layout of the mapping')
