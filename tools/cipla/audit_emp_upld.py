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
need('the bank change does not replace the first account, as FK02 did',
     r'ls_del-task\s*= c_delete\.\s*\n\s*ls_del-data_key = lt_old\[ 1 \]-bkvid\.')
need('XK01 is not replaced by LCL_EMP_BP=>CREATE', r'lcl_emp_bp=>create\(')
need('FK02 is not replaced by LCL_EMP_BP=>CHANGE_BANK', r'lcl_emp_bp=>change_bank\(')

# the HRMS layout
need('the header line does not decide the layout', r'PERFORM f_header USING lwa_line\.')
need('the HRMS columns are not found by their headings',
     r"WHEN 'EMPLOYEECODE' OR 'EMPCODE'\..*WHEN 'BANKACCTNO' OR 'BANKACCNO'\..*WHEN 'BANKIFSC'\.")
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
