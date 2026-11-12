"""ZSDS_CUST_CREDIT_UPLOAD keeps clear of the faults of its predecessor.

The credit tab of the retired ZSDS_CUST_MASS_UPLOAD (R5) had three, found
against Cipla's real file "1Customer change Credit limit.xlsb":

  1. the partner was CONV bu_partner( kunnr ) - the customer number taken
     for the business partner number, which holds only where the grouping
     gives both the same number;
  2. KLIMG, which holds X on every row of the file, was read as an amount -
     X becomes 0 and the main segment's limit would be set to zero;
  3. NO CHANGE in payment terms, customer group 3 and interest was sent to
     SAP as a value.

And one SAP behaviour: where a credit limit change needs approval, SAP keeps
the old limit - a run that does not read the limit back reports a change
that did not happen.

    python3 tools/audit_credit_upload.py
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC = open(os.path.join(ROOT, 'src/zsds_cust_credit_upload.prog.abap'), encoding='utf-8').read()
fail = []

if re.search(r'CONV\s+bu_partner\(\s*iv_kunnr\s*\)', SRC, re.I):
    fail.append('the customer number is used as the business partner number - read the partner '
                'from CVI_CUST_LINK')
if 'cvi_cust_link' not in SRC.lower():
    fail.append('the business partner is not read from CVI_CUST_LINK')
m = re.search(r"lcl_util=>to_dec\( EXPORTING iv_in = lv_klimg IMPORTING ev_dec = lv_tot ev_ok = lv_okt \)"
              r"[\s\S]*?IF lv_okt = abap_true\.\s*\n\s*credit\(", SRC)
if not m:
    fail.append('KLIMG is not checked for being an amount before it is written to the main segment '
                '(the file holds X there)')
for v in ('lv_zterm', 'lv_kvgr3', 'lv_int'):
    if not re.search(rf'IF lcl_util=>no_change\( {v} \) = abap_true\.\s*\n\s*CLEAR {v}\.', SRC):
        fail.append(f'{v}: NO CHANGE is not recognised - it would be sent to SAP as a value')
if 'stored_limit( EXPORTING' not in SRC or SRC.count('stored_limit( EXPORTING') < 2:
    fail.append('the limit is not read back after the save - a limit kept back for approval would '
                'be reported as changed')

if fail:
    print(f'{len(fail)} problem(s):')
    for f in fail:
        print('  -', f)
    sys.exit(1)
print('clean - the partner comes from the CVI link, KLIMG must be an amount, NO CHANGE changes '
      'nothing, and the limit is read back after the save')
