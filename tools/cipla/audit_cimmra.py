"""Cipla's CIMMRA vendor interface posts suppliers as business partners.

ECC: ZFI_CIMMRA_VENDOR_CREATE, ZFI_CIMMRA_EMPVEND and ZFI_CIMMRA_EMPVEND_UPD
called RFCs ZMM_RFC_VENDOR_CREATE / ZMM_RFC_VENDOR_CHANGE, which maintained the
vendor on its own. S/4HANA (November 2026): include ZFI_CIMMRA_BP posts
through CL_MD_BP_MAINTAIN. This keeps it that way:

  - no report calls the ECC RFCs any more, each includes ZFI_CIMMRA_BP and
    starts its processing under START-OF-SELECTION (after a class
    implementation, statements outside an event block are not reached);
  - the include validates, maintains and commits through the BP API and
    reads the commit's verdict;
  - the partner gets its grouping and the supplier roles from the account
    group, and GSTIN / tax number 5 as its own tax numbers - never as LFA1
    fields, where BP does not show them;
  - R_3_USER only ever gets a code SAP accepts (MESSAGE_TYPE_X otherwise).

    python3 tools/cipla/audit_cimmra.py
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
INC = open(os.path.join(ROOT, 'src/zfi_cimmra_bp.prog.abap'), encoding='utf-8').read()
REPORTS = ['zfi_cimmra_vendor_create', 'zfi_cimmra_empvend', 'zfi_cimmra_empvend_upd',
           'zfi_cimmra_vendor_ext', 'zfi_cimmra_vendor_blk']


def code(text):
    return '\n'.join(re.sub(r'".*$', '', l) for l in text.split('\n') if not l.lstrip().startswith('*'))


fail = []
for r in REPORTS:
    src = code(open(os.path.join(ROOT, f'src/{r}.prog.abap'), encoding='utf-8').read())
    if re.search(r"CALL FUNCTION 'ZMM_RFC_VENDOR_(CREATE|CHANGE)'", src, re.I):
        fail.append(f'{r}: still calls the ECC vendor RFC - in S/4HANA the vendor is a business partner')
    if re.search(r"CALL TRANSACTION|\bBDCDATA\b", src, re.I):
        fail.append(f'{r}: still drives a transaction by batch input - XK01/XK02/XK05 are replaced by BP')
    if re.search(r"vmd_ei_api=>maintain", src, re.I):
        fail.append(f'{r}: maintains the vendor through VMD_EI_API, past its business partner')
    if not re.search(r'^INCLUDE zfi_cimmra_bp\.\s*\n\s*\nSTART-OF-SELECTION\.\s*\nPERFORM', src, re.I | re.M):
        fail.append(f'{r}: does not include ZFI_CIMMRA_BP before START-OF-SELECTION and its PERFORMs')
    if not re.search(r'lcl_cimmra_bp=>(create|change|extend|block)\(', src):
        fail.append(f'{r}: never posts through LCL_CIMMRA_BP')

inc = code(INC)
for what, pat in (
        ('validates before posting', r'cl_md_bp_maintain=>validate_single\('),
        ('posts through the BP API', r'cl_md_bp_maintain=>maintain\('),
        ('reads the commit\'s verdict', r"CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'\s*\n\s*EXPORTING wait\s*=\s*abap_true\s*\n\s*IMPORTING return = ls_cret\."),
        ('takes the BP grouping from the account group', r'FROM cvic_vend_to_bp1'),
        ('gives the partner the supplier roles', r'FROM cvic_vend_to_bp2'),
        ('files GSTIN as a partner tax number', r'partner-central_data-taxnumber-taxnumbers'),
        ('keeps STCD3 / STCD5 out of LFA1', r"iv_skip = COND string\(.*\n.*`STCD3,STCD5,KTOKK`\s*\n\s*ELSE `STCD3,STCD5`"),
        ('accepts only R_3_USER codes SAP knows', r"IF lv_r3 CA '123'\.\s*\n\s*ls_tel-contact-data-r_3_user = lv_r3\."),
        ('changes the standard address of an existing partner', r"ls_adr-data_key-operation = 'XXDFLT'"),
        ('extends without copying blocks or deletion flags',
         r"iv_skip = `MANDT,LIFNR,BUKRS,ERDAT,ERNAM,LOEVM,SPERR,NODEL,ZAHLS,`"),
        ('adds the supplier roles an extension needs', r"FROM but100\s*\n\s*WHERE partner = @lv_bp AND rltyp"),
        ('blocks with a blank cell removing the block, as XK05 did',
         r"set\( EXPORTING iv_comp = 'SPERR' iv_value = to_upper\( condense\( CONV string\( iv_sperr \) \) \)")):
    if not re.search(pat, inc, re.I):
        fail.append(f'ZFI_CIMMRA_BP no longer {what}')

# QA 28.11.2026, file CIM_VEND_CRT06112026QA1199: CIMMRA's placeholder for
# "no exemption" (1111111111, rate 0, 11-11-1900 to 12-11-1900) was sent and
# the vendor refused - "Enter a valid date interval in line EXEMPT".
_vc = open(os.path.join(ROOT, 'src/zfi_cimmra_vendor_create.prog.abap'), encoding='utf-8').read()
if not re.search(r"IF lv_exrt > 0\.\s*\n\s*ls_with_holding_tax-wt_exnr = wa_al11-exemptionnumber\.", _vc):
    fail.append('ZFI_CIMMRA_VENDOR_CREATE sends a withholding tax exemption without a rate - CIMMRA\'s '
                'placeholder 11-11-1900 to 12-11-1900 is refused: "Enter a valid date interval in line EXEMPT"')
# TO_DATE read "20112026" (20.11.2026) as the year 2011: every day from the
# 19th to the 29th without separators.
if 'plausible( lv_from_dmy ) = abap_true THEN lv_from_dmy' not in inc:
    fail.append('ZFI_CIMMRA_BP TO_DATE takes DDMMYYYY of the 19th to the 29th for a year')
if not re.search(r'IF strlen\( lv \) = 8 AND lv CO \'0123456789\' AND plausible\( lv \) = abap_true\.', inc):
    fail.append('ZFI_CIMMRA_BP TO_DATE hands back dates that are not calendar dates')

if fail:
    print(f'{len(fail)} problem(s):')
    for f in fail:
        print('  -', f)
    sys.exit(1)
print(f'clean - {len(REPORTS)} CIMMRA reports post through ZFI_CIMMRA_BP and CL_MD_BP_MAINTAIN, '
      f'with grouping, roles and tax numbers on the business partner')
