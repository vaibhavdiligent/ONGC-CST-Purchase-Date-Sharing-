"""Cipla's HONC vendor interface creates and changes suppliers as business partners.

ECC: ZFI_VEND_HONC (includes _DD, _SS, _FD) read the HONC file from AL11 and
created the vendor with an FK01 batch input, changed it with FK02. Both
transactions are gone in S/4HANA. Since November 2026 the program posts
through its own local class LCL_HONC_BP (CL_MD_BP_MAINTAIN), kept apart from
the CIMMRA interface's ZFI_CIMMRA_BP. This keeps
it that way:

  - no batch input is left, ZFI_CIMMRA_BP is not included, and LCL_HONC_BP
    comes before the forms that
    call it (a local class is known only below its definition);
  - create and change go through LCL_CIMMRA_BP, with what FK01 / FK02
    entered: account group Z001, company code 1000, mobile as R_3_USER 3,
    the GST classification sent even when blank (blank = registered), the
    file's bank account replacing the first one on a change;
  - the file layout the program splits is the one HONC sends (the header of
    SMC_Vendor_18112026_120203.csv);
  - a rejected record is found by PAN with a plain READ - TA_DATA1 is not
    sorted by PAN, and a binary search let a rejected new vendor through.

    python3 tools/cipla/audit_honc.py
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def read(name):
    return open(os.path.join(ROOT, 'src', name), encoding='utf-8').read()


def code(text):
    return '\n'.join(re.sub(r'".*$', '', l) for l in text.split('\n') if not l.lstrip().startswith('*'))


MAIN = code(read('zfi_vend_honc.prog.abap'))
DD = code(read('zfi_vend_honc_dd.prog.abap'))
FD = code(read('zfi_vend_honc_fd.prog.abap'))
INC = MAIN                     # LCL_HONC_BP lives in the main program

# The HONC file, column by column, and the TY_VENDOR field each goes into.
CSV_HEADER = ('Title|Vendorcode|Vendorname|VendorSearchName|VendorAddress1|VendorAddress2|'
              'VendorAddress3|Postal Code|City|Country|Region|Telephone|Ext1|Telephone2|Ext2|'
              'Mobile Phone|Mobile Phone 2|Fax|Email ID 1|Bank Country|Bank IFSC Code|'
              'Vendor Bank Account No|Account Holder Name|Currency|PAN No|GST|'
              'Vendor Classification|Unique Transaction ID|Flag|Industry Key')
FIELDS = ['title', 'vendor', 'name1', 'sort1', 'street', 'str_suppl1', 'str_suppl2', 'post_code1',
          'city1', 'country', 'region', 'tel_number1', 'tel_extens1', 'tel_number2', 'tel_extens2',
          'mob_number1', 'mob_number2', 'fax_number', 'smtp_addr', 'banks', 'bankl', 'bankn', 'koinh',
          'curren', 'pan', 'gst', 'gstvenclass', 'unique_id', 'flag', 'ind_key']

fail = []

for name, src in (('ZFI_VEND_HONC', MAIN), ('ZFI_VEND_HONC_DD', DD), ('ZFI_VEND_HONC_FD', FD)):
    if re.search(r'CALL TRANSACTION|\bBDCDATA\b|\bBDCMSGCOLL\b|\bbdc_(field|dynpro)1\b', src, re.I):
        fail.append(f'{name}: still drives FK01 / FK02 by batch input - gone in S/4HANA, the '
                    f'vendor is a business partner')
    if re.search(r'vmd_ei_api=>maintain', src, re.I):
        fail.append(f'{name}: maintains the vendor through VMD_EI_API, past its business partner')

if re.search(r'^\s*INCLUDE\s+zfi_cimmra_bp', MAIN + DD + FD, re.I | re.M) or \
   re.search(r'lcl_cimmra_bp', MAIN + DD + FD, re.I):
    fail.append('ZFI_VEND_HONC uses ZFI_CIMMRA_BP - HONC has its own class LCL_HONC_BP, the CIMMRA '
                'include is kept apart')
if not re.search(r'^CLASS lcl_honc_bp IMPLEMENTATION\..*^INCLUDE zfi_vend_honc_fd\.', MAIN, re.I | re.M | re.S):
    fail.append('ZFI_VEND_HONC: LCL_HONC_BP is not defined before INCLUDE ZFI_VEND_HONC_FD - the '
                'forms would not know it')
if not re.search(r'^START-OF-SELECTION\.', MAIN, re.I | re.M):
    fail.append('ZFI_VEND_HONC: no START-OF-SELECTION - after a class implementation the '
                'processing would not be reached')


def form(src, name):
    m = re.search(rf'^FORM {name}\b.*?^ENDFORM\.', src, re.I | re.M | re.S)
    return m.group(0) if m else ''


create, change, bp_data = form(FD, 'create_vendor'), form(FD, 'change_vendor'), form(FD, 'bp_data')
if 'lcl_honc_bp=>create(' not in create:
    fail.append('CREATE_VENDOR does not create the vendor through LCL_HONC_BP=>CREATE')
if 'lcl_honc_bp=>change(' not in change:
    fail.append('CHANGE_VENDOR does not change the vendor through LCL_HONC_BP=>CHANGE')
if not re.search(r'iv_bank_replace\s*=\s*abap_true', change):
    fail.append('CHANGE_VENDOR adds the file\'s bank account next to the old one - FK02 replaced '
                'the first account')
if not re.search(r'iv_comm_replace\s*=\s*abap_true', change):
    fail.append('CHANGE_VENDOR adds the file\'s mobile and e-mail next to the old ones - FK02 '
                'overwrote the standard ones')
if re.search(r'vendor-central_data-bankdetail', INC, re.I):
    fail.append('LCL_HONC_BP sends bank details on the supplier - in S/4HANA CVI takes them from '
                'the business partner (CVI_EI004)')
if not re.search(r'iv_keep_blank\s*=\s*`VEN_CLASS`', change):
    fail.append('CHANGE_VENDOR does not send a blank GST classification - a vendor that registers '
                'for GST would stay "not registered"')
if not re.search(r'iv_industry\s*=\s*wa_data-ind_key', create) or \
   not re.search(r'iv_industry\s*=\s*wa_data-ind_key', change):
    fail.append('the industry key of the file is not passed on create and change')
for what, pat in (('account group Z001', r"cs_central-data-ktokk = 'Z001'\."),
                  ('company code 1000', r"cs_comp-data_key-bukrs = '1000'\."),
                  ('the mobile as R_3_USER 3', r"telephone = is_data-mob_number1 r_3_user = '3'"),
                  ('the reconciliation account of ZFI_VEND_G', r'cs_comp-data-akont\s*= gv_akont\.'),
                  ('withholding tax type and code', r'witht\s*= gv_witht\s*\n\s*wt_withcd = gv_withcd'),
                  ('the PAN', r'cs_cin-j_1ipanno = is_data-pan\.'),
                  ('the GSTIN', r'cs_central-data-stcd3 = is_data-gst\.')):
    if not re.search(pat, bp_data):
        fail.append(f'BP_DATA no longer gives the vendor {what}, as FK01 did')

m = re.search(r'SPLIT wa_records-rec AT \'\|\' INTO(.*?)\.', FD, re.S)
split = re.findall(r'wa_data-(\w+)', m.group(1)) if m else []
if split != FIELDS:
    fail.append(f'GET_FILE_DATA splits the HONC file into {split} - the file is {CSV_HEADER}')
if len(CSV_HEADER.split('|')) != len(FIELDS):
    fail.append('the audit\'s own HONC layout is inconsistent')

if not re.search(r"IF wa_data-flag <> 'A' AND wa_data-flag <> 'U'\.", FD):
    fail.append('VALIDATE_DATA skips a row whose flag is neither A nor U without a word - the run then '
                'only mails an empty list')
if not re.search(r"IF ta_data_tmp IS INITIAL\..*no mail sent.*ELSE\.\s*\n\s*PERFORM send_det_mail\.", MAIN, re.S):
    fail.append('ZFI_VEND_HONC mails the users even for a file without vendor rows, and logs nothing')
if re.search(r'READ TABLE ta_data1 INTO wa_data1 WITH KEY pan = wa_data-pan BINARY SEARCH', FD, re.I):
    fail.append('VALIDATE_DATA looks up rejected records by PAN with BINARY SEARCH on TA_DATA1, '
                'which is sorted by bank key - rejected new vendors slip through and are created')

for what, pat in (
        ('places the industry key by T016 and TB038A',
         r'FROM t016 WHERE brsch = @lv_brsch.*FROM tb038a WHERE ind_sector = @lv_sec'),
        ('sends the fields asked for even when blank', r'ASSIGN COMPONENT lv_kf OF STRUCTURE is_cin'),
        ('overwrites the first bank details ID in place on request, as FK02 typed over line 1 - an '
         'insert plus delete left both accounts on the partner',
         r'ls_bk-task\s*= c_update\.\s*\n\s*ls_bk-data_key = lt_bk_old\[ 1 \]-bkvid\.'),
        ('sends bank details on the business partner, where CVI takes them from',
         r'APPEND ls_bk TO ls_data-partner-central_data-bankdetail-bankdetails\.'),
        ('overwrites the standard telephone / fax / e-mail by its CONSNUMBER on request',
         r'ls_tel-contact-data-consnumber = ls_o2-consnumber\..*ls_fax-contact-data-consnumber = '
         r'ls_o3-consnumber\..*ls_smt-contact-data-consnumber = ls_o6-consnumber\.'),
        ('leaves the name of a person partner alone', r"IF lv_bptype = '1'\."),
        ('sends on a change only supplier fields that differ from LFA1 - any supplier field makes CVI '
         're-derive the industry (CVI_MAPPING 002 "Industry 92")',
         r"SELECT SINGLE \* FROM lfa1 WHERE lifnr = @lv_lifnr INTO @DATA\(ls_lfa1_now\)\..*"
         r"get_included_view\( \).*IF <lv_vx> = abap_true AND <lv_vd> = <lv_vo>\.\s*\n\s*CLEAR: <lv_vd>, <lv_vx>\."),
        ('saves a change in two steps - bank, address, tax first, the supplier fields on their own after '
         'it - so a CVI_MAPPING 002 refusal loses only those',
         r"ls_vc_later = ls_data-vendor-central_data-central\.\s*\n\s*ENDIF\.\s*\n\s*CLEAR ls_data-vendor-central_data-central\..*"
         r"ls_data2-vendor-central_data-central\s*= ls_vc_later\..*save\( EXPORTING is_data\s*= ls_data2"),
        ('posts a change of the partner without the supplier part when only partner data changes, as BP does',
         r"IF ls_data-vendor-company_data-company IS INITIAL\s*\n\s*AND ls_data-vendor-purchasing_data-purchasing "
         r"IS INITIAL\.\s*\n\s*CLEAR ls_data-vendor\."),
        ('posts a change without VALIDATE_SINGLE first - its buffer met the same insert again in MAINTAIN '
         '(R1 228 bank not unique, AM 605 mobile usage not unique)',
         r"IF iv_validate = abap_true\.\s*\n\s*TRY\.\s*\n\s*cl_md_bp_maintain=>validate_single\(.*"
         r"iv_validate = xsdbool\( iv_change = abap_false \)"),
        ('reads the partner\'s numbers and e-mails without restricting to an empty person number - a person\'s '
         'mobile was not seen and inserted again (AM 605)',
         r"FROM adr2\s*\n\s*WHERE addrnumber = @lv_bpadr\s*\n\s*ORDER BY"),
        ('sends nothing for a bank account the partner already has',
         r"OR to_upper\( lv_koinh_in \) = to_upper\( condense\( CONV string\( ls_bk_same-koinh \) \) \)\.\s*\n\s*CONTINUE\."),
        ('shows the partner\'s bank details, industries and CVI assignment, or telephone numbers when SAP '
         'refuses a change with R1 228, CVI_MAPPING 002 or AM 605',
         r"diagnose\( EXPORTING iv_bp\s*= lv_bp.*METHOD diagnose\.|METHOD diagnose\..*diagnose\( EXPORTING iv_bp\s*= lv_bp"),
        ('leaves the industry alone on a change when the partner has any (BUT0IS) - the supplier\'s key '
         'is derived from the partner\'s standard industry and cannot be set through the supplier '
         '(KBA 3581473, CVI_MAPPING 002)',
         r'FROM but0is\s*\n\s*WHERE partner = @lv_bp INTO @lv_bp_has_ind\.\s*\n'
         r'\s*IF lv_bp_has_ind = abap_true\.\s*\n\s*CLEAR lv_ind\.'),
        ('never sends BRSCH on a change', r"IF lv_t016 = abap_true AND iv_change = abap_false\."),

        ('leaves out an unchanged industry key on a change - CVI would insert the mapped industry '
         'again (CVI_MAPPING 002 "Industry 92 ... could not be assigned")',
         r'SELECT SINGLE brsch FROM lfa1 WHERE lifnr = @lv_lifnr INTO @lv_brsch_now\.\s*\n\s*'
         r'IF lv_brsch_now = <lv_brv>\.\s*\n\s*CLEAR: <lv_brv>, <lv_brx>\.'),
        ('keys an address change by the address GUID (BUT020) - without it BP refuses the change '
         'with R11 130 "An address with GUID ... does not exist"',
         r'SELECT SINGLE address_guid FROM but020.*ls_adr-data_key-guid = lv_aguid\.'),
        ('passes the telephone extension', r'ls_tel-contact-data-extension = lv_ext\.')):
    if not re.search(pat, INC, re.I | re.S):
        fail.append(f'LCL_HONC_BP no longer {what}')

n_save = len(re.findall(r'save\( EXPORTING is_data\s*= ls_data\s*\n', INC))
if n_save != 1:
    fail.append(f'LCL_HONC_BP saves the same request {n_save} times - after a failed save the partner\'s '
                f'buffers still hold it, and a second save writes the bank details twice (CRS, 20.11.2026)')

if re.search(r"task\s*= c_delete", INC):
    fail.append('LCL_HONC_BP deletes a bank detail - with time-dependent bank details the old one stays '
                'and the partner shows two accounts (CRS, 20.11.2026); overwrite line 1 instead')

if fail:
    print(f'{len(fail)} problem(s):')
    for f in fail:
        print('  -', f)
    sys.exit(1)
print('clean - ZFI_VEND_HONC creates and changes HONC vendors as business partners through '
      'their own LCL_HONC_BP, with the FK01 / FK02 data and the HONC file layout')
