"""Cipla's HONC vendor interface creates and changes suppliers as business partners.

ECC: ZFI_VEND_HONC (includes _DD, _SS, _FD) read the HONC file from AL11 and
created the vendor with an FK01 batch input, changed it with FK02. Both
transactions are gone in S/4HANA. Since November 2026 the program posts
through include ZFI_CIMMRA_BP (LCL_CIMMRA_BP, CL_MD_BP_MAINTAIN). This keeps
it that way:

  - no batch input is left, and ZFI_CIMMRA_BP comes before the forms that
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
INC = code(read('zfi_cimmra_bp.prog.abap'))

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

if not re.search(r'^INCLUDE zfi_vend_honc_ss\.\s*\nINCLUDE zfi_cimmra_bp\.\s*\nINCLUDE zfi_vend_honc_fd\.',
                 MAIN, re.I | re.M):
    fail.append('ZFI_VEND_HONC: ZFI_CIMMRA_BP is not included between _SS and _FD - the forms in '
                '_FD would not know LCL_CIMMRA_BP')
if not re.search(r'^START-OF-SELECTION\.', MAIN, re.I | re.M):
    fail.append('ZFI_VEND_HONC: no START-OF-SELECTION - after a class implementation the '
                'processing would not be reached')


def form(src, name):
    m = re.search(rf'^FORM {name}\b.*?^ENDFORM\.', src, re.I | re.M | re.S)
    return m.group(0) if m else ''


create, change, bp_data = form(FD, 'create_vendor'), form(FD, 'change_vendor'), form(FD, 'bp_data')
if 'lcl_cimmra_bp=>create(' not in create:
    fail.append('CREATE_VENDOR does not create the vendor through LCL_CIMMRA_BP=>CREATE')
if 'lcl_cimmra_bp=>change(' not in change:
    fail.append('CHANGE_VENDOR does not change the vendor through LCL_CIMMRA_BP=>CHANGE')
if not re.search(r'iv_bank_replace\s*=\s*abap_true', change):
    fail.append('CHANGE_VENDOR adds the file\'s bank account next to the old one - FK02 replaced '
                'the first account')
if not re.search(r'iv_comm_replace\s*=\s*abap_true', change):
    fail.append('CHANGE_VENDOR adds the file\'s mobile and e-mail next to the old ones - FK02 '
                'overwrote the standard ones')
if re.search(r'vendor-central_data-bankdetail', INC, re.I):
    fail.append('ZFI_CIMMRA_BP sends bank details on the supplier - in S/4HANA CVI takes them from '
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

if re.search(r'READ TABLE ta_data1 INTO wa_data1 WITH KEY pan = wa_data-pan BINARY SEARCH', FD, re.I):
    fail.append('VALIDATE_DATA looks up rejected records by PAN with BINARY SEARCH on TA_DATA1, '
                'which is sorted by bank key - rejected new vendors slip through and are created')

for what, pat in (
        ('places the industry key by T016 and TB038A',
         r'FROM t016 WHERE brsch = @lv_brsch.*FROM tb038a WHERE ind_sector = @lv_sec'),
        ('sends the fields asked for even when blank', r'ASSIGN COMPONENT lv_kf OF STRUCTURE is_cin'),
        ('replaces the first bank account on request', r'ls_bkdel-task\s*= c_delete\.'),
        ('sends bank details on the business partner, where CVI takes them from',
         r'APPEND ls_bk TO ls_data-partner-central_data-bankdetail-bankdetails\.'),
        ('overwrites the standard telephone / fax / e-mail by its CONSNUMBER on request',
         r'ls_tel-contact-data-consnumber = ls_o2-consnumber\..*ls_fax-contact-data-consnumber = '
         r'ls_o3-consnumber\..*ls_smt-contact-data-consnumber = ls_o6-consnumber\.'),
        ('leaves the name of a person partner alone', r"IF lv_bptype = '1'\."),
        ('leaves out an unchanged industry key on a change - CVI would insert the mapped industry '
         'again (CVI_MAPPING 002 "Industry 92 ... could not be assigned")',
         r'SELECT SINGLE brsch FROM lfa1 WHERE lifnr = @lv_lifnr INTO @lv_brsch_now\.\s*\n\s*'
         r'IF lv_brsch_now = <lv_brv>\.\s*\n\s*CLEAR: <lv_brv>, <lv_brx>\.'),
        ('keys an address change by the address GUID (BUT020) - without it BP refuses the change '
         'with R11 130 "An address with GUID ... does not exist"',
         r'SELECT SINGLE address_guid FROM but020.*ls_adr-data_key-guid = lv_aguid\.'),
        ('passes the telephone extension', r'ls_tel-contact-data-extension = lv_ext\.')):
    if not re.search(pat, INC, re.I | re.S):
        fail.append(f'ZFI_CIMMRA_BP no longer {what} (ZFI_VEND_HONC needs it)')

if fail:
    print(f'{len(fail)} problem(s):')
    for f in fail:
        print('  -', f)
    sys.exit(1)
print('clean - ZFI_VEND_HONC creates and changes HONC vendors as business partners through '
      'ZFI_CIMMRA_BP, with the FK01 / FK02 data and the HONC file layout')
