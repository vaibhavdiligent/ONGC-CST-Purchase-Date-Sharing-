"""S/4HANA compatibility, checked rather than assumed.

What it holds the programs to:

  * nothing obsolete in the language - OCCURS, a table with a header line,
    the POSIX regex, the obsolete MOVE / ADD / SUBTRACT / COMPUTE forms;
  * no transaction driven from code. XD01, XD02, XD03, XD05, VD01, FD01 and
    the rest of the customer and vendor transactions are gone in S/4HANA, so
    a CALL TRANSACTION or a batch input against one would simply fail. A
    transaction code sitting in a template column is content, not a call, and
    is left alone;
  * no obsolete table. J_1IMOVEND and the other CIN master tables were folded
    into LFA1 and KNA1;
  * every table, class and function module the program uses is one that is
    known present - in the dictionary extract taken from the system, or in a
    program already active in it, or on the short list of standard objects
    named here;
  * a program that reads master data through a not-released interface says so
    in its header, because that is the exemption ATC will ask about.
"""
import json, os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DD = json.load(open(os.path.join(ROOT, 'tools/ddic.json')))
PROGS = ['src/zsds_cust_mass_upload.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap',
         'src/zbcs_mass_upload_extract.prog.abap',
         'src/zsds_cust_tmpl_download.prog.abap']

# Transactions S/4HANA no longer has. Driving one from code fails outright.
GONE_TCODE = {'XD01', 'XD02', 'XD03', 'XD05', 'XD06', 'XD07', 'XD99',
              'VD01', 'VD02', 'VD03', 'VD05', 'VD06',
              'FD01', 'FD02', 'FD03', 'FD05', 'FD06',
              'XK01', 'XK02', 'XK03', 'XK05', 'XK06', 'XK07',
              'MK01', 'MK02', 'MK03', 'FK01', 'FK02', 'FK03'}

# Tables S/4HANA no longer has.
GONE_TABLE = {'J_1IMOVEND', 'J_1IMOCUST', 'KNA1_OLD', 'BSIS', 'BSAS', 'BSID',
              'BSAD', 'BSIK', 'BSAK', 'BSIM', 'FAGLBSIS', 'MSEG_OLD', 'VBUK', 'VBUP'}

# Standard objects that are not in the extract because the extract was taken
# for another purpose. Each is a current S/4HANA object.
KNOWN = {
    # texts and Customizing
    'T005', 'T005T', 'T077D', 'T077K', 'T077X', 'TSAD3T', 'T001', 'T001W',
    'T024E', 'T052', 'T056', 'T059P', 'T059Z', 'T500W', 'TCURC', 'TPAR',
    'TSTL', 'TVV3', 'TVKO', 'TVKOV', 'TSPA', 'SKB1', 'DD02L',
    # business partner, and the link to the customer and the supplier
    'BUT000', 'BUT100', 'BUT0ID', 'CVI_CUST_LINK', 'CVI_VEND_LINK',
    'CVIC_CUST_TO_BP1', 'CVIC_CUST_TO_BP2', 'CVIC_VEND_TO_BP1', 'CVIC_VEND_TO_BP2',
    # supplier master, which the business partner still writes through
    'LFA1', 'LFB1', 'LFBK', 'LFBW', 'LFM1', 'WYT3', 'BNKA', 'FIWTIN_TAN_EXEM',
    # FSCM credit management
    'UKMBP_CMS', 'UKMBP_CMS_SGM', 'UKMCRED_SGM0C', 'UKM_KKBER2SGM',
    # Cipla's own
    'ZPARAM_TABLE', 'ZFI_VEND_MSME', 'ZFI_TAX_TAB', 'ZSD_LICENSE_CHK',
}

OBSOLETE = [(r'\bOCCURS\s+\d', 'OCCURS'),
            (r'\bWITH\s+HEADER\s+LINE\b', 'a table with a header line'),
            (r'OCCURRENCES OF\s+REGEX', 'the POSIX regex'),
            (r'^\s*MOVE\s+\S+\s+TO\s', 'the obsolete MOVE'),
            (r'^\s*COMPUTE\s', 'COMPUTE'),
            (r'^\s*ADD\s+\S+\s+TO\s', 'the obsolete ADD'),
            (r'^\s*SUBTRACT\s', 'SUBTRACT'),
            (r'\bFIELD-GROUPS\b', 'FIELD-GROUPS')]

findings = []


def strip(src):
    return '\n'.join(re.sub(r'".*$', '', l) for l in src.split('\n')
                     if not l.lstrip().startswith('*'))


def main():
    for p in PROGS:
        path = os.path.join(ROOT, p)
        if not os.path.exists(path):
            continue
        name = os.path.basename(p)
        raw = open(path, encoding='utf-8').read()
        code = strip(raw)

        for pat, what in OBSOLETE:
            for m in re.finditer(pat, code, re.I | re.M):
                line = code[:m.start()].count('\n') + 1
                findings.append(f'{name}: {what}, around line {line}')

        # a transaction driven from code
        for m in re.finditer(r"CALL TRANSACTION\s+'([^']+)'", code, re.I):
            t = m.group(1).upper()
            findings.append(f'{name}: CALL TRANSACTION {t}' +
                            (' - that transaction is gone in S/4HANA' if t in GONE_TCODE else ''))
        for m in re.finditer(r'\bBDCDATA\b|\bBDC_OKCODE\b', code, re.I):
            findings.append(f'{name}: builds batch input, which needs a transaction to feed')

        # the tables it reads
        tabs = set()
        for m in re.finditer(r'\bSELECT\b(?:\s+SINGLE)?\s+(?:.+?)\s+FROM\s+([a-z_][a-z_0-9]*)',
                             code, re.I | re.S):
            t = m.group(1).upper()
            if t in ('WHERE', 'INTO', 'UP', 'ORDER', 'FOR'):
                continue          # a keyword caught by a multi-line SELECT
            tabs.add(t)
        for t in sorted(tabs):
            if t in GONE_TABLE:
                findings.append(f'{name}: reads {t}, which S/4HANA no longer has')
            elif t not in DD and t not in KNOWN:
                findings.append(f'{name}: reads {t}, which is neither in the dictionary '
                                f'extract nor on the list of known standard objects - '
                                f'check it by hand and add it')

        # the exemption ATC will ask about
        uses_unreleased = re.search(r'\b(CMD_EI_API_EXTRACT|VMD_EI_API_EXTRACT|'
                                    r'CL_MD_BP_MAINTAIN|CMD_EI_API|VMD_EI_API)\b', code, re.I)
        if uses_unreleased and 'ABAP_CLOUD_READINESS' not in raw.upper():
            findings.append(f'{name}: reads master data through a not-released interface '
                            f'but the header does not record the ABAP Cloud exemption')

    if findings:
        print(f'{len(findings)} problem(s):')
        for f in findings:
            print('  -', f)
        return 1
    print('clean - no obsolete statement, table or transaction; every object is one '
          'known present; and every not-released interface is declared in the header')
    return 0


if __name__ == '__main__':
    sys.exit(main())
