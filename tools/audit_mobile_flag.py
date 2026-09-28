"""R_3_USER is a coded value, not a flag.

It reads like one - a one character field on a telephone record that says
"this is a mobile" - and every one of these programs treated it as one.  It
is not.  SAP's CL_ADDR_MAP=>CONVERT_ADTEL_TO_TELEPHONE:

    CASE is_adtel-r3_user.
      WHEN space OR '1'.  CLEAR rs_telephone-mobile_phone.
      WHEN '2'   OR '3'.  rs_telephone-mobile_phone = c_true.
      WHEN OTHERS.        MESSAGE x890(am) WITH 'ADTEL-R3_USER'.
    ENDCASE.

An X is WHEN OTHERS, and that MESSAGE is type X: the run goes down with
MESSAGE_TYPE_X, "Internal error - value range of ADTEL-R3_USER", before a
single vendor is written.  Cipla's file has a mobile number in every row, so
the first row took the whole transaction with it.

Reading it back has the mirror fault: a mobile stored correctly as 3 never
equals X, so it comes back as a landline and the download writes it into the
wrong column.

So: the DATA field takes space, 1, 2 or 3 and is tested for 2 or 3.  The
DATAX field beside it is a real flag and does take X - that one is left alone.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROGS = ['src/zsds_cust_mass_upload.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap',
         'src/zbcs_mass_upload_extract.prog.abap',
         'src/zsds_cust_tmpl_download.prog.abap',
         'tools/cipla/download_skeleton.abap']

BAD = re.compile(r'(?<!data)x-r_3_user|data-r_3_user\s*(?:=|<>)\s*'
                 r'(abap_true|abap_false|\'X\'|\'x\')', re.I)
# what the value may be, wherever it is written into the DATA field
SET = re.compile(r"data-r_3_user\s*=\s*([^\s.,)]+)", re.I)
OK_VALUES = {"' '", "''", "``", "'1'", "'2'", "'3'", 'space', 'gc_mobile'}

findings = []

for prog in PROGS:
    path = os.path.join(ROOT, prog)
    if not os.path.exists(path):
        continue
    name = os.path.basename(prog)
    src = open(path, encoding='utf8').read()

    for m in re.finditer(r'\bdata-r_3_user\s*(=|<>)\s*([^\s.,)]+)', src, re.I):
        op, val = m.group(1), m.group(2)
        line = src[:m.start()].count('\n') + 1
        # the DATAX field is a genuine flag; skip it
        if src[max(0, m.start() - 6):m.start()].lower().endswith('datax'):
            continue
        if val.lower() in ('abap_true', 'abap_false', "'x'"):
            findings.append(f'{name}:{line}: R_3_USER is compared or set with '
                            f'{val.upper()} - it takes space, 1, 2 or 3, and an '
                            f'X brings the run down with MESSAGE_TYPE_X')
        elif op == '=' and val.lower() not in OK_VALUES:
            findings.append(f'{name}:{line}: R_3_USER is set to {val}, which is '
                            f'not one of space, 1, 2, 3')

    # the mobile marker itself, wherever a constant carries it
    for m in re.finditer(r"CONSTANTS\s+gc_mobile[^.]*VALUE\s+'([^']*)'", src, re.I):
        if m.group(1) not in ('2', '3'):
            line = src[:m.start()].count('\n') + 1
            findings.append(f'{name}:{line}: the mobile marker is '
                            f'"{m.group(1)}" - SAP reads only 2 and 3 as a mobile')

if findings:
    print('\n'.join(findings))
    sys.exit(1)
print('clean - the mobile marker is a coded value everywhere it is written and '
      'everywhere it is read')
