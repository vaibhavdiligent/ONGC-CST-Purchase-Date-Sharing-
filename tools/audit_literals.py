"""Blank text-field literals in string operations.

A quoted literal drops its trailing blanks, so ' ' is not a blank - it is
empty. Two consequences, both of which have bitten this project:

  * as a search pattern or separator, REPLACE / FIND / SPLIT get an empty
    pattern and the program terminates with CX_SY_REPLACE_INFINITE_LOOP;
  * as a replacement, nothing is inserted where a blank was meant, so
    "Z1/3" silently becomes "Z13".

Backquotes keep the blank: ` `. CONDENSE ... NO-GAPS removes blanks without
a pattern at all.

Only the three programs of this project are scanned; the older reports in
the repository are not ours to change.
"""
import re, sys

PROGS = ['src/zsds_cust_mass_upload.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap',
         'src/zbcs_mass_upload_extract.prog.abap']

BLANK = r"'\s+'"                      # a quoted literal of blanks only
findings = []

for p in PROGS:
    for i, l in enumerate(open(p, encoding='utf-8'), 1):
        code = l.strip()
        if code.startswith('*') or code.startswith('"'):
            continue
        code = re.sub(r'".*$', '', code)

        if re.search(r'\b(REPLACE|FIND)\b[^.]*?\bOF\s+' + BLANK, code, re.I) \
        or re.search(r'\bREPLACE\s+' + BLANK, code, re.I) \
        or re.search(r'\bSPLIT\b[^.]*?\bAT\s+' + BLANK, code, re.I):
            findings.append(f'{p}:{i}: a blank quoted literal as a search pattern - '
                            f'that pattern is EMPTY and dumps with '
                            f'CX_SY_REPLACE_INFINITE_LOOP: {code}')

        if re.search(r'\bWITH\s+' + BLANK, code, re.I) \
        or re.search(r'\bINTO\s+' + BLANK, code, re.I):
            findings.append(f'{p}:{i}: a blank quoted literal as a replacement - '
                            f'nothing is inserted where a blank was meant; '
                            f'use a backquoted ` `: {code}')

print('\n'.join(findings) if findings else
      'clean - no blank quoted literal is used as a pattern or a replacement')
sys.exit(1 if findings else 0)
