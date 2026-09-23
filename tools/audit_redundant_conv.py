"""CONV to a type the value already has.

"Redundant conversion for type STRING" is a warning, and a harmless one at
run time, but it is a line saying something the code does not do - and a
program carrying a screenful of them is a program whose real warnings nobody
reads any more.

Only the case that can be decided without a type checker is reported: the
operand is a name declared TYPE STRING everywhere it is declared in the
program, so whichever declaration is in scope, it is a STRING already.
A name declared TYPE CLIKE in one method and TYPE STRING in another is left
alone - CLIKE is generic, and CONV is needed there.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROGS = ['src/zsds_cust_mass_upload.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap',
         'src/zbcs_mass_upload_extract.prog.abap',
         'src/zsds_cust_tmpl_download.prog.abap']

findings = []

for prog in PROGS:
    path = os.path.join(ROOT, prog)
    if not os.path.exists(path):
        continue
    name = os.path.basename(prog).split('.')[0].upper()
    src = open(path, encoding='utf8').read()

    types = {}
    for m in re.finditer(r'\b(\w+)\s+TYPE\s+([\w\-/=>]+)', src):
        types.setdefault(m.group(1).lower(), set()).add(m.group(2).lower())

    for m in re.finditer(r'CONV\s+(?:string|#)\(\s*([A-Za-z_]\w*)\s*\)', src, re.I):
        op = m.group(1).lower()
        if types.get(op) == {'string'}:
            line = src[:m.start()].count('\n') + 1
            findings.append(f'{name}:{line}: CONV of {op.upper()}, which is a '
                            f'STRING already - redundant conversion')

if findings:
    print('\n'.join(findings))
    sys.exit(1)
print('clean - nothing is converted to a type it already has')
