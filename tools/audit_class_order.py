"""A class used before its definition has been read.

ABAP reads a program top to bottom. A class's components can be reached only
once its DEFINITION has been processed; where definitions and
implementations are interleaved class by class, as they are here, an
implementation that calls a class defined further down fails to activate:

    The type "LCL_MAIN" is unknown.

CLASS ... DEFINITION DEFERRED makes the NAME known early - enough for
TYPE REF TO - but not its components, so a static call or a NEW still needs
the real definition first.

This bit when the upload half was merged into the download program: the
engine's SHEET method called LCL_MAIN=>LABEL, and LCL_MAIN is defined after
the engine because it drives it.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROGS = ['src/zmms_bp_mass_upload.prog.abap',
         'src/zsds_cust_tmpl_download.prog.abap']

findings = []

for prog in PROGS:
    path = os.path.join(ROOT, prog)
    if not os.path.exists(path):
        continue
    name = os.path.basename(prog).split('.')[0].upper()
    lines = open(path, encoding='utf8').read().split('\n')

    defined_at = {}
    for i, l in enumerate(lines):
        m = re.match(r'^\s*CLASS\s+(\w+)\s+DEFINITION\b(?!.*\bDEFERRED\b)', l, re.I)
        if m:
            defined_at.setdefault(m.group(1).lower(), i)

    for i, l in enumerate(lines):
        code = re.sub(r'"[^"]*$', '', l)
        if code.lstrip().startswith('*'):
            continue
        # static call, instantiation, or a type that needs the full definition
        for m in re.finditer(r'\b(lc[lx]_\w+)\s*=>|\bNEW\s+(lc[lx]_\w+)\s*\(|'
                             r'\bRAISE\s+EXCEPTION\s+(?:NEW|TYPE)\s+(lc[lx]_\w+)', code, re.I):
            cls = (m.group(1) or m.group(2) or m.group(3)).lower()
            at = defined_at.get(cls)
            if at is not None and at > i:
                findings.append(f'{name}:{i + 1}: {cls.upper()} is used here but defined '
                                f'at line {at + 1} - its definition has not been read yet')

if findings:
    print('\n'.join(sorted(set(findings))))
    sys.exit(1)
print('clean - no class is reached before its definition has been read')
