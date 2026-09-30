"""Every method a program declares is called somewhere.

AUDIT_UNUSED looks at data. A method nobody calls is its own kind of
leftover: when ZSDS_CUST_TMPL_DOWNLOAD took over the customer upload, ten
configuration readers came across with it that only the retired credit tab
used - one of them read UKM_KKBER2SGM and UKMCRED_SGM0C on every run - and
the extractor kept PARTNER_OF after its customer scenarios were removed.

A method counts as called when its name is followed by "(" anywhere outside
a comment line, is named in CALL METHOD or SET HANDLER, or implements an
interface method or an event.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROGS = ['src/zsds_cust_tmpl_download.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap',
         'src/zbcs_mass_upload_extract.prog.abap']

# Known and accepted, with the reason.
KEEP = {
    ('src/zmms_bp_mass_upload.prog.abap', 'ok_zterm'):
        'harmless; removing it would mean re-transporting the vendor program '
        'for no functional change - take it out with the next real change',
}

findings = []
for p in PROGS:
    src = open(os.path.join(ROOT, p), encoding='utf-8').read()
    # whole-line comments only: a " inside a string template is not a comment
    code = '\n'.join(l for l in src.split('\n')
                     if not l.startswith('*') and not l.lstrip().startswith('"'))
    declared = set(m.lower() for m in
                   re.findall(r'^\s*(?:CLASS-)?METHODS\s+(\w+)\b(?!~)', code, re.M | re.I))
    for name in sorted(declared):
        if name in ('constructor', 'class_constructor'):
            continue
        if re.search(rf'METHODS\s+{name}\b[^.]*\bFOR EVENT\b', code, re.I | re.S):
            continue
        called = (re.search(rf'(?<![\w~]){name}\s*\(', code.replace(f'METHOD {name}.', ''), re.I)
                  and len(re.findall(rf'(?<![\w~]){name}\s*\(', code, re.I)) >
                      len(re.findall(rf'METHODS\s+{name}\s*\(', code, re.I)))
        called = called or re.search(rf'CALL METHOD\s+\S*?\b{name}\b|SET HANDLER\s+\S*?\b{name}\b',
                                     code, re.I)
        if called or (p, name) in KEEP:
            continue
        findings.append(f'{p}: method {name} is declared and implemented but never called')

if findings:
    print('\n'.join(findings))
    sys.exit(1)
print(f'clean - every method of the {len(PROGS)} programs is called '
      f'({len(KEEP)} accepted exception, with its reason)')
