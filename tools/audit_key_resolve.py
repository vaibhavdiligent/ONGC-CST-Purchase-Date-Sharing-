"""A key cell can hold the master record's number or the partner's.

The templates label the key column LIFNR or KUNNR, but a user working in BP
reads the partner number off the screen, and the two are only the same
number where the grouping is flagged for it in CVIC_VEND_TO_BP1 /
CVIC_CUST_TO_BP1. A row keyed by the partner number would otherwise be
turned away with "Vendor 1000000241 does not exist" - or, worse, on the
creation tab, treated as a new vendor to make.

So every key cell goes through one resolver - KEY_LIFNR in the supplier
program, KEY_KUNNR in the customer one - which tries the number as the
master record first and as the partner second. This checks that no handler
reads a key any other way.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CASES = [
    ('src/zmms_bp_mass_upload.prog.abap', 'key_lifnr',
     r'lcl_util=>lifnr\(\s*lcl_util=>cell\('),
    ('src/zsds_cust_mass_upload.prog.abap', 'key_kunnr',
     r'lv_kunnr\s*=\s*lcl_util=>alpha\('),
]

def methods(src):
    out, cur, name = [], [], None
    for i, line in enumerate(src.split('\n'), start=1):
        m = re.match(r'\s*METHOD\s+([\w~]+)\s*\.', line)
        if m:
            name, cur, start = m.group(1), [], i
            continue
        if re.match(r'\s*ENDMETHOD', line) and name:
            out.append((name, start, cur)); name = None
            continue
        if name is not None:
            cur.append((i, line))
    return out

bad = []
for path, resolver, pattern in CASES:
    src = open(os.path.join(ROOT, path), encoding='utf-8').read()
    rx = re.compile(pattern)
    for name, _start, lines in methods(src):
        if name.lower() == resolver:
            continue
        for lineno, line in lines:
            code = re.sub(r'".*$', '', line)
            if rx.search(code):
                bad.append(f'{path}:{lineno}: {name}( ) builds a key without '
                           f'{resolver.upper()}( ) - a partner number given in that '
                           f'column would not be recognised\n      {code.strip()}')

print('\n'.join(bad) if bad else
      'clean - every key cell is read through the resolver, so a business partner '
      'number in the key column finds its master record')
sys.exit(1 if bad else 0)
