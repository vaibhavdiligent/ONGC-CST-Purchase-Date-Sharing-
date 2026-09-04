"""Two columns of one tab must not write the same field of the same node.

A template column is a place in a file; a node and field is a place in the
master record. When two columns name the same one, the later column wins
and the earlier one is thrown away silently - and the field the earlier
column was meant for is never written at all.

That is what kept KNVV-ZTERM empty. Column 75 of the domestic customer tab
sits in the sales-area block and is the sales-area payment terms, but it
was mapped to the company code's ZTERM, which column 54 already wrote. The
row was refused with

    Terms of Payment (KNVV-ZTERM) is a required entry field

and the SAGA tab had the same shape: its GST number column pointed at the
reconciliation account, which the next column then overwrote.

Where a template really does repeat a field, the pair is listed below with
the reason, so a new duplicate stands out from an old one.
"""
import collections, os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# Duplicates that are the template's own doing, not a mapping mistake.
ALLOWED = {
    ('ZSDS_CUST_MASS_UPLOAD', 'R1', 'S', 'KDGRP'):
        'the tab carries "Customer group" twice, at 62 and 117, and both are KNVV-KDGRP',
    ('ZSDS_CUST_MASS_UPLOAD', 'R5', 'U', 'LIMIT_SGM'):
        'KLIME and KLIMK are the classic pair and FSCM holds one limit per '
        'segment; CREDIT( ) takes the first and warns when they disagree',
}

CASES = [
    ('ZSDS_CUST_MASS_UPLOAD', 'src/zsds_cust_mass_upload.prog.abap',
     r"\(\s*scen = '(R\d)' col = (\d+)\s+node = '(\w)' fld = '(\w*)'"),
    ('ZBCS_MASS_UPLOAD_EXTRACT', 'src/zbcs_mass_upload_extract.prog.abap',
     r"\(\s*scen = '([CV]\d)' col = (\d+)\s+hdr = '(?:[^']|'')*' node = '([\w-]*)' fld = '([^']*)'"),
]

bad = []
for prog, path, pattern in CASES:
    src = open(os.path.join(ROOT, path), encoding='utf-8').read()
    seen = collections.defaultdict(list)
    for m in re.finditer(pattern, src):
        scen, col, node, fld = m.group(1), int(m.group(2)), m.group(3), m.group(4)
        if not fld or node in ('', '-'):
            continue
        # FIELD#n addresses the nth occurrence of a repeating node, so the
        # occurrence is part of what is written and stays in the key.
        seen[(scen, node, fld)].append(col)
    for (scen, node, fld), cols in sorted(seen.items()):
        if len(cols) < 2:
            continue
        key = (prog if prog != 'ZBCS_MASS_UPLOAD_EXTRACT' else 'ZSDS_CUST_MASS_UPLOAD',
               scen.replace('C', 'R') if prog == 'ZBCS_MASS_UPLOAD_EXTRACT' and scen[0] == 'C' else scen,
               node, fld)
        if key in ALLOWED:
            continue
        bad.append(f'{prog} {scen}: columns {", ".join(str(c) for c in sorted(cols))} '
                   f'all write {node}/{fld} - only the last one has any effect')

print('\n'.join(bad) if bad else
      'clean - no two columns of a tab write the same field of the same node')
sys.exit(1 if bad else 0)
