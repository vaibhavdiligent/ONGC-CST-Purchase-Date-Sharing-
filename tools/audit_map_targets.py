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

# ---- a column that sits in one block and writes into another -----------
# The blocks of a template run together - key, address, general data,
# company code, sales area, licence - so a column whose node differs from
# the column each side of it is either a deliberate one-off or a column
# pointed at the wrong part of the record. These are the deliberate ones.
ISLAND_OK = {
    ('ZSDS_CUST_MASS_UPLOAD', 'R3',  33): 'the column is headed TIME_ZONE, which is an address field',
    ('ZSDS_CUST_MASS_UPLOAD', 'R5',  17): 'the credit tab updates the customer group alongside the limit',
    ('ZBCS_MASS_UPLOAD_EXTRACT', 'C3', 33): 'the column is headed TIME_ZONE, which is an address field',
    ('ZBCS_MASS_UPLOAD_EXTRACT', 'C5', 17): 'the credit tab updates the customer group alongside the limit',
    ('ZBCS_MASS_UPLOAD_EXTRACT', 'C3', 60): 'one tax classification column among the sales area columns',
    ('ZBCS_MASS_UPLOAD_EXTRACT', 'C4', 71): 'one tax classification column among the sales area columns',
    ('ZBCS_MASS_UPLOAD_EXTRACT', 'V9',  6): 'SPERR_1 is the company code block of an otherwise central tab',
    ('ZBCS_MASS_UPLOAD_EXTRACT', 'V9',  8): 'SPERM_1 is the purchasing block of an otherwise central tab',
}

for prog, path, pattern in CASES:
    src = open(os.path.join(ROOT, path), encoding='utf-8').read()
    per = collections.defaultdict(list)
    for m in re.finditer(pattern, src):
        per[m.group(1)].append((int(m.group(2)), m.group(3), m.group(4)))
    for scen, entries in per.items():
        rows = sorted(entries)
        for i, (col, node, fld) in enumerate(rows):
            if i == 0 or i + 1 >= len(rows):
                continue
            prv, nxt = rows[i - 1][1], rows[i + 1][1]
            if node in ('', '-') or prv in ('', '-') or prv != nxt or node == prv:
                continue
            if (prog, scen, col) in ISLAND_OK:
                continue
            bad.append(f'{prog} {scen}: column {col} writes {node}/{fld} but the columns '
                       f'each side of it write {prv} - check it is pointed at the right '
                       f'part of the record')

print('\n'.join(bad) if bad else
      'clean - no two columns of a tab write the same field of the same node, and no '
      'column writes into a different part of the record than its neighbours')
sys.exit(1 if bad else 0)
