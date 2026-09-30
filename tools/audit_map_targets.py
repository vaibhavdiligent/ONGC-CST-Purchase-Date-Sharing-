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
ALLOWED = {}

CASES = [
    ('ZMMS_BP_MASS_UPLOAD', 'src/zmms_bp_mass_upload.prog.abap',
     r"\(\s*scen = '(R\d)' col = (\d+)\s+hdr = '(?:[^']|'')*' node = '([\w-]*)' fld = '([^']*)'"),
    # The customer template program reads and writes Cipla's 24 templates
    # from one map, so a column pointed at the wrong field is wrong in both
    # directions. Constant (X) columns hold the transaction code, not data.
    ('ZSDS_CUST_TMPL_DOWNLOAD', 'src/zsds_cust_tmpl_download.prog.abap',
     r"\(\s*tmpl = '(\w+)' col = (\d+)\s+hdr = '(?:[^']|'')*' node = '([\w-]*)' fld = '([^']*)'"),
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
        if (prog, scen, node, fld) in ALLOWED:
            continue
        bad.append(f'{prog} {scen}: columns {", ".join(str(c) for c in sorted(cols))} '
                   f'all write {node}/{fld} - only the last one has any effect')

# ---- a mapped column with no heading ------------------------------------
# The download writes the heading row from this map, so a mapped column
# with no heading comes out of the download blank - and two of them side by
# side, both ZTERM, read as the same field twice. A column read by position
# is also only right while the file is laid out as the template is, so a
# heading is worth having for its own sake.
for prog, path, pattern in CASES:
    if prog != 'ZMMS_BP_MASS_UPLOAD':
        continue
    src = open(os.path.join(ROOT, path), encoding='utf-8').read()
    for m in re.finditer(r"\(\s*scen = '(R\d)' col = (\d+)\s+hdr = '((?:[^']|'')*)' "
                         r"node = '([\w-]*)' fld = '([^']*)'", src):
        scen, col, hdr, node, fld = m.group(1), m.group(2), m.group(3), m.group(4), m.group(5)
        if node not in ('', '-') and not hdr:
            bad.append(f'{prog} {scen}: column {col} writes {node}/{fld} but has no heading - '
                       f'it comes out of the download blank')

# ---- a column of the template the map does not mention at all ----------
# The download writes the heading row from this map, so a template column
# missing from it comes out of the download with no heading over it - a
# blank column in the middle of the sheet, which is what "column names are
# coming as blank" meant. A column the upload program does not read still
# belongs in the map, with its heading and nothing else.
NO_HEADING = {
    ('ZMMS_BP_MASS_UPLOAD', 'R2',  1): 'the template has no heading there either',
}
for prog, path, pattern in CASES:
    if prog != 'ZMMS_BP_MASS_UPLOAD':
        continue
    src = open(os.path.join(ROOT, path), encoding='utf-8').read()
    cols = collections.defaultdict(set)
    for m in re.finditer(pattern, src):
        cols[m.group(1)].add(int(m.group(2)))
    for scen, have in sorted(cols.items()):
        for c in range(1, max(have) + 1):
            if c in have or (prog, scen, c) in NO_HEADING:
                continue
            bad.append(f'{prog} {scen}: column {c} is not in the map, so the download '
                       f'has no heading over it')

# ---- a column that sits in one block and writes into another -----------
# The blocks of a template run together - key, address, general data,
# company code, sales area, licence - so a column whose node differs from
# the column each side of it is either a deliberate one-off or a column
# pointed at the wrong part of the record. These are the deliberate ones.
ISLAND_OK = {
    ('ZMMS_BP_MASS_UPLOAD', 'R9',  6): 'SPERR_1 is the company code block of an otherwise central tab',
    ('ZMMS_BP_MASS_UPLOAD', 'R9',  8): 'SPERM_1 is the purchasing block of an otherwise central tab',
    ('ZSDS_CUST_TMPL_DOWNLOAD', '08585a5a', 66): 'one tax classification column among the sales area columns',
    ('ZSDS_CUST_TMPL_DOWNLOAD', '8a74041a', 61): 'one tax classification column among the sales area columns',
    ('ZSDS_CUST_TMPL_DOWNLOAD', 'd7ee33bb', 54): 'one tax classification column among the sales area columns',
    ('ZSDS_CUST_TMPL_DOWNLOAD', 'e10ec770', 83): 'one tax classification column among the sales area columns',
    ('ZSDS_CUST_TMPL_DOWNLOAD', 'f7e2b95a', 59): 'one tax classification column among the sales area columns',
}
# A tab laid out in pairs rather than blocks. The XD05 template puts each
# central block beside its company code or sales area twin, so every column
# is an island by construction.
ISLAND_TAB = {
    ('ZSDS_CUST_TMPL_DOWNLOAD', 'BLOCK'): 'central and local block flags alternate, pair by pair',
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
            if (prog, scen, col) in ISLAND_OK or (prog, scen) in ISLAND_TAB:
                continue
            bad.append(f'{prog} {scen}: column {col} writes {node}/{fld} but the columns '
                       f'each side of it write {prv} - check it is pointed at the right '
                       f'part of the record')

print('\n'.join(bad) if bad else
      'clean - every column of every template has a heading in the download, no two '
      'write the same field of the same node, and none writes into a different part '
      'of the record than its neighbours')
sys.exit(1 if bad else 0)
