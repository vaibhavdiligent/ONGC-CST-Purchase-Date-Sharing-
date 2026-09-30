"""The download has to look like the template it was taken from.

The extractor writes its heading row from its own map, so any column the
map does not carry comes out with nothing over it. That is what the blank
columns in the customer sample were - the LSMW transaction code, the
reference customer and its company code, sales organisation, distribution
channel and division, the ALWAYS X flag: columns the upload program has no
use for and the map therefore never mentioned.

This compares the heading row of every tab of the two supplied templates,
column by column, with the heading row the extractor will write. Only the
differences listed below are allowed, and each says why.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, 'tools'))
from sim import sheets, squash

EX = open(os.path.join(ROOT, 'src/zmms_bp_mass_upload.prog.abap'), encoding='utf-8').read()

CUST = 'customer master LSMW -  with format.xlsx'
VEND = 'Vendor LSMW with Template.xlsx'

TAB = {
    'R1': (VEND, 'Vendor creation for All CC', 1), 'R2': (VEND, 'TDS upload', 1),
    'R3': (VEND, 'TAN details', 1),                'R4': (VEND, 'BANK Key creation', 1),
    'R5': (VEND, 'Bank details update', 1),        'R6': (VEND, 'Vendor extension', 1),
    'R7': (VEND, 'CIN details', 1),                'R8': (VEND, 'Patner function', 6),
    'R9': (VEND, 'Block_Unblocked', 4),
}

# Where the download deliberately says more than the template does.
ALLOWED = {
    ('R1', 50): 'the template heads both 50 and 62 ZTERM - one is the company '
                'code term, the other the purchasing organisation term',
    ('R1', 62): 'as above',
    ('R6',  5): 'the template repeats LIFNR, BUKRS and EKORG for the reference '
                'company code and purchasing organisation; naming them REF keeps '
                'each heading unique, which is what lets a moved column be found',
    ('R6',  6): 'as above',
    ('R6',  7): 'as above',
    ('R6',  8): 'the "Char" over 8, 10 and 11 is the Field Type row under the '
                'heading line, not a heading; 8 is read by nobody and stays blank',
    ('R6', 10): 'the template heads 8, 10 and 11 "Char"; ZWELS and REPRF say '
                'which field each one is',
    ('R6', 11): 'as above',
}

M = re.compile(r"\(\s*scen = '(R\d)' col = (\d+)\s+hdr = '((?:[^']|'')*)'")
mine = {}
for m in M.finditer(EX):
    mine.setdefault(m.group(1), {})[int(m.group(2))] = m.group(3).replace("''", "'")

bad, total = [], 0
# The vendor scenarios, R1..R9, of ZMMS_BP_MASS_UPLOAD's column map. The
# customer templates are checked by tools/cipla/sim_roundtrip.py.
for scen in [f'R{i}' for i in range(1, 10)]:
    book, tab, hrow = TAB[scen]
    path = os.path.join(ROOT, book)
    if not os.path.exists(path):
        bad.append(f'{book} is not in the repository - the comparison cannot run')
        break
    rows = dict(sheets(path)).get(tab)
    if rows is None:
        bad.append(f'{scen}: tab "{tab}" is not in {book}')
        continue
    head = dict(rows.get(hrow, {}))
    for c, v in rows.get(hrow + 1, {}).items():          # a two line heading
        if v and not head.get(c):
            head[c] = v
    cols = mine.get(scen, {})
    if not cols:
        bad.append(f'{scen}: no columns in the column map')
        continue
    total += max(cols)
    for c in range(1, max(cols) + 1):
        if squash(head.get(c, '')) == squash(cols.get(c, '')):
            continue
        if (scen, c) in ALLOWED:
            continue
        bad.append(f'{scen} column {c}: the template says {head.get(c, "")!r} and the '
                   f'download says {cols.get(c, "")!r}')

print(f'{total} columns compared over {len(TAB)} tabs')
print('\n'.join(bad) if bad else
      'clean - every tab of the download carries the template\'s own heading row, '
      'but for the eight columns the template leaves blank or names twice')
sys.exit(1 if bad else 0)
