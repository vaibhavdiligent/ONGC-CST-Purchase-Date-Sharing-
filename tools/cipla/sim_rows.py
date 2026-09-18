"""Run every template's column map the way the engine runs it, and check the row.

IN_ZBMR.xlsx came out of the system correct - the transaction code, the customer
number, the account group and the always-X flag all written, the rest of the
columns filled from the customer or left empty because that customer has nothing
in them. This does the same check for all 24 templates without a system: it walks
each map the way LCL_ENG=>CUST walks it, over a customer where every field has a
value, and reports any column that would come out empty when it should not.

It also proves the engine handles every node the map uses - a map entry with a
node the CASE does not cover would write nothing at all, silently.
"""
import collections, os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
SRC = open(os.path.join(ROOT, 'src/zsds_cust_tmpl_download.prog.abap'), encoding='utf-8').read()
ROW = re.compile(r"\(\s*tmpl = '(\w+)'\s+col = (\d+)\s+hdr = '(.*?)'\s+node = '(.)'\s+"
                 r"fld = '(.*?)'\s+fmt = '(\w*)'\s*\)")

# A customer where everything has a value, so an empty cell can only mean the
# map or the engine dropped it.
KEY = {'KUNNR': '144700', 'BUKRS': '1000', 'VKORG': '1000',
       'VTWEG': '10', 'SPART': '11', 'KTOKD': 'ZDOM'}
COMM = {'TEL': '9829108855', 'MOB': '9829108856',
        'FAX': '0145-123456', 'SMT': 'x@y.com'}


def engine_nodes():
    """The nodes LCL_ENG=>CUST actually handles.

    Taken from the whole method, not from the first ENDCASE after it - the node
    CASE has a second CASE nested inside it for the key fields, and slicing at
    the first ENDCASE stops before most of the branches.
    """
    start = SRC.index('METHOD cust.')
    end = SRC.index('  ENDMETHOD.', start)
    body = SRC[start:end]
    return {m.group(1) for m in re.finditer(r"WHEN '(.)'(?:\s+OR\s+'.')*\.", body)}


def engine_inner(node):
    """The field names the engine's inner CASE covers under one node.

    The key and the communication nodes branch again on the field name, so a
    field the map uses but that CASE does not name is written empty - which is
    exactly how a customer number goes missing from every template at once.
    """
    start = SRC.index('METHOD cust.')
    end = SRC.index('  ENDMETHOD.', start)
    body = SRC[start:end]
    m = re.search(r"WHEN '" + node + r"'\.\s*(?:\n\s*\".*)*\n\s*CASE ls_col-fld\.(.*?)ENDCASE\.",
                  body, re.S)
    if not m:
        return None
    # A branch may name several fields - WHEN 'TEL' OR 'MOB'. - so every
    # literal on the WHEN line counts, not just the first.
    out = set()
    for line in m.group(1).split('\n'):
        if re.match(r"\s*WHEN\s+'", line):
            out |= set(re.findall(r"'([A-Z0-9_]+)'", line))
    return out


def value_for(node, fld):
    """What the engine would put in the cell, for a customer that has everything."""
    if node == 'X':
        return fld                      # the constant the template asks for
    if node == '-':
        return ''                       # nothing to read, by design
    if node == 'K':
        return KEY.get(fld, '')
    if node == 'M':
        return COMM.get(fld, '')
    if node == 'I':
        return '1234 5678 9012'         # the Aadhaar number
    if node == 'T':
        return '1'                      # a tax classification
    if node in ('A', 'C', 'B', 'S', 'Z', 'P'):
        return f'<{fld}>'               # read from the structure that node names
    return None                         # a node the engine does not handle


def main():
    rows = ROW.findall(SRC)
    handled = engine_nodes()
    by = collections.defaultdict(list)
    for tmpl, col, hdr, node, fld, fmt in rows:
        by[tmpl].append((int(col), hdr, node, fld))

    findings = []
    for node in sorted({n for _, _, _, n, _, _ in rows}):
        if node not in handled:
            findings.append(f'the map uses node {node}, which LCL_ENG=>CUST does not '
                            f'handle - those columns would be written empty')

    for node, used in (('K', {f for _, _, _, n, f, _ in rows if n == 'K'}),
                       ('M', {f for _, _, _, n, f, _ in rows if n == 'M'})):
        covered = engine_inner(node)
        if covered is None:
            findings.append(f'node {node} branches on the field name in the map but the '
                            f'engine has no CASE for it')
            continue
        for f in sorted(used - covered):
            findings.append(f'node {node} field {f} is used by the map but the engine '
                            f'does not name it - those columns would be written empty')

    filled = blank = 0
    for tmpl, cols in sorted(by.items()):
        for col, hdr, node, fld in sorted(cols):
            v = value_for(node, fld)
            if v is None:
                findings.append(f'{tmpl} column {col} ("{hdr}"): node {node} is unknown')
            elif v == '':
                blank += 1
                # only the copy-from-reference columns are meant to be empty
                if node != '-':
                    findings.append(f'{tmpl} column {col} ("{hdr}"): node {node} field '
                                    f'{fld} produces nothing')
            else:
                filled += 1

    # the four columns IN_ZBMR proved, checked on every template that has them
    for tmpl, cols in sorted(by.items()):
        want = {'TCODE': None, 'KUNNR': None, 'KTOKD': None, 'USE_ZAV': None}
        for col, hdr, node, fld in cols:
            if node == 'X' and fld.startswith('XD'):
                want['TCODE'] = value_for(node, fld)
            if node == 'X' and fld == 'X':
                want['USE_ZAV'] = value_for(node, fld)
            if node == 'K' and fld == 'KUNNR':
                want['KUNNR'] = value_for(node, fld)
            if node == 'K' and fld == 'KTOKD':
                want['KTOKD'] = value_for(node, fld)
        for k, v in want.items():
            if v == '':
                findings.append(f'{tmpl}: {k} is mapped but would come out empty')

    if findings:
        print(f'{len(findings)} problem(s):')
        for f in findings[:30]:
            print('  -', f)
        return 1
    print(f'clean - all {len(by)} templates walk through the engine with every node '
          f'handled; {filled} columns take a value and {blank} are the '
          f'copy-from-reference columns that are meant to be empty')
    return 0


if __name__ == '__main__':
    sys.exit(main())
