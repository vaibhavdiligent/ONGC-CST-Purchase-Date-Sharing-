"""Check the column map of ZSDS_CUST_TMPL_DOWNLOAD against the system's dictionary.

Every column of every template says which part of the customer master holds it. This
proves that each of those fields really is a component of that structure - taken from
the DD03L extract of system CRS, not assumed - and that the map still matches the
template workbook it was generated from.
"""
import json, os, re, sys, collections

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
DD   = json.load(open(os.path.join(ROOT, 'tools/ddic.json')))
REG  = json.load(open(os.path.join(ROOT, 'docs/cipla/customer_template_registry.json')))
SRC  = open(os.path.join(ROOT, 'src/zsds_cust_tmpl_download.prog.abap'), encoding='utf-8').read()

ROW = re.compile(r"\(\s*tmpl = '(\w+)'\s+col = (\d+)\s+hdr = '(.*?)'\s+node = '(.)'\s+"
                 r"fld = '(.*?)'\s+fmt = '(\w*)'\s*\)")
# The combination row carries the REGION first: the workbook is organised by
# region, and a country alone does not name one template - Europe is four
# countries, and the United States is two entities sharing three account groups.
COMBI = re.compile(r"\(\s*regn = '(\w+)'\s+land = '(\w+)'\s+ktokd = '(\w+)'\s+"
                   r"tmpl = '(\w+)'\s*\)")

# The structure each node reads from, as the engine reads it.
NODE_TABLE = {'A': 'CVIS_EI_1VL',                 # address, BAPIAD1VL flattened
              'C': 'CMDS_EI_VMD_CENTRAL_DATA',    # general data
              'B': 'KNB1',                        # company code
              'S': 'KNVV',                        # sales area
              'Z': 'ZSD_LICENSE_CHK',             # the licence record
              'P': 'KNVK'}                        # contact person
KEY  = {'KUNNR', 'BUKRS', 'VKORG', 'VTWEG', 'SPART', 'KTOKD'}
COMM = {'TEL', 'MOB', 'FAX', 'SMT'}
RENAMED = {'cust extn': 'EXTN', 'block unblock': 'BLOCK'}

fail = []


def check(cond, text):
    if not cond:
        fail.append(text)


def main():
    rows = ROW.findall(SRC)
    check(rows, 'the program carries no column map at all')
    fields = {t: {x['f'] for x in DD.get(t, [])} for t in set(NODE_TABLE.values())}
    for t, f in fields.items():
        check(f, f'{t} is missing from the dictionary extract')

    # --- 1. every column lands on a field that exists ---------------------
    for tmpl, col, hdr, node, fld, fmt in rows:
        where = f'{tmpl} column {col} ("{hdr}")'
        if node in NODE_TABLE:
            check(fld in fields[NODE_TABLE[node]],
                  f'{where}: {fld} is not a component of {NODE_TABLE[node]}')
        elif node == 'K':
            check(fld in KEY, f'{where}: {fld} is not one of the key fields')
        elif node == 'M':
            check(fld in COMM, f'{where}: {fld} is not a communication kind')
        elif node == 'I':
            check(fld == 'X90003', f'{where}: {fld} is not the Aadhaar identification type')
        elif node == 'T':
            check(re.fullmatch(r'#\d+|[A-Z0-9]{4}', fld),
                  f'{where}: {fld} is neither a position nor a tax category')
        elif node == 'X':
            check(fld != '', f'{where}: a constant column carries no constant')
        elif node == '-':
            check(fld == '', f'{where}: a column that reads nothing carries a field name')
        else:
            fail.append(f'{where}: node {node} is not one the engine handles')
        check(hdr.strip() != '', f'{where}: the heading is blank')

    # --- 1a. the customer number -----------------------------------------
    # A downloaded row has to say which customer it is, or it cannot be read
    # back or edited. Three of the workbook's templates carry no such column
    # at all - they begin at the company code, because they were written for
    # creating a customer, where the number does not exist yet. Those are
    # named here so the gap is a recorded decision rather than an oversight.
    NO_CUSTOMER_COLUMN = {
        '43156406',   # 63 columns, the YSHP ship-to template
        '6e6467eb',   # 74 columns, YVSP / YVMI / YVTO
        'ab38ead5',   # 79 columns, the ZEXP export template
    }
    has_kunnr = collections.defaultdict(bool)
    for tmpl, col, hdr, node, fld, fmt in rows:
        if node == 'K' and fld == 'KUNNR':
            has_kunnr[tmpl] = True
    for tmpl in {t for t, *_ in rows}:
        if not has_kunnr[tmpl] and tmpl not in NO_CUSTOMER_COLUMN:
            fail.append(f'{tmpl}: no column carries the customer number, and it is not '
                        f'one of the templates recorded as having none')
    for tmpl in NO_CUSTOMER_COLUMN:
        if has_kunnr.get(tmpl):
            fail.append(f'{tmpl}: recorded as having no customer number column, but one '
                        f'is mapped - take it off the list')

    # --- 2. the shape of each template ------------------------------------
    by_tmpl = collections.defaultdict(list)
    for tmpl, col, *_ in rows:
        by_tmpl[tmpl].append(int(col))
    for tmpl, cols in by_tmpl.items():
        check(sorted(cols) == list(range(1, len(cols) + 1)),
              f'{tmpl}: the columns are not 1 to {len(cols)} without a gap or a repeat')

    # --- 3. every combination reaches a template --------------------------
    combis = COMBI.findall(SRC)
    check(combis, 'the program carries no combination table')
    seen = collections.Counter((r, l, k) for r, l, k, _ in combis)
    for key, n in seen.items():
        check(n == 1, f'{key[0]} {key[1]}/{key[2]} appears {n} times in the '
                      f'combination table')
    for regn, land, ktokd, tmpl in combis:
        check(tmpl in by_tmpl, f'{regn} {land}/{ktokd} points at template {tmpl}, '
                               f'which has no columns')

    # --- 4. the map still matches the workbook ----------------------------
    alias = {c['format']: RENAMED[c['sheet']]
             for c in REG['combinations'] if c['sheet'] in RENAMED}
    for sig, fmt in REG['formats'].items():
        name = alias.get(sig, sig)
        check(name in by_tmpl, f'template {name} is in the workbook but not in the program')
        if name in by_tmpl:
            check(len(by_tmpl[name]) == fmt['ncol'],
                  f'{name}: the workbook has {fmt["ncol"]} columns, the program '
                  f'{len(by_tmpl[name])}')
    # The heading the program writes is what the upload program matches on, so it
    # has to be the workbook's own heading, character for character.
    by_hdr = collections.defaultdict(dict)
    for tmpl, col, hdr, *_ in rows:
        by_hdr[tmpl][int(col)] = hdr.replace("''", "'")
    for sig, fmt in REG['formats'].items():
        name = alias.get(sig, sig)
        for i, desc in enumerate(fmt['desc'], 1):
            if not desc or name not in by_hdr:
                continue
            check(by_hdr[name].get(i) == desc,
                  f'{name} column {i}: the workbook says "{desc}", the program '
                  f'"{by_hdr[name].get(i)}"')

    want = {(c['region'], c['country'], c['ktokd'])
            for c in REG['combinations'] if c['country'] != '*'}
    have = {(r, l, k) for r, l, k, _ in combis}
    for miss in sorted(want - have):
        fail.append(f'{miss[0]} {miss[1]}/{miss[2]} is in the workbook but not '
                    f'in the program')
    for extra in sorted(have - want):
        fail.append(f'{extra[0]} {extra[1]}/{extra[2]} is in the program but not '
                    f'in the workbook')

    # The region and the account group must name exactly one template, which
    # is the whole reason the screen asks for a region rather than a country.
    by_rk = {}
    for r, l, k, t in combis:
        by_rk.setdefault((r, k), set()).add(t)
    for (r, k), t in sorted(by_rk.items()):
        check(len(t) == 1, f'{r}/{k} resolves to {len(t)} templates: {sorted(t)}')

    # Every region the dropdown offers has at least one template behind it,
    # and every region a combination names is in the dropdown.
    shown = set(re.findall(r"\(\s*regn = '(\w+)' text = '[^']*'\s*\)", SRC))
    used  = {r for r, _, _, _ in combis}
    for r in sorted(shown - used):
        fail.append(f'the dropdown offers region {r}, which has no template')
    for r in sorted(used - shown):
        fail.append(f'region {r} has templates but the dropdown does not offer it')
    for extra in sorted(have - want):
        fail.append(f'{extra[0]}/{extra[1]} is in the program but not in the workbook')

    if fail:
        print(f'{len(fail)} problem(s):')
        for f in fail[:40]:
            print('  -', f)
        return 1
    print(f'clean - {len(rows)} columns over {len(by_tmpl)} templates and '
          f'{len(combis)} combinations; every field exists on the structure it is read '
          f'from, and every heading is the workbook\'s own')
    return 0


if __name__ == '__main__':
    sys.exit(main())
