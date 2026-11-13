"""Cipla INC's own workbook reads into the right fields.

Runs the reader and BIND_COLUMNS of ZSDS_CUST_TMPL_DOWNLOAD, as the ABAP does
them, on docs/cipla/customer_template_cipla_inc.xlsx - the file Cipla USA
sent on 13 November 2026 - for the three account groups of region CI:

  - the tab chosen is the tab for that account group,
  - every column of the template is found by its heading or its field name,
    none is read by position, left empty as ambiguous, or blanked,
  - where the tab has a technical row, the column found carries the field
    the map writes to.

    python3 tools/cipla/audit_cipla_inc.py
"""
import collections, os, re, sys
import openpyxl

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
SRC = open(os.path.join(ROOT, 'src/zsds_cust_tmpl_download.prog.abap'), encoding='utf-8').read()
BOOK = os.path.join(ROOT, 'docs/cipla/customer_template_cipla_inc.xlsx')

# account group, tab, technical row (None: descriptions only), data row
CASES = [('YVSP', 'INC - YVSP', 5, 9), ('YSHP', 'INC-YSHP', 4, 8), ('YVMI', 'INC-YVMI', None, 2)]
# Columns the program's template has and Cipla's tab does not.
ABSENT = {('YVSP', 'LOCKB')}       # added after Incident 542; the INC tab predates it
# The tab's technical name where the map's field differs only in spelling.
ALIAS = {'TCODE': 'XD01', 'USE_ZAV': 'X', 'TITLE_MEDI': 'TITLE', 'NAME1': 'NAME',
         'NAME2': 'NAME_2', 'NAME3': 'NAME_3', 'NAME4': 'NAME_4', 'NAME_CO': 'C_O_NAME',
         'CITY1': 'CITY', 'CITY2': 'DISTRICT', 'POST_CODE1': 'POSTL_COD1',
         'TEL_NUMBER': 'TEL', 'MOB_NUMBER': 'MOB', 'FAX_NUMBER': 'FAX', 'SMTP_ADDR': 'SMT',
         'ZTERM1': 'ZTERM', 'TAXKD_01': '#1', 'TAXKD_02': '#2', 'TAXKD_03': '#3',
         'TAXKD_04': '#4'}
ALIAS = {re.sub(r'[^A-Z0-9]', '', k): v for k, v in ALIAS.items()}
TAB = {'C': 'KNA1', 'B': 'KNB1', 'S': 'KNVV', 'Z': 'ZSD_LICENSE_CHK'}
ROW = re.compile(r"\(\s*tmpl = '(\w+)'\s+col = (\d+)\s+hdr = '((?:[^']|'')*)'\s+node = '(.)'\s+"
                 r"fld = '(.*?)'\s+fmt = '\w*'\s+cnv = '\w*'\s*\)")
COMBI = re.compile(r"\(\s*regn = 'CI'\s+land = '\w+'\s+ktokd = '(\w+)'\s+tmpl = '(\w+)'\s*\)")


def sq(x):
    return re.sub(r'[^A-Z0-9]', '', str(x if x is not None else '').upper())[:40]


def tab_key(m):
    return sq(f"{TAB[m['node']]}-{m['fld']}") if m['node'] in TAB and m['fld'] else ''


def fld_keys(mp):
    c = collections.Counter(sq(m['fld']) for m in mp if sq(m['fld']))
    return {k for k, n in c.items() if n == 1}


def cell(v):
    if v is None:
        return ''
    if isinstance(v, float) and v.is_integer():
        return str(int(v))
    return str(v)


wb = openpyxl.load_workbook(BOOK, data_only=True)
SHEETS = {ws.title: {r: [cell(ws.cell(r, c).value) for c in range(1, ws.max_column + 1)]
                     for r in range(1, min(ws.max_row, 12) + 1)} for ws in wb.worksheets}
MAPS = collections.defaultdict(list)
for t, c, h, n, f in ROW.findall(SRC):
    MAPS[t].append(dict(col=int(c), hdr=sq(h.replace("''", "'")[:60]), node=n, fld=f))
CI = dict(COMBI.findall(SRC))

fail = []
for ktokd, tabname, techr, datar in CASES:
    tmpl = CI.get(ktokd)
    if not tmpl:
        fail.append(f'region CI offers no {ktokd}')
        continue
    mp = [dict(m) for m in MAPS[tmpl]]
    want = ({m['hdr'] for m in mp if m['hdr']} | fld_keys(mp)
            | {tab_key(m) for m in mp if tab_key(m)})

    # -- LCL_EXCEL=>READ: the line, over every tab, with the most headings --
    best = (0, None, None)
    for name, rows in SHEETS.items():
        for r in sorted(rows)[:10]:
            sc = len(want & {sq(v) for v in rows[r] if sq(v)})
            if sc > best[0]:
                best = (sc, name, r)
    _, tab, hrow = best
    if tab != tabname:
        fail.append(f'{ktokd}: the upload reads tab "{tab}", not "{tabname}"')
        continue
    head = list(SHEETS[tab][hrow])
    nxt = SHEETS[tab].get(hrow + 1, [])
    if want & {sq(v) for v in nxt if sq(v)}:
        for i, v in enumerate(nxt):
            if v and i < len(head) and not head[i]:
                head[i] = v

    # -- BIND_COLUMNS --------------------------------------------------------
    cnt, occ, first = collections.Counter(), {}, {}
    for i, h in enumerate(head, 1):
        k = sq(h)
        if k:
            cnt[k] += 1
            occ[(k, cnt[k])] = i
            first.setdefault(k, i)
    mcnt = collections.Counter(m['hdr'] for m in mp if m['hdr'])
    seen, done, used = collections.Counter(), set(), set()
    for ix, m in enumerate(mp):
        k = m['hdr']
        if not k:
            continue
        seen[k] += 1
        if k not in cnt:
            continue
        if cnt[k] < mcnt[k]:
            fail.append(f'{ktokd}: "{k}" is repeated less often on the tab than in the '
                        f'template - {m["fld"]} is left empty')
            done.add(ix)
            continue
        if (k, seen[k]) in occ:
            m['col'] = occ[(k, seen[k])]
            done.add(ix)
            used.add(m['col'])
    fk = fld_keys(mp)
    for ix, m in enumerate(mp):
        if ix in done or not sq(m['fld']):
            continue
        key = tab_key(m) if tab_key(m) in cnt else sq(m['fld'])
        if key not in cnt or (key == sq(m['fld']) and key not in fk):
            continue
        if cnt[key] == 1 and first[key] not in used:
            m['col'] = first[key]
            done.add(ix)
            used.add(m['col'])

    tech = SHEETS[tab][techr] if techr else []
    for ix, m in enumerate(mp):
        if ix not in done:
            if (ktokd, m['fld']) not in ABSENT:
                fail.append(f'{ktokd}: {m["node"]}/{m["fld"]} (template column {m["col"]}) is '
                            f'not found on tab "{tab}" by heading or field name')
            continue
        if tech and m['node'] not in ('-', 'N'):
            t = sq(tech[m['col'] - 1]) if m['col'] <= len(tech) else ''
            if t and sq(ALIAS.get(t, t)) != sq(m['fld']):
                fail.append(f'{ktokd}: {m["node"]}/{m["fld"]} is read from column {m["col"]}, '
                            f'which the tab names {t}')
    for f in sorted(f for k, f in ABSENT if k == ktokd):
        if f in head or any(m['fld'] == f and mp.index(m) in done for m in mp):
            fail.append(f'{ktokd}: tab "{tab}" now carries {f} - take it out of ABSENT')

# -- a row of another account group's layout is not read with this one -----
m = re.search(r'\n  METHOD master\.\n(.*?)\n  ENDMETHOD\.', SRC, re.S)
if not m or not re.search(r"lv_ktokd <> p_ktokd\.\s*\n\s*DATA\(lv_rtmpl\) = lcl_tmpl=>resolve\("
                          r"[^)]*\)\.\s*\n\s*IF lv_rtmpl <> mv_tmpl\.\s*\n\s*mo_log->add\("
                          r"[^)]*iv_type = 'E'", m.group(1)):
    fail.append('upload: a row whose account group has another layout than the one selected '
                'is read with the selected one - YSHP rows in the YVSP file land in wrong fields')

if fail:
    print(f'{len(fail)} problem(s):')
    for f in fail:
        print('  -', f)
    sys.exit(1)
print('clean - Cipla INC\'s YVSP, YSHP and YVMI tabs: every template column is found by its '
      'heading or field name and lands on the field the tab names')
