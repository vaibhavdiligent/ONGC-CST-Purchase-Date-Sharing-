"""Uploading Cipla's master workbook reads each region's own tab.

Several tabs of docs/cipla/customer_code_templates.xlsx share a layout -
Australia and Morocco (ZDOM), Kenya and SAGA, Exelan and Invagen - and the
reader used to take the first of equally good tabs, so Morocco's upload read
Australia's tab. LCL_EXCEL=>READ now breaks a tie by the region's name and
its workbook tab (LCL_TMPL=>REGION_HINTS), and stops on a tie nothing
resolves. For every region and account group this runs that choice on the
workbook and requires: no tie left unresolved, and a tie that includes the
region's own tab settled on it.

    python3 tools/cipla/audit_tab_choice.py
"""
import os, sys, runpy

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
g = runpy.run_path(os.path.join(ROOT, 'tools/cipla/audit_cipla_inc.py'), run_name='lib')
import openpyxl, collections, json

REG = json.load(open(os.path.join(ROOT, 'docs/cipla/customer_template_registry.json')))
SHEET_OF = {r['region']: r['sheet'] for r in REG['regions']}
wb = openpyxl.load_workbook(os.path.join(ROOT, 'docs/cipla/customer_code_templates.xlsx'),
                            data_only=True)
sheets = {ws.title: {r: [g['cell'](ws.cell(r, c).value) for c in range(1, ws.max_column + 1)]
                     for r in range(1, min(ws.max_row, 10) + 1)} for ws in wb.worksheets}

fail, checked, ties = [], 0, 0
for (regn, ktokd), tmpl in sorted(g['CI'].items()):
    own = SHEET_OF.get(regn)
    if own not in sheets:
        continue                 # Cipla INC: a workbook of its own
    mp = g['MAPS'][tmpl]
    c = collections.Counter(g['sq'](m['fld']) for m in mp if g['sq'](m['fld']))
    want = ({m['hdr'] for m in mp if m['hdr']} | {k for k, n in c.items() if n == 1}
            | {g['tab_key'](m) for m in mp if g['tab_key'](m)})
    per = {n: max(len(want & {g['sq'](v) for v in row if g['sq'](v)}) for row in rows.values())
           for n, rows in sheets.items()}
    top = max(per.values())
    tied = [n for n, v in per.items() if v == top and top > 0]
    tab, _, why = g['choose_tab'](sheets, want, g['HINTS'].get(regn, []))
    checked += 1
    if len(tied) > 1:
        ties += 1
        if why:
            fail.append(f'{regn}/{ktokd}: {why}')
        elif own in tied and tab != own:
            fail.append(f'{regn}/{ktokd}: tabs {tied} tie and "{tab}" is read, not "{own}"')

# -- a file of another template is refused (LCL_MAIN=>OTHER_FIT) -----------
# Cipla's block / unblock file was uploaded with "Customer extension" chosen:
# five headings matched, the rest were read by position, and the posting
# block "X" became the account group. Every template's own file must fit
# its own template at least as well as Block / unblock, Customer extension,
# Credit limit or the creation template; the block file must not fit the
# extension template.
CRED = ['CUSTOMERCODE', 'KKBER', 'KLIMG', 'KLIME', 'WAERS', 'KLIMK', 'CTLPC', 'CRBLB',
        'PAYMENTTERMS', 'CUSTGRP3', 'Z1INTERESTINDICATORINTERESTCYCLE']


def want_of(tmpl):
    if tmpl == 'CREDIT':
        return set(CRED)
    mp = g['MAPS'][tmpl]
    cc = collections.Counter(g['sq'](m['fld']) for m in mp if g['sq'](m['fld']))
    return ({m['hdr'] for m in mp if m['hdr']} | {k for k, n in cc.items() if n == 1}
            | {g['tab_key'](m) for m in mp if g['tab_key'](m)})


def other_fit(head, tmpl, crt=None):
    keys = {g['sq'](h) for h in head if g['sq'](h)}
    own = len(want_of(tmpl) & keys)
    best, name = 0, ''
    for t in ['BLOCK', 'EXTN', 'CREDIT'] + ([crt] if crt else []):
        if t == tmpl:
            continue
        sc = len(want_of(t) & keys)
        if sc > own and sc > best:
            best, name = sc, t
    return name


def heading_line(rows, want):
    best = (0, [])
    for r in sorted(rows)[:10]:
        sc = len(want & {g['sq'](v) for v in rows[r] if g['sq'](v)})
        if sc > best[0]:
            best = (sc, rows[r])
    return best[1]


# Each layout's own heading rows, as the registry records them: a tab such
# as India holds several layouts one under another, and a user's file for
# one of them carries that layout's headings, not the first on the tab.
fits = 0
for c in REG['combinations']:
    ws = wb[c['sheet']] if c['sheet'] in wb.sheetnames else None
    tmpl = g['CI'].get((c['region'], c['ktokd']))
    if ws is None or not tmpl:
        continue
    for r in [x for x in (c.get('tech_row'), c.get('desc_row')) if x]:
        head = [g['cell'](ws.cell(r, cc).value) for cc in range(1, ws.max_column + 1)]
        fits += 1
        wrong = other_fit(head, tmpl)
        if wrong:
            fail.append(f'{c["region"]}/{c["ktokd"]}: its own layout ("{c["sheet"]}" row {r}) '
                        f'would be refused as the {wrong} template')
for tmpl, tab in (('EXTN', 'cust extn'), ('BLOCK', 'block unblock')):
    head = heading_line(sheets[tab], want_of(tmpl))
    fits += 1
    wrong = other_fit(head, tmpl)
    if wrong:
        fail.append(f'{tmpl}: its own tab "{tab}" would be refused as the {wrong} template')
for (regn, ktokd, book, tabname, techr, datar) in g['CASES']:
    tmpl = g['CI'][(regn, ktokd)]
    head = heading_line(g['BOOKS'][book][tabname], want_of(tmpl))
    fits += 1
    wrong = other_fit(head, tmpl)
    if wrong:
        fail.append(f'{regn}/{ktokd}: "{tabname}" would be refused as the {wrong} template')
_bw = openpyxl.load_workbook(os.path.join(ROOT, 'docs/cipla/customer_block_unblock_crs.xlsx'),
                             data_only=True).worksheets[0]
_bh = [g['cell'](_bw.cell(1, c).value) for c in range(1, _bw.max_column + 1)]
if other_fit(_bh, 'EXTN') != 'BLOCK':
    fail.append('the CRS block file uploaded as "Customer extension" is not refused as the '
                'Block / unblock template - its posting block X is read as the account group')
if other_fit(_bh, 'BLOCK'):
    fail.append('the CRS block file is refused when Block / unblock is chosen')

# -- and the ABAP does what was just simulated ------------------------------
import re
SRC = g['SRC']
rd = re.search(r'\n  METHOD read\.\n(.*?)\n  ENDMETHOD\.', SRC, re.S)
rd = rd.group(1) if rd else ''
if not re.search(r'DELETE lt_tsc WHERE sc < lv_best', rd):
    fail.append('LCL_EXCEL=>READ keeps no list of equally good tabs - the first one wins')
if not re.search(r'IF lv_tn CS lv_hn\.', rd):
    fail.append('LCL_EXCEL=>READ does not break a tie by the region in the tab name')
if not re.search(r"IF lines\( lt_hinted \) <> 1\.[\s\S]{0,400}RAISE EXCEPTION TYPE lcx_dl", rd):
    fail.append('LCL_EXCEL=>READ guesses on a tie nothing resolves instead of stopping')
if not re.search(r"DATA\(lv_fit\) = other_fit\( it_head = lt_head iv_tmpl = lv_tmpl \)\.\s*\n"
                 r"\s*IF lv_fit IS NOT INITIAL\.\s*\n\s*lo_log->add\([^)]*iv_type = 'E'[\s\S]{0,400}?"
                 r"RETURN\.\s*\n\s*ENDIF\.\s*\n\s*\n\s*lo_eng->bind_columns", SRC):
    fail.append('the upload reads a file of another template (the block file as "Customer '
                'extension") instead of refusing it')
if not re.search(r"lt_cand = VALUE #\( \( tmpl = gc_tmpl_blk[^)]*\)\s*\n\s*\( tmpl = gc_tmpl_extn"
                 r"[^)]*\)\s*\n\s*\( tmpl = gc_tmpl_cred", SRC):
    fail.append('LCL_MAIN=>OTHER_FIT does not compare with Block / unblock, extension and credit')
if 'it_hint    = lcl_tmpl=>region_hints( p_regn )' not in SRC:
    fail.append('the customer upload hands the reader no region hints')
if not re.search(r"regn = 'MA' text = '[^']*' tab = 'Moroccco'", SRC):
    fail.append("the region table does not carry each region's workbook tab")

if fail:
    print(f'{len(fail)} problem(s):')
    for f in fail:
        print('  -', f)
    sys.exit(1)
print(f'clean - {checked} region and account-group uploads of the master workbook, '
      f'{ties} with tied tabs, each settled on the region\'s own tab; {fits} templates\' own '
      f'files fit them best, and the CRS block file is refused as "Customer extension"')
