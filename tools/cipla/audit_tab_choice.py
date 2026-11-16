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
      f'{ties} with tied tabs, each settled on the region\'s own tab')
