"""What the extractor writes, the upload programs read.

Builds the heading row ZBCS_MASS_UPLOAD_EXTRACT would write for every
scenario, packs it into a real .xlsx exactly as the program does, and then
runs each upload program's own tab-and-column resolution over it. Every
column of every scenario has to come back bound.
"""
import html, re, sys, zipfile, collections, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__)))
sys.path.insert(0, '/tmp/claude-0/-home-user-ONGC-CST-Purchase-Date-Sharing-/0a870517-dcc5-5a06-9069-d731d41a85f0/scratchpad')
from sim import sheets, squash, load_cust, load_vend
from audit_mangle import bind

EX = open('src/zbcs_mass_upload_extract.prog.abap', encoding='utf-8').read()

def cols(scen):
    out = {}
    for m in re.finditer(r"\(\s*scen = '(%s)' col = (\d+)\s+hdr = '((?:[^']|'')*)' node" % scen, EX):
        out[int(m.group(2))] = m.group(3).replace("''", "'")
    return out

def workbook(path, sheet, head):
    def letter(n):
        s = ''
        while n > 0:
            n, r = divmod(n - 1, 26); s = chr(65 + r) + s
        return s
    esc = lambda t: html.escape(t, quote=True).replace("'", "&apos;")
    row = '<row r="1">' + ''.join(
        f'<c r="{letter(i)}1" t="inlineStr"><is><t xml:space="preserve">{esc(v)}</t></is></c>'
        for i, v in enumerate(head, 1) if v) + '</row>'
    ws = ('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><worksheet xmlns='
          '"http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData>' + row +
          '</sheetData></worksheet>')
    with zipfile.ZipFile(path, 'w', zipfile.ZIP_DEFLATED) as z:
        z.writestr('[Content_Types].xml', '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="xml" ContentType="application/xml"/><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/><Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/></Types>')
        z.writestr('_rels/.rels', '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>')
        z.writestr('xl/workbook.xml', f'<?xml version="1.0" encoding="UTF-8" standalone="yes"?><workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets><sheet name="{esc(sheet)}" sheetId="1" r:id="rId1"/></sheets></workbook>')
        z.writestr('xl/_rels/workbook.xml.rels', '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/></Relationships>')
        z.writestr('xl/worksheets/sheet1.xml', ws)

SHEET = dict(re.findall(r"WHEN '([CV]\d)' THEN '([^']*)'", EX))
cm, vm = load_cust(), load_vend()
bad = []
tmp = '/tmp/_roundtrip.xlsx'
for scen in [f'C{i}' for i in range(1, 8)] + [f'V{i}' for i in range(1, 10)]:
    c = cols(scen)
    if not c:
        bad.append(f'{scen}: no columns in the extractor map'); continue
    head = [''] * max(c)
    for col, hdr in c.items():
        head[col - 1] = hdr
    workbook(tmp, SHEET.get(scen, 'Sheet1'), head)
    ents = (cm if scen[0] == 'C' else vm)['R' + scen[1]]
    rows = dict(sheets(tmp))[SHEET.get(scen, 'Sheet1')]
    src, used, bycol, known = bind([dict(e) for e in ents], rows[1], scen[0] == 'C')
    miss = [ents[i]['fld'] or ents[i]['hdr'] for i in range(len(ents)) if i not in src]
    flag = '' if not miss else '  <-- ' + ', '.join(miss[:6])
    print(f'  {scen}  {SHEET.get(scen,""):<28} {len(src):>3}/{len(ents):<3} columns bind{flag}')
    if miss:
        bad.append(f'{scen}: {len(miss)} column(s) would not bind: {", ".join(miss[:8])}')
if os.path.exists(tmp):
    os.remove(tmp)
print()
print('\n'.join(bad) if bad else 'clean - every column of every scenario is read back by the upload programs')
sys.exit(1 if bad else 0)
