"""What the extractor writes, the upload programs read.

Builds the heading row ZBCS_MASS_UPLOAD_EXTRACT would write for every
scenario, packs it into a real .xlsx exactly as the program does, and then
runs each upload program's own tab-and-column resolution over it. Every
column of every scenario has to come back bound.
"""
import html, re, sys, zipfile, collections, os
sys.path.insert(0, os.path.join(os.path.dirname(__file__)))
from sim import sheets, squash, load_cust, load_vend
from audit_column_mangle import bind

EX = open('src/zbcs_mass_upload_extract.prog.abap', encoding='utf-8').read()

def cols(scen):
    out = {}
    for m in re.finditer(r"\(\s*scen = '(%s)' col = (\d+)\s+hdr = '((?:[^']|'')*)' node" % scen, EX):
        out[int(m.group(2))] = m.group(3).replace("''", "'")
    return out

def workbook(path, sheet, head, rows=()):
    """Builds the package exactly as LCL_XLSX does - shared strings and all
       nine parts - so the round trip is run over the shape SAP will read."""
    def letter(n):
        s = ''
        while n > 0:
            n, r = divmod(n - 1, 26); s = chr(65 + r) + s
        return s
    esc = lambda t: html.escape(t, quote=True).replace("'", "&apos;")

    order, index, used = [], {}, 0
    def si(text):
        nonlocal used
        used += 1
        if text not in index:
            index[text] = len(order); order.append(text)
        return index[text]

    body, wide = '', len(head)
    for rn, cells in enumerate([list(head)] + [list(r) for r in rows], start=1):
        wide = max(wide, len(cells))
        # Column A is written empty or not, exactly as LCL_XLSX does: a row
        # that starts at B comes back one column short of the template.
        body += f'<row r="{rn}">' + ''.join(
            f'<c r="{letter(i)}{rn}" t="s"><v>{si(v)}</v></c>'
            for i, v in enumerate(cells, 1) if v or i == 1) + '</row>'
    dim = f'A1:{letter(max(wide, 1))}{len(rows) + 1}'

    M = 'http://schemas.openxmlformats.org/spreadsheetml/2006/main'
    R = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships'
    P = 'http://schemas.openxmlformats.org/package/2006/relationships'
    X = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'

    ws = (f'{X}<worksheet xmlns="{M}" xmlns:r="{R}"><dimension ref="{dim}"/>'
          '<sheetViews><sheetView tabSelected="1" workbookViewId="0"/></sheetViews>'
          f'<sheetFormatPr defaultRowHeight="15"/><sheetData>{body}</sheetData></worksheet>')
    sst = (f'{X}<sst xmlns="{M}" count="{used}" uniqueCount="{len(order)}">' +
           ''.join(f'<si><t xml:space="preserve">{esc(t)}</t></si>' for t in order) + '</sst>')
    sty = (f'{X}<styleSheet xmlns="{M}">'
           '<fonts count="1"><font><sz val="11"/><name val="Calibri"/><family val="2"/></font></fonts>'
           '<fills count="2"><fill><patternFill patternType="none"/></fill>'
           '<fill><patternFill patternType="gray125"/></fill></fills>'
           '<borders count="1"><border><left/><right/><top/><bottom/><diagonal/></border></borders>'
           '<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>'
           '<cellXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/></cellXfs>'
           '<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>'
           '</styleSheet>')
    ct = (f'{X}<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">'
          '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>'
          '<Default Extension="xml" ContentType="application/xml"/>'
          '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>'
          '<Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>'
          '<Override PartName="/xl/sharedStrings.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/>'
          '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>'
          '<Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>'
          '<Override PartName="/docProps/app.xml" ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>'
          '</Types>')
    rels = (f'{X}<Relationships xmlns="{P}">'
            f'<Relationship Id="rId1" Type="{R}/officeDocument" Target="xl/workbook.xml"/>'
            '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>'
            f'<Relationship Id="rId3" Type="{R}/extended-properties" Target="docProps/app.xml"/>'
            '</Relationships>')
    wb = (f'{X}<workbook xmlns="{M}" xmlns:r="{R}"><bookViews><workbookView/></bookViews>'
          f'<sheets><sheet name="{esc(sheet)}" sheetId="1" r:id="rId1"/></sheets></workbook>')
    wbr = (f'{X}<Relationships xmlns="{P}">'
           f'<Relationship Id="rId1" Type="{R}/worksheet" Target="worksheets/sheet1.xml"/>'
           f'<Relationship Id="rId2" Type="{R}/styles" Target="styles.xml"/>'
           f'<Relationship Id="rId3" Type="{R}/sharedStrings" Target="sharedStrings.xml"/>'
           '</Relationships>')
    core = (f'{X}<cp:coreProperties '
            'xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" '
            'xmlns:dc="http://purl.org/dc/elements/1.1/" '
            'xmlns:dcterms="http://purl.org/dc/terms/" '
            'xmlns:dcmitype="http://purl.org/dc/dcmitype/" '
            'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'
            '<dc:creator>ZBCS_MASS_UPLOAD_EXTRACT</dc:creator>'
            '<cp:lastModifiedBy>ZBCS_MASS_UPLOAD_EXTRACT</cp:lastModifiedBy></cp:coreProperties>')
    app = (f'{X}<Properties '
           'xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties" '
           'xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">'
           '<Application>SAP</Application></Properties>')

    with zipfile.ZipFile(path, 'w', zipfile.ZIP_DEFLATED) as z:
        z.writestr('[Content_Types].xml', ct)
        z.writestr('_rels/.rels', rels)
        z.writestr('docProps/core.xml', core)
        z.writestr('docProps/app.xml', app)
        z.writestr('xl/workbook.xml', wb)
        z.writestr('xl/_rels/workbook.xml.rels', wbr)
        z.writestr('xl/styles.xml', sty)
        z.writestr('xl/sharedStrings.xml', sst)
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
