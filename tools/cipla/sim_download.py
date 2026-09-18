"""Build a workbook the way the program builds it, and read it back.

Mirrors LCL_XLSX=>BUILD - the same nine parts, the same shared string table, the
same rule that column A is always written - so that the file the program produces
can be proved readable, and every heading proved to come back in the column the
template puts it in, without an SAP system.
"""
import json, os, re, sys, zipfile, io, collections

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
SRC  = open(os.path.join(ROOT, 'src/zsds_cust_tmpl_download.prog.abap'), encoding='utf-8').read()
ROW  = re.compile(r"\(\s*tmpl = '(\w+)'\s+col = (\d+)\s+hdr = '(.*?)'\s+node = '(.)'\s+"
                  r"fld = '(.*?)'\s+fmt = '(\w*)'\s*\)")


def col_letter(n):
    s = ''
    while n > 0:
        n, r = divmod(n - 1, 26)
        s = chr(65 + r) + s
    return s


def esc(t):
    for a, b in [('&', '&amp;'), ('<', '&lt;'), ('>', '&gt;'),
                 ('"', '&quot;'), ("'", '&apos;')]:
        t = t.replace(a, b)
    return t


def build(sheet, head, rows):
    si, txt, use = {}, [], 0

    def idx(t):
        nonlocal use
        use += 1
        if t not in si:
            si[t] = len(txt)
            txt.append(t)
        return si[t]

    def row_xml(cells, r):
        out = f'<row r="{r}">'
        for i, c in enumerate(cells, 1):
            if not c and i > 1:          # column A is always written
                continue
            out += f'<c r="{col_letter(i)}{r}" t="s"><v>{idx(c)}</v></c>'
        return out + '</row>'

    body = row_xml(head, 1)
    wide = len(head)
    for n, r in enumerate(rows, 2):
        body += row_xml(r, n)
        wide = max(wide, len(r))
    dim = f'A1:{col_letter(max(wide, 1))}{len(rows) + 1}'

    ws = ('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
          '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" '
          'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">'
          f'<dimension ref="{dim}"/>'
          '<sheetViews><sheetView tabSelected="1" workbookViewId="0"/></sheetViews>'
          '<sheetFormatPr defaultRowHeight="15"/>'
          f'<sheetData>{body}</sheetData></worksheet>')
    sst = ('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
           '<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" '
           f'count="{use}" uniqueCount="{len(txt)}">'
           + ''.join(f'<si><t xml:space="preserve">{esc(t)}</t></si>' for t in txt)
           + '</sst>')
    sty = ('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
           '<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">'
           '<fonts count="1"><font><sz val="11"/><name val="Calibri"/><family val="2"/></font></fonts>'
           '<fills count="2"><fill><patternFill patternType="none"/></fill>'
           '<fill><patternFill patternType="gray125"/></fill></fills>'
           '<borders count="1"><border><left/><right/><top/><bottom/><diagonal/></border></borders>'
           '<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>'
           '<cellXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/></cellXfs>'
           '<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>'
           '</styleSheet>')
    types = ('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
             '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">'
             '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>'
             '<Default Extension="xml" ContentType="application/xml"/>'
             '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>'
             '<Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>'
             '<Override PartName="/xl/sharedStrings.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml"/>'
             '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>'
             '<Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>'
             '<Override PartName="/docProps/app.xml" ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>'
             '</Types>')
    rels = ('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
            '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
            '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>'
            '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>'
            '<Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties" Target="docProps/app.xml"/>'
            '</Relationships>')
    wb = ('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
          '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" '
          'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">'
          '<bookViews><workbookView/></bookViews>'
          f'<sheets><sheet name="{esc(sheet)}" sheetId="1" r:id="rId1"/></sheets></workbook>')
    wbr = ('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
           '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
           '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>'
           '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>'
           '<Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/sharedStrings" Target="sharedStrings.xml"/>'
           '</Relationships>')
    core = ('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
            '<cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" '
            'xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/" '
            'xmlns:dcmitype="http://purl.org/dc/dcmitype/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'
            '<dc:creator>ZSDS_CUST_TMPL_DOWNLOAD</dc:creator>'
            '<cp:lastModifiedBy>ZSDS_CUST_TMPL_DOWNLOAD</cp:lastModifiedBy></cp:coreProperties>')
    app = ('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
           '<Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties" '
           'xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">'
           '<Application>SAP</Application></Properties>')

    buf = io.BytesIO()
    with zipfile.ZipFile(buf, 'w', zipfile.ZIP_DEFLATED) as z:
        for n, c in [('[Content_Types].xml', types), ('_rels/.rels', rels),
                     ('docProps/core.xml', core), ('docProps/app.xml', app),
                     ('xl/workbook.xml', wb), ('xl/_rels/workbook.xml.rels', wbr),
                     ('xl/styles.xml', sty), ('xl/sharedStrings.xml', sst),
                     ('xl/worksheets/sheet1.xml', ws)]:
            z.writestr(n, c.encode('utf-8'))
    return buf.getvalue()


def main():
    import openpyxl
    rows = ROW.findall(SRC)
    by = collections.defaultdict(dict)
    for tmpl, col, hdr, node, fld, fmt in rows:
        by[tmpl][int(col)] = hdr.replace("''", "'")

    bad = 0
    for tmpl, cols in sorted(by.items()):
        head = [cols.get(i, '') for i in range(1, max(cols) + 1)]
        # one data row, so the round trip is proved with content as well as headings
        data = [[f'v{i}' if i % 3 else '' for i in range(1, len(head) + 1)]]
        blob = build(tmpl, head, data)
        wb = openpyxl.load_workbook(io.BytesIO(blob), data_only=True)
        ws = wb.worksheets[0]
        back = [ws.cell(1, i).value or '' for i in range(1, len(head) + 1)]
        if back != head:
            bad += 1
            for i, (a, b) in enumerate(zip(head, back), 1):
                if a != b:
                    print(f'  {tmpl} column {i}: wrote "{a}", read back "{b}"')
                    break
        back2 = [ws.cell(2, i).value or '' for i in range(1, len(head) + 1)]
        if back2 != data[0]:
            bad += 1
            print(f'  {tmpl}: the data row does not come back in the same columns')
    if bad:
        print(f'{bad} template(s) failed the round trip')
        return 1
    print(f'clean - all {len(by)} templates write a readable workbook and every heading '
          f'and value comes back in the column it was written to')
    return 0


if __name__ == '__main__':
    sys.exit(main())
