"""Illustrations for the reference deck: selection screens, Excel formats and
a result list, drawn from the programs' own field texts and headings. All
sample values are invented - no data of the previous client appears."""
import os, sys
from PIL import Image, ImageDraw, ImageFont

OUT = sys.argv[1]
os.makedirs(OUT, exist_ok=True)
S = 2  # draw at 2x
F = '/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf'
FB = '/usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf'


def font(size, bold=False):
    return ImageFont.truetype(FB if bold else F, size * S)


# ---------------------------------------------------------------- SAP screen
BG = (242, 244, 247); FRAME = (160, 170, 182); TXT = (30, 34, 40)
TITLE_BG = (226, 232, 239); FIELD = (255, 255, 255); BLUE = (29, 78, 137)


def screen(name, title, blocks, w=900):
    # height from content
    h = 70
    for _t, rows in blocks:
        h += 34 + 26 * len(rows) + 14
    img = Image.new('RGB', (w * S, h * S), BG)
    d = ImageDraw.Draw(img)
    d.rectangle([0, 0, w * S, 34 * S], fill=TITLE_BG)
    d.text((14 * S, 8 * S), title, font=font(15, True), fill=TXT)
    # execute button
    d.ellipse([(w - 40) * S, 7 * S, (w - 20) * S, 27 * S], fill=(46, 139, 87))
    d.polygon([((w - 34) * S, 11 * S), ((w - 34) * S, 23 * S), ((w - 24) * S, 17 * S)], fill=(255, 255, 255))
    d.text(((w - 112) * S, 10 * S), 'Execute (F8)', font=font(12), fill=TXT)
    y = 48
    for btitle, rows in blocks:
        top = y
        bh = 22 + 26 * len(rows) + 6
        d.rectangle([12 * S, top * S, (w - 12) * S, (top + bh) * S], outline=FRAME, width=S)
        tw = d.textlength(btitle, font=font(12, True)) / S
        d.rectangle([24 * S, (top - 8) * S, (24 + tw + 12) * S, (top + 8) * S], fill=BG)
        d.text((30 * S, (top - 8) * S), btitle, font=font(12, True), fill=BLUE)
        ry = top + 16
        for row in rows:
            kind, label = row[0], row[1]
            x0 = 30
            if kind in ('radio', 'radio_on'):
                d.ellipse([x0 * S, (ry + 3) * S, (x0 + 13) * S, (ry + 16) * S], outline=TXT, width=S, fill=FIELD)
                if kind == 'radio_on':
                    d.ellipse([(x0 + 4) * S, (ry + 7) * S, (x0 + 9) * S, (ry + 12) * S], fill=TXT)
                d.text(((x0 + 22) * S, (ry + 2) * S), label, font=font(13), fill=TXT)
            elif kind in ('check', 'check_on'):
                d.rectangle([x0 * S, (ry + 3) * S, (x0 + 13) * S, (ry + 16) * S], outline=TXT, width=S, fill=FIELD)
                if kind == 'check_on':
                    d.line([((x0 + 3) * S, (ry + 9) * S), ((x0 + 6) * S, (ry + 13) * S), ((x0 + 11) * S, (ry + 5) * S)], fill=TXT, width=2 * S)
                d.text(((x0 + 22) * S, (ry + 2) * S), label, font=font(13), fill=TXT)
            elif kind in ('field', 'list', 'range'):
                val = row[2] if len(row) > 2 else ''
                fw = row[3] if len(row) > 3 else 220
                d.text((x0 * S, (ry + 2) * S), label, font=font(13), fill=TXT)
                fx = 300
                d.rectangle([fx * S, (ry + 1) * S, (fx + fw) * S, (ry + 19) * S], fill=FIELD, outline=FRAME, width=S)
                d.text(((fx + 5) * S, (ry + 3) * S), val, font=font(12), fill=TXT)
                if kind == 'list':
                    d.polygon([((fx + fw - 16) * S, (ry + 7) * S), ((fx + fw - 6) * S, (ry + 7) * S),
                               ((fx + fw - 11) * S, (ry + 13) * S)], fill=TXT)
                if kind == 'range':
                    d.text(((fx + fw + 10) * S, (ry + 2) * S), 'to', font=font(12), fill=(120, 120, 120))
                    d.rectangle([(fx + fw + 32) * S, (ry + 1) * S, (fx + 2 * fw + 32) * S, (ry + 19) * S], fill=FIELD, outline=FRAME, width=S)
            ry += 26
        y = top + bh + 20
    img = img.resize((w, h), Image.LANCZOS) if False else img
    img.save(os.path.join(OUT, name))


screen('cust_screen.png', 'Customer master template download / upload', [
    ('Download or upload', [('radio_on', 'Download a template'), ('radio', 'Upload a filled template')]),
    ('Template', [('radio_on', 'Customer create template'), ('radio', 'Customer extension template'),
                  ('radio', 'Block / unblock template')]),
    ('Country / region and account group', [('list', 'Country / region', 'India (IN)', 260),
                                             ('field', 'Customer account group', 'ZDOM', 90)]),
    ('Which customers', [('range', 'Business partner', '', 120), ('range', 'Customer', '1000123', 120),
                         ('field', 'Rows at most', '100', 60)]),
    ('File', [('field', 'File', 'C:\\temp\\IN_ZDOM.xlsx', 440), ('radio_on', 'On the PC'),
              ('radio', 'On the application server'), ('check', 'Template only (no data)')]),
])

screen('vend_screen.png', 'Business Partner / Supplier Master Download / Upload', [
    ('Download or upload', [('radio', 'Download a workbook'), ('radio_on', 'Upload a filled workbook')]),
    ('Scenario', [('radio_on', 'Vendor / BP creation - all CC'), ('radio', 'Withholding tax / TDS'),
                  ('radio', 'TAN exemption details'), ('radio', 'Bank key creation'),
                  ('radio', 'Vendor bank details'), ('radio', 'Vendor extension'),
                  ('radio', 'CIN details'), ('radio', 'Partner functions'), ('radio', 'Block / unblock')]),
    ('Upload options', [('check_on', 'Test run (nothing is posted)'), ('check', 'Stop at the first faulty row'),
                        ('field', 'Heading rows to skip', '1', 40)]),
    ('File', [('field', 'File', 'C:\\Users\\tester\\Vendor_creation.xlsx', 440), ('radio_on', 'On the PC'),
              ('radio', 'On the application server')]),
])


# ---------------------------------------------------------------- Excel sheet
GRID = (208, 215, 222); HEADBG = (233, 237, 241); HDRROW = (221, 235, 228)


def col_letter(n):
    s = ''
    while n:
        n, r = divmod(n - 1, 26)
        s = chr(65 + r) + s
    return s


def sheet(name, tab, cols, rows, widths):
    rowh, lead = 26, 34
    w = lead + sum(widths) + 1
    h = 26 + rowh * (len(rows) + 1) + 34
    img = Image.new('RGB', (w * S, h * S), (255, 255, 255))
    d = ImageDraw.Draw(img)
    # column letters
    d.rectangle([0, 0, w * S, 26 * S], fill=HEADBG)
    x = lead
    for (c, _), cw in zip(cols, widths):
        d.rectangle([x * S, 0, (x + cw) * S, 26 * S], outline=GRID, width=S)
        L = col_letter(c) if c else '...'
        tw = d.textlength(L, font=font(12)) / S
        d.text(((x + (cw - tw) / 2) * S, 6 * S), L, font=font(12), fill=(90, 90, 90))
        x += cw
    # rows
    allrows = [[hd for _, hd in cols]] + rows
    for r, vals in enumerate(allrows):
        y = 26 + r * rowh
        d.rectangle([0, y * S, lead * S, (y + rowh) * S], fill=HEADBG, outline=GRID, width=S)
        d.text((10 * S, (y + 6) * S), str(r + 1), font=font(12), fill=(90, 90, 90))
        x = lead
        for v, cw in zip(vals, widths):
            fill = HDRROW if r == 0 else (255, 255, 255)
            d.rectangle([x * S, y * S, (x + cw) * S, (y + rowh) * S], fill=fill, outline=GRID, width=S)
            f = font(11, r == 0)
            t = v
            while d.textlength(t, font=f) / S > cw - 8 and len(t) > 3:
                t = t[:-2] + '\u2026' if not t.endswith('\u2026') else t[:-2] + '\u2026'
            d.text(((x + 4) * S, (y + 7) * S), t, font=f, fill=TXT)
            x += cw
    # tab strip
    y = 26 + rowh * len(allrows) + 4
    tw = d.textlength(tab, font=font(12, True)) / S
    d.rectangle([(lead + 6) * S, y * S, (lead + 26 + tw) * S, (y + 24) * S], fill=(255, 255, 255), outline=GRID, width=S)
    d.text(((lead + 16) * S, (y + 5) * S), tab, font=font(12, True), fill=(33, 115, 70))
    img.save(os.path.join(OUT, name))


sheet('cust_excel.png', 'IN_ZDOM', [
    (1, 'Transaction Code'), (2, 'New Customer Code'), (3, 'Company Code'), (4, 'Sales Organization'),
    (0, ''), (7, 'Customer Account Group'), (0, ''), (15, 'Name 1'), (30, 'City'), (31, 'Country Key')],
    [['XD01', '', '1000', '1000', '', 'ZDOM', '', 'Sample Medical Stores', 'Pune', 'IN'],
     ['XD01', '', '1000', '1000', '', 'ZDOM', '', 'Demo Pharma Distributors', 'Nagpur', 'IN']],
    [112, 144, 104, 130, 34, 176, 34, 172, 70, 92])

sheet('vend_excel.png', 'Vendor creation for All CC', [
    (2, 'LIFNR'), (3, 'BUKRS'), (4, 'EKORG'), (5, 'KTOKK'), (7, 'NAME1'), (0, ''),
    (19, 'CITY1'), (20, 'COUNTRY'), (47, 'AKONT'), (61, 'WAERS')],
    [['', '1000', '1000', 'Z001', 'Sample Chemicals Ltd', '', 'Mumbai', 'IN', '300000', 'INR'],
     ['', '1000', '1000', 'Z001', 'Demo Packaging Co', '', 'Thane', 'IN', '300000', 'INR']],
    [70, 62, 62, 62, 160, 34, 78, 82, 74, 64])


# ---------------------------------------------------------------- result list
def result_list(name):
    cols = ['Status', 'Excel row', 'Vendor / key', 'Comp.code', 'Message']
    widths = [62, 76, 110, 84, 560]
    lines = [
        ((46, 139, 87), '2', '200000412', '1000', 'Test run OK - vendor would be created'),
        ((46, 139, 87), '3', '200000413', '1000', 'Test run OK - vendor would be created'),
        ((222, 160, 30), '4', '200000414', '2000', 'Reference company code 2000 holds no data for this vendor'),
        ((200, 50, 50), '5', '200000415', '3000', 'Vendor 200000415 is not extended to company code 3000 - extend it first'),
        ((60, 110, 170), '0', '', '', '3 column(s) sit elsewhere in this file than in the template'),
    ]
    w = sum(widths) + 2
    h = 40 + 26 * (len(lines) + 1) + 6
    img = Image.new('RGB', (w * S, h * S), (255, 255, 255))
    d = ImageDraw.Draw(img)
    d.rectangle([0, 0, w * S, 36 * S], fill=TITLE_BG)
    d.text((10 * S, 9 * S), 'TEST RUN - nothing was posted     Rows OK: 2     Rows with errors: 1',
           font=font(13, True), fill=TXT)
    y = 40
    x = 0
    for c, cw in zip(cols, widths):
        d.rectangle([x * S, y * S, (x + cw) * S, (y + 26) * S], fill=HEADBG, outline=GRID, width=S)
        d.text(((x + 6) * S, (y + 6) * S), c, font=font(12, True), fill=TXT)
        x += cw
    for i, (col, r, k, b, m) in enumerate(lines):
        y = 40 + 26 * (i + 1)
        x = 0
        for j, cw in enumerate(widths):
            d.rectangle([x * S, y * S, (x + cw) * S, (y + 26) * S], fill=(255, 255, 255), outline=GRID, width=S)
            if j == 0:
                cx = x + cw / 2
                if col == (60, 110, 170):
                    d.ellipse([(cx - 7) * S, (y + 6) * S, (cx + 7) * S, (y + 20) * S], fill=col)
                    d.text(((cx - 2) * S, (y + 6) * S), 'i', font=font(11, True), fill=(255, 255, 255))
                else:
                    d.ellipse([(cx - 7) * S, (y + 6) * S, (cx + 7) * S, (y + 20) * S], fill=col)
            else:
                v = [None, r, k, b, m][j]
                d.text(((x + 6) * S, (y + 6) * S), v, font=font(12), fill=TXT)
            x += cw
    img.save(os.path.join(OUT, name))


result_list('result_list.png')
for f in sorted(os.listdir(OUT)):
    im = Image.open(os.path.join(OUT, f))
    print(f, im.size)
