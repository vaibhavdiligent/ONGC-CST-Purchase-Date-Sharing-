"""Every template column whose field has an ALPHA domain is converted.

A numeric value in an ALPHA field is stored with leading zeros - trading
partner 1000 is 001000 - so a column that writes one without the AL (or GL)
marker hands the API a value no check table holds. The download strips the
zeros through the same marker, so a missing one breaks both directions.

ZSDS_CUST_MASS_UPLOAD padded FDGRV and VBUND; the template map lost both
when it replaced that program, which is what this catches.

Field -> domain comes from the DD03L extract (dd03l_new_2.xlsx), domain ->
conversion exit from DD01L (dd01l.xlsx). A domain DD01L does not list is
reported as not confirmable rather than passed silently.
"""
import os, re, sys, zipfile
from xml.etree import ElementTree as ET

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
NS = '{http://schemas.openxmlformats.org/spreadsheetml/2006/main}'


def rows(path):
    z = zipfile.ZipFile(path)
    try:
        ss = [''.join(t.text or '' for t in si.iter(NS + 't'))
              for si in ET.fromstring(z.read('xl/sharedStrings.xml'))]
    except KeyError:
        ss = []
    w = ET.fromstring(z.read([n for n in z.namelist() if n.startswith('xl/worksheets/')][0]))
    for row in w.iter(NS + 'row'):
        v = []
        for c in row.iter(NS + 'c'):
            x = c.find(NS + 'v')
            t = '' if x is None else (x.text or '')
            v.append(ss[int(t)] if c.get('t') == 's' and t else t)
        yield v


it = rows(os.path.join(ROOT, 'dd03l_new_2.xlsx'))
h = next(it)
ti, fi, di = h.index('TABNAME'), h.index('FIELDNAME'), h.index('DOMNAME')
DOM = {(r[ti], r[fi]): r[di] for r in it if len(r) > di}

it = rows(os.path.join(ROOT, 'dd01l.xlsx'))
h = next(it)
dn, cx = h.index('DOMNAME'), h.index('CONVEXIT')
EXIT = {r[dn]: (r[cx] if len(r) > cx else '') for r in it if len(r) > dn}

# the table each node's fields belong to
TAB = {'C': 'KNA1', 'B': 'KNB1', 'S': 'KNVV', 'K': 'KNA1', 'Z': 'ZSD_LICENSE_CHK'}

SRC = open(os.path.join(ROOT, 'src/zsds_cust_tmpl_download.prog.abap'), encoding='utf-8').read()
ROW = re.compile(r"tmpl = '(\w+)' col = (\d+)\s+hdr = '(?:[^']|'')*'\s+node = '(.)' "
                 r"fld = '([A-Z][A-Z0-9_]*)' fmt = '(\w*)' cnv = '(\w*)'")

bad, checked, unknown = [], 0, set()
for m in ROW.finditer(SRC):
    tmpl, col, node, fld, fmt, cnv = m.groups()
    tab = TAB.get(node)
    if not tab:
        continue
    dom = DOM.get((tab, fld)) or DOM.get(('KNA1', fld))
    if not dom:
        continue
    if dom not in EXIT:
        unknown.add(dom)
        continue
    checked += 1
    if EXIT[dom] == 'ALPHA' and (cnv not in ('AL', 'GL') or fmt not in ('AL', 'GL')):
        bad.append(f'{tmpl} col {col}: {tab}-{fld} has domain {dom} (ALPHA) but '
                   f'fmt="{fmt}" cnv="{cnv}" - leading zeros are neither stripped '
                   f'on download nor restored on upload')

if not checked:
    sys.exit('no template column could be checked - the map format or the extracts changed')
if bad:
    print('\n'.join(bad))
    sys.exit(1)
print(f'clean - {checked} template columns have a domain DD01L knows, and every '
      f'ALPHA one carries the AL/GL marker both ways ({len(unknown)} domains not in '
      f'the DD01L extract could not be confirmed)')
