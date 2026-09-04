"""The workbook has to be one CL_FDT_XL_SPREADSHEET will open.

That class is not a general .xlsx reader. It is the BRFplus spreadsheet
loader, and it wants the parts Excel itself writes - styles and the shared
string table among them - and it takes the text of a cell only from that
table. The first version of the extractor wrote four parts and put the text
inline: Excel opened those files, and the upload program answered

    The file is not a readable .xlsx workbook: A BRFplus exception occurred

This checks the package the extractor builds, from the source: every part
the reader needs is written, every part is well formed XML, everything the
relationships point at exists, the content types cover it, and no cell
carries its text inline.
"""
import os, re, sys, zipfile, io as _io
from xml.etree import ElementTree as ET

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
EX   = open(os.path.join(ROOT, 'src/zbcs_mass_upload_extract.prog.abap'), encoding='utf-8').read()

NEEDED = ['[Content_Types].xml', '_rels/.rels', 'xl/workbook.xml',
          'xl/_rels/workbook.xml.rels', 'xl/worksheets/sheet1.xml',
          'xl/sharedStrings.xml', 'xl/styles.xml']

bad = []

# ---- 1. which parts the program puts in the zip -------------------------
added = re.findall(r"lo_zip->add\(\s*name = '([^']+)'", EX)
for part in NEEDED:
    if part not in added:
        bad.append(f'{part} is never added to the zip - the reader needs it')

# ---- 2. how a cell carries its text -------------------------------------
if 'inlineStr' in EX:
    bad.append('a cell is written as inlineStr - CL_FDT_XL_SPREADSHEET reads '
               'text only from the shared string table')
if not re.search(r'<c r="\{[^}]*\}\{ iv_row \}" t="s">', EX):
    bad.append('cells are not written as shared strings (t="s")')

# ---- 3. the XML the program writes, assembled and parsed ----------------
# Each part is one chain of string templates assigned to a DATA(lv_x); the
# literal text between the templates is the XML. Substitutions are replaced
# by a harmless token so the result can be parsed.
def literal(var):
    m = re.search(r"DATA\(%s\) =\n(.*?)\n\n" % var, EX, re.S)
    if not m:
        return None
    body = m.group(1)
    body = re.sub(r'^\s*"[^\n]*$', '', body, flags=re.M)          # comments
    chunks = re.findall(r'\|(.*?)\|', body, re.S)
    text = ''.join(chunks)
    text = re.sub(r'\{[^{}]*\}', 'x', text)                        # { ... }
    return text

PARTS = {'[Content_Types].xml': 'lv_types', '_rels/.rels': 'lv_rels',
         'docProps/core.xml': 'lv_core', 'docProps/app.xml': 'lv_app',
         'xl/workbook.xml': 'lv_wb', 'xl/_rels/workbook.xml.rels': 'lv_wbrels',
         'xl/styles.xml': 'lv_sty'}

xml = {}
for part, var in PARTS.items():
    text = literal(var)
    if text is None:
        bad.append(f'{part}: could not find {var} in the source')
        continue
    try:
        ET.fromstring(text)
        xml[part] = text
    except ET.ParseError as e:
        bad.append(f'{part}: not well formed XML - {e}')

# ---- 4. relationships point at parts that are written -------------------
NSR = '{http://schemas.openxmlformats.org/package/2006/relationships}'
NSC = '{http://schemas.openxmlformats.org/package/2006/content-types}'

def targets(part, base):
    if part not in xml:
        return []
    return [base + r.get('Target').lstrip('/')
            for r in ET.fromstring(xml[part]).iter(NSR + 'Relationship')]

for t in targets('_rels/.rels', ''):
    if t not in added:
        bad.append(f'_rels/.rels points at {t}, which is not in the zip')
for t in targets('xl/_rels/workbook.xml.rels', 'xl/'):
    if t not in added:
        bad.append(f'xl/_rels/workbook.xml.rels points at {t}, which is not in the zip')

# ---- 5. content types cover every part ----------------------------------
if '[Content_Types].xml' in xml:
    root = ET.fromstring(xml['[Content_Types].xml'])
    over = {o.get('PartName').lstrip('/') for o in root.iter(NSC + 'Override')}
    defs = {d.get('Extension').lower() for d in root.iter(NSC + 'Default')}
    for part in added:
        if part == '[Content_Types].xml':
            continue
        ext = part.rsplit('.', 1)[-1].lower()
        if part not in over and ext not in defs:
            bad.append(f'{part} has no content type declared')

print(f'{len(added)} parts written: ' + ', '.join(added))
print('\n'.join(bad) if bad else
      'clean - the workbook carries every part the reader opens, and its text '
      'is in the shared string table')
sys.exit(1 if bad else 0)
