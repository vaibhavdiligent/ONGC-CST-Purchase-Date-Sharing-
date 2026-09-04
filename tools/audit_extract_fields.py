"""Every field the extractor reads has to be a field that exists.

The customer map was built from the dictionary, the supplier map was built
from the template headings - so it asked for POSTCODE1 where the structure
holds POST_CODE1, for BANKS01 where it holds BANKS, and for
VENDORACCOUNTNUMBER, which is a column heading and not a field name at all.
ASSIGN COMPONENT found nothing, the cell came out empty, and nothing said
so: the file looked complete and was not.

Each node of the map names the structure it is read from. Where the
dictionary extract carries that structure the name is checked against it
outright. The supplier structures are not in the extract, so those names
are checked against the vocabulary of the upload program instead - it
writes the same fields the extractor reads, so a name neither side knows
is a name nothing will ever answer.
"""
import json, os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
EX   = open(os.path.join(ROOT, 'src/zbcs_mass_upload_extract.prog.abap'), encoding='utf-8').read()
SUP  = open(os.path.join(ROOT, 'src/zmms_bp_mass_upload.prog.abap'),      encoding='utf-8').read()
CUS  = open(os.path.join(ROOT, 'src/zsds_cust_mass_upload.prog.abap'),    encoding='utf-8').read()
DD   = json.load(open(os.path.join(ROOT, 'tools/ddic.json')))

def comps(struct, under=None):
    """Component names of a structure; `under` descends into one of them."""
    rows = sorted(DD.get(struct, []), key=lambda c: c['pos'])
    if under is None:
        return {c['f'] for c in rows if str(c['depth']) == '0'}
    out, inside = set(), False
    for c in rows:
        if str(c['depth']) == '0':
            inside = (c['f'] == under)
            continue
        if inside and str(c['depth']) == '1':
            out.add(c['f'])
    return out

ADDRESS = comps('CVIS_EI_1VL', 'DATA')                     # BAPIAD1VL
BANKDET = comps('CVIS_EI_CVI_BANKDETAIL', 'DATA_KEY') | \
          comps('CVIS_EI_CVI_BANKDETAIL', 'DATA')
LICENCE = comps('ZSD_LICENSE_CHK')
# BNKA is not in the dictionary extract; these are its fields, and the only
# ones the bank-key template asks for.
BNKA = {'BANKS', 'BANKL', 'BANKA', 'PROVZ', 'STRAS', 'ORT01', 'BRNCH', 'SWIFT',
        'BNKLZ', 'PSKTO', 'BGRUP', 'XPGRO', 'BNKLZ', 'ADRNR', 'ERNAM', 'ERDAT',
        'LOEVM', 'BANKN', 'MENUE', 'BKONT'}

# structures the dictionary extract carries, per side - the node letters
# are shared but 'B' is KNB1 on the customer and LFB1 on the supplier.
BY_NODE = {
    ('C', 'C'): comps('CMDS_EI_CMD_CENTRAL', 'DATA'),
    ('C', 'B'): comps('CMDS_EI_COMPANY_DATA'),
    ('C', 'S'): comps('CMDS_EI_SALES_DATA'),
    ('C', 'Z'): LICENCE,
    ('C', 'A'): ADDRESS,
    ('V', 'A'): ADDRESS,
    ('V', 'N'): BANKDET,
    ('V', 'Y'): BNKA,
}
# nodes the engine resolves in code rather than by component name
IN_CODE = {
    'K': {'KUNNR', 'BUKRS', 'VKORG', 'VTWEG', 'SPART', 'KTOKD',
          'LIFNR', 'EKORG', 'RBUKRS', 'REKORG'},
    'M': {'TEL', 'TELX', 'TEL2', 'TELX2', 'MOB', 'MOB2', 'FAX', 'SMT', 'SMT2'},
}
FREE = {'T', 'I', 'U'}          # tax category, identification, credit master

# the TAN structure the extractor declares for itself
TAN = set(re.findall(r'^\s+(\w+)\s+TYPE string,',
                     EX[EX.index('BEGIN OF ty_tan'):EX.index('END OF ty_tan')],
                     re.M))
TAN = {t.upper() for t in TAN}

def vocabulary(src):
    """Every field name the upload program itself writes or reads."""
    v  = set(re.findall(r"iv_comp\s*=\s*'([A-Z_0-9]+)'", src))
    v |= set(re.findall(r"\|([A-Z_0-9]+);\d+", src))
    v |= {m.upper() for m in re.findall(r"-data(?:_key)?-([a-z_0-9]+)", src)}
    v |= {m.upper() for m in re.findall(r"\bls_\w+-([a-z_0-9]+)\s*=", src)}
    return v

VOCAB = vocabulary(SUP) | vocabulary(CUS)

MAP = re.compile(
    r"\(\s*scen = '([CV]\d)' col = (\d+)\s+hdr = '((?:[^']|'')*)' "
    r"node = '([\w-]*)' fld = '([^']*)' fmt = '([^']*)' \)")
rows = [(m.group(1), int(m.group(2)), m.group(4), m.group(5)) for m in MAP.finditer(EX)]
if not rows:
    sys.exit('no map entries found - the map format changed')

bad, seen = [], 0
for scen, col, node, fld in rows:
    if node in ('', '-'):
        continue
    name = fld.split('#')[0]
    seen += 1
    if (scen[0], node) in BY_NODE:
        if name not in BY_NODE[(scen[0], node)]:
            bad.append(f'{scen} col {col:>3}: {name} is not a field of the {node} node')
    elif node in IN_CODE:
        if name not in IN_CODE[node]:
            bad.append(f'{scen} col {col:>3}: the {node} node has no case for {name}')
    elif node == 'X':
        if name not in TAN:
            bad.append(f'{scen} col {col:>3}: {name} is not a field of the TAN record')
    elif node in ('V', 'P', 'W', 'F', 'B'):
        # supplier structures: not in the dictionary extract, so the test is
        # whether the upload program knows the same name.
        if name not in VOCAB:
            bad.append(f'{scen} col {col:>3}: "{name}" is a name neither program writes')
    elif node not in FREE:
        bad.append(f'{scen} col {col:>3}: node "{node}" has no structure behind it')

# ---- every column the upload program reads is a column the sample fills --
# The vendor-extension template labels five columns and the upload program
# reads twelve; the seven without a heading were not in the map at all, so
# the sample came out with no vendor number in it and could not be uploaded.
HANDLER = dict(re.findall(r"WHEN p_r(\d)\. ro = NEW (lcl_h_\w+)\(", SUP))
SCEN_OF = {cls: 'V' + n for n, cls in HANDLER.items()}

def body(cls):
    start = SUP.index(f'CLASS {cls} IMPLEMENTATION.')
    return SUP[start:SUP.index('\nENDCLASS.', start)]

written = {}
for scen, col, node, fld in rows:
    if node not in ('', '-'):
        written.setdefault(scen, set()).add(col)

gaps = []
for cls, scen in sorted(SCEN_OF.items(), key=lambda x: x[1]):
    b = body(cls)
    read = {int(c) for c in re.findall(r"iv_col = (\d+)", b)}
    read |= {int(c) for c in re.findall(r"\|[A-Z_0-9]+;(\d+)", b)}
    read |= {int(c) for c in re.findall(r"`(\d+);", b)}
    miss = sorted(read - written.get(scen, set()))
    if miss:
        gaps.append(f'{scen} ({cls}): the upload reads column(s) '
                    f'{", ".join(str(m) for m in miss)} that the sample leaves empty')

print(f'{seen} mapped columns checked')
for line in bad + gaps:
    print(line)
if not bad and not gaps:
    print('clean - every field the extractor reads exists on the node it reads '
          'it from, and every column the upload reads is one the sample fills')
sys.exit(1 if bad or gaps else 0)
