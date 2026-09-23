"""Finds actual parameters a formal parameter cannot take.

ABAP passes IMPORTING parameters by reference unless VALUE( ) is written, and
a by-reference parameter demands an actual parameter of exactly the same type
- no conversion. A value read from the workbook is a STRING, so handing one
to a parameter typed TYPE WITHT (or any other fixed DDIC type) is the syntax
error "The type STRING of LV_WT is not compatible with the type C(2) of
IV_WITHT". Declaring the parameter VALUE( ) is what allows the conversion.

Scoped per method, so a variable that was converted at its declaration
(DATA(lv_bukrs) = CONV bukrs( ... )) is not reported.
"""
import json, os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DD   = json.load(open(os.path.join(ROOT, 'tools/ddic.json')))

# A hexadecimal field and a character field do not convert into one another.
# The business partner is passed around as both: BUS_EI_INSTANCE-BPARTNERGUID
# is BU_PARTNER_GUID_BAPI, CHAR 32 holding the hexadecimal digits, and the
# CVI link tables' PARTNER_GUID is BU_PARTNER_GUID, RAW 16. Handing one to a
# parameter typed as the other is
#   The type "C(32)" of "...-BPARTNERGUID" is not compatible with ...
def kind(dt):
    if dt in ('RAW', 'RAWSTRING'):                     return 'X'
    if dt in ('DATS',):                                return 'D'
    if dt in ('TIMS',):                                return 'T'
    if dt in ('DEC', 'QUAN', 'CURR', 'FLTP', 'INT1', 'INT2', 'INT4', 'INT8'):
        return 'N'
    return 'C'

ELEM, COMP = {}, {}
for rows in DD.values():
    for c in rows:
        if c.get('roll'):
            ELEM.setdefault(c['roll'].lower(), set()).add(kind(c['dt']))
        COMP.setdefault(c['f'].lower(), set()).add(kind(c['dt']))
# Types the extract does not carry but the programs pass around.
ELEM['bu_partner_guid'] = {'X'}

def one(table, name):
    """The kind of a name, when every occurrence agrees on it."""
    k = table.get(name.lower())
    return next(iter(k)) if k and len(k) == 1 else None

GENERIC = {'any','clike','string','i','abap_bool','csequence','numeric','simple','data','xstring'}
STRINGY = ('lcl_util=>cell(', 'to_upper(', 'to_lower(', 'condense(', 'lcl_util=>squash(')

findings = []
PROGS_ALL = ['src/zsds_cust_mass_upload.prog.abap', 'src/zmms_bp_mass_upload.prog.abap',
            'src/zbcs_mass_upload_extract.prog.abap',
         'src/zsds_cust_tmpl_download.prog.abap']
for p in PROGS_ALL:
    src = open(p, encoding='utf-8').read()
    lines = src.split('\n')

    sigs, rets = {}, {}
    for m in re.finditer(r'^\s*(?:CLASS-)?METHODS[: ]\s*(\w+)([\s\S]*?)\.\s*$', src, re.M):
        params = {}
        for pm in re.finditer(r'(VALUE\()?\s*(i[vsto]_\w+)\)?\s+TYPE\s+(REF TO\s+)?([\w~=>]+)', m.group(2)):
            params[pm.group(2).lower()] = (pm.group(4).lower(), bool(pm.group(1)))
        if params:
            sigs[m.group(1).lower()] = params
        rm = re.search(r'RETURNING\s+VALUE\(\s*\w+\s*\)\s+TYPE\s+(REF TO\s+)?([\w~=>]+)', m.group(2))
        if rm:
            rets[m.group(1).lower()] = rm.group(2).lower()

    starts = [i for i, l in enumerate(lines) if re.match(r'\s*METHOD\s', l) and 'ENDMETHOD' not in l]
    for si, mi in enumerate(starts):
        end = next((i for i in range(mi, len(lines)) if re.match(r'\s*ENDMETHOD', lines[i])), len(lines))
        body = lines[mi:end]
        text = '\n'.join(body)

        # variables in THIS method whose type is STRING
        strvars = set(re.findall(r'DATA[: ]\s*(\w+)\s+TYPE\s+string\b', text, re.I))
        for dm in re.finditer(r'DATA\(\s*(\w+)\s*\)\s*=\s*([^\.]+)', text):
            rhs = dm.group(2)
            if re.match(r'\s*(CONV|VALUE|EXACT)\s', rhs):      # converted at birth
                continue
            # a helper that returns a typed value gives a typed variable
            call = re.match(r'\s*(?:lcl_\w+=>|mo_\w+->|)(\w+)\s*\(', rhs)
            if call and rets.get(call.group(1).lower(), 'string') != 'string':
                continue
            if any(s in rhs for s in STRINGY):
                strvars.add(dm.group(1))

        for off, l in enumerate(body):
            code = re.sub(r'".*$', '', l)
            for cm in re.finditer(r'(?:->|=>)?(\w+)\s*\(([^()]*(?:\([^()]*\)[^()]*)*)\)', code):
                name = cm.group(1).lower()
                if name not in sigs:
                    continue
                for am in re.finditer(r'(i[vsto]_\w+)\s*=\s*([^\s)]+(?:\([^()]*\))?)', cm.group(2)):
                    pname, actual = am.group(1).lower(), am.group(2)
                    if pname not in sigs[name]:
                        continue
                    ptype, byval = sigs[name][pname]
                    if ptype in GENERIC:
                        continue
                    # VALUE( ) allows a conversion, so the STRING rule is
                    # about by-reference parameters only. The character
                    # against hexadecimal rule below holds either way -
                    # those two never convert.
                    if not byval and (actual in strvars
                                      or any(s in actual for s in STRINGY)):
                        findings.append(f'{p}:{mi+off+1}: {name}( {pname} = {actual} ) '
                                        f'- a STRING cannot reach TYPE {ptype} by reference; '
                                        f'declare the parameter VALUE( ) or convert at the call')
                        continue

                    # character against hexadecimal, which never converts,
                    # by value or by reference
                    want = one(ELEM, ptype)
                    if want is None or '-' not in actual:
                        continue
                    got = one(COMP, actual.rsplit('-', 1)[-1])
                    if got and want != got and 'X' in (want, got):
                        findings.append(f'{p}:{mi+off+1}: {name}( {pname} = {actual} ) '
                                        f'- {actual.rsplit("-", 1)[-1]} is {got} and TYPE {ptype} '
                                        f'is {want}; a character field and a hexadecimal one '
                                        f'do not convert into one another')

# ---------------------------------------------------------------------------
# The same rule read the other way round. A by-reference parameter TYPE STRING
# takes nothing but a STRING either, so a screen field - which can never be one,
# PARAMETERS has no STRING - cannot be handed to it:
#
#   The type "C(255)" of "P_FILE" is not compatible with the type "STRING" of
#   "IV_FILE"
#
# which is what P_FILE did to LCL_EXCEL=>READ once the path parameter was
# widened. Only the first direction was checked here, so it went through.
for p in PROGS_ALL:
    path = os.path.join(ROOT, p)
    if not os.path.exists(path):
        continue
    src = open(path, encoding='utf-8').read()

    # a screen field is a character field, always
    screen = {m.group(1).lower() for m in re.finditer(
        r'^\s*(?:PARAMETERS|SELECT-OPTIONS):?\s+(\w+)', src, re.I | re.M)}
    screen |= {m.group(1).lower() for m in re.finditer(
        r'^\s{2,}(\w+)\s+(?:TYPE|LIKE|RADIOBUTTON|AS CHECKBOX)', src, re.M)
        if False}          # chained PARAMETERS are picked up below

    # chained declarations: PARAMETERS: a TYPE x, b TYPE y.
    for m in re.finditer(r'^\s*(?:PARAMETERS|SELECT-OPTIONS):\s(.*?)\.(?=\s*$)',
                         src, re.S | re.M | re.I):
        for part in m.group(1).split(','):
            d = re.match(r'\s*(\w+)', part)
            if d:
                screen.add(d.group(1).lower())

    # formal parameters TYPE STRING that are NOT VALUE( )
    byref = set()
    for m in re.finditer(r'(VALUE\(\s*)?\b(\w+)\s*\)?\s+TYPE\s+string\b',
                         src, re.I):
        if not m.group(1):
            byref.add(m.group(2).lower())

    for m in re.finditer(r'\b(\w+)\s*=\s*(\w+)\b', src):
        formal, actual = m.group(1).lower(), m.group(2).lower()
        if formal in byref and actual in screen:
            line = src[:m.start()].count('\n') + 1
            findings.append(f'{p}:{line}: {formal} = {actual} - {actual.upper()} is a '
                            f'screen field, which is a character field, and {formal.upper()} '
                            f'is a STRING taken by reference. A by-reference parameter takes '
                            f'nothing but its own type; convert at the call')

print('\n'.join(findings) if findings else
      'clean - no string reaches a by-reference parameter of a fixed type, no '
      'screen field reaches a by-reference STRING, and no character field is '
      'handed to a hexadecimal one')
sys.exit(1 if findings else 0)
