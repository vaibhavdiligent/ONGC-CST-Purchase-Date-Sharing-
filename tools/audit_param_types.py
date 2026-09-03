"""Finds actual parameters that a by-reference formal parameter cannot take.

ABAP passes IMPORTING parameters by reference unless VALUE( ) is written, and
a by-reference parameter demands an actual parameter of exactly the same type
- no conversion. A value read from the workbook is a STRING, so handing one
to a parameter typed TYPE WITHT (or any other fixed DDIC type) is the syntax
error "The type STRING of LV_WT is not compatible with the type C(2) of
IV_WITHT". Declaring the parameter VALUE( ) is what allows the conversion.

Scoped per method, so a variable that was converted at its declaration
(DATA(lv_bukrs) = CONV bukrs( ... )) is not reported.
"""
import re, sys

GENERIC = {'any','clike','string','i','abap_bool','csequence','numeric','simple','data','xstring'}
STRINGY = ('lcl_util=>cell(', 'to_upper(', 'to_lower(', 'condense(', 'lcl_util=>squash(')

findings = []
for p in ['src/zsds_cust_mass_upload.prog.abap', 'src/zmms_bp_mass_upload.prog.abap',
            'src/zbcs_mass_upload_extract.prog.abap']:
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
                    if byval or ptype in GENERIC:
                        continue
                    if actual in strvars or any(s in actual for s in STRINGY):
                        findings.append(f'{p}:{mi+off+1}: {name}( {pname} = {actual} ) '
                                        f'- a STRING cannot reach TYPE {ptype} by reference; '
                                        f'declare the parameter VALUE( ) or convert at the call')

print('\n'.join(findings) if findings else
      'clean - no string is handed to a by-reference parameter of a fixed type')
sys.exit(1 if findings else 0)
