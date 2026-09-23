"""The language key: a two letter code going into a one character field.

SAP's language key is ONE character and it is not the first letter of the
two letter ISO code the templates carry:

    ISO   EN   ES   SV   DA   PT   ZH   KO   JA   NO
    SAP   E    S    V    K    P    1    3    J    O

Both upload programs write a spreadsheet cell into a field named at runtime,
and both treat a value longer than a one character target as a flag written
out in full - TRUE, YES, NO.  A language sent down that path is mangled
twice over and in silence:

  * NO, the code for Norwegian, is read as "false" and clears the field;
  * JA, the code for Japanese, is read as "true" and sets it to X;
  * anything else is cut to its first letter, which files a Spanish address
    under English (ES -> E) and a Swedish one under Spanish (SV -> S).

Nothing raises, nothing is logged, and the address is simply filed under the
wrong language.  Domain SPRAS carries conversion exit ISOLA for exactly this
case, so every column that lands in a LANG field must go through it.

The set of LANG fields is taken from the DD03L extract, not from a list
typed here, so a column mapped to a language field nobody thought of is
caught too.
"""
import json, os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

ddic = json.load(open(os.path.join(ROOT, 'tools/ddic.json'), encoding='utf8'))
LANG = {f['f'] for fields in ddic.values() if isinstance(fields, list)
        for f in fields if f.get('dt') == 'LANG'}

findings = []

# ---- ZSDS_CUST_MASS_UPLOAD: the map carries a conversion marker per column.
cust = open(os.path.join(ROOT, 'src/zsds_cust_mass_upload.prog.abap'),
            encoding='utf8').read()
for m in re.finditer(r"scen = '(\w+)' col = (\d+)\s+node = '(\w?)' "
                     r"fld = '([^']*)' cnv = '([^']*)'", cust):
    scen, col, fld, cnv = m.group(1), m.group(2), m.group(4), m.group(5)
    if fld in LANG and cnv != 'LG':
        findings.append(f'ZSDS_CUST_MASS_UPLOAD: {scen} column {col} writes '
                        f'{fld}, a one character language key, with cnv="{cnv}" '
                        f'- it needs LG or a two letter ISO code is mangled')
if "WHEN 'LG'." not in cust:
    findings.append('ZSDS_CUST_MASS_UPLOAD: no WHEN \'LG\' branch - the marker '
                    'on the language columns converts nothing')
if 'CONVERSION_EXIT_ISOLA_INPUT' not in cust:
    findings.append('ZSDS_CUST_MASS_UPLOAD: the language is converted without '
                    'the ISOLA exit')

# ---- ZMMS_BP_MASS_UPLOAD: fields are set from "FIELD;column" lists, which
#      all take the generic path, so a LANG field must not appear in one.
vend = open(os.path.join(ROOT, 'src/zmms_bp_mass_upload.prog.abap'),
            encoding='utf8').read()
for m in re.finditer(r'\(\s*\|([A-Z0-9_]+);(\d+)(?:;\d*)?\|\s*\)', vend):
    fld, col = m.group(1), m.group(2)
    if fld in LANG:
        line = vend[:m.start()].count('\n') + 1
        findings.append(f'ZMMS_BP_MASS_UPLOAD:{line}: {fld} (column {col}) is '
                        f'set from a "FIELD;column" list, which is the generic '
                        f'path - a two letter language code is mangled there')
if 'CONVERSION_EXIT_ISOLA_INPUT' not in vend:
    findings.append('ZMMS_BP_MASS_UPLOAD: the language is converted without '
                    'the ISOLA exit')

# ---- and the ISO codes whose first letter is a different SAP language must
#      never be reachable through the flag word list.
FLAG_WORDS = ("'TRUE' OR 'YES' OR 'JA'", "'FALSE' OR 'NO' OR 'NEIN'")
for name, src in (('ZSDS_CUST_MASS_UPLOAD', cust), ('ZMMS_BP_MASS_UPLOAD', vend)):
    for w in FLAG_WORDS:
        if w in src and 'CONVERSION_EXIT_ISOLA_INPUT' not in src:
            findings.append(f'{name}: JA and NO are read as flag words and '
                            f'nothing converts a language first')

if findings:
    print('\n'.join(findings))
    sys.exit(1)
print('clean - every column that lands in a one character language key goes '
      'through the ISOLA conversion exit, not through the flag word path')
