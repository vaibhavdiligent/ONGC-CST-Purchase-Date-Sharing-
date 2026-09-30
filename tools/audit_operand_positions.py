"""No expression stands where ABAP wants a data object.

Some statements take only a variable. A method call or a constructor
expression there does not activate. Every one of these has been worked
around by hand in these programs - "MESSAGE takes a data object, not an
expression", "READ TABLE takes a table, not a method call" - and this makes
the rule a check rather than a habit.

Reported:
  * MESSAGE <text> TYPE ... whose text is a method call or a COND, SWITCH,
    CONV or VALUE expression. A string template is accepted - it activated
    in the version of ZSDS_CUST_TMPL_DOWNLOAD Cipla tested;
  * READ TABLE on a method call;
  * a method call where a statement writes: SORT, DELETE, MODIFY, CLEAR,
    FREE, APPEND ... TO, INSERT ... INTO TABLE, COLLECT ... INTO.

Accepted because they activated in the tested programs: LOOP AT on a
method call, and a method call before IS [NOT] INITIAL.
"""
import os, re, sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from abap_parse import statements, unchain

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROGS = ['src/zsds_cust_tmpl_download.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap',
         'tools/cipla/download_skeleton.abap']

CALL = r'(?:\w+(?:=>|->))*\w+\s*\('          # meth( / cls=>meth( / obj->meth(
EXPR = r'(?:COND|SWITCH|CONV|VALUE|NEW|CORRESPONDING|REDUCE|FILTER|EXACT|REF)\s+[#\w]'

RULES = [
    (re.compile(rf'^MESSAGE\s+(?:{CALL}|{EXPR})', re.I),
     'MESSAGE takes its text from a data object - put the call or expression in a variable first'),
    (re.compile(rf'^READ\s+TABLE\s+{CALL}', re.I),
     'READ TABLE reads a table, not a method call - put the result in a variable first'),
    (re.compile(rf'^(?:SORT|DELETE|MODIFY|CLEAR|FREE)\s+(?:TABLE\s+)?{CALL}', re.I),
     'a statement that writes cannot write into a method call'),
    (re.compile(rf'^APPEND\b.*\bTO\s+{CALL}', re.I),
     'APPEND cannot write into a method call'),
    (re.compile(rf'^(?:INSERT|COLLECT)\b.*\bINTO\s+(?:TABLE\s+)?{CALL}', re.I),
     'INSERT / COLLECT cannot write into a method call'),
]

findings = []
for p in PROGS:
    src = open(os.path.join(ROOT, p), encoding='utf-8').read()
    for ln, text in statements(src):
        for s in unchain(text):
            s1 = ' '.join(s.split())
            # a literal was blanked to L by the reader; "MESSAGE L TYPE" is fine
            for rx, why in RULES:
                if rx.search(s1):
                    findings.append(f'{p}:{ln}: {why}: {s1[:90]}')
                    break

if findings:
    print('\n'.join(findings))
    sys.exit(1)
print('clean - no method call or constructor expression stands where a statement '
      'needs a data object')
