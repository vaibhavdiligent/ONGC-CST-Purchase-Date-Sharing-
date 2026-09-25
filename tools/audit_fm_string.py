"""A STRING handed to a classic function module's character parameter.

A function module parameter typed with a C field takes a C field and nothing
else.  Pass a string template - which builds a STRING - and the call does not
fail to compile; it dumps at run time, before it has done anything:

    CALL_FUNCTION_CONFLICT_GEN_TYP
    An attempt was made to pass field "%_##TVREG_001" to formal parameter
    "WINDOW_TITLE". Only fields of type "C" can be passed ... Field
    "WINDOW_TITLE" has type "STRING" however.

which is what F4 on the account group did the first time it was pressed: the
window title was built with |...| instead of into a character field.  A
literal survives, because a literal is type C - so the same line reads
perfectly well until the day the title has to carry a value.

Checked here: the parameters of classic function modules that are known to be
character fields, and any parameter given a string template or a variable
declared TYPE STRING.  Class methods are not affected - they are typed, and
the compiler settles it there.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROGS = ['src/zsds_cust_mass_upload.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap',
         'src/zbcs_mass_upload_extract.prog.abap',
         'src/zsds_cust_tmpl_download.prog.abap',
         'tools/cipla/download_skeleton.abap']

findings = []

for prog in PROGS:
    path = os.path.join(ROOT, prog)
    if not os.path.exists(path):
        continue
    name = os.path.basename(prog)
    src = open(path, encoding='utf8').read()

    strvars = {m.group(1).lower() for m in
               re.finditer(r'\b(\w+)\s+TYPE\s+string\b', src, re.I)}
    # a name declared TYPE STRING somewhere and TYPE something else elsewhere
    # is ambiguous, so only report the unambiguous ones
    other = {m.group(1).lower() for m in
             re.finditer(r'\b(\w+)\s+TYPE\s+(?!string\b)[\w\-/=>]+', src, re.I)}
    strvars -= other

    for m in re.finditer(r"CALL FUNCTION\s+'([^']+)'(.*?)\.\s*\n", src, re.S):
        fm, body = m.group(1), m.group(2)
        if 'TABLES' in body:
            body = body[:body.index('TABLES')] + body[body.index('TABLES'):]
        for pm in re.finditer(r"(\w+)\s*=\s*(\|[^\n]*?\||\w+)", body):
            par, val = pm.group(1), pm.group(2)
            line = src[:m.start() + pm.start()].count('\n') + 1
            if val.startswith('|'):
                findings.append(f'{name}:{line}: {fm} is given a string template '
                                f'for {par.upper()} - build it into a character '
                                f'field first, or the call dumps with '
                                f'CALL_FUNCTION_CONFLICT_GEN_TYP')
            elif val.lower() in strvars:
                findings.append(f'{name}:{line}: {fm} is given {val.upper()}, '
                                f'which is a STRING, for {par.upper()} - a '
                                f'classic function module takes a character '
                                f'field there')

if findings:
    print('\n'.join(findings))
    sys.exit(1)
print('clean - no string template and no STRING reaches a classic function '
      "module's parameter")
