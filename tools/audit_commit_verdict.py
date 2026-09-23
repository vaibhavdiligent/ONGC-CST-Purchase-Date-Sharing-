"""A success line must rest on something that was actually checked.

Three ways these programs hand work to the database, and each has one place
that says whether it went through:

  * CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' - its RETURN parameter. WAIT = X
    makes the call come back only after the update has run, so RETURN is
    meaningful; thrown away, a row whose update terminated is still logged
    "Posted successfully".
  * CALL FUNCTION ... IN UPDATE TASK followed by COMMIT WORK AND WAIT -
    SY-SUBRC after the COMMIT. Non-zero means the update was terminated.
  * MODIFY / INSERT / UPDATE - SY-SUBRC.

Cipla asked the question that found this: the TAN tab reported "2 TAN
exemption rows sent to J_1ITAN_EXEM_SAVE" - true, and no use to anyone,
because "sent" is all the program knew. It had not looked.

So: every commit must read its verdict.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROGS = ['src/zsds_cust_mass_upload.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap']

findings = []

for prog in PROGS:
    path = os.path.join(ROOT, prog)
    if not os.path.exists(path):
        continue
    name = os.path.basename(prog).split('.')[0].upper()
    src = open(path, encoding='utf8').read()

    # --- BAPI_TRANSACTION_COMMIT reads its RETURN ------------------------
    for m in re.finditer(r"CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'(.*?)\.\s*$",
                         src, re.S | re.M):
        call = m.group(1)
        line = src[:m.start()].count('\n') + 1
        if 'IMPORTING' not in call.upper() or 'RETURN' not in call.upper():
            findings.append(f'{name}:{line}: BAPI_TRANSACTION_COMMIT without '
                            f'IMPORTING RETURN - a failed update is then '
                            f'reported as a successful post')
        if not re.search(r'\bWAIT\s*=\s*abap_true', call, re.I):
            findings.append(f'{name}:{line}: BAPI_TRANSACTION_COMMIT without '
                            f'WAIT = ABAP_TRUE - it returns before the update '
                            f'has run, so RETURN cannot say anything')

    # --- COMMIT WORK AND WAIT reads SY-SUBRC -----------------------------
    for m in re.finditer(r'COMMIT WORK AND WAIT\.', src):
        line = src[:m.start()].count('\n') + 1
        after = src[m.end():m.end() + 400]
        # the verdict has to be taken before anything else can overwrite it
        if not re.search(r'^\s*(DATA\(\w+\)\s*=\s*sy-subrc|IF\s+sy-subrc)',
                         after, re.M):
            findings.append(f'{name}:{line}: COMMIT WORK AND WAIT whose '
                            f'SY-SUBRC is never read - a terminated update is '
                            f'then indistinguishable from a saved one')

    # --- no success line for an update module that was only "sent" -------
    for m in re.finditer(r"iv_txt\s*=\s*\|?[^\n]*\bsent to\b[^\n]*", src):
        line = src[:m.start()].count('\n') + 1
        findings.append(f'{name}:{line}: a message that says work was "sent" '
                        f'tells the user nothing about whether it was saved')

if findings:
    print('\n'.join(findings))
    sys.exit(1)
print('clean - every commit reads its verdict, and no run claims a save it did '
      'not check')
