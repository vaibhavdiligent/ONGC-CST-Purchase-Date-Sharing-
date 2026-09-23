"""The file path a selection screen carries, and the check put on its name.

Two faults that keep a perfectly good workbook out of the program, and both
of them read as if the file were at fault:

  * a path parameter narrower than the path.  CL_GUI_FRONTEND_SERVICES hands
    the chosen file back as a STRING; RLGRAP-FILENAME and the other classic
    file types are CHAR 128.  A OneDrive or Teams folder passes 128
    characters on its own, so the assignment cuts the path short - and the
    ".xlsx" at the end of it goes first.  The program is then told it has
    been given a file of some other type, and says so, about a file that is
    perfectly good.  This is what stopped Cipla's vendor creation run on
    15.09.2026: every other tab of that day's test loaded.

  * an extension check that asks whether ".xlsx" appears ANYWHERE in the
    path rather than at the end of it.  A folder called "xlsx files" lets a
    .xls through; a path cut short holds a .xlsx back.  The end of the name
    is the only part that says what the file is.

  * a path parameter without LOWER CASE.  The dynpro folds input to upper
    case, which the PC survives and an application server path does not.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROGS = ['src/zsds_cust_mass_upload.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap',
         'src/zbcs_mass_upload_extract.prog.abap',
         'src/zsds_cust_tmpl_download.prog.abap']

# The classic file types, all of them CHAR 128 or shorter.
NARROW = ('rlgrap-filename', 'localfile', 'ibipparms-path', 'dxfile-filename',
          'file_name', 'fileextern', 'sapb-sappfad')

findings = []

for prog in PROGS:
    path = os.path.join(ROOT, prog)
    if not os.path.exists(path):
        continue
    name = os.path.basename(prog).split('.')[0].upper()
    src = open(path, encoding='utf8').read()

    # --- the parameter itself -------------------------------------------
    for m in re.finditer(r'PARAMETERS:?\s+(\w*file\w*)\s+TYPE\s+([\w-]+)([^,.]*)',
                         src, re.I):
        par, typ, rest = m.group(1), m.group(2).lower(), m.group(3)
        if typ in NARROW:
            findings.append(f'{name}: {par.upper()} is {typ.upper()} - the file '
                            f'dialog returns a longer path than that fits, and '
                            f'the extension is what gets cut off')
        if not re.search(r'\bLOWER\s+CASE\b', rest, re.I):
            findings.append(f'{name}: {par.upper()} has no LOWER CASE - the '
                            f'dynpro folds the path to upper case, which an '
                            f'application server path does not survive')

    # --- what is done with what the dialog returns -----------------------
    # An assignment straight from the dialog's table into the parameter with
    # nothing testing the length first.
    for m in re.finditer(r'(\w*file\w*)\s*=\s*(?:\w+\[\s*1\s*\]|\w+)-filename\s*\.',
                         src, re.I):
        near = src[max(0, m.start() - 500):m.start()]
        if not re.search(r'\bstrlen\s*\(', near, re.I):
            findings.append(f'{name}: the file dialog\'s path goes into '
                            f'{m.group(1).upper()} without its length being '
                            f'tested - a path too long is cut short in silence')

    # --- the extension check ---------------------------------------------
    for m in re.finditer(r'\b(NS|CS)\s+\'\s*\.X', src, re.I):
        line = src[:m.start()].count('\n') + 1
        findings.append(f'{name}:{line}: the extension is tested with '
                        f'{m.group(1).upper()} - that asks whether ".xlsx" is '
                        f'somewhere in the path, not whether the name ends in '
                        f'it. Use CP / NP with a leading *')

if findings:
    print('\n'.join(findings))
    sys.exit(1)
print('clean - every file path parameter is wide enough for the path the file '
      'dialog returns, keeps its case, and every extension check tests the end '
      'of the name')
