"""A green light must mean an outcome, not a remark.

Cipla's partner function run showed green and red on the same row and the
question that came back was the right one: was the partner added or not?

    2  0700000800  1000 1000  A  F2 165  Vendor 0100098685 has not been ...
    3  0100143049             S          100168036 is business partner ...
    3  0100143049  1000 1000  A  F2 165  Vendor 0100098685 has not been ...

The green line on row 3 is not a success.  It is the program saying which
vendor a business partner number resolved to - a remark made on the way,
logged as message type S and therefore given a green light, next to the red
line that says the row failed.  Three such remarks wore a green light: the
business partner resolution, the country a postal code was checked against,
and the bank accounts kept from the database.

So the rule: the green light belongs to message type S alone, S is used for
an outcome alone, and anything else gets the information icon.

And the counts have to follow the same logic.  A row counted OK because
nothing went wrong is not the same as a row something was done to: a row
passed over for want of a partner function used to be counted among the
successes.  Three buckets, and a line written against row 0 is about the run,
not about a row.
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

    m = re.search(r'icon\s*=\s*COND\s*#\((.*?)\)\s*\n', src, re.S)
    if not m:
        findings.append(f'{name}: no icon expression found - the log line no '
                        f'longer says what kind of message it is')
        continue
    expr = m.group(1)
    line = src[:m.start()].count('\n') + 1

    if not re.search(r"=\s*'S'\s*(?:THEN|.*?THEN)\s*icon_green_light", expr, re.S):
        findings.append(f'{name}:{line}: the icon expression has no branch '
                        f'giving message type S the green light - a green '
                        f'light must mean an outcome')
    tail = expr[expr.rfind('ELSE'):] if 'ELSE' in expr else ''
    if 'icon_green_light' in tail:
        findings.append(f'{name}:{line}: the icon expression falls back to '
                        f'icon_green_light, so every remark that is not an '
                        f'error looks like a success')

    # three buckets, and row 0 left out of them

    body = src[m.end():]
    if not re.search(r'\bev_skip\b|\blv_sk\b', src):
        findings.append(f'{name}: the run counts rows two ways - a row passed '
                        f'over with nothing done to it is counted among the '
                        f'successes. Count skipped rows separately')
    if not re.search(r'xlsrow\s*(?:>|<=)\s*0', src):
        findings.append(f'{name}: the row counts include lines written against '
                        f'row 0, which are about the run and not about a row')

if findings:
    print('\n'.join(findings))
    sys.exit(1)
print('clean - a green light means an outcome, and the run counts rows done, '
      'rows failed and rows passed over separately')
