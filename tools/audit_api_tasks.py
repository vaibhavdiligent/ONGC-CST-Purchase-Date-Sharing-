"""Guards the two upload programs against the CVI API's two refusals:

  "OBJECT_TASK M is not supported"                  - the API takes I or U,
                                                      never M, on any node.
  "Specify at least one number for the business
   partner" (R11 123)                               - the partner in the
                                                      message must be
                                                      identified: by its GUID
                                                      on a change, by a
                                                      generated one on a
                                                      creation.

Run it after any change to either program.
"""
import re, sys

PROGS = ['src/zsds_cust_mass_upload.prog.abap', 'src/zmms_bp_mass_upload.prog.abap']
findings = []

for p in PROGS:
    src = open(p, encoding='utf-8').read()
    lines = src.split('\n')

    # 1. no task may be 'M'
    for i, l in enumerate(lines, 1):
        code = re.sub(r'".*$', '', l)
        if re.match(r'\s*(CONSTANTS|gc_m\s+TYPE)', code):
            continue
        if re.search(r"task\s*(=|=>)?\s*.*\bgc_m\b", code, re.I) or re.search(r"task\s*=\s*'M'", code, re.I):
            findings.append(f'{p}:{i}: task M - the API answers "OBJECT_TASK M is not supported": {code.strip()}')

    # 2. every method that sets a partner object_task must also set a GUID
    meth, start = None, 0
    for i, l in enumerate(lines):
        t = l.strip()
        if re.match(r'METHOD\s', t) and 'ENDMETHOD' not in t:
            meth, start = t.split()[1].rstrip('.'), i
        elif re.match(r'ENDMETHOD', t) and meth:
            body = '\n'.join(lines[start:i])
            sets_task = re.search(r'(partner-header-object_task|ls_bp-header-object_task)\s*=', body)
            sets_guid = re.search(r'object_instance-bpartnerguid\s*=', body)
            if sets_task and not sets_guid:
                findings.append(f'{p}: {meth} sets the partner task but never its GUID '
                                f'-> "Specify at least one number for the business partner"')
            meth = None

    # 3. a creation must state the BP grouping
    if 'bp_control-grouping' not in src:
        findings.append(f'{p}: no BP grouping is ever set - a creation needs one for its number range')

    # 4. every task assignment resolves to gc_i / gc_u
    for i, l in enumerate(lines, 1):
        code = re.sub(r'".*$', '', l)
        m = re.search(r"-task\s*(?:=\s*)(.+)$", code)
        if not m:
            continue
        val = m.group(1)
        if 'COND' in val or 'gc_i' in val or 'gc_u' in val or val.strip() in ('', 'gc_i.', 'gc_u.'):
            continue
        if re.search(r"'[A-Z]'", val):
            findings.append(f'{p}:{i}: task set to a literal: {code.strip()}')

print('\n'.join(findings) if findings else 'clean - no M tasks, every partner node is identified, groupings are set')
sys.exit(1 if findings else 0)
