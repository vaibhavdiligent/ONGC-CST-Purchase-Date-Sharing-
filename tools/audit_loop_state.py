"""Per-row state that survives into the next row.

A variable declared inside a loop is created once, not once per pass, so a
value written on one row is still there on the next. This reports the two
ways that bites:

  * a variable assigned only inside an IF / CASE / DO within the row loop and
    read after that block - on a row that skips the block, the previous row's
    value is read;
  * an internal table appended to inside the row loop with no CLEAR, unless
    it is deliberately collected across rows and used after the loop.
"""
import re, sys

OPEN  = re.compile(r'^(IF|CASE|DO|WHILE|TRY|LOOP AT|SELECT\b(?!.*\bINTO\b.*\bTABLE\b))\b', re.I)
SHUT  = re.compile(r'^(ENDIF|ENDCASE|ENDDO|ENDWHILE|ENDTRY|ENDLOOP|ENDSELECT)\b', re.I)
findings = []

for p in ['src/zsds_cust_mass_upload.prog.abap', 'src/zmms_bp_mass_upload.prog.abap',
            'src/zbcs_mass_upload_extract.prog.abap']:
    lines = open(p, encoding='utf-8').read().split('\n')
    for i, l in enumerate(lines):
        if not re.match(r'\s*LOOP AT (it_row|lt_rows)\b', l):
            continue
        depth, body = 0, []
        for j in range(i, len(lines)):
            t = re.sub(r'".*$', '', lines[j]).strip()
            if OPEN.match(t):  depth += 1
            if SHUT.match(t):
                depth -= 1
                if depth == 0:
                    break
            body.append((j + 1, t, depth))

        # where is each variable assigned, and at what nesting depth
        assigned, used = {}, {}
        for n, t, d in body:
            m = re.search(r'\bDATA\(\s*(\w+)\s*\)\s*=', t) or re.match(r'\s*(\w+)\s*=[^=]', t)
            if m:
                assigned.setdefault(m.group(1).lower(), []).append((n, d))
            # CLEAR / REFRESH resets the variable just as an assignment does
            cm = re.match(r'\s*(?:CLEAR|REFRESH)[: ]\s*(.+)$', t, re.I)
            if cm:
                for v in re.findall(r'\b(\w+)\b', cm.group(1)):
                    assigned.setdefault(v.lower(), []).append((n, d))
            for v in re.findall(r'\b(l[vst]_\w+)\b', t):
                used.setdefault(v.lower(), []).append((n, d))

        for v, places in assigned.items():
            if v.startswith(('ls_', 'lt_')):
                continue                       # structures/tables: CLEAR check below
            mind = min(d for _, d in places)
            if mind <= 1:                      # assigned at the top of every pass
                continue
            after = [n for n, d in used.get(v, []) if d < mind and n > max(x for x, _ in places)]
            if after:
                findings.append(f'{p}:{places[0][0]}: {v} is only assigned inside a nested block '
                                f'but read at line {after[0]} - the previous row\'s value survives')

        txt = '\n'.join(t for _, t, _ in body)
        for tbl in sorted(set(re.findall(r'APPEND\s+(?:LINES OF\s+)?[\w\-()]+\s+TO\s+(l[ts]_\w+)', txt))):
            if re.search(r'\b(CLEAR|REFRESH)\b[^.]*\b' + re.escape(tbl) + r'\b', txt):
                continue
            # collected on purpose when it is read after the loop
            tail = '\n'.join(re.sub(r'".*$', '', x).strip() for x in lines[i + len(body):i + len(body) + 40])
            if re.search(r'\b' + re.escape(tbl) + r'\b', tail):
                continue
            findings.append(f'{p}: {tbl} is appended to in the row loop, never cleared, '
                            f'and not used after the loop')

print('\n'.join(findings) if findings else
      'clean - nothing written on one row survives into the next')
sys.exit(1 if findings else 0)
