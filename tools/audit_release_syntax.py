"""Statements the S/4HANA 2025 syntax check refuses, found before SAP does.

All three were reported by the editor on /CCBJI/RUSDMDR_CUST_BP_MAINT:

  1. "Unexpected operator IS" - a built-in function as the operand of
     IS [NOT] INITIAL / BOUND / ASSIGNED: IF condense( x ) IS INITIAL.
     A functional METHOD call is allowed there; a built-in function is not.
  2. "LV_CUST cannot be converted to a character-like value" - one column
     read into an inline table, SELECT kunnr ... INTO TABLE @DATA(lt_c):
     the line is a structure with one component, not the field, so a
     string template or a character operand cannot take it. Fine where
     the line is used as the structure it is (lv_v-lifnr).
  3. "Unknown column name CHECKTABLE" - a SELECT on DD05S. The dictionary
     tables change with the release; foreign keys are read with DDIF_FK_GET.

    python3 tools/audit_release_syntax.py
"""
import glob, os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FILES = sorted(glob.glob(os.path.join(ROOT, 'src', '*.prog.abap')))

BUILTIN = ('abs|boolc|boolx|ceil|char_off|cmax|cmin|concat_lines_of|condense|contains|'
           'contains_any_of|contains_any_not_of|count|count_any_of|count_any_not_of|'
           'escape|find|find_any_of|find_any_not_of|find_end|floor|frac|insert|lines|'
           'match|matches|nmax|nmin|numofchar|repeat|replace|reverse|round|segment|'
           'shift_left|shift_right|sign|strlen|substring|substring_after|'
           'substring_before|substring_from|substring_to|to_lower|to_mixed|to_upper|'
           'translate|trunc|xsdbool|xstrlen|from_mixed|ipow|sqrt|line_exists|line_index')
CALL = re.compile(rf'(?<![\w>~-])({BUILTIN})\(', re.I)


def code(text):
    """Source with comments and literals blanked, line structure kept."""
    out = []
    for line in text.split('\n'):
        if line.startswith('*'):
            out.append('')
            continue
        res, q, brace = [], None, 0
        for ch in line:
            if q == '|' and (brace or ch == '{'):
                # The embedded expressions of a string template are code.
                brace += (ch == '{') - (ch == '}')
                res.append(ch)
            elif q:
                res.append(' ' if ch != q else ch)
                if ch == q:
                    q = None
            elif ch in "'`|":
                q = ch
                res.append(ch)
            elif ch == '"':
                break
            else:
                res.append(ch)
        out.append(''.join(res))
    return '\n'.join(out)


fail = []
for path in FILES:
    name = os.path.basename(path)
    raw = open(path, encoding='utf-8').read()
    src = code(raw)
    lines = src.split('\n')

    # ---- 1. built-in function before IS ----------------------------------
    for no, line in enumerate(lines, 1):
        for m in CALL.finditer(line):
            depth, i = 0, m.end() - 1
            while i < len(line):
                if line[i] == '(':
                    depth += 1
                elif line[i] == ')':
                    depth -= 1
                    if depth == 0:
                        break
                i += 1
            if depth == 0 and re.match(r'\s+IS\s+(NOT\s+)?(INITIAL|BOUND|ASSIGNED|SUPPLIED)\b',
                                       line[i + 1:], re.I):
                fail.append(f'{name}:{no}: {m.group(1)}( ) before IS - "Unexpected operator IS"; '
                            f'put the result in a variable first')

    # ---- 2. one column into an inline table, its line used as the field --
    for m in re.finditer(r'\bSELECT\s+(?!SINGLE\b)(?:DISTINCT\s+)?([\w~]+)\s+FROM\b[^.]*?'
                         r'INTO\s+TABLE\s+@DATA\((\w+)\)', src, re.I | re.S):
        col, tab = m.group(1).split('~')[-1], m.group(2)
        if col == '*' or col.startswith('@'):
            continue
        # Every work area the table is read into, and what is done with it
        # up to the end of that loop (or the next few statements).
        for w in re.finditer(rf'\b(?:LOOP\s+AT|READ\s+TABLE)\s+{tab}\b[^.]*?'
                             rf'INTO\s+DATA\((\w+)\)', src[m.end():], re.I | re.S):
            wa = w.group(1)
            start = m.end() + w.end()
            stop = src.find('ENDLOOP', start)
            body = src[start:stop if 0 < stop - start < 4000 else start + 1500]
            hit = re.search(rf'\b{wa}\b(?!\s*-)', body, re.I)
            if hit:
                no = src[:start + hit.start()].count('\n') + 1
                fail.append(f'{name}:{no}: {wa} is a line of {tab}, read by SELECT {col} INTO '
                            f'TABLE @DATA( ) - a structure, not {col} ("cannot be converted to '
                            f'a character-like value"); declare {tab} as a table of the '
                            f'column\'s type, or use {wa}-{col}')

    # ---- 3. dictionary tables read straight -------------------------------
    for m in re.finditer(r'\bFROM\s+dd05s\b', src, re.I):
        no = src[:m.start()].count('\n') + 1
        fail.append(f'{name}:{no}: SELECT on DD05S - its columns differ by release '
                    f'("Unknown column name CHECKTABLE"); use DDIF_FK_GET')

if fail:
    print(f'{len(fail)} problem(s):')
    for f in fail:
        print('  -', f)
    sys.exit(1)
print(f'clean - {len(FILES)} programs: no built-in function before IS, no one-column inline '
      f'table used as its field, no SELECT on DD05S')
