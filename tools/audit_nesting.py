"""Every block opens and closes where it should - the syntax check SAP runs
first, before anything else can be checked.

ZSDS_CUST_TMPL_DOWNLOAD reached the system with LCL_UTIL declaring PUBLIC
SECTION twice and closed by ENDCLASS twice: two definitions merged into one
without their seams removed. Nothing here looked at block structure, so
the first to see it was the ABAP editor:

    A SECTION specification cannot be used more than once.
    Nesting not correct: The statement "ENDCLASS" does not have an open
    control structure introduced by "CLASS".

This reads each program as statements - comments, literals and string
templates removed, chains split - and checks:
  * CLASS/ENDCLASS, INTERFACE, METHOD, FORM, IF, CASE, LOOP, DO, WHILE, TRY,
    AT NEW/FIRST/LAST/END OF, DEFINE and SELECTION-SCREEN blocks and
    BEGIN OF / END OF open and close in the right order;
  * ELSE/ELSEIF sit in an IF, WHEN in a CASE, CATCH/CLEANUP in a TRY;
  * a class definition has each of PUBLIC, PROTECTED and PRIVATE SECTION
    at most once, in that order;
  * no name is declared twice in one scope - a type, data object, constant
    or method of a class, a DATA( ) or FIELD-SYMBOL( ) inline declaration
    in a method, a component of one structure, or a global.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROGS = ['src/zsds_cust_tmpl_download.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap',
         'tools/cipla/download_skeleton.abap']


def statements(src):
    """(line, text) per statement, literals blanked and comments dropped."""
    out, buf, line0 = [], [], None
    lines = src.split('\n')
    mode, depth = 'code', 0          # code | lit ' | bq ` | tpl |
    stack = []                       # template nesting: brace depth per level
    for ln, raw in enumerate(lines, 1):
        if mode == 'code' and not stack and raw.startswith('*'):
            continue
        i = 0
        while i < len(raw):
            ch = raw[i]
            if mode == 'lit':
                if ch == "'":
                    if raw[i + 1:i + 2] == "'":
                        i += 2; continue
                    mode = 'code'
                i += 1; continue
            if mode == 'bq':
                if ch == '`':
                    if raw[i + 1:i + 2] == '`':
                        i += 2; continue
                    mode = 'code'
                i += 1; continue
            if mode == 'tpl':
                if ch == '\\':
                    i += 2; continue
                if ch == '|':
                    mode = 'code'; stack.pop()
                    buf.append('T'); i += 1; continue
                if ch == '{':
                    mode = 'code'; stack[-1] += 1
                i += 1; continue
            # code
            if ch == '"':
                break
            if ch == "'":
                mode = 'lit'; buf.append('L'); i += 1; continue
            if ch == '`':
                mode = 'bq'; buf.append('L'); i += 1; continue
            if ch == '|':
                mode = 'tpl'; stack.append(0); i += 1; continue
            if ch == '}' and stack and stack[-1] > 0:
                stack[-1] -= 1; mode = 'tpl'; i += 1; continue
            if ch == '.' and not stack:
                # a decimal point is not a full stop
                nxt = raw[i + 1:i + 2]
                if not (nxt.isdigit() and buf and buf[-1][-1:].isdigit()):
                    text = ''.join(buf).strip()
                    if text:
                        out.append((line0 or ln, text))
                    buf, line0 = [], None
                    i += 1; continue
            if line0 is None and not ch.isspace():
                line0 = ln
            buf.append(ch)
            i += 1
        if mode == 'lit' or mode == 'bq':
            mode = 'code'            # a literal never spans lines
        buf.append(' ')
    return out


def unchain(text):
    """A chained statement is several: DATA: a TYPE i, b TYPE i."""
    m = re.match(r'([\w-]+(?:\s+[\w-]+)?)\s*:(?!=)(.*)$', text, re.S)
    if not m:
        return [text]
    head, rest = m.group(1), m.group(2)
    parts, depth, cur = [], 0, ''
    for ch in rest:
        if ch in '([':
            depth += 1
        elif ch in ')]':
            depth -= 1
        if ch == ',' and depth == 0:
            parts.append(cur); cur = ''
        else:
            cur += ch
    parts.append(cur)
    return [f'{head} {p.strip()}' for p in parts if p.strip()]


OPEN = {'IF': 'ENDIF', 'CASE': 'ENDCASE', 'LOOP': 'ENDLOOP', 'DO': 'ENDDO',
        'WHILE': 'ENDWHILE', 'TRY': 'ENDTRY', 'METHOD': 'ENDMETHOD',
        'FORM': 'ENDFORM', 'DEFINE': 'END-OF-DEFINITION', 'AT': 'ENDAT'}
CLOSE = {v: k for k, v in OPEN.items()}
INNER = {'ELSE': ('IF',), 'ELSEIF': ('IF',), 'WHEN': ('CASE',),
         'CATCH': ('TRY',), 'CLEANUP': ('TRY',)}
SECTIONS = ['PUBLIC', 'PROTECTED', 'PRIVATE']

findings = []
for p in PROGS:
    src = open(os.path.join(ROOT, p), encoding='utf-8').read()
    st = []                           # (kind, name, line)
    begins = []                       # BEGIN OF names, per statement family
    names = {}                        # (scope, name) -> first line

    def scope():
        # the class and the method both: nine handlers implement LIF_H~RUN
        path = [f'{kind} {name}' for kind, name, _ln, *_ in st
                if kind in ('CLASS', 'INTERFACE', 'METHOD', 'FORM')]
        return (' > '.join(path) or 'GLOBAL') + ''.join(f'/{b[0]}' for b in begins)

    def declare(name, ln, what):
        key = (scope(), name.lower())
        if key in names:
            findings.append(f'{p}:{ln}: {what} {name} is declared a second time in '
                            f'{key[0].lower()} (first at line {names[key]})')
        else:
            names[key] = ln
    for ln, text in statements(src):
        for s in unchain(text):
            w = s.split()
            if not w:
                continue
            k = w[0].upper()
            up = s.upper()
            where = f'{p}:{ln}'
            # ---- classes and interfaces
            if k == 'CLASS' and len(w) > 2:
                kind = w[2].upper()
                if kind in ('DEFINITION', 'IMPLEMENTATION') and \
                   not re.search(r'\b(DEFERRED|LOAD)\b', up):
                    st.append(('CLASS', w[1].lower(), ln, kind, []))
                continue
            if k == 'INTERFACE' and len(w) > 1 and not re.search(r'\b(DEFERRED|LOAD)\b', up):
                st.append(('INTERFACE', w[1].lower(), ln, '', []))
                continue
            if k in ('ENDCLASS', 'ENDINTERFACE'):
                want = k[3:]
                if not st or st[-1][0] != want:
                    top = f'{st[-1][0]} opened at line {st[-1][2]}' if st else 'nothing open'
                    findings.append(f'{where}: {k} does not close a {want} ({top})')
                    continue
                st.pop()
                continue
            if re.match(r'(PUBLIC|PROTECTED|PRIVATE)\s+SECTION$', up):
                if not st or st[-1][0] != 'CLASS' or st[-1][3] != 'DEFINITION':
                    findings.append(f'{where}: {w[0]} SECTION outside a class definition')
                    continue
                seen = st[-1][4]
                if k in seen:
                    findings.append(f'{where}: {k} SECTION a second time in class '
                                    f'{st[-1][1]} (opened line {st[-1][2]})')
                elif seen and SECTIONS.index(k) < SECTIONS.index(seen[-1]):
                    findings.append(f'{where}: {k} SECTION after {seen[-1]} SECTION in '
                                    f'class {st[-1][1]}')
                seen.append(k)
                continue
            # ---- selection screen blocks
            if k == 'SELECTION-SCREEN':
                if re.match(r'SELECTION-SCREEN\s+BEGIN\s+OF\s+(BLOCK|LINE|SCREEN|TABBED)', up):
                    st.append(('SSCR', up.split()[3], ln, '', []))
                elif re.match(r'SELECTION-SCREEN\s+END\s+OF\s+(BLOCK|LINE|SCREEN)', up):
                    if not st or st[-1][0] != 'SSCR':
                        findings.append(f'{where}: SELECTION-SCREEN END OF without a BEGIN OF')
                    else:
                        st.pop()
                continue
            # ---- BEGIN OF / END OF in TYPES, DATA, CONSTANTS
            if k in ('TYPES', 'DATA', 'CONSTANTS', 'CLASS-DATA', 'STATICS'):
                m = re.match(r'[\w-]+\s+BEGIN\s+OF\s+(?:MESH\s+|ENUM\s+)?(\w+)', up)
                if m:
                    if not begins:
                        declare(m.group(1), ln, k)
                    else:
                        declare(m.group(1), ln, 'component')
                    begins.append((m.group(1), ln)); continue
                m = re.match(r'[\w-]+\s+END\s+OF\s+(?:MESH\s+|ENUM\s+)?(\w+)', up)
                if m:
                    if not begins or begins[-1][0] != m.group(1):
                        findings.append(f'{where}: END OF {m.group(1)} without its BEGIN OF')
                    else:
                        begins.pop()
                    continue
                mm = re.match(r'[\w-]+\s+(<?\w+>?)', s)
                if mm and not (st and st[-1][0] == 'CLASS' and st[-1][3] == 'IMPLEMENTATION'):
                    declare(mm.group(1), ln, k if not begins else 'component')
                continue
            # ---- declarations
            # a class DEFINITION and its IMPLEMENTATION are one scope for
            # methods, so only the definition declares
            in_impl = any(x[0] == 'CLASS' and x[3] == 'IMPLEMENTATION' for x in st) and \
                not any(x[0] in ('METHOD', 'FORM') for x in st)
            m = re.match(r'(TYPES|DATA|CONSTANTS|CLASS-DATA|STATICS|FIELD-SYMBOLS|METHODS|'
                         r'CLASS-METHODS|EVENTS|CLASS-EVENTS|ALIASES|PARAMETERS|SELECT-OPTIONS)'
                         r'\s+(<?[\w/]+>?)', s, re.I)
            if m and not in_impl and not re.match(r'\w+\s+(BEGIN|END)\s+OF\b', up):
                nm = m.group(2)
                if not (m.group(1).upper() in ('METHODS', 'CLASS-METHODS') and '~' in s.split()[1]):
                    declare(nm, ln, m.group(1).upper())
            if any(x[0] in ('METHOD', 'FORM') for x in st):
                for nm in re.findall(r'\bDATA\((\w+)\)', s, re.I):
                    declare(nm, ln, 'DATA( )')
                for nm in re.findall(r'\bFIELD-SYMBOL\((<\w+>)\)', s, re.I):
                    declare(nm, ln, 'FIELD-SYMBOL( )')

            # ---- control structures
            if k == 'AT' and not re.match(r'AT\s+(NEW|FIRST|LAST|END\s+OF)\b', up):
                continue                   # AT SELECTION-SCREEN etc. are events
            if k == 'LOOP' and re.match(r'LOOP\s+AT\s+SCREEN\b.*\bINTO\b', up) is None and \
               False:
                pass
            if k in OPEN:
                st.append((k, w[1].lower() if len(w) > 1 else '', ln, '', []))
                continue
            if k in CLOSE:
                want = CLOSE[k]
                if not st or st[-1][0] != want:
                    top = f'{st[-1][0]} opened at line {st[-1][2]}' if st else 'nothing open'
                    findings.append(f'{where}: {k} does not close a {want} ({top})')
                    # recover: pop to the matching opener if there is one
                    for j in range(len(st) - 1, -1, -1):
                        if st[j][0] == want:
                            del st[j:]
                            break
                    continue
                st.pop()
                continue
            if k in INNER:
                if not st or st[-1][0] not in INNER[k]:
                    top = f'{st[-1][0]} opened at line {st[-1][2]}' if st else 'nothing open'
                    findings.append(f'{where}: {k} outside {"/".join(INNER[k])} ({top})')
    for kind, name, ln, *_ in st:
        findings.append(f'{p}:{ln}: {kind} {name} is never closed')
    for name, ln in begins:
        findings.append(f'{p}:{ln}: BEGIN OF {name} is never closed')

if findings:
    print('\n'.join(findings[:60]))
    sys.exit(1)
print(f'clean - every block of the {len(PROGS)} sources opens and closes in order, '
      f'no class declares a section twice, and no name is declared twice in one scope')
