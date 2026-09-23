"""Method declarations against method implementations.

Three activation errors that a diff hides well, because each one looks like a
perfectly good method on its own:

  * the same method implemented twice in one class - "METHOD X is already
    implemented" - which is what an edit that replaces a method without
    removing the old one leaves behind;
  * a method implemented that the class never declares;
  * a method declared that the class never implements;
  * an interface method declared with a tilde and no REDEFINITION -
    "METHODS lif_h~key_col." - which the editor refuses outright with
    "Names may consist only of the characters A-Z, _, 0-9 ... and -".
    INTERFACES has already declared every method of the interface; the
    tilde form is legal on a REDEFINITION and nowhere else. This audit
    matched it against its implementation and let it through, so the
    program only failed once it reached the system;
  * an ABAP Doc block ("!) separated from what it documents by an ordinary
    comment or a blank line - "ABAP Doc comment is in the wrong position".
    It is a warning, not an error, and it means the text is attached to the
    wrong declaration, which is worse than no text at all.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROGS = ['src/zsds_cust_mass_upload.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap',
         'src/zbcs_mass_upload_extract.prog.abap',
         'src/zsds_cust_tmpl_download.prog.abap']

DEFN = re.compile(r'^\s*CLASS\s+(\w+)\s+DEFINITION', re.I)
IMPL = re.compile(r'^\s*CLASS\s+(\w+)\s+IMPLEMENTATION', re.I)
END  = re.compile(r'^\s*ENDCLASS\s*\.', re.I)
# METHODS a, b, c. and CLASS-METHODS x IMPORTING ... - the name is the first
# word of each element of the chain.
DECL = re.compile(r'^\s*(?:CLASS-)?METHODS\b', re.I)
# An interface method carries a tilde - METHOD lif_h~run - and the tilde is
# not a word character, so it has to be allowed in the name.
BODY = re.compile(r'^\s*METHOD\s+([\w~]+)\s*\.', re.I)

findings = []

DOC  = re.compile(r'^\s*"!')
# what an ABAP Doc block is allowed to sit above
DECL = re.compile(r'^\s*(?:CLASS-)?(?:METHODS|DATA|TYPES|CONSTANTS|EVENTS)\b'
                  r'|^\s*(?:CLASS|INTERFACE|METHOD|FORM|FUNCTION|MODULE)\b'
                  r'|^\s*INTERFACES\b|^\s*ALIASES\b|^\s*[\w~]+\s*$', re.I)


def tilde_declarations(lines):
    """METHODS <intf>~<meth> without REDEFINITION - illegal, and an
    activation error rather than anything the tests would otherwise see."""
    out = []
    for i, line in enumerate(lines, 1):
        m = re.match(r'\s*(?:CLASS-)?METHODS\s+([\w]+~[\w]+)', line, re.I)
        if m and not re.search(r'\bREDEFINITION\b', line, re.I):
            out.append((i, m.group(1)))
    return out


def stranded_doc(lines):
    """An ABAP Doc block whose next non-doc line is not a declaration."""
    out, i, n = [], 0, len(lines)
    while i < n:
        if not DOC.match(lines[i]):
            i += 1
            continue
        start = i + 1
        while i < n and DOC.match(lines[i]):
            i += 1
        j = i
        while j < n and not lines[j].strip():
            j += 1
        if j >= n or not DECL.match(lines[j]):
            out.append((start, lines[j].strip()[:60] if j < n else '(end of file)'))
    return out



def declared_names(block):
    """Every method name a DEFINITION block declares."""
    names, text = set(), '\n'.join(block)
    # A declaration runs to the period that ends the statement - the first
    # one followed by whitespace or the end of the line, not the first one of
    # any kind, or the match runs on into the next declaration.
    for m in re.finditer(r'(?:^|\n)\s*(?:CLASS-)?METHODS\s*:?\s(.*?)\.(?=\s|$)',
                         text, re.S | re.I):
        for part in re.split(r',(?![^(]*\))', m.group(1)):
            # The name is the first word of the element. FOR and REDEFINITION
            # only ever follow it, so nothing needs excluding - and one of the
            # programs really does have a method called FOR.
            n = re.match(r'\s*([\w~]+)', part)
            if n:
                names.add(n.group(1).lower())
    return names


def main():
    for p in PROGS:
        path = os.path.join(ROOT, p)
        if not os.path.exists(path):
            continue
        name = os.path.basename(p)
        raw = open(path, encoding='utf-8').read().split('\n')
        lines = [re.sub(r'".*$', '', l) for l in raw]

        for line, meth in tilde_declarations(lines):
            findings.append(f'{name}: line {line} declares {meth} with a tilde '
                            f'and no REDEFINITION - INTERFACES has already '
                            f'declared it, and the editor refuses the name')
        for line, nxt in stranded_doc(raw):
            findings.append(f'{name}: the ABAP Doc block at line {line} is '
                            f'followed by "{nxt}" rather than by what it '
                            f'documents - it is attached to the wrong thing')

        defs, impls, cur, kind = {}, {}, None, None
        for i, l in enumerate(lines):
            m = DEFN.match(l)
            if m:
                cur, kind = m.group(1).lower(), 'D'
                defs[cur] = []
                continue
            m = IMPL.match(l)
            if m:
                cur, kind = m.group(1).lower(), 'I'
                impls[cur] = []
                continue
            if END.match(l):
                cur = kind = None
                continue
            if cur and kind == 'D':
                defs[cur].append(l)
            elif cur and kind == 'I':
                m = BODY.match(l)
                if m:
                    impls[cur].append((m.group(1).lower(), i + 1))

        for cls, bodies in impls.items():
            seen = {}
            for meth, line in bodies:
                if meth in seen:
                    findings.append(f'{name}: {cls}=>{meth} is implemented twice, '
                                    f'at line {seen[meth]} and line {line}')
                seen[meth] = line
            decl = declared_names(defs.get(cls, []))
            if not decl:
                continue
            # INTERFACES <name> declares every method of that interface, so
            # an implementation of one needs no METHODS line of its own -
            # and must not have one. Requiring a declaration here is what
            # pushed lcl_base into writing the illegal "METHODS lif_h~key_col."
            included = {i.lower() for i in re.findall(
                r'^\s*INTERFACES\s+([\w]+)', '\n'.join(defs.get(cls, [])),
                re.I | re.M)}
            for meth, line in bodies:
                if '~' in meth and meth.split('~')[0] in included:
                    continue
                if meth not in decl:
                    findings.append(f'{name}: {cls}=>{meth} at line {line} is '
                                    f'implemented but the class does not declare it')
            for meth in sorted(decl - {m for m, _ in bodies}):
                findings.append(f'{name}: {cls}=>{meth} is declared but never implemented')

    if findings:
        print(f'{len(findings)} problem(s):')
        for f in findings:
            print('  -', f)
        return 1
    print('clean - every method is declared once and implemented once')
    return 0


if __name__ == '__main__':
    sys.exit(main())
