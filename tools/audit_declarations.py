"""Method declarations against method implementations.

Three activation errors that a diff hides well, because each one looks like a
perfectly good method on its own:

  * the same method implemented twice in one class - "METHOD X is already
    implemented" - which is what an edit that replaces a method without
    removing the old one leaves behind;
  * a method implemented that the class never declares;
  * a method declared that the class never implements.
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
        lines = [re.sub(r'".*$', '', l) for l in
                 open(path, encoding='utf-8').read().split('\n')]

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
            for meth, line in bodies:
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
