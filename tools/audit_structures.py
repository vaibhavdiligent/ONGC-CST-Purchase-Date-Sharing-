"""Component and expression checks the syntax check would raise.

Three mistakes that all look harmless in a diff:

  * a component read off a variable that has no structure - the usual cause
    is SELECT SINGLE with ONE column INTO @DATA(x), which gives a field;
  * a component that does not exist on the structure it is read from, for
    the types the program declares itself - a renamed component is the
    usual cause;
  * a comparison used as a value - IF ( a = b ) = c - which ABAP rejects
    with "Operator or end of expression expected".

Everything is worked out per method, because the same variable name means
different things in different methods.
"""
import re, sys

PROGS = ['src/zsds_cust_mass_upload.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap',
         'src/zbcs_mass_upload_extract.prog.abap']
findings = []

for p in PROGS:
    src = open(p, encoding='utf-8').read()
    lines = src.split('\n')

    # ---- structure types the program declares, and their components ----
    types = {}
    for m in re.finditer(r'BEGIN OF (ty_\w+)\s*,(.*?)END OF \1', src, re.S):
        types[m.group(1).lower()] = set(
            c.lower() for c in re.findall(r'^\s*(\w+)\s+TYPE', m.group(2), re.M))

    # ---- table types over them, and every table declared over those -----
    line_of = {m.group(1).lower(): m.group(2).lower() for m in re.finditer(
        r'\b(tt_\w+)\s+TYPE\s+(?:STANDARD |SORTED |HASHED )?TABLE OF\s+(ty_\w+)', src, re.I)}
    tables = {}
    for m in re.finditer(r'\b(\w+)\s+TYPE\s+(tt_\w+)\b', src, re.I):
        if line_of.get(m.group(2).lower()):
            tables[m.group(1).lower()] = line_of[m.group(2).lower()]

    # ---- per method ----------------------------------------------------
    starts = [i for i, l in enumerate(lines)
              if re.match(r'\s*METHOD\s', l) and 'ENDMETHOD' not in l]
    starts.append(len(lines))
    for si in range(len(starts) - 1):
        a = starts[si]
        b = next((i for i in range(a, starts[si + 1])
                  if re.match(r'\s*ENDMETHOD', lines[i])), starts[si + 1])
        body = lines[a:b]
        text = '\n'.join(re.sub(r'".*$', '', x) for x in body)

        flat, typed = {}, {}
        for m in re.finditer(r'SELECT\s+SINGLE\s+(.+?)\s+FROM[\s\S]{0,400}?INTO\s+@DATA\(\s*(\w+)\s*\)',
                             text, re.I):
            if ',' not in m.group(1) and '*' not in m.group(1):
                flat[m.group(2).lower()] = a + text[:m.start()].count('\n') + 1
        for m in re.finditer(r'\bDATA[: ]\s*(\w+)\s+TYPE\s+(ty_\w+)\b', text, re.I):
            typed[m.group(1).lower()] = m.group(2).lower()
        for m in re.finditer(r'\bDATA\(\s*(\w+)\s*\)\s*=\s*VALUE\s+(ty_\w+)\(', text, re.I):
            typed[m.group(1).lower()] = m.group(2).lower()
        for m in re.finditer(r'\b(?:LOOP AT|READ TABLE)\s+([\w\-]+)[^.]{0,120}?INTO\s+DATA\(\s*(\w+)\s*\)',
                             text, re.I):
            base = m.group(1).split('-')[0].lower()
            if tables.get(base):
                typed[m.group(2).lower()] = tables[base]

        for off, l in enumerate(body):
            code = re.sub(r'".*$', '', l)
            n = a + off + 1
            if re.search(r'\bIF\s*\(\s*[\w\-]+\s*=\s*[^()]+?\)\s*=', code, re.I):
                findings.append(f'{p}:{n}: a comparison used as a value - '
                                f'wrap it in XSDBOOL( ): {code.strip()}')
            for m in re.finditer(r'\b(\w+)-(\w+)\b', code):
                var, comp = m.group(1).lower(), m.group(2).lower()
                if var in flat:
                    findings.append(f'{p}:{n}: {var.upper()} holds a single column '
                                    f'(line {flat[var]}), so it has no component {comp.upper()}')
                elif var in typed and typed[var] in types and comp not in types[typed[var]]:
                    findings.append(f'{p}:{n}: {typed[var].upper()} has no component '
                                    f'{comp.upper()} (it has '
                                    f'{", ".join(sorted(x.upper() for x in types[typed[var]]))})')

print('\n'.join(sorted(set(findings))) if findings else
      'clean - every component exists on the structure it is read from')
sys.exit(1 if findings else 0)
