"""The selection-screen and text mistakes that only show up on activation.

Each of these cost a round trip once already:

  * a radio button group split over two selection-screen blocks - "a parameter
    of the radio button group G1 was already defined in the block";
  * the deprecated POSIX regex, which the release warns on;
  * a short text longer than the ten characters SCRTEXT_S holds;
  * a TEXT-nnn with no text symbol behind it, or a selection-screen parameter
    with no selection text - both give a blank on screen rather than an error,
    which is worse;
  * a parameter name longer than eight characters, which the compiler refuses;
  * a value help that depends on another field of the same screen but reads that
    field's program variable. A value help runs before the screen has been handed
    to the program, so the variable still holds the previous round trip's value -
    empty the first time - and the help answers on the wrong country. The value
    has to come off the screen with DYNP_VALUES_READ.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROGS = ['src/zsds_cust_mass_upload.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap',
         'src/zbcs_mass_upload_extract.prog.abap',
         'src/zsds_cust_tmpl_download.prog.abap']

# A PARAMETERS or SELECT-OPTIONS statement runs to its closing period and may
# be chained, so the whole statement is taken and every element's name read off
# the front of it. Anything shorter mistakes a method parameter for a field on
# the screen, or misses a continuation line.
STMT = re.compile(r'^\s*(?:PARAMETERS|PARAMETER|SELECT-OPTIONS|SELECT-OPTION)\s*:?\s*(.*?)\.\s*$',
                  re.I | re.M | re.S)


def screen_fields(code):
    """Every name a selection-screen statement puts on the screen."""
    out = set()
    text = re.sub(r'\n', ' ', code)
    for m in re.finditer(r'\b(?:PARAMETERS|SELECT-OPTIONS)\s*:?\s*([^.]*)\.', text, re.I):
        for part in m.group(1).split(','):
            n = re.match(r'\s*(\w+)', part)
            if n:
                out.add(n.group(1).upper())
    return out

findings = []


def check(prog, cond, text):
    if not cond:
        findings.append(f'{prog}: {text}')


def main():
    for p in PROGS:
        path = os.path.join(ROOT, p)
        if not os.path.exists(path):
            continue
        name = os.path.basename(p)
        src = open(path, encoding='utf-8').read()
        code = '\n'.join(re.sub(r'".*$', '', l) for l in src.split('\n'))

        # ---- a radio group lives in one block ---------------------------
        block, groups = None, {}
        for line in code.split('\n'):
            m = re.match(r'\s*SELECTION-SCREEN BEGIN OF BLOCK (\w+)', line)
            if m:
                block = m.group(1)
            if re.match(r'\s*SELECTION-SCREEN END OF BLOCK', line):
                block = None
            for g in re.findall(r'RADIOBUTTON GROUP (\w+)', line):
                groups.setdefault(g.lower(), set()).add(block)
        for g, bs in groups.items():
            check(name, len(bs) == 1,
                  f'radio button group {g} is spread over blocks {sorted(b or "(none)" for b in bs)}')

        # ---- the regex flavour the release wants ------------------------
        check(name, not re.search(r'OCCURRENCES OF\s+REGEX', code, re.I),
              'uses the deprecated POSIX REGEX instead of PCRE')

        # ---- a short text fits in SCRTEXT_S -----------------------------
        for m in re.finditer(r"set_short_text\(\s*'([^']*)'", code):
            check(name, len(m.group(1)) <= 10,
                  f"set_short_text '{m.group(1)}' is {len(m.group(1))} characters, "
                  f'and the field holds ten')

        # ---- text symbols and selection texts ---------------------------
        xml_path = path.replace('.abap', '.xml')
        if not os.path.exists(xml_path):
            findings.append(f'{name}: has no companion XML, so it carries no texts')
            continue
        xml = open(xml_path, encoding='utf-8').read()
        used = set(re.findall(r'TEXT-(\d{3})', code))
        have = set(re.findall(r'<KEY>(\d{3})</KEY>', xml))
        for t in sorted(used - have):
            findings.append(f'{name}: TEXT-{t} is used but no text symbol defines it')
        for t in sorted(have - used):
            findings.append(f'{name}: text symbol {t} is defined but never used')

        pars = screen_fields(code)
        seltxt = {x for x in re.findall(r'<KEY>([A-Z_0-9]+)</KEY>', xml) if not x.isdigit()}
        for x in sorted(pars - seltxt):
            findings.append(f'{name}: {x} has no selection text')
        for x in sorted(seltxt - pars):
            findings.append(f'{name}: a selection text exists for {x}, which is not on the screen')

        for x in sorted(pars):
            check(name, len(x) <= 8, f'{x} is longer than the eight characters a parameter may have')

        # ---- a value help that leans on another field of the same screen -
        # A radio button whose group carries USER-COMMAND has already been
        # through PAI by the time any value help runs, so the program holds
        # its current value. Every other field needs DYNP_VALUES_READ.
        live = set()
        for m in re.finditer(r'\b(\w+)\s+RADIOBUTTON GROUP\s+(\w+)', code, re.I):
            live.add((m.group(1).upper(), m.group(2).lower()))
        cmd_groups = {g.lower() for g in re.findall(
            r'RADIOBUTTON GROUP\s+(\w+)\s+USER-COMMAND', code, re.I)}
        transported = {f for f, g in live if g in cmd_groups}

        bodies = {}
        for m in re.finditer(r'^\s*METHOD\s+(\w+)\s*\.(.*?)^\s*ENDMETHOD', code,
                             re.M | re.S | re.I):
            bodies[m.group(1).lower()] = m.group(2)
        for m in re.finditer(r'AT SELECTION-SCREEN ON VALUE-REQUEST FOR\s+(\w+)\s*\.(.*?)'
                             r'(?=^AT SELECTION-SCREEN|^START-OF-SELECTION|^INITIALIZATION|\Z)',
                             code, re.M | re.S | re.I):
            helped, block = m.group(1).upper(), m.group(2)
            # the handler and everything it reaches, however deep
            reach, seen, todo = block, set(), re.findall(r'(?:\w+=>)?(\w+)\s*\(', block)
            while todo:
                call = todo.pop().lower()
                if call in seen or call not in bodies:
                    continue
                seen.add(call)
                reach += bodies[call]
                todo += re.findall(r'(?:\w+=>)?(\w+)\s*\(', bodies[call])
            others = {x for x in pars
                      if x != helped and x not in transported
                      and re.search(r'\b' + x + r'\b', reach, re.I)}
            up = reach.upper()
            if 'F4IF_INT_TABLE_VALUE_REQUEST' in up:
                writes_back = ('DYNPROFIELD' in up
                               or ('RETURN_TAB' in up and 'DYNP_VALUES_UPDATE' in up))
                if not writes_back:
                    findings.append(
                        f'{name}: the value help for {helped} shows a list but writes '
                        f'nothing back - pass DYNPROFIELD, or take the pick from '
                        f'RETURN_TAB and write it with DYNP_VALUES_UPDATE')

            if others and 'DYNP_VALUES_READ' not in reach.upper():
                findings.append(
                    f'{name}: the value help for {helped} reads '
                    f'{", ".join(sorted(others))} straight from the program, but a value '
                    f'help runs before the screen is handed over - read it with '
                    f'DYNP_VALUES_READ')

    if findings:
        print(f'{len(findings)} problem(s):')
        for f in findings:
            print('  -', f)
        return 1
    print('clean - every radio group is in one block, no deprecated regex, every short '
          'text fits, and every text symbol and selection text is there')
    return 0


if __name__ == '__main__':
    sys.exit(main())
