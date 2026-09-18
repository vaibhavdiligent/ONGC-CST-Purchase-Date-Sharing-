"""The selection-screen and text mistakes that only show up on activation.

Each of these cost a round trip once already:

  * a radio button group split over two selection-screen blocks - "a parameter
    of the radio button group G1 was already defined in the block";
  * the deprecated POSIX regex, which the release warns on;
  * a short text longer than the ten characters SCRTEXT_S holds;
  * a TEXT-nnn with no text symbol behind it, or a selection-screen parameter
    with no selection text - both give a blank on screen rather than an error,
    which is worse;
  * a parameter name longer than eight characters, which the compiler refuses.
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
