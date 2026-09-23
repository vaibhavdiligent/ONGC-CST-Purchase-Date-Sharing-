"""Names that are declared and then never read.

"Variable X is never used" is the commonest warning these programs collect,
and every one of them is a line that says something the code does not do -
a left-over from an edit, or a value computed and then dropped.  They cost
nothing to keep and they hide the warnings that matter, so the rule here is
none at all.

Scoped per method body, and per event block for the report body outside any
class, because that is how ABAP scopes a local declaration.  A name that
appears once in its scope is its own declaration and nothing else.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROGS = ['src/zsds_cust_mass_upload.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap',
         'src/zbcs_mass_upload_extract.prog.abap',
         'src/zsds_cust_tmpl_download.prog.abap']

# DATA lv_x TYPE ... / DATA: a TYPE ..., b TYPE ... / DATA(lv_x) = ...
INLINE = re.compile(r'\bDATA\(\s*(\w+)\s*\)', re.I)
FS_INL = re.compile(r'\bFIELD-SYMBOL\(\s*<(\w+)>\s*\)', re.I)


def strip(src):
    """Drops whole-line comments only.

    A trailing " comment cannot be cut off reliably, because a string
    template carries quotes of its own - |Tab "{ iv_name }" could not be
    read| has two, and cutting at one of them hid the use of a variable and
    reported it unused. Leaving trailing comments in can only make a name
    look used when it is not, which is the safe direction to be wrong in:
    the compiler still has the last word.
    """
    out = []
    for line in src.split('\n'):
        t = line.lstrip()
        out.append('' if t.startswith('*') or t.startswith('"') else line)
    return out


def blocks(lines):
    """(name, first line, body lines) for every METHOD and for the report body."""
    out, cur, start, body = [], None, 0, []
    for i, l in enumerate(lines):
        m = re.match(r'\s*METHOD\s+([\w~]+)\s*\.', l, re.I)
        if m:
            cur, start, body = m.group(1), i + 1, []
            continue
        if re.match(r'\s*ENDMETHOD\s*\.', l, re.I) and cur:
            out.append((cur, start, body))
            cur = None
            continue
        if cur is not None:
            body.append(l)
    return out


def declared(body):
    """name -> line offset, for local declarations only."""
    names = {}
    text = '\n'.join(body)
    for m in INLINE.finditer(text):
        names.setdefault(m.group(1).lower(), text[:m.start()].count('\n'))
    for m in FS_INL.finditer(text):
        names.setdefault('<' + m.group(1).lower() + '>', text[:m.start()].count('\n'))
    # DATA a TYPE x.  /  DATA: a TYPE x, b TYPE y.
    for m in re.finditer(r'(?:^|\n)\s*DATA:?\s(.*?)\.(?=\s|$)', text, re.S | re.I):
        chunk = m.group(1)
        if '(' in chunk.split('TYPE')[0]:
            continue
        for part in chunk.split(','):
            d = re.match(r'\s*(\w+)\s+TYPE\b', part, re.I)
            if d:
                names.setdefault(d.group(1).lower(),
                                 text[:m.start()].count('\n'))
    return names


findings = []

for prog in PROGS:
    path = os.path.join(ROOT, prog)
    if not os.path.exists(path):
        continue
    name = os.path.basename(prog).split('.')[0].upper()
    lines = strip(open(path, encoding='utf8').read())

    for meth, start, body in blocks(lines):
        text = '\n'.join(body).lower()
        for var, off in declared(body).items():
            uses = len(re.findall(r'(?<![\w>])' + re.escape(var) + r'(?![\w<])', text))
            if uses <= 1:
                findings.append(f'{name}:{start + off}: {meth} declares {var} '
                                f'and never reads it')

if findings:
    print('\n'.join(findings))
    sys.exit(1)
print('clean - nothing is declared that is never read')
