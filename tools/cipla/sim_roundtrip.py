"""A file the download writes must upload back onto the fields it came from.

That is the whole case for one program: the download writes a template's
heading row from the map, and the upload reads the same map to decide which
column feeds which field. This walks all 24 templates the way the program
does it and checks every one of the 1986 columns comes home.

  * The download writes HDR as the workbook has it.
  * The upload's engine squashes HDR at load, and clears it where the
    template uses the same heading twice - such a heading cannot identify
    a column, so those columns stay positional.
  * The reader squashes the file's heading row the same way and binds by
    heading first (the nth of a repeated heading to the nth), then by
    technical field name, then by position - BIND below mirrors
    LCL_ENGINE=>BIND_COLUMNS line for line.

It then mangles each template's file - column 1 deleted and two others
swapped - and checks that no field is ever loaded with another column's
value: a column is read from its own cell or, where it cannot be found,
left empty.

It also checks that every conversion the map carries is one the upload
actually handles on the node the column goes to - a date conversion on a
licence column is read by LCL_LIC, not by SET_COMP, and the two do not know
the same list.
"""
import collections, json, os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
SRC  = open(os.path.join(ROOT, 'src/zsds_cust_tmpl_download.prog.abap'), encoding='utf8').read()

ROW = re.compile(r"\(\s*tmpl = '(\w+)'\s+col = (\d+)\s+hdr = '((?:[^']|'')*)'\s+"
                 r"node = '(.)'\s+fld = '(.*?)'\s+fmt = '(\w*)'\s+cnv = '(\w*)'\s*\)")


def squash(x):
    """LCL_UTIL=>SQUASH, which keeps 40 characters."""
    return re.sub(r'[^A-Z0-9]', '', (x or '').upper())[:40]


def bind(ents, head):
    """LCL_ENGINE=>BIND_COLUMNS. ENTS carry the engine's squashed HDR; HEAD
       is the file's heading row keyed by column. Returns the column each
       entry is read from (0 = left empty) and how it was found."""
    cnt, occ, bycol = collections.Counter(), {}, {}
    for c in sorted(head):
        k = squash(head[c])
        if not k:
            continue
        cnt[k] += 1
        occ[(k, cnt[k])] = c
        bycol[c] = k
    mcnt = collections.Counter(e['hdr'] for e in ents if e['hdr'])
    fcnt = collections.Counter(squash(e['fld']) for e in ents if squash(e['fld']))
    fk = {k for k, n in fcnt.items() if n == 1}
    col = [e['col'] for e in ents]
    how = ['pos'] * len(ents)
    done, used, seen = set(), set(), collections.Counter()
    for i, e in enumerate(ents):                       # first pass - heading
        if not e['hdr']:
            continue
        seen[e['hdr']] += 1
        if cnt[e['hdr']] == 0:
            continue
        if cnt[e['hdr']] < mcnt[e['hdr']]:             # fewer in the file: ambiguous
            col[i], how[i] = 0, 'ambiguous'
            done.add(i)
            continue
        c = occ.get((e['hdr'], seen[e['hdr']]))
        if c is None:
            continue
        col[i], how[i] = c, 'heading'
        done.add(i); used.add(c)
    for i, e in enumerate(ents):                       # second pass - field name
        if i in done:
            continue
        k = squash(e['fld'])
        if not k or k not in fk or cnt[k] != 1 or occ[(k, 1)] in used:
            continue
        col[i], how[i] = occ[(k, 1)], 'field'
        done.add(i); used.add(occ[(k, 1)])
    if not done:
        return col, how
    for i, e in enumerate(ents):                       # the rest - by position
        if i in done:
            continue
        k = bycol.get(e['col'])
        if e['col'] in used or (k and k != e['hdr'] and k != squash(e['fld'])
                                and (k in mcnt or k in fk)):
            col[i], how[i] = 0, 'blank'
    return col, how


def read(ents, head, data):
    """What each map row is loaded with, given a heading row and a data row
       keyed by file column."""
    col, how = bind(ents, head)
    return [data.get(c, '') if c else '' for c in col], how


def handled(method):
    """The conversions a method's CASE iv_cnv knows."""
    m = re.search(rf'METHOD {method}\..*?ENDMETHOD\.', SRC, re.S)
    body = m.group(0) if m else ''
    out = set()
    for w in re.finditer(r"WHEN\s+((?:'\w*'\s*(?:OR\s*)?)+)\.", body):
        out |= set(re.findall(r"'(\w*)'", w.group(1)))
    return out


# where a column goes on upload, and which method converts it
BY_NODE = {'Z': 'set'}                    # LCL_LIC=>SET
DEFAULT = 'set_comp'                      # LCL_ENGINE=>SET_COMP for the rest
IGNORED = {'X', '-', 'P', 'M', 'T', 'I', 'K'}   # read by their own code, no CNV

cols = collections.defaultdict(list)
for m in ROW.finditer(SRC):
    cols[m.group(1)].append(dict(col=int(m.group(2)), hdr=m.group(3).replace("''", "'"),
                                 node=m.group(4), fld=m.group(5), cnv=m.group(7)))

# LCL_LIC=>SET is the second SET method in the program; take the one in LCL_LIC
lic = re.search(r'CLASS lcl_lic IMPLEMENTATION\..*?ENDCLASS\.', SRC, re.S).group(0)
lic_cnv = set()
m = re.search(r'METHOD set\..*?ENDMETHOD\.', lic, re.S)
for w in re.finditer(r"WHEN\s+((?:'\w*'\s*(?:OR\s*)?)+)\.", m.group(0)):
    lic_cnv |= set(re.findall(r"'(\w*)'", w.group(1)))
comp_cnv = handled('set_comp')

fail, total, by_head, by_pos = [], 0, 0, 0

# BIND is a copy, so it is tied to the program: the two statements it relies
# on have to be there, or the simulation is of a program that no longer exists.
ENG = re.search(r'CLASS lcl_engine IMPLEMENTATION\..*?ENDCLASS\.', SRC, re.S).group(0)
for what, pat in (
        ('the constructor keeps every heading, repeated or not',
         r'ls_m-hdr = lcl_util=>squash\( ls_col-hdr \)\.'),
        ('BIND_COLUMNS leaves a repeated heading empty when the file has it fewer times',
         r'IF ls_fc-n < ls_mc-n\.\s+<ls_m>-col = 0\.')):
    if not re.search(pat, ENG):
        fail.append(f'LCL_ENGINE no longer matches this simulation: {what}')
mangle_lost = rep_lost = rtested = 0
for tmpl, cs in sorted(cols.items()):
    cs.sort(key=lambda c: c['col'])
    # what the engine holds: every heading, squashed
    engine = [dict(c, hdr=squash(c['hdr'])) for c in cs]

    # ---- the file exactly as the download wrote it ----------------------
    head = {c['col']: c['hdr'] for c in cs}
    data = {c['col']: f'V{c["col"]}' for c in cs}
    got_all, how = read(engine, head, data)
    for i, (e, got) in enumerate(zip(engine, got_all)):
        total += 1
        if how[i] in ('heading', 'field'):
            by_head += 1
        else:
            by_pos += 1
        if got != f'V{e["col"]}':
            fail.append(f'{tmpl} col {e["col"]} ({cs[i]["hdr"]}) would be loaded with '
                        f'{got or "nothing"}')

    # ---- the same file with column 1 deleted and two columns swapped ---
    width = max(head)
    order = [c for c in range(1, width + 1) if c != 1]
    if len(order) > 9:
        order[4], order[8] = order[8], order[4]
    mhead = {i + 1: head.get(c, '') for i, c in enumerate(order)}
    mdata = {i + 1: f'V{c}' for i, c in enumerate(order)}
    for e, got, c in zip(engine, read(engine, mhead, mdata)[0], cs):
        if e['col'] == 1 or got == f'V{e["col"]}':
            continue
        if got == '':
            mangle_lost += 1
        else:
            fail.append(f'{tmpl} col {e["col"]} ({c["hdr"]}) - with column 1 deleted '
                        f'and two swapped it is loaded with {got}, another column\'s value')

    # ---- one of a repeated heading's own columns deleted ----------------
    # The file then carries that heading fewer times than the template, so
    # which is which cannot be told: those columns must come back empty,
    # never shifted onto the neighbour's value.
    rep_n = collections.Counter(e['hdr'] for e in engine if e['hdr'])
    gone = next((e['col'] for e in engine if rep_n[e['hdr']] > 1), None)
    if gone:
        rtested += 1
        order = [c for c in range(1, width + 1) if c != gone]
        rhead = {i + 1: head.get(c, '') for i, c in enumerate(order)}
        rdata = {i + 1: f'V{c}' for i, c in enumerate(order)}
        for e, got, c in zip(engine, read(engine, rhead, rdata)[0], cs):
            if e['col'] == gone or got == f'V{e["col"]}':
                continue
            if got == '':
                rep_lost += 1
            else:
                fail.append(f'{tmpl} col {e["col"]} ({c["hdr"]}) - with column {gone} '
                            f'removed it is loaded with {got}, another column\'s value')

    for e in engine:
        # the conversion must be one the reading method knows
        if e['node'] in IGNORED or not e['cnv']:
            continue
        known = lic_cnv if e['node'] == 'Z' else comp_cnv
        if e['cnv'] not in known:
            fail.append(f'{tmpl} col {e["col"]} ({e["fld"]}) carries conversion '
                        f'{e["cnv"]}, which the {"licence" if e["node"] == "Z" else "customer"} '
                        f'writer does not handle - it would be written raw')

print(f'{len(cols)} templates, {total} columns: {by_head} bound by heading or field '
      f'name, {by_pos} by position')
print(f'mangled files (column 1 deleted, two swapped): '
      + ('no field loaded with another column\'s value' if not any('deleted' in f for f in fail)
         else 'see below')
      + f'; {mangle_lost} column(s) left empty rather than guessed')
print(f'one repeated column removed ({rtested} templates): '
      + ('no field loaded with another column\'s value' if not any('removed' in f for f in fail)
         else 'see below')
      + f'; {rep_lost} column(s) left empty rather than guessed')
if fail:
    print('\n'.join(fail[:40]))
    if len(fail) > 40:
        print(f'  ... and {len(fail) - 40} more')
    sys.exit(1)
print('clean - every column of every template uploads back onto the field it '
      'was downloaded from, through a conversion its writer knows')
