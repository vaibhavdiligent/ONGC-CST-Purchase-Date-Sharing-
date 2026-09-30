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
    heading first, then by position.

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
    return re.sub(r'[^A-Z0-9]', '', (x or '').upper())


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
for tmpl, cs in sorted(cols.items()):
    cs.sort(key=lambda c: c['col'])
    # what the engine holds: squashed heading, cleared where repeated
    n = collections.Counter(squash(c['hdr']) for c in cs)
    engine = [dict(c, key=squash(c['hdr']) if n[squash(c['hdr'])] == 1 else '') for c in cs]
    # what the file carries: the heading row exactly as the download wrote it
    file_head = [c['hdr'] for c in cs]
    fpos = {}
    fcnt = collections.Counter(squash(h) for h in file_head)
    for i, h in enumerate(file_head, 1):
        if fcnt[squash(h)] == 1:
            fpos[squash(h)] = i
    for e in engine:
        total += 1
        src = fpos.get(e['key']) if e['key'] else None
        if src is None:
            src = e['col']                     # positional - same layout
            by_pos += 1
        else:
            by_head += 1
        if src != e['col']:
            fail.append(f'{tmpl} col {e["col"]} ({e["hdr"]}) would be read from '
                        f'column {src}')
        # the conversion must be one the reading method knows
        if e['node'] in IGNORED or not e['cnv']:
            continue
        known = lic_cnv if e['node'] == 'Z' else comp_cnv
        if e['cnv'] not in known:
            fail.append(f'{tmpl} col {e["col"]} ({e["fld"]}) carries conversion '
                        f'{e["cnv"]}, which the {"licence" if e["node"] == "Z" else "customer"} '
                        f'writer does not handle - it would be written raw')

print(f'{len(cols)} templates, {total} columns: {by_head} bound by heading, '
      f'{by_pos} by position')
if fail:
    print('\n'.join(fail[:40]))
    if len(fail) > 40:
        print(f'  ... and {len(fail) - 40} more')
    sys.exit(1)
print('clean - every column of every template uploads back onto the field it '
      'was downloaded from, through a conversion its writer knows')
