"""A table declared WITH UNIQUE KEY is never filled in a way that can dump.

Moving rows that repeat a key into such a table raises ITAB_DUPLICATE_KEY -
a short dump, not an exception anything can catch. The vendor upload hit it
on T052, which holds one row per instalment: SELECT zterm FROM t052 INTO
TABLE of a unique table dumped as soon as one payment term had two
instalments. A SORTED table has a second trap: APPEND puts the row at the
end, and a row that does not belong there raises ITAB_ILLEGAL_SORT_ORDER.

INSERT ... INTO TABLE is always safe - a duplicate sets SY-SUBRC 4 - so it
is what these tables are filled with. What is reported:
  * SELECT ... INTO [CORRESPONDING FIELDS OF] TABLE of a unique table,
    unless the SELECT is DISTINCT;
  * a unique table assigned from another table, unless that one was filled
    by a SELECT DISTINCT, or is a unique table with the same key;
  * INSERT LINES OF ... INTO TABLE, VALUE #( ... ) and FOR constructors into
    a unique table - these dump on a duplicate as a plain move does;
  * APPEND to a SORTED or HASHED table.
"""
import os, re, sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from abap_parse import statements, unchain

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROGS = ['src/zsds_cust_tmpl_download.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap']

TBL = re.compile(r'\b(SORTED|HASHED)\s+TABLE\s+OF\b.*?\bWITH\s+(NON-UNIQUE|UNIQUE)\s+KEY\b(.*)$',
                 re.I | re.S)

findings = []
for p in PROGS:
    src = open(os.path.join(ROOT, p), encoding='utf-8').read()
    types = {}                     # table type name -> (kind, unique, key)
    scopes = [{}]                  # name -> (kind, unique, key); innermost last
    distinct = set()               # (scope depth id, name) filled by SELECT DISTINCT
    stack = []

    def lookup(name):
        for sc in reversed(scopes):
            if name in sc:
                return sc[name]
        return None

    for ln, text in statements(src):
        for s in unchain(text):
            up = ' '.join(s.split()).upper()
            w = up.split()
            if not w:
                continue
            where = f'{p}:{ln}'
            # ---- scopes: a class, a method
            if w[0] == 'CLASS' and len(w) > 2 and w[2] in ('DEFINITION', 'IMPLEMENTATION') \
                    and not re.search(r'\b(DEFERRED|LOAD)\b', up):
                stack.append(('CLASS', w[1]))
                scopes.append({})
                continue
            if w[0] == 'ENDCLASS' and stack:
                stack.pop(); scopes.pop()
                continue
            if w[0] in ('METHOD', 'FORM') and len(w) > 1:
                stack.append((w[0], w[1]))
                scopes.append({})
                continue
            if w[0] in ('ENDMETHOD', 'ENDFORM') and stack:
                stack.pop(); scopes.pop()
                continue
            # a class IMPLEMENTATION sees its DEFINITION's attributes: keep
            # them at program level too, keyed by name (attribute names in
            # these programs are distinct per class)
            # ---- declarations
            m = re.match(r'(TYPES)\s+(\w+)\s+TYPE\s+(.*)$', up, re.S)
            if m:
                t = TBL.search(m.group(3))
                if t:
                    types[m.group(2)] = (t.group(1), t.group(2) == 'UNIQUE', t.group(3).strip())
                continue
            m = re.match(r'(DATA|CLASS-DATA|STATICS)\s+(\w+)\s+TYPE\s+(.*)$', up, re.S)
            if m:
                name, rest = m.group(2), m.group(3)
                t = TBL.search(rest)
                info = None
                if t:
                    info = (t.group(1), t.group(2) == 'UNIQUE', t.group(3).strip())
                else:
                    tn = rest.split()[0]
                    info = types.get(tn)
                if info:
                    scopes[-1][name] = info
                    # attributes are visible in the implementation as well
                    if stack and stack[-1][0] == 'CLASS':
                        scopes[0][name] = info
                continue

            # ---- fills
            m = re.match(r'SELECT\b(.*)\bINTO\s+(?:CORRESPONDING\s+FIELDS\s+OF\s+)?TABLE\s+@(?:DATA\()?(\w+)', up, re.S)
            if m:
                name = m.group(2)
                is_distinct = bool(re.match(r'SELECT\s+DISTINCT\b', up))
                if is_distinct:
                    distinct.add(name)
                info = lookup(name)
                if info and info[1] and not is_distinct:
                    findings.append(f'{where}: SELECT into {name}, a table WITH UNIQUE KEY, '
                                    f'without DISTINCT - a repeated key dumps with ITAB_DUPLICATE_KEY')
                continue
            m = re.match(r'APPEND\b.*\bTO\s+(\w+)$', up)
            if m:
                info = lookup(m.group(1))
                if info:
                    findings.append(f'{where}: APPEND to {m.group(1)}, a {info[0]} table - '
                                    f'use INSERT ... INTO TABLE')
                continue
            m = re.match(r'INSERT\s+LINES\s+OF\s+\w+.*\bINTO\s+TABLE\s+(\w+)', up)
            if m:
                info = lookup(m.group(1))
                if info and info[1]:
                    findings.append(f'{where}: INSERT LINES OF into {m.group(1)}, a table WITH '
                                    f'UNIQUE KEY - a repeated key dumps; insert row by row')
                continue
            m = re.match(r'(\w+)\s*=\s*(.*)$', up, re.S)
            if m:
                name, rhs = m.group(1), m.group(2).strip()
                info = lookup(name)
                if not info or not info[1]:
                    continue
                if re.match(r'(VALUE|CORRESPONDING)\s+#?\w*\s*\(', rhs) and \
                        re.search(r'\bFOR\b|\(\s*\(', rhs):
                    findings.append(f'{where}: {name}, a table WITH UNIQUE KEY, is built with '
                                    f'a constructor - a repeated key dumps; insert row by row')
                    continue
                r = re.match(r'(\w+)$', rhs)
                if r:
                    src_name = r.group(1)
                    other = lookup(src_name)
                    if src_name in distinct or (other and other[1] and other[2] == info[2]):
                        continue
                    findings.append(f'{where}: {name}, a table WITH UNIQUE KEY, is assigned '
                                    f'{src_name}, which can repeat a key')

if findings:
    print('\n'.join(findings))
    sys.exit(1)
print('clean - every table with a unique key is filled by INSERT, a SELECT DISTINCT, or '
      'another table with the same key, and nothing is APPENDed to a sorted or hashed table')
