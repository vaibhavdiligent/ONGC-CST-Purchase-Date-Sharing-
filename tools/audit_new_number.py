"""A record created with internal numbering has to be told back to the user.

The number comes from the grouping's range during the save, so nothing in
the file has it and nothing the API returns carries it either. The way back
is the GUID the header put in the message: CVI_CUST_LINK / CVI_VEND_LINK
hold it against the customer or vendor that was made.

Miss that and the run says "Posted successfully" with an empty key column,
which is what the supplier program did: the vendor was created and the user
had no way of knowing its number.

Every method that can post a creation - that is, one whose header task can
be I rather than U - has to read the number back and put it on the log.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FILES = {
    'ZSDS_CUST_MASS_UPLOAD': ('src/zsds_cust_mass_upload.prog.abap', 'cust_by_guid'),
    'ZMMS_BP_MASS_UPLOAD':   ('src/zmms_bp_mass_upload.prog.abap',   'vend_by_guid'),
}

def methods(src):
    """(class~method, body) for every method implementation. The nine
       handlers all implement LIF_H~RUN, so the class has to be part of the
       name or they cannot be told apart."""
    out, cur, name, cls = [], [], None, '?'
    for line in src.split('\n'):
        c = re.match(r'CLASS\s+(\w+)\s+IMPLEMENTATION', line)
        if c:
            cls = c.group(1)
        m = re.match(r'\s*METHOD\s+([\w~]+)\s*\.', line)
        if m:
            name, cur = f'{cls}=>{m.group(1)}', []
            continue
        if re.match(r'\s*ENDMETHOD\s*\.', line) and name:
            out.append((name, '\n'.join(cur))); name = None
            continue
        if name is not None:
            cur.append(line)
    return out

bad, creators = [], []
for prog, (path, reader) in FILES.items():
    src = open(os.path.join(ROOT, path), encoding='utf-8').read()
    for name, body in methods(src):
        code = re.sub(r'"[^\n]*', '', body)          # drop comments
        posts = '->post(' in code
        # a creation is a header task that can be I
        creates = bool(re.search(r'(header\(|object_task\s*=|lv_task\s*\)?\s*=)'
                                 r'[^.]*\bgc_i\b', code, re.S)) \
                  or bool(re.search(r'\bTHEN gc_i\b', code))
        if not (posts and creates):
            continue
        creators.append(f'{prog} {name}')
        if reader + '(' not in code:
            bad.append(f'{prog} {name}: posts a creation but never calls {reader}( ) - '
                       f'the new number is never reported')
        elif 'set_key(' not in code:
            bad.append(f'{prog} {name}: reads the new number back but does not put it '
                       f'on the rows already logged for that spreadsheet row')

print('methods that can post a creation: ' + (', '.join(creators) or 'none'))
print('\n'.join(bad) if bad else
      'clean - every creation reads its new number back and puts it on the log')
sys.exit(1 if bad else 0)
