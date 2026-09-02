"""Simulates the reader's tab/heading resolution exactly as the two programs do it."""
import re,sys,zipfile,collections
from xml.etree import ElementTree as ET
NS='{http://schemas.openxmlformats.org/spreadsheetml/2006/main}'
R='{http://schemas.openxmlformats.org/officeDocument/2006/relationships}'
def squash(x): return re.sub(r'[^A-Z0-9]','',(x or '').upper())
def sheets(path):
    z=zipfile.ZipFile(path)
    wb=ET.fromstring(z.read('xl/workbook.xml'))
    rels={r.get('Id'):r.get('Target') for r in ET.fromstring(z.read('xl/_rels/workbook.xml.rels'))}
    try: ss=[''.join(t.text or '' for t in si.iter(NS+'t')) for si in ET.fromstring(z.read('xl/sharedStrings.xml'))]
    except KeyError: ss=[]
    out=[]
    for sh in wb.iter(NS+'sheet'):
        t=rels[sh.get(R+'id')].lstrip('/')
        t=t if t.startswith('xl/') else 'xl/'+t
        w=ET.fromstring(z.read(t)); rows={}
        for row in w.iter(NS+'row'):
            rn=int(row.get('r')); d={}
            for c in row.iter(NS+'c'):
                v=c.find(NS+'v'); x='' if v is None else (v.text or '')
                if c.get('t')=='s': x=ss[int(x)]
                elif c.get('t')=='inlineStr':
                    x=''.join(t.text or '' for t in c.iter(NS+'t'))
                n=0
                for ch in ''.join(ch for ch in c.get('r') if ch.isalpha()): n=n*26+ord(ch)-64
                d[n]=(x or '').strip()
            rows[rn]=d
        out.append((sh.get('name'),rows))
    return out
def load_cust():
    s=open('src/zsds_cust_mass_upload.prog.abap',encoding='utf-8').read()
    m=collections.defaultdict(list)
    for x in re.finditer(r"scen = '(\w+)' col = (\d+)\s+node = '(\w?)' fld = '([^']*)' cnv = '([^']*)' hdr = '([^']*)'",s):
        m[x.group(1)].append(dict(col=int(x.group(2)),node=x.group(3),fld=x.group(4),hdr=x.group(6)))
    return m
def load_vend():
    s=open('src/zmms_bp_mass_upload.prog.abap',encoding='utf-8').read()
    m=collections.defaultdict(list)
    for x in re.finditer(r"\(\s*scen = '(R\d)'\s+col = (\d+)\s+hdr = '([^']*)'\s*\)",s):
        m[x.group(1)].append(dict(col=int(x.group(2)),hdr=x.group(3),fld=''))
    return m
def want(entries, use_fld):
    keys=set(e['hdr'] for e in entries if e['hdr'])
    if use_fld:
        c=collections.Counter(squash(e['fld']) for e in entries if squash(e['fld']))
        keys |= {k for k,n in c.items() if n==1}
    return keys
def resolve(path, entries, use_fld, named):
    W=want(entries,use_fld); best=(0,None,None,None)
    for name,rows in sheets(path):
        for r in sorted(rows)[:10]:
            head=rows[r]
            keys=set(squash(v) for v in head.values() if squash(v))
            sc=len(W & keys)
            if sc>best[0] or (sc>0 and sc==best[0] and squash(name)==squash(named) and squash(best[1] or '')!=squash(named)):
                best=(sc,name,r,head)
    return best
if __name__=='__main__':
    which,path,scen,named = sys.argv[1],sys.argv[2],sys.argv[3],sys.argv[4]
    ents = (load_cust() if which=='C' else load_vend())[scen]
    use_fld = which=='C'
    sc,name,r,head = resolve(path,ents,use_fld,named)
    print(f'file={path}\n scenario={scen} expected tab="{named}"  -> chosen tab="{name}" heading row={r} score={sc} (of {len(want(ents,use_fld))} keys)')
    if not name: sys.exit(1)
    # bind
    cnt=collections.Counter(squash(v) for v in head.values() if squash(v))
    pos={squash(v):k for k,v in sorted(head.items()) if squash(v) and cnt[squash(v)]==1}
    done=set(); used=set(); moved=[]; unbound=[]
    for e in ents:
        if e['hdr'] and e['hdr'] in pos:
            if pos[e['hdr']]!=e['col']: moved.append((e['fld'] or e['hdr'],e['col'],pos[e['hdr']]))
            e['new']=pos[e['hdr']]; done.add(id(e)); used.add(pos[e['hdr']])
    if use_fld:
        fcnt=collections.Counter(squash(e['fld']) for e in ents if squash(e['fld']))
        for e in ents:
            if id(e) in done: continue
            k=squash(e['fld'])
            if k and fcnt[k]==1 and k in pos and pos[k] not in used:
                if pos[k]!=e['col']: moved.append((e['fld'],e['col'],pos[k]))
                e['new']=pos[k]; used.add(pos[k])
    for e in ents:
        if 'new' not in e: unbound.append((e['fld'] or e['hdr'], e['col']))
    print(f' bound={len(ents)-len(unbound)}/{len(ents)}  moved={len(moved)}')
    if moved: print('  moved:', ', '.join(f'{f}:{a}->{b}' for f,a,b in moved[:40]))
    if unbound: print('  positional (heading not found/ambiguous):', ', '.join(f'{f}@{c}' for f,c in unbound[:40]))
