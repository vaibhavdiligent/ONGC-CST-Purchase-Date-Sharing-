import zipfile, json, re
from xml.etree import ElementTree as ET
NS='{http://schemas.openxmlformats.org/spreadsheetml/2006/main}'
def rows(f):
    z=zipfile.ZipFile(f)
    try: ss=[''.join(t.text or '' for t in si.iter(NS+'t')) for si in ET.fromstring(z.read('xl/sharedStrings.xml'))]
    except KeyError: ss=[]
    w=ET.fromstring(z.read([n for n in z.namelist() if n.startswith('xl/worksheets/')][0]))
    out=[]
    for row in w.iter(NS+'row'):
        v=[]
        for c in row.iter(NS+'c'):
            x=c.find(NS+'v'); t='' if x is None else x.text
            if c.get('t')=='s': t=ss[int(t)]
            v.append(t)
        out.append(v)
    return out

# ---- field -> (domain, length), from every DDIC extract available -------
FLD={}
for f in ['/tmp/vm/vendor_master/tier_2.xlsx','/tmp/vm/vendor_master/dd03l_1.xlsx',
          '/home/user/ONGC-CST-Purchase-Date-Sharing-/dd03l_new_2.xlsx']:
    try: r=rows(f)
    except Exception as e: print('skip',f,e); continue
    h=r[0]
    if 'TABNAME' not in h: continue
    ti,fi=h.index('TABNAME'),h.index('FIELDNAME')
    dm=h.index('DOMNAME') if 'DOMNAME' in h else None
    ln=h.index('LENG') if 'LENG' in h else None
    for x in r[1:]:
        if len(x)<=fi: continue
        d=x[dm] if dm is not None and len(x)>dm else ''
        L=x[ln] if ln is not None and len(x)>ln else ''
        FLD.setdefault((x[ti],x[fi]),(d,L))
# ---- domain -> convexit ------------------------------------------------
CONV={}
r=rows('/home/user/ONGC-CST-Purchase-Date-Sharing-/dd01l.xlsx'); h=r[0]
di,ci=h.index('DOMNAME'),h.index('CONVEXIT')
for x in r[1:]:
    if len(x)>ci: CONV[x[di]]=x[ci]
print(f'DDIC coverage: {len(FLD)} fields, {len(CONV)} domains with a known conversion exit setting')
print(f'domains carrying ALPHA: {sorted(d for d,c in CONV.items() if c)}')

VEN=open('/home/user/ONGC-CST-Purchase-Date-Sharing-/src/zmms_bp_mass_upload.prog.abap').read()
CUS=open('/home/user/ONGC-CST-Purchase-Date-Sharing-/src/zsds_cust_mass_upload.prog.abap').read()

def dom(tabs, f):
    for tb in tabs:
        if (tb,f) in FLD:
            d,L = FLD[(tb,f)]
            return tb,d,L
    return None,'',''

# ---------- supplier: every field name it writes -----------------------
VTABS=['LFA1','LFB1','LFM1','LFBK','LFBW','WYT3','BNKA','ADRC','ADR2','ADR3','ADR6']
vf=set()
for m in re.finditer(r"\|([A-Z][A-Z0-9_]{1,20});(\d+);([0-9]*)\|", VEN):
    vf.add((m.group(1), m.group(3)))
for m in re.finditer(r"iv_comp = '([A-Z][A-Z0-9_]{1,20})'", VEN):
    vf.add((m.group(1), None))
# ---------- customer: from the column map ------------------------------
NODE_TAB={'C':'KNA1','B':'KNB1','S':'KNVV','A':'ADRC','Z':'ZSD_LICENSE_CHK','K':'KNA1'}
cf=set()
for m in re.finditer(r"node = '(\w)' fld = '([A-Z][A-Z0-9_]*)' cnv = '(\w*)'", CUS):
    n,f,c=m.groups()
    if n in NODE_TAB: cf.add((NODE_TAB[n],f,c))

def report(title, items, get):
    need=[]; ok=[]; unknown=[]
    for it in items:
        tb,d,L,f,applied = get(it)
        if not d: continue
        ex = CONV.get(d, None)
        if ex is None:      unknown.append((f,tb,d))
        elif ex:            need.append((f,tb,d,ex,L,applied))
        else:               ok.append((f,tb,d))
    print(f'\n===== {title}')
    print(f'  fields whose domain has NO conversion exit : {len(ok)}')
    print(f'  fields whose domain HAS a conversion exit  : {len(need)}')
    for f,tb,d,ex,L,applied in sorted(set(need)):
        print(f'      {f:12s} {tb:16s} domain {d:8s} {ex:6s} len {L:3s} -> padded in code: {applied}')
    print(f'  domains not in the DD01L extract (unknown)  : {len(set(x[2] for x in unknown))}')
    return sorted(set(x[2] for x in unknown))

u1=report('SUPPLIER  ZMMS_BP_MASS_UPLOAD',
   vf, lambda it: (lambda tb,d,L: (tb,d,L,it[0], ('yes, len '+it[1]) if it[1] else 'NO LENGTH GIVEN'))(*dom(VTABS,it[0])))
u2=report('CUSTOMER  ZSDS_CUST_MASS_UPLOAD',
   cf, lambda it: (lambda tb,d,L: (tb,d,L,it[1], 'yes (AL/GL, length from field)' if it[2] in ('AL','GL') else 'no'))(*dom([it[0]],it[1])))
open('unknown_domains.txt','w').write('\n'.join(sorted(set(u1+u2))))
print(f'\nDistinct domains I cannot confirm: {len(set(u1+u2))}')
