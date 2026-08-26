# Verifies every structure path used by the program, harvested from the
# source, including the line type of every VALUE <type>( ) constructor
# that is APPENDed to a table.
import json,re,sys
D=json.load(open('ddic2.json'))
def rows(t): return sorted(D.get(t,[]),key=lambda x:x['pos'])
def kind(c): return {'S':'STRUCT','L':'TABLE','E':'elem','R':'ref'}.get(c['ct'],c['ct'] or 'flat')
def ref(c):  return c['prec'] or c['reft'] or c['roll']
def children(t,parent=None):
    rs=rows(t)
    if parent is None: return {c['f']:c for c in rs if str(c['depth'])=='0'}
    out={}; col=False
    for c in rs:
        if str(c['depth'])=='0': col=(c['f']==parent); continue
        if col and str(c['depth'])=='1': out[c['f']]=c
    return out
def line_of(tt):
    """Line type of a table type. SAP's CVI communication tables use a
       <base>_STR line type; everything else drops the trailing _T."""
    base = tt[:-2] if tt.endswith('_T') else tt
    for cand in (base+'_STR', base, tt):
        if cand in D: return cand
    return None
def walk(root,path):
    cur=root; pend=None
    for s in [x.upper() for x in path.split('-')]:
        cc=children(cur,pend) if pend else children(cur)
        if s not in cc: return False,f"'{s}' not a component of {cur}"+(f" (under {pend})" if pend else ""),None
        c=cc[s]; k=kind(c); r=ref(c); pend=None
        if k=='TABLE':
            ln=line_of(r)
            if ln is None: return False,f"line type of {r} missing from extract",None
            cur=ln
        elif k=='STRUCT':
            if r in D: cur=r
            else: pend=s
        else: cur=None
    return True,'ok',cur

prog=open('/home/user/ONGC-CST-Purchase-Date-Sharing-/src/zsds_cust_mass_upload.prog.abap').read()
fails=0; checked=0

# 1. dotted paths off known root variables
ROOTS={'ls_cust':'CMDS_EI_EXTERN','ls_bp':'BUS_EI_EXTERN','ls_cvis':'CVIS_EI_EXTERN',
       'ls_comp':'CMDS_EI_COMPANY','ls_sale':'CMDS_EI_SALES'}
seen=set()
for var,root in ROOTS.items():
    for m in re.finditer(rf"\b{var}-([a-z_0-9-]+)", prog):
        seen.add((root,m.group(1)))
for root,path in sorted(seen):
    checked+=1
    ok,msg,_=walk(root,path)
    if not ok: print(f"  FAIL {root}-{path}\n        {msg}"); fails+=1

# 2. every  APPEND VALUE <type>( ... ) TO <path>  - check the constructor
#    type really IS the line type of the target table, and the fields exist
for m in re.finditer(r"APPEND VALUE (\w+)\((.*?)\)\s*\n?\s*TO ([a-z_0-9-]+)\.", prog, re.S):
    ctype, body, target = m.group(1).upper(), m.group(2), m.group(3)
    root_var = target.split('-')[0]
    if root_var not in ROOTS:            # e.g. a local table
        continue
    checked+=1
    ok,msg,line = walk(ROOTS[root_var], target.split('-',1)[1])
    if not ok:
        print(f"  FAIL target {target}: {msg}"); fails+=1; continue
    if line != ctype:
        print(f"  FAIL {target}: constructor is {ctype} but the line type is {line}")
        fails+=1; continue
    for f in re.finditer(r"([a-z_][a-z_0-9-]*)\s*=", body):
        checked+=1
        ok2,msg2,_=walk(ctype, f.group(1))
        if not ok2: print(f"  FAIL {ctype}-{f.group(1)}: {msg2}"); fails+=1
print(f"\nchecks run: {checked}, failed: {fails}")
sys.exit(1 if fails else 0)
