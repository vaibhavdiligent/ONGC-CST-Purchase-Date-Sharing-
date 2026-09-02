"""Per-radio-button binding audit for both programs, against the real workbooks.
   Reports, per scenario: tab chosen, heading row, columns bound by heading /
   by field name / left positional, headings duplicated on the tab, and any
   two map entries that would end up reading the same file column."""
import sys, collections
sys.path.insert(0,'/tmp/claude-0/-home-user-ONGC-CST-Purchase-Date-Sharing-/0a870517-dcc5-5a06-9069-d731d41a85f0/scratchpad')
from sim import sheets, squash, load_cust, load_vend

def run(which, path, scen, named, ents):
    use_fld = which=='C'
    want=set(e['hdr'] for e in ents if e['hdr'])
    if use_fld:
        c=collections.Counter(squash(e['fld']) for e in ents if squash(e['fld']))
        want |= {k for k,n in c.items() if n==1}
    best=(0,None,None,None)
    for name,rows in sheets(path):
        for r in sorted(rows)[:10]:
            keys=set(squash(v) for v in rows[r].values() if squash(v))
            sc=len(want & keys)
            if sc>best[0] or (sc>0 and sc==best[0] and squash(name)==squash(named) and squash(best[1] or '')!=squash(named)):
                best=(sc,name,r,rows[r])
    sc,name,hrow,head = best
    if not name: return f'{scen}: NO TAB MATCHED'
    cnt=collections.Counter(squash(v) for v in head.values() if squash(v))
    dupes=[k for k,n in cnt.items() if n>1]
    pos={squash(v):k for k,v in sorted(head.items()) if squash(v) and cnt[squash(v)]==1}
    fcnt=collections.Counter(squash(e['fld']) for e in ents if squash(e['fld']))
    used={}; byhdr=0; byfld=[]; unbound=[]
    for e in ents:
        e['src']=None
        if e['hdr'] and e['hdr'] in pos:
            e['src']=pos[e['hdr']]; byhdr+=1; used.setdefault(e['src'],[]).append(e['fld'] or e['hdr'])
    if use_fld:
        for e in ents:
            if e['src'] is not None: continue
            k=squash(e['fld'])
            if k and fcnt[k]==1 and k in pos and pos[k] not in used:
                e['src']=pos[k]; used.setdefault(e['src'],[]).append(e['fld'])
                # did we borrow a column whose own heading belongs elsewhere?
                own=[x for x in ents if x['hdr'] and x['hdr']==squash(head[pos[k]])]
                byfld.append((e['fld'], pos[k], squash(head[pos[k]]), bool(own)))
    for e in ents:
        if e['src'] is None: unbound.append((e['fld'] or e['hdr'], e['col']))
    clash={c:v for c,v in used.items() if len(v)>1}
    out=[f'{scen}  tab="{name}" hrow={hrow}  bound {byhdr+len(byfld)}/{len(ents)} '
         f'(heading {byhdr}, field name {len(byfld)})']
    if dupes:   out.append(f'      headings appearing twice on the tab (left positional): {", ".join(sorted(dupes))}')
    if clash:   out.append(f'      !! two fields reading the same column: {clash}')
    borrowed=[b for b in byfld if b[3]]
    if borrowed: out.append(f'      !! field-name match took a column whose own heading is another field: {borrowed}')
    if unbound: out.append(f'      positional ({len(unbound)}): ' + ', '.join(f'{f}@{c}' for f,c in unbound[:12]) + ('' if len(unbound)<=12 else ' ...'))
    return '\n'.join(out)

CUS=[('R1','domestic customer IND'),('R2','Export customer'),('R3','Morocco customer '),
     ('R4','SAGA customer'),('R5','credit Limit'),('R6','domestic customer US'),('R7','ship to party US')]
VEN=[('R1','Vendor creation for All CC'),('R2','TDS upload'),('R3','TAN details'),('R4','BANK Key creation'),
     ('R5','Bank details update'),('R6','Vendor extension'),('R7','CIN details'),('R8','Patner function'),
     ('R9','Block_Unblocked')]
cm=load_cust(); vm=load_vend()
print('### CUSTOMER  ZSDS_CUST_MASS_UPLOAD  vs "customer master LSMW -  with format.xlsx"')
for s,t in CUS: print(run('C','customer master LSMW -  with format.xlsx',s,t,cm[s]))
print('\n### CUSTOMER  R5 vs the customer-supplied credit file')
print(run('C','Credit limit_S4 (1).xlsx','R5','credit Limit',load_cust()['R5']))
print('\n### SUPPLIER  ZMMS_BP_MASS_UPLOAD  vs "Vendor LSMW with Template.xlsx"')
for s,t in VEN: print(run('V','Vendor LSMW with Template.xlsx',s,t,vm[s]))
print('\n### SUPPLIER  R1 vs the customer-supplied single-tab file')
print(run('V','Copy of Vendor Creation Template_All CC_Sample.xlsx','R1','Vendor creation for All CC',load_vend()['R1']))
