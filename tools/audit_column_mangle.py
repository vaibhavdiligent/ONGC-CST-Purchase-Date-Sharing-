"""For every radio button of both programs: take the real tab, tag each cell
   with its own column, then delete a column and swap two others, and check
   that every mapped field still reads the value of ITS OWN column."""
import sys, collections
sys.path.insert(0,'/tmp/claude-0/-home-user-ONGC-CST-Purchase-Date-Sharing-/0a870517-dcc5-5a06-9069-d731d41a85f0/scratchpad')
from sim import sheets, squash, load_cust, load_vend

def bind(ents, head, use_fld):
    """Mirrors LCL_ENGINE=>BIND_COLUMNS exactly."""
    occ={}; cnt=collections.Counter()
    for c in sorted(head):
        k=squash(head[c])
        if not k: continue
        cnt[k]+=1
        occ[(k,cnt[k])]=c
    mcnt=collections.Counter(e['hdr'] for e in ents if e['hdr'])
    fcnt=collections.Counter(squash(e['fld']) for e in ents if squash(e['fld']))
    used=set(); src={}; seen=collections.Counter()
    for i,e in enumerate(ents):                       # pass 1 - heading, by occurrence
        if not e['hdr']: continue
        seen[e['hdr']]+=1
        if cnt.get(e['hdr'],0)<mcnt[e['hdr']]: continue
        c=occ.get((e['hdr'],seen[e['hdr']]))
        if c is None: continue
        src[i]=c; used.add(c)
    if use_fld:                                       # pass 2 - field name
        for i,e in enumerate(ents):
            if i in src: continue
            k=squash(e['fld'])
            if not k or fcnt[k]!=1 or cnt.get(k,0)!=1: continue
            c=occ[(k,1)]
            if c in used: continue
            src[i]=c; used.add(c)
    bycol={c:squash(head[c]) for c in head if squash(head[c])}
    known=set(e['hdr'] for e in ents if e['hdr']) | {k for k,n in fcnt.items() if n==1}
    return src, used, bycol, known

def check(which, path, scen, tab, hrow, ents):
    use_fld = which=='C'
    rows=dict(sheets(path))[tab]
    head=dict(rows[hrow])
    nxt=rows.get(hrow+1,{})
    want=set(e['hdr'] for e in ents if e['hdr'])
    if len(want & set(squash(v) for v in nxt.values() if squash(v)))>0:
        for c,v in nxt.items():
            if v and not head.get(c): head[c]=v
    width=max(head)
    order=[c for c in range(1,width+1) if c!=1]          # delete original column 1
    if len(order)>9: order[4],order[8]=order[8],order[4] # swap two more
    mhead={i+1:head.get(c,'') for i,c in enumerate(order)}
    mdata={i+1:f'V{c}' for i,c in enumerate(order)}
    src,used,bycol,known = bind([dict(e) for e in ents], mhead, use_fld)
    if which=='V':
        # the supplier reader permutes cells back into template positions
        out={}
        wide=max([ents[i]['col'] for i in src]+[max(mdata)]) if src else max(mdata)
        tgt2src={ents[i]['col']:c for i,c in src.items()}
        claimed=set(tgt2src.values())
        for t in range(1,wide+1):
            s=tgt2src.get(t, 0 if t in claimed else t)
            out[t]=mdata.get(s,'') if s else ''
        read=lambda i,e: out.get(e['col'],'')
    else:
        def read(i,e):
            if i in src: return mdata.get(src[i],'')
            if not src: return mdata.get(e['col'],'')
            if e['col'] in used: return ''
            k=bycol.get(e['col'])
            if k and k!=e['hdr'] and k!=squash(e['fld']) and k in known: return ''
            return mdata.get(e['col'],'')
    bad=[]; lost=[]
    for i,e in enumerate(ents):
        want=f"V{e['col']}"
        got=read(i,e)
        if e['col']==1: continue          # the column deliberately deleted
        if got==want: continue
        if got=='' : lost.append(e['fld'] or e['hdr'])
        else:        bad.append((e['fld'] or e['hdr'], e['col'], got))
    status = 'OK' if not bad else f'!! WRONG VALUE: {bad[:6]}'
    print(f'  {scen:<3} {tab[:28]:<30} fields {len(ents):>3}  wrong {len(bad):>2}  '
          f'not read {len(lost):>2}  {status}')
    if lost[:6] and not bad: print(f'        (not read - heading missing/duplicated: {", ".join(lost[:6])}'
                                   f'{" ..." if len(lost)>6 else ""})')

CUS=[('R1','domestic customer IND',2),('R2','Export customer',1),('R3','Morocco customer ',1),
     ('R4','SAGA customer',2),('R5','credit Limit',1),('R6','domestic customer US',1),('R7','ship to party US',1)]
VEN=[('R1','Vendor creation for All CC',1),('R2','TDS upload',1),('R3','TAN details',1),
     ('R4','BANK Key creation',1),('R5','Bank details update',1),('R6','Vendor extension',1),
     ('R7','CIN details',1),('R8','Patner function',6),('R9','Block_Unblocked',4)]
cm=load_cust(); vm=load_vend()
print('CUSTOMER - column deleted and two swapped, every radio button:')
for s,t,h in CUS: check('C','customer master LSMW -  with format.xlsx',s,t,h,cm[s])
print('\nSUPPLIER - column deleted and two swapped, every radio button:')
for s,t,h in VEN: check('V','Vendor LSMW with Template.xlsx',s,t,h,vm[s])
