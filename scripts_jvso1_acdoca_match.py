import pandas as pd,sys
d=sys.argv[1]; J=pd.read_pickle(d+'/J.pkl').fillna(''); A=pd.read_pickle(d+'/A.pkl').fillna('')
s=lambda x: x.astype(str).str.lstrip('0')
A['doc']=A[2]+'|'+A[3]; A['ln']=s(A[15]); A['key']=A.doc+'|'+A.ln
A['awdoc']=A[2]+'|'+A[14]
J['doc']=J[46]+'|'+J[81]; J['awdoc']=J[46]+'|'+J[45]; J['ln']=s(J[50]); J['key']=J.doc+'|'+J.ln
J['awkey']=J.awdoc+'|'+s(J[47])
A['amt']=pd.to_numeric(A[20],errors='coerce').round(2); J['amt']=pd.to_numeric(J[34],errors='coerce').round(2)
J['jva']=list(zip(J[13],J[14],J[25]))
Jsel=J[['doc','awdoc','key','awkey','amt','jva',10,11,21]]
bykey={k:g for k,g in Jsel.groupby('key')}
byawkey={k:g for k,g in Jsel.groupby('awkey')}
bydoc={k:g for k,g in Jsel.groupby('doc')}
byawco={k:g for k,g in J[J[48]=='K'][['doc','awdoc','key','awkey','amt','jva',10,11,21]].groupby('awdoc')}
acnt=A[A.ln!=''].groupby('key').size().to_dict()
def jvref(g): 
    x=sorted(set(g[10])); return ', '.join(x[:3])+(' …' if len(x)>3 else '')
out=[]
for r in A.itertuples(index=False):
    cur=(r[35],r[36],r[37]); prop=None; basis=''; jvdoc=''; jvln=''; note=''
    g=bydoc.get(r.doc); docsrc='REFFIDOC'
    if g is None and r[13]=='COBK' and r.awdoc in byawco: g=byawco[r.awdoc]; docsrc='REFDOCNR'
    if g is None:
        out.append(('','','','No JV document in JVSO1','No proposal','','','','')); continue
    lm=None
    if r.ln!='':
        lm=bykey.get(r.key) if docsrc=='REFFIDOC' else byawkey.get(r.awdoc+'|'+r.ln)
        if lm is not None and acnt.get(r.key,1)>1:
            lm2=lm[lm.amt==r.amt]
            lm=lm2 if len(lm2) else lm
    if lm is not None and len(lm):
        u=set(lm.jva)
        jvdoc=jvref(lm); jvln=', '.join(sorted(set(lm[11]),key=lambda x:int(x) if x.isdigit() else 0)[:5])
        if len(u)==1:
            prop=u.pop(); basis=f'Line match: {docsrc} + ref. line item'
        else:
            basis=f'Line match: {docsrc} + ref. line item'
            note='JVSO1 has '+str(len(u))+' different JV assignments for this line: '+'; '.join('/'.join(x) for x in sorted(u))
    else:
        jvdoc=jvref(g)
        gp=g[g[21]==r[18]] if r[18] else g.iloc[0:0]
        if len(gp) and len(set(gp.jva))==1:
            prop=gp.jva.iloc[0]; basis='Derived: same document + profit center'
        elif len(set(g.jva))==1:
            prop=g.jva.iloc[0]; basis='Derived: single JV assignment in document'
        else:
            basis='Derived: not possible'
            note='Line not in JVSO1 and document has several JV assignments'
    if prop is None:
        action='No proposal'
        status='Ambiguous' if note else 'No proposal'
        out.append(('','','',status,action,basis,jvdoc,jvln,note)); continue
    diff=[n for n,c,p in zip(('VNAME','EGRUP','RECID'),cur,prop) if c!=p]
    missing=[n for n,c,p in zip(('VNAME','EGRUP','RECID'),cur,prop) if c!=p and c=='']
    wrong=[n for n in diff if n not in missing]
    if not diff: status='OK - matches JVSO1'; action='No change'
    elif wrong:
        status='Differs from JVSO1: '+', '.join(wrong)+((' / missing: '+', '.join(missing)) if missing else '')
        action='Review' if basis.startswith('Derived') else 'Update (correct value)'
    else:
        status='Missing in ACDOCA: '+', '.join(missing); action='Update (fill missing)'
    out.append((*prop,status,action,basis,jvdoc,jvln,note))
O=pd.DataFrame(out,columns=['Proposed VNAME','Proposed EGRUP','Proposed RECID','Check result','Proposed action','Proposal basis','JVSO1 document(s)','JVSO1 line(s)','Remarks'],index=A.index)
O.to_pickle(d+'/O.pkl')
print(O['Proposed action'].value_counts().to_dict())
print(pd.crosstab(O['Proposal basis'],O['Proposed action']))
print(O['Check result'].value_counts().head(20))
bl=A[35]==''
print('blank-venture lines:',bl.sum()); print(O[bl]['Proposed action'].value_counts().to_dict()); print(O[bl]['Proposal basis'].value_counts().to_dict())
