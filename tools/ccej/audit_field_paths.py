"""Every field the CCBJI customer program can offer has a place in the API.

The program is dynamic: its field catalogue is the fields of a master table
that are also components of the API's DATA structure, and the upload writes
each one by name - DATA-<field> with its flag DATAX-<field> - into the line
the part's path leads to. This walks the dictionary extract (tools/ddic.json)
the way the program does and checks, for the customer side:

  - every catalogue field of every part: DATA-<field> and DATAX-<field> exist
    at the path the upload writes to, with TASK and the DATA_KEY fields;
  - the paths LCL_CAT=>API_PATH names are the ones checked here;
  - every fixed path the upload uses: header, address, communication on the
    customer and on the business partner, texts, roles, the partner's bank
    details, tax numbers, name, search terms and title.

The supplier structures are not in the extract; they mirror the customer's,
and the program reads their fields from the real structure at runtime
(LCL_CAT=>PATH_COMPS) when a structure name gives nothing.

    python3 tools/ccej/audit_field_paths.py
"""
import json, os, re, sys
R=os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
D=json.load(open(os.path.join(R,'tools/ddic.json')))
CAT=open(os.path.join(R,'tools/ccej/bp_maint/30_cat.abap'),encoding='utf-8').read()
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
    base = tt[:-2] if tt.endswith('_T') else tt
    for cand in (base+'_STR', base, tt):
        if cand in D: return cand
    return None
def walk(root,path):
    cur=root; pend=None
    for s in [x.upper() for x in path.split('-')]:
        if cur is None: return False,f"'{s}' below an elementary field",None
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
def flds(t): return {c['f'] for c in rows(t) if not c['f'].startswith('.')}
ROOT='CVIS_EI_EXTERN'
groups=[ # grp, table, api data struct, path to the line/struct that holds TASK/DATA_KEY/DATA/DATAX, keys
 ('CENT','KNA1','CMDS_EI_VMD_CENTRAL_DATA','CUSTOMER-CENTRAL_DATA-CENTRAL',''),
 ('COMP','KNB1','CMDS_EI_COMPANY_DATA','CUSTOMER-COMPANY_DATA-COMPANY','BUKRS'),
 ('SALE','KNVV','CMDS_EI_SALES_DATA','CUSTOMER-SALES_DATA-SALES','VKORG VTWEG SPART'),
 ('TAXI','KNVI','CMDS_EI_TAX_IND_DATA','CUSTOMER-CENTRAL_DATA-TAX_IND-TAX_IND','ALAND TATYP'),
 ('UNLD','KNVA','CMDS_EI_LOADING_DATA','CUSTOMER-CENTRAL_DATA-LOADING-LOADING','ABLAD'),
 ('CONT','KNVK','CMDS_EI_CONTACTS_DATA','CUSTOMER-CENTRAL_DATA-CONTACT-CONTACTS','PARNR'),
 ('PART','KNVP','CMDS_EI_FUNCTIONS_DATA','CUSTOMER-SALES_DATA-SALES-FUNCTIONS-FUNCTIONS','PARVW PARZA'),
 ('DUNN','KNB5','CMDS_EI_DUNNING_DATA','CUSTOMER-COMPANY_DATA-COMPANY-DUNNING-DUNNING','MABER'),
 ('WTAX','KNBW','CMDS_EI_WTAX_DATA','CUSTOMER-COMPANY_DATA-COMPANY-WTAX_TYPE-WTAX_TYPE','WITHT'),
]
bad=0; tot=0
for g,tab,api,path,keys in groups:
    ok,msg,line=walk(ROOT,path)
    if not ok: print(f'FAIL {g}: path {path}: {msg}'); bad+=1; continue
    apif=flds(api)
    if tab in D:
        cat=sorted((flds(tab)&apif)-set(keys.split()))
    else:
        cat=sorted(apif)        # table not in the extract: every API field is checked instead
    miss=[]
    for f in cat:
        tot+=1
        for part in ('DATA','DATAX'):
            ok,msg,_=walk(ROOT,f'{path}-{part}-{f}')
            if not ok: miss.append(f'{part}-{f}: {msg}')
    for k in keys.split():
        ok,msg,_=walk(ROOT,f'{path}-DATA_KEY-{k}')
        if not ok: miss.append(f'DATA_KEY-{k}: {msg}')
    if keys:
        ok,msg,_=walk(ROOT,f'{path}-TASK')
        if not ok: miss.append('TASK: '+msg)
    if miss: print(f'FAIL {g} ({tab}, {len(cat)} fields, line {line}): {len(miss)} problem(s)')
    for m in miss[:12]: print('      ',m)
    bad+=len(miss)

# the paths LCL_CAT=>API_PATH gives must be the ones checked above
m=re.search(r"METHOD api_path\.(.*?)ENDMETHOD\.",CAT,re.S)
if not m:
    print('FAIL LCL_CAT has no API_PATH any more - update this audit'); bad+=1
else:
    for g,tab,api,path,keys in groups:
        tail=path.replace('CUSTOMER-','')
        tail=tail.replace('SALES_DATA-SALES','{ lv_area }')
        want='{ lv_root }-'+tail+'-DATA'
        if want not in m.group(1):
            print(f'FAIL {g}: LCL_CAT=>API_PATH does not name {want}'); bad+=1
# address / communication / texts / header / partner side
paths=['CUSTOMER-HEADER-OBJECT_TASK','CUSTOMER-HEADER-OBJECT_INSTANCE-KUNNR','CUSTOMER-CENTRAL_DATA-ADDRESS-TASK',
 'CUSTOMER-CENTRAL_DATA-CENTRAL-DATA-KTOKD','CUSTOMER-CENTRAL_DATA-CENTRAL-DATAX-KTOKD',
 'CUSTOMER-CENTRAL_DATA-ADDRESS-COMMUNICATION-PHONE-PHONE-CONTACT-TASK',
 'CUSTOMER-CENTRAL_DATA-ADDRESS-COMMUNICATION-PHONE-PHONE-CONTACT-DATA-TELEPHONE',
 'CUSTOMER-CENTRAL_DATA-ADDRESS-COMMUNICATION-PHONE-PHONE-CONTACT-DATAX-TELEPHONE',
 'CUSTOMER-CENTRAL_DATA-ADDRESS-COMMUNICATION-PHONE-PHONE-CONTACT-DATA-EXTENSION',
 'CUSTOMER-CENTRAL_DATA-ADDRESS-COMMUNICATION-PHONE-PHONE-CONTACT-DATA-R_3_USER',
 'CUSTOMER-CENTRAL_DATA-ADDRESS-COMMUNICATION-PHONE-PHONE-CONTACT-DATAX-R_3_USER',
 'CUSTOMER-CENTRAL_DATA-ADDRESS-COMMUNICATION-PHONE-PHONE-CONTACT-DATA-CONSNUMBER',
 'CUSTOMER-CENTRAL_DATA-ADDRESS-COMMUNICATION-PHONE-PHONE-CONTACT-DATAX-CONSNUMBER',
 'CUSTOMER-CENTRAL_DATA-ADDRESS-COMMUNICATION-PHONE-PHONE-CONTACT-DATA-STD_NO',
 'CUSTOMER-CENTRAL_DATA-ADDRESS-COMMUNICATION-PHONE-PHONE-CONTACT-DATAX-STD_NO',
 'CUSTOMER-CENTRAL_DATA-ADDRESS-COMMUNICATION-FAX-FAX-CONTACT-DATA-FAX',
 'CUSTOMER-CENTRAL_DATA-ADDRESS-COMMUNICATION-FAX-FAX-CONTACT-DATA-EXTENSION',
 'CUSTOMER-CENTRAL_DATA-ADDRESS-COMMUNICATION-SMTP-SMTP-CONTACT-DATA-E_MAIL',
 'CUSTOMER-CENTRAL_DATA-ADDRESS-COMMUNICATION-SMTP-SMTP-CONTACT-DATAX-E_MAIL',
 'CUSTOMER-CENTRAL_DATA-TEXT-TEXTS-TASK','CUSTOMER-CENTRAL_DATA-TEXT-TEXTS-DATA_KEY-TEXT_ID','CUSTOMER-CENTRAL_DATA-TEXT-TEXTS-DATA_KEY-LANGU',
 'CUSTOMER-COMPANY_DATA-COMPANY-TEXTS-TEXTS-DATA_KEY-TEXT_ID','CUSTOMER-SALES_DATA-SALES-TEXTS-TEXTS-DATA_KEY-TEXT_ID',
 'PARTNER-HEADER-OBJECT_TASK','PARTNER-HEADER-OBJECT_INSTANCE-BPARTNERGUID',
 'PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CONTROL-CATEGORY','PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CONTROL-GROUPING',
 'PARTNER-CENTRAL_DATA-ROLE-ROLES-TASK','PARTNER-CENTRAL_DATA-ROLE-ROLES-DATA_KEY',
 'PARTNER-CENTRAL_DATA-BANKDETAIL-BANKDETAILS-TASK','PARTNER-CENTRAL_DATA-BANKDETAIL-BANKDETAILS-DATA_KEY',
 'PARTNER-CENTRAL_DATA-TAXNUMBER-TAXNUMBERS-TASK','PARTNER-CENTRAL_DATA-TAXNUMBER-TAXNUMBERS-DATA_KEY-TAXTYPE',
 'PARTNER-CENTRAL_DATA-TAXNUMBER-TAXNUMBERS-DATA_KEY-TAXNUMBER','PARTNER-CENTRAL_DATA-TAXNUMBER-TAXNUMBERS-DATA_KEY-TAXNUMXL',
 'PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES-TASK','PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES-DATA-POSTAL-DATA-COUNTRY',
 'PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES-DATA-COMMUNICATION-PHONE-PHONE-CONTACT-DATA-TELEPHONE',
 'PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES-DATA-COMMUNICATION-PHONE-PHONE-CONTACT-DATA-R_3_USER',
 'PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES-DATA-COMMUNICATION-PHONE-PHONE-CONTACT-DATA-STD_NO',
 'PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES-DATA-COMMUNICATION-PHONE-PHONE-CONTACT-DATA-EXTENSION',
 'PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES-DATA-COMMUNICATION-FAX-FAX-CONTACT-DATA-FAX',
 'PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES-DATA-COMMUNICATION-SMTP-SMTP-CONTACT-DATA-E_MAIL',
 'PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES-DATA-COMMUNICATION-SMTP-SMTP-CONTACT-DATA-STD_NO',
 'PARTNER-HEADER-OBJECT_INSTANCE-BPARTNER']
for o in ('NAME1','NAME2','NAME3','NAME4'):
    paths += [f'PARTNER-CENTRAL_DATA-COMMON-DATA-BP_ORGANIZATION-{o}', f'PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_ORGANIZATION-{o}']
for o in ('SEARCHTERM1','SEARCHTERM2','TITLE_KEY'):
    paths += [f'PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-{o}', f'PARTNER-CENTRAL_DATA-COMMON-DATAX-BP_CENTRALDATA-{o}']
for b in ('BANK_CTRY','BANK_KEY','BANK_ACCT','ACCOUNTHOLDER','CTRL_KEY','BANK_REF','COLL_AUTH'):
    paths += [f'PARTNER-CENTRAL_DATA-BANKDETAIL-BANKDETAILS-DATA-{b}', f'PARTNER-CENTRAL_DATA-BANKDETAIL-BANKDETAILS-DATAX-{b}']
# address fields
adr=[('NAME1','NAME'),('NAME2','NAME_2'),('NAME3','NAME_3'),('NAME4','NAME_4'),('NAME_CO','C_O_NAME'),('SORT1','SORT1'),('SORT2','SORT2'),('TITLE','TITLE'),('STREET','STREET'),('HOUSE_NUM1','HOUSE_NO'),('HOUSE_NUM2','HOUSE_NO2'),('STR_SUPPL1','STR_SUPPL1'),('STR_SUPPL2','STR_SUPPL2'),('STR_SUPPL3','STR_SUPPL3'),('LOCATION','LOCATION'),('BUILDING','BUILDING'),('FLOOR','FLOOR'),('ROOMNUMBER','ROOM_NO'),('CITY1','CITY'),('CITY2','DISTRICT'),('POST_CODE1','POSTL_COD1'),('POST_CODE2','POSTL_COD2'),('PO_BOX','PO_BOX'),('COUNTRY','COUNTRY'),('REGION','REGION'),('LANGU','LANGU'),('TRANSPZONE','TRANSPZONE'),('TIME_ZONE','TIME_ZONE'),('TAXJURCODE','TAXJURCODE'),('DEFLT_COMM','COMM_TYPE'),('EXTENSION1','EXTENS_1'),('EXTENSION2','EXTENS_2')]
adrc=flds('ADRC')
OWN={'NAME','NAME_2','NAME_3','NAME_4','SORT1','SORT2','TITLE'}   # fields of the partner itself, see BUILD
for a,api in adr:
    if a not in adrc: continue
    paths += [f'CUSTOMER-CENTRAL_DATA-ADDRESS-POSTAL-DATA-{api}', f'CUSTOMER-CENTRAL_DATA-ADDRESS-POSTAL-DATAX-{api}']
    if api not in OWN and api not in ('EXTENS_1','EXTENS_2'):
        paths += [f'PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES-DATA-POSTAL-DATA-{api}', f'PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES-DATA-POSTAL-DATAX-{api}']
pb=0
for p in paths:
    ok,msg,_=walk(ROOT,p)
    if not ok: print('FAIL',p,'->',msg); pb+=1
if bad or pb:
    print(f'{bad+pb} problem(s)')
    sys.exit(1)
print(f'clean - {tot} catalogue fields of 9 parts and {len(paths)} fixed paths of the upload exist in the API structures')
