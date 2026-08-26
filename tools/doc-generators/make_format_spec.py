import zipfile,json,re
from xml.etree import ElementTree as ET
from docx import Document
from docx.shared import Pt, Cm, RGBColor
from docx.enum.table import WD_TABLE_ALIGNMENT

NS='{http://schemas.openxmlformats.org/spreadsheetml/2006/main}'
R='{http://schemas.openxmlformats.org/officeDocument/2006/relationships}'
def letters(n):
    s=''
    while n: n,r=divmod(n-1,26); s=chr(65+r)+s
    return s
def rowsof(path,upto=12):
    z=zipfile.ZipFile(path)
    wb=ET.fromstring(z.read('xl/workbook.xml'))
    rels={r.get('Id'):r.get('Target') for r in ET.fromstring(z.read('xl/_rels/workbook.xml.rels'))}
    ss=[''.join(t.text or '' for t in si.iter(NS+'t')) for si in ET.fromstring(z.read('xl/sharedStrings.xml'))]
    def cn(ref):
        s=''.join(c for c in ref if c.isalpha()); n=0
        for c in s: n=n*26+ord(c)-64
        return n
    out={}
    for sh in wb.iter(NS+'sheet'):
        nm=sh.get('name'); t=rels[sh.get(R+'id')]; t=t if t.startswith('xl/') else 'xl/'+t
        w=ET.fromstring(z.read(t)); rr={}
        for row in w.iter(NS+'row'):
            r=int(row.get('r'))
            if r>upto: break
            d={}
            for c in row.iter(NS+'c'):
                v=c.find(NS+'v'); x='' if v is None else v.text
                if c.get('t')=='s': x=ss[int(x)]
                x=(x or '').strip()
                if x: d[cn(c.get('r'))]=x
            rr[r]=d
        out[nm]=rr
    return out
VEN=rowsof('/home/user/ONGC-CST-Purchase-Date-Sharing-/Vendor LSMW with Template.xlsx')
D=json.load(open('/tmp/claude-0/-home-user-ONGC-CST-Purchase-Date-Sharing-/0a870517-dcc5-5a06-9069-d731d41a85f0/scratchpad/ddic2.json'))
DLEN=json.load(open('/tmp/claude-0/-home-user-ONGC-CST-Purchase-Date-Sharing-/0a870517-dcc5-5a06-9069-d731d41a85f0/scratchpad/ddic.json'))
prog=open('/home/user/ONGC-CST-Purchase-Date-Sharing-/src/zsds_cust_mass_upload.prog.abap').read()
MAP=re.findall(r"scen = '(\w+)' col = (\d+)\s+node = '(\w)' fld = '([^']*)' cnv = '(\w*)' \)(?:\s+\" (.*))?", prog)

doc=Document(); s=doc.styles['Normal']; s.font.name='Calibri'; s.font.size=Pt(9.5)
for h in ('Heading 1','Heading 2','Heading 3'):
    doc.styles[h].font.color.rgb=RGBColor(0x1F,0x38,0x64); doc.styles[h].font.name='Calibri'
sec=doc.sections[0]; sec.top_margin=sec.bottom_margin=Cm(1.6); sec.left_margin=sec.right_margin=Cm(1.6)
def H(t,l=1): doc.add_heading(t,level=l)
def P(t='',bold=False,italic=False,size=None):
    p=doc.add_paragraph(); r=p.add_run(t); r.bold=bold; r.italic=italic
    if size: r.font.size=Pt(size)
def B(t): doc.add_paragraph(t,style='List Bullet')
def T(hdr,rows,widths=None,fs=8.5):
    t=doc.add_table(rows=1,cols=len(hdr)); t.style='Light Grid Accent 1'
    t.alignment=WD_TABLE_ALIGNMENT.LEFT
    for i,h in enumerate(hdr):
        c=t.rows[0].cells[i]; c.text=''
        r=c.paragraphs[0].add_run(h); r.bold=True; r.font.size=Pt(fs)
    for row in rows:
        cs=t.add_row().cells
        for i,v in enumerate(row):
            v = '' if v is None else str(v)
            lines = v.replace('\r','').split('\n')
            cs[i].text = ''
            para = cs[i].paragraphs[0]
            for j, ln in enumerate(lines):
                if j: para = cs[i].add_paragraph()
                rr_ = para.add_run(ln); rr_.font.size = Pt(fs)
    if widths:
        for rr_ in t.rows:
            for i,w in enumerate(widths): rr_.cells[i].width=Cm(w)
    doc.add_paragraph(); return t
def NOTE(t):
    p=doc.add_paragraph(); r=p.add_run('Note   '); r.bold=True; r.font.size=Pt(8.5)
    r2=p.add_run(t); r2.italic=True; r2.font.size=Pt(8.5)
json.dump({'ok':1},open('/dev/null','w')) if False else None

# ------------------------------------------------------------------ layouts
# tab -> (tech row, description row, type row, length row, M/O row, first data row)
VLAY={
 'Vendor creation for All CC': (1,2,None,None,3,4),
 'TDS upload':                 (1,2,None,None,None,3),
 'TAN details':                (1,None,None,None,None,2),
 'BANK Key creation':          (1,5,2,3,4,7),
 'Bank details update':        (1,5,2,3,4,7),
 'Vendor extension':           (1,5,2,3,4,7),
 'CIN details':                (1,None,None,None,None,2),
 'Patner function':            (6,9,7,8,None,10),
 'Block_Unblocked':            (4,7,5,6,None,9),
}
VSCEN=[('Vendor / BP creation - all company codes','Vendor creation for All CC'),
       ('Withholding tax / TDS','TDS upload'),
       ('TAN exemption details','TAN details'),
       ('Bank key creation','BANK Key creation'),
       ('Vendor bank details','Bank details update'),
       ('Vendor extension','Vendor extension'),
       ('CIN details','CIN details'),
       ('Partner functions','Patner function'),
       ('Block / unblock','Block_Unblocked')]
CSCEN=[('R1','Domestic customer - India','domestic customer IND',2,3),
       ('R2','Export customer','Export customer',1,4),
       ('R3','Morocco customer','Morocco customer ',1,4),
       ('R4','SAGA customer','SAGA customer',2,4),
       ('R5','Credit limit (FSCM)','credit Limit',1,4),
       ('R6','Domestic customer - US','domestic customer US',1,2),
       ('R7','Ship-to party - US','ship to party US',1,2)]
NODE={'K':'Key field','A':'Address','M':'Communication','C':'Customer general data',
      'B':'Company code data','S':'Sales area data','T':'Tax classification',
      'Z':'Licences and Their Validity','I':'BP identification','U':'Credit management'}
SRC={'C':'KNA1','B':'KNB1','S':'KNVV','Z':'ZSD_LICENSE_CHK','A':'ADRC'}
def fmt_of(node,fld):
    tab=SRC.get(node)
    if not tab: return ''
    for c in D.get(tab,[]):
        if c['f']==fld:
            dt=c['dt'] or ''
            ln=(DLEN.get(tab,{}).get(fld,{}) or {}).get('len','')
            nice={'CHAR':'Text','DATS':'Date','NUMC':'Digits','CUKY':'Currency',
                  'INT2':'Whole number','CLNT':'Client','DEC':'Number','CURR':'Amount'}.get(dt,dt)
            return f'{nice} {ln}'.strip()
    return ''

# ------------------------------------------------------------------ cover
doc.add_heading('Upload File Format Specification',level=0)
P('Column-by-column layout for every scenario of both mass upload programs',bold=True,size=12)
P('Companion to the User Acceptance Test Guide',italic=True)
doc.add_paragraph()
T(['Programs','Scenarios','Columns documented'],
  [['ZMMS_BP_MASS_UPLOAD  (supplier master)','9','238'],
   ['ZSDS_CUST_MASS_UPLOAD  (customer master)','7','607']],widths=[8.0,3.0,5.0],fs=9.5)

H('1.  How to read this document',1)
P('There is one section per radio button. Each section names the workbook tab it reads, states how that tab '
  'is laid out today, and lists every column.')
P('Every tab of both workbooks must be laid out the same way:',bold=True)
B('Row 1 - the heading row')
B('Row 2 onwards - data only')
B('Nothing in between: no field type row, no field length row, no mandatory/optional row, no guideline '
  'row and no sample rows')
P('Both programs read the chosen tab from row 2.',bold=True)
P('Twelve of the sixteen tabs do not follow this today. Each section below states exactly which rows to '
  'delete from that tab. Please delete them rather than hide them - a hidden row is still read.')
P('Delete rows only, never columns.',bold=True)
P('Delete ROWS only - never columns. Several tabs start with a label column that holds text like "Field Tech name" or "Sample data" and is empty on the data rows. That column still counts. On the supplier creation tab, for instance, the vendor number is column B, not column A. Deleting the label column would shift every field one place to the left and the file would load into the wrong fields.')
P('The column letters in the tables below are the letters the program expects. If a letter in the table '
  'does not match your file, a column has been added or removed and the file will not load correctly.')
NOTE('The supplier program does skip rows whose first cell reads Field Type, Field Length, Sample, Project '
     'or Tech name, so some tabs will load even before they are re-cut. It is not a substitute for a clean '
     'layout: any row that does not begin with one of those words is read as data. Patner function row 6 '
     'begins with LIFNR and Block_Unblocked row 8 begins with Default XK05 - both would be read as records.')

H('2.  Heading rows to skip',1)
P('Both programs have a field on the selection screen called Heading rows to skip, set to 1. With the '
  'layout above that is correct: one heading row, then data.')
P('On the first run, the top of the result list shows a line like:')
P('        Line 1 skipped:  KUNNR / BUKRS / VKORG / VTWEG / SPART ...',italic=True)
T(['What that line shows','What it means','What to do'],
  [['Your column headings','Correct','Leave the field at 1'],
   ['A real customer or supplier record','The heading row was removed before the program saw it',
    'Set the field to 0, otherwise every file loses its first record']],
  widths=[4.5,5.5,5.0],fs=9)
P('If a tab has not been re-cut yet, this field can also be used to step over the extra rows - for example '
  'Patner function currently needs 9.')

H('3.  Mandatory and optional',1)
P('Where the workbook itself marks a column Mandatory or Optional, that marking is reproduced here. Where it '
  'does not, the column is shown blank — SAP still applies its own required-field rules for the account group, '
  'so a field can be rejected at posting even if it is not marked mandatory in the spreadsheet.')
P('A blank cell never erases data.',bold=True)
P('If you leave a cell empty the program leaves whatever is already in SAP untouched. To clear a field on '
  'purpose, type #BLANK# in the cell.')
doc.add_page_break()

# ------------------------------------------------------------------ vendor
H('4.  Supplier master - ZMMS_BP_MASS_UPLOAD',1)
n=0
for radio,tab in VSCEN:
    n+=1
    H(f'4.{n}  {radio}',2)
    tech,desc,typ,lng,mo,first = VLAY[tab]
    rr=VEN[tab]
    P(f'Workbook tab:  {tab}',bold=True)
    extra=[]
    if desc and desc!=1: extra.append(f'field description (row {desc})')
    if typ:  extra.append(f'field type (row {typ})')
    if lng:  extra.append(f'field length (row {lng})')
    if mo:   extra.append(f'mandatory/optional (row {mo})')
    if tech!=1: extra.append(f'everything above the heading row (rows 1 to {tech-1})')
    ok = (tech==1 and first==2)
    todo = ('Nothing - this tab is already correct' if ok
            else ('Move the heading row to row 1 and delete ' + ', '.join(extra) + ', and the sample rows, '
                  f'so the first record sits on row 2 (it is on row {first} today)'))
    T(['Required','Detail'],
      [['Heading row','row 1'],
       ['First data row','row 2'],
       ['Headings are on','row %d today' % tech],
       ['First record is on','row %d today' % first],
       ['To do',todo]],widths=[4.0,12.0],fs=9)
    mx=max((max(d) if d else 0) for d in rr.values())
    rows=[]
    for i in range(1,mx+1):
        t_=rr.get(tech,{}).get(i,'')
        if not t_: continue
        rows.append([letters(i), t_,
                     rr.get(desc,{}).get(i,'') if desc else '',
                     rr.get(typ,{}).get(i,'') if typ else '',
                     rr.get(lng,{}).get(i,'') if lng else '',
                     rr.get(mo,{}).get(i,'') if mo else ''])
    T(['Col','Technical name','Description','Type','Len','M/O'],rows,
      widths=[1.0,3.2,7.4,1.6,1.0,1.0])
    doc.add_page_break()

# ------------------------------------------------------------------ customer
H('5.  Customer master - ZSDS_CUST_MASS_UPLOAD',1)
CUSROWS=rowsof('/home/user/ONGC-CST-Purchase-Date-Sharing-/customer master LSMW -  with format.xlsx')
n=0
for scen,radio,tab,namerow,first in CSCEN:
    n+=1
    H(f'5.{n}  {radio}',2)
    rr=CUSROWS[tab]
    P(f'Workbook tab:  {tab.strip()}',bold=True)
    ok=(namerow==1 and first==2)
    todo=('Nothing - this tab is already correct' if ok
          else (f'Move the heading row from row {namerow} to row 1 and delete every other row above the '
                f'first record, so the first record sits on row 2 (it is on row {first} today)'))
    T(['Required','Detail'],
      [['Heading row','row 1'],
       ['First data row','row 2'],
       ['Headings are on',f'row {namerow} today'],
       ['First record is on',f'row {first} today'],
       ['To do',todo]],widths=[4.0,12.0],fs=9)
    ent={int(c):(nd,fl,cv,(cm or '').strip()) for sc,c,nd,fl,cv,cm in MAP if sc==scen}
    mx=max(list(ent)+[max(d) if d else 0 for d in rr.values()])
    rows=[]
    for i in range(1,mx+1):
        head=rr.get(namerow,{}).get(i,'')
        if i in ent:
            nd,fl,cv,cm=ent[i]
            goes=NODE.get(nd,'')
            det=fl if nd not in ('T',) else (f'tax category {fl}' if not fl.startswith('#') else f'tax category no. {fl[1:]} for the country')
            f_=fmt_of(nd,fl) or ('Date' if cv=='DT' else ('Digits' if cv=='NM' else ''))
            rows.append([letters(i), head or cm, goes, det, f_])
        else:
            if head: rows.append([letters(i), head, 'not loaded', '', ''])
    T(['Col','Heading in the workbook','Goes to','Field','Format'],rows,
      widths=[1.0,5.4,3.4,3.2,2.2])
    doc.add_page_break()

doc.save('/home/user/ONGC-CST-Purchase-Date-Sharing-/docs/BP_Mass_Upload_File_Format_Spec.docx')
print('saved')
