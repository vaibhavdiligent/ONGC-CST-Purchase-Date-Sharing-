from docx import Document
from docx.shared import Pt, Cm, RGBColor
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.enum.table import WD_TABLE_ALIGNMENT
from docx.oxml.ns import qn
from docx.oxml import OxmlElement

doc = Document()
st = doc.styles['Normal']
st.font.name = 'Calibri'; st.font.size = Pt(10.5)
for s in ('Heading 1','Heading 2','Heading 3'):
    doc.styles[s].font.color.rgb = RGBColor(0x1F,0x38,0x64)
    doc.styles[s].font.name = 'Calibri'
sec = doc.sections[0]
sec.top_margin = sec.bottom_margin = Cm(2)
sec.left_margin = sec.right_margin = Cm(2.2)

def H(t, lvl=1): doc.add_heading(t, level=lvl)
def P(t='', bold=False, italic=False, size=None):
    p = doc.add_paragraph()
    r = p.add_run(t); r.bold = bold; r.italic = italic
    if size: r.font.size = Pt(size)
    return p
def B(t):
    doc.add_paragraph(t, style='List Bullet')
def N(t):
    doc.add_paragraph(t, style='List Number')
def T(headers, rows, widths=None):
    t = doc.add_table(rows=1, cols=len(headers))
    t.style = 'Light Grid Accent 1'
    t.alignment = WD_TABLE_ALIGNMENT.LEFT
    for i,h in enumerate(headers):
        c = t.rows[0].cells[i]; c.text = ''
        r = c.paragraphs[0].add_run(h); r.bold = True; r.font.size = Pt(9.5)
    for row in rows:
        cells = t.add_row().cells
        for i,v in enumerate(row):
            v = '' if v is None else str(v)
            lines = v.replace('\r','').split('\n')
            cells[i].text = ''
            para = cells[i].paragraphs[0]
            for j, ln in enumerate(lines):
                if j: para = cells[i].add_paragraph()
                rr_ = para.add_run(ln); rr_.font.size = Pt(9.5)
    if widths:
        for r_ in t.rows:
            for i,w in enumerate(widths):
                r_.cells[i].width = Cm(w)
    doc.add_paragraph()
    return t
def NOTE(t):
    p = doc.add_paragraph()
    r = p.add_run('Note   '); r.bold = True; r.font.size = Pt(9.5)
    r2 = p.add_run(t); r2.font.size = Pt(9.5); r2.italic = True

# ------------------------------------------------------------------ cover
h = doc.add_heading('Business Partner Mass Upload Programs', level=0)
P('User Acceptance Test Guide', bold=True, size=13)
P('Supplier master and Customer master mass upload for SAP S/4HANA', italic=True)
doc.add_paragraph()
T(['Item','Detail'],
  [['Programs','ZMMS_BP_MASS_UPLOAD  (supplier / vendor master)\nZSDS_CUST_MASS_UPLOAD  (customer master)'],
   ['System','CRS, client 500'],
   ['Release','SAP S/4HANA, S4CORE 109 / SAP_BASIS 816'],
   ['Source templates','Vendor LSMW with Template.xlsx\ncustomer master LSMW -  with format.xlsx'],
   ['Document purpose','Instructions for the business team to test both programs before go-live'],
   ['Prepared for','Testing team'],
  ], widths=[4.0,12.0])

doc.add_page_break()

# ------------------------------------------------------------------ 1
H('1.  Why these programs exist', 1)
P('Your existing upload templates were built as LSMW recordings of transactions XK01, XK02, XK05 and XD01. '
  'Under the SAP S/4HANA Business Partner approach those transactions are either redirected to transaction BP '
  'or removed altogether, so the recordings can no longer run.')
P('These two programs keep your existing spreadsheet layouts exactly as they are, and replace only the engine '
  'underneath. Nothing you already do in Excel has to change.')
P('Both programs create and change data through the standard SAP Business Partner interface, so every check, '
  'validation and number range that applies when a user works in transaction BP also applies here.')

# ------------------------------------------------------------------ 2
H('2.  Before you start testing', 1)
T(['You need','Why'],
  [['A test client with representative master data','So the checks against account groups, company codes and sales areas behave as they will in production'],
   ['Your normal BP creation authorisations','The programs run under your own user and enforce the same authorisation checks as transaction BP'],
   ['The two Excel workbooks','Use the same files you use today; no re-formatting is needed'],
   ['A few known-good customers / suppliers','For the change scenarios, so you can confirm existing data is not disturbed'],
  ], widths=[6.0,10.0])

H('2.1  The file layout - this applies to every tab of both workbooks', 2)
P('Every tab must be laid out the same way:', bold=True)
B('Row 1 - the heading row.')
B('Row 2 onwards - data only.')
B('Nothing in between. No field type row, no field length row, no mandatory/optional row, '
  'no guideline row and no sample rows.')
P('Both programs read the chosen tab from row 2.', bold=True)
P('Most tabs in the two workbooks do not follow this today - they carry type, length, guideline or sample '
  'rows between the headings and the first real record. Of the sixteen tabs, four already comply and twelve '
  'need re-cutting. The companion Upload File Format Specification states, tab by tab, exactly which rows '
  'to delete.')
NOTE('Please delete those rows rather than hide them. A hidden row is still read.')
P('Delete rows only, never columns.',bold=True)
P('Delete ROWS only - never columns. Several tabs start with a label column that holds text like "Field Tech name" or "Sample data" and is empty on the data rows. That column still counts. On the supplier creation tab, for instance, the vendor number is column B, not column A. Deleting the label column would shift every field one place to the left and the file would load into the wrong fields.')

# ------------------------------------------------------------------ 3
H('3.  The most important instruction: always test run first', 1)
P('Both programs have a Test run checkbox, and it is ticked by default. Leave it ticked for your first pass.')
P('In a test run the program does everything it would normally do, including every validation, but nothing '
  'is saved. You get the full result list showing exactly what would have happened, row by row.')
P('Only untick Test run once a file comes back with no errors.', bold=True)

H('3.1  One check to do on your very first run', 2)
P('Both programs have a field called Heading rows to skip, set to 1. That matches the layout above: '
  'one heading row, then data.')
P('On your first run, look at the top of the result list. There will be a line like this:')
P('        Line 1 skipped:  KUNNR / BUKRS / VKORG / VTWEG / SPART ...', italic=True)
P('That line shows what the program treated as the heading row.')
T(['What that line shows','What it means','What to do'],
  [['Your column headings','Correct','Nothing - leave the field at 1'],
   ['A real customer or supplier record','The heading row was already removed before the program saw it',
    'Set Heading rows to skip to 0 and run again, otherwise every file loses its first record'],
  ], widths=[4.5,6.0,5.5])
P('This only has to be established once. Please tell us which of the two you see and we will set it '
  'permanently so nobody has to remember it.')

doc.add_page_break()

# ------------------------------------------------------------------ 4
H('4.  Program A — Supplier master upload', 1)
P('Program name: ZMMS_BP_MASS_UPLOAD', bold=True)
H('4.1  Selection screen', 2)
T(['Field','What to enter'],
  [['Scenario','Pick one of the nine radio buttons. This decides which tab of the workbook is read.'],
   ['Upload workbook','The path to Vendor LSMW with Template.xlsx'],
   ['File is on the PC / application server','Where the file sits. Use PC for normal testing.'],
   ['Test run (nothing is posted)','Leave ticked for the first pass'],
   ['Stop at the first faulty row','Tick this if you want the run to halt as soon as something fails, instead of working through the whole file'],
   ['Heading rows to skip','Leave at 1. See section 3.1.'],
  ], widths=[5.5,10.5])

H('4.2  The nine scenarios', 2)
T(['Radio button','Workbook tab','What it does'],
  [['Vendor / BP creation - all company codes','Vendor creation for All CC','Creates the supplier and its Business Partner, with company code and purchasing data'],
   ['Withholding tax / TDS','TDS upload','Maintains withholding tax types and codes per company code'],
   ['TAN exemption details','TAN details','Maintains the India TAN exemption records'],
   ['Bank key creation','BANK Key creation','Creates or changes bank master records'],
   ['Vendor bank details','Bank details update','Maintains the supplier’s own bank accounts'],
   ['Vendor extension','Vendor extension','Extends an existing supplier to a further company code or purchasing organisation'],
   ['CIN details','CIN details','Maintains the India tax and excise fields'],
   ['Partner functions','Patner function','Maintains purchasing partner functions'],
   ['Block / unblock','Block_Unblocked','Sets or clears posting and purchasing blocks'],
  ], widths=[4.8,4.2,7.0])
NOTE('The tab name "Patner function" is spelled as it appears in your workbook. The program looks for that exact name, so please do not correct the spelling in the file.')

doc.add_page_break()

# ------------------------------------------------------------------ 5
H('5.  Program B — Customer master upload', 1)
P('Program name: ZSDS_CUST_MASS_UPLOAD', bold=True)
H('5.1  Selection screen', 2)
T(['Field','What to enter'],
  [['Scenario','Pick one of the seven radio buttons'],
   ['Upload workbook','The path to customer master LSMW -  with format.xlsx'],
   ['File is on the PC / application server','Where the file sits'],
   ['Test run (nothing is posted)','Leave ticked for the first pass'],
   ['Stop at the first faulty row','Halt on the first failure instead of processing the whole file'],
   ['BP grouping (blank = derived)','Leave this blank. SAP then derives the BP grouping from the account group, which is the normal behaviour. Only fill it if you need to force a particular grouping.'],
   ['Heading rows to skip','Leave at 1. See section 3.1.'],
  ], widths=[5.5,10.5])

H('5.2  The seven scenarios', 2)
T(['Radio button','Workbook tab','Columns','What it does'],
  [['Domestic customer - India','domestic customer IND','136','Creates Indian domestic customers with company code, sales area, tax and licence data'],
   ['Export customer','Export customer','67','Creates export customers'],
   ['Morocco customer','Morocco customer','116','Creates Morocco customers'],
   ['SAGA customer','SAGA customer','122','Creates SAGA customers'],
   ['Credit limit (FSCM)','credit Limit','18','Sets credit limits, risk class and credit blocks'],
   ['Domestic customer - US','domestic customer US','75','Creates US domestic customers'],
   ['Ship-to party - US','ship to party US','75','Creates US ship-to parties'],
  ], widths=[4.2,4.0,1.8,6.0])

doc.add_page_break()

# ------------------------------------------------------------------ 6
H('6.  How to run a test', 1)
N('Start the program in transaction SE38 or through the transaction code once it is assigned.')
N('Choose the scenario that matches the tab you want to load.')
N('Select the workbook.')
N('Leave Test run ticked.')
N('Execute.')
N('The program reads the tab from row 2 and processes every row.')
N('A result list appears. Work through any errors, correct the spreadsheet, and run again.')
N('When the test run is clean, untick Test run and execute again to post for real.')

H('6.1  Reading the result list', 2)
P('Every row of your spreadsheet produces at least one line in the result list.')
T(['Column','Meaning'],
  [['Status','Green = fine, yellow = warning, red = error'],
   ['Excel row','The row number in your spreadsheet, so you can go straight to it'],
   ['Customer / Vendor','The account the message relates to'],
   ['Structure and Field','Which field caused the problem. This is the most useful column when something fails — it points at the spreadsheet column rather than giving a general message.'],
   ['Message','The SAP message text'],
  ], widths=[4.0,12.0])
P('A summary line at the bottom of the screen tells you how many rows were read, how many were processed '
  'and how many had errors.')

# ------------------------------------------------------------------ 7
H('7.  What to check after a live posting', 1)
T(['What you loaded','Where to check it'],
  [['Supplier or customer, general data','Transaction BP — General Data'],
   ['Company code data','Transaction BP — role FLCU00 (customer) or FLVN00 (supplier)'],
   ['Sales area data','Transaction BP — role FLCU01, Sales and Distribution'],
   ['Purchasing data','Transaction BP — role FLVN01'],
   ['Address, telephone, fax, e-mail','Transaction BP — Address tab. Mobile numbers appear flagged as mobile.'],
   ['Tax classifications','Transaction BP — Sales and Distribution, Billing, Tax classification'],
   ['Licences, bank guarantee, routing','Transaction BP — role FLCU01, tab "Licenses and Their Validity"'],
   ['Credit limits and risk class','Transaction UKM_BP, or BP in role UKM000'],
   ['Bank master records','Transaction FI03'],
  ], widths=[6.0,10.0])

doc.add_page_break()

# ------------------------------------------------------------------ 8
H('8.  Suggested test cases', 1)
P('We suggest working through these in order. Each one is quick and isolates a different part of the program.')
T(['#','Test','How','What should happen'],
  [['1','Empty file','Run any scenario against a tab with only the heading row','A message saying the tab holds no data from row 2 onwards. Nothing is posted.'],
   ['2','Wrong tab','Point the program at the customer workbook while a supplier scenario is selected','A clear message that the tab was not found'],
   ['3','One good row, test run','One valid new customer or supplier','Green line, "Test run OK". Check in BP that nothing was actually created.'],
   ['4','One good row, live','Same row with Test run unticked','Green line, record created. Verify it in BP.'],
   ['5','Deliberate error','Put an account group that does not exist in the account group column','Red line naming the field and the value. Nothing posted for that row.'],
   ['6','Bad date','Type 31.02.2026 in a date column','Red line saying the value is not a valid date'],
   ['7','Stop at first error','Two bad rows, tick "Stop at the first faulty row"','The run halts after the first failure and says so'],
   ['8','Change an existing record','Fill only the key columns and one field you want to change','Only that field changes. Everything else stays as it was — see section 9.'],
   ['9','Mixed file','Ten rows, three of them faulty','Seven green, three red. The seven are posted; the three are not.'],
   ['10','Licence data','A customer row with drug licence and bank guarantee columns filled','Values appear on the "Licenses and Their Validity" tab in BP'],
   ['11','Credit limit','A row on the credit Limit tab','Limit visible in UKM_BP for the matching credit segment'],
  ], widths=[0.9,3.4,5.2,6.5])

doc.add_page_break()

# ------------------------------------------------------------------ 9
H('9.  Behaviour you should expect', 1)
H('9.1  A blank cell does not erase data', 2)
P('If you leave a cell empty, the program leaves whatever is already in SAP untouched. This means you can '
  'load a file that only fills the columns you actually want to change.')
P('If you genuinely want to clear a field, type the word #BLANK# in the cell.', bold=True)

H('9.2  One licence record per customer', 2)
P('The licence and bank guarantee data is stored against the customer as a whole. The Plant field on that '
  'screen is informational and is not part of the key, which means a customer can hold exactly one set of '
  'licence details. If a spreadsheet carries the licence block more than once, only the first set can be loaded.')

H('9.3  Credit limit currency is checked, not written', 2)
P('In SAP S/4HANA a credit limit is held in the currency of its credit segment, which is set in configuration. '
  'The currency column on the credit Limit tab is therefore validated against the segment and the row is '
  'rejected if the two disagree — but the currency itself is not written.')
P('In your system the credit segment is the same code as the credit control area. For example credit control '
  'area 1000 uses segment 1000 in INR, 7450 uses segment 7450 in EUR, and 6600 uses segment 6600 in MAD.')

H('9.4  Two field limits in the existing licence table', 2)
T(['Field','Limit','Effect'],
  [['Bank Guarantee Amount','Whole numbers only, maximum 10 digits','A guarantee of 2,500,000.50 is stored as 2,500,000. If decimals are needed the field itself has to be changed.'],
   ['Distance in kms','Maximum 32,767','A larger value cannot be stored and the row is rejected'],
  ], widths=[4.0,4.0,8.0])

H('9.5  Tax classifications depend on the country', 2)
P('The tax classification columns are filled according to the tax categories configured for the country in question:')
T(['Country','Tax categories configured'],
  [['India','JTX1, JTX2, JTX3, JTX4, JOCG, JTC1'],
   ['United States','UTXJ, UTX2, UTX3, MWST'],
   ['Spain','MWST only'],
   ['Morocco','ZMVT only'],
  ], widths=[4.0,12.0])
P('This is why some tax classification columns on the Morocco and SAGA tabs cannot be loaded — see section 10.')

doc.add_page_break()

# ------------------------------------------------------------------ 10
H('10.  Corrections we applied to the templates', 1)
P('While mapping the spreadsheets we found several columns that could not be loaded as labelled. '
  'We corrected them rather than let them fail silently. Please review and confirm.')
T(['Tab','Column','What we found','What the program does'],
  [['domestic customer IND','BY','The heading reads JOIG, which is Integrated GST, but the description says Central GST. Your configuration has JOCG, not JOIG.','Loads the column as JOCG'],
   ['Morocco customer','61–64','TAXKD_02 to TAXKD_05, but Morocco has only one tax category (ZMVT)','The four columns are not loaded'],
   ['SAGA customer','72–75','TAXKD_02 to TAXKD_05, but Spain has only one tax category (MWST)','The four columns are not loaded'],
   ['Morocco customer','112–116','Repeat description text for columns 107–111 rather than real fields','Not loaded'],
   ['SAGA customer','36','No heading at all. Both your own LSMW recording and the India tab place the vendor number here.','Loaded as the vendor number'],
   ['SAGA customer','39–41','The three Spanish DIR3 codes required for FACe e-invoicing, but two of them pointed at the same field','Accounting Office to Tax Number 3, Managing Office to Tax Number 4, Processing Unit to Tax Number 5'],
   ['SAGA customer','44','Repeats Tax Number 3, but is annotated with a reconciliation account','Loaded as the reconciliation account'],
   ['credit Limit','14','Credit representative group. This is not configured in your system — the field is empty for every credit control area.','Not loaded'],
   ['credit Limit','R','Holds two values in one column: the interest indicator and the interest cycle','Please split this into two columns'],
  ], widths=[3.0,1.6,6.4,5.0])

# ------------------------------------------------------------------ 11
H('11.  Points we still need from you', 1)
T(['#','Point','What we need'],
  [['1','Aadhaar number','SAP has no standard field for Aadhaar. We propose the identification type X90003, matching the X90001 and X90002 types you already use. Please confirm it has been created.'],
   ['2','SAGA tab, Spanish DIR3 codes','Please confirm the allocation in section 10. Also note that columns CG and CH are annotated "VAT number" and "Accounting Office" but are in fact the 20B and 21B drug licence fields — loading Spanish tax data there would overwrite licence values that your invoice and credit note programs read.'],
   ['3','SAGA customer and domestic customer IND tabs','Please re-issue with headings on row 1 and data from row 2.'],
   ['4','Sample records','Two existing customers that already carry licence data and a bank guarantee, one Indian and one Morocco or SAGA, so we can verify the mapping against real values.'],
  ], widths=[0.9,3.6,11.5])

# ------------------------------------------------------------------ 12
H('12.  If something goes wrong', 1)
P('Please send us the following, and we can usually identify the cause without needing access to your session:')
B('The spreadsheet you used, or at least the failing rows')
B('Which scenario radio button was selected')
B('Whether Test run was ticked')
B('A screenshot of the result list, including the Structure and Field columns')
B('The customer or supplier number, if one was created')
doc.add_paragraph()
NOTE('If a row fails, nothing from that row is saved. The remaining rows are unaffected, so it is always safe to correct the file and run it again.')

doc.save('docs/BP_Mass_Upload_Test_Guide.docx')
print('written')
