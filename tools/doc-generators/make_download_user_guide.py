"""The user guide for ZSDS_CUST_TMPL_DOWNLOAD, for Cipla."""
from docx import Document
from docx.shared import Pt, Cm, RGBColor
from docx.enum.table import WD_TABLE_ALIGNMENT

doc = Document()
st = doc.styles['Normal']
st.font.name = 'Calibri'; st.font.size = Pt(10.5)
for s in ('Heading 1', 'Heading 2'):
    doc.styles[s].font.color.rgb = RGBColor(0x1F, 0x38, 0x64)
    doc.styles[s].font.name = 'Calibri'
sec = doc.sections[0]
sec.top_margin = sec.bottom_margin = Cm(2)
sec.left_margin = sec.right_margin = Cm(2.0)

def H(t, lvl=1): doc.add_heading(t, level=lvl)
def P(t='', bold=False, italic=False, size=None):
    p = doc.add_paragraph()
    r = p.add_run(t); r.bold = bold; r.italic = italic
    if size: r.font.size = Pt(size)
    return p
def B(t): doc.add_paragraph(t, style='List Bullet')
def N(t): doc.add_paragraph(t, style='List Number')
def C(t):
    p = doc.add_paragraph()
    r = p.add_run(t); r.font.name = 'Consolas'; r.font.size = Pt(9.5)
    return p

def T(headers, rows, widths):
    t = doc.add_table(rows=1, cols=len(headers))
    t.style = 'Light Grid Accent 1'
    t.alignment = WD_TABLE_ALIGNMENT.LEFT
    for i, h in enumerate(headers):
        c = t.rows[0].cells[i]; c.text = ''
        r = c.paragraphs[0].add_run(h); r.bold = True; r.font.size = Pt(9.5)
    for row in rows:
        cells = t.add_row().cells
        for i, v in enumerate(row):
            cells[i].text = ''
            para = cells[i].paragraphs[0]
            for j, ln in enumerate(str(v).split('\n')):
                if j: para = cells[i].add_paragraph()
                rr = para.add_run(ln); rr.font.size = Pt(9.5)
    for r_ in t.rows:
        for i, w in enumerate(widths):
            r_.cells[i].width = Cm(w)
    doc.add_paragraph()
    return t

doc.add_heading('Customer Template Download', level=0)
P('How to run ZSDS_CUST_TMPL_DOWNLOAD', bold=True, size=13)
doc.add_paragraph()

# ------------------------------------------------------------------ 1
H('1. What the program is for', 1)
P('It writes out a customer master template as an Excel workbook - either an '
  'empty template to fill in, or the template already filled with the data of '
  'the customers you name.')
P('The templates are the ones in the customer code template workbook: 24 layouts, '
  'reached through 76 combinations of country and customer account group, across '
  'twelve countries. The program picks the right layout from the country and the '
  'account group you choose, so the file that comes out is laid out exactly like '
  'the template for that combination.')
P('The program only reads. It does not create, change or block anything.', bold=True)

# ------------------------------------------------------------------ 2
H('2. Starting it', 1)
P('Run it from SE38 or SA38:')
C('    ZSDS_CUST_TMPL_DOWNLOAD')
P('A transaction code can be created for it if you would rather not go through '
  'SE38 - tell us and we will add one.', italic=True)

# ------------------------------------------------------------------ 3
H('3. The selection screen', 1)

H('3.1  Template', 2)
T(['Choose', 'When'], [
    ['Customer create template',
     'The normal case - a template for creating customers. The layout follows the '
     'country and the account group you choose in the next block.'],
    ['Customer extension template',
     'For extending a customer that already exists to a further company code or '
     'sales area. One layout, the same for every country, so the country and the '
     'account group are not used and grey out.'],
    ['Block / unblock template',
     'The XD05 layout, for blocking and unblocking. Also the same for every '
     'country, so those two fields grey out as well.'],
], [5.2, 11.3])

H('3.2  Country and account group', 2)
P('These two together choose the template. You can type them, but the easier way '
  'is to press F4 on either field: you get the full list of templates.')
T(['Template', 'Country', 'Name', 'Account group', 'Text', 'Columns'], [
    ['AU/ZCDP', 'AU', 'Australia', 'ZCDP', '...', '140'],
    ['AU/ZDOM', 'AU', 'Australia', 'ZDOM', '...', '83'],
    ['AU/ZEXP', 'AU', 'Australia', 'ZEXP', '...', '79'],
    ['AU/ZPLN', 'AU', 'Australia', 'ZPLN', '...', '139'],
    ['AU/ZSHP', 'AU', 'Australia', 'ZSHP', '...', '73'],
], [2.3, 1.8, 2.6, 3.0, 3.2, 1.9])
P('Pick a line and both fields are filled, so the two can never disagree. The '
  '"Columns" figure is how wide that template is, which is a quick way to see '
  'that you have the one you meant.')
P('If a country is already in the field the list shows only that country. If the '
  'field is empty the list shows all 76 combinations, which is what you want when '
  'you are not sure which ones exist.')
P('A combination the workbook does not cover is refused when you run the program, '
  'with a message naming what is wrong - whether the country has no template at '
  'all, or has one but not for that account group.')

H('3.3  Which customers', 2)
T(['Field', 'What to put in it'], [
    ['Business partner', 'One or more business partner numbers. Leave empty if you '
     'are giving customer numbers instead.'],
    ['Customer', 'One or more customer numbers. Either field will do - give '
     'whichever number you have and the program finds the other.'],
    ['Rows at most', 'A safety limit, 100 by default. It counts rows in the file, '
     'not customers: a customer with four company codes and three sales areas '
     'produces twelve rows. Raise it if the file comes out short - the log says so '
     'when the limit is what stopped it.'],
], [4.0, 12.5])
P('Leave both number fields empty and tick "Template only" to get an empty '
  'template.')

H('3.4  Output', 2)
T(['Field', 'What it does'], [
    ['Save the workbook as', 'The file name. It is proposed for you and follows '
     'the template - C:\\temp\\AU_ZDOM.xlsx, C:\\temp\\CUST_EXTN.xlsx - and '
     'changes when you change the template. Press F4 for the save dialogue, or '
     'type over it.'],
    ['Write to the PC', 'Writes to your own machine. The usual choice.'],
    ['Write to the appl. server',
     'Writes to a directory on the application server instead. Use this for a '
     'background job, where there is no PC to write to.'],
    ['Template only (no data)',
     'Ticked, the file carries the heading row and nothing else. Unticked, it '
     'carries the data of the customers you named.'],
], [4.6, 11.9])

# ------------------------------------------------------------------ 4
H('4. What comes out', 1)
P('An .xlsx workbook with one sheet, named after the template.')
B('Row 1 is the heading row, exactly the headings the template uses.')
B('Data starts at row 2, one row per company code and sales area the customer '
  'has - the same way the templates are keyed.')
B('A customer with no company code or no sales area still gets a row, with those '
  'columns empty.')
P('Dates are written as dd.mm.yyyy and numbers without leading zeros, which is '
  'how the template expects them.')
P('Columns that a created customer cannot have - the "reference" columns that '
  'exist for copying from a sample customer - come out empty. There is nothing '
  'to read for them, because the customer already exists.')

# ------------------------------------------------------------------ 5
H('5. The log', 1)
P('After the file is written the program shows a list with one line per row '
  'written, and a line for anything that needs your attention:')
T(['Light', 'Means'], [
    ['Green', 'A row was written. The line names the customer, the company code '
     'and the sales area it came from.'],
    ['Yellow', 'Something worth knowing - most often that the "Rows at most" '
     'limit stopped the run before every row was written.'],
    ['Red', 'A customer could not be read - it does not exist, or the number '
     'given does not lead to one. The rest of the file is still written.'],
], [2.2, 14.3])

# ------------------------------------------------------------------ 6
H('6. A worked example', 1)
P('To get the Australian domestic customer template filled with the data of two '
  'customers:')
N('Start ZSDS_CUST_TMPL_DOWNLOAD.')
N('Leave "Customer create template" selected.')
N('Press F4 on Country, pick the line AU/ZDOM. Both fields fill.')
N('Put the two customer numbers into "Customer".')
N('Check the file name - it now reads C:\\temp\\AU_ZDOM.xlsx.')
N('Leave "Template only" unticked, and run.')
P('And to get the same template empty, to fill in by hand: tick "Template only" '
  'and leave the customer numbers out.')

# ------------------------------------------------------------------ 7
H('7. If something is not right', 1)
T(['What you see', 'What it means'], [
    ['"No customer template exists for country XX"',
     'That country has no template in the workbook. Press F4 to see the twelve '
     'that do.'],
    ['"No customer template exists for country XX with account group YYYY"',
     'The country has templates, but not for that account group. F4 on the '
     'account group shows the ones it does have.'],
    ['"Choose a country first"',
     'The account group list needs a country to narrow to. Pick the country '
     'first, or press F4 on the country field to see everything.'],
    ['"Give a business partner or a customer - or tick Template only"',
     'The program will not write an empty file by accident. Either name some '
     'customers, or say that you want the template only.'],
    ['The file is shorter than you expected',
     'The "Rows at most" limit. The log says so in yellow. Raise the limit and '
     'run again.'],
], [6.2, 10.3])

doc.add_paragraph()
P('Prepared by Diligent Consulting.', italic=True, size=9)

import os
out = os.path.join(os.path.dirname(__file__), '..', '..', 'docs', 'cipla',
                   'Customer_Template_Download_User_Guide.docx')
doc.save(os.path.abspath(out))
print('written', os.path.abspath(out))
