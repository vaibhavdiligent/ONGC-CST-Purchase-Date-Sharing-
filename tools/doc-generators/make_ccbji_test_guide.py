"""Test guide for the CCBJI dynamic business partner programs.

    /usr/bin/python3 tools/doc-generators/make_ccbji_test_guide.py

writes docs/ccej/CCBJI_BP_Master_Download_Upload_Test_Guide.docx
"""
import os
from docx import Document
from docx.shared import Pt, Cm, RGBColor
from docx.enum.table import WD_TABLE_ALIGNMENT
from docx.oxml.ns import qn
from docx.oxml import OxmlElement

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
OUT = os.path.join(ROOT, 'docs', 'ccej', 'CCBJI_BP_Master_Download_Upload_Test_Guide.docx')

doc = Document()
st = doc.styles['Normal']
st.font.name = 'Calibri'
st.font.size = Pt(10.5)
for s in ('Heading 1', 'Heading 2', 'Heading 3'):
    doc.styles[s].font.color.rgb = RGBColor(0x1F, 0x38, 0x64)
    doc.styles[s].font.name = 'Calibri'
sec = doc.sections[0]
sec.top_margin = sec.bottom_margin = Cm(2)
sec.left_margin = sec.right_margin = Cm(2.0)
sec.page_width, sec.page_height = Cm(21.0), Cm(29.7)


def H(t, lvl=1):
    doc.add_heading(t, level=lvl)


def P(t='', bold=False, italic=False, size=None):
    p = doc.add_paragraph()
    r = p.add_run(t)
    r.bold, r.italic = bold, italic
    if size:
        r.font.size = Pt(size)
    return p


def B(t):
    doc.add_paragraph(t, style='List Bullet')


def shade(cell, hex_fill):
    tcpr = cell._tc.get_or_add_tcPr()
    shd = OxmlElement('w:shd')
    shd.set(qn('w:val'), 'clear')
    shd.set(qn('w:color'), 'auto')
    shd.set(qn('w:fill'), hex_fill)
    tcpr.append(shd)


def T(headers, rows, widths=None, size=9.5):
    t = doc.add_table(rows=1, cols=len(headers))
    t.style = 'Table Grid'
    t.alignment = WD_TABLE_ALIGNMENT.LEFT
    for i, h in enumerate(headers):
        c = t.rows[0].cells[i]
        c.text = ''
        r = c.paragraphs[0].add_run(h)
        r.bold = True
        r.font.size = Pt(size)
        r.font.color.rgb = RGBColor(0xFF, 0xFF, 0xFF)
        shade(c, '1F3864')
    for row in rows:
        cells = t.add_row().cells
        for i, v in enumerate(row):
            v = '' if v is None else str(v)
            cells[i].text = ''
            para = cells[i].paragraphs[0]
            for j, ln in enumerate(v.split('\n')):
                if j:
                    para = cells[i].add_paragraph()
                bold = ln.startswith('**') and ln.endswith('**')
                rr = para.add_run(ln.strip('*') if bold else ln)
                rr.font.size = Pt(size)
                rr.bold = bold
    if widths:
        # Word reads the cell widths, LibreOffice the grid: both are set, and
        # autofit is off so neither is recalculated.
        t.autofit = False
        grid = t._tbl.tblGrid
        for i, gc in enumerate(grid.findall(qn('w:gridCol'))):
            gc.set(qn('w:w'), str(int(widths[i] * 567)))
        for r_ in t.rows:
            for i, w in enumerate(widths):
                r_.cells[i].width = Cm(w)
    # A heading row repeats on every page the table runs onto.
    trpr = t.rows[0]._tr.get_or_add_trPr()
    th = OxmlElement('w:tblHeader')
    th.set(qn('w:val'), 'true')
    trpr.append(th)
    doc.add_paragraph()
    return t


def NOTE(t, label='Note'):
    p = doc.add_paragraph()
    r = p.add_run(label + '   ')
    r.bold = True
    r.font.size = Pt(9.5)
    r2 = p.add_run(t)
    r2.font.size = Pt(9.5)
    r2.italic = True


def CASE(cid, title, purpose, prereq, steps, expected):
    """One test case: steps and expected results side by side, with a result line."""
    H(f'{cid}  {title}', 3)
    P(purpose, italic=True)
    if prereq:
        p = doc.add_paragraph()
        r = p.add_run('Before you start:  ')
        r.bold = True
        p.add_run(prereq)
    T(['#', 'What to do', 'What you should see'],
      [[str(i + 1), s, e] for i, (s, e) in enumerate(zip(steps, expected))],
      widths=[0.8, 8.6, 7.6])
    T(['Result', 'Tested by / date', 'Remarks / defect no.'],
      [['☐ Pass    ☐ Fail', '', '']], widths=[4.0, 5.0, 8.0])


# =================================================================== cover
doc.add_heading('Customer and Supplier Master', level=0)
P('Dynamic Download / Upload - Test Guide', bold=True, size=14)
P('Coca-Cola Bottlers Japan (CCBJI) - SAP S/4HANA', italic=True)
doc.add_paragraph()
T(['Item', 'Detail'],
  [['Customer program', '/CCBJI/RUSDMDR_CUST_BP_MAINT     transaction /CCBJI/RUSDMD_CUSBP'],
   ['Supplier program', '/CCBJI/RUMMMDR_VEND_BP_MAINT     transaction /CCBJI/RUMMMD_VENBP'],
   ['System', 'Q4R (quality) - SAP S/4HANA 2025'],
   ['Replaces', 'The customer and supplier LSMW objects - one program for every field, '
                'instead of one LSMW per layout'],
   ['Purpose of this document', 'Step-by-step instructions for the test team: what to set up, '
                                'what to do, and what the system must show'],
   ['How to use it', 'Work through the test cases in order. Tick Pass or Fail under each one '
                     'and write the defect number in Remarks when something is wrong.']],
  widths=[4.5, 12.5])
doc.add_page_break()

# =================================================================== 1
H('1.  What the programs do', 1)
P('There are two programs that work in the same way: one for customers and one for suppliers '
  '(vendors). Each can do four things:')
T(['', 'Download (to a file)', 'Upload (from a file)'],
  [['Change', 'Writes the fields you choose for the customers or suppliers you name - '
              'what SAP holds today. You correct the file.',
              'Compares each row with SAP and changes only the fields that differ.'],
   ['Create', 'Writes an empty template, or a sample copied from existing records with the '
              'number left blank.',
              'Creates new business partners with the customer or supplier role from the rows.']],
  widths=[2.5, 7.3, 7.2])
P('Everything is written through the standard SAP Business Partner interface - the same checks '
  'that apply in transaction BP apply here. No table is updated directly. Every value is checked '
  'before anything is saved, and a test run shows what would change without saving anything.')

H('1.1  The file in one picture', 2)
P('Row 1 holds the field names. The data starts in row 2. One row is one record (or one entry '
  'of a repeating part, see 1.3).')
T(['A', 'B', 'C', 'D', 'E'],
  [['KNA1-KUNNR', 'KNVV-VKORG', 'KNVV-VTWEG', 'KNVV-SPART', 'KNVV-KDGRP'],
   ['1000123', '0801', 'Z1', 'Z0', '01'],
   ['1000124', '0801', 'Z1', 'Z0', '02']],
  widths=[3.4, 3.4, 3.4, 3.4, 3.4])
B('Field names are written TABLE-FIELD, for example KNA1-KUNNR, KNB1-ZTERM, KNVV-KDGRP, '
  'ADRC-NAME1, ADR2-TEL_NUMBER. A field name on its own (KDGRP) is also accepted when only one '
  'table has that field; otherwise the program asks you to add the table.')
B('The usual LSMW names are accepted too, for example KNA1-NAME1, KNA1-ORT01, KNA1-TELF1.')
B('Texts are columns named TEXT-<object>-<text ID>, for example TEXT-KNA1-0001.')
B('The number column is KNA1-KUNNR (customer) or LFA1-LIFNR (supplier). On a change it is '
  'required; on a creation it is left empty - SAP gives the number.')

H('1.2  What a cell means on a change upload', 2)
T(['The cell holds', 'What the program does'],
  [['A value', 'Compares it with SAP. If it differs, the field is changed. If it is the same, '
               'nothing happens.'],
   ['#', 'Leaves the field exactly as it is in SAP - use this when you do not want to touch a field.'],
   ['Nothing (empty)', 'Clears the field in SAP. Before anything is posted the program asks once: '
                       '"n field(s) are empty in the file but filled in SAP ... Clear them?". '
                       'Choose Cancel and nothing at all is posted.']],
  widths=[4.0, 13.0])

H('1.3  Repeating parts and the ACTION column', 2)
P('Some parts of a master record have several entries: partner functions, tax classifications, '
  'bank details, unloading points, contact persons, dunning data and withholding tax. For these the '
  'file has one row per entry, and the download adds a column ACTION after the key columns:')
T(['ACTION', 'Meaning'],
  [['I', 'Insert - add this entry. Error if it exists already.'],
   ['U', 'Update - change this entry. Error if it does not exist.'],
   ['D', 'Delete - remove this entry. Error if it does not exist.'],
   ['(empty)', 'Update if the entry exists, insert if it does not.']],
  widths=[3.0, 14.0])
NOTE('A file can hold only one repeating part. Partner functions and bank details, for example, '
     'go into two separate files. The program says so when the choice of fields mixes them.')

H('1.4  How values are written', 2)
T(['Kind of field', 'Write it as', 'Example'],
  [['Date', 'YYYY/MM/DD', '2026/11/30'],
   ['Time', 'HH:MM', '09:30'],
   ['Number', 'digits, decimal point', '1500.50'],
   ['Yes/no flag', 'X, or Yes / True; empty = no', 'X'],
   ['Code (payment terms, customer group ...)', 'the SAP code, not its text', 'Z001'],
   ['Japanese text', 'as it is, in Excel or in a UTF-8 or Shift-JIS text file', 'コカ・コーラ']],
  widths=[5.5, 6.5, 5.0])
doc.add_page_break()

# =================================================================== 2
H('2.  Before testing', 1)
H('2.1  Checklist', 2)
T(['#', 'Check', 'Done'],
  [['1', 'Both programs are active in Q4R (SE38 shows "Active") and the selection texts and text '
         'symbols are maintained - the screen shows texts, not field names.', '☐'],
   ['2', 'The transactions /CCBJI/RUSDMD_CUSBP and /CCBJI/RUMMMD_VENBP exist (SE93) and start the '
         'programs.', '☐'],
   ['3', 'Your user may display and change business partners, customers and suppliers in the '
         'company codes, sales organisations and purchasing organisations of the test '
         '(transaction BP works for you).', '☐'],
   ['4', 'SAP GUI may read and write files on your PC (the file dialog opens).', '☐'],
   ['5', 'Microsoft Excel is installed. For the text-file tests, Notepad or another editor that '
         'can save UTF-8.', '☐'],
   ['6', 'The test data in 2.2 is written down.', '☐']],
  widths=[0.8, 14.2, 2.0])

H('2.2  Test data to note down', 2)
P('Fill in this table before you start. The test cases refer to it as "test customer 1", '
  '"test sales area" and so on.')
T(['Item', 'Customer test', 'Supplier test'],
  [['Existing record 1 (number)', '', ''],
   ['Existing record 2 (number)', '', ''],
   ['Its business partner number', '', ''],
   ['Company code (e.g. 7827)', '', ''],
   ['Sales area (customer) - sales org / distribution channel / division\n'
    '(e.g. 0801 / Z1 / Z0)', '', '- not used -'],
   ['Purchasing organisation (supplier) (e.g. 2800)', '- not used -', ''],
   ['A second sales area / company code / purchasing org the record is NOT in yet', '', ''],
   ['Account group for new records', '', ''],
   ['Reconciliation account for that account group', '', ''],
   ['A valid customer group (KNVV-KDGRP) / supplier payment term (LFB1-ZTERM)', '', ''],
   ['A partner function to test (e.g. ZR)', '', '']],
  widths=[8.0, 4.5, 4.5])
NOTE('Use records that exist only for testing, or records whose changes you can set back. '
     'Always run the test run (2.3) first.', 'Important')

H('2.3  The rule for every upload', 2)
P('Run every upload twice:')
B('First with Test run ticked. Nothing is saved; the result list shows what would change.')
B('Then, only when the test run shows exactly what you expect, with Test run unticked.')

H('2.4  The result list', 2)
P('Every run ends with a list. Its columns:')
T(['Column', 'What it shows'],
  [['Status', 'Green = done (or would be done in a test run), yellow = warning, red = error, '
              'i = information'],
   ['Row', 'The row of the file the line is about (empty for a message about the whole file)'],
   ['Number', 'Customer or supplier number; "new (row n)" for a record still to be created'],
   ['Part', 'The part of the master record - General data, Company code, Sales area, Partner functions ...'],
   ['Field', 'The field, as it is named in the file'],
   ['Value in SAP', 'What SAP holds before the upload'],
   ['Value in file', 'What the file sends'],
   ['Type', 'S success, W warning, E error, I information'],
   ['Message', 'What happened, in words']],
  widths=[3.5, 13.5])
P('The title line of the list counts the records: "n record(s) posted, n with errors, n unchanged" '
  '(after a test run: "TEST RUN - n record(s) would be posted ...").')
doc.add_page_break()

# =================================================================== 3
H('3.  The selection screen', 1)
P('Start the transaction. The screen has seven blocks. Some fields open only when they apply - '
  'this is intended:')
T(['Block / field', 'Use', 'Open when'],
  [['**Create or change**', '', ''],
   ['Create new records', 'The file creates new customers / suppliers', 'always'],
   ['Change existing records', 'The file changes existing ones (default)', 'always'],
   ['**Download or upload**', '', ''],
   ['Download a file', 'Write a file (default)', 'always'],
   ['Upload a filled file', 'Read a file and post it', 'always'],
   ['**New records (create only)**', '', ''],
   ['Account group', 'Account group of the new records - decides the number range and the '
                     'business partner grouping', 'Create'],
   ['BP grouping (optional)', 'Only if the account group has no grouping in the CVI customising',
    'Create'],
   ['**Fields in the file**', '', ''],
   ['Choose fields (button)', 'Opens the list of all fields; tick the ones you want', 'Download'],
   ['(text next to the button)', 'How many fields are chosen', 'always'],
   ['Language of texts', 'Language in which texts (TEXT-... columns) are read and written',
    'always'],
   ['**Which customers / suppliers (download)**', '', ''],
   ['Customer / Supplier', 'Numbers to download. Empty = a file with the headings only',
    'Download'],
   ['Business partner', 'The same, by business partner number', 'Download'],
   ['Company code', 'Download only these company codes', 'Download'],
   ['Sales organization / Purchasing organization', 'Download only these', 'Download'],
   ['Records at most', 'Upper limit of records (default 1000)', 'Download'],
   ['**Upload**', '', ''],
   ['Test run (nothing is posted)', 'Ticked by default', 'Upload'],
   ['**File**', '', ''],
   ['File', 'Path and name on your PC - F4 opens the file dialog', 'always'],
   ['Excel (.xlsx) / Text, tab-separated (.txt)', 'Kind of file; the name must end in .xlsx or .txt',
    'always']],
  widths=[5.5, 8.5, 3.0])
P('The chosen fields are part of the screen: save them with Goto > Variants > Save as Variant '
  'and they come back with the variant. One variant per kind of change (for example "Payment terms", '
  '"Partner functions") saves choosing the fields every time.')
doc.add_page_break()

# =================================================================== 4
H('4.  Test cases - customer master', 1)
P('Transaction /CCBJI/RUSDMD_CUSBP. The supplier program is tested the same way in chapter 5.')

H('4.1  Screen and field choice', 2)
CASE('TC-C01', 'The selection screen opens and reacts',
     'Checks that the screen shows its texts and opens the fields that apply.',
     'none',
     ['Start transaction /CCBJI/RUSDMD_CUSBP.',
      'Choose "Change existing records" and "Download a file".',
      'Choose "Upload a filled file".',
      'Choose "Create new records".',
      'Without choosing any field, enter a file name ending in .xlsx and press Execute (F8) with '
      '"Download a file".',
      'Enter a file name ending in .txt while "Excel (.xlsx)" is chosen, and press F8.'],
     ['The screen shows all texts (no field names like P_CRT). Change and Download are chosen; '
      'Test run is ticked.',
      'Account group and BP grouping are greyed out. Choose fields, Customer, Business partner, '
      'Company code, Sales organization and Records at most are open. Test run is greyed out.',
      'The download fields (Choose fields, Customer ...) are greyed out; Test run is open.',
      'Account group and BP grouping are open.',
      'Message "Choose the fields for the file first". Nothing is written.',
      'Message "Excel chosen - the file name must end in .xlsx".'])

CASE('TC-C02', 'Choose fields and keep them in a variant',
     'Checks the field list, and that a variant brings the choice back.',
     'none',
     ['Choose "Change existing records" and "Download a file". Press "Choose fields".',
      'Find and tick: KNB1-ZTERM (terms of payment, company code), KNVV-KDGRP (customer group), '
      'ADRC-SORT1 (search term), ADR2-TEL_NUMBER (telephone). Press Continue (Enter).',
      'Goto > Variants > Save as Variant. Name it TEST_CHANGE, save.',
      'Leave the transaction, start it again, Goto > Variants > Get, choose TEST_CHANGE.',
      'Press "Choose fields" again.'],
     ['A popup "Choose the fields for the file" lists every field with its Part (General data, '
      'Company code, Sales area ...), Field name and Description.',
      'The text next to the button reads "4 field(s) chosen - the keys are added automatically".',
      'The variant is saved.',
      'The text reads "4 field(s) chosen ...".',
      'The four fields are ticked.'])

H('4.2  Download', 2)
CASE('TC-C03', 'Download an empty template',
     'A file with the headings only, to be filled by hand.',
     'Variant TEST_CHANGE from TC-C02.',
     ['Get variant TEST_CHANGE. Leave Customer and Business partner empty.',
      'Enter File, e.g. C:\\temp\\cust_template.xlsx (or use F4). Press F8.',
      'Open the file in Excel.'],
     ['-',
      'Message "Template with n columns written to ...".',
      'Row 1 holds: KNA1-KUNNR, KNB1-BUKRS, KNVV-VKORG, KNVV-VTWEG, KNVV-SPART, then the four '
      'chosen fields. No data rows. The keys were added by the program.'])

CASE('TC-C04', 'Download existing customers',
     'The file shows what SAP holds today, one row per company code / sales area.',
     'Test customers 1 and 2 (2.2).',
     ['Get variant TEST_CHANGE. Enter test customers 1 and 2 under Customer.',
      'Enter File C:\\temp\\cust_change.xlsx. Press F8.',
      'Open the file. Compare each value with transaction BP (role "Customer" and '
      '"Customer (Fin. Accounting)" / "Customer (Sales)") for the same company code and sales area.',
      'Run again with Company code and Sales organization restricted to the test values.'],
     ['-',
      'Message "n rows written to ...".',
      'One row per combination of company code and sales area of each customer. Every value is the '
      'value in BP. Numbers have no leading zeros.',
      'Only the rows of that company code / sales area.'])

CASE('TC-C05', 'Download by business partner number, and as a text file',
     'Business partner numbers work in place of customer numbers; the text format works.',
     'Business partner number of test customer 1.',
     ['Get variant TEST_CHANGE. Leave Customer empty, enter the business partner number.',
      'Choose "Text, tab-separated (.txt)", File C:\\temp\\cust_change.txt. Press F8.',
      'Open the file in Excel (File > Open, choose the .txt, delimiter Tab).'],
     ['-',
      'Message "n rows written to ...".',
      'The same columns and values as TC-C04 for that customer.'])

H('4.3  Change upload', 2)
CASE('TC-C06', 'Change fields - test run',
     'The program finds exactly the changed fields and posts nothing in a test run.',
     'File cust_change.xlsx from TC-C04.',
     ['In the file, for test customer 1, change KNVV-KDGRP to another valid customer group and '
      'ADRC-SORT1 to "TESTUPLOAD". Leave every other cell as downloaded. Save and close Excel.',
      'Choose "Change existing records" and "Upload a filled file". Test run ticked. '
      'File = cust_change.xlsx. Press F8.',
      'Look at the list title and lines.',
      'Check the customer in BP.'],
     ['-',
      'The result list opens.',
      'Two blue lines "Would change (test run)" with Value in SAP and Value in file for the two '
      'fields; a green line "Test run OK - nothing was posted"; test customer 2: "No change - the '
      'file matches SAP". Title: "TEST RUN - 1 record(s) would be posted, 0 with errors, 1 unchanged".',
      'Nothing has changed in BP.'])

CASE('TC-C07', 'Change fields - productive',
     'The same file changes the customer.',
     'TC-C06 passed.',
     ['Untick Test run. Same file. Press F8.',
      'Check the customer in BP: customer group in the sales area, search term in the general data.',
      'Upload the same file once more (Test run ticked).'],
     ['Green lines "Changed" for both fields and "Saved" for the customer. Title "1 record(s) '
      'posted, 0 with errors, 1 unchanged".',
      'Both new values are there. Everything else is unchanged.',
      '"No change - the file matches SAP" for both customers.'])

CASE('TC-C08', 'Leave a field alone with #, clear a field with an empty cell',
     'Empty cells clear fields only after confirmation; # protects a field.',
     'File cust_change.xlsx after TC-C07. Test customer 1 has a telephone number.',
     ['In the file, put # in ADRC-SORT1 of test customer 1, and empty its ADR2-TEL_NUMBER cell. '
      'Save. Upload with Test run ticked.',
      'Choose "Cancel" in the popup.',
      'Upload again (Test run ticked) and choose "Clear them".',
      'Untick Test run, upload, choose "Clear them". Check BP.',
      'Restore the telephone number by uploading it again (or in BP).'],
     ['A popup "Empty cells": "1 field(s) are empty in the file but filled in SAP. Empty cells clear '
      'the field. Clear them? To leave a field as it is, put # in its cell instead."',
      'Yellow line "Cancelled - nothing was posted. Put # in the cells to leave unchanged." Nothing '
      'is posted.',
      'Line for ADR2-TEL_NUMBER "Would change (test run)" with an empty Value in file. No line for '
      'ADRC-SORT1.',
      'Line "Cleared". The telephone number is gone in BP; the search term is unchanged.',
      '-'])

CASE('TC-C09', 'Wrong values are refused',
     'Every value is checked before anything is posted.',
     'File cust_change.xlsx.',
     ['Make one change per row of the file, each a different mistake:\n'
      'row 2: KNVV-KDGRP = ZZ (a group that does not exist)\n'
      'row 3: KNB1-ZTERM = a text longer than 4 characters\n'
      'and add a column KNA1-XXXXX with a value in it.\n'
      'Upload with Test run ticked.',
      'Remove the KNA1-XXXXX column, upload again.',
      'Copy column KNVV-KDGRP so that it appears twice, upload.',
      'Delete the KNA1-KUNNR column, upload.',
      'Empty the KNA1-KUNNR cell of one row (Change), upload.',
      'Enter a customer number that does not exist, upload.'],
     ['Red line: Column "KNA1-XXXXX" is not a field this program knows. "The file cannot be read - '
      'nothing was posted".',
      'Red lines: "ZZ" is not allowed for KNVV-KDGRP - it is not in T151; "..." is longer than the '
      '4 characters KNB1-ZTERM holds. These records are not posted; the others are.',
      'Red line: KNVV-KDGRP is in the file twice - columns ... Nothing posted.',
      'Red line: A change needs the number column KNA1-KUNNR. Nothing posted.',
      'Red line for that row: No number in this row - a change needs one.',
      'Red line: <number> does not exist - choose Create to create it.'])

CASE('TC-C10', 'Extend a customer to a new sales area or company code',
     'A row with keys the customer does not have yet adds them.',
     'A sales area (or company code) the test customer is not in yet (2.2).',
     ['Choose fields: KNVV-KDGRP, KNVV-WAERS, KNVV-KALKS, KNVV-VSBED and any other field your '
      'sales area needs, KNB1-AKONT and KNB1-ZTERM for a company code. Download test customer 1.',
      'Copy one row and change the sales organisation (or company code) to the new one. Fill the '
      'fields. Upload with Test run ticked.',
      'If anything is red, add the missing field (the message names it), test again.',
      'Untick Test run, upload. Check BP.'],
     ['-',
      'Lines "Would change (test run)" for the new sales area\'s fields, and "Test run OK".',
      '-',
      '"Saved". In BP the customer now has the new sales area with the values from the file.'])

CASE('TC-C11', 'Partner functions - add, change, delete (ACTION column)',
     'A repeating part: one row per entry, ACTION says what to do.',
     'Test customer 1 and a partner function (2.2).',
     ['Choose fields: the partner function fields (part "Partner functions", e.g. KNVP-KUNN2). '
      'Download test customer 1.',
      'In the file, add a row for a new partner function with ACTION = I. Upload with Test run.',
      'Change ACTION of an existing partner function row to I, upload with Test run.',
      'Put ACTION = D on the new row after it was posted (step 2 productive), upload.',
      'Put ACTION = U on a row whose partner function does not exist, upload.',
      'Put ACTION = X on a row, upload.'],
     ['Columns: KNA1-KUNNR, sales area keys, the partner function keys (KNVP-PARVW, KNVP-PARZA), '
      'ACTION, then the fields. One row per partner function.',
      'Line "Entry added" for the new partner function and "Test run OK".',
      'Red line: Entry ... exists already - use U to change it.',
      'Line "Entry deleted". In BP the partner function is gone.',
      'Red line: Entry ... does not exist - it cannot be updated.',
      'Red line: ACTION must be I (insert), U (update), D (delete) or empty.'])

CASE('TC-C12', 'Texts',
     'Texts of the general data, company code and sales area are read and written.',
     'A text ID used at CCBJI (in BP > Customer > Texts).',
     ['Choose a text field (part "Texts - general", e.g. TEXT-KNA1-0001). Language of texts = JA '
      '(or EN). Download test customer 1.',
      'Change the text in the file (Japanese characters are fine). Upload, Test run, then productive.',
      'Check the text in BP.'],
     ['The column holds the text of that ID in that language.',
      '"Changed" for the text column.',
      'The new text is there, in the chosen language.'])

CASE('TC-C13', 'Japanese characters and text files',
     'Japanese names survive the round trip in both file formats.',
     'A test customer with a Japanese name, or change one.',
     ['Choose ADRC-NAME1. Download as .xlsx; check the name in Excel.',
      'Change the name in Excel to another Japanese name and upload (Test run).',
      'Download as .txt. Open it in Notepad; change the name; save as UTF-8. Upload (Test run).',
      'Save the same .txt as ANSI / Shift-JIS and upload again (Test run).'],
     ['The Japanese name is shown correctly.',
      '"Would change (test run)" with the new name shown correctly.',
      'The same.',
      'The same - the program reads UTF-8 and Shift-JIS.'])

H('4.4  Create', 2)
CASE('TC-C14', 'Create a customer from a sample',
     'A sample copied from an existing customer, with its number left empty, creates a new one.',
     'Account group, reconciliation account, company code and sales area (2.2).',
     ['Choose "Create new records" and "Download a file". Enter the Account group.',
      'Choose fields: ADRC-NAME1, ADRC-SORT1, ADRC-STREET, ADRC-CITY1, ADRC-POST_CODE1, '
      'ADRC-REGION, ADRC-COUNTRY, ADRC-LANGU, KNB1-AKONT, KNB1-ZTERM, KNVV-WAERS, KNVV-KDGRP, '
      'KNVV-KALKS, KNVV-VSBED and the other fields your account group requires. Save as variant '
      'TEST_CREATE.',
      'Enter test customer 1 under Customer. File C:\\temp\\cust_create.xlsx. F8. Open the file.',
      'Change the name to "TEST CREATE 01" and the search term. Save.',
      'Choose "Upload a filled file". Account group as before. Test run ticked. F8.',
      'If a field is reported as required, add the column, fill it and test again.',
      'Untick Test run. F8.',
      'Open the new customer in BP.'],
     ['Account group is open.',
      'The program adds the fields a new record cannot do without (name, search term, country, '
      'language, reconciliation account, currency) - they appear in the file even if not ticked.',
      'One row per company code / sales area of customer 1, with its values - and the KNA1-KUNNR '
      'column empty.',
      '-',
      'Green "Test run OK - nothing was posted" for "new (row 2)".',
      '-',
      'Green line "Created <customer number> - business partner <BP number>".',
      'The business partner has the customer roles, the grouping of the account group, the account '
      'group, and the company code and sales area with the values from the file.'])

CASE('TC-C15', 'Create - wrong use is refused',
     'The program stops creation mistakes before SAP does.',
     'File cust_create.xlsx.',
     ['Choose Create and Upload, leave Account group empty, F8.',
      'Enter an account group that does not exist, F8.',
      'Enter an existing customer number in KNA1-KUNNR of a row, upload with Test run.',
      'Empty ADRC-NAME1 in a row, upload with Test run.'],
     ['Message "Creation needs an account group".',
      'Message "Account group ... does not exist".',
      'Red line: <number> exists already - choose Change to change it.',
      'Red line naming the missing name (from the program or from SAP). Nothing is created.'])

CASE('TC-C16', 'Create several customers in one file',
     'Each row without a number is a new customer.',
     'cust_create.xlsx from TC-C14.',
     ['Copy the data row twice, give each a different name and search term. Upload, Test run, then '
      'productive.',
      'Make one of the three rows wrong (e.g. KNVV-KDGRP = ZZ). Upload with Test run.'],
     ['Three lines "Created ..." each with its own customer and business partner number.',
      'The wrong row is red and not created; the other two show "Test run OK".'])
doc.add_page_break()

# =================================================================== 5
H('5.  Test cases - supplier master', 1)
P('Transaction /CCBJI/RUMMMD_VENBP. The screen, the file rules and the result list are the same '
  'as for the customer. Repeat the customer test cases with these differences:')
T(['Customer program', 'Supplier program'],
  [['KNA1-KUNNR (number column)', 'LFA1-LIFNR'],
   ['Customer (selection)', 'Supplier'],
   ['Sales organization (selection)', 'Purchasing organization'],
   ['KNA1 general data', 'LFA1 general data'],
   ['KNB1 company code', 'LFB1 company code'],
   ['KNVV sales area (VKORG, VTWEG, SPART)', 'LFM1 purchasing organisation (EKORG)'],
   ['KNVP partner functions', 'WYT3 partner functions (keys LTSNR, WERKS, PARVW, PARZA)'],
   ['KNBK bank details', 'LFBK bank details'],
   ['KNBW withholding tax', 'LFBW withholding tax'],
   ['KNVI tax classification, KNVA unloading points', '- (not on the supplier)'],
   ['Roles: customer', 'Roles: supplier']],
  widths=[8.5, 8.5])

T(['Supplier test', 'Same as', 'Use these fields / data'],
  [['TC-V01  Screen', 'TC-C01', '-'],
   ['TC-V02  Field choice and variant', 'TC-C02', 'LFB1-ZTERM, LFM1-WAERS, ADRC-SORT1, ADR2-TEL_NUMBER'],
   ['TC-V03  Empty template', 'TC-C03', 'row 1: LFA1-LIFNR, LFB1-BUKRS, LFM1-EKORG, then the fields'],
   ['TC-V04  Download existing suppliers', 'TC-C04', 'compare with BP role "Supplier"'],
   ['TC-V05  By BP number, text file', 'TC-C05', '-'],
   ['TC-V06/07  Change, test then productive', 'TC-C06/07', 'change LFB1-ZTERM and ADRC-SORT1'],
   ['TC-V08  # and empty cells', 'TC-C08', '-'],
   ['TC-V09  Wrong values', 'TC-C09', 'LFB1-ZTERM = a term that does not exist'],
   ['TC-V10  Extend to a purchasing organisation / company code', 'TC-C10',
    'LFM1-WAERS, LFM1-ZTERM, LFM1-KALSK ... for a purchasing org (e.g. 2801) the supplier is not in'],
   ['TC-V11  Partner functions', 'TC-C11', 'WYT3 fields; ACTION I / U / D'],
   ['TC-V12  Bank details', 'TC-C11', 'LFBK-BANKS, LFBK-BANKL, LFBK-BANKN, LFBK-KOINH; ACTION I / U / D'],
   ['TC-V13  Texts', 'TC-C12', 'TEXT-LFA1-...'],
   ['TC-V14  Japanese characters', 'TC-C13', '-'],
   ['TC-V15/16  Create, single and several', 'TC-C14/16',
    'supplier account group; LFB1-AKONT, LFM1-WAERS are added automatically'],
   ['TC-V17  Create - wrong use', 'TC-C15', '-']],
  widths=[6.0, 2.5, 8.5])
T(['Result', 'Tested by / date', 'Remarks / defect no.'],
  [['☐ Pass    ☐ Fail  (list the TC-V numbers that failed)', '', '']], widths=[6.5, 4.5, 6.0])
doc.add_page_break()

# =================================================================== 6
H('6.  Checking the result in SAP', 1)
T(['What', 'Where'],
  [['The record as a whole', 'Transaction BP - open the business partner, choose the role '
                             '(Customer / Customer (Fin. Accounting) / Customer (Sales), or '
                             'Supplier / Supplier (Fin. Accounting) / Supplier (Purchasing))'],
   ['Who changed what and when', 'BP > Extras > Change history (or the change documents of the '
                                 'customer / supplier)'],
   ['The tables', 'SE16N: KNA1, KNB1, KNVV, KNVP, KNVI / LFA1, LFB1, LFM1, WYT3, LFBK'],
   ['Business partner behind a number', 'SE16N CVI_CUST_LINK / CVI_VEND_LINK, then BUT000']],
  widths=[5.0, 12.0])

H('7.  What the programs do not do', 1)
B('Contact persons can be changed but not created from the file: a contact person is a business '
  'partner of its own, created in BP.')
B('Data held in tables of their own outside the business partner (for example the IS-JP invoice '
  'summary data) is not part of the file.')
B('A file holds one repeating part (partner functions OR bank details ...), see 1.3.')
B('A new record\'s number is given by SAP from the number range of the account group; it cannot be '
  'chosen in the file.')

H('8.  Reporting a defect', 1)
P('When a test fails, send:')
B('The test case number, and what you expected.')
B('A screenshot of the selection screen (or the variant name) and of the whole result list.')
B('The file you uploaded.')
B('The customer / supplier / business partner number.')

H('9.  Sign-off', 1)
T(['Area', 'Result', 'Name', 'Date', 'Signature'],
  [['Customer master (TC-C01 - TC-C16)', '☐ Accepted   ☐ Not accepted', '', '', ''],
   ['Supplier master (TC-V01 - TC-V17)', '☐ Accepted   ☐ Not accepted', '', '', '']],
  widths=[5.0, 4.0, 3.0, 2.0, 3.0])

doc.save(OUT)
print('wrote', OUT)
