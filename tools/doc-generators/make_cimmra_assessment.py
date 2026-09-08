"""The assessment document sent to Cipla for the Cimmra vendor interface."""
from docx import Document
from docx.shared import Pt, Cm, RGBColor
from docx.enum.text import WD_ALIGN_PARAGRAPH
from docx.enum.table import WD_TABLE_ALIGNMENT

doc = Document()
st = doc.styles['Normal']
st.font.name = 'Calibri'; st.font.size = Pt(10.5)
for s in ('Heading 1', 'Heading 2', 'Heading 3'):
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
def KV(rows, widths):
    """A plain two column block with no heading row - the cover details."""
    t = doc.add_table(rows=0, cols=2)
    t.style = 'Light List Accent 1'
    t.alignment = WD_TABLE_ALIGNMENT.LEFT
    for k, v in rows:
        cells = t.add_row().cells
        cells[0].text = ''; r = cells[0].paragraphs[0].add_run(k)
        r.bold = True; r.font.size = Pt(9.5)
        cells[1].text = ''; r = cells[1].paragraphs[0].add_run(v); r.font.size = Pt(9.5)
    for r_ in t.rows:
        for i, w in enumerate(widths):
            r_.cells[i].width = Cm(w)
    doc.add_paragraph()
    return t

def T(headers, rows, widths=None):
    t = doc.add_table(rows=1, cols=len(headers))
    t.style = 'Light Grid Accent 1'
    t.alignment = WD_TABLE_ALIGNMENT.LEFT
    for i, h in enumerate(headers):
        c = t.rows[0].cells[i]; c.text = ''
        r = c.paragraphs[0].add_run(h); r.bold = True; r.font.size = Pt(9.5)
    for row in rows:
        cells = t.add_row().cells
        for i, v in enumerate(row):
            v = '' if v is None else str(v)
            cells[i].text = ''
            para = cells[i].paragraphs[0]
            for j, ln in enumerate(v.split('\n')):
                if j: para = cells[i].add_paragraph()
                rr = para.add_run(ln); rr.font.size = Pt(9.5)
    if widths:
        for r_ in t.rows:
            for i, w in enumerate(widths):
                r_.cells[i].width = Cm(w)
    doc.add_paragraph()
    return t

# ------------------------------------------------------------------ cover
doc.add_heading('Cimmra Vendor Portal Interface', level=0)
P('Assessment for SAP S/4HANA Business Partner', bold=True, size=13)
P('Changes required in the existing custom programs, and open points for confirmation',
  italic=True)
doc.add_paragraph()
KV([
    ('Prepared for', 'Cipla Limited'),
    ('Prepared by', 'Diligent Consulting'),
    ('Subject', 'Vendor / Customer to Business Partner'),
    ('Basis', 'Six programs and the design document received 07.09.2026, '
              'ZFI_CIMMRA_VENDOR_EXT received 08.09.2026'),
    ('Status', 'Assessment only - no program has been changed'),
], [4.0, 12.5])

# ------------------------------------------------------------------ 1
H('1. Purpose', 1)
P('This document sets out, program by program, what has to change so that the Cimmra '
  'vendor interface creates and maintains Business Partners in S/4HANA instead of vendor '
  'master records.')
P('The inbound file layout and the outbound response mechanism are unchanged. The portal '
  'sends the same file it sends today, and receives a response file in the same format.')
P('Section 8 lists what we need from Cipla before development starts.')
P('ZFI_CIMMRA_VENDOR_EXT was received after the first draft of this document and is '
  'covered in section 5.6. It is the one program that has to be rewritten.', italic=True)

# ------------------------------------------------------------------ 2
H('2. What was reviewed', 1)
T(['Object', 'Type', 'Role in the interface'], [
    ['ZFI_CIMMRA_VENDOR_CREATE', 'Report', 'Reads the vendor file, calls create or change'],
    ['ZFI_CIMMRA_EMPVEND', 'Report', 'Employee vendor creation'],
    ['ZFI_CIMMRA_EMPVEND_UPD', 'Report', 'Employee vendor change'],
    ['ZFI_CIMMRA_BANK_CREATE', 'Report', 'Bank key creation'],
    ['ZFI_CIMMRA_VENDOR_EXT', 'Report', 'Extends a vendor to a further company code '
     'and purchasing organisation'],
    ['ZMM_RFC_VENDOR_CREATE', 'Function module', 'Builds the master data and posts it'],
    ['ZMM_RFC_VENDOR_CHANGE', 'Function module', 'Builds the changes and posts them'],
], [6.0, 3.2, 7.3])
P('Also reviewed: "Cimmra Vendor Portal Integration with SAP Vendor Master", which '
  'describes the process flow, the SFTP and AL11 directories, and the response file.')

# ------------------------------------------------------------------ 3
H('3. Assessment in short', 1)
P('The interface is closer to S/4HANA than its age suggests.', bold=True)
P('Both function modules already build their data in the CVI interface structures - '
  'VMDS_EI_VMD_CENTRAL for the general data, CVIS_EI_1VL for the address, and the '
  'company code, purchasing, bank, withholding tax, tax grouping and contact person '
  'nodes. Those structures are exactly the vendor half of what the S/4HANA Business '
  'Partner interface expects.')
P('The field mapping, which is the bulk of both function modules, is reused unchanged. '
  'What is added is the Business Partner half of the same message - category, grouping, '
  'roles, name, search terms and address - and a change of the call that posts it.')
P('That holds for five of the six programs. The exception is ZFI_CIMMRA_VENDOR_EXT, '
  'which does not use the interface structures at all - it drives transaction XK01 '
  'through a batch input recording. XK01 does not exist in S/4HANA, so that one '
  'program is rewritten rather than adapted. Section 5.6 sets out what it does today '
  'and what replaces it.', bold=True)

# ------------------------------------------------------------------ 4
H('4. The change that applies to both function modules', 1)
P('Today both function modules end with VMD_EI_API=>MAINTAIN_BAPI, which creates a '
  'vendor. In S/4HANA the Business Partner is the leading object and the vendor is '
  'derived from it. The posting call becomes CL_MD_BP_MAINTAIN, which takes the Business '
  'Partner and the vendor in one message and creates both together.')
P('Six points follow from that change.', bold=True)
T(['#', 'Change', 'Why'], [
    ['4.1', 'Add the Business Partner node to the message',
     'Category, grouping, roles, name, search terms and address. The grouping and the '
     'roles are read from the CVI Customizing tables at runtime, so no value is hard '
     'coded and nothing has to be maintained in the program.'],
    ['4.2', 'Identify the partner in every message, including a creation',
     'The interface refuses a message that does not name the partner, even when the '
     'number is still to be assigned. A creation is identified by a generated GUID, '
     'which becomes the new partner’s key; a change is identified by the GUID the '
     'CVI link table already holds.'],
    ['4.3', 'Replace the task code M with I or U',
     'The Business Partner interface accepts only I (insert) and U (update). '
     'ZMM_RFC_VENDOR_CHANGE uses M in 71 places and would be refused on every record. '
     'The task is derived per node from whether the record already exists.'],
    ['4.4', 'Remove the vendor number range logic',
     'The number now comes from the Business Partner grouping, not from the vendor '
     'account group. The existing code draws a number from the vendor range in advance, '
     'and on failure writes the number range table back directly, which is not '
     'supported and must be removed.'],
    ['4.5', 'Read both numbers back after the save',
     'Unless the grouping is set for the same number, the Business Partner number and '
     'the vendor number are different. Both are read back from the CVI link table so '
     'the response file can carry them.'],
    ['4.6', 'Commit and clear the Business Partner memory between records',
     'The Business Partner keeps a memory for the unit of work just closed. Without '
     'clearing it the second record in every file is refused. This affects the '
     'interface directly because a whole file is processed in one run.'],
], [1.3, 5.5, 9.7])

# ------------------------------------------------------------------ 5
H('5. Changes by program', 1)

H('5.1  ZMM_RFC_VENDOR_CREATE  (function module)', 2)
T(['Area', 'Change', 'Effort'], [
    ['Posting call', 'CL_MD_BP_MAINTAIN in place of VMD_EI_API=>MAINTAIN_BAPI', 'Medium'],
    ['Business Partner node', 'New - built from the data already mapped', 'Medium'],
    ['Partner GUID', 'Generated on creation, so the message identifies the partner', 'Small'],
    ['Number assignment', 'Remove VMD_EI_API=>GET_NUMBER and the direct write to the '
     'number range table', 'Small'],
    ['CIN details', 'The second posting call added during ATC remediation is removed; '
     'the CIN fields are folded into the single call', 'Small'],
    ['TAN exemption', 'Replace the direct table update with the standard function '
     'module', 'Small'],
    ['Commit', 'BAPI_TRANSACTION_COMMIT and clearing of the Business Partner memory', 'Small'],
    ['Field mapping', 'No change', '-'],
], [3.6, 10.4, 2.5])

H('5.2  ZMM_RFC_VENDOR_CHANGE  (function module)', 2)
T(['Area', 'Change', 'Effort'], [
    ['Task codes', '71 occurrences of task M become I or U, derived from whether the '
     'record exists', 'Medium'],
    ['Posting call', 'As 5.1', 'Medium'],
    ['Business Partner node', 'As 5.1 - the same routine', 'Small'],
    ['Repeating data', 'Bank details, withholding tax types and partner functions that '
     'are not in the message are deleted by the interface. Existing entries are read '
     'and sent back alongside the new ones.', 'Medium'],
    ['TAN exemption, commit', 'As 5.1', 'Small'],
    ['Field mapping', 'No change', '-'],
], [3.6, 10.4, 2.5])

H('5.3  ZFI_CIMMRA_VENDOR_CREATE  (report)', 2)
T(['Area', 'Change', 'Effort'], [
    ['File handling', 'Directory paths are built with a backslash. The quality paths in '
     'the design document are Unix style. The separator should follow the platform of '
     'the application server.', 'Small'],
    ['File handling', 'The inbound file is deleted before it is closed', 'Small'],
    ['Job safety', 'The job runs every fifteen minutes with no lock, so two runs can '
     'pick up the same file. A lock is recommended.', 'Small'],
    ['Response file', 'Same nine columns. The Business Partner number is added - see '
     'question Q3.', 'Small'],
    ['Authorisation', 'No authority check before master data is created', 'Small'],
], [3.6, 10.4, 2.5])

H('5.4  ZFI_CIMMRA_EMPVEND and ZFI_CIMMRA_EMPVEND_UPD  (reports)', 2)
T(['Area', 'Change', 'Effort'], [
    ['Business Partner category', 'An employee vendor is normally a person rather than '
     'an organisation, which changes the name fields, the grouping and the number '
     'range. See question Q1.', 'Small'],
    ['File handling, job safety, response', 'As 5.3', 'Small'],
], [3.6, 10.4, 2.5])

H('5.5  ZFI_CIMMRA_BANK_CREATE  (report)', 2)
P('BAPI_BANK_CREATE is unchanged in S/4HANA. Only the file handling points in 5.3 '
  'apply. This is the smallest of the five.')

H('5.6  ZFI_CIMMRA_VENDOR_EXT  (report)', 2)
P('This program is the exception. It does not call the function modules and it does '
  'not use the interface structures. It builds a batch input recording and runs it '
  'through transaction XK01 - screens 0100, 0210, 0215, 0220, 0310, 0610 and 4000 of '
  'module pool SAPMF02K, and screen 0100 of SAPLJ1I_MASTER for the CIN data.')
P('Neither XK01 nor SAPMF02K exists in S/4HANA. The Business Partner is the single '
  'point of entry for supplier master data, and the CIN master screens were removed '
  'when those fields moved onto LFA1. The program therefore stops working at the '
  'conversion, and is rewritten rather than adapted.', bold=True)
P('The rewrite is not large, because the field set is small. What the recording sets '
  'today is:')
T(['Screen', 'What it sets', 'Replaced by'], [
    ['0100', 'Vendor, company code, purchasing organisation, account group, and the '
     'reference vendor with its company code and purchasing organisation',
     'The key of the message, and a read of the reference - see the note below'],
    ['0210', 'Reconciliation account, previous account number', 'Company code node'],
    ['0215', 'Terms of payment, payment history indicator, payment methods',
     'Company code node'],
    ['0610', 'Withholding tax country. The withholding tax lines themselves are '
     'commented out in the recording, so no tax type is extended today.',
     'Company code node'],
    ['0310', 'Purchasing currency and telephone', 'Purchasing organisation node'],
    ['4000', 'Three indicators on the custom table ZFI_TAX_TAB', 'See the note below'],
    ['SAPLJ1I_MASTER', 'PAN number and SSI status', 'General data - these are LFA1 '
     'fields in S/4HANA'],
], [2.8, 7.4, 6.3])
P('Two points need a decision rather than only development.', bold=True)
T(['#', 'Point', 'What it means'], [
    ['A', 'The reference vendor',
     'On the screen, naming a reference vendor copies everything the reference has. '
     'The Business Partner interface has no such feature. The new program reads the '
     'reference itself and carries the values across - but only the fields it reads. '
     'Which fields are to be copied has to be agreed. See question Q7.'],
    ['B', 'ZFI_TAX_TAB',
     'A custom table filled today through a screen enhancement on XK01. Without the '
     'transaction there is no screen, so the program writes the table itself. This '
     'needs the table definition and its update rules. See requirement R7.'],
], [1.3, 4.4, 10.8])
P('The program also reads J_1IMOVEND directly, which is obsolete in S/4HANA - those '
  'fields are columns of LFA1 now. And the file handling points in 5.3 apply to it as '
  'they do to the other reports.')

# ------------------------------------------------------------------ 6
H('6. What does not change', 1)
B('The inbound file layout. The portal sends the same file.')
B('The SFTP and AL11 directory structure and the PI/PO channels.')
B('The response file format - the same columns, in the same order.')
B('The background job and its fifteen minute schedule.')
B('The field mapping inside both function modules.')

# ------------------------------------------------------------------ 7
H('7. Indicative effort', 1)
T(['Object', 'Relative effort'], [
    ['ZMM_RFC_VENDOR_CREATE', 'Largest of the six'],
    ['ZMM_RFC_VENDOR_CHANGE', 'Comparable, plus the task codes and the repeating data'],
    ['ZFI_CIMMRA_VENDOR_CREATE', 'Small'],
    ['ZFI_CIMMRA_EMPVEND / _UPD', 'Small each, once Q1 is answered'],
    ['ZFI_CIMMRA_BANK_CREATE', 'Very small'],
    ['ZFI_CIMMRA_VENDOR_EXT', 'Rewritten rather than adapted, but the field set is '
     'small - comparable to one of the reports'],
], [8.0, 8.5])
P('The Business Partner node is written once and used by both function modules. A firm '
  'estimate follows once the open points in section 8 are answered.')

# ------------------------------------------------------------------ 8
H('8. What we need from Cipla', 1)
P('The following points need a decision or a missing object before development starts. '
  'They are ordered by how much they hold up.', italic=True)

H('8.1  Questions', 2)
T(['#', 'Question', 'Why it matters'], [
    ['Q1', 'Is an employee vendor to be created as a person Business Partner or as an '
     'organisation?',
     'Decides the name fields, the grouping and the number range for the two employee '
     'vendor programs. A business decision, not a technical one.'],
    ['Q2', 'Should the Business Partner and the vendor carry the same number?',
     'A Customizing setting. If they differ, the portal has to be told both numbers.'],
    ['Q3', 'May the response file carry the Business Partner number - as a new column, '
     'or appended to the message text?',
     'The portal has to accept whichever form is chosen. Our recommendation is a new '
     'column, so the message text stays free.'],
    ['Q4', 'Is the customer side also in scope?',
     'The subject of the request is "Vendor / Customer to BP", but none of the programs '
     'received touches customer master data. If customers are in scope, the programs '
     'concerned have not been shared.'],
    ['Q5', 'Is the operating system of the S/4HANA application server Windows or Unix?',
     'The directory paths are built with a backslash. On a Unix server every path fails.'],
    ['Q6', 'Are the function modules called from anywhere other than these four reports '
     '- for example directly by PI/PO?',
     'Decides whether the interface of the function modules may change, or must be kept '
     'as it is.'],
    ['Q7', 'On a vendor extension, which fields should be copied from the reference '
     'vendor?',
     'Transaction XK01 copied everything the reference had. The Business Partner '
     'interface has no reference feature, so the fields to copy have to be named. '
     'Our proposal: reconciliation account, terms of payment, payment methods, payment '
     'history indicator and planning group from the reference company code, and '
     'currency, schema group and GR based invoice verification from the reference '
     'purchasing organisation - each overridden by whatever the file supplies.'],
], [1.3, 7.2, 8.0])

H('8.2  Objects and files we need', 2)
T(['#', 'What', 'Why'], [
    ['R1', 'The customer programs, if Q4 is yes', 'None were received.'],
    ['R2', 'ZGEN_UPDATE_X and ZPARAM_TABLE',
     'Used throughout. We have the calls but not the definitions.'],
    ['R3', 'The ZKRP_* structures used by the two function modules',
     'About forty structures carrying the interface parameters.'],
    ['R4', 'One sample inbound file and one sample response file, for each of the five '
     'scenarios', 'To test against the real layout rather than a reconstruction.'],
    ['R5', 'A development system and a transport, with the CVI Customizing already '
     'converted', 'The Customizing decides the grouping and the roles at runtime.'],
    ['R6', 'ZFI_TAX_TAB - the table definition, and how the three indicators are '
     'derived', 'Filled today by a screen enhancement on XK01, which will not exist.'],
    ['R7', 'The screen enhancement on XK01 that maintains ZFI_TAX_TAB, if it is a '
     'custom development', 'To see what it does before the same is done in code.'],
], [1.3, 7.2, 8.0])

H('8.3  Access', 2)
B('Development access to the S/4HANA development system, with authorisation to create '
  'and change vendor and Business Partner master data.')
B('Read access to the AL11 inbound and outbound directories used by the interface.')

# ------------------------------------------------------------------ 9
H('9. Suggested next steps', 1)
N('Cipla answers the questions in 8.1 and provides the objects in 8.2.')
N('A working session to walk through the input file and the response file, scenario by '
  'scenario.')
N('We issue a firm estimate and a technical specification for each program.')
N('Development, followed by a unit test in the development system.')
N('Cipla runs a user acceptance test with real portal files end to end.')

doc.add_paragraph()
P('Prepared by Diligent Consulting. Based on the programs and the design document '
  'received on 07.09.2026. No program has been changed.', italic=True, size=9)

import os
out = os.path.join(os.path.dirname(__file__), '..', '..', 'docs', 'cimmra',
                   'Cimmra_Vendor_Interface_S4HANA_BP_Assessment.docx')
doc.save(os.path.abspath(out))
print('written', os.path.abspath(out))
