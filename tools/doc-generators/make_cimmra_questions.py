"""The open-points document sent to Cipla for the Cimmra vendor -> Business Partner work.

It carries only what we need Cipla to answer or provide, drawn from the five
sample files (nine vendor records) and the programs that consume them.
"""
from docx import Document
from docx.shared import Pt, Cm, RGBColor
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

# ------------------------------------------------------------------ title
doc.add_heading('Cimmra Vendor Interface - Business Partner Creation', level=0)
P('Open points for Cipla', bold=True, size=13)
doc.add_paragraph()

# ------------------------------------------------------------------ 1
H('1. What this covers', 1)
P('The vendor files the portal sends today are to create Business Partners in '
  'S/4HANA instead of vendor master records. The file layout and the response file '
  'stay as they are.')
P('We have read the five sample files supplied - nine vendor records in all - against '
  'the programs that consume them, and checked each field through to where it is '
  'posted. The points below are the ones we cannot settle from the files or the code. '
  'Everything else is already decided and needs nothing from Cipla.')

# ------------------------------------------------------------------ 2
H('2. Decisions needed before development starts', 1)
P('A wrong assumption on any of these means rework rather than a small change.',
  italic=True)
T(['#', 'Question', 'Why we are asking'], [
    ['Q1', 'Should a proprietorship become a person Business Partner or an '
     'organisation Business Partner?\n\n'
     'If a person: will the portal send first name and last name separately?',
     'Every record arrives on account group Z001 or Z002, and the Business Partner '
     'grouping - and with it the category - is derived from the account group. Two of '
     'the nine records are proprietorships whose PAN carries P in the fourth position '
     '(an individual) and whose legal name is a person. A person Business Partner '
     'needs a first and a last name, and the file gives one string: '
     '"KURUBARABUDIHAL CHANDRASHEKAR CHINMAY SHETTY" cannot be split safely by rule. '
     'If proprietorships are to be persons, they also need their own account group or '
     'an explicit override.'],
    ['Q2', 'For each account group used by the portal - Z001, Z002 and any other - '
     'which Business Partner grouping applies, is the number internal or external, '
     'and should the Business Partner and the vendor carry the same number?',
     'Today the program assigns the vendor number itself: it reads the highest '
     'existing vendor for the account group and adds one. Under Business Partner the '
     'number comes from the grouping, so that logic is removed. If the two numbers '
     'differ, the portal has to be told both - see Q3.'],
    ['Q3', 'May the response file carry the Business Partner number - as a new '
     'column, or appended to the message text?',
     'Unless the grouping is set for the same number, the Business Partner number and '
     'the vendor number are different, and the portal has no way to learn the '
     'Business Partner number other than from the response. Our recommendation is a '
     'new column, so that the message text stays free.'],
    ['Q4', 'Please provide the mapping from the portal’s State code to the SAP '
     'region.',
     'The portal’s codes are self-consistent - Gujarat 06, Karnataka 10, Madhya '
     'Pradesh 12, Maharashtra 13 - but they are not the codes carried in the GST '
     'numbers on the same records (24, 29, 23, 27), and two records fit neither '
     'pattern (Kolkata 25, Hyderabad 01). The value is written straight into the '
     'address region today, so it is worth confirming for the existing interface as '
     'well.'],
    ['Q5', 'Do the columns always appear in the same order, and is a column the '
     'portal does not send always absent from the end of the line rather than from '
     'the middle?',
     'The line is read by position, not by heading. A column dropped from the middle '
     'shifts every value after it, and the record is created with the wrong value in '
     'every remaining field - silently, with no error. The five files carry 124 '
     'columns while the program reads 134, so the last ten are already absent; that '
     'is handled, but only because they are at the end.'],
], [1.2, 6.2, 9.1])

# ------------------------------------------------------------------ 3
H('3. Field mapping to be confirmed', 1)
P('Cipla has asked that every column be mapped, whether or not the portal fills it. '
  'Of the 134 columns the program reads, 53 are posted and 81 are read and then '
  'discarded. Those 81 are the work. We will propose a target for the obvious ones; '
  'the groups below need a decision from Cipla.', )
T(['#', 'Columns', 'What we need to know'], [
    ['M1', 'The location block - country, state, city, pin_code, address_line1 to 3, '
     'location_email, location_phone, location_type, sez_type, together with '
     'is_header_address_same_in_location',
     'Is this a second address for the vendor? A Business Partner holds several '
     'addresses, which the vendor master could not, so this can now be stored '
     'properly. Please confirm what the block represents and when the flag applies.'],
    ['M2', 'Tan, Cin, RegistrationNumber, TaxCode, TaxNo, w8_w9',
     'Which SAP field each of these should be written to.'],
    ['M3', 'gr_based_iv, schema_code',
     'The program hard-codes GR based invoice verification as X, and derives the '
     'schema group from a set rather than from the file. Mapping these columns '
     'changes current behaviour - please confirm that is wanted.'],
    ['M4', 'bnkcountry',
     'The vendor’s own country is used as the bank country today. Mapping this '
     'column changes behaviour for foreign vendors.'],
    ['M5', 'bank_name, branch_address, bic, micr, iban',
     'Which bank master field each belongs to. Two points to settle with it: a '
     '24-character IBAN is being written into the 18-character bank account number '
     'and truncated, while the file’s own iban column is discarded; and bic '
     'holds a branch name in eight of the nine records, not a BIC.'],
    ['M6', 'contact_number, contact_purpose, Contactlocation_id, '
     'SupplierIntroducerName / Id / Email, CompanyWebsite, Phone',
     'Should these become a Business Partner contact person, or communication data '
     'on the address?'],
    ['M7', 'Status, is_agree, IsUpdateBySupplier, CreatedSource, SupplierInvitationId, '
     'and the GST and PAN verification flags',
     'These look like portal control fields. Should they be stored, and if so where, '
     'or ignored?'],
    ['M8', 'Address3',
     'Populated but not used today. We propose street supplement 2.'],
    ['M9', 'CountryName, company, payment_term, purchasing_organization, '
     'reco_account, industry_key, schema, currency, gst_classification, '
     'OrganizationTypeCode',
     'We assume these are text echoes of the code columns next to them and that '
     'nothing is stored. Please confirm.'],
], [1.2, 6.2, 9.1])

# ------------------------------------------------------------------ 4
H('4. Two smaller points', 1)
T(['#', 'Question', 'Why we are asking'], [
    ['Q6', 'What should the previous account number (ALTKN) carry?',
     'It is filled today with the number of the previously created vendor, a side '
     'effect of the self-assigned numbering that disappears under Business Partner. '
     'Nothing, or the portal id?'],
    ['Q7', 'One sample file each for vendor change, employee vendor creation, '
     'employee vendor change, vendor extension, and a record carrying withholding tax '
     'and exemption data.',
     'All nine records supplied are creations with no withholding tax, so those paths '
     'cannot be tested against a real file.'],
], [1.2, 6.2, 9.1])

# ------------------------------------------------------------------ 5
H('5. Objects, files and access we need', 1)
T(['#', 'What', 'Why'], [
    ['R1', 'ZGEN_UPDATE_X and ZPARAM_TABLE',
     'Used throughout. We have the calls but not the definitions.'],
    ['R2', 'The ZKRP_* structures used by the two function modules',
     'About forty structures carrying the interface parameters.'],
    ['R3', 'The contents of ZFI_VEN_GRP',
     'It decides which account groups assign their own vendor number today, which is '
     'the logic Q2 replaces.'],
    ['R4', 'ZFI_VEND_MSME and ZFI_TAX_TAB - the table definitions',
     'Both are written directly by the interface. We need the field types before we '
     'can map to them.'],
    ['R5', 'The screen enhancement on XK01 that maintains ZFI_TAX_TAB',
     'Needed for the vendor extension program, where the table is filled from the '
     'screen rather than from the file.'],
    ['R6', 'A development system and a transport',
     'CVI Customizing is already in place, so the grouping and the roles are read at '
     'runtime.'],
    ['R7', 'Development authorisation for vendor and Business Partner master data, '
     'and read access to the AL11 inbound and outbound directories',
     'To develop and to test against the real files.'],
], [1.2, 6.2, 9.1])

# ------------------------------------------------------------------ 6
H('6. Points already confirmed by Cipla', 1)
P('Recorded so that they are not asked again.', italic=True)
T(['Point', 'Confirmed'], [
    ['CVI Customizing', 'Already in place in the target system.'],
    ['Columns the portal may not fill',
     'Every column is to be mapped, whether or not it is filled.'],
    ['Vendor name', 'The name is carried across Business Partner name lines 1 to 4, '
     'so a name longer than one line is no longer cut.'],
    ['Bank keys', 'The bank file is always processed before the vendor file.'],
    ['VendorTurnOver', 'A turnover band, not an amount.'],
    ['MSME', 'The MSME code is written to the SSI status field by design.'],
    ['Employee vendors', 'Created as person Business Partners.'],
    ['Customer master', 'Not in scope.'],
    ['Callers of the function modules', 'These programs only.'],
    ['Reference vendor on an extension',
     'Read from the tables and passed to the class, using the same logic as the '
     'download program already built.'],
], [5.0, 11.5])
doc.add_paragraph()

import os
out = os.path.join(os.path.dirname(__file__), '..', '..', 'docs', 'cimmra',
                   'Cimmra_Vendor_BP_Open_Points_for_Cipla.docx')
doc.save(os.path.abspath(out))
print('written', os.path.abspath(out))
