// Builds docs/cipla/Cipla_Master_Data_Upload_Programs_Guide.docx - the customer-facing
// overview and test guide for ZSDS_CUST_TMPL_DOWNLOAD and ZMMS_BP_MASS_UPLOAD.
// Needs the docx npm package.
// Usage: node tools/doc-generators/make_programs_guide.js <output.docx>
const fs = require('fs');
const {
  Document, Packer, Paragraph, TextRun, Table, TableRow, TableCell, AlignmentType,
  HeadingLevel, WidthType, ShadingType, BorderStyle, LevelFormat, PageBreak,
  Header, Footer, PageNumber, TableOfContents,
} = require('docx');

const FONT = 'Arial';
const ACCENT = '1F4E79';
const HEAD_FILL = 'DCE6F1';
const NOTE_FILL = 'FFF4E5';
const W = 9026; // A4 text width with 1" margins, DXA

// ---------------------------------------------------------------- helpers
const run = (t, o = {}) => new TextRun({ text: t, font: FONT, ...o });
function runs(text) {
  // **bold** segments
  return text.split(/(\*\*[^*]+\*\*)/).filter(Boolean).map(s =>
    s.startsWith('**') ? run(s.slice(2, -2), { bold: true }) : run(s));
}
const P = (t, o = {}) => new Paragraph({ children: runs(t), spacing: { after: 120 }, ...o });
const H1 = t => new Paragraph({ heading: HeadingLevel.HEADING_1, children: [run(t)] });
const H2 = t => new Paragraph({ heading: HeadingLevel.HEADING_2, children: [run(t)] });
const H3 = t => new Paragraph({ heading: HeadingLevel.HEADING_3, children: [run(t)] });
const B = t => new Paragraph({ numbering: { reference: 'bul', level: 0 }, children: runs(t), spacing: { after: 60 } });
let numRef = 0;
function steps(list) {
  const ref = `num${numRef++}`;
  numberingConfigs.push({ reference: ref, levels: [{ level: 0, format: LevelFormat.DECIMAL, text: '%1.',
    alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 540, hanging: 360 } } } }] });
  return list.map(t => new Paragraph({ numbering: { reference: ref, level: 0 }, children: runs(t), spacing: { after: 60 } }));
}
const PB = () => new Paragraph({ children: [new PageBreak()] });

const border = { style: BorderStyle.SINGLE, size: 4, color: 'A6A6A6' };
const borders = { top: border, bottom: border, left: border, right: border };
function cell(text, width, o = {}) {
  const paras = String(text).split('\n').map(line =>
    new Paragraph({ children: o.head ? [run(line, { bold: true, color: ACCENT })] : runs(line),
                    spacing: { after: 40 } }));
  return new TableCell({
    borders, width: { size: width, type: WidthType.DXA },
    shading: o.fill ? { fill: o.fill, type: ShadingType.CLEAR, color: 'auto' } : undefined,
    margins: { top: 60, bottom: 60, left: 100, right: 100 },
    children: paras,
  });
}
function T(head, rows, pct) {
  const widths = pct.map(p => Math.round(W * p / 100));
  widths[widths.length - 1] = W - widths.slice(0, -1).reduce((a, b) => a + b, 0);
  return new Table({
    width: { size: W, type: WidthType.DXA }, columnWidths: widths,
    rows: [
      new TableRow({ tableHeader: true, children: head.map((h, i) => cell(h, widths[i], { head: true, fill: HEAD_FILL })) }),
      ...rows.map(r => new TableRow({ children: r.map((c, i) => cell(c, widths[i])) })),
    ],
  });
}
function NOTE(text) {
  return new Table({
    width: { size: W, type: WidthType.DXA }, columnWidths: [W],
    rows: [new TableRow({ children: [cell(text, W, { fill: NOTE_FILL })] })],
  });
}
const GAP = () => new Paragraph({ children: [], spacing: { after: 60 } });

const numberingConfigs = [
  { reference: 'bul', levels: [{ level: 0, format: LevelFormat.BULLET, text: '•', alignment: AlignmentType.LEFT,
    style: { paragraph: { indent: { left: 540, hanging: 360 } } } }] },
];

// ---------------------------------------------------------------- content
const body = [];
const add = (...x) => body.push(...x);

// Title page
add(
  new Paragraph({ spacing: { before: 2400, after: 240 }, children: [run('Customer and Vendor Master', { size: 44, bold: true, color: ACCENT })] }),
  new Paragraph({ spacing: { after: 480 }, children: [run('Download / Upload Programs - Overview and Test Guide', { size: 32, color: ACCENT })] }),
  T(['Item', 'Detail'], [
    ['Programs', 'ZSDS_CUST_TMPL_DOWNLOAD - Customer master download / upload\nZMMS_BP_MASS_UPLOAD - Vendor (Business Partner) master download / upload'],
    ['System', 'SAP S/4HANA - test client'],
    ['Purpose', 'What each program does, and how the business team can check it before go-live'],
    ['Version / date', '1.1 - 30 September 2026'],
  ], [25, 75]),
  PB(),
  new Paragraph({ children: [run('Contents', { bold: true, size: 28, color: ACCENT })], spacing: { after: 120 } }),
  new TableOfContents('Contents', { hyperlink: true, headingStyleRange: '1-2' }),
  PB(),
);

// 1. Overview
add(
  H1('1. The two programs at a glance'),
  P('Your existing LSMW templates were recorded against transactions XD01, XK01, XK02 and XK05. In SAP S/4HANA these transactions are replaced by the Business Partner (transaction BP), so the recordings can no longer run. The two programs below replace them - one for customers and one for vendors, each of which both downloads and uploads. They create and change master data through the standard SAP Business Partner interface, so every check, number range and authorisation that applies in transaction BP applies here too.'),
  T(['Program', 'What it does', 'Used for'], [
    ['ZSDS_CUST_TMPL_DOWNLOAD', 'Downloads the right customer template for a country / region and account group - empty or filled with existing customers - and uploads a filled template back into SAP.', 'Customer master: create, extend to a new sales area, block and unblock'],
    ['ZMMS_BP_MASS_UPLOAD', 'Downloads a tab of the vendor workbook - empty or filled with existing vendors - and uploads a filled tab back into SAP, one scenario (tab) at a time.', 'Vendor master: create, TDS, TAN exemption, bank keys, bank details, extension, CIN, partner functions, block / unblock'],
  ], [26, 44, 30]),
  GAP(),
  NOTE('Two earlier programs are no longer used. ZSDS_CUST_MASS_UPLOAD (customer upload) is replaced by ZSDS_CUST_TMPL_DOWNLOAD; its layouts belonged to the old LSMW workbook and did not match the current customer templates. ZBCS_MASS_UPLOAD_EXTRACT (vendor sample file) is now the download mode of ZMMS_BP_MASS_UPLOAD.'),
  P('In both programs a download and an upload use one and the same column definition, so a file the program downloads is always a file it can upload.'),
);

// 2. Rules common to all
add(
  H1('2. Rules that apply to both programs'),
  H2('2.1 Always do a test run first'),
  P('The upload of both programs has a **Test run** checkbox, ticked by default. In a test run the program carries out every check it would make for real, but nothing is saved. The result list shows exactly what would have happened, row by row. Untick Test run only once a file comes back without errors.'),
  H2('2.2 Use .xlsx files'),
  P('Files must be Excel Workbooks (.xlsx). An older .xls file cannot be read - open it in Excel and use Save As, Excel Workbook (*.xlsx).'),
  H2('2.3 The file is read by its headings'),
  B('The program finds the heading row itself, even if there is a title line above it. The tab can have any name.'),
  B('Columns may be moved or removed; each is read from wherever its heading is. A heading can be the template wording ("Company Code") or the technical name ("BUKRS").'),
  B('Please do not rename headings. A column whose heading is not recognised is read from its template position, and left empty if that position clearly belongs to another field - a wrong value is worse than none. The result list says which columns this affected.'),
  B('Delete, do not hide, any rows between the heading row and the data (field length, mandatory / optional, guidance or sample rows). Hidden rows are still read.'),
  H2('2.4 An empty cell changes nothing'),
  P('When changing an existing record, a blank cell leaves the value already in SAP as it is, so a file only needs the columns you want to change. To clear a field on purpose, type **#BLANK#** in the cell.'),
  H2('2.5 Reading the result list'),
  T(['Column / signal', 'Meaning'], [
    ['Green light', 'The row was posted (or, in a test run, would be posted)'],
    ['Yellow light', 'A warning - the row went through, but read the message'],
    ['Red light', 'An error - nothing was posted for this row'],
    ['Excel row', 'The row number in your file, so you can go straight to it'],
    ['Customer / Vendor', 'The account the line is about. For a new record, the new number appears here.'],
    ['Field', 'Which field caused the message, where SAP reports it'],
    ['Summary line', 'At the bottom of the screen: rows posted, rows with errors and rows skipped'],
  ], [28, 72]),
  H2('2.6 Authorisations'),
  P('The programs run under your own user. You need the same authorisations as for creating and changing business partners in transaction BP.'),
  PB(),
);

// 3. Customer program
add(
  H1('3. ZSDS_CUST_TMPL_DOWNLOAD - Customer master template download / upload'),
  H2('3.1 What it does'),
  P('The customer workbook holds 24 different template layouts, used across 79 combinations of country / region and account group. This program knows which layout belongs to which combination, so the user never has to pick the layout.'),
  B('**Download** - choose the country / region and the account group, and the program writes the matching template. It can be filled with existing customers (to see what the template looks like with real data, or to use as a starting point) or left empty.'),
  B('**Upload** - the same file, filled in, is read back into SAP. Because download and upload use one and the same column definition, a file downloaded by the program is always a file it can upload.'),
  P('Three kinds of template are available:'),
  T(['Template', 'What an upload does'], [
    ['Customer create', 'Creates new customers with general data, address, company code, sales area, tax classification and licence data. A row whose customer number already exists changes that customer instead.'],
    ['Customer extension', 'Adds a sales area (and its data) to an existing customer. The company code in the row must be one the customer already has.'],
    ['Block / unblock', 'Sets or removes the central, company code and sales area blocks of existing customers.'],
  ], [26, 74]),
  H2('3.2 Selection screen'),
  T(['Field', 'What to enter'], [
    ['Download a template / Upload a filled template', 'Choose the direction. The screen shows only the fields that apply.'],
    ['Customer create / extension / block-unblock template', 'Which template.'],
    ['Country / region', 'Dropdown list - see 3.3. (Create template only.)'],
    ['Customer account group', 'F4 shows only the account groups used for the chosen country / region.'],
    ['Business partner / Customer', 'Download only: which existing customers to fill the template with. Leave empty together with "Template only" for an empty template.'],
    ['Rows at most', 'Download only: limit on the number of customers written (default 100).'],
    ['Test run - post nothing', 'Upload only. Ticked by default - see 2.1.'],
    ['Stop at first faulty row', 'Upload only. Stops the run at the first error instead of working through the whole file.'],
    ['BP grouping (override)', 'Upload only. Leave empty - the grouping is taken from the account group. Fill only to force a particular grouping.'],
    ['Heading rows to skip', 'Upload only. Leave at 1. Used only when no heading row can be recognised at all.'],
    ['File', 'Download: where to save. Upload: the file to read. F4 opens the file dialog.'],
    ['On the PC / On the application server', 'Where the file is. Use PC for normal work.'],
    ['Template only (no data)', 'Download only: write the headings without any customer data.'],
  ], [34, 66]),
  H2('3.3 Countries / regions and account groups'),
  T(['Country / region', 'Account groups covered'], [
    ['Australia (AU)', 'ZCDP, ZDOM, ZEXP, ZPLN, ZSHP'],
    ['Dubai (AE)', 'ZEXP, ZSHP'],
    ['Europe (GB/BE/ES/NL)', 'YSHP, ZCDP, ZDOM, ZEXP, ZPLN, ZSHP'],
    ['Exelan (US)', 'YSHP, YVMI, YVSP, YVTO, ZCDP, ZPLN'],
    ['India (IN)', 'ZBMR, ZCDP, ZDOC, ZDOD, ZDOF, ZDOM, ZEXP, ZMPC, ZNOT, ZOTC, ZPLN, ZPY1, ZREM, ZSHM, ZSHP, ZSUB'],
    ['Invagen (US)', 'YVSP, ZCDP, ZDOM, ZEXP, ZPLN, ZSHP'],
    ['Kenya (KE)', 'YDOM, YVTO, ZEXP'],
    ['Morocco (MA)', 'ZCDP, ZDOM, ZOTC, ZPLN'],
    ['QCIL (UG)', 'YSHP, ZCDP, ZDOM, ZEXP, ZOTC, ZSHP, ZSUB'],
    ['SAGA (ZA)', 'YDOM, YINT, YTDR, ZDOM, ZEXP, ZPLN'],
  ], [30, 70]),
  P('Region rather than country is used because the two do not line up one to one: Europe is four countries on one template, and the United States is two entities (Exelan and Invagen) with account groups in common.'),
  H2('3.4 How to check the download'),
  ...steps([
    'Choose **Download a template**, **Customer create template**, a country / region and an account group.',
    'Enter two or three existing customers of that account group, or tick **Template only**.',
    'Choose a file name (the program proposes one) and execute.',
    'Open the file in Excel. **Expected:** one tab, headed exactly as the Cipla template for that combination; one row per customer; dates as DD.MM.YYYY and customer numbers without leading zeros.',
    'Compare two or three values with transaction BP for the same customer.',
    'Repeat for the **Customer extension** and **Block / unblock** templates.',
  ]),
  H2('3.5 How to check the upload'),
  ...steps([
    '**Round trip.** Download one existing customer, change nothing, and upload the file with **Test run** ticked. Expected: a green line for the customer, no errors. This proves the file and the program agree.',
    '**Change one field.** In the same file change one value (for example the search term), untick Test run and upload. Expected: green line; in BP only that field has changed.',
    '**Create.** Fill one row of a create template for a new customer, leaving the customer number empty. Test run first, then for real. Expected: green line showing the new customer number; the customer exists in BP with its company code and sales area data.',
    '**Extension.** In the extension template, give an existing customer and a sales area it does not yet have. Expected: the new sales area appears in BP (role FLCU01 - Customer, Sales area data).',
    '**Block / unblock.** In the block template, fill the block you want to set - X for a posting block, the block reason code for an order, delivery or billing block. To remove a block, put #BLANK# in that column. Expected: the block shows or disappears in BP (Status tab, and the company code / sales area data).',
    '**Error handling.** Put an account group that does not exist in one row. Expected: red line "Account group ... does not exist"; the other rows are processed normally.',
  ]),
  H2('3.6 Points to know'),
  B('**Contact persons** are not loaded. In S/4HANA a contact person is a business partner of its own. If contact columns are filled, the result list shows a warning; please maintain contact persons in transaction BP.'),
  B('**Credit limits** are not part of this program. None of the current templates carries credit data.'),
  B('**Licence data** (drug licences, bank guarantee, routing) is held once per customer. If a file carries it more than once for the same customer, the first one is used.'),
  B('**Tax classification** columns are matched in order: the first "Tax classification for customer" column in the file is the first in the template. If one of them is deleted, the program cannot tell which is which and leaves them empty, with a warning.'),
  B('On the **extension template**, "Terms of Payment Key" is the payment term of the sales area being added.'),
  PB(),
);

// 4. Vendor download / upload
add(
  H1('4. ZMMS_BP_MASS_UPLOAD - Vendor master download / upload'),
  H2('4.1 What it does'),
  P('One program for every vendor mass change, in both directions. A radio button selects the scenario - the tab of your vendor workbook - and the same layout is used to download and to upload it, so no re-keying is needed.'),
  B('**Download** - reads existing vendors and writes them into the scenario’s tab: a correctly laid-out file to start from, filled with real values, or with headings only. It changes nothing in SAP.'),
  B('**Upload** - reads a filled tab and posts it.'),
  T(['Scenario (radio button)', 'Workbook tab', 'What an upload does'], [
    ['Vendor / BP creation - all CC', 'Vendor creation for All CC', 'Creates the vendor and its business partner, with company code and purchasing data'],
    ['Withholding tax / TDS', 'TDS upload', 'Maintains withholding tax types and codes per company code'],
    ['TAN exemption details', 'TAN details', 'Maintains India TAN exemption records'],
    ['Bank key creation', 'BANK Key creation', 'Creates or changes bank master records'],
    ['Vendor bank details', 'Bank details update', "Maintains the vendor's bank accounts"],
    ['Vendor extension', 'Vendor extension', 'Extends an existing vendor to another company code or purchasing organisation'],
    ['CIN details', 'CIN details', 'Maintains the India tax (CIN) fields'],
    ['Partner functions', 'Patner function', 'Maintains purchasing partner functions'],
    ['Block / unblock', 'Block_Unblocked', 'Sets or clears posting and purchasing blocks'],
  ], [32, 24, 44]),
  H2('4.2 Selection screen'),
  T(['Field', 'What to enter'], [
    ['Download a workbook / Upload a filled workbook', 'Choose the direction. The screen shows only the fields that apply.'],
    ['Scenario', 'One of the nine radio buttons above'],
    ['Business partner / Supplier', 'Download only: which existing vendors to write'],
    ['Rows at most', 'Download only: limit on the number of rows written (default 20)'],
    ['Test run (nothing is posted)', 'Upload only. Ticked by default - see 2.1'],
    ['Stop at the first faulty row', 'Upload only. Stops at the first error instead of working through the whole file'],
    ['Heading rows to skip', 'Upload only. Leave at 1'],
    ['File', 'Download: where to save (a name is proposed per scenario). Upload: the file to read. F4 opens the file dialog.'],
    ['On the PC / On the application server', 'Where the file is. Use PC for normal work.'],
    ['Headings only (empty template)', 'Download only: write the headings without data'],
  ], [34, 66]),
  H2('4.3 Order of loading'),
  P('Some scenarios need data another scenario creates. Please load in this order:'),
  ...steps([
    'Bank key creation - the bank must exist before a vendor bank account can use it.',
    'Vendor / BP creation.',
    'Vendor extension - before TDS or partner functions for a new company code or purchasing organisation.',
    'Withholding tax / TDS, TAN exemption, vendor bank details, CIN details, partner functions.',
    'Block / unblock - whenever needed.',
  ]),
  NOTE('If a scenario needs data that is not there yet - for example TDS for a company code the vendor has not been extended to - the row is refused with a message that says so, rather than posted halfway.'),
  H2('4.4 How to check the download'),
  ...steps([
    'Choose **Download a workbook**, a scenario, and two or three existing vendors. Execute.',
    'Open the file. **Expected:** the headings of the vendor workbook tab for that scenario, one row per vendor (or per company code, purchasing organisation, bank account, withholding tax type or partner function, depending on the scenario). Compare a few values with transaction BP.',
    'Tick **Headings only** and execute. **Expected:** a file with the headings and no data.',
  ]),
  H2('4.5 How to check the upload'),
  ...steps([
    '**Round trip.** Upload the file you just downloaded, same scenario, with **Test run** ticked. **Expected:** green lines, no errors. This proves the file and the program agree.',
    'Fill a tab with the rows you want to load, select it and execute with **Test run** ticked. **Expected:** one line per row; green where the row would post; red with a reason where it would not.',
    'Correct the file for any red lines and repeat until the test run is clean.',
    'Untick Test run and execute. **Expected:** the same lines, now saying the row was posted; for a new vendor, the new vendor number.',
    'Check the result in SAP using the table in 4.6.',
  ]),
  H2('4.6 Where to see the result in SAP'),
  T(['Scenario', 'Where to check'], [
    ['Vendor / BP creation', 'BP - General data, role FLVN00 (company code data) and FLVN01 (purchasing data)'],
    ['Withholding tax / TDS', 'BP, role FLVN00 - company code, Vendor: Withholding Tax'],
    ['TAN exemption details', 'SE16N, table FIWTIN_TAN_EXEM'],
    ['Bank key creation', 'FI03 - Display bank'],
    ['Vendor bank details', 'BP - Payment Transactions tab'],
    ['Vendor extension', 'BP - the new company code (FLVN00) or purchasing organisation (FLVN01)'],
    ['CIN details', 'BP, role FLVN00 - India tax / CIN data'],
    ['Partner functions', 'BP, role FLVN01 - Purchasing, Partner Functions'],
    ['Block / unblock', 'BP - Status tab, and the company code / purchasing blocks'],
  ], [30, 70]),
  PB(),
);

// 5. Sign-off sheet
const TEST = (n, prog, test, exp) => [n, prog, test, exp, '', ''];
add(
  H1('5. Test sign-off sheet'),
  P('Please record the result of each test and return this sheet with any screenshots of red lines.'),
  T(['#', 'Program', 'Test', 'Expected result', 'OK / Not OK', 'Remarks'], [
    TEST('1', 'Customer', 'Download create template, 2-3 customers', 'Correct template for the region / group, values match BP'),
    TEST('2', 'Customer', 'Download empty template', 'Headings only'),
    TEST('3', 'Customer', 'Download extension and block templates', 'Correct headings and data'),
    TEST('4', 'Customer', 'Upload downloaded file unchanged, test run', 'Green, no errors'),
    TEST('5', 'Customer', 'Change one field, live', 'Only that field changed in BP'),
    TEST('6', 'Customer', 'Create a new customer', 'New number shown; customer in BP'),
    TEST('7', 'Customer', 'Extend to a new sales area', 'Sales area visible in BP'),
    TEST('8', 'Customer', 'Block, then unblock (#BLANK#)', 'Block set, then removed'),
    TEST('9', 'Customer', 'Wrong account group', 'Red line, other rows processed'),
    TEST('10', 'Vendor', 'Download 2-3 vendors, each scenario', 'Values match BP'),
    TEST('11', 'Vendor', 'Upload downloaded file unchanged, test run', 'Green, no errors'),
    TEST('12', 'Vendor', 'Bank key creation', 'Bank visible in FI03'),
    TEST('13', 'Vendor', 'Vendor creation, test run then live', 'New vendor number; vendor in BP'),
    TEST('14', 'Vendor', 'Vendor extension', 'New company code / purch. org in BP'),
    TEST('15', 'Vendor', 'Withholding tax / TDS', 'Tax types in BP company code data'),
    TEST('16', 'Vendor', 'TAN exemption', 'Records in FIWTIN_TAN_EXEM'),
    TEST('17', 'Vendor', 'Vendor bank details', 'Account in BP Payment Transactions'),
    TEST('18', 'Vendor', 'CIN details', 'CIN fields in BP'),
    TEST('19', 'Vendor', 'Partner functions', 'Partners in BP purchasing data'),
    TEST('20', 'Vendor', 'Block / unblock', 'Blocks set / cleared in BP'),
  ], [5, 11, 28, 28, 12, 16]),
  GAP(),
  T(['Tested by', 'Date', 'Signature'], [['', '', '']], [40, 25, 35]),
);

// ---------------------------------------------------------------- document
const doc = new Document({
  creator: 'SAP project team',
  title: 'Customer and Vendor Master - Download / Upload Programs',
  styles: {
    default: { document: { run: { font: FONT, size: 20 } } },
    paragraphStyles: [
      { id: 'Heading1', name: 'Heading 1', basedOn: 'Normal', next: 'Normal', quickFormat: true,
        run: { size: 30, bold: true, font: FONT, color: ACCENT },
        paragraph: { spacing: { before: 240, after: 160 }, outlineLevel: 0 } },
      { id: 'Heading2', name: 'Heading 2', basedOn: 'Normal', next: 'Normal', quickFormat: true,
        run: { size: 24, bold: true, font: FONT, color: ACCENT },
        paragraph: { spacing: { before: 200, after: 100 }, outlineLevel: 1 } },
      { id: 'Heading3', name: 'Heading 3', basedOn: 'Normal', next: 'Normal', quickFormat: true,
        run: { size: 21, bold: true, font: FONT },
        paragraph: { spacing: { before: 160, after: 80 }, outlineLevel: 2 } },
    ],
  },
  numbering: { config: numberingConfigs },
  features: { updateFields: true },
  sections: [{
    properties: { page: { size: { width: 11906, height: 16838 },
                          margin: { top: 1440, right: 1440, bottom: 1440, left: 1440 } } },
    headers: { default: new Header({ children: [new Paragraph({ alignment: AlignmentType.RIGHT,
      children: [run('Customer and Vendor Master - Download / Upload Programs', { size: 16, color: '808080' })] })] }) },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER,
      children: [run('Page ', { size: 16, color: '808080' }),
                 new TextRun({ children: [PageNumber.CURRENT], font: FONT, size: 16, color: '808080' })] })] }) },
    children: body,
  }],
});

Packer.toBuffer(doc).then(buf => {
  fs.writeFileSync(process.argv[2], buf);
  console.log('written', process.argv[2]);
});
