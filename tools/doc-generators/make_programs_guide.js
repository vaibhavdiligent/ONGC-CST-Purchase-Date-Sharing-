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
  new Paragraph({ spacing: { after: 480 }, children: [run('Download / Upload Programs - User and Test Guide', { size: 32, color: ACCENT })] }),
  T(['Item', 'Detail'], [
    ['Programs', 'ZSDS_CUST_TMPL_DOWNLOAD - Customer master download / upload\nZMMS_BP_MASS_UPLOAD - Vendor (Business Partner) master download / upload'],
    ['System', 'SAP S/4HANA - test client'],
    ['Purpose', 'What each program does, how to use it, and the tests to run before go-live'],
    ['Version / date', '1.2 - 30 September 2026'],
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
    ['Summary', 'Vendor program: at the top of the list - test run or productive run, rows OK, rows with errors, rows skipped. Customer program: in the status bar - rows read, processed, with errors, skipped.'],
  ], [28, 72]),
  P('A download shows a shorter list: one green line per row written, and a red line for any record that could not be read. The status bar says how many columns and rows went into the file.'),
  H2('2.6 Switching between Download and Upload'),
  P('Both programs propose a file name for a download. When you switch to Upload that name is cleared, so the program cannot read the file it has just written by mistake - pick the file to upload with F4.'),
  H2('2.7 Authorisations'),
  P('The programs run under your own user. You need the same authorisations as for creating and changing business partners in transaction BP.'),
  PB(),
);

// 3. Customer program
add(
  H1('3. ZSDS_CUST_TMPL_DOWNLOAD - Customer master download / upload'),
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
    ['Download a template / Upload a filled template', 'Choose the direction. The fields that do not apply are greyed out.'],
    ['Customer create / extension / block-unblock template', 'Which template.'],
    ['Country / region', 'Dropdown list - see 3.3. Needed for the create template only; greyed out for the other two.'],
    ['Customer account group', 'F4 lists only the account groups of the chosen country / region. Needed for the create template only.'],
    ['Business partner / Customer', 'Download only: which existing customers to write. Either number can be given.'],
    ['Rows at most', 'Download only: limit on the number of rows written (default 100).'],
    ['Test run - post nothing', 'Upload only. Ticked by default - see 2.1.'],
    ['Stop at first faulty row', 'Upload only. Stops the run at the first error instead of working through the whole file.'],
    ['BP grouping (override)', 'Upload only. Leave empty - the grouping is taken from the account group.'],
    ['Heading rows to skip', 'Upload only. Leave at 1. Used only when no heading row can be recognised at all.'],
    ['File', 'Download: where to save - a name is proposed. Upload: the file to read. F4 opens the file dialog.'],
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
  H2('3.4 How to download a template'),
  ...steps([
    'Start ZSDS_CUST_TMPL_DOWNLOAD (transaction SE38, or its transaction code once assigned).',
    'Choose **Download a template**.',
    'Choose the template: **Customer create**, **Customer extension** or **Block / unblock**.',
    'For the create template: pick the **Country / region** from the list, then press F4 on **Customer account group** and pick one.',
    'Enter the customers to include under **Business partner** or **Customer**, or tick **Template only (no data)** for an empty template.',
    'Check the **File** name. A name is proposed from the template; press F4 to choose another folder.',
    'Execute (F8). The status bar says how many columns and rows were written, and a short list shows each row.',
    'Open the file in Excel.',
  ]),
  H2('3.5 How to upload a filled template'),
  ...steps([
    'Start ZSDS_CUST_TMPL_DOWNLOAD and choose **Upload a filled template**. The file name proposed for a download is cleared.',
    'Choose the same template as the file - and for the create template, the same country / region and account group.',
    'Press F4 on **File** and pick the filled .xlsx workbook.',
    'Leave **Test run - post nothing** ticked. Leave **BP grouping** empty and **Heading rows to skip** at 1.',
    'Execute (F8). Read the result list (see 2.5). Correct the file for every red line and run again.',
    'When the test run has no red lines, untick **Test run** and execute again to post. New customers show their new number in the list.',
  ]),
  H2('3.6 Points to know'),
  B('**Contact persons** are not loaded. In S/4HANA a contact person is a business partner of its own. If contact columns are filled, the result list shows a warning; please maintain contact persons in transaction BP.'),
  B('**Credit limits** are not part of this program. None of the current templates carries credit data.'),
  B('**Licence data** (drug licences, bank guarantee, routing) is held once per customer. If a file carries it more than once for the same customer, the first one is used.'),
  B('**Tax classification** columns are matched in order: the first "Tax classification for customer" column in the file is the first in the template. If one of them is deleted, the program cannot tell which is which and leaves them empty, with a warning.'),
  B('On the **extension template**, "Terms of Payment Key" is the payment term of the sales area being added.'),
  B('On the **block / unblock template**, fill X for a posting block, or the block reason code for an order, delivery or billing block. To remove a block, type #BLANK# in that column.'),
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
    ['Download a workbook / Upload a filled workbook', 'Choose the direction. The fields that do not apply are greyed out.'],
    ['Scenario', 'One of the nine radio buttons above'],
    ['Business partner / Supplier', 'Download only: which existing vendors to write. Either number can be given.'],
    ['Rows at most', 'Download only: limit on the number of rows written (default 20)'],
    ['Test run (nothing is posted)', 'Upload only. Ticked by default - see 2.1'],
    ['Stop at the first faulty row', 'Upload only. Stops at the first error instead of working through the whole file'],
    ['Heading rows to skip', 'Upload only. Leave at 1'],
    ['File', 'Download: where to save - a name is proposed per scenario. Upload: the file to read. F4 opens the file dialog.'],
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
  H2('4.4 How to download a workbook'),
  ...steps([
    'Start ZMMS_BP_MASS_UPLOAD (transaction SE38, or ZMMS_BPUPL once assigned).',
    'Choose **Download a workbook** and the scenario.',
    'Enter the vendors under **Business partner** or **Supplier**, or tick **Headings only (empty template)**.',
    'Check the **File** name. A name is proposed from the scenario; press F4 to choose another folder.',
    'Execute (F8). The status bar says how many columns and rows were written, and a short list shows each row.',
    'Open the file in Excel.',
  ]),
  H2('4.5 How to upload a filled workbook'),
  ...steps([
    'Start ZMMS_BP_MASS_UPLOAD and choose **Upload a filled workbook**. The file name proposed for a download is cleared.',
    'Choose the scenario that matches the tab you are loading.',
    'Press F4 on **File** and pick the filled .xlsx workbook.',
    'Leave **Test run (nothing is posted)** ticked and **Heading rows to skip** at 1.',
    'Execute (F8). Read the result list (see 2.5). Correct the file for every red line and run again.',
    'When the test run has no red lines, untick **Test run** and execute again to post. A new vendor shows its new number in the list.',
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

// 5. Test script
const TS = (id, what, exp) => [id, what, exp, '', ''];
const TSW = [8, 40, 34, 8, 10];
const TSH = ['#', 'What to do', 'Expected result', 'OK / Not OK', 'Remarks'];
add(
  H1('5. Test script'),
  P('Please work through the tests in order, fill in the last two columns and return the document with a screenshot of anything marked Not OK. Tests marked **(retest)** repeat an issue found in earlier testing that has since been corrected.'),
  H2('5.1 Customer - ZSDS_CUST_TMPL_DOWNLOAD'),
  T(TSH, [
    TS('C-01', 'Start the program.', 'Download / Upload choice at the top, three template buttons, a Country / region dropdown with 10 entries.'),
    TS('C-02', 'Create template: pick a region, then F4 on Customer account group and pick one. (retest)', 'Only that region’s account groups are listed; the chosen one comes back into the field. No short dump.'),
    TS('C-03', 'Choose the extension, then the block / unblock template.', 'Country / region and account group are greyed out.'),
    TS('C-04', 'Download the create template for 2-3 existing customers.', 'File opens in Excel; headings as the Cipla template for that combination; one row per customer; dates DD.MM.YYYY; values match BP.'),
    TS('C-05', 'Download with Template only ticked.', 'Headings, no data.'),
    TS('C-06', 'Download the extension and the block / unblock template.', 'Correct headings; data matches BP.'),
    TS('C-07', 'Switch to Upload.', 'The proposed download file name is cleared; the upload options can be filled; the download fields are greyed out.'),
    TS('C-08', 'Upload an .xls file (old Excel format).', 'Message that only .xlsx can be uploaded. Nothing else happens.'),
    TS('C-09', 'Upload a file kept in a long folder path (for example OneDrive). (retest)', 'The file is read; no ".xlsx not supported" message.'),
    TS('C-10', 'Round trip: upload the file from C-04 unchanged, Test run ticked.', 'Green lines, no errors.'),
    TS('C-11', 'Change one field (for example the search term), untick Test run, upload.', 'Green line; in BP only that field changed.'),
    TS('C-12', 'Create one new customer with a telephone and a mobile number. Test run, then live. (retest)', 'New customer number shown; in BP the mobile number is flagged as mobile. No short dump.'),
    TS('C-13', 'In a create row, give language ES (or another two-letter code). (retest)', 'BP shows that language (Spanish), not English.'),
    TS('C-14', 'Extension: an existing customer and a sales area it does not have.', 'The sales area appears in BP.'),
    TS('C-15', 'Block / unblock: set an order block, then remove it with #BLANK#.', 'Block shown in BP, then removed.'),
    TS('C-16', 'One row with an account group that does not exist.', 'Red line naming the account group; the other rows are processed.'),
    TS('C-17', 'Two faulty rows with Stop at first faulty row ticked.', 'The run stops after the first and says so.'),
  ], TSW),
  H2('5.2 Vendor - ZMMS_BP_MASS_UPLOAD'),
  T(TSH, [
    TS('V-01', 'Start the program. Click through the scenario buttons.', 'Download / Upload choice at the top, nine scenarios; the proposed file name changes with the scenario; upload options greyed out.'),
    TS('V-02', 'Download each scenario for 2-3 existing vendors.', 'Headings as the vendor workbook tab; values match BP.'),
    TS('V-03', 'Download with Headings only ticked.', 'Headings, no data.'),
    TS('V-04', 'Switch to Upload; press F4 on File.', 'Proposed download name cleared; the file dialog opens; download fields greyed out.'),
    TS('V-05', 'Upload an .xls file (old Excel format).', 'Message that the program reads .xlsx only.'),
    TS('V-06', 'Round trip: upload each file from V-02 unchanged, same scenario, Test run ticked.', 'Green lines, no errors.'),
    TS('V-07', 'Bank key creation, live.', 'Bank visible in FI03.'),
    TS('V-08', 'Vendor creation with a telephone and a mobile number: test run, then live. (retest)', 'New vendor number shown; mobile number flagged as mobile in BP. No short dump.'),
    TS('V-09', 'Vendor extension to a new company code / purchasing organisation.', 'Visible in BP.'),
    TS('V-10', 'TDS for the extended company code; then TDS for a company code the vendor does not have. (retest)', 'First: tax types in BP. Second: red line saying the vendor is not extended to that company code.'),
    TS('V-11', 'TAN exemption. (retest)', 'A green line per row saying it was saved; records in SE16N FIWTIN_TAN_EXEM.'),
    TS('V-12', 'Vendor bank details.', 'Account in BP Payment Transactions; existing accounts kept.'),
    TS('V-13', 'CIN details.', 'Fields visible in BP.'),
    TS('V-14', 'Partner functions, including one row whose partner vendor is not extended to that purchasing organisation. (retest)', 'Green only where the partner was added; for the other row one red line saying the partner is not extended to that purchasing organisation.'),
    TS('V-15', 'Block, then unblock.', 'Blocks set, then cleared, in BP.'),
    TS('V-16', 'A TDS file whose vendor column is empty. (retest)', 'A message naming the tab, the number of rows and the empty column - not "No data rows were found".'),
  ], TSW),
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
