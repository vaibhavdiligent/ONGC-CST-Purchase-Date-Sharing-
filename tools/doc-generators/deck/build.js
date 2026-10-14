// Builds docs/Customer_Vendor_Master_S4HANA_Reference.pptx - an anonymised reference deck
// (requirement and solution) for sharing with other customers. Images first:
//   python3 make_images.py <img dir>; then NODE_PATH=<node_modules> node build.js <img dir> <out.pptx>
// Reference deck: customer and vendor master download / upload on S/4HANA.
// usage: node build.js <img dir> <out.pptx>
const path = require('path');
const SP = path.resolve(__dirname, '..');
const pptxgen = require(path.join(SP, 'node_modules/pptxgenjs'));
const React = require(path.join(SP, 'node_modules/react'));
const RDS = require(path.join(SP, 'node_modules/react-dom/server'));
const sharp = require(path.join(SP, 'node_modules/sharp'));
const md = require(path.join(SP, 'node_modules/react-icons/md'));
const { applyTheme } = require('/root/.claude/skills/synced/e7a55bf8-c67e-4912-9f30-ca1be557e56b_b9dbfd15-b60e-4630-ab2d-bb3aa86c1921/pptx/scripts/apply_theme.js');
const sizeOf = (p) => sharp(p).metadata();

const IMG = process.argv[2];
const OUT = process.argv[3];

const THEME = {
  name: 'Master Data Teal',
  headFontFace: 'Arial',
  bodyFontFace: 'Arial',
  colors: {
    dk1: '1B2B34', lt1: 'FFFFFF', dk2: '0E4D64', lt2: 'EAF2F4',
    accent1: '137C8B', accent2: 'E8A33D', accent3: '2E8B57', accent4: 'C0392B',
    accent5: '6C7A89', accent6: '0E4D64', hlink: '137C8B', folHlink: '6C7A89',
  },
};
const HEX = THEME.colors;

const pres = new pptxgen();
pres.layout = 'LAYOUT_WIDE';            // 13.333 x 7.5
pres.theme = { headFontFace: THEME.headFontFace, bodyFontFace: THEME.bodyFontFace };
pres.title = 'Customer and Vendor Master Data in SAP S/4HANA';
pres.author = 'SAP project team';
const C = pres.SchemeColor;
const W = 13.333;

// ------------------------------------------------------------------ layouts
pres.defineSlideMaster({
  title: 'TITLE_DARK',
  background: { color: C.text2 },
  objects: [
    { placeholder: { options: { name: 'title', type: 'title', x: 0.8, y: 2.2, w: 11.7, h: 1.6,
      fontSize: 40, bold: true, color: C.background1, valign: 'bottom', align: 'left', margin: 0 }, text: '' } },
    { placeholder: { options: { name: 'body', type: 'body', x: 0.8, y: 4.0, w: 11.7, h: 1.0,
      fontSize: 20, color: C.background2, valign: 'top', margin: 0 }, text: '' } },
  ],
});
pres.defineSlideMaster({
  title: 'SECTION_DARK',
  background: { color: C.text2 },
  objects: [
    { placeholder: { options: { name: 'body', type: 'body', x: 0.8, y: 2.3, w: 11.7, h: 0.6,
      fontSize: 18, bold: true, color: C.accent2, margin: 0 }, text: '' } },
    { placeholder: { options: { name: 'title', type: 'title', x: 0.8, y: 2.9, w: 11.7, h: 1.2,
      fontSize: 40, bold: true, color: C.background1, valign: 'top', align: 'left', margin: 0 }, text: '' } },
  ],
});
pres.defineSlideMaster({
  title: 'CONTENT',
  background: { color: C.background1 },
  margin: [0.4, 0.6, 0.6, 0.6],
  objects: [
    { placeholder: { options: { name: 'title', type: 'title', x: 0.6, y: 0.35, w: 12.1, h: 0.85,
      fontSize: 32, bold: true, color: C.text2, valign: 'middle', align: 'left', margin: 0 }, text: '' } },
    { text: { text: 'Customer and Vendor Master Data in SAP S/4HANA - reference implementation',
      options: { x: 0.6, y: 7.0, w: 9, h: 0.3, fontSize: 10, color: C.accent5, margin: 0 } } },
  ],
  slideNumber: { x: 12.2, y: 7.0, w: 0.5, h: 0.3, fontSize: 10, color: C.accent5, align: 'right' },
});

// ------------------------------------------------------------------ helpers
async function icon(name, color = '#FFFFFF') {
  const svg = RDS.renderToStaticMarkup(React.createElement(md[name], { color, size: 256 }));
  const buf = await sharp(Buffer.from(svg)).png().toBuffer();
  return 'image/png;base64,' + buf.toString('base64');
}
let objN = 0;
const oid = (p) => `${p}-${++objN}`;

async function badge(slide, name, x, y, d = 0.6, fill = C.accent1) {
  slide.addShape(pres.shapes.OVAL, { x, y, w: d, h: d, fill: { color: fill }, line: { type: 'none' },
    objectName: oid('badge') });
  const pad = d * 0.22;
  slide.addImage({ data: await icon(name), x: x + pad, y: y + pad, w: d - 2 * pad, h: d - 2 * pad,
    objectName: oid('icon') });
}
function numBadge(slide, n, x, y, d = 0.42, fill = C.accent1) {
  slide.addShape(pres.shapes.OVAL, { x, y, w: d, h: d, fill: { color: fill }, line: { type: 'none' },
    objectName: oid('num') });
  slide.addText(String(n), { x, y, w: d, h: d, fontSize: 14, bold: true, color: C.background1,
    align: 'center', valign: 'middle', margin: 0, isTextBox: true, objectName: oid('numtxt') });
}
function card(slide, x, y, w, h, fill = C.background2) {
  slide.addShape(pres.shapes.ROUNDED_RECTANGLE, { x, y, w, h, rectRadius: 0.08, fill: { color: fill },
    line: { type: 'none' }, objectName: oid('card') });
}
function text(slide, t, o) {
  slide.addText(t, Object.assign({ isTextBox: true, margin: 0, valign: 'top', color: C.text1, fontSize: 15,
    objectName: oid('text') }, o));
}
function caption(slide, t, x, y, w) {
  text(slide, t, { x, y, w, h: 0.3, fontSize: 11, italic: true, color: C.accent5 });
}
async function picture(slide, file, x, y, w) {
  const m = await sizeOf(path.join(IMG, file));
  const h = w * m.height / m.width;
  slide.addImage({ path: path.join(IMG, file), x, y, w, h, objectName: oid('img'),
    altText: file.replace('.png', '').replace('_', ' ') });
  slide.addShape(pres.shapes.RECTANGLE, { x, y, w, h, fill: { type: 'none' },
    line: { color: 'C9D3DA', width: 0.75 }, objectName: oid('frame') });
  return h;
}
function content(section, title) {
  const s = pres.addSlide({ masterName: 'CONTENT', sectionTitle: section });
  s.addText(title, { placeholder: 'title' });
  return s;
}

(async () => {
  // ================================================================ 1 title
  pres.addSection({ title: 'Introduction' });
  let s = pres.addSlide({ masterName: 'TITLE_DARK', sectionTitle: 'Introduction' });
  s.addText('Customer and Vendor Master Data in SAP S/4HANA', { placeholder: 'title' });
  s.addText('Excel download and upload - the requirement and the solution, from a reference implementation',
    { placeholder: 'body' });
  text(s, 'October 2026', { x: 0.8, y: 6.4, w: 4, h: 0.4, fontSize: 14, color: C.background2 });
  s.addNotes('Reference implementation for a pharmaceutical client. Client name and data withheld; all sample values in the illustrations are invented.');

  // ================================================================ 2 at a glance
  s = content('Introduction', 'The short version');
  const stats = [
    ['2', 'programs - customer and vendor, each downloading and uploading'],
    ['24', 'customer Excel layouts, picked automatically'],
    ['79', 'country / region and account group combinations covered'],
    ['9', 'vendor scenarios, from creation to block / unblock'],
  ];
  stats.forEach(([n, l], i) => {
    const x = 0.6 + i * 3.1;
    card(s, x, 1.6, 2.85, 2.6);
    text(s, n, { x: x + 0.3, y: 1.85, w: 2.3, h: 1.1, fontSize: 60, bold: true, color: C.text2 });
    text(s, l, { x: x + 0.3, y: 3.0, w: 2.3, h: 1.1, fontSize: 14, color: C.text1 });
  });
  text(s, [
    { text: 'Each program downloads the client’s own Excel template - empty, or filled with existing data - and uploads the filled template back. ', options: {} },
    { text: 'Every posting goes through SAP’s standard Business Partner interface, with a test run before anything is saved.', options: { bold: true } },
  ], { x: 0.6, y: 4.65, w: 12.1, h: 1.2, fontSize: 18 });

  // ================================================================ 3 section
  pres.addSection({ title: 'The requirement' });
  s = pres.addSlide({ masterName: 'SECTION_DARK', sectionTitle: 'The requirement' });
  s.addText('PART 1', { placeholder: 'body' });
  s.addText('The requirement', { placeholder: 'title' });

  // ================================================================ 4 why
  s = content('The requirement', 'Why the existing loads had to be replaced');
  card(s, 0.6, 1.6, 5.6, 3.6);
  card(s, 7.1, 1.6, 5.6, 3.6, C.background2);
  await badge(s, 'MdHistory', 0.9, 1.9, 0.7, C.accent5);
  await badge(s, 'MdUpgrade', 7.4, 1.9, 0.7, C.accent1);
  text(s, 'Before - SAP ECC', { x: 1.8, y: 2.0, w: 4.2, h: 0.5, fontSize: 20, bold: true, color: C.text2 });
  text(s, 'Now - SAP S/4HANA', { x: 8.3, y: 2.0, w: 4.2, h: 0.5, fontSize: 20, bold: true, color: C.text2 });
  const bl = (arr) => arr.map((t, i) => ({ text: t, options: { bullet: true, breakLine: i < arr.length - 1, paraSpaceAfter: 8 } }));
  text(s, bl([
    'Customer and vendor master loaded with LSMW recordings of XD01, XK01, XK02 and XK05',
    'One recording per Excel layout, replaying the screens field by field',
    'A new layout meant a new recording',
  ]), { x: 0.9, y: 2.9, w: 5.0, h: 2.1, fontSize: 15 });
  text(s, bl([
    'Customers and vendors are Business Partners, maintained in transaction BP',
    'The old customer and vendor transactions are redirected or removed (SAP Note 2265093)',
    'Screen recordings against them can no longer run',
  ]), { x: 7.4, y: 2.9, w: 5.0, h: 2.1, fontSize: 15 });
  s.addShape(pres.shapes.RIGHT_ARROW, { x: 6.35, y: 3.1, w: 0.6, h: 0.6, fill: { color: C.accent2 },
    line: { type: 'none' }, objectName: oid('arrow') });
  text(s, [
    { text: 'So: ', options: { bold: true, color: C.text2 } },
    { text: 'the Excel layouts the business knows could stay - only the engine underneath had to change.', options: {} },
  ], { x: 0.6, y: 5.65, w: 12.1, h: 0.6, fontSize: 18 });

  // ================================================================ 5 customer requirement
  s = content('The requirement', 'What the business asked for - customers');
  const creq = [
    ['MdGridOn', 'A template per country and account group', 'Every country / region and account group has its own Excel layout. The user should never have to choose it.'],
    ['MdFileDownload', 'Download', 'Get the right template - empty, or filled with existing customers to start from.'],
    ['MdArrowDropDownCircle', 'Simple selection', 'Country / region from a dropdown; the account group list limited to that region.'],
    ['MdFileUpload', 'Upload', 'Create customers, extend them to a new sales area, and block or unblock them.'],
    ['MdPlaylistAddCheck', 'Test before posting', 'A test run that makes every check and saves nothing.'],
    ['MdChatBubbleOutline', 'Clear messages', 'One result line per Excel row, naming the field at fault.'],
  ];
  for (let i = 0; i < creq.length; i++) {
    const [ic, h, b] = creq[i];
    const x = 0.6 + (i % 3) * 4.1, y = 1.6 + Math.floor(i / 3) * 2.55;
    card(s, x, y, 3.85, 2.3);
    await badge(s, ic, x + 0.3, y + 0.3, 0.6);
    text(s, h, { x: x + 1.1, y: y + 0.32, w: 2.55, h: 0.6, fontSize: 16, bold: true, color: C.text2, valign: 'middle' });
    text(s, b, { x: x + 0.3, y: y + 1.05, w: 3.3, h: 1.1, fontSize: 14 });
  }

  // ================================================================ 6 vendor requirement
  s = content('The requirement', 'What the business asked for - vendors');
  text(s, [
    { text: 'The existing vendor workbook had to keep working as it is.', options: { bold: true, breakLine: true } },
    { text: 'One tab per scenario, the same columns as before - no re-keying and no new formats. Each scenario had to be loadable on its own, in test mode first.', options: {} },
  ], { x: 0.6, y: 1.6, w: 4.4, h: 3.5, fontSize: 17, paraSpaceAfter: 10 });
  const scen = ['Vendor / BP creation - all company codes', 'Withholding tax / TDS', 'TAN exemption details',
    'Bank key creation', 'Vendor bank details', 'Vendor extension', 'CIN details', 'Partner functions', 'Block / unblock'];
  scen.forEach((t, i) => {
    const x = 5.4 + (i % 3) * 2.45, y = 1.6 + Math.floor(i / 3) * 1.45;
    card(s, x, y, 2.25, 1.25);
    numBadge(s, i + 1, x + 0.18, y + 0.18, 0.4);
    text(s, t, { x: x + 0.18, y: y + 0.66, w: 1.95, h: 0.55, fontSize: 13, bold: true, color: C.text2 });
  });
  caption(s, 'The nine tabs of the vendor workbook', 5.4, 5.95, 7.0);

  // ================================================================ 7 section
  pres.addSection({ title: 'What we built' });
  s = pres.addSlide({ masterName: 'SECTION_DARK', sectionTitle: 'What we built' });
  s.addText('PART 2', { placeholder: 'body' });
  s.addText('What we built', { placeholder: 'title' });

  // ================================================================ 8 architecture
  s = content('What we built', 'One program per master, in both directions');
  const boxes = [
    ['MdTableChart', 'Excel workbook', 'The client’s own template, one tab per layout or scenario'],
    ['MdSettings', 'Program', 'One column map per template - used to write the download and to read the upload'],
    ['MdPeople', 'SAP Business Partner', 'Customer or vendor, through SAP’s standard interface'],
  ];
  for (let i = 0; i < 3; i++) {
    const x = 0.6 + i * 4.3;
    card(s, x, 1.75, 3.5, 2.5);
    await badge(s, boxes[i][0], x + 0.3, 2.0, 0.65);
    text(s, boxes[i][1], { x: x + 1.1, y: 2.0, w: 2.3, h: 0.65, fontSize: 18, bold: true, color: C.text2, valign: 'middle' });
    text(s, boxes[i][2], { x: x + 0.3, y: 2.85, w: 3.0, h: 1.2, fontSize: 14 });
  }
  for (let i = 0; i < 2; i++) {
    const x = 4.15 + i * 4.3;
    s.addShape(pres.shapes.LEFT_ARROW, { x, y: 2.35, w: 0.7, h: 0.4, fill: { color: C.accent2 }, line: { type: 'none' }, objectName: oid('arr') });
    s.addShape(pres.shapes.RIGHT_ARROW, { x, y: 3.25, w: 0.7, h: 0.4, fill: { color: C.accent1 }, line: { type: 'none' }, objectName: oid('arr') });
  }
  for (const gx of [4.1, 8.4]) {
    text(s, 'Download', { x: gx, y: 2.0, w: 0.8, h: 0.3, fontSize: 10, bold: true, color: C.accent2, align: 'center' });
    text(s, 'Upload', { x: gx, y: 3.7, w: 0.8, h: 0.3, fontSize: 10, bold: true, color: C.accent1, align: 'center' });
  }
  text(s, bl([
    'The same column map writes the download and reads the upload, so a downloaded file always uploads back.',
    'Columns are found by their headings: a moved or an extra column still loads into the right field.',
    'Every posting uses SAP’s standard Business Partner interface - no screen recordings, no direct updates of SAP tables.',
  ]), { x: 0.6, y: 4.6, w: 12.1, h: 2.0, fontSize: 15 });

  // ================================================================ 9 customer screen
  s = content('What we built', 'Customer program - selection screen');
  const ch = await picture(s, 'cust_screen.png', 0.6, 1.45, 6.6);
  caption(s, 'Illustration of the selection screen', 0.6, 1.45 + ch + 0.08, 6.6);
  const ccall = [
    ['Download or upload', 'One program for both directions; fields that do not apply are greyed out.'],
    ['Template', 'Create, extension or block / unblock.'],
    ['Country / region and account group', 'Picked from lists - the program finds the right layout from the pair.'],
    ['Which customers', 'Existing customers to download, or upload options: test run, stop at the first error.'],
    ['File', 'On the PC or the application server; a name is proposed for a download.'],
  ];
  ccall.forEach(([h, b], i) => {
    const y = 1.5 + i * 1.0;
    numBadge(s, i + 1, 7.6, y);
    text(s, h, { x: 8.2, y: y - 0.02, w: 4.5, h: 0.35, fontSize: 15, bold: true, color: C.text2 });
    text(s, b, { x: 8.2, y: y + 0.35, w: 4.5, h: 0.6, fontSize: 13 });
  });

  // ================================================================ 10 customer excel
  s = content('What we built', 'Customer Excel format');
  const eh = await picture(s, 'cust_excel.png', 0.6, 1.5, 12.1);
  caption(s, 'Illustration - a create template, selected columns; sample values are invented', 0.6, 1.5 + eh + 0.08, 12.1);
  const cfmt = [
    ['MdViewColumn', 'The client’s own headings', 'Each of the 24 layouts keeps its columns and wording - up to 136 columns.'],
    ['MdLabel', 'Named after the selection', 'The tab is named after the region and account group, for example IN_ZDOM.'],
    ['MdAddCircleOutline', 'Create or change', 'An empty customer code creates a new customer; a filled one changes that customer.'],
  ];
  for (let i = 0; i < 3; i++) {
    const x = 0.6 + i * 4.1, y = 3.85;
    card(s, x, y, 3.85, 2.2);
    await badge(s, cfmt[i][0], x + 0.3, y + 0.3, 0.6);
    text(s, cfmt[i][1], { x: x + 1.1, y: y + 0.32, w: 2.55, h: 0.6, fontSize: 16, bold: true, color: C.text2, valign: 'middle' });
    text(s, cfmt[i][2], { x: x + 0.3, y: y + 1.05, w: 3.3, h: 1.0, fontSize: 14 });
  }

  // ================================================================ 11 vendor screen
  s = content('What we built', 'Vendor program - selection screen');
  const vh = await picture(s, 'vend_screen.png', 0.6, 1.45, 6.35);
  caption(s, 'Illustration of the selection screen', 0.6, 1.45 + vh + 0.08, 6.35);
  const vcall = [
    ['Download or upload', 'The same program writes a tab from existing vendors or posts a filled one.'],
    ['Scenario', 'Nine scenarios, one per tab of the vendor workbook.'],
    ['Upload options', 'Test run ticked by default; optionally stop at the first faulty row.'],
    ['File', 'F4 picks the workbook to upload; a download proposes a name per scenario.'],
  ];
  vcall.forEach(([h, b], i) => {
    const y = 1.6 + i * 1.2;
    numBadge(s, i + 1, 7.4, y);
    text(s, h, { x: 8.0, y: y - 0.02, w: 4.7, h: 0.35, fontSize: 15, bold: true, color: C.text2 });
    text(s, b, { x: 8.0, y: y + 0.35, w: 4.7, h: 0.7, fontSize: 13 });
  });

  // ================================================================ 12 vendor excel
  s = content('What we built', 'Vendor Excel format and loading order');
  const v2 = await picture(s, 'vend_excel.png', 0.6, 1.5, 9.0);
  caption(s, 'Illustration - vendor creation tab, selected columns; sample values are invented', 0.6, 1.5 + v2 + 0.08, 9.0);
  text(s, 'Technical field names as headings, as in the existing workbook. An empty vendor number creates a new vendor from the account group’s number range.',
    { x: 9.9, y: 1.5, w: 2.8, h: 1.9, fontSize: 13 });
  text(s, 'Scenarios build on each other, so they are loaded in this order', { x: 0.6, y: 4.05, w: 12.1, h: 0.4, fontSize: 16, bold: true, color: C.text2 });
  const order = ['Bank keys', 'Vendor creation', 'Vendor extension', 'TDS, TAN, bank details, CIN, partner functions', 'Block / unblock'];
  order.forEach((t, i) => {
    const x = 0.6 + i * 2.5;
    card(s, x, 4.65, 2.2, 1.45);
    numBadge(s, i + 1, x + 0.18, 4.8, 0.4);
    text(s, t, { x: x + 0.18, y: 5.3, w: 1.9, h: 0.75, fontSize: 13, bold: true, color: C.text2 });
    if (i < order.length - 1) {
      s.addShape(pres.shapes.RIGHT_ARROW, { x: x + 2.24, y: 5.22, w: 0.22, h: 0.3, fill: { color: C.accent2 }, line: { type: 'none' }, objectName: oid('flow') });
    }
  });
  caption(s, 'A row that needs data not there yet - tax data for a company code the vendor is not in - is refused with a message, not posted halfway.', 0.6, 6.25, 12.1);

  // ================================================================ 13 result list
  s = content('What we built', 'Every Excel row gets a clear result');
  const rh = await picture(s, 'result_list.png', 0.6, 1.5, 9.4);
  caption(s, 'Illustration of the result list after a test run', 0.6, 1.5 + rh + 0.08, 9.4);
  const legend = [
    ['2E8B57', 'Done', 'posted - or, in a test run, would be'],
    ['E8A33D', 'Warning', 'went through; read the message'],
    ['C0392B', 'Error', 'nothing posted for this row'],
    ['3C6EAA', 'Information', 'about the run, not a row'],
  ];
  legend.forEach(([col, h, b], i) => {
    const y = 1.55 + i * 0.6;
    s.addShape(pres.shapes.OVAL, { x: 10.35, y: y + 0.05, w: 0.28, h: 0.28, fill: { color: col }, line: { type: 'none' }, objectName: oid('dot') });
    text(s, [{ text: h + ' ', options: { bold: true } }, { text: b, options: {} }], { x: 10.75, y, w: 2.0, h: 0.55, fontSize: 12 });
  });
  card(s, 0.6, 4.55, 12.1, 1.5);
  await badge(s, 'MdLoop', 0.9, 4.85, 0.65);
  text(s, [
    { text: 'Test run first, then post. ', options: { bold: true, color: C.text2 } },
    { text: 'The test run makes every check SAP would make and saves nothing. The same list names the Excel row, the record and the field at fault, so the file can be corrected and run again. Only then is the test run switched off to post.', options: {} },
  ], { x: 1.8, y: 4.8, w: 10.6, h: 1.1, fontSize: 15 });

  // ================================================================ 14 standards
  s = content('What we built', 'Built to SAP S/4HANA standards');
  const std = [
    ['MdVerified', 'Standard interfaces', 'Business Partner maintained through SAP’s customer / vendor integration API, with every commit checked.'],
    ['MdBlock', 'No direct table updates', 'SAP data changes only through standard interfaces; client-owned custom tables only where agreed.'],
    ['MdPlaylistAddCheck', 'Test mode built in', 'A full check of every row before anything is saved.'],
    ['MdSearch', 'Read by heading', 'Columns found by their headings, so a changed layout cannot load a value into the wrong field.'],
    ['MdCloudDone', 'Clean core aware', 'Classic ABAP extension (tier 2 in SAP’s clean core model), ATC readiness check documented.'],
    ['MdFactCheck', 'Automated checks', 'Every issue found in testing became an automated check, so it cannot come back.'],
  ];
  for (let i = 0; i < std.length; i++) {
    const [ic, h, b] = std[i];
    const x = 0.6 + (i % 3) * 4.1, y = 1.6 + Math.floor(i / 3) * 2.55;
    card(s, x, y, 3.85, 2.3);
    await badge(s, ic, x + 0.3, y + 0.3, 0.6);
    text(s, h, { x: x + 1.1, y: y + 0.32, w: 2.55, h: 0.6, fontSize: 16, bold: true, color: C.text2, valign: 'middle' });
    text(s, b, { x: x + 0.3, y: y + 1.05, w: 3.3, h: 1.15, fontSize: 14 });
  }

  // ================================================================ 15 approach
  s = content('What we built', 'How we delivered it');
  const steps = [
    ['Analyse', 'The client’s templates: layouts, country and account group combinations, every column.'],
    ['Map', 'Every column to its Business Partner field - generated from the templates, not typed by hand.'],
    ['Build', 'Download and upload in one program per master, on one column map.'],
    ['Test', 'With the business, against a test script with expected results and sign-off.'],
    ['Hand over', 'User and test guide; programs and texts ready to transport.'],
  ];
  s.addShape(pres.shapes.LINE, { x: 1.0, y: 2.25, w: 10.6, h: 0, line: { color: C.accent1, width: 2 }, objectName: oid('timeline') });
  steps.forEach(([h, b], i) => {
    const x = 0.6 + i * 2.5;
    numBadge(s, i + 1, x + 0.2, 2.0, 0.5);
    text(s, h, { x, y: 2.75, w: 2.3, h: 0.45, fontSize: 18, bold: true, color: C.text2 });
    text(s, b, { x, y: 3.25, w: 2.3, h: 1.6, fontSize: 14 });
  });
  const figs = [
    ['1,986', 'template columns mapped from the client\u2019s customer workbook'],
    ['33', 'user tests in the sign-off script, with expected results'],
    ['0', 'screen recordings - everything through standard interfaces'],
  ];
  figs.forEach(([n, l], i) => {
    const x = 0.6 + i * 4.1;
    card(s, x, 5.05, 3.85, 1.55);
    text(s, n, { x: x + 0.3, y: 5.2, w: 1.5, h: 1.25, fontSize: 36, bold: true, color: C.text2, valign: 'middle' });
    text(s, l, { x: x + 1.85, y: 5.2, w: 1.85, h: 1.25, fontSize: 12, valign: 'middle' });
  });

  // ================================================================ 16 next steps
  pres.addSection({ title: 'Next steps' });
  s = pres.addSlide({ masterName: 'SECTION_DARK', sectionTitle: 'Next steps' });
  s.addText('NEXT STEPS', { placeholder: 'body' });
  s.addText('What we need from you to start', { placeholder: 'title' });
  const need = [
    'Your Excel templates - every country, account group and scenario in use',
    'Which objects are in scope: customers, vendors, or both',
    'SAP release, and access to a development and a test system',
    'Account groups, BP groupings and number ranges',
    'Sample data for testing',
    'A business contact to confirm the field mapping',
  ];
  need.forEach((t, i) => {
    const x = 0.8 + (i % 2) * 6.0, y = 4.25 + Math.floor(i / 2) * 0.8;
    s.addShape(pres.shapes.OVAL, { x, y: y + 0.06, w: 0.3, h: 0.3, fill: { color: C.accent2 }, line: { type: 'none' }, objectName: oid('dot') });
    text(s, t, { x: x + 0.5, y, w: 5.3, h: 0.7, fontSize: 15, color: C.background1 });
  });

  await pres.writeFile({ fileName: OUT });
  await applyTheme(OUT, THEME);
  console.log('written', OUT);
})().catch((e) => { console.error(e); process.exit(1); });
