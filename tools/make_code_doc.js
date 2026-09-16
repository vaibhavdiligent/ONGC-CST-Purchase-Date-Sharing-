const fs = require("fs");
const path = require("path");
const {
  Document, Packer, Paragraph, TextRun, HeadingLevel, AlignmentType,
  Table, TableRow, TableCell, WidthType, ShadingType, BorderStyle,
  PageBreak, TableOfContents, Footer, PageNumber,
} = require("docx");

const SRC = "/home/user/ONGC-CST-Purchase-Date-Sharing-/src/rap";
const OUT = path.join(__dirname, "ZDPR_RAP_Complete_Source_Code.docx");
const BLUE = "1F4E79", GREY = "F2F2F2", CODEBG = "F7F7F7";

const H1 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_1, spacing: { before: 320, after: 160 } });
const H2 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_2, spacing: { before: 260, after: 120 } });
const P = (t, o = {}) => new Paragraph({ spacing: { after: 120 },
  children: [new TextRun({ text: t, size: 21, bold: !!o.bold, italics: !!o.italics })] });
const PB = () => new Paragraph({ children: [new PageBreak()] });
const SP = () => new Paragraph({ spacing: { after: 120 } });
function code(text) {
  const lines = text.replace(/\t/g, "    ").split("\n");
  while (lines.length && !lines[lines.length - 1].trim()) lines.pop();
  return lines.map((l) => new Paragraph({ spacing: { after: 0, line: 240 },
    shading: { type: ShadingType.CLEAR, fill: CODEBG }, indent: { left: 120, right: 120 },
    border: { left: { style: BorderStyle.SINGLE, size: 12, color: "8EA9DB", space: 6 } },
    children: [new TextRun({ text: l || " ", font: "Consolas", size: 15 })] }));
}
function table(headers, rows, widths) {
  const total = widths.reduce((a, b) => a + b, 0);
  const mk = (txt, bold, fill, w) => new TableCell({ width: { size: w, type: WidthType.DXA },
    shading: { type: ShadingType.CLEAR, fill }, margins: { top: 60, bottom: 60, left: 100, right: 100 },
    children: [new Paragraph({ spacing: { after: 0 },
      children: [new TextRun({ text: txt, bold, size: 18, color: bold ? "FFFFFF" : undefined })] })] });
  return new Table({ width: { size: total, type: WidthType.DXA }, columnWidths: widths, rows: [
    new TableRow({ tableHeader: true, children: headers.map((h, i) => mk(h, true, BLUE, widths[i])) }),
    ...rows.map((r, ri) => new TableRow({ children: r.map((c, i) => mk(c, false, ri % 2 ? GREY : "FFFFFF", widths[i])) })) ] });
}
function note(label, text, fill) {
  return new Table({ width: { size: 9360, type: WidthType.DXA }, columnWidths: [9360],
    rows: [new TableRow({ children: [new TableCell({ width: { size: 9360, type: WidthType.DXA },
      shading: { type: ShadingType.CLEAR, fill }, margins: { top: 100, bottom: 100, left: 140, right: 140 },
      children: [new Paragraph({ spacing: { after: 0 }, children: [
        new TextRun({ text: label + "  ", bold: true, size: 21 }), new TextRun({ text, size: 21 }) ] })] })] })] });
}
const read = (f) => fs.readFileSync(path.join(SRC, f), "utf8");

/* ── object catalogue, in creation order ──────────────────────────────── */
const SECTIONS = [
  { title: "Interface Views", objs: [
    ["ZDPR_I_DAILY", "ZDPR_I_DAILY.ddls.asddls", "CDS view entity", "Daily production (ZPRA_T_DLY_PRD) with calendar year/month, product and volume-type texts."],
    ["ZDPR_I_MONTHLY", "ZDPR_I_MONTHLY.ddls.asddls", "CDS view entity", "Monthly reconciled production (ZPRA_T_MREC_PRD)."],
    ["ZDPR_I_TARGET", "ZDPR_I_TARGET.ddls.asddls", "CDS view entity", "Production targets (ZPRA_T_PRD_TAR) scaled like the classic report: TargetVolume (barrels / MMSCM), TargetBoe, conversion factor from ZPRA_T_TAR_CF, days in fiscal year."],
    ["ZDPR_I_TARGET_FY", "ZDPR_I_TARGET_FY.ddls.asddls", "CDS view entity", "Annual target per fiscal year / asset / block / product (all months, volume types NET_PROD, GROSS_PROD, GAS_INJ)."],
  ]},
  { title: "Base and Aggregation Layer", objs: [
    ["ZDPR_P_DAY_BASE", "ZDPR_P_DAY_BASE.ddls.asddls", "CDS view entity", "Unit-normalised, signed daily figure (gas = GROSS_PROD − GAS_INJ), BOE factor, PI %, fiscal year/period, Business Unit."],
    ["ZDPR_P_DATE_SPINE", "ZDPR_P_DATE_SPINE.ddls.asddls", "CDS view entity", "One row per production date with its fiscal year / period."],
    ["ZDPR_P_TARGET_DAY", "ZDPR_P_TARGET_DAY.ddls.asddls", "CDS view entity", "BE target daily rate (annual volume ÷ days in FY) on every production date per asset / block / product."],
    ["ZDPR_P_BOEPD_ROWS", "ZDPR_P_BOEPD_ROWS.ddls.asddls", "CDS view entity (union)", "Actual rows (A) and target rows (T) with identical columns — row source of ZDPR_C_BOEPD_DAY."],
    ["ZDPR_P_PERF_AGG", "ZDPR_P_PERF_AGG.ddls.asddls", "CDS view entity (union)", "YTD and ANNUAL aggregates for the Production Performance table (Excel tab 3)."],
  ]},
  { title: "Analytical Cubes", objs: [
    ["ZDPR_C_PROD_CUBE", "ZDPR_C_PROD_CUBE.ddls.asddls", "CDS view entity, @Analytics.dataCategory #CUBE", "Daily production cube: JV and OVL quantities, gas MMSCMD, BOEPD."],
    ["ZDPR_C_BOEPD_DAY", "ZDPR_C_BOEPD_DAY.ddls.asddls", "CDS view entity, #CUBE", "Actual vs BE target per day (plain select on ZDPR_P_BOEPD_ROWS) — the data behind the Excel tab-2 graph."],
    ["ZDPR_C_TARGET_CUBE", "ZDPR_C_TARGET_CUBE.ddls.asddls", "CDS view entity, #CUBE, parameter P_TargetCode", "Monthly actual vs target; the join lives here because analytical queries may not join."],
  ]},
  { title: "Analytical Queries (OData V2 via @OData.publish)", objs: [
    ["ZDPR_Q_BOEPD_TREND", "ZDPR_Q_BOEPD_TREND.ddls.asddls", "Classic CDS view, @Analytics.query, @OData.publish", "Excel tab-2 graph: Actual Production vs BE Target BOEPD over dates. Service ZDPR_Q_BOEPD_TREND_CDS."],
    ["ZDPR_Q_DAILY_TREND", "ZDPR_Q_DAILY_TREND.ddls.asddls", "Classic CDS view, @Analytics.query, @OData.publish", "Daily production trend. Service ZDPR_Q_DAILY_TREND_CDS."],
    ["ZDPR_Q_PROD_QUERY", "ZDPR_Q_PROD_QUERY.ddls.asddls", "Classic CDS view, @Analytics.query, @OData.publish", "Production records by product and asset. Service ZDPR_Q_PROD_QUERY_CDS."],
    ["ZDPR_Q_TARGET_QUERY", "ZDPR_Q_TARGET_QUERY.ddls.asddls", "Classic CDS view, @Analytics.query, @OData.publish", "Target vs actual with Achievement % as a FORMULA. Service ZDPR_Q_TARGET_QUERY_CDS."],
  ]},
  { title: "Production Performance Query (OData V2 and V4)", objs: [
    ["ZDPR_Q_PROD_PERF", "ZDPR_Q_PROD_PERF.ddls.asddls", "Classic CDS view, @OData.publish (also in the V4 service definition)", "Excel tab-3 Production Performance: per-day actual and target, % achievement, criticality. OData V2 service ZDPR_Q_PROD_PERF_CDS for the Overview Page card."],
  ]},
  { title: "Excel / PDF Download — Abstract Entities", objs: [
    ["ZDPR_A_PROD_PARAM", "ZDPR_A_PROD_PARAM.ddls.asddls", "CDS abstract entity", "Action parameter: date range."],
    ["ZDPR_A_TAR_PARAM", "ZDPR_A_TAR_PARAM.ddls.asddls", "CDS abstract entity", "Action parameter: fiscal year and target code."],
    ["ZDPR_A_EXCEL_RESULT", "ZDPR_A_EXCEL_RESULT.ddls.asddls", "CDS abstract entity", "Action result: Excel file (base64)."],
    ["ZDPR_A_PDF_RESULT", "ZDPR_A_PDF_RESULT.ddls.asddls", "CDS abstract entity", "Action result: PDF file (base64)."],
  ]},
  { title: "Excel / PDF Download — Entity, Behavior and Classes", objs: [
    ["ZDPR_I_EXCEL_DL", "ZDPR_I_EXCEL_DL.ddls.asddls", "CDS root view entity", "Host entity for the static download actions (one row per logged-on user)."],
    ["ZDPR_I_EXCEL_DL (Behavior Definition)", "ZDPR_I_EXCEL_DL.bdef.asbdef", "Behavior definition, unmanaged, action-only", "Four static actions: downloadProduction, downloadTargets, downloadPdfProduction, downloadPdfTargets."],
    ["ZBP_ZDPR_EXCEL_DL", "ZBP_ZDPR_EXCEL_DL.clas.abap", "ABAP class (behavior implementation)", "Global class (top) and local handler class lhc_dpr_excel (goes into the Local Types include in ADT)."],
    ["ZCL_ZDPR_EXCEL", "ZCL_ZDPR_EXCEL.clas.abap", "ABAP class", "Excel generation with abap2xlsx (zcl_excel)."],
    ["ZCL_ZDPR_PDF", "ZCL_ZDPR_PDF.clas.abap", "ABAP class", "PDF generation via Adobe forms ZDPR_FRM_PRODUCTION / ZDPR_FRM_TARGETS (optional)."],
  ]},
  { title: "Service Definition and Service Binding", objs: [
    ["ZDPR_SD_ANALYTICS", "ZDPR_SD_ANALYTICS.srvd.asddls", "Service definition", "Exposes the performance query, cubes, interface views and the download entity over OData V4."],
    ["ZDPR_SB_ANALYTICS_O4", "ZDPR_SB_ANALYTICS_O4.srvb.asddls", "Service binding (created via ADT wizard, OData V4 – UI)", "Source form shown for reference only — create with the wizard and Publish."],
  ]},
  { title: "Metadata Extensions (create in ADT)", objs: [
    ["ZDPR_Q_BOEPD_TREND (DDLX)", "ZDPR_Q_BOEPD_TREND.ddlx.asddlx", "Metadata extension", "Line chart Actual vs BE Target, filters, table columns."],
    ["ZDPR_Q_DAILY_TREND (DDLX)", "ZDPR_Q_DAILY_TREND.ddlx.asddlx", "Metadata extension", "Daily trend line chart and table."],
    ["ZDPR_Q_PROD_QUERY (DDLX)", "ZDPR_Q_PROD_QUERY.ddlx.asddlx", "Metadata extension", "Production records: filter bar, column chart, table."],
    ["ZDPR_Q_TARGET_QUERY (DDLX)", "ZDPR_Q_TARGET_QUERY.ddlx.asddlx", "Metadata extension", "Target vs actual: bar chart, table with Achievement %."],
    ["ZDPR_Q_PROD_PERF (DDLX)", "ZDPR_Q_PROD_PERF.ddlx.asddlx", "Metadata extension", "Production Performance table and Actual-vs-Target column chart."],
  ]},
];

const c = [];
c.push(
  new Paragraph({ spacing: { before: 2200 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "ONGC Videsh — DPR Analytical RAP", bold: true, size: 44, color: BLUE })] }),
  new Paragraph({ spacing: { before: 160 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Complete Source Code", bold: true, size: 34, color: "404040" })] }),
  new Paragraph({ spacing: { before: 500 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Package  ZPR_DPR_RAP   ·   all objects, current state", size: 22 })] }),
  new Paragraph({ spacing: { before: 100 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Developer reference — every object's full source in creation order", size: 22, italics: true, color: "606060" })] }),
  new Paragraph({ spacing: { before: 900 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: `Generated ${new Date().toISOString().slice(0, 10)} from branch claude/eager-euler-dpm9rf, folder src/rap`, size: 18, color: "606060" })] }),
  PB()
);
c.push(H1("Contents"));
c.push(new TableOfContents("Contents", { hyperlink: true, headingStyleRange: "1-2" }));
c.push(PB());

/* 1. How to use */
c.push(H1("1. How to Use This Document"));
c.push(P("This document contains the complete, current source of every object in package ZPR_DPR_RAP. Objects are listed in the order in which they must be created, because each layer depends on the one before it: interface views → base layer → cubes → queries → download objects → service definition → service binding → metadata extensions."));
c.push(P("Most objects were imported through abapGit standalone and are already active; the sources are included so the developer has one complete reference. Two groups must be created by hand in ADT: the service binding (wizard + Publish) and the five metadata extensions (New → Metadata Extension, paste source, activate)."));
c.push(SP());
c.push(note("Creating an object from this document:", "In ADT create the object with the exact name and type given in its heading, delete the generated skeleton, paste the source block in full, save and activate. For classes, the block for ZBP_ZDPR_EXCEL_DL contains both the global class (paste into the main source) and the local handler class starting at CLASS lhc_dpr_excel (paste into the Local Types include).", "E2EFDA"));
c.push(SP());
c.push(P("Naming of the analytical queries: the four ZDPR_Q_* views with @Analytics.query are classic DDIC-based views (define view with @AbapCatalog.sqlViewName) because @OData.publish is required to expose them as OData V2 on this release. All other views are view entities.", { italics: true }));

c.push(H2("1.1 Object index"));
const idx = [];
SECTIONS.forEach((s) => s.objs.forEach((o) => idx.push([o[0], o[2], o[3]])));
c.push(table(["Object", "Type", "Purpose"], idx, [2500, 2600, 4260]));
c.push(SP());
c.push(P("Dependencies outside the package that must exist in every system: tables ZPRA_T_DLY_PRD, ZPRA_T_MREC_PRD, ZPRA_T_PRD_TAR, ZPRA_T_PRD_PI; view ZPRA_C_DPR_PROF; table ZOIU_PR_DN; the abap2xlsx library (ZCL_EXCEL); optionally the Adobe forms ZDPR_FRM_PRODUCTION and ZDPR_FRM_TARGETS if PDF output is switched on.", { italics: true }));
c.push(PB());

/* sections */
let n = 2;
for (const s of SECTIONS) {
  c.push(H1(`${n}. ${s.title}`));
  let k = 1;
  for (const [name, file, type, purpose] of s.objs) {
    c.push(H2(`${n}.${k}  ${name}`));
    c.push(P(`Type: ${type}`, { bold: true }));
    c.push(P(purpose));
    c.push(P(`File: src/rap/${file}`, { italics: true }));
    c.push(SP());
    c.push(...code(read(file)));
    c.push(SP());
    c.push(PB());
    k++;
  }
  n++;
}
c.pop(); // no trailing page break

const doc = new Document({
  creator: "ONGC Videsh DPR Project",
  title: "DPR Analytical RAP — Complete Source Code",
  styles: { default: {
    document: { run: { font: "Calibri", size: 21 }, paragraph: { spacing: { line: 276 } } },
    heading1: { run: { font: "Calibri Light", size: 32, bold: true, color: BLUE } },
    heading2: { run: { font: "Calibri Light", size: 26, bold: true, color: "2E74B5" } } } },
  features: { updateFields: true },
  sections: [{
    properties: { page: { size: { width: 12240, height: 15840 }, margin: { top: 1080, bottom: 1080, left: 1080, right: 1080 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER, children: [
      new TextRun({ text: "DPR Analytical RAP — Complete Source Code — Package ZPR_DPR_RAP     Page ", size: 16, color: "808080" }),
      new TextRun({ children: [PageNumber.CURRENT], size: 16, color: "808080" }),
      new TextRun({ text: " of ", size: 16, color: "808080" }),
      new TextRun({ children: [PageNumber.TOTAL_PAGES], size: 16, color: "808080" }) ] })] }) },
    children: c }],
});
Packer.toBuffer(doc).then((b) => { fs.writeFileSync(OUT, b); console.log("written", OUT, b.length, "bytes"); });
