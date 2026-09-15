const fs = require("fs");
const path = require("path");
const {
  Document, Packer, Paragraph, TextRun, HeadingLevel, AlignmentType,
  Table, TableRow, TableCell, WidthType, ShadingType, BorderStyle,
  PageBreak, TableOfContents, LevelFormat, Footer, PageNumber, convertInchesToTwip,
} = require("docx");

const ROOT = path.resolve(__dirname, "..");
const SRC = path.join(ROOT, "src", "rap");
const OUT = path.join(ROOT, "deploy", "ZDPR_RAP_Developer_Guide.docx");
const BLUE = "1F4E79", GREY = "F2F2F2", CODEBG = "F7F7F7";

const H1 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_1, spacing: { before: 320, after: 160 } });
const H2 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_2, spacing: { before: 260, after: 120 } });
const H3 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_3, spacing: { before: 200, after: 100 } });
const P = (t, o = {}) => new Paragraph({ spacing: { after: 120 },
  children: [new TextRun({ text: t, size: 21, bold: !!o.bold, italics: !!o.italics })] });
const BULLET = (t) => new Paragraph({ numbering: { reference: "bullets", level: 0 }, spacing: { after: 80 },
  children: [new TextRun({ text: t, size: 21 })] });
const STEP = (t) => new Paragraph({ numbering: { reference: "steps", level: 0 }, spacing: { after: 100 },
  children: [new TextRun({ text: t, size: 21 })] });
const PB = () => new Paragraph({ children: [new PageBreak()] });
const SP = () => new Paragraph({ spacing: { after: 140 } });
function code(text, size = 16) {
  const lines = text.replace(/\t/g, "    ").split("\n");
  while (lines.length && !lines[lines.length - 1].trim()) lines.pop();
  return lines.map((l) => new Paragraph({ spacing: { after: 0, line: 240 },
    shading: { type: ShadingType.CLEAR, fill: CODEBG }, indent: { left: 120, right: 120 },
    border: { left: { style: BorderStyle.SINGLE, size: 12, color: "8EA9DB", space: 6 } },
    children: [new TextRun({ text: l || " ", font: "Consolas", size })] }));
}
function note(label, text, fill) {
  return new Table({ width: { size: 9360, type: WidthType.DXA }, columnWidths: [9360],
    rows: [new TableRow({ children: [new TableCell({ width: { size: 9360, type: WidthType.DXA },
      shading: { type: ShadingType.CLEAR, fill }, margins: { top: 100, bottom: 100, left: 140, right: 140 },
      children: [new Paragraph({ spacing: { after: 0 }, children: [
        new TextRun({ text: label + "  ", bold: true, size: 21 }), new TextRun({ text, size: 21 }) ] })] })] })] });
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
const read = (f) => fs.readFileSync(path.join(SRC, f), "utf8");

const c = [];
/* ── Title ─────────────────────────────────────────────────────────────── */
c.push(
  new Paragraph({ spacing: { before: 2000 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "ONGC Videsh — DPR Production Dashboard", bold: true, size: 44, color: BLUE })] }),
  new Paragraph({ spacing: { before: 160 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Developer Guide", bold: true, size: 36, color: "404040" })] }),
  new Paragraph({ spacing: { before: 400 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Package ZPR_DPR_RAP  ·  one Fiori Overview Page dashboard on SAP BTP", size: 22 })] }),
  new Paragraph({ spacing: { before: 100 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Import, manual objects, service registration, dashboard build, deployment — and the complete source code", size: 21, italics: true, color: "606060" })] }),
  new Paragraph({ spacing: { before: 800 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: `Version of ${new Date().toISOString().slice(0, 10)} — branch claude/eager-euler-dpm9rf, folder deploy/`, size: 18, color: "606060" })] }),
  new Paragraph({ spacing: { before: 60 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "This document supersedes the earlier separate guides.", size: 18, color: "606060" })] }),
  PB()
);
c.push(H1("Contents"));
c.push(new TableOfContents("Contents", { hyperlink: true, headingStyleRange: "1-2" }));
c.push(PB());

/* ── 1 Overview ────────────────────────────────────────────────────────── */
c.push(H1("1. What Is Being Built"));
c.push(P("The DPR (Daily Production Report) of ONGC Videsh is today produced as an Excel workbook. Its two key views — the graph “Actual Production vs BE Target (BOEPD)” (tab 2) and the “Production Performance” table (tab 3) — are re-implemented as CDS analytical queries in the S/4HANA system and shown to users on ONE Fiori dashboard: a Fiori elements Overview Page with a card per view, opened from a single tile in the Fiori Launchpad (SAP Build Work Zone on BTP)."));
c.push(P("The solution has three layers:"));
c.push(table(
  ["Layer", "Where", "Contents", "Status"],
  [
    ["Data and logic", "S/4HANA, package ZPR_DPR_RAP", "CDS views, cubes, analytical queries, download actions, OData services", "All CDS objects active; this guide covers the remaining manual objects"],
    ["Connectivity", "Cloud Connector + BTP destination", "Tunnel from BTP to the OData services", "Basis task (section 7)"],
    ["Dashboard", "SAP BTP — Business Application Studio, Cloud Foundry, Work Zone", "Overview Page app with cards, tile", "To be built (section 8)"],
  ],
  [1500, 2400, 3060, 2400]
));
c.push(SP());
c.push(H2("1.1 Business rules implemented in the CDS layer"));
c.push(BULLET("Products: 722000001 Oil, 722000003 Condensate, 722000004 Gas, 722000005 LNG. Oil family in BOPD; gas converted to MMSCMD (MCM as is, MCF ÷ 35.3, M3 ÷ 1,000,000) and to BOEPD with factor 6290."));
c.push(BULLET("Gas has no NET_PROD rows in ZPRA_T_DLY_PRD: net gas = GROSS_PROD − GAS_INJ. Oil family uses NET_PROD. GAS_INJ rows are stored negative so SUM() nets automatically."));
c.push(BULLET("Fiscal year April–March: gjahr = start year, monat = month−3 (Apr–Dec) or month+9 (Jan–Mar). Derived with dats_add_months(date, −3)."));
c.push(BULLET("Business Unit from the asset-code prefix: RUS → BU-RUSSIA; BRA/COL/VEN → BU-LAC; MMR/VNM → BU-ASIA PACIFIC; AZE/SSU/SUD/UAE → BU-MENA CIS; other → OTHER (test data, filtered out)."));
c.push(BULLET("BE targets are read from ZPRA_T_PRD_TAR (tar_code TAR_BE, prod_vl_type_cd NET_PROD, column tar_qty)."));
c.push(SP());
c.push(note("Data prerequisite:", "BE targets for fiscal year 2025-26 are not loaded in ZPRA_T_PRD_TAR. Until they are, target lines and columns show zero for current dates. Test with FY 2024-25, for example 01.04.2024–14.04.2024, fiscal year 2024.", "FFF2CC"));
c.push(PB());

/* ── 2 Inventory ───────────────────────────────────────────────────────── */
c.push(H1("2. Object Inventory"));
c.push(P("All objects are in package ZPR_DPR_RAP. Sources are in section 10. External dependencies: tables ZPRA_T_DLY_PRD, ZPRA_T_MREC_PRD, ZPRA_T_PRD_TAR, ZPRA_T_PRD_PI, ZOIU_PR_DN, view ZPRA_C_DPR_PROF, the abap2xlsx library (ZCL_EXCEL), and optionally Adobe forms ZDPR_FRM_PRODUCTION / ZDPR_FRM_TARGETS for PDF output."));
c.push(table(
  ["Object", "Type", "Role", "How it gets into the system"],
  [
    ["ZDPR_I_DAILY, ZDPR_I_MONTHLY, ZDPR_I_TARGET", "CDS view entities", "Interface views on the tables", "abapGit ZIP"],
    ["ZDPR_P_DAY_BASE", "CDS view entity", "Unit conversion, signed gas, BU, fiscal period, PI %", "abapGit ZIP"],
    ["ZDPR_C_PROD_CUBE, ZDPR_C_BOEPD_DAY, ZDPR_C_TARGET_CUBE", "Analytical cubes", "Data behind the queries (all joins live here)", "abapGit ZIP"],
    ["ZDPR_P_PERF_AGG", "CDS view entity (union)", "YTD / Annual aggregates for tab 3", "abapGit ZIP"],
    ["ZDPR_Q_BOEPD_TREND, ZDPR_Q_DAILY_TREND, ZDPR_Q_PROD_QUERY, ZDPR_Q_TARGET_QUERY", "Classic views, @Analytics.query, @OData.publish", "Analytical queries → OData V2 services ZDPR_Q_*_CDS", "abapGit ZIP + register service (section 5)"],
    ["ZDPR_Q_PROD_PERF", "Classic view, @OData.publish (also in V4 service)", "Tab-3 Production Performance → ZDPR_Q_PROD_PERF_CDS", "abapGit ZIP + register service"],
    ["ZDPR_A_PROD_PARAM, ZDPR_A_TAR_PARAM, ZDPR_A_EXCEL_RESULT, ZDPR_A_PDF_RESULT", "Abstract entities", "Action parameters / results", "abapGit ZIP"],
    ["ZDPR_I_EXCEL_DL + behavior definition; ZBP_ZDPR_EXCEL_DL, ZCL_ZDPR_EXCEL, ZCL_ZDPR_PDF", "Root view, BDEF, classes", "Excel / PDF download actions", "abapGit ZIP"],
    ["ZDPR_SD_ANALYTICS", "Service definition", "OData V4 service (performance query, cubes, interface views, download entity)", "abapGit ZIP"],
    ["ZDPR_SB_ANALYTICS_O4", "Service binding", "OData V4 – UI endpoint", "MANUAL in ADT (section 4.1)"],
    ["ZDPR_Q_*  metadata extensions (5)", "DDLX", "Charts, tables, filters for the cards", "MANUAL in ADT (section 4.2)"],
  ],
  [3000, 2000, 2560, 1800]
));
c.push(PB());

/* ── 3 Import ──────────────────────────────────────────────────────────── */
c.push(H1("3. Importing the Package (abapGit)"));
c.push(P("The ZIP deploy/ZPR_DPR_RAP_abapgit.zip on the Git branch contains every object except the service binding and the metadata extensions, which are created by hand. Whenever a new ZIP is provided, repeat these steps; only changed objects are pulled."));
c.push(STEP("Run ZABAPGIT_STANDALONE (SE38). The offline repository “ZPR_DPR_RAP” already exists; open it."));
c.push(STEP("Import ZIP → select the ZIP file."));
c.push(STEP("Pull. Tick every object shown as new or changed."));
c.push(STEP("Do NOT tick objects proposed for deletion: the service binding ZDPR_SB_ANALYTICS_O4 and the five metadata extensions are not in the ZIP on purpose. Unticking them keeps the manual work."));
c.push(STEP("Confirm the transport request and activate all objects together when prompted."));
c.push(SP());
c.push(P("Warnings that are expected and can be ignored: “CAST from INT1 … loss of data possible”, “Entities subject to access control should declare a key”, “behavior definition should be flagged as strict”, “key field REQUESTID should be flagged as readonly”, “SUBC of the TRDIR entry”.", { italics: true }));
c.push(PB());

/* ── 4 Manual objects ──────────────────────────────────────────────────── */
c.push(H1("4. Objects Created Manually in ADT"));
c.push(H2("4.1 Service binding ZDPR_SB_ANALYTICS_O4"));
c.push(STEP("In ADT, right-click the service definition ZDPR_SD_ANALYTICS → New Service Binding."));
c.push(STEP("Name ZDPR_SB_ANALYTICS_O4, description “DPR Analytics - OData V4 Service Binding”, Binding Type OData V4 – UI, Service Definition ZDPR_SD_ANALYTICS. Next → transport → Finish."));
c.push(STEP("Activate (Ctrl+F3), then press Publish in the Service Version panel and wait for the confirmation."));
c.push(P("Service URL: /sap/opu/odata4/sap/zdpr_analytics/srvd/sap/zdpr_sd_analytics/0001/", { italics: true }));
c.push(H2("4.2 Metadata extensions (5)"));
c.push(P("A metadata extension carries the same name as the view it annotates; ADT keeps them apart by object type. The base views already have @Metadata.allowExtensions: true. Procedure for each of the five:"));
c.push(STEP("Right-click package ZPR_DPR_RAP → New → Other ABAP Repository Object → Core Data Services → Metadata Extension."));
c.push(STEP("Name exactly as in the heading of section 10.9 (for example ZDPR_Q_BOEPD_TREND); description; transport; template “Define Metadata Extension”; Finish."));
c.push(STEP("Select all in the editor, delete, paste the complete source from section 10.9, save, activate (Ctrl+F3)."));
c.push(STEP("If ADT underlines a line as unsupported on this release, delete only that line and activate again — each annotation is independent. Keep at least @UI.chart and @UI.presentationVariant; they drive the dashboard cards."));
c.push(PB());

/* ── 5 V2 registration ─────────────────────────────────────────────────── */
c.push(H1("5. Registering the OData V2 Services"));
c.push(P("The five query views carry @OData.publish: true, so the system generates an OData V2 service for each when the view is activated. On this release analytical queries cannot be exposed through a V4 service definition, which is why the dashboard runs on V2. Register each service once:"));
c.push(table(
  ["CDS view", "OData V2 service", "Dashboard card"],
  [
    ["ZDPR_Q_BOEPD_TREND", "ZDPR_Q_BOEPD_TREND_CDS", "Actual vs BE Target line chart (Excel tab 2)"],
    ["ZDPR_Q_PROD_PERF", "ZDPR_Q_PROD_PERF_CDS", "Production Performance table (Excel tab 3); also global filter"],
    ["ZDPR_Q_DAILY_TREND", "ZDPR_Q_DAILY_TREND_CDS", "Daily production trend"],
    ["ZDPR_Q_TARGET_QUERY", "ZDPR_Q_TARGET_QUERY_CDS", "Target vs actual by product"],
    ["ZDPR_Q_PROD_QUERY", "ZDPR_Q_PROD_QUERY_CDS", "Production records"],
  ],
  [2600, 2900, 3860]
));
c.push(SP());
c.push(STEP("Transaction /IWFND/MAINT_SERVICE → Add Service."));
c.push(STEP("System Alias LOCAL (or the alias Basis has configured), Technical Service Name = the service from the table → Get Services."));
c.push(STEP("Select the service → Add Selected Services → package ZPR_DPR_RAP, transport request → confirm. Repeat for all five."));
c.push(STEP("Test each one with the SAP Gateway Client or a browser inside the network:"));
c.push(...code("https://<s4-host>:<port>/sap/opu/odata/sap/ZDPR_Q_BOEPD_TREND_CDS/$metadata?sap-client=<client>"));
c.push(SP());
c.push(P("The metadata must contain the parameters (P_DateFrom, P_DateTo …) and UI annotations (search for UI.Chart). If UI.Chart is missing, the metadata extension of that query is not active yet.", { italics: true }));
c.push(PB());

/* ── 6 Verification ────────────────────────────────────────────────────── */
c.push(H1("6. Verifying the Backend Before Touching BTP"));
c.push(STEP("ADT → ZDPR_Q_BOEPD_TREND → Open With → Data Preview → parameters 01.04.2024 / 14.04.2024. Rows with ActualBoepdOvl and TargetBoepd must appear."));
c.push(STEP("ADT → ZDPR_Q_PROD_PERF → Data Preview → same dates, fiscal year 2024. Four rows: YTD/ANNUAL × OIL/GAS."));
c.push(STEP("Service binding preview (V4): entity DPRProductionPerformance shows the same four rows."));
c.push(STEP("Browser: the five V2 $metadata documents of section 5 load."));
c.push(SP());
c.push(note("Stop here if any check fails.", "Nothing in BTP can compensate for a backend problem. Fix it in ADT / Gateway first.", "FFF2CC"));
c.push(PB());

/* ── 7 Connectivity ────────────────────────────────────────────────────── */
c.push(H1("7. BTP Connectivity (Basis / BTP Administrator)"));
c.push(H2("7.1 Cloud Connector"));
c.push(STEP("Cloud Connector connected to the BTP subaccount used for the dashboard."));
c.push(STEP("System mapping for the S/4HANA system: internal host/port, virtual host/port."));
c.push(STEP("Access control of that mapping: expose /sap/opu/odata/ (Path and all sub-paths) for the V2 services and /sap/opu/odata4/ for the V4 service."));
c.push(STEP("Principal propagation for productive use (technical user with basic authentication is acceptable only for the first connectivity test)."));
c.push(H2("7.2 Destination"));
c.push(P("BTP cockpit → subaccount → Connectivity → Destinations → New Destination:"));
c.push(table(
  ["Field", "Value"],
  [
    ["Name", "ZDPR_S4_BACKEND (identical in every subaccount)"],
    ["Type / Proxy Type", "HTTP / OnPremise"],
    ["URL", "http://<virtual-host>:<virtual-port>"],
    ["Authentication", "PrincipalPropagation (BasicAuthentication for the first test only)"],
  ],
  [2400, 6960]
));
c.push(SP());
c.push(P("Additional properties (without them the Fiori generator shows no services):"));
c.push(...code(`sap-client               = <client>
HTML5.DynamicDestination = true
WebIDEEnabled            = true
WebIDEUsage              = odata_abap,odata_gen
WebIDESystem             = <SID>`));
c.push(SP());
c.push(P("Check Connection must succeed. Required BTP entitlements: Cloud Foundry runtime, HTML5 Application Repository, Destination, Connectivity, Authorization & Trust Management, SAP Build Work Zone standard edition, SAP Business Application Studio."));
c.push(PB());

/* ── 8 Dashboard ───────────────────────────────────────────────────────── */
c.push(H1("8. Building the Dashboard (Overview Page)"));
c.push(H2("8.1 Cards"));
c.push(table(
  ["#", "Card", "Card type", "Service / entity set", "Chart qualifier"],
  [
    ["1", "Actual vs BE Target (BOEPD)", "Analytical card – line", "ZDPR_Q_BOEPD_TREND_CDS / ZDPR_Q_BOEPD_TRENDResults", "BoepdVsTarget"],
    ["2", "Production Performance", "Table card", "ZDPR_Q_PROD_PERF_CDS / ZDPR_Q_PROD_PERFResults", "(LineItem)"],
    ["3", "Actual vs Target BOEPD", "Analytical card – column", "ZDPR_Q_PROD_PERF_CDS / ZDPR_Q_PROD_PERFResults", "PerfByGroup"],
    ["4", "Daily Production Trend", "Analytical card – line", "ZDPR_Q_DAILY_TREND_CDS / ZDPR_Q_DAILY_TRENDResults", "TrendLine"],
    ["5", "Target vs Actual by Product", "Analytical card – bar", "ZDPR_Q_TARGET_QUERY_CDS / ZDPR_Q_TARGET_QUERYResults", "ActualVsTarget"],
    ["6", "Production Records", "Table card", "ZDPR_Q_PROD_QUERY_CDS / ZDPR_Q_PROD_QUERYResults", "(LineItem)"],
  ],
  [400, 2000, 1700, 3400, 1860]
));
c.push(SP());
c.push(P("Cards 1 and 2 are mandatory (the Excel graph and performance table); 3–6 are optional and need no backend change.", { italics: true }));
c.push(H2("8.2 Global filter bar"));
c.push(P("One filter bar filters the whole page. Parameters are matched to cards by name, which is why the queries share parameter names:"));
c.push(table(
  ["Filter field", "Feeds cards", "Test value"],
  [["P_DateFrom / P_DateTo", "1, 2, 3, 4, 6", "01.04.2024 – 14.04.2024"], ["P_FiscalYear", "2, 3, 5", "2024"], ["P_TargetCode", "5", "TAR_BE (card default)"]],
  [2800, 2200, 4360]
));
c.push(H2("8.3 Generate the application"));
c.push(STEP("Business Application Studio → Dev Space of type SAP Fiori → New Project from Template → SAP Fiori generator."));
c.push(STEP("Template: Overview Page."));
c.push(STEP("Data source: Connect to a System → destination ZDPR_S4_BACKEND → service ZDPR_Q_PROD_PERF_CDS (becomes the default model)."));
c.push(STEP("Filter entity: ZDPR_Q_PROD_PERF (the parameter entity set; its parameters become the global filter fields)."));
c.push(STEP("Module name zdprdashboard, title “DPR Production Dashboard”. Add deployment configuration (Cloud Foundry, destination ZDPR_S4_BACKEND) and FLP configuration (semantic object DPRDashboard, action display). Finish."));
c.push(H2("8.4 Add the other services and the cards"));
c.push(P("Add the remaining services as data sources (Fiori tools: Application Info → Add Data Source) with model names boepd, daily, target, prod. Resulting manifest section:"));
c.push(...code(`"sap.app": { "dataSources": {
  "mainService": { "uri": "/sap/opu/odata/sap/ZDPR_Q_PROD_PERF_CDS/",   "type": "OData", "settings": { "odataVersion": "2.0" } },
  "boepd":       { "uri": "/sap/opu/odata/sap/ZDPR_Q_BOEPD_TREND_CDS/", "type": "OData", "settings": { "odataVersion": "2.0" } },
  "daily":       { "uri": "/sap/opu/odata/sap/ZDPR_Q_DAILY_TREND_CDS/", "type": "OData", "settings": { "odataVersion": "2.0" } },
  "target":      { "uri": "/sap/opu/odata/sap/ZDPR_Q_TARGET_QUERY_CDS/","type": "OData", "settings": { "odataVersion": "2.0" } },
  "prod":        { "uri": "/sap/opu/odata/sap/ZDPR_Q_PROD_QUERY_CDS/",  "type": "OData", "settings": { "odataVersion": "2.0" } }
}},
"sap.ui5": { "models": {
  "":       { "dataSource": "mainService", "preload": true, "settings": { "defaultCountMode": "Inline" } },
  "boepd":  { "dataSource": "boepd",  "preload": true },
  "daily":  { "dataSource": "daily",  "preload": true },
  "target": { "dataSource": "target", "preload": true },
  "prod":   { "dataSource": "prod",   "preload": true }
}}`, 15));
c.push(SP());
c.push(P("Cards (Page Map → Add Card, or directly under sap.ovp in manifest.json):"));
c.push(...code(`"sap.ovp": {
  "globalFilterModel": "",
  "globalFilterEntityType": "ZDPR_Q_PROD_PERFType",
  "containerLayout": "resizable",
  "enableLiveFilter": true,
  "cards": {
    "card01_boepdTrend": {
      "model": "boepd", "template": "sap.ovp.cards.charts.analytical",
      "settings": { "title": "Actual Production vs BE Target (BOEPD)",
        "entitySet": "ZDPR_Q_BOEPD_TRENDResults",
        "chartAnnotationPath": "com.sap.vocabularies.UI.v1.Chart#BoepdVsTarget",
        "selectionAnnotationPath": "com.sap.vocabularies.UI.v1.SelectionVariant#Params",
        "defaultSpan": { "rows": 20, "cols": 2 } } },
    "card02_prodPerf": {
      "model": "", "template": "sap.ovp.cards.table",
      "settings": { "title": "Production Performance", "subTitle": "YTD and Annual vs BE Target",
        "entitySet": "ZDPR_Q_PROD_PERFResults",
        "annotationPath": "com.sap.vocabularies.UI.v1.LineItem",
        "selectionAnnotationPath": "com.sap.vocabularies.UI.v1.SelectionVariant#Params" } },
    "card05_targetVsActual": {
      "model": "target", "template": "sap.ovp.cards.charts.analytical",
      "settings": { "title": "Target vs Actual by Product",
        "entitySet": "ZDPR_Q_TARGET_QUERYResults",
        "chartAnnotationPath": "com.sap.vocabularies.UI.v1.Chart#ActualVsTarget",
        "selectionAnnotationPath": "com.sap.vocabularies.UI.v1.SelectionVariant#Params" } }
  }
}`, 15));
c.push(SP());
c.push(note("Entity set for cards:", "An Overview Page card binds to the …Results entity set and receives the parameters through a UI.SelectionVariant annotation with a Parameters section (SAP's documented pattern for parameterised cards). This is the opposite of an Analytical List Page, which binds to the parameter entity set.", "FFF2CC"));
c.push(SP());
c.push(P("SelectionVariant#Params per card in webapp/annotations/annotation.xml (Annotation Modeler or paste). Example for the BOEPD card; the global filter overrides the values at runtime:"));
c.push(...code(`<Annotations Target="ZDPR_Q_BOEPD_TREND_CDS.ZDPR_Q_BOEPD_TRENDType">
  <Annotation Term="UI.SelectionVariant" Qualifier="Params">
    <Record><PropertyValue Property="Parameters"><Collection>
      <Record Type="UI.Parameter">
        <PropertyValue Property="PropertyName" PropertyPath="P_DateFrom"/>
        <PropertyValue Property="PropertyValue" String="20240401"/></Record>
      <Record Type="UI.Parameter">
        <PropertyValue Property="PropertyName" PropertyPath="P_DateTo"/>
        <PropertyValue Property="PropertyValue" String="20240414"/></Record>
    </Collection></PropertyValue></Record>
  </Annotation>
</Annotations>`, 15));
c.push(SP());
c.push(P("Target card: P_FiscalYear = 2024, P_TargetCode = TAR_BE. Performance cards: P_DateFrom, P_DateTo, P_FiscalYear.", { italics: true }));
c.push(H2("8.5 Test, deploy, tile"));
c.push(STEP("Preview Application. Global filter 01.04.2024–14.04.2024, fiscal year 2024. Card 1 shows two lines; card 2 shows YTD/Annual × Oil/Gas."));
c.push(STEP("Right-click mta.yaml → Build MTA Project → right-click the .mtar → Deploy MTA Archive → Cloud Foundry org/space."));
c.push(STEP("SAP Build Work Zone: Channel Manager → refresh HTML5 Apps; Content Manager → group “DPR Reporting” + role → add the app; Site → assign group and role; cockpit → assign the role collection to users."));
c.push(STEP("Open the site: one tile — DPR Production Dashboard."));
c.push(PB());

/* ── 9 Deployment ──────────────────────────────────────────────────────── */
c.push(H1("9. Transport to Quality and Production"));
c.push(H2("9.1 Transport request content"));
c.push(table(
  ["Type", "Count", "Objects"],
  [
    ["R3TR DEVC", "1", "ZPR_DPR_RAP"],
    ["R3TR DDLS", "18", "All CDS views"],
    ["R3TR DDLX", "5", "Metadata extensions"],
    ["R3TR BDEF / CLAS", "1 / 3", "ZDPR_I_EXCEL_DL; ZBP_ZDPR_EXCEL_DL, ZCL_ZDPR_EXCEL, ZCL_ZDPR_PDF"],
    ["R3TR SRVD / SRVB", "1 / 1", "ZDPR_SD_ANALYTICS; ZDPR_SB_ANALYTICS_O4 (often forgotten — check)"],
    ["R3TR IWSV / IWSG / IWMO", "5 each", "Gateway registrations of ZDPR_Q_*_CDS"],
  ],
  [2600, 1000, 5760]
));
c.push(SP());
c.push(STEP("SE10: release tasks, then the request. Basis imports into Quality (STMS); return code 0 or 4 expected (4 = the known warnings)."));
c.push(STEP("Post-import: ADT → package has no error markers; Data Preview on ZDPR_Q_BOEPD_TREND; service binding shows Published (else Publish); /IWFND/MAINT_SERVICE lists the five V2 services (add them if the registration did not travel)."));
c.push(STEP("BTP: same .mtar deployed to the Quality/Production subaccount; only the destination URL differs. Work Zone content configured per site."));
c.push(STEP("Business validation against the DPR Excel workbook, then Production."));
c.push(H2("9.2 Authorisations"));
c.push(P("The CDS views use @AccessControl.authorizationCheck: #NOT_REQUIRED — no row-level restriction; anyone reaching the service sees all business units. If restriction is required, a DCL access control on ZDPR_P_DAY_BASE is a separate change. Users need S_SERVICE for the OData services and the Work Zone role collection."));
c.push(H2("9.3 Troubleshooting"));
c.push(table(
  ["Symptom", "Cause", "Fix"],
  [
    ["Fiori generator shows no services", "WebIDE* properties missing on the destination", "Section 7.2"],
    ["Service 404 from BTP", "Path not exposed in Cloud Connector or service not registered", "7.1 / section 5"],
    ["401 / 403", "Principal propagation incomplete or S_SERVICE missing", "Basis / Security"],
    ["Card “Cannot load card”", "Model name mismatch in manifest or service unregistered", "8.4 / section 5"],
    ["Card empty", "SelectionVariant#Params missing or parameter names differ", "8.4"],
    ["Chart card without chart", "Metadata extension not active or wrong chart qualifier", "4.2 / 8.1"],
    ["Target line flat at zero", "FY 2025-26 targets not loaded", "Use FY 2024-25; load data"],
    ["Tile missing", "Content channel not refreshed or role not assigned", "8.5"],
  ],
  [2900, 3500, 2960]
));
c.push(PB());

/* ── 10 Sources ────────────────────────────────────────────────────────── */
c.push(H1("10. Complete Source Code"));
c.push(P("Every object, in creation order. To create an object by hand: create it in ADT with the exact name and type, delete the skeleton, paste the block in full, activate. The ZBP_ZDPR_EXCEL_DL block contains the global class (main source) followed by the local handler class starting at CLASS lhc_dpr_excel (Local Types include)."));
const SECTIONS = [
  ["10.1 Interface views", [["ZDPR_I_DAILY","ZDPR_I_DAILY.ddls.asddls"],["ZDPR_I_MONTHLY","ZDPR_I_MONTHLY.ddls.asddls"],["ZDPR_I_TARGET","ZDPR_I_TARGET.ddls.asddls"]]],
  ["10.2 Base and aggregation layer", [["ZDPR_P_DAY_BASE","ZDPR_P_DAY_BASE.ddls.asddls"],["ZDPR_P_PERF_AGG","ZDPR_P_PERF_AGG.ddls.asddls"]]],
  ["10.3 Analytical cubes", [["ZDPR_C_PROD_CUBE","ZDPR_C_PROD_CUBE.ddls.asddls"],["ZDPR_C_BOEPD_DAY","ZDPR_C_BOEPD_DAY.ddls.asddls"],["ZDPR_C_TARGET_CUBE","ZDPR_C_TARGET_CUBE.ddls.asddls"]]],
  ["10.4 Analytical queries (OData V2)", [["ZDPR_Q_BOEPD_TREND","ZDPR_Q_BOEPD_TREND.ddls.asddls"],["ZDPR_Q_DAILY_TREND","ZDPR_Q_DAILY_TREND.ddls.asddls"],["ZDPR_Q_PROD_QUERY","ZDPR_Q_PROD_QUERY.ddls.asddls"],["ZDPR_Q_TARGET_QUERY","ZDPR_Q_TARGET_QUERY.ddls.asddls"]]],
  ["10.5 Production Performance query (OData V2 and V4)", [["ZDPR_Q_PROD_PERF","ZDPR_Q_PROD_PERF.ddls.asddls"]]],
  ["10.6 Download — abstract entities", [["ZDPR_A_PROD_PARAM","ZDPR_A_PROD_PARAM.ddls.asddls"],["ZDPR_A_TAR_PARAM","ZDPR_A_TAR_PARAM.ddls.asddls"],["ZDPR_A_EXCEL_RESULT","ZDPR_A_EXCEL_RESULT.ddls.asddls"],["ZDPR_A_PDF_RESULT","ZDPR_A_PDF_RESULT.ddls.asddls"]]],
  ["10.7 Download — entity, behavior, classes", [["ZDPR_I_EXCEL_DL (root view)","ZDPR_I_EXCEL_DL.ddls.asddls"],["ZDPR_I_EXCEL_DL (behavior definition)","ZDPR_I_EXCEL_DL.bdef.asbdef"],["ZBP_ZDPR_EXCEL_DL","ZBP_ZDPR_EXCEL_DL.clas.abap"],["ZCL_ZDPR_EXCEL","ZCL_ZDPR_EXCEL.clas.abap"],["ZCL_ZDPR_PDF","ZCL_ZDPR_PDF.clas.abap"]]],
  ["10.8 Service definition and binding", [["ZDPR_SD_ANALYTICS","ZDPR_SD_ANALYTICS.srvd.asddls"],["ZDPR_SB_ANALYTICS_O4 (reference — create via wizard)","ZDPR_SB_ANALYTICS_O4.srvb.asddls"]]],
  ["10.9 Metadata extensions (create in ADT)", [["ZDPR_Q_BOEPD_TREND","ZDPR_Q_BOEPD_TREND.ddlx.asddlx"],["ZDPR_Q_PROD_PERF","ZDPR_Q_PROD_PERF.ddlx.asddlx"],["ZDPR_Q_DAILY_TREND","ZDPR_Q_DAILY_TREND.ddlx.asddlx"],["ZDPR_Q_PROD_QUERY","ZDPR_Q_PROD_QUERY.ddlx.asddlx"],["ZDPR_Q_TARGET_QUERY","ZDPR_Q_TARGET_QUERY.ddlx.asddlx"]]],
];
for (const [title, objs] of SECTIONS) {
  c.push(H2(title));
  for (const [name, file] of objs) {
    c.push(H3(name));
    c.push(P(`src/rap/${file}`, { italics: true }));
    c.push(...code(read(file), 15));
    c.push(SP());
  }
}

const doc = new Document({
  creator: "ONGC Videsh DPR Project",
  title: "DPR Production Dashboard — Developer Guide",
  styles: { default: {
    document: { run: { font: "Calibri", size: 21 }, paragraph: { spacing: { line: 276 } } },
    heading1: { run: { font: "Calibri Light", size: 32, bold: true, color: BLUE } },
    heading2: { run: { font: "Calibri Light", size: 26, bold: true, color: "2E74B5" } },
    heading3: { run: { font: "Calibri Light", size: 23, bold: true, color: "404040" } } } },
  numbering: { config: [
    { reference: "bullets", levels: [{ level: 0, format: LevelFormat.BULLET, text: "•", alignment: AlignmentType.LEFT,
      style: { paragraph: { indent: { left: convertInchesToTwip(0.3), hanging: convertInchesToTwip(0.2) } } } }] },
    { reference: "steps", levels: [{ level: 0, format: LevelFormat.DECIMAL, text: "%1.", alignment: AlignmentType.LEFT,
      style: { paragraph: { indent: { left: convertInchesToTwip(0.35), hanging: convertInchesToTwip(0.25) } } } }] } ] },
  features: { updateFields: true },
  sections: [{
    properties: { page: { size: { width: 12240, height: 15840 }, margin: { top: 1080, bottom: 1080, left: 1080, right: 1080 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER, children: [
      new TextRun({ text: "DPR Production Dashboard — Developer Guide     Page ", size: 16, color: "808080" }),
      new TextRun({ children: [PageNumber.CURRENT], size: 16, color: "808080" }),
      new TextRun({ text: " of ", size: 16, color: "808080" }),
      new TextRun({ children: [PageNumber.TOTAL_PAGES], size: 16, color: "808080" }) ] })] }) },
    children: c }],
});
Packer.toBuffer(doc).then((b) => { fs.writeFileSync(OUT, b); console.log("written", OUT, b.length); });
