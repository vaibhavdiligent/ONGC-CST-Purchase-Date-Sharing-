const fs = require("fs");
const path = require("path");
const {
  Document, Packer, Paragraph, TextRun, HeadingLevel, AlignmentType,
  Table, TableRow, TableCell, WidthType, ShadingType, BorderStyle,
  PageBreak, TableOfContents, LevelFormat, Footer, PageNumber, convertInchesToTwip,
} = require("docx");

const OUT = path.join(__dirname, "ZDPR_RAP_Single_Dashboard_OVP.docx");
const BLUE = "1F4E79", GREY = "F2F2F2", CODEBG = "F7F7F7";
const H1 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_1, spacing: { before: 320, after: 160 } });
const H2 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_2, spacing: { before: 260, after: 120 } });
const P = (t, o = {}) => new Paragraph({ spacing: { after: 120 },
  children: [new TextRun({ text: t, size: 21, bold: !!o.bold, italics: !!o.italics })] });
const BULLET = (t) => new Paragraph({ numbering: { reference: "bullets", level: 0 }, spacing: { after: 80 },
  children: [new TextRun({ text: t, size: 21 })] });
const STEP = (t) => new Paragraph({ numbering: { reference: "steps", level: 0 }, spacing: { after: 100 },
  children: [new TextRun({ text: t, size: 21 })] });
const PB = () => new Paragraph({ children: [new PageBreak()] });
const SP = () => new Paragraph({ spacing: { after: 140 } });
function code(text) {
  const lines = text.split("\n"); while (lines.length && !lines[lines.length - 1].trim()) lines.pop();
  return lines.map((l) => new Paragraph({ spacing: { after: 0, line: 240 },
    shading: { type: ShadingType.CLEAR, fill: CODEBG }, indent: { left: 120, right: 120 },
    border: { left: { style: BorderStyle.SINGLE, size: 12, color: "8EA9DB", space: 6 } },
    children: [new TextRun({ text: l || " ", font: "Consolas", size: 16 })] }));
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
    shading: { type: ShadingType.CLEAR, fill }, margins: { top: 70, bottom: 70, left: 110, right: 110 },
    children: [new Paragraph({ spacing: { after: 0 },
      children: [new TextRun({ text: txt, bold, size: 19, color: bold ? "FFFFFF" : undefined })] })] });
  return new Table({ width: { size: total, type: WidthType.DXA }, columnWidths: widths, rows: [
    new TableRow({ tableHeader: true, children: headers.map((h, i) => mk(h, true, BLUE, widths[i])) }),
    ...rows.map((r, ri) => new TableRow({ children: r.map((c, i) => mk(c, false, ri % 2 ? GREY : "FFFFFF", widths[i])) })) ] });
}

const c = [];
c.push(
  new Paragraph({ spacing: { before: 2200 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "ONGC Videsh — DPR Analytical RAP", bold: true, size: 44, color: BLUE })] }),
  new Paragraph({ spacing: { before: 160 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "One Dashboard: the DPR Overview Page", bold: true, size: 32, color: "404040" })] }),
  new Paragraph({ spacing: { before: 500 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "A single Fiori tile showing the graph, the performance table and the detail cards together", size: 22, italics: true, color: "606060" })] }),
  PB()
);
c.push(H1("Contents"));
c.push(new TableOfContents("Contents", { hyperlink: true, headingStyleRange: "1-2" }));
c.push(PB());

/* 1 */
c.push(H1("1. Why an Overview Page"));
c.push(P("The requirement is one dashboard, not four separate applications. In SAP Fiori elements the floorplan built for exactly this is the Overview Page (OVP): a single application, opened from a single tile, that shows several cards side by side. Each card is bound to its own OData entity set — so the tab-2 graph, the tab-3 performance table, the daily trend and the target-vs-actual view all live on one page, filtered together by one global filter bar at the top."));
c.push(P("Nothing in the backend logic changes for this. The same analytical queries that were built for the individual apps become the cards of the Overview Page. One backend adjustment was required: the Production Performance query (ZDPR_Q_PROD_PERF) is now also published as an OData V2 service, because an Overview Page works on one protocol and the other queries are on OData V2."));
c.push(SP());
c.push(note("What the user sees:", "One tile “DPR Production Dashboard”. Opening it shows a filter bar (production date range, fiscal year, target code) and, below it, cards: the Actual-vs-BE-Target line chart, the Production Performance table with % achievement, a daily trend chart, a target-vs-actual bar chart, and KPI tiles for the headline figures. Clicking a card can navigate to the detailed Analytical List Page of that query if those apps are also deployed.", "E2EFDA"));
c.push(PB());

/* 2 */
c.push(H1("2. Dashboard Design"));
c.push(H2("2.1 Cards"));
c.push(table(
  ["#", "Card", "Card type", "Service / entity set", "Content"],
  [
    ["1", "Actual vs BE Target (BOEPD)", "Analytical card — line chart", "ZDPR_Q_BOEPD_TREND_CDS / ZDPR_Q_BOEPD_TRENDResults", "The Excel tab-2 graph: two lines over production date"],
    ["2", "Production Performance", "Table card", "ZDPR_Q_PROD_PERF_CDS / ZDPR_Q_PROD_PERFResults", "Excel tab 3: YTD and Annual rows × Oil / Gas with % achievement and colour criticality"],
    ["3", "Total O+OEG (BOEPD)", "KPI / analytical card — column", "ZDPR_Q_PROD_PERF_CDS", "Headline actual vs target BOEPD"],
    ["4", "Daily Production Trend", "Analytical card — line chart", "ZDPR_Q_DAILY_TREND_CDS / ZDPR_Q_DAILY_TRENDResults", "JV and OVL production by date and product"],
    ["5", "Target vs Actual by Product", "Analytical card — bar chart", "ZDPR_Q_TARGET_QUERY_CDS / ZDPR_Q_TARGET_QUERYResults", "Monthly actual vs target per product with Achievement %"],
    ["6", "Production Records", "Table / list card", "ZDPR_Q_PROD_QUERY_CDS / ZDPR_Q_PROD_QUERYResults", "Latest daily records by asset"],
  ],
  [400, 2000, 1900, 2800, 2260]
));
c.push(SP());
c.push(P("Cards 1 and 2 are the customer's two must-haves (the Excel graph and the performance table). Cards 3–6 are optional and can be added later without touching the backend.", { italics: true }));

c.push(H2("2.2 Global filter bar and parameters"));
c.push(P("The Overview Page has one filter bar for the whole page. The parameters of the queries are mapped to it by name, which is why the parameters were given identical names across the queries:"));
c.push(table(
  ["Filter field", "Feeds parameter of", "Default for testing"],
  [
    ["Production date from / to (P_DateFrom, P_DateTo)", "Cards 1, 2, 3, 4, 6", "01.04.2024 – 14.04.2024 (FY 2024-25 has targets loaded)"],
    ["Fiscal year (P_FiscalYear)", "Cards 2, 3, 5", "2024"],
    ["Target code (P_TargetCode)", "Card 5", "TAR_BE"],
  ],
  [3600, 2400, 3360]
));
c.push(SP());
c.push(P("Technically the global filter entity of the page is the parameter entity set of one of the queries (ZDPR_Q_PROD_PERF, which carries three of the four parameters). The fourth parameter, P_TargetCode, is supplied to card 5 through a SelectionVariant annotation with a parameter default (see 4.3), which the user can still change through the card's own filter."));
c.push(PB());

/* 3 backend */
c.push(H1("3. Backend Change (already in the repository)"));
c.push(P("ZDPR_Q_PROD_PERF was a view entity exposed only through the OData V4 service definition. It is now a classic DDIC-based view with @OData.publish: true, like the four analytical queries, generating the OData V2 service ZDPR_Q_PROD_PERF_CDS. The three ratio calculations use division( a, b, decimals ) because classic views allow the '/' operator for floats only. The V4 exposure in ZDPR_SD_ANALYTICS is unchanged."));
c.push(STEP("Pull the updated ZIP (deploy/ZPR_DPR_RAP_abapgit.zip) in ZABAPGIT_STANDALONE; one object changes: ZDPR_Q_PROD_PERF. Activate."));
c.push(STEP("Register the new service in /IWFND/MAINT_SERVICE: Add Service → alias LOCAL → ZDPR_Q_PROD_PERF_CDS → Add Selected Services → package ZPR_DPR_RAP."));
c.push(STEP("Confirm all five V2 services answer:"));
c.push(...code(`/sap/opu/odata/sap/ZDPR_Q_BOEPD_TREND_CDS/$metadata
/sap/opu/odata/sap/ZDPR_Q_PROD_PERF_CDS/$metadata
/sap/opu/odata/sap/ZDPR_Q_DAILY_TREND_CDS/$metadata
/sap/opu/odata/sap/ZDPR_Q_TARGET_QUERY_CDS/$metadata
/sap/opu/odata/sap/ZDPR_Q_PROD_QUERY_CDS/$metadata`));
c.push(SP());
c.push(STEP("The metadata extension ZDPR_Q_PROD_PERF (unchanged) must be active — it provides the table columns and the chart for cards 2 and 3."));
c.push(PB());

/* 4 generator */
c.push(H1("4. Building the Overview Page in Business Application Studio"));
c.push(P("Prerequisites are the same as for any BTP deployment: Cloud Connector exposing /sap/opu/odata/, and the destination ZDPR_S4_BACKEND with the WebIDE properties (see the BTP guide). The steps below replace Step 3 of the Fiori generator walkthrough."));
c.push(H2("4.1 Generate the application"));
c.push(STEP("Dev Space of type SAP Fiori → New Project from Template → SAP Fiori generator."));
c.push(STEP("Template: Overview Page."));
c.push(STEP("Data source: Connect to a System → destination ZDPR_S4_BACKEND → service ZDPR_Q_PROD_PERF_CDS. This becomes the main (default) OData model of the page."));
c.push(STEP("Filter entity: ZDPR_Q_PROD_PERF (the parameter entity set). Its parameters P_DateFrom, P_DateTo and P_FiscalYear become the global filter fields."));
c.push(STEP("Project attributes: module name zdprdashboard, title DPR Production Dashboard. Add deployment configuration (Cloud Foundry, destination ZDPR_S4_BACKEND) and FLP configuration (semantic object DPRDashboard, action display)."));
c.push(STEP("Finish. The generator creates the app with an empty card list."));
c.push(H2("4.2 Add the other services as additional data sources"));
c.push(P("Each card that uses a different service needs that service registered as a data source and model in manifest.json. In Business Application Studio use Application Info → Add Data Source (or Fiori tools: Application Modeler → Data Sources → Add), and add:"));
c.push(...code(`ZDPR_Q_BOEPD_TREND_CDS   -> model name  boepd
ZDPR_Q_DAILY_TREND_CDS   -> model name  daily
ZDPR_Q_TARGET_QUERY_CDS  -> model name  target
ZDPR_Q_PROD_QUERY_CDS    -> model name  prod`));
c.push(SP());
c.push(P("The resulting manifest section looks like this (paths as generated by the tool, destination routing handled by xs-app.json):"));
c.push(...code(`"sap.app": {
  "dataSources": {
    "mainService": { "uri": "/sap/opu/odata/sap/ZDPR_Q_PROD_PERF_CDS/",  "type": "OData",
                     "settings": { "odataVersion": "2.0", "localUri": "localService/metadata.xml" } },
    "boepd":       { "uri": "/sap/opu/odata/sap/ZDPR_Q_BOEPD_TREND_CDS/", "type": "OData",
                     "settings": { "odataVersion": "2.0", "localUri": "localService/boepd/metadata.xml" } },
    "daily":       { "uri": "/sap/opu/odata/sap/ZDPR_Q_DAILY_TREND_CDS/", "type": "OData",
                     "settings": { "odataVersion": "2.0" } },
    "target":      { "uri": "/sap/opu/odata/sap/ZDPR_Q_TARGET_QUERY_CDS/","type": "OData",
                     "settings": { "odataVersion": "2.0" } },
    "prod":        { "uri": "/sap/opu/odata/sap/ZDPR_Q_PROD_QUERY_CDS/",  "type": "OData",
                     "settings": { "odataVersion": "2.0" } }
  }
},
"sap.ui5": {
  "models": {
    "":       { "dataSource": "mainService", "preload": true, "settings": { "defaultCountMode": "Inline" } },
    "boepd":  { "dataSource": "boepd",  "preload": true },
    "daily":  { "dataSource": "daily",  "preload": true },
    "target": { "dataSource": "target", "preload": true },
    "prod":   { "dataSource": "prod",   "preload": true }
  }
}`));
c.push(PB());
c.push(H2("4.3 Define the cards"));
c.push(P("Use the Fiori tools Page Map (Application Modeler → Page Map → Overview Page → Add Card), or add the cards directly under sap.ovp → cards in manifest.json. The annotations they reference (UI.Chart, UI.LineItem, UI.SelectionVariant) already come from the metadata extensions in the backend. Minimal definitions for the two mandatory cards and the target card:"));
c.push(...code(`"sap.ovp": {
  "globalFilterModel": "",
  "globalFilterEntityType": "ZDPR_Q_PROD_PERFType",
  "containerLayout": "resizable",
  "enableLiveFilter": true,
  "cards": {
    "card01_boepdTrend": {
      "model": "boepd",
      "template": "sap.ovp.cards.charts.analytical",
      "settings": {
        "title": "Actual Production vs BE Target (BOEPD)",
        "entitySet": "ZDPR_Q_BOEPD_TRENDResults",
        "chartAnnotationPath": "com.sap.vocabularies.UI.v1.Chart#BoepdVsTarget",
        "selectionAnnotationPath": "com.sap.vocabularies.UI.v1.SelectionVariant#Params",
        "defaultSpan": { "rows": 20, "cols": 2 }
      }
    },
    "card02_prodPerf": {
      "model": "",
      "template": "sap.ovp.cards.table",
      "settings": {
        "title": "Production Performance",
        "subTitle": "YTD and Annual vs BE Target",
        "entitySet": "ZDPR_Q_PROD_PERFResults",
        "annotationPath": "com.sap.vocabularies.UI.v1.LineItem",
        "selectionAnnotationPath": "com.sap.vocabularies.UI.v1.SelectionVariant#Params"
      }
    },
    "card05_targetVsActual": {
      "model": "target",
      "template": "sap.ovp.cards.charts.analytical",
      "settings": {
        "title": "Target vs Actual by Product",
        "entitySet": "ZDPR_Q_TARGET_QUERYResults",
        "chartAnnotationPath": "com.sap.vocabularies.UI.v1.Chart#ActualVsTarget",
        "selectionAnnotationPath": "com.sap.vocabularies.UI.v1.SelectionVariant#Params"
      }
    }
  }
}`));
c.push(SP());
c.push(note("Entity set for cards:",
  "For a parameterised OData V2 entity set, an Overview Page card is bound to the ...Results entity set, and the parameters are passed through a UI.SelectionVariant annotation with a Parameters section. This is SAP's documented pattern for parameterised cards and is the opposite of the Analytical List Page, which is bound to the parameter entity set.",
  "FFF2CC"));
c.push(SP());
c.push(P("The SelectionVariant#Params annotation is declared per card in the application's local annotation file (webapp/annotations/annotation.xml — create it with the Fiori tools Annotation Modeler, or paste). Example for the BOEPD card; the global filter overrides these values at runtime when the user changes the filter bar:"));
c.push(...code(`<Annotations Target="ZDPR_Q_BOEPD_TREND_CDS.ZDPR_Q_BOEPD_TRENDType">
  <Annotation Term="UI.SelectionVariant" Qualifier="Params">
    <Record>
      <PropertyValue Property="Parameters">
        <Collection>
          <Record Type="UI.Parameter">
            <PropertyValue Property="PropertyName" PropertyPath="P_DateFrom"/>
            <PropertyValue Property="PropertyValue" String="20240401"/>
          </Record>
          <Record Type="UI.Parameter">
            <PropertyValue Property="PropertyName" PropertyPath="P_DateTo"/>
            <PropertyValue Property="PropertyValue" String="20240414"/>
          </Record>
        </Collection>
      </PropertyValue>
    </Record>
  </Annotation>
</Annotations>`));
c.push(SP());
c.push(P("For the target card the parameters are P_FiscalYear (default 2024) and P_TargetCode (default TAR_BE); for the performance card P_DateFrom, P_DateTo and P_FiscalYear.", { italics: true }));
c.push(H2("4.4 Test locally, deploy, publish the tile"));
c.push(STEP("Preview Application. Enter the date range 01.04.2024–14.04.2024 and fiscal year 2024 in the global filter. Card 1 must show two lines; card 2 must show the YTD and Annual rows for Oil and Gas."));
c.push(STEP("Build MTA Project → Deploy MTA Archive to the Cloud Foundry space."));
c.push(STEP("SAP Build Work Zone: refresh the HTML5 content channel, add the application to the group DPR Reporting and to the role, assign the role collection to the users."));
c.push(STEP("Open the site: one tile — DPR Production Dashboard."));
c.push(PB());

/* 5 */
c.push(H1("5. Alternative Without Development: SAP Analytics Cloud"));
c.push(P("If an SAP Analytics Cloud tenant is available, the same one-page dashboard can be built without any Fiori development: one SAC story with a page containing the line chart (ZDPR_Q_BOEPD_TREND), the performance table (ZDPR_Q_PROD_PERF is a plain view, so use ZDPR_C_BOEPD_DAY or the query models) and the target chart, all driven by one set of story filters. The BTP deployment guide, section 6, describes the live-connection setup. Choose this route if the dashboard audience is management rather than operations, or if the Fiori development capacity is the bottleneck."));

c.push(H1("6. Troubleshooting"));
c.push(table(
  ["Symptom", "Cause", "Fix"],
  [
    ["Card shows “Cannot load card”", "Model name in the card does not match sap.ui5 → models, or the service is not registered in /IWFND/MAINT_SERVICE", "Section 4.2 / Section 3 step 2"],
    ["Card is empty although the service works", "Parameters not supplied — SelectionVariant#Params missing for that card, or names differ from the global filter fields", "Section 4.3"],
    ["Chart card has no chart definition", "Metadata extension of that query not active, or wrong chart qualifier in chartAnnotationPath", "Activate DDLX; qualifiers: BoepdVsTarget, TrendLine, ActualVsTarget, PerfByGroup, ByDate"],
    ["Global filter fields missing", "globalFilterEntityType not the parameter entity type", "Set to ZDPR_Q_PROD_PERFType"],
    ["Target line at zero", "FY 2025-26 targets not loaded", "Use FY 2024-25 for testing; load data"],
  ],
  [2900, 3500, 2960]
));

const doc = new Document({
  creator: "ONGC Videsh DPR Project",
  title: "DPR Analytical RAP — One Dashboard (Overview Page)",
  styles: { default: {
    document: { run: { font: "Calibri", size: 21 }, paragraph: { spacing: { line: 276 } } },
    heading1: { run: { font: "Calibri Light", size: 32, bold: true, color: BLUE } },
    heading2: { run: { font: "Calibri Light", size: 26, bold: true, color: "2E74B5" } } } },
  numbering: { config: [
    { reference: "bullets", levels: [{ level: 0, format: LevelFormat.BULLET, text: "•", alignment: AlignmentType.LEFT,
      style: { paragraph: { indent: { left: convertInchesToTwip(0.3), hanging: convertInchesToTwip(0.2) } } } }] },
    { reference: "steps", levels: [{ level: 0, format: LevelFormat.DECIMAL, text: "%1.", alignment: AlignmentType.LEFT,
      style: { paragraph: { indent: { left: convertInchesToTwip(0.35), hanging: convertInchesToTwip(0.25) } } } }] } ] },
  features: { updateFields: true },
  sections: [{
    properties: { page: { size: { width: 12240, height: 15840 }, margin: { top: 1080, bottom: 1080, left: 1080, right: 1080 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER, children: [
      new TextRun({ text: "DPR Analytical RAP — One Dashboard (Overview Page)     Page ", size: 16, color: "808080" }),
      new TextRun({ children: [PageNumber.CURRENT], size: 16, color: "808080" }),
      new TextRun({ text: " of ", size: 16, color: "808080" }),
      new TextRun({ children: [PageNumber.TOTAL_PAGES], size: 16, color: "808080" }) ] })] }) },
    children: c }],
});
Packer.toBuffer(doc).then((b) => { fs.writeFileSync(OUT, b); console.log("written", OUT, b.length); });
