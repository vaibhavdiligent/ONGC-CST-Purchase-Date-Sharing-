const fs = require("fs");
const path = require("path");
const {
  Document, Packer, Paragraph, TextRun, HeadingLevel, AlignmentType,
  Table, TableRow, TableCell, WidthType, ShadingType, BorderStyle,
  PageBreak, TableOfContents, LevelFormat, Footer, PageNumber,
} = require("docx");

const ROOT = path.dirname(__dirname);
const OUT = path.join(ROOT, "deploy", "ZDPR_RAP_Dashboard_Navigation_to_Full_Data.docx");
const BLUE = "1F4E79", GREY = "F2F2F2", CODEBG = "F7F7F7";
const H1 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_1, spacing: { before: 320, after: 160 } });
const H2 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_2, spacing: { before: 260, after: 120 } });
const P = (t, o = {}) => new Paragraph({ spacing: { after: 120 },
  children: [new TextRun({ text: t, size: 21, bold: !!o.bold, italics: !!o.italics })] });
const BULLET = (t) => new Paragraph({ numbering: { reference: "bullets", level: 0 }, spacing: { after: 80 },
  children: [new TextRun({ text: t, size: 21 })] });
let stepRef = 0;
const STEPS = () => { stepRef++; return "steps" + stepRef; };
const STEP = (ref, t) => new Paragraph({ numbering: { reference: ref, level: 0 }, spacing: { after: 100 },
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
    children: [new TextRun({ text: "From the Dashboard Card to the Complete Data", bold: true, size: 32, color: "404040" })] }),
  new Paragraph({ spacing: { before: 500 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Card navigation, target apps on SAP BTP, Work Zone setup, and the standard analysis apps — steps for the developer", size: 22, italics: true, color: "606060" })] }),
  PB()
);
c.push(H1("Contents"));
c.push(new TableOfContents("Contents", { hyperlink: true, headingStyleRange: "1-2" }));
c.push(PB());

/* 1 */
c.push(H1("1. What the user sees today and what will change"));
c.push(P("A table card of the Overview Page shows three rows and reports the total, for example “3 of 2460”. That is the design of the card type and cannot be changed. The complete data is reached by navigation: the user clicks the card header (or “View More”) and a full-screen application opens with all rows for the same filter values, with sorting, grouping, personalisation and export to Excel. This document sets that navigation up and lists the two additional ways users can analyse the complete data without waiting for development."));
c.push(table(
  ["Way to the complete data", "What the user gets", "What has to be done"],
  [
    ["A. Card navigation to a target app (sections 2–5)", "Click on the card → Analytical List Page with chart and full table, same filter values, export to Excel", "Generate and deploy one target app per query, register its intent in Work Zone, add the navigation to the cards"],
    ["B. Query Browser / View Browser (section 6)", "Pivot analysis on any of the DPR queries, drill-down, export", "Assign the SAP standard business catalog to the users; no development"],
    ["C. Excel / PDF download (section 7)", "The DPR workbook as a file", "Already built (ZDPR_I_EXCEL_DL); expose a button or tile"],
  ],
  [2900, 3300, 3160]
));
c.push(SP());
c.push(note("Naming used below:", "Semantic object DPRProduction. Actions: analyzeProduction (records, ZDPR_Q_PROD_QUERY), analyzeTrend (ZDPR_Q_BOEPD_TREND), analyzePerformance (ZDPR_Q_PROD_PERF), analyzeTarget (ZDPR_Q_TARGET_QUERY). Any other names work as long as the same value is used in the target app, in Work Zone and in the card.", "FFF2CC"));
c.push(PB());

/* 2 */
c.push(H1("2. Build the target apps (Business Application Studio)"));
c.push(P("One Fiori elements app per query. The Analytical List Page (ALP) is the right floorplan for the analytical queries: chart on top, full table below, filter bar with the parameters. For ZDPR_Q_PROD_PERF a List Report is enough. All apps use the same destination as the dashboard (ZDPR_S4_BACKEND) and the OData V2 services already registered in /IWFND/MAINT_SERVICE."));
let s = STEPS();
c.push(STEP(s, "BAS → Create Project from Template → SAP Fiori generator → Analytical List Page (OData V2)."));
c.push(STEP(s, "Data source: Connect to a System → destination ZDPR_S4_BACKEND → service ZDPR_Q_PROD_QUERY_CDS."));
c.push(STEP(s, "Main entity: the PARAMETER entity set ZDPR_Q_PROD_QUERY (not …Results). The generator binds the filter bar to the parameters P_DateFrom / P_DateTo and the table/chart to the Results set. Qualifier for the chart: ByDate; presentation variant: default."));
c.push(STEP(s, "Project attributes: module name dpr-prod-records, title “DPR Production Records”, namespace ovl.dpr. Tick “Add deployment configuration” (Cloud Foundry, destination ZDPR_S4_BACKEND) and “Add FLP configuration”."));
c.push(STEP(s, "FLP configuration: semantic object DPRProduction, action analyzeProduction, title “DPR Production Records”. This writes the inbound into manifest.json:"));
c.push(...code(`"sap.app": {
  "crossNavigation": {
    "inbounds": {
      "DPRProduction-analyzeProduction": {
        "semanticObject": "DPRProduction",
        "action": "analyzeProduction",
        "title": "{{flpTitle}}",
        "signature": { "parameters": {}, "additionalParameters": "allowed" }
      }
    }
  }
}`));
c.push(STEP(s, "Preview Application → enter the parameters → the ALP must show the chart “Daily Production by Product” and the full table with all columns of the metadata extension (Date, Product, Asset, Block, Volume Type, quantities, PI %). Use the table settings to check that Export to Excel is offered."));
c.push(STEP(s, "Repeat for ZDPR_Q_BOEPD_TREND_CDS (action analyzeTrend, chart qualifier BoepdVsTarget), ZDPR_Q_TARGET_QUERY_CDS (action analyzeTarget, chart ActualVsTarget) and, as a List Report, ZDPR_Q_PROD_PERF_CDS (action analyzePerformance)."));
c.push(SP());
c.push(note("Why the parameter entity set here:", "The dashboard cards bind to the …Results set and receive the parameters through a SelectionVariant; the full-screen apps bind to the parameter entity set so the parameters become filter-bar fields. Both are SAP's documented patterns and both read the same service.", "E2EFDA"));
c.push(PB());

/* 3 */
c.push(H1("3. Deploy the target apps to SAP BTP"));
s = STEPS();
c.push(STEP(s, "In each project: right-click → Build MTA Project. The generator created mta.yaml with the HTML5 application repository module and the destination binding."));
c.push(STEP(s, "Deploy MTA Archive to the same Cloud Foundry space as the dashboard (login to CF from the BAS terminal first: cf login, then cf target -o <org> -s <space>)."));
c.push(STEP(s, "Check in the BTP cockpit → HTML5 Applications that dpr-prod-records (and the others) appear next to the dashboard app."));
c.push(P("Nothing changes on the Cloud Connector or the destination: the target apps use the same backend service paths (/sap/opu/odata/sap/ZDPR_Q_*_CDS) that were already exposed for the dashboard.", { italics: true }));

/* 4 */
c.push(H1("4. Register the apps and intents in SAP Build Work Zone"));
s = STEPS();
c.push(STEP(s, "Work Zone → Channel Manager → HTML5 Apps → Refresh (fetches the newly deployed apps from the HTML5 repository)."));
c.push(STEP(s, "Content Manager → Content Explorer → HTML5 Apps → select the four target apps → Add to My Content. Each app arrives with its intent (semantic object and action) taken from the manifest inbound."));
c.push(STEP(s, "Open each app in Content Manager: keep “Visible” off if the app should not have its own tile, or on if users may also start it directly. The intent works in both cases."));
c.push(STEP(s, "Add the apps to the same group and the same role as the dashboard (for example group DPR Reporting, role DPR_User). Save."));
c.push(STEP(s, "Site → assign the role (if not already) and re-open the site. In the launchpad URL bar, test the intent by hand: #DPRProduction-analyzeProduction — the ALP must open."));
c.push(SP());
c.push(note("Important:", "Navigation between apps in Work Zone only works when the target app's intent is part of a role that the user has. An app that is deployed but not added to a role gives “Could not open app” when the card is clicked.", "FFF2CC"));
c.push(PB());

/* 5 */
c.push(H1("5. Add the navigation to the dashboard cards"));
c.push(P("The Overview Page reads the navigation target from an annotation on the card's entity type. The simplest place is the app's local annotation file (webapp/annotations/annotation.xml), so nothing in the back end changes. One UI.Identification record with a DataFieldForIntentBasedNavigation per card:"));
c.push(...code(`<Annotations Target="ZDPR_Q_PROD_QUERY_CDS.ZDPR_Q_PROD_QUERYType">
  <Annotation Term="UI.Identification">
    <Collection>
      <Record Type="UI.DataFieldForIntentBasedNavigation">
        <PropertyValue Property="Label" String="Show all records"/>
        <PropertyValue Property="SemanticObject" String="DPRProduction"/>
        <PropertyValue Property="Action" String="analyzeProduction"/>
      </Record>
    </Collection>
  </Annotation>
</Annotations>`));
c.push(P("Then reference it in the card settings of manifest.json and, for table or list cards, also on the line-item level so that a click on a row navigates as well:"));
c.push(...code(`"card06_records": {
  "model": "prod",
  "template": "sap.ovp.cards.table",
  "settings": {
    "title": "Production by Asset",
    "entitySet": "ZDPR_Q_PROD_QUERYResults",
    "annotationPath": "com.sap.vocabularies.UI.v1.LineItem",
    "identificationAnnotationPath": "com.sap.vocabularies.UI.v1.Identification",
    "selectionAnnotationPath": "com.sap.vocabularies.UI.v1.SelectionVariant#Params",
    "defaultSpan": { "rows": 13, "cols": 1 }
  }
}`));
c.push(P("The same pattern for the other cards: the BOEPD chart card gets DPRProduction / analyzeTrend, the performance cards DPRProduction / analyzePerformance, the target card DPRProduction / analyzeTarget."));
c.push(H2("5.1 How the filter values travel"));
c.push(BULLET("On navigation the Overview Page packs the global filter values and the card's selection variant into the app state (sap-xapp-state) and passes it to the target app."));
c.push(BULLET("The target app applies every value whose field name matches one of its filter-bar fields or parameters. Our parameters are named identically in all queries (P_DateFrom, P_DateTo, P_FiscalYear), so the date range of the dashboard is applied automatically; the user just presses Go."));
c.push(BULLET("A card-level filter such as ScopeType = YTD on the performance card is passed as well and pre-fills the filter bar of the List Report."));
c.push(H2("5.2 Redeploy and test"));
s = STEPS();
c.push(STEP(s, "Build MTA Project → Deploy MTA Archive for the dashboard app; Work Zone → HTML5 Apps → Refresh."));
c.push(STEP(s, "Open the dashboard, set 01.09.2025–30.09.2025, Go. The card header “Production by Asset” is now a link; “3 of 2460” stays, that is expected."));
c.push(STEP(s, "Click the header: the ALP opens with the same dates, all 2460 rows (paged), chart on top. Table settings → Export to Excel produces the full list."));
c.push(STEP(s, "Back arrow returns to the dashboard with the filter values kept."));
c.push(PB());

/* 6 */
c.push(H1("6. Standard analysis apps without development: Query Browser / View Browser"));
c.push(P("All DPR queries carry the analytical query annotation. SAP's standard Fiori app Query Browser (technical name F1068, also reachable through View Browser) lists exactly such queries, opens them in a pivot table with all dimensions and measures, supports drill-down and exports to Excel. It runs on the on-premise S/4HANA launchpad."));
s = STEPS();
c.push(STEP(s, "PFCG: add the business catalog SAP_CA_BC_ANA_AQ_PC (Query Browser) and its technical catalog to a role, for example ZDPR_ANALYST, and assign the role to the users."));
c.push(STEP(s, "On-premise launchpad (/UI2/FLP): the tile Query Browser appears. Search for ZDPR_Q_PROD_QUERY, open, enter the parameters, arrange dimensions and measures, export."));
c.push(STEP(s, "To offer the same tile inside the BTP site, use content federation: expose the on-premise catalog with transaction /UI2/CDM3_EXPOSURE and add the S/4HANA content provider in Work Zone (Channel Manager → New → Content Provider → SAP S/4HANA). Then the Query Browser tile can be added to the DPR Reporting group."));
c.push(P("This option costs no development and is the fastest way to give analysts everything. It is also the best tool for validating the dashboard figures against the DPR Excel, because it shows the raw query results.", { italics: true }));

/* 7 */
c.push(H1("7. Excel / PDF download"));
c.push(P("The download actions of ZDPR_I_EXCEL_DL (downloadProduction, downloadTargets, downloadPdfProduction, downloadPdfTargets) return the DPR workbook or PDF for a date range. Options to offer them to users:"));
c.push(BULLET("A tile in Work Zone pointing to the OData V4 service binding ZDPR_SB_ANALYTICS_O4 via a small Fiori elements List Report on ZDPR_I_EXCEL_DL, where the actions appear as buttons."));
c.push(BULLET("Later, a button on a custom card of the dashboard (see the Fiori limits document, section 5, option B)."));

/* 8 */
c.push(H1("8. Correction on the “Production by Asset” card"));
c.push(P("The rows in the card are single days of the same asset, not totals, because the bound query has the production date in its key and the service returns one row per day. Two ways to show totals per asset in the card:"));
c.push(BULLET("Bind the card to an additional query without the date dimension (a query on ZDPR_C_PROD_CUBE with asset, product and the OVL quantities only). This is a small back-end addition: one CDS query, one metadata extension, one OData V2 service registration. Not yet built; ask for it and it will be added to the package."));
c.push(BULLET("Keep the card as a “latest records” card: sort descending by date through a PresentationVariant so the newest three records are shown, and let the navigation take the user to the totals."));

/* 9 */
c.push(H1("9. Checklist"));
c.push(table(
  ["Step", "Where", "Done"],
  [
    ["Target apps generated (ALP ×3, List Report ×1) with FLP configuration", "BAS", ""],
    ["Target apps deployed", "Cloud Foundry space", ""],
    ["HTML5 apps refreshed and added to content, group and role", "Work Zone", ""],
    ["Intent tested by URL (#DPRProduction-analyzeProduction)", "Work Zone site", ""],
    ["UI.Identification annotations and identificationAnnotationPath added to the cards", "Dashboard project", ""],
    ["Dashboard redeployed, card click opens the target app with the same dates", "Work Zone site", ""],
    ["Query Browser catalog assigned to analysts", "PFCG on S/4HANA", ""],
  ],
  [5200, 2800, 1360]
));

const doc = new Document({
  styles: { default: { document: { run: { font: "Calibri", size: 21 } } } },
  numbering: { config: [
    { reference: "bullets", levels: [{ level: 0, format: LevelFormat.BULLET, text: "•", alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 540, hanging: 270 } } } }] },
    ...Array.from({ length: stepRef }, (_, i) => ({ reference: "steps" + (i + 1), levels: [{ level: 0, format: LevelFormat.DECIMAL, text: "%1.", alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 540, hanging: 300 } } } }] })),
  ] },
  sections: [{
    properties: { page: { margin: { top: 1200, bottom: 1200, left: 1300, right: 1300 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER,
      children: [new TextRun({ text: "ONGC Videsh — DPR Analytical RAP — Dashboard navigation to full data — page ", size: 16, color: "808080" }),
                 new TextRun({ children: [PageNumber.CURRENT], size: 16, color: "808080" })] })] }) },
    children: c,
  }],
});
Packer.toBuffer(doc).then((b) => { fs.writeFileSync(OUT, b); console.log("written", OUT, b.length); });
