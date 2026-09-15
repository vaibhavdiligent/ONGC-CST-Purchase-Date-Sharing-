const fs = require("fs");
const path = require("path");
const {
  Document, Packer, Paragraph, TextRun, HeadingLevel, AlignmentType,
  Table, TableRow, TableCell, WidthType, ShadingType, BorderStyle,
  PageBreak, TableOfContents, LevelFormat, Footer, PageNumber, convertInchesToTwip,
} = require("docx");

const OUT = path.join(__dirname, "ZDPR_RAP_Fiori_Generator_Walkthrough.docx");
const BLUE = "1F4E79", GREY = "F2F2F2", CODEBG = "F7F7F7";

const H1 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_1, spacing: { before: 320, after: 160 } });
const H2 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_2, spacing: { before: 260, after: 120 } });
const P = (t, o = {}) => new Paragraph({ spacing: { after: 120 },
  children: [new TextRun({ text: t, size: 21, bold: !!o.bold, italics: !!o.italics })] });
const BULLET = (t) => new Paragraph({ numbering: { reference: "bullets", level: 0 }, spacing: { after: 80 },
  children: [new TextRun({ text: t, size: 21 })] });
const STEP = (t) => new Paragraph({ numbering: { reference: "steps", level: 0 }, spacing: { after: 100 },
  children: [new TextRun({ text: t, size: 21 })] });
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
const PB = () => new Paragraph({ children: [new PageBreak()] });
const SP = () => new Paragraph({ spacing: { after: 140 } });

const c = [];
c.push(
  new Paragraph({ spacing: { before: 2200 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "ONGC Videsh — DPR Analytical RAP", bold: true, size: 44, color: BLUE })] }),
  new Paragraph({ spacing: { before: 160 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Deploying the DPR Service to BTP with the SAP Fiori Generator", bold: true, size: 30, color: "404040" })] }),
  new Paragraph({ spacing: { before: 500 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Step-by-step walkthrough — from an active backend to a launchpad tile", size: 22, italics: true, color: "606060" })] }),
  new Paragraph({ spacing: { before: 900 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Starting point: all CDS objects of package ZPR_DPR_RAP are active.", size: 20, color: "606060" })] }),
  PB()
);
c.push(H1("Contents"));
c.push(new TableOfContents("Contents", { hyperlink: true, headingStyleRange: "1-2" }));
c.push(PB());

/* Overview */
c.push(H1("1. Overview and Sequence"));
c.push(P("This document walks through the deployment of the DPR report to SAP BTP as a Fiori application with a tile in SAP Build Work Zone (the BTP Fiori Launchpad). The order of the steps is not optional: the SAP Fiori generator can only see a service that is already reachable from BTP, and BTP can only reach a service that is already working inside the S/4HANA system. Work top-down through the table; each step has a check that must pass before the next one starts."));
c.push(table(
  ["Step", "Where", "Who", "Check that proves it worked"],
  [
    ["0", "S/4HANA backend", "Developer / Basis", "$metadata of the OData V2 service returns XML in a browser"],
    ["1", "SAP Cloud Connector", "Basis", "Subaccount shows the Cloud Connector as connected; both OData paths exposed"],
    ["2", "BTP cockpit — Destination", "BTP administrator", "Check Connection succeeds"],
    ["3", "Business Application Studio — Fiori generator", "Developer", "Application runs locally and draws the graph"],
    ["4", "Business Application Studio — build and deploy", "Developer", "Application listed under HTML5 Applications"],
    ["5", "SAP Build Work Zone", "Fiori administrator", "Tile visible and opens the report"],
  ],
  [600, 2900, 2100, 3760]
));
c.push(SP());
c.push(P("Two services are involved. The graph (DPR Excel tab 2) is served over OData V2 because analytical queries cannot be exposed through an OData V4 service definition on this release; the Production Performance view (tab 3) is served over OData V4.", { italics: true }));
c.push(table(
  ["Purpose", "Service", "Protocol", "Entity"],
  [
    ["Tab-2 graph: Actual vs BE Target BOEPD", "ZDPR_Q_BOEPD_TREND_CDS", "OData V2 (generated by @OData.publish)", "ZDPR_Q_BOEPD_TREND"],
    ["Tab-3 Production Performance", "ZDPR_SB_ANALYTICS_O4", "OData V4 (service binding)", "DPRProductionPerformance"],
    ["Daily trend / production records / targets", "ZDPR_Q_DAILY_TREND_CDS, ZDPR_Q_PROD_QUERY_CDS, ZDPR_Q_TARGET_QUERY_CDS", "OData V2", "same-named entities"],
  ],
  [2600, 2900, 2000, 1860]
));
c.push(PB());

/* Step 0 */
c.push(H1("2. Step 0 — Make the Backend Service Reachable"));
c.push(P("Nothing in BTP can be tested until the service answers inside the ONGC network. This step is performed in SAP GUI and a browser."));
c.push(H2("2.1 Register the OData V2 services"));
c.push(STEP("Run transaction /IWFND/MAINT_SERVICE."));
c.push(STEP("Choose Add Service. System Alias: LOCAL (or the alias Basis has configured for the local system). Technical Service Name: ZDPR_Q_BOEPD_TREND_CDS. Press Get Services."));
c.push(STEP("Select the service and choose Add Selected Services. Assign package ZPR_DPR_RAP and the transport request."));
c.push(STEP("Repeat for ZDPR_Q_DAILY_TREND_CDS, ZDPR_Q_PROD_QUERY_CDS and ZDPR_Q_TARGET_QUERY_CDS."));
c.push(H2("2.2 Confirm the OData V4 binding"));
c.push(STEP("In ADT, open the service binding ZDPR_SB_ANALYTICS_O4 and confirm the status is Published. If not, press Publish."));
c.push(H2("2.3 Prove the service in a browser"));
c.push(P("From a PC inside the network, open the metadata document of the graph service:"));
c.push(...code("https://<s4-host>:<port>/sap/opu/odata/sap/ZDPR_Q_BOEPD_TREND_CDS/$metadata?sap-client=<client>"));
c.push(SP());
c.push(P("Expected: an XML document that contains the parameters P_DateFrom and P_DateTo, and UI annotations (search the text for UI.Chart). Then the V4 service:"));
c.push(...code("https://<s4-host>:<port>/sap/opu/odata4/sap/zdpr_analytics/srvd/sap/zdpr_sd_analytics/0001/$metadata?sap-client=<client>"));
c.push(SP());
c.push(note("Stop here if this fails.",
  "If the metadata does not load in the browser, no BTP configuration will make it load either. Typical causes: service not registered (2.1), binding not published (2.2), or the user lacks the OData authorisation (S_SERVICE). If UI.Chart is missing from the XML, the metadata extension ZDPR_Q_BOEPD_TREND is not active — the graph will not be pre-configured until it is.",
  "FFF2CC"));
c.push(PB());

/* Step 1 */
c.push(H1("3. Step 1 — Cloud Connector (Basis)"));
c.push(STEP("Confirm the Cloud Connector is connected to the BTP subaccount in which the application will be deployed."));
c.push(STEP("Create or reuse a system mapping for the S/4HANA system: internal host and port of the ABAP system, and a virtual host and port. The virtual host name is what the destination will reference."));
c.push(STEP("In the access control of that mapping, expose BOTH resource paths, each with Path and All Sub-Paths: /sap/opu/odata/ (OData V2, the graph) and /sap/opu/odata4/ (OData V4)."));
c.push(STEP("Configure principal propagation if users must be identified individually in S/4HANA. For a first connectivity test a technical user with basic authentication is acceptable; it is not acceptable for productive reporting."));
c.push(SP());
c.push(note("Lead time:", "Principal propagation involves trust configuration and certificates between BTP, the Cloud Connector and the S/4HANA system. It is usually the longest item in the whole deployment. Start it in parallel with everything else.", "FFF2CC"));
c.push(PB());

/* Step 2 */
c.push(H1("4. Step 2 — Destination in the BTP Cockpit"));
c.push(P("In the BTP cockpit open the subaccount and choose Connectivity → Destinations → New Destination."));
c.push(table(
  ["Field", "Value", "Comment"],
  [
    ["Name", "ZDPR_S4_BACKEND", "Referenced by the application; keep it identical in every subaccount"],
    ["Type", "HTTP", ""],
    ["URL", "http://<virtual-host>:<virtual-port>", "The Cloud Connector virtual host, not the real host"],
    ["Proxy Type", "OnPremise", "Routes the call through the Cloud Connector"],
    ["Authentication", "PrincipalPropagation", "BasicAuthentication for the first connectivity test only"],
    ["Location ID", "as configured", "Only if the Cloud Connector uses a location ID"],
  ],
  [1700, 3400, 4260]
));
c.push(SP());
c.push(P("Add these Additional Properties. Without them the Fiori generator cannot list the services of the system — an empty service list in the generator is almost always this."));
c.push(...code(`sap-client               = <client>
HTML5.DynamicDestination = true
WebIDEEnabled            = true
WebIDEUsage              = odata_abap,odata_gen
WebIDESystem             = <SID>`));
c.push(SP());
c.push(P("Press Check Connection. Success proves the tunnel through the Cloud Connector; it does not yet prove the OData service or the user's authorisation — those were proven in Step 0."));
c.push(PB());

/* Step 3 */
c.push(H1("5. Step 3 — Generate the Application in Business Application Studio"));
c.push(H2("5.1 Dev space"));
c.push(STEP("Open SAP Business Application Studio from the subaccount subscriptions."));
c.push(STEP("Create a Dev Space of type SAP Fiori and start it."));
c.push(H2("5.2 Fiori generator"));
c.push(STEP("From the Welcome page choose New Project from Template → SAP Fiori generator → Start."));
c.push(STEP("Template: Analytical List Page. This is the floorplan that renders the graph above the table."));
c.push(STEP("Data source: Connect to a System. System: choose the destination ZDPR_S4_BACKEND. The service list loads from the backend."));
c.push(STEP("Service: select ZDPR_Q_BOEPD_TREND_CDS (OData V2)."));
c.push(STEP("Main entity: ZDPR_Q_BOEPD_TREND. The generator recognises the parameterised entity and its result set automatically."));
c.push(STEP("Project attributes: module name zdprboepdtrend, application title DPR Production Trend, description, namespace per ONGC standards."));
c.push(STEP("Add deployment configuration: Yes → target Cloud Foundry → destination ZDPR_S4_BACKEND. This generates mta.yaml and xs-app.json."));
c.push(STEP("Add FLP configuration: Yes → semantic object DPRProduction, action display, title DPR Production Trend."));
c.push(STEP("Finish. The project is generated."));
c.push(H2("5.3 Run locally before deploying"));
c.push(STEP("Right-click the project → Preview Application → start with the default configuration (the one using the destination)."));
c.push(STEP("Log on with the S/4HANA user when prompted."));
c.push(STEP("In the filter bar enter a date range that has data and targets, for example 01.04.2024 to 14.04.2024, and press Go."));
c.push(STEP("Expected: a line chart with two lines — Actual Production and BE Target — and the table underneath. Both lines mean backend, connectivity and the metadata extension are all correct."));
c.push(SP());
c.push(note("Always test locally first.", "A problem found here is fixed in minutes. The same problem found after deployment costs a redeploy cycle and is harder to diagnose because the launchpad hides the error details.", "E2EFDA"));
c.push(PB());

/* Step 4 */
c.push(H1("6. Step 4 — Build and Deploy"));
c.push(STEP("Right-click mta.yaml → Build MTA Project. The build produces an .mtar file in the folder mta_archives."));
c.push(STEP("Right-click the .mtar file → Deploy MTA Archive."));
c.push(STEP("When prompted, log on to Cloud Foundry and choose the organisation and space of this landscape tier."));
c.push(STEP("Wait for the deployment to finish. In the BTP cockpit the application now appears under HTML5 Applications."));
c.push(SP());
c.push(P("The same .mtar is deployed to the Quality and Production subaccounts. Nothing is rebuilt per tier — only the destination in each subaccount points to a different S/4HANA system, which is why the destination name must be identical everywhere.", { bold: true }));
c.push(PB());

/* Step 5 */
c.push(H1("7. Step 5 — Tile in SAP Build Work Zone"));
c.push(STEP("Open SAP Build Work Zone, standard edition, from the subaccount subscriptions (Site Manager)."));
c.push(STEP("Channel Manager → select the HTML5 Apps content channel → Refresh. The deployed application appears in the content."));
c.push(STEP("Content Manager → New → Group: DPR Reporting. Assign the application to it."));
c.push(STEP("Content Manager → New → Role (or edit an existing one): assign the application. Only users with this role see the tile."));
c.push(STEP("Site Directory → open the site → assign the group and the role."));
c.push(STEP("BTP cockpit → Security → Role Collections: assign the role collection that Work Zone created for the role to the DPR users or to their identity-provider group."));
c.push(STEP("Open the site URL as a DPR user. The tile DPR Production Trend is visible and opens the report."));
c.push(H2("7.1 Second tile — Production Performance (tab 3)"));
c.push(P("Repeat Step 3 with these differences: template Analytical List Page or List Report, service ZDPR_SB_ANALYTICS_O4 (OData V4), main entity DPRProductionPerformance, module name zdprprodperf, semantic object DPRPerformance. Deploy and add to the same group."));
c.push(PB());

/* Troubleshooting */
c.push(H1("8. If Something Does Not Work"));
c.push(table(
  ["Symptom", "Cause", "Where to fix"],
  [
    ["Service list in the generator is empty", "WebIDEEnabled / WebIDEUsage missing on the destination", "Step 2, additional properties"],
    ["Check Connection fails", "Cloud Connector not connected, or virtual host mismatch", "Step 1"],
    ["Connection ok but service returns 404", "Path not exposed in the Cloud Connector, or service not registered / published", "Step 1 access control; Step 0"],
    ["401 / 403 from the service", "Principal propagation incomplete, or user lacks S_SERVICE authorisation", "Basis / Security"],
    ["App opens: plain table, no chart", "Metadata extension ZDPR_Q_BOEPD_TREND not active", "ADT — create/activate the DDLX (companion document, section 4)"],
    ["App opens: no rows", "Mandatory dates missing, or no data in the period", "Enter a FY 2024-25 range"],
    ["BE Target line flat at zero", "No FY 2025-26 targets in ZPRA_T_PRD_TAR", "Functional data load — not a technical issue"],
    ["Tile missing in Work Zone", "Channel not refreshed, or role not assigned", "Step 5"],
  ],
  [2900, 3300, 3160]
));
c.push(SP());
c.push(P("For the wider context — BTP entitlements, the SAP Analytics Cloud alternative, landscape strategy and the go-live checklist — see the companion document “DPR Analytical RAP — Deployment on SAP BTP”.", { italics: true }));

const doc = new Document({
  creator: "ONGC Videsh DPR Project",
  title: "DPR Analytical RAP — Fiori Generator Walkthrough",
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
      new TextRun({ text: "DPR Analytical RAP — Fiori Generator Walkthrough     Page ", size: 16, color: "808080" }),
      new TextRun({ children: [PageNumber.CURRENT], size: 16, color: "808080" }),
      new TextRun({ text: " of ", size: 16, color: "808080" }),
      new TextRun({ children: [PageNumber.TOTAL_PAGES], size: 16, color: "808080" }) ] })] }) },
    children: c }],
});
Packer.toBuffer(doc).then((b) => { fs.writeFileSync(OUT, b); console.log("written", OUT, b.length); });
