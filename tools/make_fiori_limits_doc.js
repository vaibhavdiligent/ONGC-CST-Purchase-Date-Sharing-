const fs = require("fs");
const path = require("path");
const {
  Document, Packer, Paragraph, TextRun, HeadingLevel, AlignmentType,
  Table, TableRow, TableCell, WidthType, ShadingType, BorderStyle,
  PageBreak, TableOfContents, LevelFormat, Footer, PageNumber, ImageRun, ExternalHyperlink,
} = require("docx");

const ROOT = path.dirname(__dirname);
const OUT = path.join(ROOT, "deploy", "ZDPR_RAP_Fiori_OVP_Limits_and_Layout.docx");
const IMG = path.join(ROOT, "deploy", "DPR_Dashboard_Fiori_Realistic.png");
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
const LINK = (label, url) => new Paragraph({ numbering: { reference: "bullets", level: 0 }, spacing: { after: 60 },
  children: [new ExternalHyperlink({ link: url, children: [new TextRun({ text: label, style: "Hyperlink", size: 20 })] })] });
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
    children: [new TextRun({ text: "Fiori Overview Page: what the dashboard can and cannot show", bold: true, size: 30, color: "404040" })] }),
  new Paragraph({ spacing: { before: 500 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Why the built page differs from the design mockup, the SAP rules behind it, and the layout to configure", size: 22, italics: true, color: "606060" })] }),
  PB()
);
c.push(H1("Contents"));
c.push(new TableOfContents("Contents", { hyperlink: true, headingStyleRange: "1-2" }));
c.push(PB());

/* 1 */
c.push(H1("1. Why the page does not look like the mockup"));
c.push(P("The design mockup was drawn as a free layout: a wide chart, a six-column performance table with four rows, KPI tiles and coloured lines. A Fiori elements Overview Page (OVP) is not a free layout. It is a template: every card is one of a fixed set of card types, each with fixed rules for size, number of columns, number of rows, chart types and colours. The rules are set by SAP and cannot be changed by annotations or by the developer, only by writing a custom card (real UI5 development). This document lists the rules, verified against SAP's documentation, and gives the layout that fits inside them."));
c.push(note("In one sentence:", "The Overview Page can show the same information as the mockup, but as small standard cards — three columns and three rows per table, standard chart styling, dates as text — with the full tables and large charts one click away in the target apps.", "FFF2CC"));
c.push(SP());
c.push(P("The picture below is what the page realistically looks like once the settings of section 4 are applied (illustrative figures)."));
c.push(new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 160 },
  children: [new ImageRun({ type: "png", data: fs.readFileSync(IMG), transformation: { width: 620, height: 404 } })] }));
c.push(PB());

/* 2 */
c.push(H1("2. The SAP rules (verified)"));
c.push(H2("2.1 Table card and list card"));
c.push(BULLET("A table card shows a maximum of 3 columns and 3 rows, with up to 3 lines of text per row. To see the full result the user navigates to the target application (“View More”)."));
c.push(BULLET("All three columns may be a data field or a data point; data points can carry semantic colours (green/orange/red through the Criticality of the DataPoint annotation). Arbitrary colours or bold cells are not possible."));
c.push(BULLET("A list card shows up to 3 items in the fixed layout, each with a title, a description and one or two values; a bar-list variant can show a small bar per item."));
c.push(BULLET("Column headers come from the labels of the UI.LineItem annotation; column order is the order of the line items. The Overview Page takes the first three line items (by position), so the three most important columns must be first."));
c.push(H2("2.2 Analytical (chart) card"));
c.push(BULLET("Supported chart types: line, column, stacked column, bar, combination (column + line), donut, bubble, scatter, vertical bullet. A line chart needs one dimension (category axis) and at least one measure; two measures give two lines, as in the DPR graph."));
c.push(BULLET("Chart colours come from the SAP theme palette in series order (blue, orange, …). The only way to influence colours is semantic colouring through the chart annotation (Criticality), which colours a series good/critical/bad. The Excel colours of the DPR graph cannot be reproduced exactly."));
c.push(BULLET("Design guideline for the fixed card layout: keep charts small — up to 4 data points or 2 series are recommended. There is no hard limit on data points, but a card with many points becomes crowded and slow. A 30-day range with two lines is fine; a full year with daily points is readable only in the resizable layout with a wide card, and is better shown in the target app."));
c.push(BULLET("The category axis shows the raw property value. A date property of the OData V2 service generated from a CDS DATS field is a string in the form YYYYMMDD, which is why the axis showed 20250901. The fix is a text dimension (see section 3)."));
c.push(BULLET("A card has one chart. A chart plus a table in one card is not possible; a table card of the same entity set is a second card."));
c.push(H2("2.3 Card header and KPI figures"));
c.push(BULLET("The card title and subtitle can have at most two lines each."));
c.push(BULLET("A KPI header (big number with target and deviation) is possible on chart, table and list cards, but it needs a DataPoint annotation on an entity set that the back end can aggregate — an analytical query. ZDPR_Q_PROD_PERF is a plain view, so its card cannot have a KPI header. A KPI header on the BOEPD chart card is possible (sum of ActualBoepdOvl over the filtered range) and is described in section 4."));
c.push(BULLET("KPI headers on parameterised entity sets are not supported for OData V4 — this is why the whole dashboard was placed on OData V2."));
c.push(H2("2.4 Page layout and filter bar"));
c.push(BULLET("Fixed card layout (default): every card is one column wide (about 20 rem) and the height is decided by the card type; the user can only hide, show and reorder cards."));
c.push(BULLET("Resizable card layout: cards sit on a grid; the developer sets the initial size per card (defaultSpan: rows and cols, one column ≈ 320 px) and the user can resize. A wider table card shows more columns; a taller one shows more rows. This is the layout to use for the DPR page."));
c.push(BULLET("The global filter bar can only contain fields of one entity type (globalFilterEntityType). Cards are filtered by name: a filter field applies to a card only if the card's entity set has a property of the same name and type. For the DPR queries the common names are ProductionDate, Asset, ProductGroup, BusinessUnit and the parameters P_DateFrom, P_DateTo, P_FiscalYear."));
c.push(BULLET("No custom fonts, logos, background colours or free-form text blocks; the shell bar, page title and card frames are the standard Fiori shell (theme Horizon or Quartz as set for the launchpad)."));
c.push(H2("2.5 Sources"));
c.push(LINK("SAP Fiori Design Guidelines — Overview Page: Table Card", "https://www.sap.com/design-system/fiori-design-web/v1-84/page-types/floorplans/overview-page-ovp/cards/overview-page-table-card"));
c.push(LINK("SAP Fiori Design Guidelines — Overview Page: Resizable Card Layout", "https://www.sap.com/design-system/fiori-design-web/v1-84/page-types/floorplans/overview-page-ovp/resizable-card-layout-overview-page/usage"));
c.push(LINK("SAP Fiori Design Guidelines — Overview Page: Fixed Card Sizes", "https://www.sap.com/design-system/fiori-design-web/v1-84/page-types/floorplans/overview-page-ovp/overview-page-fixed-card-layout/usage"));
c.push(LINK("SAP Fiori Design Guidelines — Analytical Card", "https://www.sap.com/design-system/fiori-design-web/v1-71/ui-elements/analytical-card/usage"));
c.push(LINK("SAPUI5 documentation — Overview Page Card (SAP-docs on GitHub)", "https://github.com/SAP-docs/sapui5/blob/main/docs/06_SAP_Fiori_Elements/overview-page-card-74332d5.md"));
c.push(LINK("SAP Community — Overview Page Table Cards in detail", "https://community.sap.com/t5/technology-blog-posts-by-sap/sap-fiori-overview-page-table-cards-in-detail/ba-p/13381313"));
c.push(LINK("SAP Community — Annotations for analytical cards on the Overview Page", "https://community.sap.com/t5/technology-blog-posts-by-sap/basics-of-fiori-elements-annotations-for-analytical-cards-on-overview-page/ba-p/13439808"));
c.push(LINK("SAP Community — Overview Page KPI header", "https://blogs.sap.com/2018/01/24/sap-fiori-elements-overview-page-kpi-header/"));
c.push(PB());

/* 3 */
c.push(H1("3. Mockup versus what is built"));
c.push(table(
  ["Mockup element", "Overview Page reality", "What we do"],
  [
    ["Wide line chart, custom colours, dates on the axis", "Analytical line card; theme colours; axis shows raw YYYYMMDD", "Card made 2 columns wide (resizable layout). New text dimension ProductionDateText (YYYY-MM-DD) as chart category. Colours stay blue/orange."],
    ["Performance table: 6 columns × 4 rows with % colours", "Table card: 3 columns × 3 rows", "Two table cards — YTD and Annual — each 2 rows, 3 columns: Row label | Actual | % Achv (green/orange/red from AchievementCriticality). Full 8-column table in the target app."],
    ["KPI tile “Total O+OEG 193,420 BOEPD ▼ 3.2 %”", "KPI header only on aggregatable entity sets", "KPI header on the BOEPD chart card (sum of ActualBoepdOvl vs TargetBoepd over the range) — optional, section 4.3."],
    ["Target vs Actual by Product (bars, achievement %)", "Column card; two measures; 1 dimension", "Kept as a column card, 1 column wide; Achievement % in the target app table."],
    ["Daily Production Trend by Business Unit (4 lines)", "Line card; one colour dimension", "Line card by product (3 lines) with ProductionDateText axis; BU as filter, not as series."],
    ["Production Records: 7-column asset table with “Show all 17 assets”", "List card: 3 items, title + description + value", "List card: asset name, BU, OVL BOEPD; “View More” opens the production query app."],
    ["Free text footer, logo, custom fonts", "Not available", "Dropped."],
  ],
  [2900, 3000, 3460]
));
c.push(SP());
c.push(P("Backend changes already made for this (in the ZIP and in the metadata extension sources):"));
c.push(BULLET("ZDPR_C_BOEPD_DAY and ZDPR_C_PROD_CUBE: new dimension ProductionDateText (YYYY-MM-DD). ZDPR_Q_BOEPD_TREND and ZDPR_Q_DAILY_TREND expose it."));
c.push(BULLET("ZDPR_Q_PROD_PERF: new element RowLabel (“YTD - Gas (MMSCMD)”, …) so that scope and product fit in one column."));
c.push(BULLET("Metadata extensions (re-apply in ADT, they are manual objects): ZDPR_Q_BOEPD_TREND and ZDPR_Q_DAILY_TREND charts use ProductionDateText and sort by ProductionDate; ZDPR_Q_PROD_PERF line items are ordered RowLabel, ActualPerDay, AchievementPct (data point with criticality AchievementCriticality), then the rest."));
c.push(PB());

/* 4 */
c.push(H1("4. Layout to configure (manifest.json)"));
c.push(H2("4.1 Resizable layout and card sizes"));
c.push(P("Set the container layout to resizable and give each card its initial size. One column is about 320 px; rows are grid rows of about 1 rem. These values produce the picture in section 1 on a 1366 px screen (four columns)."));
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
        "subTitle": "OVL share",
        "entitySet": "ZDPR_Q_BOEPD_TRENDResults",
        "chartAnnotationPath": "com.sap.vocabularies.UI.v1.Chart#BoepdVsTarget",
        "presentationAnnotationPath": "com.sap.vocabularies.UI.v1.PresentationVariant",
        "selectionAnnotationPath": "com.sap.vocabularies.UI.v1.SelectionVariant#Params",
        "defaultSpan": { "rows": 26, "cols": 2 }
      }
    },
    "card02_perfYtd": {
      "model": "",
      "template": "sap.ovp.cards.table",
      "settings": {
        "title": "Production Performance - YTD",
        "subTitle": "Per day, fiscal year to date",
        "entitySet": "ZDPR_Q_PROD_PERFResults",
        "annotationPath": "com.sap.vocabularies.UI.v1.LineItem",
        "selectionAnnotationPath": "com.sap.vocabularies.UI.v1.SelectionVariant#YTD",
        "defaultSpan": { "rows": 13, "cols": 1 }
      }
    },
    "card03_perfAnnual": {
      "model": "",
      "template": "sap.ovp.cards.table",
      "settings": {
        "title": "Production Performance - Annual",
        "subTitle": "BE target per day, fiscal year",
        "entitySet": "ZDPR_Q_PROD_PERFResults",
        "annotationPath": "com.sap.vocabularies.UI.v1.LineItem#Annual",
        "selectionAnnotationPath": "com.sap.vocabularies.UI.v1.SelectionVariant#Annual",
        "defaultSpan": { "rows": 13, "cols": 1 }
      }
    },
    "card04_targetVsActual": {
      "model": "target",
      "template": "sap.ovp.cards.charts.analytical",
      "settings": {
        "title": "Target vs Actual by Product",
        "entitySet": "ZDPR_Q_TARGET_QUERYResults",
        "chartAnnotationPath": "com.sap.vocabularies.UI.v1.Chart#ActualVsTarget",
        "selectionAnnotationPath": "com.sap.vocabularies.UI.v1.SelectionVariant#Params",
        "defaultSpan": { "rows": 20, "cols": 1 }
      }
    },
    "card05_dailyTrend": {
      "model": "daily",
      "template": "sap.ovp.cards.charts.analytical",
      "settings": {
        "title": "Daily Production Trend",
        "entitySet": "ZDPR_Q_DAILY_TRENDResults",
        "chartAnnotationPath": "com.sap.vocabularies.UI.v1.Chart#TrendLine",
        "presentationAnnotationPath": "com.sap.vocabularies.UI.v1.PresentationVariant",
        "selectionAnnotationPath": "com.sap.vocabularies.UI.v1.SelectionVariant#Params",
        "defaultSpan": { "rows": 20, "cols": 1 }
      }
    },
    "card06_records": {
      "model": "prod",
      "template": "sap.ovp.cards.list",
      "settings": {
        "title": "Production Records - Latest Day",
        "entitySet": "ZDPR_Q_PROD_QUERYResults",
        "annotationPath": "com.sap.vocabularies.UI.v1.LineItem",
        "listType": "condensed",
        "selectionAnnotationPath": "com.sap.vocabularies.UI.v1.SelectionVariant#Params",
        "defaultSpan": { "rows": 20, "cols": 1 }
      }
    }
  }
}`));
c.push(SP());
c.push(H2("4.2 The two performance cards (YTD and Annual)"));
c.push(P("Both cards read ZDPR_Q_PROD_PERFResults. Each gets its own SelectionVariant in the local annotation file: the parameters plus a filter on ScopeType. The Annual card uses a second LineItem with qualifier Annual (BE target and BOEPD instead of actual and % achievement, because Annual rows have no actual). These qualifiers live in the app's annotation.xml, not in the back end:"));
c.push(...code(`<Annotations Target="ZDPR_Q_PROD_PERF_CDS.ZDPR_Q_PROD_PERFType">
  <Annotation Term="UI.SelectionVariant" Qualifier="YTD">
    <Record>
      <PropertyValue Property="Parameters">
        <Collection>
          <Record Type="UI.Parameter"><PropertyValue Property="PropertyName" PropertyPath="P_DateFrom"/><PropertyValue Property="PropertyValue" String="20250401"/></Record>
          <Record Type="UI.Parameter"><PropertyValue Property="PropertyName" PropertyPath="P_DateTo"/><PropertyValue Property="PropertyValue" String="20250930"/></Record>
          <Record Type="UI.Parameter"><PropertyValue Property="PropertyName" PropertyPath="P_FiscalYear"/><PropertyValue Property="PropertyValue" String="2025"/></Record>
        </Collection>
      </PropertyValue>
      <PropertyValue Property="SelectOptions">
        <Collection>
          <Record Type="UI.SelectOptionType">
            <PropertyValue Property="PropertyName" PropertyPath="ScopeType"/>
            <PropertyValue Property="Ranges">
              <Collection>
                <Record Type="UI.SelectionRangeType">
                  <PropertyValue Property="Sign" EnumMember="UI.SelectionRangeSignType/I"/>
                  <PropertyValue Property="Option" EnumMember="UI.SelectionRangeOptionType/EQ"/>
                  <PropertyValue Property="Low" String="YTD"/>
                </Record>
              </Collection>
            </PropertyValue>
          </Record>
        </Collection>
      </PropertyValue>
    </Record>
  </Annotation>
  <!-- Qualifier "Annual": same, with Low = "ANNUAL" -->
  <Annotation Term="UI.LineItem" Qualifier="Annual">
    <Collection>
      <Record Type="UI.DataField"><PropertyValue Property="Value" Path="RowLabel"/><PropertyValue Property="Label" String="Row"/></Record>
      <Record Type="UI.DataField"><PropertyValue Property="Value" Path="TargetPerDay"/><PropertyValue Property="Label" String="BE Target"/></Record>
      <Record Type="UI.DataField"><PropertyValue Property="Value" Path="TargetBoepdPerDay"/><PropertyValue Property="Label" String="BOEPD"/></Record>
    </Collection>
  </Annotation>
</Annotations>`));
c.push(SP());
c.push(H2("4.3 Optional KPI header on the BOEPD card"));
c.push(P("ZDPR_Q_BOEPD_TREND is an analytical query, so the service can aggregate it. A KPI header showing the total actual BOEPD of the selected range against the target is configured with a DataPoint and a PresentationVariant in the local annotation file and referenced in the card:"));
c.push(...code(`"dataPointAnnotationPath": "com.sap.vocabularies.UI.v1.DataPoint#ActualKpi",
"kpiAnnotationPath": "com.sap.vocabularies.UI.v1.KPI#ActualKpi"

<Annotation Term="UI.DataPoint" Qualifier="ActualKpi">
  <Record>
    <PropertyValue Property="Title" String="Actual (BOEPD)"/>
    <PropertyValue Property="Value" Path="ActualBoepdOvl"/>
    <PropertyValue Property="TargetValue" Path="TargetBoepd"/>
    <PropertyValue Property="Criticality" Path="Criticality"/>
  </Record>
</Annotation>`));
c.push(P("The value is the sum over all rows of the card's selection, i.e. over the days — divide by the number of days if a per-day figure is wanted; that division needs a further query element and is not built yet. Leave the KPI header out for the first version.", { italics: true }));
c.push(H2("4.4 Checks in the preview"));
c.push(STEP("Filter 01.09.2025–30.09.2025, fiscal year 2025, Go. Card 1: two lines, x-axis labelled 2025-09-01 … 2025-09-30, BE Target flat."));
c.push(STEP("Card 2 shows two rows (YTD Oil, YTD Gas) with a green/orange/red % cell; card 3 shows the two Annual rows with BE target values."));
c.push(STEP("Widen card 2 by one column: the BE Target column appears (fourth line item)."));
c.push(STEP("Click a card header: the target app (Analytical List Page of the query, if deployed) opens with the same filter values."));
c.push(PB());

/* 5 */
c.push(H1("5. If the standard cards are not enough"));
c.push(table(
  ["Option", "What it gives", "Effort / remark"],
  [
    ["A. Overview Page with standard cards (this document)", "One tile, one filter bar, small standard cards, drill-down to the target apps", "No UI5 coding; annotations and manifest only. Recommended first version."],
    ["B. Custom card in the Overview Page", "A card with its own XML view: the Excel-like performance table with all columns, a large chart with chosen colours", "UI5 development (sap.ovp custom card: component + view + controller). Keeps the single tile and the global filter."],
    ["C. Analytical List Page for the graph", "Large chart plus table on one screen with a filter bar, table with all columns", "No coding, but it is one app per query; the other views become separate tiles."],
    ["D. SAP Analytics Cloud story", "Free layout, colours, KPI tiles, multiple charts per page — closest to the mockup", "Needs an SAC tenant and a live connection to the CDS queries; users open SAC, not the Fiori tile."],
    ["E. Freestyle SAPUI5 app", "Anything", "Full development and maintenance; not recommended for this scope."],
  ],
  [2600, 3400, 3360]
));
c.push(SP());
c.push(P("Recommendation: deliver A now, and if the performance table must look exactly like the Excel, add a single custom card (B) for that table later. Everything in the back end stays as it is."));

const doc = new Document({
  styles: { default: { document: { run: { font: "Calibri", size: 21 } } } },
  numbering: { config: [
    { reference: "bullets", levels: [{ level: 0, format: LevelFormat.BULLET, text: "•", alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 540, hanging: 270 } } } }] },
    { reference: "steps", levels: [{ level: 0, format: LevelFormat.DECIMAL, text: "%1.", alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 540, hanging: 300 } } } }] },
  ] },
  sections: [{
    properties: { page: { margin: { top: 1200, bottom: 1200, left: 1300, right: 1300 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER,
      children: [new TextRun({ text: "ONGC Videsh — DPR Analytical RAP — Fiori Overview Page limits and layout — page ", size: 16, color: "808080" }),
                 new TextRun({ children: [PageNumber.CURRENT], size: 16, color: "808080" })] })] }) },
    children: c,
  }],
});
Packer.toBuffer(doc).then((b) => { fs.writeFileSync(OUT, b); console.log("written", OUT, b.length); });
