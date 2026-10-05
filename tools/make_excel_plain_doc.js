const fs = require("fs");
const path = require("path");
const {
  Document, Packer, Paragraph, TextRun, HeadingLevel, AlignmentType,
  Table, TableRow, TableCell, WidthType, ShadingType,
  PageBreak, TableOfContents, LevelFormat, Footer, PageNumber,
} = require("docx");

const ROOT = path.dirname(__dirname);
const OUT = path.join(ROOT, "deploy", "Production_Dashboard_Excel_to_BTP_Plain_Guide.docx");
const BLUE = "1F4E79", GREY = "F2F2F2", GREEN = "E2EFDA", AMBER = "FFF2CC", RED = "FCE4D6";
const H1 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_1, spacing: { before: 320, after: 160 } });
const H2 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_2, spacing: { before: 260, after: 120 } });
const P = (t, o = {}) => new Paragraph({ spacing: { after: 120 },
  children: [new TextRun({ text: t, size: 22, bold: !!o.bold, italics: !!o.italics })] });
const BULLET = (t) => new Paragraph({ numbering: { reference: "bullets", level: 0 }, spacing: { after: 80 },
  children: [new TextRun({ text: t, size: 22 })] });
const PB = () => new Paragraph({ children: [new PageBreak()] });
const SP = () => new Paragraph({ spacing: { after: 140 } });
function box(label, text, fill) {
  return new Table({ width: { size: 9360, type: WidthType.DXA }, columnWidths: [9360],
    rows: [new TableRow({ children: [new TableCell({ width: { size: 9360, type: WidthType.DXA },
      shading: { type: ShadingType.CLEAR, fill }, margins: { top: 110, bottom: 110, left: 160, right: 160 },
      children: [new Paragraph({ spacing: { after: 0 }, children: [
        new TextRun({ text: label + "  ", bold: true, size: 22 }), new TextRun({ text, size: 22 }) ] })] })] })] });
}
const fillFor = (t) => t.startsWith("Yes") ? GREEN : t.startsWith("Partly") ? AMBER : t.startsWith("No") ? RED : undefined;
function table(headers, rows, widths, colorCol) {
  const total = widths.reduce((a, b) => a + b, 0);
  const mk = (txt, bold, fill, w) => new TableCell({ width: { size: w, type: WidthType.DXA },
    shading: fill ? { type: ShadingType.CLEAR, fill } : undefined, margins: { top: 70, bottom: 70, left: 110, right: 110 },
    children: [new Paragraph({ spacing: { after: 0 },
      children: [new TextRun({ text: txt, bold, size: 19, color: bold ? "FFFFFF" : undefined })] })] });
  return new Table({ width: { size: total, type: WidthType.DXA }, columnWidths: widths, rows: [
    new TableRow({ tableHeader: true, children: headers.map((h, i) => mk(h, true, BLUE, widths[i])) }),
    ...rows.map((r, ri) => new TableRow({ children: r.map((cell, i) =>
      mk(cell, false, (colorCol === i ? fillFor(cell) : undefined) || (ri % 2 ? GREY : "FFFFFF"), widths[i])) })) ] });
}
/* tab card: one block per tab */
function tab(name, shows, today, sap, gaps, proposal) {
  const out = [];
  out.push(H2(name));
  out.push(P("What this tab shows: " + shows));
  out.push(P("Where the numbers come from today: " + today));
  out.push(box("Available from SAP:", sap, GREEN));
  out.push(SP());
  if (gaps) { out.push(box("Not in SAP today:", gaps, RED)); out.push(SP()); }
  out.push(box("Proposal for the BTP dashboard:", proposal, AMBER));
  out.push(SP());
  return out;
}

const c = [];
c.push(
  new Paragraph({ spacing: { before: 2000 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "ONGC Videsh — Production Dashboard", bold: true, size: 44, color: BLUE })] }),
  new Paragraph({ spacing: { before: 160 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Moving the Excel dashboard to SAP BTP", bold: true, size: 32, color: "404040" })] }),
  new Paragraph({ spacing: { before: 160 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Tab by tab: what SAP already has, what is missing, and how we propose to close each gap", size: 24, color: "404040" })] }),
  new Paragraph({ spacing: { before: 500 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Based on Production_Dashboard FY27 (31.08.2026).xlsm — written for the business user", size: 22, italics: true, color: "606060" })] }),
  PB()
);
c.push(H1("Contents"));
c.push(new TableOfContents("Contents", { hyperlink: true, headingStyleRange: "1-2" }));
c.push(PB());

/* 1 */
c.push(H1("1. The short answer"));
c.push(P("Your Excel dashboard can be rebuilt as a web dashboard on SAP BTP. About nine out of ten numbers in the workbook already exist in SAP: the daily production you type every morning, the monthly reconciled production, the targets, and the ownership shares. What SAP does not have today is mostly text and a few hand-kept figures: the daily remarks, the partner data on wells and water, the rounded figures from Corporate Planning, the standardised rates, and some manual adjustments. Each of these can be given a small entry screen in SAP, or dropped, and the choice is yours."));
c.push(P("The words used in this document:", { bold: true }));
c.push(table(["Term", "Meaning"], [
  ["SAP daily production", "The production figures per asset and day that are recorded in SAP (the same figures the DPR report prints)."],
  ["SAP monthly reconciled production", "The final monthly figures per asset after reconciliation with the partner."],
  ["SAP targets", "Monthly targets per asset for the Budget Estimate, Revised Estimate and MoU versions."],
  ["SAP ownership share (PI)", "ONGC Videsh's percentage in each asset, with the dates from which each percentage applies."],
  ["Conversion factor", "Barrels per tonne of oil for each asset, used to convert barrels into tonnes."],
  ["Rule", "A calculation that is not stored anywhere but is built into the dashboard, for example 'OVL share = JV production × ownership share'."],
  ["Entry screen", "A simple web screen where an authorised user types or uploads data that SAP does not have (for example remarks)."],
], [2600, 6760]));
c.push(SP());
c.push(table(["Colour code used below", "Meaning"], [
  ["Yes — available from SAP", "The dashboard can show it straight away."],
  ["Partly — needs a rule", "The figures are in SAP, but a calculation from the workbook has to be rebuilt in the dashboard."],
  ["No — not maintained in SAP", "Needs an entry screen, an upload, or a decision to leave it out."],
], [3200, 6160], 0));
c.push(PB());

/* 2 */
c.push(H1("2. How the workbook is organised"));
c.push(P("The 64 tabs fall into three groups. Only the first group is what people look at; the other two are the workbook's engine and would disappear in a dashboard because SAP does that work."));
c.push(table(["Group", "Tabs", "In the BTP dashboard"], [
  ["Pages people read", "Dashboard, mtd&ytd, YTD_ach, Asking Rate, Internal, Quarterly / Monthly / Yearly Analysis, YoY, Gap_analysis, Mat_Balance, Comparison_between_ref_points, Approval_format, report_prodcompiled, the five Charts tabs, Graphs_full, all_remarks", "Become dashboard pages and cards (section 3)."],
  ["Data that is typed or pasted in", "DPR (daily), the 14 asset CP sheets (monthly), Storage_Targets (targets), Storage_rounded_off_prod (Corporate Planning figures), Standardised rates, carryforward_cf, Prodn Data 24-25", "Replaced by SAP data where it exists; the rest gets an entry screen (section 4)."],
  ["Helper tabs (formulas and switches)", "DPRavg, non-reconprod, prod_compiled, Processed_Reconciled Prod, from CP sheet, CF_lat, YTD display, YTDtarget, BEREMOU, Targets-chosen, Target_monthwise, DTR, Tables, Storage_Reconciled_prod, Storage_Reconciled_prod_chosen, interFY_transition, Dropdowns, 2FY, LBL, OperProdn Loss, Previous year, Previous Comp, reference_points", "Not needed as tabs. Their calculations become rules inside the dashboard (section 5); their switches become filter fields at the top of the page."],
], [2200, 4400, 2760]));
c.push(PB());

/* 3 */
c.push(H1("3. The pages people read — tab by tab"));
c.push(...tab("Dashboard",
  "For the chosen date, each asset's production rate (oil in BOPD, gas in MMSCMD) next to a comparison figure you pick from a dropdown (annual target, monthly target, MTD actual, previous month, same month last year, and so on). On the right the OVL-share totals, the remarks of the day and the asking-rate summary.",
  "Today's rates are looked up from the DPR tab; comparisons from the helper tabs; two comparison figures (cells L7, L8) are typed by hand.",
  "Yes — the daily rates, the OVL-share totals and all comparison figures can be produced from SAP daily production, SAP targets and SAP ownership shares.",
  "The remark lines ('Prev. day data for …', asset remarks) are free text that exists only in the workbook.",
  "One dashboard page with the date and the comparison dropdown as filters. Remarks come from a new remarks entry screen (see section 4). The typed comparison cells disappear because the dashboard calculates them."));
c.push(...tab("mtd&ytd, YTD_ach, Quarterly / Monthly / Yearly Analysis",
  "Month-to-date, quarter-to-date and year-to-date production against target per asset in MMT (oil), BCM (gas) and MMTOE, with achievement percentages.",
  "Formulas on the reconciled and daily figures and on the targets; nothing typed except the 'Remarks' and 'Major activities' columns, which are currently empty.",
  "Yes — all figures. SAP has the daily and monthly production, the targets and the shares.",
  "The 'Remarks / Production upsides and declines' and 'Major activities during the period' text columns, and the 'Operators Target' column (empty in the workbook).",
  "One 'Performance' page with a period selector (month, quarter, year to date). Free-text columns come from the remarks entry screen. 'Operators Target' can become a fourth target version in SAP if you want it."));
c.push(...tab("Asking Rate and Internal",
  "The daily rate needed for the rest of the year to reach the annual target, per asset and in total; and an internal comparison with extrapolation of the year-to-date to a full year.",
  "Formulas, plus typed annual totals (7.252 MMT, 2.574 BCM), typed BE targets in the Internal tab, and manual 'arrears' additions (+0.068 MMT, +0.014 BCM).",
  "Partly — everything is calculable from SAP targets, production and shares; the asking-rate and extrapolation formulas are rebuilt as rules.",
  "The arrears additions and any other manual corrections.",
  "An 'Asking rate' card on the Performance page. Manual corrections go into a small adjustments entry screen (period, asset, quantity, reason) so they are visible and traceable instead of hidden in a cell."));
c.push(...tab("YoY, Previous Comp, Gap_analysis, Mat_Balance, Comparison_between_ref_points",
  "Comparisons: this year against last year, this month against last month, today's target against today's actual, the change in OVL-share rate between two dates, and any two chosen reference points.",
  "Formulas on the daily and monthly figures; reference points can also be a typed 'standardised rate' per asset.",
  "Yes for all comparisons that use production and targets. Last year's figures are available if SAP keeps the reconciled history (to be confirmed).",
  "The standardised rates (a nominal rate per asset typed by the user) and the loss reference rates.",
  "One 'Comparisons' page with two date pickers and a reference dropdown. Standardised and reference rates get an entry screen (asset, rate type, valid from, value)."));
c.push(...tab("Approval_format and report_prodcompiled",
  "The quarterly, half-yearly and annual reconciled production in JV and OVL-share units, laid out for management approval; and the monthly production table with OVL averages.",
  "Formulas on the partner CP figures.",
  "Yes — from SAP monthly reconciled production and ownership shares, provided the reconciled figures in SAP are loaded from the same partner statements.",
  "The approval itself (who signed, when) is outside the dashboard.",
  "A 'Reconciled production' page with the same quarter / half-year / annual layout and an Export to Excel button, so the approval sheet can still be printed."));
c.push(...tab("Charts-OVL, Charts-Russia, Charts-LAC, Charts-AP, Charts-MENA CIS, Graphs_full",
  "Monthly OVL-share production against target per business unit, and the full-period trend graphs.",
  "Chart series on the helper tabs.",
  "Yes — production and targets. The grouping of assets into business units is a rule (Russia: Sakhalin-1, Vankor, Imperial; LAC: BC-10, MECL, CPO-5, Carabobo, Sancristobal; Asia Pacific: A1/A3, B-06.1; MENA CIS: ACG, LZC, GPOC, SPOC).",
  null,
  "Chart cards on the dashboard with a business-unit filter. Note that web charts use the standard SAP colours and are smaller than the Excel charts; a full-screen chart page is one click away."));
c.push(...tab("all_remarks and LBL",
  "The list of all daily remarks (why production was deferred, shutdowns, 'reason awaited from operator'), and short event labels used on charts.",
  "Typed daily into the DPR tab; LBL has not been maintained since February 2024.",
  "No — SAP holds no remarks for production.",
  "All of it.",
  "A remarks entry screen: date, asset, category (deferment, shutdown, data lag, event, other), text. The dashboard shows the remarks of the selected day and a searchable list. This is the single most important addition, because without it the dashboard shows numbers without reasons."));
c.push(PB());

/* 4 */
c.push(H1("4. The data that is typed in today — where it will come from"));
c.push(table(["Tab", "What is typed in", "In SAP?", "Proposal"], [
  ["DPR", "Daily JV oil (BOPD) and gas (MMSCMD) per asset; 'previous-day data' note; remarks", "Yes for the figures", "Figures come from SAP daily production, no typing. The note and the remarks go to the remarks entry screen."],
  ["ACG, BC10, MECL, A1A3, Sakhalin, IEC, Vankor, Vietnam, Carabobo, Sancristobal, LZC, GPOC, SPOC, CPO5", "The partner's monthly statement: JV barrels, JV gas, OVL tonnes, conversion factor, wells drilled, 2D/3D/4D survey, water cut, water injection, remarks, well details", "Partly — barrels and gas are in SAP monthly reconciled production if it is loaded from the same statements; the rest is not", "Production figures from SAP. For wells, surveys, water cut and water injection: either a monthly 'partner data' entry screen (or Excel upload) or leave them out of the dashboard. Your decision."],
  ["Storage_Targets", "Monthly targets for BE, RE and MoU per asset, and conversion factors per version", "Yes — SAP targets hold BE and RE; MoU has to be matched to one of the SAP MoU versions", "Targets from SAP; a dropdown to pick the version. One question to settle: which SAP version is your 'MoU'."],
  ["Storage_rounded_off_prod", "Corporate Planning's rounded monthly OVL production (3 decimals), used as the official figure", "No — SAP can compute and round the same figure, but if Corporate Planning adjusts numbers the two can differ", "Decide one official source. Preferred: compute in SAP and round. If CP figures can differ, a small 'published figures' entry screen."],
  ["carryforward_cf, CF_lat (column S)", "Year-opening conversion factors and manual overrides", "Partly — SAP has one factor per year and asset", "Compute the monthly factor from the reconciled barrels and tonnes if SAP stores both; otherwise a conversion-factor entry screen with a valid-from date."],
  ["Standardised rates, OperProdn Loss (reference rows)", "Nominal reference rates per asset", "No", "Reference-rate entry screen."],
  ["Internal, prod_compiled, Dashboard", "Manual corrections: ACG arrears, arrears additions, typed comparison figures", "No", "Adjustments entry screen (period, asset, product, quantity, reason)."],
  ["Prodn Data 24-25, Previous year", "Last year's figures kept as a copy", "Yes if SAP keeps the reconciled history", "No action if history exists; to be confirmed with the SAP team."],
], [2100, 2700, 1900, 2660], 2));
c.push(PB());

/* 5 */
c.push(H1("5. Calculations the workbook does that the dashboard must repeat"));
c.push(P("These are not data. They are the formulas hidden in the helper tabs. They will be built once into the dashboard, so they cannot go out of date and nobody has to maintain them."));
c.push(BULLET("OVL share = JV production × ownership share. The share and the date it changed (for example ACG from 2.31 % to 2.925 % in November 2024) come from SAP, so the change no longer has to be typed into formulas."));
c.push(BULLET("Tonnes = barrels ÷ conversion factor. Reconciled months use the month's own factor; the current month uses the last reconciled month's factor."));
c.push(BULLET("Oil-equivalent (MMTOE) = MMT of oil + BCM of gas, added one to one, exactly as in the workbook. Note that the DPR report uses a different convention (6290 barrels per million cubic metres) for its BOEPD total; the dashboard can show either or both."));
c.push(BULLET("Reconciled versus daily: months up to the 'reconciled till' month use reconciled figures, later months use daily figures; quarter and year rates are weighted by days, with the current month counted only up to the selected date. 'Reconciled till' becomes a filter field instead of a typed cell."));
c.push(BULLET("Targets in daily rates: monthly OVL target ÷ ownership share × conversion factor ÷ days in the month. Year-to-date target = completed months + the current month in proportion to the days elapsed."));
c.push(BULLET("Asking rate = (annual target − year-to-date actual) ÷ remaining days. Extrapolation = year-to-date × 365 ÷ days elapsed."));
c.push(BULLET("Operational loss = reference rate − actual rate (never below zero), × ownership share."));
c.push(BULLET("Rounding: reconciled MMT and BCM shown to three decimals, as today."));
c.push(PB());

/* 6 */
c.push(H1("6. What the BTP dashboard would contain"));
c.push(table(["Page", "Cards / content", "Replaces these tabs"], [
  ["Overview", "Today's rate per asset vs chosen comparison; OVL totals; BE-target line chart; remarks of the day; asking-rate summary", "Dashboard, Graphs_full"],
  ["Performance", "MTD / QTD / YTD vs target per asset, achievement %, MMTOE; quarterly, monthly and yearly views; asking rate; extrapolation", "mtd&ytd, YTD_ach, Quarterly / Monthly / Yearly Analysis, Asking Rate, Internal"],
  ["Comparisons", "Year-on-year, month-on-month, gap to target for a date, change between two dates, reference-point comparison", "YoY, Previous Comp, Gap_analysis, Mat_Balance, Comparison_between_ref_points"],
  ["Reconciled production", "Monthly, quarterly, half-year and annual reconciled figures in JV and OVL units; export to Excel", "Approval_format, report_prodcompiled, Storage tabs"],
  ["Business units", "Monthly OVL production vs target per BU with charts", "Charts-Russia, Charts-LAC, Charts-AP, Charts-MENA CIS, Charts-OVL"],
  ["Remarks", "Searchable list of remarks and events; entry screen for authorised users", "all_remarks, LBL, DPR remark columns"],
  ["Maintenance (authorised users only)", "Entry screens: remarks, reference rates, adjustments, published figures, partner monthly data (if kept)", "Standardised rates, carryforward_cf, Storage_rounded_off_prod, CP sheet non-production columns"],
], [2000, 4600, 2760]));
c.push(SP());
c.push(P("Filters at the top of every page replace the workbook's typed switches: date, fiscal year, target version (BE / RE / MoU), reconciled-till month, rounded or raw, business unit, asset."));
c.push(PB());

/* 7 */
c.push(H1("7. Decisions we need from you"));
c.push(table(["#", "Question", "Why it matters"], [
  ["1", "Which SAP target version is your 'MoU' (SAP has two MoU versions: Excellent and Very Good)?", "Without it the MoU column cannot be filled."],
  ["2", "Are the monthly reconciled figures in SAP loaded from the same partner statements you paste into the asset tabs, and do they include tonnes as well as barrels?", "Decides whether the conversion factor can be computed automatically or needs an entry screen."],
  ["3", "Which OVL figure is official: the one computed from reconciled production, or Corporate Planning's rounded figure? Can they differ?", "Decides whether a 'published figures' screen is needed."],
  ["4", "Should oil-equivalent be MMT + BCM (as in the workbook) or use the 6290 barrel factor (as in the DPR report), or show both?", "The two give different totals."],
  ["5", "Do you want wells drilled, surveys, water cut and water injection in the dashboard? If yes, who enters them each month?", "This is the only sizeable block of non-production data."],
  ["6", "Does SAP keep last year's reconciled figures?", "Needed for year-on-year comparisons; otherwise a one-time upload."],
  ["7", "Who will maintain remarks, reference rates and adjustments?", "Each entry screen needs an owner."],
  ["8", "Which pages are needed in the first release?", "Lets us deliver the Overview and Performance pages first and the rest after."],
], [500, 5200, 3660]));
c.push(SP());
c.push(box("What happens next:", "Once questions 1 to 4 are answered, the Overview, Performance and Business-unit pages can be built on the production and target data that SAP already has. The remarks entry screen is added in the same step. The other entry screens follow your decisions on questions 5 to 7.", AMBER));

const doc = new Document({
  styles: { default: { document: { run: { font: "Calibri", size: 22 } } } },
  numbering: { config: [
    { reference: "bullets", levels: [{ level: 0, format: LevelFormat.BULLET, text: "•", alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 540, hanging: 270 } } } }] },
  ] },
  sections: [{
    properties: { page: { margin: { top: 1200, bottom: 1200, left: 1300, right: 1300 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER,
      children: [new TextRun({ text: "ONGC Videsh — Production Dashboard: from Excel to SAP BTP — page ", size: 16, color: "808080" }),
                 new TextRun({ children: [PageNumber.CURRENT], size: 16, color: "808080" })] })] }) },
    children: c,
  }],
});
Packer.toBuffer(doc).then((b) => { fs.writeFileSync(OUT, b); console.log("written", OUT, b.length); });
