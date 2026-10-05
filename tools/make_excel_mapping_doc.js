const fs = require("fs");
const path = require("path");
const {
  Document, Packer, Paragraph, TextRun, HeadingLevel, AlignmentType,
  Table, TableRow, TableCell, WidthType, ShadingType, BorderStyle,
  PageBreak, TableOfContents, LevelFormat, Footer, PageNumber,
} = require("docx");

const ROOT = path.dirname(__dirname);
const OUT = path.join(ROOT, "deploy", "Production_Dashboard_Excel_SAP_Data_Mapping.docx");
const BLUE = "1F4E79", GREY = "F2F2F2";
const GREEN = "E2EFDA", AMBER = "FFF2CC", RED = "FCE4D6";
const H1 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_1, spacing: { before: 320, after: 160 } });
const H2 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_2, spacing: { before: 260, after: 120 } });
const P = (t, o = {}) => new Paragraph({ spacing: { after: 120 },
  children: [new TextRun({ text: t, size: 21, bold: !!o.bold, italics: !!o.italics })] });
const BULLET = (t) => new Paragraph({ numbering: { reference: "bullets", level: 0 }, spacing: { after: 80 },
  children: [new TextRun({ text: t, size: 21 })] });
const PB = () => new Paragraph({ children: [new PageBreak()] });
const SP = () => new Paragraph({ spacing: { after: 140 } });
function note(label, text, fill) {
  return new Table({ width: { size: 9360, type: WidthType.DXA }, columnWidths: [9360],
    rows: [new TableRow({ children: [new TableCell({ width: { size: 9360, type: WidthType.DXA },
      shading: { type: ShadingType.CLEAR, fill }, margins: { top: 100, bottom: 100, left: 140, right: 140 },
      children: [new Paragraph({ spacing: { after: 0 }, children: [
        new TextRun({ text: label + "  ", bold: true, size: 21 }), new TextRun({ text, size: 21 }) ] })] })] })] });
}
function table(headers, rows, widths, colorCol) {
  const total = widths.reduce((a, b) => a + b, 0);
  const fillFor = (txt) => txt.startsWith("A") ? GREEN : txt.startsWith("B") ? AMBER : txt.startsWith("C") ? RED : undefined;
  const mk = (txt, bold, fill, w) => new TableCell({ width: { size: w, type: WidthType.DXA },
    shading: fill ? { type: ShadingType.CLEAR, fill } : undefined, margins: { top: 60, bottom: 60, left: 100, right: 100 },
    children: [new Paragraph({ spacing: { after: 0 },
      children: [new TextRun({ text: txt, bold, size: 18, color: bold ? "FFFFFF" : undefined })] })] });
  return new Table({ width: { size: total, type: WidthType.DXA }, columnWidths: widths, rows: [
    new TableRow({ tableHeader: true, children: headers.map((h, i) => mk(h, true, BLUE, widths[i])) }),
    ...rows.map((r, ri) => new TableRow({ children: r.map((c, i) =>
      mk(c, false, (colorCol === i ? fillFor(c) : undefined) || (ri % 2 ? GREY : "FFFFFF"), widths[i])) })) ] });
}

const c = [];
c.push(
  new Paragraph({ spacing: { before: 2000 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "ONGC Videsh — Production Dashboard", bold: true, size: 44, color: BLUE })] }),
  new Paragraph({ spacing: { before: 160 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Workbook Production_Dashboard FY27 (31.08.2026).xlsm", bold: true, size: 28, color: "404040" })] }),
  new Paragraph({ spacing: { before: 160 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Which data comes from SAP, which is a rule, and which is not in SAP at all", bold: true, size: 26, color: "404040" })] }),
  new Paragraph({ spacing: { before: 500 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Analysis of all 64 sheets — basis for the dashboard design and the customer discussion", size: 22, italics: true, color: "606060" })] }),
  PB()
);
c.push(H1("Contents"));
c.push(new TableOfContents("Contents", { hyperlink: true, headingStyleRange: "1-2" }));
c.push(PB());

/* 1 */
c.push(H1("1. Summary"));
c.push(P("The workbook is the customer's working tool for the daily production review and forward planning. It has 64 sheets (21 hidden), one dashboard page, roughly 90,000 formulas and about 60,000 typed values. Every sheet was read and every block of numbers was traced to its origin: typed by hand, computed from another sheet, or linked externally. The only external link points to last year's copy of the same workbook and is not used by any live formula."));
c.push(P("Three classes are used throughout this document:", { bold: true }));
c.push(table(["Class", "Meaning", "Share of the workbook"], [
  ["A — SAP-available", "The figure exists in the SAP tables (daily production, monthly reconciled production, targets, participating interest, asset texts) and only needs unit conversion or aggregation.", "All production and target numbers: the bulk of the data volume."],
  ["B — SAP-partly (a rule)", "Derivable from SAP data, but with a calculation rule or a parameter that lives only in the workbook (reconciled-till month, conversion-factor logic, proration, asking rate, rounding switch, business-unit grouping).", "Almost every derived sheet; the rules are listed in section 5."],
  ["C — Not in SAP", "Typed by hand and not present in any SAP table: remarks and reasons, partner CP-sheet operational data, Corporate Planning rounded figures, standardised rates, reference rates, manual adjustments and overrides, previous-year archive copies.", "Small in volume, but essential to several visible sheets."],
], [2400, 4800, 2160], 0));
c.push(SP());
c.push(note("Bottom line:", "The numbers can come from SAP. What the dashboard cannot get from SAP today is the text (remarks, reasons, activities), the partner CP-sheet operational data (wells, surveys, water cut, water injection), the Corporate Planning rounded production, the standardised and reference rates, and a handful of manual adjustments. Each of these needs either a small custom table with a maintenance screen or a decision to drop it. Section 6 lists them with a proposal; section 7 lists the questions for the customer.", AMBER));
c.push(PB());

/* 2 */
c.push(H1("2. How the workbook works (data flow)"));
c.push(P("There are three independent data chains that meet on the visible pages."));
c.push(H2("2.1 Daily chain (non-reconciled)"));
c.push(BULLET("DPR: one row per calendar day since 1-Apr-2023, JV oil in BOPD and JV gas in MMSCMD per asset, typed daily from the operators' reports, with two text columns: which assets are previous-day/provisional, and remarks. OVL share = JV × PI with a hard-coded ACG PI change (2.31 % → 2.925 % from Nov-2024)."));
c.push(BULLET("DPRavg: monthly sums, days, averages and OVL tonnes/MMT and BCM from DPR, using the latest conversion factor per asset (CF_lat)."));
c.push(BULLET("Dashboard, Gap_analysis, Mat_Balance, OperProdn Loss, reference_points, all_remarks, LBL read DPR directly."));
c.push(H2("2.2 Reconciled chain (partner CP sheets)"));
c.push(BULLET("14 per-asset sheets (ACG, BC10, MECL, A1A3, Sakhalin, IEC, Vankor, Vietnam, Carabobo, Sancristobal, LZC, GPOC, SPOC, CPO5): the partner's monthly statement pasted as values: JV barrels, JV MMSCM, OVL tonnes, conversion factor, plus wells drilled, surveys, water cut, water injection, remarks."));
c.push(BULLET("from CP sheet → Processed_Reconciled Prod → Approval_format (quarterly approval) and CF_lat (latest conversion factor); carryforward_cf keeps the year-opening factors."));
c.push(BULLET("Storage_Reconciled_prod (raw reconciled, filled by macro) and Storage_rounded_off_prod (Corporate Planning figures rounded to 3 decimals, typed) → Storage_Reconciled_prod_chosen (switch) → Previous year, non-reconprod, prod_compiled, report_prodcompiled."));
c.push(BULLET("prod_compiled blends the two chains: months up to the reconciled-till month (July'26, typed) use reconciled figures, later months use DPR averages, day-weighted to the dashboard date."));
c.push(H2("2.3 Target chain"));
c.push(BULLET("Storage_Targets: monthly OVL-share targets for BE, RE and MoU versions (MMT oil, BCM gas) for Apr-2023 to Mar-2027, plus conversion factors per version and year, all typed."));
c.push(BULLET("BEREMOU (choose version) → Targets-chosen (convert to JV BOPD / MMSCMD with PI, CF and days) → Target_monthwise → DTR (daily target rate) and Tables (chart series); YTDtarget prorates to YTD/QTD/MTD."));
c.push(H2("2.4 Visible analysis pages"));
c.push(BULLET("Dashboard (per asset today's rate vs selectable comparison; OVL totals; remarks; asking rate), mtd&ytd, YTD_ach, Asking Rate, Internal, Quarterly / Monthly / Yearly Analysis, YoY, Previous Comp, Gap_analysis, Mat_Balance, Comparison_between_ref_points, Approval_format, report_prodcompiled, and five chart sheets (OVL, Russia, LAC, Asia Pacific, MENA CIS)."));
c.push(PB());

/* 3 */
c.push(H1("3. Sheet-by-sheet mapping"));
c.push(P("Class column: A = SAP-available, B = SAP-partly (rule), C = not in SAP. Where a sheet mixes classes the dominant one is given and the exceptions are named."));
const rows = [
  ["Dashboard", "visible", "Main page: today's JV rate per asset vs comparison basis, OVL totals, remarks, asking-rate summary", "B", "Rates: ZPRA_T_DLY_PRD. OVL: × ZPRA_T_PRD_PI. C: remarks (I10, I11), typed comparison figures L7/L8"],
  ["DPR", "visible (filtered)", "Daily fact table: JV BOPD and MMSCMD per asset since Apr-2023, OVL totals, month key, data-lag note, remarks", "A", "ZPRA_T_DLY_PRD NET_PROD (gas = net of injection). C: columns AI (provisional assets) and AJ (remarks)"],
  ["DPRavg", "hidden", "Monthly sums, days, averages, OVL tonnes/MMT, BCM", "B", "Aggregate of daily; CF from CF_lat (running factor, not the yearly ZPRA_T_TAR_CF); 'reconciled till' typed (F2)"],
  ["all_remarks", "visible", "Chronological list of the DPR remarks", "C", "Free text; needs a remarks table (date, asset, text)"],
  ["LBL", "visible", "Short event labels per asset-day for chart annotation (not maintained since Feb-2024)", "C", "Event/reason table"],
  ["Graphs_full, Charts-*", "visible", "Chart containers per BU and OVL total", "B", "Plots A data; BU grouping is a rule (not in SAP)"],
  ["mtd&ytd", "visible", "MTD/YTD OVL actual vs target, MMT/BCM/MMTOE", "B", "Targets ZPRA_T_PRD_TAR; actuals reconciled+daily blend; MMTOE = MMT + BCM"],
  ["YTD display", "visible", "Lookup matrix of every comparison basis per asset (targets, MTD/QTD/YTD, previous periods)", "B", "All derivable; column index contract used by the dashboard dropdown"],
  ["YTD_ach", "visible", "YTD achievement % vs BE per asset and total (7.252 MMT, 2.574 BCM, 9.826 MMTOE)", "B", "Annual goals are the sum of ZPRA_T_PRD_TAR TAR_BE months"],
  ["Asking Rate", "visible", "Required rate for the rest of the FY to meet the annual target", "B", "Rule: (annual − YTD) / remaining days, via average CF and PI. C: typed annual totals I52/J52"],
  ["Internal", "visible", "Internal YTD comparison, extrapolation to full year, arrears", "C/B", "Typed BE targets (E, K) duplicate SAP; arrears +0.068 MMT / +0.014 BCM and G24 are manual"],
  ["2FY", "visible", "Parameter cells for a two-period chart", "—", "UI only; row offsets stale (assume DPR starts 2022)"],
  ["Gap_analysis", "visible", "Today's target vs actual, JV and OVL, per asset", "A", "Broken: lookup range ends at Apr-2026 (#N/A); uses old ACG PI 2.31 %"],
  ["Dropdowns", "visible", "All selection logic: comparison basis, target type, reconciled-till, rounding switch, row visibility", "C (parameters)", "Equivalent to report parameters / customizing"],
  ["interFY_transition", "hidden", "Fiscal-year calendar master, RY (reconciled year, typed) and DY (dashboard year)", "A/C", "FY variant V3 in SAP; reconciled-till is a manual flag"],
  ["ACG … CPO5 (14 sheets)", "hidden", "Partner CP monthly statement per asset, pasted values", "B/C", "JV bbl/MMSCM and OVL tonnes: ZPRA_T_MREC_PRD if loaded from the same statements. C: conversion factor per month, wells, surveys, water cut, water injection, remarks, well details"],
  ["from CP sheet", "visible", "Consolidation of the three used CP columns into asset × month matrices", "B", "PI typed in column B (single value, no validity)"],
  ["Processed_Reconciled Prod", "visible", "Reconciled figures in reporting units, monthly conversion factor, average PI", "B", "CF = OVL bbl / OVL tonnes per month (not in SAP); carry-forward CF column"],
  ["Approval_format", "visible", "Quarterly / half-year / annual reconciled summary for management approval", "B", "Fully derived; the approval itself is outside SAP"],
  ["CF_lat", "hidden", "Latest conversion factor per asset (last reconciled month), manual override column S and x-factor", "B/C", "Override values are manual"],
  ["carryforward_cf", "visible", "Year-opening conversion factors per asset and FY", "C", "Typed; conceptually ZPRA_T_TAR_CF if that table holds actual carry-forward factors"],
  ["non-reconprod", "visible", "Side-by-side non-reconciled (DPR) and reconciled months, selector row", "B", "Rule: month ≤ reconciled-till → reconciled, else daily"],
  ["prod_compiled", "visible", "Blended monthly series with Q/H/9M/annual, YTD/QTD/MTD, previous periods", "B", "Day-weighted rates, partial current month; C: ACG Nov-2024 arrears block (P3, P21, P38, P56); MINIFS formulas broken (#NAME?)"],
  ["report_prodcompiled", "visible", "Report layout of prod_compiled with OVL-weighted rows", "B", "PI typed; ACG PI switch by FY flag"],
  ["Previous year", "visible", "Prior-FY monthly reconciled figures for YoY", "A", "ZPRA_T_MREC_PRD history (if kept) × PI"],
  ["Prodn Data 24-25", "visible", "Frozen copy of FY 2024-25 report, typed", "C (archive)", "Reproducible from ZPRA_T_MREC_PRD FY 2024-25"],
  ["Storage_Targets", "visible", "Master store of BE / RE / MoU monthly OVL targets and CF per version, typed", "A/B", "ZPRA_T_PRD_TAR (TAR_BE, TAR_RE, MoU = TAR_EX or TAR_VG to be decided); CF per version not in ZPRA_T_TAR_CF"],
  ["BEREMOU", "visible", "Selects the FY and the target version", "A", "Parameter: target code. Typed zeros override MECL gas"],
  ["Targets-chosen", "visible", "Chosen targets to Q/H/annual and to JV BOPD / MMSCMD", "B", "Rule: JV BOPD = OVL MMT / PI × CF × 1e6 / days; PI typed; OVL average row omits 4 assets"],
  ["Target_monthwise", "hidden", "12-month layout keyed YYYYMM", "A", "Reshaping only; MMTOE = MMT + BCM"],
  ["YTDtarget", "visible", "YTD / QTD / MTD target proration to the dashboard date", "B", "Rule: completed months + current month × days elapsed / days in month"],
  ["DTR", "hidden", "Daily target rate per calendar day", "A", "Monthly target as daily rate; PI row stale (ACG 2.31 %)"],
  ["Tables", "hidden", "Target vs actual monthly series per asset for charts", "A", "Derived"],
  ["OperProdn Loss", "hidden", "Daily loss = max(0, reference − actual) × PI", "C/A", "Reference rows 9–11 typed; no loss categories; rows from 20-Jul-2025 are #REF!"],
  ["Quarterly / Monthly / Yearly Analysis", "visible", "Target vs actual per asset for the period, MMTOE, remarks, major activities", "B", "C: 'Operators Target', remarks, major activities (all empty text columns)"],
  ["YoY", "visible", "Year-on-year YTD OVL comparison", "A", "Previous year from reconciled history"],
  ["Previous Comp", "hidden", "Current MTD vs previous month per-day rates", "A", "Derived"],
  ["Mat_Balance", "visible", "Change in OVL-share rate between two dates", "A", "Two daily rows × PI; currently #N/A (dates outside range)"],
  ["Standardised rates", "visible", "Nominal reference JV rate per asset (oil and gas), typed", "C", "New custom table"],
  ["reference_points", "visible", "Candidate reference points (months, quarters, DPR date, any dates, standardised)", "B/C", "Standardised column is C"],
  ["Comparison_between_ref_points", "visible", "Compare two chosen reference points per asset", "B", "User choice of references"],
  ["Storage_Reconciled_prod", "visible", "Raw reconciled production store (macro-filled)", "A", "ZPRA_T_MREC_PRD ÷ days, × PI, ÷ CF"],
  ["Storage_rounded_off_prod", "visible", "Corporate Planning rounded production (3 decimals), typed", "C", "Official published figures; may differ from computed"],
  ["Storage_Reconciled_prod_chosen", "visible", "Single feed: JV from raw, OVL volumes from raw or rounded per switch", "B", "Parameter: rounded / raw"],
];
c.push(table(["Sheet", "State", "Purpose", "Class", "SAP source / exceptions"], rows, [1700, 900, 3000, 700, 3060], 3));
c.push(PB());

/* 4 */
c.push(H1("4. What is available from SAP (class A)"));
c.push(table(["Workbook data", "SAP source", "Conversion needed"], [
  ["Daily JV oil (BOPD) and gas (MMSCMD) per asset — DPR columns D:Q and S:AF", "ZPRA_T_DLY_PRD, volume type NET_PROD (gas: GROSS_PROD − GAS_INJ, as the classic DPR report does)", "Units to bbl/day and MMSCMD (MCF ÷ 35.3, M3 ÷ 1e6)"],
  ["Monthly reconciled JV barrels and MMSCM — CP sheets columns D and H, Storage_Reconciled_prod", "ZPRA_T_MREC_PRD", "÷ days for rates; unit to MMSCM"],
  ["Participating interest per asset (typed in 6 places)", "ZPRA_T_PRD_PI with validity dates (removes the hard-coded ACG 1-Nov / 29-Nov-2024 switch)", "None"],
  ["Monthly targets BE / RE / MoU, OVL share MMT and BCM — Storage_Targets", "ZPRA_T_PRD_TAR, target codes TAR_BE, TAR_RE, TAR_EX or TAR_VG", "MoU must be mapped to one code; targets are MMT/BCM per month, JV rates by the rule in 5.3"],
  ["Conversion factor tonne → barrel per year and asset", "ZPRA_T_TAR_CF", "Only one factor per year; the workbook keeps one per target version and a monthly actual factor (section 5.2)"],
  ["Asset names, country", "ZOIU_PR_DN, ZPRA_C_DPR_PROF", "Business-unit grouping is not stored (section 5.6)"],
  ["Fiscal calendar (FY Apr–Mar, quarters, Feb days)", "Fiscal year variant of company code OVL", "None"],
  ["Previous-year monthly figures (Previous year, YoY, Prodn Data 24-25)", "ZPRA_T_MREC_PRD history", "Only if history is kept in the table; to be confirmed"],
], [3400, 3300, 2660]));
c.push(PB());

/* 5 */
c.push(H1("5. Rules that exist only in the workbook (class B)"));
c.push(P("These are not data. They must be implemented in the CDS views or in ABAP and, where they are switches, become parameters or customizing."));
c.push(H2("5.1 OVL share"));
c.push(BULLET("OVL quantity = JV quantity × PI. Tonnes = barrels ÷ conversion factor. Gas BCM = MMSCM × PI ÷ 1000. Oil-equivalent (MMTOE) = MMT + BCM added one to one, no 6290 barrel factor. This differs from the classic DPR report, which uses 6290 BOE per MMSCM for the BOEPD total."));
c.push(H2("5.2 Conversion factor logic"));
c.push(BULLET("Reconciled months: the factor is computed per month as OVL barrels ÷ OVL tonnes from the partner statement."));
c.push(BULLET("Non-reconciled months (daily data): the factor of the last reconciled month per asset (CF_lat) is used; if that month had zero production, a manual override is typed."));
c.push(BULLET("At the fiscal-year change the previous March factor is carried forward (carryforward_cf); the average factor is MMT-weighted."));
c.push(BULLET("Targets carry their own factor per version and year (Storage_Targets CF blocks)."));
c.push(H2("5.3 Targets"));
c.push(BULLET("Target version choice: exactly one of BE / RE / MoU (parameter)."));
c.push(BULLET("JV BOPD target = OVL MMT ÷ PI × CF × 1,000,000 ÷ days in month; JV MMSCMD target = OVL BCM ÷ PI × 1000 ÷ days in month."));
c.push(BULLET("YTD target = completed months + current month × (days elapsed ÷ days in month); QTD and MTD likewise. The monthly rate is used as the daily target rate (DTR)."));
c.push(H2("5.4 Reconciled versus daily data"));
c.push(BULLET("A 'reconciled till' month is set by hand (July'26). Months up to it use reconciled data; later months use daily averages. Quarter, half-year and annual rates are day-weighted, with the current month counted only up to the dashboard date."));
c.push(BULLET("OVL volumes come either from the raw reconciled store or from the Corporate Planning rounded store, by a 0/1 switch; JV rates always from the raw store. Reconciled MMT/BCM are rounded to 3 decimals, daily months are not."));
c.push(H2("5.5 Analysis formulas"));
c.push(BULLET("Asking rate = (annual target − YTD actual) ÷ remaining days of the FY, converted with the average factor and PI; remaining days from a fixed 365/366."));
c.push(BULLET("Extrapolation (Internal) = YTD × 365 ÷ days elapsed; second-half projection likewise."));
c.push(BULLET("Operational loss = max(0, reference rate − actual rate) × PI, reference chosen among three typed rows."));
c.push(BULLET("Material balance = difference of (rate × PI) between two dates. Previous-month comparison = ratio of per-day rates."));
c.push(H2("5.6 Groupings and parameters"));
c.push(BULLET("Business units: Russia = Sakhalin-1, Vankor, Imperial; LAC = BC-10, MECL, CPO-5, Carabobo, Sancristobal; Asia Pacific = A1/A3, B-06.1; MENA CIS = ACG, LZC, GPOC, SPOC. Not stored in SAP; the CDS views derive it from the asset-code prefix."));
c.push(BULLET("Dashboard parameters: date, comparison basis (11 options: annual/quarterly/monthly target, MTD/QTD/YTD actual, previous month/quarter/year, same period last year), target version, rounded/raw, reconciled-till month."));
c.push(PB());

/* 6 */
c.push(H1("6. What is not in SAP (class C) and what to do about it"));
c.push(table(["Data", "Where in the workbook", "Proposal"], [
  ["Daily remarks and reasons per asset (deferments, shutdowns, 'reason awaited from operator')", "DPR column AJ, all_remarks, Quarterly/Monthly/Yearly 'Remarks' and 'Major Activities'", "New table ZDPR_T_REMARK (date, asset, category, text) with a simple Fiori maintenance app; shown on the dashboard record cards"],
  ["Provisional / previous-day flag per asset and day", "DPR column AI", "Either a flag column in the remarks table or derived: a daily record whose value equals the previous day and was posted later is provisional"],
  ["Event labels for charts", "LBL", "Same remarks table with category 'event'; optional"],
  ["Partner CP-sheet operational data: wells drilled (development, exploratory, other), 2D/3D/4D survey, water cut %, water injection planned/actual, well details", "Per-asset sheets columns L:W", "Not production data. Either a second custom table loaded monthly with the CP statement, or out of scope for the production dashboard. Customer decision."],
  ["Corporate Planning rounded production (3 decimals) used as the official OVL figure", "Storage_rounded_off_prod", "Decide one source of truth. Preferred: compute from ZPRA_T_MREC_PRD and round in the view; if CP figures may deviate, a small table of published figures per month/asset is needed"],
  ["Monthly actual conversion factor and carry-forward / override factors", "Processed_Reconciled Prod, CF_lat column S and x-factor, carryforward_cf", "Compute the monthly factor from ZPRA_T_MREC_PRD if it stores both barrels and tonnes; otherwise extend ZPRA_T_TAR_CF with a period or add a factor table with month validity"],
  ["Manual adjustments: ACG arrears Nov-2024, arrears +0.068 MMT / +0.014 BCM, typed comparison values on the dashboard", "prod_compiled P3..P56, Internal row 24/42, Dashboard L7/L8", "Adjustment table (period, asset, product, quantity, reason) or agree that adjustments are posted into ZPRA_T_MREC_PRD"],
  ["Standardised rates and loss reference rates", "Standardised rates, OperProdn Loss rows 9–11", "New table ZDPR_T_REFRATE (asset, product, rate type, valid from, rate) with maintenance"],
  ["'Operators Target'", "Quarterly/Monthly/Yearly Analysis column F (empty)", "Could be a further target code in ZPRA_T_PRD_TAR (e.g. TAR_OP) if the customer wants it"],
  ["Approval sign-off", "Approval_format", "Outside the dashboard; the figures themselves are derivable"],
  ["Archive copies of previous years", "Prodn Data 24-25, external link to 2024 workbook", "Not needed if ZPRA_T_MREC_PRD keeps history"],
], [3200, 2600, 3560]));
c.push(PB());

/* 7 */
c.push(H1("7. Questions for the customer"));
c.push(BULLET("Which target code in SAP corresponds to the workbook's MoU version: TAR_EX or TAR_VG? Are TAR_IN and TAR_PC used at all?"));
c.push(BULLET("Is ZPRA_T_MREC_PRD loaded from the same partner CP statements as the per-asset sheets, and does it hold tonnes as well as barrels (so the monthly conversion factor can be computed)? Does it keep prior fiscal years?"));
c.push(BULLET("Which OVL figure is official: computed from reconciled JV × PI, or the Corporate Planning rounded figure? Can they differ?"));
c.push(BULLET("Oil-equivalent: the workbook adds MMT and BCM one to one; the DPR report uses 6290 BOE per MMSCM. Which convention should the dashboard show, or both?"));
c.push(BULLET("Should the operational CP data (wells, surveys, water cut, water injection) be part of the dashboard? If yes, who maintains it in SAP?"));
c.push(BULLET("Are the annual goals 7.252 MMT / 2.574 BCM / 9.826 MMTOE exactly the sum of the TAR_BE months in ZPRA_T_PRD_TAR, or a separately approved figure?"));
c.push(BULLET("Who will maintain remarks, standardised rates and manual adjustments once they live in SAP tables?"));
c.push(BULLET("Which analysis pages are needed in the dashboard's first release: Dashboard page, YTD achievement, asking rate, MTD/YTD, gap analysis, YoY, quarterly/monthly/yearly analysis, material balance, reference-point comparison?"));

/* 8 */
c.push(H1("8. Defects found in the workbook (for information)"));
c.push(BULLET("Gap_analysis looks up DPR only to April 2026: every current value is #N/A. It also uses the old ACG PI 2.31 %."));
c.push(BULLET("OperProdn Loss: all rows from 20-Jul-2025 reference deleted DPR rows (#REF!)."));
c.push(BULLET("all_remarks starts at Aug-2025 and ends with about 50 #REF! rows; LBL not maintained since Feb-2024."));
c.push(BULLET("prod_compiled uses MINIFS, which the saved Excel version does not support (#NAME? in the QTD/MTD day counters)."));
c.push(BULLET("Targets-chosen OVL average rows omit GPOC, SPOC, B-06.1 and A1/A3; a blank string in J57 produces #VALUE! downstream."));
c.push(BULLET("ACG PI change date is 1-Nov-2024 in DPR and reference_points but 29-Nov-2024 in Internal; DTR and Gap_analysis still use 2.31 %."));
c.push(BULLET("Dashboard L7/L8 comparison figures and Asking Rate annual totals are typed, so they go stale when targets change."));
c.push(BULLET("Yearly_Analysis H63 shows #DIV/0! (B-06.1 has no target); 2FY row offsets assume the DPR still starts in April 2022."));
c.push(P("None of these affect the SAP mapping, but they explain differences the customer may see when comparing workbook and dashboard figures.", { italics: true }));

const doc = new Document({
  styles: { default: { document: { run: { font: "Calibri", size: 21 } } } },
  numbering: { config: [
    { reference: "bullets", levels: [{ level: 0, format: LevelFormat.BULLET, text: "•", alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 540, hanging: 270 } } } }] },
  ] },
  sections: [{
    properties: { page: { margin: { top: 1200, bottom: 1200, left: 1300, right: 1300 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER,
      children: [new TextRun({ text: "ONGC Videsh — Production Dashboard workbook — SAP data mapping — page ", size: 16, color: "808080" }),
                 new TextRun({ children: [PageNumber.CURRENT], size: 16, color: "808080" })] })] }) },
    children: c,
  }],
});
Packer.toBuffer(doc).then((b) => { fs.writeFileSync(OUT, b); console.log("written", OUT, b.length); });
