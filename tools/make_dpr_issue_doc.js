const fs = require("fs");
const path = require("path");
const {
  Document, Packer, Paragraph, TextRun, HeadingLevel, AlignmentType,
  Table, TableRow, TableCell, WidthType, ShadingType, ImageRun,
  PageBreak, LevelFormat, Footer, PageNumber,
} = require("docx");

const ROOT = path.dirname(__dirname);
const OUT = path.join(ROOT, "deploy", "DPR_Report_Issues_01-Oct-2026_Analysis.docx");
const IMG = "/tmp/claude-0/-home-user-ONGC-CST-Purchase-Date-Sharing-/93e6a02b-96d7-5d27-9b5c-f70e165e3d5b/scratchpad/msg/";
const BLUE = "1F4E79", GREY = "F2F2F2", GREEN = "E2EFDA", AMBER = "FFF2CC", RED = "FCE4D6";
const H1 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_1, spacing: { before: 320, after: 160 } });
const H2 = (t) => new Paragraph({ text: t, heading: HeadingLevel.HEADING_2, spacing: { before: 240, after: 120 } });
const P = (t, o = {}) => new Paragraph({ spacing: { after: 120 },
  children: [new TextRun({ text: t, size: 21, bold: !!o.bold, italics: !!o.italics })] });
const BULLET = (t) => new Paragraph({ numbering: { reference: "bullets", level: 0 }, spacing: { after: 80 },
  children: [new TextRun({ text: t, size: 21 })] });
const PB = () => new Paragraph({ children: [new PageBreak()] });
const SP = () => new Paragraph({ spacing: { after: 140 } });
const fillFor = (t) => t.startsWith("Data") ? AMBER : t.startsWith("Program") ? RED : t.startsWith("Both") ? RED : undefined;
function table(headers, rows, widths, colorCol) {
  const total = widths.reduce((a, b) => a + b, 0);
  const mk = (txt, bold, fill, w) => new TableCell({ width: { size: w, type: WidthType.DXA },
    shading: fill ? { type: ShadingType.CLEAR, fill } : undefined, margins: { top: 60, bottom: 60, left: 100, right: 100 },
    children: [new Paragraph({ spacing: { after: 0 },
      children: [new TextRun({ text: txt, bold, size: 18, color: bold ? "FFFFFF" : undefined })] })] });
  return new Table({ width: { size: total, type: WidthType.DXA }, columnWidths: widths, rows: [
    new TableRow({ tableHeader: true, children: headers.map((h, i) => mk(h, true, BLUE, widths[i])) }),
    ...rows.map((r, ri) => new TableRow({ children: r.map((cell, i) =>
      mk(cell, false, (colorCol === i ? fillFor(cell) : undefined) || (ri % 2 ? GREY : "FFFFFF"), widths[i])) })) ] });
}
function box(label, text, fill) {
  return new Table({ width: { size: 9360, type: WidthType.DXA }, columnWidths: [9360],
    rows: [new TableRow({ children: [new TableCell({ width: { size: 9360, type: WidthType.DXA },
      shading: { type: ShadingType.CLEAR, fill }, margins: { top: 100, bottom: 100, left: 140, right: 140 },
      children: [new Paragraph({ spacing: { after: 0 }, children: [
        new TextRun({ text: label + "  ", bold: true, size: 21 }), new TextRun({ text, size: 21 }) ] })] })] })] });
}
const img = (f, w, h) => new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 120 },
  children: [new ImageRun({ type: "png", data: fs.readFileSync(path.join(IMG, f)), transformation: { width: w, height: h } })] });

const c = [];
c.push(
  new Paragraph({ spacing: { before: 2000 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "ONGC Videsh — SAP Daily Production Report", bold: true, size: 44, color: BLUE })] }),
  new Paragraph({ spacing: { before: 160 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Issues reported on 01-Oct-2026 (DPR for September 2026)", bold: true, size: 30, color: "404040" })] }),
  new Paragraph({ spacing: { before: 160 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Analysis of the screenshots, root cause per item, and proposed corrections", size: 24, color: "404040" })] }),
  new Paragraph({ spacing: { before: 500 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Reference: e-mail 'Error in SAP Entry for Daily production Report' from GM (P), Management & Technical Cell, forwarded by SAP India on 01-Oct-2026", size: 20, italics: true, color: "606060" })] }),
  PB()
);

c.push(H1("1. What was reported"));
c.push(P("The Technical Cell circulated the SAP DPR output of 01-Oct-2026 15:41 (September 2026, oil and gas sheets and the remarks sheet) and marked a number of cells as false or missing. Because of these figures the SAP DPR is currently not being circulated and the old Excel is used instead. Three screenshots were attached; the circled cells are reproduced below."));
c.push(img("image.png", 620, 350));
c.push(img("Screenshot 2026-10-01 155639.png", 620, 427));
c.push(img("Screenshot 2026-10-01 155702.png", 560, 184));
c.push(PB());

c.push(H1("2. Findings in one table"));
c.push(P("Each marked cell was traced to either the data entered in SAP or to the behaviour of the report program ZPRA_DPR_REPORT. Colour: amber = data entry, red = program behaviour."));
c.push(table(["#", "What is seen", "Cause", "Explanation"], [
  ["1", "Brazil BC-60 oil 123,840 and gas 5.326 on every day of September, in yellow", "Program (carry-forward)", "No BC-60 entry exists after 31-Aug. The report fills a missing day with the previous day's cell and colours it yellow, and it does this day after day without limit. A full month of copied values is counted in MTD and YTD totals."],
  ["2", "30-Sep row: BC-10, CPO-5, MECL, Vankor, GPOC, SPOC oil in yellow (same as 29-Sep)", "Expected behaviour", "Previous-day data: these assets had not reported for 30-Sep when the report ran at 15:41 on 01-Oct. The yellow colour marks this correctly. The Excel adds the note 'Prev. day data for …'; the SAP report does not."],
  ["3", "Azerbaijan ACG oil 30-Sep = 203,033 (normal 310,000–340,000)", "Data entry (incomplete)", "ACG is entered as several blocks; the four identical ACG remarks for 30-Sep show four rows. One block's row is missing or entered low, so the asset total is understated. The report only carries forward when the whole asset is missing, so a partial asset is shown as a real figure without any marking."],
  ["4", "Venezuela Carabobo oil 29-Sep = 7 (normal about 7,000) and gas 29-Sep = 0.006 (normal about 0.19)", "Data entry (wrong value)", "Values entered a thousand times too small, most likely entered in thousands or with a wrong unit. The report has no plausibility check, so the figure is printed as entered."],
  ["5", "ACG gas 30-Sep = 834,117.965; Total Gas = −24,393.514 MMSCMD; Total BOEPD = −153,286,799; monthly average ACG gas 27,797", "Data entry (unit) + Program (no guard)", "The ACG gas quantity for 30-Sep was entered about 100,000 times too large, most likely a cubic-metre figure entered with unit MMSCM, or gross and injection swapped. Net gas is gross minus injection, so the ACG net becomes a large negative number; multiplied by the 2.925 % share it produces the negative totals. The program accepts negative net gas and negative totals without warning."],
  ["6", "Vankor gas 29-Sep and 30-Sep = 0.000, Sakhalin-1 gas 30-Sep = 0.000, not in yellow", "Data entry (zero or injection only)", "A white cell with 0.000 means a gas row exists for that day with net zero: either zero was entered, or only the injection row was entered and the gross row is missing. Because a row exists, the report does not carry forward and does not mark the cell."],
  ["7", "Remarks: ACG 'Allocated Measured Theoretical Potential' printed four times for 30-Sep", "Program (no de-duplication)", "The remarks sheet prints the comment of every data row of the day. ACG has four rows (blocks / volume types) with the same comment, so it appears four times."],
  ["8", "Remarks: Vankor text cut off at '… failure of the oil ga'", "Data entry field length", "The comment field of the daily production table is limited in length; the text was truncated when it was saved, not by the report."],
  ["9", "Remarks: SPOC '97578.02 | 0.584 | 8.57 | 12.5 | 88901.888 | 0.625 | 7.808'", "Data entry", "Numbers were pasted into the remarks field instead of a sentence. The report prints the field as stored."],
], [400, 2800, 1700, 4460], 2));
c.push(PB());

c.push(H1("3. How the report fills missing days (why the yellow cells appear)"));
c.push(P("In the program, the sheet-1 table is built day by day. After each day the form fill_null_values_with_previous looks at every asset column: if the cell is still empty, it copies the value of the previous day into it, adds it to the product total and the grand total, and records the cell so that colour_yellow_cells can colour it yellow with red font. There is no limit on how many days in a row this may happen, no marker in the remarks sheet, and no check whether all blocks of an asset were entered. Gas is pre-netted (gross minus injection) before the table is built, so a day with only an injection row, or with a zero gross row, yields a real row with zero or negative value and is not treated as missing."));
c.push(P("This design is right for one missing day (previous-day data, as the Excel also does) but wrong for a month of missing BC-60 entries, and it gives no protection against entry mistakes."));

c.push(H1("4. Proposed corrections"));
c.push(H2("4.1 Data to be corrected in SAP now"));
c.push(table(["Asset / date", "Action"], [
  ["Brazil BC-60, 01-Sep to 30-Sep", "Enter the September daily figures (oil and gas). Until then the whole month is a copy of 31-Aug."],
  ["Azerbaijan ACG, 30-Sep oil", "Check that all ACG blocks are entered for 30-Sep; complete the missing block."],
  ["Azerbaijan ACG, 30-Sep gas", "Correct the gas gross and injection quantities and units for 30-Sep (expected net about 7 MMSCMD)."],
  ["Venezuela Carabobo, 29-Sep", "Correct oil (about 7,000 BOPD) and gas (about 0.19 MMSCMD)."],
  ["Russia Vankor, 29-Sep and 30-Sep gas; Sakhalin-1, 30-Sep gas", "Check whether the gross gas rows are missing or zero; enter the actual values."],
  ["South Sudan SPOC, 30-Sep remark", "Replace the pasted numbers by the intended remark text."],
  ["Russia Vankor, 30-Sep remark", "Re-enter the remark within the field length, or shorten it."],
], [3400, 5960]));
c.push(SP());
c.push(H2("4.2 Changes proposed in the report program"));
c.push(table(["#", "Change", "Where in ZPRA_DPR_REPORT", "Effect"], [
  ["P1", "Limit the carry-forward to a configurable number of days (proposal: 3). Beyond that the cell stays empty, is not added to the totals, and is shown in grey with 'no data'.", "fill_null_values_with_previous, populate_no_data_entries", "Stops a month of BC-60 copies from entering MTD and YTD."],
  ["P2", "Print a line in the remarks sheet 'Previous-day data used for: <assets>' for every day where a carry-forward happened, as the Excel does.", "prepare remarks (form around line 5479) using gt_copied_cells", "Readers see at once which figures are provisional."],
  ["P3", "Completeness check per asset and block: if an asset has blocks in the profile table and not all blocks have a row for the day, mark the cell (orange) and list it in the remarks sheet as 'incomplete'.", "fill_dynamic_table_sec1, using ZPRA_C_PRD_PROF", "ACG 203,033 would have been marked instead of printed as final."],
  ["P4", "Plausibility check: if a day's value deviates by more than a configurable percentage (proposal: 50 %) from the previous day, or if net gas is negative, mark the cell (orange) and list it in an 'Entries to verify' block of the remarks sheet. Negative net gas is shown as 0 in totals with the warning.", "after the gas netting (around line 2596) and in fill_dynamic_table_sec1", "Carabobo 7 and ACG 834,117 would have been caught; totals would not go negative."],
  ["P5", "De-duplicate remarks per asset and text for the day.", "remarks form (line 5479)", "ACG remark printed once."],
  ["P6", "Optional: a short validation report (or the same checks in the entry transaction) that the Technical Cell runs before circulation, listing missing assets, missing blocks, deviations above the threshold and negative gas for the date.", "new program ZPRA_DPR_CHECK, same selection logic", "Problems are fixed before the DPR is generated."],
], [500, 3800, 2300, 2760]));
c.push(SP());
c.push(box("Recommendation:", "Correct the data listed in 4.1 immediately and regenerate the DPR for 30-Sep; that alone removes every circled figure except the BC-60 month, which needs the September entries. Implement P1, P2, P4 and P5 as one change (about two to three days of development and test on the classic program); P3 and P6 as a second step after the Technical Cell confirms the block structure per asset.", AMBER));
c.push(SP());
c.push(P("Note on the dashboard: the CDS views built for the BTP dashboard read the same daily table without any carry-forward, so a missing day shows as a gap and a wrong entry shows as entered. The plausibility rules of P4 can be added there as a flag column once agreed.", { italics: true }));

const doc = new Document({
  styles: { default: { document: { run: { font: "Calibri", size: 21 } } } },
  numbering: { config: [
    { reference: "bullets", levels: [{ level: 0, format: LevelFormat.BULLET, text: "•", alignment: AlignmentType.LEFT, style: { paragraph: { indent: { left: 540, hanging: 270 } } } }] },
  ] },
  sections: [{
    properties: { page: { margin: { top: 1200, bottom: 1200, left: 1300, right: 1300 } } },
    footers: { default: new Footer({ children: [new Paragraph({ alignment: AlignmentType.CENTER,
      children: [new TextRun({ text: "ONGC Videsh — SAP DPR issues of 01-Oct-2026 — analysis — page ", size: 16, color: "808080" }),
                 new TextRun({ children: [PageNumber.CURRENT], size: 16, color: "808080" })] })] }) },
    children: c,
  }],
});
Packer.toBuffer(doc).then((b) => { fs.writeFileSync(OUT, b); console.log("written", OUT, b.length); });
