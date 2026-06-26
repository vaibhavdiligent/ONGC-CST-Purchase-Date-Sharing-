# WRICEF list file details
Session ID: local_2c2269da-3ca0-47da-9a73-a9213986c81f
Exported: 2026-06-24
---

Session "WRICEF list file details": idle

[user] this file was created by you C:\Users\VaibhavMaheshwari\Downloads\Additional_WRICEF_List_13_May_2026_KPMG_Scope.xlsx can you giv me details of how we created this file
[assistant] (called ToolSearch)
[assistant] (called mcp__session_info__list_sessions)
[assistant] I don't see a session titled exactly for that file. Let me search the most likely candidates' transcripts.
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] None of those are it. Let me check more sessions.
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] Still searching. Let me check the remaining likely sessions.
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] (called mcp__session_info__read_transcript)
[assistant] I searched all 21 of my retrievable past sessions and none of them created or reference `Additional_WRICEF_List_13_May_2026_KPMG_Scope.xlsx`. I can't reconstruct how that file was built.

Two likely reasons:

The file is dated 13 May 2026, which is older than most of the work still in my history. My session list only goes back 21 sessions (the WRICEF one has almost certainly aged out), so its transcript is no longer accessible to me.

What I *can* still see is a lot of adjacent SAP/Coca-Cola work — ATC object lists, add-on namespace analysis, SPDD/SPAU trackers, the deletion-plan deck — but nothing titled or scoped as "WRICEF / KPMG."

If you want, I can do one of these instead:

- Open the file directly and reverse-engineer it — tell me to access it (it's in your Downloads, which I'd need folder access to), and I'll inspect its tabs, columns, and any embedded notes to tell you exactly what's in it and infer how it was assembled.
- If you remember roughly when we made it or what source files fed it (e.g. a KPMG-provided list plus a TADIR/where-used export), I can match that against the source files still sitting in your `C:\Games` folders.

Which would you prefer?
[user] this was the source file C:\Users\VaibhavMaheshwari\Downloads\Addons_Analysis_v3 1 (1).xlsx
[assistant] I'll need access to your Downloads folder to inspect both the source file and the WRICEF output and reconstruct how it was built.
[assistant] (called ToolSearch)
[assistant] (called mcp__cowork__request_cowork_directory)
[assistant] (called mcp__workspace__bash)
[assistant] Both files are here. Let me inspect their structure to reconstruct how the WRICEF file was derived from the source.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] I found both files and reconstructed it from their contents. One thing up front: the file you pointed me to, `Addons_Analysis_v3 1 (1).xlsx`, is actually about SAP **add-ons** (PBS, Movilizer, TJC, SNP, Ortec, etc.) — it has no data overlap with the WRICEF file, whose 187 objects are custom-development requirements for Astral (Approval matrix, Auto CD reversal, ageing reports, etc.). So that add-on file was *not* the data source for this WRICEF list. The actual input was the list already sitting in `Sheet1`.

Here's how the WRICEF file was built, based on the four tabs:

**Sheet1 — the input list (untouched).** 187 rows, every one flagged `KPMG Scope = Yes` and `To be developed`. The Sr No has gaps (2, 5, 6, 7, 9…), so this was a filtered subset — only the KPMG-scope items pulled from a larger master list. Columns: Sr No, Object, status, Description, Requested Astral Department, Primary Module, Secondary Module, KPMG Scope.

**WRICEF Analysis — the classification I added.** Each object was tagged with a WRICEF Type (W/R/I/C/E/F) inferred from its name and description, since the list carried no SAP object-type metadata. 19 ambiguous ones got a "Review" flag. Result: 94 Enhancements, 56 Reports, 22 Workflows, 6 Conversions, 5 Interfaces, 4 Forms.

**WRICEF Summary — the pivot.** Three breakdowns plus a methodology note: count by Primary Module × Type (TRM 55, MM 34, FI 32, SD 20… = 187), count by Type with %, and count by Requested Astral Department.

**Effort Estimate — the man-day model.** Editable rates per type (Workflow 6, Interface 4, Form 3, Report 2, Enhancement 2, Conversion 1 days/object) multiplied by the counts → **470 man-days total** (Enhancements 188, Workflows 132, Reports 112…), broken out again by module. Note states it's build effort only — excludes spec, testing, cutover, PM.

So the workflow was: take the KPMG-scoped "to be developed" list (Sheet1) → classify every object into a WRICEF category → summarize into module/type pivots → apply per-type effort rates to get a 470-man-day estimate.

If you actually meant a *different* source file — one whose content matches these 187 objects — tell me which, and I'll trace the real lineage. There's also a `Final_Signed_WRICEF_Object_P2_KPMG_Scope.xlsx` in the same folder modified today; want me to compare the two?
[user] i think i gave wrong excel
[assistant] No problem. Which file did you mean? If you're not sure of the exact name, I can list the likely candidates in your Downloads — there are several WRICEF-related files, plus the `Final_Signed_WRICEF_Object_P2_KPMG_Scope.xlsx` that was modified today.

Just tell me the filename (or describe it) and I'll open it and trace exactly how the `Additional_WRICEF_List_13_May_2026_KPMG_Scope.xlsx` was built from it.
[user] based on analysis and different tab we created in file C:\Users\VaibhavMaheshwari\Downloads\Additional_WRICEF_List_13_May_2026_KPMG_Scope.xlsx i need same treatment for file C:\Users\VaibhavMaheshwari\Downloads\Final_Signed_WRICEF_Object_P2_KPMG_Scope.xlsx
[assistant] I'll inspect the Final_Signed file first to see its structure, then apply the same three-tab treatment.
[assistant] (called mcp__workspace__bash)
[assistant] Same structure as Sheet1 (plus User Name and Remarks columns), 172 objects. Let me pull every row in full so I can classify each into a WRICEF type, and study the original file's formatting to match it.
[assistant] (called mcp__workspace__bash)
[assistant] 157 objects, all KPMG scope, all "to be developed" — same shape as the original. Let me study the original file's formatting so the new tabs match exactly.
[assistant] (called mcp__workspace__bash)
[assistant] Let me extract the exact formulas and the full color/text scheme for all six WRICEF types so the new file is a faithful replica.
[assistant] (called mcp__workspace__bash)
[assistant] I have the complete blueprint — formulas, color codes for all six types, and formatting. Now I'll classify all 157 objects and build the three tabs as a faithful replica.
[assistant] (called mcp__workspace__bash)
[assistant] All 157 classified (9 flagged for review). Now building the workbook.
[assistant] (called mcp__workspace__bash)
[assistant] Built with 157 rows. Let me verify the formulas calculate correctly by recalculating the file and checking totals.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] All formulas calculate correctly: 7 W, 56 R, 4 I, 3 C, 83 E, 4 F = 157 objects, total **351 man-days**. Let me set full-recalc-on-open so Excel shows values immediately, and do a final structural check.
[assistant] (called mcp__workspace__bash)
[assistant] The file is built and verified. Let me present it.
[assistant] (called mcp__cowork__present_files)
[assistant] Done — same three-tab treatment applied to `Final_Signed_WRICEF_Object_P2_KPMG_Scope.xlsx`. Your original `Sheet1` is untouched; I added:

**WRICEF Analysis** — all 157 KPMG-scope objects classified into a WRICEF type from their name/description, color-coded by type, with a Classification Basis column and a red **Review** flag on 9 judgmental items (e.g. ZFI_BALLET with no description, FCC which is standard S/4 config, the user-manuals gap). Frozen header + auto-filter.

**WRICEF Summary** — live COUNTIFS pivots: count by Primary Module × Type, count by Type with %, count by Requested Astral Department, plus methodology notes.

**Effort Estimate** — same editable rate model (W6 / I4 / F3 / R2 / E2 / C1 man-days), giving **351 man-days** total build effort.

Classification breakdown: 83 Enhancements, 56 Reports, 7 Workflows, 4 Interfaces, 4 Forms, 3 Conversions. All formulas recalc-verified.

One thing worth your eye: the 9 red-flagged rows are where I had to judge from a name or vague description — quickest to scan by filtering column J on the Analysis tab. Want me to walk through those, or adjust any of the effort rates?
[user] why is wricey summary and estimate dont have value
[assistant] The tabs use formulas, and whatever you opened the file with didn't auto-recalculate them, so the cells looked blank. Let me fix it so the values are embedded and show immediately.
[assistant] (called mcp__workspace__bash)
[assistant] The recalc copy kept all formatting and has embedded values. Let me confirm its values land in the right cells, then put it in place.
[assistant] (called mcp__workspace__bash)
[assistant] All values populate correctly (Grand Total 157, fully cross-footed). Putting the value-embedded version in place.
[assistant] (called mcp__workspace__bash)
[assistant] Fixed. The Summary and Effort tabs were formula-driven and your viewer wasn't recalculating them, so they showed blank. The file now has the computed values embedded — they'll display in any viewer — while the formulas stay live, so editing the man-day rates on the Effort tab still recalculates.

Reload the file and both tabs will show numbers: Grand Total 157 objects, **351 man-days** total effort.
[user] what is BF
[assistant] (called mcp__workspace__bash)
[assistant] In your WRICEF files, **BF** is a value in the *"Requested Astral Department"* column — i.e. it's one of Astral's own internal department/function codes for who raised the requirement, not a standard SAP term.

Looking at every object tagged BF, they're all commercial-finance / receivables items — approval matrices, credit-note (CD) reversals, credit memos, penal interest, cash-discount registers, channel-finance reminders, ageing reports, dunning, scheme provisions. On that basis BF almost certainly stands for **Business Finance** (Astral's commercial/billing-finance function). It shows up on 11 rows in the original list and 10 in the P2 (Final Signed) list, and the primary SAP modules behind them are SD and FI — consistent with order-to-cash / receivables work.

That said, it's a customer-internal abbreviation, so the safest way to be certain is to confirm with Astral or KPMG what BF maps to. Want me to pull the full BF list from the P2 file so you can verify the expansion?
[user] replace bf WITH FINANCE
[assistant] I'll replace every "BF" with "FINANCE" across the P2 file (it appears as a department code in Sheet1, the Analysis tab, and the Summary's department section), then re-embed the calculated values.
[assistant] (called mcp__workspace__bash)
[assistant] Done — all 21 "BF" cells are now "FINANCE" (10 in Sheet1's department column, 10 in the Analysis tab, and the Summary department label). No "BF" remains, the department count rolls up correctly (FINANCE = 10), and the effort total is unchanged at 351 man-days.
[assistant] (called mcp__cowork__present_files)
[assistant] Reload the file to see the change. Want me to apply the same BF→FINANCE replacement to the original `Additional_WRICEF_List_13_May_2026_KPMG_Scope.xlsx` too (it has 11 BF rows)?
[user] undo the BF changes
[assistant] (called mcp__workspace__bash)
[assistant] Reverted — all 21 cells are back to "BF", no "FINANCE" remains, and totals are unchanged (351 man-days). Reload the file to see it restored.
[user] add TRM assumption of 50 objects 15 workflow and split others in different ricef component and then add man days accoridngly
[assistant] I'll add a TRM assumption block to the Effort Estimate tab. First let me see the current layout so I append cleanly.
[assistant] (called mcp__workspace__bash)
[assistant] Now I'll add the TRM assumption block (50 objects: 15 Workflow + a split across the other components) with man-days, plus a combined total. The split cells will be editable like the rate cells.
[assistant] (called mcp__workspace__bash)
[assistant] The combined man-days referenced the wrong column. Let me fix that.
[assistant] (called mcp__workspace__bash)
[assistant] Fixed and verified. Done — the Effort Estimate tab now has a TRM assumption block and a combined total.

TRM scope (50 objects), with man-days at the existing rates:

| Type | Objects | Days/obj | Man-days |
|---|---:|---:|---:|
| Workflow | 15 | 6 | 90 |
| Report | 10 | 2 | 20 |
| Interface | 6 | 4 | 24 |
| Conversion | 2 | 1 | 2 |
| Enhancement | 14 | 2 | 28 |
| Form | 3 | 3 | 9 |
| **TRM Total** | **50** | | **173** |

Combined with the 157 listed objects: **207 objects → 524 man-days** (351 listed + 173 TRM).

The 15 Workflow is fixed per your instruction; I split the other 35 across the remaining components (leaning to Report/Enhancement, with some Interface for bank/treasury). Those count cells are yellow/editable — change any and the man-days and grand total recalculate automatically.
[assistant] (called mcp__cowork__present_files)
[assistant] Want me to adjust the 35-object split across the other components, or is this distribution good?
[user] i cant find TRM in wricef summary and effort estimate like other moduels
[assistant] Right — those pivots only count what's in the Analysis tab, and TRM wasn't there (it was a separate block). I'll add the 50 TRM objects as rows in the Analysis tab (clearly tagged as assumptions) so TRM flows through every pivot exactly like the other modules, and replace the separate block with a single combined total.
[assistant] (called mcp__workspace__bash)
[assistant] TRM is now a module row in both pivots. Let me recalc to embed values and verify TRM shows up correctly with the right totals.
[assistant] (called mcp__workspace__bash)
[assistant] Done. TRM now appears as a proper module row in both the WRICEF Summary and the Effort Estimate, sitting alongside FI, MM, SD, etc.

In the Summary "by Primary Module" table, TRM shows 15 W / 10 R / 6 I / 2 C / 14 E / 3 F = 50, and the grand total is now **207 objects**. In the Effort "by Primary Module" table, TRM contributes **173 man-days** and the grand total is **524 man-days**.

The 50 TRM objects live as rows on the Analysis tab (Sr No "TRM-01"…"TRM-50", highlighted, flagged "Assumption" in the Review column) so every pivot picks them up automatically — and the TRM module cell is shaded in both summary tables so it's clear it's the assumed scope. To resize TRM later, just edit/add/remove those Analysis rows; everything recalculates.
[assistant] (called mcp__cowork__present_files)
[assistant] Reload to see TRM in place. Anything else you'd like adjusted — the TRM split, the rates, or the same TRM treatment on the other WRICEF file?
[user] based on earlier proposal you had created the ppt C:\Users\VaibhavMaheshwari\Downloads\ABAP_Proposal_All_Options (5).pptx now considering the new file create me a new proposal considering delivery time of 5 months
[assistant] I'll base the new proposal on your earlier deck. Let me first read that proposal to capture its structure, branding, and commercial model, then adapt it to the new scope (207 WRICEF objects / 524 man-days) and a 5-month delivery.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] The earlier deck is a Diligent Global ABAP proposal with three scope options (200/300/400 objects), 6 consultants at ₹1,50,000/consultant/month, 22 days/month. Before I build the new 5-month version around the new file (207 objects / 524 man-days), a few decisions that drive the commercials:
[assistant] (called AskUserQuestion)
[assistant] Decisions locked: 207 objects / 524 man-days, 6 consultants, ₹1,50,000/month, 5-month delivery → ₹45,00,000 + taxes. I'll reuse your earlier deck's branding and replace the three-option middle section with a single tailored engagement. Let me read the pptx skill for the right editing approach.
[assistant] (called Read)
[assistant] (called Read)
[assistant] Plan: reuse the deck's intro and closing slides, repurpose the existing 5-batch "400-object" option (slides 12–14, which already have the 5-month / ₹45,00,000 / 6-consultant structure) into the new 207-object engagement, and drop the other options. Let me set up tracking and start editing.
[assistant] (called ToolSearch)
[assistant] (called TaskCreate)
[assistant] (called TaskCreate)
[assistant] (called TaskCreate)
[assistant] (called TaskUpdate)
[assistant] (called mcp__workspace__bash)
[assistant] Slide map confirmed. Keeping intro (1–5), the 400-option (12–14) to repurpose, and closing (15–18); removing slides 6–11.
[assistant] (called mcp__workspace__bash)
[assistant] The outputs mount blocks file deletion. Let me redo this in the native sandbox filesystem.
[assistant] (called mcp__workspace__bash)
[assistant] Clean. Now slides 1–5, 12–14, 15–18 remain. Let me read the three option slides to edit their text precisely.
[assistant] (called mcp__workspace__bash)
[assistant] Now I'll apply the text edits with a position-aware replacer (handles the repeated cell values and date collisions safely).
[assistant] (called mcp__workspace__bash)
[assistant] The remaining "30 Sep 2026" is legitimately Batch 3's delivery date now. Let me verify the full scope table reads correctly.
[assistant] (called mcp__workspace__bash)
[assistant] All text is correct (batches sum to 207, Jul–Nov, ₹45,00,000). Small polish: update the table column header from "RICEF" to "WRICEF", then pack and render for QA.
[assistant] (called TaskUpdate)
[assistant] (called TaskUpdate)
[assistant] (called TaskUpdate)
[assistant] (called mcp__workspace__bash)
[assistant] Packed successfully. Now rendering to images for visual QA.
[assistant] (called mcp__workspace__bash)
[assistant] 12 slides, no real leftovers (the "30 Sep" is Batch 3's correct date). Let me visually inspect the three edited slides — especially the long subtitle on the scope slide.
[assistant] (called Read)
[assistant] (called mcp__workspace__bash)
[assistant] (called Read)
[assistant] Scope slide looks clean — subtitle wraps to two lines neatly inside the box, table sums to 207. Let me check the timeline and commercials slides.
[assistant] (called Read)
[assistant] Timeline is clean — Jul–Nov 2026, 6 consultants, 660 person-days, ~524 build man-days noted. Now the commercials slide.
[assistant] (called Read)
[assistant] The commercials title is too long and clips at the top edge (it wrapped to two lines). I'll shorten it and re-render.
[assistant] (called mcp__workspace__bash)
[assistant] (called Read)
[assistant] All three slides are clean now. Copying the final deck to your Downloads.
[assistant] (called mcp__workspace__bash)
[assistant] (called TaskUpdate)
[assistant] (called mcp__cowork__present_files)
[assistant] Done. New 12-slide proposal built from your earlier deck, tailored to the new file and a 5-month delivery.

Kept all the Diligent branding (cover, About Us, Journey, Value Proposition, Why Diligent, Assumptions, Customers, Partners, Thank You) and replaced the three-option middle section with a single engagement:

- **Scope** — 207 WRICEF Objects (mix E97 · R66 · W22 · I10 · F7 · C5, ~524 build man-days) across 5 monthly batches (42/42/41/41/41), delivering 31 Jul → 30 Nov 2026.
- **Deployment Timeline** — 6 ABAP consultants × 22 days × 5 months = 660 person-days (comfortably covers the 524 build man-days, leaving buffer for spec/test).
- **Commercials** — ₹1,50,000/consultant/month, ₹7,50,000 each over 5 months, **Grand Total ₹45,00,000 + taxes**, offshore, billed monthly.

All three slides passed visual QA (fixed a title that was clipping). Want me to add a dedicated WRICEF-classification breakdown slide (type + module split from the Excel), or adjust the batch sizing or start date?
[user] remove TRM objects from RICEF count and need to decrease month to 4
[assistant] Got it — drop the 50 TRM objects (back to 157 / 351 man-days) and compress to 4 months. That means the scope table loses Batch 5, the timeline loses the Nov column, and commercials drop to ₹36,00,000. Let me inspect the table structure, then edit with formatting preserved.
[assistant] (called mcp__workspace__bash)
[assistant] Structure confirmed. Now making all edits with formatting preserved — removing Batch 5 row, removing the Nov column (redistributing its width), and updating all text.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] Content is correct (157 / 351 / 4 months / ₹36,00,000, no leftover Batch 5 or Nov). Let me visually confirm the scope and timeline tables after the row/column removal.
[assistant] (called Read)
[assistant] Scope is clean — 4 batches summing to 157. Now the timeline column removal.
[assistant] (called Read)
[assistant] Timeline is clean — Nov column gone, table fills the width, 528 person-days. Let me confirm the commercials slide render, then finalize.
[assistant] (called Read)
[assistant] All three slides verified. Saving the updated deck to Downloads.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__cowork__present_files)
[assistant] Done. Updated proposal with TRM removed and compressed to 4 months:

- **Scope** — 157 WRICEF Objects (E83 · R56 · W7 · I4 · F4 · C3, ~351 build man-days) across 4 batches (40/39/39/39), delivering 31 Jul → 31 Oct 2026.
- **Timeline** — 6 consultants × 22 days × 4 months = 528 person-days (Jul–Oct; comfortably covers the 351 build man-days).
- **Commercials** — ₹6,00,000 per consultant, **Grand Total ₹36,00,000 + taxes**.

Saved as a new file (`ABAP_Proposal_157_WRICEF_4Month.pptx`) so the 207/5-month version is preserved.

One observation worth flagging: with only 157 objects / 351 build man-days, 6 consultants over 4 months (528 person-days) is fairly over-resourced (~66% utilization). If you want to cut cost, 4 consultants would still deliver in 4 months at ~₹24,00,000, or 5 consultants gives more buffer at ₹30,00,000. Want me to produce either of those as an alternative?
[user] cut the cost and update with 5 consultants
[assistant] 5 consultants it is: 5 × 4 months × ₹1,50,000 = ₹30,00,000 + taxes (440 person-days, still covers the 351). Removing Consultant 6 from the timeline and commercials tables and updating totals.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] Content checks out — 5 consultants, 110 days/month, 440 person-days. Let me visually verify both tables.
[assistant] (called Read)
[assistant] Timeline looks good (5 consultants, 110 days/month, 440 person-days). Now the commercials slide.
[assistant] (called Read)
[assistant] Both slides verified. Saving the final deck.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__cowork__present_files)
[assistant] Done. Updated to 5 consultants:

- **Scope** — 157 WRICEF objects, 4 batches (40/39/39/39), 5 ABAP consultants, delivering by 31 Oct 2026.
- **Timeline** — 5 consultants × 22 days × 4 months = 440 person-days (covers the 351 build man-days with buffer).
- **Commercials** — ₹6,00,000 per consultant, **Grand Total ₹30,00,000 + taxes** — ₹6,00,000 lower than the 6-consultant version.

Saved as a new file so the 6-consultant version is preserved.
[user] need proposal for 170 objects
[assistant] For 170 objects I'll keep the same engagement shape (5 consultants, 4 months → cost stays ₹30,00,000, since 170 objects ≈ ~380 build man-days still fits the 440 person-day capacity), and rebalance the batches to 43/43/42/42.
[assistant] (called mcp__workspace__bash)
[assistant] Content is correct. Let me visually verify the scope slide, then finalize.
[assistant] (called Read)
[assistant] Scope slide verified (43/43/42/42 = 170). Saving the final deck.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__cowork__present_files)
[assistant] Done — proposal for 170 objects:

- **Scope** — 170 WRICEF objects, 4 batches (43/43/42/42), 5 consultants, delivery by 31 Oct 2026.
- **Timeline** — 5 × 22 × 4 = 440 person-days (covers the scope with buffer).
- **Commercials** — **₹30,00,000 + taxes** (unchanged — same team and duration).

Two notes on the assumptions: the ~380 build man-days is scaled proportionally from the 157-object classification (the extra 13 objects beyond the signed list aren't individually WRICEF-classified yet), and the cost stays flat at ₹30,00,000 because 170 objects still fit comfortably within the 5-consultant / 4-month capacity. If those 13 extra objects turn out to be workflow/interface-heavy, the effort could run higher — want me to firm up the estimate once they're classified, or keep the current scaled figure?
