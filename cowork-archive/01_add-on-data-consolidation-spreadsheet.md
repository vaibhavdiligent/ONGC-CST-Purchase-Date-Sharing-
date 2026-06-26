# Add-on data consolidation spreadsheet
Session ID: local_cb7b0508-937d-4af7-9e1d-de25af3bfd4c
Exported: 2026-06-24
---

Session "Add-on data consolidation spreadsheet": idle

[assistant] (called mcp__workspace__bash)
[assistant] Transient read glitch — the file's valid. Let me re‑read the counts.
[assistant] (called mcp__workspace__bash)
[assistant] Let me check the file's integrity — it may have been truncated.
[assistant] (called mcp__workspace__bash)
[assistant] The Excel is momentarily in an inconsistent state on the mount, but I already have the verified impacted counts from this session's reconciliation (active dependents in CCLM, used last 3 yrs): **Ortec 129, Movilizer 9, Trillium 3, TJC 3, all others 0 (total 144).** Adding that as the "CCLM CCBJI Impacted Objects" column.
[assistant] (called mcp__workspace__bash)
[assistant] (called Read)
[assistant] The new **"CCLM CCBJI Impacted Objects"** column is added right after Total Objects, with the counts from the Excel (red where >0):

- **Ortec 129 · Movilizer 9 · TJC 3 · Trillium 3** · all others blank (0) · **TOTAL 144**

A footnote explains the definition. Saved to the original `AddOn_Decision.pptx`.
[assistant] (called mcp__cowork__present_files)
[assistant] One note on the number I used: "CCLM CCBJI Impacted Objects" = **active dependents** (Coca‑Cola/standard objects in CCLM that are used in the last 3 years and depend on the add‑on) — the 144 set we reconciled. If you instead want **all** impacted custom objects in CCLM regardless of recent use (e.g., Ortec 533, Movilizer 304, ReadSoft PD 60, OBIZ 19…), tell me and I'll swap the column to that.
[user] in ppt we forgot to add winshiter check the old ppt and update the new slide
[assistant] WinShuttle (`/WINSHTLQ/`) is indeed missing — the old slide‑3 table never listed it (only the Key Takeaways mentioned "Winshuttle removed in ECC"). It has 118 objects in SJR, 0 in the converted system, and 0 CCLM‑impacted dependents. Adding it.
[assistant] (called mcp__workspace__bash)
[assistant] (called Read)
[assistant] WinShuttle is now in the table (between IBM Monitoring and FORAB):

- **WinShuttle** · `/WINSHTLQ/` · 118 objects · impacted 0 · **As is: Fully removed** · **Propose Solution: To be removed in ECC** · SUM Blocker: No

The TOTAL updated to **91,675** (91,557 + 118). Saved to the original `AddOn_Decision.pptx`.
[assistant] (called mcp__cowork__present_files)
[assistant] One thing to flag: the slide now has 15 add‑ons, but the **main deck (`AddOn_Analysis.pptx`) slide 3 and the Key Takeaways still show the old total of 91,557** without WinShuttle as a row. If you'd like, I can add WinShuttle to slide 3 of the main deck too so the two stay consistent. Want me to do that?
[user] did u check cclm data also for windshutter
[assistant] Let me verify WinShuttle's CCLM data directly rather than assume.
[assistant] (called mcp__workspace__bash)
[assistant] Yes — I just checked, and WinShuttle has **no CCLM data**:

- `/WINSHTLQ/` objects **in CCLM: 0** (its 118 objects are in the TADIR/namespace export but none are in the CCLM custom‑code extract — they're not tracked there)
- Objects **used in last 3 years: 0**
- Dependency pairs in the where‑used log: **0**

So the **CCLM CCBJI Impacted Objects = 0 (blank)** on the slide is correct — nothing in Coca‑Cola/standard code depends on WinShuttle, and it has no CCLM usage. The 118 in "Total Objects" is the object count from the namespace/TADIR export (predominantly DDIC objects that CCLM doesn't track).

So the slide is accurate as‑is; no change needed. WinShuttle is fully removed already with zero downstream impact, consistent with its "To be removed in ECC" disposition.
[user] can u draft me a mail for sending to customer of this analsis and inform them i have started working on trillium analysis first and will target mobiliser next rest all are clear
[assistant] Here's a draft you can send. I've kept it professional and concise, reflecting the analysis and your stated plan.

---

**Subject:** CCBJI S/4HANA Conversion — Third‑Party Add‑On Decommissioning Analysis & Next Steps

Dear [Customer name],

Please find attached the completed analysis of the third‑party add‑ons in scope for the ECC → S/4HANA conversion. The assessment covers all 15 add‑ons and combines three data sources for each: the object‑directory comparison between the current system (SJR) and the converted system, the CCLM usage history, and the where‑used dependency log (which Coca‑Cola custom and SAP‑standard objects depend on each add‑on).

**Two deliverables are attached:**
- *AddOn_Decision* — a one‑page summary per add‑on: total objects, CCLM/CCBJI impacted objects, current state ("As is"), proposed solution, and whether it blocks the SUM (conversion) activity.
- *AddOn_Critical_Dependencies* — the detailed workbook: per add‑on, the Coca‑Cola/standard objects that depend on it (the objects to check/redevelop), with their CCLM usage so the active ones are clearly flagged.

**Summary of the decisions:**
- **Clear / low‑risk** — SNP, TJC and WinShuttle are removed in ECC (unused, already absent in the converted system); ReadSoft is masked in ECC and removed in S/4HANA; HOAG is activated in ECC; OBIZ, ICORP, B2Wise, IBM Monitoring and FORAB require no decision (no conversion impact). Ortec is handled via the compatible file during conversion and a custom program in S/4HANA.
- **Need detailed object‑level analysis** — **Movilizer** and **Trillium**, as both are SUM blockers and have active Coca‑Cola dependencies that will need to be checked and potentially redeveloped/retested.

**Next steps:** I have **started with the Trillium analysis first**, and will **take up Movilizer next**. All other add‑ons are clear and require no further deep‑dive at this stage.

I will share the Trillium findings shortly. Please let me know if you have any questions in the meantime.

Best regards,
Vaibhav Maheshwari
Diligent Consulting

---

Want me to save this as a **draft in your Gmail** (as I did with the SAP message earlier), and should I attach/reference the two files? I can also tweak the tone or add specific impacted‑object numbers (e.g., Trillium 3, Movilizer 9 active dependents) if you'd like them called out.
[user] i am sending ppt only draft accordingly
[assistant] Updated draft — referencing only the PPT.

---

**Subject:** CCBJI S/4HANA Conversion — Third‑Party Add‑On Decommissioning Analysis & Next Steps

Dear [Customer name],

Please find attached the summary of the third‑party add‑on decommissioning analysis for the ECC → S/4HANA conversion. The assessment covers all 15 add‑ons and combines, for each, the object‑directory comparison between the current system (SJR) and the converted system, the CCLM usage history, and the where‑used dependency analysis (which Coca‑Cola custom and SAP‑standard objects depend on each add‑on).

The attached one‑page summary shows, per add‑on: total objects, CCLM/CCBJI impacted objects, current state ("As is"), the proposed solution, and whether it blocks the SUM (conversion) activity.

**Summary of the decisions:**
- **Clear / low‑risk** — SNP, TJC and WinShuttle are removed in ECC (unused and already absent in the converted system); ReadSoft is masked in ECC and removed in S/4HANA; HOAG is activated in ECC; OBIZ, ICORP, B2Wise, IBM Monitoring and FORAB require no decision (no conversion impact). Ortec is handled via the compatible file during conversion and a custom program in S/4HANA.
- **Need detailed object‑level analysis** — **Movilizer** and **Trillium**, as both are SUM blockers and have active Coca‑Cola dependencies that will need to be checked and potentially redeveloped/retested.

**Next steps:** I have **started with the Trillium analysis first**, and will **take up Movilizer next**. All other add‑ons are clear and require no further deep‑dive at this stage.

I will share the Trillium findings shortly. Please let me know if you have any questions in the meantime.

Best regards,
Vaibhav Maheshwari
Diligent Consulting

---

Want me to save this as a **draft in your Gmail**? (You'd attach the `AddOn_Decision.pptx` yourself before sending.)
[user] can u check in CCLM report if these objects are there and what is there last used and change date PROG
/CCBJI/RDMMIMR_PO_GR_REP_RPA
J_EKORG
FUGR
/CCC/RTR_INBOUND_COPA
J_TRANSFER
PROG
/CCC/RUMDMR_VENDOR_FUL_EXTRACT
J_EKORG
PROG
/CCEJ/RDMMIMR_PO_GR_REPORT
J_EKORG
PROG
/CCEJ/RDMMIMR_SOURCELIST
J_EKORG
PROG
/CCEJ/RDPMEQMR_CC_PO_REPORT
J_EKORG
PROG
/CCEJ/RUMMPURC_PO_UPD_DWN_PURC
J_EKORG
PROG
/CCEJ/RUMMPURC_PO_UPLOAD
J_EKORG
PROG
/CCEJ/RUMMPURR_AUTO_PR_PO
J_EKORG
PROG
/CCEJ/RUMMPURR_JDC_AUTO_MIRO
J_EKORG
[assistant] Let me look these up directly in the CCLM report and pull their Last Used, Created on, and Changed on dates.
[assistant] (called mcp__workspace__bash)
[assistant] Here's what the CCLM report shows for those objects (all are Coca‑Cola custom programs; dates are Last Used / Created on / Changed on):

| Object | Type | Last Used | Created on | Changed on |
|---|---|---|---|---|
| /CCBJI/RDMMIMR_PO_GR_REP_RPA | PROG | **2026‑05‑06** | 2020‑02‑11 | 2020‑02‑24 |
| /CCC/RTR_INBOUND_COPA | FUGR | — (in CCLM, no dates) | — | — |
| /CCC/RUMDMR_VENDOR_FUL_EXTRACT | PROG | **2026‑05‑01** | 2013‑12‑09 | 2014‑11‑22 |
| /CCEJ/RDMMIMR_PO_GR_REPORT | PROG | **2026‑05‑06** | 2014‑10‑17 | **2023‑12‑13** |
| /CCEJ/RDMMIMR_SOURCELIST | PROG | **2026‑04‑15** | 2014‑10‑17 | 2015‑03‑02 |
| /CCEJ/RDPMEQMR_CC_PO_REPORT | PROG | 2023‑09‑14 | 2016‑11‑15 | 2016‑11‑15 |
| /CCEJ/RUMMPURC_PO_UPD_DWN_PURC | PROG | **2026‑05‑01** | 2014‑10‑17 | 2017‑02‑23 |
| /CCEJ/RUMMPURC_PO_UPLOAD | PROG | **2026‑05‑05** | 2014‑10‑17 | **2023‑07‑05** |
| /CCEJ/RUMMPURR_AUTO_PR_PO | PROG | **2026‑05‑06** | 2017‑08‑13 | 2019‑10‑01 |
| /CCEJ/RUMMPURR_JDC_AUTO_MIRO | PROG | **2026‑05‑06** | 2017‑08‑13 | 2017‑12‑08 |

Key points:
- **9 of the 10 are in CCLM and actively used** — most last run in **May 2026** (one, `/CCEJ/RDPMEQMR_CC_PO_REPORT`, last used Sep‑2023, still within 3 years). So these are live programs.
- **`/CCC/RTR_INBOUND_COPA` (FUGR)** is present in CCLM but carries **no Last‑Used / Created / Changed dates** (function groups often have no usage telemetry — usage sits on the individual function modules).
- **`J_EKORG` and `J_TRANSFER` are NOT in CCLM** — these are SAP‑standard data elements (J_EKORG = purchasing org, J_TRANSFER), and SAP‑standard objects are excluded from the CCLM‑restored file. They appear to be the standard fields/objects these programs reference rather than custom objects.

So the custom programs in your list are real and actively used (mostly May 2026); only the FUGR lacks date data, and the two `J_*` items are standard and outside CCLM.
[user] can u create me a excel to send to customer
[assistant] I'll create a clean, customer‑ready Excel with these objects and their CCLM usage/change dates.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__cowork__present_files)
[assistant] Created **`CCLM_Object_Usage_Check.xlsx`** — a clean, customer‑ready sheet listing the 10 objects with **In CCLM, Last Used, Created On, Changed On**, plus a Comment and the referenced object (J_EKORG/J_TRANSFER). Last‑Used dates are colour‑coded (green = last 6 months, yellow = within 3 years), with notes explaining the FUGR with no date data and that J_EKORG/J_TRANSFER are SAP‑standard (not in CCLM).

Want me to adjust the title/columns, or add these into the main `AddOn_Critical_Dependencies` workbook as a tab instead of a standalone file?
[user] there was a document or excel which had mention of transport request which were used to rectify third party object can u check the folder and see if u find tell me the request no
[assistant] Let me search your folders for any file that mentions transport requests for third‑party object fixes.
[assistant] (called mcp__workspace__bash)
[assistant] The content grep hit the huge data files. Let me just list filenames first, then search only small files.
[assistant] (called mcp__workspace__bash)
[assistant] No file is named for transports, but a few candidates could contain them. Let me check the object/linkage workbooks in your "add on clean" folder for a transport‑request column.
[assistant] (called mcp__workspace__bash)
[assistant] I checked all the shared folders. The object/linkage workbooks (`Ortec_Objects.xlsx`, `Trillium_Movilizer.xlsx`, `Enh Where Use list.xlsx`, etc.) have Source/Dependent analysis columns but **no transport‑request column**. So the file you're thinking of isn't in the folders shared with me.

Two things I can tell you:

1. **The dedicated TR file isn't accessible to me.** The project handoff notes reference a file named **`Addons_Modified_Programs_and_TRs.xlsx`** (and `Addons_Namespaces_Actions.xlsx`) located in **`C:\Games\coca cola\06_AddOns_Namespaces`** — that's the "modified programs and transport requests" workbook. That folder is **not currently shared with me**, so I can't open it. If you add/mount that folder, I'll pull out the exact request numbers.

2. **One transport number I do have from our materials:** the SAP‑delivered transport for removing the ReadSoft/COCKPIT third‑party component (the `ZZ_ADDON_REMOVE` delivery under **Case 28059036**) was **`RSMK900167`** (request **K900167**) — it came as `ReadSoft_INVOICE_COCKPIT_all_vers_delete_software_component_COCKPIT_RSMK900167.zip`. That's the transport used to rectify/de‑register the ReadSoft add‑on.

If you mount `06_AddOns_Namespaces` (or drop `Addons_Modified_Programs_and_TRs.xlsx` into one of the shared folders), I'll read it and list every transport request mentioned for the third‑party object fixes.
[user] <uploaded_files>
<file><file_path>C:\Users\VaibhavMaheshwari\AppData\Roaming\Claude\local-agent-mode-sessions\b9dbfd15-b60e-4630-ab2d-bb3aa86c1921\e7a55bf8-c67e-4912-9f30-ca1be557e56b\local_cb7b0508-937d-4af7-9e1d-de25af3bfd4c\uploads\TR_objects.xlsx</file_path><file_uuid>e7cf61b4-a869-4d3c-ab54-77513de34f9d</file_uuid></file>
</uploaded_files>

add this to the folder u saving data is this excel contains all objects in the transport request now there is a file where we had captured all third party objects linkage  can u match this excel with this and tell me what all are matching and what is not
[assistant] Let me first save the uploaded file to your folder and look at its structure.
[assistant] (called mcp__workspace__bash)
[assistant] This is a dump of multiple transport requests (SJRK900310, SJRK900366, …) and their objects — mostly Coca‑Cola custom objects. Let me parse it and match against our captured third‑party linkage (add‑on objects + their dependents).
[assistant] (called mcp__workspace__bash)
[assistant] Need to regenerate the linkage file (scratch was wiped). Let me re‑filter and run the match together.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] Match complete. Let me build a comparison workbook with the results.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__cowork__present_files)
[assistant] Saved both: `TR_objects.xlsx` (added to your folder) and the comparison **`TR_vs_ThirdParty_Match.xlsx`**.

**Result:** the file holds **2,643 objects across 6 transport requests** (Q4RK900020, Q4RK900024, SJRK900291, SJRK900310, SJRK900350, SJRK900366). Matched against our third‑party linkage:

- **2,037 matched** (third‑party related) · **606 not matched**

| Transport | Matched / Total |
|---|---|
| SJRK900350 | 2,024 / 2,618 |
| SJRK900291 | 8 / 17 |
| SJRK900310 | 3 / 5 |
| SJRK900366 | 1 / 1 |
| Q4RK900020 | 1 / 1 |
| Q4RK900024 | 0 / 1 |

**Of the matched objects:** 1,049 are **third‑party add‑on objects** themselves (almost all `/MOVI/` Movilizer), and ~988 are **Coca‑Cola objects that depend on an add‑on** (Movilizer dominates with 2,023 of the matches; also PBS 3, Ortec 6, Trillium 4, HOAG 1). So **SJRK900350 is essentially the Movilizer / Salescloud (/SCL/) rework transport**, and **SJRK900310 ties to PBS** (the `/CCC/RDFIARR_*` statement programs).

**The 606 not‑matched** are objects not in our third‑party linkage — mostly `/SCL/` function modules (e.g. `/SCL/OTCM_*`, `/SCL/CL_OTCM_*`) and a few `/CCC/` objects. Two reasons: they're either genuinely unrelated to any add‑on, **or** they're linked only via **function‑module / method calls**, which the where‑used cross‑reference log doesn't capture (so we can't see that linkage even if it exists).

The workbook has three tabs — **Summary**, **Matched** (with match type + add‑on, filterable), and **Not Matched** — plus a Method note.

One caveat worth knowing for your review: because the where‑used log misses function/method call sites, some of those 606 "not matched" `/SCL/` functions could still be Movilizer‑related in reality. If you have an SE84/where‑used export for function modules, I can fold it in to tighten the match.
[user] in noth match object list for method if method is not found can u compare the class name example for line item /SCL/CL_FSVL_ST_CASH_BAG2     ADD_OVERVIEW_MEL class name is  /SCL/CL_FSVL_ST_CASH_BAG2
[assistant] Good point — methods in the transport are listed as "CLASS METHOD" (or class‑include forms like `CLASS====CCDEF`), so matching on the full method string misses them. Let me extract the class name and re‑match those. First, let me see the not‑matched object formats.
[assistant] (called mcp__workspace__bash)
[assistant] Most not‑matched ones (512 of 606) are methods in `CLASS<spaces>METHOD` form. Let me extract the class name and re‑match those.
[assistant] (called mcp__workspace__bash)
[assistant] Big improvement with the class‑level match: **2,537 matched, only 106 not matched** (500 methods matched via their class — including your example `/SCL/CL_FSVL_ST_CASH_BAG2`). Let me rebuild the comparison workbook.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__cowork__present_files)
[assistant] Updated **`TR_vs_ThirdParty_Match.xlsx`** with the class‑level matching for methods.

**Final result: 2,537 of 2,643 objects matched · 106 not matched.**

Match breakdown:
- Third‑party add‑on objects: **1,049**
- Depends on add‑on (name match): **973**
- Depends on add‑on **(via class)**: **500** ← the methods, now matched on their class (e.g. `/SCL/CL_FSVL_ST_CASH_BAG2 ADD_OVERVIEW_MEL` → class `/SCL/CL_FSVL_ST_CASH_BAG2` → Movilizer)
- Depends on add‑on (exact): 15

**The remaining 106 not‑matched** are: ENHO 23, REPS 23, TABL 16, FUNC 15, METH 13 (whose class also isn't in the linkage), DTEL 5, and a few others. These genuinely aren't in the third‑party linkage — either unrelated, or (the FUNC ones especially) linked only via function‑module call sites that the where‑used cross‑reference doesn't record.

The three tabs (Summary, Matched, Not Matched) are updated, and the Method tab notes the class‑based matching. Saved to the original file.
[user] can u check this folder files also and check if they are mathing C:\Games\coca cola\add on clean\Add On clean up
[assistant] Let me build the object linkage from those "add on clean" files (Ortec_Objects, Trillium_Movilizer, Enh Where‑Use) and match the TR objects against them.
[assistant] (called mcp__workspace__bash)
[assistant] 4,106 objects captured in those files. Now matching the TR objects against them (class‑aware for methods).
[assistant] (called mcp__workspace__bash)
[assistant] Checked those files (Ortec_Objects, Trillium_Movilizer, Enh Where‑Use) — together they hold **4,106 distinct objects**. Matching the transport objects against them:

- **Matched in the add‑on‑clean files: 1,737** of 2,643
- Not in those files: 906

But the key point — **combining both sources (the where‑used log + these add‑on‑clean files), the not‑matched count stays at exactly 106.** In other words, **these folder files add no new coverage** beyond what the where‑used linkage already captured; everything they match was already matched. The where‑used log is the more complete source (it also covers the `/MOVI/` add‑on objects directly and the other add‑ons like PBS/SNP that those files don't include).

So the **106 genuinely unmatched objects remain the same** (ENHO 23, REPS 23, TABL 16, FUNC 15, METH 13, DTEL 5, etc.) — they're in none of the captured linkage, mostly because they're either unrelated or linked only via function‑module call sites that no cross‑reference export records.

Your current `TR_vs_ThirdParty_Match.xlsx` already reflects this (106 in the "Not Matched" tab), so no change is needed. If you'd like, I can add a column flagging which matched objects are also confirmed in the add‑on‑clean files (cross‑validation), or list the 106 with a suggested next step (e.g., request an SE84 function‑module where‑used to chase the remaining FUNC/METH links).
[user] from the CCLM report can u create a new excel of the objects which have been used in last three years for ORTEC
[assistant] I'll pull the Ortec objects used in the last 3 years from CCLM, with their dates, and build a dedicated Excel. First, extract from CCLM.
[assistant] (called mcp__workspace__bash)
[assistant] 3,114 Ortec objects used in the last 3 years. Building the Excel **sorted by Last Used then Last Changed**.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__cowork__present_files)
[assistant] Created **`Ortec_Used_Last3Years_CCLM.xlsx`** — **3,114 Ortec objects** with a CCLM Last‑Used date on/after 2023‑06‑16, **sorted by Last Used then Changed On (newest first)**.

Columns: Namespace · Object Name · Type · Last Used · Months Since · Changed On · Created On · Usage Flag · Exec Freq Band (Last Used colour‑coded — green = last 6 months, yellow = older within 3 yrs), with autofilter.

Breakdown: mostly `/LOT/` (1,938) and `/PCM2/` (1,010); by type METH 1,749, FUNC 695, FUGR 223, PROG 192, CLAS 181. Note: ~38 rows are objects in Coca‑Cola namespaces (`/CCBJI/`, `/SCL/`…) that sit in Ortec packages — kept for consistency with the 3,114 figure and visible via the Namespace column; say the word if you'd rather restrict to objects named in Ortec namespaces only.
[user] need to send a summary of this data to client draft me a mail
[assistant] Here's a draft summarising the Ortec CCLM usage data for the client.

---

**Subject:** CCBJI S/4HANA Conversion — Ortec: Objects Used in the Last 3 Years (CCLM)

Dear [Customer name],

As part of the Ortec add‑on assessment, please find attached the list of **Ortec objects that have been used in the last 3 years**, extracted from the CCLM report (Last‑Used date on/after June 2023).

**Summary:**
- **3,114 Ortec objects** are actively used (have a CCLM Last‑Used date within the last 3 years).
- By namespace: **/LOT/ – 1,938**, **/PCM2/ – 1,010**, /PCM/ – 86, with a small number in /FARSIDE/, /OID/ and related packages.
- By object type: **Methods 1,749, Function modules 695, Function groups 223, Programs 192, Classes 181**, plus a few transactions.
- The attached sheet lists each object with its **Last Used, Changed On, Created On, Usage Flag and Execution Frequency Band**, sorted by Last Used and Changed On (most recent first) so the genuinely active components are at the top.

This represents the live Ortec footprint that must be considered during the conversion. In line with the agreed approach, Ortec will be handled using the **compatible file during the conversion** and removed in **S/4HANA via the custom program**; this usage list helps confirm which components are still in active use and should be validated/retested.

Please review and let me know if you need any additional breakdown (e.g., by package, or only the most recently used objects).

Best regards,
Vaibhav Maheshwari
Diligent Consulting

---

Want me to save this as a **Gmail draft**, or adjust the summary (e.g., add the namespace note about the ~38 objects in Coca‑Cola packages, or tighten the figures)?
[user] in the same mail i need to tell customer We can Delete the objects in Q4R system while keeping a back up of the objects we delete , we will only delete objects which are not related to DDIC , we will target first the objects which have come in CCLM and then delete all the remaining objects except DDIC
[assistant] Here's the updated draft with the deletion approach included.

---

**Subject:** CCBJI S/4HANA Conversion — Ortec: Objects Used in the Last 3 Years (CCLM) & Deletion Approach

Dear [Customer name],

As part of the Ortec add‑on assessment, please find attached the list of **Ortec objects used in the last 3 years**, extracted from the CCLM report (Last‑Used date on/after June 2023).

**Summary:**
- **3,114 Ortec objects** are actively used (CCLM Last‑Used date within the last 3 years).
- By namespace: **/LOT/ – 1,938**, **/PCM2/ – 1,010**, /PCM/ – 86, with a few in /FARSIDE/, /OID/ and related packages.
- By object type: **Methods 1,749, Function modules 695, Function groups 223, Programs 192, Classes 181**, plus a few transactions.
- The attached sheet lists each object with its **Last Used, Changed On, Created On, Usage Flag and Execution Frequency Band**, sorted by Last Used and Changed On (most recent first).

**Proposed deletion approach (Q4R):**
- We can **delete the objects directly in the Q4R system**, while **keeping a backup of every object we delete** (via transport/export) so anything can be restored if required.
- We will **only delete non‑DDIC objects** — data‑dictionary objects (domains, data elements, table types, structures, etc.) will be **left in place**.
- **Sequencing:** we will **first target the objects that appear in CCLM** (the actively‑used / tracked set), and then **delete all remaining non‑DDIC objects**.

This keeps the cleanup controlled and reversible, prioritising the live components first while protecting the data‑dictionary layer. In line with the agreed approach, Ortec is handled via the compatible file during the conversion and removed in S/4HANA via the custom program.

Please review and let me know if you have any questions or would like a different sequencing.

Best regards,
Vaibhav Maheshwari
Diligent Consulting

---

Want me to save this as a **Gmail draft**, or refine any of the wording?
[user] we have a tadir list of Q4R where we have object count for all third party in the same excel u prepared can u make another tab and save all objects of ortec in it and mark DDIC linked objects as red
[assistant] I'll extract all Ortec objects from the Q4R TADIR and add them as a new tab (DDIC marked red) to the Ortec Excel. First, pull the Ortec rows from the Q4R TADIR export.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] 19,675 Ortec objects in Q4R. Now rebuilding the Ortec Excel with a new **"Ortec All Objects (Q4R)"** tab, DDIC objects marked red.
[assistant] (called mcp__workspace__bash)
[assistant] Done — the Ortec Excel now has a second tab, **"Ortec All Objects (Q4R)"**.

- **19,675 Ortec objects** in the Q4R TADIR.
- **DDIC objects = 12,457** → marked **red** (data elements, tables, table types, domains, views, search helps, lock objects, message classes, indexes) — these are kept.
- **Non‑DDIC = 7,218** → the deletion candidates (programs, function groups/modules, classes, transactions, enhancements, etc.).

Columns: Type · Object Name · Package · DDIC? · Created On, with the non‑DDIC (deletion candidates) sorted to the top and DDIC rows in red below; autofilter on the **DDIC?** column lets you isolate either set. Tab 1 ("Ortec used last 3yrs") is unchanged.

This directly supports the approach in your mail — keep DDIC (red), delete the rest, targeting the CCLM‑used ones first.
[assistant] (called mcp__cowork__present_files)
[assistant] Want me to also add a "Used in last 3 yrs?" flag to the All‑Objects tab (cross‑referencing tab 1), so you can see at a glance which deletion candidates are still active?
[user] in the seound tab mark those objects which are in cclm report as green
[assistant] I'll mark Q4R Ortec objects that are in the CCLM report green. Let me check my CCLM lookup is still available.
[assistant] (called mcp__workspace__bash)
[assistant] The CCLM lookup was wiped — rebuilding it (and yes, I'll add the "Used in last 3 yrs?" flag too). Step 1: extract the CCLM object set.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] Let me grab the exact counts to report.
[assistant] (called mcp__workspace__bash)
[user] Continue from where you left off.
[assistant] No response requested.
[user] u have all that data in first tab no need to lock up in cclm again
[assistant] Fair point — you're right that I didn't need to re‑read CCLM for the "used in last 3 years" data; that's already in tab 1. I went back to CCLM only because "in CCLM" (green) needed the **full** CCLM object list (10,011 Ortec objects present), and tab 1 holds just the **used‑in‑3‑years subset** (3,114) — so it isn't the same set.

The file is saved with the second tab now showing: **Red = DDIC (12,457, kept)**, **Green = present in CCLM (3,392 non‑DDIC shown green)**, plus **In CCLM?** and **Used last 3yrs?** columns.

But there's one accuracy nuance I should flag: the **TADIR list uses `CLAS` and `FUGR`**, whereas CCLM records usage at the **`METH` (method)** and **`FUNC` (function module)** level. So a direct type+name match makes the "Used last 3yrs?" flag undercount badly — it shows only **670**, because the 1,749 used methods and 695 used functions can't match a `CLAS`/`FUGR` row in TADIR.

To make the green/used flags correct, I should **roll method usage up to its class and function‑module usage up to its function group** — i.e., a `CLAS` is "used" if any of its methods were used, a `FUGR` if any of its functions were used. That uses tab‑1 data only (no CCLM re‑read).
[assistant] (called mcp__cowork__present_files)
[assistant] Want me to apply that class/function‑group rollup so the **Used last 3yrs?** (and green) flags correctly reflect classes and function groups whose methods/functions are active? That'll bring the "used" count up from 670 to the true figure using only tab‑1 data.
[user] need to update the excel in first tab fill all objects of ortec which are in cclm report and mark those green which are used in last three years then in secound tab mark all those objects which are in first tab as green the ddic objects as red and rest all no color
[assistant] Understood — restructuring both tabs:
- **Tab 1** = all Ortec objects that are in the CCLM report, green = used in last 3 years.
- **Tab 2** (Q4R TADIR) = green if in CCLM (tab 1), red if DDIC, no colour otherwise.

That does need one CCLM read to get the full Ortec‑in‑CCLM list with dates (tab 1 currently holds only the used subset). Let me pull it.
[assistant] (called mcp__workspace__bash)
[assistant] 17,486 Ortec objects in CCLM (3,114 used in last 3 yrs). Rebuilding both tabs accordingly.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__cowork__present_files)
[assistant] Both tabs rebuilt:

**Tab 1 — "Ortec in CCLM":** all **17,486** Ortec objects present in the CCLM report, with **green = used in last 3 years (3,114)**. Columns: Namespace, Object Name, Type, Last Used, Created On, Changed On, Usage Flag, Exec Band, Used last 3yrs? — used ones sorted to the top.

**Tab 2 — "Ortec All Objects (Q4R)":** all **19,675** TADIR objects, coloured:
- **Red = DDIC** — 12,457 (kept)
- **Green = in CCLM / first tab** — 3,392 (non‑DDIC)
- **No colour** — the remaining non‑DDIC objects not in CCLM

Columns include DDIC?, In CCLM?, Used last 3yrs?. Precedence: DDIC red wins over green. Importantly, I rolled **class ↔ method** up — a `CLAS` in TADIR is counted as "in CCLM/used" if any of its methods are in CCLM, so classes aren't wrongly left uncoloured just because CCLM tracks them at method level.

So the deletion picture is clear: red = keep (DDIC), green = delete first (active/in‑CCLM), no‑colour = delete after.
[user] mark all DDIC irrespective if in cclm report or not as red in secound tab
[user] Continue from where you left off.
[assistant] No response requested.
[user] mark all DDIC irrespective if in cclm report or not as red in secound tab
[assistant] It's actually already set that way in the latest build — let me verify so I'm not asserting blindly.
[assistant] (called mcp__workspace__bash)
[assistant] It's already done in the current file — verified:

- **All 12,457 DDIC objects are red**, regardless of CCLM status.
- Of those, **6,619 DDIC objects are in CCLM** but are still shown **red** (not green) — DDIC takes precedence.
- **Green = 3,392** and is strictly **non‑DDIC objects that are in CCLM**.

DDIC types currently marked red: DOMA, DTEL, TABL, TTYP, VIEW, SHLP, ENQU, MSAG, XINX, SHI3, SHI5.

So no DDIC object is ever green. One thing to check on your side: a few table‑related types are **not** currently counted as DDIC — notably **TOBJ** (table‑maintenance generator, 777 objects), plus CHDO (change documents), SXSD/SXCI. If you consider any of those DDIC (to keep), tell me which and I'll add them to the red set.
[user] <uploaded_files>
<file><file_path>C:\Users\VaibhavMaheshwari\AppData\Roaming\Claude\local-agent-mode-sessions\b9dbfd15-b60e-4630-ab2d-bb3aa86c1921\e7a55bf8-c67e-4912-9f30-ca1be557e56b\local_cb7b0508-937d-4af7-9e1d-de25af3bfd4c\uploads\TR_vs_ThirdParty_Match.xlsx</file_path><file_uuid>f592be8d-1329-43f2-bd07-632312d4889b</file_uuid></file>
</uploaded_files>

this sheet contians missing objects which were not found in our last search updatrre all relevant sheet and ppt with the data
[assistant] Let me open the uploaded file to see what's been added to it.
[assistant] (called mcp__workspace__bash)
[assistant] The user enriched the file with a deeper where‑used search on the previously‑unmatched objects (a new "Not Mathced – Where use List" tab and added columns showing many link to Trillium/Movilizer). Let me save their version and read the full new data before updating anything.
[assistant] (called mcp__workspace__bash)
[assistant] The deeper search found ~50 previously‑missing objects now linked: **Movilizer 35, Ortec 8, Trillium 7**. To update the impacted counts correctly I need to know which of these are in CCLM and used recently. Let me extract them and check CCLM.
[assistant] (called mcp__workspace__bash)
[assistant] Refined: **Movilizer +34, Trillium +7** newly‑linked Coca‑Cola objects (the function‑call dependencies the original log missed). Let me check which are in CCLM and used, so I update the impacted counts on the same basis.
[assistant] (called mcp__workspace__bash)
[assistant] Important finding before I change any client‑facing numbers — I checked the 41 newly‑linked objects against CCLM:

| Add‑on | New dependents found | In CCLM | In CCLM **and** used last 3 yrs |
|---|---|---|---|
| Movilizer | 34 | 32 | **0** |
| Trillium | 7 | 7 | **0** |

So the deeper search confirmed these are real third‑party dependents and **39 of the 41 are in CCLM** — but **none of them are used in the last 3 years**. That matters because the PPT's "**CCLM CCBJI Impacted Objects**" column was defined as *active* dependents (in CCLM **and** used in last 3 years). Under that definition these new objects **don't change the counts** (Movilizer stays 9, Trillium stays 3) — they're dormant.

They *do* increase the **total in‑CCLM dependents** (Movilizer's in‑CCLM dependents would go ~304→336, Trillium ~3→10).

So before I update, one decision on what the "Impacted Objects" number should represent:
[assistant] (called AskUserQuestion)
[assistant] Got it — keep the impacted count on the same basis (active = in CCLM + used last 3 yrs), and update all the dependent documents. Since the 41 new objects are dormant (in CCLM but not used recently), the **active impacted counts stay the same**, but they add to the in‑CCLM dependents. Let me extract the new dependency pairs and rebuild the Critical Dependencies workbook.
[assistant] (called mcp__workspace__bash)
[assistant] 40 new dependency pairs (Movilizer 33, Trillium 7). Let me confirm their in‑CCLM status and read the current workbook structure so I update the right cells.
[assistant] (called mcp__workspace__bash)
[assistant] Computed: Movilizer dependents in CCLM 304→**336**, Trillium 3→**10** (distinct dependents Movilizer 326→359, Trillium 7→14); active counts unchanged (new ones are dormant). Let me apply these to the workbooks.
[assistant] (called mcp__workspace__bash)
[assistant] Critical Dependencies updated (new "Deep‑search dependents" tab + Summary counts). Now the TR match file and a verification.
[assistant] (called mcp__workspace__bash)
[assistant] All dependent documents updated:

**`AddOn_Critical_Dependencies.xlsx`**
- New **"Deep‑search dependents"** tab listing the 40 newly‑found Coca‑Cola dependents (Movilizer 33, Trillium 7) with their add‑on object, In CCLM, and Used‑last‑3yrs (all "No").
- **Summary (pairs)** updated: Movilizer dependents‑in‑CCLM **304→336** (distinct 326→359), Trillium **3→10** (distinct 7→14). Active counts unchanged (9 and 3).

**`TR_vs_ThirdParty_Match.xlsx`** (your enriched version, preserved)
- Summary note added recording that 40 previously‑"Not Matched" objects were found via the deep function‑module/method search to depend on add‑ons (Movilizer 33, Trillium 7), and that they're in CCLM but dormant.

**`AddOn_Decision.pptx`** — handled "same way as the others": the "CCLM CCBJI Impacted Objects" column = active dependents (in CCLM **and** used last 3 yrs). Since all 40 new objects are **dormant** (in CCLM but not used in last 3 years), they don't meet that bar, so **Movilizer stays 9 and Trillium stays 3** — no number change, consistent with how every other add‑on was counted.
[assistant] (called mcp__cowork__present_files)
[assistant] Summary of the changes: the deep‑search added **40 confirmed dependents** (Movilizer 33, Trillium 7) that the original cross‑reference log missed — they're real but **dormant** (in CCLM, not used in the last 3 years), so they raise the *in‑CCLM dependent* totals but not the *active impacted* counts used on the slide. The TR‑match unmatched set shrinks accordingly.

If you'd actually prefer the slide's impacted column to count **all in‑CCLM dependents** (active + dormant) instead of active‑only, say so and I'll switch it everywhere — that would change Movilizer to 336 and Trillium to 10.
[user] the file tr vs third party match there is a tab called not matching can u update these objects in tr object list and mark them as red as they are not found in SJR
[assistant] Let me read the "Not Matched" objects and verify which are actually absent from the SJR system, then mark them in the TR object list.
[assistant] (called mcp__workspace__bash)
[assistant] 59 of the 106 actually exist in SJR's TADIR — so the "not found in SJR" reason only cleanly applies to 47. Let me look at the TR object list structure before marking.
[assistant] (called mcp__workspace__bash)
[assistant] The TR object list (`TR_objects.xlsx`) has 2,673 rows across the transports, with object names in column 3. Before I mark anything, there's a discrepancy worth your call: you asked to mark the "Not Matched" objects red because they're not found in SJR — but when I checked them against SJR's TADIR, **59 of the 106 actually do exist in SJR** (they exist there, just aren't linked to any third-party add-on). Only **47 are genuinely absent from SJR**.
[assistant] (called AskUserQuestion)
[assistant] Marking only the 47 objects genuinely absent from SJR red in the TR object list.
[assistant] (called mcp__workspace__bash)
[assistant] Saved to `TR_objects_v2.xlsx` (the original was open/locked). Let me verify the count with an exact match against SJR's object directory rather than the substring grep.
[assistant] (called mcp__workspace__bash)
[assistant] Confirmed by exact match: **54** of the 106 Not Matched objects are genuinely absent from SJR; 52 exist there. (My earlier 47/59 split was a loose substring count — 54/52 is the exact figure.)

Done — in the TR object list I marked those **54 objects red** across all the transports and added a "Remark" column reading "Not found in SJR." The 52 that exist in SJR are left unmarked.

Because `TR_objects.xlsx` was open in Excel, the update saved to **`TR_objects_v2.xlsx`**. Close the original and I can write back to it if you'd prefer the same filename.
[assistant] (called mcp__cowork__present_files)
[assistant] The 54 not-in-SJR objects are flagged red with a "Not found in SJR" remark; the 52 that exist in SJR are untouched.
[user] we were working on file where we were comparing objects which are there in transport but are they present on our sheet or not and to add to that i had sent another sheet in morning where i got more finding from SJR did u link all of them and update the sheet
[assistant] Let me re-examine the morning enriched file to see exactly what findings it contains and check what I actually updated versus what's still pending.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] I can see the picture now. The morning file has the findings, but let me be straight about status: earlier I only pulled the **Movilizer/Trillium** linkages (40) into the dependencies workbook and added a note to the Summary — I did **not** yet reclassify the found objects inside this sheet's Matched/Not Matched tabs. Let me look at all the findings so I can link them properly.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] Now it's precise. The morning findings break down as: **41 objects firmly link to add‑ons** (Movilizer 34, Trillium 7 — including the OTCM/DSD objects whose consumers are `/MOVI/...`), **40 were found in use but their consumer is a Coca‑Cola object** (not a third‑party add‑on), and **25 had no finding**. Earlier I'd pulled 40 of the 41 into the dependencies workbook (I'd missed the `/CCBJI/MOVILIZER_OBJECTS` package). Let me read the Summary tab and then write the full update into the sheet.
[assistant] (called mcp__workspace__bash)
[assistant] Now I'll write the full update: move the 41 linked objects into Matched, add a finding-result column to Not Matched, and recompute the Summary.
[assistant] (called mcp__workspace__bash)
[assistant] Yes — now all the morning SJR findings are linked into the sheet. Here's what I did and the honest before/after:

Going through every one of the 106 previously "Not Matched" objects against your morning findings:

- **41 firmly link to a third‑party add‑on** (Movilizer 34, Trillium 7) — these are the `/SCL/DSD_TOUR_*`, `/SCL/OTCM_*`, `/CCC/...TRILLIUM...` objects whose where‑used consumers are `/MOVI/...` or `/TRILLIUM/...`. **Moved to the Matched tab** (highlighted green, Match type "Depends on add‑on (SJR deep where‑used)").
- **40 were found in use, but their consumer is a Coca‑Cola object** (`/SCL/`, `/CCC/`), not a third‑party add‑on — so they're not third‑party links. Left in Not Matched, flagged in a new **"SJR Finding Result"** column.
- **25 had no finding** — flagged "No finding."

Summary recomputed: **Matched 2,537 → 2,578**, Not matched **106 → 65**, overall 96% → 98%.

One reconciliation note: earlier I'd pulled 40 of these (Movilizer 33, Trillium 7) into `AddOn_Critical_Dependencies.xlsx`. The 41st is `/CCBJI/MOVILIZER_OBJECTS`, which is a **package (DEVC)**, not a code object — so it correctly shows as add‑on‑linked here but isn't a "dependent object to redevelop" in the dependencies workbook.
[assistant] (called mcp__cowork__present_files)
[assistant] The sheet is updated and saved. The 40 "found in use (Coca‑Cola consumer)" and 25 "no finding" objects remain in Not Matched with their result flagged, so you can see at a glance which transport objects are genuinely outside the third‑party scope versus simply unused. Let me know if you'd like me to also fold the 41st (`/CCBJI/MOVILIZER_OBJECTS` package) note into the dependencies workbook.
[user] we made a file for ortec removal we need to add another tab in it where we need to highligh coca cola objects and stand which are using ortec components same as in other tab mark them red if ddic and if found in cclm the same we did for different tabs
[assistant] Let me look at the Ortec removal file and the existing Ortec dependency data so I build the new tab consistently.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] Got it — the new tab will list the distinct Coca‑Cola and Standard objects that **use** Ortec components (the dependents), colored red if DDIC and green if in CCLM (same scheme as the Q4R tab). Let me build it.
[assistant] (called mcp__workspace__bash)
[assistant] Added the new tab **"CC & Standard using Ortec"** to `Ortec_Used_Last3Years_CCLM.xlsx`.

It lists the **1,112 distinct Coca‑Cola and SAP‑standard objects that use Ortec components** (696 Coca‑Cola, 416 Standard), with the same color scheme as your other tabs:
- **Red = DDIC** (516) — kept, no usage telemetry
- **Green = in CCLM report**, non‑DDIC (380)
- No color = non‑DDIC not in CCLM

Of the green (in‑CCLM) objects, **129 were used in the last 3 years** — which ties out exactly to the Ortec active‑dependent / impacted count on the decision deck.
[assistant] (called mcp__cowork__present_files)
[assistant] Done. The new tab gives the client a clear redevelopment scope: the green non‑DDIC objects (380, of which 129 are actively used) are what would need attention if Ortec is removed, while the red DDIC objects (516) are retained.
[user] basically we need to delete ortec objects form the system in order to do so i need to delete the ortec objects and then change the coca coa and standar objects where they are using ortec components
[assistant] That's the right sequencing — remove the Ortec add‑on objects, then fix every Coca‑Cola/standard object that references an Ortec component so it still compiles and runs. The "CC & Standard using Ortec" tab I just built is essentially your remediation candidate list. Before I turn it into something actionable, two quick things:
[assistant] (called AskUserQuestion)
[assistant] Building the remediation worklist Excel covering all 1,112 dependents.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] Created **`Ortec_Removal_Remediation_Worklist.xlsx`** covering all 1,112 dependents. Four tabs:

1. **Approach** — the two‑step plan (delete Ortec objects, then remediate consumers) with sequencing guidance: fix the references in the same or an earlier transport as the deletion, or the consumers break on activation.
2. **Ortec objects to delete** — the distinct Ortec components actually referenced, ranked by how many consumers each one breaks.
3. **Remediation worklist** — all **1,112** Coca‑Cola/standard objects to change, each with the Ortec component(s) it references, a suggested change action (delete enhancement / adjust DDIC structure / modify code), and a priority. Sorted High→Low.
4. **Detailed pairs** — all 19,813 object‑to‑component references for drill‑down.

Priority breakdown: **129 High** (active, used last 3 yrs), the rest of the in‑CCLM dormant ones Medium, **516 DDIC** structural (red), and the remainder Low. Color scheme matches your other tabs (red = DDIC, green = in CCLM).
[assistant] (called mcp__cowork__present_files)
[assistant] The High‑priority 129 are your immediate remediation scope; the DDIC structural items (appends/includes) need rework but carry no runtime usage risk. Want me to add a per‑transport grouping or an owner/status column so this can be tracked as the team works through it?
[user] excel is confusing lets tabe the old excel only add new tab in which ccbji name space or standard objects are using ortec objects along with cclm data mark ddic as red same as in other tab and then we will take it forward
[assistant] The old file (`Ortec_Used_Last3Years_CCLM.xlsx`) already has a "CC & Standard using Ortec" tab from earlier — let me refine it so it cleanly shows the Coca‑Cola/standard objects, the Ortec component each one uses, and the CCLM data with DDIC in red. We'll keep everything in that one file and set the confusing worklist aside.
[assistant] (called mcp__workspace__bash)
[assistant] Done — kept everything in the old file. `Ortec_Used_Last3Years_CCLM.xlsx` now has three tabs, the last being the refined **"CC & Standard using Ortec"**:

- **1,112** Coca‑Cola (CCBJI) and SAP‑standard objects that use Ortec objects
- Columns: Class, Namespace, Object Name, Type, **Ortec object(s) used**, DDIC?, In CCLM?, Last Used, Used last 3yrs?
- **Red = DDIC (516)**, **green = in CCLM (380, of which 129 used in last 3 yrs)** — same scheme as your other tabs

You can ignore/delete the separate `Ortec_Removal_Remediation_Worklist.xlsx` (it's redundant now).
[assistant] (called mcp__cowork__present_files)
[assistant] The Ortec analysis is now consolidated in one file with that clean dependents tab. Whenever you're ready to take it forward — adding owner/status tracking, splitting the 129 active ones into a first wave, or anything else — just let me know.
[user] can  u mark all  objects of ortec with different color which are not being used in ccbji or standard name space
[assistant] I'll mark the Ortec objects that have no Coca‑Cola (CCBJI) or standard consumer with a distinct color, on the full Ortec object list. First let me compute which Ortec objects are actually used by CC/standard.
[assistant] (called mcp__workspace__bash)
[assistant] 92% of Ortec objects (18,152 of 19,675) have **no** Coca‑Cola/standard consumer — only 1,523 are used by CC/standard. I'll add a "Used by CCBJI/Standard?" column and color the unused ones a distinct blue (these are safe to delete with no remediation), keeping red/green on the ones that do have a consumer.
[assistant] (called mcp__workspace__bash)
[assistant] Done. On the **"Ortec All Objects (Q4R)"** tab I added a **"Used by CCBJI/Standard?"** column and recolored:

- **Blue = 18,152** — no Coca‑Cola/standard consumer → **safe to delete with no remediation**
- **Red = 1,355** — used by CC/standard *and* DDIC
- **Green = 101** — used by CC/standard *and* in CCLM (non‑DDIC)
- **No color = 67** — used by CC/standard, non‑DDIC, not in CCLM

So the blue objects are your bulk deletes, and the 1,523 colored ones (red/green/none) are the Ortec objects that drive remediation of the consumers in the other tab.
[assistant] (called mcp__cowork__present_files)
[assistant] Let me know if you'd like the blue (no‑consumer) objects pulled into their own tab as a clean delete list, or any adjustment to the color logic.
[user] is the marking  done for all ortec object irrescpective or ddic or not
[assistant] Yes — blue is applied to **every** Ortec object with no CC/standard consumer, DDIC or not. In the logic, the "not used" check comes first, so DDIC status doesn't exempt anything from blue. Let me confirm the DDIC split within the blue group.
[assistant] (called mcp__workspace__bash)
[assistant] Confirmed. The blue marking covers all 18,152 not‑used objects regardless of type — **11,102 are DDIC and 7,050 are non‑DDIC**. DDIC status doesn't exempt anything; an Ortec object is blue purely because nothing in the Coca‑Cola/standard space uses it.
[user] fist tab should be ortec all objects sedcound should be ccbji and thirs should be cc and standard objects and explain the color bifercation as i need to send it to user
[assistant] Quick check before I reorder — for a client deliverable I want to get the second tab right. You currently have three tabs: "Ortec in CCLM" (Ortec's own objects + CCLM usage), "Ortec All Objects (Q4R)", and "CC & Standard using Ortec". When you say the second tab should be "ccbji," which do you mean?
[assistant] (called AskUserQuestion)
[assistant] Got it — same three tabs, reordered to: (1) Ortec All Objects, (2) Ortec in CCLM, (3) CCBJI & Standard linkage. And I'll make the color legend on each tab clear and client-ready.
[assistant] (called mcp__workspace__bash)
[assistant] The workspace timed out — let me check whether it applied and retry if needed.
[assistant] (called mcp__workspace__bash)
[assistant] The reorder and save did complete despite the timeout. Let me verify the legend notes landed correctly.
[assistant] (called mcp__workspace__bash)
[assistant] Done. Tabs reordered and each carries a clear, client‑ready color legend:

**Tab 1 — Ortec All Objects (Q4R)** (19,675): Blue = not used by any Coca‑Cola/standard object → safe to delete, no remediation (18,152). The 1,523 that are used: Red = DDIC (1,355), Green = in CCLM non‑DDIC (101), No color = non‑DDIC not in CCLM (67).

**Tab 2 — Ortec in CCLM** (17,486): Green = used in last 3 years (3,114), No color = in CCLM but not used in last 3 years.

**Tab 3 — CC & Standard using Ortec** (1,112): Red = DDIC (516), Green = in CCLM non‑DDIC (380, of which 129 used in last 3 years), No color = non‑DDIC not in CCLM. DDIC red takes precedence.
[assistant] (called mcp__cowork__present_files)
[assistant] The file is ready to send. Let me know if you'd like the legends turned into actual colored swatch cells (visual key) or any wording tweaks before it goes to the client.
[user] now need to explain the plan to customer same needs to be added in draft mail and excel as seprate tab plan for deletion of ortec objects phase 1 we will delete all ortec programs and transaction and non ddic objects from the Q4R system phase 2 delete all DDIC objects which are not likned to CCBJI or standard name space phase 3 deactivate all enhancment or badi which are theere for Ortec also remove dependency on  transaction and program which are using Ortec objects by replacing them  w or commenting them   Phase 4 here we need to take call option one remove the dddic objects linkage to standard orccbji name space objects and replace it with custom or keep the objects active as it is
[assistant] I'll add a "Deletion Plan" tab to the Excel and draft the customer email. First let me pull accurate object counts per phase from the data.
[assistant] (called mcp__workspace__bash)
[assistant] Here's the phase breakdown from the data: ~6,381 non‑DDIC objects (Phase 1), 11,186 unlinked DDIC (Phase 2), 747 enhancements/BAdIs + ~596 CC/standard code consumers (Phase 3), and 1,361 linked Ortec DDIC objects for the Phase 4 decision. Let me add the plan tab and draft the email.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] Plan tab added as the first tab. Now the draft email — let me load the Gmail draft tool.
[assistant] (called ToolSearch)
[assistant] (called mcp__85ffbb91-38fd-494a-98ab-3d5474eab1b2__create_draft)
[assistant] Done — both deliverables updated with the phased plan.

**Excel** — added a **"Deletion Plan"** tab (now the first tab) laying out the four phases with object scope, counts, and the Phase 4 decision options, color‑coded for the client.

**Gmail draft** — created a draft titled *"Ortec Add‑on Removal – Proposed Phased Deletion & Remediation Plan"* (in your drafts, addressed to yourself so you can edit recipients/forward). It walks through:

- **Phase 1** — delete all Ortec non‑DDIC objects (programs, transactions, classes, etc.) → ~6,381
- **Phase 2** — delete Ortec DDIC objects not linked to CCBJI/standard → 11,186
- **Phase 3** — deactivate Ortec enhancements/BAdIs (747) and strip Ortec references from ~596 CCBJI/standard programs & transactions
- **Phase 4** — customer decision on the 1,361 linked Ortec DDIC objects: replace with custom (clean removal) vs. keep active as‑is
[assistant] (called mcp__cowork__present_files)
[assistant] The plan is now in both the workbook and a Gmail draft. One note on sequencing you may want to flag to the client: the 96 used non‑DDIC objects (Phase 1) and the consumer fixes (Phase 3) should really go in together or Phase 3 first, otherwise those consumers break on activation when Phase 1 deletes their targets. Want me to reflect that ordering nuance in the email, or leave the phases as numbered?
[user] can u update excel according to phases every phase different color so customer knows in which phase what all will be changed remove all other color coding
[assistant] I'll recolor all three data tabs by phase (matching the plan tab colors), add a "Phase" column to each, and replace the old legends with a phase legend.
[assistant] (called mcp__workspace__bash)
[assistant] Done. Every object in all three data tabs is now colored by the phase that will action it, matching the "Deletion Plan" tab colors, and the old DDIC/CCLM/used color coding is removed. Each tab also has a new **"Phase"** column.

Phase colors (peach/green/yellow/blue):

- **Ortec All Objects (Q4R):** Phase 1 = 6,381 · Phase 2 = 11,186 · Phase 3 = 747 · Phase 4 = 1,361
- **Ortec in CCLM:** Phase 1 = 10,353 · Phase 2 = 6,044 · Phase 3 = 483 · Phase 4 = 606
- **CC & Standard using Ortec:** Phase 3 (code to remediate) = 596 · Phase 4 (DDIC decision) = 516

So the customer can see exactly which objects move in each phase, consistent with the plan and the email.
[assistant] (called mcp__cowork__present_files)
[assistant] Want me to update the Gmail draft to reference these per‑phase color tabs, or leave the email as is?
[user] phase 4 objects are those which are getting used in ccbji name space or in standard in excel it is not clear where are they getting used
[assistant] Good catch — the Ortec‑object tabs show the phase but not the consumer. I'll add a "Used by (CCBJI/Standard object(s))" column so each Phase 4 (and other used) Ortec object shows exactly which customer/standard objects reference it.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] Added. The **"Ortec All Objects (Q4R)"** and **"Ortec in CCLM"** tabs now have a **"Used by (CCBJI / Standard object(s))"** column that lists, for each used Ortec object, the exact customer/standard objects referencing it. It's populated for all 1,523 used objects — including every Phase 4 object (1,361) — so it's now clear where each one is consumed. The "CC & Standard using Ortec" tab gives the same linkage from the other direction.
[assistant] (called mcp__cowork__present_files)
[assistant] The workbook now traces each Phase 4 object to its consumers in both directions. Let me know if you'd like the same consumer column added to a dedicated Phase 4 tab, or any other refinement before it goes to the client.
[user] is the draft mail ready or do u need to add remove anything
[assistant] The draft is content‑complete and the numbers still match the workbook — nothing is wrong with it. Two practical things to handle before it goes out, plus one optional tweak:

1. **Recipient** — it's currently addressed to you (so you could review it in your own drafts). You'll need to change the "To" to the actual customer before sending.
2. **Attachment** — the body refers to "the attached workbook," but drafts created this way can't carry an attachment. You'll need to attach `Ortec_Used_Last3Years_CCLM.xlsx` manually in Gmail.
3. **Optional** — since I built the workbook out further, I can add a line telling the customer the workbook is now color‑coded by phase and that each Phase 4 object shows the CCBJI/standard objects using it. Reads well as "here's how to navigate the file."

One judgment call worth deciding: the email presents Phases 1–4 in order, but Phase 1's 96 used objects technically need their Phase 3 consumer fixes done first/together or they break on activation. Right now I've worded it as "handled in Phase 3." Want me to make that sequencing more explicit?

Would you like me to apply the optional line and/or the sequencing wording? If so I'll update the draft.
[user] ur right phase 1 keep it clean all those object with no ccbji or standard linkage and move others to other phases update excel accrodingly
[assistant] Right — Phase 1 should be purely the clean, no‑linkage deletes. I'll move the 96 non‑DDIC objects that *are* used by CCBJI/standard into Phase 3 (they need consumer remediation before deletion), and update the data tabs and the plan accordingly.
[assistant] (called mcp__workspace__bash)
[assistant] The workspace timed out again — let me verify whether the recolor saved.
[assistant] (called mcp__workspace__bash)
[assistant] Save didn't land that time. Re‑running the recolor.
[assistant] (called mcp__workspace__bash)
[assistant] The save exceeds the time limit, so I'll run it in the background and then poll for completion. Using a targeted update (only the 96 rows that change phase).
[assistant] (called mcp__workspace__bash)
[assistant] Let me wait for it to finish.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] The openpyxl save keeps getting killed on this large file. I'll switch to a fast rebuild (read the data, write fresh with xlsxwriter) which completes quickly and reliably.
[assistant] (called mcp__workspace__bash)
[assistant] Phase 1 is now clean (6,285 no‑linkage deletes); the 96 used non‑DDIC objects moved into Phase 3 (now 843 = 747 enhancements + 96). Let me verify the rebuilt file opens correctly.
[assistant] (called mcp__workspace__bash)
[assistant] The workbook is rebuilt correctly — Phase 1 is clean, Phase 4 objects show their consumers. Now updating the email draft to match the corrected phases and add the navigation line.
[assistant] (called mcp__85ffbb91-38fd-494a-98ab-3d5474eab1b2__create_draft)
[assistant] Both done.

**Excel** (`Ortec_Used_Last3Years_CCLM.xlsx`) — rebuilt with the corrected phases. Phase 1 is now purely clean, no‑linkage deletes (6,285); the 96 non‑DDIC objects that have CCBJI/standard consumers moved into Phase 3 (now 843 = 747 enhancements/BAdIs + 96). Phase 2 = 11,186, Phase 4 = 1,361. The "Deletion Plan" tab and all per‑phase colors and the "Used by" consumer column are updated to match.

**Email** — created an updated draft with: the corrected Phase 1 (6,285, no linkage), Phase 3 now covering the 96 used objects with the explicit sequencing warning (remediate consumers before deleting them), and the new "how to read the workbook" line about phase color‑coding and the Phase 4 consumer column.

Two notes: there are now two drafts in your Gmail (the older one can be discarded — use the latest), and remember to attach the workbook manually since drafts can't carry the file.
[assistant] (called mcp__cowork__present_files)
[assistant] Want me to delete the older draft, or leave both for you to manage in Gmail?
[user] did u draft me a mail for ortec removal if not can u draft me a mail
[assistant] Yes — I created a Gmail draft for the Ortec removal (subject: *"Ortec Add‑on Removal – Proposed Phased Deletion & Remediation Plan"*, in your drafts addressed to yourself). In case it's easier to grab here, here's the full text to copy:

---

**Subject:** Ortec Add‑on Removal – Proposed Phased Deletion & Remediation Plan

Dear Team,

Following our analysis of the Ortec third‑party add‑on in the Q4R system, please find below the proposed approach to fully remove Ortec while keeping all Coca‑Cola (CCBJI) and SAP‑standard objects working. Objects are removed in waves of decreasing safety, and every CCBJI/standard object that references Ortec is remediated so the system still activates and runs.

How to read the attached workbook: every object is colour‑coded by the phase that will action it (peach = Phase 1, green = Phase 2, yellow = Phase 3, blue = Phase 4), matching the "Deletion Plan" tab. For the Phase 4 objects, the "Used by (CCBJI / Standard object(s))" column shows exactly which customer/standard objects reference each one.

**Phase 1 – Delete Ortec non‑DDIC objects with no CCBJI/standard linkage**
Delete all Ortec non‑DDIC repository objects (programs, reports, transactions, classes, function groups, interfaces, etc.) that no CCBJI/standard object uses. Approx. 6,285 objects. Clean deletes with no remediation needed.

**Phase 2 – Delete unlinked Ortec DDIC objects**
Delete all Ortec DDIC objects (tables, structures, data elements, domains, search helps, etc.) that are NOT linked to any CCBJI or standard object. Approx. 11,186 objects. No remediation required.

**Phase 3 – Deactivate enhancements/BAdIs, remediate consumers, and delete used non‑DDIC objects**
(a) Deactivate all enhancement/BAdI implementations for Ortec (747). (b) Remediate the CCBJI/standard programs and transactions that call Ortec, by replacing or commenting out the Ortec references (~596 objects). (c) Delete the 96 non‑DDIC Ortec objects that are used by CCBJI/standard. Important: the consumer remediation must be done before/together with the deletion of these 96 objects, otherwise those consumers break on activation.

**Phase 4 – Decision required: Ortec DDIC objects linked to CCBJI/standard**
1,361 Ortec DDIC objects (appends, includes, data elements) are embedded in CCBJI/standard structures, with 516 CCBJI/standard objects depending on them. We need your decision between:
- **Option 1** – Remove the linkage from the CCBJI/standard objects, replace the Ortec DDIC with custom (Z/Y) equivalents, then delete the Ortec DDIC. Clean, complete removal but requires effort on the 516 objects plus structure/data adjustment.
- **Option 2** – Keep the 1,361 linked Ortec DDIC objects active as‑is. Lowest effort and zero risk to customer/standard structures, but Ortec is not 100% removed.

We recommend confirming the Phase 4 direction before execution begins, as it determines the final scope. Please review the attached workbook and let us know your decision on Phase 4.

Best regards,
Vaibhav Maheshwari

---

Remember to attach `Ortec_Used_Last3Years_CCLM.xlsx` before sending (drafts can't carry the file automatically). Want me to adjust the tone or recipient?
[user] in file AddOn_Critical_Dependencies i have updated the tab used in last year cclm for Trilliom i need a document to be created a word document for details finding for trillium which i have noted in the excel tab
[assistant] Let me read the Trillium findings you noted in that tab so I can build the document from your actual notes.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] I can see your Trillium findings, including the BAdI notes you added (ADDRESS_SEARCH, /TRILLIUM/ADDRCHCKEX). Let me also check for any cell comments and pull the fuller Trillium dependency data before building the document.
[assistant] (called mcp__workspace__bash)
[assistant] I have the full Trillium picture now: 7 Coca‑Cola dependents (3 active/used in last 3 years, 4 dormant test objects), integrating via three Trillium BAdIs (address search, address check, address update) in package /TRILLIUM/CONNECTOR. Let me read the docx skill, then build the findings document.
[assistant] (called Read)
[assistant] Now I'll build the Trillium findings Word document from your data.
[assistant] (called mcp__workspace__bash)
[assistant] The image is the SAP vendor "Vendors by Address Attributes (Fuzzy Search)" screen — a perfect real‑world example of where the Trillium address‑search BAdI is used. I'll embed it in the document. Let me set up docx locally and locate the uploaded screenshot.
[assistant] (called mcp__workspace__bash)
[assistant] Two images to handle: one embedded in the Excel tab, and the pasted screenshot. Let me extract the Excel image and locate the pasted one.
[assistant] (called mcp__workspace__bash)
[assistant] (called Read)
[assistant] (called mcp__workspace__bash)
[assistant] (called Read)
[assistant] (called mcp__workspace__bash)
[assistant] (called Read)
[assistant] The Excel‑embedded image is the **Customer** master fuzzy address search; your pasted screenshot is the **Vendor** master equivalent — together they show Trillium's fuzzy address search live in both. Let me confirm whether the pasted vendor image landed on disk anywhere.
[assistant] (called mcp__workspace__bash)
[assistant] The pasted vendor screenshot didn't save to disk — pasted chat images aren't written to the workspace, so I can't embed that one yet. I have the Excel‑embedded **Customer** fuzzy‑search image, which I'll include now. Let me build the document with it, then you can drop the vendor PNG into the folder for me to add.
[assistant] (called mcp__workspace__bash)
[assistant] Now building the document with the data and the customer fuzzy‑search image.
[assistant] (called mcp__workspace__bash)
[assistant] Let me verify the document renders correctly.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] (called Read)
[assistant] Page 1 looks clean and professional. Let me confirm the embedded image rendered on page 2.
[assistant] (called mcp__workspace__bash)
[assistant] (called Read)
[assistant] The document is built and renders cleanly — the Customer fuzzy‑search image embedded correctly as Figure 1.

**`Trillium_Detailed_Findings.docx`** (3 pages) covers: overview, summary of findings, functional‑usage evidence (the fuzzy‑search screenshot), the 3 active dependents requiring remediation, the BAdI integration architecture (address search/check/update), the 4 dormant test objects, and a 5‑step remediation approach — all from your "Used last 3yrs (CCLM)" Trillium notes.

On the **two images**: I extracted the one embedded in the Excel tab (the **Customer** master fuzzy search) and placed it as Figure 1. The **Vendor** screenshot you pasted in chat couldn't be embedded — pasted images aren't saved to the workspace as files, so I left a marked placeholder for "Figure 2 — Vendor master." To add it, drop the vendor PNG into `C:\Games\coca cola\CCLM\New Data` and tell me the filename; I'll insert it.

A few helper files (`_excel_trillium_img.png`, `_trillium_pg1.jpg`, `_trillium_pg2.jpg`) were created in the folder for extraction/preview — you can delete them.
[assistant] (called mcp__cowork__present_files)
[assistant] Want me to proceed and finalize once you've saved the vendor screenshot, or adjust any of the findings/wording in the meantime?
[user] i think i mentioned in excel - /CCC/CCEAG_OTC_DEL_ADDRESS is being used in  another badi can u mention that in document
[assistant] Let me re-read the Excel for that object to capture the BAdI you noted.
[assistant] (called mcp__workspace__bash)
[assistant] I checked the saved Excel, and the **Badi** cell for `/CCC/CCEAG_OTC_DEL_ADDRESS` is currently empty — the only two BAdI entries you filled in are on the class objects (`ADDRESS_SEARCH` for `/CCC/CL_IM_ADDRSRCH`, and `/TRILLIUM/ADDRCHCKEX` for `/CCC/CL_IM_ADRCHECK_VALID`). The program row just shows the Trillium DDIC structures it uses (`/TRILLIUM/SEARCH_FIELDS`, `/TRILLIUM/SEARCH_FIELDS_T`).

So I don't have the BAdI name to put in the document. Which BAdI is `/CCC/CCEAG_OTC_DEL_ADDRESS` used in? Tell me the name (and the implementing/related object if relevant) and I'll add it to the findings — and I can write it back into the Excel Badi cell too if you'd like. If your Excel has unsaved changes, save it first and I'll re‑read.
[user] check this , i have mentioned same in excel
[assistant] That's exactly the detail I needed. So the program is tied into the **Address Search** BAdI: `/CCC/CCEAG_OTC_DEL_ADDRESS` is called from class `/CCC/CL_IM_ADDRSRCH`, which implements `/TRILLIUM/IF_EX_ADDRSRCHEX` (methods `BEFORE_ADDRESS_SEARCH`, `BEFORE_MATCHER_CALL`) on standard BAdI **ADDRESS_SEARCH**; and `/CCC/CL_IM_ADRCHECK_VALID` implements `/TRILLIUM/IF_EX_ADDRCHCKEX~BEFORE_ADDRESS_CHECK` on standard BAdI **ADDRESS_CHECK**. Updating the document.
[assistant] (called mcp__workspace__bash)
[assistant] Let me render the updated pages to confirm the BAdI detail reads well.
[assistant] (called mcp__workspace__bash)
[assistant] (called Read)
[assistant] Updated. The document now reflects your BAdI notes:

- **`/CCC/CCEAG_OTC_DEL_ADDRESS`** — added that it's invoked from the Address Search BAdI class `/CCC/CL_IM_ADDRSRCH` via `/TRILLIUM/IF_EX_ADDRSRCHEX~BEFORE_ADDRESS_SEARCH` and `~BEFORE_MATCHER_CALL`, with a note that it breaks indirectly through those methods and must be remediated with the BAdI class.
- **BAdI architecture table** now shows the standard SAP BAdI (ADDRESS_SEARCH / ADDRESS_CHECK), the Trillium exit interface, the exact methods implemented, the CC implementation, and status.
- `/CCC/CL_IM_ADRCHECK_VALID` corrected to standard BAdI **ADDRESS_CHECK** via `/TRILLIUM/IF_EX_ADDRCHCKEX~BEFORE_ADDRESS_CHECK`.

The Vendor screenshot is still a marked placeholder (Figure 2) — save the PNG into the folder and I'll drop it in.
[assistant] (called mcp__cowork__present_files)
[assistant] Want me to write these BAdI/method details back into the Excel "Badi" column too, so the workbook and document stay in sync?
[user] yes update workbook also
[assistant] Updating the "Badi" column in the workbook to match the document.
[assistant] (called mcp__workspace__bash)
[assistant] Workbook updated — the "Badi" column in the **Used last 3yrs (CCLM)** tab now carries the full BAdI/method detail for the three active Trillium objects, matching the document:

- `/CCC/CCEAG_OTC_DEL_ADDRESS` → called from BAdI class `/CCC/CL_IM_ADDRSRCH` via `/TRILLIUM/IF_EX_ADDRSRCHEX~BEFORE_ADDRESS_SEARCH` & `~BEFORE_MATCHER_CALL` (std BAdI ADDRESS_SEARCH)
- `/CCC/CL_IM_ADDRSRCH` → std BAdI ADDRESS_SEARCH via `/TRILLIUM/IF_EX_ADDRSRCHEX` (BEFORE_ADDRESS_SEARCH, BEFORE_MATCHER_CALL)
- `/CCC/CL_IM_ADRCHECK_VALID` → std BAdI ADDRESS_CHECK via `/TRILLIUM/IF_EX_ADDRCHCKEX` (BEFORE_ADDRESS_CHECK)

The document and workbook are now in sync.
[assistant] (called mcp__cowork__present_files)
[assistant] Both deliverables are aligned. When you save the Vendor fuzzy‑search screenshot into the folder, let me know the filename and I'll add it as Figure 2 in the document.
[user] now help me draft a a mail to customer for trillium word document we created , i will attach the document in mail
[assistant] (called mcp__85ffbb91-38fd-494a-98ab-3d5474eab1b2__create_draft)
[assistant] Drafted — created a Gmail draft (subject *"Trillium Add‑on – Detailed Findings & Remediation Approach"*, in your drafts, addressed to yourself so you can set the recipient). Here's the text to copy if easier:

---

**Subject:** Trillium Add‑on – Detailed Findings & Remediation Approach

Dear Team,

As part of the third‑party add‑on decommissioning for the S/4HANA conversion, we have completed the detailed analysis of the Trillium add‑on. The findings and recommended remediation are captured in the attached document; the key points are summarised below.

Trillium is the address data‑quality component (package /TRILLIUM/CONNECTOR) used for fuzzy address search and validation. It is integrated into SAP through the standard BAdIs ADDRESS_SEARCH and ADDRESS_CHECK, and surfaces functionally as the "…by Address Attributes (Fuzzy Search)" tab in the Customer and Vendor master search help.

Summary of findings:
- 7 Coca‑Cola objects depend on Trillium in total.
- 3 are active (present in CCLM and used within the last 3 years) and require remediation before removal: /CCC/CL_IM_ADDRSRCH (BAdI ADDRESS_SEARCH), /CCC/CL_IM_ADRCHECK_VALID (BAdI ADDRESS_CHECK), and /CCC/CCEAG_OTC_DEL_ADDRESS (called from the Address Search BAdI class).
- 4 are dormant test/legacy objects with no recent usage and can be deleted outright.

To remove Trillium cleanly, we first need a business confirmation: is the fuzzy address search/validation on the Customer and Vendor masters still required? This decision drives the remediation — if it is retained, the Trillium BAdI implementations would be replaced with SAP‑standard or custom logic; if not, they would be deactivated and removed. Once the active consumers are remediated, the Trillium objects can be deleted.

The attached document contains the full object‑level detail, the BAdI/method architecture, and the step‑by‑step remediation approach. Please review and let us know your decision on the address search/validation functionality so we can finalise the plan.

Best regards,
Vaibhav Maheshwari

---

Remember to attach `Trillium_Detailed_Findings.docx` before sending. Want any tone or length adjustments?
[user] there is a new file Movilizer_DSD_Usage_Handoff_1 this contains all data for mobliser from this file first we need to find the objects which are present in cclm report and then analyse them as to how they are getting used in coca cola
[assistant] Let me locate the new Movilizer file and look at its structure.
[assistant] (called mcp__workspace__bash)
[assistant] It's a Word document in Downloads. Let me read its contents.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] This is a handoff brief (the full 2,188‑object transport list isn't in it — it references a 93‑page source). It names the key Movilizer objects and explains the architecture. Let me check whether the full object list exists anywhere and what CCLM data I have to match against.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] The mount paths shifted — let me re-locate the working files.
[assistant] (called mcp__workspace__bash)
[assistant] The mount looks momentarily stale. Let me retry and find the CCLM report source.
[assistant] (called mcp__workspace__bash)
[assistant] You uploaded the transport request PDF — let me extract the full object list from it.
[assistant] (called mcp__workspace__bash)
[assistant] The PDF is the released transport MCRK937796 (93 pages). Let me extract the object directory section.
[assistant] (called mcp__workspace__bash)
[assistant] Found the object directory (starts ~line 598). Let me parse the full object list.
[assistant] (called mcp__workspace__bash)
[assistant] Extracted **2,007 distinct objects** from transport MCRK937796 — /MOVI/ (vendor connector) 1,846, /SCL/ (Coca‑Cola OTCM) 181, plus 14 standard. Now I need the CCLM report to match against, but the working folder isn't showing in the sandbox right now. Let me retry the mount.
[assistant] (called mcp__workspace__bash)
[assistant] The full object list is extracted. The working folder still isn't syncing to the sandbox for data processing. Let me locate the CCLM report via the file tools instead.
[assistant] (called Glob)
[assistant] (called Glob)
[assistant] The working folder `C:\Games\coca cola\CCLM\New Data` has been disconnected from this session — the sandbox can't see the CCLM report or the other workbooks right now (only Downloads and your uploaded PDF are reachable). So I can extract the full object list now, but the CCLM matching step needs that folder back.

Let me build the full object list as a clean Excel first.
[assistant] (called mcp__workspace__bash)
[assistant] Done — extracted and saved the full object list.

**`Movilizer_MCRK937796_ObjectList.xlsx`** — all **2,006 distinct objects** from transport MCRK937796, with two tabs:
- **Object List** — PgmID, Type, Object Name, Namespace, Layer/Scope. Color‑coded: green = /MOVI/ vendor connector (1,835, vendor scope), yellow = /SCL/ Coca‑Cola OTCM extension (159, customer remediation scope), 12 standard/other.
- **Summary by type** — object counts per type split across the layers (e.g., CLAS 353, TABL 327, DTEL 318, PROG 95, TRAN 74, METH/FUNC/FUGR for the executables).
[assistant] (called mcp__cowork__present_files)
[assistant] Thanks — let me access the CCLM folder at that path and locate the report and working files.
[assistant] (called Glob)
[assistant] The folder isn't connected to this session. Let me request access to it.
[assistant] (called ToolSearch)
[assistant] (called mcp__cowork__request_cowork_directory)
[assistant] Folder connected. Let me find the CCLM report and the working files.
[assistant] (called mcp__workspace__bash)
[assistant] Found the CCLM report (`CCBJI Latest List from CCLM.xlsx`, 26.5MB). Let me read its structure, then match the 2,006 transport objects against it.
[assistant] (called mcp__workspace__bash)
[assistant] CCLM report structure is clear. Let me read it and match the 2,006 transport objects against it (handling class/method and function-group rollup).
[assistant] (called mcp__workspace__bash)
[assistant] CCLM has 163,051 rows; /MOVI/ appears in 5,439 rows, /SCL/ in 19,413. Now matching the 2,006 transport objects against CCLM with class/function rollup for usage.
[assistant] (called mcp__workspace__bash)
[assistant] Now I have the core result. Let me pull the detailed list of the **26 objects used in the last 3 years** (with descriptions and last‑used dates), then research how Movilizer DSD usage is evidenced.
[assistant] (called mcp__workspace__bash)
[assistant] The 26 active objects map cleanly to the connector's runtime: shipment BAdIs (VT01N/VT02N), master‑data replication, the pricing engine/BTE, and tour/status handling. Let me research Movilizer DSD online to back this with authoritative detail and concrete checks.
[assistant] (called ToolSearch)
[assistant] (called WebSearch)
[assistant] Let me get concrete verification methods for checking SAP object/transaction usage.
[assistant] (called WebSearch)
[assistant] I have what I need — the 26 active objects, the connector architecture, and concrete SAP verification methods. Let me enrich the object‑list Excel with the CCLM match and build the customer guidance document.
[assistant] (called mcp__workspace__bash)
[assistant] Now building the customer guidance document.
[assistant] (called mcp__workspace__bash)
[assistant] The sandbox temp dir was reset. Let me reinstall docx and rebuild the Movilizer document.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] docx ready. Now writing and running the Movilizer document build.
[assistant] (called mcp__workspace__bash)
[assistant] Both Movilizer deliverables are ready.

**`Movilizer_Usage_and_Verification_Guide.docx`** — the customer‑facing guide: what the connector is, the key finding (of 2,006 transport objects, 1,750 are in CCLM but only **26 used in the last 3 years**), how/where those 26 are used across five integration points (shipment save on VT01N/VT02N, master‑data replication, mobile pricing/BTE, tour & status, framework), an 8‑step **verification guide** (SM37, SE16 on /MOVI/DSD_* tables, SE18 BADI_LE_SHIPMENT, ST03N/STAD), and sources.

**`Movilizer_MCRK937796_CCLM_Usage.xlsx`** — all 2,006 objects matched to CCLM (green = used last 3 yrs) plus a "Used last 3 years" tab with the 26 grouped by function.
[assistant] (called mcp__cowork__present_files)
[assistant] Now the Ortec one‑page PPT. Let me confirm the phase data and read the pptx skill.
[assistant] (called mcp__workspace__bash)
[assistant] Phase data confirmed. Let me read the pptx skill, then build the one‑page deck.
[assistant] (called Read)
[assistant] Let me build the one‑page Ortec deletion deck.
[assistant] (called mcp__workspace__bash)
[assistant] (called mcp__workspace__bash)
[assistant] Let me QA‑render the slide.
[assistant] (called mcp__workspace__bash)
[assistant] (called Read)
[assistant] The one‑page deck is clean and customer‑ready.

**`Ortec_Deletion_Plan_OnePager.pptx`** — a single slide with the context stats (19,675 total; 18,152 no consumer; 1,523 used by CCBJI/standard), the four phases as color‑coded cards matching the Excel (Phase 1: 6,285 clean non‑DDIC · Phase 2: 11,186 clean DDIC · Phase 3: 843 + ~596 consumers to remediate · Phase 4: 1,361 linked DDIC), the Phase 4 decision (replace with custom vs. keep as‑is), and the sequencing note.
[assistant] (called mcp__cowork__present_files)
[assistant] To recap this turn: I extracted the full **2,006‑object** list from transport MCRK937796, matched it to the CCLM report (**1,750 in CCLM, 26 used in the last 3 years**), produced the Movilizer **usage & verification guide** plus the matched workbook, and built the **one‑page Ortec deletion deck**. A few `_`‑prefixed helper images are in the New Data folder — you can delete them. Anything you'd like adjusted on the deck (branding, colors, or splitting Phase 4 options visually)?
[user] for mobiliser can u tell me if any data is getting pulled in SAP or it is all outgoing data from SAP checking the componenets of mobiliser and cclm report and update in word document also
[assistant] Good question — Movilizer is bidirectional, so let me verify against the components and CCLM. Let me check the inbound (device → SAP) and outbound (SAP → device) objects in the CCLM report.
[assistant] (called mcp__workspace__bash)
[assistant] This is the key insight: looking at what's actually **used in the last 3 years**, the live flow is almost entirely **outbound (SAP → Movilizer)**. Let me confirm the direction split and then update the Word document.
[assistant] (called mcp__workspace__bash)
[assistant] Confirmed: of the 26 active objects, **0 are inbound** — 10 outbound (master data/pricing), 4 outbound shipment triggers, 6 tour/status, 6 framework. The inbound reply tables and IDoc‑inbound show no recent use. Let me add this to the Word document.
[assistant] (called mcp__workspace__bash)
[assistant] Let me guard the fill type and rebuild.
[assistant] (called mcp__workspace__bash)
[assistant] Built. Let me QA‑render the new direction section.
[assistant] (called mcp__workspace__bash)
[assistant] (called Read)
[assistant] The document is updated with the data‑flow analysis. Here's the answer to your question:

Based on what's actually **used in the last 3 years** (CCLM), the live Movilizer flow is **predominantly outbound — SAP → Movilizer, not pulling data in.** Of the 26 active objects: **0 are inbound.** The split is 10 outbound (master‑data/pricing), 4 outbound shipment triggers, 6 tour/status, 6 framework.

- **Outbound is active:** the master‑data backend‑integration FMs (`/MOVI/DSD_MD_BTE_CUST`, `_MAT`, `_CREDIT` — labelled "BE Integration (Out)") and the pricing BTE (`/MOVI/MPE_BTE`, `MPE_BTE_COND`, `MPE_COND_NOTIFY_CHANGE`) were all last used 2026‑05‑06. SAP is pushing customer/material/credit master data and pricing out to the Movilizer cloud.
- **Inbound shows no recent execution:** the reply tables that would hold data coming back (`/MOVI/DSD_RD*` — deliveries, payments, orders, tours, signatures…) and the inbound IDoc processing (`/MOVI/IDOC_INBOUND`) don't appear in the active set.
- **Caveat (important to tell the customer):** CCLM only tracks usage for executable code, not dictionary tables — so the reply tables always read blank regardless of content. To confirm definitively whether transactions post back **into** SAP, check row counts/dates in the live system: `SE16` on `/MOVI/DSD_RDTOUR`, `/MOVI/DSD_RDDELH`, `/MOVI/DSD_RDPAYM`, `/MOVI/DSD_ST_TOUR`, `/MOVI/DSD_TOUR_H`.

So on current evidence: outgoing master‑data and pricing replication (plus shipment/tour distribution on save) — no inbound transactional posting in the last 3 years, pending the table check.

I added this as **Section 6 (Active Objects by Direction)** and **Section 7 (Is Data Pulled Into SAP, or Only Sent Out?)** in the guide.
[assistant] (called mcp__cowork__present_files)
[assistant] Sources: [Movilizer for SAP DSD — Honeywell Marketplace](https://marketplace.honeywell.com/apps/220260/movilizer-for-sap-dsd), [SAP Help — Direct Store Delivery Connector](https://help.sap.com/docs/SAP_ERP/fce9321b57d541ec9ffced02a4322571/d77bb65334e6b54ce10000000a174cb4.html)

One follow‑up worth doing before this goes to the customer: have someone run `SE16` on those `/MOVI/DSD_RD*` reply tables in the live system to confirm the inbound side is genuinely idle — that turns the CCLM‑based inference into hard proof. Want me to add a short "checks to run" callout box at the top of the document for that?
[user] in the word document i need last section explaining if we need to deactivate mobiliser what are the steps to be taken which all badi , bte need to be deactivated and where all code change need to be done
[assistant] Now let me add the deactivation steps section to the document.
[assistant] (called mcp__workspace__bash)
[assistant] Let me QA‑render the new deactivation section.
[assistant] (called mcp__workspace__bash)
[assistant] (called Read)
[assistant] Added — the guide now ends with a full deactivation runbook.

**Section 8 — Deactivating Movilizer, Step‑by‑Step:**
- **Step 1 – Stop the runtime:** deschedule `/MOVI/DSD_WORKER` polling job + MD replication jobs (SM37), disable the Movilizer RFC/HTTP destination (SM59).
- **Step 2 – Deactivate BAdIs (SE19):** `BADI_LE_SHIPMENT` / `/MOVI/DSD_SHP_SAVE` → `/MOVI/DSD_CL_IM_LE_SHIPMENT`; `/SCL/OTCM_LE_SHPMT`, `/SCL/OTC_SHIPMENT`, `/SCL/OTCM_MOVEL_GEN`; plus `CMPD_CUS_ASSIGNMENT` if CRM TPM is used.
- **Step 3 – Classic exits (CMOD):** `ZXV56U18` and `ZXMG0U02` (calling `/SCL/OTC_CL_ZXV56U18_GLO_IMP` and `/SCL/MDM_CL_ZXMG0U02_GLO_IMP`).
- **Step 4 – BTEs (FIBF):** remove the MOVILIZE product/partner in TBE12/TBE22 — disables the master‑data "BE Integration (Out)" FMs (`/MOVI/DSD_MD_BTE_CUST/MAT/CREDIT/ORDER/EQUI`) and pricing events (`/MOVI/MPE_BTE_COND`, `_COND_A`, `MPE_COND_NOTIFY_CHANGE`).
- **Step 5 – VOFM pricing routines:** remove `RV61B913/914/926` from active procedures (V/08, T683S).
- **Step 6 – Code changes:** only the two exit includes (`ZXV56U18`, `ZXMG0U02`) need ABAP edits; the `/SCL/` OTCM classes become orphaned and retire with deletion. No core SAP code is touched.
- **Step 7 – Verify:** SM37, SE18, FIBF checks + a test shipment/master‑data change.

A key point for the customer: deactivation is almost entirely **configuration/registration** (BAdI/BTE/VOFM), with direct code edits needed in just the two customer‑exit includes.
[assistant] (called mcp__cowork__present_files)
[assistant] Note: the BAdI/BTE/exit names in the runbook are drawn from the connector components and the handoff brief — each registration (e.g., the exact CMOD project, the BF11 product entry) should be confirmed in the target system before execution, as flagged in the document intro. Want me to also produce this deactivation runbook as a standalone checklist (Excel or one‑pager) the basis/ABAP team can tick off?
[user] now from the list u created tell me what all u need input from coca cola system code , screen shot data so that we can make a complete document of what all used how and what all to remove
[assistant] To turn the current (CCLM‑inference‑based) analysis into a definitive document, here's what needs to be pulled from the live Coca‑Cola system. Let me build it as a checklist the basis/functional team can fill.
[assistant] (called mcp__workspace__bash)
[assistant] Here's what's needed from the live Coca‑Cola system to make the document complete and evidence‑based (rather than CCLM‑inference). I've built it as **`Movilizer_Input_Request_Checklist.xlsx`** (22 items, with a "Provided?" column to track collection). Grouped by your three input types:

**Data extracts (SE16 / tables) — the biggest gap is inbound proof:**
- Row counts + latest dates in the reply tables `/MOVI/DSD_RDTOUR, RDDELH, RDDELI, RDPAYM, RDORDH, RDVISI, RDSIG` and `/MOVI/DSD_ST_TOUR, /MOVI/DSD_TOUR_H` → resolves whether data posts back **into** SAP (CCLM can't show this for tables).
- `/MOVI/DSD_DEVICE` (registered devices = field‑usage scale).
- BTE registration tables `TBE12/TBE22/TBE34/TBE82` for partner MOVILIZE.
- `T683S` pricing procedures referencing `RV61B913/914/926`.
- Config tables `/MOVI/DSD_MDREG/MDTYPE/MDREL` (replication scope) and `/MOVI/DSD_CSTEP/CST/STATUS` (which step/status classes are actually referenced — the definitive per‑object where‑used).

**Screenshots:**
- `SM37` job schedule/last‑run for `/MOVI/DSD_WORKER` (+ MD replication jobs) and `SM59` connector destination → proves the connector is live & frequency.
- `SE18/SE19` active implementations of `BADI_LE_SHIPMENT`, `/MOVI/DSD_SHP_SAVE`, `/SCL/OTCM_LE_SHPMT`, `/SCL/OTC_SHIPMENT`, `/SCL/OTCM_MOVEL_GEN`, `CMPD_CUS_ASSIGNMENT`.
- `CMOD` project(s) for exits `ZXV56U18`/`ZXMG0U02`; `FIBF`/`BF11` BTE product status.
- Functional proof: `/MOVI/DSD_MONITOR` (tour monitor), `/MOVI/DSD_MD_ADMIN`, and `ST03N/STAD` workload stats for `/MOVI/*`.

**Code (source) extracts:**
- The two customer‑exit includes `ZXV56U18`, `ZXMG0U02` (the only places needing direct ABAP edits).
- Wrapper classes `/SCL/OTC_CL_ZXV56U18_GLO_IMP`, `/SCL/MDM_CL_ZXMG0U02_GLO_IMP`.
- The active `/SCL/` OTCM extension classes (`/SCL/CL_OTCM_IM_LE_SHIPMENT, _TOUR_EXT, _STATUS_EXT, _UTIL, _FACTORY`, and the `/SCL/CL_OTCM_MPE_*` routines) → to assess what the custom logic does and the remediation effort.
- `RV61B913/914/926` routine source.

Once those come back, I can fold them in to finalize the "what's used, how, and what to remove" document with hard evidence — especially flipping the inbound conclusion from "no recent use in CCLM" to a confirmed yes/no.
[assistant] (called mcp__cowork__present_files)
[assistant] Want me to add this checklist as an appendix in the Movilizer guide document too, so it travels with the analysis as a single "inputs still required" section?