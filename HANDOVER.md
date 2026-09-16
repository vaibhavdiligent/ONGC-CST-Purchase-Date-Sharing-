# HANDOVER — ONGC Videsh DPR Analytical RAP (package ZPR_DPR_RAP)

Purpose of this file: give a fresh Claude session everything it needs to continue
this work without re-deriving it. Read fully before touching anything.
Last updated: 2026-09-15.

---

## 1. Who / what / where

- **User**: Vaibhav Maheshwari (SAP functional consultant, Diligent Consulting) —
  works for the customer ONGC Videsh. Communicates tersely, often via screenshots
  of SAP error logs. Expects concrete fixes, not options; gets frustrated by
  repeated error rounds — always audit the *whole* package against a newly
  discovered rule, never patch just the one line that was reported.
- **Repo**: `vaibhavdiligent/ONGC-CST-Purchase-Date-Sharing-`
- **Working branch**: `claude/eager-euler-dpm9rf` — ALL work goes here. Never
  push to other branches. `main` carries the user's own "Add files via upload"
  commits (Excel/PDF samples, table dumps); the branch is not kept in sync with
  main — that is fine.
- **Sources**: `src/rap/` (RAP/CDS objects), `src/zpra_dpr_report.prog.abap`
  (classic DPR report, v3.0), `src/zpra_dpr_report_s4.prog.abap` (abap2xlsx
  variant, v5.7). The two reports are DONE and already in the customer system.
- **Deliverables folder**: `deploy/` on the branch — the abapGit import ZIP and
  four Word guides. Update these whenever sources change; the user downloads from
  GitHub because chat attachments sometimes fail for him.
- **Git attribution** (append to every commit message):
  `Co-Authored-By: Claude Fable 5.1 <noreply@anthropic.com>` and
  `Claude-Session: https://claude.ai/code/session_01Xu9y1woHSkPxJdZRFiKhnN`
  (use whatever the current session's reminder says). No model names inside
  pushed artifacts.

## 2. Business context (do not re-ask)

- DPR = Daily Production Report of ONGC Videsh's overseas oil & gas assets.
  Excel tab 2 = graph "Actual Production vs BE Target BOEPD" over dates;
  tab 3 = "Production Performance" (YTD/Annual actual vs BE target per oil/gas).
- Product codes: `722000001` Oil, `722000003` Condensate, `722000004` Gas,
  `722000005` LNG.
- Gas to MMSCMD: unit MCM as-is, MCF ÷ 35.3, M3 ÷ 1,000,000. BOE factor 6290
  (gas MMSCMD × 6290 = BOEPD). Oil family quantities are BOPD.
- **Gas has NO `NET_PROD` rows in `ZPRA_T_DLY_PRD`** (verified on a full 450k-row
  dump): net gas = `GROSS_PROD` − `GAS_INJ`. Oil family uses `NET_PROD`.
- Fiscal year April–March: `gjahr` = start year; `monat` = month−3 (Apr..Dec) or
  month+9 (Jan..Mar). Implemented as `dats_add_months(date, -3)`.
- Business Unit from asset-code prefix: RUS→BU-RUSSIA; BRA/COL/VEN→BU-LAC;
  MMR/VNM→BU-ASIA PACIFIC; AZE/SSU/SUD/UAE→BU-MENA CIS; anything else → OTHER
  (test junk like A001, PCB, SCB, VANKOR — filtered out in queries).
- Tables (customer's, on-prem S/4 with IS-Oil/PRA): `ZPRA_T_DLY_PRD` (daily),
  `ZPRA_T_MREC_PRD` (monthly reconciled), `ZPRA_T_PRD_TAR` (targets; column is
  `tar_qty` — `tar_qty2` does NOT exist, it was program-computed), `ZPRA_T_PRD_PI`
  (participating interest), `ZOIU_PR_DN` (asset texts; `dn_de` has conversion
  exit OIUNM), `ZPRA_C_DPR_PROF` (config view).
- **BE target calculation (fixed 2026-09-16, was the "target line = 0" bug)**:
  `tar_qty` in `ZPRA_T_PRD_TAR` is a MONTHLY figure in MMT (oil family) / BCM
  (gas); gas targets are NOT stored as `NET_PROD`. The classic report
  (`convert_target_units`, `fill_dynamic_table_sec2b`, `fill_dynamic_table_sec5a`)
  takes all TAR_BE rows of the fiscal year, volume types NET_PROD + GROSS_PROD +
  GAS_INJ, scales oil × 1e6 × conv_factor (`ZPRA_T_TAR_CF` by gjahr/asset/block/
  product; missing CF → 0) and gas × 1000 (→ MMSCM) × 6290 (BOE), sums the year
  and divides by the days in the FY (365/366). That flat annual BOEPD is the
  "BE Target" line. Targets are OVL level (JV conversion only when p_c_jv, we
  use OVL for both lines). Implemented in ZDPR_P_TARGET_ROW → ZDPR_I_TARGET_FY →
  ZDPR_P_TARGET_DAY → ZDPR_P_BOEPD_ROWS (union with actuals) → ZDPR_C_BOEPD_DAY.
- Open customer questions for the future "Production Dashboard" (not started):
  source of annual goals 7.252 MMT oil / 2.574 BCM gas / 9.826 MMTOE, gas→ToE
  factor, remarks process, OVP vs ALP choice, JV vs OVL level per block.

## 3. Object inventory (all in package ZPR_DPR_RAP, all names ZDPR_*)

| Object | Type | Notes |
|---|---|---|
| ZDPR_I_DAILY / ZDPR_I_MONTHLY / ZDPR_I_TARGET | view entities | interface views on the tables (I_TARGET unchanged since first activation - keep it so; abapGit failed when dependents were added to a changed view) |
| ZDPR_P_TARGET_ROW | view entity | target rows from the tables, joins ZPRA_T_TAR_CF: TargetVolume (bbl/MMSCM), TargetBoe, FiscalYearStart, DaysInFiscalYear |
| ZDPR_I_TARGET_FY | view entity (group by) | annual target per TargetCode/FY/asset/block/product, all months + NET_PROD/GROSS_PROD/GAS_INJ |
| ZDPR_P_DAY_BASE | view entity | base layer: unit conversion, signed gas (GAS_INJ negative), BU, fiscal year/period, PI% |
| ZDPR_P_DATE_SPINE | view entity (group by) | distinct production dates + FY/period |
| ZDPR_P_TARGET_DAY | view entity | date spine × ZDPR_I_TARGET_FY (TAR_BE): TargetQty/TargetBoepd = annual ÷ DaysInFiscalYear |
| ZDPR_P_BOEPD_ROWS | view entity (union) | RowType 'A' actual rows (DAY_BASE) + 'T' target rows (TARGET_DAY), identical casts |
| ZDPR_C_PROD_CUBE | cube | daily production cube (PI% = #MAX measure) |
| ZDPR_C_BOEPD_DAY | cube | plain select on ZDPR_P_BOEPD_ROWS (no join): SUM per date = Actual line + flat BE Target line |
| ZDPR_C_TARGET_CUBE | cube (param P_TargetCode) | monthly actual vs target — the join lives HERE because queries may not join |
| ZDPR_P_PERF_AGG | view entity (union) | YTD + ANNUAL aggregates for tab 3 |
| ZDPR_Q_PROD_PERF | **classic `define view`**, `@OData.publish: true` (also in the V4 SRVD) | tab-3 query (ratios via division()); OData V2 service ZDPR_Q_PROD_PERF_CDS for the Overview Page |
| ZDPR_Q_BOEPD_TREND, ZDPR_Q_DAILY_TREND, ZDPR_Q_PROD_QUERY, ZDPR_Q_TARGET_QUERY | **classic `define view`**, `@Analytics.query: true`, `@OData.publish: true` | analytical queries → auto-generated OData V2 services `ZDPR_Q_*_CDS` |
| ZDPR_A_PROD_PARAM, ZDPR_A_TAR_PARAM, ZDPR_A_EXCEL_RESULT, ZDPR_A_PDF_RESULT | abstract entities | action parameters/results |
| ZDPR_I_EXCEL_DL | root view + BDEF (unmanaged, action-only) | hosts 4 static download actions |
| ZBP_ZDPR_EXCEL_DL, ZCL_ZDPR_EXCEL (abap2xlsx), ZCL_ZDPR_PDF | classes | handler + Excel/PDF generation |
| ZDPR_SD_ANALYTICS | service definition | V4: PROD_PERF, both cubes, I-views, EXCEL_DL (NOT the analytical queries) |
| ZDPR_SB_ANALYTICS_O4 | service binding (manual in ADT) | OData V4 – UI |
| ZDPR_Q_*.ddlx (5) | metadata extensions (manual in ADT) | minimal UI annotations; sources in `src/rap/*.ddlx.asddlx`. 2026-09-16: BOEPD/DAILY charts use `ProductionDateText`, PROD_PERF line items ordered RowLabel/ActualPerDay/AchievementPct (criticality) for the 3-column table card |

Old ZPRA_* names were abandoned on the user's instruction ("make everything new");
do not reintroduce them.

## 4. Current status (as of last message)

- **Direction change 2026-09-15: ONE dashboard, not four apps.** Target is a Fiori
  Overview Page (OVP) app with cards per query, all on OData V2 (hence
  ZDPR_Q_PROD_PERF also published as V2). Design + manifest/card snippets are in
  `deploy/ZDPR_RAP_Single_Dashboard_OVP.docx`. OVP cards bind to the `...Results`
  entity set and get parameters via `UI.SelectionVariant#Params`; global filter
  entity type = `ZDPR_Q_PROD_PERFType`.

- All CDS objects **active** in the customer's system (imported via
  ZABAPGIT_STANDALONE offline ZIP into ZPR_DPR_RAP, transport OCQK901644).
- The four analytical queries are on OData V2; the service binding wizard for V4
  had rejected `@Analytics.query` ("not supported") on this release.
- Developer is now in **Business Application Studio** running the SAP Fiori
  generator against `ZDPR_Q_TARGET_QUERY_CDS`. Last advice: choose the
  **parameter entity set** `ZDPR_Q_TARGET_QUERY` (not `…Results`, not the UoM
  value-help sets); fallback if the filter bar shows no parameters is
  `entitySet = ZDPR_Q_TARGET_QUERYResults` in manifest.json.
- Not yet confirmed done by the user: service binding published, V2 services
  registered in `/IWFND/MAINT_SERVICE`, the 5 DDLX created in ADT (abapGit could
  not import DDLX on this system — "Malformed annotate statement" regardless of
  content — so they are created manually from the sources).

## 5. Release-specific rules learned the hard way (apply to ANY new CDS)

The customer system is an older S/4 (supports `define view entity`, but has an
old annotation registry and strict checks). Every item below cost a round trip:

**CDS in general**
- No `year()` / `month()` functions → use `substring(date,1,4)` / `dats_add_months`.
- `@EndUserText.label` max **40** chars.
- Parameters: NOT `tabname-field`, NOT built-in `abap.dats` for OData exposure →
  use data elements (`datum`, `gjahr`, `char20`, `char10`).
- No `@Semantics.unitOfMeasure: true` in view entities; `@Semantics.calendar.date`,
  `@MappingRole`, `@Analytics.settings.maxResultSize` unknown → don't use.
- Key fields must be contiguous at the top of the select list.
- Union branches: identical types (`cast('YTD' as abap.char(6))` vs 'ANNUAL').
- No CASE in GROUP BY or inside SUM() → precompute in a lower view.
- Literal casts: 0/1 need `abap.dec(3,0)` (INT1 is 3 digits) or a warning.
- Casting a field to its own type is optimised away (conversion exit survives) →
  use `rtrim(field, ' ')` to strip the OIUNM conversion exit.

**Classic `define view` (needed for `@OData.publish`) is stricter still**
- `/` only on floats → `division(a,b,dec)` or cast to `abap.fltp`.
- WHERE may only contain `field op $parameters.X`; no `$parameters.X = ''` tricks.
- Needs `@AbapCatalog.sqlViewName` (≤16 chars).

**Analytical engine**
- Cube measures: only **SUM / MIN / MAX**. No AVG, NOP, NONE, FORMULA in a cube.
- Every DEC element in a cube must be a measure ("dec not allowed for characteristic").
- A query selects from exactly ONE cube, no joins, no `key`, no expressions except
  `@Aggregation.default: #FORMULA` elements (e.g. Achievement % =
  fltp(ActualQty)*100/fltp(TargetQty)).
- OData V4 aggregation supports only SUM/MIN/MAX/AVG/COUNT_DISTINCT (NOP/FORMULA
  rejected there).
- Analytical queries cannot be in a V4 service definition on this release →
  `@OData.publish: true` on classic views, register in `/IWFND/MAINT_SERVICE`.

**Other objects**
- SRVD: no comments allowed in source.
- BDEF: `etag none` unsupported; `managed` needs persistence → action-only
  behavior is `unmanaged implementation in class ... unique;` with no CRUD.
- DDLX: could not be imported via abapGit on this system → create in ADT; base
  views need `@Metadata.allowExtensions: true`; entity-level UI annotations go
  ABOVE `annotate view`, each element once; `@Metadata.layer: #CUSTOMER`.
- abapGit: repo is offline; user pulls ZIPs; objects not in the ZIP are proposed
  for DELETION on pull — tell the user to untick the service binding and DDLX.

## 6. How to rebuild deliverables

ZIP builder is in git: `python3 tools/build_abapgit_zip.py` (add `--with-ddlx` to
include metadata extensions) writes `deploy/ZPR_DPR_RAP_abapgit.zip`. The docx
generators live in `tools/` too where available; if missing, recreate them.
- `build_abapgit_zip.py` — builds `ZPR_DPR_RAP_abapgit.zip` from `src/rap/` with
  generated abapGit sidecar XMLs (formats copied from SAP-samples/abap-platform-rap100).
  `DDLS` list must include all 18 views; `DDLX = []` for the "no-ddlx" variant
  that is what the user actually pulls (DDLX are manual). Classic views get no
  `SOURCE_TYPE`; view entities get `W`. BDEF file is named after the root entity
  (`zdpr_i_excel_dl.bdef.asbdef`); ZBP class is split into main + `locals_imp`;
  SRVD source uses extension `.srvd.srvdsrv`; service binding is excluded.
- `make_manual_doc.js`, `make_btp_doc.js`, `make_fiori_walkthrough.js` — docx via
  the `docx` npm package (`npm install docx` in scratchpad; validate with the docx
  skill's `validate.py`; LibreOffice rendering is broken in the container, so
  verify by extracting text). Paragraph borders: left only (docx-js emits
  bottom-before-left, which violates OOXML order).
- After any source change: commit + push to the branch, rebuild ZIP, copy ZIP and
  changed docs into `deploy/`, commit + push again, and also SendUserFile.

## 7. Working agreements / pitfalls

- Container resets happen; the local clone may then come from `main`. Before
  pushing, check `git rev-list --count origin/main..HEAD` — if local has no unique
  commits, `git checkout -B claude/eager-euler-dpm9rf origin/claude/eager-euler-dpm9rf`
  and push as a fast-forward. Never force-push without explicit user approval.
- Cannot compile CDS here. When a new system rule appears, write a small Python
  audit over ALL `src/rap/*.asddls` for that rule, fix everything it finds, and
  say so — the user explicitly asked that "this kind of issue should not come".
- Verify SAP facts online (WebSearch/WebFetch on help.sap.com / SAP-docs GitHub
  raw markdown; community.sap.com returns 403) before asserting annotation
  semantics — the user asked "check online for correct annotation".
- Prefer answering from the docs in `deploy/`: `ZDPR_RAP_Manual_ADT_Objects.docx`
  (manual objects + on-prem transport + go-live), `ZDPR_RAP_BTP_Deployment_Guide.docx`
  (architecture: ABAP stays on-prem, BTP hosts UI via Cloud Connector; options A
  Fiori/Work Zone, B SAC, C Steampunk not advisable), `ZDPR_RAP_Fiori_Generator_Walkthrough.docx`
  (Step 0–5 with checks).
- Security note to keep repeating when relevant: views use
  `@AccessControl.authorizationCheck: #NOT_REQUIRED` → no row-level restriction.

## 7b. Fiori Overview Page limits (2026-09-16)

- The real OVP does not look like the first mockup: table/list cards show max
  3 columns x 3 rows, charts use theme colours, DATS shows as YYYYMMDD, KPI
  header only on aggregatable entity sets. Verified rules, mockup-vs-reality
  table, manifest (resizable layout, defaultSpan, two performance cards YTD /
  Annual via SelectionVariant on ScopeType) and options (custom card, ALP, SAC)
  are in `deploy/ZDPR_RAP_Fiori_OVP_Limits_and_Layout.docx`; realistic mockup
  `deploy/DPR_Dashboard_Fiori_Realistic.{html,png}` (tools/make_fiori_limits_doc.js).
- Backend additions for it: `ProductionDateText` (YYYY-MM-DD) in ZDPR_C_BOEPD_DAY,
  ZDPR_C_PROD_CUBE, ZDPR_Q_BOEPD_TREND, ZDPR_Q_DAILY_TREND; `RowLabel` in
  ZDPR_Q_PROD_PERF.
- abapGit pull error seen by the user after the target fix: "column
  FISCALYEARSTART/DAYSINFISCALYEAR/TARGETVOLUME/TARGETBOE unknown" in
  ZDPR_I_TARGET_FY / P_TARGET_DAY / P_BOEPD_ROWS = the new ZDPR_I_TARGET was not
  activated first; a second pull gave the same list. Resolved by decoupling:
  ZDPR_I_TARGET restored to its active version, scaling moved to the NEW view
  ZDPR_P_TARGET_ROW (no dependent of a changed object any more). Lesson: on
  this system add columns in new objects rather than extending a view that
  other new views read in the same pull.

## 8. Likely next requests

1. Fiori generator / preview problems (parameters, chart missing → DDLX not
   active, empty service list → destination `WebIDE*` properties).
2. Registering V2 services, publishing the V4 binding, Work Zone tile setup.
3. Validating figures against the DPR Excel. The target scaling now mirrors the
   classic report (see section 2). Remaining possible deviations: classic uses
   the CF of the *report* year for every target row (we use the target's own
   gjahr — same thing for the current FY); per-asset target start date
   (`get_target_start_date`) only affects per-asset columns, not the grand
   total used by the graph; ZDPR_C_TARGET_CUBE / ZDPR_Q_TARGET_QUERY still
   compare raw monthly `tar_qty` with monthly reconciled actuals.
4. Transport to QA/PRD (dependencies: ZPRA_T_* tables, ZOIU_PR_DN,
   ZPRA_C_DPR_PROF, abap2xlsx, optional Adobe forms ZDPR_FRM_PRODUCTION/TARGETS).
5. The Production Dashboard (BU-grouped rates vs targets, asking rate, remarks)
   once the customer answers the open questions in section 2.
