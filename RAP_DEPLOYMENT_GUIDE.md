# DPR Analytical RAP — Deployment Guide

End-to-end instructions for deploying the Analytical RAP version of `ZPRA_DPR_REPORT` and consuming it as a Fiori Elements **Analytical List Page** (ALP) with on-screen charts plus dedicated **Excel** and **PDF** download buttons.

---

## 1. What you are deploying

| Layer | Object | Purpose |
|-------|--------|---------|
| Tables (existing) | `ZPRA_T_DLY_PRD`, `ZPRA_T_MREC_PRD`, `ZPRA_T_MREC_APP`, `ZPRA_T_PRD_TAR`, `ZPRA_T_PRD_PI`, `ZOIU_PR_DN` | Same source tables used by the classical program |
| Interface CDS (FACT) | `ZPRA_I_DPR_DAILY`, `ZPRA_I_DPR_MONTHLY`, `ZPRA_I_DPR_TARGET` | Raw daily / monthly / target views |
| Analytical Cube | `ZPRA_C_DPR_CUBE` | `@Analytics.dataCategory: #CUBE` over daily data, joins assets + PI |
| Analytical Queries | `ZPRA_Q_DPR_PROD_QUERY`, `ZPRA_Q_DPR_TARGET_QUERY`, `ZPRA_Q_DPR_DAILY_TREND` | `@Analytics.query: true` — drive the on-screen ALP |
| UI Metadata Extensions | `ZPRA_Q_DPR_PROD_QUERY.ddlx`, `ZPRA_Q_DPR_TARGET_QUERY.ddlx`, `ZPRA_Q_DPR_DAILY_TREND.ddlx` | Selection bar, line items, **multi-chart** annotations, presentation variants |
| Action entity | `ZPRA_I_DPR_EXCEL_DL` | Lightweight singleton entity that hosts the static download actions |
| Action params | `ZPRA_A_DPR_PROD_PARAM`, `ZPRA_A_DPR_TAR_PARAM` | Input abstract entities for download actions |
| Action results | `ZPRA_A_DPR_EXCEL_RESULT`, `ZPRA_A_DPR_PDF_RESULT` | Output abstract entities (base64 + filename + MIME) |
| Behavior | `ZPRA_BP_DPR_EXCEL_DL.bdef` | Defines 4 static actions: `downloadProduction`, `downloadTargets`, `downloadPdfProduction`, `downloadPdfTargets` |
| Behavior impl | `ZBP_ZPRA_DPR_EXCEL_DL` (class) | Action handlers — one method per action |
| Excel generator | `ZCL_ZPRA_DPR_EXCEL` | Builds XLSX (binary `xstring`) |
| PDF generator | `ZCL_ZPRA_DPR_PDF` | Builds PDF via Adobe Document Services (Adobe Forms `ZPRA_FRM_DPR_PRODUCTION` and `ZPRA_FRM_DPR_TARGETS`) |
| Service definition | `ZPRA_SD_DPR_ANALYTICS` | Exposes all queries + cube + action entity |
| Service binding | `ZPRA_SB_DPR_ANALYTICS_O4` | OData V4 / UI |

After publish the OData V4 endpoint is:
```
/sap/opu/odata4/sap/zpra_dpr_analytics/srvd/sap/zpra_sd_dpr_analytics/0001/
```

---

## 2. Prerequisites

### System
- ABAP Platform **2022 or later** (required for `@Analytics.query` over a CDS cube and for Action entities). On-premise S/4HANA 2022+, or BTP ABAP Environment.
- ICF nodes active: `/sap/opu/odata4` (transaction `SICF`).
- Adobe Document Services configured (transaction `SFP`, `SOAMANAGER` connection `ADS_HTTP`) — required only for the PDF actions. If ADS is not available, see *Section 7 — PDF without ADS* for the Smart Forms fallback.

### Authorization
- `S_DEVELOP` to create/activate the objects.
- `S_RFCACL` and ADS service user for PDF.
- `S_SERVICE` for `ZPRA_DPR_ANALYTICS`.

### Tooling
- **ABAP Development Tools (ADT)** in Eclipse — Photon or later.
- (Optional) **abapGit** to import the `src/rap/` folder from the repository.
- Fiori Tools or Business Application Studio for the UI deployment (Section 5).

---

## 3. Importing the objects

### 3a. Via abapGit (recommended)
1. In ADT open the **abapGit Repositories** view.
2. **+ Online** → URL of this repository → branch `claude/zpra-dpr-program-VfvlH`.
3. Pick a transportable package, e.g. `ZPRA_DPR_RAP`.
4. **Pull**. abapGit imports everything under `src/rap/`.
5. Activate in this dependency order:

```
1. ZPRA_I_DPR_DAILY        (DDLS)
2. ZPRA_I_DPR_MONTHLY      (DDLS)
3. ZPRA_I_DPR_TARGET       (DDLS)
4. ZPRA_C_DPR_CUBE         (DDLS, @Analytics.dataCategory: #CUBE)
5. ZPRA_Q_DPR_PROD_QUERY   (DDLS, @Analytics.query: true)
6. ZPRA_Q_DPR_TARGET_QUERY (DDLS)
7. ZPRA_Q_DPR_DAILY_TREND  (DDLS)
8. ZPRA_Q_DPR_PROD_QUERY   (DDLX — UI annotations)
9. ZPRA_Q_DPR_TARGET_QUERY (DDLX)
10. ZPRA_Q_DPR_DAILY_TREND (DDLX)
11. ZPRA_A_DPR_PROD_PARAM   (abstract entity)
12. ZPRA_A_DPR_TAR_PARAM    (abstract entity)
13. ZPRA_A_DPR_EXCEL_RESULT (abstract entity)
14. ZPRA_A_DPR_PDF_RESULT   (abstract entity)
15. ZPRA_I_DPR_EXCEL_DL     (root view entity hosting the actions)
16. ZCL_ZPRA_DPR_EXCEL      (Excel generator class)
17. ZCL_ZPRA_DPR_PDF        (PDF  generator class)
18. ZPRA_BP_DPR_EXCEL_DL    (behavior definition)
19. ZBP_ZPRA_DPR_EXCEL_DL   (behavior implementation class)
20. ZPRA_SD_DPR_ANALYTICS   (service definition)
21. ZPRA_SB_DPR_ANALYTICS_O4 (service binding)  ← Publish
```

### 3b. Manual creation
If you cannot use abapGit, create each object in ADT in the same order using *New* → *Other ABAP Repository Object* → copy/paste the content from `src/rap/`.

### 3c. Activation troubleshooting
| Problem | Fix |
|---------|-----|
| `Unknown association ZPRA_T_PRD_PI` | Make sure `ZPRA_T_PRD_PI` table exists; if not, comment the PI join in `ZPRA_C_DPR_CUBE` and remove `OvlShareQty1/2` measures. |
| `@Analytics.query not allowed on view without @Analytics.dataCategory` | Activate the cube `ZPRA_C_DPR_CUBE` first. |
| `Function module FP_FUNCTION_MODULE_NAME exception` for PDF | The two Adobe forms must exist. Create them per Section 4. |

---

## 4. Creating the Adobe Forms (PDF download)

The PDF class calls two generated FMs from forms designed in transaction `SFP`. Until they are created, the PDF actions will short-dump.

### 4a. Form `ZPRA_FRM_DPR_PRODUCTION`
1. `SFP` → Form → **Create**: `ZPRA_FRM_DPR_PRODUCTION`.
2. Interface: create new (or pick existing) interface `ZPRA_IFC_DPR_PRODUCTION`:
   - Import params:
     ```
     IV_DATE_FROM   TYPE SY-DATUM
     IV_DATE_TO     TYPE SY-DATUM
     IT_DATA        TYPE ZCL_ZPRA_DPR_PDF=>TT_PROD_ROWS
     ```
3. In the form layout drag `IT_DATA` onto the body page → **Subform → Table** binding.
4. Add header text using `IV_DATE_FROM` / `IV_DATE_TO`.
5. Optional: add a chart subform (Adobe LiveCycle Designer → Insert → Chart → bind data set to `IT_DATA`).
6. Activate.

### 4b. Form `ZPRA_FRM_DPR_TARGETS`
Same procedure, interface params:
```
IV_FISCAL_YEAR  TYPE GJAHR
IV_TARGET_CODE  TYPE ZPRA_T_PRD_TAR-TAR_CODE
IT_DATA         TYPE ZCL_ZPRA_DPR_PDF=>TT_TARGET_ROWS
```

After both forms are activated, run `ZBP_ZPRA_DPR_EXCEL_DL`'s syntax check — it should activate cleanly because the FM names are resolved at runtime.

---

## 5. Publishing the OData V4 service

1. Open `ZPRA_SB_DPR_ANALYTICS_O4` (service binding) in ADT.
2. Click **Activate**, then **Publish Locally**.
3. The Service Binding editor lists each entity set; verify that the following appear:
   - `DPRProductionQuery` (analytical)
   - `DPRTargetQuery` (analytical)
   - `DPRDailyTrend` (analytical)
   - `DPRProductionCube`
   - `DPRExcelDownload` (action entity, four bound action imports)
4. Click **Preview** next to each query → opens the Fiori Elements preview directly.

---

## 6. Building the Fiori Elements Analytical List Page

You can either let the **Service Binding Preview** generate a temporary ALP or build a deployable Fiori app.

### 6a. Quick preview (no UI app needed)
In the Service Binding editor click *Open in Preview*. The ALP is rendered automatically using the DDLX annotations. The toolbar shows:
- Native **Export to Spreadsheet** button (Fiori built-in, uses `lineItem`).
- Up to three switchable charts (the qualifiers we defined: `ByDate`, `JvVsOvl`, `ByAsset`).
- Selection fields (date range, product, asset).

### 6b. Deployable Fiori app (Business Application Studio)
1. **File → New → Project from Template → SAP Fiori application**.
2. Floorplan: **Analytical List Page**.
3. Data source: choose **OData V4** and point to the service URL
   `/sap/opu/odata4/sap/zpra_dpr_analytics/srvd/sap/zpra_sd_dpr_analytics/0001/`.
4. Main entity: `DPRProductionQuery`.
5. Generate.
6. Open `webapp/manifest.json` and add **two custom toolbar actions** for the dedicated Excel/PDF buttons, calling the unbound static actions:

```json
"sap.ui5": {
  "extends": { "extensions": {
    "sap.ui.controllerExtensions": {
      "sap.fe.templates.ListReport.ListReportController": {
        "controllerName": "zdpr.ext.controller.ListReportExt"
      }
    }
  }}
}
```

### 6c. Custom controller for the two download buttons

Create `webapp/ext/controller/ListReportExt.controller.js`:

```js
sap.ui.define([], function () {
  "use strict";
  return {
    /* Triggered by the "Download Excel" button (manifest action) */
    onDownloadExcel: function () {
      var oModel = this.getView().getModel();
      var oFB    = this.getView().byId("...::FilterBar");
      var sFrom  = oFB.getControlByKey("ProductionDate").getDateValue();
      var sTo    = oFB.getControlByKey("ProductionDate").getSecondDateValue();

      oModel.bindContext("/DPRExcelDownload(Dummy)/" +
        "com.sap.gateway.srvd.zpra_sd_dpr_analytics.v0001.downloadProduction(...)" )
        .setParameter("date_from", sFrom)
        .setParameter("date_to",   sTo)
        .execute()
        .then(function (oContext) {
          var r = oContext.getBoundContext().getObject();
          // r.excel_base64 — decode and trigger browser download
          var blob = b64toBlob(r.excel_base64, r.mime_type);
          var url  = URL.createObjectURL(blob);
          var a    = document.createElement("a");
          a.href = url; a.download = r.file_name; a.click();
        });
    },

    /* Triggered by the "Download PDF" button */
    onDownloadPdf: function () {
      // identical flow but calls downloadPdfProduction and uses r.pdf_base64
    }
  };
});

function b64toBlob(b64, mime) {
  var bin = atob(b64);
  var len = bin.length;
  var u8  = new Uint8Array(len);
  for (var i = 0; i < len; i++) u8[i] = bin.charCodeAt(i);
  return new Blob([u8], { type: mime });
}
```

In `manifest.json` register the buttons under `sap.fe.templates`:

```json
"controlConfiguration": {
  "@com.sap.vocabularies.UI.v1.LineItem": {
    "actions": {
      "ExcelExport": {
        "press":   ".extension.zdpr.ext.controller.ListReportExt.onDownloadExcel",
        "visible": true,
        "enabled": true,
        "text":    "Download Excel"
      },
      "PdfExport": {
        "press":   ".extension.zdpr.ext.controller.ListReportExt.onDownloadPdf",
        "visible": true,
        "enabled": true,
        "text":    "Download PDF"
      }
    }
  }
}
```

### 6d. Deploying the UI app
- BAS → **Deploy → Deploy to ABAP repository** → choose your transport.
- Add the BSP application as a **Fiori Catalog tile** (`/UI2/FLPCM_*` or Launchpad Designer).
- Set tile target to `semantic-object: DailyProductionReport, action: display`.

---

## 7. PDF without ADS — Smart Forms fallback

If your system does not have ADS configured, replace the body of `ZCL_ZPRA_DPR_PDF=>generate_production_pdf` with:

```abap
DATA: lv_fmname    TYPE rs38l_fnam,
      ls_ctrlop    TYPE ssfctrlop,
      ls_outopt    TYPE ssfcompop,
      ls_jobout    TYPE ssfcrescl,
      lt_otf       TYPE STANDARD TABLE OF itcoo,
      ls_pdf       TYPE STANDARD TABLE OF tline.

CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
  EXPORTING formname = 'ZPRA_SF_DPR_PRODUCTION'
  IMPORTING fm_name  = lv_fmname.

ls_ctrlop-getotf    = abap_true.
ls_ctrlop-no_dialog = abap_true.

CALL FUNCTION lv_fmname
  EXPORTING control_parameters = ls_ctrlop
            output_options     = ls_outopt
            iv_date_from       = iv_date_from
            iv_date_to         = iv_date_to
            it_data            = it_data
  IMPORTING job_output_info    = ls_jobout.

CALL FUNCTION 'CONVERT_OTF'
  EXPORTING format                = 'PDF'
  IMPORTING bin_filesize          = DATA(lv_size)
  TABLES    otf                   = ls_jobout-otfdata
            lines                 = ls_pdf.

ev_pdf   = cl_bcs_convert=>solix_to_xstring( ... ).
ev_pages = lines( ls_jobout-otfdata ).
```

Build matching Smart Forms `ZPRA_SF_DPR_PRODUCTION` and `ZPRA_SF_DPR_TARGETS` using transaction `SMARTFORMS`.

---

## 8. Smoke test

After deploy, test each piece independently.

### 8a. CDS query test
ADT → Open `ZPRA_Q_DPR_PROD_QUERY` → **F8 (Data Preview)** → enter
`P_DateFrom=20260101`, `P_DateTo=20260131`. You should see daily aggregated rows.

### 8b. OData service test
Browser: navigate to
```
/sap/opu/odata4/sap/zpra_dpr_analytics/srvd/sap/zpra_sd_dpr_analytics/0001/$metadata
```
Confirm the four action imports are listed:
- `downloadProduction`
- `downloadTargets`
- `downloadPdfProduction`
- `downloadPdfTargets`

### 8c. Action invocation test (Postman / `curl`)
```
POST /sap/opu/odata4/sap/zpra_dpr_analytics/srvd/sap/zpra_sd_dpr_analytics/0001/
     DPRExcelDownload(Dummy)/
     com.sap.gateway.srvd.zpra_sd_dpr_analytics.v0001.downloadProduction
Content-Type: application/json
{ "date_from": "2026-01-01", "date_to": "2026-01-31" }
```
Response payload contains `excel_base64`, `file_name`, `mime_type`. Decode the base64 to a `.xlsx` and open in Excel.

Repeat with `downloadPdfProduction` to confirm PDF generation.

### 8d. Fiori ALP test
Open the app. The selection bar should show date range, product, asset. After **Go**:
- Top half: chart toolbar with three switchable charts.
- Bottom half: paginated grid of rows.
- Toolbar: native **Export to Spreadsheet**, custom **Download Excel**, custom **Download PDF** buttons.
- Apply different filters → all three views update reactively.

---

## 9. Mapping to legacy `ZPRA_DPR_REPORT` sections

| Legacy section | RAP query | Notes |
|---------------|-----------|-------|
| Sheet 1 — sec2a (Daily Production) | `ZPRA_Q_DPR_PROD_QUERY` | 1:1 fields |
| Sheet 1 — sec2c (Production graph) | `ZPRA_Q_DPR_DAILY_TREND` chart `TrendLine` | Line chart |
| Sheet 1 — sec2d (Monthly trend) | `ZPRA_Q_DPR_DAILY_TREND` chart `OvlShareArea` | Area chart |
| Sheet 2 — sec3a (Target vs Actual) | `ZPRA_Q_DPR_TARGET_QUERY` | Includes achievement % criticality |
| Sheet 2 — sec3f (Historical Actual) | Use `CalendarYear` filter on Cube | 5-year history via free dimension |
| Sheet 3 (Variance analysis) | `ZPRA_Q_DPR_TARGET_QUERY` chart `VarianceByPeriod` | Built into target query |

---

## 10. Going live checklist

- [ ] All 21 ABAP objects activate without warnings.
- [ ] `ZPRA_T_PRD_PI` table is populated for the test period (otherwise `OvlShareQty*` are NULL).
- [ ] Adobe forms `ZPRA_FRM_DPR_PRODUCTION` / `ZPRA_FRM_DPR_TARGETS` are activated **in the same client** as the ADS configuration.
- [ ] Service binding `ZPRA_SB_DPR_ANALYTICS_O4` is **Published**.
- [ ] OData smoke test (Section 8c) returns HTTP 200 with non-empty `excel_base64` / `pdf_base64`.
- [ ] Fiori catalog/launchpad tile points to the deployed BSP app.
- [ ] End-user test: open ALP → Apply filter → see chart + table → click each download button → file opens correctly.

If every checkbox is ticked, the analytical RAP is production-ready.
