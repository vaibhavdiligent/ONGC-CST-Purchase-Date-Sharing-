/**
 * DPRExcelDownload.controller.js
 *
 * Fiori UI5 controller fragment — add this logic to your existing
 * Analytical List Page or Overview Page controller.
 *
 * Two buttons trigger server-side Excel generation via OData V4 action:
 *   1. "Download Production Excel"  → calls downloadProduction action
 *   2. "Download Target Excel"      → calls downloadTargets action
 *
 * The server returns a base64-encoded .xlsx; the controller decodes it
 * and triggers a native browser file-save dialog.
 *
 * Prerequisites:
 *   - Service binding ZPRA_SB_DPR_ANALYTICS_O4 is published (OData V4)
 *   - The Fiori app's manifest.json points to this OData V4 service
 *   - SAP UI5 version >= 1.90 (for OData V4 model action support)
 */
sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/odata/v4/ODataModel",
    "sap/m/MessageBox",
    "sap/m/MessageToast",
    "sap/ui/core/BusyIndicator"
], function (Controller, ODataModel, MessageBox, MessageToast, BusyIndicator) {
    "use strict";

    return Controller.extend("zpra.dpr.controller.DPRExcelDownload", {

        /* ──────────────────────────────────────────────────────────────────
         * Called when user clicks "Download Production Excel" button.
         * Reads DateFrom / DateTo from the filter bar / input fields.
         * ────────────────────────────────────────────────────────────────── */
        onDownloadProductionExcel: function () {
            var oView     = this.getView();
            var oModel    = oView.getModel();           // OData V4 model

            /* Read date inputs — adjust IDs to match your view */
            var sDateFrom = oView.byId("idDateFrom").getValue().replace(/-/g, ""); // YYYYMMDD
            var sDateTo   = oView.byId("idDateTo").getValue().replace(/-/g, "");

            if (!sDateFrom || !sDateTo) {
                MessageBox.warning("Please select Date From and Date To before downloading.");
                return;
            }

            BusyIndicator.show(0);

            /* Call OData V4 unbound action */
            var oOperation = oModel.bindContext(
                "/DPRExcelDownload/downloadProduction(...)"
            );

            oOperation.setParameter("date_from", sDateFrom);
            oOperation.setParameter("date_to",   sDateTo);

            oOperation.execute().then(function () {
                var oResult = oOperation.getBoundContext().getObject();
                BusyIndicator.hide();

                if (oResult && oResult.excel_base64) {
                    _triggerDownload(
                        oResult.excel_base64,
                        oResult.file_name  || "DPR_Production.xlsx",
                        oResult.mime_type  || "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                    );
                    MessageToast.show(oResult.message || "Excel downloaded successfully.");
                } else {
                    MessageBox.error("No data returned from server.");
                }
            }).catch(function (oError) {
                BusyIndicator.hide();
                var sMsg = oError.message || "Error generating Excel. Please try again.";
                MessageBox.error(sMsg);
            });
        },

        /* ──────────────────────────────────────────────────────────────────
         * Called when user clicks "Download Target Excel" button.
         * Reads FiscalYear / TargetCode from filter inputs.
         * ────────────────────────────────────────────────────────────────── */
        onDownloadTargetExcel: function () {
            var oView      = this.getView();
            var oModel     = oView.getModel();

            /* Read inputs — adjust IDs to match your view */
            var sFiscalYear  = oView.byId("idFiscalYear").getValue();
            var sTargetCode  = oView.byId("idTargetCode").getSelectedKey();  // Select/ComboBox

            if (!sFiscalYear || !sTargetCode) {
                MessageBox.warning("Please select Fiscal Year and Target Type before downloading.");
                return;
            }

            BusyIndicator.show(0);

            var oOperation = oModel.bindContext(
                "/DPRExcelDownload/downloadTargets(...)"
            );

            oOperation.setParameter("fiscal_year",  sFiscalYear);
            oOperation.setParameter("target_code",  sTargetCode);

            oOperation.execute().then(function () {
                var oResult = oOperation.getBoundContext().getObject();
                BusyIndicator.hide();

                if (oResult && oResult.excel_base64) {
                    _triggerDownload(
                        oResult.excel_base64,
                        oResult.file_name || "DPR_Targets.xlsx",
                        oResult.mime_type || "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
                    );
                    MessageToast.show(oResult.message || "Excel downloaded successfully.");
                } else {
                    MessageBox.error("No data returned from server.");
                }
            }).catch(function (oError) {
                BusyIndicator.hide();
                var sMsg = oError.message || "Error generating Excel. Please try again.";
                MessageBox.error(sMsg);
            });
        }

    });

    /* ──────────────────────────────────────────────────────────────────────
     * Private helper: decode base64 → Blob → browser file-save dialog
     * ────────────────────────────────────────────────────────────────────── */
    function _triggerDownload(sBase64, sFileName, sMimeType) {
        /* Decode base64 string to binary */
        var sBinary = atob(sBase64);
        var aBytes  = new Uint8Array(sBinary.length);
        for (var i = 0; i < sBinary.length; i++) {
            aBytes[i] = sBinary.charCodeAt(i);
        }

        /* Create a Blob and object URL */
        var oBlob = new Blob([aBytes], { type: sMimeType });
        var sUrl  = URL.createObjectURL(oBlob);

        /* Trigger browser Save As dialog */
        var oLink       = document.createElement("a");
        oLink.href      = sUrl;
        oLink.download  = sFileName;
        oLink.style.display = "none";
        document.body.appendChild(oLink);
        oLink.click();

        /* Cleanup */
        setTimeout(function () {
            document.body.removeChild(oLink);
            URL.revokeObjectURL(sUrl);
        }, 500);
    }
});
