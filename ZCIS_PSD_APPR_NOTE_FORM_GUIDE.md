# CIS 2026-27 — PSD Discount Approval Note (SAP Interactive Form by Adobe)

Three upload files, mirroring the format of your `ZGGLRAF_DUNNING1` example:

| File | What it is | How to use |
|---|---|---|
| `ZCIS_PSD_APPR_NOTE_layout.XDP` | XFA layout (Adobe LiveCycle Designer template) | Import as the **form Layout** in SFP |
| `ZCIS_PSD_APPR_NOTE_schema.XSD` | Data/context schema | Reference for the **interface context** |
| `SFPF_ZCIS_PSD_APPR_NOTE.XML` | SAP SFP **Form object** (asx:abap) with the XDP embedded (base64) + context heap | Upload as the **Form** |

The form reproduces the customer's "Discount Approval Note" format (GAIL / PMG, Delhi header, Approval Note #, Ref. No., Date, Subject/Variant, Background, Main Body, Discount total value, Approving Authority, and the L1–L6 approval blocks each with a Remarks field and a "digital signature, CPF No. & Time Stamp" line).

## Data context (interface)

Top level:
- `DATE` → bound to `SFPSY-DATE` (system date)
- `WA_NOTE` → a structure with these fields:

| Field | Meaning |
|---|---|
| APPR_NOTE_NO | Approval Note Number |
| REF_NO | Reference number (GAIL/PMG/CPC/PSD…) |
| VARIANT_NAME | Scheme / variant name |
| DISC_TOTAL_VALUE | Discount total value (Rs.) |
| L1_REMARKS … L6_REMARKS | Remarks captured at each level |
| L1_SIGN … L6_SIGN | Approver CPF No. + time stamp at each level |

## Import — recommended order (most reliable path)

**Reliability note:** the `.XDP` (layout) and `.XSD` (schema) are standard, self-contained XML and import cleanly. The `SFPF_*.XML` is provided in the exact SAP form structure with the layout embedded and a consistent context heap; if your system's SFP upload rejects it (form objects can be system/version sensitive), use the fallback below — you lose nothing, because the layout is what carries the design work.

### A. Create the interface (SFP → Interface)
1. Create a DDIC structure, e.g. `ZCIS_APPR_NOTE_S`, with the `WA_NOTE` fields above (all CHAR; lengths in the XSD — e.g. remarks 200, sign 60).
2. SFP → **Interface** `ZCIS_PSD_APPR_NOTE` → create → import parameter `WA_NOTE TYPE ZCIS_APPR_NOTE_S`. `SFPSY-DATE` is available automatically.
3. In the interface **context**, drag `WA_NOTE` and `SFPSY-DATE` in so the context matches the `.XSD` (root `data` → `DATE`, `WA_NOTE` → fields).
4. Activate.

### B. Create the form + layout (SFP → Form)
1. SFP → **Form** `ZCIS_PSD_APPR_NOTE` → assign interface `ZCIS_PSD_APPR_NOTE`.
2. Open the **Layout** → in Adobe LiveCycle Designer, **File → Import** `ZCIS_PSD_APPR_NOTE_layout.XDP` (or open the XDP and copy the `body` subform onto the page). The fields are already bound to `$.WA_NOTE.*` and `$.DATE`, so they connect to the context automatically.
3. Activate.

### Fallback — upload the whole form object
If your team uses the SFP form upload for `SFPF_*.XML` (the same way the DUNNING files were moved), upload `SFPF_ZCIS_PSD_APPR_NOTE.XML`. It contains the interface reference, the context binding, and the embedded XDP. If it imports, you still need the interface `ZCIS_PSD_APPR_NOTE` (Step A) to exist for activation.

## Driver — printing the note from the workflow

To print/e-mail the note (e.g. at L5/L6, or on demand from YRVG052), generate the function module for the form (`FP_FUNCTION_MODULE_NAME`), then:
`FP_JOB_OPEN → <generated FM>( populate WA_NOTE from YCIS_APPRVL: APPR_NOTE_NO, REF_NO, VARIANT_NAME=scheme, DISC_TOTAL_VALUE=Σ rebate_val, L1..L6 remarks from REMARKS/REM_L4/L5/L6 and L1_SIGN..L6_SIGN from L1_USER+L1_DATE+L1_TIME … L6_USER+L6_DATE+L6_TIME ) → FP_JOB_CLOSE`.
I can build this print driver program if you want it wired to the approval table.

## Notes
- Form/interface name `ZCIS_PSD_APPR_NOTE` is a suggestion — rename in SFP to your standard (e.g. `ZGGLRAF_…`); if you rename, also update `<INTERFACE>` in `SFPF_*.XML` and the file names.
- Page size is set to A4; change `medium … stock="a4"` to `stock="letter"` in the XDP if you print on Letter.
