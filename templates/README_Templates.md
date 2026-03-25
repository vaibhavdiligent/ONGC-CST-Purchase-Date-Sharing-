# Excel Upload Templates for KS01 / KS02 BDC Programs

## Files

| Template | Program | Purpose |
|----------|---------|---------|
| `KS01_Upload_Template.csv` | YGMS_KS01_UPLOAD | Mass Create Cost Centers |
| `KS02_Change_Template.csv` | YGMS_KS02_UPLOAD | Mass Change Cost Centers |

## How to Use

1. Download the CSV template
2. Open in Excel and fill your data (keep Row 1 as header)
3. Save as `.xls` (Excel 97-2003) or `.xlsx` (Excel 2007+) - both formats supported
4. Run the program in SE38
5. Select the file and run with **Test Run** checked first
6. Uncheck Test Run to execute actual BDC

## Column Layout (14 Columns)

| Col | Field | SAP Field | KS01 Required | KS02 Required |
|-----|-------|-----------|---------------|---------------|
| A | Controlling Area | CSKS-KOKRS | Yes | Yes |
| B | Cost Center | CSKS-KOSTL | Yes | Yes |
| C | Valid From | CSKS-DATAB | Yes | Yes |
| D | Valid To | CSKS-DATBI | Yes | No |
| E | Name | CSKS-KTEXT | Yes | No |
| F | Description | CSKS-LTEXT | No | No |
| G | Person Responsible | CSKS-VERAK | No | No |
| H | Department | CSKS-ABTEI | No | No |
| I | Cost Center Category | CSKS-KOSAR | Yes | No |
| J | Hierarchy Area | CSKS-KHINR | Yes | No |
| K | Company Code | CSKS-BUKRS | Yes | No |
| L | Business Area | CSKS-GSBER | No | No |
| M | Profit Center | CSKS-PRCTR | No | No |
| N | Currency | CSKS-WAERS | No | No |

## KS02 Change Notes

- Only fill the columns you want to **change**
- Leave columns blank to keep existing values unchanged
- Columns A (Controlling Area), B (Cost Center), C (Valid From) are always required to identify the record

## Date Format

Use your SAP user date format setting (usually `DD.MM.YYYY` for Indian locale).

## Selection Screen Parameters

| Parameter | Description | Default |
|-----------|-------------|---------|
| P_FILE | Excel file path (.xls or .xlsx) | - |
| P_MODE | BDC display mode: A=All screens, E=Errors only, N=No display | N |
| P_TEST | Test run (no actual execution) | Checked |
| P_SESS | Create BDC session in SM35 instead of CALL TRANSACTION | Unchecked |
