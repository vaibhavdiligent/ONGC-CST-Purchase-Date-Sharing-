# Excel Upload Templates for KS01 / KS02 BDC Programs

## Files

| Template | Program | Purpose | Columns |
|----------|---------|---------|---------|
| `KS01_Upload_Template.csv` | YGMS_KS01_UPLOAD | Mass Create Cost Centers | 15 |
| `KS02_Change_Template.csv` | YGMS_KS02_UPLOAD | Mass Change Cost Centers | 6 |

## How to Use

1. Download the CSV template
2. Open in Excel and fill your data (keep Row 1 as header)
3. Save as `.xls` (Excel 97-2003) or `.xlsx` (Excel 2007+) - both formats supported
4. Run the program in SE38
5. Select the file and run with **Test Run** checked first
6. Uncheck Test Run to execute actual BDC

## KS01 - Create Cost Center (15 Columns)

| Col | Field | SAP Field | Required |
|-----|-------|-----------|----------|
| A | Controlling Area | CSKS-KOKRS | Yes |
| B | Cost Center | CSKS-KOSTL | Yes |
| C | Valid From | CSKS-DATAB | Yes |
| D | Valid To | CSKS-DATBI | Yes |
| E | Name | CSKS-KTEXT | Yes |
| F | Description | CSKS-LTEXT | No |
| G | Person Responsible | CSKS-VERAK | No |
| H | Cost Center Category | CSKS-KOSAR | Yes |
| I | Hierarchy Area | CSKS-KHINR | Yes |
| J | Company Code | CSKS-BUKRS | Yes |
| K | Business Area | CSKS-GSBER | No |
| L | Profit Center | CSKS-PRCTR | No |
| M | Currency | CSKS-WAERS | No |
| N | Country | CSKS-LAND1 | No |
| O | Region | CSKS-REGIO | No |

## KS02 - Change Cost Center (6 Columns)

| Col | Field | SAP Field | Required |
|-----|-------|-----------|----------|
| A | Controlling Area | CSKS-KOKRS | Yes |
| B | Cost Center | CSKS-KOSTL | Yes |
| C | Name | CSKS-KTEXT | No |
| D | Description | CSKS-LTEXT | No |
| E | Person Responsible | CSKS-VERAK | No |
| F | Department | CSKS-ABTEI | No |

### KS02 Change Notes

- Only fill the columns you want to **change**
- Leave columns blank to keep existing values unchanged
- Columns A (Controlling Area) and B (Cost Center) are always required to identify the record

## Date Format

Use your SAP user date format setting (usually `DD.MM.YYYY` for Indian locale).

## Selection Screen Parameters

| Parameter | Description | Default |
|-----------|-------------|---------|
| P_FILE | Excel file path (.xls or .xlsx) | - |
| P_MODE | BDC display mode: A=All screens, E=Errors only, N=No display | N |
| P_TEST | Test run (no actual execution) | Checked |
| P_SESS | Create BDC session in SM35 instead of CALL TRANSACTION | Unchecked |
