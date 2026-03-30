# Excel Upload Templates for KS01 / KS02 BDC Programs

## Files

| Template | Program | Purpose | Columns |
|----------|---------|---------|---------|
| `KS01_Upload_Template.csv` | YGMS_KS01_UPLOAD | Mass Create Cost Centers | 14 |
| `KS02_Change_Template.csv` | YGMS_KS02_UPLOAD | Mass Change Cost Centers | 6 |

## How to Use

1. Download the CSV template
2. Open in Excel and fill your data (keep Row 1 as header)
3. Save as `.xls` (Excel 97-2003) or `.xlsx` (Excel 2007+) - both formats supported
4. Run the program in SE38
5. Select the file and run with **Test Run** checked first
6. Uncheck Test Run to execute actual BDC

## KS01 - Create Cost Center (14 Columns)

| Col | Field | SAP Field | Required |
|-----|-------|-----------|----------|
| A | Controlling Area | CSKSZ-KOKRS | Yes |
| B | Cost Center | CSKSZ-KOSTL | Yes |
| C | Valid From | CSKSZ-DATAB_ANFO | Yes |
| D | Valid To | CSKSZ-DATBI_ANFO | Yes |
| E | Name | CSKSZ-KTEXT | Yes |
| F | Description | CSKSZ-LTEXT | No |
| G | Person Responsible | CSKSZ-VERAK | No |
| H | Cost Center Category | CSKSZ-KOSAR | Yes |
| I | Hierarchy Area | CSKSZ-KHINR | Yes |
| J | Company Code | CSKSZ-BUKRS | Yes |
| K | Business Area | CSKSZ-GSBER | No |
| L | Profit Center | CSKSZ-PRCTR | No |
| M | Country | CSKSZ-LAND1 | No |
| N | Region | CSKSZ-REGIO | No |

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
