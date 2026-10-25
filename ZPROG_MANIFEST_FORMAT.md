# Manifest file format for `ZPROG_UPLOAD`

`ZPROG_UPLOAD` uploads ABAP source from `.txt` files back into SAP and records each program in a transport request. It decides which file goes into which program from a **manifest** Excel file.

`ZPROG_DOWNLOAD` writes this manifest automatically (`ZPROG_MANIFEST_<timestamp>.xls`). Use this document when you need to build or edit a manifest by hand or with a script.

## Layout

- One Excel workbook. Only the **first worksheet** is read.
- **Three columns, read by position** (A, B, C). The header text is not checked, but the column order is.
- **Row 1 is a header row and is always skipped.** Data starts in row 2.

| Column | Header (row 1) | Max length | Required | Content |
|---|---|---|---|---|
| A | `Object Type` | 12 | No | `PROG` or `INCL`. Only shown in the upload log. It has no effect on the upload and may be left blank. |
| B | `Object Name` | 40 | Yes | Exact SAP program or include name, in upper case. Keep namespace slashes, for example `/SCL/RDOTCSLSR_CCCIL_PRICNG_A1`. The program must already exist in the target system: the upload updates programs, it does not create them. |
| C | `File Name` | 128 | Yes | Name of the source file in the upload folder, **without any folder path**, including the `.txt` extension. |

## Example

| | A | B | C |
|---|---|---|---|
| **1** | Object Type | Object Name | File Name |
| **2** | PROG | ZMM_REPORT_01 | ZMM_REPORT_01.txt |
| **3** | INCL | ZMM_REPORT_01_TOP | ZMM_REPORT_01_TOP.txt |
| **4** | PROG | /SCL/RDOTCSLSR_CCCIL_PRICNG_A1 | #SCL#RDOTCSLSR_CCCIL_PRICNG_A1.txt |

## File naming rule

Windows file names cannot contain `/`, so namespace slashes are replaced with `#`:

- Object name → file name: replace every `/` with `#`, then append `.txt`.
  `/SCL/RDOTC_PRICING` → `#SCL#RDOTC_PRICING.txt`
- File name → object name: drop `.txt`, then replace every `#` with `/`.
  `#SCL#RDOTC_PRICING.txt` → `/SCL/RDOTC_PRICING`

Column C only has to match the real file name in the folder. Following this rule keeps manifests from `ZPROG_DOWNLOAD` and hand-made ones consistent.

## Rules for the source `.txt` files

- All files listed in column C must be in **one folder**. That folder is entered in the `p_dir` field of `ZPROG_UPLOAD`, not in the manifest.
- Each file holds the **complete** source of one program or include, one ABAP line per text line.
- Encoding: **UTF-8** (code page 4110). `ZPROG_DOWNLOAD` writes UTF-8 and `ZPROG_UPLOAD` reads UTF-8. Current Notepad and VS Code save UTF-8 by default.
- Lines must not be longer than 255 characters (the ABAP editor limit).

## Saving the manifest

- Save as `.xls` or `.xlsx`.
- The upload opens the manifest through Excel on the user's PC (function module `TEXT_CONVERT_XLS_TO_SAP`, which uses OLE). Excel must be installed and `ZPROG_UPLOAD` must run in the foreground, not as a background job.
- Keep cells as plain text. Do not merge cells, add formulas, or leave the data on a sheet other than the first.

## How `ZPROG_UPLOAD` treats rows

| Situation | Result in the log |
|---|---|
| Row completely blank | Ignored |
| Object Name or File Name missing | Skipped – missing object or file name |
| File not found or empty in the folder | Error – could not read source text file |
| Program does not exist in the system | Skipped – program does not exist |
| Simulate checkbox ticked | Success – "Simulation: N lines ready to upload", nothing changed |
| Simulate unticked, update worked | Success – uploaded to the transport |
| Update failed (lock, authorization, ...) | Error – RPY_PROGRAM_UPDATE failed (subrc n) |

## Building a manifest from a folder of `.txt` files (Python)

```python
# pip install openpyxl
from pathlib import Path
from openpyxl import Workbook

folder = Path(r"C:\abap_upload")          # folder holding the .txt files
wb = Workbook()
ws = wb.active
ws.append(["Object Type", "Object Name", "File Name"])   # row 1 = header (skipped)

for f in sorted(folder.glob("*.txt")):
    obj_name = f.stem.replace("#", "/").upper()
    ws.append(["PROG", obj_name, f.name])   # column A is informational only

wb.save(folder / "ZPROG_MANIFEST.xlsx")
```

## Checklist before running `ZPROG_UPLOAD`

- [ ] Row 1 is the header; data starts in row 2.
- [ ] Column B holds the exact SAP name (upper case, namespace slashes kept).
- [ ] Column C holds only the file name with `.txt`, no folder path.
- [ ] Every file in column C exists in the upload folder and is UTF-8.
- [ ] Every program in column B already exists in the target system.
- [ ] The transport request is a modifiable **Workbench** request.
- [ ] First run with **Simulate** ticked, check the log, then run again with it unticked.
