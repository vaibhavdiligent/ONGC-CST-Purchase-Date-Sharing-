# Cipla vendor master — sandbox test of 15.09.2026

Source: `S4 - Vendor Master_Obervations.xlsx` (Pratima Shetye / Harshada Devdare,
CRS client 500) and Ashish Dumbre's mail of 23.09.2026, *"No new vendor or BP as
Vendor got created."*

The observations are not in the cells — they are **screenshots pasted onto each
tab**. Read back tab by tab, seven of the nine scenarios posted. Only the one
scenario that creates a vendor never ran.

## What each tab actually reported

| Tab (scenario) | Screenshot | Result |
|---|---|---|
| Bank key creation (R4) | `PRODUCTIVE RUN  Rows OK: 6  errors: 0` | 6 bank keys created / changed |
| **All CC Vendor creation (R1)** | *"The customer workbook is .xlsx - .xls is not supported."* | **never ran — stopped on the selection screen** |
| Bank details (R5) | Rows OK: 2 | both posted |
| Vendor extension (R6) | Rows OK: 1 | posted, with a warning that reference company code 1000 holds no data for that vendor |
| CIN details (R7) | Rows OK: 1 | posted |
| Block / unblock (R9) | 1 posted, 1 error | `SPERM_1 (purch.org block) requires a purchasing organisation in column 4` |
| **TDS upload (R2)** | *"No data rows were found to process."* | **nothing was read** |
| TAN details (R3) | 2 blocks validated, 2 rows sent to `J_1ITAN_EXEM_SAVE` | posted |
| Partner function (R8) | every row `F2 165` | `Vendor 0100098685 has not been created for purchasing organization 1000` |

So the headline is accurate but narrow: **no vendor was created because the vendor
creation tab never reached the program.** Everything downstream of it worked.

## Root causes and what was changed

### 1. A good .xlsx refused — the file path was cut short (R1 blocker)

`P_FILE` was `RLGRAP-FILENAME`, which is **CHAR 128**.
`CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG` hands the chosen file back as a
`STRING`. A path longer than 128 characters — which a OneDrive or Teams
synchronised folder reaches on its own — was silently truncated on the way into
the parameter, and the `.xlsx` at the end of it was the first thing lost. The
program then tested the name, found no `.xlsx`, and reported a file type problem
about a perfectly good file.

Changed, in both upload programs and both download programs:

* the parameter is now `TYPES ty_path TYPE c LENGTH 255` — the widest a screen
  field goes — and carries `LOWER CASE`, so an application server path keeps its
  case;
* a path longer than 255 characters is now refused at the dialog with a message
  that says so, instead of being cut short in silence;
* the extension test now looks at the **end** of the name (`NP '*.XLSX'`) rather
  than asking whether `.xlsx` appears anywhere in it. A folder called
  `xlsx files` used to let a `.xls` through, and a truncated path used to hold a
  `.xlsx` back — both are now impossible;
* the message says what to do: *"open the file in Excel and save it as Excel
  Workbook (\*.xlsx)"*.

`tools/audit_file_path.py` was added so neither fault can return.

**If the tester genuinely picked a `.xls` file**, the new message now tells them
to save it as `.xlsx`. `CL_FDT_XL_SPREADSHEET` reads the OpenXML package only;
binary `.xls` cannot be read at all, by this program or any other ABAP reader.

### 2. "No data rows were found to process." said nothing useful (R2)

That message appears when the log is completely empty — every row of the tab was
passed over because its key column was blank. It named neither the tab, nor the
row count, nor the column. The run now ends with, for example:

> Tab "TDS upload" has 12 row(s) below its heading line, and column 2 (LIFNR) is
> empty on every one of them — nothing was processed. Check that the heading line
> sits directly above the data and that this column is filled.

Each scenario now declares the column it reads its key from (`LIF_H~KEY_COL`):
column 5 for vendor creation, column 1 for TAN, CIN and partner functions, column
2 for the rest.

**Still open:** the observations workbook's own TDS tab binds perfectly — the
reader matches 64 of 64 headings and finds `LIFNR` in column 2 with a value. So
the file the tester actually uploaded was shaped differently from the one they
reported with. The new message will name the reason on the next run; if it is
wanted sooner, the TDS file they uploaded is needed.

### 3. Partner functions failed with a message that pointed at the wrong vendor (R8)

Every row asked for partner function `ZP` with partner `100098685` in purchasing
organisation 1000. The partner exists as a supplier, but it is **not extended to
purchasing organisation 1000**, so the API answered `F2 165` — *"Vendor
0100098685 has not been created for purchasing organization 1000"* — which reads
as though the row's own vendor were at fault.

The handler now checks `LFM1` for the **partner** before building the row:

> Partner 0100098685 (column 17) is not extended to purchasing organisation 1000 —
> extend it there first (tab "Vendor extension")

This is master data, not a program defect: partner `100098685` has to be extended
to purchasing organisation 1000 before those five rows can post.

### 4. Purchasing-organisation block on an employee vendor (R9)

Cipla's own note on the tab: *"Purchase Org is not applicable to Employee code"*.
Row 2 (vendor 362243) set `SPERM_1` with no purchasing organisation, and the row
was turned away.

A vendor that has no purchasing organisation cannot carry a block at that level,
and refusing the row helps nobody. The block is now applied where that vendor
does carry one — centrally, in `LFA1-SPERM` — and the row says so as a warning
rather than an error. The same now applies to `SPERR_1` without a company code.
Where the central column is already filled, the warning says that instead of
overwriting it. A row's rule breaches are also reported together now, rather than
one per run.

## To confirm with Cipla

1. **`SPERQ` together with a company-code block.** The Block / Unblock template's
   own guideline says `SPERQ` must stay blank when a company-code or
   purchasing-organisation block is set, and the program enforces it. Test row 2
   sets `SPERR_1 = X` **and** `SPERQ = 99`. That row will now fail on the `SPERQ`
   rule instead of the purchasing-organisation one. Is the guideline still
   correct, or should `SPERQ` be allowed alongside a company-code block?
2. **The TDS file.** The file actually uploaded for the TDS tab, so the empty run
   can be reproduced rather than inferred.
3. **Partner `100098685`.** Confirm it should be extended to purchasing
   organisation 1000, or that a different partner was meant.
