**Subject:** RE: Vendor and Customer Master Creation - Mass Upload Template for BP Creation - observations of 15.09.2026

Dear Ashish, Pratima, Harshada,

Thank you for the detailed testing results. We have gone through the workbook tab
by tab, including the screenshots pasted on each sheet. Please find below what we
found for each one, and what is needed from your side.

To answer the headline first: **no new vendor was created because the vendor
creation tab never reached the program.** It was stopped on the selection screen
by a defect at our end, described under tab 2 below. Every other scenario in your
test did run.

---

### Tab by tab

| # | Tab | What the test showed | Cause | Status |
|---|---|---|---|---|
| 1 | Bank key creation | 6 rows OK | – | **Worked. No action.** |
| 2 | All CC Vendor creation | *"The customer workbook is .xlsx - .xls is not supported."* | **Program** | **Fixed.** Please re-test. |
| 3 | Bank details update | 2 rows posted | – | **Worked. No action.** |
| 4 | Vendor extension | 1 row posted, with a warning on the reference | **Program** | **Fixed.** The vendor *was* extended. |
| 5 | CIN details | 1 row posted | – | **Worked. No action.** |
| 6 | Block / Unblock | 1 row posted, 1 row error on `SPERM_1` | **Program + data** | **Fixed**, and one point to confirm. |
| 7 | TDS upload | *"No data rows were found to process."* | **Program (message) + file** | **Fixed.** We need the file you used. |
| 8 | TAN details | *"2 TAN exemption rows sent…"* | **Program** | **Fixed.** |
| 9 | Partner function | `F2 165` on every row | **Master data** | **Action needed at your end.** |

---

### 2. All CC Vendor creation — program defect, fixed

The file selection field could hold only 128 characters. Any path longer than
that — which a OneDrive or Teams synchronised folder reaches on its own — was cut
short, and the `.xlsx` at the end of the name was the first thing lost. The
program then reported a file-type problem about a file that was perfectly good.

The field now holds 255 characters, a longer path is refused with a clear message
instead of being cut short silently, and the file type is judged by the end of the
name. If a genuine `.xls` file was selected, please save it as *Excel Workbook
(\*.xlsx)* — the ABAP reader can only read `.xlsx`.

### 4. Vendor extension — the vendor was extended

Vendor `100167642` (vendor `0100142655`) **was** extended: company code 3900 and
purchasing organisation 3900 were both created. The warning concerned the *copy*,
not the extension.

The row named company code 1000 as the reference to copy from, but left **REF
LIFNR (column 5) empty**, and the program was not reading that column at all — so
it looked for company code 1000 data on the vendor being extended, which does not
have any. Column 5 is now read.

Two consequences:

* Fields that only the reference would have brought — `ZTERM` and `FDGRV` — are
  **not set** on company code 3900. Please check and maintain them if needed.
* For the next test, put the source vendor in column 5 alongside REF BUKRS and
  REF EKORG, and the copy will work.

### 6. Block / Unblock — one fix, one point to confirm

The error `SPERM_1 (purch.org block) requires a purchasing organisation in column
4` was correct per the template's own guideline (*"Purch Org is mandatory if need
to apply this block"*). Following your note that **purchasing organisation is not
applicable to an employee code**, the program now treats this as a warning and
applies the purchasing block centrally (`SPERM`, column 7) instead, so the row
posts.

**One point to confirm:** the same row sets `SPERR_1 = X` **and** `SPERQ = 99`
together. The guideline above column I of your template reads *"This should be
blank if record has to block at company/ purchase level."* The program enforces
that rule, so the row will now stop on it instead.

Your own sample rows show the two valid patterns:

* row 9 — `SPERR`, `SPERM`, `SPERQ` filled, `_1` columns empty (block at vendor level);
* rows 10/11 — `SPERR_1`, `SPERM_1` filled, `SPERQ` empty (block at company code / purchasing organisation level).

For an employee-code total block, please use the first pattern. Kindly confirm
whether the `SPERQ` rule still holds, or whether `SPERQ` should be allowed
alongside a company-code block.

### 7. TDS upload — the message was hiding the real reason

*"No data rows were found to process."* was never a diagnosis. The program had
already stopped — the file reader had refused the file and said why — but that
reason was written to the status bar and then overwritten by this generic
sentence before anyone could read it.

Every reason is now written into the result list, where it stays on screen. The
column mapping itself is correct: checked against your own observations workbook,
all 64 TDS columns are recognised and the vendor is found in column B.

**Request:** please send us the exact TDS file that was uploaded. With the fix in
place the program will name the reason itself, but the file would let us confirm
it immediately.

### 8. TAN details — "sent" is now "saved" or "failed"

`J_1ITAN_EXEM_SAVE` is an update module: the call only registers the work, and the
commit that follows is what performs it. The program was not checking the result,
so the message *"2 TAN exemption rows sent"* appeared whether the rows were stored
or lost.

It now reports per row, against the vendor and the Excel row: *"2 TAN exemption
row(s) saved"*, or an error naming the failure. We reviewed every other scenario
for the same weakness and corrected it in five more places — vendor/BP posting,
bank key creation, customer posting, credit data and the licence record — so no
scenario now reports a success it has not verified.

### 9. Partner function — master data, action needed at your end

All five rows requested partner function `ZP` with partner `100098685` in
purchasing organisation 1000, and all five were rejected with

> `F2 165` — Vendor 0100098685 has not been created for purchasing organization 1000

This is a master data condition, not a program defect. Partner `100098685` exists
as a supplier but has **no purchasing organisation 1000 view**. SAP cannot store a
partner function for a partner the purchasing organisation does not know. The
message names the *partner*, which is why it reads as though the row's own vendor
were at fault.

**Action:** please extend vendor `100098685` to purchasing organisation 1000 — the
*Vendor extension* tab does exactly this — and re-run the partner function file.
Nothing in the file needs changing.

The program now checks this before calling the API and says:

> Partner 0100098685 (column 17) is not extended to purchasing organisation 1000 —
> extend it there first (tab "Vendor extension")

---

### Two further improvements you will notice in the result list

1. **A green light now means an outcome.** Lines such as *"100168036 is business
   partner 100168036 - vendor 100143049 is used"* are remarks the program makes on
   the way, not successes. They used to carry a green light, so a failed row showed
   green and red together. They now carry an information icon, and a row that
   failed shows no green line at all.
2. **The run header counts three ways** — `Rows OK`, `Rows with errors`,
   `Rows skipped` — so a row that was passed over is no longer counted among the
   successes.

---

### Summary of what we need from you

1. The **TDS file** that was actually uploaded (tab 7).
2. **Extend vendor `100098685`** to purchasing organisation 1000 (tab 9).
3. **Confirmation of the `SPERQ` rule** in the Block/Unblock template (tab 6).
4. After the corrected program is imported, a **re-test of the All CC Vendor
   creation tab**, which is the one that could not run.

We will share the corrected programs for import. Please let us know if any of the
above needs a discussion.

Best regards,
