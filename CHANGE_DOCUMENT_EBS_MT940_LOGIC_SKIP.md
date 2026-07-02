# Change Document — Electronic Bank Statement (MT940) Reference / Clearing Logic

| | |
|---|---|
| **Subject** | ICICI MT940 – custom logic skipped for some bank-statement line items |
| **Modules / Objects** | `RFEKA400` (enhancement `YDVRFI_EBS`), EBS user-exit include `ZXF01U01` |
| **Customer** | GAIL |
| **Prepared by** | Vaibhav |
| **Date** | 02.07.2026 |
| **Status** | Delivered for testing (not yet transported to production) |

---

## 1. Background

During EBS (Electronic Bank Statement) upload in the SWIFT MT940 format for ICICI,
the custom logic that replaces the **Assignment number (ZUONR)** and the FEBCL
selection field with the *original bank reference* was **not being applied to some
line items**. As a result those items did not match / clear as expected in the Bank
Reconciliation (BRS), even though the assignment appeared correct.

The processing is split across two custom objects that work as a pair:

1. **`RFEKA400` → enhancement `YDVRFI_EBS`** — while the statement file is read, the
   original bank reference for each line item is derived and **stored** in the ABAP
   INDX cluster under ID `'EBS'`.
2. **`ZXF01U01` (EBS user exit)** — later, per line item, this exit **reads back** that
   stored reference and applies it to `ZUONR` / `FEBCL`.

If step 1 does not store the reference, or step 2 does not read it correctly, the
line item is silently skipped. Both sides had defects, described below.

---

## 2. Root cause analysis

### 2.1 Producer side — `RFEKA400`, enhancement `YDVRFI_EBS` (Form `UMSATZZEILE`)

The routine that builds and exports the reference table (`LI_REFERENZ`) contained two
defects:

- **Wrong field used in the store condition.**
  The reference was stored only when `STRLEN( REFERENZ ) GT 7`, but for
  `NCOL / ICICI940` items the value actually stored was a **different field**
  (`LW_REFERENZ-ZUONR`, the last 8 characters of the raw line). Items whose
  `REFERENZ` field was short were therefore dropped even though a valid assignment
  had already been derived.

- **Destructive de-duplication in multi-statement files.**
  Before appending the new entry, the code looped over the stored table and **deleted
  every entry that was not present in the current statement's `XFEBEP`**. Because
  `XFEBEP` is re-initialised for every bank statement in the file, this **erased the
  references of all earlier statements** whenever a file contained more than one
  statement. Those earlier line items then had nothing to read in `ZXF01U01`.

- (Minor) For `NCOL`, the offset calculation `STRLEN( LW_SWIFT ) - 8` could become
  negative for very short lines, causing a runtime offset error.

### 2.2 Consumer side — EBS user exit `ZXF01U01`

- **Whole exit aborted when the reference buffer was empty.**
  A `CHECK LI_REFERENZ[] IS NOT INITIAL` at the top terminated the *entire* user exit
  when the INDX buffer was empty — including the `NCOL / NSTO` narrative (`SGTXT`)
  logic that does **not** depend on that buffer.

- **Reference replacement gated on a stale `SY-SUBRC`.**
  The block that replaces `ZUONR` / `FEBCL-SELVON` was controlled by a generic
  `IF SY-SUBRC = 0`. By that point `SY-SUBRC` no longer reflected the reference read —
  it held the result of whatever loop / search / append had run last. Consequently:
  - for some items (e.g. `NCOL` with no matching `T_FEBRE` row) the replacement was
    **skipped**, and
  - for other items the assignment was **overwritten with a blank** reference.

---

## 3. Changes delivered

All changes are marked in the source with begin/end comments:
`*** -> BEGIN CHANGE BY VAIBHAV ON 02.07.2026 (LOGIC-SKIP FIX)` …
`*** <- END CHANGE BY VAIBHAV ON 02.07.2026 (LOGIC-SKIP FIX)`.

### 3.1 `RFEKA400` — enhancement `YDVRFI_EBS`, Form `UMSATZZEILE` (reference builder)

| # | Change | Effect |
|---|--------|--------|
| 1 | The store condition now checks the value that is actually stored (`LW_REFERENZ-ZUONR`) instead of `REFERENZ`. | `NCOL / ICICI940` items are no longer dropped because of the wrong field. |
| 2 | The destructive de-duplication loop was replaced by a **targeted delete of the current line only** (`DELETE … WHERE KUKEY = … AND ESNUM = …`). | References belonging to other bank statements in the same file are preserved. |
| 3 | Added a guard so the `NCOL` offset cannot go negative. | Prevents a possible short-line runtime error. |

*Business logic that is intentionally driven by configuration was **not** changed —
specifically the `YFIBANK_EBS-FLAG` master switch and the minimum-length (`> 7`)
rule for what qualifies as a storable reference.*

### 3.2 `ZXF01U01` — EBS user exit

| # | Change | Effect |
|---|--------|--------|
| 1 | The read result is captured in an explicit flag (`LV_REF_FOUND`) immediately after the reference read. | The replacement decision no longer depends on an unrelated, stale `SY-SUBRC`. |
| 2 | The reference replacement now runs only when the reference was actually found **and** is non-initial. | Items are neither skipped incorrectly nor blanked with an empty reference. |
| 3 | The top-level `CHECK` was narrowed to a scoped `IF`. | The `NCOL / NSTO` narrative (`SGTXT`) logic keeps running even when the reference buffer is empty. |

### 3.3 Other `YDVRFI_EBS` enhancement spots (no change)

For completeness, the remaining enhancement spots were reviewed and require **no code
change**; they are documented as-is:

- **Enhancement 2** – `INSERT_FEBEP_AND_FEBRE` (exports `XFEBEP` to memory).
- **Enhancement 3** – `MEHRZWECKFELD` (NSTO / SWEEP assignment capture).
- **Enhancement 5** – `UMSATZZEILE` start (`TRFF` → `NTRF` substitution).

---

## 4. Impact

- **Functional:** For ICICI MT940 statements, the original bank reference is now applied
  consistently to **every** eligible line item, including `NCOL / ICICI940` items and
  items in **multi-statement files**. This restores the intended matching / clearing in
  BRS.
- **No change** to standard SAP code — all changes are within existing custom
  enhancement/exit objects.
- **No data migration** required. The fix takes effect for statements uploaded after the
  transport is imported.

---

## 5. Testing recommendation (before production)

Please validate in Quality with the following cases:

1. **Single-statement ICICI MT940 file** with `NCOL / ICICI940` line items — confirm
   `ZUONR` / assignment is populated and the items match/clear in BRS.
2. **Multi-statement file** (more than one `:62F:` / bank statement in a single file) —
   confirm the references of the **earlier** statements are retained (this was the main
   regression) and all items match/clear.
3. **File with short references (≤ 7 characters)** — confirm behaviour is as expected per
   the existing configuration rule.
4. **Statement where no original reference exists** — confirm the existing `ZUONR` is left
   untouched (not blanked) and the narrative/`SGTXT` logic still runs.

---

## 6. Notes for the technical team

- All modifications are enclosed in the marked comment blocks noted in §3, for easy review.
- Please perform a **syntax check and activation** in the target system; the objects could
  not be compiled outside the SAP environment.
- Objects to transport: enhancement implementation `YDVRFI_EBS` (in `RFEKA400`) and the EBS
  user-exit include `ZXF01U01`.

---

*Prepared by Vaibhav — 02.07.2026.*
