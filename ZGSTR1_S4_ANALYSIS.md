# ZFI_GSTR1_REPORT — ECC (OCP) vs S/4 (OCQ) output analysis

## Summary

The two program listings are **identical source code**. The output difference is
caused by the obsolete table `KONV`, which the program still reads directly and
which no longer returns data in S/4HANA.

## 1. The programs are the same

Extracted both PDF listings (main program + includes ZFI_GSTR1_SS,
ZFI_GSTR1_SUB, ZFI_GSTR1_TOP). Ignoring blank lines, the code MD5 is identical:

    3e81bdeab6b13776911995eb3ac8adf4   old (OCP)
    3e81bdeab6b13776911995eb3ac8adf4   new (OCQ)

The only differences in the raw listings are the attribute page and SAP release
wording:

| | old | new |
|---|---|---|
| System | OCP | OCQ |
| Last changed | 21.07.2026 ABAPUSER02 | 15.08.2026 SAP_ABAP |
| Attribute label | "Development class" | "Package" |

Neither listing contains any ATC remediation marker ("Code Remediation",
"FOR ATC", "2025_1_A"). Unlike ZFI_TDS_REPORT, this program was **never
remediated for S/4** - which is why the obsolete KONV read is still in it.

## 2. Output comparison

Both runs cover the same period (01.04.2025 - 21.03.2026).

- 555 data rows in each
- the same 555 (document, fiscal year, Sr. No.) keys in both - no rows lost
  or gained
- joined on that key: **359 rows differ, 196 match**

A naive positional diff makes almost every column look wrong; that is only row
ordering. Joined on the key, the differences are confined to the fields written
inside one code block.

## 3. Which rows differ

Perfectly separated by document origin:

| | differing (359) | matching (196) |
|---|---|---|
| Doc no. prefix | 8125, 8325 | 2225, 2325, 9222, 9925 |
| Type of Entry | Zero Rated, Scrap Sales | White Goods Recovery |
| Document Type | Bill of Supply, Credit Note | Regular Invoice |

The differing rows are the SD-billing-originated documents; the matching rows
are FI-posted documents.

## 4. Root cause

`ZFI_GSTR1_SUB`, around line 460:

```abap
IF sy-subrc = 0 AND wa_bkpf-glvor = 'SD00'.
  DATA(lv_vbeln) = wa_bkpf-AWKEY.
  SELECT SINGLE knumv FROM vbrk INTO @DATA(lv_knumv)
    WHERE vbeln = @lv_vbeln.
  IF sy-subrc = 0.
    SELECT * FROM konv INTO TABLE @DATA(it_konv)
      WHERE knumv = @lv_knumv.
    LOOP AT it_konv INTO DATA(wa_konv) WHERE kposn = wa_bseg-TXGRP.
      "  <-- sets txbval, grsval, igstrt/igstvl, cgstrt/cgstvl,
      "      sugstr/sugstv, txrate, totval
    ENDLOOP.
  ENDIF.
ELSE.
  " for fi related documents - reads BSET instead
  LOOP AT it_bset2 INTO DATA(wa_bset) ...
ENDIF.
```

SD documents take the KONV branch and get their amounts **only** from KONV.
There is no fallback: if VBRK or KONV returns nothing, every amount field stays
initial and prints as 0.00.

That is exactly the observed symptom. In all 359 differing rows, **every field
written inside the KONV loop is 0.00 in S/4**, and never merely a different
non-zero value:

| Field | differing rows |
|---|---|
| Total Inv/Note Value | 359 |
| Gross Value | 359 |
| Taxable Value | 359 |
| Tax Rate | 21 |
| IGST Rate / Amount | 14 |
| CGST Rate / Amount | 7 |
| SGST/UGST Rate / Amount | 7 |

The rate and tax columns show fewer differences only because 338 of the 359
rows are zero-rated supplies - already 0.00 in ECC, so no visible change. Of
the 21 rows that did carry GST in ECC, all 21 are 0.00 in S/4.

`KONV` was replaced by `PRCD_ELEMENTS` in S/4HANA (pricing / condition
technique simplification). The FI branch reads BSET, which is unchanged in
S/4 - which is why those 196 rows match exactly.

## 5. Verification before fixing

Confirm which of the three steps fails, in OCQ, for one failing document
(e.g. 8125000001):

1. `SE16 -> BKPF`, get `AWKEY` for the document.
2. `SE16 -> VBRK`, that `VBELN`, read `KNUMV`. If empty, the AWKEY-to-VBELN
   mapping is the problem, not KONV.
3. `SE16 -> KONV` and `SE16 -> PRCD_ELEMENTS` for that `KNUMV`. If
   PRCD_ELEMENTS has rows and KONV does not, the KONV read is confirmed.

## 6. Fix

Repoint the read at `PRCD_ELEMENTS` (same field names: KNUMV, KPOSN, KSCHL,
KAWRT, KBETR, KWERT), and replace `SELECT *` with an explicit field list.

Also worth adding, independently of the S/4 issue: the SD branch silently
produces zeros when it finds no conditions. A `sy-subrc` check that reports or
logs the document would have surfaced this immediately instead of shipping
0.00 into a GST return.

## Note on source fidelity

The SE38 PDF listings truncate code at 72 characters; 38 lines are affected.
The block analysed above (lines 455-525) is **not** among them and is fully
visible. A complete corrected program cannot be produced from the PDFs alone -
supply the source as a text file for that.

---

# Round 2 - verification after the KONV -> PRCD_ELEMENTS fix

Compared `change_output` (S/4 after the fix) against `zgstr1_old_output` (ECC
baseline), joined on (document, fiscal year, Sr. No.).

## Result

| | before fix | after fix |
|---|---|---|
| Rows differing from ECC | 359 of 555 | **345 of 555** |
| Regressions | - | **0** |

The fix worked. All 359 rows that previously printed 0.00 now carry values, and
these columns now match ECC on **every row**:

- Total Inv/Note Value
- Taxable Value
- Tax Rate
- IGST Rate / IGST Amount
- CGST Rate / CGST Amount
- SGST/UGST Rate / SGST/UGST Amount

Remaining differences: Gross Value (343 rows) and HSN/SAC (2 rows). Both were
previously masked by the zeros.

## Remaining bug - Gross Value, 343 rows

In ECC, Gross Value equals Taxable Value on all 555 rows. In S/4 it does so on
only 16 of the 359 SD rows.

| doc | Sr | Gross ECC | Gross S/4 | Taxable (both) |
|---|---|---|---|---|
| 8125000003 | 002 | 12,421.21 | 10.00 | 12,421.21 |
| 8125000007 | 002 | 502,600.00 | 140.00 | 502,600.00 |
| 8125000015 | 002 | 451,100.00 | 17,350.00 | 451,100.00 |

Cause, `ZFI_GSTR1_SUB` line 490:

```abap
      IF wa_konv-kschl = 'JOIG' OR ... 'JOUG'.
        wa_final-txbval = wa_konv-kawrt.
      ENDIF.                              "<-- IF closes here

      "Gross value
      wa_final-grsval = wa_konv-kawrt.    "<-- OUTSIDE the IF
```

`grsval` is assigned on every pass of the loop, so it retains the KAWRT of
whichever condition row comes last - freight, a surcharge, a minor condition -
rather than the GST condition. `txbval` is guarded by the IF and was always
correct.

ECC's KONV returned rows in physical key order and the GST condition happened
to land last. HANA guarantees no order without ORDER BY, so a different
condition wins. Same class of defect as the ZFI_TDS_REPORT sort bug: an
order-dependent assignment that ECC got right by accident.

### Fix

```abap
      "Taxable value
      IF wa_konv-kschl = 'JOIG'
      OR wa_konv-kschl = 'JOCG'
      OR wa_konv-kschl = 'JOSG'
      OR wa_konv-kschl = 'JOUG'.

        wa_final-txbval = wa_konv-kawrt.
        "Gross value - must come from the same GST condition, not from
        "whichever row happens to be last in the loop
        wa_final-grsval = wa_konv-kawrt.

      ENDIF.
```

Delete the old `wa_final-grsval = wa_konv-kawrt.` and its `"Gross value`
comment from below the ENDIF. Order-independent, and reproduces ECC on all
343 rows.

## Not code

**HSN/SAC, 2 rows.** Documents 8125000037 and 8325000036: `847130` in ECC vs
`85072000` in S/4 (data-processing machines vs lead-acid accumulators). That is
the material's HSN code in master data. Amounts on both rows are already
correct.

**Same latent pattern in the FI branch.** `wa_final-grsval = wa_bset-hwbas.`
also sits outside its inner IF. All 196 FI rows currently match, so it is not
biting today, but it is the same accident waiting on BSET row order. Worth
fixing at the same time.

---

# Round 3 - verification after the GRSVAL fix

Compared `new_output_1` (S/4) against `zgstr1_old_output` (ECC), joined on
(document, fiscal year, Sr. No.).

## Result - the report is fixed

| run | rows differing from ECC |
|---|---|
| original S/4 | 359 of 555 |
| after KONV -> PRCD_ELEMENTS | 345 of 555 |
| **after GRSVAL fix** | **2 of 555** |

Zero regressions at any step. Every money column now matches ECC on all 555
rows:

| column | differing rows |
|---|---|
| Total Inv/Note Value | 0 |
| Gross Value | 0 |
| Taxable Value | 0 |
| Tax Rate | 0 |
| IGST Rate / Amount | 0 |
| CGST Rate / Amount | 0 |
| SGST/UGST Rate / Amount | 0 |

The `Gross Value == Taxable Value` invariant now holds on 359 of 359 SD rows,
matching ECC exactly (it was 16 of 359 before this fix).

## The 2 remaining rows are master data, not code

| doc | Sr | HSN ECC | HSN S/4 |
|---|---|---|---|
| 8125000037 | 002 | 847130 | 85072000 |
| 8325000036 | 002 | 847130 | 85072000 |

847130 is data-processing machines, 85072000 is lead-acid accumulators - two
unrelated commodities, so this is the HSN/SAC maintained on the material, not
a reporting defect. All amounts on both rows match ECC.

Decide which HSN is correct for those materials and correct it in the material
master. Note this affects the GST return content, so it is worth resolving
before filing.

## Still open (latent, not currently biting)

`wa_final-grsval = wa_bset-hwbas.` in the FI branch sits outside its inner IF -
the same shape as the SD bug just fixed. All 196 FI rows match today, so it is
not causing an error, but it carries the same dependence on row order.

---

# Round 4 - UAT mock2 (fwexternalresapuatmock2testing.zip), FY 2024-25

Two ALV exports, both by "Assistant tax", 22 seconds apart:

- `EXPORT_20260821_124431.xlsx` - 519 data rows, header on row 4
- `zgstr1-21.08.26.XLSX` - 519 data rows, header on row 1

Period is **01.04.2024 - 31.03.2025**, a different fiscal year from the run
verified in round 3 (that was FY 2025-26, documents 8125*).

## Which export is which

Workbook metadata is identical on both (creator "SAP WebAS"), so provenance was
established from the HSN/SAC fingerprint found in round 2:

| doc | EXPORT | zgstr1 | round-2 finding |
|---|---|---|---|
| 8124000232 and 5 others | 85072000 | 847130 | S/4 = 85072000, ECC = 847130 |

So **EXPORT_20260821_124431.xlsx is the S/4 output** and
**zgstr1-21.08.26.XLSX is the ECC baseline**.

## The SD amounts are still completely missing

Joined on (document, fiscal year, Sr. No.): same 519 keys in both, no rows lost.
327 rows differ, and the split is the original pre-fix signature exactly:

| | differing (326 detail rows) | matching (192) |
|---|---|---|
| Doc prefix | 8124, 8324 | 2224, 2324, 9232, 9424, 9924, 9222 |
| Type of Entry | Zero Rated, Scrap Sales | White Goods Recovery |

**All 326 SD rows are 0.00 in S/4; all 326 are non-zero in ECC.** Not a partial
failure - the SD branch produces nothing at all.

| column | S/4 | ECC | missing |
|---|---:|---:|---:|
| Total Inv/Note Value | 0.00 | 3,333,432,903.81 | 3,333,432,903.81 |
| Taxable Value | 0.00 | 3,330,085,125.57 | 3,330,085,125.57 |
| IGST Amount | 0.00 | 475,903.26 | 475,903.26 |
| CGST Amount | 0.00 | 1,435,937.49 | 1,435,937.49 |
| SGST/UGST Amount | 0.00 | 1,435,937.49 | 1,435,937.49 |

The 192 FI rows tie out to the rupee (183,330,647.86 both sides), confirming the
BSET branch is healthy and that document selection is correct - only the SD
amounts are absent.

## What this means

This is the **round-2 bug, unchanged**, on a different fiscal year. Two possible
causes, and they are distinguishable:

1. **The corrected program is not in this system.** This is a UAT/mock2
   environment, separate from where the fix was made and verified. Most likely
   explanation - check the transport.
2. **The fix is present but PRCD_ELEMENTS has no FY 2024-25 conditions.** The
   round-3 verification passed on FY 2025-26 documents. If FY 2024-25 documents
   were migrated without their pricing conditions, the corrected read still
   returns nothing.

### How to tell them apart

1. In the mock2 system, open `ZFI_GSTR1_SUB` and look at the read near line 473.
   If it still says `FROM konv`, it is cause 1 - transport the fix.
2. If it says `FROM prcd_elements`, take document 8124000001: get `BKPF-AWKEY`,
   look up `VBRK-KNUMV`, then check `PRCD_ELEMENTS` for that `KNUMV`. No rows
   means cause 2 - a data migration gap, not a code defect.

Cause 2 would need a business decision, not a code change: FY 2024-25 GSTR1
cannot be produced from S/4 if the conditions behind those invoices were never
migrated.

## Round 4b - the mock2 program reads V_KONV_CDS

Confirmed by the developer: the read in the mock2 system is against
`V_KONV_CDS`, not `KONV`. So the remediation IS in that system, and cause 1
from round 4 (fix not transported) is ruled out.

### The loop body never executes - not once

Split the S/4 SD rows by whether a field is written inside or outside the
`LOOP AT it_konv` block:

| field | written | non-zero in S/4 | non-zero in ECC |
|---|---|---|---|
| Taxable Value | inside | 0 / 326 | 326 / 326 |
| Gross Value | inside | 0 / 326 | 326 / 326 |
| Total Inv/Note Value | inside | 0 / 326 | 326 / 326 |
| Tax Rate | inside | 0 / 326 | 19 / 326 |
| IGST Rate / Amount | inside | 0 / 326 | 12 / 326 |
| CGST Rate / Amount | inside | 0 / 326 | 7 / 326 |
| SGST/UGST Rate / Amount | inside | 0 / 326 | 7 / 326 |
| Quantity | outside | 326 / 326 populated | 326 / 326 |
| HSN/SAC | outside | 326 / 326 populated | 326 / 326 |
| Unit | outside | 326 / 326 populated | 326 / 326 |
| Place of Supply | outside | 326 / 326 populated | 326 / 326 |
| Invoice Number | outside | 324 / 326 populated | 324 / 326 |

Everything outside the loop is populated and matches ECC exactly. Every one of
the ten fields inside the loop is zero on every one of the 326 rows.

This matters: a data-quality gap would show a **mix** - some documents with
conditions, some without. Zero out of 326 across ten independent fields means
the loop body is **never entered at all**. The document, the item, the HSN, the
quantity and the unit are all resolved correctly; only the condition read
yields nothing.

### Three candidate failure points, in code order

```abap
IF sy-subrc = 0 AND wa_bkpf-glvor = 'SD00'.        " (A)
  DATA(lv_vbeln) = wa_bkpf-AWKEY.
  SELECT SINGLE knumv FROM vbrk INTO @DATA(lv_knumv)
    WHERE vbeln = @lv_vbeln.                       " (B)
  IF sy-subrc = 0.
    SELECT * FROM v_konv_cds INTO TABLE @DATA(it_konv)
      WHERE knumv = @lv_knumv.                     " (C)
    LOOP AT it_konv WHERE kposn = wa_bseg-TXGRP.   " (D)
```

- **(A)** `GLVOR` not `SD00` -> the ELSE/BSET branch runs instead and finds
  nothing for these documents. Also produces all-zeros.
- **(B)** `AWKEY` -> `VBELN` fails, or `VBRK-KNUMV` is empty, so (C) never runs.
- **(C)** `V_KONV_CDS` returns no rows for that `KNUMV`.
- **(D)** rows returned, but `KPOSN` never equals `BSEG-TXGRP`.

The evidence cannot separate these from the outside - all four produce exactly
the observed all-zero result.

### The experiment that separates them

Round 3 verified this same logic working on FY 2025-26 documents (8125*).
Round 4 fails on FY 2024-25 (8124*). Run the checks below on **one failing
document and one working document** - the contrast is what identifies the cause.

Take `8124000001` (failing) and an `8125*` document that produced correct values:

1. `SE16 -> BKPF`, that BELNR: read `GLVOR` and `AWKEY`.
   GLVOR not `SD00` on the failing one -> cause (A), and the report is taking
   the FI branch for SD documents.
2. `SE16 -> VBRK`, VBELN from AWKEY: read `KNUMV`.
   Empty or no record -> cause (B).
3. With that KNUMV, count rows in **all three**: `V_KONV_CDS`,
   `PRCD_ELEMENTS`, and `KONV`.
   - all three empty -> the conditions were never migrated (data gap)
   - `KONV` has rows but `PRCD_ELEMENTS` / `V_KONV_CDS` do not -> the pricing
     data migration did not cover this document; **switching the read to
     PRCD_ELEMENTS will not help**, the data has to be migrated
   - `PRCD_ELEMENTS` has rows but `V_KONV_CDS` does not -> the compatibility
     view is the problem; read `PRCD_ELEMENTS` directly
4. If rows are returned, compare `KPOSN` against `BSEG-TXGRP` for that document
   -> cause (D).

Step 3 is the one that decides whether this is a code change or a migration
task, so do not skip the three-way count.

### Note on scope

If the answer is that FY 2024-25 conditions are absent from PRCD_ELEMENTS, no
code change can produce those amounts, and filing GSTR1 for that year out of
S/4 needs a business decision rather than a developer fix.

## Round 4c - V_KONV_CDS returns the data; the join key is the problem

SE16 on `V_KONV_CDS` for `KNUMV = 0001172510` returns 2 rows:

| KPOSN (Item) | STUNR | KSCHL | KAWRT |
|---|---|---|---|
| 10 | 10 | ZSER | 30.00 |
| **10** | 20 | **JOIG** | **88,945.20** |

`88,945.20` is exactly the Taxable / Gross / Total value that document
`8124000001` Sr. No. 002 shows in the ECC baseline. So:

- **(B) is fine** - the KNUMV resolves.
- **(C) is fine** - `V_KONV_CDS` returns the conditions, with the right value.

That leaves **(D)**: `LOOP AT it_konv WHERE kposn = wa_bseg-TXGRP`.

### Why (D) is now the prime suspect

`KPOSN` in the view is **10** - the SD billing item number, stored as NUMC(6)
`000010`. `BSEG-TXGRP` is NUMC(3). ABAP pads the shorter operand with leading
zeros, so the loop matches only when TXGRP is `010`:

| BSEG-TXGRP | padded | vs KPOSN 000010 |
|---|---|---|
| `010` | `000010` | match |
| `001` | `000001` | no match |
| `002` | `000002` | no match |
| `000` | `000000` | no match |

In ECC the match evidently succeeded, so TXGRP was `010` there. If S/4 fills it
differently, the loop body is skipped on every row - which is precisely the
observed all-zero result.

This also fits the rest of the evidence. `wa_bseg` itself resolves correctly:
`MENGE`, `MEINS` and `HSN_SAC` come from the same work area and are populated
on all 326 rows. Only the field used as the join key fails.

Note the report has no other link to the SD item - the BSEG select
(`it_bseg_h`) reads `VBELN` but **not** `POSNR`, so `TXGRP` is the only
available key.

Also note `Sr. No.` is `BSEG-BUZEI` (`wa_final-invlin = wa_bseg-buzei`), not
TXGRP - the `002` in the output is the FI line number and is unrelated to
KPOSN.

### The check that confirms it

`SE16 -> BSEG`, the FI document behind KNUMV 0001172510, column `TXGRP`:

- `TXGRP = 010` -> (D) is fine, and the cause is (A): `BKPF-GLVOR` is not
  `SD00`, so the ELSE/BSET branch runs. For zero-rated supplies BSET holds no
  line in GL accounts 192402/192403/192404, so that branch also yields zero.
  Check `BKPF-GLVOR` in that case.
- `TXGRP` anything else -> confirmed (D).

Run the same check on a working FY 2025-26 document (8125*). If TXGRP is `010`
there and different here, that is the whole answer.

### If (D) is confirmed

Do not "fix" it by loosening the comparison. The loop needs the SD item number,
and TXGRP is only a proxy for it. Options, best first:

1. Add `POSNR` to the `it_bseg_h` select and match `kposn = wa_bseg-posnr`.
   This is the real key and removes the dependence on how TXGRP is filled.
2. If POSNR is not populated on the FI line, derive the item from the billing
   document (`VBELN` is already selected) via VBRP, and match on that.

Either way, this is a code change, not a migration issue - the condition data
is present and correct in S/4.

## Round 4d - the TDS-style SORT pattern is not present here

Checked whether the ZFI_TDS_REPORT failure mode (SORT followed by
DELETE ADJACENT DUPLICATES keeping the wrong row) also applies to this program.
It does not:

- **No `DELETE ADJACENT DUPLICATES` anywhere.** The only occurrence, line 660,
  is commented out.
- **`it_bseg_h` is never sorted.** Neither is `it_konv`.
- The only internal-table deletes are `it_bset` by MWSKZ (176), `it_bseg_h`
  WHERE `mwskz = ' '` (364), and `it_final` WHERE `revdoc NE ''` (987) - all
  value filters, none order-dependent.

The output evidence rules it out independently. Had rows been dropped from
`it_bseg_h`, those documents would be **absent** from the report; instead all
326 rows are present with correct Quantity, HSN/SAC, Unit and Place of Supply.
Had rows been dropped from `it_konv`, the result would be a **mix** of correct
and zero values, not 0 out of 326 across ten separate fields.

The all-or-nothing pattern still points at the loop condition, not at ordering.

## Round 4e - BSEG for 8124000001

SE16 on BSEG for BELNR 8124000001 / GJAHR 2024 returns **6 rows across two
company codes**:

| BUKRS | BUZEI | BSCHL | KOART | HKONT | KUNNR |
|---|---|---|---|---|---|
| OVC | 001 | 40 | S | 0000190316 | |
| OVC | 002 | 01 | D | 0000091110 | 0000010066 |
| OVC | 003 | 50 | S | 0000192401 | |
| OVC | 004 | 50 | S | 0000230101 | |
| OVL | 001 | 01 | D | 0000091111 | 0000010071 |
| **OVL** | **002** | 50 | **S** | **0000230903** | |

The relevant line is **OVL / BUZEI 002**:

- the V_KONV_CDS row carrying `KAWRT 88,945.20` shows `G/L Acct` **230903**,
  which matches `HKONT 0000230903` on that line;
- the report row is `Sr. No. 002`, and `Sr. No.` is `BSEG-BUZEI`;
- `KOART = 'S'`, so it passes the select's `koart = 'S'` filter (OVC BUZEI 002
  is KOART `D` and is excluded).

`TXGRP` is not among the columns in the SE16 default layout, so the join key is
still unconfirmed. That single field is what remains outstanding.

### Separate finding - the BSEG select has no GJAHR

```abap
        FROM bseg INTO TABLE @DATA(it_bseg_h)
        FOR ALL ENTRIES IN @it_bset
        WHERE bukrs IN @s_cc
        AND  belnr = @it_bset-belnr
        AND  koart = 'S'
        AND  buzid NE 'T'.
```

Verified not truncated in the listing (those lines are 24-35 characters). The
WHERE has `BUKRS` and `BELNR` but **no `GJAHR`**, so the same document number
from another fiscal year is pulled in. This is the same defect class as the
SKFBT read fixed in ZFI_TDS_REPORT.

It is not the cause of the zero amounts - row counts match ECC exactly at 519,
so nothing extra is currently leaking in - but it should be closed while the
program is open. `it_bset` already carries `gjahr`; add
`AND gjahr = @it_bset-gjahr`.

## Round 4f - VBELN is on the customer line only

Same 6 BSEG rows with `VBELN` now visible:

| BUKRS | BUZEI | KOART | HKONT | VBELN |
|---|---|---|---|---|
| OVC | 001 | S | 0000190316 | (blank) |
| OVC | 002 | D | 0000091110 | (blank) |
| OVC | 003 | S | 0000192401 | (blank) |
| OVC | 004 | S | 0000230101 | (blank) |
| OVL | 001 | **D** | 0000091111 | **0090002699** |
| OVL | 002 | **S** | 0000230903 | **(blank)** |

`VBELN` is populated only on the **customer (KOART `D`)** line. The select that
fills `it_bseg_h` filters `koart = 'S'`, so **every row in `it_bseg_h` has a
blank VBELN**. The field was added to that select ("added by mohd mobassir")
but on the S lines it is always empty.

This does not break the condition read directly - that path takes the billing
document from `BKPF-AWKEY`, not from `BSEG-VBELN` - but any logic elsewhere
that relies on `wa_bseg-vbeln` is reading a blank field on every row.

Useful by-product: the billing document is **0090002699**. That gives a direct
check of step (B) - `VBRK-VBELN = 0090002699` should yield
`KNUMV = 0001172510`, the KNUMV whose conditions were confirmed present in
round 4c.

`TXGRP` is still not visible in the SE16 layout, so the join key remains the
one unresolved item.

## Round 4g - TXGRP matches; (D) is ruled out

`TXGRP` from SE16:

| BUKRS | BUZEI | KOART | HKONT | TXGRP |
|---|---|---|---|---|
| OVC | 001-004 | S/D | ... | 000 |
| OVL | 001 | D | 0000091111 | 000 |
| **OVL** | **002** | **S** | **0000230903** | **010** |

`OVL / 002` is the line the report reads, and its `TXGRP` is `010`. `KPOSN` in
V_KONV_CDS is `10`, i.e. NUMC(6) `000010`; `010` as NUMC(3) pads to `000010`.
**They match.** The loop condition at line 477 is correct and (D) is eliminated.

### Elimination status

| candidate | status |
|---|---|
| (B) KNUMV resolves | conditions found under 0001172510 - **ruled out** |
| (C) V_KONV_CDS returns data | KAWRT 88,945.20 present - **ruled out** |
| (D) `kposn = wa_bseg-TXGRP` | 000010 = 000010 - **ruled out** |
| skip-check (ZGSTR1_EXEMPT) | rows are present in the output - **ruled out** |
| **(A) `wa_bkpf-glvor = 'SD00'`** | **not yet checked - the only link left** |

The skip-check is ruled out by structure: the `IF sy-subrc <> 0` at line 388
closes at line 962, **after** `APPEND wa_final TO it_final` at line 960. A
document caught by that check is dropped entirely, not zeroed. All 326 rows are
present, so it never triggers.

### Latent defect found while checking this

```abap
  SELECT bukrs belnr gjahr FROM ZGSTR1_EXEMPT INTO TABLE lt_skip_chk.
*  SORT lt_skip_chk BY bukrs belnr gjahr.        "<-- commented out
  ...
        READ TABLE lt_skip_chk INTO wa_skip_chk
          WITH KEY bukrs = ... belnr = ... gjahr = ...
                  BINARY SEARCH.                 "<-- still binary
```

The `SORT` is commented out but `BINARY SEARCH` remains. A binary read on an
unsorted table is undefined - it can return a false hit or a false miss
depending on row order. A false hit here silently drops a document from GSTR1
entirely.

It is **not** causing the zero amounts (row counts match ECC at 519, so nothing
is being dropped today), but it is live and will bite as soon as
ZGSTR1_EXEMPT grows. Either restore the SORT or drop `BINARY SEARCH`.

### Outstanding

`SE16 -> BKPF`, `BUKRS = OVL`, `BELNR = 8124000001`, `GJAHR = 2024`: read
`GLVOR` and `AWKEY`. GLVOR not `SD00` explains the entire symptom - the SD
branch is skipped and the FI/BSET branch finds nothing for zero-rated supplies.

## Round 4h - GLVOR and AWKEY are correct; the VBRK read is the last link

BKPF for 8124000001 / 2024:

| BUKRS | BLART | GLVOR | AWKEY | TCODE |
|---|---|---|---|---|
| OVC | RV | RFBU | `8124000001OVC 2024` | FB01 |
| **OVL** | RV | **SD00** | **`0090002699`** | VF02 |

The OVL row - the one the report reads - has `GLVOR = SD00` and
`AWKEY = 0090002699`. **(A) is ruled out.**

Line 426 also confirms this is one of the failing rows:
`ELSEIF wa_final-glacct = '230903'. wa_final-typent = 'Zero Rated'.`

Lines 400-460 contain no reassignment of `wa_bseg` before the loop, so the work
area reaching line 477 is OVL/002 with `TXGRP = 010` as established.

### Everything verified except one step

| step | status |
|---|---|
| `wa_bseg` = OVL/002, TXGRP 010 | verified |
| READ it_bkpf finds the OVL row | verified (GLVOR SD00) |
| `lv_vbeln = wa_bkpf-AWKEY` = 0090002699 | verified |
| **`SELECT SINGLE knumv FROM vbrk WHERE vbeln = @lv_vbeln`** | **NOT verified** |
| `v_konv_cds` has KNUMV 0001172510 | verified (KAWRT 88,945.20) |
| `kposn = wa_bseg-TXGRP` -> 000010 = 000010 | verified |

The KNUMV `0001172510` was looked up directly in V_KONV_CDS; nothing has
confirmed that the *program's* VBRK read returns it.

### Leading hypothesis - CHAR(20) host variable against a CHAR(10) column

```abap
  DATA(lv_vbeln) = wa_bkpf-AWKEY.        " inline -> type AWKEY, CHAR(20)
  SELECT SINGLE knumv FROM vbrk INTO @DATA(lv_knumv)
    WHERE vbeln = @lv_vbeln.             " VBRK-VBELN is CHAR(10)
```

`lv_vbeln` inherits `AWKEY`, which is **CHAR(20)**, so it holds
`'0090002699'` followed by **ten trailing blanks**. `VBRK-VBELN` is CHAR(10).

On the classic databases under ECC, CHAR comparison is blank-padded and
trailing blanks are insignificant, so this matched. On **HANA**, ABAP CHAR maps
to NVARCHAR, where **trailing blanks are significant**. That makes the
comparison `'0090002699' = '0090002699          '` unequal, the SELECT SINGLE
returns `sy-subrc = 4`, the `IF sy-subrc = 0` guard fails, and the condition
read never runs.

This fits every observation:

- identical source, correct in ECC, zero in S/4
- **all** SD rows fail uniformly - every AWKEY is padded the same way
- everything either side of that one statement verifies clean
- the FI branch is untouched because it never goes near VBRK

### The test - 30 seconds

`SE16 -> VBRK`, `VBELN = 0090002699`. If it returns a row with
`KNUMV = 0001172510`, the data is present and the **statement** is what fails,
confirming the hypothesis.

### The fix

Type the variable to the column, not to AWKEY:

```abap
  DATA lv_vbeln TYPE vbrk-vbeln.         " CHAR(10)
  lv_vbeln = wa_bkpf-awkey(10).
  SELECT SINGLE knumv FROM vbrk INTO @lv_knumv
    WHERE vbeln = @lv_vbeln.
```

Replaces the inline `DATA(lv_vbeln) = wa_bkpf-AWKEY.` at line 462. Declare
`lv_vbeln` with the other locals in the form.

Caveat: this does not explain why round 3 passed on FY 2025-26 documents. That
run may have been a different system or program version - worth confirming which
system produced `new_output_1` before treating the two as one code base.

## Round 4i - full chain verified for 8324000008; every input is correct

Traced the yellow-highlighted row (EXPORT row 197) end to end.

**BKPF** - OVL / 8324000008 / 2024, BLART `RC`:
`GLVOR = SD00`, `AWKEY = 0090003288`, BUDAT 31.03.2025.

**VBRK** - VBELN `0090003288` (matches AWKEY exactly):
`FKART ZCR1`, `WAERK USD`, `VKORG OVL`, `KALSM OVLEXP`,
**`KNUMV = 0001173747`**.

**V_KONV_CDS** for KNUMV `0001173747` - 3 rows, all `KPOSN (Item) = 10`:

| Item | Step | Cntr | KSCHL | KAWRT | KBETR | G/L |
|---|---|---|---|---|---|---|
| 10 | 10 | 1 | ZSER | 10.00 | 0.00 | |
| 10 | 10 | 2 | ZSER | 10.00 | 55,627.32 | 230903 |
| 10 | 20 | 1 | **JOIG** | **55,627.32** | 0.00 | |

`JOIG KAWRT = 55,627.32` is exactly the Taxable / Gross / Total value ECC
reports for this row. Walking the loop by hand:
`txbval = 55,627.32`, `igstrt = 0.00/10 = 0`, `totval = 55,627.32 + 0 + 0 + 0`
= **55,627.32**. Correct.

### Every candidate is now eliminated on data

| step | evidence | verdict |
|---|---|---|
| (A) `glvor = 'SD00'` | BKPF shows SD00 | passes |
| (B) AWKEY -> VBRK -> KNUMV | 0090003288 -> 0001173747 | passes |
| (C) V_KONV_CDS returns rows | 3 rows, JOIG 55,627.32 | passes |
| (D) `kposn = TXGRP` | KPOSN 10, TXGRP 010 -> both 000010 | passes |

Two documents traced completely (8124000001 and 8324000008); both have every
input correct and both output zero. Static analysis is exhausted - nothing left
in the data explains it.

### Important caveat on the source

**The listing analysed throughout this document reads `FROM konv`. The mock2
system reads `FROM v_konv_cds`.** So the mock2 source is definitively *not* the
source in these PDFs, and there may be further differences in that version that
cannot be seen from here. Any further static reasoning is unsafe without the
actual mock2 source of `ZFI_GSTR1_SUB` as text.

### Remaining hypothesis and a low-risk test

```abap
  DATA(lv_vbeln) = wa_bkpf-AWKEY.     " inline -> AWKEY, CHAR(20)
  SELECT SINGLE knumv FROM vbrk ... WHERE vbeln = @lv_vbeln.
```

SE16 lookups succeed because the value is typed into a CHAR(10) field. The
program passes CHAR(20) - the document number plus ten trailing blanks - and on
HANA (CHAR maps to NVARCHAR) trailing blanks are significant, unlike the
blank-padded CHAR semantics of the ECC database.

Two-line change, no risk either way:

```abap
  DATA lv_vbeln TYPE vbrk-vbeln.      " CHAR(10)
  lv_vbeln = wa_bkpf-awkey(10).
```

If the hypothesis is right this fixes all 326 rows. If not, it changes nothing -
taking the first 10 characters of an AWKEY that already holds only a billing
document number is a no-op.

### Otherwise - runtime evidence is now required

ST05 SQL trace on a narrowed run (posting date 31.03.2025, company code OVL)
shows the literal values sent to VBRK and V_KONV_CDS and the rows returned.
A debugger session on the same selection answers it just as fast: check
`sy-subrc` and the **length** of `lv_vbeln` after the VBRK read.
