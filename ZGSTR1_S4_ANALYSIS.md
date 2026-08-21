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
