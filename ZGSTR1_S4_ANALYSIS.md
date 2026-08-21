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
