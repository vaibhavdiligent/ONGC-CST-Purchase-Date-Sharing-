**Subject:** Vendor mass upload - clarifications needed on Block/Unblock and Partner Function

Dear Ashish / Pratima / Harshada,

Need a few clarifications before the next round of testing.

---

## Block / Unblock

**1. Vendor 362243 - which block is required?**

The row has `SPERR`, `SPERR_1`, `SPERM`, `SPERM_1` and `SPERQ = 99` all filled
together. These are two different ways of blocking and they cannot both apply:

* `SPERR` + `SPERM` + `SPERQ` - block at vendor level, across all company codes
  and all purchasing organisations
* `SPERR_1` + `SPERM_1` - block only in the company code / purchasing
  organisation given in the row

Which one is required for this vendor?

**2. `SPERQ` together with a company code block**

The template says `SPERQ` must be blank when the block is at company code or
purchasing organisation level. Should this stay a hard check, or should `SPERQ`
be allowed along with `SPERR_1`?

**3. Employee codes**

An employee code has no purchasing organisation, so `SPERM_1` cannot be used for
it. Where `SPERM_1` is filled and no purchasing organisation is given, the
purchasing block is applied centrally and the row posts with a warning. Is that
acceptable, or should the row be rejected so the file is corrected?

**4. How should an unblock be given in the file?**

A blank cell means "leave this indicator as it is" - it cannot mean "remove the
block", otherwise no row could set only some of the flags. At present there is no
way in the template to remove a block.

Proposal: type `UNBLOCK` in the cell to clear that indicator. Please confirm, or
suggest another marker. Also please include unblock rows in the next test file -
none have been tested so far.

---

## Partner Function

**1. Partner 100098685 is not created for purchasing organisation 1000**

All rows fail with `F2 165 - Vendor 0100098685 has not been created for purchasing
organization 1000`. The partner exists as a supplier but has no purchasing
organisation 1000 view, and a partner function cannot be stored without it.

Please extend 100098685 to purchasing organisation 1000, or give another partner
number to use for sandbox testing.

**2. Is the number in `GPARN` a vendor number or a BP number?**

Both are accepted and a BP number is converted to its vendor. 100098685 is being
read as a vendor number. Please confirm that is correct.

**3. Which partner functions are valid for vendors?**

Only `ZP` has been used so far. Partner functions are checked against `TPAR`,
which contains the functions of all partner types - customer, vendor, personnel,
contact. A customer side function typed into this tab would pass that check and
then fail in SAP with an unclear message.

Please share the list of partner functions valid for vendors, so a wrong entry can
be rejected with a proper message.

**4. Existing partner functions not listed in the file - keep or delete?**

SAP treats this segment as complete data: anything not sent is deleted. At present
the existing functions are kept, so a file adding `ZP` does not disturb the
functions created by the account group. Please confirm this is correct.

**5. Columns `D0320` and `USE_ZAV`**

These are screen control fields from the old recording and are being ignored.
Please confirm.

---

Regards,
