# ZFI_POST_ICE_TO_OVL_BAPI – duplicate posting fix

Issue (mail "ONGC Duplicate document", 29/30.09.2026): ONGC doc 8226000223 was
posted twice in OVL (8826004360 and 8826004365). The duplicate check in
`ZFIBKPF` did not stop the second posting.

## Root cause (original code in `ZFI001_NEW.zip`)

1. **Flag written only at end of run.** Every FI document is committed right after
   posting (`BAPI_TRANSACTION_COMMIT`), but `ZFI_UPDATE_ACT` (which writes the OVL
   doc no. to `ZFIBKPF`) runs only once, after the whole loop. During the run,
   documents already posted still look open in `ZFIBKPF`.
2. **No lock.** Two runs at the same time (two users, or a job and a user) both pass
   the check and both post. Doc numbers 4360 and 4365 being only 5 apart fits this.
3. **Errors ignored.** If `ZFI_UPDATE_ACT` fails, or the run dumps or times out, the
   FI documents stay posted but `ZFIBKPF` is never updated, so the next run posts
   them again.
4. **Stale check.** `FM_GET_DATA_ICE` checks `TYPE NE 'E'` once at the start from
   data it already loaded. It does not check `ZBELNR`, and it does not re-read the
   database just before posting.
5. **Last run overwrites.** `ZFI_UPDATE_ACT` uses `MODIFY ZFIBKPF FROM TABLE` and the
   key is the ONGC doc, so when two runs post the same doc, the last one to finish
   overwrites `ZBELNR`. `ZFIBKPF` then shows only one of the two OVL documents.
   The row was also rebuilt with `MOVE-CORRESPONDING` from `BKPF`, which cleared any
   `ZFIBKPF` field that doesn't exist in `BKPF`.

## Changes (`ZFI_POST_ICE_TO_OVL_BAPI_F01` / `_TOP`)

| # | Change |
|---|--------|
| 1 | Each ONGC doc is locked with the standard generic lock `ENQUEUE_E_TABLE` (TABNAME `ZFIBKPF`, VARKEY = MANDT+BUKRS+BELNR+GJAHR, `_SCOPE = '1'`). No new lock object is needed. If the doc is locked, it is skipped and the ALV shows who holds the lock. |
| 2 | Under the lock, `ZFIBKPF` is re-read (`BYPASSING BUFFER`). If `ZBELNR` is filled: "Document is already posted", not posted. |
| 3 | On posting success, `ZFI_UPDATE_ACT` is called for **that document** before `BAPI_TRANSACTION_COMMIT`. If the Z update fails, `BAPI_TRANSACTION_ROLLBACK` runs, so no FI document is posted without its `ZFIBKPF` entry. |
| 4 | The result of `BAPI_TRANSACTION_COMMIT` (WAIT = 'X') is checked. On an FI update termination (SM13), the row is set back to `E`. |
| 5 | Error status is also written per document, under the lock (new form `FM_SAVE_ERROR`). The old single `ZFI_UPDATE_ACT` call at the end of the run is removed. |
| 6 | `ZFIBKPF` and `ZFI_BSEG` rows are built from the DB row, so their Z fields are kept. |
| 7 | `FM_GET_DATA_ICE`: a doc is also skipped when `ZBELNR IS NOT INITIAL`. |
| 8 | `FM_UPLOAD_DATA`: no `FOR ALL ENTRIES` on an empty `ZFIBKPF` result (the old code read all of `ZFI_BSEG`). |
| 9 | `_TOP`: new type `tt_zfi_bseg`. |

Main program and `_E01` are unchanged (copied for completeness).

## Before transport – please check

- `ZFI_TDS_MAIL` (called at the end of `ZFI_UPDATE_ACT`) must not do its own
  `COMMIT WORK`. If it does, the FI document is committed at that point. This is still
  safe against duplicates, because `ZFIBKPF` is already written, but the rollback in
  step 3 can then no longer undo the posting.
- Any **other program** posting from `ZFIBKPF` (for example the old
  `ZFI_POST_ICE_TO_OVL`, whose includes are commented out in the main program) must
  use the same `ENQUEUE_E_TABLE` lock and `ZBELNR` check, or be retired. Use
  where-used on `ZFIBKPF` and `ZFI_UPDATE_ACT`.
- Test: run the program in two sessions at the same time on the same ONGC doc range.
  Only one posting should happen; the other session should show "being processed by"
  or "already posted".

## Data correction (functional)

Reverse 8826004365 (FB08) after Pre-Audit confirms, then make sure `ZFIBKPF` and
`ZFI_BSEG` for ONGC 8226000223 point to 8826004360.
