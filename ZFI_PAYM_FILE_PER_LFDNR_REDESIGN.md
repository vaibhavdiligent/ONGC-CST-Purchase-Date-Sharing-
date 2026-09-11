# Per-LFDNR redesign — send every DME file of a run (not just the last)

## Problem
A payment run can create several DME files (one `REGUT` row per `LFDNR`, each its
own `FSNAM`). `ZFI_PAYM_FILE` is keyed **`MANDT+LAUFD+LAUFI`** only, so the
enhancement that stores the file overwrites the same row once per file — only the
**last** file survives, and only that one reaches CPI. The other batches show
`SENT='X'` (shared row) but never actually went.

Fix: make `ZFI_PAYM_FILE` and the whole send/track path **per file (LFDNR)**.

---

## Item 1 — Dictionary: `ZFI_PAYM_FILE` (do this first; it's the enabler)
Add `LFDNR` to the table **as a key field**:

| Key | Field | Data element | Note |
|-----|-------|--------------|------|
| X | MANDT | MANDT | |
| X | LAUFD | LAUFD | |
| X | LAUFI | LAUFI | |
| **X** | **LFDNR** | **LFDNU** (same as `REGUT-LFDNR`) | **new key field** |

- Activate. Existing rows get `LFDNR = '000'` (blank) — acceptable, they are historic.
- Everything below assumes this field exists.

---

## Item 2 — Enhancement `ZBCM4` (writes `ZFI_PAYM_FILE`)
Today it sets `LAUFD/LAUFI/FILE_NAME/RAW_DATA` and `MODIFY zfi_paym_file` (keyed
run-level → overwrite). Add the file's real `LFDNR`.

The reliable source is `REGUT`, matched by the file name just written (unique per
file). After the block that fills `wa_line-char` (the name) and before the
`MODIFY`, add:

```abap
DATA: lv_lfdnr TYPE regut-lfdnr,
      lv_fsnam TYPE regut-fsnam.
lv_fsnam = wa_line-char.          "name as built (confirm vs REGUT-FSNAM format:
                                  " with/without '.txt', with/without path)
SELECT SINGLE lfdnr INTO lv_lfdnr FROM regut
   WHERE laufd = <fs_date1>
     AND laufi = <fs_iden>
     AND fsnam = lv_fsnam.
IF sy-subrc = 0.
  wa_zfi_paym_file-lfdnr = lv_lfdnr.
ENDIF.
```
Then keep `MODIFY zfi_paym_file FROM wa_zfi_paym_file.` — now it inserts one row
per file (key includes `LFDNR`).

**UAT verify:** confirm `REGUT-FSNAM` equals `wa_line-char` exactly (the exit strips
path + `.txt`; `FSNAM` may keep `.txt`). If they differ, match on the stored form
(e.g. `fsnam = <name>.txt`) or select by `FSNAM CS lv_fsnam`. If no reliable match,
fall back to `SELECT MAX( lfdnr )` for that run key **after** the row is written.

---

## Item 3 — `ZFI_BNK_APP1_TOP` and `ZFI_BNK_APP_TOP` (`ty_final`)
Add the REGUT parts as real fields so the send can key on them directly (today they
live only inside the 41-char `GUID`):

```abap
lfdnr TYPE regut-lfdnr,
banks TYPE regut-banks,
dtkey TYPE regut-dtkey,
```
`MOVE-CORRESPONDING gs_batch_header TO gs_final` already fills them (field names
match `REGUT`).

---

## Item 4 — `ZFI_BNK_APP1_I01` APPROVE2 (the send)
Two changes:

**(a) Match the file per LFDNR** (currently `LAUFD/LAUFI` only, ~line 434):
```abap
READ TABLE gt_paym2 INTO gs_paym2 WITH KEY laufd = gs_final2-laufd
                                           laufi = gs_final2-laufi
                                           lfdnr = gs_final2-lfdnr.
```
The `MODIFY zfi_paym_file FROM gs_paym2` after send now updates that one file's row.

**(b) Send per file, not per run.** The 27/08 logic waits for the WHOLE run
(`F_RUN_PENDING_COUNT` by `LAUFD/LAUFI`) then sends one file. Change it to send
**this batch's** file once **this batch (LFDNR)** carries both L1 and L2:
- Either pass `LFDNR` into `F_RUN_PENDING_COUNT` and count only that batch's
  signatures, or add a small `F_BATCH_FULLY_SIGNED` check for `gs_final2-guid`
  (its L1 + L2 rows in `ZFI_BATCH_SIGN` both `DIGITL_SIGN='X'`).
- On "fully signed", send `gs_paym2` (the LFDNR file) exactly as now.
Each batch approval then transmits its own file; a run with 3 files sends 3 files.

---

## Item 5 — `ZFI_BNK_APP1_F01` (Tab-1/2/3 prep)
Every `READ TABLE gt_paym… WITH KEY laufd/laufi` inside the per-batch loops
(≈ lines 40–44 use of key is fine; the paym reads at ~144, ~535 and the Tab-3
prep) must add `lfdnr = gs_batch_header{,2,3}-lfdnr`, e.g.:
```abap
READ TABLE gt_paym2 INTO gs_paym2 WITH KEY laufd = gs_batch_header2-laufd
                                           laufi = gs_batch_header2-laufi
                                           lfdnr = gs_batch_header2-lfdnr.
```
`SENT / SENT_ERROR / RECEIVED` are now per file — the worklist filters already work
per batch via `DIGITL_SIGN`/`SNRO`, so no extra filter change needed.

---

## Item 6 — `ZFI_BNK_APP` (`ZFI_BNK_APP_F`)
Same `LFDNR` addition on every `ZFI_PAYM_FILE` read:
- `F_PREPARE_OP_TAB` (~line 83): `READ TABLE gt_paym … laufd/laufi` → add `lfdnr = gs_batch_header-lfdnr`.
- `F_RESENT` (~line 736): `READ TABLE gt_paym … laufd/laufi` → add `lfdnr = gs_final-lfdnr` (needs `ty_final-lfdnr` from Item 3).
- `DOWNLOAD_REC_DATA` / other paym reads: same.
Result: each grid line resends/downloads **its own** file, not the run's single row.

---

## Item 7 — Inbound return-file interface (`ZII_SIIA_BANK_RETURN_FILE_RE`)
With several files per run, `LAUFD/LAUFI` alone can't pick the right row on the
return. Match by the **full file name**, which is unique per file:
```abap
SELECT SINGLE * FROM zfi_paym_file INTO ls_paym_file
   WHERE laufd = lv_datum
     AND laufi = <status+19(6)>
     AND file_name = <returned file name from the Status/return payload>.
```
(The returned `Status` echoes the sent file name; strip to the same form stored in
`FILE_NAME`.) Set `RECEIVED` on that specific row. Per-payment `ZFI_BCM_PAYORDR`
posting via `ZFI_BCMFM_SBIFILE` is by `PYORD` and is unaffected.

---

## Item 8 — Monitor (`ZFI_BNK_APRV_MON`) and payrep (`ZFI_BCM_APP_PAYREP`)
Both read `ZFI_PAYM_FILE` by `LAUFD_M/LAUFI_M`. Two sub-cases:
- **Received** — monitor and payrep already take it from `ZFI_BCM_PAYORDR-ZSTATUS`
  (per payment); unaffected.
- **Sent** — the `ZFI_PAYM_FILE-SENT` read now returns multiple rows per run (one
  per LFDNR). Decide the grain: if any file of the run is sent, treat the run as
  sent, or (better) map each row to its batch. For the monitor's REGUHM grain,
  "sent" = at least one file of the medium run has `SENT='X'`. Adjust the read to
  loop rows instead of a single `READ`.

---

## Rollout order
1. Item 1 (dictionary) — must be active first.
2. Item 2 (enhancement) — so new runs store one row per file.
3. Items 3–6 (app1 + app) — sign/send/resend per file.
4. Item 7 (inbound) — match returns per file.
5. Item 8 (reports) — display per file.

## Must-verify in UAT
- Item 2: `LFDNR` correctly resolved for each file (the `REGUT-FSNAM` match).
- A run with 2+ files: all rows persist in `ZFI_PAYM_FILE`, each is signed and sent
  separately, all reach CPI, and each return marks the correct row received.
- The `ZFI_BATCH_SIGN` rows created in `ZFI_PAYMEDIUM_DMEE_20` use the **same**
  `BATCH_NO` (incl. `LFDNR`, and real `DTKEY`/`BANKS`) that these programs build,
  or the lookups miss.
