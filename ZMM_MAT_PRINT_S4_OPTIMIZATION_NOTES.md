# ZMM_MAT_PRINT → ZMM_MAT_PRINT_S4 — S/4HANA Optimization Notes

## Scope

`ZMM_MAT_PRINT` (Material Document Print, smartform `ZMM_MATERIAL`) was analyzed from
`ZMM_MAT_PRINT.pdf` / `ZMM_MAT_PRINT_1.pdf` and rebuilt as the new program
**`ZMM_MAT_PRINT_S4`** (file `ZMM_MAT_PRINT_S4.abap`) following S/4HANA standards.
Successor APIs were taken from `ARS_API_SUCCESSOR (1).xlsx`; CDS element ↔ base-field
mapping was verified against `DDLS_BASE_FIELDS (1).txt`.

## Why ZMKPF had to go

`ZMKPF` is a custom DDIC **database view** joining `MKPF + MSEG + T001W`
(fields: MBLNR, MJAHR, BLDAT, BUDAT, XBLNR, BKTXT, WERKS, BUKRS, KOSTL, CPUDT,
CPUTM, USNAM, NAME1). In S/4HANA, `MKPF`/`MSEG` are **compatibility (proxy) views**
on the single table `MATDOC`. A classic DB view on top of proxy views is obsolete,
performs poorly and blocks the S/4 conversion. It is replaced by released CDS views
that read `MATDOC` directly.

## Data source mapping (old → new)

| Old source (ZMM_MAT_PRINT) | New source (ZMM_MAT_PRINT_S4) | Fields used (old → new element) |
|---|---|---|
| `ZMKPF` (MKPF part) | `I_MaterialDocumentHeader_2` | MBLNR→MaterialDocument, MJAHR→MaterialDocumentYear, BLDAT→DocumentDate, BUDAT→PostingDate, XBLNR→ReferenceDocument, BKTXT→MaterialDocumentHeaderText, CPUDT→CreationDate, CPUTM→CreationTime, USNAM→CreatedByUser |
| `ZMKPF` (MSEG part) + both `MSEG` selects | `I_MaterialDocumentItem_2` | WERKS→Plant, BUKRS→CompanyCode, KOSTL→CostCenter, ZEILE→MaterialDocumentItem, BWART→GoodsMovementType, MATNR→Material, LGORT→StorageLocation, CHARG→Batch, LIFNR→Supplier, MENGE→QuantityInBaseUnit, SHKZG→DebitCreditCode, WAERS→CompanyCodeCurrency, DMBTR→TotalGoodsMvtAmtInCCCrcy, MEINS→MaterialBaseUnit, SGTXT→MaterialDocumentItemText, GRUND→GoodsMovementReasonCode, VFDAT→ShelfLifeExpirationDate, HSDAT→ManufactureDate |
| `ZMKPF` (T001W part) | `I_Plant` | NAME1→PlantName |
| `T001` | `I_CompanyCode` (joined into the header select — one roundtrip saved) | BUKRS→CompanyCode, BUTXT→CompanyCodeName |
| `LFA1` (3 selects, one of them inside a loop) | `I_Supplier` (2 buffered selects, none in a loop) | LIFNR→Supplier, NAME1→OrganizationBPName1 |
| `QALS` | `I_InspectionLot` **joined with `QALS`** | ENSTEHDAT→InspLotCreatedOnLocalDate, HERSTELLER→Manufacturer, MATNR→Material, CHARG→Batch, MJAHR→MaterialDocumentYear, MBLNR→MaterialDocument. `AR_NO` is a **customer append field** on QALS and is not part of the released view — it is joined from `QALS` via `InspectionLot = PRUEFLOS`. |
| `MAKT` | `I_ProductDescription` | MATNR→Product, MAKTX→ProductDescription |
| `T001L` | `I_StorageLocation` | LGORT→StorageLocation, LGOBE→StorageLocationName |
| `USER_ADDRP` | kept (new ABAP SQL) — no released successor in the API list | BNAME, NAME_TEXT |
| `T156T` / `T157E` | kept (new ABAP SQL) — no released successor in the API list | movement type / reason texts |
| FM `VB_BATCH_GET_DETAIL` | kept — still the supported API for reading batch classification (characteristics `ZMANUFACTURER_BATCH`, `ZRETEST_DATE`, `ZMANUFACTURER`) in S/4HANA on-premise | — |
| FM `SSF_FUNCTION_MODULE_NAME` + generated smartform FM | kept — Smart Forms remain valid print technology on-premise | — |

## Code modernization

- Includes `ZDEC2`, `ZSEL12`, `ZFETCH12`, `ZCALL2` merged into **one class-based report**
  (`lcl_mat_doc_print` with dedicated methods per step).
- New ABAP SQL throughout: comma-separated `FIELDS` lists, host variables (`@`),
  `ORDER BY` pushed to the database.
- `READ TABLE ... WITH KEY` replaced by table expressions
  (`VALUE #( itab[ ... ] OPTIONAL/DEFAULT )`), `MODIFY ... TRANSPORTING` loops removed.
- The **`SELECT SINGLE` on LFA1 inside the item loop** (N+1 problem) is eliminated —
  manufacturer names are buffered once via `I_Supplier`.
- Company code text merged into the header select (one DB roundtrip instead of two).
- Authority check `M_MSEG_WMB` executed **once per plant** (`GROUP BY`) instead of once
  per document.
- Defensive fixes: division-by-zero guard for the rate (`DMBTR / MENGE`),
  proper message output for smartform errors (old code had an empty `IF sy-subrc <> 0`),
  "no data found" message.
- Selection screen names (`S_MBLNR1`, `S_MJAHR1`) kept identical; year default is now
  the current year instead of hard-coded `2015`.

## Performance tuning of the item loop (build_items)

The runtime driver of the old program was inside the item loop. Fixes applied:

1. **Bulk prefetch of batch classification** (`prefetch_batch_classification`):
   the three characteristics `ZMANUFACTURER_BATCH`, `ZRETEST_DATE`, `ZMANUFACTURER`
   are read for ALL batches of the selection in 4 set-based selects
   (CABN → MCH1/MCHA `CUOBJ_BM` → AUSP) before the loop starts.
   `VB_BATCH_GET_DETAIL` (previously called once per item row!) only remains as a
   fallback for batches not found by the prefetch, and its result is cached per
   material/batch. Items without a batch skip classification completely.
2. **Keyed lookup tables**: all text/lookup buffers are `SORTED`/`HASHED` tables,
   so every read inside the loop is a binary/hash key access instead of a full scan.
3. **AR-number lookup**: `gt_qals` is keyed by `matnr, charg, enstehdat` — the kernel
   binary-searches to the batch's lots, walks them in ascending date order and stops
   at the posting date; the last visited row is the latest lot (no comparisons).
4. **No SELECT inside the loop**: manufacturer names that only become known through
   classification are resolved with one select after the loop
   (`complete_manufacturer_names`) — the old program did a `SELECT SINGLE` on LFA1
   per item row.

## Follow-up actions in the SAP system

1. Create program `ZMM_MAT_PRINT_S4` from `ZMM_MAT_PRINT_S4.abap` (package `ZMM`),
   assign the transaction code that pointed to `ZMM_MAT_PRINT`.
2. The smartform `ZMM_MATERIAL` interface currently types `TA_HEADER` with the view
   structure `ZMKPF`. Before deleting the view, create a plain DDIC structure (e.g.
   `ZSMM_MATDOC_HDR`) with the same fields (MBLNR, MJAHR, BLDAT, BUDAT, XBLNR, BKTXT,
   WERKS, BUKRS, KOSTL, CPUDT, CPUTM, USNAM, NAME1 [+ BUTXT in the form logic]) and
   retype the form parameter. `ZMSEG2` / `ZNAMES` are plain structures and stay.
3. After the smartform is retyped and `ZMM_MAT_PRINT` is retired, **delete the DDIC view
   `ZMKPF`** (where-used check first).
4. Run ATC (S/4HANA readiness variant) on `ZMM_MAT_PRINT_S4` — no ZMKPF finding will
   remain; `T157E` keeps the `#EC CI_NOORDER` pseudo comment as in the remediated original.
