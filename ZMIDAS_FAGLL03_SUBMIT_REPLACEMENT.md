# ZMIDAS_FAGLL03 – replacing `SUBMIT fagl_account_items_gl` (note 3185840)

Source analysed: `ZMIDAS_FAGLL03_2.pdf`, `FAGL_ACCOUNT_ITEMS_GL_2.pdf`,
`FAGLL03H_2.pdf` (+ the `_1` documentation PDFs).

## What the current code does (ZMIDAS_FAGLL03_FORMS, lines 623‑767)

```
cl_salv_bs_runtime_info=>set( display = abap_false  data = abap_true )
SUBMIT fagl_account_items_gl
       WITH sd_saknr IN s_saknr      WITH sd_bukrs IN r_bukrs
       WITH x_opsel = ' ' WITH x_clsel = ' ' WITH x_aisel = 'X'
       WITH so_budat IN s_budat      WITH rldnr = '0L'
       WITH fs_dyns = lwa_free       " FAGLFLEXA_FS-DOCNR / RYEAR, 500 docs per call
       AND RETURN.
cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = o_ta_data )
MOVE-CORRESPONDING <fs_data> TO ta_fagll03_temp.   " ty_fagll03 = FAGLPOSX field names
```

So the program is not interested in FAGLL03's *layout* at all – it only
uses the ALV's internal data table, which is typed `FAGLPOSX` (plus the
custom append field `ZZGKONT_LTXT`).

## Why `SUBMIT FAGL_LINE_ITEM_BROWSER` is not a 1:1 swap

| | FAGL_ACCOUNT_ITEMS_GL (FAGLL03) | FAGL_LINE_ITEM_BROWSER (FAGLL03H) |
|---|---|---|
| Output structure | `FAGLPOSX` (+ `CI_` append → `ZZGKONT_LTXT`) | view `V_GLPOS_N_GL_CT` (GL view) – different names, no custom append |
| Display | `REUSE_ALV_GRID` → `cl_salv_bs_runtime_info` works | `PIVB_SELECT_AND_DISPLAY` (PIVB/KESS framework) → `cl_salv_bs_runtime_info=>get_data_ref` returns **nothing** |
| Columns returned | full structure | only the fields of the chosen layout (`p_layout`), aggregated |
| Account | `sd_saknr` | `s_racct` |
| Company code | `sd_bukrs` | `s_ccode` |
| Ledger | `rldnr` | `p_ledger` |
| Status | `x_opsel/x_clsel/x_aisel` | `p_oi/p_ci/p_ai` |
| Posting date | `so_budat` | `s_pdate` |
| Free selection | `fs_dyns` (RSDS_TYPE, table `FAGLFLEXA_FS`) | `psx_item` (RSDS_TYPE, table = selection view `GLPOS_N_GL_CT`) |
| Offsetting acct | `GKONT/GKART` (from ACDOCA, note 2588267) + BAdI `FAGL_ITEMS_CH_DATA` | `GKONT` only if in layout; BAdI `FAGL_LIB` instead |

Even if the parameters are translated, the data cannot be captured the
way it is captured today, and the custom column `ZZGKONT_LTXT` (filled by
the `FAGL_ITEMS_CH_DATA` implementation) would be lost.

## Recommended change

Do not SUBMIT any report. Read the same rows FAGLL03 read, directly, into
`ty_fagll03`, then run the same BAdI. See
`ZMIDAS_FAGLL03_SUBMIT_REPLACEMENT.abap` (form `get_gl_items_direct`).

What FAGLL03 returned for this call = ACDOCA rows with
`RLDNR = '0L'`, `RBUKRS/BELNR/GJAHR` ∈ documents of the current year
package, `RACCT IN s_saknr`, `BUDAT IN s_budat`, all items
(open + cleared), merged with BSEG/BKPF for entry‑view fields.
Doc‑splitting multiplicity (several ACDOCA lines per BSEG item) is the
same as in the G/L view of FAGLL03, so the post‑filter against
`ta_bseg_filter` (CR#8000003569) keeps working unchanged.

Benefits: no 500‑document chunking loop, no `FREE_SELECTIONS_RANGE_2_*`
calls, no dependency on ALV runtime capture, no dialog/background
difference, and no note‑3185840 finding.

### Field mapping `ty_fagll03` (FAGLPOSX) → source

| ty_fagll03 | Classic source | Released CDS element used in the form |
|---|---|---|
| bukrs | ACDOCA‑RBUKRS | I_JournalEntryItem‑CompanyCode |
| belnr | ACDOCA‑BELNR | AccountingDocument |
| gjahr | ACDOCA‑GJAHR | FiscalYear |
| buzei | ACDOCA‑BUZEI (entry‑view item) | AccountingDocumentItem |
| hkont / konto | ACDOCA‑RACCT | GLAccount |
| blart | ACDOCA‑BLART | AccountingDocumentType |
| bldat | ACDOCA‑BLDAT | DocumentDate |
| budat | ACDOCA‑BUDAT | PostingDate |
| bschl | ACDOCA‑BSCHL | PostingKey |
| shkzg | ACDOCA‑DRCRK | DebitCreditCode |
| dmshb | ACDOCA‑HSL (signed) | AmountInCompanyCodeCurrency |
| hwaer | ACDOCA‑RHCUR | CompanyCodeCurrency |
| wrshb | ACDOCA‑WSL (signed) | AmountInTransactionCurrency |
| hwae2 | ACDOCA‑RKCUR | GlobalCurrency |
| pswbt / pswsl | BSEG‑PSWBT (absolute) / PSWSL | I_OperationalAcctgDocItem‑AbsltAmtInBalanceTransacCrcy / BalanceTransactionCurrency |
| mwskz | ACDOCA‑MWSKZ | TaxCode |
| augbl | ACDOCA‑AUGBL | ClearingAccountingDocument |
| prctr | ACDOCA‑PRCTR | ProfitCenter |
| kostl | ACDOCA‑RCNTR | CostCenter |
| aufnr | ACDOCA‑AUFNR | OrderID |
| segment | ACDOCA‑SEGMENT | Segment |
| sgtxt | ACDOCA‑SGTXT | DocumentItemText |
| koart | ACDOCA‑KOART | FinancialAccountType |
| gkont / gkart | ACDOCA‑GKONT / GKOAR | OffsettingAccount / OffsettingAccountType |
| rebzg | ACDOCA‑REBZG | InvoiceReference |
| kidno | BSEG‑KIDNO | I_OperationalAcctgDocItem‑PaymentReference |
| buzid | BSEG‑BUZID | I_OperationalAcctgDocItem‑AccountingDocumentItemType |
| qbshb | BSEG‑QBSHB (signed) | I_OperationalAcctgDocItem‑WithholdingTaxAmount |
| u_zuonr | BSEG‑ZUONR | AssignmentReference |
| u_lifnr | BSEG‑LIFNR | Supplier |
| u_ebeln | BSEG‑EBELN | PurchasingDocument |
| xblnr | BKPF‑XBLNR | I_JournalEntry‑DocumentReferenceID |
| u_xblnr_alt | BKPF‑XBLNR_ALT | AlternativeReferenceDocument |
| u_usnam | BKPF‑USNAM | AccountingDocCreatedByUser |
| jamon | fiscal year / posting period (`GJAHR/POPER`) | computed in the form from FiscalYear + FiscalPeriod |

### Row filters applied (identical to FAGLL03 for the old call)

FAGLL03 with `x_norm='X'`, `x_glyec=' '`, `x_enfoj=' '` selects
`BSTAT IN (' ', 'L', gl_only)` and `check_item_ok` requires `UMSKZ = space`.
The form applies both (`AccountingDocumentCategory IN lt_bstat`,
`SpecialGLCode = space`), plus ledger `0L`, document key, `GLAccount IN
s_saknr`, `PostingDate IN s_budat`.

### Why the old SUBMIT lost data in S/4 (CRS incident, Dec 2025)

S/4 `FAGL_ACCOUNT_ITEMS_GL` converts the passed free selection from
`FAGLFLEXA_FS` to `ACDOCA_FS` and **deletes every field not in ACDOCA_FS**.
ACDOCA has no `DOCNR`, so the 500‑document restriction was silently dropped;
FAGLL03 ran unrestricted once per chunk, which leads to memory dumps
(`TSV_TNEW_PAGE_ALLOC_FAILED`, note 3185840) or silently caught capture
failures → missing days. The direct read has no free selection and no SUBMIT.
| zzgkont_ltxt | BAdI FAGL_ITEMS_CH_DATA | BAdI called in the form |

### If you must keep a SUBMIT (not recommended)

Replace the ALV capture by the browser's own download path:
`WITH p_appl = 'FAGLL03H_GLV' p_ledger = '0L' s_ccode IN r_bukrs
s_racct IN s_saknr p_ai = 'X' s_pdate IN s_budat psx_item = <rsds_type on
GLPOS_N_GL_CT: BELNR/GJAHR> p_outfi = 'X' p_dappl = 'X' p_dfile = <AL11 file>
p_outli = ' '` and then parse the file back. That gives a layout‑dependent
column set in `V_GLPOS_N_GL_CT` names and still needs a mapping to
`ty_fagll03` – strictly more work and more fragile than the direct read.

All CDS element names above were verified against the system export
`DDLS_BASE_FIELDS.txt` in this repository (element → base table/field).

### Validation

Run old and new for one `p_erdat`/company code and compare
`lines( ta_fagll03 )` and `SUM( dmshb )` grouped by `bukrs hkont`
before removing the SUBMIT block.

## SAP notes reviewed (full text, 30.10.2026)

| Note | What it says | Consequence for ZMIDAS |
|---|---|---|
| **3185840** (v4, 12.10.2026) | In S/4 FAGLL03 reads ACDOCA+BKPF+BSEG generically, merges and converts every field into one internal table before the ALV (`SAPLFAGL_ITEMS_SELECT`, `CONVERT_DATA_WITH_BSEG`); dumps even for 1 day / 1 CoCd / 1 account. **Resolution: use FAGLL03H, which selects directly and optimised against ACDOCA.** | SUBMIT of FAGLL03 must go. For a program, "optimised selection directly against ACDOCA" = `FORM get_gl_items_direct`. |
| **3533517** (v4, 14.07.2025) | S/4 FAGLL03 takes its dynamic-selection fields from `FAGL_GET_REPORTING_FIELDS` (`FINSV_ACDOC_FCT` / `FINS_FCT`); fields not in that catalogue are deleted (MSITEM103). | Confirms that free selections passed with FAGLFLEXA field names (`DOCNR`/`RYEAR`) are not supported in S/4 – the silent drop found in the code. |
| **2806316** (v3, 11.04.2024) | For **migrated** documents (`ACDOCA-MIG_SOURCE <> space`) `ACDOCA-GKONT` is filled only if the document is CO-relevant and COEP had GKONT. Empty GKONT otherwise is **expected behaviour**; the post-migration activity fills BSEG-GKONT only. | In CRS, December-2025 documents are migrated: GKONT-based tagging (ZFI_PROF_FPNA_OF, ZMIDAS_EU_TAG, CoCd 4500, final vendor) can differ from ECC production for those rows (7,969 rows with empty GKONT in the CRS dump). Not a program error; applies equally to S/4 FAGLL03 (note 2588267). Post go-live documents carry GKONT from posting. |
