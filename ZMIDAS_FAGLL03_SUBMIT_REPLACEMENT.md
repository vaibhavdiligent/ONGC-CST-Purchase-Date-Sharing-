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
| pswbt / pswsl | BSEG‑PSWBT / PSWSL | I_OperationalAcctgDocItem (see note (a) in the .abap) |
| mwskz | ACDOCA‑MWSKZ | TaxCode |
| augbl | ACDOCA‑AUGBL | ClearingAccountingDocument |
| prctr | ACDOCA‑PRCTR | ProfitCenter |
| kostl | ACDOCA‑RCNTR | CostCenter |
| aufnr | ACDOCA‑AUFNR | OrderID |
| segment | ACDOCA‑SEGMENT | Segment |
| sgtxt | ACDOCA‑SGTXT | DocumentItemText |
| koart | ACDOCA‑KOART | FinancialAccountType |
| gkont / gkart | ACDOCA‑GKONT / GKART | OffsettingAccount / OffsettingAccountType |
| rebzg | ACDOCA‑REBZG | InvoiceReference |
| kidno | ACDOCA‑KIDNO | PaymentReference |
| buzid | ACDOCA‑BUZID | AccountingDocumentItemType |
| qbshb | BSEG‑QBSHB | see note (b) in the .abap |
| u_zuonr | BSEG‑ZUONR | AssignmentReference |
| u_lifnr | BSEG‑LIFNR | Supplier |
| u_ebeln | BSEG‑EBELN | PurchasingDocument |
| xblnr | BKPF‑XBLNR | I_JournalEntry‑DocumentReferenceID |
| u_xblnr_alt | BKPF‑XBLNR_ALT | AlternativeReferenceDocument |
| u_usnam | BKPF‑USNAM | AccountingDocCreatedByUser |
| jamon | derived `budat(4)/budat+4(2)` | computed in the form |
| zzgkont_ltxt | BAdI FAGL_ITEMS_CH_DATA | BAdI called in the form |

### If you must keep a SUBMIT (not recommended)

Replace the ALV capture by the browser's own download path:
`WITH p_appl = 'FAGLL03H_GLV' p_ledger = '0L' s_ccode IN r_bukrs
s_racct IN s_saknr p_ai = 'X' s_pdate IN s_budat psx_item = <rsds_type on
GLPOS_N_GL_CT: BELNR/GJAHR> p_outfi = 'X' p_dappl = 'X' p_dfile = <AL11 file>
p_outli = ' '` and then parse the file back. That gives a layout‑dependent
column set in `V_GLPOS_N_GL_CT` names and still needs a mapping to
`ty_fagll03` – strictly more work and more fragile than the direct read.

### Validation

Run old and new for one `p_erdat`/company code and compare
`lines( ta_fagll03 )` and `SUM( dmshb )` grouped by `bukrs hkont`
before removing the SUBMIT block.
