*&---------------------------------------------------------------------*
*& ZMIDAS_FAGLL03 - replacement for
*&     SUBMIT fagl_account_items_gl ... AND RETURN  (FAGLL03)
*&     + cl_salv_bs_runtime_info=>get_data_ref
*& (include ZMIDAS_FAGLL03_FORMS, lines ~709-756, CR#8000003569 block)
*&
*& Why not SUBMIT FAGL_LINE_ITEM_BROWSER (FAGLL03H) instead?
*&   - FAGLL03H does not render through CL_SALV_TABLE/REUSE_ALV; it
*&     calls PIVB_SELECT_AND_DISPLAY (KESS/PIVB framework), so
*&     cl_salv_bs_runtime_info=>get_data_ref returns nothing.
*&   - Its result rows are typed by view V_GLPOS_N_GL_CT (GL view) and
*&     the columns actually filled depend on the saved layout, so a
*&     MOVE-CORRESPONDING to TY_FAGLL03 (FAGLPOSX names) is not stable.
*&   - Its selection screen is different: s_ccode/s_racct/p_ledger/
*&     p_ai/s_pdate/psx_item instead of sd_bukrs/sd_saknr/rldnr/
*&     x_aisel/so_budat/fs_dyns.
*&
*& What the old SUBMIT actually returned:
*&   G/L-view line items of ledger 0L (ACDOCA, RLDNR = '0L') for the
*&   documents in TA_BSEG_FAGLL03 (BUKRS/BELNR/GJAHR), account IN
*&   s_saknr, posting date IN s_budat, "all items", in FAGLPOSX layout,
*&   after the FAGL_ITEMS_CH_DATA BAdI ran (that is what fills the
*&   custom field ZZGKONT_LTXT; GKONT/GKART are in ACDOCA since
*&   note 2588267).
*&
*& This form reads exactly that set directly and fills TA_FAGLL03.
*& It replaces the whole DO lv_div TIMES ... ENDDO chunking loop as
*& well: FOR ALL ENTRIES handles the 500-document packaging.
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Call site (inside LOOP AT ta_bseg_gjahr ... , after ta_bseg_fagll03
* has been built for the current fiscal year):
*
*   IF NOT ta_bseg_fagll03 IS INITIAL.
*     PERFORM get_gl_items_direct
*       USING    ta_bseg_fagll03
*                wa_bseg_gjahr-gjahr
*       CHANGING ta_fagll03.
*   ENDIF.
*
* and delete: lv_from/lv_to/lv_div/lv_mod handling, the
* FREE_SELECTIONS_RANGE_2_* calls, cl_salv_bs_runtime_info=>set,
* SUBMIT fagl_account_items_gl, cl_salv_bs_runtime_info=>get_data_ref.
* The post-filter against TA_BSEG_FILTER (CR#8000003569) can stay as is.
*----------------------------------------------------------------------*

FORM get_gl_items_direct
  USING    it_docs   TYPE STANDARD TABLE   " ta_bseg_fagll03 (bukrs belnr gjahr buzei hkont)
           iv_gjahr  TYPE gjahr
  CHANGING ct_items  TYPE STANDARD TABLE.  " ta_fagll03 (ty_fagll03)

  CONSTANTS lc_ledger TYPE fins_ledger VALUE '0L'.

  DATA: lt_items TYPE STANDARD TABLE OF ty_fagll03,
        lt_badi  TYPE faglposx_t,              " FAGLPOSX table for the BAdI
        lo_badi  TYPE REF TO if_ex_fagl_items_ch_data,
        lv_impl  TYPE c LENGTH 1.

  CHECK it_docs IS NOT INITIAL.

  "--- 1. Same rows FAGLL03 produced: GL view, ledger 0L, all items ----
  " Released CDS views are used to stay in line with the S4 2025 ATC
  " remediation already done in this program (I_OPERATIONALACCTGDOCITEM).
  "   I_JournalEntryItem         = ACDOCA  (G/L view)
  "   I_OperationalAcctgDocItem  = BSEG    (entry-view-only fields)
  "   I_JournalEntry             = BKPF    (header fields)
  SELECT
      gl~companycode                      AS bukrs,
      gl~accountingdocument               AS belnr,
      gl~fiscalyear                       AS gjahr,
      gl~accountingdocumentitem           AS buzei,
      gl~glaccount                        AS hkont,
      gl~glaccount                        AS konto,
      gl~accountingdocumenttype           AS blart,
      gl~documentdate                     AS bldat,
      gl~postingdate                      AS budat,
      gl~postingkey                       AS bschl,
      gl~debitcreditcode                  AS shkzg,
      gl~amountincompanycodecurrency      AS dmshb,     " FAGLPOSX-DMSHB = signed LC amount
      gl~companycodecurrency              AS hwaer,
      gl~amountintransactioncurrency      AS wrshb,     " FAGLPOSX-WRSHB = signed doc-curr amount
      gl~globalcurrency                   AS hwae2,
      gl~taxcode                          AS mwskz,
      gl~clearingaccountingdocument       AS augbl,
      gl~profitcenter                     AS prctr,
      gl~costcenter                       AS kostl,
      gl~orderid                          AS aufnr,
      gl~segment                          AS segment,
      gl~documentitemtext                 AS sgtxt,
      gl~financialaccounttype             AS koart,
      gl~offsettingaccount                AS gkont,     " ACDOCA-GKONT (note 2588267)
      gl~offsettingaccounttype            AS gkart,     " ACDOCA-GKOAR
      gl~invoicereference                 AS rebzg,
      gl~assignmentreference              AS u_zuonr,
      gl~supplier                         AS u_lifnr,
      gl~purchasingdocument               AS u_ebeln,
      hd~documentreferenceid              AS xblnr,
      hd~alternativereferencedocument     AS u_xblnr_alt,
      hd~accountingdoccreatedbyuser       AS u_usnam,
      ev~absltamtinbalancetransaccrcy     AS pswbt,     " BSEG-PSWBT (FAGLPOSX-PSWBT is absolute)
      ev~balancetransactioncurrency       AS pswsl,     " BSEG-PSWSL
      ev~withholdingtaxamount             AS qbshb,     " BSEG-QBSHB signed (QBSHB_SHL)
      ev~paymentreference                 AS kidno,     " BSEG-KIDNO (not in I_JournalEntryItem)
      ev~accountingdocumentitemtype       AS buzid      " BSEG-BUZID (not in I_JournalEntryItem)
    FROM i_journalentryitem AS gl
      INNER JOIN i_journalentry AS hd
        ON  hd~companycode        = gl~companycode
        AND hd~accountingdocument = gl~accountingdocument
        AND hd~fiscalyear         = gl~fiscalyear
      LEFT OUTER JOIN i_operationalacctgdocitem AS ev
        ON  ev~companycode            = gl~companycode
        AND ev~accountingdocument     = gl~accountingdocument
        AND ev~fiscalyear             = gl~fiscalyear
        AND ev~accountingdocumentitem = gl~accountingdocumentitem
    FOR ALL ENTRIES IN @it_docs
    WHERE gl~ledger             = @lc_ledger
      AND gl~companycode        = @it_docs-bukrs
      AND gl~accountingdocument = @it_docs-belnr
      AND gl~fiscalyear         = @iv_gjahr
      AND gl~glaccount         IN @s_saknr
      AND gl~postingdate       IN @s_budat
    INTO CORRESPONDING FIELDS OF TABLE @lt_items.

  CHECK lt_items IS NOT INITIAL.

  "--- 2. Derived field FAGLPOSX-JAMON ('YYYY/MM') ----------------------
  LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).
    <ls_item>-jamon = |{ <ls_item>-budat(4) }/{ <ls_item>-budat+4(2) }|.
  ENDLOOP.

  "--- 3. Run the same BAdI FAGLL03 ran (fills ZZGKONT_LTXT etc.) ------
  CALL METHOD cl_exithandler=>get_instance
    EXPORTING
      exit_name              = 'FAGL_ITEMS_CH_DATA'
      null_instance_accepted = abap_true
    IMPORTING
      act_imp_existing       = lv_impl
    CHANGING
      instance               = lo_badi.

  IF lv_impl IS NOT INITIAL AND lo_badi IS BOUND.
    MOVE-CORRESPONDING lt_items TO lt_badi.
    lo_badi->change_items( CHANGING ct_items = lt_badi ).
    MOVE-CORRESPONDING lt_badi TO lt_items.
  ENDIF.

  APPEND LINES OF lt_items TO ct_items.

ENDFORM.

*----------------------------------------------------------------------*
* Notes
* (a) Every CDS element above was checked against the system export
*     DDLS_BASE_FIELDS.txt (element -> base table/field) in this repo:
*       I_JournalEntryItem        : all elements exist, base ACDOCA
*       I_JournalEntry            : DocumentReferenceID = BKPF-XBLNR,
*                                   AlternativeReferenceDocument = XBLNR_ALT,
*                                   AccountingDocCreatedByUser = USNAM
*       I_OperationalAcctgDocItem : AbsltAmtInBalanceTransacCrcy = BSEG-PSWBT,
*                                   BalanceTransactionCurrency = PSWSL,
*                                   WithholdingTaxAmount = QBSHB (signed),
*                                   PaymentReference = KIDNO,
*                                   AccountingDocumentItemType = BUZID
*     KIDNO and BUZID are NOT in I_JournalEntryItem, hence read from ev.
* (b) Reconcile: run old (FAGLL03) and new form for one p_erdat and
*     compare TA_FAGLL03 line count and SUM(DMSHB) per BUKRS/HKONT.
*----------------------------------------------------------------------*
