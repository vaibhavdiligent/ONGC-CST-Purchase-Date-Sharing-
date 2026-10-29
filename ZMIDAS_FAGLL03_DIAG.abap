*&---------------------------------------------------------------------*
*& Report  ZMIDAS_FAGLL03_DIAG   (temporary, package $TMP, run in CRS)
*&---------------------------------------------------------------------*
*& Purpose: prove where the Dec-2025 row loss of ZMIDAS_FAGLL03 happens
*&          in the S/4 system, for ONE entry date (same as scheduler
*&          variant: company codes, GL set, posting date, entry date).
*&
*& It prints, for that day:
*&   A  documents in BKPF (cpudt)                        - base
*&   B1 items  of those docs read from BSEG              - old ECC logic
*&   B2 items  of those docs read from I_OperationalAcctgDocItem
*&          (what the ATC-remediated program uses)       - CDS access
*&          control check: B2 < B1 => DCL drops rows for this user
*&   C  ACDOCA ledger 0L rows with FAGLL03's filters    - what the new
*&          FORM get_gl_items_direct will return
*&   C2 ... of which ACDOCA-GKONT is empty (KBA 2806316, migrated docs)
*&   D  rows already in ZMIDAS_FAGLL03 for that cpudt    - what the old
*&          SUBMIT path actually stored
*&   E  SUBMIT fagl_account_items_gl for the FIRST 500 documents only,
*&      exactly like the old code, and count distinct documents in the
*&      returned data. If it is far more than 500, the DOCNR free
*&      selection was ignored by S/4 FAGLL03 (root cause confirmed).
*&   All counts also per company code.
*&---------------------------------------------------------------------*
REPORT zmidas_fagll03_diag.

TABLES: bsis, ska1, skb1.

SELECT-OPTIONS: s_bukrs FOR bsis-bukrs OBLIGATORY,
                s_budat FOR bsis-budat,
                s_saknr FOR ska1-saknr.
PARAMETERS:     p_erdat TYPE skb1-erdat OBLIGATORY.

TYPES: BEGIN OF ty_doc,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
       END OF ty_doc,
       BEGIN OF ty_cnt,
         bukrs TYPE bukrs,
         cnt   TYPE i,
       END OF ty_cnt,
       tt_cnt TYPE STANDARD TABLE OF ty_cnt WITH DEFAULT KEY.

DATA: lt_doc   TYPE STANDARD TABLE OF ty_doc,
      lt_rows  TYPE tt_cnt,          " one row per selected item (cnt = 1)
      lt_cnt   TYPE tt_cnt,
      ls_c     TYPE ty_cnt,
      lt_bstat TYPE RANGE OF bstat_d,
      lv_total TYPE i.

*----------------------------------------------------------------------*
FORM aggregate USING    it_rows TYPE tt_cnt
               CHANGING et_cnt  TYPE tt_cnt
                        ev_tot  TYPE i.
  CLEAR: et_cnt, ev_tot.
  LOOP AT it_rows INTO ls_c.
    ls_c-cnt = 1.
    COLLECT ls_c INTO et_cnt.
    ev_tot = ev_tot + 1.
  ENDLOOP.
  SORT et_cnt BY bukrs.
ENDFORM.

FORM print USING iv_text TYPE csequence
                 iv_tot  TYPE i
                 it_cnt  TYPE tt_cnt.
  WRITE: / iv_text, AT 70 iv_tot.
  LOOP AT it_cnt INTO ls_c.
    WRITE: / '   ', ls_c-bukrs, AT 70 ls_c-cnt.
  ENDLOOP.
  SKIP.
ENDFORM.
*----------------------------------------------------------------------*

START-OF-SELECTION.

  "--- A: documents of the day --------------------------------------
  SELECT bukrs, belnr, gjahr FROM bkpf
    WHERE bukrs IN @s_bukrs AND cpudt = @p_erdat
    INTO TABLE @lt_doc.
  WRITE: / 'Entry date', p_erdat, ' company codes', s_bukrs-low, s_bukrs-high.
  WRITE: / 'A  BKPF documents (cpudt)', AT 70 lines( lt_doc ).
  SKIP.
  IF lt_doc IS INITIAL. RETURN. ENDIF.

  "--- B1: BSEG items (classic) -------------------------------------
  SELECT bukrs FROM bseg
    FOR ALL ENTRIES IN @lt_doc
    WHERE bukrs = @lt_doc-bukrs AND belnr = @lt_doc-belnr AND gjahr = @lt_doc-gjahr
      AND hkont IN @s_saknr
    INTO CORRESPONDING FIELDS OF TABLE @lt_rows.
  PERFORM aggregate USING lt_rows CHANGING lt_cnt lv_total.
  PERFORM print USING 'B1 BSEG items of those docs (GL in set)' lv_total lt_cnt.

  "--- B2: same via released CDS view (DCL applies!) -----------------
  SELECT companycode AS bukrs FROM i_operationalacctgdocitem
    FOR ALL ENTRIES IN @lt_doc
    WHERE companycode = @lt_doc-bukrs AND accountingdocument = @lt_doc-belnr
      AND fiscalyear = @lt_doc-gjahr AND glaccount IN @s_saknr
    INTO CORRESPONDING FIELDS OF TABLE @lt_rows.
  PERFORM aggregate USING lt_rows CHANGING lt_cnt lv_total.
  PERFORM print USING 'B2 I_OperationalAcctgDocItem items (B2<B1 => DCL/auth loss)' lv_total lt_cnt.

  "--- C: ACDOCA 0L with FAGLL03 filters (new form result) ----------
  lt_bstat = VALUE #( sign = 'I' option = 'EQ' ( low = space ) ( low = 'L' )
                      ( low = if_fins_acdoc_constants=>gc_document_status-gl_only ) ).
  SELECT rbukrs AS bukrs FROM acdoca
    FOR ALL ENTRIES IN @lt_doc
    WHERE rldnr = '0L' AND rbukrs = @lt_doc-bukrs AND belnr = @lt_doc-belnr
      AND gjahr = @lt_doc-gjahr AND racct IN @s_saknr AND budat IN @s_budat
      AND bstat IN @lt_bstat AND umskz = @space
    INTO CORRESPONDING FIELDS OF TABLE @lt_rows.
  PERFORM aggregate USING lt_rows CHANGING lt_cnt lv_total.
  PERFORM print USING 'C  ACDOCA 0L rows, FAGLL03 filters (new logic result)' lv_total lt_cnt.

  "--- C2: offsetting account filled? (KBA 2806316) ------------------
  SELECT rbukrs AS bukrs FROM acdoca
    FOR ALL ENTRIES IN @lt_doc
    WHERE rldnr = '0L' AND rbukrs = @lt_doc-bukrs AND belnr = @lt_doc-belnr
      AND gjahr = @lt_doc-gjahr AND racct IN @s_saknr AND budat IN @s_budat
      AND bstat IN @lt_bstat AND umskz = @space AND gkont = @space
    INTO CORRESPONDING FIELDS OF TABLE @lt_rows.
  PERFORM aggregate USING lt_rows CHANGING lt_cnt lv_total.
  PERFORM print USING 'C2 ... of which ACDOCA-GKONT empty (should be ~0, KBA 2806316)' lv_total lt_cnt.

  "--- D: what the old run stored -----------------------------------
  SELECT bukrs FROM zmidas_fagll03
    WHERE bukrs IN @s_bukrs AND cpudt = @p_erdat
    INTO CORRESPONDING FIELDS OF TABLE @lt_rows.
  PERFORM aggregate USING lt_rows CHANGING lt_cnt lv_total.
  PERFORM print USING 'D  rows in ZMIDAS_FAGLL03 for this cpudt (old SUBMIT run)' lv_total lt_cnt.

  "--- E: reproduce old SUBMIT for first 500 docs --------------------
  DATA: lt_field_rg TYPE rsds_trange,
        ls_field_rg LIKE LINE OF lt_field_rg,
        ls_france   TYPE rsds_frange,
        ls_free     TYPE rsds_type,
        lo_data     TYPE REF TO data,
        lt_500      TYPE STANDARD TABLE OF ty_doc,
        lt_docs_ret TYPE STANDARD TABLE OF belnr_d WITH DEFAULT KEY.
  FIELD-SYMBOLS <lt_res> TYPE STANDARD TABLE.

  SORT lt_doc BY gjahr bukrs belnr.
  DATA(lv_gjahr) = lt_doc[ 1 ]-gjahr.
  LOOP AT lt_doc INTO DATA(ls_doc) WHERE gjahr = lv_gjahr.
    APPEND ls_doc TO lt_500.
    IF lines( lt_500 ) = 500. EXIT. ENDIF.
  ENDLOOP.

  ls_field_rg-tablename = 'FAGLFLEXA_FS'.
  ls_france-fieldname = 'DOCNR'.
  LOOP AT lt_500 INTO ls_doc.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_doc-belnr ) TO ls_france-selopt_t.
  ENDLOOP.
  APPEND ls_france TO ls_field_rg-frange_t.
  CLEAR ls_france.
  ls_france-fieldname = 'RYEAR'.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_gjahr ) TO ls_france-selopt_t.
  APPEND ls_france TO ls_field_rg-frange_t.
  APPEND ls_field_rg TO lt_field_rg.
  CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_EX'
    EXPORTING field_ranges = lt_field_rg
    IMPORTING expressions  = ls_free-texpr.
  CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
    EXPORTING field_ranges  = lt_field_rg
    IMPORTING where_clauses = ls_free-clauses.
  ls_free-trange = lt_field_rg.

  cl_salv_bs_runtime_info=>set( display = abap_false metadata = abap_false data = abap_true ).
  SUBMIT fagl_account_items_gl
           WITH sd_saknr IN s_saknr
           WITH sd_bukrs IN s_bukrs
           WITH x_opsel  EQ abap_false
           WITH x_clsel  EQ abap_false
           WITH x_aisel  EQ abap_true
           WITH so_budat IN s_budat
           WITH rldnr    =  '0L'
           WITH fs_dyns  =  ls_free
         AND RETURN.
  TRY.
      cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lo_data ).
      IF lo_data IS BOUND.
        ASSIGN lo_data->* TO <lt_res>.
      ENDIF.
    CATCH cx_root INTO DATA(lx).
      WRITE: / 'E  FAGLL03 data capture FAILED:', lx->get_text( ).
  ENDTRY.
  cl_salv_bs_runtime_info=>clear_all( ).

  IF <lt_res> IS ASSIGNED.
    LOOP AT <lt_res> ASSIGNING FIELD-SYMBOL(<ls_res>).
      ASSIGN COMPONENT 'BELNR' OF STRUCTURE <ls_res> TO FIELD-SYMBOL(<belnr>).
      IF sy-subrc = 0.
        APPEND <belnr> TO lt_docs_ret.
      ENDIF.
    ENDLOOP.
    SORT lt_docs_ret.
    DELETE ADJACENT DUPLICATES FROM lt_docs_ret.
    WRITE: / 'E  SUBMIT FAGLL03 with 500 docs in fs_dyns: rows returned', AT 70 lines( <lt_res> ).
    WRITE: / '   distinct documents returned (expected <= 500)', AT 70 lines( lt_docs_ret ).
    IF lines( lt_docs_ret ) > 500.
      WRITE: / '   ==> DOCNR free selection IGNORED by S/4 FAGLL03 (ACDOCA has no DOCNR).'.
    ENDIF.
  ELSE.
    WRITE: / 'E  SUBMIT FAGLL03 returned no data reference (capture failed silently).'.
  ENDIF.
