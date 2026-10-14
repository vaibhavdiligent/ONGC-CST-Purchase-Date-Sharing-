*&---------------------------------------------------------------------*
*& Report  ZFI_COL_OVL_POSTING
*&---------------------------------------------------------------------*
*& Corporate - Colombia : re-post OVC period balances in OVL.
*&   (FS: "FS for Corporate - Colombia - Updated", Raju Ashar, as
*&    amended 14.10.26 : offsetting account of the 95671 / 956712 lines)
*&
*&   Step 1 : ACDOCA (ledger 0L, RRCTY 0) - documents of the OVC company
*&            code / year / period that hit GL 95671 or 956712.
*&   Step 2 : BSEG lines of those documents ON 95671 / 956712, subtotal
*&            of DMBTR and DMBE2 by their offsetting account :
*&              GKART = K  -> per GKONT (vendor)
*&              GKART <> K -> per GHKON (offsetting G/L account)
*&            Sign : the mapped GL takes the offsetting account's side,
*&            i.e. the 95671 / 956712 amount with opposite sign; the
*&            offset line (offset GL) takes the 95671 / 956712 side.
*&   Map    : ZFI_COL_GLMAP with KOART = 'K' for vendors, blank for GLs.
*&   Post   : ONE document in OVL (doc type CL).
*&            Document currency = OVC local currency (COP) = DMBTR,
*&            OVL currency type holding OVC 2nd local currency (USD)
*&            = DMBE2, OVL local currency (INR) converted by the system.
*&            For every GL / vendor : mapped GL line (key 40/50 by sign,
*&            CC / PC / BA from ZFI_COL_GLMAP) + its own offset line on
*&            the offset GL with the opposite key.
*&            Posting / document date = last day of the OVC period.
*&   Log    : on success the OVC company code / year / period is written
*&            to ZFI_COL_POSTLOG - a period already in the log is blocked.
*&
*& DDIC prerequisites : ZFI_COL_GLMAP, ZFI_COL_POSTLOG
*&   (see ZFI_COL_OVL_POSTING_TECH_SPEC.md)
*&---------------------------------------------------------------------*
REPORT  zfi_col_ovl_posting.

INCLUDE <icon>.

TABLES: t001.

CONSTANTS: gc_rldnr TYPE acdoca-rldnr VALUE '0L',
           gc_rrcty TYPE acdoca-rrcty VALUE '0',
           gc_gl1   TYPE acdoca-racct VALUE '95671',
           gc_gl2   TYPE acdoca-racct VALUE '956712',
           gc_blart TYPE blart       VALUE 'CL',
           gc_koart_vendor TYPE bseg-koart VALUE 'K',
           gc_bschl_dr TYPE bschl    VALUE '40',
           gc_bschl_cr TYPE bschl    VALUE '50',
           gc_max_items TYPE i       VALUE 999,
*          Offset line: fixed profit center / business area (all lines)
           gc_off_prctr TYPE prctr   VALUE 'OVJVCLBR01',
           gc_off_gsber TYPE gsber   VALUE 'JV'.

TYPES: BEGIN OF ty_doc,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
       END OF ty_doc.

TYPES: BEGIN OF ty_bseg,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         buzei TYPE bseg-buzei,
         shkzg TYPE bseg-shkzg,
         hkont TYPE bseg-hkont,
         gkart TYPE bseg-gkart,       " offsetting account type
         gkont TYPE bseg-gkont,       " offsetting account
         ghkon TYPE bseg-ghkon,       " offsetting G/L account
         dmbtr TYPE bseg-dmbtr,
         dmbe2 TYPE bseg-dmbe2,
       END OF ty_bseg.

* KOART = 'K' -> ACCT = vendor (GKONT), KOART = space -> ACCT = GL (GHKON)
TYPES: BEGIN OF ty_sum,
         koart TYPE bseg-koart,
         acct  TYPE bseg-hkont,
         dmbtr TYPE bseg-dmbtr,
         dmbe2 TYPE bseg-dmbe2,
       END OF ty_sum.

TYPES: BEGIN OF ty_out,
         status       TYPE icon_d,
         itemno       TYPE posnr_acc,
         koart        TYPE bseg-koart,
         from_hkont   TYPE bseg-hkont,
         dmbtr        TYPE bseg-dmbtr,
         waers        TYPE t001-waers,
         dmbe2        TYPE bseg-dmbe2,
         waers2       TYPE t001-waers,
         bschl        TYPE bschl,
         to_hkont     TYPE hkont,
         kostl        TYPE kostl,
         prctr        TYPE prctr,
         gsber        TYPE gsber,
         off_itemno   TYPE posnr_acc,
         offset_hkont TYPE hkont,
         off_prctr    TYPE prctr,
         off_gsber    TYPE gsber,
         off_bschl    TYPE bschl,
         message      TYPE bapi_msg,
       END OF ty_out.

DATA: gt_doc    TYPE STANDARD TABLE OF ty_doc,
      gt_bseg   TYPE STANDARD TABLE OF ty_bseg,
      gt_sum    TYPE SORTED TABLE OF ty_sum WITH UNIQUE KEY koart acct,
      gt_map    TYPE SORTED TABLE OF zfi_col_glmap
                     WITH UNIQUE KEY from_bukrs koart from_hkont to_bukrs,
      gt_out    TYPE STANDARD TABLE OF ty_out,
      gt_return TYPE STANDARD TABLE OF bapiret2,
      gr_racct  TYPE RANGE OF acdoca-racct.

DATA: gv_to_bukrs TYPE bukrs,
      gv_budat    TYPE budat,
      gv_to_gjahr TYPE gjahr,
      gv_to_poper TYPE poper,
      gv_belnr    TYPE belnr_d,
      gv_waers    TYPE t001-waers,  " document currency = OVC local (COP)
      gv_waers2   TYPE bkpf-hwae2,  " OVC 2nd local currency (USD)
      gv_curtp2   TYPE bkpf-curt2,  " OVL currency type for GV_WAERS2
*     internal (2 decimals) -> external BAPI amount factor per currency
*     (TCURX: e.g. COP with 0 decimals -> factor 100)
      gv_fact     TYPE p LENGTH 8 DECIMALS 3,
      gv_fact2    TYPE p LENGTH 8 DECIMALS 3,
      gv_msg      TYPE string,      " error text for ADD_ERR
      gv_error    TYPE flag.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY,      " From Co.Code (OVC)
            p_poper TYPE poper OBLIGATORY,      " Period (1 value)
            p_gjahr TYPE gjahr OBLIGATORY.      " Fiscal year
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X'.      " Test run (no posting)
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN ON p_bukrs.
  SELECT SINGLE * FROM t001 WHERE bukrs = p_bukrs.
  IF sy-subrc <> 0.
    MESSAGE |Company code { p_bukrs } does not exist| TYPE 'E'.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'BUKRS' FIELD p_bukrs
    ID 'ACTVT' FIELD '03'.
  IF sy-subrc <> 0.
    MESSAGE |No authorization to display company code { p_bukrs }|
            TYPE 'E'.
  ENDIF.

AT SELECTION-SCREEN ON p_poper.
  IF p_poper < 1 OR p_poper > 16.
    MESSAGE 'Period must be between 001 and 016' TYPE 'E'.
  ENDIF.

AT SELECTION-SCREEN.
  PERFORM check_already_posted.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  PERFORM main.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
*& Processing steps - stop at the first step that reports errors.
*&---------------------------------------------------------------------*
FORM main.
  PERFORM build_racct_range.
  PERFORM get_documents.
  CHECK gv_error IS INITIAL.
  PERFORM get_subtotals.
  CHECK gv_error IS INITIAL.
  PERFORM read_mapping.
  CHECK gv_error IS INITIAL.
  PERFORM get_posting_date.
  CHECK gv_error IS INITIAL.
  PERFORM get_currencies.
  CHECK gv_error IS INITIAL.
  PERFORM validate_mapping.
  CHECK gv_error IS INITIAL.
  PERFORM build_and_post.
ENDFORM.

*&---------------------------------------------------------------------*
*& Collect one error (shown in the error list at the end).
*&---------------------------------------------------------------------*
FORM add_err USING pv_text TYPE csequence.
  DATA ls_ret TYPE bapiret2.

  gv_error       = abap_true.
  ls_ret-type    = 'E'.
  ls_ret-message = pv_text.
  APPEND ls_ret TO gt_return.
ENDFORM.

*&---------------------------------------------------------------------*
*& Block a period that was already posted successfully.
*&---------------------------------------------------------------------*
FORM check_already_posted.
  DATA ls_log TYPE zfi_col_postlog.

  SELECT SINGLE * FROM zfi_col_postlog INTO ls_log
    WHERE bukrs = p_bukrs
      AND gjahr = p_gjahr
      AND poper = p_poper.
  IF sy-subrc = 0.
    MESSAGE |Period already posted: OVL document { ls_log-belnr } | &&
            |{ ls_log-to_bukrs } { ls_log-to_gjahr }| TYPE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& GL 95671 / 956712 with leading zeros (internal format).
*&---------------------------------------------------------------------*
FORM build_racct_range.
  DATA: ls_racct LIKE LINE OF gr_racct,
        lv_gl    TYPE acdoca-racct.

  ls_racct-sign   = 'I'.
  ls_racct-option = 'EQ'.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gc_gl1
    IMPORTING
      output = lv_gl.
  ls_racct-low = lv_gl.
  APPEND ls_racct TO gr_racct.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gc_gl2
    IMPORTING
      output = lv_gl.
  ls_racct-low = lv_gl.
  APPEND ls_racct TO gr_racct.
ENDFORM.

*&---------------------------------------------------------------------*
*& Step 1 - documents from ACDOCA hitting GL 95671 / 956712.
*&---------------------------------------------------------------------*
FORM get_documents.
  SELECT DISTINCT belnr, gjahr
    FROM acdoca
    WHERE rldnr  = @gc_rldnr
      AND rrcty  = @gc_rrcty
      AND rbukrs = @p_bukrs
      AND gjahr  = @p_gjahr
      AND poper  = @p_poper
      AND racct IN @gr_racct
    INTO TABLE @gt_doc.

  IF gt_doc IS INITIAL.
    gv_msg = |No documents found on GL 95671 / 956712 for | &&
             |{ p_bukrs } period { p_poper }/{ p_gjahr }|.
    PERFORM add_err USING gv_msg.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Step 2 - BSEG lines on 95671 / 956712, subtotal DMBTR + DMBE2 by
*&          offsetting account : GKART K per GKONT, others per GHKON.
*&---------------------------------------------------------------------*
FORM get_subtotals.
  DATA: ls_sum   TYPE ty_sum,
        lv_lines TYPE i.
  FIELD-SYMBOLS <ls_bseg> TYPE ty_bseg.

* Only the 95671 / 956712 lines - grouped by their offsetting account
  SELECT bukrs, belnr, gjahr, buzei, shkzg, hkont, gkart, gkont, ghkon,
         dmbtr, dmbe2
    FROM bseg
    FOR ALL ENTRIES IN @gt_doc
    WHERE bukrs = @p_bukrs
      AND belnr = @gt_doc-belnr
      AND gjahr = @gt_doc-gjahr
      AND hkont IN @gr_racct
    INTO TABLE @gt_bseg.

  LOOP AT gt_bseg ASSIGNING <ls_bseg>.
    CLEAR ls_sum.
    IF <ls_bseg>-gkart = gc_koart_vendor.  " vendor -> per GKONT
      ls_sum-koart = gc_koart_vendor.
      ls_sum-acct  = <ls_bseg>-gkont.
    ELSE.                                  " others -> per GHKON
      ls_sum-acct  = <ls_bseg>-ghkon.
    ENDIF.
*   Offsetting account not determined (blank / '*') -> cannot be mapped
    IF ls_sum-acct IS INITIAL OR ls_sum-acct CA '*'.
      gv_msg = |Document { <ls_bseg>-belnr }/{ <ls_bseg>-gjahr } line | &&
               |{ <ls_bseg>-buzei }: offsetting account (GKONT/GHKON) | &&
               |not filled in BSEG|.
      PERFORM add_err USING gv_msg.
      CONTINUE.
    ENDIF.
*   Mapped GL takes the offsetting account's side = opposite sign of the
*   95671 / 956712 line (debit 'S' -> negative, credit 'H' -> positive)
    IF <ls_bseg>-shkzg = 'S'.
      ls_sum-dmbtr = - <ls_bseg>-dmbtr.
      ls_sum-dmbe2 = - <ls_bseg>-dmbe2.
    ELSE.
      ls_sum-dmbtr = <ls_bseg>-dmbtr.
      ls_sum-dmbe2 = <ls_bseg>-dmbe2.
    ENDIF.
    COLLECT ls_sum INTO gt_sum.
  ENDLOOP.
  CHECK gv_error IS INITIAL.

  DELETE gt_sum WHERE dmbtr = 0 AND dmbe2 = 0.

  IF gt_sum IS INITIAL.
    gv_msg = 'No amounts (DMBTR / DMBE2) to post'.
    PERFORM add_err USING gv_msg.
  ELSEIF lines( gt_sum ) * 2 > gc_max_items.
    lv_lines = lines( gt_sum ).
    gv_msg = |Too many GLs / vendors for one document: | &&
             |{ lv_lines } (max 499)|.
    PERFORM add_err USING gv_msg.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Read mapping ZFI_COL_GLMAP - every GL / vendor must be mapped (KOART
*& 'K' rows for vendors, blank for GLs), all to the same target company
*& code (one document).
*&---------------------------------------------------------------------*
FORM read_mapping.
  DATA: lt_to_bukrs TYPE SORTED TABLE OF bukrs WITH UNIQUE KEY table_line,
        lv_count    TYPE i,
        lv_acct_out TYPE string.
  FIELD-SYMBOLS: <ls_sum> TYPE ty_sum,
                 <ls_map> TYPE zfi_col_glmap.

  SELECT * FROM zfi_col_glmap INTO TABLE gt_map
    WHERE from_bukrs = p_bukrs.

  LOOP AT gt_sum ASSIGNING <ls_sum>.
    CLEAR lv_count.
    LOOP AT gt_map ASSIGNING <ls_map> WHERE koart      = <ls_sum>-koart
                                        AND from_hkont = <ls_sum>-acct.
      lv_count = lv_count + 1.
      INSERT <ls_map>-to_bukrs INTO TABLE lt_to_bukrs.
    ENDLOOP.
    IF <ls_sum>-koart = gc_koart_vendor.
      lv_acct_out = |Vendor { <ls_sum>-acct ALPHA = OUT } (KOART K)|.
    ELSE.
      lv_acct_out = |GL { <ls_sum>-acct ALPHA = OUT } (KOART blank)|.
    ENDIF.
    IF lv_count = 0.
      gv_msg = |{ lv_acct_out } not mapped in ZFI_COL_GLMAP|.
      PERFORM add_err USING gv_msg.
    ELSEIF lv_count > 1.
      gv_msg = |{ lv_acct_out } mapped to more than one | &&
               |To Co.Code|.
      PERFORM add_err USING gv_msg.
    ENDIF.
  ENDLOOP.
  CHECK gv_error IS INITIAL.

  IF lines( lt_to_bukrs ) <> 1.
    gv_msg = |Mapped GLs / vendors belong to different | &&
             |To Co.Codes - one document needs one company code|.
    PERFORM add_err USING gv_msg.
    RETURN.
  ENDIF.

  READ TABLE lt_to_bukrs INTO gv_to_bukrs INDEX 1.

  IF p_test IS INITIAL.
    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
      ID 'BUKRS' FIELD gv_to_bukrs
      ID 'ACTVT' FIELD '01'.
    IF sy-subrc <> 0.
      gv_msg = |No authorization to post in company code | &&
               |{ gv_to_bukrs }|.
      PERFORM add_err USING gv_msg.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Currencies (FS updated) - standard tables only (T001, BKPF) :
*&   document currency = OVC local currency T001-WAERS (COP) <- DMBTR
*&   OVC 2nd local currency = BKPF-HWAE2 of the OVC documents (USD)
*&   OVL currency type holding that currency = BKPF-CURT2 / CURT3 of an
*&   OVL document of the OVL fiscal year              <- DMBE2
*&   OVL local currency (INR) is translated by the system.
*&---------------------------------------------------------------------*
FORM get_currencies.
  DATA: lv_waers_to TYPE t001-waers,
        ls_doc      TYPE ty_doc,
        lv_curt2    TYPE bkpf-curt2,
        lv_hwae2    TYPE bkpf-hwae2,
        lv_curt3    TYPE bkpf-curt3,
        lv_hwae3    TYPE bkpf-hwae3.

  SELECT SINGLE waers FROM t001 INTO gv_waers    WHERE bukrs = p_bukrs.
  SELECT SINGLE waers FROM t001 INTO lv_waers_to WHERE bukrs = gv_to_bukrs.

* OVC 2nd local currency - from the header of an OVC source document
  READ TABLE gt_doc INTO ls_doc INDEX 1.
  SELECT SINGLE hwae2 FROM bkpf INTO gv_waers2
    WHERE bukrs = p_bukrs
      AND belnr = ls_doc-belnr
      AND gjahr = ls_doc-gjahr.
  IF gv_waers2 IS INITIAL.
    gv_msg = |No 2nd local currency (BKPF-HWAE2) found for { p_bukrs }|.
    PERFORM add_err USING gv_msg.
    RETURN.
  ENDIF.

* OVL currency settings - from any OVL document of the OVL fiscal year
  SELECT curt2 hwae2 curt3 hwae3 FROM bkpf UP TO 1 ROWS
    INTO (lv_curt2, lv_hwae2, lv_curt3, lv_hwae3)
    WHERE bukrs = gv_to_bukrs
      AND gjahr = gv_to_gjahr.
  ENDSELECT.
  IF sy-subrc <> 0.
    SELECT curt2 hwae2 curt3 hwae3 FROM bkpf UP TO 1 ROWS
      INTO (lv_curt2, lv_hwae2, lv_curt3, lv_hwae3)
      WHERE bukrs = gv_to_bukrs.
    ENDSELECT.
  ENDIF.

  IF lv_hwae2 = gv_waers2 AND lv_curt2 IS NOT INITIAL.
    gv_curtp2 = lv_curt2.
  ELSEIF lv_hwae3 = gv_waers2 AND lv_curt3 IS NOT INITIAL.
    gv_curtp2 = lv_curt3.
  ELSEIF lv_waers_to = gv_waers2.
    gv_curtp2 = '10'.
  ELSE.
    gv_msg = |Currency { gv_waers2 } (2nd local currency of { p_bukrs }) | &&
             |is not a local currency of { gv_to_bukrs }|.
    PERFORM add_err USING gv_msg.
  ENDIF.

  PERFORM get_bapi_factor USING gv_waers  CHANGING gv_fact.
  PERFORM get_bapi_factor USING gv_waers2 CHANGING gv_fact2.
ENDFORM.

*&---------------------------------------------------------------------*
*& BSEG / ACDOCA amounts are stored internally with 2 decimals; the BAPI
*& expects the real (external) amount. Currencies with other decimals
*& are in TCURX (no entry = 2 decimals). Factor = 10 ** ( 2 - CURRDEC ).
*&   COP 0 decimals : 258222131.21 (internal) -> 25822213121 (external)
*&---------------------------------------------------------------------*
FORM get_bapi_factor USING    pv_waers TYPE t001-waers
                     CHANGING pv_fact  TYPE p.
  DATA: lv_currdec TYPE tcurx-currdec,
        lv_exp     TYPE i.

  SELECT SINGLE currdec FROM tcurx INTO lv_currdec
    WHERE currkey = pv_waers.
  IF sy-subrc <> 0.
    lv_currdec = 2.
  ENDIF.
  lv_exp  = 2 - lv_currdec.
  pv_fact = 10 ** lv_exp.
ENDFORM.

*&---------------------------------------------------------------------*
*& Last day of the period in OVC's fiscal year variant = posting date /
*& document date in OVL. OVL period is derived from that date.
*&---------------------------------------------------------------------*
FORM get_posting_date.
  DATA: lv_periv_from TYPE t001-periv,
        lv_periv_to   TYPE t001-periv.

  SELECT SINGLE periv FROM t001 INTO lv_periv_from WHERE bukrs = p_bukrs.
  SELECT SINGLE periv FROM t001 INTO lv_periv_to   WHERE bukrs = gv_to_bukrs.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr        = p_gjahr
      i_periv        = lv_periv_from
      i_poper        = p_poper
    IMPORTING
      e_date         = gv_budat
    EXCEPTIONS
      OTHERS         = 1.
  IF sy-subrc <> 0 OR gv_budat IS INITIAL.
    gv_msg = |Cannot determine last day of period | &&
             |{ p_poper }/{ p_gjahr } (variant { lv_periv_from })|.
    PERFORM add_err USING gv_msg.
    RETURN.
  ENDIF.

  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      i_date         = gv_budat
      i_periv        = lv_periv_to
    IMPORTING
      e_buper        = gv_to_poper
      e_gjahr        = gv_to_gjahr
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
    gv_msg = |Cannot determine period of { gv_to_bukrs } | &&
             |for date { gv_budat DATE = USER }|.
    PERFORM add_err USING gv_msg.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Mapping master data in the To Co.Code (as per FS ZTable):
*&   GL / offset GL -> SKB1, CC -> CSKS, PC -> CEPC (valid on posting
*&   date). GSBER -> TGSB.  All errors are listed together.
*&---------------------------------------------------------------------*
FORM validate_mapping.
  DATA: lv_src   TYPE string,
        lv_dummy TYPE char1.
  FIELD-SYMBOLS: <ls_sum> TYPE ty_sum,
                 <ls_map> TYPE zfi_col_glmap.

* Fixed offset profit center / business area
  SELECT SINGLE @abap_true FROM cepc
    WHERE prctr = @gc_off_prctr
      AND ( bukrs = @gv_to_bukrs OR bukrs = @space )
      AND datab <= @gv_budat
      AND datbi >= @gv_budat
    INTO @lv_dummy.
  IF sy-subrc <> 0.
    gv_msg = |Offset profit center { gc_off_prctr } not valid in | &&
             |{ gv_to_bukrs } on { gv_budat DATE = USER }|.
    PERFORM add_err USING gv_msg.
  ENDIF.
  SELECT SINGLE @abap_true FROM tgsb
    WHERE gsber = @gc_off_gsber
    INTO @lv_dummy.
  IF sy-subrc <> 0.
    gv_msg = |Offset business area { gc_off_gsber } does not exist|.
    PERFORM add_err USING gv_msg.
  ENDIF.

  LOOP AT gt_sum ASSIGNING <ls_sum>.
    READ TABLE gt_map ASSIGNING <ls_map>
      WITH KEY from_bukrs = p_bukrs
               koart      = <ls_sum>-koart
               from_hkont = <ls_sum>-acct
               to_bukrs   = gv_to_bukrs.
    CHECK sy-subrc = 0.

    IF <ls_sum>-koart = gc_koart_vendor.
      lv_src = |Mapping of vendor { <ls_sum>-acct ALPHA = OUT }:|.
    ELSE.
      lv_src = |Mapping of GL { <ls_sum>-acct ALPHA = OUT }:|.
    ENDIF.

    SELECT SINGLE @abap_true FROM skb1
      WHERE bukrs = @gv_to_bukrs AND saknr = @<ls_map>-to_hkont
      INTO @lv_dummy.
    IF sy-subrc <> 0.
      gv_msg = |{ lv_src } GL { <ls_map>-to_hkont ALPHA = OUT } | &&
               |not in { gv_to_bukrs }|.
      PERFORM add_err USING gv_msg.
    ENDIF.

    SELECT SINGLE @abap_true FROM skb1
      WHERE bukrs = @gv_to_bukrs AND saknr = @<ls_map>-offset_hkont
      INTO @lv_dummy.
    IF sy-subrc <> 0.
      gv_msg = |{ lv_src } offset GL | &&
               |{ <ls_map>-offset_hkont ALPHA = OUT } not in { gv_to_bukrs }|.
      PERFORM add_err USING gv_msg.
    ENDIF.

    IF <ls_map>-kostl IS NOT INITIAL.
      SELECT SINGLE @abap_true FROM csks
        WHERE kostl = @<ls_map>-kostl
          AND bukrs = @gv_to_bukrs
          AND datab <= @gv_budat
          AND datbi >= @gv_budat
        INTO @lv_dummy.
      IF sy-subrc <> 0.
        gv_msg = |{ lv_src } cost center | &&
                 |{ <ls_map>-kostl ALPHA = OUT } not valid in { gv_to_bukrs }|.
        PERFORM add_err USING gv_msg.
      ENDIF.
    ENDIF.

    IF <ls_map>-prctr IS NOT INITIAL.
      SELECT SINGLE @abap_true FROM cepc
        WHERE prctr = @<ls_map>-prctr
          AND ( bukrs = @gv_to_bukrs OR bukrs = @space )
          AND datab <= @gv_budat
          AND datbi >= @gv_budat
        INTO @lv_dummy.
      IF sy-subrc <> 0.
        gv_msg = |{ lv_src } profit center | &&
                 |{ <ls_map>-prctr ALPHA = OUT } not valid in { gv_to_bukrs }|.
        PERFORM add_err USING gv_msg.
      ENDIF.
    ENDIF.

    IF <ls_map>-gsber IS NOT INITIAL.
      SELECT SINGLE @abap_true FROM tgsb
        WHERE gsber = @<ls_map>-gsber
        INTO @lv_dummy.
      IF sy-subrc <> 0.
        gv_msg = |{ lv_src } business area | &&
                 |{ <ls_map>-gsber } does not exist|.
        PERFORM add_err USING gv_msg.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Build one document : per GL a mapped-GL line + its offset line.
*& Check it; post (and log) only when not a test run.
*&---------------------------------------------------------------------*
FORM build_and_post.
  DATA: ls_header TYPE bapiache09,
        lt_gl     TYPE STANDARD TABLE OF bapiacgl09,
        ls_gl     TYPE bapiacgl09,
        lt_curr   TYPE STANDARD TABLE OF bapiaccr09,
        lv_dmbtr  TYPE bseg-dmbtr,
        lv_dmbe2  TYPE bseg-dmbe2,
        ls_out    TYPE ty_out,
        ls_log    TYPE zfi_col_postlog,
        lv_item   TYPE posnr_acc,
        lv_objkey TYPE bapiache09-obj_key,
        lv_varkey TYPE rstable-varkey,
        lv_text   TYPE string.
  FIELD-SYMBOLS: <ls_sum> TYPE ty_sum,
                 <ls_map> TYPE zfi_col_glmap.

* Header
  ls_header-bus_act    = 'RFBU'.
  ls_header-username   = sy-uname.
  ls_header-comp_code  = gv_to_bukrs.
  ls_header-doc_date   = gv_budat.
  ls_header-pstng_date = gv_budat.
  ls_header-doc_type   = gc_blart.
  ls_header-ref_doc_no = |{ p_bukrs }/{ p_poper }/{ p_gjahr }|.
  ls_header-header_txt = |{ p_bukrs } { p_poper }/{ p_gjahr } to OVL|.

* Lines
  LOOP AT gt_sum ASSIGNING <ls_sum>.
    READ TABLE gt_map ASSIGNING <ls_map>
      WITH KEY from_bukrs = p_bukrs
               koart      = <ls_sum>-koart
               from_hkont = <ls_sum>-acct
               to_bukrs   = gv_to_bukrs.
    CHECK sy-subrc = 0.

    IF <ls_sum>-koart = gc_koart_vendor.
      lv_text = |{ p_bukrs } Vendor { <ls_sum>-acct ALPHA = OUT }|.
    ELSE.
      lv_text = |{ p_bukrs } GL { <ls_sum>-acct ALPHA = OUT }|.
    ENDIF.

    CLEAR ls_out.
    ls_out-koart        = <ls_sum>-koart.
    ls_out-from_hkont   = <ls_sum>-acct.
    ls_out-dmbtr        = <ls_sum>-dmbtr.
    ls_out-waers        = gv_waers.
    ls_out-dmbe2        = <ls_sum>-dmbe2.
    ls_out-waers2       = gv_waers2.
    ls_out-to_hkont     = <ls_map>-to_hkont.
    ls_out-kostl        = <ls_map>-kostl.
    ls_out-prctr        = <ls_map>-prctr.
    ls_out-gsber        = <ls_map>-gsber.
    ls_out-offset_hkont = <ls_map>-offset_hkont.
    ls_out-off_prctr    = gc_off_prctr.
    ls_out-off_gsber    = gc_off_gsber.
*   Sign from document currency (COP); USD only if COP is zero
    IF <ls_sum>-dmbtr > 0 OR ( <ls_sum>-dmbtr = 0 AND <ls_sum>-dmbe2 > 0 ).
      ls_out-bschl     = gc_bschl_dr.
      ls_out-off_bschl = gc_bschl_cr.
    ELSE.
      ls_out-bschl     = gc_bschl_cr.
      ls_out-off_bschl = gc_bschl_dr.
    ENDIF.

*   Mapped GL line (+ = debit / 40, - = credit / 50)
    lv_item = lv_item + 1.
    ls_out-itemno = lv_item.
    CLEAR ls_gl.
    ls_gl-itemno_acc = lv_item.
    ls_gl-gl_account = <ls_map>-to_hkont.
    ls_gl-item_text  = lv_text.
    ls_gl-costcenter = <ls_map>-kostl.
    ls_gl-profit_ctr = <ls_map>-prctr.
    ls_gl-bus_area   = <ls_map>-gsber.
    APPEND ls_gl TO lt_gl.
    PERFORM add_curr TABLES lt_curr
                     USING  lv_item <ls_sum>-dmbtr <ls_sum>-dmbe2.

*   Offset line - opposite sign / posting key
    lv_item = lv_item + 1.
    ls_out-off_itemno = lv_item.
    CLEAR ls_gl.
    ls_gl-itemno_acc = lv_item.
    ls_gl-gl_account = <ls_map>-offset_hkont.
    ls_gl-item_text  = |Offset { lv_text }|.
    ls_gl-profit_ctr = gc_off_prctr.
    ls_gl-bus_area   = gc_off_gsber.
    APPEND ls_gl TO lt_gl.
    lv_dmbtr = - <ls_sum>-dmbtr.
    lv_dmbe2 = - <ls_sum>-dmbe2.
    PERFORM add_curr TABLES lt_curr
                     USING  lv_item lv_dmbtr lv_dmbe2.

    APPEND ls_out TO gt_out.
  ENDLOOP.

* Lock the period so two users cannot post it at the same time
  CONCATENATE sy-mandt p_bukrs p_gjahr p_poper INTO lv_varkey.
  CALL FUNCTION 'ENQUEUE_E_TABLE'
    EXPORTING
      tabname        = 'ZFI_COL_POSTLOG'
      varkey         = lv_varkey
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    gv_error = abap_true.
    PERFORM set_status USING icon_red_light
                             'Period is locked by another user - try later'.
    RETURN.
  ENDIF.

* Re-check log under the lock
  SELECT SINGLE * FROM zfi_col_postlog INTO ls_log
    WHERE bukrs = p_bukrs AND gjahr = p_gjahr AND poper = p_poper.
  IF sy-subrc = 0.
    gv_error = abap_true.
    PERFORM set_status USING icon_red_light
                             'Period already posted (see ZFI_COL_POSTLOG)'.
    PERFORM dequeue USING lv_varkey.
    RETURN.
  ENDIF.

  IF p_test = abap_true.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING
        documentheader = ls_header
      TABLES
        accountgl      = lt_gl
        currencyamount = lt_curr
        return         = gt_return.
  ELSE.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader = ls_header
      IMPORTING
        obj_key        = lv_objkey
      TABLES
        accountgl      = lt_gl
        currencyamount = lt_curr
        return         = gt_return.
  ENDIF.

  LOOP AT gt_return TRANSPORTING NO FIELDS WHERE type CA 'EAX'.
    gv_error = abap_true.
    EXIT.
  ENDLOOP.

  IF gv_error = abap_true.
    IF p_test IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.
    PERFORM set_status USING icon_red_light ''.
    PERFORM map_errors_to_lines.
  ELSEIF p_test = abap_true.
    PERFORM set_status USING icon_yellow_light
                             'Test run OK - document can be posted'.
  ELSE.
    gv_belnr = lv_objkey(10).

*   Success log - same LUW as the posting
    CLEAR ls_log.
    ls_log-mandt    = sy-mandt.
    ls_log-bukrs    = p_bukrs.
    ls_log-gjahr    = p_gjahr.
    ls_log-poper    = p_poper.
    ls_log-to_bukrs = gv_to_bukrs.
    ls_log-belnr    = gv_belnr.
    ls_log-to_gjahr = lv_objkey+14(4).
    ls_log-budat    = gv_budat.
    ls_log-ernam    = sy-uname.
    ls_log-erdat    = sy-datum.
    ls_log-erzet    = sy-uzeit.
    INSERT zfi_col_postlog FROM ls_log.
    IF sy-subrc <> 0.
      gv_error = abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      PERFORM set_status USING icon_red_light
                               'Log entry exists - posting rolled back'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
      lv_text = |Posted: document { gv_belnr } { gv_to_bukrs } { ls_log-to_gjahr }|.
      PERFORM set_status USING icon_green_light lv_text.
    ENDIF.
  ENDIF.

  PERFORM dequeue USING lv_varkey.
ENDFORM.

*&---------------------------------------------------------------------*
*& Currency rows of one line item :
*&   '00'      document currency (OVC local, COP)    = DMBTR
*&   GV_CURTP2 OVL currency type of USD               = DMBE2
*&   OVL local currency (INR) is translated by the system.
*&---------------------------------------------------------------------*
FORM add_curr TABLES pt_curr  STRUCTURE bapiaccr09
              USING  pv_item  TYPE posnr_acc
                     pv_dmbtr TYPE bseg-dmbtr
                     pv_dmbe2 TYPE bseg-dmbe2.
  DATA ls_curr TYPE bapiaccr09.

  ls_curr-itemno_acc = pv_item.
  ls_curr-curr_type  = '00'.
  ls_curr-currency   = gv_waers.
  ls_curr-amt_doccur = pv_dmbtr * gv_fact.     " internal -> external
  APPEND ls_curr TO pt_curr.

  CLEAR ls_curr.
  ls_curr-itemno_acc = pv_item.
  ls_curr-curr_type  = gv_curtp2.
  ls_curr-currency   = gv_waers2.
  ls_curr-amt_doccur = pv_dmbe2 * gv_fact2.    " internal -> external
  APPEND ls_curr TO pt_curr.
ENDFORM.

*&---------------------------------------------------------------------*
FORM dequeue USING pv_varkey TYPE rstable-varkey.
  CALL FUNCTION 'DEQUEUE_E_TABLE'
    EXPORTING
      tabname = 'ZFI_COL_POSTLOG'
      varkey  = pv_varkey.
ENDFORM.

*&---------------------------------------------------------------------*
FORM set_status USING pv_icon TYPE icon_d
                      pv_text TYPE csequence.
  FIELD-SYMBOLS <ls_out> TYPE ty_out.

  LOOP AT gt_out ASSIGNING <ls_out>.
    <ls_out>-status  = pv_icon.
    <ls_out>-message = pv_text.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& BAPI errors are shown in the ALV itself :
*&   line errors (ACCOUNTGL : 2 rows per output line, CURRENCYAMOUNT :
*&   4 rows per output line) -> message column of that line,
*&   all other errors       -> extra red rows at the end of the list.
*&---------------------------------------------------------------------*
FORM map_errors_to_lines.
  DATA: lv_idx   TYPE i,
        lv_lines TYPE i,
        ls_out   TYPE ty_out.
  FIELD-SYMBOLS: <ls_ret> TYPE bapiret2,
                 <ls_out> TYPE ty_out.

  lv_lines = lines( gt_out ).

  LOOP AT gt_return ASSIGNING <ls_ret> WHERE type CA 'EAX'.
    CLEAR lv_idx.
    IF <ls_ret>-row > 0.
      IF <ls_ret>-parameter = 'ACCOUNTGL'.
        lv_idx = ( <ls_ret>-row + 1 ) DIV 2.
      ELSEIF <ls_ret>-parameter = 'CURRENCYAMOUNT'.
        lv_idx = ( <ls_ret>-row + 3 ) DIV 4.
      ENDIF.
    ENDIF.

    IF lv_idx > 0 AND lv_idx <= lv_lines.
      READ TABLE gt_out ASSIGNING <ls_out> INDEX lv_idx.
      IF <ls_out>-message IS INITIAL.
        <ls_out>-message = <ls_ret>-message.
      ELSE.
        <ls_out>-message = |{ <ls_out>-message }; { <ls_ret>-message }|.
      ENDIF.
    ELSE.
      CLEAR ls_out.
      ls_out-status  = icon_red_light.
      ls_out-message = <ls_ret>-message.
      APPEND ls_out TO gt_out.
    ENDIF.
  ENDLOOP.

  LOOP AT gt_out ASSIGNING <ls_out> FROM 1 TO lv_lines
       WHERE message IS INITIAL.
    <ls_out>-message = 'Not posted - document has errors (see red rows)'.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& ALV output + BAPI messages popup.
*&---------------------------------------------------------------------*
FORM display_alv.
  DATA: lo_alv     TYPE REF TO cl_salv_table,
        lo_msg     TYPE REF TO cl_salv_table,
        lo_cols    TYPE REF TO cl_salv_columns_table,
        lo_display TYPE REF TO cl_salv_display_settings,
        lv_title   TYPE lvc_title,
        lx_salv    TYPE REF TO cx_salv_error.

* Stopped before posting -> show the full error list
  IF gt_out IS INITIAL.
    CHECK gt_return IS NOT INITIAL.
    MESSAGE 'Errors found - nothing posted, see list' TYPE 'S'
            DISPLAY LIKE 'E'.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_msg
                                CHANGING  t_table      = gt_return ).
        lo_msg->get_columns( )->set_optimize( abap_true ).
        lo_msg->get_display_settings( )->set_list_header(
          |Errors { p_bukrs } { p_poper }/{ p_gjahr }| ).
        lo_msg->display( ).
      CATCH cx_salv_error INTO lx_salv.
        MESSAGE lx_salv TYPE 'I'.
    ENDTRY.
    RETURN.
  ENDIF.

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                              CHANGING  t_table      = gt_out ).

      lo_alv->get_functions( )->set_all( abap_true ).
      lo_cols = lo_alv->get_columns( ).
      lo_cols->set_optimize( abap_true ).
      TRY.
          lo_cols->get_column( 'DMBTR' )->set_currency_column( 'WAERS' ).
          lo_cols->get_column( 'DMBE2' )->set_currency_column( 'WAERS2' ).
        CATCH cx_salv_not_found cx_salv_data_error.     "#EC NO_HANDLER
      ENDTRY.

      PERFORM set_col USING lo_cols 'STATUS'       'Status'.
      PERFORM set_col USING lo_cols 'ITEMNO'       'Item'.
      PERFORM set_col USING lo_cols 'KOART'        'Acct Type'.
      PERFORM set_col USING lo_cols 'FROM_HKONT'   'OVC GL / Vendor'.
      PERFORM set_col USING lo_cols 'DMBTR'        'Amount (Doc Curr)'.
      PERFORM set_col USING lo_cols 'WAERS'        'Doc Curr'.
      PERFORM set_col USING lo_cols 'DMBE2'        'Amount (LC2)'.
      PERFORM set_col USING lo_cols 'WAERS2'       'LC2 Curr'.
      PERFORM set_col USING lo_cols 'BSCHL'        'PK'.
      PERFORM set_col USING lo_cols 'TO_HKONT'     'Mapped GL'.
      PERFORM set_col USING lo_cols 'OFF_ITEMNO'   'Offset Item'.
      PERFORM set_col USING lo_cols 'OFFSET_HKONT' 'Offset GL'.
      PERFORM set_col USING lo_cols 'OFF_BSCHL'    'Offset PK'.
      PERFORM set_col USING lo_cols 'OFF_PRCTR'    'Offset PC'.
      PERFORM set_col USING lo_cols 'OFF_GSBER'    'Offset BA'.
      PERFORM set_col USING lo_cols 'MESSAGE'      'Message'.

      lo_alv->get_aggregations( )->add_aggregation( 'DMBTR' ).
      lo_alv->get_aggregations( )->add_aggregation( 'DMBE2' ).

      IF p_test = abap_true.
        lv_title = |TEST RUN { p_bukrs } { p_poper }/{ p_gjahr } -> { gv_to_bukrs } dated { gv_budat DATE = USER } (OVL period { gv_to_poper }/{ gv_to_gjahr })|.
      ELSE.
        lv_title = |{ p_bukrs } { p_poper }/{ p_gjahr } -> { gv_to_bukrs } dated { gv_budat DATE = USER } (OVL period { gv_to_poper }/{ gv_to_gjahr })|.
      ENDIF.
      lo_display = lo_alv->get_display_settings( ).
      lo_display->set_list_header( lv_title ).
      lo_display->set_striped_pattern( abap_true ).

      lo_alv->display( ).
    CATCH cx_salv_error INTO lx_salv.
      MESSAGE lx_salv TYPE 'I'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
FORM set_col USING po_cols TYPE REF TO cl_salv_columns_table
                   pv_name TYPE lvc_fname
                   pv_text TYPE csequence.
  DATA: lo_col  TYPE REF TO cl_salv_column,
        lv_long TYPE scrtext_l,
        lv_med  TYPE scrtext_m,
        lv_shrt TYPE scrtext_s.

  lv_long = pv_text.
  lv_med  = pv_text.
  lv_shrt = pv_text.
  TRY.
      lo_col = po_cols->get_column( pv_name ).
      lo_col->set_long_text( lv_long ).
      lo_col->set_medium_text( lv_med ).
      lo_col->set_short_text( lv_shrt ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.
ENDFORM.
