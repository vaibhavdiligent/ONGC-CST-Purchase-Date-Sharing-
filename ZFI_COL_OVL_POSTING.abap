*&---------------------------------------------------------------------*
*& Report  ZFI_COL_OVL_POSTING
*&---------------------------------------------------------------------*
*& Corporate - Colombia : re-post OVC period balances in OVL.
*&   (FS: "FW: FS for Corporate - Colombia", Raju Ashar)
*&
*&   Step 1 : ACDOCA (ledger 0L, RRCTY 0) - documents of the OVC company
*&            code / year / period that hit GL 95671 or 956712.
*&   Step 2 : BSEG of those documents, all other GLs, subtotal of DMBE2
*&            (2nd local currency = USD) per HKONT.
*&   Post   : ONE document in OVL (doc type CL, currency USD - system
*&            converts to INR). For every GL : mapped GL line (key 40/50
*&            by sign, CC / PC / BA from ZFI_COL_GLMAP) + its own offset
*&            line on the offset GL with the opposite key.
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

CONSTANTS: gc_rldnr TYPE fins_ledger VALUE '0L',
           gc_rrcty TYPE rrcty       VALUE '0',
           gc_gl1   TYPE racct       VALUE '95671',
           gc_gl2   TYPE racct       VALUE '956712',
           gc_blart TYPE blart       VALUE 'CL',
           gc_waers TYPE waers       VALUE 'USD',
           gc_bschl_dr TYPE bschl    VALUE '40',
           gc_bschl_cr TYPE bschl    VALUE '50',
           gc_max_items TYPE i       VALUE 999.

TYPES: BEGIN OF ty_doc,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
       END OF ty_doc.

TYPES: BEGIN OF ty_bseg,
         bukrs TYPE bukrs,
         belnr TYPE belnr_d,
         gjahr TYPE gjahr,
         buzei TYPE buzei,
         shkzg TYPE shkzg,
         hkont TYPE hkont,
         dmbe2 TYPE dmbe2,
       END OF ty_bseg.

TYPES: BEGIN OF ty_sum,
         hkont TYPE hkont,
         dmbe2 TYPE dmbe2,
       END OF ty_sum.

TYPES: BEGIN OF ty_out,
         status       TYPE icon_d,
         itemno       TYPE posnr_acc,
         from_hkont   TYPE hkont,
         dmbe2        TYPE dmbe2,
         waers        TYPE waers,
         bschl        TYPE bschl,
         to_hkont     TYPE hkont,
         kostl        TYPE kostl,
         prctr        TYPE prctr,
         gsber        TYPE gsber,
         off_itemno   TYPE posnr_acc,
         offset_hkont TYPE hkont,
         off_bschl    TYPE bschl,
         message      TYPE bapi_msg,
       END OF ty_out.

DATA: gt_doc    TYPE STANDARD TABLE OF ty_doc,
      gt_bseg   TYPE STANDARD TABLE OF ty_bseg,
      gt_sum    TYPE SORTED TABLE OF ty_sum WITH UNIQUE KEY hkont,
      gt_map    TYPE SORTED TABLE OF zfi_col_glmap
                     WITH UNIQUE KEY from_bukrs from_hkont to_bukrs,
      gt_out    TYPE STANDARD TABLE OF ty_out,
      gt_return TYPE STANDARD TABLE OF bapiret2,
      gr_racct  TYPE RANGE OF racct.

DATA: gv_to_bukrs TYPE bukrs,
      gv_budat    TYPE budat,
      gv_to_gjahr TYPE gjahr,
      gv_to_poper TYPE poper,
      gv_belnr    TYPE belnr_d,
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
    MESSAGE e001(00) WITH 'Company code' p_bukrs 'does not exist'.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'BUKRS' FIELD p_bukrs
    ID 'ACTVT' FIELD '03'.
  IF sy-subrc <> 0.
    MESSAGE e001(00) WITH 'No authorization to display company code'
                          p_bukrs.
  ENDIF.

AT SELECTION-SCREEN ON p_poper.
  IF p_poper < 1 OR p_poper > 16.
    MESSAGE e001(00) WITH 'Period must be between 001 and 016'.
  ENDIF.

AT SELECTION-SCREEN.
  PERFORM check_already_posted.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  PERFORM build_racct_range.
  PERFORM get_documents.
  CHECK gv_error IS INITIAL.
  PERFORM get_subtotals.
  CHECK gv_error IS INITIAL.
  PERFORM read_mapping.
  CHECK gv_error IS INITIAL.
  PERFORM get_posting_date.
  CHECK gv_error IS INITIAL.
  PERFORM validate_mapping.
  CHECK gv_error IS INITIAL.
  PERFORM build_and_post.
  PERFORM display_alv.

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
    MESSAGE e001(00) WITH 'Period already posted: OVL document'
                          ls_log-belnr ls_log-to_bukrs ls_log-to_gjahr.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& GL 95671 / 956712 with leading zeros (internal format).
*&---------------------------------------------------------------------*
FORM build_racct_range.
  DATA: ls_racct LIKE LINE OF gr_racct,
        lv_gl    TYPE racct.

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
    gv_error = abap_true.
    MESSAGE s001(00) WITH 'No documents found on GL 95671 / 956712 for'
                          p_bukrs p_poper p_gjahr DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Step 2 - subtotal DMBE2 per HKONT (other than 95671 / 956712).
*&---------------------------------------------------------------------*
FORM get_subtotals.
  DATA: ls_sum   TYPE ty_sum,
        lv_lines TYPE i.
  FIELD-SYMBOLS <ls_bseg> TYPE ty_bseg.

  SELECT bukrs, belnr, gjahr, buzei, shkzg, hkont, dmbe2
    FROM bseg
    FOR ALL ENTRIES IN @gt_doc
    WHERE bukrs = @p_bukrs
      AND belnr = @gt_doc-belnr
      AND gjahr = @gt_doc-gjahr
      AND hkont NOT IN @gr_racct
    INTO TABLE @gt_bseg.

  LOOP AT gt_bseg ASSIGNING <ls_bseg>.
    CLEAR ls_sum.
    ls_sum-hkont = <ls_bseg>-hkont.
    IF <ls_bseg>-shkzg = 'H'.              " credit -> negative
      ls_sum-dmbe2 = - <ls_bseg>-dmbe2.
    ELSE.
      ls_sum-dmbe2 = <ls_bseg>-dmbe2.
    ENDIF.
    COLLECT ls_sum INTO gt_sum.
  ENDLOOP.

  DELETE gt_sum WHERE dmbe2 = 0.

  IF gt_sum IS INITIAL.
    gv_error = abap_true.
    MESSAGE s001(00) WITH 'No amounts (DMBE2) to post for the documents'
                          DISPLAY LIKE 'E'.
  ELSEIF lines( gt_sum ) * 2 > gc_max_items.
    gv_error = abap_true.
    lv_lines = lines( gt_sum ).
    MESSAGE s001(00) WITH 'Too many GLs for one document (max 499):'
                          lv_lines DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Read mapping ZFI_COL_GLMAP - every GL must be mapped, all to the same
*& target company code (one document).
*&---------------------------------------------------------------------*
FORM read_mapping.
  DATA: lt_to_bukrs TYPE SORTED TABLE OF bukrs WITH UNIQUE KEY table_line,
        lv_missing  TYPE string,
        lv_count    TYPE i,
        lv_gl_out   TYPE hkont.
  FIELD-SYMBOLS: <ls_sum> TYPE ty_sum,
                 <ls_map> TYPE zfi_col_glmap.

  SELECT * FROM zfi_col_glmap INTO TABLE gt_map
    WHERE from_bukrs = p_bukrs.

  LOOP AT gt_sum ASSIGNING <ls_sum>.
    CLEAR lv_count.
    LOOP AT gt_map ASSIGNING <ls_map> WHERE from_hkont = <ls_sum>-hkont.
      lv_count = lv_count + 1.
      INSERT <ls_map>-to_bukrs INTO TABLE lt_to_bukrs.
    ENDLOOP.
    IF lv_count = 0.
      WRITE <ls_sum>-hkont TO lv_gl_out NO-ZERO.
      CONDENSE lv_gl_out.
      lv_missing = |{ lv_missing } { lv_gl_out }|.
    ELSEIF lv_count > 1.
      gv_error = abap_true.
      MESSAGE s001(00) WITH 'GL' <ls_sum>-hkont
                            'mapped to more than one To Co.Code'
                            DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDLOOP.

  IF lv_missing IS NOT INITIAL.
    gv_error = abap_true.
    MESSAGE s001(00) WITH 'GL not mapped in ZFI_COL_GLMAP:' lv_missing
                          DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF lines( lt_to_bukrs ) <> 1.
    gv_error = abap_true.
    MESSAGE s001(00) WITH 'Mapped GLs belong to different To Co.Codes'
                          '- one document needs one company code'
                          DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  READ TABLE lt_to_bukrs INTO gv_to_bukrs INDEX 1.

  IF p_test IS INITIAL.
    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
      ID 'BUKRS' FIELD gv_to_bukrs
      ID 'ACTVT' FIELD '01'.
    IF sy-subrc <> 0.
      gv_error = abap_true.
      MESSAGE s001(00) WITH 'No authorization to post in company code'
                            gv_to_bukrs DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Last day of the period in OVC's fiscal year variant = posting date /
*& document date in OVL. OVL period is derived from that date.
*&---------------------------------------------------------------------*
FORM get_posting_date.
  DATA: lv_periv_from TYPE periv,
        lv_periv_to   TYPE periv.

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
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.
  IF sy-subrc <> 0 OR gv_budat IS INITIAL.
    gv_error = abap_true.
    MESSAGE s001(00) WITH 'Cannot determine last day of period'
                          p_poper p_gjahr lv_periv_from DISPLAY LIKE 'E'.
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
    gv_error = abap_true.
    MESSAGE s001(00) WITH 'Cannot determine OVL period for' gv_budat
                          gv_to_bukrs DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Mapping master data in the To Co.Code (as per FS ZTable):
*&   GL / offset GL -> SKB1, CC -> CSKS, PC -> CEPC (valid on posting
*&   date). GSBER -> TGSB.  All errors are listed together.
*&---------------------------------------------------------------------*
FORM validate_mapping.
  DATA: lv_errors TYPE string,
        lv_dummy  TYPE char1.
  FIELD-SYMBOLS: <ls_sum> TYPE ty_sum,
                 <ls_map> TYPE zfi_col_glmap.

  LOOP AT gt_sum ASSIGNING <ls_sum>.
    READ TABLE gt_map ASSIGNING <ls_map>
      WITH KEY from_bukrs = p_bukrs
               from_hkont = <ls_sum>-hkont
               to_bukrs   = gv_to_bukrs.
    CHECK sy-subrc = 0.

    SELECT SINGLE @abap_true FROM skb1
      WHERE bukrs = @gv_to_bukrs AND saknr = @<ls_map>-to_hkont
      INTO @lv_dummy.
    IF sy-subrc <> 0.
      lv_errors = |{ lv_errors } GL { <ls_map>-to_hkont ALPHA = OUT };|.
    ENDIF.

    SELECT SINGLE @abap_true FROM skb1
      WHERE bukrs = @gv_to_bukrs AND saknr = @<ls_map>-offset_hkont
      INTO @lv_dummy.
    IF sy-subrc <> 0.
      lv_errors = |{ lv_errors } Offset GL { <ls_map>-offset_hkont ALPHA = OUT };|.
    ENDIF.

    IF <ls_map>-kostl IS NOT INITIAL.
      SELECT SINGLE @abap_true FROM csks
        WHERE kostl = @<ls_map>-kostl
          AND bukrs = @gv_to_bukrs
          AND datab <= @gv_budat
          AND datbi >= @gv_budat
        INTO @lv_dummy.
      IF sy-subrc <> 0.
        lv_errors = |{ lv_errors } CC { <ls_map>-kostl ALPHA = OUT };|.
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
        lv_errors = |{ lv_errors } PC { <ls_map>-prctr ALPHA = OUT };|.
      ENDIF.
    ENDIF.

    IF <ls_map>-gsber IS NOT INITIAL.
      SELECT SINGLE @abap_true FROM tgsb
        WHERE gsber = @<ls_map>-gsber
        INTO @lv_dummy.
      IF sy-subrc <> 0.
        lv_errors = |{ lv_errors } BA { <ls_map>-gsber };|.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lv_errors IS NOT INITIAL.
    gv_error = abap_true.
    MESSAGE s001(00) WITH 'Invalid mapping for' gv_to_bukrs ':'
                          lv_errors DISPLAY LIKE 'E'.
  ENDIF.
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
        ls_curr   TYPE bapiaccr09,
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
               from_hkont = <ls_sum>-hkont
               to_bukrs   = gv_to_bukrs.
    CHECK sy-subrc = 0.

    CLEAR ls_out.
    ls_out-from_hkont   = <ls_sum>-hkont.
    ls_out-dmbe2        = <ls_sum>-dmbe2.
    ls_out-waers        = gc_waers.
    ls_out-to_hkont     = <ls_map>-to_hkont.
    ls_out-kostl        = <ls_map>-kostl.
    ls_out-prctr        = <ls_map>-prctr.
    ls_out-gsber        = <ls_map>-gsber.
    ls_out-offset_hkont = <ls_map>-offset_hkont.
    IF <ls_sum>-dmbe2 > 0.
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
    ls_gl-item_text  = |{ p_bukrs } GL { <ls_sum>-hkont ALPHA = OUT }|.
    ls_gl-costcenter = <ls_map>-kostl.
    ls_gl-profit_ctr = <ls_map>-prctr.
    ls_gl-bus_area   = <ls_map>-gsber.
    APPEND ls_gl TO lt_gl.
    CLEAR ls_curr.
    ls_curr-itemno_acc = lv_item.
    ls_curr-curr_type  = '00'.            " document currency (USD)
    ls_curr-currency   = gc_waers.        " INR derived by the system
    ls_curr-amt_doccur = <ls_sum>-dmbe2.
    APPEND ls_curr TO lt_curr.

*   Offset line - opposite sign / posting key
    lv_item = lv_item + 1.
    ls_out-off_itemno = lv_item.
    CLEAR ls_gl.
    ls_gl-itemno_acc = lv_item.
    ls_gl-gl_account = <ls_map>-offset_hkont.
    ls_gl-item_text  = |Offset { p_bukrs } GL { <ls_sum>-hkont ALPHA = OUT }|.
    ls_gl-profit_ctr = <ls_map>-prctr.
    ls_gl-bus_area   = <ls_map>-gsber.
    APPEND ls_gl TO lt_gl.
    CLEAR ls_curr.
    ls_curr-itemno_acc = lv_item.
    ls_curr-curr_type  = '00'.
    ls_curr-currency   = gc_waers.
    ls_curr-amt_doccur = - <ls_sum>-dmbe2.
    APPEND ls_curr TO lt_curr.

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
    PERFORM set_status USING icon_red_light 'Error - see messages'.
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
*& BAPI line-level errors (ROW = index in ACCOUNTGL / CURRENCYAMOUNT,
*& 2 rows per output line) are shown against the output line.
*&---------------------------------------------------------------------*
FORM map_errors_to_lines.
  DATA lv_idx TYPE i.
  FIELD-SYMBOLS: <ls_ret> TYPE bapiret2,
                 <ls_out> TYPE ty_out.

  LOOP AT gt_return ASSIGNING <ls_ret>
       WHERE type CA 'EAX'
         AND ( parameter = 'ACCOUNTGL' OR parameter = 'CURRENCYAMOUNT' )
         AND row > 0.
    lv_idx = ( <ls_ret>-row + 1 ) DIV 2.
    READ TABLE gt_out ASSIGNING <ls_out> INDEX lv_idx.
    IF sy-subrc = 0.
      <ls_out>-message = <ls_ret>-message.
    ENDIF.
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

  CHECK gt_out IS NOT INITIAL.

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                              CHANGING  t_table      = gt_out ).

      lo_alv->get_functions( )->set_all( abap_true ).
      lo_cols = lo_alv->get_columns( ).
      lo_cols->set_optimize( abap_true ).
      TRY.
          lo_cols->get_column( 'DMBE2' )->set_currency_column( 'WAERS' ).
        CATCH cx_salv_not_found cx_salv_data_error.     "#EC NO_HANDLER
      ENDTRY.

      PERFORM set_col USING lo_cols 'STATUS'       'Status'.
      PERFORM set_col USING lo_cols 'ITEMNO'       'Item'.
      PERFORM set_col USING lo_cols 'FROM_HKONT'   'OVC GL'.
      PERFORM set_col USING lo_cols 'DMBE2'        'Amount (USD)'.
      PERFORM set_col USING lo_cols 'BSCHL'        'PK'.
      PERFORM set_col USING lo_cols 'TO_HKONT'     'Mapped GL'.
      PERFORM set_col USING lo_cols 'OFF_ITEMNO'   'Offset Item'.
      PERFORM set_col USING lo_cols 'OFFSET_HKONT' 'Offset GL'.
      PERFORM set_col USING lo_cols 'OFF_BSCHL'    'Offset PK'.
      PERFORM set_col USING lo_cols 'MESSAGE'      'Message'.

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

      IF gt_return IS NOT INITIAL AND gv_error = abap_true.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_msg
                                CHANGING  t_table      = gt_return ).
        lo_msg->get_columns( )->set_optimize( abap_true ).
        lo_msg->set_screen_popup( start_column = 5  end_column = 150
                                  start_line   = 3  end_line   = 20 ).
        lo_msg->display( ).
      ENDIF.
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
