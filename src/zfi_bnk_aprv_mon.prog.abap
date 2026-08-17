*&---------------------------------------------------------------------*
*& Report  ZFI_BNK_APRV_MON
*&---------------------------------------------------------------------*
*& Payment Batch Approval Monitor
*&
*& Monitors the approval status of payment batches for release to the
*& bank (F110 / Payment Factory dual-control flow).
*&
*& Data flow (per FS "F110 Payment Run") - starting table is REGUHM:
*&   F110 run          -> REGUH / REGUP          (payment run data)
*&   Payment medium    -> REGUHM                 (LAUFD/LAUFI = F110 run,
*&                                                LAUFD_M/LAUFI_M = medium run,
*&                                                BATCHNO = batch)
*&   Batch created     -> REGUT                  (REGUT-LAUFD/LAUFI are the
*&                                                medium run = REGUHM-LAUFD_M/
*&                                                LAUFI_M)
*&   Digital signature -> ZFI_BATCH_SIGN         (dual-control approval)
*&
*& The report starts from REGUHM (selection is on the F110 run), resolves
*& the medium run, then reads REGUT - the single source of truth for batch
*& approval - and evaluates the digital signatures held in ZFI_BATCH_SIGN.
*&
*& Output grain: ONE ROW PER REGUHM RECORD. Every REGUHM entry is shown,
*& even when no REGUT batch exists yet (status "Batch Not Created").
*&
*& Batch key (matches ZFI_BNK_APP / ZFI_BNK_APP1 / ZFI_PAYMEDIUM_DMEE_20):
*&   ZBUKR + BANKS + LAUFD + LAUFI + XVORL + DTKEY + LFDNR
*&   (41 chars, built RESPECTING BLANKS) -> stored in ZFI_BATCH_SIGN-BATCH_NO
*&
*& Approval levels (see ZFI_BNK_RULE):
*&   SNRO 1 = level-1 approvers, SNRO 2 = level-2 approvers
*&---------------------------------------------------------------------*
REPORT zfi_bnk_aprv_mon.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_mon,
         batch_key  TYPE c LENGTH 45,   "Concatenated REGUT key
         zbukr      TYPE regut-zbukr,
         banks      TYPE regut-banks,
         laufd      TYPE regut-laufd,   "Payment medium run date (REGUT / REGUHM-LAUFD_M)
         laufi      TYPE regut-laufi,   "Payment medium run id   (REGUT / REGUHM-LAUFI_M)
         batchno    TYPE reguhm-batchno,"FBPM1 batch number       (REGUHM)
         src_laufd  TYPE reguhm-laufd,  "F110 run date            (REGUHM-LAUFD)
         f110_runs  TYPE c LENGTH 60,   "F110 run id              (REGUHM-LAUFI)
         vendor     TYPE c LENGTH 10,   "Vendor / customer        (REGUHM LIFNR/KUNNR)
         vblnr      TYPE reguhm-vblnr,  "Payment document         (REGUHM-VBLNR)
         dtkey      TYPE regut-dtkey,
         lfdnr      TYPE regut-lfdnr,
         waers      TYPE regut-waers,
         rbetr      TYPE p LENGTH 15 DECIMALS 2,   "Amount (from REGUT-RBETR)
         fsnam      TYPE regut-fsnam,
         l1_total   TYPE i,             "Level-1 approvers assigned
         l1_signed  TYPE i,             "Level-1 approvers signed
         l1_pending TYPE c LENGTH 60,   "Level-1 pending signers
         l2_total   TYPE i,             "Level-2 approvers assigned
         l2_signed  TYPE i,             "Level-2 approvers signed
         l2_pending TYPE c LENGTH 60,   "Level-2 pending signers
         status     TYPE c LENGTH 20,   "Overall approval status
         regut_stat TYPE regut-status,  "REGUT file status code (EPIC_REGUT_STATUS)
         regut_txt  TYPE c LENGTH 20,   "REGUT file status text
         sent_flag  TYPE zfi_paym_file-sent,   "Sent to bank
         crusr      TYPE regut-tsusr,   "Created by (TemSe user)
         crdate     TYPE regut-tsdat,
         crtime     TYPE regut-tstim,
       END OF ty_mon.

*----------------------------------------------------------------------*
* Global data
*----------------------------------------------------------------------*
DATA: gt_regut  TYPE STANDARD TABLE OF regut,
      gt_reguhm TYPE STANDARD TABLE OF reguhm,        "FBPM1 medium/batch link
      gt_sign   TYPE STANDARD TABLE OF zfi_batch_sign,
      gt_paym   TYPE STANDARD TABLE OF zfi_paym_file,
      gt_rule   TYPE STANDARD TABLE OF zfi_bnk_rule,  "Approver config
      gt_mon    TYPE STANDARD TABLE OF ty_mon.

* Approval rules (ZFI_BNK_RULE): rule -> approval level
CONSTANTS: gc_rule_l1 TYPE zfi_bnk_rule-zrule VALUE '90700005',   "Level-1 approvers
           gc_rule_l2 TYPE zfi_bnk_rule-zrule VALUE '90700006'.   "Level-2 approvers

DATA: gv_laufd TYPE reguhm-laufd,
      gv_laufi TYPE reguhm-laufi,
      gv_zbukr TYPE reguhm-zbukr.

*----------------------------------------------------------------------*
* Selection screen - the F110 payment run (REGUHM = starting table)
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: so_laufd FOR gv_laufd,   "F110 run date (REGUHM-LAUFD)
                so_laufi FOR gv_laufi,   "F110 run id   (REGUHM-LAUFI)
                so_zbukr FOR gv_zbukr.   "Paying company code
PARAMETERS: p_pend AS CHECKBOX.          "Only pending (not fully approved)
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* F4 value help for the run identifier (LAUFI) - list existing runs
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_laufi-low.
  PERFORM f_f4_laufi CHANGING so_laufi-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_laufi-high.
  PERFORM f_f4_laufi CHANGING so_laufi-high.

*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM f_get_data.
  PERFORM f_build_output.
  PERFORM f_display_alv.

*&---------------------------------------------------------------------*
*&      Form  F_GET_DATA
*&---------------------------------------------------------------------*
FORM f_get_data .
  REFRESH: gt_reguhm, gt_regut, gt_sign, gt_paym, gt_rule.

* -- Step 1 (per FS): start from REGUHM - the payment medium header
*    created after the F110 run. Selection is on the F110 run.
  SELECT * FROM reguhm INTO TABLE gt_reguhm
    WHERE laufd IN so_laufd
      AND laufi IN so_laufi
      AND zbukr IN so_zbukr.

  IF gt_reguhm IS INITIAL.
    RETURN.
  ENDIF.

* -- Step 2: read the batches from REGUT for the medium runs referenced by
*    REGUHM. REGUT-LAUFD/LAUFI is the medium run = REGUHM-LAUFD_M/LAUFI_M.
*    FOR ALL ENTRIES already removes duplicate driver tuples, so no batch
*    is lost and no manual de-duplication is needed. One REGUT row = one
*    batch, keyed by the concatenated REGUT key (as in ZFI_BNK_APP).
  SELECT * FROM regut INTO TABLE gt_regut
    FOR ALL ENTRIES IN gt_reguhm
    WHERE zbukr = gt_reguhm-zbukr
      AND laufd = gt_reguhm-laufd_m
      AND laufi = gt_reguhm-laufi_m.

  IF gt_regut IS INITIAL.
    RETURN.
  ENDIF.

* -- Step 4: configured approvers (ZFI_BNK_RULE): 90700005 = L1, 90700006 = L2
  SELECT * FROM zfi_bnk_rule INTO TABLE gt_rule
    WHERE zrule = gc_rule_l1
       OR zrule = gc_rule_l2.

* -- Step 5: signature records for the batches (BATCH_NO = concatenated key)
  DATA: lt_keys TYPE STANDARD TABLE OF zfi_batch_sign-batch_no,
        lv_key  TYPE zfi_batch_sign-batch_no,
        ls_reg  TYPE regut.

  LOOP AT gt_regut INTO ls_reg.
    CLEAR lv_key.
    CONCATENATE ls_reg-zbukr ls_reg-banks ls_reg-laufd ls_reg-laufi
                ls_reg-xvorl ls_reg-dtkey ls_reg-lfdnr
           INTO lv_key RESPECTING BLANKS.
    APPEND lv_key TO lt_keys.
  ENDLOOP.
  SORT lt_keys.
  DELETE ADJACENT DUPLICATES FROM lt_keys.

  IF lt_keys IS NOT INITIAL.
    SELECT * FROM zfi_batch_sign INTO TABLE gt_sign
      FOR ALL ENTRIES IN lt_keys
      WHERE batch_no = lt_keys-table_line.
  ENDIF.

* -- Step 6: payment file (sent status) - one row per medium LAUFD/LAUFI.
*    Select only the needed fields (avoid pulling LOB columns via FAE).
  SELECT laufd laufi sent FROM zfi_paym_file
    INTO CORRESPONDING FIELDS OF TABLE gt_paym
    FOR ALL ENTRIES IN gt_regut
    WHERE laufd = gt_regut-laufd
      AND laufi = gt_regut-laufi.
ENDFORM.                    " F_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  F_BUILD_OUTPUT
*&---------------------------------------------------------------------*
FORM f_build_output .
  DATA: ls_hm      TYPE reguhm,
        ls_reg     TYPE regut,
        ls_rule    TYPE zfi_bnk_rule,
        ls_sign    TYPE zfi_batch_sign,
        ls_paym    TYPE zfi_paym_file,
        ls_mon     TYPE ty_mon,
        lv_bkey    TYPE zfi_batch_sign-batch_no,
        lv_snro    TYPE zfi_batch_sign-snro,
        lt_bkeys   TYPE STANDARD TABLE OF zfi_batch_sign-batch_no,
        lv_nbatch  TYPE i,
        lv_signed  TYPE abap_bool.

  SORT gt_sign BY batch_no signer snro.

* ---------------------------------------------------------------------*
* One output row per REGUHM record (the FS starting table). Each REGUHM
* record is linked to its payment-medium batch(es) in REGUT via
*   REGUT-LAUFD/LAUFI = REGUHM-LAUFD_M/LAUFI_M.
* A record with no REGUT batch yet is shown as "Batch Not Created".
* ---------------------------------------------------------------------*
  LOOP AT gt_reguhm INTO ls_hm.
    CLEAR ls_mon.

*   -- REGUHM (F110 / vendor) attributes
    ls_mon-zbukr     = ls_hm-zbukr.
    ls_mon-src_laufd = ls_hm-laufd.        "F110 run date
    ls_mon-f110_runs = ls_hm-laufi.        "F110 run id
    ls_mon-laufd     = ls_hm-laufd_m.      "medium run date
    ls_mon-laufi     = ls_hm-laufi_m.      "medium run id
    ls_mon-batchno   = ls_hm-batchno.
    ls_mon-dtkey     = ls_hm-hbkid.
    ls_mon-vblnr     = ls_hm-vblnr.
    ls_mon-waers     = ls_hm-waers.        "currency per record (REGUHM)
    IF ls_hm-lifnr IS NOT INITIAL.
      ls_mon-vendor = ls_hm-lifnr.
    ELSE.
      ls_mon-vendor = ls_hm-kunnr.
    ENDIF.

*   -- Linked REGUT batch(es) for this record's medium run
    REFRESH lt_bkeys.
    CLEAR lv_nbatch.
    LOOP AT gt_regut INTO ls_reg
         WHERE zbukr = ls_hm-zbukr
           AND laufd = ls_hm-laufd_m
           AND laufi = ls_hm-laufi_m.
      lv_nbatch = lv_nbatch + 1.
      CONCATENATE ls_reg-zbukr ls_reg-banks ls_reg-laufd ls_reg-laufi
                  ls_reg-xvorl ls_reg-dtkey ls_reg-lfdnr
             INTO lv_bkey RESPECTING BLANKS.
      APPEND lv_bkey TO lt_bkeys.
*     representative batch attributes (first REGUT file of the medium run)
      IF ls_mon-batch_key IS INITIAL.
        ls_mon-batch_key  = lv_bkey.
        ls_mon-banks      = ls_reg-banks.
        ls_mon-lfdnr      = ls_reg-lfdnr.
        ls_mon-rbetr      = ls_reg-rbetr.   "amount from REGUT batch (REGUHM has none)
        IF ls_mon-waers IS INITIAL.
          ls_mon-waers    = ls_reg-waers.   "fallback if REGUHM currency empty
        ENDIF.
        ls_mon-fsnam      = ls_reg-fsnam.
        ls_mon-crusr      = ls_reg-tsusr.
        ls_mon-crdate     = ls_reg-tsdat.
        ls_mon-crtime     = ls_reg-tstim.
        ls_mon-regut_stat = ls_reg-status.
      ENDIF.
    ENDLOOP.

*   -- No batch created yet: show the REGUHM record as pending
    IF lv_nbatch = 0.
      ls_mon-status    = 'Batch Not Created'.
      ls_mon-regut_txt = 'No Batch'.
      APPEND ls_mon TO gt_mon.
      CONTINUE.
    ENDIF.

*   -- REGUT file status text (representative batch) - EPIC_REGUT_STATUS
    CASE ls_mon-regut_stat.
      WHEN space. ls_mon-regut_txt = 'Created'.
      WHEN '010'. ls_mon-regut_txt = 'Sent'.
      WHEN '020'. ls_mon-regut_txt = 'Acknowledged'.
      WHEN '030'. ls_mon-regut_txt = 'Transfer Failed'.
      WHEN '040'. ls_mon-regut_txt = 'Transfer Confirmed'.
      WHEN OTHERS. ls_mon-regut_txt = ls_mon-regut_stat.
    ENDCASE.

*   -- Approvers from config (ZFI_BNK_RULE by company). An approver counts
*      as signed only when signed on ALL batch files of the medium run.
    LOOP AT gt_rule INTO ls_rule WHERE zrule_id = ls_hm-zbukr.
      CASE ls_rule-zrule.
        WHEN gc_rule_l1.  lv_snro = '1'.
        WHEN gc_rule_l2.  lv_snro = '2'.
        WHEN OTHERS.      CONTINUE.
      ENDCASE.

      lv_signed = abap_true.
      LOOP AT lt_bkeys INTO lv_bkey.
        CLEAR ls_sign.
        READ TABLE gt_sign INTO ls_sign WITH KEY batch_no = lv_bkey
                                                 signer   = ls_rule-zuser
                                                 snro     = lv_snro
                                                 BINARY SEARCH.
        IF sy-subrc <> 0 OR ls_sign-digitl_sign <> 'X'.
          lv_signed = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_snro = '1'.
        ls_mon-l1_total = ls_mon-l1_total + 1.
        IF lv_signed = abap_true.
          ls_mon-l1_signed = ls_mon-l1_signed + 1.
        ELSE.
          PERFORM f_add_pending USING ls_rule-zuser CHANGING ls_mon-l1_pending.
        ENDIF.
      ELSE.
        ls_mon-l2_total = ls_mon-l2_total + 1.
        IF lv_signed = abap_true.
          ls_mon-l2_signed = ls_mon-l2_signed + 1.
        ELSE.
          PERFORM f_add_pending USING ls_rule-zuser CHANGING ls_mon-l2_pending.
        ENDIF.
      ENDIF.
    ENDLOOP.

*   -- Sent to bank: authoritative from REGUT status; paym file as fallback
    IF ls_mon-regut_stat = '010' OR ls_mon-regut_stat = '020'
                                  OR ls_mon-regut_stat = '040'.
      ls_mon-sent_flag = 'X'.
    ENDIF.
    READ TABLE gt_paym INTO ls_paym WITH KEY laufd = ls_hm-laufd_m
                                             laufi = ls_hm-laufi_m.
    IF sy-subrc = 0 AND ls_paym-sent = 'X'.
      ls_mon-sent_flag = 'X'.
    ENDIF.

*   -- Overall status
    IF ls_mon-sent_flag = 'X'.
      ls_mon-status = 'Sent to Bank'.
    ELSEIF ls_mon-l1_total = 0 AND ls_mon-l2_total = 0.
      ls_mon-status = 'No Approvers'.
    ELSEIF ls_mon-l1_total > 0 AND ls_mon-l1_signed < ls_mon-l1_total.
      ls_mon-status = 'Pending L1'.
    ELSEIF ls_mon-l2_total > 0 AND ls_mon-l2_signed < ls_mon-l2_total.
      ls_mon-status = 'Pending L2'.
    ELSE.
      ls_mon-status = 'Approved'.
    ENDIF.

*   -- Optional filter: only pending records
    IF p_pend = abap_true AND
     ( ls_mon-status = 'Approved' OR ls_mon-status = 'Sent to Bank' ).
      CONTINUE.
    ENDIF.

    APPEND ls_mon TO gt_mon.
  ENDLOOP.

  SORT gt_mon BY zbukr src_laufd f110_runs vendor.
ENDFORM.                    " F_BUILD_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  F_ADD_PENDING
*&---------------------------------------------------------------------*
*  Append a pending signer to the comma-separated pending list
*----------------------------------------------------------------------*
FORM f_add_pending USING iv_signer TYPE c
                CHANGING cv_pending TYPE c.
  IF cv_pending IS INITIAL.
    cv_pending = iv_signer.
  ELSE.
    CONCATENATE cv_pending iv_signer INTO cv_pending SEPARATED BY ','.
  ENDIF.
ENDFORM.                    " F_ADD_PENDING

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM f_display_alv .
  DATA: lo_alv     TYPE REF TO cl_salv_table,
        lo_cols    TYPE REF TO cl_salv_columns_table,
        lo_funcs   TYPE REF TO cl_salv_functions_list,
        lx_msg     TYPE REF TO cx_salv_msg.

  IF gt_mon IS INITIAL.
    MESSAGE 'No payment batches found for the selection' TYPE 'I'.
    RETURN.
  ENDIF.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = gt_mon ).
    CATCH cx_salv_msg INTO lx_msg.
      MESSAGE lx_msg->get_text( ) TYPE 'I'.
      RETURN.
  ENDTRY.

* Toolbar / standard functions
  lo_funcs = lo_alv->get_functions( ).
  lo_funcs->set_all( abap_true ).

* Column headers and width optimization
  lo_cols = lo_alv->get_columns( ).
  lo_cols->set_optimize( abap_true ).

  PERFORM f_col_text USING lo_cols 'BATCH_KEY'  'Batch Key'      'Batch Key'            'Batch Key (REGUT)'.
  PERFORM f_col_text USING lo_cols 'LAUFD'      'Med Date'       'Medium Run Date'      'Payment Medium Run Date'.
  PERFORM f_col_text USING lo_cols 'LAUFI'      'Med Run'        'Medium Run Id'        'Payment Medium Run Id'.
  PERFORM f_col_text USING lo_cols 'BATCHNO'    'Batch No'       'FBPM1 Batch No'       'FBPM1 Batch Number (REGUHM)'.
  PERFORM f_col_text USING lo_cols 'SRC_LAUFD'  'F110 Date'      'F110 Run Date'        'F110 Run Date (REGUHM)'.
  PERFORM f_col_text USING lo_cols 'F110_RUNS'  'F110 Run'       'F110 Run Id'          'F110 Run Id (REGUHM)'.
  PERFORM f_col_text USING lo_cols 'VENDOR'     'Vendor'         'Vendor/Customer'      'Vendor / Customer (REGUHM)'.
  PERFORM f_col_text USING lo_cols 'VBLNR'      'Pay Doc'        'Payment Doc'          'Payment Document (REGUHM)'.
  PERFORM f_col_text USING lo_cols 'RBETR'      'Amount'         'Batch Amount'         'Batch Amount (REGUT)'.
  PERFORM f_col_text USING lo_cols 'L1_TOTAL'   'L1 Tot'         'L1 Approvers'         'Level-1 Approvers'.
  PERFORM f_col_text USING lo_cols 'L1_SIGNED'  'L1 Sgn'         'L1 Signed'            'Level-1 Signed'.
  PERFORM f_col_text USING lo_cols 'L1_PENDING' 'L1 Pend'        'L1 Pending With'      'Level-1 Pending With'.
  PERFORM f_col_text USING lo_cols 'L2_TOTAL'   'L2 Tot'         'L2 Approvers'         'Level-2 Approvers'.
  PERFORM f_col_text USING lo_cols 'L2_SIGNED'  'L2 Sgn'         'L2 Signed'            'Level-2 Signed'.
  PERFORM f_col_text USING lo_cols 'L2_PENDING' 'L2 Pend'        'L2 Pending With'      'Level-2 Pending With'.
  PERFORM f_col_text USING lo_cols 'STATUS'     'Status'         'Approval Status'      'Approval Status'.
  PERFORM f_col_text USING lo_cols 'REGUT_TXT'  'File Stat'      'REGUT File Status'    'REGUT File Status (source of truth)'.
  PERFORM f_col_text USING lo_cols 'SENT_FLAG'  'Sent'           'Sent to Bank'         'Sent to Bank'.
  PERFORM f_col_text USING lo_cols 'CRUSR'      'Created By'     'Created By'           'Created By'.

* Hide the raw REGUT status code (readable text shown via REGUT_TXT)
  PERFORM f_col_hide USING lo_cols 'REGUT_STAT'.

* Column order: identifiers + status first (always populated), then the
* approval detail, then the batch / file columns (blank until the batch
* is created) at the end.
  PERFORM f_col_pos USING lo_cols 'ZBUKR'       1.
  PERFORM f_col_pos USING lo_cols 'SRC_LAUFD'   2.
  PERFORM f_col_pos USING lo_cols 'F110_RUNS'   3.
  PERFORM f_col_pos USING lo_cols 'VENDOR'      4.
  PERFORM f_col_pos USING lo_cols 'VBLNR'       5.
  PERFORM f_col_pos USING lo_cols 'STATUS'      6.
  PERFORM f_col_pos USING lo_cols 'REGUT_TXT'   7.
  PERFORM f_col_pos USING lo_cols 'L1_TOTAL'    8.
  PERFORM f_col_pos USING lo_cols 'L1_SIGNED'   9.
  PERFORM f_col_pos USING lo_cols 'L1_PENDING' 10.
  PERFORM f_col_pos USING lo_cols 'L2_TOTAL'   11.
  PERFORM f_col_pos USING lo_cols 'L2_SIGNED'  12.
  PERFORM f_col_pos USING lo_cols 'L2_PENDING' 13.
  PERFORM f_col_pos USING lo_cols 'SENT_FLAG'  14.
  PERFORM f_col_pos USING lo_cols 'LAUFD'      15.
  PERFORM f_col_pos USING lo_cols 'LAUFI'      16.
  PERFORM f_col_pos USING lo_cols 'BATCHNO'    17.
  PERFORM f_col_pos USING lo_cols 'RBETR'      18.
  PERFORM f_col_pos USING lo_cols 'WAERS'      19.
  PERFORM f_col_pos USING lo_cols 'BATCH_KEY'  20.
  PERFORM f_col_pos USING lo_cols 'BANKS'      21.
  PERFORM f_col_pos USING lo_cols 'DTKEY'      22.
  PERFORM f_col_pos USING lo_cols 'LFDNR'      23.
  PERFORM f_col_pos USING lo_cols 'FSNAM'      24.
  PERFORM f_col_pos USING lo_cols 'CRUSR'      25.

  lo_alv->display( ).
ENDFORM.                    " F_DISPLAY_ALV

*&---------------------------------------------------------------------*
*&      Form  F_COL_POS
*&---------------------------------------------------------------------*
*  Place a column at a given position in the ALV output
*----------------------------------------------------------------------*
FORM f_col_pos USING io_cols TYPE REF TO cl_salv_columns_table
                     iv_col  TYPE lvc_fname
                     iv_pos  TYPE i.
  DATA: lx_err TYPE REF TO cx_root.
  TRY.
      io_cols->set_column_position( columnname = iv_col
                                    position   = iv_pos ).
    CATCH cx_root INTO lx_err.
*     column not present / position clash - ignore
  ENDTRY.
ENDFORM.                    " F_COL_POS

*&---------------------------------------------------------------------*
*&      Form  F_COL_HIDE
*&---------------------------------------------------------------------*
*  Make a column technical (not displayed)
*----------------------------------------------------------------------*
FORM f_col_hide USING io_cols TYPE REF TO cl_salv_columns_table
                      iv_col  TYPE lvc_fname.
  DATA: lo_col TYPE REF TO cl_salv_column,
        lx_nf  TYPE REF TO cx_salv_not_found.
  TRY.
      lo_col = io_cols->get_column( iv_col ).
      lo_col->set_technical( abap_true ).
    CATCH cx_salv_not_found INTO lx_nf.
*     column not present - ignore
  ENDTRY.
ENDFORM.                    " F_COL_HIDE

*&---------------------------------------------------------------------*
*&      Form  F_COL_TEXT
*&---------------------------------------------------------------------*
FORM f_col_text USING io_cols  TYPE REF TO cl_salv_columns_table
                      iv_col   TYPE lvc_fname
                      iv_short TYPE scrtext_s
                      iv_med   TYPE scrtext_m
                      iv_long  TYPE scrtext_l.
  DATA: lo_col TYPE REF TO cl_salv_column,
        lx_nf  TYPE REF TO cx_salv_not_found.
  TRY.
      lo_col = io_cols->get_column( iv_col ).
      lo_col->set_short_text( iv_short ).
      lo_col->set_medium_text( iv_med ).
      lo_col->set_long_text( iv_long ).
    CATCH cx_salv_not_found INTO lx_nf.
*     column not present - ignore
  ENDTRY.
ENDFORM.                    " F_COL_TEXT

*&---------------------------------------------------------------------*
*&      Form  F_F4_LAUFI
*&---------------------------------------------------------------------*
*  Value help (F4) for the run identifier - shows existing F110 payment
*  runs (Run date + Run id) from REGUHM and returns the selected LAUFI.
*----------------------------------------------------------------------*
FORM f_f4_laufi CHANGING cv_laufi TYPE reguhm-laufi.
  TYPES: BEGIN OF lty_help,
           laufd TYPE reguhm-laufd,
           laufi TYPE reguhm-laufi,
         END OF lty_help.

  DATA: lt_help   TYPE STANDARD TABLE OF lty_help,
        lt_return TYPE STANDARD TABLE OF ddshretval,
        ls_return TYPE ddshretval.

  SELECT DISTINCT laufd laufi FROM reguhm
    INTO TABLE lt_help
    UP TO 500 ROWS
    WHERE laufi IN so_laufi.
  IF lt_help IS INITIAL.
*   fall back to all runs if the current restriction returns nothing
    SELECT DISTINCT laufd laufi FROM reguhm
      INTO TABLE lt_help
      UP TO 500 ROWS.
  ENDIF.
  SORT lt_help BY laufd DESCENDING laufi ASCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LAUFI'
      value_org       = 'S'
    TABLES
      value_tab       = lt_help
      return_tab      = lt_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
  IF sy-subrc = 0.
    READ TABLE lt_return INTO ls_return INDEX 1.
    IF sy-subrc = 0.
      cv_laufi = ls_return-fieldval.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_F4_LAUFI
