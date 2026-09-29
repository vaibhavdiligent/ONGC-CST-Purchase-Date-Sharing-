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
*& Output grain: ONE SUMMARY ROW PER REGUT FILE (batch = ZBUKR..LFDNR),
*& showing the batch TOTAL amount and line-item count. Double-click a row
*& to drill into its individual payments (line items), which can be printed.
*& Runs with no REGUT batch yet appear once as "Batch Not Created".
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
         sent_flag  TYPE zfi_paym_file-sent,   "Sent to bank (ZFI_PAYM_FILE-SENT)
         sent_err   TYPE zfi_paym_file-sent_error, "Send failed
         recv_flag  TYPE c LENGTH 1,    "Received back from bank (ZFI_BCM_PAYORDR-ZSTATUS)
         crusr      TYPE regut-tsusr,   "Created by (TemSe user)
         crdate     TYPE regut-tsdat,
         crtime     TYPE regut-tstim,
         ztxn       TYPE zfi_s2s_txn-ztxn, "Payment TXN (file type: CN/CT/CC)
         item_count TYPE i,             "No. of line items (payments) in the file
       END OF ty_mon.

* Detail line (drill-down): one row per payment in the selected batch
* (same payment-medium run = REGUHM-LAUFD_M/LAUFI_M). Shown when the user
* double-clicks a monitor row.
TYPES: BEGIN OF ty_det,
         zbukr    TYPE reguhm-zbukr,
         laufd    TYPE reguhm-laufd_m, "medium run date (the batch)
         laufi    TYPE reguhm-laufi_m, "medium run id   (the batch)
         batchno  TYPE reguhm-batchno,
         lifnr    TYPE reguhm-lifnr,
         kunnr    TYPE reguhm-kunnr,
         name1    TYPE lfa1-name1,     "vendor / customer name
         vblnr    TYPE reguhm-vblnr,   "payment document
         src_laufd TYPE reguhm-laufd,  "F110 run date
         src_laufi TYPE reguhm-laufi,  "F110 run id
         waers    TYPE reguhm-waers,
         rbetr    TYPE p LENGTH 15 DECIMALS 2, "amount (REGUH)
         pyord    TYPE zfi_bcm_payordr-pyord,
         zstatus  TYPE zfi_bcm_payordr-zstatus,
         stat_txt TYPE c LENGTH 15,    "status text
         zutrno   TYPE zfi_bcm_payordr-zutrno,
         zpaydate TYPE zfi_bcm_payordr-zpaydate,
       END OF ty_det.

*----------------------------------------------------------------------*
* Global data
*----------------------------------------------------------------------*
DATA: gt_regut  TYPE STANDARD TABLE OF regut,
      gt_reguhm TYPE STANDARD TABLE OF reguhm,        "FBPM1 medium/batch link
      gt_reguh  TYPE STANDARD TABLE OF reguh,         "F110 payment header (amount)
      gt_sign   TYPE STANDARD TABLE OF zfi_batch_sign,
      gt_paym   TYPE STANDARD TABLE OF zfi_paym_file,  "File send state (SENT)
      gt_payordr TYPE STANDARD TABLE OF zfi_bcm_payordr,"Per-payment bank response (ZSTATUS)
      gt_rule   TYPE STANDARD TABLE OF zfi_bnk_rule,  "Approver config
      gt_s2s    TYPE STANDARD TABLE OF zfi_s2s_txn,   "RZAWE -> TXN (file type)
      gt_mon    TYPE STANDARD TABLE OF ty_mon,        "Summary: one row per REGUT file
      gt_det    TYPE STANDARD TABLE OF ty_det.        "Drill-down detail (line items)

* Approval rules (ZFI_BNK_RULE): rule -> approval level
CONSTANTS: gc_rule_l1 TYPE zfi_bnk_rule-zrule VALUE '90700005',   "Level-1 approvers
           gc_rule_l2 TYPE zfi_bnk_rule-zrule VALUE '90700006'.   "Level-2 approvers

DATA: gv_laufd TYPE reguhm-laufd,
      gv_laufi TYPE reguhm-laufi,
      gv_zbukr TYPE reguhm-zbukr.

*----------------------------------------------------------------------*
* Local event handler - double-click on a monitor row opens the batch
* detail (all payments of that medium run) in a second ALV.
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS on_double_click FOR EVENT double_click
      OF cl_salv_events_table
      IMPORTING row column.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_double_click.
    PERFORM f_show_detail USING row.
  ENDMETHOD.
ENDCLASS.

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
  REFRESH: gt_reguhm, gt_reguh, gt_regut, gt_sign, gt_paym,
           gt_payordr, gt_rule.

* -- Step 1 (per FS): start from REGUHM - the payment medium header
*    created after the F110 run. Selection is on the F110 run.
  SELECT * FROM reguhm INTO TABLE gt_reguhm
    WHERE laufd IN so_laufd
      AND laufi IN so_laufi
      AND zbukr IN so_zbukr.

  IF gt_reguhm IS INITIAL.
    RETURN.
  ENDIF.

* -- Step 1a: F110 payment header (REGUH) for the per-vendor amount.
*    REGUHM carries no amount, so the true payment amount per record comes
*    from REGUH on the same F110 key (run + vendor + payment document).
  SELECT * FROM reguh INTO TABLE gt_reguh
    FOR ALL ENTRIES IN gt_reguhm
    WHERE laufd = gt_reguhm-laufd
      AND laufi = gt_reguhm-laufi
      AND zbukr = gt_reguhm-zbukr
      AND lifnr = gt_reguhm-lifnr
      AND kunnr = gt_reguhm-kunnr
      AND empfg = gt_reguhm-empfg
      AND vblnr = gt_reguhm-vblnr.

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

* -- Step 6: SENT state - ZFI_PAYM_FILE (one row per payment-medium run,
*    keyed by LAUFD/LAUFI = REGUHM-LAUFD_M/LAUFI_M). This is the same
*    Z-table and SENT flag that the operational program ZFI_BNK_APP1 sets
*    when the signed file is transmitted to the bank. (REGUT-STATUS is NOT
*    used: the custom send/return interfaces never update it, so it stays
*    'Created' regardless of the real transfer state.)
  SELECT laufd laufi lfdnr sent sent_error
    FROM zfi_paym_file INTO CORRESPONDING FIELDS OF TABLE gt_paym
    FOR ALL ENTRIES IN gt_reguhm
    WHERE laufd = gt_reguhm-laufd_m
      AND laufi = gt_reguhm-laufi_m.

* -- Payment method -> TXN (file type) map. A REGUT file holds the payments
*    of one payment type (CN/CT/CC...): the file name starts with this TXN
*    and each payment's RZAWE resolves to it via ZFI_S2S_TXN. Used to split
*    the run's payments across its REGUT files and to total per file.
  SELECT * FROM zfi_s2s_txn INTO TABLE gt_s2s.

* -- Step 7: RECEIVED state - per-payment bank response in ZFI_BCM_PAYORDR
*    (ZSTATUS 002 = success, 005 = rejected). This is the Z-table the
*    inbound SBI return-file interface updates per payment order (matched
*    by PYORD), so it is the reliable "received back from bank" signal -
*    the same source the payment report ZFI_BCM_APP_PAYREP uses.
  SELECT * FROM zfi_bcm_payordr INTO TABLE gt_payordr
    FOR ALL ENTRIES IN gt_reguhm
    WHERE laufd = gt_reguhm-laufd
      AND laufi = gt_reguhm-laufi
      AND zbukr = gt_reguhm-zbukr
      AND lifnr = gt_reguhm-lifnr
      AND kunnr = gt_reguhm-kunnr.
ENDFORM.                    " F_GET_DATA

*&---------------------------------------------------------------------*
*&      Form  F_BUILD_OUTPUT
*&---------------------------------------------------------------------*
FORM f_build_output .
  DATA: ls_hm      TYPE reguhm,
        ls_reguh   TYPE reguh,
        ls_reg     TYPE regut,
        ls_rule    TYPE zfi_bnk_rule,
        ls_sign    TYPE zfi_batch_sign,
        ls_paym    TYPE zfi_paym_file,
        ls_po      TYPE zfi_bcm_payordr,
        ls_s2s     TYPE zfi_s2s_txn,
        ls_mon     TYPE ty_mon,
        lv_bkey    TYPE zfi_batch_sign-batch_no,
        lv_snro    TYPE zfi_batch_sign-snro,
        lv_ftxn    TYPE string,          "TXN of the REGUT file (from FSNAM)
        lv_ptxn    TYPE string,          "TXN of a payment (from RZAWE)
        lv_po_tot  TYPE i,
        lv_po_resp TYPE i,
        lv_pay_rcv TYPE abap_bool,
        lv_signed  TYPE abap_bool,
        lv_runkey  TYPE string,
        lt_seen    TYPE STANDARD TABLE OF string.  "medium runs that have a REGUT file

  SORT gt_sign BY batch_no signer snro.

* =====================================================================*
* PART A - one SUMMARY row per REGUT file (LFDNR). This is the batch:
*   REGUT is keyed ZBUKR+BANKS+LAUFD+LAUFI+XVORL+DTKEY+LFDNR, where
*   LAUFD/LAUFI is the payment-medium run. Each file holds the payments of
*   one payment type (TXN). The row shows the batch TOTAL amount and the
*   line-item count; double-click drills into the individual payments.
* =====================================================================*
  LOOP AT gt_regut INTO ls_reg.
    CLEAR ls_mon.
    ls_mon-zbukr      = ls_reg-zbukr.
    ls_mon-banks      = ls_reg-banks.
    ls_mon-laufd      = ls_reg-laufd.       "medium run date
    ls_mon-laufi      = ls_reg-laufi.       "medium run id
    ls_mon-lfdnr      = ls_reg-lfdnr.
    ls_mon-dtkey      = ls_reg-dtkey.
    ls_mon-fsnam      = ls_reg-fsnam.
    ls_mon-waers      = ls_reg-waers.
    ls_mon-crusr      = ls_reg-tsusr.
    ls_mon-crdate     = ls_reg-tsdat.
    ls_mon-crtime     = ls_reg-tstim.
    ls_mon-regut_stat = ls_reg-status.

*   mark this medium run as having a batch (for the "not created" pass)
    CONCATENATE ls_reg-zbukr ls_reg-laufd ls_reg-laufi INTO lv_runkey.
    READ TABLE lt_seen TRANSPORTING NO FIELDS WITH KEY table_line = lv_runkey.
    IF sy-subrc <> 0.
      APPEND lv_runkey TO lt_seen.
    ENDIF.

*   batch key (incl LFDNR) - matches ZFI_BATCH_SIGN / ZFI_PAYMEDIUM_DMEE_20
    CONCATENATE ls_reg-zbukr ls_reg-banks ls_reg-laufd ls_reg-laufi
                ls_reg-xvorl ls_reg-dtkey ls_reg-lfdnr
           INTO lv_bkey RESPECTING BLANKS.
    ls_mon-batch_key = lv_bkey.

*   TXN (payment type) of this file, from the file name
    PERFORM f_file_txn USING ls_reg-fsnam CHANGING lv_ftxn.
    ls_mon-ztxn = lv_ftxn.

*   REGUT file status text
    CASE ls_mon-regut_stat.
      WHEN space. ls_mon-regut_txt = 'Created'.
      WHEN '010'. ls_mon-regut_txt = 'Sent'.
      WHEN '020'. ls_mon-regut_txt = 'Acknowledged'.
      WHEN '030'. ls_mon-regut_txt = 'Transfer Failed'.
      WHEN '040'. ls_mon-regut_txt = 'Transfer Confirmed'.
      WHEN OTHERS. ls_mon-regut_txt = ls_mon-regut_stat.
    ENDCASE.

*   -- Sum the run's payments that belong to THIS file (matched by TXN).
*      Amount = SUM(REGUH-RBETR); also count items and roll up "received".
    CLEAR: lv_po_tot, lv_po_resp.
    LOOP AT gt_reguhm INTO ls_hm WHERE zbukr   = ls_reg-zbukr
                                   AND laufd_m = ls_reg-laufd
                                   AND laufi_m = ls_reg-laufi.
      CLEAR ls_reguh.
      READ TABLE gt_reguh INTO ls_reguh
           WITH KEY laufd = ls_hm-laufd laufi = ls_hm-laufi
                    zbukr = ls_hm-zbukr lifnr = ls_hm-lifnr
                    kunnr = ls_hm-kunnr empfg = ls_hm-empfg
                    vblnr = ls_hm-vblnr.
*     payment TXN via RZAWE -> ZFI_S2S_TXN; keep only this file's payments
      CLEAR lv_ptxn.
      IF sy-subrc = 0.
        READ TABLE gt_s2s INTO ls_s2s WITH KEY rzawe = ls_reguh-rzawe.
        IF sy-subrc = 0.
          lv_ptxn = ls_s2s-ztxn.
        ENDIF.
      ENDIF.
      IF lv_ftxn IS NOT INITIAL AND lv_ptxn IS NOT INITIAL AND lv_ptxn <> lv_ftxn.
        CONTINUE.
      ENDIF.

      ls_mon-item_count = ls_mon-item_count + 1.
      ls_mon-rbetr      = ls_mon-rbetr + ls_reguh-rbetr.
      IF ls_mon-waers    IS INITIAL. ls_mon-waers    = ls_reguh-waers. ENDIF.
      IF ls_mon-batchno  IS INITIAL. ls_mon-batchno  = ls_hm-batchno.  ENDIF.
      IF ls_mon-src_laufd IS INITIAL. ls_mon-src_laufd = ls_hm-laufd.  ENDIF.
      IF ls_mon-f110_runs IS INITIAL. ls_mon-f110_runs = ls_hm-laufi.  ENDIF.

*     received: this payment has a bank response (off status 001)
      lv_pay_rcv = abap_false.
      LOOP AT gt_payordr INTO ls_po WHERE laufd = ls_hm-laufd
                                      AND laufi = ls_hm-laufi
                                      AND zbukr = ls_hm-zbukr
                                      AND lifnr = ls_hm-lifnr
                                      AND kunnr = ls_hm-kunnr.
        IF ls_po-zstatus = '002' OR ls_po-zstatus = '003' OR ls_po-zstatus = '005'.
          lv_pay_rcv = abap_true.
        ENDIF.
      ENDLOOP.
      lv_po_tot = lv_po_tot + 1.
      IF lv_pay_rcv = abap_true.
        lv_po_resp = lv_po_resp + 1.
      ENDIF.
    ENDLOOP.
    IF lv_po_tot > 0 AND lv_po_resp = lv_po_tot.
      ls_mon-recv_flag = 'X'.
    ENDIF.

*   -- Approvers (ZFI_BNK_RULE by company) signed on THIS file's batch key
    LOOP AT gt_rule INTO ls_rule WHERE zrule_id = ls_reg-zbukr.
      CASE ls_rule-zrule.
        WHEN gc_rule_l1.  lv_snro = '1'.
        WHEN gc_rule_l2.  lv_snro = '2'.
        WHEN OTHERS.      CONTINUE.
      ENDCASE.
      CLEAR ls_sign.
      READ TABLE gt_sign INTO ls_sign WITH KEY batch_no = lv_bkey
                                               signer   = ls_rule-zuser
                                               snro     = lv_snro
                                               BINARY SEARCH.
      IF sy-subrc = 0 AND ls_sign-digitl_sign = 'X'.
        lv_signed = abap_true.
      ELSE.
        lv_signed = abap_false.
      ENDIF.
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

*   -- SENT to bank: ZFI_PAYM_FILE-SENT for THIS file (LAUFD/LAUFI/LFDNR)
    CLEAR: ls_paym, ls_mon-sent_flag, ls_mon-sent_err.
    READ TABLE gt_paym INTO ls_paym WITH KEY laufd = ls_reg-laufd
                                             laufi = ls_reg-laufi
                                             lfdnr = ls_reg-lfdnr.
    IF sy-subrc = 0.
      ls_mon-sent_flag = ls_paym-sent.
      ls_mon-sent_err  = ls_paym-sent_error.
    ENDIF.

    PERFORM f_set_status CHANGING ls_mon.

    IF p_pend = abap_true AND
     ( ls_mon-status = 'Approved' OR ls_mon-status = 'Sent to Bank'
       OR ls_mon-status = 'Received from Bank' ).
      CONTINUE.
    ENDIF.
    APPEND ls_mon TO gt_mon.
  ENDLOOP.

* =====================================================================*
* PART B - medium runs with NO REGUT file yet: one "Batch Not Created"
* summary row per F110 run, totalling its payments.
* =====================================================================*
  LOOP AT gt_reguhm INTO ls_hm.
    CONCATENATE ls_hm-zbukr ls_hm-laufd_m ls_hm-laufi_m INTO lv_runkey.
    READ TABLE lt_seen TRANSPORTING NO FIELDS WITH KEY table_line = lv_runkey.
    IF sy-subrc = 0.
      CONTINUE.               "this medium run already has a REGUT batch row
    ENDIF.
    CLEAR ls_reguh.
    READ TABLE gt_reguh INTO ls_reguh
         WITH KEY laufd = ls_hm-laufd laufi = ls_hm-laufi
                  zbukr = ls_hm-zbukr lifnr = ls_hm-lifnr
                  kunnr = ls_hm-kunnr empfg = ls_hm-empfg
                  vblnr = ls_hm-vblnr.
    READ TABLE gt_mon INTO ls_mon WITH KEY zbukr     = ls_hm-zbukr
                                           src_laufd = ls_hm-laufd
                                           f110_runs = ls_hm-laufi
                                           status    = 'Batch Not Created'.
    IF sy-subrc = 0.
      ls_mon-rbetr      = ls_mon-rbetr + ls_reguh-rbetr.
      ls_mon-item_count = ls_mon-item_count + 1.
      MODIFY gt_mon FROM ls_mon INDEX sy-tabix.
    ELSE.
      CLEAR ls_mon.
      ls_mon-zbukr      = ls_hm-zbukr.
      ls_mon-src_laufd  = ls_hm-laufd.
      ls_mon-f110_runs  = ls_hm-laufi.
      ls_mon-laufd      = ls_hm-laufd_m.
      ls_mon-laufi      = ls_hm-laufi_m.
      ls_mon-batchno    = ls_hm-batchno.
      ls_mon-waers      = ls_reguh-waers.
      ls_mon-rbetr      = ls_reguh-rbetr.
      ls_mon-item_count = 1.
      ls_mon-status     = 'Batch Not Created'.
      ls_mon-regut_txt  = 'No Batch'.
      APPEND ls_mon TO gt_mon.
    ENDIF.
  ENDLOOP.

  SORT gt_mon BY zbukr laufd laufi lfdnr.
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
*&      Form  F_FILE_TXN
*&---------------------------------------------------------------------*
*  Extract the TXN (payment type, e.g. CN / CT / CC) from a REGUT file
*  name. The name is built as  [<path>/]<TXN>.<sdate>.<...>.txt, so the
*  TXN is the text before the first '.' of the last path segment.
*----------------------------------------------------------------------*
FORM f_file_txn USING iv_fsnam TYPE regut-fsnam
             CHANGING cv_txn   TYPE string.
  DATA: lv_name TYPE string,
        lv_rest TYPE string,
        lt_seg  TYPE STANDARD TABLE OF string.
  CLEAR cv_txn.
  lv_name = iv_fsnam.
  IF lv_name CS '/'.
    SPLIT lv_name AT '/' INTO TABLE lt_seg.
    READ TABLE lt_seg INDEX lines( lt_seg ) INTO lv_name.
  ENDIF.
  IF lv_name CS '.'.
    SPLIT lv_name AT '.' INTO cv_txn lv_rest.
  ELSE.
    cv_txn = lv_name.
  ENDIF.
  CONDENSE cv_txn.
  TRANSLATE cv_txn TO UPPER CASE.
ENDFORM.                    " F_FILE_TXN

*&---------------------------------------------------------------------*
*&      Form  F_SET_STATUS
*&---------------------------------------------------------------------*
*  Derive the overall batch status (lifecycle: Received > Sent > Approval).
*----------------------------------------------------------------------*
FORM f_set_status CHANGING cs_mon TYPE ty_mon.
  IF cs_mon-recv_flag = 'X'.
    cs_mon-status = 'Received from Bank'.
  ELSEIF cs_mon-sent_flag = 'X'.
    IF cs_mon-sent_err = 'X'.
      cs_mon-status = 'Sent (Error)'.
    ELSE.
      cs_mon-status = 'Sent to Bank'.
    ENDIF.
  ELSEIF cs_mon-l1_total = 0 AND cs_mon-l2_total = 0.
    cs_mon-status = 'No Approvers'.
  ELSEIF cs_mon-l1_total > 0 AND cs_mon-l1_signed < cs_mon-l1_total.
    cs_mon-status = 'Pending L1'.
  ELSEIF cs_mon-l2_total > 0 AND cs_mon-l2_signed < cs_mon-l2_total.
    cs_mon-status = 'Pending L2'.
  ELSE.
    cs_mon-status = 'Approved'.
  ENDIF.
ENDFORM.                    " F_SET_STATUS

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM f_display_alv .
  DATA: lo_alv     TYPE REF TO cl_salv_table,
        lo_cols    TYPE REF TO cl_salv_columns_table,
        lo_funcs   TYPE REF TO cl_salv_functions_list,
        lo_events  TYPE REF TO cl_salv_events_table,
        lo_handler TYPE REF TO lcl_event_handler,
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
  PERFORM f_col_text USING lo_cols 'ZTXN'       'Type'           'Payment Type'         'Payment Type / TXN (file)'.
  PERFORM f_col_text USING lo_cols 'ITEM_COUNT' 'Items'          'No. of Items'         'Number of Line Items in Batch'.
  PERFORM f_col_text USING lo_cols 'RBETR'      'Batch Amt'      'Batch Total'          'Batch Total Amount (sum of line items)'.
  PERFORM f_col_text USING lo_cols 'L1_TOTAL'   'L1 Tot'         'L1 Approvers'         'Level-1 Approvers'.
  PERFORM f_col_text USING lo_cols 'L1_SIGNED'  'L1 Sgn'         'L1 Signed'            'Level-1 Signed'.
  PERFORM f_col_text USING lo_cols 'L1_PENDING' 'L1 Pend'        'L1 Pending With'      'Level-1 Pending With'.
  PERFORM f_col_text USING lo_cols 'L2_TOTAL'   'L2 Tot'         'L2 Approvers'         'Level-2 Approvers'.
  PERFORM f_col_text USING lo_cols 'L2_SIGNED'  'L2 Sgn'         'L2 Signed'            'Level-2 Signed'.
  PERFORM f_col_text USING lo_cols 'L2_PENDING' 'L2 Pend'        'L2 Pending With'      'Level-2 Pending With'.
  PERFORM f_col_text USING lo_cols 'STATUS'     'Status'         'Batch Status'         'Batch Status'.
  PERFORM f_col_text USING lo_cols 'SENT_FLAG'  'Sent'           'Sent to Bank'         'Sent to Bank (ZFI_PAYM_FILE)'.
  PERFORM f_col_text USING lo_cols 'RECV_FLAG'  'Recd'           'Received'             'Received back from Bank'.
  PERFORM f_col_text USING lo_cols 'CRUSR'      'Created By'     'Created By'           'Created By'.

* Hide the REGUT status columns: the custom send/return interfaces never
* update REGUT-STATUS (it stays 'Created'), so it is not a reliable
* lifecycle indicator. The STATUS column carries the real state instead.
  PERFORM f_col_hide USING lo_cols 'REGUT_STAT'.
  PERFORM f_col_hide USING lo_cols 'REGUT_TXT'.
* SENT_ERROR is reflected in STATUS ('Sent (Error)'); keep it off the grid.
  PERFORM f_col_hide USING lo_cols 'SENT_ERR'.
* VENDOR / VBLNR are per-payment fields - they live in the drill-down now,
* not in the batch summary, so hide them on the first ALV.
  PERFORM f_col_hide USING lo_cols 'VENDOR'.
  PERFORM f_col_hide USING lo_cols 'VBLNR'.

* Column order: identifiers + status first, then the batch total / item
* count, then approval detail, then the batch-key / file columns at the end.
  PERFORM f_col_pos USING lo_cols 'ZBUKR'       1.
  PERFORM f_col_pos USING lo_cols 'SRC_LAUFD'   2.
  PERFORM f_col_pos USING lo_cols 'F110_RUNS'   3.
  PERFORM f_col_pos USING lo_cols 'LAUFD'       4.
  PERFORM f_col_pos USING lo_cols 'LAUFI'       5.
  PERFORM f_col_pos USING lo_cols 'LFDNR'       6.
  PERFORM f_col_pos USING lo_cols 'ZTXN'        7.
  PERFORM f_col_pos USING lo_cols 'STATUS'      8.
  PERFORM f_col_pos USING lo_cols 'ITEM_COUNT'  9.
  PERFORM f_col_pos USING lo_cols 'RBETR'      10.
  PERFORM f_col_pos USING lo_cols 'WAERS'      11.
  PERFORM f_col_pos USING lo_cols 'SENT_FLAG'  12.
  PERFORM f_col_pos USING lo_cols 'RECV_FLAG'  13.
  PERFORM f_col_pos USING lo_cols 'L1_TOTAL'   14.
  PERFORM f_col_pos USING lo_cols 'L1_SIGNED'  15.
  PERFORM f_col_pos USING lo_cols 'L1_PENDING' 16.
  PERFORM f_col_pos USING lo_cols 'L2_TOTAL'   17.
  PERFORM f_col_pos USING lo_cols 'L2_SIGNED'  18.
  PERFORM f_col_pos USING lo_cols 'L2_PENDING' 19.
  PERFORM f_col_pos USING lo_cols 'BATCHNO'    20.
  PERFORM f_col_pos USING lo_cols 'BATCH_KEY'  21.
  PERFORM f_col_pos USING lo_cols 'BANKS'      22.
  PERFORM f_col_pos USING lo_cols 'DTKEY'      23.
  PERFORM f_col_pos USING lo_cols 'FSNAM'      24.
  PERFORM f_col_pos USING lo_cols 'CRUSR'      25.

* Enable row selection and register the double-click drill-down: clicking
* a batch row opens its line-item detail (printable).
  lo_alv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
  lo_events = lo_alv->get_event( ).
  CREATE OBJECT lo_handler.
  SET HANDLER lo_handler->on_double_click FOR lo_events.

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

*&---------------------------------------------------------------------*
*&      Form  F_SHOW_DETAIL
*&---------------------------------------------------------------------*
*  Double-click drill-down: build and show all payments belonging to the
*  same batch (payment-medium run) as the clicked monitor row.
*----------------------------------------------------------------------*
FORM f_show_detail USING iv_row TYPE i.
  DATA: ls_mon  TYPE ty_mon,
        ls_hm   TYPE reguhm,
        ls_rh   TYPE reguh,
        ls_po   TYPE zfi_bcm_payordr,
        ls_s2s  TYPE zfi_s2s_txn,
        ls_det  TYPE ty_det,
        lv_ptxn TYPE string,
        lv_rsub TYPE sy-subrc.

  READ TABLE gt_mon INTO ls_mon INDEX iv_row.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  REFRESH gt_det.

* Line items of the clicked summary row:
*  - REGUT-file (batch) row  -> payments of that medium run whose payment
*    type (TXN) matches the file (LAUFD_M/LAUFI_M + TXN).
*  - "Batch Not Created" row -> all payments of that F110 run.
  LOOP AT gt_reguhm INTO ls_hm WHERE zbukr = ls_mon-zbukr.
    IF ls_mon-laufd IS NOT INITIAL OR ls_mon-laufi IS NOT INITIAL.
      IF ls_hm-laufd_m <> ls_mon-laufd OR ls_hm-laufi_m <> ls_mon-laufi.
        CONTINUE.
      ENDIF.
    ELSE.
      IF ls_hm-laufd <> ls_mon-src_laufd OR ls_hm-laufi <> ls_mon-f110_runs.
        CONTINUE.
      ENDIF.
    ENDIF.

*   REGUH (amount + payment method) for this payment
    CLEAR ls_rh.
    READ TABLE gt_reguh INTO ls_rh
         WITH KEY laufd = ls_hm-laufd laufi = ls_hm-laufi
                  zbukr = ls_hm-zbukr lifnr = ls_hm-lifnr
                  kunnr = ls_hm-kunnr empfg = ls_hm-empfg
                  vblnr = ls_hm-vblnr.
    lv_rsub = sy-subrc.

*   TXN filter (batch rows only): keep payments of this file's type
    IF ls_mon-ztxn IS NOT INITIAL.
      CLEAR lv_ptxn.
      IF lv_rsub = 0.
        READ TABLE gt_s2s INTO ls_s2s WITH KEY rzawe = ls_rh-rzawe.
        IF sy-subrc = 0.
          lv_ptxn = ls_s2s-ztxn.
          TRANSLATE lv_ptxn TO UPPER CASE.
        ENDIF.
      ENDIF.
      IF lv_ptxn IS NOT INITIAL AND lv_ptxn <> ls_mon-ztxn.
        CONTINUE.
      ENDIF.
    ENDIF.

    CLEAR ls_det.
    ls_det-zbukr     = ls_hm-zbukr.
    ls_det-laufd     = ls_hm-laufd_m.
    ls_det-laufi     = ls_hm-laufi_m.
    ls_det-batchno   = ls_hm-batchno.
    ls_det-lifnr     = ls_hm-lifnr.
    ls_det-kunnr     = ls_hm-kunnr.
    ls_det-vblnr     = ls_hm-vblnr.
    ls_det-src_laufd = ls_hm-laufd.
    ls_det-src_laufi = ls_hm-laufi.
    ls_det-waers     = ls_hm-waers.

*   Vendor / customer name
    IF ls_hm-lifnr IS NOT INITIAL.
      SELECT SINGLE name1 FROM lfa1 INTO ls_det-name1 WHERE lifnr = ls_hm-lifnr.
    ELSE.
      SELECT SINGLE name1 FROM kna1 INTO ls_det-name1 WHERE kunnr = ls_hm-kunnr.
    ENDIF.

*   Per-vendor amount from the F110 header (REGUH, already read above)
    IF lv_rsub = 0.
      ls_det-rbetr = ls_rh-rbetr.
      IF ls_det-waers IS INITIAL.
        ls_det-waers = ls_rh-waers.
      ENDIF.
    ENDIF.

*   Bank response for this vendor's payment order (ZFI_BCM_PAYORDR)
    READ TABLE gt_payordr INTO ls_po
         WITH KEY laufd = ls_hm-laufd
                  laufi = ls_hm-laufi
                  zbukr = ls_hm-zbukr
                  lifnr = ls_hm-lifnr
                  kunnr = ls_hm-kunnr.
    IF sy-subrc = 0.
      ls_det-pyord    = ls_po-pyord.
      ls_det-zstatus  = ls_po-zstatus.
      ls_det-zutrno   = ls_po-zutrno.
      ls_det-zpaydate = ls_po-zpaydate.
      CASE ls_po-zstatus.
        WHEN '001'.      ls_det-stat_txt = 'Created'.
        WHEN '002'.      ls_det-stat_txt = 'Successful'.
        WHEN '003'.      ls_det-stat_txt = 'Posted'.
        WHEN '005'.      ls_det-stat_txt = 'Rejected'.
        WHEN OTHERS.     ls_det-stat_txt = ls_po-zstatus.
      ENDCASE.
    ENDIF.

    APPEND ls_det TO gt_det.
  ENDLOOP.

  IF gt_det IS INITIAL.
    MESSAGE 'No line items found for this batch' TYPE 'I'.
    RETURN.
  ENDIF.

  SORT gt_det BY lifnr kunnr vblnr.
  PERFORM f_display_detail USING ls_mon.
ENDFORM.                    " F_SHOW_DETAIL

*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_DETAIL
*&---------------------------------------------------------------------*
*  Show the batch detail (gt_det) in a second ALV with print enabled.
*----------------------------------------------------------------------*
FORM f_display_detail USING is_mon TYPE ty_mon.
  DATA: lo_alv   TYPE REF TO cl_salv_table,
        lo_cols  TYPE REF TO cl_salv_columns_table,
        lo_funcs TYPE REF TO cl_salv_functions_list,
        lo_disp  TYPE REF TO cl_salv_display_settings,
        lv_title TYPE lvc_title,
        lx_msg   TYPE REF TO cx_salv_msg.

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = gt_det ).
    CATCH cx_salv_msg INTO lx_msg.
      MESSAGE lx_msg->get_text( ) TYPE 'I'.
      RETURN.
  ENDTRY.

* Full toolbar incl. Print (set_all switches on the standard PRINT button)
  lo_funcs = lo_alv->get_functions( ).
  lo_funcs->set_all( abap_true ).

* Title bar shows which batch is being displayed
  CONCATENATE 'Batch payments -' is_mon-zbukr is_mon-laufi
              'dt' is_mon-laufd
         INTO lv_title SEPARATED BY space.
  lo_disp = lo_alv->get_display_settings( ).
  lo_disp->set_list_header( lv_title ).
  lo_disp->set_striped_pattern( abap_true ).

* Column headings
  lo_cols = lo_alv->get_columns( ).
  lo_cols->set_optimize( abap_true ).
  PERFORM f_col_text USING lo_cols 'ZBUKR'    'Co Code'    'Company Code'     'Company Code'.
  PERFORM f_col_text USING lo_cols 'LAUFD'    'Med Date'   'Medium Run Date'  'Payment Medium Run Date'.
  PERFORM f_col_text USING lo_cols 'LAUFI'    'Med Run'    'Medium Run Id'    'Payment Medium Run Id'.
  PERFORM f_col_text USING lo_cols 'BATCHNO'  'Batch No'   'FBPM1 Batch No'   'FBPM1 Batch Number'.
  PERFORM f_col_text USING lo_cols 'LIFNR'    'Vendor'     'Vendor'           'Vendor'.
  PERFORM f_col_text USING lo_cols 'KUNNR'    'Customer'   'Customer'         'Customer'.
  PERFORM f_col_text USING lo_cols 'NAME1'    'Name'       'Name'             'Vendor / Customer Name'.
  PERFORM f_col_text USING lo_cols 'VBLNR'    'Pay Doc'    'Payment Doc'      'Payment Document'.
  PERFORM f_col_text USING lo_cols 'SRC_LAUFD' 'F110 Date' 'F110 Run Date'    'F110 Run Date'.
  PERFORM f_col_text USING lo_cols 'SRC_LAUFI' 'F110 Run'  'F110 Run Id'      'F110 Run Id'.
  PERFORM f_col_text USING lo_cols 'RBETR'    'Amount'     'Payment Amount'   'Payment Amount (REGUH)'.
  PERFORM f_col_text USING lo_cols 'WAERS'    'Curr'       'Currency'         'Currency'.
  PERFORM f_col_text USING lo_cols 'PYORD'    'Pay Order'  'Payment Order'    'Payment Order (ZFI_BCM_PAYORDR)'.
  PERFORM f_col_text USING lo_cols 'STAT_TXT' 'Status'     'Payment Status'   'Payment Order Status'.
  PERFORM f_col_text USING lo_cols 'ZUTRNO'   'UTR No'     'UTR Number'       'UTR / Transaction Number'.
  PERFORM f_col_text USING lo_cols 'ZPAYDATE' 'Pay Date'   'Payment Date'     'Payment Date'.
* ZSTATUS code is shown as text in STAT_TXT
  PERFORM f_col_hide USING lo_cols 'ZSTATUS'.

* Order: identifiers, vendor, amount, then bank response
  PERFORM f_col_pos USING lo_cols 'ZBUKR'     1.
  PERFORM f_col_pos USING lo_cols 'LIFNR'     2.
  PERFORM f_col_pos USING lo_cols 'KUNNR'     3.
  PERFORM f_col_pos USING lo_cols 'NAME1'     4.
  PERFORM f_col_pos USING lo_cols 'VBLNR'     5.
  PERFORM f_col_pos USING lo_cols 'RBETR'     6.
  PERFORM f_col_pos USING lo_cols 'WAERS'     7.
  PERFORM f_col_pos USING lo_cols 'PYORD'     8.
  PERFORM f_col_pos USING lo_cols 'STAT_TXT'  9.
  PERFORM f_col_pos USING lo_cols 'ZUTRNO'   10.
  PERFORM f_col_pos USING lo_cols 'ZPAYDATE' 11.
  PERFORM f_col_pos USING lo_cols 'LAUFD'    12.
  PERFORM f_col_pos USING lo_cols 'LAUFI'    13.
  PERFORM f_col_pos USING lo_cols 'BATCHNO'  14.
  PERFORM f_col_pos USING lo_cols 'SRC_LAUFD' 15.
  PERFORM f_col_pos USING lo_cols 'SRC_LAUFI' 16.

* Show full-screen on top of the monitor (Back returns to the monitor).
* Full-screen guarantees the complete toolbar, so the standard Print /
* spool button is available for the batch payment list. (A popup would
* offer only a reduced toolbar.)
  lo_alv->display( ).
ENDFORM.                    " F_DISPLAY_DETAIL
