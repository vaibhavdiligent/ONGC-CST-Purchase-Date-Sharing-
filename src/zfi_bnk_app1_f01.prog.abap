*&---------------------------------------------------------------------*
*& Include          ZFI_BNK_APP1_F01
*&---------------------------------------------------------------------*
*& Data source changed from BNK_BATCH_HEADER to REGUT.
*& REGUT-GUID is not populated, so the new unique key is the concatenated
*& REGUT primary key:
*&   ZBUKR + BANKS + LAUFD + LAUFI + XVORL + DTKEY + LFDNR  (41 chars,
*&   built with RESPECTING BLANKS so field positions are fixed/unique).
*& This concatenated key is carried in GT_FINAL-GUID and stored in
*& ZFI_BATCH_SIGN-BATCH_NO (which must be widened to CHAR 45).
*& REGUT is joined to ZFI_PAYM_FILE on LAUFD / LAUFI.
*& REGUT field mapping used for the output structure GT_FINAL:
*&   RBETR -> BATCH_SUM   WAERS -> BATCH_CURR
*&   TSUSR -> CRUSR       TSDAT -> CRDATE      TSTIM -> CRTIME
*&   DWUSR -> CHUSR       DWDAT -> CHDATE      DWTIM -> CHTIME
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_OP_TAB1
*&---------------------------------------------------------------------*
FORM f_prepare_op_tab1 .
  DATA: lv_key TYPE c LENGTH 45.

  REFRESH : gt_paym, gt_batch_header,gt_batch_sign,gt_final.
  SELECT * FROM zfi_paym_file INTO TABLE gt_paym.

* Do NOT hide on the file-level flag: one file holds many batches, so this
* would drop every batch of the file after only one batch is approved.
* Approved batches are excluded per-batch via DIGITL_SIGN below instead.
*  DELETE gt_paym WHERE file_data_sent IS NOT INITIAL.
  DELETE gt_paym WHERE raw_data IS INITIAL.

  IF gt_paym IS NOT INITIAL.
    SELECT * FROM regut INTO TABLE gt_batch_header
      FOR ALL ENTRIES IN gt_paym
      WHERE laufi = gt_paym-laufi
      AND   laufd = gt_paym-laufd.

*   Self-heal: make sure every REGUT batch has its approver rows. The DMEE
*   format module predicts LFDNR (MAX+1) before REGUT is committed, so a
*   batch created later in the same run can be left with no ZFI_BATCH_SIGN
*   rows and would silently drop out of the worklist below.
    PERFORM f_ensure_batch_sign USING gt_batch_header.

    IF gt_batch_header IS NOT INITIAL.
*     Read this user's level-1 signature assignments
      SELECT * FROM  zfi_batch_sign INTO TABLE gt_batch_sign
        WHERE signer = sy-uname
        AND   digitl_sign = ' '    "only batches not yet approved by this user (level 1)
        and   snro   = '1'.


      sort gt_batch_sign by CDATE1 CTIME1.
      LOOP AT gt_batch_header INTO gs_batch_header.
*       Build unique key from the REGUT primary key fields
        CLEAR lv_key.
        CONCATENATE gs_batch_header-zbukr gs_batch_header-banks
                    gs_batch_header-laufd gs_batch_header-laufi
                    gs_batch_header-xvorl gs_batch_header-dtkey
                    gs_batch_header-lfdnr
               INTO lv_key RESPECTING BLANKS.

        READ TABLE gt_batch_sign INTO gs_batch_sign WITH KEY batch_no = lv_key
                                                             signer = sy-uname
                                                             snro = '1'.
        IF sy-subrc EQ 0 .
*    if gs_batch_sign-signer = sy-uname   and gs_batch_sign-digitl_sign = ' '.
          MOVE-CORRESPONDING gs_batch_header TO gs_final.
          gs_final-guid       = lv_key.
          gs_final-batch_sum  = gs_batch_header-rbetr.
          gs_final-batch_curr = gs_batch_header-waers.
          gs_final-crusr      = gs_batch_header-tsusr.
          gs_final-crdate     = gs_batch_header-tsdat.
          gs_final-crtime     = gs_batch_header-tstim.
          gs_final-chusr      = gs_batch_header-dwusr.
          gs_final-chdate     = gs_batch_header-dwdat.
          gs_final-chtime     = gs_batch_header-dwtim.
          READ TABLE gt_paym INTO gs_paym WITH KEY laufd = gs_batch_header-laufd
                                                   laufi = gs_batch_header-laufi.
          IF sy-subrc = 0.
            gs_final-raw_data = gs_paym-raw_data.
          ENDIF.
          APPEND gs_final TO gt_final.
        ENDIF.
*    endif.
        CLEAR : gs_batch_header,gs_batch_sign,gs_final.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_PREPARE_OP_TAB1

*&---------------------------------------------------------------------*
*&      Form  F_ENSURE_BATCH_SIGN
*&---------------------------------------------------------------------*
*  Guarantee every REGUT batch in gt_batch_header has its approver rows
*  in ZFI_BATCH_SIGN, keyed to the ACTUAL REGUT key.
*
*  Why: the DMEE format module ZFI_PAYMEDIUM_DMEE_20 creates the approver
*  rows using a PREDICTED LFDNR (SELECT MAX( lfdnr ) + 1) while REGUT is
*  not yet committed. When several batches are created in one run the
*  prediction repeats, so a later batch's MODIFY overwrites an earlier
*  key instead of creating its own - leaving that REGUT batch with no
*  approver rows, and it drops out of every worklist. Here we re-create
*  any missing rows from ZFI_BNK_RULE against the real REGUT key, so no
*  batch can be orphaned. Existing (incl. already-signed) rows are left
*  untouched.
*----------------------------------------------------------------------*
FORM f_ensure_batch_sign USING it_hdr TYPE STANDARD TABLE.
  DATA: lt_rule TYPE STANDARD TABLE OF zfi_bnk_rule,
        ls_rule TYPE zfi_bnk_rule,
        ls_hdr  TYPE regut,
        ls_sign TYPE zfi_batch_sign,
        lt_new  TYPE STANDARD TABLE OF zfi_batch_sign,
        lv_key  TYPE zfi_batch_sign-batch_no,
        lv_cnt  TYPE i.

  CHECK it_hdr IS NOT INITIAL.

* Configured approvers: rule 90700005 = level 1, 90700006 = level 2
  SELECT * FROM zfi_bnk_rule INTO TABLE lt_rule
    WHERE zrule = '90700005' OR zrule = '90700006'.
  IF lt_rule IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT it_hdr INTO ls_hdr.
    CLEAR lv_key.
    CONCATENATE ls_hdr-zbukr ls_hdr-banks
                ls_hdr-laufd ls_hdr-laufi
                ls_hdr-xvorl ls_hdr-dtkey
                ls_hdr-lfdnr
           INTO lv_key RESPECTING BLANKS.

*   already has approver rows -> leave as is
    SELECT COUNT(*) INTO lv_cnt FROM zfi_batch_sign WHERE batch_no = lv_key.
    IF lv_cnt > 0.
      CONTINUE.
    ENDIF.

*   create the approver rows for this batch's paying company code
    LOOP AT lt_rule INTO ls_rule WHERE zrule_id = ls_hdr-zbukr.
      CLEAR ls_sign.
      ls_sign-batch_no = lv_key.
      ls_sign-signer   = ls_rule-zuser.
      CASE ls_rule-zrule.
        WHEN '90700005'. ls_sign-snro = '1'.
        WHEN '90700006'. ls_sign-snro = '2'.
        WHEN OTHERS.     CONTINUE.
      ENDCASE.
      ls_sign-cdate = sy-datum.
      ls_sign-ctime = sy-uzeit.
      APPEND ls_sign TO lt_new.
    ENDLOOP.
  ENDLOOP.

  IF lt_new IS NOT INITIAL.
    INSERT zfi_batch_sign FROM TABLE lt_new ACCEPTING DUPLICATE KEYS.
    COMMIT WORK.
  ENDIF.
ENDFORM.                    " F_ENSURE_BATCH_SIGN

*&---------------------------------------------------------------------*
*&      Form  F_ALV_BUILD_FIELDCAT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM f_alv_build_fieldcat1 .
  DATA lv_fldcat1 TYPE lvc_s_fcat.
  DATA: lv_index1 TYPE sy-index.
  REFRESH : it_fcat1.
  CLEAR lv_fldcat1.
  lv_fldcat1-tabname   = 'GT_FINAL'.
  lv_fldcat1-fieldname = 'GUID'.
  lv_fldcat1-scrtext_m = 'Batch Key'.
  lv_fldcat1-outputlen = '45'.
*  lv_fldcat1-row_pos   = '1'.
  lv_fldcat1-col_pos   = lv_index1.
  APPEND lv_fldcat1 TO it_fcat1.

* RULE_ID / ITEM_CNT not available in REGUT - column removed

  lv_index1 = lv_index1 + 1.
  CLEAR lv_fldcat1.
  lv_fldcat1-tabname   = 'GT_FINAL'.
  lv_fldcat1-fieldname = 'LAUFD'.
  lv_fldcat1-scrtext_m = 'Run Date'.
  lv_fldcat1-outputlen = '11'.
*  lv_fldcat1-row_pos   = '1'.
  lv_fldcat1-col_pos   = lv_index1.
  APPEND lv_fldcat1 TO it_fcat1.

  lv_index1 = lv_index1 + 1.
  CLEAR lv_fldcat1.
  lv_fldcat1-tabname   = 'GT_FINAL'.
  lv_fldcat1-fieldname = 'LAUFI'.
  lv_fldcat1-scrtext_m = 'Run Id'.
  lv_fldcat1-outputlen = '8'.
*  lv_fldcat1-row_pos   = '1'.
  lv_fldcat1-col_pos   = lv_index1.
  APPEND lv_fldcat1 TO it_fcat1.

* LAUFD_F / LAUFI_F (file date/id) not available in REGUT - column removed

  lv_index1 = lv_index1 + 1.
  CLEAR lv_fldcat1.
  lv_fldcat1-tabname   = 'GT_FINAL'.
  lv_fldcat1-fieldname = 'BATCH_SUM'.
  lv_fldcat1-scrtext_m = 'Batch Amount'.
  lv_fldcat1-outputlen = '15'.
*  lv_fldcat1-row_pos   = '1'.
  lv_fldcat1-col_pos   = lv_index1.
  APPEND lv_fldcat1 TO it_fcat1.

  lv_index1 = lv_index1 + 1.
  CLEAR lv_fldcat1.
  lv_fldcat1-tabname   = 'GT_FINAL'.
  lv_fldcat1-fieldname = 'CRUSR'.
  lv_fldcat1-scrtext_m = 'Create User'.
  lv_fldcat1-outputlen = '12'.
*  lv_fldcat1-row_pos   = '1'.
  lv_fldcat1-col_pos   = lv_index1.
  APPEND lv_fldcat1 TO it_fcat1.

  lv_index1 = lv_index1 + 1.
  CLEAR lv_fldcat1.
  lv_fldcat1-tabname   = 'GT_FINAL'.
  lv_fldcat1-fieldname = 'CRTIME'.
  lv_fldcat1-scrtext_m = 'Create Time'.
  lv_fldcat1-outputlen = '11'.
  lv_fldcat1-row_pos   = '1'.
  lv_fldcat1-col_pos   = lv_index1.
  APPEND lv_fldcat1 TO it_fcat1.

  lv_index1 = lv_index1 + 1.
  CLEAR lv_fldcat1.
  lv_fldcat1-tabname   = 'GT_FINAL'.
  lv_fldcat1-fieldname = 'CRDATE'.
  lv_fldcat1-scrtext_m = 'Create Date'.
  lv_fldcat1-outputlen = '11'.
*  lv_fldcat1-row_pos   = '1'.
  lv_fldcat1-col_pos   = lv_index1.
  APPEND lv_fldcat1 TO it_fcat1.

  lv_index1 = lv_index1 + 1.
  CLEAR lv_fldcat1.
  lv_fldcat1-tabname   = 'GT_FINAL'.
  lv_fldcat1-fieldname = 'CHUSR'.
  lv_fldcat1-scrtext_m = 'Change User'.
  lv_fldcat1-outputlen = '12'.
*  lv_fldcat1-row_pos   = '1'.
  lv_fldcat1-col_pos   = lv_index1.
  APPEND lv_fldcat1 TO it_fcat1.

  lv_index1 = lv_index1 + 1.
  CLEAR lv_fldcat1.
  lv_fldcat1-tabname   = 'GT_FINAL'.
  lv_fldcat1-fieldname = 'CHTIME'.
  lv_fldcat1-scrtext_m = 'Change Time'.
  lv_fldcat1-outputlen = '11'.
*  lv_fldcat1-row_pos   = '1'.
  lv_fldcat1-col_pos   = lv_index1.
  APPEND lv_fldcat1 TO it_fcat1.

  lv_index1 = lv_index1 + 1.
  CLEAR lv_fldcat1.
  lv_fldcat1-tabname   = 'GT_FINAL'.
  lv_fldcat1-fieldname = 'CHDATE'.
  lv_fldcat1-scrtext_m = 'Change Date'.
  lv_fldcat1-outputlen = '11'.
*  lv_fldcat1-row_pos   = '1'.
  lv_fldcat1-col_pos   = lv_index1.
  APPEND lv_fldcat1 TO it_fcat1.

  lv_index1 = lv_index1 + 1.
  CLEAR lv_fldcat1.
  lv_fldcat1-tabname   = 'GT_FINAL'.
  lv_fldcat1-fieldname = 'ZBUKR'.
  lv_fldcat1-scrtext_m = 'Paying company code'.
  lv_fldcat1-outputlen = '19'.
*  lv_fldcat1-row_pos   = '1'.
  lv_fldcat1-col_pos   = lv_index1.
  APPEND lv_fldcat1 TO it_fcat1.

* TOT_BTCH_AMT not available in REGUT - column removed

ENDFORM.                    " F_ALV_BUILD_FIELDCAT1
*&---------------------------------------------------------------------*
*&      Form  F_ALV_REPORT_LAYOUT1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM f_alv_report_layout1 .
  CLEAR : it_layout1.
  it_layout1-cwidth_opt = 'X'.
  it_layout1-sel_mode   = 'A'.
ENDFORM.                    " F_ALV_REPORT_LAYOUT1
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_OP_TAB2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM f_prepare_op_tab2 .
  DATA: lv_key TYPE c LENGTH 45.
*  BREAK sab_vaibhav.
  REFRESH : gt_paym2, gt_batch_header2,gt_final2, gt_batch_sign2.
  SELECT * FROM zfi_paym_file INTO TABLE gt_paym2 WHERE sent = ' '.

  DELETE gt_paym2 WHERE file_data_sent IS INITIAL.

  IF gt_paym2 IS NOT INITIAL.
    SELECT * FROM regut INTO TABLE gt_batch_header2
      FOR ALL ENTRIES IN gt_paym2
      WHERE laufi = gt_paym2-laufi
      AND   laufd = gt_paym2-laufd.

*   Self-heal missing approver rows (see F_ENSURE_BATCH_SIGN) so no batch
*   drops out of the level-2 worklist either.
    PERFORM f_ensure_batch_sign USING gt_batch_header2.

    IF gt_batch_header2 IS NOT INITIAL.
*     Read this user's open level-2 signature assignments
      SELECT * FROM  zfi_batch_sign INTO TABLE gt_batch_sign2
        WHERE DIGITL_SIGN = ' '
        AND   signer eq sy-uname
        and   snro = '2'.

      LOOP AT gt_batch_header2 INTO gs_batch_header2.
        CLEAR lv_key.
        CONCATENATE gs_batch_header2-zbukr gs_batch_header2-banks
                    gs_batch_header2-laufd gs_batch_header2-laufi
                    gs_batch_header2-xvorl gs_batch_header2-dtkey
                    gs_batch_header2-lfdnr
               INTO lv_key RESPECTING BLANKS.

        READ TABLE gt_batch_sign2 INTO gs_batch_sign2 WITH KEY batch_no = lv_key
                                                              signer = sy-uname
                                                              snro = '2'.
*       IF sy-subrc <> 0.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING gs_batch_header2 TO gs_final2.
          gs_final2-guid       = lv_key.
          gs_final2-batch_sum  = gs_batch_header2-rbetr.
          gs_final2-batch_curr = gs_batch_header2-waers.
          gs_final2-crusr      = gs_batch_header2-tsusr.
          gs_final2-crdate     = gs_batch_header2-tsdat.
          gs_final2-crtime     = gs_batch_header2-tstim.
          gs_final2-chusr      = gs_batch_header2-dwusr.
          gs_final2-chdate     = gs_batch_header2-dwdat.
          gs_final2-chtime     = gs_batch_header2-dwtim.
          READ TABLE gt_paym2 INTO gs_paym2 WITH KEY laufd = gs_batch_header2-laufd
                                                     laufi = gs_batch_header2-laufi.
          IF sy-subrc = 0.
            gs_final2-raw_data = gs_paym2-raw_data.
            gs_final2-file_data_sent = gs_paym2-file_data_sent.
          ENDIF.
          APPEND gs_final2 TO gt_final2.
        ENDIF.
*    endif.
        CLEAR : gs_batch_header2,gs_batch_sign2,gs_final2.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_PREPARE_OP_TAB2
*&---------------------------------------------------------------------*
*&      Form  F_ALV_BUILD_FIELDCAT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM f_alv_build_fieldcat2 .
  DATA lv_fldcat2 TYPE lvc_s_fcat.
  DATA: lv_index2 TYPE sy-index.
  REFRESH : it_fcat2.
  CLEAR lv_fldcat2.
  lv_fldcat2-tabname   = 'GT_FINAL2'.
  lv_fldcat2-fieldname = 'GUID'.
  lv_fldcat2-scrtext_m = 'Batch Key'.
  lv_fldcat2-outputlen = '45'.
*  lv_fldcat2-row_pos   = '1'.
  lv_fldcat2-col_pos   = lv_index2.
  APPEND lv_fldcat2 TO it_fcat2.

* RULE_ID / ITEM_CNT not available in REGUT - column removed

  lv_index2 = lv_index2 + 1.
  CLEAR lv_fldcat2.
  lv_fldcat2-tabname   = 'GT_FINAL2'.
  lv_fldcat2-fieldname = 'LAUFD'.
  lv_fldcat2-scrtext_m = 'Run Date'.
  lv_fldcat2-outputlen = '11'.
*  lv_fldcat2-row_pos   = '1'.
  lv_fldcat2-col_pos   = lv_index2.
  APPEND lv_fldcat2 TO it_fcat2.

  lv_index2 = lv_index2 + 1.
  CLEAR lv_fldcat2.
  lv_fldcat2-tabname   = 'GT_FINAL2'.
  lv_fldcat2-fieldname = 'LAUFI'.
  lv_fldcat2-scrtext_m = 'Run Id'.
  lv_fldcat2-outputlen = '8'.
*  lv_fldcat2-row_pos   = '1'.
  lv_fldcat2-col_pos   = lv_index2.
  APPEND lv_fldcat2 TO it_fcat2.

* LAUFD_F / LAUFI_F (file date/id) not available in REGUT - column removed

  lv_index2 = lv_index2 + 1.
  CLEAR lv_fldcat2.
  lv_fldcat2-tabname   = 'GT_FINAL2'.
  lv_fldcat2-fieldname = 'BATCH_SUM'.
  lv_fldcat2-scrtext_m = 'Batch Amount'.
  lv_fldcat2-outputlen = '15'.
*  lv_fldcat2-row_pos   = '1'.
  lv_fldcat2-col_pos   = lv_index2.
  APPEND lv_fldcat2 TO it_fcat2.

  lv_index2 = lv_index2 + 1.
  CLEAR lv_fldcat2.
  lv_fldcat2-tabname   = 'GT_FINAL2'.
  lv_fldcat2-fieldname = 'CRUSR'.
  lv_fldcat2-scrtext_m = 'Create User'.
  lv_fldcat2-outputlen = '12'.
*  lv_fldcat2-row_pos   = '1'.
  lv_fldcat2-col_pos   = lv_index2.
  APPEND lv_fldcat2 TO it_fcat2.

  lv_index2 = lv_index2 + 1.
  CLEAR lv_fldcat2.
  lv_fldcat2-tabname   = 'GT_FINAL2'.
  lv_fldcat2-fieldname = 'CRTIME'.
  lv_fldcat2-scrtext_m = 'Create Time'.
  lv_fldcat2-outputlen = '11'.
  lv_fldcat2-row_pos   = '1'.
  lv_fldcat2-col_pos   = lv_index2.
  APPEND lv_fldcat2 TO it_fcat2.

  lv_index2 = lv_index2 + 1.
  CLEAR lv_fldcat2.
  lv_fldcat2-tabname   = 'GT_FINAL2'.
  lv_fldcat2-fieldname = 'CRDATE'.
  lv_fldcat2-scrtext_m = 'Create Date'.
  lv_fldcat2-outputlen = '11'.
*  lv_fldcat2-row_pos   = '1'.
  lv_fldcat2-col_pos   = lv_index2.
  APPEND lv_fldcat2 TO it_fcat2.

  lv_index2 = lv_index2 + 1.
  CLEAR lv_fldcat2.
  lv_fldcat2-tabname   = 'GT_FINAL2'.
  lv_fldcat2-fieldname = 'CHUSR'.
  lv_fldcat2-scrtext_m = 'Change User'.
  lv_fldcat2-outputlen = '12'.
*  lv_fldcat2-row_pos   = '1'.
  lv_fldcat2-col_pos   = lv_index2.
  APPEND lv_fldcat2 TO it_fcat2.

  lv_index2 = lv_index2 + 1.
  CLEAR lv_fldcat2.
  lv_fldcat2-tabname   = 'GT_FINAL2'.
  lv_fldcat2-fieldname = 'CHTIME'.
  lv_fldcat2-scrtext_m = 'Change Time'.
  lv_fldcat2-outputlen = '11'.
*  lv_fldcat2-row_pos   = '1'.
  lv_fldcat2-col_pos   = lv_index2.
  APPEND lv_fldcat2 TO it_fcat2.

  lv_index2 = lv_index2 + 1.
  CLEAR lv_fldcat2.
  lv_fldcat2-tabname   = 'GT_FINAL2'.
  lv_fldcat2-fieldname = 'CHDATE'.
  lv_fldcat2-scrtext_m = 'Change Date'.
  lv_fldcat2-outputlen = '11'.
*  lv_fldcat2-row_pos   = '1'.
  lv_fldcat2-col_pos   = lv_index2.
  APPEND lv_fldcat2 TO it_fcat2.

  lv_index2 = lv_index2 + 1.
  CLEAR lv_fldcat2.
  lv_fldcat2-tabname   = 'GT_FINAL2'.
  lv_fldcat2-fieldname = 'ZBUKR'.
  lv_fldcat2-scrtext_m = 'Paying company code'.
  lv_fldcat2-outputlen = '19'.
*  lv_fldcat2-row_pos   = '1'.
  lv_fldcat2-col_pos   = lv_index2.
  APPEND lv_fldcat2 TO it_fcat2.

* TOT_BTCH_AMT not available in REGUT - column removed

ENDFORM.                    " F_ALV_BUILD_FIELDCAT2
*&---------------------------------------------------------------------*
*&      Form  F_ALV_REPORT_LAYOUT2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM f_alv_report_layout2 .
  CLEAR: it_layout2.
  it_layout2-cwidth_opt = 'X'.
  it_layout2-sel_mode   = 'A'.

ENDFORM.                    " F_ALV_REPORT_LAYOUT2
*&---------------------------------------------------------------------*
*&      Form  FREE_OBJECTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM free_objects1 .

  CALL METHOD c_alvgd1->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CALL METHOD c_ccont1->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " FREE_OBJECTS
*&---------------------------------------------------------------------*
*&      Form  FREE_OBJECTS2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM free_objects2 .
  CALL METHOD c_alvgd2->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CALL METHOD c_ccont2->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " FREE_OBJECTS2
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_OP_TAB3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM f_prepare_op_tab3 .
  DATA: lv_key TYPE c LENGTH 45.
  REFRESH : gt_paym3, gt_batch_header3,gt_final3, gt_batch_sign3.
*break sab_vaibhav.
  SELECT * FROM zfi_paym_file INTO TABLE gt_paym3 WHERE sent = 'X'.

*DELETE gt_paym3 WHERE file_data_sent IS INITIAL.

  IF gt_paym3 IS NOT INITIAL.
    SELECT * FROM regut INTO TABLE gt_batch_header3
      FOR ALL ENTRIES IN gt_paym3
      WHERE laufi = gt_paym3-laufi
      AND   laufd = gt_paym3-laufd.

    IF gt_batch_header3 IS NOT INITIAL.
      SELECT * FROM  zfi_batch_sign INTO TABLE gt_batch_sign3
        WHERE signer EQ sy-uname.
    ENDIF.
  ENDIF.

  LOOP AT gt_batch_header3 INTO gs_batch_header3.
    CLEAR lv_key.
    CONCATENATE gs_batch_header3-zbukr gs_batch_header3-banks
                gs_batch_header3-laufd gs_batch_header3-laufi
                gs_batch_header3-xvorl gs_batch_header3-dtkey
                gs_batch_header3-lfdnr
           INTO lv_key RESPECTING BLANKS.

    READ TABLE gt_batch_sign3 INTO gs_batch_sign3 WITH KEY batch_no = lv_key
                                                          signer = sy-uname.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING gs_batch_header3 TO gs_final3.
      gs_final3-guid       = lv_key.
      gs_final3-batch_sum  = gs_batch_header3-rbetr.
      gs_final3-batch_curr = gs_batch_header3-waers.
      gs_final3-crusr      = gs_batch_header3-tsusr.
      gs_final3-crdate     = gs_batch_header3-tsdat.
      gs_final3-crtime     = gs_batch_header3-tstim.
      gs_final3-chusr      = gs_batch_header3-dwusr.
      gs_final3-chdate     = gs_batch_header3-dwdat.
      gs_final3-chtime     = gs_batch_header3-dwtim.
      READ TABLE gt_paym3 INTO gs_paym3 WITH KEY laufd = gs_batch_header3-laufd
                                                 laufi = gs_batch_header3-laufi.
      IF sy-subrc = 0.
        gs_final3-raw_data       = gs_paym3-raw_data.
        gs_final3-file_data_sent = gs_paym3-file_data_sent.
        gs_final3-error_flag     = gs_paym3-sent_error.
      ENDIF.
      APPEND gs_final3 TO gt_final3.
    ENDIF.
*    endif.
    CLEAR : gs_batch_header3,gs_batch_sign3,gs_final3.
  ENDLOOP.

ENDFORM.                    " F_PREPARE_OP_TAB3

*&---------------------------------------------------------------------*
*&      Form  F_ALV_BUILD_FIELDCAT3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM f_alv_build_fieldcat3 .
  DATA lv_fldcat3 TYPE lvc_s_fcat.
  DATA: lv_index3 TYPE sy-index.
  REFRESH : it_fcat3.
  CLEAR lv_fldcat3.
  lv_fldcat3-tabname   = 'GT_FINAL3'.
  lv_fldcat3-fieldname = 'GUID'.
  lv_fldcat3-scrtext_m = 'Batch Key'.
  lv_fldcat3-outputlen = '45'.
*  lv_fldcat3-row_pos   = '1'.
  lv_fldcat3-col_pos   = lv_index3.
  APPEND lv_fldcat3 TO it_fcat3.

* RULE_ID / ITEM_CNT not available in REGUT - column removed

  lv_index3 = lv_index3 + 1.
  CLEAR lv_fldcat3.
  lv_fldcat3-tabname   = 'GT_FINAL3'.
  lv_fldcat3-fieldname = 'LAUFD'.
  lv_fldcat3-scrtext_m = 'Run Date'.
  lv_fldcat3-outputlen = '11'.
*  lv_fldcat3-row_pos   = '1'.
  lv_fldcat3-col_pos   = lv_index3.
  APPEND lv_fldcat3 TO it_fcat3.

  lv_index3 = lv_index3 + 1.
  CLEAR lv_fldcat3.
  lv_fldcat3-tabname   = 'GT_FINAL3'.
  lv_fldcat3-fieldname = 'LAUFI'.
  lv_fldcat3-scrtext_m = 'Run Id'.
  lv_fldcat3-outputlen = '8'.
*  lv_fldcat3-row_pos   = '1'.
  lv_fldcat3-col_pos   = lv_index3.
  APPEND lv_fldcat3 TO it_fcat3.

* LAUFD_F / LAUFI_F (file date/id) not available in REGUT - column removed

  lv_index3 = lv_index3 + 1.
  CLEAR lv_fldcat3.
  lv_fldcat3-tabname   = 'GT_FINAL3'.
  lv_fldcat3-fieldname = 'ERROR_FLAG'.
  lv_fldcat3-scrtext_m = 'Error Flag'.
  lv_fldcat3-outputlen = '10'.
*  lv_fldcat1-row_pos   = '1'.
  lv_fldcat3-col_pos   = lv_index3.
  APPEND lv_fldcat3 TO it_fcat3.

  lv_index3 = lv_index3 + 1.
  CLEAR lv_fldcat3.
  lv_fldcat3-tabname   = 'GT_FINAL3'.
  lv_fldcat3-fieldname = 'BATCH_SUM'.
  lv_fldcat3-scrtext_m = 'Batch Amount'.
  lv_fldcat3-outputlen = '15'.
*  lv_fldcat3-row_pos   = '1'.
  lv_fldcat3-col_pos   = lv_index3.
  APPEND lv_fldcat3 TO it_fcat3.

  lv_index3 = lv_index3 + 1.
  CLEAR lv_fldcat3.
  lv_fldcat3-tabname   = 'GT_FINAL3'.
  lv_fldcat3-fieldname = 'CRUSR'.
  lv_fldcat3-scrtext_m = 'Create User'.
  lv_fldcat3-outputlen = '12'.
*  lv_fldcat3-row_pos   = '1'.
  lv_fldcat3-col_pos   = lv_index3.
  APPEND lv_fldcat3 TO it_fcat3.

  lv_index3 = lv_index3 + 1.
  CLEAR lv_fldcat3.
  lv_fldcat3-tabname   = 'GT_FINAL3'.
  lv_fldcat3-fieldname = 'CRTIME'.
  lv_fldcat3-scrtext_m = 'Create Time'.
  lv_fldcat3-outputlen = '11'.
  lv_fldcat3-row_pos   = '1'.
  lv_fldcat3-col_pos   = lv_index3.
  APPEND lv_fldcat3 TO it_fcat3.

  lv_index3 = lv_index3 + 1.
  CLEAR lv_fldcat3.
  lv_fldcat3-tabname   = 'GT_FINAL3'.
  lv_fldcat3-fieldname = 'CRDATE'.
  lv_fldcat3-scrtext_m = 'Create Date'.
  lv_fldcat3-outputlen = '11'.
*  lv_fldcat3-row_pos   = '1'.
  lv_fldcat3-col_pos   = lv_index3.
  APPEND lv_fldcat3 TO it_fcat3.

  lv_index3 = lv_index3 + 1.
  CLEAR lv_fldcat3.
  lv_fldcat3-tabname   = 'GT_FINAL3'.
  lv_fldcat3-fieldname = 'CHUSR'.
  lv_fldcat3-scrtext_m = 'Change User'.
  lv_fldcat3-outputlen = '12'.
*  lv_fldcat3-row_pos   = '1'.
  lv_fldcat3-col_pos   = lv_index3.
  APPEND lv_fldcat3 TO it_fcat3.

  lv_index3 = lv_index3 + 1.
  CLEAR lv_fldcat3.
  lv_fldcat3-tabname   = 'GT_FINAL3'.
  lv_fldcat3-fieldname = 'CHTIME'.
  lv_fldcat3-scrtext_m = 'Change Time'.
  lv_fldcat3-outputlen = '11'.
*  lv_fldcat3-row_pos   = '1'.
  lv_fldcat3-col_pos   = lv_index3.
  APPEND lv_fldcat3 TO it_fcat3.

  lv_index3 = lv_index3 + 1.
  CLEAR lv_fldcat3.
  lv_fldcat3-tabname   = 'GT_FINAL3'.
  lv_fldcat3-fieldname = 'CHDATE'.
  lv_fldcat3-scrtext_m = 'Change Date'.
  lv_fldcat3-outputlen = '11'.
*  lv_fldcat3-row_pos   = '1'.
  lv_fldcat3-col_pos   = lv_index3.
  APPEND lv_fldcat3 TO it_fcat3.

  lv_index3 = lv_index3 + 1.
  CLEAR lv_fldcat3.
  lv_fldcat3-tabname   = 'GT_FINAL3'.
  lv_fldcat3-fieldname = 'ZBUKR'.
  lv_fldcat3-scrtext_m = 'Paying company code'.
  lv_fldcat3-outputlen = '19'.
*  lv_fldcat3-row_pos   = '1'.
  lv_fldcat3-col_pos   = lv_index3.
  APPEND lv_fldcat3 TO it_fcat3.

* TOT_BTCH_AMT not available in REGUT - column removed

ENDFORM.                    " F_ALV_BUILD_FIELDCAT3

*&---------------------------------------------------------------------*
*&      Form  F_ALV_REPORT_LAYOUT3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM f_alv_report_layout3 .
  CLEAR: it_layout3.
  it_layout3-cwidth_opt = 'X'.
  it_layout3-sel_mode   = 'A'.
ENDFORM.                    " F_ALV_REPORT_LAYOUT3
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_SENT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM download_sent_data .
  DATA: lv_lines TYPE i.
  DATA: fileleng TYPE i,
        flen     TYPE i,
        lv_filename TYPE string,
        enh_version_management_hex_tb TYPE enh_version_management_hex_tb.

  CALL METHOD c_alvgd3->get_selected_rows
    IMPORTING
      et_index_rows = gt_selected_rows3.

  CLEAR : lv_lines.
  DESCRIBE TABLE gt_selected_rows3 LINES lv_lines.
  IF lv_lines EQ 0.
    MESSAGE 'Please select one record' TYPE 'E'.
  ELSEIF lv_lines > 1.
    MESSAGE 'Please select only single record' TYPE 'E'.
  ENDIF.

  IF gt_selected_rows3 IS NOT INITIAL.
    READ TABLE gt_selected_rows3 INTO gs_selected_rows3 INDEX 1.
    IF sy-subrc = 0.
      READ TABLE gt_final3 INTO gs_final3 INDEX gs_selected_rows3-index.
      IF sy-subrc = 0.
        CLEAR gs_paym3.
        READ TABLE gt_paym3 INTO gs_paym3 WITH KEY laufi = gs_final3-laufi
                                                   laufd = gs_final3-laufd.
        IF sy-subrc EQ 0.
          CLEAR lv_filename.
          CONCATENATE 'C:\BCM\' gs_paym3-file_name '.TXT' INTO lv_filename.

          CALL FUNCTION 'ENH_XSTRING_TO_TAB'
            EXPORTING
              im_xstring = gs_final3-file_data_sent
            IMPORTING
              ex_data    = enh_version_management_hex_tb
              ex_leng    = flen.

          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
              bin_filesize            = flen          "ostr_updated_signed_data_l
              filename                = lv_filename    "'C:\TEST\TESTV1.TXT'
              filetype                = 'BIN'
*             APPEND                  = ' '
*             WRITE_FIELD_SEPARATOR   = ' '
*             HEADER                  = '00'
*             TRUNC_TRAILING_BLANKS   = ' '
            IMPORTING
              filelength              = fileleng
            TABLES
              data_tab                = enh_version_management_hex_tb "OUT_DATA_TABLE
            EXCEPTIONS
              file_write_error        = 1
              no_batch                = 2
              gui_refuse_filetransfer = 3
              invalid_type            = 4
              no_authority            = 5
              unknown_error           = 6
              header_not_allowed      = 7
              separator_not_allowed   = 8
              filesize_not_allowed    = 9
              header_too_long         = 10
              dp_error_create         = 11
              dp_error_send           = 12
              dp_error_write          = 13
              unknown_dp_error        = 14
              access_denied           = 15
              dp_out_of_memory        = 16
              disk_full               = 17
              dp_timeout              = 18
              file_not_found          = 19
              dataprovider_exception  = 20
              control_flush_error     = 21
              OTHERS                  = 22.
          IF sy-subrc NE 0.
*            MESSAGE E206(1S) WITH FILE.
          ENDIF.
          CLEAR :fileleng,flen.
          REFRESH : enh_version_management_hex_tb.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " DOWNLOAD_SENT_DATA

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_RAW_DATA
*&---------------------------------------------------------------------*

FORM download_raw_data .
  DATA : lv_lines TYPE i.
  DATA: fileleng TYPE i,
        flen     TYPE i,
        lv_filename TYPE string,
        enh_version_management_hex_tb TYPE enh_version_management_hex_tb.

  CALL METHOD c_alvgd3->get_selected_rows
    IMPORTING
      et_index_rows = gt_selected_rows3.

  CLEAR : lv_lines.
  DESCRIBE TABLE gt_selected_rows3 LINES lv_lines.
  IF lv_lines EQ 0.
    MESSAGE 'Please select one record' TYPE 'E'.
  ELSEIF lv_lines > 1.
    MESSAGE 'Please select only single record' TYPE 'E'.
  ENDIF.

  IF gt_selected_rows3 IS NOT INITIAL.
    READ TABLE gt_selected_rows3 INTO gs_selected_rows3 INDEX 1.
    IF sy-subrc = 0.
      READ TABLE gt_final3 INTO gs_final3 INDEX gs_selected_rows3-index.
      IF sy-subrc = 0.
        CLEAR gs_paym3.
        READ TABLE gt_paym3 INTO gs_paym3 WITH KEY laufi = gs_final3-laufi
                                                   laufd = gs_final3-laufd.
        IF sy-subrc EQ 0.
          CLEAR lv_filename.
          CONCATENATE 'C:\BCM\' gs_paym3-file_name '.TXT' INTO lv_filename.

          CALL FUNCTION 'ENH_XSTRING_TO_TAB'
            EXPORTING
              im_xstring = gs_final3-raw_data
            IMPORTING
              ex_data    = enh_version_management_hex_tb
              ex_leng    = flen.

          CALL FUNCTION 'GUI_DOWNLOAD'
            EXPORTING
              bin_filesize            = flen          "ostr_updated_signed_data_l
              filename                = lv_filename    "'C:\TEST\TESTV1.TXT'
              filetype                = 'BIN'
*             APPEND                  = ' '
*             WRITE_FIELD_SEPARATOR   = ' '
*             HEADER                  = '00'
*             TRUNC_TRAILING_BLANKS   = ' '
            IMPORTING
              filelength              = fileleng
            TABLES
              data_tab                = enh_version_management_hex_tb "OUT_DATA_TABLE
            EXCEPTIONS
              file_write_error        = 1
              no_batch                = 2
              gui_refuse_filetransfer = 3
              invalid_type            = 4
              no_authority            = 5
              unknown_error           = 6
              header_not_allowed      = 7
              separator_not_allowed   = 8
              filesize_not_allowed    = 9
              header_too_long         = 10
              dp_error_create         = 11
              dp_error_send           = 12
              dp_error_write          = 13
              unknown_dp_error        = 14
              access_denied           = 15
              dp_out_of_memory        = 16
              disk_full               = 17
              dp_timeout              = 18
              file_not_found          = 19
              dataprovider_exception  = 20
              control_flush_error     = 21
              OTHERS                  = 22.
          IF sy-subrc NE 0.
*            MESSAGE E206(1S) WITH FILE.
          ENDIF.
          CLEAR : fileleng,flen.
          REFRESH : enh_version_management_hex_tb.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " DOWNLOAD_RAW_DATA
*&---------------------------------------------------------------------*
*&      Form  FREE_OBJECTS3
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM free_objects3 .
  CALL METHOD c_alvgd3->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CALL METHOD c_ccont3->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " FREE_OBJECTS3
