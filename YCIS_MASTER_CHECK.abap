*&---------------------------------------------------------------------*
*& Report  YCIS_MASTER_CHECK
*&---------------------------------------------------------------------*
*& CIS 2026-27 - Master-Data Checker.
*&
*& Enter a Customer + Period and the report runs every master-data check
*& the CIS calculation depends on, and reports - in one ALV screen - what
*& is OK and what is MISSING / to be maintained. Turns every "why is the
*& discount zero?" into a one-screen answer without a debugger.
*&
*& Checks: CIS master & committed qty (YRVA_QAIS_DATA), grade-wise MCQ
*& (YRVA_QAIS_TNTLFT), lifting (S922), grade mapping (YRVA_GRADE_CISD),
*& seasonal indicators (YRVA_PRS_GRADES), Group/MLE relationship (BUT050
*& via CVI_CUST_LINK/BUT000), monthly waiver (YRVA_QAIS_ADD_WV), shortfall
*& (YCIS_SHORTFALL), customer group (KNVV / TVV2).
*&
*& No GUI status needed (plain display ALV).
*&---------------------------------------------------------------------*
REPORT  ycis_master_check.

TYPE-POOLS: slis.

TABLES: kna1.

TYPES: BEGIN OF ty_out,
         sno    TYPE n LENGTH 3,
         check  TYPE c LENGTH 45,
         status TYPE c LENGTH 10,      " OK / MISSING / WARNING / INFO
         detail TYPE c LENGTH 160,
       END OF ty_out.

DATA: gt_out    TYPE STANDARD TABLE OF ty_out,
      gs_out    TYPE ty_out,
      gt_fcat   TYPE slis_t_fieldcat_alv,
      gs_fcat   TYPE slis_fieldcat_alv,
      gs_layout TYPE slis_layout_alv,
      gv_sno    TYPE n LENGTH 3.

*   working data shared across checks
DATA: gs_qais   TYPE yrva_qais_data,
      gv_found  TYPE flag,
      gv_qaisno TYPE yrva_qais_data-qais_no,
      gv_month  TYPE c LENGTH 3,       " APR..MAR of the run month
      gv_kvgr2  TYPE kvgr2,
      gv_bp     TYPE but000-partner.

*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:     p_kunnr TYPE kunnr OBLIGATORY.
SELECT-OPTIONS: s_sptag FOR kna1-erdat OBLIGATORY.    " CIS period (date range)
SELECTION-SCREEN END OF BLOCK b1.

*--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM add USING 'Customer / Period' 'INFO'
    |{ p_kunnr ALPHA = OUT } for { s_sptag-low DATE = USER } to { s_sptag-high DATE = USER }|.
  PERFORM chk_cis_master.
  PERFORM chk_committed_qty.
  PERFORM chk_grade_mcq.
  PERFORM chk_lifting_and_grades.
  PERFORM chk_seasonal.
  PERFORM chk_group_mle.
  PERFORM chk_waiver.
  PERFORM chk_shortfall.
  PERFORM chk_cust_group.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
*&      Form  add   (append a result line)
*&---------------------------------------------------------------------*
FORM add USING p_check TYPE any p_status TYPE any p_detail TYPE any.
  gv_sno = gv_sno + 1.
  CLEAR gs_out.
  gs_out-sno    = gv_sno.
  gs_out-check  = p_check.
  gs_out-status = p_status.
  gs_out-detail = p_detail.
  APPEND gs_out TO gt_out.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  chk_cis_master
*&---------------------------------------------------------------------*
FORM chk_cis_master.
  CLEAR: gs_qais, gv_found, gv_qaisno.
  SELECT * FROM yrva_qais_data INTO gs_qais UP TO 1 ROWS
     WHERE kunnr     = p_kunnr
       AND mou_begda LE s_sptag-low
       AND mou_endda GE s_sptag-low
     ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF sy-subrc = 0.
    gv_found  = 'X'.
    gv_qaisno = gs_qais-qais_no.
    gv_kvgr2  = gs_qais-kvgr2.
    PERFORM add USING 'CIS signed (YRVA_QAIS_DATA)' 'OK'
      |CIS No. { gv_qaisno }, MoU { gs_qais-mou_qty } MT, KVGR2 '{ gs_qais-kvgr2 }'|.
  ELSE.
    PERFORM add USING 'CIS signed (YRVA_QAIS_DATA)' 'MISSING'
      'No CIS record for this customer covering the period - maintain YRVA_QAIS_DATA'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  chk_committed_qty   (committed qty for the run month)
*&---------------------------------------------------------------------*
FORM chk_committed_qty.
  DATA: lv_mm  TYPE i,
        lv_idx TYPE i,
        lv_fld TYPE string,
        lv_qty TYPE p DECIMALS 3.
  FIELD-SYMBOLS <cq> TYPE any.
  CHECK gv_found = 'X'.

  lv_mm = s_sptag-low+4(2).
  IF lv_mm >= 4. lv_idx = lv_mm - 3. ELSE. lv_idx = lv_mm + 9. ENDIF.   " Apr=1 .. Mar=12
  CASE lv_mm.
    WHEN 4. gv_month = 'APR'. WHEN 5. gv_month = 'MAY'. WHEN 6. gv_month = 'JUN'.
    WHEN 7. gv_month = 'JUL'. WHEN 8. gv_month = 'AUG'. WHEN 9. gv_month = 'SEP'.
    WHEN 10. gv_month = 'OCT'. WHEN 11. gv_month = 'NOV'. WHEN 12. gv_month = 'DEC'.
    WHEN 1. gv_month = 'JAN'. WHEN 2. gv_month = 'FEB'. WHEN 3. gv_month = 'MAR'.
  ENDCASE.

  lv_fld = |COMMITED_QTY_M{ lv_idx }|.
  ASSIGN COMPONENT lv_fld OF STRUCTURE gs_qais TO <cq>.
  IF <cq> IS ASSIGNED.
    lv_qty = <cq>.
    IF lv_qty > 0.
      PERFORM add USING |Committed qty for { gv_month } (MCQ)| 'OK'
        |Committed { lv_qty } MT for { gv_month }. Minimum lifting rule applies to this.|.
    ELSE.
      PERFORM add USING |Committed qty for { gv_month } (MCQ)| 'WARNING'
        |Committed qty for { gv_month } is 0 - check YRVA_QAIS_DATA (customer may not have committed this month)|.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  chk_grade_mcq   (grade-wise committed / tentative lifting)
*&---------------------------------------------------------------------*
FORM chk_grade_mcq.
  DATA lv_cnt TYPE i.
  CHECK gv_found = 'X'.
  SELECT COUNT(*) INTO lv_cnt FROM yrva_qais_tntlft
    WHERE qais_no = gv_qaisno.
  IF lv_cnt > 0.
    PERFORM add USING 'Grade-wise MCQ (YRVA_QAIS_TNTLFT)' 'OK'
      |{ lv_cnt } grade-wise committed rows found for this CIS|.
  ELSE.
    PERFORM add USING 'Grade-wise MCQ (YRVA_QAIS_TNTLFT)' 'MISSING'
      'No grade-wise committed qty for this CIS - maintain YRVA_QAIS_TNTLFT'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  chk_lifting_and_grades   (S922 lifting + grade mapping)
*&---------------------------------------------------------------------*
FORM chk_lifting_and_grades.
  TYPES: BEGIN OF ty_l, kondm TYPE kondm, ummenge TYPE s922-ummenge, END OF ty_l.
  DATA: lt_l   TYPE STANDARD TABLE OF ty_l,
        ls_l   TYPE ty_l,
        lv_tot TYPE p DECIMALS 3,
        lv_map TYPE i,
        lv_unmapped TYPE string,
        lv_g   TYPE yy_grade.

  SELECT kondm SUM( ummenge )
    INTO TABLE lt_l
    FROM s922
    WHERE pkunag = p_kunnr
      AND sptag  IN s_sptag
    GROUP BY kondm.

  IF lt_l IS INITIAL.
    PERFORM add USING 'Lifting for this customer (S922)' 'WARNING'
      'No lifting found for this customer in the period - group/MLE lifting is checked separately below'.
    RETURN.
  ENDIF.

  CLEAR lv_tot.
  LOOP AT lt_l INTO ls_l.
    lv_tot = lv_tot + ls_l-ummenge.
  ENDLOOP.
  PERFORM add USING 'Lifting for this customer (S922)' 'OK'
    |Total lifted { lv_tot } MT across { lines( lt_l ) } grade(s) in the period|.

*   grade mapping: every lifted grade must exist in YRVA_GRADE_CISD (else not captured)
  CLEAR: lv_unmapped, lv_map.
  LOOP AT lt_l INTO ls_l.
    SELECT SINGLE yy_grade INTO lv_g FROM yrva_grade_cisd
      WHERE yy_grade = ls_l-kondm.
    IF sy-subrc <> 0.
      lv_map = lv_map + 1.
      IF lv_unmapped IS INITIAL.
        lv_unmapped = ls_l-kondm.
      ELSE.
        CONCATENATE lv_unmapped ls_l-kondm INTO lv_unmapped SEPARATED BY ','.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF lv_map = 0.
    PERFORM add USING 'Grade mapping (YRVA_GRADE_CISD)' 'OK'
      'All lifted grades are mapped and will be captured'.
  ELSE.
    PERFORM add USING 'Grade mapping (YRVA_GRADE_CISD)' 'MISSING'
      |{ lv_map } lifted grade(s) NOT mapped - will NOT be captured. Maintain: { lv_unmapped }|.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  chk_seasonal   (YRVA_PRS_GRADES populated?)
*&---------------------------------------------------------------------*
FORM chk_seasonal.
  DATA lv_cnt TYPE i.
  SELECT COUNT(*) INTO lv_cnt FROM yrva_prs_grades.
  IF lv_cnt > 0.
    PERFORM add USING 'Seasonal indicators (YRVA_PRS_GRADES)' 'OK'
      |{ lv_cnt } grade indicator row(s) maintained|.
  ELSE.
    PERFORM add USING 'Seasonal indicators (YRVA_PRS_GRADES)' 'WARNING'
      'YRVA_PRS_GRADES is EMPTY - seasonal / S-R-P grade filtering will not work'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  chk_group_mle   (BP Group/MLE relationship - BUT050)
*&---------------------------------------------------------------------*
FORM chk_group_mle.
  DATA: lv_guid TYPE but000-partner_guid,
        lt_rel  TYPE STANDARD TABLE OF but050,
        ls_rel  TYPE but050,
        lv_zgp  TYPE i,
        lv_oth  TYPE i,
        lv_txt  TYPE string.

* customer -> BP
  SELECT SINGLE partner_guid INTO lv_guid FROM cvi_cust_link
    WHERE customer = p_kunnr.
  IF sy-subrc = 0.
    SELECT SINGLE partner INTO gv_bp FROM but000 WHERE partner_guid = lv_guid.
  ENDIF.
  IF gv_bp IS INITIAL.
    gv_bp = p_kunnr.               " fallback: BP number = customer number
  ENDIF.

  SELECT * FROM but050 INTO TABLE lt_rel
    WHERE partner1 = gv_bp
      AND date_to   GE s_sptag-low
      AND date_from LE s_sptag-low.
  SELECT * FROM but050 APPENDING TABLE lt_rel
    WHERE partner2 = gv_bp
      AND date_to   GE s_sptag-low
      AND date_from LE s_sptag-low.

  IF lt_rel IS INITIAL.
    PERFORM add USING 'Group / MLE relationship (BUT050)' 'INFO'
      |No BP relationship on BP { gv_bp } for the period - treated as a standalone customer|.
    RETURN.
  ENDIF.

  CLEAR: lv_zgp, lv_oth, lv_txt.
  LOOP AT lt_rel INTO ls_rel.
    IF ls_rel-reltyp CS 'ZGP'.
      lv_zgp = lv_zgp + 1.
    ELSE.
      lv_oth = lv_oth + 1.
      IF lv_txt IS INITIAL.
        lv_txt = ls_rel-reltyp.
      ELSE.
        CONCATENATE lv_txt ls_rel-reltyp INTO lv_txt SEPARATED BY ','.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lv_zgp > 0.
    PERFORM add USING 'Group / MLE relationship (BUT050)' 'OK'
      |{ lv_zgp } Group/MLE (ZGP*) relationship(s) found - members will be clubbed|.
  ELSE.
    PERFORM add USING 'Group / MLE relationship (BUT050)' 'MISSING'
      |Relationship(s) exist but type is '{ lv_txt }', NOT a Group/MLE (ZGP*) type - members will NOT be clubbed. Re-maintain as Group/MLE.|.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  chk_waiver   (monthly waiver actually granting - ADD_WV)
*&---------------------------------------------------------------------*
FORM chk_waiver.
  DATA lv_cnt TYPE i.
  CHECK gv_found = 'X'.
  SELECT COUNT(*) INTO lv_cnt FROM yrva_qais_add_wv
    WHERE qais_no      = gv_qaisno
      AND waiver_month = gv_month.
  IF lv_cnt > 0.
    PERFORM add USING |Monthly waiver for { gv_month } (YRVA_QAIS_ADD_WV)| 'OK'
      'Waiver row found - the monthly minimum-lifting floor is lowered for this month'.
  ELSE.
    PERFORM add USING |Monthly waiver for { gv_month } (YRVA_QAIS_ADD_WV)| 'INFO'
      |No waiver row for this CIS + { gv_month }. If a waiver was granted, maintain YRVA_QAIS_ADD_WV (the WAIVER_1 field alone only shows a label).|.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  chk_shortfall   (YCIS_SHORTFALL for the period)
*&---------------------------------------------------------------------*
FORM chk_shortfall.
  DATA lv_cnt TYPE i.
  SELECT COUNT(*) INTO lv_cnt FROM ycis_shortfall
    WHERE period_from LE s_sptag-low
      AND period_to   GE s_sptag-low.
  IF lv_cnt > 0.
    PERFORM add USING 'Shortfall grades (YCIS_SHORTFALL)' 'OK'
      |{ lv_cnt } shortfall grade(s) declared for the period|.
  ELSE.
    PERFORM add USING 'Shortfall grades (YCIS_SHORTFALL)' 'INFO'
      'No shortfall grade declared for the period (auto shortfall-waiver will not apply)'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  chk_cust_group   (KVGR2 assigned & exists in TVV2)
*&---------------------------------------------------------------------*
FORM chk_cust_group.
  DATA: lv_kvgr2 TYPE kvgr2,
        lv_x     TYPE kvgr2.
  SELECT SINGLE kvgr2 INTO lv_kvgr2 FROM knvv WHERE kunnr = p_kunnr.
  IF lv_kvgr2 IS INITIAL.
    PERFORM add USING 'Customer group (KNVV-KVGR2)' 'INFO'
      'No Customer Group 2 assigned to the customer (only needed for group / MLE handling)'.
    RETURN.
  ENDIF.
  SELECT SINGLE kvgr2 INTO lv_x FROM tvv2 WHERE kvgr2 = lv_kvgr2.
  IF sy-subrc = 0.
    PERFORM add USING 'Customer group (KNVV / TVV2)' 'OK'
      |Customer Group 2 '{ lv_kvgr2 }' assigned and configured in TVV2|.
  ELSE.
    PERFORM add USING 'Customer group (KNVV / TVV2)' 'MISSING'
      |Customer Group '{ lv_kvgr2 }' is assigned but NOT configured in TVV2 (OVS9) - this causes the "does not exist in TVV2" error|.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  display_alv
*&---------------------------------------------------------------------*
FORM display_alv.
  DEFINE add_fc.
    CLEAR gs_fcat.
    gs_fcat-fieldname = &1.
    gs_fcat-seltext_l = &2.
    gs_fcat-seltext_m = &2.
    gs_fcat-seltext_s = &2.
    gs_fcat-outputlen = &3.
    APPEND gs_fcat TO gt_fcat.
  END-OF-DEFINITION.
  add_fc 'SNO'    'S.No'   4.
  add_fc 'CHECK'  'Check'  45.
  add_fc 'STATUS' 'Result' 10.
  add_fc 'DETAIL' 'Finding / What to maintain' 100.

  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra             = 'X'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fcat
      i_grid_title       = 'CIS 2026-27 - Master Data Check'
    TABLES
      t_outtab           = gt_out
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM.
