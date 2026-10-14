*&---------------------------------------------------------------------*
*& Report  YCIS_SHORTFALL_MC
*&---------------------------------------------------------------------*
*& CIS 2026-27 - Maker/Checker maintenance for the Shortfall grade table
*& (YCIS_SHORTFALL).  Per CPC testing feedback pt.6:
*&   Maker  = PMG Sales Coordination Group  -> enters / edits / deletes the
*&            shortfall grade declarations (rows are saved as Pending 'P').
*&   Checker = CPC L3 (a Level-3 approver in YCIS_WF_APPR) -> Approves 'A'
*&            or Rejects 'R' the Pending rows.
*& Only APPROVED ('A') rows are used by the CIS calculation
*& (YRVG004_QAIS_EXECUTE_N1 reads YCIS_SHORTFALL WHERE appr_status = 'A').
*&
*& Role model:
*&   A user who is a Level-3 approver in YCIS_WF_APPR is the CHECKER.
*&   Any other user is a MAKER.
*&
*& DDIC prerequisite (maintain before import): add these fields to table
*& YCIS_SHORTFALL -
*&   APPR_STATUS CHAR 1   (P=Pending / A=Approved / R=Rejected)
*&   MAKER       CHAR 12  (data element XUBNAME)
*&   MAKER_DATE  DATS     (SYDATUM)   MAKER_TIME UVZEIT (SYUZEIT)
*&   CHECKER     CHAR 12  (XUBNAME)
*&   CHK_DATE    DATS     (SYDATUM)   CHK_TIME   UVZEIT (SYUZEIT)
*&   MAKER_REM   CHAR 100            CHK_REM    CHAR 100
*&
*& GUI status 'STANDARD' (create in SE41, copy from YCIS_APPROVE) with
*& function codes:  ADD  SUBMIT  APPR  REJ  DEL  SELALL  DESEL  BACK  EXIT.
*&---------------------------------------------------------------------*
REPORT  ycis_shortfall_mc.

TYPE-POOLS: slis.

TABLES: ycis_shortfall, ycis_wf_appr.

TYPES: BEGIN OF ty_out,
         sel         TYPE flag,
         matnr       TYPE ycis_shortfall-matnr,
         period_from TYPE ycis_shortfall-period_from,
         period_to   TYPE ycis_shortfall-period_to,
         appr_status TYPE ycis_shortfall-appr_status,
         status_txt  TYPE char12,
         maker       TYPE ycis_shortfall-maker,
         maker_date  TYPE ycis_shortfall-maker_date,
         checker     TYPE ycis_shortfall-checker,
         chk_date    TYPE ycis_shortfall-chk_date,
         maker_rem   TYPE ycis_shortfall-maker_rem,
         chk_rem     TYPE ycis_shortfall-chk_rem,
       END OF ty_out.

DATA: gt_sf     TYPE STANDARD TABLE OF ycis_shortfall,
      gs_sf     TYPE ycis_shortfall,
      gt_out    TYPE STANDARD TABLE OF ty_out,
      gs_out    TYPE ty_out,
      gt_fcat   TYPE slis_t_fieldcat_alv,
      gs_fcat   TYPE slis_fieldcat_alv,
      gs_layout TYPE slis_layout_alv,
      gv_checker TYPE flag.        " 'X' = current user is a CPC L3 checker

*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_prd  FOR ycis_shortfall-period_from,
                s_mat  FOR ycis_shortfall-matnr.
SELECTION-SCREEN END OF BLOCK b1.

*--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM check_role.
  PERFORM get_data.
  PERFORM build_out.
  PERFORM build_fieldcat.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
*&      Form  check_role   (checker = a Level-3 approver in YCIS_WF_APPR)
*&---------------------------------------------------------------------*
FORM check_role.
  DATA lv_cnt TYPE i.
  SELECT COUNT(*) INTO lv_cnt FROM ycis_wf_appr
    WHERE wf_level = '3' AND userid = sy-uname.
  IF lv_cnt > 0.
    gv_checker = 'X'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
FORM get_data.
  REFRESH gt_sf.
  SELECT * FROM ycis_shortfall INTO TABLE gt_sf
    WHERE period_from IN s_prd
      AND matnr       IN s_mat.
ENDFORM.

*&---------------------------------------------------------------------*
FORM build_out.
  REFRESH gt_out.
  LOOP AT gt_sf INTO gs_sf.
    CLEAR gs_out.
    MOVE-CORRESPONDING gs_sf TO gs_out.
    PERFORM status_text USING gs_sf-appr_status CHANGING gs_out-status_txt.
    APPEND gs_out TO gt_out.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
FORM status_text USING p_st TYPE any CHANGING p_txt TYPE char12.
  CASE p_st.
    WHEN 'A'. p_txt = 'Approved'.
    WHEN 'R'. p_txt = 'Rejected'.
    WHEN 'P'. p_txt = 'Pending'.
    WHEN OTHERS. p_txt = 'Pending'.   " blank legacy rows are treated as Pending
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
FORM build_fieldcat.
  DATA lv_pos TYPE i.
  DEFINE add_fc.
    CLEAR gs_fcat.
    ADD 1 TO lv_pos.
    gs_fcat-col_pos   = lv_pos.
    gs_fcat-fieldname = &1.
    gs_fcat-seltext_l = &2.
    gs_fcat-seltext_m = &2.
    gs_fcat-seltext_s = &2.
    gs_fcat-checkbox  = &3.
    gs_fcat-edit      = &3.
    APPEND gs_fcat TO gt_fcat.
  END-OF-DEFINITION.

  add_fc 'SEL'         'Select'          'X'.
  add_fc 'MATNR'       'Material/Grade'  ''.
  add_fc 'PERIOD_FROM' 'Period From'     ''.
  add_fc 'PERIOD_TO'   'Period To'       ''.
  add_fc 'STATUS_TXT'  'Status'          ''.
  add_fc 'MAKER'       'Maker'           ''.
  add_fc 'MAKER_DATE'  'Maker Date'      ''.
  add_fc 'MAKER_REM'   'Maker Remark'    ''.
  add_fc 'CHECKER'     'Checker'         ''.
  add_fc 'CHK_DATE'    'Checked On'      ''.
  add_fc 'CHK_REM'     'Checker Remark'  ''.
ENDFORM.

*&---------------------------------------------------------------------*
FORM display_alv.
  gs_layout-zebra         = 'X'.
  gs_layout-box_fieldname = 'SEL'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_top_of_page   = 'TOP_OF_PAGE'
      is_layout                = gs_layout
      it_fieldcat              = gt_fcat
    TABLES
      t_outtab                 = gt_out
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.

*&---------------------------------------------------------------------*
FORM set_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab.
ENDFORM.

*&---------------------------------------------------------------------*
FORM top_of_page.
  DATA: lt_hdr TYPE slis_t_listheader,
        ls_hdr TYPE slis_listheader.
  ls_hdr-typ = 'H'.
  ls_hdr-info = 'CIS 2026-27 - Shortfall grade Maker / Checker'.
  APPEND ls_hdr TO lt_hdr.
  ls_hdr-typ = 'S'.
  IF gv_checker = 'X'.
    ls_hdr-info = 'You are a CHECKER (CPC L3): Approve / Reject Pending rows.'.
  ELSE.
    ls_hdr-info = 'You are a MAKER: Add / Edit / Delete rows (saved as Pending).'.
  ENDIF.
  APPEND ls_hdr TO lt_hdr.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_hdr.
ENDFORM.

*&---------------------------------------------------------------------*
FORM user_command USING r_ucomm     TYPE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  rs_selfield-refresh = 'X'.
  CASE r_ucomm.
    WHEN 'ADD'.
      PERFORM add_row.
    WHEN 'DEL'.
      PERFORM delete_rows.
    WHEN 'APPR'.
      PERFORM check_action USING 'A'.
    WHEN 'REJ'.
      PERFORM check_action USING 'R'.
    WHEN 'SELALL'.
      LOOP AT gt_out INTO gs_out.
        gs_out-sel = 'X'. MODIFY gt_out FROM gs_out.
      ENDLOOP.
    WHEN 'DESEL'.
      LOOP AT gt_out INTO gs_out.
        CLEAR gs_out-sel. MODIFY gt_out FROM gs_out.
      ENDLOOP.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  add_row   (MAKER: enter a new shortfall declaration)
*&---------------------------------------------------------------------*
FORM add_row.
  DATA: lt_fields TYPE STANDARD TABLE OF sval,
        ls_field  TYPE sval,
        lv_ret    TYPE char1.
  IF gv_checker = 'X'.
    MESSAGE 'A checker (CPC L3) cannot enter rows - maker action only' TYPE 'I'.
    RETURN.
  ENDIF.
  ls_field-tabname = 'YCIS_SHORTFALL'. ls_field-fieldname = 'MATNR'.
  ls_field-field_obl = 'X'. APPEND ls_field TO lt_fields.
  CLEAR ls_field.
  ls_field-tabname = 'YCIS_SHORTFALL'. ls_field-fieldname = 'PERIOD_FROM'.
  ls_field-field_obl = 'X'. APPEND ls_field TO lt_fields.
  CLEAR ls_field.
  ls_field-tabname = 'YCIS_SHORTFALL'. ls_field-fieldname = 'PERIOD_TO'.
  ls_field-field_obl = 'X'. APPEND ls_field TO lt_fields.
  CLEAR ls_field.
  ls_field-tabname = 'YCIS_SHORTFALL'. ls_field-fieldname = 'MAKER_REM'.
  ls_field-fieldtext = 'Remark'. APPEND ls_field TO lt_fields.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'Add shortfall grade (saved as Pending)'
    IMPORTING
      returncode      = lv_ret
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  IF sy-subrc <> 0 OR lv_ret = 'A'.
    RETURN.
  ENDIF.

  CLEAR gs_sf.
  gs_sf-mandt = sy-mandt.
  READ TABLE lt_fields INTO ls_field INDEX 1. gs_sf-matnr       = ls_field-value.
  READ TABLE lt_fields INTO ls_field INDEX 2. gs_sf-period_from = ls_field-value.
  READ TABLE lt_fields INTO ls_field INDEX 3. gs_sf-period_to   = ls_field-value.
  READ TABLE lt_fields INTO ls_field INDEX 4. gs_sf-maker_rem   = ls_field-value.
  gs_sf-appr_status = 'P'.
  gs_sf-maker       = sy-uname.
  gs_sf-maker_date  = sy-datum.
  gs_sf-maker_time  = sy-uzeit.
  CLEAR: gs_sf-checker, gs_sf-chk_date, gs_sf-chk_time, gs_sf-chk_rem.

  MODIFY ycis_shortfall FROM gs_sf.
  IF sy-subrc = 0.
    COMMIT WORK.
    MESSAGE 'Shortfall grade added as Pending - awaiting CPC L3 approval' TYPE 'S'.
    PERFORM get_data.
    PERFORM build_out.
  ELSE.
    ROLLBACK WORK.
    MESSAGE 'Could not save the row' TYPE 'I'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  delete_rows   (MAKER: delete own Pending rows)
*&---------------------------------------------------------------------*
FORM delete_rows.
  DATA lv_cnt TYPE i.
  IF gv_checker = 'X'.
    MESSAGE 'A checker (CPC L3) cannot delete rows - maker action only' TYPE 'I'.
    RETURN.
  ENDIF.
  LOOP AT gt_out INTO gs_out WHERE sel = 'X'.
    IF gs_out-appr_status = 'A'.
      MESSAGE 'Approved rows cannot be deleted - reject them first' TYPE 'I'.
      CONTINUE.
    ENDIF.
    DELETE FROM ycis_shortfall
      WHERE matnr       = gs_out-matnr
        AND period_from = gs_out-period_from
        AND period_to   = gs_out-period_to.
    lv_cnt = lv_cnt + 1.
  ENDLOOP.
  IF lv_cnt > 0.
    COMMIT WORK.
    MESSAGE |{ lv_cnt } row(s) deleted| TYPE 'S'.
    PERFORM get_data.
    PERFORM build_out.
  ELSE.
    MESSAGE 'Select at least one Pending/Rejected row to delete' TYPE 'I'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  check_action   (CHECKER: Approve 'A' / Reject 'R')
*&---------------------------------------------------------------------*
FORM check_action USING p_status TYPE c.
  DATA: lv_cnt    TYPE i,
        lv_remark TYPE ycis_shortfall-chk_rem.
  IF gv_checker IS INITIAL.
    MESSAGE 'Only a CPC L3 checker can Approve / Reject' TYPE 'I'.
    RETURN.
  ENDIF.
  READ TABLE gt_out INTO gs_out WITH KEY sel = 'X'.
  IF sy-subrc <> 0.
    MESSAGE 'Please select at least one row' TYPE 'I'.
    RETURN.
  ENDIF.
  PERFORM get_chk_remark CHANGING lv_remark.
  IF lv_remark IS INITIAL.
    MESSAGE 'Checker remark is mandatory' TYPE 'I'.
    RETURN.
  ENDIF.

  LOOP AT gt_out INTO gs_out WHERE sel = 'X'.
    READ TABLE gt_sf INTO gs_sf
         WITH KEY matnr       = gs_out-matnr
                  period_from = gs_out-period_from
                  period_to   = gs_out-period_to.
    CHECK sy-subrc = 0.
    gs_sf-appr_status = p_status.
    gs_sf-checker     = sy-uname.
    gs_sf-chk_date    = sy-datum.
    gs_sf-chk_time    = sy-uzeit.
    gs_sf-chk_rem     = lv_remark.
    MODIFY ycis_shortfall FROM gs_sf.
    lv_cnt = lv_cnt + 1.
  ENDLOOP.
  IF lv_cnt > 0.
    COMMIT WORK.
    IF p_status = 'A'.
      MESSAGE |{ lv_cnt } row(s) approved - now used by the CIS calculation| TYPE 'S'.
    ELSE.
      MESSAGE |{ lv_cnt } row(s) rejected| TYPE 'S'.
    ENDIF.
    PERFORM get_data.
    PERFORM build_out.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
FORM get_chk_remark CHANGING p_remark TYPE ycis_shortfall-chk_rem.
  DATA: lt_fields TYPE STANDARD TABLE OF sval,
        ls_field  TYPE sval,
        lv_ret    TYPE char1.
  ls_field-tabname   = 'YCIS_SHORTFALL'.
  ls_field-fieldname = 'CHK_REM'.
  ls_field-fieldtext = 'Remark'.
  ls_field-field_obl = 'X'.
  APPEND ls_field TO lt_fields.
  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'Checker remark (mandatory)'
    IMPORTING
      returncode      = lv_ret
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.
  IF sy-subrc = 0 AND lv_ret <> 'A'.
    READ TABLE lt_fields INTO ls_field INDEX 1.
    p_remark = ls_field-value.
  ENDIF.
ENDFORM.
