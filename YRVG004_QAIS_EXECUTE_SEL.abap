*&---------------------------------------------------------------------*
*& Include          YRVG004_QAIS_EXECUTE_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS :
  s_sptag   FOR s922-sptag OBLIGATORY NO-EXTENSION,
  s_sptag1  FOR s922-sptag NO-DISPLAY NO-EXTENSION,
  s_sptagn  FOR s922-sptag NO-DISPLAY NO-EXTENSION,
  lv_sptag  FOR s922-sptag NO-DISPLAY NO-EXTENSION,
  s_vkbur   FOR s922-vkbur OBLIGATORY NO INTERVALS,
  s_pkunag  FOR s922-pkunag NO INTERVALS,
  s_kvgr2   FOR s922-kvgr2  NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN SKIP.

PARAMETERS: r_month   RADIOBUTTON GROUP rd1 USER-COMMAND r1 DEFAULT 'X',
            r_quater  RADIOBUTTON GROUP rd1,
            r_annual  RADIOBUTTON GROUP rd1,
            r_consis  RADIOBUTTON GROUP rd1.
SELECTION-SCREEN SKIP.
PARAMETERS: r_month1 NO-DISPLAY.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:    r_rlld   RADIOBUTTON GROUP rd1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_rhd RADIOBUTTON GROUP rd1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: c_maint RADIOBUTTON GROUP rd1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: c_maint1 RADIOBUTTON GROUP rd1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_newcus NO-DISPLAY.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:  r_rpd  NO-DISPLAY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(79) TEXT-002.
PARAMETERS: c_chk TYPE flag NO-DISPLAY DEFAULT ' '.

SELECTION-SCREEN : END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF r_month = 'X' OR r_quater = 'X' OR r_annual = 'X' OR r_rhd = 'X'
       OR r_rlld = 'X' OR c_maint = 'X' OR c_maint1 = 'X'.
      IF screen-name = 'R_CONSIS'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT SCREEN.
    IF screen-name = 'R_RPD'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'R_NEWCUS'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'R_RHD' OR screen-name = 'R_RLLD' OR screen-name = 'C_MAINT' OR screen-name = 'C_MAINT1'.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

INITIALIZATION.

  SELECT * FROM yrva_prs_grades INTO TABLE @DATA(lt_yrva_prs_grades).
  SELECT SINGLE * FROM yrva_cis_mstr INTO ls_yrva_cis_mstr.
  LOOP AT lt_yrva_prs_grades INTO DATA(ls_yrva_prs_grades).
    range_r-sign   = 'I'.
    range_r-option = 'EQ'.
    range_s-sign   = 'I'.
    range_s-option = 'EQ'.
    range_p-sign   = 'I'.
    range_p-option = 'EQ'.
    IF ls_yrva_prs_grades-yy_indicator = 'R'.
      range_r-low = ls_yrva_prs_grades-yy_grade.
      APPEND range_r.
    ELSEIF ls_yrva_prs_grades-yy_indicator = 'S'.
      range_s-low = ls_yrva_prs_grades-yy_grade.
      APPEND range_s.
    ELSEIF ls_yrva_prs_grades-yy_indicator = 'P'.
      range_p-low = ls_yrva_prs_grades-yy_grade.
      APPEND range_p.
    ENDIF.
    CLEAR: ls_yrva_prs_grades.
  ENDLOOP.
