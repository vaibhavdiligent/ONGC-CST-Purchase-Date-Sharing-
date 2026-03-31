*&---------------------------------------------------------------------*
*& Include          YRG027_PENDING_OVR_POST_VAL
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include          YRG025_PENDING_IMB_POST_VAL
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&       Form  VALIDATION
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validation .

* IF s_date-low+6(2) = '01' OR s_date-low+6(2) = '16' .
* ELSE.
*   MESSAGE 'Start date need to be monthly quater date' TYPE 'S'.
*   EXIT.
* ENDIF.
*
* CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
*   EXPORTING
*     iv_date             = s_date-low
*   IMPORTING
**     EV_MONTH_BEGIN_DATE = EV_MONTH_BEGIN_DATE
*     ev_month_end_date   = l_date.
* .
* IF s_date-high+6(2) = '15' OR s_date-high+6(2) = l_date+6(2) .
* ELSE.
*   MESSAGE 'Enter Only Fortnight date' TYPE 'S'.
*   EXIT.
* ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&       Form  GET_DATA
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  """"""""""""""""""""""""
  CONSTANTS: c_num_char(11) TYPE c VALUE '1234567890 '.

  CLEAR: w_uname,dnpi_flag.
** -> Begin of changes by of Aditi on 29.11.2024 17:03:57 for ATC
*  SELECT SINGLE uname FROM agr_users INTO w_uname WHERE from_dat LE sy-datum AND to_dat GE sy-datum AND
*         uname = sy-uname AND ( agr_name = 'ZO_GMS_CORETEAM' OR agr_name = 'ZO_CC_EHS.GMS_ROLE' ) .
  SELECT uname FROM agr_users INTO w_uname UP TO 1 ROWS WHERE from_dat LE sy-datum AND to_dat GE sy-datum AND
         uname = sy-uname AND ( agr_name = 'ZO_GMS_CORETEAM' OR agr_name = 'ZO_CC_EHS.GMS_ROLE' ) ORDER BY PRIMARY KEY.
  ENDSELECT.
** -> End of changes by of Aditi on 29.11.2024 17:04:07 for ATC
  IF ( NOT sy-uname CO c_num_char ) OR w_uname IS NOT INITIAL.
    dnpi_flag = 'X'.
  ENDIF.
  """"""""""""""""""""""""

*  SELECT nomtk,
*         nomit,
*         idate,
*         docnr,
*         locid,
*         partnr,
*         yyoij_ovr_qty,
*         yyoij_aovr_qty,
*         yyoij_unaovr_qty FROM oijnomi INTO TABLE @DATA(it_oij)
*                          WHERE idate  IN @s_date
*                          AND   locid  IN @locid
*                          AND   partnr IN @partnr
*                          AND   sityp  = 'ZD'
*                          AND   docind NE 'X'
*                          AND   delind NE 'X'.

* > "*SOC CHARM ID :4000009078 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:01.01.2025
  SELECT a~nomtk,
         a~nomit,
         a~idate,
         a~docnr,
         a~locid,
         a~partnr,
         b~pblnr AS locid1,
         c~region
         FROM oijnomi AS a INNER JOIN oifspbl AS b ON a~locid = b~pblnr
                           INNER JOIN adrc AS c ON b~addrnum = c~addrnumber
         INTO TABLE @DATA(it_oij)
         WHERE a~idate  IN @s_date
         AND   a~locid  IN @locid
         AND   a~partnr IN @partnr
         AND   c~region IN @s_region
         AND   a~sityp  = 'ZD'
         AND   a~docind NE 'X'
         AND   a~delind NE 'X'.

* > "*EOC CHARM ID :4000009078 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:01.01.2025

  IF it_oij IS NOT INITIAL.


    SELECT a~pblnr AS locid b~street b~region b~name1 FROM oifspbl AS a INNER JOIN adrc AS b ON a~addrnum = b~addrnumber
           INTO TABLE it_adrc1 FOR ALL ENTRIES IN it_oij WHERE a~pblnr = it_oij-locid.

    SELECT * FROM yrvt_contract INTO TABLE @DATA(it_yrvt1)
                 FOR ALL ENTRIES IN @it_oij
                 WHERE vbeln      = @it_oij-docnr
                 AND   value_from LE @s_date-high
                 AND   value_to   GE @s_date-low
                 AND   clause     EQ '12'.

    IF it_yrvt1 IS NOT INITIAL.
      SELECT vbeln , kunnr , vbeln_grp FROM vbak INTO TABLE @DATA(it_vbak)
             FOR ALL ENTRIES IN @it_yrvt1
             WHERE vbeln = @it_yrvt1-vbeln.
    ENDIF.

    SELECT vbeln, kunnr, adrnr FROM vbpa INTO TABLE @DATA(it_vbpa)
           FOR ALL ENTRIES IN @it_oij
           WHERE vbeln = @it_oij-docnr
           AND   parvw = 'RG'.

    IF it_vbpa IS NOT INITIAL.
      SELECT addrnumber, name1, region FROM adrc INTO TABLE @DATA(it_adrc)
             FOR ALL ENTRIES IN @it_vbpa WHERE addrnumber = @it_vbpa-adrnr.
    ENDIF.

    SELECT a~pblnr AS locid,
           b~street,
           b~region,
           b~name1
           FROM oifspbl AS a
           INNER JOIN adrc AS b ON a~addrnum = b~addrnumber
           INTO TABLE @DATA(it_adrc2)
           FOR ALL ENTRIES IN @it_oij
           WHERE a~pblnr = @it_oij-locid.

  ENDIF.


  IF it_yrvt1 IS NOT INITIAL.

    SELECT * FROM yrg_chg_ovrrun INTO TABLE @DATA(it_ycum)
             FOR ALL ENTRIES IN @it_yrvt1
             WHERE vbeln = @it_yrvt1-vbeln
             AND   begda = @s_date-low
             AND   endda = @s_date-high.
  ENDIF.


  IF it_ycum IS NOT INITIAL.

    SELECT * FROM yrva_zcontrcls INTO TABLE @DATA(it_zcon)
             FOR ALL ENTRIES IN @it_ycum
             WHERE yyvbeln    = @it_ycum-vbeln1
             AND   yyclause_id = '12' .

    SELECT vbelv,
           vbeln,
           vbtyp_n,
           erdat,
           erzet
           FROM vbfa
           INTO TABLE @DATA(lt_vbfa)
           FOR ALL ENTRIES IN @it_ycum
           WHERE vbelv   = @it_ycum-vbeln1
           AND   vbtyp_n IN ( 'M' , 'P' ).
  ENDIF.
  SORT lt_vbfa BY vbelv DESCENDING erdat DESCENDING erzet DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_vbfa COMPARING vbelv.

  SELECT * FROM yrga_oic_region INTO TABLE @DATA(it_yrga_oic_region) .
  IF it_yrga_oic_region[] IS NOT INITIAL.
    SELECT pernr, usrid_long AS usrid FROM pa0105 INTO TABLE @DATA(it_pa0105)
           FOR ALL ENTRIES IN @it_yrga_oic_region
           WHERE pernr = @it_yrga_oic_region-oic_pernr AND subty = '0010' AND endda = '99991231' .
  ENDIF.
  IF it_oij IS NOT INITIAL.
    SELECT locid, tsyst
           FROM oijtsloc
           INTO TABLE @DATA(lt_oijt)
           FOR ALL ENTRIES IN @it_oij
           WHERE locid  = @it_oij-locid
           AND   tsyst  LIKE 'NGPL%'
           AND   delind NE 'X'.
    SELECT vbeln, vkbur
           FROM vbak
           INTO TABLE @DATA(lt_vbak)
           FOR ALL ENTRIES IN @it_oij
           WHERE vbeln = @it_oij-docnr.
  ENDIF.

  DATA(it_yrvt) = it_yrvt1[].
  SORT it_yrvt BY vbeln.
  DELETE ADJACENT DUPLICATES FROM it_yrvt COMPARING vbeln.

  LOOP AT it_yrvt INTO DATA(vt).
    READ TABLE it_oij INTO DATA(oi) WITH KEY docnr = vt-vbeln.
    IF sy-subrc = 0.
      wa_cust-kunnr = oi-partnr.
      APPEND wa_cust TO it_cust.
    ENDIF.
    CLEAR: wa_cust, vt, oi.
  ENDLOOP.

  SORT it_cust BY kunnr.
  DELETE ADJACENT DUPLICATES FROM it_cust COMPARING kunnr.


  cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_true
                                          metadata  = abap_false
                                          data      = abap_true ).

  "BREAK-POINT.
  "to fill final output table
  LOOP AT it_yrvt INTO DATA(yrvt).

    READ TABLE it_oij INTO DATA(oij) WITH KEY docnr = yrvt-vbeln.
    IF sy-subrc = 0.
      wa_final-blocation = oij-locid.
      wa_final-customer  = oij-partnr.
      wa_final-cont_id   = oij-docnr.
      READ TABLE it_vbak INTO DATA(wvbak) WITH KEY vbeln = yrvt-vbeln.
      IF sy-subrc = 0.
        wa_final-m_cont_id  = wvbak-vbeln_grp.
        wa_final-m_mas_cust = wvbak-kunnr.
      ENDIF.
      READ TABLE lt_oijt INTO DATA(ls_oijt) WITH KEY locid = oij-locid.
      IF sy-subrc = 0.
        wa_final-trans_sys = ls_oijt-tsyst.
      ENDIF.
      READ TABLE lt_vbak INTO DATA(ls_vbak) WITH KEY vbeln = oij-docnr.
      IF sy-subrc = 0.
        wa_final-sal_office = ls_vbak-vkbur.
      ENDIF.
      READ TABLE it_vbpa INTO DATA(vbpa) WITH KEY vbeln = oij-docnr.
      IF sy-subrc = 0.
        wa_final-payer = vbpa-kunnr.
        READ TABLE it_adrc INTO DATA(adrc) WITH KEY addrnumber = vbpa-adrnr.
        IF sy-subrc = 0.
          wa_final-payer_name = adrc-name1.
        ENDIF.
      ENDIF.
      READ TABLE it_adrc2 INTO DATA(adrc2) WITH KEY locid = oij-locid.
      IF sy-subrc = 0.
        wa_final-oic_region = adrc2-region.
      ENDIF.

    ENDIF.

    SELECT SINGLE SUM( yyoij_ovr_qty ) AS ovr,
                  SUM( yyoij_aovr_qty ) AS aovr,
                  SUM( yyoij_unaovr_qty ) AS unaovr
                  FROM oijnomi
                  INTO ( @wa_final-cum_bal_mbg_cal , @DATA(lv_aovr), @wa_final-char_bal_mbg_cal )
                  WHERE docnr  = @oij-docnr
                  AND   idate  IN @s_date
                  AND   delind NE 'X'.




**** LOOP AT it_display2 INTO DATA(dis) WHERE vbeln = yrvt-vbeln AND kunnr = oij-partnr.
***** wa_final-cum_bal_mbg_cal = wa_final-cum_bal_mbg_cal_so + dis-totalcumi.

**** IF dis-totalpic IS NOT INITIAL AND dis-totalnic IS INITIAL.
****   wa_final-char_bal_mbg_cal = wa_final-char_bal_mbg_cal + dis-totalpic.
**** ELSEIF dis-totalpic IS INITIAL AND dis-totalnic IS NOT INITIAL.
****   wa_final-char_bal_mbg_cal = wa_final-char_bal_mbg_cal + dis-totalnic.
**** ELSE.
****   wa_final-char_bal_mbg_cal = ''.
**** ENDIF.
****
**** CLEAR:dis.
**** ENDLOOP.

    READ TABLE it_ycum INTO DATA(ycum) WITH KEY vbeln = yrvt-vbeln.
    IF sy-subrc = 0.
      wa_final-cum_bal_mbg_cal_so = ycum-totaloverrun.
*     wa_final-sal_order = ycum-yy_contract.
      wa_final-sal_order = ycum-vbeln1.
    ELSE.
      wa_final-indicator = 'N'.
    ENDIF.

    READ TABLE lt_vbfa INTO DATA(wa_vbfa) WITH KEY vbelv = ycum-vbeln1.
    IF sy-subrc EQ 0 AND ( wa_vbfa-vbtyp_n = 'M' OR wa_vbfa-vbtyp_n = 'P' ).
      wa_final-invoice = wa_vbfa-vbeln.
    ENDIF.

*   SELECT SINGLE vbeln FROM vbfa INTO wa_final-invoice WHERE vbelv = wa_final-sal_order AND vbtyp_n IN ( 'M' , 'P' ).

    READ TABLE it_zcon INTO DATA(wzcon) WITH KEY yyvbeln = ycum-vbeln1.
    IF sy-subrc = 0.
      wa_final-char_bal_mbg_cal_so = wzcon-yyzmeng.
    ENDIF.

    READ TABLE it_adrc1 INTO wa_adrc1 WITH KEY locid = wa_final-blocation.
    IF sy-subrc = 0.
      READ TABLE it_yrga_oic_region INTO DATA(oicr) WITH KEY region = wa_adrc1-region.
      IF sy-subrc = 0.
        READ TABLE it_pa0105 INTO DATA(wa_pa0105) WITH KEY pernr = oicr-oic_pernr.
        IF sy-subrc = 0.
          wa_final-oic_mail = wa_pa0105-usrid.
        ENDIF.
        wa_final-rgmc_mail = oicr-rgmc_email.
      ENDIF.
    ENDIF.

    APPEND wa_final TO it_final.
    CLEAR: wa_final, ycum, adrc,adrc2, oij, yrvt,vbpa, wa_adrc1, oicr, wa_pa0105.

  ENDLOOP.
* > "*SOC CHARM ID :4000009078 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:02.01.2025
  IF s_payer IS NOT INITIAL and s_vkbur IS NOT INITIAL .
    SORT it_final BY blocation DESCENDING customer DESCENDING payer DESCENDING.
    DELETE it_final WHERE payer not in s_payer or sal_office not in s_vkbur .
  ENDIF.
  IF s_payer IS NOT INITIAL .
    SORT it_final BY blocation DESCENDING customer DESCENDING payer DESCENDING.
    DELETE it_final WHERE payer not in s_payer .
  ENDIF.
  IF s_vkbur IS NOT INITIAL.
    SORT it_final BY blocation DESCENDING customer DESCENDING payer DESCENDING.
    DELETE it_final[] WHERE sal_office not in s_vkbur.
  ENDIF.
* > "*EOC CHARM ID :4000009078 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:02.01.2025


ENDFORM.
*&---------------------------------------------------------------------*
*&       Form  GET_FIELDCAT
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fieldcat .
  CLEAR:wa_fcat.
  REFRESH:it_fcat[].
  DATA: sno TYPE char2.
  CLEAR:sno.

* IF it_final IS NOT INITIAL.
  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'BLOCATION'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Business Location'.
  wa_fcat-outputlen = 15.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'CUSTOMER'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Customer'.
  wa_fcat-outputlen = 15.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'PAYER'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Payer'.
  wa_fcat-outputlen = 15.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'PAYER_NAME'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Payer Name'.
  wa_fcat-outputlen = 15.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'CONT_ID'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Contract ID'.
  wa_fcat-outputlen = 15.
  APPEND wa_fcat TO it_fcat.

*  CLEAR wa_fcat.
*  wa_fcat-col_pos   = sno + 1.
*  wa_fcat-fieldname = 'M_CONT_ID'.
*  wa_fcat-tabname   = 'IT_FINAL'.
*  wa_fcat-seltext_l = 'Master Contract'.
*  wa_fcat-outputlen = 15.
*  APPEND wa_fcat TO it_fcat.
*
*  CLEAR wa_fcat.
*  wa_fcat-col_pos   = sno + 1.
*  wa_fcat-fieldname = 'M_MAS_CUST'.
*  wa_fcat-tabname   = 'IT_FINAL'.
*  wa_fcat-seltext_l = 'Master Customer'.
*  wa_fcat-outputlen = 15.
*  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'OIC_REGION'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'OIC Region'.
  wa_fcat-outputlen = 15.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'TRANS_SYS'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Transport System'.
  wa_fcat-outputlen = 15.
  APPEND wa_fcat TO it_fcat.


  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'SAL_OFFICE'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Sales Office'.
  wa_fcat-outputlen = 15.
  APPEND wa_fcat TO it_fcat.


  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'CUM_BAL_MBG_CAL'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Cumulative Overrun in MBG (calculated)'.
  wa_fcat-outputlen = 25.
  APPEND wa_fcat TO it_fcat.


  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'CHAR_BAL_MBG_CAL'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Chargeable Overrun in MBG (calculated)'.
  wa_fcat-outputlen = 25.
  APPEND wa_fcat TO it_fcat.


  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'SAL_ORDER'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Sales Order'.
  wa_fcat-outputlen = 15.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'INVOICE'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Invoice'.
  wa_fcat-outputlen = 15.
  APPEND wa_fcat TO it_fcat.



  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'CUM_BAL_MBG_CAL_SO'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Cumulative Ovr in MBG (posted in SO)'.
  wa_fcat-outputlen = 25.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'CHAR_BAL_MBG_CAL_SO'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Chargeable Ovr in MBG (posted in SO)'.
  wa_fcat-outputlen = 25.
  APPEND wa_fcat TO it_fcat.
  IF dnpi_flag = 'X'.
    CLEAR wa_fcat.
    wa_fcat-col_pos   = sno + 1.
    wa_fcat-fieldname = 'RGMC_MAIL'.
    wa_fcat-tabname   = 'IT_FINAL'.
    wa_fcat-seltext_l = 'RGMC Email'.
    wa_fcat-outputlen = 15.
    APPEND wa_fcat TO it_fcat.

    CLEAR wa_fcat.
    wa_fcat-col_pos   = sno + 1.
    wa_fcat-fieldname = 'OIC_MAIL'.
    wa_fcat-tabname   = 'IT_FINAL'.
    wa_fcat-seltext_l = 'OIC Mail'.
    wa_fcat-outputlen = 15.
    APPEND wa_fcat TO it_fcat.
  ENDIF.
  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'INDICATOR'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Indicator'.
  wa_fcat-outputlen = 15.
  APPEND wa_fcat TO it_fcat.

* ENDIF.

ENDFORM.


*FORM top_of_page .
*
*
*  wa_listheader-typ = 'H'.
*  APPEND wa_listheader TO i_listheader[].
*  wa_listheader-typ = 'S'.
*  wa_listheader-key = 'Report for Date :-'.
**  WRITE sy-datum TO begda DD/MM/YYYY.
**  WRITE sy-datum TO endda DD/MM/YYYY.
*  CONCATENATE s_date-low '-' s_date-high INTO wa_listheader-info
*              SEPARATED BY space.
*  APPEND wa_listheader TO i_listheader[].
*
*  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
*    EXPORTING
*      it_list_commentary = i_listheader[].
*
*ENDFORM.


FORM e03_eventtab_build USING e03_lt_events TYPE slis_t_event.

  DATA: ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = e03_lt_events.
  READ TABLE e03_lt_events WITH KEY name = slis_ev_top_of_page
                            INTO ls_event.
  IF sy-subrc = 0.
    MOVE gc_formname_top_of_page TO ls_event-form.
    APPEND ls_event TO e03_lt_events.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&       Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
* ALV Header
*----------------------------------------------------------------------*
FORM top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_list_top_of_page
*     I_LOGO             =
*     I_END_OF_LIST_GRID =
*     I_ALV_FORM         =
    .
ENDFORM.
FORM comment_build USING lt_top_of_page TYPE slis_t_listheader.

  DATA: ls_line  TYPE slis_listheader,
        l_date_l TYPE char10,
        l_date_2 TYPE char10.

  CLEAR: l_date_l, l_date_2, ls_line.

  ls_line-typ = 'H'.     "'H' TLGASDAY
  CONCATENATE s_date-low+6(2) '.' s_date-low+4(2) '.' s_date-low+0(4) INTO l_date_l.
  CONCATENATE s_date-high+6(2) '.' s_date-high+4(2) '.' s_date-high+0(4) INTO l_date_2.
  CONCATENATE 'Report for Date Range:' l_date_l 'to' l_date_2 INTO ls_line-info SEPARATED BY space ..
  APPEND ls_line TO lt_top_of_page.
  CLEAR:ls_line.
  ls_line-typ  = 'S'.
  ls_line-info = 'Note: N means no posting done through Summary for Overrun'.
  APPEND ls_line TO lt_top_of_page.
  CLEAR:ls_line.


ENDFORM.

*&---------------------------------------------------------------------*
*&       Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_display .

  w_layout-colwidth_optimize = 'X'.

  DATA: v_repid TYPE sy-repid.
  v_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = v_repid
*     I_CALLBACK_PF_STATUS_SET          = ' '
*     I_CALLBACK_USER_COMMAND           = ' '
*     i_callback_top_of_page            = 'TOP_OF_PAGE'
      is_layout              = w_layout
      it_fieldcat            = it_fcat
*     I_DEFAULT              = 'X'
      i_save                 = 'A'
      it_events              = gt_events[]
    TABLES
      t_outtab               = it_final[]
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
  .
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.

ENDFORM.
