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

*   Calculate Diff (Chargeable Ovr) = Calculated - Posted
    wa_final-diff_char_ovr = wa_final-char_bal_mbg_cal - wa_final-char_bal_mbg_cal_so.

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

* Filter entries where Diff (Chargeable Ovr) is zero - these are already fully posted
  DELETE it_final WHERE diff_char_ovr EQ 0.

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

* New column: Diff (Chargeable Ovr) = Calculated - Posted
  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'DIFF_CHAR_OVR'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Diff (Chargeable Ovr)'.
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

*&---------------------------------------------------------------------*
*&       Form  SEND_EMAIL
*&---------------------------------------------------------------------*
* Send pending overrun posting emails per customer
* - Recipients: ERNAMs from OIJ_EL_TICKET_I (tickets) or AENAMs from OIJNOMI
* - Email ID resolved via PA0105 (SUBTY=0010, ENDDA=99991231)
* - Source: GR102 (ref: YRGR091 pattern)
* - Sender: gailpartcare@gail.co.in (same as YRGR091)
*----------------------------------------------------------------------*
FORM send_email.

  DATA: lt_cust_email    TYPE STANDARD TABLE OF ty_cust.
  DATA: lv_date_from(10) TYPE c.
  DATA: lv_date_to(10)   TYPE c.
  DATA: lv_date_range    TYPE string.
  DATA: lv_w_date(10)    TYPE c.
  DATA: lv_source        TYPE string.
  DATA: lv_kunnr_disp    TYPE string.
  DATA: lv_subject       TYPE so_obj_des.
  DATA: lv_att_name      TYPE so_obj_des.
  DATA: lt_body          TYPE bcsy_text.
  DATA: lv_body_line     TYPE so_text255.
  DATA: lv_csv_str       TYPE string.
  DATA: lv_xstring       TYPE xstring.
  DATA: lt_att_hex       TYPE solix_tab.
  DATA: lv_sent_to_all   TYPE c.
  DATA: lo_send_request  TYPE REF TO cl_bcs.
  DATA: lo_document      TYPE REF TO cl_document_bcs.
  DATA: lo_sender        TYPE REF TO if_sender_bcs.
  DATA: lo_recipient     TYPE REF TO if_recipient_bcs.
  DATA: lt_email_recip   TYPE STANDARD TABLE OF ty_email_recip.
  DATA: wa_email_recip   TYPE ty_email_recip.

  " Format date range for display
  CONCATENATE s_date-low+6(2)  '.' s_date-low+4(2)  '.' s_date-low+0(4)  INTO lv_date_from.
  CONCATENATE s_date-high+6(2) '.' s_date-high+4(2) '.' s_date-high+0(4) INTO lv_date_to.
  CONCATENATE lv_date_from ' to ' lv_date_to INTO lv_date_range.

  " Source line following YRGR091 pattern: SOURCE: GR102.UNAME.DATE.TIME
  CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) INTO lv_w_date.
  CONCATENATE 'SOURCE: GR102.' sy-uname '.' lv_w_date '.' sy-uzeit INTO lv_source.

  " Collect unique customers from filtered final table (Diff NE 0)
  LOOP AT it_final INTO DATA(wa_fe).
    READ TABLE lt_cust_email WITH KEY kunnr = wa_fe-customer TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      APPEND VALUE #( kunnr = wa_fe-customer ) TO lt_cust_email.
    ENDIF.
  ENDLOOP.

  IF lt_cust_email IS INITIAL.
    MESSAGE 'No pending overrun entries found for email.' TYPE 'I'.
    RETURN.
  ENDIF.

  " Process each customer
  LOOP AT lt_cust_email INTO DATA(wa_cust_em).

    REFRESH: lt_email_recip, lt_body, lt_att_hex.
    CLEAR: lv_csv_str.

    " Step 1: Get LOCIDs for the customer from OIJRRA
    SELECT DISTINCT locid FROM oijrra INTO TABLE @DATA(lt_locids)
           WHERE kunnr  = @wa_cust_em-kunnr
           AND   delind NE 'X'.

    IF lt_locids IS INITIAL.
      CONTINUE.
    ENDIF.

    " Step 2: Try to find recipients via OIJ_EL_TICKET_I tickets
    SELECT ticket_key, ernam, locid
           FROM oij_el_ticket_i
           INTO TABLE @DATA(lt_tkt)
           FOR ALL ENTRIES IN @lt_locids
           WHERE locid     = @lt_locids-locid
           AND   budat     IN @s_date
           AND   ticket_purpose = '1'
           AND   status    = 'C'
           AND   substatus = '6'
           AND   tktsubrc  NE '1A'.

    IF lt_tkt IS NOT INITIAL.
      " Convert ERNAM (CHAR12) to PERNR type (NUMC8) for PA0105 lookup
      TYPES: BEGIN OF ty_pernr_tkt,
               pernr TYPE pa0105-pernr,
             END OF ty_pernr_tkt.
      DATA: lt_pernr_tkt TYPE STANDARD TABLE OF ty_pernr_tkt.
      CLEAR lt_pernr_tkt.
      LOOP AT lt_tkt INTO DATA(wa_tkt_pn).
        APPEND VALUE ty_pernr_tkt( pernr = wa_tkt_pn-ernam ) TO lt_pernr_tkt.
      ENDLOOP.
      SORT lt_pernr_tkt BY pernr.
      DELETE ADJACENT DUPLICATES FROM lt_pernr_tkt COMPARING pernr.

      " Find email from ERNAM via PA0105 (ERNAM used as PERNR - ref YRGR095)
      SELECT usrid_long FROM pa0105
             INTO TABLE @DATA(lt_emails_tkt)
             FOR ALL ENTRIES IN @lt_pernr_tkt
             WHERE pernr  = @lt_pernr_tkt-pernr
             AND   subty  = '0010'
             AND   endda  = '99991231'.

      LOOP AT lt_emails_tkt INTO DATA(wa_etkt).
        IF wa_etkt-usrid_long IS NOT INITIAL.
          READ TABLE lt_email_recip WITH KEY smtp_addr = wa_etkt-usrid_long TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            APPEND VALUE #( smtp_addr = wa_etkt-usrid_long ) TO lt_email_recip.
          ENDIF.
        ENDIF.
      ENDLOOP.

    ELSE.
      " Step 3: No ticket found - get AENAMs from OIJNOMI
      SELECT DISTINCT aenam FROM oijnomi
             INTO TABLE @DATA(lt_aenam)
             FOR ALL ENTRIES IN @lt_locids
             WHERE locid  = @lt_locids-locid
             AND   idate  IN @s_date
             AND   delind NE 'X'.

      IF lt_aenam IS NOT INITIAL.
        " Convert AENAM (CHAR12) to PERNR type (NUMC8) for PA0105 lookup
        TYPES: BEGIN OF ty_pernr_nom,
                 pernr TYPE pa0105-pernr,
               END OF ty_pernr_nom.
        DATA: lt_pernr_nom TYPE STANDARD TABLE OF ty_pernr_nom.
        CLEAR lt_pernr_nom.
        LOOP AT lt_aenam INTO DATA(wa_aen_pn).
          APPEND VALUE ty_pernr_nom( pernr = wa_aen_pn-aenam ) TO lt_pernr_nom.
        ENDLOOP.
        SORT lt_pernr_nom BY pernr.
        DELETE ADJACENT DUPLICATES FROM lt_pernr_nom COMPARING pernr.

        " Find email from AENAM via PA0105 (AENAM used as PERNR - ref YRGR095)
        SELECT usrid_long FROM pa0105
               INTO TABLE @DATA(lt_emails_nom)
               FOR ALL ENTRIES IN @lt_pernr_nom
               WHERE pernr = @lt_pernr_nom-pernr
               AND   subty = '0010'
               AND   endda = '99991231'.

        LOOP AT lt_emails_nom INTO DATA(wa_enom).
          IF wa_enom-usrid_long IS NOT INITIAL.
            READ TABLE lt_email_recip WITH KEY smtp_addr = wa_enom-usrid_long TRANSPORTING NO FIELDS.
            IF sy-subrc NE 0.
              APPEND VALUE #( smtp_addr = wa_enom-usrid_long ) TO lt_email_recip.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF lt_email_recip IS INITIAL.
      CONTINUE.
    ENDIF.

    " Format customer number for display (strip leading zeros)
    lv_kunnr_disp = wa_cust_em-kunnr.
    SHIFT lv_kunnr_disp LEFT DELETING LEADING '0'.

    " Build email subject
    CONCATENATE 'Overrun Posting Pending for' lv_kunnr_disp
                'for' lv_date_range
                INTO lv_subject SEPARATED BY space.

    " Build attachment name (same as subject, truncated to field length)
    lv_att_name = lv_subject.

    " Build email body
    APPEND 'Dear Ma''am/ Sir,' TO lt_body.
    APPEND '' TO lt_body.
    CONCATENATE 'Please find below instances pertaining to the pending Overrun posting for'
                lv_kunnr_disp 'for' lv_date_range
                '. Please take necessary action in this regard.'
                INTO lv_body_line SEPARATED BY space.
    APPEND lv_body_line TO lt_body.
    APPEND '' TO lt_body.

    " Table header
    APPEND '-----------------------------------------------------------------------------------------------------' TO lt_body.
    APPEND 'Contract ID     |Cumulative Ovr  |Chargeable Ovr  |Posted Ovr      |Sales Order     |Invoice' TO lt_body.
    APPEND '-----------------------------------------------------------------------------------------------------' TO lt_body.

    " CSV header for attachment
    lv_csv_str = 'Contract ID,Cumulative Overrun (MBG),Chargeable Overrun (MBG),Posted Chargeable Ovr (MBG),Sales Order,Invoice'.

    " Table rows for this customer
    LOOP AT it_final INTO DATA(wa_row) WHERE customer = wa_cust_em-kunnr.
      DATA: lv_cum_c(15)     TYPE c.
      DATA: lv_char_c(15)    TYPE c.
      DATA: lv_posted_c(15)  TYPE c.
      DATA: lv_table_row     TYPE so_text255.

      WRITE wa_row-cum_bal_mbg_cal    TO lv_cum_c    DECIMALS 3.
      WRITE wa_row-char_bal_mbg_cal   TO lv_char_c   DECIMALS 3.
      WRITE wa_row-char_bal_mbg_cal_so TO lv_posted_c DECIMALS 3.

      CONDENSE: lv_cum_c, lv_char_c, lv_posted_c.

      CONCATENATE wa_row-cont_id         '|'
                  lv_cum_c               '|'
                  lv_char_c              '|'
                  lv_posted_c            '|'
                  wa_row-sal_order       '|'
                  wa_row-invoice
                  INTO lv_table_row.
      APPEND lv_table_row TO lt_body.

      " CSV row for attachment
      DATA: lv_csv_row TYPE string.
      CONCATENATE lv_csv_str cl_abap_char_utilities=>newline
                  wa_row-cont_id ',' lv_cum_c ',' lv_char_c ','
                  lv_posted_c ',' wa_row-sal_order ',' wa_row-invoice
                  INTO lv_csv_str.
    ENDLOOP.

    APPEND '-----------------------------------------------------------------------------------------------------' TO lt_body.
    APPEND '' TO lt_body.
    APPEND 'For more details, please execute T-code YRG011N/ YRGR102 with the required input' TO lt_body.
    APPEND '' TO lt_body.
    APPEND lv_source TO lt_body.

    " Convert CSV string to SOLIX for attachment
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text  = lv_csv_str
      IMPORTING
        buffer = lv_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.

    IF sy-subrc = 0.
      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer     = lv_xstring
        TABLES
          binary_tab = lt_att_hex.
    ENDIF.

    " Send email via cl_bcs
    TRY.
        lo_send_request = cl_bcs=>create_persistent( ).
      CATCH cx_send_req_bcs.
        CONTINUE.
    ENDTRY.

    TRY.
        lo_document = cl_document_bcs=>create_document(
                        i_type    = 'RAW'
                        i_text    = lt_body
                        i_subject = lv_subject ).
      CATCH cx_document_bcs.
        CONTINUE.
    ENDTRY.

    " Add CSV attachment
    IF lt_att_hex IS NOT INITIAL.
      TRY.
          lo_document->add_attachment(
            EXPORTING
              i_attachment_type    = 'CSV'
              i_attachment_subject = lv_att_name
              i_att_content_hex    = lt_att_hex ).
        CATCH cx_document_bcs.
          " Attachment failed - continue without it
      ENDTRY.
    ENDIF.

    TRY.
        lo_send_request->set_document( lo_document ).

        " Sender: gailpartcare@gail.co.in (same as YRGR091)
        lo_sender = cl_cam_address_bcs=>create_internet_address(
                      i_address_string = 'gailpartcare@gail.co.in' ).
        lo_send_request->set_sender(
          EXPORTING i_sender = lo_sender ).

        " Add all recipients
        LOOP AT lt_email_recip INTO wa_email_recip.
          lo_recipient = cl_cam_address_bcs=>create_internet_address(
                           wa_email_recip-smtp_addr ).
          lo_send_request->add_recipient(
            EXPORTING
              i_recipient = lo_recipient
              i_express   = 'X' ).
        ENDLOOP.

        lo_send_request->send(
          EXPORTING
            i_with_error_screen = 'X'
          RECEIVING
            result              = lv_sent_to_all ).
        COMMIT WORK.

      CATCH cx_address_bcs
            cx_send_req_bcs
            cx_bcs.
        MESSAGE 'Error sending email for customer.' TYPE 'I'.
    ENDTRY.

  ENDLOOP.

  MESSAGE 'Email processing complete.' TYPE 'I'.

ENDFORM.
