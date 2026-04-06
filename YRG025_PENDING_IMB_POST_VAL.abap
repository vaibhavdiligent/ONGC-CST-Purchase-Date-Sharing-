*&---------------------------------------------------------------------*
*& Include          YRG025_PENDING_IMB_POST_VAL
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form VALIDATION
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validation.
  IF s_date-low+6(2) = '01' OR s_date-low+6(2) = '16' .
  ELSE.
    MESSAGE 'Start date need to be monthly quater date' TYPE 'S'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date             = s_date-low
    IMPORTING
*     EV_MONTH_BEGIN_DATE = EV_MONTH_BEGIN_DATE
      ev_month_end_date   = l_date.

  IF s_date-high+6(2) = '15' OR s_date-high+6(2) = l_date+6(2) .
  ELSE.
    MESSAGE 'End date need to be monthly quater date' TYPE 'S'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .

*-> "*COC CHARM ID :4000009078 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:01.01.2025
*  SELECT nomtk,
*         nomit,
*         idate,
*         docnr,
*         locid,
*         partnr FROM oijnomi INTO TABLE @DATA(it_oij)
*    WHERE idate  IN @s_date
*      AND locid  IN @locid
*      AND partnr IN @partnr
*      AND sityp = 'ZD'
*      AND docind NE 'X'
*      AND delind NE 'X'.
*-> "*SOC CHARM ID :4000009078 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:01.01.2025
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
     AND a~locid  IN @locid
     AND a~partnr IN @partnr
     AND c~region IN @s_region
     AND a~sityp  = 'ZD'
     AND a~docind NE 'X'
     AND a~delind NE 'X'.

*-> "*EOC CHARM ID :4000009078 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:01.01.2025


  IF it_oij IS NOT INITIAL.

    SELECT a~pblnr AS locid b~street b~region b~name1 FROM oifspbl AS a
      INNER JOIN adrc AS b ON a~addrnum = b~addrnumber
      INTO TABLE it_adrc1 FOR ALL ENTRIES IN it_oij WHERE a~pblnr = it_oij-locid.

    SELECT * FROM yrvt_contract INTO TABLE @DATA(it_yrvt1)
      FOR ALL ENTRIES IN @it_oij
      WHERE vbeln      = @it_oij-docnr
        AND value_from LE @s_date-high
        AND value_to   GE @s_date-low
        AND clause     IN ( '10' , '11' ).

    IF it_yrvt1 IS NOT INITIAL.
      SELECT vbeln , kunnr , vbeln_grp FROM vbak INTO TABLE @DATA(it_vbak)
        FOR ALL ENTRIES IN @it_yrvt1
        WHERE vbeln = @it_yrvt1-vbeln.
*-> "*SOC CHARM ID :4000008690 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:15.10.2024
      IF it_vbak IS NOT INITIAL.
        SELECT vbeln,kunnr FROM vbak INTO TABLE @DATA(lt_vbak)
          FOR ALL ENTRIES IN @it_vbak
          WHERE vbeln = @it_vbak-vbeln_grp.
      ENDIF.
*-> "*EOC CHARM ID :4000008690 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:15.10.2024
    ENDIF.

    SELECT vbeln, kunnr, adrnr FROM vbpa INTO TABLE @DATA(it_vbpa)
      FOR ALL ENTRIES IN @it_oij
      WHERE vbeln = @it_oij-docnr
        AND parvw = 'RG'.

    IF it_vbpa IS NOT INITIAL.
      SELECT addrnumber, name1, region FROM adrc INTO TABLE @DATA(it_adrc)
        FOR ALL ENTRIES IN @it_vbpa WHERE addrnumber = @it_vbpa-adrnr.
    ENDIF.
*-> SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:22.10.2024
    SELECT a~pblnr AS locid,
           b~street,
           b~region,
           b~name1
      FROM oifspbl AS a
      INNER JOIN adrc AS b ON a~addrnum = b~addrnumber
      INTO TABLE @DATA(it_adrc1)
      FOR ALL ENTRIES IN @it_oij
      WHERE a~pblnr = @it_oij-locid.
*-> "*EOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:22.10.2024
  ENDIF.


  IF it_yrvt1 IS NOT INITIAL.

    SELECT * FROM yrg_cumm_imb INTO TABLE @DATA(it_ycum)
      FOR ALL ENTRIES IN @it_yrvt1
      WHERE yy_contract = @it_yrvt1-vbeln
        AND begda       = @s_date-low
        AND endda       = @s_date-high.
  ENDIF.


  IF it_ycum IS NOT INITIAL.

    SELECT * FROM yrva_zcontrcls INTO TABLE @DATA(it_zcon)
      FOR ALL ENTRIES IN @it_ycum
*-> SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:22.10.2024
      WHERE yyvbeln = @it_ycum-vbeln
*      WHERE yyvbeln = @it_ycum-yy_contract
        AND yyclause_id IN ( '10' , '11' ).

    SELECT vbelv,
           vbeln,
           vbtyp_n,
           erdat,
           erzet
      FROM vbfa
      INTO TABLE @DATA(lt_vbfa)
      FOR ALL ENTRIES IN @it_ycum
      WHERE vbelv  = @it_ycum-vbeln
        AND vbtyp_n IN ( 'M' , 'P' ).

    SORT lt_vbfa BY vbelv DESCENDING erdat DESCENDING erzet DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_vbfa COMPARING vbelv.
  ENDIF.

  SELECT * FROM yrga_oic_region  INTO TABLE @DATA(it_yrga_oic_region) .
  IF it_yrga_oic_region[] IS NOT INITIAL.
    SELECT pernr, usrid_long AS usrid FROM pa0105 INTO TABLE @DATA(it_pa0105)
      FOR ALL ENTRIES IN @it_yrga_oic_region
      WHERE pernr = @it_yrga_oic_region-oic_pernr AND subty = '0010' AND endda = '99991231' .
  ENDIF.
*-> "*SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:09.11.2024
  IF it_oij IS NOT INITIAL.
    SELECT vbeln, abrvw
      FROM vbak INTO TABLE @DATA(gt_vbak)
      FOR ALL ENTRIES IN @it_oij
      WHERE vbeln = @it_oij-docnr.
  ENDIF.

  IF it_oij IS NOT INITIAL.
    SELECT locid, tsyst
      FROM oijtsloc
      INTO TABLE @DATA(lt_oijt)
      FOR ALL ENTRIES IN @it_oij
      WHERE locid = @it_oij-locid
        AND tsyst LIKE 'NGPL%'
        AND delind NE 'X'.
    SELECT vbeln, vkbur
      FROM vbak
      INTO TABLE @DATA(lt_vbak1)
      FOR ALL ENTRIES IN @it_oij
      WHERE vbeln = @it_oij-docnr.
  ENDIF.


*-> "*SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:09.11.2024


  SORT it_zcon BY yyvbeln ASCENDING yyclause_id ASCENDING.
  *SORT IT_YCUM by .
  "BREAK-POINT.
  DATA(it_yrvt) = it_yrvt1[].
  SORT it_yrvt BY vbeln.
  DELETE ADJACENT DUPLICATES FROM it_yrvt COMPARING vbeln.

  LOOP AT it_yrvt INTO DATA(vt).
    READ TABLE it_vbak INTO DATA(ls_tmp) WITH KEY vbeln = vt-vbeln.
    IF ls_tmp-vbeln_grp IS NOT INITIAL.
      READ TABLE lt_vbak INTO DATA(lw_tmp) WITH KEY vbeln = ls_tmp-vbeln_grp.
      IF sy-subrc = 0.
        wa_cust-kunnr = lw_tmp-kunnr.
      ENDIF.

*-> "*SOC CHARM ID :4000008690 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:15.10.2024
    ELSE.
      READ TABLE it_oij INTO DATA(oi) WITH KEY docnr = vt-vbeln.
      IF sy-subrc = 0.
        wa_cust-kunnr = oi-partnr.
      ENDIF.
*-> "*EOC CHARM ID :4000008690 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:15.10.2024
    ENDIF.
    APPEND wa_cust TO it_cust.
    CLEAR: wa_cust, vt, oi.
  ENDLOOP.

  SORT it_cust BY kunnr.
  DELETE ADJACENT DUPLICATES FROM it_cust COMPARING kunnr.
  IF ch1 EQ 'X'.
    LOOP AT it_cust INTO wa_cust.

      wa_rspartab-selname = 'S_DATE'.
      wa_rspartab-kind    = 'S'.
      wa_rspartab-sign    = 'I'.
      wa_rspartab-option  = 'BT'.
      wa_rspartab-low     = s_date-low.
      wa_rspartab-high    = s_date-high.
      APPEND wa_rspartab TO i_rspartab.
      CLEAR wa_rspartab.

      wa_rspartab-selname = 'P_KUNNR'.
      wa_rspartab-kind    = 'P'.
      wa_rspartab-low     = wa_cust-kunnr.
      APPEND wa_rspartab TO i_rspartab.
      CLEAR wa_rspartab.

      cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                              metadata  = abap_false
                                              data      = abap_true ).
*SOCOMMENT by MANMOHAN/SHREYOSI Charm:4000008447 Date:09/07/2024
*      SUBMIT yrg011_imbalance_report_dcq USING SELECTION-SCREEN '1000'
*        WITH SELECTION-TABLE i_rspartab WITH r_d5 = 'X' AND RETURN.
*EOCOMMENT by MANMOHAN/SHREYOSI Charm:4000008447 Date:09/07/2024
*SOC by MANMOHAN/SHREYOSI Charm:4000008814 Date:23/09/2024
      IF sy-tcode NE 'YRGR104'.
        SUBMIT yrg011_imbalance_report_dcq USING SELECTION-SCREEN '1000'
          WITH SELECTION-TABLE i_rspartab WITH r_d5 = 'X' AND RETURN.
      ENDIF.
*EOC by MANMOHAN/SHREYOSI Charm:4000008814 Date:23/09/2024
** -> Begin of changes by of Aditi on 29.11.2024 14:32:33 for ATC
      IMPORT it_display1[] FROM MEMORY ID 'PENDING_IMB_POST'. "#EC CI_FLDEXT_OK[2215424]
** -> End of changes by of Aditi on 29.11.2024 14:32:35 for ATC
      FREE MEMORY ID 'PENDING_IMB_POST'.

      APPEND LINES OF it_display1[] TO it_display2[].

      REFRESH: i_rspartab[], it_display1[].
      CLEAR:wa_rspartab, wa_cust.
    ENDLOOP.
  ENDIF.
  cl_salv_bs_runtime_info=>set( EXPORTING display  =  abap_true
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
        wa_final-m_cont_id = wvbak-vbeln_grp.
*        wa_final-m_mas_cust = wvbak-kunnr.  "*COC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:15.10.2024
      ENDIF.
*-> "*SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:15.10.2024
      READ TABLE lt_vbak INTO DATA(ls_vbak) WITH KEY vbeln = wvbak-vbeln_grp.
      IF sy-subrc = 0.
        wa_final-m_mas_cust = ls_vbak-kunnr.
      ENDIF.
*-> "*EOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:15.10.2024
****      SELECT SINGLE tsyst FROM oijtsloc INTO wa_final-trans_sys WHERE locid = oij-locid AND tsyst LIKE 'NGPL%'
****        AND delind NE 'X'.
****      SELECT SINGLE vkbur FROM vbak INTO wa_final-sal_office WHERE vbeln = oij-docnr.

      READ TABLE lt_oijt INTO DATA(ls_oijt) WITH KEY locid = oij-locid.
      IF sy-subrc = 0.
        wa_final-trans_sys = ls_oijt-tsyst.
      ENDIF.
      READ TABLE lt_vbak1 INTO DATA(ls_vbak1) WITH KEY vbeln = oij-docnr.
      IF sy-subrc = 0.
        wa_final-sal_office = ls_vbak1-vkbur.
      ENDIF.


      READ TABLE it_vbpa INTO DATA(vbpa) WITH KEY vbeln = oij-docnr.
      IF sy-subrc = 0.
        wa_final-payer = vbpa-kunnr.
        READ TABLE it_adrc INTO DATA(adrc)  WITH KEY addrnumber = vbpa-adrnr.
        IF sy-subrc = 0.
          wa_final-payer_name = adrc-name1.
        ENDIF.
      ENDIF.
*-> "*SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:22.10.2024
      READ TABLE it_adrc1 INTO DATA(adrc1) WITH KEY locid = oij-locid.
      IF sy-subrc = 0.
        wa_final-oic_region = adrc1-region.
      ENDIF.
*-> "*EOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:22.10.2024
    ENDIF.
*->SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:28.10.2024

    READ TABLE gt_vbak INTO DATA(lv_abrvw) WITH KEY vbeln = wa_final-cont_id.
    IF sy-subrc EQ 0.
      wa_final-usage = lv_abrvw-abrvw.
    ENDIF.
*    LOOP AT it_display2 INTO DATA(dis) WHERE vbeln = yrvt-vbeln AND kunnr = oij-partnr.
    READ TABLE it_display2 INTO DATA(dis) WITH KEY vbeln = yrvt-vbeln."kunnr = oij-partnr.
    IF sy-subrc EQ 0.
      wa_final-cum_bal_mbg_cal = dis-totalcumi.
      READ TABLE it_display2 INTO DATA(ls_dis) WITH KEY vbeln = wa_final-m_cont_id.
      IF sy-subrc EQ 0.
        IF lv_abrvw-abrvw = 'Z01' AND wa_final-m_cont_id IS NOT INITIAL.
          wa_final-char_bal_mbg_cal = ls_dis-totalpic.
          wa_final-neg_bal_mbg_cal  = ls_dis-totalnic.
          wa_final-cum_mst_imb      = ls_dis-totalcumi.

        ELSEIF lv_abrvw-abrvw = '' AND wa_final-m_cont_id IS NOT INITIAL.
          wa_final-char_bal_mbg_cal = 0." wa_final-char_bal_mbg_cal + dis-totalpic.
          wa_final-neg_bal_mbg_cal  = 0." wa_final-neg_bal_mbg_cal + dis-totalnic.
          wa_final-cum_mst_imb      = ls_dis-totalcumi.
        ENDIF.
      ELSE.
        wa_final-char_bal_mbg_cal = dis-totalpic." wa_final-char_bal_mbg_cal + dis-totalpic.
        wa_final-neg_bal_mbg_cal  = dis-totalnic."wa_final-neg_bal_mbg_cal + dis-totalnic.
      ENDIF.

    ENDIF.
    CLEAR:dis.
*    ENDLOOP.
*-> EOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:28.10.2024

*    LOOP AT it_display2 INTO DATA(dis) WHERE vbeln = yrvt-vbeln AND kunnr = oij-partnr.
*      wa_final-cum_bal_mbg_cal = wa_final-cum_bal_mbg_cal_so + dis-totalcumi.
*      IF dis-totalpic IS NOT INITIAL AND dis-totalnic IS INITIAL.
*        wa_final-char_bal_mbg_cal = wa_final-char_bal_mbg_cal + dis-totalpic.
*      ELSEIF dis-totalpic IS INITIAL AND dis-totalnic IS NOT INITIAL.
*        wa_final-char_bal_mbg_cal = wa_final-char_bal_mbg_cal + dis-totalnic.
*      ELSE.
*        wa_final-char_bal_mbg_cal = ''.
*      ENDIF.
*
*      CLEAR:dis.
*    ENDLOOP.

    READ TABLE it_ycum INTO DATA(ycum) WITH KEY  yy_contract = yrvt-vbeln.
    IF sy-subrc = 0.
      wa_final-cum_bal_mbg_cal_so = ycum-yy_oij_cumimb.
*      wa_final-sal_order = ycum-yy_contract.  "-- by MANMOHAN/SHREYOSI Charm:4000008447 Date:09/07/2024
      IF lv_abrvw-abrvw = '' AND wa_final-m_cont_id IS NOT INITIAL. "*->SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:11.11.2024
        wa_final-sal_order = ''. "*->SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:11.11.2024
      ELSE.
        wa_final-sal_order = ycum-vbeln. "++ by MANMOHAN/SHREYOSI Charm:4000008447 Date:09/07/2024
      ENDIF.
    ELSE.
      wa_final-indicator = 'N'.
    ENDIF.

*    READ TABLE lt_vbfa INTO DATA(wa_vbfa) WITH KEY vbelv = ycum-vbeln."*->COC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:11.11.2024
    READ TABLE lt_vbfa INTO DATA(wa_vbfa) WITH KEY vbelv = wa_final-sal_order. "*->SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:11.11.2024
    IF sy-subrc EQ 0 AND ( wa_vbfa-vbtyp_n = 'M' OR wa_vbfa-vbtyp_n = 'P' ).
      wa_final-invoice = wa_vbfa-vbeln.
    ENDIF.

*-> SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:28.10.2024
    IF wa_final-sal_order IS NOT INITIAL.
      READ TABLE it_zcon INTO DATA(wzcon) WITH KEY yyvbeln = wa_final-sal_order yyclause_id = 10 ."BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-char_bal_mbg_cal_so = wzcon-yyzmeng.
      ENDIF.

      READ TABLE it_zcon INTO DATA(wzcon1) WITH KEY yyvbeln = wa_final-sal_order yyclause_id = 11 ."BINARY SEARCH. "*->SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:11.11.2024
      IF sy-subrc = 0.
        wa_final-neg_bal_mbg_cal_so = wzcon1-yyzmeng.
      ENDIF.
    ENDIF.
*-> EOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:28.10.2024
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
    CLEAR: wa_final,
           ycum,
           adrc,
           oij,
           yrvt,
           vbpa,
           dis,
           wa_adrc1,
           oicr,
           wa_pa0105,
           lv_abrvw,
           wzcon1,
           wzcon,
           ls_dis,
           wa_vbfa,
           ls_vbak.

  ENDLOOP.
*-> "*SOC CHARM ID :4000009078 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:02.01.2025
  IF s_payer IS NOT INITIAL AND s_vkbur IS NOT INITIAL .
    SORT it_final BY blocation DESCENDING customer DESCENDING payer DESCENDING.
    DELETE it_final WHERE payer NOT IN s_payer OR sal_office NOT IN s_vkbur .
  ENDIF.
  IF s_payer IS NOT INITIAL .
    SORT it_final BY blocation DESCENDING customer DESCENDING payer DESCENDING.
    DELETE it_final WHERE payer NOT IN s_payer .
  ENDIF.
  IF s_vkbur IS NOT INITIAL.
    SORT it_final BY blocation DESCENDING customer DESCENDING payer DESCENDING.
    DELETE it_final[] WHERE  sal_office NOT IN s_vkbur.
  ENDIF.
*-> "*EOC CHARM ID :4000009078 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:02.01.2025

ENDFORM.
FORM e03_eventtab_build  USING  e03_lt_events TYPE slis_t_event.

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
*& Form TOP_OF_PAGE
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
FORM comment_build  USING lt_top_of_page TYPE slis_t_listheader.

  DATA: ls_line  TYPE slis_listheader,
        l_date_l TYPE char10,
        l_date_2 TYPE char10.

  CLEAR: l_date_l, l_date_2, ls_line.

  ls_line-typ = 'H'. "'H' TLGASDAY
  CONCATENATE s_date-low+6(2) '.' s_date-low+4(2) '.' s_date-low+0(4) INTO l_date_l.
  CONCATENATE s_date-high+6(2) '.' s_date-high+4(2) '.' s_date-high+0(4) INTO l_date_2.
  CONCATENATE 'Report for Date Range:' l_date_l 'to' l_date_2 INTO ls_line-info SEPARATED BY space ..
  APPEND ls_line TO lt_top_of_page.
  CLEAR:ls_line.
*  ls_line-typ = 'S'.
*  ls_line-info = 'Note: N means no posting done through Summary for Overrun'.
*  APPEND ls_line TO lt_top_of_page.
*  CLEAR:ls_line.
  ls_line-typ = 'S'.
  ls_line-info = 'Note: N means no posting done through Summary for Imbalance'.
  APPEND ls_line TO lt_top_of_page.
  CLEAR:ls_line.

  ls_line-typ = 'S'.
  ls_line-info = 'All Values are in MBG'.
  APPEND ls_line TO lt_top_of_page.
  CLEAR:ls_line.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_FIELDCAT
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_fieldcat.
  CLEAR:wa_fcat.
  REFRESH:it_fcat[].
  DATA: sno TYPE char2.
  CLEAR:sno.
*
*  IF it_final IS NOT INITIAL.
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
  wa_fcat-seltext_l = 'Contract Id'.
  wa_fcat-outputlen = 15.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'M_CONT_ID'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Master Contract'.
  wa_fcat-outputlen = 15.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'M_MAS_CUST'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Master Customer'.
  wa_fcat-outputlen = 15.
  APPEND wa_fcat TO it_fcat.

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

*-> SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:28.10.2024
  IF ch1 EQ 'X'.
    CLEAR wa_fcat.
    wa_fcat-col_pos   = sno + 1.
    wa_fcat-fieldname = 'CUM_MST_IMB'.
    wa_fcat-tabname   = 'IT_FINAL'.
    wa_fcat-seltext_l = 'Cumulative Master Imb (Calculated)'.
    wa_fcat-outputlen = 25.
    APPEND wa_fcat TO it_fcat.
*-> EOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:28.10.2024

*    IF ch1 EQ 'X'.
    CLEAR wa_fcat.
    wa_fcat-col_pos   = sno + 1.
    wa_fcat-fieldname = 'CUM_BAL_MBG_CAL'.
    wa_fcat-tabname   = 'IT_FINAL'.
*    wa_fcat-seltext_l = 'Cumulative Imb in MBG (calculated)'.
    wa_fcat-seltext_l = 'Cumulative Imb (calculated)'.
    wa_fcat-outputlen = 25.
    APPEND wa_fcat TO it_fcat.

    CLEAR wa_fcat.
    wa_fcat-col_pos   = sno + 1.
    wa_fcat-fieldname = 'CHAR_BAL_MBG_CAL'.
    wa_fcat-tabname   = 'IT_FINAL'.
    wa_fcat-seltext_l = 'Positive Chg Imb (calculated)'. "*-> SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:22.10.2024
*    wa_fcat-seltext_l = 'Chargeable Imb in MBG (calculated)'.
    wa_fcat-outputlen = 25.
    APPEND wa_fcat TO it_fcat.
*-> SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:22.10.2024
    CLEAR wa_fcat.
    wa_fcat-col_pos   = sno + 1.
    wa_fcat-fieldname = 'NEG_BAL_MBG_CAL'.
    wa_fcat-tabname   = 'IT_FINAL'.
    wa_fcat-seltext_l = ' Negative Chg Imb (calculated)'.
    wa_fcat-outputlen = 25.
    APPEND wa_fcat TO it_fcat.
*-> EOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:22.10.2024
  ENDIF.
  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'USAGE'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Usage'.
  wa_fcat-outputlen = 5.
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
  wa_fcat-seltext_l = 'Cumulative Imb(posted)'.
*  wa_fcat-seltext_l = 'Cumulative Imb in MBG (posted in SO)'.
  wa_fcat-outputlen = 25.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'CHAR_BAL_MBG_CAL_SO'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Positive Chg Imb (posted in SO)'. "*-> SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:22.10.2024
*  wa_fcat-seltext_l = 'Chargeable Imb in MBG (posted in SO)'.
  wa_fcat-outputlen = 25.
  APPEND wa_fcat TO it_fcat.

*-> SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:22.10.2024
  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'NEG_BAL_MBG_CAL_SO'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Negative Chg Imb (posted in SO)'.
  wa_fcat-outputlen = 25.
  APPEND wa_fcat TO it_fcat.
*-> EOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:22.10.2024
  IF dnpi_flag = 'X'. "*-> SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:29.10.2024
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
*
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_DISPLAY
*&---------------------------------------------------------------------*
* text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv_display.

  w_layout-colwidth_optimize = 'X'.
*SOC BY MANMOHAN/SHREYOSI CHARM: 2000000816 DATE:16.07.2024

  it_final_im[] = it_final[].
  IF sy-tcode EQ 'YRGR104' OR sy-cprog EQ 'YRGR_032_GMS_CT'.
    EXPORT it_final_im TO MEMORY ID 'IM_DATA'.
  ENDIF.
*EOC BY MANMOHAN/SHREYOSI CHARM: 2000000816 DATE:16.07.2024
*  IF it_final IS NOT INITIAL.
  DATA: v_repid1 TYPE sy-repid.
  v_repid1 = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK        = ' '
*     I_BYPASSING_BUFFER       = ' '
*     I_BUFFER_ACTIVE          = ' '
      i_callback_program       = v_repid1
*     I_CALLBACK_PF_STATUS_SET = ' '
*     I_CALLBACK_USER_COMMAND  = ' '
*     I_CALLBACK_TOP_OF_PAGE   = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE = ' '
*     I_CALLBACK_HTML_END_OF_LIST = ' '
*     I_STRUCTURE_NAME         =
*     I_BACKGROUND_ID          = ' '
*     I_GRID_TITLE             =
*     I_GRID_SETTINGS          =
      is_layout                = w_layout
      it_fieldcat              = it_fcat
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
*     IT_SORT                  =
*     IT_FILTER                =
*     IS_SEL_HIDE              =
*     I_DEFAULT                = 'X'
      i_save                   = 'X'
*     IS_VARIANT               =
      it_events                = gt_events[]
*     IT_EVENT_EXIT            =
*     IS_PRINT                 =
*     IS_REPREP_ID             =
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*     I_HTML_HEIGHT_TOP        = 0
*     I_HTML_HEIGHT_END        = 0
*     IT_ALV_GRAPHICS          =
*     IT_HYPERLINK             =
*     IT_ADD_FIELDCAT          =
*     IT_EXCEPT_QINFO          =
*     IR_SALV_FULLSCREEN_ADAPTER =
*   IMPORTING
*     E_EXIT_CAUSED_BY_CALLER  =
*     ES_EXIT_CAUSED_BY_USER   =
    TABLES
      t_outtab                 = it_final[]
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  .
  IF sy-subrc <> 0.
*   Implement suitable error handling here
  ENDIF.



*  ENDIF.

ENDFORM.
