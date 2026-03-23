*&                                                                       *
*& Include              YRG025_PENDING_IMB_POST_VAL
*&                                                                       *
*&                                                                       *
*&        Form VALIDATION
*&                                                                       *
*         text
*                                                                        *
*     > p1         text
* <      p2        text
* *
FORM validation.
   IF s_date-low+6(2) = '01' OR s_date-low+6(2) = '16' .
   ELSE.
     MESSAGE 'Start date need to be monthly quater date' TYPE 'S'.
     EXIT.
   ENDIF.

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date           = s_date-low
    IMPORTING
*     EV_MONTH_BEGIN_DATE       = EV_MONTH_BEGIN_DATE
      ev_month_end_date = l_date.

  IF s_date-high+6(2) = '15' OR s_date-high+6(2) = l_date+6(2) .
  ELSE.
    MESSAGE 'End date need to be monthly quater date' TYPE 'S'.
    EXIT.
  ENDIF.

ENDFORM.
*&                                                                       *
*&       Form GET_DATA
*&                                                                       *
*        text
*                                                                        *
*    > p1         text
* <     p2        text
*                                                                        *
FORM get_data .

*     >        "*COC CHARM ID :4000009078   TECHICAL : RAVINDER SINGH    F UNCTIONAL : SHREYOSI DT:01.01.2025
* SELECT nomtk,
*         nomit,
*         idate,
*         docnr,
*         locid,
*         partnr FROM oijnomi INTO TABLE @DATA(it_oij)
*                                  WHERE idate IN @s_date
*                                  AND   locid IN @locid
*                                  AND   partnr IN @partnr
*                                  AND   sityp = 'ZD'
*                                  AND   docind NE 'X'
*                                  AND   delind NE 'X'.
*     >        "*SOC CHARM ID :4000009078   TECHICAL : RAVINDER SINGH    F UNCTIONAL : SHREYOSI DT:01.01.2025
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
                          WHERE a~idate IN @s_date
                          AND    a~locid IN @locid
                          AND    a~partnr IN @partnr
                          AND    c~region IN @s_region
                          AND    a~sityp = 'ZD'
                          AND    a~docind NE 'X'
                          AND    a~delind NE 'X'.

*     >         "*EOC CHARM ID :4000009078     TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:01.01.2025


  IF it_oij IS NOT INITIAL.


    SELECT a~pblnr AS locid b~street b~region b~name1 FROM oifspbl AS a INNER JOIN adrc AS b ON a~addrnum = b~addrnumber
      INTO TABLE it_adrc1 FOR ALL ENTRIES IN   it_oij WHERE a~pblnr = it_oij-locid.

      SELECT * FROM yrvt_contract INTO TABLE @DATA(it_yrvt1)
                     FOR ALL ENTRIES IN      @it_oij
                       WHERE vbeln        = @it_oij-docnr
                       AND   value_from   LE    @s_date-high
                       AND   value_to     GE    @s_date-low
                       AND   clause     IN ( '10' , '11' ).

      IF it_yrvt1 IS NOT INITIAL.
        SELECT vbeln , kunnr , vbeln_grp FROM vbak INTO TABLE @DATA(it_vba k)
                      FOR ALL ENTRIES IN @it_yrvt1
                      WHERE vbeln = @it_yrvt1-vbeln.
*     >        "*SOC CHARM ID :4000008690   TECHICAL : RAVINDER SINGH        F UNCTIONAL : SHREYOSI DT:15.10.2024
      IF it_vbak IS NOT INITIAL.
        SELECT vbeln,kunnr FROM vbak INTO TABLE @DATA(lt_vbak)
           FOR ALL ENTRIES IN @it_vbak
                      WHERE vbeln = @it_vbak-vbeln_grp.
      ENDIF.
*     >        "*EOC CHARM ID :4000008690   TECHICAL : RAVINDER SINGH        F UNCTIONAL : SHREYOSI DT:15.10.2024
    ENDIF.

      SELECT vbeln, kunnr, adrnr FROM vbpa INTO TABLE @DATA(it_vbpa)
                     FOR ALL ENTRIES IN @it_oij
                     WHERE vbeln = @it_oij-docnr
                     AND   parvw = 'RG'.

      IF it_vbpa IS NOT INITIAL.
        SELECT addrnumber, name1, region FROM adrc INTO TABLE @DATA(it_ad rc)
        FOR ALL ENTRIES IN @it_vbpa WHERE addrnumber = @it_vbpa-adrnr.
    ENDIF.
*     >        SOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH FUN CTIONAL : SHREYOSI DT:22.10.2024
    SELECT a~pblnr AS locid,
           b~street,
           b~region,
           b~name1
      FROM oifspbl AS a
      INNER JOIN adrc AS b ON a~addrnum = b~addrnumber
      INTO TABLE @DATA(it_adrc1)
      FOR ALL ENTRIES IN @it_oij
      WHERE a~pblnr = @it_oij-locid.
*     >        "*EOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH F UNCTIONAL : SHREYOSI DT:22.10.2024
  ENDIF.


    IF it_yrvt1 IS NOT INITIAL.

      SELECT * FROM yrg_cumm_imb INTO TABLE @DATA(it_ycum)
                             FOR ALL ENTRIES IN @it_yrvt1
                                 WHERE yy_contract = @it_yrvt1-vbeln
                                 AND   begda = @s_date-low
                                 AND   endda = @s_date-high.
    ENDIF.


    IF it_ycum IS NOT INITIAL.

    SELECT * FROM yrva_zcontrcls INTO TABLE @DATA(it_zcon)
                      FOR ALL ENTRIES IN @it_ycum
                      WHERE yyvbeln = @it_ycum-vbeln    "*    > SOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREY OSI DT:22.10.2024
*                      WHERE yyvbeln = @it_ycum-yy_contract
                      AND   yyclause_id IN ( '10' , '11' ).

      SELECT vbelv,
           vbeln,
           vbtyp_n,
           erdat,
           erzet
      FROM vbfa
      INTO TABLE @DATA(lt_vbfa)
      FOR ALL ENTRIES IN @it_ycum
      WHERE vbelv = @it_ycum-vbeln
      AND vbtyp_n IN ( 'M' , 'P' ).

      SORT lt_vbfa BY vbelv DESCENDING erdat DESCENDING   erzet DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_vbfa COMPARING vbelv.
    ENDIF.

  SELECT * FROM yrga_oic_region INTO TABLE @DATA(it_yrga_oic_region) .
  IF it_yrga_oic_region[] IS NOT INITIAL.
    SELECT pernr, usrid_long AS usrid FROM pa0105 INTO TABLE @DATA(it_pa 0105)
      FOR ALL ENTRIES IN @it_yrga_oic_region
      WHERE pernr = @it_yrga_oic_region-oic_pernr AND subty = '0010' AND  endda = '99991231' .
  ENDIF.
*     >         "*SOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH   F UNCTIONAL : SHREYOSI DT:09.11.2024
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



*     >        "*SOC CHARM ID :4000008814    TECHICAL : RAVINDER SINGH   F UNCTIONAL : SHREYOSI DT:09.11.2024


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

*     >        "*SOC CHARM ID :4000008690   TECHICAL : RAVINDER SINGH    F UNCTIONAL : SHREYOSI DT:15.10.2024
    ELSE.
      READ TABLE it_oij INTO DATA(oi) WITH KEY docnr = vt-vbeln.
      IF sy-subrc = 0.
        wa_cust-kunnr = oi-partnr.
      ENDIF.
*     >        "*EOC CHARM ID :4000008690   TECHICAL : RAVINDER SINGH    F UNCTIONAL : SHREYOSI DT:15.10.2024
    ENDIF.
    APPEND wa_cust TO it_cust.
    CLEAR: wa_cust, vt, oi.
  ENDLOOP.

  SORT it_cust BY kunnr.
  DELETE ADJACENT DUPLICATES FROM it_cust COMPARING kunnr.
*SOC - Replace SUBMIT with local fetch_imbalance_data
  IF ch1 EQ 'X'.
    LOOP AT it_cust INTO wa_cust.
      REFRESH it_display1[].
      PERFORM fetch_imbalance_data USING wa_cust-kunnr.
      APPEND LINES OF it_display1[] TO it_display2[].
      REFRESH it_display1[].
      CLEAR wa_cust.
    ENDLOOP.
  ENDIF.
*EOC - Replace SUBMIT with local fetch_imbalance_data


  "BREAK-POINT.
  "to fill final output table
  LOOP AT it_yrvt INTO DATA(yrvt).

    READ TABLE it_oij INTO DATA(oij) WITH KEY docnr = yrvt-vbeln.
    IF sy-subrc = 0.
      wa_final-blocation           =    oij-locid.
      wa_final-customer            =    oij-partnr.
      wa_final-cont_id             =    oij-docnr.
      READ TABLE it_vbak INTO DATA(wvbak) WITH KEY vbeln = yrvt-vbeln.
      IF sy-subrc = 0.
        wa_final-m_cont_id             =   wvbak-vbeln_grp.
*        wa_final-m_mas_cust            =   wvbak-kunnr.       "*COC CHAR M ID :4000008814    TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT: 15.10.2024
      ENDIF.
*     >         "*SOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH F UNCTIONAL : SHREYOSI DT:15.10.2024
      READ TABLE lt_vbak INTO DATA(ls_vbak) WITH KEY vbeln = wvbak-vbeln_grp.
      IF sy-subrc = 0.
        wa_final-m_mas_cust            =   ls_vbak-kunnr.
      ENDIF.
*     >         "*EOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH F UNCTIONAL : SHREYOSI DT:15.10.2024
****       SELECT SINGLE tsyst FROM oijtsloc INTO wa_final-trans_sys WHER E locid = oij-locid AND tsyst LIKE 'NGPL%'
****    AND delind NE 'X'.
****       SELECT SINGLE vkbur FROM vbak INTO wa_final-sal_office WHERE v beln = oij-docnr.

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
         wa_final-payer     =   vbpa-kunnr.
         READ TABLE it_adrc INTO DATA(adrc) WITH KEY addrnumber = vbpa- adrnr.
        IF sy-subrc = 0.
           wa_final-payer_name     =   adrc-name1.
        ENDIF.
      ENDIF.
*     >         "*SOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH F UNCTIONAL : SHREYOSI DT:22.10.2024
      READ TABLE it_adrc1 INTO DATA(adrc1) WITH KEY locid = oij-locid.
      IF sy-subrc = 0.
        wa_final-oic_region      =   adrc1-region.
      ENDIF.
*     >         "*EOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH F UNCTIONAL : SHREYOSI DT:22.10.2024
    ENDIF.
*     >SOC CHARM ID :4000008814    TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:28.10.2024

    READ TABLE gt_vbak INTO DATA(lv_abrvw) WITH KEY vbeln = wa_final-con t_id.
    IF sy-subrc EQ 0.
       wa_final-usage = lv_abrvw-abrvw.
    ENDIF.
*     LOOP AT it_display2 INTO DATA(dis) WHERE vbeln = yrvt-vbeln AND kun nr = oij-partnr.
    READ TABLE it_display2 INTO DATA(dis) WITH KEY vbeln = yrvt-vbeln." kunnr = oij-partnr.
    IF sy-subrc EQ 0.
      wa_final-cum_bal_mbg_cal =    dis-totalcumi.
      READ TABLE it_display2 INTO DATA(ls_dis) WITH KEY vbeln = wa_final-m_cont_id.
      IF sy-subrc EQ 0.
        IF lv_abrvw-abrvw = 'Z01' AND wa_final-m_cont_id IS NOT INITIAL.
          wa_final-char_bal_mbg_cal = ls_dis-totalpic.
          wa_final-neg_bal_mbg_cal = ls_dis-totalnic.
          wa_final-cum_mst_imb      = ls_dis-totalcumi.

        ELSEIF lv_abrvw-abrvw = '' AND wa_final-m_cont_id IS NOT INITIAL.
          wa_final-char_bal_mbg_cal = 0." wa_final-char_bal_mbg_cal + di s-totalpic.
          wa_final-neg_bal_mbg_cal = 0." wa_final-neg_bal_mbg_cal + di s-totalnic.
          wa_final-cum_mst_imb      = ls_dis-totalcumi.
        ENDIF.
      ELSE.
        wa_final-char_bal_mbg_cal = dis-totalpic." wa_final-char_bal_mbg_cal + dis-totalpic.
        wa_final-neg_bal_mbg_cal = dis-totalnic."wa_final-neg_bal_mbg_ cal + dis-totalnic.
      ENDIF.

    ENDIF.
    CLEAR:dis.
*    ENDLOOP.
*     >   EOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH       FUNCTION AL : SHREYOSI DT:28.10.2024

*    LOOP AT it_display2 INTO DATA(dis) WHERE vbeln = yrvt-vbeln AND kun nr = oij-partnr.
*       wa_final-cum_bal_mbg_cal =     wa_final-cum_bal_mbg_cal_so + dis- totalcumi.
*       IF dis-totalpic IS NOT INITIAL AND dis-totalnic IS INITIAL.
*         wa_final-char_bal_mbg_cal = wa_final-char_bal_mbg_cal + dis-t otalpic.
*       ELSEIF dis-totalpic IS INITIAL AND dis-totalnic IS NOT INITIAL.
*         wa_final-char_bal_mbg_cal = wa_final-char_bal_mbg_cal + dis-to talnic.
*       ELSE.
*         wa_final-char_bal_mbg_cal = ''.
*       ENDIF.
*
*       CLEAR:dis.
*    ENDLOOP.

    READ TABLE it_ycum INTO DATA(ycum) WITH KEY   yy_contract    =    yrvt-vb eln.
     IF sy-subrc = 0.
       wa_final-cum_bal_mbg_cal_so   =   ycum-yy_oij_cumimb.
*       wa_final-sal_order            =   ycum-yy_contract.        "   by   MANMOHAN/SHREYOSI Charm:4000008447 Date:09/07/2024
       IF lv_abrvw-abrvw = '' AND wa_final-m_cont_id IS NOT INITIAL. "*- >SOC CHARM ID :4000008814    TECHICAL : RAVINDER SINGH FUNCTIONAL : SHRE YOSI DT:11.11.2024
         wa_final-sal_order = ''. "*->SOC CHARM ID :4000008814    TECHICA L : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:11.11.2024
       ELSE.
        wa_final-sal_order            =   ycum-vbeln.     "++ by MANMOHA N/SHREYOSI Charm:4000008447 Date:09/07/2024
      ENDIF.
    ELSE.
      wa_final-indicator        = 'N'.
    ENDIF.

*     READ TABLE lt_vbfa INTO DATA(wa_vbfa) WITH KEY vbelv = ycum-vbeln." *->COC CHARM ID :4000008814    TECHICAL : RAVINDER SINGH FUNCTIONAL : SH REYOSI DT:11.11.2024
     READ TABLE lt_vbfa INTO DATA(wa_vbfa) WITH KEY vbelv = wa_final-sal_ order. "*->SOC CHARM ID :4000008814      TECHICAL : RAVINDER SINGH FUNCTI ONAL : SHREYOSI DT:11.11.2024
     IF sy-subrc EQ 0 AND ( wa_vbfa-vbtyp_n = 'M' OR wa_vbfa-vbtyp_n = 'P ' ).
       wa_final-invoice = wa_vbfa-vbeln.
     ENDIF.

*     >   SOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH FUNCTION AL : SHREYOSI DT:28.10.2024
    IF wa_final-sal_order IS NOT INITIAL.
      READ TABLE it_zcon INTO DATA(wzcon) WITH KEY yyvbeln = wa_final-s al_order yyclause_id = 10 ."BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-char_bal_mbg_cal_so =    wzcon-yyzmeng.
      ENDIF.

      READ TABLE it_zcon INTO DATA(wzcon1) WITH KEY yyvbeln = wa_final-s al_order yyclause_id = 11 ."BINARY SEARCH. "*->SOC CHARM ID :4000008814    TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:11.11.2024
      IF sy-subrc = 0.
        wa_final-neg_bal_mbg_cal_so = wzcon1-yyzmeng.
      ENDIF.
    ENDIF.
*     >   EOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH FUNCTION AL : SHREYOSI DT:28.10.2024
    READ TABLE it_adrc1 INTO wa_adrc1 WITH KEY locid = wa_final-blocati on.
    IF sy-subrc = 0.
      READ TABLE it_yrga_oic_region INTO DATA(oicr) WITH KEY region = wa_adrc1-region.
      IF sy-subrc = 0.
        READ TABLE it_pa0105 INTO DATA(wa_pa0105) WITH KEY pernr = oicr- oic_pernr.
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
*     >        "*SOC CHARM ID :4000009078   TECHICAL : RAVINDER SINGH F UNCTIONAL : SHREYOSI DT:02.01.2025
  IF s_payer IS NOT INITIAL AND s_vkbur IS NOT INITIAL .
    SORT it_final BY blocation DESCENDING customer DESCENDING payer DESC ENDING.
    DELETE it_final WHERE payer NOT IN s_payer OR sal_office NOT IN s_vk bur .
  ENDIF.
  IF s_payer IS NOT INITIAL .
    SORT it_final BY blocation DESCENDING customer DESCENDING payer DESC ENDING.
    DELETE it_final WHERE payer NOT IN s_payer .
  ENDIF.
  IF s_vkbur IS NOT INITIAL.
    SORT it_final BY blocation DESCENDING customer DESCENDING payer DESC ENDING.
    DELETE it_final[] WHERE sal_office NOT IN s_vkbur.
  ENDIF.
*     >        "*EOC CHARM ID :4000009078   TECHICAL : RAVINDER SINGH F UNCTIONAL : SHREYOSI DT:02.01.2025

ENDFORM.
*SOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala nce TR:DVRK9A1POQ
FORM customers_with_imbalance.
  DATA: wa_final_rec      TYPE ty_final,
         lt_cust_autopost TYPE STANDARD TABLE OF ty_cust,
         ls_cust_autopost TYPE ty_cust,
         lv_cust_found    TYPE abap_bool.

  IF  it_final IS INITIAL .
    MESSAGE: 'no data available' TYPE 'E' DISPLAY LIKE 'E'.
  ENDIF.

  LOOP AT it_final INTO wa_final_rec.
    IF wa_final_rec-indicator = 'N'.
       lv_cust_found = 'x'.
    ELSEIF wa_final_rec-cum_bal_mbg_cal NE wa_final_rec-cum_bal_mbg_cal_ so.
       lv_cust_found = 'x'.
    ELSEIF    wa_final_rec-char_bal_mbg_cal NE wa_final_rec-char_bal_mbg_ cal_so.
       lv_cust_found = 'x'.
    ELSEIF    wa_final_rec-neg_bal_mbg_cal NE wa_final_rec-neg_bal_mbg_c al_so.
       lv_cust_found = 'x'.
    ELSEIF ( ( wa_final_rec-char_bal_mbg_cal NE 0 ) OR
               ( wa_final_rec-neg_bal_mbg_cal NE 0 ) ) AND ( wa_final_rec-sal_order IS INITIAL ).
       lv_cust_found = 'x'.
    ENDIF.
    IF lv_cust_found = 'x'.
       READ TABLE lt_cust_autopost TRANSPORTING NO FIELDS
       WITH KEY kunnr = wa_final_rec-customer.

      IF sy-subrc NE 0.
        ls_cust_autopost-kunnr = wa_final_rec-customer.
        APPEND ls_cust_autopost TO lt_cust_autopost.
      ENDIF.
    ENDIF.
    CLEAR: lv_cust_found,wa_final_rec,ls_cust_autopost.
  ENDLOOP.

*SOC - Replace SUBMIT with local fetch + post
  LOOP AT lt_cust_autopost INTO ls_cust_autopost.
    REFRESH: it_display1[], gt_display_d[].
    PERFORM fetch_imbalance_data USING ls_cust_autopost-kunnr.
    PERFORM post_imbalance_data.
    CLEAR ls_cust_autopost.
  ENDLOOP.
*EOC - Replace SUBMIT with local fetch + post
ENDFORM.
*EOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala nce TR:DVRK9A1POQ
FORM e03_eventtab_build USING e03_lt_events TYPE slis_t_event.

  DATA: ls_event TYPE slis_alv_event.

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events    = e03_lt_events.
  READ TABLE e03_lt_events WITH KEY name = slis_ev_top_of_page
                            INTO ls_event.
  IF sy-subrc = 0.
    MOVE gc_formname_top_of_page TO ls_event-form.
    APPEND ls_event TO e03_lt_events.
  ENDIF.

ENDFORM.

*&                                                                     *
*&      Form TOP_OF_PAGE
*&                                                                     *
*       ALV Header
*                                                                      *
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

  DATA: ls_line TYPE slis_listheader,
        l_date_l TYPE char10,
        l_date_2 TYPE char10.

  CLEAR: l_date_l, l_date_2, ls_line.

  ls_line-typ = 'H'.     "'H' TLGASDAY
  CONCATENATE s_date-low+6(2) '.' s_date-low+4(2) '.' s_date-low+0(4) IN TO l_date_l.
  CONCATENATE s_date-high+6(2) '.' s_date-high+4(2) '.' s_date-high+0(4)  INTO l_date_2.
  CONCATENATE 'Report for Date Range:' l_date_l 'to' l_date_2 INTO ls_l ine-info SEPARATED BY space ..
  APPEND ls_line TO lt_top_of_page.
  CLEAR:ls_line.
* ls_line-typ = 'S'.
* ls_line-info = 'Note: N means no posting done through Summary for Ove rrun'.
* APPEND ls_line TO lt_top_of_page.
* CLEAR:ls_line.
  ls_line-typ = 'S'.
  ls_line-info = 'Note: N means no posting done through Summary for Imba lance'.
  APPEND ls_line TO lt_top_of_page.
  CLEAR:ls_line.

  ls_line-typ = 'S'.
  ls_line-info = 'All Values are in MBG'.
  APPEND ls_line TO lt_top_of_page.
  CLEAR:ls_line.



ENDFORM.
*&                                                                     *
*&       Form   GET_FIELDCAT
*&                                                                     *
*        text
*                                                                      *
*    > p1         text
* <     p2        text
*                                                            *
FORM get_fieldcat.
  CLEAR:wa_fcat.
  REFRESH:it_fcat[].
  DATA: sno TYPE char2.
  CLEAR:sno.
*
* IF it_final IS NOT INITIAL.
  CLEAR wa_fcat.
  wa_fcat-col_pos    = sno + 1.
  wa_fcat-fieldname = 'BLOCATION'.
  wa_fcat-tabname    = 'IT_FINAL'.
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

*     >        SOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH     FUN CTIONAL : SHREYOSI DT:28.10.2024
  IF ch1 EQ 'X'.
    CLEAR wa_fcat.
    wa_fcat-col_pos   = sno + 1.
    wa_fcat-fieldname = 'CUM_MST_IMB'.
    wa_fcat-tabname   = 'IT_FINAL'.
    wa_fcat-seltext_l = 'Cumulative Master Imb (Calculated)'.
    wa_fcat-outputlen = 25.
    APPEND wa_fcat TO it_fcat.
*     >        EOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH     FUN CTIONAL : SHREYOSI DT:28.10.2024

*      IF ch1 EQ 'X'.
      CLEAR wa_fcat.
      wa_fcat-col_pos   = sno + 1.
      wa_fcat-fieldname = 'CUM_BAL_MBG_CAL'.
      wa_fcat-tabname   = 'IT_FINAL'.
*      wa_fcat-seltext_l = 'Cumulative Imb in MBG (calculated)'.
      wa_fcat-seltext_l = 'Cumulative Imb (calculated)'.
      wa_fcat-outputlen = 25.
      APPEND wa_fcat TO it_fcat.

     CLEAR wa_fcat.
     wa_fcat-col_pos   = sno + 1.
     wa_fcat-fieldname = 'CHAR_BAL_MBG_CAL'.
     wa_fcat-tabname   = 'IT_FINAL'.
     wa_fcat-seltext_l = 'Positive Chg Imb (calculated)'. "*    >   SOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH FUNCTIONAL : SHRE YOSI DT:22.10.2024
*     wa_fcat-seltext_l = 'Chargeable Imb in MBG (calculated)'.
    wa_fcat-outputlen = 25.
    APPEND wa_fcat TO it_fcat.
*     >        SOC CHARM ID :4000008814    TECHICAL : RAVINDER SINGH     FUN CTIONAL : SHREYOSI DT:22.10.2024
    CLEAR wa_fcat.
    wa_fcat-col_pos   = sno + 1.
    wa_fcat-fieldname = 'NEG_BAL_MBG_CAL'.
    wa_fcat-tabname   = 'IT_FINAL'.
    wa_fcat-seltext_l = ' Negative Chg Imb (calculated)'.
    wa_fcat-outputlen = 25.
    APPEND wa_fcat TO it_fcat.
*     >        EOC CHARM ID :4000008814    TECHICAL : RAVINDER SINGH     FUN CTIONAL : SHREYOSI DT:22.10.2024
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
*    wa_fcat-seltext_l = 'Cumulative Imb in MBG (posted in SO)'.
  wa_fcat-outputlen = 25.
  APPEND wa_fcat TO it_fcat.

  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'CHAR_BAL_MBG_CAL_SO'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Positive Chg Imb (posted in SO)'. "*      >   SOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH FUNCTIONAL : SHR EYOSI DT:22.10.2024
*    wa_fcat-seltext_l = 'Chargeable Imb in MBG (posted in SO)'.
  wa_fcat-outputlen = 25.
  APPEND wa_fcat TO it_fcat.

*       >        SOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH    FUN CTIONAL : SHREYOSI DT:22.10.2024
  CLEAR wa_fcat.
  wa_fcat-col_pos   = sno + 1.
  wa_fcat-fieldname = 'NEG_BAL_MBG_CAL_SO'.
  wa_fcat-tabname   = 'IT_FINAL'.
  wa_fcat-seltext_l = 'Negative Chg Imb (posted in SO)'.
  wa_fcat-outputlen = 25.
  APPEND wa_fcat TO it_fcat.
*     >        EOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH FUN CTIONAL : SHREYOSI DT:22.10.2024
  IF dnpi_flag = 'X'. "*      >        SOC CHARM ID :4000008814   TECHIC AL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:29.10.2024
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
*    ENDIF.

ENDFORM.
*&                                                                     *
*&       Form ALV_DISPLAY
*&                                                                     *
*        text
*                                                                      *
*    > p1         text
* <     p2        text
*                                                                      *
FORM alv_display.

  w_layout-colwidth_optimize = 'X'.
*SOC BY MANMOHAN/SHREYOSI CHARM: 2000000816 DATE:16.07.2024

  it_final_im[] = it_final[].
  IF sy-tcode EQ 'YRGR104' OR sy-cprog EQ 'YRGR_032_GMS_CT'.
    EXPORT it_final_im TO MEMORY ID 'IM_DATA'.
  ENDIF.
*EOC BY MANMOHAN/SHREYOSI CHARM: 2000000816 DATE:16.07.2024
* IF it_final IS NOT INITIAL.
  DATA: v_repid1 TYPE sy-repid.
  v_repid1 = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
*     I_INTERFACE_CHECK = ' '
*     I_BYPASSING_BUFFER = ' '
*     I_BUFFER_ACTIVE     = ' '
      i_callback_program = v_repid1
*     I_CALLBACK_PF_STATUS_SET           = ' '
*     I_CALLBACK_USER_COMMAND            = ' '
*     I_CALLBACK_TOP_OF_PAGE             = ' '
*     I_CALLBACK_HTML_TOP_OF_PAGE        = ' '
*     I_CALLBACK_HTML_END_OF_LIST        = ' '
*     I_STRUCTURE_NAME    =
*     I_BACKGROUND_ID     = ' '
*     I_GRID_TITLE        =
*     I_GRID_SETTINGS     =
      is_layout           = w_layout
      it_fieldcat         = it_fcat
*     IT_EXCLUDING        =
*     IT_SPECIAL_GROUPS =
*     IT_SORT             =
*     IT_FILTER           =
*     IS_SEL_HIDE         =
*     I_DEFAULT           = 'X'
      i_save              = 'X'
*     IS_VARIANT          =
      it_events           = gt_events[]
*     IT_EVENT_EXIT       =
*     IS_PRINT            =
*     IS_REPREP_ID        =
*     I_SCREEN_START_COLUMN              = 0
*     I_SCREEN_START_LINE                = 0
*     I_SCREEN_END_COLUMN                = 0
*     I_SCREEN_END_LINE = 0
*     I_HTML_HEIGHT_TOP = 0
*     I_HTML_HEIGHT_END = 0
*     IT_ALV_GRAPHICS     =
*     IT_HYPERLINK        =
*     IT_ADD_FIELDCAT     =
*     IT_EXCEPT_QINFO     =
*     IR_SALV_FULLSCREEN_ADAPTER         =
*    IMPORTING
*     E_EXIT_CAUSED_BY_CALLER            =
*     ES_EXIT_CAUSED_BY_USER             =
    TABLES
      t_outtab            = it_final[]
    EXCEPTIONS
      program_error       = 1
      OTHERS              = 2.
  .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.



*   ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form change_so_local
*& Replicated from YRG011 change_so for r_d5 path (augru always G20)
*&---------------------------------------------------------------------*
FORM change_so_local USING w_vbeln1.

  SELECT * FROM vbkd UP TO 1 ROWS
    WHERE vbeln = w_vbeln1
    AND posnr = '000010'
    ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF sy-subrc = 0.
    vbkd-bstdk = s_date-low.
    vbkd-bstdk_e = s_date-high.
    vbkd-fbuda = s_date-high.
    MODIFY vbkd.
    COMMIT WORK AND WAIT.
  ELSE.
    SELECT * FROM vbkd
      UP TO 1 ROWS
      WHERE vbeln = w_vbeln1
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc = 0.
      vbkd-bstdk = s_date-low.
      vbkd-bstdk_e = s_date-high.
      vbkd-fbuda = s_date-high.
      MODIFY vbkd.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  SELECT SINGLE * FROM vbak
    WHERE vbeln = w_vbeln1.
  IF sy-subrc = 0.
    vbak-augru = 'G20'.
    MODIFY vbak.
    COMMIT WORK AND WAIT.
  ENDIF.

  CALL FUNCTION 'YG_CHANGE_CONFIG'
    EXPORTING
      p_vbeln = w_vbeln1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form additional_qty_local
*& Replicated from YRG011 additional_qty for r_d5 path
*&---------------------------------------------------------------------*
FORM additional_qty_local USING p_wa_display1 TYPE ty_display1.
  DATA l_sr TYPE ysrno.
  DATA: lt_yrva_zcontrcls TYPE TABLE OF yrva_zcontrcls,
        lw_yrva_zcontrcls TYPE yrva_zcontrcls,
        lv_vbeln          TYPE vbeln.

  IF p_wa_display1-vbeln1 IS NOT INITIAL.
    lv_vbeln = p_wa_display1-vbeln1.
  ELSE.
    lv_vbeln = g_ls_oikexport_d-kdauf.
  ENDIF.
  l_sr = 1.

  CLEAR: g_pos_imb_charg_d, g_neg_imb_charg_d.
  READ TABLE gt_978_d WITH KEY oicontnr = gw_vbak1_d-vbeln.
  IF sy-subrc EQ 0.
    READ TABLE gt_konp_d INTO gw_konp_d WITH KEY knumh = gt_978_d-knumh.
  ELSE.
    READ TABLE gt_a305_d WITH KEY kunnr = wa_display1-kunnr matnr = wa_display1-matnr.
    IF sy-subrc EQ 0.
      READ TABLE gt_konp_d INTO gw_konp_d WITH KEY knumh = gt_a305_d-knumh.
    ENDIF.
  ENDIF.

  IF gw_konp_d-kmein EQ 'SM3'.
    g_pos_imb_charg_d = p_wa_display1-totalpic1.
    g_neg_imb_charg_d = p_wa_display1-totalnic1.
    g_dds_imb_charg_d = p_wa_display1-totaldds1.
    g_price_uom_d = 'SM3'.
  ELSE.
    g_pos_imb_charg_d = p_wa_display1-totalpic.
    g_neg_imb_charg_d = p_wa_display1-totalnic.
    g_dds_imb_charg_d = p_wa_display1-totaldds.
    g_price_uom_d = g_ls_oikload_d-vrkme.
  ENDIF.

  IF g_price_uom_d IS INITIAL.
    READ TABLE gt_oij_el_cp_layt_d INTO gw_oij_el_cp_layt_d WITH KEY vbeln = wa_display1-vbeln.
    IF sy-subrc = 0.
      g_price_uom_d = gw_oij_el_cp_layt_d-mdq_uom.
    ENDIF.
  ENDIF.

  IF lv_vbeln IS NOT INITIAL.
    DELETE FROM yrva_zcontrcls WHERE yyvbeln = lv_vbeln.
  ENDIF.

* r_d5 is always X in this context - insert clause 10 and 11
  IF p_wa_display1-totalpic <> 0.
    lw_yrva_zcontrcls-yyvbeln = lv_vbeln.
    lw_yrva_zcontrcls-yyposnr = '000010'.
    lw_yrva_zcontrcls-yysrno = l_sr.
    lw_yrva_zcontrcls-yyclause_id = '10'.
    lw_yrva_zcontrcls-yyzmeng = g_pos_imb_charg_d.
    lw_yrva_zcontrcls-yyzieme = g_price_uom_d.
    APPEND lw_yrva_zcontrcls TO lt_yrva_zcontrcls.
    MODIFY yrva_zcontrcls FROM lw_yrva_zcontrcls.
    CLEAR: lw_yrva_zcontrcls.
  ENDIF.

  IF p_wa_display1-totalnic <> 0.
    lw_yrva_zcontrcls-yyvbeln = lv_vbeln.
    lw_yrva_zcontrcls-yyposnr = '000010'.
    lw_yrva_zcontrcls-yysrno = l_sr.
    lw_yrva_zcontrcls-yyclause_id = '11'.
    lw_yrva_zcontrcls-yyzmeng = g_neg_imb_charg_d * -1.
    lw_yrva_zcontrcls-yyzieme = g_price_uom_d.
    APPEND lw_yrva_zcontrcls TO lt_yrva_zcontrcls.
    MODIFY yrva_zcontrcls FROM lw_yrva_zcontrcls.
    CLEAR: lw_yrva_zcontrcls.
    l_sr = l_sr + 1.
  ENDIF.

* r_dds always initial - skip DDS clause 15
* r_d2 always initial - skip overrun clause 12

  COMMIT WORK AND WAIT.
  REFRESH: lt_yrva_zcontrcls.
  CLEAR lw_yrva_zcontrcls.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form entry_imb_table_local
*& Replicated from YRG011 entry_imb_table for r_d5 path
*&---------------------------------------------------------------------*
FORM entry_imb_table_local USING p_wa_display1 TYPE ty_display1.

  CLEAR gw_yrg_cumm_imb_d.
  SELECT * FROM yrg_cumm_imb UP TO 1 ROWS
    INTO gw_yrg_cumm_imb_d
    WHERE knkli = p_wa_display1-knkli
      AND kunnr = p_wa_display1-kunnr
      AND yy_contract = p_wa_display1-vbeln
      AND begda = s_date-low
      AND endda = s_date-high
    ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF sy-subrc NE 0.
    gw_yrg_cumm_imb_d-knkli = p_wa_display1-knkli.
    gw_yrg_cumm_imb_d-kunnr = p_wa_display1-kunnr.
    gw_yrg_cumm_imb_d-yy_contract = p_wa_display1-vbeln.
    gw_yrg_cumm_imb_d-begda = s_date-low.
    gw_yrg_cumm_imb_d-endda = s_date-high.
    CALL FUNCTION 'PK_DATE_TIME_INTO_TIMESTAMP'
      EXPORTING
        iv_pkldt = sy-datum
        iv_pkluz = sy-uzeit
        is_t001w = g_is_t001w_d
      IMPORTING
        ev_pktim = gw_yrg_cumm_imb_d-timestamp.
  ENDIF.
  gw_yrg_cumm_imb_d-vbeln = p_wa_display1-vbeln1.
  gw_yrg_cumm_imb_d-yy_oij_cumimb = p_wa_display1-totalcumi.
  gw_yrg_cumm_imb_d-ldatum = sy-datum.
  gw_yrg_cumm_imb_d-ernam = sy-uname.
  gw_yrg_cumm_imb_d-augru = p_wa_display1-augru.
* r_dds always initial - always MODIFY yrg_cumm_imb (not yrg_cumm_dds)
  MODIFY yrg_cumm_imb FROM gw_yrg_cumm_imb_d.
  COMMIT WORK AND WAIT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form fetch_imbalance_data
*& Replicated from YRG011 START-OF-SELECTION for r_d5 path
*& Fetches imbalance data for a single customer, populates it_display1
*&---------------------------------------------------------------------*
FORM fetch_imbalance_data USING p_kunnr TYPE kunnr.

* Local variables
  DATA: lv_tabix_loc     TYPE sy-tabix,
        lv_s_vbeln       TYPE vbeln_va,
        gv_vbdat_loc     TYPE vbdat_veda,
        gv_vndat_loc     TYPE vndat_veda,
        fs_vbak_loc      TYPE ty_vbak_d,
        nomtk_loc        TYPE oijnomi-nomtk,
        w_line_loc       TYPE i,
        w_line1_loc      TYPE i,
        w_ncv_loc        TYPE yyncv,
        w_gcv_loc        TYPE yyncv,
        w_tot_redel_vol_loc    TYPE p DECIMALS 3,
        w_tot_redel_energy_loc TYPE p DECIMALS 3,
        lv_checked_loc   TYPE c,
        l_tabix_loc      TYPE sy-tabix.

  DATA: lt_gta_imb_loc        TYPE TABLE OF yrva_gta_imb_sft,
        ls_gta_imb_loc        TYPE yrva_gta_imb_sft,
        lt_cumm_imb_temp_gta  TYPE TABLE OF yrg_cumm_imb,
        ls_cumm_imb_temp_gta  TYPE yrg_cumm_imb,
        lt_vbak_zgk_loc       TYPE TABLE OF ty_vbak_d,
        lt_parent_1_loc       TYPE TABLE OF veda,
        ls_parent_loc         TYPE veda,
        lt_rv_del_loc         TYPE STANDARD TABLE OF yrv_ep_nm_del,
        ls_rv_del_loc         TYPE yrv_ep_nm_del.

* Clear all global working tables
  REFRESH: gt_vbak_d, gt_vbak1_d, gt_vbak2_d, gt_vbak3_d, gt_vbak4_d,
           gt_vbak_temp_d, gt_veda_d, gt_oijnomi_d, gt_oij_d,
           gt_yrg_cumm_imb_d, gt_yrg_cumm_imb1_d, gt_yrg_cumm_imb_temp_d,
           gt_yrg_chg_ovrrun_d, gt_display_d, gt_disp_temp_d,
           gt_display_temp_d, gt_display3_d,
           gt_yrvt_contract_d, gt_oij_el_cp_layt_d,
           gt_yro_nom_param_d, gt_yro_nom_param1_d,
           gt_gta_imb_sft_d, gt_header_d,
           gt_lt_yrvt_cont_dcq_d,
           g_it_display_d, g_it_display11_d,
           g_it_display_temp_d, g_it_display3_loc_d,
           g_it_disp_temp_d, g_it_yrg_cumm_imb2_d,
           g_it_vbak_del_loc_d, g_lv_log_d,
           lt_gta_imb_loc, lt_cumm_imb_temp_gta,
           lt_vbak_zgk_loc, lt_parent_1_loc, lt_rv_del_loc.

  CLEAR: gw_vbak_d, gw_vbak1_d, gw_vbak3_d, gw_vbak4_d,
         gw_veda_d, gw_oijnomi_d, gw_oijnomi1_d,
         gw_yrg_cumm_imb_d, gw_yrg_cumm_imb1_d,
         gw_yrg_chg_ovrrun_d,
         gw_display_d, gw_display_temp_d,
         gw_yrvt_contract_d, gw_yrvt_contract1_d,
         gw_oij_el_cp_layt_d, gw_yro_nom_param_d,
         gw_yro_nom_param1_d, gw_header_d,
         gw_ls_yrvt_cont_dcq_d, gw_display12_d, gw_display11_d,
         g_wa_display_loc_d, g_wa_display_temp_loc_d,
         g_wv_log_d, g_daily_d, g_cumm_bal_d,
         g_daily1_d, g_daily2_d, g_mdq_quan_d,
         g_refvalue_d, g_adq_d, g_menge_d,
         g_wt_d, g_wt1_d, g_w_master_contract_d,
         g_w_group_imbalance_d, g_exit_processing_d,
         g_lv_clause_not_found_d, g_lv_pre_check_lines_d,
         g_lv_index_d, g_lv_type_d, g_lv_15_d, g_lv_11_d,
         g_lv_10_d, g_lv_12_d, g_process_flag_d,
         g_subrc_d, lv_s_vbeln, wa_display1.

*----------------------------------------------------------------------*
* Section 1: Contract Selection (YRG011:600-604)
*----------------------------------------------------------------------*
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_vbak_d
    FROM vbak
    WHERE kunnr = p_kunnr
    AND ( auart = 'ZGT1' ).

  CHECK gt_vbak_d[] IS NOT INITIAL.

*----------------------------------------------------------------------*
* Section 2: Validity period check and contract merging (YRG011:617-682)
*----------------------------------------------------------------------*
  REFRESH: gt_vbak1_d, gt_vbak2_d.

  SELECT * FROM veda INTO CORRESPONDING FIELDS OF TABLE gt_veda_d
    FOR ALL ENTRIES IN gt_vbak_d
    WHERE vbeln = gt_vbak_d-vbeln_grp
    AND vposn = '00000'
    AND vbegdat LE s_date-low
    AND venddat GE s_date-high.

  IF gt_veda_d[] IS NOT INITIAL.
    SELECT * FROM vbak
      INTO CORRESPONDING FIELDS OF TABLE gt_vbak3_d
      FOR ALL ENTRIES IN gt_veda_d
      WHERE vbeln = gt_veda_d-vbeln.
  ENDIF.

* r_d2 IS INITIAL - merge contracts from vbak3 into vbak
  LOOP AT gt_vbak3_d INTO gw_vbak3_d.
    LOOP AT gt_vbak_d INTO gw_vbak_d WHERE vbeln_grp = gw_vbak3_d-vbeln.
      lv_tabix_loc = sy-tabix.
      IF gw_vbak3_d-kunnr <> gw_vbak_d-kunnr.
        DELETE gt_vbak_d INDEX lv_tabix_loc.
      ELSE.
        MOVE-CORRESPONDING gw_vbak3_d TO gw_vbak_d.
        APPEND gw_vbak_d TO gt_vbak_d.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

* Check for ZGK contracts and append related
  READ TABLE gt_vbak_d WITH KEY auart = 'ZGK' TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    SELECT * FROM vbak
      APPENDING CORRESPONDING FIELDS OF TABLE gt_vbak_d
      FOR ALL ENTRIES IN gt_vbak_d
      WHERE vbeln_grp = gt_vbak_d-vbeln_grp
      AND vbeln_grp <> space.
  ENDIF.

  MOVE gt_vbak_d[] TO gt_vbak2_d[].
  SORT gt_vbak2_d BY vbeln_grp.

* s_vbeln is always INITIAL in YRG102 context - skip master contract filter block

  REFRESH: gt_vbak1_d, gt_vbak2_d.
  CLEAR g_menge_d.

*----------------------------------------------------------------------*
* Section 3: Contract validation loop (YRG011:752-817)
*----------------------------------------------------------------------*
  CLEAR g_lv_pre_check_lines_d.
  DESCRIBE TABLE gt_vbak_d LINES g_lv_pre_check_lines_d.

  LOOP AT gt_vbak_d INTO gw_vbak_d.
    g_lv_index_d = sy-tabix.
*   overrun IS INITIAL AND r_d1 IS INITIAL -> always true
    CLEAR g_lv_type_d.
*   r_d5 EQ 'X' -> lv_type = 'I'
    g_lv_type_d = 'I'.
    CLEAR g_exit_processing_d.
*   Validate imbalance clauses - keep original conditions
    IF NOT sy-batch EQ 'X'
       OR NOT sy-tcode EQ 'YRGR102'.
      CALL METHOD ycl_gms_report_validations=>validate_imbalance
        EXPORTING
          i_vbeln           = gw_vbak_d-vbeln
          i_valid_from      = s_date-low
          i_valid_to        = s_date-high
          i_type            = g_lv_type_d
        IMPORTING
          e_exit_processing = g_exit_processing_d
          e_not_found       = g_lv_clause_not_found_d.
      IF g_exit_processing_d = 'X'.
*       Replace LEAVE LIST-PROCESSING with RETURN
        RETURN.
      ELSEIF g_lv_clause_not_found_d = 'X'.
        DELETE gt_vbak_d INDEX g_lv_index_d.
        CONTINUE.
      ENDIF.
    ENDIF.

*   Build gt_vbak1_d - contracts with same vbeln_grp
    LOOP AT gt_vbak_d INTO gw_vbak1_d WHERE vbeln_grp = gw_vbak_d-vbeln_grp AND vbeln_grp IS NOT INITIAL.
      APPEND gw_vbak1_d TO gt_vbak1_d.
    ENDLOOP.

    READ TABLE gt_vbak1_d INTO gw_vbak1_d WITH KEY vbeln = gw_vbak_d-vbeln.
    IF sy-subrc <> 0.
      APPEND gw_vbak_d TO gt_vbak2_d.
    ENDIF.
  ENDLOOP.

* Check if all contracts removed
  IF gt_vbak_d[] IS INITIAL AND g_lv_pre_check_lines_d IS NOT INITIAL.
*   In YRGR102 context - message suppressed, just return
    RETURN.
  ENDIF.

  SORT gt_vbak1_d BY vbeln.
  DELETE ADJACENT DUPLICATES FROM gt_vbak1_d COMPARING vbeln.

*----------------------------------------------------------------------*
* Section 4: Additional contract validity check (YRG011:843-883)
*----------------------------------------------------------------------*
  LOOP AT gt_vbak_d INTO gw_vbak_d WHERE auart = 'ZGT1'.
    CALL FUNCTION 'YRV_CONTRACT_DETAILS'
      EXPORTING
        vbeln   = gw_vbak_d-vbeln
      IMPORTING
        vbegdat = gv_vbdat_loc
        venddat = gv_vndat_loc.
    IF gv_vbdat_loc LE s_date-high AND gv_vndat_loc GE s_date-low.
      MOVE-CORRESPONDING gw_vbak_d TO fs_vbak_loc.
      APPEND fs_vbak_loc TO gt_vbak_temp_d.
    ELSE.
      DELETE gt_vbak_d WHERE vbeln = gw_vbak_d-vbeln.
    ENDIF.
  ENDLOOP.

  DELETE gt_vbak_temp_d WHERE auart EQ 'ZGK'.

  IF gt_vbak_temp_d[] IS INITIAL.
*   In YRGR102 context - return with empty it_display1
    RETURN.
  ENDIF.

*----------------------------------------------------------------------*
* Section 5: Nomination fetch (YRG011:885-948)
*----------------------------------------------------------------------*
  SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_oijnomi_d
    FROM oijnomi
    FOR ALL ENTRIES IN gt_vbak_temp_d
    WHERE docnr = gt_vbak_temp_d-vbeln
    AND idate IN s_date
    AND delind <> 'X'.

* GTA WITHOUT PO
  IF gt_oijnomi_d[] IS NOT INITIAL.
*   lt_vbak_zgk_loc from gt_vbak4_d - empty since s_vbeln is initial
    SELECT *
      FROM yrva_gta_imb_sft
      INTO TABLE lt_gta_imb_loc
      FOR ALL ENTRIES IN gt_oijnomi_d
      WHERE contract_to EQ gt_oijnomi_d-docnr.

    SORT lt_gta_imb_loc BY contract_to.
    IF lt_gta_imb_loc[] IS NOT INITIAL.
      SELECT *
        FROM yrg_cumm_imb
        INTO TABLE lt_cumm_imb_temp_gta
        FOR ALL ENTRIES IN lt_gta_imb_loc
        WHERE yy_contract EQ lt_gta_imb_loc-contract_from
        AND endda LT s_date-low.
      IF sy-subrc IS INITIAL.
        SORT lt_cumm_imb_temp_gta BY timestamp DESCENDING.
      ENDIF.
    ENDIF.

*   GTA nominations (ZO/K)
    SELECT * FROM oijnomi INTO CORRESPONDING FIELDS OF TABLE gt_oij_d
      FOR ALL ENTRIES IN gt_oijnomi_d
      WHERE nomtk = gt_oijnomi_d-nomtk
      AND sityp = 'ZO'
      AND docind = 'K'
      AND delind NE 'X'.
  ENDIF.

  IF gt_oij_d IS NOT INITIAL.
    APPEND LINES OF gt_oij_d TO gt_oijnomi_d.
  ENDIF.

*----------------------------------------------------------------------*
* Section 6: Cumulative imbalance history (YRG011:952-1039)
*----------------------------------------------------------------------*
  REFRESH gt_vbak_temp_d.
  IF sy-subrc = 0.
    SORT gt_oijnomi_d BY idate.
*   r_dds is INITIAL - skip yrg_cumm_dds select
    SELECT * APPENDING TABLE gt_yrg_cumm_imb_d
      FROM yrg_cumm_imb
      FOR ALL ENTRIES IN gt_vbak_d
      WHERE yy_contract = gt_vbak_d-vbeln
      AND endda < s_date-low.
    IF sy-subrc = 0.
      SORT gt_yrg_cumm_imb_d BY timestamp ASCENDING.
      DESCRIBE TABLE gt_yrg_cumm_imb_d LINES w_line_loc.
      w_line1_loc = w_line_loc.
      DO w_line_loc TIMES.
        READ TABLE gt_yrg_cumm_imb_d INTO gw_yrg_cumm_imb1_d INDEX w_line1_loc.
        IF sy-subrc = 0.
          APPEND gw_yrg_cumm_imb1_d TO gt_yrg_cumm_imb_temp_d.
        ENDIF.
        w_line1_loc = w_line1_loc - 1.
      ENDDO.
      REFRESH gt_yrg_cumm_imb_d.
      MOVE gt_yrg_cumm_imb_temp_d[] TO gt_yrg_cumm_imb_d[].
      DELETE ADJACENT DUPLICATES FROM gt_yrg_cumm_imb_d COMPARING kunnr yy_contract.
    ENDIF.

*   Current period cumulative (r_dds INITIAL -> yrg_cumm_imb)
    SELECT * INTO TABLE gt_yrg_cumm_imb1_d
      FROM yrg_cumm_imb
      FOR ALL ENTRIES IN gt_vbak_d
      WHERE yy_contract = gt_vbak_d-vbeln
      AND begda = s_date-low
      AND endda = s_date-high.

    SORT gt_yrg_cumm_imb1_d BY timestamp ldatum endda DESCENDING.
    DELETE ADJACENT DUPLICATES FROM gt_yrg_cumm_imb1_d COMPARING kunnr yy_contract.
    IF gt_yrg_cumm_imb1_d[] IS NOT INITIAL.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE g_it_vbak_del_loc_d
        FROM vbak
        FOR ALL ENTRIES IN gt_yrg_cumm_imb1_d
        WHERE vbeln = gt_yrg_cumm_imb1_d-vbeln.
      LOOP AT gt_yrg_cumm_imb1_d INTO gw_yrg_cumm_imb_d.
        l_tabix_loc = sy-tabix.
        READ TABLE g_it_vbak_del_loc_d INTO gw_vbak_d WITH KEY vbeln = gw_yrg_cumm_imb_d-vbeln.
        IF sy-subrc <> 0.
          CLEAR gw_yrg_cumm_imb_d-vbeln.
          MODIFY gt_yrg_cumm_imb1_d FROM gw_yrg_cumm_imb_d INDEX l_tabix_loc.
        ENDIF.
      ENDLOOP.
    ENDIF.

*----------------------------------------------------------------------*
* Section 7: Overrun charges and contract clauses (YRG011:1042-1139)
*----------------------------------------------------------------------*
    SELECT * FROM yrg_chg_ovrrun INTO TABLE gt_yrg_chg_ovrrun_d
      FOR ALL ENTRIES IN gt_vbak_d
      WHERE vbeln = gt_vbak_d-vbeln
      AND begda = s_date-low AND endda EQ s_date-high.

    IF gt_oijnomi_d[] IS NOT INITIAL.
      SELECT * INTO TABLE gt_yrvt_contract_d
        FROM yrvt_contract
        FOR ALL ENTRIES IN gt_oijnomi_d
        WHERE vbeln = gt_oijnomi_d-docnr
        AND clause IN ('10','11','12','15','16').
      IF sy-subrc = 0.
        SELECT * FROM yrvt_cont_dcq INTO CORRESPONDING FIELDS OF TABLE gt_lt_yrvt_cont_dcq_d
          FOR ALL ENTRIES IN gt_oijnomi_d WHERE vbeln = gt_oijnomi_d-docnr AND ydate = gt_oijnomi_d-idate.
        LOOP AT gt_lt_yrvt_cont_dcq_d INTO gw_ls_yrvt_cont_dcq_d.
          CONCATENATE gw_ls_yrvt_cont_dcq_d-creon gw_ls_yrvt_cont_dcq_d-time INTO gw_ls_yrvt_cont_dcq_d-timest.
          MODIFY gt_lt_yrvt_cont_dcq_d FROM gw_ls_yrvt_cont_dcq_d TRANSPORTING timest.
          CLEAR gw_ls_yrvt_cont_dcq_d.
        ENDLOOP.
        SORT gt_lt_yrvt_cont_dcq_d BY vbeln timest DESCENDING.
        SORT gt_yrvt_contract_d BY vbeln.
*       r_dds is INITIAL - skip cal_period checks
      ENDIF.
    ENDIF.

*----------------------------------------------------------------------*
* Section 8: Capacity layout and params (YRG011:1142-1167)
*----------------------------------------------------------------------*
    IF gt_oijnomi_d[] IS NOT INITIAL.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_oij_el_cp_layt_d
        FROM oij_el_cp_layt
        FOR ALL ENTRIES IN gt_oijnomi_d
        WHERE vbeln = gt_oijnomi_d-docnr.
      IF sy-subrc = 0.
        SORT gt_oij_el_cp_layt_d BY vbeln.
      ENDIF.

      SELECT * INTO TABLE gt_yro_nom_param_d
        FROM yro_nom_param
        FOR ALL ENTRIES IN gt_oijnomi_d
        WHERE nomtk = gt_oijnomi_d-nomtk
        AND nomit = gt_oijnomi_d-nomit.

      SELECT * INTO CORRESPONDING FIELDS OF TABLE gt_yro_nom_param1_d
        FROM yro_nom_param1
        FOR ALL ENTRIES IN gt_oijnomi_d
        WHERE nomtk = gt_oijnomi_d-nomtk
        AND nomit = gt_oijnomi_d-nomit.
    ENDIF.

*----------------------------------------------------------------------*
* Section 9: Reorder contracts for GTA (YRG011:1178-1249)
*----------------------------------------------------------------------*
    SORT gt_oijnomi_d BY idate.
    SORT gt_vbak_d[] BY vbeln_grp DESCENDING.

    IF lt_gta_imb_loc[] IS NOT INITIAL.
      SELECT * FROM veda
        INTO TABLE lt_parent_1_loc
        FOR ALL ENTRIES IN lt_gta_imb_loc
        WHERE vbeln EQ lt_gta_imb_loc-contract_from
        AND vposn EQ '000000'.
      IF sy-subrc IS INITIAL.
        SORT lt_parent_1_loc BY venddat.
      ENDIF.
    ENDIF.

*   Determine the latest master contract
    LOOP AT gt_vbak_d INTO gw_vbak_d.
      READ TABLE gt_oijnomi_d TRANSPORTING NO FIELDS WITH KEY docnr = gw_vbak_d-vbeln.
      IF sy-subrc IS INITIAL.
        g_w_master_contract_d = gw_vbak_d-vbeln_grp.
        EXIT.
      ENDIF.
    ENDLOOP.

*   Reorder contracts using parent veda dates
    l_tabix_loc = 1.
    LOOP AT lt_parent_1_loc INTO ls_parent_loc.
      READ TABLE gt_vbak_d INTO gw_vbak_d WITH KEY vbeln = ls_parent_loc-vbeln.
      IF sy-subrc IS INITIAL.
        gw_vbak1_d = gw_vbak_d.
        DELETE gt_vbak_d WHERE vbeln EQ gw_vbak_d-vbeln.
        INSERT gw_vbak1_d INTO gt_vbak_d INDEX l_tabix_loc.
        l_tabix_loc = l_tabix_loc + 1.
      ENDIF.
    ENDLOOP.
    CLEAR: l_tabix_loc, gw_vbak_d, gw_vbak1_d.

*----------------------------------------------------------------------*
* Section 10: Delivery point data (YRG011:1254-1267)
*----------------------------------------------------------------------*
    IF gt_vbak_d[] IS NOT INITIAL.
      SELECT * FROM yrv_ep_nm_del
        INTO CORRESPONDING FIELDS OF TABLE lt_rv_del_loc
        FOR ALL ENTRIES IN gt_vbak_d
        WHERE vbeln = gt_vbak_d-vbeln
        AND doc IN s_date.
      IF sy-subrc EQ 0.
        SORT lt_rv_del_loc BY kunnr vbeln.
      ENDIF.
    ENDIF.

*----------------------------------------------------------------------*
* Section 11: Main calculation loop (YRG011:1269-1896)
*----------------------------------------------------------------------*
    LOOP AT gt_vbak_d INTO gw_vbak_d.

      READ TABLE gt_yrg_cumm_imb_d INTO gw_yrg_cumm_imb_d WITH KEY yy_contract = gw_vbak_d-vbeln.
      IF sy-subrc = 0.
        g_daily_d = gw_yrg_cumm_imb_d-yy_oij_cumimb.
      ELSE.
*       If entry not in cumm_imb, check GTA parent
        CLEAR: g_l_cumm_bal_imb_d, gw_header_d.
        SORT g_it_disp_temp_d BY vbeln ASCENDING date DESCENDING.
        LOOP AT lt_gta_imb_loc INTO ls_gta_imb_loc WHERE contract_to = gw_vbak_d-vbeln.
          gw_header_d-vbeln = gw_vbak_d-vbeln.
          gw_header_d-p_cont = ls_gta_imb_loc-contract_from.
          READ TABLE g_it_disp_temp_d INTO g_wa_display_loc_d WITH KEY vbeln = ls_gta_imb_loc-contract_from.
          IF sy-subrc IS INITIAL.
            g_l_cumm_bal_imb_d = g_l_cumm_bal_imb_d + g_wa_display_loc_d-daily_cumm.
            gw_header_d-imbal = g_wa_display_loc_d-daily_cumm.
          ELSE.
            READ TABLE lt_cumm_imb_temp_gta INTO ls_cumm_imb_temp_gta WITH KEY yy_contract = ls_gta_imb_loc-contract_from.
            IF sy-subrc IS INITIAL.
              g_l_cumm_bal_imb_d = g_l_cumm_bal_imb_d + ls_cumm_imb_temp_gta-yy_oij_cumimb.
              gw_header_d-imbal = ls_cumm_imb_temp_gta-yy_oij_cumimb.
            ENDIF.
          ENDIF.
          APPEND gw_header_d TO gt_header_d.
          CLEAR: g_wa_display_loc_d, ls_cumm_imb_temp_gta, gw_header_d.
        ENDLOOP.
        g_daily_d = g_l_cumm_bal_imb_d.
        CLEAR ls_gta_imb_loc.
      ENDIF.

*     r_d2 IS INITIAL - skip overrun read
      READ TABLE gt_yrg_cumm_imb1_d INTO gw_yrg_cumm_imb1_d WITH KEY yy_contract = gw_vbak_d-vbeln.
      IF sy-subrc = 0.
        wa_display1-vbeln1 = gw_yrg_cumm_imb1_d-vbeln.
      ENDIF.

      CLEAR g_menge_d.
      CLEAR: nomtk_loc.
      LOOP AT gt_oijnomi_d INTO gw_oijnomi_d
        WHERE docnr = gw_vbak_d-vbeln.
        CLEAR g_wa_display_loc_d.

        wa_display1-matnr = gw_oijnomi_d-s_matnr_i.
        wa_display1-vbeln = gw_vbak_d-vbeln.
        wa_display1-ktext = gw_vbak_d-ktext.
        wa_display1-knkli = gw_vbak_d-knkli.

        g_wa_display_loc_d-vbeln_grp = gw_vbak_d-vbeln_grp.

*       GTA nomination lookup
        READ TABLE gt_oijnomi_d INTO gw_oijnomi1_d WITH KEY docnr = gw_vbak_d-vbeln idate = gw_oijnomi_d-idate.
        IF sy-subrc = 0.
          nomtk_loc = gw_oijnomi1_d-nomtk.
        ENDIF.
        CLEAR gw_oijnomi1_d.

        READ TABLE gt_oijnomi_d INTO gw_oijnomi1_d WITH KEY nomtk = nomtk_loc idate = gw_oijnomi_d-idate
                                                             sityp = 'ZO' docind = 'K'.

        CLEAR: nomtk_loc.
        IF sy-subrc = 0.
*         GCV & NCV
          READ TABLE gt_yro_nom_param1_d INTO gw_yro_nom_param1_d
            WITH KEY nomtk = gw_oijnomi1_d-nomtk
                     nomit = gw_oijnomi1_d-nomit.
          IF sy-subrc = 0.
            CALL FUNCTION 'ROUND'
              EXPORTING
                input         = gw_yro_nom_param1_d-agcv
              IMPORTING
                output        = g_wa_display_loc_d-agcv
              EXCEPTIONS
                input_invalid = 1
                overflow      = 2
                type_invalid  = 3.
            IF sy-subrc NE 0.
              g_subrc_d = sy-subrc.
              REFRESH: g_lv_log_d[].
              CLEAR: g_wv_log_d.
              g_wv_log_d-progname = sy-tcode.
              g_wv_log_d-uname    = sy-uname.
              g_wv_log_d-sydate   = sy-datum.
              g_wv_log_d-sytime   = sy-uzeit.
              SELECT email FROM yrga_email_nomi
                INTO g_wv_log_d-email
                UP TO 1 ROWS
                WHERE kunnr = p_kunnr AND del_ind NE 'X'
                ORDER BY PRIMARY KEY.
              ENDSELECT.
              g_wv_log_d-logtype = 'E'.
              g_wv_log_d-kunnr   = p_kunnr.
              IF g_subrc_d = 1.
                g_wv_log_d-remarks = 'input_invalid'.
              ELSEIF g_subrc_d = 2.
                g_wv_log_d-remarks = 'overflow '.
              ELSEIF g_subrc_d = 3.
                g_wv_log_d-remarks = 'type_invalid'.
              ENDIF.
              APPEND g_wv_log_d TO g_lv_log_d.
              IF g_lv_log_d IS NOT INITIAL.
                MODIFY yrga_email_log FROM TABLE g_lv_log_d.
              ENDIF.
              CLEAR: g_subrc_d.
              EXIT.
            ENDIF.

            CALL FUNCTION 'ROUND'
              EXPORTING
                input         = gw_yro_nom_param1_d-ancv
              IMPORTING
                output        = g_wa_display_loc_d-ancv
              EXCEPTIONS
                input_invalid = 1
                overflow      = 2
                type_invalid  = 3.
            IF sy-subrc NE 0.
              g_subrc_d = sy-subrc.
              REFRESH: g_lv_log_d[].
              CLEAR: g_wv_log_d.
              g_wv_log_d-progname = sy-tcode.
              g_wv_log_d-uname    = sy-uname.
              g_wv_log_d-sydate   = sy-datum.
              g_wv_log_d-sytime   = sy-uzeit.
              SELECT email FROM yrga_email_nomi
                INTO g_wv_log_d-email
                UP TO 1 ROWS
                WHERE kunnr = p_kunnr AND del_ind NE 'X'
                ORDER BY PRIMARY KEY.
              ENDSELECT.
              g_wv_log_d-logtype = 'E'.
              g_wv_log_d-kunnr   = p_kunnr.
              IF g_subrc_d = 1.
                g_wv_log_d-remarks = 'input_invalid'.
              ELSEIF g_subrc_d = 2.
                g_wv_log_d-remarks = 'overflow '.
              ELSEIF g_subrc_d = 3.
                g_wv_log_d-remarks = 'type_invalid'.
              ENDIF.
              APPEND g_wv_log_d TO g_lv_log_d.
              IF g_lv_log_d IS NOT INITIAL.
                MODIFY yrga_email_log FROM TABLE g_lv_log_d.
              ENDIF.
              CLEAR: g_subrc_d.
              EXIT.
            ENDIF.
          ENDIF.

*         Delivery point data
          IF gw_oijnomi_d-idate GE '20201001'.
            READ TABLE lt_rv_del_loc INTO ls_rv_del_loc WITH KEY kunnr = gw_vbak_d-kunnr vbeln = gw_vbak_d-vbeln doc = gw_oijnomi_d-idate.
            IF sy-subrc EQ 0.
              g_wa_display_loc_d-del_point = ls_rv_del_loc-ddqty.
              g_wa_display_loc_d-del_uom = ls_rv_del_loc-qnt_uom.
            ENDIF.
          ELSE.
            g_wa_display_loc_d-del_point = gw_oijnomi1_d-yyoij_cnom_qty.
            g_wa_display_loc_d-del_uom = gw_oijnomi1_d-yyoij_cnom_uom.
          ENDIF.

          g_wa_display_loc_d-corr_nom = gw_oijnomi1_d-ga_allocated_qty.
          g_wa_display_loc_d-corr_uom = 'SM3'.
        ENDIF.

        g_wa_display_loc_d-redel_point = gw_oijnomi_d-yyoij_cnom_qty.
        g_wa_display_loc_d-redel_uom = gw_oijnomi_d-yyoij_cnom_uom.
        g_wa_display_loc_d-corr_nom1 = gw_oijnomi_d-ga_allocated_qty.
        g_wa_display_loc_d-corr_uom1 = 'SM3'.

        IF gw_oijnomi_d-yyoij_dpimb_qty IS NOT INITIAL.
          g_wa_display_loc_d-daily = gw_oijnomi_d-yyoij_dpimb_qty.
          wa_display1-totalpi = wa_display1-totalpi + gw_oijnomi_d-yyoij_dpimb_qty.
        ELSE.
          g_wa_display_loc_d-daily = gw_oijnomi_d-yyoij_dnimb_qty * -1.
          wa_display1-totalni = wa_display1-totalni + g_wa_display_loc_d-daily.
          IF wa_display1-totalni > 0.
            wa_display1-totalni = wa_display1-totalni * -1.
          ENDIF.
        ENDIF.

        IF gw_oijnomi_d-yyoij_dds_qty IS NOT INITIAL.
          g_wa_display_loc_d-daily_dds = gw_oijnomi_d-yyoij_dds_qty.
        ENDIF.
        g_wa_display_loc_d-daily_overrun = gw_oijnomi_d-yyoij_ovr_qty.
        g_wa_display_loc_d-daily_charge = gw_oijnomi_d-yyoij_unaovr_qty.

        g_wa_display_loc_d-daily_cumm = g_wa_display_loc_d-daily_cumm + g_wa_display_loc_d-daily + g_daily_d + g_wa_display_loc_d-daily_dds.

        IF g_wa_display_loc_d-daily_dds IS NOT INITIAL OR g_daily1_d <> 0.
          g_wa_display_loc_d-daily_cumm_dds = g_wa_display_loc_d-daily_cumm.
          g_daily1_d = g_wa_display_loc_d-daily_cumm_dds.
        ENDIF.

*       Chargeable imbalance calculation
        CLEAR gw_yrvt_contract_d.
        CLEAR g_lv_15_d.
        IF g_wa_display_loc_d-daily_cumm > 0.
          LOOP AT gt_yrvt_contract_d INTO gw_yrvt_contract_d
            WHERE vbeln = gw_oijnomi_d-docnr AND value_from <= gw_oijnomi_d-idate AND value_to => gw_oijnomi_d-idate AND clause = '10'.
          ENDLOOP.
          IF gw_yrvt_contract_d IS INITIAL.
            LOOP AT gt_yrvt_contract_d INTO gw_yrvt_contract_d
              WHERE vbeln = gw_oijnomi_d-docnr AND value_from <= gw_oijnomi_d-idate AND value_to => gw_oijnomi_d-idate AND clause = '15'.
              g_lv_15_d = 'X'.
            ENDLOOP.
          ENDIF.
          IF gw_yrvt_contract_d IS INITIAL.
            LOOP AT gt_yrvt_contract_d INTO gw_yrvt_contract_d
              WHERE vbeln = gw_oijnomi_d-docnr AND value_from <= gw_oijnomi_d-idate AND value_to => gw_oijnomi_d-idate AND clause = '16'.
            ENDLOOP.
          ENDIF.
        ELSE.
          LOOP AT gt_yrvt_contract_d INTO gw_yrvt_contract_d
            WHERE vbeln = gw_oijnomi_d-docnr AND value_from <= gw_oijnomi_d-idate AND value_to => gw_oijnomi_d-idate AND clause = '11'.
          ENDLOOP.
        ENDIF.

        CLEAR: g_mdq_quan_d, g_refvalue_d.

        IF gw_yrvt_contract_d-cal_unit = 10.
          READ TABLE gt_lt_yrvt_cont_dcq_d INTO gw_ls_yrvt_cont_dcq_d WITH KEY vbeln = gw_oijnomi_d-docnr ydate = gw_oijnomi_d-idate.
          IF sy-subrc = 0.
            g_mdq_quan_d = gw_ls_yrvt_cont_dcq_d-dcq_qty.
          ENDIF.
          CLEAR gw_ls_yrvt_cont_dcq_d-dcq_qty.
          g_refvalue_d = g_mdq_quan_d * ( gw_yrvt_contract_d-threshold / 100 ).
        ELSEIF gw_yrvt_contract_d-cal_unit = 13.
          g_mdq_quan_d = gw_yrvt_contract_d-remarks.
          g_refvalue_d = g_mdq_quan_d * ( gw_yrvt_contract_d-threshold / 100 ).
        ELSEIF gw_yrvt_contract_d-cal_unit = 11.
          READ TABLE gt_yro_nom_param_d INTO gw_yro_nom_param_d WITH KEY nomtk = gw_oijnomi_d-nomtk nomit = gw_oijnomi_d-nomit.
          IF sy-subrc = 0.
            CLEAR g_adq_d.
            g_adq_d = gw_oijnomi_d-menge.
            READ TABLE gt_oij_el_cp_layt_d INTO gw_oij_el_cp_layt_d WITH KEY vbeln = gw_oijnomi_d-docnr.
            CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
              EXPORTING
                i_trqty = g_adq_d
                i_truom = 'SM3'
                i_tguom = gw_oij_el_cp_layt_d-mdq_uom
                lv_gcv  = gw_yro_nom_param_d-agcv
                lv_ncv  = gw_yro_nom_param_d-ancv
              CHANGING
                c_tgqty = g_adq_d.
            g_refvalue_d = g_adq_d.
            g_mdq_quan_d = g_adq_d.
            g_refvalue_d = g_refvalue_d * ( gw_yrvt_contract_d-threshold / 100 ).
          ENDIF.
        ENDIF.

        IF g_lv_15_d EQ 'X'.
          CLEAR g_refvalue_d.
        ENDIF.

        IF g_wa_display_loc_d-daily_cumm > 0.
          IF g_refvalue_d < g_wa_display_loc_d-daily_cumm.
            g_wa_display_loc_d-chargable_imb = g_wa_display_loc_d-daily_cumm - g_refvalue_d.
            wa_display1-totalpic = wa_display1-totalpic + g_wa_display_loc_d-chargable_imb.
          ENDIF.
        ELSEIF g_wa_display_loc_d-daily_cumm < 0.
          g_refvalue_d = g_refvalue_d * -1.
          IF g_refvalue_d > g_wa_display_loc_d-daily_cumm.
            g_wa_display_loc_d-chargable_imb = g_wa_display_loc_d-daily_cumm - g_refvalue_d.
            wa_display1-totalnic = wa_display1-totalnic + g_wa_display_loc_d-chargable_imb.
          ENDIF.
        ENDIF.
        wa_display1-totalcharge = wa_display1-totalcharge + g_wa_display_loc_d-daily_charge.
        wa_display1-totaloverrun = wa_display1-totaloverrun + g_wa_display_loc_d-daily_overrun.

*       G32 augru check
        IF gw_vbak_d-augru = 'G32'.
          CLEAR g_wa_display_loc_d-chargable_imb.
          wa_display1-augru = gw_vbak_d-augru.
          g_wa_display_loc_d-augru = gw_vbak_d-augru.
        ENDIF.
        READ TABLE gt_vbak_d INTO gw_vbak1_d WITH KEY vbeln_grp = g_wa_display_loc_d-vbeln_grp augru = 'G32'.
        IF sy-subrc = 0.
          CLEAR g_wa_display_loc_d-chargable_imb.
          wa_display1-augru = gw_vbak1_d-augru.
          g_wa_display_loc_d-augru = gw_vbak1_d-augru.
        ENDIF.

*       DDS clause logic
        IF gw_oijnomi_d-yyoij_dds_qty IS INITIAL.
          g_wa_display_loc_d-daily_dds = g_wa_display_loc_d-daily.
        ENDIF.
        CLEAR: g_lv_15_d, g_lv_11_d, g_lv_10_d, g_lv_12_d.
        LOOP AT gt_yrvt_contract_d INTO gw_yrvt_contract1_d WHERE vbeln = gw_vbak_d-vbeln AND value_from LE gw_oijnomi_d-idate AND value_to GE gw_oijnomi_d-idate.
          IF gw_yrvt_contract1_d-clause EQ '15'.
            g_lv_15_d = 'X'.
          ENDIF.
          IF gw_yrvt_contract1_d-clause EQ '11'.
            g_lv_11_d = 'X'.
          ENDIF.
          IF gw_yrvt_contract1_d-clause EQ '10'.
            g_lv_10_d = 'X'.
          ENDIF.
          IF gw_yrvt_contract1_d-clause EQ '12'.
            g_lv_12_d = 'X'.
          ENDIF.
          CLEAR gw_yrvt_contract1_d.
        ENDLOOP.

        IF g_lv_12_d NE 'X'.
          wa_display1-totalcharge = wa_display1-totalcharge - g_wa_display_loc_d-daily_charge.
          CLEAR g_wa_display_loc_d-daily_charge.
        ENDIF.
        IF g_lv_15_d EQ 'X'.
          IF g_lv_11_d EQ 'X'.
            IF g_wa_display_loc_d-chargable_imb < 0.
            ELSE.
              IF g_wa_display_loc_d-chargable_imb > 0.
                wa_display1-totalpic = wa_display1-totalpic - g_wa_display_loc_d-chargable_imb.
                g_wa_display_loc_d-daily_cumm_dds = g_wa_display_loc_d-chargable_imb.
                CLEAR g_wa_display_loc_d-chargable_imb.
              ENDIF.
            ENDIF.
          ELSE.
            g_wa_display_loc_d-daily_cumm_dds = g_wa_display_loc_d-chargable_imb.
            IF g_wa_display_loc_d-chargable_imb > 0.
              wa_display1-totalpic = wa_display1-totalpic - g_wa_display_loc_d-chargable_imb.
              CLEAR g_wa_display_loc_d-chargable_imb.
            ENDIF.
            IF g_wa_display_loc_d-chargable_imb < 0.
              wa_display1-totalnic = wa_display1-totalnic - g_wa_display_loc_d-chargable_imb.
              CLEAR g_wa_display_loc_d-chargable_imb.
            ENDIF.
          ENDIF.
        ELSE.
          CLEAR g_wa_display_loc_d-daily_cumm_dds.
          IF g_lv_11_d EQ 'X'.
          ELSE.
            IF g_wa_display_loc_d-chargable_imb < 0.
              wa_display1-totalnic = wa_display1-totalnic - g_wa_display_loc_d-chargable_imb.
              CLEAR g_wa_display_loc_d-chargable_imb.
            ENDIF.
          ENDIF.
          IF g_lv_10_d EQ 'X'.
          ELSE.
            IF g_wa_display_loc_d-chargable_imb > 0.
              wa_display1-totalpic = wa_display1-totalpic - g_wa_display_loc_d-chargable_imb.
              CLEAR g_wa_display_loc_d-chargable_imb.
            ENDIF.
          ENDIF.
        ENDIF.

        IF g_wa_display_loc_d-daily_cumm_dds < 0.
          CLEAR g_wa_display_loc_d-daily_cumm_dds.
        ENDIF.
        wa_display1-totaldds = wa_display1-totaldds + g_wa_display_loc_d-daily_cumm_dds.

*       Set remaining display fields
        READ TABLE lt_gta_imb_loc INTO ls_gta_imb_loc WITH KEY contract_from = gw_vbak_d-vbeln.
        IF sy-subrc IS INITIAL.
          g_wa_display_loc_d-vbeln_c = ls_gta_imb_loc-contract_to.
        ENDIF.
        g_wa_display_loc_d-vbeln = gw_vbak_d-vbeln.
        g_wa_display_loc_d-date = gw_oijnomi_d-idate.
        g_wa_display_loc_d-ktext = gw_vbak_d-ktext.
        g_wa_display_loc_d-kunnr = gw_vbak_d-kunnr.
        g_wa_display_loc_d-menge = gw_oijnomi_d-menge.
        g_wa_display_loc_d-mdq_quan = g_mdq_quan_d.
        g_wa_display_loc_d-matnr = gw_oijnomi_d-s_matnr_i.

*       GCV/NCV for redelivery
        READ TABLE gt_yro_nom_param_d INTO gw_yro_nom_param_d
          WITH KEY nomtk = gw_oijnomi_d-nomtk
                   nomit = gw_oijnomi_d-nomit.
        IF sy-subrc = 0.
          CALL FUNCTION 'ROUND'
            EXPORTING
              input         = gw_yro_nom_param_d-agcv
            IMPORTING
              output        = g_wa_display_loc_d-agcv1
            EXCEPTIONS
              input_invalid = 1
              overflow      = 2
              type_invalid  = 3.
          IF sy-subrc NE 0.
            g_subrc_d = sy-subrc.
            REFRESH: g_lv_log_d[].
            CLEAR: g_wv_log_d.
            g_wv_log_d-progname = sy-tcode.
            g_wv_log_d-uname    = sy-uname.
            g_wv_log_d-sydate   = sy-datum.
            g_wv_log_d-sytime   = sy-uzeit.
            SELECT email FROM yrga_email_nomi
              INTO g_wv_log_d-email UP TO 1 ROWS
              WHERE kunnr = p_kunnr AND del_ind NE 'X'
              ORDER BY PRIMARY KEY.
            ENDSELECT.
            g_wv_log_d-logtype = 'E'.
            g_wv_log_d-kunnr   = p_kunnr.
            IF g_subrc_d = 1.
              g_wv_log_d-remarks = 'input_invalid'.
            ELSEIF g_subrc_d = 2.
              g_wv_log_d-remarks = 'overflow '.
            ELSEIF g_subrc_d = 3.
              g_wv_log_d-remarks = 'type_invalid'.
            ENDIF.
            APPEND g_wv_log_d TO g_lv_log_d.
            IF g_lv_log_d IS NOT INITIAL.
              MODIFY yrga_email_log FROM TABLE g_lv_log_d.
            ENDIF.
            CLEAR: g_subrc_d.
            EXIT.
          ENDIF.

          CALL FUNCTION 'ROUND'
            EXPORTING
              input         = gw_yro_nom_param_d-ancv
            IMPORTING
              output        = g_wa_display_loc_d-ancv1
            EXCEPTIONS
              input_invalid = 1
              overflow      = 2
              type_invalid  = 3.
          IF sy-subrc NE 0.
            REFRESH: g_lv_log_d[].
            CLEAR: g_wv_log_d.
            g_wv_log_d-progname = sy-tcode.
            g_wv_log_d-uname    = sy-uname.
            g_wv_log_d-sydate   = sy-datum.
            g_wv_log_d-sytime   = sy-uzeit.
            SELECT email FROM yrga_email_nomi
              INTO g_wv_log_d-email UP TO 1 ROWS
              WHERE kunnr = p_kunnr AND del_ind NE 'X'
              ORDER BY PRIMARY KEY.
            ENDSELECT.
            g_wv_log_d-logtype = 'E'.
            g_wv_log_d-kunnr   = p_kunnr.
            IF sy-subrc = 1.
              g_wv_log_d-remarks = 'input_invalid'.
            ELSEIF sy-subrc = 2.
              g_wv_log_d-remarks = 'overflow '.
            ELSEIF sy-subrc = 3.
              g_wv_log_d-remarks = 'type_invalid'.
            ENDIF.
            APPEND g_wv_log_d TO g_lv_log_d.
            IF g_lv_log_d IS NOT INITIAL.
              MODIFY yrga_email_log FROM TABLE g_lv_log_d.
            ENDIF.
            EXIT.
          ENDIF.
        ENDIF.

        g_menge_d = g_menge_d + gw_oijnomi_d-ga_allocated_qty.
        g_wt_d = g_wt_d + ( gw_oijnomi_d-ga_allocated_qty * g_wa_display_loc_d-agcv1 ).
        g_wt1_d = g_wt1_d + ( gw_oijnomi_d-ga_allocated_qty * g_wa_display_loc_d-ancv1 ).

*       Delivery qty in MBG
        IF g_wa_display_loc_d-agcv IS NOT INITIAL AND g_wa_display_loc_d-corr_nom IS NOT INITIAL.
          g_adq_d = g_wa_display_loc_d-agcv * g_wa_display_loc_d-corr_nom * '3.968254' / 1000000.
          g_wa_display_loc_d-del_mbg = g_adq_d.
        ENDIF.
*       Redelivery qty in MBG
        IF g_wa_display_loc_d-agcv1 IS NOT INITIAL AND g_wa_display_loc_d-corr_nom1 IS NOT INITIAL.
          g_adq_d = g_wa_display_loc_d-agcv1 * g_wa_display_loc_d-corr_nom1 * '3.968254' / 1000000.
          g_wa_display_loc_d-redel_mbg = g_adq_d.
        ENDIF.

        APPEND g_wa_display_loc_d TO g_it_display_d.
        APPEND g_wa_display_loc_d TO g_it_disp_temp_d.
        CLEAR ls_rv_del_loc.
        g_daily_d = g_wa_display_loc_d-daily_cumm.

      ENDLOOP.

      CLEAR g_daily_d.
      CLEAR g_daily1_d.

      IF wa_display1 IS NOT INITIAL.
        wa_display1-totalcumi = g_wa_display_loc_d-daily_cumm.
*       s_vbeln is INITIAL - skip vbeln_grp assignment
        wa_display1-wt = g_wt_d / g_menge_d.
        wa_display1-wt1 = g_wt1_d / g_menge_d.
        wa_display1-kunnr = gw_vbak_d-kunnr.
        wa_display1-augru = g_wa_display_loc_d-augru.
        APPEND wa_display1 TO it_display1.
        CLEAR: wa_display1, g_wa_display_loc_d.
      ENDIF.
      CLEAR: g_menge_d, g_wt_d, g_wt1_d.

    ENDLOOP.
  ENDIF.

*----------------------------------------------------------------------*
* Section 12: Post-processing - UOM conversions (YRG011:1900-1968)
*----------------------------------------------------------------------*
  REFRESH g_it_display11_d.
  DELETE it_display1 WHERE vbeln IS INITIAL.
  DATA w_ncv_loc TYPE yyncv.
  DATA w_gcv_loc TYPE yyncv.
  LOOP AT it_display1 INTO wa_display1.
    l_tabix_loc = sy-tabix.
    CLEAR: g_mdq_quan_d.
    LOOP AT gt_oij_el_cp_layt_d INTO gw_oij_el_cp_layt_d WHERE vbeln = wa_display1-vbeln.
      g_mdq_quan_d = g_mdq_quan_d + gw_oij_el_cp_layt_d-mdq_quan.
    ENDLOOP.
    CLEAR: g_adq_d, w_ncv_loc, w_gcv_loc.
    g_adq_d = wa_display1-totalpic.
    w_ncv_loc = wa_display1-wt1.
    w_gcv_loc = wa_display1-wt.
    CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
      EXPORTING
        i_trqty = g_adq_d
        i_truom = gw_oij_el_cp_layt_d-mdq_uom
        i_tguom = 'SM3'
        lv_gcv  = w_gcv_loc
        lv_ncv  = w_ncv_loc
      CHANGING
        c_tgqty = g_adq_d.
    wa_display1-totalpic1 = g_adq_d.

    CLEAR g_adq_d.
    g_adq_d = wa_display1-totaldds.
    CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
      EXPORTING
        i_trqty = g_adq_d
        i_truom = gw_oij_el_cp_layt_d-mdq_uom
        i_tguom = 'SM3'
        lv_gcv  = w_gcv_loc
        lv_ncv  = w_ncv_loc
      CHANGING
        c_tgqty = g_adq_d.
    wa_display1-totaldds1 = g_adq_d.

    CLEAR g_adq_d.
    g_adq_d = wa_display1-totalnic.
    CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
      EXPORTING
        i_trqty = g_adq_d
        i_truom = gw_oij_el_cp_layt_d-mdq_uom
        i_tguom = 'SM3'
        lv_gcv  = w_gcv_loc
        lv_ncv  = w_ncv_loc
      CHANGING
        c_tgqty = g_adq_d.
    wa_display1-totalnic1 = g_adq_d.

*   Opening and closing balance
    SORT g_it_display_d BY date ASCENDING.
    READ TABLE g_it_display_d INTO g_wa_display_loc_d WITH KEY vbeln = wa_display1-vbeln.
    IF sy-subrc EQ 0.
      wa_display1-opencumi = g_wa_display_loc_d-daily_cumm - g_wa_display_loc_d-daily.
    ENDIF.
    SORT g_it_display_d BY date DESCENDING.
    READ TABLE g_it_display_d INTO g_wa_display_loc_d WITH KEY vbeln = wa_display1-vbeln.
    IF sy-subrc EQ 0.
      wa_display1-closecumi = g_wa_display_loc_d-daily_cumm.
    ENDIF.

    MODIFY it_display1 FROM wa_display1 INDEX l_tabix_loc TRANSPORTING totalpic1 totalnic1 totaldds1 opencumi closecumi.
  ENDLOOP.

*----------------------------------------------------------------------*
* Section 13: Master contract grouping (YRG011:1970-2306)
*----------------------------------------------------------------------*
  SELECT SINGLE * FROM vbak
    INTO CORRESPONDING FIELDS OF gw_vbak_d
    WHERE vbeln = g_w_master_contract_d
    AND abrvw = 'Z02'.
  IF sy-subrc EQ 0.
*   r_dds IS INITIAL -> enter grouping block
*   Delete display records where contract not in display1
    LOOP AT g_it_display_d INTO g_wa_display_loc_d.
      READ TABLE it_display1 INTO wa_display1 WITH KEY vbeln = g_wa_display_loc_d-vbeln.
      IF sy-subrc NE 0.
        DELETE TABLE g_it_display_d FROM g_wa_display_loc_d.
      ENDIF.
    ENDLOOP.

    g_w_group_imbalance_d = 'X'.
    DATA: w_tot_redel_vol_loc     TYPE p DECIMALS 3,
          w_tot_redel_energy_loc  TYPE p DECIMALS 3.
    SORT g_it_display_d BY date.
    g_it_display3_loc_d[] = g_it_display_d[].
    DELETE g_it_display3_loc_d[] WHERE vbeln_grp IS INITIAL.
    SORT g_it_display3_loc_d BY date.

    LOOP AT g_it_display3_loc_d INTO g_wa_display_loc_d.
*     Read Z01 contract for clause determination
      READ TABLE gt_vbak_d INTO gw_vbak_d WITH KEY abrvw = 'Z01' vbeln = g_wa_display_loc_d-vbeln.
      IF sy-subrc EQ 0.
        LOOP AT gt_yrvt_contract_d INTO gw_yrvt_contract_d WHERE vbeln = gw_vbak_d-vbeln AND value_from LE g_wa_display_loc_d-date
          AND value_to GE g_wa_display_loc_d-date.
          IF gw_yrvt_contract_d-clause = '15'.
            IF g_wa_display_temp_loc_d-sel = 'Z'.
              g_wa_display_temp_loc_d-sel = 'V'.
            ELSE.
              g_wa_display_temp_loc_d-sel = 'X'.
            ENDIF.
          ELSEIF gw_yrvt_contract_d-clause = '10'.
            IF g_wa_display_temp_loc_d-sel = 'Z'.
              g_wa_display_temp_loc_d-sel = 'W'.
            ELSE.
              g_wa_display_temp_loc_d-sel = 'Y'.
            ENDIF.
          ELSEIF gw_yrvt_contract_d-clause = '11'.
            IF g_wa_display_temp_loc_d-sel = 'Y'.
              g_wa_display_temp_loc_d-sel = 'W'.
            ELSEIF g_wa_display_temp_loc_d-sel = 'X'.
              g_wa_display_temp_loc_d-sel = 'V'.
            ELSE.
              g_wa_display_temp_loc_d-sel = 'Z'.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

      g_wa_display_temp_loc_d-date = g_wa_display_loc_d-date.
      g_wa_display_temp_loc_d-mdq_quan = g_wa_display_temp_loc_d-mdq_quan + g_wa_display_loc_d-mdq_quan.
      g_wa_display_temp_loc_d-del_point = g_wa_display_temp_loc_d-del_point + g_wa_display_loc_d-del_point.
      g_wa_display_temp_loc_d-corr_nom = g_wa_display_temp_loc_d-corr_nom + g_wa_display_loc_d-corr_nom.
      g_wa_display_temp_loc_d-del_mbg = g_wa_display_temp_loc_d-del_mbg + g_wa_display_loc_d-del_mbg.
      g_wa_display_temp_loc_d-redel_point = g_wa_display_temp_loc_d-redel_point + g_wa_display_loc_d-redel_point.
      g_wa_display_temp_loc_d-corr_nom1 = g_wa_display_temp_loc_d-corr_nom1 + g_wa_display_loc_d-corr_nom1.
      g_wa_display_temp_loc_d-redel_mbg = g_wa_display_temp_loc_d-redel_mbg + g_wa_display_loc_d-redel_mbg.
      g_wa_display_temp_loc_d-corr_uom = g_wa_display_loc_d-corr_uom.
      g_wa_display_temp_loc_d-del_uom = g_wa_display_loc_d-del_uom.
      g_wa_display_temp_loc_d-redel_uom = g_wa_display_loc_d-redel_uom.
      g_wa_display_temp_loc_d-corr_uom1 = g_wa_display_loc_d-corr_uom1.
      g_wa_display_temp_loc_d-matnr = g_wa_display_loc_d-matnr.
      w_tot_redel_vol_loc = g_wa_display_loc_d-corr_nom1 + w_tot_redel_vol_loc.
      w_tot_redel_energy_loc = g_wa_display_loc_d-redel_mbg + w_tot_redel_energy_loc.
      AT END OF date.
*       s_vbeln is INITIAL - use empty vbeln for master summary
        g_wa_display_temp_loc_d-vbeln = lv_s_vbeln.
        g_wa_display_temp_loc_d-ktext = gw_vbak_d-ktext.
        g_wa_display_temp_loc_d-kunnr = gw_vbak_d-kunnr.
        APPEND g_wa_display_temp_loc_d TO g_it_display_temp_d.
        CLEAR g_wa_display_temp_loc_d.
      ENDAT.
    ENDLOOP.

*   Cumulative history for master contract
    SELECT * INTO CORRESPONDING FIELDS OF TABLE g_it_yrg_cumm_imb2_d
      FROM yrg_cumm_imb
      WHERE yy_contract = g_w_master_contract_d
      AND endda < s_date-low.
    SORT g_it_yrg_cumm_imb2_d BY timestamp DESCENDING.

    CLEAR: g_l_daily_imb_d, g_l_daily1_imb_d, g_l_daily2_imb_d.
    lv_checked_loc = space.
    SORT g_it_display_temp_d BY date.

    LOOP AT g_it_display_temp_d INTO g_wa_display_temp_loc_d.
      g_wa_display_temp_loc_d-daily = g_wa_display_temp_loc_d-del_mbg - g_wa_display_temp_loc_d-redel_mbg.
      g_w_temp_d = g_wa_display_temp_loc_d-redel_mbg - g_wa_display_temp_loc_d-mdq_quan.
      g_w_temp1_d = g_wa_display_temp_loc_d-redel_mbg - g_wa_display_temp_loc_d-redel_point.
      IF g_w_temp_d > 0 AND g_w_temp1_d > 0.
        IF g_wa_display_temp_loc_d-redel_point > g_wa_display_temp_loc_d-mdq_quan.
          g_wa_display_temp_loc_d-daily_overrun = g_wa_display_temp_loc_d-redel_mbg - g_wa_display_temp_loc_d-redel_point.
        ELSE.
          g_wa_display_temp_loc_d-daily_overrun = g_wa_display_temp_loc_d-redel_mbg - g_wa_display_temp_loc_d-mdq_quan.
        ENDIF.
      ENDIF.
      CLEAR g_w_temp_d.
      g_w_temp_d = g_wa_display_temp_loc_d-daily_overrun - g_wa_display_temp_loc_d-mdq_quan * '0.1'.
      IF g_w_temp_d > 0.
        g_wa_display_temp_loc_d-daily_charge = g_w_temp_d.
      ENDIF.

      IF g_l_daily_imb_d IS INITIAL AND lv_checked_loc IS INITIAL.
        READ TABLE g_it_yrg_cumm_imb2_d INTO gw_yrg_cumm_imb_d INDEX 1.
        IF sy-subrc = 0.
          g_l_daily_imb_d = gw_yrg_cumm_imb_d-yy_oij_cumimb.
        ELSE.
*         GTA parent fallback for master contract
          CLEAR: g_l_cumm_bal_imb_d, gw_header_d.
          SORT g_it_disp_temp_d BY vbeln ASCENDING date DESCENDING.
          LOOP AT lt_gta_imb_loc INTO ls_gta_imb_loc WHERE contract_to = g_wa_display_temp_loc_d-vbeln.
            gw_header_d-vbeln = g_wa_display_temp_loc_d-vbeln.
            gw_header_d-p_cont = ls_gta_imb_loc-contract_from.
            READ TABLE lt_cumm_imb_temp_gta INTO ls_cumm_imb_temp_gta WITH KEY yy_contract = ls_gta_imb_loc-contract_from.
            IF sy-subrc IS INITIAL.
              g_l_cumm_bal_imb_d = g_l_cumm_bal_imb_d + ls_cumm_imb_temp_gta-yy_oij_cumimb.
              gw_header_d-imbal = ls_cumm_imb_temp_gta-yy_oij_cumimb.
            ENDIF.
            APPEND gw_header_d TO gt_header_d.
            CLEAR: ls_cumm_imb_temp_gta, gw_header_d.
          ENDLOOP.
          g_l_daily_imb_d = g_l_cumm_bal_imb_d.
          CLEAR ls_gta_imb_loc.
        ENDIF.
        lv_checked_loc = 'X'.
      ENDIF.

      g_l_daily_imb_d = g_l_daily_imb_d + g_wa_display_temp_loc_d-daily.
      g_wa_display_temp_loc_d-daily_cumm = g_l_daily_imb_d.

*     Chargeable imbalance for master
      IF g_wa_display_temp_loc_d-daily_cumm > 0.
        g_wa_display_temp_loc_d-chargable_imb = g_wa_display_temp_loc_d-daily_cumm - g_wa_display_temp_loc_d-mdq_quan * '0.1'.
        IF g_wa_display_temp_loc_d-chargable_imb LT 0.
          g_wa_display_temp_loc_d-chargable_imb = 0.
        ENDIF.
      ELSEIF g_wa_display_temp_loc_d-daily_cumm < 0.
        g_wa_display_temp_loc_d-chargable_imb = ( g_wa_display_temp_loc_d-daily_cumm + g_wa_display_temp_loc_d-mdq_quan * '0.05' ).
        IF g_wa_display_temp_loc_d-chargable_imb GT 0.
          g_wa_display_temp_loc_d-chargable_imb = 0.
        ENDIF.
      ENDIF.

*     DDS clause combinations
      g_l_daily2_imb_d = g_l_daily2_imb_d + g_wa_display_temp_loc_d-daily.
      IF g_wa_display_temp_loc_d-sel = 'X'.
        IF g_l_daily2_imb_d > 0.
          g_wa_display_temp_loc_d-daily_cumm_dds = g_wa_display_temp_loc_d-daily_cumm.
        ENDIF.
        g_wa_display_temp_loc_d-daily_dds = g_wa_display_temp_loc_d-daily.
        CLEAR: g_wa_display_temp_loc_d-daily, g_wa_display_temp_loc_d-chargable_imb.
      ELSEIF g_wa_display_temp_loc_d-sel = 'V'.
        IF g_l_daily2_imb_d > 0.
          g_wa_display_temp_loc_d-daily_cumm_dds = g_wa_display_temp_loc_d-daily_cumm.
        ENDIF.
        g_wa_display_temp_loc_d-daily_dds = g_wa_display_temp_loc_d-daily.
        IF g_wa_display_temp_loc_d-daily_cumm < 0.
          IF ( ( g_wa_display_temp_loc_d-mdq_quan * '0.05' ) + g_wa_display_temp_loc_d-daily_cumm ) LE 0.
            g_wa_display_temp_loc_d-chargable_imb = g_wa_display_temp_loc_d-daily_cumm + ( g_wa_display_temp_loc_d-mdq_quan * '0.05' ).
          ENDIF.
        ENDIF.
        CLEAR: g_wa_display_temp_loc_d-daily, g_wa_display_temp_loc_d-chargable_imb.
      ELSEIF g_wa_display_temp_loc_d-sel EQ 'Y' AND g_wa_display_temp_loc_d-daily NE 0.
        g_wa_display_temp_loc_d-daily_dds = g_wa_display_temp_loc_d-daily.
        IF g_wa_display_temp_loc_d-daily_cumm > 0.
          IF ( g_wa_display_temp_loc_d-daily_cumm - ( g_wa_display_temp_loc_d-mdq_quan * '0.10' ) ) GE 0.
            g_wa_display_temp_loc_d-chargable_imb = g_wa_display_temp_loc_d-daily_cumm - ( g_wa_display_temp_loc_d-mdq_quan * '0.10' ).
          ENDIF.
        ELSE.
          CLEAR g_wa_display_temp_loc_d-chargable_imb.
        ENDIF.
      ELSEIF g_wa_display_temp_loc_d-sel EQ 'Z' AND g_wa_display_temp_loc_d-daily NE 0.
        g_wa_display_temp_loc_d-daily_dds = g_wa_display_temp_loc_d-daily.
        IF g_wa_display_temp_loc_d-daily_cumm < 0.
          IF ( ( g_wa_display_temp_loc_d-mdq_quan * '0.05' ) + g_wa_display_temp_loc_d-daily_cumm ) LE 0.
            g_wa_display_temp_loc_d-chargable_imb = g_wa_display_temp_loc_d-daily_cumm + ( g_wa_display_temp_loc_d-mdq_quan * '0.05' ).
          ENDIF.
        ELSE.
          CLEAR g_wa_display_temp_loc_d-chargable_imb.
        ENDIF.
      ELSEIF g_wa_display_temp_loc_d-sel EQ 'W' AND g_wa_display_temp_loc_d-daily NE 0.
        g_wa_display_temp_loc_d-daily_dds = g_wa_display_temp_loc_d-daily.
        IF g_wa_display_temp_loc_d-daily_cumm > 0.
          IF ( g_wa_display_temp_loc_d-daily_cumm - ( g_wa_display_temp_loc_d-mdq_quan * '0.10' ) ) GE 0.
            g_wa_display_temp_loc_d-chargable_imb = g_wa_display_temp_loc_d-daily_cumm - ( g_wa_display_temp_loc_d-mdq_quan * '0.10' ).
          ENDIF.
        ENDIF.
        IF g_wa_display_temp_loc_d-daily_cumm < 0.
          IF ( ( g_wa_display_temp_loc_d-mdq_quan * '0.05' ) + g_wa_display_temp_loc_d-daily_cumm ) LE 0.
            g_wa_display_temp_loc_d-chargable_imb = g_wa_display_temp_loc_d-daily_cumm + ( g_wa_display_temp_loc_d-mdq_quan * '0.05' ).
          ENDIF.
        ENDIF.
      ELSEIF g_wa_display_temp_loc_d-sel IS INITIAL AND g_wa_display_temp_loc_d-daily NE 0.
        g_wa_display_temp_loc_d-daily_dds = g_wa_display_temp_loc_d-daily.
        CLEAR g_wa_display_temp_loc_d-chargable_imb.
      ENDIF.

*     G32 augru cleanup for master
      READ TABLE gt_vbak1_d INTO gw_vbak1_d WITH KEY vbeln_grp = g_wa_display_temp_loc_d-vbeln augru = 'G32'.
      IF sy-subrc = 0.
        CLEAR g_wa_display_temp_loc_d-chargable_imb.
        g_wa_display_temp_loc_d-augru = gw_vbak1_d-augru.
      ENDIF.

      MODIFY g_it_display_temp_d FROM g_wa_display_temp_loc_d.
    ENDLOOP.

*   Build master contract summary for it_display1
    SORT g_it_display_temp_d BY date ASCENDING.
    CLEAR: wa_display1.
    LOOP AT g_it_display_temp_d INTO g_wa_display_temp_loc_d.
      wa_display1-vbeln = g_wa_display_temp_loc_d-vbeln.
      wa_display1-matnr = g_wa_display_temp_loc_d-matnr.
      wa_display1-ktext = g_wa_display_temp_loc_d-ktext.
      wa_display1-kunnr = g_wa_display_temp_loc_d-kunnr.
      IF g_wa_display_temp_loc_d-daily < 0.
        wa_display1-totalni = wa_display1-totalni + g_wa_display_temp_loc_d-daily.
      ELSE.
        wa_display1-totalpi = wa_display1-totalpi + g_wa_display_temp_loc_d-daily.
      ENDIF.
      IF g_wa_display_temp_loc_d-chargable_imb < 0.
        wa_display1-totalnic = wa_display1-totalnic + g_wa_display_temp_loc_d-chargable_imb.
      ELSE.
        wa_display1-totalpic = wa_display1-totalpic + g_wa_display_temp_loc_d-chargable_imb.
      ENDIF.
      wa_display1-totalcumi = g_wa_display_temp_loc_d-daily_cumm.
      wa_display1-totalcharge = wa_display1-totalcharge + g_wa_display_temp_loc_d-daily_charge.
      wa_display1-totaloverrun = wa_display1-totaloverrun + g_wa_display_temp_loc_d-daily_overrun.
    ENDLOOP.
    wa_display1-knkli = gw_vbak_d-knkli.

    wa_display1-wt = w_tot_redel_energy_loc * 1000000 / ( w_tot_redel_vol_loc * '3.968254' ).
    wa_display1-wt1 = wa_display1-wt / '1.109'.

    g_adq_d = wa_display1-totalpic.
    w_ncv_loc = wa_display1-wt1.
    w_gcv_loc = wa_display1-wt.
    CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
      EXPORTING
        i_trqty = g_adq_d
        i_truom = 'MBG'
        i_tguom = 'SM3'
        lv_gcv  = w_gcv_loc
        lv_ncv  = w_ncv_loc
      CHANGING
        c_tgqty = g_adq_d.
    wa_display1-totalpic1 = g_adq_d.

    CLEAR g_adq_d.
    g_adq_d = wa_display1-totalnic.
    CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
      EXPORTING
        i_trqty = g_adq_d
        i_truom = 'MBG'
        i_tguom = 'SM3'
        lv_gcv  = w_gcv_loc
        lv_ncv  = w_ncv_loc
      CHANGING
        c_tgqty = g_adq_d.
    wa_display1-totalnic1 = g_adq_d.

*   Get cumm_imb for master contract
    SELECT * INTO gw_yrg_cumm_imb_d
      FROM yrg_cumm_imb
      UP TO 1 ROWS
      WHERE yy_contract = g_w_master_contract_d
      AND begda = s_date-low
      AND endda = s_date-high
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF sy-subrc EQ 0.
      wa_display1-vbeln1 = gw_yrg_cumm_imb_d-vbeln.
    ENDIF.
    wa_display1-line_color = 'C410'.
    wa_display1-augru = g_wa_display_temp_loc_d-augru.
    APPEND wa_display1 TO it_display1.

    SORT g_it_display_d BY vbeln DESCENDING.
  ENDIF.

* Z01 contract aggregation
  LOOP AT gt_vbak1_d INTO gw_vbak_d WHERE abrvw = 'Z01'.
    CLEAR gw_display11_d.
    READ TABLE it_display1 INTO gw_display11_d WITH KEY vbeln = gw_vbak_d-vbeln.
    IF sy-subrc = 0.
      LOOP AT it_display1 INTO wa_display1 WHERE vbeln_grp = gw_vbak_d-vbeln_grp.
        IF wa_display1-vbeln <> gw_display11_d-vbeln.
          gw_display11_d-totalpic = gw_display11_d-totalpic + wa_display1-totalpic.
          gw_display11_d-totalnic = gw_display11_d-totalnic + wa_display1-totalnic.
          gw_display11_d-totalpic1 = gw_display11_d-totalpic1 + wa_display1-totalpic1.
          gw_display11_d-totalnic1 = gw_display11_d-totalnic1 + wa_display1-totalnic1.
          gw_display11_d-totalcharge = gw_display11_d-totalcharge + wa_display1-totalcharge.
        ENDIF.
      ENDLOOP.
      APPEND gw_display11_d TO g_it_display11_d.
    ENDIF.
  ENDLOOP.

  SORT g_it_display_d BY date.

*----------------------------------------------------------------------*
* Section 14: Previous postings check (YRG011:2341-2394)
*----------------------------------------------------------------------*
  DELETE it_display1[] WHERE vbeln IS INITIAL.
  CLEAR: g_process_flag_d.
  IF it_display1[] IS NOT INITIAL.
    CLEAR wa_display1.
    LOOP AT it_display1 INTO wa_display1.
*     check_previous_postings - keep original suppression logic
      IF ( sy-tcode NE 'YRGR102' AND NOT sy-batch EQ 'X' ).
        CALL METHOD ycl_gms_report_validations=>check_previous_postings
          EXPORTING
            i_vbeln      = wa_display1-vbeln
            i_kunnr      = wa_display1-kunnr
            i_knkli      = wa_display1-knkli
            i_valid_from = s_date-low
            i_valid_to   = s_date-high
          IMPORTING
            e_exit_processing = g_process_flag_d.
        IF g_process_flag_d = 'X'.
          EXIT.
        ELSE.
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF g_process_flag_d = 'X'.
      REFRESH it_display1.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* Section 15: G32 augru cleanup (YRG011:2398-2406)
*----------------------------------------------------------------------*
  LOOP AT it_display1 ASSIGNING <gfs_display1_d>.
    IF <gfs_display1_d>-augru = 'G32'.
      CLEAR: <gfs_display1_d>-totalnic1,
             <gfs_display1_d>-totalpic,
             <gfs_display1_d>-totalpic1,
             <gfs_display1_d>-totalnic.
    ENDIF.
  ENDLOOP.

  SORT it_display1 BY vbeln ASCENDING.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form post_imbalance_data
*& Replicated from YRG011 user_command SUBMIT case for r_d5 path
*& Assumes fetch_imbalance_data was already called
*&---------------------------------------------------------------------*
FORM post_imbalance_data.

* Local variables for posting
  DATA: lt_yrva_zcontrcls TYPE TABLE OF yrva_zcontrcls,
        lw_yrva_zcontrcls TYPE yrva_zcontrcls,
        lv_vbeln          TYPE vbeln_nach,
        lt_vbfa           TYPE TABLE OF vbfa,
        lt_vbfa1          TYPE TABLE OF vbfa,
        lt_vbrk           TYPE STANDARD TABLE OF vbrk,
        ls_vbrk           TYPE vbrk,
        lw_vbfa           TYPE vbfa,
        lv_msg            TYPE string,
        lv_diff           TYPE menge_d,
        l_tabix           TYPE sy-tabix,
        l_sr              TYPE ysrno.

  DATA: lt_display TYPE TABLE OF ty_display_d,
        ls_display TYPE ty_display_d,
        msg        TYPE char64,
        msg1       TYPE char200,
        lt_gt1     TYPE TABLE OF vbak,
        ls_gt1     TYPE vbak,
        ret        TYPE char64.

  TYPES: BEGIN OF lty_fin,
           locid TYPE oij_locid,
           kunnr TYPE kunnr,
           dat   TYPE datum,
         END OF lty_fin.
  DATA: lt_fin TYPE TABLE OF lty_fin,
        ls_fin TYPE lty_fin.

  DATA: lt_oijrra TYPE TABLE OF oijrra,
        ls_oijrra TYPE oijrra,
        lv_ind(1).

  TYPES: BEGIN OF lty_esp1_msg,
           msgty  TYPE sy-msgty,
           msgv1  TYPE char200,
           lineno LIKE mesg-zeile,
         END OF lty_esp1_msg.
  DATA: lt_esp1_msg TYPE TABLE OF lty_esp1_msg,
        ls_esp1_msg TYPE lty_esp1_msg,
        lv_lineno   TYPE mesg-zeile.

  DATA: lt_nom       TYPE yrxt_nom,
        indicator    TYPE char1,
        mess         TYPE char64,
        lv_non_gail  TYPE c,
        pipeline     TYPE char64,
        et_nom       TYPE yrxt_nom.

  DATA: lt_yrg_chg_ovrrun TYPE TABLE OF yrg_chg_ovrrun,
        lw_yrg_chg_ovrrun TYPE yrg_chg_ovrrun.

  DATA: lv_tsyst     TYPE oij_tsyst,
        lv_addn      TYPE adrnr,
        lv_street    TYPE ad_street.

  DATA: lw_tsyt TYPE yrga_me_chk_tsyt.

*----------------------------------------------------------------------*
* Step 1: Mark all entries as selected
*----------------------------------------------------------------------*
  LOOP AT it_display1 ASSIGNING <gfs_display1_d>.
    <gfs_display1_d>-sel = 'X'.
  ENDLOOP.

*----------------------------------------------------------------------*
* Step 2: Fetch GTA imbalance shift data
*----------------------------------------------------------------------*
  IF it_display1 IS NOT INITIAL.
    SELECT * FROM yrva_gta_imb_sft
      INTO TABLE gt_gta_imb_sft_d
      FOR ALL ENTRIES IN it_display1
      WHERE contract_from EQ it_display1-vbeln.
    IF sy-subrc IS NOT INITIAL.
      CLEAR gt_gta_imb_sft_d.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* Step 3: Daily imbalance validation
*----------------------------------------------------------------------*
  LOOP AT g_it_display_d INTO g_wa_display_loc_d.
    lv_diff = g_wa_display_loc_d-del_mbg - g_wa_display_loc_d-redel_mbg.
    IF lv_diff NE g_wa_display_loc_d-daily_dds.
      RETURN. "Silent return - no interactive error in auto-posting
    ENDIF.
    CLEAR: g_wa_display_loc_d, lv_diff.
  ENDLOOP.

*----------------------------------------------------------------------*
* Step 4: Overrun data refresh
*----------------------------------------------------------------------*
  IF g_it_display_d IS NOT INITIAL.
    SELECT * FROM yrg_chg_ovrrun
      INTO TABLE lt_yrg_chg_ovrrun
      FOR ALL ENTRIES IN g_it_display_d
      WHERE kunnr EQ g_it_display_d-kunnr
        AND vbeln EQ g_it_display_d-vbeln
        AND begda EQ s_date-low
        AND endda EQ s_date-high.
    IF sy-subrc EQ 0.
      LOOP AT g_it_display_d INTO g_wa_display_loc_d.
        READ TABLE lt_yrg_chg_ovrrun INTO lw_yrg_chg_ovrrun
          WITH KEY kunnr = g_wa_display_loc_d-kunnr
                   vbeln = g_wa_display_loc_d-vbeln.
        IF sy-subrc EQ 0.
          g_wa_display_loc_d-vbeln_c = lw_yrg_chg_ovrrun-vbeln1.
          MODIFY g_it_display_d FROM g_wa_display_loc_d.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* Step 5: Pipeline/OD validation
*----------------------------------------------------------------------*
  SELECT * FROM vbak INTO TABLE lt_gt1
    FOR ALL ENTRIES IN g_it_display_d
    WHERE vbeln = g_it_display_d-vbeln.
  IF sy-subrc = 0.
    SELECT * FROM oijrra INTO TABLE lt_oijrra
      FOR ALL ENTRIES IN lt_gt1
      WHERE cparid = lt_gt1-kunnr AND tsyst LIKE 'NGPL%' AND delind = ''.
    LOOP AT g_it_display_d INTO g_wa_display_loc_d.
      ls_fin-kunnr = g_wa_display_loc_d-kunnr.
      READ TABLE lt_gt1 INTO ls_gt1 WITH KEY vbeln = g_wa_display_loc_d-vbeln.
      IF sy-subrc = 0 AND ls_gt1-auart = 'ZGT1'.
        ls_fin-dat = g_wa_display_loc_d-date.
        READ TABLE lt_oijrra INTO ls_oijrra WITH KEY cparid = ls_gt1-kunnr.
        IF sy-subrc = 0.
          ls_fin-locid = ls_oijrra-locid.
          APPEND ls_fin TO lt_fin.
        ENDIF.
      ENDIF.
      CLEAR g_wa_display_loc_d.
    ENDLOOP.
  ENDIF.
  SORT lt_fin BY dat locid kunnr.
  DELETE ADJACENT DUPLICATES FROM lt_fin.

  LOOP AT lt_fin INTO ls_fin.
    CLEAR: indicator, mess, pipeline, lt_nom, lv_non_gail, msg1.
    CALL FUNCTION 'YRX_CHK_NG_TKT_STATUS'
      EXPORTING
        gas_day        = ls_fin-dat
        locid          = ls_fin-locid
      IMPORTING
        indicator      = indicator
        msg            = mess
        pipeline       = pipeline
      TABLES
        et_nominations = lt_nom.

    IF pipeline IS INITIAL.
      CLEAR: lv_tsyst, lw_tsyt.
      SELECT tsyst FROM oijrra INTO lv_tsyst UP TO 1 ROWS
        WHERE locid = ls_fin-locid AND tsyst LIKE 'NGPL%' AND delind NE 'X'
        ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF lv_tsyst IS NOT INITIAL.
        SELECT SINGLE * FROM yrga_me_chk_tsyt INTO lw_tsyt WHERE tsyst = lv_tsyst.
      ENDIF.
      IF lw_tsyt IS NOT INITIAL.
        CALL FUNCTION 'YRX_CHK_ME_OD1'
          EXPORTING
            step       = 'ME'
            locid      = ls_fin-locid
            gday       = ls_fin-dat
          IMPORTING
            return_msg = msg
            ind        = lv_ind.
      ELSEIF lw_tsyt IS INITIAL.
        lv_ind = 'S'.
      ENDIF.
      CLEAR: lv_tsyst, lw_tsyt.

      IF lv_ind = 'S'.
        CALL FUNCTION 'YRX_CHK_ME_OD1'
          EXPORTING
            step       = 'OD'
            locid      = ls_fin-locid
            gday       = ls_fin-dat
          IMPORTING
            return_msg = msg
            ind        = lv_ind.
        IF lv_ind = 'E'.
          lv_lineno = lv_lineno + 1.
          ls_esp1_msg-msgty = 'E'.
          ls_esp1_msg-msgv1 = msg.
          ls_esp1_msg-lineno = lv_lineno.
          APPEND ls_esp1_msg TO lt_esp1_msg.
        ENDIF.
      ELSE.
        lv_lineno = lv_lineno + 1.
        ls_esp1_msg-msgty = 'E'.
        ls_esp1_msg-msgv1 = msg.
        ls_esp1_msg-lineno = lv_lineno.
        APPEND ls_esp1_msg TO lt_esp1_msg.
      ENDIF.
      CLEAR: lv_ind, msg, ls_display.

    ELSE.
      CALL FUNCTION 'YRX_CHK_OD_CUST'
        EXPORTING
          step       = 'OD'
          customer   = ls_fin-kunnr
          gday       = ls_fin-dat
        IMPORTING
          return_msg = msg1
          ind        = lv_ind.
      IF lv_ind = 'E'.
        lv_lineno = lv_lineno + 1.
        ls_esp1_msg-msgty = 'E'.
        ls_esp1_msg-msgv1 = msg1.
        ls_esp1_msg-lineno = lv_lineno.
        APPEND ls_esp1_msg TO lt_esp1_msg.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lt_esp1_msg IS INITIAL.
    LOOP AT lt_fin INTO ls_fin.
      CLEAR: lv_addn, lv_street.
      SELECT SINGLE addrnum FROM oifspbl INTO lv_addn WHERE pblnr = ls_fin-locid.
      SELECT * FROM adrc UP TO 1 ROWS WHERE addrnumber = lv_addn AND street = 'NON-GAIL' ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc = 0.
        lv_street = 'NON-GAIL'.
      ENDIF.

      IF lv_street NE 'NON-GAIL'.
        CALL FUNCTION 'YRX_CHK_NG_TKT_STATUS'
          EXPORTING
            gas_day    = ls_fin-dat
            locid      = ls_fin-locid
            detail_chk = 'X'
          IMPORTING
            indicator  = lv_ind
            msg        = msg
            pipeline   = pipeline
          TABLES
            et_nominations = et_nom.
        IF lv_ind = '1' OR lv_ind = '3' OR lv_ind = '4'.
          lv_lineno = lv_lineno + 1.
          ls_esp1_msg-msgty = 'E'.
          ls_esp1_msg-msgv1 = msg.
          ls_esp1_msg-lineno = lv_lineno.
          APPEND ls_esp1_msg TO lt_esp1_msg.
        ENDIF.
        CLEAR: ls_esp1_msg, ls_fin.
      ELSEIF lv_street = 'NON-GAIL'.
        CLEAR: msg1.
        CALL FUNCTION 'YRX_CHK_CUST_COMP_TKT_STATUS'
          EXPORTING
            i_kunnr         = ls_fin-kunnr
            i_date          = ls_fin-dat
            ticket_complete = 'X'
          IMPORTING
            indicator       = lv_ind
            msg             = msg1.
        IF lv_ind = '3'.
          lv_lineno = lv_lineno + 1.
          ls_esp1_msg-msgty = 'E'.
          ls_esp1_msg-msgv1 = msg1.
          ls_esp1_msg-lineno = lv_lineno.
          APPEND ls_esp1_msg TO lt_esp1_msg.
        ENDIF.
      ENDIF.
      CLEAR: ls_fin.
    ENDLOOP.
  ENDIF.

* If any errors found, skip posting silently (auto-posting context)
  IF lt_esp1_msg IS NOT INITIAL.
    READ TABLE lt_esp1_msg TRANSPORTING NO FIELDS WITH KEY msgty = 'E'.
    IF sy-subrc IS INITIAL.
      RETURN.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* Step 6: Pricing data fetch
*----------------------------------------------------------------------*
  IF it_display1[] IS NOT INITIAL.
    SELECT * FROM a978 INTO CORRESPONDING FIELDS OF TABLE gt_978_d
      FOR ALL ENTRIES IN it_display1
      WHERE oicontnr EQ it_display1-vbeln
        AND auart_sd = 'ZNT3'
        AND kschl EQ 'ZPR3'
        AND datab LE s_date-low
        AND datbi GE s_date-high.
    IF sy-subrc EQ 0.
      SELECT * FROM konp INTO CORRESPONDING FIELDS OF TABLE gt_konp_d
        FOR ALL ENTRIES IN gt_978_d
        WHERE knumh = gt_978_d-knumh.
    ENDIF.

    SELECT vbeln matnr INTO TABLE gt_matnr_d FROM vbap
      FOR ALL ENTRIES IN it_display1
      WHERE vbeln EQ it_display1-vbeln.
    LOOP AT gt_matnr_d.
      READ TABLE it_display1 INTO wa_display1 WITH KEY vbeln = gt_matnr_d-vbeln.
      IF sy-subrc EQ 0.
        gt_matnr_d-kunnr = wa_display1-kunnr.
        MODIFY gt_matnr_d.
      ENDIF.
    ENDLOOP.
    IF gt_matnr_d[] IS NOT INITIAL.
      SELECT * FROM a305
        INTO CORRESPONDING FIELDS OF TABLE gt_a305_d
        FOR ALL ENTRIES IN gt_matnr_d
        WHERE kunnr EQ gt_matnr_d-kunnr
          AND kschl EQ 'ZPR3'
          AND datab LE s_date-low
          AND datbi GE s_date-high
          AND matnr EQ gt_matnr_d-matnr.
      IF sy-subrc EQ 0.
        SELECT * FROM konp APPENDING CORRESPONDING FIELDS OF TABLE gt_konp_d
          FOR ALL ENTRIES IN gt_a305_d
          WHERE knumh = gt_a305_d-knumh.
      ENDIF.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* Step 7: R_D5 Master contract posting (Z01)
*----------------------------------------------------------------------*
  READ TABLE gt_vbak1_d INTO gw_vbak1_d WITH KEY abrvw = 'Z01'.
  IF sy-subrc EQ 0.
    READ TABLE it_display1 INTO wa_display1 WITH KEY vbeln = gw_vbak1_d-vbeln.
    IF sy-subrc EQ 0.
*     Get Master Contract
      READ TABLE it_display1 INTO gw_display12_d WITH KEY vbeln = wa_display1-vbeln_grp.
      IF sy-subrc EQ 0.
*       Skip check_imbalance_posting popup in YRGR102/batch context

*       Create new SO if needed
        IF ( gw_display12_d-totalpic > 0 OR gw_display12_d-totalnic < 0 ) AND gw_display12_d-vbeln1 IS INITIAL.
          CLEAR: g_es_oikimport_d, g_ls_oikload_d, g_ls_oikexport_d.
          REFRESH g_lt_oikload_d.

          g_es_oikimport_d-applic = 3.
          g_es_oikimport_d-fumode = 1.
          g_es_oikimport_d-lidfuncnam = 'OIK_SD_CALLOFF_CREATE'.
          g_es_oikimport_d-vbeln = wa_display1-vbeln.
          g_es_oikimport_d-auart = 'ZNT7'.
          g_es_oikimport_d-lisof = 'X'.
          g_ls_oikload_d-doctype_s = 'G'.
          g_ls_oikload_d-docno_s = wa_display1-vbeln.
          g_ls_oikload_d-kwmeng = 1000.
          g_ls_oikload_d-docno = wa_display1-vbeln.
          g_ls_oikload_d-itemno = '0000000010'.
          g_ls_oikload_d-doctype = 'G'.
          g_ls_oikload_d-auart = 'ZNT7'.

          READ TABLE gt_oij_el_cp_layt_d INTO gw_oij_el_cp_layt_d WITH KEY vbeln = wa_display1-vbeln.
          IF sy-subrc = 0.
            g_ls_oikload_d-vrkme = gw_oij_el_cp_layt_d-mdq_uom.
          ENDIF.
          APPEND g_ls_oikload_d TO g_lt_oikload_d.

          CALL FUNCTION 'OIK_SD_CALLOFF_CREATE'
            EXPORTING
              i_oikimport = g_es_oikimport_d
            IMPORTING
              e_oikexport = g_ls_oikexport_d
            TABLES
              t_oikload   = g_lt_oikload_d.

          IF g_ls_oikexport_d-msgty <> 'E'.
            wa_display1-vbeln1 = g_ls_oikexport_d-kdauf.
            MODIFY TABLE it_display1 FROM wa_display1.
            wa_display1-totalpic = gw_display12_d-totalpic.
            wa_display1-totalnic = gw_display12_d-totalnic.
            wa_display1-totalpic1 = gw_display12_d-totalpic1.
            wa_display1-totalnic1 = gw_display12_d-totalnic1.
            PERFORM additional_qty_local USING wa_display1.
            PERFORM change_so_local USING wa_display1-vbeln1.
            LOOP AT it_display1 INTO wa_display1 WHERE sel = 'X' AND ( vbeln = gw_display12_d-vbeln OR vbeln_grp EQ gw_display12_d-vbeln ).
              wa_display1-vbeln1 = g_ls_oikexport_d-kdauf.
              CLEAR wa_display1-sel.
              MODIFY it_display1 FROM wa_display1.
            ENDLOOP.
          ENDIF. "msgty <> E - suppress error message in auto-posting

        ELSE. "SO already exists or no imbalance needing new SO
          REFRESH: lt_yrva_zcontrcls, lt_vbfa.
          CLEAR: lw_yrva_zcontrcls, lv_vbeln, lw_vbfa.
          CLEAR: lt_vbfa[], lt_vbrk[], lv_vbeln.

          SELECT * FROM vbfa INTO TABLE lt_vbfa
            WHERE vbelv = gw_display12_d-vbeln1
              AND ( vbtyp_n = 'P' OR vbtyp_n = 'M' ).
          IF sy-subrc = 0.
            SELECT * FROM vbrk INTO TABLE lt_vbrk
              FOR ALL ENTRIES IN lt_vbfa
              WHERE vbeln = lt_vbfa-vbeln
                AND fksto NE 'X'.
            IF sy-subrc EQ 0.
              READ TABLE lt_vbrk INTO ls_vbrk INDEX 1.
              IF sy-subrc EQ 0.
                lv_vbeln = ls_vbrk-vbeln.
              ENDIF.
            ENDIF.
          ENDIF.

          IF lv_vbeln IS NOT INITIAL.
*           Cannot modify - invoice already generated. Silent skip in auto-posting.
          ELSE.
*           Update Tables and SO
            IF gw_display12_d-vbeln1 IS NOT INITIAL.
              wa_display1-vbeln1 = gw_display12_d-vbeln1.
              CLEAR wa_display1-sel.
              MODIFY TABLE it_display1 FROM wa_display1 TRANSPORTING sel vbeln1.
              wa_display1-totalpic = gw_display12_d-totalpic.
              wa_display1-totalnic = gw_display12_d-totalnic.
              wa_display1-totalpic1 = gw_display12_d-totalpic1.
              wa_display1-totalnic1 = gw_display12_d-totalnic1.
              PERFORM additional_qty_local USING wa_display1.
              PERFORM change_so_local USING wa_display1-vbeln1.
              LOOP AT it_display1 INTO wa_display1 WHERE sel = 'X' AND ( vbeln = gw_display12_d-vbeln OR vbeln_grp EQ gw_display12_d-vbeln ).
                CLEAR wa_display1-sel.
                MODIFY it_display1 FROM wa_display1.
              ENDLOOP.
            ENDIF.

*           If +ive and -ive imb have become zero then delete already created SO
            IF ( gw_display12_d-totalpic IS INITIAL AND gw_display12_d-totalnic IS INITIAL ) AND gw_display12_d-vbeln1 IS NOT INITIAL.
              g_salesdocument_d = gw_display12_d-vbeln1.
              g_order_header_inx_d-updateflag = 'D'.
              CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
                EXPORTING
                  salesdocument    = g_salesdocument_d
                  order_header_inx = g_order_header_inx_d
                TABLES
                  return           = gt_return_d.
              READ TABLE gt_return_d TRANSPORTING NO FIELDS WITH KEY type = 'E'.
              IF sy-subrc NE 0.
                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                  EXPORTING
                    wait = 'X'.
                IF sy-subrc EQ 0.
                  LOOP AT it_display1 ASSIGNING <gfs_display1_d>.
                    CLEAR <gfs_display1_d>-vbeln1.
                  ENDLOOP.
                ENDIF.
              ELSE.
                CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        LOOP AT it_display1 INTO wa_display1 WHERE sel = 'X' AND ( vbeln = gw_display12_d-vbeln OR vbeln_grp EQ gw_display12_d-vbeln ).
          CLEAR wa_display1-sel.
          MODIFY it_display1 FROM wa_display1.
        ENDLOOP.

*       Create entry in imbalance table for processed contracts
        LOOP AT it_display1 INTO wa_display1 WHERE sel NE 'X'.
          PERFORM entry_imb_table_local USING wa_display1.
        ENDLOOP.

      ELSE. "Master contract not found in it_display1
*       Suppress error message in auto-posting context
      ENDIF.
    ELSE. "Z01 contract not found in it_display1
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* Step 8: Check previous postings for individual contracts
*----------------------------------------------------------------------*
  CLEAR g_futher_processing_d.
  LOOP AT it_display1 INTO wa_display1 WHERE sel = 'X'.
    CLEAR g_exit_processing_d.
*   Suppress check_imbalance_posting popup in YRGR102/batch context
  ENDLOOP.

*----------------------------------------------------------------------*
* Step 9: Individual contract posting loop
*----------------------------------------------------------------------*
  LOOP AT it_display1 INTO wa_display1 WHERE sel = 'X'.

    IF wa_display1-vbeln1 IS INITIAL AND ( wa_display1-totalpic > 0 OR wa_display1-totalnic < 0 ).
      CLEAR: gw_display11_d, gw_vbak1_d, gw_display12_d.
      CLEAR: g_es_oikimport_d, g_ls_oikload_d, g_ls_oikexport_d.
      REFRESH g_lt_oikload_d.

      g_es_oikimport_d-applic = 3.
      g_es_oikimport_d-fumode = 1.
      g_es_oikimport_d-lidfuncnam = 'OIK_SD_CALLOFF_CREATE'.
      g_es_oikimport_d-vbeln = wa_display1-vbeln.
      g_es_oikimport_d-auart = 'ZNT7'.
      g_es_oikimport_d-lisof = 'X'.
      g_ls_oikload_d-doctype_s = 'G'.
      g_ls_oikload_d-docno_s = wa_display1-vbeln.
      g_ls_oikload_d-kwmeng = 1000.
      g_ls_oikload_d-docno = wa_display1-vbeln.
      g_ls_oikload_d-itemno = '0000000010'.
      g_ls_oikload_d-doctype = 'G'.
      g_ls_oikload_d-auart = 'ZNT7'.

      READ TABLE gt_oij_el_cp_layt_d INTO gw_oij_el_cp_layt_d WITH KEY vbeln = wa_display1-vbeln.
      IF sy-subrc = 0.
        g_ls_oikload_d-vrkme = gw_oij_el_cp_layt_d-mdq_uom.
      ENDIF.
      APPEND g_ls_oikload_d TO g_lt_oikload_d.

      CALL FUNCTION 'OIK_SD_CALLOFF_CREATE'
        EXPORTING
          i_oikimport = g_es_oikimport_d
        IMPORTING
          e_oikexport = g_ls_oikexport_d
        TABLES
          t_oikload   = g_lt_oikload_d.

      IF g_ls_oikexport_d-msgty <> 'E'.
        wa_display1-vbeln1 = g_ls_oikexport_d-kdauf.
        MODIFY it_display1 FROM wa_display1.
        PERFORM additional_qty_local USING wa_display1.
        PERFORM change_so_local USING wa_display1-vbeln1.
        PERFORM entry_imb_table_local USING wa_display1.
      ENDIF. "Suppress error message in auto-posting context

    ELSE.
      REFRESH: lt_yrva_zcontrcls, lt_vbfa.
      CLEAR: lw_yrva_zcontrcls, lv_vbeln, lw_vbfa.
      CLEAR: lt_vbfa[], lt_vbrk[], lv_vbeln.

      SELECT * FROM vbfa INTO TABLE lt_vbfa
        WHERE vbelv = wa_display1-vbeln1
          AND ( vbtyp_n = 'P' OR vbtyp_n = 'M' ).
      IF sy-subrc = 0.
        SELECT * FROM vbrk INTO TABLE lt_vbrk
          FOR ALL ENTRIES IN lt_vbfa
          WHERE vbeln = lt_vbfa-vbeln
            AND fksto NE 'X'.
        IF sy-subrc EQ 0.
          READ TABLE lt_vbrk INTO ls_vbrk INDEX 1.
          IF sy-subrc EQ 0.
            lv_vbeln = ls_vbrk-vbeln.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lv_vbeln IS NOT INITIAL.
*       Cannot modify - invoice already generated. Silent skip in auto-posting.
      ELSE.
*       Update Tables and SO
        IF wa_display1-vbeln1 IS NOT INITIAL.
          PERFORM additional_qty_local USING wa_display1.
          PERFORM change_so_local USING wa_display1-vbeln1.
        ENDIF.

*       If +ive and -ive imb have become zero then delete already created SO
        IF ( wa_display1-totalpic IS INITIAL AND wa_display1-totalnic IS INITIAL ) AND wa_display1-vbeln1 IS NOT INITIAL.
          g_salesdocument_d = wa_display1-vbeln1.
          g_order_header_inx_d-updateflag = 'D'.
          CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
            EXPORTING
              salesdocument    = g_salesdocument_d
              order_header_inx = g_order_header_inx_d
            TABLES
              return           = gt_return_d.
          READ TABLE gt_return_d TRANSPORTING NO FIELDS WITH KEY type = 'E'.
          IF sy-subrc NE 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
            IF sy-subrc EQ 0.
              CLEAR wa_display1-vbeln1.
            ENDIF.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ENDIF.
        ENDIF.

        PERFORM entry_imb_table_local USING wa_display1.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
