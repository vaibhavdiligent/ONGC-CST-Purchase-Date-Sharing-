FUNCTION yrx_imb_settle_qty_fm.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ST_DATE) TYPE  OIJNOMI-IDATE
*"     VALUE(ED_DATE) TYPE  OIJNOMI-IDATE
*"     REFERENCE(PARTNR) TYPE  OIJNOMI-PARTNR OPTIONAL
*"     REFERENCE(VKBUR) TYPE  VBAK-VKBUR OPTIONAL
*"  TABLES
*"      LT_FINAL STRUCTURE  YRX_IMB_SETTLE_QTY
*"----------------------------------------------------------------------

  DATA : ls_final TYPE  yrx_imb_settle_qty.
  DATA : lv_begda   TYPE datum,
         lv_endda   TYPE datum,           " fix: was TYPE datu (typo)
         lv_fromdat TYPE datum.

  DATA : BEGIN OF s_date OCCURS 0,
           sign(1)   TYPE c,
           option(2) TYPE c,
           low       TYPE oijnomi-idate,
           high      TYPE oijnomi-idate,
         END OF s_date.
  s_date-sign   = 'I'.
  s_date-option = 'BT'.
  s_date-low    = st_date.
  s_date-high   = ed_date.
  APPEND s_date.

  SELECT *
    FROM oijnomi
    INTO TABLE @DATA(lt_oijnomi)
    WHERE idate  IN @s_date
      AND sityp  = 'ZD'
      AND docind = 'G'
      AND delind NE 'X'.

  IF lt_oijnomi[] IS NOT INITIAL.
    SELECT vbeln,
           vbegdat,
           venddat
      FROM veda
      INTO TABLE @DATA(lt_veda)
      FOR ALL ENTRIES IN @lt_oijnomi
      WHERE vbeln   EQ @lt_oijnomi-docnr
        AND venddat IN @s_date
        AND vposn   = 000000.

    SORT lt_oijnomi BY docnr.
    DELETE ADJACENT DUPLICATES FROM lt_oijnomi COMPARING docnr.
    SORT lt_veda BY vbeln.

    IF lt_veda[] IS NOT INITIAL.

      SELECT *
        FROM yrva_gta_imb_sft
        INTO TABLE @DATA(lt_gta_imb)
        FOR ALL ENTRIES IN @lt_veda
        WHERE contract_from = @lt_veda-vbeln.

      SORT lt_veda    BY vbeln.
      SORT lt_gta_imb BY contract_from.
      LOOP AT lt_gta_imb INTO DATA(ls_gta_imb).
        DELETE lt_veda WHERE vbeln = ls_gta_imb-contract_from.
        CLEAR ls_gta_imb.
      ENDLOOP.

      SELECT *
        FROM vbak
        INTO TABLE @DATA(lt_vbak)
        FOR ALL ENTRIES IN @lt_veda
        WHERE vbeln = @lt_veda-vbeln.

      LOOP AT lt_vbak INTO DATA(ls_vbak).
        IF ls_vbak-vbeln_grp IS NOT INITIAL.
          DELETE lt_veda WHERE vbeln = ls_vbak-vbeln.
          CLEAR ls_vbak.
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDIF.

  LOOP AT lt_veda INTO DATA(ls_veda).
    " Determine the FN period in which the contract end date (venddat) falls
    CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
      EXPORTING
        iv_date             = ls_veda-venddat
      IMPORTING
        ev_month_begin_date = lv_begda
        ev_month_end_date   = lv_endda.

    IF '01' <= ls_veda-venddat+6(2) AND ls_veda-venddat+6(2) <= '15'.
      CONCATENATE lv_endda+0(4) lv_endda+4(2) '01' INTO lv_fromdat.
    ELSEIF '16' <= ls_veda-venddat+6(2) AND ls_veda-venddat+6(2) <= lv_endda+6(2).
      CONCATENATE lv_endda+0(4) lv_endda+4(2) '16' INTO lv_fromdat.
    ENDIF.

** -> Begin of changes by  RITIKA on 04.11.2024 20:46:10 for ATC
*    SELECT SINGLE * FROM yrg_cumm_imb
*          INTO @DATA(ls_cumm_imb)
*         WHERE yy_contract = @ls_veda-vbeln
*        AND begda = @s_date-low
*        AND endda = @s_date-high.
** -> End of changes by  RITIKA on 04.11.2024 20:46:13 for ATC

    " SOC: Pass FN dates (lv_fromdat/venddat) instead of report date range
    "      as per DOCX change: find FN period in which venddat falls
    SELECT *
      FROM yrg_cumm_imb
      INTO @DATA(ls_cumm_imb) UP TO 1 ROWS
      WHERE yy_contract = @ls_veda-vbeln
        AND begda       = @lv_fromdat
        AND endda       = @ls_veda-venddat
      ORDER BY PRIMARY KEY.
    ENDSELECT.
    " EOC

    IF ls_cumm_imb IS INITIAL.
      ls_final-stat = 'Not Posted'.
    ELSE.
      ls_final-stat = 'Posted'.
    ENDIF.

    IF ls_cumm_imb-yy_oij_cumimb <= '0.00'.
      ls_final-ne_imbal = ls_cumm_imb-yy_oij_cumimb.
    ELSE.
      ls_final-po_imbal = ls_cumm_imb-yy_oij_cumimb.
    ENDIF.

    ls_final-vbegdat = ls_veda-vbegdat.
    ls_final-venddat = ls_veda-venddat.
    READ TABLE lt_oijnomi INTO DATA(ls_oijnomi) WITH KEY docnr = ls_veda-vbeln.
    IF sy-subrc EQ 0.
      ls_final-docnr  = ls_oijnomi-docnr.
      ls_final-matnr  = ls_oijnomi-matnr_i.
      ls_final-partnr = ls_oijnomi-partnr.
      ls_final-locid  = ls_oijnomi-locid.
    ENDIF.

    SELECT SINGLE vkbur
      FROM vbak
      INTO ls_final-vkbur
      WHERE vbeln = ls_veda-vbeln.

    APPEND ls_final TO lt_final.
    CLEAR : ls_oijnomi,
            lv_endda,
            lv_begda,
            ls_veda,
            lv_fromdat,
            ls_cumm_imb,
            ls_final.
  ENDLOOP.

  " SOC: Delete only Posted entries with zero imbalance (not Not Posted)
  "      as per DOCX change: before displaying delete 0 imbalance + status=Posted
  DELETE lt_final WHERE po_imbal EQ '0.00'
                    AND ne_imbal EQ '0.00'
                    AND stat     EQ 'Posted'.
  " EOC

ENDFUNCTION.
