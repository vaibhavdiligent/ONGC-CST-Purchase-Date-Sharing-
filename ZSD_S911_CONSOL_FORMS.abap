*&---------------------------------------------------------------------*
*&  Include           ZSD_S911_CONSOL_FORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Title             : Midas 3 ( BU P&L Automation )
*& Program           : ZSD_S911_CONSOL
*& Tcode             : ZSD_S911_CONSOL
*& Author            : Pooja Adupala
*& Functional Design : Shashank Sharma
*& Technical Design  : Pooja Adupala
*& Creation Date     : 21.03.2022
*& Transport Request : CRDK982307
*& Change request    : 8000007907
*& Description       : To summerise data from table ZSD_S911_LINE_IT
*&---------------------------------------------------------------------*
*& Modification Information
*& Date            : 02.11.2026
*& Description     : S/4HANA CRS - TSV_TNEW_PAGE_ALLOC_FAILED (job
*&                   ZSD_S911_CONSOL 11022100, period 12.2025,
*&                   1,505,481 line items x 4,426 bytes = 6.7 GB).
*&                   The old code loaded the whole month into TA_ZSD_S911
*&                   and copied it five times (ta_zsd_s911_saknr,
*&                   ta_zsd_s911_werks, ta_zsd_s911_mat14, ta_s911_rx_prov,
*&                   ta_s911_exp_inc) -> > 14 GB session quota.
*&                   New flow (main program unchanged):
*&                   GET_DATA  : lookups from DISTINCT keys (no full load),
*&                               ranges from SETLEAF (was in FINAL_TAB),
*&                               then ZSD_S911_LINE_IT read with a cursor
*&                               in packages of 50,000 rows; every row is
*&                               summarised (COLLECT into TA_FINAL) by
*&                               FORM fill_final_row - the identical
*&                               tagging logic that was duplicated three
*&                               times (RX provision / export incentive /
*&                               revenue), now in one place.
*&                   FINAL_TAB : post-processing of TA_FINAL unchanged,
*&                               except the "material with one row" check
*&                               (was copy of TA_FINAL_MP per material,
*&                               O(n^2)) -> single sorted pass.
*&                   Result in ZSD_S911_CONSOL is identical: COLLECT is
*&                   order-independent and every row is classified by the
*&                   same GL-set rules as before (RX set first, then
*&                   export incentive set, rest revenue).
*&---------------------------------------------------------------------*

* package-level data (02.11.2026)
DATA: ta_eu_sales_type TYPE TABLE OF setleaf.          " was local in FINAL_TAB
TYPES: tt_line_it TYPE STANDARD TABLE OF zsd_s911_line_it WITH DEFAULT KEY.
CONSTANTS: co_pkg_size TYPE i VALUE 50000.
CONSTANTS: co_type_rx  TYPE c LENGTH 3 VALUE 'RX',
           co_type_exp TYPE c LENGTH 3 VALUE 'EXP',
           co_type_rev TYPE c LENGTH 3 VALUE 'REV'.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.
  DATA: lv_cursor TYPE cursor,
        lt_pkg    TYPE tt_line_it,
        lv_rows   TYPE i.

* 1. distinct keys of the period - small tables, replaces the full load
  SELECT DISTINCT sakn1
    FROM zsd_s911_line_it
    WHERE ent_tag IN @s_enttag
      AND spmon   =  @p_spmon
      AND ip_vend IN @s_ipvend
      AND werks   IN @s_werks
      AND vkorg   IN @s_vkorg
      AND zone_1  IN @s_zone
      AND sakn1   IN @s_sakn1
      AND matnr   IN @s_matnr
    INTO TABLE @DATA(lt_key_sakn1).

  IF lt_key_sakn1 IS INITIAL.
    MESSAGE text-001 TYPE co_s DISPLAY LIKE co_e.
    RETURN.
  ENDIF.

  SELECT DISTINCT werks
    FROM zsd_s911_line_it
    WHERE ent_tag IN @s_enttag
      AND spmon   =  @p_spmon
      AND ip_vend IN @s_ipvend
      AND werks   IN @s_werks
      AND vkorg   IN @s_vkorg
      AND zone_1  IN @s_zone
      AND sakn1   IN @s_sakn1
      AND matnr   IN @s_matnr
    INTO TABLE @DATA(lt_key_werks).

  SELECT DISTINCT remark
    FROM zsd_s911_line_it
    WHERE ent_tag IN @s_enttag
      AND spmon   =  @p_spmon
      AND ip_vend IN @s_ipvend
      AND werks   IN @s_werks
      AND vkorg   IN @s_vkorg
      AND zone_1  IN @s_zone
      AND sakn1   IN @s_sakn1
      AND matnr   IN @s_matnr
    INTO TABLE @DATA(lt_key_remark).

  "SOC by Pooja on 03.11.2022 CR#8000007907
  SELECT * FROM setleaf
    INTO TABLE ta_setleaf
    WHERE setname  IN ( co_gl_rx_prov,co_gl_exp_inc,co_plant_trd,co_mat_mor_em,co_mat_14,co_eu_sales_type ). "Added co_plant_trd,co_mat_mor_em,co_mat_14 by Gajanan - 13.11.2024 - CR#8000003569

***SOA by Gajanan - 11.11.2024 - CR#8000003569***
  "Fetch GLs
  SELECT * FROM zsd_s911_gl INTO TABLE ta_s911_gl
    FOR ALL ENTRIES IN lt_key_sakn1
     WHERE saknr = lt_key_sakn1-sakn1.

  IF NOT ta_s911_gl IS INITIAL.
    SORT ta_s911_gl BY saknr.
  ENDIF.

  "Fetch Plants
  IF lt_key_werks IS NOT INITIAL.
    SELECT * FROM zsd_s911_plant INTO TABLE ta_s911_plant
      FOR ALL ENTRIES IN lt_key_werks
       WHERE werks = lt_key_werks-werks.
  ENDIF.

  IF NOT ta_s911_plant IS INITIAL.
    SORT ta_s911_plant BY werks.
  ENDIF.

  "Fetch Remark
  IF lt_key_remark IS NOT INITIAL.
    SELECT * FROM zsd_plant_trd INTO TABLE ta_plant_trd
      FOR ALL ENTRIES IN lt_key_remark
       WHERE remark = lt_key_remark-remark.
  ENDIF.

  IF NOT ta_plant_trd IS INITIAL.
    SORT ta_plant_trd BY remark.
  ENDIF.

  "Fetch data from ZSD_MAN_PLANT
  SELECT * FROM zsd_man_plant INTO TABLE ta_man_plant
    WHERE spmon = p_spmon.

  IF NOT ta_man_plant IS INITIAL.
    SORT ta_man_plant BY spmon.
  ENDIF.
***EOA by Gajanan - 11.11.2024 - CR#8000003569***
  "EOC by Pooja on 03.11.2022 CR#8000007907

  FREE: lt_key_sakn1, lt_key_werks, lt_key_remark.

* 2. ranges from the sets (formerly first part of FINAL_TAB)
  PERFORM build_ranges.

* 3. summarise the period in packages (02.11.2026)
  OPEN CURSOR @lv_cursor FOR
    SELECT *
      FROM zsd_s911_line_it
      WHERE ent_tag IN @s_enttag
        AND spmon   =  @p_spmon
        AND ip_vend IN @s_ipvend "Added by Gajanan - 28.11.2024 - CR#8000003569
        AND werks   IN @s_werks  "Added by Gajanan - 28.11.2024 - CR#8000003569
        AND vkorg   IN @s_vkorg
        AND zone_1  IN @s_zone
        AND sakn1   IN @s_sakn1  "Added by Gajanan - 28.11.2024 - CR#8000003569
        AND matnr   IN @s_matnr. "Added by Gajanan - 28.11.2024 - CR#8000003569
  DO.
    FETCH NEXT CURSOR @lv_cursor INTO TABLE @lt_pkg PACKAGE SIZE @co_pkg_size.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    lv_rows = lv_rows + lines( lt_pkg ).
    PERFORM summarize_package USING lt_pkg.
    CLEAR lt_pkg.
  ENDDO.
  CLOSE CURSOR @lv_cursor.
  FREE lt_pkg.
* (no COMMIT WORK / dialog between OPEN and CLOSE - the cursor stays valid)
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILD_RANGES          (moved out of FINAL_TAB, 02.11.2026)
*&---------------------------------------------------------------------*
FORM build_ranges.
  "SOC by Pooja on 04.11.2022 CR#8000007907
  IF ta_setleaf IS NOT INITIAL.
    DATA(ta_setleaf_rx_prov) = ta_setleaf.
    DELETE ta_setleaf_rx_prov WHERE setname NE co_gl_rx_prov.

    IF ta_setleaf_rx_prov IS NOT INITIAL.
      LOOP AT ta_setleaf_rx_prov  INTO DATA(wa_setleaf).
        r_sakn1-sign = wa_setleaf-valsign.
        r_sakn1-option = wa_setleaf-valoption.
        r_sakn1-low = wa_setleaf-valfrom.
        r_sakn1-high = wa_setleaf-valto.
        APPEND r_sakn1.
      ENDLOOP.
    ENDIF.

    DATA(ta_setleaf_exp_inc) = ta_setleaf.
    DELETE ta_setleaf_exp_inc WHERE setname NE co_gl_exp_inc.

    IF ta_setleaf_exp_inc IS NOT INITIAL.
      LOOP AT ta_setleaf_exp_inc  INTO DATA(wa_setleaf_exp_inc).
        r_sakn1_exp_inc-sign = wa_setleaf_exp_inc-valsign.
        r_sakn1_exp_inc-option = wa_setleaf_exp_inc-valoption.
        r_sakn1_exp_inc-low = wa_setleaf_exp_inc-valfrom.
        r_sakn1_exp_inc-high = wa_setleaf_exp_inc-valto.
        APPEND r_sakn1_exp_inc.
      ENDLOOP.
    ENDIF.

***SOA by Gajanan - 09.02.2026 - CR#8000004821***
    ta_eu_sales_type = ta_setleaf.
    DELETE ta_eu_sales_type WHERE setname NE co_eu_sales_type.
***EOA by Gajanan - 09.02.2026 - CR#8000004821***

***SOA by Gajanan - 11.11.2024 - CR#8000003569***
    DATA(ta_mat_mor_em) = ta_setleaf.
    DELETE ta_mat_mor_em WHERE setname NE co_mat_mor_em.

    IF ta_mat_mor_em IS NOT INITIAL.
      LOOP AT ta_mat_mor_em  INTO DATA(wa_mat_mor_em).
        r_mat_mor_em-sign = wa_mat_mor_em-valsign.
        r_mat_mor_em-option = wa_mat_mor_em-valoption.
        r_mat_mor_em-low = wa_mat_mor_em-valfrom.
        r_mat_mor_em-high = wa_mat_mor_em-valto.
        APPEND r_mat_mor_em.
        CLEAR: wa_mat_mor_em.
      ENDLOOP.
    ENDIF.

    DATA(ta_mat_14) = ta_setleaf.
    DELETE ta_mat_14 WHERE setname NE co_mat_14.

    IF ta_mat_14 IS NOT INITIAL.
      LOOP AT ta_mat_14  INTO DATA(wa_mat_14).
        r_mat_14-sign = wa_mat_14-valsign.
        r_mat_14-option = wa_mat_14-valoption.
        r_mat_14-low = wa_mat_14-valfrom.
        r_mat_14-high = wa_mat_14-valto.
        APPEND r_mat_14.
        CLEAR: wa_mat_14.
      ENDLOOP.

      "Create range for MAT_14
*     02.11.2026: distinct materials read from the database instead of a
*     full copy of TA_ZSD_S911 (6.7 GB) + SORT + DELETE ADJACENT DUPLICATES
      SELECT DISTINCT matnr
        FROM zsd_s911_line_it
        WHERE ent_tag IN @s_enttag
          AND spmon   =  @p_spmon
          AND ip_vend IN @s_ipvend
          AND werks   IN @s_werks
          AND vkorg   IN @s_vkorg
          AND zone_1  IN @s_zone
          AND sakn1   IN @s_sakn1
          AND matnr   IN @s_matnr
        INTO TABLE @DATA(ta_zsd_s911_mat14).
      DELETE ta_zsd_s911_mat14 WHERE matnr CO '0123456789 '.

      IF NOT ta_zsd_s911_mat14 IS INITIAL.

        LOOP AT ta_zsd_s911_mat14 INTO DATA(wa_zsd_s911_mat14).
          LOOP AT r_mat_14.
            IF wa_zsd_s911_mat14-matnr CS r_mat_14-low.
              r_mat_14_fin-sign = 'I'.
              r_mat_14_fin-option = 'EQ'.
              r_mat_14_fin-low = wa_zsd_s911_mat14-matnr.
              r_mat_14_fin-high = wa_zsd_s911_mat14-matnr.
              APPEND r_mat_14_fin.

            ENDIF.
            CLEAR: r_mat_14,r_mat_14_fin.
          ENDLOOP.
          CLEAR: wa_zsd_s911_mat14.
        ENDLOOP.

      ENDIF.
      FREE ta_zsd_s911_mat14.
    ENDIF.
***EOA by Gajanan - 11.11.2024 - CR#8000003569***
  ENDIF.
  "EOC by Pooja on 04.11.2022 CR#8000007907
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SUMMARIZE_PACKAGE     (new 02.11.2026)
*&---------------------------------------------------------------------*
*  Classifies every line item of the package exactly as the three old
*  loops did: GL in RX_PROVISION_GL set -> RX provision, else GL in
*  EXPORT_INCENTIVES set -> export incentive, else revenue.
*----------------------------------------------------------------------*
FORM summarize_package USING it_pkg TYPE tt_line_it.
  LOOP AT it_pkg ASSIGNING FIELD-SYMBOL(<fs_zsd_s911>).
    IF r_sakn1[] IS NOT INITIAL AND <fs_zsd_s911>-sakn1 IN r_sakn1.
      PERFORM fill_final_row USING <fs_zsd_s911> co_type_rx.
    ELSEIF r_sakn1_exp_inc[] IS NOT INITIAL AND <fs_zsd_s911>-sakn1 IN r_sakn1_exp_inc.
      PERFORM fill_final_row USING <fs_zsd_s911> co_type_exp.
    ELSE.
      PERFORM fill_final_row USING <fs_zsd_s911> co_type_rev.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_FINAL_ROW        (new 02.11.2026)
*&---------------------------------------------------------------------*
*  One line item -> WA_FINAL -> COLLECT into TA_FINAL. Statement order
*  is kept exactly as in the former loops (the MOVE-CORRESPONDING after
*  the explicit assignments is intentional).
*----------------------------------------------------------------------*
FORM fill_final_row USING is_item TYPE zsd_s911_line_it
                          iv_type TYPE clike.
  DATA: lv_gl_flag TYPE abap_bool,
        lv_pkunag  TYPE string,
        lwa_eu_sales_type TYPE setleaf.

  wa_final-actualbudget = co_actual.
  wa_final-spmon = p_spmon.
  CASE iv_type.
    WHEN co_type_rx.
      wa_final-expenses = 'RX_provision_GL'.
    WHEN co_type_exp.
      wa_final-expenses = 'Export Incentive'.
    WHEN OTHERS.
      wa_final-expenses = co_revenue.
  ENDCASE.
  wa_final-insurance = is_item-umkzwi5.
  wa_final-commision = is_item-umkzwi6.
  wa_final-freight = is_item-skfbp.
  "Summarization logic for mat_cost_free_qty and mat_cost_bill_qty
  IF is_item-fkart = co_zm1 OR is_item-fkart = co_zmf2 OR is_item-fkart = co_zfoc.
    wa_final-mat_cost_free_qty = is_item-mat_cost_cc_curr.
  ELSE.
    wa_final-mat_cost_bill_qty = is_item-mat_cost_cc_curr.
  ENDIF.

  "Summarization logic for variable_cost
  wa_final-variable_cost = is_item-api_vc_cc_curr + is_item-for_vc_cc_curr.
  MOVE-CORRESPONDING is_item TO wa_final.
  CLEAR:wa_final-sales_cc_curr.
  IF iv_type = co_type_exp.
    wa_final-sales_cc_curr = is_item-sales_cc_curr.          " export incentive: always
  ELSEIF is_item-sakn1 NE co_sakn1."Summarization logic for sales_cc_curr
    wa_final-sales_cc_curr = is_item-sales_cc_curr.
  ENDIF.

***SOA by Gajanan - 11.11.2024 - CR#8000003569***
  IF is_item-ip_vend IS INITIAL.
    CLEAR: wa_final-werks.
  ELSEIF is_item-ip_vend+0(4) = 'PLNT'.
    wa_final-werks = is_item-ip_vend+4(4).
  ELSE.
    wa_final-werks = 'TRD'.
  ENDIF.

  lv_mat = | { wa_final-matnr ALPHA = OUT } |.
  CONDENSE lv_mat.
  TRANSLATE lv_mat TO UPPER CASE.
  lv_fkart = wa_final-fkart.
  TRANSLATE lv_fkart TO UPPER CASE.
  lv_remark = wa_final-remark.
  TRANSLATE lv_remark TO UPPER CASE.

  READ TABLE ta_s911_plant INTO wa_s911_plant WITH KEY werks = wa_final-werks
                                                       BINARY SEARCH.
  IF sy-subrc = 0.
    wa_final-fin_desc = wa_s911_plant-name1.
    wa_final-p_type = wa_s911_plant-p_type.
    wa_final-ort01 = wa_s911_plant-ort01.
  ENDIF.

  READ TABLE ta_s911_gl INTO wa_s911_gl WITH KEY saknr = wa_final-sakn1
                                                 BINARY SEARCH.
  IF sy-subrc = 0.
    TRANSLATE wa_s911_gl-remark TO UPPER CASE.
    IF wa_s911_gl-remark = 'INTER COMPANY PROVISIONS' OR wa_s911_gl-remark = 'INCOME FROM OPERATIONS'
    OR wa_s911_gl-remark = 'CASH INCENTIVES'.
      wa_final-fin_werks = 'Reco'.
      wa_final-basis     = '00. GL based - Inter company provisions, Income from operations, Cash Incentives'.
      lv_gl_flag = abap_true.
    ENDIF.
  ENDIF.

  IF lv_gl_flag IS INITIAL.

    IF wa_final-werks IS NOT INITIAL AND wa_final-cost_plant IS NOT INITIAL AND ( wa_final-werks = wa_final-cost_plant ).
      wa_final-fin_werks = wa_final-werks.
      wa_final-basis = '01. Manufacturing plant & costing plant same'.

    ELSEIF wa_final-werks = 'TRD' AND ( ta_plant_trd IS NOT INITIAL AND  line_exists( ta_plant_trd[ remark = wa_final-remark ] ) ).
      wa_final-fin_werks = 'TRD'.
      wa_final-basis = '02. Manufacturing plant TRD & Remraks are also Trading'.

    ELSEIF wa_final-remark = '05. DN/CN'.
      wa_final-fin_werks = wa_final-werks.
      wa_final-basis = '03. Manufacturing plant consider,  As costing plant DN/CN'.

    ELSEIF lv_remark = '01. MANUAL GL' OR wa_final-ent_tag = 'Manual'.
      wa_final-fin_werks = 'Reco'.
      wa_final-basis = '04. Reco items- Remarks Manual'.

    ELSEIF wa_final-remark CS 'Other Material' OR wa_final-remark CS 'Promotion'.
      wa_final-fin_werks = 'TRD'.
      wa_final-basis = '05. Remarks  - Other Material/ Promotional'.

    ELSEIF wa_final-zone_1 = 'South Africa' AND lv_fkart = 'CMM'.
      wa_final-fin_werks = '4111'.
      wa_final-basis = '06. South africa billing type - CMM,Mirr,Thir, Teva'.

    ELSEIF wa_final-zone_1 = 'South Africa' AND lv_fkart = 'MIRR'.
      wa_final-fin_werks = '4201'.
      wa_final-basis = '06. South africa billing type - CMM,Mirr,Thir, Teva'.

    ELSEIF wa_final-zone_1 = 'South Africa' AND lv_fkart = 'THIR'.
      wa_final-fin_werks = 'TRD'.
      wa_final-basis = '06. South africa billing type - CMM,Mirr,Thir, Teva'.

    ELSEIF wa_final-zone_1 = 'South Africa' AND lv_fkart = 'TEVA'.
      wa_final-fin_werks = 'TRD'.
      wa_final-basis = '06. South africa billing type - CMM,Mirr,Thir, Teva'.

    ELSEIF lv_mat+0(2) = '12'.
      wa_final-fin_werks = 'Reco'.
      wa_final-basis = '07. Scrap sales 12 series materials or Scrap GL'.

    ELSEIF wa_final-cost_plant = 'TRD' AND wa_final-werks IS INITIAL.
      wa_final-fin_werks = 'TRD'.
      wa_final-basis = '08. Costing plant TRD & Mfg Plant is blank'.

    ELSEIF wa_final-remark CS 'Single Plant' AND wa_final-werks IS INITIAL.
      wa_final-fin_werks = wa_final-cost_plant.
      wa_final-basis = '10. Remarks Single Plant & Mfg Plant is blank'.

    ELSEIF wa_final-werks IS NOT INITIAL AND wa_final-remark CS 'Multiple Plant'.
      wa_final-fin_werks = wa_final-werks.
      wa_final-basis = '11. Mfg plant <> blank, Remarks Multiple Plant, Company Code wise & Blank'.

    ELSEIF wa_final-fkart = 'ZM1' OR wa_final-fkart = 'ZMF2'.
      wa_final-fin_werks = 'Reco'.
      wa_final-basis = '12.  MMs Billing types - ZM1 ZMF2'.

    ELSEIF lv_mat+0(2) = '11' OR lv_mat+0(1) = '2'.
      wa_final-fin_werks = 'TRD'.
      wa_final-basis = '13.  Raw material / Packing material sold'.

    ELSEIF wa_final-matnr IS INITIAL OR ( r_mat_14_fin[] IS NOT INITIAL AND lv_mat IN r_mat_14_fin[] ).
      wa_final-fin_werks = 'Reco'.
      wa_final-basis = '14. Material code = Blank, Provision, Adjustment, Unallocated. MMs'.

    ELSEIF r_mat_mor_em[] IS NOT INITIAL AND wa_final-matnr IN r_mat_mor_em[] AND wa_final-vkorg = '6600'.
      wa_final-fin_werks = '6601'.
      wa_final-basis = '15. EM & moroco tagging'.

    ELSEIF wa_final-werks IS NOT INITIAL.
      wa_final-fin_werks = wa_final-werks.
      wa_final-basis = '16. Mfg Plant taken / Costing plant taken'.

    ELSEIF wa_final-cost_plant IS NOT INITIAL.
      wa_final-fin_werks = wa_final-cost_plant.
      wa_final-basis = '16. Mfg Plant taken / Costing plant taken'.

    ENDIF.

  ENDIF.
***EOA by Gajanan - 11.11.2024 - CR#8000003569***

***SOA by Gajanan - 11.12.2025 - CR#8000004699***
  IF wa_final-zone_1 = 'Europe'.
    lv_pkunag = |{ is_item-pkunag ALPHA = OUT }|.

    READ TABLE ta_eu_sales_type INTO lwa_eu_sales_type WITH KEY valfrom = lv_pkunag.
    IF sy-subrc = 0.
      wa_final-sales_grp = lwa_eu_sales_type-valto.
    ELSE.
      wa_final-sales_grp = 'Ex CMO'.
    ENDIF.
    CLEAR:lv_pkunag,lwa_eu_sales_type.
  ENDIF.
***EOA by Gajanan - 11.12.2025 - CR#8000004699***

  COLLECT  wa_final INTO ta_final.
  CLEAR:wa_final,wa_s911_plant,wa_s911_gl,lv_gl_flag,lv_mat,lv_fkart,lv_remark.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FINAL_TAB
*&---------------------------------------------------------------------*
*  02.11.2026: summarisation moved to GET_DATA (packages). This form now
*  holds only the post-processing of TA_FINAL (unchanged logic).
*----------------------------------------------------------------------*
FORM final_tab.
  CLEAR:wa_final.

  IF NOT ta_final IS INITIAL.

    DATA(ta_final_mp) = ta_final[].
    DELETE ta_final_mp WHERE basis <> '01. Manufacturing plant & costing plant same' AND
                             basis <> '02. Manufacturing plant TRD & Remraks are also Trading' AND
                             basis <> '08. Costing plant TRD & Mfg Plant is blank ' AND
                             basis <> '10. Remarks Single Plant & Mfg Plant is blank'.
    IF NOT ta_final_mp IS INITIAL.
*     02.11.2026: materials that occur exactly once in TA_FINAL_MP
*     (was: copy of TA_FINAL_MP + DELETE per material, O(n^2))
      TYPES: BEGIN OF lty_mat_cnt,
               matnr TYPE zsd_s911_consol-matnr,
               cnt   TYPE i,
             END OF lty_mat_cnt.
      DATA: lt_mat_cnt       TYPE SORTED TABLE OF lty_mat_cnt WITH UNIQUE KEY matnr,
            ta_final_temp_mp LIKE ta_final_mp,
            wa_final_temp_mp LIKE LINE OF ta_final_mp.
      FIELD-SYMBOLS <ls_mat_cnt> TYPE lty_mat_cnt.

      LOOP AT ta_final_mp INTO DATA(wa_final_mp).
        READ TABLE lt_mat_cnt ASSIGNING <ls_mat_cnt> WITH TABLE KEY matnr = wa_final_mp-matnr.
        IF sy-subrc = 0.
          <ls_mat_cnt>-cnt = <ls_mat_cnt>-cnt + 1.
        ELSE.
          INSERT VALUE #( matnr = wa_final_mp-matnr cnt = 1 ) INTO TABLE lt_mat_cnt.
        ENDIF.
      ENDLOOP.
      SORT ta_final_mp BY matnr.
      LOOP AT ta_final_mp INTO wa_final_mp.
        READ TABLE lt_mat_cnt ASSIGNING <ls_mat_cnt> WITH TABLE KEY matnr = wa_final_mp-matnr.
        IF sy-subrc = 0 AND <ls_mat_cnt>-cnt = 1.
          APPEND wa_final_mp TO ta_final_temp_mp.
        ENDIF.
      ENDLOOP.
      FREE lt_mat_cnt.
    ENDIF.

    DATA(ta_final_blank) = ta_final[].
    DELETE ta_final WHERE fin_werks IS INITIAL.
    DELETE ta_final_blank WHERE fin_werks IS NOT INITIAL.

    IF NOT ta_final_blank IS INITIAL.

      LOOP AT ta_final_blank INTO DATA(wa_final_blank).

        READ TABLE ta_final_temp_mp INTO wa_final_temp_mp WITH KEY matnr = wa_final_blank-matnr werks = wa_final_blank-werks.
        IF sy-subrc = 0.
          wa_final_blank-basis = '17. Mfg Plant based on Sales having tagging Point number 1 ,2, 8 & 10 Above'.
          wa_final_blank-fin_werks = wa_final_temp_mp-fin_werks.

        ELSE.

          READ TABLE ta_man_plant INTO wa_man_plant WITH KEY spmon = wa_final_blank-spmon
                                                             ent_tag = wa_final_blank-ent_tag
                                                             expenses = wa_final_blank-expenses
                                                             final_tag = wa_final_blank-final_tag
                                                             sales_grp = wa_final_blank-sales_grp
                                                             katr4 = wa_final_blank-katr4
                                                             zone_1 = wa_final_blank-zone_1
                                                             add_drill_down = wa_final_blank-add_drill_down
                                                             add_drill_down2 = wa_final_blank-add_drill_down2
                                                             vkorg = wa_final_blank-vkorg
                                                             werks = wa_final_blank-werks
                                                             cost_plant = wa_final_blank-cost_plant
                                                             matnr = wa_final_blank-matnr
                                                             fkart = wa_final_blank-fkart
                                                             sakn1 = wa_final_blank-sakn1.

          IF sy-subrc = 0.
            wa_final_blank-fin_werks = wa_man_plant-fin_werks.
            wa_final_blank-basis = '17.1 Manually updated the Mfg plant'.

          ELSEIF wa_final_blank-doc_cur GE 2500000.
            wa_final_blank-fin_werks = 'Reco'.
            wa_final_blank-basis = '18. Pending value more then +25 lakhs'.

          ELSEIF wa_final_blank-doc_cur LE -2500000.
            wa_final_blank-fin_werks = 'Reco'.
            wa_final_blank-basis = '19. Pending value more then -25 lakhs'.

          ELSEIF wa_final_blank-doc_cur LT 2500000 AND wa_final_blank-doc_cur GE 0.
            wa_final_blank-fin_werks = 'Reco'.
            wa_final_blank-basis = '20. Pending value Less then +25 lakhs'.

          ELSEIF wa_final_blank-doc_cur GT -2500000 AND wa_final_blank-doc_cur LT 0.
            wa_final_blank-fin_werks = 'Reco'.
            wa_final_blank-basis = '21. Pending value Less then -25 lakhs'.

*          ELSE.

*            READ TABLE ta_man_plant INTO wa_man_plant WITH KEY spmon = wa_final_blank-spmon
*                                                               ent_tag = wa_final_blank-ent_tag
*                                                               expenses = wa_final_blank-expenses
*                                                               final_tag = wa_final_blank-final_tag
*                                                               sales_grp = wa_final_blank-sales_grp
*                                                               katr4 = wa_final_blank-katr4
*                                                               zone_1 = wa_final_blank-zone_1
*                                                               add_drill_down = wa_final_blank-add_drill_down
*                                                               add_drill_down2 = wa_final_blank-add_drill_down2
*                                                               vkorg = wa_final_blank-vkorg
*                                                               werks = wa_final_blank-werks
*                                                               cost_plant = wa_final_blank-cost_plant
*                                                               matnr = wa_final_blank-matnr
*                                                               fkart = wa_final_blank-fkart
*                                                               sakn1 = wa_final_blank-sakn1.
*            IF sy-subrc = 0.
*              wa_final_blank-fin_werks = wa_man_plant-fin_werks.
*              wa_final_blank-basis = '22. Manually updated the Mfg plant'.
*            ENDIF.
          ENDIF.
        ENDIF.

        MODIFY ta_final_blank FROM wa_final_blank TRANSPORTING fin_werks basis.

        CLEAR: wa_final_blank,wa_final_temp_mp,wa_man_plant.
      ENDLOOP.

      IF NOT ta_final_blank IS INITIAL.
        APPEND LINES OF ta_final_blank TO ta_final.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.

FORM disp_alv.

  DATA : lwa_layout TYPE slis_layout_alv,
         lv_repid   TYPE sy-repid.

  PERFORM fill_fieldcat USING 'MANDT'        'TA_FINAL' '' '' 'Client'.
  PERFORM fill_fieldcat USING 'SPMON'        'TA_FINAL' '' '' 'Month/Year '.
  PERFORM fill_fieldcat USING 'ENT_TAG'        'TA_FINAL' '' '' 'Entry Tag'. "Added by Gajanan - 21.10.2024 - CR#8000003569
  PERFORM fill_fieldcat USING 'EXPENSES'        'TA_FINAL' '' '' 'Head of Expenses'.
  PERFORM fill_fieldcat USING 'FINAL_TAG'    'TA_FINAL' '' '' 'Detailed P&L'.
  PERFORM fill_fieldcat USING 'SALES_GRP'      'TA_FINAL' '' '' 'Sales Group'.
  PERFORM fill_fieldcat USING 'KATR4'       'TA_FINAL' '' '' 'Third Party / Inter company'.
  PERFORM fill_fieldcat USING 'ZONE_1' 'TA_FINAL' '' '' 'ZONE'.
  PERFORM fill_fieldcat USING 'ADD_DRILL_DOWN'  'TA_FINAL' '' '' 'Additional Drill down'.
  PERFORM fill_fieldcat USING 'ADD_DRILL_DOWN2'     'TA_FINAL' '' '' 'Additional Drill Down 2'.
  PERFORM fill_fieldcat USING 'VKORG'      'TA_FINAL' '' '' 'Sales Organization'.
  PERFORM fill_fieldcat USING 'WERKS'      'TA_FINAL' '' '' 'MFG Plant'.
  PERFORM fill_fieldcat USING 'COST_PLANT'       'TA_FINAL' '' '' 'Costing plant'.
  PERFORM fill_fieldcat USING 'MATNR'       'TA_FINAL' '' '' 'Material code'.
  PERFORM fill_fieldcat USING 'FKART'       'TA_FINAL' '' '' 'Billing Type'.
  PERFORM fill_fieldcat USING 'SAKN1'       'TA_FINAL' '' '' 'G/L Account Number'. "Added by Gajanan - 21.10.2024 - CR#8000003569
  PERFORM fill_fieldcat USING 'ACTUALBUDGET'        'TA_FINAL' '' '' 'Actual/Budget'. "Added by Gajanan - 21.10.2024 - CR#8000003569
  PERFORM fill_fieldcat USING 'FIN_DESC'        'TA_FINAL' '' '' 'Final Description'. "Added by Gajanan - 21.10.2024 - CR#8000003569
  PERFORM fill_fieldcat USING 'P_TYPE'        'TA_FINAL' '' '' 'Plant Type'. "Added by Gajanan - 21.10.2024 - CR#8000003569
  PERFORM fill_fieldcat USING 'ORT01'        'TA_FINAL' '' '' 'Location'. "Added by Gajanan - 21.10.2024 - CR#8000003569
  PERFORM fill_fieldcat USING 'SALES_CC_CURR'       'TA_FINAL' '' '' 'Sales CC Currency'.
  PERFORM fill_fieldcat USING 'MAT_COST_BILL_QTY'       'TA_FINAL' '' '' 'Mat cost for Billed Qty'.
  PERFORM fill_fieldcat USING 'MAT_COST_FREE_QTY'       'TA_FINAL' '' '' 'Mat cost for Free Qty'.
  PERFORM fill_fieldcat USING 'VARIABLE_COST'       'TA_FINAL' '' '' 'Variable Cost'.
  PERFORM fill_fieldcat USING 'INSURANCE'       'TA_FINAL' '' '' 'Insurance'.
  PERFORM fill_fieldcat USING 'COMMISION'       'TA_FINAL' '' '' 'Commision'.
  PERFORM fill_fieldcat USING 'FREIGHT'       'TA_FINAL' '' '' 'Freight'.
  "SOC by Pooja on 03.11.2022 CR#8000007907
  PERFORM fill_fieldcat USING 'MAT_COST_STAND'       'TA_FINAL' '' '' 'Material Cost At Standalone'.
  PERFORM fill_fieldcat USING 'UKURS_ALL'       'TA_FINAL' '' '' 'WT Avg TRD Rate All'.
  PERFORM fill_fieldcat USING 'UKURS_PGI'       'TA_FINAL' '' '' 'PGI WT Avg TRD Rate All'.
  PERFORM fill_fieldcat USING 'REMARK'       'TA_FINAL' '' '' 'Remark'. "Added by Gajanan - 21.10.2024 - CR#8000003569
  PERFORM fill_fieldcat USING 'FIN_WERKS'       'TA_FINAL' '' '' 'Final Mfg Plant'. "Added by Gajanan - 21.10.2024 - CR#8000003569
  PERFORM fill_fieldcat USING 'BASIS'       'TA_FINAL' '' '' 'Basis'. "Added by Gajanan - 21.10.2024 - CR#8000003569
  PERFORM fill_fieldcat USING 'B_FKIMG'       'TA_FINAL' '' '' 'Actual Invoiced Quantity'. "Added by Gajanan - 21.10.2024 - CR#8000003569
  PERFORM fill_fieldcat USING 'MAT_COST_M'       'TA_FINAL' '' '' 'Material Cost'.
  PERFORM fill_fieldcat USING 'API_VC'       'TA_FINAL' '' '' 'API VC Costing'.
  PERFORM fill_fieldcat USING 'API_FC'       'TA_FINAL' '' '' 'API FC Costing'.
  PERFORM fill_fieldcat USING 'FORMULATION_VC '       'TA_FINAL' '' '' 'Formulation VC Costing'.
  PERFORM fill_fieldcat USING 'FORMULATION_FC '       'TA_FINAL' '' '' 'Formulation FC Costing'.
  PERFORM fill_fieldcat USING 'FORM_FC '       'TA_FINAL' '' '' 'Form FC Costing'.
  PERFORM fill_fieldcat USING 'SUB_MARGIN '       'TA_FINAL' '' '' 'Sub Margin'.
  PERFORM fill_fieldcat USING 'EDEP_M '       'TA_FINAL' '' '' 'Except Depreciation'.
  PERFORM fill_fieldcat USING 'EDNS_M '       'TA_FINAL' '' '' 'ED&S Except Dep & Emp Cost'.
  PERFORM fill_fieldcat USING 'TOTAL_COST '       'TA_FINAL' '' '' 'TOTAL_COST'.
  PERFORM fill_fieldcat USING 'GM_INR '       'TA_FINAL' '' '' 'GM - INR'.
  PERFORM fill_fieldcat USING 'FLGM_INR '       'TA_FINAL' '' '' 'FLGM - INR'.
  PERFORM fill_fieldcat USING 'USD '       'TA_FINAL' '' '' 'USD'.
  PERFORM fill_fieldcat USING 'MAT_COST_US '       'TA_FINAL' '' '' 'Material Cost - US'.
  PERFORM fill_fieldcat USING 'API_VC_US '       'TA_FINAL' '' '' 'API VC - US'.
  PERFORM fill_fieldcat USING 'API_FC_US '       'TA_FINAL' '' '' 'API FC - US'.
  PERFORM fill_fieldcat USING 'FORMULATION_VC_US '       'TA_FINAL' '' '' 'Formulation VC - US'.
  PERFORM fill_fieldcat USING 'FORMULATION_FC_US '       'TA_FINAL' '' '' 'Formulation FC - US'.
  PERFORM fill_fieldcat USING 'FORM_FC_US '       'TA_FINAL' '' '' 'Form FC - US'.
  PERFORM fill_fieldcat USING 'SUB_MARGIN_US '       'TA_FINAL' '' '' 'Sub Margin - US'.
  PERFORM fill_fieldcat USING 'EDEP_US '       'TA_FINAL' '' '' 'EDEP - US'.
  PERFORM fill_fieldcat USING 'EDNS_US '       'TA_FINAL' '' '' 'EDNS - US'.
  PERFORM fill_fieldcat USING 'TOTAL_COST_US '       'TA_FINAL' '' '' 'Total Cost - US'.
  PERFORM fill_fieldcat USING 'GM_US '       'TA_FINAL' '' '' 'GM - US'.
  PERFORM fill_fieldcat USING 'FLGM_US '       'TA_FINAL' '' '' 'FLGM - US'.
  PERFORM fill_fieldcat USING 'NETWR '       'TA_FINAL' '' '' 'Net Value in Document Currency'.
  PERFORM fill_fieldcat USING 'MAT_COST_LC '       'TA_FINAL' '' '' 'MATERIAL COST - LC'.
  PERFORM fill_fieldcat USING 'API_VC_LC '       'TA_FINAL' '' '' 'API VC - LC'.
  PERFORM fill_fieldcat USING 'API_FC_LC '       'TA_FINAL' '' '' 'API FC - LC'.
  PERFORM fill_fieldcat USING 'FORMULATION_VC_LC '       'TA_FINAL' '' '' 'FORMULATION VC - LC'.
  PERFORM fill_fieldcat USING 'FORMULATION_FC_LC '       'TA_FINAL' '' '' 'FORMULATION FC - LC'.
  PERFORM fill_fieldcat USING 'FORM_FC_LC '       'TA_FINAL' '' '' 'FORM FC - LC'.
  PERFORM fill_fieldcat USING 'SUB_MARGIN_LC '       'TA_FINAL' '' '' 'SUB MARGIN - LC'.
  PERFORM fill_fieldcat USING 'EDEP_LC '       'TA_FINAL' '' '' 'EDEP - LC'.
  PERFORM fill_fieldcat USING 'EDNS_LC '       'TA_FINAL' '' '' 'EDNS - LC'.
  PERFORM fill_fieldcat USING 'TOTAL_COST_LC '       'TA_FINAL' '' '' 'TOTAL COST - LC'.
  PERFORM fill_fieldcat USING 'GM_LC '       'TA_FINAL' '' '' 'GM - LC'.
  PERFORM fill_fieldcat USING 'FLGM_LC '       'TA_FINAL' '' '' 'FLGM - LC'.
  PERFORM fill_fieldcat USING 'DOC_CUR '       'TA_FINAL' '' '' 'Document Curr'.
  PERFORM fill_fieldcat USING 'MAT_COST_CC_CURR '       'TA_FINAL' '' '' 'MATERIAL COST - CC'.
  PERFORM fill_fieldcat USING 'API_VC_CC_CURR '       'TA_FINAL' '' '' 'API VC - CC'.
  PERFORM fill_fieldcat USING 'API_FC_CC_CURR '       'TA_FINAL' '' '' 'API FC - CC'.
  PERFORM fill_fieldcat USING 'FOR_VC_CC_CURR '       'TA_FINAL' '' '' 'FORMULATION VC - CC'.
  PERFORM fill_fieldcat USING 'FOR_FC_CC_CURR '       'TA_FINAL' '' '' 'FORMULATION FC - CC'.
  "EOC by Pooja on 03.11.2022 CR#8000007907
  lwa_layout-zebra = abap_true.
  lwa_layout-colwidth_optimize = abap_true.

  lv_repid = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = lv_repid
      is_layout          = lwa_layout
      it_fieldcat        = ta_fieldcat
    TABLES
      t_outtab           = ta_final
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

FORM update_tab.
  MODIFY zsd_s911_consol FROM TABLE ta_final.
ENDFORM.

FORM fill_fieldcat  USING  pr_fieldname     TYPE slis_fieldname
                           pr_tabname       TYPE slis_tabname
                           pr_ref_fieldname TYPE fieldname
                           pr_ref_tabname   TYPE tabname
                           pr_text          TYPE string.

  DATA : lwa_fieldcat TYPE slis_fieldcat_alv.
  CLEAR : lwa_fieldcat.

  gv_pos = gv_pos + 1.

  lwa_fieldcat-col_pos       = gv_pos.
  lwa_fieldcat-fieldname     = pr_fieldname.
  lwa_fieldcat-tabname       = pr_tabname.
  lwa_fieldcat-ref_fieldname = pr_ref_fieldname.
  lwa_fieldcat-ref_tabname   = pr_ref_tabname.
  lwa_fieldcat-seltext_l     = pr_text.

  APPEND lwa_fieldcat TO ta_fieldcat.

ENDFORM.