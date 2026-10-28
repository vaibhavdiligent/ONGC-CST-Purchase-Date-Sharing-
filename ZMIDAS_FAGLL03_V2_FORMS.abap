*&---------------------------------------------------------------------*
*&  Include           ZMIDAS_FAGLL03_V2_FORMS
*&---------------------------------------------------------------------*
*& Title             : Midas 3 ( BU P&L Automation )
*& Program           : ZMIDAS_FAGLL03_V2_FORMS
*& Tcode             : ZMIDAS_FAGLL03_V2_FORMS
*& Author            : Gajanan Patil
*& Functional Design : Arijit Podder
*& Technical Design  : Gajanan Patil
*& Creation Date     : 03.12.2025
*& Transport Request : CRDK998672
*& Change request    : 8000004907
*& Description       : Call FAGLL03 Tcode to update custom table-Excluding Company Code 1000
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Modification Information
* Date            : 28.10.2026
* Description     : SAP Note 3185840 - SUBMIT FAGL_ACCOUNT_ITEMS_GL
*                   (FAGLL03) removed. Line items are now read directly
*                   from the released CDS views (ledger 0L, G/L view)
*                   into TY_FAGLL03 and passed through BAdI
*                   FAGL_ITEMS_CH_DATA, giving the same data FAGLL03
*                   returned. See FORM get_gl_items_direct.
*&---------------------------------------------------------------------*
*&      Form  REFRESH_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_tab.

  REFRESH: ta_fagll03[],ta_final[],ta_coas[],ta_t003p[],ta_csks[],ta_cskt[],ta_kostl[],ta_t003t[],ta_skat[],ta_bkpf[],ta_bseg[],ta_reimb[],ta_pnl_hd[],
           ta_elem[],ta_prof_gl[],ta_prof_cc[],ta_prevmo[],ta_prevmo[],ta_grp[],ta_subgrp[],ta_fin_tag[],ta_det_pnl[],r_kostl[],r_not_cons[],
           r_sal_wages[],r_sal_wages[],r_staff_welfare[],r_rnd_gl[],r_rnd_cc[],r_forg_exg[],r_write_back[],r_europe_cc[],r_south_africa_cc[],r_emerging_market_cc[].

  CLEAR:   wa_fagll03,wa_final,wa_coas,wa_t003p,wa_csks,wa_cskt,wa_kostl,wa_t003t,wa_skat,wa_reimb,wa_pnl_hd,wa_elem,wa_prof_gl,wa_prof_cc,
           wa_prevmo,wa_prof_of,wa_fin_tag,wa_det_pnl,wa_grp,wa_subgrp,gv_fisc_yr,gv_pnl,gv_pnl2,gv_remark,gv_forg_val,gv_forg_val.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_GLSET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_glset.

  "Exclude Company Code based on ZPARAM_TABLE
  SELECT * FROM zparam_table
    INTO TABLE @DATA(ta_param)
    WHERE id_name = 'COCD_V2_EX'.
  IF NOT ta_param IS INITIAL.
    REFRESH:ra_bukrs[].
    LOOP AT ta_param INTO DATA(wa_param).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_param-param_name ) TO ra_bukrs.
      CLEAR: wa_param.
    ENDLOOP.

    DELETE s_bukrs[] WHERE low IN ra_bukrs[].
    IF s_bukrs[] IS INITIAL.
      MESSAGE 'Invalid Company Code' TYPE co_s DISPLAY LIKE co_e.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

  "SOC - SAP Note 3185840: local variables of the removed SUBMIT block
  "(lta_sel_op, lwa_sel_op, lta_france, lwa_france, lv_from, lv_to,
  " lv_div, lv_mod) are no longer required.
  "EOC - SAP Note 3185840


  "Fetch GL sets from table SETLEAF
  SELECT * FROM setleaf INTO TABLE @DATA(ta_setleaf)
  WHERE setname IN ( @co_not_consider , @co_sal_wages , @co_dist_opex ,@co_dist_secfreight, @co_staff_welfare , @co_rnd_gl , @co_rnd_cc ,
                    @co_europe_cc , @co_south_africa_cc , @co_emerging_market_cc , @co_main_cat_write_back , @co_main_cat_forg_exg , @co_ib_cc ,
                    @co_ach_awards_order, @co_seg_tag_costc_api, @co_seg_tag_pc_special, @co_seg_tag_costc_ipd, @co_seg_tag_costc_ipd2,
                    @co_seg_tag_costc_ipd3,@co_fagll03_zone_gl,@co_rx_other_exp_io,@co_prov_dobtful_dbt_gl,@co_freight_gl ).

  "Start of code Commented by Pooja on 17.10.2022 CR# 8000007907
*  SELECT * FROM setleaf INTO TABLE @DATA(ta_setleaf)
*WHERE setname IN ( @co_gl_set,@co_not_consider , @co_sal_wages , @co_dist_opex ,@co_dist_secfreight, @co_staff_welfare , @co_rnd_gl , @co_rnd_cc ,
*                 @co_europe_cc , @co_south_africa_cc , @co_emerging_market_cc , @co_main_cat_write_back , @co_main_cat_forg_exg , @co_ib_cc ,
*                 @co_ach_awards_order, @co_seg_tag_costc_api, @co_seg_tag_pc_special, @co_seg_tag_costc_ipd, @co_seg_tag_costc_ipd2,
*                 @co_seg_tag_costc_ipd3,@co_fagll03_zone_gl ).

  "End of code Commented by Pooja on 17.10.2022 CR# 8000007907

  IF NOT ta_setleaf IS INITIAL.
    SORT ta_setleaf BY setname.

    "ZMIDAS_GL_MAINSET - START
    DATA(ta_gl_set) = ta_setleaf.

    "Start of code Commented by Pooja on 17.10.2022 CR# 8000007907
*    DELETE ta_gl_set WHERE setname <> co_gl_set.
*
*    IF NOT ta_gl_set IS INITIAL.
*
*      LOOP AT ta_gl_set INTO DATA(wa_gl_set).
*        r_saknr-sign = wa_gl_set-valsign.
*        r_saknr-option = wa_gl_set-valoption.
*        r_saknr-low = wa_gl_set-valfrom.
*        r_saknr-high = wa_gl_set-valto.
*        APPEND r_saknr.
*
*        CLEAR: wa_gl_set.
*      ENDLOOP.
*      REFRESH:ta_gl_set[].
*    ENDIF.
    "ZMIDAS_GL_MAINSET - END
    "End of code Commented by Pooja on 17.10.2022 CR# 8000007907

    "SOC by Pooja on 29.09.2022 CR# 8000007907
    "ZMIDAS_FAGLL03_ZONE_GL - START
    DATA(ta_zone_gl) = ta_setleaf.
    DELETE ta_zone_gl WHERE setname <> co_fagll03_zone_gl.

    IF NOT ta_zone_gl IS INITIAL.

      LOOP AT ta_zone_gl INTO DATA(wa_zone_gl).
        r_zone_gl-sign = wa_zone_gl-valsign.
        r_zone_gl-option = wa_zone_gl-valoption.
        r_zone_gl-low = wa_zone_gl-valfrom.
        r_zone_gl-high = wa_zone_gl-valto.
        APPEND r_zone_gl.

        CLEAR: wa_zone_gl.
      ENDLOOP.
      REFRESH:ta_zone_gl[].
    ENDIF.
    "ZMIDAS_FAGLL03_ZONE_GL - END
    "SOC by Pooja on 29.09.2022 CR# 8000007907

    "ZMIDAS_NOT_CONSIDER_GL - START
    DATA(ta_not_cons) = ta_setleaf.
    DELETE ta_not_cons WHERE setname <> co_not_consider.

    IF NOT ta_not_cons IS INITIAL.

      LOOP AT ta_not_cons INTO DATA(wa_not_cons).
        r_not_cons-sign = wa_not_cons-valsign.
        r_not_cons-option = wa_not_cons-valoption.
        r_not_cons-low = wa_not_cons-valfrom.
        r_not_cons-high = wa_not_cons-valto.
        APPEND r_not_cons.

        CLEAR: wa_not_cons.
      ENDLOOP.
      REFRESH:ta_not_cons[].
    ENDIF.
    "ZMIDAS_NOT_CONSIDER_GL - END

    "ZMIDAS_SALWAGES_GL - START
    DATA(ta_sal_wages) = ta_setleaf.
    DELETE ta_sal_wages WHERE setname <> co_sal_wages.

    IF NOT ta_sal_wages IS INITIAL.
      ta_setleaf_sal[] = ta_sal_wages[].

      LOOP AT ta_sal_wages INTO DATA(wa_sal_wages).
        r_sal_wages-sign = wa_sal_wages-valsign.
        r_sal_wages-option = wa_sal_wages-valoption.
        r_sal_wages-low = wa_sal_wages-valfrom.
        r_sal_wages-high = wa_sal_wages-valto.
        APPEND r_sal_wages.

        CLEAR: wa_sal_wages.
      ENDLOOP.
      REFRESH:ta_sal_wages[].
    ENDIF.
    "ZMIDAS_SALWAGES_GL - END

    "ZMIDAS_DISTRIBUTION_GL - OPEX START
    DATA(ta_dist_opex) = ta_setleaf.
    DELETE ta_dist_opex WHERE setname <> co_dist_opex.

    IF NOT ta_dist_opex IS INITIAL.
      LOOP AT ta_dist_opex INTO DATA(wa_dist_opex).
        r_dist_opex-sign = wa_dist_opex-valsign.
        r_dist_opex-option = wa_dist_opex-valoption.
        r_dist_opex-low = wa_dist_opex-valfrom.
        r_dist_opex-high = wa_dist_opex-valto.
        APPEND r_dist_opex.

        CLEAR wa_dist_opex.
      ENDLOOP.
      REFRESH ta_dist_opex[].
    ENDIF.

    "ZMIDAS_DISTRIBUTION_GL - SECONDARY FREIGHT START
    DATA(ta_dist_secfreight) = ta_setleaf.
    DELETE ta_dist_secfreight WHERE setname <> co_dist_secfreight.

    IF NOT ta_dist_secfreight IS INITIAL.
      LOOP AT ta_dist_secfreight INTO DATA(wa_dist_secfreight).
        r_dist_secfreight-sign = wa_dist_secfreight-valsign.
        r_dist_secfreight-option = wa_dist_secfreight-valoption.
        r_dist_secfreight-low = wa_dist_secfreight-valfrom.
        r_dist_secfreight-high = wa_dist_secfreight-valto.
        APPEND r_dist_secfreight.

        CLEAR wa_dist_secfreight.
      ENDLOOP.
      REFRESH ta_dist_secfreight[].
    ENDIF.

    "ZMIDAS_DISTRIBUTION_GL - END

    "ZMIDAS_STAFF_WELFARE - START
    DATA(ta_staff_welfare) = ta_setleaf.
    DELETE ta_staff_welfare WHERE setname <> co_staff_welfare.

    IF NOT ta_staff_welfare IS INITIAL.

      LOOP AT ta_staff_welfare INTO DATA(wa_staff_welfare).
        r_staff_welfare-sign = wa_staff_welfare-valsign.
        r_staff_welfare-option = wa_staff_welfare-valoption.
        r_staff_welfare-low = wa_staff_welfare-valfrom.
        r_staff_welfare-high = wa_staff_welfare-valto.
        APPEND r_staff_welfare.

        CLEAR: wa_staff_welfare.
      ENDLOOP.
      REFRESH:ta_staff_welfare[].
    ENDIF.
    "ZMIDAS_STAFF_WELFARE - END

    "ZMIDAS_RND_GL - START
    DATA(ta_rnd_gl) = ta_setleaf.
    DELETE ta_rnd_gl WHERE setname <> co_rnd_gl.

    IF NOT ta_rnd_gl IS INITIAL.

      LOOP AT ta_rnd_gl INTO DATA(wa_rnd_gl).
        r_rnd_gl-sign = wa_rnd_gl-valsign.
        r_rnd_gl-option = wa_rnd_gl-valoption.
        r_rnd_gl-low = wa_rnd_gl-valfrom.
        r_rnd_gl-high = wa_rnd_gl-valto.
        APPEND r_rnd_gl.

        CLEAR: wa_rnd_gl.
      ENDLOOP.
      REFRESH:ta_rnd_gl[].
    ENDIF.
    "ZMIDAS_RND_GL - END

    "ZMIDAS_RND_CC - START
    DATA(ta_rnd_cc) = ta_setleaf.
    DELETE ta_rnd_cc WHERE setname <> co_rnd_cc.

    IF NOT ta_rnd_cc IS INITIAL.

      LOOP AT ta_rnd_cc INTO DATA(wa_rnd_cc).
        r_rnd_cc-sign = wa_rnd_cc-valsign.
        r_rnd_cc-option = wa_rnd_cc-valoption.
        r_rnd_cc-low = wa_rnd_cc-valfrom.
        r_rnd_cc-high = wa_rnd_cc-valto.
        APPEND r_rnd_cc.

        CLEAR: wa_rnd_cc.
      ENDLOOP.
      REFRESH:ta_rnd_cc[].
    ENDIF.
    "ZMIDAS_RND_CC - END

    "EUROPE_CC - START
    DATA(ta_europe_cc) = ta_setleaf.
    DELETE ta_europe_cc WHERE setname <> co_europe_cc.

    IF NOT ta_europe_cc IS INITIAL.

      LOOP AT ta_europe_cc INTO DATA(wa_europe_cc).
        r_europe_cc-sign = wa_europe_cc-valsign.
        r_europe_cc-option = wa_europe_cc-valoption.
        r_europe_cc-low = wa_europe_cc-valfrom.
        r_europe_cc-high = wa_europe_cc-valto.
        APPEND r_europe_cc.

        CLEAR: wa_europe_cc.
      ENDLOOP.
      REFRESH:ta_europe_cc[].
    ENDIF.
    "EUROPE_CC - END

    "SOUTH_AFRICA_CC - START
    DATA(ta_south_africa_cc) = ta_setleaf.
    DELETE ta_south_africa_cc WHERE setname <> co_south_africa_cc.

    IF NOT ta_south_africa_cc IS INITIAL.

      LOOP AT ta_south_africa_cc INTO DATA(wa_south_africa_cc).
        r_south_africa_cc-sign = wa_south_africa_cc-valsign.
        r_south_africa_cc-option = wa_south_africa_cc-valoption.
        r_south_africa_cc-low = wa_south_africa_cc-valfrom.
        r_south_africa_cc-high = wa_south_africa_cc-valto.
        APPEND r_south_africa_cc.

        CLEAR: wa_south_africa_cc.
      ENDLOOP.
      REFRESH:ta_south_africa_cc[].
    ENDIF.
    "SOUTH_AFRICA_CC - END

    "EMERGING_MARKET_CC - START
    DATA(ta_emerging_market_cc) = ta_setleaf.
    DELETE ta_emerging_market_cc WHERE setname <> co_emerging_market_cc.

    IF NOT ta_emerging_market_cc IS INITIAL.

      LOOP AT ta_emerging_market_cc INTO DATA(wa_emerging_market_cc).
        r_emerging_market_cc-sign = wa_emerging_market_cc-valsign.
        r_emerging_market_cc-option = wa_emerging_market_cc-valoption.
        r_emerging_market_cc-low = wa_emerging_market_cc-valfrom.
        r_emerging_market_cc-high = wa_emerging_market_cc-valto.
        APPEND r_emerging_market_cc.

        CLEAR: wa_emerging_market_cc.
      ENDLOOP.
      REFRESH:ta_emerging_market_cc[].
    ENDIF.
    "EMERGING_MARKET_CC - END

    "MAIN_CAT_FORG_EXG - START
    DATA(ta_forg_exg) = ta_setleaf.
    DELETE ta_forg_exg WHERE setname <> co_main_cat_forg_exg.

    IF NOT ta_forg_exg IS INITIAL.

      LOOP AT ta_forg_exg INTO DATA(wa_forg_exg).
        r_forg_exg-sign = wa_forg_exg-valsign.
        r_forg_exg-option = wa_forg_exg-valoption.
        r_forg_exg-low = wa_forg_exg-valfrom.
        r_forg_exg-high = wa_forg_exg-valto.
        APPEND r_forg_exg.

        CLEAR: wa_forg_exg.
      ENDLOOP.
      REFRESH:ta_forg_exg[].
    ENDIF.
    "MAIN_CAT_FORG_EXG - END

    "MAIN_CAT_WRITE_BACK - START
    DATA(ta_write_back) = ta_setleaf.
    DELETE ta_write_back WHERE setname <> co_main_cat_write_back.

    IF NOT ta_write_back IS INITIAL.

      LOOP AT ta_write_back INTO DATA(wa_write_back).
        r_write_back-sign = wa_write_back-valsign.
        r_write_back-option = wa_write_back-valoption.
        r_write_back-low = wa_write_back-valfrom.
        r_write_back-high = wa_write_back-valto.
        APPEND r_write_back.

        CLEAR: wa_write_back.
      ENDLOOP.
      REFRESH:ta_write_back[].
    ENDIF.
    "MAIN_CAT_WRITE_BACK - END

    "IB CC - START
    DATA(ta_ib_cc) = ta_setleaf.
    DELETE ta_ib_cc WHERE setname <> co_ib_cc.

    IF NOT ta_ib_cc IS INITIAL.

      LOOP AT ta_ib_cc INTO DATA(wa_ib_cc).
        r_ib_cc-sign = wa_ib_cc-valsign.
        r_ib_cc-option = wa_ib_cc-valoption.
        r_ib_cc-low = wa_ib_cc-valfrom.
        r_ib_cc-high = wa_ib_cc-valto.
        APPEND r_ib_cc.
        CLEAR wa_ib_cc.
      ENDLOOP.
      REFRESH ta_ib_cc[].

    ENDIF.
    "IB CC - END

    "ACH_AWARDS_ORDER - START
    DATA(ta_awards_order) = ta_setleaf.
    DELETE ta_awards_order WHERE setname <> co_ach_awards_order.

    IF NOT ta_awards_order IS INITIAL.

      LOOP AT ta_awards_order INTO DATA(wa_awards_order).
        r_awards_order-sign = wa_awards_order-valsign.
        r_awards_order-option = wa_awards_order-valoption.
        r_awards_order-low = wa_awards_order-valfrom.
        r_awards_order-high = wa_awards_order-valto.
        APPEND r_awards_order.

        CLEAR: wa_awards_order.
      ENDLOOP.
      REFRESH:ta_awards_order[].
    ENDIF.
    "ACH_AWARDS_ORDER - END

    "BU_NA_SEG_TAG_COSTC_API - START
    DATA(ta_seg_tag_costc_api) = ta_setleaf.
    DELETE ta_seg_tag_costc_api WHERE setname <> co_seg_tag_costc_api.

    IF NOT ta_seg_tag_costc_api IS INITIAL.

      LOOP AT ta_seg_tag_costc_api INTO DATA(wa_seg_tag_costc_api).
        r_seg_tag_costc_api-sign = wa_seg_tag_costc_api-valsign.
        r_seg_tag_costc_api-option = wa_seg_tag_costc_api-valoption.
        r_seg_tag_costc_api-low = wa_seg_tag_costc_api-valfrom.
        r_seg_tag_costc_api-high = wa_seg_tag_costc_api-valto.
        APPEND r_seg_tag_costc_api.
        CLEAR wa_seg_tag_costc_api.
      ENDLOOP.
      REFRESH ta_seg_tag_costc_api[].

    ENDIF.
    "BU_NA_SEG_TAG_COSTC_API - END

    "BU_NA_SEG_TAG_COSTC_IPD - START
    DATA(ta_seg_tag_costc_ipd) = ta_setleaf.
    DELETE ta_seg_tag_costc_ipd WHERE setname <> co_seg_tag_costc_ipd.

    IF NOT ta_seg_tag_costc_ipd IS INITIAL.

      LOOP AT ta_seg_tag_costc_ipd INTO DATA(wa_seg_tag_costc_ipd).
        r_seg_tag_costc_ipd-sign = wa_seg_tag_costc_ipd-valsign.
        r_seg_tag_costc_ipd-option = wa_seg_tag_costc_ipd-valoption.
        r_seg_tag_costc_ipd-low = wa_seg_tag_costc_ipd-valfrom.
        r_seg_tag_costc_ipd-high = wa_seg_tag_costc_ipd-valto.
        APPEND r_seg_tag_costc_ipd.
        CLEAR wa_seg_tag_costc_ipd.
      ENDLOOP.
      REFRESH ta_seg_tag_costc_ipd[].

    ENDIF.
    "BU_NA_SEG_TAG_COSTC_IPD - END

    "BU_NA_SEG_TAG_COSTC_IPD2 - START
    DATA(ta_seg_tag_costc_ipd2) = ta_setleaf.
    DELETE ta_seg_tag_costc_ipd2 WHERE setname <> co_seg_tag_costc_ipd2.

    IF NOT ta_seg_tag_costc_ipd2 IS INITIAL.

      LOOP AT ta_seg_tag_costc_ipd2 INTO DATA(wa_seg_tag_costc_ipd2).
        r_seg_tag_costc_ipd2-sign = wa_seg_tag_costc_ipd2-valsign.
        r_seg_tag_costc_ipd2-option = wa_seg_tag_costc_ipd2-valoption.
        r_seg_tag_costc_ipd2-low = wa_seg_tag_costc_ipd2-valfrom.
        r_seg_tag_costc_ipd2-high = wa_seg_tag_costc_ipd2-valto.
        APPEND r_seg_tag_costc_ipd2.
        CLEAR wa_seg_tag_costc_ipd2.
      ENDLOOP.
      REFRESH ta_seg_tag_costc_ipd2[].

    ENDIF.
    "BU_NA_SEG_TAG_COSTC_IPD2 - END

    "SOC by Pooja Adupala on 18.08.2022
    "BU_NA_SEG_TAG_COSTC_IPD3 - START
    DATA(ta_seg_tag_costc_ipd3) = ta_setleaf.
    DELETE ta_seg_tag_costc_ipd3 WHERE setname <> co_seg_tag_costc_ipd3.

    IF NOT ta_seg_tag_costc_ipd3 IS INITIAL.

      LOOP AT ta_seg_tag_costc_ipd3 INTO DATA(wa_seg_tag_costc_ipd3).
        r_seg_tag_costc_ipd3-sign = wa_seg_tag_costc_ipd3-valsign.
        r_seg_tag_costc_ipd3-option = wa_seg_tag_costc_ipd3-valoption.
        r_seg_tag_costc_ipd3-low = wa_seg_tag_costc_ipd3-valfrom.
        r_seg_tag_costc_ipd3-high = wa_seg_tag_costc_ipd3-valto.
        APPEND r_seg_tag_costc_ipd3.
        CLEAR wa_seg_tag_costc_ipd3.
      ENDLOOP.
      REFRESH ta_seg_tag_costc_ipd3[].

    ENDIF.
    "BU_NA_SEG_TAG_COSTC_IPD3 - END
    "EOC by Pooja Adupala on 18.08.2022


    "BU_NA_SEG_TAG_PC_SPECIAL - START
    DATA(ta_seg_tag_pc_special) = ta_setleaf.
    DELETE ta_seg_tag_pc_special WHERE setname <> co_seg_tag_pc_special.

    IF NOT ta_seg_tag_pc_special IS INITIAL.

      LOOP AT ta_seg_tag_pc_special INTO DATA(wa_seg_tag_pc_special).
        r_seg_tag_pc_special-sign = wa_seg_tag_pc_special-valsign.
        r_seg_tag_pc_special-option = wa_seg_tag_pc_special-valoption.
        r_seg_tag_pc_special-low = wa_seg_tag_pc_special-valfrom.
        r_seg_tag_pc_special-high = wa_seg_tag_pc_special-valto.
        APPEND r_seg_tag_pc_special.
        CLEAR wa_seg_tag_pc_special.
      ENDLOOP.
      REFRESH ta_seg_tag_pc_special[].

    ENDIF.
    "BU_NA_SEG_TAG_PC_SPECIAL - END

    DATA(ta_rx_othexp_io) = ta_setleaf.
    DELETE ta_rx_othexp_io WHERE setname <> co_rx_other_exp_io.

    IF NOT ta_rx_othexp_io IS INITIAL.
      LOOP AT ta_rx_othexp_io INTO DATA(wa_rx_othexp_io).
        r_rx_othexp_io-sign = wa_rx_othexp_io-valsign.
        r_rx_othexp_io-option = wa_rx_othexp_io-valoption.
        r_rx_othexp_io-low = wa_rx_othexp_io-valfrom.
        r_rx_othexp_io-high = wa_rx_othexp_io-valto.
        APPEND r_rx_othexp_io.
        CLEAR: wa_rx_othexp_io.
      ENDLOOP.
      REFRESH:ta_rx_othexp_io[].
    ENDIF.

    "SOC by Gajanan - 24.08.2025 - CR#8000003569
    DATA(ta_freight_gl) = ta_setleaf.
    DELETE ta_freight_gl WHERE setname <> co_freight_gl.

    IF NOT ta_freight_gl IS INITIAL.

      LOOP AT ta_freight_gl INTO DATA(wa_freight_gl).
        r_freight_gl-sign = wa_freight_gl-valsign.
        r_freight_gl-option = wa_freight_gl-valoption.
        r_freight_gl-low = wa_freight_gl-valfrom.
        r_freight_gl-high = wa_freight_gl-valto.
        APPEND r_freight_gl.

        CLEAR: wa_freight_gl.
      ENDLOOP.
      REFRESH:ta_freight_gl[].
    ENDIF.
    "EOC by Gajanan - 24.08.2025 - CR#8000003569

    "SOC by Gajanan - 24.08.2025 - CR#8000003569
    DATA(ta_prov_dobtful_dbt_gl) = ta_setleaf.
    DELETE ta_prov_dobtful_dbt_gl WHERE setname <> co_prov_dobtful_dbt_gl.

    IF NOT ta_prov_dobtful_dbt_gl IS INITIAL.

      LOOP AT ta_prov_dobtful_dbt_gl INTO DATA(wa_prov_dobtful_dbt_gl).
        r_prov_dobtful_dbt_gl-sign = wa_prov_dobtful_dbt_gl-valsign.
        r_prov_dobtful_dbt_gl-option = wa_prov_dobtful_dbt_gl-valoption.
        r_prov_dobtful_dbt_gl-low = wa_prov_dobtful_dbt_gl-valfrom.
        r_prov_dobtful_dbt_gl-high = wa_prov_dobtful_dbt_gl-valto.
        APPEND r_prov_dobtful_dbt_gl.

        CLEAR: wa_prov_dobtful_dbt_gl.
      ENDLOOP.
      REFRESH:ta_prov_dobtful_dbt_gl[].
    ENDIF.
    "EOC by Gajanan - 24.08.2025 - CR#8000003569
  ENDIF.


  IF s_erdat[] IS NOT INITIAL.

*    CALL FUNCTION 'GM_GET_FISCAL_YEAR'
*      EXPORTING
*        i_date                     = p_erdat
*        i_fyv                      = 'V3'
*      IMPORTING
*        e_fy                       = gv_fisc_yr
*      EXCEPTIONS
*        fiscal_year_does_not_exist = 1
*        not_defined_for_date       = 2
*        OTHERS                     = 3.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.

    "Fetch Document Numbers from Table BKPF
    SELECT bukrs belnr gjahr cpudt usnam FROM bkpf
      INTO TABLE ta_bkpf
      WHERE bukrs IN s_bukrs[]
*        AND gjahr = gv_fisc_yr
        AND cpudt IN s_erdat[].

    IF NOT ta_bkpf IS INITIAL.

      SORT ta_bkpf BY bukrs belnr gjahr.
      "Fetch Document Numbers from Table BSEG
" " Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 26.06.2026  FOR ATC
*      SELECT bukrs belnr gjahr buzei hkont FROM bseg
*        INTO TABLE ta_bseg
*        FOR ALL ENTRIES IN ta_bkpf
*        WHERE bukrs = ta_bkpf-bukrs
*          AND belnr = ta_bkpf-belnr
*          AND gjahr = ta_bkpf-gjahr
*          AND hkont IN s_saknr[].
 SELECT  COMPANYCODE AS BUKRS , ACCOUNTINGDOCUMENT AS BELNR ,
FISCALYEAR AS GJAHR , ACCOUNTINGDOCUMENTITEM AS BUZEI , GLACCOUNT AS
HKONT FROM I_OPERATIONALACCTGDOCITEM FOR ALL ENTRIES IN @TA_BKPF WHERE
COMPANYCODE = @TA_BKPF-BUKRS AND ACCOUNTINGDOCUMENT = @TA_BKPF-BELNR
AND FISCALYEAR = @TA_BKPF-GJAHR AND GLACCOUNT IN @S_SAKNR[] INTO TABLE
@TA_BSEG .
" " Code Remediation changes S4 2025_A Conversion * *END OF CHANGE BY ABAP7 26.06.2026 FOR ATC

      IF ta_bseg IS NOT INITIAL.
        "SOC - SAP Note 3185840: company code range R_BUKRS was only used
        "as SUBMIT parameter SD_BUKRS of FAGL_ACCOUNT_ITEMS_GL; no longer
        "required (the new SELECT restricts by document key).
        "EOC - SAP Note 3185840

        DATA(ta_bseg_filter) = ta_bseg.

        SORT ta_bseg BY belnr.
        DELETE ADJACENT DUPLICATES FROM ta_bseg COMPARING belnr.

        "Pass data year wise
        DATA(ta_bseg_gjahr) = ta_bseg.
        SORT ta_bseg_gjahr BY gjahr.
        DELETE ADJACENT DUPLICATES FROM ta_bseg_gjahr COMPARING gjahr.


        LOOP AT ta_bseg_gjahr INTO DATA(wa_bseg_gjahr).
          DATA(ta_bseg_fagll03) = ta_bseg.
          DELETE ta_bseg_fagll03 WHERE gjahr <> wa_bseg_gjahr-gjahr.

          IF NOT ta_bseg_fagll03 IS INITIAL.
            "SOC - SAP Note 3185840: SUBMIT FAGL_ACCOUNT_ITEMS_GL (FAGLL03)
            "with cl_salv_bs_runtime_info replaced by a direct read of the
            "ledger 0L G/L-view line items - see FORM get_gl_items_direct.
            PERFORM get_gl_items_direct
              USING    ta_bseg_fagll03
                       wa_bseg_gjahr-gjahr
              CHANGING ta_fagll03.
            "EOC - SAP Note 3185840
          ENDIF.
          CLEAR:wa_bseg_gjahr.
          REFRESH: ta_bseg_fagll03[].
        ENDLOOP.
      ENDIF.
    ENDIF.

***SOA by Gajanan - 28.10.2024 - CR#8000003569***

    IF NOT ta_fagll03 IS INITIAL.
      LOOP AT ta_fagll03 INTO wa_fagll03.
        IF line_exists( ta_bseg_filter[ bukrs = wa_fagll03-bukrs belnr = wa_fagll03-belnr gjahr = wa_fagll03-gjahr buzei = wa_fagll03-buzei hkont = wa_fagll03-hkont ] ).

          APPEND wa_fagll03 TO ta_fagll03_tm.

        ENDIF.
        CLEAR:wa_fagll03.
      ENDLOOP.

      IF ta_fagll03_tm IS NOT INITIAL.
        REFRESH:ta_fagll03[].
        APPEND LINES OF ta_fagll03_tm TO ta_fagll03.
      ENDIF.
    ENDIF.
***EOA by Gajanan - 28.10.2024 - CR#8000003569***

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data.
  DATA: lv_forg_period   TYPE t009b-poper,
        lv_forg_fy       TYPE t009b-bdatj,
        lv_write_period  TYPE t009b-poper,
        lv_write_fy      TYPE t009b-bdatj,
        ta_acc_bal_1     TYPE TABLE OF bapi1028_4,
        ta_acc_bal_forg  TYPE TABLE OF bapi1028_4,
        ta_acc_bal_2     TYPE TABLE OF bapi1028_4,
        ta_acc_bal_write TYPE TABLE OF bapi1028_4.


  IF NOT ta_fagll03 IS INITIAL.

    "Fetch Set ZMIDAS_SALWAGES_GL description from table SETLINET
    SELECT * FROM setlinet INTO TABLE ta_setlinet
      WHERE setname = co_sal_wages.

    IF NOT ta_setlinet IS INITIAL.
      SORT ta_setlinet BY lineid.
    ENDIF.

    "Fetch data from table COAS
    DATA(ta_temp_1) = ta_fagll03[].
    SORT ta_temp_1 BY aufnr.
    DELETE ADJACENT DUPLICATES FROM ta_temp_1 COMPARING aufnr.

    IF NOT ta_temp_1 IS INITIAL.
      SELECT aufnr auart kostv akstl
        FROM coas INTO TABLE ta_coas
        FOR ALL ENTRIES IN ta_temp_1
        WHERE aufnr = ta_temp_1-aufnr.

      IF NOT ta_coas IS INITIAL.
        SORT ta_coas BY aufnr.

        "Fetch data from table T003P
        SELECT spras auart txt
          FROM t003p INTO TABLE ta_t003p
          FOR ALL ENTRIES IN ta_coas
          WHERE spras = co_e
            AND auart = ta_coas-auart.

        IF NOT ta_t003p IS INITIAL.
          SORT ta_t003p BY auart.
        ENDIF.

        LOOP AT ta_coas INTO DATA(wa_coas_temp_1).
          wa_kostl-kostl = wa_coas_temp_1-kostv.
          APPEND wa_kostl TO ta_kostl.
          CLEAR: wa_kostl.
          wa_kostl-kostl = wa_coas_temp_1-akstl.
          APPEND wa_kostl TO ta_kostl.

          r_kostl-sign = co_i.
          r_kostl-option = co_eq.
          r_kostl-low = wa_coas_temp_1-kostv.
          APPEND r_kostl.
          r_kostl-low = wa_coas_temp_1-akstl.
          APPEND r_kostl.
          CLEAR: wa_coas_temp_1,wa_kostl.
        ENDLOOP.
      ENDIF.
    ENDIF.

    "Fetch data from table CSKS and table CSKT
    LOOP AT ta_fagll03 INTO DATA(wa_fagll03_temp_1).
      wa_kostl-kostl = wa_fagll03_temp_1-kostl.
      APPEND wa_kostl TO ta_kostl.
      r_kostl-sign = co_i.
      r_kostl-option = co_eq.
      r_kostl-low = wa_fagll03_temp_1-kostl.
      APPEND r_kostl.
      CLEAR: wa_fagll03_temp_1,wa_kostl.
    ENDLOOP.

    IF NOT r_kostl[] IS INITIAL.

      IF ta_kostl IS NOT INITIAL.
        SELECT kokrs kostl datbi name1 name2 name3
          FROM csks INTO TABLE ta_csks
          FOR ALL ENTRIES IN ta_kostl
          WHERE kostl = ta_kostl-kostl.
      ENDIF.

      IF NOT ta_csks IS INITIAL.
        SORT ta_csks BY kostl.

        SELECT spras kokrs kostl datbi ltext
          FROM cskt INTO TABLE ta_cskt
          FOR ALL ENTRIES IN ta_csks
          WHERE spras = co_e
            AND kokrs = ta_csks-kokrs
            AND kostl = ta_csks-kostl
            AND datbi = ta_csks-datbi.

        IF NOT ta_cskt IS INITIAL.
          SORT ta_cskt BY kostl.
        ENDIF.
      ENDIF.
    ENDIF.

    "Fetch data from table T003T
    DATA(ta_temp_2) = ta_fagll03[].
    SORT ta_temp_2 BY blart.
    DELETE ADJACENT DUPLICATES FROM ta_temp_2 COMPARING blart.

    IF NOT ta_temp_2 IS INITIAL.
" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 26.06.2026  FOR ATC
*      SELECT spras blart ltext
*        FROM t003t INTO TABLE ta_t003t
*        FOR ALL ENTRIES IN ta_temp_2
*        WHERE spras = co_e
*          AND blart = ta_temp_2-blart.
      SELECT spras blart ltext
        FROM t003t INTO TABLE ta_t003t
        FOR ALL ENTRIES IN ta_temp_2
        WHERE spras = co_e
" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 27.06.2026 FOR ATC NOORDER
*          AND blart = ta_temp_2-blart. "#EC CI_NOORDER
          AND blart = ta_temp_2-blart
          ORDER BY PRIMARY KEY.
" Code Remediation changes S4 2025_A Conversion **END OF CHANGE BY ABAP7 27.06.2026 FOR ATC NOORDER
" Code Remediation changes S4 2025_A Conversion * *END OF CHANGE BY ABAP7 26.06.2026 FOR ATC

      IF NOT ta_t003t IS INITIAL.
        SORT ta_t003t BY blart.
      ENDIF.
    ENDIF.

    "Fetch data from table SKAT
    DATA(ta_temp_3) = ta_fagll03[].
    SORT ta_temp_3 BY hkont.
    DELETE ADJACENT DUPLICATES FROM ta_temp_3 COMPARING hkont.

    IF NOT ta_temp_3 IS INITIAL.
      SELECT spras ktopl saknr txt50
        FROM skat INTO TABLE ta_skat
        FOR ALL ENTRIES IN ta_temp_3
        WHERE spras = co_e
          AND saknr = ta_temp_3-hkont.

      IF NOT ta_skat IS INITIAL.
        SORT ta_skat BY saknr.
      ENDIF.
    ENDIF.

    "Fetch dara from table ZCO_MIDAS_REIMB
    SELECT * FROM zco_midas_reimb
      INTO TABLE ta_reimb
      FOR ALL ENTRIES IN ta_fagll03
      WHERE spmon   = ta_fagll03-budat+0(6)
        AND bukrs   = ta_fagll03-bukrs
        AND belnr   = ta_fagll03-belnr.
*        AND hkont   = ta_fagll03-hkont
*        AND buzei   = ta_fagll03-buzei.

    IF NOT ta_reimb IS INITIAL.
      SORT ta_reimb BY spmon bukrs belnr. "hkont buzei.
    ENDIF.

    "Fetch dara from table ZCO_MIDAS_IC_ELE
    SELECT * FROM zco_midas_ic_ele
      INTO TABLE ta_elem
      FOR ALL ENTRIES IN ta_fagll03
      WHERE spmon   = ta_fagll03-budat+0(6)
        AND bukrs   = ta_fagll03-bukrs
        AND belnr   = ta_fagll03-belnr
        AND hkont   = ta_fagll03-hkont
        AND buzei   = ta_fagll03-buzei.

    IF NOT ta_elem IS INITIAL.
      SORT ta_elem BY spmon bukrs belnr hkont buzei.
    ENDIF.

    "Fetch data from table ZFI_PROF_FPNA_GL
    DATA(ta_temp_4) = ta_fagll03[].
    SORT ta_temp_4 BY bukrs hkont.
    DELETE ADJACENT DUPLICATES FROM ta_temp_4 COMPARING bukrs hkont.

    IF NOT ta_temp_4 IS INITIAL.
      SELECT * FROM zfi_prof_fpna_gl
        INTO TABLE ta_prof_gl
        FOR ALL ENTRIES IN ta_temp_4
        WHERE bukrs = ta_temp_4-bukrs
          AND saknr = ta_temp_4-hkont.

      IF NOT ta_prof_gl IS INITIAL.
        SORT ta_prof_gl BY bukrs saknr.
      ENDIF.
    ENDIF.

    "Fetch data from table ZFI_PROF_FPNA_CC
    DATA(ta_temp_5) = ta_fagll03[].
    SORT ta_temp_5 BY bukrs kostl.
    DELETE ADJACENT DUPLICATES FROM ta_temp_5 COMPARING bukrs kostl.

    IF NOT ta_temp_5 IS INITIAL.
      SELECT * FROM zfi_prof_fpna_cc
        INTO TABLE ta_prof_cc
        FOR ALL ENTRIES IN ta_temp_5
        WHERE bukrs = ta_temp_5-bukrs.
*          AND kostl IN r_kostl.

      IF NOT ta_prof_cc IS INITIAL.
        SORT ta_prof_cc BY bukrs kostl.
        DELETE ta_prof_cc WHERE kostl NOT IN r_kostl.

      ENDIF.
    ENDIF.

    "Fetch data from table ZFI_FPNA_PREVMO
    DATA(ta_temp_6) = ta_fagll03[].
    SORT ta_temp_6 BY bukrs hkont prctr.
    DELETE ADJACENT DUPLICATES FROM ta_temp_6 COMPARING bukrs hkont prctr.

    IF NOT ta_temp_6 IS INITIAL.
      SELECT * FROM zfi_fpna_prevmo
        INTO TABLE ta_prevmo
        FOR ALL ENTRIES IN ta_temp_6
        WHERE bukrs = ta_temp_6-bukrs
          AND saknr = ta_temp_6-hkont.
*          AND KOSTL = ta_temp_6-prctr.

      IF NOT ta_prevmo IS INITIAL.
        SORT ta_prevmo BY bukrs saknr kostl prctr.
        DELETE ta_prevmo WHERE kostl NOT IN r_kostl.
      ENDIF.
    ENDIF.

    "Fetch data from table ZFI_PROF_FPNA_OF
    DATA(ta_temp_7) = ta_fagll03[].
    SORT ta_temp_7 BY bukrs hkont gkont.
    DELETE ADJACENT DUPLICATES FROM ta_temp_7 COMPARING bukrs hkont gkont.

    IF NOT ta_temp_7 IS INITIAL.
      SELECT * FROM zfi_prof_fpna_of
        INTO TABLE ta_prof_of
        FOR ALL ENTRIES IN ta_temp_7
        WHERE bukrs = ta_temp_7-bukrs
          AND saknr = ta_temp_7-hkont
          AND gkont = ta_temp_7-gkont.

      IF NOT ta_prof_of IS INITIAL.
        SORT ta_prof_of BY bukrs saknr gkont.
      ENDIF.
    ENDIF.

***SOA by Gajanan - 24.09.2024 - CR#8000003569
    "Fetch data from table ZMIDAS3_GL_PC
    DATA(ta_temp_9) = ta_fagll03[].
    SORT ta_temp_9 BY bukrs hkont prctr.
    DELETE ADJACENT DUPLICATES FROM ta_temp_9 COMPARING bukrs hkont prctr.

    IF NOT ta_temp_9 IS INITIAL.
      SELECT * FROM zmidas3_gl_pc
        INTO TABLE ta_gl_pc
        FOR ALL ENTRIES IN ta_temp_9
        WHERE bukrs = ta_temp_9-bukrs
          AND prctr = ta_temp_9-prctr
          AND hkont = ta_temp_9-hkont.

      IF NOT ta_gl_pc IS INITIAL.
        SORT ta_gl_pc BY bukrs hkont prctr.
      ENDIF.
    ENDIF.
***EOA by Gajanan - 24.09.2024 - CR#8000003569

    "Calculate GL amount for set MAIN_CAT_FORG_EXG and MAIN_CAT_WRITE_BACK
    DATA(ta_temp_8) = ta_fagll03[].
    SORT ta_temp_8 BY hkont.
    DELETE ADJACENT DUPLICATES FROM ta_temp_8 COMPARING hkont.
    DATA(ta_forg_gl) = ta_temp_8[].
    DELETE ta_forg_gl WHERE hkont NOT IN r_forg_exg[].
    DATA(ta_write_gl) = ta_temp_8[].
    DELETE ta_write_gl WHERE hkont NOT IN r_write_back[].

    "Set MAIN_CAT_FORG_EXG
    IF NOT ta_forg_gl IS INITIAL.
      LOOP AT ta_forg_gl INTO DATA(wa_forg_gl).

        "Calculate Period
        CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
          EXPORTING
            i_date  = wa_forg_gl-budat
            i_periv = 'V3'
          IMPORTING
            e_buper = lv_forg_period
            e_gjahr = lv_forg_fy.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

        "Calculate Posting period balances for G/L
" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 26.06.2026  FOR ATC
        CALL FUNCTION 'BAPI_GL_GETGLACCPERIODBALANCES'"#EC CI_USAGE_OK[2628704]
" Code Remediation changes S4 2025_A Conversion * *END OF CHANGE BY ABAP7 26.06.2026 FOR ATC
          EXPORTING
            companycode      = wa_forg_gl-bukrs
            glacct           = wa_forg_gl-hkont
            fiscalyear       = lv_forg_fy
            currencytype     = '10'
          TABLES
            account_balances = ta_acc_bal_1[].
        IF NOT ta_acc_bal_1 IS INITIAL.
          SORT ta_acc_bal_1 BY fis_period.
          APPEND LINES OF ta_acc_bal_1[] FROM 1 TO lv_forg_period TO ta_acc_bal_forg[].
        ENDIF.

        CLEAR: wa_forg_gl,lv_forg_period,lv_forg_fy.
        REFRESH: ta_acc_bal_1[].
      ENDLOOP.
    ENDIF.

    IF NOT ta_acc_bal_forg[] IS INITIAL.
      LOOP AT ta_acc_bal_forg INTO DATA(wa_acc_bal_forg).

        AT LAST.
          SUM.
          gv_forg_val = wa_acc_bal_forg-debits_per + wa_acc_bal_forg-credit_per.
          CLEAR: wa_acc_bal_forg.
        ENDAT.

      ENDLOOP.
    ENDIF.

    "Set MAIN_CAT_WRITE_BACK
    IF NOT ta_write_gl IS INITIAL.
      LOOP AT ta_write_gl INTO DATA(wa_write_gl).

        "Calculate Period
        CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
          EXPORTING
            i_date  = wa_write_gl-budat
            i_periv = 'V3'
          IMPORTING
            e_buper = lv_write_period
            e_gjahr = lv_write_fy.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

        "Calculate Posting period balances for G/L
" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 26.06.2026  FOR ATC
        CALL FUNCTION 'BAPI_GL_GETGLACCPERIODBALANCES'"#EC CI_USAGE_OK[2628704]
" Code Remediation changes S4 2025_A Conversion * *END OF CHANGE BY ABAP7 26.06.2026 FOR ATC
          EXPORTING
            companycode      = wa_write_gl-bukrs
            glacct           = wa_write_gl-hkont
            fiscalyear       = lv_write_fy
            currencytype     = '10'
          TABLES
            account_balances = ta_acc_bal_2[].
        IF NOT ta_acc_bal_2 IS INITIAL.
          SORT ta_acc_bal_2 BY fis_period.
          APPEND LINES OF ta_acc_bal_2[] FROM 1 TO lv_write_period TO ta_acc_bal_write[].
        ENDIF.

        CLEAR: wa_write_gl,lv_write_period,lv_write_fy.
        REFRESH: ta_acc_bal_2[].
      ENDLOOP.
    ENDIF.

    IF NOT ta_acc_bal_write[] IS INITIAL.
      LOOP AT ta_acc_bal_write INTO DATA(wa_acc_bal_write).

        AT LAST.
          SUM.
          gv_write_val = wa_acc_bal_write-debits_per + wa_acc_bal_write-credit_per.
          CLEAR: wa_acc_bal_write.
        ENDAT.

      ENDLOOP.
    ENDIF.


    "Fetch Data from table ZMIDAS_FIN_TAG
    SELECT * FROM zmidas_fin_tag
      INTO TABLE ta_fin_tag.

    IF NOT ta_fin_tag IS INITIAL.
      SORT ta_fin_tag BY fin_pnl_sub.
    ENDIF.

    "Fetch Data from table ZMIDAS_DET_PNL
    SELECT * FROM zmidas_det_pnl
      INTO TABLE ta_det_pnl.

    IF NOT ta_det_pnl IS INITIAL.
      SORT ta_det_pnl BY fin_pnl_main.
    ENDIF.

    "Fetch Data from table ZCO_MIDAS_PNL_HD
    SELECT * FROM zco_midas_pnl_hd
      INTO TABLE ta_pnl_hd.

    IF NOT ta_pnl_hd IS INITIAL.
      SORT ta_pnl_hd BY fin_tag_rx.
    ENDIF.

    "Fetch data from table ZMIDAS_SEG_REL
    SELECT * FROM zmidas_seg_rel
      INTO TABLE ta_seg_rel.

    IF NOT ta_seg_rel IS INITIAL.
      SORT ta_seg_rel BY bukrs hkont fin_kostl.
    ENDIF.

  ELSE.

    MESSAGE text-001 TYPE co_s DISPLAY LIKE co_e.
    LEAVE LIST-PROCESSING.

  ENDIF.

  REFRESH: ta_temp_1[],ta_temp_2[],ta_temp_3[],ta_temp_4[],ta_temp_5[],ta_temp_6[],ta_temp_7[],ta_temp_8[],ta_acc_bal_1[],ta_acc_bal_2[].

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPDATE_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_tab.
  CLASS cl_abap_char_utilities DEFINITION LOAD.

  CONSTANTS: co_new_line TYPE c VALUE cl_abap_char_utilities=>newline,
             co_hor_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab.
  DATA: lv_param_name TYPE char70,
        lv_vbeln      TYPE vbrk-vbeln. "LOC added by Pooja A on 29.09.2022 CR#8000007907
  "SOC added by Pooja A on 01.03.2023 CR#8000007907
  DATA:  lv_string1 TYPE string,
         lv_string2 TYPE string.
  "EOC added by Pooja A on 01.03.2023 CR#8000007907
  CONSTANTS:co_splchar    TYPE char10 VALUE 'SPL_CHAR',
            co_idno       TYPE char3 VALUE '001',
            "SOC by Pooja Adupala on 17.08.2022
            co_updatefrom TYPE zmidas_fagll03-update_from VALUE 'Original',
            co_updatetype TYPE zmidas_fagll03-midas_type VALUE 'System'.
  "EOC by Pooja Adupala on 17.08.2022

  SELECT * FROM zmidas_bu_pnl
       INTO TABLE @DATA(ta_bu_pnl).

  IF ta_bu_pnl IS NOT INITIAL.
    SORT ta_bu_pnl BY bu_pnl.
  ENDIF.

  SELECT SINGLE param_name
    INTO lv_param_name
    FROM zparam_table
    WHERE id_name = co_splchar
    AND id_number = co_idno.

  IF ta_fagll03 IS NOT INITIAL.
    SELECT * FROM zmidas_gl_exclud
           INTO TABLE @DATA(ta_gl_exclud)
       FOR ALL ENTRIES IN @ta_fagll03
           WHERE saknr = @ta_fagll03-hkont .

    IF ta_gl_exclud IS NOT INITIAL.
      SORT ta_gl_exclud BY saknr.
    ENDIF.

    SELECT  * FROM zmidas_eu_tag
    INTO TABLE @DATA(ta_eu_tag)
      FOR ALL ENTRIES IN @ta_fagll03
     WHERE bukrs = @ta_fagll03-bukrs AND
     saknr = @ta_fagll03-hkont AND
     kostl = @ta_fagll03-kostl AND
     gkont = @ta_fagll03-gkont.

    IF ta_eu_tag IS NOT INITIAL.
      SORT ta_eu_tag BY bukrs saknr kostl gkont.
    ENDIF.
    "SOC by Pooja on 29.09.2022 CR# 8000007907
    DATA(ta_fagll03_zone_gl) = ta_fagll03.
    DELETE ta_fagll03_zone_gl WHERE hkont NOT IN r_zone_gl.
    SORT ta_fagll03_zone_gl BY xblnr.
    DELETE ADJACENT DUPLICATES FROM ta_fagll03_zone_gl COMPARING  xblnr.

    LOOP AT ta_fagll03_zone_gl INTO DATA(wa_fagll03_zone_gl).
      wa_zone_gl1-vbeln = wa_fagll03_zone_gl-xblnr.
      APPEND wa_zone_gl1 TO ta_zone_gl1.
      CLEAR:wa_zone_gl1,wa_fagll03_zone_gl.
    ENDLOOP.

    IF ta_zone_gl1 IS NOT INITIAL.
      SELECT vbeln,posnr,aubel,kvgr1 FROM vbrp "AUBEL-Added by Gajanan - 29.03.2024 - CR#8000003569
        INTO TABLE @DATA(ta_vbrp)
        FOR ALL ENTRIES IN @ta_zone_gl1
        WHERE vbeln = @ta_zone_gl1-vbeln.

      "SOA by Gajanan - 29.03.2024 - CR#8000003569
      IF ta_vbrp IS NOT INITIAL.
        SELECT vbeln,kvgr1 FROM vbak
          INTO TABLE @DATA(ta_vbak)
          FOR ALL ENTRIES IN @ta_vbrp
          WHERE vbeln = @ta_vbrp-aubel.

        IF NOT ta_vbak IS INITIAL.
          SORT ta_vbak BY vbeln.

          SELECT * FROM tvv1t
            INTO TABLE @DATA(ta_tvv1t1)
            FOR ALL ENTRIES IN @ta_vbak
            WHERE spras = 'E' AND
                  kvgr1 = @ta_vbak-kvgr1.

          IF NOT ta_tvv1t1 IS INITIAL.
            SORT ta_tvv1t1 BY kvgr1.
          ENDIF.
        ENDIF.
      ENDIF.
      "EOA by Gajanan - 29.03.2024 - CR#8000003569

    ENDIF.

    SORT ta_vbrp BY vbeln kvgr1.
    DELETE ADJACENT DUPLICATES FROM ta_vbrp COMPARING vbeln kvgr1.

    IF ta_vbrp IS NOT INITIAL.
      SELECT * FROM tvv1t
        INTO TABLE @DATA(ta_tvv1t)
        FOR ALL ENTRIES IN @ta_vbrp
        WHERE spras = 'E' AND
              kvgr1 = @ta_vbrp-kvgr1.
    ENDIF.
    SORT ta_tvv1t BY kvgr1.
    "EOC by Pooja on 29.09.2022 CR# 8000007907
* Start of Change Inc 8000003399 | TR CRDK989166 | Prathamesh Parab | 09.01.2024
* Start of Change Inc 8000003399 | TR CRDK989314 | Prathamesh Parab | 23.01.2024
*   SELECT kunnr vbund
*     FROM kna1
*     INTO TABLE ta_kna1
*     FOR ALL ENTRIES IN ta_fagll03
*    WHERE kunnr = ta_fagll03-gkont.
*
*   IF sy-subrc = 0.
*     SORT ta_kna1 BY kunnr.
*   ENDIF.

    SELECT vbeln kunrg kunag
      FROM vbrk
      INTO TABLE ta_vbrk
      FOR ALL ENTRIES IN ta_fagll03
     WHERE vbeln = ta_fagll03-kidno+0(10).

    IF sy-subrc = 0.
      SORT ta_vbrk BY vbeln.

      SELECT kunnr vbund
        FROM kna1
        INTO TABLE ta_kna1
        FOR ALL ENTRIES IN ta_vbrk
       WHERE kunnr = ta_vbrk-kunrg.

      IF sy-subrc = 0.
        SORT ta_kna1 BY kunnr.
      ENDIF.

      "SOA by Gajanan - 24.08.2025 - CR#8000003569
      SELECT kunnr,land1
        FROM kna1
        INTO TABLE @DATA(ta_land1)
        FOR ALL ENTRIES IN @ta_vbrk
       WHERE kunnr = @ta_vbrk-kunag.

      IF sy-subrc = 0.
        SORT ta_land1 BY kunnr.

        SELECT spras,land1,landx
          FROM t005t INTO TABLE @DATA(ta_country)
          FOR ALL ENTRIES IN @ta_land1
          WHERE spras = @co_e
            AND land1 = @ta_land1-land1.

        IF NOT ta_country IS INITIAL .
          SORT ta_country BY land1.
        ENDIF.
      ENDIF.
      "EOA by Gajanan - 24.08.2025 - CR#8000003569

    ENDIF.

    "SOA by Gajanan - 24.08.2025 - CR#8000003569
    DATA(ta_fagll03_zuonr) = ta_fagll03[].
    DELETE ta_fagll03_zuonr WHERE u_zuonr IS INITIAL.
    LOOP AT ta_fagll03_zuonr ASSIGNING FIELD-SYMBOL(<wa_fagll03_zuonr>).
      wa_u_zuonr-kunnr = <wa_fagll03_zuonr>-u_zuonr.
      wa_u_zuonr-kunnr = |{ wa_u_zuonr-kunnr ALPHA = IN }|.
      APPEND wa_u_zuonr TO ta_u_zuonr.
      CLEAR: wa_u_zuonr.
    ENDLOOP.

    IF ta_u_zuonr IS NOT INITIAL.
      SELECT kunnr,land1
      FROM kna1
      INTO TABLE @DATA(ta_land1_1)
      FOR ALL ENTRIES IN @ta_u_zuonr
     WHERE kunnr = @ta_u_zuonr-kunnr.

      IF sy-subrc = 0.
        SORT ta_land1_1 BY kunnr.

        SELECT spras,land1,landx
          FROM t005t INTO TABLE @DATA(ta_country_1)
          FOR ALL ENTRIES IN @ta_land1_1
          WHERE spras = @co_e
            AND land1 = @ta_land1_1-land1.

        IF NOT ta_country_1 IS INITIAL .
          SORT ta_country_1 BY land1.
        ENDIF.
      ENDIF.
    ENDIF.
    "EOA by Gajanan - 24.08.2025 - CR#8000003569
* End of Change Inc 8000003399 | TR CRDK989314 | Prathamesh Parab | 23.01.2024
* End of Change Inc 8000003399 | TR CRDK989166 | Prathamesh Parab | 09.01.2024
  ENDIF.

  DATA(ta_fagll03_t001) = ta_fagll03[].
  SORT ta_fagll03_t001 BY bukrs.
  DELETE ADJACENT DUPLICATES FROM ta_fagll03_t001 COMPARING bukrs.

  IF ta_fagll03_t001 IS NOT INITIAL.
    SELECT bukrs,
           waers
      FROM t001 INTO TABLE @DATA(ta_t001)
      FOR ALL ENTRIES IN @ta_fagll03_t001
      WHERE bukrs = @ta_fagll03_t001-bukrs.
    IF ta_t001 IS NOT INITIAL.
      SORT ta_t001 BY bukrs.
    ENDIF.
  ENDIF.

  SORT ta_bkpf BY bukrs belnr gjahr.
  LOOP AT ta_fagll03 INTO wa_fagll03.

    MOVE-CORRESPONDING wa_fagll03 TO wa_final.

    "Client
    wa_final-mandt = sy-mandt.

    "Period
    wa_final-spmon = wa_fagll03-budat+0(6).

    "Entry Date
*    wa_final-cpudt = p_erdat.

    "Doc Type
    "Start of changes by Pooja Adupala on 17.08.2022
*    IF wa_fagll03-sgtxt = 'REV PROV EAS' OR wa_fagll03-sgtxt = 'EAS PROVISION'.
*      wa_final-blart = wa_fagll03-blart = co_ep.
*    ENDIF.

    DATA(lv_string) = wa_fagll03-sgtxt.
    IF lv_string CS 'REV PROV EAS' OR lv_string CS 'EAS PROVISION'.
      wa_final-blart = wa_fagll03-blart = co_ep.
    ENDIF.
    "End of changes by Pooja Adupala on 17.08.2022

    "Not consider GL
    IF wa_fagll03-hkont IN r_not_cons[].
      wa_final-non_gl = text-t01."IB_GL Group
    ENDIF.

    "IO Cost center,Order Type and Order Type description
    READ TABLE ta_coas INTO wa_coas WITH KEY aufnr = wa_fagll03-aufnr
                                             BINARY SEARCH.
    IF sy-subrc = 0.

      IF wa_coas-kostv IS NOT INITIAL.
        wa_final-io_kostl = wa_coas-kostv.
      ELSE.
        wa_final-io_kostl = wa_coas-akstl.
      ENDIF.

      wa_final-auart = wa_coas-auart.

      READ TABLE ta_t003p INTO wa_t003p WITH KEY auart = wa_coas-auart
                                                 BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-ord_desc = wa_t003p-txt.
        CLEAR: wa_t003p.
      ENDIF.
      CLEAR: wa_coas.
    ENDIF.

    IF wa_fagll03-aufnr IN r_awards_order[].
      wa_final-ord_desc = text-t43. "Achiever Awards
    ENDIF.

    "Final Cost Center
    IF NOT wa_final-io_kostl IS INITIAL.
      wa_final-fin_kostl = wa_final-io_kostl.
    ELSE.
      wa_final-fin_kostl = wa_fagll03-kostl.
    ENDIF.


    "Department:

    IF wa_final-fin_kostl IS NOT INITIAL.
" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 26.06.2026  FOR ATC
*      SELECT SINGLE abtei FROM csks
*        INTO @DATA(lv_abtei)
*        WHERE kostl =  @wa_final-fin_kostl.
 SELECT ABTEI FROM CSKS   UP TO 1 ROWS INTO @DATA(LV_ABTEI) WHERE KOSTL
= @WA_FINAL-FIN_KOSTL  ORDER BY  KOKRS ,KOSTL, DATBI .   ENDSELECT.
" Code Remediation changes S4 2025_A Conversion * *END OF CHANGE BY ABAP7 26.06.2026 FOR ATC

      wa_final-department =  lv_abtei.
      CLEAR:lv_abtei.
    ENDIF.


    "Cost Center name
    READ TABLE ta_cskt INTO wa_cskt WITH KEY kostl = wa_final-fin_kostl
                                             BINARY SEARCH.
    IF sy-subrc = 0.
      wa_final-cc_name = wa_cskt-ltext.
      CLEAR:wa_cskt.
    ENDIF.

    "Division,CC Type and Sub division
    READ TABLE ta_csks INTO wa_csks WITH KEY kostl = wa_final-fin_kostl
                                             BINARY SEARCH.
    IF sy-subrc = 0.
      wa_final-cc_div = wa_csks-name1.
      wa_final-cc_type = wa_csks-name2.
      wa_final-cc_subdiv = wa_csks-name3.
      CLEAR:wa_csks.
    ENDIF.

    "IB / Non IB Tagging
    IF wa_final-non_gl = text-t01."IB_GL Group.
      wa_final-tagging = abap_false.
    ELSEIF wa_fagll03-hkont = co_5190010 OR wa_fagll03-hkont = co_5190001.
      IF wa_fagll03-bukrs = co_1000. "1000 "LOC added by Pooja Adupala on 16.08.2022
        wa_final-tagging = text-t02. "IB.
      ENDIF. "LOC added by Pooja Adupala on 16.08.2022
    ELSEIF r_ib_cc IS NOT INITIAL AND  wa_final-fin_kostl IN r_ib_cc[].
      IF wa_fagll03-bukrs = co_1000. "1000 "LOC added by Pooja Adupala on 16.08.2022
        wa_final-tagging = text-t02. "IB.
      ENDIF. "LOC added by Pooja Adupala on 16.08.2022
    ENDIF.

    "SOC by Pooja on 29.09.2022
    IF wa_final-bukrs NE co_1000.
      wa_final-tagging = ''.
    ENDIF.
    "EOC by Pooja on 29.09.2022

    "Document Type Description
    READ TABLE ta_t003t INTO wa_t003t WITH KEY blart = wa_fagll03-blart
                                               BINARY SEARCH.
    IF sy-subrc = 0.
      wa_final-doc_desc = wa_t003t-ltext.
      CLEAR: wa_t003t.
    ENDIF.

    "GL Description
    READ TABLE ta_skat INTO wa_skat WITH KEY saknr = wa_fagll03-hkont
                                             BINARY SEARCH.
    IF sy-subrc = 0.
      wa_final-gl_desc = wa_skat-txt50.
      CLEAR: wa_skat.
    ENDIF.

    "Order Type Initials
    IF NOT wa_fagll03-aufnr IS INITIAL.
      wa_final-ord_init = |{ wa_fagll03-aufnr ALPHA = OUT }|.
    ENDIF.

    "Manpower GL list
    IF wa_fagll03-hkont IN r_sal_wages[].
*      wa_final-manpower_gl = text-t40."Manpower"

      READ TABLE ta_setleaf_sal INTO wa_setleaf_sal WITH KEY valfrom = wa_fagll03-hkont.
      IF sy-subrc = 0.
        READ TABLE ta_setlinet INTO wa_setlinet WITH KEY lineid = wa_setleaf_sal-lineid
                                                         BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final-manpower_gl = wa_setlinet-descript.
        ELSE.
          wa_final-manpower_gl = text-t40."Manpower
        ENDIF.
        CLEAR:wa_setleaf_sal,wa_setlinet.
      ELSE.
        wa_final-manpower_gl = text-t40."Manpower
      ENDIF.
    ENDIF.

    "GL Account
    IF wa_fagll03-hkont IN r_dist_opex[].
      wa_final-dist_gl = text-t41."Opex
    ELSEIF wa_fagll03-hkont IN r_dist_secfreight[].
      wa_final-dist_gl = text-t42."Secondary Freight
    ENDIF.

    "Reimbursement document
    READ TABLE ta_reimb INTO wa_reimb WITH KEY spmon = wa_final-spmon
                                               bukrs = wa_fagll03-bukrs
                                               belnr = wa_fagll03-belnr
*                                               hkont = wa_fagll03-hkont
*                                               buzei = wa_fagll03-buzei
                                               BINARY SEARCH.
    IF sy-subrc = 0.
      wa_final-reimb_doc = wa_reimb-reimb_doc.
      CLEAR: wa_reimb.
    ENDIF.



    "Esproute / Conference
    IF wa_fagll03-hkont NOT IN r_staff_welfare[].
      IF wa_final-ord_init = co_9. "9
        wa_final-esp_conf = wa_final-ord_desc.
      ENDIF.
    ENDIF.

    "EAS
    IF wa_fagll03-hkont NOT IN r_staff_welfare[].
      IF wa_final-blart = co_z8 OR wa_final-blart = co_ep.
        wa_final-eas = text-t05. "EAS
      ENDIF.
    ENDIF.

    "Manpower
    IF NOT wa_final-manpower_gl IS INITIAL AND wa_final-cc_type = co_marketing_depot.
      wa_final-manpower = wa_final-manpower_gl.
    ENDIF.

    "Contract Cost not to be considered
    IF wa_final-fin_kostl = co_10hmu01704 AND ( wa_fagll03-hkont = co_5350011 OR wa_fagll03-hkont = co_5350051 ).
      wa_final-con_cost = text-t06. "Contract Cost not to be considered
    ENDIF.

    "Contract staff Base cost
    IF wa_final-cc_type = co_marketing_depot AND ( wa_fagll03-hkont = co_5350011 OR wa_fagll03-hkont = co_5350051 ).
      wa_final-con_staff = text-t07. "Contract Cost
    ENDIF.

    "Distribution Expense - Logic Pending
    IF wa_fagll03-hkont NOT IN r_staff_welfare[] AND wa_final-cc_type = co_distribution_cc.
      wa_final-dist_exp = wa_final-dist_gl.
    ENDIF.


    "Onco Care Commission
    IF wa_fagll03-hkont   = co_5190010 OR
   ( ( wa_fagll03-hkont   = co_5190006    OR wa_fagll03-hkont   = co_5180004    ) AND
     ( wa_final-fin_kostl = co_10hmu01521 OR wa_final-fin_kostl = co_10hmu01010 ) ).

      wa_final-onco_com = text-t08. "Onco Care Commission

    ENDIF.

    "Non Onco Care Commission
    IF wa_fagll03-hkont   = co_5190001.
      wa_final-non_onco = text-t09. "Non Onco Care Commission
    ENDIF.

    "Sales Office Expense
    IF wa_fagll03-hkont NOT IN r_staff_welfare[] AND wa_final-cc_type = co_sales_office_cc.
      wa_final-sales_exp = text-t10. "Sales Office Expesnse
    ENDIF.

    "Laptop and Lease Expense not to be considered
    IF wa_fagll03-hkont = co_5230006 AND wa_final-fin_kostl = co_10hmu01704.
      wa_final-ll_non_exp = text-t11. "Laptop and Lease Expense not to be considered
    ENDIF.

    "Laptop and Lease Expense
    IF wa_final-ll_non_exp IS INITIAL AND wa_fagll03-hkont = co_5230006.
      wa_final-ll_exp = text-t12. "Laptop and Lease Expense
    ENDIF.

    "Berok Expense
    IF wa_final-fin_kostl = co_10hmu01725.
      wa_final-berok_exp = text-t13. "Berok Expense
    ENDIF.

    "Dare to Disrupt
    IF wa_final-fin_kostl BETWEEN co_10hmu01d01 AND co_10hmu01d05.
      wa_final-disrupt = text-t14. "Dare to Disrupt
    ENDIF.

    "Reimbursement
    IF wa_fagll03-hkont NOT IN r_staff_welfare[] AND wa_final-reimb_doc IS NOT INITIAL AND wa_final-manpower IS INITIAL AND wa_final-cc_type = co_marketing_depot.
      wa_final-reimburse = text-t15. "Reimbursement
    ENDIF.


    "Contract Staff reimbursement
    IF wa_fagll03-hkont = co_5350058 AND wa_final-cc_type = co_marketing_depot.
      wa_final-cs_reimb = text-t15. "Reimbursement
    ENDIF.

    "IBL
    IF wa_fagll03-hkont = co_5080013.
      wa_final-ibl = text-t16. "IBL
    ENDIF.

    "Promotion Cost
    IF wa_final-u_ebeln IS NOT INITIAL AND wa_final-esp_conf IS INITIAL AND wa_final-eas IS INITIAL AND wa_final-manpower IS INITIAL AND wa_final-con_cost IS INITIAL
       AND wa_final-con_staff IS INITIAL AND wa_final-dist_exp IS INITIAL AND wa_final-onco_com IS INITIAL AND wa_final-non_onco IS INITIAL AND wa_final-sales_exp IS INITIAL
       AND wa_final-ll_non_exp IS INITIAL AND wa_final-ll_exp IS INITIAL AND wa_final-berok_exp IS INITIAL AND wa_final-disrupt IS INITIAL AND wa_final-reimburse IS INITIAL
       AND wa_final-cs_reimb IS INITIAL AND wa_final-ibl IS INITIAL.
      wa_final-prom_cost = text-t20. "Promotion Expenses
    ENDIF.

    "Distribution Manpower
    IF wa_final-cc_type = co_distribution_cc AND ( wa_fagll03-hkont = co_5350011 OR wa_fagll03-hkont = co_5350051 ).
      wa_final-dist_mp = text-t17. "not consider Distribution Manpower
    ENDIF.

    "Distribution Manpower1
    IF wa_final-cc_type = co_distribution_cc AND wa_final-manpower_gl IS NOT INITIAL.
      wa_final-dist_mp1 = text-t17. "not consider Distribution Manpower
    ENDIF.

    "Bad Debts + Provision for Doubtful Debts
    IF wa_fagll03-hkont = co_5310003 OR wa_fagll03-hkont = co_5310001.
      wa_final-bad_debt = wa_final-gl_desc.
    ENDIF.

    "Royalty
    IF wa_fagll03-hkont = co_5350009 OR wa_fagll03-hkont = co_5180050.
      wa_final-royalty = text-t18. "Royalty
    ENDIF.

    "Staff Welfare
    IF wa_fagll03-hkont IN r_staff_welfare[] AND ( wa_final-esp_conf <> co_esproute OR wa_final-esp_conf <> co_achiever_awards ).
      wa_final-st_welfare = text-t19. "Staff Welfare
    ENDIF.

    "Other Expense
    IF wa_final-esp_conf IS INITIAL AND wa_final-eas IS INITIAL AND wa_final-manpower IS INITIAL AND wa_final-con_cost IS INITIAL AND wa_final-con_staff IS INITIAL
       AND wa_final-dist_exp IS INITIAL AND wa_final-onco_com IS INITIAL AND wa_final-non_onco IS INITIAL AND wa_final-sales_exp IS INITIAL AND wa_final-ll_non_exp IS INITIAL
       AND wa_final-ll_exp IS INITIAL AND wa_final-berok_exp IS INITIAL AND wa_final-disrupt IS INITIAL AND wa_final-reimburse IS INITIAL AND wa_final-cs_reimb IS INITIAL
       AND wa_final-ibl IS INITIAL AND wa_final-prom_cost IS INITIAL AND wa_final-dist_mp IS INITIAL AND wa_final-dist_mp1 IS INITIAL AND wa_final-bad_debt IS INITIAL
       AND wa_final-royalty IS INITIAL AND wa_final-st_welfare IS INITIAL.
      wa_final-oth_exp = text-t21. "Other Expense
    ELSE.
      wa_final-oth_exp = ''.
    ENDIF.

    "P&L
    CONCATENATE wa_final-esp_conf wa_final-eas wa_final-manpower wa_final-con_cost wa_final-con_staff wa_final-dist_exp wa_final-onco_com wa_final-non_onco
                wa_final-sales_exp wa_final-ll_non_exp wa_final-ll_exp wa_final-berok_exp wa_final-disrupt wa_final-reimburse wa_final-cs_reimb wa_final-ibl
                wa_final-prom_cost wa_final-dist_mp wa_final-dist_mp1 wa_final-bad_debt wa_final-royalty wa_final-st_welfare wa_final-oth_exp
                INTO gv_pnl.

    "P&L 2
    gv_pnl2 = gv_pnl.
    TRANSLATE gv_pnl2 USING ','.


    "Final Management tagging for Rx
    wa_final-fin_tag_rx = gv_pnl2.
    TRANSLATE wa_final-fin_tag_rx USING '0'.

    IF wa_final-ord_desc = text-t43. "Achiever Awards.
      wa_final-fin_tag_rx = text-t43. "Achiever Awards.
    ENDIF.

    "SOC by Pooja Adupala on 01.03.2023"final management tagging
    IF r_rx_othexp_io IS NOT INITIAL AND wa_final-aufnr IN r_rx_othexp_io.
      wa_final-fin_tag_rx = 'Other Expense'.
    ENDIF.

    IF wa_final-tagging = 'IB'.
      IF wa_final-hkont IS NOT INITIAL AND wa_final-hkont IN r_sal_wages[].
        wa_final-fin_tag_rx = 'Manpower'.
      ENDIF.
    ENDIF.

    IF wa_final-berok_exp = text-t13.
      wa_final-fin_tag_rx = 'Berok'.
    ENDIF.

    IF wa_final-kostl = '10HMU01O08'.
      lv_string2 = 'One India -'.
      lv_string1 =   |{ lv_string2 }| & | | & |{   wa_final-fin_tag_rx }|.
      wa_final-fin_tag_rx = lv_string1 .
    ENDIF.
    "EOC by Pooja Adupala on 01.03.2023

    "P&L Header
    TRANSLATE wa_final-fin_tag_rx TO UPPER CASE.
    READ TABLE ta_pnl_hd INTO wa_pnl_hd WITH KEY fin_tag_rx = wa_final-fin_tag_rx.

    IF sy-subrc = 0.
      wa_final-p_l_head = wa_pnl_hd-p_l_head.
      CLEAR: wa_pnl_hd.
    ENDIF.



    "inter company Elimination Doc number
    READ TABLE ta_elem INTO wa_elem WITH KEY spmon = wa_final-spmon
                                             bukrs = wa_fagll03-bukrs
                                             belnr = wa_fagll03-belnr
                                             hkont = wa_fagll03-hkont
                                             buzei = wa_fagll03-buzei
                                             BINARY SEARCH.
    IF sy-subrc = 0.
      wa_final-ele_doc = wa_elem-ele_doc.
      CLEAR: wa_elem.
    ENDIF.

    "TB Main Group and TB Sub Group
    READ TABLE ta_prof_gl INTO wa_prof_gl WITH KEY bukrs = wa_fagll03-bukrs
                                                   saknr = wa_fagll03-hkont
                                                   BINARY SEARCH.
    IF sy-subrc = 0.
      wa_final-tb_main = wa_prof_gl-grouping.
      wa_final-tb_sub  = wa_prof_gl-sub_grouping.

      CLEAR: wa_prof_gl.
    ENDIF.


    "Final P&L Main Cat and Final P&L Sub Cat
    IF wa_final-tagging = text-t02.

      IF NOT wa_final-fin_tag_rx IS INITIAL.
        wa_final-fin_pnl_main = wa_final-fin_tag_rx.
        wa_final-fin_pnl_sub  = wa_final-fin_tag_rx.
        gv_remark = text-t26. "Based on IB tagging
      ELSE.
        wa_final-fin_pnl_main = text-t22. "NO TAGGING FOUND
        wa_final-fin_pnl_sub  = text-t22. "NO TAGGING FOUND
        gv_remark  = text-t22. "NO TAGGING FOUND
      ENDIF.

    ELSE.

      "SOC by Pooja on 1.09.2022
      READ TABLE ta_gl_exclud INTO DATA(wa_gl_exclud) WITH KEY saknr = wa_final-hkont.
      IF sy-subrc = 0.
        wa_final-fin_pnl_main = wa_gl_exclud-final_plm.
        wa_final-fin_pnl_sub  = wa_gl_exclud-final_pls.
        gv_remark = text-t50.
      ENDIF.

      IF wa_final-fin_pnl_main  IS INITIAL AND  wa_final-fin_pnl_sub  IS INITIAL AND wa_fagll03-bukrs = co_1000.
        READ TABLE ta_eu_tag INTO DATA(wa_eu_tag) WITH KEY bukrs = wa_final-bukrs
                                                           saknr = wa_final-hkont
                                                           kostl = wa_fagll03-kostl
                                                           gkont = wa_final-gkont.
        IF sy-subrc = 0.
          wa_final-fin_pnl_main = wa_eu_tag-final_plm.
          wa_final-fin_pnl_sub  = wa_eu_tag-final_pls.
          gv_remark = text-t51.
        ENDIF.
        "EOC by Pooja on 1.09.2022
        IF wa_fagll03-hkont IN r_forg_exg[] AND wa_final-fin_pnl_main  IS INITIAL AND  wa_final-fin_pnl_sub  IS INITIAL.
          IF gv_forg_val LT 0.
            wa_final-fin_pnl_main = text-t37. "Other Income
            wa_final-fin_pnl_sub  = text-t37. "Other Income

          ELSEIF gv_forg_val GE 0.
            wa_final-fin_pnl_main = text-t38. "Other Expenses
            wa_final-fin_pnl_sub  = text-t38. "Other Expenses
          ENDIF.

        ELSEIF wa_fagll03-hkont IN r_write_back[] AND wa_final-fin_pnl_main  IS INITIAL AND  wa_final-fin_pnl_sub  IS INITIAL.
          IF gv_write_val LT 0.
            wa_final-fin_pnl_main = text-t37. "Other Income
            wa_final-fin_pnl_sub  = text-t37. "Other Income

          ELSEIF gv_write_val GE 0.
            wa_final-fin_pnl_main = text-t39. "Selling and Distribution Expenses
            wa_final-fin_pnl_sub  = text-t39. "Selling and Distribution Expenses
          ENDIF.

        ELSEIF wa_fagll03-hkont IN r_rnd_gl[] AND wa_final-fin_kostl IN r_rnd_cc[] AND wa_final-fin_pnl_main  IS INITIAL AND  wa_final-fin_pnl_sub  IS INITIAL.
          wa_final-fin_pnl_main = text-t23. "R&D
          wa_final-fin_pnl_sub  = text-t23. "R&D
          gv_remark = text-t29. "Based on R&D

        ELSEIF wa_final-fin_pnl_main  IS INITIAL AND  wa_final-fin_pnl_sub IS INITIAL .

          READ TABLE ta_prevmo INTO wa_prevmo WITH KEY bukrs = wa_fagll03-bukrs
                                                       saknr = wa_fagll03-hkont
                                                       kostl = wa_final-fin_kostl
                                                       prctr = wa_fagll03-prctr
                                                       BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-fin_pnl_main = wa_prevmo-final_plm.
            wa_final-fin_pnl_sub  = wa_prevmo-final_pls.
            gv_remark = text-t30. "Company Code + GL + CC + PC

            CLEAR: wa_prevmo.

            "Code commented by PA: 16.08.2022
*          ELSE.
*
*            READ TABLE ta_prevmo INTO wa_prevmo WITH KEY bukrs = wa_fagll03-bukrs
*                                                         saknr = wa_fagll03-hkont
*                                                         prctr = wa_fagll03-prctr.
*            IF sy-subrc = 0.
*              wa_final-fin_pnl_main = wa_prevmo-final_plm.
*              wa_final-fin_pnl_sub  = wa_prevmo-final_pls.
*              gv_remark = text-t31. "Company Code + GL + PC
*
*              CLEAR: wa_prevmo.

          ELSE.
***SOA by Gajanan - 24.09.2024 - CR#8000003569
            READ TABLE ta_gl_pc INTO wa_gl_pc WITH KEY bukrs = wa_fagll03-bukrs
                                                       prctr = wa_fagll03-prctr
                                                       hkont = wa_fagll03-hkont
                                                       BINARY SEARCH.
            IF sy-subrc = 0.

              wa_final-fin_pnl_main = wa_gl_pc-fin_pnl_main.
              wa_final-fin_pnl_sub  = wa_gl_pc-fin_pnl_sub.
              gv_remark = text-t31. "Company Code + PC + GL
              CLEAR:wa_gl_pc.

            ELSE.
***EOA by Gajanan - 24.09.2024 - CR#8000003569
              READ TABLE ta_prof_gl INTO wa_prof_gl WITH KEY bukrs = wa_fagll03-bukrs
                                                             saknr = wa_fagll03-hkont
                                                             BINARY SEARCH.
              IF sy-subrc = 0.
                wa_final-fin_pnl_main = wa_prof_gl-final_plm.
                wa_final-fin_pnl_sub  = wa_prof_gl-final_pls.
                gv_remark = text-t32. "Company Code + GL
                CLEAR:wa_prof_gl.

              ELSE.
                READ TABLE ta_prof_cc INTO wa_prof_cc WITH KEY bukrs = wa_fagll03-bukrs
                                                               kostl = wa_final-fin_kostl
                                                               BINARY SEARCH.
                IF sy-subrc = 0.
                  wa_final-fin_pnl_main = wa_prof_cc-final_plm.
                  wa_final-fin_pnl_sub  = wa_prof_cc-final_pls.
                  gv_remark = text-t33. "Company Code + CC
                  CLEAR:wa_prof_cc.
                ELSE.
                  wa_final-fin_pnl_main = text-t22. "NO TAGGING FOUND
                  wa_final-fin_pnl_sub  = text-t22. "NO TAGGING FOUND
                  gv_remark  = text-t22. "NO TAGGING FOUND
                ENDIF.
              ENDIF.
*            ENDIF."LOC commented by PA: 16.08.2022
            ENDIF.
          ENDIF.
        ENDIF.

      ELSEIF wa_fagll03-bukrs = co_3800 AND wa_final-fin_pnl_main  IS INITIAL AND  wa_final-fin_pnl_sub IS INITIAL. "3800

        "SOC by Pooja Adupala on 16.08.2022
*        READ TABLE ta_prof_cc INTO wa_prof_cc WITH KEY bukrs = wa_fagll03-bukrs
*                                                       kostl = wa_final-fin_kostl
*                                                       BINARY SEARCH.
*        IF sy-subrc = 0.
*          wa_final-fin_pnl_main = wa_prof_cc-final_plm.
*          wa_final-fin_pnl_sub  = wa_prof_cc-final_pls.
*          gv_remark = text-t33. "Company Code + CC
*          CLEAR:wa_prof_cc.

        CLEAR:wa_prevmo.
        READ TABLE ta_prevmo INTO wa_prevmo WITH KEY bukrs = wa_fagll03-bukrs
                                                     saknr = wa_fagll03-hkont
                                                     kostl = wa_final-fin_kostl
                                                     prctr = wa_fagll03-prctr
                                                     BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final-fin_pnl_main = wa_prevmo-final_plm.
          wa_final-fin_pnl_sub  = wa_prevmo-final_pls.
          gv_remark = text-t30. "Company Code + GL + CC + PC
          "EOC by Pooja Adupala on 16.08.2022
        ELSE.

          READ TABLE ta_prof_gl INTO wa_prof_gl WITH KEY bukrs = wa_fagll03-bukrs
                                                         saknr = wa_fagll03-hkont
                                                         BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-fin_pnl_main = wa_prof_gl-final_plm.
            wa_final-fin_pnl_sub  = wa_prof_gl-final_pls.
            gv_remark = text-t32. "Company Code + GL
            CLEAR:wa_prof_gl.

          ELSE.
            wa_final-fin_pnl_main = text-t22. "NO TAGGING FOUND
            wa_final-fin_pnl_sub  = text-t22. "NO TAGGING FOUND
            gv_remark  = text-t22. "NO TAGGING FOUND

          ENDIF.
        ENDIF.

      ELSEIF wa_fagll03-bukrs IN r_europe_cc[] AND  wa_final-fin_pnl_main  IS INITIAL AND  wa_final-fin_pnl_sub IS INITIAL.."Europe

        READ TABLE ta_prof_of INTO wa_prof_of WITH KEY bukrs = wa_fagll03-bukrs
                                                       saknr = wa_fagll03-hkont
                                                       gkont = wa_fagll03-gkont
                                                       BINARY SEARCH.

        IF sy-subrc = 0.
          wa_final-fin_pnl_main = wa_prof_of-final_plm.
          wa_final-fin_pnl_sub  = wa_prof_of-final_pls.
          gv_remark = text-t34. "Company Code + GL + OA
          CLEAR:wa_prof_of.

        ELSE.
          READ TABLE ta_prof_cc INTO wa_prof_cc WITH KEY bukrs = wa_fagll03-bukrs
                                                         kostl = wa_final-fin_kostl
                                                         BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-fin_pnl_main = wa_prof_cc-final_plm.
            wa_final-fin_pnl_sub  = wa_prof_cc-final_pls.
            gv_remark = text-t33. "Company Code + CC
            CLEAR:wa_prof_cc.

          ELSE.
            READ TABLE ta_prof_gl INTO wa_prof_gl WITH KEY bukrs = wa_fagll03-bukrs
                                                           saknr = wa_fagll03-hkont
                                                           BINARY SEARCH.
            IF sy-subrc = 0.
              wa_final-fin_pnl_main = wa_prof_gl-final_plm.
              wa_final-fin_pnl_sub  = wa_prof_gl-final_pls.
              gv_remark = text-t32. "Company Code + GL
              CLEAR:wa_prof_gl.

            ELSE.
              wa_final-fin_pnl_main = text-t22. "NO TAGGING FOUND
              wa_final-fin_pnl_sub  = text-t22. "NO TAGGING FOUND
              gv_remark  = text-t22. "NO TAGGING FOUND
            ENDIF.
          ENDIF.
        ENDIF.

      ELSEIF wa_fagll03-bukrs = co_5200 AND  wa_final-fin_pnl_main IS INITIAL AND  wa_final-fin_pnl_sub IS INITIAL . "5200

        READ TABLE ta_prevmo INTO wa_prevmo WITH KEY bukrs = wa_fagll03-bukrs
                                                     saknr = wa_fagll03-hkont
                                                     kostl = wa_final-fin_kostl.
        IF sy-subrc = 0.
          wa_final-fin_pnl_main = wa_prevmo-final_plm.
          wa_final-fin_pnl_sub  = wa_prevmo-final_pls.
          gv_remark = text-t35. "Company Code + GL + CC

          CLEAR: wa_prevmo.

        ELSE.
          READ TABLE ta_prof_gl INTO wa_prof_gl WITH KEY bukrs = wa_fagll03-bukrs
                                                         saknr = wa_fagll03-hkont
                                                         BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-fin_pnl_main = wa_prof_gl-final_plm.
            wa_final-fin_pnl_sub  = wa_prof_gl-final_pls.
            gv_remark = text-t32. "Company Code + GL
            CLEAR:wa_prof_gl.

          ELSE.
            wa_final-fin_pnl_main = text-t22. "NO TAGGING FOUND
            wa_final-fin_pnl_sub  = text-t22. "NO TAGGING FOUND
            gv_remark  = text-t22. "NO TAGGING FOUND
          ENDIF.
        ENDIF.

      ELSEIF wa_fagll03-bukrs = co_5400 AND  wa_final-fin_pnl_main IS INITIAL AND  wa_final-fin_pnl_sub IS INITIAL . "5400

        READ TABLE ta_prevmo INTO wa_prevmo WITH KEY bukrs = wa_fagll03-bukrs
                                                     saknr = wa_fagll03-hkont
                                                     kostl = wa_final-fin_kostl.
        IF sy-subrc = 0.
          wa_final-fin_pnl_main = wa_prevmo-final_plm.
          wa_final-fin_pnl_sub  = wa_prevmo-final_pls.
          gv_remark = text-t35. "Company Code + GL + CC

          CLEAR: wa_prevmo.

        ELSE.
          READ TABLE ta_prof_gl INTO wa_prof_gl WITH KEY bukrs = wa_fagll03-bukrs
                                                         saknr = wa_fagll03-hkont
                                                         BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-fin_pnl_main = wa_prof_gl-final_plm.
            wa_final-fin_pnl_sub  = wa_prof_gl-final_pls.
            gv_remark = text-t32. "Company Code + GL
            CLEAR:wa_prof_gl.

          ELSE.
            wa_final-fin_pnl_main = text-t22. "NO TAGGING FOUND
            wa_final-fin_pnl_sub  = text-t22. "NO TAGGING FOUND
            gv_remark  = text-t22. "NO TAGGING FOUND
          ENDIF.
        ENDIF.

      ELSEIF wa_fagll03-bukrs = co_5000 AND  wa_final-fin_pnl_main IS INITIAL AND  wa_final-fin_pnl_sub IS INITIAL . "5000

        READ TABLE ta_prevmo INTO wa_prevmo WITH KEY bukrs = wa_fagll03-bukrs
                                                     saknr = wa_fagll03-hkont
                                                     kostl = wa_final-fin_kostl.
        IF sy-subrc = 0.
          wa_final-fin_pnl_main = wa_prevmo-final_plm.
          wa_final-fin_pnl_sub  = wa_prevmo-final_pls.
          gv_remark = text-t35. "Company Code + GL + CC

          CLEAR: wa_prevmo.

        ELSE.
          READ TABLE ta_prof_gl INTO wa_prof_gl WITH KEY bukrs = wa_fagll03-bukrs
                                                         saknr = wa_fagll03-hkont
                                                         BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-fin_pnl_main = wa_prof_gl-final_plm.
            wa_final-fin_pnl_sub  = wa_prof_gl-final_pls.
            gv_remark = text-t32. "Company Code + GL
            CLEAR:wa_prof_gl.

          ELSE.
            wa_final-fin_pnl_main = text-t22. "NO TAGGING FOUND
            wa_final-fin_pnl_sub  = text-t22. "NO TAGGING FOUND
            gv_remark  = text-t22. "NO TAGGING FOUND
          ENDIF.
        ENDIF.

      ELSEIF wa_fagll03-bukrs IN r_south_africa_cc AND  wa_final-fin_pnl_main IS INITIAL AND  wa_final-fin_pnl_sub IS INITIAL . "South Africa

        READ TABLE ta_prevmo INTO wa_prevmo WITH KEY bukrs = wa_fagll03-bukrs
                                                     saknr = wa_fagll03-hkont
                                                     kostl = wa_final-fin_kostl
                                                     prctr = wa_fagll03-prctr
                                                     BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final-fin_pnl_main = wa_prevmo-final_plm.
          wa_final-fin_pnl_sub  = wa_prevmo-final_pls.
          gv_remark  = text-t30. "Company Code + GL + CC + PC

          CLEAR: wa_prevmo.

        ELSE.

          "SOC By Pooja Adupala on 16.08.2022
*          READ TABLE ta_prof_cc INTO wa_prof_cc WITH KEY bukrs = wa_fagll03-bukrs
*                                                         kostl = wa_final-fin_kostl
*                                                         BINARY SEARCH.
*          IF sy-subrc = 0.
*            wa_final-fin_pnl_main = wa_prof_cc-final_plm.
*            wa_final-fin_pnl_sub  = wa_prof_cc-final_pls.
*            gv_remark = text-t33. "Company Code + CC
*            CLEAR:wa_prof_cc.

          CLEAR:wa_prof_gl.
          READ TABLE ta_prof_gl INTO wa_prof_gl WITH KEY bukrs = wa_fagll03-bukrs
                                                         saknr = wa_fagll03-hkont
                                                         BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-fin_pnl_main = wa_prof_gl-final_plm.
            wa_final-fin_pnl_sub  = wa_prof_gl-final_pls.
            gv_remark = text-t32. "Company Code + GL
            "EOC By Pooja Adupala on 16.08.2022
          ELSE.
            wa_final-fin_pnl_main = text-t22. "NO TAGGING FOUND
            wa_final-fin_pnl_sub  = text-t22. "NO TAGGING FOUND
            gv_remark  = text-t22. "NO TAGGING FOUND
          ENDIF.
        ENDIF.

      ELSEIF wa_fagll03-bukrs = co_4500 AND  wa_final-fin_pnl_main IS INITIAL AND  wa_final-fin_pnl_sub IS INITIAL . "4500

        IF wa_fagll03-hkont = co_503001.

          IF wa_fagll03-gkont = co_2260011.
            wa_final-fin_pnl_main = text-t24. "inhouse
            wa_final-fin_pnl_sub  = text-t24. "inhouse
            gv_remark = text-t36. "Based on GL 503001
          ELSEIF wa_fagll03-gkont = co_2260012.
            wa_final-fin_pnl_main = text-t25. "trading
            wa_final-fin_pnl_sub  = text-t25. "trading
            gv_remark = text-t36. "Based on GL 503001
          ELSE.
            gv_remark = text-t22. "NO TAGGING FOUND
          ENDIF.

        ELSE.
          "SOC by Pooja Adupala on 16.08.2022
*          READ TABLE ta_prof_cc INTO wa_prof_cc WITH KEY bukrs = wa_fagll03-bukrs
*                                                       kostl = wa_final-fin_kostl
*                                                       BINARY SEARCH.
*          IF sy-subrc = 0.
*            wa_final-fin_pnl_main = wa_prof_cc-final_plm.
*            wa_final-fin_pnl_sub  = wa_prof_cc-final_pls.
*            gv_remark = text-t33. "Company Code + CC
*            CLEAR:wa_prof_cc.

          CLEAR:wa_prevmo.
          READ TABLE ta_prevmo INTO wa_prevmo WITH KEY bukrs = wa_fagll03-bukrs
                                                               saknr = wa_fagll03-hkont
                                                               kostl = wa_final-fin_kostl
                                                               prctr = wa_fagll03-prctr
                                                               BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-fin_pnl_main = wa_prevmo-final_plm.
            wa_final-fin_pnl_sub  = wa_prevmo-final_pls.
            gv_remark = text-t30. "Company Code + GL + CC + PC
            "EOC by Pooja Adupala on 16.08.2022
          ELSE.
            READ TABLE ta_prof_gl INTO wa_prof_gl WITH KEY bukrs = wa_fagll03-bukrs
                                                           saknr = wa_fagll03-hkont
                                                           BINARY SEARCH.
            IF sy-subrc = 0.
              wa_final-fin_pnl_main = wa_prof_gl-final_plm.
              wa_final-fin_pnl_sub  = wa_prof_gl-final_pls.
              gv_remark = text-t32. "Company Code + GL
              CLEAR:wa_prof_gl.

            ELSE.
              wa_final-fin_pnl_main = text-t22. "NO TAGGING FOUND
              wa_final-fin_pnl_sub  = text-t22. "NO TAGGING FOUND
              gv_remark = text-t22. "NO TAGGING FOUND
            ENDIF.
          ENDIF.
        ENDIF.

      ELSEIF wa_fagll03-bukrs = co_4900 AND  wa_final-fin_pnl_main IS INITIAL AND  wa_final-fin_pnl_sub IS INITIAL . "4900

        READ TABLE ta_prof_gl INTO wa_prof_gl WITH KEY bukrs = wa_fagll03-bukrs
                                                       saknr = wa_fagll03-hkont
                                                       BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final-fin_pnl_main = wa_prof_gl-final_plm.
          wa_final-fin_pnl_sub  = wa_prof_gl-final_pls.
          gv_remark = text-t32. "Company Code + GL
          CLEAR:wa_prof_gl.

        ELSE.
          wa_final-fin_pnl_main = text-t22. "NO TAGGING FOUND
          wa_final-fin_pnl_sub  = text-t22. "NO TAGGING FOUND
          gv_remark = text-t22. "NO TAGGING FOUND
        ENDIF.

      ELSEIF wa_fagll03-bukrs = co_6100 AND  wa_final-fin_pnl_main IS INITIAL AND  wa_final-fin_pnl_sub IS INITIAL . "6100 "No Logic Given

      ELSEIF wa_fagll03-bukrs IN r_emerging_market_cc AND  wa_final-fin_pnl_main IS INITIAL AND  wa_final-fin_pnl_sub IS INITIAL . "Emerging market " No Logic Given

      ELSEIF wa_fagll03-bukrs+0(1) = '3' AND  wa_final-fin_pnl_main IS INITIAL AND  wa_final-fin_pnl_sub IS INITIAL .

        READ TABLE ta_prevmo INTO wa_prevmo WITH KEY bukrs = wa_fagll03-bukrs
                                                     saknr = wa_fagll03-hkont
                                                     kostl = wa_final-fin_kostl.
        IF sy-subrc = 0.
          wa_final-fin_pnl_main = wa_prevmo-final_plm.
          wa_final-fin_pnl_sub  = wa_prevmo-final_pls.
          gv_remark = text-t35. "Company Code + GL + CC

          CLEAR: wa_prevmo.

        ELSE.
          READ TABLE ta_prof_gl INTO wa_prof_gl WITH KEY bukrs = wa_fagll03-bukrs
                                                         saknr = wa_fagll03-hkont
                                                         BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-fin_pnl_main = wa_prof_gl-final_plm.
            wa_final-fin_pnl_sub  = wa_prof_gl-final_pls.
            gv_remark = text-t32. "Company Code + GL
            CLEAR:wa_prof_gl.

          ELSE.
            wa_final-fin_pnl_main = text-t22. "NO TAGGING FOUND
            wa_final-fin_pnl_sub  = text-t22. "NO TAGGING FOUND
            gv_remark = text-t22. "NO TAGGING FOUND
          ENDIF.
        ENDIF.

      ELSEIF  wa_final-fin_pnl_main IS INITIAL AND  wa_final-fin_pnl_sub IS INITIAL .
        wa_final-fin_pnl_main = text-t22. "NO TAGGING FOUND
        wa_final-fin_pnl_sub  = text-t22. "NO TAGGING FOUND
        gv_remark = text-t22. "NO TAGGING FOUND
      ENDIF.
    ENDIF.
*    ENDIF.

    "SOC by Pooja on 29.09.2022
    IF wa_final-hkont IN r_zone_gl.
      wa_final-fin_pnl_main = ''.
      "SOC by Pooja Adupala on 27.02.2023
*      wa_final-fin_pnl_sub  = wa_final-gl_desc.
      wa_final-fin_pnl_sub  = ''.
      IF wa_final-hkont  = '0005210002'.
        wa_final-fin_pnl_main = 'D. S&D EXPENSES'.
        wa_final-fin_pnl_sub = 'D10. FREIGHT OUTWARD DIRECT'.
      ELSEIF wa_final-hkont  = '0005210007'.
        wa_final-fin_pnl_main = 'D. S&D EXPENSES'.
        wa_final-fin_pnl_sub = 'D20. CHA CHARGES'.
      ELSEIF wa_final-hkont  = '0005210008'.
        wa_final-fin_pnl_main = 'D. S&D EXPENSES'.
        wa_final-fin_pnl_sub = 'D21. NOMINATION CHARGES'.
      ELSEIF wa_final-hkont  = '0005190005'.
        wa_final-fin_pnl_main = 'D. S&D EXPENSES'.
        wa_final-fin_pnl_sub = 'D7. COMMISSION'.
      ENDIF.
      "EOC by Pooja Adupala on 27.02.2023

      gv_remark   = text-t52.
      lv_vbeln = wa_final-xblnr.
      READ TABLE ta_vbrp INTO DATA(wa_vbrp) WITH KEY vbeln = lv_vbeln.
      IF sy-subrc = 0.
        "Start of comment by Gajanan - 29.03.2024 - CR#8000003569
*        READ TABLE ta_tvv1t INTO DATA(wa_tvv1t) WITH KEY kvgr1 = wa_vbrp-kvgr1.
*        IF sy-subrc = 0.
*          "SOC by Pooja Adupala on 27.02.2023
**          wa_final-fin_pnl_main = wa_tvv1t-bezei. "NO TAGGING FOUND
*          wa_final-zone_2 = wa_tvv1t-bezei. "NO TAGGING FOUND
*          "EOC by Pooja Adupala on 27.02.2023
*        ENDIF.
        "End of comment by Gajanan - 29.03.2024 - CR#8000003569

        "SOA by Gajanan - 29.03.2024 - CR#8000003569
        READ TABLE ta_vbak INTO DATA(wa_vbak) WITH KEY vbeln = wa_vbrp-aubel
                                                       BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE ta_tvv1t1 INTO DATA(wa_tvv1t1) WITH KEY kvgr1 = wa_vbak-kvgr1.
          IF sy-subrc = 0.
            wa_final-zone_2 = wa_tvv1t1-bezei. "NO TAGGING FOUND
          ENDIF.
          CLEAR:wa_vbrp,wa_tvv1t1,wa_vbak.
        ENDIF.
        "EOA by Gajanan - 29.03.2024 - CR#8000003569

      ENDIF.

* Start of Change Inc 8000003399 | TR CRDK989166 | Prathamesh Parab | 09.01.2024
* Start of Change Inc 8000003399 | TR CRDK989314 | Prathamesh Parab | 23.01.2024
      IF wa_final-hkont = '5210002'. " Change added by Inc 8000003399 | TR CRDK989330 | Prathamesh Parab | 24.01.2024
        READ TABLE ta_vbrk INTO wa_vbrk WITH KEY vbeln = wa_final-kidno+0(10).
        IF sy-subrc = 0.
*   READ TABLE ta_kna1 INTO wa_kna1 WITH KEY kunnr = wa_final-gkont
          READ TABLE ta_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbrk-kunrg
                                                   BINARY SEARCH.
          IF sy-subrc = 0 AND wa_kna1-vbund IS NOT INITIAL.
            wa_final-fin_pnl_main = 'B. MATERIAL COST'.
            wa_final-fin_pnl_sub = 'B8. FREIGHT OUTWARD INTER COMPANY'.
* Start of Change Inc 8000003399 | TR CRDK989366 | Prathamesh Parab | 30.01.2024
            wa_final-det_pnl = 'B. MATERIAL COST'.
            wa_final-fin_tag = 'B8. FREIGHT OUTWARD INTER COMPANY'.
* End of Change Inc 8000003399 | TR CRDK989366 | Prathamesh Parab | 30.01.2024
            CLEAR: wa_kna1.
          ENDIF.
          CLEAR: wa_vbrk.
        ENDIF.
      ENDIF. " Change added by Inc 8000003399 | TR CRDK989330 | Prathamesh Parab | 24.01.2024
* End of Change Inc 8000003399 | TR CRDK989314 | Prathamesh Parab | 23.01.2024
* End of Change Inc 8000003399 | TR CRDK989166 | Prathamesh Parab | 09.01.2024

      CLEAR:wa_vbrp.
    ENDIF.
    "EOC by Pooja on 29.09.2022


    "SOC by Pooja Adupala on 27.02.2023
    IF wa_final-tagging NE 'IB'.
      CLEAR:wa_final-p_l_head.
    ENDIF.

    IF wa_final-tagging = 'IB'.
      TRANSLATE  wa_final-p_l_head TO UPPER CASE.
      wa_final-fin_pnl_sub = wa_final-p_l_head.
      READ TABLE ta_bu_pnl INTO DATA(wa_bu_pnl) WITH KEY bu_pnl = wa_final-p_l_head.
      IF sy-subrc = 0.
        wa_final-fin_pnl_main = wa_bu_pnl-expenses.
      ELSE.
        wa_final-fin_pnl_main = 'Missing in zmidas_bu_pnl table'.
      ENDIF.
    ENDIF.
    "EOC by Pooja Adupala on 27.02.2023

    IF wa_final-fin_pnl_main IS INITIAL.
      wa_final-fin_pnl_main = text-t22. "NO TAGGING FOUND
    ENDIF.

    IF wa_final-fin_pnl_sub IS INITIAL.
      wa_final-fin_pnl_sub = text-t22. "NO TAGGING FOUND
    ENDIF.

    "Remark
    IF gv_remark IS NOT INITIAL.
      wa_final-remark = gv_remark.
    ENDIF.

    "Segment Tagging
    IF wa_final-bukrs = co_5000.

      IF wa_final-fin_kostl IN r_seg_tag_costc_api.
        wa_final-seg_tag = text-t44. "API

      ELSEIF wa_final-prctr IN r_seg_tag_pc_special.
        wa_final-seg_tag = text-t45. "Specialty

      ELSEIF line_exists( ta_seg_rel[ bukrs = wa_final-bukrs hkont = wa_final-hkont fin_kostl = wa_final-fin_kostl ] ).

        wa_seg_rel = ta_seg_rel[ bukrs = wa_final-bukrs hkont = wa_final-hkont fin_kostl = wa_final-fin_kostl ].

        wa_final-seg_tag = wa_seg_rel-seg_tag.

        "SOC by Pooja Adupala on 18.08.2022
      ELSEIF wa_final-fin_kostl IN r_seg_tag_costc_ipd.
*        wa_final-seg_tag = text-t46. "IPD
        wa_final-seg_tag = text-t46. "IPD-DTM

      ELSEIF wa_final-fin_kostl IN r_seg_tag_costc_ipd2.
*        wa_final-seg_tag = text-t47. "IPD2
        wa_final-seg_tag = text-t47. "Quality - Chargeback
      ELSEIF wa_final-fin_kostl IN r_seg_tag_costc_ipd3.
        wa_final-seg_tag = text-t49. "Biosimilar
        "EOC by Pooja Adupala on 18.08.2022
      ELSE.
        wa_final-seg_tag = text-t48. "Cipla Inc
      ENDIF.
    ENDIF.

    wa_final-midas_date = sy-datum.
    wa_final-midas_time = sy-uzeit.
    wa_final-midas_id   = sy-uname.
    wa_final-midas_type = co_system. "SYSTEM

    IF lv_param_name IS NOT INITIAL.
      TRANSLATE wa_final-sgtxt USING lv_param_name.
      REPLACE ALL OCCURRENCES OF co_new_line IN wa_final-sgtxt WITH space. CONDENSE wa_final-sgtxt.
      REPLACE ALL OCCURRENCES OF co_hor_tab IN wa_final-sgtxt WITH space. CONDENSE wa_final-sgtxt.
      TRANSLATE wa_final-u_zuonr USING lv_param_name.
      REPLACE ALL OCCURRENCES OF co_new_line IN wa_final-u_zuonr WITH space. CONDENSE wa_final-u_zuonr.
      REPLACE ALL OCCURRENCES OF co_hor_tab IN wa_final-u_zuonr WITH space. CONDENSE wa_final-u_zuonr.
      TRANSLATE wa_final-xblnr USING lv_param_name.
      REPLACE ALL OCCURRENCES OF co_new_line IN wa_final-xblnr WITH space. CONDENSE wa_final-xblnr.
      REPLACE ALL OCCURRENCES OF co_hor_tab IN wa_final-xblnr WITH space. CONDENSE wa_final-xblnr.
      TRANSLATE wa_final-zzgkont_ltxt USING lv_param_name.
      REPLACE ALL OCCURRENCES OF co_new_line IN wa_final-zzgkont_ltxt WITH space. CONDENSE wa_final-zzgkont_ltxt.
      REPLACE ALL OCCURRENCES OF co_hor_tab IN wa_final-zzgkont_ltxt WITH space. CONDENSE wa_final-zzgkont_ltxt.
      TRANSLATE wa_final-ord_desc USING lv_param_name.
      REPLACE ALL OCCURRENCES OF co_new_line IN wa_final-ord_desc WITH space. CONDENSE wa_final-ord_desc.
      REPLACE ALL OCCURRENCES OF co_hor_tab IN wa_final-ord_desc WITH space. CONDENSE wa_final-ord_desc.
      TRANSLATE wa_final-cc_name USING lv_param_name.
      REPLACE ALL OCCURRENCES OF co_new_line IN wa_final-cc_name WITH space. CONDENSE wa_final-cc_name.
      REPLACE ALL OCCURRENCES OF co_hor_tab IN wa_final-cc_name WITH space. CONDENSE wa_final-cc_name.
      TRANSLATE wa_final-doc_desc USING lv_param_name.
      REPLACE ALL OCCURRENCES OF co_new_line IN wa_final-doc_desc WITH space. CONDENSE wa_final-doc_desc.
      REPLACE ALL OCCURRENCES OF co_hor_tab IN wa_final-doc_desc WITH space. CONDENSE wa_final-doc_desc.
      TRANSLATE wa_final-gl_desc USING lv_param_name.
      REPLACE ALL OCCURRENCES OF co_new_line IN wa_final-gl_desc WITH space. CONDENSE wa_final-gl_desc.
      REPLACE ALL OCCURRENCES OF co_hor_tab IN wa_final-gl_desc WITH space. CONDENSE wa_final-gl_desc.
    ENDIF.

    "SOC by Pooja Adupala on 17.08.2022
    wa_final-update_from = co_updatefrom.
    wa_final-midas_type = co_updatetype.
    wa_final-midas_id = sy-uname.

***SOA by Gajanan - 25.09.2024 - CR#8000003569
    "Final Vendor
    IF wa_final-koart = 'D' OR wa_final-koart = 'K'.

      IF wa_final-gkart = 'D'.

        CLEAR: lv_lifnr.
        lv_lifnr = |{ wa_final-gkont ALPHA = IN }|.

        SELECT SINGLE name1 FROM kna1 INTO @DATA(lv_name) WHERE kunnr = @lv_lifnr.

        IF sy-subrc = 0.
          wa_final-fin_ven = lv_name.
          CLEAR: lv_name.
        ENDIF.

      ELSEIF wa_final-gkart = 'K'.

        CLEAR: lv_lifnr.
        lv_lifnr = |{ wa_final-gkont ALPHA = IN }|.

        SELECT SINGLE name1 FROM lfa1 INTO @lv_name WHERE lifnr = @lv_lifnr.

        IF sy-subrc = 0.
          wa_final-fin_ven = lv_name.
          CLEAR: lv_name.
        ENDIF.

      ENDIF.

    ELSEIF wa_final-koart = 'S'.

      IF wa_final-u_zuonr = 'BM EXPS'.

        wa_final-fin_ven = 'BM Expenses'.

      ELSEIF wa_final-blart = 'EP'.
        wa_final-fin_ven = 'EAS Provision'.

      ELSEIF wa_final-blart = '1A' OR wa_final-blart = '1B'.
        wa_final-fin_ven = 'iExpense'.

      ELSEIF wa_final-blart = 'CJ'.
        wa_final-fin_ven = 'Depot Cash Expense'.

      ELSEIF wa_final-blart = 'P1' OR wa_final-blart = 'P2' OR wa_final-blart = 'P3' OR wa_final-blart = 'P4' OR wa_final-blart = 'P5'.
        wa_final-fin_ven = 'Employee Reimbursement'.

      ELSE.

        CLEAR: lv_lifnr.
        lv_lifnr = |{ wa_final-u_lifnr ALPHA = IN }|.

        SELECT SINGLE name1 FROM lfa1 INTO @lv_name WHERE lifnr =  @lv_lifnr.

        IF sy-subrc = 0.
          wa_final-fin_ven = lv_name.
          CLEAR: lv_name.

        ELSE.

          CLEAR: lv_lifnr.
          lv_lifnr = |{ wa_final-u_zuonr ALPHA = IN }|.

          SELECT SINGLE name1 FROM lfa1 INTO @lv_name WHERE lifnr = @lv_lifnr.

          IF sy-subrc = 0.
            wa_final-fin_ven = lv_name.
            CLEAR: lv_name.

          ELSE.

            CLEAR: lv_lifnr.
            lv_lifnr = |{ wa_final-gkont ALPHA = IN }|.

            SELECT SINGLE name1 FROM lfa1 INTO @lv_name WHERE lifnr = @lv_lifnr.

            IF sy-subrc = 0.
              wa_final-fin_ven = lv_name.
              CLEAR: lv_name.

            ELSE.

              wa_final-fin_ven = 'Others'.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

***EOA by Gajanan - 25.09.2024 - CR#8000003569


*    "Detail P&L
    TRANSLATE   wa_final-fin_pnl_main TO UPPER CASE.
    READ TABLE ta_det_pnl INTO wa_det_pnl WITH KEY fin_pnl_main = wa_final-fin_pnl_main
                                                   BINARY SEARCH.
    IF sy-subrc = 0.
      wa_final-det_pnl = wa_det_pnl-det_pnl.
      CLEAR: wa_det_pnl.
    ELSE.
      CLEAR: wa_final-det_pnl.
    ENDIF.

    "Final Tagging
    TRANSLATE   wa_final-fin_pnl_sub TO UPPER CASE.
    READ TABLE ta_fin_tag INTO wa_fin_tag WITH KEY fin_pnl_sub = wa_final-fin_pnl_sub
                                                   BINARY SEARCH.
    IF sy-subrc = 0.
      wa_final-fin_tag = wa_fin_tag-fin_tag.
      CLEAR: wa_fin_tag.
    ENDIF.

    TRANSLATE wa_final-fin_tag_rx TO UPPER CASE.
    TRANSLATE wa_final-p_l_head TO UPPER CASE.
    TRANSLATE wa_final-fin_pnl_main TO UPPER CASE.
    TRANSLATE wa_final-fin_pnl_sub TO UPPER CASE.
    TRANSLATE wa_final-det_pnl TO UPPER CASE.
    TRANSLATE wa_final-fin_tag TO UPPER CASE.

    READ TABLE ta_t001 INTO DATA(wa_t001) WITH KEY bukrs = wa_final-bukrs.
    IF sy-subrc = 0.
      IF wa_t001-waers = 'UGX'.
        wa_final-dmshb =  wa_final-dmshb * 100.
      ENDIF.
    ENDIF.

    READ TABLE ta_bkpf INTO DATA(wa_bkpf) WITH KEY bukrs = wa_final-bukrs
                                                 belnr = wa_final-belnr
                                                 gjahr = wa_final-gjahr.
    IF sy-subrc = 0.
      wa_final-cpudt = wa_bkpf-cpudt.
      wa_final-u_usnam = wa_bkpf-usnam.
    ENDIF.

    "SOA by Gajanan - 24.08.2025 - CR#8000003569

    "Sold to Party - Country
    IF wa_final-hkont IN r_zone_gl[].
      READ TABLE ta_vbrk INTO DATA(wa_vbrk1) WITH KEY vbeln = wa_final-kidno+0(10) BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE ta_land1 INTO DATA(wa_land1) WITH KEY kunnr = wa_vbrk1-kunag BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE ta_country INTO DATA(wa_country) WITH KEY land1 = wa_land1-land1 BINARY SEARCH.
          IF sy-subrc = 0.
            wa_final-sp_country = wa_country-landx.
            CLEAR:wa_country.
          ENDIF.
          CLEAR:wa_land1.
        ENDIF.
        CLEAR: wa_vbrk1.
      ENDIF.
    ENDIF.

    IF wa_final-hkont IN r_prov_dobtful_dbt_gl[].
      CLEAR: lv_lifnr1.
      lv_lifnr1 = |{ wa_final-u_zuonr ALPHA = IN }|.
      READ TABLE ta_land1_1 INTO DATA(wa_land1_1) WITH KEY kunnr = lv_lifnr1 BINARY SEARCH.
      IF sy-subrc = 0.
        READ TABLE ta_country_1 INTO DATA(wa_country_1) WITH KEY land1 = wa_land1_1-land1 BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final-sp_country = wa_country_1-landx.
          CLEAR:wa_country_1.
        ENDIF.
        CLEAR:wa_land1_1.
      ENDIF.
    ENDIF.

    "EOA by Gajanan - 24.08.2025 - CR#8000003569


    APPEND wa_final TO ta_final.
    "EOC by Pooja Adupala on 17.08.2022
    CLEAR: wa_fagll03,wa_final,wa_coas,wa_cskt,wa_csks,wa_t003t,wa_skat,wa_t003p,wa_prof_gl,wa_reimb,wa_pnl_hd,wa_elem,gv_pnl,gv_pnl2,
           wa_prevmo,wa_prof_cc,wa_prof_of,gv_remark,wa_det_pnl,wa_fin_tag,wa_grp,wa_subgrp,wa_gl_exclud,wa_eu_tag,wa_bkpf,wa_t001,lv_name,lv_lifnr.
  ENDLOOP.


  IF NOT ta_final[] IS INITIAL.
    SORT ta_final BY spmon bukrs belnr hkont buzei dmshb DESCENDING.
    DELETE ADJACENT DUPLICATES FROM ta_final COMPARING spmon bukrs belnr hkont buzei.
    "Update table ZMIDAS_FAGLL03
    MODIFY zmidas_fagll03 FROM TABLE ta_final.
    IF sy-subrc = 0.
      COMMIT WORK.

      MESSAGE text-s02 TYPE co_s. "Table ZMIDAS_FAGLL03 updated successfully
    ELSE.
      ROLLBACK WORK.
    ENDIF.
  ENDIF.

ENDFORM.
*&      Form  HIDE_SEL_OPT_INTERVAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM hide_sel_opt_interval.
*-- Variable Declarations
  DATA: restrict TYPE sscr_restrict,
        opt_list TYPE sscr_opt_list_tab WITH HEADER LINE,
        ass_tab  TYPE sscr_ass_tab WITH HEADER LINE.

*-- Constants Declarations
  CONSTANTS:
    c_name_all TYPE rsrestrict-optionlist VALUE 'ALL',
    c_kind_s   TYPE rsscr-kind            VALUE 'S',
    c_s_bukrs  TYPE rsrestrict-objectname VALUE 'S_BUKRS',
    c_main_i   TYPE rsrestrict-sign       VALUE 'I'.

*-- Options fill
  opt_list-name       = c_name_all.
  opt_list-options-eq = abap_true.
  APPEND opt_list TO restrict-opt_list_tab.

*-- Hide ranges
  CLEAR ass_tab.
  ass_tab-kind    = c_kind_s.
  ass_tab-name    = c_s_bukrs.
  ass_tab-sg_main = c_main_i.
  ass_tab-sg_addy = space.
  ass_tab-op_main = c_name_all.
  APPEND ass_tab TO restrict-ass_tab.

*-- Interval restrictions
  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction            = restrict
    EXCEPTIONS
      too_late               = 1
      repeated               = 2
      selopt_without_options = 3
      selopt_without_signs   = 4
      invalid_sign           = 5
      empty_option_list      = 6
      invalid_kind           = 7
      repeated_kind_a        = 8
      OTHERS                 = 9.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_GL_ITEMS_DIRECT
*&---------------------------------------------------------------------*
*  SAP Note 3185840 - replacement for
*     SUBMIT fagl_account_items_gl ... AND RETURN  (FAGLL03)
*     + cl_salv_bs_runtime_info=>get_data_ref
*
*  Reads the same rows FAGLL03 returned for that call:
*     G/L-view line items of ledger 0L (ACDOCA) for the documents in
*     IT_DOCS, account IN s_saknr, posting date IN s_budat, all items,
*     entry-view fields merged from BSEG/BKPF, in FAGLPOSX layout
*     (TY_FAGLL03), then passed through BAdI FAGL_ITEMS_CH_DATA
*     (fills custom field ZZGKONT_LTXT exactly as FAGLL03 did).
*
*  Released CDS views (element -> base field verified against the
*  system export DDLS_BASE_FIELDS):
*     I_JournalEntryItem        = ACDOCA   (G/L view)
*     I_JournalEntry            = BKPF     (XBLNR, XBLNR_ALT, USNAM)
*     I_OperationalAcctgDocItem = BSEG     (PSWBT, PSWSL, QBSHB, KIDNO, BUZID)
*----------------------------------------------------------------------*
*  -->  it_docs    ta_bseg_fagll03  (bukrs belnr gjahr buzei hkont)
*  -->  iv_gjahr   fiscal year of the current package
*  <--  ct_items   ta_fagll03       (TYPE TABLE OF ty_fagll03)
*----------------------------------------------------------------------*
FORM get_gl_items_direct
  USING    it_docs   LIKE ta_bseg
           iv_gjahr  TYPE gjahr
  CHANGING ct_items  LIKE ta_fagll03.

  CONSTANTS lc_ledger TYPE fins_ledger VALUE '0L'.

  " TY_FAGLL03 + DOCLN: DOCLN keeps every G/L-view line distinct, so the
  " implicit duplicate removal of FOR ALL ENTRIES cannot merge two split
  " lines of the same BUZEI (same multiplicity as the FAGLL03 G/L view).
  TYPES: BEGIN OF lty_item.
           INCLUDE TYPE ty_fagll03.
  TYPES:   docln TYPE c LENGTH 6,            " ACDOCA-DOCLN is CHAR 6
         END OF lty_item.

  DATA: lt_sel   TYPE STANDARD TABLE OF lty_item,
        lt_items TYPE STANDARD TABLE OF ty_fagll03,
        lt_badi  TYPE faglposx_t,
        lo_badi  TYPE REF TO if_ex_fagl_items_ch_data,
        lv_impl  TYPE c LENGTH 1.

  CHECK it_docs IS NOT INITIAL.

  "--- 1. Same rows FAGLL03 produced: GL view, ledger 0L, all items ----
  SELECT
      gl~companycode                      AS bukrs,
      gl~accountingdocument               AS belnr,
      gl~fiscalyear                       AS gjahr,
      gl~accountingdocumentitem           AS buzei,
      gl~ledgergllineitem                 AS docln,
      gl~glaccount                        AS hkont,
      gl~glaccount                        AS konto,
      gl~accountingdocumenttype           AS blart,
      gl~documentdate                     AS bldat,
      gl~postingdate                      AS budat,
      gl~postingkey                       AS bschl,
      gl~debitcreditcode                  AS shkzg,
      gl~amountincompanycodecurrency      AS dmshb,     " ACDOCA-HSL  = FAGLPOSX-DMSHB (signed LC amount)
      gl~companycodecurrency              AS hwaer,     " ACDOCA-RHCUR
      gl~amountintransactioncurrency      AS wrshb,     " ACDOCA-WSL  = FAGLPOSX-WRSHB (signed doc-curr amount)
      gl~globalcurrency                   AS hwae2,     " ACDOCA-RKCUR
      gl~taxcode                          AS mwskz,
      gl~clearingaccountingdocument       AS augbl,
      gl~profitcenter                     AS prctr,
      gl~costcenter                       AS kostl,     " ACDOCA-RCNTR
      gl~orderid                          AS aufnr,
      gl~segment                          AS segment,
      gl~documentitemtext                 AS sgtxt,
      gl~financialaccounttype             AS koart,
      gl~offsettingaccount                AS gkont,     " ACDOCA-GKONT (note 2588267)
      gl~offsettingaccounttype            AS gkart,     " ACDOCA-GKOAR
      gl~invoicereference                 AS rebzg,
      gl~assignmentreference              AS u_zuonr,   " ACDOCA-ZUONR
      gl~supplier                         AS u_lifnr,   " ACDOCA-LIFNR
      gl~purchasingdocument               AS u_ebeln,   " ACDOCA-EBELN
      hd~documentreferenceid              AS xblnr,     " BKPF-XBLNR
      hd~alternativereferencedocument     AS u_xblnr_alt, " BKPF-XBLNR_ALT
      hd~accountingdoccreatedbyuser       AS u_usnam,   " BKPF-USNAM
      ev~absltamtinbalancetransaccrcy     AS pswbt,     " BSEG-PSWBT (FAGLPOSX-PSWBT is absolute)
      ev~balancetransactioncurrency       AS pswsl,     " BSEG-PSWSL
      ev~withholdingtaxamount             AS qbshb,     " BSEG-QBSHB (signed)
      ev~paymentreference                 AS kidno,     " BSEG-KIDNO
      ev~accountingdocumentitemtype       AS buzid      " BSEG-BUZID
    FROM i_journalentryitem AS gl
      INNER JOIN i_journalentry AS hd
        ON  hd~companycode        = gl~companycode
        AND hd~accountingdocument = gl~accountingdocument
        AND hd~fiscalyear         = gl~fiscalyear
      LEFT OUTER JOIN i_operationalacctgdocitem AS ev
        ON  ev~companycode            = gl~companycode
        AND ev~accountingdocument     = gl~accountingdocument
        AND ev~fiscalyear             = gl~fiscalyear
        AND ev~accountingdocumentitem = gl~accountingdocumentitem
    FOR ALL ENTRIES IN @it_docs
    WHERE gl~ledger             = @lc_ledger
      AND gl~companycode        = @it_docs-bukrs
      AND gl~accountingdocument = @it_docs-belnr
      AND gl~fiscalyear         = @iv_gjahr
      AND gl~glaccount         IN @s_saknr
      AND gl~postingdate       IN @s_budat
    INTO CORRESPONDING FIELDS OF TABLE @lt_sel.

  CHECK lt_sel IS NOT INITIAL.

  SORT lt_sel BY bukrs belnr gjahr docln.
  MOVE-CORRESPONDING lt_sel TO lt_items.

  "--- 2. Derived field FAGLPOSX-JAMON ('YYYY/MM') ----------------------
  LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).
    <ls_item>-jamon = |{ <ls_item>-budat(4) }/{ <ls_item>-budat+4(2) }|.
  ENDLOOP.

  "--- 3. Run the same BAdI FAGLL03 ran (fills ZZGKONT_LTXT etc.) ------
  CALL METHOD cl_exithandler=>get_instance
    EXPORTING
      exit_name              = 'FAGL_ITEMS_CH_DATA'
      null_instance_accepted = abap_true
    IMPORTING
      act_imp_existing       = lv_impl
    CHANGING
      instance               = lo_badi.

  IF lv_impl IS NOT INITIAL AND lo_badi IS BOUND.
    MOVE-CORRESPONDING lt_items TO lt_badi.
    lo_badi->change_items( CHANGING ct_items = lt_badi ).
    MOVE-CORRESPONDING lt_badi TO lt_items.
  ENDIF.

  APPEND LINES OF lt_items TO ct_items.

ENDFORM.
