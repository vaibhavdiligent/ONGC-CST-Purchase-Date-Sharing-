*&---------------------------------------------------------------------*
*&  Include           ZMIDAS_FAGLL03_FORMS
*&---------------------------------------------------------------------*
*& Title             : Midas 3 ( BU P&L Automation )
*& Program           : ZMIDAS_FAGLL03_COPY_FORMS
*& Tcode             :
*& Author            : Gajanan Patil
*& Functional Design : Phanindra Bagana
*& Technical Design  : Gajanan Patil
*& Creation Date     : 17.05.2022
*& Transport Request : CRDK983087
*& Change request    : 8000007907
*& Description       : Call FAGLL03 Tcode to update custom table
*&---------------------------------------------------------------------*
*& Author            : Pooja Adupala
*& Functional Design : Shashank Sharma
*& Technical Design  : Pooja Adupala
*& Creation Date     : 16.07.2023
*& Transport Request : CRDK987477
*& Change request    : 8000009091
*& Description       : Amount in USD need to be fetched from wrshb when
*&                     psws1 is usd
*&---------------------------------------------------------------------*
* Modification Information
* Date            : 09.01.2024
* Author          : Prathamesh Parab
* Functional      : Arijit Podder
* Change request  : 8000003399
* Transport number: CRDK989166
* Description     : Midas 3 New Development
*&---------------------------------------------------------------------*
* Modification Information
* Date            : 29.03.2024
* Author          : Gajanan Patil
* Functional      : Arijit Poddar
* Change request  : 8000003569
* Transport number: CRDK990287
* Description     : Midas3 Project ticket
*----------------------------------------------------------------------*
* Modification Information
* Date            : 27.04.2026
* Author          : Gajanan Patil
* Functional      : Arijit Poddar
* Change request  : 8000004929
* Transport number: CRDK9A00UZ
* Description     : Global People Cost Changes
*----------------------------------------------------------------------*
*&      Form  REFRESH_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
* Modification Information
* Date            : 01.11.2026
* Description     : CRS job ZMIDAS_FAGLL03_COPY cancelled after 89 sec
*                   (memory). Month processed in packages of 20,000 rows
*                   (FORM process_packages / get_pkg_lookups / save_pc_rows
*                   / clear_pkg); FAGLFLEXT read via FGL_FAGLFLEXT.
* 02.11.2026 package read by key table instead of held cursor
*                   (DBSQL_INVALID_CURSOR after COMMIT WORK on HANA).
*                   Business logic in GET_DATA / UPDATE_TAB unchanged.
*----------------------------------------------------------------------*
FORM refresh_tab.

  REFRESH: ta_fagll03[],ta_final[],ta_coas[],ta_t003p[],ta_csks[],ta_cskt[],ta_kostl[],ta_t003t[],ta_skat[],ta_bkpf[],ta_bseg[],ta_reimb[],ta_pnl_hd[],
           ta_elem[],ta_prof_gl[],ta_prof_cc[],ta_prevmo[],ta_prevmo[],ta_grp[],ta_subgrp[],ta_fin_tag[],ta_det_pnl[],r_saknr[],r_kostl[],r_not_cons[],
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

  DATA: lta_sel_op TYPE rsds_selopt_t,
        lwa_sel_op LIKE LINE OF lta_sel_op,
        lta_france TYPE rsds_frange_t,
        lwa_france LIKE LINE OF lta_france,
        lv_from    TYPE i,
        lv_to      TYPE i,
        lv_div     TYPE i,
        lv_mod     TYPE f.
  DATA:lv_date_temp TYPE sy-datum,
       lv_period    TYPE t009b-poper,
       lv_fy        TYPE t009b-bdatj.
  DATA: lv_count   TYPE char10,
        lv_index1  TYPE char10,
        lv_fcurr1  TYPE tcurr-fcurr,
        lv_ffact1  TYPE tcurr-ffact,
        lv_excrate TYPE tcurr-ukurs.

  "Fetch GL sets from table SETLEAF
  SELECT * FROM setleaf INTO TABLE @DATA(ta_setleaf)
  WHERE setname IN ( @co_gl_set , @co_not_consider , @co_sal_wages , @co_dist_opex ,@co_dist_secfreight, @co_staff_welfare , @co_rnd_gl , @co_rnd_cc ,
                    @co_europe_cc , @co_south_africa_cc , @co_emerging_market_cc , @co_main_cat_write_back , @co_main_cat_forg_exg , @co_ib_cc ,
                    @co_ach_awards_order , @co_seg_tag_costc_api, @co_seg_tag_pc_special, @co_seg_tag_costc_ipd, @co_seg_tag_costc_ipd2,
                    @co_seg_tag_costc_ipd3,@co_fagll03_zone_gl,@co_z1000_rx_gl,@co_z1000_gx_gl,@co_z1000_corporate,@co_z1000_rd,@co_sa_cc,
                    @co_eu_cc,@co_em_cc,@co_na_fr_cc,@co_corp_cc,@co_sub_sah_cc,@co_midas_gl_pc,@co_midas_cc_pc,@co_rx_other_exp_io,@co_prov_dobtful_dbt_gl,
                    @co_north_africa_cc,@co_mor_tag_midas3,@co_central_gl ).

  IF NOT ta_setleaf IS INITIAL.
    SORT ta_setleaf BY setname.

    "ZMIDAS_GL_MAINSET - START
    DATA(ta_gl_set) = ta_setleaf.
    DELETE ta_gl_set WHERE setname <> co_gl_set.

    IF NOT ta_gl_set IS INITIAL.

      LOOP AT ta_gl_set INTO DATA(wa_gl_set).
        r_saknr-sign = wa_gl_set-valsign.
        r_saknr-option = wa_gl_set-valoption.
        r_saknr-low = wa_gl_set-valfrom.
        r_saknr-high = wa_gl_set-valto.
        APPEND r_saknr.

        CLEAR: wa_gl_set.
      ENDLOOP.
      REFRESH:ta_gl_set[].
    ENDIF.
    "ZMIDAS_GL_MAINSET - END

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

    "SOC by Pooja Adupala on 19.08.2022
    "BU_NA_SEG_TAG_COSTC_IPD2 - START
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
    "BU_NA_SEG_TAG_COSTC_IPD2 - END
    "EOC by Pooja Adupala on 19.08.2022

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

    "SOC by pooja on 25.11.2022
    "ZMIDAS_GL_MAINSET - START
    DATA(ta_z1000_gx_gl) = ta_setleaf.
    DELETE ta_z1000_gx_gl WHERE setname <> co_z1000_gx_gl.

    IF NOT ta_z1000_gx_gl IS INITIAL.

      LOOP AT ta_z1000_gx_gl INTO DATA(wa_z1000_gx_gl).
        r_z1000_gx_gl-sign = wa_z1000_gx_gl-valsign.
        r_z1000_gx_gl-option = wa_z1000_gx_gl-valoption.
        r_z1000_gx_gl-low = wa_z1000_gx_gl-valfrom.
        r_z1000_gx_gl-high = wa_z1000_gx_gl-valto.
        APPEND r_z1000_gx_gl.

        CLEAR: wa_z1000_gx_gl.
      ENDLOOP.
      REFRESH:ta_z1000_gx_gl[].
    ENDIF.

    DATA(ta_z1000_rx_gl) = ta_setleaf.
    DELETE ta_z1000_rx_gl WHERE setname <> co_z1000_rx_gl.

    IF NOT ta_z1000_rx_gl IS INITIAL.

      LOOP AT ta_z1000_rx_gl INTO DATA(wa_z1000_rx_gl).
        r_z1000_rx_gl-sign = wa_z1000_rx_gl-valsign.
        r_z1000_rx_gl-option = wa_z1000_rx_gl-valoption.
        r_z1000_rx_gl-low = wa_z1000_rx_gl-valfrom.
        r_z1000_rx_gl-high = wa_z1000_rx_gl-valto.
        APPEND r_z1000_rx_gl.

        CLEAR: wa_z1000_gx_gl.
      ENDLOOP.
      REFRESH:ta_z1000_rx_gl[].
    ENDIF.

    DATA(ta_z1000_rd) = ta_setleaf.
    DELETE ta_z1000_rd WHERE setname <> co_z1000_rd.

    IF NOT ta_z1000_rd IS INITIAL.

      LOOP AT ta_z1000_rd INTO DATA(wa_z1000_rd).
        r_z1000_rd-sign = wa_z1000_rd-valsign.
        r_z1000_rd-option = wa_z1000_rd-valoption.
        r_z1000_rd-low = wa_z1000_rd-valfrom.
        r_z1000_rd-high = wa_z1000_rd-valto.
        APPEND r_z1000_rd.
        CLEAR: wa_z1000_rd.
      ENDLOOP.
      REFRESH:ta_z1000_rd[].
    ENDIF.

    DATA(ta_z1000_corporate) = ta_setleaf.
    DELETE ta_z1000_corporate WHERE setname <> co_z1000_corporate.

    IF NOT ta_z1000_corporate IS INITIAL.

      LOOP AT ta_z1000_corporate INTO DATA(wa_z1000_corporate).
        r_z1000_corporate-sign = wa_z1000_corporate-valsign.
        r_z1000_corporate-option = wa_z1000_corporate-valoption.
        r_z1000_corporate-low = wa_z1000_corporate-valfrom.
        r_z1000_corporate-high = wa_z1000_corporate-valto.
        APPEND r_z1000_corporate.
        CLEAR: wa_z1000_corporate.
      ENDLOOP.
      REFRESH:ta_z1000_corporate[].
    ENDIF.

    DATA(ta_sa_cc) = ta_setleaf.
    DELETE ta_sa_cc WHERE setname <> co_sa_cc.

    IF NOT ta_sa_cc IS INITIAL.

      LOOP AT ta_sa_cc INTO DATA(wa_sa_cc).
        r_sa_cc-sign = wa_sa_cc-valsign.
        r_sa_cc-option = wa_sa_cc-valoption.
        r_sa_cc-low = wa_sa_cc-valfrom.
        r_sa_cc-high = wa_sa_cc-valto.
        APPEND r_sa_cc.

        CLEAR: wa_sa_cc.
      ENDLOOP.
      REFRESH:ta_sa_cc[].
    ENDIF.

    DATA(ta_eu_cc) = ta_setleaf.
    DELETE ta_eu_cc WHERE setname <> co_eu_cc.

    IF NOT ta_eu_cc IS INITIAL.

      LOOP AT ta_eu_cc INTO DATA(wa_eu_cc).
        r_eu_cc-sign = wa_eu_cc-valsign.
        r_eu_cc-option = wa_eu_cc-valoption.
        r_eu_cc-low = wa_eu_cc-valfrom.
        r_eu_cc-high = wa_eu_cc-valto.
        APPEND r_eu_cc.

        CLEAR: wa_eu_cc.
      ENDLOOP.
      REFRESH:ta_eu_cc[].
    ENDIF.

    DATA(ta_em_cc) = ta_setleaf.
    DELETE ta_em_cc WHERE setname <> co_em_cc.

    IF NOT ta_em_cc IS INITIAL.

      LOOP AT ta_em_cc INTO DATA(wa_em_cc).
        r_em_cc-sign = wa_em_cc-valsign.
        r_em_cc-option = wa_em_cc-valoption.
        r_em_cc-low = wa_em_cc-valfrom.
        r_em_cc-high = wa_em_cc-valto.
        APPEND r_em_cc.

        CLEAR: wa_em_cc.
      ENDLOOP.
      REFRESH:ta_em_cc[].
    ENDIF.

    DATA(ta_na_fr_cc) = ta_setleaf.
    DELETE ta_na_fr_cc WHERE setname <> co_na_fr_cc.

    IF NOT ta_na_fr_cc IS INITIAL.

      LOOP AT ta_na_fr_cc INTO DATA(wa_na_fr_cc).
        r_na_fr_cc-sign = wa_na_fr_cc-valsign.
        r_na_fr_cc-option = wa_na_fr_cc-valoption.
        r_na_fr_cc-low = wa_na_fr_cc-valfrom.
        r_na_fr_cc-high = wa_na_fr_cc-valto.
        APPEND r_na_fr_cc.

        CLEAR: wa_na_fr_cc.
      ENDLOOP.
      REFRESH:ta_na_fr_cc[].
    ENDIF.

    DATA(ta_corp_cc) = ta_setleaf.
    DELETE ta_corp_cc WHERE setname <> co_corp_cc.

    IF NOT ta_corp_cc IS INITIAL.

      LOOP AT ta_corp_cc INTO DATA(wa_corp_cc).
        r_corp_cc-sign = wa_corp_cc-valsign.
        r_corp_cc-option = wa_corp_cc-valoption.
        r_corp_cc-low = wa_corp_cc-valfrom.
        r_corp_cc-high = wa_corp_cc-valto.
        APPEND r_corp_cc.

        CLEAR: wa_corp_cc.
      ENDLOOP.
      REFRESH:ta_corp_cc[].
    ENDIF.

    DATA(ta_sub_sah_cc) = ta_setleaf.
    DELETE ta_sub_sah_cc WHERE setname <> co_sub_sah_cc.

    IF NOT ta_sub_sah_cc IS INITIAL.

      LOOP AT ta_sub_sah_cc INTO DATA(wa_sub_sah_cc).
        r_sub_sah_cc-sign = wa_sub_sah_cc-valsign.
        r_sub_sah_cc-option = wa_sub_sah_cc-valoption.
        r_sub_sah_cc-low = wa_sub_sah_cc-valfrom.
        r_sub_sah_cc-high = wa_sub_sah_cc-valto.
        APPEND r_sub_sah_cc.

        CLEAR: wa_sub_sah_cc.
      ENDLOOP.
      REFRESH:ta_sub_sah_cc[].
      "EOC by pooja on 25.11.2022
    ENDIF.

    DATA(ta_gl_pc) = ta_setleaf.
    DELETE ta_gl_pc WHERE setname <> co_midas_gl_pc.
    IF NOT ta_gl_pc IS INITIAL.

      LOOP AT ta_gl_pc INTO DATA(wa_gl_pc).
        r_midas3_gl_pc-sign = wa_gl_pc-valsign.
        r_midas3_gl_pc-option = wa_gl_pc-valoption.
        r_midas3_gl_pc-low = wa_gl_pc-valfrom.
        r_midas3_gl_pc-high = wa_gl_pc-valto.
        APPEND r_midas3_gl_pc.

        CLEAR: wa_gl_pc.
      ENDLOOP.
      REFRESH:ta_gl_pc[].
    ENDIF.

    DATA(ta_cc_pc) = ta_setleaf.
    DELETE ta_cc_pc WHERE setname <> co_midas_cc_pc.
    IF NOT ta_cc_pc IS INITIAL.

      LOOP AT ta_cc_pc INTO DATA(wa_cc_pc).
        r_midas3_cc_pc-sign = wa_cc_pc-valsign.
        r_midas3_cc_pc-option = wa_cc_pc-valoption.
        r_midas3_cc_pc-low = wa_cc_pc-valfrom.
        r_midas3_cc_pc-high = wa_cc_pc-valto.
        APPEND r_midas3_cc_pc.

        CLEAR: wa_cc_pc.
      ENDLOOP.
      REFRESH:ta_cc_pc[].
    ENDIF.

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

    "SOC by Gajanan - 06.10.2025 - CR#8000004621
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
    "EOC by Gajanan - 06.10.2025 - CR#8000004621

    "SOA by Gajanan - 27.04.2026 - CR#8000004929
    DATA(ta_north_africa_cc) = ta_setleaf.
    DELETE ta_north_africa_cc WHERE setname <> co_north_africa_cc.

    IF NOT ta_north_africa_cc IS INITIAL.

      LOOP AT ta_north_africa_cc INTO DATA(wa_north_africa_cc).
        r_north_africa_cc-sign = wa_north_africa_cc-valsign.
        r_north_africa_cc-option = wa_north_africa_cc-valoption.
        r_north_africa_cc-low = wa_north_africa_cc-valfrom.
        r_north_africa_cc-high = wa_north_africa_cc-valto.
        APPEND r_north_africa_cc.

        CLEAR: wa_north_africa_cc.
      ENDLOOP.
      REFRESH:ta_north_africa_cc[].
    ENDIF.

    DATA(ta_mor_tag_midas3) = ta_setleaf.
    DELETE ta_mor_tag_midas3 WHERE setname <> co_mor_tag_midas3.

    IF NOT ta_mor_tag_midas3 IS INITIAL.

      LOOP AT ta_mor_tag_midas3 INTO DATA(wa_mor_tag_midas3).
        r_mor_tag_midas3-sign = wa_mor_tag_midas3-valsign.
        r_mor_tag_midas3-option = wa_mor_tag_midas3-valoption.
        r_mor_tag_midas3-low = wa_mor_tag_midas3-valfrom.
        r_mor_tag_midas3-high = wa_mor_tag_midas3-valto.
        APPEND r_mor_tag_midas3.

        CLEAR: wa_mor_tag_midas3.
      ENDLOOP.
      REFRESH:ta_mor_tag_midas3[].
    ENDIF.

    DATA(ta_central_gl) = ta_setleaf.
    DELETE ta_central_gl WHERE setname <> co_central_gl.

    IF NOT ta_central_gl IS INITIAL.

      LOOP AT ta_central_gl INTO DATA(wa_central_gl).
        r_central_gl-sign = wa_central_gl-valsign.
        r_central_gl-option = wa_central_gl-valoption.
        r_central_gl-low = wa_central_gl-valfrom.
        r_central_gl-high = wa_central_gl-valto.
        APPEND r_central_gl.

        CLEAR: wa_central_gl.
      ENDLOOP.
      REFRESH:ta_central_gl[].
    ENDIF.


    "EOA by Gajanan - 27.04.2026 - CR#8000004929

  ENDIF.

  IF p_spmon IS NOT INITIAL.
    lv_date_temp = |{ p_spmon  }| & || & |{ co_01 } |.
    CONDENSE lv_date_temp.

    "Get first and last day of period
    CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
      EXPORTING
        iv_date             = lv_date_temp
      IMPORTING
        ev_month_begin_date = lv_startdate
        ev_month_end_date   = lv_enddate.

    "Convert Date
    CONVERT DATE lv_startdate INTO INVERTED-DATE lv_startdate_1.
    CONVERT DATE lv_enddate INTO INVERTED-DATE lv_enddate_1.
  ENDIF.

  SELECT kurst
    fcurr
    tcurr
    gdatu
    ukurs
    ffact FROM tcurr
  INTO TABLE ta_tcurr_usd
  WHERE kurst = 'M'
   AND fcurr = 'USD'
   AND tcurr = 'INR'
   AND gdatu GE   lv_enddate_1  AND gdatu LE  lv_startdate_1.


  IF ta_tcurr_usd  IS NOT INITIAL.

    LOOP AT ta_tcurr_usd INTO  DATA(wa_tcurr_usd).
      MOVE-CORRESPONDING wa_tcurr_usd TO wa_tcurr_usd1.
      COLLECT wa_tcurr_usd1 INTO ta_tcurr_usd1.
      CLEAR:wa_tcurr_usd1.
      DATA(lv_ffact_usd) = wa_tcurr_usd-ffact.
    ENDLOOP.

    DATA(lv_lines1) = lines( ta_tcurr_usd ).

    CLEAR:lv_exrate1.
    READ TABLE ta_tcurr_usd1 INTO wa_tcurr_usd1 INDEX 1.
    IF sy-subrc = 0 AND lv_ffact_usd IS NOT INITIAL.
      lv_exrate1 = wa_tcurr_usd1-ukurs / lv_lines1 / lv_ffact_usd.
    ELSE.
      lv_exrate1 = wa_tcurr_usd1-ukurs / lv_lines1 .
    ENDIF.
  ENDIF.



  "Fetch data from table ZMIDAS_FAGLL03

  lv_date_temp = |{ p_spmon }| & || & |{ '01' } |.
  CONDENSE lv_date_temp.

  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      i_date         = lv_date_temp
      i_periv        = 'V3'
    IMPORTING
      e_buper        = lv_period
      e_gjahr        = lv_fy
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.

  IF lv_fy IS NOT INITIAL AND r_midas3_gl_pc IS NOT INITIAL AND r_midas3_cc_pc IS NOT INITIAL.
* **BEGIN OF CHANGE 01.11.2026 - S/4HANA: FAGLFLEXT is obsolete, read its
* replacement object FGL_FAGLFLEXT (CDS compatibility view on ACDOCA,
* same field names). A CDS view needs the new Open SQL syntax
* (comma list, @host variables); the old list form gives a syntax error.
* SE11 -> FAGLFLEXT -> Replacement Object = FGL_FAGLFLEXT.
    SELECT ryear, objnr00, objnr01, objnr02, objnr03, objnr04,
           objnr05, objnr06, objnr07, objnr08, drcrk, rpmax,
           racct, rbukrs, prctr,
           hsl01, hsl02, hsl03, hsl04, hsl05, hsl06,
           hsl07, hsl08, hsl09, hsl10, hsl11, hsl12
      FROM fgl_faglflext
      WHERE ryear  = @lv_fy
        AND rldnr  = '0L'
        AND racct  IN @r_midas3_gl_pc
        AND rbukrs IN @r_midas3_cc_pc
      INTO CORRESPONDING FIELDS OF TABLE @ta_fagl.
* **END OF CHANGE 01.11.2026

    IF ta_fagl IS NOT INITIAL.
      SORT ta_fagl BY racct prctr rbukrs.
      CONCATENATE 'HSL' lv_period+1(2) INTO  lv_col.
    ENDIF.


  ENDIF.

  LOOP AT ta_fagl INTO DATA(wa_fagl).
    DATA(lv_index) = sy-tabix.
    wa_fagl1-spmon = p_spmon.

    MOVE-CORRESPONDING wa_fagl TO wa_fagl1.
    wa_fagl1-lv_col = lv_col.
    IF lv_col = 'HSL01'.
      lv_amnt = wa_fagl-hsl01.
    ELSEIF lv_col = 'HSL02'.
      lv_amnt = wa_fagl-hsl02.
    ELSEIF lv_col = 'HSL03'.
      lv_amnt = wa_fagl-hsl03.
    ELSEIF lv_col = 'HSL04'.
      lv_amnt = wa_fagl-hsl04.
    ELSEIF lv_col = 'HSL05'.
      lv_amnt = wa_fagl-hsl05.
    ELSEIF lv_col = 'HSL06'.
      lv_amnt = wa_fagl-hsl06.
    ELSEIF lv_col = 'HSL07'.
      lv_amnt = wa_fagl-hsl07.
    ELSEIF lv_col = 'HSL08'.
      lv_amnt = wa_fagl-hsl08.
    ELSEIF lv_col = 'HSL09'.
      lv_amnt = wa_fagl-hsl09.
    ELSEIF lv_col = 'HSL10'.
      lv_amnt = wa_fagl-hsl10.
    ELSEIF lv_col = 'HSL11'.
      lv_amnt = wa_fagl-hsl11.
    ELSEIF lv_col = 'HSL12'.
      lv_amnt = wa_fagl-hsl12.
    ENDIF.

    IF wa_fagl-racct EQ lv_hkont AND wa_fagl-prctr NE lv_prctr
        AND wa_fagl-rbukrs EQ lv_bukrs AND lv_index NE 1.
      lv_buzei = lv_buzei + 1.
    ELSEIF lv_buzei = lv_hkont AND wa_fagl-prctr = lv_prctr AND wa_fagl-rbukrs = lv_bukrs.
      lv_buzei = lv_buzei.
    ELSEIF  wa_fagl-racct NE lv_hkont AND lv_index NE 1..
      lv_buzei = '1'.
    ELSEIF lv_index = 1.
      lv_buzei = '1'.
    ENDIF.

    wa_fagl1-buzei = lv_buzei.
    wa_fagl1-lv_amnt = lv_amnt .
    lv_bukrs = wa_fagl1-rbukrs.
    lv_hkont = wa_fagl1-racct.
    lv_prctr = wa_fagl1-prctr.
    COLLECT wa_fagl1 INTO ta_fagl1.
  ENDLOOP.

  IF r_midas3_gl_pc[] IS NOT INITIAL AND r_midas3_cc_pc[] IS NOT INITIAL.
    DELETE FROM zmidas_fagll03 WHERE spmon = p_spmon  AND
                                     bukrs IN r_midas3_cc_pc AND
                                     belnr = '' AND
                                     hkont IN r_midas3_gl_pc.
  ENDIF.

*  IF ta_fagl1 IS NOT INITIAL.

  IF ta_fagl1 IS NOT INITIAL.

    SELECT spras ktopl saknr txt50
     FROM skat INTO TABLE ta_skat_fagl
     FOR ALL ENTRIES IN ta_fagl1
     WHERE spras = co_e
       AND saknr = ta_fagl1-racct.

    IF NOT ta_skat_fagl IS INITIAL.
      SORT ta_skat_fagl BY saknr.
    ENDIF.

    SELECT bukrs,waers
      FROM t001 INTO TABLE @DATA(ta_t001_fagl)
      FOR ALL ENTRIES IN @ta_fagl1
      WHERE bukrs = @ta_fagl1-rbukrs.

    IF ta_t001_fagl IS NOT INITIAL.
      SELECT kurst
      fcurr
      tcurr
      gdatu
      ukurs
      ffact FROM tcurr
   INTO TABLE ta_tcurr_fagl
   FOR ALL ENTRIES IN ta_t001_fagl
   WHERE kurst = 'M'
     AND fcurr = ta_t001_fagl-waers
     AND tcurr = 'INR'
     AND gdatu GE   lv_enddate_1  AND gdatu LE lv_startdate_1.

      IF ta_tcurr_fagl IS NOT INITIAL.
        SORT ta_tcurr_fagl BY fcurr.
        DATA(lv_lines) = lines( ta_tcurr_fagl ).

        CLEAR:lv_count,lv_index.
        LOOP AT ta_tcurr_fagl INTO DATA(wa_tcurr_fagl).
          lv_index = sy-tabix.
          lv_count = lv_count + 1.
          MOVE-CORRESPONDING wa_tcurr_fagl TO wa_tcurr1_fagl.
          COLLECT wa_tcurr1_fagl INTO ta_tcurr1_fagl.

          IF lv_index NE 1 AND wa_tcurr1_fagl-fcurr NE lv_fcurr1.
            CLEAR   lv_excrate.
            READ TABLE ta_tcurr1_fagl INTO DATA(wa_tcurr2_fagl) WITH KEY fcurr = lv_fcurr1.
            IF sy-subrc = 0.
              IF lv_count GT 1.
                DATA(lv_count2) = lv_count - 1.
              ELSE.
                lv_count2 = lv_count.
              ENDIF.
              IF lv_count2 IS NOT INITIAL.
                IF lv_ffact1 IS NOT INITIAL.
                  lv_excrate =  wa_tcurr2_fagl-ukurs / lv_count2 / lv_ffact1.
                ELSE.
                  lv_excrate =  wa_tcurr2_fagl-ukurs / lv_count2 .
                ENDIF.
              ELSE.
                IF lv_ffact1 IS NOT INITIAL .
                  lv_excrate = wa_tcurr2_fagl-ukurs / lv_ffact1.
                ELSE.
                  lv_excrate = wa_tcurr2_fagl-ukurs.
                ENDIF.
              ENDIF.
              wa_excrate_fagl-fcurr = wa_tcurr2_fagl-fcurr.
              wa_excrate_fagl-exch_rate = lv_excrate.
              APPEND wa_excrate_fagl TO ta_excrate_fagl.
              CLEAR:lv_count,wa_excrate,wa_tcurr2_fagl.
            ENDIF.
            lv_count = 1.
          ENDIF.

          lv_ffact1 = wa_tcurr_fagl-ffact .

          IF lv_index = lv_lines.
            READ TABLE ta_tcurr1_fagl INTO wa_tcurr2_fagl WITH KEY fcurr = wa_tcurr1_fagl-fcurr.
            IF sy-subrc = 0.
              IF lv_count IS NOT INITIAL.
                IF lv_ffact1 IS NOT INITIAL.
                  lv_excrate =  wa_tcurr2_fagl-ukurs / lv_count / lv_ffact1.
                ELSE.
                  lv_excrate =  wa_tcurr2_fagl-ukurs / lv_count.
                ENDIF.
              ELSE.
                IF lv_ffact1 IS NOT INITIAL.
                  lv_excrate = wa_tcurr2_fagl-ukurs / lv_ffact1.
                ELSE.
                  lv_excrate = wa_tcurr2_fagl-ukurs.
                ENDIF.
              ENDIF.
              wa_excrate_fagl-fcurr = wa_tcurr2_fagl-fcurr.
              wa_excrate_fagl-exch_rate = lv_excrate.
              APPEND wa_excrate_fagl TO ta_excrate_fagl.
              CLEAR:wa_excrate_fagl,wa_tcurr2_fagl.
            ENDIF.
          ENDIF.

          lv_fcurr1 = wa_tcurr_fagl-fcurr .
          CLEAR:wa_tcurr1_fagl,wa_tcurr_fagl.
        ENDLOOP.

        SORT ta_excrate_fagl BY fcurr.
      ENDIF.

      SELECT  * FROM zmidas3_gl_pc
        INTO TABLE @DATA(ta_midas3_gl_pc)
        FOR ALL ENTRIES IN @ta_fagl1
        WHERE bukrs = @ta_fagl1-rbukrs AND
              prctr = @ta_fagl1-prctr AND
              hkont = @ta_fagl1-racct.
    ENDIF.

    LOOP AT ta_fagl1 INTO DATA(wa_fagl2).
      wa_final-spmon = wa_fagl2-spmon.
      wa_final-bukrs = wa_fagl2-rbukrs.
      wa_final-hkont = wa_fagl2-racct.
      wa_final-prctr = wa_fagl2-prctr.
      wa_final-buzei = wa_fagl2-buzei.
      wa_final-dmshb = wa_fagl2-lv_amnt.
      IF  wa_final-dmshb  IS NOT INITIAL.
        READ TABLE ta_t001_fagl INTO DATA(wa_t001_fagl) WITH KEY bukrs = wa_final-bukrs.
        IF sy-subrc = 0.
          READ TABLE ta_excrate_fagl INTO wa_excrate_fagl WITH KEY fcurr = wa_t001_fagl-waers.
          IF sy-subrc = 0 AND wa_excrate_fagl-exch_rate IS NOT INITIAL.
            wa_final-amnt_inr = wa_final-dmshb  * wa_excrate_fagl-exch_rate.
            IF  wa_final-amnt_inr  IS NOT INITIAL AND lv_exrate1 IS NOT INITIAL.
              wa_final-amnt_usd = wa_final-amnt_inr  / lv_exrate1.
            ENDIF.
          ENDIF.
        ENDIF.
        IF wa_t001_fagl-waers = 'INR'.
          wa_final-amnt_inr  = wa_final-dmshb.
          IF lv_exrate1 IS NOT INITIAL.
            wa_final-amnt_usd  = wa_final-dmshb / lv_exrate1.
          ENDIF.
        ELSEIF  wa_t001_fagl-waers  = 'USD'.
          wa_final-amnt_usd  = wa_final-dmshb.
          wa_final-amnt_inr  = wa_final-dmshb * lv_exrate1.
        ENDIF.
      ENDIF.
      READ TABLE ta_midas3_gl_pc INTO DATA(wa_midas3_gl_pc) WITH KEY bukrs =    wa_final-bukrs
                                                                     prctr =    wa_final-prctr
                                                                     hkont =    wa_final-hkont.
      IF sy-subrc = 0.
        wa_final-fin_pnl_main = wa_midas3_gl_pc-fin_pnl_main.
        wa_final-fin_pnl_sub = wa_midas3_gl_pc-fin_pnl_sub.
        wa_final-remark = text-t53.
        wa_final-tagging = 'IB'.

        IF wa_final-bukrs = co_3800.
          wa_final-zone_2 = co_chl. "Added by Gajanan - 20.02.2025 - CR#8000003569
        ELSE.
          wa_final-zone_2 = 'Rx'.
        ENDIF.

      ENDIF.

*      wa_final-tagging = 'IB'.
*      wa_final-zone_2 = 'Rx'.
      wa_final-det_pnl =  wa_final-fin_pnl_main .
      wa_final-fin_tag =  wa_final-fin_pnl_sub .

      IF wa_final-fin_pnl_main = 'D. S&D EXPENSES' AND wa_final-fin_pnl_sub = 'D1. MARKETING & SALES PROMOTION'.
        wa_final-fin_tag_rx = 'GST Non Recoverable (part of Marketing & Sales Promotion)'.
      ELSEIF wa_final-fin_pnl_main = 'D. S&D EXPENSES' AND wa_final-fin_pnl_sub = 'D18. OTHER EXPENSES'.
        wa_final-fin_tag_rx =  'GST Non Recoverable (part of other expenses)'.
      ELSEIF wa_final-fin_pnl_main = 'D. S&D EXPENSES' AND wa_final-fin_pnl_sub = 'D9. DISTRIBUTION - OTHER COSTS'.
        wa_final-fin_tag_rx =  'LA commission'.
      ENDIF.


      "GL Description
      READ TABLE ta_skat_fagl INTO DATA(wa_skat_fagl) WITH KEY saknr = wa_fagl2-racct
                                               BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-gl_desc = wa_skat_fagl-txt50.
        CLEAR: wa_skat_fagl.
      ENDIF.

      wa_final-midas_date = sy-datum.
      wa_final-midas_time = sy-uzeit.
      wa_final-midas_id   = sy-uname.
      wa_final-midas_type = co_system. "SYSTEM

*      wa_final-remark = text-t53.
      APPEND wa_final TO ta_final.
      CLEAR:wa_fagl2,wa_final.
    ENDLOOP.
  ENDIF.


* **BEGIN OF CHANGE 01.11.2026 - package processing (see FORM process_packages)
* Issue   : whole month read at once -> memory dump in S/4 (job cancelled
*           after 89 sec). The rows are now read by key table in
*           packages inside FORM process_packages.
*  SELECT * FROM zmidas_fagll03
*    INTO TABLE ta_fagll03
*    WHERE spmon = p_spmon
*      AND bukrs IN s_bukrs
*      AND belnr IN s_belnr
*      AND hkont IN s_hkont.
*
*  IF NOT ta_fagll03 IS INITIAL .
*    SORT ta_fagll03 BY spmon bukrs belnr hkont.
*
*    DELETE ta_fagll03 WHERE remark = 'Updated by ZMIDAS_TAGG_UPLOAD'. "Added by Gajanan - 29.03.2024 - CR#8000003569
*
**  ELSE.
**    MESSAGE 'No Data Found' TYPE co_s DISPLAY LIKE co_e.
**    LEAVE LIST-PROCESSING.
*
*  ENDIF.
* **END OF CHANGE 01.11.2026

*  IF p_erdat IS NOT INITIAL.
*
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
*
*    "Fetch Document Numbers from Table BKPF
*    SELECT bukrs belnr gjahr cpudt FROM bkpf
*      INTO TABLE ta_bkpf
*      WHERE bukrs IN s_bukrs
*        AND gjahr = gv_fisc_yr
*        AND cpudt = p_erdat.
*
*    IF NOT ta_bkpf IS INITIAL.
*
*      SORT ta_bkpf BY bukrs belnr gjahr.
*      "Fetch Document Numbers from Table BSEG
*      SELECT bukrs belnr gjahr buzei hkont FROM bseg
*        INTO TABLE ta_bseg
*        FOR ALL ENTRIES IN ta_bkpf
*        WHERE bukrs = ta_bkpf-bukrs
*          AND belnr = ta_bkpf-belnr
*          AND gjahr = ta_bkpf-gjahr
*          AND hkont IN r_saknr.
*
*      IF ta_bseg IS NOT INITIAL.
*        "Make Company Code range
*        DATA(ta_bseg_bukrs) = ta_bseg.
*        SORT ta_bseg_bukrs BY bukrs.
*        DELETE ADJACENT DUPLICATES FROM ta_bseg_bukrs COMPARING bukrs.
*        IF NOT ta_bseg_bukrs IS INITIAL.
*          LOOP AT ta_bseg_bukrs INTO DATA(wa_bseg_bukrs).
*            r_bukrs-sign   = 'I'.
*            r_bukrs-option = 'EQ'.
*            r_bukrs-low   = wa_bseg_bukrs-bukrs.
*            APPEND r_bukrs.
*            CLEAR: wa_bseg_bukrs.
*          ENDLOOP.
*        ENDIF.
*
*        SORT ta_bseg BY belnr.
*        DELETE ADJACENT DUPLICATES FROM ta_bseg COMPARING belnr.
*
*        DESCRIBE TABLE ta_bseg LINES DATA(lv_lines).
*        lv_from = 1.
*        IF lv_lines LE 500.
*          lv_to = lv_lines.
*          lv_div = 1.
*
*        ELSE.
*          lv_to = 500.
*          lv_div = lv_lines DIV 500.
*          lv_mod = lv_lines MOD 500.
*          IF lv_mod GT 0.
*            lv_div = lv_div + 1.
*          ENDIF.
*        ENDIF.
*
*
*        DO lv_div TIMES.
*
*          "Document Number
*          lwa_field_rg-tablename = 'FAGLFLEXA_FS'.
*          lwa_france-fieldname = 'DOCNR'.
*
*          LOOP AT ta_bseg INTO DATA(wa_bseg) FROM lv_from TO lv_to.
*
*            lwa_sel_op-sign   = 'I'.
*            lwa_sel_op-option = 'EQ'.
*            lwa_sel_op-low   = wa_bseg-belnr.
*            APPEND lwa_sel_op TO lwa_france-selopt_t.
*            CLEAR: lwa_sel_op,wa_bseg.
*
*          ENDLOOP.
*
*          APPEND lwa_france TO lwa_field_rg-frange_t.
*          CLEAR: lwa_france.
*
*          "Fiscal Year
*          lwa_field_rg-tablename = 'FAGLFLEXA_FS'.
*          lwa_france-fieldname = 'RYEAR'.
*          lwa_sel_op-sign   = co_i.
*          lwa_sel_op-option = co_eq.
*          lwa_sel_op-low   = gv_fisc_yr.
*          APPEND lwa_sel_op TO lwa_france-selopt_t.
*          CLEAR: lwa_sel_op.
*          APPEND lwa_france TO lwa_field_rg-frange_t.
*          CLEAR: lwa_france.
*          APPEND lwa_field_rg TO lta_field_rg.
*          CLEAR: lwa_field_rg.
*
*          CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_EX'
*            EXPORTING
*              field_ranges = lta_field_rg
*            IMPORTING
*              expressions  = lta_dyn_exp.
*
*          CALL FUNCTION 'FREE_SELECTIONS_RANGE_2_WHERE'
*            EXPORTING
*              field_ranges  = lta_field_rg
*            IMPORTING
*              where_clauses = lta_clause.
*
*          CLEAR lwa_free.
*          lwa_free-texpr = lta_dyn_exp.
*          lwa_free-trange = lta_field_rg.
*          lwa_free-clauses = lta_clause.
*
*
*          "Call Tcode FAGLL03
*          CALL METHOD cl_salv_bs_runtime_info=>set                "DISPLAY ALV = FALSE
*            EXPORTING
*              display  = abap_false
*              metadata = abap_false
*              data     = abap_true.
*
*          SUBMIT fagl_account_items_gl
*                      WITH sd_saknr     IN r_saknr
*                      WITH sd_bukrs     IN r_bukrs
*                      WITH x_opsel      EQ abap_false
*                      WITH x_clsel      EQ abap_false
*                      WITH x_aisel      EQ abap_true
*                      WITH so_budat     IN s_budat
*                      WITH rldnr = '0L'
*                      WITH fs_dyns = lwa_free
*                    AND RETURN.
*
*          TRY.
*              FREE: o_ta_data. CLEAR: o_ta_data. UNASSIGN: <fs_data>.
*
*              CALL METHOD cl_salv_bs_runtime_info=>get_data_ref  "TO GET THE DATA IN RUNTIME
*                IMPORTING
*                  r_data = o_ta_data.
*
*              IF o_ta_data IS BOUND.
*                ASSIGN o_ta_data->* TO <fs_data>.
*              ENDIF.
*
*              IF <fs_data> IS ASSIGNED.
*                MOVE-CORRESPONDING <fs_data> TO ta_fagll03_temp.
*                APPEND LINES OF ta_fagll03_temp TO ta_fagll03.
*                UNASSIGN <fs_data>.
*              ENDIF.
*
*            CATCH: cx_salv_bs_sc_runtime_info, cx_root.
*          ENDTRY.
*
*          "Update tabix values for loop
*          lv_from = lv_from + 500.
*          lv_to = lv_to + 500.
*          IF lv_to GT lv_lines.
*            lv_to = lv_lines.
*          ENDIF.
*
*          CLEAR: lwa_free.
*          REFRESH: lta_field_rg[],lta_dyn_exp[],lta_clause[],ta_fagll03_temp[].
*        ENDDO.
*        CLEAR: lv_to,lv_from,lv_lines,lv_mod,lv_div.
*      ENDIF.
*    ENDIF.
*
*  ENDIF.

* Start of Change Inc 8000003399 | TR CRDK989166 | Prathamesh Parab | 09.01.2024
  SELECT *
    FROM tvv1t
    INTO TABLE ta_tvv1t_zone
   WHERE spras = sy-langu.

  IF sy-subrc = 0.
    SORT ta_tvv1t_zone BY bezei.
  ENDIF.

  SELECT setclass
         subclass
         setname
         lineid
         valfrom
    FROM setleaf
    INTO TABLE ta_set_people_cost_gl
   WHERE setname = 'PEOPLE_COST_GL'.

  IF sy-subrc = 0.
*   SORT ta_setleaf BY setname valfrom.
    SORT ta_set_people_cost_gl BY setname valfrom. " Change added by Inc 8000003399 | TR CRDK989314 | Prathamesh Parab | 23.01.2024
  ENDIF.

* **BEGIN OF CHANGE 01.11.2026 - lookups depending on TA_FAGLL03 moved to
* FORM get_pkg_lookups (executed per package).
*  IF ta_fagll03 IS NOT INITIAL.
*    SELECT * FROM zco_midas3_dept ... FOR ALL ENTRIES IN ta_fagll03 ...
*    SELECT * FROM zco_midas3_gl   ... FOR ALL ENTRIES IN ta_fagll03 ...
*  ENDIF.
* **END OF CHANGE 01.11.2026
* End of Change Inc 8000003399 | TR CRDK989166 | Prathamesh Parab | 09.01.2024

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_PKG_LOOKUPS        (new 01.11.2026)
*&---------------------------------------------------------------------*
*  Lookups that depend on the current package of ZMIDAS_FAGLL03 rows
*  (moved unchanged from FORM get_glset).
*----------------------------------------------------------------------*
FORM get_pkg_lookups.
  REFRESH: ta_midas_dept, ta_midas_gl.
  IF ta_fagll03 IS NOT INITIAL.
    SELECT *
      FROM zco_midas3_dept
      INTO TABLE ta_midas_dept
      FOR ALL ENTRIES IN ta_fagll03
     WHERE department = ta_fagll03-department.
    IF sy-subrc = 0.
      SORT ta_midas_dept BY department.
    ENDIF.

    SELECT *
      FROM zco_midas3_gl
      INTO TABLE ta_midas_gl
      FOR ALL ENTRIES IN ta_fagll03
     WHERE hkont = ta_fagll03-hkont.
    IF sy-subrc = 0.
      SORT ta_midas_gl BY hkont.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SAVE_PC_ROWS           (new 01.11.2026)
*&---------------------------------------------------------------------*
*  Rows built in GET_GLSET from the G/L totals (profit-centre rows) are
*  written before the package processing starts, so that TA_FINAL is
*  free for the packages.
*----------------------------------------------------------------------*
FORM save_pc_rows.
  IF ta_final[] IS NOT INITIAL.
    MODIFY zmidas_fagll03 FROM TABLE ta_final.
    IF sy-subrc = 0.
      COMMIT WORK.
      MESSAGE text-s02 TYPE co_s.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
    REFRESH ta_final.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESS_PACKAGES       (new 01.11.2026, reworked 02.11.2026)
*&---------------------------------------------------------------------*
*  Processes the ZMIDAS_FAGLL03 rows of the period in packages and runs
*  the unchanged GET_DATA / UPDATE_TAB per package.
*  02.11.2026: the first version used OPEN CURSOR WITH HOLD. The COMMIT
*  WORK that UPDATE_TAB executes per package closed the cursor on the
*  CRS HANA system (dump DBSQL_INVALID_CURSOR, job 10552200, 2nd FETCH).
*  Now the keys of all rows are read once into a small key table and
*  each package is selected by its keys, so no database cursor stays
*  open across the commit.
*----------------------------------------------------------------------*
FORM process_packages.
  CONSTANTS lc_pkg TYPE i VALUE 20000.
  TYPES: BEGIN OF lty_key,
           spmon TYPE zmidas_fagll03-spmon,
           bukrs TYPE zmidas_fagll03-bukrs,
           belnr TYPE zmidas_fagll03-belnr,
           hkont TYPE zmidas_fagll03-hkont,
           buzei TYPE zmidas_fagll03-buzei,
         END OF lty_key.
  DATA: lt_keys  TYPE STANDARD TABLE OF lty_key,
        lt_pkg   TYPE STANDARD TABLE OF lty_key,
        lv_lines TYPE i,
        lv_from  TYPE i,
        lv_to    TYPE i,
        lv_total TYPE i.

  SELECT spmon, bukrs, belnr, hkont, buzei
    FROM zmidas_fagll03
    WHERE spmon = @p_spmon
      AND bukrs IN @s_bukrs
      AND belnr IN @s_belnr
      AND hkont IN @s_hkont
    ORDER BY PRIMARY KEY
    INTO TABLE @lt_keys.
  lv_lines = lines( lt_keys ).

  WHILE lv_from < lv_lines.
    lv_to = lv_from + lc_pkg.
    IF lv_to > lv_lines.
      lv_to = lv_lines.
    ENDIF.
    CLEAR lt_pkg.
    APPEND LINES OF lt_keys FROM lv_from + 1 TO lv_to TO lt_pkg.
    lv_from = lv_to.

    PERFORM clear_pkg.
    SELECT * FROM zmidas_fagll03
      FOR ALL ENTRIES IN @lt_pkg
      WHERE spmon = @lt_pkg-spmon
        AND bukrs = @lt_pkg-bukrs
        AND belnr = @lt_pkg-belnr
        AND hkont = @lt_pkg-hkont
        AND buzei = @lt_pkg-buzei
      INTO TABLE @ta_fagll03.
    IF ta_fagll03 IS INITIAL.
      CONTINUE.
    ENDIF.
    SORT ta_fagll03 BY spmon bukrs belnr hkont.
    DELETE ta_fagll03 WHERE remark = 'Updated by ZMIDAS_TAGG_UPLOAD'. "Added by Gajanan - 29.03.2024 - CR#8000003569
    IF ta_fagll03 IS INITIAL.
      CONTINUE.
    ENDIF.
    lv_total = lv_total + lines( ta_fagll03 ).
    PERFORM get_pkg_lookups.
    PERFORM get_data.
    PERFORM update_tab.
  ENDWHILE.
  FREE: lt_keys, lt_pkg.
  PERFORM clear_pkg.

  IF lv_total = 0.
    MESSAGE text-001 TYPE co_s DISPLAY LIKE co_e.
  ELSE.
    MESSAGE |{ lv_total } rows of ZMIDAS_FAGLL03 processed for period { p_spmon }| TYPE co_s.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CLEAR_PKG              (new 01.11.2026)
*&---------------------------------------------------------------------*
*  Package-level tables that GET_DATA / UPDATE_TAB fill by APPEND
*  (everything filled by SELECT INTO TABLE is overwritten anyway).
*----------------------------------------------------------------------*
FORM clear_pkg.
  REFRESH: ta_fagll03, ta_final, ta_zone_gl1, ta_u_zuonr, ta_excrate,
           ta_kostl, r_kostl, ta_vbrk, ta_kna1, ta_midas_dept, ta_midas_gl.
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

* **BEGIN OF CHANGE 01.11.2026 - rebuilt per package
  REFRESH: r_kostl[], ta_kostl[].
* **END OF CHANGE 01.11.2026

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
      SELECT aufnr auart kostv
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
          r_kostl-sign = co_i.
          r_kostl-option = co_eq.
          r_kostl-low = wa_coas_temp_1-kostv.
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

*    IF NOT r_kostl[] IS INITIAL.
    IF NOT ta_kostl IS INITIAL.

      SELECT kokrs kostl datbi name1 name2 name3
        FROM csks INTO TABLE ta_csks
        FOR ALL ENTRIES IN ta_kostl
        WHERE kostl = ta_kostl-kostl.

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
          AND blart = ta_temp_2-blart. "#EC CI_NOORDER
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
      WHERE spmon   = ta_fagll03-spmon
        AND bukrs   = ta_fagll03-bukrs
        AND belnr   = ta_fagll03-belnr.

    IF NOT ta_reimb IS INITIAL.
      SORT ta_reimb BY spmon bukrs belnr. "hkont buzei.
    ENDIF.

    "Fetch dara from table ZCO_MIDAS_IC_ELE
    SELECT * FROM zco_midas_ic_ele
      INTO TABLE ta_elem
      FOR ALL ENTRIES IN ta_fagll03
      WHERE spmon   = ta_fagll03-spmon
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
    DELETE ADJACENT DUPLICATES FROM ta_temp_5 COMPARING bukrs .

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
*    SORT ta_temp_6 BY bukrs hkont prctr.
    SORT ta_temp_6 BY bukrs hkont.
*    DELETE ADJACENT DUPLICATES FROM ta_temp_6 COMPARING bukrs hkont prctr.
    DELETE ADJACENT DUPLICATES FROM ta_temp_6 COMPARING bukrs hkont .

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
            i_date         = wa_forg_gl-budat
            i_periv        = 'V3'
          IMPORTING
            e_buper        = lv_forg_period
            e_gjahr        = lv_forg_fy
          EXCEPTIONS
            input_false    = 1
            t009_notfound  = 2
            t009b_notfound = 3
            OTHERS         = 4.
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
            i_date         = wa_write_gl-budat
            i_periv        = 'V3'
          IMPORTING
            e_buper        = lv_write_period
            e_gjahr        = lv_write_fy
          EXCEPTIONS
            input_false    = 1
            t009_notfound  = 2
            t009b_notfound = 3
            OTHERS         = 4.
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

* Start of Change Inc 8000003399 | TR CRDK989330 | Prathamesh Parab | 24.01.2024
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
    ENDIF.
* End of Change Inc 8000003399 | TR CRDK989330 | Prathamesh Parab | 24.01.2024

***SOA by Gajanan - 24.12.2025 - CR#8000004709***
    SELECT * FROM zmidas_func_map
      INTO TABLE ta_func_map.
    IF NOT ta_func_map IS INITIAL.
      SORT ta_func_map BY kostl.
    ENDIF.
***EOA by Gajanan - 24.12.2025 - CR#8000004709***
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
  DATA: lv_count   TYPE char10,
        lv_index   TYPE char10,
        lv_fcurr1  TYPE tcurr-fcurr,
        lv_ffact1  TYPE tcurr-ffact,
        lv_excrate TYPE tcurr-ukurs.
  "SOC added by Pooja A on 01.03.2023 CR#8000007907
  DATA:  lv_string1 TYPE string,
         lv_string2 TYPE string.
  "EOC added by Pooja A on 01.03.2023 CR#8000007907
  CONSTANTS:co_splchar    TYPE char10 VALUE 'SPL_CHAR',
            co_idno       TYPE char3 VALUE '001',
            "SOC by Pooja Adupala on 17.08.2022
            co_updatefrom TYPE zmidas_fagll03-update_from VALUE 'Original',
            co_updatetype TYPE zmidas_fagll03-midas_type VALUE 'System'.

  SELECT * FROM zmidas_bu_pnl
      INTO TABLE @DATA(ta_bu_pnl).
  IF ta_bu_pnl IS NOT INITIAL.
    SORT ta_bu_pnl BY bu_pnl.
  ENDIF.

  "EOC by Pooja Adupala on 17.08.2022
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
    "SOC by Pooja on 24.11.2022 CR# 8000007907


    SELECT bukrs,waers
         FROM t001 INTO TABLE @DATA(ta_t001)
         FOR ALL ENTRIES IN @ta_fagll03
         WHERE bukrs = @ta_fagll03-bukrs.

    IF ta_t001 IS NOT INITIAL.
      SELECT kurst
      fcurr
      tcurr
      gdatu
      ukurs
      ffact FROM tcurr
   INTO TABLE ta_tcurr
   FOR ALL ENTRIES IN ta_t001
   WHERE kurst = 'M'
     AND fcurr = ta_t001-waers
     AND tcurr = 'INR'
     AND gdatu GE   lv_enddate_1  AND gdatu LE lv_startdate_1.

      IF ta_tcurr IS NOT INITIAL.
        SORT ta_tcurr BY fcurr.
        DATA(lv_lines) = lines( ta_tcurr ).

        CLEAR:lv_count,lv_index.
        LOOP AT ta_tcurr INTO DATA(wa_tcurr).
          lv_index = sy-tabix.
          lv_count = lv_count + 1.
          MOVE-CORRESPONDING wa_tcurr TO wa_tcurr1.
          COLLECT wa_tcurr1 INTO ta_tcurr1.

          IF lv_index NE 1 AND wa_tcurr1-fcurr NE lv_fcurr1.
            CLEAR   lv_excrate.
            READ TABLE ta_tcurr1 INTO DATA(wa_tcurr2) WITH KEY fcurr = lv_fcurr1.
            IF sy-subrc = 0.
              IF lv_count GT 1.
                DATA(lv_count2) = lv_count - 1.
              ELSE.
                lv_count2 = lv_count.
              ENDIF.
              IF lv_count2 IS NOT INITIAL.
                IF lv_ffact1 IS NOT INITIAL.
                  lv_excrate =  wa_tcurr2-ukurs / lv_count2 / lv_ffact1.
                ELSE.
                  lv_excrate =  wa_tcurr2-ukurs / lv_count2 .
                ENDIF.
              ELSE.
                IF lv_ffact1 IS NOT INITIAL .
                  lv_excrate = wa_tcurr2-ukurs / lv_ffact1.
                ELSE.
                  lv_excrate = wa_tcurr2-ukurs.
                ENDIF.
              ENDIF.
              wa_excrate-fcurr = wa_tcurr2-fcurr.
              wa_excrate-exch_rate = lv_excrate.
              APPEND wa_excrate TO ta_excrate.
              CLEAR:lv_count,wa_excrate,wa_tcurr2.
            ENDIF.
            lv_count = 1.
          ENDIF.

          lv_ffact1 = wa_tcurr-ffact .

          IF lv_index = lv_lines.
            READ TABLE ta_tcurr1 INTO wa_tcurr2 WITH KEY fcurr = wa_tcurr1-fcurr.
            IF sy-subrc = 0.
              IF lv_count IS NOT INITIAL.
                IF lv_ffact1 IS NOT INITIAL.
                  lv_excrate =  wa_tcurr2-ukurs / lv_count / lv_ffact1.
                ELSE.
                  lv_excrate =  wa_tcurr2-ukurs / lv_count.
                ENDIF.
              ELSE.
                IF lv_ffact1 IS NOT INITIAL.
                  lv_excrate = wa_tcurr2-ukurs / lv_ffact1.
                ELSE.
                  lv_excrate = wa_tcurr2-ukurs.
                ENDIF.
              ENDIF.
              wa_excrate-fcurr = wa_tcurr2-fcurr.
              wa_excrate-exch_rate = lv_excrate.
              APPEND wa_excrate TO ta_excrate.
              CLEAR:wa_excrate,wa_tcurr2.
            ENDIF.
          ENDIF.

          lv_fcurr1 = wa_tcurr-fcurr .
          CLEAR:wa_tcurr1,wa_tcurr.
        ENDLOOP.

        SORT ta_excrate BY fcurr.
      ENDIF.
    ENDIF.
    "EOC by Pooja on 24.11.2022 CR# 8000007907

    "SOA by Gajanan - 29.03.2024 - CR#8000003569
    "Fetch zone data from table ZMIDAS_ZONE_CC
    SELECT * FROM zmidas_zone_cc INTO TABLE @DATA(ta_zone_cc).
    IF NOT ta_zone_cc IS INITIAL.
      SORT ta_zone_cc BY fin_kostl.
    ENDIF.
    "Fetch zone data from table ZMIDAS_ZONE_CC
    SELECT * FROM zmidas_zone_ccgl INTO TABLE @DATA(ta_zone_ccgl).
    IF NOT ta_zone_ccgl IS INITIAL.
      SORT ta_zone_ccgl BY fin_kostl hkont.
    ENDIF.
    "Fetch zone data from table ZMIDAS_ZONE_CC
    SELECT * FROM zmidas_zone_glsg INTO TABLE @DATA(ta_zone_glsg).
    IF NOT ta_zone_glsg IS INITIAL.
      SORT ta_zone_glsg BY hkont segment.
    ENDIF.
    "EOA by Gajanan - 29.03.2024 - CR#8000003569
  ENDIF.

  "SOA by Gajanan - 06.10.2025 - CR#8000004621
  IF NOT ta_vbrk IS INITIAL.

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
  ENDIF.

  IF NOT ta_fagll03 IS INITIAL.
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
  ENDIF.
  "EOA by Gajanan - 06.10.2025 - CR#8000004621


  LOOP AT ta_fagll03 INTO wa_fagll03.

    MOVE-CORRESPONDING wa_fagll03 TO wa_final.

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
    "Client
    wa_final-mandt = sy-mandt.

    "Period
    wa_final-spmon = wa_fagll03-spmon.

    "Entry Date
    wa_final-cpudt = wa_fagll03-cpudt.

    "Start of changes by Pooja Adupala on 17.08.2022
    "Commented by Pooja Adupala on 19.08.2022
*    "Doc Type
*    IF wa_fagll03-sgtxt = 'REV PROV EAS' OR wa_fagll03-sgtxt = 'EAS PROVISION'.
*      wa_final-blart = wa_fagll03-blart = co_ep.
*    ENDIF.
    "Commented by Pooja Adupala on 19.08.2022

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

    "SOC by Pooja on 29.09.2022 CR#8000007907
    IF wa_final-bukrs NE co_1000.
      wa_final-tagging = ''.
    ENDIF.
    "EOC by Pooja on 29.09.2022 CR#8000007907

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

      READ TABLE ta_gl_exclud INTO DATA(wa_gl_exclud) WITH KEY saknr = wa_final-hkont.
      IF sy-subrc = 0.
        wa_final-fin_pnl_main = wa_gl_exclud-final_plm.
        wa_final-fin_pnl_sub  = wa_gl_exclud-final_pls.
        DATA(lv_exclud) = 1.
        gv_remark = text-t50.
      ENDIF.

      IF  wa_fagll03-bukrs = co_1000 AND lv_exclud IS INITIAL. "1000
        READ TABLE ta_eu_tag INTO DATA(wa_eu_tag) WITH KEY bukrs = wa_final-bukrs
                                                           saknr = wa_final-hkont
                                                           kostl = wa_fagll03-kostl
                                                           gkont = wa_final-gkont
                                                           BINARY SEARCH. "Added by Gajanan - 20.06.2024 - CR#8000003569
        IF sy-subrc = 0.
          wa_final-fin_pnl_main = wa_eu_tag-final_plm.
          wa_final-fin_pnl_sub  = wa_eu_tag-final_pls.
          DATA(lv_eu_tag) = 1.
          gv_remark = text-t51.
        ENDIF.

        IF wa_fagll03-hkont IN r_forg_exg[].
          IF lv_eu_tag IS INITIAL AND  lv_exclud IS INITIAL.

            IF gv_forg_val LT 0.
              wa_final-fin_pnl_main = text-t37. "Other Income
              wa_final-fin_pnl_sub  = text-t37. "Other Income

            ELSEIF gv_forg_val GE 0.
              wa_final-fin_pnl_main = text-t38. "Other Expenses
              wa_final-fin_pnl_sub  = text-t38. "Other Expenses
            ENDIF.
          ENDIF.

        ELSEIF wa_fagll03-hkont IN r_write_back[].
          IF lv_eu_tag IS INITIAL AND  lv_exclud IS INITIAL.
            IF gv_write_val LT 0.
              wa_final-fin_pnl_main = text-t37. "Other Income
              wa_final-fin_pnl_sub  = text-t37. "Other Income

            ELSEIF gv_write_val GE 0.
              wa_final-fin_pnl_main = text-t39. "Selling and Distribution Expenses
              wa_final-fin_pnl_sub  = text-t39. "Selling and Distribution Expenses
            ENDIF.
          ENDIF.

        ELSEIF wa_fagll03-hkont IN r_rnd_gl[] AND wa_final-fin_kostl IN r_rnd_cc[].
          IF lv_eu_tag IS INITIAL AND  lv_exclud IS INITIAL.

            wa_final-fin_pnl_main = text-t23. "R&D
            wa_final-fin_pnl_sub  = text-t23. "R&D
            gv_remark = text-t29. "Based on R&D
          ENDIF.

        ELSE.
          IF lv_eu_tag IS INITIAL AND  lv_exclud IS INITIAL.
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

              "Start of Commented by Pooja A on 29.09.2022 CR#8000007907
*            ELSE.
*              READ TABLE ta_prevmo INTO wa_prevmo WITH KEY bukrs = wa_fagll03-bukrs
*                                                           saknr = wa_fagll03-hkont
*                                                           prctr = wa_fagll03-prctr.
*              IF sy-subrc = 0.
*                wa_final-fin_pnl_main = wa_prevmo-final_plm.
*                wa_final-fin_pnl_sub  = wa_prevmo-final_pls.
*                gv_remark = text-t31. "Company Code + GL + PC
*
*                CLEAR: wa_prevmo.
              "End of Commented by Pooja A on 29.09.2022 CR#8000007907

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
*            ENDIF."LOC commented by Pooja A: 29.09.2022
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSEIF wa_fagll03-bukrs = co_3800.

        "SOC by Pooja Adupala on 16.08.2022
*        READ TABLE ta_prof_cc INTO wa_prof_cc WITH KEY bukrs = wa_fagll03-bukrs
*                                                       kostl = wa_final-fin_kostl
*                                                       BINARY SEARCH.
*        IF sy-subrc = 0.
*          wa_final-fin_pnl_main = wa_prof_cc-final_plm.
*          wa_final-fin_pnl_sub  = wa_prof_cc-final_pls.
*          gv_remark = text-t33. "Company Code + CC
*          CLEAR:wa_prof_cc.
        IF  lv_exclud IS INITIAL.
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
        ENDIF.

      ELSEIF wa_fagll03-bukrs IN r_europe_cc[].
*        IF   lv_eu_tag IS INITIAL AND  lv_exclud IS INITIAL.
        IF   lv_exclud IS INITIAL.
          "SOA by Gajanan - 20.06.2024 - CR#8000003569
          CLEAR:wa_eu_tag.
          READ TABLE ta_eu_tag INTO wa_eu_tag WITH KEY bukrs = wa_final-bukrs
                                                       saknr = wa_final-hkont
                                                       kostl = wa_fagll03-kostl
                                                       gkont = wa_final-gkont
                                                       BINARY SEARCH.

          IF sy-subrc = 0.
            wa_final-fin_pnl_main = wa_eu_tag-final_plm.
            wa_final-fin_pnl_sub  = wa_eu_tag-final_pls.
            gv_remark = text-t51. "GL+Comp code+CC+Offset
            CLEAR: wa_eu_tag.

          ELSE.
            "EOA by Gajanan - 20.06.2024 - CR#8000003569

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
              "Changed below table from ta_prof_cc to ta_prevmo by Gajanan - 20.06.2024 - CR#8000003569
              CLEAR:wa_prevmo.
              READ TABLE ta_prevmo INTO wa_prevmo WITH KEY bukrs = wa_fagll03-bukrs
                                                           saknr = wa_fagll03-hkont
                                                           kostl = wa_final-fin_kostl
                                                           prctr = wa_fagll03-prctr
                                                           BINARY SEARCH.
*            READ TABLE ta_prof_cc INTO wa_prof_cc WITH KEY bukrs = wa_fagll03-bukrs
*                                                           kostl = wa_final-fin_kostl
*                                                           BINARY SEARCH.
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
          ENDIF.
        ENDIF.

      ELSEIF wa_fagll03-bukrs = co_5200.
*        IF lv_eu_tag IS INITIAL AND  lv_exclud IS INITIAL.
        IF lv_exclud IS INITIAL.

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
        ENDIF.

      ELSEIF wa_fagll03-bukrs = co_5400. "5400
        IF  lv_exclud IS INITIAL.

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
        ENDIF.

      ELSEIF wa_fagll03-bukrs = co_5000."5000
        IF lv_exclud IS INITIAL.
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
        ENDIF.

      ELSEIF wa_fagll03-bukrs IN r_south_africa_cc . "South Africa
        IF  lv_exclud IS INITIAL.
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
        ENDIF.

      ELSEIF wa_fagll03-bukrs = co_4500."4500
        IF lv_exclud IS INITIAL.
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
        ENDIF.

      ELSEIF wa_fagll03-bukrs = co_4900. "4900
        IF lv_exclud IS INITIAL.
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

      ELSEIF wa_fagll03-bukrs = co_6100. "6100 "No Logic Given
        IF lv_exclud IS INITIAL.
        ENDIF.

      ELSEIF wa_fagll03-bukrs IN r_emerging_market_cc . "Emerging market " No Logic Given
        IF lv_exclud IS INITIAL.
        ENDIF.

      ELSEIF wa_fagll03-bukrs+0(1) = '3'.
        IF lv_exclud IS INITIAL.
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
        ENDIF.

      ELSEIF wa_fagll03-bukrs IN r_mor_tag_midas3[] AND lv_exclud IS INITIAL. "MOR Tagging
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

          ENDIF.
        ENDIF.


      ELSEIF  lv_eu_tag IS INITIAL AND  lv_exclud IS INITIAL.
        wa_final-fin_pnl_main = text-t22. "NO TAGGING FOUND
        wa_final-fin_pnl_sub  = text-t22. "NO TAGGING FOUND
        gv_remark = text-t22. "NO TAGGING FOUND
      ENDIF.
    ENDIF.

    "SOC by Pooja on 29.09.2022
    IF r_zone_gl IS NOT INITIAL AND wa_final-hkont IN r_zone_gl.
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
*      READ TABLE ta_vbrp INTO DATA(wa_vbrp) WITH KEY vbeln = lv_vbeln.
*      IF sy-subrc = 0.
*        READ TABLE ta_tvv1t INTO DATA(wa_tvv1t) WITH KEY kvgr1 = wa_vbrp-kvgr1.
*        IF sy-subrc = 0.
*          "SOC by Pooja Adupala on 27.02.2023
**          wa_final-fin_pnl_main = wa_tvv1t-bezei. "NO TAGGING FOUND
*          wa_final-zone_2 = wa_tvv1t-bezei. "NO TAGGING FOUND
*          "EOC by Pooja Adupala on 27.02.2023
*        ENDIF.
*      ENDIF.
*      CLEAR:wa_vbrp,wa_tvv1t.
    ENDIF.
    "EOC by Pooja on 29.09.2022

    "SOC by Pooja Adupala on 27.02.2023
    IF wa_final-tagging NE 'IB'.
      CLEAR:wa_final-p_l_head.
    ENDIF.

    IF wa_final-tagging = 'IB'.
      TRANSLATE wa_final-p_l_head TO UPPER CASE.
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

      IF r_seg_tag_costc_api IS NOT INITIAL AND wa_final-fin_kostl IN r_seg_tag_costc_api.
        wa_final-seg_tag = text-t44. "API

      ELSEIF r_seg_tag_pc_special IS NOT INITIAL AND wa_final-prctr IN r_seg_tag_pc_special.
        wa_final-seg_tag = text-t45. "Specialty

      ELSEIF line_exists( ta_seg_rel[ bukrs = wa_final-bukrs hkont = wa_final-hkont fin_kostl = wa_final-fin_kostl ] ).

        wa_seg_rel = ta_seg_rel[ bukrs = wa_final-bukrs hkont = wa_final-hkont fin_kostl = wa_final-fin_kostl ].

        wa_final-seg_tag = wa_seg_rel-seg_tag.

        "SOC by Pooja Adupala on 19.08.2022
      ELSEIF r_seg_tag_costc_ipd IS NOT INITIAL AND wa_final-fin_kostl IN r_seg_tag_costc_ipd.
*        wa_final-seg_tag = text-t46. "IPD
        wa_final-seg_tag = text-t46. "IPD-DTM
      ELSEIF r_seg_tag_costc_ipd2 IS NOT INITIAL AND wa_final-fin_kostl IN r_seg_tag_costc_ipd2.
*        wa_final-seg_tag = text-t47. "IPD2
        wa_final-seg_tag = text-t47. "Quality - Chargeback

      ELSEIF r_seg_tag_costc_ipd3 IS NOT INITIAL AND  wa_final-fin_kostl IN r_seg_tag_costc_ipd3.
        wa_final-seg_tag = text-t49. "Biosimilar
        "EOC by Pooja Adupala on 19.08.2022
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
*    wa_final-update_from = co_updatefrom.
    wa_final-midas_type = co_updatetype.
    wa_final-midas_id = sy-uname.
    "EOC by Pooja Adupala on 17.08.2022
    CLEAR:lv_exrate, wa_final-amnt_usd.
    READ TABLE ta_t001 INTO DATA(wa_t001) WITH KEY bukrs = wa_fagll03-bukrs.
    IF sy-subrc = 0.
      READ TABLE ta_excrate INTO wa_excrate WITH KEY fcurr = wa_t001-waers.
      IF sy-subrc = 0 AND wa_excrate-exch_rate IS NOT INITIAL.
        wa_final-amnt_inr = wa_final-dmshb  * wa_excrate-exch_rate.
        lv_exrate =  wa_excrate-exch_rate.
        IF  wa_final-amnt_inr  IS NOT INITIAL AND lv_exrate1 IS NOT INITIAL.
*          wa_final-amnt_usd = lv_exrate1 / lv_exrate.
          wa_final-amnt_usd = wa_final-amnt_inr  / lv_exrate1.
        ENDIF.
      ENDIF.

      IF wa_t001-waers = 'INR'.
        wa_final-amnt_inr  = wa_fagll03-dmshb.
        IF lv_exrate1 IS NOT INITIAL.
          wa_final-amnt_usd  = wa_fagll03-dmshb / lv_exrate1.
        ENDIF.
      ELSEIF  wa_t001-waers  = 'USD'.
        wa_final-amnt_usd  = wa_fagll03-dmshb.
        wa_final-amnt_inr  = wa_fagll03-dmshb * lv_exrate1.
      ENDIF.
    ENDIF.

    "SOC by pooja on 07.07.2023 CR#8000009091
    IF wa_final-pswsl = 'USD'.
      wa_final-amnt_usd = wa_final-wrshb.
    ENDIF.
    "EOC by pooja on 07.07.2023 CR#8000009091

    "SOA by Gajanan - 29.03.2024 - CR#8000003569
    READ TABLE ta_zone_cc INTO DATA(wa_zone_cc) WITH KEY fin_kostl = wa_final-fin_kostl
                                                         BINARY SEARCH.
    IF sy-subrc = 0.
      wa_final-zone_2 = wa_zone_cc-to_bu.

    ELSE.

      READ TABLE ta_zone_ccgl INTO DATA(wa_zone_ccgl) WITH KEY fin_kostl = wa_final-fin_kostl
                                                               hkont     = wa_final-hkont
                                                               BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-zone_2 = wa_zone_ccgl-to_bu.

      ELSE.

        READ TABLE ta_zone_glsg INTO DATA(wa_zone_glsg) WITH KEY hkont     = wa_final-hkont
                                                                 segment   = wa_final-segment
                                                                 BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final-zone_2 = wa_zone_glsg-to_bu.

        ELSE.
          "EOA by Gajanan - 29.03.2024 - CR#8000003569

          "SOC by pooja on 25.11.2022  CR#8000007907
          "Remark related
          IF wa_final-remark = 'Based on Invoice No'.
            lv_vbeln = wa_final-xblnr.
            READ TABLE ta_vbrp INTO DATA(wa_vbrp) WITH KEY vbeln = lv_vbeln.
            IF sy-subrc = 0.
              "Start of comment by Gajanan - 29.03.2024 - CR#8000003569
*        READ TABLE ta_tvv1t INTO DATA(wa_tvv1t) WITH KEY kvgr1 = wa_vbrp-kvgr1.
*        IF sy-subrc = 0.
*          wa_final-zone_2 = wa_tvv1t-bezei. "NO TAGGING FOUND
*        ENDIF.
*        CLEAR:wa_vbrp,wa_tvv1t.
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
*      wa_final-zone_2 = wa_final-fin_pnl_main.
          ELSEIF wa_final-remark = 'Based on IB tagging'.
            wa_final-zone_2 = co_rx.
            "GL related
          ELSEIF r_z1000_rx_gl IS NOT INITIAL AND wa_final-hkont IN r_z1000_rx_gl AND wa_final-bukrs EQ co_1000.
            wa_final-zone_2 = co_rx.
          ELSEIF r_z1000_gx_gl IS NOT INITIAL AND wa_final-hkont IN r_z1000_gx_gl AND wa_final-bukrs EQ co_1000.
            wa_final-zone_2 = co_gx.
          ELSEIF r_z1000_rd IS NOT INITIAL AND wa_final-hkont IN r_z1000_rd AND wa_final-bukrs EQ co_1000.
            wa_final-zone_2 = co_rnd.
          ELSEIF r_z1000_corporate IS NOT INITIAL AND wa_final-hkont IN r_z1000_corporate AND wa_final-bukrs EQ co_1000.
            wa_final-zone_2 = co_corp.
            "Department related
          ELSEIF wa_final-department = 'EM'.
            wa_final-zone_2 = co_em.
          ELSEIF wa_final-department = 'SSA'.
            wa_final-zone_2 = co_sub_sah.
          ELSEIF wa_final-department = 'Europe'.
            wa_final-zone_2 = co_europe.
          ELSEIF wa_final-department = 'API'.
            wa_final-zone_2 = co_api.
          ELSEIF wa_final-department = 'CGA'.
            wa_final-zone_2 = co_cga.
          ELSEIF wa_final-department = 'SA'.
            wa_final-zone_2 = co_sa.
          ELSEIF wa_final-department = 'USA'.
            wa_final-zone_2 = co_usa.
          ELSEIF wa_final-department = 'India Gx'.
            wa_final-zone_2 = co_gx.
          ELSEIF wa_final-department = 'India Rx- On'.
            wa_final-zone_2 = co_rx.
          ELSEIF wa_final-department = 'India Rx'.
            wa_final-zone_2 = co_rx.
          ELSEIF wa_final-department = 'North Africa'.
            wa_final-zone_2 = co_naf.
* Start of Change Inc 8000003399 | TR CRDK989366 | Prathamesh Parab | 30.01.2024
*   ELSEIF wa_final-department = 'SCM-Distribu'.
*     wa_final-zone_2 = co_rx.
* End of Change Inc 8000003399 | TR CRDK989366 | Prathamesh Parab | 30.01.2024
          ELSEIF r_sa_cc IS NOT INITIAL AND wa_final-bukrs IN r_sa_cc.
            wa_final-zone_2 = co_sa.
          ELSEIF r_eu_cc IS NOT INITIAL AND wa_final-bukrs IN r_eu_cc.
            wa_final-zone_2 = co_europe.
          ELSEIF r_em_cc IS NOT INITIAL AND wa_final-bukrs IN r_em_cc.
            wa_final-zone_2 = co_em.
          ELSEIF r_na_fr_cc IS NOT INITIAL AND wa_final-bukrs IN r_na_fr_cc.
            wa_final-zone_2 = co_usa.
          ELSEIF r_corp_cc IS NOT INITIAL AND wa_final-bukrs IN r_corp_cc.
            wa_final-zone_2 = co_corp.
          ELSEIF r_sub_sah_cc IS NOT INITIAL AND wa_final-bukrs IN r_sub_sah_cc.
            wa_final-zone_2 = co_sub_sah.
          ELSEIF wa_final-bukrs = co_3800.
            wa_final-zone_2 = co_chl.
          ELSEIF wa_final-bukrs = co_3900.
            wa_final-zone_2 = 'GX'.
            "SOA by Gajanan - 27.04.2026 - CR#8000004949
          ELSEIF r_north_africa_cc[] IS NOT INITIAL AND wa_final-bukrs IN r_north_africa_cc[].
            wa_final-zone_2 = co_naf.
            "EOA by Gajanan - 27.04.2026 - CR#8000004949
          ELSE.
            wa_final-zone_2  = co_bu_tagg.
          ENDIF.
          "EOC by pooja on 25.11.2022  CR#8000007907

        ENDIF. "Added by Gajanan - 29.03.2024 - CR#8000003569
      ENDIF. "Added by Gajanan - 29.03.2024 - CR#8000003569
    ENDIF. "Added by Gajanan - 29.03.2024 - CR#8000003569

    "Detail P&L
    TRANSLATE wa_final-fin_pnl_main TO UPPER CASE.
    READ TABLE ta_det_pnl INTO wa_det_pnl WITH KEY fin_pnl_main = wa_final-fin_pnl_main
                                                   BINARY SEARCH.
    IF sy-subrc = 0.
      wa_final-det_pnl = wa_det_pnl-det_pnl.
      CLEAR: wa_det_pnl.
    ELSE.
      CLEAR: wa_final-det_pnl.
    ENDIF.

    "Final Tagging
    TRANSLATE wa_final-fin_pnl_sub TO UPPER CASE.
    READ TABLE ta_fin_tag INTO wa_fin_tag WITH KEY fin_pnl_sub = wa_final-fin_pnl_sub
                                                   BINARY SEARCH.
    IF sy-subrc = 0.
      wa_final-fin_tag = wa_fin_tag-fin_tag.
      CLEAR: wa_fin_tag.
* Start of Change Inc 8000003399 | TR CRDK989330 | Prathamesh Parab | 24.01.2024
    ELSE.
      wa_final-fin_tag = co_no_tagging_found.
* End of Change Inc 8000003399 | TR CRDK989330 | Prathamesh Parab | 24.01.2024
    ENDIF.

    TRANSLATE wa_final-fin_tag_rx TO UPPER CASE.
    TRANSLATE wa_final-p_l_head TO UPPER CASE.
    TRANSLATE wa_final-fin_pnl_main TO UPPER CASE.
    TRANSLATE wa_final-fin_pnl_sub TO UPPER CASE.
    TRANSLATE wa_final-det_pnl TO UPPER CASE.
    TRANSLATE wa_final-fin_tag TO UPPER CASE.

* Start of Change Inc 8000003399 | TR CRDK989166 | Prathamesh Parab | 09.01.2024

    lv_zone = wa_final-zone_2.
*   TRANSLATE lv_zone TO UPPER CASE.
    READ TABLE ta_tvv1t_zone INTO wa_tvv1t_zone WITH KEY bezei = wa_final-zone_2
                                                         BINARY SEARCH.
    IF sy-subrc = 0.
*     DATA(lv_fin_tag) = wa_fagll03-fin_tag.
      DATA(lv_fin_tag) = wa_final-fin_tag. " Change added by Inc 8000003399 | TR CRDK989330 | Prathamesh Parab | 24.01.2024
      TRANSLATE lv_fin_tag TO UPPER CASE.

      IF lv_fin_tag IS INITIAL OR lv_fin_tag = co_no_tagging_found.
        DATA(lv_valfrom) = wa_set_people_cost_gl-valfrom. " Change added by Inc 8000003399 | TR CRDK989314 | Prathamesh Parab | 23.01.2024
        lv_valfrom = wa_final-hkont.  " Change added by Inc 8000003399 | TR CRDK989314 | Prathamesh Parab | 23.01.2024
        READ TABLE ta_set_people_cost_gl INTO wa_set_people_cost_gl WITH KEY setname = 'PEOPLE_COST_GL'
*                                                                            valfrom = wa_final-hkont
                                                                             valfrom = lv_valfrom " Change added by Inc 8000003399 | TR CRDK989314 | Prathamesh Parab | 23.01.2024
                                                                             BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final-fin_pnl_main = 'C. PEOPLE COST'.
          wa_final-det_pnl = 'C. PEOPLE COST'.
* Start of Change Inc 8000003399 | TR CRDK989314 | Prathamesh Parab | 23.01.2024
*         wa_final-fin_pnl_sub = 'C4. PEOPLE COST BU TAGGING MISSING'.
          wa_final-fin_pnl_sub = 'C5. PEOPLE COST BU TAGGING MISSING'.
*         wa_final-fin_tag = 'C4. PEOPLE COST BU TAGGING MISSING'.
          wa_final-fin_tag = 'C5. PEOPLE COST BU TAGGING MISSING'.
* End of Change Inc 8000003399 | TR CRDK989314 | Prathamesh Parab | 23.01.2024
          CLEAR: wa_set_people_cost_gl.
        ELSE.
          wa_final-fin_pnl_main = 'D. S&D EXPENSES'.
          wa_final-det_pnl = 'D. S&D EXPENSES'.
* Start of Change Inc 8000003399 | TR CRDK989314 | Prathamesh Parab | 23.01.2024
*         wa_final-fin_pnl_sub = 'D22. S&D COST BU TAGGING MISSING'.
          wa_final-fin_pnl_sub = 'D25. S&D COST BU TAGGING MISSING'.
*         wa_final-fin_tag = 'D22. S&D COST BU TAGGING MISSING'.
          wa_final-fin_tag = 'D25. S&D COST BU TAGGING MISSING'.
* End of Change Inc 8000003399 | TR CRDK989314 | Prathamesh Parab | 23.01.2024
        ENDIF.
        wa_final-remark = 'Customer Grp1_Tagging Missing'. " Change added by Inc 8000003399 | TR CRDK989314 | Prathamesh Parab | 23.01.2024
      ENDIF.
*      CLEAR: wa_tvv1t.
*   ELSE.
*   ELSEIF wa_final-zone_2 = co_bu_tagg. " Change added by Inc 8000003399 | TR CRDK989264 | Prathamesh Parab | 18.01.2024
    ELSEIF sy-subrc <> 0 OR wa_final-zone_2 = co_bu_tagg." Change added by Inc 8000003399 | TR CRDK989272 | Prathamesh Parab | 18.01.2024
      READ TABLE ta_midas_dept INTO wa_midas_dept WITH KEY department = wa_final-department
*                                                          zone_2 = wa_final-zone_2 " Change added by Inc 8000003399 | TR CRDK989274 | Prathamesh Parab | 18.01.2024
                                                           BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-fin_pnl_main = wa_midas_dept-fin_pnl_main.
        wa_final-det_pnl = wa_midas_dept-det_pnl.
        wa_final-fin_pnl_sub = wa_midas_dept-fin_pnl_sub.
        wa_final-fin_tag = wa_midas_dept-fin_pnl_sub.
        wa_final-zone_2 = wa_midas_dept-zone_2. " Change added by Inc 8000003399 | TR CRDK989330 | Prathamesh Parab | 24.01.2024
        wa_final-remark = 'Dept_ZMIDAS3_DEPT'. " Change added by Inc 8000003399 | TR CRDK989314 | Prathamesh Parab | 23.01.2024
        CLEAR: wa_midas_dept.
      ELSE.
        READ TABLE ta_midas_gl INTO wa_midas_gl WITH KEY hkont = wa_final-hkont
*                                                        zone_2 = wa_final-zone_2 " Change added by Inc 8000003399 | TR CRDK989274 | Prathamesh Parab | 18.01.2024
                                                         BINARY SEARCH.
        IF sy-subrc = 0.
          wa_final-fin_pnl_main = wa_midas_gl-fin_pnl_main.
          wa_final-det_pnl = wa_midas_gl-det_pnl.
          wa_final-fin_pnl_sub = wa_midas_gl-fin_pnl_sub.
          wa_final-fin_tag = wa_midas_gl-fin_pnl_sub.
*         wa_final-zone_2 = wa_midas_dept-zone_2. " Change added by Inc 8000003399 | TR CRDK989330 | Prathamesh Parab | 24.01.2024
          wa_final-zone_2 = wa_midas_gl-zone_2. "Added by Inc 8000003399 | TR CRDK989388 | Lalitha | 31.01.2024
          wa_final-remark = 'GL_ZMIDAS_GL'. " Change added by Inc 8000003399 | TR CRDK989314 | Prathamesh Parab | 23.01.2024
          CLEAR: wa_midas_gl.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: lv_zone.
* End of Change Inc 8000003399 | TR CRDK989166 | Prathamesh Parab | 09.01.2024

* Start of Change Inc 8000003399 | TR CRDK989330 | Prathamesh Parab | 24.01.2024
    IF wa_final-hkont = '0005210002'.
      READ TABLE ta_vbrk INTO wa_vbrk WITH KEY vbeln = wa_final-kidno+0(10).
      IF sy-subrc = 0.
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
    ENDIF.
* End of Change Inc 8000003399 | TR CRDK989330 | Prathamesh Parab | 24.01.2024

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

    "SOA by Gajanan - 06.10.2025 - CR#8000004621

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

    "EOA by Gajanan - 06.10.2025 - CR#8000004621

***SOA by Gajanan - 24.12.2025 - CR#8000004709***
    "Function
    IF wa_final-hkont IN r_central_gl[]. "Added by Gajanan - 27.04.2026 - CR#8000004929
      wa_final-zfunc = 'CENTRAL'. "Added by Gajanan - 27.04.2026 - CR#8000004929
    ELSE.

      READ TABLE ta_func_map INTO DATA(wa_func_map) WITH KEY kostl = wa_final-fin_kostl BINARY SEARCH.
*                                                           cc_div = wa_final-cc_div BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-zfunc = wa_func_map-zfunc.
        TRANSLATE wa_func_map-zfunc TO UPPER CASE.
      ENDIF.
    ENDIF.
***EOA by Gajanan - 24.12.2025 - CR#8000004709***

    APPEND wa_final TO ta_final.

    CLEAR: wa_fagll03,wa_final,wa_coas,wa_cskt,wa_csks,wa_t003t,wa_skat,wa_t003p,wa_prof_gl,wa_reimb,wa_pnl_hd,wa_elem,gv_pnl,gv_pnl2,
           wa_prevmo,wa_prof_cc,wa_prof_of,gv_remark,wa_det_pnl,wa_fin_tag,wa_grp,wa_subgrp,lv_exclud,lv_eu_tag,lv_exrate,ta_tcurr1,
           wa_zone_cc,wa_zone_ccgl,wa_zone_glsg,lv_name,lv_lifnr. "Added by Gajanan - 29.03.2024 - CR#8000003569
  ENDLOOP.

  IF NOT ta_final[] IS INITIAL.
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