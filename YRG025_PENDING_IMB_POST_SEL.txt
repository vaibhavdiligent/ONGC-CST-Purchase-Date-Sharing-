*&---------------------------------------------------------------------*
*&  Include           YRG025_PENDING_IMB_POST_SEL
*&---------------------------------------------------------------------*

TABLES:oijnomi, yrvt_contract,yrva_zcontrcls, vbak, vbap, vbkd.
TYPE-POOLS:slis.
DATA flg TYPE c.
CONSTANTS: gc_formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE'.
TYPES: BEGIN OF tt_sl,
        tsyst  TYPE oij_tsyst,
        region TYPE regio,
        vkbur  TYPE vkbur,
        payer  TYPE kunnr,
       END OF tt_sl.

DATA: it_sl TYPE TABLE OF tt_sl WITH HEADER LINE.

DATA : begda   TYPE datum,
       endda   TYPE datum,
       from_dat TYPE datum,
       to_dat   TYPE datum,
       value    TYPE datum.

DATA : l_date    TYPE d,
       lv_prev_dt TYPE datum.

TYPES:BEGIN OF ty_final,
        blocation          TYPE oijnomi-locid,          "Business Location
        customer           TYPE oijnomi-partnr,         "customer
        payer              TYPE char50,                  "payer
        payer_name         TYPE char50,                  "payer name
        cont_id            TYPE oijnomi-docnr,           "contract id,
        m_cont_id          TYPE vbak-vbeln_grp,          "Master Contract
        m_mas_cust         TYPE vbak-kunnr,              "Master Contract
        oic_region         TYPE char50,                  "oic region,
        trans_sys          TYPE char50,                  "Transport system
        sal_office         TYPE char50,                  "Sales office
        cum_bal_mbg_cal    TYPE p DECIMALS 3,            "Cumulative Imb in MBG (calculated)
        char_bal_mbg_cal   TYPE p DECIMALS 3,            "Chargeable Imb in MBG (calculated)
        neg_bal_mbg_cal    TYPE p DECIMALS 3,            " Negative Chg Imb in MBG (calculated) *------> SOC CHARM ID :4000008814 |TECHICAL : RAVINDER SINGH| FUNCTIONAL : SHREYOSI |DT:22.10.2024
        sal_order          TYPE vbrk-vbeln,              "sale order
        invoice            TYPE vbrk-vbeln,              "invoice
        cum_bal_mbg_cal_so TYPE p DECIMALS 3,            "Cumulative Imb in MBG (posted in SO)
        char_bal_mbg_cal_so TYPE p DECIMALS 3,           "Chargeable Imb in MBG (posted in SO)
        neg_bal_mbg_cal_so TYPE p DECIMALS 3,            " Negative Chg Imb in MBG (posted in SO) *------> SOC CHARM ID :4000008814 |TECHICAL : RAVINDER SINGH| FUNCTIONAL : SHREYOSI |DT:22.10.2024
        rgmc_mail          TYPE char50,                  "Rgmc mail
        oic_mail           TYPE char50,                  "oic mail
        indicator          TYPE char1,                   "indicator.
*------> SOC CHARM ID :4000008814 |TECHICAL : RAVINDER SINGH| FUNCTIONAL : SHREYOSI |DT:22.10.2024
        cum_mst_imb        TYPE p DECIMALS 3,            "Cumulative Master Imb (Calculated)
        usage              TYPE char4,
*------> EOC CHARM ID :4000008814 |TECHICAL : RAVINDER SINGH| FUNCTIONAL : SHREYOSI |DT:22.10.2024
      END OF ty_final.

DATA:it_final    TYPE STANDARD TABLE OF ty_final,
     it_final_im TYPE STANDARD TABLE OF ty_final,          "++ BY MANMOHAN/SHREYOSI CHARM: 2000000816 DATE:16.07.2024
     wa_final    TYPE ty_final.

DATA:it_fcat             TYPE slis_t_fieldcat_alv,
     wa_fcat             TYPE slis_fieldcat_alv,
     gt_list_top_of_page TYPE slis_t_listheader,
     gt_events           TYPE slis_t_event.

DATA:i_rspartab  TYPE TABLE OF rsparams,
     wa_rspartab LIKE LINE OF i_rspartab.

TYPES: BEGIN OF ty_cust,
        kunnr TYPE oijnomi-partnr,
       END OF ty_cust.

DATA: it_cust TYPE STANDARD TABLE OF ty_cust,
      wa_cust TYPE ty_cust.

DATA:w_layout    TYPE slis_layout_alv.

TYPES: BEGIN OF ty_adrc,
        locid  TYPE oij_locid,
        street(60),
        region TYPE regio,
        name1  TYPE ad_name1,
        adrnr  TYPE adrnr,
       END OF ty_adrc .
DATA: it_adrc1 TYPE STANDARD TABLE OF ty_adrc,
      wa_adrc1 TYPE ty_adrc.

TYPES: BEGIN OF ty_display1,
        sel,
        vbeln        TYPE vbeln_va,
        ktext        TYPE ktext_v,
        knkli        TYPE vbeln_va,
        kunnr        TYPE kunnr,
        matnr        TYPE matnr,
        totalpi      TYPE yyoij_pos_cumimb,
        totalni      TYPE yyoij_pos_cumimb,
        totalpic     TYPE yyoij_pos_cumimb,
        totalnic     TYPE yyoij_pos_cumimb,
        totaldds     TYPE yyoij_dds_qty,
        totaldds_t   TYPE yyoij_dds_qty,
        totaloverrun TYPE yyoij_pos_cumimb,
        totalcharge  TYPE yyoij_pos_cumimb,
        totalcumi    TYPE yyoij_pos_cumimb,
        vbeln1       TYPE vbeln_va,
        vbeln_grp    TYPE vbeln_grp,
        wt           TYPE zreewr,
        wt1          TYPE zreewr,
        totalpic1    TYPE yyoij_pos_cumimb,
        totalnic1    TYPE yyoij_pos_cumimb,
        totaldds1    TYPE yyoij_pos_cumimb,
        opencumi     TYPE yyoij_pos_cumimb,
        closecumi    TYPE yyoij_pos_cumimb,
        line_color(4) TYPE c,
        augru        TYPE augru,
       END OF ty_display1.

*SOC - Types and data declarations for local imbalance processing (replicated from YRG011)
TYPES: BEGIN OF ty_display_d,
         sel,
         date           TYPE datum,
         vbeln          TYPE vbeln_va,
         vbeln_c        TYPE vbeln_va,
         ktext          TYPE ktext_v,
         kunnr          TYPE kunnr,
         matnr          TYPE matnr,
         del_point      TYPE yyoij_dpimb_qty,
         del_uom        TYPE yyoij_cnom_uom,
         corr_nom       TYPE yyoij_dpimb_qty,
         corr_uom       TYPE yyoij_cnom_uom,
         redel_point    TYPE yyoij_dpimb_qty,
         redel_uom      TYPE yyoij_cnom_uom,
         corr_nom1      TYPE yyoij_dpimb_qty,
         corr_uom1      TYPE yyoij_cnom_uom,
         daily          TYPE yyoij_pos_cumimb,
         daily_dds      TYPE yyoij_dds_qty,
         daily_cumm     TYPE yyoij_pos_cumimb,
         daily_cumm_dds TYPE yyoij_dds_qty,
         daily_charge   TYPE yyoij_pos_cumimb,
         daily_overrun  TYPE yyoij_pos_cumimb,
         chargable_imb  TYPE yyoij_pos_cumimb,
         menge          TYPE oij_menge,
         agcv           TYPE yy_calval,
         ancv           TYPE yy_calval,
         agcv1          TYPE yy_calval,
         ancv1          TYPE yy_calval,
         mdq_quan       TYPE yyoij_pos_cumimb,
         del_mbg        TYPE yyoij_pos_cumimb,
         redel_mbg      TYPE yyoij_pos_cumimb,
         vbeln_grp      TYPE vbak-vbeln_grp,
         augru          TYPE augru,
         line_color(4)  TYPE c,
       END OF ty_display_d.

TYPES: BEGIN OF ty_vbak_d,
         vbeln     TYPE vbeln_va,
         auart     TYPE auart,
         kunnr     TYPE kunag,
         bstnk     TYPE vbeln_va,
         knkli     TYPE knkli,
         ktext     TYPE ktext_v,
         vbeln_grp TYPE vbeln_grp,
         abrvw     TYPE abrvw,
         augru     TYPE augru,
       END OF ty_vbak_d.

TYPES: BEGIN OF ty_oijnomi_d,
         ga_allocated_qty TYPE oijnomi-ga_allocated_qty,
         idate            TYPE oijnomi-idate,
         menge            TYPE oijnomi-menge,
         delind           TYPE oijnomi-delind,
         s_matnr_i        TYPE oijnomi-s_matnr_i,
         docnr            TYPE oijnomi-docnr,
         nomtk            TYPE oijnomi-nomtk,
         nomit            TYPE oijnomi-nomit,
         sityp            TYPE oijnomi-sityp,
         docind           TYPE oijnomi-docind,
         yyoij_dds_qty    TYPE oijnomi-yyoij_dds_qty,
         yyoij_unaovr_qty TYPE oijnomi-yyoij_unaovr_qty,
         yyoij_ovr_qty    TYPE oijnomi-yyoij_ovr_qty,
         yyoij_dnimb_qty  TYPE oijnomi-yyoij_dnimb_qty,
         yyoij_dpimb_qty  TYPE oijnomi-yyoij_dpimb_qty,
         yyoij_cnom_uom   TYPE oijnomi-yyoij_cnom_uom,
         yyoij_cnom_qty   TYPE oijnomi-yyoij_cnom_qty,
       END OF ty_oijnomi_d.

TYPES: BEGIN OF ty_oij_el_cp_layt_d,
         mdq_quan TYPE oij_el_cp_layt-mdq_quan,
         vbeln    TYPE oij_el_cp_layt-vbeln,
         mdq_uom  TYPE oij_el_cp_layt-mdq_uom,
       END OF ty_oij_el_cp_layt_d.

TYPES: BEGIN OF ty_yro_nom_param1_d,
         nomtk TYPE yro_nom_param1-nomtk,
         nomit TYPE yro_nom_param1-nomit,
         ancv  TYPE yro_nom_param1-ancv,
         agcv  TYPE yro_nom_param1-agcv,
       END OF ty_yro_nom_param1_d.

TYPES: BEGIN OF ty_veda_d,
         vbeln   TYPE veda-vbeln,
         vbegdat TYPE veda-vbegdat,
         venddat TYPE veda-venddat,
       END OF ty_veda_d.

TYPES: BEGIN OF ty_yrvt_cont_dcq_d,
         mandt    TYPE mandt,
         vbeln    TYPE vbeln_va,
         ydate    TYPE ydats1,
         sy_uname TYPE uname,
         creon    TYPE erdat,
         time     TYPE uzeit,
         dcq_qty  TYPE ydcq_qty,
         max_qty  TYPE yzpercent,
         min_qty  TYPE yzpercent,
         flag(1),
         timest   TYPE num14,
       END OF ty_yrvt_cont_dcq_d.

* Data declarations for imbalance fetch and posting
DATA: gt_vbak_d          TYPE TABLE OF ty_vbak_d,
      gt_vbak1_d         TYPE TABLE OF ty_vbak_d,
      gt_vbak2_d         TYPE TABLE OF ty_vbak_d,
      gt_vbak3_d         TYPE TABLE OF ty_vbak_d,
      gt_vbak4_d         TYPE TABLE OF ty_vbak_d,
      gt_vbak_temp_d     TYPE TABLE OF ty_vbak_d,
      gt_vbak_del_d      TYPE TABLE OF ty_vbak_d,
      gw_vbak_d          TYPE ty_vbak_d,
      gw_vbak1_d         TYPE ty_vbak_d,
      gw_vbak3_d         TYPE ty_vbak_d,
      gw_vbak4_d         TYPE ty_vbak_d,
      gt_veda_d          TYPE STANDARD TABLE OF ty_veda_d,
      gw_veda_d          TYPE ty_veda_d,
      gt_oijnomi_d       TYPE STANDARD TABLE OF ty_oijnomi_d,
      gt_oij_d           TYPE STANDARD TABLE OF ty_oijnomi_d,
      gw_oijnomi_d       TYPE ty_oijnomi_d,
      gw_oijnomi1_d      TYPE ty_oijnomi_d,
      gt_yrg_cumm_imb_d  TYPE TABLE OF yrg_cumm_imb,
      gt_yrg_cumm_imb1_d TYPE TABLE OF yrg_cumm_imb,
      gt_yrg_cumm_imb_temp_d TYPE TABLE OF yrg_cumm_imb,
      gt_yrg_chg_ovrrun_d TYPE TABLE OF yrg_chg_ovrrun,
      gw_yrg_cumm_imb_d  TYPE yrg_cumm_imb,
      gw_yrg_cumm_imb1_d TYPE yrg_cumm_imb,
      gw_yrg_chg_ovrrun_d TYPE yrg_chg_ovrrun,
      gt_display_d       TYPE TABLE OF ty_display_d,
      gt_disp_temp_d     TYPE TABLE OF ty_display_d,
      gt_display_temp_d  TYPE TABLE OF ty_display_d,
      gt_display3_d      TYPE TABLE OF ty_display_d,
      gw_display_d       TYPE ty_display_d,
      gw_display_temp_d  TYPE ty_display_d,
      gt_yrvt_contract_d TYPE TABLE OF yrvt_contract,
      gw_yrvt_contract_d TYPE yrvt_contract,
      gw_yrvt_contract1_d TYPE yrvt_contract,
      gt_oij_el_cp_layt_d TYPE TABLE OF ty_oij_el_cp_layt_d,
      gw_oij_el_cp_layt_d TYPE ty_oij_el_cp_layt_d,
      gt_yro_nom_param_d TYPE TABLE OF yro_nom_param,
      gw_yro_nom_param_d TYPE yro_nom_param,
      gt_yro_nom_param1_d TYPE TABLE OF ty_yro_nom_param1_d,
      gw_yro_nom_param1_d TYPE ty_yro_nom_param1_d,
      gt_gta_imb_sft_d   TYPE TABLE OF yrva_gta_imb_sft,
      gt_header_d        TYPE TABLE OF yrgs_imb_header,
      gw_header_d        TYPE yrgs_imb_header,
      gt_lt_yrvt_cont_dcq_d TYPE TABLE OF ty_yrvt_cont_dcq_d,
      gw_ls_yrvt_cont_dcq_d TYPE ty_yrvt_cont_dcq_d,
      gw_display12_d     TYPE ty_display1,
      gw_display11_d     TYPE ty_display1,
      gt_display1_post   TYPE TABLE OF ty_display1.

DATA: BEGIN OF gt_matnr_d OCCURS 0,
        vbeln TYPE vbeln,
        matnr TYPE matnr,
        kunnr TYPE kunnr,
      END OF gt_matnr_d.

DATA: gt_978_d  TYPE STANDARD TABLE OF a978 WITH HEADER LINE,
      gt_a305_d TYPE STANDARD TABLE OF a305 WITH HEADER LINE,
      gw_konp_d TYPE konp,
      gt_konp_d TYPE STANDARD TABLE OF konp WITH HEADER LINE.

DATA: g_daily_d       TYPE yyoij_pos_cumimb,
      g_cumm_bal_d    TYPE yyoij_pos_cumimb,
      g_daily1_d      TYPE yyoij_pos_cumimb,
      g_daily2_d      TYPE yyoij_pos_cumimb,
      g_mdq_quan_d    TYPE msego2-adqnt,
      g_refvalue_d    TYPE yyoij_pos_cumimb,
      g_adq_d         TYPE oib_adqnt,
      g_menge_d       TYPE oij_menge,
      g_wt_d          TYPE zreewr,
      g_wt1_d         TYPE zreewr,
      g_master_contract_d TYPE vbeln,
      g_pos_imb_charg_d TYPE yyoij_pos_cumimb,
      g_neg_imb_charg_d TYPE yyoij_pos_cumimb,
      g_dds_imb_charg_d TYPE yyoij_pos_cumimb,
      g_price_uom_d   TYPE dzieme,
      g_exit_processing_d TYPE c,
      g_futher_processing_d TYPE c.

DATA: g_salesdocument_d    TYPE bapivbeln-vbeln,
      g_order_header_inx_d TYPE bapisdh1x,
      gt_return_d          TYPE STANDARD TABLE OF bapiret2,
      g_is_t001w_d         TYPE t001w.

FIELD-SYMBOLS : <gfs_display1_d> TYPE ty_display1.

* Additional variables for local imbalance processing
DATA: g_w_master_contract_d TYPE vbeln,
      g_w_group_imbalance_d(1),
      g_l_daily_imb_d       TYPE yyoij_pos_cumimb,
      g_l_daily1_imb_d      TYPE yyoij_pos_cumimb,
      g_l_daily2_imb_d      TYPE yyoij_pos_cumimb,
      g_l_cumm_bal_imb_d    TYPE yyoij_pos_cumimb,
      g_w_temp_d            TYPE p DECIMALS 3,
      g_w_temp1_d           TYPE p DECIMALS 3,
      g_lv_pre_check_lines_d TYPE sy-tabix,
      g_lv_index_d          TYPE sytabix,
      g_lv_type_d(10)       TYPE c,
      g_lv_15_d             TYPE c,
      g_lv_11_d             TYPE c,
      g_lv_10_d             TYPE c,
      g_lv_12_d             TYPE c,
      g_process_flag_d      TYPE c,
      g_lv_clause_not_found_d TYPE c,
      g_l_message_d         TYPE char100,
      g_l_date1_d           TYPE datum,
      g_subrc_d             TYPE sy-subrc.

DATA: g_es_oikimport_d   TYPE roikimport,
      g_ls_oikload_d      TYPE roikload_n,
      g_lt_oikload_d      LIKE TABLE OF g_ls_oikload_d,
      g_ls_oikexport_d    TYPE roikexport.

DATA: g_it_display_d      TYPE TABLE OF ty_display_d,
      g_it_display11_d    TYPE TABLE OF ty_display1,
      g_it_display_temp_d TYPE TABLE OF ty_display_d,
      g_it_display3_loc_d TYPE TABLE OF ty_display_d,
      g_it_disp_temp_d    TYPE TABLE OF ty_display_d,
      g_wa_display_loc_d  TYPE ty_display_d,
      g_wa_display_temp_loc_d TYPE ty_display_d,
      g_it_yrg_cumm_imb2_d TYPE TABLE OF yrg_cumm_imb,
      g_it_vbak_del_loc_d TYPE TABLE OF ty_vbak_d.

DATA: g_lv_log_d TYPE TABLE OF yrga_email_log,
      g_wv_log_d TYPE yrga_email_log.

*EOC - Types and data declarations for local imbalance processing

DATA: it_display1 TYPE TABLE OF ty_display1,
      it_display2 TYPE TABLE OF ty_display1,
      wa_display1 TYPE ty_display1.




SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_date  FOR oijnomi-idate OBLIGATORY,
                locid   FOR oijnomi-locid,
                partnr  FOR oijnomi-partnr,
                s_tsyst FOR it_sl-tsyst,
                s_region FOR it_sl-region,
                s_vkbur FOR it_sl-vkbur,
                s_payer FOR it_sl-payer.
SELECTION-SCREEN SKIP.
PARAMETERS ch1 AS CHECKBOX.
*SOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbalance TR:DVRK9A1POQ
parameters: p_auto as checkbox.
*EOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbalance TR:DVRK9A1POQ
*SOC Background auto-posting
PARAMETERS: p_bkgnd AS CHECKBOX.  "Background
*EOC Background auto-posting
SELECTION-SCREEN END OF BLOCK b1.



INITIALIZATION.
  CONSTANTS: c_num_char(11) TYPE c VALUE '1234567890 '.
  DATA: dnpi_flag TYPE char1,
        w_uname   TYPE xubname.
  CLEAR: w_uname,dnpi_flag.
** -> Begin of changes by of Aditi on 29.11.2024 14:30:08 for ATC
*  SELECT SINGLE uname FROM agr_users INTO w_uname WHERE from_dat LE sy-datum AND to_dat GE sy-datum AND
*    uname = sy-uname AND ( agr_name = 'ZO_GMS_CORETEAM' OR agr_name = 'ZO_CC_EHS.GMS_ROLE' ) .
  SELECT uname FROM agr_users INTO w_uname UP TO 1 ROWS WHERE from_dat LE sy-datum AND to_dat GE sy-datum AND
    uname = sy-uname AND ( agr_name = 'ZO_GMS_CORETEAM' OR agr_name = 'ZO_CC_EHS.GMS_ROLE' ) ORDER BY PRIMARY KEY.
  ENDSELECT.
** -> End of changes by of Aditi on 29.11.2024 14:30:11 for ATC
  IF ( NOT sy-uname CO c_num_char ) OR w_uname IS NOT INITIAL.
    dnpi_flag = 'X'.
  ENDIF.

  PERFORM e03_eventtab_build USING gt_events[].

  value =  sy-datum.
  CALL FUNCTION 'YRX_PRVS_DATE_FM'
    EXPORTING
      s_date  = value
    IMPORTING
      st_date = s_date-low
      ed_date = s_date-high.
  APPEND s_date.


AT SELECTION-SCREEN.

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date              = s_date-low
    IMPORTING
      ev_month_begin_date = begda
      ev_month_end_date    = endda.

  IF '01' <= s_date-low+6(2) AND s_date-low+6(2) <= '15'.
    to_dat = s_date-low + 14.
  ELSEIF '16' <= s_date-low+6(2) AND s_date-low+6(2) <=  endda+6(2).
    to_dat = endda.
  ENDIF.


  IF s_date-low+6(2) EQ '01' OR s_date-low+6(2) EQ '16'.
    IF s_date-low+6(2) EQ '01'.
      IF s_date-high+6(2) NE '15' .
        MESSAGE 'Enter fortnight Date Only' TYPE 'E'.
      ENDIF.
    ELSEIF s_date-low+6(2) EQ '16'.
      IF s_date-high+6(2) NE endda+6(2) .
        MESSAGE 'Enter fortnight Date Only' TYPE 'E'.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE 'Please Enter Start Date of Fortnight(From Date)' TYPE 'E'.
  ENDIF.

  IF s_date-high+4(2) NE s_date-low+4(2) .
    MESSAGE 'Please enter date range of same month' TYPE 'E'.
  ENDIF.

  IF NOT s_date-high BETWEEN s_date-low AND to_dat.
    MESSAGE 'Enter fortnight Date Only' TYPE 'E'.
  ENDIF.
