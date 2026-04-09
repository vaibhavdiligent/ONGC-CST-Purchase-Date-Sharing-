*&---------------------------------------------------------------------*
*& Include          YRG025_PENDING_IMB_POST_SEL
*&---------------------------------------------------------------------*

TABLES:oijnomi, yrvt_contract,yrva_zcontrcls.
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

DATA : begda    TYPE datum,
       endda    TYPE datum,
       from_dat TYPE datum,
       to_dat   TYPE datum,
       value    TYPE datum.

DATA : l_date     TYPE d,
       lv_prev_dt TYPE datum.

TYPES:BEGIN OF ty_final,
        blocation        TYPE oijnomi-locid,         "Business Location
        customer         TYPE oijnomi-partnr,         "customer
        payer            TYPE char50,                 "payer
        payer_name       TYPE char50,                 "payer name
        cont_id          TYPE oijnomi-docnr,           "contract id,
        m_cont_id        TYPE vbak-vbeln_grp,          "Master Contract
        m_mas_cust       TYPE vbak-kunnr,              "Master Contract
        oic_region       TYPE char50,                  "oic region,
        trans_sys        TYPE char50,                  "Transport system
        sal_office       TYPE char50,                  "Sales office
        cum_bal_mbg_cal  TYPE p DECIMALS 3,            "Cumulative Imb in MBG (calculated)
        char_bal_mbg_cal TYPE p DECIMALS 3,            "Chargeable Imb in MBG (calculated)
        neg_bal_mbg_cal  TYPE p DECIMALS 3,            "Negative Chg Imb in MBG (calculated)
        "*-> SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:22.10.2024
        sal_order        TYPE vbrk-vbeln,              "sale order
        invoice          TYPE vbrk-vbeln,              "invoice
        cum_bal_mbg_cal_so  TYPE p DECIMALS 3,         "Cumulative Imb in MBG (posted in SO)
        char_bal_mbg_cal_so TYPE p DECIMALS 3,         "Chargeable Imb in MBG (posted in SO)
        neg_bal_mbg_cal_so  TYPE p DECIMALS 3,         "Negative Chg Imb in MBG (posted in SO)
        "*-> SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:22.10.2024
        rgmc_mail        TYPE char50,                  "Rgmc mail
        oic_mail         TYPE char50,                  "oic mail
        indicator        TYPE char1,                   "indicator.
        "*-> SOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:22.10.2024
        cum_mst_imb      TYPE p DECIMALS 3,            "Cumulative Master Imb (Calculated)
        usage            TYPE char4,
        "*-> EOC CHARM ID :4000008814 TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSI DT:22.10.2024
*SOC Email Pending Postings
        diff_cum_imb     TYPE p DECIMALS 3,            "Diff (Cumulative Imb) = Calc - Posted
        diff_neg_chg_imb TYPE p DECIMALS 3,            "Diff (Negative Chg Imb) = Calc - Posted
        diff_pos_chg_imb TYPE p DECIMALS 3,            "Diff (Positive Chg Imb) = Calc - Posted
        line_color(4)    TYPE c,                        "Row color for ALV
*EOC Email Pending Postings
      END OF ty_final.

DATA:it_final    TYPE STANDARD TABLE OF ty_final,
     it_final_im TYPE STANDARD TABLE OF ty_final,    "++ BY MANMOHAN/SHREYOSI CHARM: 2000000816 DATE:16.07.2024
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

DATA:w_layout TYPE slis_layout_alv.

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
         vbeln      TYPE vbeln_va,
         ktext      TYPE ktext_v,
         knkli      TYPE vbeln_va,
         kunnr      TYPE kunnr,
         matnr      TYPE matnr,
         totalpi    TYPE yyoij_pos_cumimb,
         totalni    TYPE yyoij_pos_cumimb,
         totalpic   TYPE yyoij_pos_cumimb,
         totalnic   TYPE yyoij_pos_cumimb,
         totaldds   TYPE yyoij_dds_qty,
         totaldds_t TYPE yyoij_dds_qty,
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
*SOC Email Pending Postings - hidden, visible only to role ZO_CC_EHS.GMS_ROLE
PARAMETERS: p_email AS CHECKBOX.
*EOC Email Pending Postings
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
    uname = sy-uname AND ( agr_name = 'ZO_GMS_CORETEAM' OR agr_name = 'ZO_CC_EHS.GMS_ROLE' )  ORDER BY PRIMARY KEY.
  ENDSELECT.
** -> End of changes by of Aditi on 29.11.2024 14:30:11 for ATC
  IF ( NOT sy-uname CO c_num_char ) OR w_uname IS NOT INITIAL.
    dnpi_flag = 'X'.
  ENDIF.

  PERFORM e03_eventtab_build USING gt_events[].

  value = sy-datum.
  CALL FUNCTION 'YRX_PRVS_DATE_FM'
    EXPORTING
      s_date   = value
    IMPORTING
      st_date  = s_date-low
      ed_date  = s_date-high.
  APPEND s_date.


AT SELECTION-SCREEN OUTPUT.
*SOC Email Pending Postings - hide p_email for users without ZO_CC_EHS.GMS_ROLE
  LOOP AT SCREEN.
    IF screen-name = 'P_EMAIL'.
      IF dnpi_flag NE 'X'.
        screen-invisible = '1'.
        screen-active    = '0'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*EOC Email Pending Postings

AT SELECTION-SCREEN.

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date            = s_date-low
    IMPORTING
      ev_month_begin_date = begda
      ev_month_end_date   = endda.

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
