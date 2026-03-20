*                                                                       *
* Report Name         : YRG010_IMBALANCE_REPORT
* Description         : Imbalance Report
* Type                : e
* Version             : 1.0
* Development class   : zabap
* Function Consultant : Atul S.
* Author              : Shantanu Singh
*                                                                       *
* Modification Log:
* Date        Author      Req.No.       Change History
* 10.01.2019 vikram bajaj              program copied from YRG011_IMBALAN CE_REPORT_N and worked for DCQ values as per charm 4000001479
* 27.05.2024 Numan Patel(XSAP_AB) S 4000008333: GMS: Auto IMB Shift_CT End Inv
*                                                                       *
REPORT yrg011_imbalance_report_dcq.

*                                                                           *
*Tables:
TABLES : oijnomi.

TYPE-POOLS : slis.

CONSTANTS: gc_formname_top_of_page TYPE slis_formname
                                    VALUE 'TOP_OF_PAGE'.
DATA: gr_table     TYPE REF TO cl_salv_table,
      gr_functions TYPE REF TO cl_salv_functions_list,
      gr_columns   TYPE REF TO cl_salv_columns_table,
      gr_column    TYPE REF TO cl_salv_column,
      gr_col       TYPE REF TO cl_salv_column,
      gr_display   TYPE REF TO cl_salv_display_settings,
      gr_layout    TYPE REF TO cl_salv_layout,
      gv_key       TYPE salv_s_layout_key,
      gr_header    TYPE REF TO cl_salv_form_layout_grid,
      gr_label     TYPE REF TO cl_salv_form_label,
      gr_flow      TYPE REF TO cl_salv_form_layout_flow,
      gr_logo      TYPE REF TO cl_salv_form_layout_logo,
      gs_color     TYPE lvc_s_colo,
      gf_text      TYPE string,
      gf_text2     TYPE string,
      lv_len       TYPE i.
DATA it_wt_prc TYPE STANDARD TABLE OF yrga_wt_avg_prc.
DATA: es_oikimport TYPE roikimport,
** -> Begin of changes by of Aditi on 19/11/24 for ATC
*      ls_oikload   TYPE roikload,
      ls_oikload   TYPE roikload_n,
** <- End changes by of Aditi on 19/11/24 for ATC
      lt_oikload   LIKE TABLE OF ls_oikload,
      ls_oikexport TYPE roikexport.
DATA is_t001w TYPE t001w.

DATA: gt_list_top_of_page TYPE slis_t_listheader,
      wa_list_top_of_page TYPE slis_listheader. "XSAP

DATA: it_cond_fc TYPE slis_t_fieldcat_alv,              "Field catalogue.
      gt_events TYPE slis_t_event.
DATA: w_cond_fc TYPE slis_fieldcat_alv,
      w_layout   TYPE slis_layout_alv,
      w_title    TYPE lvc_title,
      w_cust_id TYPE char12,
       w_custname TYPE name1_gp,
       w_repid    TYPE sy-repid.
DATA   it_cond_fc1 TYPE slis_t_fieldcat_alv.           "Field catalogue.
*                                                                      *
* types

TYPES: BEGIN OF ty_display,
         sel,
         date           TYPE datum,
         vbeln          TYPE vbeln_va,
         vbeln_c        TYPE vbeln_va,   "Added by XSAP_AB on 5th June
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
         daily_overrun TYPE yyoij_pos_cumimb,
         chargable_imb TYPE yyoij_pos_cumimb,
         menge          TYPE oij_menge,
         agcv           TYPE yy_calval,
         ancv           TYPE yy_calval,
         agcv1          TYPE yy_calval,
         ancv1          TYPE yy_calval,
         mdq_quan       TYPE yyoij_pos_cumimb,
         del_mbg        TYPE yyoij_pos_cumimb, " DELIVERY MBG QTY
         redel_mbg      TYPE yyoij_pos_cumimb, "REDELIVERY MBG QTY
         vbeln_grp      TYPE vbak-vbeln_grp,
         augru          TYPE augru,
**********Added By Nitin Dhamija on 16.07.2019
         line_color(4) TYPE c,                   " Line Color
       END OF ty_display.
TYPES: BEGIN OF ty_display1,
         sel,
         vbeln         TYPE vbeln_va,
         ktext         TYPE ktext_v,
         knkli         TYPE vbeln_va,
         kunnr         TYPE kunnr,
         matnr         TYPE matnr,
         totalpi       TYPE yyoij_pos_cumimb,
         totalni       TYPE yyoij_pos_cumimb,
         totalpic      TYPE yyoij_pos_cumimb,
         totalnic      TYPE yyoij_pos_cumimb,
         totaldds      TYPE yyoij_dds_qty,
         totaldds_t    TYPE yyoij_dds_qty,
         totaloverrun TYPE yyoij_pos_cumimb,
         totalcharge   TYPE yyoij_pos_cumimb,
         totalcumi     TYPE yyoij_pos_cumimb,
         vbeln1        TYPE vbeln_va,
         vbeln_grp     TYPE vbeln_grp,
         wt            TYPE zreewr,
         wt1           TYPE zreewr,
         totalpic1     TYPE yyoij_pos_cumimb,
         totalnic1     TYPE yyoij_pos_cumimb,
         totaldds1     TYPE yyoij_pos_cumimb,
         opencumi      TYPE yyoij_pos_cumimb,
         closecumi     TYPE yyoij_pos_cumimb,
         line_color(4) TYPE c,
         augru         TYPE augru,
       END OF ty_display1.

TYPES: BEGIN OF ty_vbak,
         vbeln     TYPE vbeln_va,
         auart     TYPE auart,
         kunnr     TYPE kunag,
         bstnk     TYPE vbeln_va,
         knkli     TYPE knkli,
         ktext     TYPE ktext_v,
         vbeln_grp TYPE vbeln_grp,
         abrvw     TYPE abrvw,
         augru     TYPE augru,
       END OF ty_vbak.

TYPES: BEGIN OF ty_oijnomr,
         nomtk TYPE oij_nomtk,
         docnr TYPE oij_docnr,
         docitm TYPE oij_docitm,
       END OF ty_oijnomr.

DATA: BEGIN OF it_matnr OCCURS 0 ,
        vbeln TYPE vbeln,
        matnr TYPE matnr,
        kunnr TYPE kunnr,
      END OF it_matnr.

TYPES: BEGIN OF ty_f4vbak,
         vbeln TYPE vbeln,
         guebg TYPE guebg,
         gueen TYPE gueen,
       END OF ty_f4vbak.
*****soc by suhani and arghya mondal on 05.08.19
TYPES:BEGIN OF ty_oijnomi,
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
        yyoij_dnimb_qty TYPE oijnomi-yyoij_dnimb_qty,
        yyoij_dpimb_qty TYPE oijnomi-yyoij_dpimb_qty,
        yyoij_cnom_uom   TYPE oijnomi-yyoij_cnom_uom,
        yyoij_cnom_qty   TYPE oijnomi-yyoij_cnom_qty,

      END OF ty_oijnomi.
TYPES: BEGIN OF ty_oij_el_cp_layt,
         mdq_quan TYPE oij_el_cp_layt-mdq_quan,
         vbeln    TYPE oij_el_cp_layt-vbeln,
         mdq_uom TYPE oij_el_cp_layt-mdq_uom,
       END OF ty_oij_el_cp_layt.
TYPES:BEGIN OF ty_yro_nom_param1,
        nomtk TYPE yro_nom_param1-nomtk,
        nomit TYPE yro_nom_param1-nomit,
        ancv TYPE yro_nom_param1-ancv,
        agcv TYPE yro_nom_param1-agcv,
      END OF ty_yro_nom_param1.
*****eoc by suhani and arghya mondal on 05.08.19
TYPES: BEGIN OF ty_veda,
         vbeln   TYPE veda-vbeln,
         vbegdat TYPE veda-vbegdat,
         venddat TYPE veda-venddat,
       END OF ty_veda,
       "Start by XSAP_AB on 30th May,2024
       BEGIN OF ty_parent,
         vbeln TYPE vbeln,
       END OF ty_parent,
       "End by XSAP_AB on 30th May,2024
       "Start by XSAP_AB on 11th Sept,2024 - Imbalance Master Shifting
       BEGIN OF ty_r_veda,
         sign(1)   TYPE c,
         option(2) TYPE c,
         low       TYPE vbeln,
         high      TYPE vbeln,
       END OF ty_r_veda.
"End by XSAP_AB on 11th Sept,2024
*                                                                      *
* internal tables & workarea

DATA : wa_veda              TYPE ty_veda,
       it_veda              TYPE STANDARD TABLE OF ty_veda,
       it_nomi              TYPE TABLE OF oijnomi,
       it_vbak              TYPE TABLE OF ty_vbak,
       it_vbak_temp         TYPE TABLE OF ty_vbak,
       wa_vbak              TYPE ty_vbak,
       wa_vbak1             TYPE ty_vbak,
       it_oijnomr           TYPE TABLE OF ty_oijnomr,
       wa_oijnomr           TYPE ty_oijnomr,
*       it_oijnomi           TYPE TABLE OF oijnomi,
       it_oijnomi           TYPE STANDARD TABLE OF ty_oijnomi,
       it_oij               TYPE STANDARD TABLE OF ty_oijnomi,
       wa_oijnomi           TYPE ty_oijnomi, ""oijnomi,
       wa_oijnomi1          TYPE ty_oijnomi, ""oijnomi,
       it_yrg_cumm_imb      TYPE TABLE OF yrg_cumm_imb,
       it_yrg_cumm_dds      TYPE TABLE OF yrg_cumm_dds,

       it_yrg_cumm_imb_temp TYPE TABLE OF yrg_cumm_imb,
       it_yrg_cumm_imb1     TYPE TABLE OF yrg_cumm_imb,
       it_yrg_cumm_imb2     TYPE TABLE OF yrg_cumm_imb,
       it_yrg_chg_ovrrun    TYPE TABLE OF yrg_chg_ovrrun,
       wa_yrg_chg_ovrrun    TYPE yrg_chg_ovrrun,
       wa_yrg_cumm_imb      TYPE yrg_cumm_imb,
       wa_yrg_cumm_imb1     TYPE yrg_cumm_imb,
       it_display           TYPE TABLE OF ty_display,
       it_disp_temp         TYPE TABLE OF ty_display,           "Added by X SAP_AB on 27th May,2024
       it_gta_imb_sft       TYPE TABLE OF yrva_gta_imb_sft,     "Added by X SAP_AB on 27th May,2024
       wa_parent            TYPE ty_parent,                  "Added by X SAP_AB on 27th May,2024
       it_parent            TYPE TABLE OF ty_parent,         "Added by X SAP_AB on 27th May,2024
       it_header            TYPE TABLE OF yrgs_imb_header,   "Added by X SAP_AB on 18th June,2024
       wa_header            TYPE yrgs_imb_header,            "Added by X SAP_AB on 18th June,2024
       it_pdf               TYPE TABLE OF yrgs_imb_display,
       wa_pdf               TYPE yrgs_imb_display,
       it_pdf1              TYPE TABLE OF yrgs_imb_display1,
       wa_pdf1              TYPE yrgs_imb_display1,
       it_display_temp      TYPE TABLE OF ty_display,
       wa_display           TYPE ty_display,
       wa_display_temp      TYPE ty_display,
       it_display1          TYPE TABLE OF ty_display1,
       wa_display1          TYPE ty_display1,
       it_display11         TYPE TABLE OF ty_display1,
       wa_display11         TYPE ty_display1,
       wa_display12         TYPE ty_display1,
       it_yrvt_contract     TYPE TABLE OF yrvt_contract,
       wa_yrvt_contract     TYPE yrvt_contract,
       wa_yrvt_contract1    TYPE yrvt_contract,
       it_oij_el_cp_layt    TYPE TABLE OF ty_oij_el_cp_layt, ""oij_el_c p_layt
       wa_oij_el_cp_layt    TYPE ty_oij_el_cp_layt,
       wa_oij_el_cp_layt1   TYPE oij_el_cp_layt,
       it_yro_nom_param     TYPE TABLE OF yro_nom_param,
       wa_yro_nom_param     TYPE yro_nom_param,
       it_yro_nom_param1    TYPE TABLE OF ty_yro_nom_param1, ""yro_nom_p aram1
       wa_yro_nom_param1    TYPE ty_yro_nom_param1, ""yro_nom_param1
       it_vbak1             TYPE TABLE OF ty_vbak,
       fs_vbak              TYPE ty_vbak,
       it_vbak2             TYPE TABLE OF ty_vbak,
       it_vbak_del          TYPE TABLE OF ty_vbak,
       it_978               TYPE STANDARD TABLE OF a978 WITH HEADER LINE,
       it_a305              TYPE STANDARD TABLE OF a305 WITH HEADER LINE,
       wa_konp              TYPE konp,
       it_konp              TYPE STANDARD TABLE OF konp WITH HEADER LINE,
       w_pos_imb_charg      TYPE yyoij_pos_cumimb,
       w_neg_imb_charg      TYPE yyoij_pos_cumimb,
       w_dds_imb_charg      TYPE yyoij_pos_cumimb,
       lv_pre_check_lines   TYPE sy-tabix,
       lv_post_check_lines TYPE sy-tabix,
       w_price_uom          TYPE dzieme,
       w_temp               TYPE p DECIMALS 3,
       w_temp1              TYPE p DECIMALS 3,
       lv_exit_processing   TYPE c,
       lv_futher_processing TYPE c,
       lv_clause_not_found TYPE c,
       lv_index             TYPE sytabix,

       it_vbak3             TYPE TABLE OF ty_vbak,
       it_vbak4             TYPE TABLE OF ty_vbak,
       wa_vbak3             TYPE ty_vbak,
       wa_vbak4             TYPE ty_vbak,
**SOC By Gaurav(TCS) & A.Mondal(GAIL) On 01/08/2018 TO accomulate rates
       it_display3          TYPE TABLE OF ty_display,
       wa_display3          TYPE ty_display,
**EOC By Gaurav(TCS) & A.Mondal(GAIL) On 01/08/2018 TO accomulate rates
       lr_veda              TYPE TABLE OF ty_r_veda,    "Added by XSAP_A B on 11th Sept,2024 - Imbalance Master Shifting
       wa_r_veda            TYPE ty_r_veda.             "Added by XSAP_A B on 11th Sept,2024 - Imbalance Master Shifting
DATA : lv_salesdocument    TYPE bapivbeln-vbeln,
       ls_order_header_inx TYPE bapisdh1x,
       lt_return           TYPE STANDARD TABLE OF bapiret2,
       ls_yrg_cumm_imb     TYPE yrg_cumm_imb,
       lv_type(10)         TYPE c.

DATA l_message TYPE char100.
DATA l_date1 TYPE datum.

DATA:lv_log TYPE TABLE OF yrga_email_log,
     wv_log TYPE yrga_email_log,
     subrc TYPE sy-subrc.

DATA l_daily     TYPE yyoij_pos_cumimb.
DATA l_cumm_bal TYPE yyoij_pos_cumimb.       "Added by XSAP_AB on 27th Ma y,2024
DATA l_daily1     TYPE yyoij_pos_cumimb.
DATA l_daily2     TYPE yyoij_pos_cumimb.
DATA l_mdq_quan TYPE msego2-adqnt.
DATA l_refvalue TYPE yyoij_pos_cumimb.
DATA l_adq       TYPE oib_adqnt.
DATA l_menge     TYPE oij_menge.
DATA l_wt TYPE zreewr. "oij_menge.
DATA l_wt1 TYPE zreewr. "oij_menge.
DATA : l_date     TYPE d,
       lv_prev_dt TYPE datum.

DATA: i_f4vbak TYPE TABLE OF ty_f4vbak.

DATA wa_order_header_inx TYPE bapisdh1x. "order_header_inx.
DATA wa_pricing          TYPE bapisdls.
DATA return              TYPE TABLE OF bapiret2.
DATA: w_master_contract     TYPE vbeln,
      w_group_imbalance(1).

DATA: it_sortcat TYPE slis_t_sortinfo_alv,
      w_sortcat TYPE slis_sortinfo_alv.
DATA: lv_15 TYPE c,
      lv_11 TYPE c,
      lv_10 TYPE c,
      lv_12 TYPE c.

DATA : process_flag.
FIELD-SYMBOLS : <fs_display1> TYPE ty_display1.

* SOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000007034

DATA: t_att_content_hex TYPE solix_tab.

* EOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000007034

*                                                                       *
*Selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
    SELECTION-SCREEN SKIP.

  SELECT-OPTIONS s_date FOR oijnomi-idate OBLIGATORY NO-EXTENSION.
  PARAMETERS : p_kunnr TYPE vbak-kunnr OBLIGATORY,   "kunag,
               p_knki TYPE knkli,
               s_vbeln TYPE vbak-vbeln NO-DISPLAY,
               overrun RADIOBUTTON GROUP g1 USER-COMMAND uk DEFAULT 'X',
               r_d2    RADIOBUTTON GROUP g1,
               r_d1    RADIOBUTTON GROUP g1,
               r_d5    RADIOBUTTON GROUP g1,
               r_dds   RADIOBUTTON GROUP g1,
               r_d3    RADIOBUTTON GROUP g1,
               rd_wt   RADIOBUTTON GROUP g1,
               r_d4    TYPE flag NO-DISPLAY,
*               *       BOC 4000000613 : TECH- VIKRAM BAJAJ FUNC - PRAT IBHA DT 12.09.2019
               r_d6    RADIOBUTTON GROUP g1,
*               *       EOC 4000000613 : TECH- VIKRAM BAJAJ FUNC - PRAT IBHA DT 12.09.2019
*               *       BOC 4000001479 : TECH- VIKRAM BAJAJ FUNC - PRAT IBHA DT 31.01.2020
               r_d7    RADIOBUTTON GROUP g1,
*               *       EOC 4000001479 : TECH- VIKRAM BAJAJ FUNC - PRAT IBHA DT 31.01.2020
*SOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala nce TR:DVRK9A1KR0
               p_post AS CHECKBOX MODIF ID a1.
*EOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala nce TR:DVRK9A1KR0
SELECTION-SCREEN END OF BLOCK b1.
*                                               End of Selection Screen

INITIALIZATION.
*SOC BY Gaurav/Pratibha ON 16.02.2026 Auto posting of fortnightly imbala nce TR:DVRK9A1KR0

  IF sy-tcode = 'YRGR102' OR sy-tcode = 'YRGR104' OR sy-batch = abap_tr ue.
*EOC BY Gaurav/Pratibha ON 16.02.2026 Auto posting of fortnightly imbala nce TR:DVRK9A1KR0
* IF sy-tcode = 'YRGR102' OR sy-tcode = 'YRGR104' .      """**     > add  YRGR104   SOC CHARM ID :4000008852   TECHICAL : RAVINDER SINGH FUNCTIO NAL : SHREYOSHI DT:09.10.2024
    CLEAR: overrun.

    ENDIF.

    IMPORT r_date = s_date
*           p_kunnr = p_kunnr
           p_knki = p_knki
           r_d4 FROM MEMORY ID 'ID8'.



    IF r_d4 = 'X'.
      CLEAR r_d1 .
    ENDIF.



*                                                                      *
*At screen selection
AT SELECTION-SCREEN OUTPUT.
  IF overrun = 'X' OR r_d2 = 'X' OR r_d3 = 'X' OR r_dds = 'X'
****     BOC charm 613 .
   OR r_d6 = 'X' .
****     EOC charm 613 .
     .
     LOOP AT SCREEN.
       IF overrun = 'X'.
         IF screen-group1 = 'G1'.
* If you want to disable input field
           screen-input = 0.
* if you want to hide input field
           screen-active = 0.
           MODIFY SCREEN.
         ENDIF.
         CLEAR s_vbeln.
*refresh s_vbeln.

       ENDIF.
       IF r_d3 = 'X'
         OR r_d6 = 'X' .
         .
         IF screen-name = 'P_KNKI' OR screen-name = '%_P_KNKI_%_APP_%-TEX T'.
           screen-invisible = '1'.
           screen-input = '0'.
           MODIFY SCREEN.

           SELECT vbeln guebg gueen FROM vbak       " f4 help for Customer
                              INTO TABLE i_f4vbak
                              WHERE kunnr = p_kunnr
                              AND auart = 'ZGK'.
         ENDIF.
       ELSE.
         IF screen-name = 'P_KNKI' OR screen-name = '%_P_KNKI_%_APP_%-TEX T'
            OR screen-name = 'S_VBELN' OR screen-name = '%_S_VBELN_%_APP_ %-TEXT'.
          CLEAR s_vbeln.
          screen-invisible = '1'.
          screen-input = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ELSEIF r_d1 = 'X' OR r_d5 = 'X' OR r_dds = 'X' OR rd_wt EQ 'X' OR r_d 7 = 'X'." "r_d7 = 'X'"    4000001479 : TECH- VIKRAM BAJAJ FUNC - PRATIB HA DT 31.01.2020
    LOOP AT SCREEN.
      IF overrun = 'X'.
        IF screen-group1 = 'G1'.
* If you want to disable input field
          screen-input = 0.
* if you want to hide input field
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

       ENDIF.
       IF screen-name = 'P_KNKI' OR screen-name = '%_P_KNKI_%_APP_%-TEXT'.
        screen-invisible = '1'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    SELECT vbeln guebg gueen FROM vbak      " f4 help for Customer
                             INTO TABLE i_f4vbak
                             WHERE kunnr = p_kunnr
                             AND auart = 'ZGK'.

   ENDIF.
*SOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala nce TR:DVRK9A1KR0
   LOOP AT SCREEN.
     IF screen-name EQ 'P_POST'.
       screen-invisible = 1.
       screen-active = 0.
*       P_POST = 'X'.
       MODIFY SCREEN.
     ENDIF.
   ENDLOOP.
*EOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala nce TR:DVRK9A1KR0
AT SELECTION-SCREEN.
   IF overrun = 'X'. " or r_d2 = 'X'.
*     CLEAR: overrun.
   ENDIF.
* IF s_vbeln IS INITIAL. ""changed on date 1.07.2019 by aru and A. mon dal, S_vbeln storing old session value.
   SELECT * INTO CORRESPONDING FIELDS OF TABLE it_vbak
             FROM vbak
             WHERE kunnr = p_kunnr
             AND ( auart = 'ZGT1' ).
   it_vbak_temp[] = it_vbak[].
   "Start of insert by XSAP_AB on 11th Sept,2024 - Imbalance Master Shift ing
   IF it_vbak[] IS NOT INITIAL.
     SELECT vbeln
       FROM veda
       INTO TABLE @DATA(it_veda_1)
       FOR ALL ENTRIES IN @it_vbak
       WHERE vbeln EQ @it_vbak-vbeln
       AND    vbegdat LE @s_date-high
       AND    venddat GE @s_date-low.
     IF sy-subrc IS INITIAL.
       LOOP AT it_veda_1 INTO DATA(wa_veda).
          wa_r_veda-sign = 'I'.
          wa_r_veda-option = 'EQ'.
          wa_r_veda-low = wa_veda-vbeln.
          APPEND wa_r_veda TO lr_veda.
          CLEAR wa_r_veda.
       ENDLOOP.
       DELETE it_vbak WHERE vbeln NOT IN lr_veda.
     ENDIF.
   ENDIF.
   "End of insert by XSAP_AB on 11th Sept,2024
   SELECT * APPENDING CORRESPONDING FIELDS OF TABLE it_vbak
*             FROM vbak FOR ALL ENTRIES IN it_vbak_temp
             FROM vbak FOR ALL ENTRIES IN it_vbak   """""" it_vbak add SOC   CHARM ID :4000008860    TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSH I  DT:09.10.2024
           WHERE
           auart = 'ZGK'
           AND vbeln = it_vbak-vbeln_grp     """ change it_vbak-temp into it_vbak
           AND abrvw = 'Z02'.               "Added by XSAP_AB on 7th Oct,2 024
  REFRESH it_vbak_temp.

  READ TABLE it_vbak INTO wa_vbak WITH KEY auart = 'ZGK'.
  IF sy-subrc EQ 0.
    s_vbeln = wa_vbak-vbeln.
  ENDIF.
  REFRESH: it_vbak, it_vbak_temp.
  CLEAR wa_vbak.
* ENDIF.

*                                                                            *
*At screen selection
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_vbeln.
* PERFORM f_f4help1.

*&                                                                           *
*&      Form F_F4HELP1
*&                                                                           *
*       text
*                                                                            *
*    > p1        text
* <     p2       text
*                                                                            *
FORM f_f4help1 .
*                                AND spras = 'E'.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
*     DDIC_STRUCTURE         = ' '
      retfield    = 'VBELN'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'S_VBELN'
      value_org   = 'S'
    TABLES
      value_tab   = i_f4vbak
*   EXCEPTIONS
*     PARAMETER_ERROR        = 1
*     NO_VALUES_FOUND        = 2
    .

ENDFORM.                                                       " F_F4HELP1
*                                                                            *
*Start-of-selection
START-OF-SELECTION.

    IF r_d7 = 'X' .
      CALL TRANSACTION 'YRG031N' .
    ELSE .

      REFRESH : it_display,it_display1,it_vbak.
*    IF overrun = 'X' OR r_d2 = 'X'.
*      CLEAR: s_vbeln.
*    ENDIF.

    IF s_date-low+6(2) = '01' OR s_date-low+6(2) = '16' .
    ELSE.
      MESSAGE 'Start date need to be monthly quater date' TYPE 'S'.
      EXIT.
    ENDIF.

    CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
      EXPORTING
        iv_date           = s_date-low
      IMPORTING
*       EV_MONTH_BEGIN_DATE       = EV_MONTH_BEGIN_DATE
        ev_month_end_date = l_date.
    .
    IF s_date-high+6(2) = '15' OR s_date-high+6(2) = l_date+6(2) .
    ELSE.
      MESSAGE 'End date need to be monthly quater date' TYPE 'S'.
      EXIT.
    ENDIF.

    IF overrun = 'X' OR r_d2 = 'X'.
      CLEAR: s_vbeln.
    ENDIF.
    IF rd_wt IS INITIAL.
      IF p_kunnr IS NOT INITIAL AND s_vbeln IS NOT INITIAL.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE it_vbak
                 FROM vbak
                 WHERE kunnr = p_kunnr
                 AND auart = 'ZGT1'.
        "AND vbeln_grp = s_vbeln.
      ELSEIF p_kunnr IS NOT INITIAL AND s_vbeln IS INITIAL.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE it_vbak
                 FROM vbak
                 WHERE kunnr = p_kunnr
                 AND ( auart = 'ZGT1' ).
      ELSEIF p_kunnr IS INITIAL AND s_vbeln IS NOT INITIAL.
        SELECT * INTO CORRESPONDING FIELDS OF TABLE it_vbak
                 FROM vbak
                 WHERE auart = 'ZGT1'.
        "AND vbeln_grp = s_vbeln.

       ELSEIF p_kunnr IS INITIAL AND s_vbeln IS INITIAL.
         SELECT * INTO CORRESPONDING FIELDS OF TABLE it_vbak
                  FROM vbak
                  WHERE auart = 'ZGT1'.
       ENDIF.

      IF it_vbak[] IS NOT INITIAL.
        REFRESH : it_vbak1,it_vbak2.
        IF overrun = space.
**SOC By Gaurav(TCS) & Mandal(GAIL) ON 31/07/2018 TO get data correspond  to master contract no.
          SELECT * FROM veda INTO CORRESPONDING FIELDS OF TABLE it_veda
            FOR ALL ENTRIES IN it_vbak
            WHERE vbeln = it_vbak-vbeln_grp
            AND vposn = '00000'
            AND vbegdat LE s_date-low
            AND venddat GE s_date-high.

           IF it_veda[] IS NOT INITIAL.
             SELECT * FROM vbak
                      INTO CORRESPONDING FIELDS OF TABLE it_vbak3
                      FOR ALL ENTRIES IN it_veda
                      WHERE vbeln = it_veda-vbeln.
*          ELSE.
*            SELECT * FROM VBAK
*                    INTO CORRESPONDING FIELDS OF TABLE IT_VBAK3
*                    FOR ALL ENTRIES IN IT_VBAK
*                    WHERE VBELN = IT_VBAK-VBELN_GRP.
           ENDIF.

*           SELECT * FROM VEDA INTO CORRESPONDING FIELDS OF TABLE IT_VEDA
*             FOR ALL ENTRIES IN IT_VBAK
*             WHERE VBELN = IT_VBAK-VBELN_GRP
*             AND VPOSN = '00000'
*             AND VBEGDAT LE S_DATE-LOW
*             AND VENDDAT GE S_DATE-HIGH.

*           IF IT_VEDA[] IS NOT INITIAL.
*             SORT IT_VEDA BY VBELN.
*             LOOP AT IT_VEDA INTO WA_VEDA.
*               READ TABLE IT_VBAK3 INTO WA_VBAK3 WITH KEY VBELN = WA_VED A-VBELN.
*               IF SY-SUBRC EQ 0.
*                 DELETE IT_VBAK3 FROM WA.
*               ENDIF.
*             ENDLOOP.

*           ENDIF.
           IF r_d2 IS INITIAL.
             LOOP AT it_vbak3 INTO wa_vbak3.
               LOOP AT it_vbak INTO wa_vbak WHERE vbeln_grp = wa_vbak3-vb eln.
                  DATA(lv_tabix) = sy-tabix.
                  IF wa_vbak3-kunnr <> wa_vbak-kunnr.
                    DELETE it_vbak INDEX lv_tabix.
                  ELSE.
                    MOVE-CORRESPONDING wa_vbak3 TO wa_vbak.
                    APPEND wa_vbak TO it_vbak.
                  ENDIF.
                ENDLOOP.
             ENDLOOP.
           ENDIF.
           "SOC Commented by Amit Shukla / Shreyosi Ma'am on 05/03/24 Cha rm No-4000007823
*           READ TABLE it_vbak WITH KEY bstnk = space TRANSPORTING NO FIE LDS.
           READ TABLE it_vbak WITH KEY auart = 'ZGK' TRANSPORTING NO FIEL DS.
           "EOC Amit Shukla / Shreyosi Ma'am on 05/03/24 Charm No-4000007 823
           IF sy-subrc = 0.
             SELECT * FROM vbak
                        APPENDING CORRESPONDING FIELDS OF TABLE it_vbak
                        FOR ALL ENTRIES IN it_vbak
                        WHERE vbeln_grp = it_vbak-vbeln_grp
*                     AND kunnr      <> it_vbak-kunnr
                          AND vbeln_grp <> space.
           ENDIF.
         ENDIF.
**EOC By Gaurav(TCS) & Mandal(GAIL) ON 31/07/2018 TO get data correspond   to master contract no.
        MOVE it_vbak[] TO it_vbak2[].
        SORT it_vbak2 BY vbeln_grp.
*      DELETE it_vbak2 WHERE vbeln_grp IS INITIAL.

        IF s_vbeln IS NOT INITIAL.
          IF p_kunnr IS NOT INITIAL.
            SELECT * INTO CORRESPONDING FIELDS OF TABLE it_vbak1
                           FROM vbak
                           WHERE " vbeln_grp = s_vbeln "it_vbak2-vbeln_g rp
                            auart = 'ZGT1'
                           AND kunnr = p_kunnr.
          ELSE.
            SELECT * INTO CORRESPONDING FIELDS OF TABLE it_vbak1
                       FROM vbak
                       WHERE vbeln_grp = s_vbeln "it_vbak2-vbeln_grp
                       AND auart = 'ZGT1'.
          ENDIF.

           IF sy-subrc = 0.
**SOC By Gaurav(TCS) & A.Mandal(GAIL) ON 31/07/2018 TO get data correspo nd to master contract no.
             IF it_vbak1 IS NOT INITIAL AND overrun = space..
               SELECT * FROM vbak
                         INTO CORRESPONDING FIELDS OF TABLE it_vbak4
                         FOR ALL ENTRIES IN it_vbak1
                         WHERE vbeln = it_vbak1-vbeln_grp.
*                         AND kunnr     = it_vbak1-kunnr
*                         AND vbeln_grp <> space.
               IF r_d2 IS INITIAL.
                 LOOP AT it_vbak4 INTO wa_vbak4.
                   LOOP AT it_vbak1 INTO wa_vbak1 WHERE vbeln_grp = wa_vb ak4-vbeln.
                      lv_tabix = sy-tabix.
                      IF wa_vbak4-kunnr <> wa_vbak1-kunnr.
                        DELETE it_vbak1 INDEX lv_tabix.
                      ELSE.
                        MOVE-CORRESPONDING wa_vbak4 TO wa_vbak1.
                        APPEND wa_vbak1 TO it_vbak1.
                      ENDIF.
                   ENDLOOP.
                 ENDLOOP.
               ENDIF.

               "SOC Commented by Amit Shukla / Shreyosi Ma'am on 05/03/24   Charm No-4000007823
*           READ TABLE it_vbak WITH KEY bstnk = space TRANSPORTING NO FIE LDS.
               READ TABLE it_vbak WITH KEY auart = 'ZGK' TRANSPORTING NO FIELDS.
               "EOC Amit Shukla / Shreyosi Ma'am on 05/03/24 Charm No-400 0007823
               IF sy-subrc = 0.
                 SELECT * FROM vbak
                          APPENDING CORRESPONDING FIELDS OF TABLE it_vbak 1
                          FOR ALL ENTRIES IN it_vbak1
                          WHERE vbeln_grp = it_vbak1-vbeln_grp
*                          AND kunnr    <> it_vbak1-kunnr
                            AND vbeln_grp <> space.
               ENDIF.
            ENDIF.
**EOC By Gaurav(TCS) & A.Mandal(GAIL) ON 31/07/2018 TO get data correspo nd to master contract no.
            LOOP AT it_vbak1 INTO wa_vbak.

               APPEND wa_vbak TO it_vbak.
            ENDLOOP.
            SORT it_vbak BY vbeln.
            DELETE ADJACENT DUPLICATES FROM it_vbak COMPARING vbeln.
          ENDIF.
        ENDIF.
        REFRESH : it_vbak1,it_vbak2.
        CLEAR l_menge.


*   BOA by XSAP_AB2
        CLEAR lv_pre_check_lines.
        DESCRIBE TABLE it_vbak LINES lv_pre_check_lines.
*   EOA by XSAP_AB2

         LOOP AT it_vbak INTO wa_vbak.
*    BOA by XSAP_AB2-AV
           lv_index = sy-tabix.
*BOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal
           IF ( overrun IS INITIAL AND r_d1 IS INITIAL ).
             CLEAR lv_type.
             IF ( r_d2 EQ 'X').
               lv_type = 'O'.
             ELSEIF ( r_d5 EQ 'X' OR r_d3 EQ 'X'
*****             boc charm 613
               OR r_d6 = 'X'
*****              eoc charm 613
               )." OR r_dds = 'X').
               lv_type = 'I'.
             ELSEIF ( r_d3 EQ 'X' OR r_dds = 'X'
**             boc charm 613
               OR r_d6 = 'X'
**             eoc charm 613
               ).
               lv_type = 'D'.
             ENDIF.
*EOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
             CLEAR lv_exit_processing.
*    Validate Imbalance Caluses
             IF NOT sy-batch EQ 'X'         "Added by XSAP_AB on 20th Sept,2024 - Supressing popup
                OR NOT sy-tcode EQ 'YRGR102'. "Added by XSAP_AB on 04th Oct,2024 - Supressing popup for YRGR102
               CALL METHOD ycl_gms_report_validations=>validate_imbalance
                  EXPORTING
                    i_vbeln           = wa_vbak-vbeln         " Sales and Distribution Document Number
                    i_valid_from      = s_date-low            " Valid From   Date
                    i_valid_to        = s_date-high           " Valid To D ate
                    i_type            = lv_type               " O = OVERRU N I = IMBALANCE
                  IMPORTING
                    e_exit_processing = lv_exit_processing    " Exit furth er processing in calling report?
                    e_not_found       = lv_clause_not_found.   " Clause not  found
              IF lv_exit_processing = 'X'.
* SOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000007034
                 FREE MEMORY ID 'MEM_LIN'.
* EOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000007034
                 LEAVE LIST-PROCESSING.                      " Exit Furth er Processing
              ELSEIF lv_clause_not_found = 'X'.
                 DELETE it_vbak INDEX lv_index.              " Delete cur rent record
                 CONTINUE.
              ENDIF.
            ENDIF.          "Added by XSAP_AB on 20th Sept,2024 - Supress ing popup
          ENDIF.

*     EOA by XSAP_AB2-AV

          LOOP AT it_vbak INTO wa_vbak1 WHERE vbeln_grp = wa_vbak-vbeln_ grp AND vbeln_grp IS NOT INITIAL.
            APPEND wa_vbak1 TO it_vbak1.
          ENDLOOP.

*        SOC comment the kunnr = knkli check by sachin on 18.04.2012
*        IF wa_vbak-kunnr = wa_vbak-knkli.
            READ TABLE it_vbak1 INTO wa_vbak1 WITH KEY vbeln = wa_vbak-vbe ln.
            IF sy-subrc <> 0.
              APPEND wa_vbak TO it_vbak2.
            ENDIF.
*        ENDIF.
*        EOC comment the kunnr = knkli check   by sachin on 18.04.2012
          ENDLOOP.

*   BOA by XSAP_AB2-AV . If Clause is not maintained in all the contract s. Then, throw an error message to user
        IF it_vbak[] IS INITIAL AND lv_pre_check_lines IS NOT INITIAL AN D overrun IS INITIAL AND r_d1 IS INITIAL.

          IF r_d2 EQ 'X'.
            MESSAGE e028(ygms).
          ELSEIF ( r_d5 EQ 'X' OR r_d3 EQ 'X'
***          boc charm 613
            OR r_d6 = 'X'
***          eoc charm 613
            ).
            IF sy-tcode NE 'YRGR102'.
               MESSAGE e025(ygms)."""**    > add YRGR102  SOC CHARM ID :4000008814   TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSHI DT:12.1 0.2024
            ENDIF.
          ELSEIF r_dds = 'X' .
            MESSAGE e032(ygms).
          ENDIF.

          ENDIF.
*     EOC by XSAP_AB2

          SORT it_vbak1 BY vbeln.
          DELETE ADJACENT DUPLICATES FROM it_vbak1 COMPARING vbeln.
        "SOC RITESH SINGH FUNCTIONAL : PRATIBHA DANGWAL DT:23.10.2020 o n Charm 4000002912
        "ADD Additional Validation For Contract Valid Period.
         DATA:gv_vbdat TYPE vbdat_veda.
         DATA:gv_vndat TYPE vndat_veda.
         LOOP AT it_vbak INTO DATA(fs_vbak1) WHERE auart = 'ZGT1'.
           CALL FUNCTION 'YRV_CONTRACT_DETAILS'
             EXPORTING
                vbeln   = fs_vbak1-vbeln
*               GAS_DAY_FROM       =
*               GAS_DAY_TO         =
             IMPORTING
*               AUART   =
                vbegdat = gv_vbdat
                venddat = gv_vndat
*               DOM_IND =
*               DCQ_IND =
*               MSG     =
*            TABLES
*               DCQ_DETAILS        =
             .
           IF gv_vbdat LE s_date-high AND gv_vndat GE s_date-low.
             MOVE-CORRESPONDING fs_vbak1 TO fs_vbak.
             APPEND fs_vbak TO it_vbak_temp.
           ELSE.
             DELETE it_vbak WHERE vbeln    = fs_vbak1-vbeln.
           ENDIF.
*
         ENDLOOP.
        "EOC RITESH SINGH FUNCTIONAL : PRATIBHA DANGWAL DT:23.10.2020 o n Charm 4000002912
*         it_vbak_temp[] = it_vbak[].
         DELETE it_vbak_temp WHERE auart EQ 'ZGK'.
**SOC BY YASHU/SHREYOSI CHARM 4000004302: YS:GMS_YRG011N_Non Gail P/L Lo cation**
         IF it_vbak_temp[] IS INITIAL .
           IF sy-tcode EQ 'YRGR102'. "CHARM NO 4000000783
             FREE MEMORY ID 'PENDING_IMB_POST'.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
             EXPORT it_display1[] TO MEMORY ID 'PENDING_IMB_POST'. "#EC C I_FLDEXT_OK[2215424]
** <- End changes by of Aditi on 19/11/24 for ATC
           ELSE.
             MESSAGE 'LINKED MASTER CONTRACTS BELONG TO ANOTHER CUSTOMER, USE TCODE YRG012N TO SEE IMBALANCE FOR THIS CUSTOMER' TYPE 'E' DISPLAY L IKE 'E'.
             LEAVE TO SCREEN 0.
           ENDIF.
         ENDIF.
**EOC BY YASHU/SHREYOSI CHARM 4000004302: YS:GMS_YRG011N_Non Gail P/L Lo cation**
         IF it_vbak_temp[] IS NOT INITIAL.      "Added by XSAP_ab on 15th oct,2024
           SELECT * INTO CORRESPONDING FIELDS OF TABLE it_oijnomi
           FROM oijnomi
           FOR ALL ENTRIES IN it_vbak_temp
           WHERE
             docnr = it_vbak_temp-vbeln
*           OR
*           DOCNR = IT_VBAK_TEMP-BSTNK ) "COMMENTED BY ANKUR ON 18.10.202 3
          AND
          idate IN s_date
          AND
          delind <> 'X'.
        ENDIF.                "Added by XSAP_ab on 15th oct,2024

        "BOC "BOC CHARM 4000007227 FUNCTIONAL PRATIBHA DANGWAL TECHNICA L ANKUR PRASHAR ON 01.02.2024" GTA WITHOUT PO
        IF it_oijnomi[] IS NOT INITIAL.
          "Start of insert by XSAP_AB on 27th May,2024
          "Start of insert by XSAP_AB on 11th Sept,2024 - Imbalance Mast er Shifting
          DATA(it_vbak_zgk) = it_vbak4[].
          DELETE it_vbak_zgk WHERE auart NE 'ZGK'.
          "End of insert by XSAP_AB on 11th sept
          SELECT *
             FROM yrva_gta_imb_sft
             INTO TABLE @DATA(it_gta_imb)
             FOR ALL ENTRIES IN @it_oijnomi
             WHERE contract_to EQ @it_oijnomi-docnr.
          "Start of insert by XSAP_AB on 11th Sept,2024 - Imbalance Mast er Shifting
          IF it_vbak_zgk[] IS NOT INITIAL.
             SELECT *
               FROM yrva_gta_imb_sft
               APPENDING CORRESPONDING FIELDS OF TABLE it_gta_imb
               FOR ALL ENTRIES IN it_vbak_zgk
               WHERE contract_to EQ it_vbak_zgk-vbeln.
             IF sy-subrc IS INITIAL.
               CLEAR it_vbak_zgk[].
             ENDIF.
             "End of insert by XSAP_AB on 11th Sept,2024
          ENDIF.
*           IF sy-subrc IS INITIAL. **      > COC CHARM ID :4000008860   T ECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSHI DT:09.10.2024
          SORT it_gta_imb BY contract_to.
          IF it_gta_imb[] IS NOT INITIAL. "**       > SOC CHARM ID :400000 8860   TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSHI DT:09.10.2024

            SELECT *
              FROM yrg_cumm_imb
              INTO TABLE @DATA(it_cumm_imb_temp)
              FOR ALL ENTRIES IN @it_gta_imb
              WHERE yy_contract EQ @it_gta_imb-contract_from
              AND    endda LT @s_date-low.
            IF sy-subrc IS INITIAL.
              SORT it_cumm_imb_temp BY timestamp DESCENDING.
            ENDIF.
          ENDIF.
          "End of insert by XSAP_AB on 27th May,2024
          SELECT * FROM oijnomi INTO CORRESPONDING FIELDS OF TABLE it_oi j
                   FOR ALL ENTRIES IN it_oijnomi
                   WHERE nomtk = it_oijnomi-nomtk
                   AND   sityp = 'ZO'
                   AND   docind = 'K'
                   AND delind NE 'X'.
        ENDIF.

        IF it_oij IS NOT INITIAL.
          APPEND LINES OF it_oij TO it_oijnomi.
        ENDIF.

        "EOC CHARM 4000007227 FUNCTIONAL PRATIBHA DANGWAL TECHNICAL ANKU R PRASHAR ON 01.02.2024" GTA WITHOUT PO

        REFRESH it_vbak_temp[].
        IF sy-subrc = 0.
          SORT it_oijnomi BY idate.
          IF r_dds = 'X'.
            SELECT * INTO TABLE it_yrg_cumm_imb
             FROM yrg_cumm_dds
             FOR ALL ENTRIES IN it_vbak
             WHERE
             yy_contract = it_vbak-vbeln
             AND
             endda < s_date-low.


          ENDIF.
          SELECT * APPENDING TABLE it_yrg_cumm_imb
            FROM yrg_cumm_imb
            FOR ALL ENTRIES IN it_vbak
            WHERE
            yy_contract = it_vbak-vbeln
            AND
            endda < s_date-low.
          IF sy-subrc = 0.
            DATA: w_line TYPE i,
                  w_line1 TYPE i.
            SORT it_yrg_cumm_imb BY timestamp ASCENDING.     "    ldatum end da
**        move it_yrg_cumm_imb[] to it_yrg_cumm_imb_temp[].
            DESCRIBE TABLE it_yrg_cumm_imb LINES w_line.
            w_line1 = w_line.
            DO w_line TIMES.
              READ TABLE it_yrg_cumm_imb INTO wa_yrg_cumm_imb1 INDEX w_l ine1.
              IF sy-subrc = 0.
                APPEND wa_yrg_cumm_imb1 TO it_yrg_cumm_imb_temp.
              ENDIF.
              w_line1 = w_line1 - 1.
            ENDDO.
*          enddo.
*        read table it_yrg_cumm_imb into wa__yrg_cumm_imb1 index w_line.
*        loop at it_yrg_cumm_imb into wa_yrg_cumm_imb.
*
*         endloop.
             REFRESH it_yrg_cumm_imb.
             MOVE it_yrg_cumm_imb_temp[] TO it_yrg_cumm_imb[].
*         DELETE ADJACENT DUPLICATES FROM it_yrg_cumm_imb COMPARING knkli   kunnr yy_contract.
             DELETE ADJACENT DUPLICATES FROM it_yrg_cumm_imb COMPARING ku nnr yy_contract.
            ENDIF.    .

           IF r_dds = 'X'.
             SELECT * INTO TABLE it_yrg_cumm_imb1
                        FROM yrg_cumm_dds
                        FOR ALL ENTRIES IN it_vbak
                        WHERE
                        yy_contract = it_vbak-vbeln
                       AND
                   begda = s_date-low
                   AND
                       endda = s_date-high.
          ELSE.
            SELECT * INTO TABLE it_yrg_cumm_imb1
                  FROM yrg_cumm_imb
                  FOR ALL ENTRIES IN it_vbak
                  WHERE
                  yy_contract = it_vbak-vbeln
                  AND
              begda = s_date-low
              AND
                  endda = s_date-high.
          ENDIF.
*      IF sy-subrc = 0.
          SORT it_yrg_cumm_imb1 BY timestamp ldatum endda DESCENDING.
*        DELETE ADJACENT DUPLICATES FROM it_yrg_cumm_imb1 COMPARING knkl  i kunnr yy_contract.
          DELETE ADJACENT DUPLICATES FROM it_yrg_cumm_imb1 COMPARING kun  nr yy_contract.
          IF it_yrg_cumm_imb1[] IS NOT INITIAL.
            SELECT * INTO CORRESPONDING FIELDS OF TABLE it_vbak_del
              FROM vbak
              FOR ALL ENTRIES IN it_yrg_cumm_imb1
              WHERE
             vbeln = it_yrg_cumm_imb1-vbeln.

            DATA l_tabix TYPE sy-tabix.
            LOOP AT it_yrg_cumm_imb1 INTO wa_yrg_cumm_imb.
              l_tabix = sy-tabix.
              READ TABLE it_vbak_del INTO wa_vbak WITH KEY vbeln = wa_yr  g_cumm_imb-vbeln.
              IF sy-subrc <> 0.
                 CLEAR wa_yrg_cumm_imb-vbeln.
                 MODIFY it_yrg_cumm_imb1 FROM wa_yrg_cumm_imb INDEX l_tab  ix.
              ENDIF.
            ENDLOOP.
          ENDIF.
*      ENDIF.

          SELECT * FROM yrg_chg_ovrrun INTO TABLE it_yrg_chg_ovrrun    FOR ALL ENTRIES IN it_vbak
            WHERE vbeln = it_vbak-vbeln
            AND begda = s_date-low AND endda EQ s_date-high.

          SELECT * INTO TABLE it_yrvt_contract
            FROM yrvt_contract
            FOR ALL ENTRIES IN it_oijnomi
            WHERE
            vbeln = it_oijnomi-docnr
            AND
            clause IN ('10','11','12','15','16').          " Clause 16 Ad  ded by A.Mondal / Nitin Dhamija on 01.08.2019
          IF sy-subrc = 0.
*BOC CHARM ID :4000001828   TECHICAL : VIKRAM BAJAJ   FUNCTIONAL : PRATIB  HA DANGWAL DT:05.03.2020
            TYPES : BEGIN OF ty_yrvt_cont_dcq ,
                      mandt    TYPE mandt,
                      vbeln    TYPE vbeln_va,
                      ydate    TYPE ydats1,
                      sy_uname TYPEuname,
                      creon    TYPE erdat,
                      time     TYPEuzeit,
                      dcq_qty TYPE ydcq_qty,
                      max_qty TYPE yzpercent,
                      min_qty TYPE yzpercent,
                      flag(1) ,
                      timest   TYPE num14,
                    END OF ty_yrvt_cont_dcq .
*****          EOC CHARM ID : 4000001471 TECHICAL : VIKRAM BAJAJ    FUNCTI  ONAL : PRATIBHA DANGWAL   DT: 9.1.2019

              DATA : lt_yrvt_cont_dcq TYPE TABLE OF ty_yrvt_cont_dcq."YRVT  _CONT_DCQ .
              DATA : ls_yrvt_cont_dcq TYPE ty_yrvt_cont_dcq ."YRVT_CONT_DC  Q .
            SELECT * FROM yrvt_cont_dcq INTO CORRESPONDING FIELDS OF TAB  LE lt_yrvt_cont_dcq
              FOR ALL ENTRIES IN it_oijnomi WHERE vbeln = it_oijnomi-doc  nr AND ydate = it_oijnomi-idate.
            " deleted indicator = blank added 03.02.2020
*            and flag = ' '. " commented on 05.03.2020
            LOOP AT lt_yrvt_cont_dcq INTO ls_yrvt_cont_dcq .
              CONCATENATE ls_yrvt_cont_dcq-creon ls_yrvt_cont_dcq-time I  NTO ls_yrvt_cont_dcq-timest .
              MODIFY lt_yrvt_cont_dcq FROM ls_yrvt_cont_dcq TRANSPORTING  timest .
              CLEAR ls_yrvt_cont_dcq .
            ENDLOOP .
            SORT lt_yrvt_cont_dcq BY vbeln timest DESCENDING.
*EOC CHARM ID :4000001828   TECHICAL : VIKRAM BAJAJ FUNCTIONAL : PRATIB  HA DANGWAL DT:05.03.2020

*****          EOC CHARM ID : 4000001471 TECHICAL : VIKRAM BAJAJ    FUNCTI  ONAL : PRATIBHA DANGWAL   DT: 9.1.2019
            SORT it_yrvt_contract BY vbeln.
            IF r_dds = 'X'.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
*              CALL FUNCTION 'LAST_DAY_OF_MONTHS'
*                EXPORTING
*                  day_in            = s_date-low
*                IMPORTING
*                  last_day_of_month = l_date1
*                EXCEPTIONS
*                  day_in_no_date    = 1
*                  OTHERS            = 2.
              CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
                EXPORTING
                  day_in            = s_date-low
                IMPORTING
                  last_day_of_month = l_date1
                EXCEPTIONS
                  day_in_no_date    = 1
                  OTHERS            = 2.
** <- End changes by of Aditi on 19/11/24 for ATC
              IF sy-subrc <> 0.
* Implement suitable error handling here
              ENDIF.



*
*          LOOP AT it_yrvt_contract INTO wa_yrvt_contract
*            WHERE
*            value_from <= s_date-low
*            AND
*            value_to >= s_date-low.
*            CASE wa_yrvt_contract-cal_period.
*              WHEN '10'.
*                IF s_date-low+6(2) <> '01' OR s_date-high+6(2) <> '15'.
*                   CONCATENATE l_message wa_yrvt_contract-vbeln 'call pe  riod is 15 days' INTO l_message
*                   SEPARATED BY space.
*                   DELETE it_oijnomi WHERE docnr = wa_yrvt_contract-vbel  n.
*                ENDIF.
*
*              WHEN '1'.
*                IF s_date-low+6(2) <> '01' OR s_date-high <> l_date1.
*                   CONCATENATE l_message wa_yrvt_contract-vbeln 'call pe  riod is monthly' INTO l_message
*                   SEPARATED BY space.
*                   DELETE it_oijnomi WHERE docnr = wa_yrvt_contract-vbel  n.
*                ENDIF.
**         delete it_oijnomi where docnr = wa_yrvt_contract-vbeln.
*            ENDCASE.
*          ENDLOOP.

*          IF l_message IS NOT INITIAL.
*            MESSAGE l_message TYPE 'I'.
*          ENDIF.
            ENDIF.
          ENDIF .

************************************************
          IF it_oijnomi[] IS NOT INITIAL.
            SELECT * INTO CORRESPONDING FIELDS OF TABLE it_oij_el_cp_lay  t
              FROM oij_el_cp_layt
              FOR ALL ENTRIES IN it_oijnomi
              WHERE
              vbeln = it_oijnomi-docnr.
            IF sy-subrc = 0.
              SORT it_oij_el_cp_layt BY vbeln.
            ENDIF.

            SELECT * INTO TABLE it_yro_nom_param
              FROM yro_nom_param
              FOR ALL ENTRIES IN it_oijnomi
              WHERE
              nomtk = it_oijnomi-nomtk
              AND
              nomit = it_oijnomi-nomit.

            SELECT * INTO CORRESPONDING FIELDS OF TABLE it_yro_nom_param  1
              FROM yro_nom_param1
              FOR ALL ENTRIES IN it_oijnomi
              WHERE
              nomtk = it_oijnomi-nomtk
              AND
              nomit = it_oijnomi-nomit.
          ENDIF.

****************************************************
          SORT it_oijnomi BY idate.
*      SOC commented by sachin and atul saxena on 17.08.2012
*      LOOP AT it_oijnomi INTO wa_oijnomi.
*        wa_oijnomi-menge = wa_oijnomi-ga_allocated_qty.
*        MODIFY it_oijnomi FROM wa_oijnomi INDEX sy-tabix TRANSPORTING m  enge . "GA_ALLOCATED_QTY.
*      ENDLOOP.
*      EOC commented by sachin and atul saxena on 17.08.2012
**SOC By Gaurav(TCS) & A.Mondal(GAIL) On 01/08/2018 TO Sort IT_VBAK for  chargable or daily accumulate rates
          SORT it_vbak[] BY vbeln_grp DESCENDING.
          "Start of insert by XSAP_AB on 27th May,2024
          IF it_gta_imb[] IS NOT INITIAL.     "Added by XSAP_AB on 24th  Aug,2024
            SELECT * FROM veda
              INTO TABLE @DATA(it_parent_1)
              FOR ALL ENTRIES IN @it_gta_imb
              WHERE vbeln EQ @it_gta_imb-contract_from
                 AND vposn EQ '000000'.      "Added by XSAP_AB on 7th oc  t,2024
            IF sy-subrc IS INITIAL.
              SORT it_parent_1 BY venddat.
            ENDIF.
          ENDIF.
          "To determine the latest master Contract
          LOOP AT it_vbak INTO DATA(wa_vbak_1).
            READ TABLE it_oijnomi TRANSPORTING NO FIELDS WITH KEY docnr  = wa_vbak_1-vbeln.
            IF sy-subrc IS INITIAL.
              w_master_contract = wa_vbak_1-vbeln_grp.
              EXIT.
            ENDIF.
          ENDLOOP.

*           LOOP AT it_gta_imb INTO DATA(wa_gta_imb_temp).
*             READ TABLE it_oijnomi INTO wa_oijnomi WITH KEY docnr = wa_g  ta_imb_temp-contract_from.
*             IF sy-subrc IS INITIAL.
*               LOOP AT it_gta_imb ASSIGNING FIELD-SYMBOL(<lfs_gta_imb>)  WHERE contract_to EQ wa_oijnomi-docnr.
*                 READ TABLE it_parent TRANSPORTING NO FIELDS WITH KEY vb  eln = <lfs_gta_imb>-contract_to.
*                 IF sy-subrc IS NOT INITIAL.
*                   READ TABLE it_parent TRANSPORTING NO FIELDS WITH KEY  vbeln = <lfs_gta_imb>-contract_from.
*                   IF sy-subrc IS NOT INITIAL.
*                     wa_parent-vbeln = <lfs_gta_imb>-contract_from.
*                     APPEND wa_parent TO it_parent.
*                     wa_parent-vbeln = <lfs_gta_imb>-contract_to.
*                     APPEND wa_parent TO it_parent.
*                   ENDIF.
*                 ELSE.
*                   l_tabix = sy-tabix.
*                   wa_parent-vbeln = <lfs_gta_imb>-contract_from.
*                   READ TABLE it_oijnomi TRANSPORTING NO FIELDS WITH KEY  docnr = <lfs_gta_imb>-contract_from.
*                    IF sy-subrc IS INITIAL.
*                      READ TABLE it_parent TRANSPORTING NO FIELDS WITH KE  Y vbeln = <lfs_gta_imb>-contract_from.
*                      IF sy-subrc IS NOT INITIAL.
*                         INSERT wa_parent INTO it_parent INDEX l_tabix.
*                      ENDIF.
*                    ELSE.
*                      READ TABLE it_parent TRANSPORTING NO FIELDS WITH KE  Y vbeln = <lfs_gta_imb>-contract_from.
*                      IF sy-subrc IS NOT INITIAL.
*                         INSERT wa_parent INTO it_parent INDEX 1.
*                      ENDIF.
*                    ENDIF.
*                 ENDIF.
*               ENDLOOP.
*             ELSE.
*               wa_parent-vbeln = wa_gta_imb_temp-contract_from.
*               READ TABLE it_parent TRANSPORTING NO FIELDS WITH KEY vbel  n = <lfs_gta_imb>-contract_from.
*               IF sy-subrc IS NOT INITIAL.
*                 INSERT wa_parent INTO it_parent INDEX 1.
*               ENDIF.
*             ENDIF.
*           ENDLOOP.
*           CLEAR : l_tabix,wa_parent.
**           DESCRIBE TABLE it_parent LINES DATA(l_lines).
           l_tabix = 1.
           LOOP AT it_parent_1 ASSIGNING FIELD-SYMBOL(<lfs_parent>).
             READ TABLE it_vbak INTO wa_vbak WITH KEY vbeln = <lfs_parent  >-vbeln.
             IF sy-subrc IS INITIAL.
               wa_vbak1 = wa_vbak.
               DELETE it_vbak WHERE vbeln EQ wa_vbak-vbeln.
               INSERT wa_vbak1 INTO it_vbak INDEX l_tabix.
               l_tabix = l_tabix + 1.
             ENDIF.
           ENDLOOP.
           CLEAR: l_tabix,wa_vbak,wa_vbak1.
           "End of insert by XSAP_AB on 27th May,2024
*           SORT it_vbak[] BY vbeln_grp.
**EOC By Gaurav(TCS) & A.Mondal(GAIL) On 01/08/2018 TTO Sort IT_VBAK for  chargable or daily accumulate rates
           "SOC RITESH SINGH FUNCTIONAL : PRATIBHA DANGWAL DT:02.09.2020  on Charm 4000002463
           IF it_vbak[] IS NOT INITIAL.
             SELECT kunnr,
                     vbeln,
                     doc,
                     ddqty,
                     qnt_uom FROM yrv_ep_nm_del
                     INTO TABLE @DATA(gt_rv_del)
                     FOR ALL ENTRIES IN @it_vbak
                     WHERE vbeln = @it_vbak-vbeln
                      AND doc IN @s_date.
             IF sy-subrc EQ 0.
               SORT gt_rv_del BY kunnr vbeln.
             ENDIF.
           ENDIF.
           "EOC RITESH SINGH FUNCTIONAL : PRATIBHA DANGWAL DT:02.09.2020  on Charm 4000002463
           LOOP AT it_vbak INTO wa_vbak.

             READ TABLE it_yrg_cumm_imb INTO wa_yrg_cumm_imb WITH KEY yy_  contract = wa_vbak-vbeln ."or vbeln = wa_vbak-bstnk.
             IF sy-subrc = 0.
               l_daily = wa_yrg_cumm_imb-yy_oij_cumimb.
*         wa_display1-vbeln1 = wa_yrg_cumm_imb-vbeln.
               "Start of insert by XSAP_AB on 27th May,2024
               "If entry is not present in cumm_imb table, check if paren  t exists.
             ELSE.
               CLEAR: l_cumm_bal,wa_header.
               SORT it_disp_temp BY vbeln ASCENDING date DESCENDING.
               LOOP AT it_gta_imb INTO DATA(wa_gta_imb) WHERE contract_to  = wa_vbak-vbeln.
                 wa_header-vbeln = wa_vbak-vbeln.
                 wa_header-p_cont = wa_gta_imb-contract_from.
                 "check parent in final table
                 READ TABLE it_disp_temp INTO DATA(wa_disp_temp) WITH KEY  vbeln = wa_gta_imb-contract_from.
                 IF sy-subrc IS INITIAL.
                    l_cumm_bal = l_cumm_bal + wa_disp_temp-daily_cumm.
                    wa_header-imbal = wa_disp_temp-daily_cumm.
                 ELSE.     "if parent not exist in final table, fetch from  cumm_imb table
                    READ TABLE it_cumm_imb_temp INTO DATA(wa_cumm_imb_temp  ) WITH KEY yy_contract = wa_gta_imb-contract_from.
                    IF sy-subrc IS INITIAL.
                      l_cumm_bal = l_cumm_bal + wa_cumm_imb_temp-yy_oij_cu  mimb.
                      wa_header-imbal = wa_cumm_imb_temp-yy_oij_cumimb.
                    ENDIF.
                 ENDIF.
                 APPEND wa_header TO it_header.
                 CLEAR: wa_disp_temp,wa_cumm_imb_temp,wa_header.
               ENDLOOP.
               l_daily = l_cumm_bal.
               CLEAR wa_gta_imb.
               "End of insert by XSAP_AB on 27th May,2024
             ENDIF.

*BOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
            IF r_d2 EQ 'X' .
              READ TABLE it_yrg_chg_ovrrun INTO wa_yrg_chg_ovrrun WITH K  EY vbeln = wa_vbak-vbeln.
              IF sy-subrc = 0.
                wa_display1-vbeln1 = wa_yrg_chg_ovrrun-vbeln1.
              ENDIF.
*EOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
            ELSE.
              READ TABLE it_yrg_cumm_imb1 INTO wa_yrg_cumm_imb1 WITH KEY yy_contract = wa_vbak-vbeln.
              IF sy-subrc = 0.
                wa_display1-vbeln1 = wa_yrg_cumm_imb1-vbeln.
              ENDIF.
            ENDIF.

            CLEAR l_menge.
*READ TABLE IT_OIJNOMR INTO WA_OIJNOMR WITH KEY DOCNR = WA_VBAK-VBELN.
*IF SY-SUBRC = 0.
* READ TABLE IT_OIJNOMI INTO WA_OIJNOMI WITH KEY
            DATA: nomtk TYPE oijnomi-nomtk.
             CLEAR:nomtk.
             LOOP AT it_oijnomi INTO wa_oijnomi
               WHERE docnr = wa_vbak-vbeln .
*   or docnr = wa_vbak-bstnk.
               CLEAR wa_display.

              wa_display1-matnr = wa_oijnomi-s_matnr_i.
              wa_display1-vbeln = wa_vbak-vbeln.
*          wa_display1-date = wa_oijnomi-idate.
              wa_display1-ktext = wa_vbak-ktext.
              wa_display1-knkli = wa_vbak-knkli.
*             w_master_contract = wa_vbak-vbeln_grp.    "Commented by XS  AP_AB on 27th May 2024

               wa_display-vbeln_grp = wa_vbak-vbeln_grp.

              "BOC CHARM 4000007227 FUNCTIONAL PRATIBHA DANGWAL TECHNICA  L ANKUR PRASHAR ON 12.10.2023" GTA WITHOUT PO
              READ TABLE it_oijnomi INTO DATA(oj) WITH KEY docnr = wa_v  bak-vbeln idate = wa_oijnomi-idate.
              IF sy-subrc = 0.
                nomtk = oj-nomtk.
              ENDIF.
              CLEAR: oj.

              "READ TABLE it_oijnomi INTO wa_oijnomi1 WITH KEY docnr =  wa_vbak-bstnk idate = wa_oijnomi-idate.
              READ TABLE it_oijnomi INTO wa_oijnomi1 WITH KEY nomtk = n  omtk idate = wa_oijnomi-idate
                                                               sityp = '  ZO' docind = 'K'. "DOCNR = WA_VBAK-BSTNK

              "EOC CHARM 4000007227 FUNCTIONAL PRATIBHA DANGWAL TECHNICA  L ANKUR PRASHAR ON 12.10.2023" GTA WITHOUT PO

              CLEAR: nomtk.
              IF sy-subrc = 0.
*at NEW nomtk .
* if wa_display is not initial.
*
* append wa_display to it_display..
* clear wa_display.
* endif.
*ENDAT.
*********************************************************************GCV  & NCV
                READ TABLE it_yro_nom_param1 INTO wa_yro_nom_param1
                                             WITH KEY nomtk = wa_oijnomi  1-nomtk
                                                      nomit = wa_oijnomi  1-nomit.
                IF sy-subrc = 0.

                   CALL FUNCTION 'ROUND'
                     EXPORTING
*                      DECIMALS      = 2
                       input         = wa_yro_nom_param1-agcv
*                      SIGN          = ' '
                     IMPORTING
                       output        = wa_display-agcv
                     EXCEPTIONS
                      input_invalid = 1
                      overflow      = 2
                      type_invalid = 3.
                  IF sy-subrc NE 0.
                    subrc = sy-subrc.
                    REFRESH:lv_log[].
                    CLEAR: wv_log.
                    wv_log-progname = sy-tcode.
                    wv_log-uname      = sy-uname.
                    wv_log-sydate     = sy-datum.
                    wv_log-sytime     = sy-uzeit.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
*                    SELECT SINGLE email FROM yrga_email_nomi
*                    INTO   wv_log-email
*                    WHERE kunnr = p_kunnr AND del_ind NE 'X'.
*                    wv_log-logtype    = 'E'.
*                    wv_log-kunnr      = p_kunnr.
                    SELECT email FROM yrga_email_nomi
                    INTO   wv_log-email
                    UP TO 1 ROWS
                    WHERE kunnr = p_kunnr AND del_ind NE 'X'
                    ORDER BY PRIMARY KEY.
                    ENDSELECT.
** <- End changes by of Aditi on 19/11/24 for ATC
                    wv_log-logtype    = 'E'.
                    wv_log-kunnr      = p_kunnr.
                    IF subrc = 1.
                      wv_log-remarks = 'input_invalid'.
                    ELSEIF subrc = 2.
                      wv_log-remarks = 'overflow '.
                    ELSEIF subrc = 3.
                      wv_log-remarks = 'type_invalid'.
                    ENDIF.
                    APPEND wv_log TO lv_log.
                    IF lv_log IS NOT INITIAL.
                      MODIFY yrga_email_log FROM TABLE lv_log.
                    ENDIF.
                    CLEAR: subrc.
                    EXIT.
                  ENDIF.





                  CALL FUNCTION 'ROUND'
                    EXPORTING
*                     DECIMALS      = 2
                      input         = wa_yro_nom_param1-ancv
*                     SIGN          = ' '
                    IMPORTING
                      output        = wa_display-ancv
                    EXCEPTIONS
                      input_invalid = 1
                      overflow      = 2
                      type_invalid = 3.

                  IF sy-subrc NE 0.
                    subrc = sy-subrc.
                    REFRESH:lv_log[].
                    CLEAR: wv_log.
                    wv_log-progname = sy-tcode.
                    wv_log-uname      = sy-uname.
                    wv_log-sydate     = sy-datum.
                    wv_log-sytime     = sy-uzeit.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
*                    SELECT SINGLE email FROM yrga_email_nomi
*                   INTO    wv_log-email
*                   WHERE kunnr = p_kunnr AND del_ind NE 'X'.
                    SELECT email FROM yrga_email_nomi
                   INTO    wv_log-email
                   UP TO 1 ROWS
                   WHERE kunnr = p_kunnr AND del_ind NE 'X'
                   ORDER BY PRIMARY KEY.
                    ENDSELECT.
** <- End changes by of Aditi on 19/11/24 for ATC
                    wv_log-logtype    = 'E'.
                    wv_log-kunnr      = p_kunnr.
                    IF subrc = 1.
                      wv_log-remarks = 'input_invalid'.
                    ELSEIF subrc = 2.
                      wv_log-remarks = 'overflow '.
                    ELSEIF subrc = 3.
                      wv_log-remarks = 'type_invalid'.
                    ENDIF.
                    APPEND wv_log TO lv_log.
                    IF lv_log IS NOT INITIAL.
                      MODIFY yrga_email_log FROM TABLE lv_log.
                    ENDIF.
                    CLEAR: subrc.
                    EXIT.
                  ENDIF.



                 ENDIF.

************************************************************************  **************************************
*if wa_oijnomi-docnr = wa_vbak-bstnk.
                "SOC RITESH SINGH FUNCTIONAL : PRATIBHA DANGWAL DT:02.0  9.2020 on Charm 4000002463
                IF wa_oijnomi-idate GE '20201001'.
                  READ TABLE gt_rv_del INTO DATA(fs_data) WITH KEY kunn  r = wa_vbak-kunnr vbeln = wa_vbak-vbeln doc = wa_oijnomi-idate.
                  IF sy-subrc EQ 0.
                    wa_display-del_point = fs_data-ddqty.
                    wa_display-del_uom = fs_data-qnt_uom.
                  ENDIF.
                ELSE.
                  wa_display-del_point = wa_oijnomi1-yyoij_cnom_qty.
                  wa_display-del_uom = wa_oijnomi1-yyoij_cnom_uom.
                ENDIF.
                "EOC RITESH SINGH FUNCTIONAL : PRATIBHA DANGWAL DT:02.0  9.2020 on Charm 4000002463

*             wa_display-corr_nom = wa_oijnomi1-menge.
*             wa_display-corr_uom = wa_oijnomi1-unit_i.
                 wa_display-corr_nom = wa_oijnomi1-ga_allocated_qty.
                 wa_display-corr_uom = 'SM3'.
*
*   WA_DISPLAY-MATNR = WA_OIJNOMI1-S_MATNR_I.
*   WA_DISPLAY-DATE = WA_OIJNOMI1-IDATE.
               ENDIF.
*   else.
               wa_display-redel_point = wa_oijnomi-yyoij_cnom_qty.
               wa_display-redel_uom = wa_oijnomi-yyoij_cnom_uom.
               wa_display-corr_nom1 = wa_oijnomi-ga_allocated_qty.
               wa_display-corr_uom1 = 'SM3'."wa_oijnomi-unit_i.

              IF wa_oijnomi-yyoij_dpimb_qty IS NOT INITIAL.
                wa_display-daily = wa_oijnomi-yyoij_dpimb_qty.
                wa_display1-totalpi = wa_display1-totalpi + wa_oijnomi-y  yoij_dpimb_qty.
              ELSE.
                wa_display-daily = wa_oijnomi-yyoij_dnimb_qty * -1.
                wa_display1-totalni = wa_display1-totalni + wa_display-d  aily."wa_oijnomi-yyoij_dnimb_qty.
                IF wa_display1-totalni > 0.
                  wa_display1-totalni = wa_display1-totalni * -1.
                ENDIF.
              ENDIF.

               IF wa_oijnomi-yyoij_dds_qty IS NOT INITIAL.
                 wa_display-daily_dds = wa_oijnomi-yyoij_dds_qty.
               ELSE.
*   wa_display-daily_dds = wa_display-daily.
               ENDIF.
               wa_display-daily_overrun = wa_oijnomi-yyoij_ovr_qty.
               wa_display-daily_charge = wa_oijnomi-yyoij_unaovr_qty.

              wa_display-daily_cumm = wa_display-daily_cumm + wa_display  -daily + l_daily + wa_display-daily_dds.

               IF wa_display-daily_dds IS NOT INITIAL OR l_daily1 <> 0.
                 wa_display-daily_cumm_dds = wa_display-daily_cumm.

                l_daily1 = wa_display-daily_cumm_dds.
              ENDIF.
*          CLEAR l_daily.
************************************************************************  *************
*loop at it_YRVT_CONTRACT into wa_YRVT_CONTRACT where vbeln = wa_ojnomi
              CLEAR wa_yrvt_contract.
*BOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
              CLEAR lv_15.
*EOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
              IF wa_display-daily_cumm > 0.
                LOOP AT it_yrvt_contract INTO wa_yrvt_contract
                   WHERE vbeln = wa_oijnomi-docnr AND value_from <= wa_o  ijnomi-idate AND value_to => wa_oijnomi-idate AND clause = '10'.
                ENDLOOP.
                IF wa_yrvt_contract IS INITIAL.
                  LOOP AT it_yrvt_contract INTO wa_yrvt_contract
                   WHERE vbeln = wa_oijnomi-docnr AND value_from <= wa_o  ijnomi-idate AND value_to => wa_oijnomi-idate AND clause = '15'.
*BOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
                     lv_15 = 'X'.
*EOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
                  ENDLOOP.
                ENDIF.
                IF wa_yrvt_contract IS INITIAL.
                  LOOP AT it_yrvt_contract INTO wa_yrvt_contract
                    WHERE vbeln = wa_oijnomi-docnr AND value_from <= wa_o  ijnomi-idate AND value_to => wa_oijnomi-idate AND clause = '16'.
                  ENDLOOP.
                ENDIF.
              ELSE.
                LOOP AT it_yrvt_contract INTO wa_yrvt_contract
                  WHERE vbeln = wa_oijnomi-docnr AND value_from <= wa_oi  jnomi-idate AND value_to => wa_oijnomi-idate AND clause = '11'.
                ENDLOOP.
              ENDIF.

              CLEAR: l_mdq_quan,l_refvalue.


              IF wa_yrvt_contract-cal_unit = 10.
*              loop at it_oij_el_cp_layt into wa_oij_el_cp_layt where vb  eln = wa_oijnomi-docnr.
*                l_mdq_quan = l_mdq_quan + wa_oij_el_cp_layt-mdq_quan.
*              endloop.
*****         LT_YRVT_CONT_DCQ
*****          BOC CHARM ID : 4000001471 TECHICAL : VIKRAM BAJAJ FUNCTI  ONAL : PRATIBHA DANGWAL   DT: 9.1.2019
                READ TABLE lt_yrvt_cont_dcq INTO ls_yrvt_cont_dcq WITH  KEY vbeln = wa_oijnomi-docnr ydate = wa_oijnomi-idate.
                IF sy-subrc = 0 .
                  l_mdq_quan = ls_yrvt_cont_dcq-dcq_qty .

                ENDIF .
                CLEAR ls_yrvt_cont_dcq-dcq_qty . .
*****          EOC CHARM ID : 4000001471 TECHICAL : VIKRAM BAJAJ   FUNCTI  ONAL : PRATIBHA DANGWAL   DT: 9.1.2019

* read table it_yrvt_contract into wa_yrvt_contract with key vbeln = wa_  oijnomr-docnr.
* if sy-subrc = 0.
                 l_refvalue = l_mdq_quan * ( wa_yrvt_contract-threshold  / 100 ).
* endif.
               ELSEIF wa_yrvt_contract-cal_unit = 13.
                 l_mdq_quan = wa_yrvt_contract-remarks.
                 l_refvalue = l_mdq_quan * ( wa_yrvt_contract-threshold  / 100 ).

              ELSEIF wa_yrvt_contract-cal_unit = 11.

                READ TABLE it_yro_nom_param INTO wa_yro_nom_param WITH K  EY nomtk = wa_oijnomi-nomtk nomit = wa_oijnomi-nomit.
                IF sy-subrc = 0.
                  CLEAR l_adq.
*              SOC by Sachin and Atul sir on 19.08.2013
                  l_adq = wa_oijnomi-menge.

                  READ TABLE it_oij_el_cp_layt INTO wa_oij_el_cp_layt
                    WITH KEY vbeln = wa_oijnomi-docnr.

*              EOC   by Sachin and Atul sir on 19.08.2013

                  CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
                    EXPORTING
                      i_trqty = l_adq
                      i_truom = 'SM3'
                      i_tguom = wa_oij_el_cp_layt-mdq_uom
                      lv_gcv = wa_yro_nom_param-agcv
                      lv_ncv = wa_yro_nom_param-ancv
                    CHANGING
                      c_tgqty = l_adq.

                  l_refvalue = l_adq.
*        Soc by sachin remove the issue of cal type 11
                  l_mdq_quan = l_adq.
*        Eoc by sachin remove the issue of cal type 11
                  l_refvalue = l_refvalue * ( wa_yrvt_contract-threshol  d / 100 ).

                ENDIF.

              ENDIF.
*BOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
              IF lv_15 EQ 'X'.
                CLEAR l_refvalue.
              ENDIF.
*EOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
              IF wa_display-daily_cumm > 0.
                IF l_refvalue < wa_display-daily_cumm.
                  wa_display-chargable_imb = wa_display-daily_cumm - l_r  efvalue.
                  wa_display1-totalpic = wa_display1-totalpic + wa_displ  ay-chargable_imb.
                ENDIF.

              ELSEIF wa_display-daily_cumm < 0.
                l_refvalue = l_refvalue * -1.
                IF l_refvalue > wa_display-daily_cumm.
                  wa_display-chargable_imb = wa_display-daily_cumm - l_r  efvalue.
                   wa_display1-totalnic = wa_display1-totalnic + wa_displ  ay-chargable_imb.
                ENDIF.
              ENDIF.
              wa_display1-totalcharge = wa_display1-totalcharge + wa_dis  play-daily_charge.
              wa_display1-totaloverrun = wa_display1-totaloverrun + wa_d  isplay-daily_overrun.
*perform calculate_chargeble.

********************change by vaibhav 24.04.2025 to remove charage imbal  ance
              IF wa_vbak-augru = 'G32'.
                CLEAR wa_display-chargable_imb .
                wa_display1-augru = wa_vbak-augru.
                wa_display-augru = wa_vbak-augru.
              ENDIF.
              READ TABLE it_vbak INTO DATA(wa_vbak_g) WITH KEY vbeln_grp = wa_display-vbeln_grp augru = 'G32'.
              IF sy-subrc = 0.
                CLEAR wa_display-chargable_imb .
                wa_display1-augru = wa_vbak_g-augru.
                wa_display-augru = wa_vbak_g-augru.
              ENDIF.

*endif.
************************************************************************  **********************
              wa_display-matnr = wa_oijnomi-s_matnr_i.
              "Start by XSAP_AB on 5th June
              READ TABLE it_gta_imb INTO wa_gta_imb WITH KEY contract_fr  om = wa_vbak-vbeln.
              IF sy-subrc IS INITIAL.
                wa_display-vbeln_c = wa_gta_imb-contract_to.
              ENDIF.
              "End by XSAP_AB on 5th June
              wa_display-vbeln = wa_vbak-vbeln.
              wa_display-date = wa_oijnomi-idate.
              wa_display-ktext = wa_vbak-ktext.
              wa_display-kunnr = wa_vbak-kunnr.
              wa_display-menge = wa_oijnomi-menge.
              wa_display-mdq_quan = l_mdq_quan.

              READ TABLE it_yro_nom_param INTO wa_yro_nom_param
                                          WITH KEY nomtk = wa_oijnomi-no  mtk
                                                   nomit = wa_oijnomi-no  mit.
              IF sy-subrc = 0.

                CALL FUNCTION 'ROUND'
                  EXPORTING
*                   DECIMALS      = 2
                    input         = wa_yro_nom_param-agcv
*                   SIGN          = ' '
                  IMPORTING
                    output        = wa_display-agcv1
                  EXCEPTIONS
                    input_invalid = 1
                    overflow      = 2
                    type_invalid = 3.


                IF sy-subrc NE 0.
                  subrc = sy-subrc.
                  REFRESH:lv_log[].
                  CLEAR: wv_log.
                  wv_log-progname = sy-tcode.
                  wv_log-uname      = sy-uname.
                  wv_log-sydate     = sy-datum.
                  wv_log-sytime     = sy-uzeit.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
*                  SELECT SINGLE email FROM yrga_email_nomi
*                 INTO   wv_log-email
*                 WHERE kunnr = p_kunnr AND del_ind NE 'X'.
                  SELECT email FROM yrga_email_nomi
                 INTO   wv_log-email
                 UP TO 1 ROWS
                 WHERE kunnr = p_kunnr AND del_ind NE 'X'
                 ORDER BY PRIMARY KEY.
                  ENDSELECT.
** <- End changes by of Aditi on 19/11/24 for ATC
                  wv_log-logtype    = 'E'.
                  wv_log-kunnr      = p_kunnr.
                  IF subrc = 1.
                    wv_log-remarks = 'input_invalid'.
                  ELSEIF subrc = 2.
                    wv_log-remarks = 'overflow '.
                  ELSEIF subrc = 3.
                    wv_log-remarks = 'type_invalid'.
                  ENDIF.
                  APPEND wv_log TO lv_log.
                  IF lv_log IS NOT INITIAL.
                    MODIFY yrga_email_log FROM TABLE lv_log.
                  ENDIF.
                  CLEAR: subrc.
                  EXIT.
                ENDIF.




                CALL FUNCTION 'ROUND'
                  EXPORTING
*                   DECIMALS      = 2
                    input         = wa_yro_nom_param-ancv
*                   SIGN          = ' '
                  IMPORTING
                    output        = wa_display-ancv1
                  EXCEPTIONS
                    input_invalid = 1
                    overflow      = 2
                    type_invalid = 3.


                IF sy-subrc NE 0.
                  REFRESH:lv_log[].
                  CLEAR: wv_log.
                  wv_log-progname = sy-tcode.
                  wv_log-uname      = sy-uname.
                  wv_log-sydate     = sy-datum.
                  wv_log-sytime     = sy-uzeit.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
*                  SELECT SINGLE email FROM yrga_email_nomi
*                 INTO    wv_log-email
*                 WHERE kunnr = p_kunnr AND del_ind NE 'X'.
                  SELECT email FROM yrga_email_nomi
                 INTO    wv_log-email UP TO 1 ROWS
                 WHERE kunnr = p_kunnr AND del_ind NE 'X'
                 ORDER BY PRIMARY KEY.
                  ENDSELECT.
** <- End changes by of Aditi on 19/11/24 for ATC
                  wv_log-logtype    = 'E'.
                  wv_log-kunnr      = p_kunnr.
                  IF sy-subrc = 1.
                    wv_log-remarks = 'input_invalid'.
                  ELSEIF sy-subrc = 2.
                    wv_log-remarks = 'overflow '.
                  ELSEIF sy-subrc = 3.
                    wv_log-remarks = 'type_invalid'.
                  ENDIF.
                  APPEND wv_log TO lv_log.
                  IF lv_log IS NOT INITIAL.
                    MODIFY yrga_email_log FROM TABLE lv_log.
                  ENDIF.

                  EXIT.
                ENDIF.


               ENDIF.

               l_menge = l_menge + wa_oijnomi-ga_allocated_qty . "menge.
               l_wt = l_wt + ( wa_oijnomi-ga_allocated_qty * wa_display-a  gcv1 ). " change the del gcv to redel gcv by anumpam sir on 06.09.2011
               l_wt1 = l_wt1 + ( wa_oijnomi-ga_allocated_qty * wa_display  -ancv1 )." change the del ncv to redel ncv by anumpam sir on 06.09.2011
*        DELVERY QTY IN MBG
               IF wa_display-agcv IS NOT INITIAL AND wa_display-corr_nom  IS NOT INITIAL .
                 l_adq = wa_display-agcv * wa_display-corr_nom * '3.96825  4' / 1000000 .
                 wa_display-del_mbg = l_adq.
               ENDIF.
*        REDELVERY QTY IN MBG
               IF wa_display-agcv1 IS NOT INITIAL AND wa_display-corr_nom  1 IS NOT INITIAL .
                 l_adq = wa_display-agcv1 * wa_display-corr_nom1 * '3.968  254' / 1000000 .
                 wa_display-redel_mbg = l_adq.
               ENDIF.

               IF wa_oijnomi-yyoij_dds_qty IS INITIAL.
*wa_display-daily_dds = wa_oijnomi-YYOIJ_DDS_QTY.
*else.
                 wa_display-daily_dds = wa_display-daily.
               ENDIF.
*BOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
               CLEAR: lv_15, lv_11, lv_10, lv_12.
*EOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
               LOOP AT it_yrvt_contract INTO wa_yrvt_contract1 WHERE vbel  n = wa_vbak-vbeln AND value_from LE wa_oijnomi-idate AND value_to GE wa_  oijnomi-idate.
                 IF wa_yrvt_contract1-clause EQ '15'.
                   lv_15 = 'X'.
                 ENDIF.
                 IF wa_yrvt_contract1-clause EQ '11'.
                   lv_11 = 'X'.
                 ENDIF.
                 IF wa_yrvt_contract1-clause EQ '10'.
                   lv_10 = 'X'.
                 ENDIF.
                 IF wa_yrvt_contract1-clause EQ '12'.
                   lv_12 = 'X'.
                 ENDIF.
                 CLEAR wa_yrvt_contract1.
               ENDLOOP.
*BOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
               IF lv_12 NE 'X'.
                 wa_display1-totalcharge = wa_display1-totalcharge - wa_d  isplay-daily_charge.
                 CLEAR wa_display-daily_charge.
               ENDIF.
               IF lv_15 EQ 'X'.
                 IF lv_11 EQ 'X'.

                   IF wa_display-chargable_imb < 0.
                   ELSE.
*                 IF wa_display-daily_cumm_dds <> 0 AND wa_display-charga  ble_imb > 0.
                     IF wa_display-chargable_imb > 0.
                       wa_display1-totalpic = wa_display1-totalpic - wa_d  isplay-chargable_imb.
                       wa_display-daily_cumm_dds = wa_display-chargable_i  mb.
                       CLEAR wa_display-chargable_imb.
                     ENDIF.
                  ENDIF.
                ELSE.
*              IF wa_display-daily_cumm_dds <> 0 AND wa_display-chargabl  e_imb > 0.
                  wa_display-daily_cumm_dds = wa_display-chargable_imb.
                  IF wa_display-chargable_imb > 0.
                     wa_display1-totalpic = wa_display1-totalpic - wa_dis  play-chargable_imb..
                     CLEAR wa_display-chargable_imb.
                  ENDIF.
                  IF wa_display-chargable_imb < 0.
                     wa_display1-totalnic = wa_display1-totalnic - wa_dis  play-chargable_imb..
                     CLEAR wa_display-chargable_imb.
                  ENDIF.
                ENDIF.
*EOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
              ELSE.
                CLEAR wa_display-daily_cumm_dds.
                IF lv_11 EQ 'X'.
                ELSE.
                  IF wa_display-chargable_imb < 0.
                     wa_display1-totalnic = wa_display1-totalnic - wa_dis  play-chargable_imb.
                     CLEAR wa_display-chargable_imb.
                  ENDIF.
                ENDIF.
                IF lv_10 EQ 'X'.
                ELSE.
                  IF wa_display-chargable_imb > 0.
                     wa_display1-totalpic = wa_display1-totalpic - wa_dis  play-chargable_imb.
                     CLEAR wa_display-chargable_imb.
                  ENDIF.
                ENDIF.
              ENDIF.

               IF wa_display-daily_cumm_dds < 0.
*    wa_display1-totaldds = wa_display1-totaldds - wa_display-daily_cumm_  dds.
                 CLEAR wa_display-daily_cumm_dds.
               ENDIF.


                wa_display1-totaldds = wa_display1-totaldds + wa_display-d  aily_cumm_dds.

                APPEND wa_display TO it_display.
                APPEND wa_display TO it_disp_temp.   "Added by XSAP_AB on  27th May,2024
                CLEAR fs_data.
                l_daily = wa_display-daily_cumm.
*
            ENDLOOP.

           CLEAR l_daily.
           CLEAR l_daily1.
* WA_DISPLAY-MATNR = WA_OIJNOMI-S_MATNR_I.
* WA_DISPLAY-DATE = WA_OIJNOMI-IDATE.
*ENDIF.

            IF wa_display1 IS NOT INITIAL.
              wa_display1-totalcumi = wa_display-daily_cumm.
              IF s_vbeln <> ''.
                wa_display1-vbeln_grp = wa_vbak-vbeln_grp.
              ENDIF.

*        wa_display1-menge = l_menge.
              wa_display1-wt = l_wt / l_menge.
              wa_display1-wt1 = l_wt1 / l_menge.



              wa_display1-kunnr = wa_vbak-kunnr.
              wa_display1-augru = wa_display-augru.
              APPEND wa_display1 TO it_display1.
              CLEAR: wa_display1, wa_display.

            ENDIF.
            CLEAR: l_menge,l_wt,l_wt1.

          ENDLOOP.
        ENDIF.


         REFRESH it_display11.
         DELETE it_display1 WHERE vbeln IS INITIAL.
*data l_tabix type sy-tabix.
         DATA w_ncv TYPE yyncv.
         DATA w_gcv TYPE yyncv.
*     move it_display1[] to it_display11.
         LOOP AT it_display1 INTO wa_display1.
           l_tabix = sy-tabix.
           CLEAR: l_mdq_quan.
           LOOP AT it_oij_el_cp_layt INTO wa_oij_el_cp_layt WHERE vbeln =  wa_display1-vbeln.
             l_mdq_quan = l_mdq_quan + wa_oij_el_cp_layt-mdq_quan.
           ENDLOOP.
           CLEAR: l_adq,w_ncv,w_gcv.
           l_adq = wa_display1-totalpic.
           w_ncv = wa_display1-wt1.
           w_gcv = wa_display1-wt.
           CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
             EXPORTING
               i_trqty = l_adq
               i_truom = wa_oij_el_cp_layt-mdq_uom
               i_tguom = 'SM3'
               lv_gcv = w_gcv
               lv_ncv = w_ncv
             CHANGING
               c_tgqty = l_adq.
           wa_display1-totalpic1 = l_adq.

          CLEAR l_adq.
          l_adq = wa_display1-totaldds.
          CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
            EXPORTING
              i_trqty = l_adq
              i_truom = wa_oij_el_cp_layt-mdq_uom
              i_tguom = 'SM3'
              lv_gcv = w_gcv
              lv_ncv = w_ncv
            CHANGING
              c_tgqty = l_adq.
          wa_display1-totaldds1 = l_adq.



          CLEAR l_adq.
          l_adq = wa_display1-totalnic.
          CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
            EXPORTING
              i_trqty = l_adq
              i_truom = wa_oij_el_cp_layt-mdq_uom
              i_tguom = 'SM3'
              lv_gcv = w_gcv
              lv_ncv = w_ncv
            CHANGING
              c_tgqty = l_adq.
          wa_display1-totalnic1 = l_adq.
*      opening balance and closing balance.
          SORT it_display BY date ASCENDING.
          READ TABLE it_display INTO wa_display WITH KEY vbeln = wa_disp  lay1-vbeln.
          IF sy-subrc EQ 0 .
            wa_display1-opencumi = wa_display-daily_cumm - wa_display-da  ily.
          ENDIF.
          SORT it_display BY date DESCENDING.
          READ TABLE it_display INTO wa_display WITH KEY vbeln = wa_disp  lay1-vbeln.
          IF sy-subrc EQ 0 .
            wa_display1-closecumi = wa_display-daily_cumm .
          ENDIF.

          MODIFY it_display1 FROM wa_display1 INDEX l_tabix   TRANSPORTIN  G totalpic1 totalnic1 totaldds1 opencumi closecumi .

         ENDLOOP.
* grouping of contract to master contract for clubbing of imbalance
         SELECT SINGLE * FROM vbak
           INTO CORRESPONDING FIELDS OF wa_vbak
*           WHERE vbeln = s_vbeln" w_master_contract
           WHERE vbeln = w_master_contract    " add w_master_contract **-     > SOC CHARM ID :4000008860    TECHICAL : RAVINDER SINGH FUNCTIONAL :  SHREYOSHI DT:09.10.2024
           AND abrvw = 'Z02' .
         IF sy-subrc EQ 0 AND r_dds IS INITIAL.
*       delete the contract where master contract is different
*         DELETE it_display1 WHERE vbeln_grp NE s_vbeln."w_master_contrac  t.
           LOOP AT it_display INTO wa_display .
             READ TABLE it_display1 INTO wa_display1 WITH KEY vbeln = wa_  display-vbeln.
             IF sy-subrc NE 0 .
               DELETE TABLE it_display FROM wa_display.
            ENDIF.
          ENDLOOP.

          w_group_imbalance = 'X'.
          DATA: w_tot_redel_vol     TYPE p DECIMALS 3,
                 w_tot_redel_energy TYPE p DECIMALS 3.
          SORT it_display BY date.
**SOC By Gaurav(TCS) & A.Mondal(GAIL) On 01/08/2018 TO accomulate rates
          it_display3[] = it_display[].
          DELETE it_display3[] WHERE vbeln_grp IS INITIAL.
          SORT    it_display3 BY date.
          LOOP AT it_display3 INTO wa_display." WHERE vbeln_grp IS NOT I  NITIAL.
**EOC By Gaurav(TCS) & A.Mondal(GAIL) On 01/08/2018 TO accomulate rates
*BOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
            READ TABLE it_vbak INTO wa_vbak WITH KEY abrvw = 'Z01' vbeln = wa_display-vbeln.
            IF sy-subrc EQ 0.
               LOOP AT it_yrvt_contract INTO wa_yrvt_contract WHERE vbeln = wa_vbak-vbeln AND value_from LE wa_display-date
                 AND value_to GE wa_display-date.
                 IF wa_yrvt_contract-clause = '15' .
                   IF wa_display_temp-sel = 'Z'.
                     wa_display_temp-sel = 'V'.
                   ELSE.
                     wa_display_temp-sel = 'X'.
                   ENDIF.
                 ELSEIF wa_yrvt_contract-clause = '10' .
                   IF wa_display_temp-sel = 'Z'.
                     wa_display_temp-sel = 'W'.
                   ELSE.
                     wa_display_temp-sel = 'Y'.
                   ENDIF.

                 ELSEIF wa_yrvt_contract-clause = '11' .
                   IF wa_display_temp-sel = 'Y'.
                     wa_display_temp-sel = 'W'.
                   ELSEIF wa_display_temp-sel = 'X'.
                     wa_display_temp-sel = 'V'.
                   ELSE.
                     wa_display_temp-sel = 'Z'.
                   ENDIF.
                 ENDIF.
               ENDLOOP.
            ENDIF.
*EOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
            wa_display_temp-date = wa_display-date.
            wa_display_temp-mdq_quan = wa_display_temp-mdq_quan + wa_dis  play-mdq_quan .
            wa_display_temp-del_point = wa_display_temp-del_point + wa_  display-del_point .
            wa_display_temp-corr_nom = wa_display_temp-corr_nom + wa_d  isplay-corr_nom    .
            wa_display_temp-del_mbg    = wa_display_temp-del_mbg + wa_dis  play-del_mbg .
            wa_display_temp-redel_point = wa_display_temp-redel_point +  wa_display-redel_point .
            wa_display_temp-corr_nom1    = wa_display_temp-corr_nom1 + w  a_display-corr_nom1 .
            wa_display_temp-redel_mbg    = wa_display_temp-redel_mbg + wa  _display-redel_mbg .
            wa_display_temp-corr_uom = wa_display-corr_uom.
            wa_display_temp-del_uom = wa_display-del_uom.
            wa_display_temp-redel_uom = wa_display-redel_uom.
            wa_display_temp-corr_uom1 = wa_display-corr_uom1.
            wa_display_temp-matnr = wa_display-matnr.
            w_tot_redel_vol = wa_display-corr_nom1 + w_tot_redel_vol .
            w_tot_redel_energy = wa_display-redel_mbg + w_tot_redel_en  ergy .
            AT END OF date .
*******SOC by A.Mondal / Nitin Dhamija on 25.07.2019 for Master Contract Identification**********
              READ TABLE it_vbak4 INTO DATA(wa_vbeln) WITH KEY kunnr = w  a_vbak-kunnr abrvw = 'Z02'. " Added by A.Mondal / Nitin Dhamija on 25.07  .2019 for Master Contract Identification
              IF sy-subrc = 0.
                s_vbeln = wa_vbeln-vbeln.
              ENDIF.
*******EOC by A.Mondal / Nitin Dhamija on 25.07.2019 for Master Contract Identification**********
              wa_display_temp-vbeln = s_vbeln. "w_master_contract.
              wa_display_temp-ktext = wa_vbak-ktext.
              wa_display_temp-kunnr = wa_vbak-kunnr.
              APPEND wa_display_temp TO it_display_temp.
              CLEAR wa_display_temp .
            ENDAT.
          ENDLOOP.

***************SOC by A,Mondal / Nitin Dhamija on 25.07.2019************  ********

***************EOC by A,Mondal / Nitin Dhamija on 25.07.2019************  ********

          SELECT * INTO CORRESPONDING FIELDS OF TABLE it_yrg_cumm_imb2
               FROM yrg_cumm_imb
               WHERE yy_contract = w_master_contract "s_vbeln "   AND
               AND endda < s_date-low.
          SORT it_yrg_cumm_imb2 BY timestamp DESCENDING.

          CLEAR: l_daily.
          CLEAR l_daily1.
          DATA : lv_checked TYPE c.
          SORT it_display_temp BY date.
          LOOP AT it_display_temp INTO wa_display_temp .
            wa_display_temp-daily = wa_display_temp-del_mbg - wa_display  _temp-redel_mbg .
            w_temp = wa_display_temp-redel_mbg - wa_display_temp-mdq_qu  an .
            w_temp1 = wa_display_temp-redel_mbg - wa_display_temp-redel_  point .
            IF    w_temp > 0 AND w_temp1 > 0 .
               IF wa_display_temp-redel_point > wa_display_temp-mdq_quan  .
                 wa_display_temp-daily_overrun = wa_display_temp-redel_mb  g - wa_display_temp-redel_point .
               ELSE.
                 wa_display_temp-daily_overrun = wa_display_temp-redel_m  bg - wa_display_temp-mdq_quan .
               ENDIF.
            ENDIF.
            CLEAR w_temp.
            w_temp = wa_display_temp-daily_overrun - wa_display_temp-mdq  _quan * '0.1' .
            IF w_temp > 0 .
              wa_display_temp-daily_charge = w_temp.
            ENDIF.
            IF l_daily IS INITIAL AND lv_checked IS INITIAL.
              READ TABLE it_yrg_cumm_imb2 INTO wa_yrg_cumm_imb INDEX 1.
              IF sy-subrc = 0.
                l_daily = wa_yrg_cumm_imb-yy_oij_cumimb.
                "Start of insert by XSAP_AB on 11th Sept 2024 - Imbalanc  e Master Shifting
              ELSE.
                CLEAR: l_cumm_bal,wa_header.
                SORT it_disp_temp BY vbeln ASCENDING date DESCENDING.
                LOOP AT it_gta_imb INTO DATA(wa_gta_imb_1) WHERE contrac  t_to = wa_display_temp-vbeln.    "Wa_vbak-vbeln replaced by wa_display_te  mp-vbeln by XSAP_AB on 17th Oct,2024
                   wa_header-vbeln = wa_display_temp-vbeln.
                   wa_header-p_cont = wa_gta_imb_1-contract_from.
                  "start of comment by XSAP_AB on 17th Oct,2024
                  "check parent in final table
*                   READ TABLE it_disp_temp INTO DATA(wa_disp_temp_1) WIT  H KEY vbeln = wa_gta_imb_1-contract_from.
*                   IF sy-subrc IS INITIAL.
*                     l_cumm_bal = l_cumm_bal + wa_disp_temp_1-daily_cumm  .
*                     wa_header-imbal = wa_disp_temp_1-daily_cumm.
*                   ELSE.    "if parent not exist in final table, fetch f  rom cumm_imb table
                  "End of comment by XSAP_AB on 17th Oct,2024
                  "if parent not exist in final table, fetch from cumm_i  mb table
                   READ TABLE it_cumm_imb_temp INTO DATA(wa_cumm_imb_temp  _1) WITH KEY yy_contract = wa_gta_imb_1-contract_from.
                   IF sy-subrc IS INITIAL.
                     l_cumm_bal = l_cumm_bal + wa_cumm_imb_temp_1-yy_oij_  cumimb.
                     wa_header-imbal = wa_cumm_imb_temp_1-yy_oij_cumimb.
                   ENDIF.
*                   ENDIF.   "commented by XSAP_AB on 17th Oct,2024
                   APPEND wa_header TO it_header.
                   CLEAR: wa_cumm_imb_temp_1,wa_header.
                ENDLOOP.
                l_daily = l_cumm_bal.
                CLEAR wa_gta_imb_1.
                "End of insert by XSAP_AB on 11th Sept 2024
              ENDIF.
              lv_checked = 'X'. " Added by XSAP_AB2-AV
            ENDIF.
*   BOC by XSAP_AB2-AV
*        l_daily = l_daily + wa_display_temp-daily .
*          IF l_daily IS NOT INITIAL.
            l_daily = l_daily + wa_display_temp-daily .
*          ENDIF.
*   EOC by XSAP_AB2
            wa_display_temp-daily_cumm = l_daily .

*        chargeable imbalance
            IF wa_display_temp-daily_cumm > 0 .
              wa_display_temp-chargable_imb = wa_display_temp-daily_cumm- wa_display_temp-mdq_quan * '0.1' .
              IF wa_display_temp-chargable_imb LT 0 .
                wa_display_temp-chargable_imb = 0 .
              ENDIF.
            ELSEIF wa_display_temp-daily_cumm < 0 .
              wa_display_temp-chargable_imb = ( wa_display_temp-daily_cu  mm + wa_display_temp-mdq_quan * '0.05'    ) .
              IF wa_display_temp-chargable_imb GT 0 .
                wa_display_temp-chargable_imb = 0 .
              ENDIF.
            ENDIF.
*BOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
            l_daily2 = l_daily2 + wa_display_temp-daily.
            IF wa_display_temp-sel = 'X'.
              IF l_daily2 > 0.
                wa_display_temp-daily_cumm_dds = wa_display_temp-daily_c  umm." + l_daily2.
              ENDIF.
              wa_display_temp-daily_dds =     wa_display_temp-daily.
*          wa_display_temp-daily_cumm = wa_display_temp-daily_cumm + l_  daily2. ""
              CLEAR: wa_display_temp-daily, wa_display_temp-chargable_i  mb.
            ELSEIF wa_display_temp-sel = 'V'.
              IF l_daily2 > 0.
                wa_display_temp-daily_cumm_dds = wa_display_temp-daily_c  umm." + l_daily2.
              ENDIF.
              wa_display_temp-daily_dds =     wa_display_temp-daily.
*          wa_display_temp-daily_cumm = wa_display_temp-daily_cumm + l_  daily2.""
              IF wa_display_temp-daily_cumm < 0.
                IF ( ( wa_display_temp-mdq_quan * '0.05' ) + wa_displa  y_temp-daily_cumm ) LE 0.
                   wa_display_temp-chargable_imb = wa_display_temp-daily  _cumm + ( wa_display_temp-mdq_quan * '0.05' ).
                ENDIF.

              ENDIF.
              CLEAR: wa_display_temp-daily,   wa_display_temp-chargable_i  mb .
            ELSEIF wa_display_temp-sel EQ 'Y' AND wa_display_temp-daily  NE 0.
*          wa_display_temp-daily_cumm_dds = wa_display_temp-daily_cumm_d  ds + l_daily2.
*          wa_display_temp-daily_cumm = wa_display_temp-daily_cumm + l_  daily2.""
              wa_display_temp-daily_dds =   wa_display_temp-daily.
              IF wa_display_temp-daily_cumm > 0.
                IF ( wa_display_temp-daily_cumm - ( wa_display_temp-md  q_quan * '0.10' ) ) GE 0.
                  wa_display_temp-chargable_imb = wa_display_temp-daily  _cumm - ( wa_display_temp-mdq_quan * '0.10' ).
                ENDIF.
              ELSE.
                CLEAR wa_display_temp-chargable_imb.
              ENDIF.
            ELSEIF wa_display_temp-sel EQ 'Z' AND wa_display_temp-daily  NE 0.
*          wa_display_temp-daily_cumm = wa_display_temp-daily_cumm + l_  daily2.""
              wa_display_temp-daily_dds =   wa_display_temp-daily.
              IF   wa_display_temp-daily_cumm < 0.
                IF ( ( wa_display_temp-mdq_quan * '0.05' ) + wa_displa  y_temp-daily_cumm ) LE 0.
                   wa_display_temp-chargable_imb = wa_display_temp-daily  _cumm + ( wa_display_temp-mdq_quan * '0.05' ).
                ENDIF.
              ELSE.
                CLEAR wa_display_temp-chargable_imb.
              ENDIF.
            ELSEIF wa_display_temp-sel EQ 'W' AND wa_display_temp-daily  NE 0.
*          wa_display_temp-daily_cumm = wa_display_temp-daily_cumm + l_  daily2.""
              wa_display_temp-daily_dds =    wa_display_temp-daily.
              IF wa_display_temp-daily_cumm > 0.
                IF ( wa_display_temp-daily_cumm - ( wa_display_temp-md  q_quan * '0.10' ) ) GE 0.
                   wa_display_temp-chargable_imb = wa_display_temp-daily  _cumm - ( wa_display_temp-mdq_quan * '0.10' ).
                ENDIF.
              ENDIF.
              IF wa_display_temp-daily_cumm < 0.
                IF ( ( wa_display_temp-mdq_quan * '0.05' ) + wa_displa  y_temp-daily_cumm ) LE 0.
                   wa_display_temp-chargable_imb = wa_display_temp-daily  _cumm + ( wa_display_temp-mdq_quan * '0.05' ).
                ENDIF.
              ENDIF.
            ELSEIF wa_display_temp-sel IS INITIAL AND wa_display_temp-da  ily NE 0.
              wa_display_temp-daily_dds =    wa_display_temp-daily.
*          wa_display_temp-daily_cumm = wa_display_temp-daily_cumm + l_  daily2.""
              CLEAR wa_display_temp-chargable_imb.
            ENDIF.
            IF r_d1 EQ 'X' OR r_d3 EQ 'X'
*             boc charm 613 .
              OR r_d6 = 'X' .
*             eoc charm 613 .
              .
              CLEAR wa_display_temp-daily_cumm_dds.
            ENDIF.

            READ TABLE it_vbak1 INTO wa_vbak_g WITH KEY vbeln_grp = wa_d  isplay_temp-vbeln augru = 'G32'.
            IF sy-subrc = 0.
              CLEAR wa_display_temp-chargable_imb .
              wa_display_temp-augru = wa_vbak_g-augru.
            ENDIF.

            MODIFY it_display_temp FROM wa_display_temp.
          ENDLOOP.
*EOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
*        data for summary sheet.

          SORT it_display_temp BY date ASCENDING.
          CLEAR: wa_display1 .
          LOOP AT it_display_temp INTO wa_display_temp.
            wa_display1-vbeln = wa_display_temp-vbeln.
            wa_display1-matnr = wa_display_temp-matnr.
            wa_display1-ktext = wa_display_temp-ktext.
              wa_display1-kunnr = wa_display_temp-kunnr.
              IF wa_display_temp-daily < 0 .
                wa_display1-totalni = wa_display1-totalni + wa_display_tem  p-daily .
              ELSE.
                wa_display1-totalpi = wa_display1-totalpi + wa_display_tem  p-daily .
            ENDIF.
            IF wa_display_temp-chargable_imb < 0 .
              wa_display1-totalnic = wa_display1-totalnic + wa_display_t  emp-chargable_imb .
            ELSE.
              wa_display1-totalpic = wa_display1-totalpic + wa_display_t  emp-chargable_imb .
            ENDIF.
            wa_display1-totalcumi = wa_display_temp-daily_cumm .
            wa_display1-totalcharge = wa_display1-totalcharge + wa_displ  ay_temp-daily_charge.
            wa_display1-totaloverrun = wa_display1-totaloverrun + wa_dis  play_temp-daily_overrun .
          ENDLOOP.
          wa_display1-knkli = wa_vbak-knkli .

          wa_display1-wt = w_tot_redel_energy * 1000000 / ( w_tot_redel_  vol * '3.968254' ) .
          wa_display1-wt1 = wa_display1-wt / '1.109'.

            l_adq = wa_display1-totalpic.
            w_ncv = wa_display1-wt1.
            w_gcv = wa_display1-wt.
            CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
              EXPORTING
                i_trqty = l_adq
                i_truom = 'MBG'
                i_tguom = 'SM3'
                lv_gcv = w_gcv
                lv_ncv = w_ncv
              CHANGING
                c_tgqty = l_adq.
            wa_display1-totalpic1 = l_adq.

          CLEAR l_adq.
          l_adq = wa_display1-totalnic.
          CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
            EXPORTING
              i_trqty = l_adq
              i_truom = 'MBG'
              i_tguom = 'SM3'
              lv_gcv = w_gcv
              lv_ncv = w_ncv
            CHANGING
              c_tgqty = l_adq.
          wa_display1-totalnic1 = l_adq.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
*          SELECT SINGLE * INTO wa_yrg_cumm_imb
*                FROM yrg_cumm_imb
*                WHERE yy_contract = w_master_contract "s_vbeln " AND
*                AND begda = s_date-low
*                AND endda = s_date-high.
          SELECT * INTO wa_yrg_cumm_imb
                FROM yrg_cumm_imb
                 UP TO 1 ROWS
                 WHERE yy_contract = w_master_contract "s_vbeln " AND
                 AND begda = s_date-low
                 AND endda = s_date-high
                 ORDER BY PRIMARY KEY.
          ENDSELECT.
** <- End changes by of Aditi on 19/11/24 for ATC
          IF sy-subrc EQ 0 .
            wa_display1-vbeln1 = wa_yrg_cumm_imb-vbeln.
          ENDIF.
          wa_display1-line_color = 'C410' .
          wa_display1-augru = wa_display_temp-augru.
          APPEND wa_display1 TO it_display1.

          IF overrun <> 'X' AND s_vbeln IS NOT INITIAL. "Overrun,no Mast  er contract
            APPEND LINES OF it_display_temp TO it_display.
          ENDIF.

            SORT it_display BY vbeln DESCENDING.
          ENDIF.


        LOOP AT it_vbak1 INTO wa_vbak WHERE abrvw = 'Z01'.
           CLEAR wa_display11.
           READ TABLE it_display1 INTO wa_display11 WITH KEY vbeln = wa_v  bak-vbeln.
           IF sy-subrc = 0.
             LOOP AT it_display1 INTO wa_display1 WHERE vbeln_grp = wa_vb  ak-vbeln_grp.
               IF wa_display1-vbeln <> wa_display11-vbeln.
                 wa_display11-totalpic = wa_display11-totalpic + wa_displ  ay1-totalpic.
                 wa_display11-totalnic = wa_display11-totalnic + wa_displ  ay1-totalnic.
                 wa_display11-totalpic1 = wa_display11-totalpic1 + wa_dis  play1-totalpic1.
                 wa_display11-totalnic1 = wa_display11-totalnic1 + wa_dis  play1-totalnic1.
                 wa_display11-totalcharge = wa_display11-totalcharge + wa  _display1-totalcharge.

                ENDIF.
              ENDLOOP.
              APPEND wa_display11 TO it_display11.
            ENDIF.
          ENDLOOP.

*loop at it

*endif.
      ENDIF.

        SORT it_display BY date.

      IF r_d4 IS INITIAL.
        IF overrun = 'X'.
          r_d1 = 'X'.
        ENDIF.
        IF it_display[] IS NOT INITIAL AND r_d1 = 'X'.
*   Set alv field catalogue
          PERFORM alv_build_fieldcat.
          PERFORM display_alv.

        ENDIF.


        DELETE it_display1[] WHERE vbeln IS INITIAL.
        CLEAR: process_flag.
        IF it_display1[] IS NOT INITIAL AND ( r_d2 = 'X' OR r_d5 = 'X' O  R r_dds = 'X' ).
          CLEAR wa_display1.
          IF r_d5 IS NOT INITIAL OR r_dds IS NOT INITIAL.
            LOOP AT it_display1 INTO wa_display1.

*    Check Previous Postings
               IF r_dds = 'X'.
                 CALL METHOD ycl_gms_report_validations=>check_previous_p  ostting1
                   EXPORTING
                      i_vbeln           = wa_display1-vbeln     " Sales and  Distribution Document Number
                      i_kunnr           = wa_display1-kunnr     " Customer  Number
                      i_knkli           = wa_display1-knkli     " Sales Doc  ument
                      i_valid_from      = s_date-low           " Valid From  Date
                   IMPORTING
                      e_exit_processing = process_flag.
                 IF process_flag = 'X'.
                   EXIT.
                 ELSE.
                   CONTINUE.
                 ENDIF.
               ELSE.
****Change by TCS:AM dated Nov 16, 2018 Func : Aragya Mandal RMS:1244
*SOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala  nce TR:DVRK9A1KR0
                 IF ( sy-tcode NE 'YRGR102' AND NOT sy-batch EQ 'X' ) OR  ( p_post EQ 'X' ).
                  CALL METHOD ycl_gms_report_validations=>check_previous_  postings
                      EXPORTING
                        i_vbeln      = wa_display1-vbeln     " Sales and Di  stribution Document Number
                        i_kunnr      = wa_display1-kunnr     " Customer Num  ber
                        i_knkli      = wa_display1-knkli     " Sales Docume  nt
                        i_valid_from = s_date-low    " Valid From Date
                        i_valid_to   = s_date-high
                      IMPORTING
                        e_exit_processing = process_flag
                           .
*               CALL METHOD ycl_gms_report_validations=>check_previous_po  stings
*                 EXPORTING
*                    i_vbeln      = wa_display1-vbeln     " Sales and Distr  ibution Document Number
*                    i_kunnr      = wa_display1-kunnr     " Customer Number
*                    i_knkli      = wa_display1-knkli     " Sales Document
*                   i_valid_from = s_date-low.         " Valid From Date
                   IF process_flag = 'X'.
                     EXIT.
                   ELSE.
                     CONTINUE.
                   ENDIF.
                 ENDIF.
*EOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala  nce TR:DVRK9A1KR0
****Change by TCS:AM dated Nov 16, 2018 Func : Aragya Mandal RMS:1244
              ENDIF.
            ENDLOOP.
****Change by TCS:AM dated Nov 16, 2018 Func : Aragya Mandal RMS:1244
            IF process_flag = 'X'.
              REFRESH it_display1.
            ENDIF.
****Change by TCS:AM dated Nov 16, 2018 Func : Aragya Mandal RMS:1244
          ENDIF.
*   Set alv field catalogue
          LOOP AT it_display1 ASSIGNING FIELD-SYMBOL(<fs_display>).
            IF <fs_display>-augru = 'G32'.
              CLEAR :      <fs_display>-totalnic1,
                           <fs_display>-totalpic,
                           <fs_display>-totalpic1,
                           <fs_display>-totalnic.

            ENDIF.
          ENDLOOP.


          SORT it_display1 BY vbeln ASCENDING.
          PERFORM alv_build_fieldcat1.
          PERFORM build_layout.
          PERFORM display_alv1.

        ENDIF.
        IF it_display1[] IS NOT INITIAL AND
            it_display[] IS NOT INITIAL AND
*         r_d3 = 'X'
*        boc charm 613 .
          ( r_d6 = 'X' OR r_d3 = 'X' ) .
**       eoc charm 613 .
          .
          LOOP AT it_display1 ASSIGNING <fs_display>.
             IF <fs_display>-augru = 'G32'.
               CLEAR :     <fs_display>-totalnic1,
                           <fs_display>-totalpic,
                           <fs_display>-totalpic1,
                           <fs_display>-totalnic.

             ENDIF.
          ENDLOOP.
          PERFORM print_pdf.
        ENDIF.
      ELSE.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
        EXPORT it_display                    "#EC CI_FLDEXT_OK[2215424]
                it_display1
                TO MEMORY ID 'ID9'.
** <- End changes by of Aditi on 19/11/24 for ATC
      ENDIF.
     ELSE.
       IF p_kunnr IS NOT INITIAL AND s_vbeln IS NOT INITIAL.
         SELECT * FROM yrga_wt_avg_prc INTO TABLE it_wt_prc WHERE guebg =  s_date-low AND gueen EQ s_date-high AND kunnr EQ p_kunnr AND vbeln_grp  EQ s_vbeln.
       ELSEIF p_kunnr IS NOT INITIAL AND s_vbeln IS INITIAL.
         SELECT * FROM yrga_wt_avg_prc INTO TABLE it_wt_prc WHERE guebg =  s_date-low AND gueen EQ s_date-high AND kunnr EQ p_kunnr.
       ELSEIF s_vbeln IS NOT INITIAL AND p_kunnr IS INITIAL.
         SELECT * FROM yrga_wt_avg_prc INTO TABLE it_wt_prc WHERE guebg =  s_date-low AND gueen EQ s_date-high AND vbeln_grp EQ s_vbeln.
       ELSE.
         SELECT * FROM yrga_wt_avg_prc INTO TABLE it_wt_prc WHERE guebg =  s_date-low AND gueen EQ s_date-high.
       ENDIF.
       IF sy-subrc EQ 0.
         PERFORM display_wt_avg_prc.
       ENDIF.
     ENDIF.
   ENDIF .
*     SELECT *
*              FROM oijnomi
*              INTO TABLE it_nomi
*               WHERE idate in s_date AND delind NE 'X'.
*&                                                                       *
*&       Form ALV_BUILD_FIELDCAT
*&                                                                       *
*        text
*                                                                        *
*     > p1          text
* <      p2         text
*                                                                        *
FORM alv_build_fieldcat .

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'DATE'.
  w_cond_fc-seltext_l   = 'Date'.
  APPEND w_cond_fc TO it_cond_fc .

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'KUNNR'.
  w_cond_fc-seltext_l   = 'Customer'.
  APPEND w_cond_fc TO it_cond_fc .

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'VBELN'.
  w_cond_fc-seltext_l   = 'CONTRACT NUMBER'.
  APPEND w_cond_fc TO it_cond_fc .
  "start of insert by XSAP_AB on 5th June
  "Adding new field to ALV report only for imbalance Radiobutton
  CLEAR w_cond_fc.
  IF overrun IS NOT INITIAL.
    w_cond_fc-no_out = abap_true.
  ENDIF.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'VBELN_C'.
  w_cond_fc-seltext_l   = 'Imbalance Shifted to'.
  APPEND w_cond_fc TO it_cond_fc .
  "End of insert by XSAP_AB on 5th June
  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'KTEXT'.
  w_cond_fc-seltext_l   = 'CONTRACT DESC'.
  APPEND w_cond_fc TO it_cond_fc .

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'MATNR'.
  w_cond_fc-seltext_l   = 'MATERIAL'.
  APPEND w_cond_fc TO it_cond_fc .

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'MDQ_QUAN'.
  w_cond_fc-seltext_l   = 'DCQ'."'MDQ'.
  APPEND w_cond_fc TO it_cond_fc .


  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'DEL_POINT'.
  w_cond_fc-seltext_l   = 'DELIVERY POINT NOMINATION'.
  w_cond_fc-do_sum    = 'X'.
  APPEND w_cond_fc TO it_cond_fc .

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'DEL_UOM'.
  w_cond_fc-seltext_l   = 'DELIVERY POINT NOMINATION UOM'.
  APPEND w_cond_fc TO it_cond_fc .

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'CORR_NOM'.
  w_cond_fc-seltext_l   = 'Del. Allocated Volume in SM3'."'CORRECTED NOM  INATION'.
  w_cond_fc-do_sum    = 'X'.
  APPEND w_cond_fc TO it_cond_fc .
********************************gcv & ncv
  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'AGCV'.
  w_cond_fc-seltext_l   = 'Del.Allocatrd GCV'."'CORRECTED NOMINATION'.
* w_cond_fc-do_sum     = 'X'.
  APPEND w_cond_fc TO it_cond_fc .

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'DEL_MBG'.
  w_cond_fc-seltext_l   = 'DEL Allocated energy in MBG'.
  w_cond_fc-do_sum    = 'X'.
  APPEND w_cond_fc TO it_cond_fc .

* CLEAR W_COND_FC.
* W_COND_FC-TABNAME     = 'IT_DISPLAY'.
* W_COND_FC-FIELDNAME   = 'ANCV'.
* W_COND_FC-SELTEXT_L   = 'Del.Allocatrd NCV'."'CORRECTED NOMINATION'.
** w_cond_fc-do_sum    = 'X'.
* APPEND W_COND_FC TO IT_COND_FC .

***************************************************************8
  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'CORR_UOM'.
  w_cond_fc-seltext_l   = 'CORRECTED NOMINATION UOM'.
  APPEND w_cond_fc TO it_cond_fc .

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'REDEL_POINT'.
  w_cond_fc-seltext_l   = 'RE-DELIVERY POINT NOMINATION'.
  w_cond_fc-do_sum    = 'X'.
  APPEND w_cond_fc TO it_cond_fc .

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'REDEL_UOM'.
  w_cond_fc-seltext_l   = 'RE-DELIVERY POINT NOMINATION UOM'.
  APPEND w_cond_fc TO it_cond_fc .

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'CORR_NOM1'.
  w_cond_fc-seltext_l   = 'RDel. Allocated Volume in SM3'.
  w_cond_fc-do_sum    = 'X'.
  APPEND w_cond_fc TO it_cond_fc .

*****************************************gcv & ncv *********************  ***8
  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'AGCV1'.
  w_cond_fc-seltext_l   = 'RDel.Allocatrd GCV'."'CORRECTED NOMINATION'.
* w_cond_fc-do_sum     = 'X'.
  APPEND w_cond_fc TO it_cond_fc .

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'REDEL_MBG'.
  w_cond_fc-seltext_l   = 'RDEL Allocated energy in MBG'.
  w_cond_fc-do_sum    = 'X'.
  APPEND w_cond_fc TO it_cond_fc .

* CLEAR W_COND_FC.
* W_COND_FC-TABNAME     = 'IT_DISPLAY'.
* W_COND_FC-FIELDNAME   = 'ANCV1'.
* W_COND_FC-SELTEXT_L   = 'RDel.Allocatrd NCV'."'CORRECTED NOMINATION'.
** w_cond_fc-do_sum    = 'X'.
* APPEND W_COND_FC TO IT_COND_FC .

************************************************************************  ****
  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY'.
  w_cond_fc-fieldname   = 'CORR_UOM1'.
  w_cond_fc-seltext_l   = 'RD CORRECTED NOMINATION UOM'.
  APPEND w_cond_fc TO it_cond_fc .

  IF overrun = 'X'.
    CLEAR: r_d1.
  ENDIF.

  IF overrun <> 'X'.     " For Overrun

    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY'.
    w_cond_fc-fieldname   = 'DAILY'.
    w_cond_fc-seltext_l   = 'DAILY IMBALANCE(+/-)'.
    APPEND w_cond_fc TO it_cond_fc .


    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY'.
    w_cond_fc-fieldname   = 'DAILY_DDS'.
    w_cond_fc-seltext_l   = 'DAILY Difference'.
    APPEND w_cond_fc TO it_cond_fc .

    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY'.
    w_cond_fc-fieldname   = 'DAILY_CUMM'.
    w_cond_fc-seltext_l   = 'CUMMULATIVE Imbalance'.
    APPEND w_cond_fc TO it_cond_fc .

    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY'.
    w_cond_fc-fieldname   = 'DAILY_CUMM_DDS'.
    w_cond_fc-seltext_l   = 'CUMMULATIVE DDS'.
    w_cond_fc-do_sum    = 'X'.
    APPEND w_cond_fc TO it_cond_fc .


    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY'.
    w_cond_fc-fieldname   = 'CHARGABLE_IMB'.
    w_cond_fc-seltext_l   = 'Chargeable Imbalance'.
    APPEND w_cond_fc TO it_cond_fc .

  ENDIF.

  IF ( r_d1 <> 'X' ).   " For Imbalance

    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY'.
    w_cond_fc-fieldname   = 'DAILY_OVERRUN'.
    w_cond_fc-seltext_l   = 'Daily Overrun'.
    w_cond_fc-do_sum    = 'X'.
    APPEND w_cond_fc TO it_cond_fc .


    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY'.
    w_cond_fc-fieldname   = 'DAILY_CHARGE'.
    w_cond_fc-seltext_l   = 'CHARGEABLE Overrun'.
    w_cond_fc-do_sum    = 'X'.
    APPEND w_cond_fc TO it_cond_fc .

  ENDIF.

  IF overrun = 'X'.
    r_d1 = 'X'.
  ENDIF.

ENDFORM.                     " ALV_BUILD_FIELDCAT
*&                                                                     *
*&       Form DISPLAY_ALV
*&                                                                     *
*        text
*                                                                      *
*     > p1         text
* <      p2        text
*                                                                      *
FORM display_alv .
   DATA: lv_diff TYPE menge_d.
   w_repid = sy-repid.
   w_title = w_custname.

  w_layout-info_fieldname = 'LINE_COLOR'. " Added by Nitin Dhamija on 16  .07.2019

  CLEAR w_sortcat.
  w_sortcat-spos = 1.
  w_sortcat-fieldname = 'VBELN'.
  w_sortcat-up = 'X'.
  w_sortcat-subtot = 'X'.
  APPEND w_sortcat TO it_sortcat.

**********SOC By Nitin Dhamija on 16.07.2019**************
  LOOP AT it_display INTO wa_display.
    lv_diff = wa_display-del_mbg - wa_display-redel_mbg.
    IF lv_diff NE wa_display-daily_dds.
      wa_display-line_color = 'C601'.
      MODIFY it_display FROM wa_display INDEX sy-tabix TRANSPORTING line  _color.
    ENDIF.
    CLEAR: wa_display, lv_diff.
  ENDLOOP.
**********EOC By Nitin Dhamija on 16.07.2019**************
*Start of insert by XSAP_AB on 7th Oct,2024
  IF overrun EQ abap_true.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = w_repid
        i_callback_pf_status_set = 'SET_PF_STATUS'
        is_layout                = w_layout
        it_events                = gt_events[]
        it_fieldcat              = it_cond_fc[]
        it_sort                  = it_sortcat
      TABLES
        t_outtab                 = it_display
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
  ELSE.
*End of insert by XSAP_AB on 7th Oct,202
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = w_repid
        i_callback_pf_status_set = 'SET_PF_STATUS'
        i_callback_top_of_page   = 'TOP' "see FORM          "Added by XS  AP_AB on 19th June,2024
*       i_callback_user_command = 'USER_COMMAND'
*       i_grid_title             = w_title
         is_layout                = w_layout               " Added by N  itin Dhamija on 16.07.2019
         it_events                = gt_events[]
         it_fieldcat              = it_cond_fc[]
         it_sort                  = it_sortcat
      TABLES
         t_outtab                 = it_display
      EXCEPTIONS
         program_error            = 1
         OTHERS                   = 2.
  ENDIF.                                                 "Added by XSAP  _AB on 7th Oct,2024

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                     " DISPLAY_ALV
*&                                                                    *
*&       Form BUILD_LAYOUT
*&                                                                    *
*        text
*                                                                     *
*     > p1         text
* <      p2        text
*                                                                     *
FORM build_layout .
   CONSTANTS : lc_x(1) TYPE c VALUE 'X'.
   w_layout-no_input          = lc_x.
   w_layout-colwidth_optimize = lc_x.
   w_layout-info_fieldname =      'LINE_COLOR'.
   w_layout-zebra             = lc_x.
   w_layout-header_text       = 'Imbalance Report'.
   w_layout-box_fieldname     = 'SEL'.

ENDFORM.                     " BUILD_LAYOUT


*&                                                                    *
*&        Form set_pf_status
*&                                                                    *
*         text
*                                                                     *
*          >RT_EXTAB  text
*                                                                     *
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
* IF SY-TCODE = 'ZDM01_ADMIN'.
   IF r_d1 = 'X'.
     SET PF-STATUS 'STANDARD'.
   ELSEIF r_d5 = 'X'.
     SET PF-STATUS 'ZSTANDARD' EXCLUDING 'SAVE'.
   ELSE.
     SET PF-STATUS 'ZSTANDARD'.
   ENDIF.
ENDFORM.                     "SET_PF_STATUS
*&                                                                    *
*&        Form DISPLAY_ALV1
*&                                                                    *
*         text
*                                                                      *
*    > p1         text
* <     p2        text
*                                                                      *
FORM display_alv1 .

*BOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
  IF r_d2 EQ 'X'.
    LOOP AT it_display1 INTO wa_display1.
      wa_display1-sel = 'X'.
      MODIFY it_display1 FROM wa_display1.
    ENDLOOP.
  ENDIF.

  LOOP AT it_display1 ASSIGNING FIELD-SYMBOL(<fs_display>).
    IF <fs_display>-augru = 'G32'.
      CLEAR :     <fs_display>-totalnic1,
                  <fs_display>-totalpic,
                  <fs_display>-totalpic1,
                  <fs_display>-totalnic.

    ENDIF.
  ENDLOOP.


*EOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
  w_repid = sy-repid.
  w_title = w_custname.
  DATA: it_sortcat TYPE slis_t_sortinfo_alv,
         w_sortcat TYPE slis_sortinfo_alv.
  CLEAR w_sortcat.
  IF r_dds IS INITIAL.
    w_sortcat-spos = 1.
    w_sortcat-fieldname = 'VBELN_GRP'.
    w_sortcat-up = 'X'.
    w_sortcat-subtot = 'X'.
    APPEND w_sortcat TO it_sortcat.
  ENDIF.

  "CHARM NO 2000000783 BY ANKUR PRASHAR ON 10.04.2024
  IF sy-tcode EQ 'YRGR102'.
    FREE MEMORY ID 'PENDING_IMB_POST'.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
    EXPORT it_display1[] TO MEMORY ID 'PENDING_IMB_POST'. "#EC CI_FLDEXT  _OK[2215424]
** <- End changes by of Aditi on 19/11/24 for ATC
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = w_repid
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_top_of_page   = 'TOP-OF-PAGE' "see FORM
      i_callback_user_command = 'USER_COMMAND'
      i_grid_title             = w_title
      is_layout                = w_layout
      it_fieldcat              = it_cond_fc1[]
      it_sort                  = it_sortcat
    TABLES
      t_outtab                 = it_display1
    EXCEPTIONS
      program_error             = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.




ENDFORM.                    " DISPLAY_ALV1
*&                                                                   *
*&       Form ALV_BUILD_FIELDCAT1
*&                                                                   *
*        text
*                                                                    *
*    > p1         text
* <     p2        text
*                                                                    *
FORM alv_build_fieldcat1 .

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY1'.
  w_cond_fc-fieldname   = 'KUNNR'. "'KNKLI'.
  w_cond_fc-seltext_l   = 'PAYER'.
  APPEND w_cond_fc TO it_cond_fc1 .


  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY1'.
  w_cond_fc-fieldname   = 'VBELN_GRP'.
  w_cond_fc-seltext_l   = 'GROUP'.
  IF s_vbeln <> ''.
    w_cond_fc-no_out = 'X'.
  ENDIF.
  APPEND w_cond_fc TO it_cond_fc1 .

*vbeln_grp

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY1'.
  w_cond_fc-fieldname   = 'VBELN'.
  w_cond_fc-seltext_l   = 'CONTRACT NUMBER'..
  APPEND w_cond_fc TO it_cond_fc1 .


  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY1'.
  w_cond_fc-fieldname   = 'KTEXT'.
  w_cond_fc-seltext_l   = 'CONTRACT DESC'.
  APPEND w_cond_fc TO it_cond_fc1 .

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY1'.
  w_cond_fc-fieldname   = 'MATNR'.
  w_cond_fc-seltext_l   = 'MATERIAL'.
  APPEND w_cond_fc TO it_cond_fc1 .

  IF r_dds = 'X'.
    CLEAR w_cond_fc.
    w_cond_fc-tabname       = 'IT_DISPLAY1'.
    w_cond_fc-fieldname   = 'TOTALDDS'.
    w_cond_fc-seltext_l   = 'Total DDS CHG'.
    w_cond_fc-do_sum = 'X'.
    APPEND w_cond_fc TO it_cond_fc1.

    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY1'.
    w_cond_fc-fieldname   = 'TOTALDDS1'.
    w_cond_fc-seltext_l   = 'Total DDS CHG SM3'.
    w_cond_fc-do_sum = 'X'.
    APPEND w_cond_fc TO it_cond_fc1.
  ENDIF.
*endif.

  IF r_d5 = 'X' .
    "start of insert by XSAP_AB on 21st June,2024
    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY1'.
    w_cond_fc-fieldname   = 'OPENCUMI'.
    w_cond_fc-seltext_l   = 'Opening Imbalance'.
    w_cond_fc-do_sum = 'X'.
    APPEND w_cond_fc TO it_cond_fc1.
    "End of insert by XSAP_AB on 21st June,2024
    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY1'.
    w_cond_fc-fieldname   = 'TOTALPI'.
    w_cond_fc-seltext_l   = 'Total Positive Imbalance'.
    APPEND w_cond_fc TO it_cond_fc1 .

    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY1'.
    w_cond_fc-fieldname   = 'TOTALNI'.
    w_cond_fc-seltext_l   = 'Total Negative Imbalance'.
    APPEND w_cond_fc TO it_cond_fc1.

    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY1'.
    w_cond_fc-fieldname   = 'TOTALCUMI'.
    w_cond_fc-seltext_l   = 'Cumalative imbalance'.
    APPEND w_cond_fc TO it_cond_fc1.

    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY1'.
    w_cond_fc-fieldname   = 'TOTALPIC'.
    w_cond_fc-seltext_l   = 'Total Positive Imbalance Chg'.
    w_cond_fc-do_sum = 'X'.
    APPEND w_cond_fc TO it_cond_fc1.

    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY1'.
    w_cond_fc-fieldname   = 'TOTALPIC1'.
    w_cond_fc-seltext_l   = 'Total Positive Imbalance Chg SM3'.
    w_cond_fc-do_sum = 'X'.
    APPEND w_cond_fc TO it_cond_fc1.

    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY1'.
    w_cond_fc-fieldname   = 'TOTALNIC'.
    w_cond_fc-seltext_l   = 'Total Negative Imbalance CHG'.
    w_cond_fc-do_sum = 'X'.
    APPEND w_cond_fc TO it_cond_fc1.

    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY1'.
    w_cond_fc-fieldname   = 'TOTALNIC1'.
    w_cond_fc-seltext_l   = 'Total Negative Imbalance CHG SM3'.
    w_cond_fc-do_sum = 'X'.
    APPEND w_cond_fc TO it_cond_fc1.

  ENDIF.

  IF r_d2 = 'X'.

    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY1'.
    w_cond_fc-fieldname   = 'TOTALOVERRUN'.
    w_cond_fc-seltext_l   = 'Total Overrun'.
    APPEND w_cond_fc TO it_cond_fc1.

    CLEAR w_cond_fc.
    w_cond_fc-tabname     = 'IT_DISPLAY1'.
    w_cond_fc-fieldname   = 'TOTALCHARGE'.
    w_cond_fc-seltext_l   = 'Total Overrun Chargeable'.
    w_cond_fc-do_sum = 'X'.
    APPEND w_cond_fc TO it_cond_fc1.

  ENDIF.

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY1'.
  w_cond_fc-fieldname   = 'WT'.
  w_cond_fc-seltext_l   = 'Weighted Average GCV'.
  w_cond_fc-do_sum = 'X'.
  APPEND w_cond_fc TO it_cond_fc1.

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY1'.
  w_cond_fc-fieldname   = 'WT1'.
  w_cond_fc-seltext_l   = 'Weighted Average NCV'.
  w_cond_fc-do_sum = 'X'.
  APPEND w_cond_fc TO it_cond_fc1.

  CLEAR w_cond_fc.
  w_cond_fc-tabname     = 'IT_DISPLAY1'.
  w_cond_fc-fieldname   = 'VBELN1'.
  w_cond_fc-seltext_l   = 'Sales Order Number'.
  APPEND w_cond_fc TO it_cond_fc1 .


ENDFORM.                     " ALV_BUILD_FIELDCAT1
*                                                                  *
*        FORM USER_COMMAND                                         *
*                                                                  *
*          > R_UCOMM                                               *
*          > RS_SELFIELD                                           *
*                                                                  *
FORM user_command USING r_ucomm LIKE sy-ucomm
                  rs_selfield TYPE slis_selfield.
*   BOA by XSAP_Ab2-AV
  LOOP AT it_display1 ASSIGNING <fs_display1>.
    <fs_display1>-sel = 'X'.
  ENDLOOP.
*     EOA by XSAP_AB2-AV

    TABLES: vbak, vbap,vbkd.


    DATA: it_yrva_zcontrcls TYPE TABLE OF yrva_zcontrcls,
          wa_yrva_zcontrcls TYPE yrva_zcontrcls.

    DATA:
*          it_yrva_zcontrcls type table of yrva_zcontrcls,
      lt_yrva_zcontrcls TYPE TABLE OF yrva_zcontrcls,
*          wa_yrva_zcontrcls type yrva_zcontrcls,
      lv_vbeln          TYPE vbeln_nach,
      it_vbfa           TYPE TABLE OF vbfa,
      it_vbfa1          TYPE TABLE OF vbfa,
      lt_vbrk           TYPE STANDARD TABLE OF vbrk,
      ls_vbrk           TYPE vbrk,
      wa_vbfa           TYPE vbfa,
      wa_vbfa1          TYPE vbfa,
      lv_msg            TYPE string.

    DATA is_t001w TYPE t001w.
    DATA l_tabix TYPE sy-tabix.
    DATA l_sr TYPE ysrno.
    "Start of insert by XSAP_AB on 27th May,2024
    IF it_display1 IS NOT INITIAL.
      SELECT * FROM yrva_gta_imb_sft
      INTO TABLE it_gta_imb_sft
      FOR ALL ENTRIES IN it_display1
      WHERE contract_from EQ it_display1-vbeln.
      IF sy-subrc IS NOT INITIAL.
        CLEAR it_gta_imb_sft.
      ENDIF.
    ENDIF.
    "End of insert by XSAP_AB on 27th May,2024

  CASE r_ucomm.
    WHEN 'SUBMIT'.
      DATA: lv_diff TYPE menge_d.
**********SOC By Nitin Dhamija on 16.07.2019**************
      LOOP AT it_display INTO wa_display.
        lv_diff = wa_display-del_mbg - wa_display-redel_mbg.
        IF lv_diff NE wa_display-daily_dds.
          MESSAGE 'Daily imbalance calculation error, please check imbal  ance report' TYPE 'E'.
          LEAVE TO CURRENT TRANSACTION.
        ENDIF.
        CLEAR: wa_display, lv_diff.
      ENDLOOP.
**********EOC By Nitin Dhamija on 16.07.2019**************
*SOC BY MANMOHAN/ABHISHEIK TR:DVRK9A1KR1 DATE:21/07/2025
      SELECT *
        FROM yrg_chg_ovrrun
        INTO TABLE @DATA(it_yrg_chg_ovrrun)
        FOR ALL ENTRIES IN @it_display
        WHERE kunnr EQ @it_display-kunnr
        AND    vbeln EQ @it_display-vbeln
        AND    begda EQ @s_date-low
        AND    endda EQ @s_date-high.

        IF sy-subrc EQ 0.
         LOOP AT it_display INTO wa_display.
           READ TABLE it_yrg_chg_ovrrun INTO DATA(wa) WITH KEY kunnr = wa  _display-kunnr vbeln = wa_display-vbeln.
           IF sy-subrc EQ 0.
              wa_display-vbeln_c = wa-vbeln1.
              MODIFY it_display FROM wa_display.
           ENDIF.
         ENDLOOP.
       ENDIF.
*EOC BY MANMOHAN/ABHISHEIK TR:DVRK9A1KR1 DATE:21/07/2025
****boc charm id 4000003537 technical vikram bajaj functional pratibha d  angwal dt 04.03.2021
       DATA : lt_display TYPE TABLE OF ty_display .
*       data : lt_display_ERROR TYPE TABLE OF ty_display .
       DATA : ls_display TYPE ty_display .
       DATA : msg TYPE char64 .
       DATA : msg1 TYPE char200.
       DATA : msg_strg TYPE string .
       DATA : lt_gt1 TYPE TABLE OF vbak .
       DATA : ls_gt1 TYPE vbak .
       DATA : ret TYPE char64 .
       TYPES : BEGIN OF ty_fin,
                   locid TYPE oij_locid,
                   kunnr TYPE kunnr,
                   dat   TYPE datum,
                END OF ty_fin.
       DATA : lt_fin TYPE TABLE OF ty_fin,
               ls_fin TYPE ty_fin.
       DATA : lt_oijrra TYPE TABLE OF oijrra .
       DATA : ls_oijrra TYPE oijrra .
       DATA : lv_ind(1) .
       TYPES: BEGIN OF esp1_message_wa_type,
                 msgty TYPE sy-msgty,
                 msgv1 TYPE char200,
                 lineno LIKE mesg-zeile,
               END OF esp1_message_wa_type.
**SOC BY YASHU/SHREYOSI CHARM 4000004302: YS:GMS_YRG011N_Non Gail P/L Lo  cation**
       DATA : lt_nom TYPE yrxt_nom .
       DATA: indicator     TYPE char1,
              mess         TYPE char64,
*         PIPELINE TYPE CHAR64,
              lv_non_gail.
**EOC BY YASHU/SHREYOSI CHARM 4000004302: YS:GMS_YRG011N_Non Gail P/L Lo  cation**
       DATA : gt_esp1_message_wa_type TYPE TABLE OF esp1_message_wa_type  .
       DATA : gs_esp1_message_wa_type TYPE esp1_message_wa_type .
       DATA : lv_lineno TYPE mesg-zeile .
       DATA : pipeline TYPE char64 .
       DATA : et_nom TYPE yrxt_nom .
       DATA : w_title1      TYPE lvc_title.
       DATA : w_layout1     TYPE slis_layout_alv.
       DATA it_cond_fc11 TYPE slis_t_fieldcat_alv.
       DATA: w_cond_fc1 TYPE slis_fieldcat_alv.
       w_title1 = 'Error log'.
       SELECT * FROM vbak INTO TABLE lt_gt1 FOR ALL ENTRIES IN it_display  WHERE vbeln = it_display-vbeln .
       IF sy-subrc = 0 .

        SELECT * FROM oijrra INTO TABLE lt_oijrra FOR ALL ENTRIES IN lt_  gt1 WHERE cparid = lt_gt1-kunnr AND tsyst LIKE 'NGPL%' AND delind = ''.
         LOOP AT it_display INTO wa_display .
           ls_fin-kunnr = wa_display-kunnr.
           READ TABLE lt_gt1 INTO ls_gt1 WITH KEY vbeln = wa_display-vbe  ln .
           IF sy-subrc = 0 AND ls_gt1-auart = 'ZGT1'.
             ls_fin-dat = wa_display-date.
             READ TABLE lt_oijrra INTO ls_oijrra WITH KEY cparid = ls_gt1  -kunnr .
             IF sy-subrc = 0.
               ls_fin-locid = ls_oijrra-locid.
               APPEND ls_fin TO lt_fin.
             ENDIF.
           ENDIF.
           CLEAR wa_display .
         ENDLOOP .
      ENDIF .
*      sort lt_display[] by date .
*      delete ADJACENT DUPLICATES FROM lt_display COMPARING date .
      SORT lt_fin BY dat locid kunnr.
      DELETE ADJACENT DUPLICATES FROM lt_fin .
      BREAK pm02.
      LOOP AT lt_fin INTO ls_fin .
**SOC BY YASHU/SHREYOSI CHARM 4000004302: YS:GMS_YRG011N_Non Gail P/L Lo  cation**
        "for gail/non gail
         CLEAR:indicator,mess,pipeline,lt_nom,lv_non_gail,msg1.
         CALL FUNCTION 'YRX_CHK_NG_TKT_STATUS'
           EXPORTING
             gas_day        = ls_fin-dat
             locid          = ls_fin-locid
*            VBELN          =
*            DETAIL_CHK     =
           IMPORTING
             indicator      = indicator
             msg            = mess
             pipeline       = pipeline
           TABLES
             et_nominations = lt_nom.
         IF pipeline IS INITIAL .
**EOC BY YASHU/SHREYOSI CHARM 4000004302: YS:GMS_YRG011N_Non Gail P/L Lo  cation**

           "BOC 40000007823 TECHNICAL ANKUR PRASHAR FUNCTIONAL SHREYOSI D  E ON 27.02.2024
           "BOC uncomment 2000000783: TECHNICAL ravi FUNCTIONAL SHREYOSI  DE ON 24.04.2024
** -> Begin of changes by of Aditi on 19/11/24 for ATC
*           SELECT SINGLE tsyst FROM oijrra INTO @DATA(tsyst) WHERE locid  = @ls_fin-locid AND tsyst LIKE 'NGPL%' AND delind NE 'X'."DELIND Added  by Amit Shukla / Shreyosi Ma'am on 05/03/24 Charm No-4000007823
           SELECT tsyst FROM oijrra INTO @DATA(tsyst) UP TO 1 ROWS WHERE  locid = @ls_fin-locid AND tsyst LIKE 'NGPL%' AND delind NE 'X' ORDER BY  PRIMARY KEY."DELIND Added by Amit Shukla / Shreyosi Ma'am on 05/03/24 C  harm No-4000007823
           ENDSELECT.
** <- End changes by of Aditi on 19/11/24 for ATC
           IF tsyst IS NOT INITIAL.
             SELECT SINGLE * FROM yrga_me_chk_tsyt INTO @DATA(tsyt) WHERE  tsyst = @tsyst.
           ENDIF.
          IF tsyt IS NOT INITIAL.
            "EOC uncomment 2000000783: TECHNICAL ravi FUNCTIONAL SHREYO  SI DE ON 24.04.2024
            "EOC 40000007823 TECHNICAL ANKUR PRASHAR FUNCTIONAL SHREYOSI DE ON 27.02.2024
            CALL FUNCTION 'YRX_CHK_ME_OD1'
              EXPORTING
                step       = 'ME'
                locid      = ls_fin-locid
                gday       = ls_fin-dat
              IMPORTING
                return_msg = msg
                ind        = lv_ind.

            " ELSE. "BOC 40000007823 TECHNICAL ANKUR PRASHAR FUNCTIONAL  SHREYOSI DE ON 27.02.2024

          ELSEIF tsyt IS INITIAL.    ""SOC 2000000783: TECHNICAL ravi FUN  CTIONAL SHREYOSI DE ON 02.05.2024
            lv_ind = 'S'.
          ENDIF.
          CLEAR:tsyst,
                 tsyt. "EOC 40000007823 TECHNICAL ANKUR PRASHAR FUNCTIONA  L SHREYOSI DE ON 27.02.2024

          IF lv_ind = 'S'   .

            CALL FUNCTION 'YRX_CHK_ME_OD1'
              EXPORTING
                step       = 'OD'
                locid      = ls_fin-locid
                gday       = ls_fin-dat
              IMPORTING
                return_msg = msg
                ind        = lv_ind.
            IF lv_ind = 'E'.

              lv_lineno = lv_lineno + 1 .
*            GS_ESP1_MESSAGE_WA_TYPE-msgid = 'E4'.
*            GS_ESP1_MESSAGE_WA_TYPE-msgno = '000'.
              gs_esp1_message_wa_type-msgty = 'E'.
              gs_esp1_message_wa_type-msgv1 = msg.
              gs_esp1_message_wa_type-lineno = lv_lineno.
              APPEND gs_esp1_message_wa_type TO gt_esp1_message_wa_type.
            ENDIF .
          ELSE .
            lv_lineno = lv_lineno + 1 .
*            GS_ESP1_MESSAGE_WA_TYPE-msgid = 'E4'.
*            GS_ESP1_MESSAGE_WA_TYPE-msgno = '000'.
            gs_esp1_message_wa_type-msgty = 'E'.
            gs_esp1_message_wa_type-msgv1 = msg.
            gs_esp1_message_wa_type-lineno = lv_lineno.
            APPEND gs_esp1_message_wa_type TO gt_esp1_message_wa_type.
          ENDIF .
          CLEAR : lv_ind , msg , ls_display .


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

              lv_lineno = lv_lineno + 1 .
*              GS_ESP1_MESSAGE_WA_TYPE-msgid = 'E4'.
*              GS_ESP1_MESSAGE_WA_TYPE-msgno = '000'.
              gs_esp1_message_wa_type-msgty = 'E'.
              gs_esp1_message_wa_type-msgv1 = msg1.
              gs_esp1_message_wa_type-lineno = lv_lineno.
              APPEND gs_esp1_message_wa_type TO gt_esp1_message_wa_type.
            ENDIF .

          ENDIF.

        ENDLOOP .


        IF gt_esp1_message_wa_type IS INITIAL .


          LOOP AT   lt_fin INTO ls_fin .

           SELECT SINGLE addrnum FROM oifspbl INTO @DATA(addn) WHERE pbln  r = @ls_fin-locid.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
*           SELECT SINGLE * FROM adrc INTO @DATA(wadrc) WHERE addrnumber  = @addn AND street = 'NON-GAIL'.
           SELECT * FROM adrc INTO @DATA(wadrc) UP TO 1 ROWS WHERE addrn  umber = @addn AND street = 'NON-GAIL' ORDER BY PRIMARY KEY.
           ENDSELECT.
** <- End changes by of Aditi on 19/11/24 for ATC

            IF wadrc-street NE 'NON-GAIL'.
              CALL FUNCTION 'YRX_CHK_NG_TKT_STATUS'
                EXPORTING
                  gas_day        = ls_fin-dat
                  locid          = ls_fin-locid
                  detail_chk     = 'X'
                IMPORTING
                  indicator      = lv_ind
                  msg            = msg
                  pipeline       = pipeline
                TABLES
                  et_nominations = et_nom.

              "IF PIPELINE IS INITIAL.
              IF lv_ind = '1' OR lv_ind = '3' OR lv_ind = '4' .
                lv_lineno = lv_lineno + 1 .
                gs_esp1_message_wa_type-msgty = 'E'.
                gs_esp1_message_wa_type-msgv1 = msg.
                gs_esp1_message_wa_type-lineno = lv_lineno.
                APPEND gs_esp1_message_wa_type TO gt_esp1_message_wa_type.
              ENDIF .
              CLEAR : gs_esp1_message_wa_type ,ls_fin .


            ELSEIF wadrc-street = 'NON-GAIL'.
              CLEAR: msg1.
              CALL FUNCTION 'YRX_CHK_CUST_COMP_TKT_STATUS'
                EXPORTING
                  i_kunnr         = ls_fin-kunnr
                  i_date          = ls_fin-dat
                  ticket_complete = 'X'
                IMPORTING
                  indicator       = lv_ind
                  msg             = msg1.

              IF lv_ind = '3' . "OR LV_IND = '2' OR LV_IND = '4' .
                lv_lineno = lv_lineno + 1 .
                gs_esp1_message_wa_type-msgty = 'E'.
                gs_esp1_message_wa_type-msgv1 = msg1.
                gs_esp1_message_wa_type-lineno = lv_lineno.
                APPEND gs_esp1_message_wa_type TO gt_esp1_message_wa_type.
              ENDIF .

            ENDIF.

            CLEAR: addn, wadrc,ls_fin .
          ENDLOOP .
        ENDIF .

        IF gt_esp1_message_wa_type IS NOT INITIAL .
          CONSTANTS : lc_x(1) TYPE c VALUE 'X'.
          w_layout1-colwidth_optimize = lc_x.
          w_layout1-zebra             = lc_x.
          w_layout1-header_text       = 'logs'.

          CLEAR w_cond_fc.

          w_cond_fc-fieldname   = 'MSGTY'.
          w_cond_fc-seltext_l   = 'Message Type'.
          APPEND w_cond_fc TO it_cond_fc11 .

          CLEAR w_cond_fc.
          w_cond_fc-fieldname   = 'LINENO'.
          w_cond_fc-seltext_l   = 'Line'.
          APPEND w_cond_fc TO it_cond_fc1 .
          CLEAR w_cond_fc.

          w_cond_fc-fieldname   = 'MSGV1'.
          w_cond_fc-seltext_l   = 'Message'.
          APPEND w_cond_fc TO it_cond_fc11 .
*
         CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
           EXPORTING
*            i_callback_program       = w_repid
*            i_callback_top_of_page   = 'TOP-OF-PAGE' "see FORM
             i_grid_title = w_title1
             is_layout     = w_layout1
             it_fieldcat   = it_cond_fc11[]
           TABLES
             t_outtab      = gt_esp1_message_wa_type
           EXCEPTIONS
             program_error = 1
             OTHERS        = 2.
         "Start of insert by XSAP_AB on 24th Sept,2024
        READ TABLE gt_esp1_message_wa_type TRANSPORTING NO FIELDS WITH K  EY msgty = 'E'.
        IF sy-subrc IS INITIAL.
          LEAVE TO SCREEN 0.
        ENDIF.
        "End of insert by XSAP_AB on 24th Sept,2024
      ELSE .
*        ENDIF .
****eoc charm id 4000003537 technical vikram bajaj functional pratibha d  angwal dt 04.03.2021
        IF it_display1[] IS NOT INITIAL.
          SORT it_yrg_cumm_imb_temp BY kunnr knkli yy_contract endda tim  estamp DESCENDING.
* get the uom in which price is maintained
          SELECT * FROM a978 INTO CORRESPONDING FIELDS OF TABLE it_978
              FOR ALL ENTRIES IN it_display1
               WHERE oicontnr EQ it_display1-vbeln
               AND auart_sd = 'ZNT3'
               AND kschl EQ 'ZPR3'
               AND datab LE s_date-low
               AND datbi GE s_date-high.
          IF sy-subrc EQ 0 .
             SELECT * FROM konp INTO CORRESPONDING FIELDS OF TABLE it_ko  np
               FOR ALL ENTRIES IN it_978
               WHERE knumh = it_978-knumh.
          ENDIF.

          SELECT vbeln matnr INTO TABLE it_matnr FROM vbap
             FOR ALL ENTRIES IN it_display1
             WHERE vbeln EQ it_display1-vbeln .
          LOOP AT it_matnr .
             READ TABLE it_display1 INTO wa_display1 WITH KEY vbeln = it_  matnr-vbeln.
             IF sy-subrc EQ 0 .
               it_matnr-kunnr = wa_display1-kunnr.
               MODIFY it_matnr.
             ENDIF.
          ENDLOOP.
          IF it_matnr[] IS NOT INITIAL.
             SELECT * FROM a305
               INTO CORRESPONDING FIELDS OF TABLE it_a305
               FOR ALL ENTRIES IN it_matnr
               WHERE kunnr EQ it_matnr-kunnr
               AND kschl EQ 'ZPR3'
               AND datab LE s_date-low
               AND datbi GE s_date-high
               AND matnr EQ it_matnr-matnr.
             IF sy-subrc EQ 0 .
               SELECT * FROM konp APPENDING CORRESPONDING FIELDS OF TABL  E it_konp
                 FOR ALL ENTRIES IN it_a305
               WHERE knumh = it_a305-knumh.
             ENDIF.
          ENDIF.

          ENDIF.
        ENDIF .
        IF r_d2 = 'X'.
          LOOP AT it_display1 INTO wa_display1." WHERE sel = 'X'.
            IF wa_display1-totalcharge > 0 AND wa_display1-vbeln1 IS INIT  IAL.
            CLEAR: wa_display11,wa_vbak1,wa_display12.
            CLEAR : es_oikimport,ls_oikload,ls_oikexport.
            REFRESH lt_oikload.

            es_oikimport-applic = 3.
            es_oikimport-fumode = 1.
*ls_oikimport-LIDFUNC = 'OIK_SD_CALLOFF_CREATE'.
            es_oikimport-lidfuncnam = 'OIK_SD_CALLOFF_CREATE'.
            es_oikimport-vbeln = wa_display1-vbeln.
            es_oikimport-auart = 'ZNT7'."vbak-auart'.
            es_oikimport-lisof = 'X'.
            ls_oikload-doctype_s   = 'G'.
            ls_oikload-docno_s   = wa_display1-vbeln.

            ls_oikload-kwmeng               = 1000.
*ls_oikload-VRKME                  = WA_DISPLAY1-."'SM3'.
            ls_oikload-docno                   = wa_display1-vbeln.
            ls_oikload-itemno             =     '0000000010'.
            ls_oikload-doctype =                                        'G  '.
            ls_oikload-auart = 'ZNT7'.
*   ls_oikload-bstdk = s_date-low.

            READ TABLE it_oij_el_cp_layt INTO wa_oij_el_cp_layt WITH KE  Y vbeln = wa_display1-vbeln.
            IF sy-subrc = 0.
              ls_oikload-vrkme                  = wa_oij_el_cp_layt-mdq_  uom.
            ENDIF.
            APPEND ls_oikload TO lt_oikload.
            CALL FUNCTION 'OIK_SD_CALLOFF_CREATE'
              EXPORTING
                i_oikimport = es_oikimport
              IMPORTING
                e_oikexport = ls_oikexport
              TABLES
                t_oikload    = lt_oikload.

            IF ls_oikexport-msgty <> 'E'.
              wa_display1-vbeln1 = ls_oikexport-kdauf.
              MODIFY it_display1 FROM wa_display1 .
              PERFORM additional_qty USING wa_display1.
              PERFORM change_so USING wa_display1-vbeln1.
              wa_display1-vbeln1 = ls_oikexport-kdauf.      "Added by XSA  P_AB on 29.10.2024 12:11:08
            ELSE.
              "Start of insert by XSAP_AB on 24th Sept,2024
              MESSAGE ID ls_oikexport-msgid
                     TYPE ls_oikexport-msgty
                   NUMBER ls_oikexport-msgno
                     WITH ls_oikexport-msgv1
                          ls_oikexport-msgv2
                          ls_oikexport-msgv3
                          ls_oikexport-msgv4.
              "End of insert by XSAP_AB on 24th Sept,2024
            ENDIF.
          ELSE.
            REFRESH:it_yrva_zcontrcls, lt_yrva_zcontrcls,it_vbfa.
            CLEAR:wa_yrva_zcontrcls,lv_vbeln,wa_vbfa.
            SELECT * INTO TABLE it_vbfa
*            INTO lv_vbeln
              FROM vbfa
             WHERE vbelv = wa_display1-vbeln1
              AND vbtyp_n <> 'N'.
            IF sy-subrc = 0.
              SELECT * FROM vbrk INTO TABLE lt_vbrk
                      FOR ALL ENTRIES IN it_vbfa
                      WHERE vbeln = it_vbfa-vbeln
                        AND fksto NE 'X'.
              IF sy-subrc EQ 0.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
                READ TABLE lt_vbrk INTO ls_vbrk INDEX 1. "#EC CI_NOORDER
** <- End changes by of Aditi on 19/11/24 for ATC
                IF sy-subrc EQ 0.
                   lv_vbeln = ls_vbrk-vbeln.
                ENDIF.
              ENDIF.
              REFRESH lt_vbrk.
            ENDIF.

            IF lv_vbeln IS NOT INITIAL."sy-subrc = 0.
              CONCATENATE 'Cannot modify, Invoice already generated(INV. NO-' lv_vbeln ')'
              INTO lv_msg.
              MESSAGE lv_msg TYPE 'E'.
            ELSE.
              SELECT *
                INTO TABLE it_yrva_zcontrcls
                FROM yrva_zcontrcls
               WHERE yyvbeln = wa_display1-vbeln1.
*BOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
              READ TABLE it_yrva_zcontrcls INTO wa_yrva_zcontrcls WITH K  EY yyvbeln = wa_display1-vbeln1 yyclause_id = '12'.
              IF sy-subrc EQ 0.
                wa_yrva_zcontrcls-yyzmeng = wa_display1-totalcharge.
                MODIFY yrva_zcontrcls FROM wa_yrva_zcontrcls.
                CLEAR: wa_yrva_zcontrcls.
              ELSE.
                wa_yrva_zcontrcls-yyvbeln = wa_display1-vbeln1.
                wa_yrva_zcontrcls-yyposnr = '000010'.
                wa_yrva_zcontrcls-yysrno = '01'.
                wa_yrva_zcontrcls-yyclause_id = '12'.
                wa_yrva_zcontrcls-yyzmeng = wa_display1-totalcharge.
                wa_yrva_zcontrcls-yyzieme = ls_oikload-vrkme .
                APPEND wa_yrva_zcontrcls TO it_yrva_zcontrcls.
                MODIFY yrva_zcontrcls FROM wa_yrva_zcontrcls.
                CLEAR: wa_yrva_zcontrcls.
              ENDIF.

*              lt_yrva_zcontrcls[] = it_yrva_zcontrcls[].
*              LOOP AT it_yrva_zcontrcls INTO wa_yrva_zcontrcls .
*                IF wa_yrva_zcontrcls-yyclause_id = '10'.
*                  wa_yrva_zcontrcls-yyzmeng = wa_display1-totalpic .
*                ELSEIF wa_yrva_zcontrcls-yyclause_id = '11'.
*                  wa_yrva_zcontrcls-yyzmeng = wa_display1-totalnic * -1  .
*                ELSEIF wa_yrva_zcontrcls-yyclause_id = '12'.
*                  wa_yrva_zcontrcls-yyzmeng = wa_display1-totalcharge.
*                ELSEIF wa_yrva_zcontrcls-yyclause_id = '15'.
*                  wa_yrva_zcontrcls-yyzmeng = wa_display1-totaldds.
*                ENDIF.
*                  MODIFY it_yrva_zcontrcls FROM wa_yrva_zcontrcls.
*               ENDLOOP.
*               IF lt_yrva_zcontrcls[] = it_yrva_zcontrcls[].
*               ELSE.
*                  MODIFY yrva_zcontrcls FROM TABLE it_yrva_zcontrcls.
*                  COMMIT WORK AND WAIT.
*               ENDIF.
               IF ( wa_display1-totalcharge EQ 0 ) AND wa_display1-vbeln1  IS NOT INITIAL.
                  ls_order_header_inx-updateflag = 'D'.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
                  CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK  [2438131]
** <- End changes by of Aditi on 19/11/24 for ATC
                    EXPORTING
                      salesdocument    = wa_display1-vbeln1
                      order_header_inx = ls_order_header_inx
                    TABLES
                      return           = lt_return.
                  READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY typ  e = 'E'.
                  IF sy-subrc NE 0.
                    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                      EXPORTING
                         wait = 'X'.
                    IF sy-subrc EQ 0.
                      " Clear SO from Table
                      CLEAR wa_display1-vbeln1.
                      MODIFY it_display1 FROM wa_display1-vbeln1.
                    ENDIF.
*    Clear VBELN from DISP1 table so that it gets cleared from Cumm Imb t  able which gets updated down the line in code.
                  ELSE.
                    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
                  ENDIF.
               ELSE.
                  IF wa_display1-totalcharge IS NOT INITIAL AND r_d2 = 'X'  .
*               wa_yrva_zcontrcls-yyvbeln = wa_display12-vbeln1.
*                    wa_yrva_zcontrcls-yyvbeln = wa_display1-vbeln1.
*                    wa_yrva_zcontrcls-yyposnr = '000010'.
*                    wa_yrva_zcontrcls-yysrno = l_sr.
*                    wa_yrva_zcontrcls-yyclause_id = '12'.
                    wa_yrva_zcontrcls-yyzmeng = wa_display1-totalcharge.
*                    wa_yrva_zcontrcls-yyzieme = ls_oikload-vrkme .
*                    APPEND wa_yrva_zcontrcls TO it_yrva_zcontrcls.
                    MODIFY yrva_zcontrcls FROM wa_yrva_zcontrcls.
                    CLEAR: wa_yrva_zcontrcls.
*                    l_sr = l_sr + 1.
                  ENDIF.
               ENDIF.
             ENDIF.
           ENDIF.
           wa_yrg_chg_ovrrun-kunnr = wa_display1-kunnr.
           wa_yrg_chg_ovrrun-vbeln = wa_display1-vbeln.
           wa_yrg_chg_ovrrun-vbeln1 = wa_display1-vbeln1.
           wa_yrg_chg_ovrrun-begda = s_date-low.
           wa_yrg_chg_ovrrun-endda = s_date-high.
           wa_yrg_chg_ovrrun-totaloverrun = wa_display1-totaloverrun.
           wa_yrg_chg_ovrrun-totalcharge = wa_display1-totalcharge.
           CALL FUNCTION 'PK_DATE_TIME_INTO_TIMESTAMP'
             EXPORTING
               iv_pkldt = sy-datum
               iv_pkluz = sy-uzeit
               is_t001w = is_t001w
             IMPORTING
               ev_pktim = wa_yrg_chg_ovrrun-timestamp.
*           APPEND wa_yrg_chg_ovrrun TO it_yrg_chg_ovrrun. "Commented by  XSAP_AB on 24th Sept,2024
           MODIFY yrg_chg_ovrrun FROM wa_yrg_chg_ovrrun.          "Added by  XSAP_AB on 24th Sept,2024
           CLEAR wa_yrg_chg_ovrrun.                               "Added by  XSAP_AB on 24th Sept,2024
         ENDLOOP.
*         IF it_yrg_chg_ovrrun[] IS NOT INITIAL. "Moved Modify inside th  e loop above.
*           MODIFY yrg_chg_ovrrun FROM TABLE it_yrg_chg_ovrrun.
*         ENDIF.
       ELSEIF ( r_d5 = 'X' OR r_dds = 'X' ) AND gt_esp1_message_wa_type I  S INITIAL.
*         IF w_group_imbalance = 'X' .
         IF r_d5 = 'X'.
           READ TABLE it_vbak1 INTO wa_vbak1 WITH KEY abrvw = 'Z01' .
           IF sy-subrc EQ 0 .
             READ TABLE it_display1 INTO wa_display1 WITH KEY vbeln = wa  _vbak1-vbeln.
             IF sy-subrc EQ 0.
*    Get Master Contract
               READ TABLE it_display1 INTO wa_display12 WITH KEY vbeln =  wa_display1-vbeln_grp .
*    BOA by XSAP_AB2-AV - Check previous postings
               IF sy-subrc EQ 0.
                  CLEAR lv_exit_processing.
                  IF NOT sy-batch EQ 'X'        "Added by XSAP_AB on 20th S  ept,2024 - Suprresing popup
                     OR NOT sy-tcode EQ 'YRGR102'. "Added by XSAP_AB on 04  th Oct,2024 - Suprresing popup for YRGR102
                    CALL METHOD ycl_gms_report_validations=>check_imbalanc  e_posting
                      EXPORTING
                         i_contract        = wa_display12-vbeln    " Sales  Contract Number
                         i_customer        = wa_display12-kunnr    " Custom  er Number
                         i_to_date         = s_date-high           " To Dat  e
                      IMPORTING
                         e_exit_processing = lv_exit_processing.    " Exit  further processing
                    IF lv_exit_processing IS NOT INITIAL.
                      LEAVE LIST-PROCESSING.
                    ENDIF.
                  ENDIF.            "Added by XSAP_AB on 20th Sept,2024 - S  uprresing popup
               ENDIF.
*EOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
               CHECK lv_exit_processing IS INITIAL.
*    EOA by XSAP_AB2-AV
               " Begin of change on 27/01/2015 for sales order imbalance
               IF ( wa_display12-totalpic > 0 OR wa_display12-totalnic <  0 ) AND wa_display12-vbeln1 IS INITIAL. "09.02.2018
                 " End of change
                CLEAR : es_oikimport,ls_oikload,ls_oikexport.
                REFRESH lt_oikload.

                es_oikimport-applic = 3.
                es_oikimport-fumode = 1.
*ls_oikimport-LIDFUNC = ''.
                es_oikimport-lidfuncnam = 'OIK_SD_CALLOFF_CREATE'.
                es_oikimport-vbeln = wa_display1-vbeln.
                es_oikimport-auart = 'ZNT7'."vbak-auart'.
                es_oikimport-lisof = 'X'.
                ls_oikload-doctype_s   = 'G'.
                ls_oikload-docno_s   = wa_display1-vbeln.

                 ls_oikload-kwmeng              = 1000.
*ls_oikload-VRKME                   = WA_DISPLAY1-."'SM3'.
                 ls_oikload-docno                   = wa_display1-vbeln.
                 ls_oikload-itemno            =     '000000001 0'.
                 ls_oikload-doctype =  'G'.
                 ls_oikload-auart = 'ZNT7'.
*    ls_oikload-bstdk = s_date-low.

                READ TABLE it_oij_el_cp_layt INTO wa_oij_el_cp_layt WIT  H KEY vbeln = wa_display1-vbeln.
                IF sy-subrc = 0.
                  ls_oikload-vrkme                  = wa_oij_el_cp_layt-  mdq_uom.
                ENDIF.
                APPEND ls_oikload TO lt_oikload.
                CALL FUNCTION 'OIK_SD_CALLOFF_CREATE'
                  EXPORTING
                    i_oikimport = es_oikimport
                  IMPORTING
                    e_oikexport = ls_oikexport
                  TABLES
                    t_oikload    = lt_oikload.

                IF ls_oikexport-msgty <> 'E'.

                  wa_display1-vbeln1 = ls_oikexport-kdauf.
                  MODIFY TABLE it_display1 FROM wa_display1 .
                  wa_display1-totalpic = wa_display12-totalpic .
                  wa_display1-totalnic = wa_display12-totalnic .
                  wa_display1-totalpic1 = wa_display12-totalpic1 .
                  wa_display1-totalnic1 = wa_display12-totalnic1 .
                  PERFORM additional_qty USING wa_display1.
                  PERFORM change_so USING wa_display1-vbeln1.
                  LOOP AT it_display1 INTO wa_display1 WHERE sel = 'X' A  ND ( vbeln = wa_display12-vbeln OR vbeln_grp EQ wa_display12-vbeln ).
                    wa_display1-vbeln1 = ls_oikexport-kdauf.
                    CLEAR wa_display1-sel.
                    MODIFY it_display1 FROM wa_display1 .
                  ENDLOOP.
                ELSE.
*SOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala  nce TR:DVRK9A1KR0
                  IF NOT sy-batch EQ 'X' AND NOT sy-tcode EQ 'YRGR102'.
*EOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala  nce TR:DVRK9A1KR0
                    MESSAGE ID ls_oikexport-msgid
                           TYPE ls_oikexport-msgty
                           NUMBER ls_oikexport-msgno
                           WITH ls_oikexport-msgv1
                                ls_oikexport-msgv2
                                ls_oikexport-msgv3
                                ls_oikexport-msgv4.
                  ENDIF.
                ENDIF.
              ELSE.                                                "09..  02.2018
                REFRESH:it_yrva_zcontrcls, lt_yrva_zcontrcls,it_vbfa.
                CLEAR:wa_yrva_zcontrcls,lv_vbeln,wa_vbfa.

                CLEAR : it_vbfa[], lt_vbrk[], lv_vbeln.
                SELECT * FROM vbfa INTO TABLE it_vbfa
                          WHERE vbelv = wa_display12-vbeln1
                            AND ( vbtyp_n = 'P'
                               OR vbtyp_n = 'M' ).
                IF sy-subrc = 0.
                  SELECT * FROM vbrk INTO TABLE lt_vbrk
                          FOR ALL ENTRIES IN it_vbfa
                          WHERE vbeln = it_vbfa-vbeln
                            AND fksto NE 'X'.
                  IF sy-subrc EQ 0.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
                    READ TABLE lt_vbrk INTO ls_vbrk INDEX 1. "#EC CI_NOO  RDER
** <- End changes by of Aditi on 19/11/24 for ATC
                    IF sy-subrc EQ 0.
                       lv_vbeln = ls_vbrk-vbeln.
                    ENDIF.
                  ENDIF.
                ENDIF.
*   EOC by XSAP_AB2
                IF lv_vbeln IS NOT INITIAL."sy-subrc = 0.
                  CONCATENATE 'Cannot modify, Invoice already generated( INV. NO-' lv_vbeln ')'
                  INTO lv_msg.
*SOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala  nce TR:DVRK9A1KR0
                  IF NOT sy-batch EQ 'X' AND NOT sy-tcode EQ 'YRGR102'.
*EOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala  nce TR:DVRK9A1KR0
                    MESSAGE lv_msg TYPE 'E'.
                  ENDIF.
                ELSE.

*   BOC by XSAP_AB2-AV- Update Tables and SO
                  IF wa_display12-vbeln1 IS NOT INITIAL.
                    wa_display1-vbeln1 = wa_display12-vbeln1.
                    CLEAR wa_display1-sel.
                    MODIFY TABLE it_display1 FROM wa_display1 TRANSPORTI  NG sel vbeln1.
                    wa_display1-totalpic = wa_display12-totalpic .
                    wa_display1-totalnic = wa_display12-totalnic .
                    wa_display1-totalpic1 = wa_display12-totalpic1 .
                    wa_display1-totalnic1 = wa_display12-totalnic1 .
                    PERFORM additional_qty USING wa_display1.
                    PERFORM change_so USING wa_display1-vbeln1.
                    LOOP AT it_display1 INTO wa_display1 WHERE sel = 'X' AND ( vbeln = wa_display12-vbeln OR vbeln_grp EQ wa_display12-vbeln ).
                      CLEAR wa_display1-sel.
                      MODIFY   it_display1 FROM wa_display1 .
                    ENDLOOP.
                  ENDIF.

*    If positive and Negative Imb have become zero then delete alredy cre  ated Sales Order
                     IF ( wa_display12-totalpic IS INITIAL AND wa_display12  -totalnic IS INITIAL ) AND wa_display12-vbeln1 IS NOT INITIAL.
                       lv_salesdocument = wa_display12-vbeln1 .
                       ls_order_header_inx-updateflag = 'D'.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
                       CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAG  E_OK[2438131]
** <- End changes by of Aditi on 19/11/24 for ATC
                         EXPORTING
                            salesdocument    = lv_salesdocument
                            order_header_inx = ls_order_header_inx
                         TABLES
                            return           = lt_return.
                       READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY  type = 'E'.
                       IF sy-subrc NE 0.
                         CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                            EXPORTING
                              wait = 'X'.
                         IF sy-subrc EQ 0.
                           " Clear SO from Table
                            LOOP AT it_display1 ASSIGNING <fs_display1>.
                              CLEAR <fs_display1>-vbeln1.
                            ENDLOOP.
                         ENDIF.
*    Clear VBELN from DISP1 table so that it gets cleared from Cumm Imb t  able which gets updated down the line in code.
                       ELSE.
                         CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
                       ENDIF.
                     ENDIF.
*    EOC by XSAP_AB2-AV
                  ENDIF.
                ENDIF.
                LOOP AT it_display1 INTO wa_display1 WHERE sel = 'X' AND ( vbeln = wa_display12-vbeln OR vbeln_grp EQ wa_display12-vbeln ).
                  CLEAR wa_display1-sel.
                  MODIFY it_display1 FROM wa_display1 .
                ENDLOOP."09.02.2018
*              create an entry in imbalance table
                LOOP AT it_display1 INTO wa_display1 WHERE sel NE 'X' .
                  PERFORM entry_imb_table USING wa_display1.
                ENDLOOP.
              ELSE.
*SOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala  nce TR:DVRK9A1KR0
                IF NOT sy-batch EQ 'X' AND NOT sy-tcode EQ 'YRGR102'.
*EOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala  nce TR:DVRK9A1KR0
                  MESSAGE TEXT-003 TYPE 'E' .
                ENDIF.
              ENDIF.
           ELSE.
*              MESSAGE text-002 TYPE 'E' .
           ENDIF.
*        ELSE.

*   BOA by XSAP_AB2-AV - Check previous postings
           LOOP AT it_display1 INTO wa_display1 WHERE sel = 'X'.
             CLEAR lv_exit_processing.
             IF NOT sy-batch EQ 'X'         "Added by XSAP_AB on 20th Sept  ,2024 - Suprresing popup
                OR NOT sy-tcode EQ 'YRGR102'. "Added by XSAP_AB on 04th  Oct,2024 - Suprresing popup for YRGR102
               CALL METHOD ycl_gms_report_validations=>check_imbalance_po  sting
                 EXPORTING
                    i_contract        = wa_display1-vbeln    " Sales Contr  act Number
                    i_customer        = wa_display1-kunnr    " Customer Nu  mber
                    i_to_date         = s_date-high          " To Date
                 IMPORTING
                    e_exit_processing = lv_exit_processing. " Exit furthe  r processing
               IF lv_exit_processing IS NOT INITIAL.
                 lv_futher_processing = 'X'.
               ELSE.
                 EXIT.
               ENDIF.
             ENDIF. "Added by XSAP_AB on 20th Sept,2024 - Suprresing pop  up
           ENDLOOP.

          CHECK lv_futher_processing IS INITIAL.
*   EOA by XSAP_AB2-AV
          LOOP AT it_display1 INTO wa_display1 WHERE sel = 'X'.

            IF wa_display1-vbeln1 IS INITIAL AND ( wa_display1-totalpic > 0 OR wa_display1-totalnic < 0 ).
              CLEAR: wa_display11,wa_vbak1,wa_display12.
              CLEAR : es_oikimport,ls_oikload,ls_oikexport.
              REFRESH lt_oikload.

              es_oikimport-applic = 3.
              es_oikimport-fumode = 1.
*ls_oikimport-LIDFUNC = 'OIK_SD_CALLOFF_CREATE'.
              es_oikimport-lidfuncnam = 'OIK_SD_CALLOFF_CREATE'.
              es_oikimport-vbeln = wa_display1-vbeln.
              es_oikimport-auart = 'ZNT7'."vbak-auart'.
              es_oikimport-lisof = 'X'.
              ls_oikload-doctype_s   = 'G'.
              ls_oikload-docno_s   = wa_display1-vbeln.

               ls_oikload-kwmeng              = 1000.
*ls_oikload-VRKME                   = WA_DISPLAY1-."'SM3'.
               ls_oikload-docno                   = wa_display1-vbeln.
               ls_oikload-itemno            =     '0000000010' .
               ls_oikload-doctype =  'G'.
               ls_oikload-auart = 'ZNT7'.
*    ls_oikload-bstdk = s_date-low.

              READ TABLE it_oij_el_cp_layt INTO wa_oij_el_cp_layt WITH  KEY vbeln = wa_display1-vbeln.
              IF sy-subrc = 0.
                ls_oikload-vrkme                 = wa_oij_el_cp_layt-md  q_uom.
              ENDIF.
              APPEND ls_oikload TO lt_oikload.
              CALL FUNCTION 'OIK_SD_CALLOFF_CREATE'
                EXPORTING
                  i_oikimport = es_oikimport
                IMPORTING
                  e_oikexport = ls_oikexport
                TABLES
                  t_oikload   = lt_oikload.
              IF ls_oikexport-msgty <> 'E'.
                wa_display1-vbeln1 = ls_oikexport-kdauf.
                MODIFY it_display1 FROM wa_display1.
                PERFORM additional_qty USING wa_display1.
                PERFORM change_so USING wa_display1-vbeln1.
                PERFORM entry_imb_table USING wa_display1.
              ELSE.
*SOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala  nce TR:DVRK9A1KR0
                IF NOT sy-batch EQ 'X' AND NOT sy-tcode EQ 'YRGR102'.
*EOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala  nce TR:DVRK9A1KR0
                  MESSAGE ID ls_oikexport-msgid
                        TYPE ls_oikexport-msgty
                        NUMBER ls_oikexport-msgno
                        WITH ls_oikexport-msgv1
                             ls_oikexport-msgv2
                             ls_oikexport-msgv3
                             ls_oikexport-msgv4.
                ENDIF.
              ENDIF.
            ELSE.
              REFRESH:it_yrva_zcontrcls, lt_yrva_zcontrcls,it_vbfa.
              CLEAR:wa_yrva_zcontrcls,lv_vbeln,wa_vbfa.

              CLEAR : it_vbfa[], lt_vbrk[], lv_vbeln.
              SELECT * FROM vbfa INTO TABLE it_vbfa
                        WHERE vbelv = wa_display1-vbeln1
                          AND ( vbtyp_n = 'P'
                             OR vbtyp_n = 'M' ).
              IF sy-subrc = 0.
                SELECT * FROM vbrk INTO TABLE lt_vbrk
                        FOR ALL ENTRIES IN it_vbfa
                        WHERE vbeln = it_vbfa-vbeln
                          AND fksto NE 'X'.
                IF sy-subrc EQ 0.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
                  READ TABLE lt_vbrk INTO ls_vbrk INDEX 1. "#EC CI_NOORD  ER
** <- End changes by of Aditi on 19/11/24 for ATC
                  IF sy-subrc EQ 0.
                     lv_vbeln = ls_vbrk-vbeln.
                  ENDIF.
                ENDIF.
              ENDIF.

              IF lv_vbeln IS NOT INITIAL."sy-subrc = 0.
                CONCATENATE 'Cannot modify, Invoice already generated(IN  V. NO-' lv_vbeln ')'
                INTO lv_msg.
*SOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala  nce TR:DVRK9A1KR0
                IF NOT sy-batch EQ 'X' AND NOT sy-tcode EQ 'YRGR102'.
*EOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbala  nce TR:DVRK9A1KR0
                  MESSAGE lv_msg TYPE 'E'.
                ENDIF.
              ELSE.

*   BOA by XSAP_AB2-AV - Update Tables and SO
                IF wa_display1-vbeln1 IS NOT INITIAL.
                  PERFORM additional_qty USING wa_display1.
                  PERFORM change_so USING wa_display1-vbeln1.
                ENDIF.

*- If +ive and -ive imbalance have become zero then delete already creat  ed SO (if any)
                IF ( wa_display1-totalpic IS INITIAL AND wa_display1-tot  alnic IS INITIAL ) AND wa_display1-vbeln1 IS NOT INITIAL.
                  lv_salesdocument = wa_display1-vbeln1 .
                  ls_order_header_inx-updateflag = 'D'.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
                  CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_  OK[2438131]
** <- End changes by of Aditi on 19/11/24 for ATC
                    EXPORTING
                       salesdocument    = lv_salesdocument
                       order_header_inx = ls_order_header_inx
                    TABLES
                       return           = lt_return.
                  READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY t  ype = 'E'.
                  IF sy-subrc NE 0.
                    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                       EXPORTING
                         wait = 'X'.
                    IF sy-subrc EQ 0.
                       CLEAR wa_display1-vbeln1.
                    ENDIF.
*   Clear VBELN from DISP1 table so that it gets cleared from Cumm Imb t  able which gets updated down the line in code.
                  ELSE.
                    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
                  ENDIF.
                ENDIF.

                PERFORM entry_imb_table USING wa_display1.
*   EOA by XSAP_AB2

               ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
        IF r_dds = 'X'.
          LOOP AT it_display1 INTO wa_display1 WHERE sel = 'X'.

            IF   wa_display1-vbeln1 IS INITIAL AND ( wa_display1-totaldds NE 0 ).
              CLEAR: wa_display11,wa_vbak1,wa_display12.
              CLEAR : es_oikimport,ls_oikload,ls_oikexport.
              REFRESH lt_oikload.

              es_oikimport-applic = 3.
              es_oikimport-fumode = 1.
*ls_oikimport-LIDFUNC = 'OIK_SD_CALLOFF_CREATE'.
              es_oikimport-lidfuncnam = 'OIK_SD_CALLOFF_CREATE'.
              es_oikimport-vbeln = wa_display1-vbeln.
              es_oikimport-auart = 'ZNT7'."vbak-auart'.
              es_oikimport-lisof = 'X'.
              ls_oikload-doctype_s   = 'G'.
              ls_oikload-docno_s   = wa_display1-vbeln.

               ls_oikload-kwmeng              = 1000.
*ls_oikload-VRKME                   = WA_DISPLAY1-."'SM3'.
               ls_oikload-docno                   = wa_display1-vbeln.
               ls_oikload-itemno            =     '0000000010' .
               ls_oikload-doctype =  'G'.
               ls_oikload-auart = 'ZNT7'.
*    ls_oikload-bstdk = s_date-low.

              READ TABLE it_oij_el_cp_layt INTO wa_oij_el_cp_layt WITH  KEY vbeln = wa_display1-vbeln.
              IF sy-subrc = 0.
                ls_oikload-vrkme                  = wa_oij_el_cp_layt-md  q_uom.
              ENDIF.
              APPEND ls_oikload TO lt_oikload.
              CALL FUNCTION 'OIK_SD_CALLOFF_CREATE'
                EXPORTING
                  i_oikimport = es_oikimport
                IMPORTING
                  e_oikexport = ls_oikexport
                TABLES
                  t_oikload    = lt_oikload.
              IF ls_oikexport-msgty <> 'E'.
                wa_display1-vbeln1 = ls_oikexport-kdauf.
                MODIFY it_display1 FROM wa_display1.
                PERFORM additional_qty USING wa_display1.
                PERFORM change_so USING wa_display1-vbeln1.
                PERFORM entry_imb_table USING wa_display1.
              ELSE.
                MESSAGE ID ls_oikexport-msgid
                       TYPE ls_oikexport-msgty
                       NUMBER ls_oikexport-msgno
                       WITH ls_oikexport-msgv1
                            ls_oikexport-msgv2
                            ls_oikexport-msgv3
                            ls_oikexport-msgv4.
              ENDIF.
            ELSE.
              REFRESH:it_yrva_zcontrcls, lt_yrva_zcontrcls,it_vbfa.
              CLEAR:wa_yrva_zcontrcls,lv_vbeln,wa_vbfa.

              CLEAR : it_vbfa[], lt_vbrk[], lv_vbeln.
              SELECT * FROM vbfa INTO TABLE it_vbfa
                       WHERE vbelv = wa_display1-vbeln1
                         AND ( vbtyp_n = 'P'
                            OR vbtyp_n = 'M' ).
              IF sy-subrc = 0.
                SELECT * FROM vbrk INTO TABLE lt_vbrk
                       FOR ALL ENTRIES IN it_vbfa
                        WHERE vbeln = it_vbfa-vbeln
                          AND fksto NE 'X'.
                IF sy-subrc EQ 0.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
                  READ TABLE lt_vbrk INTO ls_vbrk INDEX 1. "#EC CI_NOORD  ER
** <- End changes by of Aditi on 19/11/24 for ATC
                  IF sy-subrc EQ 0.
                     lv_vbeln = ls_vbrk-vbeln.
                  ENDIF.
                ENDIF.
              ENDIF.

              IF lv_vbeln IS NOT INITIAL."sy-subrc = 0.
                CONCATENATE 'Cannot modify, Invoice already generated(IN  V. NO-' lv_vbeln ')'
                INTO lv_msg.
                MESSAGE lv_msg TYPE 'E'.
              ELSE.

*   BOA by XSAP_AB2-AV - Update Tables and SO
                IF wa_display1-vbeln1 IS NOT INITIAL.
                  PERFORM additional_qty USING wa_display1.
                  PERFORM change_so USING wa_display1-vbeln1.
                ENDIF.

*- If +ive and -ive imbalance have become zero then delete already creat  ed SO (if any)
                IF ( wa_display1-totalpic IS INITIAL AND wa_display1-tot  alnic IS INITIAL ) AND wa_display1-vbeln1 IS NOT INITIAL.
                  lv_salesdocument = wa_display1-vbeln1 .
                  ls_order_header_inx-updateflag = 'D'.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
                  CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_  OK[2438131]
** <- End changes by of Aditi on 19/11/24 for ATC
                    EXPORTING
                       salesdocument    = lv_salesdocument
                       order_header_inx = ls_order_header_inx
                    TABLES
                       return           = lt_return.
                  READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY t  ype = 'E'.
                  IF sy-subrc NE 0.
                    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                       EXPORTING
                         wait = 'X'.
                    IF sy-subrc EQ 0.
                       CLEAR wa_display1-vbeln1.
                    ENDIF.
*   Clear VBELN from DISP1 table so that it gets cleared from Cumm Imb t  able which gets updated down the line in code.
                  ELSE.
                    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
                  ENDIF.
                ENDIF.

                PERFORM entry_imb_table USING wa_display1.
*   EOA by XSAP_AB2

              ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
*          ENDIF.

        ENDIF.

        rs_selfield-refresh   = 'X'.

    WHEN 'SAVE'.
**********SOC By Nitin Dhamija on 16.07.2019**************
      LOOP AT it_display INTO wa_display.
        lv_diff = wa_display-del_mbg - wa_display-redel_mbg.
        IF lv_diff NE wa_display-daily_dds.
          MESSAGE 'Cannot Save , Please Check Imbalance Report' TYPE 'E'  .
          LEAVE TO CURRENT TRANSACTION.
        ENDIF.
        CLEAR: wa_display, lv_diff.
      ENDLOOP.
**********EOC By Nitin Dhamija on 16.07.2019**************
      REFRESH it_yrg_cumm_imb.
      IF it_display1[] IS NOT INITIAL.
        SELECT * FROM yrg_cumm_dds
         INTO TABLE it_yrg_cumm_imb_temp
         FOR ALL ENTRIES IN it_display1
         WHERE knkli EQ it_display1-knkli
         AND kunnr EQ it_display1-kunnr
         AND yy_contract EQ it_display1-vbeln.


        SELECT * FROM yrg_cumm_imb
          APPENDING TABLE it_yrg_cumm_imb_temp
          FOR ALL ENTRIES IN it_display1
          WHERE knkli EQ it_display1-knkli
          AND kunnr EQ it_display1-kunnr
          AND yy_contract EQ it_display1-vbeln.
        SORT it_yrg_cumm_imb_temp BY kunnr knkli yy_contract endda times  tamp DESCENDING.
      ENDIF.

*   BOA by XSAP_AB2-AV - Check previous postings
      IF r_d2 IS INITIAL.

         CLEAR lv_futher_processing.
         LOOP AT it_display1 INTO wa_display1 WHERE sel = 'X'.
           CLEAR lv_exit_processing.
           IF NOT sy-batch EQ 'X'.       "Added by XSAP_AB on 20th Sept,2  024 - Suprresing popup
             CALL METHOD ycl_gms_report_validations=>check_imbalance_post  ing
               EXPORTING
                 i_contract        = wa_display1-vbeln    " Sales Contrac  t Number
                 i_customer        = wa_display1-kunnr    " Customer Numb  er
                 i_to_date         = s_date-high          " To Date
               IMPORTING
                 e_exit_processing = lv_exit_processing.    " Exit furthe  r processing
             IF lv_exit_processing IS NOT INITIAL.
              lv_futher_processing = 'X'.
            ELSE.
              EXIT.
            ENDIF.
          ENDIF. "Added by XSAP_AB on 20th Sept,2024 - Suprresing popup
        ENDLOOP.

        CHECK lv_futher_processing IS INITIAL.
*   EOA by XSAP_AB2-AV

        LOOP AT it_display1 INTO wa_display1 WHERE sel = 'X'.
          wa_yrg_cumm_imb-knkli = wa_display1-knkli.
          wa_yrg_cumm_imb-kunnr = wa_display1-kunnr.
          wa_yrg_cumm_imb-yy_contract = wa_display1-vbeln.
          wa_yrg_cumm_imb-begda = s_date-low.
          wa_yrg_cumm_imb-endda = s_date-high.
*        if r_dds = 'X'.
*           wa_yrg_cumm_imb-vbeln_dds = wa_display1-vbeln1.
*        else.
          wa_yrg_cumm_imb-vbeln = wa_display1-vbeln1.
*        endif.
          wa_yrg_cumm_imb-yy_oij_cumimb = wa_display1-totalcumi.
          wa_yrg_cumm_imb-ldatum = sy-datum.
          wa_yrg_cumm_imb-ernam = sy-uname.
          wa_yrg_cumm_imb-augru = wa_display1-augru.
          READ TABLE it_yrg_cumm_imb_temp INTO wa_yrg_cumm_imb1
                WITH KEY
                  knkli = wa_display1-knkli
                  kunnr = wa_display1-kunnr
                  yy_contract = wa_display1-vbeln
                  begda = s_date-low
                  endda = s_date-high.
          IF sy-subrc EQ 0 .
            wa_yrg_cumm_imb-timestamp = wa_yrg_cumm_imb1-timestamp .
          ELSE.

            CALL FUNCTION 'PK_DATE_TIME_INTO_TIMESTAMP'
              EXPORTING
                 iv_pkldt = sy-datum
                 iv_pkluz = sy-uzeit
                 is_t001w = is_t001w
              IMPORTING
                 ev_pktim = wa_yrg_cumm_imb-timestamp.
          ENDIF.

          IF r_dds IS INITIAL.
             APPEND wa_yrg_cumm_imb TO it_yrg_cumm_imb.
          ELSE.
             APPEND wa_yrg_cumm_imb TO it_yrg_cumm_dds.
          ENDIF.
        ENDLOOP.
        MODIFY yrg_cumm_imb FROM TABLE it_yrg_cumm_imb.
        IF it_yrg_cumm_dds[] IS NOT INITIAL.
          MODIFY yrg_cumm_dds FROM TABLE it_yrg_cumm_dds.
        ENDIF.
      ENDIF.
*BOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
      IF r_d2 EQ 'X'.
        LOOP AT it_display1 INTO wa_display1.
          wa_yrg_chg_ovrrun-kunnr = wa_display1-kunnr.
          wa_yrg_chg_ovrrun-vbeln = wa_display1-vbeln.
            wa_yrg_chg_ovrrun-begda = s_date-low.
            wa_yrg_chg_ovrrun-endda = s_date-high.
            wa_yrg_chg_ovrrun-totaloverrun = wa_display1-totaloverrun.
            wa_yrg_chg_ovrrun-totalcharge = wa_display1-totalcharge.
            CALL FUNCTION 'PK_DATE_TIME_INTO_TIMESTAMP'
              EXPORTING
                iv_pkldt = sy-datum
                iv_pkluz = sy-uzeit
                is_t001w = is_t001w
              IMPORTING
                ev_pktim = wa_yrg_chg_ovrrun-timestamp.
            APPEND wa_yrg_chg_ovrrun TO it_yrg_chg_ovrrun.
         ENDLOOP.
         IF it_yrg_chg_ovrrun[] IS NOT INITIAL.
            MODIFY yrg_chg_ovrrun FROM TABLE it_yrg_chg_ovrrun.
         ENDIF.
       ENDIF.
       COMMIT WORK AND WAIT.
       IF sy-subrc EQ 0.
         MESSAGE 'Data Successfully Saved' TYPE 'I'.
       ENDIF.
       rs_selfield-refresh     = 'X'.
*EOC change by suyash goyal for RMS 823 - Pratibha / Arghya Mondal.
   ENDCASE.
ENDFORM.                      "USER_COMMAND
*&                                                                      *
*&       Form PRINT_PDF
*&                                                                      *
*        text
*                                                                       *
FORM print_pdf .
*        BOC 4000000613 : TECH- VIKRAM BAJAJ FUNC - PRATIBHA DT 29.08.20  19
   TYPES : BEGIN OF ty_yrg_cumm_imb_temp,
              yy_contract   TYPE yy_contract,
              begda         TYPE begda , " START DATE .
              endda         TYPE endda,
              yy_oij_cumimb TYPE yy_oij_cumimb,
            END OF ty_yrg_cumm_imb_temp .
   DATA : lt_yrg_cumm_imb_temp TYPE TABLE OF ty_yrg_cumm_imb_temp .
   DATA : lwa_yrg_cumm_imb_temp TYPE ty_yrg_cumm_imb_temp .
*        EOC 4000000613 .
   DATA: w_pdf       TYPE yrgs_imb_display,
         l_date(10).
   SORT it_pdf BY vbeln.
   LOOP AT it_display INTO wa_display.
     MOVE-CORRESPONDING wa_display TO wa_pdf.
     WRITE wa_display-date TO wa_pdf-date.
     wa_pdf-chargable_ddf = wa_display-daily_cumm_dds.
****     boc charm id 4000002623 technical : vikram bajaj functional pra  tibha dangwal dt 28.08.2020.
     IF r_d3 = 'X'.
       READ TABLE it_vbak3 INTO wa_vbak3 WITH KEY vbeln = wa_pdf-vbeln au  art = 'ZGK' .
       IF sy-subrc = 0 .
         wa_pdf-daily_overrun = 0 .
         wa_pdf-daily_charge = 0 .
       ENDIF .
       CLEAR wa_vbak3 .
     ENDIF .
****     eoc charm id 4000002623 technical : vikram bajaj functional pra  tibha dangwal dt 28.08.2020.
    APPEND wa_pdf TO it_pdf.

  ENDLOOP.
  SORT it_pdf BY vbeln .
  SORT it_display BY date DESCENDING.
  LOOP AT it_pdf INTO wa_pdf.
    w_pdf-vbeln = wa_pdf-vbeln.
    w_pdf-del_point = wa_pdf-del_point + w_pdf-del_point.
    w_pdf-redel_point = wa_pdf-redel_point + w_pdf-redel_point.
    w_pdf-corr_nom = wa_pdf-corr_nom + w_pdf-corr_nom.
    w_pdf-corr_nom1 = wa_pdf-corr_nom1 + w_pdf-corr_nom1.
    w_pdf-daily = wa_pdf-daily + w_pdf-daily.
*   get the cumalative imbalance of last day.
    READ TABLE it_display INTO wa_display WITH KEY vbeln = wa_pdf-vbeln.
    IF sy-subrc EQ 0 .
      WRITE wa_display-date TO l_date.
      IF l_date EQ wa_pdf-date.
        w_pdf-daily_cumm = wa_pdf-daily_cumm . "WA_PDF-DAILY_CUMM + W_PD  F-DAILY_CUMM.
      ENDIF.
    ENDIF.
    w_pdf-daily_charge = wa_pdf-daily_charge + w_pdf-daily_charge.
    w_pdf-daily_dds = wa_pdf-daily_dds + w_pdf-daily_dds.
    w_pdf-daily_overrun = wa_pdf-daily_overrun + w_pdf-daily_overrun.
    w_pdf-chargable_imb = wa_pdf-chargable_imb + w_pdf-chargable_imb.
    w_pdf-menge = wa_pdf-menge + w_pdf-menge.
    w_pdf-mdq_quan = wa_pdf-mdq_quan + w_pdf-mdq_quan.
    w_pdf-del_mbg = wa_pdf-del_mbg + w_pdf-del_mbg.
    w_pdf-redel_mbg = wa_pdf-redel_mbg + w_pdf-redel_mbg.
    w_pdf-chargable_ddf =   wa_pdf-chargable_ddf + w_pdf-chargable_ddf .
    AT END OF vbeln.
      w_pdf-date = 'TOTAL'.
      INSERT w_pdf INTO it_pdf.
      CLEAR w_pdf.
    ENDAT.
  ENDLOOP.

*    IF r_d5 = 'X' AND s_vbeln IS NOT INITIAL.
*      MODIFY yrg_cumm_imb FROM TABLE it_yrg_cumm_imb..
*      COMMIT WORK AND WAIT.
*
*      REFRESH : it_yrg_cumm_imb.
*      CLEAR wa_yrg_cumm_imb.
*    ENDIF.

*       BOC 4000000613 : TECH- VIKRAM BAJAJ FUNC - PRATIBHA DT 29.08.20  19
*TYPES : BEGIN OF TY_YRG_CUMM_IMB_TEMP,
*         YY_CONTRACT TYPE YY_CONTRACT,
*         BEGDA TYPE BEGDA , " START DATE .
*         ENDDA TYPE ENDDA ,
*         YY_OIJ_CUMIMB TYPE YY_OIJ_CUMIMB ,
*        END OF TY_YRG_CUMM_IMB_TEMP .
*DATA : LT_YRG_CUMM_IMB_TEMP TYPE TABLE OF YRG_CUMM_IMB .
*DATA : LWA_YRG_CUMM_IMB_TEMP TYPE YRG_CUMM_IMB .

    SELECT
         yy_contract
         begda
         endda
       yy_oij_cumimb
    FROM yrg_cumm_imb
    INTO TABLE lt_yrg_cumm_imb_temp
    FOR ALL ENTRIES IN it_display1
    WHERE yy_contract = it_display1-vbeln
    AND begda LT s_date-low .
  IF sy-subrc = 0 .
    SORT lt_yrg_cumm_imb_temp BY yy_contract endda DESCENDING .
  ENDIF .
*       EOC 4000000613 .

   SORT it_pdf BY vbeln date.
   LOOP AT it_display1 INTO wa_display1.
     CLEAR wa_pdf1.
     MOVE-CORRESPONDING wa_display1 TO wa_pdf1.
*        BOC 4000000613 : TECH- VIKRAM BAJAJ FUNC - PRATIBHA DT 29.08.20  19
     READ TABLE lt_yrg_cumm_imb_temp INTO lwa_yrg_cumm_imb_temp WITH KEY  yy_contract = wa_display1-vbeln BINARY SEARCH .
     IF sy-subrc = 0 .
       wa_pdf1-opencumi = lwa_yrg_cumm_imb_temp-yy_oij_cumimb .
     ENDIF .
     CLEAR lwa_yrg_cumm_imb_temp .
*        EOC 4000000613 .
****     boc charm id 4000002623 technical : vikram bajaj functional pra  tibha dangwal dt 28.08.2020.
     IF r_d3 = 'X'.
       READ TABLE it_vbak3 INTO wa_vbak3 WITH KEY vbeln = wa_display1-vbe  ln auart = 'ZGK' .
       IF sy-subrc = 0 .
         wa_pdf1-totaloverrun = 0 .
         wa_pdf1-totalcharge = 0 .
       ENDIF .
       CLEAR wa_vbak3 .
     ENDIF.
****     eoc charm id 4000002623 technical : vikram bajaj functional pra  tibha dangwal dt 28.08.2020.
     wa_pdf1-total_ddf = wa_pdf1-totaldds.
     APPEND wa_pdf1 TO it_pdf1.
   ENDLOOP.

  DATA : w_formname      TYPE fpname,
         w_funcname      TYPE funcname,
         fp_outputparams TYPE sfpoutputparams,
         fp_formoutput   TYPE fpformoutput.

  w_formname = 'YRG_IMBALANCE_FORM'.
  fp_outputparams-dest     = 'LP01'.
* boc char 613 .
  IF r_d3 = 'X' .
* eoc char 613 .
    TRY .
        CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
          EXPORTING
            i_name     = w_formname
          IMPORTING
            e_funcname = w_funcname.
      CATCH cx_fp_api_usage .
        RAISE cx_fp_api_usage .
      CATCH cx_fp_api_repository .
        RAISE cx_fp_api_repository .
      CATCH cx_fp_api_internal .
        RAISE cx_fp_api_internal .
    ENDTRY .
    CALL FUNCTION 'FP_JOB_OPEN'
      CHANGING
        ie_outputparams = fp_outputparams
      EXCEPTIONS
        cancel           = 1
        usage_error      = 2
        system_error     = 3
        internal_error = 4
        OTHERS           = 5.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
* test to be deleted
* data: IT_PDF_TEST       TYPE TABLE OF YRGS_IMB_DISPLAY.
* DO 3 TIMES.
*    IT_PDF_TEST[] = IT_PDF[].
*    APPEND LINES OF IT_PDF_TEST TO IT_PDF .
* ENDDO.

    CALL FUNCTION w_funcname
      EXPORTING
        it_display1        = it_pdf1[]
        it_display         = it_pdf[]
        it_header          = it_header[]
        lv_kunnr           = p_kunnr           "Added by XSAP_AB 24th Sept,  2024
        IMPORTING
          /1bcdwb/formoutput = fp_formoutput
        EXCEPTIONS
          usage_error        = 1
          system_error       = 2
          internal_error     = 3.

    CALL FUNCTION 'FPCOMP_JOB_CLOSE'
*   IMPORTING
*     E_JOBRESULT            =
      EXCEPTIONS
        usage_error     = 1
        system_error    = 2
        internal_error = 3
        OTHERS          = 4.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

********* BOC charm 613 .
  ELSEIF r_d6 = 'X' .


    DATA : lt_yrga_email_nomi TYPE TABLE OF yrga_email_nom.               "y  rga_email_nomi .
    DATA : lwa_yrga_email_nomi TYPE         yrga_email_nom.                "  yrga_email_nomi .
    DATA: it_mail TYPE STANDARD TABLE OF    yrga_email_nom.

* SOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000007034

*    SELECT * FROM YRGA_EMAIL_NOMI INTO TABLE LT_YRGA_EMAIL_NOMI WHERE K  UNNR = P_KUNNR AND DEL_IND NE 'X'.
*    IF SY-SUBRC = 0 .
*
*      TRY .
*          CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
*             EXPORTING
*               I_NAME     = W_FORMNAME
*             IMPORTING
*               E_FUNCNAME = W_FUNCNAME.
*          FP_OUTPUTPARAMS-NODIALOG = 'X'.
*          FP_OUTPUTPARAMS-GETPDF     = 'X'.
*        CATCH CX_FP_API_INTERNAL .
*          RAISE CX_FP_API_INTERNAL .
*        CATCH CX_FP_API_USAGE .
*          RAISE CX_FP_API_USAGE .
*        CATCH CX_FP_API_REPOSITORY .
*          RAISE CX_FP_API_REPOSITORY .
*      ENDTRY .
*      CALL FUNCTION 'FP_JOB_OPEN'
*        CHANGING
*          IE_OUTPUTPARAMS = FP_OUTPUTPARAMS
*        EXCEPTIONS
*          CANCEL           = 1
*          USAGE_ERROR      = 2
*          SYSTEM_ERROR     = 3
*          INTERNAL_ERROR = 4
*          OTHERS           = 5.
*      IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
** test to be deleted
** data: IT_PDF_TEST       TYPE TABLE OF YRGS_IMB_DISPLAY.
** DO 3 TIMES.
**    IT_PDF_TEST[] = IT_PDF[].
**    APPEND LINES OF IT_PDF_TEST TO IT_PDF .
** ENDDO.
*
*      CALL FUNCTION W_FUNCNAME
*        EXPORTING
*          IT_DISPLAY1          = IT_PDF1[]
*          IT_DISPLAY           = IT_PDF[]
*        IMPORTING
*          /1BCDWB/FORMOUTPUT = FP_FORMOUTPUT
*        EXCEPTIONS
*          USAGE_ERROR          = 1
*          SYSTEM_ERROR         = 2
*          INTERNAL_ERROR       = 3.
*
*      CALL FUNCTION 'FPCOMP_JOB_CLOSE'
**   IMPORTING
**     E_JOBRESULT           =
*        EXCEPTIONS
*          USAGE_ERROR     = 1
*          SYSTEM_ERROR    = 2
*          INTERNAL_ERROR = 3
*          OTHERS          = 4.
*      IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
** SOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000007034
**       DATA:
**         T_ATT_CONTENT_HEX TYPE SOLIX_TAB.
** EOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000007034
*
*      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
*         EXPORTING
*           BUFFER     = FP_FORMOUTPUT-PDF
**          APPEND_TO_TABLE       = ' '
** IMPORTING
**          OUTPUT_LENGTH         =
*         TABLES
*           BINARY_TAB = T_ATT_CONTENT_HEX.
*
** SOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000006716.
*
*      DATA: w_flag(1) TYPE c.
*
*      IMPORT w_flag FROM MEMORY ID 'MEM_FLAG'.
*
*      IF w_flag = 'X'.
*
*         EXPORT t_att_content_hex TO MEMORY ID 'MEM_LIN'.
*
*         LEAVE PROGRAM.
*
*         SKIP 0.
*
*      ENDIF.
*
** EOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000006716.
*
*      CLASS CL_BCS DEFINITION LOAD.
*      DATA:
*      LO_SEND_REQUEST TYPE REF TO CL_BCS VALUE IS INITIAL.
** Message body and subject
*      DATA : LV_LAST_DAY TYPE SY-DATUM .
*      LV_LAST_DAY = SY-DATUM - 1 .
*      DATA : LV_DISP_LASTDAY(10) .
*
*      CONCATENATE LV_LAST_DAY+6(2) '.' LV_LAST_DAY+4(2) '.' LV_LAST_DA  Y+0(4) INTO LV_DISP_LASTDAY .
*      DATA : LV_SUB2(50).
*      DATA:
*         LT_MESSAGE_BODY TYPE BCSY_TEXT VALUE IS INITIAL,
*         LO_DOCUMENT     TYPE REF TO CL_DOCUMENT_BCS VALUE IS INITIAL.
*
*      DATA: LV_SENT_TO_ALL(1) TYPE C VALUE IS INITIAL.
*      DATA: LV_SUB(50).
*      DATA: LX_DOCUMENT_BCS TYPE REF TO CX_DOCUMENT_BCS VALUE IS INITIA  L.
*      DATA : LV_CUSTOMER(11) .
*      DATA : LV_SO_TEXT255 TYPE SO_TEXT255 .
*
*      LV_CUSTOMER = P_KUNNR .
*      DATA : LV_IMB TYPE STRING .
*      SHIFT LV_CUSTOMER LEFT DELETING LEADING '0'.
*      CONCATENATE 'Intimation-Imbalance_' LV_CUSTOMER INTO LV_IMB .
********
** *
********* Mail program ,
*
*
**Imbalance Statement_10551_01.09.2019-15.09.2019
*       CONCATENATE 'Imbalance Statement_' LV_CUSTOMER ' _ ' S_DATE-LOW+  6(2) '.' S_DATE-LOW+4(2) '.' S_DATE-LOW+0(4) ' - ' S_DATE-HIGH+6(2) '.'  S_DATE-HIGH+4(2) '.' S_DATE-HIGH+0(4) INTO LV_SUB.
*
********
*       CONCATENATE LV_IMB 'upto Gas Day' LV_DISP_LASTDAY INTO LV_SUB2  SEPARATED BY SPACE.
*       REFRESH LT_MESSAGE_BODY[] .
*       APPEND 'Dear Sir/Maam,' TO LT_MESSAGE_BODY.
*       APPEND ' ' TO LT_MESSAGE_BODY.
** APPEND 'Please find the attached file for the imbalance statement un  der subject GTA/CT for current fortnight till gas '
** TO lt_message_body.
** append ' ' to lt_message_body.
*       CONCATENATE 'Please find the attached file for the imbalance stat  ement under subject GTA/CT for current fortnight till gas business date:  ' LV_DISP_LASTDAY INTO LV_SO_TEXT255 SEPARATED BY SPACE .
*       APPEND LV_SO_TEXT255 TO LT_MESSAGE_BODY.
*       APPEND '' TO LT_MESSAGE_BODY.
*       APPEND 'In case of any issue please contact respective RGMC/NGMC.  ' TO LT_MESSAGE_BODY.
*       APPEND '' TO LT_MESSAGE_BODY.
*       APPEND 'With warm regards,' TO LT_MESSAGE_BODY.
*       APPEND 'GAIL (INDIA) LTD.' TO LT_MESSAGE_BODY.
*       APPEND '' TO LT_MESSAGE_BODY.
*       APPEND '' TO LT_MESSAGE_BODY.
*       APPEND '' TO LT_MESSAGE_BODY.
*       APPEND '' TO LT_MESSAGE_BODY.
*       APPEND '' TO LT_MESSAGE_BODY.
*       APPEND '' TO LT_MESSAGE_BODY.
*       APPEND '                        ' TO LT_MESSAGE_BODY.
*       APPEND '' TO LT_MESSAGE_BODY.
*       APPEND '' TO LT_MESSAGE_BODY.
*       APPEND 'This is a system generated mail. Please do not reply.' TO  LT_MESSAGE_BODY.
*       APPEND '' TO LT_MESSAGE_BODY.
*       APPEND '' TO LT_MESSAGE_BODY.
*       APPEND '                        ' TO LT_MESSAGE_BODY.
********
*       DATA:
*         LO_SENDER TYPE REF TO IF_SENDER_BCS VALUE IS INITIAL,
*         L_SEND    TYPE ADR6-SMTP_ADDR. " value 'vikram.bajaj@birlasoft.  com'.
*
*       DATA:
*       LO_RECIPIENT TYPE REF TO IF_RECIPIENT_BCS VALUE IS INITIAL.
*
** loop at lt_YRGA_EMAIL_NOMI INTO lwa_YRGA_EMAIL_NOMI .
*       TRY .
*           LO_SEND_REQUEST = CL_BCS=>CREATE_PERSISTENT( ).
*         CATCH CX_SEND_REQ_BCS .
*           RAISE CX_SEND_REQ_BCS .
**Intimation - Imbalance_10551 upto Gas Day 13.09.19
*       ENDTRY .
*       TRY .
*            LO_DOCUMENT = CL_DOCUMENT_BCS=>CREATE_DOCUMENT(
*            I_TYPE = 'RAW'
*            I_TEXT = LT_MESSAGE_BODY
*            I_SUBJECT = LV_SUB2 ).
*         CATCH CX_DOCUMENT_BCS.
*            RAISE CX_DOCUMENT_BCS .
*       ENDTRY.
*
*       TRY.
*            LO_DOCUMENT->ADD_ATTACHMENT(
*            EXPORTING
*            I_ATTACHMENT_TYPE = 'PDF'
*            I_ATTACHMENT_SUBJECT = LV_SUB
*
*            I_ATT_CONTENT_HEX = T_ATT_CONTENT_HEX ).
*         CATCH CX_DOCUMENT_BCS INTO LX_DOCUMENT_BCS.
*       ENDTRY.
*       TRY .
*
*            LO_SEND_REQUEST->SET_DOCUMENT( LO_DOCUMENT ).
*            LO_SENDER = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS(
*
*            I_ADDRESS_STRING = 'gailpartcare@gail.co.in' ).
*         CATCH CX_ADDRESS_BCS.
*            RAISE CX_ADDRESS_BCS .
*         CATCH CX_SEND_REQ_BCS .
*            RAISE CX_SEND_REQ_BCS .
*         CATCH CX_FP_API_INTERNAL .
*            RAISE CX_FP_API_INTERNAL .
*         CATCH CX_FP_API_USAGE .
*            RAISE CX_FP_API_USAGE .
*         CATCH CX_FP_API_REPOSITORY .
*            RAISE CX_FP_API_REPOSITORY .
*
*       ENDTRY.
*
*       TRY .
*            LO_SEND_REQUEST->SET_SENDER(
*            EXPORTING
*            I_SENDER = LO_SENDER ).
******BOC CHARM ID 4000001049 : FUNCTIONAL : PRATIBHA DANGWAL , TECHNICA  L : VIKRAM BAJAJ DT 29.10.2019
*            LOOP AT LT_YRGA_EMAIL_NOMI INTO LWA_YRGA_EMAIL_NOMI .
******EOC CHARM ID 4000001049 : FUNCTIONAL : PRATIBHA DANGWAL , TECHNICA  L : VIKRAM BAJAJ DT 29.10.2019
*              L_SEND = LWA_YRGA_EMAIL_NOMI-EMAIL .
*              LO_RECIPIENT = CL_CAM_ADDRESS_BCS=>CREATE_INTERNET_ADDRESS( L_SEND ).
*              LO_SEND_REQUEST->ADD_RECIPIENT(
*              EXPORTING
*              I_RECIPIENT = LO_RECIPIENT
*              I_EXPRESS = 'X' ).
******BOC CHARM ID 4000001049 : FUNCTIONAL : PRATIBHA DANGWAL , TECHNICA  L : VIKRAM BAJAJ DT 29.10.2019
*            ENDLOOP .
******EOC CHARM ID 4000001049 : FUNCTIONAL : PRATIBHA DANGWAL , TECHNICA  L : VIKRAM BAJAJ DT 29.10.2019
*            LO_SEND_REQUEST->SEND(
*            EXPORTING
*            I_WITH_ERROR_SCREEN = 'X'
*             RECEIVING
*             RESULT = LV_SENT_TO_ALL ).
*             COMMIT WORK.
*           CATCH CX_ADDRESS_BCS .
*             RAISE CX_ADDRESS_BCS .
*           CATCH CX_SEND_REQ_BCS .
*             RAISE CX_SEND_REQ_BCS .
*         ENDTRY .
*         CLEAR LWA_YRGA_EMAIL_NOMI .
**       ENDLOOP .
*
**   ENDLOOP .
*
*       ENDIF .

     DATA: w_flag(1) TYPE c.

     IMPORT w_flag FROM MEMORY ID 'MEM_FLAG'.

    IF w_flag NE 'X'.
      "boc 4000007553 techincal ankur function shreyosi de on 08.01.2023
      "SELECT * FROM yrga_email_nomi INTO TABLE lt_yrga_email_nomi WHERE kunnr = p_kunnr AND del_ind NE 'X'.
      CALL FUNCTION 'YRX_CUST_MSTR_MAIL'
        EXPORTING
          kunnr   = p_kunnr
        TABLES
          et_mail = it_mail[].

         REFRESH:lt_yrga_email_nomi[].
         lt_yrga_email_nomi[] = it_mail[].
         .

         IF sy-subrc = 0 .

         TRY .
             CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
               EXPORTING
                  i_name     = w_formname
               IMPORTING
                  e_funcname = w_funcname.
             fp_outputparams-nodialog = 'X'.
             fp_outputparams-getpdf    = 'X'.
           CATCH cx_fp_api_internal .
             RAISE cx_fp_api_internal .
           CATCH cx_fp_api_usage .
             RAISE cx_fp_api_usage .
           CATCH cx_fp_api_repository .
             RAISE cx_fp_api_repository .
         ENDTRY .
         CALL FUNCTION 'FP_JOB_OPEN'
           CHANGING
             ie_outputparams = fp_outputparams
           EXCEPTIONS
             cancel           = 1
             usage_error      = 2
             system_error     = 3
             internal_error = 4
             OTHERS           = 5.
         IF sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
           ENDIF.
*       test to be deleted
*       data: IT_PDF_TEST     TYPE TABLE OF YRGS_IMB_DISPLAY.
*       DO 3 TIMES.
*         IT_PDF_TEST[] = IT_PDF[].
*         APPEND LINES OF IT_PDF_TEST TO IT_PDF .
*       ENDDO.

           CALL FUNCTION w_funcname
             EXPORTING
               it_display1        = it_pdf1[]
               it_display         = it_pdf[]
               it_header          = it_header[]
               lv_kunnr           = p_kunnr           "Added by XSAP_AB 24th S  ept,2024
             IMPORTING
               /1bcdwb/formoutput = fp_formoutput
             EXCEPTIONS
               usage_error        = 1
               system_error       = 2
               internal_error     = 3.

        CALL FUNCTION 'FPCOMP_JOB_CLOSE'
*     IMPORTING
*       E_JOBRESULT          =
          EXCEPTIONS
            usage_error    = 1
            system_error   = 2
            internal_error = 3
            OTHERS         = 4.
        IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.
*   SOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000007034
*        DATA:
*          T_ATT_CONTENT_HEX TYPE SOLIX_TAB.
*   EOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000007034

        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer     = fp_formoutput-pdf
*           APPEND_TO_TABLE       = ' '
*   IMPORTING
*           OUTPUT_LENGTH         =
          TABLES
            binary_tab = t_att_content_hex.

*   SOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000006716.

           IMPORT w_flag FROM MEMORY ID 'MEM_FLAG'.

           IF w_flag = 'X'.

*   SOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000007034.

             FREE MEMORY ID 'MEM_LIN'.

*             EXPORT t_att_content_hex TO MEMORY ID 'MEM_LIN'.

            IF t_att_content_hex IS NOT INITIAL.

              EXPORT t_att_content_hex TO MEMORY ID 'MEM_LIN'.

            ENDIF.

*     EOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000007034.

            LEAVE PROGRAM.

            SKIP 0.

          ENDIF.

*     EOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000006716.

          CLASS cl_bcs DEFINITION LOAD.
          DATA:
          lo_send_request TYPE REF TO cl_bcs VALUE IS INITIAL.
*     Message body and subject
          DATA : lv_last_day TYPE sy-datum .
          lv_last_day = sy-datum - 1 .
          DATA : lv_disp_lastday(10) .

        CONCATENATE lv_last_day+6(2) '.' lv_last_day+4(2) '.' lv_last_d  ay+0(4) INTO lv_disp_lastday .
        DATA : lv_sub2(50).
        DATA:
          lt_message_body TYPE bcsy_text VALUE IS INITIAL,
          lo_document     TYPE REF TO cl_document_bcs VALUE IS INITIAL.

          DATA: lv_sent_to_all(1) TYPE c VALUE IS INITIAL.
          DATA: lv_sub(50).
          DATA: lx_document_bcs TYPE REF TO cx_document_bcs VALUE IS INITI  AL.
          DATA : lv_customer(11) .
          DATA : lv_so_text255 TYPE so_text255 .

         lv_customer = p_kunnr .
         DATA : lv_imb TYPE string .
         SHIFT lv_customer LEFT DELETING LEADING '0'.
         CONCATENATE 'Intimation-Imbalance_' lv_customer INTO lv_imb .
*   ******
*     *
*   ******* Mail program ,


*  Imbalance Statement_10551_01.09.2019-15.09.2019
        CONCATENATE 'Imbalance Statement_' lv_customer ' _ ' s_date-low  +6(2) '.' s_date-low+4(2) '.' s_date-low+0(4) ' - ' s_date-high+6(2) '.' s_date-high+4(2) '.' s_date-high+0(4) INTO lv_sub.

*   ******
         CONCATENATE lv_imb 'upto Gas Day' lv_disp_lastday INTO lv_sub2  SEPARATED BY space.
         REFRESH lt_message_body[] .
         APPEND 'Dear Sir/Maam,' TO lt_message_body.
         APPEND ' ' TO lt_message_body.
*     APPEND 'Please find the attached file for the imbalance statement u  nder subject GTA/CT for current fortnight till gas '
*     TO lt_message_body.
*    append ' ' to lt_message_body.
        CONCATENATE 'Please find the attached file for the imbalance sta  tement under subject GTA/CT for current fortnight till gas business date  :' lv_disp_lastday INTO lv_so_text255 SEPARATED BY space .
        APPEND lv_so_text255 TO lt_message_body.
        APPEND '' TO lt_message_body.
        APPEND 'In case of any issue please contact respective RGMC/NGMC  .' TO lt_message_body.
        APPEND '' TO lt_message_body.
        APPEND 'With warm regards,' TO lt_message_body.
        APPEND 'GAIL (INDIA) LTD.' TO lt_message_body.
        APPEND '' TO lt_message_body.
        APPEND '' TO lt_message_body.
        APPEND '' TO lt_message_body.
        APPEND '' TO lt_message_body.
        APPEND '' TO lt_message_body.
        APPEND '' TO lt_message_body.
        APPEND '                        ' TO lt_message_body.
        APPEND '' TO lt_message_body.
        APPEND '' TO lt_message_body.
        APPEND 'This is a system generated mail. Please do not reply.' T  O lt_message_body.
        APPEND '' TO lt_message_body.
        APPEND '' TO lt_message_body.
        APPEND '                        ' TO lt_message_body.
* ******
        DATA:
          lo_sender TYPE REF TO if_sender_bcs VALUE IS INITIAL,
          l_send    TYPE adr6-smtp_addr. " value 'vikram.bajaj@birlasoft  .com'.

         DATA:
         lo_recipient TYPE REF TO if_recipient_bcs VALUE IS INITIAL.

*    loop at lt_YRGA_EMAIL_NOMI INTO lwa_YRGA_EMAIL_NOMI .
         TRY .
             lo_send_request = cl_bcs=>create_persistent( ).
           CATCH cx_send_req_bcs .
             RAISE cx_send_req_bcs .
*   Intimation - Imbalance_10551 upto Gas Day 13.09.19
         ENDTRY .
         TRY .
             lo_document = cl_document_bcs=>create_document(
             i_type = 'RAW'
             i_text = lt_message_body
             i_subject = lv_sub2 ).
           CATCH cx_document_bcs.
             RAISE cx_document_bcs .
         ENDTRY.

         TRY.
             lo_document->add_attachment(
             EXPORTING
             i_attachment_type = 'PDF'
             i_attachment_subject = lv_sub

             i_att_content_hex = t_att_content_hex ).
           CATCH cx_document_bcs INTO lx_document_bcs.
         ENDTRY.
           TRY .

                 lo_send_request->set_document( lo_document ).
                 lo_sender = cl_cam_address_bcs=>create_internet_address(

              i_address_string = 'gailpartcare@gail.co.in' ).
            CATCH cx_address_bcs.
              RAISE cx_address_bcs .
            CATCH cx_send_req_bcs .
              RAISE cx_send_req_bcs .
            CATCH cx_fp_api_internal .
              RAISE cx_fp_api_internal .
            CATCH cx_fp_api_usage .
              RAISE cx_fp_api_usage .
            CATCH cx_fp_api_repository .
              RAISE cx_fp_api_repository .

           ENDTRY.

        TRY .
            lo_send_request->set_sender(
            EXPORTING
            i_sender = lo_sender ).
* ****BOC CHARM ID 4000001049 : FUNCTIONAL : PRATIBHA DANGWAL , TECHNIC  AL : VIKRAM BAJAJ DT 29.10.2019
            LOOP AT lt_yrga_email_nomi INTO lwa_yrga_email_nomi .
* ****EOC CHARM ID 4000001049 : FUNCTIONAL : PRATIBHA DANGWAL , TECHNIC  AL : VIKRAM BAJAJ DT 29.10.2019
              l_send = lwa_yrga_email_nomi-email .
              lo_recipient = cl_cam_address_bcs=>create_internet_address  ( l_send ).
              lo_send_request->add_recipient(
              EXPORTING
              i_recipient = lo_recipient
              i_express = 'X' ).
* ****BOC CHARM ID 4000001049 : FUNCTIONAL : PRATIBHA DANGWAL , TECHNIC  AL : VIKRAM BAJAJ DT 29.10.2019
            ENDLOOP .
* ****EOC CHARM ID 4000001049 : FUNCTIONAL : PRATIBHA DANGWAL , TECHNIC  AL : VIKRAM BAJAJ DT 29.10.2019
            lo_send_request->send(
            EXPORTING
            i_with_error_screen = 'X'
            RECEIVING
            result = lv_sent_to_all ).
            COMMIT WORK.
          CATCH cx_address_bcs .
            RAISE cx_address_bcs .
          CATCH cx_send_req_bcs .
            RAISE cx_send_req_bcs .
        ENDTRY .
        CLEAR lwa_yrga_email_nomi .
*      ENDLOOP .

*       ENDLOOP .

         ENDIF .

    ELSEIF w_flag = 'X'.

         TRY .
          CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
             EXPORTING
               i_name     = w_formname
             IMPORTING
               e_funcname = w_funcname.
          fp_outputparams-nodialog = 'X'.
          fp_outputparams-getpdf    = 'X'.
        CATCH cx_fp_api_internal .
          RAISE cx_fp_api_internal .
        CATCH cx_fp_api_usage .
          RAISE cx_fp_api_usage .
        CATCH cx_fp_api_repository .
          RAISE cx_fp_api_repository .
      ENDTRY .
      CALL FUNCTION 'FP_JOB_OPEN'
        CHANGING
          ie_outputparams = fp_outputparams
        EXCEPTIONS
          cancel           = 1
          usage_error      = 2
          system_error     = 3
          internal_error = 4
          OTHERS           = 5.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
* test to be deleted
* data: IT_PDF_TEST       TYPE TABLE OF YRGS_IMB_DISPLAY.
* DO 3 TIMES.
*    IT_PDF_TEST[] = IT_PDF[].
*    APPEND LINES OF IT_PDF_TEST TO IT_PDF .
* ENDDO.

        CALL FUNCTION w_funcname
          EXPORTING
            it_display1        = it_pdf1[]
            it_display         = it_pdf[]
            lv_kunnr           = p_kunnr          "Added by XSAP_AB 24th Sep  t,2024
          IMPORTING
            /1bcdwb/formoutput = fp_formoutput
          EXCEPTIONS
            usage_error        = 1
            system_error       = 2
            internal_error     = 3.

      CALL FUNCTION 'FPCOMP_JOB_CLOSE'
*   IMPORTING
*     E_JOBRESULT          =
        EXCEPTIONS
          usage_error    = 1
          system_error   = 2
          internal_error = 3
          OTHERS         = 4.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
*   SOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000007034
*      DATA:
*        T_ATT_CONTENT_HEX TYPE SOLIX_TAB.
*   SOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000007034

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer     = fp_formoutput-pdf
*         APPEND_TO_TABLE       = ' '
* IMPORTING
*         OUTPUT_LENGTH         =
        TABLES
          binary_tab = t_att_content_hex.

* SOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000006716.

        IMPORT w_flag FROM MEMORY ID 'MEM_FLAG'.

        IF w_flag = 'X'.

*   SOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000007034

          FREE MEMORY ID 'MEM_LIN'.

*          EXPORT t_att_content_hex TO MEMORY ID 'MEM_LIN'.

          IF t_att_content_hex IS NOT INITIAL.

            EXPORT t_att_content_hex TO MEMORY ID 'MEM_LIN'.

          ENDIF.

*   EOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000007034

          LEAVE PROGRAM.

          SKIP 0.

        ENDIF.

* EOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000006716.

      CLASS cl_bcs DEFINITION LOAD.
* Message body and subject
      lv_last_day = sy-datum - 1 .

      CONCATENATE lv_last_day+6(2) '.' lv_last_day+4(2) '.' lv_last_day  +0(4) INTO lv_disp_lastday .

      lv_customer = p_kunnr .
      SHIFT lv_customer LEFT DELETING LEADING '0'.
      CONCATENATE 'Intimation-Imbalance_' lv_customer INTO lv_imb .
*******
* *
******** Mail program ,


*Imbalance Statement_10551_01.09.2019-15.09.2019
      CONCATENATE 'Imbalance Statement_' lv_customer ' _ ' s_date-low+6  (2) '.' s_date-low+4(2) '.' s_date-low+0(4) ' - ' s_date-high+6(2) '.' s  _date-high+4(2) '.' s_date-high+0(4) INTO lv_sub.

*******
      CONCATENATE lv_imb 'upto Gas Day' lv_disp_lastday INTO lv_sub2 S  EPARATED BY space.
      REFRESH lt_message_body[] .
      APPEND 'Dear Sir/Maam,' TO lt_message_body.
      APPEND ' ' TO lt_message_body.
* APPEND 'Please find the attached file for the imbalance statement und  er subject GTA/CT for current fortnight till gas '
* TO lt_message_body.
* append ' ' to lt_message_body.
      CONCATENATE 'Please find the attached file for the imbalance state  ment under subject GTA/CT for current fortnight till gas business date:' lv_disp_lastday INTO lv_so_text255 SEPARATED BY space .
      APPEND lv_so_text255 TO lt_message_body.
      APPEND '' TO lt_message_body.
      APPEND 'In case of any issue please contact respective RGMC/NGMC.' TO lt_message_body.
      APPEND '' TO lt_message_body.
      APPEND 'With warm regards,' TO lt_message_body.
      APPEND 'GAIL (INDIA) LTD.' TO lt_message_body.
      APPEND '' TO lt_message_body.
      APPEND '' TO lt_message_body.
      APPEND '' TO lt_message_body.
      APPEND '' TO lt_message_body.
      APPEND '' TO lt_message_body.
      APPEND '' TO lt_message_body.
      APPEND '                      ' TO lt_message_body.
      APPEND '' TO lt_message_body.
      APPEND '' TO lt_message_body.
      APPEND 'This is a system generated mail. Please do not reply.' TO  lt_message_body.
      APPEND '' TO lt_message_body.
      APPEND '' TO lt_message_body.
      APPEND '                      ' TO lt_message_body.
*******

* loop at lt_YRGA_EMAIL_NOMI INTO lwa_YRGA_EMAIL_NOMI .
      TRY .
          lo_send_request = cl_bcs=>create_persistent( ).
        CATCH cx_send_req_bcs .
          RAISE cx_send_req_bcs .
*Intimation - Imbalance_10551 upto Gas Day 13.09.19
      ENDTRY .
      TRY .
          lo_document = cl_document_bcs=>create_document(
          i_type = 'RAW'
          i_text = lt_message_body
          i_subject = lv_sub2 ).
        CATCH cx_document_bcs.
          RAISE cx_document_bcs .
      ENDTRY.

        TRY.
            lo_document->add_attachment(
            EXPORTING
            i_attachment_type = 'PDF'
            i_attachment_subject = lv_sub

            i_att_content_hex = t_att_content_hex ).
          CATCH cx_document_bcs INTO lx_document_bcs.
        ENDTRY.
        TRY .

            lo_send_request->set_document( lo_document ).
            lo_sender = cl_cam_address_bcs=>create_internet_address(

            i_address_string = 'gailpartcare@gail.co.in' ).
          CATCH cx_address_bcs.
            RAISE cx_address_bcs .
          CATCH cx_send_req_bcs .
            RAISE cx_send_req_bcs .
          CATCH cx_fp_api_internal .
            RAISE cx_fp_api_internal .
          CATCH cx_fp_api_usage .
            RAISE cx_fp_api_usage .
          CATCH cx_fp_api_repository .
            RAISE cx_fp_api_repository .

        ENDTRY.

       TRY .
           lo_send_request->set_sender(
           EXPORTING
           i_sender = lo_sender ).
*****BOC CHARM ID 4000001049 : FUNCTIONAL : PRATIBHA DANGWAL , TECHNICAL  : VIKRAM BAJAJ DT 29.10.2019
           LOOP AT lt_yrga_email_nomi INTO lwa_yrga_email_nomi .
*****EOC CHARM ID 4000001049 : FUNCTIONAL : PRATIBHA DANGWAL , TECHNICAL  : VIKRAM BAJAJ DT 29.10.2019
             l_send = lwa_yrga_email_nomi-email .
             lo_recipient = cl_cam_address_bcs=>create_internet_address( l_send ).
             lo_send_request->add_recipient(
             EXPORTING
             i_recipient = lo_recipient
             i_express = 'X' ).
*****BOC CHARM ID 4000001049 : FUNCTIONAL : PRATIBHA DANGWAL , TECHNICAL  : VIKRAM BAJAJ DT 29.10.2019
           ENDLOOP .
*****EOC CHARM ID 4000001049 : FUNCTIONAL : PRATIBHA DANGWAL , TECHNICAL  : VIKRAM BAJAJ DT 29.10.2019
           lo_send_request->send(
           EXPORTING
           i_with_error_screen = 'X'
           RECEIVING
           result = lv_sent_to_all ).
           COMMIT WORK.
         CATCH cx_address_bcs .
           RAISE cx_address_bcs .
         CATCH cx_send_req_bcs .
           RAISE cx_send_req_bcs .
       ENDTRY .
       CLEAR lwa_yrga_email_nomi .
*     ENDLOOP .

*   ENDLOOP .

     ENDIF.

* EOC by Piyush Tiwari/Pratibha Dangwal for Charm 4000007034

     MESSAGE 'Mail sent' TYPE 'S'.
   ENDIF .
* ENDIF .
********* eoc charm 613 .
ENDFORM.                     " PRINT_PDF
*&                                                                        *
*&       Form E03_EVENTTAB_BUILD
*&                                                                        *
*        text
*                                                                         *
*         >P_GT_EVENTS[] text
*                                                                         *
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
ENDFORM.                      " E03_EVENTTAB_BUILD
*&                                                                    *
*&        Form E04_COMMENT_BUILD
*&                                                                    *
*         text
*                                                                     *
*          >P_GT_LIST_TOP_OF_PAGE[] text
*                                                                     *
FORM e04_comment_build USING e04_lt_top_of_page TYPE slis_t_listheader.
* data: ls_line type slis_listheader.
* data: w_open_balance TYPE YY_OIJ_CUMIMB .
* SELECT YY_OIJ_CUMIMB INTO w_open_balance
*     FROM YRG_CUMM_IMB where
*
* clear ls_line.
* ls_line-typ = 'A'.
* CONCATENATE 'Opening Balance' w_open_balance INTO ls_line-info
* SEPARATED BY space.
** ls_line-info = 'SAch'.
* append ls_line to e04_lt_top_of_page.
ENDFORM.                      " E04_COMMENT_BUILD

*                                                                     *
*       FORM TOP_OF_PAGE                                              *
*                                                                     *
*       ........                                                      *
*                                                                     *
FORM top-of-page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
*     I_LOGO             = 'ENJOY_SAP'
      it_list_commentary = gt_list_top_of_page.

ENDFORM.                    "TOP_OF_PAGE
*&                                                                   *
*&      Form CHANGE_SO
*&                                                                   *
*       text
*                                                                    *
*        >P_WA_DISPLAY1_VBELN1 text
*                                                                    *
FORM change_so USING     w_vbeln1.
*    TABLES: VBAK, VBAP,VBKD.

** -> Begin of changes by of Aditi on 19/11/24 for ATC
* SELECT SINGLE * FROM vbkd
*                   WHERE
*                   vbeln = w_vbeln1
*                   AND
*                   posnr = '000010'.
  SELECT * FROM vbkd UP TO 1 ROWS
                   WHERE
                   vbeln = w_vbeln1
                   AND
                   posnr = '000010'
                   ORDER BY PRIMARY KEY.
  ENDSELECT.
** <- End changes by of Aditi on 19/11/24 for ATC
  IF sy-subrc = 0.
    vbkd-bstdk = s_date-low.
    vbkd-bstdk_e = s_date-high.
    vbkd-fbuda = s_date-high.
    MODIFY vbkd.
    COMMIT WORK AND WAIT.
  ELSE.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
*    SELECT SINGLE * FROM vbkd
*    WHERE
*    vbeln = w_vbeln1 .
    SELECT * FROM vbkd
    UP TO 1 ROWS
    WHERE
    vbeln = w_vbeln1
    ORDER BY PRIMARY KEY.
    ENDSELECT.
** <- End changes by of Aditi on 19/11/24 for ATC
    IF sy-subrc = 0 .
      vbkd-bstdk = s_date-low.
      vbkd-bstdk_e = s_date-high.
      vbkd-fbuda = s_date-high.
      MODIFY vbkd.
      COMMIT WORK AND WAIT.
    ENDIF.
  ENDIF.

  SELECT SINGLE * FROM vbak
    WHERE
    vbeln = w_vbeln1.
  IF sy-subrc = 0.
    "Start of change by XSAP_AB on 24th July,2024
    IF r_dds IS NOT INITIAL.
      vbak-augru = 'G32'.
    ELSE.
      vbak-augru = 'G20'.
    ENDIF.
    "End of change by XSAP_AB on 24th July,2024
    MODIFY vbak.
    COMMIT WORK AND WAIT.
  ENDIF.

  CALL FUNCTION 'YG_CHANGE_CONFIG'
    EXPORTING
      p_vbeln = w_vbeln1.

ENDFORM.                     " CHANGE_SO
*&                                                                       *
*&       Form ADDITIONAL_QTY
*&                                                                       *
*        text
*                                                                        *
*         >P_WA_DISPLAY1 text
*                                                                        *
FORM additional_qty USING      p_wa_display1 TYPE ty_display1.
   DATA l_sr TYPE ysrno.
   DATA: it_yrva_zcontrcls TYPE TABLE OF yrva_zcontrcls,
         wa_yrva_zcontrcls TYPE yrva_zcontrcls,
         lv_vbeln          TYPE vbeln.

  IF p_wa_display1-vbeln1 IS NOT INITIAL.
    lv_vbeln = p_wa_display1-vbeln1.
  ELSE.
    lv_vbeln = ls_oikexport-kdauf.
  ENDIF.
  l_sr = 1.
*           GET TOTAL QTYS IN UOM IN WHICH PRICING IS MAINTEND
  CLEAR: w_pos_imb_charg,w_neg_imb_charg.
  READ TABLE it_978 WITH KEY oicontnr = wa_vbak1-vbeln.
  IF sy-subrc EQ 0 .
    READ TABLE it_konp INTO wa_konp WITH KEY knumh = it_978-knumh.
  ELSE.
    READ TABLE it_a305 WITH KEY kunnr = wa_display1-kunnr matnr = wa_dis  play1-matnr.
    IF sy-subrc EQ 0 .
      READ TABLE it_konp INTO wa_konp WITH KEY knumh = it_a305-knumh.
    ENDIF.
  ENDIF.

  IF wa_konp-kmein EQ 'SM3'.
    w_pos_imb_charg = p_wa_display1-totalpic1 .
    w_neg_imb_charg = p_wa_display1-totalnic1.
    w_dds_imb_charg = p_wa_display1-totaldds1.
    w_price_uom = 'SM3' .
  ELSE.
    w_pos_imb_charg = p_wa_display1-totalpic .
    w_neg_imb_charg = p_wa_display1-totalnic.
    w_dds_imb_charg = p_wa_display1-totaldds.
    w_price_uom = ls_oikload-vrkme .
  ENDIF.

  IF w_price_uom IS INITIAL.
    READ TABLE it_oij_el_cp_layt INTO wa_oij_el_cp_layt WITH KEY vbeln  = wa_display1-vbeln.
    IF sy-subrc = 0.
      w_price_uom = wa_oij_el_cp_layt-mdq_uom.
    ENDIF.
  ENDIF.
  IF lv_vbeln IS NOT INITIAL .
*   Detele Already present entries (if any )
    DELETE FROM yrva_zcontrcls WHERE yyvbeln = lv_vbeln.
  ENDIF.
  IF r_d5 = 'X' ."or r_dds = 'X'.
    IF p_wa_display1-totalpic <> 0." and p_wa_display1-totaldds is INITI  AL.
*              wa_yrva_zcontrcls-yyvbeln = wa_display12-vbeln1.
      wa_yrva_zcontrcls-yyvbeln = lv_vbeln.
      wa_yrva_zcontrcls-yyposnr = '000010'.
      wa_yrva_zcontrcls-yysrno = l_sr.                      "'01'.
      wa_yrva_zcontrcls-yyclause_id = '10'.
      wa_yrva_zcontrcls-yyzmeng = w_pos_imb_charg . "WA_DISPLAY11-TOTALP  IC.
      wa_yrva_zcontrcls-yyzieme = w_price_uom . "LS_OIKLOAD-VRKME .
      APPEND wa_yrva_zcontrcls TO it_yrva_zcontrcls.
      MODIFY yrva_zcontrcls FROM wa_yrva_zcontrcls.
      CLEAR: wa_yrva_zcontrcls.
    ENDIF.


    IF p_wa_display1-totalnic <> 0.
*              wa_yrva_zcontrcls-yyvbeln = wa_display12-vbeln1.
      wa_yrva_zcontrcls-yyvbeln = lv_vbeln.
      wa_yrva_zcontrcls-yyposnr = '000010'.
      wa_yrva_zcontrcls-yysrno = l_sr.
      wa_yrva_zcontrcls-yyclause_id = '11'.
      wa_yrva_zcontrcls-yyzmeng = w_neg_imb_charg * -1 . "WA_DISPLAY11-T  OTALNIC * -1.
      wa_yrva_zcontrcls-yyzieme = w_price_uom . "LS_OIKLOAD-VRKME .
      APPEND wa_yrva_zcontrcls TO it_yrva_zcontrcls.
      MODIFY yrva_zcontrcls FROM wa_yrva_zcontrcls.
      CLEAR: wa_yrva_zcontrcls.
      l_sr = l_sr + 1.
    ENDIF.
  ENDIF.


    IF p_wa_display1-totaldds <> 0 AND r_dds = 'X'..
*                wa_yrva_zcontrcls-yyvbeln = wa_display12-vbeln1.
      wa_yrva_zcontrcls-yyvbeln = lv_vbeln.
      wa_yrva_zcontrcls-yyposnr = '000010'.
      wa_yrva_zcontrcls-yysrno = l_sr.                      "'01'.
      wa_yrva_zcontrcls-yyclause_id = '15'.
      wa_yrva_zcontrcls-yyzmeng = w_dds_imb_charg . "WA_DISPLAY11-TOTALPIC  .
      wa_yrva_zcontrcls-yyzieme = w_price_uom . "LS_OIKLOAD-VRKME .
      APPEND wa_yrva_zcontrcls TO it_yrva_zcontrcls.
      MODIFY yrva_zcontrcls FROM wa_yrva_zcontrcls.
      CLEAR: wa_yrva_zcontrcls.
    ENDIF.

    IF p_wa_display1-totalcharge IS NOT INITIAL AND r_d2 = 'X'.
*                wa_yrva_zcontrcls-yyvbeln = wa_display12-vbeln1.
      wa_yrva_zcontrcls-yyvbeln = lv_vbeln.
      wa_yrva_zcontrcls-yyposnr = '000010'.
      wa_yrva_zcontrcls-yysrno = l_sr.
      wa_yrva_zcontrcls-yyclause_id = '12'.
      wa_yrva_zcontrcls-yyzmeng = p_wa_display1-totalcharge.
      wa_yrva_zcontrcls-yyzieme = ls_oikload-vrkme .
      APPEND wa_yrva_zcontrcls TO it_yrva_zcontrcls.
    MODIFY yrva_zcontrcls FROM wa_yrva_zcontrcls.
    CLEAR: wa_yrva_zcontrcls.
    l_sr = l_sr + 1.
  ENDIF.
  COMMIT WORK AND WAIT.
  REFRESH : it_yrva_zcontrcls.
  CLEAR wa_yrva_zcontrcls.

ENDFORM.                      " ADDITIONAL_QTY
*&                                                                      *
*&        Form ENTRY_IMB_TABLE
*&                                                                      *
*         text
*                                                                       *
*          >P_WA_DISPLAY1 text
*                                                                       *
FORM entry_imb_table USING       p_wa_display1 TYPE ty_display1.
** -> Begin of changes by of Aditi on 19/11/24 for ATC
* SELECT SINGLE * FROM yrg_cumm_imb
*                    INTO wa_yrg_cumm_imb
*                    WHERE knkli = p_wa_display1-knkli
*                      AND kunnr = p_wa_display1-kunnr
*                      AND yy_contract = p_wa_display1-vbeln
*                      AND begda = s_date-low
*                      AND endda = s_date-high.
   SELECT * FROM yrg_cumm_imb UP TO 1 ROWS
                    INTO wa_yrg_cumm_imb
                    WHERE knkli = p_wa_display1-knkli
                      AND kunnr = p_wa_display1-kunnr
                      AND yy_contract = p_wa_display1-vbeln
                      AND begda = s_date-low
                      AND endda = s_date-high
                    ORDER BY PRIMARY KEY.
   ENDSELECT.
** <- End changes by of Aditi on 19/11/24 for ATC
   IF sy-subrc NE 0.
     wa_yrg_cumm_imb-knkli = p_wa_display1-knkli.
     wa_yrg_cumm_imb-kunnr = p_wa_display1-kunnr.
     wa_yrg_cumm_imb-yy_contract = p_wa_display1-vbeln.
     wa_yrg_cumm_imb-begda = s_date-low.
     wa_yrg_cumm_imb-endda = s_date-high.
     CALL FUNCTION 'PK_DATE_TIME_INTO_TIMESTAMP'
       EXPORTING
          iv_pkldt = sy-datum
          iv_pkluz = sy-uzeit
          is_t001w = is_t001w
       IMPORTING
          ev_pktim = wa_yrg_cumm_imb-timestamp.
   ENDIF.
   wa_yrg_cumm_imb-vbeln = p_wa_display1-vbeln1.
  "Start of insert by XSAP_AB on 27th May,2024
* READ TABLE it_gta_imb_sft INTO DATA(wa_imb_sft) WITH KEY contract_fro  m = p_wa_display1-vbeln.
* IF sy-subrc IS INITIAL.
*     READ TABLE it_display1 TRANSPORTING NO FIELDS WITH KEY vbeln = wa_i  mb_sft-contract_to.
*     IF sy-subrc IS INITIAL.
*       wa_yrg_cumm_imb-yy_oij_cumimb = 0.
*     ELSE.
*       wa_yrg_cumm_imb-yy_oij_cumimb = p_wa_display1-totalcumi.
*     ENDIF.
*  ELSE.
  wa_yrg_cumm_imb-yy_oij_cumimb = p_wa_display1-totalcumi.
* ENDIF.
 "End of Insert by XSAP_AB on 27th May,2024
  wa_yrg_cumm_imb-ldatum = sy-datum.
  wa_yrg_cumm_imb-ernam = sy-uname.
  wa_yrg_cumm_imb-augru = p_wa_display1-augru.
  IF r_dds = 'X'.
    MODIFY yrg_cumm_dds FROM wa_yrg_cumm_imb .
  ELSE.
    MODIFY yrg_cumm_imb FROM wa_yrg_cumm_imb .
  ENDIF.
  COMMIT WORK AND WAIT.

ENDFORM.                    " ENTRY_IMB_TABLE
*&                                                                        *
*&       Form UPDATE_ZCONTROL_TABLE
*&                                                                        *
*        text
*                                                                         *
*    > p1         text
* <     p2        text
*                                                                         *
FORM update_zcontrol_table .

ENDFORM.
*&                                                                        *
*&       Form DISPLAY_WT_AVG_PRC
*&                                                                        *
*        text
*                                                                         *
*     > p1         text
* <      p2        text
*                                                                         *
FORM display_wt_avg_prc .
   CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
     EXPORTING
       text = 'Displaying Report. Please wait...'.

    TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = gr_table
        CHANGING
** -> Begin of changes by of Aditi on 19/11/24 for ATC
          t_table      = it_wt_prc ).                   "#EC CI_NOORDER
** <- End changes by of Aditi on 19/11/24 for ATC
    CATCH cx_salv_msg.
      MESSAGE 'Error occured while generating SALV' TYPE 'I'.
      LEAVE LIST-PROCESSING.
  ENDTRY.
  gr_functions = gr_table->get_functions( ).
  gr_functions->set_all( abap_true ).

    gr_display = gr_table->get_display_settings( ).
    gr_display->set_striped_pattern( abap_true ).
    gr_display->set_list_header('Weighted Average Price').
    gr_layout = gr_table->get_layout( ).
    gv_key-report = sy-repid.
    gr_layout->set_key( gv_key ).
    gr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  gr_layout->set_default( abap_true ).
  TRY .
      gr_columns = gr_table->get_columns( ).
      gr_column = gr_columns->get_column( 'MANDT' ).
      gr_column->set_technical( 'X' ).
    CATCH cx_salv_not_found.
      RAISE cx_salv_not_found .
  ENDTRY.
  gr_table->display( ).
ENDFORM.
"Start by XSAP_AB on 19th June,2024
FORM top.
  "Adding Header in ALV report to display details of Imbalance
  DATA : lv_imbal TYPE string.

  CLEAR : gt_list_top_of_page.

  wa_list_top_of_page-typ = 'H'.
  wa_list_top_of_page-info = 'Details of Imbalance transferred from Pred  ecessor Contracts:'.
  APPEND wa_list_top_of_page TO gt_list_top_of_page.
  CLEAR wa_list_top_of_page.

  LOOP AT it_header INTO wa_header.
    wa_list_top_of_page-typ = 'S'.
    lv_imbal = wa_header-imbal.
    CONCATENATE 'Imb' lv_imbal 'shifted from' wa_header-p_cont 'to' wa_h  eader-vbeln INTO wa_list_top_of_page-info SEPARATED BY space.
    APPEND wa_list_top_of_page TO gt_list_top_of_page.
    CLEAR wa_list_top_of_page.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gt_list_top_of_page.

ENDFORM.                    "TOP_OF_PAGE
"End by XSAP_AB on 19th June,2024
