*&                                                                    *&
*&  PROGRAM NAME          : YRVU001_REB_CALC_M2                       *&
*&  TRANSACTION CODE      : YRVU015                                   *&
*&  MODULE                : SD                                        *&
*&  PACKAGE               : YV01                                      *&
*&  DEVELOPED BY          : UJJWAL JAIN                               *&
*&  VERSION               : 1.0                                       *&
*&  TYPE                  : EXECUTABLE                                *&
*&  FUNCTIONAL CONSULTANT : PRIYANKA SINGH/ANURAG MEHTA               *&
*&  RECEIVED DATE         : 16.09.2019                                *&
*&  FUN. SPECS RECIVED    : NO                                        *&
*&  TRANSPORT REQ         : DVRK9A0GCG                                *&
*&                                                                    *&
*& CHANGE HISTORY : THIS PROGRAM IS COPY OF YRVU001_REB_CALC WITH
*&                  ADDITIONAL CHANGES SUGGESTED BY PRIYANKA/ANURAG
*                   YRVU016 IS FOR VARIANT RUN PROGRAM                *&
*&                                                                     *
*& CHANGE DATE   CHANGED BY   CHANGE ID   DESCRIPTION
*&
*&  10-02-2020   UJJWAL JAIN 400001548
*&
* PROGRAM DESCRIPTION: FOR REBATE CALCULATIONS THE REBATE APPLICABLE IS
*              CALCULATED BASED ON THE UPLIFTMENT QUANTITIES. THESE   *
*              WILL BE GIVEN IN A REPORT AND ON EXECUTING THE         *
*               CREDIT MEMOS WILL HAVE TO BE CREATED.                 *
*                                                                     *
REPORT yrvu001_reb_calc_m2  NO STANDARD PAGE HEADING
               MESSAGE-ID yv01 .
*INCLUDES                                                             *
*TABLES - SAP TABLES                                                  *
TABLES : s925,
         vbak,                              "FOR REBATES
         kna1,
         yrva_rebate,
         vbrp,
         konp,
         s922.
*TABLES - CUSTOM TABLES                                               *
*VIEWS                                                                *
*STRUCTURES                                                           *
DATA : x_order_header_in LIKE bapisdhead,
       x_return_commit   LIKE bapireturn1,
       x_sold_to_party   LIKE bapisoldto,
       x_bapisdhd1       LIKE bapisdhd1.
*TYPES                                                                *
TYPE-POOLS : slis.
*INTERNAL TABLES                                                      *
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.
DATA : i_layout     TYPE slis_layout_alv,
       i_exit_event TYPE slis_t_event_exit WITH HEADER LINE.
DATA: i_events TYPE slis_t_event WITH HEADER LINE.
DATA: BEGIN OF i_reb1 OCCURS 0,
        name1    LIKE kna1-name1,
        pkunag   LIKE s925-pkunag,              "SOLD-TO PARTY
        vkbur    LIKE s925-vkbur,               "SALES OFFICE
        vkorg    LIKE s925-vkorg,               "SALES ORGANIZATION
        kvgr2    LIKE s925-kvgr2,               "GROUP COMPANY
        werks    LIKE s925-werks,               "PLANT
*         KVGR3     LIKE S922-KVGR3,               "LOCATION
*         REGIO_LIS LIKE S922-REGIO_LIS,           " REGION
        kondm    LIKE s925-kondm,               "MATERIAL PRICING GROUP
        kdgrp    LIKE s925-kdgrp,               "CUSTOMER GROUP
        kukla    LIKE kna1-kukla,               "CUSTOMER CLASSIFICATION
        ummenge  LIKE s925-ummenge,             "INVOICED QUANTITY
*         VALUE    LIKE S925-UMMENGE,
        value    LIKE   yrva_rebate-value,  "(QUANTITY * RATE)
        line     LIKE sy-linno,                 "LINE NO
        page     LIKE sy-cpage,                 "PAGE NO
        check(1),                              "CHECK INDICATOR(X)
      END OF i_reb1.
DATA: BEGIN OF i_reb_1 OCCURS 0,
        name1    LIKE kna1-name1,
        pkunag   LIKE s925-pkunag,              "SOLD-TO PARTY
        vkbur    LIKE s925-vkbur,               "SALES OFFICE
        vkorg    LIKE s925-vkorg,               "SALES ORGANIZATION
        kvgr2    LIKE s925-kvgr2,               "GROUP COMPANY
        werks    LIKE s925-werks,               "PLANT
*         KVGR3     LIKE S922-KVGR3,               "LOCATION
*         REGIO_LIS LIKE S922-REGIO_LIS,           " REGION
        kondm    LIKE s925-kondm,               "MATERIAL PRICING GROUP
        kdgrp    LIKE s925-kdgrp,               "CUSTOMER GROUP
        kukla    LIKE kna1-kukla,               "CUSTOMER CLASSIFICATION
        ummenge  LIKE s925-ummenge,             "INVOICED QUANTITY
*         VALUE    LIKE S925-UMMENGE,
        value    LIKE   yrva_rebate-value,  "(QUANTITY * RATE)
        line     LIKE sy-linno,                 "LINE NO
        page     LIKE sy-cpage,                 "PAGE NO
        check(1),                              "CHECK INDICATOR(X)
        ummenge_1  LIKE s925-ummenge,             "INVOICED QUANTITY
      END OF i_reb_1.
DATA: BEGIN OF i_reb7 OCCURS 0,
        name1    LIKE kna1-name1,
        pkunag   LIKE s925-pkunag,              "SOLD-TO PARTY
        vkbur    LIKE s925-vkbur,               "SALES OFFICE
        vkorg    LIKE s925-vkorg,               "SALES ORGANIZATION
        kvgr2    LIKE s925-kvgr2,               "GROUP COMPANY
        werks    LIKE s925-werks,               "PLANT
*         KVGR3     LIKE S922-KVGR3,               "LOCATION
*         REGIO_LIS LIKE S922-REGIO_LIS,           " REGION
        kondm    LIKE s925-kondm,               "MATERIAL PRICING GROUP
        kdgrp    LIKE s925-kdgrp,               "CUSTOMER GROUP
        kukla    LIKE kna1-kukla,               "CUSTOMER CLASSIFICATION
        ummenge  LIKE s925-ummenge,             "INVOICED QUANTITY
*         VALUE    LIKE S925-UMMENGE,
        value    LIKE   yrva_rebate-value,  "(QUANTITY * RATE)
        line     LIKE sy-linno,                 "LINE NO
        page     LIKE sy-cpage,                 "PAGE NO
        check(1),                              "CHECK INDICATOR(X)
      END OF i_reb7.
DATA: BEGIN OF i_reb_kdgrp OCCURS 0,
        vkbur    LIKE s925-vkbur,               "SALES OFFICE
        vkorg    LIKE s925-vkorg,               "SALES ORGANIZATION
        pkunag   LIKE s925-pkunag,              "SOLD-TO PARTY
        kvgr2    LIKE s925-kvgr2,               "GROUP COMPANY
        kvgr3    LIKE s922-kvgr3,               "LOCATION
        kondm    LIKE s925-kondm,               "MATERIAL PRICING GROUP
        kdgrp    LIKE s925-kdgrp,               "CUSTOMER GROUP
        werks    LIKE s925-werks,               "PLANT
        ummenge  LIKE s925-ummenge,             "INVOICED QUANTITY
*         VALUE    LIKE S925-UMMENGE,
        value    LIKE   yrva_rebate-value,  "(QUANTITY * RATE)
        line     LIKE sy-linno,                 "LINE NO
        page     LIKE sy-cpage,                 "PAGE NO
        check(1),                              "CHECK INDICATOR(X)
      END OF i_reb_kdgrp,
      i_reb3 LIKE STANDARD TABLE OF i_reb1 WITH HEADER LINE.
DATA: BEGIN OF i_reb2 OCCURS 0,
        vkbur    LIKE s925-vkbur,               "SALES OFFICE
        kukla    TYPE kukla,
*        VKORG   LIKE S925-VKORG,               "SALES ORGANIZATION
        pkunag   LIKE s925-pkunag,              "SOLD-TO PARTY
        name1    LIKE kna1-name1,
        kvgr2    LIKE s925-kvgr2,               "GROUP COMPANY
        werks    LIKE s925-werks,               "PLANT
        kvgr3    LIKE s922-kvgr3,               "LOCATION
        kdgrp    LIKE s925-kdgrp,               "CUSTOMER GROUP
        kunnr    TYPE kunnr,     " SOC BY UJJWAL/PRIYANKA ON 10-02-2020
        ummenge  LIKE s925-ummenge,                  "INVOICED QUANTITY
        ummenge1 LIKE s925-ummenge,                  "INVOICED QUANTITY
        kbetr    LIKE konm-kbetr,
*         VALUE    LIKE S925-UMMENGE,
        value    LIKE   yrva_rebate-value,  "(QUANTITY * RATE)
        line     LIKE sy-linno,                 "LINE NO
        page     LIKE sy-cpage,                 "PAGE NO
        check(1),
        rem(20),
        bezei    LIKE tvkbt-bezei, " ASHOK & M A KHAN 07.06.2007
        " SOC ADDED BY UJJWAL/PRIYANKA ON 7-07-2020 ON CHRM 4000002361 T
        vbeln    LIKE yrva_rebate-vbeln,
        " EOC ADDED BY UJJWAL/PRIYANKA ON 7-07-2020 ON CHRM 4000002361 T
      END OF i_reb2.
DATA: BEGIN OF i_reb_2 OCCURS 0,
        vkbur    LIKE s925-vkbur,               "SALES OFFICE
        kukla    TYPE kukla,
*        VKORG   LIKE S925-VKORG,               "SALES ORGANIZATION
        pkunag   LIKE s925-pkunag,              "SOLD-TO PARTY
        name1    LIKE kna1-name1,
        kvgr2    LIKE s925-kvgr2,               "GROUP COMPANY
        werks    LIKE s925-werks,               "PLANT
        kvgr3    LIKE s922-kvgr3,               "LOCATION
        kdgrp    LIKE s925-kdgrp,               "CUSTOMER GROUP
        kunnr    TYPE kunnr,     " SOC BY UJJWAL/PRIYANKA ON 10-02-2020
        ummenge  LIKE s925-ummenge,                  "INVOICED QUANTITY
        ummenge1 LIKE s925-ummenge,                  "INVOICED QUANTITY
        kbetr    LIKE konm-kbetr,
*         VALUE    LIKE S925-UMMENGE,
        value    LIKE   yrva_rebate-value,  "(QUANTITY * RATE)
        line     LIKE sy-linno,                 "LINE NO
        page     LIKE sy-cpage,                 "PAGE NO
        check(1),
        rem(20),
        bezei    LIKE tvkbt-bezei, " ASHOK & M A KHAN 07.06.2007
        " SOC ADDED BY UJJWAL/PRIYANKA ON 7-07-2020 ON CHRM 4000002361 T
        vbeln    LIKE yrva_rebate-vbeln,
        " EOC ADDED BY UJJWAL/PRIYANKA ON 7-07-2020 ON CHRM 4000002361 T
      END OF i_reb_2.
DATA: BEGIN OF i_reb5 OCCURS 0,
        pkunag   LIKE s925-pkunag,
        ummenge1 LIKE s922-ummenge,
      END OF i_reb5,
      BEGIN OF i_reb6 OCCURS 0,
        pkunag   LIKE s925-pkunag,
        ummenge1 LIKE s922-ummenge,
      END OF i_reb6.
DATA : BEGIN OF i_cond OCCURS 0,
         knumh LIKE konm-knumh,
         kbetr LIKE konm-kbetr,
         kstbm LIKE konm-kstbm,
       END OF i_cond,
       BEGIN OF i_scmz OCCURS 0,
         pkunag  LIKE s925-pkunag,
         ummenge LIKE s925-ummenge,                  "INVOICED QUANTITY
       END OF i_scmz.
DATA : i_cond1 LIKE i_cond OCCURS 0 WITH HEADER LINE.
DATA : BEGIN OF i_pkunag OCCURS 0,
         pkunag LIKE s925-pkunag,
       END OF i_pkunag.
DATA: lt_return TYPE STANDARD TABLE OF ddshretval.
DATA : BEGIN OF i_kukla OCCURS 0,
         kunnr LIKE kna1-kunnr,
         kukla LIKE kna1-kukla,
         name1 LIKE kna1-name1,
       END OF i_kukla.
* BOC ASHOK & M A KHAN 07.06.07
DATA: BEGIN OF i_tvkbt OCCURS 0,
        vkbur LIKE tvkbt-vkbur,
        bezei LIKE tvkbt-bezei,
      END OF i_tvkbt.
* EOC ASHOK & M A KHAN 07.06.07
* BOC ASHOK & M A KHAN 09.01.12
DATA: BEGIN OF i_vbrk OCCURS 0,
        vbeln LIKE vbrk-vbeln,
        fkart LIKE vbrk-fkart,
        fkdat LIKE vbrk-fkdat,
        kunag LIKE vbrk-kunag,
        sfakn LIKE vbrk-sfakn,
      END OF i_vbrk,
      i_vbrk_t LIKE i_vbrk OCCURS 0 WITH HEADER LINE,
      BEGIN OF i_vbrp OCCURS 0,
        vbeln LIKE vbrp-vbeln,
        fkimg LIKE vbrp-fkimg,
        kondm LIKE vbrp-kondm,
        kunag LIKE vbrk-kunag,
      END OF i_vbrp,
      BEGIN OF i_vbrp_tmp OCCURS 0,
        vbeln LIKE vbrp-vbeln,
        fkimg LIKE vbrp-fkimg,
        kondm LIKE vbrp-kondm,
        kunag LIKE vbrk-kunag,
      END OF i_vbrp_tmp,
      w_kunag LIKE vbrk-kunag.
* EOC ASHOK & M A KHAN 09.01.12
TYPES: BEGIN OF ty_data,
         text TYPE char20,
       END OF ty_data.
DATA: lt_custclss TYPE STANDARD TABLE OF ty_data,
      ls_custclss TYPE ty_data.
DATA : i_a350 LIKE a350 OCCURS 0.
DATA : i_a307 LIKE a307 OCCURS 0.
DATA : flag      TYPE numc1,
       lv_index  TYPE sy-tabix,  "ADDED BY KUNAL/PRIYANKA ON 05/02/2018
       lv_index1 TYPE sy-tabix. "ADDED BY VAIBHAV
DATA : WA_REC LIKE A350.
DATA : i_order_partners LIKE bapipartnr  OCCURS 0 WITH HEADER LINE,
       i_order_items_in LIKE bapiitemin  OCCURS 0 WITH HEADER LINE.
*VARIABLES                                                            *
*   COUNTERS
*   FLAGS
*   SUMS
*   VARIABLES
DATA : w_kbetr    LIKE konm-kbetr,
       w_vbeln    LIKE bapivbeln-vbeln,
       w_objtype  LIKE bapiusw01-objtype,
       w_auart    LIKE bapisdhead-doc_type,
       w_matnr    LIKE mara-matnr,
       w_vkbur    LIKE i_reb1-vkbur,
       w_vkorg    LIKE i_reb1-vkorg,
       w_pkunag   LIKE i_reb1-pkunag,
       w_kvgr2    LIKE i_reb1-kvgr2,
       w_curr_lin LIKE sy-tabix,
       w_next_lin LIKE sy-linno.
* NEW VARIABLE DECLARED ON 03.04.2014
DATA:  flag_70 TYPE c.
DATA : w_vbeln1 TYPE vbeln.
DATA: BEGIN OF i_reb_bkp OCCURS 0,
        name1    LIKE kna1-name1,
        pkunag   LIKE s925-pkunag,              "SOLD-TO PARTY
        vkbur    LIKE s925-vkbur,               "SALES OFFICE
        vkorg    LIKE s925-vkorg,               "SALES ORGANIZATION
        kvgr2    LIKE s925-kvgr2,               "GROUP COMPANY
*         KVGR3     LIKE S922-KVGR3,               "LOCATION
*         REGIO_LIS LIKE S922-REGIO_LIS,         " REGION
        kondm    LIKE s925-kondm,               "MATERIAL PRICING GROUP
        kdgrp    LIKE s925-kdgrp,               "CUSTOMER GROUP
        werks    LIKE s925-werks,               "PLANT
        kukla    LIKE kna1-kukla,               "CUSTOMER CLASSIFICATION
        ummenge  LIKE s925-ummenge,             "INVOICED QUANTITY
*         VALUE    LIKE S925-UMMENGE,
        value    LIKE   yrva_rebate-value,  "(QUANTITY * RATE)
        line     LIKE sy-linno,                 "LINE NO
        page     LIKE sy-cpage,                 "PAGE NO
        check(1),                              "CHECK INDICATOR(X)
      END OF i_reb_bkp.
*BOC CHARM ID 4000003218 TECHNICAL VIKRAM BAJAJ FUNCTIONAL PRIYANKA SING
DATA : gs_yrva_varientlog TYPE yrva_varientlog .
DATA : gs_yrva_varientlog1 TYPE yrva_varientlog ."02.02.2021
DATA : gv_ques TYPE string .
DATA : gv_ans(1) .
*EOC CHARM ID 4000003218 TECHNICAL VIKRAM BAJAJ FUNCTIONAL PRIYANKA SING
* END OF NEW CODE DECLARATION
*CONSTANTS                                                            *
DATA: gd_line TYPE i.
*SELECTION-SCREEN
SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN SKIP.
*BOC CHARM ID 4000003218 TECHNICAL VIKRAM BAJAJ FUNCTIONAL PRIYANKA SING
  PARAMETERS :pc1 AS CHECKBOX ."
* PARAMETERS : PREL TYPE DATUM . " COMMENTED ON 08.02.2021
*EOC CHARM ID 4000003218 TECHNICAL VIKRAM BAJAJ FUNCTIONAL PRIYANKA SING
  SELECT-OPTIONS  :
    s_sptag    FOR s925-sptag OBLIGATORY NO-EXTENSION,
    s_vkorg  FOR s925-vkorg  OBLIGATORY NO INTERVALS NO-EXTENSION,
    s_vkbur  FOR s925-vkbur  NO INTERVALS  NO-EXTENSION,
    s_pkunag FOR s925-pkunag NO INTERVALS, "NO-EXTENSION,
    s_auart FOR vbak-auart, " SOC by Vaishnavi/Archna TR: DVRK9A1OTG
    s_kukla  FOR kna1-mcod1 NO INTERVALS, "NO INTERVALS NO-EXTENSION,
    s_kukla2  FOR kna1-kukla NO-DISPLAY, " SOC by Vaishnavi/Archna TR: D
    s_kondm  FOR s925-kondm  OBLIGATORY,
    s_dist   FOR x_bapisdhd1-distr_chan OBLIGATORY,
                                      "DISTRIBUTATION CHANNAL
    s_div    FOR x_bapisdhd1-division NO-DISPLAY,
    s_kdgrp  FOR s925-kdgrp,
    s_kvgr2  FOR s925-kvgr2,
    s_kvgr3  FOR s922-kvgr3,
    s_ssour FOR s925-ssour NO-DISPLAY,
    s_regio FOR s922-regio_lis NO-DISPLAY,
*** CHNGE BY TCS:AM FUNC PRIYANKA DATED SEP 27, 2018.
    s_werks FOR vbrp-werks .
*** CHNGE BY TCS:AM FUNC PRIYANKA DATED SEP 27, 2018.
**SOC BY UJJWAL/PRIYANKA ON CHARM 40000002943 ON 14-10-2020 TO GET ZOPN
  PARAMETERS :  s_kvgr4 LIKE vbrp-kvgr4.
**EOC BY UJJWAL/PRIYANKA ON CHARM 40000002943 ON 14-10-2020 TO GET ZOPN
  PARAMETERS : p_kschl  LIKE konp-kschl OBLIGATORY,
               p_kschl1 LIKE konp-kschl.
  SELECTION-SCREEN SKIP.
SELECTION-SCREEN : END OF BLOCK b1.
* AT SELECTION SCREEN                                                  *
** SOC by Vaishnavi/Archna TR: DVRK9A1OTG
INITIALIZATION.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_kukla-low.
  PERFORM custclss.
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_kukla-high.
  PERFORM custclss.
** EOC by Vaishnavi/Archna TR: DVRK9A1OTG
AT SELECTION-SCREEN ON s_vkbur.
*CHECKING AUTHORIZATION
  AUTHORITY-CHECK OBJECT 'YV_VKBUR'
  ID 'VKBUR' FIELD s_vkbur-low ."I_S925-VKBUR.
  IF sy-subrc <> 0.
    IF s_vkbur-low IS INITIAL .
      flag = 0.
    ELSE.
      MESSAGE e081 WITH s_vkbur-low. "I_S925-VKBUR.
    ENDIF.
  ELSE.
    flag = 1.
  ENDIF.
AT SELECTION-SCREEN ON s_kvgr2.
  IF s_kvgr2-low IS NOT INITIAL AND s_vkbur-low IS INITIAL.
    AUTHORITY-CHECK OBJECT 'YV_KVGR2'
    ID 'YKVGR2' FIELD s_kvgr2-low ."I_S925-VKBUR.
    IF sy-subrc <> 0.
      MESSAGE e061 WITH s_vkbur-low. "I_S925-VKBUR.
    ENDIF.
  ENDIF.
AT SELECTION-SCREEN.
  IF s_vkbur-low IS INITIAL AND s_kvgr2-low IS INITIAL AND flag <> 1.
    MESSAGE e142.
*  MESSAGE 'EITHER ENTER SALES OFFICE OR GROUP COMPANY' TYPE 'E'.
  ENDIF.
* TOP OF PAGE                                                          *
TOP-OF-PAGE.
* START OF SELECTION                                                   *
START-OF-SELECTION.
*BOC CHARM ID 4000003218 TECHNICAL VIKRAM BAJAJ FUNCTIONAL PRIYANKA SING
  IF pc1 IS NOT INITIAL .
    gv_ques = TEXT-005.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'CONFIRMATION'
        text_question         = gv_ques
        text_button_1         = 'YES'
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = 'NO'
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = ' '
*       POPUP_TYPE            = 'ICON_MESSAGE_ERROR'
      IMPORTING
        answer                = gv_ans.
    IF gv_ans = 1.
      gs_yrva_varientlog-variant = sy-slset.
      gs_yrva_varientlog-checkby = sy-uname.
      gs_yrva_varientlog-checkon = sy-datum.
*                   GS_YRVA_VARIENTLOG-RELDAT  = PREL . COMMENTED ON 08.
      gs_yrva_varientlog-checkat = sy-uzeit.
      gs_yrva_varientlog-vkorg = s_vkorg-low.
      gs_yrva_varientlog-werks =  s_werks-low.
*                   IF PREL NE '00000000' . COMMENTED ON 08.02.2021
*                   GS_YRVA_VARIENTLOG-RELBY = SY-UNAME .
*                   ENDIF .
      INSERT yrva_varientlog FROM gs_yrva_varientlog .
      COMMIT WORK .
      IF sy-subrc = 0 .
        MESSAGE TEXT-006 TYPE 'S' .
      ELSE .
        MESSAGE TEXT-007 TYPE 'S' DISPLAY LIKE 'E' .
      ENDIF .
    ENDIF .
  ELSE ."
*EOC CHARM ID 4000003218 TECHNICAL VIKRAM BAJAJ FUNCTIONAL PRIYANKA SING
**SOC BY UJJWAL/PRIYANKA ON CHARM 40000002943 ON 14-10-2020 TO GET ZOPN
    IF s_kvgr4 IS NOT INITIAL AND s_kvgr4 NE 'ZOP'.
      MESSAGE 'CUSTOMER GROUP 4 SHOULD BE ZOP FOR ZOPN ORDER TYPE' TYPE
      LEAVE LIST-PROCESSING.
    ENDIF.
**EOC BY UJJWAL/PRIYANKA ON CHARM 40000002943 ON 14-10-2020 TO GET ZOPN
*  SET PF-STATUS 'PFSTAT'.
    SET PF-STATUS 'STANDARD' EXCLUDING 'CALC'     ##STAT_UNDEF . " MODIF
*CUSTOMER CLASSIFICATION
    SELECT kunnr kukla name1 FROM kna1
                       INTO TABLE i_kukla
                      WHERE kukla IN s_kukla
    AND kunnr IN s_pkunag.
**SOC BY UJJWAL/PRIYANKA ON CHARM 40000002943 ON 14-10-2020 TO GET ZOPN
    IF s_kvgr4 IS INITIAL.
      PERFORM filter_data.
    ELSE.
      PERFORM filter_data_1.
    ENDIF.
**EOC BY UJJWAL/PRIYANKA ON CHARM 40000002943 ON 14-10-2020 TO GET ZOPN
**  IF I_REB1[] IS NOT INITIAL AND S_VKBUR-LOW IS NOT INITIAL.
**    SELECT SPTAG VTWEG PKUNAG VKBUR VKORG KVGR2    KONDM
**      KDGRP UMMENGE INTO CORRESPONDING FIELDS OF
**                                 TABLE I_REB3
**                                 FROM S925
***                                 FOR ALL ENTRIES IN I_KUKLA
**                           WHERE SSOUR  IN S_SSOUR    AND
**                                 VRSIO  = '000'       AND
**                                 SPMON  = '000000'    AND
**                                 SPTAG IN S_SPTAG     AND
***                                 SPMON  IN S_MONTH    AND
**                                 SPWOC  = '000000'    AND
**                                 SPBUP  = '000000'    AND
**                                 VKORG  IN S_VKORG    AND
**                                 VTWEG  IN S_DIST     AND
***                                   REGIO_LIS  IN S_REGIO    AND
***                                 VKBUR  IN S_VKBUR    AND
**                                 KVGR2  IN S_KVGR2
**                                  AND KONDM IN S_KONDM
**                               AND KDGRP IN S_KDGRP
**                                AND WERKS IN S_WERKS.
**    DELETE I_REB3[] WHERE VKBUR IN S_VKBUR.
**    LOOP AT I_REB3.
**      READ TABLE I_REB1 WITH KEY PKUNAG = I_REB3-PKUNAG.
**      IF SY-SUBRC = 0.
**        MOVE-CORRESPONDING I_REB3 TO I_REB1.
**        APPEND I_REB1.
**      ENDIF.
**    ENDLOOP.
**  ENDIF.
*  ENDIF.
    DELETE i_reb1 WHERE NOT kondm  IN s_kondm.
    DELETE i_reb1 WHERE NOT kdgrp  IN s_kdgrp.
    IF s_sptag-high IS NOT INITIAL.
      SELECT vbeln fkart fkdat kunag sfakn INTO TABLE i_vbrk
      FROM vbrk
      WHERE vkorg = '5000'
      AND ( fkart IN ('ZP07', 'ZI07') OR fkart = 'S1' )
**SOC BY UJJWAL/PRIYANKA TO GET DISTRIBUTION CHANNEL WISE BILL DATA ON C
       AND  vtweg  IN s_dist
**EOC BY UJJWAL/PRIYANKA TO GET DISTRIBUTION CHANNEL WISE BILL DATA ON C
      AND fkdat GE s_sptag-low AND fkdat LE s_sptag-high
** -> BEGIN OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
      AND vbrk~draft = space.
** -> END OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
    ELSE.
      SELECT vbeln fkart fkdat kunag sfakn INTO TABLE i_vbrk
      FROM vbrk
      WHERE vkorg = '5000'
      AND ( fkart IN ('ZP07', 'ZI07') OR fkart = 'S1' )
**SOC BY UJJWAL/PRIYANKA TO GET DISTRIBUTION CHANNEL WISE BILL DATA ON C
       AND  vtweg  IN s_dist
**EOC BY UJJWAL/PRIYANKA TO GET DISTRIBUTION CHANNEL WISE BILL DATA ON C
      AND fkdat EQ s_sptag-low
** -> BEGIN OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
      AND vbrk~draft = space.
** -> END OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
    ENDIF.
    IF i_vbrk[] IS NOT INITIAL.
      i_vbrk_t[] = i_vbrk[].
      DELETE i_vbrk WHERE fkart = 'S1'.
      DELETE i_vbrk_t WHERE fkart = 'ZP07'.
      DELETE i_vbrk_t WHERE fkart = 'ZI07'.
      LOOP AT i_vbrk.
        READ TABLE i_vbrk_t WITH KEY sfakn = i_vbrk-vbeln.
        IF sy-subrc = 0.
          DELETE i_vbrk.
        ENDIF.
      ENDLOOP.
      IF i_vbrk[] IS NOT INITIAL.
        SELECT vbeln fkimg kondm FROM vbrp INTO TABLE i_vbrp FOR ALL ENT
        LOOP AT i_vbrp.
          READ TABLE i_vbrk WITH KEY vbeln = i_vbrp-vbeln.
          IF sy-subrc = 0.
            i_vbrp-kunag = i_vbrk-kunag.
            MODIFY i_vbrp TRANSPORTING kunag.
          ENDIF.
        ENDLOOP.
        IF i_vbrp[] IS NOT INITIAL.
          LOOP AT i_vbrp.
            CLEAR: i_vbrp-vbeln.
            COLLECT: i_vbrp INTO i_vbrp_tmp[].
          ENDLOOP.
        ENDIF.
        REFRESH: i_vbrp[].
        i_vbrp[] = i_vbrp_tmp[].
        IF i_vbrp[] IS NOT INITIAL.
          CLEAR w_kunag.
          BREAK xgail_ab2.
          LOOP AT i_reb1.
*          READ TABLE I_VBRP WITH KEY KUNAG = I_REB1-PKUNAG KONDM = I_RE
**  SOC BY KUNAL/PRIYANKA ON 05/02/2018 FOR FREE SAMPLE RESTRICTION ON P
*          IF SY-SUBRC = 0.
*            LV_INDEX = SY-TABIX.
*            LOOP AT I_VBRP FROM LV_INDEX.
*              LV_INDEX1 = SY-TABIX.
*              IF I_VBRP-KUNAG <> I_REB1-PKUNAG AND I_VBRP-KONDM <> I_RE
*                EXIT.
*              ELSE.
**  EOC BY KUNAL/PRIYANKA ON 05/02/2018 FOR FREE SAMPLE RESTRICTION ON P
*                IF W_KUNAG NE I_REB1-PKUNAG.
*                  I_REB1-UMMENGE = I_REB1-UMMENGE - I_VBRP-FKIMG.
*                  MODIFY I_REB1 TRANSPORTING UMMENGE.
*                  W_KUNAG = I_REB1-PKUNAG.
*                  DELETE I_VBRP INDEX LV_INDEX1.
*                  CLEAR LV_INDEX1.
*                ENDIF.
*              ENDIF.
*            ENDLOOP.
*          ENDIF.
            LOOP AT i_vbrp WHERE kunag = i_reb1-pkunag AND kondm = i_reb
SD:BUGFIX FOR POLYMER DISCOUNT FREE SAMP
              lv_index = sy-tabix.
              i_reb1-ummenge = i_reb1-ummenge - i_vbrp-fkimg.
              MODIFY i_reb1 TRANSPORTING ummenge.
              DELETE i_vbrp INDEX lv_index.
            ENDLOOP.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
* EOC:LOGIC FOR EXCLUDING FREE SAMPLE BY ASHOK & KHAN 11.01.2012
    SORT i_kukla BY kunnr.
    LOOP AT i_reb1.
      READ TABLE i_kukla WITH KEY kunnr = i_reb1-pkunag.
      IF sy-subrc <> 0.
        DELETE i_reb1.
      ENDIF.
    ENDLOOP.
    IF i_reb1[] IS INITIAL.
      MESSAGE i025.
      LEAVE LIST-PROCESSING.
    ENDIF.
    SORT i_kukla BY kunnr.
    LOOP AT i_reb1.
      CLEAR i_kukla.
      READ TABLE i_kukla WITH KEY kunnr = i_reb1-pkunag BINARY SEARCH.
      IF sy-subrc EQ 0.
        i_reb1-kukla = i_kukla-kukla.
        i_reb1-name1 = i_kukla-name1.
        MODIFY i_reb1 TRANSPORTING kukla name1.
      ENDIF.
    ENDLOOP.
*CHECKING FOR INITIAL
    IF i_reb1[] IS INITIAL.
      MESSAGE i025.
      LEAVE LIST-PROCESSING.
    ENDIF.
* SUMMATION FOR CUSTOMER GROUP
    SORT i_reb1 BY pkunag vkbur vkorg  kvgr2 kondm kdgrp.
* CONDITION TYPE IS NOT EQUAL TO 'ZMOU'
    IF p_kschl <> 'ZMOU'.
      SELECT * INTO TABLE i_a350 FROM a350 WHERE kappl = 'V'      AND
                                                 kschl = p_kschl  AND
                                                 vkorg IN s_vkorg AND
                                                 kfrst = ''       AND
*                                               DATBI GE SY-DATUM.    "
                                                 datab LE s_sptag-high A
      datbi GE s_sptag-low.   " ASHOK & KHAN 10.03.08
* CONDITION TYPES
      IF NOT i_a350[] IS INITIAL.
        SELECT konm~knumh konm~kbetr konm~kstbm INTO TABLE i_cond
                                       FROM konm
                               INNER JOIN konp ON konp~knumh = konm~knum
                                  FOR ALL ENTRIES IN i_a350
                                  WHERE konp~knumh = i_a350-knumh AND
        konp~kschl = p_kschl.
      ENDIF.
* CONDITION TYPE IS EQUAL TO 'ZMOU'
    ELSE.
* CUSTOMER NO IS NOT INITIAL
      IF NOT s_pkunag IS INITIAL.
        IF NOT i_reb1[] IS INITIAL.
          SELECT * INTO TABLE i_a307 FROM a307 FOR ALL ENTRIES IN i_reb1
                                          WHERE kappl = 'V'           AN
                                                kschl = p_kschl       AN
                                                vkorg IN s_vkorg      AN
                                                vtweg IN s_dist      AND
                                                kunnr = i_reb1-pkunag AN
*                                               DATBI GE SY-DATUM.    "
                                                 datab LE s_sptag-high A
          datbi GE s_sptag-low.   " ASHOK & KHAN 10.03.08
        ENDIF.
* CUSTOMER NO IS INITIAL
      ELSE.
        LOOP AT i_reb1.
          MOVE i_reb1-pkunag TO i_pkunag.
          APPEND i_pkunag.
          CLEAR i_pkunag.
        ENDLOOP.
        SORT i_pkunag BY pkunag.
        DELETE ADJACENT DUPLICATES FROM i_pkunag COMPARING ALL FIELDS.
        SELECT * INTO TABLE i_a307 FROM a307 FOR ALL ENTRIES IN i_pkunag
                                          WHERE kappl = 'V'           AN
                                                kschl = p_kschl       AN
                                                vkorg IN s_vkorg      AN
                                                vtweg IN s_dist       AN
                                                kunnr = i_pkunag-pkunag
*                                               DATBI GE SY-DATUM.     "
                                                 datab LE s_sptag-high A
        datbi GE s_sptag-low.   " ASHOK & KHAN 10.03.08
        IF NOT i_a307[] IS INITIAL.
          SELECT knumh kbetr kstbm INTO TABLE i_cond FROM konm
                                         FOR ALL ENTRIES IN i_a307
          WHERE knumh = i_a307-knumh .
        ENDIF.
      ENDIF.
    ENDIF.
    DATA : w_kvgr3 LIKE s922-kvgr3,
           w_kdgrp LIKE s925-kdgrp,
           w_kukla TYPE kukla.
    DATA : fin_qty  LIKE s925-ummenge.
    DATA : comp_grp  LIKE s925-kvgr2.
    CLEAR : w_kvgr2, w_kvgr3, w_kdgrp, w_vkbur, w_kukla.
*  BREAK-POINT.
    DATA: lv_kunnr TYPE kunnr,
          lv_werks TYPE werks,
          lv_kdgrp TYPE kdgrp.
    i_reb7[] = i_reb1[].
    SORT i_reb7 BY  pkunag vkbur kvgr2 werks.
*  DELETE ADJACENT DUPLICATES FROM I_REB7[] COMPARING PKUNAG VKBUR KVGR2
    DELETE ADJACENT DUPLICATES FROM i_reb7[] COMPARING pkunag.
    SORT i_reb1 BY pkunag vkbur kvgr2 werks.
*  BREAK-POINT.
****SOC BY UJJWAL/PRIYANKA ON CHARM 4000000157 ON 18-09-2019 TO DISPLAY
    LOOP AT i_reb7.
      LOOP AT i_reb1 WHERE pkunag = i_reb7-pkunag. "AND
*                         VKBUR = I_REB7-VKBUR AND
*                         KVGR2 = I_REB7-KVGR2 AND
*                         WERKS = I_REB7-WERKS.
        IF i_reb1 IS NOT INITIAL.
          i_reb2-name1   = i_reb1-name1.
          i_reb2-pkunag = i_reb1-pkunag.
          i_reb2-vkbur = i_reb1-vkbur.
          i_reb2-kvgr2 = i_reb1-kvgr2.
**        I_REB2-KDGRP = I_REB1-KDGRP.
*        I_REB2-WERKS = I_REB1-WERKS.
          i_reb2-kukla = i_reb1-kukla.
          i_reb2-ummenge = i_reb1-ummenge.
          i_reb2-ummenge1 = i_reb1-ummenge.
          COLLECT i_reb2.
        ENDIF.
        CLEAR: i_reb7, i_reb1.
      ENDLOOP.
    ENDLOOP.
    SORT i_reb2 BY kvgr2.
    LOOP AT i_reb2 ASSIGNING FIELD-SYMBOL(<lfs_reb2>).
      <lfs_reb2>-werks = '5010'.
    ENDLOOP.
*  ****EOC BY UJJWAL/PRIYANKA ON CHARM 4000000157 ON 18-09-2019 TO DISPL
*  BREAK-POINT.
    SORT i_reb2 BY kvgr2.
** EOC
*SUMMATION FOR  QUANTITY COMP. GROUPWISE   KAMAL 30-08-2005
    DATA: l_flag TYPE c.
*        FIN_QTY(16) TYPE P.
    DATA : BEGIN OF i_rebk OCCURS 0,
             kvgr2   LIKE i_reb2-kvgr2,
             ummenge LIKE i_reb2-ummenge,
           END OF i_rebk.
    DATA : wa_rebk LIKE LINE OF i_reb2.
    LOOP AT i_reb2 INTO wa_rebk.
      i_rebk-kvgr2 = wa_rebk-kvgr2.
      i_rebk-ummenge = wa_rebk-ummenge.
      APPEND i_rebk.
      CLEAR i_rebk.
    ENDLOOP.
**************BOC BY NITIN DHAMIJA
    IF p_kschl <> 'ZMOU'.
      LOOP AT i_rebk WHERE kvgr2 IS NOT INITIAL.
        AT NEW kvgr2.
          l_flag = 'X'.
          CLEAR fin_qty.
        ENDAT.
        IF l_flag NE 'X'.
          fin_qty = fin_qty + i_rebk-ummenge.
          i_reb2-kvgr2 = i_rebk-kvgr2.
          i_reb2-ummenge = fin_qty.
          MODIFY i_reb2 TRANSPORTING ummenge WHERE kvgr2 = i_reb2-kvgr2.
          CLEAR i_rebk.
        ELSE.
          fin_qty = i_rebk-ummenge.
        ENDIF.
        CLEAR l_flag.
      ENDLOOP.
    ENDIF.
*END OF SUMMATION FOR  QUANTITY COMP. GROUPWISE   KAMAL 30-08-2005
************END OF COMMENT BY NITIN DHAMIJA
* LOGIC FOR SUMMATION OF QTY FOR SINGLE CUSTOMER MULTUIPLE ZONAL OFFICE.
    DATA: l_ummenge  TYPE s925-ummenge,
          o_pkunag   TYPE s925-pkunag,
          n_pkunag   TYPE s925-pkunag,
          l_count(2) TYPE c.
    SORT i_reb2 BY pkunag.
    LOOP AT i_reb2 WHERE kvgr2 IS INITIAL.
      n_pkunag = i_reb2-pkunag.
      IF n_pkunag EQ o_pkunag.
        l_count = l_count + 1.
        l_ummenge = l_ummenge + i_reb2-ummenge.
      ELSEIF l_count GT '1'.
        i_scmz-ummenge = l_ummenge.
        i_scmz-pkunag = o_pkunag.
        l_count = '1'.
        APPEND i_scmz.
        CLEAR i_scmz.
        l_ummenge = i_reb2-ummenge.
      ELSE.
        l_ummenge = i_reb2-ummenge.
        l_count = '1'.
      ENDIF.
      o_pkunag = n_pkunag.
    ENDLOOP.
    IF l_count GT '1'.
      i_scmz-ummenge = l_ummenge.
      i_scmz-pkunag = o_pkunag.
      l_count = '1'.
      APPEND i_scmz.
      CLEAR i_scmz.
    ENDIF.
    LOOP AT i_reb2 WHERE kvgr2 IS INITIAL.
      READ TABLE i_scmz WITH KEY pkunag = i_reb2-pkunag.
      IF sy-subrc = 0.
        i_reb2-ummenge = i_scmz-ummenge.
        MODIFY i_reb2 TRANSPORTING ummenge.
      ENDIF.
    ENDLOOP.
*CALCULATING VALUE W.R.T RATE
    LOOP AT i_reb2.
      REFRESH i_cond1[].
      CLEAR : i_cond1, w_kbetr.
*IF P_KSCHL <> 'ZMOU'.
      IF p_kschl <> 'ZMOU'.
        SORT i_cond BY kstbm DESCENDING.
        LOOP AT i_cond WHERE kstbm <= i_reb2-ummenge.
          w_kbetr = i_cond-kbetr.
          i_reb2-value = i_reb2-ummenge1 * w_kbetr.
          EXIT.
        ENDLOOP.
      ENDIF.
      MODIFY i_reb2.
    ENDLOOP.
* START OF NEW CODE 03.04.2014
    i_reb_bkp[] = i_reb1[].
* END OF NEW CODE 03.04.2014
*  MOVE I_REB1[] TO I_REB_KDGRP[]  .
    LOOP AT i_reb1.
      MOVE-CORRESPONDING i_reb1 TO i_reb_kdgrp.
      APPEND i_reb_kdgrp.
      CLEAR i_reb_kdgrp.
    ENDLOOP.
    DELETE ADJACENT DUPLICATES FROM i_reb1 COMPARING vkbur vkorg pkunag
    kvgr2  kondm kdgrp.
* BOC ASHOK & M A KHAN 07.06.2007
    SELECT vkbur
           bezei INTO TABLE i_tvkbt
           FROM tvkbt
           WHERE spras = 'E'
    AND  vkbur IN s_vkbur.
* EOC ASHOK & M A KHAN 07.06.2007
* READING CHECK BOX AND UPDATING VALUE
*  *    ** SOC BY UJJWAL/PRIYANKA ON 10-02-2020 ON CHRM 4000001548 TO DI
    SELECT kunnr,
      lifnr
  FROM knvp
  INTO TABLE @DATA(lt_knvp)
  FOR ALL ENTRIES IN @i_reb2
  WHERE kunnr = @i_reb2-pkunag
  AND vkorg = '5000'
  AND  vtweg  IN @s_dist
  AND spart = '20'
    AND parvw = 'Z0'.
** EOC BY UJJWAL/PRIYANKA ON 10-02-2020 ON CHRM 4000001548 TO DISPLAY CS
    LOOP AT i_reb2.
      i_reb2-page   = sy-cpage.
      i_reb2-line   = sy-linno .
      w_vkbur = s_vkbur.
      w_vkorg = s_vkorg.
      w_pkunag = i_reb2-pkunag.
      w_kvgr2 = i_reb2-kvgr2.
      w_curr_lin =  sy-tabix.
      w_next_lin = w_curr_lin + 1.
** SOC BY UJJWAL/PRIYANKA ON 10-02-2020 ON CHRM 4000001548 TO DISPLAY CS
      READ TABLE lt_knvp ASSIGNING FIELD-SYMBOL(<lfs_knvp>) WITH  KEY ku
      IF sy-subrc IS  INITIAL.
        i_reb2-kunnr = <lfs_knvp>-lifnr.
      ENDIF.
** EOC BY UJJWAL/PRIYANKA ON 10-02-2020 ON CHRM 4000001548 TO DISPLAY CS
** SOC BY UJJWAL/PRIYANKA ON 7-07-2020 ON CHARM 4000002361 TO REBATE ORD
      CLEAR w_vbeln1.
*Begin of changes by of VIPUL on 20241030  for ATC
      SELECT vbeln INTO w_vbeln1 FROM yrva_rebate
      UP TO 1 ROWS
      WHERE kunnr = i_reb2-pkunag AND
      ( reb_cond = p_kschl or reb_cond = p_kschl1 ) AND
      vkbur = i_reb2-vkbur AND
      yy_per_start = s_sptag-low AND
      yy_per_end = s_sptag-high ORDER BY PRIMARY KEY .
      ENDSELECT.
*End of changes by of VIPUL on 20241030  for ATC
***   SOC BY ABHINAV/ARCHANA GROUP PROBLEM
*      IF I_REB2-PKUNAG EQ '0000035148'.
*        IF SY-SUBRC = 1.
*          I_REB2-VBELN = W_VBELN1.
*          CLEAR W_VBELN1.
*        ENDIF.
*      ELSE.
***        EOC
      IF sy-subrc = 0.
        i_reb2-vbeln = w_vbeln1.
        CLEAR w_vbeln1.
      ENDIF.
** EOC BY UJJWAL/PRIYANKA ON 7-07-2020 ON CHARM 4000002361 TO REBATE ORD
*MAKING POSTIVE
      IF i_reb2-value < 0.
        i_reb2-value = 0 - i_reb2-value.
      ENDIF.
      READ TABLE i_tvkbt WITH KEY vkbur = i_reb2-vkbur.
      IF sy-subrc = 0.
        i_reb2-bezei = i_tvkbt-bezei.
      ENDIF.
      i_reb2-rem = sy-slset.
      MODIFY i_reb2.
      CLEAR i_reb2.
    ENDLOOP.
  ENDIF ."
**SOC BY ABHINAV/ARCNA/VISHAL ON 05/06/2023 CHARM:4000006500.
  IF i_reb2[] IS NOT INITIAL.
    READ TABLE i_reb2 ASSIGNING FIELD-SYMBOL(<w_reb22>) WITH KEY pkunag
    IF sy-subrc EQ 0.
      IF sy-slset EQ 'MAY23PPLLDPE'.
        <w_reb22>-ummenge = '11'.
        <w_reb22>-ummenge1 = '11'.
      ELSEIF sy-slset EQ 'MAY23PPLLFILM'.
        <w_reb22>-ummenge = '0'.
        <w_reb22>-ummenge1 = '0'.
        <w_reb22>-value = '0'.
      ENDIF.
    ENDIF.
    READ TABLE i_reb2 ASSIGNING FIELD-SYMBOL(<w_reb222>) WITH KEY pkunag
    IF sy-subrc EQ 0.
      IF sy-slset EQ 'MAY23PPLLDPE'.
        <w_reb222>-ummenge = '29.5'.
        <w_reb222>-ummenge1 = '29.5'.
      ELSEIF sy-slset EQ 'MAY23PPLLFILM'.
        <w_reb222>-ummenge = '8'.
        <w_reb222>-ummenge1 = '8'.
      ENDIF.
    ENDIF.
  ENDIF.
** SOC BY CHILUKURI TRIPURA REDDY/ARCHNA/VISHAL CHARM NO 4000006851 DATE
  IF sy-slset EQ 'MAY23PPLLDPE' OR  sy-slset EQ 'MAY23PPLLFILM'.
** EOC BY CHILUKURI TRIPURA REDDY/ARCHNA/VISHAL CHARM NO 4000006851 DATE
    DELETE i_reb2 WHERE value EQ '0' AND pkunag = '0000035157'.
**EOC BY ABHINAV/ARCNA/VISHAL ON 05/06/2023 CHARM:4000006500
** SOC BY CHILUKURI TRIPURA REDDY/ARCHNA/VISHAL CHARM NO 4000006851  DAT
  ENDIF.
** EOC BY CHILUKURI TRIPURA REDDY/ARCHNA/VISHAL CHARM NO 4000006851 DATE
*END-OF-SELECTION                                                   *
END-OF-SELECTION.
*   ALV SPECIFIC ROUTINES
*BOC CHARM ID 4000003218 TECHNICAL VIKRAM BAJAJ FUNCTIONAL PRIYANKA SING
  IF pc1 IS INITIAL ."
*BOC CHARM ID 4000003218 TECHNICAL VIKRAM BAJAJ FUNCTIONAL PRIYANKA SING
    PERFORM create_field_catalog.
    PERFORM display_alv.
  ENDIF ."
*&                                                                     *
*&      FORM  SALES_ORDER
*&                                                                     *
*       CALLING BAPI
*                                                                      *
*    >  P1        TEXT
*  <    P2        TEXT
*                                                                      *
FORM sales_order.
*Begin of changes by of VIPUL on 20241030  for ATC
  SELECT vbeln INTO w_vbeln1 FROM yrva_rebate
  UP TO 1 ROWS
  WHERE kunnr = i_reb2-pkunag AND
  reb_cond = p_kschl AND
  vkbur = i_reb2-vkbur AND " ASHOK & KHAN 12 ORDER BY PRIMARY KEY .09 OR
              kukla        = i_reb2-kukla  AND
                             yy_per_start = s_sptag-low   AND
                             yy_per_end   = s_sptag-high AND
                             werks = i_reb2-werks ORDER BY PRIMARY KEY.
  ENDSELECT.
* *End of changes by of VIPUL on 20241030  for ATC
***   SOC BY ABHINAV/ARCHANA GROUP PROBLEM
*  IF I_REB2-PKUNAG EQ '0000035148'.
*    IF SY-SUBRC = 1.
*      I_REB2-VBELN = W_VBELN1.
*      CLEAR W_VBELN1.
*    ENDIF.
*  ELSE.
***        EOC
  IF sy-subrc = 0.
    WRITE : / 'CREDIT MEMO REQUEST ALREADY CREATED'(002),w_vbeln1.
    EXIT.
  ENDIF.
*  ENDIF.
  CALL FUNCTION 'BAPI_SALESDOCU_CREATEFROMDATA'
    EXPORTING
      order_header_in = x_order_header_in
      business_object = w_objtype
      without_commit  = ' '
    IMPORTING
      salesdocument   = w_vbeln
      sold_to_party   = x_sold_to_party
      return          = x_return_commit
    TABLES
      order_items_in  = i_order_items_in
      order_partners  = i_order_partners.
  IF w_vbeln IS INITIAL.
    WRITE : / 'ERROR MESSAGE :',x_return_commit-message.
  ELSE.
    WRITE : / 'SALES ORDER HAS BEEN CREATED WITH :'(003), w_vbeln,
              'AND CUSTOMER NO :'(004), x_sold_to_party-sold_to.
    IF NOT w_vbeln IS INITIAL.
      PERFORM table_update.
    ENDIF.
  ENDIF.
  CLEAR :   w_vbeln, x_return_commit, x_order_header_in, w_objtype,
            x_sold_to_party.
  REFRESH :  i_order_partners, i_order_items_in .
ENDFORM.                    " SALES_ORDER
*&                                                                     *
*&      FORM  DISPLAY_ALV
*&                                                                     *
*       TEXT
*                                                                      *
*    >  P1        TEXT
*  <    P2        TEXT
*                                                                      *
FORM display_alv .
*FILTER AS PER CUSTOMER CLASSIFICATION
  DELETE i_reb2 WHERE NOT kukla IN s_kukla.
******ADDED BY NITIN DHAMIJA ON 25.06.2019*****************
  IF i_reb2[] IS NOT INITIAL.
    SORT i_reb2 BY kvgr2.
    LOOP AT i_reb2.
      i_reb5-pkunag = i_reb2-pkunag.
      AT NEW pkunag.
        SUM.
        i_reb5-ummenge1 = i_reb5-ummenge1 + i_reb2-ummenge.
        APPEND i_reb5.
        CLEAR: i_reb5.
      ENDAT.
    ENDLOOP.
  ENDIF.
*
  i_reb6[] = i_reb5[].
  REFRESH: i_reb5[].
  LOOP AT i_reb6.
    i_reb5-pkunag = i_reb6-pkunag.
    AT NEW pkunag.
      SUM.
      i_reb5-ummenge1 = i_reb5-ummenge1 + i_reb6-ummenge1.
      APPEND i_reb5.
      CLEAR: i_reb5.
    ENDAT.
  ENDLOOP.
  IF p_kschl1 IS NOT INITIAL.
    SELECT *  FROM a350 INTO TABLE @DATA(i_a350_1)
    WHERE kappl = 'V'
    AND
   kschl = @p_kschl1  AND
   vkorg IN @s_vkorg AND
   kfrst = ''       AND
*dATBI GE SY-DATUM.    " ASHOK & KHAN 10.03.08
   datab LE @s_sptag-high AND " ASHOK & KHAN 10.03.08
        datbi GE @s_sptag-low.   " ASHOK & KHAN 10.03.08
* CONDITION TYPES
    IF NOT i_a350_1[] IS INITIAL.
      SELECT konm~knumh , konm~kbetr , konm~kstbm
      INTO TABLE @DATA(i_cond_1)
      FROM konm
      INNER JOIN konp ON konp~knumh = konm~knumh
      FOR ALL ENTRIES IN @i_a350_1
      WHERE konp~knumh = @i_a350_1-knumh AND
      konp~kschl = @p_kschl1.
    ENDIF.
    SORT i_cond_1 BY kstbm DESCENDING.
  ENDIF.
  if i_reb_2 is NOT INITIAL.
    DATA : BEGIN OF i_rebk_1 OCCURS 0,
             kvgr2   LIKE i_reb2-kvgr2,
             ummenge LIKE i_reb2-ummenge,
           END OF i_rebk_1.
    DATA: l_flag_1 TYPE c.
    DATA : fin_qty_1  LIKE s925-ummenge.
    DATA : wa_rebk_1 LIKE LINE OF i_reb2.
    LOOP AT i_reb_2 INTO wa_rebk_1.
      i_rebk_1-kvgr2 = wa_rebk_1-kvgr2.
      i_rebk_1-ummenge = wa_rebk_1-ummenge.
      APPEND i_rebk_1.
      CLEAR i_rebk_1.
    ENDLOOP.
    IF p_kschl <> 'ZMOU'.
      LOOP AT i_rebk_1 WHERE kvgr2 IS NOT INITIAL.
        AT NEW kvgr2.
          l_flag_1 = 'X'.
          CLEAR fin_qty_1.
        ENDAT.
        IF l_flag_1 NE 'X'.
          fin_qty_1 = fin_qty_1 + i_rebk_1-ummenge.
          i_reb_2-kvgr2 = i_rebk_1-kvgr2.
          i_reb_2-ummenge = fin_qty_1.
          MODIFY i_reb_2 TRANSPORTING ummenge WHERE kvgr2 = i_reb_2-kvgr
          CLEAR i_rebk_1.
        ELSE.
          fin_qty_1 = i_rebk_1-ummenge.
        ENDIF.
        CLEAR l_flag_1.
      ENDLOOP.
    ENDIF.
    endif.
  IF i_cond_1 IS NOT INITIAL.
    LOOP AT i_reb2.
      lv_index = sy-tabix.
      LOOP AT i_cond WHERE kstbm <= i_reb2-ummenge.
        IF i_cond IS NOT INITIAL.
          i_reb2-value = i_cond-kbetr * i_reb2-ummenge1.
          IF i_reb2-value < 0.
            i_reb2-value = i_reb2-value * -1.
          ENDIF.
          i_reb2-kbetr = i_cond-kbetr.
          IF i_reb2-kbetr < 0.
            i_reb2-kbetr = i_reb2-kbetr * -1.
          ENDIF.
          MODIFY i_reb2 INDEX lv_index TRANSPORTING kbetr value.
          EXIT.
        ENDIF.
      ENDLOOP.
      CLEAR: i_reb2, i_cond, lv_index , w_vbeln1.
    ENDLOOP.
    SORT i_reb_2 BY kvgr2.
    LOOP AT i_reb2.
      lv_index = sy-tabix.
    LOOP AT i_reb_2 INTO DATA(wa_reb_2) WHERE pkunag = i_reb2-pkunag.
      if wa_reb_2-ummenge <= i_reb2-ummenge.
        LOOP AT i_cond_1 INTO DATA(wa_cond_1) WHERE kstbm <= wa_reb_2-um
            IF wa_cond_1-kbetr < 0.
              wa_cond_1-kbetr = wa_cond_1-kbetr * -1.
            ENDIF.
            IF i_reb2-kbetr <= wa_cond_1-kbetr.   ""700<800
              SELECT SINGLE * FROM a350 INTO @wa_rec WHERE knumh = @wa_c
              i_reb2-value = wa_cond_1-kbetr * i_reb2-ummenge1.
              i_reb2-kbetr = wa_cond_1-kbetr.
            ENDIF.
            IF i_reb2-value < 0.
              i_reb2-value = i_reb2-value * -1.
            ENDIF.
        MODIFY i_reb2 INDEX lv_index TRANSPORTING kbetr value.
          EXIT.
      ENDLOOP.
        ENDIF.
      EXIT.
    ENDLOOP.
    ENDLOOP.
  ENDIF.
  IF p_kschl1 IS INITIAL.
    LOOP AT i_reb2.
      lv_index = sy-tabix.
      LOOP AT i_cond WHERE kstbm <= i_reb2-ummenge.
        IF i_cond IS NOT INITIAL.
          i_reb2-value = i_cond-kbetr * i_reb2-ummenge1.
          IF i_reb2-value < 0.
            i_reb2-value = i_reb2-value * -1.
          ENDIF.
          i_reb2-kbetr = i_cond-kbetr.
          IF i_reb2-kbetr < 0.
            i_reb2-kbetr = i_reb2-kbetr * -1.
          ENDIF.
          MODIFY i_reb2 INDEX lv_index TRANSPORTING kbetr value.
          EXIT.
        ENDIF.
      ENDLOOP.
      CLEAR: i_reb2, i_cond, lv_index , w_vbeln1.
    ENDLOOP.
  ENDIF.
******END OF ADDITION BY NITIN DHAMIJA**********************
**** BOC CHARM ID 2000000213 TECHNICAL VIKRAM BAJAJ FUNCTIONAL PRIYANKA
  IF sy-tcode = 'YRVU015' OR sy-tcode = 'YRVU016'.
**** EOC CHARM ID 2000000213 TECHNICAL VIKRAM BAJAJ FUNCTIONAL PRIYANKA
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program       = 'YRVU001_REB_CALC_M2'
        i_callback_pf_status_set = 'PF_STATUS_SET'
        is_layout                = i_layout
        it_fieldcat              = gt_fieldcat[]
        it_events                = i_events[]
        it_event_exit            = i_exit_event[]
*       I_SAVE                   = 'X'
      TABLES
        t_outtab                 = i_reb2[]
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
**** BOC CHARM ID 2000000213 TECHNICAL VIKRAM BAJAJ FUNCTIONAL PRIYANKA
  ELSE .
*    SUBMIT
    EXPORT i_reb2[] TO MEMORY ID 'M001' .
  ENDIF .
**** EOC CHARM ID 2000000213 TECHNICAL VIKRAM BAJAJ FUNCTIONAL PRIYANKA
ENDFORM.                    " DISPLAY_ALV
*&                                                                    *
*&      FORM  ON_SELECTION
*&                                                                    *
*       TEXT
*                                                                     *
*        >R_UCOMM    TEXT
*        >RS_SELFIELDTEXT
*                                                                     *
FORM on_selection USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  DATA: itab TYPE TABLE OF sy-ucomm,
        w_ln TYPE i.
  APPEND 'CALC' TO itab.
  APPEND 'FCOD' TO itab.
  APPEND '&ALL' TO itab.
  APPEND '&SAL' TO itab.
  CASE r_ucomm.
    WHEN 'CALC'.
      SET PF-STATUS 'STANDARD'   ##STAT_UNDEF.
*CALCULATING NEW VALUE AS PER QTY
*CALCULATING VALUE W.R.T RATE
*SUMMATION FOR  QUANTITY COMP. GROUPWISE   KAMAL 30-08-2005
      DATA: l_flag TYPE c.
*        FIN_QTY(16) TYPE P.
      DATA : BEGIN OF i_rebk OCCURS 0,
               kvgr2    LIKE i_reb2-kvgr2,
               ummenge1 LIKE i_reb2-ummenge1,
             END OF i_rebk.
      DATA : wa_rebk LIKE LINE OF i_reb2.
      LOOP AT i_reb2 INTO wa_rebk.
        i_rebk-kvgr2 = wa_rebk-kvgr2.
        i_rebk-ummenge1 = wa_rebk-ummenge1.
        APPEND i_rebk.
        CLEAR i_rebk.
      ENDLOOP.
*CALCULATING VALUE W.R.T RATE
      LOOP AT i_reb2.
        REFRESH i_cond1[].
        CLEAR : i_cond1, w_kbetr.
        IF p_kschl <> 'ZMOU'.
          SORT i_cond BY kstbm DESCENDING.
          DESCRIBE TABLE i_cond LINES w_ln.
          READ TABLE i_cond INDEX w_ln.
          IF i_cond-kstbm > i_reb2-ummenge1.
            CLEAR w_kbetr.
            i_reb2-value = i_reb2-ummenge1 * w_kbetr.
          ENDIF.
          IF i_reb2-kvgr2 IS INITIAL.
            LOOP AT i_cond WHERE kstbm <= i_reb2-ummenge1.
              w_kbetr = i_cond-kbetr.
              i_reb2-value = i_reb2-ummenge1 * w_kbetr.
              EXIT.
            ENDLOOP.
          ELSEIF NOT i_reb2-kvgr2 IS INITIAL.
            LOOP AT i_cond WHERE kstbm <= i_reb2-ummenge.
              w_kbetr = i_cond-kbetr.
              i_reb2-value = i_reb2-ummenge1 * w_kbetr.
              EXIT.
            ENDLOOP.
          ENDIF.
        ENDIF.
        MODIFY i_reb2.
      ENDLOOP.
*  I_REB_KDGRP[] = I_REB1[].
      LOOP AT i_reb1.
        MOVE-CORRESPONDING i_reb1 TO i_reb_kdgrp.
        APPEND i_reb_kdgrp.
        CLEAR i_reb_kdgrp.
      ENDLOOP.
      DELETE ADJACENT DUPLICATES FROM i_reb1 COMPARING vkbur vkorg pkuna
      kvgr2  kondm kdgrp.
*UPDATING VALUE
      LOOP AT i_reb2.
*MAKING POSTIVE
        IF i_reb2-value < 0.
          i_reb2-value = 0 - i_reb2-value.
        ENDIF.
        MODIFY i_reb2.
        CLEAR i_reb2.
      ENDLOOP.
*EXECUTE BUTTON
    WHEN 'FCOD'.
      SET PF-STATUS 'STANDARD' OF PROGRAM 'SAPLSALV'
                              EXCLUDING itab.
      PERFORM top_of_page.
*CALL BAPI
      PERFORM check.
      DELETE i_reb2 WHERE check <> 'X'.
*EXECUTE SELECT ALL
    WHEN 'SALL'.
      SET PF-STATUS 'STANDARD' OF PROGRAM 'YRVU001_REB_CALC_M2' EXCLUDIN
      LOOP AT i_reb2.
        i_reb2-check = 'X'.
        MODIFY i_reb2.
      ENDLOOP.
*EXECUTE DESELECT ALL
    WHEN 'DSAL'.
      SET PF-STATUS 'STANDARD' OF PROGRAM 'YRVU001_REB_CALC_M2' EXCLUDIN
      LOOP AT i_reb2 WHERE check = 'X'.
        i_reb2-check = ''.
        MODIFY i_reb2.
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "ON_SELECTION
*&                                                                     *
*&      FORM  CHECK
*&                                                                     *
*       TEXT
*                                                                      *
*    >  P1        TEXT
*  <    P2        TEXT
*                                                                      *
FORM check .
*BREAK-POINT.
  LOOP AT i_reb2 WHERE check = 'X'.
    CLEAR : w_auart,
            w_matnr,
            w_objtype,
            x_order_header_in,
            i_order_partners,
            i_order_items_in.
    REFRESH : i_order_partners,
              i_order_items_in.
    SELECT SINGLE * FROM YRVA_REBATE INTO @DATA(WA_REBATE) WHERE KUNNR =
      AND YY_PER_START eq @s_sptag-low
      AND YY_PER_END eq @s_sptag-high
      AND VKBUR  EQ  @i_reb2-vkbur    """ ADD BY ABHRIAJ SINGH ON MON 13
      AND ( REB_COND = @p_kschl or REB_COND = @p_kschl1 ).
        IF WA_REBATE IS NOT INITIAL.
          MESSAGE 'Rebate order already created for this customer' TYPE
        ELSE.
*    IF S_DIV-LOW = '20'.
*   IF S_KONDM-LOW <> '70'.
*    BREAK-POINT.
    w_auart = 'ZP09'. "'ZPCR'
    w_matnr = 'REBATE(POLYMER)'.
*   ELSE.
*    W_AUART = 'ZI09'.
*    W_MATNR = 'REBATE(POLYMER)'.
*   ENDIF.
    IF i_reb2-check = 'X'.
* NEW CODE ADDED ON 03.04.2014 FOR THE MATERIAL PRICING GROUP 70
      DATA: lv_quan  LIKE s925-ummenge,
*            LV_VALUE LIKE S925-UMMENGE,
            lv_value LIKE   yrva_rebate-value,  "(QUANTITY * RATE)
            lv_netpr TYPE s925-ummenge.
      CLEAR: lv_quan, flag_70.
      LOOP AT i_reb_bkp WHERE pkunag = i_reb2-pkunag
                  AND vkbur = i_reb2-vkbur
                  AND kvgr2 = i_reb2-kvgr2
*                  AND KVGR3 = I_REB2-KVGR3
                  AND kdgrp = i_reb2-kdgrp
                  AND kukla = i_reb2-kukla
                  AND ( kondm = '70' OR kondm = '71' ).
        lv_quan = lv_quan + i_reb_bkp-ummenge.
      ENDLOOP.
      IF lv_quan IS NOT INITIAL.
        lv_netpr = i_reb2-value / i_reb2-ummenge1.
        i_reb2-ummenge1 = i_reb2-ummenge1 - lv_quan.
        i_reb2-value = i_reb2-ummenge1 * lv_netpr.
        lv_value = lv_quan * lv_netpr.
        flag_70 = 'X'.
      ENDIF.
* END OF NEW CODE ADDED ON 03.04.2014
* HEADER DETAILS
      x_order_header_in-doc_type   = w_auart.
      x_order_header_in-sales_org  = s_vkorg-low.
      IF i_reb2-werks = '5010'.
        x_order_header_in-distr_chan = '10'.
      ELSE.
        x_order_header_in-distr_chan = '20'.
      ENDIF.
*      IF S_KONDM-LOW <> '70'.
      x_order_header_in-division   = '20'.
*      ELSE.
*        X_ORDER_HEADER_IN-DIVISION   = '70'.
*      ENDIF.
      x_order_header_in-ref_1 = lv_kunnr.
      x_order_header_in-ord_reason = 'G21'.
      x_order_header_in-sales_off  = i_reb2-vkbur.
*ZMQD
      IF ( p_kschl = 'ZMQD' ) OR ( p_kschl = 'ZLQD' )
      OR ( p_kschl = 'ZHHD' ) OR ( p_kschl = 'ZTQD' )
      OR ( p_kschl = 'ZTLD' )
      OR ( p_kschl = 'ZDCD' )  . " ASHOK AND KHAN 01.07.08
        x_order_header_in-cd_type1   = 'ZCQT'.
        x_order_header_in-cd_value1    =  i_reb2-value / 10 .
      ENDIF.
*ZAQD
      IF p_kschl = 'ZAQD' OR
         p_kschl = 'ZAD1' OR " ASHOK AND MA KHAN 30.09.08
         p_kschl = 'ZAD2' OR " ASHOK AND MA KHAN 30.09.08
         p_kschl = 'ZAD3' OR " ASHOK AND MA KHAN 30.09.08
         p_kschl = 'ZAD4' OR " ASHOK AND MA KHAN 30.09.08
         p_kschl = 'ZAD5' .  " ASHOK AND MA KHAN 07.11.08
        x_order_header_in-cd_type1   = 'ZAQT'.
        x_order_header_in-cd_value1    =  i_reb2-value / 10 .
      ENDIF.
*ZSQD
      IF ( p_kschl = 'ZSQD' ) OR ( p_kschl = 'ZBQD' ) OR ( p_kschl =
'ZPQD' ) OR ( p_kschl = 'ZIQD' ) OR ( p_kschl = 'ZEQD' ) OR ( p_kschl =
'ZQTR' )
OR ( p_kschl = 'ZRQD' )  "ADDED BY ASHOK & KHAN 10.03/06
OR ( p_kschl = 'ZMQR' )  "ADDED BY ASHOK & KHAN 29.08/06
OR  ( p_kschl = 'ZPP9' ) " CHANGED BY SACHIN AND ATUL SIR ON  16.07.2014
OR ( p_kschl = 'ZCQD' )  "ADDED BY ASHOK & KHAN 02.02.07
OR ( p_kschl = 'ZQTY' ). "ADDED BY ASHOK & KHAN 10.03.2008
        x_order_header_in-cd_type1   = 'ZSQT'.
        x_order_header_in-cd_value1  =  i_reb2-value / 10 .
      ENDIF.
      IF ( p_kschl = 'ZPRT' OR p_kschl = 'ZPPT' OR  p_kschl = 'ZMQR' OR
           p_kschl = 'ZPP1' OR " ASHOK & KHAN 30.08.2008
           p_kschl = 'ZPP2' OR " ASHOK & KHAN 26.09.2008
           p_kschl = 'ZPP3' OR " ASHOK & KHAN 07.11.2008
           p_kschl = 'ZPP4' OR " ASHOK & KHAN 07.11.2008
           p_kschl = 'ZPP5' OR " ASHOK & KHAN 07.11.2008
           p_kschl = 'ZPP6' OR " ASHOK & KHAN 07.11.2008
           p_kschl = 'ZPP7' OR " ASHOK & KHAN 07.11.2008
           p_kschl = 'ZPP8' OR " ASHOK & KHAN 07.11.2008
*           P_KSCHL = 'ZPP9' OR " ASHOK & KHAN 07.11.2008
*SOC RITESH SINGH PRIYANKA SINGH ON CHARM 4000004008 ON DATE 08.07.2021
            p_kschl = 'PE12' OR
            p_kschl = 'PE13' OR
            p_kschl = 'PE14' OR
            p_kschl = 'PE15' OR
            p_kschl = 'PE16' OR
            p_kschl = 'PE17' OR
            p_kschl = 'PE18' OR
            p_kschl = 'PE19' OR
            p_kschl = 'PE20' OR
            p_kschl = 'PE21' OR
            p_kschl = 'PE22' OR
            p_kschl = 'PE23' OR
            p_kschl = 'PE24' OR
            p_kschl = 'PE25' OR
            p_kschl = 'PE26' OR
            p_kschl = 'PE27' OR
            p_kschl = 'PE28' OR
            p_kschl = 'PE29' OR
            p_kschl = 'PE30' OR
*SOC RITESH SINGH PRIYANKA SINGH ON CHARM 4000004008 ON DATE 08.07.2021
           p_kschl = 'ZP10' OR " ASHOK & KHAN 07.11.2008
           p_kschl = 'ZP11' OR " ASHOK & KHAN 07.11.2008
           p_kschl = 'ZP12' OR
***************ADDED BY SACHIN KAUL ORO PRIYANKA SINGH ON 13/1/17*******
            p_kschl = 'ZP13' OR
            p_kschl = 'ZP14' OR
            p_kschl = 'ZP15' OR
            p_kschl = 'ZP16' OR
            p_kschl = 'ZP17' OR
            p_kschl = 'ZP18' OR
            p_kschl = 'ZP19' OR
            p_kschl = 'ZP20' OR
            p_kschl = 'ZP21' OR
            p_kschl = 'ZP22' OR
            p_kschl = 'ZP23' OR
*SOC BY KUNAL/PRIYANKA ON 13/12/2018 FOR RMS:-1337
            p_kschl = 'ZP24' OR
            p_kschl = 'ZP25' OR
            p_kschl = 'ZP26' OR
            p_kschl = 'ZP27' OR
            p_kschl = 'ZP28' OR
            p_kschl = 'ZP29' OR
            p_kschl = 'ZP30' OR
            p_kschl = 'ZP31' OR
            p_kschl = 'ZP32' OR
            p_kschl = 'ZP33' OR
            p_kschl = 'ZP34' OR
            p_kschl = 'ZP35' ).
*EOC BY KUNAL/PRIYANKA ON 13/12/2018 FOR RMS:-1337
        x_order_header_in-cd_type1   = 'ZAQT'.
        x_order_header_in-cd_value1    =  i_reb2-value / 10 .
      ENDIF.
*REMARKS
      x_order_header_in-purch_no   = i_reb2-rem.
*      BREAK-POINT.
* PARTENER DETAILS
      IF i_reb2-werks = '5010'.
        i_order_partners-partn_role  = 'AG'.
        i_order_partners-partn_numb  = i_reb2-pkunag.
        APPEND i_order_partners.
      ELSE.                       " FOR PLANTS OTHER THAN 5010
        i_order_partners-partn_role  = 'AG'.
*        I_ORDER_PARTNERS-PARTN_NUMB = I_REB2-WERKS.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = i_reb2-werks
          IMPORTING
            output = lv_kunnr.
        i_order_partners-partn_numb  = lv_kunnr.
        APPEND i_order_partners.
      ENDIF.
* ITEM DETAILS
      i_order_items_in-material   = w_matnr.
      i_order_items_in-target_qty = i_reb2-ummenge1 * 1000.
      APPEND i_order_items_in.
      w_objtype = 'BUS2094'.
      IF x_order_header_in-cd_value1 IS NOT INITIAL AND
         i_order_items_in-target_qty IS NOT INITIAL.
        PERFORM sales_order.
      ELSE.
        CLEAR : w_auart,
        w_matnr,
        w_objtype,
        x_order_header_in,
        i_order_partners,
        i_order_items_in.
        REFRESH : i_order_partners,
                  i_order_items_in.
      ENDIF.
******************START OF NEW CODE ADDED ON 03.04.2014*****************
* NEW CMR CREATION IN CASE OF MATERIAL PRICING GROUP = '70'
      IF flag_70 = 'X' AND w_vbeln1 IS INITIAL.
        CLEAR : w_auart,
                w_matnr,
                w_objtype,
                x_order_header_in,
                i_order_partners,
                i_order_items_in.
        REFRESH : i_order_partners,
                  i_order_items_in.
        w_auart = 'ZI09'.
        w_matnr = 'REBATE(POLYMER)-IM'.
* HEADER DETAILS
        x_order_header_in-doc_type   = w_auart.
        x_order_header_in-sales_org  = s_vkorg-low.
        IF i_reb2-werks = '5010'.
          x_order_header_in-distr_chan = '10'.
        ELSE.
          x_order_header_in-distr_chan = '20'.
        ENDIF.
        x_order_header_in-division   = '70'.
        x_order_header_in-ord_reason = 'G21'.
        x_order_header_in-sales_off  = i_reb2-vkbur.
*ZMQD
        IF ( p_kschl = 'ZMQD' ) OR ( p_kschl = 'ZLQD' )
        OR ( p_kschl = 'ZHHD' ) OR ( p_kschl = 'ZTQD' )
        OR ( p_kschl = 'ZTLD' )
        OR ( p_kschl = 'ZDCD' )  . " ASHOK AND KHAN 01.07.08
          x_order_header_in-cd_type1   = 'ZCQT'.
          x_order_header_in-cd_value1    =  lv_value / 10 .
        ENDIF.
*ZAQD
        IF p_kschl = 'ZAQD' OR
           p_kschl = 'ZAD1' OR " ASHOK AND MA KHAN 30.09.08
           p_kschl = 'ZAD2' OR " ASHOK AND MA KHAN 30.09.08
           p_kschl = 'ZAD3' OR " ASHOK AND MA KHAN 30.09.08
           p_kschl = 'ZAD4' OR " ASHOK AND MA KHAN 30.09.08
           p_kschl = 'ZAD5' .  " ASHOK AND MA KHAN 07.11.08
          x_order_header_in-cd_type1   = 'ZAQT'.
          x_order_header_in-cd_value1    =  lv_value / 10 .
        ENDIF.
*ZSQD
        IF ( p_kschl = 'ZSQD' ) OR ( p_kschl = 'ZBQD' ) OR ( p_kschl =
  'ZPQD' ) OR ( p_kschl = 'ZIQD' ) OR ( p_kschl = 'ZEQD' ) OR ( p_kschl
  'ZQTR' )
  OR ( p_kschl = 'ZRQD' )  "ADDED BY ASHOK & KHAN 10.03/06
  OR ( p_kschl = 'ZMQR' )  "ADDED BY ASHOK & KHAN 29.08/06
  OR ( p_kschl = 'ZCQD' )  "ADDED BY ASHOK & KHAN 02.02.07
  OR  ( p_kschl = 'ZPP9' ) " CHANGED BY SACHIN AND ATUL SIR ON  16.07.20
  OR ( p_kschl = 'ZQTY' ). "ADDED BY ASHOK & KHAN 10.03.2008
          x_order_header_in-cd_type1   = 'ZSQT'.
          x_order_header_in-cd_value1  =  lv_value / 10 .
        ENDIF.
        IF ( p_kschl = 'ZPRT' OR p_kschl = 'ZPPT' OR  p_kschl = 'ZMQR' O
             p_kschl = 'ZPP1' OR " ASHOK & KHAN 30.08.2008
             p_kschl = 'ZPP2' OR " ASHOK & KHAN 26.09.2008
             p_kschl = 'ZPP3' OR " ASHOK & KHAN 07.11.2008
             p_kschl = 'ZPP4' OR " ASHOK & KHAN 07.11.2008
             p_kschl = 'ZPP5' OR " ASHOK & KHAN 07.11.2008
             p_kschl = 'ZPP6' OR " ASHOK & KHAN 07.11.2008
             p_kschl = 'ZPP7' OR " ASHOK & KHAN 07.11.2008
             p_kschl = 'ZPP8' OR " ASHOK & KHAN 07.11.2008
*             P_KSCHL = 'ZPP9' OR " ASHOK & KHAN 07.11.2008
             p_kschl = 'ZP10' OR " ASHOK & KHAN 07.11.2008
             p_kschl = 'ZP11' OR " ASHOK & KHAN 07.11.2008
             p_kschl = 'ZP12' OR  " ASHOK & KHAN 20.11.2008
*SOC BY KUNAL/PRIYANKA ON 13/12/2018 FOR RMS:-1337
            p_kschl = 'ZP13' OR
            p_kschl = 'ZP14' OR
            p_kschl = 'ZP15' OR
            p_kschl = 'ZP16' OR
            p_kschl = 'ZP17' OR
            p_kschl = 'ZP18' OR
            p_kschl = 'ZP19' OR
            p_kschl = 'ZP20' OR
            p_kschl = 'ZP21' OR
            p_kschl = 'ZP22' OR
            p_kschl = 'ZP23' OR
            p_kschl = 'ZP24' OR
            p_kschl = 'ZP25' OR
            p_kschl = 'ZP26' OR
            p_kschl = 'ZP27' OR
            p_kschl = 'ZP28' OR
            p_kschl = 'ZP29' OR
            p_kschl = 'ZP30' OR
            p_kschl = 'ZP31' OR
            p_kschl = 'ZP32' OR
            p_kschl = 'ZP33' OR
            p_kschl = 'ZP34' OR
            p_kschl = 'ZP35' ).
*EOC BY KUNAL/PRIYANKA ON 13/12/2018 FOR RMS:-1337
          x_order_header_in-cd_type1   = 'ZAQT'.
          x_order_header_in-cd_value1    =  lv_value / 10 .
        ENDIF.
*REMARKS
        x_order_header_in-purch_no   = i_reb2-rem.
        x_order_header_in-ref_1 = lv_kunnr.
* PARTENER DETAILS
        IF i_reb2-werks = '5010'.
          i_order_partners-partn_role  = 'AG'.
          i_order_partners-partn_numb  = i_reb2-pkunag.
          APPEND i_order_partners.
        ELSE.
          i_order_partners-partn_role  = 'AG'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = i_reb2-werks
            IMPORTING
              output = lv_kunnr.
          i_order_partners-partn_numb  = lv_kunnr.
          APPEND i_order_partners.
        ENDIF.
* ITEM DETAILS
        i_order_items_in-material   = w_matnr.
        i_order_items_in-target_qty = lv_quan * 1000.
        APPEND i_order_items_in.
*
        w_objtype = 'BUS2094'.
        DATA : w_vbeln1 TYPE vbeln.
        IF x_order_header_in-cd_value1 IS NOT INITIAL AND
           i_order_items_in-target_qty IS NOT INITIAL.
          CALL FUNCTION 'BAPI_SALESDOCU_CREATEFROMDATA'
            EXPORTING
              order_header_in = x_order_header_in
              business_object = w_objtype
              without_commit  = ' '
            IMPORTING
              salesdocument   = w_vbeln
              sold_to_party   = x_sold_to_party
              return          = x_return_commit
            TABLES
              order_items_in  = i_order_items_in
              order_partners  = i_order_partners.
          IF w_vbeln IS INITIAL.
            WRITE : / 'ERROR MESSAGE :',x_return_commit-message.
          ELSE.
            WRITE : / 'SALES ORDER HAS BEEN CREATED WITH :'(003), w_vbel
                      'AND CUSTOMER NO :'(004), x_sold_to_party-sold_to.
            IF NOT w_vbeln IS INITIAL.
              CLEAR yrva_rebate.
              yrva_rebate-vbeln        = w_vbeln.
              yrva_rebate-kunnr        = i_reb2-pkunag. "X_SOLD_TO_PARTY
              yrva_rebate-vkbur        = i_reb2-vkbur.
              yrva_rebate-lft_qty      = i_reb2-ummenge.
              yrva_rebate-werks        = i_reb2-werks.
              yrva_rebate-rev_qty      = lv_quan.
              if wa_rec-kschl is NOT INITIAL.
              yrva_rebate-reb_cond     = wa_rec-kschl.
              ELSE.
                yrva_rebate-reb_cond     = p_kschl.
              ENdif.
              yrva_rebate-ord_cond     = x_order_header_in-cd_type1.
              yrva_rebate-value        = lv_value.
              yrva_rebate-yy_per_start = s_sptag-low.
              yrva_rebate-yy_per_end   = s_sptag-high.
              yrva_rebate-kukla        = i_reb2-kukla.
              INSERT yrva_rebate FROM yrva_rebate.
            ENDIF.
          ENDIF.
          CLEAR :   w_vbeln, x_return_commit, x_order_header_in, w_objty
                    x_sold_to_party.
          REFRESH :  i_order_partners, i_order_items_in .
          CLEAR: flag_70.
        ENDIF.
      ENDIF.
******************END OF NEW CODE ADDED ON 03.04.2014*******************
    ENDIF.
    ENDIF.
    CLEAR wa_rebate.
  ENDLOOP.
ENDFORM.                    " CHECK
*&                                                                    *
*&      FORM  CREATE_FIELD_CATALOG
*&                                                                    *
*       TEXT
*                                                                     *
FORM create_field_catalog .
  IF sy-lsind <> 1.
    gt_fieldcat-fieldname = 'CHECK'.
    gt_fieldcat-outputlen    =  4.
    gt_fieldcat-just         =  'C' .
    gt_fieldcat-col_pos      =  1 .
    gt_fieldcat-seltext_s    = 'FLAG'.
    gt_fieldcat-seltext_m    = 'FLAG'.
    gt_fieldcat-ddictxt      = 'M'.
    gt_fieldcat-checkbox     = 'X'.
    gt_fieldcat-input        = 'X'.
    gt_fieldcat-edit         = 'X'.
    APPEND gt_fieldcat.
    CLEAR gt_fieldcat.
    gt_fieldcat-fieldname = 'PKUNAG'.
    gt_fieldcat-outputlen    =  10.
    gt_fieldcat-just         =  'C' .
    gt_fieldcat-col_pos      =  2.
    gt_fieldcat-seltext_s    = 'CUSTOMER'.
    gt_fieldcat-seltext_m    = 'CUSTOMER'.
    gt_fieldcat-ddictxt      = 'M'.
    gt_fieldcat-do_sum       = space.
    APPEND gt_fieldcat.
    CLEAR gt_fieldcat.
    gt_fieldcat-fieldname = 'NAME1'.
    gt_fieldcat-outputlen    =  35.
    gt_fieldcat-just         =  'L' .
    gt_fieldcat-col_pos      =  3.
    gt_fieldcat-seltext_s    = 'CUSTOMER NAME'.
    gt_fieldcat-seltext_m    = 'CUSTOMER NAME'.
    gt_fieldcat-ddictxt      = 'M'.
    gt_fieldcat-do_sum       = space.
    APPEND gt_fieldcat.
    CLEAR gt_fieldcat.
    gt_fieldcat-fieldname = 'KVGR2'.
    gt_fieldcat-outputlen    =  6.
    gt_fieldcat-just         =  'C' .
    gt_fieldcat-col_pos      =  4.
    gt_fieldcat-seltext_s    = 'GRP CO.'.
    gt_fieldcat-seltext_m    = 'GRP CO.'.
    gt_fieldcat-ddictxt      = 'M'.
    gt_fieldcat-do_sum       = space.
    APPEND gt_fieldcat.
    CLEAR gt_fieldcat.
** SOC BY UJJWAL/PRIYANKA ON CHARM#400000157 ON 18-09-2019 TO  COMMENT T
**    GT_FIELDCAT-FIELDNAME = 'WERKS'.
**    GT_FIELDCAT-OUTPUTLEN    =  35.
**    GT_FIELDCAT-JUST         =  'L' .
**    GT_FIELDCAT-COL_POS      =  3.
**    GT_FIELDCAT-SELTEXT_S    = 'PLANT'.
**    GT_FIELDCAT-SELTEXT_M    = 'PLANT'.
**    GT_FIELDCAT-DDICTXT      = 'M'.
**    GT_FIELDCAT-DO_SUM       = SPACE.
**    APPEND GT_FIELDCAT.
**    CLEAR GT_FIELDCAT.
**    GT_FIELDCAT-FIELDNAME = 'KDGRP'.
**    GT_FIELDCAT-OUTPUTLEN    =  3.
**    GT_FIELDCAT-JUST         =  'C' .
**    GT_FIELDCAT-COL_POS      =  5 .
**    GT_FIELDCAT-SELTEXT_S    = 'APP'.
**    GT_FIELDCAT-SELTEXT_M    = 'APP'.
**    GT_FIELDCAT-DDICTXT      = 'M'.
**    GT_FIELDCAT-DO_SUM       = SPACE.
**    APPEND GT_FIELDCAT.
**    CLEAR GT_FIELDCAT.
** EOC BY UJJWAL/PRIYANKA ON CHARM#400000157 ON 18-09-2019 TO COMMENT TH
    gt_fieldcat-fieldname = 'KUNNR'.
    gt_fieldcat-outputlen    =  10.
    gt_fieldcat-just         =  'C' .
    gt_fieldcat-col_pos      =  5 .
    gt_fieldcat-seltext_s    = 'CS'.
    gt_fieldcat-seltext_m    = 'CONSIGNMENT STOCKIST'.
    gt_fieldcat-ddictxt      = 'M'.
    gt_fieldcat-do_sum       = space.
    APPEND gt_fieldcat.
    CLEAR gt_fieldcat.
    gt_fieldcat-fieldname = 'UMMENGE'.
    gt_fieldcat-outputlen    =  16.
    gt_fieldcat-just         =  'R' .
    gt_fieldcat-col_pos      =   6.
    gt_fieldcat-seltext_s    = 'LIFTED QTY(MT)'.
    gt_fieldcat-seltext_m    = 'LIFTED QTY(MT)'.
    gt_fieldcat-ddictxt      = 'M'.
    gt_fieldcat-do_sum       = space.
    APPEND gt_fieldcat.
    CLEAR gt_fieldcat.
    gt_fieldcat-fieldname = 'UMMENGE1'.
    gt_fieldcat-outputlen    =  16.
    gt_fieldcat-just         =  'R' .
    gt_fieldcat-col_pos      =   7.
    gt_fieldcat-seltext_s    = 'ELIGIBLE QTY(MT)'.
    gt_fieldcat-seltext_m    = 'ELIGIBLE QTY(MT)'.
    gt_fieldcat-ddictxt      = 'M'.
*    GT_FIELDCAT-INPUT        = 'X'.
    APPEND gt_fieldcat.
    CLEAR gt_fieldcat.
    gt_fieldcat-fieldname = 'KBETR'.
    gt_fieldcat-outputlen    =  16.
    gt_fieldcat-just         =  'R' .
    gt_fieldcat-col_pos      =   8.
    gt_fieldcat-seltext_s    = 'RATE'.
    gt_fieldcat-seltext_m    = 'RATE'.
    gt_fieldcat-ddictxt      = 'M'.
*    GT_FIELDCAT-INPUT        = 'X'.
    APPEND gt_fieldcat.
    CLEAR gt_fieldcat.
    gt_fieldcat-fieldname = 'VALUE'.
    gt_fieldcat-outputlen    =  16.
    gt_fieldcat-just         =  'R' .
    gt_fieldcat-col_pos      =   9.
    gt_fieldcat-seltext_s    = 'VALUE'.
    gt_fieldcat-seltext_m    = 'VALUE'.
    gt_fieldcat-ddictxt      = 'M'.
    gt_fieldcat-do_sum       = space.
*    GT_FIELDCAT-INPUT        = 'X'.
    APPEND gt_fieldcat.
    CLEAR gt_fieldcat.
* BOC ASHOK & M A KHAN 07.06.07
    gt_fieldcat-fieldname = 'BEZEI'.
    gt_fieldcat-outputlen    =  35.
    gt_fieldcat-just         =  'L' .
    gt_fieldcat-col_pos      =  10.
    gt_fieldcat-seltext_s    = 'SALES OFFICE'.
    gt_fieldcat-seltext_m    = 'SALES OFFICE'.
    gt_fieldcat-ddictxt      = 'M'.
    gt_fieldcat-do_sum       = space.
    APPEND gt_fieldcat.
    CLEAR gt_fieldcat.
* EOC ASHOK & M A KHAN 07.06.07
    gt_fieldcat-fieldname = 'REM'.
    gt_fieldcat-outputlen    =  20.
    gt_fieldcat-just         =  'L' .
    gt_fieldcat-col_pos      =  11.
    gt_fieldcat-seltext_s    = 'REMARKS'.
    gt_fieldcat-seltext_m    = 'REMARKS'.
    gt_fieldcat-ddictxt      = 'M'.
    gt_fieldcat-do_sum       = space.
    gt_fieldcat-input        = 'X'.
    APPEND gt_fieldcat.
    CLEAR gt_fieldcat.
*        " SOC ADDED BY UJJWAL/PRIYANKA ON 7-07-2020 ON CHRM 4000002361
    gt_fieldcat-fieldname = 'VBELN'.
    gt_fieldcat-outputlen    =  10.
    gt_fieldcat-just         =  'L' .
    gt_fieldcat-col_pos      =  12.
    gt_fieldcat-seltext_s    = 'ORDER'.
    gt_fieldcat-seltext_m    = 'REBATE ORDER'.
    gt_fieldcat-ddictxt      = 'M'.
    gt_fieldcat-do_sum       = space.
    APPEND gt_fieldcat.
    CLEAR gt_fieldcat.
*        " EOC ADDED BY UJJWAL/PRIYANKA ON 7-07-2020 ON CHRM 4000002361
  ENDIF.
*
  i_exit_event-ucomm = 'FCOD'.
  i_exit_event-before = 'X'.
  i_exit_event-after = ''.
  APPEND i_exit_event.
  CLEAR i_exit_event.
  i_exit_event-ucomm = 'CALC'.
  i_exit_event-before = 'X'.
  i_exit_event-after = ''.
  APPEND i_exit_event.
  CLEAR i_exit_event.
  i_exit_event-ucomm = 'SALL'.
  i_exit_event-before = 'X'.
  i_exit_event-after = 'X'.
  APPEND i_exit_event.
  CLEAR i_exit_event.
  i_exit_event-ucomm = 'DSAL'.
  i_exit_event-before = 'X'.
  i_exit_event-after = 'X'.
  APPEND i_exit_event.
  CLEAR i_exit_event.
  i_events-name = 'USER_COMMAND'.
  i_events-form = 'ON_SELECTION'.
  APPEND i_events.
  CLEAR i_events.
ENDFORM.                    " CREATE_FIELD_CATALOG
*&                                                                     *
*&      FORM  TOP_OF_PAGE
*&                                                                     *
*       TEXT
*                                                                      *
*    >  P1        TEXT
*  <    P2        TEXT
*                                                                      *
FORM top_of_page .
**HEADING
  WRITE : /.
  WRITE : / 'ZONE   :' COLOR COL_HEADING, s_vkbur-low.
  WRITE : / 'SALES ORG :' COLOR COL_HEADING,s_vkorg-low.
  WRITE : / 'MATERIAL GROUP IN:' COLOR COL_HEADING.
  LOOP AT s_kondm.
    IF s_kondm-sign = 'I' AND s_kondm-option = 'BT' .
      WRITE:  s_kondm-low ,'TO', s_kondm-high.
    ELSEIF s_kondm-sign = 'I' AND s_kondm-option = 'EQ' .
      WRITE:  s_kondm-low , ' '.
    ELSEIF s_kondm-sign = 'E' AND s_kondm-option = 'BT' .
      WRITE: 'EXCLUDING :' , s_kondm-low ,'TO', s_kondm-high.
    ELSEIF s_kondm-sign = 'E' AND s_kondm-option = 'EQ' .
      WRITE: 'EXCLUDING :' , s_kondm-low , ' '.
    ENDIF.
  ENDLOOP.
  WRITE : / 'CUSTOMER CLASSIFICATION :' COLOR COL_HEADING, s_kukla.
  ULINE.
ENDFORM.                    " TOP_OF_PAGE
*&                                                                     *
*&      FORM  TABLE_UPDATE
*&                                                                     *
*       TEXT
*                                                                      *
*    >  P1        TEXT
*  <    P2        TEXT
*                                                                      *
FORM table_update .
  CLEAR yrva_rebate.
  yrva_rebate-vbeln        = w_vbeln.
  yrva_rebate-kunnr        = i_reb2-pkunag. "X_SOLD_TO_PARTY-SOLD_TO.
  yrva_rebate-vkbur        = i_reb2-vkbur.
  yrva_rebate-lft_qty      = i_reb2-ummenge.
  yrva_rebate-werks        = i_reb2-werks.
  yrva_rebate-rev_qty      = i_reb2-ummenge1.
  IF wa_rec-kschl is NOT INITIAL.
    yrva_rebate-reb_cond     = wa_rec-kschl.
  ELSE.
    yrva_rebate-reb_cond     = p_kschl.
  ENDIF.
  yrva_rebate-ord_cond     = x_order_header_in-cd_type1.
  yrva_rebate-value        = i_reb2-value.
  yrva_rebate-yy_per_start = s_sptag-low.
  yrva_rebate-yy_per_end   = s_sptag-high.
  yrva_rebate-kukla        = i_reb2-kukla.
  INSERT yrva_rebate FROM yrva_rebate.
ENDFORM.                    " TABLE_UPDATE
*&                                                                     *
*&      FORM  FILTER_DATA
*&                                                                     *
*       TEXT
*                                                                      *
*    >  P1        TEXT
*  <    P2        TEXT
*                                                                      *
FORM filter_data .
** SOC BY UJJWAL/PRIYANKA ON CHARM#400000157 ON 18-09-2019 TO SELECT THE
  TYPES : BEGIN OF ty_vbrk,
            vbeln TYPE vbrk-vbeln,
            fkdat TYPE vbrk-fkdat,
            vkorg TYPE vbrk-vkorg,
            kunag TYPE vbrk-kunag,
            kdgrp TYPE vbrk-kdgrp,
            vtweg TYPE vbrk-vtweg,
            posnr TYPE posnr_vf,
            werks TYPE vbrp-werks,
            fkimg TYPE vbrp-fkimg,
            vkbur TYPE vbrp-vkbur,
            kvgr2 TYPE vbrp-kvgr2,
            kvgr3 TYPE vbrp-kvgr3,
            kondm TYPE vbrp-kondm,
          END OF ty_vbrk.
  DATA : lt_vbrk TYPE TABLE OF ty_vbrk,
         lw_vbrk TYPE ty_vbrk.
*  IF NOT GT_S925[] IS INITIAL.
** FETCHING FOR FKART = 'RE'  AND VBTYP = 'O'
  SELECT a~vbeln,
         a~fkdat,
         a~vkorg,
         a~kunag,
         a~kdgrp,
         a~vtweg,
         b~posnr,
         b~werks,
         b~fkimg,
         b~vkbur,
         b~kvgr2,
         b~kvgr3,
         b~kondm
         FROM vbrk AS a
         INNER JOIN vbrp AS b
         ON a~vbeln = b~vbeln
         INTO TABLE @DATA(lt_vbrk_re)
         WHERE a~fkart = 'RE'
         AND a~fkdat IN @s_sptag
         AND a~kunag IN @s_pkunag
         AND a~vkorg IN @s_vkorg
         AND a~kdgrp IN @s_kdgrp
         AND a~vtweg IN @s_dist
         AND a~vbtyp = 'O'
         AND a~fksto  = @space
         AND b~vkbur IN @s_vkbur
         AND b~werks IN @s_werks
         AND b~kvgr2 IN @s_kvgr2
*           AND B~KVGR3 IN @S_KVGR3
         AND b~kondm IN @s_kondm
** -> BEGIN OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
        AND a~draft = @space
  AND b~draft = @space.
** -> END OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
  IF sy-subrc IS INITIAL.
    SORT lt_vbrk_re BY vbeln fkdat kunag.
    DELETE ADJACENT DUPLICATES FROM lt_vbrk_re COMPARING vbeln fkdat kun
    DELETE lt_vbrk_re WHERE werks NOT IN s_werks.
***    SORT LT_VBRK_RE BY VBELN.
***    SELECT VBELV,
***           VBELN
***      FROM VBFA
***      INTO TABLE @DATA(LT_VBFA)
***      FOR ALL ENTRIES IN @LT_VBRK_RE
***      WHERE VBELN = @LT_VBRK_RE-VBELN
***      AND VBTYP_V = 'M'.
***    IF SY-SUBRC IS INITIAL.
***      SORT LT_VBFA BY VBELV VBELN.
***
***      SELECT VBELN,
***             FKART
***        FROM VBRK
***        INTO TABLE @DATA(LT_VBRK1)
***        FOR ALL ENTRIES IN @LT_VBFA
***        WHERE VBELN  = LT_VBFA-VBELV.
***      IF SY-SUBRC IS INITIAL.
***        SORT LT_VBRK1 BY VBELN FKART.
***        LOOP AT LT_VBRK1 ASSIGNING FIELD-SYMBOL(<LFS_VBRK1>).
***          READ TABLE LT_VBFA ASSIGNING FIELD-SYMBOL(<LFS_VBFA>) WITH
***          IF SY-SUBRC IS INITIAL.
***            READ TABLE LT_VBRK_RE ASSIGNING FIELD-SYMBOL(<LFS_VBRK_RE
***            IF SY-SUBRC IS INITIAL.
***                IF <LFS_VBRK_RE>-FKDAT+4(2) = <LFS_VBRK1>-FKART+4(2).
***
***                ENDIF.
***            ENDIF.
***          ENDIF.
***
***        ENDLOOP.
***      ENDIF.
***    ENDIF.
    CLEAR : lw_vbrk.
    LOOP AT lt_vbrk_re INTO lw_vbrk.
      i_reb1-pkunag = lw_vbrk-kunag.
      i_reb1-vkbur = lw_vbrk-vkbur.
      i_reb1-vkorg = lw_vbrk-vkorg.
      i_reb1-kvgr2 = lw_vbrk-kvgr2.
      i_reb1-werks = lw_vbrk-werks.
      i_reb1-kondm = lw_vbrk-kondm.
      i_reb1-kdgrp = lw_vbrk-kdgrp.
      i_reb1-ummenge = lw_vbrk-fkimg * -1.
      APPEND i_reb1.
      CLEAR i_reb1.
    ENDLOOP.
  ENDIF.
  SELECT * FROM tkukl INTO TABLE @DATA(lt_tkukl).
  REFRESH: s_kukla2[].
  LOOP AT s_kukla.
    IF s_kukla-low = 'Trader'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = '14' high = '14' )
    ENDIF.
    IF s_kukla-low = 'AUT'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = '29' high = '29' )
    ENDIF.
    IF s_kukla-low = 'Actual user'.
      LOOP AT lt_tkukl INTO DATA(ls_tkukl).
        IF ls_tkukl IS NOT INITIAL.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_tkukl-kukla
        ENDIF.
        CLEAR: ls_tkukl.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
  SELECT a~vbeln,
         a~fkdat,
         a~vkorg,
         a~kunag,
         a~kdgrp,
         a~vtweg,
         b~posnr,
         b~werks,
         b~fkimg,
         b~vkbur,
         b~kvgr2,
         b~kvgr3,
         b~kondm
         FROM vbrk AS a
         INNER JOIN vbrp AS b
         ON a~vbeln = b~vbeln
** SOC by Vaishnavi/Archna TR: DVRK9A1OTG  " Inclusion of sales type
INNER JOIN vbfa AS f
  ON f~vbeln   = a~vbeln        " billing as subsequent
 AND f~vbtyp_v = 'C'            " preceding is a Sales Order
 INNER JOIN kna1 AS c
  ON c~kunnr   = a~kunag
INNER JOIN vbak AS so
  ON so~vbeln = f~vbelv
** EOC by Vaishnavi/Archna TR: DVRK9A1OTG
         INTO TABLE  @lt_vbrk
         WHERE a~fkdat IN @s_sptag
         AND   a~kunag IN @s_pkunag
         AND a~vkorg IN @s_vkorg
         AND a~kdgrp IN @s_kdgrp
         AND a~vtweg IN @s_dist
         AND a~vbtyp = 'M'
         AND a~fksto  = @space
         AND b~vkbur IN @s_vkbur
         AND b~werks IN @s_werks
         AND b~kvgr2 IN @s_kvgr2
*           AND B~KVGR3 IN @S_KVGR3
         AND b~kondm IN @s_kondm
** SOC by Vaishnavi/Archna TR: DVRK9A1OTG
        AND so~auart IN @s_auart
        AND c~kukla IN @s_kukla2
** EOC by Vaishnavi/Archna TR: DVRK9A1OTG
** -> BEGIN OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
         AND a~draft = @space
  AND b~draft = @space.
** -> END OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
  IF sy-subrc IS INITIAL.
**    SORT lt_vbrk BY fkdat kunag.
    SORT lt_vbrk BY vbeln fkdat kunag posnr werks.
    DELETE ADJACENT DUPLICATES FROM lt_vbrk COMPARING vbeln fkdat kunag
    DELETE lt_vbrk WHERE werks NOT IN s_werks.
    SORT lt_vbrk.
    DELETE ADJACENT DUPLICATES FROM lt_vbrk COMPARING ALL FIELDS.
    CLEAR : lw_vbrk.
    LOOP AT lt_vbrk INTO lw_vbrk.
      i_reb1-pkunag = lw_vbrk-kunag.
      i_reb1-vkbur = lw_vbrk-vkbur.
      i_reb1-vkorg = lw_vbrk-vkorg.
      i_reb1-kvgr2 = lw_vbrk-kvgr2.
      i_reb1-werks = lw_vbrk-werks.
      i_reb1-kondm = lw_vbrk-kondm.
      i_reb1-kdgrp = lw_vbrk-kdgrp.
      i_reb1-ummenge = lw_vbrk-fkimg.
      APPEND i_reb1.
      CLEAR i_reb1.
    ENDLOOP.
  ENDIF.
** EOC BY UJJWAL/PRIYANKA ON CHARM#400000157 ON 18-09-2019 TO SELECT THE
**                    B.O.C BY RG DATED ON 17.04.2026
  IF p_kschl1 IS NOT INITIAL.
    PERFORM second_slab.
  ENDIF.
**                    E.O.C BY RG DATED ON 17.04.2026
ENDFORM.
*&                                                                     *
*&      FORM  FILTER_DATA_1
*&                                                                     *
*       TEXT
*                                                                      *
*    >  P1        TEXT
*  <    P2        TEXT
*                                                                      *
FORM filter_data_1 .
  TYPES : BEGIN OF ty_vbrk,
            vbeln TYPE vbrk-vbeln,
            fkdat TYPE vbrk-fkdat,
            vkorg TYPE vbrk-vkorg,
            kunag TYPE vbrk-kunag,
            kdgrp TYPE vbrk-kdgrp,
            vtweg TYPE vbrk-vtweg,
            posnr TYPE posnr_vf,
            werks TYPE vbrp-werks,
            fkimg TYPE vbrp-fkimg,
            vkbur TYPE vbrp-vkbur,
            kvgr2 TYPE vbrp-kvgr2,
            kvgr3 TYPE vbrp-kvgr3,
            kondm TYPE vbrp-kondm,
          END OF ty_vbrk.
  DATA : lt_vbrk TYPE TABLE OF ty_vbrk,
         lw_vbrk TYPE ty_vbrk.
*  IF NOT GT_S925[] IS INITIAL.
** FETCHING FOR FKART = 'RE'  AND VBTYP = 'O'
  SELECT a~vbeln,
         a~fkdat,
         a~vkorg,
         a~kunag,
         a~kdgrp,
         a~vtweg,
         b~posnr,
         b~werks,
         b~fkimg,
         b~vkbur,
         b~kvgr2,
         b~kvgr3,
         b~kondm
         FROM vbrk AS a
         INNER JOIN vbrp AS b
         ON a~vbeln = b~vbeln
** SOC by Vaishnavi/Archna TR: DVRK9A1OTG  " Inclusion of sales type
INNER JOIN vbfa AS f
  ON f~vbeln   = a~vbeln        " billing as subsequent
 AND f~vbtyp_v = 'C'            " preceding is a Sales Order
 INNER JOIN kna1 AS c
  ON c~kunnr   = a~kunag
INNER JOIN vbak AS so
  ON so~vbeln = f~vbelv
** EOC by Vaishnavi/Archna TR: DVRK9A1OTG
         INTO TABLE @DATA(lt_vbrk_re)
         WHERE a~fkart = 'RE'
         AND a~fkdat IN @s_sptag
         AND a~kunag IN @s_pkunag
         AND a~vkorg IN @s_vkorg
         AND a~kdgrp IN @s_kdgrp
         AND a~vtweg IN @s_dist
         AND a~vbtyp = 'O'
         AND a~fksto  = @space
         AND b~vkbur IN @s_vkbur
         AND b~werks IN @s_werks
         AND b~kvgr2 IN @s_kvgr2
         AND b~kvgr4 = @s_kvgr4
*           AND B~KVGR3 IN @S_KVGR3
         AND b~kondm IN @s_kondm
** SOC by Vaishnavi/Archna TR: DVRK9A1OTG
        AND so~auart IN @s_auart
        AND c~kukla IN @s_kukla2
** EOC by Vaishnavi/Archna TR: DVRK9A1OTG
** -> BEGIN OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
        AND b~draft = @space
  AND a~draft = @space.
** -> END OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
  IF sy-subrc IS INITIAL.
    SORT lt_vbrk_re BY vbeln fkdat kunag.
    DELETE ADJACENT DUPLICATES FROM lt_vbrk_re COMPARING vbeln fkdat kun
    DELETE lt_vbrk_re WHERE werks NOT IN s_werks.
    CLEAR : lw_vbrk.
    LOOP AT lt_vbrk_re INTO lw_vbrk.
      i_reb1-pkunag = lw_vbrk-kunag.
      i_reb1-vkbur = lw_vbrk-vkbur.
      i_reb1-vkorg = lw_vbrk-vkorg.
      i_reb1-kvgr2 = lw_vbrk-kvgr2.
      i_reb1-werks = lw_vbrk-werks.
      i_reb1-kondm = lw_vbrk-kondm.
      i_reb1-kdgrp = lw_vbrk-kdgrp.
      i_reb1-ummenge = lw_vbrk-fkimg * -1.
      APPEND i_reb1.
      CLEAR i_reb1.
    ENDLOOP.
  ENDIF.
  SELECT * FROM tkukl INTO TABLE @DATA(lt_tkukl).
  REFRESH: s_kukla2[].
  LOOP AT s_kukla.
    IF s_kukla-low = 'Trader'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = '14' high = '14' )
    ENDIF.
    IF s_kukla-low = 'AUT'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = '29' high = '29' )
    ENDIF.
    IF s_kukla-low = 'Actual user'.
      LOOP AT lt_tkukl INTO DATA(ls_tkukl).
        IF ls_tkukl IS NOT INITIAL.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_tkukl-kukla
        ENDIF.
        CLEAR: ls_tkukl.
      ENDLOOP.
    ENDIF.
  ENDLOOP.
  SELECT a~vbeln,
         a~fkdat,
         a~vkorg,
         a~kunag,
         a~kdgrp,
         a~vtweg,
         b~posnr,
         b~werks,
         b~fkimg,
         b~vkbur,
         b~kvgr2,
         b~kvgr3,
         b~kondm
         FROM vbrk AS a
         INNER JOIN vbrp AS b
         ON a~vbeln = b~vbeln
         INTO TABLE  @lt_vbrk
         WHERE a~fkdat IN @s_sptag
         AND   a~kunag IN @s_pkunag
         AND a~vkorg IN @s_vkorg
         AND a~kdgrp IN @s_kdgrp
         AND a~vtweg IN @s_dist
         AND a~vbtyp = 'M'
         AND a~fksto  = @space
         AND b~vkbur IN @s_vkbur
         AND b~werks IN @s_werks
         AND b~kvgr2 IN @s_kvgr2
         AND b~kvgr4 = @s_kvgr4
*           AND B~KVGR3 IN @S_KVGR3
         AND b~kondm IN @s_kondm
** -> BEGIN OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
        AND b~draft = @space
  AND a~draft = @space.
** -> END OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
  IF sy-subrc IS INITIAL.
    SORT lt_vbrk BY fkdat kunag.
    DELETE ADJACENT DUPLICATES FROM lt_vbrk COMPARING vbeln fkdat kunag
    DELETE lt_vbrk WHERE werks NOT IN s_werks.
    CLEAR : lw_vbrk.
    LOOP AT lt_vbrk INTO lw_vbrk.
      i_reb1-pkunag = lw_vbrk-kunag.
      i_reb1-vkbur = lw_vbrk-vkbur.
      i_reb1-vkorg = lw_vbrk-vkorg.
      i_reb1-kvgr2 = lw_vbrk-kvgr2.
      i_reb1-werks = lw_vbrk-werks.
      i_reb1-kondm = lw_vbrk-kondm.
      i_reb1-kdgrp = lw_vbrk-kdgrp.
      i_reb1-ummenge = lw_vbrk-fkimg.
      APPEND i_reb1.
      CLEAR i_reb1.
    ENDLOOP.
  ENDIF.
  IF p_kschl1 IS NOT INITIAL.
    PERFORM second_slab_1.
  ENDIF.
ENDFORM.
*&                                                                     *
*& Form CUSTCLSS
*&                                                                     *
*& text
*&                                                                     *
*&   >  p1        text
*& <    p2        text
*&                                                                     *
FORM custclss.
  REFRESH: lt_custclss.
  ls_custclss-text = 'Actual user'.
  APPEND ls_custclss TO lt_custclss.
  ls_custclss-text = 'Trader'.
  APPEND ls_custclss TO lt_custclss.
  ls_custclss-text = 'AUT'.
  APPEND ls_custclss TO lt_custclss.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'KUKLA'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'P_KUKLA-LOW'
      value_org       = 'S'
    TABLES
      value_tab       = lt_custclss
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.
ENDFORM.
*&                                                                     *
*& Form second_slab
*&                                                                     *
*& text
*&                                                                     *
*&   >  p1        text
*& <    p2        text
*&                                                                     *
FORM second_slab .
  DATA : LV_MONTH TYPE CHAR2.
  TYPES : s_sptag_1 TYPE RANGE OF s925-sptag.
  DATA : lv_sptag TYPE s_sptag_1.
  IF s_sptag-high IS NOT INITIAL.
    LOOP AT s_sptag.
      LV_MONTH = s_sptag-high+4(2) - 1.
      LV_MONTH =  { '0' && LV_MONTH } .
      lv_sptag = VALUE s_sptag_1( ( sign = s_sptag-sign option = s_sptag
      low = s_sptag-low
      high =  { s_sptag-high+0(4) && LV_MONTH &&  s_sptag-high+6(2) }  )
    ENDLOOP.
  ENDIF.
** FETCHING FOR FKART = 'RE'  AND VBTYP = 'O'
  SELECT a~vbeln,
         a~fkdat,
         a~vkorg,
         a~kunag,
         a~kdgrp,
         a~vtweg,
         b~posnr,
         b~werks,
         b~fkimg,
         b~vkbur,
         b~kvgr2,
         b~kvgr3,
         b~kondm
         FROM vbrk AS a
         INNER JOIN vbrp AS b
         ON a~vbeln = b~vbeln
         INTO TABLE @DATA(lt_vbrk_re_1)
         WHERE a~fkart = 'RE'
         AND a~fkdat IN @lv_sptag
         AND a~kunag IN @s_pkunag
         AND a~vkorg IN @s_vkorg
         AND a~kdgrp IN @s_kdgrp
         AND a~vtweg IN @s_dist
         AND a~vbtyp = 'O'
         AND a~fksto  = @space
         AND b~vkbur IN @s_vkbur
         AND b~werks IN @s_werks
         AND b~kvgr2 IN @s_kvgr2
*           AND B~KVGR3 IN @S_KVGR3
         AND b~kondm IN @s_kondm
** -> BEGIN OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
        AND a~draft = @space
  AND b~draft = @space.
** -> END OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
  IF sy-subrc IS INITIAL.
    SORT lt_vbrk_re_1 BY vbeln fkdat kunag.
    DELETE ADJACENT DUPLICATES FROM lt_vbrk_re_1 COMPARING vbeln fkdat k
    DELETE lt_vbrk_re_1 WHERE werks NOT IN s_werks.
    SORT LT_VBRK_RE_1 BY VBELN.
*    CLEAR : lw_vbrk.
    LOOP AT lt_vbrk_re_1 INTO DATA(lw_vbrk_1).
      i_reb_1-pkunag = lw_vbrk_1-kunag.
      i_reb_1-vkbur = lw_vbrk_1-vkbur.
      i_reb_1-vkorg = lw_vbrk_1-vkorg.
      i_reb_1-kvgr2 = lw_vbrk_1-kvgr2.
      i_reb_1-werks = lw_vbrk_1-werks.
      i_reb_1-kondm = lw_vbrk_1-kondm.
      i_reb_1-kdgrp = lw_vbrk_1-kdgrp.
      i_reb_1-ummenge = lw_vbrk_1-fkimg * -1.
      APPEND i_reb_1.
      CLEAR i_reb_1.
    ENDLOOP.
  ENDIF.
*  SELECT * FROM tkukl INTO TABLE @DATA(lt_tkukl).
*  REFRESH: s_kukla2[].
*  LOOP AT s_kukla.
*    IF s_kukla-low = 'Trader'.
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = '14' high = '14' )
*    ENDIF.
*    IF s_kukla-low = 'AUT'.
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = '29' high = '29' )
*    ENDIF.
*    IF s_kukla-low = 'Actual user'.
*      LOOP AT lt_tkukl INTO DATA(ls_tkukl).
*        IF ls_tkukl IS NOT INITIAL.
*          APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_tkukl-kukla
*        ENDIF.
*        CLEAR: ls_tkukl.
*      ENDLOOP.
*    ENDIF.
*  ENDLOOP.
  SELECT a~vbeln,
         a~fkdat,
         a~vkorg,
         a~kunag,
         a~kdgrp,
         a~vtweg,
         b~posnr,
         b~werks,
         b~fkimg,
         b~vkbur,
         b~kvgr2,
         b~kvgr3,
         b~kondm
         FROM vbrk AS a
         INNER JOIN vbrp AS b
         ON a~vbeln = b~vbeln
** SOC by Vaishnavi/Archna TR: DVRK9A1OTG  " Inclusion of sales type
INNER JOIN vbfa AS f
  ON f~vbeln   = a~vbeln        " billing as subsequent
 AND f~vbtyp_v = 'C'            " preceding is a Sales Order
 INNER JOIN kna1 AS c
  ON c~kunnr   = a~kunag
INNER JOIN vbak AS so
  ON so~vbeln = f~vbelv
** EOC by Vaishnavi/Archna TR: DVRK9A1OTG
         INTO TABLE  @DATA(lt_vbrk_1)
         WHERE a~fkdat IN @lv_sptag
         AND   a~kunag IN @s_pkunag
         AND a~vkorg IN @s_vkorg
         AND a~kdgrp IN @s_kdgrp
         AND a~vtweg IN @s_dist
         AND a~vbtyp = 'M'
         AND a~fksto  = @space
         AND b~vkbur IN @s_vkbur
         AND b~werks IN @s_werks
         AND b~kvgr2 IN @s_kvgr2
*           AND B~KVGR3 IN @S_KVGR3
         AND b~kondm IN @s_kondm
** SOC by Vaishnavi/Archna TR: DVRK9A1OTG
        AND so~auart IN @s_auart
        AND c~kukla IN @s_kukla2
** EOC by Vaishnavi/Archna TR: DVRK9A1OTG
** -> BEGIN OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
         AND a~draft = @space
  AND b~draft = @space.
** -> END OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
  IF sy-subrc IS INITIAL.
**    SORT lt_vbrk BY fkdat kunag.
    SORT lt_vbrk_1 BY vbeln fkdat kunag posnr werks.
    DELETE ADJACENT DUPLICATES FROM lt_vbrk_1 COMPARING vbeln fkdat kuna
    DELETE lt_vbrk_1 WHERE werks NOT IN s_werks.
    SORT lt_vbrk_1.
    DELETE ADJACENT DUPLICATES FROM lt_vbrk_1 COMPARING ALL FIELDS.
*
*    CLEAR : lw_vbrk.
    LOOP AT lt_vbrk_1 INTO DATA(lw_vbrk_2).
      i_reb_1-pkunag = lw_vbrk_2-kunag.
      i_reb_1-vkbur = lw_vbrk_2-vkbur.
      i_reb_1-vkorg = lw_vbrk_2-vkorg.
      i_reb_1-kvgr2 = lw_vbrk_2-kvgr2.
      i_reb_1-werks = lw_vbrk_2-werks.
      i_reb_1-kondm = lw_vbrk_2-kondm.
      i_reb_1-kdgrp = lw_vbrk_2-kdgrp.
      i_reb_1-ummenge = lw_vbrk_2-fkimg.
      APPEND i_reb_1.
      CLEAR i_reb_1.
    ENDLOOP.
  ENDIF.
  DATA(i_reb_7) = i_reb_1[].
    SORT i_reb_7 BY  pkunag vkbur kvgr2 werks.
    DELETE ADJACENT DUPLICATES FROM i_reb_7[] COMPARING pkunag.
    SORT i_reb_1 BY pkunag vkbur kvgr2 werks.
*  BREAK-POINT.
****SOC BY RAM GUPTA ON 20-04-2026 TO DISPLAY SUM ON CUSTOMER BASIS
    LOOP AT i_reb_7 INTO DATA(WA_REB_7).
      LOOP AT i_reb_1 WHERE pkunag = WA_REB_7-pkunag.
        IF i_reb_1 IS NOT INITIAL.
          i_reb_2-name1   = i_reb_1-name1.
          i_reb_2-pkunag = i_reb_1-pkunag.
          i_reb_2-vkbur = i_reb_1-vkbur.
          i_reb_2-kvgr2 = i_reb_1-kvgr2.
          i_reb_2-kukla = i_reb_1-kukla.
          i_reb_2-ummenge = i_reb_1-ummenge.
          i_reb_2-ummenge1 = i_reb_1-ummenge.
          COLLECT i_reb_2.
        ENDIF.
        CLEAR: WA_REB_7, i_reb_1.
      ENDLOOP.
    ENDLOOP.
    SORT i_reb_2 BY kvgr2.
ENDFORM.
*&                                                                     *
*& Form second_slab_1
*&                                                                     *
*& text
*&                                                                     *
*&   >  p1        text
*& <    p2        text
*&                                                                     *
FORM second_slab_1 .
  DATA : LV_MONTH TYPE CHAR2.
  TYPES : s_sptag_1 TYPE RANGE OF s925-sptag.
  DATA : lv_sptag TYPE s_sptag_1.
  IF s_sptag-high IS NOT INITIAL.
    LOOP AT s_sptag.
      LV_MONTH = s_sptag-high+4(2) - 1.
      LV_MONTH =  { '0' && LV_MONTH } .
      lv_sptag = VALUE s_sptag_1( ( sign = s_sptag-sign option = s_sptag
      low = s_sptag-low
      high =  { s_sptag-high+0(4) && LV_MONTH &&  s_sptag-high+6(2) }  )
    ENDLOOP.
  ENDIF.
** FETCHING FOR FKART = 'RE'  AND VBTYP = 'O'
  SELECT a~vbeln,
         a~fkdat,
         a~vkorg,
         a~kunag,
         a~kdgrp,
         a~vtweg,
         b~posnr,
         b~werks,
         b~fkimg,
         b~vkbur,
         b~kvgr2,
         b~kvgr3,
         b~kondm
         FROM vbrk AS a
         INNER JOIN vbrp AS b
         ON a~vbeln = b~vbeln
** SOC by Vaishnavi/Archna TR: DVRK9A1OTG  " Inclusion of sales type
INNER JOIN vbfa AS f
  ON f~vbeln   = a~vbeln        " billing as subsequent
 AND f~vbtyp_v = 'C'            " preceding is a Sales Order
 INNER JOIN kna1 AS c
  ON c~kunnr   = a~kunag
INNER JOIN vbak AS so
  ON so~vbeln = f~vbelv
** EOC by Vaishnavi/Archna TR: DVRK9A1OTG
         INTO TABLE @DATA(lt_vbrk_re_1)
         WHERE a~fkart = 'RE'
         AND a~fkdat IN @lv_sptag
         AND a~kunag IN @s_pkunag
         AND a~vkorg IN @s_vkorg
         AND a~kdgrp IN @s_kdgrp
         AND a~vtweg IN @s_dist
         AND a~vbtyp = 'O'
         AND a~fksto  = @space
         AND b~vkbur IN @s_vkbur
         AND b~werks IN @s_werks
         AND b~kvgr2 IN @s_kvgr2
         AND b~kvgr4 = @s_kvgr4
*           AND B~KVGR3 IN @S_KVGR3
         AND b~kondm IN @s_kondm
** SOC by Vaishnavi/Archna TR: DVRK9A1OTG
        AND so~auart IN @s_auart
        AND c~kukla IN @s_kukla2
** EOC by Vaishnavi/Archna TR: DVRK9A1OTG
** -> BEGIN OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
        AND b~draft = @space
  AND a~draft = @space.
** -> END OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
  IF sy-subrc IS INITIAL.
    SORT lt_vbrk_re_1 BY vbeln fkdat kunag.
    DELETE ADJACENT DUPLICATES FROM lt_vbrk_re_1 COMPARING vbeln fkdat k
    DELETE lt_vbrk_re_1 WHERE werks NOT IN s_werks.
*    CLEAR : lw_vbrk.
    LOOP AT lt_vbrk_re_1 INTO DATA(lw_vbrk_1).
      i_reb_1-pkunag = lw_vbrk_1-kunag.
      i_reb_1-vkbur = lw_vbrk_1-vkbur.
      i_reb_1-vkorg = lw_vbrk_1-vkorg.
      i_reb_1-kvgr2 = lw_vbrk_1-kvgr2.
      i_reb_1-werks = lw_vbrk_1-werks.
      i_reb_1-kondm = lw_vbrk_1-kondm.
      i_reb_1-kdgrp = lw_vbrk_1-kdgrp.
      i_reb_1-ummenge = lw_vbrk_1-fkimg * -1.
      APPEND i_reb_1.
      CLEAR i_reb_1.
    ENDLOOP.
  ENDIF.
  SELECT a~vbeln,
         a~fkdat,
         a~vkorg,
         a~kunag,
         a~kdgrp,
         a~vtweg,
         b~posnr,
         b~werks,
         b~fkimg,
         b~vkbur,
         b~kvgr2,
         b~kvgr3,
         b~kondm
         FROM vbrk AS a
         INNER JOIN vbrp AS b
         ON a~vbeln = b~vbeln
         INTO TABLE  @DATA(lt_vbrk_1)
         WHERE a~fkdat IN @lv_sptag
         AND   a~kunag IN @s_pkunag
         AND a~vkorg IN @s_vkorg
         AND a~kdgrp IN @s_kdgrp
         AND a~vtweg IN @s_dist
         AND a~vbtyp = 'M'
         AND a~fksto  = @space
         AND b~vkbur IN @s_vkbur
         AND b~werks IN @s_werks
         AND b~kvgr2 IN @s_kvgr2
         AND b~kvgr4 = @s_kvgr4
*           AND B~KVGR3 IN @S_KVGR3
         AND b~kondm IN @s_kondm
** -> BEGIN OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
        AND b~draft = @space
  AND a~draft = @space.
** -> END OF CHANGES BY OF RITIKA ON 09.10.2024 FOR ATC
  IF sy-subrc IS INITIAL.
    SORT lt_vbrk_1 BY fkdat kunag.
    DELETE ADJACENT DUPLICATES FROM lt_vbrk_1 COMPARING vbeln fkdat kuna
    DELETE lt_vbrk_1 WHERE werks NOT IN s_werks.
*    CLEAR : lw_vbrk.
    LOOP AT lt_vbrk_1 INTO DATA(lw_vbrk_2).
      i_reb_1-pkunag = lw_vbrk_2-kunag.
      i_reb_1-vkbur = lw_vbrk_2-vkbur.
      i_reb_1-vkorg = lw_vbrk_2-vkorg.
      i_reb_1-kvgr2 = lw_vbrk_2-kvgr2.
      i_reb_1-werks = lw_vbrk_2-werks.
      i_reb_1-kondm = lw_vbrk_2-kondm.
      i_reb_1-kdgrp = lw_vbrk_2-kdgrp.
      i_reb_1-ummenge = lw_vbrk_2-fkimg.
      APPEND i_reb_1.
      CLEAR i_reb_1.
    ENDLOOP.
  ENDIF.
    DATA(i_reb_7) = i_reb_1[].
    SORT i_reb_7 BY  pkunag vkbur kvgr2 werks.
    DELETE ADJACENT DUPLICATES FROM i_reb_7[] COMPARING pkunag.
    SORT i_reb_1 BY pkunag vkbur kvgr2 werks.
*  BREAK-POINT.
****SOC BY RAM GUPTA ON 20-04-2026 TO DISPLAY SUM ON CUSTOMER BASIS
    LOOP AT i_reb_7 INTO DATA(WA_REB_7).
      LOOP AT i_reb_1 WHERE pkunag = WA_REB_7-pkunag.
        IF i_reb_1 IS NOT INITIAL.
          i_reb_2-name1   = i_reb_1-name1.
          i_reb_2-pkunag = i_reb_1-pkunag.
          i_reb_2-vkbur = i_reb_1-vkbur.
          i_reb_2-kvgr2 = i_reb_1-kvgr2.
          i_reb_2-kukla = i_reb_1-kukla.
          i_reb_2-ummenge = i_reb_1-ummenge.
          i_reb_2-ummenge1 = i_reb_1-ummenge.
          COLLECT i_reb_2.
        ENDIF.
        CLEAR: WA_REB_7, i_reb_1.
      ENDLOOP.
    ENDLOOP.
    SORT i_reb_2 BY kvgr2.
ENDFORM.
 Selection Screen for Rebate Calculation
 Credit Memo Request already created
 Sales Order has been created with :
 and Customer No :
 Do you want to save varient
 Successfully saved
 Variant already saved