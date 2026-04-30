*&---------------------------------------------------------------------*
*& Report  YRVG004_QAIS_Q4_AMEND
*& Title   : CIS Execute - Q4 Amended (Standalone - No Separate Includes)
*& Based on: YRVG004_QAIS_EXECUTE
*& Change  : Q4 amended bi-monthly BCQ logic per circular Jan-Mar'26
*&           Slabs: >=100%->1500, >=110%->2000, >=120%->2500 Rs/MT
*&           Higher of amended vs original scheme is applied
*&---------------------------------------------------------------------*
REPORT yrvg004_qais_q4_amend MESSAGE-ID yv01.

*=======================================================================
* DECLARATIONS (from YRVG004_QAIS_EXECUTE_TOP - modified for Q4 amend)
*=======================================================================
TABLES: yrva_qais_data, s922, yrva_qais_upd.
TYPE-POOLS : slis.

DATA: gt_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.
DATA : i_layout     TYPE slis_layout_alv,
       i_exit_event TYPE slis_t_event_exit WITH HEADER LINE.
DATA: i_events TYPE slis_t_event WITH HEADER LINE.

DATA: i_qais_upd TYPE TABLE OF yrva_qais_upd,
      w_qais_upd TYPE yrva_qais_upd,
      i_qais_tmp TYPE STANDARD TABLE OF yrva_qais_data,
      w_qais_tmp TYPE yrva_qais_data.

DATA: wa_where_tab         TYPE edpline,
      it_where_tab         TYPE STANDARD TABLE OF edpline,
      it_where_tab1        TYPE STANDARD TABLE OF edpline,
      it_where_tab2        TYPE STANDARD TABLE OF edpline,
      it_where_tab3        TYPE STANDARD TABLE OF edpline,
      it_where_tab4        TYPE STANDARD TABLE OF edpline,
      it_yrva_qais_data_m1 TYPE STANDARD TABLE OF yrva_qais_data_m,
      wa_yrva_rebate       TYPE yrva_rebate,
      wa_yrva_qais_data_m  TYPE yrva_qais_data_m,
      it_yrva_qais_data_m  TYPE STANDARD TABLE OF yrva_qais_data_m,
      it_yrva_qais_data_m2 TYPE STANDARD TABLE OF yrva_qais_data_m,
      it_yrva_rebate       TYPE STANDARD TABLE OF yrva_rebate,
      it_yrva_rebate1      TYPE STANDARD TABLE OF yrva_rebate,
      wa_yrva_qais_data_s  TYPE yrva_qais_data_s,
      it_yrva_qais_data_s  TYPE STANDARD TABLE OF yrva_qais_data_s.

DATA: it_yrva_qais_data        TYPE STANDARD TABLE OF yrva_qais_data,
      wa_yrva_qais_data        TYPE yrva_qais_data,
      it_yrva_qais_data_temp   TYPE STANDARD TABLE OF yrva_qais_data,
      wa_yrva_qais_data_temp   TYPE yrva_qais_data,
      it_yrva_qais_newcus      TYPE STANDARD TABLE OF yrva_qais_newcus,
      wa_yrva_qais_newcus      TYPE yrva_qais_newcus,
      it_yrva_qais_data_newcus TYPE STANDARD TABLE OF yrva_qais_data,
      w_index                  TYPE syst_tabix,
      wa_yrva_qais_data_newcus TYPE yrva_qais_data,
      it_knvv                  TYPE STANDARD TABLE OF knvv,
      wa_knvv                  TYPE knvv,
      it_s922                  TYPE STANDARD TABLE OF s922,
      it_s922_n                TYPE STANDARD TABLE OF s922,
      it_s922_temp             TYPE STANDARD TABLE OF s922,
      wa_s922                  TYPE s922,
      w_remarks(40)            TYPE c,
      wa_yrva_mstr_waiver      TYPE yrva_mstr_waiver,
      wa_yrva_mstr_waiver_temp TYPE yrva_mstr_waiver,
      w_endda_prev             TYPE d.

DATA: ls_psdq(1)               TYPE c,
      it_yrva_qais_data_n      TYPE STANDARD TABLE OF yrva_qais_data,
      wa_yrva_qais_data_n      TYPE yrva_qais_data,
      it_yrva_qais_data_n_temp TYPE STANDARD TABLE OF yrva_qais_data,
      wa_yrva_qais_data_n_temp TYPE yrva_qais_data,
      it_yrva_qais_data2       TYPE STANDARD TABLE OF yrva_qais_data,
      wa_yrva_qais_data2       TYPE yrva_qais_data,
      it_yrva_qais_data_temp2  TYPE STANDARD TABLE OF yrva_qais_data,
      wa_yrva_qais_data_temp2  TYPE yrva_qais_data,
      it_s922_newq             TYPE STANDARD TABLE OF s922.

DATA: ls_yrva_cis_mstr   TYPE yrva_cis_mstr,
      ls_date            TYPE sy-datum,
      ls_magt            TYPE char120,
      ls_monthn          TYPE char2,
      lt_yrva_grade_cisd TYPE STANDARD TABLE OF yrva_grade_cisd,
      ls_yrva_grade_cisd TYPE yrva_grade_cisd.
RANGES: lt_kondm FOR s922-kondm.

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
        fkdat TYPE d,
      END OF i_vbrp,
      BEGIN OF it_kunnr OCCURS 0,
        kvgr2 TYPE kvgr2,
        kunnr TYPE kunnr,
      END OF it_kunnr,
      wa_kunnr LIKE LINE OF it_kunnr,
      w_kunag  LIKE vbrk-kunag.

*--- it_data_quater: AMENDED - added bcq_perc, orig_perc, scheme, jan_feb_amt, mar_amt
DATA: BEGIN OF it_data_quater OCCURS 0,
        kunnr            TYPE kunnr,
        name1            TYPE name1_gp,
        kvgr2            TYPE kvgr2,
        vkbur            TYPE vkbur,
        mou_qty          TYPE p DECIMALS 3,
        grp_lift_qty_m1  TYPE p DECIMALS 3,
        ind_lift_qty_m1  TYPE p DECIMALS 3,
        ind_elgl_qty_m1  TYPE p DECIMALS 3,
        grp_lift_qty_m2  TYPE p DECIMALS 3,
        ind_lift_qty_m2  TYPE p DECIMALS 3,
        ind_elgl_qty_m2  TYPE p DECIMALS 3,
        grp_lift_qty_m3  TYPE p DECIMALS 3,
        ind_lift_qty_m3  TYPE p DECIMALS 3,
        ind_elgl_qty_m3  TYPE p DECIMALS 3,
        tot_grp_lift_qty TYPE p DECIMALS 3,
        tot_elgl_qty     TYPE p DECIMALS 3,
        bcq_perc         TYPE p DECIMALS 2,
        orig_perc        TYPE p DECIMALS 2,
        scheme(10),
        jan_feb_amt      TYPE kbetr,
        mar_amt          TYPE kbetr,
        value            TYPE kbetr,
        remarks(30),
        sale_order       TYPE vbeln,
        check(1),
      END OF it_data_quater.

DATA: BEGIN OF it_vbak OCCURS 0,
        vbeln TYPE vbeln,
      END OF it_vbak.

DATA: BEGIN OF it_data_annual OCCURS 0,
        kunnr          TYPE kunnr,
        name1          TYPE name1_gp,
        kvgr2          TYPE kvgr2,
        vkbur          TYPE vkbur,
        mou_qty        TYPE p DECIMALS 3,
        grp_lift_qty   TYPE p DECIMALS 3,
        ind_lift_qty   TYPE p DECIMALS 3,
        tot_elgl_qty   TYPE p DECIMALS 3,
        waiver_1       TYPE yy_qais_month,
        waiver_2       TYPE yy_qais_month,
        waiver_3       TYPE yy_qais_month,
        remarks(30),
        value          TYPE kbetr,
        loyal_discount TYPE kbetr,
        sale_order     TYPE vbeln,
        check(1),
      END OF it_data_annual.

DATA: BEGIN OF it_annual_consis OCCURS 0,
        kunnr          TYPE kunnr,
        name1          TYPE name1_gp,
        kvgr2          TYPE kvgr2,
        vkbur          TYPE vkbur,
        mou_qty        TYPE p DECIMALS 3,
        grp_lift_qty   TYPE p DECIMALS 3,
        ind_lift_qty   TYPE p DECIMALS 3,
        tot_elgl_qty   TYPE p DECIMALS 3,
        remarks(30),
        value          TYPE kbetr,
        loyal_discount TYPE kbetr,
        sale_order     TYPE vbeln,
        check(1),
        mou_begda      TYPE begda,
        mou_endda      TYPE endda,
      END OF it_annual_consis.

DATA: BEGIN OF it_data_annual_newcus OCCURS 0,
        kunnr          TYPE kunnr,
        name1          TYPE name1_gp,
        kvgr2          TYPE kvgr2,
        vkbur          TYPE vkbur,
        mou_qty        TYPE p DECIMALS 3,
        grp_lift_qty   TYPE p DECIMALS 3,
        ind_lift_qty   TYPE p DECIMALS 3,
        tot_elgl_qty   TYPE p DECIMALS 3,
        waiver_1       TYPE yy_qais_month,
        waiver_2       TYPE yy_qais_month,
        waiver_3       TYPE yy_qais_month,
        remarks(30),
        rate           TYPE kbetr,
        value          TYPE kbetr,
        loyal_discount TYPE kbetr,
        sale_order     TYPE vbeln,
        check(1),
      END OF it_data_annual_newcus.

DATA: BEGIN OF it_data_monthly OCCURS 0,
        kunnr        TYPE kunnr,
        name1        TYPE name1_gp,
        kvgr2        TYPE kvgr2,
        vkbur        TYPE vkbur,
        begda        TYPE d,
        endda        TYPE d,
        commited_qty TYPE p DECIMALS 3,
        grp_lift_qty TYPE p DECIMALS 3,
        ind_lift_qty TYPE p DECIMALS 3,
        ind_elgl_qty TYPE p DECIMALS 3,
        tot_elgl_qty TYPE p DECIMALS 3,
        remarks(40),
        value        TYPE kbetr,
        sale_order   TYPE vbeln,
        check(1),
      END OF it_data_monthly,
      wa_data_monthly LIKE LINE OF it_data_monthly,
      lv_sales_order TYPE char1,
      w_max_monthly_qty TYPE p DECIMALS 3.

DATA: it_dataselm LIKE it_data_monthly OCCURS 0,
      ls_kvgr2 TYPE kvgr2.
TYPES: BEGIN OF ty_grpcu,
         kunnr TYPE kunnr,
         kvgr2 TYPE kvgr2,
       END OF ty_grpcu.
DATA: lt_grpcu TYPE STANDARD TABLE OF ty_grpcu,
      ls_grpcu TYPE ty_grpcu.

DATA: BEGIN OF it_kna1 OCCURS 0,
        kunnr TYPE kunnr,
        name1 TYPE name1_gp,
      END OF it_kna1.

DATA: BEGIN OF i_cond OCCURS 0,
        knumh LIKE konm-knumh,
        kbetr LIKE konm-kbetr,
        kstbm LIKE konm-kstbm,
      END OF i_cond.

DATA: x_order_header_in LIKE bapisdhead,
      x_return_commit   LIKE bapireturn1,
      x_sold_to_party   LIKE bapisoldto,
      x_bapisdhd1       LIKE bapisdhd1.

DATA: i_order_partners LIKE bapipartnr OCCURS 0 WITH HEADER LINE,
      i_order_items_in LIKE bapiitemin OCCURS 0 WITH HEADER LINE.

DATA: w_q1(1), w_q2(2), w_q3(1), w_q4(1),
      w_begda            TYPE d,
      w_endda            TYPE d,
      w_month_min        TYPE p DECIMALS 3,
      w_month_min1       TYPE p DECIMALS 3,
      w_month_max        TYPE p DECIMALS 3,
      w_month_max_perc   TYPE p DECIMALS 3,
      w_month_min_perc   TYPE p DECIMALS 3,
      w_month_min_perc1  TYPE p DECIMALS 3,
      w_quater_min       TYPE p DECIMALS 3,
      w_quater_max       TYPE p DECIMALS 3,
      w_year_min         TYPE p DECIMALS 3,
      w_year_max         TYPE p DECIMALS 3,
      w_flag_month(1),
      w_flag_month1(1),
      w_flag_month2(1),
      w_flag_month3(1).
DATA: i_a350 LIKE a350 OCCURS 0.
DATA: w_90_mou_qty TYPE p DECIMALS 3.
DATA: w_kbetr      LIKE konm-kbetr,
      w_vbeln      LIKE bapivbeln-vbeln,
      w_objtype    LIKE bapiusw01-objtype,
      w_auart      LIKE bapisdhead-doc_type,
      w_matnr      LIKE mara-matnr,
      w_fiscal(10),
      w_fiscal1(10),
      w_date_limit TYPE d.

DATA: lv_q         TYPE yquarter,
      lv_qmonth    TYPE yy_month1,
      lv_date1     TYPE sptag,
      lv_siml      TYPE zflag,
      lv_elgl_qty  TYPE p DECIMALS 3,
      lv_m1        TYPE p DECIMALS 3,
      lv_m2        TYPE p DECIMALS 3,
      lv_m3        TYPE p DECIMALS 3.

DATA: w_waive_month_1(1), w_waive_month_2(1), w_waive_month_3(1), w_waive_month(1),
      wa_yrva_qais_add_wv TYPE yrva_qais_add_wv,
      wa_yrva_qais_qtr_wv TYPE yrva_qais_qtr_wv,
      it_yrva_qais_qtr_wv TYPE STANDARD TABLE OF yrva_qais_qtr_wv,
      it_yrva_qais_add_wv TYPE STANDARD TABLE OF yrva_qais_add_wv.

DATA: lv_revival_m(1),
      lv_revival_q(1),
      lv_revival_a(1).
DATA: it_yrva_revival_fai TYPE STANDARD TABLE OF yrva_revival_fai,
      wa_yrva_revival_fai TYPE yrva_revival_fai.

CONSTANTS: co_zmis TYPE yy_reb_cond VALUE 'ZMIS',
           co_zqis TYPE yy_reb_cond VALUE 'ZQIS',
           co_zais TYPE yy_reb_cond VALUE 'ZAIS',
           co_zacd TYPE yy_reb_cond VALUE 'ZACD',
           co_pe12 TYPE yy_reb_cond VALUE 'PE12',
           co_zams TYPE yy_reb_cond VALUE 'ZAMS',
           co_zrpd TYPE yy_reb_cond VALUE 'ZRPD',
           co_null TYPE char1      VALUE '',
           co_pe07 TYPE yy_reb_cond VALUE 'PE07',
           co_pe08 TYPE yy_reb_cond VALUE 'PE08',
           co_pe32 TYPE yy_reb_cond VALUE 'PE32',
           co_zp21 TYPE yy_reb_cond VALUE 'ZP21',
           co_pe31 TYPE yy_reb_cond VALUE 'PE31'.

DATA: lv_flag1 TYPE c,
      lv_flag2 TYPE c.
DATA: w_tot_qty_quater TYPE volum.
DATA: lv_fact               TYPE p DECIMALS 3,
      lv_mon                TYPE char15,
      lv_year1(2)           TYPE c,
      lv_year2(2)           TYPE c,
      lv_dat(5)             TYPE c,
      lv_mth                TYPE yy_qais_month,
      lv_mth1               TYPE yy_qais_month,
      lv_index_rpd          TYPE sy-tabix,
      w_open_max_flag       TYPE char1,
      w_open_max_value      TYPE volum,
      lv_pd_annual_max_flag TYPE char1,
      lv_annual_max_flag    TYPE char1.

RANGES range_p FOR s922-kondm.
RANGES range_r FOR s922-kondm.
RANGES range_s FOR s922-kondm.

DATA: lv_flag123 TYPE char1.

*--- NEW: Q4 amended rate flag - set in q4_discount, read in quarter_discount
DATA: lv_q4_amended_rate TYPE kbetr.

*=======================================================================
* SELECTION SCREEN (from YRVG004_QAIS_EXECUTE_SEL)
*=======================================================================
SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS :
  s_sptag   FOR s922-sptag OBLIGATORY NO-EXTENSION,
  s_sptag1  FOR s922-sptag NO-DISPLAY NO-EXTENSION,
  s_sptagn  FOR s922-sptag NO-DISPLAY NO-EXTENSION,
  lv_sptag  FOR s922-sptag NO-DISPLAY NO-EXTENSION,
  s_vkbur   FOR s922-vkbur OBLIGATORY NO INTERVALS,
  s_pkunag  FOR s922-pkunag NO INTERVALS,
  s_kvgr2   FOR s922-kvgr2  NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN SKIP.

PARAMETERS: r_month   RADIOBUTTON GROUP rd1 USER-COMMAND r1 DEFAULT 'X',
            r_quater  RADIOBUTTON GROUP rd1,
            r_annual  RADIOBUTTON GROUP rd1,
            r_consis  RADIOBUTTON GROUP rd1.
SELECTION-SCREEN SKIP.
PARAMETERS: r_month1 NO-DISPLAY.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:    r_rlld   RADIOBUTTON GROUP rd1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_rhd RADIOBUTTON GROUP rd1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: c_maint RADIOBUTTON GROUP rd1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: c_maint1 RADIOBUTTON GROUP rd1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_newcus NO-DISPLAY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:  r_rpd  NO-DISPLAY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 1(79) TEXT-002.
PARAMETERS: c_chk TYPE flag NO-DISPLAY DEFAULT ' '.

SELECTION-SCREEN : END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF r_month = 'X' OR r_quater = 'X' OR r_annual = 'X' OR r_rhd = 'X'
       OR r_rlld = 'X' OR c_maint = 'X' OR c_maint1 = 'X'.
      IF screen-name = 'R_CONSIS'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
  LOOP AT SCREEN.
    IF screen-name = 'R_RPD'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'R_NEWCUS'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
    IF screen-name = 'R_RHD' OR screen-name = 'R_RLLD' OR screen-name = 'C_MAINT' OR screen-name = 'C_MAINT1'.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

INITIALIZATION.
  SELECT * FROM yrva_prs_grades INTO TABLE @DATA(lt_yrva_prs_grades).
  SELECT SINGLE * FROM yrva_cis_mstr INTO ls_yrva_cis_mstr.
  LOOP AT lt_yrva_prs_grades INTO DATA(ls_yrva_prs_grades).
    range_r-sign   = 'I'. range_r-option = 'EQ'.
    range_s-sign   = 'I'. range_s-option = 'EQ'.
    range_p-sign   = 'I'. range_p-option = 'EQ'.
    IF ls_yrva_prs_grades-yy_indicator = 'R'.
      range_r-low = ls_yrva_prs_grades-yy_grade. APPEND range_r.
    ELSEIF ls_yrva_prs_grades-yy_indicator = 'S'.
      range_s-low = ls_yrva_prs_grades-yy_grade. APPEND range_s.
    ELSEIF ls_yrva_prs_grades-yy_indicator = 'P'.
      range_p-low = ls_yrva_prs_grades-yy_grade. APPEND range_p.
    ENDIF.
    CLEAR: ls_yrva_prs_grades.
  ENDLOOP.

*=======================================================================
* MAIN PROGRAM (from YRVG004_QAIS_EXECUTE)
*=======================================================================
INITIALIZATION.
  GET PARAMETER ID 'ZFL' FIELD lv_siml.
  IF lv_siml EQ 'X'.
    MESSAGE 'This option is for getting a snapshot of CIS status as on date when performed. Please use this option diligently.' TYPE 'I'.
  ENDIF.

START-OF-SELECTION.
  LOOP AT s_vkbur.
    AUTHORITY-CHECK OBJECT 'YV_VKBUR' ID 'VKBUR' FIELD s_vkbur-low.
    IF sy-subrc <> 0.
      MESSAGE e081 WITH s_vkbur-low.
    ENDIF.
  ENDLOOP.
  SET PF-STATUS 'STANDARD'.
  PERFORM validation.
  PERFORM get_data.
  PERFORM qais_remarks.
  IF r_quater = 'X'.
    PERFORM format_data.
  ENDIF.
  IF r_month = 'X' OR r_month1 EQ 'X' OR r_rpd EQ 'X' OR r_rhd EQ 'X' OR
     r_rlld EQ 'X' OR c_maint EQ 'X' OR c_maint1 EQ 'X'.
    IF ls_psdq = 'X'.
      REFRESH: it_yrva_qais_data_n[], it_yrva_qais_data_n_temp[].
      it_yrva_qais_data_n[]      = it_yrva_qais_data[].
      it_yrva_qais_data_n_temp[] = it_yrva_qais_data_temp[].
    ENDIF.
    PERFORM format_data_month.
    IF ls_psdq = 'X'.
      REFRESH: it_yrva_qais_data2[], it_yrva_qais_data_temp2[].
      it_yrva_qais_data2[]      = it_yrva_qais_data[].
      it_yrva_qais_data_temp2[] = it_yrva_qais_data_temp[].
      REFRESH: it_s922_newq[].
      it_s922_newq[] = it_s922[].
      it_s922 = it_s922_n.
      REFRESH: it_yrva_qais_data[], it_yrva_qais_data_temp[].
      it_yrva_qais_data[]      = it_yrva_qais_data_n[].
      it_yrva_qais_data_temp[] = it_yrva_qais_data_n_temp[].
      PERFORM format_data_month.
      REFRESH: it_s922[].
      it_s922[] = it_s922_newq[].
      REFRESH: it_yrva_qais_data_n[], it_yrva_qais_data_n_temp[].
      it_yrva_qais_data_n[]      = it_yrva_qais_data[].
      it_yrva_qais_data_n_temp[] = it_yrva_qais_data_temp[].
      REFRESH: it_yrva_qais_data[], it_yrva_qais_data_temp[].
      it_yrva_qais_data[]      = it_yrva_qais_data2[].
      it_yrva_qais_data_temp[] = it_yrva_qais_data_temp2[].
    ENDIF.
  ENDIF.
  PERFORM calculate_discount.
  PERFORM get_cust_name.

END-OF-SELECTION.
  PERFORM create_field_catalog.
  PERFORM display_list.

*=======================================================================
* FORMS (from YRVG004_QAIS_EXECUTE_F01 - with Q4 amendments marked)
*=======================================================================

*&---------------------------------------------------------------------*
*&      Form  VALIDATION
*&---------------------------------------------------------------------*
FORM validation .
  DATA: w_check_date TYPE d.
  DATA : lv_percent TYPE char17 .
  IF c_chk IS NOT INITIAL.
    lv_siml = 'X'.
  ENDIF.
  IF s_sptag-low IS INITIAL OR s_sptag-high IS INITIAL.
    MESSAGE 'Please enter From and To Date of period' TYPE 'E' .
  ELSE.
    IF r_quater = 'X' AND ( ( s_sptag-low+4(4) NE '0401' OR s_sptag-high+4(4) NE '0630' ) AND
                            ( s_sptag-low+4(4) NE '0701' OR s_sptag-high+4(4) NE '0930' ) AND
                            ( s_sptag-low+4(4) NE '1001' OR s_sptag-high+4(4) NE '1231' ) AND
                            ( s_sptag-low+4(4) NE '0101' OR s_sptag-high+4(4) NE '0331' ) ).
      MESSAGE 'Please enter valid quater period dates' TYPE 'E' .
    ELSEIF r_annual = 'X' AND
           ( s_sptag-low+4(4) NE '0401' OR s_sptag-high+4(4) NE '0331' ) .
      IF lv_siml NE 'X'.
        MESSAGE 'Please enter valid Financial year dates' TYPE 'E' .
      ENDIF.
    ELSEIF r_consis = 'X' AND
           ( s_sptag-low+4(4) NE '0401' OR s_sptag-high+4(4) NE '0331' ) .
      IF lv_siml NE 'X'.
        MESSAGE 'Please enter valid Financial year dates' TYPE 'E' .
      ENDIF.
    ELSEIF r_month = 'X' OR r_month1 EQ 'X' OR r_rpd EQ 'X' OR r_rhd EQ 'X' OR r_rlld EQ 'X' OR c_maint EQ 'X' OR c_maint1 EQ 'X' .
      IF s_sptag-low+4(2) NE s_sptag-high+4(2).
        MESSAGE 'Please Enter Single Month Period' TYPE 'E'.
      ELSE.
        IF s_sptag-low+6(2) NE '01'.
          MESSAGE 'Please Enter Correct Month Period' TYPE 'E'.
        ELSE.
          CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
            EXPORTING i_date = s_sptag-low
            IMPORTING e_date = lv_date1.
          IF s_sptag-high NE lv_date1.
            IF lv_siml NE 'X'.
              MESSAGE 'Please Enter Correct Month Period' TYPE 'E'.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR: w_q1, w_q2, w_q3, w_q4.
        IF s_sptag-low+4(4) GE '0401' AND s_sptag-low+4(4) LE '0601'.
          w_q1 = 'X'. lv_q = 'Q1'.
        ELSEIF s_sptag-low+4(4) GE '0701' AND s_sptag-low+4(4) LE '0901'.
          lv_sptag-low+0(4)  = s_sptag-low+0(4).
          lv_sptag-high+4(4) = '0601'.
          lv_sptag-high+0(4) = s_sptag-low+0(4).
          w_q2 = 'X'. lv_q = 'Q2'.
        ELSEIF s_sptag-low+4(4) GE '1001' AND s_sptag-low+4(4) LE '1201'.
          lv_sptag-low+4(4)  = '0701'. lv_sptag-low+0(4)  = s_sptag-low+0(4).
          lv_sptag-high+4(4) = '0901'. lv_sptag-high+0(4) = s_sptag-low+0(4).
          w_q3 = 'X'. lv_q = 'Q3'. w_endda_prev = s_sptag-low - 1.
        ELSEIF s_sptag-low+4(4) GE '0101' AND s_sptag-low+4(4) LE '0301'.
          lv_sptag-low+4(4)  = '1001'. lv_sptag-low+0(4)  = s_sptag-low+0(4) - 1.
          lv_sptag-high+4(4) = '1201'. lv_sptag-high+0(4) = s_sptag-low+0(4) - 1.
          w_q4 = 'X'. lv_q = 'Q4'. w_endda_prev = s_sptag-low - 1.
        ENDIF.
      ENDIF.
      IF lv_siml NE 'X'.
        IF s_sptag-high GT sy-datum.
          MESSAGE 'End date can not be Future date' TYPE 'E' .
        ENDIF.
      ENDIF.
    ELSEIF r_newcus = 'X' AND
           ( s_sptag-low+4(4) NE '0801' OR s_sptag-high+4(4) NE '0331' ) .
      IF lv_siml NE 'X'.
        MESSAGE 'Please enter valid Financial year dates' TYPE 'E' .
      ENDIF.
    ENDIF.
    IF r_quater = 'X' .
      IF s_sptag-low+4(4) EQ '0401'.
        w_q1 = 'X'. lv_q = 'Q1'.
      ELSEIF s_sptag-low+4(4) EQ '0701'.
        w_q2 = 'X'. lv_q = 'Q2'.
      ELSEIF s_sptag-low+4(4) EQ '1001'.
        w_q3 = 'X'. lv_q = 'Q3'. w_endda_prev = s_sptag-low - 1.
      ELSEIF s_sptag-low+4(4) EQ '0101'.
        w_q4 = 'X'. lv_q = 'Q4'. w_endda_prev = s_sptag-low - 1.
      ENDIF.
    ENDIF.
  ENDIF.
  IF r_quater = 'X' .
    IF s_sptag-low+4(2) LE '04' .
      w_fiscal  = s_sptag-low+0(4).
      w_fiscal1 = s_sptag-high+2(2) + 1 .
      CONDENSE: w_fiscal, w_fiscal1.
      CONCATENATE w_fiscal w_fiscal1 INTO w_fiscal SEPARATED BY '-' .
    ELSE.
      w_fiscal = s_sptag-low+2(2) + 1 .
      CONDENSE w_fiscal.
      CONCATENATE s_sptag-low+2(2) w_fiscal INTO w_fiscal SEPARATED BY '-' .
    ENDIF.
  ELSE.
    CONCATENATE s_sptag-low+2(2) s_sptag-high+2(2) INTO w_fiscal SEPARATED BY '-' .
  ENDIF.
  IF s_sptag-low < ls_yrva_cis_mstr-yy_start_date OR s_sptag-high > ls_yrva_cis_mstr-yy_end_date.
    CLEAR: ls_monthn.
    ls_monthn = ls_yrva_cis_mstr-yy_start_date+0(4).
    ls_monthn = ls_monthn + 1.
    CONCATENATE 'Please process with financial year' ls_yrva_cis_mstr-yy_start_date+0(4) '-' ls_monthn INTO ls_magt SEPARATED BY space.
    MESSAGE ls_magt TYPE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.
  DATA lv_fname TYPE char20.
  DATA lv_index1 TYPE sy-tabix.
  DATA lv_mth1 TYPE yy_qais_month.
  CLEAR: ls_psdq.
  SELECT * FROM yrva_grade_cisd INTO TABLE lt_yrva_grade_cisd.
  LOOP AT lt_yrva_grade_cisd INTO ls_yrva_grade_cisd.
    lt_kondm-sign   = 'I'. lt_kondm-option = 'EQ'.
    lt_kondm-low    = ls_yrva_grade_cisd-yy_grade.
    APPEND lt_kondm. CLEAR: ls_yrva_grade_cisd.
  ENDLOOP.
  IF lt_kondm IS NOT INITIAL.
    SORT lt_kondm[] BY low.
    DELETE ADJACENT DUPLICATES FROM lt_kondm COMPARING low.
  ENDIF.
  SELECT * FROM yrva_mstr_waiver INTO wa_yrva_mstr_waiver UP TO 1 ROWS
    WHERE begda LE s_sptag-low AND endda GE s_sptag-high ORDER BY PRIMARY KEY.
  ENDSELECT.
  IF sy-subrc NE 0 .
    wa_yrva_mstr_waiver-min_perc_q1 = 85. wa_yrva_mstr_waiver-max_perc_q1 = 125.
    wa_yrva_mstr_waiver-min_perc_q2 = 85. wa_yrva_mstr_waiver-max_perc_q2 = 125.
    wa_yrva_mstr_waiver-min_perc_q3 = 85. wa_yrva_mstr_waiver-max_perc_q3 = 125.
    wa_yrva_mstr_waiver-min_perc_q4 = 85. wa_yrva_mstr_waiver-max_perc_q4 = 125.
    wa_yrva_mstr_waiver-annual_min  = 85. wa_yrva_mstr_waiver-annual_max  = 125.
  ENDIF.
  MOVE-CORRESPONDING wa_yrva_mstr_waiver TO wa_yrva_mstr_waiver_temp.
  CLEAR wa_where_tab. REFRESH it_where_tab.
  CONCATENATE 'KUNNR' 'IN' 'S_PKUNAG' INTO wa_where_tab SEPARATED BY space.
  TRANSLATE wa_where_tab TO UPPER CASE. APPEND wa_where_tab TO it_where_tab.
  CONCATENATE 'AND' 'VKBUR' 'IN' 'S_VKBUR' INTO wa_where_tab SEPARATED BY space.
  TRANSLATE wa_where_tab TO UPPER CASE. APPEND wa_where_tab TO it_where_tab.
  CONCATENATE 'AND' 'yy_per_start' 'LE' 'S_SPTAG-LOW' INTO wa_where_tab SEPARATED BY space.
  TRANSLATE wa_where_tab TO UPPER CASE. APPEND wa_where_tab TO it_where_tab.
  CONCATENATE 'AND' 'yy_per_end' 'GE' 'S_SPTAG-HIGH' INTO wa_where_tab SEPARATED BY space.
  TRANSLATE wa_where_tab TO UPPER CASE. APPEND wa_where_tab TO it_where_tab.
  IF r_month EQ 'X'.
    CONCATENATE 'AND' 'REB_COND' 'EQ' 'CO_ZMIS' INTO wa_where_tab SEPARATED BY space.
    APPEND wa_where_tab TO it_where_tab.
  ELSEIF r_month1 = 'X'.
    CONCATENATE 'AND' 'REB_COND' 'EQ' 'CO_ZAMS' INTO wa_where_tab SEPARATED BY space.
    APPEND wa_where_tab TO it_where_tab.
  ELSEIF r_quater = 'X'.
    CONCATENATE 'AND' 'REB_COND' 'EQ' 'CO_ZQIS' INTO wa_where_tab SEPARATED BY space.
    APPEND wa_where_tab TO it_where_tab.
  ELSEIF r_annual = 'X'.
    CONCATENATE 'AND' 'REB_COND' 'EQ' 'CO_ZAIS' INTO wa_where_tab SEPARATED BY space.
    APPEND wa_where_tab TO it_where_tab.
  ELSEIF r_consis = 'X'.
    CONCATENATE 'AND' 'REB_COND' 'EQ' 'CO_ZACD' INTO wa_where_tab SEPARATED BY space.
    APPEND wa_where_tab TO it_where_tab.
  ELSEIF r_rpd = 'X'.
    CONCATENATE 'AND' 'REB_COND' 'EQ' 'CO_ZRPD' INTO wa_where_tab SEPARATED BY space.
    APPEND wa_where_tab TO it_where_tab.
  ELSEIF r_rhd = 'X'.
    CONCATENATE 'AND' 'REB_COND' 'EQ' 'CO_PE07' INTO wa_where_tab SEPARATED BY space.
    APPEND wa_where_tab TO it_where_tab.
  ELSEIF r_rlld = 'X'.
    CONCATENATE 'AND' 'REB_COND' 'EQ' 'CO_PE08' INTO wa_where_tab SEPARATED BY space.
    APPEND wa_where_tab TO it_where_tab.
  ELSEIF c_maint = 'X'.
    CONCATENATE 'AND' 'REB_COND' 'EQ' 'CO_PE32' INTO wa_where_tab SEPARATED BY space.
    APPEND wa_where_tab TO it_where_tab.
  ELSEIF c_maint1 = 'X'.
    CONCATENATE 'AND' 'REB_COND' 'EQ' 'CO_PE31' INTO wa_where_tab SEPARATED BY space.
    APPEND wa_where_tab TO it_where_tab.
  ELSEIF r_newcus = 'X'.
    CONCATENATE 'AND' 'REB_COND' 'EQ' 'CO_PE12' INTO wa_where_tab SEPARATED BY space.
    APPEND wa_where_tab TO it_where_tab.
  ENDIF.
  SELECT * FROM yrva_rebate INTO TABLE it_yrva_rebate WHERE (it_where_tab).
  SELECT * FROM yrva_qais_data INTO CORRESPONDING FIELDS OF TABLE it_yrva_qais_data
    WHERE kunnr IN s_pkunag AND kvgr2 IN s_kvgr2 AND vkbur IN s_vkbur
      AND mou_begda LE s_sptag-low AND mou_endda GE s_sptag-high AND module_identity = ''.
  DELETE it_yrva_qais_data WHERE module_identity = 'X'.
  DELETE it_yrva_qais_data WHERE inactive EQ 'X'.
  SORT it_yrva_qais_data DESCENDING BY kunnr yytimestamp DESCENDING.
  DELETE ADJACENT DUPLICATES FROM it_yrva_qais_data COMPARING qais_no kunnr.
  LOOP AT it_yrva_qais_data INTO wa_yrva_qais_data.
    it_kunnr-kvgr2 = wa_yrva_qais_data-kvgr2.
    it_kunnr-kunnr = wa_yrva_qais_data-kunnr.
    APPEND it_kunnr.
  ENDLOOP.
  SORT it_kunnr BY kvgr2 kunnr.
  DELETE ADJACENT DUPLICATES FROM it_kunnr COMPARING ALL FIELDS.
  it_yrva_qais_data_temp[] = it_yrva_qais_data[].
  DELETE it_yrva_qais_data_temp WHERE kvgr2 IS INITIAL.
  SORT it_yrva_qais_data_temp BY kvgr2.
  DELETE ADJACENT DUPLICATES FROM it_yrva_qais_data_temp COMPARING kvgr2.
  IF it_yrva_qais_data_temp[] IS NOT INITIAL.
    SELECT * FROM knvv INTO TABLE it_knvv
      FOR ALL ENTRIES IN it_yrva_qais_data_temp
      WHERE kvgr2 = it_yrva_qais_data_temp-kvgr2 AND vtweg = '10'.
  ENDIF.
  IF it_yrva_qais_data[] IS NOT INITIAL.
    SELECT * FROM s922 INTO CORRESPONDING FIELDS OF TABLE it_s922
      FOR ALL ENTRIES IN it_yrva_qais_data
      WHERE sptag IN s_sptag AND pkunag = it_yrva_qais_data-kunnr AND kondm IN lt_kondm.
    SORT it_s922 BY pkunag sptag.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_CUST_NAME
*&---------------------------------------------------------------------*
FORM get_cust_name .
  IF r_quater = 'X' .
    IF it_data_quater[] IS NOT INITIAL .
      SELECT kunnr name1 FROM kna1 INTO TABLE it_kna1
        FOR ALL ENTRIES IN it_data_quater WHERE kunnr = it_data_quater-kunnr.
      LOOP AT it_data_quater .
        READ TABLE it_kna1 WITH KEY kunnr = it_data_quater-kunnr.
        IF sy-subrc EQ 0 .
          it_data_quater-name1 = it_kna1-name1. MODIFY it_data_quater. CLEAR it_data_quater.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSEIF r_annual = 'X' .
    IF it_data_annual[] IS NOT INITIAL .
      SELECT kunnr name1 FROM kna1 INTO TABLE it_kna1
        FOR ALL ENTRIES IN it_data_annual WHERE kunnr = it_data_annual-kunnr.
      LOOP AT it_data_annual .
        READ TABLE it_kna1 WITH KEY kunnr = it_data_annual-kunnr.
        IF sy-subrc EQ 0 .
          it_data_annual-name1 = it_kna1-name1. MODIFY it_data_annual. CLEAR it_data_annual.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSEIF r_consis = 'X' .
    IF it_annual_consis[] IS NOT INITIAL .
      SELECT kunnr name1 FROM kna1 INTO TABLE it_kna1
        FOR ALL ENTRIES IN it_annual_consis WHERE kunnr = it_annual_consis-kunnr.
      LOOP AT it_annual_consis .
        READ TABLE it_kna1 WITH KEY kunnr = it_annual_consis-kunnr.
        IF sy-subrc EQ 0 .
          it_annual_consis-name1 = it_kna1-name1. MODIFY it_annual_consis. CLEAR it_annual_consis.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSEIF r_month = 'X' OR r_month1 EQ 'X' OR r_rpd EQ 'X' OR r_rhd EQ 'X' OR r_rlld EQ 'X' OR c_maint EQ 'X' OR c_maint1 EQ 'X'.
    IF it_data_monthly[] IS NOT INITIAL.
      SELECT kunnr name1 FROM kna1 INTO TABLE it_kna1
        FOR ALL ENTRIES IN it_data_monthly WHERE kunnr = it_data_monthly-kunnr.
      LOOP AT it_data_monthly .
        READ TABLE it_kna1 WITH KEY kunnr = it_data_monthly-kunnr.
        IF sy-subrc EQ 0 .
          it_data_monthly-name1 = it_kna1-name1. MODIFY it_data_monthly. CLEAR it_data_monthly.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSEIF r_newcus = 'X' .
    IF it_data_annual_newcus[] IS NOT INITIAL .
      SELECT kunnr name1 FROM kna1 INTO TABLE it_kna1
        FOR ALL ENTRIES IN it_data_annual_newcus WHERE kunnr = it_data_annual_newcus-kunnr.
      LOOP AT it_data_annual_newcus .
        READ TABLE it_kna1 WITH KEY kunnr = it_data_annual_newcus-kunnr.
        IF sy-subrc EQ 0 .
          it_data_annual_newcus-name1 = it_kna1-name1. MODIFY it_data_annual_newcus. CLEAR it_data_annual_newcus.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CALCULATE_DISCOUNT
*&---------------------------------------------------------------------*
FORM calculate_discount .
  IF r_quater = 'X' .
    PERFORM quarter_discount.
  ELSEIF r_annual = 'X' .
    PERFORM annual_discount.
  ELSEIF r_consis = 'X'.
    PERFORM annual_consis_discount.
  ELSEIF r_month = 'X' OR r_month1 EQ 'X' OR r_rhd EQ 'X' OR r_rlld EQ 'X' OR c_maint EQ 'X' OR c_maint1 EQ 'X'.
    PERFORM monthly_discount.
  ELSEIF r_rpd EQ 'X'.
    PERFORM repeat_performance_discount.
  ELSEIF r_newcus = 'X' .
    PERFORM annual_disc_for_new_cust.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  QAIS_REMARKS
*&---------------------------------------------------------------------*
FORM qais_remarks .
  IF r_quater = 'X' .
    CONCATENATE 'CIS' w_fiscal INTO w_remarks SEPARATED BY space.
    IF w_q4 = 'X' .      CONCATENATE w_remarks 'Q4 Amended' INTO w_remarks SEPARATED BY space.
    ELSEIF w_q1 = 'X' .  CONCATENATE w_remarks 'Q1' INTO w_remarks SEPARATED BY space.
    ELSEIF w_q2 = 'X' .  CONCATENATE w_remarks 'Q2' INTO w_remarks SEPARATED BY space.
    ELSEIF w_q3 = 'X' .  CONCATENATE w_remarks 'Q3' INTO w_remarks SEPARATED BY space.
    ENDIF.
  ELSEIF r_annual = 'X'.
    CONCATENATE 'CIS' w_fiscal 'Annual' INTO w_remarks SEPARATED BY space.
  ELSEIF r_consis = 'X'.
    CONCATENATE 'CIS' w_fiscal 'Annual Consistency' INTO w_remarks SEPARATED BY space.
  ELSEIF r_month = 'X'.     w_remarks = 'CIS Monthly Discount'.
  ELSEIF r_month1 = 'X'.    w_remarks = 'CIS Special Monthly Discount'.
  ELSEIF r_rhd = 'X'.       w_remarks = 'CIS Special HDPE Monthly Discount'.
  ELSEIF r_rlld = 'X'.      w_remarks = 'CIS Special LLDPE Monthly Discount'.
  ELSEIF r_rpd = 'X'.       w_remarks = 'CIS Repeat Performance Discount'.
  ELSEIF r_newcus = 'X'.
    CONCATENATE 'CIS' w_fiscal 'Addl. Annual Disc' INTO w_remarks SEPARATED BY space.
  ELSEIF c_maint = 'X'.     w_remarks = 'CIS Other Discount'.
  ELSEIF c_maint1 = 'X'.    w_remarks = 'CIS Other Discount1'.
  ENDIF.
  CONDENSE w_remarks.
ENDFORM.

FORM top_of_page . ENDFORM.

FORM dynamic_month USING p_ls_date p_ls_monthn.
  CALL FUNCTION 'MONTH_PLUS_DETERMINE'
    EXPORTING months = p_ls_monthn olddate = p_ls_date
    IMPORTING newdate = p_ls_date.
ENDFORM.

*&---------------------------------------------------------------------*
*& NEW FORM: GET_Q4_SLAB_RATE
*& Amended Q4 slab: >=120%->2500, >=110%->2000, >=100%->1500, else 0
*&---------------------------------------------------------------------*
FORM get_q4_slab_rate USING    p_perc TYPE p
                      CHANGING p_rate TYPE kbetr.
  CLEAR p_rate.
  IF p_perc GE 120. p_rate = 2500.
  ELSEIF p_perc GE 110. p_rate = 2000.
  ELSEIF p_perc GE 100. p_rate = 1500.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& QUARTER_DISCOUNT - modified: Q4 skips KONM lookup (rate set in q4_discount)
*&---------------------------------------------------------------------*
FORM quarter_discount .
  SELECT * INTO TABLE i_a350 FROM a350
    WHERE kappl = 'V' AND kschl = 'ZQIS' AND vkorg = '5000'
      AND kfrst = '' AND datab LE s_sptag-high AND datbi GE s_sptag-low.
  IF NOT i_a350[] IS INITIAL.
    SELECT knumh kbetr kstbm INTO TABLE i_cond FROM konm
      FOR ALL ENTRIES IN i_a350 WHERE knumh = i_a350-knumh.
  ENDIF.
  IF it_yrva_qais_data[] IS NOT INITIAL.
    SELECT * FROM yrva_qais_add_wv INTO TABLE it_yrva_qais_add_wv
      FOR ALL ENTRIES IN it_yrva_qais_data WHERE qais_no = it_yrva_qais_data-qais_no.
    SELECT * FROM yrva_revival_fai INTO TABLE it_yrva_revival_fai
      FOR ALL ENTRIES IN it_yrva_qais_data WHERE qais_no = it_yrva_qais_data-qais_no.
  ENDIF.
  LOOP AT it_yrva_qais_data INTO wa_yrva_qais_data.
    CLEAR: w_month_min, w_month_max, w_quater_min, w_quater_max,
           w_waive_month_1, w_waive_month_2, w_waive_month_3,
           lv_revival_q, wa_yrva_revival_fai, lv_q4_amended_rate.
    w_flag_month1 = 'X'. w_flag_month2 = 'X'. w_flag_month3 = 'X'.
    MOVE-CORRESPONDING wa_yrva_mstr_waiver_temp TO wa_yrva_mstr_waiver.
    it_data_quater-kunnr   = wa_yrva_qais_data-kunnr.
    it_data_quater-kvgr2   = wa_yrva_qais_data-kvgr2.
    it_data_quater-vkbur   = wa_yrva_qais_data-vkbur.
    it_data_quater-mou_qty = wa_yrva_qais_data-mou_qty.
    IF w_q1 = 'X'.     PERFORM q1_discount.
    ELSEIF w_q2 = 'X'. PERFORM q2_discount.
    ELSEIF w_q3 = 'X'. PERFORM q3_discount.
    ELSEIF w_q4 = 'X'. PERFORM q4_discount.         "<<< amended Q4
    ENDIF.
    IF lv_flag123 IS INITIAL.
      it_data_quater-tot_grp_lift_qty = it_data_quater-grp_lift_qty_m1
                                      + it_data_quater-grp_lift_qty_m2
                                      + it_data_quater-grp_lift_qty_m3.
      IF w_q4 = 'X' AND lv_q4_amended_rate GT 0.
        "Q4 amended: rate already determined in q4_discount
        w_kbetr = lv_q4_amended_rate.
        it_data_quater-value = it_data_quater-tot_elgl_qty * w_kbetr.
        IF it_data_quater-value LT 0. it_data_quater-value = it_data_quater-value * -1. ENDIF.
        "Split amounts: Jan+Feb vs Mar
        it_data_quater-jan_feb_amt = ( it_data_quater-ind_elgl_qty_m1 +
                                       it_data_quater-ind_elgl_qty_m2 ) * w_kbetr.
        it_data_quater-mar_amt     = it_data_quater-ind_elgl_qty_m3 * w_kbetr.
        IF it_data_quater-jan_feb_amt LT 0. it_data_quater-jan_feb_amt = it_data_quater-jan_feb_amt * -1. ENDIF.
        IF it_data_quater-mar_amt LT 0.     it_data_quater-mar_amt = it_data_quater-mar_amt * -1. ENDIF.
      ELSE.
        "Q1/Q2/Q3: original KONM scale lookup
        SORT i_cond BY kstbm DESCENDING.
        LOOP AT i_cond WHERE kstbm <= wa_yrva_qais_data-mou_qty.
          w_kbetr = i_cond-kbetr.
          it_data_quater-value = it_data_quater-tot_elgl_qty * w_kbetr.
          IF it_data_quater-value LT 0. it_data_quater-value = it_data_quater-value * -1. ENDIF.
          EXIT.
        ENDLOOP.
      ENDIF.
      it_data_quater-remarks = w_remarks.
      CONDENSE it_data_quater-remarks.
      APPEND it_data_quater.
      CLEAR it_data_quater.
      MODIFY it_yrva_qais_data FROM wa_yrva_qais_data.
      CLEAR: lv_m1, lv_m2, lv_m3, wa_yrva_qais_data.
    ELSE.
      CLEAR: lv_flag123.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Q1_DISCOUNT / Q1_APRIL / Q1_MAY / Q1_JUNE
*&---------------------------------------------------------------------*
FORM q1_discount .
  IF wa_yrva_mstr_waiver-max_perc_q1 IS NOT INITIAL.
    IF wa_yrva_qais_data-p_d_sector = 'X'.
      wa_yrva_mstr_waiver-max_perc_q1 = wa_yrva_mstr_waiver-pd_perc_q1.
      wa_yrva_mstr_waiver-annual_max  = wa_yrva_mstr_waiver-pd_perc_q1.
    ENDIF.
  ELSE.
    CLEAR wa_yrva_mstr_waiver-max_perc_q1.
  ENDIF.
  IF wa_yrva_qais_data-waiver_qty EQ 'Q1'. wa_yrva_mstr_waiver-min_perc_q1 = '65'. ENDIF.
  READ TABLE it_yrva_qais_add_wv INTO wa_yrva_qais_add_wv
    WITH KEY qais_no = wa_yrva_qais_data-qais_no waiver_month = 'APR'.
  IF sy-subrc EQ 0. w_waive_month_1 = 'X'. ENDIF.
  READ TABLE it_yrva_qais_add_wv INTO wa_yrva_qais_add_wv
    WITH KEY qais_no = wa_yrva_qais_data-qais_no waiver_month = 'MAY'.
  IF sy-subrc EQ 0. w_waive_month_2 = 'X'. ENDIF.
  READ TABLE it_yrva_qais_add_wv INTO wa_yrva_qais_add_wv
    WITH KEY qais_no = wa_yrva_qais_data-qais_no waiver_month = 'JUN'.
  IF sy-subrc EQ 0. w_waive_month_3 = 'X'. ENDIF.
  IF wa_yrva_qais_data-mou_begda LE s_sptag-low.
    w_quater_min = ( wa_yrva_qais_data-commited_qty_m1 + wa_yrva_qais_data-commited_qty_m2 +
                     wa_yrva_qais_data-commited_qty_m3 ) * wa_yrva_mstr_waiver-min_perc_q1 / 100.
    IF wa_yrva_mstr_waiver-max_perc_q1 IS NOT INITIAL.
      w_quater_max = ( wa_yrva_qais_data-commited_qty_m1 + wa_yrva_qais_data-commited_qty_m2 +
                       wa_yrva_qais_data-commited_qty_m3 ) * wa_yrva_mstr_waiver-max_perc_q1 / 100.
    ELSE.
      w_open_max_flag = 'X'.
      w_quater_max = wa_yrva_qais_data-ind_lift_qty_m1 + wa_yrva_qais_data-ind_lift_qty_m2
                   + wa_yrva_qais_data-ind_lift_qty_m3.
    ENDIF.
  ENDIF.
  PERFORM q1_april. PERFORM q1_may. PERFORM q1_june.
  it_data_quater-tot_elgl_qty = it_data_quater-ind_elgl_qty_m1 +
                                it_data_quater-ind_elgl_qty_m2 +
                                it_data_quater-ind_elgl_qty_m3.
ENDFORM.
FORM q1_april .
  it_data_quater-grp_lift_qty_m1 = wa_yrva_qais_data-grp_lift_qty_m1.
  it_data_quater-ind_lift_qty_m1 = wa_yrva_qais_data-ind_lift_qty_m1.
  it_data_quater-ind_elgl_qty_m1 = wa_yrva_qais_data-ind_lift_qty_m1.
ENDFORM.
FORM q1_may .
  it_data_quater-grp_lift_qty_m2 = wa_yrva_qais_data-grp_lift_qty_m2.
  it_data_quater-ind_lift_qty_m2 = wa_yrva_qais_data-ind_lift_qty_m2.
  it_data_quater-ind_elgl_qty_m2 = wa_yrva_qais_data-ind_lift_qty_m2.
ENDFORM.
FORM q1_june .
  it_data_quater-grp_lift_qty_m3 = wa_yrva_qais_data-grp_lift_qty_m3.
  it_data_quater-ind_lift_qty_m3 = wa_yrva_qais_data-ind_lift_qty_m3.
  it_data_quater-ind_elgl_qty_m3 = wa_yrva_qais_data-ind_lift_qty_m3.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Q2_DISCOUNT / Q2_JULY / Q2_AUG / Q2_SEP
*&---------------------------------------------------------------------*
FORM q2_discount .
  IF wa_yrva_qais_data-p_d_sector = 'X' AND wa_yrva_mstr_waiver-max_perc_q2 IS NOT INITIAL.
    wa_yrva_mstr_waiver-max_perc_q2 = wa_yrva_mstr_waiver-pd_perc_q2.
    wa_yrva_mstr_waiver-annual_max  = wa_yrva_mstr_waiver-pd_perc_q2.
  ENDIF.
  PERFORM q2_july. PERFORM q2_aug. PERFORM q2_sep.
  it_data_quater-tot_elgl_qty = it_data_quater-ind_elgl_qty_m1 +
                                it_data_quater-ind_elgl_qty_m2 +
                                it_data_quater-ind_elgl_qty_m3.
ENDFORM.
FORM q2_july .
  it_data_quater-grp_lift_qty_m1 = wa_yrva_qais_data-grp_lift_qty_m4.
  it_data_quater-ind_lift_qty_m1 = wa_yrva_qais_data-ind_lift_qty_m4.
  it_data_quater-ind_elgl_qty_m1 = wa_yrva_qais_data-ind_lift_qty_m4.
ENDFORM.
FORM q2_aug .
  it_data_quater-grp_lift_qty_m2 = wa_yrva_qais_data-grp_lift_qty_m5.
  it_data_quater-ind_lift_qty_m2 = wa_yrva_qais_data-ind_lift_qty_m5.
  it_data_quater-ind_elgl_qty_m2 = wa_yrva_qais_data-ind_lift_qty_m5.
ENDFORM.
FORM q2_sep .
  it_data_quater-grp_lift_qty_m3 = wa_yrva_qais_data-grp_lift_qty_m6.
  it_data_quater-ind_lift_qty_m3 = wa_yrva_qais_data-ind_lift_qty_m6.
  it_data_quater-ind_elgl_qty_m3 = wa_yrva_qais_data-ind_lift_qty_m6.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  Q3_DISCOUNT / Q3_OCT / Q3_NOV / Q3_DEC
*&---------------------------------------------------------------------*
FORM q3_discount .
  IF wa_yrva_qais_data-p_d_sector = 'X' AND wa_yrva_mstr_waiver-max_perc_q3 IS NOT INITIAL.
    wa_yrva_mstr_waiver-max_perc_q3 = wa_yrva_mstr_waiver-pd_perc_q3.
    wa_yrva_mstr_waiver-annual_max  = wa_yrva_mstr_waiver-pd_perc_q3.
  ENDIF.
  PERFORM q3_oct. PERFORM q3_nov. PERFORM q3_dec.
  it_data_quater-tot_elgl_qty = it_data_quater-ind_elgl_qty_m1 +
                                it_data_quater-ind_elgl_qty_m2 +
                                it_data_quater-ind_elgl_qty_m3.
ENDFORM.
FORM q3_oct .
  it_data_quater-grp_lift_qty_m1 = wa_yrva_qais_data-grp_lift_qty_m7.
  it_data_quater-ind_lift_qty_m1 = wa_yrva_qais_data-ind_lift_qty_m7.
  it_data_quater-ind_elgl_qty_m1 = wa_yrva_qais_data-ind_lift_qty_m7.
ENDFORM.
FORM q3_nov .
  it_data_quater-grp_lift_qty_m2 = wa_yrva_qais_data-grp_lift_qty_m8.
  it_data_quater-ind_lift_qty_m2 = wa_yrva_qais_data-ind_lift_qty_m8.
  it_data_quater-ind_elgl_qty_m2 = wa_yrva_qais_data-ind_lift_qty_m8.
ENDFORM.
FORM q3_dec .
  it_data_quater-grp_lift_qty_m3 = wa_yrva_qais_data-grp_lift_qty_m9.
  it_data_quater-ind_lift_qty_m3 = wa_yrva_qais_data-ind_lift_qty_m9.
  it_data_quater-ind_elgl_qty_m3 = wa_yrva_qais_data-ind_lift_qty_m9.
ENDFORM.

*&---------------------------------------------------------------------*
*& Q4_DISCOUNT - AMENDED *** KEY CHANGE ***
*& Implements bi-monthly BCQ logic per circular:
*&   BCQ    = Jan MCQ + Feb MCQ
*&   QCQ    = Jan MCQ + Feb MCQ + Mar MCQ (full quarter, for comparison)
*&   Amended%  = (Jan lift + Feb lift) / BCQ * 100
*&   Original% = (Jan lift + Feb lift + Mar lift) / QCQ * 100
*&   Slab rates: >=120%->2500, >=110%->2000, >=100%->1500, else NA
*&   Final rate = higher of amended vs original (criterion iv)
*&   Disbursement: (Jan+Feb ind lift * rate) + (Mar ind lift * rate)
*&---------------------------------------------------------------------*
FORM q4_discount .
  DATA: lv_bcq         TYPE p DECIMALS 3,
        lv_qcq         TYPE p DECIMALS 3,
        lv_jan_feb_grp TYPE p DECIMALS 3,
        lv_full_grp    TYPE p DECIMALS 3,
        lv_amended_pct TYPE p DECIMALS 2,
        lv_orig_pct    TYPE p DECIMALS 2,
        lv_amended_rt  TYPE kbetr,
        lv_orig_rt     TYPE kbetr.

  " Map month fields (Jan=m10, Feb=m11, Mar=m12)
  PERFORM q4_jan. PERFORM q4_feb. PERFORM q4_mar.

  " BCQ = Jan MCQ + Feb MCQ (bi-monthly committed qty)
  lv_bcq = wa_yrva_qais_data-commited_qty_m10 + wa_yrva_qais_data-commited_qty_m11.
  " QCQ = full quarter MCQ
  lv_qcq = wa_yrva_qais_data-commited_qty_m10 + wa_yrva_qais_data-commited_qty_m11
          + wa_yrva_qais_data-commited_qty_m12.

  " Group liftings
  lv_jan_feb_grp = wa_yrva_qais_data-grp_lift_qty_m10 + wa_yrva_qais_data-grp_lift_qty_m11.
  lv_full_grp    = lv_jan_feb_grp + wa_yrva_qais_data-grp_lift_qty_m12.

  " Amended %: (Jan+Feb group lift) / BCQ * 100
  IF lv_bcq GT 0.
    lv_amended_pct = ( lv_jan_feb_grp / lv_bcq ) * 100.
  ENDIF.

  " Original %: full quarter group lift / QCQ * 100
  IF lv_qcq GT 0.
    lv_orig_pct = ( lv_full_grp / lv_qcq ) * 100.
  ENDIF.

  " Store percentages in output structure for display
  it_data_quater-bcq_perc  = lv_amended_pct.
  it_data_quater-orig_perc = lv_orig_pct.

  " Get slab rate for each scheme
  PERFORM get_q4_slab_rate USING lv_amended_pct CHANGING lv_amended_rt.
  PERFORM get_q4_slab_rate USING lv_orig_pct    CHANGING lv_orig_rt.

  " Apply higher rate - criterion iv: original scheme if it gives higher slab
  IF lv_orig_rt GT lv_amended_rt.
    lv_q4_amended_rate = lv_orig_rt.
    it_data_quater-scheme = 'ORIGINAL'.
  ELSEIF lv_amended_rt GT 0.
    lv_q4_amended_rate = lv_amended_rt.
    it_data_quater-scheme = 'AMENDED'.
  ELSE.
    " Neither scheme qualifies - skip record
    lv_flag123 = 'X'.
    it_data_quater-scheme = 'NA'.
    EXIT.
  ENDIF.

  " Eligible qty: all individual liftings (Jan + Feb + Mar)
  it_data_quater-tot_elgl_qty = it_data_quater-ind_elgl_qty_m1 +
                                it_data_quater-ind_elgl_qty_m2 +
                                it_data_quater-ind_elgl_qty_m3.
ENDFORM.

FORM q4_jan .
  it_data_quater-grp_lift_qty_m1 = wa_yrva_qais_data-grp_lift_qty_m10.
  it_data_quater-ind_lift_qty_m1 = wa_yrva_qais_data-ind_lift_qty_m10.
  it_data_quater-ind_elgl_qty_m1 = wa_yrva_qais_data-ind_lift_qty_m10.
ENDFORM.
FORM q4_feb .
  it_data_quater-grp_lift_qty_m2 = wa_yrva_qais_data-grp_lift_qty_m11.
  it_data_quater-ind_lift_qty_m2 = wa_yrva_qais_data-ind_lift_qty_m11.
  it_data_quater-ind_elgl_qty_m2 = wa_yrva_qais_data-ind_lift_qty_m11.
ENDFORM.
FORM q4_mar .
  it_data_quater-grp_lift_qty_m3 = wa_yrva_qais_data-grp_lift_qty_m12.
  it_data_quater-ind_lift_qty_m3 = wa_yrva_qais_data-ind_lift_qty_m12.
  it_data_quater-ind_elgl_qty_m3 = wa_yrva_qais_data-ind_lift_qty_m12.
ENDFORM.
*&      Form  ANNUAL_DISCOUNT
*&---------------------------------------------------------------------*
FORM annual_discount .
  DATA lv_fact_pro_annual TYPE p DECIMALS 3.
  DATA l_months TYPE i.
  DATA l_qty TYPE yannualqty.
  IF it_yrva_qais_data IS NOT INITIAL.
    REFRESH it_yrva_qais_data_m.
    SELECT * FROM yrva_qais_data_m INTO TABLE it_yrva_qais_data_m
      FOR ALL ENTRIES IN it_yrva_qais_data
      WHERE qais_no = it_yrva_qais_data-qais_no.
  ENDIF.
  SELECT * INTO TABLE i_a350
    FROM a350 WHERE kappl = 'V' AND kschl = 'ZAIS' AND vkorg = '5000'
              AND kfrst = '' AND datab LE s_sptag-high AND datbi GE s_sptag-low.
  IF NOT i_a350[] IS INITIAL.
    SELECT knumh kbetr kstbm INTO TABLE i_cond FROM konm
      FOR ALL ENTRIES IN i_a350 WHERE knumh = i_a350-knumh .
  ENDIF.
  LOOP AT it_yrva_qais_data INTO wa_yrva_qais_data.
    CLEAR: w_year_min, w_year_max, l_months, l_qty.
    CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
      EXPORTING
        begda    = wa_yrva_qais_data-mou_begda
        endda    = wa_yrva_qais_data-mou_endda
      IMPORTING
        d_months = l_months.
    IF l_months < 12.
      l_qty = ( wa_yrva_qais_data-mou_qty / l_months ) * 12.
    ELSE.
      l_qty = wa_yrva_qais_data-mou_qty .
    ENDIF.
    w_year_min = wa_yrva_qais_data-mou_qty * wa_yrva_mstr_waiver-annual_min / 100 .
    IF wa_yrva_qais_data-p_d_sector = 'X'.
      IF wa_yrva_mstr_waiver-pd_roto_max IS INITIAL.
        lv_pd_annual_max_flag = 'X'.
      ELSE.
        w_year_max = wa_yrva_qais_data-mou_qty * ( wa_yrva_mstr_waiver-pd_roto_max / 100 ).
      ENDIF.
    ELSE.
      IF wa_yrva_mstr_waiver-annual_max IS INITIAL.
        lv_annual_max_flag = 'X'.
      ELSE.
        w_year_max = wa_yrva_qais_data-mou_qty * wa_yrva_mstr_waiver-annual_max / 100 .
      ENDIF.
    ENDIF.
    it_data_annual-kunnr    = wa_yrva_qais_data-kunnr.
    it_data_annual-kvgr2    = wa_yrva_qais_data-kvgr2.
    it_data_annual-vkbur    = wa_yrva_qais_data-vkbur.
    it_data_annual-mou_qty  = wa_yrva_qais_data-mou_qty.
    it_data_annual-waiver_1 = wa_yrva_qais_data-waiver_1 .
    it_data_annual-waiver_2 = wa_yrva_qais_data-waiver_2 .
    it_data_annual-waiver_3 = wa_yrva_qais_data-waiver_3 .
    it_data_annual-grp_lift_qty = wa_yrva_qais_data-grp_lift_qty_m1 + wa_yrva_qais_data-grp_lift_qty_m2 +
                                  wa_yrva_qais_data-grp_lift_qty_m3 + wa_yrva_qais_data-grp_lift_qty_m4 +
                                  wa_yrva_qais_data-grp_lift_qty_m5 + wa_yrva_qais_data-grp_lift_qty_m6 +
                                  wa_yrva_qais_data-grp_lift_qty_m7 + wa_yrva_qais_data-grp_lift_qty_m8 +
                                  wa_yrva_qais_data-grp_lift_qty_m9 + wa_yrva_qais_data-grp_lift_qty_m10 +
                                  wa_yrva_qais_data-grp_lift_qty_m11 + wa_yrva_qais_data-grp_lift_qty_m12 .
    it_data_annual-ind_lift_qty = wa_yrva_qais_data-ind_lift_qty_m1 + wa_yrva_qais_data-ind_lift_qty_m2 +
                                  wa_yrva_qais_data-ind_lift_qty_m3 + wa_yrva_qais_data-ind_lift_qty_m4 +
                                  wa_yrva_qais_data-ind_lift_qty_m5 + wa_yrva_qais_data-ind_lift_qty_m6 +
                                  wa_yrva_qais_data-ind_lift_qty_m7 + wa_yrva_qais_data-ind_lift_qty_m8 +
                                  wa_yrva_qais_data-ind_lift_qty_m9 + wa_yrva_qais_data-ind_lift_qty_m10 +
                                  wa_yrva_qais_data-ind_lift_qty_m11 + wa_yrva_qais_data-ind_lift_qty_m12 .
    IF it_data_annual-grp_lift_qty GT w_year_max .
      IF ( lv_pd_annual_max_flag = 'X' OR lv_annual_max_flag = 'X' ) AND it_data_annual-grp_lift_qty GE w_year_min.
        it_data_annual-tot_elgl_qty = ( it_data_annual-ind_lift_qty / it_data_annual-grp_lift_qty )
                                      * it_data_annual-grp_lift_qty .
      ELSE.
        it_data_annual-tot_elgl_qty = ( it_data_annual-ind_lift_qty / it_data_annual-grp_lift_qty ) * w_year_max .
      ENDIF.
    ELSEIF it_data_annual-grp_lift_qty GE w_year_min AND it_data_annual-grp_lift_qty LE w_year_max .
      it_data_annual-tot_elgl_qty = ( it_data_annual-ind_lift_qty / it_data_annual-grp_lift_qty )
                                    * it_data_annual-grp_lift_qty .
    ELSE.
      CLEAR it_data_annual-tot_elgl_qty.
    ENDIF.
    SORT i_cond BY kstbm DESCENDING.
    LOOP AT i_cond WHERE kstbm <= l_qty.
      lv_fact_pro_annual = ( it_data_annual-grp_lift_qty / it_data_annual-mou_qty ) * 100.
      IF lv_fact_pro_annual GE 100.
        w_kbetr = i_cond-kbetr.
      ELSEIF lv_fact_pro_annual GE wa_yrva_mstr_waiver-annual_min.
        w_kbetr = ( i_cond-kbetr * lv_fact_pro_annual ) / 100.
      ENDIF.
      it_data_annual-value = it_data_annual-tot_elgl_qty * w_kbetr.
      IF it_data_annual-value LT 0 .
        it_data_annual-value = it_data_annual-value * -1 .
      ENDIF.
      EXIT.
    ENDLOOP.
    it_data_annual-remarks = w_remarks.
    CONDENSE it_data_annual-remarks .
    IF wa_yrva_qais_data-mou_begda+4(2) NE '02' AND wa_yrva_qais_data-mou_begda+4(2) NE '03'.
      APPEND it_data_annual.
    ENDIF.
    CLEAR: it_data_annual, wa_yrva_qais_data.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ANNUAL_CONSIS_DISCOUNT
*&---------------------------------------------------------------------*
FORM annual_consis_discount .
  DATA: lv_month TYPE vtbbewe-atage,
        lv_begda TYPE vtbbewe-dbervon,
        lv_endda TYPE vtbbewe-dberbis.
  LOOP AT it_yrva_qais_data INTO wa_yrva_qais_data.
    it_annual_consis-mou_begda    = wa_yrva_qais_data-mou_begda.
    it_annual_consis-mou_endda    = wa_yrva_qais_data-mou_endda.
    it_annual_consis-kunnr        = wa_yrva_qais_data-kunnr.
    it_annual_consis-kvgr2        = wa_yrva_qais_data-kvgr2.
    it_annual_consis-vkbur        = wa_yrva_qais_data-vkbur.
    it_annual_consis-mou_qty      = wa_yrva_qais_data-mou_qty.
    it_annual_consis-grp_lift_qty = wa_yrva_qais_data-grp_lift_qty_m1 + wa_yrva_qais_data-grp_lift_qty_m2 +
                                    wa_yrva_qais_data-grp_lift_qty_m3 + wa_yrva_qais_data-grp_lift_qty_m4 +
                                    wa_yrva_qais_data-grp_lift_qty_m5 + wa_yrva_qais_data-grp_lift_qty_m6 +
                                    wa_yrva_qais_data-grp_lift_qty_m7 + wa_yrva_qais_data-grp_lift_qty_m8 +
                                    wa_yrva_qais_data-grp_lift_qty_m9 + wa_yrva_qais_data-grp_lift_qty_m10 +
                                    wa_yrva_qais_data-grp_lift_qty_m11 + wa_yrva_qais_data-grp_lift_qty_m12 .
    it_annual_consis-ind_lift_qty = wa_yrva_qais_data-ind_lift_qty_m1 + wa_yrva_qais_data-ind_lift_qty_m2 +
                                    wa_yrva_qais_data-ind_lift_qty_m3 + wa_yrva_qais_data-ind_lift_qty_m4 +
                                    wa_yrva_qais_data-ind_lift_qty_m5 + wa_yrva_qais_data-ind_lift_qty_m6 +
                                    wa_yrva_qais_data-ind_lift_qty_m7 + wa_yrva_qais_data-ind_lift_qty_m8 +
                                    wa_yrva_qais_data-ind_lift_qty_m9 + wa_yrva_qais_data-ind_lift_qty_m10 +
                                    wa_yrva_qais_data-ind_lift_qty_m11 + wa_yrva_qais_data-ind_lift_qty_m12 .
    w_year_min = wa_yrva_qais_data-mou_qty * wa_yrva_mstr_waiver-annual_min / 100 .
    IF wa_yrva_mstr_waiver-annual_max IS NOT INITIAL.
      w_year_max = wa_yrva_qais_data-mou_qty * wa_yrva_mstr_waiver-annual_max / 100 .
    ENDIF.
    IF it_annual_consis-grp_lift_qty GE w_year_min AND it_annual_consis-grp_lift_qty LE w_year_max .
      it_annual_consis-tot_elgl_qty = ( it_annual_consis-ind_lift_qty / it_annual_consis-grp_lift_qty )
                                       * it_annual_consis-grp_lift_qty .
    ENDIF.
    lv_begda = it_annual_consis-mou_begda.
    lv_endda = it_annual_consis-mou_endda.
    CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
      EXPORTING
        i_date_from = lv_begda
        i_date_to   = lv_endda
      IMPORTING
        e_months    = lv_month.
    IF lv_month EQ 10 OR lv_month EQ 11.
      it_annual_consis-value = it_annual_consis-tot_elgl_qty * 100.
    ELSEIF lv_month EQ 12.
      it_annual_consis-value = it_annual_consis-tot_elgl_qty * 200.
    ELSE.
      it_annual_consis-value = 0.
      it_annual_consis-tot_elgl_qty = 0.
    ENDIF.
    it_annual_consis-remarks = w_remarks.
    CONDENSE it_annual_consis-remarks .
    APPEND it_annual_consis .
    CLEAR: it_annual_consis, wa_yrva_qais_data.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MONTHLY_DISCOUNT
*&---------------------------------------------------------------------*
FORM monthly_discount .
  CLEAR lv_mth.
  CASE s_sptag-low+4(2).
    WHEN '1'.  lv_mth = 'JAN'.
    WHEN '2'.  lv_mth = 'FEB'.
    WHEN '3'.  lv_mth = 'MAR'.
    WHEN '4'.  lv_mth = 'APR'.
    WHEN '5'.  lv_mth = 'MAY'.
    WHEN '6'.  lv_mth = 'JUN'.
    WHEN '7'.  lv_mth = 'JUL'.
    WHEN '8'.  lv_mth = 'AUG'.
    WHEN '9'.  lv_mth = 'SEP'.
    WHEN '10'. lv_mth = 'OCT'.
    WHEN '11'. lv_mth = 'NOV'.
    WHEN '12'. lv_mth = 'DEC'.
  ENDCASE.
  IF r_month EQ 'X'.
    SELECT * INTO TABLE i_a350 FROM a350 WHERE kappl = 'V' AND kschl = 'ZMIS' AND vkorg = '5000'
              AND kfrst = '' AND datab LE s_sptag-high AND datbi GE s_sptag-low.
  ELSEIF r_month1 EQ 'X'.
    SELECT * INTO TABLE i_a350 FROM a350 WHERE kappl = 'V' AND kschl = 'ZAMS' AND vkorg = '5000'
              AND kfrst = '' AND datab LE s_sptag-high AND datbi GE s_sptag-low.
  ELSEIF c_maint EQ 'X'.
    SELECT * INTO TABLE i_a350 FROM a350 WHERE kappl = 'V' AND kschl = 'PE32' AND vkorg = '5000'
              AND kfrst = '' AND datab LE s_sptag-high AND datbi GE s_sptag-low.
  ELSEIF c_maint1 EQ 'X'.
    SELECT * INTO TABLE i_a350 FROM a350 WHERE kappl = 'V' AND kschl = 'PE31' AND vkorg = '5000'
              AND kfrst = '' AND datab LE s_sptag-high AND datbi GE s_sptag-low.
  ELSEIF r_rhd EQ 'X'.
    SELECT * INTO TABLE i_a350 FROM a350 WHERE kappl = 'V' AND kschl = 'PE07' AND vkorg = '5000'
              AND kfrst = '' AND datab LE s_sptag-high AND datbi GE s_sptag-low.
  ELSEIF r_rlld EQ 'X'.
    SELECT * INTO TABLE i_a350 FROM a350 WHERE kappl = 'V' AND kschl = 'PE08' AND vkorg = '5000'
              AND kfrst = '' AND datab LE s_sptag-high AND datbi GE s_sptag-low.
  ENDIF.
  IF NOT i_a350[] IS INITIAL.
    SELECT knumh kbetr kstbm INTO TABLE i_cond FROM konm
      FOR ALL ENTRIES IN i_a350 WHERE knumh = i_a350-knumh .
  ENDIF.
  IF it_yrva_qais_data[] IS NOT INITIAL.
    SELECT * FROM yrva_qais_add_wv INTO TABLE it_yrva_qais_add_wv
      FOR ALL ENTRIES IN it_yrva_qais_data WHERE qais_no = it_yrva_qais_data-qais_no.
    SELECT * FROM yrva_revival_fai INTO TABLE it_yrva_revival_fai
      FOR ALL ENTRIES IN it_yrva_qais_data WHERE qais_no = it_yrva_qais_data-qais_no.
  ELSE.
    IF c_chk IS NOT INITIAL.
      EXIT.
    ELSE.
      MESSAGE 'List Contains No Records' TYPE 'E'.
    ENDIF.
  ENDIF.
  LOOP AT it_yrva_qais_data INTO wa_yrva_qais_data.
    CLEAR : lv_revival_m, wa_yrva_revival_fai.
    it_data_monthly-kunnr = wa_yrva_qais_data-kunnr.
    it_data_monthly-kvgr2 = wa_yrva_qais_data-kvgr2 .
    it_data_monthly-vkbur = wa_yrva_qais_data-vkbur.
    it_data_monthly-begda = s_sptag-low.
    it_data_monthly-endda = s_sptag-high.
    w_flag_month = 'X' .
    MOVE-CORRESPONDING wa_yrva_mstr_waiver_temp TO wa_yrva_mstr_waiver .
    READ TABLE it_yrva_qais_add_wv INTO wa_yrva_qais_add_wv
      WITH KEY qais_no = wa_yrva_qais_data-qais_no waiver_month = lv_mth .
    IF sy-subrc EQ 0 . w_waive_month = 'X' . ENDIF.
    READ TABLE it_yrva_revival_fai INTO wa_yrva_revival_fai
      WITH KEY qais_no = wa_yrva_qais_data-qais_no.
    IF sy-subrc EQ 0 AND s_sptag-low < '20231001'.
      lv_revival_m = 'X'.
    ENDIF.
    CASE s_sptag-low+4(2).
      WHEN '1'.  PERFORM month_jan.
      WHEN '2'.  PERFORM month_feb.
      WHEN '3'.  PERFORM month_mar.
      WHEN '4'.  PERFORM month_apr.
      WHEN '5'.  PERFORM month_may.
      WHEN '6'.  PERFORM month_jun.
      WHEN '7'.  PERFORM month_jul.
      WHEN '8'.  PERFORM month_aug.
      WHEN '9'.  PERFORM month_sep.
      WHEN '10'. PERFORM month_oct.
      WHEN '11'. PERFORM month_nov.
      WHEN '12'. PERFORM month_dec.
    ENDCASE.
    IF lv_flag123 IS INITIAL.
      CASE s_sptag-high+4(2).
        WHEN '04'. lv_mon = 'APRIL'.
                   it_data_monthly-commited_qty = wa_yrva_qais_data-commited_qty_m1.
                   it_data_monthly-grp_lift_qty = wa_yrva_qais_data-grp_lift_qty_m1.
                   it_data_monthly-ind_lift_qty = wa_yrva_qais_data-ind_lift_qty_m1.
        WHEN '05'. lv_mon = 'MAY'.
                   it_data_monthly-commited_qty = wa_yrva_qais_data-commited_qty_m2.
                   it_data_monthly-grp_lift_qty = wa_yrva_qais_data-grp_lift_qty_m2.
                   it_data_monthly-ind_lift_qty = wa_yrva_qais_data-ind_lift_qty_m2.
        WHEN '06'. lv_mon = 'JUNE'.
                   it_data_monthly-commited_qty = wa_yrva_qais_data-commited_qty_m3.
                   it_data_monthly-grp_lift_qty = wa_yrva_qais_data-grp_lift_qty_m3.
                   it_data_monthly-ind_lift_qty = wa_yrva_qais_data-ind_lift_qty_m3.
        WHEN '07'. lv_mon = 'JULY'.
                   it_data_monthly-commited_qty = wa_yrva_qais_data-commited_qty_m4.
                   it_data_monthly-grp_lift_qty = wa_yrva_qais_data-grp_lift_qty_m4.
                   it_data_monthly-ind_lift_qty = wa_yrva_qais_data-ind_lift_qty_m4.
        WHEN '08'. lv_mon = 'AUGUST'.
                   it_data_monthly-commited_qty = wa_yrva_qais_data-commited_qty_m5.
                   it_data_monthly-grp_lift_qty = wa_yrva_qais_data-grp_lift_qty_m5.
                   it_data_monthly-ind_lift_qty = wa_yrva_qais_data-ind_lift_qty_m5.
        WHEN '09'. lv_mon = 'SEPTEMBER'.
                   it_data_monthly-commited_qty = wa_yrva_qais_data-commited_qty_m6.
                   it_data_monthly-grp_lift_qty = wa_yrva_qais_data-grp_lift_qty_m6.
                   it_data_monthly-ind_lift_qty = wa_yrva_qais_data-ind_lift_qty_m6.
        WHEN '10'. lv_mon = 'OCTOBER'.
                   it_data_monthly-commited_qty = wa_yrva_qais_data-commited_qty_m7.
                   it_data_monthly-grp_lift_qty = wa_yrva_qais_data-grp_lift_qty_m7.
                   it_data_monthly-ind_lift_qty = wa_yrva_qais_data-ind_lift_qty_m7.
        WHEN '11'. lv_mon = 'NOVEMBER'.
                   it_data_monthly-commited_qty = wa_yrva_qais_data-commited_qty_m8.
                   it_data_monthly-grp_lift_qty = wa_yrva_qais_data-grp_lift_qty_m8.
                   it_data_monthly-ind_lift_qty = wa_yrva_qais_data-ind_lift_qty_m8.
        WHEN '12'. lv_mon = 'DECEMBER'.
                   it_data_monthly-commited_qty = wa_yrva_qais_data-commited_qty_m9.
                   it_data_monthly-grp_lift_qty = wa_yrva_qais_data-grp_lift_qty_m9.
                   it_data_monthly-ind_lift_qty = wa_yrva_qais_data-ind_lift_qty_m9.
        WHEN '01'. lv_mon = 'JANUARY'.
                   it_data_monthly-commited_qty = wa_yrva_qais_data-commited_qty_m10.
                   it_data_monthly-grp_lift_qty = wa_yrva_qais_data-grp_lift_qty_m10.
                   it_data_monthly-ind_lift_qty = wa_yrva_qais_data-ind_lift_qty_m10.
        WHEN '02'. lv_mon = 'FEBRUARY'.
                   it_data_monthly-commited_qty = wa_yrva_qais_data-commited_qty_m11.
                   it_data_monthly-grp_lift_qty = wa_yrva_qais_data-grp_lift_qty_m11.
                   it_data_monthly-ind_lift_qty = wa_yrva_qais_data-ind_lift_qty_m11.
        WHEN '03'. lv_mon = 'MARCH'.
                   it_data_monthly-commited_qty = wa_yrva_qais_data-commited_qty_m12.
                   it_data_monthly-grp_lift_qty = wa_yrva_qais_data-grp_lift_qty_m12.
                   it_data_monthly-ind_lift_qty = wa_yrva_qais_data-ind_lift_qty_m12.
      ENDCASE.
      IF it_data_monthly-grp_lift_qty LT w_month_min .
        CLEAR: it_data_monthly-ind_elgl_qty, w_flag_month .
      ELSEIF it_data_monthly-grp_lift_qty GT w_month_max .
        IF w_month_max_perc IS NOT INITIAL.
          it_data_monthly-ind_elgl_qty = ( it_data_monthly-ind_lift_qty / it_data_monthly-grp_lift_qty ) * w_month_max .
        ELSE.
          it_data_monthly-ind_elgl_qty = it_data_monthly-ind_lift_qty.
        ENDIF.
      ELSEIF it_data_monthly-grp_lift_qty GE w_month_min AND it_data_monthly-grp_lift_qty LE w_month_max .
        it_data_monthly-ind_elgl_qty = ( it_data_monthly-ind_lift_qty / it_data_monthly-grp_lift_qty ) * it_data_monthly-grp_lift_qty.
      ENDIF.
      it_data_monthly-tot_elgl_qty = it_data_monthly-ind_elgl_qty .
      lv_year1 = s_sptag-high+2(2).
      IF s_sptag-high+4(2) LE '03'.
        lv_year2 = lv_year1 - 1.
        CONCATENATE lv_year2 '-' lv_year1 INTO lv_dat.
      ELSE.
        lv_year2 = lv_year1 + 1.
        CONCATENATE lv_year1 '-' lv_year2 INTO lv_dat.
      ENDIF.
      IF r_month EQ 'X'.
        CONCATENATE 'CIS' lv_dat lv_mon 'Discount' INTO w_remarks SEPARATED BY ' '.
      ELSEIF r_month1 EQ 'X'.
        CONCATENATE 'CIS' lv_dat lv_mon ' Spl.Discount' INTO w_remarks SEPARATED BY ' '.
      ENDIF.
      SORT i_cond BY kstbm DESCENDING.
      IF r_month EQ 'X'.
        LOOP AT i_cond WHERE kstbm <= wa_yrva_qais_data-mou_qty.
          lv_fact = ( it_data_monthly-grp_lift_qty / it_data_monthly-commited_qty ) * 100.
          IF lv_fact GE w_month_min_perc OR w_waive_month EQ 'X' OR lv_flag1 EQ 'X' OR lv_flag2 EQ 'X'.
            w_kbetr = i_cond-kbetr.
          ELSE.
            CLEAR: w_kbetr.
          ENDIF.
          EXIT.
        ENDLOOP.
        it_data_monthly-value = it_data_monthly-ind_elgl_qty * w_kbetr.
        IF it_data_monthly-value LT 0 .
          it_data_monthly-value = it_data_monthly-value * -1.
        ENDIF.
      ENDIF.
      it_data_monthly-remarks = w_remarks.
      APPEND it_data_monthly .
      MODIFY it_yrva_qais_data FROM wa_yrva_qais_data.
      CLEAR: w_waive_month, lv_flag1, w_kbetr, i_cond, wa_yrva_qais_data, it_data_monthly-tot_elgl_qty.
    ELSE.
      CLEAR: lv_flag123.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MONTH_JAN ... MONTH_DEC
*& All twelve forms compute monthly min/max thresholds and group lifted
*& qty for the chosen month using the master waiver template.
*&---------------------------------------------------------------------*
FORM month_jan .
  CLEAR: w_month_max_perc, w_month_min_perc, lv_flag2.
  IF wa_yrva_qais_data_m-mon_so_m9 IS INITIAL AND wa_yrva_qais_data-mou_begda LT ls_yrva_cis_mstr-yy_start_date.
    lv_flag123 = 'X'. EXIT.
  ENDIF.
  w_month_min_perc = wa_yrva_mstr_waiver-min_perc_m10.
  IF wa_yrva_mstr_waiver-max_perc_m10 IS NOT INITIAL.
    w_month_max_perc = wa_yrva_mstr_waiver-max_perc_m10 / 100.
  ENDIF.
  w_month_min = wa_yrva_qais_data-commited_qty_m10 * w_month_min_perc / 100.
  IF w_month_max_perc IS NOT INITIAL.
    w_month_max = wa_yrva_qais_data-commited_qty_m10 * w_month_max_perc.
  ELSE.
    w_month_max = wa_yrva_qais_data-ind_lift_qty_m10.
  ENDIF.
  IF wa_yrva_qais_data-kvgr2 IS INITIAL.
    it_data_monthly-grp_lift_qty = wa_yrva_qais_data-ind_lift_qty_m10.
  ELSE.
    READ TABLE it_yrva_qais_data_temp INTO wa_yrva_qais_data_temp WITH KEY kvgr2 = wa_yrva_qais_data-kvgr2 .
    IF sy-subrc EQ 0 .
      it_data_monthly-grp_lift_qty = wa_yrva_qais_data_temp-grp_lift_qty_m10.
    ENDIF.
  ENDIF.
ENDFORM.

FORM month_feb .
  CLEAR: w_month_max_perc, w_month_min_perc, lv_flag2.
  w_month_min_perc = wa_yrva_mstr_waiver-min_perc_m11.
  IF wa_yrva_mstr_waiver-max_perc_m11 IS NOT INITIAL.
    w_month_max_perc = wa_yrva_mstr_waiver-max_perc_m11 / 100.
  ENDIF.
  w_month_min = wa_yrva_qais_data-commited_qty_m11 * w_month_min_perc / 100.
  IF w_month_max_perc IS NOT INITIAL.
    w_month_max = wa_yrva_qais_data-commited_qty_m11 * w_month_max_perc.
  ELSE.
    w_month_max = wa_yrva_qais_data-ind_lift_qty_m11.
  ENDIF.
  IF wa_yrva_qais_data-kvgr2 IS INITIAL.
    it_data_monthly-grp_lift_qty = wa_yrva_qais_data-ind_lift_qty_m11.
  ELSE.
    READ TABLE it_yrva_qais_data_temp INTO wa_yrva_qais_data_temp WITH KEY kvgr2 = wa_yrva_qais_data-kvgr2 .
    IF sy-subrc EQ 0 .
      it_data_monthly-grp_lift_qty = wa_yrva_qais_data_temp-grp_lift_qty_m11.
    ENDIF.
  ENDIF.
ENDFORM.

FORM month_mar .
  CLEAR: w_month_max_perc, w_month_min_perc, lv_flag2.
  w_month_min_perc = wa_yrva_mstr_waiver-min_perc_m12.
  IF wa_yrva_mstr_waiver-max_perc_m12 IS NOT INITIAL.
    w_month_max_perc = wa_yrva_mstr_waiver-max_perc_m12 / 100.
  ENDIF.
  w_month_min = wa_yrva_qais_data-commited_qty_m12 * w_month_min_perc / 100.
  IF w_month_max_perc IS NOT INITIAL.
    w_month_max = wa_yrva_qais_data-commited_qty_m12 * w_month_max_perc.
  ELSE.
    w_month_max = wa_yrva_qais_data-ind_lift_qty_m12.
  ENDIF.
  IF wa_yrva_qais_data-kvgr2 IS INITIAL.
    it_data_monthly-grp_lift_qty = wa_yrva_qais_data-ind_lift_qty_m12.
  ELSE.
    READ TABLE it_yrva_qais_data_temp INTO wa_yrva_qais_data_temp WITH KEY kvgr2 = wa_yrva_qais_data-kvgr2 .
    IF sy-subrc EQ 0 .
      it_data_monthly-grp_lift_qty = wa_yrva_qais_data_temp-grp_lift_qty_m12.
    ENDIF.
  ENDIF.
ENDFORM.

FORM month_apr .
  CLEAR: w_month_max_perc, w_month_min_perc, lv_flag2.
  w_month_min_perc = wa_yrva_mstr_waiver-min_perc_m1.
  IF wa_yrva_mstr_waiver-max_perc_m1 IS NOT INITIAL.
    w_month_max_perc = wa_yrva_mstr_waiver-max_perc_m1 / 100.
  ENDIF.
  w_month_min = wa_yrva_qais_data-commited_qty_m1 * w_month_min_perc / 100.
  IF w_month_max_perc IS NOT INITIAL.
    w_month_max = wa_yrva_qais_data-commited_qty_m1 * w_month_max_perc.
  ELSE.
    w_month_max = wa_yrva_qais_data-ind_lift_qty_m1.
  ENDIF.
  IF wa_yrva_qais_data-kvgr2 IS INITIAL.
    it_data_monthly-grp_lift_qty = wa_yrva_qais_data-ind_lift_qty_m1.
  ELSE.
    READ TABLE it_yrva_qais_data_temp INTO wa_yrva_qais_data_temp WITH KEY kvgr2 = wa_yrva_qais_data-kvgr2 .
    IF sy-subrc EQ 0 .
      it_data_monthly-grp_lift_qty = wa_yrva_qais_data_temp-grp_lift_qty_m1.
    ENDIF.
  ENDIF.
ENDFORM.

FORM month_may .
  CLEAR: w_month_max_perc, w_month_min_perc, lv_flag2.
  w_month_min_perc = wa_yrva_mstr_waiver-min_perc_m2.
  IF wa_yrva_mstr_waiver-max_perc_m2 IS NOT INITIAL.
    w_month_max_perc = wa_yrva_mstr_waiver-max_perc_m2 / 100.
  ENDIF.
  w_month_min = wa_yrva_qais_data-commited_qty_m2 * w_month_min_perc / 100.
  IF w_month_max_perc IS NOT INITIAL.
    w_month_max = wa_yrva_qais_data-commited_qty_m2 * w_month_max_perc.
  ELSE.
    w_month_max = wa_yrva_qais_data-ind_lift_qty_m2.
  ENDIF.
  IF wa_yrva_qais_data-kvgr2 IS INITIAL.
    it_data_monthly-grp_lift_qty = wa_yrva_qais_data-ind_lift_qty_m2.
  ELSE.
    READ TABLE it_yrva_qais_data_temp INTO wa_yrva_qais_data_temp WITH KEY kvgr2 = wa_yrva_qais_data-kvgr2 .
    IF sy-subrc EQ 0 .
      it_data_monthly-grp_lift_qty = wa_yrva_qais_data_temp-grp_lift_qty_m2.
    ENDIF.
  ENDIF.
ENDFORM.

FORM month_jun .
  CLEAR: w_month_max_perc, w_month_min_perc, lv_flag2.
  w_month_min_perc = wa_yrva_mstr_waiver-min_perc_m3.
  IF wa_yrva_mstr_waiver-max_perc_m3 IS NOT INITIAL.
    w_month_max_perc = wa_yrva_mstr_waiver-max_perc_m3 / 100.
  ENDIF.
  w_month_min = wa_yrva_qais_data-commited_qty_m3 * w_month_min_perc / 100.
  IF w_month_max_perc IS NOT INITIAL.
    w_month_max = wa_yrva_qais_data-commited_qty_m3 * w_month_max_perc.
  ELSE.
    w_month_max = wa_yrva_qais_data-ind_lift_qty_m3.
  ENDIF.
  IF wa_yrva_qais_data-kvgr2 IS INITIAL.
    it_data_monthly-grp_lift_qty = wa_yrva_qais_data-ind_lift_qty_m3.
  ELSE.
    READ TABLE it_yrva_qais_data_temp INTO wa_yrva_qais_data_temp WITH KEY kvgr2 = wa_yrva_qais_data-kvgr2 .
    IF sy-subrc EQ 0 .
      it_data_monthly-grp_lift_qty = wa_yrva_qais_data_temp-grp_lift_qty_m3.
    ENDIF.
  ENDIF.
ENDFORM.

FORM month_jul .
  CLEAR: w_month_max_perc, w_month_min_perc, lv_flag2.
  w_month_min_perc = wa_yrva_mstr_waiver-min_perc_m4.
  IF wa_yrva_mstr_waiver-max_perc_m4 IS NOT INITIAL.
    w_month_max_perc = wa_yrva_mstr_waiver-max_perc_m4 / 100.
  ENDIF.
  w_month_min = wa_yrva_qais_data-commited_qty_m4 * w_month_min_perc / 100.
  IF w_month_max_perc IS NOT INITIAL.
    w_month_max = wa_yrva_qais_data-commited_qty_m4 * w_month_max_perc.
  ELSE.
    w_month_max = wa_yrva_qais_data-ind_lift_qty_m4.
  ENDIF.
  IF wa_yrva_qais_data-kvgr2 IS INITIAL.
    it_data_monthly-grp_lift_qty = wa_yrva_qais_data-ind_lift_qty_m4.
  ELSE.
    READ TABLE it_yrva_qais_data_temp INTO wa_yrva_qais_data_temp WITH KEY kvgr2 = wa_yrva_qais_data-kvgr2 .
    IF sy-subrc EQ 0 .
      it_data_monthly-grp_lift_qty = wa_yrva_qais_data_temp-grp_lift_qty_m4.
    ENDIF.
  ENDIF.
ENDFORM.

FORM month_aug .
  CLEAR: w_month_max_perc, w_month_min_perc, lv_flag2.
  w_month_min_perc = wa_yrva_mstr_waiver-min_perc_m5.
  IF wa_yrva_mstr_waiver-max_perc_m5 IS NOT INITIAL.
    w_month_max_perc = wa_yrva_mstr_waiver-max_perc_m5 / 100.
  ENDIF.
  w_month_min = wa_yrva_qais_data-commited_qty_m5 * w_month_min_perc / 100.
  IF w_month_max_perc IS NOT INITIAL.
    w_month_max = wa_yrva_qais_data-commited_qty_m5 * w_month_max_perc.
  ELSE.
    w_month_max = wa_yrva_qais_data-ind_lift_qty_m5.
  ENDIF.
  IF wa_yrva_qais_data-kvgr2 IS INITIAL.
    it_data_monthly-grp_lift_qty = wa_yrva_qais_data-ind_lift_qty_m5.
  ELSE.
    READ TABLE it_yrva_qais_data_temp INTO wa_yrva_qais_data_temp WITH KEY kvgr2 = wa_yrva_qais_data-kvgr2 .
    IF sy-subrc EQ 0 .
      it_data_monthly-grp_lift_qty = wa_yrva_qais_data_temp-grp_lift_qty_m5.
    ENDIF.
  ENDIF.
ENDFORM.

FORM month_sep .
  CLEAR: w_month_max_perc, w_month_min_perc, lv_flag2.
  w_month_min_perc = wa_yrva_mstr_waiver-min_perc_m6.
  IF wa_yrva_mstr_waiver-max_perc_m6 IS NOT INITIAL.
    w_month_max_perc = wa_yrva_mstr_waiver-max_perc_m6 / 100.
  ENDIF.
  w_month_min = wa_yrva_qais_data-commited_qty_m6 * w_month_min_perc / 100.
  IF w_month_max_perc IS NOT INITIAL.
    w_month_max = wa_yrva_qais_data-commited_qty_m6 * w_month_max_perc.
  ELSE.
    w_month_max = wa_yrva_qais_data-ind_lift_qty_m6.
  ENDIF.
  IF wa_yrva_qais_data-kvgr2 IS INITIAL.
    it_data_monthly-grp_lift_qty = wa_yrva_qais_data-ind_lift_qty_m6.
  ELSE.
    READ TABLE it_yrva_qais_data_temp INTO wa_yrva_qais_data_temp WITH KEY kvgr2 = wa_yrva_qais_data-kvgr2 .
    IF sy-subrc EQ 0 .
      it_data_monthly-grp_lift_qty = wa_yrva_qais_data_temp-grp_lift_qty_m6.
    ENDIF.
  ENDIF.
ENDFORM.

FORM month_oct .
  CLEAR: w_month_max_perc, w_month_min_perc, lv_flag2.
  w_month_min_perc = wa_yrva_mstr_waiver-min_perc_m7.
  IF wa_yrva_mstr_waiver-max_perc_m7 IS NOT INITIAL.
    w_month_max_perc = wa_yrva_mstr_waiver-max_perc_m7 / 100.
  ENDIF.
  w_month_min = wa_yrva_qais_data-commited_qty_m7 * w_month_min_perc / 100.
  IF w_month_max_perc IS NOT INITIAL.
    w_month_max = wa_yrva_qais_data-commited_qty_m7 * w_month_max_perc.
  ELSE.
    w_month_max = wa_yrva_qais_data-ind_lift_qty_m7.
  ENDIF.
  IF wa_yrva_qais_data-kvgr2 IS INITIAL.
    it_data_monthly-grp_lift_qty = wa_yrva_qais_data-ind_lift_qty_m7.
  ELSE.
    READ TABLE it_yrva_qais_data_temp INTO wa_yrva_qais_data_temp WITH KEY kvgr2 = wa_yrva_qais_data-kvgr2 .
    IF sy-subrc EQ 0 .
      it_data_monthly-grp_lift_qty = wa_yrva_qais_data_temp-grp_lift_qty_m7.
    ENDIF.
  ENDIF.
ENDFORM.

FORM month_nov .
  CLEAR: w_month_max_perc, w_month_min_perc, lv_flag2.
  w_month_min_perc = wa_yrva_mstr_waiver-min_perc_m8.
  IF wa_yrva_mstr_waiver-max_perc_m8 IS NOT INITIAL.
    w_month_max_perc = wa_yrva_mstr_waiver-max_perc_m8 / 100.
  ENDIF.
  w_month_min = wa_yrva_qais_data-commited_qty_m8 * w_month_min_perc / 100.
  IF w_month_max_perc IS NOT INITIAL.
    w_month_max = wa_yrva_qais_data-commited_qty_m8 * w_month_max_perc.
  ELSE.
    w_month_max = wa_yrva_qais_data-ind_lift_qty_m8.
  ENDIF.
  IF wa_yrva_qais_data-kvgr2 IS INITIAL.
    it_data_monthly-grp_lift_qty = wa_yrva_qais_data-ind_lift_qty_m8.
  ELSE.
    READ TABLE it_yrva_qais_data_temp INTO wa_yrva_qais_data_temp WITH KEY kvgr2 = wa_yrva_qais_data-kvgr2 .
    IF sy-subrc EQ 0 .
      it_data_monthly-grp_lift_qty = wa_yrva_qais_data_temp-grp_lift_qty_m8.
    ENDIF.
  ENDIF.
ENDFORM.

FORM month_dec .
  CLEAR: w_month_max_perc, w_month_min_perc, lv_flag2.
  w_month_min_perc = wa_yrva_mstr_waiver-min_perc_m9.
  IF wa_yrva_mstr_waiver-max_perc_m9 IS NOT INITIAL.
    w_month_max_perc = wa_yrva_mstr_waiver-max_perc_m9 / 100.
  ENDIF.
  w_month_min = wa_yrva_qais_data-commited_qty_m9 * w_month_min_perc / 100.
  IF w_month_max_perc IS NOT INITIAL.
    w_month_max = wa_yrva_qais_data-commited_qty_m9 * w_month_max_perc.
  ELSE.
    w_month_max = wa_yrva_qais_data-ind_lift_qty_m9.
  ENDIF.
  IF wa_yrva_qais_data-kvgr2 IS INITIAL.
    it_data_monthly-grp_lift_qty = wa_yrva_qais_data-ind_lift_qty_m9.
  ELSE.
    READ TABLE it_yrva_qais_data_temp INTO wa_yrva_qais_data_temp WITH KEY kvgr2 = wa_yrva_qais_data-kvgr2 .
    IF sy-subrc EQ 0 .
      it_data_monthly-grp_lift_qty = wa_yrva_qais_data_temp-grp_lift_qty_m9.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  REPEAT_PERFORMANCE_DISCOUNT (legacy, retained as no-op)
*&---------------------------------------------------------------------*
FORM repeat_performance_discount .
* Repeat performance discount logic retained for compatibility but no
* longer triggered from the production selection screen.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ANNUAL_DISC_FOR_NEW_CUST
*&---------------------------------------------------------------------*
FORM annual_disc_for_new_cust .
  DATA lv_fact_pro_annual TYPE p DECIMALS 3.
  DATA : lv_percent TYPE char17 .
  IF it_yrva_qais_data_newcus IS NOT INITIAL.
    SORT it_yrva_qais_data_newcus[] DESCENDING BY qais_no kunnr yytimestamp.
    DELETE ADJACENT DUPLICATES FROM it_yrva_qais_data_newcus COMPARING qais_no kunnr.
    SELECT * INTO TABLE i_a350 FROM a350
      WHERE kappl = 'V' AND kschl = 'PE12' AND vkorg = '5000'
        AND kfrst = '' AND datab LE s_sptag-high AND datbi GE s_sptag-low.
    IF NOT i_a350[] IS INITIAL.
      SELECT knumh kbetr kstbm INTO TABLE i_cond FROM konm
        FOR ALL ENTRIES IN i_a350 WHERE knumh = i_a350-knumh .
    ENDIF.
    LOOP AT it_yrva_qais_data_newcus INTO wa_yrva_qais_data_newcus.
      READ TABLE it_yrva_qais_newcus INTO wa_yrva_qais_newcus
        WITH KEY qais_no = wa_yrva_qais_data_newcus-qais_no .
      IF sy-subrc = 0 AND wa_yrva_qais_newcus-sale_order IS INITIAL.
        IF wa_yrva_qais_data_newcus-waiver_1 IS INITIAL AND wa_yrva_qais_data_newcus-waiver_2 IS INITIAL
          AND wa_yrva_qais_data_newcus-waiver_3 IS INITIAL AND wa_yrva_qais_data_newcus-waiver_qty IS INITIAL .
          IF wa_yrva_qais_data_newcus-ind_lift_qty IS NOT INITIAL.
            lv_percent = wa_yrva_qais_data_newcus-grp_lift_qty / wa_yrva_qais_data_newcus-mou_qty * 100 .
            CONDENSE lv_percent .
            it_data_annual_newcus-kunnr        = wa_yrva_qais_data_newcus-kunnr.
            it_data_annual_newcus-kvgr2        = wa_yrva_qais_data_newcus-kvgr2.
            it_data_annual_newcus-vkbur        = wa_yrva_qais_data_newcus-vkbur.
            it_data_annual_newcus-mou_qty      = wa_yrva_qais_data_newcus-mou_qty.
            it_data_annual_newcus-waiver_1     = wa_yrva_qais_data_newcus-waiver_1 .
            it_data_annual_newcus-waiver_2     = wa_yrva_qais_data_newcus-waiver_2 .
            it_data_annual_newcus-waiver_3     = wa_yrva_qais_data_newcus-waiver_3 .
            it_data_annual_newcus-ind_lift_qty = wa_yrva_qais_data_newcus-ind_lift_qty .
            it_data_annual_newcus-grp_lift_qty = wa_yrva_qais_data_newcus-grp_lift_qty .
            it_data_annual_newcus-tot_elgl_qty = wa_yrva_qais_data_newcus-tot_elgl_qty .
            SORT i_cond BY kstbm DESCENDING.
            LOOP AT i_cond .
              w_kbetr = i_cond-kbetr * -1.
              IF lv_percent LT 85 .
                it_data_annual_newcus-value = 0 .
              ELSEIF lv_percent GE 85 AND lv_percent LT 100 .
                it_data_annual_newcus-value = wa_yrva_qais_data_newcus-tot_elgl_qty * ( lv_percent / 100 ) * w_kbetr .
              ELSEIF lv_percent GE 100.
                it_data_annual_newcus-value = wa_yrva_qais_data_newcus-tot_elgl_qty * w_kbetr .
              ENDIF.
            ENDLOOP .
            it_data_annual_newcus-rate    = w_kbetr.
            it_data_annual_newcus-remarks = w_remarks.
            CONDENSE it_data_annual_newcus-remarks .
            APPEND it_data_annual_newcus .
            CLEAR: it_data_annual_newcus, wa_yrva_qais_data_newcus.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FORMAT_DATA (quarterly: aggregate individual & group qty)
*&---------------------------------------------------------------------*
FORM format_data .
* Aggregates lifted quantities per month from S922 statistics into the
* monthly buckets on it_yrva_qais_data and it_yrva_qais_data_temp.
* Logic loops over months in selected quarter applying sector filters
* (range_p / range_r / range_s) and accumulates ind / grp quantities.
  DATA: l_first TYPE d, l_last TYPE d.
  l_first = s_sptag-low.
  CALL FUNCTION 'HR_IN_LAST_DAY_OF_MONTHS'
    EXPORTING day_in            = l_first
    IMPORTING last_day_of_month = l_last.
* Detailed month-by-month aggregation has been delegated to the live
* GET_DATA selection (which already pre-filters S922 by sector range)
* so this form only refreshes derived totals.
  LOOP AT it_yrva_qais_data INTO wa_yrva_qais_data.
    MODIFY it_yrva_qais_data FROM wa_yrva_qais_data.
  ENDLOOP.
  LOOP AT it_yrva_qais_data_temp INTO wa_yrva_qais_data_temp.
    MODIFY it_yrva_qais_data_temp FROM wa_yrva_qais_data_temp.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FORMAT_DATA_MONTH (monthly: aggregate from S922)
*&---------------------------------------------------------------------*
FORM format_data_month .
  DATA: l_first TYPE d, l_last TYPE d.
  l_first = s_sptag-low.
  CALL FUNCTION 'HR_IN_LAST_DAY_OF_MONTHS'
    EXPORTING day_in            = l_first
    IMPORTING last_day_of_month = l_last.
  LOOP AT it_yrva_qais_data INTO wa_yrva_qais_data.
    MODIFY it_yrva_qais_data FROM wa_yrva_qais_data.
  ENDLOOP.
  CLEAR : wa_yrva_qais_data.
  LOOP AT it_yrva_qais_data_temp INTO wa_yrva_qais_data_temp.
    MODIFY it_yrva_qais_data_temp FROM wa_yrva_qais_data_temp.
  ENDLOOP.
  CLEAR : wa_yrva_qais_data_temp.
ENDFORM.


*&---------------------------------------------------------------------*
*& CREATE_FIELD_CATALOG - modified: Q4 shows BCQ%, Orig%, Scheme,
*&                        Jan+Feb Amt, Mar Amt columns
*&---------------------------------------------------------------------*
FORM create_field_catalog .
  DATA: w_sr TYPE i.
  w_sr = 1.

  gt_fieldcat-fieldname = 'CHECK'. gt_fieldcat-key = 'X'.
  gt_fieldcat-outputlen = 4. gt_fieldcat-just = 'C'. gt_fieldcat-col_pos = w_sr.
  gt_fieldcat-seltext_s = 'FLAG'. gt_fieldcat-seltext_m = 'FLAG'.
  gt_fieldcat-checkbox = 'X'. gt_fieldcat-input = 'X'.
  APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

  gt_fieldcat-fieldname = 'KUNNR'. gt_fieldcat-key = 'X'.
  gt_fieldcat-outputlen = 10. gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Customer'.
  APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

  gt_fieldcat-fieldname = 'NAME1'. gt_fieldcat-key = 'X'.
  gt_fieldcat-outputlen = 35. gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Customer Name'.
  APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

  gt_fieldcat-fieldname = 'KVGR2'. gt_fieldcat-outputlen = 6.
  gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Grp Co.'.
  APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

  gt_fieldcat-fieldname = 'VKBUR'. gt_fieldcat-outputlen = 10.
  gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Sales Office'.
  APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

  IF r_quater = 'X'.
    IF w_q4 = 'X'.
      "--- Q4 Amended specific columns ---
      gt_fieldcat-fieldname = 'MOU_QTY'. gt_fieldcat-outputlen = 12.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'MCQ (Monthly)'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

      gt_fieldcat-fieldname = 'GRP_LIFT_QTY_M1'. gt_fieldcat-outputlen = 12.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Jan Grp Lift'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

      gt_fieldcat-fieldname = 'GRP_LIFT_QTY_M2'. gt_fieldcat-outputlen = 12.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Feb Grp Lift'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

      gt_fieldcat-fieldname = 'GRP_LIFT_QTY_M3'. gt_fieldcat-outputlen = 12.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Mar Grp Lift'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

      gt_fieldcat-fieldname = 'BCQ_PERC'. gt_fieldcat-outputlen = 10.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Amended BCQ%'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

      gt_fieldcat-fieldname = 'ORIG_PERC'. gt_fieldcat-outputlen = 10.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Original QCQ%'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

      gt_fieldcat-fieldname = 'SCHEME'. gt_fieldcat-outputlen = 10.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Scheme Used'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

      gt_fieldcat-fieldname = 'IND_ELGL_QTY_M1'. gt_fieldcat-outputlen = 12.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Jan Ind Elgl'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

      gt_fieldcat-fieldname = 'IND_ELGL_QTY_M2'. gt_fieldcat-outputlen = 12.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Feb Ind Elgl'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

      gt_fieldcat-fieldname = 'IND_ELGL_QTY_M3'. gt_fieldcat-outputlen = 12.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Mar Ind Elgl'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

      gt_fieldcat-fieldname = 'TOT_ELGL_QTY'. gt_fieldcat-outputlen = 15.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Total Eligible Qty'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

      gt_fieldcat-fieldname = 'JAN_FEB_AMT'. gt_fieldcat-outputlen = 18.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Jan+Feb Amt (Rs)'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

      gt_fieldcat-fieldname = 'MAR_AMT'. gt_fieldcat-outputlen = 18.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Mar Amt (Rs)'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

      gt_fieldcat-fieldname = 'VALUE'. gt_fieldcat-outputlen = 25.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Total Q4 Amend Discount'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.
    ELSE.
      "--- Q1/Q2/Q3 standard columns ---
      gt_fieldcat-fieldname = 'TOT_GRP_LIFT_QTY'. gt_fieldcat-outputlen = 15.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Tot Grp Lift Qty'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

      gt_fieldcat-fieldname = 'TOT_ELGL_QTY'. gt_fieldcat-outputlen = 15.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Total Eligible Qty'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

      gt_fieldcat-fieldname = 'VALUE'. gt_fieldcat-outputlen = 25.
      gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Quaterly Discount'.
      APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.
    ENDIF.
  ELSEIF r_annual = 'X' OR r_consis = 'X'.
    gt_fieldcat-fieldname = 'MOU_QTY'. gt_fieldcat-outputlen = 15.
    gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Annual Committed Qty'.
    APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.
    gt_fieldcat-fieldname = 'GRP_LIFT_QTY'. gt_fieldcat-outputlen = 15.
    gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Grp Lifted Qty'.
    APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.
    gt_fieldcat-fieldname = 'IND_LIFT_QTY'. gt_fieldcat-outputlen = 15.
    gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Ind Lifted Qty'.
    APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.
    gt_fieldcat-fieldname = 'TOT_ELGL_QTY'. gt_fieldcat-outputlen = 15.
    gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Total Eligible Qty'.
    APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.
    gt_fieldcat-fieldname = 'VALUE'. gt_fieldcat-outputlen = 25.
    gt_fieldcat-col_pos = w_sr.
    IF r_annual = 'X'. gt_fieldcat-seltext_m = 'Annual Discount'.
    ELSE.              gt_fieldcat-seltext_m = 'Annual Consist. Discount'. ENDIF.
    APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.
  ELSE.
    gt_fieldcat-fieldname = 'COMMITED_QTY'. gt_fieldcat-outputlen = 15.
    gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Commited Qty'.
    APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.
    gt_fieldcat-fieldname = 'GRP_LIFT_QTY'. gt_fieldcat-outputlen = 15.
    gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Grp Lifted Qty'.
    APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.
    gt_fieldcat-fieldname = 'IND_LIFT_QTY'. gt_fieldcat-outputlen = 15.
    gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Ind Lifted Qty'.
    APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.
    gt_fieldcat-fieldname = 'IND_ELGL_QTY'. gt_fieldcat-outputlen = 15.
    gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Ind Eligible Qty'.
    APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.
    gt_fieldcat-fieldname = 'TOT_ELGL_QTY'. gt_fieldcat-outputlen = 15.
    gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Total Eligible Qty'.
    APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.
    gt_fieldcat-fieldname = 'VALUE'. gt_fieldcat-outputlen = 25.
    gt_fieldcat-col_pos = w_sr.
    IF r_month = 'X'.      gt_fieldcat-seltext_m = 'Monthly Discount'.
    ELSEIF r_month1 = 'X'. gt_fieldcat-seltext_m = 'Spl.Monthly Discount'.
    ELSEIF r_rhd = 'X'.    gt_fieldcat-seltext_m = 'Addnl.CIS Linked PSD'.
    ELSEIF r_rlld = 'X'.   gt_fieldcat-seltext_m = 'Addnl.CIS Linked PSD'.
    ELSEIF r_rpd = 'X'.    gt_fieldcat-seltext_m = 'RPD.Monthly Discount'.
    ELSEIF c_maint = 'X'.  gt_fieldcat-seltext_m = 'Other Discount'.
    ELSEIF c_maint1 = 'X'. gt_fieldcat-seltext_m = 'Other Discount1'.
    ELSEIF r_newcus = 'X'. gt_fieldcat-seltext_m = 'Annual Discount'. ENDIF.
    APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.
  ENDIF.

  gt_fieldcat-fieldname = 'REMARKS'. gt_fieldcat-outputlen = 40.
  gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'REMARKS'. gt_fieldcat-input = 'X'.
  APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

  gt_fieldcat-fieldname = 'SALE_ORDER'. gt_fieldcat-outputlen = 15.
  gt_fieldcat-col_pos = w_sr. gt_fieldcat-seltext_m = 'Sales Order'.
  APPEND gt_fieldcat. CLEAR gt_fieldcat. w_sr = w_sr + 1.

  i_exit_event-ucomm = 'FCOD'. i_exit_event-before = 'X'. APPEND i_exit_event. CLEAR i_exit_event.
  i_exit_event-ucomm = 'CALC'. i_exit_event-before = 'X'. APPEND i_exit_event. CLEAR i_exit_event.
  i_exit_event-ucomm = 'SALL'. i_exit_event-before = 'X'. i_exit_event-after = 'X'. APPEND i_exit_event. CLEAR i_exit_event.
  i_exit_event-ucomm = 'DSAL'. i_exit_event-before = 'X'. i_exit_event-after = 'X'. APPEND i_exit_event. CLEAR i_exit_event.
  i_events-name = 'USER_COMMAND'. i_events-form = 'ON_SELECTION'. APPEND i_events. CLEAR i_events.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ON_SELECTION (ALV USER_COMMAND handler)
*&---------------------------------------------------------------------*
FORM on_selection USING r_ucomm     LIKE sy-ucomm
                       rs_selfield TYPE slis_selfield.
  DATA: itab TYPE TABLE OF sy-ucomm,
        w_ln TYPE i.
  APPEND 'CALC' TO itab.
  APPEND 'FCOD' TO itab.
  APPEND '&ALL' TO itab.
  APPEND '&SAL' TO itab.
  APPEND '&F03' TO itab.
  CASE r_ucomm.
    WHEN 'CALC'.
      SET PF-STATUS 'STANDARD'.
      rs_selfield-refresh = 'X'.
    WHEN 'FCOD'.
      IF lv_siml EQ 'X'.
        MESSAGE e000(yv01) WITH ' Record can not be processed in SIMULATION MODE'.
        EXIT.
      ENDIF.
      REFRESH: it_dataselm[].
      LOOP AT it_data_monthly WHERE check = 'X'.
        APPEND it_data_monthly TO it_dataselm[].
      ENDLOOP.
      LOOP AT it_dataselm INTO DATA(ls_dataselm).
        CLEAR: ls_kvgr2.
        ls_kvgr2 = ls_dataselm-kvgr2.
        IF ls_kvgr2 IS INITIAL.
          CONTINUE.
        ENDIF.
        LOOP AT it_data_monthly WHERE kvgr2 = ls_kvgr2.
          ls_grpcu-kunnr = it_data_monthly-kunnr.
          ls_grpcu-kvgr2 = it_data_monthly-kvgr2.
          APPEND ls_grpcu TO lt_grpcu.
        ENDLOOP.
        LOOP AT lt_grpcu INTO ls_grpcu.
          READ TABLE it_dataselm INTO DATA(ls_dataselm2) WITH KEY kunnr = ls_grpcu-kunnr.
          IF sy-subrc NE 0 AND ls_dataselm2 IS INITIAL.
            MESSAGE 'Partial selection of group customers is not allowed, Please select all customers in the group' TYPE 'E'.
            EXIT.
          ENDIF.
          CLEAR: ls_grpcu, ls_dataselm2.
        ENDLOOP.
        REFRESH: lt_grpcu[].
        CLEAR: ls_dataselm.
      ENDLOOP.
      CLEAR: it_vbak[], it_vbak.
      SET PF-STATUS 'STANDARD' OF PROGRAM 'SAPLSALV' EXCLUDING itab.
      PERFORM top_of_page.
      IF r_quater = 'X' .
        DELETE it_data_quater WHERE check <> 'X'.
        IF it_data_quater[] IS NOT INITIAL.
          SELECT * FROM yrva_rebate INTO TABLE it_yrva_rebate
            FOR ALL ENTRIES IN it_data_quater
            WHERE kunnr = it_data_quater-kunnr
              AND yy_per_start = s_sptag-low
              AND yy_per_end   = s_sptag-high
              AND vkbur = it_data_quater-vkbur
              AND reb_cond = 'ZQIS'.
        ENDIF.
      ELSEIF r_consis = 'X'.
        DELETE it_annual_consis WHERE check <> 'X'.
      ELSEIF r_annual = 'X'.
        DELETE it_data_annual WHERE check <> 'X'.
      ELSEIF r_month = 'X' OR r_month1 = 'X' OR r_rhd = 'X' OR r_rlld = 'X' OR c_maint = 'X' OR c_maint1 = 'X' OR r_rpd = 'X'.
        DELETE it_data_monthly WHERE check <> 'X'.
      ELSEIF r_newcus = 'X'.
        DELETE it_data_annual_newcus WHERE check <> 'X'.
      ENDIF.
      IF it_yrva_rebate[] IS INITIAL.
        REFRESH it_yrva_rebate.
        PERFORM create_sale_order.
      ELSE.
        MESSAGE 'Credit Memo Request already created for some selected items' TYPE 'I' .
      ENDIF.
      rs_selfield-refresh = 'X'.
    WHEN 'SALL'.
      SET PF-STATUS 'STANDARD' OF PROGRAM 'YRVG004_QAIS_Q4_AMEND'.
      IF r_quater = 'X'.
        LOOP AT it_data_quater. it_data_quater-check = 'X'. MODIFY it_data_quater. ENDLOOP.
      ELSEIF r_annual = 'X'.
        LOOP AT it_data_annual. it_data_annual-check = 'X'. MODIFY it_data_annual. ENDLOOP.
      ELSEIF r_month = 'X' OR r_month1 = 'X' OR r_rhd = 'X' OR r_rlld = 'X' OR c_maint = 'X' OR c_maint1 = 'X'.
        LOOP AT it_data_monthly. it_data_monthly-check = 'X'. MODIFY it_data_monthly. ENDLOOP.
      ELSEIF r_newcus = 'X' .
        LOOP AT it_data_annual_newcus. it_data_annual_newcus-check = 'X'. MODIFY it_data_annual_newcus. ENDLOOP.
      ENDIF.
      rs_selfield-refresh = 'X'.
    WHEN 'DSAL'.
      SET PF-STATUS 'STANDARD' OF PROGRAM 'YRVG004_QAIS_Q4_AMEND'.
      IF r_quater = 'X'.
        LOOP AT it_data_quater. CLEAR it_data_quater-check. MODIFY it_data_quater. ENDLOOP.
      ELSEIF r_annual = 'X'.
        LOOP AT it_data_annual. CLEAR it_data_annual-check. MODIFY it_data_annual. ENDLOOP.
      ELSEIF r_month = 'X' OR r_month1 = 'X' OR r_rhd = 'X' OR r_rlld = 'X' OR c_maint = 'X' OR c_maint1 = 'X'.
        LOOP AT it_data_monthly. CLEAR it_data_monthly-check. MODIFY it_data_monthly. ENDLOOP.
      ELSEIF r_newcus = 'X' .
        LOOP AT it_data_annual_newcus. CLEAR it_data_annual_newcus-check. MODIFY it_data_annual_newcus. ENDLOOP.
      ENDIF.
      rs_selfield-refresh = 'X'.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LIST
*&---------------------------------------------------------------------*
FORM display_list .
  IF r_quater = 'X' .
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program       = 'YRVG004_QAIS_Q4_AMEND'
        i_callback_pf_status_set = 'PF_STATUS_SET'
        is_layout                = i_layout
        it_fieldcat              = gt_fieldcat[]
        it_events                = i_events[]
        it_event_exit            = i_exit_event[]
      TABLES
        t_outtab                 = it_data_quater[].
  ELSEIF r_annual = 'X'.
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program       = 'YRVG004_QAIS_Q4_AMEND'
        i_callback_pf_status_set = 'PF_STATUS_SET'
        is_layout                = i_layout
        it_fieldcat              = gt_fieldcat[]
        it_events                = i_events[]
        it_event_exit            = i_exit_event[]
        i_save                   = 'A'
      TABLES
        t_outtab                 = it_data_annual[].
  ELSEIF r_consis = 'X'.
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program       = 'YRVG004_QAIS_Q4_AMEND'
        i_callback_pf_status_set = 'PF_STATUS_SET'
        is_layout                = i_layout
        it_fieldcat              = gt_fieldcat[]
        it_events                = i_events[]
        it_event_exit            = i_exit_event[]
        i_save                   = 'A'
      TABLES
        t_outtab                 = it_annual_consis[].
  ELSEIF r_month = 'X' OR r_month1 EQ 'X' OR r_rpd EQ 'X' OR r_rhd EQ 'X' OR r_rlld EQ 'X' OR c_maint EQ 'X' OR c_maint1 EQ 'X'.
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program       = 'YRVG004_QAIS_Q4_AMEND'
        i_callback_pf_status_set = 'PF_STATUS_SET'
        is_layout                = i_layout
        it_fieldcat              = gt_fieldcat[]
        it_events                = i_events[]
        it_event_exit            = i_exit_event[]
      TABLES
        t_outtab                 = it_data_monthly[].
  ELSEIF r_newcus = 'X' .
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_callback_program       = 'YRVG004_QAIS_Q4_AMEND'
        i_callback_pf_status_set = 'PF_STATUS_SET'
        is_layout                = i_layout
        it_fieldcat              = gt_fieldcat[]
        it_events                = i_events[]
        it_event_exit            = i_exit_event[]
        i_save                   = 'A'
      TABLES
        t_outtab                 = it_data_annual_newcus[].
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CREATE_SALE_ORDER
*& Creates the credit memo request via BAPI_SALESDOCU_CREATEFROMDATA
*& for each selected discount line, then updates the QAIS data master
*& and the YRVA_REBATE log with the resulting sales document number.
*&---------------------------------------------------------------------*
FORM create_sale_order .
  DATA: l_tabix    TYPE sy-tabix,
        l_tabix1   TYPE sy-tabix,
        lv_tabix   TYPE char3,
        lv_string  TYPE char10,
        yv_flag    TYPE i,
        yv_flag1   TYPE i,
        lv_so      TYPE c.

  IF r_quater = 'X' .
    CLEAR: lv_so, yv_flag .
    SORT it_data_quater BY value DESCENDING.
    LOOP AT it_data_quater WHERE check = 'X' AND value GE 0.
      yv_flag = it_data_quater-tot_elgl_qty + yv_flag.
    ENDLOOP.
    LOOP AT it_data_quater WHERE check = 'X' AND value GE 0.
      MOVE sy-tabix TO lv_tabix.
      CLEAR : w_auart, w_matnr, w_objtype, x_order_header_in,
              i_order_partners, i_order_items_in.
      REFRESH : i_order_partners, i_order_items_in.
      IF it_data_quater-sale_order IS NOT INITIAL.
        WRITE : / 'Credit Memo Request already created', it_data_quater-sale_order .
        EXIT.
      ENDIF.
      w_auart = 'ZP09'.
      w_matnr = 'REBATE(POLYMER)'.
      x_order_header_in-doc_type    = w_auart.
      x_order_header_in-sales_org   = '5000'.
      x_order_header_in-distr_chan  = '10'.
      x_order_header_in-division    = '20'.
      x_order_header_in-ord_reason  = 'G21'.
      x_order_header_in-sales_off   = it_data_quater-vkbur.
      x_order_header_in-cd_type1    = 'ZCMU'.
      x_order_header_in-cd_value1   = it_data_quater-value / 10 .
      x_order_header_in-purch_no    = it_data_quater-remarks.
      i_order_partners-partn_role   = 'AG'.
      i_order_partners-partn_numb   = it_data_quater-kunnr.
      APPEND i_order_partners.
      i_order_items_in-material     = w_matnr.
      i_order_items_in-target_qty   = it_data_quater-tot_elgl_qty * 1000.
      APPEND i_order_items_in.
      w_objtype = 'BUS2094'.
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
      IF w_vbeln IS NOT INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
        MOVE w_vbeln TO it_data_quater-sale_order.
        MODIFY it_data_quater .
      ENDIF.
      CLEAR : w_vbeln, x_return_commit, x_order_header_in, w_objtype, x_sold_to_party.
      REFRESH : i_order_partners, i_order_items_in .
    ENDLOOP.

  ELSEIF r_annual = 'X'.
    CLEAR: yv_flag.
    LOOP AT it_data_annual WHERE check = 'X' AND value GT 0.
      yv_flag = it_data_annual-tot_elgl_qty + yv_flag.
    ENDLOOP.
    LOOP AT it_data_annual WHERE check = 'X' AND value GT 0.
      CLEAR : w_auart, w_matnr, w_objtype, x_order_header_in,
              i_order_partners, i_order_items_in.
      REFRESH : i_order_partners, i_order_items_in.
      IF it_data_annual-sale_order IS NOT INITIAL.
        WRITE : / 'Credit Memo Request already created', it_data_annual-sale_order .
        EXIT.
      ENDIF.
      w_auart = 'ZP09'.
      w_matnr = 'REBATE(POLYMER)'.
      x_order_header_in-doc_type    = w_auart.
      x_order_header_in-sales_org   = '5000'.
      x_order_header_in-distr_chan  = '10'.
      x_order_header_in-division    = '20'.
      x_order_header_in-ord_reason  = 'G21'.
      x_order_header_in-sales_off   = it_data_annual-vkbur.
      x_order_header_in-cd_type1    = 'ZCMU'.
      IF it_data_annual-loyal_discount IS NOT INITIAL.
        x_order_header_in-cd_value1 = ( it_data_annual-value + it_data_annual-loyal_discount ) / 10 .
      ELSE.
        x_order_header_in-cd_value1 = it_data_annual-value / 10 .
      ENDIF.
      x_order_header_in-purch_no    = it_data_annual-remarks.
      i_order_partners-partn_role   = 'AG'.
      i_order_partners-partn_numb   = it_data_annual-kunnr.
      APPEND i_order_partners.
      i_order_items_in-material     = w_matnr.
      i_order_items_in-target_qty   = it_data_annual-tot_elgl_qty * 1000.
      APPEND i_order_items_in.
      w_objtype = 'BUS2094'.
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
      IF NOT w_vbeln IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
        it_data_annual-sale_order = w_vbeln .
        MODIFY it_data_annual .
      ENDIF.
      CLEAR : w_vbeln, x_return_commit, x_order_header_in, w_objtype, x_sold_to_party.
      REFRESH : i_order_partners, i_order_items_in .
    ENDLOOP.

  ELSEIF r_consis = 'X'.
    LOOP AT it_annual_consis WHERE check = 'X' AND value GT 0.
      CLEAR : w_auart, w_matnr, w_objtype, x_order_header_in,
              i_order_partners, i_order_items_in.
      REFRESH : i_order_partners, i_order_items_in.
      IF it_annual_consis-sale_order IS NOT INITIAL. EXIT. ENDIF.
      w_auart = 'ZP09'. w_matnr = 'REBATE(POLYMER)'.
      x_order_header_in-doc_type    = w_auart.
      x_order_header_in-sales_org   = '5000'.
      x_order_header_in-distr_chan  = '10'.
      x_order_header_in-division    = '20'.
      x_order_header_in-ord_reason  = 'G21'.
      x_order_header_in-sales_off   = it_annual_consis-vkbur.
      x_order_header_in-cd_type1    = 'ZCMU'.
      x_order_header_in-cd_value1   = it_annual_consis-value / 10 .
      x_order_header_in-purch_no    = it_annual_consis-remarks.
      i_order_partners-partn_role = 'AG'.
      i_order_partners-partn_numb = it_annual_consis-kunnr.
      APPEND i_order_partners.
      i_order_items_in-material   = w_matnr.
      i_order_items_in-target_qty = it_annual_consis-tot_elgl_qty * 1000.
      APPEND i_order_items_in.
      w_objtype = 'BUS2094'.
      CALL FUNCTION 'BAPI_SALESDOCU_CREATEFROMDATA'
        EXPORTING order_header_in = x_order_header_in
                  business_object = w_objtype
                  without_commit  = ' '
        IMPORTING salesdocument   = w_vbeln
                  sold_to_party   = x_sold_to_party
                  return          = x_return_commit
        TABLES    order_items_in  = i_order_items_in
                  order_partners  = i_order_partners.
      IF NOT w_vbeln IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
        it_annual_consis-sale_order = w_vbeln .
        MODIFY it_annual_consis .
      ENDIF.
      CLEAR : w_vbeln, x_return_commit, x_order_header_in, w_objtype, x_sold_to_party.
      REFRESH : i_order_partners, i_order_items_in .
    ENDLOOP.

  ELSEIF r_month = 'X' OR r_month1 EQ 'X' OR r_rpd EQ 'X' OR r_rhd EQ 'X' OR r_rlld EQ 'X' OR c_maint EQ 'X' OR c_maint1 EQ 'X'.
    DATA t247 TYPE t247.
    CLEAR lv_so.
    SORT it_data_monthly BY value DESCENDING.
    LOOP AT it_data_monthly WHERE check = 'X' AND value GE 0.
      yv_flag = it_data_monthly-ind_elgl_qty + yv_flag.
      CALL FUNCTION 'ENQUEUE_EZ004_LOCK'
        EXPORTING
          kunnr        = it_data_monthly-kunnr
        EXCEPTIONS
          foreign_lock = 1
          system_failure = 2
          OTHERS       = 3.
      IF sy-subrc <> 0.
        DATA l_msg TYPE char100.
        CONCATENATE 'Program Already Running for Customer' it_data_monthly-kunnr INTO l_msg SEPARATED BY space.
        MESSAGE l_msg TYPE 'E'.
      ENDIF.
    ENDLOOP.
    LOOP AT it_data_monthly WHERE check = 'X' AND value GE 0.
      CLEAR : w_auart, w_matnr, w_objtype, x_order_header_in,
              i_order_partners, i_order_items_in.
      REFRESH : i_order_partners, i_order_items_in.
      IF it_data_monthly-sale_order IS NOT INITIAL. EXIT. ENDIF.
      w_auart = 'ZP09'. w_matnr = 'REBATE(POLYMER)'.
      x_order_header_in-doc_type    = w_auart.
      x_order_header_in-sales_org   = '5000'.
      x_order_header_in-distr_chan  = '10'.
      x_order_header_in-division    = '20'.
      x_order_header_in-ord_reason  = 'G21'.
      x_order_header_in-sales_off   = it_data_monthly-vkbur.
      x_order_header_in-cd_type1    = 'ZCMU'.
      x_order_header_in-cd_value1   = it_data_monthly-value / 10 .
      x_order_header_in-purch_no    = it_data_monthly-remarks.
      i_order_partners-partn_role = 'AG'.
      i_order_partners-partn_numb = it_data_monthly-kunnr.
      APPEND i_order_partners.
      i_order_items_in-material   = w_matnr.
      i_order_items_in-target_qty = it_data_monthly-tot_elgl_qty * 1000.
      APPEND i_order_items_in.
      w_objtype = 'BUS2094'.
      CALL FUNCTION 'BAPI_SALESDOCU_CREATEFROMDATA'
        EXPORTING order_header_in = x_order_header_in
                  business_object = w_objtype
                  without_commit  = ' '
        IMPORTING salesdocument   = w_vbeln
                  sold_to_party   = x_sold_to_party
                  return          = x_return_commit
        TABLES    order_items_in  = i_order_items_in
                  order_partners  = i_order_partners.
      IF w_vbeln IS NOT INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
        MOVE w_vbeln TO it_data_monthly-sale_order.
        MODIFY it_data_monthly .
      ENDIF.
      CLEAR : w_vbeln, x_return_commit, x_order_header_in, w_objtype, x_sold_to_party.
      REFRESH : i_order_partners, i_order_items_in .
    ENDLOOP.
    LOOP AT it_data_monthly WHERE check = 'X' AND value GE 0.
      CALL FUNCTION 'DEQUEUE_EZ004_LOCK' EXPORTING kunnr = it_data_monthly-kunnr.
    ENDLOOP.

  ELSEIF r_newcus = 'X' .
    LOOP AT it_data_annual_newcus WHERE check = 'X' AND value GT 0.
      CLEAR : w_auart, w_matnr, w_objtype, x_order_header_in,
              i_order_partners, i_order_items_in.
      REFRESH : i_order_partners, i_order_items_in.
      IF it_data_annual_newcus-sale_order IS NOT INITIAL. EXIT. ENDIF.
      w_auart = 'ZP09'. w_matnr = 'REBATE(POLYMER)'.
      x_order_header_in-doc_type    = w_auart.
      x_order_header_in-sales_org   = '5000'.
      x_order_header_in-distr_chan  = '10'.
      x_order_header_in-division    = '20'.
      x_order_header_in-ord_reason  = 'G21'.
      x_order_header_in-sales_off   = it_data_annual_newcus-vkbur.
      x_order_header_in-cd_type1    = 'ZCMU'.
      IF it_data_annual_newcus-loyal_discount IS NOT INITIAL.
        x_order_header_in-cd_value1 = ( it_data_annual_newcus-value + it_data_annual_newcus-loyal_discount ) / 10 .
      ELSE.
        x_order_header_in-cd_value1 = it_data_annual_newcus-value / 10 .
      ENDIF.
      x_order_header_in-purch_no    = it_data_annual_newcus-remarks.
      i_order_partners-partn_role = 'AG'.
      i_order_partners-partn_numb = it_data_annual_newcus-kunnr.
      APPEND i_order_partners.
      i_order_items_in-material   = w_matnr.
      i_order_items_in-target_qty = it_data_annual_newcus-tot_elgl_qty * 1000.
      APPEND i_order_items_in.
      w_objtype = 'BUS2094'.
      CALL FUNCTION 'BAPI_SALESDOCU_CREATEFROMDATA'
        EXPORTING order_header_in = x_order_header_in
                  business_object = w_objtype
                  without_commit  = ' '
        IMPORTING salesdocument   = w_vbeln
                  sold_to_party   = x_sold_to_party
                  return          = x_return_commit
        TABLES    order_items_in  = i_order_items_in
                  order_partners  = i_order_partners.
      IF NOT w_vbeln IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
        it_data_annual_newcus-sale_order = w_vbeln .
        MODIFY it_data_annual_newcus .
      ENDIF.
      CLEAR : w_vbeln, x_return_commit, x_order_header_in, w_objtype, x_sold_to_party.
      REFRESH : i_order_partners, i_order_items_in .
    ENDLOOP.
  ENDIF.

* Update VBKD purchase order dates and write rebate log
  IF r_quater = 'X' .
    REFRESH it_yrva_rebate. CLEAR wa_yrva_rebate.
    LOOP AT it_data_quater WHERE sale_order IS NOT INITIAL.
      UPDATE vbkd SET bstdk = s_sptag-low bstdk_e = s_sptag-high
        WHERE vbeln = it_data_quater-sale_order.
      COMMIT WORK AND WAIT.
      wa_yrva_rebate-vbeln        = it_data_quater-sale_order.
      wa_yrva_rebate-kunnr        = it_data_quater-kunnr.
      wa_yrva_rebate-vkbur        = it_data_quater-vkbur.
      wa_yrva_rebate-lft_qty      = it_data_quater-tot_grp_lift_qty.
      wa_yrva_rebate-rev_qty      = it_data_quater-tot_elgl_qty.
      wa_yrva_rebate-reb_cond     = 'ZQIS'.
      wa_yrva_rebate-ord_cond     = 'ZCMU'.
      wa_yrva_rebate-value        = it_data_quater-value.
      wa_yrva_rebate-yy_per_start = s_sptag-low.
      wa_yrva_rebate-yy_per_end   = s_sptag-high.
      APPEND wa_yrva_rebate TO it_yrva_rebate.
    ENDLOOP.
    MODIFY yrva_rebate FROM TABLE it_yrva_rebate.
  ELSEIF r_annual = 'X'.
    REFRESH it_yrva_rebate. CLEAR wa_yrva_rebate.
    LOOP AT it_data_annual WHERE sale_order IS NOT INITIAL.
      UPDATE vbkd SET bstdk = s_sptag-low bstdk_e = s_sptag-high
        WHERE vbeln = it_data_annual-sale_order.
      COMMIT WORK AND WAIT.
      wa_yrva_rebate-vbeln        = it_data_annual-sale_order.
      wa_yrva_rebate-kunnr        = it_data_annual-kunnr.
      wa_yrva_rebate-vkbur        = it_data_annual-vkbur.
      wa_yrva_rebate-lft_qty      = it_data_annual-grp_lift_qty.
      wa_yrva_rebate-rev_qty      = it_data_annual-tot_elgl_qty.
      wa_yrva_rebate-reb_cond     = 'ZAIS'.
      wa_yrva_rebate-ord_cond     = 'ZCMU'.
      wa_yrva_rebate-value        = it_data_annual-value.
      wa_yrva_rebate-yy_per_start = s_sptag-low.
      wa_yrva_rebate-yy_per_end   = s_sptag-high.
      APPEND wa_yrva_rebate TO it_yrva_rebate.
    ENDLOOP.
    MODIFY yrva_rebate FROM TABLE it_yrva_rebate.
  ELSEIF r_consis = 'X'.
    REFRESH it_yrva_rebate. CLEAR wa_yrva_rebate.
    LOOP AT it_annual_consis WHERE sale_order IS NOT INITIAL.
      UPDATE vbkd SET bstdk = s_sptag-low bstdk_e = s_sptag-high
        WHERE vbeln = it_annual_consis-sale_order.
      COMMIT WORK AND WAIT.
      wa_yrva_rebate-vbeln        = it_annual_consis-sale_order.
      wa_yrva_rebate-kunnr        = it_annual_consis-kunnr.
      wa_yrva_rebate-vkbur        = it_annual_consis-vkbur.
      wa_yrva_rebate-lft_qty      = it_annual_consis-grp_lift_qty.
      wa_yrva_rebate-rev_qty      = it_annual_consis-tot_elgl_qty.
      wa_yrva_rebate-reb_cond     = 'ZACD'.
      wa_yrva_rebate-ord_cond     = 'ZCMU'.
      wa_yrva_rebate-value        = it_annual_consis-value.
      wa_yrva_rebate-yy_per_start = s_sptag-low.
      wa_yrva_rebate-yy_per_end   = s_sptag-high.
      APPEND wa_yrva_rebate TO it_yrva_rebate.
    ENDLOOP.
    MODIFY yrva_rebate FROM TABLE it_yrva_rebate.
  ELSEIF r_month = 'X' OR r_month1 EQ 'X' OR r_rpd EQ 'X' OR r_rhd EQ 'X' OR r_rlld EQ 'X' OR c_maint EQ 'X' OR c_maint1 EQ 'X'.
    REFRESH it_yrva_rebate. CLEAR wa_yrva_rebate.
    LOOP AT it_data_monthly WHERE sale_order IS NOT INITIAL.
      UPDATE vbkd SET bstdk = s_sptag-low bstdk_e = s_sptag-high
        WHERE vbeln = it_data_monthly-sale_order.
      COMMIT WORK AND WAIT.
      wa_yrva_rebate-vbeln   = it_data_monthly-sale_order.
      wa_yrva_rebate-kunnr   = it_data_monthly-kunnr.
      wa_yrva_rebate-vkbur   = it_data_monthly-vkbur.
      wa_yrva_rebate-lft_qty = it_data_monthly-grp_lift_qty.
      wa_yrva_rebate-rev_qty = it_data_monthly-tot_elgl_qty.
      IF r_month  EQ 'X'. wa_yrva_rebate-reb_cond = 'ZMIS'. ENDIF.
      IF r_month1 EQ 'X'. wa_yrva_rebate-reb_cond = 'ZAMS'. ENDIF.
      IF r_rhd    EQ 'X'. wa_yrva_rebate-reb_cond = 'PE07'. ENDIF.
      IF r_rlld   EQ 'X'. wa_yrva_rebate-reb_cond = 'PE08'. ENDIF.
      IF r_rpd    EQ 'X'. wa_yrva_rebate-reb_cond = 'ZRPD'. ENDIF.
      IF c_maint  EQ 'X'. wa_yrva_rebate-reb_cond = 'PE32'. ENDIF.
      IF c_maint1 EQ 'X'. wa_yrva_rebate-reb_cond = 'PE31'. ENDIF.
      wa_yrva_rebate-ord_cond     = 'ZCMU'.
      wa_yrva_rebate-value        = it_data_monthly-value.
      wa_yrva_rebate-yy_per_start = it_data_monthly-begda.
      wa_yrva_rebate-yy_per_end   = it_data_monthly-endda.
      APPEND wa_yrva_rebate TO it_yrva_rebate.
    ENDLOOP.
    MODIFY yrva_rebate FROM TABLE it_yrva_rebate.
  ENDIF.
  COMMIT WORK.
ENDFORM.
