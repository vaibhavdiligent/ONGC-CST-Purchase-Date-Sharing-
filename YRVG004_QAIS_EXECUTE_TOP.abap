*&---------------------------------------------------------------------*
*& Include          YRVG004_QAIS_EXECUTE_TOP
*&---------------------------------------------------------------------*
TABLES: yrva_qais_data, s922, yrva_qais_upd.
*TYPES-----------------------------------------------------------------*
TYPE-POOLS : slis.

*INTERNAL TABLES FOR ALV-----------------------------------------------*
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.

DATA : i_layout     TYPE slis_layout_alv,
       i_exit_event TYPE slis_t_event_exit WITH HEADER LINE.

DATA: i_events TYPE slis_t_event WITH HEADER LINE.

* PROGRAM RELATED DATA
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

      BEGIN OF it_kunnr OCCURS 0 ,
        kvgr2 TYPE kvgr2,
        kunnr TYPE kunnr,
      END OF it_kunnr ,

      wa_kunnr LIKE LINE OF it_kunnr,
      w_kunag  LIKE vbrk-kunag.

DATA: BEGIN OF it_data_quater OCCURS 0 ,
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
        value            TYPE kbetr,
        remarks(30),
        sale_order       TYPE vbeln,
        check(1),
      END OF it_data_quater.

DATA: BEGIN OF it_vbak OCCURS 0 ,
        vbeln TYPE vbeln,
      END OF it_vbak.

DATA: BEGIN OF it_data_annual OCCURS 0 ,
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
      END OF it_data_annual .

DATA: BEGIN OF it_annual_consis OCCURS 0 ,
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
      END OF it_annual_consis .

DATA: BEGIN OF it_data_annual_newcus OCCURS 0 ,
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
      END OF it_data_annual_newcus .

DATA: BEGIN OF it_data_monthly OCCURS 0 ,
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

DATA: BEGIN OF it_kna1 OCCURS 0 ,
        kunnr TYPE kunnr,
        name1 TYPE name1_gp,
      END OF it_kna1 .

DATA : BEGIN OF i_cond OCCURS 0,
         knumh LIKE konm-knumh,
         kbetr LIKE konm-kbetr,
         kstbm LIKE konm-kstbm,
       END OF i_cond.
*    sales order related fields
DATA : x_order_header_in LIKE bapisdhead,
       x_return_commit   LIKE bapireturn1,
       x_sold_to_party   LIKE bapisoldto,
       x_bapisdhd1       LIKE bapisdhd1.

DATA : i_order_partners LIKE bapipartnr OCCURS 0 WITH HEADER LINE,
       i_order_items_in LIKE bapiitemin OCCURS 0 WITH HEADER LINE.

DATA: w_q1(1),
      w_q2(2),
      w_q3(1),
      w_q4(1),
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
      w_flag_month3(1) .
DATA : i_a350 LIKE a350 OCCURS 0.

DATA: w_90_mou_qty TYPE p DECIMALS 3.
DATA : w_kbetr      LIKE konm-kbetr,
       w_vbeln      LIKE bapivbeln-vbeln,
       w_objtype    LIKE bapiusw01-objtype,
       w_auart      LIKE bapisdhead-doc_type,
       w_matnr      LIKE mara-matnr,
       w_fiscal(10),
       w_fiscal1(10),
       w_date_limit TYPE d.

DATA : lv_q         TYPE yquarter,
       lv_qmonth    TYPE yy_month1,
       lv_date1     TYPE sptag,
       lv_siml      TYPE zflag,
       lv_elgl_qty  TYPE p DECIMALS 3,
       lv_m1        TYPE p DECIMALS 3,
       lv_m2        TYPE p DECIMALS 3,
       lv_m3        TYPE p DECIMALS 3.

*DECLARATION FOR ADDITIONAL WAIVER FOR SHORTFALL GRADE ON 01.08.2014
DATA: w_waive_month_1(1), w_waive_month_2(1), w_waive_month_3(1), w_waive_month(1),
      wa_yrva_qais_add_wv TYPE yrva_qais_add_wv,
      wa_yrva_qais_qtr_wv TYPE yrva_qais_qtr_wv,
      it_yrva_qais_qtr_wv TYPE STANDARD TABLE OF yrva_qais_qtr_wv,
      it_yrva_qais_add_wv TYPE STANDARD TABLE OF yrva_qais_add_wv.

DATA : lv_revival_m(1),
       lv_revival_q(1),
       lv_revival_a(1).
DATA : it_yrva_revival_fai TYPE STANDARD TABLE OF yrva_revival_fai,
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
