*&---------------------------------------------------------------------*
*& Report  ZPRA_DPR_REPORT
*&
*&---------------------------------------------------------------------*
*& Daily Production Report (DPR) - Single flat program without includes
*& VERSION : 1.6  |  Git: dly_rprd-PI-CS |  Date: 05-MAY-2026
*& Changes : Fix sec3f Sakhalin-1 historical oil — apply PI in dly_rprd
*&           path (ovl_prd_vl_qty1 stores JV for SK1 historical years).
*&           Broaden asset match to CS 'SK' in case code differs from RUS_SK1.
*&           Also: gas PI in dly_prd, CF lookup from zpra_t_tar_cf.
*&---------------------------------------------------------------------*
REPORT ZPRA_DPR_REPORT.

*--- Data Declarations, Types, Field-Symbols, Selection Screen ---*
*&---------------------------------------------------------------------*
CONSTANTS : c_prod_oil TYPE zpra_t_dly_prd-product VALUE  '722000001',
            c_prod_con TYPE zpra_t_dly_prd-product VALUE  '722000003',
            c_prod_gas TYPE zpra_t_dly_prd-product VALUE  '722000004',
            c_prod_lng TYPE zpra_t_dly_prd-product VALUE  '722000005'.

CONSTANTS : c_oil_desc TYPE char20                 VALUE  'Oil',
            c_con_desc TYPE char20                 VALUE  'Condensate',
            c_gas_desc TYPE char20                 VALUE  'Gas',
            c_lng_desc TYPE char20                 VALUE  'LNG'.


TYPES : BEGIN OF ty_zpra_t_prd_pi  ,
         asset          TYPE zpra_t_prd_pi-asset            ,
         block          TYPE zpra_t_prd_pi-block            ,
         vld_frm        TYPE zpra_t_prd_pi-vld_frm          ,
         vld_to         TYPE zpra_t_prd_pi-vld_to           ,
         pi             TYPE zpra_t_prd_pi-pi               ,
         prod_start_date TYPE zpra_t_prd_pi-prod_start_date ,
        END OF ty_zpra_t_prd_pi ,
        BEGIN OF ty_zpra_t_mrec_prd ,
         gjahr          TYPE zpra_t_mrec_prd-gjahr          ,
         monat          TYPE zpra_t_mrec_prd-monat          ,
         asset          TYPE zpra_t_mrec_prd-asset          ,
         block          TYPE zpra_t_mrec_prd-block          ,
         product        TYPE zpra_t_mrec_prd-product        ,
         prd_vl_type    TYPE zpra_t_mrec_prd-prd_vl_type    ,
         prod_vl_qty1   TYPE zpra_t_mrec_prd-prod_vl_qty1   ,
         prod_vl_uom1   TYPE zpra_t_mrec_prd-prod_vl_uom1   ,
         prod_vl_qty2   TYPE zpra_t_mrec_prd-prod_vl_qty2   ,
         prod_vl_uom2   TYPE zpra_t_mrec_prd-prod_vl_uom2   ,
        END OF ty_zpra_t_mrec_prd ,
        BEGIN OF ty_wtd_pi ,
         production_date TYPE zpra_t_dly_prd-production_date,
         product        TYPE zpra_t_dly_prd-product         ,
         asset          TYPE zpra_t_prd_pi-asset            ,
         pi             TYPE p LENGTH 12  DECIMALS 9        ,
         numerator      TYPE zpra_t_dly_prd-prod_vl_qty1    ,
         denominator    TYPE zpra_t_dly_prd-prod_vl_qty1    ,
        END OF   ty_wtd_pi ,
        BEGIN OF ty_wtd_cf ,
         product        TYPE zpra_t_mrec_prd-product         ,
         asset          TYPE zpra_t_mrec_prd-asset           ,
         cf          TYPE zpra_t_mrec_prd-prod_vl_qty1    ,
        END OF   ty_wtd_cf ,
        BEGIN OF ty_asset_desc ,
          asset         TYPE zoiu_pr_dn-dn_no ,
          desc          TYPE zoiu_pr_dn-dn_de ,
        END OF   ty_asset_desc ,
        BEGIN OF ty_paste ,
         lv_data(7000) TYPE c,
        END OF ty_paste ,
        BEGIN OF ty_copied_cells,
          row TYPE sy-tabix ,
          col TYPE sy-tabix ,
        END OF   ty_copied_cells ,
        BEGIN OF ty_product_col ,
           product TYPE char50 ,
           s_col   TYPE sy-tabix,
           e_col   TYPE sy-tabix,
           desc    TYPE char50  ,
        END OF   ty_product_col ,
        BEGIN OF ty_zpra_t_tar_pi ,
          asset       TYPE zpra_t_tar_pi-asset ,
          block       TYPE zpra_t_tar_pi-block ,
          tar_code    TYPE zpra_t_tar_pi-tar_code ,
          vld_frm     TYPE zpra_t_tar_pi-vld_frm ,
          vld_to      TYPE zpra_t_tar_pi-vld_to ,
          pi          TYPE zpra_t_tar_pi-pi ,
        END OF   ty_zpra_t_tar_pi ,
        BEGIN OF ty_zpra_t_prd_tar,
          tar_code TYPE zpra_t_prd_tar-tar_code ,
          gjahr TYPE zpra_t_prd_tar-gjahr ,
          monat TYPE zpra_t_prd_tar-monat ,
          asset TYPE zpra_t_prd_tar-asset ,
          block TYPE zpra_t_prd_tar-block ,
          product TYPE zpra_t_prd_tar-product ,
          prod_vl_type_cd TYPE zpra_t_prd_tar-prod_vl_type_cd ,
*          tar_qty TYPE zpra_t_prd_tar-tar_qty ,
          tar_qty TYPE p LENGTH 16  DECIMALS 9 ,
          uom     TYPE zpra_t_prd_tar-uom    ,
          tar_qty2 TYPE p LENGTH 16 DECIMALS 9  ,
        END   OF ty_zpra_t_prd_tar,
        BEGIN OF ty_zpra_c_prd_prof ,
          product TYPE zpra_c_prd_prof-product ,
          asset   TYPE zpra_c_prd_prof-asset ,
          block   TYPE zpra_c_prd_prof-block ,
        END   OF ty_zpra_c_prd_prof ,
        BEGIN OF ty_zpra_t_mrec_app ,
          gjahr                     TYPE zpra_t_mrec_app-gjahr ,
          monat                     TYPE zpra_t_mrec_app-monat ,
          asset                     TYPE zpra_t_mrec_app-asset ,
          block                     TYPE zpra_t_mrec_app-block ,
          product                   TYPE zpra_t_mrec_app-product ,
          prd_vl_type               TYPE zpra_t_mrec_app-prd_vl_type ,
          app_vl_qty                TYPE zpra_t_mrec_app-app_vl_qty ,
          app_vl_uom                TYPE zpra_t_mrec_app-app_vl_uom ,
        END   OF ty_zpra_t_mrec_app ,
        BEGIN OF ty_gas_combine     ,
          asset TYPE zpra_t_mrec_app-asset ,
        END OF   ty_gas_combine     ,
        BEGIN OF ty_prod_start_end_dates ,
          asset TYPE zpra_t_prd_pi-asset ,
          block TYPE zpra_t_prd_pi-block ,
          vld_frm TYPE zpra_t_prd_pi-vld_frm ,
          vld_to TYPE zpra_t_prd_pi-vld_to ,
          prod_start_date TYPE zpra_t_prd_pi-prod_start_date ,
          liscense_exp_dt TYPE zpra_t_prd_pi-liscense_exp_dt ,
        END   OF ty_prod_start_end_dates ,
        BEGIN OF ty_tar_start_dates ,
          asset TYPE zpra_t_tar_pi-asset ,
          tar_code TYPE zpra_t_tar_pi-tar_code ,
          vld_frm  TYPE zpra_t_tar_pi-vld_frm ,
        END   OF ty_tar_start_dates .

DATA : gt_zpra_t_dly_prd            TYPE STANDARD TABLE OF zpra_t_dly_prd ,
       gt_zpra_t_dly_prd2           TYPE STANDARD TABLE OF zpra_t_dly_prd ,
       gt_zpra_t_dly_prd_mb         TYPE STANDARD TABLE OF zpra_t_dly_prd ,
       gt_zpra_t_dly_prd_2a3        TYPE STANDARD TABLE OF zpra_t_dly_prd ,
       gt_zpra_t_dly_prd_2d         TYPE STANDARD TABLE OF zpra_t_dly_prd ,
       gt_zpra_t_dly_prd_2f         TYPE STANDARD TABLE OF zpra_t_dly_prd ,
       gt_zpra_t_dly_prd_3c         TYPE STANDARD TABLE OF zpra_t_dly_prd ,
       gt_zpra_t_dly_rprd_2d        TYPE STANDARD TABLE OF zpra_t_dly_rprd,
       gt_zpra_t_dly_rprd_2f        TYPE STANDARD TABLE OF zpra_t_dly_rprd,
       gt_zpra_t_dly_rprd_3c        TYPE STANDARD TABLE OF zpra_t_dly_rprd,
       gt_zpra_t_dly_prd_3f         TYPE STANDARD TABLE OF zpra_t_dly_prd ,
       gt_zpra_t_dly_prd_nd         TYPE STANDARD TABLE OF zpra_t_dly_prd ,
       gt_zpra_t_dly_rprd_3f        TYPE STANDARD TABLE OF zpra_t_dly_rprd,
       gt_zpra_t_dly_prd_4a         TYPE STANDARD TABLE OF zpra_t_dly_prd ,
       gt_zpra_t_dly_rprd_4a        TYPE STANDARD TABLE OF zpra_t_dly_rprd ,
       gt_zpra_t_dly_prd_5a         TYPE STANDARD TABLE OF zpra_t_dly_prd ,
       gt_zdpr_gas_combine          TYPE STANDARD TABLE OF ty_gas_combine ,
       gt_zpra_t_prd_pi             TYPE STANDARD TABLE OF ty_zpra_t_prd_pi   ,
       gt_zpra_t_prd_pi_mb          TYPE STANDARD TABLE OF ty_zpra_t_prd_pi   ,
       gt_zpra_t_prd_pi_lm          TYPE STANDARD TABLE OF ty_zpra_t_prd_pi   ,
       gt_zpra_t_prd_pi_2d          TYPE STANDARD TABLE OF ty_zpra_t_prd_pi   ,
       gt_zpra_t_prd_pi_2f          TYPE STANDARD TABLE OF ty_zpra_t_prd_pi   ,
       gt_zpra_t_prd_pi_3c          TYPE STANDARD TABLE OF ty_zpra_t_prd_pi   ,
       gt_zpra_t_prd_pi_3f          TYPE STANDARD TABLE OF ty_zpra_t_prd_pi   ,
       gt_zpra_t_prd_pi_4a          TYPE STANDARD TABLE OF ty_zpra_t_prd_pi   ,
       gt_zpra_t_prd_pi_5a          TYPE STANDARD TABLE OF ty_zpra_t_prd_pi   ,
       gt_zpra_t_mrec_prd           TYPE STANDARD TABLE OF ty_zpra_t_mrec_prd ,
       gt_zpra_t_mrec_prd_2a3       TYPE STANDARD TABLE OF ty_zpra_t_mrec_prd ,
       gt_zpra_t_mrec_prd_2d        TYPE STANDARD TABLE OF ty_zpra_t_mrec_prd ,
       gt_zpra_t_mrec_prd_2f        TYPE STANDARD TABLE OF ty_zpra_t_mrec_prd ,
       gt_zpra_t_mrec_prd_3c        TYPE STANDARD TABLE OF ty_zpra_t_mrec_prd ,
       gt_zpra_t_tar_cf_3c          TYPE STANDARD TABLE OF zpra_t_tar_cf      ,
       gt_zpra_t_mrec_prd_3f        TYPE STANDARD TABLE OF ty_zpra_t_mrec_prd ,
       gt_zpra_t_mrec_prd_4a        TYPE STANDARD TABLE OF ty_zpra_t_mrec_prd ,
       gt_zpra_t_mrec_prd_5a        TYPE STANDARD TABLE OF ty_zpra_t_mrec_prd ,
       gt_wtd_pi                    TYPE STANDARD TABLE OF ty_wtd_pi       ,
       gt_wtd_cf                    TYPE STANDARD TABLE OF ty_wtd_cf    ,
       gt_asset_desc                TYPE STANDARD TABLE OF ty_asset_desc ,
       gt_paste                     TYPE STANDARD TABLE OF ty_paste       ,
       gt_copied_cells              TYPE STANDARD TABLE OF ty_copied_cells  ,
       gt_product_col               TYPE STANDARD TABLE OF ty_product_col ,
       gt_zpra_t_tar_pi             TYPE STANDARD TABLE OF ty_zpra_t_tar_pi ,
       gt_zpra_t_prd_tar            TYPE STANDARD TABLE OF ty_zpra_t_prd_tar  ,
       gt_zpra_t_prd_tar_2c         TYPE STANDARD TABLE OF ty_zpra_t_prd_tar  ,
       gt_zpra_t_prd_tar_3a         TYPE STANDARD TABLE OF ty_zpra_t_prd_tar  ,
       gt_zpra_t_prd_tar_3b         TYPE STANDARD TABLE OF ty_zpra_t_prd_tar  ,
       gt_zpra_t_prd_tar_5a         TYPE STANDARD TABLE OF ty_zpra_t_prd_tar  ,
       gt_zpra_t_tar_cf             TYPE STANDARD TABLE OF zpra_t_tar_cf      ,
       gt_cf                        TYPE STANDARD TABLE OF zpra_t_tar_cf      ,
       gt_zpra_t_tar_cf_5a          TYPE STANDARD TABLE OF zpra_t_tar_cf      ,
       gt_zpra_c_prd_prof           TYPE STANDARD TABLE OF ty_zpra_c_prd_prof ,
       gt_zpra_c_dpr_prof           TYPE STANDARD TABLE OF zpra_c_dpr_prof ,
       gt_zpra_t_mrec_app           TYPE STANDARD TABLE OF ty_zpra_t_mrec_app ,
       gt_zpra_t_mrec_app_2a3       TYPE STANDARD TABLE OF ty_zpra_t_mrec_app ,
       gt_zpra_t_mrec_app_2d        TYPE STANDARD TABLE OF ty_zpra_t_mrec_app ,
       gt_zpra_t_mrec_app_2f        TYPE STANDARD TABLE OF ty_zpra_t_mrec_app ,
       gt_zpra_t_mrec_app_3f        TYPE STANDARD TABLE OF ty_zpra_t_mrec_app ,
       gt_zpra_t_mrec_app_4a        TYPE STANDARD TABLE OF ty_zpra_t_mrec_app ,
       gt_prod_start_end_dates      TYPE STANDARD TABLE OF ty_prod_start_end_dates ,
       gt_tar_start_dates           TYPE STANDARD TABLE OF ty_tar_start_dates .

DATA : gs_zpra_t_dly_prd            TYPE                   zpra_t_dly_prd ,
       gs_zpra_t_dly_prd_nd         TYPE                   zpra_t_dly_prd ,
       gs_zpra_t_dly_rprd           TYPE                   zpra_t_dly_rprd,
       gs_zdpr_gas_combine          TYPE                   ty_gas_combine ,
       gs_zpra_t_prd_pi             TYPE                   ty_zpra_t_prd_pi ,
       gs_zpra_t_mrec_prd           TYPE                   ty_zpra_t_mrec_prd ,
       gs_wtd_pi                    TYPE                   ty_wtd_pi        ,
       gs_wtd_cf                    TYPE                   ty_wtd_cf     ,
       gs_asset_desc                TYPE                   ty_asset_desc  ,
       gs_paste                     TYPE                   ty_paste         ,
       gs_copied_cells              TYPE                   ty_copied_cells  ,
       gs_product_col               TYPE                   ty_product_col ,
       gs_zpra_t_tar_pi             TYPE                   ty_zpra_t_tar_pi ,
       gs_zpra_t_prd_tar            TYPE                   ty_zpra_t_prd_tar ,
       gs_zpra_t_tar_cf             TYPE                   zpra_t_tar_cf ,
       gs_cf                        TYPE                   zpra_t_tar_cf ,
       gs_zpra_c_prd_prof           TYPE                   ty_zpra_c_prd_prof ,
       gs_zpra_c_dpr_prof           TYPE                   zpra_c_dpr_prof ,
       gs_zpra_t_mrec_app           TYPE                   ty_zpra_t_mrec_app ,
       gs_zpra_t_tar_cf_3c          TYPE                   zpra_t_tar_cf      ,
       gs_prod_start_end_dates      TYPE                   ty_prod_start_end_dates ,
       gs_tar_start_dates           TYPE                   ty_tar_start_dates .

RANGES : r_product                  FOR gs_zpra_t_dly_prd-product     ,
         r_prd_vl_type              FOR gs_zpra_t_dly_prd-prd_vl_type ,
         r_combine_asset            FOR gs_zdpr_gas_combine-asset     ,
         r_tar_code                 FOR gs_zpra_t_tar_pi-tar_code     ,
         r_production_date          FOR gs_zpra_t_dly_prd-production_date .


* Dynamic Table Declarations


DATA : gt_dyn_table                 TYPE REF TO data,
       gt_sec2a1_table              TYPE REF TO data,
       gt_sec2a2_table              TYPE REF TO data,
       gt_sec2a3_table              TYPE REF TO data,
       gt_sec2b_table               TYPE REF TO data,
       gt_sec2c_table               TYPE REF TO data,
       gt_sec2d_table               TYPE REF TO data,
       gt_sec2e_table               TYPE REF TO data,
       gt_sec2f_table               TYPE REF TO data,
       gt_sec3a_table               TYPE REF TO data,
       gt_sec3b_table               TYPE REF TO data,
       gt_sec3c_table               TYPE REF TO data,
       gt_sec3d_table               TYPE REF TO data,
       gt_sec3e_table               TYPE REF TO data,
       gt_sec3f_table               TYPE REF TO data,
       gt_sec4a_table               TYPE REF TO data,
       gt_sec5a_table               TYPE REF TO data,
       gt_sec6_table                TYPE REF TO data,
       gs_line                      TYPE REF TO data,
       gs_line1                     TYPE REF TO data,
       gs_dyn_fcat                  TYPE lvc_s_fcat,
       gt_dyn_fcat                  TYPE lvc_t_fcat.

DATA : gv_pos                       TYPE i ,
       gv_len                       TYPE i ,
       gv_table_columns             TYPE sy-tabix ,
       gv_year_start_date           TYPE sy-datum ,
       gv_year_end_date             TYPE sy-datum ,
       gv_current_gjahr_days        TYPE sy-tabix ,
       gv_month_days_till_today     TYPE sy-tabix ,
       gv_current_gjahr             TYPE zpra_t_mrec_prd-gjahr ,
       gv_current_calendar_gjahr    TYPE zpra_t_mrec_prd-gjahr ,
       gv_next_gjahr                TYPE zpra_t_mrec_prd-gjahr ,
       gv_last_gjahr                TYPE zpra_t_mrec_prd-gjahr ,
       gv_5_back_gjahr              TYPE zpra_t_mrec_prd-gjahr ,
       gv_current_monat             TYPE zpra_t_mrec_prd-monat ,
       gv_last_gjahr_days           TYPE sy-tabix ,
       gv_month_name                TYPE t247-ltx ,
       gv_last_month_name           TYPE t247-ltx ,
       gv_current_month_days        TYPE sy-tabix ,
       gv_month_back_datum          TYPE sy-datum ,
       gv_month_back_begin_datum    TYPE sy-datum ,
       gv_search_begin_datum        TYPE sy-datum ,
       gv_month_back_end_datum      TYPE sy-datum ,
       gv_search_end_datum          TYPE sy-datum ,
       gv_month_back_gjahr          TYPE gjahr ,
       gv_month_back_calendar_gjahr TYPE gjahr ,
       gv_month_back_monat          TYPE zpra_t_mrec_prd-monat ,
       gv_month_begin_datum         TYPE sy-datum ,
       gv_month_end_datum           TYPE sy-datum ,
       gv_year_back_begin_datum     TYPE sy-datum ,
       gv_year_back_end_datum       TYPE sy-datum ,
       gv_total_days_in_year        TYPE sy-tabix  ,
       gv_days_gone_in_year         TYPE sy-tabix ,
       gv_last_year_s_date          TYPE sy-datum ,
       gv_last_year_t_date          TYPE sy-datum ,
       gv_days_gone_in_last_year    TYPE sy-tabix ,
       gv_days_left_in_year         TYPE sy-tabix  ,
       gv_mrec_monat_start          TYPE zpra_t_mrec_prd-monat ,
       gv_mrec_monat_end            TYPE zpra_t_mrec_prd-monat ,
       gv_mrec_gjahr_start          TYPE zpra_t_mrec_prd-gjahr ,
       gv_mrec_gjahr_end            TYPE zpra_t_mrec_prd-gjahr ,
       gv_individual_mul            TYPE char10 ,
       gv_total_mul                 TYPE char10 ,
       gv_grand_total_mul           TYPE char10 ,
       gv_periv                     TYPE t001-periv ,
       gv_grand_total_desc          TYPE char100 ,
       gv_5a_cols                   TYPE sy-tabix ,
       gv_5a_rows                   TYPE sy-tabix ,
       gv_image_name                TYPE rlgrap-filename .

DATA : gv_header_colour             TYPE sy-tabix VALUE 10092543 ,
       gv_dates_colour              TYPE sy-tabix VALUE 10079487 ,
       gv_header_oil_colour         TYPE sy-tabix VALUE 8838836  ,
       gv_header_cond_colour        TYPE sy-tabix VALUE 16772019 ,
       gv_header_lng_colour         TYPE sy-tabix VALUE 5296274  ,
       gv_header_gas_colour         TYPE sy-tabix VALUE 7199999  ,
       gv_header_gt_colour          TYPE sy-tabix VALUE 14851781 ,
       gv_sec2a_tgt_colour          TYPE sy-tabix VALUE  6724095 ,
       gv_sec2b_colour              TYPE sy-tabix VALUE 12171181 ,
       gv_sec2c_colour              TYPE sy-tabix VALUE 16751001 ,
       gv_sec2a2_colour             TYPE sy-tabix VALUE  3381759 ,
       gv_sec2a3_colour             TYPE sy-tabix VALUE 10079487 ,
       gv_sec2d_colour              TYPE sy-tabix VALUE 10092441 ,
       gv_sec2e_colour              TYPE sy-tabix VALUE 16764057 ,
       gv_sec2f_colour              TYPE sy-tabix VALUE 11711154 ,
       gv_sec3h_colour              TYPE sy-tabix VALUE 13434879 ,
       gv_sec3a_colour              TYPE sy-tabix VALUE 13421823 ,
       gv_sec3b_colour              TYPE sy-tabix VALUE 16772300 ,
       gv_sec3c_colour              TYPE sy-tabix VALUE 9293736  ,
       gv_sec3d_colour              TYPE sy-tabix VALUE 16772300 ,
       gv_sec3e_colour              TYPE sy-tabix VALUE 11711154 ,
       gv_sec3f_colour              TYPE sy-tabix VALUE 16701082 ,
       gv_colour                    TYPE sy-tabix                ,
       gv_rep_date                  TYPE sy-datum                ,
       gv_repdate_e                 TYPE char20                  .

FIELD-SYMBOLS: <gfs_dyn_table>      TYPE STANDARD TABLE,
               <gfs_sec2_table>     TYPE STANDARD TABLE,
               <gfs_sec2a2_table>   TYPE STANDARD TABLE,
               <gfs_sec2a3_table>   TYPE STANDARD TABLE,
               <gfs_sec2b_table>    TYPE STANDARD TABLE,
               <gfs_sec2c_table>    TYPE STANDARD TABLE,
               <gfs_sec2d_table>    TYPE STANDARD TABLE,
               <gfs_sec2e_table>    TYPE STANDARD TABLE,
               <gfs_sec2f_table>    TYPE STANDARD TABLE,
               <gfs_sec3a_table>    TYPE STANDARD TABLE,
               <gfs_sec3b_table>    TYPE STANDARD TABLE,
               <gfs_sec3c_table>    TYPE STANDARD TABLE,
               <gfs_sec3d_table>    TYPE STANDARD TABLE,
               <gfs_sec3e_table>    TYPE STANDARD TABLE,
               <gfs_sec3f_table>    TYPE STANDARD TABLE,
               <gfs_sec4a_table>    TYPE STANDARD TABLE,
               <gfs_sec5a_table>    TYPE STANDARD TABLE,
               <gfs_sec6_table>     TYPE STANDARD TABLE,
               <gfs_dyn_line>  ,
               <gfs_dyn_line1> ,
               <gfs_dyn_line2>  ,
               <gfs_dyn_line3>  ,
               <gfs_field>      ,
               <gfs_field2>     ,
               <gfs_field3>     .

DATA: go_excel                      TYPE ole2_object,
      go_workbooks                  TYPE ole2_object,
      go_workbook                   TYPE ole2_object,
      go_workbook2                  TYPE ole2_object,
      go_sheets                     TYPE ole2_object,
      go_worksheet                  TYPE ole2_object,
      go_worksheet2                 TYPE ole2_object,
      go_worksheet3                 TYPE ole2_object,
      go_application                TYPE ole2_object ,
      go_pagesetup                  TYPE ole2_object ,
      go_cell                       TYPE ole2_object,
      go_font                       TYPE ole2_object,
      go_border                     TYPE ole2_object,
      go_range0                     TYPE ole2_object,
      go_range                      TYPE ole2_object,
      go_column                     TYPE ole2_object,
      go_row                        TYPE ole2_object,
      go_cell_from                  TYPE ole2_object,
      go_cell_to                    TYPE ole2_object,
      go_interior                   TYPE ole2_object,
      go_charts                     TYPE ole2_object,
      go_chart                      TYPE ole2_object,
      go_chartobjects               TYPE ole2_object ,
      go_title                      TYPE ole2_object ,
      go_titlechar                  TYPE ole2_object ,
      go_axes                       TYPE ole2_object ,
      go_axestitle                  TYPE ole2_object ,
      go_legend                     TYPE ole2_object .

DATA: gv_row                        TYPE sy-tabix   ,
      gv_col                        TYPE sy-tabix   ,
      gv_s_row                      TYPE sy-tabix   ,
      gv_s_col                      TYPE sy-tabix   ,
      gv_e_row                      TYPE sy-tabix   ,
      gv_e_col                      TYPE sy-tabix   ,
      gv_sec1_data_start_row        TYPE sy-tabix ,
      gv_sec1_h_start_row           TYPE sy-tabix ,
      gv_sec3_h_start_row           TYPE sy-tabix ,
      gv_sec3a_start_row            TYPE sy-tabix ,
      gv_sec4_start_row             TYPE sy-tabix ,
      gv_sec4_end_row               TYPE sy-tabix ,
      gv_sec2_start_row             TYPE sy-tabix ,
      gv_sec2d_start_row            TYPE sy-tabix ,
      gv_sec2f_start_row            TYPE sy-tabix ,
      gv_sec2_end_row               TYPE sy-tabix ,
      gv_sec3_end_row               TYPE sy-tabix ,
      gv_sec2a_tgt_start_row        TYPE sy-tabix ,
      gv_sec2b_start_row            TYPE sy-tabix ,
      gv_sec1_lines                 TYPE sy-tabix  ,
      gv_rc                         TYPE sy-subrc   ,
      gv_txt                        TYPE char50     ,
      gv_start_row                  TYPE sy-tabix  ,
      gv_sheet1_name                TYPE char50  ,
      gv_sheet2_name                TYPE char50  ,
      gv_sheet3_name                TYPE char50  .
DATA gs_temp TYPE zoiu_pr_dn .
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001 .
  PARAMETERS : p_date TYPE sy-datum OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK b1 .

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002 .
  PARAMETERS : p_bb  TYPE char1 RADIOBUTTON GROUP rb1 ,               "BBL / BOE
               p_bbd TYPE char1 RADIOBUTTON GROUP rb1 ,               "BOPD / BOEPD
               p_tm  TYPE char1 RADIOBUTTON GROUP rb1 ,               "Tons / MSCM
               p_tmd TYPE char1 RADIOBUTTON GROUP rb1 ,               "TPD / MSCMD
               p_mb  TYPE char1 RADIOBUTTON GROUP rb1 ,               "MMT / BCM
               p_bmd TYPE char1 RADIOBUTTON GROUP rb1 DEFAULT 'X'.    "BOPD / MMSCMD
SELECTION-SCREEN END   OF BLOCK b2 .

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003 .
  PARAMETERS : p_c_jv  TYPE char1 RADIOBUTTON GROUP rb2 DEFAULT 'X', "JV
               p_c_olv TYPE char1 RADIOBUTTON GROUP rb2            . "OVL
SELECTION-SCREEN END OF BLOCK   b3 .
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-004 .
  PARAMETERS : p_t_be TYPE char1 AS CHECKBOX DEFAULT 'X',
               p_t_in TYPE char1 AS CHECKBOX ,
               p_t_ex TYPE char1 AS CHECKBOX ,
               p_t_vg TYPE char1 AS CHECKBOX ,
               p_t_pc TYPE char1 AS CHECKBOX ,
               p_t_re TYPE char1 AS CHECKBOX .
SELECTION-SCREEN END OF BLOCK   b4 .
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-005 .
  PARAMETERS : p_fname TYPE string OBLIGATORY .
SELECTION-SCREEN END OF BLOCK   b5 .

AT SELECTION-SCREEN .
  IF p_t_be IS INITIAL AND
     p_t_in IS INITIAL AND
     p_t_ex IS INITIAL AND
     p_t_vg IS INITIAL AND
     p_t_pc IS INITIAL AND
     p_t_re IS INITIAL .
    MESSAGE 'Please select atleast one target' TYPE 'E' .
  ENDIF.
  IF p_date GT sy-datum.
    MESSAGE 'DPR cannot be run for future dates' TYPE 'E' .
  ENDIF.
  AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
*
*CALL FUNCTION 'F4_FILENAME'
*
*EXPORTING
*
*program_name = syst-cprog
*
*dynpro_number = syst-dynnr
*
*field_name = ' '
*
*IMPORTING
*
*file_name = p_fname.

CALL METHOD cl_gui_frontend_services=>directory_browse
  EXPORTING
    window_title         = 'Please Select Directory to download File'
*    initial_folder       =
  CHANGING
    selected_folder      = p_fname
  EXCEPTIONS
    cntl_error           = 1
    error_no_gui         = 2
    not_supported_by_gui = 3
    others               = 4
        .
IF sy-subrc IS NOT INITIAL.
  MESSAGE 'Error getting Directory Name' TYPE 'E' .
ENDIF.
*&---------------------------------------------------------------------*

START-OF-SELECTION .
  PERFORM clear_variables .
  PERFORM fetch_data .
  PERFORM process_gas_records  .
  PERFORM process_data .

*--- Form Routines ---*
*&---------------------------------------------------------------------*
FORM fetch_data .
*  DATA : lt_zpra_t_dly_prd  TYPE STANDARD TABLE OF zpra_t_dly_prd .
  DATA : lv_mrec_start_date  TYPE sy-datum,
         lv_monat            TYPE t009b-poper,
         lv_date1            TYPE sy-datum,
         lv_date2            TYPE sy-datum,
         lv_gjahr            TYPE gjahr,
         lv_index            TYPE sy-tabix,
         lv_combine_tar_date TYPE sy-datum,
         lv_min_date         TYPE sy-datum,
         lv_max_date         TYPE sy-datum,
         lv_year             TYPE gjahr.
  DATA : lt_zpra_c_prd_prof TYPE STANDARD TABLE OF ty_zpra_c_prd_prof .

  gv_rep_date = p_date .
  p_date = p_date - 1 .

  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
    EXPORTING
      input  = gv_rep_date
    IMPORTING
      output = gv_repdate_e.

  PERFORM add_product_range USING  :  c_prod_oil ,
                                      c_prod_con ,
                                      c_prod_lng ,
                                      c_prod_gas .
  PERFORM add_vl_type_range USING  :  'NET_PROD'  ,
                                      'GROSS_PROD',
                                      'GAS_INJ'   .
  PERFORM buiild_tar_code_range .
* Select those assets which are configured for DPR Report .
  SELECT *
    FROM zpra_c_dpr_prof
    INTO TABLE gt_zpra_c_dpr_prof
   WHERE product IN r_product[]
     AND display_opt NE ' ' .
  IF sy-subrc IS NOT INITIAL .
    MESSAGE 'No assets configured for DPR Report' TYPE 'E' .
  ENDIF.
  SELECT product
         asset
         block
    FROM zpra_c_prd_prof
    INTO TABLE          gt_zpra_c_prd_prof
     FOR ALL ENTRIES IN gt_zpra_c_dpr_prof
   WHERE product     EQ gt_zpra_c_dpr_prof-product
     AND asset       EQ gt_zpra_c_dpr_prof-asset .
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'No Assets found in product profile' TYPE 'E' .
  ENDIF.
  SORT gt_zpra_c_prd_prof .
  DELETE ADJACENT DUPLICATES FROM gt_zpra_c_prd_prof COMPARING ALL FIELDS .
  lv_year = p_date(4) .
  lv_year = lv_year - 5 .
  CONCATENATE lv_year '0401' INTO lv_min_date .
  lv_max_date  = p_date .

  SELECT SINGLE periv
           FROM t001
           INTO gv_periv
          WHERE bukrs EQ 'OVL' .
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Company Code OVL does not exist' TYPE 'E' .
  ENDIF.

  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      i_date         = p_date
    " I_MONMIT       = 00
      i_periv        = gv_periv
    IMPORTING
      e_buper        = lv_monat
      e_gjahr        = gv_current_gjahr
    EXCEPTIONS
      input_false    = 01
      t009_notfound  = 02
      t009b_notfound = 03.

  IF sy-subrc <> 0.
    MESSAGE 'Error getting Period' TYPE 'E' .
  ENDIF.

  CALL FUNCTION 'FIRST_AND_LAST_DAY_IN_YEAR_GET'
    EXPORTING
      i_gjahr        = gv_current_gjahr
      i_periv        = gv_periv
    IMPORTING
      e_first_day    = gv_year_start_date
      e_last_day     = gv_year_end_date
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
    MESSAGE 'Unexpected Internal Error' TYPE 'E'.
  ENDIF.
  lv_max_date = gv_year_end_date .
  PERFORM remove_expired_blocks TABLES gt_zpra_c_prd_prof USING  lv_min_date lv_max_date .

  CALL FUNCTION 'CCM_GO_BACK_MONTHS'
    EXPORTING
      currdate   = p_date
      backmonths = '001'
    IMPORTING
      newdate    = gv_month_back_datum.

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date             = p_date
    IMPORTING
      ev_month_begin_date = gv_month_begin_datum
      ev_month_end_date   = gv_month_end_datum.
  gv_month_days_till_today = p_date - gv_month_begin_datum + 1 .

  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date             = gv_month_back_datum
    IMPORTING
      ev_month_begin_date = gv_month_back_begin_datum
      ev_month_end_date   = gv_month_back_end_datum.

  gv_current_month_days = gv_month_end_datum - gv_month_begin_datum + 1.
  SELECT SINGLE ltx
           FROM t247
           INTO gv_month_name
          WHERE spras = sy-langu
            AND mnr EQ p_date+4(2).

  SELECT SINGLE ltx
           FROM t247
           INTO gv_last_month_name
          WHERE spras = sy-langu
            AND mnr EQ gv_month_back_begin_datum+4(2).

* SELECT *
*   FROM zdpr_gas_combine
*   INTO TABLE gt_zdpr_gas_combine
*  WHERE xfeld EQ 'X' .
  LOOP AT gt_zpra_c_dpr_prof INTO gs_zpra_c_dpr_prof WHERE product EQ c_prod_gas
                                                       AND display_opt EQ 'C' .
    gs_zdpr_gas_combine-asset =  gs_zpra_c_dpr_prof-asset .
    APPEND gs_zdpr_gas_combine TO gt_zdpr_gas_combine.
  ENDLOOP.
  SORT gt_zdpr_gas_combine BY asset .

  REFRESH r_combine_asset[] .
  LOOP AT gt_zdpr_gas_combine INTO gs_zdpr_gas_combine.
    r_combine_asset-sign   = 'I' .
    r_combine_asset-option = 'EQ' .
    r_combine_asset-low    = gs_zdpr_gas_combine-asset .
    APPEND r_combine_asset .
  ENDLOOP.

  SELECT *
    FROM zpra_t_dly_prd
    INTO TABLE gt_zpra_t_dly_prd
     FOR ALL ENTRIES IN gt_zpra_c_prd_prof
   WHERE production_date LE p_date
     AND production_date GE gv_month_back_datum
     AND product     EQ gt_zpra_c_prd_prof-product
     AND asset       EQ gt_zpra_c_prd_prof-asset
     AND block       EQ gt_zpra_c_prd_prof-block
     AND prd_vl_type IN r_prd_vl_type[] .

  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'No data found for the selection' TYPE 'E' .
  ENDIF.


  SORT  gt_zpra_t_dly_prd BY production_date product asset .

* lt_zpra_t_dly_prd = gt_zpra_t_dly_prd .
* SORT lt_zpra_t_dly_prd BY asset block product prd_vl_type.
* DELETE ADJACENT DUPLICATES FROM lt_zpra_t_dly_prd COMPARING asset block product prd_vl_type .

  SELECT asset
        block
        vld_frm
        vld_to
        pi
        prod_start_date
   FROM zpra_t_prd_pi
   INTO TABLE gt_zpra_t_prd_pi
    FOR ALL ENTRIES IN gt_zpra_c_prd_prof
  WHERE asset EQ gt_zpra_c_prd_prof-asset
    AND block EQ gt_zpra_c_prd_prof-block
    AND vld_frm LE p_date
    AND vld_to  GE gv_month_back_datum .

  SORT gt_zpra_t_prd_pi BY asset block vld_frm vld_to .

  CALL FUNCTION 'CCM_GO_BACK_MONTHS'
    EXPORTING
      currdate   = gv_month_back_datum
      backmonths = '020'
    IMPORTING
      newdate    = lv_mrec_start_date.

  SELECT SINGLE periv
           FROM t001
           INTO gv_periv
          WHERE bukrs EQ 'OVL' .
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Company Code OVL does not exist' TYPE 'E' .
  ENDIF.
  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      i_date         = p_date
    " I_MONMIT       = 00
      i_periv        = gv_periv
    IMPORTING
      e_buper        = lv_monat
      e_gjahr        = gv_current_gjahr
    EXCEPTIONS
      input_false    = 01
      t009_notfound  = 02
      t009b_notfound = 03.

  IF sy-subrc <> 0.
    MESSAGE 'Error getting Period' TYPE 'E' .
  ENDIF.
  gv_current_calendar_gjahr = p_date(4) .
  gv_next_gjahr   = gv_current_gjahr + 1 .
  gv_last_gjahr   = gv_current_gjahr - 1 .
  gv_5_back_gjahr = gv_current_gjahr - 5 .

  CALL FUNCTION 'FIRST_AND_LAST_DAY_IN_YEAR_GET'
    EXPORTING
      i_gjahr        = gv_current_gjahr
      i_periv        = gv_periv
    IMPORTING
      e_first_day    = gv_year_start_date
      e_last_day     = gv_year_end_date
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
    MESSAGE 'Unexpected Internal Error' TYPE 'E'.
  ENDIF.
  gv_total_days_in_year = gv_year_end_date - gv_year_start_date + 1 .
  gv_current_gjahr_days = gv_total_days_in_year .
  gv_days_gone_in_year  =  p_date - gv_year_start_date + 1 .
*  gv_days_left_in_year  =  gv_year_end_date - p_date   + 1 .
  gv_days_left_in_year  =  gv_year_end_date - p_date   .
  gv_current_monat = lv_monat+1(2) .

  lv_gjahr = gv_year_start_date(4) - 1 .
  CONCATENATE lv_gjahr gv_year_start_date+4(4) INTO gv_last_year_s_date .

  lv_gjahr = p_date(4) - 1 .
  CONCATENATE lv_gjahr p_date+4(4) INTO gv_last_year_t_date .

  gv_days_gone_in_last_year  =  gv_last_year_t_date - gv_last_year_s_date + 1 .

  CALL FUNCTION 'FIRST_AND_LAST_DAY_IN_YEAR_GET'
    EXPORTING
      i_gjahr        = gv_last_gjahr
      i_periv        = gv_periv
    IMPORTING
      e_first_day    = lv_date1
      e_last_day     = lv_date2
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
    MESSAGE 'Unexpected Internal Error' TYPE 'E'.
  ENDIF.
  gv_last_gjahr_days = lv_date2 - lv_date1 + 1 .

  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      i_date         = lv_mrec_start_date
    " I_MONMIT       = 00
      i_periv        = gv_periv
    IMPORTING
      e_buper        = lv_monat
      e_gjahr        = gv_mrec_gjahr_start
    EXCEPTIONS
      input_false    = 01
      t009_notfound  = 02
      t009b_notfound = 03.

  IF sy-subrc <> 0.
    MESSAGE 'Error getting MREC Date' TYPE 'E' .
  ENDIF.
  gv_mrec_monat_start = lv_monat+1(2) .

  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      i_date         = gv_month_back_datum
    " I_MONMIT       = 00
      i_periv        = gv_periv
    IMPORTING
      e_buper        = lv_monat
      e_gjahr        = gv_mrec_gjahr_end
    EXCEPTIONS
      input_false    = 01
      t009_notfound  = 02
      t009b_notfound = 03.

  IF sy-subrc <> 0.
    MESSAGE 'Error getting MREC Date' TYPE 'E' .
  ENDIF.
  gv_month_back_calendar_gjahr = gv_month_back_datum(4) .
  gv_month_back_gjahr = gv_mrec_gjahr_end .
  gv_month_back_monat = lv_monat+1(2) .
  gv_mrec_monat_end = lv_monat+1(2) .
  IF gv_mrec_gjahr_start EQ gv_mrec_gjahr_end .
    SELECT gjahr
           monat
           asset
           block
           product
           prd_vl_type
           prod_vl_qty1
           prod_vl_uom1
           prod_vl_qty2
           prod_vl_uom2
      FROM zpra_t_mrec_prd
      INTO TABLE gt_zpra_t_mrec_prd
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE asset        EQ gt_zpra_c_prd_prof-asset
       AND block        EQ gt_zpra_c_prd_prof-block
       AND product      EQ gt_zpra_c_prd_prof-product
       AND prd_vl_type  IN r_prd_vl_type[]
       AND gjahr EQ gv_mrec_gjahr_start
       AND monat GE gv_mrec_monat_start
       AND monat LE gv_mrec_monat_end .
  ELSE.
    SELECT gjahr
           monat
           asset
           block
           product
           prd_vl_type
           prod_vl_qty1
           prod_vl_uom1
           prod_vl_qty2
           prod_vl_uom2
      FROM zpra_t_mrec_prd
      INTO TABLE gt_zpra_t_mrec_prd
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE asset        EQ gt_zpra_c_prd_prof-asset
       AND block        EQ gt_zpra_c_prd_prof-block
       AND product      EQ gt_zpra_c_prd_prof-product
       AND prd_vl_type  IN r_prd_vl_type[]
*     AND ( ( gjahr EQ gv_mrec_gjahr_start AND
*           monat GE gv_mrec_monat_start ) OR
*           ( gjahr EQ gv_mrec_gjahr_end AND
*           monat LE gv_mrec_monat_end ) ) .
       AND ( gjahr GE gv_mrec_gjahr_start AND
             gjahr LE gv_mrec_gjahr_end ) .
    DELETE gt_zpra_t_mrec_prd WHERE gjahr EQ gv_mrec_gjahr_end   AND monat GT gv_mrec_monat_end   .
    DELETE gt_zpra_t_mrec_prd WHERE gjahr EQ gv_mrec_gjahr_start AND monat LT gv_mrec_monat_start .
  ENDIF.

  SELECT dn_no
         dn_de
    FROM zoiu_pr_dn
    INTO TABLE gt_asset_desc
     FOR ALL ENTRIES IN gt_zpra_c_prd_prof
   WHERE dn_no EQ gt_zpra_c_prd_prof-asset .

  SORT gt_asset_desc BY asset .

  DELETE gt_zpra_t_mrec_prd WHERE product EQ c_prod_gas .
  SORT gt_zpra_t_mrec_prd BY product ASCENDING asset ASCENDING gjahr DESCENDING monat DESCENDING block ASCENDING prd_vl_type ASCENDING .
  IF r_tar_code[] IS NOT INITIAL.
    SELECT asset
           block
           tar_code
           vld_frm
           vld_to
           pi
      FROM zpra_t_tar_pi
      INTO TABLE gt_zpra_t_tar_pi
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE asset EQ gt_zpra_c_prd_prof-asset
       AND block EQ gt_zpra_c_prd_prof-block
       AND vld_frm LE gv_month_end_datum
       AND vld_to  GE gv_month_begin_datum .
    SORT gt_zpra_t_tar_pi BY asset block tar_code vld_frm .

    SELECT tar_code
           gjahr
           monat
           asset
           block
           product
           prod_vl_type_cd
           tar_qty
           uom
      FROM zpra_t_prd_tar
      INTO TABLE gt_zpra_t_prd_tar
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE gjahr EQ gv_current_gjahr
       AND monat EQ gv_current_monat
       AND asset EQ gt_zpra_c_prd_prof-asset
       AND block EQ gt_zpra_c_prd_prof-block
       AND product EQ gt_zpra_c_prd_prof-product
       AND prod_vl_type_cd IN r_prd_vl_type[]
       AND tar_code IN r_tar_code[] .
    SORT gt_zpra_t_prd_tar BY gjahr monat asset block product prod_vl_type_cd tar_code .

    SELECT *
      FROM zpra_t_tar_cf
      INTO TABLE gt_zpra_t_tar_cf
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE gjahr EQ gv_current_gjahr
       AND asset EQ gt_zpra_c_prd_prof-asset
       AND block EQ gt_zpra_c_prd_prof-block
       AND product EQ gt_zpra_c_prd_prof-product .

    SORT gt_zpra_t_tar_cf BY gjahr asset block product .
  ENDIF.

  SELECT *
    FROM zpra_t_tar_cf
    INTO TABLE gt_cf
     FOR ALL ENTRIES IN gt_zpra_c_prd_prof
   WHERE   asset EQ gt_zpra_c_prd_prof-asset
     AND   block EQ gt_zpra_c_prd_prof-block
     AND product EQ gt_zpra_c_prd_prof-product .

  SORT gt_cf BY gjahr product asset block  .

  PERFORM fetch_section2a2_data .

  SELECT asset
         tar_code
         MIN( vld_frm )
    FROM zpra_t_tar_pi
    INTO TABLE gt_tar_start_dates
   WHERE tar_code IN r_tar_code[]
   GROUP BY asset tar_code .
  SORT gt_tar_start_dates BY asset tar_code .

  lt_zpra_c_prd_prof = gt_zpra_c_prd_prof .
  SORT lt_zpra_c_prd_prof BY asset  .

  LOOP AT gt_tar_start_dates INTO gs_tar_start_dates.
    lv_index = sy-tabix .
    READ TABLE lt_zpra_c_prd_prof TRANSPORTING NO FIELDS WITH KEY asset = gs_tar_start_dates-asset BINARY SEARCH .
    IF sy-subrc IS NOT INITIAL.
      DELETE gt_tar_start_dates INDEX lv_index .
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM process_data .
  PERFORM start_excel      .

  PERFORM process_sec1_data .
  PERFORM display_section1 .

  PERFORM process_sec2_data .

  PERFORM process_sec3_data .

  PERFORM process_sec4_data .

  CALL METHOD OF go_worksheet2 'ACTIVATE'.

  PERFORM process_sec5_data .

  PERFORM process_sec6_data .

  CALL METHOD OF go_worksheet 'ACTIVATE'.

  PERFORM set_columns_width .

  PERFORM border_cells .
  PERFORM set_cell_formats .
  PERFORM finalize_worksheet .
ENDFORM.
FORM process_sec1_data .
  gt_zpra_t_dly_prd2 = gt_zpra_t_dly_prd.
  SORT gt_zpra_t_dly_prd2 BY product asset  .
  DELETE ADJACENT DUPLICATES FROM gt_zpra_t_dly_prd2 COMPARING product asset .
  PERFORM calculated_wtd_pi .
  PERFORM calculated_wtd_cf .
  PERFORM prepare_dynamic_table_sec1 .
  PERFORM fill_dynamic_table_sec1    .

ENDFORM.
FORM process_sec2_data .

  PERFORM process_sec2a_data .
* Section 2b is same as 2a1 only difference being section 2a1 takes only
* current month data, 2b shows whole year data
  PERFORM fetch_data_section2b .
  PERFORM create_dynamic_table CHANGING gt_sec2b_table .
  ASSIGN gt_sec2b_table->* TO <gfs_sec2b_table> .
  PERFORM process_sec2b_data .
* Section 2c is same as 2b only difference being section  2b shows whole year data
* 2c shows YTD data
  PERFORM fetch_data_section2c .
  PERFORM create_dynamic_table CHANGING gt_sec2c_table .
  ASSIGN gt_sec2c_table->* TO <gfs_sec2c_table> .
  PERFORM process_sec2c_data .

  PERFORM fetch_data_section2d .
  PERFORM process_gas_records_2d .
  PERFORM create_dynamic_table CHANGING gt_sec2d_table .
  ASSIGN gt_sec2d_table->* TO <gfs_sec2d_table> .
  PERFORM fill_dynamic_table_sec2d .
  PERFORM display_section2d .

  IF p_bbd IS NOT INITIAL OR
     p_tmd IS NOT INITIAL OR
     p_bmd IS NOT INITIAL .
    PERFORM create_dynamic_table CHANGING gt_sec2e_table .
    ASSIGN gt_sec2e_table->* TO <gfs_sec2e_table> .
    PERFORM fill_dynamic_table_sec2e .
    PERFORM display_section2e .
  ENDIF.
  PERFORM fetch_data_section2f .
  PERFORM process_gas_records_2f .
  PERFORM create_dynamic_table CHANGING gt_sec2f_table .
  ASSIGN gt_sec2f_table->* TO <gfs_sec2f_table> .
  PERFORM fill_dynamic_table_sec2f .
  PERFORM display_section2f .

ENDFORM.
FORM process_sec3_data .

  PERFORM display_section3_header .

  PERFORM create_dynamic_table CHANGING gt_sec3a_table .
  ASSIGN gt_sec3a_table->* TO <gfs_sec3a_table> .
  PERFORM fill_dynamic_table_sec3a .
  PERFORM display_section3a .

  PERFORM create_dynamic_table CHANGING gt_sec3b_table .
  ASSIGN gt_sec3b_table->* TO <gfs_sec3b_table> .
  PERFORM process_sec3b_data .
  PERFORM fill_dynamic_table_sec3b .
  PERFORM display_section3b .

  PERFORM fetch_data_section3c .
  PERFORM process_gas_records_3c .
  PERFORM create_dynamic_table CHANGING gt_sec3c_table .
  ASSIGN gt_sec3c_table->* TO <gfs_sec3c_table> .
  PERFORM fill_dynamic_table_sec3c .
  PERFORM display_section3c .

  PERFORM create_dynamic_table CHANGING gt_sec3d_table .
  ASSIGN gt_sec3d_table->* TO <gfs_sec3d_table> .
  PERFORM fill_dynamic_table_sec3d .
  PERFORM display_section3d .

  PERFORM create_dynamic_table CHANGING gt_sec3e_table .
  ASSIGN gt_sec3e_table->* TO <gfs_sec3e_table> .
  PERFORM fill_dynamic_table_sec3e .
  PERFORM display_section3e .

  PERFORM fetch_data_section3f .
  PERFORM process_gas_records_3f .
  PERFORM create_dynamic_table CHANGING gt_sec3f_table .
  ASSIGN gt_sec3f_table->* TO <gfs_sec3f_table> .
  PERFORM fill_dynamic_table_sec3f .
  PERFORM display_section3f .

  gv_sec3_end_row = gv_row .
ENDFORM.
FORM process_sec4_data .
  PERFORM fetch_data_section4a .
  PERFORM process_gas_records_4a .
  PERFORM create_dynamic_table CHANGING gt_sec4a_table .
  ASSIGN gt_sec4a_table->* TO <gfs_sec4a_table> .
  PERFORM fill_dynamic_table_sec4a .

  PERFORM display_section4a .
  PERFORM display_section4b .

  PERFORM display_run_date_time .

ENDFORM.
FORM process_sec5_data .
  PERFORM process_sec5a_data .
ENDFORM .
FORM process_sec5a_data .
  PERFORM prepare_dynamic_table_sec5a .
  PERFORM fetch_data_section5a .
  PERFORM process_gas_records_5a .
  PERFORM fill_dynamic_table_sec5a .
  PERFORM display_section5a .
  PERFORM show_progress USING '90' .
ENDFORM .
FORM process_sec6_data .
  PERFORM prepare_dynamic_table_sec6 .
  PERFORM fill_dynamic_table_sec6 .
  PERFORM display_section6 .
ENDFORM .

FORM prepare_dynamic_table_sec1 .
  DATA : lv_col_name       TYPE lvc_fname,
         lv_product        TYPE zpra_t_dly_prd-product,
         lv_no_column      TYPE c,
         lv_combine_column TYPE c.
  DATA : lt_zpra_c_prd_prof  TYPE STANDARD TABLE OF ty_zpra_c_prd_prof,
         lt_zpra_c_prd_prof2 TYPE STANDARD TABLE OF ty_zpra_c_prd_prof.
  CLEAR gv_pos .
  REFRESH gt_dyn_fcat .
  PERFORM add_dyn_field USING 'COL01' 'Column1' 30 .
  PERFORM add_dyn_field USING 'COL02' 'Column1' 30 .

  lt_zpra_c_prd_prof = gt_zpra_c_prd_prof .
  SORT lt_zpra_c_prd_prof BY product asset.
  DELETE ADJACENT DUPLICATES FROM lt_zpra_c_prd_prof COMPARING product asset .

  lt_zpra_c_prd_prof2 = lt_zpra_c_prd_prof .
  DELETE lt_zpra_c_prd_prof  WHERE product EQ c_prod_gas .
  DELETE lt_zpra_c_prd_prof2 WHERE product NE c_prod_gas .
  APPEND LINES OF lt_zpra_c_prd_prof2 TO lt_zpra_c_prd_prof .

  LOOP AT lt_zpra_c_prd_prof INTO gs_zpra_c_prd_prof.
    CLEAR lv_no_column .
    IF lv_product IS NOT INITIAL.
      IF lv_product NE gs_zpra_c_prd_prof-product.
        IF lv_product EQ '722000004' AND lv_combine_column IS NOT INITIAL.
          gv_len = strlen( lv_product ) .
          CONCATENATE lv_product(gv_len) '-' 'COMBINE' INTO lv_col_name .
          PERFORM add_dyn_field USING lv_col_name lv_col_name 35 .
        ENDIF.
        gv_len = strlen( lv_product ) .
        CONCATENATE lv_product(gv_len) '-' 'TOTAL' INTO lv_col_name .
        PERFORM add_dyn_field USING lv_col_name lv_col_name 35 .
      ENDIF.
    ENDIF.
    IF gs_zpra_c_prd_prof-product EQ c_prod_gas.
      READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_c_prd_prof-asset BINARY SEARCH .
      IF sy-subrc IS INITIAL .
        lv_no_column = 'X' .
        lv_combine_column = 'X' .
      ENDIF.
    ENDIF.
    IF lv_no_column IS INITIAL .
      gv_len = strlen( gs_zpra_c_prd_prof-product ) .
      CONCATENATE gs_zpra_c_prd_prof-product(gv_len) '-' gs_zpra_c_prd_prof-asset INTO lv_col_name .
      PERFORM add_dyn_field USING lv_col_name lv_col_name 35 .
    ENDIF.
    lv_product = gs_zpra_c_prd_prof-product .
  ENDLOOP.

  IF lv_product EQ c_prod_gas AND lv_combine_column IS NOT INITIAL.
    gv_len = strlen( lv_product ) .
    CONCATENATE lv_product(gv_len) '-' 'COMBINE' INTO lv_col_name .
    PERFORM add_dyn_field USING lv_col_name lv_col_name 35 .
  ENDIF.

  gv_len = strlen( gs_zpra_c_prd_prof-product ) .
  CONCATENATE gs_zpra_c_prd_prof-product(gv_len) '-' 'TOTAL' INTO lv_col_name .
  PERFORM add_dyn_field USING lv_col_name lv_col_name 35 .

  CONCATENATE 'GRAND' '-' 'TOTAL' INTO lv_col_name .
  PERFORM add_dyn_field USING lv_col_name lv_col_name 35 .

  gv_table_columns = gv_pos .
** Create a dynamic internal table with this structure.
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
*     i_style_table             = X
      it_fieldcatalog           = gt_dyn_fcat
    IMPORTING
      ep_table                  = gt_dyn_table
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2.
  IF sy-subrc NE 0.
    MESSAGE 'Unexpected Internal Error' TYPE 'E' .
  ELSE.
* Assign the new table to field symbol
    ASSIGN gt_dyn_table->* TO <gfs_dyn_table>.
  ENDIF.
ENDFORM.
FORM add_dyn_field  USING    p_colname
                             p_coltext
                             p_outputlen.

  gv_pos = gv_pos + 1.
  gs_dyn_fcat-fieldname = p_colname .
  gs_dyn_fcat-outputlen = p_outputlen.
*gs_dyn_fcat-tabname   = IT_DEMO.
  gs_dyn_fcat-coltext   = p_coltext.
  gs_dyn_fcat-col_pos   = gv_pos.
*gs_dyn_fcat-key = X.
*gs_dyn_fcat-key_sel = X.
  APPEND gs_dyn_fcat TO gt_dyn_fcat.
ENDFORM.
FORM fill_dynamic_table_sec1 .
  DATA : lv_col_name      TYPE lvc_fname,
         lv_combine_field TYPE c,
         lv_index         TYPE sy-tabix,
         lv_product       TYPE char100,
         lv_asset         TYPE char100.
  DATA : lt_zpra_t_dly_prd     TYPE STANDARD TABLE OF zpra_t_dly_prd.

  SORT gt_wtd_pi BY production_date product asset .
  PERFORM add_dummy_data TABLES lt_zpra_t_dly_prd .
*  BREAK JVUSER01 .
  LOOP AT lt_zpra_t_dly_prd INTO gs_zpra_t_dly_prd.
    lv_index = sy-tabix .
    CLEAR lv_combine_field .
    AT NEW production_date .
      IF lv_index NE 1 .
        PERFORM fill_null_values_with_previous .
      ENDIF.
      APPEND INITIAL LINE TO <gfs_dyn_table> ASSIGNING <gfs_dyn_line> .
      ASSIGN COMPONENT 'COL01' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
        EXPORTING
          input  = gs_zpra_t_dly_prd-production_date
        IMPORTING
          output = <gfs_field>.
      IF sy-subrc <> 0.
        MESSAGE 'Unexpteced Internal Error' TYPE 'E' .
      ENDIF.
      REPLACE ALL OCCURRENCES OF '.' IN <gfs_field> WITH '-' .
    ENDAT .
    IF gs_zpra_t_dly_prd-comments EQ 'DUMMYDPRDATA'.
      CONTINUE.
    ENDIF.
*    PERFORM convert_non_gas_units CHANGING gs_zpra_t_dly_prd.
    PERFORM convert_non_gas_units_2a2 CHANGING gs_zpra_t_dly_prd.

    IF gs_zpra_t_dly_prd-product = c_prod_gas.
      READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_dly_prd-asset BINARY SEARCH .
      IF  sy-subrc IS INITIAL.
        lv_combine_field = 'X' .
      ENDIF.
    ENDIF.
    PERFORM get_constorium_multipliers USING  gs_zpra_t_dly_prd lv_combine_field.
    PERFORM get_constorium_multipliers_2a2 USING '1' .

* Individual Column..
    gv_len = strlen( gs_zpra_t_dly_prd-product ) .
    IF lv_combine_field IS INITIAL.
      CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' gs_zpra_t_dly_prd-asset INTO lv_col_name .
    ELSE.
      CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
    ENDIF.
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_individual_mul.
      UNASSIGN <gfs_field> .
    ENDIF.
* Product Total..
    CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_total_mul .
      UNASSIGN <gfs_field> .
    ENDIF.
* Grand Total
    CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_grand_total_mul.
      UNASSIGN <gfs_field> .
    ENDIF.
  ENDLOOP.
  PERFORM fill_null_values_with_previous .

  LOOP AT <gfs_dyn_table> ASSIGNING <gfs_dyn_line> .
    DO .
      lv_index = sy-index .
      ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      IF sy-subrc IS NOT INITIAL.
        EXIT .
      ELSE.
        IF lv_index GT 2.
          IF p_mb IS NOT INITIAL.
            <gfs_field> = <gfs_field> / 1000000 .
          ENDIF.
          IF p_bmd IS NOT INITIAL.
            READ TABLE gt_dyn_fcat INTO gs_dyn_fcat WITH KEY col_pos = lv_index BINARY SEARCH .
            IF sy-subrc IS INITIAL.
              SPLIT gs_dyn_fcat-fieldname AT '-' INTO lv_product lv_asset .
              IF lv_product EQ c_prod_gas.
                <gfs_field> = <gfs_field> / 6290 .
              ENDIF.
            ENDIF.
          ENDIF.
          IF <gfs_field> LT 0 .
            <gfs_field> = <gfs_field> * -1 .
            CONDENSE <gfs_field>.
            CONCATENATE '-' <gfs_field> INTO <gfs_field> .
          ENDIF.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDLOOP.
ENDFORM.
FORM display_section1 .
  PERFORM display_section1_header .
  PERFORM display_section1_data   .
  PERFORM formatting_section1 .
ENDFORM.
FORM display_section2a1 .
  PERFORM display_sec2a_targets .
ENDFORM.
FORM display_section2a2 .
  DATA lv_lines TYPE sy-tabix .
  DESCRIBE TABLE <gfs_sec2a2_table> LINES lv_lines .
  gv_row = gv_row + 1 .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row + lv_lines - 1 .
  gv_e_col = gv_s_col + gv_table_columns - 1.
  PERFORM prepare_paste_data TABLES <gfs_sec2a2_table> .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM paste_data .

  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = gv_sec2a2_colour .
  gv_row = gv_e_row .

  PERFORM select_range USING gv_s_row 2 gv_e_row 2  .
  PERFORM set_numberformat USING 'mmm yyyy'.

  PERFORM select_range USING gv_s_row 1 gv_e_row 1  .
  CALL METHOD OF go_range 'Merge' .
  PERFORM set_range USING 'Prod. : MTD Actual'  0.

ENDFORM.
FORM display_section2a3 .
  DATA lv_lines TYPE sy-tabix .
  DESCRIBE TABLE <gfs_sec2a3_table> LINES lv_lines .
  gv_row = gv_row + 1 .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row + lv_lines - 1 .
  gv_e_col = gv_s_col + gv_table_columns - 1.
  PERFORM prepare_paste_data TABLES <gfs_sec2a3_table> .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM paste_data .

  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = gv_sec2a3_colour .
  gv_row = gv_e_row .

  PERFORM select_range USING gv_row 2 gv_row 2  .
  PERFORM set_numberformat USING 'mmm yyyy'.

*  PERFORM select_range USING gv_s_row 1 gv_e_row 1 .
*  PERFORM set_range USING 'Monthly Actual' 0.

ENDFORM.
FORM display_section2d .
  DATA lv_lines TYPE sy-tabix .
  DESCRIBE TABLE <gfs_sec2d_table> LINES lv_lines .
  gv_row = gv_row + 1 .
  gv_sec2d_start_row = gv_row .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row + lv_lines - 1 .
  gv_e_col = gv_s_col + gv_table_columns - 1.
  PERFORM prepare_paste_data TABLES <gfs_sec2d_table> .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM paste_data .

  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = gv_sec2d_colour .
  gv_row = gv_e_row .

*  PERFORM select_range USING gv_row 2 gv_row 2  .
*  PERFORM set_numberformat USING 'mmmm yyyy'.

  PERFORM select_range USING gv_s_row 1 gv_e_row 1 .
  CALL METHOD OF go_range 'Merge' .
  PERFORM set_range USING 'YTD Actual Prod.' 0.
ENDFORM.
FORM display_section2f .
  DATA lv_lines TYPE sy-tabix .
  DESCRIBE TABLE <gfs_sec2f_table> LINES lv_lines .
  gv_row = gv_row + 1 .
  gv_sec2f_start_row = gv_row .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row + lv_lines - 1 .
  gv_e_col = gv_s_col + gv_table_columns - 1.
  PERFORM prepare_paste_data TABLES <gfs_sec2f_table> .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM paste_data .

  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = gv_sec2f_colour .
  gv_row = gv_e_row .
  gv_sec2_end_row = gv_row .
*  PERFORM select_range USING gv_row 2 gv_row 2  .
*  PERFORM set_numberformat USING 'mmmm yyyy'.
ENDFORM.
FORM display_section3a .
  DATA lv_lines TYPE sy-tabix .
  DESCRIBE TABLE <gfs_sec3a_table> LINES lv_lines .
*----------------Changes to show blank line if target data is not available----------*
*  CHECK lv_lines IS NOT INITIAL .
  IF lv_lines IS INITIAL.
    lv_lines = 1.
  ENDIF.
*----------------End of changes by Abhishek----------TR OCDK904738--------------------*
  gv_row = gv_row + 1 .
  gv_sec3a_start_row = gv_row .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row + lv_lines - 1 .
  gv_e_col = gv_s_col + gv_table_columns - 1.
  PERFORM prepare_paste_data TABLES <gfs_sec3a_table> .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
*----------------Changes to show blank line if target data is not available----------*
  IF gt_paste IS NOT INITIAL.
    PERFORM paste_data .
  ENDIF.
*----------------End of changes by Abhishek----------TR OCDK904738--------------------*

  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = gv_sec3a_colour .
  gv_row = gv_e_row .

  PERFORM select_range USING gv_s_row 1 gv_e_row 1 .
  CALL METHOD OF go_range 'Merge' .
  CONCATENATE 'Target :' gv_current_gjahr '-' gv_next_gjahr+2(2) INTO gv_txt SEPARATED BY space .
  PERFORM set_range USING gv_txt 0.

ENDFORM.

FORM display_section3b .
  DATA lv_lines TYPE sy-tabix .
  DESCRIBE TABLE <gfs_sec3b_table> LINES lv_lines .
*----------------Changes to show blank line if target data is not available----------*
*  CHECK lv_lines IS NOT INITIAL .
  IF lv_lines IS INITIAL.
    lv_lines = 1.
  ENDIF.
*----------------End of changes by Abhishek----------TR OCDK904738--------------------*
  gv_row   = gv_row + 1 .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row + lv_lines - 1 .
  gv_e_col = gv_s_col + gv_table_columns - 1.
  PERFORM prepare_paste_data TABLES <gfs_sec3b_table> .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
*----------------Changes to show blank line if target data is not available----------*
  IF gt_paste IS NOT INITIAL.
    PERFORM paste_data .
  ENDIF.
*----------------End of changes by Abhishek----------TR OCDK904738--------------------*

  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = gv_sec3b_colour .
  gv_row = gv_e_row .

  PERFORM select_range USING gv_s_row 1 gv_e_row 1 .
  CALL METHOD OF go_range 'Merge' .
  CONCATENATE 'YTD Target :' gv_current_gjahr '-' gv_next_gjahr+2(2) INTO gv_txt SEPARATED BY space .
  PERFORM set_range USING gv_txt 0.

ENDFORM.
FORM display_section2e .
  DATA lv_lines TYPE sy-tabix .
  DATA lv_txt   TYPE char50 .
  DESCRIBE TABLE <gfs_sec2e_table> LINES lv_lines .
*----------------Changes to show blank line if asking rate is not available----------*
*  CHECK lv_lines IS NOT INITIAL .
  IF lv_lines IS INITIAL.
    lv_lines = 1.
  ENDIF.
*----------------End of changes by Abhishek----------TR OCDK904754--------------------*
  gv_row = gv_row + 1 .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row + lv_lines - 1 .
  gv_e_col = gv_s_col + gv_table_columns - 1.
  PERFORM prepare_paste_data TABLES <gfs_sec2e_table> .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
*----------------Changes to show blank line if asking rate is not available----------*
  IF gt_paste IS NOT INITIAL.
    PERFORM paste_data .
  ENDIF.
*----------------End of changes by Abhishek----------TR OCDK904754--------------------*

  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = gv_sec2e_colour .
  gv_row = gv_e_row .

*  PERFORM select_range USING gv_row 2 gv_row 2  .
*  PERFORM set_numberformat USING 'mmmm yyyy'.

  PERFORM select_range USING gv_s_row 1 gv_e_row 1 .
  CALL METHOD OF go_range 'Merge' .
  CONCATENATE 'Asking Rate :' gv_current_gjahr '-' gv_next_gjahr+2(2) INTO lv_txt SEPARATED BY space.
  PERFORM set_range USING lv_txt 1.

ENDFORM.
FORM display_section3c .
  DATA lv_lines TYPE sy-tabix .
  DESCRIBE TABLE <gfs_sec3c_table> LINES lv_lines .
  CHECK lv_lines IS NOT INITIAL .
  gv_row   = gv_row + 1 .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row + lv_lines - 1 .
  gv_e_col = gv_s_col + gv_table_columns - 1.
  PERFORM prepare_paste_data TABLES <gfs_sec3c_table> .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM paste_data .

  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = gv_sec3c_colour .
  gv_row = gv_e_row .

ENDFORM.
FORM display_section4a .
  DATA lv_lines TYPE sy-tabix .
  DESCRIBE TABLE <gfs_sec4a_table> LINES lv_lines .
  CHECK lv_lines IS NOT INITIAL .
  gv_row   = gv_row + 2 .
  gv_sec4_start_row = gv_row .
  PERFORM select_range USING gv_row 1 gv_row 2  .
  CALL METHOD OF go_range 'Merge' .
  PERFORM set_range USING 'Gas Production (YTD)' 0.

  PERFORM set_range_interior USING gv_header_gas_colour.

  gv_row   = gv_row + 1 .
  PERFORM select_range USING gv_row 1 gv_row 2  .
  CALL METHOD OF go_range 'Merge' .
  PERFORM set_range USING 'Unit of Measurement: MMSCM' 0.

  PERFORM set_range_interior USING gv_header_gas_colour.

  gv_row   = gv_row + 1 .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row + lv_lines - 1 .
  gv_e_col = 2 .
  PERFORM prepare_section4a_paste_data .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM paste_data .

  PERFORM select_range USING gv_s_row gv_e_col gv_e_row gv_e_col  .
  PERFORM set_numberformat USING '0.000' .

  gv_row = gv_e_row .
  gv_sec4_end_row = gv_row .

ENDFORM.
FORM display_section4b .
  DATA lv_lines TYPE sy-tabix .

  gv_row   = gv_row + 2 .
  PERFORM select_range USING gv_row 1 gv_row 1  .
  PERFORM set_range USING 'Remarks' 0.
  PERFORM set_range_font  USING 13 1 .

  PERFORM prepare_section4b_paste_data .
  DESCRIBE TABLE gt_paste LINES lv_lines .
  CHECK lv_lines GT 0 .

  gv_row   = gv_row + 1 .
  PERFORM select_range USING gv_row 1 gv_row 1  .
  PERFORM set_range USING 'Asset' 0.

  PERFORM select_range USING gv_row 2 gv_row 2  .
  PERFORM set_range USING 'Date' 0.

  PERFORM select_range USING gv_row 3 gv_row 9  .
  CALL METHOD OF go_range 'Merge' .
  PERFORM set_range USING 'Comments' 0.

  PERFORM select_range USING gv_row 1 gv_row 9  .
  PERFORM set_range_font  USING 13 1 .
  PERFORM set_all_borders_range .

  gv_row   = gv_row + 1 .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row + lv_lines - 1 .
  gv_e_col = 3 .

  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM paste_data .

  PERFORM select_range USING gv_s_row 1 gv_e_row 2  .
  PERFORM set_all_borders_range .

  PERFORM select_range USING gv_s_row 3 gv_e_row 9  .
  PERFORM set_thin_border USING 1 1 1 1 .
  CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '12'.
  SET PROPERTY OF go_border 'LineStyle' = '1' .
  CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '10'.
  SET PROPERTY OF go_border 'LineStyle' = '1' .


  gv_row = gv_e_row .

ENDFORM.
FORM display_section5a .
  DATA lv_lines TYPE sy-tabix .
  gv_row   =  1 .

  DESCRIBE TABLE <gfs_sec5a_table> LINES lv_lines .
  CHECK lv_lines IS NOT INITIAL .
  gv_5a_rows = lv_lines .

  gv_row   = gv_row + 1 .
  gv_s_row = gv_row .
  gv_s_col = 2 .
  gv_e_row = gv_s_row + gv_5a_cols - 1 .
  gv_e_col = gv_s_col + gv_5a_rows .

  PERFORM prepare_section5a_paste_data .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM paste_data_sheet2 .

  PERFORM create_chart .

ENDFORM.
FORM create_chart .
  DATA : lv_chart_title TYPE char50,
         lv_unit        TYPE char10.
  CASE abap_true.
    WHEN p_bb.
      lv_unit   = 'BOE' .
    WHEN p_bbd.
      lv_unit   = 'BOEPD' .
    WHEN p_tm.
      lv_unit   = 'TOE' .
    WHEN p_tmd.
      lv_unit   = 'TOEPD' .
    WHEN p_mb.
      lv_unit   = 'MMTOE' .
    WHEN p_bmd.
      lv_unit   = 'BOEPD' .
    WHEN OTHERS.
  ENDCASE.
  CONCATENATE 'Production Performance' gv_current_gjahr '-' gv_next_gjahr INTO lv_chart_title SEPARATED BY space .
  GET PROPERTY OF go_application 'Charts' = go_charts .
  CALL METHOD OF go_charts 'Add' = go_chart .
  CALL METHOD OF go_chart 'Activate' .
  SET PROPERTY OF go_chart 'HasTitle' = 1.
  GET PROPERTY OF go_chart 'ChartTitle' = go_title.
  GET PROPERTY OF go_title 'Characters' = go_titlechar.
  SET PROPERTY OF go_titlechar 'Text' = lv_chart_title.

  CALL METHOD OF go_chart 'Axes' = go_axes
    EXPORTING    #2 = 2.
  SET PROPERTY OF go_axes 'HasTitle' = 1.
  GET PROPERTY OF go_axes 'AxisTitle' = go_axestitle.
  GET PROPERTY OF go_axestitle 'Characters' = go_axestitle.
  SET PROPERTY OF go_axestitle 'Text' = lv_unit.

  SET PROPERTY OF go_chart 'HasLegend' = 1.
  GET PROPERTY OF go_chart 'Legend'  = go_legend.
  CALL METHOD OF go_legend 'Select'.
  SET PROPERTY OF go_legend 'Position'  =  '-4160'.

  SET PROPERTY OF go_chart 'ChartType' = '65' .
  CALL METHOD OF go_chart 'SetSourceData'
    EXPORTING
      #1 = go_range
      #2 = 1.
  CALL METHOD OF go_worksheet3 'ACTIVATE'.

  CALL METHOD OF go_chart 'Location'
    EXPORTING
      #1 = 2
      #2 = gv_sheet3_name.
  CALL METHOD OF go_worksheet3 'ChartObjects' = go_chartobjects .
  SET PROPERTY OF go_chartobjects 'Left' = 1 .
  SET PROPERTY OF go_chartobjects 'Top' = 30 .
  SET PROPERTY OF go_chartobjects 'Height' = 600 .
  SET PROPERTY OF go_chartobjects 'Width' = 1000 .

ENDFORM.
FORM display_section6 .
  DATA lv_lines TYPE sy-tabix .
  DATA : lv_ind_unit   TYPE char50,
         lv_total_unit TYPE char50,
         lv_gt_unit    TYPE char50 . "grand total

  gv_row   = 44 .
  gv_col   = 1  .

  PERFORM prepare_paste_data TABLES <gfs_sec6_table> .
  DESCRIBE TABLE gt_paste LINES lv_lines .
  CHECK lv_lines GT 0 .

  gv_row   = gv_row + 1 .
  gv_s_row = gv_row .
  gv_e_row = gv_s_row + 1 .
  gv_s_col = 1 .
  gv_e_col = 1 .

  CONCATENATE 'ONGC Videsh' gv_current_gjahr '-' gv_next_gjahr+2(2) INTO gv_txt .

  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  CALL METHOD OF go_range 'Merge' .
  PERFORM set_range USING gv_txt 0.

  gv_e_row = gv_s_row .
  gv_s_col = gv_s_col + 1 .
  gv_e_col = gv_s_col + 1 .

  PERFORM get_unit_desc USING c_prod_oil CHANGING lv_ind_unit lv_total_unit .
  CONCATENATE 'Oil, LNG & Condensate (' lv_ind_unit ')' INTO gv_txt SEPARATED BY space.
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  CALL METHOD OF go_range 'Merge' .
  PERFORM set_range USING gv_txt 0.

  gv_e_row = gv_s_row .
  gv_s_col = gv_e_col + 1 .
  gv_e_col = gv_s_col + 1 .

  PERFORM get_unit_desc USING c_prod_gas CHANGING lv_ind_unit lv_total_unit .
  CONCATENATE 'Gas (' lv_ind_unit ')' INTO gv_txt SEPARATED BY space.
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  CALL METHOD OF go_range 'Merge' .
  PERFORM set_range USING gv_txt 0.

  CASE abap_true.
    WHEN p_bb.
      lv_gt_unit   = 'BOE' .
    WHEN p_bbd.
      lv_gt_unit   = 'BOEPD' .
    WHEN p_tm.
      lv_gt_unit   = 'TOE' .
    WHEN p_tmd.
      lv_gt_unit   = 'TOEPD' .
    WHEN p_mb.
      lv_gt_unit   = 'MMTOE' .
    WHEN p_bmd.
      lv_gt_unit   = 'BOEPD' .
    WHEN OTHERS.
  ENDCASE.

  gv_e_row = gv_s_row .
  gv_s_col = gv_e_col + 1 .
  gv_e_col = gv_s_col + 1 .

  CONCATENATE 'Total (O+OEG) (' lv_gt_unit ')' INTO gv_txt SEPARATED BY space.
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  CALL METHOD OF go_range 'Merge' .
  PERFORM set_range USING gv_txt 0.

  gv_s_row = gv_s_row + 1 .
  gv_e_row = gv_s_row .
  gv_s_col =  2 .
  gv_e_col =  2 .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM set_range USING 'Annual' 0.

  gv_s_col = gv_s_col +  1 .
  gv_e_col = gv_e_col +  1 .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM set_range USING 'YTD' 0.

  gv_s_col = gv_s_col +  1 .
  gv_e_col = gv_e_col +  1 .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM set_range USING 'Annual' 0.

  gv_s_col = gv_s_col +  1 .
  gv_e_col = gv_e_col +  1 .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM set_range USING 'YTD' 0.

  gv_s_col = gv_s_col +  1 .
  gv_e_col = gv_e_col +  1 .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM set_range USING 'Annual' 0.

  gv_s_col = gv_s_col +  1 .
  gv_e_col = gv_e_col +  1 .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM set_range USING 'YTD' 0.

  PERFORM select_range USING gv_row 1 gv_e_row gv_e_col  .
  PERFORM set_range_font  USING 13 1 .
  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = 12611584 .
  GET PROPERTY OF go_range 'FONT' = go_font .
  SET PROPERTY OF go_font 'COLOR' = '-460552' .
  PERFORM set_range_formatting USING  0 'C' 'C' .
  PERFORM set_all_borders_range .

  gv_row   = gv_row + 2 .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row + lv_lines - 1 .
  gv_e_col = 7 .

  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM paste_data_sheet3 .
  PERFORM set_all_borders_range .

  CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '12'.
  SET PROPERTY OF go_border 'LineStyle' = '1' .
  CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '10'.
  SET PROPERTY OF go_border 'LineStyle' = '1' .

  gv_row = gv_e_row .
  gv_s_col = gv_s_col + 1 .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM set_range_formatting USING  0 'C' 'C' .
  PERFORM set_numberformat USING '0.000' .

  PERFORM col_width USING 1 1 '27'  .
  PERFORM col_width USING 2 7 '16'  .

  PERFORM select_range USING 1 1 1 1  .

ENDFORM.
FORM display_section3f .
  DATA lv_lines TYPE sy-tabix .
  DESCRIBE TABLE <gfs_sec3f_table> LINES lv_lines .
  CHECK lv_lines IS NOT INITIAL .
  gv_row   = gv_row + 1 .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row + lv_lines - 1 .
  gv_e_col = gv_s_col + gv_table_columns - 1.
  PERFORM prepare_paste_data TABLES <gfs_sec3f_table> .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM paste_data .

  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = gv_sec3f_colour .
  gv_row = gv_e_row .

  PERFORM select_range USING gv_s_row 1 gv_e_row 1 .
  CALL METHOD OF go_range 'Merge' .
  gv_txt = 'Actual Production'.
  PERFORM set_range USING gv_txt 1.

ENDFORM.

FORM display_section3d .
  DATA lv_lines TYPE sy-tabix .
  DESCRIBE TABLE <gfs_sec3d_table> LINES lv_lines .
  CHECK lv_lines IS NOT INITIAL .
  gv_row   = gv_row + 1 .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row + lv_lines - 1 .
  gv_e_col = gv_s_col + gv_table_columns - 1.
  PERFORM prepare_paste_data TABLES <gfs_sec3d_table> .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM paste_data .

  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = gv_sec3d_colour .
  gv_row = gv_e_row .

  PERFORM select_range USING gv_s_row 1 gv_e_row 1 .
  CALL METHOD OF go_range 'Merge' .
  gv_txt = '% Achiev. wrt YTD Target' .
  PERFORM set_range USING gv_txt 1.

ENDFORM.
FORM display_section3e .
  DATA lv_lines TYPE sy-tabix .
  DESCRIBE TABLE <gfs_sec3e_table> LINES lv_lines .
  CHECK lv_lines IS NOT INITIAL .
  gv_row   = gv_row + 1 .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row + lv_lines - 1 .
  gv_e_col = gv_s_col + gv_table_columns - 1.
  PERFORM prepare_paste_data TABLES <gfs_sec3e_table> .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM paste_data .

  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = gv_sec3e_colour .
  gv_row = gv_e_row .

  PERFORM select_range USING gv_s_row 1 gv_e_row 1 .
  CALL METHOD OF go_range 'Merge' .
  gv_txt = '% Achiev. wrt Yearly Target' .
  PERFORM set_range USING gv_txt 1.

  PERFORM show_progress USING '40' .
ENDFORM.

FORM start_excel .
  CREATE OBJECT go_excel 'excel.application' .
  SET PROPERTY OF go_excel 'VISIBLE' = 0 .
  CALL METHOD OF go_excel 'WORKBOOKS' = go_workbooks .

  gv_sheet1_name = '1' .
  gv_sheet2_name = '2' .
  gv_sheet3_name = 'Production Performance' .

  CALL METHOD OF go_workbooks 'ADD' = go_workbook .
  GET PROPERTY OF go_workbook 'Application' = go_application .
  CALL METHOD OF go_excel 'WORKSHEETS' = go_worksheet3
  EXPORTING
    #1 =  1.
  GET PROPERTY OF go_excel 'Sheets' = go_sheets .
  CALL METHOD OF go_sheets 'Add' = go_worksheet2 .
  CALL METHOD OF go_sheets 'Add' = go_worksheet .
  SET PROPERTY OF go_worksheet  'Name' = gv_sheet1_name .
  SET PROPERTY OF go_worksheet2 'Name' = gv_sheet2_name .
  SET PROPERTY OF go_worksheet3 'Name' = gv_sheet3_name .

  CALL METHOD OF go_worksheet 'ACTIVATE'.
  SET PROPERTY OF go_excel 'PrintCommunication' = abap_false.
  CALL METHOD OF go_worksheet 'PAGESETUP' = go_pagesetup .
  SET PROPERTY OF go_pagesetup 'FitToPagesWide' = 1 .
  SET PROPERTY OF go_pagesetup 'FitToPagesTall' = 0 .
  SET PROPERTY OF go_pagesetup 'Orientation' = 2 .
  SET PROPERTY OF go_excel 'PrintCommunication' = abap_true.

  SET PROPERTY OF go_excel 'PrintCommunication' = abap_false.
  CALL METHOD OF go_worksheet3 'PAGESETUP' = go_pagesetup .
  SET PROPERTY OF go_pagesetup 'FitToPagesWide' = 1 .
  SET PROPERTY OF go_pagesetup 'FitToPagesTall' = 0 .
  SET PROPERTY OF go_pagesetup 'Orientation' = 2 .
  SET PROPERTY OF go_excel 'PrintCommunication' = abap_true.


  gv_row = 1 .
ENDFORM.
FORM display_section1_header .
  PERFORM display_logo .
  PERFORM display_report_date .
  PERFORM display_product_names .
  PERFORM display_asset_names .
  PERFORM display_pi          .
  PERFORM display_cf          .
  PERFORM display_consortium_level .
  PERFORM display_units .
  PERFORM join_header_total_cells .
  PERFORM join_header1_column_1_2 .
  PERFORM set_section1_header_colors .
ENDFORM.
FORM display_section1_data .
  DATA : lv_lines TYPE sy-tabix,
         lv_col   TYPE sy-tabix.
  DESCRIBE TABLE <gfs_dyn_table> LINES lv_lines .
  gv_row = gv_row + 1 .
  gv_sec1_data_start_row = gv_row .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row + lv_lines - 1 .
  gv_e_col = gv_s_col + gv_table_columns - 1.
  PERFORM prepare_paste_data TABLES <gfs_dyn_table> .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM paste_data .
  PERFORM colour_alternate_rows .
  PERFORM colour_yellow_cells .
  PERFORM colour_dates .
  PERFORM colour_sec1_data_totals .
  gv_row = gv_e_row .
  gv_sec2_start_row = gv_row + 1.

  lv_col = 3 .
  PERFORM select_range USING gv_sec1_data_start_row lv_col gv_row lv_lines  .
  PERFORM set_numberformat USING '0.000' .
ENDFORM.
FORM formatting_section1 .
  PERFORM merge_col_1_2_section1 .
  PERFORM format_sec1_header .
  PERFORM format_sec1_data_col_1_2 .
ENDFORM .
FORM select_cell  USING    p_row
                           p_col.
  CALL METHOD OF go_excel 'Cells' = go_cell
                         EXPORTING #1 = p_row #2 = p_col.
ENDFORM.
FORM set_cell  USING    p_cell_value
                        p_wraptext  .
  SET PROPERTY OF go_cell 'Value' = p_cell_value .
  SET PROPERTY OF go_cell 'WrapText' = p_wraptext .
ENDFORM.

FORM select_range  USING    p_s_row
                            p_s_col
                            p_e_row
                            p_e_col .

  CALL METHOD OF go_excel 'Cells' = go_cell_from
   EXPORTING
   #1 = p_s_row
   #2 = p_s_col .

  CALL METHOD OF go_excel 'Cells' = go_cell_to
   EXPORTING
   #1 = p_e_row
   #2 = p_e_col .

  CALL METHOD OF go_excel 'Range' = go_range
    EXPORTING
    #1 = go_cell_from
    #2 = go_cell_to.

  CALL METHOD OF go_range 'Select' .

ENDFORM.
FORM set_range  USING    p_cell_value
                        p_wraptext  .
  SET PROPERTY OF go_range 'Value' = p_cell_value .
  SET PROPERTY OF go_range 'WrapText' = p_wraptext .
ENDFORM.
FORM set_range_font  USING    p_size
                              p_bold.
  GET PROPERTY OF go_range 'FONT' = go_font .
  SET PROPERTY OF go_font  'BOLD' = p_bold .
  SET PROPERTY OF go_font  'Size' = p_size .
ENDFORM.

FORM set_range_formatting USING p_wraptext
                                p_horizontal
                                p_vertical .
  SET PROPERTY OF go_range 'WrapText' = p_wraptext .
  IF p_horizontal EQ 'C'.
    SET PROPERTY OF go_range 'HorizontalAlignment' =  -4108 .
  ELSEIF  p_horizontal EQ 'L'.
    SET PROPERTY OF go_range 'HorizontalAlignment' =  -4131 .
  ELSEIF  p_horizontal EQ 'R'.
    SET PROPERTY OF go_range 'HorizontalAlignment' =  -4152 .
  ENDIF.
  IF p_vertical EQ 'C' .
    SET PROPERTY OF go_range 'VerticalAlignment' = -4108 .
  ELSEIF p_vertical EQ 'T' .
    SET PROPERTY OF go_range 'VerticalAlignment' = -4160 .
  ELSEIF p_vertical EQ 'B' .
    SET PROPERTY OF go_range 'VerticalAlignment' = -4107 .
  ENDIF.

ENDFORM .
FORM set_thin_border   USING    p_left
                                p_right
                                p_top
                                p_bottom .
  IF p_left EQ 1 .
    CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '7' .
    SET PROPERTY OF go_border 'LineStyle' = '1'  .
  ENDIF.
  IF p_right EQ 1 .
    CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '10'.
    SET PROPERTY OF go_border 'LineStyle' = '1' .
  ENDIF.
  IF p_top EQ 1 .
    CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '8'.
    SET PROPERTY OF go_border 'LineStyle' = '1' .
  ENDIF.
  IF p_bottom EQ 1 .
    CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '9'.
    SET PROPERTY OF go_border 'LineStyle' = '1' .
  ENDIF.
ENDFORM.

FORM row_height  USING  p_row
                        p_height .
  CALL METHOD OF go_excel 'ROWS' = go_row
    EXPORTING
      #1 = p_row .
*CALL METHOD OF go_row 'Select' .
  SET PROPERTY OF go_row 'Rowheight' = p_height.

ENDFORM.
FORM merge_col_1_2_section1 .
  DATA lv_row TYPE sy-tabix .
  PERFORM select_range USING gv_sec1_h_start_row 1 gv_sec1_h_start_row 2 .
  go_range0 = go_range .
  CALL METHOD OF go_range0 'Copy' .
  lv_row = gv_sec1_h_start_row + 1 .
  PERFORM select_range USING lv_row 1 gv_row 2 .
  CALL METHOD OF go_range 'PasteSpecial'
    EXPORTING
      #1 = -4122.

  PERFORM select_range USING lv_row 1 gv_row 2  .
  PERFORM set_numberformat USING 'dd-mmm-yyyy'.

ENDFORM.
FORM format_sec1_header .
  DATA : lv_row  TYPE sy-tabix .
  lv_row = gv_sec1_h_start_row + 1 .
  PERFORM select_range USING gv_sec1_h_start_row 1 lv_row gv_table_columns .
  PERFORM set_range_formatting USING  1 'C' 'C' .
  PERFORM row_height USING gv_sec1_h_start_row 15 .
  PERFORM row_height USING lv_row 45 .
ENDFORM .
FORM format_sec1_data_col_1_2 .
  PERFORM select_range USING gv_sec1_data_start_row 1 gv_row 2 .
  PERFORM set_range_formatting USING  0 'C' 'C' .
ENDFORM.
FORM add_product_range  USING  p_product.

  CLEAR r_product .

  r_product-sign   = 'I' .
  r_product-option = 'EQ' .
  r_product-low    = p_product .

  APPEND r_product TO r_product[] .
ENDFORM.
FORM add_vl_type_range  USING  p_vl_type .

  CLEAR r_prd_vl_type .

  r_prd_vl_type-sign   = 'I' .
  r_prd_vl_type-option = 'EQ' .
  r_prd_vl_type-low    = p_vl_type .

  APPEND r_prd_vl_type TO r_prd_vl_type[] .
ENDFORM.
FORM buiild_tar_code_range .
  REFRESH r_tar_code[] .
  IF p_t_be IS NOT INITIAL.
    PERFORM add_tar_code_range USING 'TAR_BE' .
  ENDIF.
  IF p_t_in IS NOT INITIAL.
    PERFORM add_tar_code_range USING 'TAR_IN' .
  ENDIF.
  IF p_t_ex IS NOT INITIAL.
    PERFORM add_tar_code_range USING 'TAR_EX' .
  ENDIF.
  IF p_t_vg IS NOT INITIAL.
    PERFORM add_tar_code_range USING 'TAR_VG' .
  ENDIF.
  IF p_t_pc IS NOT INITIAL.
    PERFORM add_tar_code_range USING 'TAR_PC' .
  ENDIF.
  IF p_t_re IS NOT INITIAL.
    PERFORM add_tar_code_range USING 'TAR_RE' .
  ENDIF.
ENDFORM .
FORM add_tar_code_range  USING  p_tar_code .

  CLEAR r_tar_code .

  r_tar_code-sign   = 'I' .
  r_tar_code-option = 'EQ' .
  r_tar_code-low    = p_tar_code .

  APPEND r_tar_code TO r_tar_code[] .
ENDFORM.
FORM calculated_wtd_pi .

  DATA : lt_dly_prd         TYPE STANDARD TABLE OF zpra_t_dly_prd,
         lt_wtd_pi          TYPE STANDARD TABLE OF ty_wtd_pi,
         lt_zpra_c_prd_prof TYPE STANDARD TABLE OF ty_zpra_c_prd_prof.
  DATA : ls_dly_prd         TYPE                   zpra_t_dly_prd,
         ls_wtd_pi          TYPE                   ty_wtd_pi,
         ls_wtd_pi_t        TYPE                   ty_wtd_pi,
         ls_dly_prd_temp    TYPE                   zpra_t_dly_prd,
         ls_zpra_c_prd_prof TYPE                   ty_zpra_c_prd_prof.
  DATA : lv_index       TYPE                   sy-tabix,
         lv_numerator   TYPE                   p LENGTH 16 DECIMALS 9,
         lv_denominator TYPE                   p LENGTH 16 DECIMALS 9,
         lv_pi          TYPE                   zpra_t_prd_pi-pi,
         lv_index1      TYPE                   sy-tabix,
         lv_date1       TYPE                   sy-datum,
         lv_date2       TYPE                   sy-datum.
  DATA : lt_zpra_t_ast_pi TYPE STANDARD TABLE OF zpra_t_ast_pi,
         ls_zpra_t_ast_pi TYPE zpra_t_ast_pi.

  FIELD-SYMBOLS : <fs_wtd_pi> TYPE ty_wtd_pi .
* Here we are calculating weighted PI for each asset for each day. This will be used to convert quantities to OVL level
* Formula used is wtd pi = sum of all block of assets' pi * quantity  divided by sum of all blocks quantity
  REFRESH gt_wtd_pi .
  lt_dly_prd = gt_zpra_t_dly_prd .

  PERFORM populate_no_data_entries TABLES lt_dly_prd USING gv_month_back_datum p_date .

  SORT lt_dly_prd BY production_date DESCENDING product ASCENDING asset ASCENDING block ASCENDING .

  LOOP AT lt_dly_prd INTO ls_dly_prd.
    lv_index = sy-tabix .
    PERFORM convert_non_gas_units_2a2 CHANGING ls_dly_prd.

****    IF ls_dly_prd-prod_vl_qty1 IS INITIAL.
***** When some blocks are shut down, then quantity comes as zero; but due to this, PI is calculated as 0
***** to avoid this making the quantity as 1 so that PI is calculated rightly
****     ls_dly_prd-prod_vl_qty1 = 1 .
****    ENDIF.
    IF ( ls_dly_prd-product         EQ ls_dly_prd_temp-product          AND
         ls_dly_prd-asset           EQ ls_dly_prd_temp-asset            AND
         ls_dly_prd-production_date EQ  ls_dly_prd_temp-production_date AND
         ls_dly_prd-block           NE ls_dly_prd_temp-block )
        OR lv_index = 1 .
      IF ls_dly_prd-prod_vl_qty1 IS NOT INITIAL .
        CLEAR lv_pi .
        LOOP AT gt_zpra_t_prd_pi INTO gs_zpra_t_prd_pi WHERE asset   EQ ls_dly_prd-asset
                                                         AND block   EQ ls_dly_prd-block
                                                         AND vld_frm LE ls_dly_prd-production_date
                                                         AND vld_to  GE ls_dly_prd-production_date .
          lv_pi = gs_zpra_t_prd_pi-pi .
          EXIT .
        ENDLOOP .
        lv_numerator   = lv_numerator   + ( ls_dly_prd-prod_vl_qty1  * lv_pi ).
        lv_denominator = lv_denominator + ls_dly_prd-prod_vl_qty1 .
      ENDIF.
    ENDIF.
    IF ( ls_dly_prd-production_date NE ls_dly_prd_temp-production_date OR
         ls_dly_prd-product         NE ls_dly_prd_temp-product   OR
         ls_dly_prd-asset           NE ls_dly_prd_temp-asset  ) AND
         lv_index NE 1.

      gs_wtd_pi-production_date     = ls_dly_prd_temp-production_date   .
      gs_wtd_pi-product             = ls_dly_prd_temp-product           .
      gs_wtd_pi-asset               = ls_dly_prd_temp-asset             .
      gs_wtd_pi-numerator           = lv_numerator                      .
      gs_wtd_pi-denominator         = lv_denominator                    .

      IF lv_denominator IS INITIAL.
        gs_wtd_pi-pi = 0 .
      ELSE.
        gs_wtd_pi-pi = lv_numerator / lv_denominator .
      ENDIF.
      APPEND gs_wtd_pi TO gt_wtd_pi .
      CLEAR : lv_numerator   ,
              lv_denominator .

      IF ls_dly_prd-prod_vl_qty1 IS NOT INITIAL .
        CLEAR lv_pi .
        LOOP AT gt_zpra_t_prd_pi INTO gs_zpra_t_prd_pi WHERE asset   EQ ls_dly_prd-asset
                                                         AND block   EQ ls_dly_prd-block
                                                         AND vld_frm LE ls_dly_prd-production_date
                                                         AND vld_to  GE ls_dly_prd-production_date .
          lv_pi = gs_zpra_t_prd_pi-pi .
          EXIT .
        ENDLOOP .
        lv_numerator   = lv_numerator   + ( ls_dly_prd-prod_vl_qty1 * lv_pi ).
        lv_denominator = lv_denominator + ls_dly_prd-prod_vl_qty1 .
      ENDIF.
    ENDIF.
    ls_dly_prd_temp = ls_dly_prd .
  ENDLOOP.

  gs_wtd_pi-production_date = ls_dly_prd_temp-production_date  .
  gs_wtd_pi-product = ls_dly_prd_temp-product  .
  gs_wtd_pi-asset   = ls_dly_prd_temp-asset    .
  gs_wtd_pi-numerator   = lv_numerator         .
  gs_wtd_pi-denominator = lv_denominator       .

  IF lv_denominator IS INITIAL.
    gs_wtd_pi-pi = 0 .
  ELSE.
    gs_wtd_pi-pi = lv_numerator / lv_denominator .
  ENDIF.
  APPEND gs_wtd_pi TO gt_wtd_pi .
* Putting previous day's PI if it is not there
  SORT gt_wtd_pi BY production_date product asset .

  lt_zpra_c_prd_prof = gt_zpra_c_prd_prof .
  SORT lt_zpra_c_prd_prof BY product asset .
  DELETE ADJACENT DUPLICATES FROM lt_zpra_c_prd_prof COMPARING product asset .

  LOOP AT lt_zpra_c_prd_prof INTO ls_zpra_c_prd_prof..
    lv_date1 = gv_month_back_datum .
    DO .
      READ TABLE gt_wtd_pi INTO ls_wtd_pi_t WITH KEY production_date = lv_date1
                                                             product = ls_zpra_c_prd_prof-product
                                                             asset   = ls_zpra_c_prd_prof-asset BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL OR ls_wtd_pi_t-pi IS INITIAL.
        lv_date2 = lv_date1 - 1.
        READ TABLE gt_wtd_pi INTO ls_wtd_pi WITH KEY production_date = lv_date2
                                                             product = ls_zpra_c_prd_prof-product
                                                             asset   = ls_zpra_c_prd_prof-asset BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          ls_wtd_pi-production_date = ls_wtd_pi-production_date + 1 .
          APPEND ls_wtd_pi TO gt_wtd_pi .
          SORT gt_wtd_pi BY production_date product asset .
        ENDIF.
      ENDIF.
      IF lv_date1 GE p_date.
        EXIT .
      ENDIF.
      lv_date1 = lv_date1 + 1 .
    ENDDO.
  ENDLOOP.

  DELETE ADJACENT DUPLICATES FROM gt_wtd_pi COMPARING ALL FIELDS.
* Pre-fetch zpra_t_ast_pi for all assets to avoid SELECT SINGLE inside loop
  IF gt_zpra_c_prd_prof IS NOT INITIAL.
    SELECT *
      FROM zpra_t_ast_pi
      INTO TABLE lt_zpra_t_ast_pi
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE asset EQ gt_zpra_c_prd_prof-asset.
    SORT lt_zpra_t_ast_pi BY asset vld_frm vld_to.
  ENDIF.
* If on last date no data of any block of asset is maintained, PI at top will not be shown. Calcuating that
  LOOP AT gt_zpra_c_prd_prof INTO gs_zpra_c_prd_prof.
    READ TABLE gt_wtd_pi INTO gs_wtd_pi WITH KEY production_date = p_date
                                                         product = gs_zpra_c_prd_prof-product
                                                         asset   = gs_zpra_c_prd_prof-asset.
    IF sy-subrc IS NOT INITIAL .
      CLEAR lv_pi .
      LOOP AT lt_zpra_t_ast_pi INTO ls_zpra_t_ast_pi
        WHERE asset   EQ gs_zpra_c_prd_prof-asset
          AND vld_frm LE p_date
          AND vld_to  GE p_date.
        lv_pi = ls_zpra_t_ast_pi-pi.
        EXIT.
      ENDLOOP.
      IF lv_pi IS NOT INITIAL.
        gs_wtd_pi-production_date  = p_date .
        gs_wtd_pi-product          = gs_zpra_c_prd_prof-product .
        gs_wtd_pi-asset            = gs_zpra_c_prd_prof-asset .
        gs_wtd_pi-pi = lv_pi .
        APPEND gs_wtd_pi TO gt_wtd_pi .
      ENDIF.
    ELSEIF   gs_wtd_pi-pi IS INITIAL .
      lv_index = sy-tabix .
      CLEAR lv_pi .
      LOOP AT lt_zpra_t_ast_pi INTO ls_zpra_t_ast_pi
        WHERE asset   EQ gs_wtd_pi-asset
          AND vld_frm LE gs_wtd_pi-production_date
          AND vld_to  GE gs_wtd_pi-production_date.
        lv_pi = ls_zpra_t_ast_pi-pi.
        EXIT.
      ENDLOOP.
      IF lv_pi IS NOT INITIAL.
        gs_wtd_pi-pi = lv_pi .
        MODIFY gt_wtd_pi FROM gs_wtd_pi INDEX lv_index .
      ENDIF.
    ENDIF.
  ENDLOOP.
* We have some assets in GAS shown as combinded, so need to calculate combined PI of all those assets
  IF r_combine_asset[] IS NOT INITIAL.
    CLEAR : lv_numerator   ,
            lv_denominator .
    lt_wtd_pi = gt_wtd_pi .
    DELETE gt_wtd_pi WHERE product EQ c_prod_gas
                       AND asset   IN r_combine_asset[] .
    DELETE lt_wtd_pi WHERE product NE c_prod_gas .
    DELETE lt_wtd_pi WHERE asset  NOT IN r_combine_asset[] .
    SORT lt_wtd_pi BY production_date .
    LOOP AT lt_wtd_pi INTO ls_wtd_pi.
      lv_index = sy-tabix .
      AT NEW production_date .
        IF lv_index NE 1 .
          IF lv_denominator IS INITIAL.
            <fs_wtd_pi>-pi = 0 .
          ELSE.
            <fs_wtd_pi>-pi = lv_numerator / lv_denominator .
            <fs_wtd_pi>-numerator   = lv_numerator .
            <fs_wtd_pi>-denominator = lv_denominator .
          ENDIF.
        ENDIF.
        CLEAR : lv_numerator   ,
                lv_denominator .
        APPEND INITIAL LINE TO gt_wtd_pi ASSIGNING <fs_wtd_pi> .
        <fs_wtd_pi>-production_date = ls_wtd_pi-production_date  .
        <fs_wtd_pi>-product         = c_prod_gas  .
        <fs_wtd_pi>-asset           = 'COMBINE'    .
      ENDAT .
      lv_numerator   = lv_numerator   + ls_wtd_pi-numerator .
      lv_denominator = lv_denominator + ls_wtd_pi-denominator .
    ENDLOOP.
    IF lv_denominator IS INITIAL.
      <fs_wtd_pi>-pi = 0 .
    ELSE.
      <fs_wtd_pi>-pi = lv_numerator / lv_denominator .
      <fs_wtd_pi>-numerator   = lv_numerator .
      <fs_wtd_pi>-denominator = lv_denominator .
    ENDIF.

  ENDIF.


ENDFORM.
FORM calculated_wtd_cf .
  DATA : ls_zpra_t_mrec_prd_temp TYPE            ty_zpra_t_mrec_prd .
  DATA : lv_index       TYPE                   sy-tabix,
         lv_numerator   TYPE                   zpra_t_dly_prd-prod_vl_qty1,
         lv_denominator TYPE                   zpra_t_dly_prd-prod_vl_qty1,
         lv_count       TYPE                   sy-tabix.

  LOOP AT gt_zpra_t_mrec_prd INTO gs_zpra_t_mrec_prd.
    lv_index = sy-tabix .
    IF lv_index NE 1.
      IF ls_zpra_t_mrec_prd_temp-product EQ gs_zpra_t_mrec_prd-product AND
         ls_zpra_t_mrec_prd_temp-asset   EQ gs_zpra_t_mrec_prd-asset   AND
         ( ls_zpra_t_mrec_prd_temp-monat NE gs_zpra_t_mrec_prd-monat OR
         ls_zpra_t_mrec_prd_temp-gjahr   NE gs_zpra_t_mrec_prd-gjahr ).
        CONTINUE .
      ENDIF.
    ENDIF.
    IF ( gs_zpra_t_mrec_prd-product EQ ls_zpra_t_mrec_prd_temp-product AND
         gs_zpra_t_mrec_prd-asset   EQ ls_zpra_t_mrec_prd_temp-asset   )
         OR lv_index = 1 .
      IF gs_zpra_t_mrec_prd-prod_vl_qty2 IS NOT INITIAL .
        lv_numerator   = lv_numerator   + gs_zpra_t_mrec_prd-prod_vl_qty1 .
        lv_denominator = lv_denominator + gs_zpra_t_mrec_prd-prod_vl_qty2.
      ENDIF.
    ENDIF.
    IF ( gs_zpra_t_mrec_prd-product NE ls_zpra_t_mrec_prd_temp-product OR
         gs_zpra_t_mrec_prd-asset   NE ls_zpra_t_mrec_prd_temp-asset ) AND
         lv_index NE 1.
      gs_wtd_cf-product     = ls_zpra_t_mrec_prd_temp-product  .
      gs_wtd_cf-asset       = ls_zpra_t_mrec_prd_temp-asset    .

      IF lv_denominator IS INITIAL.
        gs_wtd_cf-cf = 0 .
      ELSE.
        gs_wtd_cf-cf = lv_numerator / lv_denominator .
      ENDIF.
      APPEND gs_wtd_cf TO gt_wtd_cf .
      CLEAR : lv_numerator   ,
              lv_denominator .

      IF gs_zpra_t_mrec_prd-prod_vl_qty2 IS NOT INITIAL .
        lv_numerator   = lv_numerator   + gs_zpra_t_mrec_prd-prod_vl_qty1 .
        lv_denominator = lv_denominator + gs_zpra_t_mrec_prd-prod_vl_qty2.
      ENDIF.
    ENDIF.
    ls_zpra_t_mrec_prd_temp = gs_zpra_t_mrec_prd .
  ENDLOOP.

  gs_wtd_cf-product     = ls_zpra_t_mrec_prd_temp-product  .
  gs_wtd_cf-asset       = ls_zpra_t_mrec_prd_temp-asset    .

  IF lv_denominator IS INITIAL.
    gs_wtd_cf-cf = 0 .
  ELSE.
    gs_wtd_cf-cf = lv_numerator / lv_denominator .
  ENDIF.
  APPEND gs_wtd_cf TO gt_wtd_cf .

  SORT gt_wtd_cf BY product asset .
  LOOP AT gt_zpra_c_prd_prof INTO gs_zpra_c_prd_prof.
    READ TABLE gt_wtd_cf INTO gs_wtd_cf WITH KEY product = gs_zpra_c_prd_prof-product
                                                   asset = gs_zpra_c_prd_prof-asset BINARY SEARCH .
    IF sy-subrc IS NOT INITIAL.
      CLEAR lv_count .
      CLEAR gs_wtd_cf .
      LOOP AT gt_zpra_t_tar_cf INTO gs_zpra_t_tar_cf WHERE   gjahr EQ gv_current_gjahr
                                                       AND product EQ gs_zpra_c_prd_prof-product
                                                       AND   asset EQ gs_zpra_c_prd_prof-asset
                                                       AND   block EQ gs_zpra_c_prd_prof-block .
        lv_count = lv_count + 1 .
        gs_wtd_cf-product = gs_zpra_c_prd_prof-product .
        gs_wtd_cf-asset   = gs_zpra_c_prd_prof-asset   .
        gs_wtd_cf-cf      = gs_wtd_cf-cf + gs_zpra_t_tar_cf-conv_factor .
      ENDLOOP .
      IF lv_count IS NOT INITIAL.
        gs_wtd_cf-cf = gs_wtd_cf-cf / lv_count .
        APPEND gs_wtd_cf TO gt_wtd_cf .
        SORT gt_wtd_cf BY product asset .
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM process_gas_records .
  DATA : lt_zpra_t_dly_prd  TYPE STANDARD TABLE OF zpra_t_dly_prd  .

  DATA : ls_zpra_t_dly_prd   TYPE                  zpra_t_dly_prd  .
  FIELD-SYMBOLS : <fs_dly_prd> TYPE zpra_t_dly_prd  .

  DATA : lv_index TYPE sy-tabix .
  lt_zpra_t_dly_prd  = gt_zpra_t_dly_prd .
  SORT  lt_zpra_t_dly_prd BY production_date product asset block prd_vl_type .

  DELETE gt_zpra_t_dly_prd  WHERE product EQ c_prod_gas .
  DELETE lt_zpra_t_dly_prd  WHERE product NE c_prod_gas .

  IF lt_zpra_t_dly_prd IS NOT INITIAL.
    LOOP AT lt_zpra_t_dly_prd INTO ls_zpra_t_dly_prd .
      lv_index = sy-tabix .
      IF lv_index EQ 1 OR
        <fs_dly_prd>-production_date NE ls_zpra_t_dly_prd-production_date  OR
        <fs_dly_prd>-product         NE ls_zpra_t_dly_prd-product          OR
        <fs_dly_prd>-asset           NE ls_zpra_t_dly_prd-asset            OR
        <fs_dly_prd>-block           NE ls_zpra_t_dly_prd-block .

        APPEND INITIAL LINE TO gt_zpra_t_dly_prd ASSIGNING <fs_dly_prd> .
        <fs_dly_prd>-mandt           = ls_zpra_t_dly_prd-mandt .
        <fs_dly_prd>-production_date = ls_zpra_t_dly_prd-production_date .
        <fs_dly_prd>-product         = ls_zpra_t_dly_prd-product .
        <fs_dly_prd>-asset           = ls_zpra_t_dly_prd-asset .
        <fs_dly_prd>-block           = ls_zpra_t_dly_prd-block .
        <fs_dly_prd>-prod_vl_uom1    = ls_zpra_t_dly_prd-prod_vl_uom1 .
        <fs_dly_prd>-prd_vl_type     = 'NET_PROD' .
      ENDIF.
      PERFORM convert_gas_units CHANGING ls_zpra_t_dly_prd.
      IF ls_zpra_t_dly_prd-prd_vl_type EQ 'GAS_INJ' .
        ls_zpra_t_dly_prd-prod_vl_qty1 = ls_zpra_t_dly_prd-prod_vl_qty1 * -1 .
      ENDIF.
      <fs_dly_prd>-prod_vl_qty1    = <fs_dly_prd>-prod_vl_qty1 + ls_zpra_t_dly_prd-prod_vl_qty1 .
    ENDLOOP.
  ENDIF.

  lt_zpra_t_dly_prd = gt_zpra_t_dly_prd .
*gt_zdpr_gas_combine
  LOOP AT  lt_zpra_t_dly_prd INTO ls_zpra_t_dly_prd.
    lv_index = sy-tabix .
    IF ls_zpra_t_dly_prd-product NE c_prod_gas.
      DELETE lt_zpra_t_dly_prd INDEX lv_index .
      CONTINUE .
    ENDIF.
    READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY ls_zpra_t_dly_prd-asset BINARY SEARCH .
    IF sy-subrc IS NOT INITIAL.
      DELETE lt_zpra_t_dly_prd INDEX lv_index .
    ENDIF.
  ENDLOOP.

  LOOP AT  gt_zpra_t_dly_prd INTO ls_zpra_t_dly_prd.
    lv_index = sy-tabix .
    IF ls_zpra_t_dly_prd-product EQ c_prod_gas.
      READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY ls_zpra_t_dly_prd-asset BINARY SEARCH .
      IF sy-subrc IS INITIAL.
        DELETE gt_zpra_t_dly_prd INDEX lv_index .
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM populate_no_data_entries TABLES lt_zpra_t_dly_prd USING gv_month_back_datum p_date .

  APPEND LINES OF lt_zpra_t_dly_prd TO gt_zpra_t_dly_prd .

  SORT  gt_zpra_t_dly_prd BY production_date product asset .


  lt_zpra_t_dly_prd  = gt_zpra_t_dly_prd_mb .
  SORT  lt_zpra_t_dly_prd BY production_date product asset block prd_vl_type .

  DELETE gt_zpra_t_dly_prd_mb  WHERE product EQ c_prod_gas .
  DELETE lt_zpra_t_dly_prd     WHERE product NE c_prod_gas .
  IF lt_zpra_t_dly_prd IS NOT INITIAL.

    LOOP AT lt_zpra_t_dly_prd INTO ls_zpra_t_dly_prd .
      lv_index = sy-tabix .
      IF lv_index EQ 1 OR
        <fs_dly_prd>-production_date NE ls_zpra_t_dly_prd-production_date  OR
        <fs_dly_prd>-product         NE ls_zpra_t_dly_prd-product          OR
        <fs_dly_prd>-asset           NE ls_zpra_t_dly_prd-asset            OR
        <fs_dly_prd>-block           NE ls_zpra_t_dly_prd-block .

        APPEND INITIAL LINE TO gt_zpra_t_dly_prd_mb ASSIGNING <fs_dly_prd> .
        <fs_dly_prd>-mandt           = ls_zpra_t_dly_prd-mandt .
        <fs_dly_prd>-production_date = ls_zpra_t_dly_prd-production_date .
        <fs_dly_prd>-product         = ls_zpra_t_dly_prd-product .
        <fs_dly_prd>-asset           = ls_zpra_t_dly_prd-asset .
        <fs_dly_prd>-block           = ls_zpra_t_dly_prd-block .
        <fs_dly_prd>-prod_vl_uom1    = ls_zpra_t_dly_prd-prod_vl_uom1 .
        <fs_dly_prd>-prd_vl_type     = 'NET_PROD' .
      ENDIF.
      PERFORM convert_gas_units CHANGING ls_zpra_t_dly_prd.
      IF ls_zpra_t_dly_prd-prd_vl_type EQ 'GAS_INJ' .
        ls_zpra_t_dly_prd-prod_vl_qty1 = ls_zpra_t_dly_prd-prod_vl_qty1 * -1 .
      ENDIF.
      <fs_dly_prd>-prod_vl_qty1    = <fs_dly_prd>-prod_vl_qty1 + ls_zpra_t_dly_prd-prod_vl_qty1 .
    ENDLOOP.
  ENDIF.
  SORT  gt_zpra_t_dly_prd_mb BY production_date product asset .

ENDFORM.
FORM fill_null_values_with_previous .

  DATA : ls_zpra_t_dly_prd TYPE zpra_t_dly_prd .
  DATA  :lv_col_index     TYPE sy-tabix,
         lv_lines         TYPE sy-tabix,
         lv_combine_field TYPE c,
         lv_date          TYPE sy-datum,
         lv_col_name      TYPE lvc_fname.

  FIELD-SYMBOLS : <lfs_dyn_line> ,
                  <lfs_field> ,
                  <lfs_field_previous> .
  UNASSIGN : <lfs_dyn_line> ,
             <lfs_field>    ,
             <lfs_field_previous> .
  DESCRIBE TABLE <gfs_dyn_table> LINES lv_lines .
  lv_lines = lv_lines - 1 .
  IF lv_lines GT 0.
    READ TABLE <gfs_dyn_table> ASSIGNING <lfs_dyn_line> INDEX lv_lines .
  ENDIF.
  DO .
    lv_col_index = sy-index .
    ASSIGN COMPONENT lv_col_index OF STRUCTURE <gfs_dyn_line> TO <lfs_field> .
    IF sy-subrc IS NOT INITIAL.
      EXIT .
    ENDIF.
    READ TABLE gt_dyn_fcat INTO gs_dyn_fcat WITH KEY col_pos = lv_col_index BINARY SEARCH .
    IF sy-subrc IS INITIAL.
      SPLIT gs_dyn_fcat-fieldname AT '-' INTO ls_zpra_t_dly_prd-product ls_zpra_t_dly_prd-asset .
      IF NOT ( gs_dyn_fcat-fieldname CS 'COL01' OR
               gs_dyn_fcat-fieldname CS 'COL02' OR
               gs_dyn_fcat-fieldname CS 'TOTAL' ).
        IF <lfs_field> EQ ' '.
          IF <lfs_dyn_line> IS ASSIGNED.
            ASSIGN COMPONENT lv_col_index OF STRUCTURE <lfs_dyn_line> TO <lfs_field_previous> .
            <lfs_field> = <lfs_field_previous> .
            IF ls_zpra_t_dly_prd-product = c_prod_gas.
              CLEAR lv_combine_field .
              READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = ls_zpra_t_dly_prd-asset BINARY SEARCH .
              IF  sy-subrc IS INITIAL.
                lv_combine_field = 'X' .
              ENDIF.
            ENDIF.
            lv_date = gs_zpra_t_dly_prd-production_date - 1 .
            ls_zpra_t_dly_prd-production_date = lv_date .
            IF p_c_jv IS NOT INITIAL .
              PERFORM get_pi_for_null_values USING ls_zpra_t_dly_prd lv_combine_field.
            ELSE.
              gv_total_mul       = 1 .
              gv_grand_total_mul = 1 .
              IF p_bmd IS NOT INITIAL AND ls_zpra_t_dly_prd-product EQ c_prod_gas.
*                 gv_grand_total_mul = gv_grand_total_mul * 6290 .
              ENDIF.
            ENDIF.
* Product Total..
            gv_len = strlen( ls_zpra_t_dly_prd-product ) .
            CONCATENATE ls_zpra_t_dly_prd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
            ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <lfs_field> .
            IF <lfs_field> IS ASSIGNED.
              <lfs_field> = <lfs_field> + <lfs_field_previous> * gv_total_mul .
              UNASSIGN <lfs_field> .
            ENDIF.
*Grand Total
            CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
            ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <lfs_field> .
            IF <lfs_field> IS ASSIGNED.
              <lfs_field> = <lfs_field> + <lfs_field_previous> * gv_grand_total_mul .
              UNASSIGN <lfs_field> .
            ENDIF.
          ELSE.
            <lfs_field> = '0.00' .
* Product Total..
            gv_len = strlen( ls_zpra_t_dly_prd-product ) .
            CONCATENATE ls_zpra_t_dly_prd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
            ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <lfs_field> .
            IF <lfs_field> IS ASSIGNED.
              <lfs_field> = <lfs_field> + '0.00' .
              UNASSIGN <lfs_field> .
            ENDIF.
*Grand Total
            CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
            ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <lfs_field> .
            IF <lfs_field> IS ASSIGNED.
              <lfs_field> = <lfs_field> + '0.00' .
              UNASSIGN <lfs_field> .
            ENDIF.

          ENDIF.
          gs_copied_cells-row = lv_lines + 1 .
          gs_copied_cells-col = lv_col_index .
          APPEND gs_copied_cells TO gt_copied_cells .
        ENDIF.
      ENDIF.
    ENDIF.
    UNASSIGN : <lfs_field> ,
               <lfs_field_previous> .
  ENDDO.
  UNASSIGN : <lfs_dyn_line> .
ENDFORM.
FORM paste_data .
  DO 10 TIMES .
    CALL METHOD cl_gui_frontend_services=>clipboard_export
      IMPORTING
        data         = gt_paste
      CHANGING
        rc           = gv_rc
      EXCEPTIONS
        cntl_error   = 1
        error_no_gui = 2
        OTHERS       = 4.
    IF sy-subrc IS INITIAL AND gv_rc IS INITIAL.
      EXIT .
    ENDIF.
  ENDDO.
  DO 10 TIMES .
    CALL METHOD OF go_worksheet 'Paste'.
    IF sy-subrc IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.
FORM paste_data_sheet3 .
  DO 10 TIMES .
    CALL METHOD cl_gui_frontend_services=>clipboard_export
      IMPORTING
        data         = gt_paste
      CHANGING
        rc           = gv_rc
      EXCEPTIONS
        cntl_error   = 1
        error_no_gui = 2
        OTHERS       = 4.
    IF sy-subrc IS INITIAL AND gv_rc IS INITIAL.
      EXIT .
    ENDIF.
  ENDDO.
  DO 10 TIMES .
    CALL METHOD OF go_worksheet3 'Paste'.
    IF sy-subrc IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.

ENDFORM.
FORM paste_data_sheet2 .
  DO 10 TIMES .
    CALL METHOD cl_gui_frontend_services=>clipboard_export
      IMPORTING
        data         = gt_paste
      CHANGING
        rc           = gv_rc
      EXCEPTIONS
        cntl_error   = 1
        error_no_gui = 2
        OTHERS       = 4.
    IF sy-subrc IS INITIAL AND gv_rc IS INITIAL.
      EXIT .
    ENDIF.
  ENDDO.
  DO 10 TIMES .
    CALL METHOD OF go_worksheet2 'Paste'.
    IF sy-subrc IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.
FORM colour_yellow_cells .
  DATA : lv_row TYPE sy-tabix .
  LOOP AT gt_copied_cells INTO gs_copied_cells.
    lv_row = gv_row + gs_copied_cells-row - 1 .
    CALL METHOD OF go_excel 'Cells' = go_cell
     EXPORTING
     #1 = lv_row
     #2 = gs_copied_cells-col .

    GET PROPERTY OF go_cell 'interior' = go_interior .
    SET PROPERTY OF go_interior 'Color' = 65535 .

    GET PROPERTY OF go_cell 'FONT' = go_font .
    SET PROPERTY OF go_font 'COLOR' = '-16776961' .
  ENDLOOP.
ENDFORM.
FORM display_logo .
  PERFORM download_image .
  PERFORM display_image  .
  PERFORM delete_image   .

  PERFORM show_progress USING '0' .
ENDFORM.
FORM download_image .

  DATA : l_bytecount TYPE i,
         l_tdbtype   LIKE stxbitmaps-tdbtype,
         l_content   TYPE STANDARD TABLE OF bapiconten INITIAL SIZE 0.

  DATA: graphic_size TYPE i.

  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.

  CONCATENATE p_fname '\' sy-datum sy-uzeit '.bmp' INTO gv_image_name .
  CALL FUNCTION 'SAPSCRIPT_GET_GRAPHIC_BDS'
    EXPORTING
      i_object       = 'GRAPHICS'
      i_name         = 'OVL_FULL'
      i_id           = 'BMAP'
      i_btype        = 'BCOL'
    IMPORTING
      e_bytecount    = l_bytecount
    TABLES
      content        = l_content
    EXCEPTIONS
      not_found      = 1
      bds_get_failed = 2
      bds_no_content = 3
      OTHERS         = 4.

  CALL FUNCTION 'SAPSCRIPT_CONVERT_BITMAP'
    EXPORTING
      old_format               = 'BDS'
      new_format               = 'BMP'
      bitmap_file_bytecount_in = l_bytecount
    IMPORTING
      bitmap_file_bytecount    = graphic_size
    TABLES
      bds_bitmap_file          = l_content
      bitmap_file              = graphic_table
    EXCEPTIONS
      OTHERS                   = 1.

  CALL FUNCTION 'WS_DOWNLOAD'
    EXPORTING
      bin_filesize            = graphic_size
      filename                = gv_image_name
      filetype                = 'BIN'
    TABLES
      data_tab                = graphic_table
    EXCEPTIONS
      invalid_filesize        = 1
      invalid_table_width     = 2
      invalid_type            = 3
      no_batch                = 4
      unknown_error           = 5
      gui_refuse_filetransfer = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
FORM display_image .
  DATA lo_shapes TYPE ole2_object .


  GET PROPERTY OF go_worksheet 'Shapes' = lo_shapes.

  CALL METHOD OF lo_shapes 'AddPicture'
    EXPORTING
      #1 = gv_image_name "image file name on presentation server
      #2 = '1'
      #3 = '1'
      #4 = 1      "left
      #5 = 1      "top
      #6 = 390    "right
      #7 = 45.    "bottom

  gv_row = gv_row + 2 .
ENDFORM.
FORM delete_image .
  DATA : lv_file TYPE string,
         lv_rc   TYPE i.
  lv_file = gv_image_name .

  CALL METHOD cl_gui_frontend_services=>file_delete
    EXPORTING
      filename             = lv_file
    CHANGING
      rc                   = lv_rc
    EXCEPTIONS
      file_delete_failed   = 1
      cntl_error           = 2
      error_no_gui         = 3
      file_not_found       = 4
      access_denied        = 5
      unknown_error        = 6
      not_supported_by_gui = 7
      wrong_parameter      = 8
      OTHERS               = 9.
ENDFORM.
FORM display_report_date .
  DATA : lv_date    TYPE char50,
         lv_heading TYPE char100.

  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
    EXPORTING
      input  = gv_rep_date
    IMPORTING
      output = lv_date.
  REPLACE ALL OCCURRENCES OF '.' IN lv_date WITH '-' .

  CONCATENATE 'Daily Production Report :' lv_date INTO lv_heading SEPARATED BY space .

  gv_row = gv_row + 1 .
  PERFORM select_range USING gv_row 1 gv_row 5  .
  CALL METHOD OF go_range 'Merge' .
  PERFORM set_range USING lv_heading 0.

  PERFORM set_range_formatting USING  1 'L' 'C' .
  PERFORM set_range_interior USING gv_header_colour .
  GET PROPERTY OF go_range 'FONT' = go_font .
  SET PROPERTY OF go_font  'BOLD' = 1 .

ENDFORM.
FORM display_product_names .
  DATA : lv_separator TYPE char10,
         lv_no_comma  TYPE c.

  CLEAR gv_grand_total_desc .
  gv_row = gv_row + 1 .
  gv_start_row = gv_row .
  gv_sec1_h_start_row = gv_row .
  gv_col = 1 .
  gv_s_row = gv_row .
  gv_s_col = gv_col .
  gv_e_row = gv_s_row .
  gv_e_col = gv_col .
  PERFORM select_cell USING gv_s_row gv_s_col .
  PERFORM set_cell USING 'Product' 0.
  CLEAR gv_txt .
  gv_col = gv_col + 1 .

  LOOP AT r_product.
    CASE r_product-low .
      WHEN c_prod_oil.
        gv_txt =  c_oil_desc.
      WHEN c_prod_con.
        gv_txt = c_con_desc .
      WHEN c_prod_lng.
        gv_txt = c_lng_desc .
      WHEN c_prod_gas.
        gv_txt = c_gas_desc .
      WHEN OTHERS.
    ENDCASE.
    PERFORM write_product_name USING r_product-low.
  ENDLOOP.
  LOOP AT gt_product_col INTO gs_product_col.
    AT FIRST .
      gv_grand_total_desc = 'Total' .
      lv_no_comma  = 'X' .
    ENDAT .
    AT LAST .
      IF sy-tabix NE 1 .
        CONCATENATE gv_grand_total_desc 'and' INTO gv_grand_total_desc SEPARATED BY space .
      ENDIF.
      lv_no_comma  = 'X' .
    ENDAT .
    IF lv_no_comma IS INITIAL.
      CONCATENATE gv_grand_total_desc  ',' INTO gv_grand_total_desc .
    ENDIF.
    CONCATENATE gv_grand_total_desc gs_product_col-desc INTO gv_grand_total_desc SEPARATED BY space .
    CLEAR lv_no_comma .
  ENDLOOP.
*  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
ENDFORM.
FORM display_asset_names .
  DATA : lv_product    TYPE zpra_t_dly_prd-product,
         lv_asset      TYPE zpra_t_dly_prd-asset,
         lv_asset_desc TYPE char512,
         lv_asset_pre  TYPE char512,
         lv_index      TYPE sy-tabix.
  DATA : v_expdt_cnt   TYPE i,
         lv_expdt_lines TYPE sy-tabix.
  DATA : lt_expdt     TYPE STANDARD TABLE OF zpra_t_prd_pi,
         ls_expdt     TYPE zpra_t_prd_pi,
         lt_all_prd_pi TYPE STANDARD TABLE OF zpra_t_prd_pi.
  CLEAR   gs_paste .
  REFRESH gt_paste .

* Pre-fetch zpra_t_prd_pi for all assets to avoid SELECT inside loop
  IF gt_zpra_c_prd_prof IS NOT INITIAL.
    SELECT *
      FROM zpra_t_prd_pi
      INTO TABLE lt_all_prd_pi
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE asset EQ gt_zpra_c_prd_prof-asset.
    SORT lt_all_prd_pi BY asset liscense_exp_dt.
  ENDIF.

  LOOP AT gt_dyn_fcat INTO gs_dyn_fcat .
    IF gs_dyn_fcat-fieldname EQ 'COL01'.
      lv_asset_desc = 'Asset' .
    ELSEIF gs_dyn_fcat-fieldname EQ 'COL02'.
    ELSEIF gs_dyn_fcat-fieldname EQ 'GRAND-TOTAL' .
      lv_asset_desc = gv_grand_total_desc .
    ELSEIF gs_dyn_fcat-fieldname CS 'TOTAL' .
      SPLIT gs_dyn_fcat-fieldname AT '-' INTO lv_product lv_asset .
      CASE lv_product .
        WHEN c_prod_oil.
          lv_asset_desc =  c_oil_desc.
        WHEN c_prod_con.
          lv_asset_desc = c_con_desc .
        WHEN c_prod_lng.
          lv_asset_desc = c_lng_desc .
        WHEN c_prod_gas.
          lv_asset_desc = c_gas_desc .
        WHEN OTHERS.
      ENDCASE.
      CONCATENATE 'Total' lv_asset_desc INTO lv_asset_desc SEPARATED BY space.
    ELSE .
      SPLIT gs_dyn_fcat-fieldname AT '-' INTO lv_product lv_asset .
      IF lv_asset NE 'COMBINE'.
        READ TABLE gt_asset_desc INTO gs_asset_desc WITH KEY asset = lv_asset BINARY SEARCH .
        IF sy-subrc IS INITIAL.
          lv_asset_desc = gs_asset_desc-desc .
*          SELECT SINGLE LISCENSE_EXP_DT from ZPRA_T_PRD_PI INTO @data(v_expdt)
*            WHERE ASSET = @lv_asset.
*            IF  v_expdt is NOT INITIAL and v_expdt LT P_DATE.
*            CONCATENATE lv_asset_desc '*' INTO lv_asset_desc.
*            ENDIF
************************************Changes by hrishikesh nikam on 31.03.2022*************************************
          REFRESH lt_expdt.
          lt_expdt = lt_all_prd_pi.
          DELETE lt_expdt WHERE asset NE lv_asset.
          IF lt_expdt IS NOT INITIAL.
            DESCRIBE TABLE lt_expdt LINES lv_expdt_lines.
            CLEAR v_expdt_cnt.
            LOOP AT lt_expdt INTO ls_expdt
              WHERE asset          = lv_asset
                AND liscense_exp_dt LT p_date.
              IF ls_expdt-liscense_exp_dt GT '00000000'.
                v_expdt_cnt = v_expdt_cnt + 1.
              ENDIF.
              CLEAR ls_expdt.
            ENDLOOP.
            IF v_expdt_cnt = lv_expdt_lines.
              CONCATENATE lv_asset_desc '*' INTO lv_asset_desc.
            ENDIF.
          ENDIF.
*******************************************************************************************************************
        ENDIF.
      ELSE.

        LOOP AT r_combine_asset.
          lv_index = sy-tabix .
          READ TABLE gt_asset_desc INTO gs_asset_desc WITH KEY asset = r_combine_asset-low BINARY SEARCH .
          IF sy-subrc IS INITIAL.
            SPLIT gs_asset_desc-desc AT ',' INTO lv_asset_pre gs_asset_desc-desc .
            CONDENSE gs_asset_desc-desc .
            IF lv_index NE 1.
              CONCATENATE lv_asset_desc ', ' INTO lv_asset_desc .
            ENDIF.
            CONCATENATE lv_asset_desc gs_asset_desc-desc INTO lv_asset_desc .
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
    IF gs_dyn_fcat-col_pos NE 1.
      CONCATENATE gs_paste cl_abap_char_utilities=>horizontal_tab INTO gs_paste .
    ENDIF.
    CONCATENATE gs_paste lv_asset_desc INTO gs_paste .
    CLEAR : gs_asset_desc , lv_asset_desc .
  ENDLOOP.

  APPEND gs_paste TO gt_paste .

  gv_row = gv_row + 1 .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row .
  gv_e_col = gv_s_col + gv_table_columns - 1.

  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM paste_data .

ENDFORM.
FORM display_pi .

  DATA : lv_product TYPE zpra_t_dly_prd-product,
         lv_asset   TYPE zpra_t_dly_prd-asset,
         lv_pi      TYPE char50,
         lv_col     TYPE sy-tabix.
  CLEAR   gs_paste .
  REFRESH gt_paste .
  SORT gt_wtd_pi BY product ASCENDING asset ASCENDING production_date DESCENDING .
  LOOP AT gt_dyn_fcat INTO gs_dyn_fcat .
    IF gs_dyn_fcat-fieldname EQ 'COL01'.
      lv_pi = 'ONGC VideshShare (%)' .
    ELSEIF gs_dyn_fcat-fieldname EQ 'COL01'.
    ELSEIF gs_dyn_fcat-fieldname CS 'TOTAL' .
*      gs_asset_desc-desc = '' .
    ELSE .
      SPLIT gs_dyn_fcat-fieldname AT '-' INTO lv_product lv_asset .
      IF lv_asset NE 'COMBINE'.
        CLEAR gs_wtd_pi .
        READ TABLE gt_wtd_pi INTO gs_wtd_pi WITH KEY product = lv_product
                                                     asset   = lv_asset .
        IF sy-subrc IS INITIAL.
          lv_pi = gs_wtd_pi-pi .
        ENDIF.
      ELSE.
        CLEAR gs_wtd_pi .
        READ TABLE gt_wtd_pi INTO gs_wtd_pi WITH KEY product = lv_product
                                                     asset   = 'COMBINE' .
        IF sy-subrc IS INITIAL.
          lv_pi = gs_wtd_pi-pi .
        ENDIF.
      ENDIF.
    ENDIF.
    IF gs_dyn_fcat-col_pos NE 1.
      CONCATENATE gs_paste cl_abap_char_utilities=>horizontal_tab INTO gs_paste .
    ENDIF.
    CONCATENATE gs_paste lv_pi INTO gs_paste .
    CLEAR lv_pi .
  ENDLOOP.

  APPEND gs_paste TO gt_paste .

  gv_row = gv_row + 1 .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row .
  gv_e_col = gv_s_col + gv_table_columns - 1.

  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM paste_data .

  lv_col = 3 .
  PERFORM select_range USING gv_s_row lv_col gv_e_row gv_e_col  .
  PERFORM set_numberformat USING '0.000' .

ENDFORM.
FORM display_cf .

  DATA : lv_product TYPE zpra_t_dly_prd-product,
         lv_asset   TYPE zpra_t_dly_prd-asset,
         lv_cf      TYPE char50,
         lv_col     TYPE sy-tabix.
  CLEAR   gs_paste .
  REFRESH gt_paste .

  LOOP AT gt_dyn_fcat INTO gs_dyn_fcat .
    IF gs_dyn_fcat-fieldname EQ 'COL01'.
      lv_cf = 'Conversion Factor (bbl/tonnes)' .
    ELSEIF gs_dyn_fcat-fieldname CS 'COL02' .
    ELSEIF gs_dyn_fcat-fieldname CS 'TOTAL' .
*      gs_asset_desc-desc = '' .
    ELSE .
      SPLIT gs_dyn_fcat-fieldname AT '-' INTO lv_product lv_asset .
      IF lv_asset NE 'COMBINE'.
        CLEAR gs_wtd_pi .
        READ TABLE gt_wtd_cf INTO gs_wtd_cf WITH KEY product = lv_product
                                                           asset   = lv_asset BINARY SEARCH .
        IF sy-subrc IS INITIAL.
          lv_cf = gs_wtd_cf-cf .
        ENDIF.
      ENDIF.
    ENDIF.
    IF gs_dyn_fcat-col_pos NE 1.
      CONCATENATE gs_paste cl_abap_char_utilities=>horizontal_tab INTO gs_paste .
    ENDIF.
    CONCATENATE gs_paste lv_cf INTO gs_paste .
    CLEAR lv_cf .
  ENDLOOP.

  APPEND gs_paste TO gt_paste .

  gv_row = gv_row + 1 .
  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row .
  gv_e_col = gv_s_col + gv_table_columns - 1.

  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM paste_data .

  lv_col = 3 .

  PERFORM select_range USING gv_s_row lv_col gv_e_row gv_e_col  .
  PERFORM set_numberformat USING '0.000' .

ENDFORM.
FORM display_consortium_level .
  CONSTANTS : c_ongc TYPE char50 VALUE 'ONGC Videsh',
              c_jvl  TYPE char50 VALUE 'JV'.
  DATA : lv_consortium_txt TYPE char50 .

  IF p_c_jv IS NOT INITIAL.
    lv_consortium_txt = c_jvl  .
  ELSE .
    lv_consortium_txt = c_ongc .
  ENDIF.

  gv_row = gv_row + 1 .
  gv_col = 1 .
  gv_s_row = gv_row .
  gv_s_col = gv_col .
  gv_e_row = gv_s_row .
  gv_e_col = gv_col .
  PERFORM select_cell USING gv_s_row gv_s_col .
  PERFORM set_cell USING 'Consortium Level' 0.
  CLEAR gv_txt .
  gv_col = gv_col + 2 .

  LOOP AT gt_product_col INTO gs_product_col.
    gv_s_col = gs_product_col-s_col .
    gv_e_col = gs_product_col-e_col - 1 .
    PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
    CALL METHOD OF go_range 'Merge' .
    PERFORM set_range USING lv_consortium_txt 0.
    PERFORM set_range_formatting USING 0 'C' 'C' .

    gv_s_col = gs_product_col-e_col .
    gv_e_col = gv_s_col .

    PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
    PERFORM set_range USING c_ongc  0.
    PERFORM set_range_formatting USING 0 'C' 'C' .

  ENDLOOP.

  gv_s_col = gv_s_col + 1 .
  gv_e_col = gv_s_col .

  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM set_range USING c_ongc  0.
  PERFORM set_range_formatting USING 0 'C' 'C' .

ENDFORM .
FORM display_units .
  DATA : lv_ind_unit   TYPE char50 , "non gas
         lv_total_unit TYPE char50 , "gas
         lv_gt_unit    TYPE char50 . "grand total

  gv_row = gv_row + 1 .
  gv_col = 1 .
  gv_s_row = gv_row .
  gv_s_col = gv_col .
  gv_e_row = gv_s_row .
  gv_e_col = gv_col .
  PERFORM select_cell USING gv_s_row gv_s_col .
  PERFORM set_cell USING 'Date/Unit' 0.
  CLEAR gv_txt .
  gv_col = gv_col + 2 .

  LOOP AT gt_product_col INTO gs_product_col.
    gv_s_col = gs_product_col-s_col .
    gv_e_col = gs_product_col-e_col - 1 .
    PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
    CALL METHOD OF go_range 'Merge' .

    PERFORM get_unit_desc USING gs_product_col-product CHANGING lv_ind_unit lv_total_unit .

    PERFORM set_range USING lv_ind_unit 0.
    PERFORM set_range_formatting USING 0 'C' 'C' .
    gv_s_col = gs_product_col-e_col .
    gv_e_col = gv_s_col .

    PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
    PERFORM set_range USING lv_total_unit  0.
    PERFORM set_range_formatting USING 0 'C' 'C' .

  ENDLOOP.

  gv_s_col = gv_s_col + 1 .
  gv_e_col = gv_s_col .

  CASE abap_true.
    WHEN p_bb.
      lv_gt_unit   = 'BOE' .
    WHEN p_bbd.
      lv_gt_unit   = 'BOEPD' .
    WHEN p_tm.
      lv_gt_unit   = 'TOE' .
    WHEN p_tmd.
      lv_gt_unit   = 'TOEPD' .
    WHEN p_mb.
      lv_gt_unit   = 'MMTOE' .
    WHEN p_bmd.
      lv_gt_unit   = 'BOEPD' .
    WHEN OTHERS.
  ENDCASE.

  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM set_range USING lv_gt_unit  0.
  PERFORM set_range_formatting USING 0 'C' 'C' .

ENDFORM .
FORM get_unit_desc  USING    p_product
                    CHANGING p_ind_unit
                             p_total_unit.
  CASE p_product.
    WHEN c_prod_oil.
      CASE abap_true.
        WHEN p_bb.
          p_ind_unit   = 'BBL' .
          p_total_unit = 'BBL' .
        WHEN p_bbd.
          p_ind_unit   = 'BOPD' .
          p_total_unit = 'BOPD' .
        WHEN p_tm.
          p_ind_unit   = 'Tons' .
          p_total_unit = 'Tons' .
        WHEN p_tmd.
          p_ind_unit   = 'TPD' .
          p_total_unit = 'TPD' .
        WHEN p_mb.
          p_ind_unit   = 'MMT' .
          p_total_unit = 'MMT' .
        WHEN p_bmd.
          p_ind_unit   = 'BOPD' .
          p_total_unit = 'BOPD' .
        WHEN OTHERS.
      ENDCASE.
    WHEN c_prod_con.
      CASE abap_true.
        WHEN p_bb.
          p_ind_unit   = 'BBL' .
          p_total_unit = 'BBL' .
        WHEN p_bbd.
          p_ind_unit   = 'BCPD' .
          p_total_unit = 'BCPD' .
        WHEN p_tm.
          p_ind_unit   = 'Tons' .
          p_total_unit = 'Tons' .
        WHEN p_tmd.
          p_ind_unit   = 'TPD' .
          p_total_unit = 'TPD' .
        WHEN p_mb.
          p_ind_unit   = 'MMT' .
          p_total_unit = 'MMT' .
        WHEN p_bmd.
          p_ind_unit   = 'BCPD' .
          p_total_unit = 'BCPD' .
        WHEN OTHERS.
      ENDCASE.
    WHEN c_prod_lng.
      CASE abap_true.
        WHEN p_bb.
          p_ind_unit   = 'BOE' .
          p_total_unit = 'BOE' .
        WHEN p_bbd.
          p_ind_unit   = 'BOEPD' .
          p_total_unit = 'BOEPD' .
        WHEN p_tm.
          p_ind_unit   = 'Tons' .
          p_total_unit = 'Tons' .
        WHEN p_tmd.
          p_ind_unit   = 'TPD' .
          p_total_unit = 'TPD' .
        WHEN p_mb.
          p_ind_unit   = 'MMT' .
          p_total_unit = 'MMT' .
        WHEN p_bmd.
          p_ind_unit   = 'MMSCMD' .
          p_total_unit = 'MMSCMD' .
        WHEN OTHERS.
      ENDCASE.
    WHEN c_prod_gas.
      CASE abap_true.
        WHEN p_bb.
          p_ind_unit   = 'BOE' .
          p_total_unit = 'BOE' .
        WHEN p_bbd.
          p_ind_unit   = 'BOEPD' .
          p_total_unit = 'BOEPD' .
        WHEN p_tm.
          p_ind_unit   = 'MSCM' .
          p_total_unit = 'MSCM' .
        WHEN p_tmd.
          p_ind_unit   = 'MSCMD' .
          p_total_unit = 'MSCMD' .
        WHEN p_mb.
          p_ind_unit   = 'BCM' .
          p_total_unit = 'BCM' .
        WHEN p_bmd.
          p_ind_unit   = 'MMSCMD' .
          p_total_unit = 'MMSCMD' .
        WHEN OTHERS.
      ENDCASE.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
FORM write_product_name  USING    p_product.
  DATA : lv_product   TYPE zpra_t_dly_prd-product,
         lv_asset     TYPE zpra_t_dly_prd-asset,
         lv_col_count TYPE sy-tabix.

  LOOP AT gt_dyn_fcat INTO gs_dyn_fcat.
    IF gs_dyn_fcat-fieldname EQ 'COL01'.
    ELSEIF gs_dyn_fcat-fieldname EQ 'COL02'.
    ELSE.
      SPLIT gs_dyn_fcat-fieldname AT '-' INTO lv_product lv_asset .
      IF lv_product EQ p_product .
        IF NOT lv_asset CS 'TOTAL'.
          lv_col_count = lv_col_count + 1.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF lv_col_count IS NOT INITIAL.
    gv_col   = gv_col + 1 .
    gv_s_col = gv_col .
    gv_e_col = gv_col + lv_col_count - 1 .
    gv_col   = gv_e_col .
    PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .

    CALL METHOD OF go_range 'Merge' .
    PERFORM set_range USING gv_txt 0.
    gv_col   = gv_col + 1 .
    gs_product_col-product = p_product .
    gs_product_col-s_col   = gv_s_col  .
    gs_product_col-e_col   = gv_e_col + 1 .
    gs_product_col-desc    = gv_txt    .
    APPEND gs_product_col TO gt_product_col .

  ENDIF.

ENDFORM.
FORM colour_alternate_rows .
  DATA : lv_lines       TYPE sy-tabix,
         lv_switch_flag TYPE c.
  DESCRIBE TABLE <gfs_dyn_table> LINES lv_lines .
  DO lv_lines TIMES.
    IF lv_switch_flag EQ 'X'.
      CLEAR lv_switch_flag .
      gv_s_row = gv_s_row + 1 .
      CONTINUE .
    ENDIF.
    PERFORM select_range USING gv_s_row gv_s_col gv_s_row gv_e_col  .
    PERFORM set_range_interior USING 14540253 .
    lv_switch_flag = 'X'.
    gv_s_row = gv_s_row + 1 .
  ENDDO.
ENDFORM.
FORM set_range_interior  USING    p_color.
  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = p_color .
ENDFORM.
FORM set_numberformat USING p_format.
  SET PROPERTY OF go_range 'NumberFormat' = p_format .
ENDFORM.

FORM set_section1_header_colors .
  DATA lv_color TYPE sy-tabix .
  PERFORM select_range USING gv_start_row 1 gv_row 2 .
  PERFORM set_range_interior USING gv_dates_colour .

  LOOP AT gt_product_col INTO gs_product_col.
    PERFORM select_range USING gv_start_row gs_product_col-s_col gv_row gs_product_col-e_col .
    CASE  gs_product_col-product.
      WHEN '722000001'.
        lv_color = gv_header_oil_colour .
      WHEN '722000003'.
        lv_color = gv_header_cond_colour .
      WHEN '722000005'.
        lv_color = gv_header_lng_colour .
      WHEN '722000004'.
        lv_color = gv_header_gas_colour .
      WHEN OTHERS.
    ENDCASE.
    PERFORM set_range_interior USING lv_color .
  ENDLOOP.
  PERFORM select_range USING gv_start_row gv_table_columns gv_row gv_table_columns .
  PERFORM set_range_interior USING gv_header_gt_colour .
ENDFORM.
FORM convert_gas_units  CHANGING p_zpra_t_dly_prd TYPE zpra_t_dly_prd.
  DATA : lv_qty TYPE char50 .
* First Convert to MCM

  CASE p_zpra_t_dly_prd-prod_vl_uom1 .
    WHEN 'MCM'.
      lv_qty = p_zpra_t_dly_prd-prod_vl_qty1.
    WHEN 'M3'.
      lv_qty = p_zpra_t_dly_prd-prod_vl_qty1 / 1000000 .
    WHEN 'MCF'.
      lv_qty = p_zpra_t_dly_prd-prod_vl_qty1 / '35.3'.
    WHEN OTHERS.
  ENDCASE.

* Convert to display UoM
  CASE abap_true.
    WHEN p_bb.
      p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty * 6290 .
    WHEN p_bbd.
      p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty * 6290 .
    WHEN p_tm.
      p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty * 1000 .
    WHEN p_tmd.
      p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty * 1000 .
    WHEN p_mb.
      p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty * 1000 .
    WHEN p_bmd.
      p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty * 6290.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
FORM convert_gas_units2  CHANGING p_zpra_t_dly_prd TYPE zpra_t_dly_prd.
  DATA : lv_qty TYPE char50 .
* First Convert to MCM

  CASE p_zpra_t_dly_prd-prod_vl_uom1 .
    WHEN 'MCM'.
      lv_qty = p_zpra_t_dly_prd-prod_vl_qty1.
    WHEN 'M3'.
      lv_qty = p_zpra_t_dly_prd-prod_vl_qty1 / 1000000 .
    WHEN 'MCF'.
      lv_qty = p_zpra_t_dly_prd-prod_vl_qty1 / '35.3'.
    WHEN OTHERS.
  ENDCASE.

* Convert to display UoM
  CASE abap_true.
    WHEN p_bb.
      p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty .              "* 6290 .
    WHEN p_bbd.
      p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty .              "* 6290 .
    WHEN p_tm.
      p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty * 1000 .
    WHEN p_tmd.
      p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty * 1000 .
    WHEN p_mb.
      p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty * 1000 .
    WHEN p_bmd.
      p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty .              "* 6290.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
FORM convert_gas_units_to_bopd  CHANGING p_zpra_t_dly_prd TYPE zpra_t_dly_prd.
  DATA : lv_qty TYPE char50 .
* First Convert to MCM

  CASE p_zpra_t_dly_prd-prod_vl_uom1 .
    WHEN 'MCM'.
      lv_qty = p_zpra_t_dly_prd-prod_vl_qty1.
    WHEN 'M3'.
      lv_qty = p_zpra_t_dly_prd-prod_vl_qty1 / 1000000 .
    WHEN 'MCF'.
      lv_qty = p_zpra_t_dly_prd-prod_vl_qty1 / '35.3'.
    WHEN OTHERS.
  ENDCASE.

  p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty * 6290 .

ENDFORM.
FORM convert_gas_units_to_bcm  CHANGING p_zpra_t_dly_prd TYPE zpra_t_dly_prd.
  DATA : lv_qty TYPE char50 .
* First Convert to MCM

  CASE p_zpra_t_dly_prd-prod_vl_uom1 .
    WHEN 'MCM'.
      lv_qty = p_zpra_t_dly_prd-prod_vl_qty1.
    WHEN 'M3'.
      lv_qty = p_zpra_t_dly_prd-prod_vl_qty1 / 1000000 .
    WHEN 'MCF'.
      lv_qty = p_zpra_t_dly_prd-prod_vl_qty1 / '35.3'.
    WHEN OTHERS.
  ENDCASE.
* BCM being small unit, dividing here makes decimals error.
* Will divide at end..
* p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty / 1000 .
  p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty * 1000 .

ENDFORM.
FORM convert_gas_units_to_mmscm  CHANGING p_zpra_t_dly_prd TYPE zpra_t_dly_prd.
  DATA : lv_qty TYPE char50 .
* First Convert to MCM
* Multiplying by 1000 due to decimal precision, will divide later
  CASE p_zpra_t_dly_prd-prod_vl_uom1 .
    WHEN 'MCM'.
      lv_qty = 1000 * p_zpra_t_dly_prd-prod_vl_qty1 .
    WHEN 'M3'.
      lv_qty = 1000 * p_zpra_t_dly_prd-prod_vl_qty1 / 1000000 .
    WHEN 'MCF'.
      lv_qty = 1000 * p_zpra_t_dly_prd-prod_vl_qty1 / '35.3'.
    WHEN OTHERS.
  ENDCASE.

* p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty * 6290 .
  p_zpra_t_dly_prd-prod_vl_qty1 = lv_qty  .

ENDFORM.

FORM convert_mrec_gas_units  CHANGING p_zpra_t_mrec_prd TYPE ty_zpra_t_mrec_prd.
  DATA : lv_qty TYPE char50 .
* First Convert to MCM

  CASE p_zpra_t_mrec_prd-prod_vl_uom1 .
    WHEN 'MCM'.
      lv_qty = p_zpra_t_mrec_prd-prod_vl_qty1.
    WHEN 'M3'.
      lv_qty = p_zpra_t_mrec_prd-prod_vl_qty1 / 1000000 .
    WHEN 'MCF'.
      lv_qty = p_zpra_t_mrec_prd-prod_vl_qty1 / '35.3'.
    WHEN OTHERS.
  ENDCASE.

* Convert to display UoM
  CASE abap_true.
    WHEN p_bb.
      p_zpra_t_mrec_prd-prod_vl_qty1 = lv_qty * 6290 .
    WHEN p_bbd.
      p_zpra_t_mrec_prd-prod_vl_qty1 = lv_qty * 6290 .
    WHEN p_tm.
      p_zpra_t_mrec_prd-prod_vl_qty1 = lv_qty * 1000 .
    WHEN p_tmd.
      p_zpra_t_mrec_prd-prod_vl_qty1 = lv_qty * 1000 .
    WHEN p_mb.
      p_zpra_t_mrec_prd-prod_vl_qty1 = lv_qty * 1000 .
    WHEN p_bmd.
      p_zpra_t_mrec_prd-prod_vl_qty1 = lv_qty * 6290.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
FORM convert_mrec_to_bcm  CHANGING p_zpra_t_mrec_prd TYPE ty_zpra_t_mrec_prd.
  DATA : lv_qty TYPE char50 .
* First Convert to MCM

  CASE p_zpra_t_mrec_prd-prod_vl_uom1 .
    WHEN 'MCM'.
      lv_qty = p_zpra_t_mrec_prd-prod_vl_qty1.
    WHEN 'M3'.
      lv_qty = p_zpra_t_mrec_prd-prod_vl_qty1 / 1000000 .
    WHEN 'MCF'.
      lv_qty = p_zpra_t_mrec_prd-prod_vl_qty1 / '35.3'.
    WHEN OTHERS.
  ENDCASE.

  p_zpra_t_mrec_prd-prod_vl_qty1 = lv_qty * 1000 .
ENDFORM.
FORM convert_mrec_gas_to_mmscm  CHANGING p_zpra_t_mrec_app TYPE ty_zpra_t_mrec_app.
  DATA : lv_qty TYPE char50 .
* First Convert to MCM
* Mutiplying by 1000 for decimal error precision, will divide later
* lv_qty = p_zpra_t_mrec_app-app_vl_qty * 1000 .
  p_zpra_t_mrec_app-app_vl_qty = 1000 * p_zpra_t_mrec_app-app_vl_qty * 1000 .

* Convert to BOE
* p_zpra_t_mrec_app-app_vl_qty = lv_qty * 6290 .

ENDFORM.

FORM convert_non_gas_units  CHANGING p_zpra_t_dly_prd TYPE zpra_t_dly_prd.
  CHECK p_zpra_t_dly_prd-product NE '722000004' .
  CLEAR gs_wtd_cf .
  READ TABLE gt_wtd_cf INTO gs_wtd_cf WITH KEY product = p_zpra_t_dly_prd-product
                                               asset   = p_zpra_t_dly_prd-asset BINARY SEARCH .
  CASE abap_true.
    WHEN p_bb.
    WHEN p_bbd.
    WHEN p_tm.
      IF gs_wtd_cf-cf IS NOT INITIAL.
        p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / gs_wtd_cf-cf .
      ENDIF.
    WHEN p_tmd.
      IF gs_wtd_cf-cf IS NOT INITIAL.
        p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / gs_wtd_cf-cf .
      ENDIF.
    WHEN p_mb .
      IF gs_wtd_cf-cf IS NOT INITIAL.
        p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / gs_wtd_cf-cf .
*      p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / 1000000 .
* Converting to TONS now. will convert to MMT at end
      ENDIF.
    WHEN p_bmd.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
FORM convert_non_gas_units2  USING p_days CHANGING p_zpra_t_dly_prd TYPE zpra_t_dly_prd .
  DATA : lv_cf TYPE zpra_t_tar_cf-conv_factor .
  DATA : lv_buper TYPE t009b-poper,
         lv_monat TYPE zpra_t_mrec_prd-monat,
         lv_gjahr TYPE t009b-bdatj.

  CHECK p_zpra_t_dly_prd-product NE c_prod_gas .
  CLEAR gs_zpra_t_mrec_prd .

  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      i_date         = p_zpra_t_dly_prd-production_date
    " I_MONMIT       = 00
      i_periv        = gv_periv
    IMPORTING
      e_buper        = lv_buper
      e_gjahr        = lv_gjahr
    EXCEPTIONS
      input_false    = 01
      t009_notfound  = 02
      t009b_notfound = 03.
  lv_monat = lv_buper+1(2) .


  LOOP AT gt_zpra_t_mrec_prd INTO gs_zpra_t_mrec_prd WHERE  product = p_zpra_t_dly_prd-product
                                                       AND  asset   = p_zpra_t_dly_prd-asset
                                                       AND  block   = p_zpra_t_dly_prd-block
                                                       AND  prd_vl_type = p_zpra_t_dly_prd-prd_vl_type . " No binary search
    IF gs_zpra_t_mrec_prd-gjahr GT lv_gjahr.
      CLEAR gs_zpra_t_mrec_prd .
      CONTINUE .
    ENDIF.
    IF gs_zpra_t_mrec_prd-gjahr EQ lv_gjahr AND
       gs_zpra_t_mrec_prd-monat GT lv_monat .
      CLEAR gs_zpra_t_mrec_prd .
      CONTINUE .
    ENDIF.
    EXIT .
  ENDLOOP.
  CASE abap_true.
    WHEN p_bb.
    WHEN p_bbd.
      p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / p_days .
    WHEN p_tm.
      IF gs_zpra_t_mrec_prd-prod_vl_qty2 IS NOT INITIAL.
        lv_cf = gs_zpra_t_mrec_prd-prod_vl_qty1 /  gs_zpra_t_mrec_prd-prod_vl_qty2 .
      ELSE.
        PERFORM get_cf_from_date USING  p_zpra_t_dly_prd-production_date
                                        p_zpra_t_dly_prd-product
                                        p_zpra_t_dly_prd-asset
                                        p_zpra_t_dly_prd-block
                               CHANGING lv_cf.
      ENDIF.
      IF lv_cf IS NOT INITIAL.
        p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / lv_cf .
      ENDIF.
    WHEN p_tmd.
      IF gs_zpra_t_mrec_prd-prod_vl_qty2 IS NOT INITIAL.
        lv_cf = gs_zpra_t_mrec_prd-prod_vl_qty1 /  gs_zpra_t_mrec_prd-prod_vl_qty2 .
      ELSE.
        PERFORM get_cf_from_date USING  p_zpra_t_dly_prd-production_date
                                        p_zpra_t_dly_prd-product
                                        p_zpra_t_dly_prd-asset
                                        p_zpra_t_dly_prd-block
                               CHANGING lv_cf.
      ENDIF.
      IF lv_cf IS NOT INITIAL.
        p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / lv_cf .
      ENDIF.
      p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / p_days .

    WHEN p_mb .
      IF gs_zpra_t_mrec_prd-prod_vl_qty2 IS NOT INITIAL.
        lv_cf = gs_zpra_t_mrec_prd-prod_vl_qty1 /  gs_zpra_t_mrec_prd-prod_vl_qty2 .
      ELSE.
        PERFORM get_cf_from_date USING  p_zpra_t_dly_prd-production_date
                                        p_zpra_t_dly_prd-product
                                        p_zpra_t_dly_prd-asset
                                        p_zpra_t_dly_prd-block
                               CHANGING lv_cf.
      ENDIF.
      IF lv_cf IS NOT INITIAL.
        p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / lv_cf .
*      p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / 1000000 .
      ENDIF.
    WHEN p_bmd.
      p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / p_days .
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
FORM convert_non_gas_units3c  USING p_monat TYPE zpra_t_mrec_prd-monat CHANGING p_zpra_t_dly_prd TYPE zpra_t_dly_prd .
  DATA : lv_cf TYPE zpra_t_mrec_prd-prod_vl_qty1 .
  DATA : lv_buper TYPE t009b-poper,
         lv_monat TYPE zpra_t_mrec_prd-monat,
         lv_gjahr TYPE t009b-bdatj.

  CHECK p_zpra_t_dly_prd-product NE c_prod_gas .
  CLEAR gs_zpra_t_mrec_prd .

  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      i_date         = p_zpra_t_dly_prd-production_date
    " I_MONMIT       = 00
      i_periv        = gv_periv
    IMPORTING
      e_buper        = lv_buper
      e_gjahr        = lv_gjahr
    EXCEPTIONS
      input_false    = 01
      t009_notfound  = 02
      t009b_notfound = 03.
  lv_monat = lv_buper+1(2) .



  LOOP AT gt_zpra_t_mrec_prd_3c INTO gs_zpra_t_mrec_prd WHERE product EQ p_zpra_t_dly_prd-product
                                                          AND asset   EQ p_zpra_t_dly_prd-asset
                                                          AND block   EQ p_zpra_t_dly_prd-block
                                                          AND prd_vl_type EQ p_zpra_t_dly_prd-prd_vl_type . " No binary search
    IF gs_zpra_t_mrec_prd-gjahr GT lv_gjahr.
      CLEAR gs_zpra_t_mrec_prd .
      CONTINUE .
    ENDIF.
    IF gs_zpra_t_mrec_prd-gjahr EQ lv_gjahr AND
       gs_zpra_t_mrec_prd-monat GT lv_monat .
      CLEAR gs_zpra_t_mrec_prd .
      CONTINUE .
    ENDIF.
    EXIT .
  ENDLOOP .
  IF gs_zpra_t_mrec_prd-prod_vl_qty2 IS NOT INITIAL.
    lv_cf = gs_zpra_t_mrec_prd-prod_vl_qty1 / gs_zpra_t_mrec_prd-prod_vl_qty2 .
  ELSE .
    PERFORM get_cf_from_date USING  p_zpra_t_dly_prd-production_date
                                    p_zpra_t_dly_prd-product
                                    p_zpra_t_dly_prd-asset
                                    p_zpra_t_dly_prd-block
                           CHANGING lv_cf.
  ENDIF.
  IF lv_cf IS NOT INITIAL.
    p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / lv_cf .
*   p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / 1000 .
  ELSE.
*   CLEAR p_zpra_t_dly_prd-prod_vl_qty1 .
  ENDIF.
ENDFORM.
FORM convert_rprd_units_mb  USING    p_zpra_t_dly_rprd TYPE zpra_t_dly_rprd.

  IF  p_zpra_t_dly_rprd-product NE c_prod_gas.
*  CASE abap_true.
*    WHEN p_bb OR p_bbd OR p_bmd.
*      IF p_c_jv IS NOT INITIAL.
*        p_zpra_t_dly_rprd-jv_prd_vl_qty1  = p_zpra_t_dly_rprd-jv_rcn_vl_qty2 .
*        p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty2 .
*      ELSE.
*        p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty2 .
*        p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty2 .
*      ENDIF.
*    WHEN p_tm OR p_tmd.
*      IF p_c_jv IS NOT INITIAL.
*        p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-jv_rcn_vl_qty3 .
*        p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 .
*      ELSE.
*        p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 .
*        p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 .
*      ENDIF.
*    WHEN p_mb.
***      IF p_c_jv IS NOT INITIAL.
***        p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-jv_rcn_vl_qty3 / 1000000 .
***        p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 / 1000000 .
***      ELSE.
    p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3   .
    p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3  .
***      ENDIF.
*    WHEN OTHERS.
*  ENDCASE.
  ELSE .
* CASE abap_true.
*   WHEN p_bb OR p_bbd.
*    IF p_c_jv IS NOT INITIAL.
*      p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-jv_rcn_vl_qty3 * 6290 .
*      p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 6290  .
*    ELSE.
*      p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 6290 .
*      p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 6290  .
*    ENDIF.
*   WHEN p_tm OR p_tmd.
*    IF p_c_jv IS NOT INITIAL.
*      p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-jv_rcn_vl_qty3 * 1000 .
*      p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 1000  .
*    ELSE.
*      p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 1000 .
*      p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 1000  .
*    ENDIF.
*   WHEN p_mb.
***    IF p_c_jv IS NOT INITIAL.
***      p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-jv_rcn_vl_qty3 / 1000 .
***      p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 / 1000  .
***    ELSE.
    p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3  * 1000.
    p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3  * 1000 .
***    ENDIF.
*   WHEN p_bmd.
*    IF p_c_jv IS NOT INITIAL.
*      p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-jv_rcn_vl_qty3 .
*      p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 .
*    ELSE.
*      p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 .
*      p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 .
*    ENDIF.
*   WHEN OTHERS.
* ENDCASE.

  ENDIF.
ENDFORM.
FORM convert_rprd_units  USING    p_zpra_t_dly_rprd TYPE zpra_t_dly_rprd.

  IF  p_zpra_t_dly_rprd-product NE c_prod_gas.
    CASE abap_true.
      WHEN p_bb OR p_bbd OR p_bmd.
        IF p_c_jv IS NOT INITIAL.
          p_zpra_t_dly_rprd-jv_prd_vl_qty1  = p_zpra_t_dly_rprd-jv_rcn_vl_qty2 .
          p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty2 .
        ELSE.
          p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty2 .
          p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty2 .
        ENDIF.
      WHEN p_tm OR p_tmd.
        IF p_c_jv IS NOT INITIAL.
          p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-jv_rcn_vl_qty3 .
          p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 .
        ELSE.
          p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 .
          p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 .
        ENDIF.
      WHEN p_mb.
        IF p_c_jv IS NOT INITIAL.
          p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-jv_rcn_vl_qty3 .
          p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 .
        ELSE.
          p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3   .
          p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3  .
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ELSE .
    CASE abap_true.
      WHEN p_bb OR p_bbd.
        IF p_c_jv IS NOT INITIAL.
          p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-jv_rcn_vl_qty3 * 6290 .
          p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 6290  .
        ELSE.
          p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 6290 .
          p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 6290  .
        ENDIF.
      WHEN p_tm OR p_tmd.
        IF p_c_jv IS NOT INITIAL.
          p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-jv_rcn_vl_qty3 * 1000 .
          p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 1000  .
        ELSE.
          p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 1000 .
          p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 1000  .
        ENDIF.
      WHEN p_mb.
        IF p_c_jv IS NOT INITIAL.
          p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-jv_rcn_vl_qty3 * 1000 .
          p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 1000  .
        ELSE.
          p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3  * 1000.
          p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3  * 1000 .
        ENDIF.
      WHEN p_bmd.
        IF p_c_jv IS NOT INITIAL.
          p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-jv_rcn_vl_qty3 * 6290 .
          p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 6290 .
        ELSE.
          p_zpra_t_dly_rprd-jv_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 6290 .
          p_zpra_t_dly_rprd-ovl_prd_vl_qty1 = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 6290 .
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

  ENDIF.
ENDFORM.

FORM convert_non_gas_units3f  USING p_days CHANGING p_zpra_t_dly_prd TYPE zpra_t_dly_prd .
  DATA : lv_cf TYPE zpra_t_tar_cf-conv_factor .

  CHECK p_zpra_t_dly_prd-product NE c_prod_gas.

  " DEBUG: hardcode CF=7.39 for RUS_SK1 to confirm this form is reached.
  " If Sakhalin-1 oil 2020-21 changes from 12.238 -> ~2.45, CF lookup is
  " the issue. If value stays 12.238, the data is coming via MREC_APP path.
  IF p_zpra_t_dly_prd-asset EQ 'RUS_SK1' OR
     p_zpra_t_dly_prd-asset CS 'SK' OR
     p_zpra_t_dly_prd-asset CS 'SAKH'.
    lv_cf = '7.39'.
  ELSE.
    " Try exact fiscal-year match from the dedicated CF table (zpra_t_tar_cf).
    PERFORM get_cf_from_date USING  p_zpra_t_dly_prd-production_date
                                    p_zpra_t_dly_prd-product
                                    p_zpra_t_dly_prd-asset
                                    p_zpra_t_dly_prd-block
                           CHANGING lv_cf.

    " No exact-year CF found: use the most-recent entry for this
    " product/asset/block (gt_cf sorted gjahr ASC, last iteration wins).
    IF lv_cf IS INITIAL .
      LOOP AT gt_cf INTO gs_cf WHERE product EQ p_zpra_t_dly_prd-product
                                 AND asset   EQ p_zpra_t_dly_prd-asset
                                 AND block   EQ p_zpra_t_dly_prd-block .
        lv_cf = gs_cf-conv_factor .
      ENDLOOP .
    ENDIF .
  ENDIF.

  IF lv_cf IS NOT INITIAL.
    p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / lv_cf .
  ENDIF.
ENDFORM.
FORM convert_non_gas_mrec_units  USING p_days CHANGING p_zpra_t_mrec_prd TYPE ty_zpra_t_mrec_prd.
  CHECK p_zpra_t_mrec_prd-product NE c_prod_gas.
  CASE abap_true.
    WHEN p_bb.
    WHEN p_bbd.
      p_zpra_t_mrec_prd-prod_vl_qty1 = p_zpra_t_mrec_prd-prod_vl_qty1 / p_days .
    WHEN p_tm.
      p_zpra_t_mrec_prd-prod_vl_qty1 = p_zpra_t_mrec_prd-prod_vl_qty2 .
    WHEN p_tmd.
      p_zpra_t_mrec_prd-prod_vl_qty1 = p_zpra_t_mrec_prd-prod_vl_qty2 / p_days.
    WHEN p_mb .
*     IF gs_wtd_cf-cf IS NOT INITIAL.
      p_zpra_t_mrec_prd-prod_vl_qty1  = p_zpra_t_mrec_prd-prod_vl_qty2  .
*      p_zpra_t_mrec_prd-prod_vl_qty1  = p_zpra_t_mrec_prd-prod_vl_qty1  / 1000000 .
*     ENDIF.
    WHEN p_bmd.
      p_zpra_t_mrec_prd-prod_vl_qty1 = p_zpra_t_mrec_prd-prod_vl_qty1 / p_days .
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
FORM convert_non_gas_units_2a2  CHANGING p_zpra_t_dly_prd TYPE zpra_t_dly_prd.

  DATA : lv_cf TYPE zpra_t_tar_cf-conv_factor .
  CHECK p_zpra_t_dly_prd-product NE c_prod_gas .
  CLEAR gs_zpra_t_mrec_prd .
  READ TABLE gt_zpra_t_mrec_prd INTO gs_zpra_t_mrec_prd WITH KEY product = p_zpra_t_dly_prd-product
                                                                 asset   = p_zpra_t_dly_prd-asset
                                                                 block   = p_zpra_t_dly_prd-block
                                                                 prd_vl_type = p_zpra_t_dly_prd-prd_vl_type . " No binary search
  CASE abap_true.
    WHEN p_bb.
    WHEN p_bbd.
      p_zpra_t_dly_prd-prod_vl_qty1 =  p_zpra_t_dly_prd-prod_vl_qty1 ." / ( p_date - gv_month_begin_datum + 1 ) .
    WHEN p_tm.
      IF gs_zpra_t_mrec_prd-prod_vl_qty2 IS NOT INITIAL.
        lv_cf = gs_zpra_t_mrec_prd-prod_vl_qty1 /  gs_zpra_t_mrec_prd-prod_vl_qty2 .
      ELSE.
        PERFORM get_cf_from_date USING  p_zpra_t_dly_prd-production_date
                                        p_zpra_t_dly_prd-product
                                        p_zpra_t_dly_prd-asset
                                        p_zpra_t_dly_prd-block
                               CHANGING lv_cf.
      ENDIF.
      IF lv_cf IS NOT INITIAL.
        p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / lv_cf .
      ENDIF.
    WHEN p_tmd.
      IF gs_zpra_t_mrec_prd-prod_vl_qty2 IS NOT INITIAL.
        lv_cf = gs_zpra_t_mrec_prd-prod_vl_qty1 /  gs_zpra_t_mrec_prd-prod_vl_qty2 .
      ELSE.
        PERFORM get_cf_from_date USING  p_zpra_t_dly_prd-production_date
                                        p_zpra_t_dly_prd-product
                                        p_zpra_t_dly_prd-asset
                                        p_zpra_t_dly_prd-block
                               CHANGING lv_cf.
      ENDIF.
      IF lv_cf IS NOT INITIAL.
        p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / lv_cf .
*      p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / gs_zpra_t_mrec_prd-prod_vl_qty1 *  gs_zpra_t_mrec_prd-prod_vl_qty2 .
*      p_zpra_t_dly_prd-prod_vl_qty1 =  p_zpra_t_dly_prd-prod_vl_qty1 ."/ ( p_date - gv_month_begin_datum + 1 ) .
      ENDIF.
    WHEN p_mb .
      IF gs_zpra_t_mrec_prd-prod_vl_qty2 IS NOT INITIAL.
        lv_cf = gs_zpra_t_mrec_prd-prod_vl_qty1 /  gs_zpra_t_mrec_prd-prod_vl_qty2 .
      ELSE.
        PERFORM get_cf_from_date USING  p_zpra_t_dly_prd-production_date
                                        p_zpra_t_dly_prd-product
                                        p_zpra_t_dly_prd-asset
                                        p_zpra_t_dly_prd-block
                               CHANGING lv_cf.
      ENDIF.
      IF lv_cf IS NOT INITIAL.
        p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / lv_cf .
*      p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / gs_zpra_t_mrec_prd-prod_vl_qty1 *  gs_zpra_t_mrec_prd-prod_vl_qty2 .
*      p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / 1000000 .
      ENDIF.
    WHEN p_bmd.
      p_zpra_t_dly_prd-prod_vl_qty1 =  p_zpra_t_dly_prd-prod_vl_qty1 ." / ( p_date - gv_month_begin_datum + 1 ) .
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
FORM convert_target_units  CHANGING p_zpra_t_prd_tar TYPE ty_zpra_t_prd_tar.
* Consolidated converter used by sec2a, sec2b, sec2c and sec5a (originally 4 identical FORMs)
  CLEAR gs_zpra_t_tar_cf .
  READ TABLE gt_zpra_t_tar_cf INTO gs_zpra_t_tar_cf WITH KEY gjahr   = gv_current_gjahr
                                                             asset   = p_zpra_t_prd_tar-asset
                                                             block   = p_zpra_t_prd_tar-block
                                                             product = p_zpra_t_prd_tar-product BINARY SEARCH .
  IF p_zpra_t_prd_tar-product NE c_prod_gas .
    CASE abap_true.
      WHEN p_bb OR p_bbd OR p_bmd.
        p_zpra_t_prd_tar-tar_qty  = p_zpra_t_prd_tar-tar_qty  * 1000000 * gs_zpra_t_tar_cf-conv_factor .
        p_zpra_t_prd_tar-tar_qty2 = p_zpra_t_prd_tar-tar_qty2 * 1000000 * gs_zpra_t_tar_cf-conv_factor .
      WHEN p_tm OR p_tmd.
        p_zpra_t_prd_tar-tar_qty  = p_zpra_t_prd_tar-tar_qty  * 1000000 .
        p_zpra_t_prd_tar-tar_qty2 = p_zpra_t_prd_tar-tar_qty2 * 1000000 .
      WHEN OTHERS.
    ENDCASE.
  ELSE.
    CASE abap_true.
      WHEN p_bb OR p_bbd.
        p_zpra_t_prd_tar-tar_qty  = p_zpra_t_prd_tar-tar_qty  * 1000 * 6290 .
        p_zpra_t_prd_tar-tar_qty2 = p_zpra_t_prd_tar-tar_qty2 * 1000 * 6290 .
      WHEN p_tm OR p_tmd.
        p_zpra_t_prd_tar-tar_qty  = p_zpra_t_prd_tar-tar_qty  * 1000000 .
        p_zpra_t_prd_tar-tar_qty2 = p_zpra_t_prd_tar-tar_qty2 * 1000000 .
      WHEN p_bmd.
        p_zpra_t_prd_tar-tar_qty  = p_zpra_t_prd_tar-tar_qty  * 1000 .
        p_zpra_t_prd_tar-tar_qty2 = p_zpra_t_prd_tar-tar_qty2 * 1000 .
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
ENDFORM.
FORM get_pi_for_null_values USING p_zpra_t_dly_prd TYPE zpra_t_dly_prd
                                      p_combine.
  CLEAR gs_wtd_cf .
  IF p_combine IS NOT INITIAL.
    READ TABLE gt_wtd_pi INTO gs_wtd_pi WITH KEY production_date = p_zpra_t_dly_prd-production_date
                                                         product = p_zpra_t_dly_prd-product
                                                           asset = 'COMBINE' BINARY SEARCH  .

  ELSE.
    READ TABLE gt_wtd_pi INTO gs_wtd_pi WITH KEY production_date = p_zpra_t_dly_prd-production_date
                                                         product = p_zpra_t_dly_prd-product
                                                           asset = p_zpra_t_dly_prd-asset BINARY SEARCH .
  ENDIF.
  IF sy-subrc IS INITIAL.
    CASE abap_true .
      WHEN p_c_jv.
        gv_individual_mul   = 1 .
        gv_total_mul        = gs_wtd_pi-pi / 100 .
        gv_total_mul        = gs_wtd_pi-pi / 100 .
      WHEN p_c_olv .
        gv_individual_mul = gs_wtd_pi-pi / 100  .
        gv_total_mul      = gs_wtd_pi-pi / 100  ..
      WHEN OTHERS.
    ENDCASE.
    gv_grand_total_mul = gv_total_mul .
    IF p_bmd IS NOT INITIAL AND p_zpra_t_dly_prd-product EQ c_prod_gas.
*      gv_grand_total_mul = gv_grand_total_mul * 6290 .
    ENDIF.
  ELSE.
    gv_individual_mul   = 0 .
    gv_total_mul        = 0 .
    gv_grand_total_mul  = 0 .
  ENDIF.
ENDFORM.
FORM get_constorium_multipliers USING p_zpra_t_dly_prd TYPE zpra_t_dly_prd
                                      p_combine.
*  CLEAR gs_wtd_cf .
*  IF p_combine IS NOT INITIAL.
*    READ TABLE gt_wtd_pi INTO gs_wtd_pi WITH KEY production_date = p_zpra_t_dly_prd-production_date
*                                                         product = p_zpra_t_dly_prd-product
*                                                           asset = 'COMBINE' BINARY SEARCH  .
*
*  ELSE.
*    READ TABLE gt_wtd_pi INTO gs_wtd_pi WITH KEY production_date = p_zpra_t_dly_prd-production_date
*                                                         product = p_zpra_t_dly_prd-product
*                                                           asset = p_zpra_t_dly_prd-asset BINARY SEARCH .
*  ENDIF.
*  IF sy-subrc IS INITIAL.
*    CASE abap_true .
*      WHEN p_c_jv.
*        gv_individual_mul   = 1 .
*        gv_total_mul        = gs_wtd_pi-pi / 100 .
*        gv_total_mul        = gs_wtd_pi-pi / 100 .
*      WHEN p_c_olv .
*        gv_individual_mul = gs_wtd_pi-pi / 100  .
*        gv_total_mul      = gs_wtd_pi-pi / 100  ..
*      WHEN OTHERS.
*    ENDCASE.
*    gv_grand_total_mul = gv_total_mul .
*    IF p_bmd IS NOT INITIAL AND p_zpra_t_dly_prd-product EQ c_prod_gas.
*      gv_grand_total_mul = gv_grand_total_mul * 6290 .
*    ENDIF.
*  ELSE.
*    gv_individual_mul   = 0 .
*    gv_total_mul        = 0 .
*    gv_grand_total_mul  = 0 .
*  ENDIF.
  LOOP AT gt_zpra_t_prd_pi INTO gs_zpra_t_prd_pi WHERE asset   EQ p_zpra_t_dly_prd-asset
                                                   AND block   EQ p_zpra_t_dly_prd-block
                                                   AND vld_frm LE p_zpra_t_dly_prd-production_date
                                                   AND vld_to  GE p_zpra_t_dly_prd-production_date .
    EXIT .
  ENDLOOP .
  CASE abap_true .
    WHEN p_c_jv.
      gv_individual_mul = 1 .
      gv_total_mul      = gs_zpra_t_prd_pi-pi / 100 .
    WHEN p_c_olv .
      gv_individual_mul = gs_zpra_t_prd_pi-pi / 100  .
      gv_total_mul      = gs_zpra_t_prd_pi-pi / 100  .
    WHEN OTHERS.
  ENDCASE.
  gv_grand_total_mul = gv_total_mul .
  IF p_bmd IS NOT INITIAL AND p_zpra_t_dly_prd-product EQ c_prod_gas..
*    gv_grand_total_mul = gv_grand_total_mul * 6290 .
  ENDIF.

ENDFORM.
FORM get_constorium_multipliers_2a2 USING p_table.
  CLEAR gs_zpra_t_prd_pi .
  DATA: lv_numerator   TYPE                   zpra_t_dly_prd-prod_vl_qty1,
        lv_denominator TYPE                   zpra_t_dly_prd-prod_vl_qty1,
        lv_prod_gas    TYPE                    c,
        lv_low_date    TYPE                   sy-datum   VALUE '99991231',
        lv_high_date   TYPE                   sy-datum   VALUE '19000101'.

  IF p_table EQ '1' .
    LOOP AT gt_zpra_t_prd_pi INTO gs_zpra_t_prd_pi WHERE asset   EQ gs_zpra_t_dly_prd-asset
                                                     AND block   EQ gs_zpra_t_dly_prd-block
                                                     AND vld_frm LE gs_zpra_t_dly_prd-production_date
                                                     AND vld_to  GE gs_zpra_t_dly_prd-production_date .
      EXIT .
    ENDLOOP .
    IF gs_zpra_t_dly_prd-product EQ c_prod_gas .
      lv_prod_gas = 'X' .
    ENDIF.
  ELSEIF p_table EQ '2' .
    LOOP AT gt_zpra_t_prd_pi_mb INTO gs_zpra_t_prd_pi WHERE asset   EQ gs_zpra_t_dly_prd-asset
                                                     AND block   EQ gs_zpra_t_dly_prd-block
                                                     AND vld_frm LE gs_zpra_t_dly_prd-production_date
                                                     AND vld_to  GE gs_zpra_t_dly_prd-production_date .
      EXIT .
    ENDLOOP .
    IF gs_zpra_t_dly_prd-product EQ c_prod_gas .
      lv_prod_gas = 'X' .
    ENDIF.
  ELSEIF p_table EQ '3' .
    LOOP AT gt_zpra_t_prd_pi_lm INTO gs_zpra_t_prd_pi WHERE asset   EQ gs_zpra_t_mrec_prd-asset
                                                        AND block   EQ gs_zpra_t_mrec_prd-block
                                                        AND vld_frm LE gv_month_back_end_datum
                                                        AND vld_to  GE gv_month_back_begin_datum .
      IF gs_zpra_t_prd_pi-vld_frm LT gv_month_back_begin_datum .
        gs_zpra_t_prd_pi-vld_frm = gv_month_back_begin_datum  .
      ENDIF.
      IF gs_zpra_t_prd_pi-vld_to GT gv_month_back_end_datum.
        gs_zpra_t_prd_pi-vld_to = gv_month_back_end_datum .
      ENDIF.
      IF gs_zpra_t_prd_pi-vld_frm LT lv_low_date.
        lv_low_date = gs_zpra_t_prd_pi-vld_frm .
      ENDIF.
      IF gs_zpra_t_prd_pi-vld_to GT lv_high_date.
        lv_high_date = gs_zpra_t_prd_pi-vld_to .
      ENDIF.

      lv_numerator = lv_numerator + gs_zpra_t_prd_pi-pi * ( gs_zpra_t_prd_pi-vld_to - gs_zpra_t_prd_pi-vld_frm + 1 ) .
    ENDLOOP .
*    gs_zpra_t_prd_pi-pi = lv_numerator / ( gv_month_back_end_datum - gv_month_back_begin_datum + 1 ) .
    gs_zpra_t_prd_pi-pi = lv_numerator / ( lv_high_date - lv_low_date + 1 ) .

    IF gs_zpra_t_mrec_prd-product EQ c_prod_gas .
      lv_prod_gas = 'X' .
    ENDIF.
  ELSEIF p_table EQ '4' .
    LOOP AT gt_zpra_t_prd_pi_lm INTO gs_zpra_t_prd_pi WHERE asset   EQ gs_zpra_t_dly_prd-asset
                                                        AND block   EQ gs_zpra_t_dly_prd-block
                                                        AND vld_frm LE gs_zpra_t_dly_prd-production_date
                                                        AND vld_to  GE gs_zpra_t_dly_prd-production_date .
      EXIT .
    ENDLOOP .
    IF gs_zpra_t_dly_prd-product EQ c_prod_gas .
      lv_prod_gas = 'X' .
    ENDIF.
  ELSEIF p_table EQ '5' .
    LOOP AT gt_zpra_t_prd_pi_2d INTO gs_zpra_t_prd_pi WHERE asset   EQ gs_zpra_t_mrec_prd-asset
                                                        AND block   EQ gs_zpra_t_mrec_prd-block
                                                        AND vld_frm LE gv_search_end_datum
                                                        AND vld_to  GE gv_search_begin_datum .
      IF gs_zpra_t_prd_pi-vld_frm LT gv_search_begin_datum .
        gs_zpra_t_prd_pi-vld_frm = gv_search_begin_datum  .
      ENDIF.
      IF gs_zpra_t_prd_pi-vld_to GT gv_search_end_datum.
        gs_zpra_t_prd_pi-vld_to = gv_search_end_datum .
      ENDIF.
      IF gs_zpra_t_prd_pi-vld_frm LT lv_low_date.
        lv_low_date = gs_zpra_t_prd_pi-vld_frm .
      ENDIF.
      IF gs_zpra_t_prd_pi-vld_to GT lv_high_date.
        lv_high_date = gs_zpra_t_prd_pi-vld_to .
      ENDIF.
      lv_numerator = lv_numerator + gs_zpra_t_prd_pi-pi * ( gs_zpra_t_prd_pi-vld_to - gs_zpra_t_prd_pi-vld_frm + 1 ) .
    ENDLOOP .
*    gs_zpra_t_prd_pi-pi = lv_numerator / ( gv_search_end_datum - gv_search_begin_datum + 1 ) .
    gs_zpra_t_prd_pi-pi = lv_numerator / ( lv_high_date - lv_low_date + 1 ) .
    IF gs_zpra_t_mrec_prd-product EQ c_prod_gas .
      lv_prod_gas = 'X' .
    ENDIF.
  ELSEIF p_table EQ '6' .
    LOOP AT gt_zpra_t_prd_pi_2f INTO gs_zpra_t_prd_pi WHERE asset   EQ gs_zpra_t_mrec_prd-asset
                                                        AND block   EQ gs_zpra_t_mrec_prd-block
                                                        AND vld_frm LE gv_search_end_datum
                                                        AND vld_to  GE gv_search_begin_datum .
      IF gs_zpra_t_prd_pi-vld_frm LT gv_search_begin_datum .
        gs_zpra_t_prd_pi-vld_frm = gv_search_begin_datum  .
      ENDIF.
      IF gs_zpra_t_prd_pi-vld_to GT gv_search_end_datum.
        gs_zpra_t_prd_pi-vld_to = gv_search_end_datum .
      ENDIF.
      IF gs_zpra_t_prd_pi-vld_frm LT lv_low_date.
        lv_low_date = gs_zpra_t_prd_pi-vld_frm .
      ENDIF.
      IF gs_zpra_t_prd_pi-vld_to GT lv_high_date.
        lv_high_date = gs_zpra_t_prd_pi-vld_to .
      ENDIF.

      lv_numerator = lv_numerator + gs_zpra_t_prd_pi-pi * ( gs_zpra_t_prd_pi-vld_to - gs_zpra_t_prd_pi-vld_frm + 1 ) .
    ENDLOOP .
*    gs_zpra_t_prd_pi-pi = lv_numerator / ( gv_search_end_datum - gv_search_begin_datum + 1 ) .
    gs_zpra_t_prd_pi-pi = lv_numerator / ( lv_high_date - lv_low_date + 1 ) .

    IF gs_zpra_t_mrec_prd-product EQ c_prod_gas .
      lv_prod_gas = 'X' .
    ENDIF.
  ELSEIF p_table EQ '7' .

    LOOP AT gt_zpra_t_prd_pi_3c INTO gs_zpra_t_prd_pi WHERE asset   EQ gs_zpra_t_dly_prd-asset
                                                        AND block   EQ gs_zpra_t_dly_prd-block
                                                        AND vld_frm LE gs_zpra_t_dly_prd-production_date
                                                        AND vld_to  GE gs_zpra_t_dly_prd-production_date .
      EXIT .
    ENDLOOP .
    IF gs_zpra_t_dly_prd-product EQ c_prod_gas .
      lv_prod_gas = 'X' .
    ENDIF.
  ELSEIF p_table EQ '8' .

    LOOP AT gt_zpra_t_prd_pi_3f INTO gs_zpra_t_prd_pi WHERE asset   EQ gs_zpra_t_dly_prd-asset
                                                        AND block   EQ gs_zpra_t_dly_prd-block
                                                        AND vld_frm LE gs_zpra_t_dly_prd-production_date
                                                        AND vld_to  GE gs_zpra_t_dly_prd-production_date .
      EXIT .
    ENDLOOP .
    IF gs_zpra_t_dly_prd-product EQ c_prod_gas .
      lv_prod_gas = 'X' .
    ENDIF.
  ELSEIF p_table EQ '9' .

    LOOP AT gt_zpra_t_prd_pi_4a INTO gs_zpra_t_prd_pi WHERE asset   EQ gs_zpra_t_dly_prd-asset
                                                        AND block   EQ gs_zpra_t_dly_prd-block
                                                        AND vld_frm LE gs_zpra_t_dly_prd-production_date
                                                        AND vld_to  GE gs_zpra_t_dly_prd-production_date .
      EXIT .
    ENDLOOP .
    IF gs_zpra_t_dly_prd-product EQ c_prod_gas .
      lv_prod_gas = 'X' .
    ENDIF.
  ENDIF.
  CASE abap_true .
    WHEN p_c_jv.
      gv_individual_mul = 1 .
      gv_total_mul      = gs_zpra_t_prd_pi-pi / 100 .
    WHEN p_c_olv .
      gv_individual_mul = gs_zpra_t_prd_pi-pi / 100  .
      gv_total_mul      = gs_zpra_t_prd_pi-pi / 100  .
    WHEN OTHERS.
  ENDCASE.
  gv_grand_total_mul = gv_total_mul .
  IF p_bmd IS NOT INITIAL AND lv_prod_gas IS NOT INITIAL.
*    gv_grand_total_mul = gv_grand_total_mul * 6290 .
  ENDIF.
ENDFORM.
FORM set_columns_width .
  DATA lv_end_col TYPE sy-tabix .
  lv_end_col = gv_table_columns - 1 .
  PERFORM col_width USING 1 1 '25'  .
  PERFORM col_width USING 2 2 '11.15'  .
  PERFORM col_width USING 3 lv_end_col '11.5'  .
  LOOP AT gt_product_col INTO gs_product_col.
    PERFORM col_width USING gs_product_col-e_col gs_product_col-e_col '12'  .
  ENDLOOP.
  PERFORM col_width USING gv_table_columns gv_table_columns '12'  .

ENDFORM.
FORM col_width  USING p_col_start
                      p_col_end
                      p_width.
*CALL METHOD OF go_excel 'Columns' = go_column
*  EXPORTING
*    #1 = p_col.

  PERFORM select_range USING 1 p_col_start 1 p_col_end  .
  SET PROPERTY OF go_range 'ColumnWidth' = p_width.

* PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .

ENDFORM.
FORM colour_dates .
  DATA : lv_lines   TYPE sy-tabix,
         lv_end_row TYPE sy-tabix.
  DESCRIBE TABLE <gfs_dyn_table> LINES lv_lines .

  lv_end_row = gv_sec1_data_start_row + lv_lines - 1 .

  PERFORM select_range USING gv_sec1_data_start_row 1 lv_end_row 2  .

  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = gv_dates_colour .

ENDFORM.
FORM join_header_total_cells .
  DATA : lv_end_row   TYPE sy-tabix .
  lv_end_row = gv_sec1_h_start_row + 1 .
  LOOP AT gt_product_col INTO gs_product_col.
    PERFORM select_range USING gv_sec1_h_start_row gs_product_col-e_col lv_end_row gs_product_col-e_col .
    CALL METHOD OF go_range 'Merge' .
  ENDLOOP.
* Grand Total
  gs_product_col-e_col = gs_product_col-e_col + 1.
  PERFORM select_range USING gv_sec1_h_start_row gs_product_col-e_col lv_end_row gs_product_col-e_col .
  CALL METHOD OF go_range 'Merge' .

ENDFORM.
FORM join_header1_column_1_2.
  PERFORM select_range USING gv_sec1_h_start_row 1 gv_sec1_h_start_row 2 .
  CALL METHOD OF go_range 'Merge' .
ENDFORM .

FORM colour_sec1_data_totals .

  DATA : lv_end_row TYPE sy-tabix,
         lv_color   TYPE sy-tabix,
         lv_lines   TYPE sy-tabix.

  DESCRIBE TABLE <gfs_dyn_table> LINES lv_lines .

  lv_end_row = gv_sec1_data_start_row + lv_lines - 1 .

  LOOP AT gt_product_col INTO gs_product_col.
    PERFORM select_range USING gv_sec1_data_start_row gs_product_col-e_col  lv_end_row gs_product_col-e_col.
    CASE  gs_product_col-product.
      WHEN '722000001'.
        lv_color = gv_header_oil_colour .
      WHEN '722000003'.
        lv_color = gv_header_cond_colour .
      WHEN '722000005'.
        lv_color = gv_header_lng_colour .
      WHEN '722000004'.
        lv_color = gv_header_gas_colour .
      WHEN OTHERS.
    ENDCASE.
    PERFORM set_range_interior USING lv_color .
  ENDLOOP.
  gs_product_col-e_col = gs_product_col-e_col + 1.
  PERFORM select_range USING gv_sec1_data_start_row gs_product_col-e_col lv_end_row gs_product_col-e_col .
  PERFORM set_range_interior USING gv_header_gt_colour .

ENDFORM.
FORM process_sec2a_data .
  SORT gt_zpra_t_mrec_prd BY product ASCENDING asset ASCENDING block ASCENDING prd_vl_type ASCENDING  gjahr DESCENDING monat DESCENDING .

  PERFORM convert_sec2a_to_jvl.
  PERFORM create_dynamic_table CHANGING gt_sec2a1_table .
  ASSIGN gt_sec2a1_table->* TO <gfs_sec2_table> .
  PERFORM fill_dynamic_table_sec2a1    .
  gv_colour = gv_sec2a_tgt_colour .
  CONCATENATE 'Target' ':' gv_month_name gv_current_calendar_gjahr INTO gv_txt SEPARATED BY space.
  PERFORM display_section2a1 .

  PERFORM create_dynamic_table CHANGING gt_sec2a2_table .
  ASSIGN gt_sec2a2_table->* TO <gfs_sec2a2_table> .
  PERFORM populate_no_data_entries TABLES gt_zpra_t_dly_prd USING gv_month_back_datum p_date .
  PERFORM fill_dynamic_table_sec2a2    .
  PERFORM display_section2a2 .

  PERFORM fetch_data_section2a3 .
  PERFORM process_gas_records_2a3 .
  PERFORM create_dynamic_table CHANGING gt_sec2a3_table .
  ASSIGN gt_sec2a3_table->* TO <gfs_sec2a3_table> .
  PERFORM fill_dynamic_table_sec2a3    .
  PERFORM display_section2a3 .

ENDFORM.
FORM convert_sec2a_to_jvl .
  FIELD-SYMBOLS : <lfs_zpra_t_prd_tar> TYPE ty_zpra_t_prd_tar .
  DATA:  lv_numerator                   TYPE ty_zpra_t_prd_tar-tar_qty .
  LOOP AT gt_zpra_t_prd_tar ASSIGNING <lfs_zpra_t_prd_tar>.
    CLEAR lv_numerator .
    <lfs_zpra_t_prd_tar>-tar_qty2 = <lfs_zpra_t_prd_tar>-tar_qty .
    IF p_c_jv IS NOT INITIAL.
      LOOP AT gt_zpra_t_tar_pi INTO gs_zpra_t_tar_pi WHERE asset     EQ <lfs_zpra_t_prd_tar>-asset
                                                       AND block     EQ <lfs_zpra_t_prd_tar>-block
                                                       AND tar_code  EQ <lfs_zpra_t_prd_tar>-tar_code.
        IF gs_zpra_t_tar_pi-vld_frm LT gv_month_begin_datum.
          gs_zpra_t_tar_pi-vld_frm = gv_month_begin_datum .
        ENDIF.
        IF gs_zpra_t_tar_pi-vld_to GT gv_month_end_datum.
          gs_zpra_t_tar_pi-vld_to = gv_month_end_datum .
        ENDIF.
        IF gs_zpra_t_tar_pi-pi IS NOT INITIAL.
          lv_numerator = lv_numerator + ( gs_zpra_t_tar_pi-vld_to - gs_zpra_t_tar_pi-vld_frm + 1 ) / gs_zpra_t_tar_pi-pi  .
        ENDIF.
      ENDLOOP.
      <lfs_zpra_t_prd_tar>-tar_qty = <lfs_zpra_t_prd_tar>-tar_qty * lv_numerator * 100 / gv_current_month_days .
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM convert_sec2b_to_jvl .
  FIELD-SYMBOLS : <lfs_zpra_t_prd_tar> TYPE ty_zpra_t_prd_tar .
  DATA :lv_numerator TYPE                   p LENGTH 16 DECIMALS 9,
        lv_first_day TYPE                   sy-datum,
        lv_last_day  TYPE                   sy-datum.

  LOOP AT gt_zpra_t_prd_tar ASSIGNING <lfs_zpra_t_prd_tar>.
    CLEAR lv_numerator .
    <lfs_zpra_t_prd_tar>-tar_qty2 = <lfs_zpra_t_prd_tar>-tar_qty .
    CALL FUNCTION 'PERIOD_DAY_DETERMINE'
      EXPORTING
        i_gjahr              = <lfs_zpra_t_prd_tar>-gjahr
        i_monat              = <lfs_zpra_t_prd_tar>-monat
        i_periv              = gv_periv
      IMPORTING
        e_fday               = lv_first_day
        e_lday               = lv_last_day
*       E_SPERIOD            =
      EXCEPTIONS
        error_period         = 1
        error_period_version = 2
        firstday_not_defined = 3
        period_not_defined   = 4
        year_invalid         = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      MESSAGE 'Internal Date error' TYPE 'E' .
    ENDIF.

    IF p_c_jv IS NOT INITIAL.
      LOOP AT gt_zpra_t_tar_pi INTO gs_zpra_t_tar_pi WHERE asset     EQ <lfs_zpra_t_prd_tar>-asset
                                                       AND block     EQ <lfs_zpra_t_prd_tar>-block
                                                       AND tar_code  EQ <lfs_zpra_t_prd_tar>-tar_code .
        IF gs_zpra_t_tar_pi-vld_to LT lv_first_day OR gs_zpra_t_tar_pi-vld_frm GT lv_last_day.
          CONTINUE.
        ENDIF.
        IF gs_zpra_t_tar_pi-vld_frm LT lv_first_day.
          gs_zpra_t_tar_pi-vld_frm = lv_first_day .
        ENDIF.
        IF gs_zpra_t_tar_pi-vld_to GT lv_last_day.
          gs_zpra_t_tar_pi-vld_to = lv_last_day .
        ENDIF.
        IF gs_zpra_t_tar_pi-pi IS NOT INITIAL.
          lv_numerator = lv_numerator + ( gs_zpra_t_tar_pi-vld_to - gs_zpra_t_tar_pi-vld_frm + 1 ) / gs_zpra_t_tar_pi-pi  .
        ENDIF.
      ENDLOOP.
      <lfs_zpra_t_prd_tar>-tar_qty = <lfs_zpra_t_prd_tar>-tar_qty * lv_numerator * 100 / ( lv_last_day - lv_first_day + 1 ) .
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM convert_sec2c_to_jvl .
  FIELD-SYMBOLS : <lfs_zpra_t_prd_tar> TYPE ty_zpra_t_prd_tar .
  DATA :lv_numerator TYPE                   p LENGTH 16 DECIMALS 9,
        lv_first_day TYPE                   sy-datum,
        lv_last_day  TYPE                   sy-datum.
  LOOP AT gt_zpra_t_prd_tar_2c ASSIGNING <lfs_zpra_t_prd_tar>.
    CLEAR lv_numerator .

    CALL FUNCTION 'PERIOD_DAY_DETERMINE'
      EXPORTING
        i_gjahr              = <lfs_zpra_t_prd_tar>-gjahr
        i_monat              = <lfs_zpra_t_prd_tar>-monat
        i_periv              = gv_periv
      IMPORTING
        e_fday               = lv_first_day
        e_lday               = lv_last_day
*       E_SPERIOD            =
      EXCEPTIONS
        error_period         = 1
        error_period_version = 2
        firstday_not_defined = 3
        period_not_defined   = 4
        year_invalid         = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      MESSAGE 'Internal Date error' TYPE 'E' .
    ENDIF.

    IF <lfs_zpra_t_prd_tar>-monat EQ gv_current_monat.
      <lfs_zpra_t_prd_tar>-tar_qty = <lfs_zpra_t_prd_tar>-tar_qty * ( p_date - lv_first_day + 1 ) / ( lv_last_day - lv_first_day + 1 ) .
      lv_last_day = p_date .
    ENDIF.
    <lfs_zpra_t_prd_tar>-tar_qty2 = <lfs_zpra_t_prd_tar>-tar_qty .

    IF p_c_jv IS NOT INITIAL.
      LOOP AT gt_zpra_t_tar_pi INTO gs_zpra_t_tar_pi WHERE asset     EQ <lfs_zpra_t_prd_tar>-asset
                                                       AND block     EQ <lfs_zpra_t_prd_tar>-block
                                                       AND tar_code  EQ <lfs_zpra_t_prd_tar>-tar_code .
        IF gs_zpra_t_tar_pi-vld_to LT lv_first_day OR gs_zpra_t_tar_pi-vld_frm GT lv_last_day.
          CONTINUE.
        ENDIF.
        IF gs_zpra_t_tar_pi-vld_frm LT lv_first_day.
          gs_zpra_t_tar_pi-vld_frm = lv_first_day .
        ENDIF.
        IF gs_zpra_t_tar_pi-vld_to GT lv_last_day.
          gs_zpra_t_tar_pi-vld_to = lv_last_day .
        ENDIF.
        IF gs_zpra_t_tar_pi-pi IS NOT INITIAL.
          lv_numerator = lv_numerator + ( gs_zpra_t_tar_pi-vld_to - gs_zpra_t_tar_pi-vld_frm + 1 ) / gs_zpra_t_tar_pi-pi  .
        ENDIF.
      ENDLOOP.
      <lfs_zpra_t_prd_tar>-tar_qty = <lfs_zpra_t_prd_tar>-tar_qty * lv_numerator * 100 / ( lv_last_day - lv_first_day + 1 ) .
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM fill_dynamic_table_sec2a1 .
  DATA : lv_col_name      TYPE lvc_fname,
         lv_combine_field TYPE c,
         lv_index         TYPE sy-tabix,
         lv_target_name   TYPE char25,
         lv_mmscmd_mul    TYPE sy-tabix,
         lv_days          TYPE sy-tabix,
         lv_start_date    TYPE sy-tabix,
         lv_tar_code      TYPE zpra_t_prd_tar-tar_code.

  SORT gt_zpra_t_prd_tar BY tar_code product asset.
  LOOP AT gt_zpra_t_prd_tar INTO gs_zpra_t_prd_tar .
    CLEAR lv_combine_field .
    AT NEW tar_code .
      APPEND INITIAL LINE TO <gfs_sec2_table> ASSIGNING <gfs_dyn_line> .
      ASSIGN COMPONENT 'COL02' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      PERFORM get_target_type_name USING gs_zpra_t_prd_tar-tar_code CHANGING lv_target_name.
      <gfs_field> = lv_target_name.
    ENDAT .
    PERFORM convert_target_units CHANGING gs_zpra_t_prd_tar.

    IF gs_zpra_t_prd_tar-product = c_prod_gas.
      READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_prd_tar-asset BINARY SEARCH .
      IF  sy-subrc IS INITIAL.
        lv_combine_field = 'X' .
      ENDIF.
    ENDIF.
* Individual Column..
    gv_len = strlen( gs_zpra_t_prd_tar-product ) .
    IF lv_combine_field IS INITIAL.
      CONCATENATE gs_zpra_t_prd_tar-product(gv_len) '-' gs_zpra_t_prd_tar-asset INTO lv_col_name .
    ELSE.
      CONCATENATE gs_zpra_t_prd_tar-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
    ENDIF.
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_prd_tar-tar_qty .
      UNASSIGN <gfs_field> .
    ENDIF.
* Product Total..
    CONCATENATE gs_zpra_t_prd_tar-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_prd_tar-tar_qty2 .
      UNASSIGN <gfs_field> .
    ENDIF.
* Grand Total
    CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      lv_mmscmd_mul = 1 .
      IF gs_zpra_t_prd_tar-product EQ c_prod_gas AND p_bmd IS NOT INITIAL.
        lv_mmscmd_mul = 6290 .
      ENDIF.
      <gfs_field> = <gfs_field> + ( gs_zpra_t_prd_tar-tar_qty2 * lv_mmscmd_mul ).
      UNASSIGN <gfs_field> .
    ENDIF.
  ENDLOOP.

* Now dividing by no of days in case of per day units
  IF p_bbd IS NOT INITIAL OR
     p_tmd IS NOT INITIAL OR
     p_bmd IS NOT INITIAL .
    LOOP AT <gfs_sec2_table> ASSIGNING <gfs_dyn_line> .
      DO .
        lv_index = sy-index .
        IF lv_index EQ 2.
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF sy-subrc IS INITIAL.
            lv_target_name = <gfs_field> .
            PERFORM get_target_from_name USING lv_target_name CHANGING lv_tar_code.
          ENDIF.
        ENDIF.
        IF lv_index GT 2.
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF sy-subrc IS NOT INITIAL.
            EXIT .
          ENDIF.

          lv_days = gv_month_end_datum - gv_month_begin_datum + 1 .
          PERFORM get_target_start_date USING lv_index lv_tar_code CHANGING lv_start_date .
          IF lv_start_date GT gv_month_begin_datum .
            lv_days =  gv_month_end_datum - lv_start_date + 1 .
          ENDIF.
          <gfs_field> = <gfs_field> / lv_days .
          UNASSIGN <gfs_field> .
        ENDIF.
      ENDDO.
    ENDLOOP .
  ENDIF.

ENDFORM.
FORM fill_dynamic_table_sec2b .
  DATA : lv_col_name      TYPE lvc_fname,
         lv_combine_field TYPE c,
         lv_index         TYPE sy-tabix,
         lv_target_name   TYPE char25,
         lv_mmscmd_mul    TYPE sy-tabix,
         lv_days          TYPE sy-tabix,
         lv_start_date    TYPE sy-tabix,
         lv_tar_code      TYPE zpra_t_prd_tar-tar_code.

  SORT gt_zpra_t_prd_tar BY tar_code .
  LOOP AT gt_zpra_t_prd_tar INTO gs_zpra_t_prd_tar .
    CLEAR lv_combine_field .
    AT NEW tar_code .
      APPEND INITIAL LINE TO <gfs_sec2_table> ASSIGNING <gfs_dyn_line> .
      ASSIGN COMPONENT 'COL02' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      PERFORM get_target_type_name USING gs_zpra_t_prd_tar-tar_code CHANGING lv_target_name.
      <gfs_field> = lv_target_name.
    ENDAT .
    PERFORM convert_target_units CHANGING gs_zpra_t_prd_tar.

    IF gs_zpra_t_prd_tar-product = c_prod_gas.
      READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_prd_tar-asset BINARY SEARCH .
      IF  sy-subrc IS INITIAL.
        lv_combine_field = 'X' .
      ENDIF.
    ENDIF.
* Individual Column..
    gv_len = strlen( gs_zpra_t_prd_tar-product ) .
    IF lv_combine_field IS INITIAL.
      CONCATENATE gs_zpra_t_prd_tar-product(gv_len) '-' gs_zpra_t_prd_tar-asset INTO lv_col_name .
    ELSE.
      CONCATENATE gs_zpra_t_prd_tar-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
    ENDIF.
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_prd_tar-tar_qty .
      UNASSIGN <gfs_field> .
    ENDIF.
* Product Total..
    CONCATENATE gs_zpra_t_prd_tar-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_prd_tar-tar_qty2 .
      UNASSIGN <gfs_field> .
    ENDIF.
* Grand Total
    CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      lv_mmscmd_mul = 1 .
      IF gs_zpra_t_prd_tar-product EQ c_prod_gas AND p_bmd IS NOT INITIAL.
        lv_mmscmd_mul = 6290 .
      ENDIF.
      <gfs_field> = <gfs_field> + ( gs_zpra_t_prd_tar-tar_qty2 * lv_mmscmd_mul ).
      UNASSIGN <gfs_field> .
    ENDIF.
  ENDLOOP.
* Now dividing by no of days in case of per day units
  IF p_bbd IS NOT INITIAL OR
     p_tmd IS NOT INITIAL OR
     p_bmd IS NOT INITIAL .
    LOOP AT <gfs_sec2_table> ASSIGNING <gfs_dyn_line> .
      DO .
        lv_index = sy-index .
        IF lv_index EQ 2.
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF sy-subrc IS INITIAL.
            lv_target_name = <gfs_field> .
            PERFORM get_target_from_name USING lv_target_name CHANGING lv_tar_code.
          ENDIF.
        ENDIF.
        IF lv_index GT 2.
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF sy-subrc IS NOT INITIAL.
            EXIT .
          ENDIF.

          lv_days = gv_year_end_date - gv_year_start_date + 1 .
          PERFORM get_target_start_date USING lv_index lv_tar_code CHANGING lv_start_date .
          IF lv_start_date GT gv_year_start_date .
            lv_days =  gv_year_end_date - lv_start_date + 1 .
          ENDIF.
          IF <gfs_field> IS NOT INITIAL.
            <gfs_field> = <gfs_field> / lv_days .
          ENDIF.
          UNASSIGN <gfs_field> .
        ENDIF.
      ENDDO.
    ENDLOOP .
  ENDIF.

ENDFORM.
FORM fill_dynamic_table_sec2c .
  DATA : lv_col_name      TYPE lvc_fname,
         lv_combine_field TYPE c,
         lv_index         TYPE sy-tabix,
         lv_target_name   TYPE char25,
         lv_mmscmd_mul    TYPE sy-tabix,
         lv_days          TYPE sy-tabix,
         lv_start_date    TYPE sy-tabix,
         lv_tar_code      TYPE zpra_t_prd_tar-tar_code.

  SORT gt_zpra_t_prd_tar_2c BY tar_code .
  LOOP AT gt_zpra_t_prd_tar_2c INTO gs_zpra_t_prd_tar .
    CLEAR lv_combine_field .
    AT NEW tar_code .
      APPEND INITIAL LINE TO <gfs_sec2_table> ASSIGNING <gfs_dyn_line> .
      ASSIGN COMPONENT 'COL02' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      PERFORM get_target_type_name USING gs_zpra_t_prd_tar-tar_code CHANGING lv_target_name.
      <gfs_field> = lv_target_name.
    ENDAT .
    PERFORM convert_target_units CHANGING gs_zpra_t_prd_tar.

    IF gs_zpra_t_prd_tar-product = c_prod_gas.
      READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_prd_tar-asset BINARY SEARCH .
      IF  sy-subrc IS INITIAL.
        lv_combine_field = 'X' .
      ENDIF.
    ENDIF.
* Individual Column..
    gv_len = strlen( gs_zpra_t_prd_tar-product ) .
    IF lv_combine_field IS INITIAL.
      CONCATENATE gs_zpra_t_prd_tar-product(gv_len) '-' gs_zpra_t_prd_tar-asset INTO lv_col_name .
    ELSE.
      CONCATENATE gs_zpra_t_prd_tar-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
    ENDIF.
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_prd_tar-tar_qty .
      UNASSIGN <gfs_field> .
    ENDIF.
* Product Total..
    CONCATENATE gs_zpra_t_prd_tar-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_prd_tar-tar_qty2 .
      UNASSIGN <gfs_field> .
    ENDIF.
* Grand Total
    CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      lv_mmscmd_mul = 1 .
      IF gs_zpra_t_prd_tar-product EQ c_prod_gas AND p_bmd IS NOT INITIAL.
        lv_mmscmd_mul = 6290 .
      ENDIF.
      <gfs_field> = <gfs_field> + ( gs_zpra_t_prd_tar-tar_qty2 * lv_mmscmd_mul ).
      UNASSIGN <gfs_field> .
    ENDIF.
  ENDLOOP.
* Now dividing by no of days in case of per day units
  IF p_bbd IS NOT INITIAL OR
     p_tmd IS NOT INITIAL OR
     p_bmd IS NOT INITIAL .
    LOOP AT <gfs_sec2_table> ASSIGNING <gfs_dyn_line> .
      DO .
        lv_index = sy-index .
        IF lv_index EQ 2.
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF sy-subrc IS INITIAL.
            lv_target_name = <gfs_field> .
            PERFORM get_target_from_name USING lv_target_name CHANGING lv_tar_code.
          ENDIF.
        ENDIF.
        IF lv_index GT 2.
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF sy-subrc IS NOT INITIAL.
            EXIT .
          ENDIF.
          lv_days = p_date - gv_year_start_date + 1 .
          PERFORM get_target_start_date USING lv_index lv_tar_code CHANGING lv_start_date .
          IF lv_start_date GT gv_year_start_date .
            lv_days =  p_date - lv_start_date + 1 .
          ENDIF.
          <gfs_field> = <gfs_field> / lv_days .
          UNASSIGN <gfs_field> .
        ENDIF.
      ENDDO.
    ENDLOOP .
  ENDIF.

ENDFORM.

FORM fill_dynamic_table_sec2a2 .
  DATA : lv_col_name      TYPE lvc_fname,
         lv_combine_field TYPE c,
         lv_index         TYPE sy-tabix,
         lv_year_index    TYPE sy-tabix,
         lv_last_gjahr    TYPE gjahr,
         lv_days          TYPE sy-tabix,
         lv_days2         TYPE sy-tabix,
         lv_start_date    TYPE sy-datum,
         lv_product       TYPE char100,
         lv_asset         TYPE char100.

  DATA : lv_debug              TYPE c         .

  IF lv_debug IS NOT INITIAL.
    SORT gt_zpra_t_dly_prd BY production_date .
  ENDIF.

  lv_days  = 1 .
  lv_days2 =  p_date - gv_month_begin_datum + 1 .

  REFRESH <gfs_sec2a2_table> .
  APPEND INITIAL LINE TO <gfs_sec2a2_table> ASSIGNING <gfs_dyn_line> .
  ASSIGN COMPONENT 'COL02' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
  CONCATENATE gv_month_name  gv_current_calendar_gjahr INTO <gfs_field> SEPARATED BY space .
  LOOP AT gt_zpra_t_dly_prd INTO gs_zpra_t_dly_prd WHERE production_date GE gv_month_begin_datum.
    CLEAR lv_combine_field .

    PERFORM convert_non_gas_units_2a2 CHANGING gs_zpra_t_dly_prd.
    PERFORM convert_gas_per_day_units USING lv_days CHANGING gs_zpra_t_dly_prd .
    IF gs_zpra_t_dly_prd-product = c_prod_gas.
      READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_dly_prd-asset BINARY SEARCH .
      IF  sy-subrc IS INITIAL.
        lv_combine_field = 'X' .
      ENDIF.
    ENDIF.
    PERFORM get_constorium_multipliers_2a2 USING '1' .
* Individual Column..
    gv_len = strlen( gs_zpra_t_dly_prd-product ) .
    IF lv_combine_field IS INITIAL.
      CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' gs_zpra_t_dly_prd-asset INTO lv_col_name .
    ELSE.
      CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
    ENDIF.
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_individual_mul.
      UNASSIGN <gfs_field> .
    ENDIF.
* Product Total..
    CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_total_mul .
      UNASSIGN <gfs_field> .
    ENDIF.
* Grand Total
    CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_grand_total_mul .
      UNASSIGN <gfs_field> .
    ENDIF.
  ENDLOOP.
* Last Year Same Month Data
  lv_last_gjahr = gv_current_calendar_gjahr - 1 .
  APPEND INITIAL LINE TO <gfs_sec2a2_table> ASSIGNING <gfs_dyn_line> .
  ASSIGN COMPONENT 'COL02' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
  CONCATENATE  ' ' gv_month_name  INTO <gfs_field>  .
  CONCATENATE <gfs_field> lv_last_gjahr INTO <gfs_field> SEPARATED BY space .
*  CONCATENATE gv_month_name lv_last_gjahr INTO <gfs_field> SEPARATED BY space .
  LOOP AT gt_zpra_t_dly_prd_mb INTO gs_zpra_t_dly_prd .
    CLEAR lv_combine_field .

    PERFORM convert_non_gas_units_2a2 CHANGING gs_zpra_t_dly_prd.
    PERFORM convert_gas_per_day_units USING lv_days CHANGING gs_zpra_t_dly_prd .
    IF gs_zpra_t_dly_prd-product = c_prod_gas .
      READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_dly_prd-asset BINARY SEARCH .
      IF  sy-subrc IS INITIAL.
        lv_combine_field = 'X' .
      ENDIF.
    ENDIF.
    PERFORM get_constorium_multipliers_2a2 USING '2'.
* Individual Column..
    gv_len = strlen( gs_zpra_t_dly_prd-product ) .
    IF lv_combine_field IS INITIAL.
      CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' gs_zpra_t_dly_prd-asset INTO lv_col_name .
    ELSE.
      CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
    ENDIF.
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_individual_mul.
      UNASSIGN <gfs_field> .
    ENDIF.
* Product Total..
    CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_total_mul .
      UNASSIGN <gfs_field> .
    ENDIF.
* Grand Total
    CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_grand_total_mul .
      UNASSIGN <gfs_field> .
    ENDIF.
  ENDLOOP.

* Now dividing by no of days in case of per day units
  IF p_bbd IS NOT INITIAL OR
     p_tmd IS NOT INITIAL OR
     p_bmd IS NOT INITIAL .
    LOOP AT <gfs_sec2a2_table> ASSIGNING <gfs_dyn_line> .
      lv_year_index = sy-tabix .
      DO .
        lv_index = sy-index .
        lv_days2 =  p_date - gv_month_begin_datum + 1 .
        IF lv_index GT 2.
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF sy-subrc IS NOT INITIAL.
            EXIT .
          ENDIF.
          PERFORM get_asset_start_date USING lv_index CHANGING lv_start_date .
          IF lv_year_index = 1.
            IF lv_start_date GT gv_month_begin_datum .
              lv_days2 =  p_date - lv_start_date + 1 .
            ENDIF.
          ELSE.
            IF lv_start_date GT gv_year_back_begin_datum .
              lv_days2 = lv_days2 + gv_year_back_begin_datum - lv_start_date .
            ENDIF.
          ENDIF.
          <gfs_field> = <gfs_field> / lv_days2 .
          UNASSIGN <gfs_field> .
        ENDIF.
      ENDDO.
    ENDLOOP .
  ENDIF.

  IF p_mb IS NOT INITIAL.
    LOOP AT <gfs_sec2a2_table> ASSIGNING <gfs_dyn_line> .
      DO .
        lv_index = sy-index .
        ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF sy-subrc IS NOT INITIAL.
          EXIT .
        ELSE.
          IF lv_index GT 2.
            <gfs_field> = <gfs_field> / 1000000 .
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDIF.

  IF p_bmd IS NOT INITIAL.
    LOOP AT <gfs_sec2a2_table> ASSIGNING <gfs_dyn_line> .
      DO .
        lv_index = sy-index .
        ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF sy-subrc IS NOT INITIAL.
          EXIT .
        ELSE.
          IF lv_index GT 2.
            READ TABLE gt_dyn_fcat INTO gs_dyn_fcat WITH KEY col_pos = lv_index BINARY SEARCH .
            IF sy-subrc IS INITIAL.
              SPLIT gs_dyn_fcat-fieldname AT '-' INTO lv_product lv_asset .
              IF lv_product EQ c_prod_gas.
                <gfs_field> = <gfs_field> / 6290 .
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDIF.

ENDFORM.
FORM fill_dynamic_table_sec2a3 .
  DATA : lt_zpra_t_dly_prd     TYPE STANDARD TABLE OF zpra_t_dly_prd .
  DATA : ls_zpra_t_dly_prd     TYPE                   zpra_t_dly_prd .
  DATA : lv_col_name      TYPE lvc_fname,
         lv_combine_field TYPE c,
         lv_index         TYPE sy-tabix,
         lv_days          TYPE sy-tabix,
         lv_start_date    TYPE sy-datum,
         lv_product       TYPE char100,
         lv_asset         TYPE char100.

*  lv_days = gv_month_back_end_datum - gv_month_back_begin_datum + 1 .
  lv_days = 1 .
  gt_zpra_t_mrec_prd =  gt_zpra_t_mrec_prd_2a3 .
  lt_zpra_t_dly_prd = gt_zpra_t_dly_prd .
  SORT lt_zpra_t_dly_prd BY product asset block .
  DELETE ADJACENT DUPLICATES FROM lt_zpra_t_dly_prd COMPARING product asset block .

  APPEND INITIAL LINE TO <gfs_sec2a3_table> ASSIGNING <gfs_dyn_line> .
  ASSIGN COMPONENT 'COL01' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
  <gfs_field> = 'Prod. : Montly Actual' .
  ASSIGN COMPONENT 'COL02' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
  CONCATENATE gv_last_month_name gv_month_back_calendar_gjahr INTO <gfs_field> .
  LOOP AT gt_zpra_c_prd_prof INTO gs_zpra_c_prd_prof.
    lv_index = sy-tabix .
    CLEAR lv_combine_field .
    READ TABLE gt_zpra_t_mrec_app_2a3 INTO gs_zpra_t_mrec_app WITH KEY gjahr   = gv_month_back_gjahr
                                                                       monat   = gv_month_back_monat
                                                                       product = gs_zpra_c_prd_prof-product
                                                                       asset   = gs_zpra_c_prd_prof-asset
                                                                       block   = gs_zpra_c_prd_prof-block BINARY SEARCH .
    IF sy-subrc IS INITIAL. "if found in MREC APP
      gs_zpra_t_mrec_app-app_vl_qty = gs_zpra_t_mrec_app-app_vl_qty  * 1000000 .
      IF gs_zpra_t_mrec_app-product = c_prod_gas.
        READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_mrec_app-asset BINARY SEARCH .
        IF  sy-subrc IS INITIAL.
          lv_combine_field = 'X' .
        ENDIF.
      ENDIF.

*   Individual Column..
      gv_len = strlen( gs_zpra_t_mrec_app-product ) .
      IF lv_combine_field IS INITIAL.
        CONCATENATE gs_zpra_t_mrec_app-product(gv_len) '-' gs_zpra_t_mrec_app-asset INTO lv_col_name .
      ELSE.
        CONCATENATE gs_zpra_t_mrec_app-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
      ENDIF.
      ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      IF <gfs_field> IS ASSIGNED.
        <gfs_field> = <gfs_field> + gs_zpra_t_mrec_app-app_vl_qty .
        UNASSIGN <gfs_field> .
      ENDIF.
*   Product Total..
      CONCATENATE gs_zpra_t_mrec_app-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
      ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      IF <gfs_field> IS ASSIGNED.
        <gfs_field> = <gfs_field> + gs_zpra_t_mrec_app-app_vl_qty .
        UNASSIGN <gfs_field> .
      ENDIF.
*   Grand Total
      CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
      ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      IF <gfs_field> IS ASSIGNED.
        <gfs_field> = <gfs_field> + gs_zpra_t_mrec_app-app_vl_qty.
        UNASSIGN <gfs_field> .
      ENDIF.
    ELSE. "Now work out from MREC

      READ TABLE gt_zpra_t_mrec_prd_2a3 INTO gs_zpra_t_mrec_prd WITH KEY product = gs_zpra_c_prd_prof-product
                                                                         asset   = gs_zpra_c_prd_prof-asset
                                                                         block   = gs_zpra_c_prd_prof-block
                                                                         gjahr   = gv_month_back_gjahr
                                                                         monat   = gv_month_back_monat BINARY SEARCH .
      IF sy-subrc IS INITIAL. "if found in MREC
        gs_zpra_t_dly_prd = ls_zpra_t_dly_prd .
        PERFORM convert_non_gas_mrec_units USING lv_days CHANGING gs_zpra_t_mrec_prd.
        IF gs_zpra_t_mrec_prd-product = c_prod_gas.
          READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_mrec_prd-asset BINARY SEARCH .
          IF  sy-subrc IS INITIAL.
            lv_combine_field = 'X' .
          ENDIF.
        ENDIF.
        PERFORM get_constorium_multipliers_2a2 USING '3'.

*   Individual Column..
        gv_len = strlen( gs_zpra_t_mrec_prd-product ) .
        IF lv_combine_field IS INITIAL.
          CONCATENATE gs_zpra_t_mrec_prd-product(gv_len) '-' gs_zpra_t_mrec_prd-asset INTO lv_col_name .
        ELSE.
          CONCATENATE gs_zpra_t_mrec_prd-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
        ENDIF.
        ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF <gfs_field> IS ASSIGNED.
          <gfs_field> = <gfs_field> + gs_zpra_t_mrec_prd-prod_vl_qty1 * gv_individual_mul.
          UNASSIGN <gfs_field> .
        ENDIF.
*   Product Total..
        CONCATENATE gs_zpra_t_mrec_prd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
        ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF <gfs_field> IS ASSIGNED.
          <gfs_field> = <gfs_field> + gs_zpra_t_mrec_prd-prod_vl_qty1 * gv_total_mul .
          UNASSIGN <gfs_field> .
        ENDIF.
*   Grand Total
        CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
        ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF <gfs_field> IS ASSIGNED.
          <gfs_field> = <gfs_field> + gs_zpra_t_mrec_prd-prod_vl_qty1 * gv_grand_total_mul.
          UNASSIGN <gfs_field> .
        ENDIF.

      ELSE . "Now work out from daily production
        LOOP AT gt_zpra_t_dly_prd_2a3 INTO gs_zpra_t_dly_prd WHERE product EQ gs_zpra_c_prd_prof-product
                                                               AND asset   EQ gs_zpra_c_prd_prof-asset
                                                               AND block   EQ gs_zpra_c_prd_prof-block .
          lv_index = sy-tabix .
          CLEAR lv_combine_field .

          PERFORM convert_non_gas_units2 USING lv_days CHANGING gs_zpra_t_dly_prd .

          IF gs_zpra_t_dly_prd-product = c_prod_gas.
            READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_dly_prd-asset BINARY SEARCH .
            IF  sy-subrc IS INITIAL.
              lv_combine_field = 'X' .
            ENDIF.
          ENDIF.
          PERFORM get_constorium_multipliers_2a2 USING '4'.
*       Individual Column..
          gv_len = strlen( gs_zpra_t_dly_prd-product ) .
          IF lv_combine_field IS INITIAL.
            CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' gs_zpra_t_dly_prd-asset INTO lv_col_name .
          ELSE.
            CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
          ENDIF.
          ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF <gfs_field> IS ASSIGNED.
            <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_individual_mul.
            UNASSIGN <gfs_field> .
          ENDIF.
*       Product Total..
          CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
          ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF <gfs_field> IS ASSIGNED.
            <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_total_mul .
            UNASSIGN <gfs_field> .
          ENDIF.
*       Grand Total
          CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
          ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF <gfs_field> IS ASSIGNED.
            <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_grand_total_mul.
            UNASSIGN <gfs_field> .
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

* Now dividing by no of days in case of per day units
  IF p_bbd IS NOT INITIAL OR
     p_tmd IS NOT INITIAL OR
     p_bmd IS NOT INITIAL .
    LOOP AT <gfs_sec2a3_table> ASSIGNING <gfs_dyn_line> .
      DO .
        lv_index = sy-index .
        lv_days = gv_month_back_end_datum - gv_month_back_begin_datum + 1 .
        IF lv_index GT 2.
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF sy-subrc IS NOT INITIAL.
            EXIT .
          ENDIF.
          PERFORM get_asset_start_date USING lv_index CHANGING lv_start_date .

          IF lv_start_date GT gv_month_back_begin_datum .
            lv_days =  gv_month_back_end_datum - lv_start_date + 1 .
          ENDIF.

          <gfs_field> = <gfs_field> / lv_days .
          UNASSIGN <gfs_field> .
        ENDIF.
      ENDDO.
    ENDLOOP .
  ENDIF.
  IF p_mb IS NOT INITIAL.
    LOOP AT <gfs_sec2a3_table> ASSIGNING <gfs_dyn_line> .
      DO .
        lv_index = sy-index .
        ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF sy-subrc IS NOT INITIAL.
          EXIT .
        ELSE.
          IF lv_index GT 2.
            <gfs_field> = <gfs_field> / 1000000 .
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDIF.

  IF p_bmd IS NOT INITIAL.
    LOOP AT <gfs_sec2a3_table> ASSIGNING <gfs_dyn_line> .
      DO .
        lv_index = sy-index .
        ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF sy-subrc IS NOT INITIAL.
          EXIT .
        ELSE.
          IF lv_index GT 2.
            READ TABLE gt_dyn_fcat INTO gs_dyn_fcat WITH KEY col_pos = lv_index BINARY SEARCH .
            IF sy-subrc IS INITIAL.
              SPLIT gs_dyn_fcat-fieldname AT '-' INTO lv_product lv_asset .
              IF lv_product EQ c_prod_gas.
                <gfs_field> = <gfs_field> / 6290 .
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDIF.

ENDFORM.
FORM get_target_type_name  USING    p_tar_code
                           CHANGING p_target_name.
  CASE p_tar_code.
    WHEN 'TAR_BE'.
      p_target_name = 'BE' .
    WHEN 'TAR_IN'.
      p_target_name = 'IN' .
    WHEN 'TAR_EX'.
      p_target_name = 'MOU-Ex' .
    WHEN 'TAR_VG'.
      p_target_name = 'MOU VG' .
    WHEN 'TAR_PC'.
      p_target_name = 'PC' .
    WHEN 'TAR_RE'.
      p_target_name = 'RE' .
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
FORM get_target_from_name  USING    p_target_name
                           CHANGING p_tar_code.
  CASE p_target_name.
    WHEN 'BE'.
      p_tar_code = 'TAR_BE' .
    WHEN 'IN'.
      p_tar_code = 'TAR_IN' .
    WHEN 'MOU-Ex'.
      p_tar_code = 'TAR_EX' .
    WHEN 'MOU VG'.
      p_tar_code = 'TAR_VG' .
    WHEN 'PC'.
      p_tar_code = 'TAR_PC' .
    WHEN 'RE'.
      p_tar_code = 'TAR_RE' .
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
FORM display_sec2a_targets .

  DATA lv_lines TYPE sy-tabix .
  DESCRIBE TABLE <gfs_sec2_table> LINES lv_lines .
*----------------Changes to show blank line if target data is not available----------*
*  CHECK lv_lines IS NOT INITIAL .
  IF lv_lines IS INITIAL.
    lv_lines = 1.
  ENDIF.
*----------------End of changes by Abhishek----------TR OCDK904738--------------------*
  gv_row = gv_row + 1 .
  gv_sec2a_tgt_start_row = gv_row .

  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row + lv_lines - 1 .
  gv_e_col = gv_s_col + gv_table_columns - 1.
  PERFORM prepare_paste_data TABLES <gfs_sec2_table> .
  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
*----------------Changes to show blank line if target data is not available----------*
  IF gt_paste IS NOT INITIAL.
    PERFORM paste_data .
  ENDIF.
*----------------End of changes by Abhishek----------TR OCDK904738--------------------*

  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = gv_colour .
  gv_row = gv_e_row .

  PERFORM select_range USING gv_s_row 1 gv_e_row 1 .
  CALL METHOD OF go_range 'Merge' .
  PERFORM set_range USING gv_txt 0.


ENDFORM.
FORM prepare_section4a_paste_data .
  DATA lv_index TYPE sy-tabix .
  CLEAR gs_paste.
  REFRESH gt_paste .
  LOOP AT <gfs_sec4a_table> ASSIGNING <gfs_dyn_line>.
    CLEAR gs_paste .
    DO 2 TIMES.
      lv_index = sy-index .
      ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      IF sy-subrc IS INITIAL.
        IF lv_index NE 1.
          CONCATENATE gs_paste cl_abap_char_utilities=>horizontal_tab INTO gs_paste .
        ENDIF.
        CONCATENATE gs_paste <gfs_field> INTO gs_paste .
      ELSE.
        EXIT .
      ENDIF.
    ENDDO.
    APPEND gs_paste TO gt_paste .
  ENDLOOP.
ENDFORM.
FORM prepare_section4b_paste_data .
  DATA : lv_date        TYPE char20,
         lv_asset_desc  TYPE char512,
         lv_expcom      TYPE char100,
         lv_expdt       TYPE char20.
  DATA : lt_exp_asset   TYPE STANDARD TABLE OF zpra_t_prd_pi,
         lt_exp_asset1  TYPE STANDARD TABLE OF zpra_t_prd_pi,
         ls_exp_asset   TYPE zpra_t_prd_pi,
         ls_exp_asset1  TYPE zpra_t_prd_pi.

  CLEAR gs_paste.
  REFRESH gt_paste .
  LOOP AT gt_zpra_t_dly_prd INTO gs_zpra_t_dly_prd WHERE production_date EQ p_date AND comments IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
      EXPORTING
        input  = gs_zpra_t_dly_prd-production_date
      IMPORTING
        output = lv_date.
    READ TABLE gt_asset_desc INTO gs_asset_desc WITH KEY asset = gs_zpra_t_dly_prd-asset BINARY SEARCH .
    IF sy-subrc IS INITIAL.
      lv_asset_desc = gs_asset_desc-desc .
    ENDIF.
    CONCATENATE lv_asset_desc
                cl_abap_char_utilities=>horizontal_tab
                lv_date
                cl_abap_char_utilities=>horizontal_tab
                gs_zpra_t_dly_prd-comments
                INTO gs_paste .

    APPEND gs_paste TO gt_paste .
    CLEAR gs_paste.
  ENDLOOP.

***********************************Changes by hrishikesh nikam on 23.05.22**********
  SELECT *
    FROM zpra_t_prd_pi
    INTO TABLE lt_exp_asset
   WHERE liscense_exp_dt LT sy-datum.

  SELECT asset
    FROM zpra_t_prd_pi
    INTO TABLE lt_exp_asset1
     FOR ALL ENTRIES IN lt_exp_asset
   WHERE asset          = lt_exp_asset-asset
     AND liscense_exp_dt GT sy-datum.


*    DELETE lt_exp_asset WHERE LISCENSE_EXP_DT GT SY-DATUM. "Todays Date

    LOOP AT lt_exp_asset1 INTO ls_exp_asset1.
*      IF ls_exp_asset1-asset GT SY-DATUM.
       DELETE lt_exp_asset WHERE ASSET = ls_exp_asset1-asset .
*      ENDIF.
    ENDLOOP.

    DELETE lt_exp_asset WHERE LISCENSE_EXP_DT is INITIAL.
    SORT lt_exp_asset BY ASSET LISCENSE_EXP_DT  DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_exp_asset COMPARING ASSET.   ""changes by hrishikesh nikam on 30.03.2022
    SORT lt_exp_asset BY ASSET ASCENDING.                           ""changes by hrishikesh nikam on 30.03.2022


***********************************end of Changes by hrishikesh nikam on 23.05.22**********
    LOOP AT lt_exp_asset INTO ls_exp_asset.
      if ls_exp_asset-liscense_exp_dt LT p_date.                    ""changes by hrishikesh nikam

         CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
      EXPORTING
        input  = ls_exp_asset-liscense_exp_dt
      IMPORTING
        output = lv_expdt.

        CONCATENATE 'The respective asset has been relinquished on' lv_expdt INTO lv_expcom SEPARATED BY space.

       CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
      EXPORTING
        input  = p_date
      IMPORTING
        output = lv_date.


    READ TABLE gt_asset_desc INTO gs_asset_desc WITH KEY asset = ls_exp_asset-asset BINARY SEARCH .
    IF sy-subrc IS INITIAL.
      lv_asset_desc = gs_asset_desc-desc .
    ENDIF.
    CONCATENATE lv_asset_desc
                cl_abap_char_utilities=>horizontal_tab
                lv_date
                cl_abap_char_utilities=>horizontal_tab
                lv_expcom
                INTO gs_paste .

    APPEND gs_paste TO gt_paste .
    CLEAR: gs_paste ,  lv_expcom , lv_expdt.
      endif.

    ENDLOOP.


ENDFORM.
FORM prepare_section5a_paste_data .
  DATA : lv_index  TYPE sy-tabix,
         lv_col_no TYPE sy-tabix.
  CLEAR gs_paste.
  REFRESH gt_paste .
  DO gv_5a_cols TIMES .
    lv_col_no = sy-index .
    READ TABLE gt_dyn_fcat INTO gs_dyn_fcat INDEX lv_col_no .
    IF sy-subrc IS  INITIAL.
      gs_paste = gs_dyn_fcat-coltext .
    ENDIF.
    LOOP AT <gfs_sec5a_table> ASSIGNING <gfs_dyn_line>.
      lv_index = sy-tabix .
      ASSIGN COMPONENT lv_col_no OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      CONCATENATE gs_paste cl_abap_char_utilities=>horizontal_tab INTO gs_paste .
      CONCATENATE gs_paste <gfs_field> INTO gs_paste .
    ENDLOOP .
    APPEND gs_paste TO gt_paste .
  ENDDO.
ENDFORM.
FORM fetch_section2a2_data .
  DATA : lt_zpra_t_dly_prd         TYPE STANDARD TABLE OF zpra_t_dly_prd,
         lt_zpra_t_dly_prd_current TYPE STANDARD TABLE OF zpra_t_dly_prd.
  DATA : lv_last_year_date  TYPE sy-datum,
         lv_last_year_gjahr TYPE gjahr,
         lv_index           TYPE sy-tabix.
  lt_zpra_t_dly_prd_current = gt_zpra_t_dly_prd .
  SORT lt_zpra_t_dly_prd_current BY product asset block prd_vl_type .
  DELETE ADJACENT DUPLICATES FROM lt_zpra_t_dly_prd_current COMPARING product asset block prd_vl_type .

  lv_last_year_gjahr = p_date(4) - 1 .
  CONCATENATE lv_last_year_gjahr p_date+4(4) INTO lv_last_year_date .
  CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
    EXPORTING
      iv_date             = lv_last_year_date
    IMPORTING
      ev_month_begin_date = gv_year_back_begin_datum
      ev_month_end_date   = gv_year_back_end_datum.

  SELECT *
   FROM zpra_t_dly_prd
   INTO TABLE gt_zpra_t_dly_prd_mb
    FOR ALL ENTRIES IN gt_zpra_c_prd_prof
  WHERE product EQ gt_zpra_c_prd_prof-product
    AND asset   EQ gt_zpra_c_prd_prof-asset
    AND block   EQ gt_zpra_c_prd_prof-block
    AND production_date LE lv_last_year_date
    AND production_date GE gv_year_back_begin_datum
    AND prd_vl_type IN r_prd_vl_type[] .

  PERFORM populate_no_data_entries TABLES gt_zpra_t_dly_prd_mb USING gv_year_back_begin_datum lv_last_year_date .

  SORT  gt_zpra_t_dly_prd_mb BY production_date product asset .
* LOOP AT gt_zpra_t_dly_prd_mb INTO gs_zpra_t_dly_prd_mb.
*   READ TABLE gt_zpra_t_dly_prd TRANSPORTING NO FIELDS WITH KEY product = gs_zpra_t_dly_prd_mb-product .
* ENDLOOP.
  lt_zpra_t_dly_prd = gt_zpra_t_dly_prd_mb .
  SORT lt_zpra_t_dly_prd BY asset block product prd_vl_type.
  DELETE ADJACENT DUPLICATES FROM lt_zpra_t_dly_prd COMPARING asset block product prd_vl_type .

  SELECT asset
        block
        vld_frm
        vld_to
        pi
        prod_start_date
   FROM zpra_t_prd_pi
   INTO TABLE gt_zpra_t_prd_pi_mb
    FOR ALL ENTRIES IN gt_zpra_t_dly_prd_mb
  WHERE asset EQ gt_zpra_t_dly_prd_mb-asset
    AND block EQ gt_zpra_t_dly_prd_mb-block
    AND vld_frm LE gt_zpra_t_dly_prd_mb-production_date
    AND vld_to  GE gt_zpra_t_dly_prd_mb-production_date .

  SORT gt_zpra_t_prd_pi_mb BY asset block vld_frm vld_to .


ENDFORM.
FORM fetch_data_section2b .
* DATA : lt_zpra_t_dly_prd  TYPE STANDARD TABLE OF zpra_t_dly_prd .
*
* lt_zpra_t_dly_prd = gt_zpra_t_dly_prd .
* SORT lt_zpra_t_dly_prd BY asset block product prd_vl_type.
* DELETE ADJACENT DUPLICATES FROM lt_zpra_t_dly_prd COMPARING asset block product prd_vl_type .

  REFRESH : gt_zpra_t_tar_pi ,
            gt_zpra_t_prd_tar .
  IF r_tar_code[] IS NOT INITIAL.

    SELECT asset
           block
           tar_code
           vld_frm
           vld_to
           pi
      FROM zpra_t_tar_pi
      INTO TABLE gt_zpra_t_tar_pi
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE asset EQ gt_zpra_c_prd_prof-asset
       AND block EQ gt_zpra_c_prd_prof-block
       AND vld_frm LE gv_year_end_date
       AND vld_to  GE gv_year_start_date
       AND tar_code IN r_tar_code[] .
    SORT gt_zpra_t_tar_pi BY asset block tar_code vld_frm .

    SELECT tar_code
           gjahr
           monat
           asset
           block
           product
           prod_vl_type_cd
           tar_qty
           uom
      FROM zpra_t_prd_tar
      INTO TABLE gt_zpra_t_prd_tar
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE gjahr EQ gv_current_gjahr
       AND asset EQ gt_zpra_c_prd_prof-asset
       AND block EQ gt_zpra_c_prd_prof-block
       AND product EQ gt_zpra_c_prd_prof-product
       AND prod_vl_type_cd IN r_prd_vl_type[]
       AND tar_code IN r_tar_code[] .
    SORT gt_zpra_t_prd_tar BY gjahr monat asset block product prod_vl_type_cd tar_code .

    gt_zpra_t_prd_tar_2c = gt_zpra_t_prd_tar.
    gt_zpra_t_prd_tar_3a = gt_zpra_t_prd_tar.
    gt_zpra_t_prd_tar_3b = gt_zpra_t_prd_tar.

  ENDIF.

ENDFORM.
FORM process_sec2b_data .
  REFRESH <gfs_sec2_table> .

  PERFORM convert_sec2b_to_jvl.
  PERFORM fill_dynamic_table_sec2b    .
  gv_colour = gv_sec2b_colour .
  CONCATENATE 'Target :'  gv_current_gjahr '-' gv_next_gjahr+2(2) INTO gv_txt SEPARATED BY space .
  PERFORM display_section2a1 .
  <gfs_sec2b_table> = <gfs_sec2_table> .
  PERFORM show_progress USING '10' .

ENDFORM.
FORM process_sec2c_data .
  REFRESH <gfs_sec2_table> .
  PERFORM convert_sec2c_to_jvl.
  PERFORM fill_dynamic_table_sec2c    .
  gv_colour = gv_sec2c_colour .
  CONCATENATE 'YTD Target :'  gv_current_gjahr '-' gv_next_gjahr+2(2) INTO gv_txt SEPARATED BY space .
  PERFORM display_section2a1 .
  <gfs_sec2c_table> = <gfs_sec2_table> .
ENDFORM.
FORM process_sec3b_data .
  FIELD-SYMBOLS : <lfs_zpra_t_prd_tar> TYPE ty_zpra_t_prd_tar .
  DATA:  lv_denominator TYPE                   zpra_t_dly_prd-prod_vl_qty1,
         lv_first_day   TYPE                   sy-datum,
         lv_last_day    TYPE                   sy-datum.

  DELETE gt_zpra_t_prd_tar_3b WHERE monat GT gv_current_monat .
  LOOP AT gt_zpra_t_prd_tar_3b ASSIGNING <lfs_zpra_t_prd_tar>.
    CLEAR lv_denominator .

    CALL FUNCTION 'PERIOD_DAY_DETERMINE'
      EXPORTING
        i_gjahr              = <lfs_zpra_t_prd_tar>-gjahr
        i_monat              = <lfs_zpra_t_prd_tar>-monat
        i_periv              = gv_periv
      IMPORTING
        e_fday               = lv_first_day
        e_lday               = lv_last_day
*       E_SPERIOD            =
      EXCEPTIONS
        error_period         = 1
        error_period_version = 2
        firstday_not_defined = 3
        period_not_defined   = 4
        year_invalid         = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      MESSAGE 'Internal Date error' TYPE 'E' .
    ENDIF.

    IF <lfs_zpra_t_prd_tar>-monat EQ gv_current_monat.
      <lfs_zpra_t_prd_tar>-tar_qty = <lfs_zpra_t_prd_tar>-tar_qty * ( p_date - lv_first_day + 1 ) / ( lv_last_day - lv_first_day + 1 ) .
      lv_last_day = p_date .
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM fetch_data_section2c .
  DELETE gt_zpra_t_prd_tar    WHERE monat GT gv_current_monat .
  DELETE gt_zpra_t_prd_tar_2c WHERE monat GT gv_current_monat .

  gt_zpra_t_prd_tar_5a = gt_zpra_t_prd_tar .
ENDFORM.
FORM fetch_data_section2a3 .
*  DATA : lt_zpra_t_dly_prd TYPE STANDARD TABLE OF zpra_t_dly_prd .
*  lt_zpra_t_dly_prd = gt_zpra_t_dly_prd .
*  SORT lt_zpra_t_dly_prd BY asset block product .
*  DELETE ADJACENT DUPLICATES FROM lt_zpra_t_dly_prd COMPARING asset block product .
  DATA : lv_gjahr TYPE zpra_t_mrec_prd-gjahr .
  SELECT *
    FROM zpra_t_dly_prd
    INTO TABLE gt_zpra_t_dly_prd_2a3
     FOR ALL ENTRIES IN gt_zpra_c_prd_prof
   WHERE asset EQ gt_zpra_c_prd_prof-asset
     AND block EQ gt_zpra_c_prd_prof-block
     AND product EQ gt_zpra_c_prd_prof-product
     AND prd_vl_type IN r_prd_vl_type[]
     AND production_date GE gv_month_back_begin_datum
     AND production_date LE gv_month_back_end_datum .

  PERFORM populate_no_data_entries TABLES gt_zpra_t_dly_prd_2a3 USING gv_month_back_begin_datum gv_month_back_end_datum .

  lv_gjahr = gv_month_back_gjahr - 2 .
  SELECT gjahr
         monat
         asset
         block
         product
         prd_vl_type
         prod_vl_qty1
         prod_vl_uom1
         prod_vl_qty2
         prod_vl_uom2
    FROM zpra_t_mrec_prd
    INTO TABLE gt_zpra_t_mrec_prd_2a3
     FOR ALL ENTRIES IN gt_zpra_c_prd_prof
   WHERE asset        EQ gt_zpra_c_prd_prof-asset
     AND block        EQ gt_zpra_c_prd_prof-block
     AND product      EQ gt_zpra_c_prd_prof-product
     AND prd_vl_type  IN r_prd_vl_type[]
     AND gjahr GE lv_gjahr
     AND gjahr LE gv_month_back_gjahr .
*     AND gjahr EQ gv_month_back_gjahr
*     AND monat EQ gv_month_back_monat .
  DELETE gt_zpra_t_mrec_prd_2a3 WHERE gjahr EQ gv_month_back_gjahr AND monat GT gv_month_back_monat .
  IF p_mb    IS NOT INITIAL AND
     p_c_olv IS NOT INITIAL.
    SELECT gjahr
           monat
           asset
           block
           product
           prd_vl_type
           app_vl_qty
           app_vl_uom
      FROM zpra_t_mrec_app
      INTO TABLE gt_zpra_t_mrec_app_2a3
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
   WHERE asset        EQ gt_zpra_c_prd_prof-asset
     AND block        EQ gt_zpra_c_prd_prof-block
     AND product      EQ gt_zpra_c_prd_prof-product
     AND prd_vl_type  IN r_prd_vl_type[]
     AND gjahr EQ gv_month_back_gjahr
     AND monat EQ gv_month_back_monat .

    SORT gt_zpra_t_mrec_app_2a3 BY gjahr monat product asset block  prd_vl_type .
  ENDIF.
  SELECT asset
        block
        vld_frm
        vld_to
        pi
        prod_start_date
   FROM zpra_t_prd_pi
   INTO TABLE gt_zpra_t_prd_pi_lm
    FOR ALL ENTRIES IN gt_zpra_c_prd_prof
  WHERE asset EQ gt_zpra_c_prd_prof-asset
    AND block EQ gt_zpra_c_prd_prof-block
    AND vld_frm LE gv_month_back_end_datum
    AND vld_to  GE gv_month_back_begin_datum .

  SORT gt_zpra_t_prd_pi_lm BY asset block vld_frm vld_to .

ENDFORM.
FORM fetch_data_section2d .
*  DATA : lt_zpra_t_dly_prd   TYPE STANDARD TABLE OF zpra_t_dly_prd .
*  DATA : ls_zpra_t_dly_prd   TYPE                   zpra_t_dly_prd .
  DATA : lv_monat            TYPE                   zpra_t_mrec_prd-monat,
         lv_gjahr            TYPE                   zpra_t_mrec_prd-gjahr,
         lv_date             TYPE                   sy-datum,
         lv_month_begin_date TYPE                   sy-datum,
         lv_month_end_date   TYPE                   sy-datum,
         lv_poper            TYPE                   t009b-poper,
         lv_count            TYPE                   sy-tabix.
*  lt_zpra_t_dly_prd = gt_zpra_t_dly_prd .
*  SORT lt_zpra_t_dly_prd BY product asset block  .
*  DELETE ADJACENT DUPLICATES FROM lt_zpra_t_dly_prd COMPARING product asset block  .
  IF p_mb    IS NOT INITIAL AND
     p_c_olv IS NOT INITIAL.

    SELECT gjahr
           monat
           asset
           block
           product
           prd_vl_type
           app_vl_qty
           app_vl_uom
      FROM zpra_t_mrec_app
      INTO TABLE gt_zpra_t_mrec_app_2d
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE asset        EQ gt_zpra_c_prd_prof-asset
       AND block        EQ gt_zpra_c_prd_prof-block
       AND product      EQ gt_zpra_c_prd_prof-product
       AND prd_vl_type  IN r_prd_vl_type[]
       AND ( gjahr EQ gv_last_gjahr
        OR ( gjahr EQ gv_current_gjahr  AND
             monat LE gv_current_monat ) ) .

    SORT gt_zpra_t_mrec_app_2d BY gjahr monat product asset block  prd_vl_type .
  ENDIF.
  lv_gjahr = gv_last_gjahr - 2 .

  SELECT gjahr
         monat
         asset
         block
         product
         prd_vl_type
         prod_vl_qty1
         prod_vl_uom1
         prod_vl_qty2
         prod_vl_uom2
    FROM zpra_t_mrec_prd
    INTO TABLE gt_zpra_t_mrec_prd_2d
     FOR ALL ENTRIES IN gt_zpra_c_prd_prof
   WHERE asset        EQ gt_zpra_c_prd_prof-asset
     AND block        EQ gt_zpra_c_prd_prof-block
     AND product      EQ gt_zpra_c_prd_prof-product
     AND prd_vl_type  IN r_prd_vl_type[]
     AND gjahr GE lv_gjahr
     AND gjahr LE gv_current_gjahr  .
  DELETE gt_zpra_t_mrec_prd_2d WHERE gjahr EQ gv_current_gjahr AND monat GT gv_current_monat .
*     AND ( gjahr EQ gv_last_gjahr
*      OR ( gjahr EQ gv_current_gjahr  AND
*           monat LE gv_current_monat ) ) .

*  SORT gt_zpra_t_mrec_prd_2d BY gjahr monat product asset block .
  SORT gt_zpra_t_mrec_prd_2d BY product ASCENDING asset ASCENDING block  ASCENDING gjahr DESCENDING monat DESCENDING .

  REFRESH r_production_date[] .

  LOOP AT gt_zpra_c_prd_prof INTO gs_zpra_c_prd_prof .
    lv_monat = '00' .
    DO .
      lv_monat = lv_monat + 1 .
      lv_gjahr = gv_last_gjahr .
      DO  2 TIMES .
        READ TABLE gt_zpra_t_mrec_app_2d TRANSPORTING NO FIELDS WITH KEY gjahr = lv_gjahr
                                                                         monat = lv_monat
                                                                       product = gs_zpra_c_prd_prof-product
                                                                         asset = gs_zpra_c_prd_prof-asset BINARY SEARCH .
        IF sy-subrc IS NOT INITIAL.
          lv_poper = lv_monat .
          CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
            EXPORTING
              i_gjahr        = lv_gjahr
*             I_MONMIT       = 00
              i_periv        = gv_periv
              i_poper        = lv_poper
            IMPORTING
              e_date         = lv_month_begin_date
            EXCEPTIONS
              input_false    = 1
              t009_notfound  = 2
              t009b_notfound = 3
              OTHERS         = 4.
          IF sy-subrc <> 0.
            MESSAGE 'Internal Error' TYPE 'E' .
          ENDIF.

          CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
            EXPORTING
              i_gjahr        = lv_gjahr
*             I_MONMIT       = 00
              i_periv        = gv_periv
              i_poper        = lv_poper
            IMPORTING
              e_date         = lv_month_end_date
            EXCEPTIONS
              input_false    = 1
              t009_notfound  = 2
              t009b_notfound = 3
              OTHERS         = 4.
          IF sy-subrc <> 0.
            MESSAGE 'Internal Error' TYPE 'E' .
          ENDIF.
          r_production_date-sign   = 'I' .
          r_production_date-option = 'BT' .
          r_production_date-low    = lv_month_begin_date .
          r_production_date-high   = lv_month_end_date .
          APPEND r_production_date .
        ENDIF.

        lv_gjahr = lv_gjahr + 1 .
      ENDDO.
      IF lv_monat GE gv_current_monat .
        EXIT .
      ENDIF.
    ENDDO .
  ENDLOOP.
  SORT r_production_date[] .
  DELETE ADJACENT DUPLICATES FROM r_production_date[] COMPARING ALL FIELDS .
  IF r_production_date[] IS NOT INITIAL .
    SELECT *
      FROM zpra_t_dly_prd
      INTO TABLE gt_zpra_t_dly_prd_2d
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE asset            EQ gt_zpra_c_prd_prof-asset
       AND block            EQ gt_zpra_c_prd_prof-block
       AND product          EQ gt_zpra_c_prd_prof-product
       AND prd_vl_type      IN r_prd_vl_type[]
       AND production_date  IN r_production_date[] .
    DESCRIBE TABLE r_production_date[] LINES lv_count .
    lv_count = lv_count / 2 .
    LOOP AT r_production_date.
      IF sy-tabix EQ lv_count .
        PERFORM show_progress USING '20' .
      ENDIF.
      PERFORM populate_no_data_entries TABLES gt_zpra_t_dly_prd_2d USING r_production_date-low r_production_date-high .
    ENDLOOP.
    SORT gt_zpra_t_dly_prd_2d BY product asset block production_date .

    SELECT *
      FROM zpra_t_dly_rprd
      INTO TABLE gt_zpra_t_dly_rprd_2d
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE asset EQ gt_zpra_c_prd_prof-asset
       AND block EQ gt_zpra_c_prd_prof-block
       AND product EQ gt_zpra_c_prd_prof-product
*       AND prd_vl_type IN r_prd_vl_type[]
       AND prd_vl_type EQ 'NET_PROD'
       AND production_date IN r_production_date[] .

    SORT gt_zpra_t_dly_rprd_2d BY product asset block production_date .

  ENDIF.
  CONCATENATE gv_last_gjahr '0101' INTO lv_date .
  SELECT asset
        block
        vld_frm
        vld_to
        pi
        prod_start_date
   FROM zpra_t_prd_pi
   INTO TABLE gt_zpra_t_prd_pi_2d
    FOR ALL ENTRIES IN gt_zpra_c_prd_prof
  WHERE asset EQ gt_zpra_c_prd_prof-asset
    AND block EQ gt_zpra_c_prd_prof-block
    AND vld_frm LE p_date
    AND vld_to  GE lv_date .

  SORT gt_zpra_t_prd_pi_2d BY asset block vld_frm vld_to .

ENDFORM.
FORM fetch_data_section2f .
*  DATA : lt_zpra_t_dly_prd   TYPE STANDARD TABLE OF zpra_t_dly_prd .
*  DATA : ls_zpra_t_dly_prd   TYPE                   zpra_t_dly_prd .
  DATA : lv_monat            TYPE                   zpra_t_mrec_prd-monat,
         lv_gjahr            TYPE                   zpra_t_mrec_prd-gjahr,
         lv_date             TYPE                   sy-datum,
         lv_date2            TYPE                   sy-datum,
         lv_month_begin_date TYPE                   sy-datum,
         lv_month_end_date   TYPE                   sy-datum,
         lv_poper            TYPE                   t009b-poper.

*  lt_zpra_t_dly_prd = gt_zpra_t_dly_prd .
*  SORT lt_zpra_t_dly_prd BY product asset block  .
*  DELETE ADJACENT DUPLICATES FROM lt_zpra_t_dly_prd COMPARING product asset block  .
  lv_gjahr = gv_last_gjahr - 3 .
  SELECT gjahr
         monat
         asset
         block
         product
         prd_vl_type
         prod_vl_qty1
         prod_vl_uom1
         prod_vl_qty2
         prod_vl_uom2
    FROM zpra_t_mrec_prd
    INTO TABLE gt_zpra_t_mrec_prd_2f
     FOR ALL ENTRIES IN gt_zpra_c_prd_prof
   WHERE asset        EQ gt_zpra_c_prd_prof-asset
     AND block        EQ gt_zpra_c_prd_prof-block
     AND product      EQ gt_zpra_c_prd_prof-product
     AND prd_vl_type  IN r_prd_vl_type[]
     AND gjahr        LE gv_last_gjahr
     AND gjahr        GE lv_gjahr .

*  SORT gt_zpra_t_mrec_prd_2f BY gjahr monat product asset block .
  SORT gt_zpra_t_mrec_prd_2f  BY product ASCENDING asset ASCENDING block  ASCENDING gjahr DESCENDING monat DESCENDING .

  IF p_mb    IS NOT INITIAL AND
     p_c_olv IS NOT INITIAL.
    SELECT gjahr
           monat
           asset
           block
           product
           prd_vl_type
           app_vl_qty
           app_vl_uom
      FROM zpra_t_mrec_app
      INTO TABLE gt_zpra_t_mrec_app_2f
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE gjahr EQ gv_last_gjahr
       AND asset EQ gt_zpra_c_prd_prof-asset
       AND block EQ gt_zpra_c_prd_prof-block
       AND product EQ gt_zpra_c_prd_prof-product
       AND prd_vl_type IN r_prd_vl_type[] .

    SORT gt_zpra_t_mrec_app_2f BY gjahr monat product asset block  prd_vl_type .
  ENDIF.
  REFRESH r_production_date[] .

  LOOP AT gt_zpra_c_prd_prof INTO gs_zpra_c_prd_prof .
    lv_monat = '00' .
    DO .
      lv_monat = lv_monat + 1 .
      lv_gjahr = gv_last_gjahr .
      DO  1 TIMES .
        READ TABLE gt_zpra_t_mrec_app_2f TRANSPORTING NO FIELDS WITH KEY gjahr = lv_gjahr
                                                                         monat = lv_monat
                                                                       product = gs_zpra_c_prd_prof-product
                                                                         asset = gs_zpra_c_prd_prof-asset BINARY SEARCH .
        IF sy-subrc IS NOT INITIAL.
          lv_poper = lv_monat .
          CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
            EXPORTING
              i_gjahr        = lv_gjahr
*             I_MONMIT       = 00
              i_periv        = gv_periv
              i_poper        = lv_poper
            IMPORTING
              e_date         = lv_month_begin_date
            EXCEPTIONS
              input_false    = 1
              t009_notfound  = 2
              t009b_notfound = 3
              OTHERS         = 4.
          IF sy-subrc <> 0.
            MESSAGE 'Internal Error' TYPE 'E' .
          ENDIF.

          CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
            EXPORTING
              i_gjahr        = lv_gjahr
*             I_MONMIT       = 00
              i_periv        = gv_periv
              i_poper        = lv_poper
            IMPORTING
              e_date         = lv_month_end_date
            EXCEPTIONS
              input_false    = 1
              t009_notfound  = 2
              t009b_notfound = 3
              OTHERS         = 4.
          IF sy-subrc <> 0.
            MESSAGE 'Internal Error' TYPE 'E' .
          ENDIF.
          r_production_date-sign   = 'I' .
          r_production_date-option = 'BT' .
          r_production_date-low    = lv_month_begin_date .
          r_production_date-high   = lv_month_end_date .
          APPEND r_production_date .
        ENDIF.

        lv_gjahr = lv_gjahr + 1 .
      ENDDO.
      IF lv_monat GE 12 .
        EXIT .
      ENDIF.
    ENDDO .
  ENDLOOP.
  SORT r_production_date[] .
  DELETE ADJACENT DUPLICATES FROM r_production_date[] COMPARING ALL FIELDS .

  IF r_production_date[] IS NOT INITIAL .
    SELECT *
      FROM zpra_t_dly_prd
      INTO TABLE gt_zpra_t_dly_prd_2f
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE asset EQ gt_zpra_c_prd_prof-asset
       AND block EQ gt_zpra_c_prd_prof-block
       AND product EQ gt_zpra_c_prd_prof-product
       AND prd_vl_type IN r_prd_vl_type[]
       AND production_date IN r_production_date[] .

    LOOP AT r_production_date.
      PERFORM populate_no_data_entries TABLES gt_zpra_t_dly_prd_2f USING r_production_date-low r_production_date-high .
    ENDLOOP.


    SORT gt_zpra_t_dly_prd_2f BY product asset block production_date .

    SELECT *
      FROM zpra_t_dly_rprd
      INTO TABLE gt_zpra_t_dly_rprd_2f
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE asset EQ gt_zpra_c_prd_prof-asset
       AND block EQ gt_zpra_c_prd_prof-block
       AND product EQ gt_zpra_c_prd_prof-product
*       AND prd_vl_type IN r_prd_vl_type[]
       AND prd_vl_type EQ 'NET_PROD'
       AND production_date IN r_production_date[] .

    SORT gt_zpra_t_dly_rprd_2f BY product asset block production_date .


  ENDIF.

  CONCATENATE gv_last_gjahr '0401' INTO lv_date .
  CONCATENATE gv_current_gjahr '0331' INTO lv_date2 .

  SELECT asset
        block
        vld_frm
        vld_to
        pi
        prod_start_date
   FROM zpra_t_prd_pi
   INTO TABLE gt_zpra_t_prd_pi_2f
    FOR ALL ENTRIES IN gt_zpra_c_prd_prof
  WHERE asset EQ gt_zpra_c_prd_prof-asset
    AND block EQ gt_zpra_c_prd_prof-block
    AND vld_frm LE lv_date2
    AND vld_to  GE lv_date .

  SORT gt_zpra_t_prd_pi_2f BY asset block vld_frm vld_to .
  PERFORM show_progress USING '30' .

ENDFORM.
FORM process_gas_records_2a3 .

  DATA : lt_zpra_t_dly_prd  TYPE STANDARD TABLE OF zpra_t_dly_prd,
         lt_zpra_t_mrec_prd TYPE STANDARD TABLE OF ty_zpra_t_mrec_prd.

  DATA : ls_zpra_t_dly_prd  TYPE                  zpra_t_dly_prd,
         ls_zpra_t_mrec_prd TYPE                  ty_zpra_t_mrec_prd.
  FIELD-SYMBOLS : <fs_dly_prd> TYPE zpra_t_dly_prd,
                  <fs_mrec>    TYPE ty_zpra_t_mrec_prd.

  DATA : lv_index TYPE sy-tabix,
         lv_days  TYPE sy-tabix.
* lv_days = gv_month_back_end_datum - gv_month_back_begin_datum + 1 .
  lv_days = 1 .
  lt_zpra_t_dly_prd  = gt_zpra_t_dly_prd_2a3 .
  SORT  lt_zpra_t_dly_prd BY production_date product asset block prd_vl_type .

  DELETE gt_zpra_t_dly_prd_2a3  WHERE product EQ c_prod_gas .
  DELETE lt_zpra_t_dly_prd      WHERE product NE c_prod_gas .

  IF lt_zpra_t_dly_prd IS NOT INITIAL.

    LOOP AT lt_zpra_t_dly_prd INTO ls_zpra_t_dly_prd .
      lv_index = sy-tabix .
      IF lv_index EQ 1 OR
        <fs_dly_prd>-production_date NE ls_zpra_t_dly_prd-production_date  OR
        <fs_dly_prd>-product         NE ls_zpra_t_dly_prd-product          OR
        <fs_dly_prd>-asset           NE ls_zpra_t_dly_prd-asset            OR
        <fs_dly_prd>-block           NE ls_zpra_t_dly_prd-block .

        APPEND INITIAL LINE TO gt_zpra_t_dly_prd_2a3 ASSIGNING <fs_dly_prd> .
        <fs_dly_prd>-mandt           = ls_zpra_t_dly_prd-mandt .
        <fs_dly_prd>-production_date = ls_zpra_t_dly_prd-production_date .
        <fs_dly_prd>-product         = ls_zpra_t_dly_prd-product .
        <fs_dly_prd>-asset           = ls_zpra_t_dly_prd-asset .
        <fs_dly_prd>-block           = ls_zpra_t_dly_prd-block .
        <fs_dly_prd>-prod_vl_uom1    = ls_zpra_t_dly_prd-prod_vl_uom1 .
        <fs_dly_prd>-prd_vl_type     = 'NET_PROD' .
      ENDIF.
      PERFORM convert_gas_units CHANGING ls_zpra_t_dly_prd.
      PERFORM convert_gas_per_day_units USING lv_days CHANGING ls_zpra_t_dly_prd .
      IF ls_zpra_t_dly_prd-prd_vl_type EQ 'GAS_INJ' .
        ls_zpra_t_dly_prd-prod_vl_qty1 = ls_zpra_t_dly_prd-prod_vl_qty1 * -1 .
      ENDIF.
      <fs_dly_prd>-prod_vl_qty1    = <fs_dly_prd>-prod_vl_qty1 + ls_zpra_t_dly_prd-prod_vl_qty1 .
    ENDLOOP.
    SORT  gt_zpra_t_dly_prd_2a3 BY production_date product asset .
  ENDIF.


  lt_zpra_t_mrec_prd  = gt_zpra_t_mrec_prd_2a3 .
  SORT  lt_zpra_t_mrec_prd BY gjahr monat product asset block prd_vl_type .

  DELETE gt_zpra_t_mrec_prd_2a3  WHERE product EQ c_prod_gas .
  DELETE lt_zpra_t_mrec_prd      WHERE product NE c_prod_gas .
  CHECK lt_zpra_t_mrec_prd IS NOT INITIAL.

  LOOP AT lt_zpra_t_mrec_prd INTO ls_zpra_t_mrec_prd .
    lv_index = sy-tabix .
    IF lv_index EQ 1 OR
      <fs_mrec>-gjahr     NE ls_zpra_t_mrec_prd-gjahr   OR
      <fs_mrec>-monat     NE ls_zpra_t_mrec_prd-monat   OR
      <fs_mrec>-product   NE ls_zpra_t_mrec_prd-product OR
      <fs_mrec>-asset     NE ls_zpra_t_mrec_prd-asset   OR
      <fs_mrec>-block     NE ls_zpra_t_mrec_prd-block .

      APPEND INITIAL LINE TO gt_zpra_t_mrec_prd_2a3 ASSIGNING <fs_mrec> .
      <fs_mrec>-gjahr         = ls_zpra_t_mrec_prd-gjahr .
      <fs_mrec>-monat         = ls_zpra_t_mrec_prd-monat .
      <fs_mrec>-product       = ls_zpra_t_mrec_prd-product .
      <fs_mrec>-asset         = ls_zpra_t_mrec_prd-asset .
      <fs_mrec>-block         = ls_zpra_t_mrec_prd-block .
      <fs_mrec>-prod_vl_uom1  = ls_zpra_t_mrec_prd-prod_vl_uom1 .
      <fs_mrec>-prd_vl_type   = 'NET_PROD' .
    ENDIF.
    PERFORM convert_mrec_gas_units CHANGING ls_zpra_t_mrec_prd.
    PERFORM convert_mrec_gas_per_day_units USING lv_days CHANGING ls_zpra_t_mrec_prd .
    IF ls_zpra_t_mrec_prd-prd_vl_type EQ 'GAS_INJ' .
      ls_zpra_t_mrec_prd-prod_vl_qty1 = ls_zpra_t_mrec_prd-prod_vl_qty1 * -1 .
    ENDIF.
    <fs_mrec>-prod_vl_qty1    = <fs_mrec>-prod_vl_qty1 + ls_zpra_t_mrec_prd-prod_vl_qty1 .
  ENDLOOP.
* SORT  gt_zpra_t_mrec_prd_2a3 BY gjahr monat product asset block prd_vl_type . .
  SORT gt_zpra_t_mrec_prd_2a3 BY product ASCENDING asset ASCENDING block  ASCENDING gjahr DESCENDING monat DESCENDING .

ENDFORM.
FORM process_gas_records_2d .

  DATA : lt_zpra_t_dly_prd  TYPE STANDARD TABLE OF zpra_t_dly_prd,
         lt_zpra_t_mrec_prd TYPE STANDARD TABLE OF ty_zpra_t_mrec_prd.

  DATA : ls_zpra_t_dly_prd  TYPE                  zpra_t_dly_prd,
         ls_zpra_t_mrec_prd TYPE                  ty_zpra_t_mrec_prd.
  FIELD-SYMBOLS : <fs_dly_prd> TYPE zpra_t_dly_prd,
                  <fs_mrec>    TYPE ty_zpra_t_mrec_prd.

  DATA : lv_index            TYPE sy-tabix,
         lv_days             TYPE sy-tabix,
         lv_poper            TYPE t009b-poper,
         lv_month_begin_date TYPE sy-datum,
         lv_month_end_date   TYPE sy-datum.

  lt_zpra_t_dly_prd  = gt_zpra_t_dly_prd_2d .
  SORT  lt_zpra_t_dly_prd BY production_date product asset block prd_vl_type .

  DELETE gt_zpra_t_dly_prd_2d  WHERE product EQ c_prod_gas .
  DELETE lt_zpra_t_dly_prd     WHERE product NE c_prod_gas .

  IF lt_zpra_t_dly_prd IS NOT INITIAL.

    LOOP AT lt_zpra_t_dly_prd INTO ls_zpra_t_dly_prd .
      lv_index = sy-tabix .
      IF lv_index EQ 1 OR
        <fs_dly_prd>-production_date NE ls_zpra_t_dly_prd-production_date  OR
        <fs_dly_prd>-product         NE ls_zpra_t_dly_prd-product          OR
        <fs_dly_prd>-asset           NE ls_zpra_t_dly_prd-asset            OR
        <fs_dly_prd>-block           NE ls_zpra_t_dly_prd-block .

        APPEND INITIAL LINE TO gt_zpra_t_dly_prd_2d ASSIGNING <fs_dly_prd> .
        <fs_dly_prd>-mandt           = ls_zpra_t_dly_prd-mandt .
        <fs_dly_prd>-production_date = ls_zpra_t_dly_prd-production_date .
        <fs_dly_prd>-product         = ls_zpra_t_dly_prd-product .
        <fs_dly_prd>-asset           = ls_zpra_t_dly_prd-asset .
        <fs_dly_prd>-block           = ls_zpra_t_dly_prd-block .
        <fs_dly_prd>-prod_vl_uom1    = ls_zpra_t_dly_prd-prod_vl_uom1 .
        <fs_dly_prd>-prd_vl_type     = 'NET_PROD' .
      ENDIF.
      PERFORM convert_gas_units CHANGING ls_zpra_t_dly_prd.

      CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
        EXPORTING
          iv_date             = ls_zpra_t_dly_prd-production_date
        IMPORTING
          ev_month_begin_date = lv_month_begin_date
          ev_month_end_date   = lv_month_end_date.
*   lv_days = lv_month_end_date - lv_month_begin_date + 1 .
*   PERFORM convert_gas_per_day_units USING lv_days CHANGING ls_zpra_t_dly_prd .

      IF ls_zpra_t_dly_prd-prd_vl_type EQ 'GAS_INJ' .
        ls_zpra_t_dly_prd-prod_vl_qty1 = ls_zpra_t_dly_prd-prod_vl_qty1 * -1 .
      ENDIF.
      <fs_dly_prd>-prod_vl_qty1    = <fs_dly_prd>-prod_vl_qty1 + ls_zpra_t_dly_prd-prod_vl_qty1 .
    ENDLOOP.
    SORT  gt_zpra_t_dly_prd_2d BY  product asset block production_date.
  ENDIF.


  lt_zpra_t_mrec_prd  = gt_zpra_t_mrec_prd_2d .
  SORT  lt_zpra_t_mrec_prd BY gjahr monat product asset block prd_vl_type .

  DELETE gt_zpra_t_mrec_prd_2d  WHERE product EQ c_prod_gas .
  DELETE lt_zpra_t_mrec_prd     WHERE product NE c_prod_gas .
  CHECK lt_zpra_t_mrec_prd IS NOT INITIAL.

  LOOP AT lt_zpra_t_mrec_prd INTO ls_zpra_t_mrec_prd .
    lv_index = sy-tabix .
    IF lv_index EQ 1 OR
      <fs_mrec>-gjahr     NE ls_zpra_t_mrec_prd-gjahr   OR
      <fs_mrec>-monat     NE ls_zpra_t_mrec_prd-monat   OR
      <fs_mrec>-product   NE ls_zpra_t_mrec_prd-product OR
      <fs_mrec>-asset     NE ls_zpra_t_mrec_prd-asset   OR
      <fs_mrec>-block     NE ls_zpra_t_mrec_prd-block .

      APPEND INITIAL LINE TO gt_zpra_t_mrec_prd_2d ASSIGNING <fs_mrec> .
      <fs_mrec>-gjahr         = ls_zpra_t_mrec_prd-gjahr .
      <fs_mrec>-monat         = ls_zpra_t_mrec_prd-monat .
      <fs_mrec>-product       = ls_zpra_t_mrec_prd-product .
      <fs_mrec>-asset         = ls_zpra_t_mrec_prd-asset .
      <fs_mrec>-block         = ls_zpra_t_mrec_prd-block .
      <fs_mrec>-prod_vl_uom1  = ls_zpra_t_mrec_prd-prod_vl_uom1 .
      <fs_mrec>-prd_vl_type   = 'NET_PROD' .
    ENDIF.
    PERFORM convert_mrec_gas_units CHANGING ls_zpra_t_mrec_prd.

    lv_poper = ls_zpra_t_mrec_prd-monat .
    CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
      EXPORTING
        i_gjahr        = ls_zpra_t_mrec_prd-gjahr
*       I_MONMIT       = 00
        i_periv        = gv_periv
        i_poper        = lv_poper
      IMPORTING
        e_date         = lv_month_begin_date
      EXCEPTIONS
        input_false    = 1
        t009_notfound  = 2
        t009b_notfound = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      MESSAGE 'Internal Error' TYPE 'E' .
    ENDIF.

    CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
      EXPORTING
        i_gjahr        = ls_zpra_t_mrec_prd-gjahr
*       I_MONMIT       = 00
        i_periv        = gv_periv
        i_poper        = lv_poper
      IMPORTING
        e_date         = lv_month_end_date
      EXCEPTIONS
        input_false    = 1
        t009_notfound  = 2
        t009b_notfound = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      MESSAGE 'Internal Error' TYPE 'E' .
    ENDIF.

*   lv_days = lv_month_end_date - lv_month_begin_date + 1 .
*   PERFORM convert_mrec_gas_per_day_units USING lv_days CHANGING ls_zpra_t_mrec_prd .

    IF ls_zpra_t_mrec_prd-prd_vl_type EQ 'GAS_INJ' .
      ls_zpra_t_mrec_prd-prod_vl_qty1 = ls_zpra_t_mrec_prd-prod_vl_qty1 * -1 .
    ENDIF.
    <fs_mrec>-prod_vl_qty1    = <fs_mrec>-prod_vl_qty1 + ls_zpra_t_mrec_prd-prod_vl_qty1 .
  ENDLOOP.
* SORT  gt_zpra_t_mrec_prd_2d BY gjahr monat product asset block prd_vl_type . .
  SORT gt_zpra_t_mrec_prd_2d BY product ASCENDING asset ASCENDING block  ASCENDING gjahr DESCENDING monat DESCENDING .
ENDFORM.
FORM process_gas_records_3c .

  DATA : lt_zpra_t_dly_prd  TYPE STANDARD TABLE OF zpra_t_dly_prd,
         lt_zpra_t_mrec_app TYPE STANDARD TABLE OF ty_zpra_t_mrec_app,
         lt_zpra_t_mrec_prd TYPE STANDARD TABLE OF ty_zpra_t_mrec_prd.

  DATA : ls_zpra_t_dly_prd  TYPE                  zpra_t_dly_prd,
         ls_zpra_t_mrec_app TYPE                  ty_zpra_t_mrec_app,
         ls_zpra_t_mrec_prd TYPE                  ty_zpra_t_mrec_prd.
  FIELD-SYMBOLS : <fs_dly_prd>  TYPE zpra_t_dly_prd,
                  <fs_mrec_app> TYPE ty_zpra_t_mrec_app,
                  <fs_mrec_prd> TYPE ty_zpra_t_mrec_prd.

  DATA : lv_index            TYPE sy-tabix,
         lv_days             TYPE sy-tabix,
         lv_poper            TYPE t009b-poper,
         lv_month_begin_date TYPE sy-datum,
         lv_month_end_date   TYPE sy-datum.

  lt_zpra_t_dly_prd  = gt_zpra_t_dly_prd_3c .
  SORT  lt_zpra_t_dly_prd BY production_date product asset block prd_vl_type .

  DELETE gt_zpra_t_dly_prd_3c  WHERE product EQ c_prod_gas .
  DELETE lt_zpra_t_dly_prd     WHERE product NE c_prod_gas .

  IF lt_zpra_t_dly_prd IS NOT INITIAL.

    LOOP AT lt_zpra_t_dly_prd INTO ls_zpra_t_dly_prd .
      lv_index = sy-tabix .
      IF lv_index EQ 1 OR
        <fs_dly_prd>-production_date NE ls_zpra_t_dly_prd-production_date  OR
        <fs_dly_prd>-product         NE ls_zpra_t_dly_prd-product          OR
        <fs_dly_prd>-asset           NE ls_zpra_t_dly_prd-asset            OR
        <fs_dly_prd>-block           NE ls_zpra_t_dly_prd-block .

        APPEND INITIAL LINE TO gt_zpra_t_dly_prd_3c ASSIGNING <fs_dly_prd> .
        <fs_dly_prd>-mandt           = ls_zpra_t_dly_prd-mandt .
        <fs_dly_prd>-production_date = ls_zpra_t_dly_prd-production_date .
        <fs_dly_prd>-product         = ls_zpra_t_dly_prd-product .
        <fs_dly_prd>-asset           = ls_zpra_t_dly_prd-asset .
        <fs_dly_prd>-block           = ls_zpra_t_dly_prd-block .
        <fs_dly_prd>-prod_vl_uom1    = ls_zpra_t_dly_prd-prod_vl_uom1 .
        <fs_dly_prd>-prd_vl_type     = 'NET_PROD' .
      ENDIF.
      PERFORM convert_gas_units_to_bcm CHANGING ls_zpra_t_dly_prd.

      IF ls_zpra_t_dly_prd-prd_vl_type EQ 'GAS_INJ' .
        ls_zpra_t_dly_prd-prod_vl_qty1 = ls_zpra_t_dly_prd-prod_vl_qty1 * -1 .
      ENDIF.
      <fs_dly_prd>-prod_vl_qty1    = <fs_dly_prd>-prod_vl_qty1 + ls_zpra_t_dly_prd-prod_vl_qty1 .
    ENDLOOP.
    SORT  gt_zpra_t_dly_prd_3c BY  product asset block production_date.
  ENDIF.


  lt_zpra_t_mrec_prd  = gt_zpra_t_mrec_prd_3c .
  SORT  lt_zpra_t_mrec_prd BY gjahr monat product asset block prd_vl_type .

  DELETE gt_zpra_t_mrec_prd_3c  WHERE product EQ c_prod_gas .
  DELETE lt_zpra_t_mrec_prd  WHERE product NE c_prod_gas .

  IF lt_zpra_t_mrec_prd IS NOT INITIAL.

    LOOP AT lt_zpra_t_mrec_prd INTO ls_zpra_t_mrec_prd .
      lv_index = sy-tabix .
      IF lv_index EQ 1 OR
        <fs_mrec_prd>-gjahr     NE ls_zpra_t_mrec_prd-gjahr   OR
        <fs_mrec_prd>-monat     NE ls_zpra_t_mrec_prd-monat   OR
        <fs_mrec_prd>-product   NE ls_zpra_t_mrec_prd-product OR
        <fs_mrec_prd>-asset     NE ls_zpra_t_mrec_prd-asset   OR
        <fs_mrec_prd>-block     NE ls_zpra_t_mrec_prd-block .

        APPEND INITIAL LINE TO gt_zpra_t_mrec_prd_3c ASSIGNING <fs_mrec_prd> .
        <fs_mrec_prd>-gjahr         = ls_zpra_t_mrec_prd-gjahr .
        <fs_mrec_prd>-monat         = ls_zpra_t_mrec_prd-monat .
        <fs_mrec_prd>-product       = ls_zpra_t_mrec_prd-product .
        <fs_mrec_prd>-asset         = ls_zpra_t_mrec_prd-asset .
        <fs_mrec_prd>-block         = ls_zpra_t_mrec_prd-block .
        <fs_mrec_prd>-prod_vl_uom1    = ls_zpra_t_mrec_prd-prod_vl_uom1 .
        <fs_mrec_prd>-prd_vl_type   = 'NET_PROD' .
      ENDIF.
      PERFORM convert_mrec_to_bcm CHANGING ls_zpra_t_mrec_prd.

      IF ls_zpra_t_mrec_prd-prd_vl_type EQ 'GAS_INJ' .
        ls_zpra_t_mrec_prd-prod_vl_qty1 = ls_zpra_t_mrec_prd-prod_vl_qty1 * -1 .
      ENDIF.
      <fs_mrec_prd>-prod_vl_qty1    = <fs_mrec_prd>-prod_vl_qty1 + ls_zpra_t_mrec_prd-prod_vl_qty1 .
    ENDLOOP.
  ENDIF.
* SORT  gt_zpra_t_mrec_app BY gjahr monat product asset block prd_vl_type .
  SORT gt_zpra_t_mrec_prd_3c BY product ASCENDING asset ASCENDING block  ASCENDING gjahr DESCENDING monat DESCENDING .

ENDFORM.
FORM process_gas_records_3f .

  DATA : lt_zpra_t_dly_prd  TYPE STANDARD TABLE OF zpra_t_dly_prd,
         lt_zpra_t_mrec_app TYPE STANDARD TABLE OF ty_zpra_t_mrec_app,
         lt_zpra_t_mrec_prd TYPE STANDARD TABLE OF ty_zpra_t_mrec_prd.

  DATA : ls_zpra_t_dly_prd  TYPE                  zpra_t_dly_prd,
         ls_zpra_t_mrec_app TYPE                  ty_zpra_t_mrec_app,
         ls_zpra_t_mrec_prd TYPE                  ty_zpra_t_mrec_prd.
  FIELD-SYMBOLS : <fs_dly_prd>  TYPE zpra_t_dly_prd,
                  <fs_mrec_app> TYPE ty_zpra_t_mrec_app,
                  <fs_mrec_prd> TYPE ty_zpra_t_mrec_prd.

  DATA : lv_index            TYPE sy-tabix,
         lv_days             TYPE sy-tabix,
         lv_poper            TYPE t009b-poper,
         lv_month_begin_date TYPE sy-datum,
         lv_month_end_date   TYPE sy-datum.

  lt_zpra_t_dly_prd  = gt_zpra_t_dly_prd_3f .
  SORT  lt_zpra_t_dly_prd BY production_date product asset block prd_vl_type .

  DELETE gt_zpra_t_dly_prd_3f  WHERE product EQ c_prod_gas .
  DELETE lt_zpra_t_dly_prd     WHERE product NE c_prod_gas .

  IF lt_zpra_t_dly_prd IS NOT INITIAL.

    LOOP AT lt_zpra_t_dly_prd INTO ls_zpra_t_dly_prd .
      lv_index = sy-tabix .
      IF lv_index EQ 1 OR
        <fs_dly_prd>-production_date NE ls_zpra_t_dly_prd-production_date  OR
        <fs_dly_prd>-product         NE ls_zpra_t_dly_prd-product          OR
        <fs_dly_prd>-asset           NE ls_zpra_t_dly_prd-asset            OR
        <fs_dly_prd>-block           NE ls_zpra_t_dly_prd-block .

        APPEND INITIAL LINE TO gt_zpra_t_dly_prd_3f ASSIGNING <fs_dly_prd> .
        <fs_dly_prd>-mandt           = ls_zpra_t_dly_prd-mandt .
        <fs_dly_prd>-production_date = ls_zpra_t_dly_prd-production_date .
        <fs_dly_prd>-product         = ls_zpra_t_dly_prd-product .
        <fs_dly_prd>-asset           = ls_zpra_t_dly_prd-asset .
        <fs_dly_prd>-block           = ls_zpra_t_dly_prd-block .
        <fs_dly_prd>-prod_vl_uom1    = ls_zpra_t_dly_prd-prod_vl_uom1 .
        <fs_dly_prd>-prd_vl_type     = 'NET_PROD' .
      ENDIF.
      PERFORM convert_gas_units_to_bcm CHANGING ls_zpra_t_dly_prd.

      IF ls_zpra_t_dly_prd-prd_vl_type EQ 'GAS_INJ' .
        ls_zpra_t_dly_prd-prod_vl_qty1 = ls_zpra_t_dly_prd-prod_vl_qty1 * -1 .
      ENDIF.
      <fs_dly_prd>-prod_vl_qty1    = <fs_dly_prd>-prod_vl_qty1 + ls_zpra_t_dly_prd-prod_vl_qty1 .
    ENDLOOP.
    SORT  gt_zpra_t_dly_prd_3f BY  product asset block production_date.
  ENDIF.

  lt_zpra_t_mrec_prd  = gt_zpra_t_mrec_prd_3f .
  SORT  lt_zpra_t_mrec_prd BY gjahr monat product asset block prd_vl_type .

  DELETE gt_zpra_t_mrec_prd_3f  WHERE product EQ c_prod_gas .
  DELETE lt_zpra_t_mrec_prd  WHERE product NE c_prod_gas .

  IF lt_zpra_t_mrec_prd IS NOT INITIAL.

    LOOP AT lt_zpra_t_mrec_prd INTO ls_zpra_t_mrec_prd .
      lv_index = sy-tabix .
      IF lv_index EQ 1 OR
        <fs_mrec_prd>-gjahr     NE ls_zpra_t_mrec_prd-gjahr   OR
        <fs_mrec_prd>-monat     NE ls_zpra_t_mrec_prd-monat   OR
        <fs_mrec_prd>-product   NE ls_zpra_t_mrec_prd-product OR
        <fs_mrec_prd>-asset     NE ls_zpra_t_mrec_prd-asset   OR
        <fs_mrec_prd>-block     NE ls_zpra_t_mrec_prd-block .

        APPEND INITIAL LINE TO gt_zpra_t_mrec_prd_3f ASSIGNING <fs_mrec_prd> .
        <fs_mrec_prd>-gjahr         = ls_zpra_t_mrec_prd-gjahr .
        <fs_mrec_prd>-monat         = ls_zpra_t_mrec_prd-monat .
        <fs_mrec_prd>-product       = ls_zpra_t_mrec_prd-product .
        <fs_mrec_prd>-asset         = ls_zpra_t_mrec_prd-asset .
        <fs_mrec_prd>-block         = ls_zpra_t_mrec_prd-block .
        <fs_mrec_prd>-prod_vl_uom1    = ls_zpra_t_mrec_prd-prod_vl_uom1 .
        <fs_mrec_prd>-prd_vl_type   = 'NET_PROD' .
      ENDIF.
      PERFORM convert_mrec_to_bcm CHANGING ls_zpra_t_mrec_prd.

      IF ls_zpra_t_mrec_prd-prd_vl_type EQ 'GAS_INJ' .
        ls_zpra_t_mrec_prd-prod_vl_qty1 = ls_zpra_t_mrec_prd-prod_vl_qty1 * -1 .
      ENDIF.
      <fs_mrec_prd>-prod_vl_qty1    = <fs_mrec_prd>-prod_vl_qty1 + ls_zpra_t_mrec_prd-prod_vl_qty1 .
    ENDLOOP.
  ENDIF.
* SORT  gt_zpra_t_mrec_app BY gjahr monat product asset block prd_vl_type .
  SORT gt_zpra_t_mrec_prd_3f BY product ASCENDING asset ASCENDING block  ASCENDING gjahr DESCENDING monat DESCENDING .

ENDFORM.

FORM process_gas_records_4a .

  DATA : lt_zpra_t_dly_prd  TYPE STANDARD TABLE OF zpra_t_dly_prd,
         lt_zpra_t_mrec_app TYPE STANDARD TABLE OF ty_zpra_t_mrec_app.

  DATA : ls_zpra_t_dly_prd  TYPE                  zpra_t_dly_prd,
         ls_zpra_t_mrec_app TYPE                  ty_zpra_t_mrec_app.
  FIELD-SYMBOLS : <fs_dly_prd>  TYPE zpra_t_dly_prd,
                  <fs_mrec_app> TYPE ty_zpra_t_mrec_app.

  DATA : lv_index            TYPE sy-tabix,
         lv_days             TYPE sy-tabix,
         lv_poper            TYPE t009b-poper,
         lv_month_begin_date TYPE sy-datum,
         lv_month_end_date   TYPE sy-datum.

  lt_zpra_t_dly_prd  = gt_zpra_t_dly_prd_4a .
  SORT  lt_zpra_t_dly_prd BY production_date product asset block prd_vl_type .

  DELETE gt_zpra_t_dly_prd_4a  WHERE product EQ '722000004' .
  DELETE lt_zpra_t_dly_prd     WHERE product NE '722000004' .

  IF lt_zpra_t_dly_prd IS NOT INITIAL.

    LOOP AT lt_zpra_t_dly_prd INTO ls_zpra_t_dly_prd .
      lv_index = sy-tabix .
      IF lv_index EQ 1 OR
        <fs_dly_prd>-production_date NE ls_zpra_t_dly_prd-production_date  OR
        <fs_dly_prd>-product         NE ls_zpra_t_dly_prd-product          OR
        <fs_dly_prd>-asset           NE ls_zpra_t_dly_prd-asset            OR
        <fs_dly_prd>-block           NE ls_zpra_t_dly_prd-block .

        APPEND INITIAL LINE TO gt_zpra_t_dly_prd_4a ASSIGNING <fs_dly_prd> .
        <fs_dly_prd>-mandt           = ls_zpra_t_dly_prd-mandt .
        <fs_dly_prd>-production_date = ls_zpra_t_dly_prd-production_date .
        <fs_dly_prd>-product         = ls_zpra_t_dly_prd-product .
        <fs_dly_prd>-asset           = ls_zpra_t_dly_prd-asset .
        <fs_dly_prd>-block           = ls_zpra_t_dly_prd-block .
        <fs_dly_prd>-prod_vl_uom1    = ls_zpra_t_dly_prd-prod_vl_uom1 .
        <fs_dly_prd>-prd_vl_type     = 'NET_PROD' .
      ENDIF.
      PERFORM convert_gas_units_to_mmscm CHANGING ls_zpra_t_dly_prd.

      IF ls_zpra_t_dly_prd-prd_vl_type EQ 'GAS_INJ' .
        ls_zpra_t_dly_prd-prod_vl_qty1 = ls_zpra_t_dly_prd-prod_vl_qty1 * -1 .
      ENDIF.
      <fs_dly_prd>-prod_vl_qty1    = <fs_dly_prd>-prod_vl_qty1 + ls_zpra_t_dly_prd-prod_vl_qty1 .
    ENDLOOP.
    SORT  gt_zpra_t_dly_prd_4a BY  product asset block production_date.
  ENDIF.


  lt_zpra_t_mrec_app  = gt_zpra_t_mrec_app_4a .
  SORT  lt_zpra_t_mrec_app BY gjahr monat product asset block prd_vl_type .

  DELETE gt_zpra_t_mrec_app_4a  WHERE product EQ '722000004' .
  DELETE lt_zpra_t_mrec_app     WHERE product NE '722000004' .
  CHECK lt_zpra_t_mrec_app IS NOT INITIAL.

  LOOP AT lt_zpra_t_mrec_app INTO ls_zpra_t_mrec_app .
    lv_index = sy-tabix .
    IF lv_index EQ 1 OR
      <fs_mrec_app>-gjahr     NE ls_zpra_t_mrec_app-gjahr   OR
      <fs_mrec_app>-monat     NE ls_zpra_t_mrec_app-monat   OR
      <fs_mrec_app>-product   NE ls_zpra_t_mrec_app-product OR
      <fs_mrec_app>-asset     NE ls_zpra_t_mrec_app-asset   OR
      <fs_mrec_app>-block     NE ls_zpra_t_mrec_app-block .

      APPEND INITIAL LINE TO gt_zpra_t_mrec_app_4a ASSIGNING <fs_mrec_app> .
      <fs_mrec_app>-gjahr         = ls_zpra_t_mrec_app-gjahr .
      <fs_mrec_app>-monat         = ls_zpra_t_mrec_app-monat .
      <fs_mrec_app>-product       = ls_zpra_t_mrec_app-product .
      <fs_mrec_app>-asset         = ls_zpra_t_mrec_app-asset .
      <fs_mrec_app>-block         = ls_zpra_t_mrec_app-block .
      <fs_mrec_app>-app_vl_uom    = ls_zpra_t_mrec_app-app_vl_uom .
      <fs_mrec_app>-prd_vl_type   = 'NET_PROD' .
    ENDIF.
    PERFORM convert_mrec_gas_to_mmscm CHANGING ls_zpra_t_mrec_app.

    IF ls_zpra_t_mrec_app-prd_vl_type EQ 'GAS_INJ' .
      ls_zpra_t_mrec_app-app_vl_qty = ls_zpra_t_mrec_app-app_vl_qty * -1 .
    ENDIF.
    <fs_mrec_app>-app_vl_qty    = <fs_mrec_app>-app_vl_qty + ls_zpra_t_mrec_app-app_vl_qty .
  ENDLOOP.
  SORT  gt_zpra_t_mrec_app_4a BY gjahr monat product asset block prd_vl_type . .

ENDFORM.

FORM process_gas_records_2f .

  DATA : lt_zpra_t_dly_prd  TYPE STANDARD TABLE OF zpra_t_dly_prd,
         lt_zpra_t_mrec_prd TYPE STANDARD TABLE OF ty_zpra_t_mrec_prd.

  DATA : ls_zpra_t_dly_prd  TYPE                  zpra_t_dly_prd,
         ls_zpra_t_mrec_prd TYPE                  ty_zpra_t_mrec_prd.
  FIELD-SYMBOLS : <fs_dly_prd> TYPE zpra_t_dly_prd,
                  <fs_mrec>    TYPE ty_zpra_t_mrec_prd.

  DATA : lv_index            TYPE sy-tabix,
         lv_days             TYPE sy-tabix,
         lv_poper            TYPE t009b-poper,
         lv_month_begin_date TYPE sy-datum,
         lv_month_end_date   TYPE sy-datum.

  lt_zpra_t_dly_prd  = gt_zpra_t_dly_prd_2f .
  SORT  lt_zpra_t_dly_prd BY production_date product asset block prd_vl_type .

  DELETE gt_zpra_t_dly_prd_2f  WHERE product EQ c_prod_gas .
  DELETE lt_zpra_t_dly_prd     WHERE product NE c_prod_gas .

  IF lt_zpra_t_dly_prd IS NOT INITIAL.

    LOOP AT lt_zpra_t_dly_prd INTO ls_zpra_t_dly_prd .
      lv_index = sy-tabix .
      IF lv_index EQ 1 OR
        <fs_dly_prd>-production_date NE ls_zpra_t_dly_prd-production_date  OR
        <fs_dly_prd>-product         NE ls_zpra_t_dly_prd-product          OR
        <fs_dly_prd>-asset           NE ls_zpra_t_dly_prd-asset            OR
        <fs_dly_prd>-block           NE ls_zpra_t_dly_prd-block .

        APPEND INITIAL LINE TO gt_zpra_t_dly_prd_2f ASSIGNING <fs_dly_prd> .
        <fs_dly_prd>-mandt           = ls_zpra_t_dly_prd-mandt .
        <fs_dly_prd>-production_date = ls_zpra_t_dly_prd-production_date .
        <fs_dly_prd>-product         = ls_zpra_t_dly_prd-product .
        <fs_dly_prd>-asset           = ls_zpra_t_dly_prd-asset .
        <fs_dly_prd>-block           = ls_zpra_t_dly_prd-block .
        <fs_dly_prd>-prod_vl_uom1    = ls_zpra_t_dly_prd-prod_vl_uom1 .
        <fs_dly_prd>-prd_vl_type     = 'NET_PROD' .
      ENDIF.
      PERFORM convert_gas_units CHANGING ls_zpra_t_dly_prd.

      CALL FUNCTION 'HR_JP_MONTH_BEGIN_END_DATE'
        EXPORTING
          iv_date             = ls_zpra_t_dly_prd-production_date
        IMPORTING
          ev_month_begin_date = lv_month_begin_date
          ev_month_end_date   = lv_month_end_date.
      lv_days = lv_month_end_date - lv_month_begin_date + 1 .
      lv_days = 1 . "will be calculated at end .
      PERFORM convert_gas_per_day_units USING lv_days CHANGING ls_zpra_t_dly_prd .
      IF ls_zpra_t_dly_prd-prd_vl_type EQ 'GAS_INJ' .
        ls_zpra_t_dly_prd-prod_vl_qty1 = ls_zpra_t_dly_prd-prod_vl_qty1 * -1 .
      ENDIF.
      <fs_dly_prd>-prod_vl_qty1    = <fs_dly_prd>-prod_vl_qty1 + ls_zpra_t_dly_prd-prod_vl_qty1 .
    ENDLOOP.
    SORT  gt_zpra_t_dly_prd_2f BY  product asset block production_date.
  ENDIF.


  lt_zpra_t_mrec_prd  = gt_zpra_t_mrec_prd_2f .
  SORT  lt_zpra_t_mrec_prd BY gjahr monat product asset block prd_vl_type .

  DELETE gt_zpra_t_mrec_prd_2f  WHERE product EQ c_prod_gas .
  DELETE lt_zpra_t_mrec_prd     WHERE product NE c_prod_gas .
  CHECK lt_zpra_t_mrec_prd IS NOT INITIAL.

  LOOP AT lt_zpra_t_mrec_prd INTO ls_zpra_t_mrec_prd .
    lv_index = sy-tabix .
    IF lv_index EQ 1 OR
      <fs_mrec>-gjahr     NE ls_zpra_t_mrec_prd-gjahr   OR
      <fs_mrec>-monat     NE ls_zpra_t_mrec_prd-monat   OR
      <fs_mrec>-product   NE ls_zpra_t_mrec_prd-product OR
      <fs_mrec>-asset     NE ls_zpra_t_mrec_prd-asset   OR
      <fs_mrec>-block     NE ls_zpra_t_mrec_prd-block .

      APPEND INITIAL LINE TO gt_zpra_t_mrec_prd_2f ASSIGNING <fs_mrec> .
      <fs_mrec>-gjahr         = ls_zpra_t_mrec_prd-gjahr .
      <fs_mrec>-monat         = ls_zpra_t_mrec_prd-monat .
      <fs_mrec>-product       = ls_zpra_t_mrec_prd-product .
      <fs_mrec>-asset         = ls_zpra_t_mrec_prd-asset .
      <fs_mrec>-block         = ls_zpra_t_mrec_prd-block .
      <fs_mrec>-prod_vl_uom1  = ls_zpra_t_mrec_prd-prod_vl_uom1 .
      <fs_mrec>-prd_vl_type   = 'NET_PROD' .
    ENDIF.
    PERFORM convert_mrec_gas_units CHANGING ls_zpra_t_mrec_prd.

    lv_poper = ls_zpra_t_mrec_prd-monat .
    CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
      EXPORTING
        i_gjahr        = ls_zpra_t_mrec_prd-gjahr
*       I_MONMIT       = 00
        i_periv        = gv_periv
        i_poper        = lv_poper
      IMPORTING
        e_date         = lv_month_begin_date
      EXCEPTIONS
        input_false    = 1
        t009_notfound  = 2
        t009b_notfound = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      MESSAGE 'Internal Error' TYPE 'E' .
    ENDIF.

    CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
      EXPORTING
        i_gjahr        = ls_zpra_t_mrec_prd-gjahr
*       I_MONMIT       = 00
        i_periv        = gv_periv
        i_poper        = lv_poper
      IMPORTING
        e_date         = lv_month_end_date
      EXCEPTIONS
        input_false    = 1
        t009_notfound  = 2
        t009b_notfound = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      MESSAGE 'Internal Error' TYPE 'E' .
    ENDIF.

    lv_days = lv_month_end_date - lv_month_begin_date + 1 .
    lv_days = 1 . "will be calculated at end
    PERFORM convert_mrec_gas_per_day_units USING lv_days CHANGING ls_zpra_t_mrec_prd .
    IF ls_zpra_t_mrec_prd-prd_vl_type EQ 'GAS_INJ' .
      ls_zpra_t_mrec_prd-prod_vl_qty1 = ls_zpra_t_mrec_prd-prod_vl_qty1 * -1 .
    ENDIF.
    <fs_mrec>-prod_vl_qty1    = <fs_mrec>-prod_vl_qty1 + ls_zpra_t_mrec_prd-prod_vl_qty1 .
  ENDLOOP.
* SORT  gt_zpra_t_mrec_prd_2f BY gjahr monat product asset block prd_vl_type . .
  SORT gt_zpra_t_mrec_prd_2f  BY product ASCENDING asset ASCENDING block  ASCENDING gjahr DESCENDING monat DESCENDING .
ENDFORM.
FORM convert_gas_per_day_units  USING    p_days
                                CHANGING p_zpra_t_dly_prd TYPE zpra_t_dly_prd.
  CHECK p_zpra_t_dly_prd-product EQ c_prod_gas .
  IF p_bbd IS NOT INITIAL OR
     p_tmd IS NOT INITIAL OR
     p_bmd IS NOT INITIAL .
    p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / p_days .
  ENDIF.
ENDFORM.
FORM convert_mrec_gas_per_day_units  USING    p_days
                                CHANGING p_zpra_t_mrec_prd TYPE ty_zpra_t_mrec_prd.
  CHECK p_zpra_t_mrec_prd-product EQ '722000004' .
  IF p_bbd IS NOT INITIAL OR
     p_tmd IS NOT INITIAL OR
     p_bmd IS NOT INITIAL .
    p_zpra_t_mrec_prd-prod_vl_qty1 = p_zpra_t_mrec_prd-prod_vl_qty1 / p_days .
  ENDIF.
ENDFORM.
FORM finalize_worksheet .
  DATA: lv_sheet_name TYPE char50,
        lv_uzeit_ext  TYPE char8,
        lv_datum_ext  TYPE char10.
  CONCATENATE sy-uzeit(2) '-' sy-uzeit+2(2) '-' sy-uzeit+4(2) INTO lv_uzeit_ext .
  CONCATENATE sy-datum+6(2) '-' sy-datum+4(2) '-' sy-datum(4) INTO lv_datum_ext .

  PERFORM free_clipboard .
  PERFORM lock_xls .

  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
    EXPORTING
      input  = gv_rep_date
    IMPORTING
      output = lv_sheet_name.
  IF sy-subrc <> 0.
    MESSAGE 'Unexpteced Internal Error' TYPE 'E' .
  ENDIF.

  REPLACE ALL OCCURRENCES OF '.' IN lv_sheet_name WITH '-' .
  CONCATENATE 'DPR (' lv_sheet_name ')' INTO lv_sheet_name SEPARATED BY space .
  SET PROPERTY OF go_worksheet 'Name' = lv_sheet_name.
  SET PROPERTY OF go_excel 'VISIBLE' = 1 .
  SET PROPERTY OF go_worksheet2 'Visible' = 0 .
  PERFORM select_range USING 1 1 1 1 .
  .
  CONCATENATE p_fname '\DPR -' gv_repdate_e '-' 'On -' lv_datum_ext '-' lv_uzeit_ext '.PDF' INTO p_fname .
* CONCATENATE p_fname '\DPR -' gv_repdate_e '-' 'Extracted On -' lv_datum_ext '-' lv_uzeit_ext '.PDF' INTO p_fname .

  CALL METHOD OF go_workbook 'ExportAsFixedFormat'
    EXPORTING
      #1 = 0
      #2 = p_fname
      #3 = 0
      #4 = 1.

*CALL METHOD OF go_workbook 'Close'
*EXPORTING #1 = 0 .
*CALL METHOD OF go_workbook 'Quit'.
*  PERFORM delete_image   .
  EXPORT p_fname TO MEMORY ID 'DPR_FILE_NAME' .
  MESSAGE 'Report downloaded sucessfully' TYPE 'S' .
ENDFORM.
FORM lock_xls .

  DATA : lv_uuid_16    TYPE sysuuid_x16 .

* lv_uuid_16 = cl_system_uuid=>create_uuid_x16_static( ).
*  CALL METHOD OF go_worksheet 'PROTECT'
*    EXPORTING #1 = lv_uuid_16.

ENDFORM.

FORM fill_dynamic_table_sec2d .

  DATA : lt_dly_prd_main   TYPE STANDARD TABLE OF zpra_t_dly_prd,
         lt_zpra_t_dly_prd TYPE STANDARD TABLE OF zpra_t_dly_prd.
  DATA : ls_dly_prd_main   TYPE                   zpra_t_dly_prd,
         ls_zpra_t_dly_prd TYPE                   zpra_t_dly_prd.
  DATA : lv_monat         TYPE                   zpra_t_mrec_prd-monat,
         lv_gjahr         TYPE                   zpra_t_mrec_prd-gjahr,
         lv_gjahr_next    TYPE                   zpra_t_mrec_prd-gjahr,
         lv_combine_field TYPE                   c,
         lv_days          TYPE                   sy-tabix,
         lv_days2         TYPE                   sy-tabix,
         lv_index         TYPE                   sy-tabix,
         lv_rprd_index    TYPE                   sy-tabix,
         lv_col_name      TYPE                   lvc_fname,
         lv_year_index    TYPE                   sy-tabix,
         lv_start_date    TYPE                   sy-datum,
         lv_mmscmd_mul    TYPE                   sy-tabix,
         lv_product       TYPE                   char100,
         lv_asset         TYPE                   char100.

  gt_zpra_t_mrec_prd =  gt_zpra_t_mrec_prd_2d .
  lt_dly_prd_main = gt_zpra_t_dly_prd_2d .
  SORT lt_dly_prd_main BY product asset block  .
  DELETE ADJACENT DUPLICATES FROM lt_dly_prd_main COMPARING product asset block  .
  lv_gjahr = gv_current_gjahr .
  DO 2 TIMES .
    lv_gjahr_next = lv_gjahr + 1 .
    APPEND INITIAL LINE TO <gfs_sec2d_table> ASSIGNING <gfs_dyn_line> .
    ASSIGN COMPONENT 'COL02' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    CONCATENATE lv_gjahr '-' lv_gjahr_next INTO <gfs_field> SEPARATED BY space.

    LOOP AT gt_zpra_c_prd_prof INTO gs_zpra_c_prd_prof.
      lv_monat = '00' .
      DO .
        lv_monat = lv_monat + 1 .
        CLEAR lv_combine_field .
        READ TABLE gt_zpra_t_mrec_app_2d INTO gs_zpra_t_mrec_app WITH KEY gjahr   = lv_gjahr
                                                                          monat   = lv_monat
                                                                          product = gs_zpra_c_prd_prof-product
                                                                          asset   = gs_zpra_c_prd_prof-asset
                                                                          block   = gs_zpra_c_prd_prof-block BINARY SEARCH .
        IF sy-subrc IS INITIAL. "if found in MREC APP
          gs_zpra_t_mrec_app-app_vl_qty = gs_zpra_t_mrec_app-app_vl_qty  * 1000000 .
          IF gs_zpra_t_mrec_app-product = c_prod_gas.
            READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_mrec_app-asset BINARY SEARCH .
            IF  sy-subrc IS INITIAL.
              lv_combine_field = 'X' .
            ENDIF.
          ENDIF.
*     Individual Column..
          gv_len = strlen( gs_zpra_t_mrec_app-product ) .
          IF lv_combine_field IS INITIAL.
            CONCATENATE gs_zpra_t_mrec_app-product(gv_len) '-' gs_zpra_t_mrec_app-asset INTO lv_col_name .
          ELSE.
            CONCATENATE gs_zpra_t_mrec_app-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
          ENDIF.
          ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF <gfs_field> IS ASSIGNED.
            <gfs_field> = <gfs_field> + gs_zpra_t_mrec_app-app_vl_qty .
            UNASSIGN <gfs_field> .
          ENDIF.
*     Product Total..
          CONCATENATE gs_zpra_t_mrec_app-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
          ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF <gfs_field> IS ASSIGNED.
            <gfs_field> = <gfs_field> + gs_zpra_t_mrec_app-app_vl_qty .
            UNASSIGN <gfs_field> .
          ENDIF.
*     Grand Total
          CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
          ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF <gfs_field> IS ASSIGNED.
            <gfs_field> = <gfs_field> + gs_zpra_t_mrec_app-app_vl_qty.
            UNASSIGN <gfs_field> .
          ENDIF.
**        ELSE . "Now get from MREC
**          READ TABLE gt_zpra_t_mrec_prd_2d INTO gs_zpra_t_mrec_prd WITH KEY gjahr   = lv_gjahr
**                                                                            monat   = lv_monat
**                                                                            product = gs_zpra_c_prd_prof-product
**                                                                            asset   = gs_zpra_c_prd_prof-asset
**                                                                            block   = gs_zpra_c_prd_prof-block BINARY SEARCH .
**          IF sy-subrc IS INITIAL. "if found in MREC
**            gs_zpra_t_dly_prd = ls_dly_prd_main .
**            PERFORM get_days_in_period USING gs_zpra_t_mrec_prd-gjahr gs_zpra_t_mrec_prd-monat CHANGING lv_days .
**            IF lv_monat EQ gv_current_monat.
**              gs_zpra_t_mrec_prd-prod_vl_qty2 = gs_zpra_t_mrec_prd-prod_vl_qty2 * gv_month_days_till_today / lv_days.
**              gs_zpra_t_mrec_prd-prod_vl_qty1 = gs_zpra_t_mrec_prd-prod_vl_qty1 * gv_month_days_till_today / lv_days.
**              CONCATENATE gv_search_end_datum(6) p_date+6(2) INTO gv_search_end_datum .
**            ENDIF.
***   Dividing monthly quantity is giving slightly off values, due to precision. Will sum the quantity for the whole period
***   and then divide by total days. to keep code structure intact, currently making days as 1
**            lv_days = 1 .
**            PERFORM convert_non_gas_mrec_units USING lv_days CHANGING gs_zpra_t_mrec_prd.
**            IF gs_zpra_t_mrec_prd-product = c_prod_gas.
**             READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_mrec_prd-asset BINARY SEARCH .
**             IF  sy-subrc IS INITIAL.
**               lv_combine_field = 'X' .
**             ENDIF.
**            ENDIF.
**            PERFORM get_constorium_multipliers_2a2 USING '5'.
**
***       Individual Column..
**           gv_len = strlen( gs_zpra_t_mrec_prd-product ) .
**           IF lv_combine_field IS INITIAL.
**             CONCATENATE gs_zpra_t_mrec_prd-product(gv_len) '-' gs_zpra_t_mrec_prd-asset INTO lv_col_name .
**           ELSE.
**             CONCATENATE gs_zpra_t_mrec_prd-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
**           ENDIF.
**           ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
**           IF <gfs_field> IS ASSIGNED.
**             <gfs_field> = <gfs_field> + gs_zpra_t_mrec_prd-prod_vl_qty1 * gv_individual_mul.
**             UNASSIGN <gfs_field> .
**           ENDIF.
***       Product Total..
**           CONCATENATE gs_zpra_t_mrec_prd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
**           ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
**           IF <gfs_field> IS ASSIGNED.
**             <gfs_field> = <gfs_field> + gs_zpra_t_mrec_prd-prod_vl_qty1 * gv_total_mul .
**             UNASSIGN <gfs_field> .
**           ENDIF.
***       Grand Total
**           CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
**           ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
**           IF <gfs_field> IS ASSIGNED.
**             <gfs_field> = <gfs_field> + gs_zpra_t_mrec_prd-prod_vl_qty1 * gv_grand_total_mul.
**             UNASSIGN <gfs_field> .
**           ENDIF.
        ELSE.
          PERFORM get_days_in_period USING lv_gjahr lv_monat CHANGING lv_days .
          IF lv_monat EQ gv_current_monat.
            CONCATENATE gv_search_end_datum(4) p_date+4(4) INTO gv_search_end_datum .
          ENDIF.
          READ TABLE gt_zpra_t_dly_rprd_2d INTO gs_zpra_t_dly_rprd WITH KEY product = gs_zpra_c_prd_prof-product
                                                                              asset = gs_zpra_c_prd_prof-asset
                                                                              block = gs_zpra_c_prd_prof-block
                                                                    production_date = gv_search_begin_datum BINARY SEARCH .
          IF sy-subrc IS NOT INITIAL.
            LOOP AT gt_zpra_t_dly_rprd_2d INTO gs_zpra_t_dly_rprd WHERE product         EQ gs_zpra_c_prd_prof-product
                                                                    AND asset           EQ gs_zpra_c_prd_prof-asset
                                                                    AND block           EQ gs_zpra_c_prd_prof-block
                                                                    AND production_date GE gv_search_begin_datum
                                                                    AND production_date LE gv_search_end_datum .
              EXIT .
            ENDLOOP .
          ENDIF.
          IF sy-subrc IS INITIAL.
            lv_rprd_index = sy-tabix .
            LOOP AT gt_zpra_t_dly_rprd_2d INTO gs_zpra_t_dly_rprd FROM lv_rprd_index .
              IF gs_zpra_t_dly_rprd-product EQ gs_zpra_c_prd_prof-product      AND
                 gs_zpra_t_dly_rprd-asset   EQ gs_zpra_c_prd_prof-asset        AND
                 gs_zpra_t_dly_rprd-block   EQ gs_zpra_c_prd_prof-block        AND
                 gs_zpra_t_dly_rprd-production_date GE gv_search_begin_datum   AND
                 gs_zpra_t_dly_rprd-production_date LE gv_search_end_datum .
              ELSE.
                EXIT .
              ENDIF.
              lv_index = sy-tabix .
              CLEAR lv_combine_field .

              PERFORM convert_rprd_units USING gs_zpra_t_dly_rprd .

              IF gs_zpra_t_dly_rprd-product = c_prod_gas .
                READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_dly_rprd-asset BINARY SEARCH .
                IF  sy-subrc IS INITIAL.
                  lv_combine_field = 'X' .
                ENDIF.
              ENDIF.
*           Individual Column..
              gv_len = strlen( gs_zpra_t_dly_rprd-product ) .
              IF lv_combine_field IS INITIAL.
                CONCATENATE gs_zpra_t_dly_rprd-product(gv_len) '-' gs_zpra_t_dly_rprd-asset INTO lv_col_name .
              ELSE.
                CONCATENATE gs_zpra_t_dly_rprd-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
              ENDIF.
              ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
              IF <gfs_field> IS ASSIGNED.
                <gfs_field> = <gfs_field> + gs_zpra_t_dly_rprd-jv_prd_vl_qty1.
                UNASSIGN <gfs_field> .
              ENDIF.
*           Product Total..
              CONCATENATE gs_zpra_t_dly_rprd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
              ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
              IF <gfs_field> IS ASSIGNED.
                <gfs_field> = <gfs_field> +  gs_zpra_t_dly_rprd-ovl_prd_vl_qty1 .
                UNASSIGN <gfs_field> .
              ENDIF.
*           Grand Total
              CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
              ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
              IF <gfs_field> IS ASSIGNED.
                lv_mmscmd_mul = 1 .
                IF gs_zpra_t_dly_rprd-product EQ c_prod_gas AND p_bmd IS NOT INITIAL.
*                  lv_mmscmd_mul = 6290 .
                ENDIF.
                <gfs_field> = <gfs_field> + ( gs_zpra_t_dly_rprd-ovl_prd_vl_qty1 * lv_mmscmd_mul ).
                UNASSIGN <gfs_field> .
              ENDIF.
            ENDLOOP.
          ELSE. "Now work out from daily production
            PERFORM get_days_in_period USING lv_gjahr lv_monat CHANGING lv_days .
            lv_days = 1 .
            IF lv_monat EQ gv_current_monat.
              CONCATENATE gv_search_end_datum(4) p_date+4(4) INTO gv_search_end_datum .
            ENDIF.
            LOOP AT gt_zpra_t_dly_prd_2d INTO gs_zpra_t_dly_prd WHERE product EQ gs_zpra_c_prd_prof-product
                                                                  AND asset   EQ gs_zpra_c_prd_prof-asset
                                                                  AND block   EQ gs_zpra_c_prd_prof-block
                                                                  AND production_date GE gv_search_begin_datum
                                                                  AND production_date LE gv_search_end_datum .
              lv_index = sy-tabix .
              CLEAR lv_combine_field .

              PERFORM convert_non_gas_units2 USING lv_days CHANGING gs_zpra_t_dly_prd .

              IF gs_zpra_t_dly_prd-product = c_prod_gas.
                READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_dly_prd-asset BINARY SEARCH .
                IF  sy-subrc IS INITIAL.
                  lv_combine_field = 'X' .
                ENDIF.
              ENDIF.
              PERFORM get_constorium_multipliers_2a2 USING '1'.
*           Individual Column..
              gv_len = strlen( gs_zpra_t_dly_prd-product ) .
              IF lv_combine_field IS INITIAL.
                CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' gs_zpra_t_dly_prd-asset INTO lv_col_name .
              ELSE.
                CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
              ENDIF.
              ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
              IF <gfs_field> IS ASSIGNED.
                <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_individual_mul.
                UNASSIGN <gfs_field> .
              ENDIF.
*           Product Total..
              CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
              ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
              IF <gfs_field> IS ASSIGNED.
                <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_total_mul .
                UNASSIGN <gfs_field> .
              ENDIF.
*           Grand Total
              CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
              ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
              IF <gfs_field> IS ASSIGNED.
                lv_mmscmd_mul = 1 .
                IF gs_zpra_t_dly_rprd-product EQ c_prod_gas AND p_bmd IS NOT INITIAL.
*                  lv_mmscmd_mul = 6290 .
                ENDIF.
*                <gfs_field> = <gfs_field> + ( gs_zpra_t_dly_prd-prod_vl_qty1 * lv_mmscmd_mul ).
                <gfs_field> = <gfs_field> + ( gs_zpra_t_dly_prd-prod_vl_qty1 * gv_grand_total_mul ).
                UNASSIGN <gfs_field> .
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
        IF lv_monat GE gv_current_monat.
          EXIT .
        ENDIF.
      ENDDO.
    ENDLOOP.
    lv_gjahr = lv_gjahr - 1 .
  ENDDO .
* Now dividing by no of days in case of per day units
  IF p_bbd IS NOT INITIAL OR
     p_tmd IS NOT INITIAL OR
     p_bmd IS NOT INITIAL .
    LOOP AT <gfs_sec2d_table> ASSIGNING <gfs_dyn_line> .
      lv_year_index = sy-tabix .
      DO .
        lv_index = sy-index .
        IF lv_index GT 2.
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF sy-subrc IS NOT INITIAL.
            EXIT .
          ENDIF.
          PERFORM get_asset_start_date USING lv_index CHANGING lv_start_date .
          IF lv_year_index = 1.
            IF lv_start_date GT gv_year_start_date .
              lv_days2 = p_date - lv_start_date + 1 .
            ELSE.
              lv_days2 = p_date - gv_year_start_date + 1 .
            ENDIF.
          ELSE.
            IF lv_start_date GT gv_last_year_s_date .
              lv_days2 = gv_last_year_t_date - lv_start_date + 1 .
            ELSE.
              lv_days2 = gv_last_year_t_date - gv_last_year_s_date + 1 .
            ENDIF.
          ENDIF.

          <gfs_field> = <gfs_field> / lv_days2 .
          UNASSIGN <gfs_field> .
        ENDIF.
      ENDDO.
    ENDLOOP .
  ENDIF.
  IF p_mb IS NOT INITIAL.
    LOOP AT <gfs_sec2d_table> ASSIGNING <gfs_dyn_line> .
      DO .
        lv_index = sy-index .
        ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF sy-subrc IS NOT INITIAL.
          EXIT .
        ELSE.
          IF lv_index GT 2.
            <gfs_field> = <gfs_field> / 1000000 .
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDIF.


  IF p_bmd IS NOT INITIAL.
    LOOP AT <gfs_sec2d_table> ASSIGNING <gfs_dyn_line> .
      DO .
        lv_index = sy-index .
        ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF sy-subrc IS NOT INITIAL.
          EXIT .
        ELSE.
          IF lv_index GT 2.
            READ TABLE gt_dyn_fcat INTO gs_dyn_fcat WITH KEY col_pos = lv_index BINARY SEARCH .
            IF sy-subrc IS INITIAL.
              SPLIT gs_dyn_fcat-fieldname AT '-' INTO lv_product lv_asset .
              IF lv_product EQ c_prod_gas.
                <gfs_field> = <gfs_field> / 6290 .
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDIF.

ENDFORM.
FORM fill_dynamic_table_sec2f .


  DATA : lt_dly_prd_main   TYPE STANDARD TABLE OF zpra_t_dly_prd,
         lt_zpra_t_dly_prd TYPE STANDARD TABLE OF zpra_t_dly_prd.
  DATA : ls_dly_prd_main   TYPE                   zpra_t_dly_prd,
         ls_zpra_t_dly_prd TYPE                   zpra_t_dly_prd.
  DATA : lv_monat         TYPE                   zpra_t_mrec_prd-monat,
         lv_gjahr         TYPE                   zpra_t_mrec_prd-gjahr,
         lv_gjahr_next    TYPE                   zpra_t_mrec_prd-gjahr,
         lv_combine_field TYPE                   c,
         lv_days          TYPE                   sy-tabix,
         lv_index         TYPE                   sy-tabix,
         lv_rprd_index    TYPE                   sy-tabix,
         lv_col_name      TYPE                   lvc_fname,
         lv_days2         TYPE                   sy-tabix,
         lv_start_date    TYPE                   sy-datum,
         lv_mmscmd_mul    TYPE                   sy-tabix,
         lv_product       TYPE                   char100,
         lv_asset         TYPE                   char100.

  gt_zpra_t_mrec_prd =  gt_zpra_t_mrec_prd_2f .
  lt_dly_prd_main = gt_zpra_t_dly_prd .
  SORT lt_dly_prd_main BY product asset block  .
  DELETE ADJACENT DUPLICATES FROM lt_dly_prd_main COMPARING product asset block  .

  lv_gjahr = gv_last_gjahr .
  lv_gjahr_next = lv_gjahr + 1 .
  APPEND INITIAL LINE TO <gfs_sec2f_table> ASSIGNING <gfs_dyn_line> .
  ASSIGN COMPONENT 'COL01' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
  CONCATENATE 'Actual Prod. :' lv_gjahr '-' lv_gjahr_next+2(2) INTO <gfs_field> SEPARATED BY space.

  ASSIGN COMPONENT 'COL02' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
*     <gfs_field> = 'Last Year' .
  LOOP AT gt_zpra_c_prd_prof INTO gs_zpra_c_prd_prof.
    lv_monat = '00' .
    DO .
      lv_monat = lv_monat + 1 .
      CLEAR lv_combine_field .
      READ TABLE gt_zpra_t_mrec_app_2f INTO gs_zpra_t_mrec_app WITH KEY gjahr   = lv_gjahr
                                                                        monat   = lv_monat
                                                                        product = gs_zpra_c_prd_prof-product
                                                                        asset   = gs_zpra_c_prd_prof-asset
                                                                        block   = gs_zpra_c_prd_prof-block BINARY SEARCH .
      IF sy-subrc IS INITIAL. "if found in MREC APP
        gs_zpra_t_mrec_app-app_vl_qty = gs_zpra_t_mrec_app-app_vl_qty * 1000000 .
        IF gs_zpra_t_mrec_app-product = c_prod_gas.
          READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_mrec_app-asset BINARY SEARCH .
          IF  sy-subrc IS INITIAL.
            lv_combine_field = 'X' .
          ENDIF.
        ENDIF.
*     Individual Column..
        gv_len = strlen( gs_zpra_t_mrec_app-product ) .
        IF lv_combine_field IS INITIAL.
          CONCATENATE gs_zpra_t_mrec_app-product(gv_len) '-' gs_zpra_t_mrec_app-asset INTO lv_col_name .
        ELSE.
          CONCATENATE gs_zpra_t_mrec_app-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
        ENDIF.
        ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF <gfs_field> IS ASSIGNED.
          <gfs_field> = <gfs_field> + gs_zpra_t_mrec_app-app_vl_qty .
          UNASSIGN <gfs_field> .
        ENDIF.
*     Product Total..
        CONCATENATE gs_zpra_t_mrec_app-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
        ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF <gfs_field> IS ASSIGNED.
          <gfs_field> = <gfs_field> + gs_zpra_t_mrec_app-app_vl_qty .
          UNASSIGN <gfs_field> .
        ENDIF.
*     Grand Total
        CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
        ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF <gfs_field> IS ASSIGNED.
          <gfs_field> = <gfs_field> + gs_zpra_t_mrec_app-app_vl_qty.
          UNASSIGN <gfs_field> .
        ENDIF.
**        ELSE. "Now work out from MREC
**          READ TABLE gt_zpra_t_mrec_prd_2f INTO gs_zpra_t_mrec_prd WITH KEY gjahr   = lv_gjahr
**                                                                            monat   = lv_monat
**                                                                            product = gs_zpra_c_prd_prof-product
**                                                                            asset   = gs_zpra_c_prd_prof-asset
**                                                                            block   = gs_zpra_c_prd_prof-block BINARY SEARCH .
**          IF sy-subrc IS INITIAL. "if found in MREC
**
**            gs_zpra_t_dly_prd = ls_dly_prd_main .
**            PERFORM get_days_in_period USING gs_zpra_t_mrec_prd-gjahr gs_zpra_t_mrec_prd-monat CHANGING lv_days .
***            lv_days = lv_days * 12 .
**            lv_days = 1 .
**            PERFORM convert_non_gas_mrec_units USING lv_days CHANGING gs_zpra_t_mrec_prd.
**            IF gs_zpra_c_prd_prof-product = c_prod_gas.
**             READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_mrec_prd-asset BINARY SEARCH .
**             IF  sy-subrc IS INITIAL.
**               lv_combine_field = 'X' .
**             ENDIF.
**            ENDIF.
**            PERFORM get_constorium_multipliers_2a2 USING '6'.
**
***       Individual Column..
**           gv_len = strlen( gs_zpra_t_mrec_prd-product ) .
**           IF lv_combine_field IS INITIAL.
**             CONCATENATE gs_zpra_t_mrec_prd-product(gv_len) '-' gs_zpra_t_mrec_prd-asset INTO lv_col_name .
**           ELSE.
**             CONCATENATE gs_zpra_t_mrec_prd-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
**           ENDIF.
**           ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
**           IF <gfs_field> IS ASSIGNED.
**             <gfs_field> = <gfs_field> + gs_zpra_t_mrec_prd-prod_vl_qty1 * gv_individual_mul.
**             UNASSIGN <gfs_field> .
**           ENDIF.
***       Product Total..
**           CONCATENATE gs_zpra_t_mrec_prd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
**           ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
**           IF <gfs_field> IS ASSIGNED.
**             <gfs_field> = <gfs_field> + gs_zpra_t_mrec_prd-prod_vl_qty1 * gv_total_mul .
**             UNASSIGN <gfs_field> .
**           ENDIF.
***       Grand Total
**           CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
**           ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
**           IF <gfs_field> IS ASSIGNED.
**             <gfs_field> = <gfs_field> + gs_zpra_t_mrec_prd-prod_vl_qty1 * gv_grand_total_mul.
**             UNASSIGN <gfs_field> .
**           ENDIF.
      ELSE.
        PERFORM get_days_in_period USING lv_gjahr lv_monat CHANGING lv_days .
        READ TABLE gt_zpra_t_dly_rprd_2f INTO gs_zpra_t_dly_rprd WITH KEY product = gs_zpra_c_prd_prof-product
                                                                            asset = gs_zpra_c_prd_prof-asset
                                                                            block = gs_zpra_c_prd_prof-block
                                                                  production_date = gv_search_begin_datum BINARY SEARCH .
        IF sy-subrc IS NOT INITIAL.
          LOOP AT gt_zpra_t_dly_rprd_2f INTO gs_zpra_t_dly_rprd WHERE product         EQ gs_zpra_c_prd_prof-product
                                                                  AND asset           EQ gs_zpra_c_prd_prof-asset
                                                                  AND block           EQ gs_zpra_c_prd_prof-block
                                                                  AND production_date GE gv_search_begin_datum
                                                                  AND production_date LE gv_search_end_datum .
            EXIT .
          ENDLOOP .
        ENDIF.
        IF sy-subrc IS INITIAL.
          lv_rprd_index = sy-tabix .
          LOOP AT gt_zpra_t_dly_rprd_2f INTO gs_zpra_t_dly_rprd FROM lv_rprd_index .
            IF gs_zpra_t_dly_rprd-product EQ gs_zpra_c_prd_prof-product      AND
               gs_zpra_t_dly_rprd-asset   EQ gs_zpra_c_prd_prof-asset        AND
               gs_zpra_t_dly_rprd-block   EQ gs_zpra_c_prd_prof-block        AND
               gs_zpra_t_dly_rprd-production_date GE gv_search_begin_datum   AND
               gs_zpra_t_dly_rprd-production_date LE gv_search_end_datum .
            ELSE.
              EXIT .
            ENDIF.
            lv_index = sy-tabix .
            CLEAR lv_combine_field .

            PERFORM convert_rprd_units USING gs_zpra_t_dly_rprd .

            IF gs_zpra_t_dly_rprd-product = c_prod_gas .
              READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_dly_rprd-asset BINARY SEARCH .
              IF  sy-subrc IS INITIAL.
                lv_combine_field = 'X' .
              ENDIF.
            ENDIF.
*           Individual Column..
            gv_len = strlen( gs_zpra_t_dly_rprd-product ) .
            IF lv_combine_field IS INITIAL.
              CONCATENATE gs_zpra_t_dly_rprd-product(gv_len) '-' gs_zpra_t_dly_rprd-asset INTO lv_col_name .
            ELSE.
              CONCATENATE gs_zpra_t_dly_rprd-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
            ENDIF.
            ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
            IF <gfs_field> IS ASSIGNED.
              <gfs_field> = <gfs_field> + gs_zpra_t_dly_rprd-jv_prd_vl_qty1.
              UNASSIGN <gfs_field> .
            ENDIF.
*           Product Total..
            CONCATENATE gs_zpra_t_dly_rprd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
            ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
            IF <gfs_field> IS ASSIGNED.
              <gfs_field> = <gfs_field> + gs_zpra_t_dly_rprd-ovl_prd_vl_qty1.
              UNASSIGN <gfs_field> .
            ENDIF.
*           Grand Total
            CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
            ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
            IF <gfs_field> IS ASSIGNED.
              lv_mmscmd_mul = 1 .
              IF gs_zpra_t_dly_rprd-product EQ c_prod_gas AND p_bmd IS NOT INITIAL.
*                  lv_mmscmd_mul = 6290 .
              ENDIF.
              <gfs_field> = <gfs_field> + ( gs_zpra_t_dly_rprd-ovl_prd_vl_qty1 * lv_mmscmd_mul ).
              UNASSIGN <gfs_field> .
            ENDIF.
          ENDLOOP.
        ELSE. "Now work out from daily production
          PERFORM get_days_in_period USING lv_gjahr lv_monat CHANGING lv_days .
*            lv_days = lv_days * 12 .
          lv_days = 1 .
          LOOP AT gt_zpra_t_dly_prd_2f INTO gs_zpra_t_dly_prd WHERE product EQ gs_zpra_c_prd_prof-product
                                                                AND asset   EQ gs_zpra_c_prd_prof-asset
                                                                AND block   EQ gs_zpra_c_prd_prof-block
                                                                AND production_date GE gv_search_begin_datum
                                                                AND production_date LE gv_search_end_datum .
            lv_index = sy-tabix .
            CLEAR lv_combine_field .

            PERFORM convert_non_gas_units2 USING lv_days CHANGING gs_zpra_t_dly_prd .

            IF gs_zpra_t_dly_prd-product = c_prod_gas.
              READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_dly_prd-asset BINARY SEARCH .
              IF  sy-subrc IS INITIAL.
                lv_combine_field = 'X' .
              ENDIF.
            ENDIF.
            PERFORM get_constorium_multipliers_2a2 USING '1'.
*           Individual Column..
            gv_len = strlen( gs_zpra_t_dly_prd-product ) .
            IF lv_combine_field IS INITIAL.
              CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' gs_zpra_t_dly_prd-asset INTO lv_col_name .
            ELSE.
              CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
            ENDIF.
            ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
            IF <gfs_field> IS ASSIGNED.
              <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_individual_mul.
              UNASSIGN <gfs_field> .
            ENDIF.
*           Product Total..
            CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
            ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
            IF <gfs_field> IS ASSIGNED.
              <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_total_mul .
              UNASSIGN <gfs_field> .
            ENDIF.
*           Grand Total
            CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
            ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
            IF <gfs_field> IS ASSIGNED.
              <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_grand_total_mul.
              UNASSIGN <gfs_field> .
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
      IF lv_monat GE 12.
        EXIT .
      ENDIF.
    ENDDO.
  ENDLOOP.
* Now dividing by no of days in case of per day units
  IF p_bbd IS NOT INITIAL OR
     p_tmd IS NOT INITIAL OR
     p_bmd IS NOT INITIAL .

    LOOP AT <gfs_sec2f_table> ASSIGNING <gfs_dyn_line> .
      DO .
        lv_index = sy-index .
        lv_days2 = gv_last_gjahr_days .
        ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF sy-subrc IS INITIAL.
          IF lv_index GT 2.
            PERFORM get_asset_start_date USING lv_index CHANGING lv_start_date .
            IF lv_start_date GT gv_last_year_s_date .
              lv_days2 = lv_days2 + gv_last_year_s_date - lv_start_date .
            ENDIF.
            <gfs_field> = <gfs_field> / lv_days2 .
          ENDIF.
        ELSE.
          EXIT .
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDIF.
  IF p_mb IS NOT INITIAL.
    LOOP AT <gfs_sec2f_table> ASSIGNING <gfs_dyn_line> .
      DO .
        lv_index = sy-index .
        ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF sy-subrc IS NOT INITIAL.
          EXIT .
        ELSE.
          IF lv_index GT 2.
            <gfs_field> = <gfs_field> / 1000000 .
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDIF.

  IF p_bmd IS NOT INITIAL.
    LOOP AT <gfs_sec2f_table> ASSIGNING <gfs_dyn_line> .
      DO .
        lv_index = sy-index .
        ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF sy-subrc IS NOT INITIAL.
          EXIT .
        ELSE.
          IF lv_index GT 2.
            READ TABLE gt_dyn_fcat INTO gs_dyn_fcat WITH KEY col_pos = lv_index BINARY SEARCH .
            IF sy-subrc IS INITIAL.
              SPLIT gs_dyn_fcat-fieldname AT '-' INTO lv_product lv_asset .
              IF lv_product EQ c_prod_gas.
                <gfs_field> = <gfs_field> / 6290 .
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDIF.


ENDFORM.
FORM fill_dynamic_table_sec2e .
  DATA   lv_index TYPE sy-index .
  DATA : lv_amt         TYPE p LENGTH 12 DECIMALS 9,
         lv_amt2        TYPE p LENGTH 12 DECIMALS 9,
         lv_amt3        TYPE p LENGTH 12 DECIMALS 9,
         lv_target_name TYPE char25,
         lv_days        TYPE sy-tabix,
         lv_start_date  TYPE sy-tabix,
         lv_tar_code    TYPE zpra_t_prd_tar-tar_code.

  REFRESH <gfs_sec2e_table> .
  READ TABLE <gfs_sec2d_table> ASSIGNING <gfs_dyn_line> INDEX 1 .
  IF  sy-subrc IS INITIAL .
    LOOP AT <gfs_sec2b_table> ASSIGNING <gfs_dyn_line2>.
      APPEND INITIAL LINE TO <gfs_sec2e_table> ASSIGNING <gfs_dyn_line3> .
      DO gv_table_columns TIMES.
        lv_index = sy-index .
        IF lv_index EQ 1.
        ELSEIF lv_index EQ 2 .
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line2> TO <gfs_field2> .
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line3> TO <gfs_field3> .
          <gfs_field3> = <gfs_field2> .
          lv_target_name = <gfs_field2> .
          PERFORM get_target_from_name USING lv_target_name CHANGING lv_tar_code.
        ELSE .
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line>  TO <gfs_field> .
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line2> TO <gfs_field2> .
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line3> TO <gfs_field3> .
          IF <gfs_field>  IS ASSIGNED AND
             <gfs_field2> IS ASSIGNED AND
             <gfs_field3> IS ASSIGNED .
            IF p_bbd IS NOT INITIAL OR
               p_tmd IS NOT INITIAL OR
               p_bmd IS NOT INITIAL .
              IF gv_days_left_in_year IS NOT INITIAL .
                IF <gfs_field2> IS INITIAL.
                  <gfs_field3> = '-' .
                ELSE.
                  lv_days = gv_year_end_date - gv_year_start_date + 1 .
                  PERFORM get_target_start_date USING lv_index lv_tar_code CHANGING lv_start_date .
                  IF lv_start_date GT gv_year_start_date .
                    lv_days =  gv_year_end_date - lv_start_date + 1 .
                  ENDIF.
                  lv_amt  = <gfs_field2> * lv_days .

                  lv_days = p_date - gv_year_start_date + 1 .
                  PERFORM get_asset_start_date USING lv_index CHANGING lv_start_date .
                  IF lv_start_date GT gv_year_start_date .
                    lv_days =  p_date - lv_start_date + 1 .
                  ENDIF.
                  lv_amt2 = <gfs_field> * lv_days .

                  lv_amt3 = ( lv_amt - lv_amt2 ) / gv_days_left_in_year .
                  <gfs_field3> = lv_amt3 .
                  IF lv_amt3 LT 0.
                    <gfs_field3> = <gfs_field3> * -1 .
                    SHIFT <gfs_field3> LEFT DELETING LEADING space .
                    CONCATENATE '-' <gfs_field3> INTO <gfs_field3> .
                  ENDIF.
                ENDIF.
              ELSE.
                <gfs_field3> = '-' .
              ENDIF.
            ELSE.
              <gfs_field3> = '-' .
            ENDIF.
          ENDIF.
          UNASSIGN : <gfs_field> , <gfs_field2> , <gfs_field3>.
        ENDIF.
      ENDDO.
    ENDLOOP.

  ENDIF.
ENDFORM.

FORM fill_dynamic_table_sec3c .


  DATA : lt_zpra_t_dly_prd     TYPE STANDARD TABLE OF zpra_t_dly_prd .
  DATA : ls_zpra_t_dly_prd     TYPE                   zpra_t_dly_prd .

  DATA : lv_monat         TYPE                   zpra_t_mrec_prd-monat,
         lv_gjahr         TYPE                   zpra_t_mrec_prd-gjahr,
         lv_gjahr_next    TYPE                   zpra_t_mrec_prd-gjahr,
         lv_combine_field TYPE                   c,
         lv_days          TYPE                   sy-tabix,
         lv_index         TYPE                   sy-tabix,
         lv_rprd_index    TYPE                   sy-tabix,
         lv_col_name      TYPE                   lvc_fname,
         lv_found         TYPE                   c.
  FIELD-SYMBOLS : <lfs_dyn_line> ,
                  <lfs_field>    .
* If directly working with MMT, then due to rounding off, value mismatches..
* First doing in MMT * 1000, at the end will divide by 1000 .
  lv_gjahr = gv_current_gjahr .
  lv_gjahr_next = lv_gjahr + 1 .
  APPEND INITIAL LINE TO <gfs_sec3c_table> ASSIGNING <gfs_dyn_line> .
  ASSIGN COMPONENT 'COL01' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
  CONCATENATE 'YTD Actual Prod.:' lv_gjahr '-' lv_gjahr_next+2(2) INTO <gfs_field> SEPARATED BY space.
  LOOP AT gt_zpra_c_prd_prof INTO gs_zpra_c_prd_prof.
    lv_monat = '00' .
    DO .
      lv_monat = lv_monat + 1 .
      CLEAR lv_combine_field .
      READ TABLE gt_zpra_t_mrec_app INTO gs_zpra_t_mrec_app WITH KEY gjahr   = lv_gjahr
                                                                     monat   = lv_monat
                                                                     product = gs_zpra_c_prd_prof-product
                                                                     asset   = gs_zpra_c_prd_prof-asset
                                                                     block   = gs_zpra_c_prd_prof-block BINARY SEARCH .
      IF sy-subrc IS INITIAL. "if found in MREC APP
        gs_zpra_t_mrec_app-app_vl_qty = gs_zpra_t_mrec_app-app_vl_qty * 1000000 .
        IF gs_zpra_t_mrec_app-monat EQ gv_current_monat.
          PERFORM get_days_in_period USING gs_zpra_t_mrec_app-gjahr gs_zpra_t_mrec_app-monat CHANGING lv_days .
          gs_zpra_t_mrec_app-app_vl_qty = gs_zpra_t_mrec_app-app_vl_qty * ( p_date - gv_search_begin_datum + 1 ) / lv_days .
        ENDIF.
        IF gs_zpra_t_mrec_app-product = c_prod_gas.
          READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_mrec_app-asset BINARY SEARCH .
          IF  sy-subrc IS INITIAL.
            lv_combine_field = 'X' .
          ENDIF.
        ENDIF.
*     Individual Column..
        gv_len = strlen( gs_zpra_t_mrec_app-product ) .
        IF lv_combine_field IS INITIAL.
          CONCATENATE gs_zpra_t_mrec_app-product(gv_len) '-' gs_zpra_t_mrec_app-asset INTO lv_col_name .
        ELSE.
          CONCATENATE gs_zpra_t_mrec_app-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
        ENDIF.
        ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF <gfs_field> IS ASSIGNED.
          <gfs_field> = <gfs_field> + gs_zpra_t_mrec_app-app_vl_qty .
          UNASSIGN <gfs_field> .
        ENDIF.
*     Product Total..
        CONCATENATE gs_zpra_t_mrec_app-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
        ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF <gfs_field> IS ASSIGNED.
          <gfs_field> = <gfs_field> + gs_zpra_t_mrec_app-app_vl_qty .
          UNASSIGN <gfs_field> .
        ENDIF.
*     Grand Total
        CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
        ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF <gfs_field> IS ASSIGNED.
          <gfs_field> = <gfs_field> + gs_zpra_t_mrec_app-app_vl_qty.
          UNASSIGN <gfs_field> .
        ENDIF.
      ELSE. "Now work out from daily reconciled production
        PERFORM get_days_in_period USING lv_gjahr lv_monat CHANGING lv_days .
        IF lv_monat EQ gv_current_monat.
          gv_search_end_datum = p_date .
        ENDIF.
        CLEAR lv_found .
        READ TABLE gt_zpra_t_dly_rprd_3c INTO gs_zpra_t_dly_rprd WITH KEY product = gs_zpra_c_prd_prof-product
                                                                            asset = gs_zpra_c_prd_prof-asset
                                                                            block = gs_zpra_c_prd_prof-block
                                                                  production_date = gv_search_begin_datum BINARY SEARCH .
        IF sy-subrc IS NOT INITIAL.
          LOOP AT gt_zpra_t_dly_rprd_3c INTO gs_zpra_t_dly_rprd WHERE product         EQ gs_zpra_c_prd_prof-product
                                                                  AND asset           EQ gs_zpra_c_prd_prof-asset
                                                                  AND block           EQ gs_zpra_c_prd_prof-block
                                                                  AND production_date GE gv_search_begin_datum
                                                                  AND production_date LE gv_search_end_datum .
            EXIT .
          ENDLOOP .
        ENDIF.
        IF sy-subrc IS INITIAL.
          lv_rprd_index = sy-tabix .
          lv_found      = 'X' .
          LOOP AT gt_zpra_t_dly_rprd_3c INTO gs_zpra_t_dly_rprd FROM lv_rprd_index .
            IF gs_zpra_t_dly_rprd-product EQ gs_zpra_c_prd_prof-product      AND
               gs_zpra_t_dly_rprd-asset   EQ gs_zpra_c_prd_prof-asset        AND
               gs_zpra_t_dly_rprd-block   EQ gs_zpra_c_prd_prof-block        AND
               gs_zpra_t_dly_rprd-production_date GE gv_search_begin_datum   AND
               gs_zpra_t_dly_rprd-production_date LE gv_search_end_datum .
            ELSE.
              EXIT .
            ENDIF.
            lv_index = sy-tabix .
            CLEAR lv_combine_field .

            PERFORM convert_rprd_units_mb USING gs_zpra_t_dly_rprd .

            IF gs_zpra_t_dly_rprd-product = c_prod_gas .
              READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_dly_rprd-asset BINARY SEARCH .
              IF  sy-subrc IS INITIAL.
                lv_combine_field = 'X' .
              ENDIF.
            ENDIF.
*           Individual Column..
            gv_len = strlen( gs_zpra_t_dly_rprd-product ) .
            IF lv_combine_field IS INITIAL.
              CONCATENATE gs_zpra_t_dly_rprd-product(gv_len) '-' gs_zpra_t_dly_rprd-asset INTO lv_col_name .
            ELSE.
              CONCATENATE gs_zpra_t_dly_rprd-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
            ENDIF.
            ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
            IF <gfs_field> IS ASSIGNED.
              <gfs_field> = <gfs_field> + gs_zpra_t_dly_rprd-jv_prd_vl_qty1.
              UNASSIGN <gfs_field> .
            ENDIF.
*           Product Total..
            CONCATENATE gs_zpra_t_dly_rprd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
            ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
            IF <gfs_field> IS ASSIGNED.
              <gfs_field> = <gfs_field> + gs_zpra_t_dly_rprd-ovl_prd_vl_qty1.
              UNASSIGN <gfs_field> .
            ENDIF.
*           Grand Total
            CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
            ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
            IF <gfs_field> IS ASSIGNED.
              <gfs_field> = <gfs_field> + gs_zpra_t_dly_rprd-ovl_prd_vl_qty1.
              UNASSIGN <gfs_field> .
            ENDIF.
          ENDLOOP.
        ENDIF.
        IF lv_found IS INITIAL ."Now work out from daily production
          LOOP AT gt_zpra_t_dly_prd_3c INTO gs_zpra_t_dly_prd WHERE product EQ gs_zpra_c_prd_prof-product
                                                                AND asset   EQ gs_zpra_c_prd_prof-asset
                                                                AND block   EQ gs_zpra_c_prd_prof-block
                                                                AND production_date GE gv_search_begin_datum
                                                                AND production_date LE gv_search_end_datum .
            lv_index = sy-tabix .
            CLEAR lv_combine_field .

            PERFORM convert_non_gas_units3c USING lv_monat CHANGING gs_zpra_t_dly_prd .

            IF gs_zpra_t_dly_prd-product = c_prod_gas .
              READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_dly_prd-asset BINARY SEARCH .
              IF  sy-subrc IS INITIAL.
                lv_combine_field = 'X' .
              ENDIF.
            ENDIF.
            PERFORM get_constorium_multipliers_2a2 USING '7'.
            gv_individual_mul  = gv_total_mul . "section 3 is always OVL Level
            gv_grand_total_mul = gv_total_mul . "section 3 is always OVL Level
*           Individual Column..
            gv_len = strlen( gs_zpra_t_dly_prd-product ) .
            IF lv_combine_field IS INITIAL.
              CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' gs_zpra_t_dly_prd-asset INTO lv_col_name .
            ELSE.
              CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
            ENDIF.
            ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
            IF <gfs_field> IS ASSIGNED.
              <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_individual_mul.
              UNASSIGN <gfs_field> .
            ENDIF.
*           Product Total..
            CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
            ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
            IF <gfs_field> IS ASSIGNED.
              <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_total_mul .
              UNASSIGN <gfs_field> .
            ENDIF.
*           Grand Total
            CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
            ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
            IF <gfs_field> IS ASSIGNED.
              <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_total_mul.
              UNASSIGN <gfs_field> .
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
      IF lv_monat GE gv_current_monat.
        EXIT .
      ENDIF.
    ENDDO.
  ENDLOOP.

  LOOP AT <gfs_sec3c_table> ASSIGNING <gfs_dyn_line> .
    DO .
      lv_index = sy-index .
      ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      IF sy-subrc IS NOT INITIAL.
        EXIT .
      ENDIF.
      IF lv_index GT 2.
        <gfs_field> = <gfs_field> / 1000000 .
      ENDIF.
    ENDDO.
  ENDLOOP.
  IF p_mb IS NOT INITIAL AND p_c_jv IS NOT INITIAL.
    LOOP AT gt_product_col INTO gs_product_col.
      CONCATENATE gs_product_col-product 'TOTAL' INTO lv_col_name SEPARATED BY '-' .
      UNASSIGN <gfs_field>.
      ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      IF <gfs_field> IS ASSIGNED.
        gv_s_row = gv_sec2d_start_row .
        gv_e_row = gv_sec2d_start_row .
        gv_s_col = gs_product_col-e_col .
        gv_e_col = gs_product_col-e_col .
        PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
        PERFORM set_range USING <gfs_field> 0 .
      ENDIF.
      READ TABLE <gfs_sec2d_table> ASSIGNING <lfs_dyn_line> INDEX 1 .
      IF sy-subrc IS INITIAL.
        ASSIGN COMPONENT lv_col_name OF STRUCTURE <lfs_dyn_line> TO <lfs_field> .
        IF <lfs_field> IS ASSIGNED.
          <lfs_field> = <gfs_field>.
        ENDIF.
      ENDIF.
    ENDLOOP.
    lv_col_name = 'GRAND-TOTAL' .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      gv_s_row = gv_sec2d_start_row .
      gv_e_row = gv_sec2d_start_row .
      gv_s_col = gv_table_columns   .
      gv_e_col = gv_table_columns   .
      PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
      PERFORM set_range USING <gfs_field> 0 .
      READ TABLE <gfs_sec2d_table> ASSIGNING <lfs_dyn_line> INDEX 1 .
      IF sy-subrc IS INITIAL.
        ASSIGN COMPONENT lv_col_name OF STRUCTURE <lfs_dyn_line> TO <lfs_field> .
        IF <lfs_field> IS ASSIGNED.
          <lfs_field> = <gfs_field>.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
FORM fill_dynamic_table_sec3f .

  DATA : lt_zpra_t_dly_prd     TYPE STANDARD TABLE OF zpra_t_dly_prd .
  DATA : ls_zpra_t_dly_prd     TYPE                   zpra_t_dly_prd .

  DATA : lv_monat         TYPE                   zpra_t_mrec_prd-monat,
         lv_gjahr         TYPE                   zpra_t_mrec_prd-gjahr,
         lv_gjahr_next    TYPE                   zpra_t_mrec_prd-gjahr,
         lv_combine_field TYPE                   c,
         lv_days          TYPE                   sy-tabix,
         lv_index         TYPE                   sy-tabix,
         lv_rprd_index    TYPE                   sy-tabix,
         lv_col_name      TYPE                   lvc_fname,
         lv_found         TYPE                   c.
  FIELD-SYMBOLS : <lfs_dyn_line> ,
                  <lfs_field>    .

  lv_gjahr = gv_5_back_gjahr .
  DO 5 TIMES .
    IF sy-index EQ 2.
      PERFORM show_progress USING '60' .
    ENDIF.
    IF sy-index EQ 3.
      PERFORM show_progress USING '70' .
    ENDIF.
    IF sy-index EQ 5.
      PERFORM show_progress USING '80' .
    ENDIF.
    lv_gjahr_next = lv_gjahr + 1 .

    APPEND INITIAL LINE TO <gfs_sec3f_table> ASSIGNING <gfs_dyn_line> .
    ASSIGN COMPONENT 'COL02' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    CONCATENATE lv_gjahr '-' lv_gjahr_next+2(2) INTO <gfs_field> SEPARATED BY space.
    LOOP AT gt_zpra_c_prd_prof INTO gs_zpra_c_prd_prof.
      lv_monat = '00' .
      DO .
        lv_monat = lv_monat + 1 .
        CLEAR lv_combine_field .
        READ TABLE gt_zpra_t_mrec_app_3f INTO gs_zpra_t_mrec_app WITH KEY gjahr   = lv_gjahr
                                                                          monat   = lv_monat
                                                                          asset   = gs_zpra_c_prd_prof-asset
                                                                          block   = gs_zpra_c_prd_prof-block
                                                                          product = gs_zpra_c_prd_prof-product BINARY SEARCH .
        IF sy-subrc IS INITIAL. "if found in MREC APP
          gs_zpra_t_mrec_app-app_vl_qty = gs_zpra_t_mrec_app-app_vl_qty * 1000000 .
          IF gs_zpra_t_mrec_app-product = c_prod_gas.
            READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_mrec_app-asset BINARY SEARCH .
            IF  sy-subrc IS INITIAL.
              lv_combine_field = 'X' .
            ENDIF.
          ENDIF.
*     Individual Column..
          gv_len = strlen( gs_zpra_t_mrec_app-product ) .
          IF lv_combine_field IS INITIAL.
            CONCATENATE gs_zpra_t_mrec_app-product(gv_len) '-' gs_zpra_t_mrec_app-asset INTO lv_col_name .
          ELSE.
            CONCATENATE gs_zpra_t_mrec_app-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
          ENDIF.
          ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF <gfs_field> IS ASSIGNED.
            <gfs_field> = <gfs_field> + gs_zpra_t_mrec_app-app_vl_qty .
            UNASSIGN <gfs_field> .
          ENDIF.
*     Product Total..
          CONCATENATE gs_zpra_t_mrec_app-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
          ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF <gfs_field> IS ASSIGNED.
            <gfs_field> = <gfs_field> + gs_zpra_t_mrec_app-app_vl_qty .
            UNASSIGN <gfs_field> .
          ENDIF.
*     Grand Total
          CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
          ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF <gfs_field> IS ASSIGNED.
            <gfs_field> = <gfs_field> + gs_zpra_t_mrec_app-app_vl_qty.
            UNASSIGN <gfs_field> .
          ENDIF.
        ELSE. "Now work out from daily reconciled production
          PERFORM get_days_in_period USING lv_gjahr lv_monat CHANGING lv_days .
          CLEAR lv_found .
          READ TABLE gt_zpra_t_dly_rprd_3f INTO gs_zpra_t_dly_rprd WITH KEY product = gs_zpra_c_prd_prof-product
                                                                              asset = gs_zpra_c_prd_prof-asset
                                                                              block = gs_zpra_c_prd_prof-block
                                                                    production_date = gv_search_begin_datum BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            LOOP AT gt_zpra_t_dly_rprd_3f INTO gs_zpra_t_dly_rprd WHERE product EQ gs_zpra_c_prd_prof-product
                                                                  AND   asset   EQ gs_zpra_c_prd_prof-asset
                                                                  AND   block   EQ gs_zpra_c_prd_prof-block
                                                                  AND production_date GE gv_search_begin_datum
                                                                  AND production_date LE gv_search_end_datum .
              EXIT.
            ENDLOOP.
          ENDIF.
          IF sy-subrc IS INITIAL.
            lv_rprd_index = sy-tabix .
            lv_found      = 'X' .
            LOOP AT gt_zpra_t_dly_rprd_3f INTO gs_zpra_t_dly_rprd FROM lv_rprd_index .
              IF gs_zpra_t_dly_rprd-product EQ gs_zpra_c_prd_prof-product      AND
                 gs_zpra_t_dly_rprd-asset   EQ gs_zpra_c_prd_prof-asset        AND
                 gs_zpra_t_dly_rprd-block   EQ gs_zpra_c_prd_prof-block        AND
                 gs_zpra_t_dly_rprd-production_date GE gv_search_begin_datum   AND
                 gs_zpra_t_dly_rprd-production_date LE gv_search_end_datum .
              ELSE.
                EXIT .
              ENDIF.
              lv_index = sy-tabix .
              CLEAR lv_combine_field .

              PERFORM convert_rprd_units_mb USING gs_zpra_t_dly_rprd .

              " For Sakhalin-1 oil, dly_rprd ovl_prd_vl_qty1 actually stores
              " JV production (data inconsistency). Apply PI to get OVL share.
              IF ( gs_zpra_t_dly_rprd-asset EQ 'RUS_SK1' OR
                   gs_zpra_t_dly_rprd-asset CS 'SK' OR
                   gs_zpra_t_dly_rprd-asset CS 'SAKH' ) AND
                 gs_zpra_t_dly_rprd-product NE c_prod_gas .
                CLEAR gs_zpra_t_prd_pi .
                LOOP AT gt_zpra_t_prd_pi_3f INTO gs_zpra_t_prd_pi
                  WHERE asset   EQ gs_zpra_t_dly_rprd-asset
                    AND block   EQ gs_zpra_t_dly_rprd-block
                    AND vld_frm LE gs_zpra_t_dly_rprd-production_date
                    AND vld_to  GE gs_zpra_t_dly_rprd-production_date .
                  EXIT .
                ENDLOOP .
                IF sy-subrc IS NOT INITIAL .
                  LOOP AT gt_zpra_t_prd_pi_3f INTO gs_zpra_t_prd_pi
                    WHERE asset EQ gs_zpra_t_dly_rprd-asset .
                    EXIT .
                  ENDLOOP .
                ENDIF .
                IF gs_zpra_t_prd_pi-pi IS NOT INITIAL .
                  gs_zpra_t_dly_rprd-ovl_prd_vl_qty1 = gs_zpra_t_dly_rprd-ovl_prd_vl_qty1 * gs_zpra_t_prd_pi-pi / 100 .
                ENDIF .
              ENDIF .

              IF gs_zpra_t_dly_rprd-product = c_prod_gas .
                READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_dly_rprd-asset BINARY SEARCH .
                IF  sy-subrc IS INITIAL.
                  lv_combine_field = 'X' .
                ENDIF.
              ENDIF.
*           Individual Column..
              gv_len = strlen( gs_zpra_t_dly_rprd-product ) .
              IF lv_combine_field IS INITIAL.
                CONCATENATE gs_zpra_t_dly_rprd-product(gv_len) '-' gs_zpra_t_dly_rprd-asset INTO lv_col_name .
              ELSE.
                CONCATENATE gs_zpra_t_dly_rprd-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
              ENDIF.
              ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
              IF <gfs_field> IS ASSIGNED.
                <gfs_field> = <gfs_field> + gs_zpra_t_dly_rprd-ovl_prd_vl_qty1.
                UNASSIGN <gfs_field> .
              ENDIF.
*           Product Total..
              CONCATENATE gs_zpra_t_dly_rprd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
              ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
              IF <gfs_field> IS ASSIGNED.
                <gfs_field> = <gfs_field> + gs_zpra_t_dly_rprd-ovl_prd_vl_qty1.
                UNASSIGN <gfs_field> .
              ENDIF.
*           Grand Total
              CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
              ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
              IF <gfs_field> IS ASSIGNED.
                <gfs_field> = <gfs_field> + gs_zpra_t_dly_rprd-ovl_prd_vl_qty1.
                UNASSIGN <gfs_field> .
              ENDIF.
            ENDLOOP.
          ENDIF.
          IF lv_found IS INITIAL ."Now work out from daily production
            LOOP AT gt_zpra_t_dly_prd_3f INTO gs_zpra_t_dly_prd WHERE product EQ gs_zpra_c_prd_prof-product
                                                                  AND asset   EQ gs_zpra_c_prd_prof-asset
                                                                  AND block   EQ gs_zpra_c_prd_prof-block
                                                                  AND production_date GE gv_search_begin_datum
                                                                  AND production_date LE gv_search_end_datum .
              lv_index = sy-tabix .
              CLEAR lv_combine_field .

              PERFORM convert_non_gas_units3f USING lv_days CHANGING gs_zpra_t_dly_prd .

              " Gas in zpra_t_dly_prd stores JV production; oil already stores OVL share.
              " Apply PI to gas; also apply PI to SK non-gas (data inconsistency: SK
              " historical oil in dly_prd also stores JV after CF conversion).
              IF gs_zpra_t_dly_prd-product EQ c_prod_gas OR
                 gs_zpra_t_dly_prd-asset CS 'SK' OR
                 gs_zpra_t_dly_prd-asset CS 'SAKH'.
                CLEAR gs_zpra_t_prd_pi .
                LOOP AT gt_zpra_t_prd_pi_3f INTO gs_zpra_t_prd_pi
                  WHERE asset   EQ gs_zpra_t_dly_prd-asset
                    AND block   EQ gs_zpra_t_dly_prd-block
                    AND vld_frm LE gs_zpra_t_dly_prd-production_date
                    AND vld_to  GE gs_zpra_t_dly_prd-production_date .
                  EXIT .
                ENDLOOP .
                IF sy-subrc IS NOT INITIAL .
                  LOOP AT gt_zpra_t_prd_pi_3f INTO gs_zpra_t_prd_pi
                    WHERE asset EQ gs_zpra_t_dly_prd-asset .
                    EXIT .
                  ENDLOOP .
                ENDIF .
                IF gs_zpra_t_prd_pi-pi IS NOT INITIAL .
                  gs_zpra_t_dly_prd-prod_vl_qty1 = gs_zpra_t_dly_prd-prod_vl_qty1 * gs_zpra_t_prd_pi-pi / 100 .
                ENDIF .
              ENDIF.

              IF gs_zpra_t_dly_prd-product EQ c_prod_gas.
                READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_dly_prd-asset BINARY SEARCH .
                IF  sy-subrc IS INITIAL.
                  lv_combine_field = 'X' .
                ENDIF.
              ENDIF.
*           Individual Column..
              gv_len = strlen( gs_zpra_t_dly_prd-product ) .
              IF lv_combine_field IS INITIAL.
                CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' gs_zpra_t_dly_prd-asset INTO lv_col_name .
              ELSE.
                CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
              ENDIF.
              ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
              IF <gfs_field> IS ASSIGNED.
                <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1.
                UNASSIGN <gfs_field> .
              ENDIF.
*           Product Total..
              CONCATENATE gs_zpra_t_dly_prd-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
              ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
              IF <gfs_field> IS ASSIGNED.
                <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1.
                UNASSIGN <gfs_field> .
              ENDIF.
*           Grand Total
              CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
              ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
              IF <gfs_field> IS ASSIGNED.
                <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1.
                UNASSIGN <gfs_field> .
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.
        IF lv_monat GE 12.
          EXIT .
        ENDIF.
      ENDDO.
    ENDLOOP.
    lv_gjahr = lv_gjahr + 1 .
  ENDDO .

  LOOP AT <gfs_sec3f_table> ASSIGNING <gfs_dyn_line> .
    DO .
      lv_index = sy-index .
      ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      IF sy-subrc IS NOT INITIAL.
        EXIT .
      ENDIF.
      IF lv_index GT 2.
        <gfs_field> = <gfs_field> / 1000000 .
      ENDIF.
    ENDDO.
  ENDLOOP.
  IF p_mb IS NOT INITIAL AND p_c_jv IS NOT INITIAL.
    READ TABLE <gfs_sec3f_table> ASSIGNING <gfs_dyn_line> INDEX 5 .
    IF sy-subrc IS INITIAL.
      LOOP AT gt_product_col INTO gs_product_col.
        CONCATENATE gs_product_col-product 'TOTAL' INTO lv_col_name SEPARATED BY '-' .
        UNASSIGN <gfs_field>.
        ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF <gfs_field> IS ASSIGNED.
          gv_s_row = gv_sec2f_start_row .
          gv_e_row = gv_sec2f_start_row .
          gv_s_col = gs_product_col-e_col .
          gv_e_col = gs_product_col-e_col .
          PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
          PERFORM set_range USING <gfs_field> 0 .
        ENDIF.
        READ TABLE <gfs_sec2f_table> ASSIGNING <lfs_dyn_line> INDEX 1 .
        IF sy-subrc IS INITIAL.
          ASSIGN COMPONENT lv_col_name OF STRUCTURE <lfs_dyn_line> TO <lfs_field> .
          IF <lfs_field> IS ASSIGNED.
            <lfs_field> = <gfs_field>.
          ENDIF.
        ENDIF.
      ENDLOOP.
      lv_col_name = 'GRAND-TOTAL' .
      ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      IF <gfs_field> IS ASSIGNED.
        gv_s_row = gv_sec2f_start_row .
        gv_e_row = gv_sec2f_start_row .
        gv_s_col = gv_table_columns   .
        gv_e_col = gv_table_columns   .
        PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
        PERFORM set_range USING <gfs_field> 0 .
        READ TABLE <gfs_sec2f_table> ASSIGNING <lfs_dyn_line> INDEX 1 .
        IF sy-subrc IS INITIAL.
          ASSIGN COMPONENT lv_col_name OF STRUCTURE <lfs_dyn_line> TO <lfs_field> .
          IF <lfs_field> IS ASSIGNED.
            <lfs_field> = <gfs_field>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
FORM fill_dynamic_table_sec4a .

  DATA : lt_zpra_t_dly_prd     TYPE STANDARD TABLE OF zpra_t_dly_prd .
  DATA : ls_zpra_t_dly_prd     TYPE                   zpra_t_dly_prd .

  DATA : lv_monat      TYPE                   zpra_t_mrec_prd-monat,
         lv_gjahr      TYPE                   zpra_t_mrec_prd-gjahr,
         lv_gjahr_next TYPE                   zpra_t_mrec_prd-gjahr,
         lv_last_asset TYPE                   zpra_t_mrec_prd-asset,
         lv_days       TYPE                   sy-tabix,
         lv_index      TYPE                   sy-tabix,
         lv_col_name   TYPE                   lvc_fname,
         lv_found      TYPE                   c.

  lv_gjahr = gv_current_gjahr .
  LOOP AT gt_zpra_c_prd_prof INTO gs_zpra_c_prd_prof WHERE product EQ '722000004' .
    READ TABLE gt_zdpr_gas_combine TRANSPORTING NO FIELDS WITH KEY asset = gs_zpra_c_prd_prof-asset BINARY SEARCH .
    IF sy-subrc IS NOT INITIAL.
      CONTINUE .
    ENDIF.
    IF lv_last_asset IS INITIAL OR
       lv_last_asset NE gs_zpra_c_prd_prof-asset .
      APPEND INITIAL LINE TO <gfs_sec4a_table> ASSIGNING <gfs_dyn_line> .
      ASSIGN COMPONENT 'COL01' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .

      READ TABLE gt_asset_desc INTO gs_asset_desc WITH KEY asset = gs_zpra_c_prd_prof-asset BINARY SEARCH .
      IF sy-subrc IS INITIAL.
        <gfs_field> = gs_asset_desc-desc .
      ENDIF.
    ENDIF.
    lv_monat = '00' .
    DO .
      lv_monat = lv_monat + 1 .
      READ TABLE gt_zpra_t_mrec_app_4a INTO gs_zpra_t_mrec_app WITH KEY gjahr   = lv_gjahr
                                                                        monat   = lv_monat
                                                                        product = gs_zpra_c_prd_prof-product
                                                                        asset   = gs_zpra_c_prd_prof-asset
                                                                        block   = gs_zpra_c_prd_prof-block BINARY SEARCH .
      IF sy-subrc IS INITIAL. "if found in MREC APP
        ASSIGN COMPONENT 2 OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
        IF <gfs_field> IS ASSIGNED.
          <gfs_field> = <gfs_field> + gs_zpra_t_mrec_app-app_vl_qty .
          UNASSIGN <gfs_field> .
        ENDIF.
      ELSE. "Now work out from RPRD
**       READ TABLE gt_zpra_t_mrec_prd_4a INTO gs_zpra_t_mrec_prd WITH KEY gjahr   = lv_gjahr
**                                                                         monat   = lv_monat
**                                                                         product = gs_zpra_c_prd_prof-product
**                                                                         asset   = gs_zpra_c_prd_prof-asset
**                                                                         block   = gs_zpra_c_prd_prof-block BINARY SEARCH .
**       IF sy-subrc IS INITIAL. "if found in MREC
**         ASSIGN COMPONENT 2 OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
**         IF <gfs_field> IS ASSIGNED.
**           <gfs_field> = <gfs_field> + gs_zpra_t_mrec_prd-prod_vl_qty1 .
**           UNASSIGN <gfs_field> .
**         ENDIF.
        PERFORM get_days_in_period USING lv_gjahr lv_monat CHANGING lv_days .
        IF lv_monat EQ gv_current_monat.
          gv_search_end_datum = p_date .
          lv_days = gv_search_end_datum - gv_search_begin_datum + 1 .
        ENDIF.
        CLEAR lv_found .
        LOOP AT gt_zpra_t_dly_rprd_4a INTO gs_zpra_t_dly_rprd WHERE product EQ gs_zpra_c_prd_prof-product
                                                                AND asset   EQ gs_zpra_c_prd_prof-asset
                                                                AND block   EQ gs_zpra_c_prd_prof-block
                                                                AND production_date GE gv_search_begin_datum
                                                                AND production_date LE gv_search_end_datum .
          lv_found = 'X' .
          lv_index = sy-tabix .
*            PERFORM convert_gas_rprd_to_boe CHANGING gs_zpra_t_dly_rprd.
          gv_individual_mul = gv_total_mul . "section 4 is always OVL Level
          ASSIGN COMPONENT 2 OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
          IF <gfs_field> IS ASSIGNED.
            IF p_c_jv IS NOT INITIAL .
              <gfs_field> = <gfs_field> + gs_zpra_t_dly_rprd-jv_rcn_vl_qty3 * 1000.
            ELSE.
              <gfs_field> = <gfs_field> + gs_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 1000.
            ENDIF.
            UNASSIGN <gfs_field> .
          ENDIF.
        ENDLOOP.
        IF lv_found IS INITIAL.
          PERFORM get_days_in_period USING lv_gjahr lv_monat CHANGING lv_days .
          IF lv_monat EQ gv_current_monat.
            gv_search_end_datum = p_date .
            lv_days = gv_search_end_datum - gv_search_begin_datum + 1 .
          ENDIF.
          LOOP AT gt_zpra_t_dly_prd_4a INTO gs_zpra_t_dly_prd WHERE product EQ gs_zpra_c_prd_prof-product
                                                                AND asset   EQ gs_zpra_c_prd_prof-asset
                                                                AND block   EQ gs_zpra_c_prd_prof-block
                                                                AND production_date GE gv_search_begin_datum
                                                                AND production_date LE gv_search_end_datum .
            lv_index = sy-tabix .
            PERFORM get_constorium_multipliers_2a2 USING '9'.
*            gv_individual_mul = gv_total_mul . "section 4 is always OVL Level
            ASSIGN COMPONENT 2 OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
            IF <gfs_field> IS ASSIGNED.
              <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 * gv_individual_mul.
              UNASSIGN <gfs_field> .
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
      IF lv_monat GE gv_current_monat.
        EXIT .
      ENDIF.
    ENDDO.
    lv_last_asset = gs_zpra_c_prd_prof-asset .
  ENDLOOP.

  LOOP AT <gfs_sec4a_table> ASSIGNING <gfs_dyn_line>.
    ASSIGN COMPONENT 2 OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> / 1000 .
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM fill_dynamic_table_sec3d .
  DATA lv_index TYPE sy-index .
  DATA lv_amt TYPE p LENGTH 12 DECIMALS 9 .
  REFRESH <gfs_sec3d_table> .
  READ TABLE <gfs_sec3c_table> ASSIGNING <gfs_dyn_line> INDEX 1 .
  IF  sy-subrc IS INITIAL .
    LOOP AT <gfs_sec3b_table> ASSIGNING <gfs_dyn_line2>.
      APPEND INITIAL LINE TO <gfs_sec3d_table> ASSIGNING <gfs_dyn_line3> .
      DO gv_table_columns TIMES.
        lv_index = sy-index .
        IF lv_index EQ 1.
        ELSEIF lv_index EQ 2 .
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line2> TO <gfs_field2> .
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line3> TO <gfs_field3> .
          <gfs_field3> = <gfs_field2> .
        ELSE .
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line>  TO <gfs_field> .
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line2> TO <gfs_field2> .
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line3> TO <gfs_field3> .
          IF <gfs_field>  IS ASSIGNED AND
             <gfs_field2> IS ASSIGNED AND
             <gfs_field3> IS ASSIGNED .
            IF <gfs_field2> IS NOT INITIAL .
              SHIFT <gfs_field2> LEFT DELETING LEADING space .
              IF <gfs_field2> NE '0'.
                <gfs_field3>  =  <gfs_field>  / <gfs_field2> * 100.
              ENDIF.
              lv_amt = <gfs_field3> .
              IF lv_amt LT 0.
                <gfs_field3> = <gfs_field3> * -1 .
                SHIFT <gfs_field3> LEFT DELETING LEADING space .
                CONCATENATE '-' <gfs_field3> INTO <gfs_field3> .
              ENDIF.
            ENDIF.
          ENDIF.
          UNASSIGN : <gfs_field> , <gfs_field2> , <gfs_field3>.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDIF.
ENDFORM.
FORM fill_dynamic_table_sec3e .
  DATA lv_index TYPE sy-index .
  DATA lv_amt TYPE p LENGTH 12 DECIMALS 9 .

  REFRESH <gfs_sec3e_table> .
  READ TABLE <gfs_sec3c_table> ASSIGNING <gfs_dyn_line> INDEX 1 .
  IF  sy-subrc IS INITIAL .
    LOOP AT <gfs_sec3a_table> ASSIGNING <gfs_dyn_line2>.
      APPEND INITIAL LINE TO <gfs_sec3e_table> ASSIGNING <gfs_dyn_line3> .
      DO gv_table_columns TIMES.
        lv_index = sy-index .
        IF lv_index EQ 1.
        ELSEIF lv_index EQ 2 .
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line2> TO <gfs_field2> .
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line3> TO <gfs_field3> .
          <gfs_field3> = <gfs_field2> .
        ELSE .
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line>  TO <gfs_field> .
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line2> TO <gfs_field2> .
          ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line3> TO <gfs_field3> .
          IF <gfs_field>  IS ASSIGNED AND
             <gfs_field2> IS ASSIGNED AND
             <gfs_field3> IS ASSIGNED .
            IF <gfs_field2> IS NOT INITIAL .
              SHIFT <gfs_field2> LEFT DELETING LEADING space .
              IF <gfs_field2> NE '0'.
                <gfs_field3>  = <gfs_field>  / <gfs_field2> * 100.
              ENDIF.
              lv_amt = <gfs_field3>.
              IF lv_amt LT 0.
                <gfs_field3> = <gfs_field3> * -1 .
                SHIFT <gfs_field3> LEFT DELETING LEADING space .
                CONCATENATE '-' <gfs_field3> INTO <gfs_field3> .
              ENDIF.
            ENDIF.
          ENDIF.
          UNASSIGN : <gfs_field> , <gfs_field2> , <gfs_field3>.
        ENDIF.
      ENDDO.
    ENDLOOP.
  ENDIF.
ENDFORM.
FORM get_days_in_period  USING    p_gjahr
                                  p_monat
                         CHANGING p_days.

  DATA : lv_poper TYPE t009b-poper .
  lv_poper = p_monat .

  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr        = p_gjahr
*     I_MONMIT       = 00
      i_periv        = gv_periv
      i_poper        = lv_poper
    IMPORTING
      e_date         = gv_search_begin_datum
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
    MESSAGE 'Internal Error' TYPE 'E' .
  ENDIF.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      i_gjahr        = p_gjahr
*     I_MONMIT       = 00
      i_periv        = gv_periv
      i_poper        = lv_poper
    IMPORTING
      e_date         = gv_search_end_datum
    EXCEPTIONS
      input_false    = 1
      t009_notfound  = 2
      t009b_notfound = 3
      OTHERS         = 4.
  IF sy-subrc <> 0.
    MESSAGE 'Internal Error' TYPE 'E' .
  ENDIF.

  p_days = gv_search_end_datum - gv_search_begin_datum + 1 .

ENDFORM.
FORM display_section3_header .
  PERFORM display_section3_header_1 .
  PERFORM display_section3_header_2 .
ENDFORM.
FORM display_section3_header_1 .

  gv_row = gv_row + 1 .
  gv_sec3_h_start_row = gv_row .

  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row .
  gv_e_col = 2 .

  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .

  CALL METHOD OF go_range 'Merge' .
  PERFORM set_range USING 'Consortium Level' 0.

  gv_s_row = gv_row .
  gv_s_col = 3 .
  gv_e_row = gv_s_row .
  gv_e_col = gv_table_columns .

  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .

  CALL METHOD OF go_range 'Merge' .
  PERFORM set_range USING 'ONGC Videsh' 0.

  PERFORM select_range USING gv_s_row 1 gv_e_row gv_e_col  .

  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = gv_sec3h_colour .

  GET PROPERTY OF go_range 'FONT' = go_font .
  SET PROPERTY OF go_font  'BOLD' = 1 .

ENDFORM.
FORM display_section3_header_2 .
  DATA gv_txt TYPE char50 .

  gv_row = gv_row + 1 .
  gv_sec3_h_start_row = gv_row .

  gv_s_row = gv_row .
  gv_s_col = 1 .
  gv_e_row = gv_s_row .
  gv_e_col = 2 .

  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .

  CALL METHOD OF go_range 'Merge' .
  PERFORM set_range USING 'Unit' 0.

  LOOP AT gt_product_col INTO gs_product_col.
    gv_s_col = gs_product_col-s_col .
    gv_e_col = gs_product_col-e_col .
    PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
    CALL METHOD OF go_range 'Merge' .
    IF gs_product_col-product EQ c_prod_gas.
      gv_txt = 'BCM' .
    ELSE.
      gv_txt = 'MMT' .
    ENDIF.
    PERFORM set_range USING gv_txt 0.
  ENDLOOP.

  gv_s_col = gv_table_columns .
  gv_e_col = gv_table_columns .

  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM set_range USING 'MMTOE' 0.

  PERFORM select_range USING gv_sec3_h_start_row 1 gv_e_row gv_e_col  .

  GET PROPERTY OF go_range 'interior' = go_interior .
  SET PROPERTY OF go_interior 'Color' = gv_sec3h_colour .

  GET PROPERTY OF go_range 'FONT' = go_font .
  SET PROPERTY OF go_font  'BOLD' = 1 .

ENDFORM.

FORM fill_dynamic_table_sec3a .
  DATA : lv_col_name      TYPE lvc_fname,
         lv_combine_field TYPE c,
         lv_index         TYPE sy-tabix,
         lv_target_name   TYPE char25.
  SORT gt_zpra_t_prd_tar_3a BY tar_code .
  LOOP AT gt_zpra_t_prd_tar_3a INTO gs_zpra_t_prd_tar .
    CLEAR lv_combine_field .
    AT NEW tar_code .
      APPEND INITIAL LINE TO <gfs_sec3a_table> ASSIGNING <gfs_dyn_line> .
      ASSIGN COMPONENT 'COL02' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      PERFORM get_target_type_name USING gs_zpra_t_prd_tar-tar_code CHANGING lv_target_name.
      <gfs_field> = lv_target_name.
    ENDAT .
*    PERFORM convert_target_units CHANGING gs_zpra_t_prd_tar.

    IF gs_zpra_t_prd_tar-product = '722000004'.
      READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_prd_tar-asset BINARY SEARCH .
      IF  sy-subrc IS INITIAL.
        lv_combine_field = 'X' .
      ENDIF.
    ENDIF.
* Individual Column..
    gv_len = strlen( gs_zpra_t_prd_tar-product ) .
    IF lv_combine_field IS INITIAL.
      CONCATENATE gs_zpra_t_prd_tar-product(gv_len) '-' gs_zpra_t_prd_tar-asset INTO lv_col_name .
    ELSE.
      CONCATENATE gs_zpra_t_prd_tar-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
    ENDIF.
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_prd_tar-tar_qty .
      UNASSIGN <gfs_field> .
    ENDIF.
* Product Total..
    CONCATENATE gs_zpra_t_prd_tar-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_prd_tar-tar_qty .
      UNASSIGN <gfs_field> .
    ENDIF.
* Grand Total
    CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_prd_tar-tar_qty .
      UNASSIGN <gfs_field> .
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM fill_dynamic_table_sec3b .
  DATA : lv_col_name      TYPE lvc_fname,
         lv_combine_field TYPE c,
         lv_index         TYPE sy-tabix,
         lv_target_name   TYPE char25.
  SORT gt_zpra_t_prd_tar_3b BY tar_code .
  LOOP AT gt_zpra_t_prd_tar_3b INTO gs_zpra_t_prd_tar .
    CLEAR lv_combine_field .
    AT NEW tar_code .
      APPEND INITIAL LINE TO <gfs_sec3b_table> ASSIGNING <gfs_dyn_line> .
      ASSIGN COMPONENT 'COL02' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      PERFORM get_target_type_name USING gs_zpra_t_prd_tar-tar_code CHANGING lv_target_name.
      <gfs_field> = lv_target_name.
    ENDAT .
*    PERFORM convert_target_units CHANGING gs_zpra_t_prd_tar.

    IF gs_zpra_t_prd_tar-product = '722000004'.
      READ TABLE gt_zdpr_gas_combine INTO gs_zdpr_gas_combine WITH KEY asset = gs_zpra_t_prd_tar-asset BINARY SEARCH .
      IF  sy-subrc IS INITIAL.
        lv_combine_field = 'X' .
      ENDIF.
    ENDIF.
* Individual Column..
    gv_len = strlen( gs_zpra_t_prd_tar-product ) .
    IF lv_combine_field IS INITIAL.
      CONCATENATE gs_zpra_t_prd_tar-product(gv_len) '-' gs_zpra_t_prd_tar-asset INTO lv_col_name .
    ELSE.
      CONCATENATE gs_zpra_t_prd_tar-product(gv_len) '-' 'COMBINE'               INTO lv_col_name .
    ENDIF.
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_prd_tar-tar_qty .
      UNASSIGN <gfs_field> .
    ENDIF.
* Product Total..
    CONCATENATE gs_zpra_t_prd_tar-product(gv_len) '-' 'TOTAL'                   INTO lv_col_name .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_prd_tar-tar_qty .
      UNASSIGN <gfs_field> .
    ENDIF.
* Grand Total
    CONCATENATE 'GRAND' '-' 'TOTAL'                                              INTO lv_col_name .
    ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      <gfs_field> = <gfs_field> + gs_zpra_t_prd_tar-tar_qty .
      UNASSIGN <gfs_field> .
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM fetch_data_section3c .

  DATA : lv_monat            TYPE                   zpra_t_mrec_prd-monat,
         lv_gjahr            TYPE                   zpra_t_mrec_prd-gjahr,
         lv_date             TYPE                   sy-datum,
         lv_month_begin_date TYPE                   sy-datum,
         lv_month_end_date   TYPE                   sy-datum,
         lv_poper            TYPE                   t009b-poper.
  lv_gjahr = gv_last_gjahr - 1 .
  SELECT gjahr
         monat
         asset
         block
         product
         prd_vl_type
         prod_vl_qty1
         prod_vl_uom1
         prod_vl_qty2
         prod_vl_uom2
    FROM zpra_t_mrec_prd
    INTO TABLE gt_zpra_t_mrec_prd_3c
     FOR ALL ENTRIES IN gt_zpra_c_prd_prof
   WHERE asset        EQ gt_zpra_c_prd_prof-asset
     AND block        EQ gt_zpra_c_prd_prof-block
     AND product      EQ gt_zpra_c_prd_prof-product
     AND prd_vl_type  IN r_prd_vl_type[]
*     AND ( gjahr EQ gv_last_gjahr OR gjahr EQ gv_current_gjahr   ) .
    AND gjahr GE lv_gjahr
    AND gjahr LE gv_current_gjahr .
  SORT gt_zpra_t_mrec_prd_3c BY product ASCENDING asset ASCENDING block  ASCENDING gjahr DESCENDING monat DESCENDING .
  DELETE gt_zpra_t_mrec_prd_3c WHERE gjahr EQ gv_current_gjahr AND monat GT gv_current_monat .
  SELECT gjahr
         monat
         asset
         block
         product
         prd_vl_type
         app_vl_qty
         app_vl_uom
    FROM zpra_t_mrec_app
    INTO TABLE gt_zpra_t_mrec_app
     FOR ALL ENTRIES IN gt_zpra_c_prd_prof
   WHERE gjahr EQ gv_current_gjahr
     AND monat LE gv_current_monat
     AND asset EQ gt_zpra_c_prd_prof-asset
     AND block EQ gt_zpra_c_prd_prof-block
     AND product EQ gt_zpra_c_prd_prof-product
     AND prd_vl_type IN r_prd_vl_type[] .

  SORT gt_zpra_t_mrec_app BY gjahr monat product asset block  prd_vl_type .

  SELECT *
    FROM zpra_t_tar_cf
    INTO TABLE gt_zpra_t_tar_cf_3c
     FOR ALL ENTRIES IN gt_zpra_c_prd_prof
   WHERE gjahr        EQ gv_current_gjahr
     AND asset        EQ gt_zpra_c_prd_prof-asset
     AND block        EQ gt_zpra_c_prd_prof-block
     AND product      EQ gt_zpra_c_prd_prof-product .

  SORT gt_zpra_t_tar_cf_3c BY gjahr asset block product.

  REFRESH r_production_date[] .

  LOOP AT gt_zpra_c_prd_prof INTO gs_zpra_c_prd_prof .
    lv_monat = '00' .
    DO .
      lv_monat = lv_monat + 1 .
      READ TABLE gt_zpra_t_mrec_app TRANSPORTING NO FIELDS WITH KEY gjahr = gv_current_gjahr
                                                                    monat = lv_monat
                                                                  product = gs_zpra_c_prd_prof-product
                                                                    asset = gs_zpra_c_prd_prof-asset BINARY SEARCH .
      IF sy-subrc IS NOT INITIAL.
        lv_poper = lv_monat .
        CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
          EXPORTING
            i_gjahr        = gv_current_gjahr
*           I_MONMIT       = 00
            i_periv        = gv_periv
            i_poper        = lv_poper
          IMPORTING
            e_date         = lv_month_begin_date
          EXCEPTIONS
            input_false    = 1
            t009_notfound  = 2
            t009b_notfound = 3
            OTHERS         = 4.
        IF sy-subrc <> 0.
          MESSAGE 'Internal Error' TYPE 'E' .
        ENDIF.

        CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
          EXPORTING
            i_gjahr        = gv_current_gjahr
*           I_MONMIT       = 00
            i_periv        = gv_periv
            i_poper        = lv_poper
          IMPORTING
            e_date         = lv_month_end_date
          EXCEPTIONS
            input_false    = 1
            t009_notfound  = 2
            t009b_notfound = 3
            OTHERS         = 4.
        IF sy-subrc <> 0.
          MESSAGE 'Internal Error' TYPE 'E' .
        ENDIF.
        r_production_date-sign   = 'I' .
        r_production_date-option = 'BT' .
        r_production_date-low    = lv_month_begin_date .
        r_production_date-high   = lv_month_end_date .
        APPEND r_production_date .
      ENDIF.
      IF lv_monat GE gv_current_monat .
        EXIT .
      ENDIF.
    ENDDO .
  ENDLOOP.
  SORT r_production_date[] .
  DELETE ADJACENT DUPLICATES FROM r_production_date[] COMPARING ALL FIELDS .

  IF r_production_date[] IS NOT INITIAL .
    SELECT *
      FROM zpra_t_dly_prd
      INTO TABLE gt_zpra_t_dly_prd_3c
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE asset EQ gt_zpra_c_prd_prof-asset
       AND block EQ gt_zpra_c_prd_prof-block
       AND product EQ gt_zpra_c_prd_prof-product
       AND prd_vl_type IN r_prd_vl_type[]
       AND production_date IN r_production_date[] .
    LOOP AT r_production_date.
      PERFORM populate_no_data_entries TABLES gt_zpra_t_dly_prd_3c USING r_production_date-low r_production_date-high .
    ENDLOOP.

    SORT gt_zpra_t_dly_prd_3c BY product asset block production_date .

    SELECT *
      FROM zpra_t_dly_rprd
      INTO TABLE gt_zpra_t_dly_rprd_3c
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE asset EQ gt_zpra_c_prd_prof-asset
       AND block EQ gt_zpra_c_prd_prof-block
       AND product EQ gt_zpra_c_prd_prof-product
*       AND prd_vl_type IN r_prd_vl_type[]
       AND prd_vl_type EQ 'NET_PROD'
       AND production_date IN r_production_date[] .

    SORT gt_zpra_t_dly_rprd_3c BY product asset block production_date .

    CONCATENATE gv_current_gjahr '0101' INTO lv_date .
    SELECT asset
          block
          vld_frm
          vld_to
          pi
          prod_start_date
     FROM zpra_t_prd_pi
     INTO TABLE gt_zpra_t_prd_pi_3c
      FOR ALL ENTRIES IN gt_zpra_c_prd_prof
    WHERE asset EQ gt_zpra_c_prd_prof-asset
      AND block EQ gt_zpra_c_prd_prof-block
      AND vld_frm LE p_date
      AND vld_to  GE lv_date .

    SORT gt_zpra_t_prd_pi_3c BY asset block vld_frm vld_to .


  ENDIF.

ENDFORM.
FORM fetch_data_section3f .

  DATA : lv_monat            TYPE                   zpra_t_mrec_prd-monat,
         lv_gjahr            TYPE                   zpra_t_mrec_prd-gjahr,
         lv_date             TYPE                   sy-datum,
         lv_date2            TYPE                   sy-datum,
         lv_month_begin_date TYPE                   sy-datum,
         lv_month_end_date   TYPE                   sy-datum,
         lv_poper            TYPE                   t009b-poper,
         lv_count            TYPE                   sy-tabix,
         lv_flagnd.

  SELECT gjahr
         monat
         asset
         block
         product
         prd_vl_type
         prod_vl_qty1
         prod_vl_uom1
         prod_vl_qty2
         prod_vl_uom2
    FROM zpra_t_mrec_prd
    INTO TABLE gt_zpra_t_mrec_prd_3f
     FOR ALL ENTRIES IN gt_zpra_c_prd_prof
   WHERE asset        EQ gt_zpra_c_prd_prof-asset
     AND block        EQ gt_zpra_c_prd_prof-block
     AND product      EQ gt_zpra_c_prd_prof-product
     AND prd_vl_type  IN r_prd_vl_type[]
     AND gjahr GE gv_5_back_gjahr
     AND gjahr LE gv_last_gjahr.
  SORT gt_zpra_t_mrec_prd_3f BY product ASCENDING asset ASCENDING block  ASCENDING gjahr DESCENDING monat DESCENDING .

  SELECT gjahr
         monat
         asset
         block
         product
         prd_vl_type
         app_vl_qty
         app_vl_uom
    FROM zpra_t_mrec_app
    INTO TABLE gt_zpra_t_mrec_app_3f
     FOR ALL ENTRIES IN gt_zpra_c_prd_prof
   WHERE gjahr GE gv_5_back_gjahr
     AND gjahr LE gv_last_gjahr
     AND asset EQ gt_zpra_c_prd_prof-asset
     AND block EQ gt_zpra_c_prd_prof-block
     AND product EQ gt_zpra_c_prd_prof-product .

  SORT gt_zpra_t_mrec_app_3f BY gjahr monat asset block product .

    CONCATENATE gv_5_back_gjahr '0401' INTO lv_date  .
    CONCATENATE gv_current_gjahr   '0331' INTO lv_date2 .
    SELECT asset
          block
          vld_frm
          vld_to
          pi
          prod_start_date
     FROM zpra_t_prd_pi
     INTO TABLE gt_zpra_t_prd_pi_3f
      FOR ALL ENTRIES IN gt_zpra_c_prd_prof
    WHERE asset EQ gt_zpra_c_prd_prof-asset
      AND vld_frm LE lv_date2
      AND vld_to  GE lv_date .

    SORT gt_zpra_t_prd_pi_3f BY asset block vld_frm vld_to .

       SELECT *
      FROM zpra_t_dly_prd
      INTO TABLE gt_zpra_t_dly_prd_3f
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE asset EQ gt_zpra_c_prd_prof-asset
       AND block EQ gt_zpra_c_prd_prof-block
       AND product EQ gt_zpra_c_prd_prof-product
       AND prd_vl_type IN r_prd_vl_type[]
       AND  production_date BETWEEN  lv_date  AND LV_DATE2 .
*       AND production_date IN r_production_date[] .

       SELECT *
      FROM zpra_t_dly_rprd
      INTO TABLE gt_zpra_t_dly_rprd_3f
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE asset EQ gt_zpra_c_prd_prof-asset
       AND block EQ gt_zpra_c_prd_prof-block
       AND product EQ gt_zpra_c_prd_prof-product
*       AND prd_vl_type IN r_prd_vl_type[]
       AND prd_vl_type EQ 'NET_PROD'
        AND  production_date BETWEEN  lv_date  AND LV_DATE2 .
*       AND production_date IN r_production_date[] .
    SORT gt_zpra_t_dly_rprd_3f BY product asset block production_date .


  REFRESH r_production_date[] .
  lv_gjahr = gv_5_back_gjahr - 1.
  DO 5 TIMES .
    lv_gjahr = lv_gjahr + 1 .
    LOOP AT gt_zpra_c_prd_prof INTO gs_zpra_c_prd_prof .
      lv_monat = '00' .
      DO .
        lv_monat = lv_monat + 1 .

        READ TABLE gt_zpra_t_mrec_prd_3f TRANSPORTING NO FIELDS WITH KEY gjahr = lv_gjahr
                                                                         monat = lv_monat
                                                                       product = gs_zpra_c_prd_prof-product
                                                                         asset = gs_zpra_c_prd_prof-asset.
        IF sy-subrc IS NOT INITIAL.
          lv_poper = lv_monat .
          CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
            EXPORTING
              i_gjahr        = lv_gjahr
*             I_MONMIT       = 00
              i_periv        = gv_periv
              i_poper        = lv_poper
            IMPORTING
              e_date         = lv_month_begin_date
            EXCEPTIONS
              input_false    = 1
              t009_notfound  = 2
              t009b_notfound = 3
              OTHERS         = 4.
          IF sy-subrc <> 0.
            MESSAGE 'Internal Error' TYPE 'E' .
          ENDIF.
   clear GS_ZPRA_T_PRD_PI.
   READ TABLE gt_zpra_t_prd_pi_3f INTO GS_ZPRA_T_PRD_PI WITH KEY  asset = GS_zpra_c_prd_prof-asset
                                                                  block = GS_zpra_c_prd_prof-block.
    IF SY-SUBRC eq 0 and  GS_ZPRA_T_PRD_PI-PROD_START_DATE LE  lv_month_begin_date.





          CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
            EXPORTING
              i_gjahr        = lv_gjahr
*             I_MONMIT       = 00
              i_periv        = gv_periv
              i_poper        = lv_poper
            IMPORTING
              e_date         = lv_month_end_date
            EXCEPTIONS
              input_false    = 1
              t009_notfound  = 2
              t009b_notfound = 3
              OTHERS         = 4.
          IF sy-subrc <> 0.
            MESSAGE 'Internal Error' TYPE 'E' .
          ENDIF.
          r_production_date-sign   = 'I' .
          r_production_date-option = 'BT' .
          r_production_date-low    = lv_month_begin_date .
          r_production_date-high   = lv_month_end_date .
          APPEND r_production_date .
        ENDIF.
          ENDIF.
        IF lv_monat GE 12 .
          EXIT .
        ENDIF.
      ENDDO .
    ENDLOOP.
  ENDDO.

  SORT r_production_date[] .
  DELETE ADJACENT DUPLICATES FROM r_production_date[] COMPARING ALL FIELDS .

  IF r_production_date[] IS NOT INITIAL .

      SELECT *
      FROM zpra_dly_prd_nd
      INTO TABLE gt_zpra_t_dly_prd_nd
       FOR ALL ENTRIES IN gt_zpra_c_prd_prof
     WHERE asset EQ gt_zpra_c_prd_prof-asset
       AND block EQ gt_zpra_c_prd_prof-block
       AND product EQ gt_zpra_c_prd_prof-product
       AND prd_vl_type IN r_prd_vl_type[]
       AND  production_date BETWEEN  lv_date  AND LV_DATE2 .

*    LOOP AT r_production_date.
*    loop at gt_zpra_t_dly_prd_nd INTO gs_zpra_t_dly_prd_nd where PRODUCTION_DATE BETWEEN  r_production_date-low  AND r_production_date-high .
*    lv_flagnd = 'X'.
*    exit.
*    endloop.
*    if lv_flagnd <> 'X'.
*      PERFORM populate_no_data_entries_3f TABLES gt_zpra_t_dly_prd_3f USING r_production_date-low r_production_date-high .
*      endif.
*    clear lv_flagnd.
*    ENDLOOP.

      if gt_zpra_t_dly_prd_nd[] is INITIAL.
      LOOP AT r_production_date.
      PERFORM populate_no_data_entries_3f TABLES gt_zpra_t_dly_prd_3f USING r_production_date-low r_production_date-high .
      ENDLOOP.
      else.
      APPEND LINES OF gt_zpra_t_dly_prd_nd to gt_zpra_t_dly_prd_3f.
      endif.




    SORT gt_zpra_t_dly_prd_3f BY product asset block production_date .


    PERFORM show_progress USING '50' .


  ENDIF.
clear GS_ZPRA_T_PRD_PI.
ENDFORM.
FORM fetch_data_section4a .
  DATA : lv_monat            TYPE                   zpra_t_mrec_prd-monat,
         lv_gjahr            TYPE                   zpra_t_mrec_prd-gjahr,
         lv_date             TYPE                   sy-datum,
         lv_date2            TYPE                   sy-datum,
         lv_month_begin_date TYPE                   sy-datum,
         lv_month_end_date   TYPE                   sy-datum,
         lv_poper            TYPE                   t009b-poper.

  SELECT gjahr
         monat
         asset
         block
         product
         prd_vl_type
         prod_vl_qty1
         prod_vl_uom1
         prod_vl_qty2
         prod_vl_uom2
    FROM zpra_t_mrec_prd
    INTO TABLE gt_zpra_t_mrec_prd_4a
     FOR ALL ENTRIES IN gt_zdpr_gas_combine
   WHERE asset        EQ gt_zdpr_gas_combine-asset
     AND product      EQ c_prod_gas
     AND prd_vl_type  IN r_prd_vl_type[]
     AND gjahr GE gv_last_gjahr
     AND monat LE gv_current_monat .

  SORT gt_zpra_t_mrec_prd_4a BY product ASCENDING asset ASCENDING block  ASCENDING gjahr DESCENDING monat DESCENDING .
***  SELECT gjahr
***         monat
***         asset
***         block
***         product
***         prd_vl_type
***         app_vl_qty
***         app_vl_uom
***    FROM zpra_t_mrec_app
***    INTO TABLE gt_zpra_t_mrec_app_4a
***     FOR ALL ENTRIES IN gt_zdpr_gas_combine
***   WHERE gjahr EQ gv_current_gjahr
***     AND monat LE gv_current_monat
***     AND asset EQ gt_zdpr_gas_combine-asset
***     AND product EQ c_prod_gas
***     AND prd_vl_type IN r_prd_vl_type[] .
***
***  SORT gt_zpra_t_mrec_app_4a BY gjahr monat product asset block  prd_vl_type .
***

  REFRESH r_production_date[] .
  lv_gjahr = gv_current_gjahr .
  LOOP AT gt_zdpr_gas_combine INTO gs_zdpr_gas_combine .
    lv_monat = '00' .
    DO .
      lv_monat = lv_monat + 1 .
      READ TABLE gt_zpra_t_mrec_app_4a TRANSPORTING NO FIELDS WITH KEY gjahr = lv_gjahr
                                                                       monat = lv_monat
                                                                     product = c_prod_gas
                                                                       asset = gs_zdpr_gas_combine-asset BINARY SEARCH .
      IF sy-subrc IS NOT INITIAL.
        lv_poper = lv_monat .
        CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
          EXPORTING
            i_gjahr        = lv_gjahr
*           I_MONMIT       = 00
            i_periv        = gv_periv
            i_poper        = lv_poper
          IMPORTING
            e_date         = lv_month_begin_date
          EXCEPTIONS
            input_false    = 1
            t009_notfound  = 2
            t009b_notfound = 3
            OTHERS         = 4.
        IF sy-subrc <> 0.
          MESSAGE 'Internal Error' TYPE 'E' .
        ENDIF.

        CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
          EXPORTING
            i_gjahr        = lv_gjahr
*           I_MONMIT       = 00
            i_periv        = gv_periv
            i_poper        = lv_poper
          IMPORTING
            e_date         = lv_month_end_date
          EXCEPTIONS
            input_false    = 1
            t009_notfound  = 2
            t009b_notfound = 3
            OTHERS         = 4.
        IF sy-subrc <> 0.
          MESSAGE 'Internal Error' TYPE 'E' .
        ENDIF.
        r_production_date-sign   = 'I' .
        r_production_date-option = 'BT' .
        r_production_date-low    = lv_month_begin_date .
        r_production_date-high   = lv_month_end_date .
        APPEND r_production_date .
      ENDIF.
      IF lv_monat GE gv_current_monat .
        EXIT .
      ENDIF.
    ENDDO .
  ENDLOOP.

  SORT r_production_date[] .
  DELETE ADJACENT DUPLICATES FROM r_production_date[] COMPARING ALL FIELDS .

  IF r_production_date[] IS NOT INITIAL .
    SELECT *
      FROM zpra_t_dly_rprd
      INTO TABLE gt_zpra_t_dly_rprd_4a
       FOR ALL ENTRIES IN gt_zdpr_gas_combine
     WHERE asset EQ gt_zdpr_gas_combine-asset
       AND product EQ c_prod_gas
       AND prd_vl_type EQ 'NET_PROD'
       AND production_date IN r_production_date[].
    SORT gt_zpra_t_dly_rprd_4a BY product asset block production_date .

    SELECT *
      FROM zpra_t_dly_prd
      INTO TABLE gt_zpra_t_dly_prd_4a
       FOR ALL ENTRIES IN gt_zdpr_gas_combine
     WHERE asset EQ gt_zdpr_gas_combine-asset
       AND product EQ c_prod_gas
       AND prd_vl_type IN r_prd_vl_type[]
       AND production_date IN r_production_date[] .
    SORT gt_zpra_t_dly_prd_4a BY product asset block production_date .

    LOOP AT r_production_date.
      PERFORM populate_no_data_entries TABLES gt_zpra_t_dly_prd_4a USING r_production_date-low r_production_date-high .
    ENDLOOP.

    CONCATENATE gv_last_gjahr '0101' INTO lv_date  .
    SELECT asset
          block
          vld_frm
          vld_to
          pi
          prod_start_date
     FROM zpra_t_prd_pi
     INTO TABLE gt_zpra_t_prd_pi_4a
      FOR ALL ENTRIES IN gt_zdpr_gas_combine
    WHERE asset EQ gt_zdpr_gas_combine-asset
      AND vld_frm LE p_date
      AND vld_to  GE lv_date .

    SORT gt_zpra_t_prd_pi_4a BY asset block vld_frm vld_to .
  ENDIF.
ENDFORM.
FORM prepare_dynamic_table_sec5a .
  REFRESH gt_dyn_fcat .

  PERFORM add_dyn_field USING 'PRODUCTION_DATE' 'Production Date'  15 .

  PERFORM add_dyn_field USING 'ACT_PRODUCTION' 'Actual Production'  15 .

  IF p_t_be IS NOT INITIAL.
    PERFORM add_dyn_field USING 'TAR_BE' 'BE Target' 15 .
  ENDIF.
  IF p_t_in IS NOT INITIAL.
    PERFORM add_dyn_field USING 'TAR_IN' 'Internal Target' 15 .
  ENDIF.
  IF p_t_ex IS NOT INITIAL.
    PERFORM add_dyn_field USING 'TAR_EX' 'MOU Excellent Target' 15 .
  ENDIF.
  IF p_t_vg IS NOT INITIAL.
    PERFORM add_dyn_field USING 'TAR_VG' 'MOU Very Good Target' 15 .
  ENDIF.
  IF p_t_pc IS NOT INITIAL.
    PERFORM add_dyn_field USING 'TAR_PC' 'PC Target' 15 .
  ENDIF.
  IF p_t_re IS NOT INITIAL.
    PERFORM add_dyn_field USING 'TAR_RE' 'RE Target' 15 .
  ENDIF.

  DESCRIBE TABLE gt_dyn_fcat LINES gv_5a_cols .

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
*     i_style_table             = X
      it_fieldcatalog           = gt_dyn_fcat
    IMPORTING
      ep_table                  = gt_sec5a_table
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2.
  IF sy-subrc NE 0.
    MESSAGE 'Unexpected Internal Error' TYPE 'E' .
  ELSE.
* Assign the new table to field symbol
    ASSIGN gt_sec5a_table->* TO <gfs_sec5a_table>.
  ENDIF.
ENDFORM.
FORM prepare_dynamic_table_sec6 .
  REFRESH gt_dyn_fcat .

  PERFORM add_dyn_field USING 'COL1' 'COL1'  50 .
  PERFORM add_dyn_field USING 'OIL_ANNUAL' 'OIL_ANNUAL'  35 .
  PERFORM add_dyn_field USING 'OIL_YTD' 'OIL_YTD'  35 .
  PERFORM add_dyn_field USING 'GAS_ANNUAL' 'GAS_ANNUAL'  35 .
  PERFORM add_dyn_field USING 'GAS_YTD' 'GAS_YTD'  35 .
  PERFORM add_dyn_field USING 'TOTAL_ANNUAL' 'TOTAL_ANNUAL'  35 .
  PERFORM add_dyn_field USING 'TOTAL_YTD' 'TOTAL_YTD'  35 .

  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
*     i_style_table             = X
      it_fieldcatalog           = gt_dyn_fcat
    IMPORTING
      ep_table                  = gt_sec6_table
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2.
  IF sy-subrc NE 0.
    MESSAGE 'Unexpected Internal Error' TYPE 'E' .
  ELSE.
* Assign the new table to field symbol
    ASSIGN gt_sec6_table->* TO <gfs_sec6_table>.
  ENDIF.
ENDFORM.
FORM fill_dynamic_table_sec5a .
  DATA : lv_date         TYPE sy-datum,
         lv_monat_3digit TYPE t009b-poper,
         lv_monat_2digit TYPE bkpf-monat,
         lv_gjahr        TYPE bkpf-gjahr,
         lv_first_day    TYPE sy-datum,
         lv_last_day     TYPE sy-datum,
         lv_days         TYPE sy-tabix,
         lv_col_name     TYPE lvc_fname.

  lv_date =  gv_year_start_date.
  DO .
    IF lv_date GT p_date.
      EXIT .
    ENDIF.
    APPEND INITIAL LINE TO <gfs_sec5a_table> ASSIGNING <gfs_dyn_line> .
    ASSIGN COMPONENT 'PRODUCTION_DATE' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
      EXPORTING
        input  = lv_date
      IMPORTING
        output = <gfs_field>.
    IF sy-subrc <> 0.
      MESSAGE 'Unexpteced Internal Error' TYPE 'E' .
    ENDIF.
    IF lv_date+6(2) EQ '01'.
      CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
        EXPORTING
          i_date         = lv_date
        " I_MONMIT       = 00
          i_periv        = gv_periv
        IMPORTING
          e_buper        = lv_monat_3digit
          e_gjahr        = lv_gjahr
        EXCEPTIONS
          input_false    = 01
          t009_notfound  = 02
          t009b_notfound = 03.

      IF sy-subrc <> 0.
        MESSAGE 'Error getting Period' TYPE 'E' .
      ENDIF.
      lv_monat_2digit = lv_monat_3digit+1(2) .
      CALL FUNCTION 'PERIOD_DAY_DETERMINE'
        EXPORTING
          i_gjahr              = lv_gjahr
          i_monat              = lv_monat_2digit
          i_periv              = gv_periv
        IMPORTING
          e_fday               = lv_first_day
          e_lday               = lv_last_day
*         E_SPERIOD            =
        EXCEPTIONS
          error_period         = 1
          error_period_version = 2
          firstday_not_defined = 3
          period_not_defined   = 4
          year_invalid         = 5
          OTHERS               = 6.
      IF sy-subrc <> 0.
        MESSAGE 'Internal Date error' TYPE 'E' .
      ENDIF.
      lv_days = lv_last_day - lv_first_day + 1 .
    ENDIF.
    ASSIGN COMPONENT 2 OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED.
      LOOP AT gt_zpra_t_dly_prd_5a INTO gs_zpra_t_dly_prd WHERE production_date EQ lv_date.
        PERFORM convert_non_gas_units_5a CHANGING gs_zpra_t_dly_prd.
        LOOP AT gt_zpra_t_prd_pi_5a INTO gs_zpra_t_prd_pi WHERE asset EQ gs_zpra_t_dly_prd-asset
                                                            AND block EQ gs_zpra_t_dly_prd-block
                                                            AND vld_frm LE lv_date
                                                            AND vld_to  GE lv_date.
          EXIT .
        ENDLOOP.
        gs_zpra_t_dly_prd-prod_vl_qty1 = gs_zpra_t_dly_prd-prod_vl_qty1 * gs_zpra_t_prd_pi-pi / 100 .
        <gfs_field> = <gfs_field> + gs_zpra_t_dly_prd-prod_vl_qty1 .
      ENDLOOP.
      UNASSIGN <gfs_field> .
    ENDIF.
    LOOP AT gt_zpra_t_prd_tar_5a INTO gs_zpra_t_prd_tar WHERE gjahr EQ lv_gjahr
                                                          AND monat EQ lv_monat_2digit.
***     IF gs_zpra_t_prd_tar-product EQ c_prod_gas .
***      gs_zpra_t_prd_tar-tar_qty2 = gs_zpra_t_prd_tar-tar_qty2 * 1000 * 6290.
***     ELSE.
***      CLEAR gs_zpra_t_tar_cf .
***      READ TABLE gt_zpra_t_tar_cf_5a INTO gs_zpra_t_tar_cf WITH KEY gjahr   = lv_gjahr
***                                                                    product = gs_zpra_t_prd_tar-product
***                                                                    asset   = gs_zpra_t_prd_tar-asset
***                                                                    block   = gs_zpra_t_prd_tar-block BINARY SEARCH .
***      gs_zpra_t_prd_tar-tar_qty2 = gs_zpra_t_prd_tar-tar_qty2 *   gs_zpra_t_tar_cf-conv_factor .
***      gs_zpra_t_prd_tar-tar_qty2 = gs_zpra_t_prd_tar-tar_qty2 * 1000000 .
***     ENDIF.
      PERFORM convert_target_units CHANGING gs_zpra_t_prd_tar.

      lv_col_name = gs_zpra_t_prd_tar-tar_code .
      ASSIGN COMPONENT lv_col_name OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      <gfs_field> = <gfs_field> + gs_zpra_t_prd_tar-tar_qty2 / lv_days .
      UNASSIGN <gfs_field> .

    ENDLOOP.

    lv_date = lv_date + 1 .
  ENDDO.
  IF p_mb IS NOT INITIAL.
    LOOP AT <gfs_sec5a_table> ASSIGNING <gfs_dyn_line> .
      ASSIGN COMPONENT 'ACT_PRODUCTION' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
      <gfs_field> = <gfs_field> / 1000000 .
    ENDLOOP.
  ENDIF.
ENDFORM.
FORM fill_dynamic_table_sec6 .
  PERFORM fill_dynamic_table_sec6a .
  PERFORM fill_dynamic_table_sec6b .
ENDFORM .
FORM fill_dynamic_table_sec6a .
  DATA lv_qty TYPE char50 .

  READ TABLE <gfs_sec2d_table> ASSIGNING <gfs_dyn_line> INDEX 1 .
  IF sy-subrc IS INITIAL.
    UNASSIGN <gfs_field>.
    APPEND INITIAL LINE TO <gfs_sec6_table> ASSIGNING <gfs_dyn_line2> .
    ASSIGN COMPONENT 'COL1' OF STRUCTURE <gfs_dyn_line2> TO <gfs_field2> .
    <gfs_field2> = 'Actual' .
    UNASSIGN <gfs_field2> .

    ASSIGN COMPONENT '722000001-TOTAL' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED .
      lv_qty = lv_qty + <gfs_field> .
      UNASSIGN <gfs_field> .
    ENDIF.

    ASSIGN COMPONENT '722000003-TOTAL' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED .
      lv_qty = lv_qty + <gfs_field> .
      UNASSIGN <gfs_field> .
    ENDIF.

    ASSIGN COMPONENT '722000005-TOTAL' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED .
      lv_qty = lv_qty + <gfs_field> .
      UNASSIGN <gfs_field> .
    ENDIF.
    SHIFT lv_qty LEFT DELETING LEADING space .
    ASSIGN COMPONENT 'OIL_ANNUAL' OF STRUCTURE  <gfs_dyn_line2> TO <gfs_field2> .
    IF <gfs_field2> IS ASSIGNED.
      <gfs_field2> = lv_qty .
      UNASSIGN <gfs_field2> .
    ENDIF.
    ASSIGN COMPONENT 'OIL_YTD' OF STRUCTURE  <gfs_dyn_line2> TO <gfs_field2> .
    IF <gfs_field2> IS ASSIGNED.
      <gfs_field2> = lv_qty .
      UNASSIGN <gfs_field2> .
    ENDIF.

    CLEAR lv_qty .
    ASSIGN COMPONENT '722000004-TOTAL' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED .
      lv_qty =  <gfs_field> .
      UNASSIGN <gfs_field> .
    ENDIF.
    SHIFT lv_qty LEFT DELETING LEADING space .
    ASSIGN COMPONENT 'GAS_ANNUAL' OF STRUCTURE  <gfs_dyn_line2> TO <gfs_field2> .
    IF <gfs_field2> IS ASSIGNED.
      <gfs_field2> = lv_qty .
      UNASSIGN <gfs_field2> .
    ENDIF.
    ASSIGN COMPONENT 'GAS_YTD' OF STRUCTURE  <gfs_dyn_line2> TO <gfs_field2> .
    IF <gfs_field2> IS ASSIGNED.
      <gfs_field2> = lv_qty .
      UNASSIGN <gfs_field2> .
    ENDIF.

    CLEAR lv_qty .
    ASSIGN COMPONENT 'GRAND-TOTAL' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED .
      lv_qty =  <gfs_field> .
      UNASSIGN <gfs_field> .
    ENDIF.
    SHIFT lv_qty LEFT DELETING LEADING space .
    ASSIGN COMPONENT 'TOTAL_ANNUAL' OF STRUCTURE  <gfs_dyn_line2> TO <gfs_field2> .
    IF <gfs_field2> IS ASSIGNED.
      <gfs_field2> = lv_qty .
      UNASSIGN <gfs_field2> .
    ENDIF.
    ASSIGN COMPONENT 'TOTAL_YTD' OF STRUCTURE  <gfs_dyn_line2> TO <gfs_field2> .
    IF <gfs_field2> IS ASSIGNED.
      <gfs_field2> = lv_qty .
      UNASSIGN <gfs_field2> .
    ENDIF.

  ENDIF.
ENDFORM .
FORM fill_dynamic_table_sec6b .
  FIELD-SYMBOLS : <lfs_field1> ,
                  <lfs_field2>  .
  READ TABLE <gfs_sec6_table> ASSIGNING <gfs_dyn_line1> INDEX 1 .
  LOOP AT <gfs_sec2b_table> ASSIGNING <gfs_dyn_line>.

    APPEND INITIAL LINE TO <gfs_sec6_table> ASSIGNING <gfs_dyn_line2> .
    ASSIGN COMPONENT 1 OF STRUCTURE <gfs_dyn_line2> TO <lfs_field2> .
    ASSIGN COMPONENT 'COL02' OF STRUCTURE <gfs_dyn_line> TO <lfs_field1> .

    CONCATENATE <lfs_field1> 'Target' INTO <lfs_field2> SEPARATED BY space .

    PERFORM get_annual_targets_6b .
    PERFORM get_ytd_targets_6b .
    PERFORM get_achievement_6c .
  ENDLOOP.
ENDFORM .
FORM get_annual_targets_6b .
  DATA lv_qty TYPE char35 .
  ASSIGN COMPONENT '722000001-TOTAL' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
  IF <gfs_field> IS ASSIGNED .
    lv_qty = lv_qty + <gfs_field> .
    UNASSIGN <gfs_field> .
  ENDIF.

  ASSIGN COMPONENT '722000003-TOTAL' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
  IF <gfs_field> IS ASSIGNED .
    lv_qty = lv_qty + <gfs_field> .
    UNASSIGN <gfs_field> .
  ENDIF.

  ASSIGN COMPONENT '722000005-TOTAL' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
  IF <gfs_field> IS ASSIGNED .
    lv_qty = lv_qty + <gfs_field> .
    UNASSIGN <gfs_field> .
  ENDIF.

  ASSIGN COMPONENT 'OIL_ANNUAL' OF STRUCTURE  <gfs_dyn_line2> TO <gfs_field2> .
  IF <gfs_field2> IS ASSIGNED.
    <gfs_field2> = lv_qty .
    UNASSIGN <gfs_field2> .
  ENDIF.

  CLEAR lv_qty .
  ASSIGN COMPONENT '722000004-TOTAL' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
  IF <gfs_field> IS ASSIGNED .
    lv_qty =  <gfs_field> .
    UNASSIGN <gfs_field> .
  ENDIF.

  ASSIGN COMPONENT 'GAS_ANNUAL' OF STRUCTURE  <gfs_dyn_line2> TO <gfs_field2> .
  IF <gfs_field2> IS ASSIGNED.
    <gfs_field2> = lv_qty .
    UNASSIGN <gfs_field2> .
  ENDIF.

  CLEAR lv_qty .
  ASSIGN COMPONENT 'GRAND-TOTAL' OF STRUCTURE <gfs_dyn_line> TO <gfs_field> .
  IF <gfs_field> IS ASSIGNED .
    lv_qty =  <gfs_field> .
    UNASSIGN <gfs_field> .
  ENDIF.

  ASSIGN COMPONENT 'TOTAL_ANNUAL' OF STRUCTURE  <gfs_dyn_line2> TO <gfs_field2> .
  IF <gfs_field2> IS ASSIGNED.
    <gfs_field2> = lv_qty .
    UNASSIGN <gfs_field2> .
  ENDIF.
ENDFORM .
FORM get_ytd_targets_6b .
  DATA lv_qty TYPE char35 .
  FIELD-SYMBOLS : <lfs_field1> ,
                  <lfs_field2> .
  LOOP AT <gfs_sec2c_table> ASSIGNING <gfs_dyn_line3> .
    ASSIGN COMPONENT 'COL02' OF STRUCTURE <gfs_dyn_line> TO <lfs_field1>.
    ASSIGN COMPONENT 'COL02' OF STRUCTURE <gfs_dyn_line3> TO <lfs_field2>.
    IF <lfs_field1> NE <lfs_field2>.
      CONTINUE .
    ENDIF.
    ASSIGN COMPONENT '722000001-TOTAL' OF STRUCTURE <gfs_dyn_line3> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED .
      lv_qty = lv_qty + <gfs_field> .
      UNASSIGN <gfs_field> .
    ENDIF.

    ASSIGN COMPONENT '722000003-TOTAL' OF STRUCTURE <gfs_dyn_line3> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED .
      lv_qty = lv_qty + <gfs_field> .
      UNASSIGN <gfs_field> .
    ENDIF.

    ASSIGN COMPONENT '722000005-TOTAL' OF STRUCTURE <gfs_dyn_line3> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED .
      lv_qty = lv_qty + <gfs_field> .
      UNASSIGN <gfs_field> .
    ENDIF.

    ASSIGN COMPONENT 'OIL_YTD' OF STRUCTURE  <gfs_dyn_line2> TO <gfs_field2> .
    IF <gfs_field2> IS ASSIGNED.
      <gfs_field2> = lv_qty .
      UNASSIGN <gfs_field2> .
    ENDIF.

    CLEAR lv_qty .
    ASSIGN COMPONENT '722000004-TOTAL' OF STRUCTURE <gfs_dyn_line3> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED .
      lv_qty =  <gfs_field> .
      UNASSIGN <gfs_field> .
    ENDIF.

    ASSIGN COMPONENT 'GAS_YTD' OF STRUCTURE  <gfs_dyn_line2> TO <gfs_field2> .
    IF <gfs_field2> IS ASSIGNED.
      <gfs_field2> = lv_qty .
      UNASSIGN <gfs_field2> .
    ENDIF.

    CLEAR lv_qty .
    ASSIGN COMPONENT 'GRAND-TOTAL' OF STRUCTURE <gfs_dyn_line3> TO <gfs_field> .
    IF <gfs_field> IS ASSIGNED .
      lv_qty =  <gfs_field> .
      UNASSIGN <gfs_field> .
    ENDIF.

    ASSIGN COMPONENT 'TOTAL_YTD' OF STRUCTURE  <gfs_dyn_line2> TO <gfs_field2> .
    IF <gfs_field2> IS ASSIGNED.
      <gfs_field2> = lv_qty .
      UNASSIGN <gfs_field2> .
    ENDIF.
  ENDLOOP.
ENDFORM .
FORM get_achievement_6c .
  DATA lv_index TYPE sy-tabix .
  DATA: lv_num TYPE p length 16 DECIMALS 10.
  FIELD-SYMBOLS : <lfs_field1> ,
                  <lfs_field2> ,
                  <lfs_field3> .
  APPEND INITIAL LINE TO <gfs_sec6_table> ASSIGNING <gfs_dyn_line3> .

  ASSIGN COMPONENT 1 OF STRUCTURE <gfs_dyn_line3> TO <lfs_field3> .
  ASSIGN COMPONENT 'COL02' OF STRUCTURE <gfs_dyn_line> TO <lfs_field1> .

  CONCATENATE '% Achv w.r.t.' <lfs_field1> 'Target' INTO <lfs_field3> SEPARATED BY space .

  DO 7 TIMES.
    lv_index = sy-index .
    ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line3> TO <lfs_field3> .
    IF lv_index = 1.
    ELSE.
      ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line1> TO <lfs_field1> .
* Begin of changes by Arnav on 24.03.2026
      ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line2> TO <lfs_field2>.
      IF <lfs_field2> IS ASSIGNED.
        lv_num = <lfs_field2>.  " implicit conversion
      IF lv_num <> 0.
        <lfs_field3> = <lfs_field1> / lv_num * 100.
      ENDIF.
      ENDIF.

*      ASSIGN COMPONENT lv_index OF STRUCTURE <gfs_dyn_line2> TO <lfs_field2> .
*      CONDENSE: <lfs_field2>.
*      IF <lfs_field2> IS NOT INITIAL and <lfs_field2> <> '0'.
*        <lfs_field3> = <lfs_field1> / <lfs_field2> * 100 .
*      ENDIF.

* End of changes by Arnav on 24.03.2026

    ENDIF.
  ENDDO.
ENDFORM .
FORM fetch_data_section5a .
  DATA : lv_gjahr TYPE zpra_t_mrec_prd-gjahr .
  SELECT *
    FROM zpra_t_dly_prd
    INTO TABLE gt_zpra_t_dly_prd_5a
     FOR ALL ENTRIES IN gt_zpra_c_prd_prof
   WHERE asset EQ gt_zpra_c_prd_prof-asset
     AND block EQ gt_zpra_c_prd_prof-block
     AND product EQ gt_zpra_c_prd_prof-product
     AND prd_vl_type IN r_prd_vl_type[]
     AND production_date GE gv_year_start_date
     AND production_date LE p_date .

  PERFORM populate_no_data_entries TABLES gt_zpra_t_dly_prd_5a USING gv_year_start_date p_date .

  SORT gt_zpra_t_dly_prd_5a BY production_date product asset block  .

  lv_gjahr = gv_current_gjahr - 3 .
  SELECT gjahr
         monat
         asset
         block
         product
         prd_vl_type
         prod_vl_qty1
         prod_vl_uom1
         prod_vl_qty2
         prod_vl_uom2
    FROM zpra_t_mrec_prd
    INTO TABLE gt_zpra_t_mrec_prd_5a
     FOR ALL ENTRIES IN gt_zpra_c_prd_prof
   WHERE asset        EQ gt_zpra_c_prd_prof-asset
     AND block        EQ gt_zpra_c_prd_prof-block
     AND product      EQ gt_zpra_c_prd_prof-product
     AND prd_vl_type  IN r_prd_vl_type[]
     AND gjahr        GE lv_gjahr
     AND gjahr        LE gv_current_gjahr .

  SORT gt_zpra_t_mrec_prd_5a BY product ASCENDING asset ASCENDING block  ASCENDING prd_vl_type ASCENDING gjahr DESCENDING monat DESCENDING .


  SELECT asset
        block
        vld_frm
        vld_to
        pi
        prod_start_date
   FROM zpra_t_prd_pi
   INTO TABLE gt_zpra_t_prd_pi_5a
    FOR ALL ENTRIES IN gt_zpra_c_prd_prof
  WHERE asset EQ gt_zpra_c_prd_prof-asset
    AND block EQ gt_zpra_c_prd_prof-block
    AND vld_frm LE p_date
    AND vld_to  GE gv_year_start_date .

  SELECT *
    FROM zpra_t_tar_cf
    INTO TABLE gt_zpra_t_tar_cf_5a
     FOR ALL ENTRIES IN gt_zpra_c_prd_prof
   WHERE gjahr EQ gv_current_gjahr
     AND asset EQ gt_zpra_c_prd_prof-asset
     AND block EQ gt_zpra_c_prd_prof-block
     AND product EQ gt_zpra_c_prd_prof-product .
  SORT gt_zpra_t_tar_cf_5a BY gjahr product asset block .
ENDFORM.
FORM process_gas_records_5a .

  DATA : lt_zpra_t_dly_prd  TYPE STANDARD TABLE OF zpra_t_dly_prd  .

  DATA : ls_zpra_t_dly_prd   TYPE                  zpra_t_dly_prd  .
  FIELD-SYMBOLS : <fs_dly_prd> TYPE zpra_t_dly_prd  .

  DATA : lv_index TYPE sy-tabix .
  lt_zpra_t_dly_prd  = gt_zpra_t_dly_prd_5a .
  SORT  lt_zpra_t_dly_prd BY production_date product asset block prd_vl_type .

  DELETE gt_zpra_t_dly_prd_5a  WHERE product EQ c_prod_gas .
  DELETE lt_zpra_t_dly_prd     WHERE product NE c_prod_gas .
  CHECK lt_zpra_t_dly_prd IS NOT INITIAL.

  LOOP AT lt_zpra_t_dly_prd INTO ls_zpra_t_dly_prd .
    lv_index = sy-tabix .
    IF lv_index EQ 1 OR
      <fs_dly_prd>-production_date NE ls_zpra_t_dly_prd-production_date  OR
      <fs_dly_prd>-product         NE ls_zpra_t_dly_prd-product          OR
      <fs_dly_prd>-asset           NE ls_zpra_t_dly_prd-asset            OR
      <fs_dly_prd>-block           NE ls_zpra_t_dly_prd-block .

      APPEND INITIAL LINE TO gt_zpra_t_dly_prd_5a ASSIGNING <fs_dly_prd> .
      <fs_dly_prd>-mandt           = ls_zpra_t_dly_prd-mandt .
      <fs_dly_prd>-production_date = ls_zpra_t_dly_prd-production_date .
      <fs_dly_prd>-product         = ls_zpra_t_dly_prd-product .
      <fs_dly_prd>-asset           = ls_zpra_t_dly_prd-asset .
      <fs_dly_prd>-block           = ls_zpra_t_dly_prd-block .
      <fs_dly_prd>-prod_vl_uom1    = ls_zpra_t_dly_prd-prod_vl_uom1 .
      <fs_dly_prd>-prd_vl_type     = 'NET_PROD' .
    ENDIF.
*   PERFORM convert_gas_units_to_bopd CHANGING ls_zpra_t_dly_prd.
    PERFORM convert_gas_units CHANGING ls_zpra_t_dly_prd.
    IF p_bmd IS NOT INITIAL.
      ls_zpra_t_dly_prd-prod_vl_qty1 = ls_zpra_t_dly_prd-prod_vl_qty1 / 6290 .
    ENDIF.
    IF ls_zpra_t_dly_prd-prd_vl_type EQ 'GAS_INJ' .
      ls_zpra_t_dly_prd-prod_vl_qty1 = ls_zpra_t_dly_prd-prod_vl_qty1 * -1 .
    ENDIF.
    <fs_dly_prd>-prod_vl_qty1    = <fs_dly_prd>-prod_vl_qty1 + ls_zpra_t_dly_prd-prod_vl_qty1 .
  ENDLOOP.
  SORT  gt_zpra_t_dly_prd_5a BY production_date product asset .

ENDFORM.
FORM border_cells .

  gv_s_row  = gv_start_row .
  gv_s_col  = 1 .
  gv_e_row  = gv_sec3_end_row .
  gv_e_col  = gv_table_columns .

  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM set_all_borders_range .


  gv_s_row  = gv_sec4_start_row .
  gv_s_col  = 1 .
  gv_e_row  = gv_sec4_end_row .
  gv_e_col  = 2 .

  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  PERFORM set_all_borders_range .

ENDFORM.
FORM set_cell_formats .
  DATA : lv_number_format TYPE char10,
         lv_row           TYPE sy-tabix,
         lv_row2          TYPE sy-tabix.
  PERFORM get_full_number_format CHANGING lv_number_format .
  PERFORM select_range USING gv_sec1_data_start_row 3 gv_sec2_end_row gv_table_columns  .
  PERFORM set_numberformat USING lv_number_format .
  IF p_bmd IS NOT INITIAL.
    lv_number_format = '0.000' .
    READ TABLE gt_product_col INTO gs_product_col WITH KEY product = c_prod_gas  BINARY SEARCH .
    IF sy-subrc IS INITIAL.
      PERFORM select_range USING gv_sec1_data_start_row gs_product_col-s_col gv_sec2_end_row gs_product_col-e_col  .
      PERFORM set_numberformat USING lv_number_format .
    ENDIF.
  ENDIF.
*  lv_row
  PERFORM select_range USING gv_sec3a_start_row 3 gv_sec3_end_row gv_table_columns  .
  PERFORM set_numberformat USING '0.000' .

  PERFORM select_range USING gv_sec1_h_start_row 1 gv_sec3_end_row gv_table_columns  .
  PERFORM set_range_formatting USING  1 'C' 'C' .

  PERFORM select_range USING gv_sec2_start_row 1 gv_sec3_end_row 1  .
  PERFORM set_range_formatting USING  1 'L' 'C' .

  lv_row = gv_sec4_start_row + 1 .
  PERFORM select_range USING gv_sec4_start_row 1 lv_row 2  .
  PERFORM set_range_formatting USING  1 'C' 'C' .
  lv_row = gv_sec1_h_start_row + 4 .
  PERFORM row_height USING lv_row 15 .

*  lv_row  = gv_sec2_end_row + 1 .
*  lv_row2 = gv_sec3a_start_row - 1 .
*  PERFORM select_range USING lv_row 3 lv_row2 gv_table_columns  .
*  PERFORM set_range_formatting USING  1 'C' 'C' .

ENDFORM.
FORM set_border_range  USING    p_left
                                p_right
                                p_top
                                p_bottom .
  IF p_left EQ 1 .
    CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '7' .
    SET PROPERTY OF go_border 'LineStyle' = '1'  .
  ENDIF.
  IF p_right EQ 1 .
    CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '10'.
    SET PROPERTY OF go_border 'LineStyle' = '1' .
  ENDIF.
  IF p_top EQ 1 .
    CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '8'.
    SET PROPERTY OF go_border 'LineStyle' = '1' .
  ENDIF.
  IF p_bottom EQ 1 .
    CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '9'.
    SET PROPERTY OF go_border 'LineStyle' = '1' .
  ENDIF.
ENDFORM.
FORM set_all_borders_range  .
  CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '7' .
  SET PROPERTY OF go_border 'LineStyle' = '1'  .

  CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '8'.
  SET PROPERTY OF go_border 'LineStyle' = '1' .

  CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '9'.
  SET PROPERTY OF go_border 'LineStyle' = '1' .

  CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '10'.
  SET PROPERTY OF go_border 'LineStyle' = '1' .

  CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '11'.
  SET PROPERTY OF go_border 'LineStyle' = '1' .

  CALL METHOD OF go_range 'Borders' = go_border EXPORTING #1 = '12'.
  SET PROPERTY OF go_border 'LineStyle' = '1' .

ENDFORM.
FORM clear_variables .

  REFRESH : gt_zpra_t_dly_prd            ,
            gt_zpra_t_dly_prd2           ,
            gt_zpra_t_dly_prd_mb         ,
            gt_zpra_t_dly_prd_2a3        ,
            gt_zpra_t_dly_prd_2d         ,
            gt_zpra_t_dly_prd_2f         ,
            gt_zpra_t_dly_prd_3c         ,
            gt_zpra_t_dly_prd_3f         ,
            gt_zpra_t_dly_prd_4a         ,
            gt_zpra_t_dly_prd_5a         ,
            gt_zdpr_gas_combine          ,
            gt_zpra_t_prd_pi             ,
            gt_zpra_t_prd_pi_mb          ,
            gt_zpra_t_prd_pi_lm          ,
            gt_zpra_t_prd_pi_2d          ,
            gt_zpra_t_prd_pi_2f          ,
            gt_zpra_t_prd_pi_3c          ,
            gt_zpra_t_prd_pi_3f          ,
            gt_zpra_t_prd_pi_4a          ,
            gt_zpra_t_prd_pi_5a          ,
            gt_zpra_t_mrec_prd           ,
            gt_zpra_t_mrec_prd_2a3       ,
            gt_zpra_t_mrec_prd_2d        ,
            gt_zpra_t_mrec_prd_2f        ,
            gt_zpra_t_mrec_prd_3c        ,
            gt_zpra_t_mrec_prd_3f        ,
            gt_zpra_t_mrec_prd_4a        ,
            gt_wtd_pi                    ,
            gt_wtd_cf                    ,
            gt_asset_desc                ,
            gt_paste                     ,
            gt_copied_cells              ,
            gt_product_col               ,
            gt_zpra_t_tar_pi             ,
            gt_zpra_t_prd_tar            ,
            gt_zpra_t_prd_tar_2c         ,
            gt_zpra_t_prd_tar_3a         ,
            gt_zpra_t_prd_tar_3b         ,
            gt_zpra_t_prd_tar_5a         ,
            gt_zpra_t_tar_cf             ,
            gt_zpra_t_tar_cf_5a          ,
            gt_zpra_c_prd_prof           ,
            gt_zpra_c_dpr_prof           ,
            gt_zpra_t_mrec_app           ,
            gt_zpra_t_mrec_app_3f        ,
            gt_zpra_t_mrec_app_4a        .
  CLEAR   : gs_zpra_t_dly_prd            ,
            gs_zdpr_gas_combine          ,
            gs_zpra_t_prd_pi             ,
            gs_zpra_t_mrec_prd           ,
            gs_wtd_pi                    ,
            gs_wtd_cf                    ,
            gs_asset_desc                ,
            gs_paste                     ,
            gs_copied_cells              ,
            gs_product_col               ,
            gs_zpra_t_tar_pi             ,
            gs_zpra_t_prd_tar            ,
            gs_zpra_t_tar_cf             ,
            gs_zpra_c_prd_prof           ,
            gs_zpra_c_dpr_prof           ,
            gs_zpra_t_mrec_app           .

  REFRESH : r_product[]                  ,
            r_prd_vl_type[]              ,
            r_combine_asset[]            ,
            r_tar_code[]                 ,
            r_production_date[]          .

  CLEAR   : r_product                    ,
            r_prd_vl_type                ,
            r_combine_asset              ,
            r_tar_code                   ,
            r_production_date            .

  FREE    : gt_dyn_table                 ,
            gt_sec2a1_table              ,
            gt_sec2a2_table              ,
            gt_sec2a3_table              ,
            gt_sec2b_table               ,
            gt_sec2d_table               ,
            gt_sec2e_table               ,
            gt_sec2f_table               ,
            gt_sec3a_table               ,
            gt_sec3b_table               ,
            gt_sec3c_table               ,
            gt_sec3d_table               ,
            gt_sec3e_table               ,
            gt_sec3f_table               ,
            gt_sec4a_table               ,
            gt_sec5a_table               ,
            gs_line                      ,
            gs_line1                     ,
            gs_dyn_fcat                  ,
            gt_dyn_fcat                  .

  CLEAR   : gv_pos                       ,
            gv_len                       ,
            gv_table_columns             ,
            gv_year_start_date           ,
            gv_year_end_date             ,
            gv_current_gjahr             ,
            gv_current_calendar_gjahr    ,
            gv_next_gjahr                ,
            gv_last_gjahr                ,
            gv_5_back_gjahr              ,
            gv_current_monat             ,
            gv_month_name                ,
            gv_last_month_name           ,
            gv_current_month_days        ,
            gv_month_back_datum          ,
            gv_month_back_begin_datum    ,
            gv_search_begin_datum        ,
            gv_month_back_end_datum      ,
            gv_search_end_datum          ,
            gv_month_back_gjahr          ,
            gv_month_back_calendar_gjahr ,
            gv_month_back_monat          ,
            gv_month_begin_datum         ,
            gv_month_end_datum           ,
            gv_year_back_begin_datum     ,
            gv_year_back_end_datum       ,
            gv_total_days_in_year        ,
            gv_days_gone_in_year         ,
            gv_days_left_in_year         ,
            gv_mrec_monat_start          ,
            gv_mrec_monat_end            ,
            gv_mrec_gjahr_start          ,
            gv_mrec_gjahr_end            ,
            gv_individual_mul            ,
            gv_total_mul                 ,
            gv_periv                     ,
            gv_grand_total_desc          ,
            gv_5a_cols                   ,
            gv_5a_rows                   ,
            gv_image_name                .

  UNASSIGN: <gfs_dyn_table>              ,
            <gfs_sec2_table>             ,
            <gfs_sec2a2_table>           ,
            <gfs_sec2a3_table>           ,
            <gfs_sec2b_table>            ,
            <gfs_sec2d_table>            ,
            <gfs_sec2e_table>            ,
            <gfs_sec2f_table>            ,
            <gfs_sec3a_table>            ,
            <gfs_sec3b_table>            ,
            <gfs_sec3c_table>            ,
            <gfs_sec3d_table>            ,
            <gfs_sec3e_table>            ,
            <gfs_sec3f_table>            ,
            <gfs_sec4a_table>            ,
            <gfs_sec5a_table>            ,
            <gfs_dyn_line>               ,
            <gfs_dyn_line2>              ,
            <gfs_dyn_line3>              ,
            <gfs_field>                  ,
            <gfs_field2>                 ,
            <gfs_field3>                 .
  CLEAR :   gv_row                       ,
            gv_col                       ,
            gv_s_row                     ,
            gv_s_col                     ,
            gv_e_row                     ,
            gv_e_col                     ,
            gv_sec1_data_start_row       ,
            gv_sec1_h_start_row          ,
            gv_sec3_h_start_row          ,
            gv_sec3a_start_row           ,
            gv_sec3_end_row              ,
            gv_sec2a_tgt_start_row       ,
            gv_sec2b_start_row           ,
            gv_sec1_lines                ,
            gv_rc                        ,
            gv_txt                       ,
            gv_start_row                 ,
            gv_sheet1_name               ,
            gv_sheet2_name               ,
            gv_sheet3_name               .

  FREE OBJECT:  go_excel                 ,
                go_workbooks             ,
                go_workbook              ,
                go_workbook2             ,
                go_sheets                ,
                go_worksheet             ,
                go_worksheet2            ,
                go_worksheet3            ,
                go_application           ,
                go_pagesetup             ,
                go_cell                  ,
                go_font                  ,
                go_border                ,
                go_range0                ,
                go_range                 ,
                go_column                ,
                go_row                   ,
                go_cell_from             ,
                go_cell_to               ,
                go_interior              ,
                go_charts                ,
                go_chart                 ,
                go_chartobjects          ,
                go_title                 ,
                go_titlechar             ,
                go_axes                  ,
                go_axestitle             ,
                go_legend                .


ENDFORM.
FORM show_progress  USING   p_percent .
  DATA lv_text TYPE char50 .
  CONCATENATE p_percent '% Complete' INTO lv_text .
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
*     PERCENTAGE       = 0
      text = lv_text.

ENDFORM.
FORM get_full_number_format  CHANGING p_number_format.

  p_number_format = '0.000' .

  IF p_bb  IS NOT INITIAL OR
     p_bbd IS NOT INITIAL OR
     p_bmd IS NOT INITIAL .
    p_number_format = '0' .
  ENDIF.
ENDFORM.
FORM add_dummy_data  TABLES   pt_zpra_t_dly_prd STRUCTURE zpra_t_dly_prd.

  DATA : ls_zpra_t_dly_prd TYPE zpra_t_dly_prd .
  DATA : lv_date           TYPE sy-datum       .

  lv_date = gv_month_back_datum .
  APPEND LINES OF gt_zpra_t_dly_prd TO pt_zpra_t_dly_prd .
  DO .
    READ TABLE pt_zpra_t_dly_prd TRANSPORTING NO FIELDS WITH KEY production_date = lv_date .
    IF sy-subrc IS NOT INITIAL.
      ls_zpra_t_dly_prd-mandt           = sy-mandt .
      ls_zpra_t_dly_prd-production_date = lv_date .
      ls_zpra_t_dly_prd-comments        = 'DUMMYDPRDATA' .
      APPEND ls_zpra_t_dly_prd TO pt_zpra_t_dly_prd .
    ENDIF.
    lv_date = lv_date + 1 .
    IF lv_date GT p_date.
      EXIT .
    ENDIF.
  ENDDO.
  SORT pt_zpra_t_dly_prd BY production_date .
ENDFORM.
FORM get_asset_start_date  USING    p_index
                           CHANGING p_start_date.
  DATA : lt_zpra_t_prd_pi TYPE STANDARD TABLE OF ty_zpra_t_prd_pi .
  DATA : ls_zpra_t_prd_pi TYPE ty_zpra_t_prd_pi .

  DATA :  lv_product TYPE zpra_t_dly_prd-product,
          lv_asset   TYPE zpra_t_prd_pi-asset.

  CLEAR p_start_date .

  READ TABLE gt_dyn_fcat INTO gs_dyn_fcat INDEX p_index .
  IF sy-subrc IS INITIAL.
    SPLIT gs_dyn_fcat-fieldname AT '-' INTO lv_product lv_asset .
    IF sy-subrc IS INITIAL.
      IF lv_asset NE 'TOTAL' AND lv_asset NE 'COMBINE' .
        lt_zpra_t_prd_pi = gt_zpra_t_prd_pi .
        DELETE lt_zpra_t_prd_pi WHERE asset NE lv_asset .
        SORT lt_zpra_t_prd_pi BY prod_start_date .
        READ TABLE lt_zpra_t_prd_pi INTO ls_zpra_t_prd_pi WITH KEY asset = lv_asset .
        IF sy-subrc IS INITIAL.
          p_start_date = ls_zpra_t_prd_pi-prod_start_date .
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
FORM get_target_start_date  USING    p_index
                                     p_tar_code
                           CHANGING p_start_date.

  DATA :  lv_product TYPE zpra_t_dly_prd-product,
          lv_asset   TYPE zpra_t_prd_pi-asset.

  CLEAR p_start_date .

  READ TABLE gt_dyn_fcat INTO gs_dyn_fcat INDEX p_index .
  IF sy-subrc IS INITIAL.
    SPLIT gs_dyn_fcat-fieldname AT '-' INTO lv_product lv_asset .
    IF sy-subrc IS INITIAL.
      IF lv_asset NE 'TOTAL' AND lv_asset NE 'COMBINE' .

        READ TABLE gt_tar_start_dates INTO gs_tar_start_dates WITH KEY asset = lv_asset
                                                                    tar_code = p_tar_code BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          p_start_date = gs_tar_start_dates-vld_frm .
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
FORM free_clipboard .
  REFRESH gt_paste .
  CALL METHOD cl_gui_frontend_services=>clipboard_export
    IMPORTING
      data         = gt_paste
    CHANGING
      rc           = gv_rc
    EXCEPTIONS
      cntl_error   = 1
      error_no_gui = 2
      OTHERS       = 4.

ENDFORM.
FORM populate_no_data_entries  TABLES p_zpra_t_dly_prd STRUCTURE zpra_t_dly_prd
                               USING  p_from_date p_to_date.

  DATA : lt_zpra_c_prd_prof   TYPE STANDARD TABLE OF ty_zpra_c_prd_prof .
  DATA : ls_zpra_c_prd_prof TYPE                   ty_zpra_c_prd_prof,
         ls_zpra_t_dly_prd  TYPE                   zpra_t_dly_prd,
         ls_zpra_t_dly_prd2 TYPE                   zpra_t_dly_prd.
  DATA : lv_date  TYPE                   sy-datum,
         lv_date2 TYPE                   sy-datum,
         lv_index TYPE                   sy-tabix.

  lt_zpra_c_prd_prof = gt_zpra_c_prd_prof .
  SORT lt_zpra_c_prd_prof BY product asset block .
  DELETE ADJACENT DUPLICATES FROM  lt_zpra_c_prd_prof COMPARING product asset .

  SORT p_zpra_t_dly_prd BY product asset production_date block prd_vl_type DESCENDING.

  LOOP AT lt_zpra_c_prd_prof INTO ls_zpra_c_prd_prof.
    lv_date = p_from_date .
    DO .
      IF lv_date GT p_to_date.
        EXIT .
      ENDIF.
      READ TABLE p_zpra_t_dly_prd INTO ls_zpra_t_dly_prd WITH KEY product         = ls_zpra_c_prd_prof-product
                                                                  asset           = ls_zpra_c_prd_prof-asset
                                                                  production_date = lv_date BINARY SEARCH .
      IF sy-subrc IS NOT INITIAL.
        lv_date2 = lv_date - 1.
        READ TABLE p_zpra_t_dly_prd INTO ls_zpra_t_dly_prd WITH KEY product         = ls_zpra_c_prd_prof-product
                                                                    asset           = ls_zpra_c_prd_prof-asset
                                                                    production_date = lv_date2 BINARY SEARCH .
        IF sy-subrc IS INITIAL .
          READ TABLE p_zpra_t_dly_prd INTO ls_zpra_t_dly_prd WITH KEY product         = ls_zpra_c_prd_prof-product
                                                                      asset           = ls_zpra_c_prd_prof-asset
                                                                      production_date = lv_date2
                                                                      block           = ls_zpra_c_prd_prof-block .
          IF sy-subrc IS INITIAL.
            lv_index = sy-tabix .
            LOOP AT p_zpra_t_dly_prd INTO ls_zpra_t_dly_prd FROM lv_index.
              IF ls_zpra_t_dly_prd-product          NE ls_zpra_c_prd_prof-product OR
                 ls_zpra_t_dly_prd-asset            NE ls_zpra_c_prd_prof-asset   OR
                 ls_zpra_t_dly_prd-production_date  NE lv_date2                   .
                EXIT .
              ENDIF.
              ls_zpra_t_dly_prd-production_date = lv_date .
              APPEND ls_zpra_t_dly_prd TO p_zpra_t_dly_prd .
            ENDLOOP.
            SORT p_zpra_t_dly_prd BY product asset production_date block prd_vl_type DESCENDING.
          ENDIF.
        ENDIF.
      ENDIF.
      lv_date = lv_date + 1 .
    ENDDO.
  ENDLOOP.
ENDFORM.
FORM display_run_date_time .
  DATA : lv_date_ext TYPE char20,
         lv_time_ext TYPE char8.

  CALL FUNCTION 'CONVERSION_EXIT_SDATE_OUTPUT'
    EXPORTING
      input  = sy-datum
    IMPORTING
      output = lv_date_ext.
  CONCATENATE sy-uzeit(2) ':' sy-uzeit+2(2) ':' sy-uzeit+4(2) INTO lv_time_ext .
  CONCATENATE 'Report Generated On:' lv_date_ext
              'At'                   lv_time_ext
         INTO gv_txt SEPARATED BY space .

  gv_s_row = gv_row + 3.
  gv_s_col = 1 .
  gv_e_row = gv_s_row .
  gv_e_col = 5.

  PERFORM select_range USING gv_s_row gv_s_col gv_e_row gv_e_col  .
  CALL METHOD OF go_range 'Merge' .

  PERFORM set_range USING gv_txt 0.
  PERFORM set_range_formatting USING  0 'L' 'C' .
  PERFORM set_range_font  USING 9 0 .

ENDFORM.
FORM remove_expired_blocks  TABLES   p_zpra_c_prd_prof STRUCTURE gs_zpra_c_prd_prof
                            USING    p_from_date p_to_date.

  DATA : ls_zpra_c_prd_prof TYPE ty_zpra_c_prd_prof .
  DATA : lv_index TYPE sy-tabix .
  REFRESH gt_prod_start_end_dates .
  SELECT asset
         block
         vld_frm
         vld_to
         prod_start_date
         liscense_exp_dt
    FROM zpra_t_prd_pi
    INTO TABLE gt_prod_start_end_dates
     FOR ALL ENTRIES IN p_zpra_c_prd_prof
   WHERE asset EQ p_zpra_c_prd_prof-asset
     AND block EQ p_zpra_c_prd_prof-block .

  SORT gt_prod_start_end_dates BY asset ASCENDING block ASCENDING vld_to DESCENDING .
  DELETE ADJACENT DUPLICATES FROM gt_prod_start_end_dates COMPARING asset block .
  LOOP AT p_zpra_c_prd_prof INTO ls_zpra_c_prd_prof.
    lv_index = sy-tabix .
    READ TABLE gt_prod_start_end_dates INTO gs_prod_start_end_dates WITH KEY asset = ls_zpra_c_prd_prof-asset
                                                                             block = ls_zpra_c_prd_prof-block BINARY SEARCH .
    IF sy-subrc IS INITIAL.
      IF gs_prod_start_end_dates-prod_start_date IS INITIAL.
        gs_prod_start_end_dates-prod_start_date = '00000000' .
      ENDIF.
      IF gs_prod_start_end_dates-liscense_exp_dt IS INITIAL.
        gs_prod_start_end_dates-liscense_exp_dt = '99991231' .
      ENDIF.
      IF gs_prod_start_end_dates-liscense_exp_dt    LT p_from_date OR
         gs_prod_start_end_dates-prod_start_date GT p_to_date .
        DELETE p_zpra_c_prd_prof INDEX lv_index .
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.
FORM convert_gas_rprd_to_boe  CHANGING p_zpra_t_dly_rprd TYPE zpra_t_dly_rprd.

  p_zpra_t_dly_rprd-jv_rcn_vl_qty3   = p_zpra_t_dly_rprd-jv_rcn_vl_qty3 * 6290 .
  p_zpra_t_dly_rprd-ovl_rcn_vl_qty3  = p_zpra_t_dly_rprd-ovl_rcn_vl_qty3 * 6290 .

ENDFORM.
FORM get_cf_from_date  USING    p_rec_date
                                p_product
                                p_asset
                                p_block
                       CHANGING p_cf.

  DATA : lv_monat TYPE t009b-poper,
         lv_gjahr TYPE t009b-bdatj.

  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      i_date         = p_rec_date
    " I_MONMIT       = 00
      i_periv        = gv_periv
    IMPORTING
      e_buper        = lv_monat
      e_gjahr        = lv_gjahr
    EXCEPTIONS
      input_false    = 01
      t009_notfound  = 02
      t009b_notfound = 03.

  IF sy-subrc IS INITIAL.
    READ TABLE gt_cf INTO gs_cf WITH KEY gjahr = lv_gjahr
                                       product = p_product
                                         asset = p_asset
                                         block = p_block BINARY SEARCH .
    IF sy-subrc IS INITIAL.
      p_cf = gs_cf-conv_factor .

    ENDIF.
  ENDIF.
ENDFORM.
FORM convert_non_gas_units_5a  CHANGING p_zpra_t_dly_prd TYPE zpra_t_dly_prd.

  DATA : lv_cf    TYPE zpra_t_tar_cf-conv_factor,
         lv_poper TYPE t009b-poper,
         lv_monat TYPE zpra_t_mrec_prd-monat,
         lv_gjahr TYPE zpra_t_mrec_prd-gjahr.

  CHECK p_zpra_t_dly_prd-product NE c_prod_gas .

  CALL FUNCTION 'DATE_TO_PERIOD_CONVERT'
    EXPORTING
      i_date         = p_zpra_t_dly_prd-production_date
    " I_MONMIT       = 00
      i_periv        = gv_periv
    IMPORTING
      e_buper        = lv_poper
      e_gjahr        = lv_gjahr
    EXCEPTIONS
      input_false    = 01
      t009_notfound  = 02
      t009b_notfound = 03.

  IF sy-subrc <> 0.
    MESSAGE 'Error getting Period' TYPE 'E' .
  ENDIF.
  lv_monat = lv_poper+1(2) .
  CLEAR gs_zpra_t_mrec_prd .
  LOOP AT gt_zpra_t_mrec_prd_5a INTO gs_zpra_t_mrec_prd WHERE product     EQ p_zpra_t_dly_prd-product
                                                          AND asset       EQ p_zpra_t_dly_prd-asset
                                                          AND block       EQ p_zpra_t_dly_prd-block
                                                          AND prd_vl_type EQ p_zpra_t_dly_prd-prd_vl_type .
    IF gs_zpra_t_mrec_prd-gjahr LT lv_gjahr.
      EXIT .
    ELSEIF gs_zpra_t_mrec_prd-gjahr EQ lv_gjahr AND
           gs_zpra_t_mrec_prd-monat LE lv_monat .
      EXIT.
    ENDIF.
  ENDLOOP.
  CASE abap_true.
    WHEN p_bb.
    WHEN p_bbd.
    WHEN p_tm.
      IF gs_zpra_t_mrec_prd-prod_vl_qty2 IS NOT INITIAL.
        lv_cf = gs_zpra_t_mrec_prd-prod_vl_qty1 /  gs_zpra_t_mrec_prd-prod_vl_qty2 .
      ELSE.
        PERFORM get_cf_from_date USING  p_zpra_t_dly_prd-production_date
                                        p_zpra_t_dly_prd-product
                                        p_zpra_t_dly_prd-asset
                                        p_zpra_t_dly_prd-block
                               CHANGING lv_cf.
      ENDIF.
      IF lv_cf IS NOT INITIAL.
        p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / lv_cf .
      ENDIF.
    WHEN p_tmd.
      IF gs_zpra_t_mrec_prd-prod_vl_qty2 IS NOT INITIAL.
        lv_cf = gs_zpra_t_mrec_prd-prod_vl_qty1 /  gs_zpra_t_mrec_prd-prod_vl_qty2 .
      ELSE.
        PERFORM get_cf_from_date USING  p_zpra_t_dly_prd-production_date
                                        p_zpra_t_dly_prd-product
                                        p_zpra_t_dly_prd-asset
                                        p_zpra_t_dly_prd-block
                               CHANGING lv_cf.
      ENDIF.
      IF lv_cf IS NOT INITIAL.
        p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / lv_cf .
      ENDIF.
    WHEN p_mb .
      IF gs_zpra_t_mrec_prd-prod_vl_qty2 IS NOT INITIAL.
        lv_cf = gs_zpra_t_mrec_prd-prod_vl_qty1 /  gs_zpra_t_mrec_prd-prod_vl_qty2 .
      ELSE.
        PERFORM get_cf_from_date USING  p_zpra_t_dly_prd-production_date
                                        p_zpra_t_dly_prd-product
                                        p_zpra_t_dly_prd-asset
                                        p_zpra_t_dly_prd-block
                               CHANGING lv_cf.
      ENDIF.
      IF lv_cf IS NOT INITIAL.
        p_zpra_t_dly_prd-prod_vl_qty1 = p_zpra_t_dly_prd-prod_vl_qty1 / lv_cf .
      ENDIF.
    WHEN p_bmd.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
FORM POPULATE_NO_DATA_ENTRIES_3F  TABLES p_zpra_t_dly_prd STRUCTURE zpra_t_dly_prd
                               USING  p_from_date p_to_date.

   DATA : lt_zpra_c_prd_prof   TYPE STANDARD TABLE OF ty_zpra_c_prd_prof .
  DATA : ls_zpra_c_prd_prof TYPE                   ty_zpra_c_prd_prof,
         ls_zpra_t_dly_prd  TYPE                   zpra_t_dly_prd,
         ls_zpra_t_dly_prd2 TYPE                   zpra_t_dly_prd.
  DATA : lv_date  TYPE                   sy-datum,
         lv_date2 TYPE                   sy-datum,
         lv_index TYPE                   sy-tabix.

  lt_zpra_c_prd_prof = gt_zpra_c_prd_prof .
  SORT lt_zpra_c_prd_prof BY product asset block .
  DELETE ADJACENT DUPLICATES FROM  lt_zpra_c_prd_prof COMPARING product asset .

  SORT p_zpra_t_dly_prd BY product asset production_date block prd_vl_type DESCENDING.

  LOOP AT lt_zpra_c_prd_prof INTO ls_zpra_c_prd_prof.
    clear GS_ZPRA_T_PRD_PI.
    READ TABLE GT_ZPRA_T_PRD_PI_3F INTO GS_ZPRA_T_PRD_PI WITH KEY  asset = ls_zpra_c_prd_prof-asset
                                                                block = ls_zpra_c_prd_prof-block .
    IF SY-SUBRC = 0.
     IF GS_ZPRA_T_PRD_PI-PROD_START_DATE gt p_from_date .
      CONTINUE.
     ENDIF.
    ENDIF.
    lv_date = p_from_date .
    DO .
      IF lv_date GT p_to_date.
        EXIT .
      ENDIF.
      READ TABLE p_zpra_t_dly_prd INTO ls_zpra_t_dly_prd WITH KEY product         = ls_zpra_c_prd_prof-product
                                                                  asset           = ls_zpra_c_prd_prof-asset
                                                                  production_date = lv_date BINARY SEARCH .
      IF sy-subrc IS NOT INITIAL.
        lv_date2 = lv_date - 1.
        READ TABLE p_zpra_t_dly_prd INTO ls_zpra_t_dly_prd WITH KEY product         = ls_zpra_c_prd_prof-product
                                                                    asset           = ls_zpra_c_prd_prof-asset
                                                                    production_date = lv_date2 BINARY SEARCH .
        IF sy-subrc IS INITIAL .
          READ TABLE p_zpra_t_dly_prd INTO ls_zpra_t_dly_prd WITH KEY product         = ls_zpra_c_prd_prof-product
                                                                      asset           = ls_zpra_c_prd_prof-asset
                                                                      production_date = lv_date2
                                                                      block           = ls_zpra_c_prd_prof-block .
          IF sy-subrc IS INITIAL.
            lv_index = sy-tabix .
            LOOP AT p_zpra_t_dly_prd INTO ls_zpra_t_dly_prd FROM lv_index.
              IF ls_zpra_t_dly_prd-product          NE ls_zpra_c_prd_prof-product OR
                 ls_zpra_t_dly_prd-asset            NE ls_zpra_c_prd_prof-asset   OR
                 ls_zpra_t_dly_prd-production_date  NE lv_date2                   .
                EXIT .
              ENDIF.
              ls_zpra_t_dly_prd-production_date = lv_date .
              APPEND ls_zpra_t_dly_prd TO p_zpra_t_dly_prd .
              insert ZPRA_DLY_PRD_ND FROM ls_zpra_t_dly_prd.
            ENDLOOP.
            SORT p_zpra_t_dly_prd BY product asset production_date block prd_vl_type DESCENDING.
          ENDIF.
        ENDIF.
      ENDIF.
      lv_date = lv_date + 1 .
    ENDDO.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PREPARE_PASTE_DATA
*& Generic clipboard paste preparation - works with any dynamic table
*&---------------------------------------------------------------------*
FORM prepare_paste_data TABLES p_table.
  FIELD-SYMBOLS: <fs_line> TYPE ANY,
                 <fs_fld>  TYPE ANY.
  DATA lv_index TYPE sy-tabix.
  CLEAR gs_paste.
  REFRESH gt_paste.
  LOOP AT p_table ASSIGNING <fs_line>.
    CLEAR gs_paste.
    DO.
      lv_index = sy-index.
      ASSIGN COMPONENT lv_index OF STRUCTURE <fs_line> TO <fs_fld>.
      IF sy-subrc IS INITIAL.
        IF lv_index NE 1.
          CONCATENATE gs_paste cl_abap_char_utilities=>horizontal_tab INTO gs_paste.
        ENDIF.
        CONCATENATE gs_paste <fs_fld> INTO gs_paste.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
    APPEND gs_paste TO gt_paste.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_DYNAMIC_TABLE
*& Creates a dynamic table from gt_dyn_fcat into the given reference
*&---------------------------------------------------------------------*
FORM create_dynamic_table CHANGING p_table TYPE REF TO data.
  CALL METHOD cl_alv_table_create=>create_dynamic_table
    EXPORTING
      it_fieldcatalog           = gt_dyn_fcat
    IMPORTING
      ep_table                  = p_table
    EXCEPTIONS
      generate_subpool_dir_full = 1
      OTHERS                    = 2.
  IF sy-subrc NE 0.
    MESSAGE 'Unexpected Internal Error' TYPE 'E'.
  ENDIF.
ENDFORM.
