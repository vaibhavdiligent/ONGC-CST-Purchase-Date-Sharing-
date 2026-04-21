*&
*&  Include           ZPRA_DPR_REPORT_TOP
*&
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
       gt_wtd_pi                    TYPE STANDARD TABLE OF ty_wtd_pi   ,
       gt_wtd_cf                    TYPE STANDARD TABLE OF ty_wtd_cf,
       gt_asset_desc                TYPE STANDARD TABLE OF ty_asset_desc ,
       gt_paste                     TYPE STANDARD TABLE OF ty_paste  ,
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
       gs_wtd_pi                    TYPE                   ty_wtd_pi    ,
       gs_wtd_cf                    TYPE                   ty_wtd_cf ,
       gs_asset_desc                TYPE                   ty_asset_desc  ,
       gs_paste                     TYPE                   ty_paste    ,
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
