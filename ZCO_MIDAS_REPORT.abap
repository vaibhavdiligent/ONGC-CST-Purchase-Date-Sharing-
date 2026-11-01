*&---------------------------------------------------------------------*
*& Report  ZCO_MIDAS_REPORT
*&
*&---------------------------------------------------------------------*
* Title           : Costing table update
* Author          : Parashar Khedekar
* Program Type    : Executable
* Creation Date   : 10.04.2019
* Transaction     : ZCO_MIDAS_REPORT
* Functional design: Shashank sharma
* Technical design: <>
* Change request : 8000005475
* Transport request: CRDK958464
* Description     : Midas project 2-3
*&---------------------------------------------------------------------*
*Modification Information
* Date            : 13.09.2019
* Author          : Parashar
* Functional      : Rahul.M
* Change request  : 8000005475
* Transport number: CRDK964595
* Description     :Project -MIDAS 2 and 3
*&---------------------------------------------------------------------*
*Modification Information
* Date            : 06.10.2019
* Author          : Parashar
* Functional      : Rahul.M
* Change request  : 8000005475
* Transport number: CRDK965429
* Description     :Project -MIDAS 2 and 3
*&---------------------------------------------------------------------*
*Modification Information
* Date            : 13.11.2019
* Author          : Parashar
* Functional      : Rahul.M
* Change request  : 8000005475
* Transport number: CRDK965579
* Description     :Project -MIDAS 2 and 3
*&---------------------------------------------------------------------*
*Modification Information
* Date            : 30.07.2020
* Author          : Gajanan Patil
* Functional      : Bhakti Joshi
* Change request  : 8000006372
* Transport number: CRDK972310
* Description     : Hypercare Reprt for Midas2
*&---------------------------------------------------------------------*
*Modification Information
* Date            : 01.06.2021
* Author          : Sushil Pandey
* Functional      : Bhakti Joshi
* Change request  : 8000007120
* Transport number: CRDK978246
* Description     : MIDAS_7
*&---------------------------------------------------------------------*
*Modification Information
* Date            : 20.08.2021
* Author          : Sushil Pandey
* Functional      : Bhakti Joshi
* Change request  : 8000007312
* Transport number: CRDK979701
* Description     : MIDAS2_8
*&---------------------------------------------------------------------*
*Modification Information
* Date            : 30.03.2022
* Author          : Pooja Adupala/Gajanan Patil
* Functional      : Shashank Sharma
* Change request  : 8000007907
* Transport number: CRDK982491
* Description     : Midas 3 ( BU P&L Automation )
*&---------------------------------------------------------------------*
*Modification Information
* Date            : 11.11.2022
* Author          : Pooja Adupala/Gajanan Patil
* Functional      : Shashank Sharma
* Change request  : 8000007907
* Transport number: CRDK985241
* Description     : Midas 3 ( BU P&L Automation )
*&---------------------------------------------------------------------*
*Modification Information
* Date            : 07.11.2023
* Author          : Prathamesh Parab
* Functional      : Shashank Sharma/ Sai Sampath
* Change request  : 8000009287/8000003020
* Transport number: CRDK988689
* Description     :  SAP development for data flow into SAC;
*                    SD part changes for Bonus quantities and IP Code
*&---------------------------------------------------------------------*
*Modification Information
* Date            : 18.10.2024
* Author          : Gajanan Patil
* Functional      : Arijit Poder
* Change request  : 8000003569
* Transport number:
* Description     : Logic change
*&---------------------------------------------------------------------*
* Modification Information
* Date            : 01.11.2026
* Description     : CRS job ZCO_MIDAS_REPORT cancelled after ~300 sec.
*                   Memory / runtime: ZCO_PGI_RATE read with field list,
*                   duplicate-free FOR ALL ENTRIES drivers (material /
*                   company code+material), linear line_exists scans and
*                   copy-and-filter loops (14.x / 16.x logic) replaced by
*                   sorted processing - same results; BINARY SEARCH on
*                   T001K. Marked **BEGIN/END OF CHANGE 01.11.2026.
*----------------------------------------------------------------------*
REPORT zco_midas_report.

TABLES:zco_midas.
* **BEGIN OF CHANGE 01.11.2026 - performance / memory (CRS job cancelled)
TYPES: tt_pgi_rate TYPE STANDARD TABLE OF zco_pgi_rate WITH DEFAULT KEY,
       BEGIN OF ty_matnr_cnt,
         matnr TYPE zco_pgi_rate-matnr,
         cnt   TYPE i,
       END OF ty_matnr_cnt,
       tt_matnr_cnt TYPE STANDARD TABLE OF ty_matnr_cnt WITH DEFAULT KEY,
       BEGIN OF ty_drv_mat,
         matnr TYPE zco_midas-matnr,
       END OF ty_drv_mat,
       BEGIN OF ty_drv_ccm,
         vkorg TYPE zco_midas-vkorg,
         matnr TYPE zco_midas-matnr,
       END OF ty_drv_ccm,
       BEGIN OF ty_drv_cc,
         vkorg TYPE zco_midas-vkorg,
       END OF ty_drv_cc.
DATA: lt_drv_mat TYPE STANDARD TABLE OF ty_drv_mat WITH DEFAULT KEY,
      lt_drv_ccm TYPE STANDARD TABLE OF ty_drv_ccm WITH DEFAULT KEY,
      lt_drv_cc  TYPE STANDARD TABLE OF ty_drv_cc  WITH DEFAULT KEY,
      lt_cnt     TYPE tt_matnr_cnt,
      ls_cnt     TYPE ty_matnr_cnt.
* **END OF CHANGE 01.11.2026

TYPES : BEGIN OF ty_final,
          spmon            TYPE zco_midas-spmon,
          ent_tag          TYPE zco_midas-ent_tag,
          vkorg            TYPE zco_midas-vkorg,
          vtweg            TYPE zco_midas-vtweg,
          matnr            TYPE zco_midas-matnr,
          locl_prd         TYPE zco_midas-locl_prd,
          werks            TYPE zco_midas-werks,
          kdgrp            TYPE zco_midas-kdgrp,
          sub_zone         TYPE zco_midas-sub_zone,
          katr4            TYPE zco_midas-katr4,
          zone_d           TYPE zco_midas-zone_d,
          plawe            TYPE zco_midas-plawe,
          plaag_01         TYPE zco_midas-plaag_01,
          sakn1            TYPE zco_midas-sakn1,
          spara            TYPE zco_midas-spara,
          fkart            TYPE zco_midas-fkart,
          spart            TYPE zco_midas-spart,
          doc_cur          TYPE zco_midas-doc_cur,
          usd_curr         TYPE zco_midas-usd_curr,
          b_fkimg          TYPE zco_midas-b_fkimg,
          basme            TYPE zco_midas-basme,
*          m_inr       TYPE netwr,
          remark(128)      TYPE c,
          kadky            TYPE keko-kadky,
          cost_plant       TYPE keko-werks,
          kalnr            TYPE keko-kalnr,
          kalka            TYPE keko-kalka,
          tvers            TYPE keko-tvers,
          bwvar            TYPE keko-bwvar,
          kokrs            TYPE keko-kokrs,
          verid            TYPE keko-verid,
          stnum            TYPE keko-stnum,
          stlan            TYPE keko-stlan,
          stalt            TYPE keko-stalt,
          losgr            TYPE keko-losgr,
          meins            TYPE keko-meins,
          feh_sta          TYPE keko-feh_sta,
          klvar            TYPE keko-klvar,
          poper            TYPE keko-poper,
          bdatj            TYPE keko-bdatj,
          mat_cost         TYPE keph-kst001,
          api_vc           TYPE keph-kst001,
          api_fc           TYPE keph-kst001,
          form_vc          TYPE keph-kst001,
          form_fc          TYPE keph-kst001,
          form_fc_y        TYPE keph-kst001,
          sub_margin       TYPE keph-kst001,
          edep             TYPE keph-kst001,
          edns             TYPE keph-kst001,
          total            TYPE keph-kst001,
          total_cost       TYPE keph-kst001, " LOC added by Sushil as on 02.06.2021
          cost_curr        TYPE zco_midas-cost_curr,
          inr_rate         TYPE zco_midas-inr_rate,
          mat_value        TYPE zco_midas-mat_value,
          frieght          TYPE zco_midas-frieght,
          insaurance       TYPE zco_midas-insaurance,
          skfbp            TYPE zco_midas-skfbp,
          bukrs            TYPE t001-bukrs,
          waers            TYPE t001-waers,
          stand_value      TYPE zco_midas-stand_value,
*  *        "Start of changes by Pooja Adupala on 30.03.2022 for CR#8000007907
          app_mat          TYPE zco_midas-app_mat,
          sales_grp        TYPE zco_midas-sales_grp,
          add_drill_down   TYPE zco_midas-add_drill_down,
          zcluster         TYPE zco_midas-zcluster,
          sub_div_code     TYPE zco_midas-sub_div_code,
          div_code         TYPE zco_midas-div_code,
          launch_year      TYPE zco_midas-launch_year,
          respi_prod       TYPE zco_midas-respi_prod,
          final_tag        TYPE zco_midas-final_tag,
          add_drill_down2  TYPE zco_midas-add_drill_down2,
          sales_cc_curr    TYPE zco_midas-sales_cc_curr,
          mat_cost_cc_curr TYPE zco_midas-mat_cost_cc_curr,
          api_vc_cc_curr   TYPE zco_midas-api_vc_cc_curr,
          api_fc_cc_curr   TYPE zco_midas-api_fc_cc_curr,
          for_vc_cc_curr   TYPE zco_midas-for_vc_cc_curr,
          for_fc_cc_curr   TYPE zco_midas-for_fc_cc_curr,
**         "End of changes  by Pooja Adupala on 30.03.2022 for CR#8000007907

**        "SOA by Lalitha on 06.02.2024 for CR#8000009287
          excl_qty         TYPE zexcl_bonus,
          bonus_qty        TYPE zsd_bonus_qty,
          bonus_ret_qty    TYPE zsd_bonus_ret_qty,
          ip_code          TYPE zip_code,
**        "EOA by Lalitha on 06.02.2024 for CR#8000009287

        END OF ty_final.

"Start of changes  by Pooja Adupala on 08.09.2022 for CR#8000007907
TYPES: BEGIN OF ty_pgi_rate,
         matnr  TYPE zco_pgi_rate-matnr,
         rbukrs TYPE zco_pgi_rate-rbukrs,
       END OF ty_pgi_rate.
"End of changes  by Pooja Adupala on 08.09.2022 for CR#8000007907
TYPES: BEGIN OF ty_keko,
         matnr   TYPE keko-matnr,
         werks   TYPE keko-werks,
         kadat   TYPE keko-kadat,
         bidat   TYPE keko-bidat,
         kadky   TYPE keko-kadky,
         kalnr   TYPE keko-kalnr,
         kalka   TYPE keko-kalka,
         tvers   TYPE keko-tvers,
         kokrs   TYPE keko-kokrs,
         verid   TYPE keko-verid,
         stnum   TYPE keko-stnum,
         stlan   TYPE keko-stlan,
         stalt   TYPE keko-stalt,
         losgr   TYPE keko-losgr,
         meins   TYPE keko-meins,
         feh_sta TYPE keko-feh_sta,
         klvar   TYPE keko-klvar,
         poper   TYPE keko-poper,
         bdatj   TYPE keko-bdatj,
         bwvar   TYPE keko-bwvar,
         bukrs   TYPE bukrs,
       END OF ty_keko.

TYPES: BEGIN OF ty_zco_midas,
         spmon            TYPE zco_midas-spmon,
         ent_tag          TYPE zco_midas-ent_tag,
         vkorg            TYPE zco_midas-vkorg,
         vtweg            TYPE zco_midas-vtweg,
         matnr            TYPE zco_midas-matnr,
         locl_prd         TYPE zco_midas-locl_prd,
         werks            TYPE zco_midas-werks,
         kdgrp            TYPE zco_midas-kdgrp,
         sub_zone         TYPE zco_midas-sub_zone,
         katr4            TYPE zco_midas-katr4,
         zone_d           TYPE zco_midas-zone_d,
         plawe            TYPE zco_midas-plawe,
         plaag_01         TYPE zco_midas-plaag_01,
         sakn1            TYPE zco_midas-sakn1,
         spara            TYPE zco_midas-spara,
         fkart            TYPE zco_midas-fkart,
         spart            TYPE zco_midas-spart,
         doc_cur          TYPE zco_midas-doc_cur,
         usd_curr         TYPE zco_midas-usd_curr,
         b_fkimg          TYPE zco_midas-b_fkimg,
         basme            TYPE zco_midas-basme,
*         bukrs            TYPE bukrs,"LOC commeneted by Pooja Adupala as on 30.03.2022 for CR#8000007907
**         "Start of changes by Pooja Adupala on 30.03.2022 for CR#8000007907
         app_mat          TYPE zco_midas-app_mat,
         sales_grp        TYPE zco_midas-sales_grp,
         add_drill_down   TYPE zco_midas-add_drill_down,
         zcluster         TYPE zco_midas-zcluster,
         sub_div_code     TYPE zco_midas-sub_div_code,
         div_code         TYPE zco_midas-div_code,
         launch_year      TYPE zco_midas-launch_year,
         respi_prod       TYPE zco_midas-respi_prod,
         final_tag        TYPE zco_midas-final_tag,
         add_drill_down2  TYPE zco_midas-add_drill_down2,
         sales_cc_curr    TYPE zco_midas-sales_cc_curr,
         mat_cost_cc_curr TYPE zco_midas-mat_cost_cc_curr,
         api_vc_cc_curr   TYPE zco_midas-api_vc_cc_curr,
         api_fc_cc_curr   TYPE zco_midas-api_fc_cc_curr,
         for_vc_cc_curr   TYPE zco_midas-for_vc_cc_curr,
         for_fc_cc_curr   TYPE zco_midas-for_fc_cc_curr,
**         "End of changes  by Pooja Adupala on 30.03.2022 for CR#8000007907

**        "SOA by Lalitha on 06.02.2024 for CR#8000009287
         excl_qty         TYPE zexcl_bonus,
         bonus_qty        TYPE zsd_bonus_qty,
         bonus_ret_qty    TYPE zsd_bonus_ret_qty,
         ip_code          TYPE zip_code,
**        "EOA by Lalitha on 06.02.2024 for CR#8000009287

         bukrs            TYPE bukrs, "LOC added by Pooja Adupala as on 30.03.2022 for CR#8000007907

       END OF ty_zco_midas.


TYPES : BEGIN OF ty_zco_trd,
*          spmon TYPE zco_trd-spmon,
          bukrs TYPE zco_trd-bukrs,
*          mblnr TYPE zco_trd-mblnr,
          matnr TYPE zco_trd-matnr,
          bamng TYPE zco_trd-bamng,
          dmbtr TYPE zco_trd-dmbtr,
        END OF ty_zco_trd.

*** Added by parashar CRDK965579 8000005475 13.11.2019
TYPES : BEGIN OF ty_zco_trd1,
          spmon TYPE zco_trd-spmon,
          bukrs TYPE zco_trd-bukrs,
*          mblnr TYPE zco_trd-mblnr,
          matnr TYPE zco_trd-matnr,
          bamng TYPE zco_trd-bamng,
          dmbtr TYPE zco_trd-dmbtr,
        END OF ty_zco_trd1.
*** Added upto by parashar CRDK965579 8000005475 13.11.2019
TYPES: BEGIN OF ty_keph,
         kalnr  TYPE keph-kalnr,
         kalka  TYPE keph-kalka,
         kadky  TYPE keph-kadky,
         tvers  TYPE keph-tvers,
         bwvar  TYPE keph-bwvar,
         keart  TYPE keph-keart,
         kst001 TYPE keph-kst001,
         kst002 TYPE keph-kst002,
         kst003 TYPE keph-kst003,
         kst004 TYPE keph-kst004,
         kst005 TYPE keph-kst005,
         kst006 TYPE keph-kst006,
         kst007 TYPE keph-kst007,
         kst008 TYPE keph-kst008,
         kst009 TYPE keph-kst009,
         kst010 TYPE keph-kst010,
         kst011 TYPE keph-kst011,
         kst012 TYPE keph-kst012,
         kst013 TYPE keph-kst013,
         kst014 TYPE keph-kst014,
         kst015 TYPE keph-kst015,
         kst016 TYPE keph-kst016,
         kst017 TYPE keph-kst017,
         kst018 TYPE keph-kst018,
         kst019 TYPE keph-kst019,
         kst020 TYPE keph-kst020,
         kst021 TYPE keph-kst021,
         kst022 TYPE keph-kst022,
         kst023 TYPE keph-kst023,
         kst024 TYPE keph-kst024,
         kst025 TYPE keph-kst025,
         kst026 TYPE keph-kst026,
         kst027 TYPE keph-kst027,
         kst028 TYPE keph-kst028,
         kst029 TYPE keph-kst029,
         kst030 TYPE keph-kst030,
         kst031 TYPE keph-kst031,
         kst032 TYPE keph-kst032,
         kst033 TYPE keph-kst033,
         kst034 TYPE keph-kst034,
         kst035 TYPE keph-kst035,
         kst036 TYPE keph-kst036,
         kst037 TYPE keph-kst037,
         kst038 TYPE keph-kst038,
         kst039 TYPE keph-kst039,
         kst040 TYPE keph-kst040,
       END OF ty_keph.

TYPES : BEGIN OF ty_tcurr,
          kurst TYPE tcurr-kurst,
          fcurr TYPE tcurr-fcurr,
          ukurs TYPE tcurr-ukurs,
        END OF ty_tcurr.

TYPES: BEGIN OF ty_t001,
         bukrs TYPE t001-bukrs,
         waers TYPE t001-waers,
       END OF ty_t001.

TYPES: BEGIN OF ty_setleaf,
         setclass TYPE setleaf-setclass,
         setname  TYPE setleaf-setname,
         valfrom  TYPE setleaf-valfrom,
         fcurr    TYPE tcurr-fcurr,
       END OF ty_setleaf,

       BEGIN OF ty_tcurr1,
         kurst TYPE tcurr-kurst,
         fcurr TYPE tcurr-fcurr,
         tcurr TYPE tcurr-tcurr,
         ukurs TYPE tcurr-ukurs,
*         ffact TYPE tcurr-ffact,
       END OF ty_tcurr1,

       BEGIN OF ty_excrate,
         fcurr     TYPE tcurr-fcurr,
         exch_rate TYPE tcurr-ukurs,
       END OF ty_excrate.


DATA :               ta_zco_midas               TYPE TABLE OF ty_zco_midas,
                     wa_zco_midas               TYPE ty_zco_midas,
                     "SOA by pooja on 10.11.2022
                     wa_tcurr_ukurs1            TYPE ty_tcurr1,
                     ta_tcurr_ukurs1            TYPE TABLE OF ty_tcurr1,
                     wa_excrate                 TYPE ty_excrate,
                     ta_excrate                 TYPE TABLE OF  ty_excrate,
                     "SOA by pooja on 10.11.2022
                     ta_zco_pgi_rate_all        TYPE TABLE OF zco_pgi_rate, " LOC added by Sushil as on 29.06.2021 against ticket no. 8000007120
                     ta_zco_pgi_rate_3year      TYPE TABLE OF zco_pgi_rate, " LOC added by pooja as on 03.01.2023 against ticket no.CR#8000007907
                     ta_zco_pgi_rate_c          TYPE TABLE OF zco_pgi_rate, " LOC added by Sushil as on 02.06.2021 against ticket no. 8000007120
                     ta_zco_pgi_rate_1601       TYPE TABLE OF zco_pgi_rate, " LOC added by pooja as on 02.01.2023 against ticket no.8000007907
                     ta_zco_pgi_rate_1601_final TYPE TABLE OF zco_pgi_rate, " LOC added by pooja as on 06.01.2023 against ticket no.8000007907
                     ta_zco_pgi_rate_p          TYPE TABLE OF zco_pgi_rate, " LOC added by Sushil as on 02.06.2021 against ticket no. 8000007120
                     ta_zco_pgi_rate_1401       TYPE TABLE OF zco_pgi_rate, " LOC added by Sushil as on 03.06.2021 against ticket no. 8000007120
*                     ta_zco_pgi_rate_1602       TYPE TABLE OF zco_pgi_rate, " LOC added by pooja as on 02.01.2023 against ticket no.8000007907
                     ta_zco_pgi_rate_1602_final TYPE TABLE OF zco_pgi_rate, " LOC added by pooja as on 02.01.2023 against ticket no.8000007907
                     ta_zco_pgi_rate_1603_final TYPE TABLE OF zco_pgi_rate, " LOC added by pooja as on 02.01.2023 against ticket no.8000007907
                     ta_pgi_rate_3year_latest1  TYPE TABLE OF zco_pgi_rate, " LOC added by pooja as on 02.01.2023 against ticket no.8000007907
                     ta_zco_pgi_rate_1402       TYPE TABLE OF zco_pgi_rate, " LOC added by Sushil as on 03.06.2021 against ticket no. 8000007120
                     ta_zco_pgi_rate_1403       TYPE TABLE OF zco_pgi_rate, " LOC added by Sushil as on 03.06.2021 against ticket no. 8000007120
*                     ta_zco_pgi_rate_1603       TYPE TABLE OF zco_pgi_rate, " LOC added by pooja as on 02.01.2023 against ticket no.8000007907
                     ta_zco_pgi_rate_1404       TYPE TABLE OF zco_pgi_rate, " LOC added by Sushil as on 03.06.2021 against ticket no. 8000007120
                     ta_final                   TYPE TABLE OF ty_final,
                     wa_final                   TYPE ty_final,
                     ta_pgi_rate                TYPE TABLE OF ty_pgi_rate, "LOC added by Pooja on 08.09.2022  CR#8000007907
                     wa_pgi_rate                TYPE  ty_pgi_rate, "LOC added by Pooja on 08.09.2022 CR#8000007907
                     ta_keko                    TYPE TABLE OF ty_keko,
                     wa_keko                    TYPE ty_keko,
                     ta_keko_7                  TYPE TABLE OF ty_keko,
                     wa_keko_7                  TYPE ty_keko,
                     ta_keko_8                  TYPE TABLE OF ty_keko,
                     wa_keko_8                  TYPE ty_keko,
                     ta_keko_9                  TYPE TABLE OF ty_keko,
                     wa_keko_9                  TYPE ty_keko,
                     ta_trd                     TYPE TABLE OF ty_zco_trd,
                     wa_trd                     TYPE ty_zco_trd,
                     ta_keph                    TYPE TABLE OF ty_keph,
                     wa_keph                    TYPE ty_keph,
                     ta_dtm                     TYPE TABLE OF zco_dtm_std_cost,
                     wa_dtm                     TYPE zco_dtm_std_cost,
                     ta_cost                    TYPE TABLE OF zco_std_cost,
                     wa_cost                    TYPE zco_std_cost,
                     ta_midas                   TYPE TABLE OF zco_midas,
                     wa_midas                   TYPE zco_midas,
                     ta_curr                    TYPE TABLE OF ty_tcurr,
                     wa_curr                    TYPE ty_tcurr,
                     ta_t001                    TYPE TABLE OF ty_t001,
                     wa_t001                    TYPE ty_t001,
                     ta_setleaf                 TYPE TABLE OF ty_setleaf,
                     wa_setleaf                 TYPE ty_setleaf,
                     ta_trd_2                   TYPE TABLE OF ty_zco_trd, "Added by parashar CRDK965579 8000005475 13.11.2019
                     wa_trd_2                   TYPE ty_zco_trd,          "Added by parashar CRDK965579 8000005475 13.11.2019
                     ta_trd_3                   TYPE TABLE OF ty_zco_trd1, "Added by parashar CRDK965579 8000005475 13.11.2019
                     wa_trd_3                   TYPE ty_zco_trd1,       "Added by parashar CRDK965579 8000005475 13.11.2019
                     lv_fyr                     TYPE bapi0002_4-fiscal_year,
                     lv_fpr                     TYPE bapi0002_4-fiscal_period,
                     lv_spmonc                  TYPE spmon,
                     lv_fcomp                   TYPE bapi0002_2-comp_code,
                     lv_fpdat                   TYPE bapi0002_4-posting_date,
                     lwa_bapr                   TYPE bapireturn1.
***SOC by Gajanan on 21.10.2020 for CR#8000006372***
TYPES:               BEGIN OF ty_keko_05,
                       matnr   TYPE keko-matnr,
                       werks   TYPE keko-werks,
                       kadat   TYPE keko-kadat,
                       bidat   TYPE keko-bidat,
                       kadky   TYPE keko-kadky,
                       kalnr   TYPE keko-kalnr,
                       kalka   TYPE keko-kalka,
                       tvers   TYPE keko-tvers,
                       kokrs   TYPE keko-kokrs,
                       verid   TYPE keko-verid,
                       stnum   TYPE keko-stnum,
                       stlan   TYPE keko-stlan,
                       stalt   TYPE keko-stalt,
                       losgr   TYPE keko-losgr,
                       meins   TYPE keko-meins,
                       feh_sta TYPE keko-feh_sta,
                       klvar   TYPE keko-klvar,
                       poper   TYPE keko-poper,
                       bdatj   TYPE keko-bdatj,
                       bwvar   TYPE keko-bwvar,
                       bukrs   TYPE bukrs,
                       flag(1) TYPE c,
                     END OF ty_keko_05.
DATA: ta_keko_05          TYPE TABLE OF ty_keko_05,
      wa_keko_05          TYPE ty_keko_05,
      wa_zco_midas_priori TYPE zco_midas_priori.
CONSTANTS: co_x(1) VALUE 'X'.
***EOC by Gajanan on 21.10.2020 for CR#8000006372***

DATA : wa_qurter  TYPE p99sg_quarter,
       gv_return  TYPE sy-subrc,
       gv_date    TYPE sy-datum,
       gv_date1   TYPE sy-datum,
       gv_date2   TYPE sy-datum,
       gv_date3   TYPE sy-datum,
       gv_year(4) TYPE c,
       gv_mon(2)  TYPE c,
       gv_ukurs   TYPE ukurs_curr,
       gv_ukurs1  TYPE ukurs_curr.

DATA : t_fcat TYPE slis_t_fieldcat_alv,
       w_fcat TYPE slis_fieldcat_alv.

DATA : t_header TYPE slis_t_listheader,
       w_header TYPE slis_listheader.
DATA : wa_variant TYPE disvariant.

*SOA by Gajanan - 08.04.2022 - CR#8000007907
TYPES: BEGIN OF ty_t001_midas,
         bukrs TYPE t001-bukrs,
         waers TYPE t001-waers,
       END OF ty_t001_midas,

       BEGIN OF ty_exch_tab,
         waers TYPE t001-waers,
         ukurs TYPE ukursp,
       END OF ty_exch_tab.

DATA: ta_t001_midas  TYPE TABLE OF ty_t001_midas,
      wa_t001_midas  TYPE ty_t001_midas,
      ta_tcurr3      TYPE TABLE OF tcurr,
      wa_tcurr3      TYPE tcurr,
      ta_exch_tab    TYPE TABLE OF ty_exch_tab,
      wa_exch_tab    TYPE ty_exch_tab,
      lv_date_temp   TYPE sy-datum,
      lv_startdate   TYPE sy-datum,
      lv_enddate     TYPE sy-datum,
      lv_startdate_1 TYPE sy-datum,
      lv_enddate_1   TYPE sy-datum,
      lv_exchrate    TYPE ukurs_curr.
CONSTANTS: co_01 TYPE char2 VALUE '01'.
*EOA by Gajanan - 08.04.2022 - CR#8000007907

FIELD-SYMBOLS: <fs_lt_keph> TYPE ty_keph,
               <fs_value>   TYPE any.

RANGES:r_spmon FOR zco_midas-spmon,
       r_spmon1 FOR zco_midas-spmon,
       r_spmon2 FOR zco_midas-spmon, "Added by parashar CRDK965579 8000005475 13.11.2019
       r_spmon3 FOR zco_midas-spmon. "Added by pooja 03.01.2023  CR#8000007907

CONSTANTS :co_m         TYPE char1 VALUE 'M',
           co_waers_inr TYPE waers VALUE 'INR',
           co_waers_usd TYPE waers VALUE 'USD',
           co_waers_mad TYPE waers VALUE 'MAD', " LOC Added by Sushil as on 23.08.2021 against ticket no. 8000007312
           co_waers_aud TYPE waers VALUE 'AUD', " LOC Added by Sushil as on 23.08.2021 against ticket no. 8000007312
           co_waers_zar TYPE waers VALUE 'ZAR',
           co_waers_ugx TYPE waers VALUE 'UGX',
           co_sign      TYPE c VALUE 'I',
           co_option(2) TYPE c VALUE 'BT',
           co_setname   TYPE setleaf-setname VALUE 'MIDAS_CURRENCY',
           co_losgr     TYPE losgr VALUE '1000.000', "Added by parashar CRDK965579 8000005475 13.11.2019
           co_trd       TYPE werks_d VALUE 'TRD',   "Added by parashar CRDK965579 8000005475 13.11.2019
           co_vkorg     TYPE vkorg VALUE '1000'.  "Added parashar 8000005475 CRDK969082 07.04.2020

SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME.
PARAMETERS: p_dt TYPE d OBLIGATORY. "Added parashar 8000005475 CRDK969082 07.04.2020
PARAMETERS : p_spmon TYPE zco_midas-spmon." OBLIGATORY.
SELECT-OPTIONS : s_ent   FOR zco_midas-ent_tag,
                 s_vkorg FOR zco_midas-vkorg,
                 s_vtweg FOR zco_midas-vtweg,
                 s_matnr FOR zco_midas-matnr,
                 s_loc_p FOR zco_midas-locl_prd,
                 s_werks FOR zco_midas-werks,
                 s_kdgrp FOR zco_midas-kdgrp,
                 s_zone1 FOR zco_midas-zone_d, "Added parashar 8000005475 CRDK969082 07.04.2020
                 s_zone FOR zco_midas-sub_zone,
                 s_katr4 FOR zco_midas-katr4.
SELECTION-SCREEN : END OF BLOCK b1.

AT SELECTION-SCREEN.
  CLEAR:p_spmon.
  CONCATENATE p_dt+0(4) p_dt+4(2) INTO p_spmon.

INITIALIZATION.
  wa_variant-report = sy-repid.

START-OF-SELECTION.

  PERFORM  fetch_data.
  PERFORM  final_data.

*&---------------------------------------------------------------------*
*&      Form  FETCH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fetch_data.
  CONSTANTS:lc_day(2)   TYPE c VALUE '01',
            lc_kalka    TYPE kalka VALUE '01',
            lc_tvers(2) TYPE c VALUE '01',
            lc_bwvar    TYPE bwvar VALUE '001',
            lc_kokrs    TYPE kokrs VALUE '1000',
            lc_feh(2)   TYPE c VALUE 'FR',
            lc_klvar(4) TYPE c VALUE'PPC1'.

  CONCATENATE p_spmon lc_day INTO gv_date.
  gv_date1 = gv_date.
  gv_date3 = gv_date.
  CALL FUNCTION 'HR_99S_GET_QUARTER'
    EXPORTING
      im_date       = gv_date
*     IM_ABKRS      =
*     IM_PERMO      =
    IMPORTING
      ex_quarter    = wa_qurter
      ex_returncode = gv_return.

  r_spmon-sign = co_sign."'I'.
  r_spmon-option = co_option."'BT'.
  gv_year = wa_qurter-begda(4) - 5."1."Changed by parashar 8000005475 CRDK965963.
  gv_mon = wa_qurter-begda+4(2).
  CONCATENATE gv_year gv_mon INTO r_spmon-low.
  r_spmon-high = wa_qurter-endda+0(6).
  APPEND r_spmon.

  gv_date = wa_qurter-begda - 1.

  SELECT spmon
         ent_tag
         vkorg
         vtweg
         matnr
         locl_prd
         werks
         kdgrp
         sub_zone
         katr4
         zone_d
         plawe
         plaag_01
         sakn1
         spara
         fkart
         spart
         doc_cur
         usd_curr
         b_fkimg
         basme
    "Start of changes by Pooja Adupala on 30.03.2022 for CR#8000007907
         app_mat
         sales_grp
         add_drill_down
         zcluster
         sub_div_code
         div_code
         launch_year
         respi_prod
         final_tag
         add_drill_down2
         sales_cc_curr
         mat_cost_cc_curr
         api_vc_cc_curr
         api_fc_cc_curr
         for_vc_cc_curr
         for_fc_cc_curr
    "End of changes by Pooja Adupala on 30.03.2022 for CR#8000007907
*        "SOA by Lalitha on 06.02.2024 for CR#8000009287
          excl_qty
          bonus_qty
          bonus_ret_qty
          ip_code
*        "EOA by Lalitha on 06.02.2024 for CR#8000009287
    FROM zco_midas
    INTO TABLE ta_zco_midas
    WHERE spmon = p_spmon"IN r_spmon "Changed by parashar 8000005475 06.11.2019 CRDK965429
    AND ent_tag IN s_ent
    AND vkorg IN s_vkorg
    AND vtweg IN s_vtweg
    AND matnr IN s_matnr
    AND locl_prd IN s_loc_p
    AND werks IN s_werks
    AND kdgrp IN s_kdgrp
    AND sub_zone IN s_zone
    AND katr4 IN s_katr4
    AND zone_d IN s_zone1. "Added parashar 8000005475 CRDK969082 07.04.2020
  IF ta_zco_midas IS NOT INITIAL.
* **BEGIN OF CHANGE 01.11.2026 - duplicate-free FOR ALL ENTRIES drivers and
* sorted key tables (replace linear line_exists( ta_zco_midas[ ... ] ) scans)
    lt_drv_mat = CORRESPONDING #( ta_zco_midas ).
    SORT lt_drv_mat BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_drv_mat COMPARING matnr.
    lt_drv_ccm = CORRESPONDING #( ta_zco_midas ).
    SORT lt_drv_ccm BY vkorg matnr.
    DELETE ADJACENT DUPLICATES FROM lt_drv_ccm COMPARING vkorg matnr.
    lt_drv_cc = CORRESPONDING #( ta_zco_midas ).
    SORT lt_drv_cc BY vkorg.
    DELETE ADJACENT DUPLICATES FROM lt_drv_cc COMPARING vkorg.
* **END OF CHANGE 01.11.2026

    SELECT bwkey,bukrs FROM t001k INTO TABLE @DATA(ta_t001k)
      FOR ALL ENTRIES IN @ta_zco_midas
      WHERE bwkey = @ta_zco_midas-werks.

    IF ta_t001k IS NOT INITIAL.
      SORT ta_t001k BY bwkey.
    ENDIF.

    LOOP AT ta_zco_midas INTO wa_zco_midas.
      READ TABLE ta_t001k INTO DATA(wa_t001k) WITH KEY bwkey = wa_zco_midas-werks BINARY SEARCH. " 01.11.2026
      IF sy-subrc EQ 0.
        wa_zco_midas-bukrs = wa_t001k-bukrs.
        MODIFY ta_zco_midas FROM wa_zco_midas TRANSPORTING bukrs.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = gv_date1 "wa_qurter-begda
      IMPORTING
        last_day_of_month = gv_date1
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    """ SOC Done by Sushil as on 02.06.2021 against ticket no. 8000007120
    DATA: lv_mon    TYPE t5a4a-dlymo VALUE '60'.
    DATA: lv_date_c TYPE p0001-begda.
    DATA: lv_date_f TYPE p0001-begda.
    lv_date_c = gv_date1.
    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_date_c
        days      = '00'
        months    = lv_mon
        signum    = '-'
        years     = '00'
      IMPORTING
        calc_date = lv_date_f.

    CONCATENATE lv_date_f+0(6) lc_day INTO gv_date2.
    """ EOC Done by Sushil as on 02.06.2021 against ticket no. 8000007120
***    CONCATENATE r_spmon-low lc_day INTO gv_date2. " Commented by Sushil as on 02.06.2021 against ticket no. 8000007120

    SELECT matnr
           werks
           kadat
           bidat
           kadky
           kalnr
           kalka
           tvers
           kokrs
           verid
           stnum
           stlan
           stalt
           losgr
           meins
           feh_sta
           klvar
           poper
           bdatj
           bwvar
      FROM keko
      INTO TABLE ta_keko
      FOR ALL ENTRIES IN lt_drv_mat
      WHERE kalka = lc_kalka"'01'
      AND tvers = lc_tvers"'01'
      AND bwvar = lc_bwvar"'001'
      AND kokrs = lc_kokrs"'1000'
      AND feh_sta = lc_feh"'FR'
      AND klvar = lc_klvar "'PPC1'
*      AND kadat BETWEEN gv_date2 AND gv_date """ LOC Commented By Sushil as on 02.06.2021
      AND kadat GE gv_date2 AND kadat LE gv_date1
      AND matnr = lt_drv_mat-matnr.
*      AND werks = ta_zco_midas-werks.


    """ SOC done by Sushil as on 02.06.2021 against ticket no. 8000007120
    lv_fcomp = '1000'."s_vkorg-low.
    lv_fpdat = |{ p_spmon }01|.

    CALL FUNCTION 'BAPI_COMPANYCODE_GET_PERIOD'
      EXPORTING
        companycodeid = lv_fcomp
        posting_date  = lv_fpdat
      IMPORTING
        fiscal_year   = lv_fyr
        fiscal_period = lv_fpr
        return        = lwa_bapr.
    IF sy-subrc IS INITIAL AND lwa_bapr IS INITIAL.
      lv_spmonc = |{ lv_fyr }{ lv_fpr }|.
    ENDIF.
    """" Begin-Chages done as on 29.06.2021 against ticket no. 8000007120
    DATA(lv_spmonc1) = lv_spmonc - 1.

    SELECT fp_gjahr rbukrs matnr tagging waers ukurs ukurs1 ukurs_pur ukurs_fx ukurs_all  " 01.11.2026 field list instead of * (memory)
      FROM zco_pgi_rate INTO CORRESPONDING FIELDS OF TABLE ta_zco_pgi_rate_all
      FOR ALL ENTRIES IN lt_drv_mat
      WHERE fp_gjahr IN ( lv_spmonc1, lv_spmonc ) AND
*              rbukrs   = ta_zco_midas-vkorg  AND
            matnr    = lt_drv_mat-matnr  AND
            tagging  = '1'
* Code Remediation S4 2025_A **BEGIN OF CHANGE BY ABAP7 08.07.2026 FOR ATC
*            %_HINTS MSSQLNT 'INDEX("ZCO_PGI_RATE""ZCO_PGI_RATE~ZMI")'.
            .
* **END OF CHANGE BY ABAP7 08.07.2026 FOR ATC

    IF sy-subrc IS INITIAL.

      LOOP AT ta_zco_pgi_rate_all ASSIGNING FIELD-SYMBOL(<fs_rate_all>).
        IF <fs_rate_all>-fp_gjahr = lv_spmonc.
          """ Internal table appeding for current month
          READ TABLE lt_drv_cc TRANSPORTING NO FIELDS WITH KEY vkorg = <fs_rate_all>-rbukrs BINARY SEARCH. " 01.11.2026
          IF sy-subrc = 0.
            APPEND <fs_rate_all> TO ta_zco_pgi_rate_c.
          ENDIF.

          READ TABLE lt_drv_mat TRANSPORTING NO FIELDS WITH KEY matnr = <fs_rate_all>-matnr BINARY SEARCH. " 01.11.2026
          IF sy-subrc = 0.
            APPEND <fs_rate_all> TO ta_zco_pgi_rate_1401.
            APPEND <fs_rate_all> TO ta_zco_pgi_rate_1403.
          ENDIF.

        ELSEIF <fs_rate_all>-fp_gjahr = lv_spmonc1.
          """ Internal table appending for previous month
          READ TABLE lt_drv_cc TRANSPORTING NO FIELDS WITH KEY vkorg = <fs_rate_all>-rbukrs BINARY SEARCH. " 01.11.2026
          IF sy-subrc = 0.
            APPEND <fs_rate_all> TO ta_zco_pgi_rate_p.
          ENDIF.

          READ TABLE lt_drv_mat TRANSPORTING NO FIELDS WITH KEY matnr = <fs_rate_all>-matnr BINARY SEARCH. " 01.11.2026
          IF sy-subrc = 0.
            APPEND <fs_rate_all> TO ta_zco_pgi_rate_1402.
            APPEND <fs_rate_all> TO ta_zco_pgi_rate_1404.
          ENDIF.


        ENDIF.
      ENDLOOP.

      DELETE ta_zco_pgi_rate_c WHERE ukurs_pur IS INITIAL.
      DELETE ta_zco_pgi_rate_p WHERE ukurs_pur IS INITIAL.
      SORT ta_zco_pgi_rate_c BY matnr rbukrs.
      SORT ta_zco_pgi_rate_p BY matnr rbukrs.

      " Logic begin - 14.1 materical + Single company code-Current Period
      DELETE ta_zco_pgi_rate_1401 WHERE ukurs_pur IS INITIAL.
      SORT ta_zco_pgi_rate_1401 BY matnr.
      DATA(ta_zco_pgi_rate_1401_org) = ta_zco_pgi_rate_1401[].
      DELETE ADJACENT DUPLICATES FROM ta_zco_pgi_rate_1401 COMPARING matnr.

* **BEGIN OF CHANGE 01.11.2026 - distinct company codes per material computed
* once (sorted) instead of copying and filtering the whole table per row
      PERFORM pgi_rate_cocd_count USING ta_zco_pgi_rate_1401_org CHANGING lt_cnt.
      LOOP AT ta_zco_pgi_rate_1401 ASSIGNING FIELD-SYMBOL(<fs_zco_pgi_rate_1401>).
        CLEAR ls_cnt.
        READ TABLE lt_cnt INTO ls_cnt WITH KEY matnr = <fs_zco_pgi_rate_1401>-matnr BINARY SEARCH.
        IF ls_cnt-cnt GT 1.
          <fs_zco_pgi_rate_1401>-rbukrs = abap_false.
        ENDIF.
      ENDLOOP.
* **END OF CHANGE 01.11.2026
      DELETE ta_zco_pgi_rate_1401 WHERE rbukrs = abap_false.
*      DELETE ta_zco_pgi_rate_1401 WHERE ukurs_

      " Logic end - 14.1 materical + Single company code-Current Period

      " Logic begin - 14.3 materical + Multiple company code-Current Period
      DELETE ta_zco_pgi_rate_1403 WHERE ukurs_pur IS INITIAL.
      SORT ta_zco_pgi_rate_1403 BY matnr.
      DATA(ta_zco_pgi_rate_1403_org) = ta_zco_pgi_rate_1403[].
      DELETE ADJACENT DUPLICATES FROM ta_zco_pgi_rate_1403 COMPARING matnr.

* **BEGIN OF CHANGE 01.11.2026 - distinct company codes per material computed
* once (sorted) instead of copying and filtering the whole table per row
      PERFORM pgi_rate_cocd_count USING ta_zco_pgi_rate_1403_org CHANGING lt_cnt.
      LOOP AT ta_zco_pgi_rate_1403 ASSIGNING FIELD-SYMBOL(<fs_zco_pgi_rate_1403>).
        CLEAR ls_cnt.
        READ TABLE lt_cnt INTO ls_cnt WITH KEY matnr = <fs_zco_pgi_rate_1403>-matnr BINARY SEARCH.
        IF ls_cnt-cnt EQ 1.
          <fs_zco_pgi_rate_1403>-rbukrs = abap_false.
        ENDIF.
      ENDLOOP.
* **END OF CHANGE 01.11.2026
      DELETE ta_zco_pgi_rate_1403 WHERE rbukrs = abap_false.
      " Logic end - 14.3 materical + Multiple company code-Current Period

      " Logic begin - 14.2 materical + Single company code-previous period
      DELETE ta_zco_pgi_rate_1402 WHERE ukurs_pur IS INITIAL.
      SORT  ta_zco_pgi_rate_1402 BY matnr.
      DATA(ta_zco_pgi_rate_1402_org) = ta_zco_pgi_rate_1402[].
      DELETE ADJACENT DUPLICATES FROM ta_zco_pgi_rate_1402 COMPARING matnr.

* **BEGIN OF CHANGE 01.11.2026 - distinct company codes per material computed
* once (sorted) instead of copying and filtering the whole table per row
      PERFORM pgi_rate_cocd_count USING ta_zco_pgi_rate_1402_org CHANGING lt_cnt.
      LOOP AT ta_zco_pgi_rate_1402 ASSIGNING FIELD-SYMBOL(<fs_zco_pgi_rate_1402>).
        CLEAR ls_cnt.
        READ TABLE lt_cnt INTO ls_cnt WITH KEY matnr = <fs_zco_pgi_rate_1402>-matnr BINARY SEARCH.
        IF ls_cnt-cnt GT 1.
          <fs_zco_pgi_rate_1402>-rbukrs = abap_false.
        ENDIF.
      ENDLOOP.
* **END OF CHANGE 01.11.2026
      DELETE ta_zco_pgi_rate_1402 WHERE rbukrs = abap_false.

      " Logic end - 14.2 materical + Single company code-previous period

      """ Logic begin - 14.4 materical + Multiple company code-previous period
      SORT ta_zco_pgi_rate_1404 BY matnr.
      DATA(ta_zco_pgi_rate_1404_org) = ta_zco_pgi_rate_1404[].
      DELETE ADJACENT DUPLICATES FROM ta_zco_pgi_rate_1404 COMPARING matnr.

* **BEGIN OF CHANGE 01.11.2026 - distinct company codes per material computed
* once (sorted) instead of copying and filtering the whole table per row
      PERFORM pgi_rate_cocd_count USING ta_zco_pgi_rate_1404_org CHANGING lt_cnt.
      LOOP AT ta_zco_pgi_rate_1404 ASSIGNING FIELD-SYMBOL(<fs_zco_pgi_rate_1404>).
        CLEAR ls_cnt.
        READ TABLE lt_cnt INTO ls_cnt WITH KEY matnr = <fs_zco_pgi_rate_1404>-matnr BINARY SEARCH.
        IF ls_cnt-cnt EQ 1.
          <fs_zco_pgi_rate_1404>-rbukrs = abap_false.
        ENDIF.
      ENDLOOP.
* **END OF CHANGE 01.11.2026
      DELETE ta_zco_pgi_rate_1404 WHERE rbukrs = abap_false.
      """ Logic End - 14.4 materical + Multiple company code-previous period
    ENDIF.

    "SOC by Pooja on 03.01.2023 CR#8000007907
    r_spmon3-sign = co_sign.
    r_spmon3-option = co_option.
    r_spmon3-low = lv_spmonc - 000300.
    r_spmon3-high = lv_spmonc.
    APPEND r_spmon3.

    SELECT fp_gjahr rbukrs matnr tagging waers ukurs ukurs1 ukurs_pur ukurs_fx ukurs_all  " 01.11.2026 field list instead of * (memory)
      FROM zco_pgi_rate INTO CORRESPONDING FIELDS OF TABLE ta_zco_pgi_rate_3year
     FOR ALL ENTRIES IN lt_drv_mat
     WHERE fp_gjahr IN r_spmon3 AND
           matnr    = lt_drv_mat-matnr  AND
           tagging  = '1'
* Code Remediation S4 2025_A **BEGIN OF CHANGE BY ABAP7 08.07.2026 FOR ATC
*           %_HINTS MSSQLNT 'INDEX("ZCO_PGI_RATE""ZCO_PGI_RATE~ZMI")'.
           .
* **END OF CHANGE BY ABAP7 08.07.2026 FOR ATC

    SELECT fp_gjahr rbukrs matnr tagging waers ukurs ukurs1 ukurs_pur ukurs_fx ukurs_all  " 01.11.2026 field list instead of * (memory)
      FROM zco_pgi_rate INTO CORRESPONDING FIELDS OF TABLE ta_zco_pgi_rate_1601
    FOR ALL ENTRIES IN lt_drv_ccm
    WHERE rbukrs = lt_drv_ccm-vkorg AND
          matnr    = lt_drv_ccm-matnr  AND
          fp_gjahr  IN r_spmon3 AND
          tagging  = '1'
* Code Remediation S4 2025_A **BEGIN OF CHANGE BY ABAP7 08.07.2026 FOR ATC
*          %_HINTS MSSQLNT 'INDEX("ZCO_PGI_RATE""ZCO_PGI_RATE~ZMI")'.
          .
* **END OF CHANGE BY ABAP7 08.07.2026 FOR ATC


    IF ta_zco_pgi_rate_1601 IS NOT INITIAL.
* **BEGIN OF CHANGE 01.11.2026 - latest period per material/company code
* by one sort instead of copy+filter per row (same result, O(n log n))
      ta_zco_pgi_rate_1601_final = ta_zco_pgi_rate_1601.
      DELETE ta_zco_pgi_rate_1601_final WHERE ukurs_pur IS INITIAL.
      SORT ta_zco_pgi_rate_1601_final BY matnr rbukrs fp_gjahr DESCENDING.
      DELETE ADJACENT DUPLICATES FROM ta_zco_pgi_rate_1601_final COMPARING matnr rbukrs.
      SORT ta_zco_pgi_rate_1601 BY rbukrs matnr.
      DELETE ADJACENT DUPLICATES FROM ta_zco_pgi_rate_1601 COMPARING  rbukrs matnr .
      SORT ta_zco_pgi_rate_1601_final BY matnr rbukrs.
* **END OF CHANGE 01.11.2026
    ENDIF.

    IF ta_zco_pgi_rate_3year IS NOT INITIAL.

      " Logic begin - 16.2 Material + Single Company code - PGI Rate for Trading - Latest period till 3 years
      DELETE ta_zco_pgi_rate_3year WHERE ukurs_pur IS INITIAL.
      SORT ta_zco_pgi_rate_3year BY matnr .
      DATA(ta_zco_pgi_rate_1602) = ta_zco_pgi_rate_3year[].
      DATA(ta_zco_pgi_rate_1603) = ta_zco_pgi_rate_3year[].
      DATA(ta_pgi_rate_3year_latest) = ta_zco_pgi_rate_3year[].
      SORT ta_pgi_rate_3year_latest BY matnr.
      DELETE ADJACENT DUPLICATES FROM ta_pgi_rate_3year_latest COMPARING matnr.


* **BEGIN OF CHANGE 01.11.2026 - latest period per material by one sort
      ta_pgi_rate_3year_latest1 = ta_zco_pgi_rate_3year.
      SORT ta_pgi_rate_3year_latest1 BY matnr fp_gjahr DESCENDING rbukrs.
      DELETE ADJACENT DUPLICATES FROM ta_pgi_rate_3year_latest1 COMPARING matnr.
* **END OF CHANGE 01.11.2026


      ta_zco_pgi_rate_1602_final = ta_pgi_rate_3year_latest1[].
      ta_zco_pgi_rate_1603_final = ta_pgi_rate_3year_latest1[].

*      DELETE ta_zco_pgi_rate_1602 WHERE ukurs_pur IS INITIAL.
*      DELETE ta_zco_pgi_rate_1603 WHERE ukurs_pur IS INITIAL.

* **BEGIN OF CHANGE 01.11.2026 - distinct company codes per material once
      PERFORM pgi_rate_cocd_count USING ta_zco_pgi_rate_1602 CHANGING lt_cnt.
      LOOP AT ta_zco_pgi_rate_1602_final ASSIGNING FIELD-SYMBOL(<fs_zco_pgi_rate_1602_final>).
        CLEAR ls_cnt.
        READ TABLE lt_cnt INTO ls_cnt WITH KEY matnr = <fs_zco_pgi_rate_1602_final>-matnr BINARY SEARCH.
        IF ls_cnt-cnt GT 1 OR ls_cnt-cnt EQ 0.
          <fs_zco_pgi_rate_1602_final>-rbukrs = abap_false.
        ENDIF.
      ENDLOOP.
* **END OF CHANGE 01.11.2026

      DELETE ta_zco_pgi_rate_1602_final WHERE rbukrs = abap_false.
      SORT ta_zco_pgi_rate_1602_final BY matnr.
      " Logic end - 16.2 Material + Single Company code - PGI Rate for Trading - Latest period till 3 years

      "Logic begin - 16.3 Material + Multiple Company code - PGI Rate for Trading - Latest period till 3 years

* **BEGIN OF CHANGE 01.11.2026 - distinct company codes per material once
      PERFORM pgi_rate_cocd_count USING ta_zco_pgi_rate_1603 CHANGING lt_cnt.
      LOOP AT ta_zco_pgi_rate_1603_final ASSIGNING FIELD-SYMBOL(<fs_zco_pgi_rate_1603_final>).
        CLEAR ls_cnt.
        READ TABLE lt_cnt INTO ls_cnt WITH KEY matnr = <fs_zco_pgi_rate_1603_final>-matnr BINARY SEARCH.
        IF ls_cnt-cnt EQ 1 OR ls_cnt-cnt EQ 0.
          <fs_zco_pgi_rate_1603_final>-rbukrs = abap_false.
        ENDIF.
      ENDLOOP.
* **END OF CHANGE 01.11.2026
      DELETE ta_zco_pgi_rate_1603_final WHERE rbukrs = abap_false.
      SORT ta_zco_pgi_rate_1603_final BY matnr.
      "Logic end - 16.3 Material + Multiple Company code - PGI Rate for Trading - Latest period till 3 years
    ENDIF.
    "EOC by Pooja on 03.01.2023 CR#8000007907
    """" End-Chages done as on 29.06.2021 against ticket no. 8000007120
****      "Logic begin for current period
****      SELECT * FROM zco_pgi_rate INTO TABLE ta_zco_pgi_rate_c
****        FOR ALL ENTRIES IN ta_zco_midas
****        WHERE FP_GJAHR = lv_spmonc  AND
****              rbukrs   = ta_zco_midas-vkorg  AND
****              matnr    = ta_zco_midas-matnr  AND
****              tagging  = '1'.
****
****       " Logic begin - 14.1 materical + Single company code-Current Period
****
****       SELECT * FROM zco_pgi_rate INTO TABLE ta_zco_pgi_rate_1401
****        FOR ALL ENTRIES IN ta_zco_midas
****        WHERE FP_GJAHR = lv_spmonc  AND
****              matnr    = ta_zco_midas-matnr  AND
****              tagging  = '1'.
****
****       sort ta_zco_pgi_rate_1401 by matnr.
****       data(ta_zco_pgi_rate_1401_org) = ta_zco_pgi_rate_1401[].
****       DELETE ADJACENT DUPLICATES FROM ta_zco_pgi_rate_1401 COMPARING matnr.
****
****       LOOP AT ta_zco_pgi_rate_1401 ASSIGNING FIELD-SYMBOL(<fs_zco_pgi_rate_1401>).
****         data(ta_zco_pgi_rate_1401_tmp) = ta_zco_pgi_rate_1401_org[].
****         delete ta_zco_pgi_rate_1401_tmp WHERE matnr ne <fs_zco_pgi_rate_1401>-matnr.
****         SORT ta_zco_pgi_rate_1401_tmp by rbukrs matnr .
****         DELETE ADJACENT DUPLICATES FROM ta_zco_pgi_rate_1401_tmp COMPARING rbukrs matnr.
****         describe TABLE ta_zco_pgi_rate_1401_tmp LINES data(lv_c_1401).
****         IF lv_c_1401 gt 1.
****           <fs_zco_pgi_rate_1401>-rbukrs = abap_false.
****         ENDIF.
****       ENDLOOP.
****       delete ta_zco_pgi_rate_1401 WHERE rbukrs = abap_false.
****       " Logic end - 14.1 materical + Single company code-Current Period
****
****       " Logic begin - 14.3 materical + Multiple company code-Current Period
****
****      SELECT * FROM zco_pgi_rate INTO TABLE ta_zco_pgi_rate_1403
****        FOR ALL ENTRIES IN ta_zco_midas
****        WHERE FP_GJAHR = lv_spmonc  AND
****              matnr    = ta_zco_midas-matnr  AND
****              tagging  = '1'.
****
****       sort ta_zco_pgi_rate_1403 by matnr.
****       data(ta_zco_pgi_rate_1403_org) = ta_zco_pgi_rate_1403[].
****       DELETE ADJACENT DUPLICATES FROM ta_zco_pgi_rate_1403 COMPARING matnr.
****
****       LOOP AT ta_zco_pgi_rate_1403 ASSIGNING FIELD-SYMBOL(<fs_zco_pgi_rate_1403>).
****         data(ta_zco_pgi_rate_1403_tmp) = ta_zco_pgi_rate_1403_org[].
****         delete ta_zco_pgi_rate_1403_tmp WHERE matnr ne <fs_zco_pgi_rate_1403>-matnr.
****         SORT ta_zco_pgi_rate_1403_tmp by rbukrs matnr .
****         DELETE ADJACENT DUPLICATES FROM ta_zco_pgi_rate_1403_tmp COMPARING rbukrs matnr.
****         describe TABLE ta_zco_pgi_rate_1403_tmp LINES data(lv_c_1403).
****         IF lv_c_1403 eq 1.
****           <fs_zco_pgi_rate_1403>-rbukrs = abap_false.
****         ENDIF.
****       ENDLOOP.
****       delete ta_zco_pgi_rate_1403 WHERE rbukrs = abap_false.
****       " Logic end - 14.3 materical + Multiple company code-Current Period
****
****      """ Logic begins for previous period
****      data(lv_spmon_prv) = lv_spmonc - 1.
****      SELECT * FROM zco_pgi_rate INTO TABLE ta_zco_pgi_rate_p
****        FOR ALL ENTRIES IN ta_zco_midas
****        WHERE FP_GJAHR = lv_spmon_prv  AND
****              rbukrs   = ta_zco_midas-vkorg  AND
****              matnr    = ta_zco_midas-matnr  AND
****              tagging  = '1'.
****
****        " Logic begin - 14.2 materical + Single company code-previous period
****              SELECT * FROM zco_pgi_rate INTO TABLE ta_zco_pgi_rate_1402
****                FOR ALL ENTRIES IN ta_zco_midas
****                WHERE FP_GJAHR = lv_spmon_prv  AND
****                      matnr    = ta_zco_midas-matnr  AND
****                      tagging  = '1'.
****
****        sort  ta_zco_pgi_rate_1402 by matnr.
****        data(ta_zco_pgi_rate_1402_org) = ta_zco_pgi_rate_1402[].
****        DELETE ADJACENT DUPLICATES FROM ta_zco_pgi_rate_1402 COMPARING matnr.
****
****      LOOP AT ta_zco_pgi_rate_1402 ASSIGNING FIELD-SYMBOL(<fs_zco_pgi_rate_1402>).
****         data(ta_zco_pgi_rate_1402_tmp) = ta_zco_pgi_rate_1402_org[].
****         delete ta_zco_pgi_rate_1402_tmp WHERE matnr ne <fs_zco_pgi_rate_1402>-matnr.
****         SORT ta_zco_pgi_rate_1402_tmp by rbukrs matnr .
****         DELETE ADJACENT DUPLICATES FROM ta_zco_pgi_rate_1402_tmp COMPARING rbukrs matnr.
****         describe TABLE ta_zco_pgi_rate_1402_tmp LINES data(lv_c_1402).
****         IF lv_c_1402 gt 1.
****           <fs_zco_pgi_rate_1402>-rbukrs = abap_false.
****         ENDIF.
****       ENDLOOP.
****       delete ta_zco_pgi_rate_1402 WHERE rbukrs = abap_false.
****       " Logic end - 14.2 materical + Single company code-previous period
****
****       " Logic begin - 14.4 materical + Multiple company code-previous period
****       SELECT * FROM zco_pgi_rate INTO TABLE ta_zco_pgi_rate_1404
****                FOR ALL ENTRIES IN ta_zco_midas
****                WHERE FP_GJAHR = lv_spmon_prv  AND
****                      matnr    = ta_zco_midas-matnr  AND
****                      tagging  = '1'.
****
****       sort ta_zco_pgi_rate_1404 by matnr.
****       data(ta_zco_pgi_rate_1404_org) = ta_zco_pgi_rate_1404[].
****       DELETE ADJACENT DUPLICATES FROM ta_zco_pgi_rate_1404 COMPARING matnr.
****
****       LOOP AT ta_zco_pgi_rate_1404 ASSIGNING FIELD-SYMBOL(<fs_zco_pgi_rate_1404>).
****         data(ta_zco_pgi_rate_1404_tmp) = ta_zco_pgi_rate_1404_org[].
****         delete ta_zco_pgi_rate_1404_tmp WHERE matnr ne <fs_zco_pgi_rate_1404>-matnr.
****         SORT ta_zco_pgi_rate_1404_tmp by rbukrs matnr .
****         DELETE ADJACENT DUPLICATES FROM ta_zco_pgi_rate_1404_tmp COMPARING rbukrs matnr.
****         describe TABLE ta_zco_pgi_rate_1404_tmp LINES data(lv_c_1404).
****         IF lv_c_1404 eq 1.
****           <fs_zco_pgi_rate_1404>-rbukrs = abap_false.
****         ENDIF.
****       ENDLOOP.
****       delete ta_zco_pgi_rate_1404 WHERE rbukrs = abap_false.
****
****       " Logic End - 14.4 materical + Multiple company code-previous period
    """ EOC done by Sushil as on 02.06.2021 against ticket no. 8000007120
    IF ta_keko IS NOT INITIAL.
      SELECT bwkey bukrs FROM t001k
        APPENDING TABLE ta_t001k
        FOR ALL ENTRIES IN ta_keko
            WHERE bwkey = ta_keko-werks.
      IF sy-subrc = 0.
      ENDIF.
      SORT ta_t001k BY bwkey. " 01.11.2026
    ENDIF.

    LOOP AT ta_keko INTO wa_keko.
      READ TABLE ta_t001k INTO wa_t001k WITH KEY bwkey = wa_keko-werks BINARY SEARCH. " 01.11.2026
      IF sy-subrc EQ 0.
        wa_keko-bukrs = wa_t001k-bukrs.
        MODIFY ta_keko FROM wa_keko TRANSPORTING bukrs.
      ENDIF.
    ENDLOOP.
    DELETE ta_keko WHERE bukrs IS INITIAL.
    SELECT matnr
         werks
         kadat
         bidat
         kadky
         kalnr
         kalka
         tvers
         kokrs
         verid
         stnum
         stlan
         stalt
         losgr
         meins
         feh_sta
         klvar
         poper
         bdatj
         bwvar
    FROM keko
    INTO TABLE ta_keko_7
    FOR ALL ENTRIES IN lt_drv_mat
    WHERE kalka = lc_kalka"'01'
    AND tvers = lc_tvers"''01'
    AND bwvar = lc_bwvar"''001'
    AND kokrs = lc_kokrs                                    "''1000'
    AND feh_sta = lc_feh"'= 'FR'
    AND klvar = lc_klvar "'PPC1'
    AND kadat BETWEEN wa_qurter-begda AND gv_date1 "wa_qurter-endda
    AND matnr = lt_drv_mat-matnr.
*    AND werks = ta_zco_midas-werks.

    IF ta_keko_7 IS NOT INITIAL.
      SELECT bwkey bukrs FROM t001k
            APPENDING TABLE ta_t001k
            FOR ALL ENTRIES IN ta_keko_7
                WHERE bwkey = ta_keko_7-werks.
    ENDIF.
    SORT ta_t001k BY bwkey.
    LOOP AT ta_keko_7 INTO wa_keko_7.
      READ TABLE ta_t001k INTO wa_t001k WITH KEY bwkey = wa_keko_7-werks BINARY SEARCH. " 01.11.2026
      IF sy-subrc EQ 0.
        wa_keko_7-bukrs = wa_t001k-bukrs.
        MODIFY ta_keko_7 FROM wa_keko_7 TRANSPORTING bukrs.
      ENDIF.
    ENDLOOP.
    DELETE ta_keko_7 WHERE bukrs IS INITIAL.

***SOC by Gajanan on 21.10.2020 for CR#8000006372***
    CLEAR: wa_zco_midas_priori.

    SELECT SINGLE * FROM zco_midas_priori
      INTO wa_zco_midas_priori
      WHERE zmon = p_spmon.

    IF sy-subrc = 0.

      SELECT matnr
               werks
               kadat
               bidat
               kadky
               kalnr
               kalka
               tvers
               kokrs
               verid
               stnum
               stlan
               stalt
               losgr
               meins
               feh_sta
               klvar
               poper
               bdatj
               bwvar
          FROM keko
          INTO TABLE ta_keko_05
          FOR ALL ENTRIES IN lt_drv_mat
          WHERE kalka = lc_kalka"'01'
          AND tvers = lc_tvers"''01'
          AND bwvar = lc_bwvar"''001'
          AND kokrs = lc_kokrs                              "''1000'
          AND feh_sta = lc_feh"'= 'FR'
          AND klvar = lc_klvar "'PPC1'
          AND kadky = wa_zco_midas_priori-zsc_mon
          AND matnr = lt_drv_mat-matnr.

      IF NOT ta_keko_05 IS INITIAL.
        DATA(ta_keko_werks) = ta_keko_05.
        SORT ta_keko_werks BY werks.
        DELETE ADJACENT DUPLICATES FROM ta_keko_werks COMPARING werks.

        SELECT bwkey bukrs FROM t001k
          APPENDING TABLE ta_t001k
          FOR ALL ENTRIES IN ta_keko_werks
              WHERE bwkey = ta_keko_werks-werks.
        SORT ta_t001k BY bwkey. " 01.11.2026

        SELECT werks, land1 FROM t001w
          INTO TABLE @DATA(ta_t001w)
          FOR ALL ENTRIES IN @ta_keko_werks
          WHERE werks = @ta_keko_werks-werks.

        LOOP AT ta_keko_05 INTO wa_keko_05.
          READ TABLE ta_t001k INTO wa_t001k WITH KEY bwkey = wa_keko_05-werks BINARY SEARCH. " 01.11.2026
          IF sy-subrc EQ 0.
            wa_keko_05-bukrs = wa_t001k-bukrs.
            MODIFY ta_keko_05 FROM wa_keko_05 TRANSPORTING bukrs.
          ENDIF.

          READ TABLE ta_t001w INTO DATA(wa_t001w) WITH KEY werks = wa_keko_05-werks.
          IF sy-subrc = 0 AND wa_t001w-land1 NE wa_zco_midas_priori-land1.
            wa_keko_05-flag = co_x.
            MODIFY ta_keko_05 FROM wa_keko_05 TRANSPORTING flag.
          ENDIF.

          CLEAR: wa_keko_05,wa_t001k,wa_t001w.
        ENDLOOP.

        DELETE ta_keko_05 WHERE flag = co_x.


      ENDIF.
    ENDIF.
***EOC by Gajanan on 21.10.2020 for CR#8000006372***

    DATA: lv_begdat_1(8) TYPE c,
          lv_enddat_1(8) TYPE c.

    SELECT bukrs waers FROM t001 INTO TABLE ta_t001.
*      FOR ALL ENTRIES IN ta_t001k "Commented by parashar 8000005475 CRDK967001 14.01.2020
*      WHERE bukrs = ta_t001k-bukrs. "Commented by parashar 8000005475 CRDK967001 14.01.2020
    IF ta_t001 IS NOT INITIAL.
      CLEAR : lv_begdat_1, lv_enddat_1.
      CONVERT DATE gv_date3 INTO INVERTED-DATE lv_begdat_1.
      CONVERT DATE gv_date1 INTO INVERTED-DATE lv_enddat_1.
      SELECT kurst,fcurr,gdatu,ukurs FROM tcurr INTO TABLE @DATA(ta_tcurr)
          FOR ALL ENTRIES IN @ta_t001
          WHERE kurst = @co_m
          AND   fcurr = @ta_t001-waers
          AND   tcurr = @co_waers_inr
          AND  gdatu GE @lv_enddat_1 AND gdatu LE @lv_begdat_1.
      IF sy-subrc = 0.
        SORT ta_tcurr BY fcurr.
      ENDIF.

      SELECT  setclass
              setname
              valfrom
        FROM setleaf
        INTO TABLE ta_setleaf
        WHERE setname = co_setname.
      IF sy-subrc EQ 0.
        LOOP AT ta_setleaf INTO wa_setleaf.
          READ TABLE ta_tcurr INTO DATA(wa_tcurr) WITH KEY fcurr = wa_setleaf-valfrom.
          IF sy-subrc EQ 0.
            DELETE ta_setleaf WHERE valfrom = wa_setleaf-valfrom.
          ELSE.
            wa_setleaf-fcurr = wa_setleaf-valfrom.
            MODIFY ta_setleaf FROM wa_setleaf TRANSPORTING fcurr.
          ENDIF.
          CLEAR: wa_setleaf,wa_tcurr.
        ENDLOOP.
        SELECT kurst fcurr gdatu ukurs FROM tcurr APPENDING TABLE ta_tcurr
        FOR ALL ENTRIES IN ta_setleaf
        WHERE kurst = co_m
        AND   fcurr = ta_setleaf-fcurr
        AND   tcurr = co_waers_inr
        AND  gdatu GE lv_enddat_1 AND gdatu LE lv_begdat_1.
      ENDIF.

      SORT ta_tcurr BY fcurr.
      DATA(ta_tcurr1) = ta_tcurr.
      SORT ta_tcurr1 BY fcurr.
      DELETE ADJACENT DUPLICATES FROM ta_tcurr1 COMPARING fcurr.

      LOOP AT ta_tcurr1 INTO DATA(wa_tcurr1).
        DATA(ta_tcurr2) = ta_tcurr.
        SORT ta_tcurr2 BY fcurr.
        DELETE ta_tcurr2 WHERE fcurr NE wa_tcurr1-fcurr.
        DESCRIBE TABLE ta_tcurr2 LINES DATA(lv_line).
        gv_ukurs = REDUCE ukurs_curr( INIT x = '0.0000000'  FOR wa_tcurr2 IN ta_tcurr2 NEXT x = x + wa_tcurr2-ukurs ).
        IF lv_line IS NOT INITIAL.
          gv_ukurs = gv_ukurs / lv_line.
        ENDIF.
        MOVE-CORRESPONDING wa_tcurr1 TO wa_curr.
        wa_curr-ukurs = gv_ukurs.
        APPEND wa_curr TO ta_curr.
        CLEAR: wa_curr,gv_ukurs,lv_line.
      ENDLOOP.

    ENDIF.


*    PERFORM f_month.
*
**    SELECT spmon
*    SELECT bukrs
**           mblnr
*           matnr
*           SUM( bamng ) AS bamng
*           SUM( dmbtr ) AS dmbtr
*      FROM zco_trd
*      INTO TABLE ta_trd
**      FOR ALL ENTRIES IN ta_zco_midas
*      WHERE spmon IN r_spmon1
**      AND matnr = ta_zco_midas-matnr
*      GROUP BY bukrs matnr.
*** Added by parashar CRDK965579 8000005475 13.11.2019
    PERFORM f_month1.

    SELECT bukrs
           matnr
           SUM( bamng ) AS bamng
           SUM( dmbtr ) AS dmbtr
           FROM zco_trd
           INTO TABLE ta_trd_2
           WHERE spmon IN r_spmon2
           GROUP BY bukrs matnr.

    SELECT spmon
         bukrs
         matnr
         SUM( bamng ) AS bamng
         SUM( dmbtr ) AS dmbtr
        FROM zco_trd
        INTO TABLE ta_trd_3
        WHERE spmon LE p_spmon
        GROUP BY spmon bukrs matnr.
*** "Added upto by parashar CRDK965579 8000005475 13.11.2019
    SELECT * FROM zco_dtm_std_cost INTO TABLE ta_dtm
      FOR ALL ENTRIES IN ta_zco_midas
*      WHERE bukrs = ta_zco_midas-bukrs "Commented by parashar 8000005475 CRDK965881 26.11.2019
      WHERE matnr = ta_zco_midas-matnr
*      AND locl_prd = ta_zco_midas-locl_prd "Commented by parashar 8000005475 CRDK965881 26.11.2019
      AND spmon = ta_zco_midas-spmon.

    SELECT * FROM zco_std_cost INTO TABLE ta_cost.

*SOA by Gajanan - 08.04.2022 - CR#8000007907
    "Fetch data from table T001
    DATA(ta_midas_vkorg) = ta_zco_midas.

    SORT ta_midas_vkorg BY vkorg.
    DELETE ADJACENT DUPLICATES FROM ta_midas_vkorg COMPARING vkorg.

    IF NOT ta_midas_vkorg IS INITIAL.
      SELECT bukrs waers FROM t001
        INTO TABLE ta_t001_midas
        FOR ALL ENTRIES IN ta_midas_vkorg
        WHERE bukrs = ta_midas_vkorg-vkorg.

      IF NOT ta_t001_midas IS INITIAL.

        IF line_exists( ta_t001_midas[  waers =  co_waers_usd ] ).
          "Do nothing
        ELSE.
          wa_t001_midas-waers = co_waers_usd.
          APPEND wa_t001_midas TO ta_t001_midas.
          CLEAR: wa_t001_midas.
        ENDIF.

        SORT ta_t001_midas BY bukrs.


        IF p_spmon IS NOT INITIAL.
          lv_date_temp = |{ p_spmon }| & || & |{ co_01 } |.
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


        "Fetch data from table TCURR
        SELECT * FROM tcurr
          INTO TABLE ta_tcurr3
          FOR ALL ENTRIES IN ta_t001_midas
          WHERE kurst = 'M'
            AND fcurr = ta_t001_midas-waers
            AND tcurr = co_waers_inr
            AND gdatu GE lv_enddate_1 AND gdatu LE lv_startdate_1.

        "Calculate Exchange Rate
        IF NOT ta_tcurr3 IS INITIAL.

          SORT ta_tcurr3 BY fcurr.
          DATA(ta_curr1_temp) = ta_tcurr3.
          SORT ta_curr1_temp BY fcurr.
          DELETE ADJACENT DUPLICATES FROM ta_curr1_temp COMPARING fcurr.

          LOOP AT ta_curr1_temp INTO DATA(wa_tcurr1_temp).

            DATA(ta_exch) = ta_tcurr3.
            DELETE ta_exch WHERE fcurr <> wa_tcurr1_temp-fcurr.

            IF NOT ta_exch IS INITIAL.

              DESCRIBE TABLE ta_exch LINES DATA(lv_exch_lines).

              lv_exchrate = REDUCE ukurs_curr( INIT x = '0.0000000'  FOR wa_exch IN ta_exch NEXT x = x + wa_exch-ukurs ).
              IF lv_exch_lines IS NOT INITIAL.
                lv_exchrate = lv_exchrate / lv_exch_lines.
              ENDIF.

              wa_exch_tab-waers = wa_tcurr1_temp-fcurr.
              wa_exch_tab-ukurs = lv_exchrate.

              APPEND wa_exch_tab TO ta_exch_tab.
            ENDIF.

            CLEAR: wa_exch_tab,lv_exchrate,lv_exch_lines,wa_tcurr1_temp.
            REFRESH: ta_exch[].
          ENDLOOP.

          IF NOT ta_exch_tab IS INITIAL.
            SORT ta_exch_tab BY waers.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
*EOA by Gajanan - 08.04.2022 - CR#8000007907

  ENDIF.
*HR_99S_GET_QUARTER

*HR_99S_GET_DATES_QUARTER

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FINAL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM final_data .
  DATA : gv_count TYPE i,
         gv_werks TYPE t001w-werks.
  "SOA by Pooja on 11.11.2022
  DATA:      lv_count       TYPE char2,
*             lv_index       TYPE char2,
             lv_fcurr1      TYPE tcurr-fcurr,
             lv_ffact       TYPE tcurr-ffact,
             lv_excrate     TYPE tcurr-ukurs,
             lv_startdate_2 TYPE tcurr-gdatu,
             lv_enddate_2   TYPE tcurr-gdatu.
  "EOA by Pooja on 11.11.2022
  CONSTANTS:lc_sign TYPE c VALUE '+'.
  CONSTANTS:lc_ent      TYPE zco_midas-ent_tag VALUE 'Manual',
            lc_mat1(2)  TYPE c VALUE '51',
            lc_mat2(2)  TYPE c VALUE '11',
            lc_mat3(2)  TYPE c VALUE '12',
            lc_mat4(2)  TYPE c VALUE '20',
            lc_mat5(2)  TYPE c VALUE '21',
            lc_mat6(2)  TYPE c VALUE '13',
            lc_mat7(2)  TYPE c VALUE '32',
            lc_mat8(2)  TYPE c VALUE '52', "'5',"Changed by parashar 17.02.2020 CRDK967959
            lc_mat9(2)  TYPE c VALUE '91', "'9', "Changed by parashar 17.02.2020 CRDK967959
            lc_mat10(2) TYPE c VALUE '99', "Added by parashar 17.02.2020 CRDK967959
            lc_fkimg    TYPE zco_midas-b_fkimg VALUE '0.000',
            lc_rate     TYPE ukurs VALUE '1.00000',
            lc_rectify  TYPE zco_midas-ent_tag VALUE 'Rectify'. "Added by Gajanan on 30.07.2020 for CR#8000006372

  ta_keko_8 = ta_keko.
  ta_keko_9 = ta_keko.
  DATA(ta_keko_t1) = ta_keko.
  DATA(ta_keko_t2) = ta_keko.
  DATA(ta_keko_t3) = ta_keko.
  DATA(ta_keko_10) = ta_keko.
  DATA(ta_keko_11) = ta_keko.
  DATA(ta_keko_12) = ta_keko_7.
  DATA(ta_keko_13) = ta_keko_7.

  SORT ta_keko DESCENDING BY  kadky matnr werks.
  SORT ta_keko_7 DESCENDING BY  kadky matnr werks.
  SORT ta_keko_8 DESCENDING BY  kadky matnr bukrs.
  SORT ta_keko_9 DESCENDING BY  kadky matnr bukrs werks."Added parashar 8000005475 CRDK969082 07.04.2020
  SORT ta_keko_10 DESCENDING BY  kadky matnr werks.
  SORT ta_keko_11 DESCENDING BY  kadky matnr werks."Added parashar 8000005475 CRDK969082 07.04.2020
  SORT ta_keko_12 DESCENDING BY  kadky matnr werks.
  SORT ta_keko_13 DESCENDING BY  kadky matnr.
  SORT ta_trd BY matnr.
  SORT ta_trd_2 BY matnr. "Added by parashar CRDK965579 8000005475 13.11.2019
  SORT ta_trd_3 DESCENDING BY spmon bukrs matnr. "Added by parashar CRDK965579 8000005475 13.11.2019

***SOC by Gajanan on 21.10.2020 for CR#8000006372***
  DELETE ta_keko_05 WHERE bukrs IS INITIAL.
  IF NOT ta_keko_05 IS INITIAL.
    SORT ta_keko_05 DESCENDING BY kadky matnr werks.
    DATA(ta_keko_502) = ta_keko_05.
    DATA(ta_keko_503) = ta_keko_05.
    DATA(ta_keko_504) = ta_keko_05.
    DATA(ta_keko_505) = ta_keko_05.
    DATA(ta_keko_t8) = ta_keko_05.
    DATA(ta_keko_t9) = ta_keko_05.
    DATA(ta_keko_t10) = ta_keko_05.

    SORT ta_keko_502 DESCENDING BY kadky matnr bukrs.
    SORT ta_keko_503 DESCENDING BY kadky matnr bukrs werks.
    SORT ta_keko_504 DESCENDING BY kadky matnr werks.
    SORT ta_keko_505 DESCENDING BY kadky matnr werks.

    "Point 5.2 Logic
    SORT ta_keko_t8 BY bukrs.
    DELETE ADJACENT DUPLICATES FROM ta_keko_t8 COMPARING bukrs.
    DELETE ta_keko_t8 WHERE bukrs IS INITIAL.
*    SORT ta_keko_t9 BY bukrs matnr werks.
    SORT ta_keko_t9 BY  matnr werks.
    DELETE ADJACENT DUPLICATES FROM ta_keko_t9 COMPARING matnr werks.
    DELETE ta_keko_t8 WHERE bukrs IS INITIAL.
    SORT ta_keko_t10 BY matnr bukrs werks.

    LOOP AT ta_keko_t8 INTO DATA(wa_keko_t8).

      LOOP AT ta_keko_t9 INTO DATA(wa_keko_t9) WHERE bukrs = wa_keko_t8-bukrs.

        LOOP AT ta_keko_t10 INTO DATA(wa_keko_t10) WHERE matnr = wa_keko_t9-matnr AND bukrs = wa_keko_t9-bukrs .
          AT NEW werks.
            gv_count = gv_count + 1.
          ENDAT.
          CLEAR wa_keko_t10.
        ENDLOOP.
        IF gv_count GT 1.
          DELETE ta_keko_502 WHERE matnr = wa_keko_t9-matnr AND bukrs = wa_keko_t9-bukrs .
        ENDIF.
        CLEAR : gv_count,wa_keko_t9.

      ENDLOOP.
      CLEAR wa_keko_t8.
    ENDLOOP.

    "Point 5.4 Logic

    DATA(ta_keko_t11) = ta_keko_05.
    SORT ta_keko_t11 BY werks matnr .
    DELETE ADJACENT DUPLICATES FROM ta_keko_t11 COMPARING werks matnr.
    DELETE ta_keko_t11 WHERE werks IS INITIAL.
    DATA(ta_keko_t12) = ta_keko_05.
    SORT ta_keko_t12 BY matnr werks.

    LOOP AT ta_keko_t11 INTO DATA(wa_keko_t11).
      LOOP AT ta_keko_t12 INTO DATA(wa_keko_t12) WHERE matnr = wa_keko_t11-matnr.
        AT NEW werks.
          gv_count = gv_count + 1.
        ENDAT.
        CLEAR : wa_keko_t12.
      ENDLOOP.
      IF gv_count GT 1.
        DELETE ta_keko_504 WHERE matnr = wa_keko_t11-matnr.
      ENDIF.
      CLEAR : gv_count,wa_keko_t11.
    ENDLOOP.
  ENDIF.
***EOC by Gajanan on 21.10.2020 for CR#8000006372***

  DELETE ta_keko_8 WHERE bukrs = ''.
***** Point 8 logic
  SORT ta_keko_t1 BY bukrs.
  DELETE ADJACENT DUPLICATES FROM ta_keko_t1 COMPARING bukrs.
  DELETE ta_keko_t1 WHERE bukrs = ''.
*  SORT ta_keko_t2 BY bukrs matnr werks.
  SORT ta_keko_t2 BY  matnr werks.
  DELETE ADJACENT DUPLICATES FROM ta_keko_t2 COMPARING matnr werks.
  DELETE ta_keko_t2 WHERE bukrs = ''.
  SORT ta_keko_t3 BY  matnr bukrs werks.
  LOOP AT ta_keko_t1 INTO DATA(wa_keko_t1).

    LOOP AT ta_keko_t2 INTO DATA(wa_keko_t2) WHERE bukrs = wa_keko_t1-bukrs.

      LOOP AT ta_keko_t3 INTO DATA(wa_keko_t3) WHERE matnr = wa_keko_t2-matnr AND bukrs = wa_keko_t2-bukrs .
        AT NEW werks.
          gv_count = gv_count + 1.
        ENDAT.
      ENDLOOP.
      IF gv_count GT 1.
        DELETE ta_keko_8 WHERE matnr = wa_keko_t2-matnr AND bukrs = wa_keko_t2-bukrs .
      ENDIF.
      CLEAR : gv_count.
    ENDLOOP.
  ENDLOOP.
***** Point 10 logic
  DATA(ta_keko_t4) = ta_keko.
  SORT ta_keko_t4 BY werks matnr .
  DELETE ADJACENT DUPLICATES FROM ta_keko_t4 COMPARING werks matnr.
  DELETE ta_keko_t4 WHERE werks = ''.
  DATA(ta_keko_t5) = ta_keko.
  SORT ta_keko_t5 BY matnr werks.

  LOOP AT ta_keko_t4 INTO DATA(wa_keko_t4).
    LOOP AT ta_keko_t5 INTO DATA(wa_keko_t5) WHERE matnr = wa_keko_t4-matnr.
      AT NEW werks.
        gv_count = gv_count + 1.
      ENDAT.
    ENDLOOP.
    IF gv_count GT 1.
      DELETE ta_keko_10 WHERE matnr = wa_keko_t4-matnr.
    ENDIF.
    CLEAR : gv_count.
  ENDLOOP.
***** Point 12 logic
  DATA(ta_keko_t6) = ta_keko_7.
  SORT ta_keko_t6 BY werks matnr .
  DELETE ADJACENT DUPLICATES FROM ta_keko_t6 COMPARING werks matnr.
  DELETE ta_keko_t5 WHERE werks = ''.
  DATA(ta_keko_t7) = ta_keko_7.
  SORT ta_keko_t7 BY matnr werks.

  LOOP AT ta_keko_t6 INTO DATA(wa_keko_t6).
    LOOP AT ta_keko_t7 INTO DATA(wa_keko_t7) WHERE matnr = wa_keko_t6-matnr.
      AT NEW werks.
        gv_count = gv_count + 1.
      ENDAT.
    ENDLOOP.
    IF gv_count GT 1.
      DELETE ta_keko_12 WHERE matnr = wa_keko_t6-matnr.
    ENDIF.
    CLEAR : gv_count.
  ENDLOOP.
  """" SOC Done by Sushil as on 02.06.2021 against ticket no. 8000007120 for point-8
  LOOP AT ta_keko_8 ASSIGNING FIELD-SYMBOL(<fs_keko_8_tmp>).

    DATA(ta_keko_8_tmp) = ta_keko_8[].
    DELETE ta_keko_8_tmp WHERE matnr NE <fs_keko_8_tmp>-matnr .
    DELETE ta_keko_8_tmp WHERE  bukrs NE <fs_keko_8_tmp>-bukrs.
    SORT ta_keko_8_tmp BY bukrs matnr werks.

    DELETE ADJACENT DUPLICATES FROM ta_keko_8_tmp COMPARING bukrs matnr werks.
    DESCRIBE TABLE ta_keko_8_tmp LINES DATA(lv_count_keko).
    IF lv_count_keko GT 1.
      <fs_keko_8_tmp>-bukrs = abap_false.
    ENDIF.

  ENDLOOP.

  DELETE ta_keko_8 WHERE bukrs = abap_false.
  SORT ta_keko_8 DESCENDING BY  kadky matnr bukrs werks.
  """" EOC Done by Sushil as on 02.06.2021 against ticket no. 8000007120 for point-8

  LOOP AT ta_zco_midas INTO wa_zco_midas.
    MOVE-CORRESPONDING wa_zco_midas TO wa_final.
    SHIFT wa_zco_midas-matnr LEFT DELETING LEADING '0'.
    IF wa_zco_midas-ent_tag EQ lc_ent."'Manual'.
      wa_final-remark = text-001."'01. MANUAL GL'.
    ELSEIF wa_zco_midas-matnr(2) = lc_mat1."'51'.
      wa_final-remark = text-002."'02. Promotional Sales'.
***            """ Begin of commented section done by Sushil as on 17.06.2021 against ticket no. 8000007120
***            """ as this section of data move to different section-discussed by Shashank
***    ELSEIF wa_zco_midas-matnr(2) = lc_mat2 OR wa_zco_midas-matnr(2) = lc_mat3 OR wa_zco_midas-matnr(2) = lc_mat4 OR wa_zco_midas-matnr(2) = lc_mat5.
***      wa_final-remark = text-003."'03. RM/PM Sales'.
***            """ End of commented section done by Sushil as on 17.06.2021 against ticket no. 8000007120
***            """ as this section of data move to different section
*    ELSEIF ( wa_zco_midas-matnr(2) NE lc_mat6 OR wa_zco_midas-matnr(2) = lc_mat7 )
*      AND ( wa_zco_midas-matnr(2) = lc_mat6 OR wa_zco_midas-matnr(2) NE lc_mat7 ).
*      FIND FIRST OCCURRENCE OF REGEX '[A-Za-z]' IN wa_zco_midas-matnr.
*      IF sy-subrc EQ 0.
*        READ TABLE ta_dtm INTO wa_dtm WITH KEY bukrs = wa_zco_midas-bukrs locl_prd = wa_zco_midas-locl_prd spmon = wa_zco_midas-spmon.
*        IF sy-subrc EQ 0.
*          wa_final-waers      = wa_dtm-waerk.
*          wa_final-mat_cost   = wa_dtm-mat_cost.
*          wa_final-api_vc     = wa_dtm-api_vc.
*          wa_final-api_fc     = wa_dtm-api_fc.
*          wa_final-form_vc    = wa_dtm-formulation_vc.
*          wa_final-form_fc    = wa_dtm-formulation_fc.
*          wa_final-form_fc_y  = wa_dtm-form_fc.
*          wa_final-sub_margin =  wa_dtm-sub_margin.
*          wa_final-total      = wa_dtm-total_cost.
*          wa_final-remark = text-015."'15 - DTM Data'.
*        ELSE.
*      wa_final-remark = text-004."'04. Other Materials'.
*        ENDIF.
*      ENDIF.
*    ELSEIF ( wa_zco_midas-matnr(1) = lc_mat8 OR wa_zco_midas-matnr(1) = lc_mat9 ). "Commented by parashar 17.02.2020 CRDK967959
    ELSEIF ( wa_zco_midas-matnr(2) = lc_mat8 OR wa_zco_midas-matnr(2) = lc_mat9 OR wa_zco_midas-matnr(2) = lc_mat10 ). "Added by parashar 17.02.2020 CRDK967959
      wa_final-remark = text-004."'04. Other Materials'.
***Start of comment by Gajanan - 17.10.2024 - CR#8000003569***
*    ELSEIF ( wa_zco_midas-matnr(2) = lc_mat6 OR wa_zco_midas-matnr(2) = lc_mat7 ) AND wa_zco_midas-b_fkimg = lc_fkimg."'0.000'.
*      wa_final-remark = text-005."'05. DN/CN'.
***End of comment by Gajanan - 17.10.2024 - CR#8000003569***
    ELSE.

***SOC by Gajanan on 21.10.2020 for CR#8000006372***
      READ TABLE ta_keko_05 INTO wa_keko_05 WITH KEY matnr = wa_final-matnr
                                                     werks = wa_zco_midas-werks.

      IF sy-subrc = 0.
        wa_final-kadky = wa_keko_05-kadky.
        wa_final-kalnr = wa_keko_05-kalnr.
        wa_final-cost_plant = wa_keko_05-werks.
        wa_final-kalka = wa_keko_05-kalka.
        wa_final-tvers = wa_keko_05-tvers.
        wa_final-bwvar = wa_keko_05-bwvar.
        wa_final-kokrs  = wa_keko_05-kokrs.
        wa_final-verid  = wa_keko_05-verid.
        wa_final-stnum  = wa_keko_05-stnum.
        wa_final-stlan  = wa_keko_05-stlan.
        wa_final-stalt  = wa_keko_05-stalt.
        wa_final-losgr  = wa_keko_05-losgr.
        wa_final-meins  = wa_keko_05-meins.
        wa_final-feh_sta = wa_keko_05-feh_sta.
        wa_final-klvar  = wa_keko_05-klvar.
        wa_final-poper = wa_keko_05-poper.
        wa_final-bukrs = wa_keko_05-bukrs.
        wa_final-remark = text-028."5.1 Plant + Material - Priority Period
      ELSE.

        READ TABLE ta_keko_502 INTO DATA(wa_keko_502)
        WITH KEY matnr = wa_final-matnr
                bukrs = wa_zco_midas-vkorg."wa_zco_midas-bukrs replaced by wa_zco_midas-vkorg as bukrs value is blank-SOC done by Sushil as on 01.06.2021 against 8000007120.
*        IF sy-subrc EQ 0.
        IF sy-subrc EQ 0 AND wa_zco_midas-werks NE co_trd."'TRD'.
          wa_final-remark = text-029."5.2 Material - Single Plant + Company code - Priority Period
          wa_final-kadky = wa_keko_502-kadky.
          wa_final-kalnr = wa_keko_502-kalnr.
          wa_final-cost_plant = wa_keko_502-werks.
          wa_final-kalka = wa_keko_502-kalka.
          wa_final-tvers = wa_keko_502-tvers.
          wa_final-bwvar = wa_keko_502-bwvar.
          wa_final-kokrs  = wa_keko_502-kokrs.
          wa_final-verid  = wa_keko_502-verid.
          wa_final-stnum  = wa_keko_502-stnum.
          wa_final-stlan  = wa_keko_502-stlan.
          wa_final-stalt  = wa_keko_502-stalt.
          wa_final-losgr  = wa_keko_502-losgr.
          wa_final-meins  = wa_keko_502-meins.
          wa_final-feh_sta = wa_keko_502-feh_sta.
          wa_final-klvar  = wa_keko_502-klvar.
          wa_final-poper = wa_keko_502-poper.
          wa_final-bukrs = wa_keko_502-bukrs.

        ELSE.

          READ TABLE ta_keko_503 INTO DATA(wa_keko_503)
          WITH KEY matnr = wa_final-matnr
                bukrs = wa_zco_midas-vkorg."wa_zco_midas-bukrs replaced by wa_zco_midas-vkorg as bukrs value is blank-SOC done by Sushil as on 01.06.2021 against 8000007120.
*          IF sy-subrc EQ 0.
          IF sy-subrc EQ 0 AND wa_zco_midas-werks NE co_trd."'TRD'.
            wa_final-remark = text-030."5.3 Material - Multiple Plant + Company code - Priority Period
            wa_final-kadky = wa_keko_503-kadky.
            wa_final-kalnr = wa_keko_503-kalnr.
            wa_final-cost_plant = wa_keko_503-werks.
            wa_final-kalka = wa_keko_503-kalka.
            wa_final-tvers = wa_keko_503-tvers.
            wa_final-bwvar = wa_keko_503-bwvar.
            wa_final-kokrs  = wa_keko_503-kokrs.
            wa_final-verid  = wa_keko_503-verid.
            wa_final-stnum  = wa_keko_503-stnum.
            wa_final-stlan  = wa_keko_503-stlan.
            wa_final-stalt  = wa_keko_503-stalt.
            wa_final-losgr  = wa_keko_503-losgr.
            wa_final-meins  = wa_keko_503-meins.
            wa_final-feh_sta = wa_keko_503-feh_sta.
            wa_final-klvar  = wa_keko_503-klvar.
            wa_final-poper = wa_keko_503-poper.
            wa_final-bukrs = wa_keko_503-bukrs.


            """ Begin of commented section done by Sushil as on 01.06.2021 against ticket no. 8000007120
            """ as this section of data move to different section
***          ELSE.
***            READ TABLE ta_keko_504 INTO DATA(wa_keko_504) WITH KEY matnr = wa_final-matnr.
***            IF sy-subrc EQ 0 AND wa_zco_midas-werks NE co_trd."'TRD'.
****            IF sy-subrc EQ 0.
***              wa_final-remark = text-031."5.4 Material - Single Plant - Priority Period
***              wa_final-kadky = wa_keko_504-kadky.
***              wa_final-kalnr = wa_keko_504-kalnr.
***              wa_final-cost_plant = wa_keko_504-werks.
***              wa_final-kalka = wa_keko_504-kalka.
***              wa_final-tvers = wa_keko_504-tvers.
***              wa_final-bwvar = wa_keko_504-bwvar.
***              wa_final-kokrs  = wa_keko_504-kokrs.
***              wa_final-verid  = wa_keko_504-verid.
***              wa_final-stnum  = wa_keko_504-stnum.
***              wa_final-stlan  = wa_keko_504-stlan.
***              wa_final-stalt  = wa_keko_504-stalt.
***              wa_final-losgr  = wa_keko_504-losgr.
***              wa_final-meins  = wa_keko_504-meins.
***              wa_final-feh_sta = wa_keko_504-feh_sta.
***              wa_final-klvar  = wa_keko_504-klvar.
***              wa_final-poper = wa_keko_504-poper.
***              wa_final-bukrs = wa_keko_504-bukrs.
***            ELSE.
***
***              READ TABLE ta_keko_505 INTO DATA(wa_keko_505) WITH KEY matnr = wa_final-matnr.
****              IF sy-subrc EQ 0.
***              IF sy-subrc EQ 0 AND wa_zco_midas-werks NE co_trd."'TRD'.
***                wa_final-remark = text-032."5.5 Material - Multiple Plant - Priority Period
***                wa_final-kadky = wa_keko_505-kadky.
***                wa_final-kalnr = wa_keko_505-kalnr.
***                wa_final-cost_plant = wa_keko_505-werks.
***                wa_final-kalka = wa_keko_505-kalka.
***                wa_final-tvers = wa_keko_505-tvers.
***                wa_final-bwvar = wa_keko_505-bwvar.
***                wa_final-kokrs  = wa_keko_505-kokrs.
***                wa_final-verid  = wa_keko_505-verid.
***                wa_final-stnum  = wa_keko_505-stnum.
***                wa_final-stlan  = wa_keko_505-stlan.
***                wa_final-stalt  = wa_keko_505-stalt.
***                wa_final-losgr  = wa_keko_505-losgr.
***                wa_final-meins  = wa_keko_505-meins.
***                wa_final-feh_sta = wa_keko_505-feh_sta.
***                wa_final-klvar  = wa_keko_505-klvar.
***                wa_final-poper = wa_keko_505-poper.
***                wa_final-bukrs = wa_keko_505-bukrs.
***            """ End of commented section done by Sushil as on 01.06.2021 against ticket no. 8000007120
***            """ as this section of data move to different section
          ELSE.
***EOC by Gajanan on 21.10.2020 for CR#8000006372***

            READ TABLE ta_keko INTO wa_keko WITH KEY matnr = wa_final-matnr werks = wa_zco_midas-werks.
            IF sy-subrc EQ 0.
              wa_final-kadky = wa_keko-kadky.
              wa_final-kalnr = wa_keko-kalnr.
              wa_final-cost_plant = wa_keko-werks.
              wa_final-kalka = wa_keko-kalka.
              wa_final-tvers = wa_keko-tvers.
              wa_final-bwvar = wa_keko-bwvar.
              wa_final-kokrs  = wa_keko-kokrs.
              wa_final-verid  = wa_keko-verid.
              wa_final-stnum  = wa_keko-stnum.
              wa_final-stlan  = wa_keko-stlan.
              wa_final-stalt  = wa_keko-stalt.
              wa_final-losgr  = wa_keko-losgr.
              wa_final-meins  = wa_keko-meins.
              wa_final-feh_sta = wa_keko-feh_sta.
              wa_final-klvar  = wa_keko-klvar.
              wa_final-poper = wa_keko-poper.
              wa_final-bukrs = wa_keko-bukrs.
              wa_final-remark = text-006."'06. Plant + Material - Last 4 Qtrs'.

            ELSE.
***            """ Begin of commented section done by Sushil as on 01.06.2021 against ticket no. 8000007120
***            """ as this section of data move to different section
***                  READ TABLE ta_keko_7 INTO wa_keko_7 WITH KEY matnr = wa_final-matnr werks = wa_zco_midas-werks.
***                  IF sy-subrc EQ 0.
***                    wa_final-kadky = wa_keko_7-kadky.
***                    wa_final-kalnr = wa_keko_7-kalnr.
***                    wa_final-cost_plant = wa_keko_7-werks.
***                    wa_final-kalka = wa_keko_7-kalka.
***                    wa_final-tvers = wa_keko_7-tvers.
***                    wa_final-bwvar = wa_keko_7-bwvar.
***                    wa_final-kokrs  = wa_keko_7-kokrs.
***                    wa_final-verid  = wa_keko_7-verid.
***                    wa_final-stnum  = wa_keko_7-stnum.
***                    wa_final-stlan  = wa_keko_7-stlan.
***                    wa_final-stalt  = wa_keko_7-stalt.
***                    wa_final-losgr  = wa_keko_7-losgr.
***                    wa_final-meins  = wa_keko_7-meins.
***                    wa_final-feh_sta = wa_keko_7-feh_sta.
***                    wa_final-klvar  = wa_keko_7-klvar.
***                    wa_final-poper = wa_keko_7-poper.
***                    wa_final-bukrs = wa_keko_7-bukrs.
***                    wa_final-remark = text-007."'07. Plant + Material - Current Qtrs'.
***                  ELSE.
***            """ End of commented section done by Sushil as on 01.06.2021 against ticket no. 8000007120
***            """ as this section of data move to different section

              READ TABLE ta_keko_8 INTO wa_keko_8
                WITH KEY matnr = wa_final-matnr
                         bukrs = wa_zco_midas-vkorg. "wa_zco_midas-bukrs replaced by wa_zco_midas-vkorg as bukrs value is blank-SOC done by Sushil as on 01.06.2021 against 8000007120.


              IF sy-subrc EQ 0 AND wa_zco_midas-werks NE co_trd."'TRD'.
                wa_final-remark     = text-008."'08. Material - Last 4 Qtrs - Single Plant + Company code'.
                wa_final-kadky      = wa_keko_8-kadky.
                wa_final-kalnr      = wa_keko_8-kalnr.
                wa_final-cost_plant = wa_keko_8-werks.
                wa_final-kalka      = wa_keko_8-kalka.
                wa_final-tvers      = wa_keko_8-tvers.
                wa_final-bwvar      = wa_keko_8-bwvar.
                wa_final-kokrs      = wa_keko_8-kokrs.
                wa_final-verid      = wa_keko_8-verid.
                wa_final-stnum      = wa_keko_8-stnum.
                wa_final-stlan      = wa_keko_8-stlan.
                wa_final-stalt      = wa_keko_8-stalt.
                wa_final-losgr      = wa_keko_8-losgr.
                wa_final-meins      = wa_keko_8-meins.
                wa_final-feh_sta    = wa_keko_8-feh_sta.
                wa_final-klvar      = wa_keko_8-klvar.
                wa_final-poper      = wa_keko_8-poper.
                wa_final-bukrs      = wa_keko_8-bukrs.

              ELSE.

                READ TABLE ta_keko_9 INTO wa_keko_9
                  WITH KEY matnr = wa_final-matnr
                           bukrs = wa_zco_midas-vkorg. "wa_zco_midas-bukrs replaced by wa_zco_midas-vkorg as bukrs value is blank-SOC done by Sushil as on 01.06.2021 against 8000007120.
                IF sy-subrc EQ 0 AND wa_zco_midas-werks NE co_trd."'TRD'.
                  wa_final-remark     = text-009."'09. Material - Last 4 Qtrs - Multiple Plant + Company code'.
                  wa_final-kadky      = wa_keko_9-kadky.
                  wa_final-kalnr      = wa_keko_9-kalnr.
                  wa_final-cost_plant = wa_keko_9-werks.
                  wa_final-kalka      = wa_keko_9-kalka.
                  wa_final-tvers      = wa_keko_9-tvers.
                  wa_final-bwvar      = wa_keko_9-bwvar.
                  wa_final-kokrs      = wa_keko_9-kokrs.
                  wa_final-verid      = wa_keko_9-verid.
                  wa_final-stnum      = wa_keko_9-stnum.
                  wa_final-stlan      = wa_keko_9-stlan.
                  wa_final-stalt      = wa_keko_9-stalt.
                  wa_final-losgr      = wa_keko_9-losgr.
                  wa_final-meins      = wa_keko_9-meins.
                  wa_final-feh_sta    = wa_keko_9-feh_sta.
                  wa_final-klvar      = wa_keko_9-klvar.
                  wa_final-poper      = wa_keko_9-poper.
                  wa_final-bukrs      = wa_keko_9-bukrs.

                  """ SOC done by Sushil as on 02.06.2021 for 9.1 against ticket no. 8000007120
                ELSE.
                  READ TABLE ta_zco_pgi_rate_c INTO DATA(wa_pgi_rate_901)
                   WITH KEY matnr = wa_final-matnr
                            rbukrs = wa_zco_midas-vkorg. "wa_zco_midas-bukrs replaced by wa_zco_midas-vkorg as bukrs value is blank-SOC done by Sushil as on 01.06.2021 against 8000007120.
                  IF sy-subrc EQ 0.
                    wa_final-remark       = text-901."9.1 - Material + Company Code - PGI Rate for trading - Current Month
                    wa_final-losgr        = '1000'.

                    "SOC by Pooja on 02.01.2023 CR#8000007907
*                    wa_final-mat_cost     = wa_pgi_rate_901-ukurs.
*                    wa_final-total_cost   = wa_pgi_rate_901-ukurs.
*                    wa_final-inr_rate     = wa_pgi_rate_901-ukurs1.
*                    wa_final-mat_value    = wa_zco_midas-b_fkimg * wa_pgi_rate_901-ukurs.
                    wa_final-mat_cost     = wa_pgi_rate_901-ukurs_pur.
                    wa_final-total_cost   = wa_pgi_rate_901-ukurs_pur.
                    wa_final-inr_rate     = wa_pgi_rate_901-ukurs_fx.
                    wa_final-mat_value    = wa_zco_midas-b_fkimg * wa_pgi_rate_901-ukurs_pur.
                    "EOC by Pooja on 02.01.2023 CR#8000007907
                    wa_final-cost_curr    = wa_pgi_rate_901-waers.
                    """ SOC Done by Sushil as on 20.08.2021 against ticket no. 8000007312
                    IF wa_final-losgr IS NOT INITIAL.
                      wa_final-mat_value = wa_final-mat_value / wa_final-losgr.
                    ENDIF.
                    """ EOC Done by Sushil as on 20.08.2021 against ticket no. 8000007312
                    wa_final-cost_plant = 'TRD'. "Added by Gajanan - 17.10.2024 - CR#8000003569
                  ELSE.

                    READ TABLE ta_zco_pgi_rate_p INTO DATA(wa_pgi_rate_902)
                    WITH KEY matnr = wa_final-matnr
                             rbukrs = wa_zco_midas-vkorg. "wa_zco_midas-bukrs replaced by wa_zco_midas-vkorg as bukrs value is blank-SOC done by Sushil as on 01.06.2021 against 8000007120.
                    IF sy-subrc EQ 0.
                      wa_final-remark       = text-902."9.2 - Material + Company Code - PGI Rate for trading - Previous Month
                      wa_final-losgr        = '1000'.
                      "SOC by Pooja on 02.01.2023 CR#8000007907
*                      wa_final-mat_cost     = wa_pgi_rate_902-ukurs.
*                      wa_final-total_cost   = wa_pgi_rate_902-ukurs.
*                      wa_final-inr_rate     = wa_pgi_rate_902-ukurs1.
*                      wa_final-mat_value    = wa_zco_midas-b_fkimg * wa_pgi_rate_902-ukurs.
                      wa_final-mat_cost     = wa_pgi_rate_902-ukurs_pur.
                      wa_final-total_cost   = wa_pgi_rate_902-ukurs_pur.
                      wa_final-inr_rate     = wa_pgi_rate_902-ukurs_fx.
                      wa_final-mat_value    = wa_zco_midas-b_fkimg * wa_pgi_rate_902-ukurs_pur.
                      "EOC by Pooja on 02.01.2023 CR#8000007907
                      wa_final-cost_curr    = wa_pgi_rate_902-waers.
                      """ SOC Done by Sushil as on 20.08.2021 against ticket no. 8000007312
                      IF wa_final-losgr IS NOT INITIAL.
                        wa_final-mat_value = wa_final-mat_value / wa_final-losgr.
                      ENDIF.
                      """ EOC Done by Sushil as on 20.08.2021 against ticket no. 8000007312
                      """ EOC done by Sushil as on 02.06.2021 for 9.1 against ticket no. 8000007120
                      wa_final-cost_plant = 'TRD'. "Added by Gajanan - 17.10.2024 - CR#8000003569

                    ELSE.



                      READ TABLE ta_keko_10 INTO DATA(wa_keko_10) WITH KEY matnr = wa_final-matnr.
                      IF sy-subrc EQ 0 AND wa_zco_midas-werks NE co_trd."'TRD'.
                        wa_final-remark = text-010."'10. Material - Last 4 Qtrs - Single Plant'.
                        wa_final-kadky = wa_keko_10-kadky.
                        wa_final-kalnr = wa_keko_10-kalnr.
                        wa_final-cost_plant = wa_keko_10-werks.
                        wa_final-kalka = wa_keko_10-kalka.
                        wa_final-tvers = wa_keko_10-tvers.
                        wa_final-bwvar = wa_keko_10-bwvar.
                        wa_final-kokrs  = wa_keko_10-kokrs.
                        wa_final-verid  = wa_keko_10-verid.
                        wa_final-stnum  = wa_keko_10-stnum.
                        wa_final-stlan  = wa_keko_10-stlan.
                        wa_final-stalt  = wa_keko_10-stalt.
                        wa_final-losgr  = wa_keko_10-losgr.
                        wa_final-meins  = wa_keko_10-meins.
                        wa_final-feh_sta = wa_keko_10-feh_sta.
                        wa_final-klvar  = wa_keko_10-klvar.
                        wa_final-poper = wa_keko_10-poper.
                        wa_final-bukrs = wa_keko_10-bukrs.
                      ELSE.
                        READ TABLE ta_keko_11 INTO DATA(wa_keko_11) WITH KEY matnr = wa_final-matnr.
                        IF sy-subrc EQ 0 AND wa_zco_midas-werks NE co_trd."'TRD'.
                          wa_final-remark = text-011."'11. Material - Last 4 Qtrs - Multiple Plant'.
                          wa_final-kadky = wa_keko_11-kadky.
                          wa_final-kalnr = wa_keko_11-kalnr.
                          wa_final-cost_plant = wa_keko_11-werks.
                          wa_final-kalka = wa_keko_11-kalka.
                          wa_final-tvers = wa_keko_11-tvers.
                          wa_final-bwvar = wa_keko_11-bwvar.
                          wa_final-kokrs  = wa_keko_11-kokrs.
                          wa_final-verid  = wa_keko_11-verid.
                          wa_final-stnum  = wa_keko_11-stnum.
                          wa_final-stlan  = wa_keko_11-stlan.
                          wa_final-stalt  = wa_keko_11-stalt.
                          wa_final-losgr  = wa_keko_11-losgr.
                          wa_final-meins  = wa_keko_11-meins.
                          wa_final-feh_sta = wa_keko_11-feh_sta.
                          wa_final-klvar  = wa_keko_11-klvar.
                          wa_final-poper = wa_keko_11-poper.
                          wa_final-bukrs = wa_keko_11-bukrs.

***            """ Begin of commented section done by Sushil as on 03.06.2021 against ticket no. 8000007120
***            """ as this section of data move to different section
***                            ELSE.
***                            READ TABLE ta_keko_12 INTO DATA(wa_keko_12) WITH KEY matnr = wa_final-matnr.
***                            IF sy-subrc EQ 0 AND wa_zco_midas-werks NE co_trd."'TRD'.
***                              wa_final-remark = text-012."'12. Material - Current Qtrs - Single Plant'.
***                              wa_final-kadky = wa_keko_12-kadky.
***                              wa_final-kalnr = wa_keko_12-kalnr.
***                              wa_final-cost_plant = wa_keko_12-werks.
***                              wa_final-kalka = wa_keko_12-kalka.
***                              wa_final-tvers = wa_keko_12-tvers.
***                              wa_final-bwvar = wa_keko_12-bwvar.
***                              wa_final-kokrs  = wa_keko_12-kokrs.
***                              wa_final-verid  = wa_keko_12-verid.
***                              wa_final-stnum  = wa_keko_12-stnum.
***                              wa_final-stlan  = wa_keko_12-stlan.
***                              wa_final-stalt  = wa_keko_12-stalt.
***                              wa_final-losgr  = wa_keko_12-losgr.
***                              wa_final-meins  = wa_keko_12-meins.
***                              wa_final-feh_sta = wa_keko_12-feh_sta.
***                              wa_final-klvar  = wa_keko_12-klvar.
***                              wa_final-poper = wa_keko_12-poper.
***                              wa_final-bukrs = wa_keko_12-bukrs.
***                            ELSE.
***                              READ TABLE ta_keko_13 INTO DATA(wa_keko_13) WITH KEY matnr = wa_final-matnr.
***                              IF sy-subrc EQ 0 AND wa_zco_midas-werks NE co_trd."'TRD'.
***                                wa_final-remark = text-013."'13. Material - Current Qtrs – Multiple Plant'.
***                                wa_final-kadky = wa_keko_13-kadky.
***                                wa_final-kalnr = wa_keko_13-kalnr.
***                                wa_final-cost_plant = wa_keko_13-werks.
***                                wa_final-kalka = wa_keko_13-kalka.
***                                wa_final-tvers = wa_keko_13-tvers.
***                                wa_final-bwvar = wa_keko_13-bwvar.
***                                wa_final-kokrs  = wa_keko_13-kokrs.
***                                wa_final-verid  = wa_keko_13-verid.
***                                wa_final-stnum  = wa_keko_13-stnum.
***                                wa_final-stlan  = wa_keko_13-stlan.
***                                wa_final-stalt  = wa_keko_13-stalt.
***                                wa_final-losgr  = wa_keko_13-losgr.
***                                wa_final-meins  = wa_keko_13-meins.
***                                wa_final-feh_sta = wa_keko_13-feh_sta.
***                                wa_final-klvar  = wa_keko_13-klvar.
***                                wa_final-poper = wa_keko_13-poper.
***                                wa_final-bukrs = wa_keko_13-bukrs.
***                              ELSE.
***            """ End of commented section done by Sushil as on 03.06.2021 against ticket no. 8000007120
***            """ as this section of data move to different section

*                      READ TABLE ta_trd INTO wa_trd WITH KEY matnr = wa_final-matnr.
*                      IF sy-subrc EQ 0.
*                        wa_final-remark = text-014."'14. Trading Purchase Rate'.
*                        wa_final-losgr  = co_losgr."'1000.000'. "Added by parashar CRDK965579 8000005475 13.11.2019
*                        wa_final-bukrs = wa_trd-bukrs.
*                        wa_final-mat_cost = ( wa_trd-dmbtr / wa_trd-bamng ) * 1000.
*                      ELSE.
*** Added by parashar CRDK965579 8000005475 13.11.2019
                          """ SOC Done by Sushil as on 03.06.2021 against ticket no. 8000007120
                        ELSE.
                          DATA(lv_zco_midas_matnr) = |{ wa_zco_midas-matnr ALPHA = IN }|.
                          READ TABLE ta_zco_pgi_rate_1401 INTO DATA(wa_zco_pgi_rate_1401)
                           WITH KEY matnr   = lv_zco_midas_matnr. "wa_zco_midas-matnr.

                          IF sy-subrc IS INITIAL.
                            wa_final-remark       = text-401."14.1 Material + Single Company code - PGI Rate for Trading ( - Current Month )
                            wa_final-losgr        = '1000'.

                            "SOC by Pooja on 02.01.2023 CR#8000007907
*                            wa_final-mat_cost     = wa_zco_pgi_rate_1401-ukurs.
*                            wa_final-total_cost   = wa_zco_pgi_rate_1401-ukurs.
*                            wa_final-inr_rate     = wa_zco_pgi_rate_1401-ukurs1.
*                            wa_final-mat_value    = wa_zco_midas-b_fkimg * wa_zco_pgi_rate_1401-ukurs.
                            wa_final-mat_cost     = wa_zco_pgi_rate_1401-ukurs_pur.
                            wa_final-total_cost   = wa_zco_pgi_rate_1401-ukurs_pur.
                            wa_final-inr_rate     = wa_zco_pgi_rate_1401-ukurs_fx.
                            wa_final-mat_value    = wa_zco_midas-b_fkimg * wa_zco_pgi_rate_1401-ukurs_pur.
                            "EOC by Pooja on 02.01.2023 CR#8000007907

                            wa_final-cost_curr    = wa_zco_pgi_rate_1401-waers.
                            """ SOC Done by Sushil as on 20.08.2021 against ticket no. 8000007312
                            IF wa_final-losgr IS NOT INITIAL.
                              wa_final-mat_value = wa_final-mat_value / wa_final-losgr.
                            ENDIF.
                            """ EOC Done by Sushil as on 20.08.2021 against ticket no. 8000007312
                            wa_final-cost_plant = 'TRD'. "Added by Gajanan - 17.10.2024 - CR#8000003569
                          ELSE.

                            READ TABLE ta_zco_pgi_rate_1402 INTO wa_zco_pgi_rate_1401
                           WITH KEY matnr   = lv_zco_midas_matnr. "wa_zco_midas-matnr.

                            IF sy-subrc IS INITIAL.
                              wa_final-remark       = text-402."14.2 Material + Single Company code - PGI Rate for Trading ( - Previous Month )
                              wa_final-losgr        = '1000'.

                              "SOC by Pooja on 02.01.2023 CR#8000007907
*                              wa_final-mat_cost     = wa_zco_pgi_rate_1401-ukurs.
*                              wa_final-total_cost   = wa_zco_pgi_rate_1401-ukurs.
*                              wa_final-inr_rate     = wa_zco_pgi_rate_1401-ukurs1.
*                              wa_final-mat_value    = wa_zco_midas-b_fkimg * wa_zco_pgi_rate_1401-ukurs.
                              wa_final-mat_cost     = wa_zco_pgi_rate_1401-ukurs_pur.
                              wa_final-total_cost   = wa_zco_pgi_rate_1401-ukurs_pur.
                              wa_final-inr_rate     = wa_zco_pgi_rate_1401-ukurs_fx.
                              wa_final-mat_value    = wa_zco_midas-b_fkimg * wa_zco_pgi_rate_1401-ukurs_pur.
                              "EOC by Pooja on 02.01.2023 CR#8000007907

                              wa_final-cost_curr    = wa_zco_pgi_rate_1401-waers.
                              """ SOC Done by Sushil as on 20.08.2021 against ticket no. 8000007312
                              IF wa_final-losgr IS NOT INITIAL.
                                wa_final-mat_value = wa_final-mat_value / wa_final-losgr.
                              ENDIF.
                              """ EOC Done by Sushil as on 20.08.2021 against ticket no. 8000007312
                              wa_final-cost_plant = 'TRD'. "Added by Gajanan - 17.10.2024 - CR#8000003569

                            ELSE.

                              READ TABLE ta_zco_pgi_rate_1403 INTO wa_zco_pgi_rate_1401
                              WITH KEY matnr   = lv_zco_midas_matnr. "wa_zco_midas-matnr.

                              IF sy-subrc IS INITIAL.
                                wa_final-remark       = text-403."14.3 Material + Multiple Company code - PGI Rate for Trading ( - Current Month )
                                wa_final-losgr        = '1000'.

                                "SOC by Pooja on 02.01.2023 CR#8000007907
*                                wa_final-mat_cost     = wa_zco_pgi_rate_1401-ukurs.
*                                wa_final-total_cost   = wa_zco_pgi_rate_1401-ukurs.
*                                wa_final-inr_rate     = wa_zco_pgi_rate_1401-ukurs1.
*                                wa_final-mat_value    = wa_zco_midas-b_fkimg * wa_zco_pgi_rate_1401-ukurs.
                                wa_final-mat_cost     = wa_zco_pgi_rate_1401-ukurs_pur.
                                wa_final-total_cost   = wa_zco_pgi_rate_1401-ukurs_pur.
                                wa_final-inr_rate     = wa_zco_pgi_rate_1401-ukurs_fx.
                                wa_final-mat_value    = wa_zco_midas-b_fkimg * wa_zco_pgi_rate_1401-ukurs_pur.
                                "EOC by Pooja on 02.01.2023 CR#8000007907

                                wa_final-cost_curr    = wa_zco_pgi_rate_1401-waers.
                                """ SOC Done by Sushil as on 20.08.2021 against ticket no. 8000007312
                                IF wa_final-losgr IS NOT INITIAL.
                                  wa_final-mat_value = wa_final-mat_value / wa_final-losgr.
                                ENDIF.
                                """ EOC Done by Sushil as on 20.08.2021 against ticket no. 8000007312
                                wa_final-cost_plant = 'TRD'. "Added by Gajanan - 28.10.2024 - CR#8000003569
                              ELSE.

                                READ TABLE ta_zco_pgi_rate_1404 INTO wa_zco_pgi_rate_1401
                                  WITH KEY matnr   = lv_zco_midas_matnr. "wa_zco_midas-matnr.

                                IF sy-subrc IS INITIAL.
                                  wa_final-remark       = text-404."14.4 Material + Multiple Company code - PGI Rate for Trading ( - Previous Period Month )
                                  wa_final-losgr        = '1000'.
                                  wa_final-mat_cost     = wa_zco_pgi_rate_1401-ukurs.
                                  wa_final-total_cost   = wa_zco_pgi_rate_1401-ukurs.
                                  wa_final-cost_curr    = wa_zco_pgi_rate_1401-waers.
                                  wa_final-inr_rate     = wa_zco_pgi_rate_1401-ukurs1.
                                  wa_final-mat_value    = wa_zco_midas-b_fkimg * wa_zco_pgi_rate_1401-ukurs.
                                  """ SOC Done by Sushil as on 20.08.2021 against ticket no. 8000007312
                                  IF wa_final-losgr IS NOT INITIAL.
                                    wa_final-mat_value = wa_final-mat_value / wa_final-losgr.
                                  ENDIF.
                                  """ EOC Done by Sushil as on 20.08.2021 against ticket no. 8000007312
                                  """ EOC Done by Sushil as on 03.06.2021 against ticket no. 8000007120
                                  wa_final-cost_plant = 'TRD'. "Added by Gajanan - 17.10.2024 - CR#8000003569
***            """ Begin of commented section done by Sushil as on 03.06.2021 against ticket no. 8000007120
***            """ as this section of data move to different section
***                                READ TABLE ta_trd_2 INTO wa_trd_2 WITH KEY matnr = wa_final-matnr.
***                                IF sy-subrc EQ 0.
****                          wa_final-remark = text-025."'14. Trading Purchase Rate'.
***                                  wa_final-remark = text-014."'14. Trading Purchase Rate'.
***                                  wa_final-losgr  = co_losgr."'1000.000'.
***                                  wa_final-bukrs = wa_trd_2-bukrs.
***                                  IF wa_trd_2-bamng IS NOT INITIAL. "Added parashar 8000005475 CRDK969082 07.04.2020
***                                    wa_final-mat_cost = ( wa_trd_2-dmbtr / wa_trd_2-bamng ) * 1000.
***                                  ENDIF.
***            """ End of commented section done by Sushil as on 03.06.2021 against ticket no. 8000007120
***            """ as this section of data move to different section

                                ELSE.
***            """ Begin of commented section done by Sushil as on 17.06.2021 against ticket no. 8000007120
***            """ as this section of data move to different section
***                                  READ TABLE ta_trd_3 INTO wa_trd_3 WITH KEY matnr = wa_final-matnr.
***                                  IF sy-subrc EQ 0.
***                                    wa_final-remark = text-026."'14. Trading Purchase Rate'.
***                                    wa_final-losgr  = co_losgr."'1000.000'.
***                                    wa_final-bukrs = wa_trd_3-bukrs.
***                                    IF wa_trd_3-bamng IS NOT INITIAL."Added parashar 8000005475 CRDK969082 07.04.2020
***                                      wa_final-mat_cost = ( wa_trd_3-dmbtr / wa_trd_3-bamng ) * 1000.
***                                    ENDIF.
*** Added upto by parashar CRDK965579 8000005475 13.11.2019
***                                  ELSE.
***            """ Begin of commented section done by Sushil as on 17.06.2021 against ticket no. 8000007120
***            """ as this section of data move to different section
*                            READ TABLE ta_dtm INTO wa_dtm WITH KEY bukrs = wa_zco_midas-bukrs locl_prd = wa_zco_midas-locl_prd spmon = wa_zco_midas-spmon.
                                  READ TABLE ta_dtm INTO wa_dtm WITH KEY matnr = wa_final-matnr spmon = wa_zco_midas-spmon. "Added by parashar 8000005475 CRDK965903 26.11.2019
                                  IF sy-subrc EQ 0.
                                    wa_final-waers      = wa_dtm-waerk.
                                    wa_final-mat_cost   = wa_dtm-mat_cost.
                                    wa_final-api_vc     = wa_dtm-api_vc.
                                    wa_final-api_fc     = wa_dtm-api_fc.
                                    wa_final-form_vc    = wa_dtm-formulation_vc.
                                    wa_final-form_fc    = wa_dtm-formulation_fc.
                                    wa_final-form_fc_y  = wa_dtm-form_fc.
                                    wa_final-sub_margin =  wa_dtm-sub_margin.
                                    wa_final-total      = wa_dtm-total_cost.
                                    wa_final-remark = text-015."'15 - DTM Data'.
*                                  ENDIF.
                                    "SOC by Pooja on 02.01.2023 CR#8000007907
                                    wa_final-cost_plant = 'TRD'. "Added by Gajanan - 17.10.2024 - CR#8000003569
                                  ELSE.
                                    "16.1 logic begin
                                    READ TABLE ta_zco_pgi_rate_1601_final INTO DATA(wa_pgi_rate_1601)
                                                      WITH KEY matnr = wa_final-matnr
                                                               rbukrs = wa_zco_midas-vkorg. "wa_zco_midas-bukrs replaced by wa_zco_midas-vkorg as bukrs value is blank-SOC done by Sushil as on 01.06.2021 against 8000007120.
                                    IF sy-subrc EQ 0.
                                      wa_final-remark       = text-161."16.1 - 16.1 - Material + Company Code - PGI Rate for trading - Latest period till 3 years
                                      wa_final-losgr        = '1000'.
                                      wa_final-mat_cost     = wa_pgi_rate_1601-ukurs_pur.
                                      wa_final-total_cost   = wa_pgi_rate_1601-ukurs_pur.
                                      wa_final-inr_rate     = wa_pgi_rate_1601-ukurs_fx.
                                      wa_final-mat_value    = wa_zco_midas-b_fkimg * wa_pgi_rate_1601-ukurs_pur.
                                      wa_final-cost_curr    = wa_pgi_rate_1601-waers.
                                      IF wa_final-losgr IS NOT INITIAL.
                                        wa_final-mat_value = wa_final-mat_value / wa_final-losgr.
                                      ENDIF.
                                      "16.1 logic end
                                      wa_final-cost_plant = 'TRD'. "Added by Gajanan - 17.10.2024 - CR#8000003569
                                    ELSE.
                                      "16.2 logic begin
                                      DATA(lv_zco_midas_matnr1) = |{ wa_zco_midas-matnr ALPHA = IN }|.
                                      READ TABLE ta_zco_pgi_rate_1602_final INTO DATA(wa_zco_pgi_rate_1602_final)
                                       WITH KEY matnr   = lv_zco_midas_matnr1. "wa_zco_midas-matnr.

                                      IF sy-subrc IS INITIAL.
                                        wa_final-remark       = text-162."16.2 Material + Single Company code - PGI Rate for Trading - Latest period till 3 years
                                        wa_final-losgr        = '1000'.
                                        wa_final-mat_cost     = wa_zco_pgi_rate_1602_final-ukurs_pur.
                                        wa_final-total_cost   = wa_zco_pgi_rate_1602_final-ukurs_pur.
                                        wa_final-inr_rate     = wa_zco_pgi_rate_1602_final-ukurs_fx.
                                        wa_final-mat_value    = wa_zco_midas-b_fkimg * wa_zco_pgi_rate_1602_final-ukurs_pur.
                                        wa_final-cost_curr    = wa_zco_pgi_rate_1602_final-waers.
                                        IF wa_final-losgr IS NOT INITIAL.
                                          wa_final-mat_value = wa_final-mat_value / wa_final-losgr.
                                        ENDIF.
                                        "16.2 logic end
                                        wa_final-cost_plant = 'TRD'. "Added by Gajanan - 17.10.2024 - CR#8000003569
                                      ELSE.
                                        "16.3 begin
                                        READ TABLE ta_zco_pgi_rate_1603_final INTO DATA(wa_zco_pgi_rate_1603)
                                               WITH KEY matnr   = lv_zco_midas_matnr1. "wa_zco_midas-matnr.

                                        IF sy-subrc IS INITIAL.
                                          wa_final-remark       = text-163."16.3 Material + Multiple Company code - PGI Rate for Trading - Latest period till 3 years
                                          wa_final-losgr        = '1000'.
                                          wa_final-mat_cost     = wa_zco_pgi_rate_1603-ukurs_pur.
                                          wa_final-total_cost   = wa_zco_pgi_rate_1603-ukurs_pur.
                                          wa_final-inr_rate     = wa_zco_pgi_rate_1603-ukurs_fx.
                                          wa_final-mat_value    = wa_zco_midas-b_fkimg * wa_zco_pgi_rate_1603-ukurs_pur.
                                          wa_final-cost_curr    = wa_zco_pgi_rate_1603-waers.
                                          IF wa_final-losgr IS NOT INITIAL.
                                            wa_final-mat_value = wa_final-mat_value / wa_final-losgr.
                                          ENDIF.
                                        ENDIF.
                                        "16.3 end
                                        wa_final-cost_plant = 'TRD'. "Added by Gajanan - 17.10.2024 - CR#8000003569
                                      ENDIF.
                                    ENDIF.
                                    "EOC by Pooja on 02.01.2023 CR#8000007907
                                  ENDIF.
                                ENDIF.
***                                ENDIF. """ LOC commented by Sushil as on 17.06.2021 against ticket no. 8000007120
                              ENDIF.
                            ENDIF.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDIF. """ LOC added by Sushil as on 02.06.2021 against ticket no. 8000007120
                  ENDIF. """ LOC added by Sushil as on 02.06.2021 against ticket no. 8000007120
                ENDIF.
              ENDIF.
***                  ENDIF. """ LOC commented by Sushil as on 02.06.2021 against ticket no. 8000007120
            ENDIF.
***SOC by Gajanan on 21.10.2020 for CR#8000006372***
***              ENDIF." """ LOC commented by Sushil as on 02.06.2021 against ticket no. 8000007120
***            ENDIF."  """ LOC commented by Sushil as on 02.06.2021 against ticket no. 8000007120
          ENDIF."
        ENDIF."
      ENDIF."
***EOC by Gajanan on 21.10.2020 for CR#8000006372***
    ENDIF.
    APPEND wa_final TO ta_final.
    CLEAR : wa_final,wa_zco_midas.

    CLEAR: wa_keko_05,
           wa_keko_502,
           wa_keko_503.
*           wa_keko_504,
*           wa_keko_505.
  ENDLOOP.

  DATA(ta_fina_t) = ta_final.
  DELETE ta_fina_t WHERE kalnr IS INITIAL AND kadky IS INITIAL.
  IF ta_fina_t IS NOT INITIAL.
    SELECT kalnr
           kalka
           kadky
           tvers
           bwvar
           keart
           kst001
           kst002
           kst003
           kst004
           kst005
           kst006
           kst007
           kst008
           kst009
           kst010
           kst011
           kst012
           kst013
           kst014
           kst015
           kst016
           kst017
           kst018
           kst019
           kst020
           kst021
           kst022
           kst023
           kst024
           kst025
           kst026
           kst027
           kst028
           kst029
           kst030
           kst031
           kst032
           kst033
           kst034
           kst035
           kst036
           kst037
           kst038
           kst039
           kst040
    FROM keph
      INTO TABLE ta_keph
      FOR ALL ENTRIES IN ta_fina_t
      WHERE kalnr = ta_fina_t-kalnr
      AND kalka = ta_fina_t-kalka
      AND kadky = ta_fina_t-kadky
      AND tvers = ta_fina_t-tvers
      AND bwvar = ta_fina_t-bwvar
      AND losfx = ''
      AND kkzst = ''
      AND kkzmm = ''
      AND kkzma = ''.
  ENDIF.

  LOOP AT ta_final INTO wa_final.
    "SOC by Pooja on 08.09.2022 CR#8000007907
    IF wa_final-cost_plant+0(1) = '3' OR wa_final-werks = 'TRD'.

      wa_pgi_rate-matnr = wa_final-matnr.
      wa_pgi_rate-rbukrs = wa_final-vkorg.
      APPEND wa_pgi_rate TO ta_pgi_rate.
      CLEAR:wa_pgi_rate.
    ENDIF.
    "EOC by Pooja on 08.09.2022 CR#8000007907
    IF wa_final-kadky IS INITIAL AND wa_final-werks NE co_trd AND wa_final-remark NE text-014 AND wa_final-remark NE text-015
      AND wa_final-remark NE text-025 AND wa_final-remark NE text-026."Added by parashar CRDK965579 8000005475 13.11.2019
      CONTINUE.
    ENDIF.
    READ TABLE ta_t001 INTO wa_t001 WITH KEY bukrs = wa_final-bukrs.
    IF sy-subrc EQ 0.
      READ TABLE ta_curr INTO wa_curr WITH KEY fcurr = wa_t001-waers.
      IF sy-subrc EQ 0.
        gv_ukurs1 = wa_curr-ukurs.
      ENDIF.
    ELSE.
      READ TABLE ta_curr INTO wa_curr WITH KEY fcurr = wa_final-waers.
      IF sy-subrc EQ 0.
        gv_ukurs1 = wa_curr-ukurs.
      ENDIF.
    ENDIF.

    IF wa_final-remark NE text-014 AND wa_final-remark NE text-015
      AND wa_final-remark NE text-025 AND wa_final-remark NE text-026. "Added by parashar CRDK965579 8000005475 13.11.2019

      LOOP AT ta_cost INTO wa_cost.
        READ TABLE ta_keph INTO wa_keph WITH KEY kalnr = wa_final-kalnr kalka = wa_final-kalka kadky = wa_final-kadky
                                             tvers = wa_final-tvers bwvar = wa_final-bwvar keart = wa_cost-ind .
        IF sy-subrc EQ 0.
          IF wa_cost-component = text-016."'MATCO'.
            ASSIGN COMPONENT wa_cost-formula OF STRUCTURE wa_keph TO <fs_value>.
            IF sy-subrc EQ 0.
              IF wa_cost-sign = lc_sign."'+'.
                wa_final-mat_cost = wa_final-mat_cost + <fs_value>.
              ELSE.
                wa_final-mat_cost = wa_final-mat_cost - <fs_value>.
              ENDIF.
            ENDIF.
          ENDIF.
          IF wa_cost-component = text-017."'APIVC'.
            ASSIGN COMPONENT wa_cost-formula OF STRUCTURE wa_keph TO <fs_value>.
            IF sy-subrc EQ 0.
              IF wa_cost-sign = lc_sign."'+'.
                wa_final-api_vc = wa_final-api_vc + <fs_value>.
              ELSE.
                wa_final-api_vc = wa_final-api_vc - <fs_value>.
              ENDIF.
            ENDIF.
          ENDIF.
          IF wa_cost-component = text-018."'APIFC'.
            ASSIGN COMPONENT wa_cost-formula OF STRUCTURE wa_keph TO <fs_value>.
            IF sy-subrc EQ 0.
              IF wa_cost-sign = lc_sign."'+'.
                wa_final-api_fc = wa_final-api_fc + <fs_value>.
              ELSE.
                wa_final-api_fc = wa_final-api_fc - <fs_value>.
              ENDIF.
            ENDIF.
          ENDIF.
          IF wa_cost-component = text-019."'FORMUVC'.
            ASSIGN COMPONENT wa_cost-formula OF STRUCTURE wa_keph TO <fs_value>.
            IF sy-subrc EQ 0.
              IF wa_cost-sign = lc_sign."'+'.
                wa_final-form_vc = wa_final-form_vc + <fs_value>.
              ELSE.
                wa_final-form_vc = wa_final-form_vc - <fs_value>.
              ENDIF.
            ENDIF.
          ENDIF.
          IF wa_cost-component = text-020."'FORMUFC'.
            ASSIGN COMPONENT wa_cost-formula OF STRUCTURE wa_keph TO <fs_value>.
            IF sy-subrc EQ 0.
              IF wa_cost-sign = lc_sign."'+'.
                wa_final-form_fc = wa_final-form_fc + <fs_value>.
              ELSE.
                wa_final-form_fc = wa_final-form_fc - <fs_value>.
              ENDIF.
            ENDIF.
          ENDIF.
          IF wa_cost-component = text-021."'FORMFCY'.
            ASSIGN COMPONENT wa_cost-formula OF STRUCTURE wa_keph TO <fs_value>.
            IF sy-subrc EQ 0.
              IF wa_cost-sign = lc_sign."'+'.
                wa_final-form_fc_y = wa_final-form_fc_y + <fs_value>.
              ELSE.
                wa_final-form_fc_y = wa_final-form_fc_y - <fs_value>.
              ENDIF.
            ENDIF.
          ENDIF.
          IF wa_cost-component = text-022."'SUBMR'.
            ASSIGN COMPONENT wa_cost-formula OF STRUCTURE wa_keph TO <fs_value>.
            IF sy-subrc EQ 0.
              IF wa_cost-sign = lc_sign."'+'.
                wa_final-sub_margin = wa_final-sub_margin + <fs_value>.
              ELSE.
                wa_final-sub_margin = wa_final-sub_margin - <fs_value>.
              ENDIF.
            ENDIF.
          ENDIF.
          IF wa_cost-component = text-023."'EDEP'.
            ASSIGN COMPONENT wa_cost-formula OF STRUCTURE wa_keph TO <fs_value>.
            IF sy-subrc EQ 0.
              IF wa_cost-sign = lc_sign."'+'.
                wa_final-edep = wa_final-edep + <fs_value>.
              ELSE.
                wa_final-edep = wa_final-edep - <fs_value>.
              ENDIF.
            ENDIF.
          ENDIF.
          IF wa_cost-component = text-024."'ED&S'.
            ASSIGN COMPONENT wa_cost-formula OF STRUCTURE wa_keph TO <fs_value>.
            IF sy-subrc EQ 0.
              IF wa_cost-sign = lc_sign."'+'.
                wa_final-edns = wa_final-edns + <fs_value>.
              ELSE.
                wa_final-edns = wa_final-edns - <fs_value>.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        CLEAR :wa_cost.
      ENDLOOP.
    ENDIF.

    IF  wa_t001-waers   = co_waers_usd    OR
        wa_t001-waers   = co_waers_zar    OR
        wa_t001-waers   = co_waers_mad    OR
        wa_t001-waers   = co_waers_aud    OR
        wa_final-waers   = co_waers_usd   OR
        wa_final-waers   = co_waers_zar   OR
        wa_final-waers   = co_waers_mad   OR
        wa_final-waers   = co_waers_aud.

      wa_final-mat_cost = wa_final-mat_cost * gv_ukurs1.
      wa_final-api_vc = wa_final-api_vc * gv_ukurs1.
      wa_final-api_fc = wa_final-api_fc * gv_ukurs1.
      wa_final-form_vc = wa_final-form_vc * gv_ukurs1.
      wa_final-form_fc = wa_final-form_fc * gv_ukurs1.
      wa_final-form_fc_y = wa_final-form_fc_y * gv_ukurs1.
      wa_final-sub_margin = wa_final-sub_margin * gv_ukurs1.
      wa_final-edep = wa_final-edep * gv_ukurs1.
      wa_final-edns = wa_final-edns * gv_ukurs1.
      IF wa_t001-waers IS NOT INITIAL.
        wa_final-cost_curr = wa_t001-waers.
      ELSE.
        wa_final-cost_curr = wa_final-waers.
      ENDIF.

      wa_final-inr_rate = gv_ukurs1.
    ELSEIF wa_t001-waers = co_waers_ugx OR  wa_final-waers = co_waers_ugx.
      wa_final-mat_cost = ( wa_final-mat_cost * gv_ukurs1 ) * 100.
      wa_final-api_vc = ( wa_final-api_vc * gv_ukurs1 ) * 100 .
      wa_final-api_fc = ( wa_final-api_fc * gv_ukurs1 ) * 100.
      wa_final-form_vc = ( wa_final-form_vc * gv_ukurs1 ) * 100.
      wa_final-form_fc = ( wa_final-form_fc * gv_ukurs1 ) * 100.
      wa_final-form_fc_y = ( wa_final-form_fc_y * gv_ukurs1 ) * 100.
      wa_final-sub_margin = ( wa_final-sub_margin * gv_ukurs1 ) * 100.
      wa_final-edep = ( wa_final-edep * gv_ukurs1 ) * 100.
      wa_final-edns = ( wa_final-edns * gv_ukurs1 ) * 100.

      IF wa_t001-waers IS NOT INITIAL.
        wa_final-cost_curr = wa_t001-waers.
      ELSE.
        wa_final-cost_curr = wa_final-waers.
      ENDIF.
      wa_final-inr_rate = gv_ukurs1.
    ELSEIF wa_t001-waers =  co_waers_inr OR  wa_final-waers = co_waers_inr.
*      wa_final-cost_curr = co_waers_inr.
      IF wa_t001-waers IS NOT INITIAL.
        wa_final-cost_curr = wa_t001-waers.
      ELSE.
        wa_final-cost_curr = wa_final-waers.
      ENDIF.
      wa_final-inr_rate = lc_rate.                          "'1.00000'.
    ENDIF.
    IF  wa_final-losgr IS NOT INITIAL.
      wa_final-mat_value = ( wa_final-b_fkimg * wa_final-mat_cost ) / wa_final-losgr.
    ELSE.
      wa_final-mat_value = ( wa_final-b_fkimg * wa_final-mat_cost ).
    ENDIF.
    wa_final-total = wa_final-mat_cost + wa_final-api_vc + wa_final-api_fc + wa_final-form_vc + wa_final-form_fc + wa_final-form_fc_y + wa_final-sub_margin
    + wa_final-edep + wa_final-edns.
*** SOC parashar 8000005475 CRDK969082 07.04.2020
    IF wa_final-vkorg = co_vkorg.
      IF wa_final-werks NE '' AND wa_final-werks NE co_trd AND wa_final-werks+0(1) NE '1'.
        IF  wa_final-losgr IS NOT INITIAL.
          wa_final-stand_value = ( wa_final-b_fkimg * wa_final-total ) / wa_final-losgr.
        ENDIF.
      ELSE.
        wa_final-stand_value = wa_final-mat_value.
      ENDIF.
    ENDIF.

*** EOC parashar 8000005475 CRDK969082 07.04.2020
    MODIFY ta_final FROM wa_final TRANSPORTING mat_cost api_vc api_fc form_vc form_fc form_fc_y sub_margin total cost_curr inr_rate edep edns mat_value  stand_value.
    CLEAR : wa_final,wa_t001,gv_ukurs1. "Added 'gv_ukurs1' by parashar 8000005475 CRDK964595
  ENDLOOP.

*SOA by Pooja - 08.09.2022 - CR#8000007907

  SORT ta_pgi_rate BY matnr rbukrs.
  DELETE ADJACENT DUPLICATES FROM ta_pgi_rate COMPARING matnr rbukrs.

  IF ta_pgi_rate IS NOT INITIAL.
    SELECT fp_gjahr,
           rbukrs,
           awkey,
           vbeln_im,
           vbelp_im,
           mblnr,
           matnr,
           werks,
           charg,
           ukurs_all FROM zco_pgi_rate
      INTO TABLE @DATA(ta_pgi_rate_ukursall)
      FOR ALL ENTRIES IN @ta_pgi_rate
      WHERE fp_gjahr = @lv_spmonc AND
       matnr = @ta_pgi_rate-matnr AND
      rbukrs = @ta_pgi_rate-rbukrs.

    IF ta_pgi_rate_ukursall IS NOT INITIAL.
      SORT ta_pgi_rate_ukursall BY fp_gjahr rbukrs ukurs_all.
      DELETE ADJACENT DUPLICATES FROM ta_pgi_rate_ukursall COMPARING fp_gjahr rbukrs ukurs_all.
    ENDIF.
    SORT ta_pgi_rate_ukursall BY matnr rbukrs.
  ENDIF.
*EOA by Pooja - 08.09.2022 - CR#8000007907

  IF ta_final IS NOT INITIAL.
    "SOA by Pooja on 11.11.2022 CR#8000007907
    lv_startdate_2 = lv_startdate_1.
    lv_enddate_2 = lv_enddate_1.
    SELECT bukrs,waers FROM t001
      INTO TABLE @DATA(ta_t0011)
      FOR ALL ENTRIES IN @ta_final
      WHERE bukrs = @ta_final-vkorg.

    IF ta_t0011 IS NOT INITIAL.
      SORT ta_t0011 BY bukrs.
      SELECT kurst,
             fcurr,
             tcurr,
             gdatu,
             ukurs,
             ffact FROM tcurr INTO
        TABLE @DATA(ta_tcurr_ukurs)
        FOR ALL ENTRIES IN @ta_t0011
        WHERE  kurst = 'M'
            AND fcurr = @ta_t0011-waers
            AND tcurr = 'INR'
            AND gdatu GE @lv_enddate_2 AND gdatu LE @lv_startdate_2.
    ENDIF.

    SORT ta_tcurr_ukurs BY fcurr.
    DATA(lv_lines) = lines( ta_tcurr_ukurs ).

    CLEAR:lv_count.

    LOOP AT ta_tcurr_ukurs INTO DATA(wa_tcurr_ukurs).
      DATA(lv_index) = sy-tabix.
      lv_count = lv_count + 1.
      MOVE-CORRESPONDING wa_tcurr_ukurs TO wa_tcurr_ukurs1.
      COLLECT wa_tcurr_ukurs1 INTO ta_tcurr_ukurs1.

      IF lv_index NE 1 AND wa_tcurr_ukurs-fcurr NE lv_fcurr1.
        CLEAR:lv_excrate.
        READ TABLE ta_tcurr_ukurs1 INTO DATA(wa_tcurr_ukurs2) WITH KEY fcurr = lv_fcurr1.
        IF sy-subrc = 0.
          IF lv_count GT 1.
            DATA(lv_count2) = lv_count - 1.
          ELSE.
            lv_count2 = lv_count.
          ENDIF.
          IF lv_count2 IS NOT INITIAL.
            IF lv_ffact IS NOT INITIAL AND lv_ffact NE '1'.
              lv_excrate =  wa_tcurr_ukurs2-ukurs / lv_count2 / lv_ffact .
            ELSE.
              lv_excrate =  wa_tcurr_ukurs2-ukurs / lv_count2.
            ENDIF.
          ELSE.
            IF lv_ffact IS NOT INITIAL AND lv_ffact NE '1'.
              lv_excrate = wa_tcurr_ukurs2-ukurs /  lv_ffact .
            ELSE.
              lv_excrate = wa_tcurr_ukurs2-ukurs.
            ENDIF.
          ENDIF.
          wa_excrate-fcurr = wa_tcurr_ukurs2-fcurr.
          wa_excrate-exch_rate = lv_excrate.
          APPEND wa_excrate TO ta_excrate.
          CLEAR:lv_count,wa_excrate,wa_tcurr_ukurs2.
        ENDIF.
        lv_count = 1.
      ENDIF.

      IF lv_index = lv_lines.
        READ TABLE ta_tcurr_ukurs1 INTO wa_tcurr_ukurs2 WITH KEY fcurr = wa_tcurr_ukurs1-fcurr.
        IF sy-subrc = 0.
          IF lv_count IS NOT INITIAL.
            IF lv_ffact IS NOT INITIAL AND lv_ffact NE '1'.
              lv_excrate =  wa_tcurr_ukurs2-ukurs / lv_count / lv_ffact .
            ELSE.
              lv_excrate =  wa_tcurr_ukurs2-ukurs / lv_count.
            ENDIF.
          ELSE.
            IF lv_ffact IS NOT INITIAL AND lv_ffact NE '1'.
              lv_excrate = wa_tcurr_ukurs2-ukurs /  lv_ffact.
            ELSE.
              lv_excrate = wa_tcurr_ukurs2-ukurs.
            ENDIF.
          ENDIF.
          wa_excrate-fcurr = wa_tcurr_ukurs2-fcurr.
          wa_excrate-exch_rate = lv_excrate.
          APPEND wa_excrate TO ta_excrate.
          CLEAR:wa_excrate,wa_tcurr_ukurs2.
        ENDIF.
      ENDIF.

      lv_fcurr1 = wa_tcurr_ukurs-fcurr .
      lv_ffact = wa_tcurr_ukurs-ffact .
      CLEAR:wa_tcurr_ukurs1,wa_tcurr_ukurs.
    ENDLOOP.

    SORT ta_tcurr_ukurs1 BY fcurr.
    SORT ta_excrate BY fcurr.
    "EOA by Pooja on 11.11.2022 CR#8000007907

    LOOP AT ta_final INTO wa_final.
      MOVE-CORRESPONDING wa_final TO wa_midas.
      wa_midas-formulation_vc = wa_final-form_vc.
      wa_midas-formulation_fc = wa_final-form_fc.
      wa_midas-form_fc = wa_final-form_fc_y.
      wa_midas-total_cost = wa_final-total.
*** Added by parashar 18.12.2019 8000005475  CRDK966488
      IF wa_midas-remark = text-005.
        IF wa_midas-plaag_01 IS INITIAL.
          wa_midas-remark = text-001.
        ENDIF.
      ENDIF.
*** Added upto by parashar 18.12.2019 8000005475  CRDK966488

*SOC by Gajanan on 30.07.2020 for CR#8000006372*
      IF wa_midas-ent_tag = lc_rectify AND wa_midas-matnr IS INITIAL AND
         wa_midas-plawe IS INITIAL AND wa_midas-plaag_01 IS INITIAL.

        wa_midas-remark = text-027.

      ENDIF.
*EOC by Gajanan on 30.07.2020 for CR#8000006372*
*SOA by Gajanan - 08.04.2022 - CR#8000007907
*      IF wa_midas-vkorg IS INITIAL.
*        READ TABLE ta_exch_tab INTO wa_exch_tab WITH KEY waers = co_waers_usd
*                                                BINARY SEARCH.
*        IF sy-subrc = 0.
*          wa_midas-mat_cost_cc_curr = wa_midas-mat_cost / wa_exch_tab-ukurs.
*          wa_midas-api_vc_cc_curr   = wa_midas-api_vc / wa_exch_tab-ukurs.
*          wa_midas-api_fc_cc_curr   = wa_midas-api_fc / wa_exch_tab-ukurs.
*          wa_midas-for_vc_cc_curr   = wa_midas-formulation_vc / wa_exch_tab-ukurs.
*          wa_midas-for_fc_cc_curr   = wa_midas-formulation_fc / wa_exch_tab-ukurs.
*          CLEAR: wa_exch_tab.
*        ENDIF.
*
*      ELSE.

      READ TABLE ta_t001_midas INTO wa_t001_midas WITH KEY bukrs = wa_midas-vkorg
                                                  BINARY SEARCH.
      IF sy-subrc = 0.
        IF wa_t001_midas-waers = co_waers_inr.
          wa_midas-mat_cost_cc_curr = wa_midas-mat_cost.
          wa_midas-api_vc_cc_curr   = wa_midas-api_vc.
          wa_midas-api_fc_cc_curr   = wa_midas-api_fc.
          wa_midas-for_vc_cc_curr   = wa_midas-formulation_vc.
          wa_midas-for_fc_cc_curr   = wa_midas-formulation_fc.

        ELSE.
          READ TABLE ta_exch_tab INTO wa_exch_tab WITH KEY waers = wa_t001_midas-waers
                                                  BINARY SEARCH.
          IF sy-subrc = 0.
            wa_midas-mat_cost_cc_curr = wa_midas-mat_cost / wa_exch_tab-ukurs.
            wa_midas-api_vc_cc_curr   = wa_midas-api_vc / wa_exch_tab-ukurs.
            wa_midas-api_fc_cc_curr   = wa_midas-api_fc / wa_exch_tab-ukurs.
            wa_midas-for_vc_cc_curr   = wa_midas-formulation_vc / wa_exch_tab-ukurs.
            wa_midas-for_fc_cc_curr   = wa_midas-formulation_fc / wa_exch_tab-ukurs.
            CLEAR: wa_exch_tab.
          ENDIF.

        ENDIF.
        CLEAR: wa_t001_midas.

      ELSE.

        READ TABLE ta_exch_tab INTO wa_exch_tab WITH KEY waers = co_waers_usd
                                                BINARY SEARCH.
        IF sy-subrc = 0.
          wa_midas-mat_cost_cc_curr = wa_midas-mat_cost / wa_exch_tab-ukurs.
          wa_midas-api_vc_cc_curr   = wa_midas-api_vc / wa_exch_tab-ukurs.
          wa_midas-api_fc_cc_curr   = wa_midas-api_fc / wa_exch_tab-ukurs.
          wa_midas-for_vc_cc_curr   = wa_midas-formulation_vc / wa_exch_tab-ukurs.
          wa_midas-for_fc_cc_curr   = wa_midas-formulation_fc / wa_exch_tab-ukurs.
          CLEAR: wa_exch_tab.
        ENDIF.

      ENDIF.
*      ENDIF.
*EOA by Gajanan - 08.04.2022 - CR#8000007907
**SOA by Pooja - 08.09.2022 - CR#8000007907
      READ TABLE ta_pgi_rate_ukursall INTO DATA(wa_pgi_rate_ukursall) WITH KEY matnr = wa_final-matnr
                                                                               rbukrs = wa_final-vkorg .
      IF sy-subrc = 0.
        wa_midas-ukurs_pgi = wa_pgi_rate_ukursall-ukurs_all.
        IF  wa_midas-b_fkimg IS NOT INITIAL AND wa_pgi_rate_ukursall-ukurs_all IS NOT INITIAL.
          wa_midas-ukurs_all = wa_pgi_rate_ukursall-ukurs_all * wa_midas-b_fkimg / 1000.
        ENDIF.
      ELSE.
        wa_midas-ukurs_all  = wa_midas-mat_value.
      ENDIF.
**EOA by Pooja - 08.09.2022 - CR#8000007907
***SOA by Pooja - 10.11.2022 - CR#8000007907
      READ TABLE ta_t0011 INTO DATA(wa_t0011) WITH KEY bukrs = wa_midas-vkorg.
      IF sy-subrc = 0.
        wa_midas-waers = wa_t0011-waers.
      ENDIF.

      READ TABLE ta_excrate INTO wa_excrate WITH KEY fcurr = wa_midas-waers.
      IF sy-subrc = 0.
        wa_midas-ukurs = wa_excrate-exch_rate.
      ENDIF.
**EOA by Pooja - 10.11.2022 - CR#8000007907

***SOA by Gajanan - CR#800003422 - 05.03.2024
      IF wa_midas-vkorg = co_vkorg.
        IF wa_midas-werks = co_trd.
          IF  wa_midas-losgr IS NOT INITIAL.
            wa_midas-stand_value = ( wa_midas-b_fkimg * wa_midas-ukurs_pgi ) / wa_midas-losgr.
          ELSE.
            CLEAR: wa_midas-stand_value.
          ENDIF.
        ENDIF.
      ENDIF.
***EOA by Gajanan - CR#800003422 - 05.03.2024

***SOA by Gajanan - 28.10.2024 - CR#8000003569***
      IF wa_midas-remark IS INITIAL.
        CLEAR: wa_midas-cost_plant.

        IF wa_midas-b_fkimg IS INITIAL.
          wa_midas-remark = text-005.
        ENDIF.
      ENDIF.
***EOA by Gajanan - 28.10.2024 - CR#8000003569***
      APPEND wa_midas TO ta_midas.
      CLEAR :wa_midas,wa_final.
    ENDLOOP.

* Start of Change Inc 8000009287 | TR CRDK988943 | Prathamesh Parab | 12.12.2023
* Code Reversal for CR 8000009287/8000003020
** Start of Change Inc 8000009287 | TR CRDK988689 | Prathamesh Parab | 07.11.2023
*    SELECT *
*      FROM zco_midas
*      INTO TABLE @DATA(ta_midas_temp)
*      FOR ALL ENTRIES IN @ta_midas
*     WHERE spmon    = @ta_midas-spmon
*       AND ent_tag  = @ta_midas-ent_tag
*       AND vkorg    = @ta_midas-vkorg
*       AND vtweg    = @ta_midas-vtweg
*       AND matnr    = @ta_midas-matnr
*       AND locl_prd = @ta_midas-locl_prd
*       AND werks    = @ta_midas-werks
*       AND kdgrp    = @ta_midas-kdgrp
*       AND sub_zone = @ta_midas-sub_zone
*       AND katr4    = @ta_midas-katr4
*       AND zone_d   = @ta_midas-zone_d
*       AND plawe    = @ta_midas-plawe
*       AND plaag_01 = @ta_midas-plaag_01
*       AND sakn1    = @ta_midas-sakn1
*       AND spara    = @ta_midas-spara
*       AND fkart    = @ta_midas-fkart.
*
*    IF ta_midas_temp IS NOT INITIAL.
*      LOOP AT ta_midas ASSIGNING FIELD-SYMBOL(<fs_midas>).
*        READ TABLE ta_midas_temp INTO DATA(wa_midas_temp) WITH KEY spmon    = <fs_midas>-spmon
*                                                                   ent_tag  = <fs_midas>-ent_tag
*                                                                   vkorg    = <fs_midas>-vkorg
*                                                                   vtweg    = <fs_midas>-vtweg
*                                                                   matnr    = <fs_midas>-matnr
*                                                                   locl_prd = <fs_midas>-locl_prd
*                                                                   werks    = <fs_midas>-werks
*                                                                   kdgrp    = <fs_midas>-kdgrp
*                                                                   sub_zone = <fs_midas>-sub_zone
*                                                                   katr4    = <fs_midas>-katr4
*                                                                   zone_d   = <fs_midas>-zone_d
*                                                                   plawe    = <fs_midas>-plawe
*                                                                   plaag_01 = <fs_midas>-plaag_01
*                                                                   sakn1    = <fs_midas>-sakn1
*                                                                   spara    = <fs_midas>-spara
*                                                                   fkart    = <fs_midas>-fkart.
*        IF sy-subrc = 0.
*          <fs_midas>-excl_bonus = wa_midas_temp-excl_bonus.
*          <fs_midas>-bonus_qty = wa_midas_temp-bonus_qty.
*          <fs_midas>-bonus_ret_qty = wa_midas_temp-bonus_ret_qty.
*          <fs_midas>-ip_code = wa_midas_temp-ip_code.
*          CLEAR: wa_midas_temp.
*        ENDIF.
*      ENDLOOP.
*      UNASSIGN: <fs_midas>.
*    ENDIF.
** End of Change Inc 8000009287 | TR CRDK988689 | Prathamesh Parab | 07.11.2023
* End of Change Inc 8000009287 | TR CRDK988943 | Prathamesh Parab | 12.12.2023
    MODIFY zco_midas FROM TABLE ta_midas.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_month .
  CONSTANTS: lc_01(2) TYPE c VALUE '01',
             lc_02(2) TYPE c VALUE '02',
             lc_03(2) TYPE c VALUE '03',
             lc_04(2) TYPE c VALUE '04',
             lc_05(2) TYPE c VALUE '05',
             lc_06(2) TYPE c VALUE '06',
             lc_07(2) TYPE c VALUE '07',
             lc_08(2) TYPE c VALUE '08',
             lc_09(2) TYPE c VALUE '09',
             lc_10(2) TYPE c VALUE '10',
             lc_11(2) TYPE c VALUE '11',
             lc_12(2) TYPE c VALUE '12'.

  r_spmon1-sign = co_sign."'I'.
  r_spmon1-option = co_option."'BT'.

  CASE p_spmon+4(2).
    WHEN lc_01."'01'.
      gv_year = p_spmon(4) - 1.
      gv_mon = lc_08."'08'.
      CONCATENATE gv_year gv_mon INTO r_spmon1-low.
    WHEN lc_02."'02'.
      gv_year = p_spmon(4) - 1.
      gv_mon = lc_09."'09'.
      CONCATENATE gv_year gv_mon INTO r_spmon1-low.
    WHEN lc_03."'03'.
      gv_year = p_spmon(4) - 1.
      gv_mon = lc_10."'10'.
      CONCATENATE gv_year gv_mon INTO r_spmon1-low.
    WHEN lc_04."'04'.
      gv_year = p_spmon(4) - 1.
      gv_mon = lc_11."'11'.
      CONCATENATE gv_year gv_mon INTO r_spmon1-low.
    WHEN lc_05."'05'.
      gv_year = p_spmon(4) - 1.
      gv_mon = lc_12."'12'.
      CONCATENATE gv_year gv_mon INTO r_spmon1-low.
    WHEN lc_06."'06'.
      gv_year = p_spmon(4).
      gv_mon = lc_01."'01'.
      CONCATENATE gv_year gv_mon INTO r_spmon1-low.
    WHEN lc_07."'07'.
      gv_year = p_spmon(4).
      gv_mon = lc_02."'02'.
      CONCATENATE gv_year gv_mon INTO r_spmon1-low.
    WHEN lc_08."'08'.
      gv_year = p_spmon(4).
      gv_mon = lc_03."'03'.
      CONCATENATE gv_year gv_mon INTO r_spmon1-low.
    WHEN lc_09."'09'.
      gv_year = p_spmon(4).
      gv_mon = lc_04."'04'.
      CONCATENATE gv_year gv_mon INTO r_spmon1-low.
    WHEN lc_10."'10'.
      gv_year = p_spmon(4).
      gv_mon = lc_05."'05'.
      CONCATENATE gv_year gv_mon INTO r_spmon1-low.
    WHEN lc_11."'11'.
      gv_year = p_spmon(4).
      gv_mon = lc_06."'06'.
      CONCATENATE gv_year gv_mon INTO r_spmon1-low.
    WHEN lc_12."'12'.
      gv_year = p_spmon(4).
      gv_mon = lc_07."'07'.
      CONCATENATE gv_year gv_mon INTO r_spmon1-low.
  ENDCASE.

  r_spmon1-high = p_spmon.
  APPEND r_spmon1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MONTH1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_month1 .

  CONSTANTS: lc_01(2) TYPE c VALUE '01',
             lc_02(2) TYPE c VALUE '02',
             lc_03(2) TYPE c VALUE '03',
             lc_04(2) TYPE c VALUE '04',
             lc_05(2) TYPE c VALUE '05',
             lc_06(2) TYPE c VALUE '06',
             lc_07(2) TYPE c VALUE '07',
             lc_08(2) TYPE c VALUE '08',
             lc_09(2) TYPE c VALUE '09',
             lc_10(2) TYPE c VALUE '10',
             lc_11(2) TYPE c VALUE '11',
             lc_12(2) TYPE c VALUE '12'.

  r_spmon2-sign = co_sign."'I'.
  r_spmon2-option = co_option."'BT'.
  CLEAR : gv_year,gv_mon.
  CASE p_spmon+4(2).
    WHEN lc_01."'01'.
      gv_year = p_spmon(4) - 1.
      gv_mon = lc_02."'02'.
      CONCATENATE gv_year gv_mon INTO r_spmon2-low.
    WHEN lc_02."'02'.
      gv_year = p_spmon(4) - 1.
      gv_mon = lc_03."'03'.
      CONCATENATE gv_year gv_mon INTO r_spmon2-low.
    WHEN lc_03."'03'.
      gv_year = p_spmon(4) - 1.
      gv_mon = lc_04."'04'.
      CONCATENATE gv_year gv_mon INTO r_spmon2-low.
    WHEN lc_04."'04'.
      gv_year = p_spmon(4) - 1.
      gv_mon = lc_05."'05'.
      CONCATENATE gv_year gv_mon INTO r_spmon2-low.
    WHEN lc_05."'05'.
      gv_year = p_spmon(4) - 1.
      gv_mon = lc_06."'06'.
      CONCATENATE gv_year gv_mon INTO r_spmon2-low.
    WHEN lc_06."'06'.
      gv_year = p_spmon(4) - 1.
      gv_mon = lc_07."'07'.
      CONCATENATE gv_year gv_mon INTO r_spmon2-low.
    WHEN lc_07."'07'.
      gv_year = p_spmon(4) - 1.
      gv_mon = lc_08."'02'.
      CONCATENATE gv_year gv_mon INTO r_spmon2-low.
    WHEN lc_08."'08'.
      gv_year = p_spmon(4) - 1.
      gv_mon = lc_09."'03'.
      CONCATENATE gv_year gv_mon INTO r_spmon2-low.
    WHEN lc_09."'09'.
      gv_year = p_spmon(4) - 1.
      gv_mon = lc_10."'04'.
      CONCATENATE gv_year gv_mon INTO r_spmon2-low.
    WHEN lc_10."'10'.
      gv_year = p_spmon(4) - 1.
      gv_mon = lc_11."'05'.
      CONCATENATE gv_year gv_mon INTO r_spmon2-low.
    WHEN lc_11."'11'.
      gv_year = p_spmon(4) - 1.
      gv_mon = lc_12."'06'.
      CONCATENATE gv_year gv_mon INTO r_spmon2-low.
    WHEN lc_12."'12'.
      gv_year = p_spmon(4).
      gv_mon = lc_01."'07'.
      CONCATENATE gv_year gv_mon INTO r_spmon2-low.
  ENDCASE.

  r_spmon2-high = p_spmon.
  APPEND r_spmon2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PGI_RATE_COCD_COUNT     (new 01.11.2026)
*&---------------------------------------------------------------------*
*  Number of distinct company codes per material in a ZCO_PGI_RATE
*  table (sorted result, read with BINARY SEARCH).
*----------------------------------------------------------------------*
FORM pgi_rate_cocd_count USING    it_rate TYPE tt_pgi_rate
                         CHANGING et_cnt  TYPE tt_matnr_cnt.
  TYPES: BEGIN OF lty_key,
           matnr  TYPE zco_pgi_rate-matnr,
           rbukrs TYPE zco_pgi_rate-rbukrs,
         END OF lty_key.
  DATA: lt_key TYPE STANDARD TABLE OF lty_key WITH DEFAULT KEY,
        ls_cnt TYPE ty_matnr_cnt.

  CLEAR et_cnt.
  lt_key = CORRESPONDING #( it_rate ).
  SORT lt_key BY matnr rbukrs.
  DELETE ADJACENT DUPLICATES FROM lt_key COMPARING matnr rbukrs.
  LOOP AT lt_key ASSIGNING FIELD-SYMBOL(<ls_key>).
    ls_cnt-matnr = <ls_key>-matnr.
    ls_cnt-cnt   = 1.
    COLLECT ls_cnt INTO et_cnt.
  ENDLOOP.
  SORT et_cnt BY matnr.
ENDFORM.
