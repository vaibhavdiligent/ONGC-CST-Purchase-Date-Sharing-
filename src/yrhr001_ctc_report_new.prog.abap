*&---------------------------------------------------------------------*
*& Report  YRHR001_CTC_REPORT                                          *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
***
REPORT  yrhr001_ctc_report_new                                          .

TYPE-POOLS: slis.                           "Generic list types

TYPE-POOLS : imis.

NODES: pernr.

INFOTYPES: 0000,
           0001,
           0014,
           0015,
           0045,
           0078,
           0267,
           9109,
           0590,
           0581,
           2001.

RANGES: w_wagetypes FOR pc207-lgart,
        w_wt_retro FOR pc207-lgart.

DATA:  BEGIN OF i_bdcdata OCCURS 0. "HODLING THE BDC PARAMETERS
         INCLUDE STRUCTURE bdcdata.
DATA:  END OF i_bdcdata.

DATA : w_no(3).
DATA: BEGIN OF messtab OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF messtab.

DATA : BEGIN OF i_error OCCURS 0,
         msg TYPE imis_type_s_message,
       END OF i_error.

DATA: BEGIN OF i_trv,
        yy_sno           LIKE yrhs_ctc_h-yy_sno,
        yy_pernr         LIKE yrhs_ctc_h-yy_pernr,
        yy_fpbeg         LIKE yrhs_ctc_h-yy_fpbeg,
        yy_fpend         LIKE yrhs_ctc_h-yy_fpend,
        yy_bukrs         LIKE yrhs_ctc_h-yy_bukrs,
        yy_gsber         LIKE yrhs_ctc_h-yy_gsber,
        yy_werks         LIKE yrhs_ctc_h-yy_werks,
        yy_btrtl         LIKE yrhs_ctc_h-yy_btrtl,
        yy_name          LIKE yrhs_ctc_h-yy_name,
        yy_grade         LIKE yrhs_ctc_h-yy_grade,
        yy_designation   LIKE yrhs_ctc_h-yy_designation,
        yy_department    LIKE yrhs_ctc_h-yy_department,
        yy_location      LIKE yrhs_ctc_h-yy_location,
        yy_total_trv     LIKE yrhs_ctc_h-yy_total_ctc,
        yy_trip_reason   LIKE yrhs_ctc_travel-yy_trip_reason,
        yy_trip_location LIKE yrhs_ctc_travel-yy_trip_location,
        yy_trip_begin    LIKE yrhs_ctc_travel-yy_trip_begin,
        yy_trip_end      LIKE yrhs_ctc_travel-yy_trip_end,
      END OF i_trv.

DATA: BEGIN OF i_ctc_dtl,
        yy_sno          TYPE c,
        yy_pernr        LIKE yrhs_ctc_h-yy_pernr,
        yy_fpper        LIKE yrhs_ctc_h-yy_fpper,
        yy_fpbeg        LIKE yrhs_ctc_h-yy_fpbeg,
        yy_fpend        LIKE yrhs_ctc_h-yy_fpend,
        yy_start_period LIKE yrhs_ctc_h-yy_start_period,
        yy_end_period   LIKE yrhs_ctc_h-yy_end_period,
        yy_apznr        LIKE yrhs_ctc_h-yy_apznr,
        yy_bukrs        LIKE yrhs_ctc_h-yy_bukrs,
        yy_butxt        LIKE yrhs_ctc_h-yy_butxt,
        yy_gsber        LIKE yrhs_ctc_h-yy_gsber,
        yy_gtext        LIKE yrhs_ctc_h-yy_gtext,
        yy_werks        LIKE yrhs_ctc_h-yy_werks,
        yy_name1        LIKE yrhs_ctc_h-yy_name1,
        yy_btrtl        LIKE yrhs_ctc_h-yy_btrtl,
        yy_persg        LIKE yrhs_ctc_h-yy_persg,
        yy_ptext        LIKE yrhs_ctc_h-yy_ptext,
        yy_name         LIKE yrhs_ctc_h-yy_name,
        yy_grade        LIKE yrhs_ctc_h-yy_grade,
        yy_designation  LIKE yrhs_ctc_h-yy_designation,
        yy_department   LIKE yrhs_ctc_h-yy_department,
        yy_location     LIKE yrhs_ctc_h-yy_location,
        yy_total_ctc    LIKE yrhs_ctc_h-yy_total_ctc,
        yy_py_prd_days  LIKE yrhs_ctc_h-yy_py_prd_days,
        yy_prd_days     LIKE yrhs_ctc_h-yy_prd_days,
        yypayscale_min  LIKE yrhs_ctc_h-yypayscale_min,
        yypayscale_max  LIKE yrhs_ctc_h-yypayscale_max,
        yy_lgart        LIKE yrhs_ctc_d-yy_lgart,
        yy_lgtxt        LIKE yrhs_ctc_d-yy_lgtxt,
        yy_betrg        LIKE yrhs_ctc_d-yy_betrg,
        yy_ben_type     LIKE yrhs_ctc_d-yy_ben_type,
        yy_ben_typ_txt  LIKE yrhs_ctc_d-yy_ben_typ_txt,
*        yy_pd_food      LIKE yrhs_ctc_d-yy_pd_food,
*        yy_total_trv    LIKE yrhs_ctc_h-yy_total_ctc,
      END OF i_ctc_dtl.

DATA: itab_ctc_dtl   LIKE STANDARD TABLE OF i_ctc_dtl WITH HEADER LINE,
      ls_wage_driver TYPE yrha_ctc_driver.

***BEGIN: DECLARATIONS FOR RETERO ACCOUNTING REPORT
DATA: BEGIN OF i_inper_directory_entry,
        iperm LIKE pc261-iperm,
        inper LIKE pc261-inper,
        inpty LIKE pc261-inpty,
        inpid LIKE pc261-inpid,
        inocr LIKE pc261-inocr,
        ipend LIKE pc261-ipend,
        rundt LIKE pc261-rundt,
        runtm LIKE pc261-runtm,
      END OF i_inper_directory_entry.

DATA: BEGIN OF i_inper,
        iabkrs     LIKE pc261-iabkrs,
        iabkrs_txt LIKE t549t-atext,
        iperm      LIKE pc261-iperm,
        iperm_txt  LIKE t549n-atext,
        inper      LIKE pc261-inper,
        ipend      LIKE pc261-ipend,
        inpty      LIKE pc261-inpty,
        inpid      LIKE pc261-inpid,
      END OF i_inper.
DATA: itab_inper LIKE STANDARD TABLE OF i_inper WITH HEADER LINE.

DATA: BEGIN OF i_fpper,
        abkrs     LIKE pc261-abkrs,
        abkrs_txt LIKE t549t-atext,
        permo     LIKE pc261-permo,
        permo_txt LIKE t549n-atext,
        fpper     LIKE pc261-fpper,
        paydt     LIKE pc261-paydt,
        payty     LIKE pc261-payty,
        payid     LIKE pc261-payid,
      END OF i_fpper.
DATA: itab_fpper LIKE STANDARD TABLE OF i_fpper WITH HEADER LINE.

DATA: BEGIN OF i_lgart,
        molga     LIKE t512w-molga,
        lgart     LIKE pc207-lgart,
        lgart_txt LIKE t512t-lgtxt,
      END OF i_lgart.
DATA: itab_lgart LIKE STANDARD TABLE OF i_lgart WITH HEADER LINE.

DATA: BEGIN OF i_bukrs,
        bukrs     LIKE pc205-bukrs,
        bukrs_txt LIKE hrca_company-comp_name,
      END OF i_bukrs.
DATA: itab_bukrs LIKE STANDARD TABLE OF i_bukrs WITH HEADER LINE.

DATA: BEGIN OF i_persa,
        persa     LIKE pc205-werks,
        persa_txt LIKE t500p-name1,
      END OF i_persa.
DATA: itab_persa LIKE STANDARD TABLE OF i_persa WITH HEADER LINE.

DATA: BEGIN OF i_btrtl,
        btrtl_persa LIKE pc205-werks,
        btrtl       LIKE pc205-btrtl,
        btrtl_txt   LIKE t001p-btext,
      END OF i_btrtl.
DATA: itab_btrtl LIKE STANDARD TABLE OF i_btrtl WITH HEADER LINE.

DATA: BEGIN OF i_kostl,
        kostl_kokrs     LIKE hrca_contr-co_area,
        kostl_kokrs_txt LIKE hrca_contr-name,
        kostl           LIKE hrca_costc-costcenter,
        kostl_txt       LIKE hrca_costc-name,
        kdate           TYPE pc261-fpend,
      END OF i_kostl.
DATA: itab_kostl LIKE STANDARD TABLE OF i_kostl WITH HEADER LINE.

DATA: BEGIN OF i_persg,
        persg     LIKE pc205-persg,
        persg_txt LIKE t501t-ptext,
      END OF i_persg.
DATA: itab_persg LIKE STANDARD TABLE OF i_persg WITH HEADER LINE.

DATA: BEGIN OF i_persk,
        persk     LIKE pc205-persk,
        persk_txt LIKE t503t-ptext,
      END OF i_persk.
DATA: itab_persk LIKE STANDARD TABLE OF i_persk WITH HEADER LINE.
***************SOC-Priya tiwari**********************************
"DATA: lt_perc TYPE YRHR_CTC_SC-percentage.
DATA: lt_perc        TYPE TABLE OF yrhr_ctc_sc,
      ls_perc        TYPE yrhr_ctc_sc,
      lv_flag        TYPE char1,
      lv_yy_perc_amt TYPE yrhr_ctc_sc-percentage.
**************EOC-Priya tiwari***********************************

DATA: BEGIN OF i_kokrs,
        kokrs     LIKE hrca_contr-co_area,
        kokrs_txt LIKE hrca_contr-name,
      END OF i_kokrs.

DATA: BEGIN OF i_pa0001 OCCURS 0,
        pernr LIKE pa0001-pernr,
        kostl LIKE pa0001-kostl,
      END OF i_pa0001.

DATA: itab_kokrs LIKE STANDARD TABLE OF i_kokrs WITH HEADER LINE.

DATA: BEGIN OF i_pernr_common,
        pernr LIKE pernr-pernr,
        sname LIKE p0001-sname,
        ename LIKE p0001-ename,
        perid LIKE p0002-perid, "XFE
      END OF i_pernr_common.
DATA: itab_pernr_common LIKE STANDARD TABLE OF i_pernr_common
                        WITH HEADER LINE.
DATA: pfstatus TYPE slis_formname VALUE 'PF_STATUS_SET'.
DATA: BEGIN OF i_datatable,
        pernr     LIKE pernr-pernr,
        ename     LIKE p0001-ename,
        bukrs     LIKE pc205-bukrs,
*    bukrs_txt like hrca_company-comp_name,
        persa     LIKE pc205-werks,
        persk     LIKE pc205-persk,
        btrtl     LIKE pc205-btrtl,
        btrtl_txt LIKE t001p-btext,
*    persa_txt like t500p-name1,
        molga     LIKE pc202-molga,
        lgart     LIKE pc207-lgart,
        lgart_txt LIKE t512t-lgtxt,
        inper     LIKE pc261-inper,
        ipend     LIKE pc261-ipend,
        abkrs     LIKE pc261-abkrs,
*    abkrs_txt like t549t-atext,
        permo     LIKE pc261-permo,
*    permo_txt like t549n-atext,
        fpper     LIKE pc261-fpper,
        paydt     LIKE pc261-paydt,
        payty     LIKE pc261-payty,
        payid     LIKE pc261-payid,
        anzhl     LIKE pc207-anzhl,
        betrg     LIKE pc207-betrg,
        waers     LIKE pc207-amt_curr,
      END OF i_datatable.
DATA: itab_datatable          LIKE STANDARD TABLE OF i_datatable
              WITH HEADER LINE,
      itab_datatable_2        LIKE STANDARD TABLE OF i_datatable
              WITH HEADER LINE,
      itab_datatable_oc       LIKE STANDARD TABLE OF i_datatable
              WITH HEADER LINE,
      itab_datatable_oc_final LIKE STANDARD TABLE OF i_datatable
              WITH HEADER LINE.

DATA: wa_inper_directory_entry LIKE i_inper_directory_entry,
      wa_retro_basic_rate      LIKE i_datatable,
      wa_retro_basic           LIKE i_datatable,
      wa_retro_da              LIKE i_datatable,
      wa_datatable             LIKE i_datatable.

DATA: itab_inper_directory LIKE STANDARD TABLE OF
              i_inper_directory_entry, " WITH HEADER LINE,
      itab_evp_inper       LIKE STANDARD TABLE OF pc261 WITH HEADER LINE,
      itab_evp             LIKE STANDARD TABLE OF pc261 WITH HEADER LINE.

DATA: w_beg                 LIKE sy-datum VALUE '18000101',
      w_end                 LIKE sy-datum,
      w_evp_lines           TYPE i,
      w_import_relid        LIKE t500l-relid,
      w_molga               LIKE t500l-molga,
      w_anzhl               LIKE pc207-anzhl,
      w_betrg_retro         LIKE pc207-betrg,
      w_waers               LIKE pc207-amt_curr,
      w_mth_retro(2)        TYPE c,
      w_month_txt_retro(10) TYPE c.

DATA: wa_variant TYPE disvariant.
CONSTANTS: c_formname_top_of_page TYPE slis_formname
                                  VALUE 'TOP_OF_PAGE'.
***END: DECLARATIONS FOR RETERO ACCOUNTING REPORT

DATA: itab_p0000            LIKE STANDARD TABLE OF p0000 WITH HEADER LINE,
      itab_p0001            LIKE STANDARD TABLE OF p0001 WITH HEADER LINE,
      itab_p0045            LIKE STANDARD TABLE OF p0045 WITH HEADER LINE,
      itab_p0015            LIKE STANDARD TABLE OF p0015 WITH HEADER LINE,
      itab_p0015_claim      LIKE STANDARD TABLE OF p0015 WITH HEADER LINE,
      itab_p0001_claim      LIKE STANDARD TABLE OF p0001 WITH HEADER LINE,
      itab_p0078            LIKE STANDARD TABLE OF p0078 WITH HEADER LINE,
      itab_p0267            LIKE STANDARD TABLE OF p0267 WITH HEADER LINE,
      itab_p0581            LIKE STANDARD TABLE OF p0581 WITH HEADER LINE,
      itab_p0590            LIKE STANDARD TABLE OF p0590 WITH HEADER LINE,
      itab_p2001            LIKE STANDARD TABLE OF p2001 WITH HEADER LINE,
      itab_p9109            LIKE STANDARD TABLE OF p9109 WITH HEADER LINE,
      it_rgdir              TYPE STANDARD TABLE OF pc261,
      itab_rgdir_p          LIKE STANDARD TABLE OF pc261 WITH HEADER LINE,
      itab_rgdir_a          LIKE STANDARD TABLE OF pc261 WITH HEADER LINE,
      itab_final_h          LIKE STANDARD TABLE OF yrhs_ctc_h WITH HEADER LINE,
      itab_final_h2         LIKE STANDARD TABLE OF yrhs_ctc_h WITH HEADER LINE,
      itab_final_d          LIKE STANDARD TABLE OF yrhs_ctc_d WITH HEADER LINE,
      itab_final_d1         LIKE STANDARD TABLE OF yrhs_ctc_d WITH HEADER LINE,
      itab_final_d2         LIKE STANDARD TABLE OF yrhs_ctc_d WITH HEADER LINE,
      itab_t512t            LIKE STANDARD TABLE OF t512t WITH HEADER LINE,
      itab_t001             LIKE STANDARD TABLE OF t001 WITH HEADER LINE,
      itab_tgsbt            LIKE STANDARD TABLE OF tgsbt WITH HEADER LINE,
      itab_t7ina9           LIKE STANDARD TABLE OF t7ina9 WITH HEADER LINE,
      itab_prp_per          LIKE STANDARD TABLE OF yrha_grd_percent WITH HEADER
                   LINE,
      itab_grp_ins          LIKE STANDARD TABLE OF yrha_ctc_grp_ins WITH HEADER
                   LINE,
      itab_loan_result      LIKE STANDARD TABLE OF pclo_repay,
      itab_p0078_1          TYPE pclo_p0078,
      itab_t591s            LIKE STANDARD TABLE OF t591s WITH HEADER LINE,
      itab_ptrv_perio       LIKE STANDARD TABLE OF ptrv_perio WITH HEADER LINE
      ,
      itab_ptrv_shdr        LIKE STANDARD TABLE OF ptrv_shdr WITH HEADER LINE,
      itab_ptrv_srec        LIKE STANDARD TABLE OF v_ptrv_srec WITH HEADER LINE
      ,
      itab_ptrv_head        LIKE STANDARD TABLE OF ptrv_head WITH HEADER LINE,
      itab_ftpt_req_advance LIKE STANDARD TABLE OF ftpt_req_advance
                            WITH HEADER LINE,
      itab_t706b5           LIKE STANDARD TABLE OF t706b5 WITH HEADER LINE,
      itab_travel           LIKE STANDARD TABLE OF yrhs_ctc_travel WITH HEADER
                  LINE,
      itab_trv_smry         LIKE STANDARD TABLE OF i_trv,
      itab_t500p            LIKE STANDARD TABLE OF t500p WITH HEADER LINE,
      itab_t001p            LIKE STANDARD TABLE OF t001p WITH HEADER LINE,
      itab_pay_scale        LIKE STANDARD TABLE OF yrha_pay_scale WITH
                     HEADER LINE,
      itab_pay_rev2017      LIKE STANDARD TABLE OF yrha_pay_rev2017 WITH
                     HEADER LINE,
      itab_t527x            LIKE STANDARD TABLE OF t527x WITH HEADER LINE,
      itab_spect            LIKE STANDARD TABLE OF yrha_spec_reimb
                      WITH HEADER LINE,
      itab_ctc_smry         LIKE STANDARD TABLE OF yrhs_ctc_h WITH HEADER LINE,
      i_exit_event          TYPE slis_t_event_exit WITH HEADER LINE,
      itab_t501t            LIKE STANDARD TABLE OF t501t WITH HEADER LINE.

***SOC BY SANYOGITA ON 05.04.2011 TO GET CLAIM DATA FROM CLAIMS CLUSTER
DATA: wa_mlcnt TYPE hressinml_content.

DATA: itab_mlcnt TYPE TABLE OF pin_ml_content WITH HEADER LINE,
      itab_headr TYPE TABLE OF pin_header_sf WITH HEADER LINE,
      itab_haedc TYPE TABLE OF pin_hlcu_field WITH HEADER LINE.
***EOC BY SANYOGITA ON 05.04.2011 TO GET CLAIM DATA FROM CLAIMS CLUSTER

DATA:
  w_end_pay_period TYPE q0045-abrdt,     "Payroll Period end
  w_country_code   LIKE t500l-molga,     "Country Grouping
  w_relid          LIKE pcl2-relid,      "Area identifier PCLx
  w_int_start_date TYPE q0045-abrdt,     "Payroll Period end
  w_paper          LIKE pn-paper,        "Current Payroll Period
  w_designation    LIKE hrp1000-stext,
  w_department     LIKE yrha_department-yy_department,
  w_btext          LIKE t001p-btext,
  w_werks          LIKE t001p-werks,
  w_btrtl          LIKE t001p-btrtl,
  w_fm_name        TYPE rs38l_fnam,
  w_count          TYPE i,
  w_from_dt(10)    TYPE c,
  w_to_dt(10)      TYPE c,
  w_total_ctc      LIKE pc207-betrg,
  w_assamt         LIKE pa9109-yy_assamt01,
  w_billdt         LIKE pa9109-yy_billdt01,
  w_betrg          LIKE yrhs_ctc_d-yy_betrg,
  w_lines          TYPE i,
  w_furn_maint     LIKE yrhs_ctc_d-yy_betrg,
  w_split_flag     TYPE c.

DATA:
  wa_result       TYPE pay99_result,
  wa_rt           LIKE LINE OF wa_result-inter-rt,
  wa_wpbp         LIKE LINE OF wa_result-inter-wpbp,
  wa_ab           LIKE LINE OF wa_result-inter-ab,      " Added by Tushar 4000004249, TG:YRHS039-CTC REPORT LEAVE CONDITION
  wa_rgdir        LIKE LINE OF it_rgdir,
  wa_t500l        LIKE t500l,
  wa_result_2     LIKE pclo_repay,
  wa_0078         LIKE p0078,
  wa_final_d_9022 LIKE yrhs_ctc_d,
  wa_final_d_9023 LIKE yrhs_ctc_d,
  wa_final_d_9024 LIKE yrhs_ctc_d,
  wa_final_d_9015 LIKE yrhs_ctc_d,
  wa_final_d_4075 LIKE yrhs_ctc_d,
  wa_final_d_9025 LIKE yrhs_ctc_d,
  wa_final_d_9026 LIKE yrhs_ctc_d,
  wa_ctc_smry     LIKE yrhs_ctc_h,
  wa_trv_smry     LIKE i_trv.

DATA: itab_wpbp LIKE STANDARD TABLE OF wa_wpbp
                     WITH HEADER LINE.

DATA: w_basic      LIKE wa_rt-betrg,
      w_da         LIKE wa_rt-betrg,
      w_pp         LIKE wa_rt-betrg,
      w_no_days    TYPE i,
      w_mth_days   TYPE i,
      w_day_salary LIKE wa_rt-betrg,
      w_asign      LIKE pa0001-ename,
      w_asg_dept   LIKE yrha_department-yy_department,
      w_asg_loc    LIKE t001p-btext,
      w_asg_desg   LIKE hrp1000-stext,
      w_basic_rate LIKE wa_rt-betrg,
      w_da_rate    LIKE wa_rt-betrg,
      w_pp_rate    LIKE wa_rt-betrg,
      w_basic_hpl  LIKE wa_rt-betrg,
      w_da_hpl     LIKE wa_rt-betrg,
      w_pp_hpl     LIKE wa_rt-betrg,
      w_amt_a      LIKE wa_rt-betrg,
      w_amt_b      LIKE wa_rt-betrg.

***DECLARATIONS FOR SMARTFORM TO PDF CONVERSION
DATA: v_language     TYPE sflangu VALUE 'E',
      v_e_devtype    TYPE rspoptype,
      v_bin_filesize TYPE i,
      v_name         TYPE string,
      v_path         TYPE string,
      v_fullpath     TYPE string,
      v_filter       TYPE string,
      v_uact         TYPE i,
      v_guiobj       TYPE REF TO cl_gui_frontend_services,
      v_filename     TYPE string.

DATA: st_output_options       TYPE ssfcompop,
      st_control_parameters   TYPE ssfctrlop,
      st_document_output_info TYPE ssfcrespd,
      st_job_output_options   TYPE ssfcresop,
      st_job_output_info      TYPE ssfcrescl.

DATA: it_docs  TYPE STANDARD TABLE OF docs,
      it_lines TYPE STANDARD TABLE OF tline.

***ALV DECLARATIONS
TYPE-POOLS : slis, vrm.

DATA: i_fieldcat    TYPE slis_t_fieldcat_alv,
      i_top_of_page TYPE slis_t_listheader,
      i_layout      TYPE slis_layout_alv,
      fpfstatus     TYPE slis_formname VALUE 'PF_STATUS_SET',
      i_sort        TYPE slis_t_sortinfo_alv,
      i_print       TYPE slis_print_alv,
      i_events      TYPE slis_t_event,
      w_repid       LIKE sy-repid,

      w_f2code      LIKE sy-ucomm VALUE '&ETA'.
***  SOC by Tushar Goyal & Bibhu Ranjan Sahoo on 19/08/2021 2000000337, TG:Changes in CTC Report - YRHS039
DATA: l_from_beg    TYPE datum,
      l_from_end    TYPE datum,
      l_to_beg      TYPE datum,
      l_to_end      TYPE datum,
      l_fin_year(9),
      l_year2(4).

TYPES: BEGIN OF ty_period,
         month(2),
         year(4),
         begda    TYPE datum,
         endda    TYPE datum,
       END OF ty_period.

DATA: i_period  TYPE TABLE OF ty_period,
      wa_period TYPE ty_period.

DATA: i_header  TYPE TABLE OF yrhs_ctc_h,
      i_data    TYPE TABLE OF yrhs_ctc_d,
      wa_data   TYPE yrhs_ctc_d,
      wa_header TYPE yrhs_ctc_h.
***  EOC by Tushar Goyal & Bibhu Ranjan Sahoo on 19/08/2021 2000000337, TG:Changes in CTC Report - YRHS039

*  SELECTION SCREEN--------------------------------------------------*
***  SOC by Tushar Goyal & Bibhu Ranjan Sahoo on 19/08/2021 2000000337, TG:Changes in CTC Report - YRHS039
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-aaa.

  PARAMETERS: p_from TYPE char2,
              p_to   TYPE char2,
              p_year TYPE char4.

SELECTION-SCREEN END OF BLOCK b1.
***  EOC by Tushar Goyal & Bibhu Ranjan Sahoo on 19/08/2021 2000000337, TG:Changes in CTC Report - YRHS039
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_asign TYPE pa0001-pernr,   " OBLIGATORY.
              p_tran  TYPE pc207-betrg,
              p_elec  TYPE pc207-betrg,
              p_scf   TYPE pc207-betrg,
              p_med   TYPE pc207-betrg,
              p_drvwg TYPE pc207-betrg,
              p_qtf   TYPE pc207-betrg,
              p_ins   TYPE pc207-betrg,
              p_ins2  TYPE pc207-betrg,
              p_fin   TYPE pc207-betrg,
              p_edli  TYPE pc207-betrg,
              p_arr   TYPE pc207-betrg.

* SOC By RAVI/BIBHU ON 20.04.2022 CHARM:4000004733, YRHR001_CTC_REPORT_NEW- CTC
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(31) TEXT-056.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS : p_mis TYPE char30.
    SELECTION-SCREEN POSITION POS_LOW.
    PARAMETERS:  p_mis1 TYPE pc207-betrg.
  SELECTION-SCREEN END OF LINE.
* EOC By RAVI/BIBHU ON 20.04.2022 CHARM:4000004733, YRHR001_CTC_REPORT_NEW- CTC

SELECTION-SCREEN END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_rpt RADIOBUTTON GROUP gr1 DEFAULT 'X' USER-COMMAND adv,
              p_pdf RADIOBUTTON GROUP gr1,
              p_dtl RADIOBUTTON GROUP gr1,
              p_sry RADIOBUTTON GROUP gr1,
              p_trv RADIOBUTTON GROUP gr1,
*             P_RTR RADIOBUTTON GROUP GR1,
              p_rts RADIOBUTTON GROUP gr1,
              p_dbt RADIOBUTTON GROUP gr1.
*             P_RPD RADIOBUTTON GROUP GR1.
  SELECTION-SCREEN SKIP.
  PARAMETERS: p_vari   TYPE disvariant-variant,
              p_glcode TYPE rf05a-newko,
              p_budat  TYPE bkpf-budat,
              p_bukrs  TYPE bkpf-bukrs,
              p_gsber  TYPE cobl-gsber,
              p_mode   LIKE ctu_params-dismode AS LISTBOX VISIBLE LENGTH 25 DEFAULT 'A'.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  IF sy-tcode = 'YRFR035'.
    pnpxabkr = '01'.
    p_rpt = ' '.
    p_dbt = 'X'.
  ENDIF.
  PERFORM initialize_wt.

* AT SELECTION SCREEN--------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant USING p_vari.

AT SELECTION-SCREEN OUTPUT.

  IF p_dbt <> 'X'.
    LOOP AT SCREEN.
      IF screen-name CS 'P_GLCODE'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_BUDAT'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_BUKRS'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_MODE'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_GSBER'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF."        screen-invisible = 1.

  IF p_dbt = 'X'.
    IF sy-tcode <> 'YRFR035'.
      MESSAGE 'You are not authorized for generating Debit Note' TYPE 'E'.
    ENDIF.
    LOOP AT SCREEN.
      IF screen-name CS 'P_GLCODE'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_BUDAT'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_BUKRS'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_MODE'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_GSBER'.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name CS 'P_ASIGN'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_COACC'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_SCF'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_DRVWG'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_QTF'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name CS 'P_VARI'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_RTS'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_TRV'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_PDF'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_DTL'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name CS 'P_RPT'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name CS 'P_DTL'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF."        screen-invisible = 1.
* Begin of Change by XSAP_SUBHEND
AT SELECTION-SCREEN ON p_bukrs.
  PERFORM auth_check.

AT SELECTION-SCREEN ON p_gsber.
  PERFORM auth_check.
* End of Change by XSAP_SUBHEND
START-OF-SELECTION.
  SET PF-STATUS 'STANDARD'.
*  w_end = pnpdised.
  DATA: i_t549q    TYPE TABLE OF t549q,
        wa_t549q   TYPE t549q,
        l_month(2).

***  SOC by Tushar Goyal & Bibhu Ranjan Sahoo on 19/08/2021 2000000337, TG:Changes in CTC Report - YRHS039
  " Validation.
  IF p_from IS INITIAL.
    MESSAGE 'Payroll Month (From) is mandatory.' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
    EXIT.
  ENDIF.

  IF p_year IS INITIAL.
    MESSAGE 'Payroll Year is mandatory.' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
    EXIT.
  ENDIF.

  IF p_from GT p_to.
    MESSAGE 'Payroll Month (From) can not be greater than Payroll Month (To).' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
    EXIT.
  ENDIF.

  IF p_from GT '12' OR p_to GT '12'.
    MESSAGE 'Payroll Month (From) or Payroll Month (To) can not be greater than 12.' TYPE 'S' DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
    EXIT.
  ENDIF.

  IF p_rpt EQ 'X' OR
     p_pdf EQ 'X' OR
     p_dtl EQ 'X' OR
     p_sry EQ 'X'.
  ELSE.
    IF p_from NE p_to.
      MESSAGE 'Payroll Month (From) and Payroll Month (To) must be same' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
      EXIT.
    ENDIF.
  ENDIF.

  CLEAR: i_t549q, wa_t549q.
** -> Begin of changes by of Aditi on 29.11.2024 17:32:31 for ATC
*  SELECT * FROM t549q INTO TABLE i_t549q WHERE permo EQ '01' AND pabrj EQ p_year.
  SELECT * FROM t549q INTO TABLE i_t549q WHERE permo EQ '01' AND pabrj EQ p_year ORDER BY PRIMARY KEY.
** -> End of changes by of Aditi on 29.11.2024 17:32:35 for ATC
  IF sy-subrc EQ 0.
    SORT i_t549q BY pabrp.
  ENDIF.

  CLEAR: l_from_beg, l_from_end, l_to_beg, l_to_end, wa_t549q.
  READ TABLE i_t549q INTO wa_t549q WITH KEY pabrp = p_from.
  IF sy-subrc EQ 0.
    l_from_beg = wa_t549q-begda.
    l_from_end = wa_t549q-endda.
  ENDIF.

  READ TABLE i_t549q INTO wa_t549q WITH KEY pabrp = p_to.
  IF sy-subrc EQ 0.
    l_to_beg = wa_t549q-begda.
    l_to_end = wa_t549q-endda.
  ENDIF.

  CLEAR: l_fin_year, l_year2.
  l_year2 = p_year + 1.
  CONCATENATE p_year l_year2 INTO l_fin_year SEPARATED BY '-'.

  " Fill Internal table with all payroll periods, BEGDA and ENDDA.
  DATA: l_no TYPE i.
  CLEAR: i_period, wa_period, l_no, l_month.

  l_no = ( p_to - p_from ) + 1.
  l_month = p_from.
  UNPACK l_month TO l_month.

  DO l_no TIMES.
    CLEAR: wa_t549q.
    READ TABLE i_t549q INTO wa_t549q WITH KEY pabrp = l_month.
    wa_period-begda = wa_t549q-begda.
    wa_period-endda = wa_t549q-endda.
    wa_period-month = wa_t549q-pabrp.
    wa_period-year  = wa_t549q-pabrj.

    APPEND wa_period TO i_period.
    CLEAR: wa_period.
    l_month = l_month  + 1.
    UNPACK l_month TO l_month.
  ENDDO.

***  EOC by Tushar Goyal & Bibhu Ranjan Sahoo on 19/08/2021 2000000337, TG:Changes in CTC Report - YRHS039

GET pernr.
  IF pernr IS NOT INITIAL.
    IF p0000[] IS NOT INITIAL.
      SORT p0000 BY begda DESCENDING.
      CLEAR p0000.
***  SOC by Tushar Goyal & Bibhu Ranjan Sahoo on 19/08/2021 2000000337, TG:Changes in CTC Report - YRHS039
*    READ TABLE P0000 WITH KEY STAT2 = '3' ENDDA = '99991231'.
      READ TABLE p0000 WITH KEY stat2 = '3'.      " Latest active record.
*      IF p0000-endda >= pnpdised OR p0000-endda > pnpdisbd.
      IF p0000-endda > l_from_beg.                " Latest active record must fall in Payroll period range.
*        IF sy-subrc <> 0.
*          REFRESH p0000.
*        ELSE.
***  EOC by Tushar Goyal & Bibhu Ranjan Sahoo on 19/08/2021 2000000337, TG:Changes in CTC Report - YRHS039
        IF p0001[] IS NOT INITIAL.
          SORT p0001 BY pernr ASCENDING endda DESCENDING.
          CLEAR p0001.
          READ TABLE p0001 WITH KEY endda = '99991231'.
          IF sy-subrc = 0.
            PERFORM append_data.
          ENDIF.
        ENDIF.
*        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

END-OF-SELECTION.

***SELECTION OF WAGE TYPE TEXTS
  SELECT * FROM t512t INTO CORRESPONDING FIELDS OF TABLE itab_t512t
           WHERE sprsl = 'EN' AND molga = '40'.

***SELECTION OF COMPANY CODE TEXT
  SELECT * FROM t001 INTO CORRESPONDING FIELDS OF TABLE itab_t001
           WHERE spras = 'EN' AND ktopl = 'GAIL' .

***SELECTION OF BUSINESS AREA TEXT
  SELECT * FROM tgsbt INTO CORRESPONDING FIELDS OF TABLE itab_tgsbt
           WHERE spras = 'EN'.

***SELECTION OF PRP PERCENTAGE
  SELECT * FROM yrha_grd_percent INTO CORRESPONDING FIELDS OF TABLE
  itab_prp_per WHERE yy_fin_year EQ l_fin_year.

***SELECTION OF GROUP INSURANCE PREMIUM FOR GPA, EDLI, FIN. ASST.
  SELECT * FROM yrha_ctc_grp_ins INTO CORRESPONDING FIELDS OF TABLE
           itab_grp_ins.

***SELECTION OF SUBTYPE FOR IT0045
  SELECT * FROM t591s INTO CORRESPONDING FIELDS OF TABLE itab_t591s
           WHERE sprsl = 'EN' AND infty = '0045'.

***SELECTION OF EXPENSE TYPES TEXT
  SELECT * FROM t706b5 INTO CORRESPONDING FIELDS OF TABLE itab_t706b5
           WHERE spras = 'EN' AND morei = '99'.

***SELECTION OF PERSONAL AREA TEXT
  SELECT * FROM t500p INTO CORRESPONDING FIELDS OF TABLE itab_t500p
           WHERE molga = '40'.

***SELECTION OF PERSONAL SUB AREA TEXT
  SELECT * FROM t001p INTO CORRESPONDING FIELDS OF TABLE itab_t001p
           WHERE molga = '40'.

***SELECTION OF BASIC SLAB
  SELECT * FROM yrha_pay_scale INTO CORRESPONDING FIELDS OF TABLE
           itab_pay_scale WHERE endda = '99991231'.

****selection for driver wages*******************
*    SELECT SINGLE value INTO lv_travel_amt_drv FROM YRHA_CTC_DRIVER.

***ADD by VS on 03.01.2017 RMS 717- SOC
***SELECTION OF BASIC SLAB
  SELECT * FROM yrha_pay_rev2017 INTO CORRESPONDING FIELDS OF TABLE
           itab_pay_rev2017 FOR ALL ENTRIES IN itab_p0001 WHERE yy_grade = itab_p0001-persk AND yy_endda = '99991231'.
**ADD by VS on 03.01.2017 RMS 717- EOC

***SELECTION OF SPECTACLE CLAIMS
  SELECT * FROM yrha_spec_reimb INTO CORRESPONDING FIELDS OF TABLE
           itab_spect WHERE yy_blkyear = sy-datum+0(4).

***SELECTION OF ORG. UNIT TEXT (ORGEH) FOR DEPARTMENT FROM RT
  SELECT * FROM t527x INTO CORRESPONDING FIELDS OF TABLE itab_t527x
           WHERE sprsl = 'EN' AND endda = '99991231'.

***SELECTION OF EMPLOYEE GROUP TEXTS
  SELECT * FROM t501t INTO CORRESPONDING FIELDS OF TABLE itab_t501t
           WHERE sprsl = 'EN'.

***SOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 06.05.2020, Charm No #4000001849
*Remarks: Below code is written to fetch data from table T7INA9 for telephone reim.,
*briefcase reim. and spectacles reim.
  SELECT * FROM t7ina9 INTO TABLE @DATA(gt_t7ina9) WHERE molga = '40' AND
*                                                     AND ( lgart = '4015' OR
*                                                           lgart = '4016' OR
*                                                           lgart = '4060' OR
                                                          ( lgart = '4035' OR
                                                           lgart = '4095' OR lgart = '4064' ) " 4064 wage type added for driver wages logic enhancement
*                                                     AND endda = '99991231'.
                                                     AND begda LE @l_to_end AND endda GE @l_to_end.
***EOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 06.05.2020, Charm No #4000001849

*SELECT * FROM V_T7INB7 INTO CORRESPONDING FIELDS OF TABLE ITAB_T7INB7
*         WHERE BASCD LIKE 'E%' OR BASCD LIKE 'S%'.

  IF itab_p0015[] IS NOT INITIAL.
    DELETE itab_p0015 WHERE subty <> '4095' AND lgart <> '4095' AND
                            subty <> '4018' AND lgart <> '4018' AND
                            subty <> '1404' AND lgart <> '1404' AND " Added by Shruti 9.4.19
***SOC BY SANYOGITA & RIMJHIM ON 27.11.2013 TO RESOLVE DOUBLE ENTRY IN REPORT
                            subty <> '1261' AND lgart <> '1261'.
***EOC BY SANYOGITA & RIMJHIM ON 27.11.2013 TO RESOLVE DOUBLE ENTRY IN REPORT                            .
  ENDIF.

  " Logic for HIRE PURCHASE
  DATA: i_entl  TYPE TABLE OF yrha_hpemplent,
        i_calc  TYPE TABLE OF yctc_calc,
        wa_entl TYPE yrha_hpemplent,
        wa_calc TYPE yctc_calc.

  CLEAR: i_entl, i_calc.
  SELECT * FROM yrha_hpemplent INTO TABLE i_entl.
  IF sy-subrc EQ 0.
    SORT i_entl.
  ENDIF.

  SELECT * FROM yctc_calc INTO TABLE i_calc.
  IF sy-subrc EQ 0.
    SORT i_calc.
  ENDIF.

  DATA: i_t539j  TYPE TABLE OF t539j,
        wa_t539j TYPE t539j.

  DATA: i_t539j2  TYPE TABLE OF t539j,
        wa_t539j2 TYPE t539j.
  CLEAR: i_t539j.
  SELECT * FROM t539j INTO TABLE i_t539j WHERE modul EQ 'PRZNT' AND molga EQ '40' AND bwlga IN ('1041', '1042', '1043', '1051', '1052', '1053','1141', '1142', '1143', '1151', '1152', '1153')
    AND begda LE l_to_end AND endda GE l_to_end.
  IF sy-subrc EQ 0.
    SORT i_t539j BY bwlga.
  ENDIF.

  CLEAR: i_t539j2.
  SELECT * FROM t539j INTO TABLE i_t539j2 WHERE molga EQ '40'  AND bwlga EQ '1008' AND begda LE l_to_end AND endda GE l_to_end.
  IF sy-subrc EQ 0.
    SORT i_t539j2 BY baslg.
  ENDIF.

  CLEAR: i_header, wa_header.
  LOOP AT itab_p0000.
    CLEAR: wa_period.
    LOOP AT i_period INTO wa_period.
      CLEAR: itab_final_h[].
      w_end = wa_period-endda.
      pnpdisbd = wa_period-begda.
      pnpdised = wa_period-endda.
      pn-paper-pabrj = wa_period-year.
      pn-paper-pabrp = wa_period-month.
      pn-begda = wa_period-begda.
      pn-endda = wa_period-endda.

      pn-begps = wa_period-begda.
      pn-endps = wa_period-endda.

      pn-pabrp = wa_period-month.
      pn-pabrj = wa_period-year.
      pn-permo = '01'.

      itab_final_h-yy_pernr = itab_p0000-pernr.
      wa_header-yy_pernr    = itab_p0000-pernr.
      itab_final_d-yy_pernr = itab_p0000-pernr.

      PERFORM get_basic_data.
      PERFORM read_rt_data.
      PERFORM add_selection_data.
      PERFORM get_data_other.
      PERFORM get_offcycle_data.
      PERFORM get_loan_perk.
      PERFORM get_travel_data.

***SOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 06.05.2020, Charm No #4000001849
*Remarks: A new subroutine has been created to implement logic for
*1. Telephone call charges reimbursement
*2. Local conveyance reimbursement
*3. News paper reimbusrement residence
*4. News paper reimbursement office
*5. Briefcase reimbursement
*6. Contact lense reimbursement
      PERFORM get_reimbursement_data.
***EOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 06.05.2020, Charm No #4000001849
    ENDLOOP.
  ENDLOOP.

  CLEAR: i_data, wa_data, itab_final_d1[].
  SORT itab_final_d BY yy_pernr yy_lgart yy_fpbeg DESCENDING..
  itab_final_d1[] = itab_final_d[].
  DELETE ADJACENT DUPLICATES FROM itab_final_d1[] COMPARING yy_pernr yy_lgart.
  SELECT * FROM yrhr_ctc_sc INTO TABLE lt_perc WHERE begda <= l_to_end AND endda >= l_to_end .
  IF  sy-subrc = 0.
    lv_flag = 'X'.
  ENDIF.

  LOOP AT itab_final_d1.
    MOVE-CORRESPONDING itab_final_d1 TO wa_data.
    CLEAR: wa_data-yy_betrg.
    wa_data-yy_fpbeg = l_from_beg.
    wa_data-yy_fpend = l_to_end.
    wa_data-yy_start_period = l_from_beg.
    wa_data-yy_end_period = l_to_end.

    LOOP AT itab_final_d WHERE yy_pernr EQ itab_final_d1-yy_pernr AND yy_lgart EQ itab_final_d1-yy_lgart.
      wa_data-yy_betrg = wa_data-yy_betrg + itab_final_d-yy_betrg.


    ENDLOOP.
*******************************SOC-Priya Tiwari***********************
    IF lv_flag = 'X'.
      READ TABLE lt_perc INTO ls_perc INDEX 1.
      IF ls_perc-wage1 = itab_final_d1-yy_lgart .
        lv_yy_perc_amt = ( wa_data-yy_betrg + wa_retro_basic_rate-betrg ) * ls_perc-percentage / 100.
      ELSEIF ls_perc-wage2 = itab_final_d1-yy_lgart .
        lv_yy_perc_amt = ( wa_data-yy_betrg + wa_retro_basic_rate-betrg ) * ls_perc-percentage / 100.
      ELSEIF ls_perc-wage3 = itab_final_d1-yy_lgart .
        lv_yy_perc_amt = ( wa_data-yy_betrg + wa_retro_basic_rate-betrg ) * ls_perc-percentage / 100.
      ELSEIF ls_perc-wage4 = itab_final_d1-yy_lgart .
        lv_yy_perc_amt = ( wa_data-yy_betrg + wa_retro_basic_rate-betrg ) * ls_perc-percentage / 100.
      ELSEIF ls_perc-wage5 = itab_final_d1-yy_lgart .
        lv_yy_perc_amt = ( wa_data-yy_betrg + wa_retro_basic_rate-betrg ) * ls_perc-percentage / 100.
      ELSEIF ls_perc-wage6 = itab_final_d1-yy_lgart .
        lv_yy_perc_amt = ( wa_data-yy_betrg + wa_retro_basic_rate-betrg ) * ls_perc-percentage / 100.
      ENDIF.
    ENDIF.

******************************EOC-Priya Tiwari********************************************
    APPEND wa_data TO i_data.
*********************************************SOC-CHANGE*******************************
  ENDLOOP.
  wa_data-yy_fpbeg = l_from_beg.
  wa_data-yy_fpend = l_to_end.
  wa_data-yy_start_period = l_from_beg.
  wa_data-yy_end_period = l_to_end.
  wa_data-yy_ben_type = '05'.
  wa_data-yy_ben_typ_txt = 'Social Security' .
  wa_data-yy_betrg = lv_yy_perc_amt .
  APPEND wa_data TO i_data.

**************************EOC-Priya Tiwari****************************************************************************************
  IF i_data IS NOT INITIAL.
    CLEAR: itab_final_d[].
    itab_final_d[] = i_data[].
  ENDIF.

  IF i_header IS NOT INITIAL.
    SORT i_header BY yy_pernr yy_fpbeg DESCENDING.
    DELETE ADJACENT DUPLICATES FROM i_header COMPARING yy_pernr.

    LOOP AT i_header INTO wa_header.
      wa_header-yy_fpbeg = l_from_beg.
      wa_header-yy_fpend = l_to_end.
      wa_header-yy_start_period = l_from_beg.
      wa_header-yy_end_period = l_to_end.
      MODIFY i_header FROM wa_header INDEX sy-tabix.
    ENDLOOP.

    CLEAR: itab_final_h[].
    itab_final_h[] = i_header[].
  ENDIF.

*BREAK-POINT.
  IF itab_final_h[] IS NOT INITIAL AND itab_final_d[] IS NOT INITIAL.
    SORT itab_final_h BY yy_pernr yy_start_period.
    SORT itab_final_d BY yy_pernr yy_start_period yy_lgart.
    SORT itab_final_h BY yy_pernr yy_fpbeg yy_start_period yy_bukrs
                         yy_gsber yy_werks yy_btrtl.
    SORT itab_final_d BY yy_pernr yy_fpbeg yy_start_period yy_werks
                         yy_btrtl yy_ben_type yy_lgart.


    LOOP AT itab_final_h.
      LOOP AT itab_final_d
              WHERE yy_pernr = itab_final_h-yy_pernr AND
                    yy_start_period >= itab_final_h-yy_start_period AND
                    yy_end_period <= itab_final_h-yy_end_period.

        IF itab_final_d-yy_start_period > itab_final_h-yy_start_period.
          itab_final_d-yy_start_period = itab_final_h-yy_start_period.
          MODIFY itab_final_d TRANSPORTING yy_start_period
               WHERE yy_pernr = itab_final_h-yy_pernr AND
                     yy_lgart = itab_final_d-yy_lgart AND
                     yy_werks = itab_final_h-yy_werks AND
                     yy_btrtl = itab_final_h-yy_btrtl.
        ENDIF.
        IF itab_final_d-yy_end_period < itab_final_h-yy_end_period.
          itab_final_d-yy_end_period = itab_final_h-yy_end_period.
          MODIFY itab_final_d TRANSPORTING yy_end_period
               WHERE yy_pernr = itab_final_h-yy_pernr AND
                     yy_lgart = itab_final_d-yy_lgart AND
                     yy_werks = itab_final_h-yy_werks AND
                     yy_btrtl = itab_final_h-yy_btrtl.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    SORT itab_final_h BY yy_pernr yy_fpbeg yy_start_period yy_bukrs
                         yy_gsber yy_werks yy_btrtl.
    SORT itab_final_d BY yy_pernr yy_fpbeg yy_start_period yy_werks
                         yy_btrtl yy_ben_type yy_lgart.

    LOOP AT itab_final_h.
      itab_final_h-yy_total_ctc = 0.
      w_count = 0.
      LOOP AT itab_final_d

              WHERE yy_pernr = itab_final_h-yy_pernr AND
                    yy_start_period >= itab_final_h-yy_start_period AND
                    yy_end_period <= itab_final_h-yy_end_period.
        IF itab_final_d-yy_lgart NE '1404'. " added by Shruti 10.4.19
          w_count = w_count + 1.
          itab_final_d-yy_sno = w_count.
          MODIFY itab_final_d TRANSPORTING yy_sno
                 WHERE yy_pernr = itab_final_h-yy_pernr AND
                       yy_lgart = itab_final_d-yy_lgart AND
                       yy_werks = itab_final_h-yy_werks AND
                       yy_btrtl = itab_final_h-yy_btrtl.

          itab_final_h-yy_total_ctc = itab_final_h-yy_total_ctc +
                                      itab_final_d-yy_betrg.
        ENDIF.

      ENDLOOP.

***SOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 06.05.2020, Charm No #4000001849
*Remarks: The below code is written to round off Total CTC value.
      DATA: gv_amount_im TYPE j_1itaxvar-j_1itaxam1,
            gv_amount_ex TYPE j_1itaxvar-j_1itaxam1.

      gv_amount_im = itab_final_h-yy_total_ctc.
      CALL FUNCTION 'J_1I6_ROUND_TO_NEAREST_AMT'
        EXPORTING
          i_amount = gv_amount_im
        IMPORTING
          e_amount = gv_amount_ex.

      itab_final_h-yy_total_ctc = gv_amount_ex.
***EOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 06.05.2020, Charm No #4000001849

      MODIFY itab_final_h TRANSPORTING yy_total_ctc.
    ENDLOOP.

*BREAK-POINT.

    PERFORM get_asg_data.

    CLEAR itab_final_h2.
    REFRESH itab_final_h2.
    itab_final_h2[] = itab_final_h[].
    PERFORM get_final_retro_data.

    DELETE itab_final_d WHERE yy_lgart = '1404'. " added by Shruti 10.4.19

    IF p_dbt = 'X' AND sy-tcode <> 'YRFR035'.
      MESSAGE 'You are not authorized to generate Debit Note' TYPE 'E'.
    ENDIF.
    IF p_rpt = 'X'.
      PERFORM print_ctc.
    ELSEIF p_pdf = 'X'.
      PERFORM smartform_to_pdf.
      PERFORM print_ctc.
    ELSEIF p_dtl = 'X'.
      PERFORM detail_ctc.
      IF itab_ctc_dtl[] IS NOT INITIAL.
        PERFORM summary_report.
      ENDIF.
    ELSEIF p_sry = 'X' OR p_dbt = 'X'.
*    IF
      PERFORM move_to_alv_table.
*    ENDIF.
      PERFORM summary_report.
    ELSEIF p_trv = 'X'.
      PERFORM summary_report.
*  ELSEIF P_RTR = 'X'.
    ELSEIF p_rts = 'X'.
      PERFORM display_retro_summary.
*  ELSEIF P_RPD = 'X'.
    ENDIF.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  GET_BASIC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_basic_data .

  CLEAR itab_p0001.
  READ TABLE itab_p0001 WITH KEY pernr = itab_p0000-pernr.

  IF itab_p0001 IS NOT INITIAL.
    itab_final_h-yy_name  = itab_p0001-ename.

*    APPEND itab_final_h.
  ENDIF.

ENDFORM.                    " GET_BASIC_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_RT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_rt_data .

  DATA: w_count_header TYPE i.

* Read cluster CU
  CALL FUNCTION 'CU_READ_RGDIR_NEW'
    EXPORTING
      persnr                = itab_p0000-pernr
      check_read_authority  = space
*     IMP_CLIENT            =
    IMPORTING
      molga                 = w_country_code
    TABLES
      in_rgdir              = it_rgdir
    EXCEPTIONS
      no_record_found       = 1
      import_mismatch_error = 2
      no_read_authority     = 3
      OTHERS                = 4.

  IF sy-subrc <> 0.
  ENDIF.

  CLEAR itab_rgdir_p.
  REFRESH itab_rgdir_p.
  CLEAR itab_rgdir_a.
  REFRESH itab_rgdir_a.

  IF it_rgdir[] IS NOT INITIAL.

    PERFORM get_retro_data.
*   Soc by sachin to get the cluster data of claims (new functionality)
    PERFORM get_cluster_data.

*   Eoc by sachin to get the cluster data of claims (new functionality)
    "" CHeck
    CLEAR: wa_rgdir.
    READ TABLE it_rgdir INTO wa_rgdir WITH KEY fpper = pn-paper
                                               inper = pn-paper.
*                                             SRTZA = 'A'.
*    CONCATENATE wa_rgdir-fpbeg+6(2) '.' wa_rgdir-fpbeg+4(2) '.'
*                wa_rgdir-fpbeg+0(4) INTO w_from_dt.
*    CONCATENATE wa_rgdir-fpend+6(2) '.' wa_rgdir-fpend+4(2) '.'
*                wa_rgdir-fpend+0(4) INTO w_to_dt.

    CONCATENATE l_from_beg+6(2) '.' l_from_beg+4(2) '.'
                l_from_beg+0(4) INTO w_from_dt.
    CONCATENATE l_to_end+6(2) '.' l_to_end+4(2) '.'
                l_to_end+0(4) INTO w_to_dt.


    IF sy-subrc = 0 AND NOT wa_rgdir IS INITIAL.
      MOVE wa_rgdir-fpend TO w_end_pay_period.
    ENDIF.

* Get Personnel Country Grouping
    SELECT SINGLE * FROM t500l INTO wa_t500l
                    WHERE molga = w_country_code.

    IF sy-subrc <> 0.
      MESSAGE e000(yh01) WITH 'Personnel Country Grouping not found.'(003).
    ELSE.
* Area identifier for cluster in tables PCLx
      MOVE wa_t500l-relid TO w_relid.
    ENDIF.

  ENDIF.

  CLEAR wa_result.
* Read Payroll Results
  CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
    EXPORTING
      clusterid                    = w_relid
      employeenumber               = itab_p0000-pernr
      sequencenumber               = wa_rgdir-seqnr
      read_only_international      = 'X'
      check_read_authority         = space
    CHANGING
      payroll_result               = wa_result
    EXCEPTIONS
      illegal_isocode_or_clusterid = 1
      error_generating_import      = 2
      import_mismatch_error        = 3
      subpool_dir_full             = 4
      no_read_authority            = 5
      no_record_found              = 6
      versions_do_not_match        = 7
      error_reading_archive        = 8
      error_reading_relid          = 9
      OTHERS                       = 10.
  IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*       WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR itab_wpbp.
  REFRESH itab_wpbp.
  CLEAR w_split_flag.

  IF wa_result IS NOT INITIAL.
*  W_COUNT = 0.
*  BREAK-POINT.
    SORT wa_result-inter-wpbp BY begda apznr.
    itab_wpbp[] = wa_result-inter-wpbp[].
    CLEAR itab_final_d1.
    REFRESH itab_final_d1.
    LOOP AT wa_result-inter-wpbp INTO wa_wpbp.

      itab_final_h-yy_fpper = wa_rgdir-fpper.
      itab_final_h-yy_fpbeg = wa_rgdir-fpbeg.
      itab_final_h-yy_fpend = wa_rgdir-fpend.
      itab_final_d1-yy_fpper = wa_rgdir-fpper.
      itab_final_d1-yy_fpbeg = wa_rgdir-fpbeg.
      itab_final_d1-yy_fpend = wa_rgdir-fpend.

      IF wa_wpbp-apznr = '01'.

        itab_final_h-yy_apznr = wa_wpbp-apznr.
        itab_final_h-yy_start_period = wa_wpbp-begda.
        itab_final_h-yy_end_period = wa_wpbp-endda.
        itab_final_h-yy_bukrs = wa_wpbp-bukrs.
        CLEAR itab_t001.
        READ TABLE itab_t001 WITH KEY bukrs = itab_final_h-yy_bukrs.
        IF itab_t001 IS NOT INITIAL.
          itab_final_h-yy_butxt = itab_t001-butxt.
        ENDIF.

        itab_final_h-yy_gsber = wa_wpbp-gsber.
        CLEAR itab_tgsbt.
        READ TABLE itab_tgsbt WITH KEY gsber = itab_final_h-yy_gsber.
        IF itab_tgsbt IS NOT INITIAL.
          itab_final_h-yy_gtext = itab_tgsbt-gtext.
        ENDIF.

        itab_final_h-yy_werks = wa_wpbp-werks.
        CLEAR itab_t500p.
        READ TABLE itab_t500p WITH KEY persa = itab_final_h-yy_werks.
        IF itab_t500p IS NOT INITIAL.
          itab_final_h-yy_name1 = itab_t500p-name1.
        ENDIF.

        itab_final_h-yy_btrtl = wa_wpbp-btrtl.
        CLEAR itab_t001p.
        READ TABLE itab_t001p WITH KEY werks = itab_final_h-yy_werks
                                       btrtl = itab_final_h-yy_btrtl.
        IF itab_t001p IS NOT INITIAL.
          itab_final_h-yy_location = itab_t001p-btext.
        ENDIF.

        itab_final_h-yy_grade = wa_wpbp-persk.
** -> Begin of changes by of Aditi on 29.11.2024 17:24:50 for ATC
*        SELECT SINGLE mc_stext INTO w_designation FROM hrp1000
*               WHERE otype = 'S' AND plvar = '01' AND
*                     langu = 'E' AND objid = wa_wpbp-plans.
        SELECT mc_stext INTO w_designation FROM hrp1000
               UP TO 1 ROWS
               WHERE otype = 'S' AND plvar = '01' AND
                     langu = 'E' AND objid = wa_wpbp-plans
               ORDER BY PRIMARY KEY.
        ENDSELECT.
** -> End of changes by of Aditi on 29.11.2024 17:24:53 for ATC
        itab_final_h-yy_designation = w_designation.

        CLEAR itab_t527x.
        READ TABLE itab_t527x WITH KEY orgeh = wa_wpbp-orgeh.
        itab_final_h-yy_department = itab_t527x-orgtx.

        itab_final_h-yy_persg = wa_wpbp-persg.
        CLEAR itab_t501t.
        READ TABLE itab_t501t WITH KEY persg = wa_wpbp-persg.
        itab_final_h-yy_ptext = itab_t501t-ptext.

        IF itab_pay_rev2017[] IS NOT INITIAL.
          CLEAR itab_pay_rev2017.
          READ TABLE itab_pay_rev2017 WITH KEY yy_grade = wa_wpbp-persk.
          IF sy-subrc = 0.
            itab_final_h-yypayscale_min = itab_pay_rev2017-yy_new_basic_low.
            itab_final_h-yypayscale_max = itab_pay_rev2017-yy_new_basic_hgh.
          ENDIF.
        ENDIF.

        itab_final_h-yy_py_prd_days = ( itab_final_h-yy_fpend -
          itab_final_h-yy_fpbeg + 1 ).
        itab_final_h-yy_prd_days = ( itab_final_h-yy_end_period -
          itab_final_h-yy_start_period + 1 ).

        APPEND itab_final_h.
        MOVE-CORRESPONDING itab_final_h TO wa_header.
        APPEND wa_header TO i_header.
      ELSE.
        IF wa_wpbp-apznr <> itab_final_h-yy_apznr.
***  SOC by Tushar Goyal & Bibhu Ranjan Sahoo on 04/10/2021 4000004245, TG:CTC Report Modification
*          IF itab_final_h-yy_werks = wa_wpbp-werks.
*            IF itab_final_h-yy_btrtl = wa_wpbp-btrtl.
          itab_final_h-yy_apznr = wa_wpbp-apznr.
          itab_final_h-yy_end_period = wa_wpbp-endda.

          itab_final_h-yy_prd_days = ( itab_final_h-yy_end_period - " Logic is incorrect
            itab_final_h-yy_start_period + 1 ).

          MODIFY itab_final_h TRANSPORTING yy_apznr yy_end_period
                                           yy_prd_days
                 WHERE yy_pernr = itab_final_h-yy_pernr AND
                       yy_start_period = itab_final_h-yy_start_period.

          MODIFY i_header FROM wa_header TRANSPORTING yy_apznr yy_end_period
                                    yy_prd_days
          WHERE yy_pernr = itab_final_h-yy_pernr AND
                yy_start_period = itab_final_h-yy_start_period.
***            ELSE.
***              w_split_flag = w_split_flag + 1.
***              IF w_split_flag = 1.
***                itab_final_h-yy_apznr = wa_wpbp-apznr.
***                itab_final_h-yy_end_period = wa_wpbp-endda.
***                itab_final_h-yy_prd_days = ( itab_final_h-yy_end_period -
***                                      itab_final_h-yy_start_period + 1 ).
***
***                MODIFY itab_final_h TRANSPORTING yy_apznr yy_end_period
***                                                 yy_prd_days
***                     WHERE yy_pernr = itab_final_h-yy_pernr AND
***                           yy_start_period = itab_final_h-yy_start_period.
***
***                MODIFY i_header FROM wa_header TRANSPORTING yy_apznr yy_end_period
***                                 yy_prd_days
***     WHERE yy_pernr = itab_final_h-yy_pernr AND
***           yy_start_period = itab_final_h-yy_start_period.
***              ELSE.
***                itab_final_h-yy_apznr = wa_wpbp-apznr.
***                itab_final_h-yy_start_period = wa_wpbp-begda.
***                itab_final_h-yy_end_period = wa_wpbp-endda.
***                itab_final_h-yy_bukrs = wa_wpbp-bukrs.
***                CLEAR itab_t001.
***                READ TABLE itab_t001 WITH KEY
***                                      bukrs = itab_final_h-yy_bukrs.
***                IF itab_t001 IS NOT INITIAL.
***                  itab_final_h-yy_butxt = itab_t001-butxt.
***                ENDIF.
***
***                itab_final_h-yy_gsber = wa_wpbp-gsber.
***                CLEAR itab_tgsbt.
***                READ TABLE itab_tgsbt WITH KEY
***                                gsber = itab_final_h-yy_gsber
***  .
***                IF itab_tgsbt IS NOT INITIAL.
***                  itab_final_h-yy_gtext = itab_tgsbt-gtext.
***                ENDIF.
***
***                itab_final_h-yy_werks = wa_wpbp-werks.
***                CLEAR itab_t500p.
***                READ TABLE itab_t500p WITH KEY
***                                    persa = itab_final_h-yy_werks
***  .
***                IF itab_t500p IS NOT INITIAL.
***                  itab_final_h-yy_name1 = itab_t500p-name1.
***                ENDIF.
***
***                itab_final_h-yy_btrtl = wa_wpbp-btrtl.
***                CLEAR itab_t001p.
***                READ TABLE itab_t001p WITH KEY
***                                      werks = itab_final_h-yy_werks
***                                      btrtl = itab_final_h-yy_btrtl
***  .
***                IF itab_t001p IS NOT INITIAL.
***                  itab_final_h-yy_location = itab_t001p-btext.
***                ENDIF.
***
***                itab_final_h-yy_grade = wa_wpbp-persk.
***
***                SELECT SINGLE mc_stext INTO w_designation FROM hrp1000
***                       WHERE otype = 'S' AND plvar = '01' AND
***                             langu = 'E' AND objid = wa_wpbp-plans.
***                itab_final_h-yy_designation = w_designation.
***
***                CLEAR itab_t527x.
***                READ TABLE itab_t527x WITH KEY orgeh = wa_wpbp-orgeh.
***                itab_final_h-yy_department = itab_t527x-orgtx.
***
***                itab_final_h-yy_persg = wa_wpbp-persg.
***                CLEAR itab_t501t.
***                READ TABLE itab_t501t WITH KEY persg = wa_wpbp-persg.
***                itab_final_h-yy_ptext = itab_t501t-ptext.
***
***                IF itab_pay_scale[] IS NOT INITIAL.
***                  CLEAR itab_pay_scale.
***                  READ TABLE itab_pay_scale WITH KEY
***                                        persk = wa_wpbp-persk.
***
***                  IF itab_pay_scale IS NOT INITIAL.
***                    itab_final_h-yypayscale_min =
***                                            itab_pay_scale-yypayscale_min.
***                    itab_final_h-yypayscale_max =
***                                            itab_pay_scale-yypayscale_max.
***                  ENDIF.
***                ENDIF.
***
***                itab_final_h-yy_prd_days = ( itab_final_h-yy_end_period -
***                                      itab_final_h-yy_start_period + 1 ).
***                APPEND itab_final_h.
***                MOVE-CORRESPONDING itab_final_h TO wa_header.
***                APPEND wa_header TO i_header.
***              ENDIF.
***            ENDIF.
***          ELSE.
***            w_split_flag = w_split_flag + 1.
****            IF w_split_flag = 1.
***            IF w_split_flag GE 1.
***              itab_final_h-yy_apznr = wa_wpbp-apznr.
***              itab_final_h-yy_end_period = wa_wpbp-endda.
***              itab_final_h-yy_prd_days = ( itab_final_h-yy_end_period -
***                                    itab_final_h-yy_start_period + 1 ).
***
***              MODIFY itab_final_h TRANSPORTING yy_apznr yy_end_period
***                                               yy_prd_days
***                   WHERE yy_pernr = itab_final_h-yy_pernr AND
***                         yy_start_period = itab_final_h-yy_start_period.
***
***              MODIFY i_header FROM wa_header TRANSPORTING yy_apznr yy_end_period
***                                 yy_prd_days
***     WHERE yy_pernr = itab_final_h-yy_pernr AND
***           yy_start_period = itab_final_h-yy_start_period.
***            ELSE.
***              itab_final_h-yy_apznr = wa_wpbp-apznr.
***              itab_final_h-yy_start_period = wa_wpbp-begda.
***              itab_final_h-yy_end_period = wa_wpbp-endda.
***              itab_final_h-yy_bukrs = wa_wpbp-bukrs.
***              CLEAR itab_t001.
***              READ TABLE itab_t001 WITH KEY bukrs = itab_final_h-yy_bukrs.
***              IF itab_t001 IS NOT INITIAL.
***                itab_final_h-yy_butxt = itab_t001-butxt.
***              ENDIF.
***
***              itab_final_h-yy_gsber = wa_wpbp-gsber.
***              CLEAR itab_tgsbt.
***              READ TABLE itab_tgsbt WITH KEY gsber = itab_final_h-yy_gsber
***  .
***              IF itab_tgsbt IS NOT INITIAL.
***                itab_final_h-yy_gtext = itab_tgsbt-gtext.
***              ENDIF.
***
***              itab_final_h-yy_werks = wa_wpbp-werks.
***              CLEAR itab_t500p.
***              READ TABLE itab_t500p WITH KEY persa = itab_final_h-yy_werks
***  .
***              IF itab_t500p IS NOT INITIAL.
***                itab_final_h-yy_name1 = itab_t500p-name1.
***              ENDIF.
***
***              itab_final_h-yy_btrtl = wa_wpbp-btrtl.
***              CLEAR itab_t001p.
***              READ TABLE itab_t001p WITH KEY werks = itab_final_h-yy_werks
***                                             btrtl = itab_final_h-yy_btrtl
***  .
***              IF itab_t001p IS NOT INITIAL.
***                itab_final_h-yy_location = itab_t001p-btext.
***              ENDIF.
***
***              itab_final_h-yy_grade = wa_wpbp-persk.
***
***              SELECT SINGLE mc_stext INTO w_designation FROM hrp1000
***                     WHERE otype = 'S' AND plvar = '01' AND
***                           langu = 'E' AND objid = wa_wpbp-plans.
***              itab_final_h-yy_designation = w_designation.
***
***              CLEAR itab_t527x.
***              READ TABLE itab_t527x WITH KEY orgeh = wa_wpbp-orgeh.
***              itab_final_h-yy_department = itab_t527x-orgtx.
***
***              itab_final_h-yy_persg = wa_wpbp-persg.
***              CLEAR itab_t501t.
***              READ TABLE itab_t501t WITH KEY persg = wa_wpbp-persg.
***              itab_final_h-yy_ptext = itab_t501t-ptext.
***
***              IF itab_pay_scale[] IS NOT INITIAL.
***                CLEAR itab_pay_scale.
***                READ TABLE itab_pay_scale WITH KEY
***                                      persk = wa_wpbp-persk.
***
***                IF itab_pay_scale IS NOT INITIAL.
***                  itab_final_h-yypayscale_min =
***                                          itab_pay_scale-yypayscale_min.
***                  itab_final_h-yypayscale_max =
***                                          itab_pay_scale-yypayscale_max.
***                ENDIF.
***              ENDIF.
***
***              itab_final_h-yy_prd_days = ( itab_final_h-yy_end_period -
***                                    itab_final_h-yy_start_period + 1 ).
***              APPEND itab_final_h.
***              MOVE-CORRESPONDING itab_final_h TO wa_header.
***              APPEND wa_header TO i_header.
***            ENDIF.
***          ENDIF.
        ENDIF.

      ENDIF.

    ENDLOOP.

    CLEAR: w_pp, w_da, w_basic, w_basic_rate.
    LOOP AT wa_result-inter-rt INTO wa_rt WHERE lgart IN w_wagetypes.
      itab_final_d1-yy_pernr = itab_final_h-yy_pernr.

      CLEAR itab_final_d1-yy_sno.
      CLEAR itab_final_d1-yy_lgart.
      CLEAR itab_final_d1-yy_betrg.
      CLEAR itab_final_d1-yy_lgtxt.

      IF wa_rt-lgart = '9000'.
        w_basic_rate = wa_rt-betrg.
      ENDIF.

      CLEAR itab_wpbp.
      READ TABLE itab_wpbp WITH KEY apznr = wa_rt-apznr.

      LOOP AT itab_final_h WHERE yy_pernr = itab_final_d1-yy_pernr.
        IF itab_wpbp-begda >= itab_final_h-yy_start_period AND
           itab_wpbp-endda <= itab_final_h-yy_end_period.
          itab_final_d1-yy_werks = itab_final_h-yy_werks.
          itab_final_d1-yy_btrtl = itab_final_h-yy_btrtl.
        ELSE.
          itab_final_d1-yy_werks = itab_final_h-yy_werks.
          itab_final_d1-yy_btrtl = itab_final_h-yy_btrtl.
        ENDIF.

      ENDLOOP.

      itab_final_d1-yy_start_period = itab_wpbp-begda.
      itab_final_d1-yy_end_period = itab_wpbp-endda.

      IF itab_final_d1-yy_start_period IS INITIAL.
        itab_final_d1-yy_start_period = itab_final_d1-yy_fpbeg.
      ENDIF.
      IF itab_final_d1-yy_end_period IS INITIAL.
        itab_final_d1-yy_end_period = itab_final_d1-yy_fpend.
      ENDIF.

      itab_final_d1-yy_apznr = wa_rt-apznr.
      itab_final_d1-yy_lgart = wa_rt-lgart.
      itab_final_d1-yy_betrg = wa_rt-betrg.

      CLEAR itab_t512t.
      READ TABLE itab_t512t WITH KEY lgart = wa_rt-lgart.
      itab_final_d1-yy_lgtxt = itab_t512t-lgtxt.

      IF wa_rt-lgart = '1000' OR wa_rt-lgart = '1001' OR
         wa_rt-lgart = '1005' OR wa_rt-lgart = '1006' OR
         wa_rt-lgart = '1007' OR wa_rt-lgart = '1008' OR
         wa_rt-lgart = '1010' OR wa_rt-lgart = '1015' OR
         wa_rt-lgart = '1020' OR wa_rt-lgart = '1021' OR wa_rt-lgart = '1022' OR   " Added by Tushar
         wa_rt-lgart = '1034'." OR wa_rt-lgart = '1040'.

        itab_final_d1-yy_ben_type = 1.
        itab_final_d1-yy_ben_typ_txt = 'BASIC SALARY'.
      ELSEIF wa_rt-lgart = '1221' OR wa_rt-lgart = '1222' OR
             wa_rt-lgart = '1261'.

        itab_final_d1-yy_ben_type = 4.
        itab_final_d1-yy_ben_typ_txt = 'INCENTIVES'.
*        ELSEIF wa_rt-lgart = '/3F3' OR wa_rt-lgart = '/3W2' OR wa_rt-lgart = '/3F4'.
*          itab_final_d1-yy_ben_type = 4.
*          itab_final_d1-yy_ben_typ_txt = 'SUPERANNUATION BENEFITS'.
***  SOC by Tushar on 28/12/2021 4000004444, TG:YRHS039-Add LWF WT in CTC Report
      ELSEIF wa_rt-lgart EQ '/3W2'.
        itab_final_d1-yy_ben_type = 5.
        itab_final_d1-yy_ben_typ_txt = 'SOCIAL SECURITY'.
***  EOC by Tushar on 28/12/2021 4000004444, TG:YRHS039-Add LWF WT in CTC Report
      ELSE.
        itab_final_d1-yy_ben_type = 3.
        itab_final_d1-yy_ben_typ_txt = 'PERQUISITES'.
      ENDIF.

      IF wa_rt-lgart <> '9000' AND itab_final_d1-yy_betrg <> 0.
***SOC BY SANYOGITA & BHARAT ON 06.05.2013 - EXCLUDE WT 1403 FOR DATE MORE THAN 01.04.2012
        IF wa_rt-lgart = '1403'.
          IF pnpdisbd < '20120401'.
            APPEND itab_final_d1.
          ENDIF.
        ELSE.
          APPEND itab_final_d1.
        ENDIF.
      ENDIF.
*      ENDIF.

    ENDLOOP.

    " Logic for company accomodation
    RANGES: w_wagetypes2 FOR pc207-lgart.
    CLEAR:w_wagetypes2, wa_t539j.

    w_wagetypes2-sign       = 'I'.
    w_wagetypes2-option     = 'EQ'.
    LOOP AT i_t539j INTO wa_t539j.
      w_wagetypes2-low        = wa_t539j-baslg.
      APPEND w_wagetypes2.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM w_wagetypes2.
*    " Company Owned Accomodation ITAB_P0001-PERSK
    CLEAR: itab_final_d1-yy_betrg.
    LOOP AT itab_p0581 WHERE pernr = itab_final_d-yy_pernr AND begda LE wa_period-endda AND endda GE wa_period-begda AND accom = '9'.
      IF itab_p0581-begda LT wa_period-begda.
        itab_p0581-begda = wa_period-begda.
      ENDIF.

      IF itab_p0581-endda GT wa_period-endda.
        itab_p0581-endda = wa_period-endda.
      ENDIF.


      IF itab_p0581-yy_city_cat EQ 'X'.
        IF itab_p0001-persk+0(1) EQ 'S'.
          LOOP AT wa_result-inter-rt INTO wa_rt WHERE lgart IN w_wagetypes2.
            CLEAR: wa_t539j.
            READ TABLE i_t539j INTO wa_t539j WITH KEY bwlga = '1041' baslg = wa_rt-lgart.
            IF sy-subrc EQ 0.
              itab_final_d1-yy_betrg = itab_final_d1-yy_betrg +
             ( ( ( ( itab_p0581-endda - itab_p0581-begda ) + 1 ) / ( ( wa_period-endda - wa_period-begda ) + 1 ) ) * wa_rt-betrg * wa_t539j-gwcht  / 100 ).
            ENDIF.
          ENDLOOP.
        ELSE.

          LOOP AT wa_result-inter-rt INTO wa_rt WHERE lgart IN w_wagetypes2.
            CLEAR: wa_t539j.
            READ TABLE i_t539j INTO wa_t539j WITH KEY bwlga = '1051' baslg = wa_rt-lgart.
            IF sy-subrc EQ 0.
              itab_final_d1-yy_betrg = itab_final_d1-yy_betrg +
             ( ( ( ( itab_p0581-endda - itab_p0581-begda ) + 1 ) / ( ( wa_period-endda - wa_period-begda ) + 1 ) ) * wa_rt-betrg * wa_t539j-gwcht  / 100 ).
            ENDIF.
          ENDLOOP.
        ENDIF.

      ELSEIF itab_p0581-yy_city_cat EQ 'Y'.
        IF itab_p0001-persk+0(1) EQ 'S'.
          LOOP AT wa_result-inter-rt INTO wa_rt WHERE lgart IN w_wagetypes2.
            CLEAR: wa_t539j.
            READ TABLE i_t539j INTO wa_t539j WITH KEY bwlga = '1042' baslg = wa_rt-lgart.
            IF sy-subrc EQ 0.
              itab_final_d1-yy_betrg = itab_final_d1-yy_betrg +
             ( ( ( ( itab_p0581-endda - itab_p0581-begda ) + 1 ) / ( ( wa_period-endda - wa_period-begda ) + 1 ) ) * wa_rt-betrg * wa_t539j-gwcht  / 100 ).
            ENDIF.
          ENDLOOP.
        ELSE.
          LOOP AT wa_result-inter-rt INTO wa_rt WHERE lgart IN w_wagetypes2.
            CLEAR: wa_t539j.
            READ TABLE i_t539j INTO wa_t539j WITH KEY bwlga = '1052' baslg = wa_rt-lgart.
            IF sy-subrc EQ 0.
              itab_final_d1-yy_betrg = itab_final_d1-yy_betrg +
             ( ( ( ( itab_p0581-endda - itab_p0581-begda ) + 1 ) / ( ( wa_period-endda - wa_period-begda ) + 1 ) ) * wa_rt-betrg * wa_t539j-gwcht  / 100 ).
            ENDIF.
          ENDLOOP.
        ENDIF.
      ELSE.    " Z
        IF itab_p0001-persk+0(1) EQ 'S'.
          LOOP AT wa_result-inter-rt INTO wa_rt WHERE lgart IN w_wagetypes2.
            CLEAR: wa_t539j.
            READ TABLE i_t539j INTO wa_t539j WITH KEY bwlga = '1043' baslg = wa_rt-lgart.
            IF sy-subrc EQ 0.
              itab_final_d1-yy_betrg = itab_final_d1-yy_betrg +
             ( ( ( ( itab_p0581-endda - itab_p0581-begda ) + 1 ) / ( ( wa_period-endda - wa_period-begda ) + 1 ) ) * wa_rt-betrg * wa_t539j-gwcht  / 100 ).
            ENDIF.
          ENDLOOP.
        ELSE.
          LOOP AT wa_result-inter-rt INTO wa_rt WHERE lgart IN w_wagetypes2.
            CLEAR: wa_t539j.
            READ TABLE i_t539j INTO wa_t539j WITH KEY bwlga = '1053' baslg = wa_rt-lgart.
            IF sy-subrc EQ 0.
              itab_final_d1-yy_betrg = itab_final_d1-yy_betrg +
             ( ( ( ( itab_p0581-endda - itab_p0581-begda ) + 1 ) / ( ( wa_period-endda - wa_period-begda ) + 1 ) ) * wa_rt-betrg * wa_t539j-gwcht  / 100 ).
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF itab_final_d1-yy_betrg IS NOT INITIAL.
      CLEAR itab_final_d1-yy_sno.
      CLEAR itab_final_d1-yy_lgart.
*      CLEAR itab_final_d1-yy_betrg.
      CLEAR itab_final_d1-yy_lgtxt.

      itab_final_d1-yy_pernr = itab_final_h-yy_pernr.
      itab_final_d1-yy_werks = itab_final_h-yy_werks.
      itab_final_d1-yy_btrtl = itab_final_h-yy_btrtl.
      itab_final_d1-yy_start_period = wa_period-begda.
      itab_final_d1-yy_end_period = wa_period-endda.
      itab_final_d1-yy_lgart = '9201'.
      itab_final_d1-yy_ben_type = 3.
      itab_final_d1-yy_lgtxt = 'COMPANY OWNED ACCOMODATION'.
      itab_final_d1-yy_ben_typ_txt = 'PERQUISITES'.
      APPEND itab_final_d1.
    ENDIF.

    CLEAR itab_final_d2.
    REFRESH itab_final_d2.

    SORT itab_final_d1 BY yy_pernr yy_lgart yy_apznr
  yy_werks yy_btrtl.
    IF itab_final_d1[] IS NOT INITIAL.
      LOOP AT itab_final_d1.
        itab_final_d2 = itab_final_d1.
        APPEND itab_final_d2.
      ENDLOOP.
    ENDIF.
    DELETE ADJACENT DUPLICATES FROM itab_final_d2
           COMPARING yy_pernr yy_lgart." yy_werks yy_btrtl.

    IF itab_final_d1[] IS NOT INITIAL AND itab_final_h[] IS NOT INITIAL.
      LOOP AT itab_final_d2 WHERE yy_pernr = itab_final_h-yy_pernr.
        LOOP AT itab_final_d1 WHERE yy_pernr = itab_final_d2-yy_pernr AND
                                    yy_lgart = itab_final_d2-yy_lgart AND yy_fpbeg EQ wa_period-begda.
          IF itab_final_d1-yy_apznr <> itab_final_d2-yy_apznr.
            IF itab_final_d1-yy_werks = itab_final_d2-yy_werks.
              IF itab_final_d1-yy_btrtl = itab_final_d2-yy_btrtl.
                itab_final_d2-yy_betrg = itab_final_d2-yy_betrg +
                                        itab_final_d1-yy_betrg.
                itab_final_d2-yy_end_period = itab_final_d1-yy_end_period.

                MODIFY itab_final_d2 TRANSPORTING yy_betrg yy_end_period.
              ELSE.

              ENDIF.
            ELSE.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF itab_final_d2[] IS NOT INITIAL AND itab_final_h[] IS NOT INITIAL.

    DESCRIBE TABLE itab_final_d2 LINES w_lines.

    LOOP AT itab_final_d2 WHERE yy_pernr = itab_p0000-pernr.
      IF sy-tabix <= w_lines.
        IF ( itab_final_d2-yy_start_period = itab_final_d2-yy_fpbeg AND
           itab_final_d2-yy_end_period = itab_final_d2-yy_fpend ) OR
           itab_final_d2-yy_lgart = '/3F3'.

          CLEAR w_betrg.
          w_betrg = itab_final_d2-yy_betrg.
          w_count_header = 0.
          LOOP AT itab_final_h WHERE yy_pernr = itab_final_d2-yy_pernr.
            IF itab_final_h-yy_start_period <> itab_final_h-yy_fpbeg OR
               itab_final_h-yy_end_period <> itab_final_h-yy_fpend.
              w_count_header = w_count_header + 1.
*            IF SY-TABIX = 1.
              IF w_count_header = 1.
                itab_final_d2-yy_start_period =
                                      itab_final_h-yy_start_period.
                itab_final_d2-yy_end_period = itab_final_h-yy_end_period.
                itab_final_d2-yy_werks = itab_final_h-yy_werks.
                itab_final_d2-yy_btrtl = itab_final_h-yy_btrtl.
                itab_final_d2-yy_betrg = w_betrg.
*                    ( w_betrg / itab_final_h-yy_py_prd_days ) *
*                      itab_final_h-yy_prd_days.
                MODIFY itab_final_d2 TRANSPORTING yy_start_period
                                                 yy_end_period
                                                 yy_werks
                                                 yy_btrtl
                                                 yy_betrg.
              ELSE.
                itab_final_d2-yy_start_period =
                                      itab_final_h-yy_start_period.
                itab_final_d2-yy_end_period = itab_final_h-yy_end_period.
                itab_final_d2-yy_werks = itab_final_h-yy_werks.
                itab_final_d2-yy_btrtl = itab_final_h-yy_btrtl.
                itab_final_d2-yy_betrg = w_betrg.
*                    ( w_betrg / itab_final_h-yy_py_prd_days ) *
*                      itab_final_h-yy_prd_days.
                IF itab_final_d2-yy_betrg <> 0.
                  APPEND itab_final_d2.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT itab_final_h BY yy_pernr yy_start_period.
  SORT itab_final_d2 BY yy_pernr yy_start_period yy_lgart.

  IF itab_final_d2[] IS NOT INITIAL.
    LOOP AT itab_final_d2.
      itab_final_d = itab_final_d2.
      APPEND itab_final_d.
    ENDLOOP.
  ENDIF.

  CLEAR w_da.
  CLEAR w_basic.
  CLEAR: w_pp, w_da_rate, w_pp_rate, w_amt_a, w_amt_b.
  LOOP AT itab_final_d WHERE yy_pernr = itab_final_h-yy_pernr AND yy_fpbeg EQ wa_period-begda.
    IF itab_final_d-yy_lgart = '1000'.
      w_basic = w_basic + itab_final_d-yy_betrg.
    ENDIF.
    IF itab_final_d-yy_lgart = '1005'.
      w_da = w_da + itab_final_d-yy_betrg.
    ELSEIF itab_final_d-yy_lgart = '1008'.
      w_da = w_da + itab_final_d-yy_betrg.
    ENDIF.

    IF itab_final_d-yy_lgart = '1021' OR itab_final_d-yy_lgart = '1022'.
      w_pp = w_pp + itab_final_d-yy_betrg.
    ENDIF.
  ENDLOOP.


  DATA: l_leaves    TYPE abwtg,
        l_basic_day TYPE maxbt,
        l_da_day    TYPE maxbt,
        l_pp_day    TYPE maxbt,
        l_ded_hpl   TYPE maxbt.

  IF w_basic_rate IS NOT INITIAL.
    w_amt_a = w_basic / w_basic_rate .  " factor for basic deduction
*    w_da_rate = w_da / w_amt_a.
    w_pp_rate = w_pp / w_amt_a.

    LOOP AT i_t539j2 INTO wa_t539j2 WHERE begda LE wa_period-endda AND endda GE wa_period-endda.
      IF wa_t539j2-baslg EQ '1000'.
        w_da_rate = w_da_rate + ( w_basic_rate * wa_t539j2-gwcht ) / 100.
      ELSEIF wa_t539j2-baslg EQ '1021'.
        w_da_rate = w_da_rate + ( w_pp_rate * wa_t539j2-gwcht ) / 100.
      ELSEIF wa_t539j2-baslg EQ '1022'.
        w_da_rate = w_da_rate + ( w_pp_rate * wa_t539j2-gwcht ) / 100.
      ENDIF.
    ENDLOOP.

    " Fetch HPL Deduction
    CLEAR: l_basic_day, l_da_day, l_pp_day.
    l_basic_day = w_basic_rate / ( ( wa_period-endda - wa_period-begda ) + 1 ).
    l_da_day = w_da_rate / ( ( wa_period-endda - wa_period-begda ) + 1 ).
    l_pp_day = w_pp_rate / ( ( wa_period-endda - wa_period-begda ) + 1 ).

    CLEAR: w_basic_hpl, w_pp_hpl, w_da_hpl, l_leaves.
***  SOC by Tushar Goyal & Bibhu Ranjan Sahoo on 04/10/2021 4000004245, TG:CTC Report Modification
*    LOOP AT itab_p2001 WHERE subty EQ '03' AND begda LE wa_period-endda AND endda GE wa_period-begda.
*** SOC by Tushar Goyal 4000004249, TG:YRHS039-CTC REPORT LEAVE CONDITION
***    LOOP AT itab_p2001 WHERE pernr EQ itab_final_h-yy_pernr AND subty EQ '03' AND begda LE wa_period-endda AND endda GE wa_period-begda.
******  EOC by Tushar Goyal & Bibhu Ranjan Sahoo on 04/10/2021 4000004245, TG:CTC Report Modification
***      IF itab_p2001-begda LT wa_period-begda.
***        itab_p2001-begda = wa_period-begda.
***      ENDIF.
***
***      IF itab_p2001-endda GT wa_period-endda.
***        itab_p2001-endda = wa_period-endda.
***      ENDIF.
***
***      l_leaves = l_leaves + ( ( itab_p2001-endda - itab_p2001-begda ) + 1 ).
***    ENDLOOP.
    LOOP AT wa_result-inter-ab INTO wa_ab WHERE ( awart EQ '03' OR awart EQ '07' OR awart EQ '28' ).
      l_leaves = l_leaves + ( ( wa_ab-endda - wa_ab-begda ) + 1 ).
    ENDLOOP.
*** EOC by Tushar Goyal 4000004249, TG:YRHS039-CTC REPORT LEAVE CONDITION
    IF l_leaves IS NOT INITIAL.
      l_leaves = l_leaves / 2.
    ENDIF.

    CLEAR: l_ded_hpl.
    l_ded_hpl = l_leaves * l_basic_day.

    w_basic_hpl = w_basic + l_ded_hpl.

    CLEAR: l_ded_hpl.
    l_ded_hpl = l_leaves * l_da_day.

*    w_da_hpl = w_da + l_ded_hpl.
    w_da_hpl = w_da.   " DA doesn't get prorated in case of HPL.

    CLEAR: l_ded_hpl.
    l_ded_hpl = l_leaves * l_pp_day.

    w_pp_hpl = w_pp + l_ded_hpl.

  ENDIF.


ENDFORM.                    " READ_RT_DATA
*&---------------------------------------------------------------------*
*&      Form  INITIALIZE_WT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialize_wt.
  DATA: i_t512t  TYPE TABLE OF t512t,
        wa_t512t TYPE t512t.

  CLEAR:w_wagetypes.
  w_wagetypes-sign       = 'I'.
  w_wagetypes-option     = 'BT'.
  w_wagetypes-low        = '1000'.
  w_wagetypes-high       = '1999'.
  APPEND w_wagetypes.

  CLEAR:w_wagetypes.
  w_wagetypes-sign       = 'I'.
  w_wagetypes-option     = 'BT'.
  w_wagetypes-low        = '4000'.
  w_wagetypes-high       = '4399'.
  APPEND w_wagetypes.

  CLEAR:w_wagetypes.
  w_wagetypes-sign       = 'I'.
  w_wagetypes-option     = 'EQ'.
  w_wagetypes-low        = '9000'.    "Basic Rate
  APPEND w_wagetypes.
  w_wagetypes-low        = '9999'.    "IT PERK (EMPLOYER)
  APPEND w_wagetypes.
  w_wagetypes-low        = '2046'.    "HRR RECOVERY
  APPEND w_wagetypes.
  w_wagetypes-low        = '6145'.    "HRR RECOVERY
  APPEND w_wagetypes.

  w_wagetypes-low        = '6146'.    "HRR RECOVERY
  APPEND w_wagetypes.

  w_wagetypes-low        = '6205'.    "HRR RECOVERY
  APPEND w_wagetypes.

***  SOC by Tushar Goyal & Bibhu Ranjan Sahoo on 28/12/2021 4000004444, TG:YRHS039-Add LWF WT in CTC Report
  w_wagetypes-low        = '/3W2'.
  APPEND w_wagetypes.
***  EOC by Tushar Goyal & Bibhu Ranjan Sahoo on 28/12/2021 4000004444, TG:YRHS039-Add LWF WT in CTC Report


  CLEAR: w_wagetypes.
  w_wagetypes-sign       = 'E'.
  w_wagetypes-option     = 'EQ'.
  w_wagetypes-low        = '1290'.
  APPEND w_wagetypes.

  w_wagetypes-low        = '1031' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1040' .
  APPEND w_wagetypes.
*  w_wagetypes-low        = '1041' .
*  APPEND w_wagetypes.
*  w_wagetypes-low        = '1042' .
*  APPEND w_wagetypes.
*  w_wagetypes-low        = '1043' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1050' .
  APPEND w_wagetypes.
*  w_wagetypes-low        = '1051' .
*  APPEND w_wagetypes.
*  w_wagetypes-low        = '1052' .
*  APPEND w_wagetypes.
*  w_wagetypes-low        = '1053' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1055' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1070' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1115' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1116' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1117' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1264' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1301' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1310' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1376' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1377' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1378' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1379' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1380' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1381' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1382' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1383' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1385' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1386' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '1387' .
  APPEND w_wagetypes.
  w_wagetypes-low        = '/LPM' .
  APPEND w_wagetypes.

  w_wagetypes-low        = '1221'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '1222'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '1231'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '1261'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '1265'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '1266'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '1291' .
  APPEND w_wagetypes.


  w_wagetypes-low        = '1400'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '1401'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '1402'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '4011'.
  APPEND w_wagetypes.
  w_wagetypes-low        =  '4015'.
  APPEND w_wagetypes.
  w_wagetypes-low        =  '4016'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '4018'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '4035'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '4037'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '4038'.
  APPEND w_wagetypes.
  w_wagetypes-low        =  '4060'.
  APPEND w_wagetypes.
  w_wagetypes-low        =  '4095'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '4095'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '4235'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '4237'.
  APPEND w_wagetypes.
  w_wagetypes-low        = '4238'.
  APPEND w_wagetypes.

  CLEAR: i_t512t.
  SELECT * FROM t512t INTO TABLE i_t512t WHERE sprsl EQ 'E' AND molga EQ '40' AND lgart IN w_wagetypes.
  IF sy-subrc EQ 0.

  ENDIF.

  CLEAR: w_wagetypes[],w_wt_retro[] .

  LOOP AT i_t512t INTO wa_t512t.
    w_wagetypes-sign       = 'I'.
    w_wagetypes-option     = 'EQ'.
    w_wagetypes-low        = wa_t512t-lgart.
    APPEND w_wagetypes.

    w_wt_retro-sign       = 'I'.
    w_wt_retro-option     = 'EQ'.
    w_wt_retro-low        = wa_t512t-lgart.
    APPEND w_wt_retro.
  ENDLOOP.

ENDFORM.                    " INITIALIZE_WT
*&---------------------------------------------------------------------*
*&      Form  PRINT_CTC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_ctc .


READ TABLE ITAB_FINAL_D into data(wa_final_d) with key
yy_lgtxt = 'Food per deims' YY_BEN_TYP_TXT = 'Transfer/Travel Benefits'.
if sy-subrc = 0.
delete itab_final_d where yy_lgtxt = 'Food per deims' and YY_BEN_TYP_TXT = 'Transfer/Travel Benefits'.
read TABLE ITAB_TRAVEL into data(wa_travel1) index 1.
if sy-subrc = 0.
  wa_travel1-yy_sptxt = wa_final_d-YY_LGTXT.
  wa_travel1-YY_REC_AMOUNT = wa_final_d-YY_BETRG.
  append wa_travel1 to itab_travel.
endif.
endif.

loop at itab_final_h ASSIGNING FIELD-SYMBOL(<fs_h>).
  <fs_h>-YY_TOTAL_CTC_P1 = <fs_h>-YY_TOTAL_CTC.
  loop at itab_travel into data(wa_travel).
    <fs_h>-yy_total_ctc_p1 = <fs_h>-yy_total_ctc_p1 - wa_travel-yy_rec_amount.
    endloop.

  ENDLOOP.



  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'YRH_CTC'
    IMPORTING
      fm_name            = w_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION w_fm_name
    EXPORTING
      w_from_dt        = w_from_dt
      w_to_dt          = w_to_dt
      w_asign          = w_asign
      w_asg_dept       = w_asg_dept
      w_asg_loc        = w_asg_loc
      w_asg_desg       = w_asg_desg
*     CONTROL_PARAMETERS = W_CTRLOP
*     OUTPUT_OPTIONS   = W_COMPOP
*     USER_SETTINGS    = 'X'
**    IMPORTING
**      JOB_OUTPUT_INFO = W_RETURN
    TABLES
      itab_final_h     = itab_final_h
      itab_final_d     = itab_final_d
      itab_final_h2    = itab_final_h2
      itab_travel      = itab_travel
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " PRINT_CTC
*&---------------------------------------------------------------------*
*&      Form  SMARTFORM_TO_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM smartform_to_pdf .

*break-point.
  CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
    EXPORTING
      i_language    = v_language
      i_application = 'SAPDEFAULT'
    IMPORTING
      e_devtype     = v_e_devtype.

  st_output_options-tdprinter = v_e_devtype.
*st_output_options-tdprinter = 'locl'.
  st_control_parameters-no_dialog = 'X'.
  st_control_parameters-getotf = 'X'.

*.................GET SMARTFORM FUNCTION MODULE NAME.................*
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'YRH_CTC'
    IMPORTING
      fm_name            = w_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
* WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*...........................CALL SMARTFORM............................*
  CALL FUNCTION w_fm_name
    EXPORTING
      control_parameters   = st_control_parameters
      output_options       = st_output_options
      w_from_dt            = w_from_dt
      w_to_dt              = w_to_dt
      w_asign              = w_asign
      w_asg_dept           = w_asg_dept
      w_asg_loc            = w_asg_loc
      w_asg_desg           = w_asg_desg
    IMPORTING
      document_output_info = st_document_output_info
      job_output_info      = st_job_output_info
      job_output_options   = st_job_output_options
    TABLES
      itab_final_h         = itab_final_h
      itab_final_d         = itab_final_d
      itab_final_h2        = itab_final_h2
      itab_travel          = itab_travel
    EXCEPTIONS
      formatting_error     = 1
      internal_error       = 2
      send_error           = 3
      user_canceled        = 4
      OTHERS               = 5.

  IF sy-subrc <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
*.........................CONVERT TO OTF TO PDF.......................*
    CALL FUNCTION 'CONVERT_OTF_2_PDF'
      IMPORTING
        bin_filesize           = v_bin_filesize
      TABLES
        otf                    = st_job_output_info-otfdata
        doctab_archive         = it_docs
        lines                  = it_lines
      EXCEPTIONS
        err_conv_not_possible  = 1
        err_otf_mc_noendmarker = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
* WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

*........................GET THE FILE NAME TO STORE....................*

    CONCATENATE 'CTC_' sy-datum sy-uzeit '.pdf' INTO v_name.
    CREATE OBJECT v_guiobj.
    CALL METHOD v_guiobj->file_save_dialog
      EXPORTING
        default_extension = 'pdf'
        default_file_name = v_name
        file_filter       = v_filter
      CHANGING
        filename          = v_name
        path              = v_path
        fullpath          = v_fullpath
        user_action       = v_uact.

    IF v_uact = v_guiobj->action_cancel.
      EXIT.
    ENDIF.
*..................................DOWNLOAD AS FILE....................*
    MOVE v_fullpath TO v_filename.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize            = v_bin_filesize
        filename                = v_filename
        filetype                = 'BIN'
      TABLES
        data_tab                = it_lines
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
* WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

ENDFORM.                    " SMARTFORM_TO_PDF
*&---------------------------------------------------------------------*
*&      Form  GET_LOAN_PERK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_loan_perk .

  DATA: wa_final_d LIKE yrhs_ctc_d.
*break-point.
  IF itab_p0045[] IS NOT INITIAL.

    LOOP AT itab_p0045." WHERE PERNR = ITAB_P0000-PERNR.
*    IF ITAB_P0045-SUBTY <> '9100' AND ITAB_P0045-SUBTY <> '9101' AND
*       ITAB_P0045-SUBTY <> '9101' AND ITAB_P0045-SUBTY <> '9102' AND
*       ITAB_P0045-SUBTY <> '9103' AND ITAB_P0045-SUBTY <> '9104' AND
*       ITAB_P0045-SUBTY <> '9110' AND ITAB_P0045-SUBTY <> '9111' AND
*       ITAB_P0045-SUBTY <> '9112' AND ITAB_P0045-SUBTY <> '9113' AND
*       ITAB_P0045-SUBTY <> '9140' AND ITAB_P0045-SUBTY <> '9141'.

      IF itab_p0045-subty CP '91++'.
      ELSE.
        DELETE itab_p0045.
      ENDIF.
    ENDLOOP.

    IF itab_p0045[] IS NOT INITIAL.
      CLEAR itab_p0078_1.
      REFRESH itab_p0078_1.
      LOOP AT itab_p0078 WHERE pernr = itab_p0000-pernr.
        wa_0078 = itab_p0078..
*      ITAB_P0078_1 = ITAB_P0078.
        APPEND wa_0078 TO itab_p0078_1.
      ENDLOOP.

*      LOOP AT itab_final_h WHERE yy_pernr = itab_p0000-pernr.
*        LOOP AT itab_p0045 WHERE pernr = itab_p0000-pernr.
*
*          CALL FUNCTION 'PCLO_BUILD_REPAYMENT_PLAN'
*            EXPORTING
*              ps_p0045         = itab_p0045
*              pf_fday          = itab_p0045-begda
*              pf_lday          = '99991231'
*              pt_paym          = itab_p0078_1
**             PF_RATE          =
**             PF_RATECURR      =
**             PF_TILBG         =
**             PF_CURR          =
**             PF_VARIANT       =
*              pf_nodisp        = 'Y'
*            IMPORTING
**             PS_PAPER         =
**             PF_DLEND         =
**             PF_EFFIN         =
*              pt_plan          = itab_loan_result
*            EXCEPTIONS
*              err_with_alv     = 1
*              err_curr_conv    = 2
*              no_table_entry   = 3
*              err_in_loan_calc = 4
*              OTHERS           = 5.
*          IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*          ENDIF.
*
*          IF itab_loan_result[] IS NOT INITIAL.
*            CLEAR wa_result_2.
*            READ TABLE itab_loan_result INTO wa_result_2
*                               WITH KEY begda = itab_final_h-yy_fpbeg
*                                        endda = itab_final_h-yy_fpend.
*            IF wa_result_2 IS NOT INITIAL.
*              CLEAR itab_final_d-yy_sno.
*              CLEAR itab_final_d-yy_lgart.
*              CLEAR itab_final_d-yy_betrg.
*              CLEAR itab_final_d-yy_lgtxt.
*              CLEAR itab_final_d-yy_start_period.
*              CLEAR itab_final_d-yy_end_period.
*              CLEAR itab_final_d-yy_fpper.
*              CLEAR itab_final_d-yy_fpbeg.
*              CLEAR itab_final_d-yy_fpend.
*              CLEAR itab_final_d-yy_werks.
*              CLEAR itab_final_d-yy_btrtl.
*
*              itab_final_d-yy_start_period =
*                                      itab_final_h-yy_start_period.
*              itab_final_d-yy_end_period = itab_final_h-yy_end_period.
*              itab_final_d-yy_fpper = itab_final_h-yy_fpper.
*              itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
*              itab_final_d-yy_fpend = itab_final_h-yy_fpend.
*              itab_final_d-yy_werks = itab_final_h-yy_werks.
*              itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
*              itab_final_d-yy_lgart = itab_p0045-subty.
*              CLEAR itab_t591s.
*              READ TABLE itab_t591s WITH KEY subty = itab_p0045-subty.
*              IF itab_t591s IS NOT INITIAL.
*                CONCATENATE itab_t591s-stext '- INTEREST SUBSIDY'
*                      INTO itab_final_d-yy_lgtxt.
**                CONTINUE.
*              ENDIF.
*
*              CLEAR wa_final_d.
*              READ TABLE itab_final_d INTO wa_final_d WITH KEY
*                         yy_pernr = itab_final_h-yy_pernr
*                         yy_lgart = itab_p0045-subty
*                         yy_start_period = itab_final_h-yy_start_period
*                         yy_end_period = itab_final_h-yy_end_period.
*              IF wa_final_d IS NOT INITIAL.
*                itab_final_d-yy_betrg = wa_final_d-yy_betrg +
*                                        wa_result_2-lbm.
*
*                MODIFY itab_final_d TRANSPORTING yy_betrg
*                 WHERE yy_pernr = itab_final_h-yy_pernr AND
*                       yy_lgart = itab_final_d-yy_lgart AND
*                       yy_start_period = itab_final_h-yy_start_period AND
*                       yy_end_period = itab_final_h-yy_end_period.
*              ELSE.
*                itab_final_d-yy_betrg = wa_result_2-lbm.
*                itab_final_d-yy_ben_type = 2.
*                itab_final_d-yy_ben_typ_txt =
*                                      'ALLOWANCES AND PERQUISITES'.
*
*                IF itab_final_d-yy_betrg > 0.
*                  APPEND itab_final_d.
*                ENDIF.
*
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDLOOP.
*      ENDLOOP.
    ENDIF.
  ENDIF.

  LOOP AT itab_final_h WHERE yy_pernr = itab_p0000-pernr.
    LOOP AT itab_final_d WHERE
                    yy_pernr = itab_p0000-pernr AND
                    yy_start_period = itab_final_h-yy_start_period AND
                    yy_end_period = itab_final_h-yy_end_period.
      CLEAR itab_p0045.
      READ TABLE itab_p0045 WITH KEY
                            pernr = itab_final_d-yy_pernr
                            subty = itab_final_d-yy_lgart.
      IF itab_p0045 IS NOT INITIAL.
        itab_final_d-yy_betrg =
              ( itab_final_d-yy_betrg / itab_final_h-yy_py_prd_days ) *
                itab_final_h-yy_prd_days.
        MODIFY itab_final_d TRANSPORTING yy_betrg
                 WHERE yy_pernr = itab_final_h-yy_pernr AND
                       yy_lgart = itab_final_d-yy_lgart AND
                       yy_start_period = itab_final_h-yy_start_period AND
                       yy_end_period = itab_final_h-yy_end_period.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " GET_LOAN_PERK
*&---------------------------------------------------------------------*
*&      Form  ADD_SELECTION_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_selection_data .
  DATA: i_ipd  TYPE TABLE OF yrha_hosp_cr_lt,
        wa_ipd TYPE yrha_hosp_cr_lt.

  " Check IPD expenses from table YRHA_HOSP_CR_LT
  CLEAR: i_ipd, wa_ipd.
  SELECT * FROM yrha_hosp_cr_lt INTO TABLE i_ipd WHERE emp_no EQ itab_p0000-pernr AND
   ( hosp_bill_dt BETWEEN wa_period-begda AND wa_period-endda ).

  LOOP AT itab_final_h WHERE yy_pernr = itab_p0000-pernr.

    IF p_med IS NOT INITIAL.
      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

      itab_final_d-yy_lgart = '9202'.
      itab_final_d-yy_betrg = p_med.

      IF i_ipd IS NOT INITIAL.
        LOOP AT i_ipd INTO wa_ipd.
          itab_final_d-yy_betrg = itab_final_d-yy_betrg + wa_ipd-amount_passed.
        ENDLOOP.
      ENDIF.

      itab_final_d-yy_lgtxt = 'MEDICAL EXPENSES (IPD)'.
      itab_final_d-yy_ben_type = 3.
      itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
      IF itab_final_d-yy_betrg <> 0.
        APPEND itab_final_d.
      ENDIF.

    ELSE.
      IF i_ipd IS NOT INITIAL.
        CLEAR itab_final_d-yy_sno.
        CLEAR itab_final_d-yy_lgart.
        CLEAR itab_final_d-yy_betrg.
        CLEAR itab_final_d-yy_lgtxt.
        CLEAR itab_final_d-yy_start_period.
        CLEAR itab_final_d-yy_end_period.
        CLEAR itab_final_d-yy_fpper.
        CLEAR itab_final_d-yy_fpbeg.
        CLEAR itab_final_d-yy_fpend.
        CLEAR itab_final_d-yy_werks.
        CLEAR itab_final_d-yy_btrtl.

        itab_final_d-yy_start_period = itab_final_h-yy_start_period.
        itab_final_d-yy_end_period = itab_final_h-yy_end_period.
        itab_final_d-yy_fpper = itab_final_h-yy_fpper.
        itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
        itab_final_d-yy_fpend = itab_final_h-yy_fpend.
        itab_final_d-yy_werks = itab_final_h-yy_werks.
        itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

        itab_final_d-yy_lgart = '9202'.

        LOOP AT i_ipd INTO wa_ipd.
          itab_final_d-yy_betrg = itab_final_d-yy_betrg + wa_ipd-amount_passed.
        ENDLOOP.

        itab_final_d-yy_lgtxt = 'MEDICAL EXPENSES (IPD)'.
        itab_final_d-yy_ben_type = 3.
        itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
        IF itab_final_d-yy_betrg <> 0.
          APPEND itab_final_d.
        ENDIF.
      ENDIF.
    ENDIF.

    IF p_tran IS NOT INITIAL.
      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

      itab_final_d-yy_lgart = '9106'.
      itab_final_d-yy_betrg = p_tran.
      itab_final_d-yy_lgtxt = 'TRANSFER EXPENSES'.
      itab_final_d-yy_ben_type = 3.
      itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
      IF itab_final_d-yy_betrg <> 0.
        APPEND itab_final_d.
      ENDIF.
    ENDIF.

    IF p_elec IS NOT INITIAL.
      READ TABLE itab_final_d WITH KEY yy_pernr = itab_final_h-yy_pernr yy_start_period = itab_final_h-yy_start_period  yy_lgart = '4000'.
      IF sy-subrc EQ 0.
        itab_final_d-yy_betrg = p_elec + itab_final_d-yy_betrg.
        MODIFY itab_final_d TRANSPORTING yy_betrg WHERE yy_pernr = itab_final_h-yy_pernr
         AND yy_start_period = itab_final_h-yy_start_period AND yy_lgart = '4000'.
      ELSE.
        CLEAR itab_final_d-yy_sno.
        CLEAR itab_final_d-yy_lgart.
        CLEAR itab_final_d-yy_betrg.
        CLEAR itab_final_d-yy_lgtxt.
        CLEAR itab_final_d-yy_start_period.
        CLEAR itab_final_d-yy_end_period.
        CLEAR itab_final_d-yy_fpper.
        CLEAR itab_final_d-yy_fpbeg.
        CLEAR itab_final_d-yy_fpend.
        CLEAR itab_final_d-yy_werks.
        CLEAR itab_final_d-yy_btrtl.

        itab_final_d-yy_start_period = itab_final_h-yy_start_period.
        itab_final_d-yy_end_period = itab_final_h-yy_end_period.
        itab_final_d-yy_fpper = itab_final_h-yy_fpper.
        itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
        itab_final_d-yy_fpend = itab_final_h-yy_fpend.
        itab_final_d-yy_werks = itab_final_h-yy_werks.
        itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

        itab_final_d-yy_lgart = '9107'.
        itab_final_d-yy_betrg = p_elec.
        itab_final_d-yy_lgtxt = 'Electricity charges reimbursement'.
        itab_final_d-yy_ben_type = 3.
        itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
        IF itab_final_d-yy_betrg <> 0.
          APPEND itab_final_d.
        ENDIF.
      ENDIF.
    ENDIF.

    IF p_scf IS NOT INITIAL.
      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

      itab_final_d-yy_lgart = '9018'.
*      IF itab_final_d-yy_start_period = itab_final_d-yy_fpbeg AND
*         itab_final_d-yy_end_period = itab_final_d-yy_fpend.
      itab_final_d-yy_betrg = p_scf.
*      ELSEIF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
*             itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
*        itab_final_d-yy_betrg =
*             ( p_scf / itab_final_h-yy_py_prd_days ) *
*               itab_final_h-yy_prd_days.
*      ENDIF.

      itab_final_d-yy_lgtxt = 'STAFF CAR FACILITY'.
      itab_final_d-yy_ben_type = 3.
      itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
      IF itab_final_d-yy_betrg <> 0.
        APPEND itab_final_d.
      ENDIF.
    ENDIF.
***---------------------------------------------------------------------------------------------------------------***
*    IF p_drvwg IS NOT INITIAL.
*      CLEAR itab_final_d-yy_sno.
*      CLEAR itab_final_d-yy_lgart.
*      CLEAR itab_final_d-yy_betrg.
*      CLEAR itab_final_d-yy_lgtxt.
*      CLEAR itab_final_d-yy_start_period.
*      CLEAR itab_final_d-yy_end_period.
*      CLEAR itab_final_d-yy_fpper.
*      CLEAR itab_final_d-yy_fpbeg.
*      CLEAR itab_final_d-yy_fpend.
*      CLEAR itab_final_d-yy_werks.
*      CLEAR itab_final_d-yy_btrtl.
*
*      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
*      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
*      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
*      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
*      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
*      itab_final_d-yy_werks = itab_final_h-yy_werks.
*      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
*
*      itab_final_d-yy_lgart = '9020'.
****------------------ALREADY COMMENTED CODE-------------------------***
*
**      IF itab_final_d-yy_start_period = itab_final_d-yy_fpbeg AND
**         itab_final_d-yy_end_period = itab_final_d-yy_fpend.
****-----------------------------------------------------------------***
*      itab_final_d-yy_betrg = p_drvwg.
****------------------ALREADY COMMENTED CODE-------------------------***
**      ELSEIF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
**             itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
**        itab_final_d-yy_betrg =
**             ( p_drvwg / itab_final_h-yy_py_prd_days ) *
**               itab_final_h-yy_prd_days.
**      ENDIF.
****-----------------------------------------------------------------***
*      itab_final_d-yy_lgtxt = 'DRIVER WAGES'.
*      itab_final_d-yy_ben_type = 3.
*      itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
*      IF itab_final_d-yy_betrg <> 0.
*        APPEND itab_final_d.
*      ENDIF.
*      ENDIF.
***------------------------------------------------------------------------------------------------------------------***

    "------------------------------------------------------------------------------------------------------------------
    "Updated Logic for Calculating driver wages based on grade and entitlement, if not provided in selection screen
    "SOC by Bhaiyasaheb Chaudhari (AB_BSAHEB) on 08/01/2026
    "CTC Report – Enhance Driver Wages Calculation Based on Grade & Period
    "------------------------------------------------------------------------------------------------------------------

    DATA: lv_total_amt      TYPE p DECIMALS 2 VALUE 0, "Final driver wages amount for the employee
          lv_e8_monthly_amt TYPE p DECIMALS 2 VALUE 0, "Monthly driver wages entitlement for E8
          lv_e9_monthly_amt TYPE p DECIMALS 2 VALUE 0, "Monthly driver wages entitlement for E9
*new declaration for driver wages - priya tiwari**********************
          lv_monthly_amt    TYPE yrha_ctc_driver-value,
**********************************************************************
          lv_months         TYPE i,                    "Number of months employee is eligible
          lv_overlap_from   TYPE datum,                "Start date of overlapping period
          lv_overlap_to     TYPE datum.                "End date of overlapping period

    DATA: ls_p0001  TYPE p0001,
          ls_t7ina9 TYPE t7ina9.

    "Driver wages provided in selection screen
    IF p_drvwg IS NOT INITIAL.


      CLEAR: itab_final_d-yy_sno,
             itab_final_d-yy_lgart,
             itab_final_d-yy_betrg,
             itab_final_d-yy_lgtxt,
             itab_final_d-yy_start_period,
             itab_final_d-yy_end_period,
             itab_final_d-yy_fpper,
             itab_final_d-yy_fpbeg,
             itab_final_d-yy_fpend,
             itab_final_d-yy_werks,
             itab_final_d-yy_btrtl.

      "Copy header period and org data
      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period   = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper        = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg        = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend        = itab_final_h-yy_fpend.
      itab_final_d-yy_werks        = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl        = itab_final_h-yy_btrtl.

      "Set driver wages from selection screen
      itab_final_d-yy_lgart        = '9020'.
      itab_final_d-yy_betrg        = p_drvwg.
      itab_final_d-yy_lgtxt        = 'DRIVER WAGES'.
      itab_final_d-yy_ben_type     = 3.
      itab_final_d-yy_ben_typ_txt  = 'PERQUISITES'.

      "Append only if non-zero value is provided in selection screen
      IF itab_final_d-yy_betrg <> 0.
        APPEND itab_final_d.
      ENDIF.

      "SOC: Also read non-driver types (Electricity, Entertainment) from YRHA_CTC_DRIVER table
      "When only Driver wage is provided in selection screen, other types should still come from table
      LOOP AT p0001 INTO ls_p0001
        WHERE ( persk = 'E8' OR persk = 'E9')
          AND begda <= l_to_end
          AND endda >= l_to_end.

        lv_overlap_from = ls_p0001-begda.
        IF lv_overlap_from < l_from_beg.
          lv_overlap_from = l_from_beg.
        ENDIF.
        lv_overlap_to = ls_p0001-endda.
        IF lv_overlap_to > l_to_end.
          lv_overlap_to = l_to_end.
        ENDIF.

        SELECT * INTO TABLE @DATA(lt_wages_oth) FROM yrha_ctc_driver
          WHERE grade = @ls_p0001-persk
            AND begda <= @lv_overlap_to
            AND endda >= @lv_overlap_from.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        LOOP AT lt_wages_oth INTO DATA(ls_wage_oth).
          "Skip driver wages type - already added from selection screen above
          IF ls_wage_oth-type CS 'DRI' OR ls_wage_oth-type CS 'DRIVER'.
            CONTINUE.
          ENDIF.

          CLEAR lv_total_amt.
          lv_monthly_amt = ls_wage_oth-value.
          lv_months =
            ( lv_overlap_to+0(4) * 12 + lv_overlap_to+4(2) ) -
            ( lv_overlap_from+0(4) * 12 + lv_overlap_from+4(2) ) + 1.
          lv_total_amt = lv_monthly_amt * lv_months.

          IF lv_total_amt > 0.
            CLEAR: itab_final_d-yy_sno,
                   itab_final_d-yy_lgart,
                   itab_final_d-yy_betrg,
                   itab_final_d-yy_lgtxt,
                   itab_final_d-yy_start_period,
                   itab_final_d-yy_end_period,
                   itab_final_d-yy_fpper,
                   itab_final_d-yy_fpbeg,
                   itab_final_d-yy_fpend,
                   itab_final_d-yy_werks,
                   itab_final_d-yy_btrtl.
            itab_final_d-yy_start_period = itab_final_h-yy_start_period.
            itab_final_d-yy_end_period   = itab_final_h-yy_end_period.
            itab_final_d-yy_fpper        = itab_final_h-yy_fpper.
            itab_final_d-yy_fpbeg        = itab_final_h-yy_fpbeg.
            itab_final_d-yy_fpend        = itab_final_h-yy_fpend.
            itab_final_d-yy_werks        = itab_final_h-yy_werks.
            itab_final_d-yy_btrtl        = itab_final_h-yy_btrtl.

            DATA(l_tabix_oth) = sy-tabix.
            itab_final_d-yy_lgart        = 9020 + l_tabix_oth - 1.
            itab_final_d-yy_betrg        = lv_total_amt.
            itab_final_d-yy_lgtxt        = ls_wage_oth-type.
            itab_final_d-yy_ben_type     = 3.
            itab_final_d-yy_ben_typ_txt  = 'PERQUISITES'.
            APPEND itab_final_d.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
      "EOC: Non-driver types from YRHA_CTC_DRIVER

      "Driver wages not provided in selection screen, calculating based on grade and entitlement
    ELSE.

      CLEAR: lv_total_amt,
             lv_e8_monthly_amt,
             lv_e9_monthly_amt.

      " Loop employee grade history for E8 / E9 overlap
      LOOP AT p0001 INTO ls_p0001
        WHERE ( persk = 'E8' OR persk = 'E9')
          AND begda <= l_to_end
          AND endda >= l_to_end.
*          AND endda >= l_from_beg.


        " Calculate overlap start date
        lv_overlap_from = ls_p0001-begda.
        IF lv_overlap_from < l_from_beg.
          lv_overlap_from = l_from_beg.
        ENDIF.

        " Calculate overlap end date
        lv_overlap_to = ls_p0001-endda.
        IF lv_overlap_to > l_to_end.
          lv_overlap_to = l_to_end.
        ENDIF.
******************calculate driver wages - priya tiwari***************************
        CLEAR: ls_wage_driver.
        SELECT * INTO TABLE @DATA(lt_wages) FROM yrha_ctc_driver WHERE grade = @ls_p0001-persk
          AND begda <= @lv_overlap_to
          AND endda >= @lv_overlap_from.

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
        LOOP AT lt_wages INTO DATA(ls_wage).
          data(l_tabix) = sy-tabix.
          clear lv_total_amt.
*          IF ls_wage-type = 'DRI WAGES'.
            " Monthly amount from driver table
            lv_monthly_amt = ls_wage-value.

****************************************************************************************
            " Calculate number of months in overlap
            lv_months =
              ( lv_overlap_to+0(4) * 12 + lv_overlap_to+4(2) ) -
              ( lv_overlap_from+0(4) * 12 + lv_overlap_from+4(2) ) + 1.

******************************total amount-priya tiwari *****************************
            " Accumulate driver wages
            lv_total_amt = lv_total_amt + ( lv_monthly_amt * lv_months ).
*          ENDIF.


******************************************************************************


*        " Read entitlement for E8 once
*        IF ls_p0001-persk = 'E8' AND lv_e8_monthly_amt IS INITIAL.
*          READ TABLE gt_t7ina9 INTO ls_t7ina9
*            WITH KEY lgart = '4064'
*                     algrp = 'AE8'.
*          IF sy-subrc = 0.
*            lv_e8_monthly_amt = ls_t7ina9-amunt / 12.
*          ENDIF.
*
*          " Read entitlement for E9 once
*        ELSEIF ls_p0001-persk = 'E9' AND lv_e9_monthly_amt IS INITIAL.
*          READ TABLE gt_t7ina9 INTO ls_t7ina9
*            WITH KEY lgart = '4064'
*                     algrp = 'AE9'.
*          IF sy-subrc = 0.
*            lv_e9_monthly_amt = ls_t7ina9-amunt / 12.
*          ENDIF.
*        ENDIF.

*****************************priya tiwari**************************************
      " Append Driver Wages record
      IF lv_total_amt > 0.
***********************************************************************************
*        " Accumulate driver wages based on grade
*        IF ls_p0001-persk = 'E8' AND lv_e8_monthly_amt IS NOT INITIAL.
*          lv_total_amt = lv_total_amt + ( lv_e8_monthly_amt * lv_months ).
*        ELSEIF ls_p0001-persk = 'E9' AND lv_e9_monthly_amt IS NOT INITIAL.
*          lv_total_amt = lv_total_amt + ( lv_e9_monthly_amt * lv_months ).
*        ENDIF.

*      ENDLOOP.

*      IF lv_total_amt IS NOT INITIAL.

        CLEAR: itab_final_d-yy_sno,
               itab_final_d-yy_lgart,
               itab_final_d-yy_betrg,
               itab_final_d-yy_lgtxt,
               itab_final_d-yy_start_period,
               itab_final_d-yy_end_period,
               itab_final_d-yy_fpper,
               itab_final_d-yy_fpbeg,
               itab_final_d-yy_fpend,
               itab_final_d-yy_werks,
               itab_final_d-yy_btrtl.
        " Copy header period and org data
        itab_final_d-yy_start_period = itab_final_h-yy_start_period.
        itab_final_d-yy_end_period   = itab_final_h-yy_end_period.
        itab_final_d-yy_fpper        = itab_final_h-yy_fpper.
        itab_final_d-yy_fpbeg        = itab_final_h-yy_fpbeg.
        itab_final_d-yy_fpend        = itab_final_h-yy_fpend.
        itab_final_d-yy_werks        = itab_final_h-yy_werks.
        itab_final_d-yy_btrtl        = itab_final_h-yy_btrtl.

        " Set calculated driver wages
        itab_final_d-yy_lgart        = 9020 + l_tabix - 1. "Should be 4064?
        itab_final_d-yy_betrg        = lv_total_amt.
        itab_final_d-yy_lgtxt        = ls_wage-type."'DRIVER WAGES'.
        itab_final_d-yy_ben_type     = 3.
        itab_final_d-yy_ben_typ_txt  = 'PERQUISITES'.

        IF itab_final_d-yy_betrg <> 0.
          APPEND itab_final_d.
        ENDIF.
        ENDIF.
    ENDLOOP.
      ENDLOOP.
      ENDIF.


    "------------------------------------------------------------------------------------------------------------------
    "EOC by Bhaiyasaheb Chaudhari (AB_BSAHEB) on 08/01/2026
    "------------------------------------------------------------------------------------------------------------------

    IF p_qtf IS NOT INITIAL.
      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

      itab_final_d-yy_lgart = '9021'.
*      IF itab_final_d-yy_start_period = itab_final_d-yy_fpbeg AND
*         itab_final_d-yy_end_period = itab_final_d-yy_fpend.
      itab_final_d-yy_betrg = p_qtf.
*      ELSEIF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
*             itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
*        itab_final_d-yy_betrg =
*             ( p_qtf / itab_final_h-yy_py_prd_days ) *
*               itab_final_h-yy_prd_days.
*      ENDIF.

      itab_final_d-yy_lgtxt = 'QUARTERLY TRANSIT FACILITY'.
      itab_final_d-yy_ben_type = 3.
      itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
      IF itab_final_d-yy_betrg <> 0.
        APPEND itab_final_d.
      ENDIF.

    ENDIF.

**  By Tushar
    IF p_ins IS NOT INITIAL.
      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

      itab_final_d-yy_lgart = '9101'.
*      IF itab_final_d-yy_start_period = itab_final_d-yy_fpbeg AND
*         itab_final_d-yy_end_period = itab_final_d-yy_fpend.
      itab_final_d-yy_betrg = p_ins.
*      ELSEIF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
*             itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
*        itab_final_d-yy_betrg =
*             ( p_ins / itab_final_h-yy_py_prd_days ) *
*               itab_final_h-yy_prd_days.
*      ENDIF.

      itab_final_d-yy_lgtxt = 'GROUP INSURANCE (PERSONAL SCHEME)'.
      itab_final_d-yy_ben_type = 5.
      itab_final_d-yy_ben_typ_txt = 'SOCIAL SECURITY/INSURANCE'.
      IF itab_final_d-yy_betrg <> 0.
        APPEND itab_final_d.
      ENDIF.

    ENDIF.

    IF p_ins2 IS NOT INITIAL.
      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

      itab_final_d-yy_lgart = '9102'.
*      IF itab_final_d-yy_start_period = itab_final_d-yy_fpbeg AND
*         itab_final_d-yy_end_period = itab_final_d-yy_fpend.
      itab_final_d-yy_betrg = p_ins2.
*      ELSEIF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
*             itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
*        itab_final_d-yy_betrg =
*             ( p_ins2 / itab_final_h-yy_py_prd_days ) *
*               itab_final_h-yy_prd_days.
*      ENDIF.

      itab_final_d-yy_lgtxt = 'GROUP PERSONAL ACCIDENT INSURANCE SCHEME'.
      itab_final_d-yy_ben_type = 5.
      itab_final_d-yy_ben_typ_txt = 'SOCIAL SECURITY/INSURANCE'.
      IF itab_final_d-yy_betrg <> 0.
        APPEND itab_final_d.
      ENDIF.

    ENDIF.

    IF p_fin IS NOT INITIAL.
      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

      itab_final_d-yy_lgart = '9103'.
*      IF itab_final_d-yy_start_period = itab_final_d-yy_fpbeg AND
*         itab_final_d-yy_end_period = itab_final_d-yy_fpend.
      itab_final_d-yy_betrg = p_fin.
*      ELSEIF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
*             itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
*        itab_final_d-yy_betrg =
*             ( p_fin / itab_final_h-yy_py_prd_days ) *
*               itab_final_h-yy_prd_days.
*      ENDIF.

      itab_final_d-yy_lgtxt = 'FINANCIAL ASSISTANCE SCHEME'.
      itab_final_d-yy_ben_type = 5.
      itab_final_d-yy_ben_typ_txt = 'SOCIAL SECURITY/INSURANCE'.
      IF itab_final_d-yy_betrg <> 0.
        APPEND itab_final_d.
      ENDIF.

    ENDIF.

    IF p_edli IS NOT INITIAL.
      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

      itab_final_d-yy_lgart = '9104'.
*      IF itab_final_d-yy_start_period = itab_final_d-yy_fpbeg AND
*         itab_final_d-yy_end_period = itab_final_d-yy_fpend.
      itab_final_d-yy_betrg = p_edli.
*      ELSEIF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
*             itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
*        itab_final_d-yy_betrg =
*             ( p_edli / itab_final_h-yy_py_prd_days ) *
*               itab_final_h-yy_prd_days.
*      ENDIF.

      itab_final_d-yy_lgtxt = 'EDLI'.
      itab_final_d-yy_ben_type = 5.
      itab_final_d-yy_ben_typ_txt = 'SOCIAL SECURITY/INSURANCE'.
      IF itab_final_d-yy_betrg <> 0.
        APPEND itab_final_d.
      ENDIF.
    ENDIF.

    IF p_arr IS NOT INITIAL.
      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

      itab_final_d-yy_lgart = '9210'.
      itab_final_d-yy_betrg = p_arr.
      itab_final_d-yy_lgtxt = 'ARREARS FROM PREVIOUS MONTH(S)'.
      itab_final_d-yy_ben_type = 3.
      itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
      IF itab_final_d-yy_betrg <> 0.
        APPEND itab_final_d.
      ENDIF.
    ENDIF.

* SOC By RAVI/BIBHU ON 20.04.2022 CHARM:4000004733, YRHR001_CTC_REPORT_NEW- CTC
    IF p_mis1 IS NOT INITIAL.
      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

*      itab_final_d-yy_lgart = '9210'.
      itab_final_d-yy_betrg = p_mis1.
      IF p_mis IS NOT INITIAL.
        CONCATENATE 'MISCELLANEOUS EXPENSES' '(' p_mis ')' INTO itab_final_d-yy_lgtxt SEPARATED BY space.
      ELSE.
        itab_final_d-yy_lgtxt = 'MISCELLANEOUS EXPENSES'.
      ENDIF.
      itab_final_d-yy_ben_type = 3.
      itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
      IF itab_final_d-yy_betrg <> 0.
        APPEND itab_final_d.
      ENDIF.
    ENDIF.
* EOC By RAVI/BIBHU ON 20.04.2022 CHARM:4000004733, YRHR001_CTC_REPORT_NEW- CTC

  ENDLOOP.

ENDFORM.                    " ADD_SELECTION_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_OTHER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM   get_data_other .

  IF itab_p9109[] IS NOT INITIAL.
    LOOP AT itab_p9109.

    ENDLOOP.
  ENDIF.

  LOOP AT itab_final_h WHERE yy_pernr = itab_p0000-pernr.
    CLEAR itab_final_d-yy_sno.
    CLEAR itab_final_d-yy_lgart.
    CLEAR itab_final_d-yy_betrg.
    CLEAR itab_final_d-yy_lgtxt.
    CLEAR itab_final_d-yy_start_period.
    CLEAR itab_final_d-yy_end_period.
    CLEAR itab_final_d-yy_fpper.
    CLEAR itab_final_d-yy_fpbeg.
    CLEAR itab_final_d-yy_fpend.
    CLEAR itab_final_d-yy_werks.
    CLEAR itab_final_d-yy_btrtl.

    itab_final_d-yy_start_period = itab_final_h-yy_start_period.
    itab_final_d-yy_end_period = itab_final_h-yy_end_period.
    itab_final_d-yy_fpper = itab_final_h-yy_fpper.
    itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
    itab_final_d-yy_fpend = itab_final_h-yy_fpend.
    itab_final_d-yy_werks = itab_final_h-yy_werks.
    itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
    itab_final_d-yy_lgart = '9001'.

    itab_final_d-yy_betrg = ( ( w_basic + w_da + w_pp ) * 30 ) / 100.
    itab_final_d-yy_lgtxt = 'SUPERANNUATION BENEFITS'.
    itab_final_d-yy_ben_type = 5.
    itab_final_d-yy_ben_typ_txt = 'SOCIAL SECURITY/INSURANCE'.
    IF itab_final_d-yy_betrg <> 0.
      APPEND itab_final_d.
    ENDIF.

    CLEAR itab_final_d-yy_sno.
    CLEAR itab_final_d-yy_lgart.
    CLEAR itab_final_d-yy_betrg.
    CLEAR itab_final_d-yy_lgtxt.
    CLEAR itab_final_d-yy_start_period.
    CLEAR itab_final_d-yy_end_period.
    CLEAR itab_final_d-yy_fpper.
    CLEAR itab_final_d-yy_fpbeg.
    CLEAR itab_final_d-yy_fpend.
    CLEAR itab_final_d-yy_werks.
    CLEAR itab_final_d-yy_btrtl.

    itab_final_d-yy_start_period = itab_final_h-yy_start_period.
    itab_final_d-yy_end_period = itab_final_h-yy_end_period.
    itab_final_d-yy_fpper = itab_final_h-yy_fpper.
    itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
    itab_final_d-yy_fpend = itab_final_h-yy_fpend.
    itab_final_d-yy_werks = itab_final_h-yy_werks.
    itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
    itab_final_d-yy_lgart = '9105'.

*    itab_final_d-yy_betrg = ( ( w_basic_hpl + w_da_hpl + w_pp_hpl )  * 35 ) / 100 .
    itab_final_d-yy_betrg = ( ( w_basic_hpl + w_pp_hpl )  * 35 ) / 100 .
    itab_final_d-yy_lgtxt = 'CAFETERIA ALLOWANCES'.
    itab_final_d-yy_ben_type = 2.
    itab_final_d-yy_ben_typ_txt = 'CAFETERIA'.
    IF itab_final_d-yy_betrg <> 0.
      APPEND itab_final_d.
    ENDIF.

    CLEAR itab_final_d-yy_sno.
    CLEAR itab_final_d-yy_lgart.
    CLEAR itab_final_d-yy_betrg.
    CLEAR itab_final_d-yy_lgtxt.
    CLEAR itab_final_d-yy_start_period.
    CLEAR itab_final_d-yy_end_period.
    CLEAR itab_final_d-yy_fpper.
    CLEAR itab_final_d-yy_fpbeg.
    CLEAR itab_final_d-yy_fpend.
    CLEAR itab_final_d-yy_werks.
    CLEAR itab_final_d-yy_btrtl.

    itab_final_d-yy_start_period = itab_final_h-yy_start_period.
    itab_final_d-yy_end_period = itab_final_h-yy_end_period.
    itab_final_d-yy_fpper = itab_final_h-yy_fpper.
    itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
    itab_final_d-yy_fpend = itab_final_h-yy_fpend.
    itab_final_d-yy_werks = itab_final_h-yy_werks.
    itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

    itab_final_d-yy_lgart = '9003'.
    itab_final_d-yy_betrg = ( w_basic_rate + w_da_rate + w_pp_rate ) * '11.11' / 100.
    IF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
       itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
      itab_final_d-yy_betrg =
             ( itab_final_d-yy_betrg / itab_final_h-yy_py_prd_days ) *
               itab_final_h-yy_prd_days.
    ENDIF.
    itab_final_d-yy_lgtxt = 'LEAVE SALARY CONTRIBUTION'.
    itab_final_d-yy_ben_type = 6.
    itab_final_d-yy_ben_typ_txt = 'LEAVE SALARY'.
    IF itab_final_d-yy_betrg <> 0.
      APPEND itab_final_d.
    ENDIF.

    CLEAR itab_final_d-yy_sno.
    CLEAR itab_final_d-yy_lgart.
    CLEAR itab_final_d-yy_betrg.
    CLEAR itab_final_d-yy_lgtxt.
    CLEAR itab_final_d-yy_start_period.
    CLEAR itab_final_d-yy_end_period.
    CLEAR itab_final_d-yy_fpper.
    CLEAR itab_final_d-yy_fpbeg.
    CLEAR itab_final_d-yy_fpend.
    CLEAR itab_final_d-yy_werks.
    CLEAR itab_final_d-yy_btrtl.

*    itab_final_d-yy_start_period = itab_final_h-yy_start_period.
*    itab_final_d-yy_end_period = itab_final_h-yy_end_period.
*    itab_final_d-yy_fpper = itab_final_h-yy_fpper.
*    itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
*    itab_final_d-yy_fpend = itab_final_h-yy_fpend.
*    itab_final_d-yy_werks = itab_final_h-yy_werks.
*    itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
*
*    itab_final_d-yy_lgart = '9004'.
*    itab_final_d-yy_betrg = ( w_basic_rate + w_da_rate + w_pp_rate ) * '2.78' / 100.
*    IF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
*       itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
*      itab_final_d-yy_betrg =
*             ( itab_final_d-yy_betrg / itab_final_h-yy_py_prd_days ) *
*               itab_final_h-yy_prd_days.
*    ENDIF.
*
*    itab_final_d-yy_lgtxt = 'HALF PAY LEAVE'.
*    itab_final_d-yy_ben_type = 6.
*    itab_final_d-yy_ben_typ_txt = 'LEAVE SALARY'.
*    IF itab_final_d-yy_betrg <> 0.
*      APPEND itab_final_d.
*    ENDIF.

*******************************add transfer amount-Priya Tiwari**********************************
*data: lv_recAmount TYPE v_ptrv_srec-rec_amount.
*SELECT SINGLE REC_AMOUNT INTO lv_recamount FROM v_ptrv_srec WHERE pernr = itab_p0000-pernr ."AND
**                     reinr = itab_ptrv_head-reinr.

    CLEAR itab_ptrv_perio.
    REFRESH itab_ptrv_perio.
    CLEAR itab_ptrv_shdr.
    REFRESH itab_ptrv_shdr.
    CLEAR itab_ptrv_srec.
    REFRESH itab_ptrv_srec.
    CLEAR itab_ptrv_head.
    REFRESH itab_ptrv_head.

    SELECT * FROM ptrv_perio INTO CORRESPONDING FIELDS OF TABLE
             itab_ptrv_perio
             WHERE pernr = itab_p0000-pernr
                   AND abrec = '2' AND
                   accdt >= itab_final_h-yy_fpbeg AND
                   accdt <= itab_final_h-yy_fpend.
    IF itab_ptrv_perio[] IS NOT INITIAL.
      SELECT * FROM ftpt_req_advance INTO CORRESPONDING FIELDS OF TABLE
               itab_ftpt_req_advance FOR ALL ENTRIES IN itab_ptrv_perio
               WHERE pernr = itab_ptrv_perio-pernr AND
                     reinr = itab_ptrv_perio-reinr.

      IF itab_ftpt_req_advance[] IS NOT INITIAL.
        LOOP AT itab_ptrv_perio.
          CLEAR itab_ftpt_req_advance.
          READ TABLE itab_ftpt_req_advance WITH KEY
                                pernr = itab_ptrv_perio-pernr
                                reinr = itab_ptrv_perio-reinr.
          IF itab_ftpt_req_advance IS NOT INITIAL AND
             itab_ftpt_req_advance-datvs = itab_ptrv_perio-accdt.
            DELETE itab_ptrv_perio.
          ENDIF.
        ENDLOOP.
      ENDIF.



      IF itab_ptrv_perio[] IS NOT INITIAL.
        SELECT * FROM ptrv_head INTO CORRESPONDING FIELDS OF TABLE
                 itab_ptrv_head FOR ALL ENTRIES IN itab_ptrv_perio
                 WHERE pernr = itab_ptrv_perio-pernr AND
                       reinr = itab_ptrv_perio-reinr  AND
                       morei = '99' AND kztkt <> '' AND kztkt <> ' '.
      ENDIF.

      IF itab_ptrv_head[] IS NOT INITIAL.
        SELECT * FROM ptrv_shdr INTO CORRESPONDING FIELDS OF TABLE
                 itab_ptrv_shdr FOR ALL ENTRIES IN itab_ptrv_head
                 WHERE pernr = itab_ptrv_head-pernr AND
                       reinr = itab_ptrv_head-reinr.
      ENDIF.
      SELECT * FROM v_ptrv_srec INTO CORRESPONDING FIELDS OF TABLE
           itab_ptrv_srec FOR ALL ENTRIES IN itab_ptrv_head
           WHERE pernr = itab_ptrv_head-pernr AND
                 reinr = itab_ptrv_head-reinr.
    ENDIF.
*  LOOP at lt_recamount INTO DATA(ls_recamount).
********************************end ***********************************************************************
    DATA: wa_final_hotel LIKE itab_final_d.
    "-------------------- FIX STARTS HERE --------------------

*DATA: lv_travel_amt TYPE p DECIMALS 2,
*      lv_hotel_amt  TYPE p DECIMALS 2.
*
*CLEAR: lv_travel_amt, lv_hotel_amt.
*      CLEAR itab_final_d-yy_sno.
*    CLEAR itab_final_d-yy_lgart.
*    CLEAR itab_final_d-yy_betrg.
*    CLEAR itab_final_d-yy_lgtxt.
*    CLEAR itab_final_d-yy_start_period.
*    CLEAR itab_final_d-yy_end_period.
*    CLEAR itab_final_d-yy_fpper.
*    CLEAR itab_final_d-yy_fpbeg.
*    CLEAR itab_final_d-yy_fpend.
*    CLEAR itab_final_d-yy_werks.
*    CLEAR itab_final_d-yy_btrtl.
*
*    itab_final_d-yy_start_period = itab_final_h-yy_start_period.
*    itab_final_d-yy_end_period = itab_final_h-yy_end_period.
*    itab_final_d-yy_fpper = itab_final_h-yy_fpper.
*    itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
*    itab_final_d-yy_fpend = itab_final_h-yy_fpend.
*    itab_final_d-yy_werks = itab_final_h-yy_werks.
*    itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
*
*
*LOOP AT itab_ptrv_srec INTO DATA(wa_ptrv_srec).
*
*  IF wa_ptrv_srec-exp_type = 'AAHA'.
*    lv_travel_amt = lv_travel_amt + wa_ptrv_srec-rec_amount.
*  ENDIF.
*
*  IF wa_ptrv_srec-exp_type = 'BBA1'.
*    lv_hotel_amt = lv_hotel_amt + wa_ptrv_srec-rec_amount.
*  ENDIF.
*
*ENDLOOP.
*
*-------------------- AIR TRAVEL LINE --------------------
*
*IF lv_travel_amt <> 0.
*
*  CLEAR itab_final_d.
*
*  itab_final_d-yy_start_period = itab_final_h-yy_start_period.
*  itab_final_d-yy_end_period   = itab_final_h-yy_end_period.
*  itab_final_d-yy_fpper        = itab_final_h-yy_fpper.
*  itab_final_d-yy_fpbeg        = itab_final_h-yy_fpbeg.
*  itab_final_d-yy_fpend        = itab_final_h-yy_fpend.
*  itab_final_d-yy_werks        = itab_final_h-yy_werks.
*  itab_final_d-yy_btrtl        = itab_final_h-yy_btrtl.
*
*  itab_final_d-yy_lgart        = 'AAHA'.
*  itab_final_d-yy_betrg        = lv_travel_amt.
*  itab_final_d-yy_lgtxt        = 'AIR TRAVEL'.
*  itab_final_d-yy_ben_type     = 7.
*  itab_final_d-yy_ben_typ_txt = 'Transfer/Travel Benefits'.
*
*  APPEND itab_final_d.
*
*ENDIF.
*
*-------------------- HOTEL ACCOMODATION LINE --------------------
*
*IF lv_hotel_amt <> 0.
*
*  CLEAR itab_final_d.
*
*  itab_final_d-yy_start_period = itab_final_h-yy_start_period.
*  itab_final_d-yy_end_period   = itab_final_h-yy_end_period.
*  itab_final_d-yy_fpper        = itab_final_h-yy_fpper.
*  itab_final_d-yy_fpbeg        = itab_final_h-yy_fpbeg.
*  itab_final_d-yy_fpend        = itab_final_h-yy_fpend.
*  itab_final_d-yy_werks        = itab_final_h-yy_werks.
*  itab_final_d-yy_btrtl        = itab_final_h-yy_btrtl.
*
*  itab_final_d-yy_lgart        = 'BBA1'.
*  itab_final_d-yy_betrg        = lv_hotel_amt.
*  itab_final_d-yy_lgtxt        = 'HOTEL ACCOMODATION'.
*  itab_final_d-yy_ben_type     = 7.
*  itab_final_d-yy_ben_typ_txt = 'Transfer/Travel Benefits'.
*
*  APPEND itab_final_d.
*
*ENDIF.
*endif.


********end - Priya Tiwari******************************
    CLEAR itab_final_d-yy_sno.
    CLEAR itab_final_d-yy_lgart.
    CLEAR itab_final_d-yy_betrg.
    CLEAR itab_final_d-yy_lgtxt.
    CLEAR itab_final_d-yy_start_period.
    CLEAR itab_final_d-yy_end_period.
    CLEAR itab_final_d-yy_fpper.
    CLEAR itab_final_d-yy_fpbeg.
    CLEAR itab_final_d-yy_fpend.
    CLEAR itab_final_d-yy_werks.
    CLEAR itab_final_d-yy_btrtl.

    itab_final_d-yy_start_period = itab_final_h-yy_start_period.
    itab_final_d-yy_end_period = itab_final_h-yy_end_period.
    itab_final_d-yy_fpper = itab_final_h-yy_fpper.
    itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
    itab_final_d-yy_fpend = itab_final_h-yy_fpend.
    itab_final_d-yy_werks = itab_final_h-yy_werks.
    itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

    LOOP AT itab_ptrv_srec INTO DATA(wa_ptrv_srec).
*     IF wa_ptrv_srec-exp_type = 'AAHA' .
      itab_final_d-yy_lgart = wa_ptrv_srec-exp_type.
*    IF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
*       itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
      itab_final_d-yy_betrg =  wa_ptrv_srec-rec_amount.

*    ENDIF.
* IF itab_ptrv_srec IS NOT INITIAL.
*              itab_final_d-yy_lgtxt = itab_t706b5-sptxt.
*            ENDIF.
      LOOP AT itab_t706b5 INTO DATA(wa_t706b5)
           WHERE spkzl = wa_ptrv_srec-exp_type.
        IF sy-subrc = 0.
          itab_final_d-yy_lgtxt = wa_t706b5-sptxt.
        ENDIF.
      ENDLOOP.
*        itab_final_d-yy_lgtxt = itab_t706b5-sptxt.
      itab_final_d-yy_ben_type = 7.
      itab_final_d-yy_ben_typ_txt = 'Transfer/Travel Benefits'.
      IF itab_final_d-yy_betrg <> 0.
        APPEND itab_final_d.
      ENDIF.
*endloop.
      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

      itab_final_d-yy_lgart = wa_ptrv_srec-exp_type.
*      IF wa_ptrv_srec-exp_type = 'AAHA'.
*        itab_final_d-yy_lgart = wa_ptrv_srec-exp_type.
*    IF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
*       itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
*        itab_final_d-yy_betrg =  wa_ptrv_srec-rec_amount.

*     ENDIF.


*  if wa_ptrv_srec-exp_type = 'BBA1'.
*        itab_final_d-yy_lgtxt = 'HOTEL ACCOMODATION'.
      itab_final_d-yy_ben_type = 7.
      itab_final_d-yy_ben_typ_txt = 'Transfer/Travel Benefits'.
      IF itab_final_d-yy_betrg <> 0.
        APPEND itab_final_d.
      ENDIF.
    ENDLOOP.

*****************************************************************
*************Insurance Overhead - Component 24***************************
*   Calculation on same lines as Leave Salary Contribution
*   Pick percentage from YRHR_CTC_SC by passing end date
    CLEAR itab_final_d-yy_sno.
    CLEAR itab_final_d-yy_lgart.
    CLEAR itab_final_d-yy_betrg.
    CLEAR itab_final_d-yy_lgtxt.
    CLEAR itab_final_d-yy_start_period.
    CLEAR itab_final_d-yy_end_period.
    CLEAR itab_final_d-yy_fpper.
    CLEAR itab_final_d-yy_fpbeg.
    CLEAR itab_final_d-yy_fpend.
    CLEAR itab_final_d-yy_werks.
    CLEAR itab_final_d-yy_btrtl.

    itab_final_d-yy_start_period = itab_final_h-yy_start_period.
    itab_final_d-yy_end_period = itab_final_h-yy_end_period.
    itab_final_d-yy_fpper = itab_final_h-yy_fpper.
    itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
    itab_final_d-yy_fpend = itab_final_h-yy_fpend.
    itab_final_d-yy_werks = itab_final_h-yy_werks.
    itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

    itab_final_d-yy_lgart = '9024'.
    IF lv_flag = 'X'.
      READ TABLE lt_perc INTO ls_perc INDEX 1.
      itab_final_d-yy_betrg = ( w_basic_rate + w_da_rate + w_pp_rate ) * ls_perc-percentage / 100.
      IF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
         itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
        itab_final_d-yy_betrg =
               ( itab_final_d-yy_betrg / itab_final_h-yy_py_prd_days ) *
                 itab_final_h-yy_prd_days.
      ENDIF.
    ENDIF.
    itab_final_d-yy_lgtxt = 'INSURANCE OVERHEAD'.
    itab_final_d-yy_ben_type = 5.
    itab_final_d-yy_ben_typ_txt = 'SOCIAL SECURITY/INSURANCE'.
    IF itab_final_d-yy_betrg <> 0.
      APPEND itab_final_d.
    ENDIF.







************************************************ended- priya Tiwari************************************************
    IF itab_prp_per[] IS NOT INITIAL.
      CLEAR itab_prp_per.
      READ TABLE itab_prp_per WITH KEY yy_grade = itab_final_h-yy_grade.

      IF itab_prp_per IS NOT INITIAL.
        CLEAR itab_final_d-yy_sno.
        CLEAR itab_final_d-yy_lgart.
        CLEAR itab_final_d-yy_betrg.
        CLEAR itab_final_d-yy_lgtxt.
        CLEAR itab_final_d-yy_start_period.
        CLEAR itab_final_d-yy_end_period.
        CLEAR itab_final_d-yy_fpper.
        CLEAR itab_final_d-yy_fpbeg.
        CLEAR itab_final_d-yy_fpend.
        CLEAR itab_final_d-yy_werks.
        CLEAR itab_final_d-yy_btrtl.

        itab_final_d-yy_start_period = itab_final_h-yy_start_period.
        itab_final_d-yy_end_period = itab_final_h-yy_end_period.
        itab_final_d-yy_fpper = itab_final_h-yy_fpper.
        itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
        itab_final_d-yy_fpend = itab_final_h-yy_fpend.
        itab_final_d-yy_werks = itab_final_h-yy_werks.
        itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

        itab_final_d-yy_lgart = '9005'.
        itab_final_d-yy_betrg = w_basic * itab_prp_per-yy_percentage / 100
  .
*        IF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
*           itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
*          itab_final_d-yy_betrg =
*               ( itab_final_d-yy_betrg / itab_final_h-yy_py_prd_days ) *
*                 itab_final_h-yy_prd_days.
*        ENDIF.
        itab_final_d-yy_lgtxt = 'PRP'.
        itab_final_d-yy_ben_type = 4.
        itab_final_d-yy_ben_typ_txt = 'INCENTIVES'.
******************************************************************************************************************
        READ TABLE itab_p0001 WITH KEY pernr = itab_p0000-pernr .
        IF sy-subrc = 0 AND itab_p0001-yy_org = 'GGL'.
          itab_final_d-yy_betrg = 0.
        ENDIF.
******************************************************************************************************************
        IF itab_final_d-yy_betrg <> 0 OR itab_p0001-yy_org = 'GGL'.
          APPEND itab_final_d.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR itab_final_d-yy_sno.
    CLEAR itab_final_d-yy_lgart.
    CLEAR itab_final_d-yy_betrg.
    CLEAR itab_final_d-yy_lgtxt.
    CLEAR itab_final_d-yy_start_period.
    CLEAR itab_final_d-yy_end_period.
    CLEAR itab_final_d-yy_fpper.
    CLEAR itab_final_d-yy_fpbeg.
    CLEAR itab_final_d-yy_fpend.
    CLEAR itab_final_d-yy_werks.
    CLEAR itab_final_d-yy_btrtl.

    itab_final_d-yy_start_period = itab_final_h-yy_start_period.
    itab_final_d-yy_end_period = itab_final_h-yy_end_period.
    itab_final_d-yy_fpper = itab_final_h-yy_fpper.
    itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
    itab_final_d-yy_fpend = itab_final_h-yy_fpend.
    itab_final_d-yy_werks = itab_final_h-yy_werks.
    itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
    itab_final_d-yy_lgart = '9008'.
    IF itab_p0581[] IS NOT INITIAL.
      LOOP AT itab_p0581 WHERE pernr = itab_final_d-yy_pernr AND
                               accom = '5'.
        IF itab_p0581-begda <= itab_final_h-yy_start_period AND
           itab_p0581-endda >= itab_final_d-yy_end_period.

          itab_final_d-yy_betrg = itab_p0581-rtamt.
          IF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
             itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
            itab_final_d-yy_betrg =
               ( itab_final_d-yy_betrg / itab_final_h-yy_py_prd_days ) *
                 itab_final_h-yy_prd_days.
          ENDIF.
          itab_final_d-yy_lgtxt = 'LEASE'.
          itab_final_d-yy_ben_type = 3.
          itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
          IF itab_final_d-yy_betrg <> 0.
            APPEND itab_final_d.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

*    " Company Owned Accomodation ITAB_P0001-PERSK
*    LOOP AT itab_p0581 WHERE pernr = itab_final_d-yy_pernr AND begda LE wa_period-endda AND endda GE wa_period-begda AND accom = '9'.
*      IF itab_p0581-yy_city_cat EQ 'X'.
*        IF itab_p0001-persk+0(1) EQ 'S'.
*          clear: wa_t539j.
*          read TABLE i_t539j INTO wa_t539j with key bwlga = '1041' baslg .
*          IF sy-subrc EQ 0.
*
*          ENDIF.
*
*        ELSE.
*
*        ENDIF.
*
*      ELSEIF itab_p0581-yy_city_cat EQ 'Y'.
*
*
*      ELSE.    " Z
*
*
*      ENDIF.
*
*
*    ENDLOOP.

    PERFORM get_toll_data.

***SOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 06.05.2020, Charm No #4000001849
*Remarks: The below code has been commented because this is useless code because as
*the IT0015 WT 1230 is being filled from RT table. This block of code is failing
*because it doesnot have WT 1230 which is deleted earlier in program. Therefore it
*is useless.
*    PERFORM get_refreshment_data.
****OC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 06.05.2020, Charm No #4000001849

    PERFORM get_entertainment_data.
***SOC BY SANYOGITA & RIMJHIM ON 27.11.2013 TO RESOLVE DOUBLE ENTRY IN REPORT
    PERFORM get_token_reward.
***EOC BY SANYOGITA & RIMJHIM ON 27.11.2013 TO RESOLVE DOUBLE ENTRY IN REPORT
  ENDLOOP.

ENDFORM.                    " GET_DATA_OTHER
*&---------------------------------------------------------------------*
*&      Form  GET_PC_SCHEME_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_pc_scheme_data .

*  DATA: w_diff TYPE i.
*
*  CLEAR itab_final_d-yy_sno.
*  CLEAR itab_final_d-yy_lgart.
*  CLEAR itab_final_d-yy_betrg.
*  CLEAR itab_final_d-yy_lgtxt.
*  CLEAR itab_final_d-yy_start_period.
*  CLEAR itab_final_d-yy_end_period.
*  CLEAR itab_final_d-yy_fpper.
*  CLEAR itab_final_d-yy_fpbeg.
*  CLEAR itab_final_d-yy_fpend.
*  CLEAR itab_final_d-yy_werks.
*  CLEAR itab_final_d-yy_btrtl.
*
*  LOOP AT itab_p9109 WHERE pernr = itab_final_d-yy_pernr AND
*                           ( subty = '02' OR subty = '04' ).
*
*    DO 15 TIMES VARYING w_assamt FROM
*                      itab_p9109-yy_assamt01 NEXT itab_p9109-yy_assamt02
*                VARYING w_billdt FROM
*                      itab_p9109-yy_billdt01 NEXT itab_p9109-yy_billdt02.
*      IF w_billdt <> '00000000'.
*        w_diff = itab_final_h-yy_fpbeg - w_billdt.
*
*        IF w_diff <= 1461 AND w_diff > 0.                   "4 YEARS.
*          itab_final_d-yy_start_period = itab_final_h-yy_start_period.
*          itab_final_d-yy_end_period = itab_final_h-yy_end_period.
*          itab_final_d-yy_fpper = itab_final_h-yy_fpper.
*          itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
*          itab_final_d-yy_fpend = itab_final_h-yy_fpend.
*          itab_final_d-yy_werks = itab_final_h-yy_werks.
*          itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
*
*          itab_final_d-yy_lgart = '9013'.
*          itab_final_d-yy_betrg = itab_final_d-yy_betrg + w_assamt.
*          itab_final_d-yy_lgtxt = 'PC/PERIPHERALS AT RESIDENCE'.
*        ENDIF.
*      ENDIF.
*    ENDDO.
*  ENDLOOP.
*
*  IF itab_final_d-yy_betrg IS NOT INITIAL.
*    itab_final_d-yy_betrg = itab_final_d-yy_betrg / 48.
*    IF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
*       itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
*      itab_final_d-yy_betrg =
*             ( itab_final_d-yy_betrg / itab_final_h-yy_py_prd_days ) *
*               itab_final_h-yy_prd_days.
*    ENDIF.
*
*    itab_final_d-yy_ben_type = 3.
*    itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
*    IF itab_final_d-yy_betrg <> 0.
*      APPEND itab_final_d.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " GET_PC_SCHEME_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_FURN_OFF_RES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_furn_off_res .

*  DATA: w_diff TYPE i.
*
*  CLEAR itab_final_d-yy_sno.
*  CLEAR itab_final_d-yy_lgart.
*  CLEAR itab_final_d-yy_betrg.
*  CLEAR itab_final_d-yy_lgtxt.
*  CLEAR itab_final_d-yy_start_period.
*  CLEAR itab_final_d-yy_end_period.
*  CLEAR itab_final_d-yy_fpper.
*  CLEAR itab_final_d-yy_fpbeg.
*  CLEAR itab_final_d-yy_fpend.
*  CLEAR itab_final_d-yy_werks.
*  CLEAR itab_final_d-yy_btrtl.
*
*  LOOP AT itab_p9109 WHERE pernr = itab_final_d-yy_pernr AND subty = '03'.
*
*    DO 15 TIMES VARYING w_assamt FROM
*                      itab_p9109-yy_assamt01 NEXT itab_p9109-yy_assamt02
*                VARYING w_billdt FROM
*                      itab_p9109-yy_billdt01 NEXT itab_p9109-yy_billdt02.
*      IF w_billdt <> '00000000'.
*        w_diff = itab_final_h-yy_fpbeg - w_billdt.
*
*        IF w_diff <= 2557 AND w_diff > 0.                   "7 YEARS
*          itab_final_d-yy_start_period = itab_final_h-yy_start_period.
*          itab_final_d-yy_end_period = itab_final_h-yy_end_period.
*          itab_final_d-yy_fpper = itab_final_h-yy_fpper.
*          itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
*          itab_final_d-yy_fpend = itab_final_h-yy_fpend.
*          itab_final_d-yy_werks = itab_final_h-yy_werks.
*          itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
*
*          itab_final_d-yy_lgart = '9014'.
*          itab_final_d-yy_betrg = itab_final_d-yy_betrg + w_assamt.
*          itab_final_d-yy_lgtxt = 'FURNITURE ITEMS OFFICE AT RESIDENCE'.
*        ENDIF.
*      ENDIF.
*    ENDDO.
*  ENDLOOP.
*
*  IF itab_final_d-yy_betrg IS NOT INITIAL.
*    itab_final_d-yy_betrg = itab_final_d-yy_betrg / 84.
*    IF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
*       itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
*      itab_final_d-yy_betrg =
*             ( itab_final_d-yy_betrg / itab_final_h-yy_py_prd_days ) *
*               itab_final_h-yy_prd_days.
*    ENDIF.
*
*    itab_final_d-yy_ben_type = 3.
*    itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
*    IF itab_final_d-yy_betrg <> 0.
*      APPEND itab_final_d.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " GET_FURN_OFF_RES
*&---------------------------------------------------------------------*
*&      Form  GET_OFFCYCLE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_offcycle_data .

  DATA: w_diff          TYPE i,
        w_mth(2)        TYPE c,
        w_month_txt(10) TYPE c,
        w_year(4)       TYPE c.

  IF itab_p0267[] IS NOT INITIAL.
    LOOP AT itab_final_h WHERE yy_pernr = itab_p0000-pernr.
      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
      itab_final_d-yy_lgart = '9015'.
      itab_final_d-yy_lgtxt = 'MEDICAL REIMBURSEMENT'.
      LOOP AT itab_p0267 WHERE pernr = itab_final_h-yy_pernr AND
                             ( subty = '4080' OR subty = '4081' OR
                               subty = '4082' )." OR subty = '9ZMR' ).

        IF itab_p0267-begda >= itab_final_h-yy_start_period AND itab_p0267-begda <= itab_final_h-yy_end_period.
          itab_final_d-yy_betrg = itab_final_d-yy_betrg +  itab_p0267-betrg.
        ENDIF.
      ENDLOOP.

      IF itab_final_d-yy_betrg > 0.
        itab_final_d-yy_ben_type = 3.
        itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
        APPEND itab_final_d.
        CLEAR: itab_final_d-yy_betrg.
      ENDIF.

      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
      itab_final_d-yy_lgart = '4040'.
      CLEAR itab_t512t.
      READ TABLE itab_t512t WITH KEY lgart = '4040'.  "CREDIT CARD REMB.
      itab_final_d-yy_lgtxt = itab_t512t-lgtxt.
      LOOP AT itab_p0267 WHERE pernr = itab_final_h-yy_pernr AND
                     subty = '4040'.
        IF itab_p0267-begda >= itab_final_h-yy_start_period AND
        itab_p0267-begda <= itab_final_h-yy_end_period.

          itab_final_d-yy_betrg = itab_p0267-betrg.
          IF itab_final_d-yy_betrg > 0.
            itab_final_d-yy_ben_type = 3.
            itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
            APPEND itab_final_d.
            CLEAR: itab_final_d-yy_betrg.
          ENDIF.
        ENDIF.
      ENDLOOP.

      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
***PROFESSIONAL MEMBERSHIP REIMBURSEMENT
      LOOP AT itab_p0267 WHERE pernr = itab_final_h-yy_pernr AND
                     ( subty = '4090' OR subty = '4091' ).
        IF itab_p0267-begda >= itab_final_h-yy_start_period AND
        itab_p0267-begda <= itab_final_h-yy_end_period.
          itab_final_d-yy_lgart = itab_p0267-subty.
          CLEAR itab_t512t.
          READ TABLE itab_t512t WITH KEY lgart = itab_p0267-subty.
          itab_final_d-yy_lgtxt = itab_t512t-lgtxt.

          itab_final_d-yy_betrg = itab_p0267-betrg.
          IF itab_final_d-yy_betrg > 0.
            itab_final_d-yy_ben_type = 3.
            itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
            APPEND itab_final_d.
            CLEAR: itab_final_d-yy_betrg.
          ENDIF.
        ENDIF.
      ENDLOOP.

      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
***ENTERTAINMENT EXPENSE REIMBURSEMENT DOP 6.10C
      LOOP AT itab_p0267 WHERE pernr = itab_final_h-yy_pernr AND
                     ( subty = '4045' OR subty = '4047' ).
        IF itab_p0267-begda >= itab_final_h-yy_start_period AND
        itab_p0267-begda <= itab_final_h-yy_end_period.
          itab_final_d-yy_lgart = itab_p0267-subty.
          CLEAR itab_t512t.
          READ TABLE itab_t512t WITH KEY lgart = itab_p0267-subty.
          itab_final_d-yy_lgtxt = itab_t512t-lgtxt.

          itab_final_d-yy_betrg = itab_p0267-betrg.
          IF itab_final_d-yy_betrg > 0.
            itab_final_d-yy_ben_type = 3.
            itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
            APPEND itab_final_d.
            CLEAR: itab_final_d-yy_betrg.
          ENDIF.
        ENDIF.
      ENDLOOP.

      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
***BROKERAGE CHARGE REIMBURSEMENT
      LOOP AT itab_p0267 WHERE pernr = itab_final_h-yy_pernr AND
                     subty = '4030'.
        IF itab_p0267-begda >= itab_final_h-yy_start_period AND
        itab_p0267-begda <= itab_final_h-yy_end_period.
          itab_final_d-yy_lgart = itab_p0267-subty.
          CLEAR itab_t512t.
          READ TABLE itab_t512t WITH KEY lgart = itab_p0267-subty.
          itab_final_d-yy_lgtxt = itab_t512t-lgtxt.

          itab_final_d-yy_betrg = itab_p0267-betrg.
          IF itab_final_d-yy_betrg > 0.
            itab_final_d-yy_ben_type = 3.
            itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
            APPEND itab_final_d.
            CLEAR: itab_final_d-yy_betrg.
          ENDIF.
        ENDIF.
      ENDLOOP.

*      CLEAR itab_final_d-yy_sno.
*      CLEAR itab_final_d-yy_betrg.
*      CLEAR itab_final_d-yy_lgart.
*      CLEAR itab_final_d-yy_lgtxt.
*      CLEAR itab_final_d-yy_start_period.
*      CLEAR itab_final_d-yy_end_period.
*      CLEAR itab_final_d-yy_fpper.
*      CLEAR itab_final_d-yy_fpbeg.
*      CLEAR itab_final_d-yy_fpend.
*      CLEAR itab_final_d-yy_werks.
*      CLEAR itab_final_d-yy_btrtl.
*
*      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
*      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
*      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
*      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
*      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
*      itab_final_d-yy_werks = itab_final_h-yy_werks.
*      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
****SPECIAL REWARD - CEA
*      LOOP AT itab_p0267 WHERE pernr = itab_final_h-yy_pernr AND
*                               subty = '1235'.
*        IF itab_p0267-begda >= itab_final_h-yy_start_period AND
*           itab_p0267-begda <= itab_final_h-yy_end_period.
*          itab_final_d-yy_lgart = itab_p0267-subty.
*          CLEAR itab_t512t.
*          READ TABLE itab_t512t WITH KEY lgart = itab_p0267-subty.
*          itab_final_d-yy_lgtxt = itab_t512t-lgtxt.
*
*          itab_final_d-yy_betrg = itab_p0267-betrg.
*
*          IF itab_final_d-yy_betrg > 0.
*            itab_final_d-yy_ben_type = 2.
*            itab_final_d-yy_ben_typ_txt = 'ALLOWANCES AND PERQUISITES'.
*            APPEND itab_final_d.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.


      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
***JOURNEY FARE
      LOOP AT itab_p0267 WHERE pernr = itab_final_h-yy_pernr AND
                     subty = '4115'.
        IF itab_p0267-begda >= itab_final_h-yy_start_period AND
        itab_p0267-begda <= itab_final_h-yy_end_period.
          itab_final_d-yy_lgart = itab_p0267-subty.
          CLEAR itab_t512t.
          READ TABLE itab_t512t WITH KEY lgart = itab_p0267-subty.
          itab_final_d-yy_lgtxt = itab_t512t-lgtxt.

          itab_final_d-yy_betrg = itab_p0267-betrg.

          IF itab_final_d-yy_betrg > 0.
            itab_final_d-yy_ben_type = 3.
            itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
            APPEND itab_final_d.
            CLEAR: itab_final_d-yy_betrg.
          ENDIF.
        ENDIF.
      ENDLOOP.


      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
***ARTIFICIAL APPLIANCE FOR HP CHILDREN
      LOOP AT itab_p0267 WHERE pernr = itab_final_h-yy_pernr AND
                     subty = '4025'.
        IF itab_p0267-begda >= itab_final_h-yy_start_period AND
        itab_p0267-begda <= itab_final_h-yy_end_period.
          itab_final_d-yy_lgart = itab_p0267-subty.
          CLEAR itab_t512t.
          READ TABLE itab_t512t WITH KEY lgart = itab_p0267-subty.
          itab_final_d-yy_lgtxt = itab_t512t-lgtxt.

          itab_final_d-yy_betrg = itab_p0267-betrg.

          IF itab_final_d-yy_betrg > 0.
            itab_final_d-yy_ben_type = 3.
            itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
            APPEND itab_final_d.
            CLEAR: itab_final_d-yy_betrg.
          ENDIF.
        ENDIF.
      ENDLOOP.
      "SOC SAKSHI/BIBHU DATE - 08-08-2024 CHARM - 4000008592
      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

      LOOP AT itab_p0267 WHERE pernr = itab_final_h-yy_pernr AND
                    subty = '1038'.
        IF itab_p0267-begda >= itab_final_h-yy_start_period AND
          itab_p0267-begda <= itab_final_h-yy_end_period.
          itab_final_d-yy_lgart = itab_p0267-subty.
          CLEAR itab_t512t.
          READ TABLE itab_t512t WITH KEY lgart = itab_p0267-subty.
          itab_final_d-yy_lgtxt = itab_t512t-lgtxt.

          itab_final_d-yy_betrg = itab_p0267-betrg.

          IF itab_final_d-yy_betrg > 0.
            itab_final_d-yy_ben_type = 3.
            itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
            APPEND itab_final_d.
            CLEAR: itab_final_d-yy_betrg.
          ENDIF.
        ENDIF.

        "EOC SAKSHI/BIBHU DATE - 08-08-2024 CHARM - 4000008592

      ENDLOOP.

      "SOC ARPITA/GAURAV DATE - 05-08-2025 TR - DVRK9A1LAM
      CLEAR itab_final_d-yy_sno.
      CLEAR itab_final_d-yy_betrg.
      CLEAR itab_final_d-yy_lgart.
      CLEAR itab_final_d-yy_lgtxt.
      CLEAR itab_final_d-yy_start_period.
      CLEAR itab_final_d-yy_end_period.
      CLEAR itab_final_d-yy_fpper.
      CLEAR itab_final_d-yy_fpbeg.
      CLEAR itab_final_d-yy_fpend.
      CLEAR itab_final_d-yy_werks.
      CLEAR itab_final_d-yy_btrtl.

      itab_final_d-yy_start_period = itab_final_h-yy_start_period.
      itab_final_d-yy_end_period = itab_final_h-yy_end_period.
      itab_final_d-yy_fpper = itab_final_h-yy_fpper.
      itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
      itab_final_d-yy_fpend = itab_final_h-yy_fpend.
      itab_final_d-yy_werks = itab_final_h-yy_werks.
      itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.

*UNIFORM REIMBURSEMENT
      LOOP AT itab_p0267 WHERE pernr = itab_final_h-yy_pernr AND
                    subty = '4061'.
        IF itab_p0267-begda >= itab_final_h-yy_start_period AND
          itab_p0267-begda <= itab_final_h-yy_end_period.
          itab_final_d-yy_lgart = itab_p0267-subty.
          CLEAR itab_t512t.
          READ TABLE itab_t512t WITH KEY lgart = itab_p0267-subty.
          itab_final_d-yy_lgtxt = itab_t512t-lgtxt.

          itab_final_d-yy_betrg = itab_p0267-betrg.

          IF itab_final_d-yy_betrg > 0.
            itab_final_d-yy_ben_type = 3.
            itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
            APPEND itab_final_d.
            CLEAR: itab_final_d-yy_betrg.
          ENDIF.
        ENDIF.
      ENDLOOP.
      "EOC ARPITA/GAURAV DATE - 05-08-2025 TR - DVRK9A1LAM

    ENDLOOP.
  ENDIF.

ENDFORM.                    " GET_OFFCYCLE_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_TRAVEL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_travel_data .

  DATA: wa_trv LIKE i_trv.

  CLEAR itab_ptrv_perio.
  REFRESH itab_ptrv_perio.
  CLEAR itab_ptrv_shdr.
  REFRESH itab_ptrv_shdr.
  CLEAR itab_ptrv_srec.
  REFRESH itab_ptrv_srec.
  CLEAR itab_ptrv_head.
  REFRESH itab_ptrv_head.
  CLEAR itab_ftpt_req_advance.
  REFRESH itab_ftpt_req_advance.

  SELECT * FROM ptrv_perio INTO CORRESPONDING FIELDS OF TABLE
           itab_ptrv_perio
           WHERE pernr = itab_p0000-pernr
                 AND abrec = '2' AND
                 accdt >= itab_final_h-yy_fpbeg AND
                 accdt <= itab_final_h-yy_fpend.
*ITAB_FINAL_D-YY_BETRG =

  IF itab_ptrv_perio[] IS NOT INITIAL.
    SELECT * FROM ftpt_req_advance INTO CORRESPONDING FIELDS OF TABLE
             itab_ftpt_req_advance FOR ALL ENTRIES IN itab_ptrv_perio
             WHERE pernr = itab_ptrv_perio-pernr AND
                   reinr = itab_ptrv_perio-reinr.

    IF itab_ftpt_req_advance[] IS NOT INITIAL.
      LOOP AT itab_ptrv_perio.
        CLEAR itab_ftpt_req_advance.
        READ TABLE itab_ftpt_req_advance WITH KEY
                              pernr = itab_ptrv_perio-pernr
                              reinr = itab_ptrv_perio-reinr.
        IF itab_ftpt_req_advance IS NOT INITIAL AND
           itab_ftpt_req_advance-datvs = itab_ptrv_perio-accdt.
          DELETE itab_ptrv_perio.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF itab_ptrv_perio[] IS NOT INITIAL.
      SELECT * FROM ptrv_head INTO CORRESPONDING FIELDS OF TABLE
               itab_ptrv_head FOR ALL ENTRIES IN itab_ptrv_perio
               WHERE pernr = itab_ptrv_perio-pernr AND
                     reinr = itab_ptrv_perio-reinr AND
***SOC BY SANYOGITA ON 30.09.2010, FOR INCLUDING ALL TRAVEL DATA
*                   KZTKT = 'B'.
                     morei = '99' AND kztkt <> '' AND kztkt <> ' '.
*                  ( KZTKT = 'A' OR KZTKT = 'B' OR KZTKT = 'C' OR
*                    KZTKT = 'D' OR KZTKT = 'E' OR KZTKT = 'F' OR
*                    KZTKT = 'G' OR KZTKT = 'H' OR KZTKT = 'I' OR
*                    KZTKT = 'J' OR KZTKT = 'K' OR KZTKT = 'N' OR
*                    KZTKT = 'O' OR KZTKT = 'Q' OR KZTKT = 'R' ).
***EOC BY SANYOGITA ON 30.09.2010, FOR INCLUDING ALL TRAVEL DATA

      IF itab_ptrv_head[] IS NOT INITIAL.
        SELECT * FROM ptrv_shdr INTO CORRESPONDING FIELDS OF TABLE
                 itab_ptrv_shdr FOR ALL ENTRIES IN itab_ptrv_head
                 WHERE pernr = itab_ptrv_head-pernr AND
                       reinr = itab_ptrv_head-reinr.

        SELECT * FROM v_ptrv_srec INTO CORRESPONDING FIELDS OF TABLE
                 itab_ptrv_srec FOR ALL ENTRIES IN itab_ptrv_head
                 WHERE pernr = itab_ptrv_head-pernr AND
                       reinr = itab_ptrv_head-reinr.

        IF itab_ptrv_shdr[] IS NOT INITIAL AND
           itab_ptrv_srec[] IS NOT INITIAL.
          SORT itab_ptrv_perio BY pernr reinr DESCENDING.
          SORT itab_ptrv_shdr BY pernr reinr.
          SORT itab_ptrv_srec BY pernr reinr.
          CLEAR wa_trv_smry.
          wa_trv_smry-yy_pernr = itab_final_h-yy_pernr.
          wa_trv_smry-yy_fpbeg = itab_final_h-yy_fpbeg.
          wa_trv_smry-yy_fpend = itab_final_h-yy_fpend.
          wa_trv_smry-yy_bukrs = itab_final_h-yy_bukrs.
          wa_trv_smry-yy_gsber = itab_final_h-yy_gsber.
          wa_trv_smry-yy_werks = itab_final_h-yy_werks.
          wa_trv_smry-yy_btrtl = itab_final_h-yy_btrtl.
          wa_trv_smry-yy_name  = itab_final_h-yy_name.
          wa_trv_smry-yy_grade = itab_final_h-yy_grade.
          wa_trv_smry-yy_designation = itab_final_h-yy_designation.
          wa_trv_smry-yy_department = itab_final_h-yy_department.
          wa_trv_smry-yy_location = itab_final_h-yy_location.

          LOOP AT itab_ptrv_srec.
            itab_travel-yy_pernr = itab_ptrv_srec-pernr.
            itab_travel-yy_reinr = itab_ptrv_srec-reinr.
            itab_travel-yy_exp_type = itab_ptrv_srec-exp_type.

            CLEAR itab_t706b5.
            READ TABLE itab_t706b5 WITH KEY
                            spkzl = itab_ptrv_srec-exp_type.
            IF itab_ptrv_srec IS NOT INITIAL.
              itab_travel-yy_sptxt = itab_t706b5-sptxt.
            ENDIF.

            itab_travel-yy_rec_amount = itab_ptrv_srec-rec_amount.

***SOC BY SANYOGITA ON 01.10.2010 FOR ADDING REASON & DATE FOR TRIP
            CLEAR itab_ptrv_head.
            READ TABLE itab_ptrv_head WITH KEY pernr = itab_ptrv_srec-pernr
                                               reinr = itab_ptrv_srec-reinr.
            itab_travel-yy_trip_reason = itab_ptrv_head-kunde.
            itab_travel-yy_trip_location = itab_ptrv_head-zort1.
            itab_travel-yy_trip_begin = itab_ptrv_head-datv1.
            itab_travel-yy_trip_end = itab_ptrv_head-datb1.

            wa_trv_smry-yy_trip_reason = itab_ptrv_head-kunde.
            wa_trv_smry-yy_trip_location = itab_ptrv_head-zort1.
            wa_trv_smry-yy_trip_begin = itab_ptrv_head-datv1.
            wa_trv_smry-yy_trip_end = itab_ptrv_head-datb1.
***EOC BY SANYOGITA ON 01.10.2010 FOR ADDING REASON & DATE FOR TRIP

            APPEND itab_travel.
            CLEAR itab_travel.

*        WA_TRV_SMRY-YY_SNO = ITAB_FINAL_H-YY_SNO.
            wa_trv_smry-yy_total_trv = wa_trv_smry-yy_total_trv +
                                       itab_ptrv_srec-rec_amount.
          ENDLOOP.

          CLEAR wa_trv.
          READ TABLE itab_trv_smry INTO wa_trv
                     WITH KEY yy_pernr = wa_trv_smry-yy_pernr
                              yy_total_trv = wa_trv_smry-yy_total_trv
                              yy_fpbeg = wa_trv_smry-yy_fpbeg
                              yy_fpend = wa_trv_smry-yy_fpend.
          IF wa_trv IS INITIAL.
            APPEND wa_trv_smry TO itab_trv_smry.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_TRAVEL_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_ASG_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_asg_data .

  DATA: wa_p0001 LIKE pa0001.
** -> Begin of changes by of Aditi on 29.11.2024 17:30:09 for ATC
*  SELECT SINGLE * FROM pa0001 INTO wa_p0001 WHERE pernr = p_asign AND
*                                           endda = '99991231'.
  SELECT  * FROM pa0001 INTO wa_p0001 UP TO 1 ROWS WHERE pernr = p_asign AND
                                           endda = '99991231'  ORDER BY PRIMARY KEY.
  ENDSELECT.
** -> End of changes by of Aditi on 29.11.2024 17:30:13 for ATC
  w_asign = wa_p0001-ename.

  SELECT SINGLE yy_department INTO w_asg_dept
         FROM yrha_department
         WHERE yy_dept = wa_p0001-yy_dept.
** -> Begin of changes by of Aditi on 29.11.2024 17:26:14 for ATC
*      SELECT SINGLE mc_stext INTO w_asg_desg FROM hrp1000
*               WHERE otype = 'S' AND plvar = '01' AND
*                     langu = 'E' AND objid = wa_p0001-plans.
  SELECT mc_stext INTO w_asg_desg FROM hrp1000
           UP TO 1 ROWS
           WHERE otype = 'S' AND plvar = '01' AND
                 langu = 'E' AND objid = wa_p0001-plans
           ORDER BY PRIMARY KEY.
  ENDSELECT.
** -> End of changes by of Aditi on 29.11.2024 17:26:17 for ATC
  CLEAR itab_t001p.
  READ TABLE itab_t001p WITH KEY werks = wa_p0001-werks
                                 btrtl = wa_p0001-btrtl.
  IF itab_t001p IS NOT INITIAL.
    w_asg_loc = itab_t001p-btext.
  ENDIF.

ENDFORM.                    " GET_ASG_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_FURN_RES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_furn_res .

*  DATA: w_diff TYPE i.
*
*  CLEAR itab_final_d-yy_sno.
*  CLEAR itab_final_d-yy_lgart.
*  CLEAR itab_final_d-yy_betrg.
*  CLEAR itab_final_d-yy_lgtxt.
*  CLEAR itab_final_d-yy_start_period.
*  CLEAR itab_final_d-yy_end_period.
*  CLEAR itab_final_d-yy_fpper.
*  CLEAR itab_final_d-yy_fpbeg.
*  CLEAR itab_final_d-yy_fpend.
*  CLEAR itab_final_d-yy_werks.
*  CLEAR itab_final_d-yy_btrtl.
*
*  LOOP AT itab_p9109 WHERE pernr = itab_final_d-yy_pernr AND subty = '01'.
*
*    DO 15 TIMES VARYING w_assamt FROM
*                      itab_p9109-yy_assamt01 NEXT itab_p9109-yy_assamt02
*                VARYING w_billdt FROM
*                      itab_p9109-yy_billdt01 NEXT itab_p9109-yy_billdt02.
*      IF w_billdt <> '00000000'.
*        w_diff = itab_final_h-yy_fpbeg - w_billdt.
*
*        IF w_diff <= 2557 AND w_diff > 0.                   "7 YEARS
*          itab_final_d-yy_start_period = itab_final_h-yy_start_period.
*          itab_final_d-yy_end_period = itab_final_h-yy_end_period.
*          itab_final_d-yy_fpper = itab_final_h-yy_fpper.
*          itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
*          itab_final_d-yy_fpend = itab_final_h-yy_fpend.
*          itab_final_d-yy_werks = itab_final_h-yy_werks.
*          itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
*
*          itab_final_d-yy_lgart = '9007'.
*          itab_final_d-yy_betrg = itab_final_d-yy_betrg + w_assamt.
*          itab_final_d-yy_lgtxt = 'FURNISHING ITEMS AT RESIDENCE'.
*        ENDIF.
*      ENDIF.
*    ENDDO.
*  ENDLOOP.
*
*  IF itab_final_d-yy_betrg IS NOT INITIAL.
*    CLEAR w_furn_maint.
*    w_furn_maint = itab_final_d-yy_betrg.
*    itab_final_d-yy_betrg = itab_final_d-yy_betrg / 84.
*    IF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
*       itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
*      itab_final_d-yy_betrg =
*             ( itab_final_d-yy_betrg / itab_final_h-yy_py_prd_days ) *
*               itab_final_h-yy_prd_days.
*    ENDIF.
*
*    itab_final_d-yy_ben_type = 3.
*    itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
*    IF itab_final_d-yy_betrg <> 0.
*      APPEND itab_final_d.
*      PERFORM get_furn_res_maint.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " GET_FURN_RES
*&---------------------------------------------------------------------*
*&      Form  GET_FURN_RES_MAINT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_furn_res_maint .

*  CLEAR itab_final_d-yy_sno.
*  CLEAR itab_final_d-yy_lgart.
*  CLEAR itab_final_d-yy_betrg.
*  CLEAR itab_final_d-yy_lgtxt.
*  CLEAR itab_final_d-yy_start_period.
*  CLEAR itab_final_d-yy_end_period.
*  CLEAR itab_final_d-yy_fpper.
*  CLEAR itab_final_d-yy_fpbeg.
*  CLEAR itab_final_d-yy_fpend.
*  CLEAR itab_final_d-yy_werks.
*  CLEAR itab_final_d-yy_btrtl.
*
*  itab_final_d-yy_start_period = itab_final_h-yy_start_period.
*  itab_final_d-yy_end_period = itab_final_h-yy_end_period.
*  itab_final_d-yy_fpper = itab_final_h-yy_fpper.
*  itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
*  itab_final_d-yy_fpend = itab_final_h-yy_fpend.
*  itab_final_d-yy_werks = itab_final_h-yy_werks.
*  itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
*
*  itab_final_d-yy_lgart = '4235'.
*  itab_final_d-yy_betrg = ( w_furn_maint * 10 / 100 ) / 12.
*  IF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
*     itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
*    itab_final_d-yy_betrg =
*            ( itab_final_d-yy_betrg / itab_final_h-yy_py_prd_days ) *
*              itab_final_h-yy_prd_days.
*  ENDIF.
*
*  itab_final_d-yy_lgtxt = 'FURNITURE MAINTENANCE CHARGE'.
*  itab_final_d-yy_ben_type = 3.
*  itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
*  IF itab_final_d-yy_betrg <> 0.
*    APPEND itab_final_d.
*  ENDIF.

ENDFORM.                    " GET_FURN_RES_MAINT
*&---------------------------------------------------------------------*
*&      Form  GET_SPECT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_spect_data .
*BREAK-POINT.

  CLEAR itab_final_d-yy_sno.
  CLEAR itab_final_d-yy_betrg.
  CLEAR itab_final_d-yy_lgart.
  CLEAR itab_final_d-yy_lgtxt.
  CLEAR itab_final_d-yy_start_period.
  CLEAR itab_final_d-yy_end_period.
  CLEAR itab_final_d-yy_fpper.
  CLEAR itab_final_d-yy_fpbeg.
  CLEAR itab_final_d-yy_fpend.
  CLEAR itab_final_d-yy_werks.
  CLEAR itab_final_d-yy_btrtl.

  itab_final_d-yy_start_period = itab_final_h-yy_start_period.
  itab_final_d-yy_end_period = itab_final_h-yy_end_period.
  itab_final_d-yy_fpper = itab_final_h-yy_fpper.
  itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
  itab_final_d-yy_fpend = itab_final_h-yy_fpend.
  itab_final_d-yy_werks = itab_final_h-yy_werks.
  itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
  itab_final_d-yy_lgart = '9022'.

  IF itab_p0015[] IS NOT INITIAL.
    LOOP AT itab_p0015 WHERE pernr = itab_final_h-yy_pernr AND
                             subty = '4095' AND lgart = '4095' AND
***SOC BY SANYOGITA ON 05.04.2011 TO GET CLAIM DATA FROM CLAIMS CLUSTER
***                             BEGDA+0(4) = ITAB_FINAL_H-YY_FPPER+0(4).
                             begda >= itab_final_h-yy_fpbeg AND
                             begda <= itab_final_h-yy_fpend.
***                             BEGDA+0(4) = SY-DATUM+0(4).
***EOC BY SANYOGITA ON 05.04.2011 TO GET CLAIM DATA FROM CLAIMS CLUSTER

      IF itab_p0015-begda >= itab_final_h-yy_start_period AND
         itab_p0015-begda <= itab_final_h-yy_end_period.

        IF itab_p0015-begda <= '20110214'.
          CLEAR itab_spect.
          READ TABLE itab_spect WITH KEY yy_pernr = itab_final_h-yy_pernr
                                   yy_claim_number = itab_p0015-zuord+5(15)
                                   yy_claim_date = itab_p0015-begda
                                   yy_cmph_flag = 'P'.
***SOC BY SANYOGITA ON 05.04.2011 TO GET CLAIM DATA FROM CLAIMS CLUSTER
        ELSEIF itab_p0015-begda > '20110214'.
          CLEAR itab_headr.
          REFRESH itab_headr.
          CLEAR itab_haedc.
          REFRESH itab_haedc.
          DATA w_refnr TYPE pin_refnr.
          w_refnr = itab_p0015-zuord+0(13).
          CALL FUNCTION 'HRPBSINCLAIMS_CLS_READ'
            EXPORTING
              ifd_refnr = w_refnr
*             IFD_RFDAT = ' '
            IMPORTING
*             EST_TRANS = ITAB_TRANS
              est_headr = itab_headr
*             ETB_ADREP =
*             ETB_DEPDT =
*             ETB_RQRMK =
*             ETB_FDATA =
*             ETB_INFDT =
              est_haedc = itab_haedc
              etb_mlcnt = wa_mlcnt
*             EFD_REFNR =
*             EFD_ERRFG =
*             ETB_ERRTB =
            .
        ENDIF.

        IF itab_p0015-begda > '20110214'.
          IF wa_mlcnt IS NOT INITIAL.
            APPEND LINES OF wa_mlcnt TO itab_mlcnt.
            IF itab_mlcnt[] IS NOT INITIAL.
              LOOP AT itab_mlcnt.
                IF itab_mlcnt-cdt01 >= itab_final_h-yy_start_period AND
                   itab_mlcnt-cdt01 <= itab_final_h-yy_end_period.

                  CLEAR wa_final_d_9022.
                  READ TABLE itab_final_d INTO wa_final_d_9022 WITH KEY
                                    yy_pernr = itab_final_h-yy_pernr
                                    yy_lgart = '9022'.
                  IF wa_final_d_9022 IS NOT INITIAL.
                    wa_final_d_9022-yy_betrg = wa_final_d_9022-yy_betrg +
                                               itab_mlcnt-apamt.
                    MODIFY TABLE itab_final_d FROM wa_final_d_9022 TRANSPORTING
                                             yy_betrg.
                  ELSE.
                    itab_final_d-yy_lgart = '9022'.
                    itab_final_d-yy_lgtxt = 'SPECTACLES REIMBURSEMENT'.
                    itab_final_d-yy_betrg = itab_final_d-yy_betrg +
                                            itab_mlcnt-apamt.
                    IF itab_final_d-yy_betrg > 0.
                      APPEND itab_final_d.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ELSEIF itab_p0015-begda <= '20110214'.
***EOC BY SANYOGITA ON 05.04.2011 TO GET CLAIM DATA FROM CLAIMS CLUSTER
          IF itab_spect-yy_begda >= itab_final_h-yy_start_period AND
             itab_spect-yy_begda <= itab_final_h-yy_end_period.

            CLEAR wa_final_d_9022.
            READ TABLE itab_final_d INTO wa_final_d_9022 WITH KEY
                                  yy_pernr = itab_final_h-yy_pernr
                                  yy_lgart = '9022'.
            IF wa_final_d_9022 IS NOT INITIAL.
              wa_final_d_9022-yy_betrg = wa_final_d_9022-yy_betrg +
                                         itab_p0015-betrg.
              MODIFY itab_final_d FROM wa_final_d_9022 TRANSPORTING
                                       yy_betrg.
            ELSE.
              itab_final_d-yy_lgart = '9022'.
              itab_final_d-yy_lgtxt = 'SPECTACLES REIMBURSEMENT'.
              itab_final_d-yy_betrg = itab_final_d-yy_betrg +
                                      itab_p0015-betrg.
              IF itab_final_d-yy_betrg > 0.
                APPEND itab_final_d.
              ENDIF.
            ENDIF.

          ELSEIF itab_spect-yy_begda < itab_final_h-yy_start_period AND
                 itab_spect-yy_begda < itab_final_h-yy_end_period.

            CLEAR wa_final_d_9023.
            READ TABLE itab_final_d INTO wa_final_d_9023 WITH KEY
                                  yy_pernr = itab_final_h-yy_pernr
                                  yy_lgart = '9023'.
            IF wa_final_d_9023 IS NOT INITIAL.
              wa_final_d_9023-yy_betrg = wa_final_d_9023-yy_betrg +
                                         itab_p0015-betrg.
              MODIFY itab_final_d FROM wa_final_d_9023 TRANSPORTING
                                      yy_betrg.
            ELSE.
              itab_final_d-yy_lgart = '9023'.
              IF itab_spect-yy_begda+4(2) = '01'.
                itab_final_d-yy_lgtxt =
                                  'BF-SPECTACLES REIMBURSEMENT JANUARY'.
              ENDIF.
              IF itab_spect-yy_begda+4(2) = '02'.
                itab_final_d-yy_lgtxt =
                                  'BF-SPECTACLES REIMBURSEMENT FEBRUARY'.
              ENDIF.
              IF itab_spect-yy_begda+4(2) = '03'.
                itab_final_d-yy_lgtxt = 'BF-SPECTACLES REIMBURSEMENT MARCH'.
              ENDIF.
              IF itab_spect-yy_begda+4(2) = '04'.
                itab_final_d-yy_lgtxt = 'BF-SPECTACLES REIMBURSEMENT APRIL'.
              ENDIF.
              IF itab_spect-yy_begda+4(2) = '05'.
                itab_final_d-yy_lgtxt = 'BF-SPECTACLES REIMBURSEMENT MAY'.
              ENDIF.
              IF itab_spect-yy_begda+4(2) = '06'.
                itab_final_d-yy_lgtxt = 'BF-SPECTACLES REIMBURSEMENT JUNE'.
              ENDIF.
              IF itab_spect-yy_begda+4(2) = '07'.
                itab_final_d-yy_lgtxt = 'BF-SPECTACLES REIMBURSEMENT JULY'.
              ENDIF.
              IF itab_spect-yy_begda+4(2) = '08'.
                itab_final_d-yy_lgtxt =
                                  'BF-SPECTACLES REIMBURSEMENT AUGUST'.
              ENDIF.
              IF itab_spect-yy_begda+4(2) = '09'.
                itab_final_d-yy_lgtxt =
                                  'BF-SPECTACLES REIMBURSEMENT SEPTEMBER'.
              ENDIF.
              IF itab_spect-yy_begda+4(2) = '10'.
                itab_final_d-yy_lgtxt =
                                  'BF-SPECTACLES REIMBURSEMENT OCTOBER'.
              ENDIF.
              IF itab_spect-yy_begda+4(2) = '11'.
                itab_final_d-yy_lgtxt =
                                  'BF-SPECTACLES REIMBURSEMENT NOVEMBER'.
              ENDIF.
              IF itab_spect-yy_begda+4(2) = '12'.
                itab_final_d-yy_lgtxt =
                                  'BF-SPECTACLES REIMBURSEMENT DECEMBER'.
              ENDIF.

              itab_final_d-yy_betrg = itab_final_d-yy_betrg +
                                      itab_p0015-betrg.
              IF itab_final_d-yy_betrg > 0.
                APPEND itab_final_d.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GET_SPECT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_TOLL_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_toll_data .

  DATA: w_mth(2)        TYPE c,
        w_month_txt(10) TYPE c,
        w_year(4)       TYPE c.

  CLEAR itab_final_d-yy_sno.
  CLEAR itab_final_d-yy_betrg.
  CLEAR itab_final_d-yy_lgart.
  CLEAR itab_final_d-yy_lgtxt.
  CLEAR itab_final_d-yy_start_period.
  CLEAR itab_final_d-yy_end_period.
  CLEAR itab_final_d-yy_fpper.
  CLEAR itab_final_d-yy_fpbeg.
  CLEAR itab_final_d-yy_fpend.
  CLEAR itab_final_d-yy_werks.
  CLEAR itab_final_d-yy_btrtl.

  itab_final_d-yy_start_period = itab_final_h-yy_start_period.
  itab_final_d-yy_end_period = itab_final_h-yy_end_period.
  itab_final_d-yy_fpper = itab_final_h-yy_fpper.
  itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
  itab_final_d-yy_fpend = itab_final_h-yy_fpend.
  itab_final_d-yy_werks = itab_final_h-yy_werks.
  itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
  itab_final_d-yy_lgart = '9024'.

  IF itab_p0015[] IS NOT INITIAL.
    LOOP AT itab_p0015 WHERE pernr = itab_final_h-yy_pernr AND
                             subty = '4018' AND lgart = '4018'.

      IF itab_p0015-begda >= itab_final_d-yy_start_period AND
         itab_p0015-begda <= itab_final_d-yy_end_period.

        IF itab_p0015-begda >= '20110402'.
          CLEAR itab_headr.
          REFRESH itab_headr.
          CLEAR itab_haedc.
          REFRESH itab_haedc.
          DATA w_refnr TYPE pin_refnr.
          w_refnr = itab_p0015-zuord+0(13).
          CALL FUNCTION 'HRPBSINCLAIMS_CLS_READ'
            EXPORTING
              ifd_refnr = w_refnr
*             IFD_RFDAT = ' '
            IMPORTING
*             EST_TRANS = ITAB_TRANS
              est_headr = itab_headr
*             ETB_ADREP =
*             ETB_DEPDT =
*             ETB_RQRMK =
*             ETB_FDATA =
*             ETB_INFDT =
              est_haedc = itab_haedc
              etb_mlcnt = wa_mlcnt
*             EFD_REFNR =
*             EFD_ERRFG =
*             ETB_ERRTB =
            .
          IF wa_mlcnt IS NOT INITIAL.
            APPEND LINES OF wa_mlcnt TO itab_mlcnt.
            IF itab_mlcnt[] IS NOT INITIAL.
              LOOP AT itab_mlcnt.
                IF itab_mlcnt-cdt01 >= itab_final_h-yy_start_period AND
                   itab_mlcnt-cdt01 <= itab_final_h-yy_end_period.
                  CLEAR wa_final_d_9024.
                  READ TABLE itab_final_d INTO wa_final_d_9024 WITH KEY
                                    yy_pernr = itab_final_h-yy_pernr
                                    yy_lgart = '9024'.
                  IF wa_final_d_9024 IS NOT INITIAL.
                    wa_final_d_9024-yy_betrg = wa_final_d_9024-yy_betrg +
                                               itab_mlcnt-apamt.
                    MODIFY TABLE itab_final_d FROM wa_final_d_9024 TRANSPORTING
                                             yy_betrg.
                  ELSE.
                    itab_final_d-yy_lgart = '9024'.
                    w_year = itab_mlcnt-cdt01+0(4).
                    w_mth = itab_mlcnt-cdt01+4(2).
                    CASE w_mth.
                      WHEN '01'.
                        w_month_txt = 'JANUARY'.
                      WHEN '02'.
                        w_month_txt = 'FEBRUARY'.
                      WHEN '03'.
                        w_month_txt = 'MARCH'.
                      WHEN '04'.
                        w_month_txt = 'APRIL'.
                      WHEN '05'.
                        w_month_txt = 'MAY'.
                      WHEN '06'.
                        w_month_txt = 'JUNE'.
                      WHEN '07'.
                        w_month_txt = 'JULY'.
                      WHEN '08'.
                        w_month_txt = 'AUGUST'.
                      WHEN '09'.
                        w_month_txt = 'SEPTEMBER'.
                      WHEN '10'.
                        w_month_txt = 'OCTOBER'.
                      WHEN '11'.
                        w_month_txt = 'NOVEMBER'.
                      WHEN '12'.
                        w_month_txt = 'DECEMBER'.
                    ENDCASE.
                    CONCATENATE 'TOLL CHARGE REIMB. - ' w_month_txt w_year
                                INTO itab_final_d-yy_lgtxt
                                SEPARATED BY space.
                    itab_final_d-yy_betrg = itab_final_d-yy_betrg +
                                            itab_mlcnt-apamt.
                    IF itab_final_d-yy_betrg > 0.
                      APPEND itab_final_d.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ELSEIF itab_p0015-begda < '20110402'.
          w_year = itab_p0015-zuord+15(4).
          w_mth = itab_p0015-zuord+12(2).
          CASE w_mth.
            WHEN '01'.
              w_month_txt = 'JANUARY'.
            WHEN '02'.
              w_month_txt = 'FEBRUARY'.
            WHEN '03'.
              w_month_txt = 'MARCH'.
            WHEN '04'.
              w_month_txt = 'APRIL'.
            WHEN '05'.
              w_month_txt = 'MAY'.
            WHEN '06'.
              w_month_txt = 'JUNE'.
            WHEN '07'.
              w_month_txt = 'JULY'.
            WHEN '08'.
              w_month_txt = 'AUGUST'.
            WHEN '09'.
              w_month_txt = 'SEPTEMBER'.
            WHEN '10'.
              w_month_txt = 'OCTOBER'.
            WHEN '11'.
              w_month_txt = 'NOVEMBER'.
            WHEN '12'.
              w_month_txt = 'DECEMBER'.
          ENDCASE.
          CONCATENATE 'TOLL CHARGE REIMB. - ' w_month_txt w_year
                      INTO itab_final_d-yy_lgtxt SEPARATED BY space.
          itab_final_d-yy_betrg = itab_p0015-betrg.
          IF itab_final_d-yy_betrg > 0.
            APPEND itab_final_d.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GET_TOLL_DATA
*&---------------------------------------------------------------------*
*&      Form  SUMMARY_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM summary_report .

*populate fields
  PERFORM fill_fieldcat USING i_fieldcat.

*heading
  PERFORM build_header USING i_top_of_page.

  PERFORM build_sort USING i_sort.

  i_layout-colwidth_optimize = 'X'.
  i_layout-f2code = w_f2code.
  i_layout-detail_popup = ' '.
  i_layout-detail_titlebar = 'DETAILS OF RECORD'.

  PERFORM disp_report.

ENDFORM.                    " SUMMARY_REPORT
*&---------------------------------------------------------------------*
*&      Form  APPEND_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM append_data .

  APPEND p0000 TO itab_p0000.
  APPEND p0001 TO itab_p0001.

  APPEND LINES OF p0045 TO itab_p0045.
  APPEND LINES OF p0078 TO itab_p0078.

  APPEND LINES OF p9109 TO itab_p9109.
  APPEND LINES OF p0590 TO itab_p0590.
  APPEND LINES OF p0581 TO itab_p0581.
  APPEND LINES OF p0267 TO itab_p0267.
  APPEND LINES OF p2001 TO itab_p2001.
  APPEND LINES OF p0015 TO itab_p0015.

  APPEND LINES OF p0001 TO itab_p0001_claim.
  APPEND LINES OF p0015 TO itab_p0015_claim.


ENDFORM.                    " APPEND_DATA
*&---------------------------------------------------------------------*
*&      Form  FILL_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_FIELDCAT  text
*----------------------------------------------------------------------*
FORM fill_fieldcat  USING    p_i_fieldcat TYPE slis_t_fieldcat_alv.

  DATA: wa_fieldcat TYPE slis_fieldcat_alv.

  IF p_sry = 'X' OR p_dbt = 'X'.
    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 1.
    wa_fieldcat-tabname  =  'ITAB_CTC_SMRY'.
    wa_fieldcat-fieldname = 'YY_SNO'.
    wa_fieldcat-seltext_m = 'SNo'.
    wa_fieldcat-key = 'X'.
    wa_fieldcat-key_sel = 'X'.
    wa_fieldcat-fix_column = 'X'.
    wa_fieldcat-input  = 'X'.
    wa_fieldcat-sp_group = 'A'.

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 2.
    wa_fieldcat-tabname  =  'ITAB_CTC_SMRY'.
    wa_fieldcat-fieldname = 'YY_PERNR'.
    wa_fieldcat-seltext_m = 'Personnel Number'(004).
    wa_fieldcat-key = 'X'.
    wa_fieldcat-key_sel = 'X'.
    wa_fieldcat-fix_column = 'X'.
    wa_fieldcat-input  = 'X'.
    wa_fieldcat-sp_group = 'A'.

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 3.
    wa_fieldcat-tabname  =  'ITAB_CTC_SMRY'.
    wa_fieldcat-fieldname = 'YY_NAME'.
    wa_fieldcat-seltext_m = 'Name of Employee'(005).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 4.
    wa_fieldcat-tabname  =  'ITAB_CTC_SMRY'.
    wa_fieldcat-fieldname = 'YY_GRADE'.
    wa_fieldcat-seltext_m = 'Grade'(006).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 5.
    wa_fieldcat-tabname  =  'ITAB_CTC_SMRY'.
    wa_fieldcat-fieldname = 'YY_DESIGNATION'.
    wa_fieldcat-seltext_m = 'Designation'(007).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 6.
    wa_fieldcat-tabname  =  'ITAB_CTC_SMRY'.
    wa_fieldcat-fieldname = 'YY_DEPARTMENT'.
    wa_fieldcat-seltext_m = 'Department'(008).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 7.
    wa_fieldcat-tabname  =  'ITAB_CTC_SMRY'.
    wa_fieldcat-fieldname = 'YY_LOCATION'.
    wa_fieldcat-seltext_m = 'Location'(009).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 8.
    wa_fieldcat-tabname  =  'ITAB_CTC_SMRY'.
    wa_fieldcat-fieldname = 'YY_BUKRS'.
    wa_fieldcat-seltext_m = 'Company Code'(010).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 9.
    wa_fieldcat-tabname  =  'ITAB_CTC_SMRY'.
    wa_fieldcat-fieldname = 'YY_GSBER'.
    wa_fieldcat-seltext_m = 'Business Area'(011).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 10.
    wa_fieldcat-tabname  =  'ITAB_CTC_SMRY'.
    wa_fieldcat-fieldname = 'YY_WERKS'.
    wa_fieldcat-seltext_m = 'Personnel Area'(012).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 11.
    wa_fieldcat-tabname  =  'ITAB_CTC_SMRY'.
    wa_fieldcat-fieldname = 'YY_BTRTL'.
    wa_fieldcat-seltext_m = 'Personnel Subarea'(013).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 12.
    wa_fieldcat-tabname  =  'ITAB_CTC_SMRY'.
    wa_fieldcat-fieldname = 'YY_PERSG'.
    wa_fieldcat-seltext_m = 'Employee Group'(014).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 13.
    wa_fieldcat-tabname  =  'ITAB_CTC_SMRY'.
    wa_fieldcat-fieldname = 'YY_PTEXT'.
    wa_fieldcat-seltext_m = 'Employee Group Text'(015).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 14.
    wa_fieldcat-tabname  =  'ITAB_CTC_SMRY'.
    wa_fieldcat-fieldname = 'YY_START_PERIOD'.
    wa_fieldcat-seltext_m = 'Start Date'(016).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 15.
    wa_fieldcat-tabname  =  'ITAB_CTC_SMRY'.
    wa_fieldcat-fieldname = 'YY_END_PERIOD'.
    wa_fieldcat-seltext_m = 'End Date'(017).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 16.
    wa_fieldcat-tabname  =  'ITAB_CTC_SMRY'.
    wa_fieldcat-fieldname = 'YY_TOTAL_CTC'.
    wa_fieldcat-seltext_m = 'Total CTC Amount'(018).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.


  ELSEIF p_trv = 'X'.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 1.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_SNO'.
    wa_fieldcat-seltext_m = 'SNo'(019).
    wa_fieldcat-key = 'X'.
    wa_fieldcat-key_sel = 'X'.
    wa_fieldcat-fix_column = 'X'.
    wa_fieldcat-input  = 'X'.
    wa_fieldcat-sp_group = 'A'.

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 2.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_PERNR'.
    wa_fieldcat-seltext_m = 'Personnel Number'(004).
    wa_fieldcat-key = 'X'.
    wa_fieldcat-key_sel = 'X'.
    wa_fieldcat-fix_column = 'X'.
    wa_fieldcat-input  = 'X'.
    wa_fieldcat-sp_group = 'A'.

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 3.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_NAME'.
    wa_fieldcat-seltext_m = 'Name of Employee'(005).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 4.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_GRADE'.
    wa_fieldcat-seltext_m = 'Grade'(006).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 5.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_DESIGNATION'.
    wa_fieldcat-seltext_m = 'Designation'(007).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 6.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_DEPARTMENT'.
    wa_fieldcat-seltext_m = 'Department'(008).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 7.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_LOCATION'.
    wa_fieldcat-seltext_m = 'Location'(009).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 8.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_BUKRS'.
    wa_fieldcat-seltext_m = 'Company Code'(010).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 9.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_GSBER'.
    wa_fieldcat-seltext_m = 'Business Area'(011).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 10.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_WERKS'.
    wa_fieldcat-seltext_m = 'Personnel Area'(012).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 11.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_BTRTL'.
    wa_fieldcat-seltext_m = 'Personnel Subarea'(013).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 12.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_FPBEG'.
    wa_fieldcat-seltext_m = 'Start Date'(016).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 13.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_FPEND'.
    wa_fieldcat-seltext_m = 'End Date'(017).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

***SOC BY SANYOGITA ON 01.10.2010 FOR ADDING REASON & DATE FOR TRIP
    wa_fieldcat-col_pos = 14.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_TRIP_REASON'.
    wa_fieldcat-seltext_m = 'Trip Reason'(020).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 15.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_TRIP_LOCATION'.
    wa_fieldcat-seltext_m = 'Trip Location'(021).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 16.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_TRIP_BEGIN'.
    wa_fieldcat-seltext_m = 'Trip Begin Date'(022).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 17.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_TRIP_END'.
    wa_fieldcat-seltext_m = 'Trip End Date'(023).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

***EOC BY SANYOGITA ON 01.10.2010 FOR ADDING REASON & DATE FOR TRIP

    wa_fieldcat-col_pos = 18.
    wa_fieldcat-tabname  =  'ITAB_TRV_SMRY'.
    wa_fieldcat-fieldname = 'YY_TOTAL_TRV'.
***SOC BY SANYOGITA ON 01.10.2010 FOR ADDING REASON & DATE FOR TRIP
*  WA_FIELDCAT-SELTEXT_M = 'Total Transfer Benifit Amount'.
    wa_fieldcat-seltext_m = 'Total Transfer / Travel Amount'(024).
***EOC BY SANYOGITA ON 01.10.2010 FOR ADDING REASON & DATE FOR TRIP

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

  ELSEIF p_rts = 'X'.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 1.
    wa_fieldcat-tabname  =  'ITAB_DATATABLE'.
    wa_fieldcat-fieldname = 'PERNR'.
    wa_fieldcat-seltext_m = 'Personnel Number'(004).
    wa_fieldcat-key = 'X'.
    wa_fieldcat-key_sel = 'X'.
    wa_fieldcat-fix_column = 'X'.
    wa_fieldcat-input  = 'X'.
    wa_fieldcat-sp_group = 'A'.

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 2.
    wa_fieldcat-tabname  =  'ITAB_DATATABLE'.
    wa_fieldcat-fieldname = 'ENAME'.
    wa_fieldcat-seltext_m = 'Name of Employee'(005).
    wa_fieldcat-key = 'X'.
    wa_fieldcat-key_sel = 'X'.
    wa_fieldcat-fix_column = 'X'.
    wa_fieldcat-input  = 'X'.
    wa_fieldcat-sp_group = 'A'.

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 3.
    wa_fieldcat-tabname  =  'ITAB_DATATABLE'.
    wa_fieldcat-fieldname = 'PERSK'.
    wa_fieldcat-seltext_m = 'Grade'(006).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 4.
    wa_fieldcat-tabname  =  'ITAB_DATATABLE'.
    wa_fieldcat-fieldname = 'BTRTL_TXT'.
    wa_fieldcat-seltext_m = 'Location'(009).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 5.
    wa_fieldcat-tabname  =  'ITAB_DATATABLE'.
    wa_fieldcat-fieldname = 'BUKRS'.
    wa_fieldcat-seltext_m = 'Company Code'(010).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 6.
    wa_fieldcat-tabname  =  'ITAB_DATATABLE'.
    wa_fieldcat-fieldname = 'PERSA'.
    wa_fieldcat-seltext_m = 'Personnel Area'(012).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 7.
    wa_fieldcat-tabname  =  'ITAB_DATATABLE'.
    wa_fieldcat-fieldname = 'LGART'.
    wa_fieldcat-seltext_m = 'Wage / Expense Type'(025).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 8.
    wa_fieldcat-tabname  =  'ITAB_DATATABLE'.
    wa_fieldcat-fieldname = 'LGART_TXT'.
    wa_fieldcat-seltext_m = 'Wage Type Text'(026).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 9.
    wa_fieldcat-tabname  =  'ITAB_DATATABLE'.
    wa_fieldcat-fieldname = 'INPER'.
    wa_fieldcat-seltext_m = 'In Period'(027).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 10.
    wa_fieldcat-tabname  =  'ITAB_DATATABLE'.
    wa_fieldcat-fieldname = 'IPEND'.
    wa_fieldcat-seltext_m = 'End of payroll period'(028).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 11.
    wa_fieldcat-tabname  =  'ITAB_DATATABLE'.
    wa_fieldcat-fieldname = 'FPPER'.
    wa_fieldcat-seltext_m = 'For-period for payroll'(029).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 12.
    wa_fieldcat-tabname  =  'ITAB_DATATABLE'.
    wa_fieldcat-fieldname = 'PAYDT'.
    wa_fieldcat-seltext_m = 'Pay date for payroll result'(030).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

***  WA_FIELDCAT-COL_POS = 13.
***  WA_FIELDCAT-TABNAME  =  'ITAB_DATATABLE'.
***  WA_FIELDCAT-FIELDNAME = 'PAYTY'.
***  WA_FIELDCAT-SELTEXT_M = 'Payroll type'.
***
***  APPEND WA_FIELDCAT TO P_I_FIELDCAT.
***  CLEAR WA_FIELDCAT.
***
***  WA_FIELDCAT-COL_POS = 14.
***  WA_FIELDCAT-TABNAME  =  'ITAB_DATATABLE'.
***  WA_FIELDCAT-FIELDNAME = 'PAYID'.
***  WA_FIELDCAT-SELTEXT_M = 'Payroll ID'.
***
***  APPEND WA_FIELDCAT TO P_I_FIELDCAT.
***  CLEAR WA_FIELDCAT.

    wa_fieldcat-col_pos = 13.
    wa_fieldcat-tabname  =  'ITAB_DATATABLE'.
    wa_fieldcat-fieldname = 'ANZHL'.
    wa_fieldcat-seltext_m = 'HR payroll: Number'(031).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 14.
    wa_fieldcat-tabname  =  'ITAB_DATATABLE'.
    wa_fieldcat-fieldname = 'BETRG'.
    wa_fieldcat-seltext_m = 'Amount'(032).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

  ELSEIF p_dtl = 'X'.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 1.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_PERNR'.
    wa_fieldcat-seltext_m = 'Personnel Number'(004).
    wa_fieldcat-key = 'X'.
    wa_fieldcat-key_sel = 'X'.
    wa_fieldcat-fix_column = 'X'.
    wa_fieldcat-input  = 'X'.
    wa_fieldcat-sp_group = 'A'.

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    CLEAR wa_fieldcat.
    wa_fieldcat-col_pos = 2.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_NAME'.
    wa_fieldcat-seltext_m = 'Name of Employee'(005).
    wa_fieldcat-key = 'X'.
    wa_fieldcat-key_sel = 'X'.
    wa_fieldcat-fix_column = 'X'.
    wa_fieldcat-input  = 'X'.
    wa_fieldcat-sp_group = 'A'.

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 3.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_GRADE'.
    wa_fieldcat-seltext_m = 'Grade'(006).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 4.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_DESIGNATION'.
    wa_fieldcat-seltext_m = 'Designation'(007).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 5.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_DEPARTMENT'.
    wa_fieldcat-seltext_m = 'Department'(008).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 6.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_LOCATION'.
    wa_fieldcat-seltext_m = 'Location'(009).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 7.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_BUKRS'.
    wa_fieldcat-seltext_m = 'Company Code'(010).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 8.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_BUTXT'.
    wa_fieldcat-seltext_m = 'Company Code Desc'(033).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 9.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_GSBER'.
    wa_fieldcat-seltext_m = 'Business Area'(011).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 10.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_GTEXT'.
    wa_fieldcat-seltext_m = 'Business Area Desc'(034).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 11.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_WERKS'.
    wa_fieldcat-seltext_m = 'Personnel Area'(012).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 12.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_NAME1'.
    wa_fieldcat-seltext_m = 'Personnel Area Desc'(035).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 13.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_BTRTL'.
    wa_fieldcat-seltext_m = 'Personnel Subarea'(013).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 14.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_PERSG'.
    wa_fieldcat-seltext_m = 'Employee Group'(014).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 15.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_PTEXT'.
    wa_fieldcat-seltext_m = 'Employee Group Desc'(036).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 16.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_FPPER'.
    wa_fieldcat-seltext_m = 'For-period'(037).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 17.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_FPBEG'.
    wa_fieldcat-seltext_m = 'Start date(FOR-period)'(038).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 18.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_FPEND'.
    wa_fieldcat-seltext_m = 'End Date(FOR-period)'(039).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 19.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_START_PERIOD'.
    wa_fieldcat-seltext_m = 'Start Date - Split Period'(040).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 20.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_END_PERIOD'.
    wa_fieldcat-seltext_m = 'End Date - Spilt Period'(041).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 21.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_PY_PRD_DAYS'.
    wa_fieldcat-seltext_m = 'Payroll Period-No of Days'(042).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 22.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_PRD_DAYS'.
    wa_fieldcat-seltext_m = 'Split Period-No of Days'(043).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 23.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YYPAYSCALE_MIN'.
    wa_fieldcat-seltext_m = 'Minimum Basic Amount'(044).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 24.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YYPAYSCALE_MAX'.
    wa_fieldcat-seltext_m = 'Maximum basic amount'(045).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 25.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_LGART'.
    wa_fieldcat-seltext_m = 'Wage /Expense type'(046).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 26.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_LGTXT'.
    wa_fieldcat-seltext_m = 'Wage Type Text'(026).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 27.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_BETRG'.
    wa_fieldcat-seltext_m = 'Amount'(032).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 28.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_BEN_TYPE'.
    wa_fieldcat-seltext_m = 'Benefit Type Code'(047).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.

    wa_fieldcat-col_pos = 29.
    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
    wa_fieldcat-fieldname = 'YY_BEN_TYP_TXT'.
    wa_fieldcat-seltext_m = 'Benefit Type Text'(048).

    APPEND wa_fieldcat TO p_i_fieldcat.
    CLEAR wa_fieldcat.


**************enhance new field*******************
*    wa_fieldcat-col_pos = 30.
*    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
*    wa_fieldcat-fieldname = 'YY_PD_FOOD'.
*    wa_fieldcat-seltext_m = 'Meals per Dimes'(057).
*    APPEND wa_fieldcat TO p_i_fieldcat.
*    CLEAR wa_fieldcat.
*
*
*    wa_fieldcat-col_pos = 31.
*    wa_fieldcat-tabname  =  'ITAB_CTC_DTL'.
*    wa_fieldcat-fieldname = 'yy_total_trv'.
*    wa_fieldcat-seltext_m = 'Total Transfer / Travel Amount'(058).
*
*
*    APPEND wa_fieldcat TO p_i_fieldcat.
*    CLEAR wa_fieldcat.
*
************************************************************************************


  ENDIF.

ENDFORM.                    " FILL_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  MOVE_TO_ALV_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM move_to_alv_table .

  DATA: w_count TYPE i.

  SORT itab_final_h BY yy_pernr yy_start_period.
  w_count = 0.
  LOOP AT itab_final_h.
    w_count = w_count + 1.
    CLEAR wa_ctc_smry.

    MOVE-CORRESPONDING itab_final_h TO wa_ctc_smry.
    wa_ctc_smry-yy_sno = w_count.
    APPEND wa_ctc_smry TO itab_ctc_smry.
  ENDLOOP.

ENDFORM.                    " MOVE_TO_ALV_TABLE
*&---------------------------------------------------------------------*
*&      Form  BUILD_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_TOP_OF_PAGE  text
*----------------------------------------------------------------------*
FORM build_header  USING    p_i_top_of_page TYPE slis_t_listheader.

  DATA: wa_line   TYPE slis_listheader,
        text(150) TYPE c.

  CLEAR wa_line.

  text = 'CTC Summary Report'(049).

  wa_line-typ = 'H'.
  wa_line-info = text.

  APPEND wa_line TO p_i_top_of_page.


ENDFORM.                    " BUILD_HEADER
*&---------------------------------------------------------------------*
*&      Form  BUILD_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_SORT  text
*----------------------------------------------------------------------*
FORM build_sort  USING    p_i_sort TYPE slis_t_sortinfo_alv.

  DATA: wa_sort TYPE slis_sortinfo_alv.

  IF p_sry = 'X' OR p_dbt = 'X'.

    CLEAR wa_sort.

    wa_sort-tabname = 'ITAB_CTC_SMRY'.
    wa_sort-fieldname = 'YY_PERNR'.
    wa_sort-spos = '1'.
    wa_sort-up = 'X'.
*  WA_SORT-DOWN = 'X'.
    APPEND wa_sort TO p_i_sort.

  ELSEIF p_trv = 'X'.
    CLEAR wa_sort.

    wa_sort-tabname = 'ITAB_TRV_SMRY'.
    wa_sort-fieldname = 'YY_PERNR'.
    wa_sort-spos = '1'.
    wa_sort-up = 'X'.
*  WA_SORT-DOWN = 'X'.
    APPEND wa_sort TO p_i_sort.

  ELSEIF p_rts = 'X'.
    CLEAR wa_sort.

    wa_sort-tabname = 'ITAB_DATATABLE'.
    wa_sort-fieldname = 'PERNR'.
    wa_sort-spos = '1'.
    wa_sort-up = 'X'.
*  WA_SORT-DOWN = 'X'.
    APPEND wa_sort TO p_i_sort.

  ELSEIF p_dtl = 'X'.
    CLEAR wa_sort.

    wa_sort-tabname = 'ITAB_CTC_DTL'.
    wa_sort-fieldname = 'YY_PERNR'.
    wa_sort-spos = '1'.
    wa_sort-up = 'X'.
*  WA_SORT-DOWN = 'X'.
    APPEND wa_sort TO p_i_sort.

    CLEAR wa_sort.

    wa_sort-tabname = 'ITAB_CTC_DTL'.
    wa_sort-fieldname = 'YY_START_PERIOD'.
    wa_sort-spos = '1'.
    wa_sort-up = 'X'.
*  WA_SORT-DOWN = 'X'.
    APPEND wa_sort TO p_i_sort.

    CLEAR wa_sort.

    wa_sort-tabname = 'ITAB_CTC_DTL'.
    wa_sort-fieldname = 'YY_BEN_TYPE'.
    wa_sort-spos = '1'.
    wa_sort-up = 'X'.
*  WA_SORT-DOWN = 'X'.
    APPEND wa_sort TO p_i_sort.

    CLEAR wa_sort.

    wa_sort-tabname = 'ITAB_CTC_DTL'.
    wa_sort-fieldname = 'YY_LGART'.
    wa_sort-spos = '1'.
    wa_sort-up = 'X'.
*  WA_SORT-DOWN = 'X'.
    APPEND wa_sort TO p_i_sort.
  ENDIF.

ENDFORM.                    " BUILD_SORT
*&---------------------------------------------------------------------*
*&      Form  DISP_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM disp_report .

  DATA: w_title TYPE text70.

  IF p_sry = 'X'.
    CONCATENATE 'CTC IMPLICATION OF EMPLOYEES FOR PERIOD'
                 w_from_dt '-' w_to_dt INTO w_title SEPARATED BY space.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*       I_INTERFACE_CHECK  = ' '
*       I_BYPASSING_BUFFER = ' '
*       I_BUFFER_ACTIVE    = ' '
        i_callback_program = w_repid
*       I_CALLBACK_PF_STATUS_SET          = ' '
*       I_CALLBACK_USER_COMMAND           = ' '
*       I_CALLBACK_TOP_OF_PAGE            = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   =
*       I_BACKGROUND_ID    = ' '
        i_grid_title       = w_title
*       I_GRID_SETTINGS    =
        is_layout          = i_layout
        it_fieldcat        = i_fieldcat
*       IT_EXCLUDING       =
*       IT_SPECIAL_GROUPS  =
        it_sort            = i_sort
*       IT_FILTER          =
*       IS_SEL_HIDE        =
*       I_DEFAULT          = 'X'
        i_save             = 'A'
*       IS_VARIANT         = 'A'
*       IT_EVENTS          =
*       IT_EVENT_EXIT      =
*       IS_PRINT           =
*       IS_REPREP_ID       =
*       I_SCREEN_START_COLUMN             = 0
*       I_SCREEN_START_LINE               = 0
*       I_SCREEN_END_COLUMN               = 0
*       I_SCREEN_END_LINE  = 0
*       IT_ALV_GRAPHICS    =
*       IT_HYPERLINK       =
*       IT_ADD_FIELDCAT    =
*       IT_EXCEPT_QINFO    =
*       I_HTML_HEIGHT_TOP  =
*       I_HTML_HEIGHT_END  =
* IMPORTING
*       E_EXIT_CAUSED_BY_CALLER           =
*       ES_EXIT_CAUSED_BY_USER            =
      TABLES
        t_outtab           = itab_ctc_smry
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSEIF p_dbt = 'X'.
    DATA: wa_events LIKE LINE OF i_events.
    i_exit_event-ucomm = 'FCOD'.
    i_exit_event-before = 'X'.
    i_exit_event-after = ''.
    APPEND i_exit_event.
    CLEAR i_exit_event.

    wa_events-name = 'USER_COMMAND'.
    wa_events-form = 'ON_SELECTION'.
*     events-form = 'USER_COMMAND'.
    APPEND wa_events TO i_events.
    CLEAR wa_events.

    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY' "'REUSE_ALV_GRID_DISPLAY' "
      EXPORTING
        i_callback_program       = 'YRHR001_CTC_REPORT' "W_REPID
        i_callback_pf_status_set = fpfstatus
        is_layout                = i_layout
        it_fieldcat              = i_fieldcat[]
        it_events                = i_events[] "I_EVENTS
        it_event_exit            = i_exit_event[]
      TABLES
        t_outtab                 = itab_ctc_smry
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
    ENDIF.

  ELSEIF p_trv = 'X'.
***SOC BY SANYOGITA ON 01.10.2010 FOR ADDING REASON & DATE FOR TRIP
*  CONCATENATE 'SETTLEMENT OF TRANSFER BENEFITS OF EMPLOYEES FOR PERIOD'
    CONCATENATE 'SETTLEMENT OF TRANSFER/TOUR OF EMPLOYEES FOR PERIOD'
***EOC BY SANYOGITA ON 01.10.2010 FOR ADDING REASON & DATE FOR TRIP
                   w_from_dt '-' w_to_dt INTO w_title SEPARATED BY space.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*       I_INTERFACE_CHECK  = ' '
*       I_BYPASSING_BUFFER = ' '
*       I_BUFFER_ACTIVE    = ' '
        i_callback_program = w_repid
*       I_CALLBACK_PF_STATUS_SET          = ' '
*       I_CALLBACK_USER_COMMAND           = ' '
*       I_CALLBACK_TOP_OF_PAGE            = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   =
*       I_BACKGROUND_ID    = ' '
        i_grid_title       = w_title
*       I_GRID_SETTINGS    =
        is_layout          = i_layout
        it_fieldcat        = i_fieldcat
*       IT_EXCLUDING       =
*       IT_SPECIAL_GROUPS  =
        it_sort            = i_sort
*       IT_FILTER          =
*       IS_SEL_HIDE        =
*       I_DEFAULT          = 'X'
        i_save             = 'A'
*       IS_VARIANT         = 'A'
*       IT_EVENTS          =
*       IT_EVENT_EXIT      =
*       IS_PRINT           =
*       IS_REPREP_ID       =
*       I_SCREEN_START_COLUMN             = 0
*       I_SCREEN_START_LINE               = 0
*       I_SCREEN_END_COLUMN               = 0
*       I_SCREEN_END_LINE  = 0
*       IT_ALV_GRAPHICS    =
*       IT_HYPERLINK       =
*       IT_ADD_FIELDCAT    =
*       IT_EXCEPT_QINFO    =
*       I_HTML_HEIGHT_TOP  =
*       I_HTML_HEIGHT_END  =
* IMPORTING
*       E_EXIT_CAUSED_BY_CALLER           =
*       ES_EXIT_CAUSED_BY_USER            =
      TABLES
        t_outtab           = itab_trv_smry
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSEIF p_rts = 'X'.
    PERFORM get_month_txt.
    CONCATENATE
          'BROUGHT FORWARD IN THE SALARY OF MONTH '
          w_month_txt_retro pn-begda+0(4) INTO w_title SEPARATED BY space.

*  CONCATENATE 'RETROACTIVE DATA '
*               W_FROM_DT '-' W_TO_DT INTO W_TITLE SEPARATED BY SPACE.

    IF wa_variant IS INITIAL AND p_vari IS NOT INITIAL .
      wa_variant-variant = p_vari.
      wa_variant-report  = w_repid.
      wa_variant-dependvars = 'S'.
    ENDIF.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*       I_INTERFACE_CHECK  = ' '
*       I_BYPASSING_BUFFER = ' '
*       I_BUFFER_ACTIVE    = ' '
        i_callback_program = w_repid
*       I_CALLBACK_PF_STATUS_SET          = ' '
*       I_CALLBACK_USER_COMMAND           = ' '
*       I_CALLBACK_TOP_OF_PAGE            = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   =
*       I_BACKGROUND_ID    = ' '
        i_grid_title       = w_title
*       I_GRID_SETTINGS    =
        is_layout          = i_layout
        it_fieldcat        = i_fieldcat[]
*       IT_EXCLUDING       =
*       IT_SPECIAL_GROUPS  =
        it_sort            = i_sort
*       IT_FILTER          =
*       IS_SEL_HIDE        =
*       I_DEFAULT          = 'X'
        i_save             = 'A'
        is_variant         = wa_variant
        it_events          = i_events[]
*       IT_EVENT_EXIT      =
*       IS_PRINT           =
*       IS_REPREP_ID       =
*       I_SCREEN_START_COLUMN             = 0
*       I_SCREEN_START_LINE               = 0
*       I_SCREEN_END_COLUMN               = 0
*       I_SCREEN_END_LINE  = 0
*       IT_ALV_GRAPHICS    =
*       IT_HYPERLINK       =
*       IT_ADD_FIELDCAT    =
*       IT_EXCEPT_QINFO    =
*       I_HTML_HEIGHT_TOP  =
*       I_HTML_HEIGHT_END  =
* IMPORTING
*       E_EXIT_CAUSED_BY_CALLER           =
*       ES_EXIT_CAUSED_BY_USER            =
      TABLES
        t_outtab           = itab_datatable
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSEIF p_dtl = 'X'.

    CONCATENATE 'CTC IMPLICATION OF EMPLOYEES FOR PERIOD'
                 w_from_dt '-' w_to_dt INTO w_title SEPARATED BY space.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*       I_INTERFACE_CHECK  = ' '
*       I_BYPASSING_BUFFER = ' '
*       I_BUFFER_ACTIVE    = ' '
        i_callback_program = w_repid
*       I_CALLBACK_PF_STATUS_SET          = ' '
*       I_CALLBACK_USER_COMMAND           = ' '
*       I_CALLBACK_TOP_OF_PAGE            = ' '
*       I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*       I_CALLBACK_HTML_END_OF_LIST       = ' '
*       I_STRUCTURE_NAME   =
*       I_BACKGROUND_ID    = ' '
        i_grid_title       = w_title
*       I_GRID_SETTINGS    =
        is_layout          = i_layout
        it_fieldcat        = i_fieldcat
*       IT_EXCLUDING       =
*       IT_SPECIAL_GROUPS  =
        it_sort            = i_sort
*       IT_FILTER          =
*       IS_SEL_HIDE        =
*       I_DEFAULT          = 'X'
        i_save             = 'A'
*       IS_VARIANT         = 'A'
*       IT_EVENTS          =
*       IT_EVENT_EXIT      =
*       IS_PRINT           =
*       IS_REPREP_ID       =
*       I_SCREEN_START_COLUMN             = 0
*       I_SCREEN_START_LINE               = 0
*       I_SCREEN_END_COLUMN               = 0
*       I_SCREEN_END_LINE  = 0
*       IT_ALV_GRAPHICS    =
*       IT_HYPERLINK       =
*       IT_ADD_FIELDCAT    =
*       IT_EXCEPT_QINFO    =
*       I_HTML_HEIGHT_TOP  =
*       I_HTML_HEIGHT_END  =
* IMPORTING
*       E_EXIT_CAUSED_BY_CALLER           =
*       ES_EXIT_CAUSED_BY_USER            =
      TABLES
        t_outtab           = itab_ctc_dtl
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDFORM.                    " DISP_REPORT
*&---------------------------------------------------------------------*
*&      Form  GET_RETRO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_retro_data .

  CLEAR itab_inper_directory.
  REFRESH itab_inper_directory.
  CLEAR itab_evp.
  REFRESH itab_evp.

  LOOP AT it_rgdir INTO wa_rgdir
                   WHERE ipend BETWEEN pn-begda AND pn-endda AND
                         void IS INITIAL.

    MOVE-CORRESPONDING wa_rgdir TO wa_inper_directory_entry.
    COLLECT wa_inper_directory_entry INTO itab_inper_directory.

  ENDLOOP.

  LOOP AT itab_inper_directory INTO wa_inper_directory_entry.
    CLEAR itab_evp_inper. REFRESH itab_evp_inper.
    CALL FUNCTION 'CD_EVALUATION_PERIODS'
      EXPORTING
        bonus_date      = wa_inper_directory_entry-ipend
        inper_modif     = wa_inper_directory_entry-iperm
        inper           = wa_inper_directory_entry-inper
        pay_type        = wa_inper_directory_entry-inpty
        pay_ident       = wa_inper_directory_entry-inpid
      TABLES
        rgdir           = it_rgdir
        evpdir          = itab_evp_inper
*       IABKRS          =
      EXCEPTIONS
        no_record_found = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
*     message id sy-msgid type sy-msgty number sy-msgno
*     with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.


    LOOP AT itab_evp_inper INTO itab_evp
*                           WHERE fpend BETWEEN w_beg AND w_end.
                           WHERE fpend BETWEEN '18000101' AND wa_period-endda.  " Tushar
      IF itab_evp-srtza EQ 'P'.
        MOVE-CORRESPONDING wa_inper_directory_entry TO itab_evp.
      ENDIF.
      APPEND itab_evp.
    ENDLOOP.
  ENDLOOP.

*DELETE ITAB_EVP WHERE INPER = '000000'.
  DESCRIBE TABLE itab_evp LINES w_evp_lines.
  IF w_evp_lines > 0.
    PERFORM read_relid USING w_country_code
                             w_import_relid.
    PERFORM proces_evp TABLES itab_evp
                       USING itab_p0000-pernr w_import_relid.
  ENDIF.

ENDFORM.                    " GET_RETRO_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_RELID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_MOLGA  text
*      -->P_IMPORT_RELID  text
*----------------------------------------------------------------------*
FORM read_relid  USING    VALUE(iv_molga) LIKE t500l-molga
                          ov_relid LIKE t500l-relid.

  TABLES: t500l.

  SELECT SINGLE * FROM t500l WHERE molga = iv_molga.

  ov_relid = t500l-relid.

ENDFORM.                    " READ_RELID
*&---------------------------------------------------------------------*
*&      Form  PROCES_EVP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITAB_EVP  text
*      -->P_ITAB_P0000_PERNR  text
*      -->P_W_IMPORT_RELID  text
*----------------------------------------------------------------------*
FORM proces_evp  TABLES evp STRUCTURE pc261
                 USING  VALUE(pernr) LIKE pernr-pernr
                        VALUE(import_relid) LIKE t500l-relid.

  DATA: return LIKE sy-subrc.
  DATA: postinfo LIKE hrpp_postinfo OCCURS 0 WITH HEADER LINE.
  DATA: postinfo_size TYPE i.
  DATA: result   TYPE pay99_result,
        result_2 TYPE pay99_result.
  DATA: lastwpbp LIKE LINE OF result-inter-wpbp.
  DATA: wpbp LIKE LINE OF result-inter-wpbp.
  DATA: rt               LIKE LINE OF result-inter-rt,
        wa_rt_basic_rate LIKE LINE OF result-inter-rt,
        wa_rt_basic      LIKE LINE OF result-inter-rt,
        wa_rt_da         LIKE LINE OF result-inter-rt.

  CALL FUNCTION 'HR_PCLX_INIT_BUFFER'.
  CALL FUNCTION 'HR_IMPORT_RGDIR_FROM_PCLX'
    EXPORTING
      employee_number   = pernr
      cluster_id        = import_relid
    TABLES
      import_rgdir      = evp
    EXCEPTIONS
      no_results        = 1
      no_read_authority = 2
      OTHERS            = 3.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT evp.

    MOVE-CORRESPONDING evp TO itab_inper.
    MOVE-CORRESPONDING evp TO itab_fpper.
*   py1_info_source-biwmonth = evp-paydt(6).
    CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
      EXPORTING
        clusterid                    = import_relid
        employeenumber               = pernr
        sequencenumber               = evp-seqnr
*       READ_ONLY_BUFFER             = ' '
        read_only_international      = 'X'
      CHANGING
        payroll_result               = result
      EXCEPTIONS
        illegal_isocode_or_clusterid = 1
        error_generating_import      = 2
        import_mismatch_error        = 3
        subpool_dir_full             = 4
        no_read_authority            = 5
        no_record_found              = 6
        versions_do_not_match        = 7
        OTHERS                       = 8.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CLEAR result_2.
    result_2 = result.
*   py1_info_source-molga = result-int-versc-molga.
    LOOP AT result-inter-wpbp INTO lastwpbp. ENDLOOP.
    LOOP AT result-inter-rt INTO rt WHERE lgart IN w_wt_retro.
      itab_lgart-lgart = rt-lgart.

      CLEAR itab_t512t.
      READ TABLE itab_t512t WITH KEY lgart = rt-lgart.
      IF itab_t512t IS NOT INITIAL.
        itab_lgart-lgart_txt = itab_t512t-lgtxt.
      ENDIF.

      READ TABLE result-inter-wpbp WITH KEY apznr = rt-apznr INTO wpbp.
      IF sy-subrc NE 0.
        wpbp = lastwpbp.
      ENDIF.
      itab_pernr_common-pernr = pernr.
      itab_pernr_common-ename = itab_final_h-yy_name.
      itab_bukrs-bukrs = wpbp-bukrs.
      itab_persa-persa = wpbp-werks.
      itab_btrtl-btrtl_persa = wpbp-werks.
      itab_btrtl-btrtl = wpbp-btrtl.

      CLEAR itab_t001p.
      READ TABLE itab_t001p WITH KEY werks = wpbp-werks
                                     btrtl = wpbp-btrtl.
      itab_btrtl-btrtl_txt = itab_t001p-btext.

      PERFORM read_kokrs USING wpbp-bukrs wpbp-gsber
                               itab_kokrs-kokrs.
      itab_kostl-kostl_kokrs = itab_kokrs-kokrs.
      itab_kostl-kostl = wpbp-kostl.
      itab_persg-persg = wpbp-persg.
      itab_persk-persk = wpbp-persk.

      IF evp-srtza EQ 'A'.
        w_anzhl = rt-anzhl.
        w_betrg_retro = rt-betrg.
      ELSE.
        w_anzhl = - rt-anzhl.
        w_betrg_retro = - rt-betrg.
      ENDIF.
      w_waers = result-inter-versc-waers.

      PERFORM process.

    ENDLOOP.               "rt
  ENDLOOP.                 "evp

ENDFORM.                    " PROCES_EVP
*&---------------------------------------------------------------------*
*&      Form  READ_KOKRS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WPBP_BUKRS  text
*      -->P_WPBP_GSBER  text
*      -->P_ITAB_KOKRS_KOKRS  text
*----------------------------------------------------------------------*
FORM read_kokrs  USING    VALUE(iv_bukrs)
                          VALUE(iv_gsber)
                          ov_kokrs.

  CALL FUNCTION 'HRCA_CONTROLLINGAREA_FIND'
    EXPORTING
      companycode  = iv_bukrs
      businessarea = iv_gsber
    IMPORTING
      contrlarea   = ov_kokrs
    EXCEPTIONS
      not_found    = 1
      OTHERS       = 2.

ENDFORM.                    " READ_KOKRS
*&---------------------------------------------------------------------*
*&      Form  PROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process .

  itab_datatable-anzhl = w_anzhl.
  itab_datatable-betrg = w_betrg_retro.
  itab_datatable-waers = w_waers.
  MOVE-CORRESPONDING itab_pernr_common TO itab_datatable.
  MOVE-CORRESPONDING itab_bukrs TO itab_datatable.
  MOVE-CORRESPONDING itab_persa TO itab_datatable.
  MOVE-CORRESPONDING itab_btrtl TO itab_datatable.
  MOVE-CORRESPONDING itab_kokrs TO itab_datatable.
  MOVE-CORRESPONDING itab_kostl TO itab_datatable.
  MOVE-CORRESPONDING itab_persg TO itab_datatable.
  MOVE-CORRESPONDING itab_persk TO itab_datatable.
  MOVE-CORRESPONDING itab_lgart TO itab_datatable.
  MOVE-CORRESPONDING itab_inper TO itab_datatable.
  MOVE-CORRESPONDING itab_fpper TO itab_datatable.
  COLLECT itab_datatable.
  COLLECT itab_pernr_common.
  COLLECT itab_bukrs.
  COLLECT itab_persa.
  COLLECT itab_btrtl.
  COLLECT itab_kokrs.
  COLLECT itab_kostl.
  COLLECT itab_persg.
  COLLECT itab_persk.
  COLLECT itab_lgart.
  COLLECT itab_inper.
  COLLECT itab_fpper.

ENDFORM.                    " PROCESS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RETRO_SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_retro_summary .

*populate fields
  PERFORM fill_fieldcat USING i_fieldcat.

*heading
  PERFORM build_header USING i_top_of_page.
  PERFORM build_layout USING i_layout.

  PERFORM build_sort USING i_sort.
  PERFORM build_eventtab USING i_events.

  i_layout-colwidth_optimize = 'X'.
  i_layout-f2code = w_f2code.
  i_layout-detail_popup = ' '.
  i_layout-detail_titlebar = 'DETAILS OF RECORD'.

  PERFORM disp_report.

ENDFORM.                    " DISPLAY_RETRO_SUMMARY
*&---------------------------------------------------------------------*
*&      Form  GET_FINAL_RETRO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_final_retro_data .

  IF itab_datatable[] IS NOT INITIAL.

    itab_datatable_2[] = itab_datatable[].

*  PERFORM CALC_OFFCYCLE_RETRO.
    DELETE itab_datatable WHERE betrg = 0.
    DELETE itab_datatable WHERE lgart = '9000'.
    DELETE itab_datatable WHERE payty = 'A'.

    IF itab_datatable[] IS NOT INITIAL.
      LOOP AT itab_datatable.
        IF itab_datatable-inper = itab_datatable-fpper.
*         AND ITAB_DATATABLE-INPER <> '000000'
*         AND ITAB_DATATABLE-FPPER <> '000000'.
          DELETE itab_datatable.
        ENDIF.
      ENDLOOP.

      IF itab_datatable[] IS NOT INITIAL.
        LOOP AT itab_datatable WHERE lgart = '1000'.
          CLEAR wa_retro_basic_rate.
          READ TABLE itab_datatable_2 INTO wa_retro_basic_rate WITH KEY
                     pernr = itab_datatable-pernr
                     lgart = '9000'
                     inper = itab_datatable-inper
                     fpper = itab_datatable-fpper.

          CLEAR wa_retro_basic.
          READ TABLE itab_datatable_2 INTO wa_retro_basic WITH KEY
                     pernr = itab_datatable-pernr
                     lgart = '1000'
                     inper = itab_datatable-inper
                     fpper = itab_datatable-fpper.

          CLEAR wa_retro_da.
          READ TABLE itab_datatable_2 INTO wa_retro_da WITH KEY
                     pernr = itab_datatable-pernr
                     lgart = '1005'
                     inper = itab_datatable-inper
                     fpper = itab_datatable-fpper.
          IF wa_retro_da IS INITIAL.
            READ TABLE itab_datatable_2 INTO wa_retro_da
                    WITH KEY pernr = itab_datatable-pernr
                             lgart = '1008'
                             inper = itab_datatable-inper
                             fpper = itab_datatable-fpper.
          ENDIF.


          CLEAR itab_prp_per.
          READ TABLE itab_prp_per
               WITH KEY yy_grade = itab_datatable-persk.
          CLEAR itab_datatable-lgart.
          CLEAR itab_datatable-lgart_txt.
          CLEAR itab_datatable-betrg.
          itab_datatable-lgart = '9005'.
          itab_datatable-lgart_txt = 'RETRO - PRP'.
          itab_datatable-betrg = wa_retro_basic-betrg *
                                 itab_prp_per-yy_percentage / 100.
          APPEND itab_datatable.

          CLEAR itab_datatable-lgart.
          CLEAR itab_datatable-lgart_txt.
          CLEAR itab_datatable-betrg.
          itab_datatable-lgart = '9001'.
          itab_datatable-lgart_txt = 'RETRO - GRATUITY'.
          itab_datatable-betrg = ( wa_retro_basic_rate-betrg +
                                   wa_retro_da-betrg ) * '4.86' / 100.
          APPEND itab_datatable.
          CLEAR itab_datatable-lgart.
          CLEAR itab_datatable-lgart_txt.
          CLEAR itab_datatable-betrg.
          itab_datatable-lgart = '9002'.
          itab_datatable-lgart_txt =
                        'RETRO - SUPERANNUATION BENEFITS (PRMS/ SBF)'.
          itab_datatable-betrg = ( wa_retro_basic_rate-betrg +
                                   wa_retro_da-betrg )  * '13.14' / 100.
          APPEND itab_datatable.

          CLEAR itab_datatable-lgart.
          CLEAR itab_datatable-lgart_txt.
          CLEAR itab_datatable-betrg.
          itab_datatable-lgart = '9003'.
          itab_datatable-lgart_txt = 'LEAVE SALARY CONTRIBUTION'.
          itab_datatable-betrg = ( wa_retro_basic_rate-betrg +
                                   wa_retro_da-betrg ) * '11.11' / 100.
          APPEND itab_datatable.

*          CLEAR itab_datatable-lgart.
*          CLEAR itab_datatable-lgart_txt.
*          CLEAR itab_datatable-betrg.
*          itab_datatable-lgart = '9004'.
*          itab_datatable-lgart_txt = 'RETRO - HALF PAY LEAVE'.
*          itab_datatable-betrg = ( wa_retro_basic_rate-betrg +
*                                   wa_retro_da-betrg ) * '2.78' / 100.
*          APPEND itab_datatable.

        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_FINAL_RETRO_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_MONTH_TXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_month_txt .

  w_mth_retro = pn-begda+4(2).
  CASE w_mth_retro.
    WHEN '01'.
      w_month_txt_retro = 'JANUARY'.
    WHEN '02'.
      w_month_txt_retro = 'FEBRUARY'.
    WHEN '03'.
      w_month_txt_retro = 'MARCH'.
    WHEN '04'.
      w_month_txt_retro = 'APRIL'.
    WHEN '05'.
      w_month_txt_retro = 'MAY'.
    WHEN '06'.
      w_month_txt_retro = 'JUNE'.
    WHEN '07'.
      w_month_txt_retro = 'JULY'.
    WHEN '08'.
      w_month_txt_retro = 'AUGUST'.
    WHEN '09'.
      w_month_txt_retro = 'SEPTEMBER'.
    WHEN '10'.
      w_month_txt_retro = 'OCTOBER'.
    WHEN '11'.
      w_month_txt_retro = 'NOVEMBER'.
    WHEN '12'.
      w_month_txt_retro = 'DECEMBER'.
  ENDCASE.

ENDFORM.                    " GET_MONTH_TXT
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_VARI  text
*----------------------------------------------------------------------*
FORM f4_for_variant  USING    p_p_vari .

  DATA: v_exit(1) TYPE c.
  DATA: v_save(1) TYPE c VALUE 'X'.

  wa_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = wa_variant
      i_save     = v_save
    IMPORTING
      e_exit     = v_exit
      es_variant = wa_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF v_exit = space.
      p_vari = wa_variant-variant.
    ENDIF.
  ENDIF.

ENDFORM.                    " F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*&      Form  BUILD_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_LAYOUT  text
*----------------------------------------------------------------------*
FORM build_layout  USING    pi_layout TYPE slis_layout_alv.

  pi_layout-zebra = 'X'.
  pi_layout-colwidth_optimize  = 'X'.

ENDFORM.                    " BUILD_LAYOUT
*--------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&--------------------------------------------------------------------*
FORM top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = i_top_of_page.

ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTTAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_I_EVENTS  text
*----------------------------------------------------------------------*
FORM build_eventtab  USING    pi_events TYPE slis_t_event.

  DATA: wal_event TYPE slis_alv_event.
*
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = pi_events.

  READ TABLE pi_events WITH KEY name =  slis_ev_top_of_page
                             INTO wal_event.

  IF sy-subrc = 0.
    MOVE c_formname_top_of_page TO wal_event-form.
    MODIFY pi_events FROM wal_event INDEX sy-tabix.
  ENDIF.

ENDFORM.                    " BUILD_EVENTTAB
*&---------------------------------------------------------------------*
*&      Form  DETAIL_CTC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM detail_ctc .

  IF itab_final_h[] IS NOT INITIAL AND itab_final_d[] IS NOT INITIAL.
    LOOP AT itab_final_h.
      CLEAR itab_ctc_dtl.
      MOVE-CORRESPONDING itab_final_h TO itab_ctc_dtl.

      LOOP AT itab_final_d WHERE yy_pernr = itab_final_h-yy_pernr AND
                                 yy_fpper = itab_final_h-yy_fpper AND
                                 yy_fpbeg = itab_final_h-yy_fpbeg AND
                                 yy_fpend = itab_final_h-yy_fpend AND
                   yy_start_period = itab_final_h-yy_start_period AND
                   yy_end_period = itab_final_h-yy_end_period.

        itab_ctc_dtl-yy_lgart       = itab_final_d-yy_lgart.
        itab_ctc_dtl-yy_lgtxt       = itab_final_d-yy_lgtxt.
        itab_ctc_dtl-yy_betrg       = itab_final_d-yy_betrg.
        itab_ctc_dtl-yy_ben_type    = itab_final_d-yy_ben_type.
        itab_ctc_dtl-yy_ben_typ_txt = itab_final_d-yy_ben_typ_txt.
*        itab_ctc_dtl-yy_pd_food     = itab_final_d-yy_pd_food.
*        itab_ctc_dtl-yy_total_trv   = itab_final_h-yy_total_ctc.
        APPEND itab_ctc_dtl.
      ENDLOOP.

    ENDLOOP.
  ENDIF.
ENDFORM.                    " DETAIL_CTC
*&---------------------------------------------------------------------*
*&      Form  CALC_OFFCYCLE_RETRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calc_offcycle_retro .

  itab_datatable_oc[] = itab_datatable[].
  DELETE itab_datatable_oc WHERE payty <> 'A'.
  DELETE itab_datatable WHERE payty = 'A'.

  IF itab_datatable_oc[] IS NOT INITIAL.
    LOOP AT itab_datatable_oc.
      itab_datatable_oc_final-pernr = itab_datatable_oc-pernr.
      itab_datatable_oc_final-ename = itab_datatable_oc-ename.
      itab_datatable_oc_final-bukrs = itab_datatable_oc-bukrs.
      itab_datatable_oc_final-persa = itab_datatable_oc-persa.
      itab_datatable_oc_final-molga = itab_datatable_oc-molga.
      itab_datatable_oc_final-lgart = itab_datatable_oc-lgart.
      itab_datatable_oc_final-lgart_txt = itab_datatable_oc-lgart_txt.
      itab_datatable_oc_final-inper = itab_datatable_oc-inper.
      itab_datatable_oc_final-ipend = itab_datatable_oc-ipend.
      itab_datatable_oc_final-abkrs = itab_datatable_oc-abkrs.
      itab_datatable_oc_final-permo = itab_datatable_oc-permo.
      itab_datatable_oc_final-fpper = itab_datatable_oc-fpper.
      itab_datatable_oc_final-paydt = itab_datatable_oc-paydt.
      itab_datatable_oc_final-payty = itab_datatable_oc-payty.
      itab_datatable_oc_final-payid = itab_datatable_oc-payid.
      itab_datatable_oc_final-anzhl = itab_datatable_oc-anzhl.
      itab_datatable_oc_final-betrg = itab_datatable_oc-betrg.
      itab_datatable_oc_final-waers = itab_datatable_oc-waers.

      COLLECT itab_datatable_oc_final.
    ENDLOOP.
  ENDIF.


  IF itab_datatable_oc_final[] IS NOT INITIAL.
    LOOP AT itab_datatable_oc_final.
      CLEAR itab_datatable_oc.
      READ TABLE itab_datatable_oc
           WITH KEY pernr = itab_datatable_oc_final-pernr
                    ename = itab_datatable_oc_final-ename
                    bukrs = itab_datatable_oc_final-bukrs
                    persa = itab_datatable_oc_final-persa
                    molga = itab_datatable_oc_final-molga
                    lgart = itab_datatable_oc_final-lgart
                    lgart_txt = itab_datatable_oc_final-lgart_txt
                    inper = itab_datatable_oc_final-inper
                    ipend = itab_datatable_oc_final-ipend
                    abkrs = itab_datatable_oc_final-abkrs
                    permo = itab_datatable_oc_final-permo
                    fpper = itab_datatable_oc_final-fpper
                    paydt = itab_datatable_oc_final-paydt
                    payty = itab_datatable_oc_final-payty
                    payid = itab_datatable_oc_final-payid.
      IF itab_datatable_oc IS NOT INITIAL.
        itab_datatable_oc_final-persk = itab_datatable_oc-persk.
        itab_datatable_oc_final-btrtl = itab_datatable_oc-btrtl.
        itab_datatable_oc_final-btrtl_txt = itab_datatable_oc-btrtl_txt.

        MODIFY itab_datatable_oc_final TRANSPORTING persk btrtl btrtl_txt.
      ENDIF.

      MOVE-CORRESPONDING itab_datatable_oc_final TO itab_datatable.
      APPEND itab_datatable.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " CALC_OFFCYCLE_RETRO
*&---------------------------------------------------------------------*
*&      Form  GET_REFRESHMENT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_refreshment_data .

  DATA: w_mth(2)        TYPE c,
        w_month_txt(10) TYPE c,
        w_year(4)       TYPE c.

  CLEAR itab_final_d-yy_sno.
  CLEAR itab_final_d-yy_betrg.
  CLEAR itab_final_d-yy_lgart.
  CLEAR itab_final_d-yy_lgtxt.
  CLEAR itab_final_d-yy_start_period.
  CLEAR itab_final_d-yy_end_period.
  CLEAR itab_final_d-yy_fpper.
  CLEAR itab_final_d-yy_fpbeg.
  CLEAR itab_final_d-yy_fpend.
  CLEAR itab_final_d-yy_werks.
  CLEAR itab_final_d-yy_btrtl.

  itab_final_d-yy_start_period = itab_final_h-yy_start_period.
  itab_final_d-yy_end_period = itab_final_h-yy_end_period.
  itab_final_d-yy_fpper = itab_final_h-yy_fpper.
  itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
  itab_final_d-yy_fpend = itab_final_h-yy_fpend.
  itab_final_d-yy_werks = itab_final_h-yy_werks.
  itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
  itab_final_d-yy_lgart = '9025'.
  CLEAR itab_t512t.
  READ TABLE itab_t512t WITH KEY lgart = '1230'.
  itab_final_d-yy_lgtxt = itab_t512t-lgtxt.

  IF itab_p0015[] IS NOT INITIAL.
    LOOP AT itab_p0015 WHERE pernr = itab_final_h-yy_pernr AND
                             subty = '1230' AND lgart = '1230'.

      IF itab_p0015-begda >= itab_final_d-yy_start_period AND
         itab_p0015-begda <= itab_final_d-yy_end_period.

        IF itab_p0015-begda >= '20110308'.
          CLEAR itab_headr.
          REFRESH itab_headr.
          CLEAR itab_haedc.
          REFRESH itab_haedc.
          DATA w_refnr TYPE pin_refnr.
          w_refnr = itab_p0015-zuord+0(13).
          CALL FUNCTION 'HRPBSINCLAIMS_CLS_READ'
            EXPORTING
              ifd_refnr = w_refnr
*             IFD_RFDAT = ' '
            IMPORTING
*             EST_TRANS = ITAB_TRANS
              est_headr = itab_headr
*             ETB_ADREP =
*             ETB_DEPDT =
*             ETB_RQRMK =
*             ETB_FDATA =
*             ETB_INFDT =
              est_haedc = itab_haedc
              etb_mlcnt = wa_mlcnt
*             EFD_REFNR =
*             EFD_ERRFG =
*             ETB_ERRTB =
            .
          IF wa_mlcnt IS NOT INITIAL.
            APPEND LINES OF wa_mlcnt TO itab_mlcnt.
            IF itab_mlcnt[] IS NOT INITIAL.
              LOOP AT itab_mlcnt.
                IF itab_mlcnt-cdt01 >= itab_final_h-yy_start_period AND
                   itab_mlcnt-cdt01 <= itab_final_h-yy_end_period.
                  CLEAR wa_final_d_9025.
                  READ TABLE itab_final_d INTO wa_final_d_9025 WITH KEY
                                    yy_pernr = itab_final_h-yy_pernr
                                    yy_lgart = '9025'.
                  IF wa_final_d_9025 IS NOT INITIAL.
                    wa_final_d_9025-yy_betrg = wa_final_d_9025-yy_betrg +
                                               itab_mlcnt-apamt.
                    MODIFY TABLE itab_final_d FROM wa_final_d_9025 TRANSPORTING
                                             yy_betrg.
                  ELSE.
                    itab_final_d-yy_betrg = itab_final_d-yy_betrg +
                                            itab_mlcnt-apamt.
                    IF itab_final_d-yy_betrg > 0.
                      itab_final_d-yy_ben_type = 3.
                      itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
                      APPEND itab_final_d.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GET_REFRESHMENT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_ENTERTAINMENT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_entertainment_data .

  DATA: w_mth(2)  TYPE c,
        w_year(4) TYPE c.

  CLEAR itab_final_d-yy_sno.
  CLEAR itab_final_d-yy_betrg.
  CLEAR itab_final_d-yy_lgart.
  CLEAR itab_final_d-yy_lgtxt.
  CLEAR itab_final_d-yy_start_period.
  CLEAR itab_final_d-yy_end_period.
  CLEAR itab_final_d-yy_fpper.
  CLEAR itab_final_d-yy_fpbeg.
  CLEAR itab_final_d-yy_fpend.
  CLEAR itab_final_d-yy_werks.
  CLEAR itab_final_d-yy_btrtl.

  itab_final_d-yy_start_period = itab_final_h-yy_start_period.
  itab_final_d-yy_end_period = itab_final_h-yy_end_period.
  itab_final_d-yy_fpper = itab_final_h-yy_fpper.
  itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
  itab_final_d-yy_fpend = itab_final_h-yy_fpend.
  itab_final_d-yy_werks = itab_final_h-yy_werks.
  itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
  itab_final_d-yy_lgart = '9026'.
  CLEAR itab_t512t.
  READ TABLE itab_t512t WITH KEY lgart = '4046'.  "Telephone reimb.
  itab_final_d-yy_lgtxt = 'Entertainment exp (Residence)'.

  LOOP AT itab_p0267 WHERE pernr = itab_final_h-yy_pernr AND
                           subty = '4046'.
    IF itab_p0267-begda >= itab_final_h-yy_start_period AND
       itab_p0267-begda <= itab_final_h-yy_end_period.
      IF itab_p0267-begda >= '20110214'.
        CLEAR itab_headr.
        REFRESH itab_headr.
        CLEAR itab_haedc.
        REFRESH itab_haedc.
        DATA w_refnr TYPE pin_refnr.
        w_refnr = itab_p0267-zuord+0(13).
        CALL FUNCTION 'HRPBSINCLAIMS_CLS_READ'
          EXPORTING
            ifd_refnr = w_refnr
*           IFD_RFDAT = ' '
          IMPORTING
*           EST_TRANS = ITAB_TRANS
            est_headr = itab_headr
*           ETB_ADREP =
*           ETB_DEPDT =
*           ETB_RQRMK =
*           ETB_FDATA =
*           ETB_INFDT =
            est_haedc = itab_haedc
*           ETB_MLCNT = ITAB_MLCNT
*           EFD_REFNR =
*           EFD_ERRFG =
*           ETB_ERRTB =
          .
        IF itab_headr[] IS NOT INITIAL AND
           itab_haedc[] IS NOT INITIAL.
          CLEAR itab_haedc.
          READ TABLE itab_haedc INDEX 1.
          CLEAR itab_headr.
          READ TABLE itab_headr INDEX 1.

          IF itab_headr IS NOT INITIAL AND itab_haedc IS NOT INITIAL.
            CLEAR w_year.
            CLEAR w_mth.
            w_mth = itab_haedc-c20t1.
            w_year = itab_haedc-c04t1.
            itab_final_d-yy_betrg = itab_headr-apamt.
          ENDIF.
        ENDIF.
      ENDIF.

      IF itab_final_d-yy_betrg > 0.
        itab_final_d-yy_ben_type = 3.
        itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
        APPEND itab_final_d.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " GET_ENTERTAINMENT_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_CLUSTER_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_cluster_data .
  DATA: w_retro_consider(1),
        w_for_period TYPE d.
  DATA w_refnr TYPE pin_refnr.
  CLEAR itab_datatable .
* Refreshment Claim
  LOOP AT  itab_p0015_claim WHERE pernr = itab_p0000-pernr
          AND subty = '1230' AND begda GE '20110214'
*          AND begda GE pn-begda AND begda LE pn-endda.
          AND begda GE wa_period-begda AND begda LE wa_period-endda.

    CLEAR: w_retro_consider,w_for_period.
    CLEAR itab_headr.
    REFRESH itab_headr.
    CLEAR itab_haedc.
    REFRESH itab_haedc.
    w_refnr = itab_p0015_claim-zuord+0(13).
    CALL FUNCTION 'HRPBSINCLAIMS_CLS_READ'
      EXPORTING
        ifd_refnr = w_refnr
*       IFD_RFDAT = ' '
      IMPORTING
*       EST_TRANS = ITAB_TRANS
        est_headr = itab_headr
*       ETB_ADREP =
*       ETB_DEPDT =
*       ETB_RQRMK =
*       ETB_FDATA =
*       ETB_INFDT =
        est_haedc = itab_haedc
        etb_mlcnt = wa_mlcnt
*       EFD_REFNR =
*       EFD_ERRFG =
*       ETB_ERRTB =
      .
    IF wa_mlcnt IS NOT INITIAL.
      CLEAR: itab_mlcnt,itab_mlcnt[].
      APPEND LINES OF wa_mlcnt TO itab_mlcnt.
      IF itab_mlcnt[] IS NOT INITIAL.
        LOOP AT itab_mlcnt.
          IF itab_mlcnt-cdt01+0(6) NE pn-endda+0(6) .
            w_retro_consider = 'X'.

            CALL FUNCTION 'HR_HCP_GET_LAST_DAY_OF_MONTH'
              EXPORTING
                im_date              = itab_mlcnt-cdt01
              IMPORTING
*               EX_DATE_IS_MONTHEND  =
                ex_last_day_of_month = w_for_period.

            EXIT.
          ENDIF.
        ENDLOOP.
*    add the amount if claim is for retro summary
        IF w_retro_consider EQ 'X'.
          itab_datatable-pernr = itab_p0000-pernr.
*       NAME
          READ TABLE itab_p0001 WITH KEY pernr = itab_p0000-pernr.
          IF itab_p0001 IS NOT INITIAL.
            itab_datatable-ename  = itab_p0001-ename.
          ENDIF.
*      WAGE TYPE AND ITS DESCRIPTION
          itab_datatable-lgart = '1230'.

          CLEAR itab_t512t.
          READ TABLE itab_t512t WITH KEY lgart = '1230'.
          IF itab_t512t IS NOT INITIAL.
            itab_datatable-lgart_txt = itab_t512t-lgtxt.
          ENDIF.
*       PICK THE COMPANY CODE AND BUSINESS AREA LOCATION  GRADE
          LOOP AT itab_p0001_claim WHERE pernr EQ itab_p0000-pernr
            AND begda LE w_for_period AND endda GE w_for_period.
            itab_datatable-bukrs = itab_p0001_claim-bukrs.
            itab_datatable-persa = itab_p0001_claim-werks.
            itab_datatable-persk = itab_p0001_claim-persk.
            itab_datatable-btrtl = itab_p0001_claim-btrtl.
            READ TABLE itab_t001p WITH KEY werks = itab_datatable-persa
                                     btrtl = itab_datatable-btrtl.
            itab_datatable-btrtl_txt = itab_t001p-btext.
          ENDLOOP.
*       PICK THE AMOUNT
          LOOP AT itab_mlcnt.
            itab_datatable-betrg = itab_datatable-betrg + itab_mlcnt-apamt.
          ENDLOOP.
*       FILL IN PERIOD
*          itab_datatable-ipend = pn-endda.
          itab_datatable-ipend = wa_period-endda.
          PERFORM get_period USING itab_datatable-ipend
                CHANGING itab_datatable-inper.
*       FILL FOR PERIOD
          itab_datatable-paydt =  w_for_period.
          PERFORM get_period USING w_for_period
                CHANGING itab_datatable-fpper.

          COLLECT itab_datatable.
          CLEAR itab_datatable.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

*  Toll charges
  LOOP AT  itab_p0015_claim WHERE pernr = itab_p0000-pernr
          AND subty = '4018' AND begda GE '20110214'
          AND begda GE pn-begda AND begda LE pn-endda.

    CLEAR: w_retro_consider,w_for_period.
    CLEAR itab_headr.
    REFRESH itab_headr.
    CLEAR itab_haedc.
    REFRESH itab_haedc.
    w_refnr = itab_p0015_claim-zuord+0(13).
    CALL FUNCTION 'HRPBSINCLAIMS_CLS_READ'
      EXPORTING
        ifd_refnr = w_refnr
*       IFD_RFDAT = ' '
      IMPORTING
*       EST_TRANS = ITAB_TRANS
        est_headr = itab_headr
*       ETB_ADREP =
*       ETB_DEPDT =
*       ETB_RQRMK =
*       ETB_FDATA =
*       ETB_INFDT =
        est_haedc = itab_haedc
        etb_mlcnt = wa_mlcnt
*       EFD_REFNR =
*       EFD_ERRFG =
*       ETB_ERRTB =
      .
    IF wa_mlcnt IS NOT INITIAL.
      CLEAR: itab_mlcnt,itab_mlcnt[].
      APPEND LINES OF wa_mlcnt TO itab_mlcnt.
      IF itab_mlcnt[] IS NOT INITIAL.
        LOOP AT itab_mlcnt.
          IF itab_mlcnt-cdt01+0(6) NE pn-endda+0(6) .
            w_retro_consider = 'X'.

            CALL FUNCTION 'HR_HCP_GET_LAST_DAY_OF_MONTH'
              EXPORTING
                im_date              = itab_mlcnt-cdt01
              IMPORTING
*               EX_DATE_IS_MONTHEND  =
                ex_last_day_of_month = w_for_period.

            EXIT.
          ENDIF.
        ENDLOOP.
*    add the amount if claim is for retro summary
        IF w_retro_consider EQ 'X'.
          itab_datatable-pernr = itab_p0000-pernr.
*       NAME
          READ TABLE itab_p0001 WITH KEY pernr = itab_p0000-pernr.
          IF itab_p0001 IS NOT INITIAL.
            itab_datatable-ename  = itab_p0001-ename.
          ENDIF.
*      WAGE TYPE AND ITS DESCRIPTION
          itab_datatable-lgart = '4018'.

          CLEAR itab_t512t.
          READ TABLE itab_t512t WITH KEY lgart = '4018'.
          IF itab_t512t IS NOT INITIAL.
            itab_datatable-lgart_txt = itab_t512t-lgtxt.
          ENDIF.
*       PICK THE COMPANY CODE AND BUSINESS AREA LOCATION  GRADE
          LOOP AT itab_p0001_claim WHERE pernr EQ itab_p0000-pernr
            AND begda LE w_for_period AND endda GE w_for_period.
            itab_datatable-bukrs = itab_p0001_claim-bukrs.
            itab_datatable-persa = itab_p0001_claim-werks.
            itab_datatable-persk = itab_p0001_claim-persk.
            itab_datatable-btrtl = itab_p0001_claim-btrtl.
            READ TABLE itab_t001p WITH KEY werks = itab_datatable-persa
                                     btrtl = itab_datatable-btrtl.
            itab_datatable-btrtl_txt = itab_t001p-btext.
          ENDLOOP.
*       PICK THE AMOUNT
          LOOP AT itab_mlcnt.
            itab_datatable-betrg = itab_datatable-betrg + itab_mlcnt-apamt.
          ENDLOOP.
*       FILL IN PERIOD
          itab_datatable-ipend = pn-endda.
          PERFORM get_period USING itab_datatable-ipend
                CHANGING itab_datatable-inper.
*       FILL FOR PERIOD
          itab_datatable-paydt =  w_for_period.
          PERFORM get_period USING w_for_period
                CHANGING itab_datatable-fpper.

          COLLECT itab_datatable.
          CLEAR itab_datatable.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

***  LTER
***SOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 18.05.2020, Charm No #4000002138
*Remarks: The below code has been commented out because a new logic has been written
*to fetch amount of WT - 4075. This block of code
*was executing and appending the record with incorrect value.
*  LOOP AT  itab_p0267 WHERE pernr = itab_p0000-pernr
*          AND subty = '4075' AND begda GE '20110214'
*          AND begda GE pn-begda AND begda LE pn-endda.
*
*    CLEAR: w_retro_consider,w_for_period.
*    CLEAR itab_headr.
*    REFRESH itab_headr.
*    CLEAR itab_haedc.
*    REFRESH itab_haedc.
*    w_refnr = itab_p0267-zuord+0(13).
*    CALL FUNCTION 'HRPBSINCLAIMS_CLS_READ'
*      EXPORTING
*        ifd_refnr = w_refnr
**       IFD_RFDAT = ' '
*      IMPORTING
**       EST_TRANS = ITAB_TRANS
*        est_headr = itab_headr
**       ETB_ADREP =
**       ETB_DEPDT =
**       ETB_RQRMK =
**       ETB_FDATA =
**       ETB_INFDT =
*        est_haedc = itab_haedc
*        etb_mlcnt = wa_mlcnt
**       EFD_REFNR =
**       EFD_ERRFG =
**       ETB_ERRTB =
*      .
*    IF wa_mlcnt IS NOT INITIAL.
*      CLEAR: itab_mlcnt,itab_mlcnt[].
*      APPEND LINES OF wa_mlcnt TO itab_mlcnt.
*      IF itab_mlcnt[] IS NOT INITIAL.
*        LOOP AT itab_mlcnt.
*          IF itab_mlcnt-cdt01+0(6) NE pn-endda+0(6) .
*            w_retro_consider = 'X'.
*
*            CALL FUNCTION 'HR_HCP_GET_LAST_DAY_OF_MONTH'
*              EXPORTING
*                im_date              = itab_mlcnt-cdt01
*              IMPORTING
**               EX_DATE_IS_MONTHEND  =
*                ex_last_day_of_month = w_for_period.
*
*            EXIT.
*          ENDIF.
*        ENDLOOP.
**    add the amount if claim is for retro summary
*        IF w_retro_consider EQ 'X'.
*          itab_datatable-pernr = itab_p0000-pernr.
**       NAME
*          READ TABLE itab_p0001 WITH KEY pernr = itab_p0000-pernr.
*          IF itab_p0001 IS NOT INITIAL.
*            itab_datatable-ename  = itab_p0001-ename.
*          ENDIF.
**      WAGE TYPE AND ITS DESCRIPTION
*          itab_datatable-lgart = '4075'.
*
*          CLEAR itab_t512t.
*          READ TABLE itab_t512t WITH KEY lgart = '4075'.
*          IF itab_t512t IS NOT INITIAL.
*            itab_datatable-lgart_txt = itab_t512t-lgtxt.
*          ENDIF.
**       PICK THE COMPANY CODE AND BUSINESS AREA LOCATION  GRADE
*          LOOP AT itab_p0001_claim WHERE pernr EQ itab_p0000-pernr
*            AND begda LE w_for_period AND endda GE w_for_period.
*            itab_datatable-bukrs = itab_p0001_claim-bukrs.
*            itab_datatable-persa = itab_p0001_claim-werks.
*            itab_datatable-persk = itab_p0001_claim-persk.
*            itab_datatable-btrtl = itab_p0001_claim-btrtl.
*            READ TABLE itab_t001p WITH KEY werks = itab_datatable-persa
*                                     btrtl = itab_datatable-btrtl.
*            itab_datatable-btrtl_txt = itab_t001p-btext.
*          ENDLOOP.
**       PICK THE AMOUNT
*          LOOP AT itab_mlcnt.
*            itab_datatable-betrg = itab_datatable-betrg + itab_mlcnt-apamt.
*          ENDLOOP.
**       FILL IN PERIOD
*          itab_datatable-ipend = pn-endda.
*          PERFORM get_period USING itab_datatable-ipend
*                CHANGING itab_datatable-inper.
**       FILL FOR PERIOD
*          itab_datatable-paydt =  w_for_period.
*          PERFORM get_period USING w_for_period
*                CHANGING itab_datatable-fpper.
*
*          COLLECT itab_datatable.
*          CLEAR itab_datatable.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
***EOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 18.05.2020, Charm No #4000002138

***  ENTERTAINMENT REIMBURSEMENT
  LOOP AT  itab_p0015_claim WHERE pernr = itab_p0000-pernr
          AND subty = '4046' AND begda GE '20110214'
          AND begda GE pn-begda AND begda LE pn-endda.

    CLEAR: w_retro_consider,w_for_period.
    CLEAR itab_headr.
    REFRESH itab_headr.
    CLEAR itab_haedc.
    REFRESH itab_haedc.
    w_refnr = itab_p0015_claim-zuord+0(13).
    CALL FUNCTION 'HRPBSINCLAIMS_CLS_READ'
      EXPORTING
        ifd_refnr = w_refnr
*       IFD_RFDAT = ' '
      IMPORTING
*       EST_TRANS = ITAB_TRANS
        est_headr = itab_headr
*       ETB_ADREP =
*       ETB_DEPDT =
*       ETB_RQRMK =
*       ETB_FDATA =
*       ETB_INFDT =
        est_haedc = itab_haedc
        etb_mlcnt = wa_mlcnt
*       EFD_REFNR =
*       EFD_ERRFG =
*       ETB_ERRTB =
      .
    IF wa_mlcnt IS NOT INITIAL.
      CLEAR: itab_mlcnt,itab_mlcnt[].
      APPEND LINES OF wa_mlcnt TO itab_mlcnt.
      IF itab_mlcnt[] IS NOT INITIAL.
        LOOP AT itab_mlcnt.
          IF itab_mlcnt-cdt01+0(6) NE pn-endda+0(6) .
            w_retro_consider = 'X'.

            CALL FUNCTION 'HR_HCP_GET_LAST_DAY_OF_MONTH'
              EXPORTING
                im_date              = itab_mlcnt-cdt01
              IMPORTING
*               EX_DATE_IS_MONTHEND  =
                ex_last_day_of_month = w_for_period.

            EXIT.
          ENDIF.
        ENDLOOP.
*    add the amount if claim is for retro summary
        IF w_retro_consider EQ 'X'.
          itab_datatable-pernr = itab_p0000-pernr.
*       NAME
          READ TABLE itab_p0001 WITH KEY pernr = itab_p0000-pernr.
          IF itab_p0001 IS NOT INITIAL.
            itab_datatable-ename  = itab_p0001-ename.
          ENDIF.
*      WAGE TYPE AND ITS DESCRIPTION
          itab_datatable-lgart = '4046'.

          CLEAR itab_t512t.
          READ TABLE itab_t512t WITH KEY lgart = '4046'.
          IF itab_t512t IS NOT INITIAL.
            itab_datatable-lgart_txt = itab_t512t-lgtxt.
          ENDIF.
*       PICK THE COMPANY CODE AND BUSINESS AREA LOCATION  GRADE
          LOOP AT itab_p0001_claim WHERE pernr EQ itab_p0000-pernr
            AND begda LE w_for_period AND endda GE w_for_period.
            itab_datatable-bukrs = itab_p0001_claim-bukrs.
            itab_datatable-persa = itab_p0001_claim-werks.
            itab_datatable-persk = itab_p0001_claim-persk.
            itab_datatable-btrtl = itab_p0001_claim-btrtl.
            READ TABLE itab_t001p WITH KEY werks = itab_datatable-persa
                                     btrtl = itab_datatable-btrtl.
            itab_datatable-btrtl_txt = itab_t001p-btext.
          ENDLOOP.
*       PICK THE AMOUNT
          LOOP AT itab_mlcnt.
            itab_datatable-betrg = itab_datatable-betrg + itab_mlcnt-apamt.
          ENDLOOP.
*       FILL IN PERIOD
          itab_datatable-ipend = pn-endda.
          PERFORM get_period USING itab_datatable-ipend
                CHANGING itab_datatable-inper.
*       FILL FOR PERIOD
          itab_datatable-paydt =  w_for_period.
          PERFORM get_period USING w_for_period
                CHANGING itab_datatable-fpper.

          COLLECT itab_datatable.
          CLEAR itab_datatable.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

*  NEWS PAPER REIMBURSEMENT 4010

***SOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 18.05.2020, Charm No #4000002138
*Remarks: The below code has been commented out because a new logic has been written
*to fetch amount of WT - 4010. This block of code
*was executing and appending the record with incorrect value.
*  LOOP AT  itab_p0267 WHERE pernr = itab_p0000-pernr
*            AND subty = '4010' AND begda GE '20110214'
*            AND begda GE pn-begda AND begda LE pn-endda.
*
*    CLEAR: w_retro_consider,w_for_period.
*    CLEAR itab_headr.
*    REFRESH itab_headr.
*    CLEAR itab_haedc.
*    REFRESH itab_haedc.
*    w_refnr = itab_p0267-zuord+0(13).
*    CALL FUNCTION 'HRPBSINCLAIMS_CLS_READ'
*      EXPORTING
*        ifd_refnr = w_refnr
**       IFD_RFDAT = ' '
*      IMPORTING
**       EST_TRANS = ITAB_TRANS
*        est_headr = itab_headr
**       ETB_ADREP =
**       ETB_DEPDT =
**       ETB_RQRMK =
**       ETB_FDATA =
**       ETB_INFDT =
*        est_haedc = itab_haedc
*        etb_mlcnt = wa_mlcnt
**       EFD_REFNR =
**       EFD_ERRFG =
**       ETB_ERRTB =
*      .
*    IF itab_headr IS NOT INITIAL AND
*             itab_haedc IS NOT INITIAL.
*
*      IF itab_haedc-c40t1 NE pn-endda+4(2) OR itab_haedc-c10t1 NE pn-endda+0(4) .
*        CONCATENATE itab_haedc-c10t1 itab_haedc-c40t1 '01' INTO w_for_period.
*        CALL FUNCTION 'HR_HCP_GET_LAST_DAY_OF_MONTH'
*          EXPORTING
*            im_date              = w_for_period
*          IMPORTING
*            ex_last_day_of_month = w_for_period.
*
*
*        itab_datatable-pernr = itab_p0000-pernr.
**       NAME
*        READ TABLE itab_p0001 WITH KEY pernr = itab_p0000-pernr.
*        IF itab_p0001 IS NOT INITIAL.
*          itab_datatable-ename  = itab_p0001-ename.
*        ENDIF.
**      WAGE TYPE AND ITS DESCRIPTION
*        itab_datatable-lgart = '4010'.
*
*        CLEAR itab_t512t.
*        READ TABLE itab_t512t WITH KEY lgart = '4010'.
*        IF itab_t512t IS NOT INITIAL.
*          itab_datatable-lgart_txt = itab_t512t-lgtxt.
*        ENDIF.
**       PICK THE COMPANY CODE AND BUSINESS AREA LOCATION  GRADE
*        LOOP AT itab_p0001_claim WHERE pernr EQ itab_p0000-pernr
*          AND begda LE w_for_period AND endda GE w_for_period.
*          itab_datatable-bukrs = itab_p0001_claim-bukrs.
*          itab_datatable-persa = itab_p0001_claim-werks.
*          itab_datatable-persk = itab_p0001_claim-persk.
*          itab_datatable-btrtl = itab_p0001_claim-btrtl.
*          READ TABLE itab_t001p WITH KEY werks = itab_datatable-persa
*                                   btrtl = itab_datatable-btrtl.
*          itab_datatable-btrtl_txt = itab_t001p-btext.
*        ENDLOOP.
**       PICK THE AMOUNT
*        itab_datatable-betrg = itab_headr-apamt..
*
**       FILL IN PERIOD
*        itab_datatable-ipend = pn-endda.
*        PERFORM get_period USING itab_datatable-ipend
*              CHANGING itab_datatable-inper.
**       FILL FOR PERIOD
*        itab_datatable-paydt =  w_for_period.
*        PERFORM get_period USING w_for_period
*              CHANGING itab_datatable-fpper.
*
*        COLLECT itab_datatable.
*        CLEAR itab_datatable.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
***EOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 18.05.2020, Charm No #4000002138

*  NEWS PAPER REIMBURSEMENT OFFICE 4011
***SOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 18.05.2020, Charm No #4000002138
*Remarks: The below code has been commented out because a new logic has been written
*to fetch amount of WT - 4011. This block of code
*was executing and appending the record with incorrect value.
*  LOOP AT  itab_p0267 WHERE pernr = itab_p0000-pernr
*            AND subty = '4011' AND begda GE '20110214'
*            AND begda GE pn-begda AND begda LE pn-endda.
*
*    CLEAR: w_retro_consider,w_for_period.
*    CLEAR itab_headr.
*    REFRESH itab_headr.
*    CLEAR itab_haedc.
*    REFRESH itab_haedc.
*    w_refnr = itab_p0267-zuord+0(13).
*    CALL FUNCTION 'HRPBSINCLAIMS_CLS_READ'
*      EXPORTING
*        ifd_refnr = w_refnr
**       IFD_RFDAT = ' '
*      IMPORTING
**       EST_TRANS = ITAB_TRANS
*        est_headr = itab_headr
**       ETB_ADREP =
**       ETB_DEPDT =
**       ETB_RQRMK =
**       ETB_FDATA =
**       ETB_INFDT =
*        est_haedc = itab_haedc
*        etb_mlcnt = wa_mlcnt
**       EFD_REFNR =
**       EFD_ERRFG =
**       ETB_ERRTB =
*      .
*    IF itab_headr IS NOT INITIAL AND
*             itab_haedc IS NOT INITIAL.
**      CLEAR ITAB_HAEDC.
**      READ TABLE ITAB_HAEDC INDEX 1.
**      CLEAR ITAB_HEADR.
**      READ TABLE ITAB_HEADR INDEX 1.
*
*      IF itab_haedc-c40t1 NE pn-endda+4(2) OR itab_haedc-c10t1 NE pn-endda+0(4) .
*        CONCATENATE itab_haedc-c10t1 itab_haedc-c40t1 '01' INTO w_for_period.
*        CALL FUNCTION 'HR_HCP_GET_LAST_DAY_OF_MONTH'
*          EXPORTING
*            im_date              = w_for_period
*          IMPORTING
*            ex_last_day_of_month = w_for_period.
*
*
*        itab_datatable-pernr = itab_p0000-pernr.
**       NAME
*        READ TABLE itab_p0001 WITH KEY pernr = itab_p0000-pernr.
*        IF itab_p0001 IS NOT INITIAL.
*          itab_datatable-ename  = itab_p0001-ename.
*        ENDIF.
**      WAGE TYPE AND ITS DESCRIPTION
*        itab_datatable-lgart = '4011'.
*
*        CLEAR itab_t512t.
*        READ TABLE itab_t512t WITH KEY lgart = '4011'.
*        IF itab_t512t IS NOT INITIAL.
*          itab_datatable-lgart_txt = itab_t512t-lgtxt.
*        ENDIF.
**       PICK THE COMPANY CODE AND BUSINESS AREA LOCATION  GRADE
*        LOOP AT itab_p0001_claim WHERE pernr EQ itab_p0000-pernr
*          AND begda LE w_for_period AND endda GE w_for_period.
*          itab_datatable-bukrs = itab_p0001_claim-bukrs.
*          itab_datatable-persa = itab_p0001_claim-werks.
*          itab_datatable-persk = itab_p0001_claim-persk.
*          itab_datatable-btrtl = itab_p0001_claim-btrtl.
*          READ TABLE itab_t001p WITH KEY werks = itab_datatable-persa
*                                   btrtl = itab_datatable-btrtl.
*          itab_datatable-btrtl_txt = itab_t001p-btext.
*        ENDLOOP.
**       PICK THE AMOUNT
*        itab_datatable-betrg = itab_headr-apamt..
*
**       FILL IN PERIOD
*        itab_datatable-ipend = pn-endda.
*        PERFORM get_period USING itab_datatable-ipend
*              CHANGING itab_datatable-inper.
**       FILL FOR PERIOD
*        itab_datatable-paydt =  w_for_period.
*        PERFORM get_period USING w_for_period
*              CHANGING itab_datatable-fpper.
*
*        COLLECT itab_datatable.
*        CLEAR itab_datatable.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
***EOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 18.05.2020, Charm No #4000002138


*  TELEPHONE REMIMBURSEMENT 4015

***SOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 18.05.2020, Charm No #4000002138
*Remarks: The below code has been commented out because a new logic has been written
*to fetch amount of WT - 4015. This block of code was executing and appending the
*record with incorrect value.
*  LOOP AT  itab_p0267 WHERE pernr = itab_p0000-pernr
*            AND subty = '4015' AND begda GE '20110214'
*            AND begda GE pn-begda AND begda LE pn-endda.
*
*    CLEAR: w_retro_consider,w_for_period.
*    CLEAR itab_headr.
*    REFRESH itab_headr.
*    CLEAR itab_haedc.
*    REFRESH itab_haedc.
*    w_refnr = itab_p0267-zuord+0(13).
*    CALL FUNCTION 'HRPBSINCLAIMS_CLS_READ'
*      EXPORTING
*        ifd_refnr = w_refnr
**       IFD_RFDAT = ' '
*      IMPORTING
**       EST_TRANS = ITAB_TRANS
*        est_headr = itab_headr
**       ETB_ADREP =
**       ETB_DEPDT =
**       ETB_RQRMK =
**       ETB_FDATA =
**       ETB_INFDT =
*        est_haedc = itab_haedc
*        etb_mlcnt = wa_mlcnt
**       EFD_REFNR =
**       EFD_ERRFG =
**       ETB_ERRTB =
*      .
*    IF itab_headr IS NOT INITIAL AND
*             itab_haedc IS NOT INITIAL.
**      CLEAR ITAB_HAEDC.
**      READ TABLE ITAB_HAEDC INDEX 1.
**      CLEAR ITAB_HEADR.
**      READ TABLE ITAB_HEADR INDEX 1.
*
*      IF itab_haedc-c20t1 NE pn-endda+4(2) OR itab_haedc-c04t1 NE pn-endda+0(4) .
*        CONCATENATE itab_haedc-c04t1 itab_haedc-c20t1 '01' INTO w_for_period.
*        CALL FUNCTION 'HR_HCP_GET_LAST_DAY_OF_MONTH'
*          EXPORTING
*            im_date              = w_for_period
*          IMPORTING
*            ex_last_day_of_month = w_for_period.
*
*
*        itab_datatable-pernr = itab_p0000-pernr.
**       NAME
*        READ TABLE itab_p0001 WITH KEY pernr = itab_p0000-pernr.
*        IF itab_p0001 IS NOT INITIAL.
*          itab_datatable-ename  = itab_p0001-ename.
*        ENDIF.
**      WAGE TYPE AND ITS DESCRIPTION
*        itab_datatable-lgart = '4015'.
*
*        CLEAR itab_t512t.
*        READ TABLE itab_t512t WITH KEY lgart = '4015'.
*        IF itab_t512t IS NOT INITIAL.
*          itab_datatable-lgart_txt = itab_t512t-lgtxt.
*        ENDIF.
**       PICK THE COMPANY CODE AND BUSINESS AREA LOCATION  GRADE
*        LOOP AT itab_p0001_claim WHERE pernr EQ itab_p0000-pernr
*          AND begda LE w_for_period AND endda GE w_for_period.
*          itab_datatable-bukrs = itab_p0001_claim-bukrs.
*          itab_datatable-persa = itab_p0001_claim-werks.
*          itab_datatable-persk = itab_p0001_claim-persk.
*          itab_datatable-btrtl = itab_p0001_claim-btrtl.
*          READ TABLE itab_t001p WITH KEY werks = itab_datatable-persa
*                                   btrtl = itab_datatable-btrtl.
*          itab_datatable-btrtl_txt = itab_t001p-btext.
*        ENDLOOP.
**       PICK THE AMOUNT
*        itab_datatable-betrg = itab_headr-apamt..
*
**       FILL IN PERIOD
*        itab_datatable-ipend = pn-endda.
*        PERFORM get_period USING itab_datatable-ipend
*              CHANGING itab_datatable-inper.
**       FILL FOR PERIOD
*        itab_datatable-paydt =  w_for_period.
*        PERFORM get_period USING w_for_period
*              CHANGING itab_datatable-fpper.
*
*        COLLECT itab_datatable.
*        CLEAR itab_datatable.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
***EOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 18.05.2020, Charm No #4000002138

***  SPECTACLES  REIMBURSEMENT

***SOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 18.05.2020, Charm No #4000002138
*Remarks: The below code has been commented out because a new logic has been written
*to fetch amount of WT - 4095. This block of code was executing and appending the
*record with incorrect value.
*  LOOP AT  itab_p0015_claim WHERE pernr = itab_p0000-pernr
*          AND subty = '4095' AND begda GE '20110214'
*          AND begda GE pn-begda AND begda LE pn-endda.
*
*    CLEAR: w_retro_consider,w_for_period.
*    CLEAR itab_headr.
*    REFRESH itab_headr.
*    CLEAR itab_haedc.
*    REFRESH itab_haedc.
*    w_refnr = itab_p0015_claim-zuord+0(13).
*    CALL FUNCTION 'HRPBSINCLAIMS_CLS_READ'
*      EXPORTING
*        ifd_refnr = w_refnr
**       IFD_RFDAT = ' '
*      IMPORTING
**       EST_TRANS = ITAB_TRANS
*        est_headr = itab_headr
**       ETB_ADREP =
**       ETB_DEPDT =
**       ETB_RQRMK =
**       ETB_FDATA =
**       ETB_INFDT =
*        est_haedc = itab_haedc
*        etb_mlcnt = wa_mlcnt
**       EFD_REFNR =
**       EFD_ERRFG =
**       ETB_ERRTB =
*      .
*    IF wa_mlcnt IS NOT INITIAL.
*      CLEAR: itab_mlcnt,itab_mlcnt[].
*      APPEND LINES OF wa_mlcnt TO itab_mlcnt.
*      IF itab_mlcnt[] IS NOT INITIAL.
*        LOOP AT itab_mlcnt.
*          CLEAR: w_for_period .
*          IF itab_mlcnt-cdt01+0(6) NE pn-endda+0(6) .
*
*            CALL FUNCTION 'HR_HCP_GET_LAST_DAY_OF_MONTH'
*              EXPORTING
*                im_date              = itab_mlcnt-cdt01
*              IMPORTING
*                ex_last_day_of_month = w_for_period.
*
**    FILL THE INTERNAL TABLE
*            itab_datatable-pernr = itab_p0000-pernr.
*            READ TABLE itab_p0001 WITH KEY pernr = itab_p0000-pernr.
*            IF itab_p0001 IS NOT INITIAL.
*              itab_datatable-ename  = itab_p0001-ename.
*            ENDIF.
**      WAGE TYPE AND ITS DESCRIPTION
*            itab_datatable-lgart = '4095'.
*
*            CLEAR itab_t512t.
*            READ TABLE itab_t512t WITH KEY lgart = '4095'.
*            IF itab_t512t IS NOT INITIAL.
*              itab_datatable-lgart_txt = itab_t512t-lgtxt.
*            ENDIF.
**       PICK THE COMPANY CODE AND BUSINESS AREA LOCATION  GRADE
*            LOOP AT itab_p0001_claim WHERE pernr EQ itab_p0000-pernr
*              AND begda LE w_for_period AND endda GE w_for_period.
*              itab_datatable-bukrs = itab_p0001_claim-bukrs.
*              itab_datatable-persa = itab_p0001_claim-werks.
*              itab_datatable-persk = itab_p0001_claim-persk.
*              itab_datatable-btrtl = itab_p0001_claim-btrtl.
*              READ TABLE itab_t001p WITH KEY werks = itab_datatable-persa
*                                       btrtl = itab_datatable-btrtl.
*              itab_datatable-btrtl_txt = itab_t001p-btext.
*            ENDLOOP.
**       PICK THE AMOUNT
*            itab_datatable-betrg =  itab_mlcnt-apamt  .
**       FILL IN PERIOD
*            itab_datatable-ipend = pn-endda.
*            PERFORM get_period USING itab_datatable-ipend
*                  CHANGING itab_datatable-inper.
**       FILL FOR PERIOD
*            itab_datatable-paydt =  w_for_period.
*            PERFORM get_period USING w_for_period
*                  CHANGING itab_datatable-fpper.
*
*            COLLECT itab_datatable.
*            CLEAR itab_datatable.
*
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
***SOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 18.05.2020, Charm No #4000002138

***  MEDICAL REIMBURSEMENT
  LOOP AT  itab_p0267 WHERE pernr = itab_p0000-pernr
          AND subty = '9ZMR' AND begda GE '20110214'
          AND begda GE pn-begda AND begda LE pn-endda.

    CLEAR: w_retro_consider,w_for_period.
    CLEAR itab_headr.
    REFRESH itab_headr.
    CLEAR itab_haedc.
    REFRESH itab_haedc.
    w_refnr = itab_p0267-zuord+0(13).
    CALL FUNCTION 'HRPBSINCLAIMS_CLS_READ'
      EXPORTING
        ifd_refnr = w_refnr
*       IFD_RFDAT = ' '
      IMPORTING
*       EST_TRANS = ITAB_TRANS
        est_headr = itab_headr
*       ETB_ADREP =
*       ETB_DEPDT =
*       ETB_RQRMK =
*       ETB_FDATA =
*       ETB_INFDT =
        est_haedc = itab_haedc
        etb_mlcnt = wa_mlcnt
*       EFD_REFNR =
*       EFD_ERRFG =
*       ETB_ERRTB =
      .
    IF wa_mlcnt IS NOT INITIAL.
      CLEAR: itab_mlcnt,itab_mlcnt[].
      APPEND LINES OF wa_mlcnt TO itab_mlcnt.
      IF itab_mlcnt[] IS NOT INITIAL.
        LOOP AT itab_mlcnt.
          CLEAR: w_for_period .
          IF itab_mlcnt-cdt01+0(6) NE pn-endda+0(6) .

            CALL FUNCTION 'HR_HCP_GET_LAST_DAY_OF_MONTH'
              EXPORTING
                im_date              = itab_mlcnt-cdt01
              IMPORTING
                ex_last_day_of_month = w_for_period.

*    FILL THE INTERNAL TABLE
            itab_datatable-pernr = itab_p0000-pernr.
            READ TABLE itab_p0001 WITH KEY pernr = itab_p0000-pernr.
            IF itab_p0001 IS NOT INITIAL.
              itab_datatable-ename  = itab_p0001-ename.
            ENDIF.
*      WAGE TYPE AND ITS DESCRIPTION
            itab_datatable-lgart = '9ZMR'.
*            CLEAR ITAB_T512T.
*            READ TABLE ITAB_T512T WITH KEY LGART = '4095'.
*            IF ITAB_T512T IS NOT INITIAL.
*              ITAB_DATATABLE-LGART_TXT = ITAB_T512T-LGTXT.
*            ENDIF.
            itab_datatable-lgart_txt = 'MEDICAL PAYMENTS'.

*       PICK THE COMPANY CODE AND BUSINESS AREA LOCATION  GRADE
            LOOP AT itab_p0001_claim WHERE pernr EQ itab_p0000-pernr
              AND begda LE w_for_period AND endda GE w_for_period.
              itab_datatable-bukrs = itab_p0001_claim-bukrs.
              itab_datatable-persa = itab_p0001_claim-werks.
              itab_datatable-persk = itab_p0001_claim-persk.
              itab_datatable-btrtl = itab_p0001_claim-btrtl.
              READ TABLE itab_t001p WITH KEY werks = itab_datatable-persa
                                       btrtl = itab_datatable-btrtl.
              itab_datatable-btrtl_txt = itab_t001p-btext.
            ENDLOOP.
*       PICK THE AMOUNT
            itab_datatable-betrg =  itab_mlcnt-apamt  .
*       FILL IN PERIOD
            itab_datatable-ipend = pn-endda.
            PERFORM get_period USING itab_datatable-ipend
                  CHANGING itab_datatable-inper.
*       FILL FOR PERIOD
            itab_datatable-paydt =  w_for_period.
            PERFORM get_period USING w_for_period
                  CHANGING itab_datatable-fpper.

            COLLECT itab_datatable.
            CLEAR itab_datatable.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR: itab_mlcnt,itab_mlcnt[].
ENDFORM.                    " GET_CLUSTER_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_PERIOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_FOR_PERIOD  text
*      <--P_ITAB_DATATABLE_FPPER  text
*----------------------------------------------------------------------*
FORM get_period  USING    p_w_for_period
                 CHANGING p_itab_datatable_fpper.
  DATA: l_month(2),l_year(4),len TYPE i.
  l_month = p_w_for_period+4(2).
  l_year = p_w_for_period+0(4).
  IF l_month LT 3 .
    l_year = l_year -  1 .
    l_month = 12 + l_month - 3.
  ELSEIF l_month EQ 3.
    l_year = l_year -  1 .
    l_month = '12' .
  ELSE.
    l_month = l_month - 3 .
    len = strlen( l_month ).
    IF len EQ 1 .
      CONCATENATE '0' l_month INTO l_month.
    ENDIF.

  ENDIF.
  CONCATENATE l_year l_month INTO p_itab_datatable_fpper .
ENDFORM.                    " GET_PERIOD

FORM on_selection USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  DATA: itab TYPE TABLE OF sy-ucomm.
*PERFORM USER_COMMAND USING sy-ucomm RS_SELFIELD TYPE SLIS_SELFIELD.
  APPEND '&ALL' TO itab.
  APPEND '&SAL' TO itab.

  CASE r_ucomm.

    WHEN 'FCOD'."

      SET PF-STATUS 'STANDARD' OF PROGRAM 'YRHR001_CTC_REPORT'.

      PERFORM debit_bdc.
  ENDCASE.
ENDFORM.

FORM debit_bdc.
  DATA : sy_datum(10), w_cnt(3) TYPE n, w_amt(16), w_txt(50), month(3), w_txt1(50),
  s_date(10).

  DESCRIBE TABLE itab_ctc_smry LINES w_cnt.

  CONCATENATE p_budat+6(2) '.' p_budat+4(2) '.' p_budat(4) INTO s_date.

  CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4) INTO sy_datum.
  SELECT pernr kostl FROM pa0001 INTO CORRESPONDING FIELDS OF TABLE i_pa0001 FOR ALL ENTRIES IN
    itab_ctc_smry WHERE pernr = itab_ctc_smry-yy_pernr AND begda <= p_budat AND endda >= p_budat.



  PERFORM bdc_dynpro  USING 'SAPMF05A' '0100'.
  PERFORM bdc_field   USING 'BDC_CURSOR' 'RF05A-NEWKO'.
  PERFORM bdc_field   USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field   USING 'BKPF-BLDAT' sy_datum.
  PERFORM bdc_field   USING 'BKPF-BLART' 'SA'.
  PERFORM bdc_field   USING 'BKPF-BUKRS' p_bukrs. "'1000'.
  PERFORM bdc_field   USING 'BKPF-BUDAT' s_date. "P_BUDAT. "'03.02.2012'.
*    PERFORM bdc_field   USING 'BKPF-MONAT' '11'.
  PERFORM bdc_field   USING 'BKPF-WAERS' 'INR'.
  PERFORM bdc_field   USING 'FS006-DOCID' '*'.
  PERFORM bdc_field   USING 'RF05A-NEWBS' '50'.
  PERFORM bdc_field   USING 'RF05A-NEWKO' '6111010'.
  PERFORM bdc_field   USING 'BDC_SUBSCR' 'SAPMF05A'.
  PERFORM bdc_field   USING 'BDC_SUBSCR' 'SAPLSEXM'.

  LOOP AT itab_ctc_smry.
    CLEAR w_amt.
    CLEAR w_txt.

    w_amt = itab_ctc_smry-yy_total_ctc.
    CONDENSE w_amt.

    CASE itab_ctc_smry-yy_start_period+4(2) .
      WHEN '01'.
        month = 'Jan'.
      WHEN '02' .
        month = 'Feb'.
      WHEN '03'.
        month = 'Mar'.
      WHEN '04' .
        month = 'APR'.
      WHEN '05'.
        month = 'May'.
      WHEN '06' .
        month = 'Jun'.
      WHEN '07'.
        month = 'Jul'.
      WHEN '08' .
        month = 'Aug'.
      WHEN '09'.
        month = 'Sept'.
      WHEN '10' .
        month = 'Oct'.
      WHEN '11'.
        month = 'Nov'.
      WHEN '12' .
        month = 'Dec'.
    ENDCASE.

    CONCATENATE 'Salary debit note CPF' itab_ctc_smry-yy_pernr 'for' month
    itab_ctc_smry-yy_start_period(4) INTO w_txt SEPARATED BY space.

    CONCATENATE 'Salary debit note' 'for' month
    itab_ctc_smry-yy_start_period(4) INTO w_txt1 SEPARATED BY space.

    READ TABLE i_pa0001 WITH KEY pernr = itab_ctc_smry-yy_pernr.
    IF sy-subrc = 0.
      IF sy-tabix <> w_cnt.
        PERFORM bdc_dynpro  USING 'SAPMF05A' '300'.
        PERFORM bdc_field   USING 'BDC_CURSOR' 'RF05A-NEWKO'.
        PERFORM bdc_field   USING 'BDC_OKCODE' '/00'.
        PERFORM bdc_field   USING 'BSEG-WRBTR' w_amt.
        PERFORM bdc_field   USING 'BSEG-SGTXT' w_txt.
        PERFORM bdc_field   USING 'RF05A-NEWBS' '50'.
        PERFORM bdc_field   USING 'RF05A-NEWKO' '6111010'.
        PERFORM bdc_field   USING 'BDC_SUBSCR' 'SAPLKACB'.

        PERFORM bdc_dynpro  USING 'SAPLKACB' '0002'.
        PERFORM bdc_field   USING 'BDC_CURSOR' 'COBL-KOSTL'.
        PERFORM bdc_field   USING 'BDC_OKCODE' '=ENTE'.
*        PERFORM bdc_field   USING 'COBL-GSBER' '1015'.
        PERFORM bdc_field   USING 'COBL-KOSTL' i_pa0001-kostl. "'CPND1150'.
        PERFORM bdc_field   USING 'BDC_SUBSCR' 'SAPLKACB'.
      ENDIF.

      IF sy-tabix = w_cnt.
        PERFORM bdc_dynpro  USING 'SAPMF05A' '300'.
        PERFORM bdc_field   USING 'BDC_CURSOR' 'RF05A-NEWKO'.
        PERFORM bdc_field   USING 'BDC_OKCODE' '/00'.
        PERFORM bdc_field   USING 'BSEG-WRBTR' w_amt.
        PERFORM bdc_field   USING 'BSEG-SGTXT' w_txt.
        PERFORM bdc_field   USING 'RF05A-NEWBS' '40'.
        PERFORM bdc_field   USING 'RF05A-NEWKO' p_glcode.
        PERFORM bdc_field   USING 'BDC_SUBSCR' 'SAPLKACB'.

        PERFORM bdc_dynpro  USING 'SAPLKACB' '0002'.
        PERFORM bdc_field   USING 'BDC_CURSOR' 'COBL-KOSTL'.
        PERFORM bdc_field   USING 'BDC_OKCODE' '=ENTE'.
*          PERFORM bdc_field   USING 'COBL-GSBER' '1015'.
        PERFORM bdc_field   USING 'COBL-KOSTL' i_pa0001-kostl. "'CPND1150'.
        PERFORM bdc_field   USING 'BDC_SUBSCR' 'SAPLKACB'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  PERFORM bdc_dynpro  USING 'SAPMF05A' '300'.
  PERFORM bdc_field   USING 'BDC_CURSOR' 'BSEG-SGTXT'.
  PERFORM bdc_field   USING 'BDC_OKCODE' '=AB'.
  PERFORM bdc_field   USING 'BSEG-WRBTR' '*'.
  PERFORM bdc_field   USING 'BSEG-SGTXT' w_txt1.
*    PERFORM bdc_field   USING 'RF05A-NEWBS' '50'.
*    PERFORM bdc_field   USING 'RF05A-NEWKO' '6111010'.
  PERFORM bdc_field   USING 'BDC_SUBSCR' 'SAPLKACB'.

  PERFORM bdc_dynpro  USING 'SAPLKACB' '0002'.
  PERFORM bdc_field   USING 'BDC_CURSOR' 'COBL-GSBER'.
  PERFORM bdc_field   USING 'BDC_OKCODE' '=ENTE'.
  PERFORM bdc_field   USING 'COBL-GSBER' p_gsber . "'1015'.
*    PERFORM bdc_field   USING 'COBL-KOSTL' 'CPND1150'.
  PERFORM bdc_field   USING 'BDC_SUBSCR' 'SAPLKACB'.

  PERFORM bdc_dynpro  USING 'SAPMF05A' '0700'.
  PERFORM bdc_field   USING 'BDC_CURSOR' 'RF05A-NEWBS'.
  PERFORM bdc_field   USING 'BDC_OKCODE' '=BU'.

  CALL TRANSACTION 'F-02' USING i_bdcdata
                             MODE   p_mode
*                            update P_UPDATE
                             MESSAGES INTO messtab.
  REFRESH i_bdcdata.

  READ TABLE messtab WITH KEY msgtyp = 'E'.
*    read table MESSTAB .

  IF sy-subrc NE 0.

  ELSEIF sy-subrc = 0.
    w_no = messtab-msgnr.
    LOOP AT messtab.
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = messtab-msgid
          lang      = sy-langu
          no        = w_no
          v1        = messtab-msgv1
          v2        = messtab-msgv2
          v3        = messtab-msgv3
          v4        = messtab-msgv4
        IMPORTING
          msg       = i_error-msg
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc <> 0.
*                   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      APPEND i_error.
      CLEAR  i_error.
    ENDLOOP.
  ENDIF.
ENDFORM.

FORM bdc_dynpro USING program dynpro.
  CLEAR i_bdcdata.
  i_bdcdata-program  = program.
  i_bdcdata-dynpro   = dynpro.
  i_bdcdata-dynbegin = 'X'.
  APPEND i_bdcdata.
ENDFORM.                    "bdc_dynpro

*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FNAM       text
*      -->FVAL       text
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR i_bdcdata.
  i_bdcdata-fnam = fnam.
  i_bdcdata-fval = fval.
  APPEND i_bdcdata.
ENDFORM.                    "bdc_field

*FORM GAIL_GGL.
*  READ TABLE ITAB_P0001 WITH KEY PERNR = ITAB_P0000-PERNR YY_ORG = 'GGL'.
*  IF SY-SUBRC = 0.
*    DELETE ITAB_FINAL_D WHERE YY_PERNR = ITAB_P0000-PERNR AND ( YY_LGTXT = 'PRP' OR YY_LGTXT = 'TOKEN REWARD' ).
*  ENDIF.
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_TOKEN_REWARD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_token_reward .

  DATA: w_cyear(4) TYPE c,
*      W_CMONTH(2) TYPE C,
        w_pyear(4) TYPE c,
*      W_PMONTH(2) TYPE C,
        w_year(4)  TYPE c,
        w_month(2) TYPE c.

  w_cyear = pnpdisbd+0(4).
*W_CMONTH = PNPDISBD+4(2).

  w_pyear = w_cyear - 1.
*W_PMONTH = PNPDISBD+4(2).

  CLEAR itab_final_d-yy_sno.
  CLEAR itab_final_d-yy_betrg.
  CLEAR itab_final_d-yy_lgart.
  CLEAR itab_final_d-yy_lgtxt.
  CLEAR itab_final_d-yy_start_period.
  CLEAR itab_final_d-yy_end_period.
  CLEAR itab_final_d-yy_fpper.
  CLEAR itab_final_d-yy_fpbeg.
  CLEAR itab_final_d-yy_fpend.
  CLEAR itab_final_d-yy_werks.
  CLEAR itab_final_d-yy_btrtl.

  itab_final_d-yy_start_period = itab_final_h-yy_start_period.
  itab_final_d-yy_end_period = itab_final_h-yy_end_period.
  itab_final_d-yy_fpper = itab_final_h-yy_fpper.
  itab_final_d-yy_fpbeg = itab_final_h-yy_fpbeg.
  itab_final_d-yy_fpend = itab_final_h-yy_fpend.
  itab_final_d-yy_werks = itab_final_h-yy_werks.
  itab_final_d-yy_btrtl = itab_final_h-yy_btrtl.
  itab_final_d-yy_lgart = '1261'.

***TOKEN REWARD
  CLEAR itab_t512t.
  READ TABLE itab_t512t WITH KEY lgart = '1261'.      "DIWALI GIFT
  itab_final_d-yy_lgtxt = itab_t512t-lgtxt.

  CLEAR itab_p0267.
  SORT itab_p0267 BY begda DESCENDING.
  READ TABLE itab_p0267 WITH KEY pernr = itab_final_h-yy_pernr
                                 subty = '1261'
                                 lgart = '1261'.
  w_year = itab_p0267-begda+0(4).
  w_month = itab_p0267-begda+4(2).
  IF ( w_month <= '3' AND w_year = w_cyear ) OR
     ( w_month >= '4' AND w_year = w_pyear ).

    LOOP AT itab_p0267 WHERE pernr = itab_final_h-yy_pernr AND
                             subty = '1261' AND lgart = '1261'.
      IF itab_p0267-begda <= itab_final_d-yy_fpend AND
        itab_final_d-yy_betrg IS INITIAL.
***SOC BY SANYOGITA & BHARAT ON 06.12.2013 FOR REMOVING ADDITION OF 10% IN CALCULATIOM
***      ITAB_FINAL_D-YY_BETRG = ( ITAB_P0267-BETRG +
***                              ( ITAB_P0267-BETRG * 10 / 100 ) ) / 12.
        itab_final_d-yy_betrg = itab_p0267-betrg / 12.
***EOC BY SANYOGITA & BHARAT ON 06.12.2013 FOR REMOVING ADDITION OF 10% IN CALCULATIOM
        IF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
           itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
          itab_final_d-yy_betrg =
                ( itab_final_d-yy_betrg / itab_final_h-yy_py_prd_days ) *
                  itab_final_h-yy_prd_days.
        ENDIF.
        itab_final_d-yy_ben_type = 4.
        itab_final_d-yy_ben_typ_txt = 'INCENTIVES'.
        IF itab_final_d-yy_betrg <> 0.
          APPEND itab_final_d.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF itab_final_d-yy_betrg IS INITIAL.
    CLEAR itab_p0015.
    SORT itab_p0015 BY begda DESCENDING.
    READ TABLE itab_p0015 WITH KEY pernr = itab_final_h-yy_pernr
                                   subty = '1261'
                                   lgart = '1261'.
    w_year = itab_p0015-begda+0(4).
    w_month = itab_p0015-begda+4(2).
    IF ( w_month <= '3' AND w_year = w_cyear ) OR
       ( w_month >= '4' AND w_year = w_pyear ).

      LOOP AT itab_p0015 WHERE pernr = itab_final_h-yy_pernr AND
                               subty = '1261' AND lgart = '1261'.
        IF itab_p0015-begda <= itab_final_d-yy_fpend AND
           itab_final_d-yy_betrg IS INITIAL.
***SOC BY SANYOGITA & BHARAT ON 06.12.2013 FOR REMOVING ADDITION OF 10% IN CALCULATIOM
***        ITAB_FINAL_D-YY_BETRG = ( ITAB_P0015-BETRG +
***                                ( ITAB_P0015-BETRG * 10 / 100 ) ) / 12.
          itab_final_d-yy_betrg = itab_p0015-betrg / 12.
***EOC BY SANYOGITA & BHARAT ON 06.12.2013 FOR REMOVING ADDITION OF 10% IN CALCULATIOM
          IF itab_final_d-yy_start_period <> itab_final_d-yy_fpbeg OR
             itab_final_d-yy_end_period <> itab_final_d-yy_fpend.
            itab_final_d-yy_betrg =
                  ( itab_final_d-yy_betrg / itab_final_h-yy_py_prd_days ) *
                    itab_final_h-yy_prd_days.
          ENDIF.
          itab_final_d-yy_ben_type = 4.
          itab_final_d-yy_ben_typ_txt = 'INCENTIVES'.
          IF itab_final_d-yy_betrg <> 0.
            APPEND itab_final_d.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_TOKEN_REWARD

***SOC by Abhinesh Sinha & Bibhu Ranjan Sahoo on 06.05.2020, Charm No #4000001849
*Remarks: A new subroutine has been created to implement logic for
*1. Telephone call charges reimbursement
*2. Local conveyance reimbursement
*3. News paper reimbusrement residence
*4. News paper reimbursement office
*5. Briefcase reimbursement
*6. Contact lense reimbursement
FORM get_reimbursement_data.
*  " Logic for HIRE PURCHASE
  DATA: l_amount     TYPE yy_enamt,
        l_amount_tmp TYPE yy_enamt,
        l_factor     TYPE yelig.

  " IT9109-01 WT 4237 Entitlement
  CLEAR: wa_entl, wa_calc, l_amount, l_factor.
  LOOP AT itab_p0001_claim WHERE pernr EQ itab_p0000-pernr  AND begda LE wa_period-endda AND endda GE wa_period-endda. " For grade.
  ENDLOOP.

*  READ TABLE i_entl INTO wa_entl WITH KEY yy_endda = '99991231' persk = itab_p0001_claim-persk.
  LOOP AT i_entl INTO wa_entl WHERE yy_begda LE wa_period-endda AND yy_endda GE wa_period-endda AND persk = itab_p0001_claim-persk AND
    persg EQ itab_p0001_claim-persg.
    l_amount = wa_entl-yy_enamt.
  ENDLOOP.

  LOOP AT i_calc INTO wa_calc WHERE start_date LE wa_period-endda AND  end_date GE wa_period-endda AND wage_type = '4237'.
    l_factor = wa_calc-eligibility.
    l_amount = l_amount / l_factor.
  ENDLOOP.

  CLEAR itab_final_d-yy_sno.
  CLEAR itab_final_d-yy_lgart.
  CLEAR itab_final_d-yy_betrg.
  CLEAR itab_final_d-yy_lgtxt.
  CLEAR itab_final_d-yy_start_period.
  CLEAR itab_final_d-yy_end_period.
  CLEAR itab_final_d-yy_fpper.
  CLEAR itab_final_d-yy_fpbeg.
  CLEAR itab_final_d-yy_fpend.
  CLEAR itab_final_d-yy_werks.
  CLEAR itab_final_d-yy_btrtl.

  itab_final_d-yy_pernr        = itab_final_h-yy_pernr.
  itab_final_d-yy_start_period = itab_final_h-yy_start_period.
  itab_final_d-yy_end_period   = itab_final_h-yy_end_period.
  itab_final_d-yy_fpper        = itab_final_h-yy_fpper.
  itab_final_d-yy_fpbeg        = itab_final_h-yy_fpbeg.
  itab_final_d-yy_fpend        = itab_final_h-yy_fpend.
  itab_final_d-yy_werks        = itab_final_h-yy_werks.
  itab_final_d-yy_btrtl        = itab_final_h-yy_btrtl.

  IF l_amount IS NOT INITIAL.
    itab_final_d-yy_lgart = '4237'.
    itab_final_d-yy_betrg = l_amount.
    itab_final_d-yy_lgtxt = 'FURNISHING ITEMS AT RESIDENCE'.
    itab_final_d-yy_ben_type = 3.
    itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
    APPEND itab_final_d.
    CLEAR: itab_final_d-yy_betrg.
  ENDIF.

  "   " IT9109-02 & 04 WT 4037 / 4038 Entitlement
  CLEAR: l_amount, l_factor.
  l_amount = wa_entl-yy_enamt2.
  LOOP AT i_calc INTO wa_calc WHERE start_date LE wa_period-endda AND  end_date GE wa_period-endda AND wage_type = '4037'.
    l_factor = wa_calc-eligibility.
    l_amount = l_amount / l_factor.
  ENDLOOP.

  CLEAR: l_factor, l_amount_tmp.
  l_amount_tmp = l_amount.
  l_amount = wa_entl-yy_enamt4.
  LOOP AT i_calc INTO wa_calc WHERE start_date LE wa_period-endda AND  end_date GE wa_period-endda AND wage_type = '4038'.
    l_factor = wa_calc-eligibility.
    l_amount = l_amount / l_factor.
  ENDLOOP.

  l_amount = l_amount + l_amount_tmp.

  IF l_amount IS NOT INITIAL.
    itab_final_d-yy_lgart = '4037'.
    itab_final_d-yy_betrg = l_amount.
    itab_final_d-yy_lgtxt = 'PC/PERIPHERALS AT RESIDENCE'.
    itab_final_d-yy_ben_type = 3.
    itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
    APPEND itab_final_d.
    CLEAR: itab_final_d-yy_betrg.
  ENDIF.

  "   " IT9109-03 WT 4238 Entitlement
  CLEAR: l_amount, l_factor.
  l_amount = wa_entl-yy_enamt3.
  LOOP AT i_calc INTO wa_calc WHERE start_date LE wa_period-endda AND  end_date GE wa_period-endda AND wage_type = '4238'.
    l_factor = wa_calc-eligibility.
    l_amount = l_amount / l_factor.
  ENDLOOP.

  IF l_amount IS NOT INITIAL.
    itab_final_d-yy_lgart = '4238'.
    itab_final_d-yy_betrg = l_amount.
    itab_final_d-yy_lgtxt = 'FURNISHING ITEMS OFFICER AT RESIDENCE'.
    itab_final_d-yy_ben_type = 3.
    itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
    APPEND itab_final_d.
    CLEAR: itab_final_d-yy_betrg.
  ENDIF.

  "   " IT9109-05 WT 4016 Entitlement
  CLEAR: l_amount, l_factor.
  l_amount = wa_entl-yy_curr_enamt.
  LOOP AT i_calc INTO wa_calc WHERE start_date LE wa_period-endda AND  end_date GE wa_period-endda AND wage_type = '4016'.
    l_factor = wa_calc-eligibility.
    l_amount = l_amount / l_factor.
  ENDLOOP.

  IF l_amount IS NOT INITIAL.
    itab_final_d-yy_lgart = '4016'.
    itab_final_d-yy_betrg = l_amount.
    itab_final_d-yy_lgtxt = 'CELL PHONE INSTRUMENT REIMBURSEMENT'.
    itab_final_d-yy_ben_type = 3.
    itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
    APPEND itab_final_d.
    CLEAR: itab_final_d-yy_betrg.
  ENDIF.

  " Maintenance IT9109-01 WT 4235.
  CLEAR: l_amount, l_amount_tmp.
  l_amount = wa_entl-yy_enamt.
  l_amount_tmp = ( ( ( l_amount * 10 ) / 100 ) / 12 ).

  IF l_amount_tmp IS NOT INITIAL.
    itab_final_d-yy_lgart = '4235'.
    itab_final_d-yy_betrg = l_amount_tmp.
    itab_final_d-yy_lgtxt = 'FURNITURE MAINTENANCE CHARGE'.
    itab_final_d-yy_ben_type = 3.
    itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
    APPEND itab_final_d.
    CLEAR: itab_final_d-yy_betrg.
  ENDIF.

*--> Logic for Telephone call charges reimbursement - WT 4015 SOC
  DATA: lv_rtalgrp TYPE t7ina1-algrp.

  CLEAR: lv_rtalgrp.
  CALL FUNCTION 'HR_IN_GET_ALGRP'
    EXPORTING
      empno           = itab_p0000-pernr
      rfdate          = pn-endda
    IMPORTING
      rtalgrp         = lv_rtalgrp
    EXCEPTIONS
      algrp_not_found = 1
      OTHERS          = 2.

  IF lv_rtalgrp IS NOT INITIAL.
    LOOP AT gt_t7ina9 ASSIGNING FIELD-SYMBOL(<lfs_t7ina9>)
                                       WHERE algrp = lv_rtalgrp.
      IF <lfs_t7ina9> IS ASSIGNED.

        IF <lfs_t7ina9>-lgart = '4035'.
          DATA(lv_amunta) = <lfs_t7ina9>-amunt.
          lv_amunta = lv_amunta / 12.

          itab_final_d-yy_lgart = <lfs_t7ina9>-lgart.
          itab_final_d-yy_betrg = lv_amunta.

          itab_final_d-yy_lgtxt = 'PC MAINTENANCE AND INTERNET CHARGES'.
          itab_final_d-yy_ben_type = 3.
          itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
          CLEAR: lv_amunta.
        ENDIF.

*        IF <lfs_t7ina9>-lgart = '4015'.
*          DATA(lv_amunt1) = <lfs_t7ina9>-amunt.
*          lv_amunt1 = lv_amunt1 / 12.
*
*          itab_final_d-yy_lgart = <lfs_t7ina9>-lgart.
*          itab_final_d-yy_betrg = lv_amunt1.
*
*          itab_final_d-yy_lgtxt = 'TEL/CELLULAR FACILITY'.
*          itab_final_d-yy_ben_type = 3.
*          itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
*          CLEAR: lv_amunt1.
*        ENDIF.

*        IF <lfs_t7ina9>-lgart = '4060'.
*          DATA(lv_amunt2) = <lfs_t7ina9>-amunt.
*          lv_amunt2 = lv_amunt2 / 36.
*
*          itab_final_d-yy_lgart = <lfs_t7ina9>-lgart.
*          itab_final_d-yy_betrg = lv_amunt2.
*
*          itab_final_d-yy_lgtxt = 'BRIEFCASE/HANDBAG REIMBURSEMENT'.
*          itab_final_d-yy_ben_type = 3.
*          itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
*          CLEAR: lv_amunt2.
*        ENDIF.

        IF <lfs_t7ina9>-lgart = '4095'.
          DATA(lv_amunt3) = <lfs_t7ina9>-amunt.
          lv_amunt3 = lv_amunt3 / 24.

          itab_final_d-yy_lgart = <lfs_t7ina9>-lgart.
          itab_final_d-yy_betrg = lv_amunt3.

          itab_final_d-yy_lgtxt = 'SPECS/CONTACT LENSE REIMBURSEMENT'.
          itab_final_d-yy_ben_type = 3.
          itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
          CLEAR: lv_amunt3.
        ENDIF.

        IF itab_final_d-yy_betrg IS NOT INITIAL.
          APPEND itab_final_d.
        ENDIF.
        CLEAR itab_final_d-yy_betrg.
      ENDIF.
    ENDLOOP.
  ENDIF.
*--> Logic for Telephone call charges reimbursement - WT 4015 EOC

*--> Logic to read data from IT0267 - WT 4075 SOC
  DATA: lt_p0267 TYPE STANDARD TABLE OF p0267,
        wa_p0267 TYPE p0267.

  REFRESH lt_p0267.
  CALL FUNCTION 'HR_READ_INFOTYPE'
    EXPORTING
*     TCLAS           = 'A'
      pernr           = itab_p0000-pernr
      infty           = '0267'
      begda           = pn-begda
      endda           = pn-endda
    TABLES
      infty_tab       = lt_p0267
    EXCEPTIONS
      infty_not_found = 1
      invalid_input   = 2
      OTHERS          = 3.

  IF lt_p0267 IS NOT INITIAL.
    SORT lt_p0267.

    CLEAR: wa_p0267, itab_final_d-yy_betrg.
    LOOP AT lt_p0267 INTO wa_p0267 WHERE lgart EQ '4015'.
      itab_final_d-yy_betrg = itab_final_d-yy_betrg + wa_p0267-betrg.
    ENDLOOP.

    itab_final_d-yy_lgart = '4015'.
    itab_final_d-yy_lgtxt = 'TEL/CELLULAR FACILITY'.
    itab_final_d-yy_ben_type = 3.
    itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
    IF itab_final_d-yy_betrg IS NOT INITIAL.
      APPEND itab_final_d.
      CLEAR: itab_final_d-yy_betrg.
    ENDIF.

    CLEAR: wa_p0267, itab_final_d-yy_betrg.
    LOOP AT lt_p0267 INTO wa_p0267 WHERE lgart EQ '4075'.
      itab_final_d-yy_betrg = itab_final_d-yy_betrg + wa_p0267-betrg.
    ENDLOOP.

    itab_final_d-yy_lgart = '4075'.
    itab_final_d-yy_lgtxt = 'LOCAL CONVEYANCE REIMBURSEMENT'.
    itab_final_d-yy_ben_type = 3.
    itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
    IF itab_final_d-yy_betrg IS NOT INITIAL.
      APPEND itab_final_d.
      CLEAR: itab_final_d-yy_betrg.
    ENDIF.

    CLEAR: wa_p0267, itab_final_d-yy_betrg.
    LOOP AT lt_p0267 INTO wa_p0267 WHERE lgart EQ '4010'.
      itab_final_d-yy_betrg = itab_final_d-yy_betrg + wa_p0267-betrg.
    ENDLOOP.

    itab_final_d-yy_lgart = '4010'.
    itab_final_d-yy_lgtxt = 'NEWS PAPER REIMBURSEMENT RESIDENCE'.
    itab_final_d-yy_ben_type = 3.
    itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
    IF itab_final_d-yy_betrg IS NOT INITIAL.
      APPEND itab_final_d.
      CLEAR: itab_final_d-yy_betrg.
    ENDIF.

    CLEAR: wa_p0267, itab_final_d-yy_betrg.
    LOOP AT lt_p0267 INTO wa_p0267 WHERE lgart EQ '4011'.
      itab_final_d-yy_betrg = itab_final_d-yy_betrg + wa_p0267-betrg.
    ENDLOOP.

    itab_final_d-yy_lgart = '4011'.
    itab_final_d-yy_lgtxt = 'NEWS PAPER REIMBURSEMENT OFFICE'.
    itab_final_d-yy_ben_type = 3.
    itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
    IF itab_final_d-yy_betrg IS NOT INITIAL.
      APPEND itab_final_d.
      CLEAR: itab_final_d-yy_betrg.
    ENDIF.

    "    " 'BRIEFCASE/HANDBAG REIMBURSEMENT'.
    CLEAR: wa_p0267, itab_final_d-yy_betrg.
    LOOP AT lt_p0267 INTO wa_p0267 WHERE lgart EQ '4060'.
      itab_final_d-yy_betrg = itab_final_d-yy_betrg + wa_p0267-betrg.
    ENDLOOP.

    itab_final_d-yy_lgart = '4060'.
    itab_final_d-yy_lgtxt = 'BRIEFCASE/HANDBAG REIMBURSEMENT'.
    itab_final_d-yy_ben_type = 3.
    itab_final_d-yy_ben_typ_txt = 'PERQUISITES'.
    IF itab_final_d-yy_betrg IS NOT INITIAL.
      APPEND itab_final_d.
      CLEAR: itab_final_d-yy_betrg.
    ENDIF.
  ENDIF.

*ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  AUTH_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM auth_check .
  IF p_bukrs IS NOT INITIAL.
    AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'BUKRS' FIELD p_bukrs
    ID 'ACTVT' FIELD '03'.
    IF sy-subrc <> 0.
      DATA(lv_text) = |You are not Authorized for the company code, { p_bukrs }|.
      MESSAGE lv_text TYPE 'E'.
    ENDIF.
  ENDIF.
  IF p_gsber IS NOT INITIAL.
    AUTHORITY-CHECK OBJECT 'F_BKPF_GSB'
    ID 'GSBER' FIELD p_gsber
    ID 'ACTVT' FIELD '03'.
    IF sy-subrc <> 0.
      lv_text = |You are not Authorized for the BA, { p_gsber }|.
      MESSAGE lv_text TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.