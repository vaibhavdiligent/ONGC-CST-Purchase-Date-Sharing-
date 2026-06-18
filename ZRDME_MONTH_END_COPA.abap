*&---------------------------------------------------------------------*
*& Report  ZRDME_MONTH_END_COPA
*&---------------------------------------------------------------------*
*& Program Name   : ZRDME_MONTH_END_COPA
*& Program Title  : DME Tool - S/4HANA Month-End CO-PA Posting
*& Program Type   : Report (Executable)
*& Transaction    : ZRDME_ACC_DME (to be created)
*& Based on FS     : FS - DME_Month_End_COPA posting_V1.0  (18-May-2026)
*& Reference (ECC) : /CCBJI/RUFIAPR_ACCURAL_DME (Tcode /CCBJI/RURAP_ACC_DME)
*& Scope          : ECC 6.0 -> S/4HANA Transformation - Lift Phase
*& Priority Scope : Spot Pay (Phase 1). Pre-Pay / Post-Pay / Regular Pay /
*&                  Post-Pay Secondary are routed but deferred to Phase 2.
*&---------------------------------------------------------------------*
*& TO-BE KEY CHANGES vs AS-IS (per FS sections 1.3, 3, 4)
*&   1) Contract object backend migrated Rebate Agreement (KONA) ->
*&      CCM Condition Contract (WCOCOH/KONP). Contract attributes are now
*&      read from WCOCOH (sales office WCOCOH-VKBUR, bottler account
*&      WCOCOH-ZZBOTACC, dealer WCOCOH-CUST_OWNER).
*&   2) GL determination read from ZDME_GL_MAPPING, extended with a new
*&      Account-Based CO-PA GL account column (GL_ACCT_ACCTBSD).
*&   3) Sales data is fetched through a CDS view (ZC_DME_SALES_COPA) that
*&      sources CE2JP00 (till Nov-2027) and ACDOCA (from Dec-2027) instead
*&      of a direct SELECT on CE1JP00 / CE2JP00.
*&   4) Both Costing-Based CO-PA and Account-Based CO-PA are posted in the
*&      same run via BAPI_ACC_DOCUMENT_POST (profitability segment passed
*&      through the CRITERIA table).  [Technical feasibility item #1 - FS 11]
*&   5) Dummy sales: no more "1.01" write-back to CE2JP00; a dummy material
*&      is used as the allocation basis when no actual sales exist.
*&---------------------------------------------------------------------*
*& OPEN / FEASIBILITY ITEMS (FS section 11) - flagged inline with "TODO-FS"
*&   - Confirm BAPI_ACC_DOCUMENT_POST posts CB + AB CO-PA in one call when
*&     operating concern JP00 has both types active.
*&   - Confirm AB op. concern carries the custom characteristics
*&     (VKAUS, WW228, WW229, KUNWE) - FS section 9.2.
*&   - Finance/CO to provide AB GL accounts for ZDME_GL_MAPPING - FS 9.3.
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& EXTERNAL FIELD-NAME MAPPING  —  SINGLE EDIT POINT (T1 / T2)
*&---------------------------------------------------------------------*
*& The following customer-append field names are NOT yet confirmed and
*& are the only items that block activation. When confirmed, change them
*& here (program) and in ZC_DME_SALES_COPA (CDS) — both files carry a
*& matching banner. Nothing else needs to change.
*&
*&  T1 - WCOCOH append (confirm with Pankaj-san) — used in F_GET_DATA:
*&         bottler account : WCOCOH-ZZBOTACC      <-- edit in SELECT
*&         dealer / owner  : WCOCOH-CUST_OWNER    <-- edit in SELECT
*&  T2 - CI_ACDOCA append (confirm with Gaurav-san) — used in CDS view:
*&         WW207 / WW214 / WW228 / WW229 / VKAUS  <-- edit in CDS branch 2
*&---------------------------------------------------------------------*
REPORT zrdme_month_end_copa MESSAGE-ID /ccbji/rtr.

*&---------------------------------------------------------------------*
*&  TOP - Type / Data / Constant declarations
*&  (mirrors include /CCBJI/RUFIAPR_DME_TOP, reduced to Spot-Pay scope)
*&---------------------------------------------------------------------*

* ---- DME contract header (custom Z-table /CCBJI/T_DME_HDR - unchanged) -
TYPES: BEGIN OF ty_dhdr,
         knuma        TYPE knuma,                     "CCM contract no.
         bukrs        TYPE bukrs,
         pay_type     TYPE /ccbji/dme_paytyp,
         cal_type     TYPE /ccbji/dme_cal_typ,
         boart        TYPE boart,
         start_date   TYPE oij_schstrdt,
         cont_amnt    TYPE /ccbji/otc_contamt,
         cont_vol     TYPE /ccbji/otc_contqty,
         waers        TYPE waers,
         compl        TYPE /scl/otc_lsa_compl,
         tax_ind      TYPE /ccbji/otc_taxin,
         cross_ind    TYPE /ccbji/otc_cross,
         kvsl1        TYPE kvsl1,
         hkont        TYPE hkont,
         agre_typ     TYPE /ccbji/otc_agre_typ,
         main_bukrs   TYPE /ccbji/otc_main_bukrs,
         main_vkbur   TYPE /ccbji/otc_main_vkbur,
         main_kostl   TYPE /ccbji/otc_main_kostl,
         sub_bukrs    TYPE /ccbji/otc_sub_bukrs,
         sub_vkbur    TYPE /ccbji/otc_sub_vkbur,
         sub_kostl    TYPE /ccbji/otc_sub_kostl,
         status       TYPE /ccbji/dme_access,
         app_date     TYPE /ccbji/otc_appdt,
       END OF ty_dhdr,

* ---- DME contract item (custom Z-table /CCBJI/T_DME_ITM) --------------
       BEGIN OF ty_ditem,
         knuma        TYPE knuma,
         item         TYPE /ccbji/otc_item,
         bukrs        TYPE bukrs,
         tposn        TYPE /ccbji/otc_tposn,
         tran_type    TYPE /ccbji/otc_dme_tran,
         sched_dat    TYPE /ccbji/otc_sched_date,
         vkbur        TYPE vkbur,
         tar_unit     TYPE /ccbji/otc_unit,
         cross_typ    TYPE /ccbji/otc_crotyp,
         compl        TYPE /scl/otc_lsa_compl,
         act_amnt     TYPE /ccbji/otc_lsaactamt,
         act_vol      TYPE /ccbji/otc_act_vol,
         mwskz        TYPE mwskz,
         lsa_doc      TYPE /ccbji/otc_lsa_doc,         "FI document (orig.)
         new_doc      TYPE /ccbji/dme_nwdoc,           "FI document (post rev.)
         budat        TYPE budat,
         item_status  TYPE /ccbji/dme_access,
         zupdind      TYPE /ccej/mdm_upd_ind,
       END OF ty_ditem,

* ---- CCM condition contract header (TO-BE: replaces KONA read) --------
*  AS-IS ty_kona was read from rebate agreement; TO-BE reads WCOCOH.
       BEGIN OF ty_ccm,
         knuma        TYPE knuma,                      "WCOCOH-NUM
         vkorg        TYPE vkorg,
         vtweg        TYPE vtweg,
         spart        TYPE spart,
         vkbur        TYPE vkbur,                      "WCOCOH-VKBUR
         boart        TYPE boart,
         botacc       TYPE /ccej/mdm_botacct,          "WCOCOH-ZZBOTACC
         cust_owner   TYPE kunnr,                      "WCOCOH-CUST_OWNER
         datab        TYPE abdatab,                    "valid-from
         datbi        TYPE abdatbi,                    "valid-to
         botext       TYPE botext,                     "description
       END OF ty_ccm,

* ---- ZDME_GL_MAPPING (extended with AB CO-PA GL account) --------------
       BEGIN OF ty_dmegl,
         ktopl        TYPE ktopl,
         bukrs        TYPE bukrs,
         boart        TYPE boart,
         kvsl1        TYPE kvsl1,
         sakn1        TYPE saknr,                      "CB debit GL
         sakn3        TYPE /ccbji/sakn3,               "credit GL
         mwskz        TYPE mwskz,
         vfield       TYPE rke_wrtfld,                 "CB CO-PA value field
         gl_acct_acctbsd TYPE saknr,                   "NEW: AB CO-PA GL (FS 9.3)
       END OF ty_dmegl,

* ---- FI document line items (BSEG) -----------------------------------
       BEGIN OF ty_bseg,
         bukrs        TYPE bukrs,
         belnr        TYPE belnr_d,
         gjahr        TYPE gjahr,
         buzei        TYPE buzei,
         shkzg        TYPE shkzg,
         mwskz        TYPE mwskz,
         wrbtr        TYPE wrbtr,
         fwbas        TYPE fwbas,
         hkont        TYPE hkont,
         kunnr        TYPE kunnr,
       END OF ty_bseg,

* ---- Sales line read from CDS view ZC_DME_SALES_COPA -----------------
*  CDS sources CE2JP00 (<=Nov-2027) / ACDOCA (>=Dec-2027). Field names
*  kept compatible with the AS-IS CE1/CE2 profitability structure so the
*  downstream COPA build is unchanged.
       BEGIN OF ty_sales,
         gjahr        TYPE gjahr,
         perde        TYPE periode,
         bukrs        TYPE bukrs,
         kndnr        TYPE kunde_pa,                   "customer / dealer
         artnr        TYPE artnr,                      "product
         werks        TYPE werks_d,                    "plant / store
         vkorg        TYPE vkorg,
         vtweg        TYPE vtweg,
         spart        TYPE spart,
         prctr        TYPE prctr,
         kunwe        TYPE kunwe,                      "ship-to
         kmvkbu       TYPE vkbur,                      "sales office
         ww207        TYPE rkeg_ww207,                 "dealer characteristic
         ww214        TYPE /ccej/mdm_botacct,          "bottler account
         vkaus        TYPE abrvw,                      "usage code
         ww228        TYPE knuma,                      "contract no.
         ww229        TYPE /ccbji/otc_tposn,           "contract item
         erlos        TYPE rke2_erlos,                 "revenue
         vv506        TYPE rke2_vv506,                 "sales tax exclusive
         vv507        TYPE rke2_vv507,                 "sales tax inclusive
       END OF ty_sales,

* ---- Aggregated CO-PA work structure ---------------------------------
       BEGIN OF ty_copa,
         ww228        TYPE knuma,
         ww229        TYPE /ccbji/otc_tposn,
         kndnr        TYPE kunde_pa,
         artnr        TYPE artnr,
         vkaus        TYPE abrvw,
         gjahr        TYPE gjahr,
         perde        TYPE periode,
         bukrs        TYPE bukrs,
         werks        TYPE werks_d,
         vkorg        TYPE vkorg,
         vtweg        TYPE vtweg,
         spart        TYPE spart,
         prctr        TYPE prctr,
         kunwe        TYPE kunwe,
         kmvkbu       TYPE vkbur,
         ww207        TYPE rkeg_ww207,
         ww214        TYPE /ccej/mdm_botacct,
         vv506        TYPE rke2_vv506,
         vv507        TYPE rke2_vv507,
       END OF ty_copa,

* ---- ALV result line --------------------------------------------------
       BEGIN OF ty_sfinal,
         monat        TYPE monat,
         gjahr        TYPE gjahr,
         bukrs        TYPE bukrs,
         knuma        TYPE knuma,
         datab        TYPE abdatab,
         datbi        TYPE abdatbi,
         amnt         TYPE /ccej/rtr_tot,              "CO-PA amount (CB)
         atax         TYPE /ccej/rtr_tot,              "CO-PA tax
         cont_amnt    TYPE /ccej/rtr_tot,
         cb_doc       TYPE belnr_d,                    "CB CO-PA document
         ab_doc       TYPE belnr_d,                    "AB CO-PA document (NEW)
         msg          TYPE bapi_msg,
       END OF ty_sfinal.

* ---- Global internal tables / work areas -----------------------------
DATA: gt_dhdr   TYPE STANDARD TABLE OF ty_dhdr,
      gt_ditem  TYPE STANDARD TABLE OF ty_ditem,
      gt_ccm    TYPE STANDARD TABLE OF ty_ccm,
      gt_dmegl  TYPE STANDARD TABLE OF ty_dmegl,
      gt_bseg   TYPE STANDARD TABLE OF ty_bseg,
      gt_sales  TYPE STANDARD TABLE OF ty_sales,
      gt_sfinal TYPE STANDARD TABLE OF ty_sfinal.

DATA: gv_knuma  TYPE knuma,
      gv_vkbur  TYPE vkbur,
      gv_botac  TYPE /ccej/mdm_botacct,
      gv_kunnr  TYPE kunnr,
      gv_last   TYPE sy-datum,                         "month-end date
      gv_dbgl   TYPE saknr,                            "CB debit GL
      gv_crgl   TYPE /ccbji/sakn3,                     "credit GL
      gv_abgl   TYPE saknr,                            "AB CO-PA GL (NEW)
      gv_vfield TYPE rke_wrtfld,                       "CB value field
      gv_mwskz  TYPE mwskz,
      gv_eflag  TYPE char1,                            "error flag
      gr_stat   TYPE RANGE OF /ccbji/dme_access,
      gr_bukrs  TYPE RANGE OF bukrs.                   "B3: main + sub company

* ---- ALV object references -------------------------------------------
DATA: go_table   TYPE REF TO cl_salv_table,
      go_columns TYPE REF TO cl_salv_columns_table,
      go_column  TYPE REF TO cl_salv_column.

* ---- Constants (subset of AS-IS, Spot-Pay relevant) ------------------
CONSTANTS: c_i      TYPE char1            VALUE 'I',
           c_eq     TYPE char2            VALUE 'EQ',
           c_btw    TYPE char2            VALUE 'BT',
           c_ex     TYPE char1            VALUE 'E',
           c_err    TYPE char1            VALUE 'E',
           c_succ   TYPE char1            VALUE 'S',
           c_sep    TYPE char1            VALUE ',',
           c_zero   TYPE char1            VALUE '0',
           c_two    TYPE /ccbji/dme_paytyp VALUE '2',  "Spot Pay
           c_appr   TYPE /ccbji/dme_access VALUE '05', "Approved
           c_calc   TYPE /ccbji/dme_access VALUE '06', "Calculated
           c_clos   TYPE /ccbji/dme_access VALUE '07', "Closed
           c_canc   TYPE /ccbji/dme_access VALUE '08', "Cancelled
           c_with   TYPE /ccbji/dme_access VALUE '09', "Withdrawn
           c_post   TYPE char2            VALUE '01',  "Transaction type 01
           c_amort  TYPE /ccbji/otc_dme_tran VALUE '04',
           c_sales  TYPE /ccbji/otc_dme_tran VALUE '91',
           c_erkrs  TYPE erkrs            VALUE 'JP00',"Operating concern
           c_opcon  TYPE ledbo            VALUE '01',  "Currency type
           c_curr   TYPE waers            VALUE 'JPY',
           c_dtyp   TYPE blart            VALUE 'GR',  "DME accrual doc type
           c_object   TYPE xuobject       VALUE 'F_BKPF_BUK',
           c_activity TYPE xufield        VALUE 'ACTVT',
           c_bukv     TYPE xufield        VALUE 'BUKRS',
           c_act01    TYPE activ_auth     VALUE '01',
           c_dummy_mat TYPE matnr         VALUE '9651030000', "B2: dummy mat.
           c_gltv   TYPE tvarvc-name      VALUE '/CCBJI/RTR_DME_CRGL',
           c_type   TYPE rsscr_kind       VALUE 'P',
           c_numb   TYPE tvarv_numb       VALUE '0000',
*  Switch-over period CE2JP00 -> ACDOCA for the sales source (F3).
*  Production rule: CE2JP00 till Nov-2027, ACDOCA from Dec-2027 ('2027012').
*  Testing  rule  : ACDOCA from Jan-2026 ('2026001') per Gaurav-san.
*  The active boundary is read from TVARVC c_acdoca_tvar (fallback below)
*  and passed to the CDS view so test/prod use the same code.
           c_acdoca_from TYPE jahrper     VALUE '2027012',   "prod default
           c_acdoca_tvar TYPE tvarvc-name VALUE '/CCBJI/DME_ACDOCA_FROM'.

*&---------------------------------------------------------------------*
*&  SELECTION SCREEN  (mirrors include /CCBJI/RUFIAPR_DME_SEL)
*&  Field names retained from AS-IS; labels reference CCM contract.
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS:     p_bukrs TYPE bukrs OBLIGATORY,        "Company code
                p_monat TYPE monat OBLIGATORY,        "Fiscal period
                p_gjahr TYPE gjahr OBLIGATORY.        "Fiscal year
SELECT-OPTIONS: s_knuma  FOR gv_knuma,                "CCM contract no.
                s_vkbur  FOR gv_vkbur,                "Sales office
                s_botacc FOR gv_botac,                "Bottler account
                s_dealer FOR gv_kunnr.                "Dealer (customer)
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-012.
PARAMETERS:     p_budat TYPE budat,                   "Posting date override
                p_pmon  TYPE monat NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE text-027.
PARAMETERS:     rb_sale RADIOBUTTON GROUP r3 USER-COMMAND u1 DEFAULT 'X',
                rb_othr RADIOBUTTON GROUP r3.
SELECTION-SCREEN END OF BLOCK b5.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-016.
PARAMETERS:     rb_pre  RADIOBUTTON GROUP r2 DEFAULT 'X' USER-COMMAND u4,
                rb_pos  RADIOBUTTON GROUP r2,
                rb_spot RADIOBUTTON GROUP r2,
                rb_reg  RADIOBUTTON GROUP r2,
                rb_sec  RADIOBUTTON GROUP r2.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS:     rb_test RADIOBUTTON GROUP r1 USER-COMMAND u1 DEFAULT 'X',
                rb_post RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK b2.

*&---------------------------------------------------------------------*
*&  EVENTS
*&---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM f_clear.

AT SELECTION-SCREEN.
  PERFORM f_validations.
  PERFORM f_authorization_bukrs.

START-OF-SELECTION.
  PERFORM f_get_data.
  PERFORM f_process_route.

END-OF-SELECTION.
  PERFORM f_display_alv.

*&---------------------------------------------------------------------*
*&  Form  F_CLEAR  (Step 1: INITIALIZATION)
*&---------------------------------------------------------------------*
FORM f_clear.
  CLEAR: gt_dhdr, gt_ditem, gt_ccm, gt_dmegl, gt_bseg, gt_sales,
         gt_sfinal, gv_last, gv_dbgl, gv_crgl, gv_abgl, gv_vfield,
         gv_mwskz, gv_eflag, gr_stat, gr_bukrs.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_VALIDATIONS  (Step 2: VALIDATION)
*&---------------------------------------------------------------------*
FORM f_validations.
* Period must be 1..12
  IF p_monat NOT BETWEEN 1 AND 12.
    MESSAGE e000 WITH 'Enter a valid fiscal period (01-12)'(e01).
  ENDIF.
* Posting-date override (if any) must lie in the selected period
  IF p_budat IS NOT INITIAL.
    IF p_budat+0(4) <> p_gjahr OR p_budat+4(2) <> p_monat.
      MESSAGE e000 WITH 'Posting date must be in the selected period'(e02).
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_AUTHORIZATION_BUKRS
*&---------------------------------------------------------------------*
FORM f_authorization_bukrs.
  AUTHORITY-CHECK OBJECT c_object
                  ID c_bukv     FIELD p_bukrs
                  ID c_activity FIELD c_act01.
  IF sy-subrc <> 0.
    MESSAGE e001 WITH p_bukrs.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_GET_DATA  (Steps 3-6: master / contract / sales reads)
*&---------------------------------------------------------------------*
FORM f_get_data.
  DATA: lv_dat   TYPE sy-datum,
        lr_apdat TYPE RANGE OF /ccbji/otc_appdt,
        lwa_apd  LIKE LINE OF lr_apdat,
        lwa_stat LIKE LINE OF gr_stat.

* ---- Build status range (Approved 05 / Calculated 06) ----------------
  lwa_stat-sign = c_i. lwa_stat-option = c_eq.
  lwa_stat-low  = c_appr. APPEND lwa_stat TO gr_stat.
  lwa_stat-low  = c_calc. APPEND lwa_stat TO gr_stat.

* ---- Determine month-end date for the run period --------------------
  CONCATENATE p_gjahr p_monat c_opcon INTO lv_dat.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_dat
    IMPORTING
      last_day_of_month = gv_last
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
  IF sy-subrc = 0.
    lwa_apd-sign = c_i. lwa_apd-option = c_btw.
    lwa_apd-low  = lv_dat. lwa_apd-high = gv_last.
    APPEND lwa_apd TO lr_apdat.
  ENDIF.

* =====================================================================
* Step 3a: DME contract header (custom Z-table, unchanged) - Spot Pay
*  PAY_TYPE = 2 (Spot), not completed, approved/calculated, app.date in
*  run period (FS 4.2.1).
* =====================================================================
  IF rb_spot = abap_true.
    SELECT knuma bukrs pay_type cal_type boart start_date cont_amnt
           cont_vol waers compl tax_ind cross_ind kvsl1 hkont agre_typ
           main_bukrs main_vkbur main_kostl sub_bukrs sub_vkbur sub_kostl
           status app_date
      FROM /ccbji/t_dme_hdr
      INTO CORRESPONDING FIELDS OF TABLE gt_dhdr
      WHERE knuma    IN s_knuma
        AND bukrs    EQ p_bukrs
        AND pay_type EQ c_two
        AND compl    EQ space
        AND status   IN gr_stat
        AND app_date IN lr_apdat.
  ENDIF.

  IF gt_dhdr IS INITIAL.
    RETURN.
  ENDIF.
  SORT gt_dhdr BY knuma.

* =====================================================================
* Step 3b: DME contract items (custom Z-table). Key now references the
*  CCM contract number (FS 4.2.3). FI document = NEW_DOC if set, else
*  LSA_DOC (post-reversal precedence).
* =====================================================================
  SELECT knuma item bukrs tposn tran_type sched_dat vkbur tar_unit
         cross_typ compl act_amnt act_vol mwskz lsa_doc new_doc budat
         item_status zupdind
    FROM /ccbji/t_dme_itm
    INTO CORRESPONDING FIELDS OF TABLE gt_ditem
    FOR ALL ENTRIES IN gt_dhdr
    WHERE knuma = gt_dhdr-knuma
      AND bukrs = p_bukrs.
  DELETE gt_ditem WHERE zupdind = 'D'.            "drop deleted items
  SORT gt_ditem BY knuma tran_type item.

* =====================================================================
* Step 3c: CCM condition contract header (TO-BE: replaces KONA read).
*  Sales office / bottler account / dealer now come from WCOCOH and are
*  honoured against the s_vkbur / s_botacc / s_dealer ranges (FS 2.1).
*  >>> T1 EDIT POINT: confirm ZZBOTACC / CUST_OWNER append names with
*      Pankaj-san. These two columns are the only activation blocker here.
* =====================================================================
  SELECT num        AS knuma,
         vkorg      AS vkorg,
         vtweg      AS vtweg,
         spart      AS spart,
         vkbur      AS vkbur,
         boart      AS boart,
         zzbotacc   AS botacc,
         cust_owner AS cust_owner,
         datab      AS datab,
         datbi      AS datbi,
         botext     AS botext
    FROM wcocoh                                   "#EC CI_NOFIELD
    FOR ALL ENTRIES IN @gt_dhdr
    WHERE num        = @gt_dhdr-knuma
      AND vkbur      IN @s_vkbur
      AND zzbotacc   IN @s_botacc
      AND cust_owner IN @s_dealer
    INTO CORRESPONDING FIELDS OF TABLE @gt_ccm.
  SORT gt_ccm BY knuma.
* Keep only contracts that survived the CCM selection-screen filters
  IF s_vkbur[] IS NOT INITIAL OR s_botacc[] IS NOT INITIAL
                              OR s_dealer[] IS NOT INITIAL.
    LOOP AT gt_dhdr ASSIGNING FIELD-SYMBOL(<ls_chk>).
      READ TABLE gt_ccm TRANSPORTING NO FIELDS
           WITH KEY knuma = <ls_chk>-knuma BINARY SEARCH.
      IF sy-subrc <> 0.
        DELETE gt_dhdr INDEX sy-tabix.
      ENDIF.
    ENDLOOP.
  ENDIF.

* =====================================================================
* Step 4: GL account determination.
*  CLIENT DECISION (B1): a separate Account-Based CO-PA GL mapping is NOT
*  yet confirmed. Until it is, read the existing DME GL master table
*  /CCBJI/T_DME_GL and use the SAME GL accounts for CB and AB CO-PA.
*  The ZDME_GL_MAPPING extension / GL_ACCT_ACCTBSD column (FS 9.3) is
*  retained in the data model as a reserved field for the future split.
* =====================================================================
  SELECT ktopl bukrs boart kvsl1 sakn1 sakn3 mwskz vfield
    FROM /ccbji/t_dme_gl
    INTO CORRESPONDING FIELDS OF TABLE gt_dmegl
    WHERE bukrs = p_bukrs.
  SORT gt_dmegl BY boart kvsl1.

* =====================================================================
* Step 5: FI document amounts (BSEG) for the approved item documents.
* =====================================================================
  IF gt_ditem IS NOT INITIAL.
    SELECT bukrs belnr gjahr buzei shkzg mwskz wrbtr fwbas hkont kunnr
      FROM bseg
      INTO CORRESPONDING FIELDS OF TABLE gt_bseg
      FOR ALL ENTRIES IN gt_ditem
      WHERE bukrs = gt_ditem-bukrs
        AND belnr = gt_ditem-lsa_doc
        AND gjahr = p_gjahr.                       "#EC CI_NOFIELD
    SORT gt_bseg BY bukrs belnr gjahr buzei.
  ENDIF.

* =====================================================================
* B3 - Cross-territory / cross-company: build the company-code range that
*  sales must be read for. Always includes the run company P_BUKRS, plus
*  the SUB_BUKRS of every cross-territory contract (CROSS_IND set).
*  Test case 5000442076.
* =====================================================================
  gr_bukrs = VALUE #( sign = 'I' option = 'EQ' ( low = p_bukrs ) ).
  LOOP AT gt_dhdr ASSIGNING FIELD-SYMBOL(<ls_cross>)
       WHERE cross_ind IS NOT INITIAL AND sub_bukrs IS NOT INITIAL.
    IF NOT line_exists( gr_bukrs[ low = <ls_cross>-sub_bukrs ] ).
      APPEND VALUE #( sign = 'I' option = 'EQ'
                      low = <ls_cross>-sub_bukrs ) TO gr_bukrs.
    ENDIF.
  ENDLOOP.

* =====================================================================
* Step 6: Sales data via CDS view ZC_DME_SALES_COPA.
*  The CDS view encapsulates the CE2JP00 (<=Nov-2027) / ACDOCA (>=Dec-2027)
*  source switch, replacing the direct CE1JP00 / CE2JP00 SELECT.
*  Filtered by operating concern period, company, sales office, dealer /
*  bottler and contract number (FS 4.2.4).
* =====================================================================
  PERFORM f_get_sales.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_GET_SALES  (Step 6 detail - CDS based)
*&---------------------------------------------------------------------*
FORM f_get_sales.
  DATA: lv_perio   TYPE jahrper,
        lv_cutover TYPE jahrper.

  CONCATENATE p_gjahr c_zero p_monat INTO lv_perio.   "YYYY0PP

* CLIENT DECISION (F3): the CE2JP00 -> ACDOCA cut-over period is
* configurable. Production uses Dec-2027; testing uses Jan-2026. The
* active value is taken from TVARVC c_acdoca_tvar; if not maintained,
* the production default c_acdoca_from applies.
  SELECT SINGLE low FROM tvarvc INTO lv_cutover
    WHERE name = c_acdoca_tvar AND type = c_type AND numb = c_numb.
  IF sy-subrc <> 0 OR lv_cutover IS INITIAL.
    lv_cutover = c_acdoca_from.
  ENDIF.

* The CDS view ZC_DME_SALES_COPA exposes a normalised profitability set
* sourced from CE2JP00 (before the cut-over) or ACDOCA (from the
* cut-over). The boundary is passed as a CDS parameter. Selection by
* contract (WW228) plus dealer / bottler is applied here.
  SELECT gjahr, perde, bukrs, kndnr, artnr, werks, vkorg, vtweg, spart,
         prctr, kunwe, kmvkbu, ww207, ww214, vkaus, ww228, ww229,
         erlos, vv506, vv507
    FROM zc_dme_sales_copa( p_cutover = @lv_cutover )
    FOR ALL ENTRIES IN @gt_dhdr
    WHERE perio  = @lv_perio
      AND bukrs  IN @gr_bukrs                       "B3: main + cross company
      AND kmvkbu IN @s_vkbur
      AND ww228  = @gt_dhdr-knuma
    INTO CORRESPONDING FIELDS OF TABLE @gt_sales.   "#EC CI_NOFIELD
  SORT gt_sales BY ww228 kndnr artnr.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_PROCESS_ROUTE  (Step 7: route by payment type)
*&---------------------------------------------------------------------*
FORM f_process_route.
  CASE abap_true.
    WHEN rb_spot.
      PERFORM f_process_spdata.                    "Phase 1 - implemented
    WHEN rb_pre OR rb_pos OR rb_reg OR rb_sec.
*     TODO-FS Phase 2: Pre-Pay / Post-Pay / Regular Pay / Post-Pay Sec.
*     follow the same structural pattern as Spot Pay (FS sections 5-8).
      MESSAGE s000 WITH
        'Selected payment type is delivered in Phase 2'(p02)
        INTO DATA(lv_dummy_msg).
      APPEND VALUE ty_sfinal( monat = p_monat gjahr = p_gjahr
                              msg = lv_dummy_msg ) TO gt_sfinal.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_PROCESS_SPDATA   (Spot Pay - FS section 4.2)
*&  Steps 7-13 for each Spot-Pay contract.
*&---------------------------------------------------------------------*
FORM f_process_spdata.
  DATA: lwa_sfinal TYPE ty_sfinal,
        lt_copa    TYPE STANDARD TABLE OF ty_copa,
        lwa_copa   TYPE ty_copa,
        lv_aamnt   TYPE /ccej/rtr_tot,             "base amount (item)
        lv_coamnt  TYPE /ccej/rtr_tot,             "CO-PA amount
        lv_cotax   TYPE /ccej/rtr_tot,             "CO-PA tax
        lv_bsamnt  TYPE /ccej/rtr_tot,             "BSEG expense amount
        lv_bstax   TYPE /ccej/rtr_tot,             "BSEG tax amount
        lv_comp    TYPE /ccej/rtr_tot,             "running posted total
        lv_adjst   TYPE /ccej/rtr_tot,             "rounding adjustment
        lv_message TYPE char255.

  FIELD-SYMBOLS: <ls_dhdr>  TYPE ty_dhdr,
                 <ls_ccm>   TYPE ty_ccm,
                 <ls_dmegl> TYPE ty_dmegl,
                 <ls_ditem> TYPE ty_ditem,
                 <ls_sales> TYPE ty_sales,
                 <ls_copa>  TYPE ty_copa.

* Read TVARVC for the credit GL (carried over from AS-IS)
  SELECT SINGLE low FROM tvarvc INTO gv_crgl
    WHERE name = c_gltv AND type = c_type AND numb = c_numb.
  IF sy-subrc = 0.
    PERFORM f_conversion CHANGING gv_crgl.
  ENDIF.

  LOOP AT gt_dhdr ASSIGNING <ls_dhdr>.
    CLEAR: lwa_sfinal, lt_copa, lv_aamnt, lv_coamnt, lv_cotax,
           lv_bsamnt, lv_bstax, lv_comp, gv_eflag, gv_dbgl, gv_abgl,
           gv_vfield, gv_mwskz, lv_message.

    lwa_sfinal-monat = p_monat.
    lwa_sfinal-gjahr = p_gjahr.
    lwa_sfinal-bukrs = <ls_dhdr>-bukrs.
    lwa_sfinal-knuma = <ls_dhdr>-knuma.
    lwa_sfinal-cont_amnt = <ls_dhdr>-cont_amnt.

*   ---- CCM contract header ------------------------------------------
    READ TABLE gt_ccm ASSIGNING <ls_ccm>
         WITH KEY knuma = <ls_dhdr>-knuma BINARY SEARCH.
    IF sy-subrc <> 0.
      MESSAGE s038 WITH <ls_dhdr>-knuma INTO lwa_sfinal-msg.
      APPEND lwa_sfinal TO gt_sfinal.
      CONTINUE.
    ENDIF.
    lwa_sfinal-datab = <ls_ccm>-datab.
    lwa_sfinal-datbi = <ls_ccm>-datbi.

*   ---- GL determination (FS 4.2.2) ----------------------------------
*   CB debit GL (SAKN1), CB value field (VFIELD) and the NEW AB CO-PA GL.
    READ TABLE gt_dmegl ASSIGNING <ls_dmegl>
         WITH KEY boart = <ls_ccm>-boart
                  kvsl1 = <ls_dhdr>-kvsl1 BINARY SEARCH.
    IF sy-subrc = 0.
      gv_dbgl   = <ls_dmegl>-sakn1.
      gv_vfield = <ls_dmegl>-vfield.
*     CLIENT DECISION (B1): AB CO-PA uses the same GL as CB until a
*     separate account-based GL mapping is confirmed. When the
*     GL_ACCT_ACCTBSD column is populated, switch gv_abgl to it.
      gv_abgl   = COND #( WHEN <ls_dmegl>-gl_acct_acctbsd IS NOT INITIAL
                          THEN <ls_dmegl>-gl_acct_acctbsd
                          ELSE <ls_dmegl>-sakn1 ).
      PERFORM f_conversion CHANGING gv_dbgl.
      PERFORM f_conversion CHANGING gv_abgl.
    ENDIF.

*   ---- FI document reference + tax code from item (FS 4.2.3) ---------
    PERFORM f_get_fi_ref USING <ls_dhdr>
                      CHANGING lv_bsamnt lv_bstax.

*   ---- Base amount = approved actual amount of the item -------------
    READ TABLE gt_ditem ASSIGNING <ls_ditem>
         WITH KEY knuma = <ls_dhdr>-knuma.
    IF sy-subrc = 0.
      lv_aamnt = <ls_ditem>-act_amnt.
      gv_mwskz = <ls_ditem>-mwskz.
    ENDIF.

*   ---- Amount & tax calculation (FS 4.2.6) --------------------------
    IF <ls_dhdr>-tax_ind IS INITIAL.
*     consumption tax extracted from base amount
      PERFORM f_cal_ctax USING <ls_dhdr>
                      CHANGING lv_cotax lv_aamnt.
      lv_coamnt = lv_aamnt.
    ELSE.
*     sales tax: CO-PA amount = base amount minus tax
      PERFORM f_cal_stax USING <ls_dhdr>
                      CHANGING lv_cotax lv_aamnt.
      lv_coamnt = lv_aamnt - lv_cotax.
    ENDIF.
*   BSEG amounts (if available) override the calculated values
    IF lv_bsamnt IS NOT INITIAL.
      lv_coamnt = lv_bsamnt.
      lv_cotax  = lv_bstax.
    ENDIF.
    lwa_sfinal-amnt = lv_coamnt.
    lwa_sfinal-atax = lv_cotax.

*   ---- Build CO-PA allocation table ----------------------------------
*   Aggregate sales for this contract by dealer / product / store.
    LOOP AT gt_sales ASSIGNING <ls_sales>
         WHERE ww228 = <ls_dhdr>-knuma.
      CLEAR lwa_copa.
      MOVE-CORRESPONDING <ls_sales> TO lwa_copa.
      COLLECT lwa_copa INTO lt_copa.
    ENDLOOP.

*   ---- Dummy material allocation (FS 4.2.5) --------------------------
*   No actual sales -> allocate the full contract amount to a single
*   dummy-material profitability segment (replaces the old 1.01 write).
    IF lt_copa IS INITIAL.
      PERFORM f_add_dummy USING <ls_dhdr> <ls_ccm> <ls_dhdr>-bukrs
                       CHANGING lt_copa.
*     B3: cross-company -> add a dummy allocation line for the sub company
      IF <ls_dhdr>-cross_ind IS NOT INITIAL
         AND <ls_dhdr>-sub_bukrs IS NOT INITIAL.
        PERFORM f_add_dummy USING <ls_dhdr> <ls_ccm> <ls_dhdr>-sub_bukrs
                         CHANGING lt_copa.
      ENDIF.
    ENDIF.

*   ---- Prorate CO-PA amount across the allocation basis -------------
    PERFORM f_prorate CHANGING lt_copa lv_coamnt lv_cotax.

*   ---- POST: CB + AB CO-PA via BAPI_ACC_DOCUMENT_POST (FS 4.3) -------
*   TODO-FS feasibility: single-call dual posting on op. concern JP00.
    PERFORM f_post_copa_acc USING <ls_dhdr> <ls_ccm> lt_copa
                         CHANGING lwa_sfinal lv_message.

*   ---- Commit / rollback (Step 11-12) --------------------------------
    IF rb_post = abap_true.
      IF gv_eflag IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING wait = abap_true.
*       ---- Update DME item table with CO-PA references (Step 12) ------
        PERFORM f_upd_item USING <ls_dhdr> lwa_sfinal.
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.
    ENDIF.

    IF lwa_sfinal-msg IS INITIAL.
      lwa_sfinal-msg = COND #( WHEN rb_test = abap_true
                               THEN 'Test run successful'(t01)
                               ELSE 'Posting successful'(t02) ).
    ENDIF.
    APPEND lwa_sfinal TO gt_sfinal.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_GET_FI_REF   (FS 4.2.3)
*&  Reads the approved item (tran_type 05) to obtain the FI document and
*&  the corresponding expense / tax amounts from BSEG.
*&---------------------------------------------------------------------*
FORM f_get_fi_ref USING    ps_dhdr  TYPE ty_dhdr
                  CHANGING pv_bsamnt TYPE /ccej/rtr_tot
                           pv_bstax  TYPE /ccej/rtr_tot.
  DATA: lv_doc TYPE belnr_d.
  FIELD-SYMBOLS: <ls_itm>  TYPE ty_ditem,
                 <ls_bseg> TYPE ty_bseg.

  READ TABLE gt_ditem ASSIGNING <ls_itm>
       WITH KEY knuma = ps_dhdr-knuma.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.
* NEW_DOC takes precedence over LSA_DOC (post-reversal scenario)
  lv_doc = COND #( WHEN <ls_itm>-new_doc IS NOT INITIAL
                   THEN <ls_itm>-new_doc ELSE <ls_itm>-lsa_doc ).
  IF lv_doc IS INITIAL.
    RETURN.
  ENDIF.
  IF <ls_itm>-mwskz IS NOT INITIAL.
    gv_mwskz = <ls_itm>-mwskz.
  ENDIF.

  LOOP AT gt_bseg ASSIGNING <ls_bseg>
       WHERE bukrs = ps_dhdr-bukrs AND belnr = lv_doc.
    IF <ls_bseg>-hkont = gv_dbgl.                  "expense line
      pv_bsamnt = pv_bsamnt + <ls_bseg>-wrbtr.
    ELSEIF <ls_bseg>-mwskz IS NOT INITIAL.         "tax line
      pv_bstax = pv_bstax + <ls_bseg>-wrbtr.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_ADD_DUMMY   (B3 / FS 4.2.5)
*&  Appends one dummy-material allocation line for the given company code
*&  (weight 1). Called once for the main company and again for the sub
*&  company in cross-territory contracts.
*&---------------------------------------------------------------------*
FORM f_add_dummy USING ps_dhdr TYPE ty_dhdr
                       ps_ccm  TYPE ty_ccm
                       pv_bukrs TYPE bukrs
              CHANGING pt_copa TYPE STANDARD TABLE.
  DATA lwa_copa TYPE ty_copa.
  lwa_copa-ww228  = ps_dhdr-knuma.
  lwa_copa-bukrs  = pv_bukrs.
  lwa_copa-artnr  = c_dummy_mat.                   "dummy material 9651030000
  lwa_copa-kndnr  = ps_ccm-cust_owner.
  lwa_copa-kmvkbu = ps_ccm-vkbur.
  lwa_copa-vkorg  = ps_ccm-vkorg.
  lwa_copa-vtweg  = ps_ccm-vtweg.
  lwa_copa-spart  = ps_ccm-spart.
  lwa_copa-vv506  = 1.                             "weight = 1
  APPEND lwa_copa TO pt_copa.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_PRORATE
*&  Distributes the CO-PA amount / tax over the allocation lines in
*&  proportion to the sales weight (VV506), applying a rounding
*&  correction on the last line so the total ties to the contract
*&  amount (FS 4.2.6 last bullet).
*&---------------------------------------------------------------------*
FORM f_prorate CHANGING pt_copa  TYPE STANDARD TABLE
                        pv_amnt  TYPE /ccej/rtr_tot
                        pv_tax   TYPE /ccej/rtr_tot.
  DATA: lv_total TYPE rke2_vv506,
        lv_acc   TYPE /ccej/rtr_tot,
        lv_atax  TYPE /ccej/rtr_tot,
        lv_lines TYPE i,
        lv_idx   TYPE i.
  FIELD-SYMBOLS: <ls_copa> TYPE ty_copa.

  LOOP AT pt_copa ASSIGNING <ls_copa>.
    lv_total = lv_total + <ls_copa>-vv506.
  ENDLOOP.
  IF lv_total IS INITIAL.
    RETURN.
  ENDIF.

  DESCRIBE TABLE pt_copa LINES lv_lines.
  LOOP AT pt_copa ASSIGNING <ls_copa>.
    lv_idx = sy-tabix.
    IF lv_idx = lv_lines.
*     last line absorbs the rounding difference
      <ls_copa>-vv506 = pv_amnt - lv_acc.
      <ls_copa>-vv507 = pv_tax  - lv_atax.
    ELSE.
      <ls_copa>-vv506 = pv_amnt * <ls_copa>-vv506 / lv_total.
      <ls_copa>-vv507 = pv_tax  * <ls_copa>-vv506 / lv_total.
      lv_acc  = lv_acc  + <ls_copa>-vv506.
      lv_atax = lv_atax + <ls_copa>-vv507.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_POST_COPA_ACC   (FS 4.3 - TO-BE)
*&  Posts Costing-Based and Account-Based CO-PA via BAPI_ACC_DOCUMENT_POST.
*&  The profitability segment characteristics are passed through the
*&  CRITERIA table; CB value fields are passed through CURRENCYAMOUNT /
*&  the value-field GL line.  When operating concern JP00 has both CO-PA
*&  types active, the single posting updates both ledgers.
*&  TODO-FS: confirm single-call dual posting (open item #1, FS 11).
*&---------------------------------------------------------------------*
FORM f_post_copa_acc USING    ps_dhdr TYPE ty_dhdr
                              ps_ccm  TYPE ty_ccm
                              pt_copa TYPE STANDARD TABLE
                     CHANGING ps_sfinal TYPE ty_sfinal
                              pv_message TYPE char255.
  DATA: lwa_hdr     TYPE bapiache09,
        lt_accountgl TYPE bapiacgl09_tab,
        lt_curr     TYPE bapiaccr09_tab,
        lt_crit     TYPE STANDARD TABLE OF bapiackec9,
        lt_acctax   TYPE bapiactx09_tab,
        lt_return   TYPE STANDARD TABLE OF bapiret2,
        lwa_accountgl TYPE bapiacgl09,
        lwa_curr    TYPE bapiaccr09,
        lwa_crit    TYPE bapiackec9,
        lv_item     TYPE posnr_acc,
        lv_obj_key  TYPE bapiache09-obj_key.
  FIELD-SYMBOLS: <ls_copa> TYPE ty_copa,
                 <ls_ret>  TYPE bapiret2.

* ---- Document header --------------------------------------------------
  lwa_hdr-username   = sy-uname.
  lwa_hdr-comp_code  = p_bukrs.
  lwa_hdr-doc_date   = sy-datlo.
  lwa_hdr-pstng_date = COND #( WHEN p_budat IS NOT INITIAL
                               THEN p_budat ELSE gv_last ).
  lwa_hdr-doc_type   = c_dtyp.                     "GR - DME accrual
  lwa_hdr-ref_doc_no = ps_dhdr-knuma.
  lwa_hdr-fisc_year  = p_gjahr.

* ---- One GL line + profitability segment per allocation row ----------
  LOOP AT pt_copa ASSIGNING <ls_copa>.
    lv_item = lv_item + 1.

*   GL line (debit). For AB CO-PA, the value-field GL is the new mapped
*   GL_ACCT_ACCTBSD; CB CO-PA value fields are derived from VFIELD.
    CLEAR lwa_accountgl.
    lwa_accountgl-itemno_acc = lv_item.
    lwa_accountgl-gl_account = COND #( WHEN gv_abgl IS NOT INITIAL
                                       THEN gv_abgl ELSE gv_dbgl ).
    lwa_accountgl-item_text  = ps_ccm-botext.
    lwa_accountgl-tax_code   = gv_mwskz.
    lwa_accountgl-costcenter = COND #(
        WHEN <ls_copa>-bukrs = ps_dhdr-bukrs
        THEN ps_dhdr-main_kostl ELSE ps_dhdr-sub_kostl ).
    APPEND lwa_accountgl TO lt_accountgl.

*   Currency amount
    CLEAR lwa_curr.
    lwa_curr-itemno_acc   = lv_item.
    lwa_curr-currency_iso = ps_dhdr-waers.
    lwa_curr-amt_doccur   = <ls_copa>-vv506.
    lwa_curr-amt_base     = <ls_copa>-vv506.
    APPEND lwa_curr TO lt_curr.

*   Profitability segment characteristics (CRITERIA) - FS 4.2.7 / 4.3.3
    CLEAR lwa_crit.
    lwa_crit-itemno_acc = lv_item.
    PERFORM f_add_crit USING lv_item 'KNDNR'  <ls_copa>-kndnr  CHANGING lt_crit.
    PERFORM f_add_crit USING lv_item 'ARTNR'  <ls_copa>-artnr  CHANGING lt_crit.
    PERFORM f_add_crit USING lv_item 'WERKS'  <ls_copa>-werks  CHANGING lt_crit.
    PERFORM f_add_crit USING lv_item 'VKORG'  <ls_copa>-vkorg  CHANGING lt_crit.
    PERFORM f_add_crit USING lv_item 'VTWEG'  <ls_copa>-vtweg  CHANGING lt_crit.
    PERFORM f_add_crit USING lv_item 'PRCTR'  <ls_copa>-prctr  CHANGING lt_crit.
    PERFORM f_add_crit USING lv_item 'KMVKBU' <ls_copa>-kmvkbu CHANGING lt_crit.
*   Custom characteristics - must exist in the AB op. concern (FS 9.2)
    PERFORM f_add_crit USING lv_item 'VKAUS'  <ls_copa>-vkaus  CHANGING lt_crit.
    PERFORM f_add_crit USING lv_item 'WW228'  <ls_copa>-ww228  CHANGING lt_crit.
    PERFORM f_add_crit USING lv_item 'WW229'  <ls_copa>-ww229  CHANGING lt_crit.
    PERFORM f_add_crit USING lv_item 'KUNWE'  <ls_copa>-kunwe  CHANGING lt_crit.
  ENDLOOP.

* ---- Credit (offsetting) line ----------------------------------------
  lv_item = lv_item + 1.
  CLEAR lwa_accountgl.
  lwa_accountgl-itemno_acc = lv_item.
  lwa_accountgl-gl_account = gv_crgl.
  lwa_accountgl-item_text  = ps_ccm-botext.
  APPEND lwa_accountgl TO lt_accountgl.
  CLEAR lwa_curr.
  lwa_curr-itemno_acc   = lv_item.
  lwa_curr-currency_iso = ps_dhdr-waers.
  lwa_curr-amt_doccur   = ps_sfinal-amnt * -1.
  lwa_curr-amt_base     = ps_sfinal-amnt * -1.
  APPEND lwa_curr TO lt_curr.

* ---- Call BAPI (check on test run, post otherwise) -------------------
  IF rb_test = abap_true.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
      EXPORTING documentheader = lwa_hdr
      TABLES    accountgl      = lt_accountgl
                currencyamount = lt_curr
                criteria       = lt_crit
                accounttax     = lt_acctax
                return         = lt_return.
  ELSE.
    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING documentheader = lwa_hdr
      IMPORTING obj_key        = lv_obj_key
      TABLES    accountgl      = lt_accountgl
                currencyamount = lt_curr
                criteria       = lt_crit
                accounttax     = lt_acctax
                return         = lt_return.
    ps_sfinal-cb_doc = lv_obj_key+0(10).
    ps_sfinal-ab_doc = lv_obj_key+0(10).           "same FI doc, both ledgers
  ENDIF.

* ---- Evaluate return -------------------------------------------------
  SORT lt_return BY type.
  READ TABLE lt_return ASSIGNING <ls_ret>
       WITH KEY type = c_succ BINARY SEARCH.
  IF sy-subrc = 0.
    pv_message = <ls_ret>-message.
  ELSE.
    gv_eflag = abap_true.
    LOOP AT lt_return ASSIGNING <ls_ret> WHERE type = c_err.
      CONCATENATE <ls_ret>-message pv_message
             INTO pv_message SEPARATED BY c_sep.
    ENDLOOP.
  ENDIF.
  ps_sfinal-msg = pv_message.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_ADD_CRIT  (helper: append one CRITERIA characteristic)
*&---------------------------------------------------------------------*
FORM f_add_crit USING pv_item  TYPE posnr_acc
                      pv_field TYPE fieldname
                      pv_value TYPE clike
             CHANGING pt_crit  TYPE STANDARD TABLE.
  DATA lwa_crit TYPE bapiackec9.
  CHECK pv_value IS NOT INITIAL.
  lwa_crit-itemno_acc = pv_item.
  lwa_crit-fieldname  = pv_field.
  lwa_crit-character  = pv_value.
  APPEND lwa_crit TO pt_crit.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_CAL_CTAX  (Consumption tax - extract from net) - FS 4.2.6
*&  Mirrors AS-IS F_CAL_CTAX (CALCULATE_TAX_FROM_NET_AMOUNT).
*&---------------------------------------------------------------------*
FORM f_cal_ctax USING    ps_dhdr  TYPE ty_dhdr
                CHANGING pv_tax   TYPE /ccej/rtr_tot
                         pv_value TYPE /ccej/rtr_tot.
  DATA: lt_mwdat TYPE STANDARD TABLE OF rtax1u15,
        lv_round TYPE j_1itaxamt,
        lv_wrbtr TYPE wrbtr,
        lv_total TYPE wmwst.
  FIELD-SYMBOLS <ls_mw> TYPE rtax1u15.

  lv_wrbtr = pv_value.
  CALL FUNCTION 'CALCULATE_TAX_FROM_NET_AMOUNT'
    EXPORTING
      i_bukrs = ps_dhdr-bukrs
      i_mwskz = gv_mwskz
      i_waers = c_curr
      i_wrbtr = lv_wrbtr
    TABLES
      t_mwdat = lt_mwdat
    EXCEPTIONS
      OTHERS  = 1.
  IF sy-subrc = 0.
    LOOP AT lt_mwdat ASSIGNING <ls_mw> WHERE wmwst IS NOT INITIAL.
      lv_round = <ls_mw>-wmwst.
      CALL FUNCTION 'J_1I6_ROUND_TO_NEAREST_AMT'
        EXPORTING i_amount = lv_round
        IMPORTING e_amount = lv_round.
      lv_total = lv_total + lv_round.
    ENDLOOP.
  ENDIF.
  pv_tax   = lv_total.
  pv_value = lv_wrbtr.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_CAL_STAX  (Sales / inclusive tax - extract from gross)
*&  Mirrors AS-IS F_CAL_STAX (CALCULATE_TAX_FROM_GROSSAMOUNT).
*&---------------------------------------------------------------------*
FORM f_cal_stax USING    ps_dhdr  TYPE ty_dhdr
                CHANGING pv_tax   TYPE /ccej/rtr_tot
                         pv_value TYPE /ccej/rtr_tot.
  DATA: lt_mwdat TYPE STANDARD TABLE OF rtax1u15,
        lv_round TYPE j_1itaxamt,
        lv_wrbtr TYPE wrbtr,
        lv_total TYPE wmwst.
  FIELD-SYMBOLS <ls_mw> TYPE rtax1u15.

  lv_wrbtr = pv_value.
  CALL FUNCTION 'CALCULATE_TAX_FROM_GROSSAMOUNT'
    EXPORTING
      i_bukrs = ps_dhdr-bukrs
      i_mwskz = gv_mwskz
      i_waers = c_curr
      i_wrbtr = lv_wrbtr
    TABLES
      t_mwdat = lt_mwdat
    EXCEPTIONS
      OTHERS  = 1.
  IF sy-subrc = 0.
    LOOP AT lt_mwdat ASSIGNING <ls_mw> WHERE wmwst IS NOT INITIAL.
      lv_round = <ls_mw>-wmwst.
      CALL FUNCTION 'J_1I6_ROUND_TO_NEAREST_AMT'
        EXPORTING i_amount = lv_round
        IMPORTING e_amount = lv_round.
      lv_total = lv_total + lv_round.
    ENDLOOP.
  ENDIF.
  pv_tax   = lv_total.
  pv_value = lv_wrbtr.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_UPD_ITEM   (Step 12 - update DME item table)
*&  Writes a new amortisation/sales item row carrying the CO-PA document
*&  reference and completion flag (mirrors AS-IS F_UPD_TAB).
*&---------------------------------------------------------------------*
FORM f_upd_item USING ps_dhdr   TYPE ty_dhdr
                      ps_sfinal TYPE ty_sfinal.
  DATA: lwa_ditem TYPE /ccbji/t_dme_itm,
        lv_varkey TYPE rstable-varkey,
        lv_flag   TYPE char1,
        lv_item   TYPE /ccbji/otc_item.
  CONSTANTS lc_tab TYPE rstable-tabname VALUE '/CCBJI/T_DME_ITM'.
  FIELD-SYMBOLS <ls_itm> TYPE ty_ditem.

* next free item number
  LOOP AT gt_ditem ASSIGNING <ls_itm> WHERE knuma = ps_dhdr-knuma.
    IF <ls_itm>-item > lv_item.
      lv_item = <ls_itm>-item.
    ENDIF.
  ENDLOOP.
  lv_item = lv_item + 1.

  lwa_ditem-knuma     = ps_dhdr-knuma.
  lwa_ditem-item      = lv_item.
  lwa_ditem-bukrs     = ps_dhdr-bukrs.
  lwa_ditem-sched_dat = gv_last.
  lwa_ditem-act_amnt  = ps_sfinal-amnt.
  lwa_ditem-mwskz     = gv_mwskz.
  lwa_ditem-lsa_doc   = ps_sfinal-cb_doc.          "CO-PA document reference
  lwa_ditem-budat     = COND #( WHEN p_budat IS NOT INITIAL
                                THEN p_budat ELSE gv_last ).
  lwa_ditem-tran_type = COND #( WHEN rb_sale = abap_true
                                THEN c_sales ELSE c_amort ).
  lwa_ditem-zernam    = sy-uname.
  lwa_ditem-zersda    = sy-datlo.
  lwa_ditem-zerzzt    = sy-timlo.

  CONCATENATE sy-mandt lwa_ditem-knuma lwa_ditem-item INTO lv_varkey.
  DO 3 TIMES.
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING  mode_rstable = 'E' tabname = lc_tab varkey = lv_varkey
      EXCEPTIONS foreign_lock = 1 system_failure = 2 OTHERS = 3.
    IF sy-subrc = 0.
      lv_flag = abap_true.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.

  IF lv_flag = abap_true.
    MODIFY /ccbji/t_dme_itm FROM lwa_ditem.        "#EC CI_MODIFY
    COMMIT WORK.
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING mode_rstable = 'E' tabname = lc_tab varkey = lv_varkey.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_CONVERSION  (ALPHA input conversion - from AS-IS)
*&---------------------------------------------------------------------*
FORM f_conversion CHANGING pv_value TYPE clike.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING  input  = pv_value
    IMPORTING  output = pv_value.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_DISPLAY_ALV   (Step 13 - results)
*&---------------------------------------------------------------------*
FORM f_display_alv.
  IF gt_sfinal IS INITIAL.
    MESSAGE s000 WITH 'No data found for the given selection'(n01).
    RETURN.
  ENDIF.

  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = go_table
        CHANGING  t_table      = gt_sfinal ).

      go_table->get_functions( )->set_all( abap_true ).
      go_table->get_display_settings( )->set_striped_pattern( abap_true ).
      go_columns = go_table->get_columns( ).
      go_columns->set_optimize( abap_true ).

      PERFORM f_set_col_text USING 'KNUMA'  'Contract No.'(c01).
      PERFORM f_set_col_text USING 'AMNT'   'CO-PA Amount'(c02).
      PERFORM f_set_col_text USING 'ATAX'   'CO-PA Tax'(c03).
      PERFORM f_set_col_text USING 'CB_DOC' 'CB CO-PA Doc'(c04).
      PERFORM f_set_col_text USING 'AB_DOC' 'AB CO-PA Doc'(c05).
      PERFORM f_set_col_text USING 'MSG'    'Message'(c06).

      go_table->display( ).
    CATCH cx_salv_msg cx_salv_not_found INTO DATA(lo_err).
      MESSAGE lo_err->get_text( ) TYPE 'I'.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*&  Form  F_SET_COL_TEXT  (helper: set ALV column heading)
*&---------------------------------------------------------------------*
FORM f_set_col_text USING pv_col  TYPE lvc_fname
                          pv_text TYPE clike.
  TRY.
      go_column = go_columns->get_column( pv_col ).
      go_column->set_long_text( CONV scrtext_l( pv_text ) ).
      go_column->set_medium_text( CONV scrtext_m( pv_text ) ).
      go_column->set_short_text( CONV scrtext_s( pv_text ) ).
    CATCH cx_salv_not_found.
  ENDTRY.
ENDFORM.
