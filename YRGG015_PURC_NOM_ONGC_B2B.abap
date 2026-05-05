*&---------------------------------------------------------------------*
*& Report  YRGG015_PURC_NOM_ONGC_B2B
*& Purchase Nomination Creation - ONGC B2B
*& T-Code: YRGG015
*&---------------------------------------------------------------------*
REPORT yrgg015_purc_nom_ongc_b2b MESSAGE-ID oo
                                  LINE-SIZE 255
                                  NO STANDARD PAGE HEADING.

TABLES: oijnomi.

*----------------------------------------------------------------------*
* TYPE DECLARATIONS
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_pur,
         gas_day     TYPE aedat,
         locid       TYPE char10,
         material    TYPE matnr,
         state_code  TYPE char2,
         qty_scm     TYPE p LENGTH 13 DECIMALS 3,
         gail_id     TYPE char20,
         ongc_id     TYPE char20,
         deleted     TYPE char1,
       END OF ty_pur.

TYPES: BEGIN OF ty_main,
         tsyst  TYPE char4,
         vbeln  TYPE ebeln,
         date   TYPE aedat,
         locid  TYPE oij_locid,
         matnr  TYPE matnr,
         menge  TYPE p LENGTH 13 DECIMALS 3,
         unit   TYPE meins,
         charg  TYPE charg_d,
         rank   TYPE i,
         ancv   TYPE char10,
         agcv   TYPE char10,
         nomtk  TYPE char20,
         nomit  TYPE char10,
         msg    TYPE char220,
         msgty  TYPE msgty,
       END OF ty_main.

TYPES: BEGIN OF ty_display,
         sel         TYPE char1,
         exclude     TYPE char1,
         gas_day     TYPE aedat,
         locid       TYPE char10,
         material    TYPE matnr,
         state_code  TYPE char2,
         qty_scm     TYPE p LENGTH 13 DECIMALS 3,
         gail_id     TYPE char20,
         ongc_id     TYPE char20,
         outline_agr TYPE ebeln,
         charg       TYPE charg_d,
         oa_missing  TYPE char1,
         celltab     TYPE lvc_t_styl,
         t_color     TYPE lvc_t_scol,
       END OF ty_display.

TYPES: BEGIN OF ty_batch_vals,
         charg TYPE charg_d,
         matnr TYPE matnr,
         werks TYPE werks_d,
         ersda TYPE ersda,
       END OF ty_batch_vals.

TYPES: tt_main    TYPE STANDARD TABLE OF ty_main.
TYPES: tt_display TYPE STANDARD TABLE OF ty_display.

TYPES: BEGIN OF ty_batch_assign,
         matnr      TYPE matnr,
         state_code TYPE char2,
         charg      TYPE charg_d,
       END OF ty_batch_assign.
TYPES: tt_batch_assign TYPE STANDARD TABLE OF ty_batch_assign.

" Cache types for bulk pre-fetch (performance)
TYPES: BEGIN OF ty_mot_cache,
         vbeln    TYPE ebeln,
         matnr    TYPE matnr,
         locid    TYPE char10,
         fromdate TYPE d,
         todate   TYPE d,
       END OF ty_mot_cache.
TYPES: BEGIN OF ty_ekoa_cache,
         ebeln TYPE ebeln,
         bedat TYPE d,
         werks TYPE werks_d,
       END OF ty_ekoa_cache.
TYPES: BEGIN OF ty_t001w_cache,
         werks TYPE werks_d,
         regio TYPE regio,
       END OF ty_t001w_cache.
TYPES: BEGIN OF ty_mcha_cache,
         matnr TYPE matnr,
         werks TYPE werks_d,
         charg TYPE charg_d,
         ersda TYPE d,
       END OF ty_mcha_cache.

*----------------------------------------------------------------------*
* DATA DECLARATIONS
*----------------------------------------------------------------------*
DATA: gt_display      TYPE tt_display,
      gt_main         TYPE tt_main,
      go_alv          TYPE REF TO cl_gui_alv_grid,
      gs_layout       TYPE lvc_s_layo,
      gt_fcat         TYPE lvc_t_fcat,
      gv_auth_bg      TYPE char1,
      gv_toolbar_done TYPE char1,
      go_batch_popup  TYPE REF TO cl_gui_dialogbox_container,
      go_batch_alv    TYPE REF TO cl_gui_alv_grid,
      gt_batch_assign TYPE tt_batch_assign.

" Reference data cache (populated once in fetch_pur_data for performance)
DATA: gt_mot_c   TYPE STANDARD TABLE OF ty_mot_cache,
      gt_ekoa_c  TYPE STANDARD TABLE OF ty_ekoa_cache,
      gt_t001w_c TYPE STANDARD TABLE OF ty_t001w_cache,
      gt_mcha_c  TYPE STANDARD TABLE OF ty_mcha_cache.

CONSTANTS: gc_memory_id  TYPE char30 VALUE 'YRGG015_NOM_DATA',
           gc_err_mem_id TYPE char30 VALUE 'YRGG015_NOM_ERRORS',
           gc_role_core  TYPE char30 VALUE 'ZC_GMS_CORE_TEAM',
           gc_excl_state TYPE char2  VALUE 'GJ',
           gc_deleted    TYPE char1  VALUE 'X',
           gc_sm3        TYPE meins  VALUE 'SM3'.

*----------------------------------------------------------------------*
* ALV EVENT HANDLER CLASS (main grid data_changed + batch dialog)
*----------------------------------------------------------------------*
CLASS lcl_alv_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_alv_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      on_main_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,
      on_batch_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,
      on_batch_dlg_close
        FOR EVENT close OF cl_gui_dialogbox_container.
ENDCLASS.

DATA: go_alv_handler TYPE REF TO lcl_alv_handler.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_locid FOR oijnomi-locid NO INTERVALS,
                  s_date  FOR oijnomi-idate.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_bgrun AS CHECKBOX MODIF ID bg.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM set_default_fn_dates.
*  TEXT-001 = 'Selection Criteria'.
*  TEXT-002 = 'Background Processing'.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM control_screen_fields.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM validate_selection_screen.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM fetch_pur_data.
  IF gt_display IS INITIAL.
    MESSAGE 'No data found for the given selection criteria.' TYPE 'S'
            DISPLAY LIKE 'W'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF sy-batch = 'X'.
    PERFORM create_all_nominations_bg.
  ELSEIF p_bgrun = abap_true.
    PERFORM schedule_background_job.
  ELSE.
    PERFORM display_alv_grid.
  ENDIF.

*----------------------------------------------------------------------*
* ALV EVENT HANDLER - IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_alv_handler IMPLEMENTATION.

  METHOD on_alv_toolbar.
    DATA: ls_tb TYPE stb_button.
    IF gv_toolbar_done = abap_true. RETURN. ENDIF.
    CLEAR ls_tb.
    ls_tb-function  = 'BCMASS'.
    ls_tb-icon      = icon_batch.
    ls_tb-quickinfo = 'Batch Change in Mass'.
    ls_tb-text      = 'Batch Change'.
    INSERT ls_tb INTO e_object->mt_toolbar INDEX 1.
    CLEAR ls_tb.
    ls_tb-butn_type = 3.
    INSERT ls_tb INTO e_object->mt_toolbar INDEX 2.
    CLEAR ls_tb.
    ls_tb-function  = 'CRENOM'.
    ls_tb-icon      = icon_execute_object.
    ls_tb-quickinfo = 'Create Nomination'.
    ls_tb-text      = 'Create Nomination'.
    INSERT ls_tb INTO e_object->mt_toolbar INDEX 3.
    gv_toolbar_done = abap_true.
  ENDMETHOD.

  METHOD on_main_data_changed.
    DATA: ls_mod  TYPE lvc_s_modi,
          ls_disp TYPE ty_display.
    LOOP AT er_data_changed->mt_mod_cells INTO ls_mod.
      IF ls_mod-fieldname = 'CHARG'.
        READ TABLE gt_display INDEX ls_mod-row_id INTO ls_disp.
        IF sy-subrc = 0.
          ls_disp-charg = ls_mod-value.
          MODIFY gt_display INDEX ls_mod-row_id FROM ls_disp.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_batch_data_changed.
    DATA: ls_mod    TYPE lvc_s_modi,
          ls_assign TYPE ty_batch_assign.
    LOOP AT er_data_changed->mt_mod_cells INTO ls_mod.
      IF ls_mod-fieldname = 'CHARG'.
        READ TABLE gt_batch_assign INDEX ls_mod-row_id INTO ls_assign.
        IF sy-subrc = 0.
          ls_assign-charg = ls_mod-value.
          MODIFY gt_batch_assign INDEX ls_mod-row_id FROM ls_assign.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_batch_dlg_close.
    DATA: ls_assign TYPE ty_batch_assign,
          ls_disp   TYPE ty_display.
    LOOP AT gt_batch_assign INTO ls_assign.
      IF ls_assign-charg IS INITIAL. CONTINUE. ENDIF.
      LOOP AT gt_display INTO ls_disp WHERE sel = abap_true AND material = ls_assign-matnr.
        ls_disp-charg = ls_assign-charg.
        MODIFY gt_display FROM ls_disp.
      ENDLOOP.
    ENDLOOP.
    IF go_batch_alv IS NOT INITIAL.
      go_batch_alv->free( ). CLEAR go_batch_alv.
    ENDIF.
    IF go_batch_popup IS NOT INITIAL.
      go_batch_popup->free( ). CLEAR go_batch_popup.
    ENDIF.
    IF go_alv IS NOT INITIAL.
      go_alv->refresh_table_display( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* FORM set_default_fn_dates

*----------------------------------------------------------------------*
FORM set_default_fn_dates.
  DATA: lv_today TYPE sy-datum,
        lv_day   TYPE i,
        lv_low   TYPE sy-datum,
        lv_high  TYPE sy-datum,
        ls_date  LIKE LINE OF s_date.
  lv_today = sy-datum.
  lv_day   = lv_today+6(2).
  IF lv_day <= 15.
    lv_high      = lv_today.
    lv_high+6(2) = '01'.
    lv_high      = lv_high - 1.
    lv_low       = lv_high.
    lv_low+6(2)  = '16'.
  ELSE.
    lv_low      = lv_today.
    lv_low+6(2) = '01'.
    lv_high      = lv_today.
    lv_high+6(2) = '15'.
  ENDIF.
  ls_date-sign   = 'I'.
  ls_date-option = 'BT'.
  ls_date-low    = lv_low.
  ls_date-high   = lv_high.
  APPEND ls_date TO s_date.
ENDFORM.

*----------------------------------------------------------------------*
* FORM control_screen_fields
*----------------------------------------------------------------------*
FORM control_screen_fields.
  CLEAR gv_auth_bg.
  AUTHORITY-CHECK OBJECT 'S_TCODE' ID 'TCD' FIELD gc_role_core.
  IF sy-subrc = 0. gv_auth_bg = abap_true. ENDIF.
  LOOP AT SCREEN.
    IF screen-group1 = 'BG'.
      IF gv_auth_bg = abap_true.
        screen-active    = 1.
        screen-invisible = 0.
      ELSE.
        screen-active    = 0.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
* FORM validate_selection_screen
*----------------------------------------------------------------------*
FORM validate_selection_screen.
  DATA: ls_locid  LIKE LINE OF s_locid,
        ls_date   LIKE LINE OF s_date,
        lv_day_lo TYPE i,
        lv_day_hi TYPE i,
        lv_loc    TYPE oij_locid.

  LOOP AT s_locid INTO ls_locid WHERE sign = 'I' AND option = 'EQ'.
    SELECT SINGLE GAIL_LOC_ID FROM yrga_cst_loc_map INTO lv_loc
      WHERE GAIL_LOC_ID = ls_locid-low.
    IF sy-subrc <> 0.
      MESSAGE e000(oo) WITH
        'Business Location doesn''t pertain to ONGC CST Purchase:'
        ls_locid-low ' ' ' '.
    ENDIF.
  ENDLOOP.

  IF s_date[] IS INITIAL.
    MESSAGE e000(oo) WITH 'Gas Day from date is mandatory' ' ' ' ' ' '.
  ENDIF.

  LOOP AT s_date INTO ls_date WHERE sign = 'I' AND option = 'BT'.
    IF ls_date-low IS INITIAL.
      MESSAGE e000(oo) WITH 'Gas Day from date is mandatory' ' ' ' ' ' '.
    ENDIF.
    lv_day_lo = ls_date-low+6(2).
    lv_day_hi = ls_date-high+6(2).
    IF lv_day_lo <> 1 AND lv_day_lo <> 16.
      MESSAGE e000(oo) WITH 'Date range must start on 1st or 16th of month' ' ' ' ' ' '.
    ENDIF.
    IF lv_day_hi <> 15 AND lv_day_hi < 28.
      MESSAGE e000(oo) WITH 'Date range must end on 15th or last day of month' ' ' ' ' ' '.
    ENDIF.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
* FORM fetch_pur_data
*----------------------------------------------------------------------*
FORM fetch_pur_data.
  DATA: lt_pur  TYPE STANDARD TABLE OF ty_pur,
        ls_pur  TYPE ty_pur,
        ls_disp TYPE ty_display,
        ls_styl TYPE lvc_s_styl,
        ls_col  TYPE lvc_s_scol.

  SELECT gas_day location AS locid material state_code
         qty_in_scm AS qty_scm gail_id ongc_id deleted
    FROM yrga_cst_pur
    INTO CORRESPONDING FIELDS OF TABLE lt_pur
    WHERE gas_day  IN s_date
      AND location IN s_locid
      AND deleted <> gc_deleted.

  IF sy-subrc <> 0 OR lt_pur IS INITIAL. RETURN. ENDIF.

  " Bulk pre-fetch all reference data (5 SELECTs instead of N*M)
  PERFORM prefetch_reference_data USING lt_pur.

  LOOP AT lt_pur INTO ls_pur.
    CLEAR ls_disp.
    ls_disp-gas_day    = ls_pur-gas_day.
    ls_disp-locid      = ls_pur-locid.
    ls_disp-material   = ls_pur-material.
    ls_disp-state_code = ls_pur-state_code.
    ls_disp-qty_scm    = ls_pur-qty_scm.
    ls_disp-gail_id    = ls_pur-gail_id.
    ls_disp-ongc_id    = ls_pur-ongc_id.

    " Determine if row is excluded from nomination (GJ state or zero qty)
    IF ls_pur-state_code = gc_excl_state OR ls_pur-qty_scm = 0.
      ls_disp-exclude = 'X'.
      ls_disp-sel     = ' '.
      " Disable both SEL checkbox and CHARG edit for excluded rows
      CLEAR ls_styl.
      ls_styl-fieldname = 'SEL'.
      ls_styl-style     = cl_gui_alv_grid=>mc_style_disabled.
      APPEND ls_styl TO ls_disp-celltab.
      ls_styl-fieldname = 'CHARG'.
      ls_styl-style     = cl_gui_alv_grid=>mc_style_disabled.
      APPEND ls_styl TO ls_disp-celltab.
      " Light grey row background for visual distinction
      CLEAR ls_col.
      ls_col-fname     = 'EXCLUDE'.
      ls_col-color-col = 7.
      ls_col-color-int = 0.
      APPEND ls_col TO ls_disp-t_color.
    ELSE.
      ls_disp-exclude = ' '.
      ls_disp-sel     = ' '.
      CLEAR ls_styl.
      ls_styl-fieldname = 'CHARG'.
      ls_styl-style     = cl_gui_alv_grid=>mc_style_enabled.
      APPEND ls_styl TO ls_disp-celltab.

      PERFORM derive_outline_agreement
        USING    ls_pur-locid ls_pur-material ls_pur-gas_day ls_pur-state_code
        CHANGING ls_disp-outline_agr ls_disp-oa_missing.

      PERFORM derive_batch USING ls_pur-material ls_pur-state_code CHANGING ls_disp-charg.

      IF ls_disp-oa_missing = abap_true.
        CLEAR ls_col.
        ls_col-fname     = 'OUTLINE_AGR'.
        ls_col-color-col = 6.
        ls_col-color-int = 1.
        APPEND ls_col TO ls_disp-t_color.
      ENDIF.
    ENDIF.

    APPEND ls_disp TO gt_display.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
* FORM prefetch_reference_data — 5 bulk SELECTs replacing N*M queries
*----------------------------------------------------------------------*
FORM prefetch_reference_data USING it_pur TYPE STANDARD TABLE.
  DATA: ls_pur     TYPE ty_pur,
        ls_mot     TYPE ty_mot_cache,
        ls_sdate   LIKE LINE OF s_date,
        lv_min_dt  TYPE d,
        lv_max_dt  TYPE d,
        lr_state   TYPE RANGE OF regio,
        ls_rstate  LIKE LINE OF lr_state,
        lr_matnr   TYPE RANGE OF matnr,
        ls_rmatnr  LIKE LINE OF lr_matnr,
        lr_vbeln   TYPE RANGE OF ebeln,
        ls_rvbeln  LIKE LINE OF lr_vbeln.

  " Collect unique states and materials for range conditions
  LOOP AT it_pur INTO ls_pur.
    ls_rstate-sign = 'I'. ls_rstate-option = 'EQ'.
    ls_rstate-low  = ls_pur-state_code.
    APPEND ls_rstate TO lr_state.
    ls_rmatnr-sign = 'I'. ls_rmatnr-option = 'EQ'.
    ls_rmatnr-low  = ls_pur-material.
    APPEND ls_rmatnr TO lr_matnr.
  ENDLOOP.
  SORT lr_state  BY low. DELETE ADJACENT DUPLICATES FROM lr_state  COMPARING low.
  SORT lr_matnr  BY low. DELETE ADJACENT DUPLICATES FROM lr_matnr  COMPARING low.

  " Min/max gas day from selection screen
  LOOP AT s_date INTO ls_sdate WHERE sign = 'I'.
    IF lv_min_dt IS INITIAL OR ls_sdate-low < lv_min_dt.
      lv_min_dt = ls_sdate-low.
    ENDIF.
    IF ls_sdate-high > lv_max_dt. lv_max_dt = ls_sdate-high. ENDIF.
    IF ls_sdate-option = 'EQ' AND ls_sdate-low > lv_max_dt.
      lv_max_dt = ls_sdate-low.
    ENDIF.
  ENDLOOP.

  " 1. T001W: all relevant plants by state
  REFRESH gt_t001w_c.
  SELECT werks regio FROM t001w INTO CORRESPONDING FIELDS OF TABLE gt_t001w_c
    WHERE regio IN lr_state AND werks LIKE '2%'.
  SORT gt_t001w_c BY regio werks.

  " 2. OIJ_EL_DOC_MOT: all OAs overlapping the selected date range
  REFRESH gt_mot_c.
  SELECT vbeln matnr locid fromdate todate FROM oij_el_doc_mot
    INTO CORRESPONDING FIELDS OF TABLE gt_mot_c
    WHERE delind    <> 'X'
      AND fromdate  <= lv_max_dt
      AND todate    >= lv_min_dt.
  SORT gt_mot_c BY matnr locid.

  " 3. Build VBELN range from MOT results
  LOOP AT gt_mot_c INTO ls_mot.
    ls_rvbeln-sign = 'I'. ls_rvbeln-option = 'EQ'.
    ls_rvbeln-low  = ls_mot-vbeln.
    APPEND ls_rvbeln TO lr_vbeln.
  ENDLOOP.
  SORT lr_vbeln BY low. DELETE ADJACENT DUPLICATES FROM lr_vbeln COMPARING low.

  " 4. EKKO+EKPO: all relevant OA lines (DQ tax, not deleted)
  REFRESH gt_ekoa_c.
  IF lr_vbeln IS NOT INITIAL AND gt_t001w_c IS NOT INITIAL.
    SELECT ekko~ebeln ekko~bedat ekpo~werks
      FROM ekko INNER JOIN ekpo ON ekpo~ebeln = ekko~ebeln
      INTO CORRESPONDING FIELDS OF TABLE gt_ekoa_c
      WHERE ekko~ebeln IN lr_vbeln
        AND ekpo~lvorm <> 'X'
        AND ekpo~mwskz =  'DQ'
        AND ekko~loekz =  ' '.
    SORT gt_ekoa_c BY ebeln werks.
  ENDIF.

  " 5. MCHA: all batches for relevant materials (not marked for deletion)
  REFRESH gt_mcha_c.
  IF lr_matnr IS NOT INITIAL.
    SELECT matnr werks charg ersda FROM mcha
      INTO CORRESPONDING FIELDS OF TABLE gt_mcha_c
      WHERE matnr IN lr_matnr AND lvorm = ' '.
    SORT gt_mcha_c BY matnr ersda DESCENDING.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM derive_outline_agreement — uses pre-fetched cache (no DB calls)
*----------------------------------------------------------------------*
FORM derive_outline_agreement
  USING    iv_locid   TYPE char10
           iv_matnr   TYPE matnr
           iv_date    TYPE aedat
           iv_state   TYPE char2
  CHANGING cv_vbeln   TYPE ebeln
           cv_missing TYPE char1.

  DATA: ls_mot        TYPE ty_mot_cache,
        ls_t001w      TYPE ty_t001w_cache,
        ls_ek         TYPE ty_ekoa_cache,
        lt_cand       TYPE STANDARD TABLE OF ebeln,
        lt_werks      TYPE STANDARD TABLE OF werks_d,
        lv_vbeln      TYPE ebeln,
        lv_werks      TYPE werks_d,
        lv_best_vbeln TYPE ebeln,
        lv_best_bedat TYPE d.

  CLEAR: cv_vbeln, cv_missing.

  " 5a: find candidate OAs from pre-fetched MOT data (in memory)
  LOOP AT gt_mot_c INTO ls_mot WHERE matnr = iv_matnr AND locid = iv_locid.
    IF iv_date >= ls_mot-fromdate AND iv_date <= ls_mot-todate.
      APPEND ls_mot-vbeln TO lt_cand.
    ENDIF.
  ENDLOOP.
  IF lt_cand IS INITIAL. cv_missing = abap_true. RETURN. ENDIF.

  " 5b: get plants for state from pre-fetched T001W
  LOOP AT gt_t001w_c INTO ls_t001w WHERE regio = iv_state.
    APPEND ls_t001w-werks TO lt_werks.
  ENDLOOP.
  IF lt_werks IS INITIAL. cv_missing = abap_true. RETURN. ENDIF.

  " 5c+5d: find best OA by latest BEDAT from pre-fetched EKKO+EKPO
  LOOP AT lt_cand INTO lv_vbeln.
    LOOP AT lt_werks INTO lv_werks.
      LOOP AT gt_ekoa_c INTO ls_ek WHERE ebeln = lv_vbeln AND werks = lv_werks.
        IF ls_ek-bedat > lv_best_bedat.
          lv_best_bedat = ls_ek-bedat.
          lv_best_vbeln = ls_ek-ebeln.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  IF lv_best_vbeln IS NOT INITIAL.
    cv_vbeln = lv_best_vbeln.
  ELSE.
    cv_missing = abap_true.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM derive_batch — uses pre-fetched MCHA cache (no DB calls)
*----------------------------------------------------------------------*
FORM derive_batch
  USING    iv_matnr TYPE matnr
           iv_state TYPE char2
  CHANGING cv_charg TYPE charg_d.
  DATA: ls_t001w TYPE ty_t001w_cache,
        ls_mcha  TYPE ty_mcha_cache,
        lt_werks TYPE STANDARD TABLE OF werks_d,
        lv_werks TYPE werks_d.
  CLEAR cv_charg.

  " Get state-specific plants from cache
  LOOP AT gt_t001w_c INTO ls_t001w WHERE regio = iv_state.
    APPEND ls_t001w-werks TO lt_werks.
  ENDLOOP.

  " Find latest batch for this material (cache sorted by ersda DESC)
  LOOP AT gt_mcha_c INTO ls_mcha WHERE matnr = iv_matnr.
    IF lt_werks IS NOT INITIAL.
      READ TABLE lt_werks WITH KEY table_line = ls_mcha-werks TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0. CONTINUE. ENDIF.
    ENDIF.
    cv_charg = ls_mcha-charg. RETURN.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
* FORM get_valid_batches_for_material  - for F4 dropdown
*----------------------------------------------------------------------*
FORM get_valid_batches_for_material
  USING    iv_matnr  TYPE matnr
           iv_state  TYPE char2
  CHANGING ct_batch  TYPE STANDARD TABLE.
  DATA: lt_mcha  TYPE STANDARD TABLE OF mcha,
        ls_mcha  TYPE mcha,
        ls_val   TYPE ty_batch_vals,
        lt_werks TYPE STANDARD TABLE OF werks_d,
        lv_werks TYPE werks_d.
  REFRESH ct_batch.
  SELECT werks FROM t001w INTO TABLE lt_werks
    WHERE werks LIKE '2%' AND regio = iv_state.
  IF lt_werks IS NOT INITIAL.
    SORT lt_werks. DELETE ADJACENT DUPLICATES FROM lt_werks.
    LOOP AT lt_werks INTO lv_werks.
      SELECT matnr werks charg ersda lvorm FROM mcha
        APPENDING CORRESPONDING FIELDS OF TABLE lt_mcha
        WHERE matnr = iv_matnr AND werks = lv_werks AND lvorm = ' '.
    ENDLOOP.
  ELSE.
    SELECT matnr werks charg ersda lvorm FROM mcha
      INTO CORRESPONDING FIELDS OF TABLE lt_mcha
      WHERE matnr = iv_matnr AND lvorm = ' '.
  ENDIF.
  SORT lt_mcha BY ersda DESCENDING.
  LOOP AT lt_mcha INTO ls_mcha.
    ls_val-charg = ls_mcha-charg.
    ls_val-matnr = ls_mcha-matnr.
    ls_val-werks = ls_mcha-werks.
    ls_val-ersda = ls_mcha-ersda.
    APPEND ls_val TO ct_batch.
    CLEAR ls_val.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
* FORM display_alv_grid  — REUSE_ALV_GRID_DISPLAY_LVC, no screen painter
*----------------------------------------------------------------------*
FORM display_alv_grid.
  DATA: lv_title   TYPE lvc_title,
        ls_variant TYPE disvariant,
        ls_sloc    LIKE LINE OF s_locid,
        ls_sdate   LIKE LINE OF s_date,
        lv_locid   TYPE char40,
        lv_dates   TYPE char40.

  PERFORM build_fieldcat.
  PERFORM set_alv_layout.

  " Build grid title with selected location/date info
  READ TABLE s_locid INDEX 1 INTO ls_sloc.
  IF sy-subrc = 0. lv_locid = ls_sloc-low. ENDIF.
  READ TABLE s_date  INDEX 1 INTO ls_sdate.
  IF sy-subrc = 0.
    CONCATENATE ls_sdate-low '-' ls_sdate-high INTO lv_dates.
  ENDIF.
  CONCATENATE 'Purchase Nomination - ONGC B2B'
              '  |  Location:' lv_locid
              '  |  Period:' lv_dates
              INTO lv_title SEPARATED BY ' '.
  CONDENSE lv_title.

  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      i_grid_title             = lv_title
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fcat
      i_save                   = 'A'
      is_variant               = ls_variant
    TABLES
      t_outtab                 = gt_display
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Error displaying ALV grid.' TYPE 'E'.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM set_pf_status — sets PF status with custom buttons
* Requires PF status 'YRGG015' created in SE41 for this program,
* copied from SAPLSLVC_FULLSCREEN/STANDARD_FULLSCREEN with BCMASS+CRENOM
*----------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS 'YRGG015' EXCLUDING rt_extab.
  " Register data_changed handler here (fires during ALV initialization)
  IF go_alv IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING e_grid = go_alv.
  ENDIF.
  IF go_alv IS NOT INITIAL AND go_alv_handler IS INITIAL.
    CREATE OBJECT go_alv_handler.
    SET HANDLER go_alv_handler->on_main_data_changed FOR go_alv.
    go_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM build_fieldcat
*----------------------------------------------------------------------*
FORM build_fieldcat.
  DATA: ls_fcat TYPE lvc_s_fcat.

  " SEL - checkbox with hotspot for group toggle
  CLEAR ls_fcat.
  ls_fcat-fieldname = 'SEL'.
  ls_fcat-coltext   = 'Sel'.
  ls_fcat-seltext   = 'Select'.
  ls_fcat-checkbox  = abap_true.
  ls_fcat-hotspot   = abap_true.
  ls_fcat-outputlen = 4.
  APPEND ls_fcat TO gt_fcat.

  " EXCLUDE - marks rows not eligible for nomination (GJ state or zero qty)
  CLEAR ls_fcat.
  ls_fcat-fieldname = 'EXCLUDE'.
  ls_fcat-coltext   = 'Exclude'.
  ls_fcat-seltext   = 'Excluded'.
  ls_fcat-outputlen = 8.
  ls_fcat-checkbox  = abap_true.
  APPEND ls_fcat TO gt_fcat.

  " GAS_DAY
  CLEAR ls_fcat.
  ls_fcat-fieldname = 'GAS_DAY'.
  ls_fcat-coltext   = 'Gas Day'.
  ls_fcat-seltext   = 'Gas Day'.
  ls_fcat-outputlen = 12.
  APPEND ls_fcat TO gt_fcat.

  " LOCID
  CLEAR ls_fcat.
  ls_fcat-fieldname = 'LOCID'.
  ls_fcat-coltext   = 'Location'.
  ls_fcat-seltext   = 'Location'.
  ls_fcat-outputlen = 12.
  APPEND ls_fcat TO gt_fcat.

  " MATERIAL
  CLEAR ls_fcat.
  ls_fcat-fieldname = 'MATERIAL'.
  ls_fcat-coltext   = 'Material'.
  ls_fcat-seltext   = 'Material'.
  ls_fcat-outputlen = 20.
  APPEND ls_fcat TO gt_fcat.

  " STATE_CODE
  CLEAR ls_fcat.
  ls_fcat-fieldname = 'STATE_CODE'.
  ls_fcat-coltext   = 'State'.
  ls_fcat-seltext   = 'State Code'.
  ls_fcat-outputlen = 6.
  APPEND ls_fcat TO gt_fcat.

  " QTY_SCM - right-aligned quantity
  CLEAR ls_fcat.
  ls_fcat-fieldname = 'QTY_SCM'.
  ls_fcat-coltext   = 'Qty (SCM)'.
  ls_fcat-seltext   = 'Quantity SCM'.
  ls_fcat-outputlen = 16.
  ls_fcat-datatype  = 'QUAN'.
  ls_fcat-no_sign   = abap_true.
  APPEND ls_fcat TO gt_fcat.

  " GAIL_ID
  CLEAR ls_fcat.
  ls_fcat-fieldname = 'GAIL_ID'.
  ls_fcat-coltext   = 'GAIL ID'.
  ls_fcat-seltext   = 'GAIL ID'.
  ls_fcat-outputlen = 22.
  APPEND ls_fcat TO gt_fcat.

  " ONGC_ID
  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ONGC_ID'.
  ls_fcat-coltext   = 'ONGC ID'.
  ls_fcat-seltext   = 'ONGC ID'.
  ls_fcat-outputlen = 22.
  APPEND ls_fcat TO gt_fcat.

  " OUTLINE_AGR
  CLEAR ls_fcat.
  ls_fcat-fieldname = 'OUTLINE_AGR'.
  ls_fcat-coltext   = 'Outline Agreement'.
  ls_fcat-seltext   = 'Outline Agreement'.
  ls_fcat-outputlen = 14.
  APPEND ls_fcat TO gt_fcat.

  " CHARG - editable batch field
  CLEAR ls_fcat.
  ls_fcat-fieldname = 'CHARG'.
  ls_fcat-coltext   = 'Batch'.
  ls_fcat-seltext   = 'Batch Number'.
  ls_fcat-outputlen = 12.
  ls_fcat-edit      = abap_true.
  APPEND ls_fcat TO gt_fcat.

  " Technical fields (hidden)
  CLEAR ls_fcat.
  ls_fcat-fieldname = 'T_COLOR'. ls_fcat-tech = abap_true. APPEND ls_fcat TO gt_fcat.
  CLEAR ls_fcat.
  ls_fcat-fieldname = 'CELLTAB'. ls_fcat-tech = abap_true. APPEND ls_fcat TO gt_fcat.
ENDFORM.

*----------------------------------------------------------------------*
* FORM set_alv_layout
*----------------------------------------------------------------------*
FORM set_alv_layout.
  gs_layout-cwidth_opt  = abap_true.
  gs_layout-zebra       = abap_true.
  gs_layout-edit        = abap_true.
  gs_layout-ctab_fname  = 'T_COLOR'.
  gs_layout-stylefname  = 'CELLTAB'.
  gs_layout-no_rowmark  = abap_true.
ENDFORM.

*----------------------------------------------------------------------*
* FORM alv_toolbar  — adds BCMASS and CRENOM buttons to ALV toolbar
*----------------------------------------------------------------------*
FORM alv_toolbar USING e_object      TYPE REF TO cl_alv_event_toolbar_set
                        e_interactive TYPE char1.
  DATA: ls_tb TYPE stb_button.
  IF gv_toolbar_done = abap_true. RETURN. ENDIF.
  CLEAR ls_tb.
  ls_tb-function  = 'BCMASS'.
  ls_tb-icon      = icon_batch.
  ls_tb-quickinfo = 'Batch Change in Mass'.
  ls_tb-text      = 'Batch Change'.
  INSERT ls_tb INTO e_object->mt_toolbar INDEX 1.
  CLEAR ls_tb.
  ls_tb-butn_type = 3.
  INSERT ls_tb INTO e_object->mt_toolbar INDEX 2.
  CLEAR ls_tb.
  ls_tb-function  = 'CRENOM'.
  ls_tb-icon      = icon_execute_object.
  ls_tb-quickinfo = 'Create Nomination'.
  ls_tb-text      = 'Create Nomination'.
  INSERT ls_tb INTO e_object->mt_toolbar INDEX 3.
  gv_toolbar_done = abap_true.
ENDFORM.

*----------------------------------------------------------------------*
* FORM user_command  — handles toolbar buttons and hotspot clicks
*----------------------------------------------------------------------*
FORM user_command USING r_ucomm    TYPE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  IF go_alv IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING e_grid = go_alv.
  ENDIF.
  IF go_alv IS NOT INITIAL.
    go_alv->check_changed_data( ).
  ENDIF.
  CASE r_ucomm.
    WHEN 'BCMASS'. PERFORM handle_batch_mass_change.
    WHEN 'CRENOM'. PERFORM handle_create_nomination.
    WHEN '&IC1'.
      IF rs_selfield-fieldname = 'SEL'.
        PERFORM toggle_sel_for_row USING rs_selfield-tabindex.
      ENDIF.
  ENDCASE.
  rs_selfield-refresh = abap_true.
ENDFORM.


*----------------------------------------------------------------------*
* FORM toggle_sel_for_row  — toggle SEL for all rows sharing locid+date
*----------------------------------------------------------------------*
FORM toggle_sel_for_row USING iv_index TYPE i.
  DATA: ls_disp   TYPE ty_display,
        lv_locid  TYPE char10,
        lv_date   TYPE aedat,
        lv_newsel TYPE char1.
  READ TABLE gt_display INDEX iv_index INTO ls_disp.
  IF sy-subrc <> 0. RETURN. ENDIF.
  IF ls_disp-exclude = 'X'. RETURN. ENDIF.   " excluded rows cannot be selected
  IF ls_disp-sel = abap_true.
    lv_newsel = ' '.
  ELSE.
    lv_newsel = abap_true.
  ENDIF.
  lv_locid = ls_disp-locid.
  lv_date  = ls_disp-gas_day.
  LOOP AT gt_display INTO ls_disp.
    IF ls_disp-locid = lv_locid AND ls_disp-gas_day = lv_date
       AND ls_disp-exclude <> 'X'.
      ls_disp-sel = lv_newsel.
      MODIFY gt_display FROM ls_disp.
    ENDIF.
  ENDLOOP.
  IF go_alv IS NOT INITIAL.
    go_alv->refresh_table_display( ).
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM handle_create_nomination
*----------------------------------------------------------------------*
FORM handle_create_nomination.
  DATA: lt_sel      TYPE tt_display,
        ls_disp     TYPE ty_display,
        lt_main     TYPE tt_main,
        ls_main     TYPE ty_main,
        lt_errors   TYPE tt_main,
        i_rspartab  TYPE STANDARD TABLE OF rsparams,
        wa_rspartab LIKE LINE OF i_rspartab,
        ls_sdate    LIKE LINE OF s_date,
        ls_slocid   LIKE LINE OF s_locid.

  " Collect selected rows
  LOOP AT gt_display INTO ls_disp WHERE sel = abap_true.
    APPEND ls_disp TO lt_sel.
  ENDLOOP.
  IF lt_sel IS INITIAL.
    MESSAGE 'Please select at least one row.' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  " Pre-flight checks
  LOOP AT lt_sel INTO ls_disp.
    IF ls_disp-outline_agr IS INITIAL.
      MESSAGE 'Selected row(s) have no Outline Agreement.' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    IF ls_disp-charg IS INITIAL.
      MESSAGE |Batch missing for { ls_disp-material }. Assign before creating nomination.| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDLOOP.

  " Build i_main: tsyst blank (batch_validate fills from oij_el_doc_mot),
  " rank = 1 for all rows (equal priority in ONGC B2B purchase)
  LOOP AT lt_sel INTO ls_disp.
    CLEAR ls_main.
    ls_main-tsyst = ''.
    ls_main-vbeln = ls_disp-outline_agr.
    ls_main-date  = ls_disp-gas_day.
    ls_main-locid = ls_disp-locid.
    ls_main-matnr = ls_disp-material.
    ls_main-menge = ls_disp-qty_scm.
    ls_main-unit  = gc_sm3.
    ls_main-charg = ls_disp-charg.
    ls_main-rank  = 1.
    APPEND ls_main TO lt_main.
  ENDLOOP.

  " Export i_main so YRXR036_PURC_NOM_G1 imports it at START-OF-SELECTION
  EXPORT lt_main TO MEMORY ID gc_memory_id.

  " R_EXCEL = 'X' triggers batch_validate -> get_nomination -> createfromdata in YRGR040
  CLEAR wa_rspartab.
  wa_rspartab-selname = 'R_EXCEL'.
  wa_rspartab-kind    = 'P'.
  wa_rspartab-low     = abap_true.
  APPEND wa_rspartab TO i_rspartab.

  " Pass S_DATE range so YRGR040 filters nominations by the same fortnight
  LOOP AT s_date INTO ls_sdate.
    CLEAR wa_rspartab.
    wa_rspartab-selname = 'S_DATE'.
    wa_rspartab-kind    = 'S'.
    wa_rspartab-sign    = ls_sdate-sign.
    wa_rspartab-option  = ls_sdate-option.
    wa_rspartab-low     = ls_sdate-low.
    wa_rspartab-high    = ls_sdate-high.
    APPEND wa_rspartab TO i_rspartab.
  ENDLOOP.

  " Pass P_LOCID1 for each location selected on YRGG015 selection screen
  LOOP AT s_locid INTO ls_slocid WHERE sign = 'I' AND option = 'EQ'.
    CLEAR wa_rspartab.
    wa_rspartab-selname = 'P_LOCID1'.
    wa_rspartab-kind    = 'P'.
    wa_rspartab-low     = ls_slocid-low.
    APPEND wa_rspartab TO i_rspartab.
  ENDLOOP.

  SUBMIT yrxr036_purc_nom_g1
    USING SELECTION-SCREEN '1000'
    WITH SELECTION-TABLE i_rspartab
    AND RETURN.

  " Retrieve any errors YRGR040 exported back
  IMPORT lt_errors FROM MEMORY ID gc_err_mem_id.
  FREE MEMORY ID gc_memory_id.
  FREE MEMORY ID gc_err_mem_id.

  IF lt_errors IS NOT INITIAL.
    PERFORM display_nomination_errors USING lt_errors.
  ELSE.
    MESSAGE 'Nominations created successfully.' TYPE 'S'.
    go_alv->refresh_table_display( ).
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM display_nomination_errors
*----------------------------------------------------------------------*
FORM display_nomination_errors USING it_errors TYPE tt_main.
  DATA: lt_fcat   TYPE lvc_t_fcat,
        ls_fcat   TYPE lvc_s_fcat,
        ls_layout TYPE lvc_s_layo,
        ls_e      TYPE ty_main.

  ls_fcat-fieldname = 'LOCID'. ls_fcat-coltext = 'Location'. APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  ls_fcat-fieldname = 'MATNR'. ls_fcat-coltext = 'Material'. APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  ls_fcat-fieldname = 'DATE'.  ls_fcat-coltext = 'Date'.     APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  ls_fcat-fieldname = 'VBELN'. ls_fcat-coltext = 'OA'.       APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  ls_fcat-fieldname = 'MSG'.   ls_fcat-coltext = 'Message'.  ls_fcat-outputlen = 80. APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

  ls_layout-cwidth_opt = abap_true.
  DATA: lo_popup TYPE REF TO cl_gui_dialogbox_container,
        lo_aerr  TYPE REF TO cl_gui_alv_grid.
  CREATE OBJECT lo_popup
    EXPORTING caption = 'Nomination Errors' top = 10 left = 10 width = 500 height = 300
    EXCEPTIONS OTHERS = 1.
  IF sy-subrc <> 0.
    LOOP AT it_errors INTO ls_e WHERE msgty = 'E' OR msgty = 'A'.
      MESSAGE ls_e-msg TYPE 'S' DISPLAY LIKE 'E'.
    ENDLOOP.
    RETURN.
  ENDIF.
  CREATE OBJECT lo_aerr EXPORTING i_parent = lo_popup EXCEPTIONS OTHERS = 1.
  lo_aerr->set_table_for_first_display(
    EXPORTING is_layout = ls_layout
    CHANGING  it_outtab = it_errors it_fieldcatalog = lt_fcat
    EXCEPTIONS OTHERS = 1 ).
  MESSAGE 'Nomination errors found. See error popup.' TYPE 'S' DISPLAY LIKE 'W'.
ENDFORM.

*----------------------------------------------------------------------*
* FORM handle_batch_mass_change
* Gap 2: distinct messages for "nothing selected" vs "no batch-managed"
* Gap 3: single combined dialog for all materials instead of sequential popups
*----------------------------------------------------------------------*
FORM handle_batch_mass_change.
  DATA: ls_disp     TYPE ty_display,
        ls_assign   TYPE ty_batch_assign,
        lv_xchpf    TYPE mara-xchpf,
        lv_rows_sel TYPE i,
        lt_fcat     TYPE lvc_t_fcat,
        ls_fcat     TYPE lvc_s_fcat,
        ls_layout   TYPE lvc_s_layo.

  REFRESH gt_batch_assign.
  lv_rows_sel = 0.

  LOOP AT gt_display INTO ls_disp WHERE sel = abap_true.
    ADD 1 TO lv_rows_sel.
    READ TABLE gt_batch_assign WITH KEY matnr = ls_disp-material TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      SELECT SINGLE xchpf FROM mara INTO lv_xchpf WHERE matnr = ls_disp-material.
      IF sy-subrc = 0 AND lv_xchpf = 'X'.
        CLEAR ls_assign.
        ls_assign-matnr      = ls_disp-material.
        ls_assign-state_code = ls_disp-state_code.
        PERFORM derive_batch USING ls_disp-material ls_disp-state_code
                             CHANGING ls_assign-charg.
        APPEND ls_assign TO gt_batch_assign.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lv_rows_sel = 0.
    MESSAGE 'Select rows first.' TYPE 'S' DISPLAY LIKE 'W'. RETURN.
  ENDIF.
  IF gt_batch_assign IS INITIAL.
    MESSAGE 'No batch-managed materials found in selected rows.' TYPE 'S'
            DISPLAY LIKE 'W'. RETURN.
  ENDIF.

  " Single batch assignment dialog showing all materials at once (FSD Point 6)
  IF go_alv_handler IS INITIAL.
    CREATE OBJECT go_alv_handler.
  ENDIF.
  CREATE OBJECT go_batch_popup
    EXPORTING
      caption    = 'Batch Assignment'
      top        = 5
      left       = 5
      width      = 500
      height     = 300
    EXCEPTIONS OTHERS = 1.
  IF sy-subrc <> 0.
    MESSAGE 'Error opening batch assignment dialog.' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.
  SET HANDLER go_alv_handler->on_batch_dlg_close FOR go_batch_popup.

  CREATE OBJECT go_batch_alv
    EXPORTING i_parent = go_batch_popup
    EXCEPTIONS OTHERS = 1.
  IF sy-subrc <> 0. RETURN. ENDIF.
  SET HANDLER go_alv_handler->on_batch_data_changed FOR go_batch_alv.

  ls_fcat-fieldname = 'MATNR'. ls_fcat-coltext = 'Material'. ls_fcat-outputlen = 18.
  APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  ls_fcat-fieldname = 'CHARG'. ls_fcat-coltext = 'Batch'. ls_fcat-outputlen = 10.
  ls_fcat-edit = abap_true.
  APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

  ls_layout-cwidth_opt = abap_true.
  ls_layout-edit       = abap_true.

  go_batch_alv->set_table_for_first_display(
    EXPORTING is_layout       = ls_layout
    CHANGING  it_outtab       = gt_batch_assign
              it_fieldcatalog = lt_fcat
    EXCEPTIONS OTHERS = 1 ).

  go_batch_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
ENDFORM.

*----------------------------------------------------------------------*
* FORM schedule_background_job  (ZC_GMS_CORE_TEAM only)
*----------------------------------------------------------------------*
FORM schedule_background_job.
  DATA: lv_jobname  TYPE tbtcjob-jobname VALUE 'YRGG015_PURC_NOM_ONGC_B2B',
        lv_jobcount TYPE tbtcjob-jobcount.
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING  jobname          = lv_jobname
    IMPORTING  jobcount         = lv_jobcount
    EXCEPTIONS cant_create_job  = 1 invalid_job_data = 2 jobname_missing = 3 OTHERS = 4.
  IF sy-subrc <> 0. MESSAGE 'Error creating background job.' TYPE 'E'. RETURN. ENDIF.
  SUBMIT yrgg015_purc_nom_ongc_b2b
    WITH s_date  IN s_date
    WITH s_locid IN s_locid
    VIA JOB lv_jobname NUMBER lv_jobcount
    AND RETURN.
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING  jobcount             = lv_jobcount jobname = lv_jobname strtimmed = abap_true
    EXCEPTIONS cant_start_immediate = 1 invalid_startdate = 2 jobname_missing = 3
               job_close_failed = 4 job_nosteps = 5 OTHERS = 6.
  IF sy-subrc = 0.
    MESSAGE |Background job { lv_jobname } scheduled.| TYPE 'S'.
  ELSE.
    MESSAGE 'Error scheduling background job.' TYPE 'E'.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM create_all_nominations_bg
* Called when sy-batch='X': builds nominations for ALL valid display rows
* and SUBMITs YRGR040 without any GUI interaction (FSD Point 14)
*----------------------------------------------------------------------*
FORM create_all_nominations_bg.
  DATA: ls_disp     TYPE ty_display,
        lt_main     TYPE tt_main,
        ls_main     TYPE ty_main,
        i_rspartab  TYPE STANDARD TABLE OF rsparams,
        wa_rspartab LIKE LINE OF i_rspartab,
        ls_sdate    LIKE LINE OF s_date,
        ls_slocid   LIKE LINE OF s_locid.

  LOOP AT gt_display INTO ls_disp.
    IF ls_disp-outline_agr IS INITIAL OR ls_disp-charg IS INITIAL. CONTINUE. ENDIF.
    CLEAR ls_main.
    ls_main-vbeln = ls_disp-outline_agr.
    ls_main-date  = ls_disp-gas_day.
    ls_main-locid = ls_disp-locid.
    ls_main-matnr = ls_disp-material.
    ls_main-menge = ls_disp-qty_scm.
    ls_main-unit  = gc_sm3.
    ls_main-charg = ls_disp-charg.
    ls_main-rank  = 1.
    APPEND ls_main TO lt_main.
  ENDLOOP.

  IF lt_main IS INITIAL. RETURN. ENDIF.

  EXPORT lt_main TO MEMORY ID gc_memory_id.

  CLEAR wa_rspartab.
  wa_rspartab-selname = 'R_EXCEL'. wa_rspartab-kind = 'P'. wa_rspartab-low = abap_true.
  APPEND wa_rspartab TO i_rspartab.

  LOOP AT s_date INTO ls_sdate.
    CLEAR wa_rspartab.
    wa_rspartab-selname = 'S_DATE'. wa_rspartab-kind    = 'S'.
    wa_rspartab-sign    = ls_sdate-sign. wa_rspartab-option = ls_sdate-option.
    wa_rspartab-low     = ls_sdate-low.  wa_rspartab-high   = ls_sdate-high.
    APPEND wa_rspartab TO i_rspartab.
  ENDLOOP.

  LOOP AT s_locid INTO ls_slocid WHERE sign = 'I' AND option = 'EQ'.
    CLEAR wa_rspartab.
    wa_rspartab-selname = 'P_LOCID1'. wa_rspartab-kind = 'P'.
    wa_rspartab-low     = ls_slocid-low.
    APPEND wa_rspartab TO i_rspartab.
  ENDLOOP.

  SUBMIT yrxr036_purc_nom_g1
    USING SELECTION-SCREEN '1000'
    WITH SELECTION-TABLE i_rspartab
    AND RETURN.

  FREE MEMORY ID gc_memory_id.
ENDFORM.

