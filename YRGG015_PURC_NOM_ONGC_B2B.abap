*&---------------------------------------------------------------------*
*& Report  YRGG015_PURC_NOM_ONGC_B2B
*& Purchase Nomination Creation - ONGC B2B
*& T-Code: YRGG015
*&---------------------------------------------------------------------*
REPORT yrgg015_purc_nom_ongc_b2b MESSAGE-ID oo
                                  LINE-SIZE 255
                                  NO STANDARD PAGE HEADING.

TABLES: oijnomi.

TYPE-POOLS: icon, slis.

*----------------------------------------------------------------------*
* TYPE DECLARATIONS
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_pur,
         gas_day     TYPE aedat,
         locid       TYPE char10,
         material    TYPE char30,
         state_code  TYPE regio,
         qty_scm     TYPE p LENGTH 13 DECIMALS 3,
         qty_mbg     TYPE p LENGTH 13 DECIMALS 3,
         gail_id     TYPE char14,
         ongc_id     TYPE char20,
         ongc_mater  TYPE char30,
         deleted     TYPE char1,
         exclude     TYPE char1,
       END OF ty_pur.

TYPES: BEGIN OF ty_main,
         tsyst       TYPE oij_tsyst,
         vbeln       TYPE vbeln,
         date        TYPE sy-datum,
         locid       TYPE oij_locid,
         matnr       TYPE matnr,
         menge       TYPE oij_menge,
         unit        TYPE oij_uniti,
         charg       TYPE charg_d,
         rank        TYPE i,
         ancv        TYPE yyncv,
         agcv        TYPE yygcv,
         nomtk       TYPE oij_nomtk,
         nomit       TYPE oij_item,
         st_qty      TYPE oijnomi-yyoij_dpimb_qty,
         del_ind     TYPE char1,
         flag        TYPE char1,
         post_status TYPE char30,
         ticketnr    TYPE oij_tktnr,
         ticket_key  TYPE oij_el_tkt_key,
         ticket_item TYPE oij_el_tkt_posnr,
         color(4),
         error_msg(170) TYPE c,
       END OF ty_main.

" Error log structure — must match ty_log in YRXR036_PURC_NOM_G1
TYPES: BEGIN OF ty_log,
         tsyst    TYPE oij_tsyst,
         ebeln    TYPE ekpo-ebeln,
         date     TYPE sy-datum,
         locid    TYPE oij_locid,
         matnr    TYPE matnr,
         charg    TYPE charg_d,
         message(100),
       END OF ty_log.

TYPES: BEGIN OF ty_display,
         sel         TYPE char1,
         exclude     TYPE char1,   " display: mirrors YRGA_CST_PUR exclude flag
         is_excl     TYPE char1,   " logic:   'X' for any excluded row (state or flag)
         row_color   TYPE char4,
         gas_day     TYPE aedat,
         locid       TYPE char10,
         material    TYPE char30,
         state_code  TYPE regio,
         qty_scm     TYPE p LENGTH 13 DECIMALS 3,
         qty_mbg     TYPE p LENGTH 13 DECIMALS 3,
         gail_id     TYPE char14,
         ongc_id     TYPE char20,
         ongc_mater  TYPE char30,
         outline_agr TYPE ebeln,
         oa_locid    TYPE char10,
         oa_werks    TYPE werks_d,
         oa_matnr    TYPE char30,
         oa_desc     TYPE char40,
         oa_tsyst    TYPE oij_tsyst,
         oa_batch    TYPE charg_d,
         charg       TYPE charg_d,
         nomtk       TYPE oij_nomtk,
         nomit       TYPE oij_item,
         oa_missing  TYPE char1,
         celltab     TYPE lvc_t_styl,
         t_color     TYPE lvc_t_scol,
       END OF ty_display.

TYPES: BEGIN OF ty_batch_vals,
         charg TYPE charg_d,
         matnr TYPE char30,
         werks TYPE werks_d,
         ersda TYPE ersda,
       END OF ty_batch_vals.

TYPES: tt_main    TYPE STANDARD TABLE OF ty_main.
TYPES: tt_log     TYPE STANDARD TABLE OF ty_log.
TYPES: tt_display TYPE STANDARD TABLE OF ty_display.

TYPES: BEGIN OF ty_batch_assign,
         matnr       TYPE char30,
         state_code  TYPE regio,
         outline_agr TYPE ebeln,
         charg       TYPE charg_d,
       END OF ty_batch_assign.
TYPES: tt_batch_assign TYPE STANDARD TABLE OF ty_batch_assign.

" Cache types for bulk pre-fetch (performance)
TYPES: BEGIN OF ty_mot_cache,
         vbeln    TYPE ebeln,
         matnr    TYPE char30,
         locid    TYPE char10,
         tsyst    TYPE oij_tsyst,
         fromdate TYPE d,
         todate   TYPE d,
         vbtyp    TYPE char1,
       END OF ty_mot_cache.
TYPES: BEGIN OF ty_ekoa_cache,
         ebeln TYPE ebeln,
         bedat TYPE d,
         werks TYPE werks_d,
         matnr TYPE char30,
         txz01 TYPE char40,
       END OF ty_ekoa_cache.
TYPES: BEGIN OF ty_t001w_cache,
         werks TYPE werks_d,
         regio TYPE regio,
       END OF ty_t001w_cache.
TYPES: BEGIN OF ty_mcha_cache,
         matnr TYPE char30,
         werks TYPE werks_d,
         charg TYPE charg_d,
         ersda TYPE d,
         lvorm TYPE char1,
       END OF ty_mcha_cache.
TYPES: BEGIN OF ty_mara_cache,
         matnr TYPE char30,
         xchpf TYPE xchpf,
       END OF ty_mara_cache.
TYPES: BEGIN OF ty_ekbe_cache,
         ebeln TYPE ebeln,
         charg TYPE charg_d,
       END OF ty_ekbe_cache.

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
      gt_mcha_c  TYPE STANDARD TABLE OF ty_mcha_cache,
      gt_mara_c  TYPE STANDARD TABLE OF ty_mara_cache,
      gt_ekbe_c  TYPE STANDARD TABLE OF ty_ekbe_cache.

CONSTANTS: gc_memory_id  TYPE char30 VALUE 'YRGG015_NOM_DATA',
           gc_err_mem_id TYPE char30 VALUE 'YRGG015_NOM_ERRORS',
           gc_call_flag  TYPE char30 VALUE 'YRGG015_CALL_FLAG',
           gc_role_core  TYPE char30 VALUE 'ZC_GMS_CORE_TEAM',
           gc_excl_state TYPE regio  VALUE 'GJ',
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
      on_main_data_changed_finished
        FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified,
      on_main_f4
        FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data et_bad_cells e_display,
      on_batch_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,
      on_batch_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      on_batch_cmd
        FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      on_batch_f4
        FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data et_bad_cells e_display,
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
    ls_tb-function  = 'SELALL'.
    ls_tb-icon      = '@2V@'.
    ls_tb-quickinfo = 'Select All'.
    ls_tb-text      = 'Select All'.
    INSERT ls_tb INTO e_object->mt_toolbar INDEX 1.
    CLEAR ls_tb.
    ls_tb-function  = 'DSLALL'.
    ls_tb-icon      = '@2W@'.
    ls_tb-quickinfo = 'Deselect All'.
    ls_tb-text      = 'Deselect All'.
    INSERT ls_tb INTO e_object->mt_toolbar INDEX 2.
    CLEAR ls_tb.
    ls_tb-butn_type = 3.
    INSERT ls_tb INTO e_object->mt_toolbar INDEX 3.
    CLEAR ls_tb.
    ls_tb-function  = 'BCMASS'.
    ls_tb-icon      = '@EJ@'.
    ls_tb-quickinfo = 'Batch Change in Mass'.
    ls_tb-text      = 'Batch Change'.
    INSERT ls_tb INTO e_object->mt_toolbar INDEX 4.
    CLEAR ls_tb.
    ls_tb-butn_type = 3.
    INSERT ls_tb INTO e_object->mt_toolbar INDEX 5.
    CLEAR ls_tb.
    ls_tb-function  = 'CRENOM'.
    ls_tb-icon      = '@15@'.
    ls_tb-quickinfo = 'Create Nomination'.
    ls_tb-text      = 'Create Nomination'.
    INSERT ls_tb INTO e_object->mt_toolbar INDEX 6.
    gv_toolbar_done = abap_true.
  ENDMETHOD.

  METHOD on_main_data_changed.
    DATA: ls_mod   TYPE lvc_s_modi,
          ls_disp  TYPE ty_display,
          lv_vbeln TYPE ebeln.
    LOOP AT er_data_changed->mt_mod_cells INTO ls_mod.
      READ TABLE gt_display INDEX ls_mod-row_id INTO ls_disp.
      IF sy-subrc <> 0. CONTINUE. ENDIF.
      CASE ls_mod-fieldname.
        WHEN 'CHARG'.
          IF ls_mod-value IS NOT INITIAL.
            ls_disp-charg = ls_mod-value.
            MODIFY gt_display INDEX ls_mod-row_id FROM ls_disp.
          ENDIF.
        WHEN 'OUTLINE_AGR'.
          lv_vbeln = ls_mod-value.
          ls_disp-outline_agr = lv_vbeln.
          PERFORM derive_oa_fields_from_oa
            USING    lv_vbeln
            CHANGING ls_disp-oa_locid ls_disp-oa_werks ls_disp-oa_matnr ls_disp-oa_desc
                     ls_disp-oa_tsyst ls_disp-oa_batch.
          MODIFY gt_display INDEX ls_mod-row_id FROM ls_disp.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD on_main_data_changed_finished.
    DATA: ls_stbl TYPE lvc_s_stbl.
    IF e_modified = abap_true AND go_alv IS NOT INITIAL.
      ls_stbl-row = abap_true. ls_stbl-col = abap_true.
      go_alv->refresh_table_display( is_stable = ls_stbl ).
    ENDIF.
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

  METHOD on_main_f4.
    DATA: ls_disp  TYPE ty_display,
          ls_mcha  TYPE ty_mcha_cache,
          ls_ek    TYPE ty_ekoa_cache,
          lt_werks TYPE STANDARD TABLE OF werks_d,
          lt_ret   TYPE STANDARD TABLE OF ddshretval,
          ls_ret   TYPE ddshretval,
          ls_stbl  TYPE lvc_s_stbl.
    DATA: BEGIN OF ls_f4val,
            charg TYPE charg_d,
          END OF ls_f4val.
    DATA lt_f4vals LIKE TABLE OF ls_f4val.
    IF e_fieldname <> 'CHARG'. RETURN. ENDIF.
    READ TABLE gt_display INDEX es_row_no-row_id INTO ls_disp.
    IF sy-subrc <> 0 OR ls_disp-is_excl = 'X'. RETURN. ENDIF.
    " Get plants from EKPO of the OA
    LOOP AT gt_ekoa_c INTO ls_ek WHERE ebeln = ls_disp-outline_agr.
      APPEND ls_ek-werks TO lt_werks.
    ENDLOOP.
    LOOP AT gt_mcha_c INTO ls_mcha WHERE matnr = ls_disp-material.
      IF lt_werks IS NOT INITIAL.
        READ TABLE lt_werks WITH KEY table_line = ls_mcha-werks TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0. CONTINUE. ENDIF.
      ENDIF.
      ls_f4val-charg = ls_mcha-charg.
      APPEND ls_f4val TO lt_f4vals.
    ENDLOOP.
    IF lt_f4vals IS INITIAL.
      MESSAGE 'No valid batches found for this material.' TYPE 'S' DISPLAY LIKE 'W'.
      er_event_data->m_event_handled = abap_true. RETURN.
    ENDIF.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING retfield        = 'CHARG'
                value_org       = 'S'
      TABLES    value_tab       = lt_f4vals
                return_tab      = lt_ret
      EXCEPTIONS parameter_error = 1 no_values_found = 2 OTHERS = 3.
    READ TABLE lt_ret INTO ls_ret INDEX 1.
    IF sy-subrc = 0 AND ls_ret-fieldval IS NOT INITIAL.
      ls_disp-charg = ls_ret-fieldval.
      MODIFY gt_display INDEX es_row_no-row_id FROM ls_disp.
      IF go_alv IS NOT INITIAL.
        ls_stbl-row = abap_true. ls_stbl-col = abap_true.
        go_alv->refresh_table_display( is_stable = ls_stbl ).
      ENDIF.
    ENDIF.
    er_event_data->m_event_handled = abap_true.
  ENDMETHOD.

  METHOD on_batch_toolbar.
    DATA: ls_tb TYPE stb_button.
    CLEAR ls_tb.
    ls_tb-function  = 'BATCH_OK'.
    ls_tb-text      = 'Apply Batch'.
    ls_tb-quickinfo = 'Apply batch to selected rows'.
    ls_tb-icon      = '@2L@'.
    INSERT ls_tb INTO e_object->mt_toolbar INDEX 1.
  ENDMETHOD.

  METHOD on_batch_cmd.
    DATA: ls_assign TYPE ty_batch_assign,
          ls_disp   TYPE ty_display,
          ls_stbl   TYPE lvc_s_stbl.
    IF e_ucomm <> 'BATCH_OK'. RETURN. ENDIF.
    IF go_batch_alv IS NOT INITIAL.
      go_batch_alv->check_changed_data( ).
    ENDIF.
    LOOP AT gt_batch_assign INTO ls_assign WHERE charg IS NOT INITIAL.
      LOOP AT gt_display INTO ls_disp WHERE material = ls_assign-matnr AND is_excl <> 'X'.
        ls_disp-charg = ls_assign-charg.
        MODIFY gt_display INDEX sy-tabix FROM ls_disp.
      ENDLOOP.
    ENDLOOP.
    IF go_batch_alv IS NOT INITIAL.
      go_batch_alv->free( ). CLEAR go_batch_alv.
    ENDIF.
    IF go_batch_popup IS NOT INITIAL.
      go_batch_popup->free( ). CLEAR go_batch_popup.
    ENDIF.
    IF go_alv IS NOT INITIAL.
      ls_stbl-row = abap_true. ls_stbl-col = abap_true.
      go_alv->refresh_table_display( is_stable = ls_stbl ).
    ENDIF.
  ENDMETHOD.

  METHOD on_batch_f4.
    DATA: ls_assign TYPE ty_batch_assign,
          ls_mcha   TYPE ty_mcha_cache,
          ls_ek     TYPE ty_ekoa_cache,
          lt_werks  TYPE STANDARD TABLE OF werks_d,
          lt_ret    TYPE STANDARD TABLE OF ddshretval,
          ls_ret    TYPE ddshretval,
          ls_stbl   TYPE lvc_s_stbl.
    DATA: BEGIN OF ls_f4val,
            charg TYPE charg_d,
          END OF ls_f4val.
    DATA lt_f4vals LIKE TABLE OF ls_f4val.
    IF e_fieldname <> 'CHARG'. RETURN. ENDIF.
    READ TABLE gt_batch_assign INDEX es_row_no-row_id INTO ls_assign.
    IF sy-subrc <> 0. RETURN. ENDIF.
    " Get plants from EKPO of the OA
    LOOP AT gt_ekoa_c INTO ls_ek WHERE ebeln = ls_assign-outline_agr.
      APPEND ls_ek-werks TO lt_werks.
    ENDLOOP.
    LOOP AT gt_mcha_c INTO ls_mcha WHERE matnr = ls_assign-matnr.
      IF lt_werks IS NOT INITIAL.
        READ TABLE lt_werks WITH KEY table_line = ls_mcha-werks TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0. CONTINUE. ENDIF.
      ENDIF.
      ls_f4val-charg = ls_mcha-charg.
      APPEND ls_f4val TO lt_f4vals.
    ENDLOOP.
    IF lt_f4vals IS INITIAL.
      MESSAGE 'No valid batches for this material.' TYPE 'S' DISPLAY LIKE 'W'.
      er_event_data->m_event_handled = abap_true. RETURN.
    ENDIF.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING retfield        = 'CHARG'
                value_org       = 'S'
      TABLES    value_tab       = lt_f4vals
                return_tab      = lt_ret
      EXCEPTIONS parameter_error = 1 no_values_found = 2 OTHERS = 3.
    READ TABLE lt_ret INTO ls_ret INDEX 1.
    IF sy-subrc = 0 AND ls_ret-fieldval IS NOT INITIAL.
      ls_assign-charg = ls_ret-fieldval.
      MODIFY gt_batch_assign INDEX es_row_no-row_id FROM ls_assign.
      IF go_batch_alv IS NOT INITIAL.
        ls_stbl-row = abap_true. ls_stbl-col = abap_true.
        go_batch_alv->refresh_table_display( is_stable = ls_stbl ).
      ENDIF.
    ENDIF.
    er_event_data->m_event_handled = abap_true.
  ENDMETHOD.

  METHOD on_batch_dlg_close.
    " X pressed = cancel, discard changes
    IF go_batch_alv IS NOT INITIAL.
      go_batch_alv->free( ). CLEAR go_batch_alv.
    ENDIF.
    IF go_batch_popup IS NOT INITIAL.
      go_batch_popup->free( ). CLEAR go_batch_popup.
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
  " Show Background checkbox only to users with role ZO_CC_EHS.GMS_ROLE
  SELECT SINGLE uname FROM agr_users INTO @DATA(lv_chk_user)
    WHERE uname    = @sy-uname
      AND agr_name = 'ZO_CC_EHS.GMS_ROLE'.
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
  DATA: ls_locid      LIKE LINE OF s_locid,
        ls_date       LIKE LINE OF s_date,
        lv_day_lo     TYPE i,
        lv_day_hi     TYPE i,
        lv_loc        TYPE oij_locid,
        lv_today      TYPE d,
        lv_fn_end     TYPE d,
        lv_fn_day     TYPE i,
        lv_fn_end_low TYPE d.

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
    " Compute the end of the fortnight that contains the FROM date
    " FN1 = 1st-15th, FN2 = 16th-last day of month
    CLEAR lv_fn_end_low.
    IF lv_day_lo <= 15.
      lv_fn_end_low      = ls_date-low.
      lv_fn_end_low+6(2) = '15'.
    ELSE.
      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING day_in            = ls_date-low
        IMPORTING last_day_of_month = lv_fn_end_low.
    ENDIF.
    IF ls_date-high > lv_fn_end_low.
      MESSAGE e000(oo) WITH 'Date range cannot span more than one fortnight.'
                             'Max To date:' lv_fn_end_low ' '.
    ENDIF.
  ENDLOOP.

  " Validate selected dates do not exceed current fortnight end date
  lv_today  = sy-datum.
  lv_fn_day = lv_today+6(2).
  IF lv_fn_day <= 15.
    lv_fn_end      = lv_today.
    lv_fn_end+6(2) = '15'.
  ELSE.
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING day_in            = lv_today
      IMPORTING last_day_of_month = lv_fn_end.
  ENDIF.

  LOOP AT s_date INTO ls_date WHERE sign = 'I'.
    IF ( ls_date-option = 'BT' AND ls_date-high > lv_fn_end ) OR
       ( ls_date-option = 'EQ' AND ls_date-low  > lv_fn_end ).
      MESSAGE e000(oo) WITH 'Date cannot exceed current FN end date:'
                             lv_fn_end ' ' ' '.
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
         qty_in_scm AS qty_scm qty_in_mbg AS qty_mbg
         gail_id ongc_id ongc_mater deleted exclude
    FROM yrga_cst_pur
    INTO CORRESPONDING FIELDS OF TABLE lt_pur
    WHERE gas_day  IN s_date
      AND location IN s_locid
      AND deleted <> gc_deleted.

  IF sy-subrc <> 0 OR lt_pur IS INITIAL. RETURN. ENDIF.

  " Remove genuine duplicates only: same gas_day+locid+material+state_code
  " (same material for different states are NOT duplicates and must stay)
  SORT lt_pur BY gas_day locid material state_code.
  DELETE ADJACENT DUPLICATES FROM lt_pur COMPARING gas_day locid material state_code.

  " Bulk pre-fetch all reference data (5 SELECTs instead of N*M)
  PERFORM prefetch_reference_data USING lt_pur.

  LOOP AT lt_pur INTO ls_pur.
    CLEAR ls_disp.
    ls_disp-gas_day    = ls_pur-gas_day.
    ls_disp-locid      = ls_pur-locid.
    ls_disp-material   = ls_pur-material.
    ls_disp-state_code = ls_pur-state_code.
    ls_disp-qty_scm    = ls_pur-qty_scm.
    ls_disp-qty_mbg    = ls_pur-qty_mbg.
    ls_disp-gail_id    = ls_pur-gail_id.
    ls_disp-ongc_id    = ls_pur-ongc_id.
    ls_disp-ongc_mater = ls_pur-ongc_mater.

    " Derive Outline Agreement for ALL rows (including zero qty and GJ)
    PERFORM derive_outline_agreement
      USING    ls_pur-locid ls_pur-material ls_pur-gas_day ls_pur-state_code
      CHANGING ls_disp-outline_agr ls_disp-oa_missing.

    " Populate OA detail columns from pre-fetched cache
    PERFORM derive_oa_display_fields
      USING    ls_disp-outline_agr
      CHANGING ls_disp-oa_locid ls_disp-oa_werks ls_disp-oa_matnr ls_disp-oa_desc
               ls_disp-oa_tsyst ls_disp-oa_batch.

    " Excluded from nomination: GJ state OR Exclude flag set in YRGA_CST_PUR
    IF ls_pur-state_code = gc_excl_state OR ls_pur-exclude = 'X'.
      ls_disp-is_excl   = 'X'.          " logic flag: used in all WHERE/IF
      ls_disp-exclude   = ls_pur-exclude." display: tick only if TABLE has exclude='X'
      ls_disp-sel       = ' '.
      ls_disp-row_color = 'C700'.   " grey entire row (excluded)
      " Disable SEL checkbox and CHARG edit
      CLEAR ls_styl.
      ls_styl-fieldname = 'SEL'.
      ls_styl-style     = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_styl INTO TABLE ls_disp-celltab.
      ls_styl-fieldname = 'CHARG'.
      ls_styl-style     = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_styl INTO TABLE ls_disp-celltab.
      " Match SEL cell colour to row grey so checkbox visually appears greyed
      CLEAR ls_col.
      ls_col-fname      = 'SEL'.
      ls_col-color-col  = 7.
      ls_col-color-int  = 0.
      INSERT ls_col INTO TABLE ls_disp-t_color.
    ELSE.
      ls_disp-exclude = ' '.
      ls_disp-sel     = ' '.
      " Batch editable only for batch-managed materials (xchpf = 'X')
      DATA(lv_xchpf_r) = VALUE xchpf( ).
      READ TABLE gt_mara_c INTO DATA(ls_mara_r) WITH KEY matnr = ls_pur-material.
      IF sy-subrc = 0. lv_xchpf_r = ls_mara_r-xchpf. ENDIF.
      " Qty = 0: disable SEL (cannot nominate) but don't exclude the row
      CLEAR ls_styl.
      IF ls_pur-qty_scm = 0.
        ls_styl-fieldname = 'SEL'.
        ls_styl-style     = cl_gui_alv_grid=>mc_style_disabled.
        INSERT ls_styl INTO TABLE ls_disp-celltab.
        CLEAR ls_styl.
      ENDIF.
      ls_styl-fieldname = 'CHARG'.
      IF lv_xchpf_r = 'X' AND ls_pur-qty_scm <> 0.
        ls_styl-style = cl_gui_alv_grid=>mc_style_enabled.
      ELSE.
        ls_styl-style = cl_gui_alv_grid=>mc_style_disabled.
      ENDIF.
      INSERT ls_styl INTO TABLE ls_disp-celltab.

      IF lv_xchpf_r = 'X' AND ls_disp-outline_agr IS NOT INITIAL.
        PERFORM derive_batch USING ls_pur-material ls_disp-outline_agr
                             CHANGING ls_disp-charg.
      ENDIF.

      IF ls_disp-oa_missing = abap_true.
        CLEAR ls_col.
        ls_col-fname     = 'OUTLINE_AGR'.
        ls_col-color-col = 6.
        ls_col-color-int = 1.
        INSERT ls_col INTO TABLE ls_disp-t_color.
      ENDIF.
    ENDIF.

    " Colour all OA columns (col=5, green) on all rows for visual distinction
    CLEAR ls_col.
    ls_col-color-col = 5. ls_col-color-int = 0.
    ls_col-fname = 'OA_LOCID'.  INSERT ls_col INTO TABLE ls_disp-t_color.
    ls_col-fname = 'OA_WERKS'.  INSERT ls_col INTO TABLE ls_disp-t_color.
    ls_col-fname = 'OA_MATNR'.  INSERT ls_col INTO TABLE ls_disp-t_color.
    ls_col-fname = 'OA_DESC'.   INSERT ls_col INTO TABLE ls_disp-t_color.
    ls_col-fname = 'OA_TSYST'.  INSERT ls_col INTO TABLE ls_disp-t_color.
    ls_col-fname = 'OA_BATCH'.  INSERT ls_col INTO TABLE ls_disp-t_color.

    APPEND ls_disp TO gt_display.
  ENDLOOP.

  PERFORM fetch_nomination_status.
ENDFORM.

*----------------------------------------------------------------------*
* FORM fetch_nomination_status — bulk lookup in OIJNOMI, populate
* nomtk/nomit on display rows; grey SEL for locid+day with nominations
*----------------------------------------------------------------------*
FORM fetch_nomination_status.
  DATA: ls_disp   TYPE ty_display,
        ls_nomi   TYPE oijnomi,
        lt_nomi   TYPE STANDARD TABLE OF oijnomi,
        ls_styl   TYPE lvc_s_styl,
        lv_locid  TYPE char10,
        lv_day    TYPE aedat,
        lr_docnr  TYPE RANGE OF oijnomi-docnr,
        ls_rdocnr LIKE LINE OF lr_docnr,
        lr_idate  TYPE RANGE OF oijnomi-idate,
        ls_ridate LIKE LINE OF lr_idate,
        l_tabix   TYPE sy-tabix.

  " Build ranges from non-excluded rows that have an OA
  LOOP AT gt_display INTO ls_disp WHERE is_excl <> 'X' AND outline_agr IS NOT INITIAL.
    ls_rdocnr-sign = 'I'. ls_rdocnr-option = 'EQ'.
    ls_rdocnr-low  = ls_disp-outline_agr.
    APPEND ls_rdocnr TO lr_docnr.
    ls_ridate-sign = 'I'. ls_ridate-option = 'EQ'.
    ls_ridate-low  = ls_disp-gas_day.
    APPEND ls_ridate TO lr_idate.
  ENDLOOP.
  SORT lr_docnr BY low. DELETE ADJACENT DUPLICATES FROM lr_docnr COMPARING low.
  SORT lr_idate BY low. DELETE ADJACENT DUPLICATES FROM lr_idate COMPARING low.
  IF lr_docnr IS INITIAL. RETURN. ENDIF.

  SELECT nomtk nomit docnr idate FROM oijnomi
    INTO CORRESPONDING FIELDS OF TABLE lt_nomi
    WHERE docnr  IN lr_docnr
      AND idate  IN lr_idate
      AND delind <> 'X'.
  IF sy-subrc <> 0. RETURN. ENDIF.
  SORT lt_nomi BY docnr idate.

  " Populate nomtk/nomit on matching display rows
  LOOP AT gt_display INTO ls_disp.
    l_tabix = sy-tabix.
    IF ls_disp-outline_agr IS INITIAL. CONTINUE. ENDIF.
    READ TABLE lt_nomi INTO ls_nomi
      WITH KEY docnr = ls_disp-outline_agr idate = ls_disp-gas_day
      BINARY SEARCH.
    IF sy-subrc = 0.
      ls_disp-nomtk = ls_nomi-nomtk.
      ls_disp-nomit = ls_nomi-nomit.
      MODIFY gt_display INDEX l_tabix FROM ls_disp.
    ENDIF.
  ENDLOOP.

  " Disable SEL for ALL rows of any locid+gas_day that has any nomination
  LOOP AT gt_display INTO ls_disp WHERE nomtk IS NOT INITIAL AND is_excl <> 'X'.
    lv_locid = ls_disp-locid.
    lv_day   = ls_disp-gas_day.
    " Disable SEL on ALL rows with same locid+gas_day
    LOOP AT gt_display INTO ls_disp WHERE locid = lv_locid AND gas_day = lv_day AND is_excl <> 'X'.
      l_tabix = sy-tabix.
      ls_disp-sel = ' '.
      DELETE ls_disp-celltab WHERE fieldname = 'SEL'.
      CLEAR ls_styl.
      ls_styl-fieldname = 'SEL'.
      ls_styl-style     = cl_gui_alv_grid=>mc_style_disabled.
      INSERT ls_styl INTO TABLE ls_disp-celltab.
      MODIFY gt_display INDEX l_tabix FROM ls_disp.
    ENDLOOP.
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

  " 1. T001W: plants for relevant states, WERKS range 2000-2999 (FS fix)
  REFRESH gt_t001w_c.
  SELECT werks regio FROM t001w INTO CORRESPONDING FIELDS OF TABLE gt_t001w_c
    WHERE regio IN lr_state AND werks BETWEEN '2000' AND '2999'.
  DELETE gt_t001w_c WHERE werks CA 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
  SORT gt_t001w_c BY regio werks.

  " 2. OIJ_EL_DOC_MOT: OAs overlapping date range, VBTYP=K, filtered by input locations (FS fix)
  REFRESH gt_mot_c.
  SELECT vbeln matnr locid tsyst fromdate todate vbtyp FROM oij_el_doc_mot
    INTO CORRESPONDING FIELDS OF TABLE gt_mot_c
    WHERE delind   <> 'X'
      AND vbtyp    =  'K'
      AND locid    IN s_locid
      AND fromdate <= lv_max_dt
      AND todate   >= lv_min_dt.
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
    SELECT ekko~ebeln ekko~bedat ekpo~werks ekpo~matnr ekpo~txz01
      FROM ekko INNER JOIN ekpo ON ekpo~ebeln = ekko~ebeln
      INTO CORRESPONDING FIELDS OF TABLE gt_ekoa_c
      WHERE ekko~ebeln IN lr_vbeln
        AND ekpo~loekz <> 'X'
        AND ekpo~mwskz =  'DQ'
        AND ekko~loekz =  ' '.
    SORT gt_ekoa_c BY ebeln werks.
  ENDIF.

  " 5. MCHA: active (lvorm = ' ') batches for relevant materials
  REFRESH gt_mcha_c.
  IF lr_matnr IS NOT INITIAL.
    SELECT matnr werks charg ersda lvorm FROM mcha
      INTO CORRESPONDING FIELDS OF TABLE gt_mcha_c
      WHERE matnr IN lr_matnr
        AND lvorm = ' '.
    SORT gt_mcha_c BY matnr ersda DESCENDING.
  ENDIF.

  " 6. MARA: batch management flag (xchpf) per material (FS fix: GMS_NG-Z not editable)
  REFRESH gt_mara_c.
  IF lr_matnr IS NOT INITIAL.
    SELECT matnr xchpf FROM mara
      INTO CORRESPONDING FIELDS OF TABLE gt_mara_c
      WHERE matnr IN lr_matnr.
    SORT gt_mara_c BY matnr.
  ENDIF.

  " 7. EKBE: OA batch from purchasing document history
  REFRESH gt_ekbe_c.
  IF lr_vbeln IS NOT INITIAL.
    SELECT ebeln charg FROM ekbe
      INTO CORRESPONDING FIELDS OF TABLE gt_ekbe_c
      WHERE ebeln IN lr_vbeln
        AND charg <> ' '.
    SORT gt_ekbe_c BY ebeln.
    DELETE ADJACENT DUPLICATES FROM gt_ekbe_c COMPARING ebeln.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM derive_outline_agreement — uses pre-fetched cache (no DB calls)
*----------------------------------------------------------------------*
FORM derive_outline_agreement
  USING    iv_locid   TYPE char10
           iv_matnr   TYPE char30
           iv_date    TYPE aedat
           iv_state   TYPE regio
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
* FORM derive_batch — uses EKPO-WERKS from the OA (pre-fetched EKOA cache)
*----------------------------------------------------------------------*
FORM derive_batch
  USING    iv_matnr TYPE char30
           iv_vbeln TYPE ebeln
  CHANGING cv_charg TYPE charg_d.
  DATA: ls_ek    TYPE ty_ekoa_cache,
        ls_mcha  TYPE ty_mcha_cache,
        lt_werks TYPE STANDARD TABLE OF werks_d.
  CLEAR cv_charg.
  IF iv_vbeln IS INITIAL. RETURN. ENDIF.

  " Get plants from EKPO for this OA
  LOOP AT gt_ekoa_c INTO ls_ek WHERE ebeln = iv_vbeln.
    APPEND ls_ek-werks TO lt_werks.
  ENDLOOP.
  IF lt_werks IS INITIAL. RETURN. ENDIF.

  " Find latest active batch for this material in those plants (sorted ersda DESC)
  LOOP AT gt_mcha_c INTO ls_mcha WHERE matnr = iv_matnr.
    READ TABLE lt_werks WITH KEY table_line = ls_mcha-werks TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      cv_charg = ls_mcha-charg. RETURN.
    ENDIF.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
* FORM derive_oa_display_fields — OA fields from pre-fetched cache
*----------------------------------------------------------------------*
FORM derive_oa_display_fields
  USING    iv_vbeln TYPE ebeln
  CHANGING cv_locid TYPE char10
           cv_werks TYPE werks_d
           cv_matnr TYPE char30
           cv_desc  TYPE char40
           cv_tsyst TYPE oij_tsyst
           cv_batch TYPE charg_d.
  DATA: ls_mot TYPE ty_mot_cache,
        ls_ek  TYPE ty_ekoa_cache,
        ls_eb  TYPE ty_ekbe_cache.
  CLEAR: cv_locid, cv_werks, cv_matnr, cv_desc, cv_tsyst, cv_batch.
  IF iv_vbeln IS INITIAL. RETURN. ENDIF.
  " OA Location + Transport System: first MOT entry for this OA
  LOOP AT gt_mot_c INTO ls_mot WHERE vbeln = iv_vbeln.
    cv_locid = ls_mot-locid.
    cv_tsyst = ls_mot-tsyst.
    EXIT.
  ENDLOOP.
  " OA Plant / Material / Description: first EKPO line for this OA
  READ TABLE gt_ekoa_c INTO ls_ek WITH KEY ebeln = iv_vbeln BINARY SEARCH.
  IF sy-subrc = 0.
    cv_werks = ls_ek-werks.
    cv_matnr = ls_ek-matnr.
    cv_desc  = ls_ek-txz01.
  ENDIF.
  " OA Batch: from EKBE history
  READ TABLE gt_ekbe_c INTO ls_eb WITH KEY ebeln = iv_vbeln BINARY SEARCH.
  IF sy-subrc = 0. cv_batch = ls_eb-charg. ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM derive_oa_fields_from_oa — OA change on screen: cache + DB fallback
*----------------------------------------------------------------------*
FORM derive_oa_fields_from_oa
  USING    iv_vbeln TYPE ebeln
  CHANGING cv_locid TYPE char10
           cv_werks TYPE werks_d
           cv_matnr TYPE char30
           cv_desc  TYPE char40
           cv_tsyst TYPE oij_tsyst
           cv_batch TYPE charg_d.
  CLEAR: cv_locid, cv_werks, cv_matnr, cv_desc, cv_tsyst, cv_batch.
  IF iv_vbeln IS INITIAL. RETURN. ENDIF.
  " Try pre-fetched cache first
  PERFORM derive_oa_display_fields
    USING    iv_vbeln
    CHANGING cv_locid cv_werks cv_matnr cv_desc cv_tsyst cv_batch.
  " Fallback: DB selects for a manually entered OA not in cache
  IF cv_locid IS INITIAL.
    SELECT SINGLE locid tsyst FROM oij_el_doc_mot
      INTO (@cv_locid, @cv_tsyst)
      WHERE vbeln = @iv_vbeln AND delind <> 'X'.
  ENDIF.
  IF cv_werks IS INITIAL.
    SELECT SINGLE ekpo~werks ekpo~matnr ekpo~txz01
      FROM ekko INNER JOIN ekpo ON ekpo~ebeln = ekko~ebeln
      INTO (@cv_werks, @cv_matnr, @cv_desc)
      WHERE ekko~ebeln = @iv_vbeln
        AND ekko~loekz = ' '
        AND ekpo~loekz <> 'X'
        AND ekpo~mwskz = 'DQ'.
  ENDIF.
  IF cv_batch IS INITIAL.
    SELECT SINGLE charg FROM ekbe INTO cv_batch
      WHERE ebeln = iv_vbeln AND charg <> ' '.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM get_valid_batches_for_material  - for F4 dropdown
*----------------------------------------------------------------------*
FORM get_valid_batches_for_material
  USING    iv_matnr  TYPE char30
           iv_state  TYPE regio
  CHANGING ct_batch  TYPE STANDARD TABLE.
  DATA: lt_mcha  TYPE STANDARD TABLE OF mcha,
        ls_mcha  TYPE mcha,
        ls_val   TYPE ty_batch_vals,
        lt_werks TYPE STANDARD TABLE OF werks_d,
        lv_werks TYPE werks_d.
  REFRESH ct_batch.
  SELECT werks FROM t001w INTO TABLE lt_werks
    WHERE regio = iv_state AND werks BETWEEN '2000' AND '2999'.
  IF lt_werks IS NOT INITIAL.
    SORT lt_werks. DELETE ADJACENT DUPLICATES FROM lt_werks.
    LOOP AT lt_werks INTO lv_werks.
      SELECT matnr werks charg ersda FROM mcha
        APPENDING CORRESPONDING FIELDS OF TABLE lt_mcha
        WHERE matnr = iv_matnr AND werks = lv_werks.
    ENDLOOP.
  ELSE.
    SELECT matnr werks charg ersda FROM mcha
      INTO CORRESPONDING FIELDS OF TABLE lt_mcha
      WHERE matnr = iv_matnr.
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
  DATA: lv_title    TYPE lvc_title,
        ls_variant  TYPE disvariant,
        ls_sdate    LIKE LINE OF s_date,
        lv_date_lo  TYPE char10,
        lv_date_hi  TYPE char10,
        lv_dates    TYPE char40,
        lt_sort     TYPE lvc_t_sort,
        ls_sort    TYPE lvc_s_sort.

  PERFORM build_fieldcat.
  PERFORM set_alv_layout.

  " Sort data by Location / Gas Day / Material as per FS
  SORT gt_display BY locid gas_day material.

  " Also pass sort to ALV so it shows sort indicators in column headers
  CLEAR ls_sort.
  ls_sort-fieldname = 'LOCID'.    ls_sort-up = abap_true. ls_sort-spos = 1.
  APPEND ls_sort TO lt_sort.
  CLEAR ls_sort.
  ls_sort-fieldname = 'GAS_DAY'.  ls_sort-up = abap_true. ls_sort-spos = 2.
  APPEND ls_sort TO lt_sort.
  CLEAR ls_sort.
  ls_sort-fieldname = 'MATERIAL'. ls_sort-up = abap_true. ls_sort-spos = 3.
  APPEND ls_sort TO lt_sort.

  " Build grid title: no location; dates in DD.MM.YYYY format
  CLEAR gv_toolbar_done.
  READ TABLE s_date INDEX 1 INTO ls_sdate.
  IF sy-subrc = 0.
    CONCATENATE ls_sdate-low+6(2) '.' ls_sdate-low+4(2) '.' ls_sdate-low(4)
                INTO lv_date_lo.
    CONCATENATE ls_sdate-high+6(2) '.' ls_sdate-high+4(2) '.' ls_sdate-high(4)
                INTO lv_date_hi.
    CONCATENATE lv_date_lo ' -' lv_date_hi INTO lv_dates.
  ENDIF.
  CONCATENATE 'Purchase Nomination - ONGC B2B'
              '| Period:' lv_dates
              INTO lv_title SEPARATED BY ' '.
  CONDENSE lv_title.

  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_top_of_page   = 'TOP_OF_PAGE'
      i_grid_title             = lv_title
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fcat
      it_sort_lvc              = lt_sort
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
    SET HANDLER go_alv_handler->on_main_data_changed          FOR go_alv.
    SET HANDLER go_alv_handler->on_main_data_changed_finished FOR go_alv.
    SET HANDLER go_alv_handler->on_main_f4                    FOR go_alv.
    SET HANDLER go_alv_handler->on_alv_toolbar                FOR go_alv.
    go_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
    DATA: lt_f4 TYPE lvc_t_f4, ls_f4 TYPE lvc_s_f4.
    CLEAR ls_f4.
    ls_f4-fieldname  = 'CHARG'.
    ls_f4-register   = abap_true.
    ls_f4-chngeafter = abap_false.
    INSERT ls_f4 INTO TABLE lt_f4.
    go_alv->register_f4_for_fields( it_f4 = lt_f4 ).
    go_alv->set_toolbar_interactive( ).
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM top_of_page — header printed above the ALV grid
*----------------------------------------------------------------------*
FORM top_of_page.
  DATA: lt_header TYPE slis_t_listheader,
        ls_line   TYPE slis_listheader.

  CLEAR ls_line.
  ls_line-typ  = 'A'.
  ls_line-info = 'Note:'.
  APPEND ls_line TO lt_header.

  CLEAR ls_line.
  ls_line-typ  = 'A'.
  ls_line-info = '1. Nomination will not be created for line items with State GJ'.
  APPEND ls_line TO lt_header.

  CLEAR ls_line.
  ls_line-typ  = 'A'.
  ls_line-info = '2. Nomination will not be created for Materials excluded for'.
  APPEND ls_line TO lt_header.

  CLEAR ls_line.
  ls_line-typ  = 'A'.
  ls_line-info = '   Allocation'.
  APPEND ls_line TO lt_header.

  CLEAR ls_line.
  ls_line-typ  = 'A'.
  ls_line-info = '3. Nominations will be created in SM3'.
  APPEND ls_line TO lt_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_header.
ENDFORM.

*----------------------------------------------------------------------*
* FORM build_fieldcat
*----------------------------------------------------------------------*
FORM build_fieldcat.
  DATA: ls_fcat TYPE lvc_s_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'SEL'.
  ls_fcat-coltext   = 'Sel'.
  ls_fcat-seltext   = 'Select'.
  ls_fcat-checkbox  = abap_true.
  ls_fcat-hotspot   = abap_true.
  ls_fcat-outputlen = 4.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'EXCLUDE'.
  ls_fcat-coltext   = 'Exclude'.
  ls_fcat-seltext   = 'Excluded'.
  ls_fcat-outputlen = 8.
  ls_fcat-checkbox  = abap_true.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'GAS_DAY'.
  ls_fcat-coltext   = 'Gas Day'.
  ls_fcat-seltext   = 'Gas Day'.
  ls_fcat-outputlen = 12.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'LOCID'.
  ls_fcat-coltext   = 'Location'.
  ls_fcat-seltext   = 'Location'.
  ls_fcat-outputlen = 12.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'MATERIAL'.
  ls_fcat-coltext   = 'Material'.
  ls_fcat-seltext   = 'Material'.
  ls_fcat-outputlen = 20.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'STATE_CODE'.
  ls_fcat-coltext   = 'State'.
  ls_fcat-seltext   = 'State Code'.
  ls_fcat-outputlen = 6.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'QTY_SCM'.
  ls_fcat-coltext   = 'Qty (SCM)'.
  ls_fcat-seltext   = 'Quantity SCM'.
  ls_fcat-outputlen = 16.
  ls_fcat-datatype  = 'QUAN'.
  ls_fcat-no_sign   = abap_true.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'QTY_MBG'.
  ls_fcat-coltext   = 'Qty (MBG)'.
  ls_fcat-seltext   = 'Quantity MBG'.
  ls_fcat-outputlen = 16.
  ls_fcat-datatype  = 'QUAN'.
  ls_fcat-no_sign   = abap_true.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'GAIL_ID'.
  ls_fcat-coltext   = 'GAIL ID'.
  ls_fcat-seltext   = 'GAIL ID'.
  ls_fcat-outputlen = 22.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ONGC_ID'.
  ls_fcat-coltext   = 'ONGC ID'.
  ls_fcat-seltext   = 'ONGC ID'.
  ls_fcat-outputlen = 22.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ONGC_MATER'.
  ls_fcat-coltext   = 'ONGC Material'.
  ls_fcat-seltext   = 'ONGC Material'.
  ls_fcat-outputlen = 22.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'OUTLINE_AGR'.
  ls_fcat-coltext   = 'Outline Agreement'.
  ls_fcat-seltext   = 'Outline Agreement'.
  ls_fcat-outputlen = 14.
  ls_fcat-edit      = abap_true.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname  = 'CHARG'.
  ls_fcat-coltext    = 'Batch'.
  ls_fcat-seltext    = 'Batch Number'.
  ls_fcat-outputlen  = 12.
  ls_fcat-edit       = abap_true.
  ls_fcat-f4availabl = abap_true.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'OA_LOCID'.
  ls_fcat-coltext   = 'OA Location'.
  ls_fcat-seltext   = 'OA Location'.
  ls_fcat-outputlen = 12.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'OA_WERKS'.
  ls_fcat-coltext   = 'OA Plant'.
  ls_fcat-seltext   = 'OA Plant'.
  ls_fcat-outputlen = 8.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'OA_MATNR'.
  ls_fcat-coltext   = 'OA Material'.
  ls_fcat-seltext   = 'OA Material'.
  ls_fcat-outputlen = 18.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'OA_DESC'.
  ls_fcat-coltext   = 'OA Description'.
  ls_fcat-seltext   = 'OA Description'.
  ls_fcat-outputlen = 25.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'OA_BATCH'.
  ls_fcat-coltext   = 'OA Batch'.
  ls_fcat-seltext   = 'OA Batch'.
  ls_fcat-outputlen = 12.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'OA_TSYST'.
  ls_fcat-coltext   = 'OA Trans.Sys'.
  ls_fcat-seltext   = 'OA Transport System'.
  ls_fcat-outputlen = 14.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'NOMTK'.
  ls_fcat-coltext   = 'Nomination Key'.
  ls_fcat-seltext   = 'Nomination Key'.
  ls_fcat-outputlen = 12.
  ls_fcat-no_zero   = abap_true.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'NOMIT'.
  ls_fcat-coltext   = 'Nom. Item'.
  ls_fcat-seltext   = 'Nomination Item'.
  ls_fcat-outputlen = 6.
  ls_fcat-no_zero   = abap_true.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'IS_EXCL'. ls_fcat-tech = abap_true. APPEND ls_fcat TO gt_fcat.
  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ROW_COLOR'. ls_fcat-tech = abap_true. APPEND ls_fcat TO gt_fcat.
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
  gs_layout-ctab_fname  = 'T_COLOR'.
  gs_layout-stylefname  = 'CELLTAB'.
  gs_layout-info_fname  = 'ROW_COLOR'.
  gs_layout-no_rowmark  = abap_true.
ENDFORM.

*----------------------------------------------------------------------*
* FORM alv_toolbar
*----------------------------------------------------------------------*
FORM alv_toolbar USING e_object      TYPE REF TO cl_alv_event_toolbar_set
                        e_interactive TYPE char1.
  DATA: ls_tb TYPE stb_button.
  IF gv_toolbar_done = abap_true. RETURN. ENDIF.
  CLEAR ls_tb.
  ls_tb-function  = 'SELALL'.
  ls_tb-icon      = '@2V@'.
  ls_tb-quickinfo = 'Select All'.
  ls_tb-text      = 'Select All'.
  INSERT ls_tb INTO e_object->mt_toolbar INDEX 1.
  CLEAR ls_tb.
  ls_tb-function  = 'DSLALL'.
  ls_tb-icon      = '@2W@'.
  ls_tb-quickinfo = 'Deselect All'.
  ls_tb-text      = 'Deselect All'.
  INSERT ls_tb INTO e_object->mt_toolbar INDEX 2.
  CLEAR ls_tb.
  ls_tb-butn_type = 3.
  INSERT ls_tb INTO e_object->mt_toolbar INDEX 3.
  CLEAR ls_tb.
  ls_tb-function  = 'BCMASS'.
  ls_tb-icon      = '@EJ@'.
  ls_tb-quickinfo = 'Batch Change in Mass'.
  ls_tb-text      = 'Batch Change'.
  INSERT ls_tb INTO e_object->mt_toolbar INDEX 4.
  CLEAR ls_tb.
  ls_tb-butn_type = 3.
  INSERT ls_tb INTO e_object->mt_toolbar INDEX 5.
  CLEAR ls_tb.
  ls_tb-function  = 'CRENOM'.
  ls_tb-icon      = '@15@'.
  ls_tb-quickinfo = 'Create Nomination'.
  ls_tb-text      = 'Create Nomination'.
  INSERT ls_tb INTO e_object->mt_toolbar INDEX 6.
  gv_toolbar_done = abap_true.
ENDFORM.

*----------------------------------------------------------------------*
* FORM user_command
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
    WHEN 'SELALL'.
      LOOP AT gt_display INTO DATA(ls_sa) WHERE is_excl <> 'X'.
        ls_sa-sel = abap_true.
        MODIFY gt_display FROM ls_sa.
      ENDLOOP.
    WHEN 'DSLALL'.
      LOOP AT gt_display INTO DATA(ls_da).
        CLEAR ls_da-sel.
        MODIFY gt_display FROM ls_da.
      ENDLOOP.
    WHEN '&IC1'.
      IF rs_selfield-fieldname = 'SEL'.
        PERFORM toggle_sel_for_row USING rs_selfield-tabindex.
      ENDIF.
  ENDCASE.
  rs_selfield-refresh = abap_true.
ENDFORM.

*----------------------------------------------------------------------*
* FORM toggle_sel_for_row
*----------------------------------------------------------------------*
FORM toggle_sel_for_row USING iv_index TYPE i.
  DATA: ls_disp   TYPE ty_display,
        lv_locid  TYPE char10,
        lv_date   TYPE aedat,
        lv_newsel TYPE char1.
  READ TABLE gt_display INDEX iv_index INTO ls_disp.
  IF sy-subrc <> 0. RETURN. ENDIF.
  IF ls_disp-is_excl = 'X'. RETURN. ENDIF.
  IF ls_disp-sel = abap_true.
    lv_newsel = ' '.
  ELSE.
    lv_newsel = abap_true.
  ENDIF.
  lv_locid = ls_disp-locid.
  lv_date  = ls_disp-gas_day.
  LOOP AT gt_display INTO ls_disp.
    IF ls_disp-locid = lv_locid AND ls_disp-gas_day = lv_date
       AND ls_disp-is_excl <> 'X'.
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
        i_main      TYPE tt_main,
        ls_main     TYPE ty_main,
        lt_errors   TYPE tt_log,
        i_rspartab  TYPE STANDARD TABLE OF rsparams,
        wa_rspartab LIKE LINE OF i_rspartab,
        ls_sdate    LIKE LINE OF s_date,
        ls_slocid   LIKE LINE OF s_locid,
        lv_dtext    TYPE char10.

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
    IF ls_disp-nomtk IS NOT INITIAL.
      CONCATENATE ls_disp-gas_day+6(2) '.' ls_disp-gas_day+4(2) '.' ls_disp-gas_day(4)
                  INTO lv_dtext.
      MESSAGE |Nomination already exists for { ls_disp-locid } on { lv_dtext }.| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    READ TABLE gt_mara_c INTO DATA(ls_mara_chk) WITH KEY matnr = ls_disp-material.
    IF sy-subrc = 0 AND ls_mara_chk-xchpf = 'X'.
      IF ls_disp-charg IS INITIAL.
        MESSAGE |Batch missing for { ls_disp-material }. Assign before creating nomination.| TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDIF.
  ENDLOOP.

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
    APPEND ls_main TO i_main.
  ENDLOOP.

  EXPORT i_main[] TO MEMORY ID gc_memory_id.
  DATA: lv_yrgg015 TYPE char1 VALUE 'X'.
  EXPORT lv_yrgg015 = lv_yrgg015 TO MEMORY ID gc_call_flag.

  CLEAR wa_rspartab.
  wa_rspartab-selname = 'R_EXCEL'.
  wa_rspartab-kind    = 'P'.
  wa_rspartab-low     = abap_true.
  APPEND wa_rspartab TO i_rspartab.

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

  IMPORT lt_errors FROM MEMORY ID gc_err_mem_id.
  FREE MEMORY ID gc_memory_id.
  FREE MEMORY ID gc_err_mem_id.

  IF lt_errors IS NOT INITIAL.
    PERFORM display_nomination_errors USING lt_errors.
  ENDIF.
  PERFORM fetch_nomination_status.
  IF go_alv IS NOT INITIAL.
    go_alv->refresh_table_display( ).
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM display_nomination_errors
*----------------------------------------------------------------------*
FORM display_nomination_errors USING it_errors TYPE tt_log.
  DATA: lt_fcat   TYPE lvc_t_fcat,
        ls_fcat   TYPE lvc_s_fcat,
        ls_layout TYPE lvc_s_layo,
        ls_e      TYPE ty_log.

  ls_fcat-fieldname = 'LOCID'.   ls_fcat-coltext = 'Location'. APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  ls_fcat-fieldname = 'MATNR'.   ls_fcat-coltext = 'Material'. APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  ls_fcat-fieldname = 'DATE'.    ls_fcat-coltext = 'Date'.     APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  ls_fcat-fieldname = 'EBELN'.   ls_fcat-coltext = 'PO/OA'.   APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  ls_fcat-fieldname = 'CHARG'.   ls_fcat-coltext = 'Batch'.   APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  ls_fcat-fieldname = 'MESSAGE'. ls_fcat-coltext = 'Message'.
  ls_fcat-outputlen = 100. APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

  ls_layout-cwidth_opt = abap_true.
  DATA: lo_popup TYPE REF TO cl_gui_dialogbox_container,
        lo_aerr  TYPE REF TO cl_gui_alv_grid.
  CREATE OBJECT lo_popup
    EXPORTING caption = 'Nomination Errors' top = 10 left = 10 width = 600 height = 350
    EXCEPTIONS OTHERS = 1.
  IF sy-subrc <> 0.
    LOOP AT it_errors INTO ls_e.
      MESSAGE ls_e-message TYPE 'S' DISPLAY LIKE 'E'.
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

  LOOP AT gt_display INTO ls_disp WHERE is_excl <> 'X'.
    ADD 1 TO lv_rows_sel.
    READ TABLE gt_batch_assign WITH KEY matnr = ls_disp-material TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      READ TABLE gt_mara_c INTO DATA(ls_mara_b) WITH KEY matnr = ls_disp-material.
      lv_xchpf = COND #( WHEN sy-subrc = 0 THEN ls_mara_b-xchpf ELSE space ).
      IF lv_xchpf = 'X'.
        CLEAR ls_assign.
        ls_assign-matnr       = ls_disp-material.
        ls_assign-state_code  = ls_disp-state_code.
        ls_assign-outline_agr = ls_disp-outline_agr.
        PERFORM derive_batch USING ls_disp-material ls_disp-outline_agr
                             CHANGING ls_assign-charg.
        APPEND ls_assign TO gt_batch_assign.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF gt_batch_assign IS INITIAL.
    MESSAGE 'No batch-managed materials found in the list.' TYPE 'S'
            DISPLAY LIKE 'W'. RETURN.
  ENDIF.

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
  SET HANDLER go_alv_handler->on_batch_toolbar       FOR go_batch_alv.
  SET HANDLER go_alv_handler->on_batch_cmd           FOR go_batch_alv.
  SET HANDLER go_alv_handler->on_batch_f4            FOR go_batch_alv.

  ls_fcat-fieldname = 'MATNR'. ls_fcat-coltext = 'Material'. ls_fcat-outputlen = 18.
  ls_fcat-no_out    = abap_false.
  APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  ls_fcat-fieldname  = 'CHARG'. ls_fcat-coltext = 'Batch'. ls_fcat-outputlen = 10.
  ls_fcat-edit       = abap_true.
  ls_fcat-f4availabl = abap_true.
  APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

  ls_layout-cwidth_opt = abap_true.

  go_batch_alv->set_table_for_first_display(
    EXPORTING is_layout       = ls_layout
    CHANGING  it_outtab       = gt_batch_assign
              it_fieldcatalog = lt_fcat
    EXCEPTIONS OTHERS = 1 ).

  go_batch_alv->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
  DATA: lt_f4b TYPE lvc_t_f4, ls_f4b TYPE lvc_s_f4.
  CLEAR ls_f4b.
  ls_f4b-fieldname  = 'CHARG'.
  ls_f4b-register   = abap_true.
  ls_f4b-chngeafter = abap_false.
  INSERT ls_f4b INTO TABLE lt_f4b.
  go_batch_alv->register_f4_for_fields( it_f4 = lt_f4b ).
  go_batch_alv->set_toolbar_interactive( ).
ENDFORM.

*----------------------------------------------------------------------*
* FORM schedule_background_job
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
*----------------------------------------------------------------------*
FORM create_all_nominations_bg.
  DATA: ls_disp     TYPE ty_display,
        i_main      TYPE tt_main,
        ls_main     TYPE ty_main,
        i_rspartab  TYPE STANDARD TABLE OF rsparams,
        wa_rspartab LIKE LINE OF i_rspartab,
        ls_sdate    LIKE LINE OF s_date,
        ls_slocid   LIKE LINE OF s_locid.

  LOOP AT gt_display INTO ls_disp WHERE is_excl <> 'X'.
    IF ls_disp-outline_agr IS INITIAL. CONTINUE. ENDIF.
    READ TABLE gt_mara_c INTO DATA(ls_mara_bg) WITH KEY matnr = ls_disp-material.
    IF sy-subrc = 0 AND ls_mara_bg-xchpf = 'X' AND ls_disp-charg IS INITIAL. CONTINUE. ENDIF.
    CLEAR ls_main.
    ls_main-vbeln = ls_disp-outline_agr.
    ls_main-date  = ls_disp-gas_day.
    ls_main-locid = ls_disp-locid.
    ls_main-matnr = ls_disp-material.
    ls_main-menge = ls_disp-qty_scm.
    ls_main-unit  = gc_sm3.
    ls_main-charg = ls_disp-charg.
    ls_main-rank  = 1.
    APPEND ls_main TO i_main.
  ENDLOOP.

  IF i_main IS INITIAL. RETURN. ENDIF.

  EXPORT i_main[] TO MEMORY ID gc_memory_id.
  DATA: lv_yrgg015 TYPE char1 VALUE 'X'.
  EXPORT lv_yrgg015 = lv_yrgg015 TO MEMORY ID gc_call_flag.

  CLEAR wa_rspartab.
  wa_rspartab-selname = 'R_EXCEL'. wa_rspartab-kind = 'P'. wa_rspartab-low = abap_true.
  APPEND wa_rspartab TO i_rspartab.

  " Pass background flag so YRXR036 BATCH_VALIDATE checks p_bgrun instead of sy-batch
  CLEAR wa_rspartab.
  wa_rspartab-selname = 'P_BGRUN'. wa_rspartab-kind = 'P'. wa_rspartab-low = abap_true.
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
