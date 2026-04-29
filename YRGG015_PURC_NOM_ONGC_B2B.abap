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
         location_id TYPE char10,
         material    TYPE matnr,
         state_code  TYPE char2,
         qty_scm     TYPE p LENGTH 13 DECIMALS 3,
         gail_id     TYPE char20,
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
         gas_day     TYPE aedat,
         location_id TYPE char10,
         material    TYPE matnr,
         state_code  TYPE char2,
         qty_scm     TYPE p LENGTH 13 DECIMALS 3,
         gail_id     TYPE char20,
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

*----------------------------------------------------------------------*
* DATA DECLARATIONS
*----------------------------------------------------------------------*
DATA: gt_display   TYPE tt_display,
      gt_main      TYPE tt_main,
      go_alv       TYPE REF TO cl_gui_alv_grid,
      go_container TYPE REF TO cl_gui_custom_container,
      gs_layout    TYPE lvc_s_layo,
      gt_fcat      TYPE lvc_t_fcat,
      gv_okcode    TYPE syucomm,
      gv_auth_bg   TYPE char1.

CONSTANTS: gc_memory_id  TYPE char30 VALUE 'YRGG015_NOM_DATA',
           gc_err_mem_id TYPE char30 VALUE 'YRGG015_NOM_ERRORS',
           gc_role_core  TYPE string VALUE 'ZC_GMS_CORE_TEAM',
           gc_excl_state TYPE char2  VALUE 'GJ',
           gc_deleted    TYPE char1  VALUE 'X',
           gc_sm3        TYPE meins  VALUE 'SM3'.

*----------------------------------------------------------------------*
* EVENT HANDLER CLASS DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      on_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      on_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,
      on_hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,
      on_onf4
        FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname es_row_no er_event_data.
ENDCLASS.

DATA: go_handler TYPE REF TO lcl_event_handler.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_locid FOR oijnomi-locid NO INTERVALS,
                  s_date  FOR oijnomi-nom_date.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_bgrun AS CHECKBOX MODIF ID bg.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM set_default_fn_dates.
  TEXT-001 = 'Selection Criteria'.
  TEXT-002 = 'Background Processing'.

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

  IF p_bgrun = abap_true.
    PERFORM schedule_background_job.
  ELSE.
    PERFORM display_alv_grid.
  ENDIF.

*----------------------------------------------------------------------*
* EVENT HANDLER CLASS - IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_toolbar.
    DATA: ls_tb TYPE stb_button.
    CLEAR ls_tb.
    ls_tb-function  = 'BCMASS'.
    ls_tb-icon      = icon_batch_input.
    ls_tb-quickinfo = 'Batch Change in Mass'.
    ls_tb-text      = 'Batch Change'.
    APPEND ls_tb TO e_object->mt_toolbar.
    CLEAR ls_tb.
    ls_tb-butn_type = 3.
    APPEND ls_tb TO e_object->mt_toolbar.
    CLEAR ls_tb.
    ls_tb-function  = 'CRENOM'.
    ls_tb-icon      = icon_execute_object.
    ls_tb-quickinfo = 'Create Nomination'.
    ls_tb-text      = 'Create Nomination'.
    APPEND ls_tb TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD on_user_command.
    CASE e_ucomm.
      WHEN 'BCMASS'. PERFORM handle_batch_mass_change.
      WHEN 'CRENOM'. PERFORM handle_create_nomination.
    ENDCASE.
  ENDMETHOD.

  METHOD on_data_changed.
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

  METHOD on_hotspot_click.
    DATA: ls_disp  TYPE ty_display,
          lv_locid TYPE char10,
          lv_date  TYPE aedat,
          lv_newsel TYPE char1.
    IF e_column_id-fieldname = 'SEL'.
      READ TABLE gt_display INDEX e_row_id-index INTO ls_disp.
      IF sy-subrc = 0.
        lv_newsel = COND #( WHEN ls_disp-sel = abap_true THEN ' ' ELSE abap_true ).
        lv_locid  = ls_disp-location_id.
        lv_date   = ls_disp-gas_day.
        LOOP AT gt_display INTO ls_disp.
          IF ls_disp-location_id = lv_locid AND ls_disp-gas_day = lv_date.
            ls_disp-sel = lv_newsel.
            MODIFY gt_display FROM ls_disp.
          ENDIF.
        ENDLOOP.
        go_alv->refresh_table_display( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD on_onf4.
    IF e_fieldname = 'CHARG'.
      DATA: ls_disp    TYPE ty_display,
            lt_batches TYPE STANDARD TABLE OF ty_batch_vals,
            lt_f4vals  TYPE STANDARD TABLE OF ddshretval,
            ls_f4val   TYPE ddshretval,
            ls_bat     TYPE ty_batch_vals.
      READ TABLE gt_display INDEX es_row_no-row_id INTO ls_disp.
      IF sy-subrc <> 0. RETURN. ENDIF.
      PERFORM get_valid_batches_for_material USING ls_disp-material CHANGING lt_batches.
      IF lt_batches IS INITIAL.
        MESSAGE 'No valid batches found for this material.' TYPE 'S' DISPLAY LIKE 'W'.
        RETURN.
      ENDIF.
      LOOP AT lt_batches INTO ls_bat.
        ls_f4val-fieldname = 'CHARG'.
        ls_f4val-fieldval  = ls_bat-charg.
        APPEND ls_f4val TO lt_f4vals.
      ENDLOOP.
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'CHARG'
          dynpprog        = sy-repid
          dynpnr          = sy-dynnr
          stepl           = es_row_no-row_id
          value_org       = 'S'
        TABLES
          value_tab       = lt_f4vals
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.
      IF sy-subrc = 0.
        er_event_data->m_event_handled = abap_true.
      ENDIF.
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
        lv_day_hi TYPE i.

  LOOP AT s_locid INTO ls_locid WHERE sign = 'I' AND option = 'EQ'.
    SELECT SINGLE location_id FROM yrga_cst_loc_map INTO @DATA(lv_loc)
      WHERE location_id = @ls_locid-low.
    IF sy-subrc <> 0.
      MESSAGE e000(oo) WITH
        'Business Location doesn''t pertain to ONGC CST Purchase:'
        ls_locid-low ' ' ' '.
    ENDIF.
  ENDLOOP.

  LOOP AT s_date INTO ls_date WHERE sign = 'I' AND option = 'BT'.
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

  SELECT gas_day, location_id, material, state_code, qty_scm, gail_id, deleted
    FROM yrga_cst_pur
    INTO TABLE @lt_pur
    WHERE gas_day     IN @s_date
      AND location_id IN @s_locid
      AND deleted    <> @gc_deleted
      AND state_code <> @gc_excl_state.

  IF sy-subrc <> 0 OR lt_pur IS INITIAL. RETURN. ENDIF.
  DELETE lt_pur WHERE qty_scm = 0.

  LOOP AT lt_pur INTO ls_pur.
    CLEAR ls_disp.
    ls_disp-sel         = ' '.
    ls_disp-gas_day     = ls_pur-gas_day.
    ls_disp-location_id = ls_pur-location_id.
    ls_disp-material    = ls_pur-material.
    ls_disp-state_code  = ls_pur-state_code.
    ls_disp-qty_scm     = ls_pur-qty_scm.
    ls_disp-gail_id     = ls_pur-gail_id.

    PERFORM derive_outline_agreement
      USING    ls_pur-location_id ls_pur-material
      CHANGING ls_disp-outline_agr ls_disp-oa_missing.

    PERFORM derive_batch USING ls_pur-material CHANGING ls_disp-charg.

    CLEAR ls_disp-celltab.
    ls_styl-fieldname = 'CHARG'.
    ls_styl-style     = cl_gui_alv_grid=>mc_style_enabled.
    APPEND ls_styl TO ls_disp-celltab.

    IF ls_disp-oa_missing = abap_true.
      CLEAR ls_col.
      ls_col-fname     = 'OUTLINE_AGR'.
      ls_col-color-col = 6.
      ls_col-color-int = 1.
      APPEND ls_col TO ls_disp-t_color.
    ENDIF.

    APPEND ls_disp TO gt_display.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
* FORM derive_outline_agreement
* OIJ_EL_DOC_MOT -> T001W (WERKS=2*) -> EKPO (MWSKZ=DQ, loekz=' ') -> latest
*----------------------------------------------------------------------*
FORM derive_outline_agreement
  USING    iv_locid   TYPE char10
           iv_matnr   TYPE matnr
  CHANGING cv_vbeln   TYPE ebeln
           cv_missing TYPE char1.

  DATA: lt_mot        TYPE STANDARD TABLE OF oij_el_doc_mot,
        ls_mot        TYPE oij_el_doc_mot,
        lt_werks      TYPE STANDARD TABLE OF werks_d,
        lv_werks      TYPE werks_d,
        lv_best_vbeln TYPE ebeln,
        lv_best_bedat TYPE bedat.

  CLEAR: cv_vbeln, cv_missing.

  SELECT * FROM oij_el_doc_mot INTO TABLE @lt_mot WHERE locid = @iv_locid.
  IF lt_mot IS INITIAL. cv_missing = abap_true. RETURN. ENDIF.

  LOOP AT lt_mot INTO ls_mot.
    SELECT werks FROM t001w INTO TABLE @DATA(lt_t001w)
      WHERE werks LIKE '2%' AND kunnr = @ls_mot-bpnam.
    IF sy-subrc = 0.
      LOOP AT lt_t001w INTO DATA(ls_t001w).
        APPEND ls_t001w-werks TO lt_werks.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  IF lt_werks IS INITIAL.
    SELECT werks FROM t001w INTO TABLE @lt_werks
      WHERE werks LIKE '2%' AND regio = @iv_locid(2).
    IF sy-subrc <> 0. cv_missing = abap_true. RETURN. ENDIF.
  ENDIF.

  SORT lt_werks. DELETE ADJACENT DUPLICATES FROM lt_werks.

  LOOP AT lt_werks INTO lv_werks.
    SELECT ekko~ebeln, ekko~bedat
      FROM ekko INNER JOIN ekpo ON ekpo~ebeln = ekko~ebeln
      INTO TABLE @DATA(lt_res)
      WHERE ekko~bstyp = 'K'
        AND ekpo~werks  = @lv_werks
        AND ekpo~matnr  = @iv_matnr
        AND ekpo~mwskz  = 'DQ'
        AND ekpo~loekz  = ' '
        AND ekko~loekz  = ' '.
    LOOP AT lt_res INTO DATA(ls_res).
      IF ls_res-bedat > lv_best_bedat.
        lv_best_bedat = ls_res-bedat.
        lv_best_vbeln = ls_res-ebeln.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF lv_best_vbeln IS NOT INITIAL.
    cv_vbeln = lv_best_vbeln.
  ELSE.
    cv_missing = abap_true.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM derive_batch  - latest batch from MCHA (LVORM = blank)
*----------------------------------------------------------------------*
FORM derive_batch
  USING    iv_matnr TYPE matnr
  CHANGING cv_charg TYPE charg_d.
  DATA: lt_mcha TYPE STANDARD TABLE OF mcha,
        ls_mcha TYPE mcha.
  CLEAR cv_charg.
  SELECT matnr, werks, charg, ersda, lvorm FROM mcha
    INTO TABLE @lt_mcha WHERE matnr = @iv_matnr AND lvorm = ' '.
  IF sy-subrc <> 0 OR lt_mcha IS INITIAL. RETURN. ENDIF.
  SORT lt_mcha BY ersda DESCENDING.
  READ TABLE lt_mcha INDEX 1 INTO ls_mcha.
  IF sy-subrc = 0. cv_charg = ls_mcha-charg. ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM get_valid_batches_for_material  - for F4 dropdown
*----------------------------------------------------------------------*
FORM get_valid_batches_for_material
  USING    iv_matnr  TYPE matnr
  CHANGING ct_batch  TYPE STANDARD TABLE.
  DATA: lt_mcha TYPE STANDARD TABLE OF mcha,
        ls_mcha TYPE mcha,
        ls_val  TYPE ty_batch_vals.
  REFRESH ct_batch.
  SELECT matnr, werks, charg, ersda, lvorm FROM mcha
    INTO TABLE @lt_mcha WHERE matnr = @iv_matnr AND lvorm = ' '.
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
* FORM display_alv_grid
*----------------------------------------------------------------------*
FORM display_alv_grid.
  IF go_container IS INITIAL.
    CREATE OBJECT go_container
      EXPORTING container_name = 'ALV_CONTAINER'
      EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0. MESSAGE 'Error creating ALV container.' TYPE 'E'. ENDIF.
  ENDIF.
  IF go_alv IS INITIAL.
    CREATE OBJECT go_alv
      EXPORTING i_parent = go_container
      EXCEPTIONS OTHERS = 1.
    IF sy-subrc <> 0. MESSAGE 'Error creating ALV grid.' TYPE 'E'. ENDIF.
  ENDIF.
  CREATE OBJECT go_handler.
  SET HANDLER go_handler->on_toolbar       FOR go_alv.
  SET HANDLER go_handler->on_user_command  FOR go_alv.
  SET HANDLER go_handler->on_data_changed  FOR go_alv.
  SET HANDLER go_handler->on_hotspot_click FOR go_alv.
  SET HANDLER go_handler->on_onf4          FOR go_alv.
  go_alv->register_f4_for_fields(
    EXPORTING it_f4 = VALUE lvc_t_f4(
      ( fieldname = 'CHARG' register_f4 = cl_gui_alv_grid=>m_mb_f4 ) ) ).
  PERFORM build_fieldcat.
  PERFORM set_alv_layout.
  go_alv->set_table_for_first_display(
    EXPORTING
      is_layout            = gs_layout
      it_toolbar_excluding = VALUE ui_functions( )
    CHANGING
      it_outtab            = gt_display
      it_fieldcatalog      = gt_fcat
    EXCEPTIONS OTHERS = 1 ).
  IF sy-subrc <> 0. MESSAGE 'Error displaying ALV grid.' TYPE 'E'. ENDIF.
  CALL SCREEN 1000.
ENDFORM.

*----------------------------------------------------------------------*
* FORM build_fieldcat
*----------------------------------------------------------------------*
FORM build_fieldcat.
  DATA: ls_fcat TYPE lvc_s_fcat.

  DEFINE add_field.
    CLEAR ls_fcat.
    ls_fcat-fieldname = &1.
    ls_fcat-coltext   = &2.
    ls_fcat-seltext   = &2.
    ls_fcat-outputlen = &3.
    ls_fcat-edit      = &4.
    ls_fcat-hotspot   = &5.
    APPEND ls_fcat TO gt_fcat.
  END-OF-DEFINITION.

  add_field 'SEL'         'Sel'               3   ' ' 'X'.
  add_field 'GAS_DAY'     'Gas Day'          10   ' ' ' '.
  add_field 'LOCATION_ID' 'Location'         10   ' ' ' '.
  add_field 'MATERIAL'    'Material'         18   ' ' ' '.
  add_field 'STATE_CODE'  'State'             4   ' ' ' '.
  add_field 'QTY_SCM'     'Qty SCM'          15   ' ' ' '.
  add_field 'GAIL_ID'     'GAIL ID'          20   ' ' ' '.
  add_field 'OUTLINE_AGR' 'Outline Agreement' 10  ' ' ' '.
  add_field 'CHARG'       'Batch'            10   'X' ' '.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'T_COLOR'. ls_fcat-tech = abap_true. APPEND ls_fcat TO gt_fcat.
  ls_fcat-fieldname = 'CELLTAB'. ls_fcat-tech = abap_true. APPEND ls_fcat TO gt_fcat.
ENDFORM.

*----------------------------------------------------------------------*
* FORM set_alv_layout
*----------------------------------------------------------------------*
FORM set_alv_layout.
  gs_layout-cwidth_opt = abap_true.
  gs_layout-zebra      = abap_true.
  gs_layout-edit       = abap_true.
  gs_layout-ctab_fname = 'T_COLOR'.
  gs_layout-stylefname = 'CELLTAB'.
  gs_layout-sel_mode   = 'D'.
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
    ls_main-locid = ls_disp-location_id.
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
        ls_layout TYPE lvc_s_layo.

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
    LOOP AT it_errors INTO DATA(ls_e) WHERE msgty = 'E' OR msgty = 'A'.
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
*----------------------------------------------------------------------*
FORM handle_batch_mass_change.
  DATA: ls_disp   TYPE ty_display,
        lt_matnrs TYPE STANDARD TABLE OF matnr,
        lv_matnr  TYPE matnr,
        lt_batches TYPE STANDARD TABLE OF ty_batch_vals,
        ls_bat    TYPE ty_batch_vals,
        lt_f4     TYPE STANDARD TABLE OF ddshretval,
        ls_f4     TYPE ddshretval,
        lv_charg  TYPE charg_d.

  LOOP AT gt_display INTO ls_disp WHERE sel = abap_true.
    READ TABLE lt_matnrs WITH KEY table_line = ls_disp-material TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0. APPEND ls_disp-material TO lt_matnrs. ENDIF.
  ENDLOOP.
  IF lt_matnrs IS INITIAL.
    MESSAGE 'Select rows first.' TYPE 'S' DISPLAY LIKE 'W'. RETURN.
  ENDIF.

  LOOP AT lt_matnrs INTO lv_matnr.
    REFRESH: lt_batches, lt_f4.
    PERFORM get_valid_batches_for_material USING lv_matnr CHANGING lt_batches.
    IF lt_batches IS INITIAL. CONTINUE. ENDIF.
    LOOP AT lt_batches INTO ls_bat.
      ls_f4-fieldname = 'CHARG'. ls_f4-fieldval = ls_bat-charg.
      APPEND ls_f4 TO lt_f4. CLEAR ls_f4.
    ENDLOOP.
    CLEAR lv_charg.
    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING endpos_col = 60 endpos_row = 15 startpos_col = 5 startpos_row = 5
                titletext  = |Select Batch for Material { lv_matnr }|
      IMPORTING RETURNCODE = DATA(lv_rc)
      TABLES    valuetab   = lt_f4
      EXCEPTIONS OTHERS    = 1.
    IF sy-subrc = 0 AND lv_rc = ''.
      READ TABLE lt_f4 INDEX 1 INTO ls_f4.
      IF sy-subrc = 0. lv_charg = ls_f4-fieldval. ENDIF.
    ENDIF.
    IF lv_charg IS INITIAL. CONTINUE. ENDIF.
    LOOP AT gt_display INTO ls_disp WHERE sel = abap_true AND material = lv_matnr.
      ls_disp-charg = lv_charg.
      MODIFY gt_display FROM ls_disp.
    ENDLOOP.
  ENDLOOP.
  go_alv->refresh_table_display( ).
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
* Screen 1000 PAI
*----------------------------------------------------------------------*
MODULE user_command_1000 INPUT.
  CASE gv_okcode.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR gv_okcode.
ENDMODULE.
