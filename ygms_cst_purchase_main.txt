*&---------------------------------------------------------------------*
*& Report YGMS_CST_PURCHASE_MAIN
*& Description: ONGC CST Purchase Data - Gas Receipt Processing
*& Version: 4.0 - Editable ALV with buttons
*&---------------------------------------------------------------------*
REPORT ygms_cst_purchase_main.
TABLES : yrga_cst_pur.
*----------------------------------------------------------------------*
* Type Definitions
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_gas_receipt,
         gas_day       TYPE datum,
         ctp_id        TYPE ygms_de_ongc_ctp,
         location_id   TYPE ygms_de_loc_id,
         ongc_material TYPE ygms_de_ongc_mat,
         material      TYPE ygms_de_gail_mat,
         qty_scm       TYPE ygms_de_qty_mbg_cal,
         gcv           TYPE ygms_de_gcv,
         ncv           TYPE ygms_de_ncv,
         qty_mbg       TYPE ygms_de_qty_mbg_cal,
         ongc_id       TYPE c LENGTH 20,
       END OF ty_gas_receipt.
TYPES: BEGIN OF ty_loc_ctp_map,
         gail_loc_id TYPE ygms_de_loc_id,
         ongc_ctp_id TYPE ygms_de_ongc_ctp,
       END OF ty_loc_ctp_map.
* Validation ALV structure
TYPES: BEGIN OF ty_validation,
         location_id      TYPE ygms_de_loc_id,
         material         TYPE ygms_de_gail_mat,
         allocated_scm    TYPE ygms_de_qty_mbg_cal, "p DECIMALS 6,
         allocated_mbg    TYPE ygms_de_qty_mbg_cal, "p DECIMALS 6,
         ctp_id           TYPE ygms_de_ongc_ctp,
         ongc_material    TYPE ygms_de_ongc_mat,
         supply_scm       TYPE ygms_de_qty_mbg_cal, "p DECIMALS 6,
         supply_mbg       TYPE ygms_de_qty_mbg_cal, "p DECIMALS 6,
         diff_pur_sup_scm TYPE ygms_de_qty_mbg_cal, "p DECIMALS 6,
         diff_pur_sup_mbg TYPE ygms_de_qty_mbg_cal, "p DECIMALS 6,
       END OF ty_validation.
* ALV Display structure
TYPES: BEGIN OF ty_alv_display,
         exclude     TYPE c LENGTH 1,
         state_code  TYPE regio,
         state       TYPE bezei20,
         location_id TYPE ygms_de_loc_id,
         material    TYPE ygms_de_gail_mat,
         total_mbg   TYPE p DECIMALS 6,
         total_scm   TYPE p DECIMALS 6,
         gcv         TYPE p DECIMALS 6, "ygms_de_gcv,
         ncv         TYPE p DECIMALS 6, "ygms_de_ncv,
         day01       TYPE p DECIMALS 6,
         day02       TYPE p DECIMALS 6,
         day03       TYPE p DECIMALS 6,
         day04       TYPE p DECIMALS 6,
         day05       TYPE p DECIMALS 6,
         day06       TYPE p DECIMALS 6,
         day07       TYPE p DECIMALS 6,
         day08       TYPE p DECIMALS 6,
         day09       TYPE p DECIMALS 6,
         day10       TYPE p DECIMALS 6,
         day11       TYPE p DECIMALS 6,
         day12       TYPE p DECIMALS 6,
         day13       TYPE p DECIMALS 6,
         day14       TYPE p DECIMALS 6,
         day15       TYPE p DECIMALS 6,
         day16       TYPE p DECIMALS 6,
         celltab     TYPE lvc_t_styl,
       END OF ty_alv_display.
TYPES: BEGIN OF ty_final,
         vstel      TYPE vbap-vstel,
         abtnr      TYPE vbkd-abtnr,
         empst      TYPE empst,
         regio      TYPE t001w-regio,
         regio_desc TYPE bezei20,
         address    TYPE char60,
         matnr1     TYPE p DECIMALS 3,
         matnr2     TYPE p DECIMALS 3,
         matnr3     TYPE p DECIMALS 3,
         matnr4     TYPE p DECIMALS 3,
         matnr5     TYPE p DECIMALS 3,
         matnr6     TYPE p DECIMALS 3,
         matnr7     TYPE p DECIMALS 3,
         matnr8     TYPE p DECIMALS 3,
         matnr9     TYPE p DECIMALS 3,
         matnr10    TYPE p DECIMALS 3,
         matnr11    TYPE p DECIMALS 3,
         matnr12    TYPE p DECIMALS 3,
         matnr13    TYPE p DECIMALS 3,
         matnr14    TYPE p DECIMALS 3,
         matnr15    TYPE p DECIMALS 3,
       END OF ty_final.
TYPES: BEGIN OF ty_final1,
         vstel      TYPE vbap-vstel,
         abtnr      TYPE vbkd-abtnr,
         empst      TYPE empst,
         regio      TYPE t001w-regio,
         regio_desc TYPE bezei20,
         address    TYPE char60,
         matnr      TYPE matnr,
         matnr1     TYPE p DECIMALS 3,
       END OF ty_final1.
TYPES: BEGIN OF ty_vali_b2b,
         time_stamp    TYPE timestamp,
         gas_day       TYPE datum,
         ctp_id        TYPE ygms_de_ongc_ctp,
         ongc_material TYPE ygms_de_ongc_mat,
         received_on   TYPE datum,
         received_at   TYPE erzet,
         ongc_id       TYPE ygms_de_ongc_id,
         qty_scm       TYPE ygms_de_qty_scm,
         gcv           TYPE ygms_de_gcv,
         ncv           TYPE ygms_de_ncv,
         data_source   TYPE ygms_de_data_src,
         created_by    TYPE ernam,
         time          TYPE sy-uzeit,
         date          TYPE sy-datum,
       END OF ty_vali_b2b.
TYPES: BEGIN OF ty_data_daily,
         gas_day       TYPE string,
         ctp           TYPE string,
         ongc_material TYPE string,
         state_code    TYPE string,
         state         TYPE string,
         qty_in_scm    TYPE string,
         gcv           TYPE string,
         ncv           TYPE string,
         qty_in_mbg    TYPE string,
         ongc_id       TYPE string,
         gail_id       TYPE string,
       END OF ty_data_daily.
* Saved Data ALV structures (for View -> Saved Data sub-option)
TYPES: BEGIN OF ty_saved_daily,
         gas_day      TYPE datum,
         ctp          TYPE ygms_de_ongc_ctp,
         ongc_mater   TYPE ygms_de_ongc_mat,
         state_code   TYPE regio,
         state        TYPE bezei20,
         qty_in_scm   TYPE ygms_de_qty_mbg_cal,
         gcv          TYPE ygms_de_gcv,
         ncv          TYPE ygms_de_ncv,
         qty_in_mbg   TYPE ygms_de_qty_mbg_cal,
         ongc_id      TYPE c LENGTH 20,
         gail_id      TYPE c LENGTH 20,
         location     TYPE ygms_de_loc_id,
         material     TYPE ygms_de_gail_mat,
         exclude      TYPE c LENGTH 1,
         created_by   TYPE ernam,
         created_date TYPE datum,
         created_time TYPE sy-uzeit,
         sent_e       TYPE c LENGTH 12,
         sent_by      TYPE ernam,
         sent_on      TYPE datum,
         sent_at      TYPE sy-uzeit,
         deleted      TYPE flag,
       END OF ty_saved_daily.
TYPES: BEGIN OF ty_saved_fnt,
         date_from    TYPE datum,
         date_to      TYPE datum,
         ctp          TYPE ygms_de_ongc_ctp,
         ongc_mater   TYPE ygms_de_ongc_mat,
         state_code   TYPE regio,
         state        TYPE bezei20,
         qty_in_scm   TYPE ygms_de_qty_mbg_cal,
         gcv          TYPE ygms_de_gcv,
         ncv          TYPE ygms_de_ncv,
         qty_in_mbg   TYPE ygms_de_qty_mbg_cal,
         gail_id      TYPE c LENGTH 20,
         location     TYPE ygms_de_loc_id,
         material     TYPE ygms_de_gail_mat,
         created_by   TYPE ernam,
         created_date TYPE datum,
         created_time TYPE sy-uzeit,
         sent_e       TYPE c LENGTH 12,
         sent_by      TYPE ernam,
         sent_on      TYPE datum,
         sent_at      TYPE sy-uzeit,
         deleted      TYPE flag,
       END OF ty_saved_fnt.
DATA:     wa_final_daily TYPE ty_data_daily.
DATA: BEGIN OF ty_final_daily,
        record TYPE TABLE OF ty_data_daily,
      END OF ty_final_daily.
TYPES: BEGIN OF ty_data_fn,
         date_from     TYPE string,
         date_to       TYPE string,
         ctp           TYPE string,
         ongc_material TYPE string,
         state_code    TYPE string,
         state         TYPE string,
         qty_in_scm    TYPE string,
         gcv           TYPE string,
         ncv           TYPE string,
         qty_in_mbg    TYPE string,
         gail_id       TYPE string,
       END OF ty_data_fn.
DATA:     wa_final_fn TYPE ty_data_fn.
DATA: BEGIN OF ty_final_fn,
        record TYPE TABLE OF ty_data_fn,
      END OF ty_final_fn.
DATA: lv_api_dt TYPE yha_api_dtls.
DATA: lt_send_data TYPE TABLE OF yrga_cst_pur.
DATA: lt_send_data_fn TYPE TABLE OF yrga_cst_fn_data.
TYPES: BEGIN OF ty_out ,
         code    TYPE char20,
         message TYPE char100,
       END OF ty_out .
DATA: lv_json_download TYPE string .
DATA: lv_token_url     TYPE string,
      lv_api_get       TYPE string,
      lv_api_url       TYPE string,
      lv_client_id     TYPE string,
      lv_client_secret TYPE string,
      lv_proxy_host    TYPE string,
      lv_proxy_service TYPE string,
      lv_access_token  TYPE string,
      lv_response      TYPE string,
      log_data         TYPE string,
      itab             TYPE TABLE OF ty_out WITH HEADER LINE.
DATA: html           TYPE string,
      lv_http_client TYPE REF TO if_http_client,
      code           TYPE i,
      reason         TYPE string.
data g_error_api type flag.
DATA:
  lo_http_client   TYPE REF TO if_http_client.
*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
DATA: gv_loc_id TYPE ygms_de_loc_id.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_loc  FOR gv_loc_id OBLIGATORY,
                  s_date FOR sy-datum OBLIGATORY,
                  s_matnr FOR yrga_cst_pur-material NO-DISPLAY.
  PARAMETERS: p_alloc  TYPE char1 RADIOBUTTON GROUP r1 USER-COMMAND uc1,
              p_view   TYPE char1 RADIOBUTTON GROUP r1,
              p_send   TYPE char1 RADIOBUTTON GROUP r1,
              p_downld TYPE char1 RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK b1.
* Sub-options for View mode
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_valloc TYPE char1 RADIOBUTTON GROUP r2 DEFAULT 'X' MODIF ID viw,
              p_vsave  TYPE char1 RADIOBUTTON GROUP r2 MODIF ID viw,
              p_vsent  TYPE char1 RADIOBUTTON GROUP r2 MODIF ID viw.
SELECTION-SCREEN END OF BLOCK b2.
* Sub-options for Download mode
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_dxls TYPE char1 RADIOBUTTON GROUP r3 DEFAULT 'X' MODIF ID dwn,
              p_dpdf TYPE char1 RADIOBUTTON GROUP r3 MODIF ID dwn.
SELECTION-SCREEN END OF BLOCK b3.
*----------------------------------------------------------------------*
* Custom Popup Selection Screen for Email (Screen 2000)
*----------------------------------------------------------------------*
DATA: gv_email1   TYPE c LENGTH 120,
      gv_email2   TYPE c LENGTH 120,
      gv_email3   TYPE c LENGTH 120,
      gv_email4   TYPE c LENGTH 120,
      gv_email5   TYPE c LENGTH 120,
      gv_send_pdf TYPE c LENGTH 1 VALUE 'X',
      gv_send_xls TYPE c LENGTH 1 VALUE 'X'.

SELECTION-SCREEN BEGIN OF SCREEN 2000 TITLE TEXT-e00.
  SELECTION-SCREEN BEGIN OF BLOCK b_email WITH FRAME TITLE TEXT-e01.
    PARAMETERS: p_eml1 TYPE c LENGTH 120 LOWER CASE MODIF ID eml,
                p_eml2 TYPE c LENGTH 120 LOWER CASE MODIF ID eml,
                p_eml3 TYPE c LENGTH 120 LOWER CASE MODIF ID eml,
                p_eml4 TYPE c LENGTH 120 LOWER CASE MODIF ID eml,
                p_eml5 TYPE c LENGTH 120 LOWER CASE MODIF ID eml.
  SELECTION-SCREEN END OF BLOCK b_email.
  SELECTION-SCREEN BEGIN OF BLOCK b_fmt WITH FRAME TITLE TEXT-e02.
    PARAMETERS: p_pdf AS CHECKBOX DEFAULT 'X' MODIF ID fmt,
                p_xls AS CHECKBOX DEFAULT 'X' MODIF ID fmt.
  SELECTION-SCREEN END OF BLOCK b_fmt.
SELECTION-SCREEN END OF SCREEN 2000.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: gt_gas_receipt TYPE TABLE OF ty_gas_receipt,
      gt_loc_ctp_map TYPE TABLE OF ty_loc_ctp_map,
      gv_date_from   TYPE datum,
      gv_date_to     TYPE datum.
DATA: gt_alv_display TYPE TABLE OF ty_alv_display,
      gs_alv_display TYPE ty_alv_display.
DATA: gt_validation TYPE TABLE OF ty_validation,
      gs_validation TYPE ty_validation.
DATA: go_container TYPE REF TO cl_gui_custom_container,
      go_grid      TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_layout    TYPE lvc_s_layo,
      gt_exclude   TYPE ui_functions.
TYPE-POOLS : slis.
DATA: gt_fieldcat_slis TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      it_final         TYPE TABLE OF ty_final,
      it_final_main    TYPE TABLE OF ty_final1,
      wa_final_main    TYPE ty_final1.
DATA gt_cst_b2b_1 TYPE TABLE OF yrga_cst_b2b_1.
* Flags for button visibility and state
DATA: gv_allocated    TYPE abap_bool VALUE abap_false,  " Allocation done flag
      gv_validated    TYPE abap_bool VALUE abap_false,  " Validation done flag
      gv_save_enabled TYPE abap_bool VALUE abap_false,  " Save enabled flag (diff <= 1)
      gv_data_saved   TYPE abap_bool VALUE abap_false.  " Data saved flag - disable editing after save
* Global table for new receipt data (used by View Details in validation popup)
DATA: gt_new_receipt_data TYPE TABLE OF ty_vali_b2b.
DATA gs_new_receipt_data TYPE ty_vali_b2b.
* Saved Data view tables and mode flag
DATA: gt_saved_daily TYPE TABLE OF ty_saved_daily,
      gt_saved_fnt   TYPE TABLE OF ty_saved_fnt,
      gv_saved_view  TYPE c LENGTH 1 VALUE 'D'.  " D=Daily, F=Fortnightly
*----------------------------------------------------------------------*
* Class Definition for ALV Event Handler
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object e_interactive,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.
ENDCLASS.
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_toolbar.
    " Toolbar buttons are handled via PF-STATUS 'ZALV_STATUS'
    " This method is kept for potential future ALV grid toolbar customization
  ENDMETHOD.
  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'ALLOCATION' OR 'REALLOCATE'.
        DATA: lr_grid1        TYPE REF TO cl_gui_alv_grid,
              lt_fcat         TYPE lvc_t_fcat,
              ls_fcat         TYPE lvc_s_fcat,
              lv_day_edit     TYPE abap_bool,
              lv_new_day_edit TYPE abap_bool,
              ls_style        TYPE lvc_s_styl.
        " Save exclude flags before refresh
        TYPES: BEGIN OF lty_exclude_save,
                 location_id TYPE ygms_de_loc_id,
                 state_code  TYPE regio,
                 material    TYPE ygms_de_gail_mat,
                 exclude     TYPE c LENGTH 1,
               END OF lty_exclude_save.
        DATA: lt_excl_save TYPE TABLE OF lty_exclude_save,
              ls_excl_save TYPE lty_exclude_save.
        LOOP AT gt_alv_display INTO gs_alv_display WHERE exclude = 'X'.
          ls_excl_save-location_id = gs_alv_display-location_id.
          ls_excl_save-state_code  = gs_alv_display-state_code.
          ls_excl_save-material    = gs_alv_display-material.
          ls_excl_save-exclude     = gs_alv_display-exclude.
          APPEND ls_excl_save TO lt_excl_save.
        ENDLOOP.
        " Get ALV grid reference
        CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
          IMPORTING
            e_grid = lr_grid1.
        " Get current field catalog
        lr_grid1->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fcat ).
        " Check current edit state of DAY01 field
        READ TABLE lt_fcat INTO ls_fcat WITH KEY fieldname = 'DAY01'.
        IF sy-subrc = 0.
          lv_day_edit = ls_fcat-edit.
        ENDIF.
        REFRESH : gt_cst_b2b_1 , gt_gas_receipt , gt_alv_display ,it_final_main.
        PERFORM fetch_b2b_data.
        PERFORM map_location_ids.
        PERFORM map_material_names.
        PERFORM fetch_data_yrxr098.
        PERFORM build_alv_display_table.
        " Restore exclude flags after rebuild
        LOOP AT lt_excl_save INTO ls_excl_save.
          LOOP AT gt_alv_display ASSIGNING FIELD-SYMBOL(<fs_restore_cls>)
            WHERE location_id = ls_excl_save-location_id
              AND state_code  = ls_excl_save-state_code
              AND material    = ls_excl_save-material.
            <fs_restore_cls>-exclude = ls_excl_save-exclude.
          ENDLOOP.
        ENDLOOP.
        PERFORM handle_allocate.
        IF lv_day_edit = 'X'.
          PERFORM handle_edit.
        ENDIF.
      WHEN 'VALIDATE'.
        PERFORM handle_validate.
      WHEN 'EDIT'.
        PERFORM handle_edit.
      WHEN 'SAVE'.
        PERFORM handle_save.
      WHEN 'RESET'.
        PERFORM handle_reset.
      WHEN 'SEND'.
        PERFORM handle_send.
    ENDCASE.
  ENDMETHOD.
  METHOD handle_data_changed.
    DATA: ls_mod_cell TYPE lvc_s_modi.
    " First apply the changed values to gt_alv_display
    LOOP AT er_data_changed->mt_mod_cells INTO ls_mod_cell.
      READ TABLE gt_alv_display ASSIGNING FIELD-SYMBOL(<fs_row>) INDEX ls_mod_cell-row_id.
      IF sy-subrc = 0.
        ASSIGN COMPONENT ls_mod_cell-fieldname OF STRUCTURE <fs_row> TO FIELD-SYMBOL(<fs_field>).
        IF sy-subrc = 0.
          <fs_field> = ls_mod_cell-value.
        ENDIF.
        " When Exclude checkbox changes, apply to all rows of the same state within that location
        IF ls_mod_cell-fieldname = 'EXCLUDE'.
          LOOP AT gt_alv_display ASSIGNING FIELD-SYMBOL(<fs_state_row>)
            WHERE location_id = <fs_row>-location_id
              AND state_code  = <fs_row>-state_code.
            <fs_state_row>-exclude = <fs_row>-exclude.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.
    " Now recalculate totals
    PERFORM recalculate_totals.
  ENDMETHOD.
ENDCLASS.
DATA: go_event_handler TYPE REF TO lcl_event_handler.
*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  DATA: lv_first_day TYPE datum.
  lv_first_day = sy-datum.
  lv_first_day+6(2) = '01'.
  s_date-sign   = 'I'.
  s_date-option = 'BT'.
  s_date-low    = lv_first_day.
  s_date-high   = sy-datum.
  APPEND s_date.
*----------------------------------------------------------------------*
* Selection Screen Output - Show/Hide View sub-options
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'VIW'.
      IF p_view IS NOT INITIAL.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
    IF screen-group1 = 'DWN'.
      IF p_downld IS NOT INITIAL.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*----------------------------------------------------------------------*
* Force screen refresh when radio button group r1 changes
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON RADIOBUTTON GROUP r1.
  " Triggers AT SELECTION-SCREEN OUTPUT to show/hide View sub-options
*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  READ TABLE s_date INTO DATA(ls_date) INDEX 1.
  gv_date_from = ls_date-low.
  gv_date_to   = ls_date-high.
  PERFORM fetch_location_ctp_mappings.
  CHECK gt_loc_ctp_map IS NOT INITIAL.
  IF p_send IS NOT INITIAL.
    " Direct Send mode - bypass ALV, go straight to send flow
    PERFORM handle_send_direct.
  ELSEIF p_downld IS NOT INITIAL.
    " Download mode - download files to local computer
    PERFORM handle_download.
  ELSEIF p_view IS NOT INITIAL AND p_vsave IS NOT INITIAL.
    " View -> Saved Data: display data from YRGA_CST_PUR / YRGA_CST_FN_DATA
    PERFORM fetch_saved_data.
    PERFORM display_saved_data_alv.
  ELSEIF p_view IS NOT INITIAL AND p_vsent IS NOT INITIAL.
    " View -> Sent B2B Data: fetch from YRGA_CST_B2B_2 / YRGA_CST_B2B_3
    PERFORM fetch_sent_data.
    PERFORM display_saved_data_alv.
  ELSE.
    PERFORM fetch_b2b_data.
    CHECK gt_gas_receipt IS NOT INITIAL.
    PERFORM map_location_ids.
    PERFORM map_material_names.
      " Validate calorific values after mapping so GT_GAS_RECEIPT has location_id populated
    IF p_view IS INITIAL.
      DATA: lv_cv_valid TYPE abap_bool.
      PERFORM validate_cv_data CHANGING lv_cv_valid.
      IF lv_cv_valid = abap_false.
        MESSAGE s000(ygms_msg) WITH 'CV validation failed. Program cannot proceed.'.
        RETURN.
      ENDIF.
    ENDIF.
    PERFORM fetch_data_yrxr098.
    IF p_view IS INITIAL.
      PERFORM build_alv_display_table.
    ELSE.
      " View -> Allocation Details: same as existing view logic
      PERFORM build_alv_display_table_view.
    ENDIF.
    PERFORM display_editable_alv.
  ENDIF.
*&---------------------------------------------------------------------*
*& Form FETCH_LOCATION_CTP_MAPPINGS
*&---------------------------------------------------------------------*
FORM fetch_location_ctp_mappings.
  DATA: lt_loc_map TYPE TABLE OF yrga_cst_loc_map.
  IF s_loc[] IS NOT INITIAL.
    SELECT * FROM yrga_cst_loc_map
      INTO TABLE lt_loc_map
      WHERE gail_loc_id IN s_loc
        AND valid_from <= gv_date_from
        AND valid_to   >= gv_date_to.
*  ELSE.
*    SELECT * FROM yrga_cst_loc_map
*      INTO TABLE lt_loc_map
*      WHERE valid_from <= gv_date_from
*        AND valid_to   >= gv_date_to.
  ENDIF.
  IF lt_loc_map IS INITIAL.
    MESSAGE s000(ygms_msg) WITH 'No valid Location-CTP mappings found for date range'.
    RETURN.
  ENDIF.
  LOOP AT lt_loc_map INTO DATA(ls_loc_map).
    DATA(ls_map) = VALUE ty_loc_ctp_map(
      gail_loc_id = ls_loc_map-gail_loc_id
      ongc_ctp_id = ls_loc_map-ongc_ctp_id
    ).
    APPEND ls_map TO gt_loc_ctp_map.
  ENDLOOP.
  DATA(lv_count) = lines( gt_loc_ctp_map ).
  MESSAGE s000(ygms_msg) WITH lv_count 'Location-CTP mappings found'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FETCH_B2B_DATA
*&---------------------------------------------------------------------*
FORM fetch_b2b_data.
  DATA: lt_b2b_data TYPE TABLE OF yrga_cst_b2b_1.
  DATA: c_tgqty TYPE msego2-adqnt,
        i_trqty TYPE msego2-adqnt,
        lv_gcv  TYPE oib_par_fltp,
        lv_ncv  TYPE oib_par_fltp.
  DATA: lt_ctp_ids TYPE TABLE OF ygms_de_ongc_ctp.
  LOOP AT gt_loc_ctp_map INTO DATA(ls_map).
    COLLECT ls_map-ongc_ctp_id INTO lt_ctp_ids.
  ENDLOOP.
  IF lt_ctp_ids IS NOT INITIAL.
    SELECT * FROM yrga_cst_b2b_1
      INTO TABLE lt_b2b_data
      FOR ALL ENTRIES IN lt_ctp_ids
      WHERE ctp_id  = lt_ctp_ids-table_line
        AND gas_day BETWEEN gv_date_from AND gv_date_to
        AND qty_scm > 0.
  ENDIF.
  IF lt_b2b_data IS INITIAL.
    CONCATENATE 'No receipt data available for' s_loc-low 'for the entered period'
          INTO DATA(l_error) SEPARATED BY space.
    MESSAGE s000(ygms_msg) WITH l_error.
    RETURN.
  ENDIF.
  IF lt_b2b_data[] IS NOT INITIAL.
    SORT lt_b2b_data BY ctp_id gas_day ongc_material ASCENDING time_stamp DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_b2b_data COMPARING ctp_id gas_day ongc_material.
  ENDIF.
  LOOP AT lt_b2b_data INTO DATA(ls_b2b).
    DATA(ls_receipt) = VALUE ty_gas_receipt(
      gas_day       = ls_b2b-gas_day
      ctp_id        = ls_b2b-ctp_id
      ongc_material = ls_b2b-ongc_material
      qty_scm       = ls_b2b-qty_scm
      gcv           = ls_b2b-gcv
      ncv           = ls_b2b-ncv
      ongc_id       = ls_b2b-ongc_id
    ).
    i_trqty = ls_b2b-qty_scm.
    lv_gcv = ls_b2b-gcv.
    lv_ncv = ls_b2b-ncv.
    CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
      EXPORTING
        i_trqty = i_trqty
        i_truom = 'SM3'
        i_tguom = 'MBG'
        lv_gcv  = lv_gcv
        lv_ncv  = lv_ncv
      CHANGING
        c_tgqty = c_tgqty.
    ls_receipt-qty_mbg = c_tgqty.
    APPEND ls_receipt TO gt_gas_receipt.
  ENDLOOP.
  DATA(lv_count) = lines( gt_gas_receipt ).
  MESSAGE s000(ygms_msg) WITH lv_count 'B2B records fetched'.
  MOVE lt_b2b_data[] TO gt_cst_b2b_1[].
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAP_LOCATION_IDS
*&---------------------------------------------------------------------*
FORM map_location_ids.
  LOOP AT gt_gas_receipt ASSIGNING FIELD-SYMBOL(<fs_receipt>).
    READ TABLE gt_loc_ctp_map INTO DATA(ls_map)
      WITH KEY ongc_ctp_id = <fs_receipt>-ctp_id.
    IF sy-subrc = 0.
      <fs_receipt>-location_id = ls_map-gail_loc_id.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAP_MATERIAL_NAMES
*&---------------------------------------------------------------------*
FORM map_material_names.
  DATA: lt_mat_map TYPE TABLE OF yrga_cst_mat_map.
  DATA: BEGIN OF ls_loc_mat,
          location_id   TYPE ygms_de_loc_id,
          ongc_material TYPE ygms_de_ongc_mat,
        END OF ls_loc_mat,
        lt_loc_mat LIKE TABLE OF ls_loc_mat.
  LOOP AT gt_gas_receipt INTO DATA(ls_receipt).
    ls_loc_mat-location_id   = ls_receipt-location_id.
    ls_loc_mat-ongc_material = ls_receipt-ongc_material.
    COLLECT ls_loc_mat INTO lt_loc_mat.
  ENDLOOP.
  IF lt_loc_mat IS NOT INITIAL.
    SELECT * FROM yrga_cst_mat_map
      INTO TABLE lt_mat_map
      FOR ALL ENTRIES IN lt_loc_mat
      WHERE location_id    = lt_loc_mat-location_id
        AND ongc_material  = lt_loc_mat-ongc_material
        AND valid_from    <= gv_date_from
        AND valid_to      >= gv_date_to.
  ENDIF.
  LOOP AT gt_gas_receipt ASSIGNING FIELD-SYMBOL(<fs_receipt>).
    READ TABLE lt_mat_map INTO DATA(ls_mat)
      WITH KEY location_id   = <fs_receipt>-location_id
               ongc_material = <fs_receipt>-ongc_material.
    IF sy-subrc = 0.
      <fs_receipt>-material = ls_mat-gail_material.
    ENDIF.
  ENDLOOP.
  SELECT * INTO TABLE @DATA(it_yrva_cst_pur_mat)
    FROM yrva_cst_pur_mat.
  IF sy-subrc = 0.
    RANGES r_matnr FOR mara-matnr.
    LOOP AT it_yrva_cst_pur_mat INTO DATA(wa_yrva_cst_pur_mat).
      r_matnr-low = wa_yrva_cst_pur_mat-matnr.
      r_matnr-sign = 'I'.
      r_matnr-option = 'EQ'.
      APPEND r_matnr.
    ENDLOOP.
    SORT gt_gas_receipt BY material.
    DELETE gt_gas_receipt WHERE material NOT IN r_matnr.
  ENDIF.
  SORT gt_gas_receipt BY material.
  DELETE gt_gas_receipt WHERE material NOT IN s_matnr.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FETCH_DATA_YRXR098
*&---------------------------------------------------------------------*
FORM fetch_data_yrxr098.
  SUBMIT yrvr098_states_qty_report USING SELECTION-SCREEN '1000'
        WITH s_fkdat IN s_date
        WITH p_ex = 'X'
        AND RETURN.
  IMPORT gt_fieldcat = gt_fieldcat_slis  FROM MEMORY ID 'FC'.
  IMPORT it_final FROM MEMORY ID 'FI'.
  LOOP AT it_final INTO DATA(wa_final).
    MOVE-CORRESPONDING wa_final TO wa_final_main.
    LOOP AT gt_fieldcat_slis INTO DATA(wa_fieldcat) WHERE fieldname CS 'MATNR'.
      REPLACE ALL OCCURRENCES OF 'Qty in MMBTU of' IN wa_fieldcat-seltext_l WITH space.
      CONDENSE wa_fieldcat-seltext_l.
      wa_final_main-matnr = wa_fieldcat-seltext_l.
      ASSIGN COMPONENT wa_fieldcat-fieldname OF STRUCTURE wa_final TO FIELD-SYMBOL(<fs_value>).
      IF sy-subrc = 0.
        wa_final_main-matnr1 = <fs_value>.
      ENDIF.
      APPEND wa_final_main TO it_final_main.
    ENDLOOP.
  ENDLOOP.
  SORT it_final_main BY matnr.
  DELETE it_final_main WHERE matnr NOT IN s_matnr.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_ALV_DISPLAY_TABLE
*&---------------------------------------------------------------------*
FORM build_alv_display_table.
  DATA: lt_state_info TYPE TABLE OF t005u,
        ls_alv        TYPE ty_alv_display,
        lv_day_num    TYPE i,
        lv_day_field  TYPE string.
  DATA it_final_temp TYPE TABLE OF ty_final1.
  MOVE it_final_main[] TO it_final_temp[].
  SORT it_final_temp BY matnr.
  DELETE ADJACENT DUPLICATES FROM it_final_temp COMPARING matnr.
  LOOP AT it_final_temp INTO DATA(wa_final_temp).
    READ TABLE it_final_main TRANSPORTING NO FIELDS WITH KEY
      regio = 'GJ' matnr = wa_final_temp-matnr.
    IF sy-subrc <> 0.
      CLEAR wa_final_main.
      wa_final_main-matnr = wa_final_temp-matnr.
      wa_final_main-regio = 'GJ'.
      wa_final_main-regio_desc = 'Gujrat'.
      wa_final_main-empst = wa_final_temp-empst.
      APPEND wa_final_main TO it_final_main.
    ENDIF.
  ENDLOOP.
  LOOP AT it_final_main INTO wa_final_main.
    ls_alv-state_code = wa_final_main-regio.
    ls_alv-state      = wa_final_main-regio_desc.
    ls_alv-material   = wa_final_main-matnr.
    ls_alv-location_id = wa_final_main-empst.
    APPEND ls_alv TO gt_alv_display.
  ENDLOOP.
  DATA it_gas_receipt TYPE TABLE OF ty_gas_receipt.
  MOVE gt_gas_receipt[] TO it_gas_receipt[].
  SORT it_gas_receipt BY location_id material.
  DELETE ADJACENT DUPLICATES FROM it_gas_receipt COMPARING location_id material.
  LOOP AT it_gas_receipt INTO DATA(wa_gas_temp).
    READ TABLE gt_alv_display TRANSPORTING NO FIELDS
      WITH KEY location_id = wa_gas_temp-location_id
               material    = wa_gas_temp-material
               state_code  = 'GJ'.
    IF sy-subrc <> 0.
      CLEAR ls_alv.
      ls_alv-state_code  = 'GJ'.
      ls_alv-state       = 'Gujrat'.
      ls_alv-material    = wa_gas_temp-material.
      ls_alv-location_id = wa_gas_temp-location_id.
      APPEND ls_alv TO gt_alv_display.
      CLEAR wa_final_main.
      wa_final_main-empst      = wa_gas_temp-location_id.
      wa_final_main-regio      = 'GJ'.
      wa_final_main-regio_desc = 'Gujrat'.
      wa_final_main-matnr      = wa_gas_temp-material.
      APPEND wa_final_main TO it_final_main.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_EDITABLE_ALV
*&---------------------------------------------------------------------*
FORM display_editable_alv.
  DATA: ls_fieldcat TYPE lvc_s_fcat,
        lv_day      TYPE i,
        lv_fname    TYPE lvc_fname,
        lv_date     TYPE datum,
        lv_date_str TYPE c LENGTH 10,
        lv_num_days TYPE i.
  " Compute number of days dynamically to support fortnights with 13-16 days
  lv_num_days = gv_date_to - gv_date_from + 1.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'EXCLUDE'.
  ls_fieldcat-coltext   = 'Exclude'.
  ls_fieldcat-checkbox  = abap_true.
  IF p_view IS INITIAL.
    ls_fieldcat-edit      = abap_true.
  ENDIF.
  ls_fieldcat-outputlen = 7.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'STATE_CODE'.
  ls_fieldcat-coltext   = 'State Code'.
  ls_fieldcat-outputlen = 10.
  ls_fieldcat-edit      = abap_false.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'STATE'.
  ls_fieldcat-coltext   = 'State'.
  ls_fieldcat-outputlen = 15.
  ls_fieldcat-edit      = abap_false.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'LOCATION_ID'.
  ls_fieldcat-coltext   = 'Location ID'.
  ls_fieldcat-outputlen = 12.
  ls_fieldcat-edit      = abap_false.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MATERIAL'.
  ls_fieldcat-coltext   = 'Material'.
  ls_fieldcat-outputlen = 18.
  ls_fieldcat-edit      = abap_false.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TOTAL_MBG'.
  ls_fieldcat-coltext   = 'Total, MBG'.
  ls_fieldcat-outputlen = 12.
  ls_fieldcat-do_sum    = abap_true.
  ls_fieldcat-edit      = abap_false.
  ls_fieldcat-decimals_o  = 3.
  ls_fieldcat-inttype   = 'P'.
  ls_fieldcat-decimals  = 6.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TOTAL_SCM'.
  ls_fieldcat-coltext   = 'Total, Sm3'.
  ls_fieldcat-outputlen = 12.
  ls_fieldcat-do_sum    = abap_true.
  ls_fieldcat-edit      = abap_false.
  ls_fieldcat-decimals_o  = 3.
  ls_fieldcat-inttype   = 'P'.
  ls_fieldcat-decimals  = 6.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GCV'.
  ls_fieldcat-coltext   = 'Wt. Avg. GCV'.
  ls_fieldcat-outputlen = 12.
*  ls_fieldcat-do_sum    = abap_true.
  ls_fieldcat-decimals_o  = 3.
  ls_fieldcat-inttype   = 'P'.
  ls_fieldcat-decimals  = 6.
  ls_fieldcat-edit      = abap_false.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NCV'.
  ls_fieldcat-coltext   = 'Wt. Avg. NCV'.
  ls_fieldcat-outputlen = 12.
  ls_fieldcat-decimals_o  = 3.
  ls_fieldcat-inttype   = 'P'.
  ls_fieldcat-decimals  = 6.
*  ls_fieldcat-do_sum    = abap_true.
  ls_fieldcat-edit      = abap_false.
  APPEND ls_fieldcat TO gt_fieldcat.
  lv_date = gv_date_from.
  DO lv_num_days TIMES.
    lv_day = sy-index.
    CLEAR ls_fieldcat.
    CASE lv_day.
      WHEN 1.  ls_fieldcat-fieldname = 'DAY01'.
      WHEN 2.  ls_fieldcat-fieldname = 'DAY02'.
      WHEN 3.  ls_fieldcat-fieldname = 'DAY03'.
      WHEN 4.  ls_fieldcat-fieldname = 'DAY04'.
      WHEN 5.  ls_fieldcat-fieldname = 'DAY05'.
      WHEN 6.  ls_fieldcat-fieldname = 'DAY06'.
      WHEN 7.  ls_fieldcat-fieldname = 'DAY07'.
      WHEN 8.  ls_fieldcat-fieldname = 'DAY08'.
      WHEN 9.  ls_fieldcat-fieldname = 'DAY09'.
      WHEN 10. ls_fieldcat-fieldname = 'DAY10'.
      WHEN 11. ls_fieldcat-fieldname = 'DAY11'.
      WHEN 12. ls_fieldcat-fieldname = 'DAY12'.
      WHEN 13. ls_fieldcat-fieldname = 'DAY13'.
      WHEN 14. ls_fieldcat-fieldname = 'DAY14'.
      WHEN 15. ls_fieldcat-fieldname = 'DAY15'.
      WHEN 16. ls_fieldcat-fieldname = 'DAY16'.
    ENDCASE.
    WRITE lv_date TO lv_date_str DD/MM/YYYY.
    REPLACE ALL OCCURRENCES OF '/' IN lv_date_str WITH '-'.
    ls_fieldcat-coltext   = lv_date_str.
    ls_fieldcat-outputlen = 12.
    ls_fieldcat-inttype   = 'P'.
    ls_fieldcat-decimals  = 6.             " Must match internal p DECIMALS 6 to avoid divide on edit
    ls_fieldcat-decimals_o  = 3.           " Output decimals displayed to user (3 places)
    ls_fieldcat-edit      = abap_false.  " Not editable initially, enabled via Edit button
    ls_fieldcat-do_sum    = abap_true.
    APPEND ls_fieldcat TO gt_fieldcat.
    lv_date = lv_date + 1.
  ENDDO.
  gs_layout-cwidth_opt = abap_true.
  gs_layout-zebra      = abap_true.
  gs_layout-sel_mode   = 'A'.
  gs_layout-edit       = abap_false.  " Disable grid-level editing, use field catalog for specific fields
  gs_layout-stylefname = 'CELLTAB'.  " Cell style field for row-level edit control
  gs_layout-grid_title = 'ONGC CST Statewise Allocation'.
  SORT gt_alv_display BY state location_id material.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fieldcat
    TABLES
      t_outtab                 = gt_alv_display
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  DATA: lr_grid  TYPE REF TO cl_gui_alv_grid,
        ls_extab TYPE slis_extab,
        lt_excl  TYPE slis_t_extab.
  " Copy incoming exclusion table
  lt_excl = rt_extab.
  " Point 6: In view mode, show Reallocate instead of Allocate
  " Note: 'REALLOCATE' function code must be defined in PF-STATUS ZALV_STATUS
  IF p_view IS NOT INITIAL.
    CLEAR ls_extab.
    ls_extab-fcode = 'ALLOCATION'.
    APPEND ls_extab TO lt_excl.
  ELSE.
    CLEAR ls_extab.
    ls_extab-fcode = 'REALLOCATE'.
    APPEND ls_extab TO lt_excl.
  ENDIF.
  " Before allocation: only show ALLOCATION/REALLOCATE and RESET
  IF gv_allocated = abap_false.
    CLEAR ls_extab.
    ls_extab-fcode = 'VALIDATE'.
    APPEND ls_extab TO lt_excl.
    CLEAR ls_extab.
    ls_extab-fcode = 'EDIT'.
    APPEND ls_extab TO lt_excl.
    CLEAR ls_extab.
    ls_extab-fcode = 'SAVE'.
    APPEND ls_extab TO lt_excl.
  ELSE.
    " After allocation: exclude SAVE if validation not done or not enabled or data already saved
    IF gv_validated = abap_false OR gv_save_enabled = abap_false OR gv_data_saved = abap_true.
      CLEAR ls_extab.
      ls_extab-fcode = 'SAVE'.
      APPEND ls_extab TO lt_excl.
    ENDIF.
  ENDIF.
  " SEND only visible after data is saved
  IF gv_data_saved = abap_false.
    CLEAR ls_extab.
    ls_extab-fcode = 'SEND'.
    APPEND ls_extab TO lt_excl.
  ENDIF.
  " After data saved: disable EDIT, ALLOCATION/REALLOCATE, RESET
  IF gv_data_saved = abap_true.
    CLEAR ls_extab.
    ls_extab-fcode = 'EDIT'.
    APPEND ls_extab TO lt_excl.
    CLEAR ls_extab.
    ls_extab-fcode = 'ALLOCATION'.
    APPEND ls_extab TO lt_excl.
    CLEAR ls_extab.
    ls_extab-fcode = 'REALLOCATE'.
    APPEND ls_extab TO lt_excl.
    CLEAR ls_extab.
    ls_extab-fcode = 'RESET'.
    APPEND ls_extab TO lt_excl.
  ENDIF.
  SET PF-STATUS 'ZALV_STATUS' EXCLUDING lt_excl.
  " Get ALV grid reference and register data_changed event
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.
  IF lr_grid IS BOUND AND go_event_handler IS NOT BOUND.
    CREATE OBJECT go_event_handler.
    SET HANDLER go_event_handler->handle_data_changed FOR lr_grid.
    " Register for edit events
    lr_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command USING r_ucomm     TYPE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.
  lr_grid->check_changed_data( ).
  " Debug: Show which function code was triggered (can be removed later)
  MESSAGE s000(ygms_msg) WITH 'Function code:' r_ucomm.
  CASE r_ucomm.
    WHEN 'ALLOCATION' OR 'REALLOCATE'.
      DATA: lr_grid1        TYPE REF TO cl_gui_alv_grid,
            lt_fcat         TYPE lvc_t_fcat,
            ls_fcat         TYPE lvc_s_fcat,
            lv_day_edit     TYPE abap_bool,
            lv_new_day_edit TYPE abap_bool,
            ls_style        TYPE lvc_s_styl.
      " Save exclude flags before refresh
      TYPES: BEGIN OF ty_exclude_save,
               location_id TYPE ygms_de_loc_id,
               state_code  TYPE regio,
               material    TYPE ygms_de_gail_mat,
               exclude     TYPE c LENGTH 1,
             END OF ty_exclude_save.
      DATA: lt_exclude_save TYPE TABLE OF ty_exclude_save,
            ls_exclude_save TYPE ty_exclude_save.
      LOOP AT gt_alv_display INTO gs_alv_display WHERE exclude = 'X'.
        ls_exclude_save-location_id = gs_alv_display-location_id.
        ls_exclude_save-state_code  = gs_alv_display-state_code.
        ls_exclude_save-material    = gs_alv_display-material.
        ls_exclude_save-exclude     = gs_alv_display-exclude.
        APPEND ls_exclude_save TO lt_exclude_save.
      ENDLOOP.
      " Get ALV grid reference
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = lr_grid1.
      " Get current field catalog
      lr_grid1->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fcat ).
      " Check current edit state of DAY01 field
      READ TABLE lt_fcat INTO ls_fcat WITH KEY fieldname = 'DAY01'.
      IF sy-subrc = 0.
        lv_day_edit = ls_fcat-edit.
      ENDIF.
      REFRESH : gt_cst_b2b_1 , gt_gas_receipt , gt_alv_display ,it_final_main.
      PERFORM fetch_b2b_data.
      PERFORM map_location_ids.
      PERFORM map_material_names.
      PERFORM fetch_data_yrxr098.
      PERFORM build_alv_display_table.
      " Restore exclude flags after rebuild
      LOOP AT lt_exclude_save INTO ls_exclude_save.
        LOOP AT gt_alv_display ASSIGNING FIELD-SYMBOL(<fs_restore>)
          WHERE location_id = ls_exclude_save-location_id
            AND state_code  = ls_exclude_save-state_code
            AND material    = ls_exclude_save-material.
          <fs_restore>-exclude = ls_exclude_save-exclude.
        ENDLOOP.
      ENDLOOP.
      PERFORM handle_allocate.
      IF lv_day_edit = 'X'.
        PERFORM handle_edit.
      ENDIF.
    WHEN 'VALIDATE'.
      PERFORM handle_validate.
    WHEN 'EDIT'.
      PERFORM handle_edit.
    WHEN 'SAVE'.
      PERFORM handle_save.
    WHEN 'RESET'.
      PERFORM handle_reset.
    WHEN 'SEND'.
      PERFORM handle_send.
  ENDCASE.
  rs_selfield-refresh = abap_true.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_ALLOCATE
*&---------------------------------------------------------------------*
FORM handle_allocate.
  TYPES : BEGIN OF ty_sales,
            empst          TYPE empst,
            matnr          TYPE matnr,
            qty_mbg        TYPE ygms_de_qty_mbg_cal,
            qty_mbg_supply TYPE ygms_de_qty_mbg_cal,
            qty_mbg_diff   TYPE ygms_de_qty_mbg_cal,
            qty_allocated  TYPE ygms_de_qty_mbg_cal,
            qty_access     TYPE ygms_de_qty_mbg_cal,
          END OF ty_sales.
  TYPES : BEGIN OF ty_asales,
            empst   TYPE empst,
            regio   TYPE regio,
            qty_mbg TYPE ygms_de_qty_mbg_cal,
          END OF ty_asales.
  DATA l_left TYPE ygms_de_qty_mbg_cal.
  TYPES : BEGIN OF ty_state,
            empst         TYPE empst,
            state_code    TYPE regio,
            state         TYPE bezei20,
            matnr         TYPE matnr,
            qty_mbg       TYPE ygms_de_qty_mbg_cal,
            qty_mbg_diff  TYPE ygms_de_qty_mbg_cal,
            qty_allocated TYPE ygms_de_qty_mbg_cal,
            percentage    TYPE ygms_ongc_percentage,
          END OF ty_state.
  DATA : it_sales  TYPE TABLE OF ty_sales,
         wa_sales  TYPE ty_sales,
         it_state  TYPE TABLE OF ty_state,
         wa_state  TYPE ty_state,
         it_state1 TYPE TABLE OF ty_state,
         it_asales TYPE TABLE OF ty_asales,
         wa_asales TYPE ty_asales.
  " Collect sales data per location + state (skip excluded states)
  LOOP AT it_final_main INTO wa_final_main.
    " Skip if this state is excluded in the main ALV table
    READ TABLE gt_alv_display TRANSPORTING NO FIELDS
      WITH KEY location_id = wa_final_main-empst
               state_code  = wa_final_main-regio
               exclude     = 'X'.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
    wa_asales-empst   = wa_final_main-empst.
    wa_asales-qty_mbg = wa_final_main-matnr1.
    wa_asales-regio   = wa_final_main-regio.
    COLLECT wa_asales INTO it_asales.
    CLEAR wa_asales.
  ENDLOOP.
  SORT it_asales BY empst qty_mbg DESCENDING.
  " Collect sales and state data per location + material (skip excluded states)
  LOOP AT it_final_main INTO wa_final_main.
    " Skip if this state is excluded in the main ALV table
    READ TABLE gt_alv_display TRANSPORTING NO FIELDS
      WITH KEY location_id = wa_final_main-empst
               state_code  = wa_final_main-regio
               exclude     = 'X'.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
    IF wa_final_main-regio <> 'GJ'.
      wa_sales-empst = wa_final_main-empst.
      wa_sales-matnr = wa_final_main-matnr.
      wa_sales-qty_mbg = wa_final_main-matnr1.
      COLLECT wa_sales INTO it_sales.
      CLEAR wa_sales.
    ENDIF.
    CLEAR wa_state.
    wa_state-empst      = wa_final_main-empst.
    wa_state-state_code = wa_final_main-regio.
    wa_state-state      = wa_final_main-regio_desc.
    wa_state-qty_mbg    = wa_final_main-matnr1.
    wa_state-matnr      = wa_final_main-matnr.
    COLLECT wa_state INTO it_state.
  ENDLOOP.
  " Collect supply data per location + material
  LOOP AT gt_gas_receipt INTO DATA(wa_gas_receipt).
    wa_sales-empst          = wa_gas_receipt-location_id.
    wa_sales-matnr          = wa_gas_receipt-material.
    wa_sales-qty_mbg_supply = wa_gas_receipt-qty_mbg.
    COLLECT wa_sales INTO it_sales.
    CLEAR wa_sales.
  ENDLOOP.
  " Calculate diff per location + material
  LOOP AT it_sales ASSIGNING FIELD-SYMBOL(<fs_sales>).
    <fs_sales>-qty_mbg_diff = <fs_sales>-qty_mbg_supply - <fs_sales>-qty_mbg.
  ENDLOOP.
  SORT it_sales BY empst qty_mbg_diff DESCENDING.
  SORT it_asales BY empst qty_mbg DESCENDING.
  CLEAR l_left.
  DATA l_exit TYPE flag.
  " Allocate per location + material
  LOOP AT it_sales ASSIGNING <fs_sales>.
    CLEAR l_exit.
    l_left = <fs_sales>-qty_mbg_diff.
    IF l_left < 0.
      LOOP AT it_asales INTO wa_asales WHERE empst = <fs_sales>-empst.
        LOOP AT it_state ASSIGNING FIELD-SYMBOL(<fs_state>) WHERE
          empst = <fs_sales>-empst AND
          matnr = <fs_sales>-matnr AND state_code = wa_asales-regio.
          IF sy-subrc = 0.
            IF <fs_state>-qty_mbg > abs( l_left ).
              <fs_state>-qty_allocated = <fs_state>-qty_mbg + l_left.
              l_exit = 'X'.
              EXIT.
            ELSE.
              l_left = l_left + <fs_state>-qty_mbg.
              CLEAR <fs_state>-qty_allocated.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF l_exit = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.
    ELSE.
      READ TABLE it_state ASSIGNING <fs_state>
        WITH KEY empst = <fs_sales>-empst state_code = 'GJ' matnr = <fs_sales>-matnr.
      IF sy-subrc = 0.
        <fs_state>-qty_allocated = l_left + <fs_state>-qty_allocated.
        CLEAR l_left.
      ENDIF.
    ENDIF.
    CLEAR l_left.
  ENDLOOP.
  LOOP AT it_state ASSIGNING <fs_state> WHERE qty_allocated IS INITIAL.
    <fs_state>-qty_allocated = <fs_state>-qty_mbg.
  ENDLOOP.
  " Calculate percentage per location + material
  MOVE it_state[] TO it_state1[].
  SORT it_state1 BY empst matnr.
  DELETE ADJACENT DUPLICATES FROM it_state1 COMPARING empst matnr.
  LOOP AT it_state1 INTO DATA(wa_state1).
    CLEAR l_left.
    LOOP AT it_state INTO wa_state WHERE empst = wa_state1-empst AND matnr = wa_state1-matnr.
      l_left = l_left + wa_state-qty_allocated.
    ENDLOOP.
    LOOP AT it_state ASSIGNING <fs_state> WHERE empst = wa_state1-empst AND matnr = wa_state1-matnr.
      <fs_state>-percentage = ( <fs_state>-qty_allocated / l_left ) * 100.
    ENDLOOP.
  ENDLOOP.
  DATA l_day TYPE char10.
  DATA l_index(2) TYPE n.
  DATA l_date TYPE sy-datum.
  DATA l_ncv TYPE ygms_de_qty_mbg_cal."ygms_de_gcv.
  DATA l_gcv TYPE ygms_de_qty_mbg_cal."ygms_de_gcv.
  DATA l_day_sm3 TYPE p DECIMALS 6.
  " 2.1b: Clear ALV day data before allocation to prevent additive quantities on repeated clicks
  LOOP AT gt_alv_display ASSIGNING FIELD-SYMBOL(<fs_clear>).
    CLEAR: <fs_clear>-total_mbg, <fs_clear>-total_scm,
           <fs_clear>-gcv, <fs_clear>-ncv,
           <fs_clear>-day01, <fs_clear>-day02, <fs_clear>-day03,
           <fs_clear>-day04, <fs_clear>-day05, <fs_clear>-day06,
           <fs_clear>-day07, <fs_clear>-day08, <fs_clear>-day09,
           <fs_clear>-day10, <fs_clear>-day11, <fs_clear>-day12,
           <fs_clear>-day13, <fs_clear>-day14, <fs_clear>-day15,
           <fs_clear>-day16.
  ENDLOOP.
  " Apply allocation per location + state + material
  LOOP AT it_state INTO wa_state WHERE percentage IS NOT INITIAL.
    LOOP AT gt_alv_display ASSIGNING FIELD-SYMBOL(<fs_alv>)
      WHERE location_id = wa_state-empst
        AND state_code  = wa_state-state_code
        AND material    = wa_state-matnr
        AND exclude     IS INITIAL.
      CLEAR l_index.
      CLEAR: l_ncv, l_gcv,l_day_sm3.
      l_date = s_date-low.
      DATA lv_alloc_days TYPE i.
      lv_alloc_days = gv_date_to - gv_date_from + 1.
      DO lv_alloc_days TIMES.
        l_index = l_index + 1.
        CLEAR l_day.
        CONCATENATE 'DAY' l_index INTO l_day.
        ASSIGN COMPONENT l_day OF STRUCTURE <fs_alv> TO FIELD-SYMBOL(<fs_day>).
        IF sy-subrc = 0.
          READ TABLE gt_gas_receipt INTO wa_gas_receipt WITH KEY
            location_id = wa_state-empst
            gas_day     = l_date
            material    = wa_state-matnr.
          IF sy-subrc = 0.
            <fs_day> = ( wa_gas_receipt-qty_mbg * wa_state-percentage ) / 100.
            <fs_alv>-total_mbg = <fs_alv>-total_mbg + <fs_day>.
            l_day_sm3 = ( wa_gas_receipt-qty_scm * wa_state-percentage ) / 100.
            <fs_day> = l_day_sm3.
            <fs_alv>-total_scm = <fs_alv>-total_scm + l_day_sm3.
            l_gcv = ( l_day_sm3 * wa_gas_receipt-gcv ) + l_gcv.
            l_ncv = ( l_day_sm3 * wa_gas_receipt-ncv ) + l_ncv.
          ENDIF.
        ENDIF.
        l_date = l_date + 1.
      ENDDO.
      <fs_alv>-gcv = l_gcv / <fs_alv>-total_scm.
      <fs_alv>-ncv = l_ncv / <fs_alv>-total_scm.
      CLEAR l_day_sm3.
    ENDLOOP.
  ENDLOOP.
  " 2.1a/2.3e: Disable checkboxes after allocation
  DATA: lr_grid_alloc TYPE REF TO cl_gui_alv_grid,
        lt_fcat_alloc TYPE lvc_t_fcat.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid_alloc.
  IF lr_grid_alloc IS BOUND.
    lr_grid_alloc->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fcat_alloc ).
    LOOP AT lt_fcat_alloc ASSIGNING FIELD-SYMBOL(<fs_fcat_alloc>).
      IF <fs_fcat_alloc>-fieldname = 'EXCLUDE'.
        <fs_fcat_alloc>-edit = abap_false.
      ENDIF.
    ENDLOOP.
    lr_grid_alloc->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = lt_fcat_alloc ).
    SORT gt_alv_display BY location_id state_code material ASCENDING.
    lr_grid_alloc->refresh_table_display( ).
  ENDIF.
  " Set allocation flag and refresh PF-STATUS to show Validate/Edit/Send buttons
  gv_allocated = abap_true.
  PERFORM refresh_pf_status.
  " Point 7: Show popup after allocation is done
  CALL FUNCTION 'POPUP_TO_INFORM'
    EXPORTING
      titel = 'Allocation Complete'
      txt1  = 'Allocation carried out.'
      txt2  = 'Please validate the data.'
      txt3  = ''
      txt4  = ''.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_VALIDATE
*&---------------------------------------------------------------------*
FORM handle_validate.
  DATA: lv_diff_ok TYPE abap_bool,
        lv_answer  TYPE c LENGTH 1.
  DATA l_error TYPE flag.
  " Check for new receipt data from ONGC (timestamp comparison)
  SELECT * INTO TABLE @DATA(lt_cst)
    FROM yrga_cst_b2b_1
    FOR ALL ENTRIES IN @gt_cst_b2b_1
    WHERE gas_day = @gt_cst_b2b_1-gas_day
      AND ctp_id = @gt_cst_b2b_1-ctp_id
      AND ongc_material = @gt_cst_b2b_1-ongc_material.
  IF sy-subrc = 0.
    SORT lt_cst BY ctp_id gas_day ongc_material ASCENDING time_stamp DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_cst COMPARING ctp_id gas_day ongc_material.
    CLEAR gt_new_receipt_data.
    LOOP AT lt_cst INTO DATA(ls_cst).
      READ TABLE gt_cst_b2b_1 INTO DATA(ls_cst_g) WITH KEY
        gas_day = ls_cst-gas_day
        ctp_id = ls_cst-ctp_id
        ongc_material = ls_cst-ongc_material.
      IF sy-subrc = 0.
        IF ls_cst_g-time_stamp <> ls_cst-time_stamp.
          l_error = 'X'.
          MOVE-CORRESPONDING ls_cst TO gs_new_receipt_data.
          CONVERT TIME STAMP gs_new_receipt_data-time_stamp TIME ZONE 'UTC'
            INTO DATE gs_new_receipt_data-date TIME gs_new_receipt_data-time.
          APPEND gs_new_receipt_data TO gt_new_receipt_data.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF l_error = 'X'.
    " Point 5: Show popup with View Details option for new receipt data
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        textline1      = 'Validation unsuccessful as new receipt data has been received from ONGC.'
        textline2      = 'Please run allocation again. Click Yes to view new data details.'
        titel          = 'Validation Unsuccessful'
        cancel_display = ' '
      IMPORTING
        answer         = lv_answer.
    IF lv_answer = 'J'.  " User clicked Yes = View Details
      PERFORM display_new_receipt_data.
    ENDIF.
    EXIT.
  ENDIF.
  PERFORM build_validation_data.
  DELETE gt_validation WHERE ctp_id IS INITIAL.
  " Check if any DIFF_PUR_SUP_MBG > 0.009
  lv_diff_ok = abap_true.
  LOOP AT gt_validation INTO gs_validation.
    IF abs( gs_validation-diff_pur_sup_mbg ) > '0.009'.
      lv_diff_ok = abap_false.
      EXIT.
    ENDIF.
  ENDLOOP.
  " Set flags
  gv_validated = abap_true.
  gv_save_enabled = lv_diff_ok.
  " Display validation ALV popup
  PERFORM display_validation_alv.
  " Show popup message based on save status using POPUP_TO_INFORM
  IF gv_save_enabled = abap_false.
    " Validation failed: keep screen edit state as-is (no change to field editability)
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Validation FAILED'
        txt1  = 'Difference > 0.009 MBG found in validation data.'
        txt2  = 'Please adjust the day values and re-validate.'
        txt3  = 'Save button will remain DISABLED.'
        txt4  = ''.
  ELSE.
    " Validation passed: disable all fields on screen
    PERFORM disable_all_fields.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Validation PASSED'
        txt1  = 'All differences are within acceptable limit (<= 0.009 MBG).'
        txt2  = 'Save button is now ENABLED.'
        txt3  = 'You can proceed to save the data.'
        txt4  = ''.
  ENDIF.
  " Refresh the PF-STATUS to update Save button visibility
  PERFORM refresh_pf_status.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_PF_STATUS
*&---------------------------------------------------------------------*
FORM refresh_pf_status.
  DATA: lt_extab TYPE slis_t_extab,
        ls_extab TYPE slis_extab.
  " Point 6: In view mode, show Reallocate instead of Allocate
  IF p_view IS NOT INITIAL.
    CLEAR ls_extab.
    ls_extab-fcode = 'ALLOCATION'.
    APPEND ls_extab TO lt_extab.
  ELSE.
    CLEAR ls_extab.
    ls_extab-fcode = 'REALLOCATE'.
    APPEND ls_extab TO lt_extab.
  ENDIF.
  " Before allocation: only show ALLOCATION/REALLOCATE and RESET
  IF gv_allocated = abap_false.
    CLEAR ls_extab.
    ls_extab-fcode = 'VALIDATE'.
    APPEND ls_extab TO lt_extab.
    CLEAR ls_extab.
    ls_extab-fcode = 'EDIT'.
    APPEND ls_extab TO lt_extab.
    CLEAR ls_extab.
    ls_extab-fcode = 'SAVE'.
    APPEND ls_extab TO lt_extab.
  ELSE.
    " After allocation: exclude SAVE if validation not done or not enabled or data already saved
    IF gv_validated = abap_false OR gv_save_enabled = abap_false OR gv_data_saved = abap_true.
      CLEAR ls_extab.
      ls_extab-fcode = 'SAVE'.
      APPEND ls_extab TO lt_extab.
    ENDIF.
  ENDIF.
  " SEND only visible after data is saved
  IF gv_data_saved = abap_false.
    CLEAR ls_extab.
    ls_extab-fcode = 'SEND'.
    APPEND ls_extab TO lt_extab.
  ENDIF.
  " After data saved: disable EDIT, ALLOCATION/REALLOCATE, RESET
  IF gv_data_saved = abap_true.
    CLEAR ls_extab.
    ls_extab-fcode = 'EDIT'.
    APPEND ls_extab TO lt_extab.
    CLEAR ls_extab.
    ls_extab-fcode = 'ALLOCATION'.
    APPEND ls_extab TO lt_extab.
    CLEAR ls_extab.
    ls_extab-fcode = 'REALLOCATE'.
    APPEND ls_extab TO lt_extab.
    CLEAR ls_extab.
    ls_extab-fcode = 'RESET'.
    APPEND ls_extab TO lt_extab.
  ENDIF.
  SET PF-STATUS 'ZALV_STATUS' EXCLUDING lt_extab.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_VALIDATION_DATA
*&---------------------------------------------------------------------*
FORM build_validation_data.
  DATA: ls_validation TYPE ty_validation.
  CLEAR gt_validation.
  DATA: BEGIN OF ls_key,
          location_id TYPE ygms_de_loc_id,
          material    TYPE ygms_de_gail_mat,
        END OF ls_key,
        lt_keys LIKE TABLE OF ls_key.
  LOOP AT gt_alv_display INTO gs_alv_display WHERE exclude IS INITIAL.
    ls_key-location_id = gs_alv_display-location_id.
    ls_key-material    = gs_alv_display-material.
    COLLECT ls_key INTO lt_keys.
  ENDLOOP.
  LOOP AT lt_keys INTO ls_key.
    CLEAR ls_validation.
    ls_validation-location_id = ls_key-location_id.
    ls_validation-material    = ls_key-material.
    LOOP AT gt_alv_display INTO gs_alv_display
      WHERE location_id = ls_key-location_id
        AND material    = ls_key-material
        AND exclude     IS INITIAL.
      ls_validation-allocated_scm = ls_validation-allocated_scm + gs_alv_display-total_scm.
      ls_validation-allocated_mbg = ls_validation-allocated_mbg + gs_alv_display-total_mbg.
    ENDLOOP.
    READ TABLE gt_gas_receipt INTO DATA(ls_receipt)
      WITH KEY location_id = ls_key-location_id
               material    = ls_key-material.
    IF sy-subrc = 0.
      ls_validation-ctp_id        = ls_receipt-ctp_id.
      ls_validation-ongc_material = ls_receipt-ongc_material.
    ENDIF.
    LOOP AT gt_gas_receipt INTO ls_receipt
      WHERE location_id = ls_key-location_id
        AND material    = ls_key-material.
      ls_validation-supply_scm = ls_validation-supply_scm + ls_receipt-qty_scm.
      ls_validation-supply_mbg = ls_validation-supply_mbg + ls_receipt-qty_mbg.
    ENDLOOP.
    ls_validation-diff_pur_sup_scm = ls_validation-allocated_scm - ls_validation-supply_scm.
    ls_validation-diff_pur_sup_mbg = ls_validation-allocated_mbg - ls_validation-supply_mbg.
    APPEND ls_validation TO gt_validation.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_VALIDATION_ALV
*&---------------------------------------------------------------------*
FORM display_validation_alv.
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat TYPE slis_fieldcat_alv,
        ls_layout   TYPE slis_layout_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'LOCATION_ID'.
  ls_fieldcat-seltext_l = 'Location ID'.
  ls_fieldcat-col_pos   = 1.
  ls_fieldcat-outputlen = 15.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MATERIAL'.
  ls_fieldcat-seltext_l = 'GAIL Material'.
  ls_fieldcat-col_pos   = 2.
  ls_fieldcat-outputlen = 30.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ALLOCATED_SCM'.
  ls_fieldcat-seltext_l = 'Allocated Sm³'.
  ls_fieldcat-col_pos   = 3.
  ls_fieldcat-do_sum    = abap_true.
  ls_fieldcat-decimals_out = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ALLOCATED_MBG'.
  ls_fieldcat-seltext_l = 'Allocated MBG'.
  ls_fieldcat-col_pos   = 4.
  ls_fieldcat-do_sum    = abap_true.
  ls_fieldcat-decimals_out = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'CTP_ID'.
  ls_fieldcat-seltext_l = 'CTP ID'.
  ls_fieldcat-col_pos   = 5.
  ls_fieldcat-outputlen = 15.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ONGC_MATERIAL'.
  ls_fieldcat-seltext_l = 'ONGC Material'.
  ls_fieldcat-col_pos   = 6.
  ls_fieldcat-outputlen = 25.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'SUPPLY_SCM'.
  ls_fieldcat-seltext_l = 'Receipt Sm³'.
  ls_fieldcat-col_pos   = 7.
  ls_fieldcat-do_sum    = abap_true.
  ls_fieldcat-decimals_out = 3.
  ls_fieldcat-outputlen = 25.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'SUPPLY_MBG'.
  ls_fieldcat-seltext_l = 'Receipt MBG'.
  ls_fieldcat-col_pos   = 8.
  ls_fieldcat-do_sum    = abap_true.
  ls_fieldcat-decimals_out = 3.
  ls_fieldcat-outputlen = 25.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'DIFF_PUR_SUP_SCM'.
  ls_fieldcat-seltext_l = 'Diff. Alloc. Vs Rec. Sm³'.
  ls_fieldcat-col_pos   = 9.
  ls_fieldcat-do_sum    = abap_true.
  ls_fieldcat-decimals_out = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'DIFF_PUR_SUP_MBG'.
  ls_fieldcat-seltext_l = 'Diff. Alloc. Vs Rec. MBG'.
  ls_fieldcat-col_pos   = 10.
  ls_fieldcat-do_sum    = abap_true.
  ls_fieldcat-decimals_out = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  ls_layout-colwidth_optimize = abap_true.
  ls_layout-zebra             = abap_true.
  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title               = 'Validation Results'
      i_selection           = ' '
      i_zebra               = abap_true
      i_screen_start_column = 5
      i_screen_start_line   = 5
      i_screen_end_column   = 150
      i_screen_end_line     = 25
      i_tabname             = 'GT_VALIDATION'
      it_fieldcat           = lt_fieldcat
    TABLES
      t_outtab              = gt_validation
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_EDIT
*&---------------------------------------------------------------------*
FORM handle_edit.
  DATA: lr_grid         TYPE REF TO cl_gui_alv_grid,
        lt_fcat         TYPE lvc_t_fcat,
        ls_fcat         TYPE lvc_s_fcat,
        lv_day_edit     TYPE abap_bool,
        lv_new_day_edit TYPE abap_bool,
        ls_style        TYPE lvc_s_styl.
  " Get ALV grid reference
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.
  " Get current field catalog
  lr_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fcat ).
  " Check current edit state of DAY01 field
  READ TABLE lt_fcat INTO ls_fcat WITH KEY fieldname = 'DAY01'.
  IF sy-subrc = 0.
    lv_day_edit = ls_fcat-edit.
  ENDIF.
  " Toggle only DAY columns (2.3c: Checkboxes are NOT toggled by Edit button)
  IF lv_day_edit = abap_true.
    lv_new_day_edit = abap_false.
  ELSE.
    lv_new_day_edit = abap_true.
  ENDIF.
  " Apply toggle to DAY field catalog only
  LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    IF <fs_fcat>-fieldname CP 'DAY*'.
      <fs_fcat>-edit = lv_new_day_edit.
    ENDIF.
  ENDLOOP.
  " Set updated field catalog
  lr_grid->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = lt_fcat ).
  " Set cell styles: disable DAY columns for excluded rows
  LOOP AT gt_alv_display ASSIGNING FIELD-SYMBOL(<fs_edit_row>).
    CLEAR <fs_edit_row>-celltab.
    IF lv_new_day_edit = abap_true AND <fs_edit_row>-exclude = 'X'.
      " Disable all DAY columns for excluded rows
      DATA lv_edit_days TYPE i.
      lv_edit_days = gv_date_to - gv_date_from + 1.
      DO lv_edit_days TIMES.
        CLEAR ls_style.
        CASE sy-index.
          WHEN 1.  ls_style-fieldname = 'DAY01'.
          WHEN 2.  ls_style-fieldname = 'DAY02'.
          WHEN 3.  ls_style-fieldname = 'DAY03'.
          WHEN 4.  ls_style-fieldname = 'DAY04'.
          WHEN 5.  ls_style-fieldname = 'DAY05'.
          WHEN 6.  ls_style-fieldname = 'DAY06'.
          WHEN 7.  ls_style-fieldname = 'DAY07'.
          WHEN 8.  ls_style-fieldname = 'DAY08'.
          WHEN 9.  ls_style-fieldname = 'DAY09'.
          WHEN 10. ls_style-fieldname = 'DAY10'.
          WHEN 11. ls_style-fieldname = 'DAY11'.
          WHEN 12. ls_style-fieldname = 'DAY12'.
          WHEN 13. ls_style-fieldname = 'DAY13'.
          WHEN 14. ls_style-fieldname = 'DAY14'.
          WHEN 15. ls_style-fieldname = 'DAY15'.
          WHEN 16. ls_style-fieldname = 'DAY16'.
        ENDCASE.
        ls_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT ls_style INTO TABLE <fs_edit_row>-celltab.
      ENDDO.
    ENDIF.
  ENDLOOP.
  " Refresh the ALV
  lr_grid->refresh_table_display( ).
  " 2.3a: Revoke validation status and hide Save button when entering edit mode
  IF lv_new_day_edit = abap_true.
    gv_validated = abap_false.
    gv_save_enabled = abap_false.
    PERFORM refresh_pf_status.
    MESSAGE s000(ygms_msg) WITH 'Edit mode enabled - Day columns are now editable'.
  ELSE.
    MESSAGE s000(ygms_msg) WITH 'Edit mode disabled - Day columns are now read-only'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_SAVE
*&---------------------------------------------------------------------*
FORM handle_save.
  DATA: lv_answer TYPE c LENGTH 1.
  " Confirm with user before saving using POPUP_TO_CONFIRM_STEP
  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      textline1      = 'Do you want to save the data to database?'
      titel          = 'Confirm Save'
      cancel_display = ' '
    IMPORTING
      answer         = lv_answer.
  IF lv_answer = 'J'.  " User clicked Yes (J = Ja/Yes)
    " Save data to database
    PERFORM save_data_to_db.
    " Set data saved flag
    gv_data_saved = abap_true.
    " Disable all fields in ALV
    PERFORM disable_all_fields.
    " Refresh PF-STATUS to hide Save, Edit, Allocation, Reset buttons
    PERFORM refresh_pf_status.
    " Show success message
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Save Successful'
        txt1  = 'Data has been saved successfully to the database.'
        txt2  = 'All fields are now in display mode.'
        txt3  = ''
        txt4  = ''.
  ELSE.
    " User clicked No - just return without saving
    MESSAGE s000(ygms_msg) WITH 'Save operation cancelled by user'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SAVE_DATA_TO_DB
*&---------------------------------------------------------------------*
FORM save_data_to_db.
  " Type for GAIL ID mapping
  TYPES: BEGIN OF ty_gail_id_map,
           location_id TYPE ygms_de_loc_id,
           material    TYPE ygms_de_gail_mat,
           state_code  TYPE regio,
           gail_id     TYPE c LENGTH 14,
         END OF ty_gail_id_map.
  " Type for error log
  TYPES: BEGIN OF ty_error_log,
           date_from   TYPE datum,
           date_to     TYPE datum,
           location_id TYPE ygms_de_loc_id,
           material    TYPE ygms_de_gail_mat,
           state_code  TYPE regio,
           gail_ids    TYPE string,
         END OF ty_error_log.
  DATA: lt_cst_pur     TYPE TABLE OF yrga_cst_pur,
        ls_cst_pur     TYPE yrga_cst_pur,
        lt_cst_fnt     TYPE TABLE OF yrga_cst_fn_data,
        ls_cst_fnt     TYPE yrga_cst_fn_data,
        lv_timestamp   TYPE timestampl,
        lv_ts_char     TYPE c LENGTH 14,
        lv_date        TYPE datum,
        lv_day_index   TYPE i,
        lv_day_qty     TYPE p DECIMALS 3,
        lv_counter     TYPE i,
        lv_fnt_counter TYPE i,
        lt_gail_id_map TYPE TABLE OF ty_gail_id_map,
        ls_gail_id_map TYPE ty_gail_id_map,
        lv_gail_prefix TYPE c LENGTH 8,
        lv_fortnight   TYPE c LENGTH 2,
        lv_seq_number  TYPE n LENGTH 6,
        lv_return_code TYPE inri-returncode,
        lt_error_log   TYPE TABLE OF ty_error_log,
        ls_error_log   TYPE ty_error_log,
        lv_error_found TYPE abap_bool.
  " Variables for weighted average calculation
  DATA: lv_total_vol   TYPE YGMS_DE_QTY_MBG_CAL,"p DECIMALS 3,
        lv_sum_vol_gcv TYPE YGMS_DE_QTY_MBG_CAL,"p DECIMALS 6,
        lv_sum_vol_ncv TYPE YGMS_DE_QTY_MBG_CAL,"p DECIMALS 6,
        lv_avg_gcv     TYPE ygms_de_gcv,
        lv_avg_ncv     TYPE ygms_de_ncv,
        lv_total_mbg   TYPE YGMS_DE_QTY_MBG_CAL,"p DECIMALS 3,
        lv_total_scm   TYPE YGMS_DE_QTY_MBG_CAL."p DECIMALS 3.
  " Variables for GAIL ID validation
  DATA: lt_gail_ids    TYPE TABLE OF yrga_cst_pur-gail_id,
        lv_gail_count  TYPE i,
        lv_gail_id_str TYPE string.
  " Get current timestamp in UTC - format as YYYYMMDDHHmmSS (14 chars)
  DATA: lv_ts_date TYPE sy-datum,
        lv_ts_time TYPE sy-uzeit.
  GET TIME STAMP FIELD lv_timestamp.
  CONVERT TIME STAMP lv_timestamp TIME ZONE 'UTC'
    INTO DATE lv_ts_date TIME lv_ts_time.
  CONCATENATE lv_ts_date lv_ts_time INTO lv_ts_char.
  " Generate GAIL_ID prefix: GA + YYMM + F1/F2
  DATA(lv_day) = gv_date_from+6(2).
  IF lv_day <= 15.
    lv_fortnight = 'F1'.
  ELSE.
    lv_fortnight = 'F2'.
  ENDIF.
  " Build prefix: GA + YY + MM + F1/F2 (e.g., GA2510F1)
  CONCATENATE 'GA' gv_date_from+2(2) gv_date_from+4(2) lv_fortnight INTO lv_gail_prefix.
  " Initialize error flag
  lv_error_found = abap_false.
  " First pass: Generate unique GAIL_IDs for each Location-Material-State combination
  LOOP AT gt_alv_display INTO gs_alv_display WHERE exclude IS INITIAL.
    READ TABLE lt_gail_id_map INTO ls_gail_id_map
      WITH KEY location_id = gs_alv_display-location_id
               material    = gs_alv_display-material
               state_code  = gs_alv_display-state_code.
    IF sy-subrc <> 0.
      " Get next number from number range
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'ZCST'
        IMPORTING
          number      = lv_seq_number
          returncode  = lv_return_code.
      IF lv_return_code IS INITIAL OR lv_return_code = '1'.
        CLEAR ls_gail_id_map.
        ls_gail_id_map-location_id = gs_alv_display-location_id.
        ls_gail_id_map-material    = gs_alv_display-material.
        ls_gail_id_map-state_code  = gs_alv_display-state_code.
        CONCATENATE lv_gail_prefix lv_seq_number INTO ls_gail_id_map-gail_id.
        APPEND ls_gail_id_map TO lt_gail_id_map.
      ENDIF.
    ENDIF.
  ENDLOOP.
  " Second pass: Create daily records for YRGA_CST_PUR
  LOOP AT gt_alv_display INTO gs_alv_display ."WHERE exclude IS INITIAL.
    READ TABLE lt_gail_id_map INTO ls_gail_id_map
      WITH KEY location_id = gs_alv_display-location_id
               material    = gs_alv_display-material
               state_code  = gs_alv_display-state_code.
    lv_date = gv_date_from.
    DATA lv_save_days TYPE i.
    lv_save_days = gv_date_to - gv_date_from + 1.
    DO lv_save_days TIMES.
      lv_day_index = sy-index.
      CASE lv_day_index.
        WHEN 1.  lv_day_qty = gs_alv_display-day01.
        WHEN 2.  lv_day_qty = gs_alv_display-day02.
        WHEN 3.  lv_day_qty = gs_alv_display-day03.
        WHEN 4.  lv_day_qty = gs_alv_display-day04.
        WHEN 5.  lv_day_qty = gs_alv_display-day05.
        WHEN 6.  lv_day_qty = gs_alv_display-day06.
        WHEN 7.  lv_day_qty = gs_alv_display-day07.
        WHEN 8.  lv_day_qty = gs_alv_display-day08.
        WHEN 9.  lv_day_qty = gs_alv_display-day09.
        WHEN 10. lv_day_qty = gs_alv_display-day10.
        WHEN 11. lv_day_qty = gs_alv_display-day11.
        WHEN 12. lv_day_qty = gs_alv_display-day12.
        WHEN 13. lv_day_qty = gs_alv_display-day13.
        WHEN 14. lv_day_qty = gs_alv_display-day14.
        WHEN 15. lv_day_qty = gs_alv_display-day15.
        WHEN 16. lv_day_qty = gs_alv_display-day16.
      ENDCASE.
      CLEAR ls_cst_pur.
      ls_cst_pur-gas_day      = lv_date.
      ls_cst_pur-location     = gs_alv_display-location_id.
      ls_cst_pur-material     = gs_alv_display-material.
      ls_cst_pur-state_code   = gs_alv_display-state_code.
      ls_cst_pur-state        = gs_alv_display-state.
      " Get CTP, ONGC material, and GCV/NCV from gas receipt (supply table)
      " 2.4a: GCV/NCV values from supply table per Gas Day + Location ID
      READ TABLE gt_gas_receipt INTO DATA(ls_receipt)
        WITH KEY location_id = gs_alv_display-location_id
                 material    = gs_alv_display-material
                 gas_day     = lv_date.
      IF sy-subrc = 0.
        ls_cst_pur-ctp         = ls_receipt-ctp_id.
        ls_cst_pur-ongc_mater  = ls_receipt-ongc_material.
        ls_cst_pur-ongc_id     = ls_receipt-ongc_id.
        ls_cst_pur-gcv         = ls_receipt-gcv.
        ls_cst_pur-ncv         = ls_receipt-ncv.
      ELSE.
        READ TABLE gt_gas_receipt INTO ls_receipt
          WITH KEY location_id = gs_alv_display-location_id
                   material    = gs_alv_display-material.
        IF sy-subrc = 0.
          ls_cst_pur-ctp         = ls_receipt-ctp_id.
          ls_cst_pur-ongc_mater  = ls_receipt-ongc_material.
          ls_cst_pur-ongc_id     = ls_receipt-ongc_id.
*          ls_cst_pur-gcv         = ls_receipt-gcv.
*          ls_cst_pur-ncv         = ls_receipt-ncv.
        ENDIF.
      ENDIF.
      ls_cst_pur-time_stamp   = lv_ts_char.
      ls_cst_pur-qty_in_scm   = lv_day_qty.
      " Calculate MBG for this day's quantity using supply GCV/NCV
      DATA: c_tgqty TYPE msego2-adqnt,
            i_trqty TYPE msego2-adqnt,
            lv_gcv  TYPE oib_par_fltp,
            lv_ncv  TYPE oib_par_fltp.
      IF ls_cst_pur-gcv > 0 AND lv_day_qty > 0.
        CLEAR c_tgqty.
        i_trqty = lv_day_qty.
        lv_gcv  = ls_cst_pur-gcv.
        lv_ncv  = ls_cst_pur-ncv.
        CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
          EXPORTING
            i_trqty = i_trqty
            i_truom = 'SM3'
            i_tguom = 'MBG'
            lv_gcv  = lv_gcv
            lv_ncv  = lv_ncv
          CHANGING
            c_tgqty = c_tgqty.
        ls_cst_pur-qty_in_mbg = c_tgqty.
      ENDIF.
      ls_cst_pur-gail_id      = ls_gail_id_map-gail_id.
      ls_cst_pur-exclude      = gs_alv_display-exclude.
      ls_cst_pur-created_by   = sy-uname.
      ls_cst_pur-created_date = sy-datum.
      ls_cst_pur-created_time = sy-uzeit.
      IF ls_cst_pur-ctp IS NOT INITIAL.
        APPEND ls_cst_pur TO lt_cst_pur.
      ENDIF.
      lv_date = lv_date + 1.
    ENDDO.
  ENDLOOP.
  " Third pass: Create fortnightly aggregated records for YRGA_CST_FNT_DATA
  " Calculate weighted average GCV/NCV: Σ(Volume × GCV/NCV) / Total Volume
  LOOP AT lt_gail_id_map INTO ls_gail_id_map.
    CLEAR: lv_total_vol, lv_sum_vol_gcv, lv_sum_vol_ncv, lv_total_mbg, lv_total_scm.
    CLEAR ls_cst_fnt.
    " Sum quantities and calculate weighted averages for this Location-Material-State
    LOOP AT lt_cst_pur INTO ls_cst_pur
      WHERE location     = ls_gail_id_map-location_id
        AND material     = ls_gail_id_map-material
        AND state_code   = ls_gail_id_map-state_code.
      " Accumulate totals
      lv_total_mbg = lv_total_mbg + ls_cst_pur-qty_in_mbg.
      lv_total_scm = lv_total_scm + ls_cst_pur-qty_in_scm.
      " For weighted average: Σ(Volume × GCV) and Σ(Volume × NCV)
      lv_sum_vol_gcv = lv_sum_vol_gcv + ( ls_cst_pur-qty_in_scm * ls_cst_pur-gcv ).
      lv_sum_vol_ncv = lv_sum_vol_ncv + ( ls_cst_pur-qty_in_scm * ls_cst_pur-ncv ).
      lv_total_vol   = lv_total_vol + ls_cst_pur-qty_in_scm.
      " Get CTP and ONGC material (from first record with values)
      IF ls_cst_fnt-ctp IS INITIAL AND ls_cst_pur-ctp IS NOT INITIAL.
        ls_cst_fnt-ctp        = ls_cst_pur-ctp.
        ls_cst_fnt-ongc_mater = ls_cst_pur-ongc_mater.
        ls_cst_fnt-state      = ls_cst_pur-state.
      ENDIF.
    ENDLOOP.
    " Fallback: if CTP still not set, get from gas receipt directly
    IF ls_cst_fnt-ctp IS INITIAL.
      READ TABLE gt_gas_receipt INTO DATA(ls_fnt_receipt)
        WITH KEY location_id = ls_gail_id_map-location_id
                 material    = ls_gail_id_map-material.
      IF sy-subrc = 0.
        ls_cst_fnt-ctp        = ls_fnt_receipt-ctp_id.
        ls_cst_fnt-ongc_mater = ls_fnt_receipt-ongc_material.
      ENDIF.
    ENDIF.
    " Get state description from ALV display if not yet set
    IF ls_cst_fnt-state IS INITIAL.
      READ TABLE gt_alv_display INTO gs_alv_display
        WITH KEY location_id = ls_gail_id_map-location_id
                 material    = ls_gail_id_map-material
                 state_code  = ls_gail_id_map-state_code.
      IF sy-subrc = 0.
        ls_cst_fnt-state = gs_alv_display-state.
      ENDIF.
    ENDIF.
    " Calculate weighted average GCV/NCV
    IF lv_total_vol > 0.
      lv_avg_gcv = lv_sum_vol_gcv / lv_total_vol.
      lv_avg_ncv = lv_sum_vol_ncv / lv_total_vol.
    ENDIF.
    " Populate fortnightly record
    ls_cst_fnt-date_from    = gv_date_from.
    ls_cst_fnt-date_to      = gv_date_to.
    ls_cst_fnt-location     = ls_gail_id_map-location_id.
    ls_cst_fnt-material     = ls_gail_id_map-material.
    ls_cst_fnt-state_code   = ls_gail_id_map-state_code.
    ls_cst_fnt-time_stamp   = lv_ts_char.
    ls_cst_fnt-qty_in_mbg   = lv_total_mbg.
    ls_cst_fnt-gcv          = lv_avg_gcv.
    ls_cst_fnt-ncv          = lv_avg_ncv.
    ls_cst_fnt-qty_in_scm   = lv_total_scm.
    ls_cst_fnt-gail_id      = ls_gail_id_map-gail_id.
    ls_cst_fnt-created_by   = sy-uname.
    ls_cst_fnt-created_date = sy-datum.
    ls_cst_fnt-created_time = sy-uzeit.
    IF ls_cst_fnt-ctp IS NOT INITIAL.
      APPEND ls_cst_fnt TO lt_cst_fnt.
    ENDIF.
  ENDLOOP.
  " Check for duplicate GAIL IDs (validation step c)
  " For each Location-Material-State, check if GAIL ID already exists in YRGA_CST_PUR
  LOOP AT lt_gail_id_map INTO ls_gail_id_map.
    CLEAR: lt_gail_ids, lv_gail_count, lv_gail_id_str.
    " Check if GAIL IDs already exist for this combination in the date range
    SELECT DISTINCT gail_id FROM yrga_cst_pur
      INTO TABLE lt_gail_ids
      WHERE location    = ls_gail_id_map-location_id
        AND material    = ls_gail_id_map-material
        AND state_code  = ls_gail_id_map-state_code
        AND gas_day    BETWEEN gv_date_from AND gv_date_to
      AND deleted = ' '.
    lv_gail_count = lines( lt_gail_ids ).
    IF lv_gail_count > 1.
      " Multiple GAIL IDs found - log error
      lv_error_found = abap_true.
      CLEAR ls_error_log.
      ls_error_log-date_from   = gv_date_from.
      ls_error_log-date_to     = gv_date_to.
      ls_error_log-location_id = ls_gail_id_map-location_id.
      ls_error_log-material    = ls_gail_id_map-material.
      ls_error_log-state_code  = ls_gail_id_map-state_code.
      " Build GAIL IDs string for error message
      LOOP AT lt_gail_ids INTO DATA(lv_gail_id).
        IF lv_gail_id_str IS INITIAL.
          lv_gail_id_str = lv_gail_id.
        ELSE.
          CONCATENATE lv_gail_id_str ',' lv_gail_id INTO lv_gail_id_str SEPARATED BY space.
        ENDIF.
      ENDLOOP.
      ls_error_log-gail_ids = lv_gail_id_str.
      APPEND ls_error_log TO lt_error_log.
    ENDIF.
  ENDLOOP.
  " If errors found, display error log and exit without saving
  IF lv_error_found = abap_true.
    PERFORM display_gail_id_error_log USING lt_error_log.
    MESSAGE e000(ygms_msg) WITH 'Multiple GAIL IDs found. Data NOT saved.'.
    RETURN.
  ENDIF.
  " Delete existing data for same Location ID and Fortnight (step f)
  UPDATE yrga_cst_pur SET deleted = 'X'
  deleted_by = sy-uname
deleted_on = sy-datum
delete_at = sy-uzeit
deleted_reson = '1'
*  DELETE FROM yrga_cst_pur
    WHERE gas_day BETWEEN gv_date_from AND gv_date_to and deleted = ' '.
  UPDATE yrga_cst_fn_data SET deleted = 'X'
  deleted_by = sy-uname
deleted_on = sy-datum
delete_at = sy-uzeit
deleted_reson = '1'
*  DELETE FROM yrga_cst_fn_data
    WHERE date_from = gv_date_from
      AND date_to   = gv_date_to and deleted = ' '.
  " Save records to both database tables
  IF lt_cst_pur IS NOT INITIAL.
    MODIFY yrga_cst_pur FROM TABLE lt_cst_pur.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      MESSAGE e000(ygms_msg) WITH 'Error saving data to YRGA_CST_PUR'.
      RETURN.
    ENDIF.
  ENDIF.
  IF lt_cst_fnt IS NOT INITIAL.
    MODIFY yrga_cst_fn_data FROM TABLE lt_cst_fnt.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      MESSAGE e000(ygms_msg) WITH 'Error saving data to YRGA_CST_FNT_DATA'.
      RETURN.
    ENDIF.
  ENDIF.
  " Commit if both saves successful
  COMMIT WORK AND WAIT.
  lv_counter = lines( lt_cst_pur ).
  lv_fnt_counter = lines( lt_cst_fnt ).
  MESSAGE s000(ygms_msg) WITH lv_counter 'daily,' lv_fnt_counter 'fortnightly records saved'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_GAIL_ID_ERROR_LOG
*&---------------------------------------------------------------------*
FORM display_gail_id_error_log USING pt_error_log TYPE STANDARD TABLE.
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'DATE_FROM'.
  ls_fieldcat-seltext_l = 'From Date'.
  ls_fieldcat-col_pos   = 1.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'DATE_TO'.
  ls_fieldcat-seltext_l = 'To Date'.
  ls_fieldcat-col_pos   = 2.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'LOCATION_ID'.
  ls_fieldcat-seltext_l = 'Location ID'.
  ls_fieldcat-col_pos   = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MATERIAL'.
  ls_fieldcat-seltext_l = 'Material'.
  ls_fieldcat-col_pos   = 4.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'STATE_CODE'.
  ls_fieldcat-seltext_l = 'State Code'.
  ls_fieldcat-col_pos   = 5.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GAIL_IDS'.
  ls_fieldcat-seltext_l = 'GAIL IDs Found'.
  ls_fieldcat-col_pos   = 6.
  ls_fieldcat-outputlen = 50.
  APPEND ls_fieldcat TO lt_fieldcat.
  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title               = 'Error: Multiple GAIL IDs Found'
      i_selection           = ' '
      i_zebra               = abap_true
      i_screen_start_column = 5
      i_screen_start_line   = 5
      i_screen_end_column   = 130
      i_screen_end_line     = 20
      i_tabname             = 'PT_ERROR_LOG'
      it_fieldcat           = lt_fieldcat
    TABLES
      t_outtab              = pt_error_log
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISABLE_ALL_FIELDS
*&---------------------------------------------------------------------*
FORM disable_all_fields.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid,
        lt_fcat TYPE lvc_t_fcat.
  " Get ALV grid reference
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.
  IF lr_grid IS NOT BOUND.
    RETURN.
  ENDIF.
  " Get current field catalog
  lr_grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fcat ).
  " Disable editing for all fields
  LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
    <fs_fcat>-edit = abap_false.
  ENDLOOP.
  " Set updated field catalog
  lr_grid->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = lt_fcat ).
  " Refresh the ALV
  lr_grid->refresh_table_display( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_RESET
*&---------------------------------------------------------------------*
FORM handle_reset.
  CLEAR gt_alv_display.
  PERFORM build_alv_display_table.
  " 2.3d: Enable checkboxes whenever reset button is clicked
  DATA: lr_grid_reset TYPE REF TO cl_gui_alv_grid,
        lt_fcat_reset TYPE lvc_t_fcat.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid_reset.
  IF lr_grid_reset IS BOUND.
    lr_grid_reset->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fcat_reset ).
    LOOP AT lt_fcat_reset ASSIGNING FIELD-SYMBOL(<fs_fcat_reset>).
      IF <fs_fcat_reset>-fieldname = 'EXCLUDE'.
        <fs_fcat_reset>-edit = abap_true.
      ENDIF.
      " Also disable DAY column editing on reset
      IF <fs_fcat_reset>-fieldname CP 'DAY*'.
        <fs_fcat_reset>-edit = abap_false.
      ENDIF.
    ENDLOOP.
    lr_grid_reset->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = lt_fcat_reset ).
    lr_grid_reset->refresh_table_display( ).
  ENDIF.
  " Reset allocation and validation status
  gv_allocated = abap_false.
  gv_validated = abap_false.
  gv_save_enabled = abap_false.
  PERFORM refresh_pf_status.
  MESSAGE s000(ygms_msg) WITH 'Data reset to original values'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_SEND_DIRECT
*& Direct send from selection screen - same flow as ALV Send button
*&---------------------------------------------------------------------*
FORM handle_send_direct.
  DATA: lv_valid TYPE abap_bool.
  " Validate before send (check for new ONGC receipt data)
  PERFORM validate_before_send CHANGING lv_valid.
  IF lv_valid = abap_false.
    RETURN.
  ENDIF.
  " Show data preview with daily/fortnightly toggle, then send mode popup
  PERFORM display_send_preview.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_DOWNLOAD
*& Download saved data as Excel or PDF to local computer
*&---------------------------------------------------------------------*
FORM handle_download.
  DATA: lt_send_data TYPE TABLE OF yrga_cst_pur,
        lt_fnt_data  TYPE TABLE OF yrga_cst_fn_data.
  DATA: lv_date_from_str TYPE c LENGTH 10,
        lv_date_to_str   TYPE c LENGTH 10.
  DATA: lv_filename    TYPE string,
        lv_fullpath    TYPE string,
        lv_path        TYPE string,
        lv_user_action TYPE i.

  WRITE gv_date_from TO lv_date_from_str DD/MM/YYYY.
  WRITE gv_date_to   TO lv_date_to_str   DD/MM/YYYY.

  " Fetch daily data from YRGA_CST_PUR where EXCLUDED flag is not X
  SELECT * FROM yrga_cst_pur
    INTO TABLE lt_send_data
    WHERE gas_day BETWEEN gv_date_from AND gv_date_to
      AND location IN s_loc
      AND exclude <> 'X' AND deleted = ' '.

  IF lt_send_data IS INITIAL.
    MESSAGE s000(ygms_msg) WITH 'No saved data found for the selected period' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  " Fetch fortnightly data from YRGA_CST_FN_DATA
  SELECT * FROM yrga_cst_fn_data
    INTO TABLE lt_fnt_data
    WHERE date_from = gv_date_from
      AND date_to   = gv_date_to
      AND location  IN s_loc AND deleted = ' '.

  IF p_dxls IS NOT INITIAL.
    " --- Download as Excel ---
    " Build daily Excel content
    DATA: lt_daily_xls    TYPE solix_tab,
          lv_daily_sz_raw TYPE sood-objlen,
          lv_daily_sz     TYPE i.
    PERFORM build_excel_attachment USING lt_send_data
                                  CHANGING lt_daily_xls lv_daily_sz_raw.
    lv_daily_sz = lv_daily_sz_raw.

    " Prompt user for daily file save location
    DATA lv_def_daily TYPE string.
    CONCATENATE 'Daily_CST_Purchase_' lv_date_from_str '_' lv_date_to_str '.xls' INTO lv_def_daily.
    REPLACE ALL OCCURRENCES OF '/' IN lv_def_daily WITH '-'.
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        default_file_name = lv_def_daily
        default_extension = 'xls'
        file_filter       = 'Excel Files (*.xls)|*.xls|All Files (*.*)|*.*'
      CHANGING
        filename          = lv_filename
        path              = lv_path
        fullpath          = lv_fullpath
        user_action       = lv_user_action.

    IF lv_user_action = cl_gui_frontend_services=>action_ok.
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          bin_filesize = lv_daily_sz
          filename     = lv_fullpath
          filetype     = 'BIN'
        CHANGING
          data_tab     = lt_daily_xls.
      MESSAGE s000(ygms_msg) WITH 'Daily Excel downloaded successfully'.
    ENDIF.

    " Build fortnightly Excel content
    IF lt_fnt_data IS NOT INITIAL.
      DATA: lt_fnt_xls    TYPE solix_tab,
            lv_fnt_sz_raw TYPE sood-objlen,
            lv_fnt_sz     TYPE i.
      PERFORM build_fnt_excel_attachment USING lt_fnt_data
                                        CHANGING lt_fnt_xls lv_fnt_sz_raw.
      lv_fnt_sz = lv_fnt_sz_raw.

      DATA lv_def_fnt TYPE string.
      CONCATENATE 'Fortnightly_CST_Purchase_' lv_date_from_str '_' lv_date_to_str '.xls' INTO lv_def_fnt.
      REPLACE ALL OCCURRENCES OF '/' IN lv_def_fnt WITH '-'.
      CLEAR: lv_filename, lv_fullpath, lv_path.
      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        EXPORTING
          default_file_name = lv_def_fnt
          default_extension = 'xls'
          file_filter       = 'Excel Files (*.xls)|*.xls|All Files (*.*)|*.*'
        CHANGING
          filename          = lv_filename
          path              = lv_path
          fullpath          = lv_fullpath
          user_action       = lv_user_action.

      IF lv_user_action = cl_gui_frontend_services=>action_ok.
        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            bin_filesize = lv_fnt_sz
            filename     = lv_fullpath
            filetype     = 'BIN'
          CHANGING
            data_tab     = lt_fnt_xls.
        MESSAGE s000(ygms_msg) WITH 'Fortnightly Excel downloaded successfully'.
      ENDIF.
    ENDIF.

  ELSEIF p_dpdf IS NOT INITIAL.
    " --- Download as PDF ---
    " Build daily PDF content (spool-based)
    DATA: lt_daily_pdf        TYPE soli_tab,
          lv_daily_pdf_sz_raw TYPE sood-objlen,
          lv_daily_pdf_sz     TYPE i.
    DATA: lt_daily_pdf_raw TYPE TABLE OF tline,
          lv_daily_pdf_len TYPE i.
    PERFORM build_pdf_attachment USING lt_send_data
                                CHANGING lt_daily_pdf lv_daily_pdf_sz_raw
                                         lt_daily_pdf_raw lv_daily_pdf_len.

    DATA lv_def_daily_pdf TYPE string.
    CONCATENATE 'Daily_CST_Purchase_' lv_date_from_str '_' lv_date_to_str '.pdf' INTO lv_def_daily_pdf.
    REPLACE ALL OCCURRENCES OF '/' IN lv_def_daily_pdf WITH '-'.
    CLEAR: lv_filename, lv_fullpath, lv_path.
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        default_file_name = lv_def_daily_pdf
        default_extension = 'pdf'
        file_filter       = 'PDF Files (*.pdf)|*.pdf|All Files (*.*)|*.*'
      CHANGING
        filename          = lv_filename
        path              = lv_path
        fullpath          = lv_fullpath
        user_action       = lv_user_action.

    IF lv_user_action = cl_gui_frontend_services=>action_ok.
      " Use raw tline table directly — avoids Unicode dump with solix_tab
      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          bin_filesize = lv_daily_pdf_len
          filename     = lv_fullpath
          filetype     = 'BIN'
        CHANGING
          data_tab     = lt_daily_pdf_raw.
      MESSAGE s000(ygms_msg) WITH 'Daily PDF downloaded successfully'.
    ENDIF.

    " Build fortnightly PDF content
    IF lt_fnt_data IS NOT INITIAL.
      DATA: lt_fnt_pdf        TYPE soli_tab,
            lv_fnt_pdf_sz_raw TYPE sood-objlen,
            lv_fnt_pdf_sz     TYPE i.
      DATA: lt_fnt_pdf_raw TYPE TABLE OF tline,
            lv_fnt_pdf_len TYPE i.
      PERFORM build_fnt_pdf_attachment USING lt_fnt_data
                                      CHANGING lt_fnt_pdf lv_fnt_pdf_sz_raw
                                               lt_fnt_pdf_raw lv_fnt_pdf_len.

      DATA lv_def_fnt_pdf TYPE string.
      CONCATENATE 'Fortnightly_CST_Purchase_' lv_date_from_str '_' lv_date_to_str '.pdf' INTO lv_def_fnt_pdf.
      REPLACE ALL OCCURRENCES OF '/' IN lv_def_fnt_pdf WITH '-'.
      CLEAR: lv_filename, lv_fullpath, lv_path.
      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        EXPORTING
          default_file_name = lv_def_fnt_pdf
          default_extension = 'pdf'
          file_filter       = 'PDF Files (*.pdf)|*.pdf|All Files (*.*)|*.*'
        CHANGING
          filename          = lv_filename
          path              = lv_path
          fullpath          = lv_fullpath
          user_action       = lv_user_action.

      IF lv_user_action = cl_gui_frontend_services=>action_ok.
        " Use raw tline table directly — avoids Unicode dump with solix_tab
        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            bin_filesize = lv_fnt_pdf_len
            filename     = lv_fullpath
            filetype     = 'BIN'
          CHANGING
            data_tab     = lt_fnt_pdf_raw.
        MESSAGE s000(ygms_msg) WITH 'Fortnightly PDF downloaded successfully'.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_SEND
*& Point 1: Send Allocation Data
*&---------------------------------------------------------------------*
FORM handle_send.
  DATA: lv_valid TYPE abap_bool.

  " Step 1.2: Data validation before initiating data transfer
  " Check if any new receipt data has been received from ONGC
  PERFORM validate_before_send CHANGING lv_valid.
  IF lv_valid = abap_false.
    RETURN.
  ENDIF.

  " Step 1.2.4: If no new data, display data preview with daily/fortnightly toggle
  " Then show Send mode selection popup
  PERFORM display_send_preview.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form VALIDATE_BEFORE_SEND
*& 1.2: Check if new receipt data from ONGC before sending
*&---------------------------------------------------------------------*
FORM validate_before_send CHANGING cv_valid TYPE abap_bool.
  DATA: lt_b2b_receipt TYPE TABLE OF yrga_cst_b2b_1,
        lt_cst_pur     TYPE TABLE OF yrga_cst_pur,
        lv_answer      TYPE c LENGTH 1,
        lv_new_found   TYPE abap_bool.

  cv_valid = abap_true.

  " 1.2.1: Fetch latest receipt data from YRGA_CST_B2B_1 for user inputs
  " Use YRGA_CST_LOC_MAP to convert Location ID to ONGC CTP ID
  DATA: lt_ctp_ids TYPE TABLE OF ygms_de_ongc_ctp.
  LOOP AT gt_loc_ctp_map INTO DATA(ls_map).
    COLLECT ls_map-ongc_ctp_id INTO lt_ctp_ids.
  ENDLOOP.

  IF lt_ctp_ids IS NOT INITIAL.
    SELECT * FROM yrga_cst_b2b_1
      INTO TABLE lt_b2b_receipt
      FOR ALL ENTRIES IN lt_ctp_ids
      WHERE ctp_id  = lt_ctp_ids-table_line
        AND gas_day BETWEEN gv_date_from AND gv_date_to
        AND qty_scm > 0.
  ENDIF.

  IF lt_b2b_receipt IS INITIAL.
    RETURN.  " No receipt data at all, proceed with send
  ENDIF.

  " Keep only latest records (dedup by timestamp)
  SORT lt_b2b_receipt BY ctp_id gas_day ongc_material ASCENDING time_stamp DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_b2b_receipt COMPARING ctp_id gas_day ongc_material.

  " 1.2.2: Fetch saved data from YRGA_CST_PUR by passing user inputs
  SELECT * FROM yrga_cst_pur
    INTO TABLE lt_cst_pur
    WHERE gas_day BETWEEN gv_date_from AND gv_date_to
      AND location IN s_loc AND deleted = ' '.

  " 1.2.3: Pick all ONGC IDs appearing in receipt data and check if they
  " appear in the saved data. Even if one ONGC ID is not found, block send.
  CLEAR gt_new_receipt_data.
  lv_new_found = abap_false.

  LOOP AT lt_b2b_receipt INTO DATA(ls_b2b).
    READ TABLE lt_cst_pur TRANSPORTING NO FIELDS
      WITH KEY ongc_id = ls_b2b-ongc_id.
    IF sy-subrc <> 0.
      " ONGC ID not found in saved data - new receipt data received
      lv_new_found = abap_true.
      MOVE-CORRESPONDING ls_b2b TO gs_new_receipt_data.
      APPEND gs_new_receipt_data TO gt_new_receipt_data.
    ENDIF.
  ENDLOOP.

  IF lv_new_found = abap_true.
    cv_valid = abap_false.
    " Show popup: Cannot send data as new receipt data from ONGC has been received
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        textline1      = 'Cannot send data as new receipt data from ONGC has been received.'
        textline2      = 'Please run allocation again. Click Yes to view details.'
        titel          = 'Cannot Send Data'
        cancel_display = ' '
      IMPORTING
        answer         = lv_answer.
    IF lv_answer = 'J'.  " Yes = View Details
      " Show complete record associated with new ONGC ID in separate screen
      PERFORM display_new_receipt_data.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_SEND_MODE_POPUP
*& 1.1: Select mode of data transfer (Email or B2B)
*&---------------------------------------------------------------------*
FORM show_send_mode_popup.
  DATA: lv_answer TYPE c LENGTH 1.

  CALL FUNCTION 'POPUP_TO_DECIDE'
    EXPORTING
      defaultoption = '1'
      textline1     = 'Please select the mode of data transfer:'
      textline2     = ''
      text_option1  = 'Through Email'
      text_option2  = 'Through B2B'
      titel         = 'Send Allocation Data'
    IMPORTING
      answer        = lv_answer.

  CASE lv_answer.
    WHEN '1'.
      " 1.1.1: Through Email
      PERFORM handle_send_email.
    WHEN '2'.
      " 1.1.2: Through B2B
      PERFORM handle_send_b2b.
    WHEN OTHERS.
      MESSAGE s000(ygms_msg) WITH 'Send operation cancelled'.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_SEND_EMAIL
*& 1.1.1: Send data through Email with PDF/Excel attachments
*&---------------------------------------------------------------------*
FORM handle_send_email.
  DATA: lt_send_data  TYPE TABLE OF yrga_cst_pur,
        lt_emails     TYPE TABLE OF string,
        lv_send_pdf   TYPE c LENGTH 1,
        lv_send_excel TYPE c LENGTH 1,
        lv_email_line TYPE string.

  " Clear popup fields before display
  CLEAR: p_eml1, p_eml2, p_eml3, p_eml4, p_eml5.
  p_pdf = 'X'.
  p_xls = 'X'.

  " Show custom popup selection screen 2000
  CALL SELECTION-SCREEN 2000 STARTING AT 5 5
                              ENDING AT 95 14.

  IF sy-subrc <> 0.  " User pressed Cancel / Back
    MESSAGE s000(ygms_msg) WITH 'Email send cancelled'.
    RETURN.
  ENDIF.

  " Read format options
  lv_send_pdf   = p_pdf.
  lv_send_excel = p_xls.

  " Validate at least one format selected
  IF lv_send_pdf IS INITIAL AND lv_send_excel IS INITIAL.
    MESSAGE s000(ygms_msg) WITH 'Please select at least one format (PDF or Excel)'.
    RETURN.
  ENDIF.

  " Collect all non-empty email addresses into internal table
  IF p_eml1 IS NOT INITIAL.
    lv_email_line = p_eml1.
    CONDENSE lv_email_line.
    APPEND lv_email_line TO lt_emails.
  ENDIF.
  IF p_eml2 IS NOT INITIAL.
    lv_email_line = p_eml2.
    CONDENSE lv_email_line.
    APPEND lv_email_line TO lt_emails.
  ENDIF.
  IF p_eml3 IS NOT INITIAL.
    lv_email_line = p_eml3.
    CONDENSE lv_email_line.
    APPEND lv_email_line TO lt_emails.
  ENDIF.
  IF p_eml4 IS NOT INITIAL.
    lv_email_line = p_eml4.
    CONDENSE lv_email_line.
    APPEND lv_email_line TO lt_emails.
  ENDIF.
  IF p_eml5 IS NOT INITIAL.
    lv_email_line = p_eml5.
    CONDENSE lv_email_line.
    APPEND lv_email_line TO lt_emails.
  ENDIF.

  IF lt_emails IS INITIAL.
    MESSAGE s000(ygms_msg) WITH 'Please enter at least one email address'.
    RETURN.
  ENDIF.

  " Fetch daily data from YRGA_CST_PUR where EXCLUDED flag is not X
  SELECT * FROM yrga_cst_pur
    INTO TABLE lt_send_data
    WHERE gas_day BETWEEN gv_date_from AND gv_date_to
      AND location IN s_loc
      AND exclude <> 'X' AND deleted = ' '.

  IF lt_send_data IS INITIAL.
    MESSAGE s000(ygms_msg) WITH 'No data found to send for the selected period'.
    RETURN.
  ENDIF.

  " Fetch fortnightly data from YRGA_CST_FN_DATA
  DATA lt_fnt_data TYPE TABLE OF yrga_cst_fn_data.
  SELECT * FROM yrga_cst_fn_data
    INTO TABLE lt_fnt_data
    WHERE date_from = gv_date_from
      AND date_to   = gv_date_to
      AND location  IN s_loc AND deleted = ' '.

  " Send email with PDF and/or Excel attachments (daily + fortnightly)
  PERFORM send_email USING lt_emails lt_send_data lt_fnt_data lv_send_pdf lv_send_excel.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SEND_EMAIL
*& Send email with PDF and/or Excel attachments using CL_BCS
*&---------------------------------------------------------------------*
FORM send_email USING pt_emails   TYPE string_table
                      pt_data     TYPE STANDARD TABLE
                      pt_fnt_data TYPE STANDARD TABLE
                      pv_send_pdf TYPE c
                      pv_send_xls TYPE c.
  DATA: lo_send_request TYPE REF TO cl_bcs,
        lo_document     TYPE REF TO cl_document_bcs,
        lo_recipient    TYPE REF TO if_recipient_bcs,
        lo_sender       TYPE REF TO cl_sapuser_bcs,
        lt_body         TYPE bcsy_text,
        ls_body         TYPE soli,
        lv_subject      TYPE so_obj_des,
        lt_att_hex      TYPE solix_tab,
        lt_att_text     TYPE soli_tab,
        lv_att_subject  TYPE sood-objdes,
        lv_att_size     TYPE sood-objlen,
        lv_sent_all     TYPE os_boolean,
        lx_bcs          TYPE REF TO cx_bcs.
  DATA: lv_date_from_str TYPE c LENGTH 10,
        lv_date_to_str   TYPE c LENGTH 10.
  DATA: l_mail TYPE adr6-smtp_addr.
  DATA: lv_ctp_list TYPE string,
        ls_pur      TYPE yrga_cst_pur.
  " Build unique CTP ID list
  DATA: lt_ctp TYPE TABLE OF ygms_de_ongc_ctp,
        lv_ctp TYPE ygms_de_ongc_ctp.
  LOOP AT pt_data INTO ls_pur.
    READ TABLE lt_ctp WITH KEY table_line = ls_pur-ctp TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND ls_pur-ctp TO lt_ctp.
    ENDIF.
  ENDLOOP.
  " Build CTP list string (comma-separated, no space before comma)
  LOOP AT lt_ctp INTO lv_ctp.
    IF lv_ctp_list IS INITIAL.
      lv_ctp_list = lv_ctp.
    ELSE.
      CONCATENATE lv_ctp_list ',' INTO lv_ctp_list.
      CONCATENATE lv_ctp_list lv_ctp INTO lv_ctp_list SEPARATED BY space.
    ENDIF.
  ENDLOOP.
  DATA: lv_date_from_dot TYPE c LENGTH 10,
        lv_date_to_dot   TYPE c LENGTH 10.
  WRITE gv_date_from TO lv_date_from_str DD/MM/YYYY.
  WRITE gv_date_to   TO lv_date_to_str   DD/MM/YYYY.
  " Build dot-separated dates (DD.MM.YYYY) for subject
  lv_date_from_dot = lv_date_from_str.
  REPLACE ALL OCCURRENCES OF '/' IN lv_date_from_dot WITH '.'.
  lv_date_to_dot = lv_date_to_str.
  REPLACE ALL OCCURRENCES OF '/' IN lv_date_to_dot WITH '.'.
  TRY.
      " Create persistent send request
      lo_send_request = cl_bcs=>create_persistent( ).
      " Build email subject: State wise CST purchase DD.MM.YYYY to DD.MM.YYYY.
      CONCATENATE 'State wise CST purchase'
        lv_date_from_dot 'to' lv_date_to_dot
        INTO lv_subject SEPARATED BY space.
      CONCATENATE lv_subject '.' INTO lv_subject.
      " Build email body
      CONCATENATE 'Please find attached daily and fortnightly state wise CST purchase data for CTP IDs'
        lv_ctp_list 'for the period' lv_date_from_str 'to' lv_date_to_str
        INTO ls_body-line SEPARATED BY space.
      CONCATENATE ls_body-line '.' INTO ls_body-line.
      APPEND ls_body TO lt_body.
      " Create email document (body)
      lo_document = cl_document_bcs=>create_document(
        i_type    = 'RAW'
        i_text    = lt_body
        i_subject = lv_subject ).
      " --- Daily attachments ---
      " Add Daily Excel attachment if selected
      IF pv_send_xls = 'X'.
        CLEAR: lt_att_hex, lv_att_size.
        PERFORM build_excel_attachment USING pt_data
                                      CHANGING lt_att_hex lv_att_size.
        CONCATENATE 'Daily CST Purchase' lv_date_from_str INTO lv_att_subject SEPARATED BY space.
        CONCATENATE lv_att_subject '-' lv_date_to_str INTO lv_att_subject.
        lo_document->add_attachment(
          i_attachment_type    = 'XLS'
          i_attachment_subject = lv_att_subject
          i_attachment_size    = lv_att_size
          i_att_content_hex    = lt_att_hex ).
      ENDIF.
      " Add Daily PDF attachment if selected
      IF pv_send_pdf = 'X'.
        CLEAR: lt_att_text, lv_att_size.
        DATA: lt_dummy_tline TYPE TABLE OF tline,
              lv_dummy_len   TYPE i.
        PERFORM build_pdf_attachment USING pt_data
                                    CHANGING lt_att_text lv_att_size
                                             lt_dummy_tline lv_dummy_len.
        CONCATENATE 'Daily CST Purchase' lv_date_from_str INTO lv_att_subject SEPARATED BY space.
        CONCATENATE lv_att_subject '-' lv_date_to_str INTO lv_att_subject.
        lo_document->add_attachment(
          i_attachment_type    = 'PDF'
          i_attachment_subject = lv_att_subject
          i_attachment_size    = lv_att_size
          i_att_content_text   = lt_att_text ).
      ENDIF.
      " --- Fortnightly attachments ---
      " Add Fortnightly Excel attachment if selected
      IF pv_send_xls = 'X'.
        CLEAR: lt_att_hex, lv_att_size.
        PERFORM build_fnt_excel_attachment USING pt_fnt_data
                                          CHANGING lt_att_hex lv_att_size.
        CONCATENATE 'Fortnightly CST Purchase' lv_date_from_str INTO lv_att_subject SEPARATED BY space.
        CONCATENATE lv_att_subject '-' lv_date_to_str INTO lv_att_subject.
        lo_document->add_attachment(
          i_attachment_type    = 'XLS'
          i_attachment_subject = lv_att_subject
          i_attachment_size    = lv_att_size
          i_att_content_hex    = lt_att_hex ).
      ENDIF.
      " Add Fortnightly PDF attachment if selected
      IF pv_send_pdf = 'X'.
        CLEAR: lt_att_text, lv_att_size.
        CLEAR: lt_dummy_tline, lv_dummy_len.
        PERFORM build_fnt_pdf_attachment USING pt_fnt_data
                                        CHANGING lt_att_text lv_att_size
                                                 lt_dummy_tline lv_dummy_len.
        CONCATENATE 'Fortnightly CST Purchase' lv_date_from_str INTO lv_att_subject SEPARATED BY space.
        CONCATENATE lv_att_subject '-' lv_date_to_str INTO lv_att_subject.
        lo_document->add_attachment(
          i_attachment_type    = 'PDF'
          i_attachment_subject = lv_att_subject
          i_attachment_size    = lv_att_size
          i_att_content_text   = lt_att_text ).
      ENDIF.
      " Set document to send request
      lo_send_request->set_document( lo_document ).
      " Add all recipients
      LOOP AT pt_emails INTO DATA(lv_email).
        CONDENSE lv_email.
        l_mail = lv_email.
        lo_recipient = cl_cam_address_bcs=>create_internet_address( l_mail ).
        lo_send_request->add_recipient( lo_recipient ).
      ENDLOOP.
      " Set sender as current user
      lo_sender = cl_sapuser_bcs=>create( sy-uname ).
      lo_send_request->set_sender( lo_sender ).
      " Add CC to the sending user - look up email from PA0105
      DATA: lv_sender_email TYPE adr6-smtp_addr.
      SELECT SINGLE usrid INTO @lv_sender_email
        FROM pa0105
        WHERE pernr = @sy-uname
          AND subty = 'E-ML'
          AND endda = '99991231'.
      IF sy-subrc = 0 AND lv_sender_email IS NOT INITIAL.
        l_mail = lv_sender_email.
        DATA(lo_cc_recipient) = cl_cam_address_bcs=>create_internet_address( l_mail ).
        lo_send_request->add_recipient(
          i_recipient = lo_cc_recipient
          i_copy      = abap_true ).
      ENDIF.
      " Send immediately
      lo_send_request->set_send_immediately( abap_true ).
      lv_sent_all = lo_send_request->send( ).
      IF lv_sent_all = abap_true.
        COMMIT WORK.
        MESSAGE s000(ygms_msg) WITH 'Email sent successfully'.
      ELSE.
        MESSAGE s000(ygms_msg) WITH 'Error sending email'.
      ENDIF.
    CATCH cx_bcs INTO lx_bcs.
      DATA(lv_error_text) = lx_bcs->get_text( ).
      MESSAGE s000(ygms_msg) WITH 'Email error:' lv_error_text.
  ENDTRY.
  " Update sent tracking fields in source tables (email = 2)
  UPDATE yrga_cst_pur SET sent_e  = '2'
                          sent_by = sy-uname
                          sent_on = sy-datum
                          sent_at = sy-uzeit
    WHERE gas_day BETWEEN gv_date_from AND gv_date_to
      AND location IN s_loc
      AND exclude <> 'X' and deleted = ' '.
  UPDATE yrga_cst_fn_data SET sent_e  = '2'
                              sent_by = sy-uname
                              sent_on = sy-datum
                              sent_at = sy-uzeit
    WHERE date_from = gv_date_from
      AND date_to   = gv_date_to
      AND location  IN s_loc and deleted = ' '.
  COMMIT WORK AND WAIT.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_EXCEL_ATTACHMENT
*& Build daily Excel attachment using XML Spreadsheet 2003 format
*&---------------------------------------------------------------------*
FORM build_excel_attachment USING pt_data    TYPE STANDARD TABLE
                           CHANGING ct_content TYPE solix_tab
                                    cv_size    TYPE sood-objlen.
  DATA: lv_xml     TYPE string,
        lv_xstring TYPE xstring,
        ls_pur     TYPE yrga_cst_pur.
  DATA: lv_gas_day TYPE c LENGTH 10,
        lv_gcv     TYPE c LENGTH 15,
        lv_ncv     TYPE c LENGTH 15,
        lv_qty_scm TYPE c LENGTH 15,
        lv_qty_mbg TYPE c LENGTH 15.
  " Summary sheet types
  TYPES: BEGIN OF ty_summary_key,
           ctp        TYPE ygms_de_ongc_ctp,
           ongc_mater TYPE ygms_de_ongc_mat,
           state_code TYPE yrga_cst_pur-state_code,
           state      TYPE yrga_cst_pur-state,
         END OF ty_summary_key.
  TYPES: BEGIN OF ty_summary,
           ctp        TYPE ygms_de_ongc_ctp,
           ongc_mater TYPE ygms_de_ongc_mat,
           state_code TYPE yrga_cst_pur-state_code,
           state      TYPE yrga_cst_pur-state,
           total_mbg  TYPE yrga_cst_pur-qty_in_mbg,
           total_scm  TYPE yrga_cst_pur-qty_in_scm,
           sum_gcv    TYPE yrga_cst_pur-gcv,
           sum_ncv    TYPE yrga_cst_pur-ncv,
           cnt        TYPE i,
         END OF ty_summary.
  DATA: lt_summary TYPE SORTED TABLE OF ty_summary WITH UNIQUE KEY ctp state_code ongc_mater,
        ls_summary TYPE ty_summary.
  DATA: lt_days    TYPE SORTED TABLE OF sy-datum WITH UNIQUE KEY table_line,
        lt_day_qty TYPE HASHED TABLE OF yrga_cst_pur WITH UNIQUE KEY ctp ongc_mater state_code gas_day.
  DATA: lv_day_str TYPE c LENGTH 10,
        lv_val     TYPE c LENGTH 15,
        lv_avg     TYPE c LENGTH 15.
  DATA: lc_xml_hdr TYPE string,
        lc_wb_open TYPE string,
        lc_styles  TYPE string.
  lc_xml_hdr = '<?xml version="1.0" encoding="UTF-8"?><?mso-application progid="Excel.Sheet"?>'.
  CONCATENATE '<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"'
    ' xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"'
    ' xmlns:x="urn:schemas-microsoft-com:office:excel">'
    INTO lc_wb_open.
  CONCATENATE '<Styles>'
    '<Style ss:ID="hdr"><Font ss:Bold="1" ss:FontName="Times New Roman" ss:Size="11"/>'
    '<Alignment ss:Horizontal="Center" ss:Vertical="Center"/>'
    '<Borders><Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/></Borders></Style>'
    '<Style ss:ID="dat"><Font ss:FontName="Times New Roman" ss:Size="11"/>'
    '<Borders><Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/></Borders></Style>'
    '<Style ss:ID="num"><Font ss:FontName="Times New Roman" ss:Size="11"/>'
    '<NumberFormat ss:Format="0.000"/><Alignment ss:Horizontal="Right"/>'
    '<Borders><Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/></Borders></Style>'
    '</Styles>'
    INTO lc_styles.
  " ---- Build summary data (aggregate by CTP/Material/State) ----
  LOOP AT pt_data INTO ls_pur.
    COLLECT: ls_pur-gas_day INTO lt_days.
    READ TABLE lt_summary WITH KEY ctp = ls_pur-ctp ongc_mater = ls_pur-ongc_mater
      state_code = ls_pur-state_code ASSIGNING FIELD-SYMBOL(<fs_sum>).
    IF sy-subrc = 0.
      <fs_sum>-total_mbg = <fs_sum>-total_mbg + ls_pur-qty_in_mbg.
      <fs_sum>-total_scm = <fs_sum>-total_scm + ls_pur-qty_in_scm.
      <fs_sum>-sum_gcv   = <fs_sum>-sum_gcv + ls_pur-gcv.
      <fs_sum>-sum_ncv   = <fs_sum>-sum_ncv + ls_pur-ncv.
      <fs_sum>-cnt       = <fs_sum>-cnt + 1.
    ELSE.
      ls_summary-ctp        = ls_pur-ctp.
      ls_summary-ongc_mater = ls_pur-ongc_mater.
      ls_summary-state_code = ls_pur-state_code.
      ls_summary-state      = ls_pur-state.
      ls_summary-total_mbg  = ls_pur-qty_in_mbg.
      ls_summary-total_scm  = ls_pur-qty_in_scm.
      ls_summary-sum_gcv    = ls_pur-gcv.
      ls_summary-sum_ncv    = ls_pur-ncv.
      ls_summary-cnt        = 1.
      INSERT ls_summary INTO TABLE lt_summary.
    ENDIF.
    INSERT ls_pur INTO TABLE lt_day_qty.
  ENDLOOP.
  " ---- Sheet 1: Summary (first sheet) ----
  CONCATENATE lc_xml_hdr lc_wb_open lc_styles
    '<Worksheet ss:Name="Summary"><Table>'
    '<Column ss:AutoFitWidth="1" ss:Width="80"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="100"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="75"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="120"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="80"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="80"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="70"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="70"/>'
    INTO lv_xml.
  " Add column widths for each day
  LOOP AT lt_days INTO DATA(lv_day_date).
    CONCATENATE lv_xml '<Column ss:AutoFitWidth="1" ss:Width="80"/>' INTO lv_xml.
  ENDLOOP.
  " Summary header row
  CONCATENATE lv_xml
    '<Row ss:StyleID="hdr">'
    '<Cell><Data ss:Type="String">CTP ID</Data></Cell>'
    '<Cell><Data ss:Type="String">ONGC Material</Data></Cell>'
    '<Cell><Data ss:Type="String">State Code</Data></Cell>'
    '<Cell><Data ss:Type="String">State</Data></Cell>'
    '<Cell><Data ss:Type="String">Total MBG</Data></Cell>'
    '<Cell><Data ss:Type="String">Total Sm3</Data></Cell>'
    '<Cell><Data ss:Type="String">Avg. GCV</Data></Cell>'
    '<Cell><Data ss:Type="String">Avg. NCV</Data></Cell>'
    INTO lv_xml.
  " One header column per gas day (formatted as DD.MM.YYYY)
  LOOP AT lt_days INTO lv_day_date.
    WRITE lv_day_date TO lv_day_str DD/MM/YYYY.
    CONDENSE lv_day_str.
    REPLACE ALL OCCURRENCES OF '/' IN lv_day_str WITH '.'.
    CONCATENATE lv_xml '<Cell><Data ss:Type="String">' lv_day_str '</Data></Cell>' INTO lv_xml.
  ENDLOOP.
  CONCATENATE lv_xml '</Row>' INTO lv_xml.
  " Summary data rows
  LOOP AT lt_summary INTO ls_summary.
    WRITE ls_summary-total_mbg TO lv_qty_mbg DECIMALS 3.
    WRITE ls_summary-total_scm TO lv_qty_scm DECIMALS 3.
    CONDENSE: lv_qty_mbg, lv_qty_scm.
    " Average GCV/NCV
    IF ls_summary-cnt > 0.
      DATA(lv_avg_gcv) = ls_summary-sum_gcv / ls_summary-cnt.
      DATA(lv_avg_ncv) = ls_summary-sum_ncv / ls_summary-cnt.
    ELSE.
      lv_avg_gcv = 0.
      lv_avg_ncv = 0.
    ENDIF.
    WRITE lv_avg_gcv TO lv_gcv DECIMALS 3.
    WRITE lv_avg_ncv TO lv_ncv DECIMALS 3.
    CONDENSE: lv_gcv, lv_ncv.
    CONCATENATE lv_xml
      '<Row ss:StyleID="dat">'
      '<Cell><Data ss:Type="String">' ls_summary-ctp '</Data></Cell>'
      '<Cell><Data ss:Type="String">' ls_summary-ongc_mater '</Data></Cell>'
      '<Cell><Data ss:Type="String">' ls_summary-state_code '</Data></Cell>'
      '<Cell><Data ss:Type="String">' ls_summary-state '</Data></Cell>'
      '<Cell ss:StyleID="num"><Data ss:Type="Number">' lv_qty_mbg '</Data></Cell>'
      '<Cell ss:StyleID="num"><Data ss:Type="Number">' lv_qty_scm '</Data></Cell>'
      '<Cell ss:StyleID="num"><Data ss:Type="Number">' lv_gcv '</Data></Cell>'
      '<Cell ss:StyleID="num"><Data ss:Type="Number">' lv_ncv '</Data></Cell>'
      INTO lv_xml.
    " Per-day Qty in SCM values
    LOOP AT lt_days INTO lv_day_date.
      READ TABLE lt_day_qty WITH KEY ctp = ls_summary-ctp ongc_mater = ls_summary-ongc_mater
        state_code = ls_summary-state_code gas_day = lv_day_date INTO ls_pur.
      IF sy-subrc = 0.
        WRITE ls_pur-qty_in_scm TO lv_val DECIMALS 3.
        CONDENSE lv_val.
      ELSE.
        lv_val = '0.000'.
      ENDIF.
      CONCATENATE lv_xml '<Cell ss:StyleID="num"><Data ss:Type="Number">' lv_val '</Data></Cell>'
        INTO lv_xml.
    ENDLOOP.
    CONCATENATE lv_xml '</Row>' INTO lv_xml.
  ENDLOOP.
  CONCATENATE lv_xml '</Table></Worksheet>' INTO lv_xml.
  " ---- Sheet 2: Daily Data ----
  CONCATENATE lv_xml
    '<Worksheet ss:Name="Daily Data"><Table>'
    '<Column ss:AutoFitWidth="1" ss:Width="80"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="80"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="100"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="75"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="120"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="80"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="65"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="65"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="80"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="100"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="100"/>'
    INTO lv_xml.
  " Header row
  CONCATENATE lv_xml
    '<Row ss:StyleID="hdr">'
    '<Cell><Data ss:Type="String">Gas Day</Data></Cell>'
    '<Cell><Data ss:Type="String">CTP ID</Data></Cell>'
    '<Cell><Data ss:Type="String">ONGC Material</Data></Cell>'
    '<Cell><Data ss:Type="String">State Code</Data></Cell>'
    '<Cell><Data ss:Type="String">State</Data></Cell>'
    '<Cell><Data ss:Type="String">Qty SCM</Data></Cell>'
    '<Cell><Data ss:Type="String">GCV</Data></Cell>'
    '<Cell><Data ss:Type="String">NCV</Data></Cell>'
    '<Cell><Data ss:Type="String">Qty MBG</Data></Cell>'
    '<Cell><Data ss:Type="String">ONGC ID</Data></Cell>'
    '<Cell><Data ss:Type="String">GAIL ID</Data></Cell>'
    '</Row>'
    INTO lv_xml.
  " Data rows
  LOOP AT pt_data INTO ls_pur.
    WRITE ls_pur-gas_day TO lv_gas_day DD/MM/YYYY.
    WRITE ls_pur-gcv TO lv_gcv DECIMALS 3.
    WRITE ls_pur-ncv TO lv_ncv DECIMALS 3.
    WRITE ls_pur-qty_in_scm TO lv_qty_scm DECIMALS 3.
    WRITE ls_pur-qty_in_mbg TO lv_qty_mbg DECIMALS 3.
    CONDENSE: lv_gas_day, lv_gcv, lv_ncv, lv_qty_scm, lv_qty_mbg.
    CONCATENATE lv_xml
      '<Row ss:StyleID="dat">'
      '<Cell><Data ss:Type="String">' lv_gas_day '</Data></Cell>'
      '<Cell><Data ss:Type="String">' ls_pur-ctp '</Data></Cell>'
      '<Cell><Data ss:Type="String">' ls_pur-ongc_mater '</Data></Cell>'
      '<Cell><Data ss:Type="String">' ls_pur-state_code '</Data></Cell>'
      '<Cell><Data ss:Type="String">' ls_pur-state '</Data></Cell>'
      '<Cell ss:StyleID="num"><Data ss:Type="Number">' lv_qty_scm '</Data></Cell>'
      '<Cell ss:StyleID="num"><Data ss:Type="Number">' lv_gcv '</Data></Cell>'
      '<Cell ss:StyleID="num"><Data ss:Type="Number">' lv_ncv '</Data></Cell>'
      '<Cell ss:StyleID="num"><Data ss:Type="Number">' lv_qty_mbg '</Data></Cell>'
      '<Cell><Data ss:Type="String">' ls_pur-ongc_id '</Data></Cell>'
      '<Cell><Data ss:Type="String">' ls_pur-gail_id '</Data></Cell>'
      '</Row>'
      INTO lv_xml.
  ENDLOOP.
  CONCATENATE lv_xml '</Table></Worksheet></Workbook>' INTO lv_xml.
  " Convert XML string to xstring then to solix_tab
  DATA(lo_conv) = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
  lo_conv->convert( EXPORTING data = lv_xml IMPORTING buffer = lv_xstring ).
  ct_content = cl_bcs_convert=>xstring_to_solix( lv_xstring ).
  cv_size = xstrlen( lv_xstring ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_PDF_ATTACHMENT
*& Build PDF attachment from daily data using spool-to-PDF conversion
*&---------------------------------------------------------------------*
FORM build_pdf_attachment USING pt_data    TYPE STANDARD TABLE
                         CHANGING ct_content TYPE soli_tab
                                  cv_size    TYPE sood-objlen
                                  ct_pdf_raw TYPE STANDARD TABLE
                                  cv_pdf_len TYPE i.
  DATA: ls_pur     TYPE yrga_cst_pur.
  DATA: lv_gas_day TYPE c LENGTH 10,
        lv_gcv     TYPE c LENGTH 15,
        lv_ncv     TYPE c LENGTH 15,
        lv_qty_scm TYPE c LENGTH 15,
        lv_qty_mbg TYPE c LENGTH 15.
  DATA: lv_date_from_str TYPE c LENGTH 10,
        lv_date_to_str   TYPE c LENGTH 10.
  DATA: ls_params  TYPE pri_params,
        lv_valid   TYPE c,
        lv_spool   TYPE rspoid,
        lt_pdf     TYPE TABLE OF tline,
        lv_pdf_len TYPE i,
        lv_xstring TYPE xstring.
  DATA: lv_date_str TYPE c LENGTH 10,
        lv_time_str TYPE c LENGTH 8,
        lv_page     TYPE i VALUE 1,
        lv_page_str TYPE c LENGTH 5.
  " Summary aggregation types
  TYPES: BEGIN OF ty_pdf_sum,
           ctp        TYPE ygms_de_ongc_ctp,
           ongc_mater TYPE ygms_de_ongc_mat,
           state_code TYPE yrga_cst_pur-state_code,
           state      TYPE yrga_cst_pur-state,
           total_mbg  TYPE yrga_cst_pur-qty_in_mbg,
           total_scm  TYPE yrga_cst_pur-qty_in_scm,
           sum_gcv    TYPE yrga_cst_pur-gcv,
           sum_ncv    TYPE yrga_cst_pur-ncv,
           cnt        TYPE i,
         END OF ty_pdf_sum.
  DATA: lt_pdf_sum TYPE SORTED TABLE OF ty_pdf_sum WITH UNIQUE KEY ctp state_code ongc_mater,
        ls_pdf_sum TYPE ty_pdf_sum.
  DATA: lv_avg_str TYPE c LENGTH 15.

  WRITE gv_date_from TO lv_date_from_str DD/MM/YYYY.
  WRITE gv_date_to   TO lv_date_to_str   DD/MM/YYYY.
  " Download date/time
  WRITE sy-datum TO lv_date_str DD/MM/YYYY.
  WRITE sy-uzeit TO lv_time_str USING EDIT MASK '__:__:__'.

  " Build summary aggregation
  LOOP AT pt_data INTO ls_pur.
    READ TABLE lt_pdf_sum WITH KEY ctp = ls_pur-ctp ongc_mater = ls_pur-ongc_mater
      state_code = ls_pur-state_code ASSIGNING FIELD-SYMBOL(<fs_psum>).
    IF sy-subrc = 0.
      <fs_psum>-total_mbg = <fs_psum>-total_mbg + ls_pur-qty_in_mbg.
      <fs_psum>-total_scm = <fs_psum>-total_scm + ls_pur-qty_in_scm.
      <fs_psum>-sum_gcv   = <fs_psum>-sum_gcv + ls_pur-gcv.
      <fs_psum>-sum_ncv   = <fs_psum>-sum_ncv + ls_pur-ncv.
      <fs_psum>-cnt       = <fs_psum>-cnt + 1.
    ELSE.
      ls_pdf_sum-ctp        = ls_pur-ctp.
      ls_pdf_sum-ongc_mater = ls_pur-ongc_mater.
      ls_pdf_sum-state_code = ls_pur-state_code.
      ls_pdf_sum-state      = ls_pur-state.
      ls_pdf_sum-total_mbg  = ls_pur-qty_in_mbg.
      ls_pdf_sum-total_scm  = ls_pur-qty_in_scm.
      ls_pdf_sum-sum_gcv    = ls_pur-gcv.
      ls_pdf_sum-sum_ncv    = ls_pur-ncv.
      ls_pdf_sum-cnt        = 1.
      INSERT ls_pdf_sum INTO TABLE lt_pdf_sum.
    ENDIF.
  ENDLOOP.

  " Get print parameters for spool creation
  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      no_dialog      = 'X'
      immediately    = ' '
      release        = ' '
      new_list_id    = 'X'
      line_size      = 200
      line_count     = 65
    IMPORTING
      out_parameters = ls_params
      valid          = lv_valid
    EXCEPTIONS
      OTHERS         = 1.

  IF lv_valid <> 'X'.
    RETURN.
  ENDIF.

  " Create spool with formatted table output
  NEW-PAGE PRINT ON PARAMETERS ls_params NO DIALOG.

  " ---- Page 1+: Daily Detail (one row per gas day record) ----
  lv_page_str = lv_page.
  CONDENSE lv_page_str.
  WRITE: /5 'Downloaded', lv_date_str, AT 180 lv_time_str.
  WRITE: /75 'ONGC CST Statewise Allocation', AT 185 lv_page_str.
  WRITE: /5 'Daily CST Purchase Data -',
           lv_date_from_str, 'to', lv_date_to_str.
  SKIP 1.
  ULINE AT /5(180).
  FORMAT INTENSIFIED ON.
  WRITE: /5(10) 'Gas Day',
          16(12) 'CTP ID',
          29(15) 'ONGC Material',
          45(8)  'State Cd',
          54(20) 'State',
          75(15) 'Qty SCM',
          91(12) 'GCV',
          104(12) 'NCV',
          117(15) 'Qty MBG',
          133(20) 'ONGC ID',
          154(14) 'GAIL ID'.
  FORMAT INTENSIFIED OFF.
  ULINE AT /5(180).

  " Sort by gas day, then CTP, state, material so records appear in date order
  DATA lt_daily_sorted TYPE TABLE OF yrga_cst_pur.
  lt_daily_sorted = pt_data.
  SORT lt_daily_sorted BY gas_day ctp state_code ongc_mater ASCENDING.

  LOOP AT lt_daily_sorted INTO ls_pur.
    WRITE ls_pur-gas_day TO lv_gas_day DD/MM/YYYY.
    WRITE ls_pur-qty_in_scm TO lv_qty_scm DECIMALS 3.
    WRITE ls_pur-gcv TO lv_gcv DECIMALS 3.
    WRITE ls_pur-ncv TO lv_ncv DECIMALS 3.
    WRITE ls_pur-qty_in_mbg TO lv_qty_mbg DECIMALS 3.
    CONDENSE: lv_gas_day, lv_qty_scm, lv_gcv, lv_ncv, lv_qty_mbg.
    WRITE: /5(10) lv_gas_day,
            16(12) ls_pur-ctp,
            29(15) ls_pur-ongc_mater,
            45(8)  ls_pur-state_code,
            54(20) ls_pur-state,
            75(15) lv_qty_scm,
            91(12) lv_gcv,
            104(12) lv_ncv,
            117(15) lv_qty_mbg,
            133(20) ls_pur-ongc_id,
            154(14) ls_pur-gail_id.
    ULINE AT /5(180).
  ENDLOOP.

  " ---- Summary Page (after daily detail) ----
  lv_page = lv_page + 1.
  NEW-PAGE.
  lv_page_str = lv_page.
  CONDENSE lv_page_str.
  WRITE: /5 'Downloaded', lv_date_str, AT 180 lv_time_str.
  WRITE: /75 'ONGC CST Statewise Allocation', AT 185 lv_page_str.
  WRITE: /5 'Daily CST Purchase Data - Summary -',
           lv_date_from_str, 'to', lv_date_to_str.
  SKIP 1.
  ULINE AT /5(175).
  FORMAT INTENSIFIED ON.
  WRITE: /5(12) 'CTP ID',
          18(15) 'ONGC Material',
          34(8)  'State Cd',
          43(20) 'State',
          64(15) 'Total MBG',
          80(15) 'Total Sm3',
          96(12) 'Avg. GCV',
          109(12) 'Avg. NCV'.
  FORMAT INTENSIFIED OFF.
  ULINE AT /5(175).

  LOOP AT lt_pdf_sum INTO ls_pdf_sum.
    WRITE ls_pdf_sum-total_mbg TO lv_qty_mbg DECIMALS 3.
    WRITE ls_pdf_sum-total_scm TO lv_qty_scm DECIMALS 3.
    CONDENSE: lv_qty_mbg, lv_qty_scm.
    IF ls_pdf_sum-cnt > 0.
      DATA(lv_a_gcv) = ls_pdf_sum-sum_gcv / ls_pdf_sum-cnt.
      DATA(lv_a_ncv) = ls_pdf_sum-sum_ncv / ls_pdf_sum-cnt.
    ELSE.
      lv_a_gcv = 0.
      lv_a_ncv = 0.
    ENDIF.
    WRITE lv_a_gcv TO lv_gcv DECIMALS 3.
    WRITE lv_a_ncv TO lv_ncv DECIMALS 3.
    CONDENSE: lv_gcv, lv_ncv.
    WRITE: /5(12) ls_pdf_sum-ctp,
            18(15) ls_pdf_sum-ongc_mater,
            34(8)  ls_pdf_sum-state_code,
            43(20) ls_pdf_sum-state,
            64(15) lv_qty_mbg,
            80(15) lv_qty_scm,
            96(12) lv_gcv,
            109(12) lv_ncv.
    ULINE AT /5(175).
  ENDLOOP.

  NEW-PAGE PRINT OFF.

  " Get the spool request ID (most recent for current user)
  SELECT rqident FROM tsp01 UP TO 1 ROWS
    INTO lv_spool
    WHERE rqowner  = sy-uname
      AND rqclient = sy-mandt
    ORDER BY rqident DESCENDING.
  ENDSELECT.

  IF sy-subrc <> 0 OR lv_spool IS INITIAL.
    RETURN.
  ENDIF.

  " Convert spool to PDF
  CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
    EXPORTING
      src_spoolid   = lv_spool
      no_dialog     = 'X'
    IMPORTING
      pdf_bytecount = lv_pdf_len
    TABLES
      pdf           = lt_pdf
    EXCEPTIONS
      OTHERS        = 1.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  " Convert tline table (134 bytes/line) to soli (255 bytes/line) for email
  CALL FUNCTION 'SX_TABLE_LINE_WIDTH_CHANGE'
    EXPORTING
      line_width_src = 134
      line_width_dst = 255
      transfer_bin   = 'X'
    TABLES
      content_in     = lt_pdf
      content_out    = ct_content
    EXCEPTIONS
      OTHERS         = 1.

  cv_size = lv_pdf_len.

  " Also return raw tline table for binary download (avoids Unicode dump)
  ct_pdf_raw[] = lt_pdf[].
  cv_pdf_len   = lv_pdf_len.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_FNT_EXCEL_ATTACHMENT
*& Build fortnightly Excel attachment using XML Spreadsheet 2003 format
*&---------------------------------------------------------------------*
FORM build_fnt_excel_attachment USING pt_data    TYPE STANDARD TABLE
                               CHANGING ct_content TYPE solix_tab
                                        cv_size    TYPE sood-objlen.
  DATA: lv_xml     TYPE string,
        lv_xstring TYPE xstring,
        ls_fnt     TYPE yrga_cst_fn_data.
  DATA: lv_date_from TYPE c LENGTH 10,
        lv_date_to   TYPE c LENGTH 10,
        lv_gcv       TYPE c LENGTH 15,
        lv_ncv       TYPE c LENGTH 15,
        lv_qty_scm   TYPE c LENGTH 15,
        lv_qty_mbg   TYPE c LENGTH 15.
  DATA: lc_xml_hdr TYPE string,
        lc_wb_open TYPE string,
        lc_styles  TYPE string.
  lc_xml_hdr = '<?xml version="1.0" encoding="UTF-8"?><?mso-application progid="Excel.Sheet"?>'.
  CONCATENATE '<Workbook xmlns="urn:schemas-microsoft-com:office:spreadsheet"'
    ' xmlns:ss="urn:schemas-microsoft-com:office:spreadsheet"'
    ' xmlns:x="urn:schemas-microsoft-com:office:excel">'
    INTO lc_wb_open.
  CONCATENATE '<Styles>'
    '<Style ss:ID="hdr"><Font ss:Bold="1" ss:FontName="Times New Roman" ss:Size="11"/>'
    '<Alignment ss:Horizontal="Center" ss:Vertical="Center"/>'
    '<Borders><Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/></Borders></Style>'
    '<Style ss:ID="dat"><Font ss:FontName="Times New Roman" ss:Size="11"/>'
    '<Borders><Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/></Borders></Style>'
    '<Style ss:ID="num"><Font ss:FontName="Times New Roman" ss:Size="11"/>'
    '<NumberFormat ss:Format="0.000"/><Alignment ss:Horizontal="Right"/>'
    '<Borders><Border ss:Position="Bottom" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Left" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Right" ss:LineStyle="Continuous" ss:Weight="1"/>'
    '<Border ss:Position="Top" ss:LineStyle="Continuous" ss:Weight="1"/></Borders></Style>'
    '</Styles>'
    INTO lc_styles.
  " Build XML Spreadsheet
  CONCATENATE lc_xml_hdr lc_wb_open lc_styles
    '<Worksheet ss:Name="Fortnightly Data"><Table>'
    '<Column ss:AutoFitWidth="1" ss:Width="80"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="80"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="80"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="100"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="75"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="120"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="80"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="65"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="65"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="80"/>'
    '<Column ss:AutoFitWidth="1" ss:Width="100"/>'
    INTO lv_xml.
  " Header row
  CONCATENATE lv_xml
    '<Row ss:StyleID="hdr">'
    '<Cell><Data ss:Type="String">From</Data></Cell>'
    '<Cell><Data ss:Type="String">To</Data></Cell>'
    '<Cell><Data ss:Type="String">CTP ID</Data></Cell>'
    '<Cell><Data ss:Type="String">ONGC Material</Data></Cell>'
    '<Cell><Data ss:Type="String">State Code</Data></Cell>'
    '<Cell><Data ss:Type="String">State</Data></Cell>'
    '<Cell><Data ss:Type="String">Qty in SCM</Data></Cell>'
    '<Cell><Data ss:Type="String">GCV</Data></Cell>'
    '<Cell><Data ss:Type="String">NCV</Data></Cell>'
    '<Cell><Data ss:Type="String">Qty in MBG</Data></Cell>'
    '<Cell><Data ss:Type="String">GAIL ID</Data></Cell>'
    '</Row>'
    INTO lv_xml.
  " Sort data: From Date - To Date - CTP ID - State Code - ONGC Material
  " Use a typed local copy as pt_data is generic TYPE STANDARD TABLE
  DATA lt_fnt_sorted TYPE TABLE OF yrga_cst_fn_data.
  lt_fnt_sorted = pt_data.
  SORT lt_fnt_sorted BY date_from date_to ctp state_code ongc_mater ASCENDING.
  " Data rows
  LOOP AT lt_fnt_sorted INTO ls_fnt.
    WRITE ls_fnt-date_from TO lv_date_from DD/MM/YYYY.
    WRITE ls_fnt-date_to   TO lv_date_to   DD/MM/YYYY.
    WRITE ls_fnt-gcv TO lv_gcv DECIMALS 3.
    WRITE ls_fnt-ncv TO lv_ncv DECIMALS 3.
    WRITE ls_fnt-qty_in_scm TO lv_qty_scm DECIMALS 3.
    WRITE ls_fnt-qty_in_mbg TO lv_qty_mbg DECIMALS 3.
    CONDENSE: lv_date_from, lv_date_to, lv_gcv, lv_ncv, lv_qty_scm, lv_qty_mbg.
    CONCATENATE lv_xml
      '<Row ss:StyleID="dat">'
      '<Cell><Data ss:Type="String">' lv_date_from '</Data></Cell>'
      '<Cell><Data ss:Type="String">' lv_date_to '</Data></Cell>'
      '<Cell><Data ss:Type="String">' ls_fnt-ctp '</Data></Cell>'
      '<Cell><Data ss:Type="String">' ls_fnt-ongc_mater '</Data></Cell>'
      '<Cell><Data ss:Type="String">' ls_fnt-state_code '</Data></Cell>'
      '<Cell><Data ss:Type="String">' ls_fnt-state '</Data></Cell>'
      '<Cell ss:StyleID="num"><Data ss:Type="Number">' lv_qty_scm '</Data></Cell>'
      '<Cell ss:StyleID="num"><Data ss:Type="Number">' lv_gcv '</Data></Cell>'
      '<Cell ss:StyleID="num"><Data ss:Type="Number">' lv_ncv '</Data></Cell>'
      '<Cell ss:StyleID="num"><Data ss:Type="Number">' lv_qty_mbg '</Data></Cell>'
      '<Cell><Data ss:Type="String">' ls_fnt-gail_id '</Data></Cell>'
      '</Row>'
      INTO lv_xml.
  ENDLOOP.
  CONCATENATE lv_xml '</Table></Worksheet></Workbook>' INTO lv_xml.
  " Convert XML string to xstring then to solix_tab
  DATA(lo_conv) = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ).
  lo_conv->convert( EXPORTING data = lv_xml IMPORTING buffer = lv_xstring ).
  ct_content = cl_bcs_convert=>xstring_to_solix( lv_xstring ).
  cv_size = xstrlen( lv_xstring ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BUILD_FNT_PDF_ATTACHMENT
*& Build fortnightly PDF attachment using spool-to-PDF conversion
*&---------------------------------------------------------------------*
FORM build_fnt_pdf_attachment USING pt_data    TYPE STANDARD TABLE
                              CHANGING ct_content TYPE soli_tab
                                       cv_size    TYPE sood-objlen
                                       ct_pdf_raw TYPE STANDARD TABLE
                                       cv_pdf_len TYPE i.
  DATA: ls_fnt     TYPE yrga_cst_fn_data.
  DATA: lv_date_from TYPE c LENGTH 10,
        lv_date_to   TYPE c LENGTH 10,
        lv_gcv       TYPE c LENGTH 15,
        lv_ncv       TYPE c LENGTH 15,
        lv_qty_scm   TYPE c LENGTH 15,
        lv_qty_mbg   TYPE c LENGTH 15.
  DATA: lv_date_from_str TYPE c LENGTH 10,
        lv_date_to_str   TYPE c LENGTH 10.
  DATA: ls_params  TYPE pri_params,
        lv_valid   TYPE c,
        lv_spool   TYPE rspoid,
        lt_pdf     TYPE TABLE OF tline,
        lv_pdf_len TYPE i,
        lv_xstring TYPE xstring.
  DATA: lv_date_str TYPE c LENGTH 10,
        lv_time_str TYPE c LENGTH 8,
        lv_page     TYPE i VALUE 1,
        lv_page_str TYPE c LENGTH 5.
  " Daily data for date-wise detail section
  DATA: lt_daily     TYPE TABLE OF yrga_cst_pur,
        ls_daily     TYPE yrga_cst_pur,
        lv_gas_day   TYPE c LENGTH 10.
  " For column-wise pivot — fixed structure with 16 day fields
  TYPES: BEGIN OF ty_pivot,
           ctp   TYPE c LENGTH 12,
           mat   TYPE c LENGTH 12,
           stcd  TYPE c LENGTH 5,
           state TYPE c LENGTH 15,
           d01   TYPE c LENGTH 14, d02 TYPE c LENGTH 14,
           d03   TYPE c LENGTH 14, d04 TYPE c LENGTH 14,
           d05   TYPE c LENGTH 14, d06 TYPE c LENGTH 14,
           d07   TYPE c LENGTH 14, d08 TYPE c LENGTH 14,
           d09   TYPE c LENGTH 14, d10 TYPE c LENGTH 14,
           d11   TYPE c LENGTH 14, d12 TYPE c LENGTH 14,
           d13   TYPE c LENGTH 14, d14 TYPE c LENGTH 14,
           d15   TYPE c LENGTH 14, d16 TYPE c LENGTH 14,
         END OF ty_pivot.
  DATA: lt_days     TYPE TABLE OF datum,
        lv_curr_day TYPE datum,
        lv_day      TYPE datum,
        lv_day_c    TYPE c LENGTH 8,
        lv_day_dd   TYPE c LENGTH 2,
        lt_keys     TYPE TABLE OF yrga_cst_pur,
        ls_key      TYPE yrga_cst_pur,
        lv_num_days TYPE i,
        lv_day_idx  TYPE i,
        lv_idx_str  TYPE c LENGTH 2,
        lv_comp_nm  TYPE c LENGTH 4,
        lv_read_rc  TYPE i,
        lv_val14    TYPE c LENGTH 14,
        ls_hdr      TYPE ty_pivot,
        ls_piv_mbg  TYPE ty_pivot,
        ls_piv_scm  TYPE ty_pivot.
  FIELD-SYMBOLS: <fs_cell> TYPE c.

  WRITE gv_date_from TO lv_date_from_str DD/MM/YYYY.
  WRITE gv_date_to   TO lv_date_to_str   DD/MM/YYYY.
  " Download date/time
  WRITE sy-datum TO lv_date_str DD/MM/YYYY.
  WRITE sy-uzeit TO lv_time_str USING EDIT MASK '__:__:__'.

  " Fetch daily records
  SELECT * FROM yrga_cst_pur
    INTO TABLE lt_daily
    WHERE gas_day  BETWEEN gv_date_from AND gv_date_to
      AND location IN s_loc
      AND exclude  <> 'X'
      AND deleted  =  ' '.
  SORT lt_daily BY ctp ongc_mater state_code gas_day ASCENDING.

  " Build ordered list of days in the period
  lv_curr_day = gv_date_from.
  WHILE lv_curr_day <= gv_date_to.
    APPEND lv_curr_day TO lt_days.
    lv_curr_day = lv_curr_day + 1.
  ENDWHILE.
  DESCRIBE TABLE lt_days LINES lv_num_days.

  " Collect unique CTP / Material / State combinations
  LOOP AT lt_daily INTO ls_daily.
    READ TABLE lt_keys WITH KEY ctp        = ls_daily-ctp
                                ongc_mater = ls_daily-ongc_mater
                                state_code = ls_daily-state_code
                       TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      ls_key-ctp        = ls_daily-ctp.
      ls_key-ongc_mater = ls_daily-ongc_mater.
      ls_key-state_code = ls_daily-state_code.
      ls_key-state      = ls_daily-state.
      APPEND ls_key TO lt_keys.
    ENDIF.
  ENDLOOP.

  " Build header pivot row (day DD values as column headers)
  CLEAR ls_hdr.
  ls_hdr-ctp   = 'CTP ID'.
  ls_hdr-mat   = 'ONGC Material'.
  ls_hdr-stcd  = 'St.Cd'.
  ls_hdr-state = 'State'.
  lv_day_idx = 1.
  LOOP AT lt_days INTO lv_day.
    lv_day_c = lv_day. lv_day_dd = lv_day_c+6(2).
    WRITE lv_day_idx TO lv_idx_str RIGHT-JUSTIFIED.
    CONDENSE lv_idx_str.
    IF lv_day_idx < 10.
      CONCATENATE 'D0' lv_idx_str INTO lv_comp_nm.
    ELSE.
      CONCATENATE 'D'  lv_idx_str INTO lv_comp_nm.
    ENDIF.
    ASSIGN COMPONENT lv_comp_nm OF STRUCTURE ls_hdr TO <fs_cell>.
    IF sy-subrc = 0. <fs_cell> = lv_day_dd. ENDIF.
    lv_day_idx = lv_day_idx + 1.
  ENDLOOP.

  " Get print parameters — wider line for column layout
  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      no_dialog      = 'X'
      immediately    = ' '
      release        = ' '
      new_list_id    = 'X'
      line_size      = 280
      line_count     = 65
    IMPORTING
      out_parameters = ls_params
      valid          = lv_valid
    EXCEPTIONS
      OTHERS         = 1.

  IF lv_valid <> 'X'.
    RETURN.
  ENDIF.

  NEW-PAGE PRINT ON PARAMETERS ls_params NO DIALOG.

  " ---- Page 1: Column-wise Qty MBG (each date = one column) ----
  lv_page_str = lv_page.
  CONDENSE lv_page_str.
  WRITE: /5 'Downloaded', lv_date_str, AT 260 lv_time_str.
  WRITE: /120 'ONGC CST Statewise Allocation', AT 265 lv_page_str.
  WRITE: /5 'Daily Qty MBG -', lv_date_from_str, 'to', lv_date_to_str.
  SKIP 1.
  ULINE AT /5(275).
  FORMAT INTENSIFIED ON.
  WRITE: /5(12) ls_hdr-ctp,   18(12) ls_hdr-mat,
          31(5) ls_hdr-stcd,  37(15) ls_hdr-state,
          53(14) ls_hdr-d01,  67(14) ls_hdr-d02,
          81(14) ls_hdr-d03,  95(14) ls_hdr-d04,
          109(14) ls_hdr-d05, 123(14) ls_hdr-d06,
          137(14) ls_hdr-d07, 151(14) ls_hdr-d08,
          165(14) ls_hdr-d09, 179(14) ls_hdr-d10,
          193(14) ls_hdr-d11, 207(14) ls_hdr-d12,
          221(14) ls_hdr-d13, 235(14) ls_hdr-d14,
          249(14) ls_hdr-d15, 263(14) ls_hdr-d16.
  FORMAT INTENSIFIED OFF.
  ULINE AT /5(275).

  " Build and write one MBG pivot row per CTP/Material/State
  LOOP AT lt_keys INTO ls_key.
    CLEAR ls_piv_mbg.
    ls_piv_mbg-ctp   = ls_key-ctp.
    ls_piv_mbg-mat   = ls_key-ongc_mater.
    ls_piv_mbg-stcd  = ls_key-state_code.
    ls_piv_mbg-state = ls_key-state.
    lv_day_idx = 1.
    LOOP AT lt_days INTO lv_day.
      READ TABLE lt_daily INTO ls_daily
        WITH KEY ctp        = ls_key-ctp
                 ongc_mater = ls_key-ongc_mater
                 state_code = ls_key-state_code
                 gas_day    = lv_day.
      lv_read_rc = sy-subrc.
      WRITE lv_day_idx TO lv_idx_str RIGHT-JUSTIFIED.
      CONDENSE lv_idx_str.
      IF lv_day_idx < 10.
        CONCATENATE 'D0' lv_idx_str INTO lv_comp_nm.
      ELSE.
        CONCATENATE 'D'  lv_idx_str INTO lv_comp_nm.
      ENDIF.
      ASSIGN COMPONENT lv_comp_nm OF STRUCTURE ls_piv_mbg TO <fs_cell>.
      IF sy-subrc = 0.
        IF lv_read_rc = 0.
          WRITE ls_daily-qty_in_mbg TO lv_val14 DECIMALS 3.
          CONDENSE lv_val14.
          <fs_cell> = lv_val14.
        ELSE.
          <fs_cell> = ''.
        ENDIF.
      ENDIF.
      lv_day_idx = lv_day_idx + 1.
    ENDLOOP.
    WRITE: /5(12) ls_piv_mbg-ctp,   18(12) ls_piv_mbg-mat,
            31(5) ls_piv_mbg-stcd,  37(15) ls_piv_mbg-state,
            53(14) ls_piv_mbg-d01,  67(14) ls_piv_mbg-d02,
            81(14) ls_piv_mbg-d03,  95(14) ls_piv_mbg-d04,
            109(14) ls_piv_mbg-d05, 123(14) ls_piv_mbg-d06,
            137(14) ls_piv_mbg-d07, 151(14) ls_piv_mbg-d08,
            165(14) ls_piv_mbg-d09, 179(14) ls_piv_mbg-d10,
            193(14) ls_piv_mbg-d11, 207(14) ls_piv_mbg-d12,
            221(14) ls_piv_mbg-d13, 235(14) ls_piv_mbg-d14,
            249(14) ls_piv_mbg-d15, 263(14) ls_piv_mbg-d16.
    ULINE AT /5(275).
  ENDLOOP.

  " ---- Page 2: Column-wise Qty SCM ----
  lv_page = lv_page + 1.
  NEW-PAGE.
  lv_page_str = lv_page.
  CONDENSE lv_page_str.
  WRITE: /5 'Downloaded', lv_date_str, AT 260 lv_time_str.
  WRITE: /120 'ONGC CST Statewise Allocation', AT 265 lv_page_str.
  WRITE: /5 'Daily Qty SCM -', lv_date_from_str, 'to', lv_date_to_str.
  SKIP 1.
  ULINE AT /5(275).
  FORMAT INTENSIFIED ON.
  WRITE: /5(12) ls_hdr-ctp,   18(12) ls_hdr-mat,
          31(5) ls_hdr-stcd,  37(15) ls_hdr-state,
          53(14) ls_hdr-d01,  67(14) ls_hdr-d02,
          81(14) ls_hdr-d03,  95(14) ls_hdr-d04,
          109(14) ls_hdr-d05, 123(14) ls_hdr-d06,
          137(14) ls_hdr-d07, 151(14) ls_hdr-d08,
          165(14) ls_hdr-d09, 179(14) ls_hdr-d10,
          193(14) ls_hdr-d11, 207(14) ls_hdr-d12,
          221(14) ls_hdr-d13, 235(14) ls_hdr-d14,
          249(14) ls_hdr-d15, 263(14) ls_hdr-d16.
  FORMAT INTENSIFIED OFF.
  ULINE AT /5(275).

  " Build and write one SCM pivot row per CTP/Material/State
  LOOP AT lt_keys INTO ls_key.
    CLEAR ls_piv_scm.
    ls_piv_scm-ctp   = ls_key-ctp.
    ls_piv_scm-mat   = ls_key-ongc_mater.
    ls_piv_scm-stcd  = ls_key-state_code.
    ls_piv_scm-state = ls_key-state.
    lv_day_idx = 1.
    LOOP AT lt_days INTO lv_day.
      READ TABLE lt_daily INTO ls_daily
        WITH KEY ctp        = ls_key-ctp
                 ongc_mater = ls_key-ongc_mater
                 state_code = ls_key-state_code
                 gas_day    = lv_day.
      lv_read_rc = sy-subrc.
      WRITE lv_day_idx TO lv_idx_str RIGHT-JUSTIFIED.
      CONDENSE lv_idx_str.
      IF lv_day_idx < 10.
        CONCATENATE 'D0' lv_idx_str INTO lv_comp_nm.
      ELSE.
        CONCATENATE 'D'  lv_idx_str INTO lv_comp_nm.
      ENDIF.
      ASSIGN COMPONENT lv_comp_nm OF STRUCTURE ls_piv_scm TO <fs_cell>.
      IF sy-subrc = 0.
        IF lv_read_rc = 0.
          WRITE ls_daily-qty_in_scm TO lv_val14 DECIMALS 3.
          CONDENSE lv_val14.
          <fs_cell> = lv_val14.
        ELSE.
          <fs_cell> = ''.
        ENDIF.
      ENDIF.
      lv_day_idx = lv_day_idx + 1.
    ENDLOOP.
    WRITE: /5(12) ls_piv_scm-ctp,   18(12) ls_piv_scm-mat,
            31(5) ls_piv_scm-stcd,  37(15) ls_piv_scm-state,
            53(14) ls_piv_scm-d01,  67(14) ls_piv_scm-d02,
            81(14) ls_piv_scm-d03,  95(14) ls_piv_scm-d04,
            109(14) ls_piv_scm-d05, 123(14) ls_piv_scm-d06,
            137(14) ls_piv_scm-d07, 151(14) ls_piv_scm-d08,
            165(14) ls_piv_scm-d09, 179(14) ls_piv_scm-d10,
            193(14) ls_piv_scm-d11, 207(14) ls_piv_scm-d12,
            221(14) ls_piv_scm-d13, 235(14) ls_piv_scm-d14,
            249(14) ls_piv_scm-d15, 263(14) ls_piv_scm-d16.
    ULINE AT /5(275).
    ULINE AT /5(245).
  ENDLOOP.

  " ---- Fortnightly Summary Page ----
  lv_page = lv_page + 1.
  NEW-PAGE.
  lv_page_str = lv_page.
  CONDENSE lv_page_str.
  WRITE: /5 'Downloaded', lv_date_str, AT 180 lv_time_str.
  WRITE: /75 'ONGC CST Statewise Allocation', AT 185 lv_page_str.
  WRITE: /5 'Fortnightly CST Purchase Data -',
           lv_date_from_str, 'to', lv_date_to_str.
  SKIP 1.
  ULINE AT /5(170).
  FORMAT INTENSIFIED ON.
  WRITE: /5(10) 'From',
          16(10) 'To',
          27(12) 'CTP ID',
          40(15) 'ONGC Material',
          56(8)  'State Cd',
          65(20) 'State',
          86(15) 'Qty in SCM',
          102(12) 'GCV',
          115(12) 'NCV',
          128(15) 'Qty in MBG',
          144(14) 'GAIL ID'.
  FORMAT INTENSIFIED OFF.
  ULINE AT /5(170).

  " Sort data: From Date - To Date - CTP ID - State Code - ONGC Material
  DATA lt_fnt_sorted TYPE TABLE OF yrga_cst_fn_data.
  lt_fnt_sorted = pt_data.
  SORT lt_fnt_sorted BY date_from date_to ctp state_code ongc_mater ASCENDING.
  LOOP AT lt_fnt_sorted INTO ls_fnt.
    WRITE ls_fnt-date_from TO lv_date_from DD/MM/YYYY.
    WRITE ls_fnt-date_to   TO lv_date_to   DD/MM/YYYY.
    WRITE ls_fnt-qty_in_scm TO lv_qty_scm DECIMALS 3.
    WRITE ls_fnt-gcv TO lv_gcv DECIMALS 3.
    WRITE ls_fnt-ncv TO lv_ncv DECIMALS 3.
    WRITE ls_fnt-qty_in_mbg TO lv_qty_mbg DECIMALS 3.
    CONDENSE: lv_date_from, lv_date_to, lv_qty_scm, lv_gcv, lv_ncv, lv_qty_mbg.
    WRITE: /5(10) lv_date_from,
            16(10) lv_date_to,
            27(12) ls_fnt-ctp,
            40(15) ls_fnt-ongc_mater,
            56(8)  ls_fnt-state_code,
            65(20) ls_fnt-state,
            86(15) lv_qty_scm,
            102(12) lv_gcv,
            115(12) lv_ncv,
            128(15) lv_qty_mbg,
            144(14) ls_fnt-gail_id.
    ULINE AT /5(170).
  ENDLOOP.

  NEW-PAGE PRINT OFF.

  " Get the spool request ID (most recent for current user)
  SELECT rqident FROM tsp01 UP TO 1 ROWS
    INTO lv_spool
    WHERE rqowner  = sy-uname
      AND rqclient = sy-mandt
    ORDER BY rqident DESCENDING.
  ENDSELECT.

  IF sy-subrc <> 0 OR lv_spool IS INITIAL.
    RETURN.
  ENDIF.

  " Convert spool to PDF
  CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
    EXPORTING
      src_spoolid   = lv_spool
      no_dialog     = 'X'
    IMPORTING
      pdf_bytecount = lv_pdf_len
    TABLES
      pdf           = lt_pdf
    EXCEPTIONS
      OTHERS        = 1.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  " Convert tline table (134 bytes/line) to soli (255 bytes/line) for email
  CALL FUNCTION 'SX_TABLE_LINE_WIDTH_CHANGE'
    EXPORTING
      line_width_src = 134
      line_width_dst = 255
      transfer_bin   = 'X'
    TABLES
      content_in     = lt_pdf
      content_out    = ct_content
    EXCEPTIONS
      OTHERS         = 1.

  cv_size = lv_pdf_len.

  " Also return raw tline table for binary download (avoids Unicode dump)
  ct_pdf_raw[] = lt_pdf[].
  cv_pdf_len   = lv_pdf_len.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form HANDLE_SEND_B2B
*& 1.1.2: Send data through B2B PI connectivity
*&---------------------------------------------------------------------*
FORM handle_send_b2b.
  " Fetch data from YRGA_CST_PUR where EXCLUDED flag is not X
  SELECT * FROM yrga_cst_pur
    INTO TABLE lt_send_data
    WHERE gas_day BETWEEN gv_date_from AND gv_date_to
      AND location IN s_loc
      AND exclude <> 'X' AND deleted = ' '.
  SELECT * FROM yrga_cst_fn_data
  INTO TABLE lt_send_data_fn
  WHERE date_from = gv_date_from AND date_to = gv_date_to
    AND location IN s_loc AND deleted = ' '.
  IF lt_send_data IS INITIAL .
    MESSAGE s000(ygms_msg) WITH 'No data found to send for the selected period'.
    RETURN.
  ELSE.
    IF lt_send_data[] IS NOT INITIAL.
      CLEAR: lv_json_download.
      LOOP AT lt_send_data INTO DATA(w_fin).
        wa_final_daily-ctp = w_fin-ctp.
        wa_final_daily-gail_id = w_fin-gail_id.
        wa_final_daily-gas_day = w_fin-gas_day.
        wa_final_daily-gcv = w_fin-gcv.
        wa_final_daily-ncv = w_fin-ncv.
        wa_final_daily-ongc_id = w_fin-ongc_id.
        wa_final_daily-ongc_material = w_fin-ongc_mater.
        wa_final_daily-qty_in_mbg = w_fin-qty_in_mbg .
        wa_final_daily-qty_in_scm = w_fin-qty_in_scm .
        wa_final_daily-state = w_fin-state .
        wa_final_daily-state_code = w_fin-state_code .
        APPEND wa_final_daily TO ty_final_daily-record.
        CLEAR:  wa_final_daily, w_fin.
      ENDLOOP.
      lv_json_download = /ui2/cl_json=>serialize(
        data        = ty_final_daily
        compress    = abap_true
        pretty_name = /ui2/cl_json=>pretty_mode-none ).
      SELECT SINGLE * FROM yha_api_dtls INTO @lv_api_dt WHERE sysid = @sy-sysid AND yy_api_type = 'ONGC_B2B_DAILY'.
      PERFORM call_api.
    ENDIF.
    if g_error_api is INITIAL.
    IF lt_send_data_fn[] IS NOT INITIAL.
      CLEAR: lv_json_download.
      LOOP AT lt_send_data_fn INTO DATA(w_fin_fn).
        wa_final_fn-ctp = w_fin_fn-ctp.
        wa_final_fn-gail_id = w_fin_fn-gail_id.
        wa_final_fn-date_from = w_fin_fn-date_from.
        wa_final_fn-date_to = w_fin_fn-date_to.
        wa_final_fn-gcv = w_fin_fn-gcv.
        wa_final_fn-ncv = w_fin_fn-ncv.
        wa_final_fn-ongc_material = w_fin_fn-ongc_mater.
        wa_final_fn-qty_in_mbg = w_fin_fn-qty_in_mbg .
        wa_final_fn-qty_in_scm = w_fin_fn-qty_in_scm .
        wa_final_fn-state = w_fin_fn-state .
        wa_final_fn-state_code = w_fin_fn-state_code .
        APPEND wa_final_fn TO ty_final_fn-record.
        CLEAR:  wa_final_fn, w_fin.
      ENDLOOP.
      lv_json_download = /ui2/cl_json=>serialize(
        data        = ty_final_fn
        compress    = abap_true
        pretty_name = /ui2/cl_json=>pretty_mode-none ).
      SELECT SINGLE * FROM yha_api_dtls INTO @lv_api_dt WHERE sysid = @sy-sysid AND yy_api_type = 'ONGC_B2B_FN'.
      PERFORM call_api.
    ENDIF.
  ENDIF.
  if g_error_api is INITIAL.
  " After successful API call, save sent data to B2B tables
  PERFORM save_b2b_sent_data USING lt_send_data lt_send_data_fn.
  " Send confirmation email to the sending user
  PERFORM send_b2b_confirmation_email USING lt_send_data.
  " B2B PI connectivity - to be implemented when B2B connection is established
  " MESSAGE s000(ygms_msg) WITH 'B2B connectivity not yet established. Please use Email.'.
  endif.
  endif.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SEND_B2B_CONFIRMATION_EMAIL
*& Send confirmation email to the user who triggered the B2B send
*&---------------------------------------------------------------------*
FORM send_b2b_confirmation_email USING pt_daily TYPE STANDARD TABLE.
  DATA: lo_send_request TYPE REF TO cl_bcs,
        lo_document     TYPE REF TO cl_document_bcs,
        lo_recipient    TYPE REF TO if_recipient_bcs,
        lo_sender       TYPE REF TO cl_sapuser_bcs,
        lt_body         TYPE bcsy_text,
        ls_body         TYPE soli,
        lv_subject      TYPE so_obj_des,
        lx_bcs          TYPE REF TO cx_bcs.
  DATA: lv_user_email TYPE adr6-smtp_addr,
        l_mail        TYPE adr6-smtp_addr.
  DATA: lv_date_from_str TYPE c LENGTH 10,
        lv_date_to_str   TYPE c LENGTH 10.
  DATA: ls_pur          TYPE yrga_cst_pur,
        lt_loc          TYPE TABLE OF ygms_de_loc_id,
        lv_loc          TYPE ygms_de_loc_id,
        lv_loc_list     TYPE string,
        lv_loc_trimmed  TYPE string.
  DATA: lv_sent_on_str TYPE c LENGTH 10,
        lv_sent_at_str TYPE c LENGTH 8.

  " Look up sender email from PA0105
  SELECT SINGLE usrid INTO @lv_user_email
    FROM pa0105
    WHERE pernr = @sy-uname
      AND subty = 'E-ML'
      AND endda = '99991231'.
  IF sy-subrc <> 0 OR lv_user_email IS INITIAL.
    RETURN.
  ENDIF.

  " Format date/time values
  WRITE gv_date_from TO lv_date_from_str DD/MM/YYYY.
  WRITE gv_date_to   TO lv_date_to_str   DD/MM/YYYY.
  WRITE sy-datum TO lv_sent_on_str DD/MM/YYYY.
  WRITE sy-uzeit TO lv_sent_at_str USING EDIT MASK '__:__:__'.

  " Build unique location ID list from daily data
  LOOP AT pt_daily INTO ls_pur.
    READ TABLE lt_loc WITH KEY table_line = ls_pur-location TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND ls_pur-location TO lt_loc.
    ENDIF.
  ENDLOOP.
  SORT lt_loc.
  LOOP AT lt_loc INTO lv_loc.
    lv_loc_trimmed = lv_loc.
    CONDENSE lv_loc_trimmed.
    IF lv_loc_list IS INITIAL.
      lv_loc_list = lv_loc_trimmed.
    ELSE.
      lv_loc_list = |{ lv_loc_list }, { lv_loc_trimmed }|.
    ENDIF.
  ENDLOOP.

  " Trim date/time strings before embedding in body lines
  DATA: lv_dfrom TYPE string,
        lv_dto   TYPE string,
        lv_son   TYPE string,
        lv_sat   TYPE string.
  lv_dfrom = lv_date_from_str. CONDENSE lv_dfrom.
  lv_dto   = lv_date_to_str.   CONDENSE lv_dto.
  lv_son   = lv_sent_on_str.   CONDENSE lv_son.
  lv_sat   = lv_sent_at_str.   CONDENSE lv_sat.

  " Build email subject
  lv_subject = |Transmission Confirmation - ONGC CST B2B Data for { lv_dfrom } to { lv_dto }|.

  " Build email body
  ls_body-line = 'Statewise allocation data for CST purchase has been sent to ONGC through B2B as per the following details:'.
  APPEND ls_body TO lt_body.
  CLEAR ls_body.
  APPEND ls_body TO lt_body.  " blank line
  ls_body-line = |Fortnight: { lv_dfrom } to { lv_dto }|.
  APPEND ls_body TO lt_body.
  CLEAR ls_body.
  ls_body-line = |Location IDs: { lv_loc_list }|.
  APPEND ls_body TO lt_body.
  CLEAR ls_body.
  ls_body-line = |Sent On: { lv_son }|.
  APPEND ls_body TO lt_body.
  CLEAR ls_body.
  ls_body-line = |Sent At: { lv_sat }|.
  APPEND ls_body TO lt_body.

  TRY.
      lo_send_request = cl_bcs=>create_persistent( ).
      lo_document = cl_document_bcs=>create_document(
        i_type    = 'RAW'
        i_text    = lt_body
        i_subject = lv_subject ).
      lo_send_request->set_document( lo_document ).
      l_mail = lv_user_email.
      lo_recipient = cl_cam_address_bcs=>create_internet_address( l_mail ).
      lo_send_request->add_recipient( lo_recipient ).
      lo_sender = cl_sapuser_bcs=>create( sy-uname ).
      lo_send_request->set_sender( lo_sender ).
      lo_send_request->set_send_immediately( abap_true ).
      lo_send_request->send( ).
      COMMIT WORK.
    CATCH cx_bcs INTO lx_bcs.
      " Silently ignore email errors for confirmation mail
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
FORM call_api.
  clear g_error_api.
****  Added by Aishwarya/ Abhisheik for ONGC b2b API 03.03.2026
  IF sy-subrc = 0.
    lv_token_url     = lv_api_dt-yy_token_url.
    lv_api_get     = lv_api_dt-yy_api_url.
    lv_client_id     = lv_api_dt-yy_client_id.
    lv_client_secret = lv_api_dt-yy_client_secret.
    lv_proxy_host    = 'proxy'.
    lv_proxy_service = '3128'.
  ENDIF.
  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = lv_token_url
      proxy_host         = lv_proxy_host
      proxy_service      = lv_proxy_service
    IMPORTING
      client             = lo_http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
    WRITE: / 'Error creating HTTP client for token request'.
    g_error_api = 'X'.
    RETURN.
  ENDIF.
  lo_http_client->propertytype_logon_popup = lo_http_client->co_disabled.
  lo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/x-www-form-urlencoded' ).
  lo_http_client->request->set_cdata( |grant_type=client_credentials&client_id={ lv_client_id }&client_secret={ lv_client_secret }| ).
  CALL METHOD lo_http_client->send
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.
  IF sy-subrc <> 0.
    WRITE: / 'Error sending token request'.
     g_error_api = 'X'.
    RETURN.
  ENDIF.
* Receive token response
  CALL METHOD lo_http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.
  IF sy-subrc <> 0.
    WRITE: / 'Error receiving token response'.
     g_error_api = 'X'.
    RETURN.
  ENDIF.
  lv_response = lo_http_client->response->get_cdata( ).
  lo_http_client->close( ).
***    * Extract access token from response (assuming JSON response)
  DATA: lt_json       TYPE TABLE OF string,
        lv_token_json TYPE string.
  SPLIT lv_response AT ',' INTO TABLE lt_json.
  LOOP AT lt_json INTO lv_token_json.
    IF lv_token_json CS '"access_token"'.
      REPLACE ALL OCCURRENCES OF '"' IN lv_token_json WITH ''.
      REPLACE ALL OCCURRENCES OF 'access_token:' IN lv_token_json WITH ''.
      lv_access_token = lv_token_json.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF lv_access_token IS NOT INITIAL.
    DATA: "lv_http_client TYPE REF TO if_http_client,
           "lv_response    TYPE string,
           lt_headers     TYPE tihttpnvp.
    " Create HTTP client for the Vendor data request
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url           = lv_api_get
        proxy_host    = lv_proxy_host
        proxy_service = lv_proxy_service
      IMPORTING
        client        = lv_http_client.
    lv_http_client->request->set_method( 'POST' ).
    lv_http_client->request->set_header_field( name = 'Authorization' value = |Bearer { lv_access_token }| ). "name = 'Content-Type' value = 'application/x-www-form-urlencoded' ).
    lv_http_client->request->set_header_field( name = 'Accept' value = 'application/json' ). "name = 'Content-Type' value = 'application/x-www-form-urlencoded' ).
*  lv_http_client->request->set_cdata( |grant_type=client_credentials&client_id={ lv_client_id }&client_secret={ lv_client_secret }| ).
    lv_http_client->request->set_cdata( lv_json_download ).
    lv_http_client->send( EXCEPTIONS http_communication_failure = 1
                                     http_invalid_state         = 2 ).
    lv_http_client->receive( EXCEPTIONS http_communication_failure = 1
                                        http_invalid_state         = 2
                                        http_processing_failed     = 3 ).
    lv_http_client->response->get_status(
      IMPORTING
        code   = code
        reason = reason ).
    lv_response = lv_http_client->response->get_cdata( ).
    log_data =   lv_response.
    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = log_data
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = itab ).
    lv_http_client->close( ).
  ELSE.
    WRITE:/ 'NO DATA TO DOWNLOAD.'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_SEND_PREVIEW
*& 1.2.4: Display data preview with toggle (daily/fortnightly) + Send button
*&---------------------------------------------------------------------*
FORM display_send_preview.
  " Type for daily send data (includes GAIL Location/Material for preview only)
  TYPES: BEGIN OF ty_send_daily,
           gas_day       TYPE datum,
           ctp_id        TYPE ygms_de_ongc_ctp,
           gail_loc_id   TYPE ygms_de_loc_id,
           gail_material TYPE ygms_de_gail_mat,
           ongc_material TYPE ygms_de_ongc_mat,
           state_code    TYPE regio,
           state         TYPE bezei20,
           qty_in_scm    TYPE ygms_de_qty_scm,
           gcv           TYPE ygms_de_gcv,
           ncv           TYPE ygms_de_ncv,
           qty_in_mbg    TYPE ygms_de_qty_mbg_cal,
           ongc_id       TYPE ygms_de_ongc_id,
           gail_id       TYPE c LENGTH 14,
         END OF ty_send_daily.
  " Type for fortnightly send data (flat, per YRGA_CST_FN_DATA structure)
  TYPES: BEGIN OF ty_send_fnt,
           date_from     TYPE datum,
           date_to       TYPE datum,
           ctp_id        TYPE ygms_de_ongc_ctp,
           ongc_mater    TYPE ygms_de_ongc_mat,
           gail_loc_id   TYPE ygms_de_loc_id,
           gail_material TYPE ygms_de_gail_mat,
           state_code    TYPE regio,
           state         TYPE bezei20,
           qty_in_scm    TYPE p DECIMALS 6,
           gcv           TYPE p DECIMALS 6,
           ncv           TYPE p DECIMALS 6,
           qty_in_mbg    TYPE p DECIMALS 6,
           gail_id       TYPE c LENGTH 14,
         END OF ty_send_fnt.

  DATA: lt_daily_data TYPE TABLE OF ty_send_daily,
        lt_fnt_data   TYPE TABLE OF ty_send_fnt,
        lt_cst_pur    TYPE TABLE OF yrga_cst_pur,
        lt_cst_fnt    TYPE TABLE OF yrga_cst_fn_data,
        lv_answer     TYPE c LENGTH 1.

  " Fetch daily data from YRGA_CST_PUR where EXCLUDED flag is not X
  SELECT * FROM yrga_cst_pur
    INTO TABLE lt_cst_pur
    WHERE gas_day BETWEEN gv_date_from AND gv_date_to
      AND location IN s_loc
      AND exclude <> 'X' AND deleted = ' '.

  IF lt_cst_pur IS INITIAL.
    MESSAGE s000(ygms_msg) WITH 'No data found to send for the selected period'.
    RETURN.
  ENDIF.

  " Build daily send data (with GAIL Location/Material for preview)
  LOOP AT lt_cst_pur INTO DATA(ls_pur).
    DATA(ls_daily) = VALUE ty_send_daily(
      gas_day       = ls_pur-gas_day
      ctp_id        = ls_pur-ctp
      gail_loc_id   = ls_pur-location
      gail_material = ls_pur-material
      ongc_material = ls_pur-ongc_mater
      state_code    = ls_pur-state_code
      state         = ls_pur-state
      qty_in_scm    = ls_pur-qty_in_scm
      gcv           = ls_pur-gcv
      ncv           = ls_pur-ncv
      qty_in_mbg    = ls_pur-qty_in_mbg
      ongc_id       = ls_pur-ongc_id
      gail_id       = ls_pur-gail_id
    ).
    APPEND ls_daily TO lt_daily_data.
  ENDLOOP.

  " Fetch fortnightly data from YRGA_CST_FN_DATA
  SELECT * FROM yrga_cst_fn_data
    INTO TABLE lt_cst_fnt
    WHERE date_from = gv_date_from
      AND date_to   = gv_date_to
      AND location  IN s_loc AND deleted = ' '.

  " Build fortnightly preview data
  LOOP AT lt_cst_fnt INTO DATA(ls_fnt_db).
    DATA ls_fnt TYPE ty_send_fnt.
    CLEAR ls_fnt.
    ls_fnt-date_from     = ls_fnt_db-date_from.
    ls_fnt-date_to       = ls_fnt_db-date_to.
    ls_fnt-ctp_id        = ls_fnt_db-ctp.
    ls_fnt-ongc_mater    = ls_fnt_db-ongc_mater.
    ls_fnt-gail_loc_id   = ls_fnt_db-location.
    ls_fnt-gail_material = ls_fnt_db-material.
    ls_fnt-state_code    = ls_fnt_db-state_code.
    ls_fnt-state         = ls_fnt_db-state.
    ls_fnt-qty_in_scm    = ls_fnt_db-qty_in_scm.
    ls_fnt-gcv           = ls_fnt_db-gcv.
    ls_fnt-ncv           = ls_fnt_db-ncv.
    ls_fnt-qty_in_mbg    = ls_fnt_db-qty_in_mbg.
    ls_fnt-gail_id       = ls_fnt_db-gail_id.
    APPEND ls_fnt TO lt_fnt_data.
  ENDLOOP.

  " Main popup: View Data or Send to ONGC
  DO.
    CALL FUNCTION 'POPUP_TO_DECIDE'
      EXPORTING
        defaultoption  = '1'
        textline1      = 'Click to view Fortnightly / Daily data or'
        textline2      = 'Click to proceed to send data to ONGC.'
        text_option1   = 'View Data'
        text_option2   = 'Send Data to ONGC'
        titel          = 'CST Purchase Data'
        cancel_display = 'X'
      IMPORTING
        answer         = lv_answer.

    CASE lv_answer.
      WHEN '1'.
        " Sub-popup: Choose Daily or Fortnightly data to view
        DATA lv_view_answer TYPE c LENGTH 1.
        CALL FUNCTION 'POPUP_TO_DECIDE'
          EXPORTING
            defaultoption  = '1'
            textline1      = 'Select which data to view.'
            textline2      = ' '
            text_option1   = 'Daily Data'
            text_option2   = 'Fortnightly Data'
            titel          = 'View Data'
            cancel_display = 'X'
          IMPORTING
            answer         = lv_view_answer.
        CASE lv_view_answer.
          WHEN '1'.
            PERFORM display_daily_preview USING lt_daily_data.
          WHEN '2'.
            PERFORM display_fnt_preview USING lt_fnt_data.
          WHEN OTHERS.
            " Cancel - go back to main popup
        ENDCASE.
      WHEN '2'.
        " Proceed to send - go directly to Email/B2B selection
        EXIT.
      WHEN OTHERS.
        " Cancel - exit entirely
        MESSAGE s000(ygms_msg) WITH 'Send operation cancelled'.
        RETURN.
    ENDCASE.
  ENDDO.

  " Show send mode selection (Email or B2B)
  PERFORM show_send_mode_popup.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_DAILY_PREVIEW
*& Display daily send data in ALV popup
*&---------------------------------------------------------------------*
FORM display_daily_preview USING pt_daily TYPE STANDARD TABLE.
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat TYPE slis_fieldcat_alv.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GAS_DAY'.
  ls_fieldcat-seltext_l = 'Gas Day'.
  ls_fieldcat-col_pos   = 1.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'CTP_ID'.
  ls_fieldcat-seltext_l = 'CTP ID'.
  ls_fieldcat-col_pos   = 2.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GAIL_LOC_ID'.
  ls_fieldcat-seltext_l = 'GAIL Location ID'.
  ls_fieldcat-col_pos   = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GAIL_MATERIAL'.
  ls_fieldcat-seltext_l = 'GAIL Material'.
  ls_fieldcat-col_pos   = 4.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ONGC_MATERIAL'.
  ls_fieldcat-seltext_l = 'ONGC Material'.
  ls_fieldcat-col_pos   = 5.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'STATE_CODE'.
  ls_fieldcat-seltext_l = 'State Code'.
  ls_fieldcat-col_pos   = 6.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'STATE'.
  ls_fieldcat-seltext_l = 'State'.
  ls_fieldcat-col_pos   = 7.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'QTY_IN_SCM'.
  ls_fieldcat-seltext_l = 'Qty in SCM'.
  ls_fieldcat-col_pos   = 8.
  ls_fieldcat-do_sum    = abap_true.
  ls_fieldcat-decimals_out = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GCV'.
  ls_fieldcat-seltext_l = 'GCV'.
  ls_fieldcat-col_pos   = 9.
  ls_fieldcat-decimals_out = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NCV'.
  ls_fieldcat-seltext_l = 'NCV'.
  ls_fieldcat-col_pos   = 10.
  ls_fieldcat-decimals_out = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'QTY_IN_MBG'.
  ls_fieldcat-seltext_l = 'Qty in MBG'.
  ls_fieldcat-col_pos   = 11.
  ls_fieldcat-do_sum    = abap_true.
  ls_fieldcat-decimals_out = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ONGC_ID'.
  ls_fieldcat-seltext_l = 'ONGC ID'.
  ls_fieldcat-col_pos   = 12.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GAIL_ID'.
  ls_fieldcat-seltext_l = 'GAIL ID'.
  ls_fieldcat-col_pos   = 13.
  ls_fieldcat-outputlen = 20.
  APPEND ls_fieldcat TO lt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title               = 'Daily Data to be Sent to ONGC'
      i_selection           = ' '
      i_zebra               = abap_true
      i_screen_start_column = 2
      i_screen_start_line   = 3
      i_screen_end_column   = 160
      i_screen_end_line     = 30
      i_tabname             = 'PT_DAILY'
      it_fieldcat           = lt_fieldcat
    TABLES
      t_outtab              = pt_daily
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_FNT_PREVIEW
*& Display fortnightly send data in ALV popup
*&---------------------------------------------------------------------*
FORM display_fnt_preview USING pt_fnt TYPE STANDARD TABLE.
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat TYPE slis_fieldcat_alv.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'DATE_FROM'.
  ls_fieldcat-seltext_l = 'From'.
  ls_fieldcat-col_pos   = 1.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'DATE_TO'.
  ls_fieldcat-seltext_l = 'To'.
  ls_fieldcat-col_pos   = 2.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'CTP_ID'.
  ls_fieldcat-seltext_l = 'CTP ID'.
  ls_fieldcat-col_pos   = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ONGC_MATER'.
  ls_fieldcat-seltext_l = 'ONGC Material'.
  ls_fieldcat-col_pos   = 4.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GAIL_LOC_ID'.
  ls_fieldcat-seltext_l = 'GAIL Location ID'.
  ls_fieldcat-col_pos   = 5.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GAIL_MATERIAL'.
  ls_fieldcat-seltext_l = 'GAIL Material'.
  ls_fieldcat-col_pos   = 6.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'STATE_CODE'.
  ls_fieldcat-seltext_l = 'State Code'.
  ls_fieldcat-col_pos   = 7.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'STATE'.
  ls_fieldcat-seltext_l = 'State'.
  ls_fieldcat-col_pos   = 8.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'QTY_IN_SCM'.
  ls_fieldcat-seltext_l = 'Qty in SCM'.
  ls_fieldcat-col_pos   = 9.
  ls_fieldcat-do_sum    = abap_true.
  ls_fieldcat-decimals_out = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GCV'.
  ls_fieldcat-seltext_l = 'GCV'.
  ls_fieldcat-col_pos   = 10.
  ls_fieldcat-decimals_out = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NCV'.
  ls_fieldcat-seltext_l = 'NCV'.
  ls_fieldcat-col_pos   = 11.
  ls_fieldcat-decimals_out = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'QTY_IN_MBG'.
  ls_fieldcat-seltext_l = 'Qty in MBG'.
  ls_fieldcat-col_pos   = 12.
  ls_fieldcat-do_sum    = abap_true.
  ls_fieldcat-decimals_out = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GAIL_ID'.
  ls_fieldcat-seltext_l = 'GAIL ID'.
  ls_fieldcat-col_pos   = 13.
  ls_fieldcat-outputlen = 20.
  APPEND ls_fieldcat TO lt_fieldcat.

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title               = 'Fortnightly Data to be Sent to ONGC'
      i_selection           = ' '
      i_zebra               = abap_true
      i_screen_start_column = 2
      i_screen_start_line   = 3
      i_screen_end_column   = 160
      i_screen_end_line     = 30
      i_tabname             = 'PT_FNT'
      it_fieldcat           = lt_fieldcat
    TABLES
      t_outtab              = pt_fnt
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form RECALCULATE_TOTALS
*&---------------------------------------------------------------------*
FORM recalculate_totals.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid,
        c_tgqty TYPE msego2-adqnt,
        i_trqty TYPE msego2-adqnt,
        lv_gcv  TYPE oib_par_fltp,
        lv_ncv  TYPE oib_par_fltp.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.
  " Recalculate totals for each row (only TOTAL_MBG and TOTAL_SCM - GCV/NCV unchanged)
  LOOP AT gt_alv_display ASSIGNING FIELD-SYMBOL(<fs_alv>).
    " Sum all day columns for TOTAL_SCM - DAY16 only included when fortnight has 16 days
    <fs_alv>-total_scm = <fs_alv>-day01 + <fs_alv>-day02 + <fs_alv>-day03 +
                         <fs_alv>-day04 + <fs_alv>-day05 + <fs_alv>-day06 +
                         <fs_alv>-day07 + <fs_alv>-day08 + <fs_alv>-day09 +
                         <fs_alv>-day10 + <fs_alv>-day11 + <fs_alv>-day12 +
                         <fs_alv>-day13 + <fs_alv>-day14 + <fs_alv>-day15.
    IF gv_date_to - gv_date_from + 1 = 16.
      <fs_alv>-total_scm = <fs_alv>-total_scm + <fs_alv>-day16.
    ENDIF.
    " Convert TOTAL_MBG to TOTAL_SCM using existing GCV/NCV values (no gt_gas_receipt)
    IF <fs_alv>-gcv > 0 AND <fs_alv>-total_mbg > 0.
      CLEAR c_tgqty.
      i_trqty = <fs_alv>-total_scm.
      lv_gcv  = <fs_alv>-gcv.
      lv_ncv  = <fs_alv>-ncv.
      CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
        EXPORTING
          i_trqty = i_trqty
          i_truom = 'SM3'
          i_tguom = 'MBG'
          lv_gcv  = lv_gcv
          lv_ncv  = lv_ncv
        CHANGING
          c_tgqty = c_tgqty.
      <fs_alv>-total_mbg = c_tgqty.
    ELSE.
      <fs_alv>-total_scm = 0.
    ENDIF.
    " GCV and NCV remain unchanged (already set during allocation)
  ENDLOOP.
  " Refresh ALV display
  IF lr_grid IS BOUND.
    lr_grid->refresh_table_display( ).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form build_alv_display_table_view
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& --> p1        text
*& <-- p2        text
*&---------------------------------------------------------------------*
FORM build_alv_display_table_view .
  DATA it_cst_pur_temp TYPE TABLE OF yrga_cst_pur.
  DATA ls_alv TYPE ty_alv_display.
  DATA l_day TYPE char10.
  DATA l_index(2) TYPE n.
  DATA l_ncv TYPE ygms_de_qty_mbg_cal."ygms_de_gcv.
  DATA l_gcv TYPE ygms_de_qty_mbg_cal."ygms_de_gcv.
  DATA l_day_sm3 TYPE p DECIMALS 6.
  SELECT * INTO TABLE @DATA(it_yrga_cst_pur)
    FROM yrga_cst_pur
    WHERE gas_day IN @s_date
      AND location IN @s_loc AND deleted = ' '.
  IF sy-subrc = 0.
    SORT it_yrga_cst_pur BY created_date DESCENDING created_time DESCENDING.
    DELETE ADJACENT DUPLICATES FROM it_yrga_cst_pur COMPARING gas_day location material state_code.
    MOVE it_yrga_cst_pur[] TO it_cst_pur_temp[].
    SORT it_cst_pur_temp BY location material state_code.
    DELETE ADJACENT DUPLICATES FROM it_cst_pur_temp COMPARING location material state_code.
    SORT it_yrga_cst_pur BY gas_day location material state_code.
    LOOP AT it_cst_pur_temp INTO DATA(wa_csr_pur_temp).
      ls_alv-state_code  = wa_csr_pur_temp-state_code.
      ls_alv-state       = wa_csr_pur_temp-state.
      ls_alv-material    = wa_csr_pur_temp-material.
      ls_alv-location_id = wa_csr_pur_temp-location.
      ls_alv-exclude     = wa_csr_pur_temp-exclude.
      CLEAR l_index.
      LOOP AT it_yrga_cst_pur INTO DATA(wa_yrga_cst_pur)
        WHERE location   = wa_csr_pur_temp-location
          AND material   = wa_csr_pur_temp-material
          AND state_code = wa_csr_pur_temp-state_code.
        l_index = l_index + 1.
        CLEAR l_day.
        CONCATENATE 'DAY' l_index INTO l_day.
        ASSIGN COMPONENT l_day OF STRUCTURE ls_alv TO FIELD-SYMBOL(<fs_day>).
        IF sy-subrc = 0.
          <fs_day> = wa_yrga_cst_pur-qty_in_scm.
          ls_alv-total_mbg = ls_alv-total_mbg + wa_yrga_cst_pur-qty_in_mbg.
          ls_alv-total_scm = ls_alv-total_scm + <fs_day>.
          l_gcv = ( <fs_day> * wa_yrga_cst_pur-gcv ) + l_gcv.
          l_ncv = ( <fs_day> * wa_yrga_cst_pur-ncv ) + l_ncv.
        ENDIF.
      ENDLOOP.
      ls_alv-gcv = l_gcv / ls_alv-total_scm.
      ls_alv-ncv = l_ncv / ls_alv-total_scm.
      APPEND ls_alv TO gt_alv_display.
      CLEAR: ls_alv, l_gcv, l_ncv.
    ENDLOOP.
    CLEAR: ls_alv.
    CLEAR: l_gcv, l_ncv.
  ENDIF.
  DATA it_gas_receipt TYPE TABLE OF ty_gas_receipt.
  MOVE gt_gas_receipt[] TO it_gas_receipt[].
  SORT it_gas_receipt BY location_id material.
  DELETE ADJACENT DUPLICATES FROM it_gas_receipt COMPARING location_id material.
  LOOP AT it_gas_receipt INTO DATA(wa_gas_temp).
    READ TABLE gt_alv_display TRANSPORTING NO FIELDS
      WITH KEY location_id = wa_gas_temp-location_id
               material    = wa_gas_temp-material
               state_code  = 'GJ'.
    IF sy-subrc <> 0.
      CLEAR ls_alv.
      ls_alv-state_code  = 'GJ'.
      ls_alv-state       = 'Gujrat'.
      ls_alv-material    = wa_gas_temp-material.
      ls_alv-location_id = wa_gas_temp-location_id.
      APPEND ls_alv TO gt_alv_display.
    ENDIF.
    READ TABLE it_final_main TRANSPORTING NO FIELDS
    WITH KEY empst = wa_gas_temp-location_id
             matnr   = wa_gas_temp-material
             regio  = 'GJ'.
    IF sy-subrc <> 0.
      CLEAR wa_final_main.
      wa_final_main-empst      = wa_gas_temp-location_id.
      wa_final_main-regio      = 'GJ'.
      wa_final_main-regio_desc = 'Gujrat'.
      wa_final_main-matnr      = wa_gas_temp-material.
      APPEND wa_final_main TO it_final_main.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_NEW_RECEIPT_DATA
*& Point 5: Display new receipt data details in ALV popup
*&---------------------------------------------------------------------*
FORM display_new_receipt_data.
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat TYPE slis_fieldcat_alv.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GAS_DAY'.
  ls_fieldcat-seltext_l = 'Gas Day'.
  ls_fieldcat-col_pos   = 1.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'CTP_ID'.
  ls_fieldcat-seltext_l = 'CTP ID'.
  ls_fieldcat-col_pos   = 2.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ONGC_MATERIAL'.
  ls_fieldcat-seltext_l = 'ONGC Material'.
  ls_fieldcat-col_pos   = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'QTY_SCM'.
  ls_fieldcat-seltext_l = 'Qty SCM'.
  ls_fieldcat-col_pos   = 4.
  ls_fieldcat-decimals_out = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GCV'.
  ls_fieldcat-seltext_l = 'GCV'.
  ls_fieldcat-col_pos   = 5.
  ls_fieldcat-decimals_out = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NCV'.
  ls_fieldcat-seltext_l = 'NCV'.
  ls_fieldcat-col_pos   = 6.
  ls_fieldcat-decimals_out = 3.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ONGC_ID'.
  ls_fieldcat-seltext_l = 'ONGC ID'.
  ls_fieldcat-col_pos   = 7.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'DATE'.
  ls_fieldcat-seltext_l = 'Creation date'.
  ls_fieldcat-col_pos   = 8.
  APPEND ls_fieldcat TO lt_fieldcat.
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TIME'.
  ls_fieldcat-seltext_l = 'Creation time'.
  ls_fieldcat-col_pos   = 9.
  APPEND ls_fieldcat TO lt_fieldcat.
  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title               = 'New Receipt Data from ONGC'
      i_selection           = ' '
      i_zebra               = abap_true
      i_screen_start_column = 5
      i_screen_start_line   = 5
      i_screen_end_column   = 150
      i_screen_end_line     = 25
      i_tabname             = 'GT_NEW_RECEIPT_DATA'
      it_fieldcat           = lt_fieldcat
    TABLES
      t_outtab              = gt_new_receipt_data
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FETCH_SAVED_DATA
*& Fetch saved daily data from YRGA_CST_PUR and fortnightly from
*& YRGA_CST_FN_DATA for the selected date range and locations
*&---------------------------------------------------------------------*
FORM fetch_saved_data.
  DATA: ls_daily TYPE ty_saved_daily,
        ls_fnt   TYPE ty_saved_fnt.
  DATA lv_answer TYPE c .
  CLEAR: gt_saved_daily, gt_saved_fnt.
  " Fetch daily data from YRGA_CST_PUR
  SELECT * INTO TABLE @DATA(lt_cst_pur)
    FROM yrga_cst_pur
    WHERE gas_day IN @s_date
      AND location IN @s_loc AND deleted = ' '.
  IF sy-subrc = 0.
    SORT lt_cst_pur BY created_date DESCENDING created_time DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_cst_pur COMPARING gas_day location material state_code.
    SORT lt_cst_pur BY gas_day location material state_code.
    LOOP AT lt_cst_pur INTO DATA(wa_pur).
      CLEAR ls_daily.
      ls_daily-gas_day    = wa_pur-gas_day.
      ls_daily-ctp        = wa_pur-ctp.
      ls_daily-ongc_mater = wa_pur-ongc_mater.
      ls_daily-state_code = wa_pur-state_code.
      ls_daily-state      = wa_pur-state.
      ls_daily-qty_in_scm = wa_pur-qty_in_scm.
      ls_daily-gcv        = wa_pur-gcv.
      ls_daily-ncv        = wa_pur-ncv.
      ls_daily-qty_in_mbg = wa_pur-qty_in_mbg.
      ls_daily-ongc_id    = wa_pur-ongc_id.
      ls_daily-gail_id    = wa_pur-gail_id.
      ls_daily-location     = wa_pur-location.
      ls_daily-material     = wa_pur-material.
      ls_daily-exclude      = wa_pur-exclude.
      ls_daily-created_by   = wa_pur-created_by.
      ls_daily-created_date = wa_pur-created_date.
      ls_daily-created_time = wa_pur-created_time.
      ls_daily-sent_by = wa_pur-sent_by.
      ls_daily-sent_on = wa_pur-sent_on.
      ls_daily-sent_at = wa_pur-sent_at.
      CASE wa_pur-sent_e.
        WHEN '1'. ls_daily-sent_e = '1 - B2B PI'.
        WHEN '2'. ls_daily-sent_e = '2 - Email'.
        WHEN OTHERS. ls_daily-sent_e = wa_pur-sent_e.
      ENDCASE.
      ls_daily-deleted = wa_pur-deleted.
      APPEND ls_daily TO gt_saved_daily.
    ENDLOOP.
* else.
  ENDIF.
  SELECT * INTO TABLE @lt_cst_pur
 FROM yrga_cst_pur
 WHERE gas_day IN @s_date
   AND location IN @s_loc AND deleted = 'X'.
  IF sy-subrc = 0.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        textline1      = 'Deleted Data found '
        textline2      = 'for the queried period. Do you want to view the deleted data also ?'
        titel          = 'Deleted Data Found'
        cancel_display = ' '
      IMPORTING
        answer         = lv_answer.
    IF lv_answer = 'J'.
      SORT lt_cst_pur BY created_date DESCENDING created_time DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_cst_pur COMPARING gas_day location material state_code.
      SORT lt_cst_pur BY gas_day location material state_code.
      LOOP AT lt_cst_pur INTO wa_pur.
        CLEAR ls_daily.
        ls_daily-gas_day    = wa_pur-gas_day.
        ls_daily-ctp        = wa_pur-ctp.
        ls_daily-ongc_mater = wa_pur-ongc_mater.
        ls_daily-state_code = wa_pur-state_code.
        ls_daily-state      = wa_pur-state.
        ls_daily-qty_in_scm = wa_pur-qty_in_scm.
        ls_daily-gcv        = wa_pur-gcv.
        ls_daily-ncv        = wa_pur-ncv.
        ls_daily-qty_in_mbg = wa_pur-qty_in_mbg.
        ls_daily-ongc_id    = wa_pur-ongc_id.
        ls_daily-gail_id    = wa_pur-gail_id.
        ls_daily-location     = wa_pur-location.
        ls_daily-material     = wa_pur-material.
        ls_daily-exclude      = wa_pur-exclude.
        ls_daily-created_by   = wa_pur-created_by.
        ls_daily-created_date = wa_pur-created_date.
        ls_daily-created_time = wa_pur-created_time.
        ls_daily-sent_by = wa_pur-sent_by.
        ls_daily-sent_on = wa_pur-sent_on.
        ls_daily-sent_at = wa_pur-sent_at.
        CASE wa_pur-sent_e.
          WHEN '1'. ls_daily-sent_e = '1 - B2B PI'.
          WHEN '2'. ls_daily-sent_e = '2 - Email'.
          WHEN OTHERS. ls_daily-sent_e = wa_pur-sent_e.
        ENDCASE.
        ls_daily-deleted = wa_pur-deleted.
        APPEND ls_daily TO gt_saved_daily.
      ENDLOOP.
*    endif.
    ENDIF.
  ENDIF.
  " Fetch fortnightly data from YRGA_CST_FN_DATA
  SELECT * INTO TABLE @DATA(lt_cst_fnt)
    FROM yrga_cst_fn_data
    WHERE date_from IN @s_date
      AND location IN @s_loc AND deleted = ' '.
  IF sy-subrc = 0.
    SORT lt_cst_fnt BY created_date DESCENDING created_time DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_cst_fnt COMPARING date_from date_to location material state_code.
    SORT lt_cst_fnt BY date_from location material state_code.
    LOOP AT lt_cst_fnt INTO DATA(wa_fnt).
      CLEAR ls_fnt.
      ls_fnt-date_from  = wa_fnt-date_from.
      ls_fnt-date_to    = wa_fnt-date_to.
      ls_fnt-ctp        = wa_fnt-ctp.
      ls_fnt-ongc_mater = wa_fnt-ongc_mater.
      ls_fnt-state_code = wa_fnt-state_code.
      ls_fnt-state      = wa_fnt-state.
      ls_fnt-qty_in_scm = wa_fnt-qty_in_scm.
      ls_fnt-gcv        = wa_fnt-gcv.
      ls_fnt-ncv        = wa_fnt-ncv.
      ls_fnt-qty_in_mbg = wa_fnt-qty_in_mbg.
      ls_fnt-gail_id      = wa_fnt-gail_id.
      ls_fnt-location     = wa_fnt-location.
      ls_fnt-material     = wa_fnt-material.
      ls_fnt-created_by   = wa_fnt-created_by.
      ls_fnt-created_date = wa_fnt-created_date.
      ls_fnt-created_time = wa_fnt-created_time.
      ls_fnt-sent_by = wa_fnt-sent_by.
      ls_fnt-sent_on = wa_fnt-sent_on.
      ls_fnt-sent_at = wa_fnt-sent_at.
      CASE wa_fnt-sent_e.
        WHEN '1'. ls_fnt-sent_e = '1 - B2B PI'.
        WHEN '2'. ls_fnt-sent_e = '2 - Email'.
        WHEN OTHERS. ls_fnt-sent_e = wa_fnt-sent_e.
      ENDCASE.
      ls_fnt-deleted = wa_fnt-deleted.
      APPEND ls_fnt TO gt_saved_fnt.
    ENDLOOP.
    IF lv_answer = 'J'.
      SELECT * INTO TABLE @lt_cst_fnt
     FROM yrga_cst_fn_data
     WHERE date_from IN @s_date
       AND location IN @s_loc AND deleted = 'X'.
      IF sy-subrc = 0.
        SORT lt_cst_fnt BY created_date DESCENDING created_time DESCENDING.
        DELETE ADJACENT DUPLICATES FROM lt_cst_fnt COMPARING date_from date_to location material state_code.
        SORT lt_cst_fnt BY date_from location material state_code.
        LOOP AT lt_cst_fnt INTO wa_fnt.
          CLEAR ls_fnt.
          ls_fnt-date_from  = wa_fnt-date_from.
          ls_fnt-date_to    = wa_fnt-date_to.
          ls_fnt-ctp        = wa_fnt-ctp.
          ls_fnt-ongc_mater = wa_fnt-ongc_mater.
          ls_fnt-state_code = wa_fnt-state_code.
          ls_fnt-state      = wa_fnt-state.
          ls_fnt-qty_in_scm = wa_fnt-qty_in_scm.
          ls_fnt-gcv        = wa_fnt-gcv.
          ls_fnt-ncv        = wa_fnt-ncv.
          ls_fnt-qty_in_mbg = wa_fnt-qty_in_mbg.
          ls_fnt-gail_id      = wa_fnt-gail_id.
          ls_fnt-location     = wa_fnt-location.
          ls_fnt-material     = wa_fnt-material.
          ls_fnt-created_by   = wa_fnt-created_by.
          ls_fnt-created_date = wa_fnt-created_date.
          ls_fnt-created_time = wa_fnt-created_time.
          ls_fnt-sent_by = wa_fnt-sent_by.
          ls_fnt-sent_on = wa_fnt-sent_on.
          ls_fnt-sent_at = wa_fnt-sent_at.
          CASE wa_fnt-sent_e.
            WHEN '1'. ls_fnt-sent_e = '1 - B2B PI'.
            WHEN '2'. ls_fnt-sent_e = '2 - Email'.
            WHEN OTHERS. ls_fnt-sent_e = wa_fnt-sent_e.
          ENDCASE.
          ls_fnt-deleted = wa_fnt-deleted.
          APPEND ls_fnt TO gt_saved_fnt.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
  IF gt_saved_daily IS INITIAL AND gt_saved_fnt IS INITIAL.
    MESSAGE s000(ygms_msg) WITH 'No saved data found for the selected criteria.' DISPLAY LIKE 'W'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FETCH_SENT_DATA
*& First check YRGA_CST_FN_DATA / YRGA_CST_PUR where sent_e is filled.
*& If found, show that data. Otherwise fall back to YRGA_CST_B2B_2/3.
*&---------------------------------------------------------------------*
FORM fetch_sent_data.
  DATA: ls_daily TYPE ty_saved_daily,
        ls_fnt   TYPE ty_saved_fnt.
  DATA: lv_found TYPE abap_bool VALUE abap_false.
  CLEAR: gt_saved_daily, gt_saved_fnt.
  " --- 1. Check YRGA_CST_FN_DATA for sent records ---
*  SELECT *
*    FROM yrga_cst_fn_data
*    WHERE date_from IN @s_date
*      AND location  IN @s_loc
*      AND sent_e    IS NOT INITIAL "AND deleted = ' '
*    INTO TABLE @DATA(lt_fnt_sent).
*  IF sy-subrc = 0 AND lt_fnt_sent IS NOT INITIAL.
*    lv_found = abap_true.
*    SORT lt_fnt_sent BY created_date DESCENDING created_time DESCENDING.
**    DELETE ADJACENT DUPLICATES FROM lt_fnt_sent COMPARING date_from date_to location material state_code.
*    SORT lt_fnt_sent BY date_from location material state_code.
*    LOOP AT lt_fnt_sent INTO DATA(wa_fnt_s).
*      CLEAR ls_fnt.
*      ls_fnt-date_from  = wa_fnt_s-date_from.
*      ls_fnt-date_to    = wa_fnt_s-date_to.
*      ls_fnt-ctp        = wa_fnt_s-ctp.
*      ls_fnt-ongc_mater = wa_fnt_s-ongc_mater.
*      ls_fnt-state_code = wa_fnt_s-state_code.
*      ls_fnt-state      = wa_fnt_s-state.
*      ls_fnt-qty_in_scm = wa_fnt_s-qty_in_scm.
*      ls_fnt-gcv        = wa_fnt_s-gcv.
*      ls_fnt-ncv        = wa_fnt_s-ncv.
*      ls_fnt-qty_in_mbg = wa_fnt_s-qty_in_mbg.
*      ls_fnt-gail_id    = wa_fnt_s-gail_id.
*      ls_fnt-location   = wa_fnt_s-location.
*      ls_fnt-material   = wa_fnt_s-material.
*      ls_fnt-sent_by    = wa_fnt_s-sent_by.
*      ls_fnt-sent_on    = wa_fnt_s-sent_on.
*      ls_fnt-sent_at    = wa_fnt_s-sent_at.
*      APPEND ls_fnt TO gt_saved_fnt.
*    ENDLOOP.
*    " Also fetch daily sent data from YRGA_CST_PUR
*    SELECT *
*      FROM yrga_cst_pur
*      WHERE gas_day  IN @s_date
*        AND location IN @s_loc
*        AND sent_e   IS NOT INITIAL" AND deleted = ' '
*      INTO TABLE @DATA(lt_pur_sent).
*    IF sy-subrc = 0.
*      SORT lt_pur_sent BY created_date DESCENDING created_time DESCENDING.
**      DELETE ADJACENT DUPLICATES FROM lt_pur_sent COMPARING gas_day location material state_code.
*      SORT lt_pur_sent BY gas_day location material state_code.
*      LOOP AT lt_pur_sent INTO DATA(wa_pur_s).
*        CLEAR ls_daily.
*        ls_daily-gas_day    = wa_pur_s-gas_day.
*        ls_daily-ctp        = wa_pur_s-ctp.
*        ls_daily-ongc_mater = wa_pur_s-ongc_mater.
*        ls_daily-state_code = wa_pur_s-state_code.
*        ls_daily-state      = wa_pur_s-state.
*        ls_daily-qty_in_scm = wa_pur_s-qty_in_scm.
*        ls_daily-gcv        = wa_pur_s-gcv.
*        ls_daily-ncv        = wa_pur_s-ncv.
*        ls_daily-qty_in_mbg = wa_pur_s-qty_in_mbg.
*        ls_daily-ongc_id    = wa_pur_s-ongc_id.
*        ls_daily-gail_id    = wa_pur_s-gail_id.
*        ls_daily-location   = wa_pur_s-location.
*        ls_daily-material   = wa_pur_s-material.
*        ls_daily-exclude    = wa_pur_s-exclude.
*        ls_daily-sent_by    = wa_pur_s-sent_by.
*        ls_daily-sent_on    = wa_pur_s-sent_on.
*        ls_daily-sent_at    = wa_pur_s-sent_at.
*        APPEND ls_daily TO gt_saved_daily.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.
  " --- 2. Fallback: fetch from YRGA_CST_B2B_2 / B2B_3 ---
*  IF lv_found = abap_false.
  " Fetch latest daily records from YRGA_CST_B2B_2
  SELECT * INTO TABLE @DATA(lt_b2b_2)
    FROM yrga_cst_b2b_2
    WHERE gas_day IN @s_date.
  IF sy-subrc = 0.
    SORT lt_b2b_2 BY time_stamp DESCENDING.
*      DELETE ADJACENT DUPLICATES FROM lt_b2b_2 COMPARING gas_day ctp_id ongc_material state_code.
    SORT lt_b2b_2 BY gas_day ctp_id ongc_material state_code.
    LOOP AT lt_b2b_2 INTO DATA(wa_b2b_2).
      CLEAR ls_daily.
      ls_daily-gas_day    = wa_b2b_2-gas_day.
      ls_daily-ctp        = wa_b2b_2-ctp_id.
      ls_daily-ongc_mater = wa_b2b_2-ongc_material.
      ls_daily-state_code = wa_b2b_2-state_code.
      ls_daily-state      = wa_b2b_2-state.
      ls_daily-qty_in_scm = wa_b2b_2-qty_scm.
      ls_daily-gcv        = wa_b2b_2-gcv.
      ls_daily-ncv        = wa_b2b_2-ncv.
      ls_daily-qty_in_mbg = wa_b2b_2-qty_in_mbg.
      ls_daily-ongc_id    = wa_b2b_2-ongc_id.
      ls_daily-gail_id =   wa_b2b_2-gail_id.
      APPEND ls_daily TO gt_saved_daily.
    ENDLOOP.
  ENDIF.
  " Fetch latest fortnightly records from YRGA_CST_B2B_3
  SELECT * INTO TABLE @DATA(lt_b2b_3)
    FROM yrga_cst_b2b_3
    WHERE date_from IN @s_date.
  IF sy-subrc = 0.
    SORT lt_b2b_3 BY time_stamp DESCENDING.
*      DELETE ADJACENT DUPLICATES FROM lt_b2b_3 COMPARING date_from date_to ctp ongc_material state_code.
    SORT lt_b2b_3 BY date_from ctp ongc_material state_code.
    LOOP AT lt_b2b_3 INTO DATA(wa_b2b_3).
      CLEAR ls_fnt.
      ls_fnt-date_from  = wa_b2b_3-date_from.
      ls_fnt-date_to    = wa_b2b_3-date_to.
      ls_fnt-ctp        = wa_b2b_3-ctp.
      ls_fnt-ongc_mater = wa_b2b_3-ongc_material.
      ls_fnt-state_code = wa_b2b_3-state_code.
      ls_fnt-state      = wa_b2b_3-state.
      ls_fnt-qty_in_scm = wa_b2b_3-qty_in_scm.
      ls_fnt-gcv        = wa_b2b_3-gcv.
      ls_fnt-ncv        = wa_b2b_3-ncv.
      ls_fnt-qty_in_mbg = wa_b2b_3-qty_in_mbg.
      ls_fnt-gail_id    = wa_b2b_3-gail_id.
      APPEND ls_fnt TO gt_saved_fnt.
    ENDLOOP.
  ENDIF.
*  ENDIF.
  IF gt_saved_daily IS INITIAL AND gt_saved_fnt IS INITIAL.
    MESSAGE s000(ygms_msg) WITH 'No sent data found for the selected criteria.' DISPLAY LIKE 'W'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_SAVED_DATA_ALV
*& Display saved data in ALV with Daily/Fortnightly toggle buttons.
*& Uses a loop so toggling exits the current ALV and re-displays.
*&---------------------------------------------------------------------*
FORM display_saved_data_alv.
  gv_saved_view = 'D'.  " Start with Daily Data view
  DO.
    IF gv_saved_view = 'D'.
      PERFORM display_saved_daily_alv.
    ELSE.
      PERFORM display_saved_fnt_alv.
    ENDIF.
    " If user pressed Back (not a toggle), exit the loop
    IF sy-ucomm <> 'DAILY_DATA' AND sy-ucomm <> 'FNT_DATA'.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_SAVED_DAILY_ALV
*& Display daily saved data from YRGA_CST_PUR in ALV
*&---------------------------------------------------------------------*
FORM display_saved_daily_alv.
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat TYPE slis_fieldcat_alv,
        ls_layout   TYPE slis_layout_alv,
        lv_col      TYPE i VALUE 0.
  IF p_vsent IS NOT INITIAL.
    " View Sent Data: show columns matching YRGA_CST_PUR minus location/material/exclude
    " plus Sent By, Sent On, Sent Time
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'GAS_DAY'.
    ls_fieldcat-seltext_l = 'Gas Day'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'CTP'.
    ls_fieldcat-seltext_l = 'CTP ID'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'ONGC_MATER'.
    ls_fieldcat-seltext_l = 'ONGC Material'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 15.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'STATE_CODE'.
    ls_fieldcat-seltext_l = 'State Code'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'STATE'.
    ls_fieldcat-seltext_l = 'State'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 15.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'QTY_IN_SCM'.
    ls_fieldcat-seltext_l = 'Qty SCM'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    ls_fieldcat-do_sum    = abap_true.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'GCV'.
    ls_fieldcat-seltext_l = 'GCV'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'NCV'.
    ls_fieldcat-seltext_l = 'NCV'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'QTY_IN_MBG'.
    ls_fieldcat-seltext_l = 'Qty MBG'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    ls_fieldcat-do_sum    = abap_true.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'ONGC_ID'.
    ls_fieldcat-seltext_l = 'ONGC ID'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'GAIL_ID'.
    ls_fieldcat-seltext_l = 'GAIL ID'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 15.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'SENT_BY'.
    ls_fieldcat-seltext_l = 'Sent By'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'SENT_ON'.
    ls_fieldcat-seltext_l = 'Sent On'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'SENT_AT'.
    ls_fieldcat-seltext_l = 'Sent Time'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 8.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'DELETED'.
    ls_fieldcat-seltext_l = 'Deletion flag'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 8.
    APPEND ls_fieldcat TO lt_fieldcat.
  ELSE.
    " View Saved Data: show all YRGA_CST_PUR columns except timestamp
    " Layout matches YRGA_CST_PUR table field order
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'GAS_DAY'.
    ls_fieldcat-seltext_l = 'Gas Day'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'CTP'.
    ls_fieldcat-seltext_l = 'CTP ID'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'ONGC_MATER'.
    ls_fieldcat-seltext_l = 'ONGC Material'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 15.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'STATE_CODE'.
    ls_fieldcat-seltext_l = 'State Code'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'STATE'.
    ls_fieldcat-seltext_l = 'State'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 15.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'QTY_IN_SCM'.
    ls_fieldcat-seltext_l = 'Qty SCM'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    ls_fieldcat-do_sum    = abap_true.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'GCV'.
    ls_fieldcat-seltext_l = 'GCV'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'NCV'.
    ls_fieldcat-seltext_l = 'NCV'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'QTY_IN_MBG'.
    ls_fieldcat-seltext_l = 'Qty MBG'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    ls_fieldcat-do_sum    = abap_true.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'ONGC_ID'.
    ls_fieldcat-seltext_l = 'ONGC ID'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'GAIL_ID'.
    ls_fieldcat-seltext_l = 'GAIL ID'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 15.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'LOCATION'.
    ls_fieldcat-seltext_l = 'Location'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'MATERIAL'.
    ls_fieldcat-seltext_l = 'Material'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 18.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'EXCLUDE'.
    ls_fieldcat-seltext_l = 'Exclude'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 7.
    ls_fieldcat-checkbox  = abap_true.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'CREATED_BY'.
    ls_fieldcat-seltext_l = 'Created By'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'CREATED_DATE'.
    ls_fieldcat-seltext_l = 'Created Date'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'CREATED_TIME'.
    ls_fieldcat-seltext_l = 'Created Time'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 8.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'SENT_BY'.
    ls_fieldcat-seltext_l = 'Sent By'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'SENT_ON'.
    ls_fieldcat-seltext_l = 'Sent On'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'SENT_AT'.
    ls_fieldcat-seltext_l = 'Sent Time'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 8.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'SENT_E'.
    ls_fieldcat-seltext_l = 'Sent Via'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'DELETED'.
    ls_fieldcat-seltext_l = 'Deletion flag'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 8.
    APPEND ls_fieldcat TO lt_fieldcat.
  ENDIF.
  ls_layout-colwidth_optimize = abap_true.
  ls_layout-zebra             = abap_true.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_PF_STATUS_SAVED'
      i_callback_user_command  = 'USER_COMMAND_SAVED'
      is_layout                = ls_layout
      it_fieldcat              = lt_fieldcat
    TABLES
      t_outtab                 = gt_saved_daily
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DISPLAY_SAVED_FNT_ALV
*& Display fortnightly saved data from YRGA_CST_FN_DATA in ALV
*&---------------------------------------------------------------------*
FORM display_saved_fnt_alv.
  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv,
        ls_fieldcat TYPE slis_fieldcat_alv,
        ls_layout   TYPE slis_layout_alv,
        lv_col      TYPE i VALUE 0.
  IF p_vsent IS NOT INITIAL.
    " View Sent Data: show columns minus location/material
    " plus Sent By, Sent On, Sent Time
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'DATE_FROM'.
    ls_fieldcat-seltext_l = 'From'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'DATE_TO'.
    ls_fieldcat-seltext_l = 'To'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'CTP'.
    ls_fieldcat-seltext_l = 'CTP ID'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'ONGC_MATER'.
    ls_fieldcat-seltext_l = 'ONGC Material'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 15.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'STATE_CODE'.
    ls_fieldcat-seltext_l = 'State Code'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'STATE'.
    ls_fieldcat-seltext_l = 'State'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 15.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'QTY_IN_SCM'.
    ls_fieldcat-seltext_l = 'Qty in SCM'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    ls_fieldcat-do_sum    = abap_true.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'GCV'.
    ls_fieldcat-seltext_l = 'GCV'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'NCV'.
    ls_fieldcat-seltext_l = 'NCV'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'QTY_IN_MBG'.
    ls_fieldcat-seltext_l = 'Qty in MBG'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    ls_fieldcat-do_sum    = abap_true.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'GAIL_ID'.
    ls_fieldcat-seltext_l = 'GAIL ID'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 15.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'SENT_BY'.
    ls_fieldcat-seltext_l = 'Sent By'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'SENT_ON'.
    ls_fieldcat-seltext_l = 'Sent On'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'SENT_AT'.
    ls_fieldcat-seltext_l = 'Sent Time'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 8.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'DELETED'.
    ls_fieldcat-seltext_l = 'Deletion flag'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 8.
    APPEND ls_fieldcat TO lt_fieldcat.
  ELSE.
    " View Saved Data: show all YRGA_CST_FN_DATA columns except timestamp
    " Layout matches YRGA_CST_FN_DATA table field order
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'DATE_FROM'.
    ls_fieldcat-seltext_l = 'From'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'DATE_TO'.
    ls_fieldcat-seltext_l = 'To'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'CTP'.
    ls_fieldcat-seltext_l = 'CTP ID'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'ONGC_MATER'.
    ls_fieldcat-seltext_l = 'ONGC Material'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 15.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'STATE_CODE'.
    ls_fieldcat-seltext_l = 'State Code'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'STATE'.
    ls_fieldcat-seltext_l = 'State'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 15.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'QTY_IN_SCM'.
    ls_fieldcat-seltext_l = 'Qty in SCM'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    ls_fieldcat-do_sum    = abap_true.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'GCV'.
    ls_fieldcat-seltext_l = 'GCV'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'NCV'.
    ls_fieldcat-seltext_l = 'NCV'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'QTY_IN_MBG'.
    ls_fieldcat-seltext_l = 'Qty in MBG'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    ls_fieldcat-do_sum    = abap_true.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'GAIL_ID'.
    ls_fieldcat-seltext_l = 'GAIL ID'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 15.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'LOCATION'.
    ls_fieldcat-seltext_l = 'Location'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'MATERIAL'.
    ls_fieldcat-seltext_l = 'Material'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 18.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'CREATED_BY'.
    ls_fieldcat-seltext_l = 'Created By'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'CREATED_DATE'.
    ls_fieldcat-seltext_l = 'Created Date'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'CREATED_TIME'.
    ls_fieldcat-seltext_l = 'Created Time'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 8.
    APPEND ls_fieldcat TO lt_fieldcat.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'SENT_BY'.
    ls_fieldcat-seltext_l = 'Sent By'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'SENT_ON'.
    ls_fieldcat-seltext_l = 'Sent On'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 10.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'SENT_AT'.
    ls_fieldcat-seltext_l = 'Sent Time'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 8.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'SENT_E'.
    ls_fieldcat-seltext_l = 'Sent Via'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 12.
    APPEND ls_fieldcat TO lt_fieldcat.
    lv_col = lv_col + 1. CLEAR ls_fieldcat.
    ls_fieldcat-fieldname = 'DELETED'.
    ls_fieldcat-seltext_l = 'Deletion flag'.
    ls_fieldcat-col_pos   = lv_col.
    ls_fieldcat-outputlen = 8.
    APPEND ls_fieldcat TO lt_fieldcat.
  ENDIF.
  ls_layout-colwidth_optimize = abap_true.
  ls_layout-zebra             = abap_true.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_PF_STATUS_SAVED'
      i_callback_user_command  = 'USER_COMMAND_SAVED'
      is_layout                = ls_layout
      it_fieldcat              = lt_fieldcat
    TABLES
      t_outtab                 = gt_saved_fnt
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PF_STATUS_SAVED
*& PF-Status for Saved Data ALV - shows Daily Data / Fortnightly toggle
*&---------------------------------------------------------------------*
FORM set_pf_status_saved USING rt_extab TYPE slis_t_extab.
  DATA: lt_excl  TYPE slis_t_extab,
        ls_extab TYPE slis_extab.
  lt_excl = rt_extab.
  " Hide all standard workflow buttons not relevant in saved data view
  ls_extab-fcode = 'ALLOCATION'.  APPEND ls_extab TO lt_excl.
  ls_extab-fcode = 'REALLOCATE'.  APPEND ls_extab TO lt_excl.
  ls_extab-fcode = 'VALIDATE'.    APPEND ls_extab TO lt_excl.
  ls_extab-fcode = 'EDIT'.        APPEND ls_extab TO lt_excl.
  ls_extab-fcode = 'SAVE'.        APPEND ls_extab TO lt_excl.
  ls_extab-fcode = 'SEND'.        APPEND ls_extab TO lt_excl.
  ls_extab-fcode = 'RESET'.       APPEND ls_extab TO lt_excl.
  SET PF-STATUS 'ZSAVED_STATUS' EXCLUDING lt_excl.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form USER_COMMAND_SAVED
*& Handle toggle between Daily Data and Fortnightly Data views
*&---------------------------------------------------------------------*
FORM user_command_saved USING r_ucomm     TYPE sy-ucomm
                              rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN 'DAILY_DATA'.
      IF gv_saved_view <> 'D'.
        gv_saved_view = 'D'.
        rs_selfield-exit = abap_true.
      ENDIF.
    WHEN 'FNT_DATA'.
      IF gv_saved_view <> 'F'.
        gv_saved_view = 'F'.
        rs_selfield-exit = abap_true.
      ENDIF.
  ENDCASE.
ENDFORM.
FORM save_b2b_sent_data USING pt_daily TYPE STANDARD TABLE
                              pt_fnt   TYPE STANDARD TABLE.
  DATA: lt_b2b_2   TYPE TABLE OF yrga_cst_b2b_2,
        ls_b2b_2   TYPE yrga_cst_b2b_2,
        lt_b2b_3   TYPE TABLE OF yrga_cst_b2b_3,
        ls_b2b_3   TYPE yrga_cst_b2b_3,
        ls_pur     TYPE yrga_cst_pur,
        ls_fnt     TYPE yrga_cst_fn_data,
        lv_count_d TYPE i,
        lv_count_f TYPE i,
        lv_tstamp  TYPE timestamp.            " <-- NEW variable
  DATA l_date TYPE sy-datum.
  DATA l_time TYPE sy-uzeit.
  " Build daily B2B records from YRGA_CST_PUR data
  GET TIME STAMP FIELD lv_tstamp.              " <-- NEW (DEC 15 timestamp)
  LOOP AT pt_daily INTO ls_pur.
    CLEAR ls_b2b_2.
    ls_b2b_2-gas_day       = ls_pur-gas_day.
    ls_b2b_2-ctp_id        = ls_pur-ctp.        " <-- was ctp
    ls_b2b_2-ongc_material = ls_pur-ongc_mater.  " <-- was ongc_mater
    ls_b2b_2-time_stamp    = lv_tstamp.          " <-- was ls_pur-time_stamp
    ls_b2b_2-state_code    = ls_pur-state_code.
    ls_b2b_2-state         = ls_pur-state.
    ls_b2b_2-qty_scm       = ls_pur-qty_in_scm.  " <-- was qty_in_scm
    ls_b2b_2-gcv           = ls_pur-gcv.
    ls_b2b_2-ncv           = ls_pur-ncv.
    ls_b2b_2-qty_in_mbg    = ls_pur-qty_in_mbg.
    ls_b2b_2-ongc_id       = ls_pur-ongc_id.
    ls_b2b_2-sent_by       = sy-uname.
    ls_b2b_2-sent_on       = sy-datum.
    ls_b2b_2-sent_at       = sy-uzeit.
    ls_b2b_2-gail_id = ls_pur-gail_id.
    " REMOVED: ls_b2b_2-location, ls_b2b_2-material, ls_b2b_2-gail_id
    APPEND ls_b2b_2 TO lt_b2b_2.
  ENDLOOP.
  " Build fortnightly B2B records from YRGA_CST_FNT_DATA
  LOOP AT pt_fnt INTO ls_fnt.
    CLEAR ls_b2b_3.
    ls_b2b_3-date_from      = ls_fnt-date_from.
    ls_b2b_3-date_to        = ls_fnt-date_to.
    ls_b2b_3-ctp            = ls_fnt-ctp.
    ls_b2b_3-ongc_material  = ls_fnt-ongc_mater.  " <-- was ongc_mater
    ls_b2b_3-time_stamp     = lv_tstamp..
    ls_b2b_3-state_code     = ls_fnt-state_code.
    ls_b2b_3-state          = ls_fnt-state.
    ls_b2b_3-qty_in_mbg     = ls_fnt-qty_in_mbg.
    ls_b2b_3-gcv            = ls_fnt-gcv.
    ls_b2b_3-ncv            = ls_fnt-ncv.
    ls_b2b_3-qty_in_scm     = ls_fnt-qty_in_scm.
    ls_b2b_3-gail_id        = ls_fnt-gail_id.
    ls_b2b_3-sent_by        = sy-uname.
    ls_b2b_3-sent_on        = sy-datum.
    ls_b2b_3-sent_at        = sy-uzeit.
    " REMOVED: ls_b2b_3-location, ls_b2b_3-material
    APPEND ls_b2b_3 TO lt_b2b_3.
  ENDLOOP.
  " Delete existing B2B data for the same period before inserting
*  DELETE FROM yrga_cst_b2b_2
*    WHERE gas_day BETWEEN gv_date_from AND gv_date_to.
*  DELETE FROM yrga_cst_b2b_3
*    WHERE date_from = gv_date_from
*      AND date_to   = gv_date_to.
  " Save daily B2B records to YRGA_CST_B2B_2
  IF lt_b2b_2 IS NOT INITIAL.
    MODIFY yrga_cst_b2b_2 FROM TABLE lt_b2b_2.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      MESSAGE e000(ygms_msg) WITH 'Error saving data to YRGA_CST_B2B_2'.
      RETURN.
    ENDIF.
  ENDIF.
  " Save fortnightly B2B records to YRGA_CST_B2B_3
  IF lt_b2b_3 IS NOT INITIAL.
    MODIFY yrga_cst_b2b_3 FROM TABLE lt_b2b_3.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      MESSAGE e000(ygms_msg) WITH 'Error saving data to YRGA_CST_B2B_3'.
      RETURN.
    ENDIF.
  ENDIF.
  " Update sent tracking fields in source tables (B2B API = 1)
  CONVERT TIME STAMP lv_tstamp TIME ZONE sy-zonlo
   INTO DATE l_date TIME l_time.
  UPDATE yrga_cst_pur SET sent_e  = '1'
                          sent_by = sy-uname
                          sent_on = l_date
                          sent_at = l_time
    WHERE gas_day BETWEEN gv_date_from AND gv_date_to
      AND location IN s_loc
      AND exclude <> 'X' and deleted = ' '.
  UPDATE yrga_cst_fn_data SET sent_e  = '1'
                              sent_by = sy-uname
                              sent_on = l_date
                              sent_at = l_time
    WHERE date_from = gv_date_from
      AND date_to   = gv_date_to
      AND location  IN s_loc and deleted = ' '.
  COMMIT WORK AND WAIT.
  " Commit if both saves successful
  COMMIT WORK AND WAIT.
  lv_count_d = lines( lt_b2b_2 ).
  lv_count_f = lines( lt_b2b_3 ).
  MESSAGE s000(ygms_msg) WITH lv_count_d 'daily,' lv_count_f 'fortnightly B2B records saved'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATE_CV_DATA
*& Validates calorific values between YRXA_CMDATA and GT_GAS_RECEIPT.
*& Also validates receipt volumes (YRGA_CST_B2B_1) against measurement
*& volumes (YYCUM_VOL from YRXA_CMDATA) for each CTP ID and Gas Day.
*& Shows mismatch popup if GCV/NCV values differ. Program does not
*& proceed until CV validation is cleared. Volume mismatch also blocks
*& the allocation process if mismatches are detected.
*& Returns: cv_valid = abap_true if all validation passed, abap_false if not
*&---------------------------------------------------------------------*
FORM validate_cv_data CHANGING cv_valid TYPE abap_bool.
  " Local type matching YRXA_CMDATA actual field names:
  "   YYDATE         (DATS)   - Gas Day
  "   YYBUS_LOCATION (CHAR 10) - Location ID
  "   YYAVG_GCV      (FLTP 16) - Average GCV
  "   YYAVG_NCV      (FLTP 16) - Average NCV
  "   YYCUM_VOL      (FLTP 16) - Cumulative Volume (Sm3)
  "   YYTIMESTAMP    (DEC 15)  - UTC Timestamp
  TYPES: BEGIN OF lty_cmdata,
           yydate         TYPE dats,
           yybus_location TYPE oij_locid,
           yyavg_gcv      TYPE yyavg_gcv,
           yyavg_ncv      TYPE yyavg_ncv,
           yycum_vol      TYPE yycum_vol,
           yytimestamp    TYPE timestamp,
         END OF lty_cmdata.
  TYPES: BEGIN OF lty_cv_mismatch,
           gas_day       TYPE datum,
           location_id   TYPE ygms_de_loc_id,
           ctp_id        TYPE ygms_de_ongc_ctp,
           source        TYPE char30,
           gcv_meas      TYPE ygms_de_gcv,
           ncv_meas      TYPE ygms_de_ncv,
           gcv_receipt   TYPE ygms_de_gcv,
           ncv_receipt   TYPE ygms_de_ncv,
         END OF lty_cv_mismatch.
  " Volume matching types
  TYPES: BEGIN OF lty_b2b_vol,
           gas_day       TYPE datum,
           ctp_id        TYPE ygms_de_ongc_ctp,
           ongc_material TYPE ygms_de_ongc_mat,
           qty_scm       TYPE ygms_de_qty_scm,
           time_stamp    TYPE timestamp,
         END OF lty_b2b_vol.
  TYPES: BEGIN OF lty_receipt_total,
           gas_day  TYPE datum,
           ctp_id   TYPE ygms_de_ongc_ctp,
           qty_scm  TYPE p DECIMALS 3,
         END OF lty_receipt_total.
  TYPES: BEGIN OF lty_vol_mismatch,
           gas_day      TYPE datum,
           ctp_id       TYPE ygms_de_ongc_ctp,
           receipt_vol  TYPE p DECIMALS 3,
           location_id  TYPE ygms_de_loc_id,
           meas_vol     TYPE p DECIMALS 3,
         END OF lty_vol_mismatch.
  DATA: lt_cmdata    TYPE TABLE OF lty_cmdata,
        ls_cmdata    TYPE lty_cmdata,
        lt_mismatch  TYPE TABLE OF lty_cv_mismatch,
        ls_mismatch  TYPE lty_cv_mismatch,
        lv_answer    TYPE c LENGTH 1.
  " Volume matching data
  DATA: lt_b2b_raw      TYPE TABLE OF lty_b2b_vol,
        lt_receipt_total TYPE TABLE OF lty_receipt_total,
        ls_receipt_total TYPE lty_receipt_total,
        lt_vol_mismatch  TYPE TABLE OF lty_vol_mismatch,
        ls_vol_mismatch  TYPE lty_vol_mismatch,
        lt_ctp_ids       TYPE TABLE OF ygms_de_ongc_ctp,
        lv_vol_answer    TYPE c LENGTH 1.
  CONSTANTS: lc_cv_tolerance TYPE f VALUE '0.01'.  " Tolerance for FLTP vs packed decimal comparison
  cv_valid = abap_true.
  " 2.1: Pass dates and location ID from user inputs to YRXA_CMDATA
  "       Also fetch YYCUM_VOL for volume matching
  SELECT yydate yybus_location yyavg_gcv yyavg_ncv yycum_vol yytimestamp
    FROM yrxa_cmdata
    INTO TABLE lt_cmdata
    WHERE yybus_location IN s_loc
      AND yydate BETWEEN gv_date_from AND gv_date_to.
  IF lt_cmdata IS INITIAL.
    " No measurement data found - allow program to proceed
    RETURN.
  ENDIF.
  " 2.2: Keep only the latest records for each Gas Day-Location ID combination
  SORT lt_cmdata BY yydate yybus_location yytimestamp DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_cmdata COMPARING yydate yybus_location.
  " 2.3: Compare GCV and NCV with GT_GAS_RECEIPT for each Gas Day-Location ID
  LOOP AT lt_cmdata INTO ls_cmdata.
    READ TABLE gt_gas_receipt INTO DATA(ls_gas_rcpt)
      WITH KEY gas_day     = ls_cmdata-yydate
               location_id = ls_cmdata-yybus_location.
    IF sy-subrc = 0.
      IF abs( ls_cmdata-yyavg_gcv - ls_gas_rcpt-gcv ) > lc_cv_tolerance
        OR abs( ls_cmdata-yyavg_ncv - ls_gas_rcpt-ncv ) > lc_cv_tolerance.
        CLEAR ls_mismatch.
        ls_mismatch-gas_day     = ls_cmdata-yydate.
        ls_mismatch-location_id = ls_cmdata-yybus_location.
        ls_mismatch-ctp_id      = ls_gas_rcpt-ctp_id.
        ls_mismatch-gcv_meas    = ls_cmdata-yyavg_gcv.
        ls_mismatch-ncv_meas    = ls_cmdata-yyavg_ncv.
        ls_mismatch-gcv_receipt = ls_gas_rcpt-gcv.
        ls_mismatch-ncv_receipt = ls_gas_rcpt-ncv.
        APPEND ls_mismatch TO lt_mismatch.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF lt_mismatch IS NOT INITIAL.
    " CV Mismatch found - show popup
    cv_valid = abap_false.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        textline1      = 'Mismatch found in calorific values'
        textline2      = 'Click Yes to view details, No to cancel'
        titel          = 'CV Validation Failed'
        cancel_display = ' '
      IMPORTING
        answer         = lv_answer.
    IF lv_answer = 'J'.  " Yes - show details
      " Build detail ALV showing mismatches from both sources
      DATA: lt_detail_fcat TYPE slis_t_fieldcat_alv,
            ls_detail_fcat TYPE slis_fieldcat_alv.
      TYPES: BEGIN OF lty_cv_detail,
               gas_day     TYPE datum,
               location_id TYPE ygms_de_loc_id,
               ctp_id      TYPE ygms_de_ongc_ctp,
               gcv_meas    TYPE ygms_de_gcv,
               ncv_meas    TYPE ygms_de_ncv,
               gcv_receipt TYPE ygms_de_gcv,
               ncv_receipt TYPE ygms_de_ncv,
             END OF lty_cv_detail.
      DATA: lt_cv_detail TYPE TABLE OF lty_cv_detail,
            ls_cv_detail TYPE lty_cv_detail.
      " 2.4: Build detail table - one row per mismatch with both sources side by side
      LOOP AT lt_mismatch INTO ls_mismatch.
        CLEAR ls_cv_detail.
        ls_cv_detail-gas_day     = ls_mismatch-gas_day.
        ls_cv_detail-location_id = ls_mismatch-location_id.
        ls_cv_detail-ctp_id      = ls_mismatch-ctp_id.
        ls_cv_detail-gcv_meas    = ls_mismatch-gcv_meas.
        ls_cv_detail-ncv_meas    = ls_mismatch-ncv_meas.
        ls_cv_detail-gcv_receipt = ls_mismatch-gcv_receipt.
        ls_cv_detail-ncv_receipt = ls_mismatch-ncv_receipt.
        APPEND ls_cv_detail TO lt_cv_detail.
      ENDLOOP.
      " Build field catalog for detail ALV
      CLEAR ls_detail_fcat.
      ls_detail_fcat-fieldname = 'GAS_DAY'.
      ls_detail_fcat-seltext_l = 'Gas Day'.
      ls_detail_fcat-outputlen = 12.
      APPEND ls_detail_fcat TO lt_detail_fcat.
      CLEAR ls_detail_fcat.
      ls_detail_fcat-fieldname = 'LOCATION_ID'.
      ls_detail_fcat-seltext_l = 'Location ID'.
      ls_detail_fcat-outputlen = 12.
      APPEND ls_detail_fcat TO lt_detail_fcat.
      CLEAR ls_detail_fcat.
      ls_detail_fcat-fieldname = 'CTP_ID'.
      ls_detail_fcat-seltext_l = 'CTP ID'.
      ls_detail_fcat-outputlen = 12.
      APPEND ls_detail_fcat TO lt_detail_fcat.
      CLEAR ls_detail_fcat.
      ls_detail_fcat-fieldname = 'GCV_MEAS'.
      ls_detail_fcat-seltext_l = 'Measurement Table GCV'.
      ls_detail_fcat-seltext_m = 'Meas. Table GCV'.
      ls_detail_fcat-seltext_s = 'Meas GCV'.
      ls_detail_fcat-outputlen = 16.
      APPEND ls_detail_fcat TO lt_detail_fcat.
      CLEAR ls_detail_fcat.
      ls_detail_fcat-fieldname = 'NCV_MEAS'.
      ls_detail_fcat-seltext_l = 'Measurement Table NCV'.
      ls_detail_fcat-seltext_m = 'Meas. Table NCV'.
      ls_detail_fcat-seltext_s = 'Meas NCV'.
      ls_detail_fcat-outputlen = 16.
      APPEND ls_detail_fcat TO lt_detail_fcat.
      CLEAR ls_detail_fcat.
      ls_detail_fcat-fieldname = 'GCV_RECEIPT'.
      ls_detail_fcat-seltext_l = 'Receipt Table GCV'.
      ls_detail_fcat-seltext_m = 'Receipt Tbl GCV'.
      ls_detail_fcat-seltext_s = 'Rcpt GCV'.
      ls_detail_fcat-outputlen = 16.
      APPEND ls_detail_fcat TO lt_detail_fcat.
      CLEAR ls_detail_fcat.
      ls_detail_fcat-fieldname = 'NCV_RECEIPT'.
      ls_detail_fcat-seltext_l = 'Receipt Table NCV'.
      ls_detail_fcat-seltext_m = 'Receipt Tbl NCV'.
      ls_detail_fcat-seltext_s = 'Rcpt NCV'.
      ls_detail_fcat-outputlen = 16.
      APPEND ls_detail_fcat TO lt_detail_fcat.
      " Display detail ALV as popup
      DATA: ls_detail_layout TYPE slis_layout_alv.
      ls_detail_layout-zebra = 'X'.
      ls_detail_layout-colwidth_optimize = 'X'.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program = sy-repid
          is_layout          = ls_detail_layout
          it_fieldcat        = lt_detail_fcat
          i_screen_start_column = 10
          i_screen_start_line   = 5
          i_screen_end_column   = 150
          i_screen_end_line     = 25
        TABLES
          t_outtab           = lt_cv_detail
        EXCEPTIONS
          program_error = 1
          OTHERS        = 2.
    ENDIF.
    " CV validation failed - exit form, do not proceed to volume matching
    RETURN.
  ENDIF.

  "-----------------------------------------------------------------------
  " Volume Matching: Receipt volume (YRGA_CST_B2B_1) vs Measurement
  " volume (YYCUM_VOL from YRXA_CMDATA) for each CTP ID and Gas Day
  "-----------------------------------------------------------------------

  " V.1: Collect all CTP IDs from location mapping
  LOOP AT gt_loc_ctp_map INTO DATA(ls_map_v).
    COLLECT ls_map_v-ongc_ctp_id INTO lt_ctp_ids.
  ENDLOOP.
  IF lt_ctp_ids IS INITIAL.
    RETURN.
  ENDIF.

  " V.1a: Fetch B2B data for all CTP IDs and gas days
  SELECT gas_day ctp_id ongc_material qty_scm time_stamp
    FROM yrga_cst_b2b_1
    INTO TABLE lt_b2b_raw
    FOR ALL ENTRIES IN lt_ctp_ids
    WHERE ctp_id  = lt_ctp_ids-table_line
      AND gas_day BETWEEN gv_date_from AND gv_date_to
      AND qty_scm > 0.
  IF lt_b2b_raw IS INITIAL.
    RETURN.
  ENDIF.

  " V.1b: Keep latest data for each CTP ID - Gas Day - ONGC Material
  SORT lt_b2b_raw BY ctp_id gas_day ongc_material ASCENDING time_stamp DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_b2b_raw COMPARING ctp_id gas_day ongc_material.

  " V.1c: Sum volume (Sm3) of all materials per CTP ID - Gas Day
  LOOP AT lt_b2b_raw INTO DATA(ls_b2b_v).
    ls_receipt_total-gas_day = ls_b2b_v-gas_day.
    ls_receipt_total-ctp_id  = ls_b2b_v-ctp_id.
    ls_receipt_total-qty_scm = ls_b2b_v-qty_scm.
    COLLECT ls_receipt_total INTO lt_receipt_total.
    CLEAR ls_receipt_total.
  ENDLOOP.

  " Round receipt totals to 3 decimal places
  LOOP AT lt_receipt_total ASSIGNING FIELD-SYMBOL(<fs_rcpt_total>).
    <fs_rcpt_total>-qty_scm = round( val = <fs_rcpt_total>-qty_scm dec = 3 ).
  ENDLOOP.

  " V.2: Match volumes - use lt_cmdata already fetched above (has YYCUM_VOL)
  " For each CTP ID - Gas Day, find Location ID and compare with YYCUM_VOL
  LOOP AT lt_receipt_total INTO ls_receipt_total.
    " Find Location ID for this CTP ID
    READ TABLE gt_loc_ctp_map INTO DATA(ls_map_v3)
      WITH KEY ongc_ctp_id = ls_receipt_total-ctp_id.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    " Find measurement volume from lt_cmdata (already fetched and de-duped)
    READ TABLE lt_cmdata INTO ls_cmdata
      WITH KEY yydate         = ls_receipt_total-gas_day
               yybus_location = ls_map_v3-gail_loc_id.
    IF sy-subrc = 0.
      DATA(lv_meas_vol) = round( val = ls_cmdata-yycum_vol dec = 3 ).
      " Compare receipt volume with measurement volume
      IF ls_receipt_total-qty_scm <> lv_meas_vol.
        CLEAR ls_vol_mismatch.
        ls_vol_mismatch-gas_day     = ls_receipt_total-gas_day.
        ls_vol_mismatch-ctp_id      = ls_receipt_total-ctp_id.
        ls_vol_mismatch-receipt_vol = ls_receipt_total-qty_scm.
        ls_vol_mismatch-location_id = ls_map_v3-gail_loc_id.
        ls_vol_mismatch-meas_vol    = lv_meas_vol.
        APPEND ls_vol_mismatch TO lt_vol_mismatch.
      ENDIF.
    ENDIF.
  ENDLOOP.

  " V.3: If volume mismatch found, show popup and block allocation
  IF lt_vol_mismatch IS NOT INITIAL.
    cv_valid = abap_false.
    CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
      EXPORTING
        textline1      = 'Volume mismatch detected. Allocation cannot proceed.'
        textline2      = 'Click Yes to view details, No to go back'
        titel          = 'Volume Mismatch - Allocation Blocked'
        cancel_display = ' '
      IMPORTING
        answer         = lv_vol_answer.
    IF lv_vol_answer = 'J'.  " Yes = Details
      " Build field catalog for mismatch detail ALV
      DATA: lt_vol_fcat TYPE slis_t_fieldcat_alv,
            ls_vol_fcat TYPE slis_fieldcat_alv.
      CLEAR ls_vol_fcat.
      ls_vol_fcat-fieldname = 'GAS_DAY'.
      ls_vol_fcat-seltext_l = 'Gas Day'.
      ls_vol_fcat-outputlen = 12.
      APPEND ls_vol_fcat TO lt_vol_fcat.
      CLEAR ls_vol_fcat.
      ls_vol_fcat-fieldname = 'CTP_ID'.
      ls_vol_fcat-seltext_l = 'CTP ID'.
      ls_vol_fcat-outputlen = 15.
      APPEND ls_vol_fcat TO lt_vol_fcat.
      CLEAR ls_vol_fcat.
      ls_vol_fcat-fieldname = 'RECEIPT_VOL'.
      ls_vol_fcat-seltext_l = 'Receipt Vol. (Sm3)'.
      ls_vol_fcat-seltext_m = 'Receipt Vol.'.
      ls_vol_fcat-outputlen = 18.
      APPEND ls_vol_fcat TO lt_vol_fcat.
      CLEAR ls_vol_fcat.
      ls_vol_fcat-fieldname = 'LOCATION_ID'.
      ls_vol_fcat-seltext_l = 'Location ID'.
      ls_vol_fcat-outputlen = 12.
      APPEND ls_vol_fcat TO lt_vol_fcat.
      CLEAR ls_vol_fcat.
      ls_vol_fcat-fieldname = 'MEAS_VOL'.
      ls_vol_fcat-seltext_l = 'Measurement Vol. (Sm3)'.
      ls_vol_fcat-seltext_m = 'Meas. Vol.'.
      ls_vol_fcat-outputlen = 18.
      APPEND ls_vol_fcat TO lt_vol_fcat.
      " Display mismatch detail ALV as popup
      DATA: ls_vol_layout TYPE slis_layout_alv.
      ls_vol_layout-zebra = 'X'.
      ls_vol_layout-colwidth_optimize = 'X'.
      SORT lt_vol_mismatch BY gas_day ctp_id.
      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_callback_program    = sy-repid
          is_layout             = ls_vol_layout
          it_fieldcat           = lt_vol_fcat
          i_screen_start_column = 10
          i_screen_start_line   = 5
          i_screen_end_column   = 120
          i_screen_end_line     = 25
        TABLES
          t_outtab              = lt_vol_mismatch
        EXCEPTIONS
          program_error = 1
          OTHERS        = 2.
    ENDIF.
    " Volume mismatch blocks the allocation process - do not continue
    RETURN.
  ENDIF.
ENDFORM.