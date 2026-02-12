*&---------------------------------------------------------------------*
*& Report YGMS_CST_PURCHASE_MAIN
*& Description: ONGC CST Purchase Data - Gas Receipt Processing
*& Version: 4.0 - Editable ALV with buttons
*&---------------------------------------------------------------------*
REPORT ygms_cst_purchase_main.

*----------------------------------------------------------------------*
* Type Definitions
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_gas_receipt,
         gas_day       TYPE datum,
         ctp_id        TYPE ygms_de_ongc_ctp,
         location_id   TYPE ygms_de_loc_id,
         ongc_material TYPE ygms_de_ongc_mat,
         material      TYPE ygms_de_gail_mat,
         qty_scm       TYPE ygms_de_qty_scm,
         gcv           TYPE ygms_de_gcv,
         ncv           TYPE ygms_de_ncv,
         qty_mbg       TYPE ygms_de_qty_mbg,
         ongc_id       TYPE c LENGTH 9,
       END OF ty_gas_receipt.

TYPES: BEGIN OF ty_loc_ctp_map,
         gail_loc_id TYPE ygms_de_loc_id,
         ongc_ctp_id TYPE ygms_de_ongc_ctp,
       END OF ty_loc_ctp_map.

* ALV Display structure - matches image layout
TYPES: BEGIN OF ty_alv_display,
         exclude     TYPE c LENGTH 1,       " Checkbox for exclude
         state_code  TYPE regio,            " State Code (BH, OD, UP, GJ)
         state       TYPE bezei20,          " State Name
         location_id TYPE ygms_de_loc_id,   " Location ID
         material    TYPE ygms_de_gail_mat, " Material
         total_mbg   TYPE p DECIMALS 3,     " Total, MBG
         total_scm   TYPE p DECIMALS 3,     " Total, Sm3
            gcv           TYPE ygms_de_gcv,
         ncv           TYPE ygms_de_ncv,
         day01       TYPE p DECIMALS 3,     " Day 1 quantity
         day02       TYPE p DECIMALS 3,     " Day 2 quantity
         day03       TYPE p DECIMALS 3,     " Day 3 quantity
         day04       TYPE p DECIMALS 3,     " Day 4 quantity
         day05       TYPE p DECIMALS 3,     " Day 5 quantity
         day06       TYPE p DECIMALS 3,     " Day 6 quantity
         day07       TYPE p DECIMALS 3,     " Day 7 quantity
         day08       TYPE p DECIMALS 3,     " Day 8 quantity
         day09       TYPE p DECIMALS 3,     " Day 9 quantity
         day10       TYPE p DECIMALS 3,     " Day 10 quantity
         day11       TYPE p DECIMALS 3,     " Day 11 quantity
         day12       TYPE p DECIMALS 3,     " Day 12 quantity
         day13       TYPE p DECIMALS 3,     " Day 13 quantity
         day14       TYPE p DECIMALS 3,     " Day 14 quantity
         day15       TYPE p DECIMALS 3,     " Day 15 quantity
         day16       TYPE p DECIMALS 3,     " Day 16 quantity
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

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
DATA: gv_loc_id TYPE ygms_de_loc_id.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_loc  FOR gv_loc_id,
                  s_date FOR sy-datum OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: gt_gas_receipt TYPE TABLE OF ty_gas_receipt,
      gt_loc_ctp_map TYPE TABLE OF ty_loc_ctp_map,
      gv_date_from   TYPE datum,
      gv_date_to     TYPE datum.

* ALV Display table
DATA: gt_alv_display TYPE TABLE OF ty_alv_display,
      gs_alv_display TYPE ty_alv_display.

* ALV Grid objects
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

*----------------------------------------------------------------------*
* Class Definition for ALV Event Handler
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object e_interactive,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_toolbar.
    DATA: ls_toolbar TYPE stb_button.

    " Add separator
    CLEAR ls_toolbar.
    ls_toolbar-butn_type = 3. " Separator
    APPEND ls_toolbar TO e_object->mt_toolbar.

    " Allocate button
    CLEAR ls_toolbar.
    ls_toolbar-function  = 'ALLOCATE'.
    ls_toolbar-icon      = icon_calculation.
    ls_toolbar-text      = 'Allocate'.
    ls_toolbar-quickinfo = 'Allocate quantities'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    " Validate button
    CLEAR ls_toolbar.
    ls_toolbar-function  = 'VALIDATE'.
    ls_toolbar-icon      = icon_check.
    ls_toolbar-text      = 'Validate'.
    ls_toolbar-quickinfo = 'Validate data'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    " Edit button
    CLEAR ls_toolbar.
    ls_toolbar-function  = 'EDIT'.
    ls_toolbar-icon      = icon_change.
    ls_toolbar-text      = 'Edit'.
    ls_toolbar-quickinfo = 'Edit data'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    " Save button
    CLEAR ls_toolbar.
    ls_toolbar-function  = 'SAVE'.
    ls_toolbar-icon      = icon_system_save.
    ls_toolbar-text      = 'Save'.
    ls_toolbar-quickinfo = 'Save data'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    " Reset button
    CLEAR ls_toolbar.
    ls_toolbar-function  = 'RESET'.
    ls_toolbar-icon      = icon_refresh.
    ls_toolbar-text      = 'Reset'.
    ls_toolbar-quickinfo = 'Reset data'.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    " Send button
    CLEAR ls_toolbar.
    ls_toolbar-function  = 'SEND'.
    ls_toolbar-icon      = icon_mail.
    ls_toolbar-text      = 'Send'.
    ls_toolbar-quickinfo = 'Send data'.
    APPEND ls_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'ALLOCATION'.
        PERFORM handle_allocate.
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
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  READ TABLE s_date INTO DATA(ls_date) INDEX 1.
  gv_date_from = ls_date-low.
  gv_date_to   = ls_date-high.

  " Step 1: Fetch Location ID - CTP ID mappings
  PERFORM fetch_location_ctp_mappings.
  CHECK gt_loc_ctp_map IS NOT INITIAL.

  " Step 2: Fetch B2B data
  PERFORM fetch_b2b_data.
  CHECK gt_gas_receipt IS NOT INITIAL.

  " Step 3: Map Location IDs to CTP IDs
  PERFORM map_location_ids.

  " Step 4: Map Material names
  PERFORM map_material_names.

  " Step 5: Fetch data from YRXR098
  PERFORM fetch_data_yrxr098.

  " Step 6: Build ALV display table
  PERFORM build_alv_display_table.

  " Step 7: Display editable ALV
  PERFORM display_editable_alv.

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
  ELSE.
    SELECT * FROM yrga_cst_loc_map
      INTO TABLE lt_loc_map
      WHERE valid_from <= gv_date_from
        AND valid_to   >= gv_date_to.
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
    MESSAGE s000(ygms_msg) WITH 'No B2B data found for the selected criteria'.
    RETURN.
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
ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_ALV_DISPLAY_TABLE
*&---------------------------------------------------------------------*
FORM build_alv_display_table.
  DATA: lt_state_info TYPE TABLE OF t005u,
        ls_alv        TYPE ty_alv_display,
        lv_day_num    TYPE i,
        lv_day_field  TYPE string.



 data it_final_temp TYPE TABLE OF ty_final1.

 move it_final_main[] to it_final_temp[].
  sort it_final_temp by matnr.
  delete ADJACENT DUPLICATES FROM it_final_temp COMPARING matnr.
 loop at it_final_temp into data(wa_final_temp).
   READ TABLE it_final_main TRANSPORTING NO FIELDS with key
   regio = 'GJ' matnr = wa_final_temp-matnr.
   if sy-subrc <> 0.
clear wa_final_main.
wa_final_main-matnr = wa_final_temp-matnr.
wa_final_main-regio = 'GJ'.
wa_final_main-regio_desc = 'Gujrat'.
wa_final_main-empst = wa_final_temp-empst.
append wa_final_main to it_final_main.
   endif.
 ENDLOOP.


  LOOP AT it_final_main INTO wa_final_main.
    ls_alv-state_code = wa_final_main-regio.
    ls_alv-state      = wa_final_main-regio_desc.
    ls_alv-material   = wa_final_main-matnr.
    ls_alv-location_id = wa_final_main-empst.
    APPEND ls_alv TO gt_alv_display.
  ENDLOOP.

*  " Get state descriptions
*  SELECT * FROM t005u INTO TABLE lt_state_info
*    WHERE spras = sy-langu
*      AND land1 = 'IN'.
*
*  " Get unique combinations of Location ID + Material
*  DATA: BEGIN OF ls_key,
*          location_id TYPE ygms_de_loc_id,
*          material    TYPE ygms_de_gail_mat,
*        END OF ls_key,
*        lt_keys LIKE TABLE OF ls_key.
*
*  LOOP AT gt_gas_receipt INTO DATA(ls_receipt).
*    ls_key-location_id = ls_receipt-location_id.
*    ls_key-material    = ls_receipt-material.
*    COLLECT ls_key INTO lt_keys.
*  ENDLOOP.
*
*  " Build ALV display table
*  LOOP AT lt_keys INTO ls_key.
*    CLEAR ls_alv.
*    ls_alv-location_id = ls_key-location_id.
*    ls_alv-material    = ls_key-material.
*
*    " Get state info from it_final_main if available
*    READ TABLE it_final_main INTO wa_final_main INDEX 1.
*    IF sy-subrc = 0.
*      ls_alv-state_code = wa_final_main-regio.
*      ls_alv-state      = wa_final_main-regio_desc.
*    ENDIF.
*
*    " Sum quantities by day
*    LOOP AT gt_gas_receipt INTO ls_receipt
*      WHERE location_id = ls_key-location_id
*        AND material    = ls_key-material.
*
*      " Calculate day number from gas_day
*      lv_day_num = ls_receipt-gas_day+6(2).
*
*      " Add to total
*      ls_alv-total_mbg = ls_alv-total_mbg + ls_receipt-qty_mbg.
*      ls_alv-total_scm = ls_alv-total_scm + ls_receipt-qty_scm.
*
*      " Assign to day field
*      CASE lv_day_num.
*        WHEN 1.  ls_alv-day01 = ls_alv-day01 + ls_receipt-qty_scm.
*        WHEN 2.  ls_alv-day02 = ls_alv-day02 + ls_receipt-qty_scm.
*        WHEN 3.  ls_alv-day03 = ls_alv-day03 + ls_receipt-qty_scm.
*        WHEN 4.  ls_alv-day04 = ls_alv-day04 + ls_receipt-qty_scm.
*        WHEN 5.  ls_alv-day05 = ls_alv-day05 + ls_receipt-qty_scm.
*        WHEN 6.  ls_alv-day06 = ls_alv-day06 + ls_receipt-qty_scm.
*        WHEN 7.  ls_alv-day07 = ls_alv-day07 + ls_receipt-qty_scm.
*        WHEN 8.  ls_alv-day08 = ls_alv-day08 + ls_receipt-qty_scm.
*        WHEN 9.  ls_alv-day09 = ls_alv-day09 + ls_receipt-qty_scm.
*        WHEN 10. ls_alv-day10 = ls_alv-day10 + ls_receipt-qty_scm.
*        WHEN 11. ls_alv-day11 = ls_alv-day11 + ls_receipt-qty_scm.
*        WHEN 12. ls_alv-day12 = ls_alv-day12 + ls_receipt-qty_scm.
*        WHEN 13. ls_alv-day13 = ls_alv-day13 + ls_receipt-qty_scm.
*        WHEN 14. ls_alv-day14 = ls_alv-day14 + ls_receipt-qty_scm.
*        WHEN 15. ls_alv-day15 = ls_alv-day15 + ls_receipt-qty_scm.
*        WHEN 16. ls_alv-day16 = ls_alv-day16 + ls_receipt-qty_scm.
*      ENDCASE.
*    ENDLOOP.
*
*    APPEND ls_alv TO gt_alv_display.
*  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_EDITABLE_ALV
*&---------------------------------------------------------------------*
FORM display_editable_alv.
  DATA: ls_fieldcat TYPE lvc_s_fcat,
        lv_day      TYPE i,
        lv_fname    TYPE lvc_fname,
        lv_date     TYPE datum,
        lv_date_str TYPE c LENGTH 10.

  " Build field catalog
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'EXCLUDE'.
  ls_fieldcat-coltext   = 'Exclude'.
  ls_fieldcat-checkbox  = abap_true.
  ls_fieldcat-edit      = abap_true.
  ls_fieldcat-outputlen = 7.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'STATE_CODE'.
  ls_fieldcat-coltext   = 'State Code'.
  ls_fieldcat-outputlen = 10.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'STATE'.
  ls_fieldcat-coltext   = 'State'.
  ls_fieldcat-outputlen = 15.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'LOCATION_ID'.
  ls_fieldcat-coltext   = 'Location ID'.
  ls_fieldcat-outputlen = 12.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MATERIAL'.
  ls_fieldcat-coltext   = 'Material'.
  ls_fieldcat-outputlen = 18.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TOTAL_MBG'.
  ls_fieldcat-coltext   = 'Total, MBG'.
  ls_fieldcat-outputlen = 12.
  ls_fieldcat-do_sum    = abap_true.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'TOTAL_SCM'.
  ls_fieldcat-coltext   = 'Total, Sm3'.
  ls_fieldcat-outputlen = 12.
  ls_fieldcat-do_sum    = abap_true.
  APPEND ls_fieldcat TO gt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GCV'.
  ls_fieldcat-coltext   = 'Average GCV'.
  ls_fieldcat-outputlen = 12.
  ls_fieldcat-do_sum    = abap_true.
  APPEND ls_fieldcat TO gt_fieldcat.


  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NCV'.
  ls_fieldcat-coltext   = 'Average NCV'.
  ls_fieldcat-outputlen = 12.
  ls_fieldcat-do_sum    = abap_true.
  APPEND ls_fieldcat TO gt_fieldcat.


  " Add day columns with date headers
  lv_date = gv_date_from.
  DO 15 TIMES.
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
*      WHEN 16. ls_fieldcat-fieldname = 'DAY16'.
    ENDCASE.

    " Format date as DD-MM-YYYY
    WRITE lv_date TO lv_date_str DD/MM/YYYY.
    REPLACE ALL OCCURRENCES OF '/' IN lv_date_str WITH '-'.
    ls_fieldcat-coltext   = lv_date_str.
    ls_fieldcat-outputlen = 12.
    ls_fieldcat-edit      = abap_true.
    ls_fieldcat-do_sum    = abap_true.
    APPEND ls_fieldcat TO gt_fieldcat.

    lv_date = lv_date + 1.
  ENDDO.

  " Set layout - enable edit mode
  gs_layout-cwidth_opt = abap_true.
  gs_layout-zebra      = abap_true.
  gs_layout-sel_mode   = 'A'.
  gs_layout-edit       = abap_true.

  " Call ALV Grid
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
  SET PF-STATUS 'ZALV_STATUS'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form USER_COMMAND
*&---------------------------------------------------------------------*
FORM user_command USING r_ucomm     TYPE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.

  " Get ALV grid reference
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lr_grid.

  " Check for data changes
  lr_grid->check_changed_data( ).

  CASE r_ucomm.
    WHEN 'ALLOCATION'.
      PERFORM handle_allocate.
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

  " Refresh display
  rs_selfield-refresh = abap_true.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form HANDLE_ALLOCATE
*&---------------------------------------------------------------------*
FORM handle_allocate.

  TYPES : BEGIN OF ty_sales,
            matnr          TYPE matnr,
            qty_mbg        TYPE ygms_de_qty_mbg,
            qty_mbg_supply TYPE ygms_de_qty_mbg,
            qty_mbg_diff   TYPE ygms_de_qty_mbg,
            qty_allocated  TYPE ygms_de_qty_mbg,
            qty_access     TYPE ygms_de_qty_mbg,

          END OF ty_sales.

  TYPES : BEGIN OF ty_asales,
          regio type regio,
*            matnr          TYPE matnr,
                      qty_mbg        TYPE ygms_de_qty_mbg,
*            qty_mbg_supply TYPE ygms_de_qty_mbg,
*            qty_mbg_diff   TYPE ygms_de_qty_mbg,
*            qty_allocated  TYPE ygms_de_qty_mbg,
*            qty_access     TYPE ygms_de_qty_mbg,

          END OF ty_asales.

  DATA l_left TYPE ygms_de_qty_mbg.

  TYPES : BEGIN OF ty_state,
            state_code    TYPE regio,            " State Code (BH, OD, UP, GJ)
            state         TYPE bezei20,          " State Name
            matnr         TYPE matnr,
            qty_mbg       TYPE ygms_de_qty_mbg,
            qty_mbg_diff  TYPE ygms_de_qty_mbg,
            qty_allocated TYPE ygms_de_qty_mbg,
            percentage    TYPE YGMS_ONGC_PERCENTAGE,"p02_agprm,
          END OF  ty_state.


  DATA : it_sales  TYPE TABLE OF ty_sales,
         wa_sales  TYPE ty_sales,
         it_state  TYPE TABLE OF ty_state,
         wa_state  TYPE ty_state,
         it_state1 TYPE TABLE OF ty_state,
         it_asales type table of ty_asales,
         wa_asales type ty_asales.
*           it_supply TYPE table of ty_sales,
*           wa_supply type ty_sales.


loop at it_final_main into wa_final_main .
*      wa_asales-matnr = wa_final_main-matnr.
      wa_asales-qty_mbg = wa_final_main-matnr1.
      wa_asales-regio = wa_final_main-regio.
      COLLECT wa_asales INTO it_asales.
      CLEAR wa_asales.
ENDLOOP.

sort it_asales by qty_mbg DESCENDING.


  LOOP AT it_final_main INTO wa_final_main ."where regio <> 'GJ'.
    IF wa_final_main-regio <> 'GJ'.
      wa_sales-matnr = wa_final_main-matnr.
      wa_sales-qty_mbg = wa_final_main-matnr1.
      COLLECT wa_sales INTO it_sales.
      CLEAR wa_sales.
    ENDIF.
    CLEAR wa_state.
    wa_state-state_code = wa_final_main-regio.
    wa_state-state = wa_final_main-regio_desc.
    wa_state-qty_mbg = wa_final_main-matnr1.
    wa_state-matnr = wa_final_main-matnr.
    COLLECT wa_state  INTO it_state.
  ENDLOOP.

  LOOP AT gt_gas_receipt INTO DATA(wa_gas_receipt).
    wa_sales-matnr = wa_gas_receipt-material.
    wa_sales-qty_mbg_supply = wa_gas_receipt-qty_mbg.
    COLLECT wa_sales INTO it_sales.
    CLEAR wa_sales.
  ENDLOOP.

  LOOP AT it_sales ASSIGNING FIELD-SYMBOL(<fs_sales>).
    <fs_sales>-qty_mbg_diff = <fs_sales>-qty_mbg_supply - <fs_sales>-qty_mbg.

  ENDLOOP.
  SORT it_sales BY qty_mbg_diff DESCENDING.
*  SORT it_state BY state ASCENDING.
  sort it_asales by qty_mbg DESCENDING.

CLEAR l_left.

*loop at it_sales into wa_sales.
*   READ TABLE it_state ASSIGNING FIELD-SYMBOL(<fs_state>) WITH KEY state_code = 'GJ' matnr = wa_sales-matnr.
*   if sy-subrc <> 0.
*     clear wa_state.
*              wa_state-state_code = 'GJ'.
*         wa_state-matnr = wa_sales-matnr.
**         wa_state-qty_allocated = l_left.
*         append wa_state to it_state.
*   endif.
*ENDLOOP.

*BREAK-POINT.
data l_exit type flag.

loop at it_sales ASSIGNING <fs_sales> .
  clear l_exit.
l_left = <fs_sales>-qty_mbg_diff.
if l_left < 0.
loop at it_asales into wa_asales .

loop at  it_state ASSIGNING FIELD-SYMBOL(<fs_state>) WHERE
matnr = <fs_sales>-matnr and state_code = wa_asales-regio.
if sy-subrc = 0.
if <fs_state>-qty_mbg > abs( l_left ).
<fs_state>-qty_allocated = <fs_state>-qty_mbg + l_left.
l_exit = 'X'.
exit.
else.
l_left = l_left + <fs_state>-qty_mbg.
clear <fs_state>-qty_allocated .
endif.
ENDIF.
endloop.

if l_exit = 'X'.
  exit.
  endif.

ENDLOOP.
else.
       READ TABLE it_state ASSIGNING <fs_state> WITH KEY state_code = 'GJ' matnr = <fs_sales>-matnr.
      IF sy-subrc = 0.
*        <fs_sales>-qty_allocated = l_left + <fs_sales>-qty_allocated .
        <fs_state>-qty_allocated = l_left + <fs_state>-qty_allocated.
        CLEAR l_left.
*        else.
*         wa_sales-state_cdoe = 'GJ'.
*         wa_sales-material = <fs_sales>-amtnr.
*         wa_sales-qty_allocated = l_left.
*         append wa_sales to it_sales.
      ENDIF.

endif.

clear l_left.
ENDLOOP.

*  LOOP AT it_sales ASSIGNING <fs_sales>.
*    CLEAR l_left .
*    l_left =   <fs_sales>-qty_mbg_diff.
*    IF l_left < 0.
*   loop at it_asales into wa_asales where matnr = <fs_sales>-matnr.
*      if WA_asales-qty_mbg > abs( l_left ).
*       LOOP AT it_state ASSIGNING FIELD-SYMBOL(<fs_state>) WHERE state_code = wa_asales-regio AND matnr = <fs_sales>-matnr.
*        <fs_sales>-qty_allocated = wa_asales-qty_mbg - l_left.
*        exit.
*        ENDLOOP.
*        else.
*
*      endif.
*   ENDLOOP.
*    ENDIF.
*      LOOP AT it_state ASSIGNING FIELD-SYMBOL(<fs_state>) WHERE state_code <> 'GJ' AND matnr = <fs_sales>-matnr.
*        IF <fs_state>-qty_mbg > abs( l_left ).
*          <fs_sales>-qty_allocated = l_left * -1.
*          <fs_sales>-qty_access = <fs_sales>-qty_mbg - <fs_sales>-qty_allocated .
*          <fs_state>-qty_allocated = <fs_sales>-qty_allocated.
*          CLEAR l_left.
*          EXIT.
*        ELSE.
*          <fs_sales>-qty_allocated = ( l_left * -1 ) - <fs_sales>-qty_mbg.
*          l_left = l_left +  <fs_sales>-qty_allocated .
*          <fs_state>-qty_allocated = <fs_sales>-qty_allocated.
*          IF l_left > 0 .
*            <fs_sales>-qty_allocated = <fs_sales>-qty_allocated - l_left.
*            <fs_state>-qty_allocated = <fs_sales>-qty_allocated.
*            CLEAR l_left.
*          ENDIF.
*        ENDIF.
**<fs_sales>-qty_allocated = <fs_state>-qty_mbg + l_left.
**l_left =  <fs_sales>-qty_allocated + l_left.
**if l_left >= 0.
**  exit.
**  endif.
*      ENDLOOP.
*    ELSE.
*      READ TABLE it_state ASSIGNING <fs_state> WITH KEY state_code = 'GJ' matnr = <fs_sales>-matnr.
*      IF sy-subrc = 0.
*        <fs_sales>-qty_allocated = l_left + <fs_sales>-qty_allocated .
*        <fs_state>-qty_allocated = l_left.
*        CLEAR l_left.
*      ENDIF.
*    ENDIF.
**endif.
*  ENDLOOP.

loop at it_state ASSIGNING <fs_state> where qty_allocated is INITIAL.
<fs_state>-qty_allocated = <fs_state>-qty_mbg.
ENDLOOP.


  MOVE it_state[] TO it_state1[].
  SORT it_state1 BY matnr.
  DELETE ADJACENT DUPLICATES FROM it_state1 COMPARING matnr.

  LOOP AT it_state1 INTO DATA(wa_state1).
    CLEAR l_left.
    LOOP AT it_state INTO wa_state WHERE matnr = wa_state1-matnr.
      l_left = l_left + wa_state-qty_allocated.
    ENDLOOP.

    LOOP AT it_state ASSIGNING  <fs_state> WHERE matnr = wa_state1-matnr.
      <fs_state>-percentage = ( <fs_state>-qty_allocated / l_left ) * 100.
    ENDLOOP.

  ENDLOOP.
data l_day type char10.
data l_index(2) type n.
data l_date type sy-datum.
data l_ncv type ygms_de_gcv.
data l_gcv type ygms_de_gcv.

loop at it_state into wa_state where percentage is not INITIAL.
*if wa_state-percentage is INITIAL.
*  wa_state-percentage = 1.
*  endif.

LOOP AT gt_alv_display ASSIGNING FIELD-SYMBOL(<fs_alv>) where state_code = wa_state-state_code AND
  material = wa_state-matnr.
  clear l_index.
  clear : l_ncv,l_gcv.
  l_date = s_date-low.
  do 15 times.
    l_index = l_index + 1.
        clear l_day.
    CONCATENATE 'DAY' l_index into l_day.
  ASSIGN COMPONENT l_day of STRUCTURE <fs_alv> to FIELD-SYMBOL(<fs_day>).
if sy-subrc = 0.
 READ TABLE GT_GAS_RECEIPT into wa_gas_receipt with KEY
 gas_day = l_date
 material = wa_state-matnr.
 if sy-subrc = 0.
  <fs_day> = ( wa_gas_receipt-qty_mbg * wa_state-percentage ) / 100.
  <fs_alv>-total_mbg = <fs_alv>-total_mbg + <fs_day>.
  data(l_day_sm3) = ( wa_gas_receipt-qty_scm * wa_state-percentage ) / 100.
  <fs_alv>-total_scm = <fs_alv>-total_scm + l_day_sm3.
  l_gcv =  ( l_day_sm3 * wa_gas_receipt-gcv ) + l_gcv.
  l_ncv = ( l_day_sm3 * wa_gas_receipt-ncv ) + l_ncv.
  endif.
ENDIF.
l_date = l_date + 1.
ENDDO.
<fs_alv>-gcv = l_gcv / <fs_alv>-total_scm.
<fs_alv>-ncv = l_ncv / <fs_alv>-total_scm.
clear l_day_sm3.
endloop.
ENDLOOP.

  MESSAGE s000(ygms_msg) WITH 'Allocate function triggered'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form HANDLE_VALIDATE
*&---------------------------------------------------------------------*
FORM handle_validate.
  DATA: lv_errors TYPE i VALUE 0.

  LOOP AT gt_alv_display INTO gs_alv_display.
    " Validate totals match sum of days
    DATA(lv_sum) = gs_alv_display-day01 + gs_alv_display-day02 +
                   gs_alv_display-day03 + gs_alv_display-day04 +
                   gs_alv_display-day05 + gs_alv_display-day06 +
                   gs_alv_display-day07 + gs_alv_display-day08 +
                   gs_alv_display-day09 + gs_alv_display-day10 +
                   gs_alv_display-day11 + gs_alv_display-day12 +
                   gs_alv_display-day13 + gs_alv_display-day14 +
                   gs_alv_display-day15 + gs_alv_display-day16.

    IF lv_sum <> gs_alv_display-total_scm.
      lv_errors = lv_errors + 1.
    ENDIF.
  ENDLOOP.

  IF lv_errors > 0.
    MESSAGE e000(ygms_msg) WITH lv_errors 'validation errors found'.
  ELSE.
    MESSAGE s000(ygms_msg) WITH 'Validation successful'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form HANDLE_EDIT
*&---------------------------------------------------------------------*
FORM handle_edit.
  MESSAGE s000(ygms_msg) WITH 'Edit mode enabled'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form HANDLE_SAVE
*&---------------------------------------------------------------------*
FORM handle_save.
  " Save data to database
  MESSAGE s000(ygms_msg) WITH 'Data saved successfully'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form HANDLE_RESET
*&---------------------------------------------------------------------*
FORM handle_reset.
  " Reset to original data
  CLEAR gt_alv_display.
  PERFORM build_alv_display_table.
  MESSAGE s000(ygms_msg) WITH 'Data reset to original values'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form HANDLE_SEND
*&---------------------------------------------------------------------*
FORM handle_send.
  MESSAGE s000(ygms_msg) WITH 'Send function triggered'.
ENDFORM.
