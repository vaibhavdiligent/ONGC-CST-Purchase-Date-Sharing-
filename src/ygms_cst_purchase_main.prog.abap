*&---------------------------------------------------------------------*
*& Report YGMS_CST_PURCHASE_MAIN
*& Description: ONGC CST Purchase Data - Gas Receipt Display
*& Version: 3.0 - Rewritten with new logic for editable ALV
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
         gail_loc_id  TYPE ygms_de_loc_id,
         ongc_ctp_id  TYPE ygms_de_ongc_ctp,
       END OF ty_loc_ctp_map.

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
      go_grid        TYPE REF TO cl_gui_alv_grid,
      go_container   TYPE REF TO cl_gui_custom_container,
      gv_date_from   TYPE datum,
      gv_date_to     TYPE datum.

*----------------------------------------------------------------------*
* Class Definition for ALV Event Handler
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
               IMPORTING er_data_changed.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_data_changed.
    " Handle data changes if needed
  ENDMETHOD.
ENDCLASS.

DATA: go_handler TYPE REF TO lcl_event_handler.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  " Set default date range (current month)
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
  " Get date range
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

  " Step 5: Display editable ALV
  PERFORM display_editable_alv.

*&---------------------------------------------------------------------*
*& Form FETCH_LOCATION_CTP_MAPPINGS
*& 3.1.1 - Fetch Location ID - CTP ID combinations from YRGA_CST_LOC_MAP
*&---------------------------------------------------------------------*
FORM fetch_location_ctp_mappings.
  DATA: lt_loc_map TYPE TABLE OF yrga_cst_loc_map.

  " Fetch mappings where:
  " - From input date >= valid_from
  " - To input date <= valid_to
  " - Deletion indicator is not set
  IF s_loc[] IS NOT INITIAL.
    SELECT * FROM yrga_cst_loc_map
      INTO TABLE lt_loc_map
      WHERE gail_loc_id IN s_loc
        AND valid_from <= gv_date_from
        AND valid_to   >= gv_date_to
        AND deleted    = abap_false.
  ELSE.
    SELECT * FROM yrga_cst_loc_map
      INTO TABLE lt_loc_map
      WHERE valid_from <= gv_date_from
        AND valid_to   >= gv_date_to
        AND deleted    = abap_false.
  ENDIF.

  IF lt_loc_map IS INITIAL.
    MESSAGE s000(ygms_msg) WITH 'No valid Location-CTP mappings found for date range'.
    RETURN.
  ENDIF.

  " Build Location ID - CTP ID map
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
*& 3.1.2 - Fetch records from YRGA_CST_B2B_1
*&---------------------------------------------------------------------*
FORM fetch_b2b_data.
  DATA: lt_b2b_data TYPE TABLE OF yrga_cst_b2b_1.

  " Get unique CTP IDs
  DATA: lt_ctp_ids TYPE TABLE OF ygms_de_ongc_ctp.
  LOOP AT gt_loc_ctp_map INTO DATA(ls_map).
    COLLECT ls_map-ongc_ctp_id INTO lt_ctp_ids.
  ENDLOOP.

  " Fetch B2B data where qty_scm > 0
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

  " Build gas receipt table
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
    APPEND ls_receipt TO gt_gas_receipt.
  ENDLOOP.

  DATA(lv_count) = lines( gt_gas_receipt ).
  MESSAGE s000(ygms_msg) WITH lv_count 'B2B records fetched'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form MAP_LOCATION_IDS
*& 3.1.3 - Insert GAIL Location IDs against corresponding CTP IDs
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
*& 3.1.4 - Fetch GAIL Material names from YRGA_CST_MAT_MAP
*&---------------------------------------------------------------------*
FORM map_material_names.
  DATA: lt_mat_map TYPE TABLE OF yrga_cst_mat_map.

  DATA: BEGIN OF ls_loc_mat,
          location_id   TYPE ygms_de_loc_id,
          ongc_material TYPE ygms_de_ongc_mat,
        END OF ls_loc_mat,
        lt_loc_mat LIKE TABLE OF ls_loc_mat.

  " Get unique Location ID - ONGC Material combinations
  LOOP AT gt_gas_receipt INTO DATA(ls_receipt).
    ls_loc_mat-location_id   = ls_receipt-location_id.
    ls_loc_mat-ongc_material = ls_receipt-ongc_material.
    COLLECT ls_loc_mat INTO lt_loc_mat.
  ENDLOOP.

  " Fetch material mappings
  IF lt_loc_mat IS NOT INITIAL.
    SELECT * FROM yrga_cst_mat_map
      INTO TABLE lt_mat_map
      FOR ALL ENTRIES IN lt_loc_mat
      WHERE location_id    = lt_loc_mat-location_id
        AND ongc_material  = lt_loc_mat-ongc_material
        AND valid_from    <= gv_date_from
        AND valid_to      >= gv_date_to.
  ENDIF.

  " Map GAIL Material names
  LOOP AT gt_gas_receipt ASSIGNING FIELD-SYMBOL(<fs_receipt>).
    READ TABLE lt_mat_map INTO DATA(ls_mat)
      WITH KEY location_id   = <fs_receipt>-location_id
               ongc_material = <fs_receipt>-ongc_material.
    IF sy-subrc = 0.
      <fs_receipt>-material = ls_mat-gail_material.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_EDITABLE_ALV
*&---------------------------------------------------------------------*
FORM display_editable_alv.
  DATA: lt_fieldcat  TYPE lvc_t_fcat,
        ls_fieldcat  TYPE lvc_s_fcat,
        ls_layout    TYPE lvc_s_layo,
        lt_exclude   TYPE ui_functions.

  " Build field catalog
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GAS_DAY'.
  ls_fieldcat-coltext   = 'Gas Day'.
  ls_fieldcat-outputlen = 10.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'CTP_ID'.
  ls_fieldcat-coltext   = 'CTP ID'.
  ls_fieldcat-outputlen = 10.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'LOCATION_ID'.
  ls_fieldcat-coltext   = 'Location ID'.
  ls_fieldcat-outputlen = 12.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ONGC_MATERIAL'.
  ls_fieldcat-coltext   = 'ONGC Material'.
  ls_fieldcat-outputlen = 15.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'MATERIAL'.
  ls_fieldcat-coltext   = 'Material'.
  ls_fieldcat-outputlen = 15.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'QTY_SCM'.
  ls_fieldcat-coltext   = 'Qty in SCM'.
  ls_fieldcat-outputlen = 12.
  ls_fieldcat-do_sum    = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'GCV'.
  ls_fieldcat-coltext   = 'GCV'.
  ls_fieldcat-outputlen = 10.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'NCV'.
  ls_fieldcat-coltext   = 'NCV'.
  ls_fieldcat-outputlen = 10.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'QTY_MBG'.
  ls_fieldcat-coltext   = 'Qty in MBG'.
  ls_fieldcat-outputlen = 12.
  ls_fieldcat-do_sum    = 'X'.
  APPEND ls_fieldcat TO lt_fieldcat.

  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ONGC_ID'.
  ls_fieldcat-coltext   = 'ONGC ID (9 char max.)'.
  ls_fieldcat-outputlen = 15.
  ls_fieldcat-edit      = 'X'.
  ls_fieldcat-intlen    = 9.
  APPEND ls_fieldcat TO lt_fieldcat.

  " Set layout
  ls_layout-zebra      = 'X'.
  ls_layout-cwidth_opt = 'X'.
  ls_layout-sel_mode   = 'A'.

  " Display using REUSE_ALV_GRID_DISPLAY
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
      is_layout_lvc      = ls_layout
      it_fieldcat_lvc    = lt_fieldcat
      i_save             = 'A'
    TABLES
      t_outtab           = gt_gas_receipt
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE e001(ygms_msg) WITH 'Error displaying ALV'.
  ENDIF.
ENDFORM.
