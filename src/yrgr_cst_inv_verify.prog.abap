*&---------------------------------------------------------------------*
*& Report  YRGR_CST_INV_VERIFY
*& Description: ONGC CST Invoice Data
*&              Phase 1: View Invoice Data
*&---------------------------------------------------------------------*
REPORT yrgr_cst_inv_verify.

TABLES: yrga_cst_b2b_4, yrga_cst_b2b_5, yrga_cst_loc_map,
        yrga_cst_mat_map, yrga_cst_b2b_3, sscrfields.

*----------------------------------------------------------------------*
* Type definitions
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_loc_ctp,
         gail_loc_id TYPE ygms_de_loc_id,
         ongc_ctp_id TYPE ygms_de_ongc_ctp,
       END OF ty_loc_ctp.

TYPES: BEGIN OF ty_item_alv,
         invoice_no    TYPE yrga_de_invoice_no,
         invoice_date  TYPE datum,
         item_no       TYPE posnr,
         time_stamp    TYPE timestamp,
         received_on   TYPE datum,
         received_at   TYPE uzeit,
         user_id       TYPE syuname,
         bill_from     TYPE datum,
         bill_to       TYPE datum,
         ctp_id        TYPE ygms_de_ongc_ctp,
         ongc_material TYPE ygms_de_ongc_mat,
         gail_id       TYPE ygms_de_gail_id,
         state_code    TYPE regio,
         gail_location TYPE ygms_de_loc_id,
         gail_material TYPE ygms_de_gail_mat,
         qty_cuom      TYPE ygms_de_qty_scm,
         cuom          TYPE meins,
         qty_ruom      TYPE ygms_de_qty_mbg,
         qty_sent_scm  TYPE ygms_de_qty_scm,
         qty_sent_mbg  TYPE ygms_de_qty_mbg,
         diff_scm      TYPE ygms_de_qty_scm,
         diff_mbg      TYPE ygms_de_qty_mbg,
         avg_gcv       TYPE ygms_de_gcv,
         avg_ncv       TYPE ygms_de_ncv,
         rate          TYPE yrga_de_price,
         rate_curr     TYPE waers,
         rate_per      TYPE p LENGTH 6 DECIMALS 3,
         rate_uom      TYPE meins,
         basic_amt     TYPE yrga_de_amount,
         tax_type      TYPE yrga_de_name50,
         tax_rate      TYPE yrga_de_tax_rate,
         tax_amt       TYPE yrga_de_amount,
         inv_amt       TYPE yrga_de_amount,
         hsn_code      TYPE yrga_de_hsn_code,
       END OF ty_item_alv.

TYPES: BEGIN OF ty_header_alv,
         invoice_no   TYPE yrga_de_invoice_no,
         invoice_date TYPE datum,
         time_stamp   TYPE timestamp,
         received_on  TYPE datum,
         received_at  TYPE uzeit,
         user_id      TYPE syuname,
         inv_ref_no   TYPE yrga_de_invoice_no,
         inv_ref_date TYPE datum,
         gross_price  TYPE yrga_de_price,
         prem_disc    TYPE yrga_de_price,
         currency     TYPE waers,
         price_uom    TYPE meins,
         exch_rate    TYPE yrga_de_exch_rate,
         tot_qty_cuom TYPE ygms_de_qty_scm,
         cuom         TYPE meins,
         tot_qty_ruom TYPE ygms_de_qty_mbg,
         ruom         TYPE meins,
         basic_amt    TYPE yrga_de_amount,
         total_taxes  TYPE yrga_de_amount,
         round_off    TYPE yrga_de_round_off,
         payable_amt  TYPE yrga_de_amount,
         bill_party   TYPE yrga_de_name50,
         bill_street  TYPE yrga_de_name50,
         bill_city    TYPE yrga_de_name50,
         bill_pin     TYPE yrga_de_pincode,
         bill_regio   TYPE regio,
         bill_state   TYPE bezei30,
         bill_land1   TYPE land1,
         bill_pan     TYPE yrga_de_pan,
         bill_vat_tin TYPE yrga_de_tin,
         bill_cst_tin TYPE yrga_de_tin,
         bill_gst     TYPE yrga_de_gstin,
         due_date     TYPE datum,
         pay_terms    TYPE yrga_de_name50,
         supplier     TYPE yrga_de_name50,
         supp_addr    TYPE yrga_de_name50,
         supp_regio   TYPE regio,
         supp_state   TYPE bezei30,
         supp_land1   TYPE land1,
         supp_pan     TYPE yrga_de_pan,
         supp_vat_tin TYPE yrga_de_tin,
         supp_cst_tin TYPE yrga_de_tin,
         supp_gst     TYPE yrga_de_gstin,
         supp_through TYPE yrga_de_invoice_no,
       END OF ty_header_alv.

TYPES: BEGIN OF ty_inv_key,
         invoice_no   TYPE yrga_de_invoice_no,
         invoice_date TYPE datum,
         time_stamp   TYPE timestamp,
       END OF ty_inv_key.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
DATA: gv_loc_id TYPE ygms_de_loc_id.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_date FOR sy-datum OBLIGATORY NO-EXTENSION,
                  s_loc  FOR gv_loc_id OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_view  RADIOBUTTON GROUP r1 DEFAULT 'X',
              p_verfy RADIOBUTTON GROUP r1 MODIF ID hid,
              p_vstat RADIOBUTTON GROUP r1 MODIF ID hid,
              p_canc  RADIOBUTTON GROUP r1 MODIF ID hid.
SELECTION-SCREEN END OF BLOCK b2.

" Output screen hosting the ALV in a container so toolbar buttons work
" Title is a variable (filled before CALL SELECTION-SCREEN) so the screen
" never shows the raw name 'TEXT-003' when the text symbol is not maintained
SELECTION-SCREEN BEGIN OF SCREEN 1001 TITLE gv_t1001.
  SELECTION-SCREEN COMMENT /1(1) gv_dummy.
SELECTION-SCREEN END OF SCREEN 1001.

*----------------------------------------------------------------------*
* Global data
*----------------------------------------------------------------------*
DATA: gt_loc_ctp     TYPE TABLE OF ty_loc_ctp,
      gt_item_alv    TYPE TABLE OF ty_item_alv,
      gt_header_alv  TYPE TABLE OF ty_header_alv,
      go_alv         TYPE REF TO cl_salv_table,
      go_dock        TYPE REF TO cl_gui_docking_container,
      gv_show_header TYPE abap_bool VALUE abap_false.

CONSTANTS: gc_btn_item   TYPE salv_de_function VALUE 'ITEM_DATA',
           gc_btn_header TYPE salv_de_function VALUE 'HEADER_DATA',
           gc_ucomm_swt  TYPE sy-ucomm         VALUE 'ZSWITCH'.

*----------------------------------------------------------------------*
* Event handler class for ALV toolbar
*----------------------------------------------------------------------*
CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS on_user_command FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.
ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD on_user_command.
    " SET_DATA is not allowed inside a SALV event handler ("Data cannot be
    " set again in event handlers"). Only remember the requested view here
    " and raise a PAI with function ZSWITCH; the data is switched in
    " AT SELECTION-SCREEN, after the handler has finished.
    CASE e_salv_function.
      WHEN gc_btn_item.
        IF gv_show_header = abap_true.
          gv_show_header = abap_false.
          cl_gui_cfw=>set_new_ok_code( new_code = gc_ucomm_swt ).
        ENDIF.
      WHEN gc_btn_header.
        IF gv_show_header = abap_false.
          gv_show_header = abap_true.
          cl_gui_cfw=>set_new_ok_code( new_code = gc_ucomm_swt ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

DATA: go_handler TYPE REF TO lcl_handler.

*----------------------------------------------------------------------*
* Selection screen output
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  CASE sy-dynnr.
    WHEN '1000'.
      LOOP AT SCREEN.
        IF screen-group1 = 'HID'.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    WHEN '1001'.
      PERFORM hide_1001_toolbar.
      IF go_dock IS INITIAL.
        PERFORM display_alv.
      ENDIF.
  ENDCASE.

*----------------------------------------------------------------------*
* Selection screen validations
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sy-dynnr.
    WHEN '1000'.
      PERFORM validate_selection.
    WHEN '1001'.
      " Item Data / Header Data toggle requested by the ALV toolbar handler
      IF sscrfields-ucomm = gc_ucomm_swt OR sy-ucomm = gc_ucomm_swt.
        CLEAR sscrfields-ucomm.
        PERFORM switch_alv_data.
      ENDIF.
  ENDCASE.

*----------------------------------------------------------------------*
* Main processing
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF p_view = abap_true.
    PERFORM process_view_invoice.
  ELSE.
    MESSAGE 'This option is not yet implemented in current phase' TYPE 'I'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form validate_selection
*&---------------------------------------------------------------------*
*  Validate fortnight date range and reject wildcards in location IDs
*&---------------------------------------------------------------------*
FORM validate_selection.

  DATA: lv_from_day  TYPE i,
        lv_to_day    TYPE i,
        lv_from_mon  TYPE n LENGTH 2,
        lv_from_year TYPE n LENGTH 4,
        lv_to_mon    TYPE n LENGTH 2,
        lv_to_year   TYPE n LENGTH 4,
        lv_last_day  TYPE i,
        lv_eom_date  TYPE datum.

  READ TABLE s_date INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_date>).
  IF sy-subrc <> 0.
    MESSAGE 'Date range is mandatory' TYPE 'E'.
  ENDIF.

  IF <fs_date>-low IS INITIAL OR <fs_date>-high IS INITIAL.
    MESSAGE 'Both From and To dates are mandatory' TYPE 'E'.
  ENDIF.

  IF <fs_date>-option <> 'BT' OR <fs_date>-sign <> 'I'.
    MESSAGE 'Date must be entered as a range (Between)' TYPE 'E'.
  ENDIF.

  lv_from_day  = <fs_date>-low+6(2).
  lv_from_mon  = <fs_date>-low+4(2).
  lv_from_year = <fs_date>-low(4).
  lv_to_day    = <fs_date>-high+6(2).
  lv_to_mon    = <fs_date>-high+4(2).
  lv_to_year   = <fs_date>-high(4).

  IF lv_from_year <> lv_to_year OR lv_from_mon <> lv_to_mon.
    MESSAGE 'From and To dates must be in the same fortnight (same month)' TYPE 'E'.
  ENDIF.

  " FM LAST_DAY_OF_MONTHS is removed in S/4HANA (ATC, SAP Note 2296016);
  " RP_LAST_DAY_OF_MONTHS returns the month-end (28/29/30/31) instead.
  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = <fs_date>-low
    IMPORTING
      last_day_of_month = lv_eom_date
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Invalid From date' TYPE 'E'.
  ENDIF.
  lv_last_day = lv_eom_date+6(2).

  IF lv_from_day = 1 AND lv_to_day = 15.
    " Valid fortnight 1
  ELSEIF lv_from_day = 16 AND lv_to_day = lv_last_day.
    " Valid fortnight 2
  ELSE.
    MESSAGE 'Date range must align to a fortnight (1-15 or 16-EOM)' TYPE 'E'.
  ENDIF.

  LOOP AT s_loc ASSIGNING FIELD-SYMBOL(<fs_loc>).
    IF <fs_loc>-low CA '*+' OR <fs_loc>-high CA '*+'.
      MESSAGE 'Wildcard entries are not allowed in Location ID' TYPE 'E'.
    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form process_view_invoice
*&---------------------------------------------------------------------*
FORM process_view_invoice.

  PERFORM map_locations.
  IF gt_loc_ctp IS INITIAL.
    MESSAGE 'No CTP mapping found for the entered Location IDs' TYPE 'I'.
    RETURN.
  ENDIF.

  PERFORM fetch_item_data.
  IF gt_item_alv IS INITIAL.
    MESSAGE 'No invoice received for the input parameters' TYPE 'I'.
    RETURN.
  ENDIF.

  PERFORM fetch_header_data.
  PERFORM enrich_item_data.

  gv_show_header = abap_false.
  gv_t1001 = 'ONGC CST Invoice Data'(003).
  CALL SELECTION-SCREEN 1001.

  IF go_dock IS BOUND.
    go_dock->free( ).
  ENDIF.
  CLEAR: go_dock, go_alv.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form map_locations
*&  Convert GAIL Location ID to ONGC CTP ID via YRGA_CST_LOC_MAP
*&---------------------------------------------------------------------*
FORM map_locations.

  READ TABLE s_date INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_date>).

  SELECT gail_loc_id, ongc_ctp_id
    FROM yrga_cst_loc_map
    WHERE gail_loc_id IN @s_loc
      AND valid_from  <= @<fs_date>-low
      AND valid_to    >= @<fs_date>-high
      AND deleted     = @abap_false
    INTO TABLE @gt_loc_ctp.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form fetch_item_data
*&  Pull from YRGA_CST_B2B_5, keep latest per
*&  (ITEM_NO, BILL_FROM, BILL_TO, CTP_ID, ONGC_MATERIAL, STATE_CODE)
*&---------------------------------------------------------------------*
FORM fetch_item_data.

  DATA: lt_b2b5   TYPE TABLE OF yrga_cst_b2b_5,
        lr_ctp_id TYPE RANGE OF ygms_de_ongc_ctp,
        lv_from   TYPE datum,
        lv_to     TYPE datum.

  READ TABLE s_date INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_date>).
  lv_from = <fs_date>-low.
  lv_to   = <fs_date>-high.

  lr_ctp_id = VALUE #( FOR <ls> IN gt_loc_ctp
                       ( sign = 'I' option = 'EQ' low = <ls>-ongc_ctp_id ) ).
  SORT lr_ctp_id BY low.
  DELETE ADJACENT DUPLICATES FROM lr_ctp_id COMPARING low.

  SELECT *
    FROM yrga_cst_b2b_5
    WHERE bill_from = @lv_from
      AND bill_to   = @lv_to
      AND ctp_id    IN @lr_ctp_id
    INTO TABLE @lt_b2b5.

  IF lt_b2b5 IS INITIAL.
    RETURN.
  ENDIF.

  " Keep only the latest TIME_STAMP per group
  SORT lt_b2b5 BY item_no bill_from bill_to ctp_id ongc_material state_code
                  time_stamp DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_b2b5
    COMPARING item_no bill_from bill_to ctp_id ongc_material state_code.

  LOOP AT lt_b2b5 ASSIGNING FIELD-SYMBOL(<fs_item>).
    APPEND VALUE ty_item_alv(
      invoice_no    = <fs_item>-invoice_no
      invoice_date  = <fs_item>-invoice_date
      item_no       = <fs_item>-item_no
      time_stamp    = <fs_item>-time_stamp
      received_on   = <fs_item>-received_on
      received_at   = <fs_item>-received_at
      user_id       = <fs_item>-user_id
      bill_from     = <fs_item>-bill_from
      bill_to       = <fs_item>-bill_to
      ctp_id        = <fs_item>-ctp_id
      ongc_material = <fs_item>-ongc_material
      gail_id       = <fs_item>-gail_id
      state_code    = <fs_item>-state_code
      qty_cuom      = <fs_item>-qty_cuom
      cuom          = <fs_item>-cuom
      qty_ruom      = <fs_item>-qty_ruom
      avg_gcv       = <fs_item>-avg_gcv
      avg_ncv       = <fs_item>-avg_ncv
      rate          = <fs_item>-rate
      rate_curr     = <fs_item>-rate_curr
      rate_per      = <fs_item>-rate_per
      rate_uom      = <fs_item>-rate_uom
      basic_amt     = <fs_item>-basic_amt
      tax_type      = <fs_item>-tax_type
      tax_rate      = <fs_item>-tax_rate
      tax_amt       = <fs_item>-tax_amt
      inv_amt       = <fs_item>-inv_amt
      hsn_code      = <fs_item>-hsn_code ) TO gt_item_alv.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form fetch_header_data
*&  Pull header rows from YRGA_CST_B2B_4 keyed by INVOICE_NO+DATE+TIMESTAMP
*&---------------------------------------------------------------------*
FORM fetch_header_data.

  DATA: lt_keys TYPE TABLE OF ty_inv_key,
        lt_b2b4 TYPE TABLE OF yrga_cst_b2b_4.

  lt_keys = VALUE #( FOR <ls> IN gt_item_alv
                     ( invoice_no   = <ls>-invoice_no
                       invoice_date = <ls>-invoice_date
                       time_stamp   = <ls>-time_stamp ) ).
  SORT lt_keys.
  DELETE ADJACENT DUPLICATES FROM lt_keys
    COMPARING invoice_no invoice_date time_stamp.

  IF lt_keys IS INITIAL.
    RETURN.
  ENDIF.

  SELECT *
    FROM yrga_cst_b2b_4
    FOR ALL ENTRIES IN @lt_keys
    WHERE invoice_no   = @lt_keys-invoice_no
      AND invoice_date = @lt_keys-invoice_date
      AND time_stamp   = @lt_keys-time_stamp
    INTO TABLE @lt_b2b4.

  gt_header_alv = CORRESPONDING #( lt_b2b4 ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form enrich_item_data
*&  Enrich ALV items with GAIL Location, GAIL Material, Qty sent to
*&  ONGC and differences
*&---------------------------------------------------------------------*
FORM enrich_item_data.

  DATA: lv_gail_loc TYPE ygms_de_loc_id,
        lv_gail_mat TYPE ygms_de_gail_mat,
        lv_sent_scm TYPE ygms_de_qty_scm,
        lv_sent_mbg TYPE ygms_de_qty_mbg.

  LOOP AT gt_item_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

    CLEAR lv_gail_loc.
    SELECT SINGLE gail_loc_id
      FROM yrga_cst_loc_map
      WHERE ongc_ctp_id = @<fs_alv>-ctp_id
        AND valid_from  <= @<fs_alv>-bill_from
        AND valid_to    >= @<fs_alv>-bill_to
        AND deleted     = @abap_false
      INTO @lv_gail_loc.
    <fs_alv>-gail_location = lv_gail_loc.

    CLEAR lv_gail_mat.
    SELECT SINGLE gail_material
      FROM yrga_cst_mat_map
      WHERE location_id   = @lv_gail_loc
        AND ongc_material = @<fs_alv>-ongc_material
        AND valid_from    <= @<fs_alv>-bill_from
        AND valid_to      >= @<fs_alv>-bill_to
        AND deleted       = @abap_false
      INTO @lv_gail_mat.
    <fs_alv>-gail_material = lv_gail_mat.

    CLEAR: lv_sent_scm, lv_sent_mbg.
    SELECT SINGLE qty_in_scm, qty_in_mbg
      FROM yrga_cst_b2b_3
      WHERE gail_id = @<fs_alv>-gail_id
      INTO (@lv_sent_scm, @lv_sent_mbg).
    <fs_alv>-qty_sent_scm = lv_sent_scm.
    <fs_alv>-qty_sent_mbg = lv_sent_mbg.

    <fs_alv>-diff_scm = lv_sent_scm - <fs_alv>-qty_cuom.
    <fs_alv>-diff_mbg = lv_sent_mbg - <fs_alv>-qty_ruom.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form hide_1001_toolbar
*&  Remove Execute/Variant functions from the output screen
*&---------------------------------------------------------------------*
FORM hide_1001_toolbar.

  DATA lt_excl TYPE TABLE OF sy-ucomm.

  lt_excl = VALUE #( ( 'ONLI' ) ( 'PRIN' ) ( 'SPOS' ) ( 'GET' ) ( 'VDEL' ) ).
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = '%_00'
    TABLES
      p_exclude = lt_excl.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form display_alv
*&  Display item data in cl_salv_table inside a docking container on
*&  screen 1001 (container mode supports custom toolbar buttons)
*&---------------------------------------------------------------------*
FORM display_alv.

  CREATE OBJECT go_dock
    EXPORTING
      repid     = sy-repid
      dynnr     = '1001'
      side      = cl_gui_docking_container=>dock_at_top
      ratio     = 95
    EXCEPTIONS
      OTHERS    = 1.
  IF sy-subrc <> 0.
    MESSAGE 'Unable to create ALV container' TYPE 'E'.
  ENDIF.

  TRY.
      cl_salv_table=>factory(
        EXPORTING r_container  = go_dock
        IMPORTING r_salv_table = go_alv
        CHANGING  t_table      = gt_item_alv ).

      go_alv->get_functions( )->set_all( abap_true ).

      go_alv->get_functions( )->add_function(
        name     = gc_btn_item
        icon     = '@04@'
        text     = 'Item Data'
        tooltip  = 'Show invoice item details'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      go_alv->get_functions( )->add_function(
        name     = gc_btn_header
        icon     = '@05@'
        text     = 'Header Data'
        tooltip  = 'Show invoice header details'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      CREATE OBJECT go_handler.
      SET HANDLER go_handler->on_user_command FOR go_alv->get_event( ).

      PERFORM set_item_columns.
      go_alv->display( ).

    CATCH cx_salv_msg cx_salv_existing cx_salv_wrong_call INTO DATA(lx_salv).
      MESSAGE lx_salv->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form switch_alv_data
*&  Toggle the ALV between item and header data
*&---------------------------------------------------------------------*
FORM switch_alv_data.

  TRY.
      IF gv_show_header = abap_true.
        go_alv->set_data( CHANGING t_table = gt_header_alv ).
        PERFORM set_header_columns.
      ELSE.
        go_alv->set_data( CHANGING t_table = gt_item_alv ).
        PERFORM set_item_columns.
      ENDIF.
      go_alv->refresh( refresh_mode = if_salv_c_refresh=>full ).

    CATCH cx_salv_no_new_data_allowed INTO DATA(lx_salv).
      MESSAGE lx_salv->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form set_item_columns
*&  Column labels for the item view
*&---------------------------------------------------------------------*
FORM set_item_columns.

  go_alv->get_columns( )->set_optimize( abap_true ).

  PERFORM set_col USING 'INVOICE_DATE'  'Inv. Date'  'Invoice Date'      'Invoice Date'.
  PERFORM set_col USING 'RECEIVED_ON'   'Recvd On'   'Received On'       'Received On'.
  PERFORM set_col USING 'RECEIVED_AT'   'Recvd At'   'Received At'       'Received At'.
  PERFORM set_col USING 'USER_ID'       'User'       'User'              'User'.
  PERFORM set_col USING 'BILL_FROM'     'Bill From'  'Bill From'         'Bill From'.
  PERFORM set_col USING 'BILL_TO'       'Bill To'    'Bill To'           'Bill To'.
  PERFORM set_col USING 'GAIL_ID'       'GAIL ID'    'GAIL ID'           'GAIL ID'.
  PERFORM set_col USING 'GAIL_LOCATION' 'GAIL Loc'   'GAIL Location'     'GAIL Location'.
  PERFORM set_col USING 'GAIL_MATERIAL' 'GAIL Mat'   'GAIL Material'     'GAIL Material'.
  PERFORM set_col USING 'QTY_CUOM'      'Qty CUOM'   'Qty in CUOM'       'Quantity in Contractual UOM'.
  PERFORM set_col USING 'CUOM'          'CUOM'       'CUOM'              'Contractual UOM'.
  PERFORM set_col USING 'QTY_RUOM'      'Qty RUOM'   'Qty in RUOM'       'Quantity in Rate UOM'.
  PERFORM set_col USING 'QTY_SENT_SCM'  'Sent SCM'   'Qty Sent(SCM)'     'Qty sent to ONGC (SCM)'.
  PERFORM set_col USING 'QTY_SENT_MBG'  'Sent MBG'   'Qty Sent(MBG)'     'Qty sent to ONGC (MBG)'.
  PERFORM set_col USING 'DIFF_SCM'      'Diff SCM'   'Diff (SCM)'        'Diff (SCM)'.
  PERFORM set_col USING 'DIFF_MBG'      'Diff MBG'   'Diff (MBG)'        'Diff (MBG)'.
  PERFORM set_col USING 'RATE'          'Rate'       'Rate'              'Rate'.
  PERFORM set_col USING 'RATE_CURR'     'Currency'   'Currency'          'Currency'.
  PERFORM set_col USING 'RATE_PER'      'Rate Per'   'Rate Per'          'Rate Per'.
  PERFORM set_col USING 'RATE_UOM'      'Rate UOM'   'Rate UOM'          'Rate UOM'.
  PERFORM set_col USING 'BASIC_AMT'     'Basic Amt'  'Basic Amount'      'Basic Amount'.
  PERFORM set_col USING 'TAX_TYPE'      'Tax Type'   'Tax Type'          'Tax Type'.
  PERFORM set_col USING 'TAX_RATE'      'Tax Rate'   'Tax Rate'          'Tax Rate'.
  PERFORM set_col USING 'TAX_AMT'       'Tax Amount' 'Tax Amount'        'Tax Amount'.
  PERFORM set_col USING 'INV_AMT'       'Inv Amount' 'Invoice Amount'    'Invoice Amount'.
  PERFORM set_col USING 'HSN_CODE'      'HSN Code'   'HSN Code'          'HSN Code'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form set_header_columns
*&  Column labels for the header view (customer list, 44 columns)
*&---------------------------------------------------------------------*
FORM set_header_columns.

  go_alv->get_columns( )->set_optimize( abap_true ).

  PERFORM set_col USING 'INVOICE_NO'    'Invoice No'  'Invoice Number'     'Invoice Number'.
  PERFORM set_col USING 'INVOICE_DATE'  'Inv. Date'   'Invoice Date'       'Invoice Date'.
  PERFORM set_col USING 'TIME_STAMP'    'Time Stamp'  'Time Stamp'         'Time Stamp'.
  PERFORM set_col USING 'RECEIVED_ON'   'Recvd On'    'Received On'        'Received On'.
  PERFORM set_col USING 'RECEIVED_AT'   'Recvd At'    'Received At'        'Received At'.
  PERFORM set_col USING 'USER_ID'       'User Name'   'User Name'          'User Name'.
  PERFORM set_col USING 'INV_REF_NO'    'Inv.Ref.No'  'Inv. Ref. No.'      'Inv. Ref. No.'.
  PERFORM set_col USING 'INV_REF_DATE'  'Ref. Date'   'Inv. Ref. Date'     'Inv. Ref. Date'.
  PERFORM set_col USING 'GROSS_PRICE'   'Gross Prc'   'Gross Price'        'Gross Price'.
  PERFORM set_col USING 'PREM_DISC'     'Prem/Disc'   'Premium/Discount'   'Premium/Discount'.
  PERFORM set_col USING 'CURRENCY'      'Currency'    'Currency'           'Currency'.
  PERFORM set_col USING 'PRICE_UOM'     'Price UOM'   'Price UOM'          'Price UOM'.
  PERFORM set_col USING 'EXCH_RATE'     'Exch. Rate'  'Exchange Rate'      'Exchange Rate'.
  PERFORM set_col USING 'TOT_QTY_CUOM'  'Qty CUOM'    'Total Qty in CUOM'  'Total Qty in CUOM'.
  PERFORM set_col USING 'CUOM'          'CUOM'        'Contractual UOM'    'Contractual UOM'.
  PERFORM set_col USING 'TOT_QTY_RUOM'  'Qty RUOM'    'Total Qty in RUOM'  'Total Qty in RUOM'.
  PERFORM set_col USING 'RUOM'          'Rate UOM'    'Rate UOM'           'Rate UOM'.
  PERFORM set_col USING 'BASIC_AMT'     'Basic Amt'   'Basic Amount'       'Basic Amount'.
  PERFORM set_col USING 'TOTAL_TAXES'   'Tot. Taxes'  'Total Taxes'        'Total Taxes'.
  PERFORM set_col USING 'ROUND_OFF'     'Round Off'   'Round Off Amount'   'Round Off Amount'.
  PERFORM set_col USING 'PAYABLE_AMT'   'Payable'     'Payable Amount'     'Payable Amount'.
  PERFORM set_col USING 'BILL_PARTY'    'Bill To'     'Bill To'            'Bill To'.
  PERFORM set_col USING 'BILL_STREET'   'Bill Addr'   'Bill To Address'    'Bill To Address'.
  PERFORM set_col USING 'BILL_CITY'     'Bill City'   'Bill To City'       'Bill To City'.
  PERFORM set_col USING 'BILL_PIN'      'Bill PIN'    'Bill To PIN'        'Bill To PIN'.
  PERFORM set_col USING 'BILL_REGIO'    'Bill St.Cd'  'Bill To State Code' 'Bill To State Code'.
  PERFORM set_col USING 'BILL_STATE'    'Bill State'  'Bill to State'      'Bill to State'.
  PERFORM set_col USING 'BILL_LAND1'    'Bill Cntry'  'Bill to Country'    'Bill to Country'.
  PERFORM set_col USING 'BILL_PAN'      'Bill PAN'    'Bill To PAN'        'Bill To PAN'.
  PERFORM set_col USING 'BILL_VAT_TIN'  'Bill VAT'    'Bill To VAT TIN'    'Bill To VAT TIN'.
  PERFORM set_col USING 'BILL_CST_TIN'  'Bill CST'    'Bill To CST TIN'    'Bill To CST TIN'.
  PERFORM set_col USING 'BILL_GST'      'Bill GST'    'Bill To GST'        'Bill To GST'.
  PERFORM set_col USING 'DUE_DATE'      'Due Date'    'Due Date'           'Due Date'.
  PERFORM set_col USING 'PAY_TERMS'     'Pay Terms'   'Payment Terms'      'Payment Terms'.
  PERFORM set_col USING 'SUPPLIER'      'Supplier'    'Supplier'           'Supplier'.
  PERFORM set_col USING 'SUPP_ADDR'     'Supp. Addr'  'Supplier Address'   'Supplier Address'.
  PERFORM set_col USING 'SUPP_REGIO'    'Supp.St.Cd'  'Supplier State Code' 'Supplier State Code'.
  PERFORM set_col USING 'SUPP_STATE'    'Supp.State'  'Supplier State'     'Supplier State'.
  PERFORM set_col USING 'SUPP_LAND1'    'Supp.Cntry'  'Supplier Country'   'Supplier Country'.
  PERFORM set_col USING 'SUPP_PAN'      'Supp. PAN'   'Supplier PAN'       'Supplier PAN'.
  PERFORM set_col USING 'SUPP_VAT_TIN'  'Supp. VAT'   'Supplier VAT TIN'   'Supplier VAT TIN'.
  PERFORM set_col USING 'SUPP_CST_TIN'  'Supp. CST'   'Supplier CST TIN'   'Supplier CST TIN'.
  PERFORM set_col USING 'SUPP_GST'      'Supp. GST'   'Supplier GST'       'Supplier GST'.
  PERFORM set_col USING 'SUPP_THROUGH'  'Supp.Thru'   'Supplied Through'   'Supplied Through'.

  " Always show the full description (long text) as the column heading
  LOOP AT go_alv->get_columns( )->get( ) INTO DATA(ls_col).
    ls_col-r_column->set_fixed_header_text( 'L' ).
  ENDLOOP.

ENDFORM.

FORM set_col USING iv_name   TYPE csequence
                   iv_short  TYPE csequence
                   iv_medium TYPE csequence
                   iv_long   TYPE csequence.

  TRY.
      DATA(lo_col) = go_alv->get_columns( )->get_column( CONV #( iv_name ) ).
      lo_col->set_short_text( CONV #( iv_short ) ).
      lo_col->set_medium_text( CONV #( iv_medium ) ).
      lo_col->set_long_text( CONV #( iv_long ) ).
    CATCH cx_salv_not_found.
  ENDTRY.

ENDFORM.
