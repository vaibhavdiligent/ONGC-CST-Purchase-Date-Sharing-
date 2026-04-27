*&---------------------------------------------------------------------*
*& Report  YRGR_CST_INV_VERIFY
*& Description: ONGC CST - Invoice Receipt Verification
*&              Phase 1: View Invoice Receipt
*&---------------------------------------------------------------------*
REPORT yrgr_cst_inv_verify.

TABLES: yrga_cst_b2b_4, yrga_cst_b2b_5, yrga_cst_loc_map.

*----------------------------------------------------------------------*
* Type definitions
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_loc_ctp,
         gail_loc_id TYPE ygms_de_loc_id,
         ongc_ctp_id TYPE ygms_de_ongc_ctp,
       END OF ty_loc_ctp.

TYPES: BEGIN OF ty_item_alv,
         gail_loc_id   TYPE ygms_de_loc_id,
         ctp_id        TYPE ygms_de_ongc_ctp,
         invoice_no    TYPE yrga_de_invoice_no,
         invoice_date  TYPE datum,
         item_no       TYPE posnr,
         time_stamp    TYPE timestamp,
         bill_from     TYPE datum,
         bill_to       TYPE datum,
         ongc_material TYPE ygms_de_ongc_mat,
         state_code    TYPE regio,
         qty_scm       TYPE ygms_de_qty_scm,
         qty_mbg       TYPE ygms_de_qty_mbg,
         avg_gcv       TYPE ygms_de_gcv,
         avg_ncv       TYPE ygms_de_ncv,
         rate          TYPE yrga_de_price,
         rate_curr     TYPE waers,
         rate_uom      TYPE meins,
         gross_amt     TYPE yrga_de_amount,
         tax_type      TYPE yrga_de_name50,
         tax_1         TYPE yrga_de_tax_rate,
         tax_2         TYPE yrga_de_tax_rate,
         tax_3         TYPE yrga_de_tax_rate,
         tax_1_amt     TYPE yrga_de_amount,
         tax_2_amt     TYPE yrga_de_amount,
         tax_3_amt     TYPE yrga_de_amount,
         net_amt       TYPE yrga_de_amount,
         received_on   TYPE datum,
         received_at   TYPE uzeit,
         user_id       TYPE syuname,
         gail_id       TYPE ygms_de_gail_id,
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
         tot_qty_scm  TYPE ygms_de_qty_scm,
         tot_qty_mbg  TYPE ygms_de_qty_mbg,
         gross_amt    TYPE yrga_de_amount,
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
         hsn_code     TYPE yrga_de_hsn_code,
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
              p_verfy RADIOBUTTON GROUP r1,
              p_vstat RADIOBUTTON GROUP r1,
              p_canc  RADIOBUTTON GROUP r1.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Global data
*----------------------------------------------------------------------*
DATA: gt_loc_ctp     TYPE TABLE OF ty_loc_ctp,
      gt_item_alv    TYPE TABLE OF ty_item_alv,
      gt_header_alv  TYPE TABLE OF ty_header_alv,
      go_alv         TYPE REF TO cl_salv_table,
      gv_show_header TYPE abap_bool VALUE abap_false.

CONSTANTS: gc_btn_item   TYPE salv_de_function VALUE 'ITEM_DATA',
           gc_btn_header TYPE salv_de_function VALUE 'HEADER_DATA'.

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
    CASE e_salv_function.
      WHEN gc_btn_item.
        gv_show_header = abap_false.
        PERFORM display_alv.
      WHEN gc_btn_header.
        gv_show_header = abap_true.
        PERFORM display_alv.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

DATA: go_handler TYPE REF TO lcl_handler.

*----------------------------------------------------------------------*
* Selection screen validations
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM validate_selection.

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

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = <fs_date>-low
    IMPORTING
      last_day_of_month = lv_eom_date.
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
  PERFORM display_alv.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form map_locations
*&  Convert GAIL Location ID to ONGC CTP ID via YRGA_CST_LOC_MAP
*&---------------------------------------------------------------------*
FORM map_locations.

  SELECT gail_loc_id, ongc_ctp_id
    FROM yrga_cst_loc_map
    WHERE gail_loc_id IN @s_loc
      AND deleted     = @abap_false
    INTO TABLE @gt_loc_ctp.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form fetch_item_data
*&  Pull from YRGA_CST_B2B_5, keep latest per
*&  (BILL_FROM, BILL_TO, CTP_ID, ONGC_MATERIAL, STATE_CODE)
*&---------------------------------------------------------------------*
FORM fetch_item_data.

  DATA: lt_b2b5    TYPE TABLE OF yrga_cst_b2b_5,
        lt_ctp_ids TYPE TABLE OF ygms_de_ongc_ctp,
        lv_from    TYPE datum,
        lv_to      TYPE datum.

  READ TABLE s_date INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_date>).
  lv_from = <fs_date>-low.
  lv_to   = <fs_date>-high.

  lt_ctp_ids = VALUE #( FOR <ls> IN gt_loc_ctp ( <ls>-ongc_ctp_id ) ).
  SORT lt_ctp_ids.
  DELETE ADJACENT DUPLICATES FROM lt_ctp_ids.

  SELECT *
    FROM yrga_cst_b2b_5
    WHERE bill_from = @lv_from
      AND bill_to   = @lv_to
      AND ctp_id    IN ( SELECT ongc_ctp_id FROM yrga_cst_loc_map
                          WHERE gail_loc_id IN @s_loc
                            AND deleted     = @abap_false )
    INTO TABLE @lt_b2b5.

  IF lt_b2b5 IS INITIAL.
    RETURN.
  ENDIF.

  " Keep only the latest TIME_STAMP per group
  SORT lt_b2b5 BY bill_from bill_to ctp_id ongc_material state_code time_stamp DESCENDING.
  DELETE ADJACENT DUPLICATES FROM lt_b2b5
    COMPARING bill_from bill_to ctp_id ongc_material state_code.

  " Build ALV item table joined with GAIL Location ID
  LOOP AT lt_b2b5 ASSIGNING FIELD-SYMBOL(<fs_item>).
    DATA(ls_alv) = VALUE ty_item_alv(
      ctp_id        = <fs_item>-ctp_id
      invoice_no    = <fs_item>-invoice_no
      invoice_date  = <fs_item>-invoice_date
      item_no       = <fs_item>-item_no
      time_stamp    = <fs_item>-time_stamp
      bill_from     = <fs_item>-bill_from
      bill_to       = <fs_item>-bill_to
      ongc_material = <fs_item>-ongc_material
      state_code    = <fs_item>-state_code
      qty_scm       = <fs_item>-qty_scm
      qty_mbg       = <fs_item>-qty_mbg
      avg_gcv       = <fs_item>-avg_gcv
      avg_ncv       = <fs_item>-avg_ncv
      rate          = <fs_item>-rate
      rate_curr     = <fs_item>-rate_curr
      rate_uom      = <fs_item>-rate_uom
      gross_amt     = <fs_item>-gross_amt
      tax_type      = <fs_item>-tax_type
      tax_1         = <fs_item>-tax_1
      tax_2         = <fs_item>-tax_2
      tax_3         = <fs_item>-tax_3
      tax_1_amt     = <fs_item>-tax_1_amt
      tax_2_amt     = <fs_item>-tax_2_amt
      tax_3_amt     = <fs_item>-tax_3_amt
      net_amt       = <fs_item>-net_amt
      received_on   = <fs_item>-received_on
      received_at   = <fs_item>-received_at
      user_id       = <fs_item>-user_id
      gail_id       = <fs_item>-gail_id ).

    READ TABLE gt_loc_ctp ASSIGNING FIELD-SYMBOL(<fs_map>)
      WITH KEY ongc_ctp_id = <fs_item>-ctp_id.
    IF sy-subrc = 0.
      ls_alv-gail_loc_id = <fs_map>-gail_loc_id.
    ENDIF.

    APPEND ls_alv TO gt_item_alv.
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
*&      Form display_alv
*&  Display the active dataset (item or header) in cl_salv_table
*&  with toggle buttons in the toolbar
*&---------------------------------------------------------------------*
FORM display_alv.

  CLEAR go_alv.

  TRY.
      IF gv_show_header = abap_true.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = go_alv
          CHANGING  t_table      = gt_header_alv ).
      ELSE.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = go_alv
          CHANGING  t_table      = gt_item_alv ).
      ENDIF.

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

      go_alv->get_columns( )->set_optimize( abap_true ).
      go_alv->get_display_settings( )->set_list_header(
        COND #( WHEN gv_show_header = abap_true
                THEN 'ONGC CST Invoice - Header Data'
                ELSE 'ONGC CST Invoice - Item Data' ) ).

      go_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx_msg).
      MESSAGE lx_msg->get_text( ) TYPE 'E'.
  ENDTRY.

ENDFORM.
