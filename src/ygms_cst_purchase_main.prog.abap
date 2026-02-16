*&---------------------------------------------------------------------*
*& Report YGMS_CST_PURCHASE_MAIN
*&---------------------------------------------------------------------*
*& Description: ONGC CST Purchase Data Sharing - Main Program
*& Author:      [To be filled]
*& Date:        [Creation date]
*& Module:      FI/SD
*& WRICEF ID:   R-GAIL-CST-001
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*
REPORT ygms_cst_purchase_main MESSAGE-ID ygms_msg.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: go_controller  TYPE REF TO ygms_cl_cst_controller,
      go_alv_handler TYPE REF TO ygms_cl_cst_alv_handler.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
  PARAMETERS:
    rb_upld  RADIOBUTTON GROUP mode DEFAULT 'X' USER-COMMAND mode,
    rb_alloc RADIOBUTTON GROUP mode,
    rb_view  RADIOBUTTON GROUP mode,
    rb_send  RADIOBUTTON GROUP mode,
    rb_dwnld RADIOBUTTON GROUP mode,
    rb_nom   RADIOBUTTON GROUP mode.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
  PARAMETERS:
    p_loc    TYPE ygms_loc_id OBLIGATORY,
    p_frdat  TYPE datum,
    p_todat  TYPE datum.
  SELECT-OPTIONS:
    s_mat    FOR ygms_cst_pur-material.
  PARAMETERS:
    p_file   TYPE string LOWER CASE MODIF ID upl.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
  SELECT-OPTIONS:
    s_exst   FOR ygms_cst_pur-state_code NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* At Selection Screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_loc.
  PERFORM f4_location.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  " Set default dates to current fortnight
  DATA(lv_day) = sy-datum+6(2).
  IF lv_day <= 15.
    p_frdat = sy-datum(6) && '01'.
    p_todat = sy-datum(6) && '15'.
  ELSE.
    p_frdat = sy-datum(6) && '16'.
    " Calculate end of month
    DATA(lv_next_month) = sy-datum.
    lv_next_month+6(2) = '01'.
    lv_next_month = lv_next_month + 32.
    lv_next_month+6(2) = '01'.
    p_todat = lv_next_month - 1.
  ENDIF.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  " Initialize controller
  go_controller = NEW ygms_cl_cst_controller(
    iv_location_id = p_loc
    iv_from_date   = p_frdat
    iv_to_date     = p_todat
  ).

  TRY.
      DATA: lt_messages   TYPE bapiret2_t,
            lt_allocation TYPE ygms_tt_allocation.

      CASE abap_true.
        WHEN rb_upld.
          " Upload mode
          go_controller->process_upload(
            EXPORTING iv_file_path = p_file
            IMPORTING et_messages  = lt_messages
          ).
          PERFORM display_messages USING lt_messages.

        WHEN rb_alloc.
          " Allocation mode
          go_controller->execute_allocation(
            EXPORTING it_excluded_states = s_exst[]
            IMPORTING et_allocation      = lt_allocation
                      et_messages        = lt_messages
          ).

          " Display ALV for editing
          go_alv_handler = NEW ygms_cl_cst_alv_handler( ).
          go_alv_handler->display_allocation( lt_allocation ).

        WHEN rb_view.
          " View mode - Display existing data in ALV

        WHEN rb_send.
          " Send mode - Send data via email

        WHEN rb_dwnld.
          " Download mode - Download data to Excel

        WHEN rb_nom.
          " Nomination mode - Display purchase nominations

      ENDCASE.

      " Only save if there is quantity for this day
      IF lv_day_qty > 0.
        CLEAR ls_cst_pur.

        " Populate the record
        ls_cst_pur-gas_day      = lv_date.
        ls_cst_pur-location     = gs_alv_display-location_id.
        ls_cst_pur-material     = gs_alv_display-material.
        ls_cst_pur-state_code   = gs_alv_display-state_code.
        ls_cst_pur-state        = gs_alv_display-state.

        " Get CTP and ONGC material from gas receipt
        READ TABLE gt_gas_receipt INTO DATA(ls_receipt)
          WITH KEY location_id = gs_alv_display-location_id
                   material    = gs_alv_display-material
                   gas_day     = lv_date.
        IF sy-subrc = 0.
          ls_cst_pur-ctp         = ls_receipt-ctp_id.
          ls_cst_pur-ongc_mater  = ls_receipt-ongc_material.
          ls_cst_pur-ongc_id     = ls_receipt-ongc_id.
        ELSE.
          " Try to get from any record with same location and material
          READ TABLE gt_gas_receipt INTO ls_receipt
            WITH KEY location_id = gs_alv_display-location_id
                     material    = gs_alv_display-material.
          IF sy-subrc = 0.
            ls_cst_pur-ctp         = ls_receipt-ctp_id.
            ls_cst_pur-ongc_mater  = ls_receipt-ongc_material.
            ls_cst_pur-ongc_id     = ls_receipt-ongc_id.
          ENDIF.
        ENDIF.

        ls_cst_pur-time_stamp   = lv_ts_char.
        ls_cst_pur-qty_in_mbg   = lv_day_qty.
        ls_cst_pur-gcv          = gs_alv_display-gcv.
        ls_cst_pur-ncv          = gs_alv_display-ncv.

        " Calculate SCM for this day's quantity
        DATA: c_tgqty TYPE msego2-adqnt,
              i_trqty TYPE msego2-adqnt,
              lv_gcv  TYPE oib_par_fltp,
              lv_ncv  TYPE oib_par_fltp.
        IF gs_alv_display-gcv > 0.
          CLEAR c_tgqty.
          i_trqty = lv_day_qty.
          lv_gcv  = gs_alv_display-gcv.
          lv_ncv  = gs_alv_display-ncv.
          CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
            EXPORTING
              i_trqty = i_trqty
              i_truom = 'MBG'
              i_tguom = 'SM3'
              lv_gcv  = lv_gcv
              lv_ncv  = lv_ncv
            CHANGING
              c_tgqty = c_tgqty.
          ls_cst_pur-qty_in_scm = c_tgqty.
        ENDIF.

        " Assign GAIL_ID from the mapping (same for all days of same Location-Material-State)
        ls_cst_pur-gail_id = ls_gail_id_map-gail_id.

        ls_cst_pur-exclude      = gs_alv_display-exclude.
        ls_cst_pur-created_by   = sy-uname.
        ls_cst_pur-created_date = sy-datum.
        ls_cst_pur-created_time = sy-uzeit.

        " Append to internal table
        APPEND ls_cst_pur TO lt_cst_pur.
      ENDIF.

      " Move to next day
      lv_date = lv_date + 1.
    ENDDO.
  ENDLOOP.

*----------------------------------------------------------------------*
* Forms
*----------------------------------------------------------------------*
FORM f4_location.
  DATA: lt_return TYPE STANDARD TABLE OF ddshretval.

  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
    EXPORTING
      tabname    = 'YGMS_CST_LOC_MAP'
      fieldname  = 'LOCATION_ID'
    TABLES
      return_tab = lt_return.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
  IF sy-subrc = 0.
    p_loc = ls_return-fieldval.
  ENDIF.
ENDFORM.

FORM f4_file.
  DATA: lt_file TYPE filetable,
        lv_rc   TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      file_filter = '*.xlsx;*.xls'
    CHANGING
      file_table  = lt_file
      rc          = lv_rc
  ).

  READ TABLE lt_file INTO DATA(ls_file) INDEX 1.
  IF sy-subrc = 0.
    p_file = ls_file-filename.
  ENDIF.
ENDFORM.

FORM modify_screen.
  LOOP AT SCREEN.
    " Show file path only for upload mode
    IF screen-group1 = 'UPL' AND rb_upld <> abap_true.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM display_messages USING it_messages TYPE bapiret2_t.
  LOOP AT it_messages INTO DATA(ls_message).
    CASE ls_message-type.
      WHEN 'E'.
        WRITE: / icon_led_red AS ICON, ls_message-message.
      WHEN 'W'.
        WRITE: / icon_led_yellow AS ICON, ls_message-message.
      WHEN 'S' OR 'I'.
        WRITE: / icon_led_green AS ICON, ls_message-message.
    ENDCASE.
  ENDLOOP.
ENDFORM.
