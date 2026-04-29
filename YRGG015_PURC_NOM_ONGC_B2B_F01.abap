*&---------------------------------------------------------------------*
*& Include YRGG015_PURC_NOM_ONGC_B2B_F01
*& FN Date Logic, Location Validation, Data Fetch from YRGA_CST_PUR
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& FORM set_default_fn_dates
*& Sets s_date defaults to the previous fortnight period
*&---------------------------------------------------------------------*
FORM set_default_fn_dates.
  DATA: lv_today  TYPE sy-datum,
        lv_day    TYPE i,
        lv_low    TYPE sy-datum,
        lv_high   TYPE sy-datum,
        lv_year   TYPE char4,
        lv_month  TYPE char2,
        ls_date   LIKE LINE OF s_date.

  lv_today = sy-datum.
  lv_day   = lv_today+6(2).   " Day of month

  IF lv_day <= 15.
    " We are in first half → previous FN = 16-EOM of prior month
    lv_high = lv_today.
    lv_high+6(2) = '01'.
    lv_high = lv_high - 1.   " Last day of previous month
    lv_low  = lv_high.
    lv_low+6(2) = '16'.
  ELSE.
    " We are in second half → previous FN = 01-15 of current month
    lv_low  = lv_today.
    lv_low+6(2) = '01'.
    lv_high = lv_today.
    lv_high+6(2) = '15'.
  ENDIF.

  CLEAR ls_date.
  ls_date-sign   = 'I'.
  ls_date-option = 'BT'.
  ls_date-low    = lv_low.
  ls_date-high   = lv_high.
  APPEND ls_date TO s_date.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM control_screen_fields
*& Shows/hides background checkbox based on role authority
*&---------------------------------------------------------------------*
FORM control_screen_fields.
  DATA: lv_auth TYPE char1.

  CLEAR gv_auth_bg.
  AUTHORITY-CHECK OBJECT 'S_TCODE'
    ID 'TCD' FIELD gc_role_core.
  IF sy-subrc = 0.
    gv_auth_bg = abap_true.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-group1 = 'BG'.
      IF gv_auth_bg = abap_true.
        screen-active   = 1.
        screen-invisible = 0.
      ELSE.
        screen-active   = 0.
        screen-invisible = 1.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM validate_selection_screen
*& Validates locations against YRGA_CST_LOC_MAP and FN date boundaries
*&---------------------------------------------------------------------*
FORM validate_selection_screen.
  PERFORM validate_locations.
  PERFORM validate_fn_dates.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM validate_locations
*& Each selected location must exist in YRGA_CST_LOC_MAP
*&---------------------------------------------------------------------*
FORM validate_locations.
  DATA: ls_locid  LIKE LINE OF s_locid,
        lv_exists TYPE char1.

  LOOP AT s_locid INTO ls_locid WHERE sign = 'I' AND option = 'EQ'.
    SELECT SINGLE location_id
      FROM yrga_cst_loc_map
      INTO @DATA(lv_loc)
      WHERE location_id = @ls_locid-low.
    IF sy-subrc <> 0.
      MESSAGE e000(oo) WITH
        'Business Location doesn''t pertain to ONGC CST Purchase:'
        ls_locid-low
        ' ' ' '.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM validate_fn_dates
*& Dates must fall within a valid fortnight (1-15 or 16-EOM)
*&---------------------------------------------------------------------*
FORM validate_fn_dates.
  DATA: ls_date   LIKE LINE OF s_date,
        lv_day_lo TYPE i,
        lv_day_hi TYPE i.

  LOOP AT s_date INTO ls_date WHERE sign = 'I'.
    lv_day_lo = ls_date-low+6(2).

    IF ls_date-option = 'EQ' OR ls_date-option = 'GE' OR ls_date-option = 'LE'.
      " Single date validation
      IF NOT ( lv_day_lo BETWEEN 1 AND 15 OR lv_day_lo BETWEEN 16 AND 31 ).
        MESSAGE e000(oo) WITH 'Invalid date: must be within a fortnight period' ls_date-low ' ' ' '.
      ENDIF.
    ELSEIF ls_date-option = 'BT'.
      lv_day_hi = ls_date-high+6(2).
      " Low must be 1 or 16, High must be 15 or EOM
      IF lv_day_lo <> 1 AND lv_day_lo <> 16.
        MESSAGE e000(oo) WITH 'Date range must start on 1st or 16th of month' ' ' ' ' ' '.
      ENDIF.
      IF lv_day_hi <> 15 AND lv_day_hi < 28.
        " Allow 28-31 as end-of-month approximation
        IF lv_day_hi <> 15.
          MESSAGE e000(oo) WITH 'Date range must end on 15th or last day of month' ' ' ' ' ' '.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM fetch_pur_data
*& Reads YRGA_CST_PUR excluding Gujarat and deleted rows, then enriches
*&---------------------------------------------------------------------*
FORM fetch_pur_data.
  DATA: lt_pur    TYPE STANDARD TABLE OF ty_pur,
        ls_pur    TYPE ty_pur,
        ls_disp   TYPE ty_display.

  " Fetch from YRGA_CST_PUR - exclude GJ state and deleted
  SELECT gas_day, location_id, material, state_code,
         qty_scm, gail_id, deleted
    FROM yrga_cst_pur
    INTO TABLE @lt_pur
    WHERE gas_day     IN @s_date
      AND location_id IN @s_locid
      AND deleted    <> @gc_deleted
      AND state_code <> @gc_excl_state.

  IF sy-subrc <> 0 OR lt_pur IS INITIAL.
    RETURN.
  ENDIF.

  " Skip zero-qty rows
  DELETE lt_pur WHERE qty_scm = 0.

  " Build display table with OA and Batch derivation
  LOOP AT lt_pur INTO ls_pur.
    CLEAR ls_disp.
    ls_disp-sel         = ' '.
    ls_disp-gas_day     = ls_pur-gas_day.
    ls_disp-location_id = ls_pur-location_id.
    ls_disp-material    = ls_pur-material.
    ls_disp-state_code  = ls_pur-state_code.
    ls_disp-qty_scm     = ls_pur-qty_scm.
    ls_disp-gail_id     = ls_pur-gail_id.

    " Derive Outline Agreement
    PERFORM derive_outline_agreement
      USING    ls_pur-location_id ls_pur-material
      CHANGING ls_disp-outline_agr ls_disp-oa_missing.

    " Derive Batch
    PERFORM derive_batch
      USING    ls_pur-material
      CHANGING ls_disp-charg.

    " Set editable cell style for CHARG column
    PERFORM set_cell_style CHANGING ls_disp-celltab.

    " Highlight red if OA missing
    IF ls_disp-oa_missing = abap_true.
      PERFORM set_row_color_red CHANGING ls_disp-t_color.
    ENDIF.

    APPEND ls_disp TO gt_display.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM set_cell_style
*& Makes the CHARG (Batch) column editable in ALV
*&---------------------------------------------------------------------*
FORM set_cell_style CHANGING ct_styl TYPE lvc_t_styl.
  DATA: ls_styl TYPE lvc_s_styl.

  CLEAR ct_styl.
  ls_styl-fieldname = 'CHARG'.
  ls_styl-style     = cl_gui_alv_grid=>mc_style_enabled.
  APPEND ls_styl TO ct_styl.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM set_row_color_red
*& Paints entire row red using cell color table
*&---------------------------------------------------------------------*
FORM set_row_color_red CHANGING ct_color TYPE lvc_t_scol.
  DATA: ls_color TYPE lvc_s_scol.

  CLEAR ct_color.
  ls_color-fname     = 'OUTLINE_AGR'.  " Field triggering highlight
  ls_color-color-col = 6.              " Red
  ls_color-color-int = 1.
  ls_color-color-inv = 0.
  APPEND ls_color TO ct_color.
ENDFORM.
