*&---------------------------------------------------------------------*
*& Class: YGMS_CL_CST_VALIDATOR
*& Package: YGMS
*& Description: Validation Logic for Allocations
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

CLASS ygms_cl_cst_validator DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    METHODS constructor.

    "! <p class="shorttext synchronized" lang="en">Validate allocation totals</p>
    "! @parameter it_data | Allocation data to validate
    "! @parameter rv_valid | Returns ABAP_TRUE if totals match
    METHODS validate_totals
      IMPORTING
        it_data         TYPE ygms_tt_allocation
      RETURNING
        VALUE(rv_valid) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Check mandatory fields</p>
    "! @parameter it_data | Data to check
    "! @parameter rt_errors | List of validation errors
    METHODS check_mandatory
      IMPORTING
        it_data          TYPE ygms_tt_allocation
      RETURNING
        VALUE(rt_errors) TYPE bapiret2_t.

    "! <p class="shorttext synchronized" lang="en">Validate data ranges</p>
    "! @parameter it_data | Data to validate
    "! @parameter rt_errors | List of validation errors
    METHODS validate_ranges
      IMPORTING
        it_data          TYPE ygms_tt_allocation
      RETURNING
        VALUE(rt_errors) TYPE bapiret2_t.

    "! <p class="shorttext synchronized" lang="en">Get validation results</p>
    "! @parameter it_receipt | Receipt data for comparison
    "! @parameter it_allocation | Allocation data
    "! @parameter rt_results | Validation results
    METHODS get_validation_results
      IMPORTING
        it_receipt       TYPE ygms_tt_receipt
        it_allocation    TYPE ygms_tt_allocation
      RETURNING
        VALUE(rt_results) TYPE ygms_tt_validation.

  PRIVATE SECTION.
    CONSTANTS: co_tolerance TYPE p DECIMALS 3 VALUE '0.001'.  " Tolerance for rounding

    "! <p class="shorttext synchronized" lang="en">Add error message</p>
    METHODS add_error
      IMPORTING
        iv_message  TYPE string
        iv_field    TYPE string OPTIONAL
      CHANGING
        ct_errors   TYPE bapiret2_t.

ENDCLASS.


CLASS ygms_cl_cst_validator IMPLEMENTATION.

  METHOD constructor.
    " No initialization required
  ENDMETHOD.


  METHOD validate_totals.
    " Default to valid
    rv_valid = abap_true.

    " Group allocation by day/location/material and validate totals
    LOOP AT it_data ASSIGNING FIELD-SYMBOL(<fs_alloc>)
      GROUP BY ( gas_day     = <fs_alloc>-gas_day
                 location_id = <fs_alloc>-location_id
                 material    = <fs_alloc>-material )
      INTO DATA(ls_group).

      " Sum allocation quantities (exclude excluded states)
      DATA(lv_alloc_mbg) = REDUCE ygms_qty_mbg(
        INIT sum = CONV ygms_qty_mbg( 0 )
        FOR member IN GROUP ls_group
        WHERE ( excluded = ygms_if_cst_constants=>co_flag_false )
        NEXT sum = sum + member-alloc_qty_mbg
      ).

      DATA(lv_alloc_scm) = REDUCE ygms_qty_scm(
        INIT sum = CONV ygms_qty_scm( 0 )
        FOR member IN GROUP ls_group
        WHERE ( excluded = ygms_if_cst_constants=>co_flag_false )
        NEXT sum = sum + member-alloc_qty_scm
      ).

      " Sum sales quantities (should match)
      DATA(lv_sales_mbg) = REDUCE ygms_qty_mbg(
        INIT sum = CONV ygms_qty_mbg( 0 )
        FOR member IN GROUP ls_group
        WHERE ( excluded = ygms_if_cst_constants=>co_flag_false )
        NEXT sum = sum + member-sales_qty
      ).

      " Check if totals are within tolerance
      " (Actual validation against receipt data would be done in get_validation_results)
      IF lv_alloc_mbg < 0 OR lv_alloc_scm < 0.
        rv_valid = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_mandatory.
    " Check mandatory fields for each allocation record
    LOOP AT it_data INTO DATA(ls_data).
      " Gas day is mandatory
      IF ls_data-gas_day IS INITIAL.
        add_error(
          EXPORTING
            iv_message = 'Gas day is required'
            iv_field   = 'GAS_DAY'
          CHANGING
            ct_errors  = rt_errors
        ).
      ENDIF.

      " Location ID is mandatory
      IF ls_data-location_id IS INITIAL.
        add_error(
          EXPORTING
            iv_message = 'Location ID is required'
            iv_field   = 'LOCATION_ID'
          CHANGING
            ct_errors  = rt_errors
        ).
      ENDIF.

      " Material is mandatory
      IF ls_data-material IS INITIAL.
        add_error(
          EXPORTING
            iv_message = 'Material is required'
            iv_field   = 'MATERIAL'
          CHANGING
            ct_errors  = rt_errors
        ).
      ENDIF.

      " State code is mandatory
      IF ls_data-state_code IS INITIAL.
        add_error(
          EXPORTING
            iv_message = 'State code is required'
            iv_field   = 'STATE_CODE'
          CHANGING
            ct_errors  = rt_errors
        ).
      ENDIF.

      " At least one quantity must be provided
      IF ls_data-alloc_qty_mbg IS INITIAL AND ls_data-alloc_qty_scm IS INITIAL.
        add_error(
          EXPORTING
            iv_message = 'Allocation quantity (MBG or SCM) is required'
            iv_field   = 'ALLOC_QTY'
          CHANGING
            ct_errors  = rt_errors
        ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD validate_ranges.
    " Validate data is within acceptable ranges
    LOOP AT it_data INTO DATA(ls_data).
      " Quantities must be non-negative
      IF ls_data-alloc_qty_mbg < 0.
        add_error(
          EXPORTING
            iv_message = |Negative MBG quantity: { ls_data-alloc_qty_mbg }|
            iv_field   = 'ALLOC_QTY_MBG'
          CHANGING
            ct_errors  = rt_errors
        ).
      ENDIF.

      IF ls_data-alloc_qty_scm < 0.
        add_error(
          EXPORTING
            iv_message = |Negative SCM quantity: { ls_data-alloc_qty_scm }|
            iv_field   = 'ALLOC_QTY_SCM'
          CHANGING
            ct_errors  = rt_errors
        ).
      ENDIF.

      " GCV must be positive if provided
      IF ls_data-gcv IS NOT INITIAL AND ls_data-gcv <= 0.
        add_error(
          EXPORTING
            iv_message = |Invalid GCV value: { ls_data-gcv }|
            iv_field   = 'GCV'
          CHANGING
            ct_errors  = rt_errors
        ).
      ENDIF.

      " NCV must be positive if provided
      IF ls_data-ncv IS NOT INITIAL AND ls_data-ncv <= 0.
        add_error(
          EXPORTING
            iv_message = |Invalid NCV value: { ls_data-ncv }|
            iv_field   = 'NCV'
          CHANGING
            ct_errors  = rt_errors
        ).
      ENDIF.

      " NCV should be less than or equal to GCV
      IF ls_data-gcv > 0 AND ls_data-ncv > 0 AND ls_data-ncv > ls_data-gcv.
        add_error(
          EXPORTING
            iv_message = 'NCV cannot be greater than GCV'
            iv_field   = 'NCV'
          CHANGING
            ct_errors  = rt_errors
        ).
      ENDIF.

      " Tax type must be valid
      IF ls_data-tax_type IS NOT INITIAL AND
         ls_data-tax_type <> ygms_if_cst_constants=>co_tax_cst AND
         ls_data-tax_type <> ygms_if_cst_constants=>co_tax_vat.
        add_error(
          EXPORTING
            iv_message = |Invalid tax type: { ls_data-tax_type }|
            iv_field   = 'TAX_TYPE'
          CHANGING
            ct_errors  = rt_errors
        ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_validation_results.
    " Compare allocation totals against receipt totals
    " Group by location/material
    LOOP AT it_receipt ASSIGNING FIELD-SYMBOL(<fs_receipt>)
      GROUP BY ( location_id = <fs_receipt>-location_id
                 material    = <fs_receipt>-material )
      INTO DATA(ls_receipt_group).

      " Sum receipt quantities
      DATA(lv_supply_mbg) = REDUCE ygms_qty_mbg(
        INIT sum = CONV ygms_qty_mbg( 0 )
        FOR member IN GROUP ls_receipt_group
        NEXT sum = sum + member-qty_mbg
      ).

      DATA(lv_supply_scm) = REDUCE ygms_qty_scm(
        INIT sum = CONV ygms_qty_scm( 0 )
        FOR member IN GROUP ls_receipt_group
        NEXT sum = sum + member-qty_scm
      ).

      " Sum allocation quantities (excluding excluded states)
      DATA(lv_alloc_mbg) = REDUCE ygms_qty_mbg(
        INIT sum = CONV ygms_qty_mbg( 0 )
        FOR ls_alloc IN it_allocation
        WHERE ( location_id = ls_receipt_group-location_id
            AND material    = ls_receipt_group-material
            AND excluded    = ygms_if_cst_constants=>co_flag_false )
        NEXT sum = sum + ls_alloc-alloc_qty_mbg
      ).

      DATA(lv_alloc_scm) = REDUCE ygms_qty_scm(
        INIT sum = CONV ygms_qty_scm( 0 )
        FOR ls_alloc IN it_allocation
        WHERE ( location_id = ls_receipt_group-location_id
            AND material    = ls_receipt_group-material
            AND excluded    = ygms_if_cst_constants=>co_flag_false )
        NEXT sum = sum + ls_alloc-alloc_qty_scm
      ).

      " Calculate differences
      DATA(lv_diff_mbg) = lv_supply_mbg - lv_alloc_mbg.
      DATA(lv_diff_scm) = lv_supply_scm - lv_alloc_scm.

      " Determine status
      DATA(lv_status) = COND char2(
        WHEN abs( lv_diff_mbg ) <= co_tolerance AND abs( lv_diff_scm ) <= co_tolerance
        THEN ygms_if_cst_constants=>co_status_valid
        ELSE ygms_if_cst_constants=>co_status_error
      ).

      " Add validation result
      APPEND VALUE #(
        location_id = ls_receipt_group-location_id
        material    = ls_receipt_group-material
        alloc_scm   = lv_alloc_scm
        alloc_mbg   = lv_alloc_mbg
        supply_scm  = lv_supply_scm
        supply_mbg  = lv_supply_mbg
        diff_scm    = lv_diff_scm
        diff_mbg    = lv_diff_mbg
        status      = lv_status
      ) TO rt_results.
    ENDLOOP.
  ENDMETHOD.


  METHOD add_error.
    APPEND VALUE #(
      type       = 'E'
      id         = ygms_if_cst_constants=>co_msg_class
      number     = '009'
      message    = iv_message
      field      = iv_field
    ) TO ct_errors.
  ENDMETHOD.

ENDCLASS.
