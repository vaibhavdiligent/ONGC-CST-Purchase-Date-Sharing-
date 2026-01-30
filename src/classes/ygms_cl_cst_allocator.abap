*&---------------------------------------------------------------------*
*& Class: YGMS_CL_CST_ALLOCATOR
*& Package: YGMS
*& Description: Business Logic for State-wise Allocation
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

CLASS ygms_cl_cst_allocator DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    METHODS constructor.

    "! <p class="shorttext synchronized" lang="en">Calculate state-wise allocation</p>
    "! @parameter it_receipt_data | Receipt data from ONGC
    "! @parameter it_sales_data | Sales data for allocation basis
    "! @parameter it_excluded_states | States to exclude from allocation
    "! @parameter rt_allocation | Calculated allocation data
    METHODS calculate_allocation
      IMPORTING
        it_receipt_data    TYPE ygms_tt_receipt
        it_sales_data      TYPE ygms_tt_sales_data
        it_excluded_states TYPE ygms_rt_state_code OPTIONAL
      RETURNING
        VALUE(rt_allocation) TYPE ygms_tt_allocation.

    "! <p class="shorttext synchronized" lang="en">Apply variance adjustment</p>
    "! @parameter it_receipt_data | Receipt data for comparison
    "! @parameter ct_allocation | Allocation data to adjust
    METHODS apply_variance_adjustment
      IMPORTING
        it_receipt_data TYPE ygms_tt_receipt
      CHANGING
        ct_allocation   TYPE ygms_tt_allocation.

    "! <p class="shorttext synchronized" lang="en">Determine tax type for state</p>
    "! @parameter iv_state_code | State code
    "! @parameter rv_tax_type | Tax type (CST or VAT)
    METHODS determine_tax_type
      IMPORTING
        iv_state_code      TYPE ygms_state_cd
      RETURNING
        VALUE(rv_tax_type) TYPE ygms_tax_type.

    "! <p class="shorttext synchronized" lang="en">Calculate SCM from MMBTU</p>
    "! @parameter iv_qty_mbg | Quantity in MMBTU
    "! @parameter iv_gcv | Gross Calorific Value
    "! @parameter rv_qty_scm | Quantity in SCM
    METHODS calculate_scm_from_mmbtu
      IMPORTING
        iv_qty_mbg       TYPE ygms_qty_mbg
        iv_gcv           TYPE ygms_gcv
      RETURNING
        VALUE(rv_qty_scm) TYPE ygms_qty_scm.

    "! <p class="shorttext synchronized" lang="en">Calculate MMBTU from SCM</p>
    "! @parameter iv_qty_scm | Quantity in SCM
    "! @parameter iv_gcv | Gross Calorific Value
    "! @parameter rv_qty_mbg | Quantity in MMBTU
    METHODS calculate_mmbtu_from_scm
      IMPORTING
        iv_qty_scm       TYPE ygms_qty_scm
        iv_gcv           TYPE ygms_gcv
      RETURNING
        VALUE(rv_qty_mbg) TYPE ygms_qty_mbg.

  PRIVATE SECTION.
    DATA: mv_gail_state TYPE ygms_state_cd VALUE 'GJ'.  " Gujarat - GAIL's state

    "! <p class="shorttext synchronized" lang="en">Calculate allocation percentage</p>
    METHODS calculate_allocation_percentage
      IMPORTING
        it_sales_data         TYPE ygms_tt_sales_data
        it_excluded_states    TYPE ygms_rt_state_code
      RETURNING
        VALUE(rt_percentages) TYPE STANDARD TABLE OF ty_state_percentage.

    "! Structure for state-wise percentage
    TYPES: BEGIN OF ty_state_percentage,
             state_code TYPE ygms_state_cd,
             state      TYPE ygms_state,
             percentage TYPE p DECIMALS 6,
           END OF ty_state_percentage.

    TYPES: tt_state_percentage TYPE STANDARD TABLE OF ty_state_percentage
                               WITH KEY state_code.

ENDCLASS.


CLASS ygms_cl_cst_allocator IMPLEMENTATION.

  METHOD constructor.
    " Initialize state code for Gujarat (GAIL's state for VAT)
    mv_gail_state = ygms_if_cst_constants=>co_state_gujarat.
  ENDMETHOD.


  METHOD calculate_allocation.
    DATA: lt_percentages TYPE tt_state_percentage,
          lv_total_sales TYPE ygms_qty_mbg.

    " Calculate state-wise sales percentages
    " First, aggregate sales by state
    DATA(lt_state_sales) = VALUE tt_state_percentage( ).

    LOOP AT it_sales_data INTO DATA(ls_sales)
      GROUP BY ( state_code = ls_sales-state_code
                 state      = ls_sales-state )
      INTO DATA(ls_group).

      DATA(lv_state_total) = REDUCE ygms_qty_mbg(
        INIT sum = CONV ygms_qty_mbg( 0 )
        FOR member IN GROUP ls_group
        NEXT sum = sum + member-sales_qty
      ).

      " Check if state is excluded
      IF ls_group-state_code NOT IN it_excluded_states.
        APPEND VALUE #(
          state_code = ls_group-state_code
          state      = ls_group-state
          percentage = lv_state_total
        ) TO lt_state_sales.
        lv_total_sales = lv_total_sales + lv_state_total.
      ENDIF.
    ENDLOOP.

    " Calculate percentages
    IF lv_total_sales > 0.
      LOOP AT lt_state_sales ASSIGNING FIELD-SYMBOL(<fs_state>).
        <fs_state>-percentage = <fs_state>-percentage / lv_total_sales.
      ENDLOOP.
    ENDIF.

    " If no sales data, allocate 100% to Gujarat
    IF lt_state_sales IS INITIAL.
      lt_state_sales = VALUE #( (
        state_code = mv_gail_state
        state      = 'GUJARAT'
        percentage = 1
      ) ).
    ENDIF.

    " Allocate receipt quantities based on percentages
    LOOP AT it_receipt_data INTO DATA(ls_receipt)
      GROUP BY ( gas_day     = ls_receipt-gas_day
                 location_id = ls_receipt-location_id
                 material    = ls_receipt-material )
      INTO DATA(ls_receipt_group).

      " Sum total receipt for this group
      DATA(lv_total_mbg) = REDUCE ygms_qty_mbg(
        INIT sum = CONV ygms_qty_mbg( 0 )
        FOR member IN GROUP ls_receipt_group
        NEXT sum = sum + member-qty_mbg
      ).

      DATA(lv_total_scm) = REDUCE ygms_qty_scm(
        INIT sum = CONV ygms_qty_scm( 0 )
        FOR member IN GROUP ls_receipt_group
        NEXT sum = sum + member-qty_scm
      ).

      " Allocate to each state based on percentage
      LOOP AT lt_state_sales INTO DATA(ls_state).
        DATA(lv_alloc_mbg) = lv_total_mbg * ls_state-percentage.
        DATA(lv_alloc_scm) = lv_total_scm * ls_state-percentage.

        " Check if excluded
        DATA(lv_excluded) = xsdbool( ls_state-state_code IN it_excluded_states ).

        INSERT VALUE #(
          gas_day       = ls_receipt_group-gas_day
          location_id   = ls_receipt_group-location_id
          material      = ls_receipt_group-material
          state_code    = ls_state-state_code
          state         = ls_state-state
          sales_qty     = ls_state-percentage * lv_total_mbg
          alloc_qty_mbg = lv_alloc_mbg
          alloc_qty_scm = lv_alloc_scm
          excluded      = COND #( WHEN lv_excluded = abap_true
                                  THEN ygms_if_cst_constants=>co_flag_true
                                  ELSE ygms_if_cst_constants=>co_flag_false )
        ) INTO TABLE rt_allocation.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD apply_variance_adjustment.
    " Apply variance adjustment to ensure allocation totals match supply
    " Variance is allocated to the highest allocation (typically Gujarat)

    " Group by day/location/material
    LOOP AT ct_allocation ASSIGNING FIELD-SYMBOL(<fs_alloc>)
      GROUP BY ( gas_day     = <fs_alloc>-gas_day
                 location_id = <fs_alloc>-location_id
                 material    = <fs_alloc>-material )
      INTO DATA(ls_group).

      " Calculate total allocation for this group
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

      " Find corresponding receipt total
      DATA(lv_supply_mbg) = REDUCE ygms_qty_mbg(
        INIT sum = CONV ygms_qty_mbg( 0 )
        FOR ls_receipt IN it_receipt_data
        WHERE ( gas_day     = ls_group-gas_day
            AND location_id = ls_group-location_id
            AND material    = ls_group-material )
        NEXT sum = sum + ls_receipt-qty_mbg
      ).

      DATA(lv_supply_scm) = REDUCE ygms_qty_scm(
        INIT sum = CONV ygms_qty_scm( 0 )
        FOR ls_receipt IN it_receipt_data
        WHERE ( gas_day     = ls_group-gas_day
            AND location_id = ls_group-location_id
            AND material    = ls_group-material )
        NEXT sum = sum + ls_receipt-qty_scm
      ).

      " Calculate variance
      DATA(lv_var_mbg) = lv_supply_mbg - lv_alloc_mbg.
      DATA(lv_var_scm) = lv_supply_scm - lv_alloc_scm.

      " Apply variance to highest allocation (Gujarat first, then others)
      IF lv_var_mbg <> 0 OR lv_var_scm <> 0.
        " Find Gujarat allocation first
        LOOP AT ct_allocation ASSIGNING FIELD-SYMBOL(<fs_adj>)
          WHERE gas_day     = ls_group-gas_day
            AND location_id = ls_group-location_id
            AND material    = ls_group-material
            AND state_code  = mv_gail_state
            AND excluded    = ygms_if_cst_constants=>co_flag_false.
          <fs_adj>-alloc_qty_mbg = <fs_adj>-alloc_qty_mbg + lv_var_mbg.
          <fs_adj>-alloc_qty_scm = <fs_adj>-alloc_qty_scm + lv_var_scm.
          EXIT.  " Adjust only Gujarat
        ENDLOOP.

        IF sy-subrc <> 0.
          " Gujarat not found, adjust the first non-excluded state
          LOOP AT ct_allocation ASSIGNING <fs_adj>
            WHERE gas_day     = ls_group-gas_day
              AND location_id = ls_group-location_id
              AND material    = ls_group-material
              AND excluded    = ygms_if_cst_constants=>co_flag_false.
            <fs_adj>-alloc_qty_mbg = <fs_adj>-alloc_qty_mbg + lv_var_mbg.
            <fs_adj>-alloc_qty_scm = <fs_adj>-alloc_qty_scm + lv_var_scm.
            EXIT.  " Adjust only first found
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD determine_tax_type.
    " Gujarat = VAT (same state as GAIL)
    " Other states = CST (Central Sales Tax)
    IF iv_state_code = mv_gail_state.
      rv_tax_type = ygms_if_cst_constants=>co_tax_vat.
    ELSE.
      rv_tax_type = ygms_if_cst_constants=>co_tax_cst.
    ENDIF.
  ENDMETHOD.


  METHOD calculate_scm_from_mmbtu.
    " Formula: SCM = MMBTU * 1000000 / (GCV * Conversion Factor)
    " Standard conversion: 1 MMBTU = 252 kcal approximately
    " GCV is in kcal/SCM

    IF iv_gcv > 0.
      " Standard formula for natural gas conversion
      " 1 MMBTU = 293.07 kWh = approximately 252,000 kcal
      " SCM = MMBTU * 252000 / GCV
      rv_qty_scm = iv_qty_mbg * 252000 / iv_gcv.
    ELSE.
      " Use standard conversion factor if GCV not available
      rv_qty_scm = iv_qty_mbg * ygms_if_cst_constants=>co_mmbtu_to_scm.
    ENDIF.
  ENDMETHOD.


  METHOD calculate_mmbtu_from_scm.
    " Formula: MMBTU = SCM * GCV / 252000
    IF iv_gcv > 0.
      rv_qty_mbg = iv_qty_scm * iv_gcv / 252000.
    ELSE.
      " Use standard conversion factor if GCV not available
      rv_qty_mbg = iv_qty_scm / ygms_if_cst_constants=>co_mmbtu_to_scm.
    ENDIF.
  ENDMETHOD.


  METHOD calculate_allocation_percentage.
    DATA: lv_total TYPE ygms_qty_mbg.

    " Calculate total excluding excluded states
    LOOP AT it_sales_data INTO DATA(ls_sales)
      WHERE state_code NOT IN it_excluded_states.
      lv_total = lv_total + ls_sales-sales_qty.
    ENDLOOP.

    " Calculate percentages
    LOOP AT it_sales_data INTO ls_sales
      WHERE state_code NOT IN it_excluded_states
      GROUP BY ( state_code = ls_sales-state_code
                 state      = ls_sales-state )
      INTO DATA(ls_group).

      DATA(lv_state_total) = REDUCE ygms_qty_mbg(
        INIT sum = CONV ygms_qty_mbg( 0 )
        FOR member IN GROUP ls_group
        NEXT sum = sum + member-sales_qty
      ).

      IF lv_total > 0.
        APPEND VALUE #(
          state_code = ls_group-state_code
          state      = ls_group-state
          percentage = lv_state_total / lv_total
        ) TO rt_percentages.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
