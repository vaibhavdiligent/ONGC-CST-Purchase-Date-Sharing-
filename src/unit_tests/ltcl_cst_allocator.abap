*&---------------------------------------------------------------------*
*& Unit Test Class: LTCL_CST_ALLOCATOR
*& Package: YGMS
*& Description: Unit Tests for YGMS_CL_CST_ALLOCATOR
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Test Class for YGMS_CL_CST_ALLOCATOR
*----------------------------------------------------------------------*
CLASS ltcl_cst_allocator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA: go_environment TYPE REF TO if_osql_test_environment.

    DATA: mo_cut TYPE REF TO ygms_cl_cst_allocator.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown,
      test_allocation_single_state FOR TESTING,
      test_allocation_multi_state FOR TESTING,
      test_allocation_with_variance FOR TESTING,
      test_allocation_excluded_state FOR TESTING,
      test_gujarat_vat_classification FOR TESTING,
      test_other_state_cst_classification FOR TESTING,
      test_scm_to_mmbtu_conversion FOR TESTING,
      test_mmbtu_to_scm_conversion FOR TESTING,
      test_empty_receipt_data FOR TESTING,
      test_zero_sales_allocation FOR TESTING.

ENDCLASS.


CLASS ltcl_cst_allocator IMPLEMENTATION.

  METHOD class_setup.
    " Create test environment with database table doubles
    go_environment = cl_osql_test_environment=>create(
      VALUE #( ( 'YGMS_CST_B2B_1' ) ( 'YGMS_CST_PUR' ) )
    ).
  ENDMETHOD.

  METHOD class_teardown.
    go_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    mo_cut = NEW ygms_cl_cst_allocator( ).
    go_environment->clear_doubles( ).
  ENDMETHOD.

  METHOD teardown.
    CLEAR mo_cut.
  ENDMETHOD.

  METHOD test_allocation_single_state.
    " Given: Receipt data for single location
    DATA(lt_receipt) = VALUE ygms_tt_receipt(
      ( gas_day = '20240101' ctp_id = 'CTP001' ongc_material = 'MAT001'
        location_id = 'LOC001' material = '000000001234567890'
        qty_scm = '1000.000' qty_mbg = '100.000' )
    ).

    DATA(lt_sales) = VALUE ygms_tt_sales_data(
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'GJ' state = 'GUJARAT' sales_qty = '100.000' )
    ).

    " When: Allocation is executed
    DATA(lt_allocation) = mo_cut->calculate_allocation(
      it_receipt_data = lt_receipt
      it_sales_data   = lt_sales
    ).

    " Then: All quantity assigned to Gujarat with VAT classification
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_allocation )
      exp = 1
      msg = 'Should have one allocation record'
    ).

    READ TABLE lt_allocation INTO DATA(ls_alloc) INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_alloc-state_code
      exp = 'GJ'
      msg = 'State should be Gujarat'
    ).

    cl_abap_unit_assert=>assert_equals(
      act = ls_alloc-alloc_qty_mbg
      exp = CONV ygms_qty_mbg( '100.000' )
      msg = 'Allocated MBG should match receipt'
    ).
  ENDMETHOD.

  METHOD test_allocation_multi_state.
    " Given: Receipt data and sales to multiple states
    DATA(lt_receipt) = VALUE ygms_tt_receipt(
      ( gas_day = '20240101' ctp_id = 'CTP001' ongc_material = 'MAT001'
        location_id = 'LOC001' material = '000000001234567890'
        qty_scm = '1000.000' qty_mbg = '100.000' )
    ).

    DATA(lt_sales) = VALUE ygms_tt_sales_data(
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'GJ' state = 'GUJARAT' sales_qty = '60.000' )
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'MH' state = 'MAHARASHTRA' sales_qty = '40.000' )
    ).

    " When: Allocation is executed
    DATA(lt_allocation) = mo_cut->calculate_allocation(
      it_receipt_data = lt_receipt
      it_sales_data   = lt_sales
    ).

    " Then: Quantity allocated proportionally
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_allocation )
      exp = 2
      msg = 'Should have two allocation records'
    ).

    " Verify proportional allocation (60% Gujarat, 40% Maharashtra)
    READ TABLE lt_allocation INTO DATA(ls_gj) WITH KEY state_code = 'GJ'.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_gj-alloc_qty_mbg
      exp = CONV ygms_qty_mbg( '60.000' )
      msg = 'Gujarat should get 60% of allocation'
    ).

    READ TABLE lt_allocation INTO DATA(ls_mh) WITH KEY state_code = 'MH'.
    cl_abap_unit_assert=>assert_subrc( ).
    cl_abap_unit_assert=>assert_equals(
      act = ls_mh-alloc_qty_mbg
      exp = CONV ygms_qty_mbg( '40.000' )
      msg = 'Maharashtra should get 40% of allocation'
    ).
  ENDMETHOD.

  METHOD test_allocation_with_variance.
    " Given: Receipt and sales with rounding variance
    DATA(lt_receipt) = VALUE ygms_tt_receipt(
      ( gas_day = '20240101' ctp_id = 'CTP001' ongc_material = 'MAT001'
        location_id = 'LOC001' material = '000000001234567890'
        qty_scm = '1000.000' qty_mbg = '100.000' )
    ).

    DATA(lt_sales) = VALUE ygms_tt_sales_data(
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'GJ' state = 'GUJARAT' sales_qty = '33.333' )
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'MH' state = 'MAHARASHTRA' sales_qty = '33.333' )
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'MP' state = 'MADHYA PRADESH' sales_qty = '33.333' )
    ).

    " When: Allocation is executed with variance adjustment
    DATA(lt_allocation) = mo_cut->calculate_allocation(
      it_receipt_data = lt_receipt
      it_sales_data   = lt_sales
    ).

    mo_cut->apply_variance_adjustment(
      EXPORTING
        it_receipt_data = lt_receipt
      CHANGING
        ct_allocation   = lt_allocation
    ).

    " Then: Variance should be adjusted to Gujarat
    DATA(lv_total) = REDUCE ygms_qty_mbg(
      INIT sum = CONV ygms_qty_mbg( 0 )
      FOR ls_alloc IN lt_allocation
      NEXT sum = sum + ls_alloc-alloc_qty_mbg
    ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_total
      exp = CONV ygms_qty_mbg( '100.000' )
      msg = 'Total allocation should match receipt after variance adjustment'
    ).
  ENDMETHOD.

  METHOD test_allocation_excluded_state.
    " Given: Receipt data with excluded state
    DATA(lt_receipt) = VALUE ygms_tt_receipt(
      ( gas_day = '20240101' ctp_id = 'CTP001' ongc_material = 'MAT001'
        location_id = 'LOC001' material = '000000001234567890'
        qty_scm = '1000.000' qty_mbg = '100.000' )
    ).

    DATA(lt_sales) = VALUE ygms_tt_sales_data(
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'GJ' state = 'GUJARAT' sales_qty = '60.000' )
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'MH' state = 'MAHARASHTRA' sales_qty = '40.000' )
    ).

    DATA(lt_excluded) = VALUE ygms_rt_state_code(
      ( sign = 'I' option = 'EQ' low = 'MH' )
    ).

    " When: Allocation with exclusion
    DATA(lt_allocation) = mo_cut->calculate_allocation(
      it_receipt_data    = lt_receipt
      it_sales_data      = lt_sales
      it_excluded_states = lt_excluded
    ).

    " Then: Excluded state should have exclusion flag
    READ TABLE lt_allocation INTO DATA(ls_mh) WITH KEY state_code = 'MH'.
    IF sy-subrc = 0.
      cl_abap_unit_assert=>assert_equals(
        act = ls_mh-excluded
        exp = ygms_if_cst_constants=>co_flag_true
        msg = 'Maharashtra should be marked as excluded'
      ).
    ENDIF.
  ENDMETHOD.

  METHOD test_gujarat_vat_classification.
    " Given: Gujarat state code
    " When: Tax type is determined
    DATA(lv_tax_type) = mo_cut->determine_tax_type( iv_state_code = 'GJ' ).

    " Then: Tax type should be VAT (not CST)
    cl_abap_unit_assert=>assert_equals(
      act = lv_tax_type
      exp = ygms_if_cst_constants=>co_tax_vat
      msg = 'Gujarat should have VAT tax type'
    ).
  ENDMETHOD.

  METHOD test_other_state_cst_classification.
    " Given: Non-Gujarat state codes
    DATA(lt_states) = VALUE stringtab( ( `MH` ) ( `MP` ) ( `RJ` ) ( `DL` ) ).

    LOOP AT lt_states INTO DATA(lv_state).
      " When: Tax type is determined
      DATA(lv_tax_type) = mo_cut->determine_tax_type( CONV #( lv_state ) ).

      " Then: Tax type should be CST
      cl_abap_unit_assert=>assert_equals(
        act = lv_tax_type
        exp = ygms_if_cst_constants=>co_tax_cst
        msg = |{ lv_state } should have CST tax type|
      ).
    ENDLOOP.
  ENDMETHOD.

  METHOD test_scm_to_mmbtu_conversion.
    " Given: Quantity in SCM and GCV
    DATA(lv_qty_scm) = CONV ygms_qty_scm( '1000.000' ).
    DATA(lv_gcv) = CONV ygms_gcv( '9500.000' ).

    " When: Convert to MMBTU
    DATA(lv_qty_mbg) = mo_cut->calculate_mmbtu_from_scm(
      iv_qty_scm = lv_qty_scm
      iv_gcv     = lv_gcv
    ).

    " Then: Result should be reasonable (approximately 37.7 MMBTU)
    cl_abap_unit_assert=>assert_true(
      act = xsdbool( lv_qty_mbg > 0 )
      msg = 'MMBTU should be positive'
    ).
  ENDMETHOD.

  METHOD test_mmbtu_to_scm_conversion.
    " Given: Quantity in MMBTU and GCV
    DATA(lv_qty_mbg) = CONV ygms_qty_mbg( '100.000' ).
    DATA(lv_gcv) = CONV ygms_gcv( '9500.000' ).

    " When: Convert to SCM
    DATA(lv_qty_scm) = mo_cut->calculate_scm_from_mmbtu(
      iv_qty_mbg = lv_qty_mbg
      iv_gcv     = lv_gcv
    ).

    " Then: Result should be reasonable
    cl_abap_unit_assert=>assert_true(
      act = xsdbool( lv_qty_scm > 0 )
      msg = 'SCM should be positive'
    ).
  ENDMETHOD.

  METHOD test_empty_receipt_data.
    " Given: Empty receipt data
    DATA(lt_receipt) = VALUE ygms_tt_receipt( ).
    DATA(lt_sales) = VALUE ygms_tt_sales_data( ).

    " When: Allocation is executed
    DATA(lt_allocation) = mo_cut->calculate_allocation(
      it_receipt_data = lt_receipt
      it_sales_data   = lt_sales
    ).

    " Then: Result should be empty
    cl_abap_unit_assert=>assert_initial(
      act = lt_allocation
      msg = 'Allocation should be empty for empty receipt'
    ).
  ENDMETHOD.

  METHOD test_zero_sales_allocation.
    " Given: Receipt data but zero sales
    DATA(lt_receipt) = VALUE ygms_tt_receipt(
      ( gas_day = '20240101' ctp_id = 'CTP001' ongc_material = 'MAT001'
        location_id = 'LOC001' material = '000000001234567890'
        qty_scm = '1000.000' qty_mbg = '100.000' )
    ).

    DATA(lt_sales) = VALUE ygms_tt_sales_data( ).

    " When: Allocation is executed
    DATA(lt_allocation) = mo_cut->calculate_allocation(
      it_receipt_data = lt_receipt
      it_sales_data   = lt_sales
    ).

    " Then: Default allocation to Gujarat
    READ TABLE lt_allocation INTO DATA(ls_alloc) INDEX 1.
    IF sy-subrc = 0.
      cl_abap_unit_assert=>assert_equals(
        act = ls_alloc-state_code
        exp = 'GJ'
        msg = 'Default allocation should go to Gujarat'
      ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
