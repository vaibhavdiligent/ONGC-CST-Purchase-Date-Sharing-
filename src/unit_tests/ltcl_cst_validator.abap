*&---------------------------------------------------------------------*
*& Unit Test Class: LTCL_CST_VALIDATOR
*& Package: YGMS
*& Description: Unit Tests for YGMS_CL_CST_VALIDATOR
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Test Class for YGMS_CL_CST_VALIDATOR
*----------------------------------------------------------------------*
CLASS ltcl_cst_validator DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO ygms_cl_cst_validator.

    METHODS:
      setup,
      teardown,
      test_validate_totals_success FOR TESTING,
      test_validate_totals_failure FOR TESTING,
      test_check_mandatory_all_present FOR TESTING,
      test_check_mandatory_missing_fields FOR TESTING,
      test_validate_ranges_positive FOR TESTING,
      test_validate_ranges_negative FOR TESTING,
      test_validate_gcv_ncv_ratio FOR TESTING,
      test_validate_tax_type FOR TESTING,
      test_validation_results_match FOR TESTING,
      test_validation_results_mismatch FOR TESTING.

ENDCLASS.


CLASS ltcl_cst_validator IMPLEMENTATION.

  METHOD setup.
    mo_cut = NEW ygms_cl_cst_validator( ).
  ENDMETHOD.

  METHOD teardown.
    CLEAR mo_cut.
  ENDMETHOD.

  METHOD test_validate_totals_success.
    " Given: Valid allocation data
    DATA(lt_allocation) = VALUE ygms_tt_allocation(
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'GJ' alloc_qty_mbg = '60.000' alloc_qty_scm = '600.000' )
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'MH' alloc_qty_mbg = '40.000' alloc_qty_scm = '400.000' )
    ).

    " When: Validate totals
    DATA(lv_valid) = mo_cut->validate_totals( lt_allocation ).

    " Then: Should be valid
    cl_abap_unit_assert=>assert_true(
      act = lv_valid
      msg = 'Valid allocation should pass validation'
    ).
  ENDMETHOD.

  METHOD test_validate_totals_failure.
    " Given: Allocation with negative values
    DATA(lt_allocation) = VALUE ygms_tt_allocation(
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'GJ' alloc_qty_mbg = '-60.000' alloc_qty_scm = '600.000' )
    ).

    " When: Validate totals
    DATA(lv_valid) = mo_cut->validate_totals( lt_allocation ).

    " Then: Should be invalid
    cl_abap_unit_assert=>assert_false(
      act = lv_valid
      msg = 'Negative allocation should fail validation'
    ).
  ENDMETHOD.

  METHOD test_check_mandatory_all_present.
    " Given: Complete allocation data
    DATA(lt_allocation) = VALUE ygms_tt_allocation(
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'GJ' alloc_qty_mbg = '100.000' )
    ).

    " When: Check mandatory fields
    DATA(lt_errors) = mo_cut->check_mandatory( lt_allocation ).

    " Then: No errors
    cl_abap_unit_assert=>assert_initial(
      act = lt_errors
      msg = 'Complete data should have no mandatory field errors'
    ).
  ENDMETHOD.

  METHOD test_check_mandatory_missing_fields.
    " Given: Incomplete allocation data
    DATA(lt_allocation) = VALUE ygms_tt_allocation(
      ( gas_day = '00000000' location_id = '' material = ''
        state_code = '' alloc_qty_mbg = '0.000' alloc_qty_scm = '0.000' )
    ).

    " When: Check mandatory fields
    DATA(lt_errors) = mo_cut->check_mandatory( lt_allocation ).

    " Then: Multiple errors
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_errors
      msg = 'Missing mandatory fields should generate errors'
    ).
  ENDMETHOD.

  METHOD test_validate_ranges_positive.
    " Given: Valid range values
    DATA(lt_allocation) = VALUE ygms_tt_allocation(
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'GJ' alloc_qty_mbg = '100.000' alloc_qty_scm = '1000.000'
        gcv = '9500.000' ncv = '8550.000' tax_type = 'VAT' )
    ).

    " When: Validate ranges
    DATA(lt_errors) = mo_cut->validate_ranges( lt_allocation ).

    " Then: No errors
    cl_abap_unit_assert=>assert_initial(
      act = lt_errors
      msg = 'Valid ranges should have no errors'
    ).
  ENDMETHOD.

  METHOD test_validate_ranges_negative.
    " Given: Negative quantity values
    DATA(lt_allocation) = VALUE ygms_tt_allocation(
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'GJ' alloc_qty_mbg = '-100.000' alloc_qty_scm = '-1000.000' )
    ).

    " When: Validate ranges
    DATA(lt_errors) = mo_cut->validate_ranges( lt_allocation ).

    " Then: Errors for negative values
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_errors
      msg = 'Negative quantities should generate errors'
    ).
  ENDMETHOD.

  METHOD test_validate_gcv_ncv_ratio.
    " Given: NCV greater than GCV (invalid)
    DATA(lt_allocation) = VALUE ygms_tt_allocation(
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'GJ' alloc_qty_mbg = '100.000' alloc_qty_scm = '1000.000'
        gcv = '8000.000' ncv = '9500.000' )  " NCV > GCV is invalid
    ).

    " When: Validate ranges
    DATA(lt_errors) = mo_cut->validate_ranges( lt_allocation ).

    " Then: Error for invalid GCV/NCV ratio
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_errors
      msg = 'NCV greater than GCV should generate error'
    ).
  ENDMETHOD.

  METHOD test_validate_tax_type.
    " Given: Invalid tax type
    DATA(lt_allocation) = VALUE ygms_tt_allocation(
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'GJ' alloc_qty_mbg = '100.000' tax_type = 'XXX' )
    ).

    " When: Validate ranges
    DATA(lt_errors) = mo_cut->validate_ranges( lt_allocation ).

    " Then: Error for invalid tax type
    cl_abap_unit_assert=>assert_not_initial(
      act = lt_errors
      msg = 'Invalid tax type should generate error'
    ).
  ENDMETHOD.

  METHOD test_validation_results_match.
    " Given: Receipt and allocation that match
    DATA(lt_receipt) = VALUE ygms_tt_receipt(
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        qty_scm = '1000.000' qty_mbg = '100.000' )
    ).

    DATA(lt_allocation) = VALUE ygms_tt_allocation(
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'GJ' alloc_qty_mbg = '60.000' alloc_qty_scm = '600.000' )
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'MH' alloc_qty_mbg = '40.000' alloc_qty_scm = '400.000' )
    ).

    " When: Get validation results
    DATA(lt_results) = mo_cut->get_validation_results(
      it_receipt    = lt_receipt
      it_allocation = lt_allocation
    ).

    " Then: Status should be valid
    READ TABLE lt_results INTO DATA(ls_result) INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-status
      exp = ygms_if_cst_constants=>co_status_valid
      msg = 'Matching totals should have valid status'
    ).
  ENDMETHOD.

  METHOD test_validation_results_mismatch.
    " Given: Receipt and allocation that don't match
    DATA(lt_receipt) = VALUE ygms_tt_receipt(
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        qty_scm = '1000.000' qty_mbg = '100.000' )
    ).

    DATA(lt_allocation) = VALUE ygms_tt_allocation(
      ( gas_day = '20240101' location_id = 'LOC001' material = '000000001234567890'
        state_code = 'GJ' alloc_qty_mbg = '50.000' alloc_qty_scm = '500.000' )
    ).

    " When: Get validation results
    DATA(lt_results) = mo_cut->get_validation_results(
      it_receipt    = lt_receipt
      it_allocation = lt_allocation
    ).

    " Then: Status should be error (50 allocated vs 100 receipt)
    READ TABLE lt_results INTO DATA(ls_result) INDEX 1.
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-status
      exp = ygms_if_cst_constants=>co_status_error
      msg = 'Mismatched totals should have error status'
    ).

    " Verify difference is calculated correctly
    cl_abap_unit_assert=>assert_equals(
      act = ls_result-diff_mbg
      exp = CONV ygms_qty_mbg( '50.000' )
      msg = 'Difference should be 50 MBG'
    ).
  ENDMETHOD.

ENDCLASS.
