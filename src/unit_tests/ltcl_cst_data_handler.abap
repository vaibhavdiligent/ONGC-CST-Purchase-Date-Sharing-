*&---------------------------------------------------------------------*
*& Unit Test Class: LTCL_CST_DATA_HANDLER
*& Package: YGMS
*& Description: Unit Tests for YGMS_CL_CST_DATA_HANDLER
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Test Class for YGMS_CL_CST_DATA_HANDLER
*----------------------------------------------------------------------*
CLASS ltcl_cst_data_handler DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA: go_environment TYPE REF TO if_osql_test_environment.

    DATA: mo_cut           TYPE REF TO ygms_cl_cst_data_handler,
          mo_audit_handler TYPE REF TO ygms_cl_cst_audit_handler.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      teardown,
      test_get_location_mapping_found FOR TESTING,
      test_get_location_mapping_not_found FOR TESTING,
      test_get_location_mapping_expired FOR TESTING,
      test_get_material_mapping_found FOR TESTING,
      test_get_material_mapping_not_found FOR TESTING,
      test_save_receipt_data FOR TESTING,
      test_get_receipt_data FOR TESTING,
      test_check_data_exists FOR TESTING.

ENDCLASS.


CLASS ltcl_cst_data_handler IMPLEMENTATION.

  METHOD class_setup.
    " Create test environment with database table doubles
    go_environment = cl_osql_test_environment=>create(
      VALUE #(
        ( 'YGMS_CST_LOC_MAP' )
        ( 'YGMS_CST_MAT_MAP' )
        ( 'YGMS_CST_B2B_1' )
        ( 'YGMS_CST_PUR' )
        ( 'YGMS_CST_AUDIT_LOG' )
      )
    ).
  ENDMETHOD.

  METHOD class_teardown.
    go_environment->destroy( ).
  ENDMETHOD.

  METHOD setup.
    go_environment->clear_doubles( ).

    " Create audit handler and data handler
    mo_audit_handler = NEW ygms_cl_cst_audit_handler( ).
    mo_cut = NEW ygms_cl_cst_data_handler(
      io_audit_handler = mo_audit_handler
    ).

    " Insert test data for location mapping
    DATA(lt_loc_map) = VALUE STANDARD TABLE OF ygms_cst_loc_map(
      ( mandt = sy-mandt ctp_id = 'CTP001' valid_from = '20230101' valid_to = '99991231'
        location_id = 'LOC001' description = 'Test Location 1' )
      ( mandt = sy-mandt ctp_id = 'CTP002' valid_from = '20230101' valid_to = '20231231'
        location_id = 'LOC002' description = 'Test Location 2 - Expired' )
    ).
    go_environment->insert_test_data( lt_loc_map ).

    " Insert test data for material mapping
    DATA(lt_mat_map) = VALUE STANDARD TABLE OF ygms_cst_mat_map(
      ( mandt = sy-mandt ongc_material = 'ONGC_MAT001' location_id = 'LOC001'
        valid_from = '20230101' valid_to = '99991231' gail_material = '000000001234567890' )
    ).
    go_environment->insert_test_data( lt_mat_map ).
  ENDMETHOD.

  METHOD teardown.
    CLEAR: mo_cut, mo_audit_handler.
  ENDMETHOD.

  METHOD test_get_location_mapping_found.
    " Given: Valid CTP ID and date
    " When: Get location mapping
    TRY.
        DATA(lv_location) = mo_cut->get_location_mapping(
          iv_ctp_id     = 'CTP001'
          iv_valid_date = '20240115'
        ).

        " Then: Location should be returned
        cl_abap_unit_assert=>assert_equals(
          act = lv_location
          exp = 'LOC001'
          msg = 'Should return correct location mapping'
        ).

      CATCH ygms_cx_cst_error.
        cl_abap_unit_assert=>fail( 'Should not raise exception for valid mapping' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_get_location_mapping_not_found.
    " Given: Invalid CTP ID
    " When: Get location mapping
    TRY.
        DATA(lv_location) = mo_cut->get_location_mapping(
          iv_ctp_id     = 'INVALID'
          iv_valid_date = '20240115'
        ).

        cl_abap_unit_assert=>fail( 'Should raise exception for invalid mapping' ).

      CATCH ygms_cx_cst_error INTO DATA(lx_error).
        " Then: Exception should be raised
        cl_abap_unit_assert=>assert_bound( lx_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_get_location_mapping_expired.
    " Given: CTP ID with expired validity
    " When: Get location mapping for date after expiry
    TRY.
        DATA(lv_location) = mo_cut->get_location_mapping(
          iv_ctp_id     = 'CTP002'
          iv_valid_date = '20240115'  " After 20231231
        ).

        cl_abap_unit_assert=>fail( 'Should raise exception for expired mapping' ).

      CATCH ygms_cx_cst_error INTO DATA(lx_error).
        " Then: Exception should be raised
        cl_abap_unit_assert=>assert_bound( lx_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_get_material_mapping_found.
    " Given: Valid ONGC material and location
    " When: Get material mapping
    TRY.
        DATA(lv_material) = mo_cut->get_material_mapping(
          iv_ongc_material = 'ONGC_MAT001'
          iv_location_id   = 'LOC001'
          iv_valid_date    = '20240115'
        ).

        " Then: Material should be returned
        cl_abap_unit_assert=>assert_equals(
          act = lv_material
          exp = '000000001234567890'
          msg = 'Should return correct material mapping'
        ).

      CATCH ygms_cx_cst_error.
        cl_abap_unit_assert=>fail( 'Should not raise exception for valid mapping' ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_get_material_mapping_not_found.
    " Given: Invalid ONGC material
    " When: Get material mapping
    TRY.
        DATA(lv_material) = mo_cut->get_material_mapping(
          iv_ongc_material = 'INVALID'
          iv_location_id   = 'LOC001'
          iv_valid_date    = '20240115'
        ).

        cl_abap_unit_assert=>fail( 'Should raise exception for invalid mapping' ).

      CATCH ygms_cx_cst_error INTO DATA(lx_error).
        " Then: Exception should be raised
        cl_abap_unit_assert=>assert_bound( lx_error ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_save_receipt_data.
    " Given: Receipt data to save
    DATA(lt_receipt) = VALUE ygms_tt_receipt(
      ( gas_day = '20240101' ctp_id = 'CTP001' ongc_material = 'MAT001'
        location_id = 'LOC001' material = '000000001234567890'
        qty_scm = '1000.000' qty_mbg = '100.000' ongc_id = 'ONGC00001' )
    ).

    " When: Save receipt data
    TRY.
        mo_cut->save_receipt_data( lt_receipt ).

        " Then: Data should be saved
        SELECT COUNT(*) FROM ygms_cst_b2b_1 INTO @DATA(lv_count).
        cl_abap_unit_assert=>assert_equals(
          act = lv_count
          exp = 1
          msg = 'Receipt data should be saved'
        ).

      CATCH ygms_cx_cst_error INTO DATA(lx_error).
        cl_abap_unit_assert=>fail( lx_error->get_text( ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_get_receipt_data.
    " Given: Receipt data in database
    DATA(lt_receipt) = VALUE STANDARD TABLE OF ygms_cst_b2b_1(
      ( mandt = sy-mandt gas_day = '20240101' ctp_id = 'CTP001'
        ongc_material = 'MAT001' location_id = 'LOC001'
        material = '000000001234567890' qty_scm = '1000.000' qty_mbg = '100.000' )
      ( mandt = sy-mandt gas_day = '20240102' ctp_id = 'CTP001'
        ongc_material = 'MAT001' location_id = 'LOC001'
        material = '000000001234567890' qty_scm = '2000.000' qty_mbg = '200.000' )
    ).
    go_environment->insert_test_data( lt_receipt ).

    " When: Get receipt data
    DATA(lt_result) = mo_cut->get_receipt_data(
      iv_from_date   = '20240101'
      iv_to_date     = '20240131'
      iv_location_id = 'LOC001'
    ).

    " Then: Both records should be returned
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_result )
      exp = 2
      msg = 'Should return all receipt records in date range'
    ).
  ENDMETHOD.

  METHOD test_check_data_exists.
    " Given: Purchase data in database
    DATA(lt_purchase) = VALUE STANDARD TABLE OF ygms_cst_pur(
      ( mandt = sy-mandt gas_day = '20240101' location_id = 'LOC001'
        material = '000000001234567890' state_code = 'GJ'
        qty_mbg = '100.000' qty_scm = '1000.000' )
    ).
    go_environment->insert_test_data( lt_purchase ).

    " When: Check data exists
    DATA(lv_exists) = mo_cut->check_data_exists(
      iv_from_date   = '20240101'
      iv_to_date     = '20240131'
      iv_location_id = 'LOC001'
    ).

    " Then: Should return true
    cl_abap_unit_assert=>assert_true(
      act = lv_exists
      msg = 'Should return true when data exists'
    ).

    " Check non-existing data
    DATA(lv_not_exists) = mo_cut->check_data_exists(
      iv_from_date   = '20250101'
      iv_to_date     = '20250131'
      iv_location_id = 'LOC001'
    ).

    cl_abap_unit_assert=>assert_false(
      act = lv_not_exists
      msg = 'Should return false when data does not exist'
    ).
  ENDMETHOD.

ENDCLASS.
