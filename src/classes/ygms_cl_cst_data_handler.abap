*&---------------------------------------------------------------------*
*& Class: YGMS_CL_CST_DATA_HANDLER
*& Package: YGMS
*& Description: Data Access Layer for all DB operations
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

CLASS ygms_cl_cst_data_handler DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    "! @parameter io_audit_handler | Audit handler for logging
    METHODS constructor
      IMPORTING
        io_audit_handler TYPE REF TO ygms_cl_cst_audit_handler OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Get location mapping</p>
    "! @parameter iv_ctp_id | ONGC Terminal ID
    "! @parameter iv_valid_date | Date for validity check
    "! @parameter rv_location_id | Mapped GAIL Location ID
    "! @raising ygms_cx_cst_error | Mapping not found
    METHODS get_location_mapping
      IMPORTING
        iv_ctp_id             TYPE ygms_ctp_id
        iv_valid_date         TYPE datum DEFAULT sy-datum
      RETURNING
        VALUE(rv_location_id) TYPE ygms_loc_id
      RAISING
        ygms_cx_cst_error.

    "! <p class="shorttext synchronized" lang="en">Get material mapping</p>
    "! @parameter iv_ongc_material | ONGC Material Code
    "! @parameter iv_location_id | GAIL Location ID
    "! @parameter iv_valid_date | Date for validity check
    "! @parameter rv_gail_material | Mapped GAIL Material Code
    "! @raising ygms_cx_cst_error | Mapping not found
    METHODS get_material_mapping
      IMPORTING
        iv_ongc_material        TYPE ygms_ongc_mat
        iv_location_id          TYPE ygms_loc_id
        iv_valid_date           TYPE datum DEFAULT sy-datum
      RETURNING
        VALUE(rv_gail_material) TYPE matnr
      RAISING
        ygms_cx_cst_error.

    "! <p class="shorttext synchronized" lang="en">Save receipt data</p>
    "! @parameter it_data | Receipt data to save
    "! @raising ygms_cx_cst_error | Save failed
    METHODS save_receipt_data
      IMPORTING
        it_data TYPE ygms_tt_receipt
      RAISING
        ygms_cx_cst_error.

    "! <p class="shorttext synchronized" lang="en">Save purchase data</p>
    "! @parameter it_data | Purchase data to save
    "! @parameter ev_gail_id | Generated GAIL ID
    "! @raising ygms_cx_cst_error | Save failed
    METHODS save_purchase_data
      IMPORTING
        it_data    TYPE ygms_tt_purchase
      EXPORTING
        ev_gail_id TYPE ygms_gail_id
      RAISING
        ygms_cx_cst_error.

    "! <p class="shorttext synchronized" lang="en">Get receipt data</p>
    "! @parameter iv_from_date | Start date
    "! @parameter iv_to_date | End date
    "! @parameter iv_location_id | Location filter (optional)
    "! @parameter rt_data | Receipt data
    METHODS get_receipt_data
      IMPORTING
        iv_from_date   TYPE datum
        iv_to_date     TYPE datum
        iv_location_id TYPE ygms_loc_id OPTIONAL
      RETURNING
        VALUE(rt_data) TYPE ygms_tt_receipt.

    "! <p class="shorttext synchronized" lang="en">Get purchase data</p>
    "! @parameter iv_from_date | Start date
    "! @parameter iv_to_date | End date
    "! @parameter iv_location_id | Location filter (optional)
    "! @parameter rt_data | Purchase data
    METHODS get_purchase_data
      IMPORTING
        iv_from_date   TYPE datum
        iv_to_date     TYPE datum
        iv_location_id TYPE ygms_loc_id OPTIONAL
      RETURNING
        VALUE(rt_data) TYPE ygms_tt_purchase.

    "! <p class="shorttext synchronized" lang="en">Get sales data for allocation</p>
    "! @parameter iv_from_date | Start date
    "! @parameter iv_to_date | End date
    "! @parameter iv_location_id | Location filter
    "! @parameter rt_data | Sales data
    METHODS get_sales_data
      IMPORTING
        iv_from_date   TYPE datum
        iv_to_date     TYPE datum
        iv_location_id TYPE ygms_loc_id OPTIONAL
      RETURNING
        VALUE(rt_data) TYPE ygms_tt_sales_data.

    "! <p class="shorttext synchronized" lang="en">Get GCV/NCV values</p>
    "! @parameter iv_gas_day | Gas day
    "! @parameter iv_location_id | Location ID
    "! @parameter ev_gcv | Gross Calorific Value
    "! @parameter ev_ncv | Net Calorific Value
    "! @raising ygms_cx_cst_error | GCV/NCV not found
    METHODS get_gcv_ncv
      IMPORTING
        iv_gas_day     TYPE datum
        iv_location_id TYPE ygms_loc_id
      EXPORTING
        ev_gcv         TYPE ygms_gcv
        ev_ncv         TYPE ygms_ncv
      RAISING
        ygms_cx_cst_error.

    "! <p class="shorttext synchronized" lang="en">Mark data as sent</p>
    "! @parameter iv_from_date | Start date
    "! @parameter iv_to_date | End date
    "! @parameter iv_location_id | Location ID
    METHODS mark_as_sent
      IMPORTING
        iv_from_date   TYPE datum
        iv_to_date     TYPE datum
        iv_location_id TYPE ygms_loc_id.

    "! <p class="shorttext synchronized" lang="en">Check if data exists for period</p>
    "! @parameter iv_from_date | Start date
    "! @parameter iv_to_date | End date
    "! @parameter iv_location_id | Location ID
    "! @parameter rv_exists | Returns ABAP_TRUE if data exists
    METHODS check_data_exists
      IMPORTING
        iv_from_date    TYPE datum
        iv_to_date      TYPE datum
        iv_location_id  TYPE ygms_loc_id
      RETURNING
        VALUE(rv_exists) TYPE abap_bool.

    "! <p class="shorttext synchronized" lang="en">Delete existing data for reprocessing</p>
    "! @parameter iv_from_date | Start date
    "! @parameter iv_to_date | End date
    "! @parameter iv_location_id | Location ID
    METHODS delete_existing_data
      IMPORTING
        iv_from_date   TYPE datum
        iv_to_date     TYPE datum
        iv_location_id TYPE ygms_loc_id.

  PRIVATE SECTION.
    DATA: mo_audit_handler TYPE REF TO ygms_cl_cst_audit_handler.

ENDCLASS.


CLASS ygms_cl_cst_data_handler IMPLEMENTATION.

  METHOD constructor.
    mo_audit_handler = io_audit_handler.
  ENDMETHOD.


  METHOD get_location_mapping.
    " Get location mapping with validity check
    SELECT SINGLE location_id
      FROM ygms_cst_loc_map
      WHERE ctp_id     = @iv_ctp_id
        AND valid_from <= @iv_valid_date
        AND valid_to   >= @iv_valid_date
      INTO @rv_location_id.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ygms_cx_cst_error
        EXPORTING
          textid    = ygms_cx_cst_error=>mapping_not_found
          mv_param1 = |{ iv_ctp_id }|
          mv_param2 = |{ iv_valid_date DATE = USER }|.
    ENDIF.
  ENDMETHOD.


  METHOD get_material_mapping.
    " Get material mapping with validity check
    SELECT SINGLE gail_material
      FROM ygms_cst_mat_map
      WHERE ongc_material = @iv_ongc_material
        AND location_id   = @iv_location_id
        AND valid_from   <= @iv_valid_date
        AND valid_to     >= @iv_valid_date
      INTO @rv_gail_material.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ygms_cx_cst_error
        EXPORTING
          textid    = ygms_cx_cst_error=>material_mapping_not_found
          mv_param1 = |{ iv_ongc_material }|
          mv_param2 = |{ iv_location_id }|.
    ENDIF.
  ENDMETHOD.


  METHOD save_receipt_data.
    " Prepare data with audit fields
    DATA: lt_db_data TYPE STANDARD TABLE OF ygms_cst_b2b_1.

    LOOP AT it_data INTO DATA(ls_data).
      APPEND VALUE #(
        mandt         = sy-mandt
        gas_day       = ls_data-gas_day
        ctp_id        = ls_data-ctp_id
        ongc_material = ls_data-ongc_material
        location_id   = ls_data-location_id
        material      = ls_data-material
        qty_scm       = ls_data-qty_scm
        qty_mbg       = ls_data-qty_mbg
        ongc_id       = ls_data-ongc_id
        created_by    = sy-uname
        created_on    = sy-datum
        created_at    = sy-uzeit
      ) TO lt_db_data.
    ENDLOOP.

    " Insert data
    INSERT ygms_cst_b2b_1 FROM TABLE @lt_db_data.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ygms_cx_cst_error
        EXPORTING
          textid    = ygms_cx_cst_error=>save_failed
          mv_param1 = 'YGMS_CST_B2B_1'.
    ENDIF.

    " Log audit entries
    IF mo_audit_handler IS BOUND.
      LOOP AT lt_db_data INTO DATA(ls_db_data).
        mo_audit_handler->log_insert(
          iv_table_name = 'YGMS_CST_B2B_1'
          is_new_data   = ls_db_data
          iv_key1       = |{ ls_db_data-gas_day }|
          iv_key2       = |{ ls_db_data-ctp_id }|
          iv_key3       = |{ ls_db_data-ongc_material }|
        ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD save_purchase_data.
    " Prepare data with audit fields
    DATA: lt_db_data TYPE STANDARD TABLE OF ygms_cst_pur.

    LOOP AT it_data INTO DATA(ls_data).
      DATA(ls_db) = CORRESPONDING ygms_cst_pur( ls_data ).
      ls_db-mandt = sy-mandt.
      IF ls_db-created_by IS INITIAL.
        ls_db-created_by = sy-uname.
        ls_db-created_on = sy-datum.
        ls_db-created_at = sy-uzeit.
      ENDIF.
      APPEND ls_db TO lt_db_data.
    ENDLOOP.

    " Get GAIL ID from first record
    READ TABLE lt_db_data INTO DATA(ls_first) INDEX 1.
    IF sy-subrc = 0.
      ev_gail_id = ls_first-gail_id.
    ENDIF.

    " Insert data
    INSERT ygms_cst_pur FROM TABLE @lt_db_data.

    IF sy-subrc <> 0.
      " Try modify in case records already exist
      MODIFY ygms_cst_pur FROM TABLE @lt_db_data.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE ygms_cx_cst_error
          EXPORTING
            textid    = ygms_cx_cst_error=>save_failed
            mv_param1 = 'YGMS_CST_PUR'.
      ENDIF.
    ENDIF.

    " Log audit entries
    IF mo_audit_handler IS BOUND.
      LOOP AT lt_db_data INTO DATA(ls_db_data).
        mo_audit_handler->log_insert(
          iv_table_name = 'YGMS_CST_PUR'
          is_new_data   = ls_db_data
          iv_key1       = |{ ls_db_data-gas_day }|
          iv_key2       = |{ ls_db_data-location_id }|
          iv_key3       = |{ ls_db_data-state_code }|
        ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_receipt_data.
    SELECT gas_day, ctp_id, ongc_material, location_id, material,
           qty_scm, qty_mbg, ongc_id,
           created_by, created_on, created_at,
           changed_by, changed_on, changed_at
      FROM ygms_cst_b2b_1
      WHERE gas_day BETWEEN @iv_from_date AND @iv_to_date
        AND ( @iv_location_id IS INITIAL OR location_id = @iv_location_id )
      INTO TABLE @rt_data.
  ENDMETHOD.


  METHOD get_purchase_data.
    SELECT *
      FROM ygms_cst_pur
      WHERE gas_day BETWEEN @iv_from_date AND @iv_to_date
        AND ( @iv_location_id IS INITIAL OR location_id = @iv_location_id )
      INTO TABLE @rt_data.
  ENDMETHOD.


  METHOD get_sales_data.
    " Get sales data from SD tables
    " This is a placeholder - actual implementation depends on client's SD configuration
    " Typically reads from VBRK/VBRP or custom sales tables

    " Simulated sales data retrieval
    " In production, this would call a function module or read from actual sales tables

    " Example structure:
    " SELECT billing_date AS gas_day,
    "        location AS location_id,
    "        material,
    "        ship_to_state AS state_code,
    "        state_name AS state,
    "        quantity AS sales_qty,
    "        customer
    "   FROM sales_data_view
    "   WHERE billing_date BETWEEN @iv_from_date AND @iv_to_date
    "     AND ( @iv_location_id IS INITIAL OR location = @iv_location_id )
    "   INTO TABLE @rt_data.

    " For now, return empty table - actual implementation needed
    rt_data = VALUE #( ).
  ENDMETHOD.


  METHOD get_gcv_ncv.
    " Get GCV/NCV from existing table (YRXA_CMDATA or similar)
    " This is a placeholder - actual table name needs confirmation from client

    " SELECT SINGLE gcv, ncv
    "   FROM yrxa_cmdata
    "   WHERE gas_day     = @iv_gas_day
    "     AND location_id = @iv_location_id
    "   INTO (@ev_gcv, @ev_ncv).

    " For now, use default values if not found
    ev_gcv = '9500.000'.  " Default GCV
    ev_ncv = '8550.000'.  " Default NCV (90% of GCV)

    " Uncomment below when actual table is available
    " IF sy-subrc <> 0.
    "   RAISE EXCEPTION TYPE ygms_cx_cst_error
    "     EXPORTING
    "       textid    = ygms_cx_cst_error=>gcv_ncv_missing
    "       mv_param1 = |{ iv_gas_day DATE = USER }|
    "       mv_param2 = |{ iv_location_id }|.
    " ENDIF.
  ENDMETHOD.


  METHOD mark_as_sent.
    " Update sent flag for records
    UPDATE ygms_cst_pur
      SET sent       = @ygms_if_cst_constants=>co_flag_true
          changed_by = @sy-uname
          changed_on = @sy-datum
          changed_at = @sy-uzeit
      WHERE gas_day BETWEEN @iv_from_date AND @iv_to_date
        AND location_id = @iv_location_id.
  ENDMETHOD.


  METHOD check_data_exists.
    SELECT SINGLE @abap_true
      FROM ygms_cst_pur
      WHERE gas_day BETWEEN @iv_from_date AND @iv_to_date
        AND location_id = @iv_location_id
      INTO @rv_exists.
  ENDMETHOD.


  METHOD delete_existing_data.
    " Get existing data for audit logging
    IF mo_audit_handler IS BOUND.
      SELECT *
        FROM ygms_cst_pur
        WHERE gas_day BETWEEN @iv_from_date AND @iv_to_date
          AND location_id = @iv_location_id
        INTO TABLE @DATA(lt_old_data).

      LOOP AT lt_old_data INTO DATA(ls_old).
        mo_audit_handler->log_delete(
          iv_table_name = 'YGMS_CST_PUR'
          is_old_data   = ls_old
          iv_key1       = |{ ls_old-gas_day }|
          iv_key2       = |{ ls_old-location_id }|
          iv_key3       = |{ ls_old-state_code }|
        ).
      ENDLOOP.
    ENDIF.

    " Delete existing data
    DELETE FROM ygms_cst_pur
      WHERE gas_day BETWEEN @iv_from_date AND @iv_to_date
        AND location_id = @iv_location_id.
  ENDMETHOD.

ENDCLASS.
