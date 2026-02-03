CLASS ygms_cl_cst_data_handler DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Get B2B receipt data</p>
    METHODS get_b2b_data
      IMPORTING
        iv_location_id   TYPE ygms_de_loc_id
        iv_date_from     TYPE datum
        iv_date_to       TYPE datum
      RETURNING
        VALUE(rt_data)   TYPE ygms_tt_b2b.

    "! <p class="shorttext synchronized" lang="en">Get material mapping</p>
    METHODS get_material_mapping
      IMPORTING
        iv_location_id   TYPE ygms_de_loc_id
      RETURNING
        VALUE(rt_mapping) TYPE STANDARD TABLE OF yrga_cst_mat_map WITH DEFAULT KEY.

    "! <p class="shorttext synchronized" lang="en">Save purchase data</p>
    METHODS save_purchase_data
      IMPORTING
        it_data TYPE ygms_tt_purchase.

    "! <p class="shorttext synchronized" lang="en">Get location mapping</p>
    METHODS get_location_mapping
      IMPORTING
        iv_location_id     TYPE ygms_de_loc_id
      RETURNING
        VALUE(rs_location) TYPE yrga_cst_loc_map.

ENDCLASS.


CLASS ygms_cl_cst_data_handler IMPLEMENTATION.

  METHOD get_b2b_data.
    " Get B2B data by joining with location mapping to find CTP_ID
    SELECT b~* FROM yrga_cst_b2b_1 AS b
      INNER JOIN yrga_cst_loc_map AS l
        ON b~ctp_id = l~ongc_ctp_id
      WHERE l~gail_loc_id = @iv_location_id
        AND b~gas_day BETWEEN @iv_date_from AND @iv_date_to
        AND l~deleted = @abap_false
      INTO TABLE @rt_data.
  ENDMETHOD.


  METHOD get_material_mapping.
    SELECT * FROM yrga_cst_mat_map
      WHERE location_id = @iv_location_id
        AND deleted = @abap_false
      INTO TABLE @rt_mapping.
  ENDMETHOD.


  METHOD save_purchase_data.
    IF it_data IS NOT INITIAL.
      MODIFY ygms_cst_pur FROM TABLE it_data.
      IF sy-subrc = 0.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_location_mapping.
    SELECT SINGLE * FROM yrga_cst_loc_map
      WHERE gail_loc_id = @iv_location_id
        AND deleted = @abap_false
      INTO @rs_location.
  ENDMETHOD.

ENDCLASS.
