CLASS ygms_cl_cst_allocator DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Allocate B2B data to states</p>
    METHODS allocate
      IMPORTING
        it_b2b_data        TYPE ygms_tt_b2b
        it_mat_map         TYPE STANDARD TABLE
        it_excluded_states TYPE ygms_tt_state_excl OPTIONAL
      RETURNING
        VALUE(rt_allocation) TYPE ygms_tt_allocation.

ENDCLASS.


CLASS ygms_cl_cst_allocator IMPLEMENTATION.

  METHOD allocate.
    DATA: ls_allocation TYPE ygms_s_allocation.

    LOOP AT it_b2b_data INTO DATA(ls_b2b).
      " Find matching material mappings
      LOOP AT it_mat_map INTO DATA(ls_mat)
           WHERE ('LOCATION_ID = ls_b2b-location_id AND MATERIAL = ls_b2b-material').

        CLEAR ls_allocation.
        ls_allocation-gas_day       = ls_b2b-gas_day.
        ls_allocation-location_id   = ls_b2b-location_id.
        ls_allocation-material      = ls_b2b-material.

        " Get state info from mapping
        ASSIGN COMPONENT 'STATE' OF STRUCTURE ls_mat TO FIELD-SYMBOL(<lv_state>).
        IF sy-subrc = 0.
          ls_allocation-state = <lv_state>.
        ENDIF.

        ASSIGN COMPONENT 'STATE_CODE' OF STRUCTURE ls_mat TO FIELD-SYMBOL(<lv_state_code>).
        IF sy-subrc = 0.
          ls_allocation-state_code = <lv_state_code>.
        ENDIF.

        ASSIGN COMPONENT 'TAX_TYPE' OF STRUCTURE ls_mat TO FIELD-SYMBOL(<lv_tax_type>).
        IF sy-subrc = 0.
          ls_allocation-tax_type = <lv_tax_type>.
        ENDIF.

        " Set supply quantities
        ls_allocation-supply_qty_mbg = ls_b2b-qty_mbg.
        ls_allocation-supply_qty_scm = ls_b2b-qty_scm.
        ls_allocation-gcv = ls_b2b-gcv.
        ls_allocation-ncv = ls_b2b-ncv.

        " Calculate allocation (equal distribution for now)
        ls_allocation-alloc_pct = 100.
        ls_allocation-alloc_qty_mbg = ls_b2b-qty_mbg.
        ls_allocation-alloc_qty_scm = ls_b2b-qty_scm.

        " Check if state is excluded
        READ TABLE it_excluded_states TRANSPORTING NO FIELDS
             WITH KEY table_line = ls_allocation-state_code.
        IF sy-subrc = 0.
          ls_allocation-excluded = abap_true.
        ENDIF.

        APPEND ls_allocation TO rt_allocation.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
