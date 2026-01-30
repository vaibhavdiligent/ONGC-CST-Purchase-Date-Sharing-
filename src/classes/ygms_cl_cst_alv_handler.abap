*&---------------------------------------------------------------------*
*& Class: YGMS_CL_CST_ALV_HANDLER
*& Package: YGMS
*& Description: ALV Grid Display and Event Handling
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

CLASS ygms_cl_cst_alv_handler DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    METHODS constructor.

    "! <p class="shorttext synchronized" lang="en">Display allocation data in ALV</p>
    "! @parameter it_data | Allocation data to display
    METHODS display_allocation
      IMPORTING
        it_data TYPE ygms_tt_allocation.

    "! <p class="shorttext synchronized" lang="en">Display purchase data in ALV</p>
    "! @parameter it_data | Purchase data to display
    METHODS display_purchase
      IMPORTING
        it_data TYPE ygms_tt_purchase.

    "! <p class="shorttext synchronized" lang="en">Display validation results in ALV</p>
    "! @parameter it_data | Validation results to display
    METHODS display_validation
      IMPORTING
        it_data TYPE ygms_tt_validation.

    "! <p class="shorttext synchronized" lang="en">Get modified data from ALV</p>
    "! @parameter rt_data | Modified allocation data
    METHODS get_modified_data
      RETURNING
        VALUE(rt_data) TYPE ygms_tt_allocation.

    "! <p class="shorttext synchronized" lang="en">Handle double click event</p>
    "! @parameter iv_row | Row number clicked
    "! @parameter iv_column | Column name clicked
    METHODS on_double_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.

    "! <p class="shorttext synchronized" lang="en">Handle user command</p>
    "! @parameter iv_function | Function code
    METHODS on_user_command
      FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.

  PRIVATE SECTION.
    DATA: mo_alv        TYPE REF TO cl_salv_table,
          mt_allocation TYPE ygms_tt_allocation,
          mt_purchase   TYPE ygms_tt_purchase,
          mt_validation TYPE ygms_tt_validation,
          mv_edit_mode  TYPE abap_bool.

    CONSTANTS:
      co_fc_save     TYPE salv_de_function VALUE 'SAVE',
      co_fc_validate TYPE salv_de_function VALUE 'VALIDATE',
      co_fc_exclude  TYPE salv_de_function VALUE 'EXCLUDE',
      co_fc_include  TYPE salv_de_function VALUE 'INCLUDE',
      co_fc_refresh  TYPE salv_de_function VALUE 'REFRESH'.

    "! <p class="shorttext synchronized" lang="en">Set up ALV columns</p>
    METHODS setup_columns
      IMPORTING
        io_columns TYPE REF TO cl_salv_columns_table.

    "! <p class="shorttext synchronized" lang="en">Set up ALV functions</p>
    METHODS setup_functions
      IMPORTING
        io_functions TYPE REF TO cl_salv_functions_list.

    "! <p class="shorttext synchronized" lang="en">Set up event handlers</p>
    METHODS setup_events.

    "! <p class="shorttext synchronized" lang="en">Add custom functions to toolbar</p>
    METHODS add_custom_functions.

ENDCLASS.


CLASS ygms_cl_cst_alv_handler IMPLEMENTATION.

  METHOD constructor.
    mv_edit_mode = abap_false.
  ENDMETHOD.


  METHOD display_allocation.
    " Store data for modification
    mt_allocation = it_data.

    TRY.
        " Create ALV
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_alv
          CHANGING
            t_table      = mt_allocation
        ).

        " Set up display settings
        DATA(lo_display) = mo_alv->get_display_settings( ).
        lo_display->set_striped_pattern( abap_true ).
        lo_display->set_list_header( 'ONGC CST Purchase - State Allocation' ).

        " Set up columns
        DATA(lo_columns) = mo_alv->get_columns( ).
        setup_columns( lo_columns ).

        " Set up functions
        DATA(lo_functions) = mo_alv->get_functions( ).
        setup_functions( lo_functions ).

        " Add custom functions
        add_custom_functions( ).

        " Set up events
        setup_events( ).

        " Enable editing for certain columns
        lo_columns->set_optimize( abap_true ).

        " Display ALV
        mo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv).
        MESSAGE lx_salv TYPE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD display_purchase.
    mt_purchase = it_data.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_alv
          CHANGING
            t_table      = mt_purchase
        ).

        DATA(lo_display) = mo_alv->get_display_settings( ).
        lo_display->set_striped_pattern( abap_true ).
        lo_display->set_list_header( 'ONGC CST Purchase Data' ).

        DATA(lo_columns) = mo_alv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        " Hide technical columns
        TRY.
            lo_columns->get_column( 'MANDT' )->set_visible( abap_false ).
            lo_columns->get_column( 'CREATED_BY' )->set_visible( abap_false ).
            lo_columns->get_column( 'CREATED_ON' )->set_visible( abap_false ).
            lo_columns->get_column( 'CREATED_AT' )->set_visible( abap_false ).
            lo_columns->get_column( 'CHANGED_BY' )->set_visible( abap_false ).
            lo_columns->get_column( 'CHANGED_ON' )->set_visible( abap_false ).
            lo_columns->get_column( 'CHANGED_AT' )->set_visible( abap_false ).
          CATCH cx_salv_not_found.
            " Column not found - ignore
        ENDTRY.

        mo_alv->get_functions( )->set_all( abap_true ).

        mo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv).
        MESSAGE lx_salv TYPE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD display_validation.
    mt_validation = it_data.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = mo_alv
          CHANGING
            t_table      = mt_validation
        ).

        DATA(lo_display) = mo_alv->get_display_settings( ).
        lo_display->set_striped_pattern( abap_true ).
        lo_display->set_list_header( 'Validation Results' ).

        DATA(lo_columns) = mo_alv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        " Set column headers
        TRY.
            CAST cl_salv_column_table( lo_columns->get_column( 'STATUS' ) )->set_short_text( 'Status' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'DIFF_SCM' ) )->set_short_text( 'Diff SCM' ).
            CAST cl_salv_column_table( lo_columns->get_column( 'DIFF_MBG' ) )->set_short_text( 'Diff MBG' ).
          CATCH cx_salv_not_found.
            " Column not found - ignore
        ENDTRY.

        mo_alv->get_functions( )->set_all( abap_true ).

        mo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv).
        MESSAGE lx_salv TYPE 'E'.
    ENDTRY.
  ENDMETHOD.


  METHOD get_modified_data.
    rt_data = mt_allocation.
  ENDMETHOD.


  METHOD on_double_click.
    " Handle double click on specific rows/columns
    " Can be used to drill down or edit specific cells

    READ TABLE mt_allocation INTO DATA(ls_data) INDEX row.
    IF sy-subrc = 0.
      " Display detail or allow editing
      MESSAGE |Selected: { ls_data-state } - { ls_data-alloc_qty_mbg } MMBTU| TYPE 'I'.
    ENDIF.
  ENDMETHOD.


  METHOD on_user_command.
    CASE e_salv_function.
      WHEN co_fc_save.
        " Save data
        MESSAGE 'Save functionality - to be implemented by controller' TYPE 'I'.

      WHEN co_fc_validate.
        " Validate allocations
        MESSAGE 'Validate functionality - to be implemented by controller' TYPE 'I'.

      WHEN co_fc_exclude.
        " Exclude selected states
        MESSAGE 'Exclude functionality - to be implemented by controller' TYPE 'I'.

      WHEN co_fc_include.
        " Include previously excluded states
        MESSAGE 'Include functionality - to be implemented by controller' TYPE 'I'.

      WHEN co_fc_refresh.
        " Refresh data
        mo_alv->refresh( ).

    ENDCASE.
  ENDMETHOD.


  METHOD setup_columns.
    TRY.
        " Set column texts
        io_columns->get_column( 'GAS_DAY' )->set_short_text( 'Gas Day' ).
        io_columns->get_column( 'LOCATION_ID' )->set_short_text( 'Location' ).
        io_columns->get_column( 'MATERIAL' )->set_short_text( 'Material' ).
        io_columns->get_column( 'STATE_CODE' )->set_short_text( 'State Cd' ).
        io_columns->get_column( 'STATE' )->set_short_text( 'State' ).
        io_columns->get_column( 'SALES_QTY' )->set_short_text( 'Sales Qty' ).
        io_columns->get_column( 'ALLOC_QTY_MBG' )->set_short_text( 'Alloc MBG' ).
        io_columns->get_column( 'ALLOC_QTY_SCM' )->set_short_text( 'Alloc SCM' ).
        io_columns->get_column( 'GCV' )->set_short_text( 'GCV' ).
        io_columns->get_column( 'NCV' )->set_short_text( 'NCV' ).
        io_columns->get_column( 'TAX_TYPE' )->set_short_text( 'Tax Type' ).
        io_columns->get_column( 'EXCLUDED' )->set_short_text( 'Excluded' ).

      CATCH cx_salv_not_found.
        " Column not found - ignore
    ENDTRY.
  ENDMETHOD.


  METHOD setup_functions.
    " Enable standard functions
    io_functions->set_all( abap_true ).
  ENDMETHOD.


  METHOD setup_events.
    " Get events object
    DATA(lo_events) = mo_alv->get_event( ).

    " Register event handlers
    SET HANDLER on_double_click FOR lo_events.
    SET HANDLER on_user_command FOR lo_events.
  ENDMETHOD.


  METHOD add_custom_functions.
    TRY.
        " Add custom functions to toolbar
        mo_alv->get_functions( )->add_function(
          name     = co_fc_save
          icon     = '@2L@'  " Save icon
          text     = 'Save'
          tooltip  = 'Save allocation data'
          position = if_salv_c_function_position=>right_of_salv_functions
        ).

        mo_alv->get_functions( )->add_function(
          name     = co_fc_validate
          icon     = '@0Y@'  " Check icon
          text     = 'Validate'
          tooltip  = 'Validate allocations'
          position = if_salv_c_function_position=>right_of_salv_functions
        ).

        mo_alv->get_functions( )->add_function(
          name     = co_fc_exclude
          icon     = '@3D@'  " Exclude icon
          text     = 'Exclude'
          tooltip  = 'Exclude selected states'
          position = if_salv_c_function_position=>right_of_salv_functions
        ).

        mo_alv->get_functions( )->add_function(
          name     = co_fc_include
          icon     = '@3C@'  " Include icon
          text     = 'Include'
          tooltip  = 'Include selected states'
          position = if_salv_c_function_position=>right_of_salv_functions
        ).

      CATCH cx_salv_existing cx_salv_wrong_call.
        " Function already exists or wrong call - ignore
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
