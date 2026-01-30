*&---------------------------------------------------------------------*
*& Class: YGMS_CL_CST_CONTROLLER
*& Package: YGMS
*& Description: Main Controller Class - Orchestrates all processing
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

CLASS ygms_cl_cst_controller DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES ygms_if_cst_processor.

    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    "! @parameter iv_location_id | GAIL Location ID
    "! @parameter iv_from_date | Start date of processing period
    "! @parameter iv_to_date | End date of processing period
    METHODS constructor
      IMPORTING
        iv_location_id TYPE ygms_loc_id OPTIONAL
        iv_from_date   TYPE datum OPTIONAL
        iv_to_date     TYPE datum OPTIONAL.

    "! <p class="shorttext synchronized" lang="en">Process file upload</p>
    "! @parameter iv_file_path | Path to Excel file
    "! @parameter et_messages | Return messages
    "! @raising ygms_cx_cst_error | Upload error occurred
    METHODS process_upload
      IMPORTING
        iv_file_path TYPE string
      EXPORTING
        et_messages  TYPE bapiret2_t
      RAISING
        ygms_cx_cst_error.

    "! <p class="shorttext synchronized" lang="en">Execute state-wise allocation</p>
    "! @parameter it_excluded_states | Range of states to exclude
    "! @parameter et_allocation | Allocation results
    "! @parameter et_messages | Return messages
    "! @raising ygms_cx_cst_error | Allocation error occurred
    METHODS execute_allocation
      IMPORTING
        it_excluded_states TYPE ygms_rt_state_code OPTIONAL
      EXPORTING
        et_allocation      TYPE ygms_tt_allocation
        et_messages        TYPE bapiret2_t
      RAISING
        ygms_cx_cst_error.

    "! <p class="shorttext synchronized" lang="en">Save allocation data</p>
    "! @parameter it_data | Allocation data to save
    "! @parameter ev_gail_id | Generated GAIL Transaction ID
    "! @parameter et_messages | Return messages
    "! @raising ygms_cx_cst_error | Save error occurred
    METHODS save_data
      IMPORTING
        it_data     TYPE ygms_tt_allocation
      EXPORTING
        ev_gail_id  TYPE ygms_gail_id
        et_messages TYPE bapiret2_t
      RAISING
        ygms_cx_cst_error.

    "! <p class="shorttext synchronized" lang="en">Send data via email</p>
    "! @parameter iv_email_address | Recipient email address
    "! @parameter et_messages | Return messages
    "! @raising ygms_cx_cst_error | Email error occurred
    METHODS send_data
      IMPORTING
        iv_email_address TYPE ad_smtpadr
      EXPORTING
        et_messages      TYPE bapiret2_t
      RAISING
        ygms_cx_cst_error.

    "! <p class="shorttext synchronized" lang="en">Download data to Excel</p>
    "! @parameter iv_file_path | Path to save Excel file
    "! @parameter et_messages | Return messages
    "! @raising ygms_cx_cst_error | Download error occurred
    METHODS download_data
      IMPORTING
        iv_file_path TYPE string
      EXPORTING
        et_messages  TYPE bapiret2_t
      RAISING
        ygms_cx_cst_error.

    "! <p class="shorttext synchronized" lang="en">Get purchase nominations</p>
    "! @parameter rt_nominations | Nomination data
    METHODS get_nominations
      RETURNING
        VALUE(rt_nominations) TYPE ygms_tt_purchase.

  PRIVATE SECTION.
    DATA: mo_data_handler   TYPE REF TO ygms_cl_cst_data_handler,
          mo_allocator      TYPE REF TO ygms_cl_cst_allocator,
          mo_excel_handler  TYPE REF TO ygms_cl_cst_excel_handler,
          mo_email_sender   TYPE REF TO ygms_cl_cst_email_sender,
          mo_validator      TYPE REF TO ygms_cl_cst_validator,
          mo_pdf_generator  TYPE REF TO ygms_cl_cst_pdf_generator,
          mo_audit_handler  TYPE REF TO ygms_cl_cst_audit_handler,
          mv_location_id    TYPE ygms_loc_id,
          mv_from_date      TYPE datum,
          mv_to_date        TYPE datum,
          mt_receipt_data   TYPE ygms_tt_receipt,
          mt_purchase_data  TYPE ygms_tt_purchase.

    "! <p class="shorttext synchronized" lang="en">Check authorization</p>
    METHODS check_authorization
      IMPORTING
        iv_location_id TYPE ygms_loc_id
        iv_state_code  TYPE ygms_state_cd OPTIONAL
        iv_activity    TYPE activ_auth
      RAISING
        ygms_cx_cst_error.

    "! <p class="shorttext synchronized" lang="en">Determine fortnight from date</p>
    METHODS determine_fortnight
      IMPORTING
        iv_date             TYPE datum
      RETURNING
        VALUE(rs_fortnight) TYPE ygms_if_cst_constants=>ty_fortnight.

    "! <p class="shorttext synchronized" lang="en">Generate GAIL Transaction ID</p>
    METHODS generate_gail_id
      RETURNING
        VALUE(rv_gail_id) TYPE ygms_gail_id.

    "! <p class="shorttext synchronized" lang="en">Add message to message table</p>
    METHODS add_message
      IMPORTING
        iv_type       TYPE symsgty
        iv_id         TYPE symsgid
        iv_number     TYPE symsgno
        iv_v1         TYPE symsgv OPTIONAL
        iv_v2         TYPE symsgv OPTIONAL
        iv_v3         TYPE symsgv OPTIONAL
        iv_v4         TYPE symsgv OPTIONAL
      CHANGING
        ct_messages   TYPE bapiret2_t.

ENDCLASS.


CLASS ygms_cl_cst_controller IMPLEMENTATION.

  METHOD constructor.
    mv_location_id = iv_location_id.
    mv_from_date   = iv_from_date.
    mv_to_date     = iv_to_date.

    " Initialize handler objects
    mo_audit_handler  = NEW ygms_cl_cst_audit_handler( ).
    mo_data_handler   = NEW ygms_cl_cst_data_handler( io_audit_handler = mo_audit_handler ).
    mo_allocator      = NEW ygms_cl_cst_allocator( ).
    mo_excel_handler  = NEW ygms_cl_cst_excel_handler( ).
    mo_validator      = NEW ygms_cl_cst_validator( ).
    mo_email_sender   = NEW ygms_cl_cst_email_sender( ).
    mo_pdf_generator  = NEW ygms_cl_cst_pdf_generator( ).
  ENDMETHOD.


  METHOD process_upload.
    " Check authorization
    check_authorization(
      iv_location_id = mv_location_id
      iv_activity    = ygms_if_cst_constants=>co_activity_create
    ).

    " Upload and parse Excel file
    DATA(lt_receipt) = mo_excel_handler->upload_file( iv_file_path ).

    " Validate single fortnight
    mo_excel_handler->validate_single_fortnight( lt_receipt ).

    " Apply mappings
    LOOP AT lt_receipt ASSIGNING FIELD-SYMBOL(<fs_receipt>).
      <fs_receipt>-location_id = mo_data_handler->get_location_mapping(
        iv_ctp_id     = <fs_receipt>-ctp_id
        iv_valid_date = <fs_receipt>-gas_day
      ).

      <fs_receipt>-material = mo_data_handler->get_material_mapping(
        iv_ongc_material = <fs_receipt>-ongc_material
        iv_location_id   = <fs_receipt>-location_id
        iv_valid_date    = <fs_receipt>-gas_day
      ).
    ENDLOOP.

    " Save receipt data
    mo_data_handler->save_receipt_data( lt_receipt ).

    " Store for later use
    mt_receipt_data = lt_receipt.

    " Return success message
    add_message(
      EXPORTING
        iv_type   = 'S'
        iv_id     = ygms_if_cst_constants=>co_msg_class
        iv_number = '007'
        iv_v1     = CONV #( lines( lt_receipt ) )
      CHANGING
        ct_messages = et_messages
    ).
  ENDMETHOD.


  METHOD execute_allocation.
    " Check authorization
    check_authorization(
      iv_location_id = mv_location_id
      iv_activity    = ygms_if_cst_constants=>co_activity_change
    ).

    " Get receipt data if not already loaded
    IF mt_receipt_data IS INITIAL.
      mt_receipt_data = mo_data_handler->get_receipt_data(
        iv_from_date   = mv_from_date
        iv_to_date     = mv_to_date
        iv_location_id = mv_location_id
      ).
    ENDIF.

    " Get sales data for allocation
    DATA(lt_sales_data) = mo_data_handler->get_sales_data(
      iv_from_date   = mv_from_date
      iv_to_date     = mv_to_date
      iv_location_id = mv_location_id
    ).

    " Calculate allocations
    et_allocation = mo_allocator->calculate_allocation(
      it_receipt_data    = mt_receipt_data
      it_sales_data      = lt_sales_data
      it_excluded_states = it_excluded_states
    ).

    " Get GCV/NCV values for each allocation
    LOOP AT et_allocation ASSIGNING FIELD-SYMBOL(<fs_alloc>).
      mo_data_handler->get_gcv_ncv(
        EXPORTING
          iv_gas_day     = <fs_alloc>-gas_day
          iv_location_id = <fs_alloc>-location_id
        IMPORTING
          ev_gcv         = <fs_alloc>-gcv
          ev_ncv         = <fs_alloc>-ncv
      ).
    ENDLOOP.

    " Apply variance adjustment
    mo_allocator->apply_variance_adjustment(
      EXPORTING
        it_receipt_data = mt_receipt_data
      CHANGING
        ct_allocation   = et_allocation
    ).

    " Determine tax type for each allocation
    LOOP AT et_allocation ASSIGNING <fs_alloc>.
      <fs_alloc>-tax_type = mo_allocator->determine_tax_type( <fs_alloc>-state_code ).
    ENDLOOP.

    " Count allocated states
    DATA(lv_state_count) = lines( VALUE ygms_rt_state_code(
      FOR GROUPS <grp> OF <wa> IN et_allocation GROUP BY <wa>-state_code
      ( sign = 'I' option = 'EQ' low = <grp> )
    ) ).

    " Return success message
    add_message(
      EXPORTING
        iv_type   = 'S'
        iv_id     = ygms_if_cst_constants=>co_msg_class
        iv_number = '008'
        iv_v1     = CONV #( lv_state_count )
      CHANGING
        ct_messages = et_messages
    ).
  ENDMETHOD.


  METHOD save_data.
    " Validate allocations
    TRY.
        IF NOT mo_validator->validate_totals( it_data ).
          RAISE EXCEPTION TYPE ygms_cx_cst_validation
            EXPORTING
              textid = ygms_cx_cst_validation=>allocation_mismatch.
        ENDIF.
      CATCH ygms_cx_cst_validation INTO DATA(lx_validation).
        RAISE EXCEPTION TYPE ygms_cx_cst_error
          EXPORTING
            textid    = ygms_cx_cst_error=>validation_failed
            previous  = lx_validation
            mv_param1 = 'Allocation'
            mv_param2 = 'Supply'.
    ENDTRY.

    " Generate GAIL ID
    ev_gail_id = generate_gail_id( ).

    " Convert allocation data to purchase data
    DATA(lt_purchase) = VALUE ygms_tt_purchase( ).

    LOOP AT it_data INTO DATA(ls_alloc).
      APPEND VALUE #(
        gas_day       = ls_alloc-gas_day
        location_id   = ls_alloc-location_id
        material      = ls_alloc-material
        state_code    = ls_alloc-state_code
        state         = ls_alloc-state
        qty_mbg       = ls_alloc-alloc_qty_mbg
        gcv           = ls_alloc-gcv
        ncv           = ls_alloc-ncv
        qty_scm       = ls_alloc-alloc_qty_scm
        gail_id       = ev_gail_id
        tax_type      = ls_alloc-tax_type
        excluded      = ls_alloc-excluded
        created_by    = sy-uname
        created_on    = sy-datum
        created_at    = sy-uzeit
      ) TO lt_purchase.
    ENDLOOP.

    " Save purchase data
    mo_data_handler->save_purchase_data(
      EXPORTING
        it_data    = lt_purchase
      IMPORTING
        ev_gail_id = ev_gail_id
    ).

    " Store for later use
    mt_purchase_data = lt_purchase.

    " Return success message
    add_message(
      EXPORTING
        iv_type   = 'S'
        iv_id     = ygms_if_cst_constants=>co_msg_class
        iv_number = '010'
        iv_v1     = CONV #( ev_gail_id )
      CHANGING
        ct_messages = et_messages
    ).
  ENDMETHOD.


  METHOD send_data.
    " Check authorization
    check_authorization(
      iv_location_id = mv_location_id
      iv_activity    = ygms_if_cst_constants=>co_activity_change
    ).

    " Get data to send if not already loaded
    IF mt_purchase_data IS INITIAL.
      mt_purchase_data = mo_data_handler->get_purchase_data(
        iv_from_date   = mv_from_date
        iv_to_date     = mv_to_date
        iv_location_id = mv_location_id
      ).
    ENDIF.

    " Generate PDF report
    DATA(lv_pdf_content) = mo_pdf_generator->generate_pdf( mt_purchase_data ).

    " Generate Excel attachment
    DATA(lv_excel_content) = mo_excel_handler->generate_excel( mt_purchase_data ).

    " Send email with attachments
    mo_email_sender->send_email(
      EXPORTING
        iv_recipient     = iv_email_address
        iv_subject       = |ONGC CST Purchase Data - { mv_from_date DATE = USER } to { mv_to_date DATE = USER }|
        iv_body          = |Please find attached the CST Purchase data for the period.|
        iv_pdf_content   = lv_pdf_content
        iv_excel_content = lv_excel_content
    ).

    " Mark data as sent
    mo_data_handler->mark_as_sent(
      iv_from_date   = mv_from_date
      iv_to_date     = mv_to_date
      iv_location_id = mv_location_id
    ).

    " Return success message
    add_message(
      EXPORTING
        iv_type   = 'S'
        iv_id     = ygms_if_cst_constants=>co_msg_class
        iv_number = '013'
        iv_v1     = CONV #( iv_email_address )
      CHANGING
        ct_messages = et_messages
    ).
  ENDMETHOD.


  METHOD download_data.
    " Get data to download if not already loaded
    IF mt_purchase_data IS INITIAL.
      mt_purchase_data = mo_data_handler->get_purchase_data(
        iv_from_date   = mv_from_date
        iv_to_date     = mv_to_date
        iv_location_id = mv_location_id
      ).
    ENDIF.

    " Generate Excel file
    DATA(lv_excel_content) = mo_excel_handler->generate_excel( mt_purchase_data ).

    " Save to file
    mo_excel_handler->save_to_file(
      iv_file_path = iv_file_path
      iv_content   = lv_excel_content
    ).

    " Return success message
    add_message(
      EXPORTING
        iv_type   = 'S'
        iv_id     = ygms_if_cst_constants=>co_msg_class
        iv_number = '007'
        iv_v1     = CONV #( lines( mt_purchase_data ) )
      CHANGING
        ct_messages = et_messages
    ).
  ENDMETHOD.


  METHOD get_nominations.
    " Get nomination data
    rt_nominations = mo_data_handler->get_purchase_data(
      iv_from_date   = mv_from_date
      iv_to_date     = mv_to_date
      iv_location_id = mv_location_id
    ).
  ENDMETHOD.


  METHOD check_authorization.
    " Location authorization check
    AUTHORITY-CHECK OBJECT 'YGMS_LOC'
      ID 'LOCID' FIELD iv_location_id
      ID 'ACTVT' FIELD iv_activity.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE ygms_cx_cst_error
        EXPORTING
          textid    = ygms_cx_cst_error=>auth_failed
          mv_param1 = |Location { iv_location_id }|.
    ENDIF.

    " State authorization check
    IF iv_state_code IS NOT INITIAL.
      AUTHORITY-CHECK OBJECT 'YGMS_STATE'
        ID 'STATE' FIELD iv_state_code
        ID 'ACTVT' FIELD iv_activity.

      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE ygms_cx_cst_error
          EXPORTING
            textid    = ygms_cx_cst_error=>state_auth_failed
            mv_param1 = |State { iv_state_code }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD determine_fortnight.
    DATA(lv_day) = iv_date+6(2).

    IF lv_day <= 15.
      " First fortnight: Day 1-15
      rs_fortnight-from_date = iv_date(6) && '01'.
      rs_fortnight-to_date   = iv_date(6) && '15'.
    ELSE.
      " Second fortnight: Day 16 - End of month
      rs_fortnight-from_date = iv_date(6) && '16'.

      " Calculate end of month
      DATA(lv_next_month) = iv_date.
      lv_next_month+6(2) = '01'.
      lv_next_month = lv_next_month + 32.
      lv_next_month+6(2) = '01'.
      rs_fortnight-to_date = lv_next_month - 1.
    ENDIF.
  ENDMETHOD.


  METHOD generate_gail_id.
    DATA: lv_number TYPE i.

    " Get number from number range
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = ygms_if_cst_constants=>co_nr_object
      IMPORTING
        number      = lv_number.

    " Format GAIL ID: GAIL-YYYYMMDD-NNNNNN
    rv_gail_id = |GAIL-{ sy-datum }-{ lv_number ALIGN = RIGHT PAD = '0' WIDTH = 6 }|.
  ENDMETHOD.


  METHOD add_message.
    APPEND VALUE #(
      type       = iv_type
      id         = iv_id
      number     = iv_number
      message_v1 = iv_v1
      message_v2 = iv_v2
      message_v3 = iv_v3
      message_v4 = iv_v4
    ) TO ct_messages.
  ENDMETHOD.


  METHOD ygms_if_cst_processor~process.
    " Dispatch based on mode
    CASE iv_mode.
      WHEN ygms_if_cst_constants=>co_mode_upload.
        " Upload mode - requires file path from selection screen
        " File path should be passed via separate method call

      WHEN ygms_if_cst_constants=>co_mode_allocate.
        execute_allocation(
          IMPORTING
            et_messages = et_messages
        ).

      WHEN ygms_if_cst_constants=>co_mode_view.
        " View mode - get existing data
        mt_purchase_data = mo_data_handler->get_purchase_data(
          iv_from_date   = mv_from_date
          iv_to_date     = mv_to_date
          iv_location_id = mv_location_id
        ).

      WHEN ygms_if_cst_constants=>co_mode_send.
        " Send mode - handled via send_data method

      WHEN ygms_if_cst_constants=>co_mode_download.
        " Download mode - handled via download_data method

      WHEN ygms_if_cst_constants=>co_mode_nomination.
        " Nomination mode - get purchase nominations
        mt_purchase_data = get_nominations( ).

      WHEN OTHERS.
        " Unknown mode
    ENDCASE.
  ENDMETHOD.


  METHOD ygms_if_cst_processor~validate.
    rv_valid = mo_validator->validate_totals( it_data ).
  ENDMETHOD.


  METHOD ygms_if_cst_processor~get_results.
    rt_results = mt_purchase_data.
  ENDMETHOD.

ENDCLASS.
