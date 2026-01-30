*&---------------------------------------------------------------------*
*& Class: YGMS_CL_CST_EMAIL_SENDER
*& Package: YGMS
*& Description: Email Transmission with Attachments
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

CLASS ygms_cl_cst_email_sender DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    METHODS constructor.

    "! <p class="shorttext synchronized" lang="en">Send email with attachments</p>
    "! @parameter iv_recipient | Recipient email address
    "! @parameter iv_cc | CC email address (optional)
    "! @parameter iv_subject | Email subject
    "! @parameter iv_body | Email body text
    "! @parameter iv_pdf_content | PDF attachment content (optional)
    "! @parameter iv_excel_content | Excel attachment content (optional)
    "! @raising ygms_cx_cst_error | Email send failed
    METHODS send_email
      IMPORTING
        iv_recipient     TYPE ad_smtpadr
        iv_cc            TYPE ad_smtpadr OPTIONAL
        iv_subject       TYPE so_obj_des
        iv_body          TYPE string
        iv_pdf_content   TYPE xstring OPTIONAL
        iv_excel_content TYPE xstring OPTIONAL
      RAISING
        ygms_cx_cst_error.

    "! <p class="shorttext synchronized" lang="en">Attach PDF to email</p>
    "! @parameter io_document | BCS document
    "! @parameter iv_content | PDF content
    "! @parameter iv_filename | PDF filename
    METHODS attach_pdf
      IMPORTING
        io_document TYPE REF TO cl_document_bcs
        iv_content  TYPE xstring
        iv_filename TYPE string.

    "! <p class="shorttext synchronized" lang="en">Attach Excel to email</p>
    "! @parameter io_document | BCS document
    "! @parameter iv_content | Excel content
    "! @parameter iv_filename | Excel filename
    METHODS attach_excel
      IMPORTING
        io_document TYPE REF TO cl_document_bcs
        iv_content  TYPE xstring
        iv_filename TYPE string.

  PRIVATE SECTION.
    "! <p class="shorttext synchronized" lang="en">Convert xstring to solix table</p>
    METHODS xstring_to_solix
      IMPORTING
        iv_xstring       TYPE xstring
      RETURNING
        VALUE(rt_solix) TYPE solix_tab.

ENDCLASS.


CLASS ygms_cl_cst_email_sender IMPLEMENTATION.

  METHOD constructor.
    " No initialization required
  ENDMETHOD.


  METHOD send_email.
    DATA: lo_send_request TYPE REF TO cl_bcs,
          lo_document     TYPE REF TO cl_document_bcs,
          lo_sender       TYPE REF TO cl_sapuser_bcs,
          lo_recipient    TYPE REF TO if_recipient_bcs.

    TRY.
        " Create send request
        lo_send_request = cl_bcs=>create_persistent( ).

        " Create document with body
        DATA(lt_body) = VALUE soli_tab( ).
        DATA(lv_offset) = 0.
        DATA(lv_body_len) = strlen( iv_body ).

        WHILE lv_offset < lv_body_len.
          DATA(lv_line_len) = nmin( val1 = 255 val2 = lv_body_len - lv_offset ).
          APPEND VALUE #( line = iv_body+lv_offset(lv_line_len) ) TO lt_body.
          lv_offset = lv_offset + lv_line_len.
        ENDWHILE.

        lo_document = cl_document_bcs=>create_document(
          i_type    = 'RAW'
          i_text    = lt_body
          i_subject = iv_subject
        ).

        " Attach PDF if provided
        IF iv_pdf_content IS NOT INITIAL.
          attach_pdf(
            io_document = lo_document
            iv_content  = iv_pdf_content
            iv_filename = |ONGC_CST_Purchase_{ sy-datum }.pdf|
          ).
        ENDIF.

        " Attach Excel if provided
        IF iv_excel_content IS NOT INITIAL.
          attach_excel(
            io_document = lo_document
            iv_content  = iv_excel_content
            iv_filename = |ONGC_CST_Purchase_{ sy-datum }.xlsx|
          ).
        ENDIF.

        " Set document
        lo_send_request->set_document( lo_document ).

        " Set sender (current user)
        lo_sender = cl_sapuser_bcs=>create( sy-uname ).
        lo_send_request->set_sender( lo_sender ).

        " Set recipient
        lo_recipient = cl_cam_address_bcs=>create_internet_address( iv_recipient ).
        lo_send_request->add_recipient(
          i_recipient = lo_recipient
          i_express   = abap_true
        ).

        " Set CC if provided
        IF iv_cc IS NOT INITIAL.
          DATA(lo_cc) = cl_cam_address_bcs=>create_internet_address( iv_cc ).
          lo_send_request->add_recipient(
            i_recipient = lo_cc
            i_copy      = abap_true
          ).
        ENDIF.

        " Set send immediately
        lo_send_request->set_send_immediately( abap_true ).

        " Send email
        DATA(lv_sent) = lo_send_request->send( ).

        IF lv_sent = abap_false.
          RAISE EXCEPTION TYPE ygms_cx_cst_error
            EXPORTING
              textid    = ygms_cx_cst_error=>email_failed
              mv_param1 = 'Send request returned false'.
        ENDIF.

        " Commit work
        COMMIT WORK AND WAIT.

      CATCH cx_bcs INTO DATA(lx_bcs).
        RAISE EXCEPTION TYPE ygms_cx_cst_error
          EXPORTING
            textid    = ygms_cx_cst_error=>email_failed
            mv_param1 = lx_bcs->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD attach_pdf.
    DATA: lt_solix TYPE solix_tab.

    " Convert xstring to solix
    lt_solix = xstring_to_solix( iv_content ).

    " Add attachment
    TRY.
        io_document->add_attachment(
          i_attachment_type    = 'PDF'
          i_attachment_subject = CONV #( iv_filename )
          i_att_content_hex    = lt_solix
        ).
      CATCH cx_document_bcs INTO DATA(lx_doc).
        " Log error but continue
        MESSAGE lx_doc TYPE 'W'.
    ENDTRY.
  ENDMETHOD.


  METHOD attach_excel.
    DATA: lt_solix TYPE solix_tab.

    " Convert xstring to solix
    lt_solix = xstring_to_solix( iv_content ).

    " Add attachment
    TRY.
        io_document->add_attachment(
          i_attachment_type    = 'XLS'
          i_attachment_subject = CONV #( iv_filename )
          i_att_content_hex    = lt_solix
        ).
      CATCH cx_document_bcs INTO DATA(lx_doc).
        " Log error but continue
        MESSAGE lx_doc TYPE 'W'.
    ENDTRY.
  ENDMETHOD.


  METHOD xstring_to_solix.
    rt_solix = cl_bcs_convert=>xstring_to_solix( iv_xstring ).
  ENDMETHOD.

ENDCLASS.
