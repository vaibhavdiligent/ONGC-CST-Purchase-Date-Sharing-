*&---------------------------------------------------------------------*
*& Include          ZBCM_CLASS_DECLARE
*&---------------------------------------------------------------------*


CLASS lcl_file_verifier IMPLEMENTATION.

  METHOD get_instance.
    IF lcl_file_verifier=>go_object IS INITIAL.
      CREATE OBJECT lcl_file_verifier=>go_object.
    ENDIF.

    ro_obj = lcl_file_verifier=>go_object.
  ENDMETHOD.                    "get_instance

*  METHOD get_file_info.
*
*    TYPES : BEGIN OF ty_file,
*              content TYPE sdok_sdatx,
*            END OF ty_file.
*
*    DATA: lv_filename_save TYPE         string,
*          lv_filename      TYPE         string,
*          lv_temp          TYPE         string,
*          lv_temp1         TYPE         string,
*          lv_extn          TYPE         string,
*          lv_invalid_file  TYPE         xfeld,
*          lv_length        TYPE         i,
*          lt_data_tab      TYPE TABLE OF ty_file,
*          lv_signature     TYPE         xstring.
*
*    CLEAR lv_invalid_file.
*
*    lv_filename = iv_filename.
*    TRANSLATE lv_filename TO LOWER CASE.
*    MOVE lv_filename TO lv_temp1.
*
*    DO.
*      SPLIT lv_temp1 AT '.'
*            INTO lv_temp
*                 lv_temp1.
*
*      IF lv_temp1 IS INITIAL.
*        "invalid file.
*        lv_invalid_file = abap_true.
*        EXIT.
*      ELSEIF lv_temp1 EQ 'sig'.
*        lv_extn = lv_temp.
*        EXIT.
*      ENDIF.
*    ENDDO.
*
*    IF lv_invalid_file EQ abap_true.
** Message: Invalid file type, only signed files can be verified
*      MESSAGE text-017 TYPE 'E'.
*      RETURN.
*    ELSE.
*      " get the original filename inorder to save the data later.
*      SPLIT lv_filename AT '.sig'
*            INTO lv_filename_save lv_temp.
*
*      SPLIT lv_filename_save AT '.' INTO lv_file_down lv_dummy .
*      CONCATENATE lv_file_down '_verified.' lv_extn INTO lv_file_down.
**      MOVE lv_filename_save TO me->mv_filename_save.
*      MOVE lv_file_down TO me->mv_filename_save.
*
*      TRANSLATE lv_extn TO UPPER CASE.
*
*      CASE lv_extn.
*        WHEN 'PDF'.
*          me->mv_file_type = 'BIN'.
*        WHEN 'TXT'.
*          me->mv_file_type = 'BIN'."'ASC'.
*        WHEN OTHERS.
*          me->mv_file_type = 'BIN'.
*      ENDCASE.
*
*** Incase of signed file the GUI_UPLOAD file type would always be BIN.
*      CALL FUNCTION 'GUI_UPLOAD'
*        EXPORTING
*          filename                = lv_filename
*          filetype                = me->mv_file_type "'BIN'
*        IMPORTING
*          filelength              = lv_length
*        TABLES
*          data_tab                = lt_data_tab
*        EXCEPTIONS
*          file_open_error         = 1
*          file_read_error         = 2
*          no_batch                = 3
*          gui_refuse_filetransfer = 4
*          invalid_type            = 5
*          no_authority            = 6
*          unknown_error           = 7
*          bad_data_format         = 8
*          header_not_allowed      = 9
*          separator_not_allowed   = 10
*          header_too_long         = 11
*          unknown_dp_error        = 12
*          access_denied           = 13
*          dp_out_of_memory        = 14
*          disk_full               = 15
*          dp_timeout              = 16
*          OTHERS                  = 17.
*
*      IF sy-subrc <> 0.
** Message: Error while uploading the file, please contact system administrator
*        MESSAGE text-014 TYPE 'E'.
*      ELSE.
*        " convert data into xstring.
*        IF me->mv_file_type EQ 'BIN'.
*          CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
*            EXPORTING
*              input_length = lv_length
*            IMPORTING
*              buffer       = lv_signature
*            TABLES
*              binary_tab   = lt_data_tab
*            EXCEPTIONS
*              failed       = 1
*              OTHERS       = 2.
*        ELSE.
*          CALL FUNCTION 'SCMS_TEXT_TO_XSTRING'
*            IMPORTING
*              buffer   = lv_signature
*            TABLES
*              text_tab = lt_data_tab
*            EXCEPTIONS
*              failed   = 1
*              OTHERS   = 2.
*        ENDIF.
*
*
*        IF sy-subrc <> 0.
** Message: Data conversion error, please contact system administrator
*          MESSAGE text-015 TYPE 'E'.
*        ELSE.
*          ev_signature = lv_signature.
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*
*  ENDMETHOD.                    "get_file_info

  METHOD verify_signature.

    DATA: lv_crc             TYPE i,
          lv_bindocument_out TYPE xstring,
          lt_signerlist      TYPE ssfsignertab,
          ls_signerlist      TYPE ssfsigner.

    CALL FUNCTION 'SSFS_SERVER_VERIFY'
      EXPORTING
        signature       = iv_signature
      IMPORTING
        crc             = lv_crc
        signerlist      = lt_signerlist
        bindocument_out = lv_bindocument_out
      EXCEPTIONS
        kernel_error    = 1
        parameter_error = 2
        conversion_error = 3
        OTHERS          = 4.

    IF sy-subrc <> 0.
* Message: Verification failed.
      MESSAGE text-016 TYPE 'E'.
    ELSE.
      IF lv_crc <> 0 AND lv_crc <> 5.
* Message: Verification failed.
        MESSAGE text-016 TYPE 'E'.
      ELSE.
* Message: Verification Successful.
        MESSAGE text-004 TYPE 'S'.
        " get signer info.
        READ TABLE lt_signerlist INTO ls_signerlist INDEX 1.

        CALL METHOD get_signer_info
          EXPORTING
            is_signerlist = ls_signerlist
          IMPORTING
            ev_owner      = ev_owner
            ev_email      = ev_email
            ev_serial     = ev_serial
            ev_thumbprint = ev_thumbprint
            ev_validfrom  = ev_validfrom
            ev_validto    = ev_validto
            ev_issuer     = ev_issuer.

        ev_bindocument_out = lv_bindocument_out.

      ENDIF.
    ENDIF.

  ENDMETHOD.                    "verify_signature

  METHOD get_signer_info.

    DATA: lo_pdo_dig_sig TYPE REF TO cl_abap_x509_certificate,
          lv_text        TYPE c LENGTH 200,
          ls_email       TYPE sx_address.

    lo_pdo_dig_sig = cl_abap_x509_certificate=>get_instance( is_signerlist-cert ).

    TRY.
        CALL METHOD lo_pdo_dig_sig->get_subject_issuer
          IMPORTING
            ef_subject = ev_owner
            ef_issuer  = ev_issuer.
      CATCH cx_abap_x509_certificate .
    ENDTRY.

    TRY.
        CALL METHOD lo_pdo_dig_sig->get_serial_hex
          RECEIVING
            rf_value = ev_serial.
      CATCH cx_abap_x509_certificate .
    ENDTRY.

    TRY.
        CALL METHOD lo_pdo_dig_sig->get_pk_finger_print
          RECEIVING
            rf_value = ev_thumbprint.
      CATCH cx_abap_x509_certificate .
    ENDTRY.

    TRY.
        CALL METHOD lo_pdo_dig_sig->get_valid_from
          IMPORTING
            ef_gmt_string = ev_validfrom.
      CATCH cx_abap_x509_certificate .
    ENDTRY.

    TRY.
        CALL METHOD lo_pdo_dig_sig->get_valid_to
          IMPORTING
            ef_gmt_string = ev_validto.
      CATCH cx_abap_x509_certificate .
    ENDTRY.


    try.

*    TRY.
        CALL METHOD lo_pdo_dig_sig->get_version
          RECEIVING
            rf_int = e_int
            .
*      CATCH cx_abap_x509_certificate .
    ENDTRY.


*CALL METHOD lo_pdo_dig_sig->GET_VERSION
*returning
*  rf_int = e_int.
*endtry.

    TRY.
        CALL METHOD lo_pdo_dig_sig->get_subject_alt_string
          RECEIVING
            rf_string = ev_email.

        IF ev_email IS NOT INITIAL.
          CLEAR lv_text.
          SPLIT ev_email AT '=' INTO ev_email lv_text.

          ls_email-type    = 'INT'.
          ls_email-address = lv_text.

          CALL FUNCTION 'SX_INTERNET_ADDRESS_TO_NORMAL'
            EXPORTING
              address_unstruct   = ls_email
            EXCEPTIONS
              error_address_type = 1
              error_address      = 2
              error_group_address = 3
              OTHERS             = 4.
          IF sy-subrc = 0.
            ev_email = lv_text.
          ELSE.
            CLEAR ev_email.
          ENDIF.
        ENDIF.

      CATCH cx_abap_x509_certificate .
    ENDTRY.


  ENDMETHOD.                    "get_signer_info


  METHOD get_filename.

    ev_filename_save = mv_filename_save.
    ev_file_type     = mv_file_type.

  ENDMETHOD.                    "get_filename

ENDCLASS.                    "lcl_utility IMPLEMENTATION
