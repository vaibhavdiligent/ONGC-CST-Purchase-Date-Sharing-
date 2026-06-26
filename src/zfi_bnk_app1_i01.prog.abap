*&---------------------------------------------------------------------*
*& Include          ZFI_BNK_APP1_I01
*&---------------------------------------------------------------------*

*&SPWIZARD: INPUT MODULE FOR TS 'TABSTRIP'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tabstrip_active_tab_get INPUT.
*  ok_code = sy-ucomm.
*  CASE ok_code.
*    WHEN c_tabstrip-tab1.
*      g_tabstrip-pressed_tab = c_tabstrip-tab1.
*    WHEN c_tabstrip-tab2.
*      g_tabstrip-pressed_tab = c_tabstrip-tab2.
*    WHEN c_tabstrip-tab3.
*      g_tabstrip-pressed_tab = c_tabstrip-tab3.
*    WHEN OTHERS.
**&SPWIZARD:      DO NOTHING
*  ENDCASE.
ENDMODULE.                    "TABSTRIP_ACTIVE_TAB_GET INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROW_TAB1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_row_tab1 INPUT.
  DATA : ok_code1 TYPE sy-ucomm.
  ok_code1 = sy-ucomm.
  DATA: crc       TYPE i.
  DATA: doc_sig   TYPE xstring.
  DATA: lv_filename TYPE string.

  DATA: sig_list TYPE ssfsignertab.
  DATA: signer   TYPE ssfsigner.
  DATA: doc_ver  TYPE xstring.
  DATA: doc_line TYPE string.
  DATA: doc_tab  TYPE ssftxttab.
  DATA l_sign TYPE ssfsigner.
  DATA it_zuser_signer TYPE TABLE OF zuser_signer.
  DATA wa_zuser_signer TYPE zuser_signer.

  DATA: ev_owner         TYPE string,
        ev_email         TYPE string,
        ev_serial        TYPE string,
        ev_thumbprint    TYPE string,
        ev_validfrom     TYPE string,
        ev_validto       TYPE string,
        ev_issuer        TYPE string,
        ev_bindocument_out TYPE xstring,
        e_int            TYPE i.

  DATA : lv_lines TYPE i.

  DATA: go_file_verifier         TYPE REF TO lcl_file_verifier.

  IF ok_code1 = 'APPROVE1'.

***        Getting the selected rows index
    CALL METHOD c_alvgd1->get_selected_rows
      IMPORTING
        et_index_rows = gt_selected_rows1.

    CLEAR : lv_lines.
    DESCRIBE TABLE gt_selected_rows1 LINES lv_lines.
    IF lv_lines EQ 0.
      MESSAGE 'Please select one record' TYPE 'I'.
      EXIT.
    ELSEIF lv_lines > 1.
      MESSAGE 'Please select only single record' TYPE 'I'.
      EXIT.
    ENDIF.

    IF gt_selected_rows1 IS NOT INITIAL.
      READ TABLE gt_selected_rows1 INTO gs_selected_rows1 INDEX 1.
      IF sy-subrc = 0.
" Code Remediation changes S4 2025_1_A Conversion **BEGIN OF CHANGE BY SAP_ABAP 06.06.2026  FOR ATC
        READ TABLE gt_final INTO gs_final INDEX gs_selected_rows1-index.   "#EC CI_NOORDER
" Code Remediation changes S4 2025_1_A Conversion * *END OF CHANGE BY SAP_ABAP 06.06.2026  FOR ATC
        IF sy-subrc = 0.
          CALL FUNCTION 'SSFS_CALL_CONTROL'
            EXPORTING
*             TXTDOCUMENT     = doc_tab
              bindocument     = gs_final-raw_data "doc_bin
              doc_type        = 'TXT'
*             ssf_id          = 'CN=Kashyap Banaam Bhushan, SP=Delhi, postalCode=110092, OU=CID - 2923089, O=Oil and Natural Gas Corporation Ltd., C=IN'
            IMPORTING
              crc             = crc
              signature       = doc_sig
            EXCEPTIONS
              parameter_error = 1
              conversion_error = 2
              control_error   = 3
              frontend_error  = 4
              OTHERS          = 5.


          IF crc = 0.
            REFRESH sig_list.
            CALL FUNCTION 'SSFS_SERVER_VERIFY'
              EXPORTING
*               SSFAPPLIC       =
                signature       = doc_sig
              IMPORTING
                crc             = crc
                signerlist      = sig_list
                txtdocument_out = doc_tab
                bindocument_out = doc_ver
              EXCEPTIONS
                kernel_error    = 1
                parameter_error = 2
                OTHERS          = 3.
            IF crc = 0.
              go_file_verifier = lcl_file_verifier=>get_instance( ).
              READ TABLE sig_list INTO l_sign INDEX 1.
              IF sy-subrc = 0.
                CALL METHOD go_file_verifier->get_signer_info
                  EXPORTING
                    is_signerlist = l_sign
                  IMPORTING
                    ev_owner      = ev_owner
                    ev_email      = ev_email
                    ev_serial     = ev_serial
                    ev_thumbprint = ev_thumbprint
                    ev_validfrom  = ev_validfrom
                    ev_validto    = ev_validto
                    ev_issuer     = ev_issuer
                    e_int         = e_int.
              ENDIF.
              SELECT * INTO TABLE it_zuser_signer
                FROM zuser_signer WHERE uname = sy-uname
                                  AND   active = 'X'.
              IF sy-subrc = 0.
                READ TABLE it_zuser_signer INTO wa_zuser_signer WITH KEY thumbprin = ev_thumbprint.
                IF sy-subrc <> 0.
                  MESSAGE 'Wrong Signature used for signing' TYPE 'I'.
                  EXIT.
                ENDIF.
              ELSE.
                MESSAGE 'No Signature Maintained for user' TYPE 'I'.
                EXIT.
              ENDIF.
            ENDIF.

            CLEAR gs_paym.
            READ TABLE gt_paym INTO gs_paym WITH KEY laufi = gs_final-laufi
                                                     laufd = gs_final-laufd.
            IF sy-subrc = 0.
              gs_paym-file_data_sent = doc_sig.
              MODIFY zfi_paym_file FROM gs_paym.
            ENDIF.


***** modify record in table ZFI_BATCH_SIGN
            CLEAR : gs_batch_sign.
            READ TABLE gt_batch_sign INTO gs_batch_sign WITH KEY guid = gs_final-guid
                                                                 signer = sy-uname.
            IF sy-subrc = 0.
              gs_batch_sign-guid        = gs_final-guid.
              gs_batch_sign-signer      = sy-uname.
              gs_batch_sign-digitl_sign = 'X'.
              gs_batch_sign-cdate1      = sy-datum.
              gs_batch_sign-ctime1      = sy-uzeit.
              MODIFY zfi_batch_sign FROM gs_batch_sign.
              REFRESH : gt_selected_rows1.
            ENDIF.
          ELSE.
            CASE crc.
              WHEN '2'.
                MESSAGE 'Please insert E-token' TYPE 'I'.
                EXIT.
            ENDCASE.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " GET_SELECTED_ROW_TAB1  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROW_TAB2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_row_tab2 INPUT.
  REFRESH : gt_selected_rows2.
  DATA: ok_code2  TYPE sy-ucomm,
        lv_lines1 TYPE i.

  ok_code2 = sy-ucomm.
  DATA: lv_crc     TYPE i.
  DATA: lv_doc_sig TYPE xstring.

  DATA: lo_bnk_detail    TYPE REF TO zco_sios_bank_interface_encryp, "zco_sios_sbiwebservice_encrypt, "zfi_bnk_sbi_in_encco_si_sbiweb, 14.07.2016
        proxy_data       TYPE zfile_upload2,                         "zfi_bnk_sbi_in_encfile_upload2, 14.07.2016
        lt_input         TYPE zfile_upload_response2,                "zfi_bnk_sbi_in_encfile_upload1, 14.07.2016
        lo_sys_exception TYPE REF TO cx_ai_system_fault.
  DATA: err_string       TYPE string.

  DATA: sig_list1 TYPE ssfsignertab.
  DATA: signer1   TYPE ssfsigner.
  DATA: doc_ver1  TYPE xstring.
  DATA: doc_line1 TYPE string.
  DATA: doc_tab1  TYPE ssftxttab.
  DATA l_sign1 TYPE ssfsigner.
  DATA it_zuser_signer1 TYPE TABLE OF zuser_signer.
  DATA wa_zuser_signer1 TYPE zuser_signer.

  DATA: ev_owner1         TYPE string,
        ev_email1         TYPE string,
        ev_serial1        TYPE string,
        ev_thumbprint1    TYPE string,
        ev_validfrom1     TYPE string,
        ev_validto1       TYPE string,
        ev_issuer1        TYPE string,
        ev_bindocument_out1 TYPE xstring,
        e_int1            TYPE i.

  DATA: go_file_verifier1        TYPE REF TO lcl_file_verifier.

  CREATE OBJECT lo_bnk_detail.


  IF ok_code2 = 'APPROVE2'.
    CALL METHOD c_alvgd2->get_selected_rows
      IMPORTING
        et_index_rows = gt_selected_rows2.

    CLEAR : lv_lines1.
    DESCRIBE TABLE gt_selected_rows2 LINES lv_lines1.
    IF lv_lines1 EQ 0.
      MESSAGE 'Please select one record' TYPE 'I'.
      EXIT.
    ELSEIF lv_lines1 > 1.
      MESSAGE 'Please select only single record' TYPE 'I'.
      EXIT.
    ENDIF.

    IF gt_selected_rows2 IS NOT INITIAL.
      READ TABLE gt_selected_rows2 INTO gs_selected_rows2 INDEX 1.
      IF sy-subrc = 0.
        READ TABLE gt_final2 INTO gs_final2 INDEX gs_selected_rows2-index.
        IF sy-subrc = 0.
          CALL FUNCTION 'SSFS_CALL_CONTROL'
            EXPORTING
*             TXTDOCUMENT     = doc_tab
              bindocument     = gs_final2-file_data_sent "doc_bin
              doc_type        = 'TXT'
*             ssf_id          = 'CN=Kashyap Banaam Bhushan, SP=Delhi, postalCode=110092, OU=CID - 2923089, O=Oil and Natural Gas Corporation Ltd., C=IN'
            IMPORTING
              crc             = lv_crc
              signature       = lv_doc_sig
            EXCEPTIONS
              parameter_error = 1
              conversion_error = 2
              control_error   = 3
              frontend_error  = 4
              OTHERS          = 5.


          IF lv_crc = 0.
            REFRESH sig_list1.
            CALL FUNCTION 'SSFS_SERVER_VERIFY'
              EXPORTING
*               SSFAPPLIC       =
                signature       = lv_doc_sig
              IMPORTING
                crc             = lv_crc
                signerlist      = sig_list1
                txtdocument_out = doc_tab1
                bindocument_out = doc_ver1
              EXCEPTIONS
                kernel_error    = 1
                parameter_error = 2
                OTHERS          = 3.
*BREAK-POINT.
            IF lv_crc = 0.
              go_file_verifier1 = lcl_file_verifier=>get_instance( ).

              READ TABLE sig_list1 INTO l_sign1 INDEX 1.
              IF sy-subrc = 0.
                CALL METHOD go_file_verifier1->get_signer_info
                  EXPORTING
                    is_signerlist = l_sign1
                  IMPORTING
                    ev_owner      = ev_owner1
                    ev_email      = ev_email1
                    ev_serial     = ev_serial1
                    ev_thumbprint = ev_thumbprint1
                    ev_validfrom  = ev_validfrom1
                    ev_validto    = ev_validto1
                    ev_issuer     = ev_issuer1
                    e_int         = e_int1.
              ENDIF.

              SELECT * INTO TABLE it_zuser_signer1
                FROM zuser_signer WHERE  uname = sy-uname
                                  AND    active = 'X'.
              IF sy-subrc = 0.
                READ TABLE it_zuser_signer1 INTO wa_zuser_signer1 WITH KEY thumbprin = ev_thumbprint1.
                IF sy-subrc <> 0.
                  MESSAGE 'Wrong Signature used for signing' TYPE 'I'.
                  EXIT.
                ENDIF.
              ELSE.
                MESSAGE 'No Signature Maintained for user' TYPE 'I'.
                EXIT.
              ENDIF.
            ENDIF.

            CLEAR gs_paym2.
            READ TABLE gt_paym2 INTO gs_paym2 WITH KEY laufi = gs_final2-laufi
                                                       laufd = gs_final2-laufd.
            IF sy-subrc = 0.
              IF gs_paym2-sent <> ' '.
                MESSAGE 'Can not be sent' TYPE 'I'.
                EXIT.
              ELSE.
                proxy_data-parameters-arg0  = gs_paym2-file_name.
                proxy_data-parameters-arg1  = lv_doc_sig.
                TRY.
                    CALL METHOD lo_bnk_detail->sios_bank_interface_encryption "sios_sbiwebservice_encryption "si_sbiwebservice_enc
                      EXPORTING
                        output = proxy_data
                      IMPORTING
                        input  = lt_input.
                  CATCH cx_ai_system_fault INTO lo_sys_exception.
                    err_string = lo_sys_exception->get_text( ).

                  CATCH cx_ai_application_fault .
                ENDTRY.
                IF err_string IS NOT INITIAL.
                  gs_paym2-file_data_sent = lv_doc_sig.
                  gs_paym2-sent_error     = 'X'.
                  gs_paym2-sent           = 'X'.
                  gs_paym2-sent_date      = sy-datum.
                  gs_paym2-sent_time      = sy-uzeit.
                  MODIFY zfi_paym_file FROM gs_paym2.
                  MESSAGE 'Error in sending' TYPE 'I'.
                ELSE.
                  gs_paym2-file_data_sent = lv_doc_sig.
                  gs_paym2-sent           = 'X'.
                  gs_paym2-sent_date      = sy-datum.
                  gs_paym2-sent_time      = sy-uzeit.
                  MODIFY zfi_paym_file FROM gs_paym2.
                ENDIF.
***** insert record in table ZFI_BATCH_SIGN
                CLEAR : gs_batch_sign2.
                READ TABLE gt_batch_sign2 INTO gs_batch_sign2 WITH KEY guid = gs_final2-guid
                                                                       signer   = sy-uname.
                IF sy-subrc = 0.
                  gs_batch_sign2-guid        = gs_final2-guid.
                  gs_batch_sign2-signer      = sy-uname.
                  gs_batch_sign2-digitl_sign = 'X'.
                  gs_batch_sign2-cdate1      = sy-datum.
                  gs_batch_sign2-ctime1      = sy-uzeit.
                  MODIFY zfi_batch_sign FROM gs_batch_sign2.
                  CLEAR : proxy_data.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " GET_SELECTED_ROW_TAB2  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  DATA lv_ucomm TYPE sy-ucomm.
  lv_ucomm = sy-ucomm.
  CASE lv_ucomm.
    WHEN 'APPROVE1'.
      PERFORM free_objects1.
    WHEN 'CANCEL' OR 'EXIT'.
      PERFORM free_objects1.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      PERFORM free_objects1.
*      SET SCREEN '0'.
      LEAVE PROGRAM.
  ENDCASE.


ENDMODULE.                 " USER_COMMAND_0110  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0102 INPUT.
  DATA lv_ucomm1 TYPE sy-ucomm.
  lv_ucomm1 = sy-ucomm.
  CASE lv_ucomm1.
    WHEN 'APPROVE2'.
      PERFORM free_objects2.
    WHEN 'CANCEL' OR 'EXIT'.
      PERFORM free_objects2.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      PERFORM free_objects2.
*      SET SCREEN '0'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0102  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
*DATA lv_ucomm2 TYPE sy-ucomm.
*  lv_ucomm2 = sy-ucomm.
*  CASE lv_ucomm2.
*    WHEN 'CANCEL' OR 'EXIT'.
*      PERFORM free_objects2.
*      LEAVE PROGRAM.
*    WHEN 'BACK'.
*      PERFORM free_objects2.
*      SET SCREEN '0'.
*      LEAVE SCREEN.
*  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROW_TAB3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_row_tab3 INPUT.
  DATA : lv_code TYPE sy-ucomm.
  lv_code = sy-ucomm.

  DATA: fileleng                      TYPE i,
        flen                          TYPE i,
        enh_version_management_hex_tb TYPE enh_version_management_hex_tb.

  IF lv_code = 'DOWNLOAD1'.
    PERFORM download_sent_data.
  ELSEIF lv_code = 'DOWNLOAD2'.
    PERFORM download_raw_data.
  ENDIF.

ENDMODULE.                 " GET_SELECTED_ROW_TAB3  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0103 INPUT.
  DATA lv_ucomm3 TYPE sy-ucomm.
  lv_ucomm3 = sy-ucomm.
  CASE lv_ucomm3.
    WHEN 'DOWNLOAD1' OR 'DOWNLOAD2'.
*      PERFORM free_objects3.
    WHEN 'CANCEL' OR 'EXIT'.
*      PERFORM free_objects3.
      LEAVE PROGRAM.
    WHEN 'BACK'.
*      PERFORM free_objects3.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0103  INPUT
