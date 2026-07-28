*----------------------------------------------------------------------*
***INCLUDE ZFI_BNK_APP_F.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_OP_TAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM f_prepare_op_tab .
* Data source changed from BNK_BATCH_HEADER to REGUT. As REGUT-GUID is not
* populated, BATCH_NO holds the concatenated REGUT primary key:
*   ZBUKR + BANKS + LAUFD + LAUFI + XVORL + DTKEY + LFDNR (RESPECTING BLANKS)
  DATA: lv_key  TYPE zfi_batch_sign-batch_no.
  DATA: lt_keys TYPE STANDARD TABLE OF zfi_batch_sign-batch_no,
        lv_k    TYPE zfi_batch_sign-batch_no.

  REFRESH : gt_paym, gt_batch_header,gt_final, gt_batch_sign.
*break sab_vaibhav.
  SELECT * FROM zfi_paym_file INTO TABLE gt_paym
                  WHERE laufd IN so_laufd
                  AND   laufi IN so_laufi
                  AND   sent_date IN so_sent.
  "                AND   sent = 'X'.

  IF gt_paym IS NOT INITIAL.
    SELECT * FROM regut INTO TABLE gt_batch_header
                        FOR ALL ENTRIES IN gt_paym
                        WHERE laufi = gt_paym-laufi
                        AND   laufd = gt_paym-laufd.

    IF gt_batch_header IS NOT INITIAL.
*     Build the concatenated REGUT key for every row, then read the
*     matching signature records (ZFI_BATCH_SIGN-BATCH_NO holds that key).
      REFRESH lt_keys.
      LOOP AT gt_batch_header INTO gs_batch_header.
        CLEAR lv_k.
        CONCATENATE gs_batch_header-zbukr gs_batch_header-banks
                    gs_batch_header-laufd gs_batch_header-laufi
                    gs_batch_header-xvorl gs_batch_header-dtkey
                    gs_batch_header-lfdnr
               INTO lv_k RESPECTING BLANKS.
        APPEND lv_k TO lt_keys.
        CLEAR gs_batch_header.
      ENDLOOP.
      SORT lt_keys.
      DELETE ADJACENT DUPLICATES FROM lt_keys.
      IF lt_keys IS NOT INITIAL.
        SELECT * FROM  zfi_batch_sign INTO TABLE gt_batch_sign
                        FOR ALL ENTRIES IN lt_keys
                        WHERE batch_no = lt_keys-table_line.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT gt_batch_header INTO gs_batch_header.

    CLEAR lv_key.
    CONCATENATE gs_batch_header-zbukr gs_batch_header-banks
                gs_batch_header-laufd gs_batch_header-laufi
                gs_batch_header-xvorl gs_batch_header-dtkey
                gs_batch_header-lfdnr
           INTO lv_key RESPECTING BLANKS.

    READ TABLE gt_batch_sign INTO gs_batch_sign WITH KEY batch_no = lv_key.

    IF sy-subrc EQ 0.
      IF gs_batch_sign-digitl_sign = ' '.
        gs_final-pending_with = gs_batch_sign-signer.
      ENDIF.
      MOVE-CORRESPONDING gs_batch_header TO gs_final.
      gs_final-guid       = lv_key.
      gs_final-batch_sum  = gs_batch_header-rbetr.
      gs_final-batch_curr = gs_batch_header-waers.
      gs_final-crusr      = gs_batch_header-tsusr.
      gs_final-crdate     = gs_batch_header-tsdat.
      gs_final-crtime     = gs_batch_header-tstim.
      gs_final-chusr      = gs_batch_header-dwusr.
      gs_final-chdate     = gs_batch_header-dwdat.
      gs_final-chtime     = gs_batch_header-dwtim.
      READ TABLE gt_paym INTO gs_paym WITH KEY laufd = gs_batch_header-laufd
                                               laufi = gs_batch_header-laufi.
      IF sy-subrc = 0.
        gs_final-raw_data           = gs_paym-raw_data.
        gs_final-file_data_sent     = gs_paym-file_data_sent.
        gs_final-error_flag         = gs_paym-sent_error.
        gs_final-sent_flag          = gs_paym-sent.
        gs_final-rec_flag           = gs_paym-received.               "28.07.2014
        gs_final-file_data_received = gs_paym-file_data_received.     "28.07.2014
        gs_final-file_name          = gs_paym-file_name.              "28.07.2014
        gs_final-received_filename  = gs_paym-received_filename.      "28.07.2014
      ENDIF.
      APPEND gs_final TO gt_final.
    ENDIF.
*    endif.
    CLEAR : gs_batch_header,gs_batch_sign,gs_final.
  ENDLOOP.
  SORT gt_final BY guid.
ENDFORM.                    " F_PREPARE_OP_TAB
*&---------------------------------------------------------------------*
*&      Form  F_ALV_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM f_alv_build_fieldcat .
  DATA lv_fldcat TYPE lvc_s_fcat.
  DATA: lv_index TYPE sy-index.
  REFRESH : it_fcat.
  CLEAR lv_fldcat.
  lv_fldcat-tabname   = 'GT_FINAL'.
  lv_fldcat-fieldname = 'GUID'.
  lv_fldcat-scrtext_m = 'Batch Key'.
  lv_fldcat-outputlen = '45'.
*  lv_fldcat-row_pos   = '1'.
  lv_fldcat-col_pos   = lv_index.
  APPEND lv_fldcat TO it_fcat.

* RULE_ID / ITEM_CNT not available in REGUT - columns removed

  lv_index  = lv_index + 1.
  CLEAR lv_fldcat.
  lv_fldcat-tabname   = 'GT_FINAL'.
  lv_fldcat-fieldname = 'LAUFD'.
  lv_fldcat-scrtext_m = 'Run Date'.
  lv_fldcat-outputlen = '11'.
*  lv_fldcat-row_pos   = '1'.
  lv_fldcat-col_pos   = lv_index.
  APPEND lv_fldcat TO it_fcat.

  lv_index  = lv_index + 1.
  CLEAR lv_fldcat.
  lv_fldcat-tabname   = 'GT_FINAL'.
  lv_fldcat-fieldname = 'LAUFI'.
  lv_fldcat-scrtext_m = 'Run Id'.
  lv_fldcat-outputlen = '8'.
*  lv_fldcat-row_pos   = '1'.
  lv_fldcat-col_pos   = lv_index.
  APPEND lv_fldcat TO it_fcat.

* LAUFD_F / LAUFI_F (file date/id) not available in REGUT - columns removed

  lv_index  = lv_index + 1.
  CLEAR lv_fldcat.
  lv_fldcat-tabname   = 'GT_FINAL'.
  lv_fldcat-fieldname = 'SENT_FLAG'.
  lv_fldcat-scrtext_m = 'Sent'.
  lv_fldcat-outputlen = '4'.
*  lv_fldcat-row_pos   = '1'.
  lv_fldcat-col_pos   = lv_index.
  APPEND lv_fldcat TO it_fcat.

  lv_index  = lv_index + 1.
  CLEAR lv_fldcat.
  lv_fldcat-tabname   = 'GT_FINAL'.
  lv_fldcat-fieldname = 'ERROR_FLAG'.
  lv_fldcat-scrtext_m = 'Error'.
  lv_fldcat-outputlen = '5'.
*  lv_fldcat-row_pos   = '1'.
  lv_fldcat-col_pos   = lv_index.
  APPEND lv_fldcat TO it_fcat.

  lv_index  = lv_index + 1.
  CLEAR lv_fldcat.
  lv_fldcat-tabname   = 'GT_FINAL'.
  lv_fldcat-fieldname = 'REC_FLAG'.
  lv_fldcat-scrtext_m = 'Rec.'.
  lv_fldcat-outputlen = '4'.
*  lv_fldcat-row_pos   = '1'.
  lv_fldcat-col_pos   = lv_index.
  APPEND lv_fldcat TO it_fcat.

  lv_index  = lv_index + 1.
  CLEAR lv_fldcat.
  lv_fldcat-tabname   = 'GT_FINAL'.
  lv_fldcat-fieldname = 'BATCH_SUM'.
  lv_fldcat-scrtext_m = 'Batch Amount'.
  lv_fldcat-outputlen = '15'.
*  lv_fldcat-row_pos   = '1'.
  lv_fldcat-col_pos   = lv_index.
  APPEND lv_fldcat TO it_fcat.

*  lv_index  = lv_index + 1.
*  CLEAR lv_fldcat.
*  lv_fldcat-tabname   = 'GT_FINAL'.
*  lv_fldcat-fieldname = 'MAX_PAY_AMT'.
*  lv_fldcat-scrtext_m = 'Maximum payment'.
*  lv_fldcat-outputlen = '15'.
**  lv_fldcat-row_pos   = '1'.
*  lv_fldcat-col_pos   = lv_index.
*  APPEND lv_fldcat TO it_fcat.

  lv_index  = lv_index + 1.
  CLEAR lv_fldcat.
  lv_fldcat-tabname   = 'GT_FINAL'.
  lv_fldcat-fieldname = 'CRUSR'.
  lv_fldcat-scrtext_m = 'Create User'.
  lv_fldcat-outputlen = '12'.
*  lv_fldcat-row_pos   = '1'.
  lv_fldcat-col_pos   = lv_index.
  APPEND lv_fldcat TO it_fcat.

  lv_index  = lv_index + 1.
  CLEAR lv_fldcat.
  lv_fldcat-tabname   = 'GT_FINAL'.
  lv_fldcat-fieldname = 'CRTIME'.
  lv_fldcat-scrtext_m = 'Create Time'.
  lv_fldcat-outputlen = '11'.
  lv_fldcat-row_pos   = '1'.
  lv_fldcat-col_pos   = lv_index.
  APPEND lv_fldcat TO it_fcat.

  lv_index  = lv_index + 1.
  CLEAR lv_fldcat.
  lv_fldcat-tabname   = 'GT_FINAL'.
  lv_fldcat-fieldname = 'CRDATE'.
  lv_fldcat-scrtext_m = 'Create Date'.
  lv_fldcat-outputlen = '11'.
*  lv_fldcat-row_pos   = '1'.
  lv_fldcat-col_pos   = lv_index.
  APPEND lv_fldcat TO it_fcat.

  lv_index  = lv_index + 1.
  CLEAR lv_fldcat.
  lv_fldcat-tabname   = 'GT_FINAL'.
  lv_fldcat-fieldname = 'CHUSR'.
  lv_fldcat-scrtext_m = 'Change User'.
  lv_fldcat-outputlen = '12'.
*  lv_fldcat-row_pos   = '1'.
  lv_fldcat-col_pos   = lv_index.
  APPEND lv_fldcat TO it_fcat.

  lv_index  = lv_index + 1.
  CLEAR lv_fldcat.
  lv_fldcat-tabname   = 'GT_FINAL'.
  lv_fldcat-fieldname = 'CHTIME'.
  lv_fldcat-scrtext_m = 'Change Time'.
  lv_fldcat-outputlen = '11'.
*  lv_fldcat-row_pos   = '1'.
  lv_fldcat-col_pos   = lv_index.
  APPEND lv_fldcat TO it_fcat.

  lv_index  = lv_index + 1.
  CLEAR lv_fldcat.
  lv_fldcat-tabname   = 'GT_FINAL'.
  lv_fldcat-fieldname = 'CHDATE'.
  lv_fldcat-scrtext_m = 'Change Date'.
  lv_fldcat-outputlen = '11'.
*  lv_fldcat-row_pos   = '1'.
  lv_fldcat-col_pos   = lv_index.
  APPEND lv_fldcat TO it_fcat.

  lv_index  = lv_index + 1.
  CLEAR lv_fldcat.
  lv_fldcat-tabname   = 'GT_FINAL'.
  lv_fldcat-fieldname = 'ZBUKR'.
  lv_fldcat-scrtext_m = 'Paying company code'.
  lv_fldcat-outputlen = '19'.
*  lv_fldcat-row_pos   = '1'.
  lv_fldcat-col_pos   = lv_index.
  APPEND lv_fldcat TO it_fcat.

*  lv_index  = lv_index + 1.
*   CLEAR lv_fldcat.
*  lv_fldcat-tabname   = 'GT_FINAL'.
*  lv_fldcat-fieldname = 'HBKID'.
*  lv_fldcat-scrtext_m = 'Short Key for a House Bank'.
*  lv_fldcat-outputlen = '26'.
**  lv_fldcat-row_pos   = '1'.
*  lv_fldcat-col_pos   = lv_index.
*  APPEND lv_fldcat TO it_fcat.

* TOT_BTCH_AMT not available in REGUT - column removed

  lv_index  = lv_index + 1.
  CLEAR lv_fldcat.
  lv_fldcat-tabname   = 'GT_FINAL'.
  lv_fldcat-fieldname = 'PENDING_WITH'.
  lv_fldcat-scrtext_m = 'Pending With'.
  lv_fldcat-outputlen = '12'.
*  lv_fldcat-row_pos   = '1'.
  lv_fldcat-col_pos   = lv_index.
  APPEND lv_fldcat TO it_fcat.
*  lv_index  = lv_index + 1.
*   CLEAR lv_fldcat.
*  lv_fldcat-tabname   = 'GT_FINAL'.
*  lv_fldcat-fieldname = 'MAXPAYAMT_RULECU'.
*  lv_fldcat-scrtext_m = 'Maximum payment Amount'.
*  lv_fldcat-outputlen = '22'.
*  lv_fldcat-row_pos   = '1'.
*  lv_fldcat-col_pos   = lv_index.
*  APPEND lv_fldcat TO it_fcat.

*   lv_index  = lv_index + 1.
*   CLEAR lv_fldcat.
*  lv_fldcat-tabname   = 'GT_FINAL'.
*  lv_fldcat-fieldname = 'GRP_FIELD1_VALUE'.
*  lv_fldcat-scrtext_m = 'Grouping field val1'.
*  lv_fldcat-outputlen = '30'.
*  lv_fldcat-row_pos   = '1'.
*  lv_fldcat-col_pos   = lv_index.
*  APPEND lv_fldcat TO it_fcat.
*
*   lv_index  = lv_index + 1.
*   CLEAR lv_fldcat.
*  lv_fldcat-tabname   = 'GT_FINAL'.
*  lv_fldcat-fieldname = 'GRP_FIELD2_VALUE'.
*  lv_fldcat-scrtext_m = 'Grouping field val2'.
*  lv_fldcat-outputlen = '30'.
*  lv_fldcat-row_pos   = '1'.
*  lv_fldcat-col_pos   = lv_index.
*  APPEND lv_fldcat TO it_fcat.

ENDFORM.                    " F_ALV_BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_ALV_REPORT_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM f_alv_report_layout .
  CLEAR: it_layout.
  it_layout-cwidth_opt = 'X'.
  it_layout-sel_mode   = 'A'.

ENDFORM.                    " F_ALV_REPORT_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_SENT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM download_sent_data .

  DATA:   lv_lines TYPE i.
  DATA: fileleng TYPE i,
        flen     TYPE i,
        lv_filename TYPE string,
        enh_version_management_hex_tb TYPE enh_version_management_hex_tb.


  DATA: window_title TYPE string,
        def_ext      TYPE string VALUE '.TXT',
        file_name    TYPE string,
        file_path    TYPE string,
        full_path    TYPE string.

  REFRESH : gt_selected_rows.
  CALL METHOD c_alvgd->get_selected_rows
    IMPORTING
      et_index_rows = gt_selected_rows.

  CLEAR : lv_lines.
  DESCRIBE TABLE gt_selected_rows LINES lv_lines.
  IF lv_lines EQ 0.
    MESSAGE 'Please select one record' TYPE 'I'.
    EXIT.
  ELSEIF lv_lines > 1.
    MESSAGE 'Please select only single record' TYPE 'I'.
    EXIT.
  ENDIF.

  IF gt_selected_rows IS NOT INITIAL.
    READ TABLE gt_selected_rows INTO gs_selected_rows INDEX 1.
    IF sy-subrc = 0.
      READ TABLE gt_final INTO gs_final INDEX gs_selected_rows-index.
      IF sy-subrc = 0.
        IF gs_final-sent_flag = ' '.
          MESSAGE 'Download can not be performed' TYPE 'I'.
          EXIT.
        ELSE.
          CLEAR gs_paym.
          READ TABLE gt_paym INTO gs_paym WITH KEY laufi = gs_final-laufi
                                                   laufd = gs_final-laufd.
          IF sy-subrc EQ 0.
            CLEAR file_name.
            file_name = gs_paym-file_name.

            CALL FUNCTION 'ENH_XSTRING_TO_TAB'
              EXPORTING
                im_xstring = gs_final-file_data_sent
              IMPORTING
                ex_data    = enh_version_management_hex_tb
                ex_leng    = flen.

            window_title = 'Enter File Name and File Path'.
            CALL METHOD cl_gui_frontend_services=>file_save_dialog
              EXPORTING
                window_title      = window_title
                default_extension = def_ext
                default_file_name = file_name
              CHANGING
                filename          = file_name
                path              = file_path
                fullpath          = full_path.
            IF full_path IS INITIAL.
              MESSAGE 'Enter File name and File Path' TYPE 'I'.
            ELSE.
              CALL FUNCTION 'GUI_DOWNLOAD'
                EXPORTING
                  bin_filesize            = flen          "ostr_updated_signed_data_l
                  filename                = full_path      "lv_filename "'C:\TEST\TESTV1.TXT'
                  filetype                = 'BIN'
*                 APPEND                  = ' '
*                 WRITE_FIELD_SEPARATOR   = ' '
*                 HEADER                  = '00'
*                 TRUNC_TRAILING_BLANKS   = ' '
                IMPORTING
                  filelength              = fileleng
                TABLES
                  data_tab                = enh_version_management_hex_tb "OUT_DATA_TABLE
                EXCEPTIONS
                  file_write_error        = 1
                  no_batch                = 2
                  gui_refuse_filetransfer = 3
                  invalid_type            = 4
                  no_authority            = 5
                  unknown_error           = 6
                  header_not_allowed      = 7
                  separator_not_allowed   = 8
                  filesize_not_allowed    = 9
                  header_too_long         = 10
                  dp_error_create         = 11
                  dp_error_send           = 12
                  dp_error_write          = 13
                  unknown_dp_error        = 14
                  access_denied           = 15
                  dp_out_of_memory        = 16
                  disk_full               = 17
                  dp_timeout              = 18
                  file_not_found          = 19
                  dataprovider_exception  = 20
                  control_flush_error     = 21
                  OTHERS                  = 22.
              IF sy-subrc EQ 0.
                MESSAGE 'File Downloaded Succesfully' TYPE 'I'.
              ENDIF.
              CLEAR :fileleng,flen,file_name,file_path,full_path.
              REFRESH : enh_version_management_hex_tb.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " DOWNLOAD_SENT_DATA
*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_RAW_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM download_raw_data .
  DATA: lv_lines TYPE i.
  DATA: fileleng TYPE i,
        flen     TYPE i,
        lv_filename TYPE string,
        enh_version_management_hex_tb TYPE enh_version_management_hex_tb.

  DATA: window_title TYPE string,
        def_ext      TYPE string VALUE '.TXT',
        file_name    TYPE string,
        file_path    TYPE string,
        full_path    TYPE string.

  REFRESH : gt_selected_rows.
  CALL METHOD c_alvgd->get_selected_rows
    IMPORTING
      et_index_rows = gt_selected_rows.

  CLEAR : lv_lines.
  DESCRIBE TABLE gt_selected_rows LINES lv_lines.
  IF lv_lines EQ 0.
    MESSAGE 'Please select one record' TYPE 'I'.
    EXIT.
  ELSEIF lv_lines > 1.
    MESSAGE 'Please select only single record' TYPE 'I'.
    EXIT.
  ENDIF.

  IF gt_selected_rows IS NOT INITIAL.
    READ TABLE gt_selected_rows INTO gs_selected_rows INDEX 1.
    IF sy-subrc = 0.
      READ TABLE gt_final INTO gs_final INDEX gs_selected_rows-index.
      IF sy-subrc = 0.
        CLEAR gs_paym.
        READ TABLE gt_paym INTO gs_paym WITH KEY laufi = gs_final-laufi
                                                 laufd = gs_final-laufd.
        IF sy-subrc EQ 0.
          CLEAR file_name.
          CONCATENATE 'R' gs_paym-file_name INTO file_name.

          CALL FUNCTION 'ENH_XSTRING_TO_TAB'
            EXPORTING
              im_xstring = gs_final-raw_data
            IMPORTING
              ex_data    = enh_version_management_hex_tb
              ex_leng    = flen.

          window_title = 'Enter File Name and File Path'.
          CALL METHOD cl_gui_frontend_services=>file_save_dialog
            EXPORTING
              window_title      = window_title
              default_extension = def_ext
              default_file_name = file_name
            CHANGING
              filename          = file_name
              path              = file_path
              fullpath          = full_path.
          IF full_path IS INITIAL.
            MESSAGE 'Enter File name and File Path' TYPE 'I'.
          ELSE.
            CALL FUNCTION 'GUI_DOWNLOAD'
              EXPORTING
                bin_filesize            = flen          "ostr_updated_signed_data_l
                filename                = full_path      "lv_filename "'C:\TEST\TESTV1.TXT'
                filetype                = 'BIN'
*               APPEND                  = ' '
*               WRITE_FIELD_SEPARATOR   = ' '
*               HEADER                  = '00'
*               TRUNC_TRAILING_BLANKS   = ' '
              IMPORTING
                filelength              = fileleng
              TABLES
                data_tab                = enh_version_management_hex_tb "OUT_DATA_TABLE
              EXCEPTIONS
                file_write_error        = 1
                no_batch                = 2
                gui_refuse_filetransfer = 3
                invalid_type            = 4
                no_authority            = 5
                unknown_error           = 6
                header_not_allowed      = 7
                separator_not_allowed   = 8
                filesize_not_allowed    = 9
                header_too_long         = 10
                dp_error_create         = 11
                dp_error_send           = 12
                dp_error_write          = 13
                unknown_dp_error        = 14
                access_denied           = 15
                dp_out_of_memory        = 16
                disk_full               = 17
                dp_timeout              = 18
                file_not_found          = 19
                dataprovider_exception  = 20
                control_flush_error     = 21
                OTHERS                  = 22.
            IF sy-subrc EQ 0.
              MESSAGE 'File Downloaded Succesfully' TYPE 'I'.
            ENDIF.
            CLEAR :fileleng,flen,file_name,file_path,full_path.
            REFRESH : enh_version_management_hex_tb.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " DOWNLOAD_RAW_DATA

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_REC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM download_rec_data .
  DATA: lv_lines TYPE i.
  DATA: fileleng TYPE i,
        flen     TYPE i,
        lv_filename TYPE string,
        enh_version_management_hex_tb TYPE enh_version_management_hex_tb.


  DATA: window_title TYPE string,
        def_ext      TYPE string VALUE '.TXT',
        file_name    TYPE string,
        file_path    TYPE string,
        full_path    TYPE string.

  REFRESH : gt_selected_rows.
  CALL METHOD c_alvgd->get_selected_rows
    IMPORTING
      et_index_rows = gt_selected_rows.

  CLEAR : lv_lines.
  DESCRIBE TABLE gt_selected_rows LINES lv_lines.
  IF lv_lines EQ 0.
    MESSAGE 'Please select one record' TYPE 'I'.
    EXIT.
  ELSEIF lv_lines > 1.
    MESSAGE 'Please select only single record' TYPE 'I'.
    EXIT.
  ENDIF.

  IF gt_selected_rows IS NOT INITIAL.
    READ TABLE gt_selected_rows INTO gs_selected_rows INDEX 1.
    IF sy-subrc = 0.
      READ TABLE gt_final INTO gs_final INDEX gs_selected_rows-index.
      IF sy-subrc = 0.
        IF gs_final-rec_flag = ' '.
          MESSAGE 'Download can not be performed' TYPE 'I'.
          EXIT.
        ELSE.
          CLEAR gs_paym.
          READ TABLE gt_paym INTO gs_paym WITH KEY laufi = gs_final-laufi
                                                   laufd = gs_final-laufd.
          IF sy-subrc EQ 0.
            CLEAR file_name.
            file_name = gs_paym-received_filename .

            CALL FUNCTION 'ENH_XSTRING_TO_TAB'
              EXPORTING
                im_xstring = gs_final-file_data_received
              IMPORTING
                ex_data    = enh_version_management_hex_tb
                ex_leng    = flen.

            window_title = 'Enter File Name and File Path'.
            CALL METHOD cl_gui_frontend_services=>file_save_dialog
              EXPORTING
                window_title      = window_title
                default_extension = def_ext
                default_file_name = file_name
              CHANGING
                filename          = file_name
                path              = file_path
                fullpath          = full_path.
            IF full_path IS INITIAL.
              MESSAGE 'Enter File name and File Path' TYPE 'I'.
            ELSE.
              CALL FUNCTION 'GUI_DOWNLOAD'
                EXPORTING
                  bin_filesize            = flen          "ostr_updated_signed_data_l
                  filename                = full_path      "lv_filename "'C:\TEST\TESTV1.TXT'
                  filetype                = 'BIN'
*                 APPEND                  = ' '
*                 WRITE_FIELD_SEPARATOR   = ' '
*                 HEADER                  = '00'
*                 TRUNC_TRAILING_BLANKS   = ' '
                IMPORTING
                  filelength              = fileleng
                TABLES
                  data_tab                = enh_version_management_hex_tb "OUT_DATA_TABLE
                EXCEPTIONS
                  file_write_error        = 1
                  no_batch                = 2
                  gui_refuse_filetransfer = 3
                  invalid_type            = 4
                  no_authority            = 5
                  unknown_error           = 6
                  header_not_allowed      = 7
                  separator_not_allowed   = 8
                  filesize_not_allowed    = 9
                  header_too_long         = 10
                  dp_error_create         = 11
                  dp_error_send           = 12
                  dp_error_write          = 13
                  unknown_dp_error        = 14
                  access_denied           = 15
                  dp_out_of_memory        = 16
                  disk_full               = 17
                  dp_timeout              = 18
                  file_not_found          = 19
                  dataprovider_exception  = 20
                  control_flush_error     = 21
                  OTHERS                  = 22.
              IF sy-subrc EQ 0.
                MESSAGE 'File Downloaded Succesfully' TYPE 'I'.
              ENDIF.
              CLEAR :fileleng,flen,file_name,file_path,full_path.
              REFRESH : enh_version_management_hex_tb.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " DOWNLOAD_REC_DATA
*&---------------------------------------------------------------------*
*&      Form  F_RESENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P1        text
*      <--P2        text
*----------------------------------------------------------------------*
FORM f_resent .
  REFRESH : gt_selected_rows.
  DATA: lv_lines TYPE i.
  DATA: fileleng TYPE i,
        flen     TYPE i,
        lv_filename TYPE string,
        enh_version_management_hex_tb TYPE enh_version_management_hex_tb.

  DATA: lo_bnk_detail    TYPE REF TO zco_sios_bank_interface_encryp, "zco_sios_sbiwebservice_encrypt, "zfi_bnk_sbi_in_encco_si_sbiweb,
        proxy_data       TYPE zfile_upload2,                         "zfi_bnk_sbi_in_encfile_upload2,
        lt_input         TYPE zfile_upload_response2,                "zfi_bnk_sbi_in_encfile_upload1,
        lo_sys_exception TYPE REF TO cx_ai_system_fault.

  DATA: err_string TYPE string.

  CREATE OBJECT lo_bnk_detail.

  CALL METHOD c_alvgd->get_selected_rows
    IMPORTING
      et_index_rows = gt_selected_rows.

  CLEAR : lv_lines.
  DESCRIBE TABLE gt_selected_rows LINES lv_lines.
  IF lv_lines EQ 0.

    MESSAGE 'Please select one record' TYPE 'I'.
    EXIT.
  ELSEIF lv_lines > 1.
    MESSAGE 'Please select only single record' TYPE 'I'.
    EXIT.
  ENDIF.

  IF gt_selected_rows IS NOT INITIAL.
    READ TABLE gt_selected_rows INTO gs_selected_rows INDEX 1.
    IF sy-subrc = 0.
      READ TABLE gt_final INTO gs_final INDEX gs_selected_rows-index.
      IF sy-subrc = 0.

        CLEAR gs_paym.
        READ TABLE gt_paym INTO gs_paym WITH KEY laufi = gs_final-laufi
                                                 laufd = gs_final-laufd.
        IF sy-subrc = 0.
          IF gs_paym-sent <> 'X'.
            MESSAGE 'Can not be resent' TYPE 'I'.
            EXIT.
          ELSE.
            proxy_data-parameters-arg0  = gs_paym-file_name.
            proxy_data-parameters-arg1  = gs_paym-file_data_sent.
            TRY.
                CALL METHOD lo_bnk_detail->sios_bank_interface_encryption "SIOS_SBIWEBSERVICE_ENCRYPTION "si_sbiwebservice_enc
                  EXPORTING
                    output = proxy_data
                  IMPORTING
                    input  = lt_input.
              CATCH cx_ai_system_fault INTO lo_sys_exception.
                err_string = lo_sys_exception->get_text( ).

              CATCH cx_ai_application_fault .
            ENDTRY.
            IF err_string IS NOT INITIAL.
              gs_paym-file_data_sent = gs_paym-file_data_sent.
              gs_paym-sent_error     = 'X'.
              gs_paym-sent           = 'X'.
              gs_paym-sent_date      = sy-datum.
              gs_paym-sent_time      = sy-uzeit.
              MODIFY zfi_paym_file FROM gs_paym.
              MESSAGE 'Error in sending' TYPE 'I'.
            ELSE.
              gs_paym-file_data_sent = gs_paym-file_data_sent.
              gs_paym-sent           = 'X'.
              gs_paym-sent_error     = ' '.
              gs_paym-sent_date      = sy-datum.
              gs_paym-sent_time      = sy-uzeit.
              MODIFY zfi_paym_file FROM gs_paym.
              MESSAGE 'Sent Successfully' TYPE 'I'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_RESENT
