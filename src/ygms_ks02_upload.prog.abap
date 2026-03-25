*&---------------------------------------------------------------------*
*& Report YGMS_KS02_UPLOAD
*& Description: Mass Change Cost Centers via BDC on Transaction KS02
*& Supports both .xls and .xlsx Excel formats
*&---------------------------------------------------------------------*
REPORT ygms_ks02_upload.

TYPE-POOLS: slis.

*----------------------------------------------------------------------*
* Type Definitions
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_excel_data,
         kokrs TYPE csks-kokrs,     " Controlling Area
         kostl TYPE csks-kostl,     " Cost Center
         ktext TYPE csks-ktext,     " Name
         ltext TYPE csks-ltext,     " Description
         verak TYPE csks-verak,     " Person Responsible
         abtei TYPE csks-abtei,     " Department
       END OF ty_excel_data.

TYPES: BEGIN OF ty_result,
         icon    TYPE icon_d,
         kokrs   TYPE csks-kokrs,
         kostl   TYPE csks-kostl,
         ktext   TYPE csks-ktext,
         msgtyp  TYPE bdcmsgcoll-msgtyp,
         msgnr   TYPE bdcmsgcoll-msgnr,
         message TYPE char220,
       END OF ty_result.

*----------------------------------------------------------------------*
* Data Declarations
*----------------------------------------------------------------------*
DATA: gt_excel   TYPE TABLE OF ty_excel_data,
      gs_excel   TYPE ty_excel_data,
      gt_result  TYPE TABLE OF ty_result,
      gs_result  TYPE ty_result,
      gt_raw     TYPE TABLE OF alsmex_tabline,
      gs_raw     TYPE alsmex_tabline,
      bdcdata    TYPE TABLE OF bdcdata WITH HEADER LINE,
      messtab    TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      gv_success TYPE i,
      gv_error   TYPE i,
      gv_total   TYPE i.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_mode TYPE char1 DEFAULT 'N'.
  PARAMETERS: p_test AS CHECKBOX DEFAULT 'X'.
  PARAMETERS: p_sess AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file_help.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM check_authority.
  PERFORM upload_excel.
  PERFORM convert_excel_to_data.
  PERFORM validate_data.
  PERFORM process_bdc.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
*& Form F4_FILE_HELP
*&---------------------------------------------------------------------*
FORM f4_file_help.
  DATA: lt_file_table TYPE filetable,
        ls_file       TYPE file_table,
        lv_rc         TYPE i,
        lv_action     TYPE i.

  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      window_title   = 'Select Excel File'
      file_filter    = 'Excel Files (*.xlsx;*.xls)|*.xlsx;*.xls|All Files (*.*)|*.*'
    CHANGING
      file_table     = lt_file_table
      rc             = lv_rc
      user_action    = lv_action
  ).

  IF lv_action = cl_gui_frontend_services=>action_ok AND lv_rc = 1.
    READ TABLE lt_file_table INTO ls_file INDEX 1.
    p_file = ls_file-filename.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_AUTHORITY
*&---------------------------------------------------------------------*
FORM check_authority.
  AUTHORITY-CHECK OBJECT 'K_CSKS'
    ID 'ACTVT' FIELD '02'
    ID 'KOKRS' DUMMY.

  IF sy-subrc <> 0.
    MESSAGE e001(00) WITH 'No authorization to change cost centers'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPLOAD_EXCEL
*&---------------------------------------------------------------------*
FORM upload_excel.
  DATA: lv_ext TYPE char10.

  " Determine file extension
  PERFORM get_file_extension USING p_file CHANGING lv_ext.

  TRANSLATE lv_ext TO UPPER CASE.

  IF lv_ext = 'XLSX'.
    PERFORM upload_xlsx.
  ELSEIF lv_ext = 'XLS'.
    PERFORM upload_xls.
  ELSE.
    MESSAGE e001(00) WITH 'Unsupported file format. Use .xls or .xlsx'.
  ENDIF.

  IF gt_raw IS INITIAL.
    MESSAGE e001(00) WITH 'No data found in Excel file'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_FILE_EXTENSION
*&---------------------------------------------------------------------*
FORM get_file_extension USING pv_file TYPE rlgrap-filename
                        CHANGING pv_ext TYPE char10.
  DATA: lv_file TYPE string,
        lv_len  TYPE i,
        lv_pos  TYPE i.

  lv_file = pv_file.
  lv_len = strlen( lv_file ).

  " Find last dot position
  lv_pos = -1.
  DO lv_len TIMES.
    DATA(lv_idx) = sy-index - 1.
    IF lv_file+lv_idx(1) = '.'.
      lv_pos = lv_idx.
    ENDIF.
  ENDDO.

  IF lv_pos >= 0.
    lv_pos = lv_pos + 1.
    pv_ext = lv_file+lv_pos.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPLOAD_XLS - Read .xls files via ALSM_EXCEL_TO_INTERNAL_TABLE
*&---------------------------------------------------------------------*
FORM upload_xls.
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 6
      i_end_row               = 9999
    TABLES
      intern                  = gt_raw
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    MESSAGE e001(00) WITH 'Error reading .xls file'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPLOAD_XLSX - Read .xlsx files via cl_fdt_xl_spreadsheet
*&---------------------------------------------------------------------*
FORM upload_xlsx.
  DATA: lt_bin_data  TYPE solix_tab,
        lv_file_size TYPE i,
        lv_xstring   TYPE xstring,
        lo_excel     TYPE REF TO cl_fdt_xl_spreadsheet,
        lt_worksheets TYPE if_fdt_doc_spreadsheet=>t_worksheet_names,
        lv_sheet     TYPE string,
        lo_data      TYPE REF TO data.

  FIELD-SYMBOLS: <ft_data>  TYPE STANDARD TABLE,
                 <fs_row>   TYPE any,
                 <fv_value> TYPE any.

  " Upload file as binary
  cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename   = CONV #( p_file )
      filetype   = 'BIN'
    IMPORTING
      filelength = lv_file_size
    CHANGING
      data_tab   = lt_bin_data
    EXCEPTIONS
      OTHERS     = 1
  ).

  IF sy-subrc <> 0.
    MESSAGE e001(00) WITH 'Error uploading .xlsx file'.
  ENDIF.

  " Convert binary to xstring
  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = lv_file_size
    IMPORTING
      buffer       = lv_xstring
    TABLES
      binary_tab   = lt_bin_data
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    MESSAGE e001(00) WITH 'Error converting file data'.
  ENDIF.

  " Create spreadsheet instance
  TRY.
      CREATE OBJECT lo_excel
        EXPORTING
          document_name = 'KS02_UPLOAD'
          xdocument     = lv_xstring.
    CATCH cx_fdt_excel_core.
      MESSAGE e001(00) WITH 'Error parsing .xlsx file'.
  ENDTRY.

  " Get first worksheet
  lo_excel->if_fdt_doc_spreadsheet~get_worksheet_names(
    IMPORTING
      worksheet_names = lt_worksheets
  ).

  IF lt_worksheets IS INITIAL.
    MESSAGE e001(00) WITH 'No worksheets found in .xlsx file'.
  ENDIF.

  READ TABLE lt_worksheets INTO lv_sheet INDEX 1.

  " Get worksheet data as internal table
  lo_data = lo_excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lv_sheet ).

  ASSIGN lo_data->* TO <ft_data>.

  IF <ft_data> IS NOT ASSIGNED OR <ft_data> IS INITIAL.
    MESSAGE e001(00) WITH 'No data found in worksheet'.
  ENDIF.

  " Convert xlsx data to ALSMEX format (skip header row 1)
  DATA: lv_row TYPE i VALUE 0,
        lv_col TYPE i.

  LOOP AT <ft_data> ASSIGNING <fs_row>.
    lv_row = lv_row + 1.

    " Skip header row
    IF lv_row = 1.
      CONTINUE.
    ENDIF.

    DO 6 TIMES.
      lv_col = sy-index.
      ASSIGN COMPONENT lv_col OF STRUCTURE <fs_row> TO <fv_value>.
      IF sy-subrc = 0 AND <fv_value> IS NOT INITIAL.
        CLEAR gs_raw.
        gs_raw-row   = lv_row.
        gs_raw-col   = lv_col.
        gs_raw-value = <fv_value>.
        APPEND gs_raw TO gt_raw.
      ENDIF.
    ENDDO.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CONVERT_EXCEL_TO_DATA
*&---------------------------------------------------------------------*
FORM convert_excel_to_data.
  DATA: lv_prev_row TYPE i VALUE 0.

  LOOP AT gt_raw INTO gs_raw.
    IF gs_raw-row <> lv_prev_row AND lv_prev_row <> 0.
      " Save previous row
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_excel-kostl
        IMPORTING
          output = gs_excel-kostl.
      APPEND gs_excel TO gt_excel.
      CLEAR gs_excel.
    ENDIF.

    lv_prev_row = gs_raw-row.

    CASE gs_raw-col.
      WHEN 1. gs_excel-kokrs = gs_raw-value.
      WHEN 2. gs_excel-kostl = gs_raw-value.
      WHEN 3. gs_excel-ktext = gs_raw-value.
      WHEN 4. gs_excel-ltext = gs_raw-value.
      WHEN 5. gs_excel-verak = gs_raw-value.
      WHEN 6. gs_excel-abtei = gs_raw-value.
    ENDCASE.
  ENDLOOP.

  " Append last row
  IF lv_prev_row <> 0.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_excel-kostl
      IMPORTING
        output = gs_excel-kostl.
    APPEND gs_excel TO gt_excel.
    CLEAR gs_excel.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form VALIDATE_DATA
*&---------------------------------------------------------------------*
FORM validate_data.
  DATA: lv_index TYPE sy-tabix.

  LOOP AT gt_excel INTO gs_excel.
    lv_index = sy-tabix.
    CLEAR gs_result.

    gs_result-kokrs = gs_excel-kokrs.
    gs_result-kostl = gs_excel-kostl.
    gs_result-ktext = gs_excel-ktext.

    " Check mandatory fields for KS02 (only key fields required)
    IF gs_excel-kokrs IS INITIAL.
      gs_result-icon    = icon_led_red.
      gs_result-msgtyp  = 'E'.
      gs_result-message = 'Controlling Area is mandatory'.
      APPEND gs_result TO gt_result.
      DELETE gt_excel INDEX lv_index.
      CONTINUE.
    ENDIF.

    IF gs_excel-kostl IS INITIAL.
      gs_result-icon    = icon_led_red.
      gs_result-msgtyp  = 'E'.
      gs_result-message = 'Cost Center is mandatory'.
      APPEND gs_result TO gt_result.
      DELETE gt_excel INDEX lv_index.
      CONTINUE.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form PROCESS_BDC
*&---------------------------------------------------------------------*
FORM process_bdc.
  DATA: lv_msg    TYPE char220,
        lv_subrc  TYPE sy-subrc,
        lv_sessid TYPE apqi-groupid.

  gv_total = lines( gt_excel ).

  " Open BDC session if session mode selected
  IF p_sess = abap_true AND p_test = abap_false.
    lv_sessid = 'KS02_UPLOAD'.
    CALL FUNCTION 'BDC_OPEN_GROUP'
      EXPORTING
        client              = sy-mandt
        group               = lv_sessid
        user                = sy-uname
        keep                = 'X'
      EXCEPTIONS
        client_invalid      = 1
        destination_invalid = 2
        group_invalid       = 3
        group_is_locked     = 4
        holddate_invalid    = 5
        internal_error      = 6
        queue_error         = 7
        running             = 8
        system_lock_error   = 9
        user_invalid        = 10
        OTHERS              = 11.
    IF sy-subrc <> 0.
      MESSAGE e001(00) WITH 'Error opening BDC session'.
    ENDIF.
  ENDIF.

  LOOP AT gt_excel INTO gs_excel.
    CLEAR: gs_result, lv_msg.

    gs_result-kokrs = gs_excel-kokrs.
    gs_result-kostl = gs_excel-kostl.
    gs_result-ktext = gs_excel-ktext.

    " Build BDC table
    PERFORM build_bdc_ks02 USING gs_excel.

    IF p_test = abap_true.
      " Test run - no execution
      gs_result-icon    = icon_led_yellow.
      gs_result-msgtyp  = 'I'.
      gs_result-message = 'Test Run - No Execution'.
      APPEND gs_result TO gt_result.
      REFRESH bdcdata.
      CONTINUE.
    ENDIF.

    IF p_sess = abap_true.
      " Insert into BDC session
      CALL FUNCTION 'BDC_INSERT'
        EXPORTING
          tcode     = 'KS02'
        TABLES
          dynprotab = bdcdata
        EXCEPTIONS
          internal_error  = 1
          not_open        = 2
          queue_error     = 3
          tcode_invalid   = 4
          printing_invalid = 5
          posting_invalid  = 6
          OTHERS          = 7.
      IF sy-subrc = 0.
        gs_result-icon    = icon_led_green.
        gs_result-msgtyp  = 'S'.
        gs_result-message = 'Added to BDC session KS02_UPLOAD'.
        gv_success = gv_success + 1.
      ELSE.
        gs_result-icon    = icon_led_red.
        gs_result-msgtyp  = 'E'.
        gs_result-message = 'Error inserting into BDC session'.
        gv_error = gv_error + 1.
      ENDIF.
      APPEND gs_result TO gt_result.
      REFRESH bdcdata.
      CONTINUE.
    ENDIF.

    " Call Transaction KS02
    REFRESH messtab.
    CALL TRANSACTION 'KS02' USING bdcdata
                             MODE p_mode
                             MESSAGES INTO messtab.
    lv_subrc = sy-subrc.

    " Evaluate messages
    PERFORM format_bdc_messages USING lv_subrc
                                CHANGING gs_result.

    APPEND gs_result TO gt_result.
    REFRESH bdcdata.
  ENDLOOP.

  " Close BDC session
  IF p_sess = abap_true AND p_test = abap_false.
    CALL FUNCTION 'BDC_CLOSE_GROUP'
      EXCEPTIONS
        not_open    = 1
        queue_error = 2
        OTHERS      = 3.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_BDC_KS02
*&---------------------------------------------------------------------*
FORM build_bdc_ks02 USING ps_data TYPE ty_excel_data.
  REFRESH bdcdata.

  " Screen 0100 - Initial Screen (only key fields)
  PERFORM bdc_dynpro USING 'SAPMKMA0' '0100'.
  PERFORM bdc_field  USING 'BDC_CURSOR'  'CSKS-KOSTL'.
  PERFORM bdc_field  USING 'BDC_OKCODE'  '/00'.
  PERFORM bdc_field  USING 'CSKS-KOKRS'  ps_data-kokrs.
  PERFORM bdc_field  USING 'CSKS-KOSTL'  ps_data-kostl.

  " Screen 0200 - Basic Data (only populate non-blank fields)
  PERFORM bdc_dynpro USING 'SAPMKMA0' '0200'.
  PERFORM bdc_field  USING 'BDC_CURSOR'  'CSKS-KTEXT'.
  PERFORM bdc_field  USING 'BDC_OKCODE'  '=SAVE'.

  IF ps_data-ktext IS NOT INITIAL.
    PERFORM bdc_field USING 'CSKS-KTEXT' ps_data-ktext.
  ENDIF.
  IF ps_data-ltext IS NOT INITIAL.
    PERFORM bdc_field USING 'CSKS-LTEXT' ps_data-ltext.
  ENDIF.
  IF ps_data-verak IS NOT INITIAL.
    PERFORM bdc_field USING 'CSKS-VERAK' ps_data-verak.
  ENDIF.
  IF ps_data-abtei IS NOT INITIAL.
    PERFORM bdc_field USING 'CSKS-ABTEI' ps_data-abtei.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form BDC_FIELD
*&---------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FORMAT_BDC_MESSAGES
*&---------------------------------------------------------------------*
FORM format_bdc_messages USING pv_subrc TYPE sy-subrc
                         CHANGING ps_result TYPE ty_result.
  DATA: lv_msg TYPE char220,
        lv_err TYPE abap_bool VALUE abap_false.

  " Check for errors in message table
  LOOP AT messtab WHERE msgtyp = 'E' OR msgtyp = 'A'.
    lv_err = abap_true.
    EXIT.
  ENDLOOP.

  IF lv_err = abap_true OR pv_subrc <> 0.
    " Error occurred
    ps_result-icon = icon_led_red.
    gv_error = gv_error + 1.

    " Get error message text
    LOOP AT messtab WHERE msgtyp = 'E' OR msgtyp = 'A'.
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = messtab-msgid
          lang      = sy-langu
          no        = messtab-msgnr
          v1        = messtab-msgv1
          v2        = messtab-msgv2
          v3        = messtab-msgv3
          v4        = messtab-msgv4
        IMPORTING
          msg       = lv_msg
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      ps_result-msgtyp  = messtab-msgtyp.
      ps_result-msgnr   = messtab-msgnr.
      ps_result-message = lv_msg.
      EXIT.
    ENDLOOP.
  ELSE.
    " Success
    ps_result-icon = icon_led_green.
    gv_success = gv_success + 1.

    " Get success message
    LOOP AT messtab WHERE msgtyp = 'S'.
      CALL FUNCTION 'FORMAT_MESSAGE'
        EXPORTING
          id        = messtab-msgid
          lang      = sy-langu
          no        = messtab-msgnr
          v1        = messtab-msgv1
          v2        = messtab-msgv2
          v3        = messtab-msgv3
          v4        = messtab-msgv4
        IMPORTING
          msg       = lv_msg
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      ps_result-msgtyp  = messtab-msgtyp.
      ps_result-msgnr   = messtab-msgnr.
      ps_result-message = lv_msg.
      EXIT.
    ENDLOOP.

    IF ps_result-message IS INITIAL.
      ps_result-msgtyp  = 'S'.
      ps_result-message = 'Cost Center changed successfully'.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM display_alv.
  DATA: lt_fcat   TYPE slis_t_fieldcat_alv,
        ls_fcat   TYPE slis_fieldcat_alv,
        ls_layout TYPE slis_layout_alv,
        lv_title  TYPE lvc_title.

  " Layout
  ls_layout-zebra      = 'X'.
  ls_layout-colwidth_optimize = 'X'.

  " Build fieldcatalog
  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ICON'.
  ls_fcat-seltext_l = 'Status'.
  ls_fcat-icon      = 'X'.
  ls_fcat-outputlen = 4.
  ls_fcat-col_pos   = 1.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'KOKRS'.
  ls_fcat-seltext_l = 'Ctrl Area'.
  ls_fcat-col_pos   = 2.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'KOSTL'.
  ls_fcat-seltext_l = 'Cost Center'.
  ls_fcat-col_pos   = 3.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'KTEXT'.
  ls_fcat-seltext_l = 'Name'.
  ls_fcat-col_pos   = 4.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'MSGTYP'.
  ls_fcat-seltext_l = 'Msg Type'.
  ls_fcat-col_pos   = 5.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'MSGNR'.
  ls_fcat-seltext_l = 'Msg No'.
  ls_fcat-col_pos   = 6.
  APPEND ls_fcat TO lt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'MESSAGE'.
  ls_fcat-seltext_l = 'Message'.
  ls_fcat-outputlen = 80.
  ls_fcat-col_pos   = 7.
  APPEND ls_fcat TO lt_fcat.

  " Title with counts
  WRITE gv_total   TO lv_title+0(5).
  WRITE gv_success TO lv_title+15(5).
  WRITE gv_error   TO lv_title+30(5).
  CONCATENATE 'Total:' lv_title(5)
              '| Success:' lv_title+15(5)
              '| Errors:' lv_title+30(5)
              INTO lv_title SEPARATED BY space.
  CONDENSE lv_title.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      is_layout          = ls_layout
      it_fieldcat        = lt_fcat
      i_grid_title       = lv_title
    TABLES
      t_outtab           = gt_result
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

  IF sy-subrc <> 0.
    MESSAGE e001(00) WITH 'Error displaying ALV'.
  ENDIF.
ENDFORM.
