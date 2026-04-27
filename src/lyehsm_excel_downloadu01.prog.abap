FUNCTION YEHSM_ECEL_DOWN.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(WERKS) TYPE  WERKS_D OPTIONAL
*"     VALUE(AUTYP) TYPE  CHAR40 OPTIONAL
*"     VALUE(STDAT) TYPE  DATUM OPTIONAL
*"     VALUE(RCDAT) TYPE  DATUM OPTIONAL
*"     VALUE(OBSNO) TYPE  I DEFAULT 10
*"----------------------------------------------------------------------

  p_werks = WERKS.
  p_autyp = AUTYP.
  p_stdat = STDAT.
  p_rcdat = RCDAT.
  p_obsno = OBSNO.

* Fetch locations from YEHS_LOCATIONS for the selected plant
  SELECT plant location sub_location
    FROM yehs_locations
    INTO TABLE lt_locations
    WHERE plant = p_werks.

* Build the location dropdown string (comma-separated)
  DATA: lv_loc_dropdown TYPE string.
  LOOP AT lt_locations INTO DATA(ls_loc).
    IF lv_loc_dropdown IS INITIAL.
      lv_loc_dropdown = ls_loc-locid && | | && ls_loc-locdesc.
    ELSE.
      lv_loc_dropdown = lv_loc_dropdown && |,| && ls_loc-locid && | | && ls_loc-locdesc.
    ENDIF.
  ENDLOOP.

  IF lv_loc_dropdown IS INITIAL.
    lv_loc_dropdown = ' '.
  ENDIF.

* Format dates for Excel (d-mmm-yyyy)
  DATA: lv_stdate TYPE string,
        lv_rcdate TYPE string.

  PERFORM format_date USING p_stdat CHANGING lv_stdate.
  PERFORM format_date USING p_rcdat CHANGING lv_rcdate.

* Build the Excel XML
  PERFORM build_excel_xml USING p_werks
                                p_autyp
                                lv_stdate
                                lv_rcdate
                                p_obsno
                                lv_loc_dropdown
                         CHANGING lv_xml.

* Convert string to xstring
  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = lv_xml
    IMPORTING
      buffer = lv_xstring
    EXCEPTIONS
      OTHERS = 1.

  IF sy-subrc <> 0.
    MESSAGE 'Error converting XML string' TYPE 'E'.
    RETURN.
  ENDIF.

* File save dialog - .xls extension for direct Excel open
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      default_extension = 'xls'
      default_file_name = |EHSM_Audit_{ p_werks }_{ sy-datum }.xls|
      file_filter       = 'Excel Files (*.xls) *.xls'
    CHANGING
      filename          = lv_filename
      path              = lv_path
      fullpath          = lv_fullpath
      user_action       = lv_action.

  IF lv_action <> cl_gui_frontend_services=>action_ok.
    MESSAGE 'Download cancelled by user' TYPE 'S'.
    RETURN.
  ENDIF.

* Convert to binary
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = lv_xstring
    IMPORTING
      output_length = lv_size
    TABLES
      binary_tab    = lt_binary.

* Download
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      bin_filesize = lv_size
      filename     = lv_fullpath
      filetype     = 'BIN'
    CHANGING
      data_tab     = lt_binary
    EXCEPTIONS
      OTHERS       = 1.

ENDFUNCTION.
