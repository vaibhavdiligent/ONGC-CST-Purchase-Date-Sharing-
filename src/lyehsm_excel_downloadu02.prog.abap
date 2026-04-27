FUNCTION YEHSM_ECEL_UPLOAD.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_EXCEL_DATA TYPE  YTTEHS_EXCEL_D OPTIONAL
*"----------------------------------------------------------------------

  data p_file TYPE rlgrap-filename.

  TYPES: BEGIN OF ty_upload,
           sno      TYPE i,
           plant    TYPE werks_d,
           autype   TYPE char40,
           aufrom   TYPE char20,
           rcdate   TYPE char20,
           refno    TYPE char40,
           priority TYPE char10,
           tgtdate  TYPE char20,
           location TYPE char40,
           subloc   TYPE char40,
           obstext  TYPE char1024,
           impl01   TYPE char10,
           impl02   TYPE char10,
           impl03   TYPE char10,
           impl04   TYPE char10,
           impl05   TYPE char10,
           impl06   TYPE char10,
           impl07   TYPE char10,
           impl08   TYPE char10,
           impl09   TYPE char10,
           impl10   TYPE char10,
           impl11   TYPE char10,
           impl12   TYPE char10,
           impl13   TYPE char10,
           impl14   TYPE char10,
           impl15   TYPE char10,
         END OF ty_upload.

  TYPES: BEGIN OF ty_raw,
           row TYPE i,
           col TYPE i,
           val TYPE char1024,
         END OF ty_raw.

  DATA: lt_upload TYPE TABLE OF ty_upload,
        ls_upload TYPE ty_upload,
        lt_raw    TYPE TABLE OF ty_raw,
        ls_raw    TYPE ty_raw,
        lt_intern TYPE TABLE OF alsmex_tabline,
        ls_intern TYPE alsmex_tabline,
        lv_file   TYPE rlgrap-filename,
        lv_rc     TYPE i.

  DATA: lt_filetab TYPE filetable,
        lv_subrc   TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title = 'Select Excel File'
      file_filter  = 'Excel Files (*.xls;*.xlsx) *.xls;*.xlsx'
    CHANGING
      file_table   = lt_filetab
      rc           = lv_subrc.

  IF lv_subrc = 1.
    READ TABLE lt_filetab INTO DATA(ls_file) INDEX 1.
    IF sy-subrc = 0.
      p_file = ls_file-filename.
    ENDIF.
  ENDIF.

* Upload Excel using ALSM_EXCEL_TO_INTERNAL_TABLE
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename              = p_file
      i_begin_col           = 1
      i_begin_row           = 2
      i_end_col             = 26
      i_end_row             = 9999
    TABLES
      intern                = lt_intern
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole            = 2
      OTHERS                = 3.

  IF sy-subrc <> 0.
    MESSAGE |Error uploading Excel file. SY-SUBRC = { sy-subrc }| TYPE 'E'.
    RETURN.
  ENDIF.

  IF lt_intern IS INITIAL.
    MESSAGE 'No data found in Excel file' TYPE 'E'.
    RETURN.
  ENDIF.

* Convert ALSM format to raw table
  LOOP AT lt_intern INTO ls_intern.
    ls_raw-row = ls_intern-row.
    ls_raw-col = ls_intern-col.
    ls_raw-val = ls_intern-value.
    APPEND ls_raw TO lt_raw.
  ENDLOOP.

* Build internal table from raw data
  DATA: lv_prev_row TYPE i VALUE 0.

  SORT lt_raw BY row col.

  LOOP AT lt_raw INTO ls_raw.

*   New row - save previous and clear
    IF lv_prev_row <> 0 AND ls_raw-row <> lv_prev_row.
      IF ls_upload-plant IS NOT INITIAL.
        APPEND ls_upload TO lt_upload.
      ENDIF.
      CLEAR ls_upload.
    ENDIF.

    lv_prev_row = ls_raw-row.

*   Map columns to structure fields
    CASE ls_raw-col.
      WHEN 1.  ls_upload-sno      = ls_raw-val.
      WHEN 2.  ls_upload-plant    = ls_raw-val.
      WHEN 3.  ls_upload-autype   = ls_raw-val.
      WHEN 4.  ls_upload-aufrom   = ls_raw-val.
      WHEN 5.  ls_upload-rcdate   = ls_raw-val.
      WHEN 6.  ls_upload-refno    = ls_raw-val.
      WHEN 7.  ls_upload-priority = ls_raw-val.
      WHEN 8.  ls_upload-tgtdate  = ls_raw-val.
      WHEN 9.  ls_upload-location = ls_raw-val.
      WHEN 10. ls_upload-subloc   = ls_raw-val.
      WHEN 11. ls_upload-obstext  = ls_raw-val.
      WHEN 12. ls_upload-impl01   = ls_raw-val.
      WHEN 13. ls_upload-impl02   = ls_raw-val.
      WHEN 14. ls_upload-impl03   = ls_raw-val.
      WHEN 15. ls_upload-impl04   = ls_raw-val.
      WHEN 16. ls_upload-impl05   = ls_raw-val.
      WHEN 17. ls_upload-impl06   = ls_raw-val.
      WHEN 18. ls_upload-impl07   = ls_raw-val.
      WHEN 19. ls_upload-impl08   = ls_raw-val.
      WHEN 20. ls_upload-impl09   = ls_raw-val.
      WHEN 21. ls_upload-impl10   = ls_raw-val.
      WHEN 22. ls_upload-impl11   = ls_raw-val.
      WHEN 23. ls_upload-impl12   = ls_raw-val.
      WHEN 24. ls_upload-impl13   = ls_raw-val.
      WHEN 25. ls_upload-impl14   = ls_raw-val.
      WHEN 26. ls_upload-impl15   = ls_raw-val.
    ENDCASE.

  ENDLOOP.

* Append last row
  IF ls_upload-plant IS NOT INITIAL.
    APPEND ls_upload TO lt_upload.
  ENDIF.

* Append last row to global table
  IF ls_upload-plant IS NOT INITIAL.
    APPEND ls_upload TO gt_upload.
  ENDIF.

* Display count
  DATA(lv_count) = lines( gt_upload ).

* Map to output table
  IF lt_upload IS NOT INITIAL.
    data ls_YTTEHS_EXCEL_D type YSEHS_EXCEL_D.
    loop at lt_upload INTO ls_upload.
      MOVE-CORRESPONDING ls_upload to ls_YTTEHS_EXCEL_D.
      append ls_YTTEHS_EXCEL_D to it_excel_data.
    ENDLOOP.
  ELSE.
    MESSAGE 'No valid data found in Excel file' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFUNCTION.
