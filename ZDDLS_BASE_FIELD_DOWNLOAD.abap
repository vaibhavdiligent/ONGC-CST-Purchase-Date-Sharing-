*&---------------------------------------------------------------------*
*& Program: ZDDLS_BASE_FIELD_DOWNLOAD
*& Purpose: Fetch all DDLS successor entries from ARS_API_SUCCESSOR,
*&          call CL_DD_DDL_FIELD_TRACKER=>GET_BASE_FIELD_INFORMATION
*&          for each DDL source view name, accumulate the results and
*&          download the combined output to a local PC file.
*&---------------------------------------------------------------------*
REPORT zddls_base_field_download.

*----------------------------------------------------------------------*
* Data Declarations
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_successor,
         tadir_obj_name          TYPE ars_api_successor-tadir_obj_name,
         successor_tadir_obj_na  TYPE ars_api_successor-successor_tadir_obj_na,
       END OF ty_successor.

TYPES: BEGIN OF ts_base_field,
         entity_name   TYPE dd_cds_entity_name,
         element_name  TYPE fieldname,
         base_object   TYPE objectname,
         base_field    TYPE fieldname,
         is_calculated TYPE dd_cds_calculated,
       END OF ts_base_field.
TYPES: tt_base_fields TYPE SORTED TABLE OF ts_base_field
                      WITH UNIQUE KEY entity_name element_name.

TYPES: BEGIN OF ty_output,
         ddl_name      TYPE viewname,
         entity_name   TYPE dd_cds_entity_name,
         element_name  TYPE fieldname,
         base_object   TYPE objectname,
         base_field    TYPE fieldname,
         is_calculated TYPE dd_cds_calculated,
       END OF ty_output.

DATA: lt_successor  TYPE STANDARD TABLE OF ty_successor,
      lt_base_fields TYPE tt_base_fields,        " return table from class method
      lt_all_output  TYPE STANDARD TABLE OF ty_output,
      ls_output      TYPE ty_output,
      lt_file_lines  TYPE STANDARD TABLE OF string,
      lv_line        TYPE string,
      lv_file        TYPE string,
      lv_path        TYPE string,
      lv_fullpath    TYPE string,
      lv_ddlname     TYPE viewname,
      lv_total       TYPE i,
      lv_processed   TYPE i,
      lv_msg         TYPE string.

DATA: l_cl_ddl TYPE REF TO cl_dd_ddl_field_tracker.

FIELD-SYMBOLS: <fs_succ>   TYPE ty_successor,
               <fs_field>  TYPE LINE OF tt_base_fields,
               <fs_out>    TYPE ty_output.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE text-001.
  PARAMETERS: p_path TYPE string LOWER CASE.
SELECTION-SCREEN END OF BLOCK sel.

*----------------------------------------------------------------------*
* F4 help for output file path
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  DATA: lv_f4_file TYPE string,
        lv_f4_path TYPE string,
        lv_f4_full TYPE string.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Select output file'
      default_file_name = 'DDLS_BASE_FIELDS.txt'
      default_extension = 'txt'
      file_filter       = 'Text Files (*.txt)|*.txt|CSV Files (*.csv)|*.csv|All Files (*.*)|*.*|'
    CHANGING
      filename          = lv_f4_file
      path              = lv_f4_path
      fullpath          = lv_f4_full
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc = 0 AND lv_f4_full IS NOT INITIAL.
    p_path = lv_f4_full.
  ENDIF.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Determine output file path
  IF p_path IS INITIAL.
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        default_file_name = 'DDLS_BASE_FIELDS.txt'
        default_extension = 'txt'
        file_filter       = 'Text Files (*.txt)|*.txt|CSV Files (*.csv)|*.csv|All Files (*.*)|*.*|'
      CHANGING
        filename          = lv_file
        path              = lv_path
        fullpath          = lv_fullpath
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.
    IF sy-subrc <> 0 OR lv_fullpath IS INITIAL.
      MESSAGE 'No file selected. Program cancelled.' TYPE 'I'.
      LEAVE PROGRAM.
    ENDIF.
    lv_file = lv_fullpath.
  ELSE.
    lv_file = p_path.
  ENDIF.

* Fetch all rows from ARS_API_SUCCESSOR where TADIR_OBJECT = 'TABL'
* and SUCCESSOR_TADIR_OBJECT = 'DDLS'
  SELECT tadir_obj_name
         successor_tadir_obj_na
    INTO TABLE lt_successor
    FROM ars_api_successor
    WHERE tadir_object           = 'TABL'
      AND successor_tadir_object = 'DDLS'.

  IF sy-subrc <> 0 OR lt_successor IS INITIAL.
    MESSAGE 'No DDLS successor entries found in ARS_API_SUCCESSOR.' TYPE 'I'.
    LEAVE PROGRAM.
  ENDIF.

  lv_total = lines( lt_successor ).
  WRITE: / 'Total DDLS entries found:', lv_total.
  ULINE.

* Write header line
  lv_line = 'DDL_NAME|ENTITY_NAME|ELEMENT_NAME|BASE_OBJECT|BASE_FIELD|IS_CALCULATED'.
  APPEND lv_line TO lt_file_lines.

* Loop over each DDLS entry, instantiate tracker class and collect fields
  lv_processed = 0.

  LOOP AT lt_successor ASSIGNING <fs_succ>.
    lv_ddlname = <fs_succ>-successor_tadir_obj_na.

    TRY.
        CREATE OBJECT l_cl_ddl
          EXPORTING
            iv_ddlname = lv_ddlname.

        CLEAR lt_base_fields.
        l_cl_ddl->get_base_field_information(
          RECEIVING
            rt_base_fields = lt_base_fields ).

        LOOP AT lt_base_fields ASSIGNING <fs_field>.
          CLEAR ls_output.
          ls_output-ddl_name     = lv_ddlname.
              ls_output-entity_name   = <fs_field>-entity_name.
          ls_output-element_name  = <fs_field>-element_name.
          ls_output-base_object   = <fs_field>-base_object.
          ls_output-base_field    = <fs_field>-base_field.
          ls_output-is_calculated = <fs_field>-is_calculated.
          APPEND ls_output TO lt_all_output.
        ENDLOOP.

      CATCH cx_dd_ddl_read INTO DATA(lx_read).
        WRITE: / 'Warning: Could not read DDL', lv_ddlname, '-', lx_read->get_text( ).
    ENDTRY.

    lv_processed = lv_processed + 1.
    WRITE: / 'Processed', lv_processed, '/', lv_total, '- DDL:', lv_ddlname.
  ENDLOOP.

  ULINE.
  WRITE: / 'Total base field entries collected:', lines( lt_all_output ).
  ULINE.

* Build output lines for download
  LOOP AT lt_all_output ASSIGNING <fs_out>.
    CONCATENATE <fs_out>-ddl_name
                <fs_out>-entity_name
                <fs_out>-element_name
                <fs_out>-base_object
                <fs_out>-base_field
                <fs_out>-is_calculated
           INTO lv_line
           SEPARATED BY '|'.
    APPEND lv_line TO lt_file_lines.
  ENDLOOP.

* Download to PC
  CALL METHOD cl_gui_frontend_services=>gui_download
    EXPORTING
      filename         = lv_file
      filetype         = 'ASC'
      append           = ' '
      codepage         = '4110'
      write_bom        = 'X'
    CHANGING
      data_tab         = lt_file_lines
    EXCEPTIONS
      file_write_error = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    CONCATENATE 'File write error. Path:' lv_file INTO lv_msg SEPARATED BY space.
    MESSAGE lv_msg TYPE 'E'.
  ELSE.
    CONCATENATE 'Download complete.' lines( lt_all_output )
                'records written to:' lv_file
           INTO lv_msg SEPARATED BY space.
    MESSAGE lv_msg TYPE 'S'.
    WRITE: / lv_msg.
  ENDIF.
