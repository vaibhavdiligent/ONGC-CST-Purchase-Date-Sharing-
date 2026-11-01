*&---------------------------------------------------------------------*
*& Report  ZSD_S911_UPLOAD_DATA
*&---------------------------------------------------------------------*
*& Manual upload of Gx spill rows into info structure S911 (Excel file)
*&---------------------------------------------------------------------*
*& 01.11.2026  S/4 CRS remediation (performance / ATC / robustness):
*&   - Column 41 (UMNET) was never transferred: second WHEN '0040' was
*&     unreachable -> corrected to WHEN '0041'.
*&   - Excel read errors were ignored (empty IF after the FM call) and
*&     the program ended silently -> explicit E message with the cause.
*&   - ALSM_EXCEL_TO_INTERNAL_TABLE needs SAP GUI / OLE: running the
*&     program in background (SM36) is not possible -> explicit message
*&     instead of a cancelled job.
*&   - Numeric and date cells are converted with error handling: a bad
*&     cell no longer dumps (CONVT_NO_NUMBER) and the row/column is listed.
*&   - Database update changed from MODIFY per row + S message per row
*&     to one mass MODIFY + COMMIT WORK with a single result message.
*&   - Obsolete OCCURS / header-line table replaced (ATC).
*&   - Authority check S_TABU_NAM / S911 change added (ATC).
*&---------------------------------------------------------------------*
REPORT zsd_s911_upload_data.

*&---------------------------------------------------------------------*
*& Structure Declaration
*&---------------------------------------------------------------------*
TYPES : BEGIN OF ty_upload,
          ssour      TYPE ssour,
          vrsio      TYPE vrsio,
          spmon      TYPE spmon,
          sptag      TYPE sptag,
          spwoc      TYPE spwoc,
          spbup      TYPE spbup,
          vkorg      TYPE vkorg,
          vtweg      TYPE vtweg,
          fkart      TYPE fkart,
          inco1      TYPE inco1,
          kvgr1      TYPE kvgr1,
          pkunag     TYPE kunag,
          matnr      TYPE matnr,
          ip_vend    TYPE /irm/ip_vendor,
          plaag_01   TYPE plaag,

          periv      TYPE periv,
          vwdat      TYPE vwdat,
          vrkme      TYPE vrkme,
          waerk      TYPE waerk,
          stwae      TYPE stwae,
          basme      TYPE meins,
          werks      TYPE werks_d,
          vkbur      TYPE vkbur,
          kdgrp      TYPE kdgrp,
          plawe      TYPE plawe,
          kursk      TYPE kursk,
          sakn1      TYPE saknr,
          spart      TYPE spart,
          spara      TYPE spart_ak,
          katr3      TYPE katr3,

          katr4      TYPE katr4,
          fkimg      TYPE fkimg,
          netwr      TYPE netwr_fp,
          skfbp      TYPE skfbp,
          wavwr      TYPE wavwr,
          mwsbp      TYPE mwsbp,
          umkzwi6    TYPE mc_umkzwi6,
          umkzwi5    TYPE mc_umkzwi5,
          fklmg      TYPE fklmg,
          "Code Remediations Begin of changes by ABAP7
*          irm_slsamt TYPE /irm/br_totamt,
          irm_slsamt TYPE belwer,
          "Code Remediations end of changes by ABAP7
          umnet      TYPE /bev4/plumnet,
        END OF ty_upload.

TYPES : BEGIN OF ty_err,
          row  TYPE kcd_ex_row_n,
          col  TYPE kcd_ex_col_n,
          val  TYPE kcd_value,
          text TYPE c LENGTH 60,
        END OF ty_err.

CONSTANTS : c_last_col TYPE i VALUE 41.

*&---------------------------------------------------------------------*
*& Internal Table Declaration
*&---------------------------------------------------------------------*
DATA : t_upload TYPE STANDARD TABLE OF ty_upload.
DATA : t_intern TYPE STANDARD TABLE OF alsmex_tabline.
DATA : t_s911   TYPE STANDARD TABLE OF s911.
DATA : t_err    TYPE STANDARD TABLE OF ty_err.
*&---------------------------------------------------------------------*
*& Work Area Declaration
*&---------------------------------------------------------------------*
DATA : w_upload TYPE ty_upload.
DATA : w_intern TYPE alsmex_tabline.
DATA : w_s911   TYPE s911.
DATA : w_err    TYPE ty_err.

*&---------------------------------------------------------------------*
*& Selection Screen Declaration
*&---------------------------------------------------------------------*
SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS : p_fname TYPE rlgrap-filename OBLIGATORY,
             p_indic TYPE char1.
SELECTION-SCREEN : END OF BLOCK b1.
*&---------------------------------------------------------------------*
*& At Selection Screen Declaration
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'P_FNAME'
    IMPORTING
      file_name     = p_fname.

*&---------------------------------------------------------------------*
*& Start of selection Declaration
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM sub_check_prereq.

  PERFORM sub_read_upload_data.

  PERFORM sub_transfer_data.

  PERFORM sub_process_data.

*&---------------------------------------------------------------------*
*&      Form  SUB_CHECK_PREREQ
*&---------------------------------------------------------------------*
*  Excel is read through OLE on the SAP GUI front end: no background
*  mode. Direct update of S911 requires table change authorization.
*----------------------------------------------------------------------*
FORM sub_check_prereq .
  IF sy-batch = abap_true.
    MESSAGE 'ZSD_S911_UPLOAD_DATA reads Excel via SAP GUI; run it online, not as a background job'
      TYPE 'E'.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'S_TABU_NAM'
    ID 'ACTVT' FIELD '02'
    ID 'TABLE' FIELD 'S911'.
  IF sy-subrc <> 0.
    MESSAGE 'No authorization to change table S911 (S_TABU_NAM)' TYPE 'E'.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SUB_READ_UPLOAD_DATA
*&---------------------------------------------------------------------*
FORM sub_read_upload_data .
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_fname
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = c_last_col
      i_end_row               = 999999
    TABLES
      intern                  = t_intern
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  CASE sy-subrc.
    WHEN 0.
    WHEN 1.
      MESSAGE 'Excel read failed: inconsistent parameters' TYPE 'E'.
    WHEN 2.
      MESSAGE 'Excel read failed (OLE): check file path, file open in Excel, or GUI/Excel installation'
        TYPE 'E'.
    WHEN OTHERS.
      MESSAGE 'Excel read failed' TYPE 'E'.
  ENDCASE.

  IF t_intern IS INITIAL.
    MESSAGE 'No data rows found in the file (data expected from row 2, columns 1-41)' TYPE 'E'.
  ENDIF.

  SORT t_intern BY row col.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SUB_TRANSFER_DATA
*&---------------------------------------------------------------------*
FORM sub_transfer_data .
  DATA : lv_row TYPE kcd_ex_row_n.

  LOOP AT t_intern INTO w_intern.
    IF lv_row IS INITIAL.
      lv_row = w_intern-row.
    ENDIF.
    CASE w_intern-col.
      WHEN '0001'.
        w_upload-ssour = w_intern-value.
      WHEN '0002'.
        w_upload-vrsio = w_intern-value.
      WHEN '0003'.
        CALL FUNCTION 'CONVERSION_EXIT_BCVMN_INPUT'
          EXPORTING
            input  = w_intern-value
          IMPORTING
            output = w_upload-spmon.
      WHEN '0004'.
        PERFORM sub_conv_date USING w_intern CHANGING w_upload-sptag.
      WHEN '0005'.
        w_upload-spwoc = w_intern-value.
      WHEN '0006'.
        w_upload-spbup = w_intern-value.
      WHEN '0007'.
        w_upload-vkorg = w_intern-value.
      WHEN '0008'.
        w_upload-vtweg = w_intern-value.
      WHEN '0009'.
        w_upload-fkart = w_intern-value.
      WHEN '0010'.
        w_upload-inco1 = w_intern-value.
      WHEN '0011'.
        w_upload-kvgr1 = w_intern-value.
      WHEN '0012'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = w_intern-value
          IMPORTING
            output = w_upload-pkunag.
      WHEN '0013'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = w_intern-value
          IMPORTING
            output = w_upload-matnr.
      WHEN '0014'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = w_intern-value
          IMPORTING
            output = w_upload-ip_vend.
      WHEN '0015'.
        w_upload-plaag_01 = w_intern-value.
      WHEN '0016'.
        w_upload-periv = w_intern-value.
      WHEN '0017'.
        PERFORM sub_conv_date USING w_intern CHANGING w_upload-vwdat.
      WHEN '0018'.
        w_upload-vrkme = w_intern-value.
      WHEN '0019'.
        w_upload-waerk = w_intern-value.
      WHEN '0020'.
        w_upload-stwae = w_intern-value.
      WHEN '0021'.
        w_upload-basme = w_intern-value.
      WHEN '0022'.
        w_upload-werks = w_intern-value.
      WHEN '0023'.
        w_upload-vkbur = w_intern-value.
      WHEN '0024'.
        w_upload-kdgrp = w_intern-value.
      WHEN '0025'.
        w_upload-plawe = w_intern-value.
      WHEN '0026'.
        PERFORM sub_conv_num USING w_intern CHANGING w_upload-kursk.
      WHEN '0027'.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = w_intern-value
          IMPORTING
            output = w_upload-sakn1.
      WHEN '0028'.
        w_upload-spart = w_intern-value.
      WHEN '0029'.
        w_upload-spara = w_intern-value.
      WHEN '0030'.
        w_upload-katr3 = w_intern-value.
      WHEN '0031'.
        w_upload-katr4 = w_intern-value.
      WHEN '0032'.
        PERFORM sub_conv_num USING w_intern CHANGING w_upload-fkimg.
      WHEN '0033'.
        PERFORM sub_conv_num USING w_intern CHANGING w_upload-netwr.
      WHEN '0034'.
        PERFORM sub_conv_num USING w_intern CHANGING w_upload-skfbp.
      WHEN '0035'.
        PERFORM sub_conv_num USING w_intern CHANGING w_upload-wavwr.
      WHEN '0036'.
        PERFORM sub_conv_num USING w_intern CHANGING w_upload-mwsbp.
      WHEN '0037'.
        PERFORM sub_conv_num USING w_intern CHANGING w_upload-umkzwi6.
      WHEN '0038'.
        PERFORM sub_conv_num USING w_intern CHANGING w_upload-umkzwi5.
      WHEN '0039'.
        PERFORM sub_conv_num USING w_intern CHANGING w_upload-fklmg.
      WHEN '0040'.
        PERFORM sub_conv_num USING w_intern CHANGING w_upload-irm_slsamt.
      WHEN '0041'.                      " was WHEN '0040' twice -> UMNET never filled
        PERFORM sub_conv_num USING w_intern CHANGING w_upload-umnet.
    ENDCASE.
    AT END OF row.
      APPEND w_upload TO t_upload.
      CLEAR w_upload.
    ENDAT.
  ENDLOOP.
  SORT t_upload.
  DELETE ADJACENT DUPLICATES FROM t_upload COMPARING ALL FIELDS.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SUB_CONV_NUM
*&---------------------------------------------------------------------*
*  Excel delivers cell text; '1,234.50' or blanks dump on direct
*  assignment to a packed field. Convert safely, log bad cells.
*----------------------------------------------------------------------*
FORM sub_conv_num USING    is_cell TYPE alsmex_tabline
                  CHANGING cv_num  TYPE any.
  DATA : lv_val TYPE string.

  lv_val = is_cell-value.
  CONDENSE lv_val NO-GAPS.
  IF lv_val IS INITIAL.
    CLEAR cv_num.
    RETURN.
  ENDIF.
  REPLACE ALL OCCURRENCES OF ',' IN lv_val WITH ''.
  TRY.
      cv_num = lv_val.
    CATCH cx_sy_conversion_no_number cx_sy_conversion_overflow cx_sy_arithmetic_overflow.
      CLEAR cv_num.
      w_err-row  = is_cell-row.
      w_err-col  = is_cell-col.
      w_err-val  = is_cell-value.
      w_err-text = 'Not a valid number'.
      APPEND w_err TO t_err.
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SUB_CONV_DATE
*&---------------------------------------------------------------------*
*  Accepts YYYYMMDD or the user's display format (e.g. 31.12.2025).
*----------------------------------------------------------------------*
FORM sub_conv_date USING    is_cell TYPE alsmex_tabline
                   CHANGING cv_date TYPE d.
  DATA : lv_val TYPE string,
         lv_ext TYPE c LENGTH 10.

  lv_val = is_cell-value.
  CONDENSE lv_val NO-GAPS.
  CLEAR cv_date.
  IF lv_val IS INITIAL.
    RETURN.
  ENDIF.
  IF strlen( lv_val ) = 8 AND lv_val CO '0123456789'.
    cv_date = lv_val.
    RETURN.
  ENDIF.
  lv_ext = lv_val.
  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
    EXPORTING
      date_external            = lv_ext
    IMPORTING
      date_internal            = cv_date
    EXCEPTIONS
      date_external_is_invalid = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    CLEAR cv_date.
    w_err-row  = is_cell-row.
    w_err-col  = is_cell-col.
    w_err-val  = is_cell-value.
    w_err-text = 'Not a valid date'.
    APPEND w_err TO t_err.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SUB_PROCESS_DATA
*&---------------------------------------------------------------------*
*  Mass update of S911 (one MODIFY, one COMMIT) instead of one MODIFY
*  and one message per row. Rows are rejected as a whole if any cell
*  failed conversion, so a partially converted file is never posted.
*----------------------------------------------------------------------*
FORM sub_process_data .
  DATA : lv_count TYPE n LENGTH 5,
         lv_lines TYPE i.

  IF t_err IS NOT INITIAL.
    SORT t_err BY row col.
    WRITE : / 'Upload NOT posted. Invalid cells in file', p_fname.
    WRITE : / 'Row', 10 'Col', 20 'Value', 60 'Problem'.
    ULINE.
    LOOP AT t_err INTO w_err.
      WRITE : / w_err-row, 10 w_err-col, 20 w_err-val(38), 60 w_err-text.
    ENDLOOP.
    MESSAGE 'File contains invalid cells; nothing updated. See list.' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_upload INTO w_upload.
    lv_count = lv_count + 1.
    MOVE-CORRESPONDING w_upload TO w_s911.
    CONCATENATE p_indic lv_count INTO w_s911-spwoc.
    CONDENSE w_s911-spwoc.
    w_s911-mandt = sy-mandt.
    APPEND w_s911 TO t_s911.
    CLEAR w_s911.
  ENDLOOP.

  IF t_s911 IS INITIAL.
    MESSAGE 'No rows to update' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  MODIFY s911 FROM TABLE t_s911.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
    lv_lines = lines( t_s911 ).
    MESSAGE |S911 updated: { lv_lines } rows, indicator { p_indic }| TYPE 'S'.
  ELSE.
    ROLLBACK WORK.
    MESSAGE 'Update of S911 failed; no rows posted' TYPE 'E'.
  ENDIF.
ENDFORM.
