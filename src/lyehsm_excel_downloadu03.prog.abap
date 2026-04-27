FUNCTION YEHSM_ECEL_DATA_VALID.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IS_AUDIT_HEADER) TYPE  YSROOT1_D
*"  EXPORTING
*"     REFERENCE(EV_SAVE) TYPE  FLAG
*"  TABLES
*"      IT_EXCEL_DATA TYPE  YTTEHS_EXCEL_D
*"----------------------------------------------------------------------

  DATA: lv_rcdate TYPE datum,
        lv_aufrom TYPE datum,
        ls_audit_header TYPE YSROOT1_D.

  ls_audit_header = is_audit_header.

  LOOP AT it_excel_data INTO DATA(ls_upload).

    CLEAR: lv_rcdate, lv_aufrom.
    IF ls_upload-impl01 IS INITIAL OR ls_upload-obstext IS INITIAL
       OR ls_upload-refno IS INITIAL OR ls_upload-tgtdate IS INITIAL
       OR ls_upload-priority IS INITIAL
       OR ls_upload-rcdate IS INITIAL OR ls_upload-autype IS INITIAL.
      ev_save = abap_true.
      MESSAGE : 'Excel is incomplete : enter all the fields' TYPE 'E'.
      EXIT.
    ELSE.
      CALL FUNCTION 'CONVERT_DATE_INPUT'
        EXPORTING
          input  = ls_upload-rcdate
        IMPORTING
          output = lv_rcdate
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc <> 0.
        MESSAGE 'Invalid RC Date format' TYPE 'E'.
        EXIT.
      ENDIF.
      IF lv_rcdate GT sy-datum.
        ev_save = abap_true.
        MESSAGE 'AUDIT/ACTIVITY REPORT RECEIVED DATE CANNOT BE IN FUTURE' TYPE 'E'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'CONVERT_DATE_INPUT'
        EXPORTING
          input  = ls_upload-rcdate
        IMPORTING
          output = lv_aufrom
        EXCEPTIONS
          OTHERS = 1.
      IF lv_aufrom GT sy-datum.
        ev_save = abap_true.
        MESSAGE 'AUDIT/ACTIVITY FROM DATE CAN NOT BE IN FUTURE' TYPE 'E'.
        EXIT.
      ENDIF.

      IF lv_aufrom GT ls_audit_header-audit_to.
        ev_save = abap_true.
        MESSAGE 'AUDIT/ACTIVITY FROM-DATE CAN NOT BE GREATER THAN AUDIT-TO DATE' TYPE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    CLEAR ls_upload.
  ENDLOOP.

ENDFUNCTION.
