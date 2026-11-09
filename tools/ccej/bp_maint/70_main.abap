*----------------------------------------------------------------------*
* LCL_APP - the selection screen
*----------------------------------------------------------------------*
CLASS lcl_app DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS init.
    CLASS-METHODS output.
    CLASS-METHODS pai.
    CLASS-METHODS run.
ENDCLASS.


CLASS lcl_app IMPLEMENTATION.

  METHOD init.
    bt_fld = 'Choose fields'.
  ENDMETHOD.

  METHOD output.
    cm_fld = |{ lines( s_fld ) } field(s) chosen - the keys are added automatically|.
    LOOP AT SCREEN.
      " Account group and grouping: creation only.
      IF screen-name = 'P_KTOGR' OR screen-name = 'P_BPGRP'.
        screen-input = COND #( WHEN p_crt = abap_true THEN '1' ELSE '0' ).
        MODIFY SCREEN.
      ENDIF.
      " Which fields and which records: download only. An upload takes its
      " fields from the file's first row.
      IF screen-name = 'BT_FLD' OR screen-name = 'P_MAX'
         OR screen-name CP 'S_NUM*' OR screen-name CP '%_S_NUM_*'
         OR screen-name CP 'S_BP*' OR screen-name CP '%_S_BP_*'
         OR screen-name CP 'S_BUKRS*' OR screen-name CP '%_S_BUKRS_*'
         OR screen-name CP 'S_ORG*' OR screen-name CP '%_S_ORG_*'.
        screen-input = COND #( WHEN p_down = abap_true THEN '1' ELSE '0' ).
        MODIFY SCREEN.
      ENDIF.
      " Test run: upload only.
      IF screen-name = 'P_TEST'.
        screen-input = COND #( WHEN p_up = abap_true THEN '1' ELSE '0' ).
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD pai.
    CASE sscrfields-ucomm.
      WHEN 'FLD'.
        lcl_sel=>choose( ).
        RETURN.
      WHEN 'ONLI' OR 'PRIN' OR 'SJOB'.
        " checked below
      WHEN OTHERS.
        " A radio button or Enter: nothing to complain about yet.
        RETURN.
    ENDCASE.

    IF p_crt = abap_true.
      IF p_ktogr IS INITIAL.
        MESSAGE 'Creation needs an account group' TYPE 'E'.
      ENDIF.
      IF lcl_cfg=>ok_ktogr( p_ktogr ) = abap_false.
        MESSAGE |Account group { p_ktogr } does not exist| TYPE 'E'.
      ENDIF.
      IF p_bpgrp IS INITIAL AND lcl_cfg=>bp_group( p_ktogr ) IS INITIAL AND p_up = abap_true.
        MESSAGE |Account group { p_ktogr } has no business partner grouping in CVI customising - give one| TYPE 'E'.
      ENDIF.
    ENDIF.
    IF p_down = abap_true AND s_fld[] IS INITIAL.
      MESSAGE 'Choose the fields for the file first' TYPE 'E'.
    ENDIF.
    IF p_file IS INITIAL.
      MESSAGE 'Give a file name - F4 opens the file dialog' TYPE 'E'.
    ENDIF.
    DATA(lv_name) = to_upper( CONV string( p_file ) ).
    IF p_xlsx = abap_true AND lv_name NP '*.XLSX'.
      MESSAGE 'Excel chosen - the file name must end in .xlsx' TYPE 'E'.
    ELSEIF p_txt = abap_true AND lv_name NP '*.TXT'.
      MESSAGE 'Text chosen - the file name must end in .txt' TYPE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD run.
    IF p_down = abap_true.
      lcl_dl=>run( ).
    ELSE.
      lcl_ul=>run( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Events
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_app=>init( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_app=>output( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_file=>f4( ).

AT SELECTION-SCREEN.
  lcl_app=>pai( ).

START-OF-SELECTION.
  lcl_app=>run( ).
