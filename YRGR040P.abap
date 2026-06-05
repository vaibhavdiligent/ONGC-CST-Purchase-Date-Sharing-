*&---------------------------------------------------------------------*
*& Report  YRGR040P
*& Purchase Nomination Upload, Allocation & Ticket Posting
*&---------------------------------------------------------------------*
*& Created   : 24.07.2023  Karavadi Ravi Chandra
*& Modified  : 28.05.2026  XSAP_AB2
*& Reference : YRGG015_PURC_NOM_ONGC_B2B
*&---------------------------------------------------------------------*
REPORT yrgr040p MESSAGE-ID oo
                LINE-SIZE 255
                NO STANDARD PAGE HEADING.

TABLES: oijnomi.

TYPE-POOLS: icon.

*----------------------------------------------------------------------*
* TYPE DECLARATIONS
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_main,
         tsyst       TYPE oij_tsyst,
         vbeln       TYPE vbeln,
         date        TYPE sy-datum,
         locid       TYPE oij_locid,
         matnr       TYPE matnr,
         menge       TYPE oij_menge,
         unit        TYPE oij_uniti,
         charg       TYPE charg_d,
         rank        TYPE i,
         ancv        TYPE yyncv,
         agcv        TYPE yygcv,
         nomtk       TYPE oij_nomtk,
         nomit       TYPE oij_item,
         st_qty      TYPE oijnomi-yyoij_dpimb_qty,
         del_ind     TYPE char1,
         flag        TYPE char1,
         post_status TYPE char30,
         ticketnr    TYPE oij_tktnr,
         ticket_key  TYPE oij_el_tkt_key,
         ticket_item TYPE oij_el_tkt_posnr,
         color(4)    TYPE c,
         error_msg(170) TYPE c,
       END OF ty_main.

TYPES: BEGIN OF ty_main_r,
         date   TYPE sy-datum,
         locid  TYPE oij_locid,
         rank   TYPE i,
         nomtk  TYPE oij_nomtk,
         nomit  TYPE oij_item,
       END OF ty_main_r.

TYPES: BEGIN OF ty_message,
         id      TYPE symsgid,
         number  TYPE symsgno,
         message TYPE char200,
       END OF ty_message.

TYPES: BEGIN OF ty_temp,
         matnr  TYPE matnr,
         werks  TYPE werks_d,
         charg  TYPE charg_d,
         locid  TYPE oij_locid,
         tsyst  TYPE oij_tsyst,
       END OF ty_temp.

TYPES: BEGIN OF ty_log,
         tsyst   TYPE oij_tsyst,
         ebeln   TYPE ekpo-ebeln,
         date    TYPE sy-datum,
         locid   TYPE oij_locid,
         matnr   TYPE matnr,
         charg   TYPE charg_d,
         message(100) TYPE c,
       END OF ty_log.

TYPES: BEGIN OF ty_display_final,
         sel         TYPE char1,
         row_color   TYPE char4,
         date        TYPE sy-datum,
         locid       TYPE oij_locid,
         matnr       TYPE matnr,
         menge       TYPE oij_menge,
         unit        TYPE oij_uniti,
         charg       TYPE charg_d,
         nomtk       TYPE oij_nomtk,
         nomit       TYPE oij_item,
         post_status TYPE char30,
         ticketnr    TYPE oij_tktnr,
         error_msg(170) TYPE c,
       END OF ty_display_final.

TYPES: tt_main         TYPE STANDARD TABLE OF ty_main.
TYPES: tt_log          TYPE STANDARD TABLE OF ty_log.
TYPES: tt_display_final TYPE STANDARD TABLE OF ty_display_final.

*----------------------------------------------------------------------*
* DATA DECLARATIONS
*----------------------------------------------------------------------*
DATA: gt_main          TYPE tt_main,
      gt_log           TYPE tt_log,
      gt_display_final TYPE tt_display_final,
      go_alv           TYPE REF TO cl_gui_alv_grid,
      gs_layout        TYPE lvc_s_layo,
      gt_fcat          TYPE lvc_t_fcat.

DATA: i_tab       TYPE STANDARD TABLE OF alsmexa_tabline,
      i_nom_item  TYPE STANDARD TABLE OF bapiitswnom03,
      i_return    TYPE STANDARD TABLE OF bapiret2,
      i_error     TYPE tt_log,
      i_message   TYPE tt_log.

DATA: wa_main     TYPE ty_main,
      wa_main1    TYPE ty_main,
      wa_log      TYPE ty_log.

DATA: it_oijnomi  TYPE STANDARD TABLE OF oijnomi,
      it_oijts    TYPE STANDARD TABLE OF oijts,
      it_ekko     TYPE STANDARD TABLE OF ekko.

CONSTANTS: gc_call_flag TYPE char30 VALUE 'YRGGG015_CALL_FLAG',
           gc_nom_data  TYPE char30 VALUE 'YRGGG015_NOM_DATA'.

*----------------------------------------------------------------------*
* ALV HANDLER CLASS
*----------------------------------------------------------------------*
CLASS lcl_alv_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      on_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.

DATA: go_alv_handler TYPE REF TO lcl_alv_handler.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME.
  PARAMETERS: r_excel RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND ucom,
              r_pro   RADIOBUTTON GROUP rad1,
              r_dis   RADIOBUTTON GROUP rad1,
              r_act   RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_fname TYPE rlgraph-filename MODIF ID abc.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_locid FOR oijnomi-locid NO INTERVALS,
                  s_date  FOR oijnomi-idate.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_qty TYPE oij_allocated_qty NO-DISPLAY,
              p_gcv TYPE p DECIMALS 3      NO-DISPLAY,
              p_ncv TYPE p DECIMALS 3      NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  TEXT-001 = 'File Selection'.
  TEXT-002 = 'Selection Criteria'.
  TEXT-003 = 'Conversion Factors'.
  PERFORM set_default_dates.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM control_screen_fields.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM validate_selection.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask            = '*.xlsx,*.xls,Excel Files|*.xlsx;*.xls'
      static          = 'X'
    CHANGING
      file_name       = p_fname
    EXCEPTIONS
      mask_too_long   = 1
      OTHERS          = 2.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  IF r_excel = 'X'.
    PERFORM fetch_data_from_excel_sheet.
    PERFORM arrange_data.
    PERFORM batch_validate.
    PERFORM createfromdata.
    PERFORM show_log.
  ELSEIF r_pro = 'X'.
    PERFORM get_nomination.
    PERFORM process_nomination.
    PERFORM show_log.
  ELSEIF r_dis = 'X'.
    PERFORM get_nomination.
    PERFORM display_results.
  ELSEIF r_act = 'X'.
    PERFORM get_nomination.
    PERFORM activate_nomination.
    PERFORM show_log.
  ENDIF.

*----------------------------------------------------------------------*
* ALV HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_alv_handler IMPLEMENTATION.

  METHOD on_toolbar.
    DATA: ls_tb TYPE stb_button.
    CLEAR ls_tb.
    ls_tb-function  = 'POST'.
    ls_tb-icon      = '@15@'.
    ls_tb-quickinfo = 'Post Nomination'.
    ls_tb-text      = 'Post'.
    INSERT ls_tb INTO e_object->mt_toolbar INDEX 1.
  ENDMETHOD.

  METHOD on_user_command.
    CASE e_ucomm.
      WHEN 'POST'.
        PERFORM process_nomination.
        PERFORM refresh_alv.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* FORM: set_default_dates
*----------------------------------------------------------------------*
FORM set_default_dates.
  DATA: lv_today TYPE sy-datum,
        lv_day   TYPE i,
        lv_low   TYPE sy-datum,
        lv_high  TYPE sy-datum,
        ls_date  LIKE LINE OF s_date.

  lv_today = sy-datum.
  lv_day   = lv_today+6(2).

  IF lv_day <= 15.
    lv_high      = lv_today.
    lv_high+6(2) = '01'.
    lv_high      = lv_high - 1.
    lv_low       = lv_high.
    lv_low+6(2)  = '16'.
  ELSE.
    lv_low      = lv_today.
    lv_low+6(2) = '01'.
    lv_high      = lv_today.
    lv_high+6(2) = '15'.
  ENDIF.

  ls_date-sign   = 'I'.
  ls_date-option = 'BT'.
  ls_date-low    = lv_low.
  ls_date-high   = lv_high.
  APPEND ls_date TO s_date.
ENDFORM.

*----------------------------------------------------------------------*
* FORM: control_screen_fields
*----------------------------------------------------------------------*
FORM control_screen_fields.
  LOOP AT SCREEN.
    IF screen-group1 = 'ABC'.
      IF r_excel = 'X'.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ELSEIF screen-group1 = 'DEF'.
      IF r_pro = 'X' OR r_dis = 'X' OR r_act = 'X'.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
* FORM: validate_selection
*----------------------------------------------------------------------*
FORM validate_selection.
  IF r_excel = 'X'.
    IF p_fname IS INITIAL.
      MESSAGE e000(oo) WITH 'File name is mandatory for Excel upload' ' ' ' ' ' '.
    ENDIF.
  ELSE.
    IF s_date[] IS INITIAL.
      MESSAGE e000(oo) WITH 'Date range is mandatory' ' ' ' ' ' '.
    ENDIF.
    IF s_locid[] IS INITIAL.
      MESSAGE e000(oo) WITH 'Location is mandatory' ' ' ' ' ' '.
    ENDIF.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM: fetch_data_from_excel_sheet
*----------------------------------------------------------------------*
FORM fetch_data_from_excel_sheet.
  DATA: lv_filename TYPE string.
  lv_filename = p_fname.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = lv_filename
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 20
      i_end_row               = 9999
    TABLES
      intern                  = i_tab
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    MESSAGE e000(oo) WITH 'Error reading Excel file:' p_fname ' ' ' '.
  ENDIF.

  IF i_tab IS INITIAL.
    MESSAGE e000(oo) WITH 'No data found in Excel file' ' ' ' ' ' '.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM: arrange_data
*----------------------------------------------------------------------*
FORM arrange_data.
  DATA: ls_tab     TYPE alsmexa_tabline,
        ls_main    TYPE ty_main,
        lv_row_prev TYPE i VALUE 0,
        lv_col     TYPE i.

  SORT i_tab BY row col.
  CLEAR ls_main.

  LOOP AT i_tab INTO ls_tab.
    IF ls_tab-row <> lv_row_prev AND lv_row_prev > 0.
      APPEND ls_main TO gt_main.
      CLEAR ls_main.
    ENDIF.
    lv_col = ls_tab-col.
    CASE lv_col.
      WHEN 1.  " Description — skip
      WHEN 2.  ls_main-tsyst = ls_tab-value.
      WHEN 3.  ls_main-vbeln = ls_tab-value.
      WHEN 4.  ls_main-date  = ls_tab-value.
      WHEN 5.  ls_main-locid = ls_tab-value.
      WHEN 6.  ls_main-matnr = ls_tab-value.
      WHEN 7.  ls_main-menge = ls_tab-value.
      WHEN 8.  ls_main-unit  = ls_tab-value.
      WHEN 9.  ls_main-charg = ls_tab-value.
      WHEN 10. ls_main-rank  = ls_tab-value.
    ENDCASE.
    lv_row_prev = ls_tab-row.
  ENDLOOP.

  IF ls_main IS NOT INITIAL.
    APPEND ls_main TO gt_main.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM: batch_validate
*----------------------------------------------------------------------*
FORM batch_validate.
  DATA: ls_main  TYPE ty_main,
        ls_log   TYPE ty_log,
        lv_found TYPE char1.

  LOOP AT gt_main INTO ls_main.
    CLEAR lv_found.

    " Validate date
    IF ls_main-date IS INITIAL.
      ls_log-date    = sy-datum.
      ls_log-locid   = ls_main-locid.
      ls_log-matnr   = ls_main-matnr.
      ls_log-message = 'Gas day date is missing'.
      APPEND ls_log TO i_error.
      ls_main-flag        = 'E'.
      ls_main-post_status = 'Error: Date missing'.
      ls_main-color       = 'C60'.
      MODIFY gt_main FROM ls_main INDEX sy-tabix.
      CONTINUE.
    ENDIF.

    " Validate location
    IF ls_main-locid IS INITIAL.
      ls_log-date    = ls_main-date.
      ls_log-matnr   = ls_main-matnr.
      ls_log-message = 'Location ID is missing'.
      APPEND ls_log TO i_error.
      ls_main-flag        = 'E'.
      ls_main-post_status = 'Error: Location missing'.
      ls_main-color       = 'C60'.
      MODIFY gt_main FROM ls_main INDEX sy-tabix.
      CONTINUE.
    ENDIF.

    " Validate material
    IF ls_main-matnr IS INITIAL.
      ls_log-date    = ls_main-date.
      ls_log-locid   = ls_main-locid.
      ls_log-message = 'Material number is missing'.
      APPEND ls_log TO i_error.
      ls_main-flag        = 'E'.
      ls_main-post_status = 'Error: Material missing'.
      ls_main-color       = 'C60'.
      MODIFY gt_main FROM ls_main INDEX sy-tabix.
      CONTINUE.
    ENDIF.

    " Validate quantity — reject zero explicitly (not just initial)
    IF ls_main-menge <= 0.
      ls_log-date    = ls_main-date.
      ls_log-locid   = ls_main-locid.
      ls_log-matnr   = ls_main-matnr.
      ls_log-message = 'Nominated quantity is zero or negative — row skipped'.
      APPEND ls_log TO i_error.
      ls_main-flag        = 'E'.
      ls_main-post_status = 'Skipped: Zero Qty'.
      ls_main-color       = 'C60'.
      MODIFY gt_main FROM ls_main INDEX sy-tabix.
      CONTINUE.
    ENDIF.

    ls_main-flag  = 'V'.
    ls_main-color = 'C32'.
    MODIFY gt_main FROM ls_main INDEX sy-tabix.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
* FORM: get_nomination
*----------------------------------------------------------------------*
FORM get_nomination.
  DATA: ls_oijnomi TYPE oijnomi,
        ls_main    TYPE ty_main.

  SELECT * FROM oijnomi INTO TABLE it_oijnomi
    WHERE locid IN s_locid
      AND idate IN s_date.

  LOOP AT it_oijnomi INTO ls_oijnomi.
    CLEAR ls_main.
    ls_main-date    = ls_oijnomi-idate.
    ls_main-locid   = ls_oijnomi-locid.
    ls_main-matnr   = ls_oijnomi-matnr.
    ls_main-menge   = ls_oijnomi-imenge.
    ls_main-unit    = ls_oijnomi-iunit.
    ls_main-charg   = ls_oijnomi-charg.
    ls_main-nomtk   = ls_oijnomi-nomtk.
    ls_main-nomit   = ls_oijnomi-nomit.
    ls_main-del_ind = ls_oijnomi-del_ind.
    ls_main-color   = 'C32'.
    APPEND ls_main TO gt_main.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
* FORM: createfromdata
*----------------------------------------------------------------------*
FORM createfromdata.
  DATA: ls_main       TYPE ty_main,
        ls_nom_item   TYPE bapiitswnom03,
        ls_return     TYPE bapiret2,
        lv_nomtk      TYPE oij_nomtk,
        lv_errmsg     TYPE char200.

  LOOP AT gt_main INTO ls_main WHERE flag = 'V' AND menge > 0.
    CLEAR: i_nom_item, i_return.
    CLEAR ls_nom_item.

    ls_nom_item-locid    = ls_main-locid.
    ls_nom_item-idate    = ls_main-date.
    ls_nom_item-matnr    = ls_main-matnr.
    ls_nom_item-imenge   = ls_main-menge.
    ls_nom_item-iunit    = ls_main-unit.
    ls_nom_item-charg    = ls_main-charg.
    APPEND ls_nom_item TO i_nom_item.

    CALL FUNCTION 'OIJ_NOM_CREATE_FROM_DATA'
      EXPORTING
        i_tsyst     = ls_main-tsyst
      IMPORTING
        e_nomtk     = lv_nomtk
      TABLES
        it_nom_item = i_nom_item
        et_return   = i_return
      EXCEPTIONS
        OTHERS      = 1.

    IF sy-subrc = 0.
      READ TABLE i_return INTO ls_return WITH KEY type = 'E'.
      IF sy-subrc = 0.
        ls_main-post_status = 'Error'.
        ls_main-error_msg   = ls_return-message.
        ls_main-color       = 'C60'.
        CLEAR wa_log.
        wa_log-date    = ls_main-date.
        wa_log-locid   = ls_main-locid.
        wa_log-matnr   = ls_main-matnr.
        wa_log-message = ls_return-message.
        APPEND wa_log TO i_error.
      ELSE.
        ls_main-nomtk       = lv_nomtk.
        ls_main-post_status = 'Created'.
        ls_main-color       = 'C32'.
        CLEAR wa_log.
        wa_log-date    = ls_main-date.
        wa_log-locid   = ls_main-locid.
        wa_log-matnr   = ls_main-matnr.
        CONCATENATE 'Nomination' lv_nomtk 'created' INTO wa_log-message SEPARATED BY space.
        APPEND wa_log TO i_message.
      ENDIF.
    ELSE.
      ls_main-post_status = 'FM Error'.
      ls_main-color       = 'C60'.
    ENDIF.

    MODIFY gt_main FROM ls_main.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
* FORM: process_nomination
*----------------------------------------------------------------------*
FORM process_nomination.
  DATA: ls_main   TYPE ty_main,
        ls_return TYPE bapiret2.

  LOOP AT gt_main INTO ls_main WHERE del_ind IS INITIAL.
    CLEAR i_return.

    CALL FUNCTION 'OIJ_NOM_PROCESS'
      EXPORTING
        i_nomtk   = ls_main-nomtk
        i_nomit   = ls_main-nomit
      TABLES
        et_return = i_return
      EXCEPTIONS
        OTHERS    = 1.

    READ TABLE i_return INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      ls_main-post_status = 'Error'.
      ls_main-error_msg   = ls_return-message.
      ls_main-color       = 'C60'.
    ELSE.
      ls_main-post_status = 'Processed'.
      ls_main-color       = 'C32'.
    ENDIF.
    MODIFY gt_main FROM ls_main.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
* FORM: activate_nomination
*----------------------------------------------------------------------*
FORM activate_nomination.
  DATA: ls_main   TYPE ty_main,
        ls_return TYPE bapiret2.

  LOOP AT gt_main INTO ls_main WHERE del_ind IS INITIAL.
    CLEAR i_return.

    CALL FUNCTION 'OIJ_NOM_ACTIVATE'
      EXPORTING
        i_nomtk   = ls_main-nomtk
        i_nomit   = ls_main-nomit
      TABLES
        et_return = i_return
      EXCEPTIONS
        OTHERS    = 1.

    READ TABLE i_return INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc = 0.
      ls_main-post_status = 'Activation Error'.
      ls_main-error_msg   = ls_return-message.
      ls_main-color       = 'C60'.
    ELSE.
      ls_main-post_status = 'Activated'.
      ls_main-color       = 'C32'.
    ENDIF.
    MODIFY gt_main FROM ls_main.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
* FORM: show_log
*----------------------------------------------------------------------*
FORM show_log.
  DATA: lt_disp  TYPE tt_display_final,
        ls_disp  TYPE ty_display_final,
        ls_main  TYPE ty_main,
        ls_log   TYPE ty_log.

  LOOP AT gt_main INTO ls_main.
    CLEAR ls_disp.
    ls_disp-date        = ls_main-date.
    ls_disp-locid       = ls_main-locid.
    ls_disp-matnr       = ls_main-matnr.
    ls_disp-menge       = ls_main-menge.
    ls_disp-unit        = ls_main-unit.
    ls_disp-charg       = ls_main-charg.
    ls_disp-nomtk       = ls_main-nomtk.
    ls_disp-nomit       = ls_main-nomit.
    ls_disp-post_status = ls_main-post_status.
    ls_disp-ticketnr    = ls_main-ticketnr.
    ls_disp-error_msg   = ls_main-error_msg.
    ls_disp-row_color   = ls_main-color.
    APPEND ls_disp TO lt_disp.
  ENDLOOP.

  IF lt_disp IS INITIAL.
    MESSAGE s000(oo) WITH 'No records to display' ' ' ' ' ' '.
    RETURN.
  ENDIF.

  PERFORM display_log_alv USING lt_disp.
ENDFORM.

*----------------------------------------------------------------------*
* FORM: display_results
*----------------------------------------------------------------------*
FORM display_results.
  PERFORM show_log.
ENDFORM.

*----------------------------------------------------------------------*
* FORM: display_log_alv
*----------------------------------------------------------------------*
FORM display_log_alv USING pt_disp TYPE tt_display_final.
  DATA: lo_container TYPE REF TO cl_gui_custom_container,
        ls_fcat      TYPE lvc_s_fcat,
        ls_layout    TYPE lvc_s_layo,
        ls_variant   TYPE disvariant.

  " Build field catalog
  CLEAR gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'DATE'.
  ls_fcat-coltext   = 'Gas Day'.
  ls_fcat-outputlen = 10.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'LOCID'.
  ls_fcat-coltext   = 'Location'.
  ls_fcat-outputlen = 10.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'MATNR'.
  ls_fcat-coltext   = 'Material'.
  ls_fcat-outputlen = 18.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'MENGE'.
  ls_fcat-coltext   = 'Quantity'.
  ls_fcat-outputlen = 15.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'UNIT'.
  ls_fcat-coltext   = 'Unit'.
  ls_fcat-outputlen = 6.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'CHARG'.
  ls_fcat-coltext   = 'Batch'.
  ls_fcat-outputlen = 10.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'NOMTK'.
  ls_fcat-coltext   = 'Nom Ticket'.
  ls_fcat-outputlen = 12.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'NOMIT'.
  ls_fcat-coltext   = 'Nom Item'.
  ls_fcat-outputlen = 6.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'POST_STATUS'.
  ls_fcat-coltext   = 'Status'.
  ls_fcat-outputlen = 20.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'TICKETNR'.
  ls_fcat-coltext   = 'Ticket No'.
  ls_fcat-outputlen = 12.
  APPEND ls_fcat TO gt_fcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname = 'ERROR_MSG'.
  ls_fcat-coltext   = 'Message'.
  ls_fcat-outputlen = 50.
  APPEND ls_fcat TO gt_fcat.

  " Layout
  ls_layout-cwidth_opt = abap_true.
  ls_layout-zebra      = abap_true.
  ls_layout-info_fname = 'ROW_COLOR'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      it_fieldcat_lvc = gt_fcat
      is_layout_lvc   = ls_layout
    TABLES
      t_outtab        = pt_disp
    EXCEPTIONS
      program_error   = 1
      OTHERS          = 2.
ENDFORM.

*----------------------------------------------------------------------*
* FORM: refresh_alv
*----------------------------------------------------------------------*
FORM refresh_alv.
  DATA: ls_stbl TYPE lvc_s_stbl.
  IF go_alv IS NOT INITIAL.
    ls_stbl-row = abap_true.
    ls_stbl-col = abap_true.
    go_alv->refresh_table_display( is_stable = ls_stbl ).
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
* FORM: error_display
*----------------------------------------------------------------------*
FORM error_display.
  DATA: ls_log TYPE ty_log.
  LOOP AT i_error INTO ls_log.
    WRITE: / ls_log-date, ls_log-locid, ls_log-matnr, ls_log-message.
  ENDLOOP.
ENDFORM.
