*&---------------------------------------------------------------------*
*& Report YGMS_DELETE_NOMINATION
*&---------------------------------------------------------------------*
REPORT YGMS_DELETE_NOMINATION.

*----------------------------------------------------------------------*
* TYPE-POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: slis, icon.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: oijnomi, vbak, oifspbl.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_output,
*         sel              TYPE char1,          "Row selection field
         gail_flag        TYPE char10,
         nomtk            TYPE oijnomi-nomtk,
         nomit            TYPE oijnomi-nomit,
         partnr           TYPE oijnomi-partnr,
         locid            TYPE oijnomi-locid,
         idate            TYPE oijnomi-idate,
         s_matnr_i        TYPE oijnomi-s_matnr_i,
         menge            TYPE oijnomi-menge,
         unit_i           TYPE oijnomi-unit_i,
         docnr            TYPE oijnomi-docnr,
         ga_allocated_qty TYPE oijnomi-ga_allocated_qty,
         actqty           TYPE oijnomi-actualqty,
         tkt_status       TYPE char20,
         sityp            TYPE oijnomi-sityp,
         delind           TYPE oijnomi-delind,
       END OF ty_output.

TYPES: BEGIN OF ty_error,
         msgty TYPE symsgty,
         msgtx TYPE char200,
       END OF ty_error.

*----------------------------------------------------------------------*
* DATA DECLARATIONS
*----------------------------------------------------------------------*
DATA: gt_oijnomi    TYPE STANDARD TABLE OF oijnomi,
      gt_oijnomi_zo TYPE STANDARD TABLE OF oijnomi,
      gt_output     TYPE STANDARD TABLE OF ty_output,
      gt_vbak       TYPE STANDARD TABLE OF vbak,
      gt_oifspbl    TYPE STANDARD TABLE OF oifspbl,
      gt_errors     TYPE STANDARD TABLE OF ty_error,
      gt_fieldcat   TYPE slis_t_fieldcat_alv,
      gt_events     TYPE slis_t_event.

DATA: gs_oijnomi  TYPE oijnomi,
      gs_output   TYPE ty_output,
      gs_error    TYPE ty_error,
      gs_fieldcat TYPE slis_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv,
      gs_event    TYPE slis_alv_event.

DATA: gv_pipeline  TYPE char10,
      gv_indicator TYPE char1,
      gv_repid     TYPE sy-repid,
      gv_valid     TYPE abap_bool.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  SELECT-OPTIONS: s_docnr FOR oijnomi-docnr no INTERVALS,
                  s_idate FOR oijnomi-idate  no INTERVALS OBLIGATORY,
                  s_locid FOR oijnomi-locid no INTERVALS.
SELECTION-SCREEN END OF BLOCK b01.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  gv_repid = sy-repid.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM validate_input.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM fetch_nominations.
  PERFORM build_output.

*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF gt_output IS NOT INITIAL.
    PERFORM build_fieldcat.
    PERFORM display_alv.
  ELSE.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = 'Information'(i01)
        txt1  = 'No data available for the selected criteria'(m06)
        txt2  = space.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form VALIDATE_INPUT
*&---------------------------------------------------------------------*
FORM validate_input.

  DATA: lv_start_day   TYPE numc2,
        lv_end_day     TYPE numc2,
        lv_start_month(6) TYPE n,
        lv_end_month(6)   TYPE n,
        lv_last_day    TYPE sy-datum,
        lv_error       TYPE abap_bool.

  CLEAR: gt_errors, lv_error.

  IF s_docnr[] IS INITIAL AND s_locid[] IS INITIAL.
    gs_error-msgty = 'E'.
    gs_error-msgtx = 'Please enter either Contract ID or both.'(m05).
    APPEND gs_error TO gt_errors.
    lv_error = abap_true.
  ENDIF.

  IF s_docnr[] IS NOT INITIAL.
    SELECT vbeln FROM vbak
      INTO TABLE gt_vbak
      WHERE vbeln IN s_docnr.
    IF sy-subrc NE 0.
      gs_error-msgty = 'E'.
      gs_error-msgtx = 'None of the Contract IDs exist'(m01).
      APPEND gs_error TO gt_errors.
      lv_error = abap_true.
    ENDIF.
  ENDIF.

  IF s_idate-high IS NOT INITIAL.
    lv_start_day   = s_idate-low+6(2).
    lv_end_day     = s_idate-high+6(2).
    lv_start_month = s_idate-low(6).
    lv_end_month   = s_idate-high(6).

    IF lv_start_month NE lv_end_month.
      gs_error-msgty = 'E'.
      gs_error-msgtx = 'The date range should belong to the same FN'(m03).
      APPEND gs_error TO gt_errors.
      lv_error = abap_true.
    ELSE.
      IF lv_start_day >= '01' AND lv_start_day <= '15'.
        IF lv_end_day > '15'.
          gs_error-msgty = 'E'.
          gs_error-msgtx = 'The date range should belong to the same FN'(m03).
          APPEND gs_error TO gt_errors.
          lv_error = abap_true.
        ENDIF.
      ELSEIF lv_start_day >= '16'.
        CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
          EXPORTING
            day_in            = s_idate-low
          IMPORTING
            last_day_of_month = lv_last_day
          EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.
        IF sy-subrc = 0 AND s_idate-high > lv_last_day.
          gs_error-msgty = 'E'.
          gs_error-msgtx = 'The date range should belong to the same FN'(m03).
          APPEND gs_error TO gt_errors.
          lv_error = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF s_locid[] IS NOT INITIAL.
    SELECT pblnr FROM oifspbl
      INTO TABLE gt_oifspbl
      WHERE pblnr IN s_locid
        AND pbltyp = 'YRDE'.
    IF sy-subrc NE 0.
      gs_error-msgty = 'E'.
      gs_error-msgtx = 'Enter valid sale location IDs'(m04).
      APPEND gs_error TO gt_errors.
      lv_error = abap_true.
    ENDIF.
  ENDIF.

  IF lv_error = abap_true.
    PERFORM display_error_screen.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ERROR_SCREEN
*&---------------------------------------------------------------------*
FORM display_error_screen.

  READ TABLE gt_errors INTO gs_error INDEX 1.
  MESSAGE gs_error-msgtx TYPE 'E'.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FETCH_NOMINATIONS
*&---------------------------------------------------------------------*
FORM fetch_nominations.

  DATA: lt_nomtk TYPE RANGE OF oijnomi-nomtk,
        ls_nomtk LIKE LINE OF lt_nomtk.

  SELECT * FROM oijnomi
    INTO TABLE gt_oijnomi
    WHERE docnr IN s_docnr
      AND locid IN s_locid
      AND idate IN s_idate
      AND delind NE 'X'.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  LOOP AT gt_oijnomi INTO gs_oijnomi WHERE sityp = 'ZD'.
    ls_nomtk-sign   = 'I'.
    ls_nomtk-option = 'EQ'.
    ls_nomtk-low    = gs_oijnomi-nomtk.
    APPEND ls_nomtk TO lt_nomtk.
  ENDLOOP.

  SORT lt_nomtk BY low.
  DELETE ADJACENT DUPLICATES FROM lt_nomtk COMPARING low.

  IF lt_nomtk IS NOT INITIAL.
    SELECT * FROM oijnomi
      INTO TABLE gt_oijnomi_zo
      WHERE nomtk IN lt_nomtk
        AND sityp = 'ZO'
        AND docind = 'K'
        AND delind NE 'X'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_OUTPUT
*&---------------------------------------------------------------------*
FORM build_output.

  CLEAR gt_output.

  LOOP AT gt_oijnomi INTO gs_oijnomi.
    CASE gs_oijnomi-sityp.
      WHEN 'DU'.
        PERFORM build_output_line USING gs_oijnomi.
      WHEN 'ZD'.
        PERFORM build_output_line USING gs_oijnomi.
        DATA: ls_zo TYPE oijnomi.
        LOOP AT gt_oijnomi_zo INTO ls_zo
          WHERE nomtk = gs_oijnomi-nomtk.
          PERFORM build_output_line USING ls_zo.
        ENDLOOP.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_OUTPUT_LINE
*&---------------------------------------------------------------------*
FORM build_output_line USING ps_nom TYPE oijnomi.

  DATA: ls_out TYPE ty_output.
  CLEAR ls_out.

*  ls_out-sel              = space.
  ls_out-nomtk            = ps_nom-nomtk.
  ls_out-nomit            = ps_nom-nomit.
  ls_out-partnr           = ps_nom-partnr.
  ls_out-locid            = ps_nom-locid.
  ls_out-idate            = ps_nom-idate.
  ls_out-s_matnr_i        = ps_nom-s_matnr_i.
  ls_out-menge            = ps_nom-menge.
  ls_out-unit_i           = ps_nom-unit_i.
  ls_out-docnr            = ps_nom-docnr.
  ls_out-ga_allocated_qty = ps_nom-ga_allocated_qty.
  ls_out-actqty           = ps_nom-actualqty.
  ls_out-sityp            = ps_nom-sityp.
  ls_out-delind           = ps_nom-delind.

  PERFORM determine_gail_flag USING    ps_nom-locid
                              CHANGING ls_out-gail_flag.
  PERFORM determine_tkt_status USING    ps_nom
                                        ls_out-gail_flag
                               CHANGING ls_out-tkt_status.

  APPEND ls_out TO gt_output.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DETERMINE_GAIL_FLAG
*&---------------------------------------------------------------------*
FORM determine_gail_flag USING    pv_locid TYPE oijnomi-locid
                         CHANGING pv_flag  TYPE char10.

  DATA: lv_pipeline TYPE char64.
  DATA et_nominations TYPE yrxt_nom.
  CLEAR pv_flag.

        data : lv_loc_c     TYPE oijnomi-locid,
        lv_prefix    TYPE char1.

 lv_prefix = pv_locid+0(1).
if lv_prefix = 'V'.
   CONCATENATE 'C' pv_locid+1 INTO lv_loc_c.

  else.
    lv_loc_c = pv_locid.
ENDIF.
  CALL FUNCTION 'YRX_CHK_NG_TKT_STATUS'
    EXPORTING
      gas_day  = s_idate-low
      locid    = lv_loc_c
    IMPORTING
      pipeline = lv_pipeline
    TABLES
      et_nominations = et_nominations.

  IF sy-subrc = 0.
    IF lv_pipeline CS 'NON-GAIL' OR lv_pipeline CS 'NON_GAIL'.
      pv_flag = 'Non-GAIL'.
    ELSE.
      pv_flag = 'GAIL'.
    ENDIF.
  ELSE.
    pv_flag = 'GAIL'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DETERMINE_TKT_STATUS
*&---------------------------------------------------------------------*
FORM determine_tkt_status USING    ps_nom       TYPE oijnomi
                                   pv_gail_flag TYPE char10
                          CHANGING pv_status    TYPE char20.

  DATA: lv_indicator TYPE char1,
        lv_loc_c     TYPE oijnomi-locid,
        lv_prefix    TYPE char1.
  DATA et_nominations TYPE yrxt_nom.
  CLEAR pv_status.

  lv_prefix = ps_nom-locid+0(1).

  IF pv_gail_flag = 'GAIL'.
    IF lv_prefix = 'C'.
      CALL FUNCTION 'YRX_CHK_NG_TKT_STATUS'
        EXPORTING
          gas_day   = ps_nom-idate
          locid     = ps_nom-locid
        IMPORTING
          indicator = lv_indicator
        TABLES
          et_nominations = et_nominations.
      IF sy-subrc = 0 AND lv_indicator = 'C'.
        pv_status = 'ticket present'.
      ENDIF.
    ELSEIF lv_prefix = 'V'.
*      LV_LOC_C = PS_NOM-LOCID.
*      LV_VOV_C(1) = 'C'.
      CONCATENATE 'C' ps_nom-locid+1 INTO lv_loc_c.
      CALL FUNCTION 'YRX_CHK_NG_TKT_STATUS'
        EXPORTING
          gas_day   = ps_nom-idate
          locid     = lv_loc_c
        IMPORTING
          indicator = lv_indicator
        TABLES
          et_nominations = et_nominations.
      IF sy-subrc = 0 AND lv_indicator = 'C'.
        pv_status = 'ticket present'.
      ENDIF.
    ENDIF.
  ELSEIF pv_gail_flag = 'Non-GAIL'.
    CALL FUNCTION 'YRX_CHK_CUST_COMP_TKT_STATUS'
      EXPORTING
        i_kunnr         = ps_nom-partnr
        i_date          = ps_nom-idate
        ticket_complete = 'X'
      IMPORTING
        indicator       = lv_indicator.
    IF sy-subrc = 0 AND lv_indicator = 'A'.
      pv_status = 'ticket present'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*& NOTE: SEL field is NOT in field catalog.
*& It is handled by gs_layout-box_fieldname which renders it
*& as a row-selection checkbox column automatically.
*&---------------------------------------------------------------------*
FORM build_fieldcat.

  CLEAR gt_fieldcat.

  DEFINE add_field.
    CLEAR gs_fieldcat.
    gs_fieldcat-col_pos   = &1.
    gs_fieldcat-fieldname = &2.
    gs_fieldcat-seltext_l = &3.
    gs_fieldcat-seltext_m = &3.
    gs_fieldcat-seltext_s = &4.
    gs_fieldcat-outputlen = &5.
    gs_fieldcat-key       = &6.
    gs_fieldcat-tabname   = 'GT_OUTPUT'.
    APPEND gs_fieldcat TO gt_fieldcat.
  END-OF-DEFINITION.

* NO SEL field in fieldcat - box_fieldname handles it
  add_field  1  'GAIL_FLAG'          'PIEPELINE'     'PIPE'         10  ' '.
  add_field  2  'NOMTK'              'Nomination Key'     'Nom Key'      20  'X'.
  add_field  3  'NOMIT'              'Nomination Item'    'Nom Item'      6  'X'.
  add_field  4  'PARTNR'             'Location Partner'   'Partner'      10  ' '.
  add_field  5  'LOCID'              'Location ID'        'Loc ID'       15  ' '.
  add_field  6  'IDATE'              'Gas Day'            'Gas Day'      10  ' '.
  add_field  7  'S_MATNR_I'          'Material'           'Material'     18  ' '.
  add_field  8  'MENGE'              'Nomination Qty'     'Nom Qty'      15  ' '.
  add_field  9  'UNIT_I'             'Nomination UoM'     'UoM'           6  ' '.
  add_field 10  'DOCNR'              'Contract ID'        'Contract'     10  ' '.
  add_field 11  'GA_ALLOCATED_QTY'   'Allocated Qty'      'Alloc Qty'    15  ' '.
  add_field 12  'ACTQTY'             'Actual Qty'         'Act Qty'      15  ' '.
  add_field 13  'TKT_STATUS'         'Ticket Status'      'Tkt Stat'     20  ' '.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM display_alv.

  CLEAR gs_layout.
  gs_layout-zebra             = 'X'.
  gs_layout-colwidth_optimize = 'X'.
*  gs_layout-box_fieldname     = 'SEL'.  "Row selection via checkbox

  gs_event-name = slis_ev_pf_status_set.
  gs_event-form = 'SET_PF_STATUS'.
  APPEND gs_event TO gt_events.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = gv_repid
      i_callback_user_command  = 'HANDLE_USER_COMMAND'
      is_layout                = gs_layout
      it_fieldcat              = gt_fieldcat
      it_events                = gt_events
      i_default                = 'X'
      i_save                   = 'A'
    TABLES
      t_outtab                 = gt_output
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

  IF sy-subrc NE 0.
    MESSAGE 'Error displaying ALV grid'(e01) TYPE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_PF_STATUS
*&---------------------------------------------------------------------*
FORM set_pf_status USING pt_extab TYPE slis_t_extab.
  SET PF-STATUS 'ZSTATUS_NOMDEL' EXCLUDING pt_extab.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*& box_fieldname = 'SEL' ensures the SLIS framework automatically
*& marks SEL = 'X' for selected rows and SEL = '' for deselected
*& rows BEFORE this callback is triggered.
*& No check_changed_data or GET_GLOBALS_FROM_SLSCREEN needed.
*&---------------------------------------------------------------------*
FORM handle_user_command USING pv_ucomm    TYPE sy-ucomm
                               ps_selfield TYPE slis_selfield.


  CASE pv_ucomm.
    WHEN 'ZDEL' OR '&DEL'.
      PERFORM delete_nominations.
      ps_selfield-refresh    = 'X'.
      ps_selfield-col_stable = 'X'.
      ps_selfield-row_stable = 'X'.

    WHEN 'ZSELALL'.
      PERFORM select_all_rows.
      ps_selfield-refresh    = 'X'.

    WHEN 'ZDESAL'.
      PERFORM deselect_all_rows.
      ps_selfield-refresh    = 'X'.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SELECT_ALL_ROWS
*&---------------------------------------------------------------------*
FORM select_all_rows.
*  FIELD-SYMBOLS: <lfs_out> TYPE ty_output.
*  LOOP AT gt_output ASSIGNING <lfs_out>.
*    <lfs_out>-sel = 'X'.
*  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DESELECT_ALL_ROWS
*&---------------------------------------------------------------------*
FORM deselect_all_rows.
*  FIELD-SYMBOLS: <lfs_out> TYPE ty_output.
*  LOOP AT gt_output ASSIGNING <lfs_out>.
*    <lfs_out>-sel = space.
*  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DELETE_NOMINATIONS
*&---------------------------------------------------------------------*
FORM delete_nominations.

  DATA: lv_answer    TYPE char1,
        lv_count     TYPE i,
        lv_fail      TYPE i,
        lv_sel_count TYPE i,
        lv_count_c   TYPE char10,
        lv_fail_c    TYPE char10,
        lv_msg       TYPE char200.

  DATA: ls_nom_header   TYPE ROIJNOMHIO,
        ls_nom_header_x TYPE oijnomh,
        lt_nom_items    TYPE ROIJNOMIIO_T,
        ls_nom_item     TYPE ROIJNOMIIO,
        lt_nom_items_x  TYPE STANDARD TABLE OF oijnomi,
        ls_nom_item_x   TYPE oijnomi,
        lt_return       TYPE STANDARD TABLE OF bapiret2,
        ls_return       TYPE bapiret2,
        lv_has_error    TYPE abap_bool.

  DATA: lt_nomtk_processed TYPE SORTED TABLE OF oijnomi-nomtk
                           WITH UNIQUE KEY table_line.

* Count selected rows
  LOOP AT gt_output INTO gs_output ."WHERE sel = 'X'.
    lv_sel_count = lv_sel_count + 1.
  ENDLOOP.

  IF lv_sel_count = 0.
    MESSAGE 'Please select at least one nomination to delete'(m09) TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

* Confirmation
  DATA: lv_question TYPE char200.
  lv_count_c = lv_sel_count.
  CONDENSE lv_count_c.
  CONCATENATE lv_count_c 'nomination(s) selected for deletion. Do you wish to proceed?'
    INTO lv_question SEPARATED BY space.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirm Deletion'(t01)
      text_question         = lv_question
      text_button_1         = 'Yes'(t02)
      icon_button_1         = 'ICON_CHECKED'
      text_button_2         = 'No'(t03)
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '2'
      display_cancel_button = ' '
    IMPORTING
      answer                = lv_answer.

  CHECK lv_answer = '1'.

  lv_count = 0.
  lv_fail  = 0.

select  * into table @data(it_oijnomh)
  from oijnomh
  FOR ALL ENTRIES IN @gt_output
  WHERE
  nomtk = @gt_output-nomtk.

* Process selected nominations
  LOOP AT gt_output INTO gs_output." WHERE sel = 'X'.

READ TABLE it_oijnomh into data(l_oijnomh)
with key nomtk = gs_output-nomtk.


    READ TABLE lt_nomtk_processed TRANSPORTING NO FIELDS
      WITH TABLE KEY table_line = gs_output-nomtk.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
    INSERT gs_output-nomtk INTO TABLE lt_nomtk_processed.

    CLEAR: ls_nom_header, ls_nom_header_x,
           lt_nom_items, lt_nom_items_x,
           lt_return, lv_has_error.

*    ls_nom_header-nomtk = gs_output-nomtk.
MOVE-CORRESPONDING l_oijnomh to ls_nom_header.

    LOOP AT gt_output INTO DATA(ls_item)
      WHERE nomtk = gs_output-nomtk." AND sel = 'X'.

      CLEAR ls_nom_item.
      ls_nom_item-nomtk  = ls_item-nomtk.
      ls_nom_item-nomit  = ls_item-nomit.
      ls_nom_item-locid  = ls_item-locid.
      ls_nom_item-idate  = ls_item-idate.
      ls_nom_item-docnr  = ls_item-docnr.
      ls_nom_item-partnr = ls_item-partnr.
      ls_nom_item-sityp  = ls_item-sityp.
      ls_nom_item-delind = 'X'.
      ls_nom_item-updkz = 'U'.
      APPEND ls_nom_item TO lt_nom_items.

      CLEAR ls_nom_item_x.
      ls_nom_item_x-nomtk  = ls_item-nomtk.
      ls_nom_item_x-nomit  = ls_item-nomit.
      ls_nom_item_x-delind = 'X'.

      APPEND ls_nom_item_x TO lt_nom_items_x.
    ENDLOOP.


CALL FUNCTION 'OIJ_NOM_MAINTAIN'
  EXPORTING
    is_nom_header               = ls_nom_header
    it_nom_item                 = lt_nom_items
*   IT_NOM_BALANCE              = IT_NOM_BALANCE
*   IT_NOM_EVENT                = IT_NOM_EVENT
*   IT_NOM_EVENT_TEXT           = IT_NOM_EVENT_TEXT
*   IT_COMMENT_TEXT             = IT_COMMENT_TEXT
*   IT_NOM_STAGES               = IT_NOM_STAGES
*   IT_NOM_SUBITEM              = IT_NOM_SUBITEM
*   IT_NOM_REF                  = IT_NOM_REF
*   IT_NOM_LOADDISCHARGE        = IT_NOM_LOADDISCHARGE
*   IT_TICKET_ITEMS             = IT_TICKET_ITEMS
*   IT_VEHICLE_PERF             = IT_VEHICLE_PERF
*   IT_NOM_STATUS_EXT           = IT_NOM_STATUS_EXT
*   IV_UPDATE_WORKLIST          = 'X'
*   IV_UPDATE_STOCKPROJ         = 'X'
*   IV_ADD_TO_LOG               = IV_ADD_TO_LOG
*   IV_INTERNAL                 = ' '
*   IV_LOCK                     = 'X'
*   IV_AUTHORITY_CHECK          = 'X'
*   IV_APPLICATION              = 'NOMINATION'
*   IS_CALLBACK                 = IS_CALLBACK
*   IV_RECURSIVE                = ' '
*   IT_NOM_SUBITEM_EVENTS       = IT_NOM_SUBITEM_EVENTS
 IMPORTING
*   ES_NOM_HEADER               = ES_NOM_HEADER
*   ET_NOM_ITEM                 = ET_NOM_ITEM
*   ET_NOM_EVENT                = ET_NOM_EVENT
*   ET_NOM_COPY                 = ET_NOM_COPY
*   ET_NOM_MATBALANCE           = ET_NOM_MATBALANCE
*   ET_NOM_STAGES               = ET_NOM_STAGES
*   ET_NOM_SUBITEM              = ET_NOM_SUBITEM
*   ET_NOM_REF                  = ET_NOM_REF
   ET_RETURN                   = lT_RETURN
*   ET_NOM_SUBITEM_EVENTS       = ET_NOM_SUBITEM_EVENTS
 EXCEPTIONS
   NOMINATION_LOCKED           = 1
   STATUS_UPDATE_FAILURE       = 2
          .
*DATA I_NOMTK          TYPE OIJ_NOMTK.
*DATA I_NOMIT          TYPE OIJ_ITEM.
*DATA DELETE_WHOLE_NOM TYPE CHAR01.
*DATA RETURN           TYPE BAPIRET2_T.

*CALL FUNCTION 'OIJ05_DEL_NOM_DB'
*  EXPORTING
*    i_nomtk                = ls_item-nomtk
*   I_NOMIT                = ls_item-nomit
**   DELETE_WHOLE_NOM       = ABAP_FALSE
* IMPORTING
*   RETURN                 = lt_RETURN
*          .


    IF sy-subrc = 0.
      LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
        lv_has_error = abap_true.
        EXIT.
      ENDLOOP.
      IF lv_has_error = abap_false.
        lv_count = lv_count + lines( lt_nom_items ).
        DELETE gt_output WHERE nomtk = gs_output-nomtk." AND sel = 'X'.
                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*         EXPORTING
*           WAIT          = WAIT
*         IMPORTING
*           RETURN        = RETURN
                  .
      ELSE.
*DATA WAIT   TYPE BAPITA-WAIT.
*DATA RETURN TYPE BAPIRET2.



        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        lv_fail = lv_fail + lines( lt_nom_items ).
      ENDIF.
    ELSE.
      lv_fail = lv_fail + lines( lt_nom_items ).
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

  ENDLOOP.

* Results
  IF lv_count > 0.
    lv_count_c = lv_count.
    CONDENSE lv_count_c.
    IF lv_fail > 0.
      lv_fail_c = lv_fail.
      CONDENSE lv_fail_c.
      CONCATENATE lv_count_c 'nomination(s) deleted successfully.'
                  lv_fail_c 'nomination(s) failed.'
        INTO lv_msg SEPARATED BY space.
      MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'W'.
    ELSE.
      CONCATENATE lv_count_c 'nomination(s) deleted successfully.'
        INTO lv_msg SEPARATED BY space.
      MESSAGE lv_msg TYPE 'S'.
    ENDIF.
  ELSEIF lv_fail > 0.
    lv_fail_c = lv_fail.
    CONDENSE lv_fail_c.
    CONCATENATE 'Deletion failed for' lv_fail_c
      'nomination(s). Check if nominations are locked or already deleted.'
      INTO lv_msg SEPARATED BY space.
    MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
  ELSE.
    MESSAGE 'No nominations were processed for deletion.'(m08) TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
