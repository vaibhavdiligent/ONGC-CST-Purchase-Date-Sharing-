*&---------------------------------------------------------------------*
*& Include YRGG015_PURC_NOM_ONGC_B2B_F03
*& ALV Grid Display, Fieldcat, Event Handler Class Implementation
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Event Handler Class - Implementation
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  " Add custom toolbar buttons: BCMASS (Batch Change in Mass) + CRENOM (Create Nomination)
  METHOD on_toolbar.
    DATA: ls_toolbar TYPE stb_button.

    " Batch Change in Mass button
    CLEAR ls_toolbar.
    ls_toolbar-function  = 'BCMASS'.
    ls_toolbar-icon      = icon_batch_input.
    ls_toolbar-quickinfo = 'Batch Change in Mass'.
    ls_toolbar-text      = 'Batch Change'.
    ls_toolbar-disabled  = ' '.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    " Separator
    CLEAR ls_toolbar.
    ls_toolbar-butn_type = 3.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    " Create Nomination button
    CLEAR ls_toolbar.
    ls_toolbar-function  = 'CRENOM'.
    ls_toolbar-icon      = icon_execute_object.
    ls_toolbar-quickinfo = 'Create Nomination'.
    ls_toolbar-text      = 'Create Nomination'.
    ls_toolbar-disabled  = ' '.
    APPEND ls_toolbar TO e_object->mt_toolbar.
  ENDMETHOD.

  " Handle custom toolbar button clicks
  METHOD on_user_command.
    CASE e_ucomm.
      WHEN 'BCMASS'.
        PERFORM handle_batch_mass_change.
      WHEN 'CRENOM'.
        PERFORM handle_create_nomination.
    ENDCASE.
  ENDMETHOD.

  " Handle inline batch edits
  METHOD on_data_changed.
    DATA: ls_mod  TYPE lvc_s_modi,
          ls_disp TYPE ty_display.

    LOOP AT er_data_changed->mt_mod_cells INTO ls_mod.
      IF ls_mod-fieldname = 'CHARG'.
        READ TABLE gt_display INDEX ls_mod-row_id INTO ls_disp.
        IF sy-subrc = 0.
          ls_disp-charg = ls_mod-value.
          MODIFY gt_display INDEX ls_mod-row_id FROM ls_disp.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  " Checkbox click auto-selects all rows with same LOCID + GAS_DAY
  METHOD on_hotspot_click.
    DATA: ls_disp   TYPE ty_display,
          lv_locid  TYPE char10,
          lv_date   TYPE aedat,
          lv_newsel TYPE char1,
          lv_idx    TYPE i.

    lv_idx = e_row_id-index.

    IF e_column_id-fieldname = 'SEL'.
      READ TABLE gt_display INDEX lv_idx INTO ls_disp.
      IF sy-subrc = 0.
        " Toggle selection
        IF ls_disp-sel = abap_true.
          lv_newsel = ' '.
        ELSE.
          lv_newsel = abap_true.
        ENDIF.
        lv_locid = ls_disp-location_id.
        lv_date  = ls_disp-gas_day.

        " Auto-select all rows with same location + date
        LOOP AT gt_display INTO ls_disp.
          IF ls_disp-location_id = lv_locid AND ls_disp-gas_day = lv_date.
            ls_disp-sel = lv_newsel.
            MODIFY gt_display FROM ls_disp.
          ENDIF.
        ENDLOOP.

        go_alv->refresh_table_display( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  " F4 help for CHARG (Batch) column
  METHOD on_onf4.
    IF e_fieldname = 'CHARG'.
      DATA: ls_disp     TYPE ty_display,
            lt_batches  TYPE STANDARD TABLE OF ty_batch_vals,
            lt_f4vals   TYPE STANDARD TABLE OF ddshretval,
            ls_f4val    TYPE ddshretval,
            ls_batch    TYPE ty_batch_vals.

      READ TABLE gt_display INDEX e_rowno INTO ls_disp.
      IF sy-subrc <> 0. RETURN. ENDIF.

      PERFORM get_valid_batches_for_material
        USING    ls_disp-material
        CHANGING lt_batches.

      IF lt_batches IS INITIAL.
        MESSAGE 'No valid batches found for this material.' TYPE 'S' DISPLAY LIKE 'W'.
        RETURN.
      ENDIF.

      " Build F4 value table
      LOOP AT lt_batches INTO ls_batch.
        CLEAR ls_f4val.
        ls_f4val-fieldname = 'CHARG'.
        ls_f4val-fieldval  = ls_batch-charg.
        APPEND ls_f4val TO lt_f4vals.
      ENDLOOP.

      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield        = 'CHARG'
          dynpprog        = sy-repid
          dynpnr          = sy-dynnr
          stepl           = e_rowno
          value_org       = 'S'
        TABLES
          value_tab       = lt_f4vals
        EXCEPTIONS
          parameter_error = 1
          no_values_found = 2
          OTHERS          = 3.

      IF sy-subrc = 0.
        er_event_data->m_event_handled = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& FORM display_alv_grid
*& Creates container and ALV grid, registers event handler
*&---------------------------------------------------------------------*
FORM display_alv_grid.
  DATA: lv_screen TYPE char4 VALUE '1000'.

  " Create custom container on screen 1000
  IF go_container IS INITIAL.
    CREATE OBJECT go_container
      EXPORTING
        container_name = 'ALV_CONTAINER'
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      MESSAGE 'Error creating ALV container.' TYPE 'E'.
    ENDIF.
  ENDIF.

  IF go_alv IS INITIAL.
    CREATE OBJECT go_alv
      EXPORTING
        i_parent = go_container
      EXCEPTIONS
        OTHERS   = 1.
    IF sy-subrc <> 0.
      MESSAGE 'Error creating ALV grid.' TYPE 'E'.
    ENDIF.
  ENDIF.

  " Register event handler
  CREATE OBJECT go_handler.
  SET HANDLER go_handler->on_toolbar       FOR go_alv.
  SET HANDLER go_handler->on_user_command  FOR go_alv.
  SET HANDLER go_handler->on_data_changed  FOR go_alv.
  SET HANDLER go_handler->on_hotspot_click FOR go_alv.
  SET HANDLER go_handler->on_onf4          FOR go_alv.

  " Register F4 for CHARG
  go_alv->register_f4_for_fields(
    EXPORTING
      it_f4 = VALUE lvc_t_f4(
                ( fieldname = 'CHARG' register_f4 = cl_gui_alv_grid=>m_mb_f4 ) ) ).

  " Build field catalog and layout
  PERFORM build_fieldcat.
  PERFORM set_alv_layout.

  " Display grid
  go_alv->set_table_for_first_display(
    EXPORTING
      is_layout        = gs_layout
      it_toolbar_excluding = VALUE ui_functions( )
    CHANGING
      it_outtab        = gt_display
      it_fieldcatalog  = gt_fcat
    EXCEPTIONS
      OTHERS           = 1 ).

  IF sy-subrc <> 0.
    MESSAGE 'Error displaying ALV grid.' TYPE 'E'.
  ENDIF.

  " Call screen
  CALL SCREEN 1000.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM build_fieldcat
*& Defines columns for ALV display
*&---------------------------------------------------------------------*
FORM build_fieldcat.
  DATA: ls_fcat TYPE lvc_s_fcat.

  DEFINE add_field.
    CLEAR ls_fcat.
    ls_fcat-fieldname = &1.
    ls_fcat-coltext   = &2.
    ls_fcat-seltext   = &2.
    ls_fcat-outputlen = &3.
    ls_fcat-edit      = &4.
    ls_fcat-hotspot   = &5.
    APPEND ls_fcat TO gt_fcat.
  END-OF-DEFINITION.

  "          fieldname      coltext                      len  edit  hotspot
  add_field 'SEL'          'Sel'                         3    ' '  'X'.
  add_field 'GAS_DAY'      'Gas Day'                    10    ' '  ' '.
  add_field 'LOCATION_ID'  'Location'                   10    ' '  ' '.
  add_field 'MATERIAL'     'Material'                   18    ' '  ' '.
  add_field 'STATE_CODE'   'State'                       4    ' '  ' '.
  add_field 'QTY_SCM'      'Qty SCM'                    15    ' '  ' '.
  add_field 'GAIL_ID'      'GAIL ID'                    20    ' '  ' '.
  add_field 'OUTLINE_AGR'  'Outline Agreement'          10    ' '  ' '.
  add_field 'CHARG'        'Batch'                      10    'X'  ' '.  " Editable

  " Cell color and style - hide from display but configure
  ls_fcat-fieldname  = 'T_COLOR'.
  ls_fcat-tech       = abap_true.
  APPEND ls_fcat TO gt_fcat.

  ls_fcat-fieldname  = 'CELLTAB'.
  ls_fcat-tech       = abap_true.
  APPEND ls_fcat TO gt_fcat.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM set_alv_layout
*& Configures ALV layout: row coloring, cell styles, etc.
*&---------------------------------------------------------------------*
FORM set_alv_layout.
  gs_layout-cwidth_opt = abap_true.
  gs_layout-zebra      = abap_true.
  gs_layout-edit       = abap_true.
  gs_layout-ctab_fname = 'T_COLOR'.
  gs_layout-stylefname = 'CELLTAB'.
  gs_layout-sel_mode   = 'D'.     " Disable standard checkbox (using custom SEL)
ENDFORM.

*----------------------------------------------------------------------*
* Screen 1000 - PAI
*----------------------------------------------------------------------*
MODULE user_command_1000 INPUT.
  CASE gv_okcode.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
  CLEAR gv_okcode.
ENDMODULE.
