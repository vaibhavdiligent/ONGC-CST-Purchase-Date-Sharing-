*&---------------------------------------------------------------------*
*& Include  YRGR_033_GMS_IMBAL_CLASS
*& Local class definitions
*&---------------------------------------------------------------------*

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: get_data.
    CLASS-METHODS:
      button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no sender,
      top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING e_dyndoc_id table_index,
      toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive sender,
      user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm sender,
      data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed sender.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD get_data.
    PERFORM get_data.
  ENDMETHOD.
  METHOD button_click.
  ENDMETHOD.
  METHOD top_of_page.
    PERFORM top_of_page USING dg_dyndoc_id.
  ENDMETHOD.
  METHOD toolbar.
    " Add Save button to ALV toolbar when Action Taken (r4) mode is active
    IF r4 EQ 'X'.
      DATA: ls_toolbar TYPE stb_button.
      DATA: lv_icon TYPE icon_d.
      " Look up save icon from SAP ICON table (avoids TYPE-POOLS icon constant issues)
      SELECT SINGLE id FROM icon INTO lv_icon WHERE name = 'ICON_SAVE'.
      ls_toolbar-function  = 'SAVE_ACT'.
      ls_toolbar-icon      = lv_icon.
      ls_toolbar-text      = 'Save'.
      ls_toolbar-butn_type = 0.
      APPEND ls_toolbar TO e_object->mt_toolbar.
    ENDIF.
  ENDMETHOD.
  METHOD user_command.
    CASE e_ucomm.
      WHEN 'SAVE_ACT'.
        PERFORM save_action_taken.
    ENDCASE.
  ENDMETHOD.
  METHOD data_changed.
    PERFORM handle_data_changed USING er_data_changed.
  ENDMETHOD.
ENDCLASS.
