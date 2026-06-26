*&---------------------------------------------------------------------*
*& Include          ZFI_BNK_APP1_O01
*&---------------------------------------------------------------------*

*&SPWIZARD: OUTPUT MODULE FOR TS 'TABSTRIP'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE tabstrip_active_tab_set OUTPUT.
*  tabstrip-activetab = g_tabstrip-pressed_tab.
*  CASE g_tabstrip-pressed_tab.
*    WHEN c_tabstrip-tab1.
*      g_tabstrip-subscreen = '0101'.
*    WHEN c_tabstrip-tab2.
*      g_tabstrip-subscreen = '0102'.
*    WHEN c_tabstrip-tab3.
*      g_tabstrip-subscreen = '0103'.
*    WHEN OTHERS.
**&SPWIZARD:      DO NOTHING
*  ENDCASE.
ENDMODULE.                    "TABSTRIP_ACTIVE_TAB_SET OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OUTPUT_TAB1  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE output_tab1 OUTPUT.
  PERFORM f_prepare_op_tab1.
***
  CREATE OBJECT c_ccont1
    EXPORTING
      container_name = 'CONTROL1'.

  CREATE OBJECT c_alvgd1
    EXPORTING
      i_parent = c_ccont1.

**Set field for ALV
  PERFORM f_alv_build_fieldcat1.

**Set ALV attributes FOR LAYOUT
  PERFORM f_alv_report_layout1.

  CHECK NOT c_alvgd1 IS INITIAL.

**Call ALV GRID
  CALL METHOD c_alvgd1->set_table_for_first_display
    EXPORTING
      is_layout                     = it_layout1
    CHANGING
      it_outtab                     = gt_final
      it_fieldcatalog               = it_fcat1
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMODULE.                 " OUTPUT_TAB1  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  OUTPUT_TAB2  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE output_tab2 OUTPUT.
  PERFORM f_prepare_op_tab2.
***
  CREATE OBJECT c_ccont2
    EXPORTING
      container_name = 'CONTROL2'.

  CREATE OBJECT c_alvgd2
    EXPORTING
      i_parent = c_ccont2.

* **Set field for ALV
  PERFORM f_alv_build_fieldcat2.

**Set ALV attributes FOR LAYOUT
  PERFORM f_alv_report_layout2.

  CHECK NOT c_alvgd2 IS INITIAL.

**Call ALV GRID
  CALL METHOD c_alvgd2->set_table_for_first_display
    EXPORTING
      is_layout                     = it_layout2
    CHANGING
      it_outtab                     = gt_final2
      it_fieldcatalog               = it_fcat2
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMODULE.                 " OUTPUT_TAB2  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ZPF_FI_BNK_APP1'.
  SET TITLEBAR 'ZTITLE_FI_BNK_APP1'.

ENDMODULE.                 " STATUS_0100  OUTPUT


*&---------------------------------------------------------------------*
*&      Module  OUTPUT_TAB3  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE output_tab3 OUTPUT.
  PERFORM f_prepare_op_tab3.
***
  IF  c_ccont3 IS INITIAL.

    CREATE OBJECT c_ccont3
      EXPORTING
        container_name = 'CONTROL3'.
  ENDIF.

  IF c_alvgd3 IS INITIAL.
    CREATE OBJECT c_alvgd3
      EXPORTING
        i_parent = c_ccont3.
  ENDIF.
* **Set field for ALV
  PERFORM f_alv_build_fieldcat3.

**Set ALV attributes FOR LAYOUT
  PERFORM f_alv_report_layout3.

  CHECK NOT c_alvgd3 IS INITIAL.

**Call ALV GRID
  CALL METHOD c_alvgd3->set_table_for_first_display
    EXPORTING
      is_layout                     = it_layout3
    CHANGING
      it_outtab                     = gt_final3
      it_fieldcatalog               = it_fcat3
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc EQ 0.
*    CALL METHOD c_alvgd3->refresh_table_display.

  ENDIF.
ENDMODULE.                 " OUTPUT_TAB3  OUTPUT
