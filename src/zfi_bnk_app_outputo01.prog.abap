*----------------------------------------------------------------------*
***INCLUDE ZFI_BNK_APP_OUTPUTO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  OUTPUT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE output OUTPUT.
  PERFORM f_prepare_op_tab.
***
  IF  c_ccont IS INITIAL.

    CREATE OBJECT c_ccont
      EXPORTING
        container_name = 'CONTROL'.
  ENDIF.

  IF c_alvgd IS INITIAL.
    CREATE OBJECT c_alvgd
      EXPORTING
        i_parent = c_ccont.
  ENDIF.
* **Set field for ALV
  PERFORM f_alv_build_fieldcat.

**Set ALV attributes FOR LAYOUT
  PERFORM f_alv_report_layout.

  CHECK NOT c_alvgd IS INITIAL.

**Call ALV GRID
  CALL METHOD c_alvgd->set_table_for_first_display
    EXPORTING
      is_layout                     = it_layout
    CHANGING
      it_outtab                     = gt_final
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc EQ 0.
*    CALL METHOD c_alvgd3->refresh_table_display.

  ENDIF.


ENDMODULE.                 " OUTPUT  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_0100  OUTPUT
