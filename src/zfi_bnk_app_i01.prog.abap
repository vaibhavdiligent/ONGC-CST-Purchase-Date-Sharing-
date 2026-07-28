*----------------------------------------------------------------------*
***INCLUDE ZFI_BNK_APP_I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA : lv_code type sy-ucomm.
  lv_code = sy-ucomm.
  case lv_code.
    when 'E'.
      LEAVE to SCREEN 0.
*      call TRANSACTION 'ZBNK_APP2'.

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROW  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_row INPUT.
  DATA : lv_code1 TYPE sy-ucomm.
  lv_code1 = sy-ucomm.
  IF lv_code1 = 'DOWNLOAD1'.
    PERFORM download_sent_data.
  ELSEIF lv_code1 =  'DOWNLOAD2'.
    PERFORM download_raw_data.
  ELSEIF lv_code1 =  'DOWNLOAD3'.
    PERFORM download_rec_data.
  ELSEIF lv_code1 = 'RESENT'.
    PERFORM f_resent.
  ENDIF.

ENDMODULE.                 " GET_SELECTED_ROW  INPUT
