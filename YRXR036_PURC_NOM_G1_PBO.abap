*&---------------------------------------------------------------------*
*& Include  YRXR036_PURC_NOM_G1_PBO
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
* SET PF-STATUS 'YRFR316_STATUS_9000'.
  SET PF-STATUS 'YACT'.
  SET TITLEBAR '900'.
* lcl_meet_exp=>lm_display_alv( ).
  PERFORM display_act_alv.
  PERFORM display_9000.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*  text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  DATA : lv_lines        TYPE sytabix,
         lv_index        TYPE syindex,
         lt_message      TYPE tab_bdcmsgcoll,
         ls_message      TYPE bdcmsgcoll,
         lv_check        TYPE char1,
         lv_count        TYPE i,
         lv_message(132) TYPE c.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANCEL'.
      LEAVE TO TRANSACTION 'YRGR040'.
      LEAVE TO SCREEN 0.
      SET SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'SEL_ALL'.
      LOOP AT it_oijnomi_display_final1 ASSIGNING FIELD-SYMBOL(<fs_final>).
        <fs_final>-sel = 'X'.
      ENDLOOP.
    WHEN 'DESEL_ALL'.
      LOOP AT it_oijnomi_display_final1 ASSIGNING <fs_final>.
        CLEAR <fs_final>-sel.
      ENDLOOP.
    WHEN 'REFRESH'.
      IF it_oijnomi_display_final1[] IS NOT INITIAL.
        DELETE it_oijnomi_display_final1 WHERE post_status EQ 'COMPLETED'.
        LOOP AT it_oijnomi_display_final1[] ASSIGNING FIELD-SYMBOL(<lfs_final>) WHERE sel EQ 'X'.
          IF <lfs_final>-message IS NOT INITIAL.
            CLEAR <lfs_final>-message.
            CLEAR <lfs_final>-sel.
          ENDIF.
        ENDLOOP.
      ENDIF.
    WHEN 'SUBMIT'.
      READ TABLE it_oijnomi_display_final1 TRANSPORTING NO FIELDS WITH KEY sel = 'X'.
      IF sy-subrc NE 0.
        MESSAGE | Please select an entry for Actualization | TYPE 'I'.
        LEAVE LIST-PROCESSING.
      ELSE.
        DATA(gt_final_tmp) = it_oijnomi_display_final1[].
        DELETE gt_final_tmp WHERE sel IS INITIAL.
        LOOP AT gt_final_tmp INTO DATA(wa_final).
          LOOP AT it_oijnomi_display_final1 ASSIGNING <fs_final>
            WHERE idate EQ wa_final-idate AND locid EQ wa_final-locid.
            <fs_final>-sel = 'X'.
          ENDLOOP.
        ENDLOOP.
        REFRESH gt_final_tmp.
        gt_final_tmp[] = it_oijnomi_display_final1[].
        DELETE gt_final_tmp WHERE sel IS INITIAL.
        DESCRIBE TABLE gt_final_tmp LINES lv_lines.
        CLEAR lv_index.
        LOOP AT it_oijnomi_display_final1 ASSIGNING <fs_final> WHERE sel EQ 'X'.
          lv_index = lv_index + 1.
          cl_progress_indicator=>progress_indicate(
            EXPORTING
              i_text               = | Actualizing Ticket - { lv_index } / { lv_lines } |
              i_processed          = lv_index
              i_total              = lv_lines
              i_output_immediately = abap_true ).
          CLEAR lt_message.
          CALL FUNCTION 'YRX_TICKET_ACTUAL_BACKGROND'
            EXPORTING
              i_ticketnr = <fs_final>-ticketnr
              i_item     = <fs_final>-ticket_item
              i_choose   = lv_check
              i_version  = <fs_final>-version
            TABLES
              et_message = lt_message.
          CLEAR : ls_message, lv_message.
          " Check for Success Messages
          LOOP AT lt_message INTO ls_message WHERE msgid = 'OD'
                                             AND ( msgnr = '792'
                                                OR msgnr = '721'
                                                OR msgnr = '800').
            <fs_final>-post_status = 'COMPLETED'.
            EXIT.
          ENDLOOP.
          IF sy-subrc NE 0.
            DESCRIBE TABLE lt_message LINES DATA(lv_tab_lines).
            " Get Generic Error Messages
            READ TABLE lt_message INTO ls_message INDEX lv_tab_lines.
          ENDIF.
          IF ls_message IS NOT INITIAL.
            MESSAGE ID ls_message-msgid
                  TYPE ls_message-msgtyp
                NUMBER ls_message-msgnr
                  WITH ls_message-msgv1
                       ls_message-msgv2
                       ls_message-msgv3
                       ls_message-msgv4
                  INTO lv_message.
            IF sy-subrc EQ 0.
              <fs_final>-message = lv_message.
            ENDIF.
          ENDIF.
        ENDLOOP.
        MESSAGE | Selected Ticket(s) have been Processed. Please check Message Column for Status | TYPE 'I'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
