*----------------------------------------------------------------------*
***INCLUDE LZOIU_DELIVERY_NETWORKI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PROCESS_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE PROCESS_EXIT INPUT.

  IF SY-DYNNR = '0100'
   OR SY-DYNNR = C_SCREEN_1100.
    SET SCREEN 0.
    LEAVE SCREEN.
  ELSE.
    IF DELIVERY_NETWORK_TB1 NE ROIU_PR_DN OR W_MODE = C_CREATE.
      CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
           EXPORTING
                TEXTLINE1 = TEXT-010
                TEXTLINE2 = TEXT-017
                TITEL     = TEXT-009
           IMPORTING
                ANSWER    = W_ANSWER.
      IF W_ANSWER = C_YES.
* begin change note 196947 05/21/2001 lk
* the table should not be locked for all the networks
* when creating a external delivery network.
      PERFORM unlock_delivery_number.
* end change note 196947 05/21/2001 lk
       PERFORM CLEAR_INFORMATION.
*       IF SY-DYNNR = C_SCREEN_1200.
*         SET SCREEN 1100.
*         LEAVE SCREEN.
*        ELSE.
         SET SCREEN 100.
         LEAVE SCREEN.
*        ENDIF.
      ENDIF.
    ELSE.
      PERFORM CLEAR_INFORMATION.
*      IF SY-DYNNR = C_SCREEN_1200.
*       SET SCREEN 1100.
*       LEAVE SCREEN.
*      ELSE.
       SET SCREEN 100.
       LEAVE SCREEN.
*      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " PROCESS_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CASE W_FCODE.
    WHEN C_ENTER.
      IF W_MODE = C_UPDATE.
*       Set the DB action for updates and lock the number from changes
        ROIU_PR_DN-XXDBACTION = 'U'.
        PERFORM LOCK_DELIVERY_NUMBER.
*       Move the original dn info into 2 temp internal tables for later
*       comparisons to see if changes have occured
        DELIVERY_NETWORK_TB1 = ROIU_PR_DN.
*       delivery_network_tb2 = delivery_network_tb1.
        APPEND DELIVERY_NETWORK_TB1.
*       APPEND delivery_network_tb2.
      ENDIF.
      IF SY-DYNNR = C_SCREEN_1100.
**     SET SCREEN 1200.
       SET SCREEN 200.
       LEAVE SCREEN.
      ELSE.
       SET SCREEN 200.
       LEAVE SCREEN.
      ENDIF.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0200 INPUT.

  CASE W_FCODE.
    WHEN C_CREATE OR C_UPDATE OR C_DISPLAY.
*     Leave to the beginning of a new type of transaction
      IF ( DELIVERY_NETWORK_TB1 NE ROIU_PR_DN  AND
           W_MODE = C_UPDATE )                 OR
           W_MODE = C_CREATE.
        CALL FUNCTION 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
             EXPORTING
                  TEXTLINE1 = TEXT-010
                  TEXTLINE2 = TEXT-017
                  TITEL     = TEXT-009
             IMPORTING
                  ANSWER    = W_ANSWER.
      ELSE.
        W_ANSWER = C_YES.
      ENDIF.
      IF W_ANSWER = C_YES.
        PERFORM CLEAR_INFORMATION.
        W_MODE = W_FCODE.
        PERFORM LEAVE_TO_TRANSACTION USING W_MODE.
      ENDIF.
    WHEN C_OTHR.
*     Leave to the beginning of the same type of transaction
      PERFORM LEAVE_TO_TRANSACTION USING W_MODE.
    WHEN C_ADMN.
*     Call the administrative data popup window
      PERFORM ADMINISTRATIVE_DATA.
    WHEN C_OBJC.
*     Check for the change document
      OBJECTID = ROIU_PR_DN-DN_NO.
      SUBMIT RSSCD100 WITH OBJEKT   EQ 'OIU_DN'
                      WITH OBJEKTID EQ OBJECTID
                      WITH AENDERER EQ SPACE
                      WITH DATUM    EQ SPACE
                      WITH ZEIT     EQ SPACE
                      WITH NUMMER   EQ SPACE
                      WITH TABNAME  EQ SPACE
                      WITH TABKEY   EQ SPACE
                      WITH KEY_EXP  EQ 'X'
                      AND  RETURN.
    WHEN C_LONG.
*     Calls the long text processing function
      PERFORM PREPARE_LONG_TEXT.
    WHEN C_SAVE.
**      perform verify_entity_name.
        PERFORM CHECK_FOR_GAS_PLANT.
        IF NOT ROIU_PR_DN-GPLT_DN_NO IS INITIAL.
           PERFORM CHECK_VALID_GASPLANT_NTWK_ID.
        ENDIF.
      check W_FCODE = c_save.
*     Stop processing when save is selected but no changes have occured
      IF DELIVERY_NETWORK_TB1 EQ ROIU_PR_DN
         AND W_LONG_TEXT_WRITTEN_FG IS INITIAL.
*       Send user a message when no changes have occured on save
        PERFORM NO_CHANGES_MADE.
      ENDIF.
      CASE W_MODE.
        WHEN C_CREATE.
          PERFORM CREATE_DELIVERY_NETWORK.
        WHEN C_UPDATE.
          PERFORM UPDATE_DELIVERY_NETWORK.
      ENDCASE.
    WHEN C_BACK OR C_EXIT.
*     Checks to see if there are any changes if so popup to save is
*     activated.
      PERFORM CHECK_FOR_CHANGES.
* Change to delivery network dated transaction
    WHEN C_FCODE_DND1 OR C_FCODE_DND2 OR C_FCODE_DND3 OR C_FCODE_DND6.
      PERFORM CHANGE_TO_DN_DATED.
  ENDCASE.

  IF W_FCODE = C_BACK AND W_ANSWER <> C_CANCEL.
    IF SY-CALLD = C_TRUE  OR  SY-BINPT = C_TRUE.
      LEAVE TO SCREEN 0.
    ELSE.
      LEAVE TO TRANSACTION SY-TCODE.
    ENDIF.

  ELSEIF W_FCODE = C_EXIT AND W_ANSWER <> C_CANCEL.
      PERFORM CLEAR_INFORMATION.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_NUMBER_RANGE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_NUMBER_RANGE INPUT.

  IF W_MODE = C_CREATE.
*   Check to see if the number range group exists
*    PERFORM CHECK_FOR_NUMBER.
*   Get the number range information in order to get the number range
    PERFORM GET_NUMBER_RANGE_INFO.
*   If the number range is external lock the delivery network number
    IF W_INTERVAL_DS-EXTERNIND = C_FLAG_ON.
      PERFORM LOCK_DELIVERY_NUMBER.
    ENDIF.
*   Checks to see if the proper fields are populated based on internal
*   and external number ranges
    PERFORM CHECK_FOR_VALIDITY.
*   If the indicator is not external set del network number = INTER-001
    IF W_INTERVAL_DS-EXTERNIND NE C_TRUE.
      ROIU_PR_DN-DN_NO = C_INTERNAL.
    ENDIF.
  ENDIF.

ENDMODULE.                 " CHECK_NUMBER_RANGE  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_DELIVERY_NETWORK_DATA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_DELIVERY_NETWORK_DATA INPUT.

  CASE W_MODE.
    WHEN C_CREATE.
*     Check for a duplicate delivery network for external number assign
      PERFORM CHECK_FOR_DUPLICATE.
*     Check for a duplicate DN in Razorfish OIUH_PR_DN table    "N 1732281
      PERFORM CHECK_RAZORFISH_DN_DUPLICATE.                     "N 1732281
*     When using create by reference get the reference del network
      IF NOT ROIU_PR_DN-REF_DN_NO IS INITIAL.
        W_REF_FG = C_FLAG_ON.
*       get delivery network detail info for create by reference
        PERFORM GET_REF_DELIVERY_NETWORK USING ROIU_PR_DN-REF_DN_NO
                                               TEXT-001
                                               TEXT-002
                                               ROIU_PR_DN-REF_DN_NO
                                               W_REF_FG.
      ELSE.
        PERFORM DEFAULT_HIER_NODE.
      ENDIF.
    WHEN OTHERS.
      CLEAR W_REF_FG.
*     Get the delivery network detail information
      PERFORM GET_REF_DELIVERY_NETWORK USING ROIU_PR_DN-DN_NO
                                             TEXT-002
                                             ROIU_PR_DN-DN_NO
                                             SPACE
                                             W_REF_FG.
  ENDCASE.
ENDMODULE.                 " CHECK_DELIVERY_NETWORK_DATA  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_GAS_PLANT_NETWORK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*MODULE CHECK_GAS_PLANT_NETWORK INPUT.
*
*  IF W_MODE NE C_DISPLAY.
**   Validates required fields (company code/Joint Venture) when
**   gas plant delivery network type is selected
*    PERFORM CHECK_FOR_GAS_PLANT.
**   When the gas plant delivery network is filled make sure it is valid
**   gas plant network
*    IF NOT ROIU_PR_DN-GPLT_DN_NO IS INITIAL.
*      PERFORM CHECK_VALID_GASPLANT_NTWK_ID.
*    ENDIF.
*  ENDIF.
*
*ENDMODULE.                 " CHECK_GAS_PLANT_NETWORK  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_CNTRY_REGION_CITY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_CNTRY_REGION_CITY INPUT.

  IF W_MODE NE C_DISPLAY.
*   Check if country has a required city/state etc.
    CHECK W_FCODE <> C_FCODE_LOC.
    PERFORM CHECK_COUNTRY_AND_REGION.
  ENDIF.

ENDMODULE.                 " CHECK_CNTRY_REGION_CITY  INPUT
*&---------------------------------------------------------------------*
*&      Module  SET_FUNCTION_CODE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_FUNCTION_CODE INPUT.

  CLEAR W_FCODE.
  W_FCODE = OK_CODE.

ENDMODULE.                 " SET_FUNCTION_CODE  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_COMBOBOX_STATUS  INPUT
*&---------------------------------------------------------------------*
*  CALL PERFORM TO RESET COMBOBOXES BASED ON VALUE CHANGES.
*----------------------------------------------------------------------*
module check_combobox_status input.
* IF SY-DYNNR <> C_SCREEN_1200.
  PERFORM CHECK_COMBOBOX_STATUS.
* ENDIF.
endmodule.                 " CHECK_COMBOBOX_STATUS  INPUT


*&---------------------------------------------------------------------*
*&      Module  CHECK_SETID  INPUT
*&---------------------------------------------------------------------*
* VALIDATE THAT THE SETID THAT IS INPUT EXISTS IN THE STANDARD HIERARCHY
* FOR THE DELIVERY NETWORK SET CLASS.
*----------------------------------------------------------------------*
module check_setid input.
* IF SY-DYNNR = '1200'.
*  PERFORM CHECK_DN_SETID USING ROIU_PR_DN-DN_SET.
* ENDIF.
  PERFORM CHECK_SETID USING ROIU_PR_DN-DN_SET.

endmodule.                 " CHECK_SETID  INPUT

*&---------------------------------------------------------------------*
*&      Module  HELP_DN_SET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module help_dn_set input.


  W_PROGRAM = SY-CPROG.
  W_DYNNR   = SY-DYNNR.

  IF W_MODE <> C_CREATE AND
     W_MODE <> C_UPDATE.
    W_DISPLAY = C_FLAG_ON.
  ELSE.
    CLEAR W_DISPLAY.
  ENDIF.

  CALL FUNCTION 'OIU_HELP_STD_HIER_NODES'
       EXPORTING
            fieldname    = C_DN_SET_FNAME
            program      = W_PROGRAM
            dynnr        = W_DYNNR
            setclass     = C_SETCLASS
            DISPLAY_ONLY = W_DISPLAY
            .

endmodule.                 " HELP_MP_SET  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_DNDE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_DNDE INPUT.
* IF SY-DYNNR = '1200'.
** Check that required field description is populated
*  PERFORM CHECK_DESCRIPTION.
* ENDIF.
ENDMODULE.                 " CHECK_DNDE  INPUT

*&---------------------------------------------------------------------*
*&      Module  CHECK_TOL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CHECK_TOL INPUT.
  perform check_allocation_tolerance.
ENDMODULE.                 " CHECK_TOL  INPUT
