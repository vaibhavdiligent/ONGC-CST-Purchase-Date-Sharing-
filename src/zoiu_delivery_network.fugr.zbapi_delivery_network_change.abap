FUNCTION ZBAPI_DELIVERY_NETWORK_CHANGE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(BAPIOIUDNHEADER) LIKE  BAPI_OIU_DN STRUCTURE  BAPI_OIU_DN
*"     VALUE(BAPIOIUDN1DETAIL) LIKE  BAPI_OIU_DN_1 STRUCTURE
*"       BAPI_OIU_DN_1
*"  EXPORTING
*"     VALUE(BAPIOIUDNHEADERE) LIKE  BAPI_OIU_DN STRUCTURE  BAPI_OIU_DN
*"     VALUE(BAPIOIUDN1DETAILE) LIKE  BAPI_OIU_DN_1 STRUCTURE
*"       BAPI_OIU_DN_1
*"  TABLES
*"     RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: BEGIN OF ORIG_OIU_PR_DN OCCURS 1.
          INCLUDE STRUCTURE ROIU_PR_DN.
  DATA: END OF ORIG_OIU_PR_DN.

  DATA: CHANGE_OIU_PR_DN LIKE ORIG_OIU_PR_DN.
* Clear message table and set bapi flag
  REFRESH G_RETURN_TB.
* CLEAR ALL WORKING STORAGE.
  PERFORM CLEAR_INFORMATION.

  CLEAR G_BAPI_STOP.
  G_BAPI_FG = C_TRUE.

* Authority check
  perform check_bapi_authorization using 'O3U_DN2'.

  MOVE-CORRESPONDING BAPIOIUDNHEADER  TO CHANGE_OIU_PR_DN.
  MOVE-CORRESPONDING BAPIOIUDN1DETAIL TO CHANGE_OIU_PR_DN.
  CHANGE_OIU_PR_DN-BUKRS = BAPIOIUDN1DETAIL-COMP_CODE.
  CHANGE_OIU_PR_DN-VNAME = BAPIOIUDN1DETAIL-VENTURE.

* changes all field to uppercase except for description
  PERFORM TRANSLATE_TEXTS.
* Get the delivery network detail information
  PERFORM GET_REF_DELIVERY_NETWORK USING CHANGE_OIU_PR_DN-DN_NO
                                         TEXT-002
                                         CHANGE_OIU_PR_DN-DN_NO
                                         SPACE
                                         SPACE.
* If the delivery network does not exist
  CHECK_BAPI_STOP.
* Move the old values from the structure into temporary internal table
  MOVE-CORRESPONDING ROIU_PR_DN TO ORIG_OIU_PR_DN.
* Move the new values into the old structure for edits and validations
  CHANGE_OIU_PR_DN-DN_NO   = ROIU_PR_DN-DN_NO.
  CHANGE_OIU_PR_DN-TYPE_CD = ROIU_PR_DN-TYPE_CD.
  CHANGE_OIU_PR_DN-DN_GRP  = ROIU_PR_DN-DN_GRP.
  MOVE-CORRESPONDING CHANGE_OIU_PR_DN TO ROIU_PR_DN.

  IF ORIG_OIU_PR_DN NE CHANGE_OIU_PR_DN.
*    validate the fields required
    PERFORM BAPI_EDIT_CHECKS_CHANGE.
*    If edits and validations have an error pass messages to user
    CHECK_BAPI_STOP.
*    Lock the record from changes
    PERFORM LOCK_DELIVERY_NUMBER.
*    Enqueue of DN Standard Hierarchy              " Start SOGK013053 EY
    IF ORIG_OIU_PR_DN-dn_set <> CHANGE_OIU_PR_DN-dn_set.
      PERFORM enqueue_setid USING ORIG_OIU_PR_DN-dn_set
                                  CHANGE_OIU_PR_DN-dn_set.
    ENDIF.
    CHECK_BAPI_STOP.                               " End   SOGK013053 EY
*    Prepare all the fields for the save.
    PERFORM PREPARE_UPDATE_FOR_SAVE.
*    Call the function to update the table
    PERFORM SAVE_DEL_NET_DATA.
    Perform F_UPDATE_HIERARCHY.                     "SOGK027671 VV
    CHECK_BAPI_STOP.
*    Call the function to update the change document
    PERFORM CHANGE_DOCUMENT USING ORIG_OIU_PR_DN
                                  ROIU_PR_DN.
*    Error message of the change document failed.
    CHECK_BAPI_STOP.
*    Sends user a success message
    PERFORM SAVE_SUCCESSFUL.
*    Return success message from the save and exits the function module
    RETURN[] = G_RETURN_TB[].
*    Move the new records to the return structure
    MOVE-CORRESPONDING ROIU_PR_DN TO BAPIOIUDNHEADERE.
    MOVE-CORRESPONDING ROIU_PR_DN TO BAPIOIUDN1DETAILE.
  ENDIF.

ENDFUNCTION.
