FUNCTION ZBAPI_DELIVERY_NETWORK_CREATE.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(BAPIOIUDNHEADER) LIKE  BAPI_OIU_DN
*"       STRUCTURE  BAPI_OIU_DN
*"     VALUE(BAPIOIUDN1DETAIL) LIKE  BAPI_OIU_DN_1
*"       STRUCTURE  BAPI_OIU_DN_1
*"  EXPORTING
*"     VALUE(BAPIOIUDNHEADERE) LIKE  BAPI_OIU_DN
*"       STRUCTURE  BAPI_OIU_DN
*"     VALUE(BAPIOIUDN1DETAILE) LIKE  BAPI_OIU_DN_1
*"       STRUCTURE  BAPI_OIU_DN_1
*"  TABLES
*"     RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
  CLEAR: RETURN.
* Clear message table and set bapi flag
  REFRESH G_RETURN_TB.
* CLEAR ALL WORKING STORAGE.
  PERFORM CLEAR_INFORMATION.

* Bring bapi passed variables into program structure
  MOVE-CORRESPONDING BAPIOIUDNHEADER TO ROIU_PR_DN.
  MOVE-CORRESPONDING BAPIOIUDN1DETAIL TO ROIU_PR_DN.
  ROIU_PR_DN-BUKRS = BAPIOIUDN1DETAIL-COMP_CODE.
  ROIU_PR_DN-VNAME = BAPIOIUDN1DETAIL-VENTURE.

  W_MODE = C_CREATE.
  G_BAPI_FG = C_TRUE.

* Authority check
  perform check_bapi_authorization using 'O3U_DN1'.

* changes the description texts to uppercase
  PERFORM TRANSLATE_TEXTS.
  REFRESH G_RETURN_TB.
  CLEAR G_BAPI_STOP.
* Get number range and begin creation of delivery network
  IF ROIU_PR_DN-DN_GRP IS INITIAL.
    PERFORM CHECK_SINGLE_GRP.
  ENDIF.
  perform CHECK_FOR_NUMBER.
  PERFORM NUMBER_RANGE_CHECK_BAPI.
* Make bapi edit checks.
  PERFORM BAPI_EDIT_CHECKS.
* Return all messages from the edit checks and exit the function module
  CHECK_BAPI_STOP.
*  IF g_bapi_stop = c_true.
*    return[] = g_return_tb[].
*    EXIT.
*  ENDIF.
* Check for create by reference and get reference fields if necessary
*  IF NOT ROIU_PR_DN-REF_DN_NO IS INITIAL.
*    W_REF_FG = C_FLAG_ON.
*    PERFORM GET_REF_DELIVERY_NETWORK USING ROIU_PR_DN-REF_DN_NO
*                                           TEXT-001
*                                           TEXT-002
*                                           ROIU_PR_DN-REF_DN_NO
*                                           SPACE.
*  ENDIF.
*
* Validate that the set id passed is valid for the standard hierarchy.
  PERFORM CHECK_SETID USING ROIU_PR_DN-DN_SET.
* Get unique delivery network number set up internal table data
* and insert data into db table
  PERFORM GET_DEL_NETWORK_NUMBER.
* Check for DN number in Razorfish table                    "N 1736401
  PERFORM check_razorfish_dn_duplicate.                     "N 1736401
* Lock the delivery network unique number once you get it
  PERFORM LOCK_DELIVERY_NUMBER.
* Enqueue DN Standard Hierarchy                       " SOGK013053 EY
  PERFORM enqueue_setid USING space ROIU_PR_DN-DN_SET. " SOGK013053 EY
  CHECK_BAPI_STOP.                                    " SOGK013053 EY
* Set all data in internal tables before the save
  PERFORM PREPARE_CREATE_FOR_SAVE.
* Calls the funuction module to insert the new record into the DB
  PERFORM SAVE_DEL_NET_DATA.
  Perform F_UPDATE_HIERARCHY.                          "SOGK027671 VV
* Return error message from the save routine and exits the function mod
  CHECK_BAPI_STOP.
*  IF g_bapi_stop = c_true.
*    return[] = g_return_tb[].
*    EXIT.
*  ENDIF.
* Sends user a success message
  PERFORM SAVE_SUCCESSFUL.
* Return success message from the save and exits the function module
  RETURN[] = G_RETURN_TB[].

* Move the new records to the return structure
  MOVE-CORRESPONDING ROIU_PR_DN TO BAPIOIUDNHEADERE.
  MOVE-CORRESPONDING ROIU_PR_DN TO BAPIOIUDN1DETAILE.

ENDFUNCTION.
