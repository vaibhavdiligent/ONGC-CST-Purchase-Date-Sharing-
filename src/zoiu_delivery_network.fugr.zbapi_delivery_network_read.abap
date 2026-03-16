FUNCTION ZBAPI_DELIVERY_NETWORK_READ.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(BAPIDNNUMBER) LIKE  BAPI_OIU_DN-DN_NO
*"  EXPORTING
*"     VALUE(BAPIOIUDNHEADERE) LIKE  BAPI_OIU_DN
*"       STRUCTURE  BAPI_OIU_DN
*"     VALUE(BAPIOIUDN1DETAILE) LIKE  BAPI_OIU_DN_1
*"       STRUCTURE  BAPI_OIU_DN_1
*"  TABLES
*"     RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------
* Clear message table and set bapi flag
  REFRESH G_RETURN_TB.
* CLEAR ALL WORKING STORAGE.
  PERFORM CLEAR_INFORMATION.

  G_BAPI_FG = C_TRUE.

* Authority check
  perform check_bapi_authorization using 'O3U_DN3'.

  ROIU_PR_DN-DN_NO = BAPIDNNUMBER.

* Get the delivery network detail information
  PERFORM GET_REF_DELIVERY_NETWORK USING ROIU_PR_DN-DN_NO
                                         TEXT-002
                                         ROIU_PR_DN-DN_NO
                                         SPACE
                                         SPACE.

  IF G_BAPI_STOP = C_TRUE.
    RETURN[] = G_RETURN_TB[].
    EXIT.
  ELSE.
    MOVE-CORRESPONDING ROIU_PR_DN TO BAPIOIUDNHEADERE.
    MOVE-CORRESPONDING ROIU_PR_DN TO BAPIOIUDN1DETAILE.
  ENDIF.




ENDFUNCTION.
