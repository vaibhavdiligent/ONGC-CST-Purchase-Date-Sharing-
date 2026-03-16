FUNCTION ZIDOC_OUT_OIU_DELIVERY_NETWK.
*"----------------------------------------------------------------------
*"*"Update function module:
*"
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_ROIU_PR_DN) LIKE  ROIU_PR_DN STRUCTURE  ROIU_PR_DN
*"     VALUE(REFRESH) TYPE  CHAR1 OPTIONAL
*"----------------------------------------------------------------------
  DATA: W_LS LIKE BDI_LOGSYS-LOGSYS.
  DATA: BEGIN OF CNTRL_DATA OCCURS 0.
          INCLUDE STRUCTURE EDP13.
  DATA: END OF CNTRL_DATA.
  DATA: W_MESTYP LIKE EDP13-MESTYP.
  data: C_RFSH_IDOC_MESTYP LIKE EDP13-MESTYP VALUE 'OIURDN1'.

  CLEAR: CNTRL_DATA,
         IDOC_CNTRL.
  REFRESH: CNTRL_DATA,
           IDOC_CNTRL.

  IF REFRESH IS INITIAL.
    W_MESTYP = C_IDOC_MESTYP.
  ELSE.
    W_MESTYP = C_RFSH_IDOC_MESTYP.
  ENDIF.

*- set control data ----------------------------4.6A-----------------------*
  CALL FUNCTION 'OIU_GET_LS_PARTNER_PROFILES'
       EXPORTING
            I_IDOC_MESTYP           = W_MESTYP
       IMPORTING
            I_LS                    = W_LS
       TABLES
            I_CNTRL_DATA            = CNTRL_DATA
       EXCEPTIONS
            NO_LOGICAL_SYSTEM_FOUND = 1
            NO_PARTNER_DATA_FOUND   = 2
            OTHERS                  = 3.

  IF SY-SUBRC = 0.
    PERFORM FILL_CONTROL_SEGMENT TABLES IDOC_CNTRL
                                        CNTRL_DATA.
*Fill the internal table IDOC_DATA -----------------------------------------*
    PERFORM FILL_DATA_SEGMENT USING I_ROIU_PR_DN.
*Create the IDOC -----------------------------------------------------------*
    PERFORM SEND_IDOC.
  ENDIF.
ENDFUNCTION.
