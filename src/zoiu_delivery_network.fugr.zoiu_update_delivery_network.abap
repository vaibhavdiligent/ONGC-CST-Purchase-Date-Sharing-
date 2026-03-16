FUNCTION ZOIU_UPDATE_DELIVERY_NETWORK.
*"----------------------------------------------------------------------
*"*"Update function module:
*"
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_DELIVERY_NETWORK) LIKE  ROIU_PR_DN STRUCTURE  ROIU_PR_DN
*"----------------------------------------------------------------------

  DATA: BEGIN OF dn_tb OCCURS 0.
          INCLUDE STRUCTURE ROIU_PR_DN.
  DATA: END OF dn_tb.

  CLEAR:   dn_tb.
  REFRESH: dn_tb.

    MOVE-CORRESPONDING I_DELIVERY_NETWORK TO ZOIU_PR_DN.
    CASE I_DELIVERY_NETWORK-XXDBACTION.
      WHEN 'I'.
          INSERT ZOIU_PR_DN.
      WHEN 'U'.
          UPDATE ZOIU_PR_DN.
    ENDCASE.

    IF SY-SUBRC <> 0.
        MESSAGE A899(OIUCM) WITH text-035.
    ENDIF.

*Razorfish Integration starts here

  MOVE-CORRESPONDING i_delivery_network to dn_tb.
  APPEND dn_tb.

    CALL FUNCTION 'OIUHS_INTG_PR_DN35'
        TABLES
              IN_PR_DN = DN_TB
        .

*Razorfish Integration ends here

ENDFUNCTION.
