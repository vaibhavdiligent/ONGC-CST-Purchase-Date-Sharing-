FUNCTION ZOIU_GET_DELIVERY_NETWORK.
*"----------------------------------------------------------------------
*"*"Local interface:
*"  IMPORTING
*"     VALUE(I_DN_NO) LIKE  ZOIU_PR_DN-DN_NO
*"  EXPORTING
*"     VALUE(E_ROIU_PR_DN) LIKE  ROIU_PR_DN
*"       STRUCTURE  ROIU_PR_DN
*"     VALUE(E_NOT_FOUND) LIKE  ROIU_PR_DN-XXDBACTION
*"  EXCEPTIONS
*"     NOT_FOUND
*"----------------------------------------------------------------------

************************************************************************
*            Master data select using full key                         *
************************************************************************

    SELECT SINGLE * FROM ZOIU_PR_DN
                    WHERE DN_NO = I_DN_NO.

    IF SY-SUBRC NE 0.
      E_NOT_FOUND = C_TRUE.
    ELSE.
      E_ROIU_PR_DN = ZOIU_PR_DN.
    ENDIF.

ENDFUNCTION.
