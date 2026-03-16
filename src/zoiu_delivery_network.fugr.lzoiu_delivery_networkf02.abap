* *
*   INCLUDE LZOIU_DELIVERY_NETWORKF02                                   *
* *
*******************************************************************
*THE FOLLOWING SUBROUTINES DEAL WITH IDOC OUTPUT CREATION *
*& *
*&      Form  FILL_CONTROL_SEGMENT
*& *
*       text - FILL THE TRANSACTIONAL RFC INFORMATION IN THE CONTROL -
*              TABLE
* *
*      >IDOC_CNTRL  text
*      >C_IDOC_MESTYP  text
* *
FORM FILL_CONTROL_SEGMENT TABLES   IDOC_CNTRL STRUCTURE EDIDC
                                   CNTRL_DATA STRUCTURE EDP13.
 LOOP AT CNTRL_DATA.
  CLEAR IDOC_CNTRL.
    IDOC_CNTRL-MANDT    = SY-MANDT.
    IDOC_CNTRL-SERIAL   = SY-DATUM.
    IDOC_CNTRL-SERIAL+8 = SY-UZEIT.
    IDOC_CNTRL-MESTYP   = CNTRL_DATA-MESTYP.
    IDOC_CNTRL-DOCTYP   = C_IDOC_DOCTYP.
    IDOC_CNTRL-DIRECT   = C_IDOC_DIRECT.
    IDOC_CNTRL-RCVPRT   = CNTRL_DATA-RCVPRT.
    IDOC_CNTRL-RCVPRN   = CNTRL_DATA-RCVPRN.
    IDOC_CNTRL-CIMTYP   = CNTRL_DATA-CIMTYP.
    APPEND IDOC_CNTRL.
  ENDLOOP.

ENDFORM.                    " FILL_CONTROL_SEGMENT

*& *
*&      Form  FILL_DATA_SEGMENT
*& *
*       text - FILL THE SEGMENT DATA INTO THE EDIDD STRUCTURE
* *
*      >IDOC_DATA  text
* *
FORM FILL_DATA_SEGMENT USING   I_ROIU_PR_DN STRUCTURE ROIU_PR_DN.
  CLEAR: IDOC_DATA,
         E1OIUDN.
  REFRESH: IDOC_DATA.
  MOVE-CORRESPONDING I_ROIU_PR_DN TO E1OIUDN.
  E1OIUDN-OIU_DATUM     = SY-DATUM.
  E1OIUDN-OIU_UZEIT     = SY-UZEIT.
*For action codes D or U insert the change user into the field 'CRUSER'
*create user.  For action code 'I' leave the create user in this field
   IF NOT E1OIUDN-XXDBACTION = 'I'.
      E1OIUDN-OIU_CRUSER = SY-UNAME.
   ENDIF.

  MOVE E1OIUDN TO IDOC_DATA-SDATA.
  IDOC_DATA-SEGNAM      = 'E1OIUDN'.
  APPEND IDOC_DATA.

ENDFORM.                    " FILL_DATA_SEGMENT

*& *
*&      Form  SEND_IDOC
*& *
*       text SEND THE OUTBOUND IDOC
* *
*  >  p1        text
*  <   p2        text
* *
FORM SEND_IDOC.
DATA: BEGIN OF COMMUNICATION_IDOC_CONTROL OCCURS 0.
          INCLUDE STRUCTURE EDIDC.
DATA: END OF COMMUNICATION_IDOC_CONTROL .

LOOP AT IDOC_CNTRL.
 CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
      EXPORTING
           MASTER_IDOC_CONTROL            = IDOC_CNTRL
*          OBJ_TYPE                       = ''
*          CHNUM                          = ''
      TABLES
           COMMUNICATION_IDOC_CONTROL     = COMMUNICATION_IDOC_CONTROL
           MASTER_IDOC_DATA               = IDOC_DATA
      EXCEPTIONS
           ERROR_IN_IDOC_CONTROL          = 1
           ERROR_WRITING_IDOC_STATUS      = 2
           ERROR_IN_IDOC_DATA             = 3
           SENDING_LOGICAL_SYSTEM_UNKNOWN = 4.

 IF SY-SUBRC <> 0.
  PERFORM SET_RETURN_MESSAGE USING  SY-MSGTY
                                    C_MESSAGEID_CM
                                     SY-MSGNO
                                     SPACE
                                     SPACE
                                     TEXT-IDO
                                     SY-MSGV1
                                     SY-MSGV2
                                     SY-MSGV3
                                     SPACE
                                     SPACE
                                     SPACE.
 ENDIF.

ENDLOOP.
ENDFORM.                    " SEND_IDOC
