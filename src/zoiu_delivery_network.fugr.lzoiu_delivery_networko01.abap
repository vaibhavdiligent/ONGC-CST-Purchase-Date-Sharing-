*----------------------------------------------------------------------*
***INCLUDE LZOIU_DELIVERY_NETWORKO01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SCREEN_0100_INTIALIZATION  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SCREEN_0100_INTIALIZATION OUTPUT.

  IF W_MODE IS INITIAL.
*   Get the mode from the transaction code
    CALL FUNCTION 'OIU_CM_TXN_CTL'
         EXPORTING
              I_TCODE    = SY-TCODE
         IMPORTING
              E_MCODE    = W_MODE
              E_MCDTXT   = W_TTLTXT
              E_OIUFORMAT = W_FORMAT
        EXCEPTIONS
              NONE_FOUND = 1
              OTHERS     = 2.
    IF SY-SUBRC NE 0.
      PERFORM SET_RETURN_MESSAGE USING SY-MSGTY
                                       SY-MSGID
                                       SY-MSGNO
                                       SPACE
                                       SPACE
                                       SY-MSGV1
                                       SY-MSGV2
                                       SY-MSGV3
                                       SY-MSGV4
                                       SPACE
                                       SPACE
                                       SPACE.
    ENDIF.
  ENDIF.

* Set parameter ids for screen fields
  PERFORM SET_PARAMETER_ID.
* Set the status and the titlebar based on the mode
  REFRESH FCODE_TB.

  FCODE_TB = SY-TCODE.
  APPEND FCODE_TB.

  CALL FUNCTION 'OIUPR_GT_STATUS_CHK'
       TABLES
         t_fcodes = FCODE_TB.

  IF SY-DYNNR = C_SCREEN_1100.
   SET PF-STATUS '0100' EXCLUDING FCODE_TB.
   SET TITLEBAR  '0100' WITH      W_TTLTXT.
  ELSE.
   SET PF-STATUS SY-DYNNR EXCLUDING FCODE_TB.
   SET TITLEBAR  SY-DYNNR WITH      W_TTLTXT.
  ENDIF.

* Turn create by reference field off when not in create mode
  IF W_MODE NE C_CREATE.
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 1.
        SCREEN-ACTIVE = C_OFF_FG.
        MODIFY SCREEN.
      Elseif screen-group2 = 1.
        screen-required = 1.
        modify screen.
      endif.
    ENDLOOP.
  ENDIF.
 IF SY-DYNNR = C_SCREEN_1100.
  PERFORM GET_TYPECD_TEXTS.
 ENDIF.

ENDMODULE.                 " SCREEN_0100_INTIALIZATION  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SCREEN_0200_INITIALIZATION  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SCREEN_0200_INITIALIZATION OUTPUT.

  REFRESH FCODE_TB.

  IF W_MODE = C_DISPLAY.
    FCODE_TB = C_SAVE.
    APPEND FCODE_TB.
  ENDIF.
  IF W_MODE = C_CREATE.
    FCODE_TB = C_ADMN.
    APPEND FCODE_TB.
  ENDIF.

  FCODE_TB = W_MODE.
  APPEND FCODE_TB.

  CALL FUNCTION 'OIUPR_GT_STATUS_CHK'
       TABLES
         t_fcodes = FCODE_TB.

*  IF SY-DYNNR = C_SCREEN_1200.
*   SET PF-STATUS '0200' EXCLUDING FCODE_TB.
*   SET TITLEBAR  '0200' WITH W_TTLTXT.
*  ELSE.
   SET PF-STATUS SY-DYNNR EXCLUDING FCODE_TB.
   SET TITLEBAR  SY-DYNNR WITH W_TTLTXT.
*  ENDIF.

* Turn all screen fields to display when in display mode
    LOOP AT SCREEN.
      IF SCREEN-GROUP1 = 1 AND W_MODE = C_DISPLAY.
        SCREEN-INPUT = C_OFF_FG.
        MODIFY SCREEN.
      ENDIF.

      IF ROIU_PR_DN-TYPE_CD NE 'P' AND SCREEN-GROUP3 = 3.
        SCREEN-ACTIVE = C_OFF_FG.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.


ENDMODULE.                 " SCREEN_0200_INITIALIZATION  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  SET_TEXTS_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE SET_TEXTS_SCREEN OUTPUT.

* Get the country/state/county texts for screen 200
  PERFORM GET_COUNTRY_TEXTS.
   IF SY-DYNNR = C_SCREEN_1100.
*   IF SY-DYNNR = C_SCREEN_1100 OR
*      SY-DYNNR = C_SCREEN_1200.
** Get the type code texts for the delivery network
   PERFORM GET_TYPECD_TEXTS.
  ENDIF.
* Get the company code/JV/gas plant delivery network text
  PERFORM GET_GAS_PLANT_TEXTS.

ENDMODULE.                 " SET_TEXTS_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  POPULATE_COMBOBOXES  OUTPUT
*&---------------------------------------------------------------------*
*  POPULATE PRIMARY AND SECONDARY GEOGRAPHIC LOCATION COMBOBOXES BASED
*  ON WHETHER THE SCREEN FIELD HAS THE SAME VALUE AS WAS USED TO
*  POPULATE THE VALUE LAST.
*----------------------------------------------------------------------*
MODULE POPULATE_COMBOBOXES OUTPUT.

DATA: TABNAME LIKE DD03V-TABNAME.
DATA: FIELDNAME LIKE DD03V-FIELDNAME.
DATA: VALUESET_ID(80) TYPE C.

  IF W_CNTRY <> ROIU_PR_DN-CNTRY_CD.
    W_CNTRY = ROIU_PR_DN-CNTRY_CD.
    W_PRIMARY = ROIU_PR_DN-PRI_GEO_LOC.

    TABNAME = 'ROIU_PR_DN'.
    FIELDNAME = 'PRI_GEO_LOC'.
    CONCATENATE TABNAME '-' FIELDNAME INTO VALUESET_ID.

    CALL FUNCTION 'VRM_DELETE_VALUES'
         EXPORTING
              id           = VALUESET_ID
         EXCEPTIONS
              ID_NOT_FOUND = 1
              OTHERS       = 2
              .

    CALL FUNCTION 'DD_SHLP_COMBOBOX_FILL'
         EXPORTING
              tabname        =  TABNAME
              fieldname      =  FIELDNAME
              valueset_id    =  VALUESET_ID
*        IMPORTING
*             RC             =
*        TABLES
*             VALUE_PAIR_TAB =
              .
    TABNAME = 'ROIU_PR_DN'.
    FIELDNAME = 'SEC_GEO_LOC'.
    CONCATENATE TABNAME '-' FIELDNAME INTO VALUESET_ID.

    CALL FUNCTION 'VRM_DELETE_VALUES'
         EXPORTING
              id           = VALUESET_ID
         EXCEPTIONS
              ID_NOT_FOUND = 1
              OTHERS       = 2
              .

    CALL FUNCTION 'DD_SHLP_COMBOBOX_FILL'
         EXPORTING
              tabname        =  TABNAME
              fieldname      =  FIELDNAME
              valueset_id    =  VALUESET_ID
*        IMPORTING
*             RC             =
*        TABLES
*             VALUE_PAIR_TAB =
              .
  ELSEIF W_PRIMARY <> ROIU_PR_DN-PRI_GEO_LOC.

    W_PRIMARY = ROIU_PR_DN-PRI_GEO_LOC.
    TABNAME = 'ROIU_PR_DN'.
    FIELDNAME = 'SEC_GEO_LOC'.
    CONCATENATE TABNAME '-' FIELDNAME INTO VALUESET_ID.

    CALL FUNCTION 'VRM_DELETE_VALUES'
         EXPORTING
              id           = VALUESET_ID
         EXCEPTIONS
              ID_NOT_FOUND = 1
              OTHERS       = 2
              .

    CALL FUNCTION 'DD_SHLP_COMBOBOX_FILL'
         EXPORTING
              tabname        =  TABNAME
              fieldname      =  FIELDNAME
              valueset_id    =  VALUESET_ID
*        IMPORTING
*             RC             =
*        TABLES
*             VALUE_PAIR_TAB =
              .
  ENDIF.

ENDMODULE.                 " POPULATE_COMBOBOXES  OUTPUT
