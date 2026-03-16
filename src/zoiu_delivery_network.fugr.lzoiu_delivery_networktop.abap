FUNCTION-POOL ZOIU_DELIVERY_NETWORK          MESSAGE-ID OIUCM.
***************************************************************
* Project        : E&P (Exploration and Production)
* Program Name   : SAPLZOIU_DELIVERY_NETWORK
* Program Title  : Delivery Network Identification
* Date Written   : 06/02/99
* Author         : Gregory A. Kisver
* Description    : (Detailed description of the program and its purpose)
* Tables Updated : List the tables actually updated by this program
* Special Logic  : Describe any special logic within the program
* Includes       : List includes and their purpose
* Return Codes   : List all return codes and description
* Input  Parameters for Includes/Function Modules:
* List the input parameters and a description
* Output Parameters for Includes/function Modules:
* List the output parameters and a description
***************************************************************
* *
*               C H A N G E   H I S T O R Y   B L O C K                *
* *
*>> Please copy the following lines of code when a change is made to *
*>> this program and fill the change information                     *
* *
* Change Request Number:
*  (- New lines of code: add this number to the end of the line of code.
*   - Lines to be changed: copy the line prior to changing it, comment
*     the line to be changed out and change the copied line. Add this
*     number to both lines of code.
*   - Lines to be deleted: Comment these lines out and add this number
*     to the lines of code.)
* Change date: MM/DD/YYYY
* Changed by : (Name of person making changes)
* Description: (Detailed description of change)
* *
TABLES:  T005T,
         T001,
         T8JVT,
         T8JV,
         DD07V,
         oiu_do_jv,
         OIU_CM_LXPGL_TX,
         OIU_CM_LXSGL_TX,
         OIU_PR_DN_GRP_TX,
         OIU_CM_TXN_CTL,
         OIU_CM_EDIT,
         OIU_PR_DN_GRP,
         ROIU_PR_DN,
         ROIU_ADMINISTRATIVE,
         SETHEADERT,
         EDP13,                   "IDOC RFC CONTROL TABLE
         E1OIUDN.                 "IDOC DELIVERY NETWORK SEGMENT


* *
*                       Working Variables                              *
* *
DATA:        W_FCODE       LIKE   SY-UCOMM,
             W_MODE        LIKE   OIU_CM_TXN_CTL-MCODE,
             W_NEW_MODE    LIKE   W_MODE,
             W_TTLTXT      LIKE   SY-MSGV1,
             W_REF_FG      TYPE  C,                   "Create by ref flag
             W_ANSWER      TYPE  C,
             W_LONG_TEXT_READ_FG          TYPE C,
             W_LONG_TEXT_FG               TYPE C,
             W_TEMP_LONG_TEXT_WRITTEN_FG  TYPE C,
             W_LONG_TEXT_WRITTEN_FG       TYPE C,
             W_DOMAIN_VALUE LIKE DD07V-DOMVALUE_L,
             W_DDTEXT       LIKE DD07V-DDTEXT,
             W_GEO_REQUIRED_FG   TYPE C,
             W_SEC_REQUIRED_FG   TYPE C,
             W_CNTRY             LIKE OIU_PR_DN-CNTRY_CD,
             W_PRIMARY           LIKE OIU_PR_DN-PRI_GEO_LOC,
             OK_CODE             LIKE SY-UCOMM,
             W_NO_IDOC           TYPE C,
             W_PROGRAM           LIKE SY-CPROG,
             W_DYNNR             LIKE SY-DYNNR,
             W_DISPLAY(1)        TYPE C,
             W_OLD_SET           LIKE SETHEADER-SETNAME,
             W_NEW_SET           LIKE SETHEADER-SETNAME,
             W_SET_OBJECT        LIKE SETVALUES-FROM.
*                                                    Start SOGK013053 EY
  Data:
        w_1st_time(1)            type c,
        w_HIER_ACTIVE            like OIU_CM_PARAM-EP_ACTIVE.

Define m_HIER_ACTIVE.
* Get the E&P set parameters once
  If w_1st_time is initial.
     w_1st_time  = c_yes.

     SELECT SINGLE
            HIER_active
       INTO w_HIER_ACTIVE
       FROM oiu_cm_param
     CLIENT SPECIFIED
      WHERE mandt = sy-mandt.
  EndIf.
END-OF-DEFINITION.
*                                                    Start SOGK013053 EY
* *
*                         CONSTANTS                                    *
* *
CONSTANTS:   C_OFF_FG      TYPE  C          VALUE 0,     "Screen Off Flag
             C_gas_plant_jv   like oiu_do_jv-vname value '04',
             C_FLAG_ON     TYPE  C          VALUE 'X',   "Flag On
             C_FLAG_OFF    TYPE  C          VALUE ' ',   "Flag Off
             C_YES         TYPE  C          VALUE 'J',   "Yes answer
             C_UPDATE_ACT  TYPE  C          VALUE 'U',   "DB Update
             C_INSERT_ACT  TYPE  C          VALUE 'I',   "DB Insert
             C_CREATE(2)   TYPE  C          VALUE '01',    "Create Mode
             C_UPDATE(2)   TYPE  C          VALUE '02',    "Update Mode
             C_DISPLAY(2)  TYPE  C          VALUE '03',   "Display Mode
             C_ENTER       LIKE  SY-UCOMM   VALUE 'ENTR', "Enter fcode
             C_SAVE        LIKE  SY-UCOMM   VALUE 'SAVE', "Save  fcode
             C_BACK        LIKE  SY-UCOMM   VALUE 'BACK', "Back  fcode
             C_EXIT        LIKE  SY-UCOMM   VALUE 'EXIT', "Exit  fcode
             C_ADMN        LIKE  SY-UCOMM   VALUE 'ADMN', "Admin Data fc
             C_OBJC        LIKE  SY-UCOMM   VALUE 'OBJC', "object changes
             C_OTHR        LIKE  SY-UCOMM   VALUE 'OTHR', "new fcode
             C_MESSAGEID_CM  LIKE SY-MSGID VALUE 'OIUCM', "msg class CM
             C_MESSAGEID_PR  LIKE SY-MSGID VALUE 'OIUPR', "msg class PR
             C_MESSAGETYPE_S LIKE SY-MSGTY VALUE 'S',     "success Msg
             C_MESSAGETYPE_W LIKE SY-MSGTY VALUE 'W',     "Warning Msg
             C_MESSAGETYPE_E LIKE SY-MSGTY VALUE 'E',     "Error Msg
*             C_NUM_RNG_OBJ   LIKE INRI-OBJECT VALUE 'OIU_DN',
             C_INTERNAL(11)  TYPE C        VALUE 'INTERN-0001',
             C_DUMMY         LIKE SY-MSGV1 VALUE SPACE,   "Dummy funct
             C_GAS_PLANT     TYPE C        VALUE 'P',
             C_FUEL_SYSTEM   TYPE C        VALUE 'F',
             C_CLUSTER       TYPE C        VALUE 'C',
             C_GATHERING_SYS TYPE C        VALUE 'N',
             C_US            LIKE OIU_CM_LXPGL-PRI_GEO_LOC VALUE 'US',
             C_CANADA        LIKE C_US     VALUE 'CA',
             C_LONG          LIKE SY-UCOMM VALUE 'LONG',
             C_LONGTXT(7)    TYPE C        VALUE 'LONGTXT',
             C_LTXT          LIKE THEAD-TDID VALUE 'LTXT',
             C_DN_TAB_NAME(9) TYPE C       VALUE 'OIU_PR_DN',
             C_DN_TYPECD_DOM  LIKE DD07V-DOMNAME VALUE 'OIU_DN_TYP_CD',
             C_ZERO            TYPE C     VALUE '0',
             C_LT_THOUSAND(3)  TYPE C     VALUE '999',
             C_CANCEL          TYPE C     VALUE 'A',
             C_TYPE_CD         LIKE OIU_PR_DN-TYPE_CD VALUE 'N',
             C_FCODE_DND1      LIKE SY-UCOMM    VALUE 'DND1',
             C_FCODE_DND2      LIKE SY-UCOMM    VALUE 'DND2',
             C_FCODE_DND3      LIKE SY-UCOMM    VALUE 'DND3',
             C_FCODE_DND6      LIKE SY-UCOMM    VALUE 'DND6',
             C_FCODE_LOC       LIKE SY-UCOMM    VALUE 'LOC',
             C_TRANS_DND1      LIKE SY-UCOMM    VALUE 'O3U_DND1',
             C_TRANS_DND2      LIKE SY-UCOMM    VALUE 'O3U_DND2',
             C_TRANS_DND3      LIKE SY-UCOMM    VALUE 'O3U_DND3',
             C_TRANS_DND6      LIKE SY-UCOMM    VALUE 'O3U_DND6',
             C_SCREEN_1100     LIKE SY-DYNNR    VALUE '1100',
*             C_SCREEN_1200     LIKE SY-DYNNR    VALUE '1200',
             C_DN_SET_FNAME    LIKE DYNPREAD-FIELDNAME
                               VALUE 'ROIU_PR_DN-DN_SET',
             C_SETCLASS        LIKE SETHEADER-SETCLASS VALUE '8001',
*The following constants are for IDOC output creation **
             C_IDOC_MESTYP     LIKE EDP13-MESTYP VALUE 'OIUDN1',
             C_IDOC_DOCTYP     LIKE EDP13-DOCTYP VALUE 'OIUDN01',
             C_IDOC_DIRECT     LIKE EDIDC-DIRECT VALUE '1'.

* *
*                       Internal Tables                                *
* *
DATA:  BEGIN OF DELIVERY_NETWORK_TB1 OCCURS 1.
         INCLUDE STRUCTURE ROIU_PR_DN.
DATA:  END   OF DELIVERY_NETWORK_TB1.

DATA:  BEGIN OF DELIVERY_NETWORK_TB2 OCCURS 1.
         INCLUDE STRUCTURE DELIVERY_NETWORK_TB1.
DATA:  END   OF DELIVERY_NETWORK_TB2.

DATA:  FCODE_TB  LIKE SY-UCOMM OCCURS 10 WITH HEADER LINE.
*   Reference Delivery network itab
DATA:  BEGIN OF REF_DELIVERY_NETWORK_DS.
DATA:    DN_DE          LIKE ROIU_PR_DN-DN_DE,
         DN_SET         LIKE ROIU_PR_DN-DN_SET,
         CNTRY_CD       LIKE ROIU_PR_DN-CNTRY_CD,
         PRI_GEO_LOC    LIKE ROIU_PR_DN-PRI_GEO_LOC,
         SEC_GEO_LOC    LIKE ROIU_PR_DN-SEC_GEO_LOC,
         TOL_PC         LIKE ROIU_PR_DN-TOL_PC,
         BUKRS          LIKE ROIU_PR_DN-BUKRS,
         VNAME          LIKE ROIU_PR_DN-VNAME,
         GPLT_DN_NO     LIKE ROIU_PR_DN-GPLT_DN_NO.
DATA:  END  OF REF_DELIVERY_NETWORK_DS.
*THE FOLLOWING INTERNAL TABLE IS FOR IDOC CREATION
DATA:  BEGIN OF IDOC_EDP13 OCCURS 1.
         INCLUDE STRUCTURE EDP13.
DATA:  END   OF IDOC_EDP13.

DATA:  BEGIN OF IDOC_CNTRL OCCURS 1.
         INCLUDE STRUCTURE EDIDC.
DATA:  END   OF IDOC_CNTRL.

DATA: BEGIN OF IDOC_DATA OCCURS 0.
        INCLUDE STRUCTURE EDIDD.
DATA: END OF IDOC_DATA.
DATA: W_FORMAT.
* *
*                      Data Structures                                 *
* *
DATA: W_RETURN_DS    LIKE BAPIRET2.
DATA: W_INTERVAL_DS  LIKE NRIV.
* *
*                      Include Programs                                *
* *
* Change document include
INCLUDE: FOIU_DNCDT,
         FOIU_DNCDC,
         LOIU_COMMONTP2,
         LOIU_COMMONF02.



* Changed by VV on 20020729 for Maintaining Production Number range

DATA:  wa_oiu_cm_edit TYPE oiu_cm_edit.
DATA:  g_tabname TYPE oiu_cm_sn_nr-tabname.


LOAD-OF-PROGRAM.
  SELECT SINGLE *
    FROM oiu_cm_edit
    INTO wa_oiu_cm_edit
    WHERE edit_no = '0007'.
  CHECK sy-subrc EQ 0.
* check to see if integration is on or not
  IF NOT wa_oiu_cm_edit-status IS INITIAL.
* integration is turned on
    g_tabname = 'OIU_PR_DN'.
  ELSE.
* integration is turned off
    g_tabname = 'OIU_PR_DN_10'.
  ENDIF.
