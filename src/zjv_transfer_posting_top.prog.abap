*&---------------------------------------------------------------------*
*&  Include           ZJV_TRANSFER_POSTING_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Top Include
*----------------------------------------------------------------------*

TABLES: t001.

TYPE-POOLS: slis.

DATA: gv_lines TYPE i.

* JVSO1 Final Data
* S/4HANA: data read from CDS view JV_JVSO1_ACDOCA (CJVSO1_ACD)
* as per SAP Note 3082514 - ledger 4C amounts in HSL_4C / KSL_4C
TYPES: BEGIN OF ty_jvso1,
         rjvnam TYPE char10,
         rrecin TYPE char5,
         racct  TYPE saknr,
         rcntr  TYPE kostl,
         rordnr TYPE aufnr,
         rprojk TYPE ps_posid,
         rprctr TYPE prctr,
         tsl    TYPE p LENGTH 15 DECIMALS 2,
         hsl    TYPE p LENGTH 15 DECIMALS 2,
         ksl    TYPE p LENGTH 15 DECIMALS 2,
         hsl_4c TYPE p LENGTH 15 DECIMALS 2,
         ksl_4c TYPE p LENGTH 15 DECIMALS 2,
         budat  TYPE budat,
       END OF ty_jvso1.

DATA: gt_jvso1 TYPE STANDARD TABLE OF ty_jvso1,
      gs_jvso1 TYPE ty_jvso1.

* ALV Output
DATA: gt_fieldcat TYPE slis_t_fieldcat_alv,
      gs_layout   TYPE slis_layout_alv.

* BAPI Structures
DATA: gt_accountgl TYPE TABLE OF bapiacgl09,
      gt_currency  TYPE TABLE OF bapiaccr09,
      gt_return    TYPE TABLE OF bapiret2.

DATA: gs_accountgl TYPE bapiacgl09,
      gs_currency  TYPE bapiaccr09.

DATA: gv_postdate TYPE budat,
      gv_docdate  TYPE budat.
