*&---------------------------------------------------------------------*
*& Include YGMS_CST_PURCHASE_SEL
*& Description: Selection Screen Definition
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Selection Screen - Block 1: Processing Mode
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
  PARAMETERS:
    rb_upld  RADIOBUTTON GROUP mode DEFAULT 'X' USER-COMMAND mode,  " Upload
    rb_alloc RADIOBUTTON GROUP mode,                                 " Allocate
    rb_view  RADIOBUTTON GROUP mode,                                 " View
    rb_send  RADIOBUTTON GROUP mode,                                 " Send
    rb_dwnld RADIOBUTTON GROUP mode,                                 " Download
    rb_nom   RADIOBUTTON GROUP mode.                                 " Nomination
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Selection Screen - Block 2: Selection Criteria
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
  PARAMETERS:
    p_loc    TYPE ygms_loc_id OBLIGATORY,           " Location ID
    p_frdat  TYPE datum,                             " From Date
    p_todat  TYPE datum.                             " To Date
  SELECT-OPTIONS:
    s_mat    FOR ygms_cst_pur-material.              " Material
  PARAMETERS:
    p_file   TYPE string LOWER CASE MODIF ID upl.   " File Path (for upload)
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Selection Screen - Block 3: Exclusions
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-b03.
  SELECT-OPTIONS:
    s_exst   FOR ygms_cst_pur-state_code NO INTERVALS. " Excluded States
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* Selection Screen - Block 4: Email Options
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-b04.
  PARAMETERS:
    p_email  TYPE ad_smtpadr LOWER CASE MODIF ID snd.  " Email Address
SELECTION-SCREEN END OF BLOCK b4.

*----------------------------------------------------------------------*
* Selection Screen - Block 5: Download Options
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-b05.
  PARAMETERS:
    p_dwnfl  TYPE string LOWER CASE MODIF ID dwn.      " Download File Path
SELECTION-SCREEN END OF BLOCK b5.

*----------------------------------------------------------------------*
* Selection Screen Texts (to be defined in SE32/Text Elements)
*----------------------------------------------------------------------*
* TEXT-b01: Processing Mode
* TEXT-b02: Selection Criteria
* TEXT-b03: State Exclusions
* TEXT-b04: Email Options
* TEXT-b05: Download Options
