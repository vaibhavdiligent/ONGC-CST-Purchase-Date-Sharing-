*&---------------------------------------------------------------------*
*& Report YGMS_CST_UPLOAD_ALLOC
*& Description: Combined screen for ONGC Receipt Data Upload,
*&              State-wise Allocation, Invoice Details and Nomination Data Creation
*& Transaction: YRGG015
*&---------------------------------------------------------------------*
REPORT ygms_cst_upload_alloc.

TABLES sscrfields.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
* Section 1: ONGC Receipt Data
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN COMMENT /1(55) c_txt1.
  SELECTION-SCREEN PUSHBUTTON /1(40) btn1 USER-COMMAND upload.
SELECTION-SCREEN END OF BLOCK b1.

* Section 2: Allocate Quantities to State
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECTION-SCREEN COMMENT /1(55) c_txt2.
  SELECTION-SCREEN PUSHBUTTON /1(40) btn2 USER-COMMAND alloc.
SELECTION-SCREEN END OF BLOCK b2.

* Section 3: ONGC Invoice Details
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  SELECTION-SCREEN COMMENT /1(55) c_txt3.
  SELECTION-SCREEN PUSHBUTTON /1(40) btn3 USER-COMMAND invdt.
SELECTION-SCREEN END OF BLOCK b3.

* Section 4: Create Nomination Data
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  SELECTION-SCREEN COMMENT /1(55) c_txt4.
  SELECTION-SCREEN PUSHBUTTON /1(40) btn4 USER-COMMAND nomin.
SELECTION-SCREEN END OF BLOCK b4.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  btn1   = '@48@ Upload/View Receipt Data'.
  btn2   = '@48@ Run Allocation'.
  btn3   = '@48@ ONGC Invoice Details'.
  btn4   = '@48@ Create Nomination Data'.
  c_txt1 = 'Click here to upload/view receipt data'.
  c_txt2 = 'Allocate receipt quantities to states'.
  c_txt3 = 'View and verify ONGC invoice receipt data'.
  c_txt4 = 'Create nomination data for ONGC B2B'.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'UPLOAD'.
      SUBMIT ygms_ongc_cst_pur VIA SELECTION-SCREEN AND RETURN.
    WHEN 'ALLOC'.
      SUBMIT ygms_cst_purchase_main VIA SELECTION-SCREEN AND RETURN.
    WHEN 'INVDT'.
      SUBMIT yrgr_cst_inv_verify VIA SELECTION-SCREEN AND RETURN.
    WHEN 'NOMIN'.
      SUBMIT yrgg015_purc_nom_ongc_b2b VIA SELECTION-SCREEN AND RETURN.
  ENDCASE.
