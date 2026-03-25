*&---------------------------------------------------------------------*
*& Report YGMS_CST_UPLOAD_ALLOC
*& Description: Combined screen for ONGC Receipt Data Upload and
*&              State-wise Allocation
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

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  btn1   = '@48@ Open Upload Program'.
  btn2   = '@48@ Open Allocation Program'.
  c_txt1 = 'Upload ONGC CST Purchase Data from Excel'.
  c_txt2 = 'Allocate receipt quantities to states'.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'UPLOAD'.
      SUBMIT ygms_ongc_cst_pur VIA SELECTION-SCREEN AND RETURN.
    WHEN 'ALLOC'.
      SUBMIT ygms_cst_purchase_main VIA SELECTION-SCREEN AND RETURN.
  ENDCASE.
