*&---------------------------------------------------------------------*
*& Report YGMS_CST_MASTER_DATA
*& Description: Combined screen for CST Master Data Maintenance
*&              (Location Mapping, Material Mapping, Exclusion Mapping)
*& Transaction: YRGX001
*&---------------------------------------------------------------------*
REPORT ygms_cst_master_data.

TABLES sscrfields.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
* Section 1: Location Mapping
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN COMMENT /1(55) c_txt1.
  SELECTION-SCREEN PUSHBUTTON /1(40) btn1 USER-COMMAND locmap.
SELECTION-SCREEN END OF BLOCK b1.

* Section 2: Material Mapping
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECTION-SCREEN COMMENT /1(55) c_txt2.
  SELECTION-SCREEN PUSHBUTTON /1(40) btn2 USER-COMMAND matmap.
SELECTION-SCREEN END OF BLOCK b2.

* Section 3: Exclusion Mapping
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  SELECTION-SCREEN COMMENT /1(55) c_txt3.
  SELECTION-SCREEN PUSHBUTTON /1(40) btn3 USER-COMMAND exclmap.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  btn1   = '@48@ Open Location Mapping'.
  btn2   = '@48@ Open Material Mapping'.
  btn3   = '@48@ Open Exclusion Mapping'.
  c_txt1 = 'Map ONGC CTP IDs to GAIL Location IDs'.
  c_txt2 = 'Map ONGC materials to GAIL materials'.
  c_txt3 = 'Maintain material exclusions for allocation'.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN 'LOCMAP'.
      SUBMIT ygms_cst_loc_map VIA SELECTION-SCREEN AND RETURN.
    WHEN 'MATMAP'.
      SUBMIT ygms_cst_mat_map VIA SELECTION-SCREEN AND RETURN.
    WHEN 'EXCLMAP'.
      SUBMIT ygms_cst_exclude VIA SELECTION-SCREEN AND RETURN.
  ENDCASE.
