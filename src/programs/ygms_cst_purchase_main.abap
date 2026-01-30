*&---------------------------------------------------------------------*
*& Report YGMS_CST_PURCHASE_MAIN
*&---------------------------------------------------------------------*
*& Description: ONGC CST Purchase Data Sharing - Main Program
*& Author:      [To be filled]
*& Date:        [Creation date]
*& Module:      FI/SD
*& WRICEF ID:   R-GAIL-CST-001
*& Package:     YGMS
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*
REPORT ygms_cst_purchase_main MESSAGE-ID ygms_msg.

*----------------------------------------------------------------------*
* Includes
*----------------------------------------------------------------------*
INCLUDE ygms_cst_purchase_top.    " Global data declarations
INCLUDE ygms_cst_purchase_sel.    " Selection screen
INCLUDE ygms_cst_purchase_f01.    " Forms and subroutines
INCLUDE ygms_cst_purchase_evt.    " Event handling

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM initialization.

*----------------------------------------------------------------------*
* At Selection Screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_loc.
  PERFORM f4_location.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modify_screen.

AT SELECTION-SCREEN.
  PERFORM validate_selection.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM process_main.

*----------------------------------------------------------------------*
* End of Selection
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM display_results.
