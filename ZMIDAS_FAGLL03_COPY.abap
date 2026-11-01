*&---------------------------------------------------------------------*
*& Report  ZMIDAS_FAGLL03
*&
*&---------------------------------------------------------------------*
*& Title             : Midas 3 ( BU P&L Automation )
*& Program           : ZMIDAS_FAGLL03_COPY
*& Tcode             : ZMIDAS_FAGLL03_COPY
*& Author            : Gajanan Patil
*& Functional Design : Phanindra Bagana
*& Technical Design  : Gajanan Patil
*& Creation Date     : 15.02.2022
*& Transport Request : CRDK983087
*& Change request    : 8000007907
*& Description       : Call FAGLL03 Tcode to update custom table
*&---------------------------------------------------------------------*
* Modification Information
* Date            : 29.03.2024
* Author          : Gajanan Patil
* Functional      : Arijit Poddar
* Change request  : 8000003569
* Transport number: CRDK990287
* Description     : Midas3 Project ticket
*----------------------------------------------------------------------*
REPORT zmidas_fagll03_copy.

INCLUDE zmidas_fagll03_copy_dec.
INCLUDE zmidas_fagll03_copy_forms.

AT SELECTION-SCREEN.
  CLEAR:p_spmon.

  CONCATENATE p_date+0(4) p_date+4(2)  INTO p_spmon.


START-OF-SELECTION.
  PERFORM get_glset.
* **BEGIN OF CHANGE 01.11.2026 - CRS: job cancelled after 89 sec (memory)
* Issue   : the whole month of ZMIDAS_FAGLL03 (600k+ wide rows) was read
*           into TA_FAGLL03 and copied again into TA_FINAL -> several GB
*           -> TSV_TNEW_PAGE_ALLOC_FAILED in the S/4 system.
* Solution: rows derived from the G/L totals are saved first, then the
*           ZMIDAS_FAGLL03 rows are processed in packages of 20,000
*           (same GET_DATA / UPDATE_TAB logic per package).
*  PERFORM get_data.
*  PERFORM update_tab.
  PERFORM save_pc_rows.
  PERFORM process_packages.
* **END OF CHANGE 01.11.2026