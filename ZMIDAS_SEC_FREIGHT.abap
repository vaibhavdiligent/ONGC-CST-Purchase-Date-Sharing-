*&---------------------------------------------------------------------*
*& Report  ZMIDAS_SEC_FREIGHT
*&
*&---------------------------------------------------------------------*
*& Title             : Midas 3 ( BU P&L Automation )
*& Program           : ZMIDAS_SEC_FREIGHT
*& Tcode             : ZMIDAS_SEC_FREIGHT
*& Author            : Pooja Adupala
*& Functional Design : Shashank Sharma
*& Technical Design  : Pooja Adupala
*& Creation Date     : 13.04.2022
*& Transport Request : CRDK982563
*& Change request    : 8000007907
*& Description       : Program to Update Custom Table-ZMIDAS_SEC_FRGHT
*&---------------------------------------------------------------------*
REPORT zmidas_sec_freight.
INCLUDE zmidas_sec_freight_dec.
INCLUDE zmidas_sec_freight_forms.

***SOA by Gajanan - 04.03.2025 - CR#8000003569
AT SELECTION-SCREEN.
  CLEAR:p_spmon.

  CONCATENATE p_date+0(4) p_date+4(2)  INTO p_spmon.
***EOA by Gajanan - 04.03.2025 - CR#8000003569

START-OF-SELECTION.
  PERFORM refresh_tab.
  PERFORM get_data.
  IF ta_s911_consol IS NOT INITIAL.
    PERFORM get_final.
    IF ta_final IS NOT INITIAL.
      PERFORM update_table.
    ENDIF.
  ELSE.
* **BEGIN OF CHANGE 01.11.2026 - CRS: job cancelled after 0 sec
* Issue   : when ZSD_S911_CONSOL has no rows for the period the E message
*           cancels the background job ("Canceled" in SM37) although the
*           program itself is fine. The data comes from the S911
*           consolidation run, which must have run for the period first.
* Solution: information message (job finishes, text in the job log).
*    MESSAGE text-001 TYPE co_e DISPLAY LIKE co_s.
    MESSAGE text-001 TYPE co_s DISPLAY LIKE co_e.
    MESSAGE |ZSD_S911_CONSOL has no rows for period { p_spmon } - run the S911 consolidation for this period first| TYPE co_s.
* **END OF CHANGE 01.11.2026
  ENDIF.