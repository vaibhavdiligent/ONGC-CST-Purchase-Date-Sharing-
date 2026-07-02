*&=====================================================================*
*&  ENHANCEMENT SPOT : YDVRFI_EBS   (ENHANCEMENT 3)
*&  PROGRAM          : RFEKA400
*&  FORM             : MEHRZWECKFELD            (spot (4))
*&  PURPOSE          : For NSTO / SWEEP lines, look back to the matching
*&                     :61: line and, from the current :86: SWEEP line,
*&                     capture the bank number into XFEBEP-ZUONR.
*&                     Also logs the :86: string into YRFA_MT940_DOC.
*&=====================================================================*


*&---------------------------------------------------------------------*
*&  ORIGINAL CODE (as currently active in the system)
*&---------------------------------------------------------------------*
ENHANCEMENT 3  YDVRFI_EBS.    "active version

***********************************************************
* Developed By      :   Man Mohan Acharya
* Suggested By      :   Pankaj Gupta
* Date              :  26.04.2019 18:03:31
* Purpose           :   To capture BANK NO from SWEEP Lines
*************************************************************

DATA : ls_swift_temp  TYPE char512.
DATA : lv_sweep_pos   TYPE string.
DATA : lv_nsto_pos    TYPE string.


DATA(temp_tabix) = sy-tabix - 1.
do.
TRY .
DATA(ls_swift_prev) = swift[ temp_tabix ].

if ls_swift_prev+0(3) = ':61'.
SEARCH ls_swift_prev FOR 'NSTO'.
 if sy-subrc = 0.
  lv_nsto_pos = sy-fdpos.
  SHIFT ls_swift_prev LEFT by lv_nsto_pos PLACES.
   CONDENSE ls_swift_prev.
   if ls_swift_prev+0(4) = 'NSTO'.
    data(lv_read) = 'X'.
    exit.
   endif.
  endif.
 exit.
else.
temp_tabix = temp_tabix - 1.
endif.
CATCH cx_sy_itab_line_not_found.
exit.
ENDTRY.
ENDDO.


if lv_read eq 'X'.
if  XFEBEP-texts = 'NSTO'.
ls_swift_temp = swift-zeile.
if ls_swift_temp+0(3) = ':86'.
     SEARCH ls_swift_temp FOR 'SWEEP'.
     if sy-subrc = 0.
     lv_sweep_pos = sy-fdpos.
     CONDENSE lv_sweep_pos.
     SHIFT ls_swift_temp LEFT BY lv_sweep_pos PLACES.

     SPLIT ls_swift_temp at ' ' INTO DATA(lv_sweep_left) DATA(lv_bank_no_right).
     if lv_bank_no_right is NOT INITIAL.
     CONDENSE lv_bank_no_right.
     xfebep-zuonr = lv_bank_no_right.
     endif.
     endif.
endif.
endif.
endif.

****End Of Development on 26.04.2019 18:03:31********************


*SOC  : On 06.05.2019 11:09:25 By Man Mohan Acharya / Pankaj Gupta / Sunny Babla

*BREAK xsap_fi.


DATA :ls_mt940 TYPE YRFA_MT940_DOC.
DATA : lv_date(10).
DATA : lv_day(02).
DATA : lv_mon(02).
DATA : lv_year(04).


TRY .

DATA(lv_86_zeile) = swift-zeile.
if lv_86_zeile+0(3) = ':86'.
SHIFT lv_86_zeile LEFT by 3 PLACES.
if lv_86_zeile+0(1) = ':'.
SHIFT lv_86_zeile LEFT BY 1 PLACES.
FIND FIRST OCCURRENCE OF '-' in lv_86_zeile RESULTS DATA(result_tab).
if sy-subrc = 0 and result_tab-offset is NOT INITIAL.

ls_mt940-bukrs       = '1000'.
ls_mt940-DOC_NO      = lv_86_zeile+0(result_tab-OFFSET).
ls_mt940-UPDATE_DATE = sy-datum.
ls_mt940-UPDATE_time = sy-uzeit.
ls_mt940-STRING_86   = swift-zeile.
ls_mt940-LINE_NO     = sy-tabix.

SHIFT lv_86_zeile LEFT by ( result_tab-OFFSET + 1 ) PLACES.
CLEAR result_tab.refresh : result_tab-SUBMATCHES.

"For Time in string
FIND FIRST OCCURRENCE OF '-' in lv_86_zeile RESULTS result_tab.
if result_tab-offset is NOT INITIAL.
lv_date = lv_86_zeile+0(result_tab-OFFSET).

SPLIT lv_date AT space INTO lv_day lv_mon lv_year.
DATA(data_len) = strlen( lv_day ).
if data_len lt 2.
  lv_day       = |{ lv_day ALPHA = IN }|.
endif.

CLEAR data_len.
data_len       = strlen( lv_mon ).
if data_len lt 2.
  lv_mon       = |{ lv_mon ALPHA = IN }|.
endif.


CLEAR lv_date.
lv_date = lv_year && lv_mon && lv_day.
CONDENSE lv_date.

if lv_date is NOT INITIAL and ls_mt940-DOC_NO IS NOT INITIAL.
ls_mt940-bldat       = lv_date.


ls_mt940-USER_NAME     = sy-uname.
ls_mt940-FILENAME      = AUSZUGFILE.


MODIFY YRFA_MT940_DOC FROM ls_mt940.

endif.
endif.
endif.
endif.
endif.

CATCH cx_root INTO data(lr_data).

  DATA(lv_msg) = lr_data->if_message~get_text( ).
  ls_mt940-ERROR_UPDATE = lv_msg.
  MODIFY YRFA_MT940_DOC FROM ls_mt940.

ENDTRY.

*EOC  : On 06.05.2019 11:09:39 By Man Mohan Acharya / Pankaj Gupta / Sunny Babla

ENDENHANCEMENT.


*&---------------------------------------------------------------------*
*&  MODIFIED CODE
*&---------------------------------------------------------------------*
*&  NO CODE CHANGE APPLIED (behaviour intentionally preserved).
*&
*&  This spot only populates XFEBEP-ZUONR for NSTO/SWEEP items and logs
*&  the :86: line. It is guarded by design (previous :61: must be NSTO
*&  and current :86: must contain 'SWEEP'), so a non-matching item is
*&  meant to be skipped - that is not the reported defect.
*&
*&  KNOWN RISK to watch (left unchanged to avoid altering behaviour):
*&    * The backward scan starts at DATA(temp_tabix) = sy-tabix - 1.
*&      SY-TABIX here is whatever the last table operation left behind,
*&      not necessarily the current SWIFT loop index. If SY-TABIX is
*&      unreliable at this point, the look-back for the matching :61:
*&      NSTO line can miss and ZUONR is not set. If SWEEP items are the
*&      ones being skipped, capture the real loop index in ENHANCEMENT 5
*&      / the main LOOP and pass it in instead of relying on SY-TABIX.
*&
*&  The primary skip defect is fixed in
*&  YDVRFI_EBS_ENH1_UMSATZZEILE_REFERENCE.abap.
*&---------------------------------------------------------------------*
