*&=====================================================================*
*&  ENHANCEMENT SPOT : YDVRFI_EBS   (ENHANCEMENT 5)
*&  PROGRAM          : RFEKA400
*&  FORM             : UMSATZZEILE            (spot (2) - start of form)
*&  PURPOSE          : Replace external transaction type 'TRFF' by
*&                     'NTRF' in the SWIFT ZEILE so the item can post.
*&=====================================================================*


*&---------------------------------------------------------------------*
*&  ORIGINAL CODE (as currently active in the system)
*&---------------------------------------------------------------------*
ENHANCEMENT 5  YDVRFI_EBS.    "active version

*******************************************************
* Developed By      :   Man Mohan Acharya
* Suggested By      :   Suresh Nayak / Pankaj Gupta / Sunny Babla
* Date              :   02.05.2019 15:23:37
* Purpose           :   To Replace TRFF by NTRF in SWIFT ZEILE -for accepting posting
******************************************************

TRY .
if zeile is NOT INITIAL.
*  DATA(lv_str_len) = strlen( zeile ).
  SEARCH zeile FOR 'TRFF' .
  if sy-subrc = 0.
    data(lv_pos_trff) = sy-fdpos.
    data(prev_pos) = sy-fdpos - 1.

    if zeile+prev_pos(1) CA '0123456789.'.

*    Before TRFF one More Character Value Found

    zeile+lv_pos_trff(4) = 'NTRF'.
    CLEAR lv_pos_trff.

    else.

    zeile+prev_pos(4) = 'NTRF'.
    CLEAR prev_pos.

    endif.

  endif.
ENDIF.
CATCH cx_root.
ENDTRY.

ENDENHANCEMENT.


*&---------------------------------------------------------------------*
*&  MODIFIED CODE
*&---------------------------------------------------------------------*
*&  NO CHANGE REQUIRED.
*&
*&  This spot only rewrites 'TRFF' -> 'NTRF' in ZEILE. It has no per-item
*&  gate that suppresses downstream processing (the whole block is
*&  wrapped in TRY/CATCH and simply does nothing when 'TRFF' is absent),
*&  so it is not a source of the "logic skipped" behaviour.
*&
*&  Note (context only): when 'TRFF' is at offset 0, PREV_POS = -1 and
*&  the CA test on ZEILE+PREV_POS(1) is invalid; the CX_ROOT catch
*&  swallows it and the line is left unchanged. This is a corner case,
*&  not the reported defect - left as-is to preserve current behaviour.
*&
*&  The primary skip defect is fixed in
*&  YDVRFI_EBS_ENH1_UMSATZZEILE_REFERENCE.abap.
*&---------------------------------------------------------------------*
