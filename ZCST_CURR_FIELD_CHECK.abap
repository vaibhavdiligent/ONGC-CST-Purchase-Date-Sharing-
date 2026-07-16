*&---------------------------------------------------------------------*
*& Report  ZCST_CURR_FIELD_CHECK
*&---------------------------------------------------------------------*
*& Validate a DECFLOAT34 input and move it safely into a currency
*& field (data element KSTBW - CURR, 2 decimals).
*&
*& Example : input DECFLOAT34 = 1.9  ->  output KSTBW = 1.90  (1.9)
*&---------------------------------------------------------------------*
REPORT zcst_curr_field_check.

*----------------------------------------------------------------------*
* Selection screen ( online input )
*----------------------------------------------------------------------*
PARAMETERS: p_in   TYPE decfloat34,          " raw amount entered online
            p_waers TYPE waers DEFAULT 'INR'. " currency key

*----------------------------------------------------------------------*
* Form : validate DECFLOAT34 -> currency amount ( KSTBW )
*   iv_value  = input decimal float
*   iv_waers  = currency key ( decimal places come from TCURX )
*   ev_amount = output currency field ( KSTBW )
*   ev_valid  = 'X' when input is a valid, non-overflowing amount
*----------------------------------------------------------------------*
FORM check_curr_field
  USING    iv_value  TYPE decfloat34
           iv_waers  TYPE waers
  CHANGING ev_amount TYPE kstbw
           ev_valid  TYPE abap_bool
           ev_msg    TYPE string.

  DATA: lv_decimals TYPE i VALUE 2,          " default currency decimals
        lv_rounded  TYPE decfloat34,
        lv_max      TYPE decfloat34.

  CLEAR: ev_amount, ev_valid, ev_msg.

* 1) how many decimals does this currency really have? ( JPY = 0 etc. )
  SELECT SINGLE currdec FROM tcurx
    INTO lv_decimals
    WHERE currkey = iv_waers.
  IF sy-subrc <> 0.
    lv_decimals = 2.                          " ISO default
  ENDIF.

* 2) reject negative / zero if you only allow positive amounts
*    ( remove this block if negatives are allowed )
  IF iv_value < 0.
    ev_msg = 'Amount must not be negative'.
    RETURN.
  ENDIF.

* 3) round HALF-UP to the currency's decimal places.
*    DECFLOAT34 keeps 1.9 exact, so 1.9 -> 1.90 -> 1.9 ( no float noise )
  lv_rounded = round( val = iv_value dec = lv_decimals mode = cl_abap_math=>round_half_up ).

* 4) overflow check against the CURR field capacity ( KSTBW = CURR 15,2 )
*    15 total digits, 2 decimals -> 13 integer digits
*    max value that fits = 9999999999999.99
  lv_max = '9999999999999.99'.
  IF abs( lv_rounded ) > lv_max.
    ev_msg = 'Amount exceeds currency field capacity'.
    RETURN.
  ENDIF.

* 5) safe assignment to the currency field
  ev_amount = lv_rounded.
  ev_valid  = abap_true.

ENDFORM.

*----------------------------------------------------------------------*
* Main
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: lv_amount TYPE kstbw,
        lv_valid  TYPE abap_bool,
        lv_msg    TYPE string.

  PERFORM check_curr_field
    USING    p_in
             p_waers
    CHANGING lv_amount
             lv_valid
             lv_msg.

  IF lv_valid = abap_true.
    WRITE: / 'Input :', p_in.
    WRITE: / 'Output:', lv_amount CURRENCY p_waers.   " 1.9 -> 1.90
  ELSE.
    WRITE: / 'Invalid input:', lv_msg.
  ENDIF.
