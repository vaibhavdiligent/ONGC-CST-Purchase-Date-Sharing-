*&=====================================================================*
*&  ENHANCEMENT SPOT : YDVRFI_EBS   (ENHANCEMENT 1)
*&  PROGRAM          : RFEKA400
*&  FORM             : UMSATZZEILE            (spot (3) - end of form)
*&  PURPOSE          : Build LI_REFERENZ and EXPORT it to
*&                     INDX(ST) ID 'EBS'. This is the reference that
*&                     ZXF01U01 later IMPORTs to replace ZUONR / FEBCL.
*&
*&  ISSUE (logic skipped for some line items):
*&    1. The dedup LOOP/DELETE removed every LI_REFERENZ entry that was
*&       not present in the CURRENT XFEBEP. XFEBEP is REFRESHed per
*&       statement (see SCHLUSSSALDO / INTRADAY_END), so in a file that
*&       contains more than one bank statement (several :62F:) the
*&       references of all but the LAST statement were wiped. Those
*&       line items then find nothing in ZXF01U01 -> logic skipped.
*&    2. The store guard tested STRLEN( REFERENZ ) GT 7, but for NCOL /
*&       ICICI940 the value actually stored is LW_REFERENZ-ZUONR
*&       (last 8 chars of the raw line), NOT REFERENZ. So NCOL items
*&       whose REFERENZ was short were dropped even though a valid
*&       ZUONR had been derived.
*&    3. For NCOL, L_STR_LEN = STRLEN( LW_SWIFT ) - 8 can go negative
*&       when the line is <= 8 chars (offset error).
*&=====================================================================*


*&---------------------------------------------------------------------*
*&  ORIGINAL CODE (as currently active in the system)
*&---------------------------------------------------------------------*
ENHANCEMENT 1  YDVRFI_EBS.    "active version
*&---------------------------------------------------------------------*
*& ENH-  YDVRFI_EBS
*&---------------------------------------------------------------------*
* Customer              : GAIL                                         *
* Program Name          : YDVRFI_EBS               *
* Development Class     : YF01                                         *
* Transport Request No  : DVRK9A09UN                                   *
* Initiated by          : Gaurav Sharma / Mr. Khichi                   *
* Developed by          : Bipin Shukla                                 *
* Version No            : V 1.0                                        *
* Program Description   : EBS Payment Enh                              *
*----------------------------------------------------------------------*
DATA :lw_swift TYPE char256.
DATA : BEGIN OF lw_referenz,
         kukey TYPE kukey_eb,
         esnum TYPE esnum_eb,
         zuonr TYPE dzuonr,
       END OF lw_referenz,
       li_referenz LIKE STANDARD TABLE OF lw_referenz,
       wa_referenz LIKE lw_referenz.

DATA : l_str_len TYPE I.
DATA : wa_zsd_param TYPE yfi_bk_payext.
TABLES : indx.

DATA : indexkey TYPE indx-srtfd VALUE 'EBS'.


DATA : l_pay_meth TYPE vgext_eb,
       l_fdpos    TYPE sy-fdpos,
       l_flag     TYPE c.

indx-aedat = sy-datum.
indx-srtfd = 'EBS'.
**BREAK XSAP_FI.

CLEAR: fdpos,
len_referenz,
offs.

CLEAR : wa_zsd_param, l_flag.
lw_swift = swift.

SELECT SINGLE FLAG INTO @DATA(LV_FLAG) FROM YFIBANK_EBS
  WHERE BANKN = @FEB_ABSND-BANKN.

IF LV_FLAG IS INITIAL.

"SOC   :  On 25.02.2019(Centralised Payment) By Man Mohan Acharya/ Gaurav Sharma / Sunny Babla
SEARCH swift-zeile FOR 'CR'.
if sy-subrc = 0.
  if sy-fdpos ge 0.
SPLIT swift at '//' INTO DATA(lv_str1) DATA(lv_str2).
if lv_str1 is NOT INITIAL.
  SPLIT lv_str1 at ',' INTO DATA(lv_str1_left) DATA(lv_str1_right).
  if lv_str1_right is NOT INITIAL.
  DATA(lv_length) = strlen( lv_str1_right ).
  if lv_length ge 8.
  DATA(lv_offset) = lv_length - 8.
  DATA(lv_temp_chect) = lv_str1_right+lv_offset(08).
  DATA(lv_overwrite) = 'X'.
  endif.
  endif.

ENDIF.
ENDIF.
endif.
"EOC   :  On 25.02.2019(Centralised Payment) By Man Mohan Acharya/ Gaurav Sharma / Sunny Babla


SHIFT lw_swift LEFT BY 6 PLACES.
IF zeile+0(1)  NE 'D'
AND zeile+0(1) NE 'C'
AND zeile+0(1) NE 'R'.
  IF zeile+0(4) IS NOT INITIAL.

    IF xfebep-valut+4(2) = '01' AND
    lw_swift+0(2)        = '12'.
    ELSEIF xfebep-valut+4(2) = '12' AND
      lw_swift+0(2)        = '01'.
    ENDIF.
  ENDIF.
  SHIFT lw_swift LEFT BY 4 PLACES.
ENDIF.


* Subfeld 3: Vorzeichen
IF lw_swift+0(1) = 'D'.
  SHIFT lw_swift LEFT BY 1 PLACES.
ELSEIF lw_swift+0(1) = 'C'.
  SHIFT lw_swift LEFT BY 1 PLACES.
ELSEIF lw_swift+0(2) = 'RC'.
  SHIFT lw_swift LEFT BY 2 PLACES.
ELSEIF lw_swift+0(2) = 'RD'.
  SHIFT lw_swift LEFT BY 2 PLACES.
ENDIF.

* Subfeld 4: Waehrungsart
IF lw_swift+0(1) CN '0123456789.,'.                          "mp31H
  SHIFT lw_swift LEFT.
ENDIF.

* Subfeld 5: Amount
IF lw_swift CA 'NSF'.                                        "30E
  fdpos = sy-fdpos.                                       "30F

  SHIFT lw_swift LEFT BY fdpos PLACES.
* This string will contain payment method transaction type such as NRR,etc
  l_pay_meth = lw_swift+0(4).   " gomathi001
  SHIFT lw_swift LEFT BY 4 PLACES.
ENDIF.
IF lw_swift = space.
  EXIT.
ENDIF.
IF lw_swift CS '//'.
  IF sy-fdpos GT 0.                                       "mp45A
* begin of changes gomathi001
* when transaction type is 'MT940_5'
    IF t028b-vgtyp = 'MT940_5' .


      SELECT SINGLE * FROM yfi_bk_payext
      INTO wa_zsd_param
      WHERE parnum  = 'EBS'
      AND segnam = 'VGEXT'
      AND segfld   = l_pay_meth.
      IF sy-subrc = 0.
        l_flag = 'X'.
      ENDIF.
    ENDIF.


    IF l_flag = 'X'.

      if lv_overwrite <> 'X'."+******SOC  : on 25.02.2019(Centralised Payment) By Man Mohan / Gaurav Sharma / Sunny Babla
      l_fdpos = sy-fdpos.
      l_fdpos = l_fdpos + 2.

      ASSIGN lw_swift+l_fdpos(16) TO <referenz>.
      MOVE <referenz> TO referenz.
      MOVE referenz TO xfebep-chect.
      else.
    "+******SOC  : on 25.02.2019(Centralised Payment) By Man Mohan / Gaurav Sharma / Sunny Babla

      MOVE lv_temp_chect to xfebep-chect.
      MOVE lv_temp_chect to referenz.
      endif.
    "+******EOC  : on 25.02.2019(Centralised Payment) By Man Mohan / Gaurav Sharma / Sunny Babla
    "+******SOC  : on 25.02.2019(Centralised Payment) By Man Mohan / Gaurav Sharma / Sunny Babla
    ELSEIF lv_overwrite = 'X'.
    MOVE lv_temp_chect to xfebep-chect.
    MOVE lv_temp_chect to referenz.
   "+******EOC  : on 25.02.2019(Centralised Payment) By Man Mohan / Gaurav Sharma / Sunny Babla
    ELSE.

* end of changes gomathi001
      ASSIGN lw_swift+0(sy-fdpos) TO <referenz>.
      MOVE <referenz> TO referenz.
    ENDIF.
  ELSE.
    CLEAR: referenz.
  ENDIF.                                                  "mp45A
ENDIF.
*XFEBEP
IF ( strlen( referenz ) GT 7 ).              "SOC : (Centralised Payment) Man Mohan on 25.02.2019 / Gaurav Sharma/ Sunny Babla
*delete from database indx(st) id indexkey.
  IMPORT li_referenz FROM DATABASE indx(st) ID indexkey.

  LOOP AT li_referenz INTO wa_referenz.
    READ TABLE xfebep ASSIGNING FIELD-SYMBOL(<fs_xfebep>) WITH KEY kukey = wa_referenz-kukey
    esnum = wa_referenz-esnum.
    IF sy-subrc NE 0.
      DELETE TABLE li_referenz FROM wa_referenz.
    ENDIF.
    CLEAR wa_referenz.
  ENDLOOP.
  lw_referenz-kukey = xfebep-kukey.
  lw_referenz-esnum = xfebep-esnum.
**SOC by Aakash Tyagi/Brijesh Choudhary on 24.01.2024 Charm :S 4000007661: ICICI MT940 BUG FIXING
*  lw_referenz-zuonr = referenz.
*** -> Begin of changes by of Harsh on 17Oct-24 for ATC
IF l_pay_meth = 'NCOL' AND febko-vgtyp = 'ICICI940'. "#EC CI_USAGE_OK[2689873]
*** <- End changes by of Harsh on 17Oct-24 for ATC
CLEAR : l_str_len.
  l_str_len = strlen( lw_swift ).
  l_str_len = l_str_len - 8.
  lw_referenz-zuonr = lw_swift+l_str_len(8).
ELSE.
  lw_referenz-zuonr = referenz.
ENDIF.
**EOC by Aakash Tyagi/Brijesh Choudhary on 24.01.2024 Charm :S 4000007661: ICICI MT940 BUG FIXING
  APPEND lw_referenz TO li_referenz.

  EXPORT li_referenz TO DATABASE indx(st) ID indexkey.
ENDIF.

CLEAR: lw_swift,lw_referenz,li_referenz[],lv_overwrite,lv_str1,lv_str2,lv_str1_right,lv_str1_left,lv_length,lv_offset.
ENDIF.

ENDENHANCEMENT.


*&---------------------------------------------------------------------*
*&  MODIFIED CODE (fix for skipped line items)
*&  Only the tail (from "*XFEBEP") is changed; everything above the
*&  "IF ( strlen( referenz ) GT 7 )." line is identical to the original.
*&---------------------------------------------------------------------*
ENHANCEMENT 1  YDVRFI_EBS.    "active version
*&---------------------------------------------------------------------*
*& ENH-  YDVRFI_EBS
*&---------------------------------------------------------------------*
* Customer              : GAIL                                         *
* Program Name          : YDVRFI_EBS               *
* Development Class     : YF01                                         *
* Transport Request No  : DVRK9A09UN                                   *
* Initiated by          : Gaurav Sharma / Mr. Khichi                   *
* Developed by          : Bipin Shukla                                 *
* Version No            : V 1.1  (skip fix by Vaibhav 02.07.2026)      *
* Program Description   : EBS Payment Enh                              *
* Change Remark         : Logic-skip fix - changed by Vaibhav on       *
*                         02.07.2026 (see marked block near end)       *
*----------------------------------------------------------------------*
DATA :lw_swift TYPE char256.
DATA : BEGIN OF lw_referenz,
         kukey TYPE kukey_eb,
         esnum TYPE esnum_eb,
         zuonr TYPE dzuonr,
       END OF lw_referenz,
       li_referenz LIKE STANDARD TABLE OF lw_referenz,
       wa_referenz LIKE lw_referenz.

DATA : l_str_len TYPE I.
DATA : wa_zsd_param TYPE yfi_bk_payext.
TABLES : indx.

DATA : indexkey TYPE indx-srtfd VALUE 'EBS'.


DATA : l_pay_meth TYPE vgext_eb,
       l_fdpos    TYPE sy-fdpos,
       l_flag     TYPE c.

indx-aedat = sy-datum.
indx-srtfd = 'EBS'.
**BREAK XSAP_FI.

CLEAR: fdpos,
len_referenz,
offs.

CLEAR : wa_zsd_param, l_flag.
lw_swift = swift.

SELECT SINGLE FLAG INTO @DATA(LV_FLAG) FROM YFIBANK_EBS
  WHERE BANKN = @FEB_ABSND-BANKN.

IF LV_FLAG IS INITIAL.

"SOC   :  On 25.02.2019(Centralised Payment) By Man Mohan Acharya/ Gaurav Sharma / Sunny Babla
SEARCH swift-zeile FOR 'CR'.
if sy-subrc = 0.
  if sy-fdpos ge 0.
SPLIT swift at '//' INTO DATA(lv_str1) DATA(lv_str2).
if lv_str1 is NOT INITIAL.
  SPLIT lv_str1 at ',' INTO DATA(lv_str1_left) DATA(lv_str1_right).
  if lv_str1_right is NOT INITIAL.
  DATA(lv_length) = strlen( lv_str1_right ).
  if lv_length ge 8.
  DATA(lv_offset) = lv_length - 8.
  DATA(lv_temp_chect) = lv_str1_right+lv_offset(08).
  DATA(lv_overwrite) = 'X'.
  endif.
  endif.

ENDIF.
ENDIF.
endif.
"EOC   :  On 25.02.2019(Centralised Payment) By Man Mohan Acharya/ Gaurav Sharma / Sunny Babla


SHIFT lw_swift LEFT BY 6 PLACES.
IF zeile+0(1)  NE 'D'
AND zeile+0(1) NE 'C'
AND zeile+0(1) NE 'R'.
  IF zeile+0(4) IS NOT INITIAL.

    IF xfebep-valut+4(2) = '01' AND
    lw_swift+0(2)        = '12'.
    ELSEIF xfebep-valut+4(2) = '12' AND
      lw_swift+0(2)        = '01'.
    ENDIF.
  ENDIF.
  SHIFT lw_swift LEFT BY 4 PLACES.
ENDIF.


* Subfeld 3: Vorzeichen
IF lw_swift+0(1) = 'D'.
  SHIFT lw_swift LEFT BY 1 PLACES.
ELSEIF lw_swift+0(1) = 'C'.
  SHIFT lw_swift LEFT BY 1 PLACES.
ELSEIF lw_swift+0(2) = 'RC'.
  SHIFT lw_swift LEFT BY 2 PLACES.
ELSEIF lw_swift+0(2) = 'RD'.
  SHIFT lw_swift LEFT BY 2 PLACES.
ENDIF.

* Subfeld 4: Waehrungsart
IF lw_swift+0(1) CN '0123456789.,'.                          "mp31H
  SHIFT lw_swift LEFT.
ENDIF.

* Subfeld 5: Amount
IF lw_swift CA 'NSF'.                                        "30E
  fdpos = sy-fdpos.                                       "30F

  SHIFT lw_swift LEFT BY fdpos PLACES.
* This string will contain payment method transaction type such as NRR,etc
  l_pay_meth = lw_swift+0(4).   " gomathi001
  SHIFT lw_swift LEFT BY 4 PLACES.
ENDIF.
IF lw_swift = space.
  EXIT.
ENDIF.
IF lw_swift CS '//'.
  IF sy-fdpos GT 0.                                       "mp45A
* begin of changes gomathi001
* when transaction type is 'MT940_5'
    IF t028b-vgtyp = 'MT940_5' .


      SELECT SINGLE * FROM yfi_bk_payext
      INTO wa_zsd_param
      WHERE parnum  = 'EBS'
      AND segnam = 'VGEXT'
      AND segfld   = l_pay_meth.
      IF sy-subrc = 0.
        l_flag = 'X'.
      ENDIF.
    ENDIF.


    IF l_flag = 'X'.

      if lv_overwrite <> 'X'.
      l_fdpos = sy-fdpos.
      l_fdpos = l_fdpos + 2.

      ASSIGN lw_swift+l_fdpos(16) TO <referenz>.
      MOVE <referenz> TO referenz.
      MOVE referenz TO xfebep-chect.
      else.
      MOVE lv_temp_chect to xfebep-chect.
      MOVE lv_temp_chect to referenz.
      endif.
    ELSEIF lv_overwrite = 'X'.
    MOVE lv_temp_chect to xfebep-chect.
    MOVE lv_temp_chect to referenz.
    ELSE.
* end of changes gomathi001
      ASSIGN lw_swift+0(sy-fdpos) TO <referenz>.
      MOVE <referenz> TO referenz.
    ENDIF.
  ELSE.
    CLEAR: referenz.
  ENDIF.                                                  "mp45A
ENDIF.

*XFEBEP
*** -> BEGIN CHANGE BY VAIBHAV ON 02.07.2026 (LOGIC-SKIP FIX) -----------
* REMARK (VAIBHAV 02.07.2026): Derive the assignment value (ZUONR) FIRST,
* then decide whether to store it. The old code tested
* STRLEN( REFERENZ ) GT 7 but, for NCOL, stored LW_REFERENZ-ZUONR -
* i.e. it checked the wrong field and dropped valid NCOL items.
CLEAR lw_referenz.
lw_referenz-kukey = xfebep-kukey.
lw_referenz-esnum = xfebep-esnum.

IF l_pay_meth = 'NCOL' AND febko-vgtyp = 'ICICI940'. "#EC CI_USAGE_OK[2689873]
  CLEAR l_str_len.
  l_str_len = strlen( lw_swift ).
  IF l_str_len GT 8.                 " guard: avoid negative offset
    l_str_len = l_str_len - 8.
    lw_referenz-zuonr = lw_swift+l_str_len(8).
  ELSE.
    lw_referenz-zuonr = lw_swift.
  ENDIF.
ELSE.
  lw_referenz-zuonr = referenz.
ENDIF.

* Store only when a usable reference was actually derived. The check
* now looks at the value that is stored (LW_REFERENZ-ZUONR).
IF strlen( lw_referenz-zuonr ) GT 7.

  IMPORT li_referenz FROM DATABASE indx(st) ID indexkey.

* FIX (multi-statement files): remove ONLY a pre-existing entry for the
* SAME statement line, so re-processing does not create duplicates.
* The previous LOOP/DELETE removed every entry that was not present in
* the CURRENT (per-statement, REFRESHed) XFEBEP and therefore wiped the
* references of all earlier statements in the same file - those items
* then found nothing in ZXF01U01 and the clearing logic was skipped.
  DELETE li_referenz WHERE kukey = xfebep-kukey
                       AND esnum = xfebep-esnum.

  APPEND lw_referenz TO li_referenz.

  EXPORT li_referenz TO DATABASE indx(st) ID indexkey.
ENDIF.
*** <- END CHANGE BY VAIBHAV ON 02.07.2026 (LOGIC-SKIP FIX) -------------

CLEAR: lw_swift,lw_referenz,li_referenz[],lv_overwrite,lv_str1,lv_str2,lv_str1_right,lv_str1_left,lv_length,lv_offset.
ENDIF.

ENDENHANCEMENT.
