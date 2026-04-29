*&---------------------------------------------------------------------*
*& Include YRGG015_PURC_NOM_ONGC_B2B_F02
*& OA Derivation (5-step) and Batch Derivation from MCHA
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& FORM derive_outline_agreement
*& Derives OA for a given location+material via:
*&   OIJ_EL_DOC_MOT → T001W (WERKS=2*) → EKPO (LVORM≠X, MWSKZ=DQ) → latest
*&---------------------------------------------------------------------*
FORM derive_outline_agreement
  USING    iv_locid  TYPE char10
           iv_matnr  TYPE matnr
  CHANGING cv_vbeln  TYPE ebeln
           cv_missing TYPE char1.

  DATA: lv_werks   TYPE werks_d,
        lt_werks   TYPE STANDARD TABLE OF werks_d,
        lv_vbeln   TYPE ebeln,
        lv_bedat   TYPE bedat,
        lv_best_vbeln TYPE ebeln,
        lv_best_bedat TYPE bedat.

  CLEAR: cv_vbeln, cv_missing.

  " Step 1: Get transport system / installation for this location
  DATA: lt_mot TYPE STANDARD TABLE OF oij_el_doc_mot,
        ls_mot TYPE oij_el_doc_mot.

  SELECT * FROM oij_el_doc_mot
    INTO TABLE @lt_mot
    WHERE locid = @iv_locid.

  IF lt_mot IS INITIAL.
    cv_missing = abap_true.
    RETURN.
  ENDIF.

  " Step 2: For each MOT entry, find WERKS starting with '2'
  LOOP AT lt_mot INTO ls_mot.
    SELECT werks
      FROM t001w
      INTO TABLE @DATA(lt_t001w_all)
      WHERE werks LIKE '2%'
        AND kunnr = @ls_mot-bpnam.  " Business partner / customer
    IF sy-subrc = 0.
      LOOP AT lt_t001w_all INTO DATA(ls_t001w).
        APPEND ls_t001w-werks TO lt_werks.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  IF lt_werks IS INITIAL.
    " Fallback: try direct lookup by location
    SELECT werks
      FROM t001w
      INTO TABLE @lt_werks
      WHERE werks LIKE '2%'
        AND regio = @iv_locid(2).
    IF sy-subrc <> 0.
      cv_missing = abap_true.
      RETURN.
    ENDIF.
  ENDIF.

  " Step 3+4+5: Find latest EKPO with matching WERKS, MWSKZ=DQ, LVORM<>X
  "             for the given material, from outline agreements (EKKO-BSTYP='K')
  SORT lt_werks.
  DELETE ADJACENT DUPLICATES FROM lt_werks.

  LOOP AT lt_werks INTO lv_werks.
    SELECT ekko~ebeln, ekko~bedat
      FROM ekko
      INNER JOIN ekpo ON ekpo~ebeln = ekko~ebeln
      INTO TABLE @DATA(lt_ekpo_res)
      WHERE ekko~bstyp  = 'K'          " Outline Agreement
        AND ekpo~werks  = @lv_werks
        AND ekpo~matnr  = @iv_matnr
        AND ekpo~mwskz  = 'DQ'
        AND ekpo~loekz  = ' '          " Not deleted
        AND ekko~loekz  = ' '.

    LOOP AT lt_ekpo_res INTO DATA(ls_res).
      IF ls_res-bedat > lv_best_bedat.
        lv_best_bedat = ls_res-bedat.
        lv_best_vbeln = ls_res-ebeln.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF lv_best_vbeln IS NOT INITIAL.
    cv_vbeln   = lv_best_vbeln.
    cv_missing = ' '.
  ELSE.
    cv_missing = abap_true.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM derive_batch
*& Gets the latest valid batch for a material from MCHA
*& Criteria: LVORM = blank, sorted by ERSDA desc → take first
*&---------------------------------------------------------------------*
FORM derive_batch
  USING    iv_matnr  TYPE matnr
  CHANGING cv_charg  TYPE charg_d.

  DATA: lt_mcha TYPE STANDARD TABLE OF mcha,
        ls_mcha TYPE mcha.

  CLEAR cv_charg.

  SELECT matnr, werks, charg, ersda, lvorm
    FROM mcha
    INTO TABLE @lt_mcha
    WHERE matnr = @iv_matnr
      AND lvorm = ' '.              " Not flagged for deletion

  IF sy-subrc <> 0 OR lt_mcha IS INITIAL.
    RETURN.
  ENDIF.

  " Sort by creation date descending → take the newest batch
  SORT lt_mcha BY ersda DESCENDING.
  READ TABLE lt_mcha INDEX 1 INTO ls_mcha.
  IF sy-subrc = 0.
    cv_charg = ls_mcha-charg.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM get_valid_batches_for_material
*& Returns all valid batches for a material (for F4 dropdown)
*&---------------------------------------------------------------------*
FORM get_valid_batches_for_material
  USING    iv_matnr  TYPE matnr
  CHANGING ct_batch  TYPE STANDARD TABLE.

  DATA: lt_mcha TYPE STANDARD TABLE OF mcha,
        ls_mcha TYPE mcha,
        ls_val  TYPE ty_batch_vals.

  REFRESH ct_batch.

  SELECT matnr, werks, charg, ersda, lvorm
    FROM mcha
    INTO TABLE @lt_mcha
    WHERE matnr = @iv_matnr
      AND lvorm = ' '.

  SORT lt_mcha BY ersda DESCENDING.

  LOOP AT lt_mcha INTO ls_mcha.
    CLEAR ls_val.
    ls_val-charg = ls_mcha-charg.
    ls_val-matnr = ls_mcha-matnr.
    ls_val-werks = ls_mcha-werks.
    ls_val-ersda = ls_mcha-ersda.
    APPEND ls_val TO ct_batch.
  ENDLOOP.
ENDFORM.
