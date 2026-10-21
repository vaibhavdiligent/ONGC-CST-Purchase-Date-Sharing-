*&---------------------------------------------------------------------*
*& Report  ZFI_JV_ITD_TRIAL_BAL
*&---------------------------------------------------------------------*
*& Transaction   : ZJVITD (suggested)
*& Program Title : Venture / Equity Group / RI / Account report
*&                 based on ITD (inception-to-date) balances
*&
*& Source spec   : "ZJVTB Logic.docx"
*&
*& Data sources
*&   JVTO1  (JV ledger totals)  RLDNR '4A', RRCTY '0', RVERS '001',
*&          RYEAR 2026 only (GC_BASE_YEAR). Holds the balances migrated
*&          from ECC: carryforward xSLVT and periods xSL01..xSL16.
*&   ACDOCA (Universal Journal) RLDNR '0L', RRCTY '0'.
*&          For 2026 only lines posted after the cut-over
*&          (ACDOCA-TIMESTAMP > cut-over), for later years all lines.
*&          Period 000 (balance carryforward) is never read; the
*&          opening balance is built cumulatively from the sources.
*&
*& Opening balance (OB) for selected year Y
*&   Y = 2026 : JVTO1 2026 xSLVT
*&   Y > 2026 : JVTO1 2026 xSLVT + xSL01..xSL16
*&            + ACDOCA 2026 periods 1-16 with TIMESTAMP > cut-over
*&            + ACDOCA 2027..Y-1 periods 1-16 (all lines)
*& Debit / Credit for periods 1 .. P (P = "Period to")
*&   Y = 2026 : JVTO1 2026 xSL01..xSLP, split by DRCRK (S/H)
*&            + ACDOCA 2026 periods 1..P with TIMESTAMP > cut-over
*&   Y > 2026 : ACDOCA Y periods 1..P (all lines), split by DRCRK
*& Closing balance (CB) = OB + Debit + Credit (credit is negative)
*&
*& Field mapping   JVTO1    ACDOCA
*&   Venture       RJVNAM   VNAME
*&   Equity group  REGROU   EGRUP
*&   Recovery ind. RRECIN   RECID
*&   G/L account   RACCT    RACCT
*&   D/C indicator DRCRK    DRCRK
*&   CoCd currency HSLxx    HSL
*&   Group curr.   KSLxx    KSL
*&
*& Layouts
*&   Venture columns : one row per G/L account; for every venture the
*&                     columns Opening / Debit / Credit / Closing (per
*&                     selected currency), plus a total over all
*&                     ventures. This is the layout from the spec.
*&   Detail          : one row per Venture / Equity group / RI /
*&                     G/L account with Opening / Debit / Credit /
*&                     Closing and the split JVTO1 vs ACDOCA.
*&
*& Selection texts (SE38 -> Text elements -> Selection texts)
*&   P_BUKRS  Company Code              P_GJAHR  Fiscal Year
*&   P_PERFR  Period from               P_PERTO  Period to
*&   S_VNAME  Venture                   S_RECID  Recovery Indicator
*&   P_HSL    Company code currency     P_KSL    Group currency
*&   P_BOTH   Both currencies           P_MATRIX Venture columns
*&   P_DETAIL Detail Venture/EG/RI/Acct P_NOZERO Hide zero lines
*&   P_BLANK  Incl. lines w/o venture
*&   P_CUTDT  Cut-over date             P_CUTTM  Cut-over time
*&   P_TZONE  Cut-over time zone
*& Text symbols
*&   B01 Selection   B02 Currency   B03 Output   B04 Cut-over (2026)
*&---------------------------------------------------------------------*
REPORT zfi_jv_itd_trial_bal.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: gc_base_year TYPE gjahr        VALUE '2026',
           gc_ldgr_jv   TYPE rldnr        VALUE '4A',
           gc_ldgr_gl   TYPE rldnr        VALUE '0L',
           gc_rrcty     TYPE acdoca-rrcty VALUE '0',
           gc_rvers     TYPE c LENGTH 3   VALUE '001',
           gc_credit    TYPE shkzg        VALUE 'H'.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES ty_amt TYPE p LENGTH 16 DECIMALS 2.

" Accumulated values per Venture / Equity group / RI / Account
TYPES: BEGIN OF ty_bal,
         vname    TYPE acdoca-vname,
         egrup    TYPE acdoca-egrup,
         recid    TYPE acdoca-recid,
         racct    TYPE acdoca-racct,
         txt50    TYPE skat-txt50,
         " company code currency
         ob_h     TYPE ty_amt,
         dr_h     TYPE ty_amt,
         cr_h     TYPE ty_amt,
         cb_h     TYPE ty_amt,
         " group currency
         ob_k     TYPE ty_amt,
         dr_k     TYPE ty_amt,
         cr_k     TYPE ty_amt,
         cb_k     TYPE ty_amt,
         " split by source (reconciliation)
         ob_h_jv  TYPE ty_amt,     " opening from JVTO1
         ob_h_acd TYPE ty_amt,     " opening from ACDOCA
         pr_h_jv  TYPE ty_amt,     " debit+credit from JVTO1
         pr_h_acd TYPE ty_amt,     " debit+credit from ACDOCA
         ob_k_jv  TYPE ty_amt,
         ob_k_acd TYPE ty_amt,
         pr_k_jv  TYPE ty_amt,
         pr_k_acd TYPE ty_amt,
       END OF ty_bal,
       ty_t_bal TYPE HASHED TABLE OF ty_bal
                WITH UNIQUE KEY vname egrup recid racct.

" JVTO1 aggregated per key and D/C indicator
TYPES: BEGIN OF ty_jvto1,
         rjvnam TYPE acdoca-vname,
         regrou TYPE acdoca-egrup,
         rrecin TYPE acdoca-recid,
         racct  TYPE acdoca-racct,
         drcrk  TYPE shkzg,
         hslvt  TYPE ty_amt, hsl01 TYPE ty_amt, hsl02 TYPE ty_amt,
         hsl03  TYPE ty_amt, hsl04 TYPE ty_amt, hsl05 TYPE ty_amt,
         hsl06  TYPE ty_amt, hsl07 TYPE ty_amt, hsl08 TYPE ty_amt,
         hsl09  TYPE ty_amt, hsl10 TYPE ty_amt, hsl11 TYPE ty_amt,
         hsl12  TYPE ty_amt, hsl13 TYPE ty_amt, hsl14 TYPE ty_amt,
         hsl15  TYPE ty_amt, hsl16 TYPE ty_amt,
         kslvt  TYPE ty_amt, ksl01 TYPE ty_amt, ksl02 TYPE ty_amt,
         ksl03  TYPE ty_amt, ksl04 TYPE ty_amt, ksl05 TYPE ty_amt,
         ksl06  TYPE ty_amt, ksl07 TYPE ty_amt, ksl08 TYPE ty_amt,
         ksl09  TYPE ty_amt, ksl10 TYPE ty_amt, ksl11 TYPE ty_amt,
         ksl12  TYPE ty_amt, ksl13 TYPE ty_amt, ksl14 TYPE ty_amt,
         ksl15  TYPE ty_amt, ksl16 TYPE ty_amt,
       END OF ty_jvto1.

" ACDOCA aggregated per key (and D/C indicator)
TYPES: BEGIN OF ty_acd,
         vname TYPE acdoca-vname,
         egrup TYPE acdoca-egrup,
         recid TYPE acdoca-recid,
         racct TYPE acdoca-racct,
         drcrk TYPE shkzg,
         hsl   TYPE ty_amt,
         ksl   TYPE ty_amt,
       END OF ty_acd.

" Measure / value pair used to fill the venture columns
TYPES: BEGIN OF ty_mv,
         m TYPE c LENGTH 1,        " O opening, D debit, C credit, B closing
         v TYPE ty_amt,
       END OF ty_mv,
       ty_t_mv TYPE STANDARD TABLE OF ty_mv WITH EMPTY KEY.

*----------------------------------------------------------------------*
* Global data
*----------------------------------------------------------------------*
DATA: gt_bal       TYPE ty_t_bal,
      gt_detail    TYPE STANDARD TABLE OF ty_bal WITH EMPTY KEY,
      gr_matrix    TYPE REF TO data,
      gt_coltext   TYPE string_table,       " NAME|long text|medium text
      gv_cutoff    TYPE timestamp,
      gv_hwaer     TYPE waers,
      gv_kwaer     TYPE waers,
      gv_ktopl     TYPE ktopl,
      gr_noblank   TYPE RANGE OF acdoca-vname,
      gv_sel_vname TYPE acdoca-vname,        " for SELECT-OPTIONS only
      gv_sel_recid TYPE acdoca-recid.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY MEMORY ID buk,
              p_gjahr TYPE gjahr OBLIGATORY,
              p_perfr TYPE poper DEFAULT '001' MODIF ID dis,
              p_perto TYPE poper OBLIGATORY.
  SELECT-OPTIONS: s_vname FOR gv_sel_vname,
                  s_recid FOR gv_sel_recid.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.
  PARAMETERS: p_hsl  RADIOBUTTON GROUP cur DEFAULT 'X',
              p_ksl  RADIOBUTTON GROUP cur,
              p_both RADIOBUTTON GROUP cur.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-b03.
  PARAMETERS: p_matrix RADIOBUTTON GROUP lay DEFAULT 'X',
              p_detail RADIOBUTTON GROUP lay,
              p_nozero AS CHECKBOX DEFAULT 'X',
              p_blank  AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK b03.

SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE TEXT-b04.
  PARAMETERS: p_cutdt TYPE d       DEFAULT '20260926' OBLIGATORY,
              p_cuttm TYPE t       DEFAULT '235959'   OBLIGATORY,
              p_tzone TYPE tznzone DEFAULT 'INDIA'    OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b04.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'DIS'.
      screen-input = '0'.          " Period from is fixed to 1
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  p_perfr = '001'.
  IF p_gjahr < gc_base_year.
    MESSAGE |Fiscal year must be { gc_base_year } or later| TYPE 'E' ##NO_TEXT.
  ENDIF.
  IF p_perto < 1 OR p_perto > 16.
    MESSAGE 'Period to must be between 1 and 16' TYPE 'E' ##NO_TEXT.
  ENDIF.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'BUKRS' FIELD p_bukrs
    ID 'ACTVT' FIELD '03'.
  IF sy-subrc <> 0.
    MESSAGE |No display authorization for company code { p_bukrs }| TYPE 'E' ##NO_TEXT.
  ENDIF.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  PERFORM init.
  PERFORM read_jvto1.
  PERFORM read_acdoca_opening.
  PERFORM read_acdoca_period.
  PERFORM finalize.

  IF gt_detail IS INITIAL.
    MESSAGE 'No data found for the selection' TYPE 'S' DISPLAY LIKE 'W' ##NO_TEXT.
    RETURN.
  ENDIF.

  IF p_matrix = abap_true.
    PERFORM build_matrix.
    PERFORM display_matrix.
  ELSE.
    PERFORM display_detail.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form INIT - cut-over time stamp, currencies, chart of accounts
*&---------------------------------------------------------------------*
FORM init.
  " ACDOCA-TIMESTAMP is UTC: convert the local cut-over moment to UTC
  CONVERT DATE p_cutdt TIME p_cuttm INTO TIME STAMP gv_cutoff TIME ZONE p_tzone.

  SELECT SINGLE waers, ktopl FROM t001 WHERE bukrs = @p_bukrs
    INTO (@gv_hwaer, @gv_ktopl).

  SELECT SINGLE rkcur FROM acdoca
    WHERE rldnr  = @gc_ldgr_gl
      AND rbukrs = @p_bukrs
      AND rkcur <> @space
    INTO @gv_kwaer.

  " Lines without venture are excluded unless requested
  CLEAR gr_noblank.
  IF p_blank IS INITIAL.
    gr_noblank = VALUE #( ( sign = 'E' option = 'EQ' low = space ) ).
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_BAL - find or create the accumulation line for a key
*&---------------------------------------------------------------------*
FORM get_bal USING    iv_vname TYPE acdoca-vname
                      iv_egrup TYPE acdoca-egrup
                      iv_recid TYPE acdoca-recid
                      iv_racct TYPE acdoca-racct
             CHANGING cr_bal   TYPE REF TO ty_bal.
  READ TABLE gt_bal REFERENCE INTO cr_bal
       WITH TABLE KEY vname = iv_vname egrup = iv_egrup
                      recid = iv_recid racct = iv_racct.
  IF sy-subrc <> 0.
    INSERT VALUE #( vname = iv_vname egrup = iv_egrup
                    recid = iv_recid racct = iv_racct )
           INTO TABLE gt_bal REFERENCE INTO cr_bal.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form READ_JVTO1 - 2026 balances from the JV ledger totals table
*&---------------------------------------------------------------------*
FORM read_jvto1.
  DATA: lt_jvto1 TYPE STANDARD TABLE OF ty_jvto1 WITH EMPTY KEY,
        lr_bal   TYPE REF TO ty_bal,
        lv_per   TYPE n LENGTH 2,
        lv_last  TYPE i,
        lv_h     TYPE ty_amt,
        lv_k     TYPE ty_amt.
  FIELD-SYMBOLS: <lv_h> TYPE ty_amt,
                 <lv_k> TYPE ty_amt.

  SELECT rjvnam, regrou, rrecin, racct, drcrk,
         SUM( hslvt ) AS hslvt,
         SUM( hsl01 ) AS hsl01, SUM( hsl02 ) AS hsl02, SUM( hsl03 ) AS hsl03,
         SUM( hsl04 ) AS hsl04, SUM( hsl05 ) AS hsl05, SUM( hsl06 ) AS hsl06,
         SUM( hsl07 ) AS hsl07, SUM( hsl08 ) AS hsl08, SUM( hsl09 ) AS hsl09,
         SUM( hsl10 ) AS hsl10, SUM( hsl11 ) AS hsl11, SUM( hsl12 ) AS hsl12,
         SUM( hsl13 ) AS hsl13, SUM( hsl14 ) AS hsl14, SUM( hsl15 ) AS hsl15,
         SUM( hsl16 ) AS hsl16,
         SUM( kslvt ) AS kslvt,
         SUM( ksl01 ) AS ksl01, SUM( ksl02 ) AS ksl02, SUM( ksl03 ) AS ksl03,
         SUM( ksl04 ) AS ksl04, SUM( ksl05 ) AS ksl05, SUM( ksl06 ) AS ksl06,
         SUM( ksl07 ) AS ksl07, SUM( ksl08 ) AS ksl08, SUM( ksl09 ) AS ksl09,
         SUM( ksl10 ) AS ksl10, SUM( ksl11 ) AS ksl11, SUM( ksl12 ) AS ksl12,
         SUM( ksl13 ) AS ksl13, SUM( ksl14 ) AS ksl14, SUM( ksl15 ) AS ksl15,
         SUM( ksl16 ) AS ksl16
    FROM jvto1
    WHERE rldnr  = @gc_ldgr_jv
      AND rrcty  = @gc_rrcty
      AND rvers  = @gc_rvers
      AND rbukrs = @p_bukrs
      AND ryear  = @gc_base_year
      AND rjvnam IN @s_vname
      AND rjvnam IN @gr_noblank
      AND rrecin IN @s_recid
    GROUP BY rjvnam, regrou, rrecin, racct, drcrk
    INTO CORRESPONDING FIELDS OF TABLE @lt_jvto1.

  " Periods that belong to the period movement (2026) or to the
  " opening balance (later years: all 16 periods of 2026)
  lv_last = COND #( WHEN p_gjahr = gc_base_year THEN p_perto ELSE 16 ).

  LOOP AT lt_jvto1 INTO DATA(ls_j).
    PERFORM get_bal USING ls_j-rjvnam ls_j-regrou ls_j-rrecin ls_j-racct
                    CHANGING lr_bal.

    " Carryforward is always part of the opening balance
    lr_bal->ob_h_jv += ls_j-hslvt.
    lr_bal->ob_k_jv += ls_j-kslvt.

    CLEAR: lv_h, lv_k.
    DO lv_last TIMES.
      lv_per = sy-index.
      ASSIGN COMPONENT |HSL{ lv_per }| OF STRUCTURE ls_j TO <lv_h>.
      ASSIGN COMPONENT |KSL{ lv_per }| OF STRUCTURE ls_j TO <lv_k>.
      lv_h += <lv_h>.
      lv_k += <lv_k>.
    ENDDO.

    IF p_gjahr = gc_base_year.
      " 2026: periods 1..P are the debit / credit of the report
      IF ls_j-drcrk = gc_credit.
        lr_bal->cr_h += lv_h.
        lr_bal->cr_k += lv_k.
      ELSE.
        lr_bal->dr_h += lv_h.
        lr_bal->dr_k += lv_k.
      ENDIF.
      lr_bal->pr_h_jv += lv_h.
      lr_bal->pr_k_jv += lv_k.
    ELSE.
      " Later years: 2026 periods 1..16 belong to the opening balance
      lr_bal->ob_h_jv += lv_h.
      lr_bal->ob_k_jv += lv_k.
    ENDIF.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form READ_ACDOCA_OPENING - ACDOCA from 2026 up to the year before
*&   the selected year, periods 1-16. 2026 only after the cut-over.
*&---------------------------------------------------------------------*
FORM read_acdoca_opening.
  DATA: lt_acd TYPE STANDARD TABLE OF ty_acd WITH EMPTY KEY,
        lr_bal TYPE REF TO ty_bal.

  IF p_gjahr = gc_base_year.
    RETURN.                       " 2026: opening comes from JVTO1 only
  ENDIF.

  SELECT vname, egrup, recid, racct,
         SUM( hsl ) AS hsl, SUM( ksl ) AS ksl
    FROM acdoca
    WHERE rldnr  = @gc_ldgr_gl
      AND rrcty  = @gc_rrcty
      AND rbukrs = @p_bukrs
      AND gjahr >= @gc_base_year
      AND gjahr  < @p_gjahr
      AND poper BETWEEN '001' AND '016'
      AND ( acdoca~gjahr <> @gc_base_year OR acdoca~timestamp > @gv_cutoff )
      AND vname IN @s_vname
      AND vname IN @gr_noblank
      AND recid IN @s_recid
    GROUP BY vname, egrup, recid, racct
    INTO CORRESPONDING FIELDS OF TABLE @lt_acd.

  LOOP AT lt_acd INTO DATA(ls_a).
    PERFORM get_bal USING ls_a-vname ls_a-egrup ls_a-recid ls_a-racct
                    CHANGING lr_bal.
    lr_bal->ob_h_acd += ls_a-hsl.
    lr_bal->ob_k_acd += ls_a-ksl.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form READ_ACDOCA_PERIOD - ACDOCA of the selected year, periods 1..P
*&   (2026 only after the cut-over), split by debit / credit
*&---------------------------------------------------------------------*
FORM read_acdoca_period.
  DATA: lt_acd TYPE STANDARD TABLE OF ty_acd WITH EMPTY KEY,
        lr_bal TYPE REF TO ty_bal.

  SELECT vname, egrup, recid, racct, drcrk,
         SUM( hsl ) AS hsl, SUM( ksl ) AS ksl
    FROM acdoca
    WHERE rldnr  = @gc_ldgr_gl
      AND rrcty  = @gc_rrcty
      AND rbukrs = @p_bukrs
      AND gjahr  = @p_gjahr
      AND poper BETWEEN '001' AND @p_perto
      AND ( acdoca~gjahr <> @gc_base_year OR acdoca~timestamp > @gv_cutoff )
      AND vname IN @s_vname
      AND vname IN @gr_noblank
      AND recid IN @s_recid
    GROUP BY vname, egrup, recid, racct, drcrk
    INTO CORRESPONDING FIELDS OF TABLE @lt_acd.

  LOOP AT lt_acd INTO DATA(ls_a).
    PERFORM get_bal USING ls_a-vname ls_a-egrup ls_a-recid ls_a-racct
                    CHANGING lr_bal.
    IF ls_a-drcrk = gc_credit.
      lr_bal->cr_h += ls_a-hsl.
      lr_bal->cr_k += ls_a-ksl.
    ELSE.
      lr_bal->dr_h += ls_a-hsl.
      lr_bal->dr_k += ls_a-ksl.
    ENDIF.
    lr_bal->pr_h_acd += ls_a-hsl.
    lr_bal->pr_k_acd += ls_a-ksl.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form FINALIZE - totals, zero suppression, G/L texts, sorted output
*&---------------------------------------------------------------------*
FORM finalize.
  DATA: lt_racct TYPE SORTED TABLE OF acdoca-racct WITH UNIQUE KEY table_line,
        lt_skat  TYPE HASHED TABLE OF skat WITH UNIQUE KEY saknr.

  LOOP AT gt_bal ASSIGNING FIELD-SYMBOL(<ls_bal>).
    <ls_bal>-ob_h = <ls_bal>-ob_h_jv + <ls_bal>-ob_h_acd.
    <ls_bal>-ob_k = <ls_bal>-ob_k_jv + <ls_bal>-ob_k_acd.
    <ls_bal>-cb_h = <ls_bal>-ob_h + <ls_bal>-dr_h + <ls_bal>-cr_h.
    <ls_bal>-cb_k = <ls_bal>-ob_k + <ls_bal>-dr_k + <ls_bal>-cr_k.
  ENDLOOP.

  IF p_nozero = abap_true.
    DELETE gt_bal WHERE ob_h = 0 AND dr_h = 0 AND cr_h = 0 AND cb_h = 0
                    AND ob_k = 0 AND dr_k = 0 AND cr_k = 0 AND cb_k = 0.
  ENDIF.

  LOOP AT gt_bal ASSIGNING <ls_bal>.
    INSERT <ls_bal>-racct INTO TABLE lt_racct.
  ENDLOOP.
  IF lt_racct IS NOT INITIAL.
    SELECT * FROM skat
      FOR ALL ENTRIES IN @lt_racct
      WHERE spras = @sy-langu
        AND ktopl = @gv_ktopl
        AND saknr = @lt_racct-table_line
      INTO TABLE @lt_skat.
  ENDIF.

  LOOP AT gt_bal ASSIGNING <ls_bal>.
    <ls_bal>-txt50 = VALUE #( lt_skat[ saknr = <ls_bal>-racct ]-txt50 OPTIONAL ).
    APPEND <ls_bal> TO gt_detail.
  ENDLOOP.
  SORT gt_detail BY vname egrup recid racct.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form BUILD_MATRIX - one row per G/L account, column groups per JV
*&---------------------------------------------------------------------*
FORM build_matrix.
  DATA: lt_comp   TYPE cl_abap_structdescr=>component_table,
        lt_vname  TYPE SORTED TABLE OF acdoca-vname WITH UNIQUE KEY table_line,
        lt_curr   TYPE string_table,
        lt_meas   TYPE string_table,
        lt_val    TYPE ty_t_mv,
        lv_idx    TYPE n LENGTH 3,
        lv_amt    TYPE ty_amt,
        lv_racct  TYPE acdoca-racct,
        lv_txt50  TYPE skat-txt50,
        lr_line   TYPE REF TO data.
  FIELD-SYMBOLS: <lt_matrix> TYPE STANDARD TABLE,
                 <ls_row>    TYPE any,
                 <lv_fld>    TYPE any.

  DATA(lo_amt)   = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( lv_amt ) ).
  DATA(lo_racct) = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( lv_racct ) ).
  DATA(lo_txt)   = CAST cl_abap_datadescr( cl_abap_typedescr=>describe_by_data( lv_txt50 ) ).

  LOOP AT gt_detail INTO DATA(ls_det).
    INSERT ls_det-vname INTO TABLE lt_vname.
  ENDLOOP.

  IF p_hsl = abap_true OR p_both = abap_true.
    APPEND `H` TO lt_curr.
  ENDIF.
  IF p_ksl = abap_true OR p_both = abap_true.
    APPEND `K` TO lt_curr.
  ENDIF.
  " measure key | long text | short text
  lt_meas = VALUE #( ( `O|Opening Bal|OB` ) ( `D|Debit|Dr` )
                     ( `C|Credit|Cr` )      ( `B|Closing Bal|CB` ) ) ##NO_TEXT.

  " Key columns
  APPEND VALUE #( name = 'RACCT' type = lo_racct ) TO lt_comp.
  APPEND VALUE #( name = 'TXT50' type = lo_txt )   TO lt_comp.
  APPEND `RACCT|G/L Account|G/L Account` TO gt_coltext ##NO_TEXT.
  APPEND `TXT50|G/L Description|G/L Description` TO gt_coltext ##NO_TEXT.

  " Per venture: Opening / Debit / Credit / Closing per currency
  LOOP AT lt_vname INTO DATA(lv_vname).
    lv_idx = sy-tabix.
    DATA(lv_jv) = COND string( WHEN lv_vname IS INITIAL THEN `(no JV)` ELSE CONV string( lv_vname ) ).
    LOOP AT lt_curr INTO DATA(lv_c).
      DATA(lv_cur) = COND string( WHEN lv_c = `H` THEN CONV string( gv_hwaer ) ELSE CONV string( gv_kwaer ) ).
      LOOP AT lt_meas INTO DATA(lv_m).
        SPLIT lv_m AT '|' INTO DATA(lv_mk) DATA(lv_ml) DATA(lv_ms).
        DATA(lv_name) = |J{ lv_idx }_{ lv_c }_{ lv_mk }|.
        APPEND VALUE #( name = lv_name type = lo_amt ) TO lt_comp.
        APPEND |{ lv_name }\|{ lv_jv } { lv_ml } { lv_cur }\|{ lv_jv } { lv_ms } { lv_cur }| TO gt_coltext.
      ENDLOOP.
    ENDLOOP.
  ENDLOOP.

  " Total over all ventures
  LOOP AT lt_curr INTO lv_c.
    lv_cur = COND string( WHEN lv_c = `H` THEN CONV string( gv_hwaer ) ELSE CONV string( gv_kwaer ) ).
    LOOP AT lt_meas INTO lv_m.
      SPLIT lv_m AT '|' INTO lv_mk lv_ml lv_ms.
      lv_name = |TOT_{ lv_c }_{ lv_mk }|.
      APPEND VALUE #( name = lv_name type = lo_amt ) TO lt_comp.
      APPEND |{ lv_name }\|Total { lv_ml } { lv_cur }\|Total { lv_ms } { lv_cur }| TO gt_coltext.
    ENDLOOP.
  ENDLOOP.

  DATA(lo_struct) = cl_abap_structdescr=>create( lt_comp ).
  DATA(lo_table)  = cl_abap_tabledescr=>create( p_line_type = lo_struct ).
  CREATE DATA gr_matrix TYPE HANDLE lo_table.
  ASSIGN gr_matrix->* TO <lt_matrix>.

  " Fill: one row per G/L account; equity groups / RIs are summed per venture
  DATA(lt_sorted) = gt_detail.
  SORT lt_sorted BY racct vname.
  LOOP AT lt_sorted INTO DATA(ls_d) GROUP BY ls_d-racct ASSIGNING FIELD-SYMBOL(<lv_racct>).
    CREATE DATA lr_line TYPE HANDLE lo_struct.
    ASSIGN lr_line->* TO <ls_row>.
    LOOP AT GROUP <lv_racct> INTO DATA(ls_m).
      ASSIGN COMPONENT 'RACCT' OF STRUCTURE <ls_row> TO <lv_fld>.
      <lv_fld> = ls_m-racct.
      ASSIGN COMPONENT 'TXT50' OF STRUCTURE <ls_row> TO <lv_fld>.
      <lv_fld> = ls_m-txt50.
      lv_idx = line_index( lt_vname[ table_line = ls_m-vname ] ).
      LOOP AT lt_curr INTO lv_c.
        IF lv_c = `H`.
          lt_val = VALUE #( ( m = 'O' v = ls_m-ob_h ) ( m = 'D' v = ls_m-dr_h )
                            ( m = 'C' v = ls_m-cr_h ) ( m = 'B' v = ls_m-cb_h ) ).
        ELSE.
          lt_val = VALUE #( ( m = 'O' v = ls_m-ob_k ) ( m = 'D' v = ls_m-dr_k )
                            ( m = 'C' v = ls_m-cr_k ) ( m = 'B' v = ls_m-cb_k ) ).
        ENDIF.
        LOOP AT lt_val INTO DATA(ls_val).
          ASSIGN COMPONENT |J{ lv_idx }_{ lv_c }_{ ls_val-m }| OF STRUCTURE <ls_row> TO <lv_fld>.
          IF sy-subrc = 0.
            <lv_fld> = <lv_fld> + ls_val-v.
          ENDIF.
          ASSIGN COMPONENT |TOT_{ lv_c }_{ ls_val-m }| OF STRUCTURE <ls_row> TO <lv_fld>.
          IF sy-subrc = 0.
            <lv_fld> = <lv_fld> + ls_val-v.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
    APPEND <ls_row> TO <lt_matrix>.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_MATRIX
*&---------------------------------------------------------------------*
FORM display_matrix.
  DATA lo_alv TYPE REF TO cl_salv_table.
  FIELD-SYMBOLS <lt_matrix> TYPE STANDARD TABLE.

  ASSIGN gr_matrix->* TO <lt_matrix>.
  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                              CHANGING  t_table      = <lt_matrix> ).
    CATCH cx_salv_msg.
      RETURN.
  ENDTRY.
  PERFORM alv_common USING lo_alv.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_DETAIL
*&---------------------------------------------------------------------*
FORM display_detail.
  DATA: lo_alv  TYPE REF TO cl_salv_table,
        lt_hide TYPE string_table.
  DATA(lv_h) = CONV string( gv_hwaer ).
  DATA(lv_k) = CONV string( gv_kwaer ).

  gt_coltext = VALUE #(
    ( `VNAME|Venture|Venture` ) ( `EGRUP|Equity Group|Equity Group` )
    ( `RECID|Recovery Indicator|Recovery Ind.` )
    ( `RACCT|G/L Account|G/L Account` ) ( `TXT50|G/L Description|G/L Description` )
    ( |OB_H\|Opening Bal { lv_h }\|OB { lv_h }| )   ( |DR_H\|Debit { lv_h }\|Dr { lv_h }| )
    ( |CR_H\|Credit { lv_h }\|Cr { lv_h }| )        ( |CB_H\|Closing Bal { lv_h }\|CB { lv_h }| )
    ( |OB_K\|Opening Bal { lv_k }\|OB { lv_k }| )   ( |DR_K\|Debit { lv_k }\|Dr { lv_k }| )
    ( |CR_K\|Credit { lv_k }\|Cr { lv_k }| )        ( |CB_K\|Closing Bal { lv_k }\|CB { lv_k }| )
    ( |OB_H_JV\|Opening from JVTO1 { lv_h }\|OB JVTO1 { lv_h }| )
    ( |OB_H_ACD\|Opening from ACDOCA { lv_h }\|OB ACDOCA { lv_h }| )
    ( |PR_H_JV\|Period (Dr+Cr) JVTO1 { lv_h }\|Per JVTO1 { lv_h }| )
    ( |PR_H_ACD\|Period (Dr+Cr) ACDOCA { lv_h }\|Per ACDOCA { lv_h }| )
    ( |OB_K_JV\|Opening from JVTO1 { lv_k }\|OB JVTO1 { lv_k }| )
    ( |OB_K_ACD\|Opening from ACDOCA { lv_k }\|OB ACDOCA { lv_k }| )
    ( |PR_K_JV\|Period (Dr+Cr) JVTO1 { lv_k }\|Per JVTO1 { lv_k }| )
    ( |PR_K_ACD\|Period (Dr+Cr) ACDOCA { lv_k }\|Per ACDOCA { lv_k }| ) ) ##NO_TEXT.

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                              CHANGING  t_table      = gt_detail ).
    CATCH cx_salv_msg.
      RETURN.
  ENDTRY.

  " Hide the currency that was not selected
  IF p_hsl = abap_true.
    lt_hide = VALUE #( ( `OB_K` ) ( `DR_K` ) ( `CR_K` ) ( `CB_K` )
                       ( `OB_K_JV` ) ( `OB_K_ACD` ) ( `PR_K_JV` ) ( `PR_K_ACD` ) ).
  ELSEIF p_ksl = abap_true.
    lt_hide = VALUE #( ( `OB_H` ) ( `DR_H` ) ( `CR_H` ) ( `CB_H` )
                       ( `OB_H_JV` ) ( `OB_H_ACD` ) ( `PR_H_JV` ) ( `PR_H_ACD` ) ).
  ENDIF.
  DATA(lo_cols) = lo_alv->get_columns( ).
  LOOP AT lt_hide INTO DATA(lv_hide).
    TRY.
        lo_cols->get_column( CONV lvc_fname( lv_hide ) )->set_technical( abap_true ).
      CATCH cx_salv_not_found.
    ENDTRY.
  ENDLOOP.

  " Sort with subtotals per venture
  TRY.
      DATA(lo_sort) = lo_alv->get_sorts( ).
      lo_sort->add_sort( columnname = 'VNAME' subtotal = abap_true ).
      lo_sort->add_sort( columnname = 'EGRUP' ).
      lo_sort->add_sort( columnname = 'RECID' ).
      lo_sort->add_sort( columnname = 'RACCT' ).
    CATCH cx_salv_not_found cx_salv_existing cx_salv_data_error.
  ENDTRY.

  PERFORM alv_common USING lo_alv.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form ALV_COMMON - column texts, totals, header, display
*&---------------------------------------------------------------------*
FORM alv_common USING io_alv TYPE REF TO cl_salv_table.
  DATA lo_col TYPE REF TO cl_salv_column_table.

  io_alv->get_functions( )->set_all( abap_true ).
  io_alv->get_display_settings( )->set_striped_pattern( abap_true ).
  DATA(lo_cols) = io_alv->get_columns( ).
  lo_cols->set_optimize( abap_true ).
  DATA(lo_aggr) = io_alv->get_aggregations( ).

  LOOP AT gt_coltext INTO DATA(lv_text).
    SPLIT lv_text AT '|' INTO DATA(lv_col) DATA(lv_long) DATA(lv_med).
    TRY.
        lo_col ?= lo_cols->get_column( CONV lvc_fname( lv_col ) ).
        lo_col->set_long_text( CONV scrtext_l( lv_long ) ).
        lo_col->set_medium_text( CONV scrtext_m( lv_med ) ).
        lo_col->set_short_text( CONV scrtext_s( lv_med ) ).
        IF lv_col <> 'VNAME' AND lv_col <> 'EGRUP' AND lv_col <> 'RECID'
           AND lv_col <> 'RACCT' AND lv_col <> 'TXT50'.
          lo_aggr->add_aggregation( columnname  = CONV lvc_fname( lv_col )
                                    aggregation = if_salv_c_aggregation=>total ).
        ENDIF.
      CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
    ENDTRY.
  ENDLOOP.

  " Header
  DATA(lo_grid) = NEW cl_salv_form_layout_grid( ).
  lo_grid->create_header_information( row = 1 column = 1
    text = 'Joint Venture Trial Balance (ITD)' ) ##NO_TEXT.
  lo_grid->create_label( row = 2 column = 1 text = 'Company Code / Year' ) ##NO_TEXT.
  lo_grid->create_text(  row = 2 column = 2 text = |{ p_bukrs } / { p_gjahr }| ).
  lo_grid->create_label( row = 3 column = 1 text = 'Periods' ) ##NO_TEXT.
  lo_grid->create_text(  row = 3 column = 2 text = |{ p_perfr ALPHA = OUT } To { p_perto ALPHA = OUT }| ).
  lo_grid->create_label( row = 4 column = 1 text = 'Currency' ) ##NO_TEXT.
  lo_grid->create_text(  row = 4 column = 2
    text = COND string( WHEN p_hsl = abap_true THEN |Company code currency { gv_hwaer }|
                        WHEN p_ksl = abap_true THEN |Group currency { gv_kwaer }|
                        ELSE |{ gv_hwaer } and { gv_kwaer }| ) ) ##NO_TEXT.
  lo_grid->create_label( row = 5 column = 1 text = 'ACDOCA 2026 posted after' ) ##NO_TEXT.
  lo_grid->create_text(  row = 5 column = 2
    text = |{ p_cutdt DATE = USER } { p_cuttm TIME = USER } ({ p_tzone })| ).
  io_alv->set_top_of_list( lo_grid ).
  io_alv->set_top_of_list_print( lo_grid ).

  io_alv->display( ).
ENDFORM.
