*&---------------------------------------------------------------------*
*& Report  ZSD_PRICING_COMPARE
*&---------------------------------------------------------------------*
*& CCBJI - Pricing regression validation after ECC -> S/4HANA migration
*&
*& PURPOSE
*& -------
*& Sales orders migrated from ECC carry the pricing result calculated
*& by the ECC pricing engine (now stored in PRCD_ELEMENTS after the
*& KONV -> PRCD_ELEMENTS conversion). To prove that the S/4HANA pricing
*& configuration (pricing procedure, condition records, VOFM routines)
*& reproduces the ECC result, the program lets S/4HANA re-derive the
*& pricing from scratch on a copy order Y.
*&
*& Two selection modes (radio buttons):
*&
*&   R1 AUTOMATIC (default): document type and creation date range are
*&      mandatory; optionally a customer range and how many orders to
*&      check (default 1). The program picks the top-N orders X with
*&      the HIGHEST net value (VBAK-NETWR) in that period - overall
*&      when no customer is entered, or per customer when a customer
*&      range is entered (e.g. 5 customers x top 5 orders = 25 copy
*&      orders).
*&
*&   R2 ORDER LIST: the user enters specific sales order numbers
*&      (multiple selection); each listed order is copied and compared.
*&
*& Every other detail of X (org data, partners, items, quantities,
*& pricing date, conditions) is read from the database
*& (VBAK/VBAP/VBPA/VBKD/PRCD_ELEMENTS).
*&
*& Y is created with BAPI_SALESORDER_CREATEFROMDAT2 and
*& LOGIC_SWITCH-PRICING = 'B' (carry out new pricing) using X's
*& ORIGINAL pricing date (VBKD-PRSDT), so exactly the same
*& condition-record validity periods apply as when X was priced in ECC
*& - a true replication of X. Y's conditions are read back from
*& PRCD_ELEMENTS and compared with X's conditions. Y remains in the
*& system; its PO number field is stamped 'PRCVAL-<X>' so the test
*& copies are easy to find (VA05 / VBAK-BSTNK) and clean up.
*&
*& COMPARISON
*& ----------
*& Condition lines are matched per item + condition type + occurrence
*& and compared on rate (KBETR), pricing unit (KPEIN), condition unit
*& (KMEIN) and condition value (KWERT). In addition the stored value
*& fields are compared between X and Y (customer requirement):
*&   VBAP: NETWR NETPR SKTOF WAVWR KZWI1..KZWI6 MWSBP  (per item)
*&   VBAK: NETWR                                       (header)
*&
*& OUTPUT (two screens)
*& --------------------
*&   Screen 1: overview ALV - one row per order with net values X/Y,
*&             check counters and a clear verdict (ALL OK / CHECK /
*&             ERROR). The report header shows the overall RESULT.
*&   Screen 2: double-click an order row -> popup ALV with the full
*&             condition-by-condition and field-by-field comparison
*&             of that order (incl. the pricing date used).
*&   Level 3:  double-click a condition row in the detail -> VK13 for
*&             that condition type (check the record valid on the
*&             pricing date shown in the row).
*&
*& Amounts are normalised to external format before comparison, i.e.
*& the TCURX decimal shift is applied (JPY has 0 decimals -> internal
*& value * 100) and percentage rates (KRECH = 'A') are divided by 10.
*& BAPI output amounts are already external (cf. SAP KBA 2333377).
*&
*& Classification:
*&   OK          rate/value identical
*&   MISMATCH    values differ -> S/4 pricing deviates from ECC result
*&   MISSING_S4  condition exists on X but was not re-determined on Y
*&               (missing/wrong condition record or access sequence);
*&               red only if X carried a value <> 0, otherwise yellow
*&               (zero value = no impact on the pricing outcome)
*&   NEW_IN_S4   condition determined on Y but absent on X; red only
*&               if Y carries a value <> 0, otherwise yellow
*&   MANUAL      manually entered condition on X (KHERK 'C' / KMPRS) -
*&               not re-derivable by repricing, reported for info only
*&   ERROR       order Y could not be created (BAPI messages)
*&
*& Inactive condition lines (KINAK <> space) and statistical lines
*& (KSTAT = 'X') are ignored. Fully rejected items of X
*& (ABGRU <> space) are skipped.
*&
*& TEXT ELEMENTS (maintain in SE38 -> Goto -> Text elements)
*&   TEXT-001  Automatic selection (top orders by net value)
*&   TEXT-002  Selection mode
*&   TEXT-003  Specific sales orders
*&---------------------------------------------------------------------*
REPORT zsd_pricing_compare.

TABLES: vbak.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_auto RADIOBUTTON GROUP md DEFAULT 'X', " R1 automatic top-N
              p_list RADIOBUTTON GROUP md.             " R2 explicit orders
SELECTION-SCREEN END OF BLOCK b0.

* --- R1: automatic selection (doc type + period mandatory) ------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_auart FOR vbak-auart,              " document type
                  s_erdat FOR vbak-erdat,              " creation period
                  s_kunnr FOR vbak-kunnr.              " customer (optional)
  PARAMETERS p_topn TYPE i DEFAULT 1.                  " orders to check
SELECTION-SCREEN END OF BLOCK b1.

* --- R2: user-specified sales orders ----------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
  SELECT-OPTIONS s_vbeln FOR vbak-vbeln.               " order numbers
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Local class
*----------------------------------------------------------------------*
CLASS lcl_app DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS run.

  PRIVATE SECTION.

    TYPES ty_amount TYPE p LENGTH 15 DECIMALS 3.

    TYPES:
      BEGIN OF ty_vbak,
        vbeln TYPE vbak-vbeln,
        auart TYPE vbak-auart,
        vkorg TYPE vbak-vkorg,
        vtweg TYPE vbak-vtweg,
        spart TYPE vbak-spart,
        knumv TYPE vbak-knumv,
        waerk TYPE vbak-waerk,
        erdat TYPE vbak-erdat,
        kunnr TYPE vbak-kunnr,
        netwr TYPE vbak-netwr,
      END OF ty_vbak,
      ty_t_vbak TYPE STANDARD TABLE OF ty_vbak WITH DEFAULT KEY,

      BEGIN OF ty_vbap,
        posnr  TYPE vbap-posnr,
        matnr  TYPE vbap-matnr,
        werks  TYPE vbap-werks,
        kwmeng TYPE vbap-kwmeng,
        vrkme  TYPE vbap-vrkme,
        abgru  TYPE vbap-abgru,
        " item fields compared ECC vs S/4 (customer requirement)
        netwr  TYPE vbap-netwr,
        netpr  TYPE vbap-netpr,
        sktof  TYPE vbap-sktof,
        wavwr  TYPE vbap-wavwr,
        kzwi1  TYPE vbap-kzwi1,
        kzwi2  TYPE vbap-kzwi2,
        kzwi3  TYPE vbap-kzwi3,
        kzwi4  TYPE vbap-kzwi4,
        kzwi5  TYPE vbap-kzwi5,
        kzwi6  TYPE vbap-kzwi6,
        mwsbp  TYPE vbap-mwsbp,
      END OF ty_vbap,
      ty_t_vbap TYPE STANDARD TABLE OF ty_vbap WITH DEFAULT KEY,

      " normalised condition line (amounts already in external format)
      BEGIN OF ty_cond,
        posnr  TYPE posnr_va,
        kschl  TYPE kscha,
        occ    TYPE i,                 " occurrence of kschl within item
        krech  TYPE prcd_elements-krech,
        rate   TYPE ty_amount,        " KBETR, external / percent
        waers  TYPE waers,
        kpein  TYPE kpein,
        kmein  TYPE kmein,
        kwert  TYPE ty_amount,        " KWERT, external
        kstat  TYPE prcd_elements-kstat,
        kinak  TYPE prcd_elements-kinak,
        kherk  TYPE prcd_elements-kherk,
        kmprs  TYPE prcd_elements-kmprs,
        used   TYPE abap_bool,
      END OF ty_cond,
      ty_t_cond TYPE STANDARD TABLE OF ty_cond WITH DEFAULT KEY,

      BEGIN OF ty_result,
        vbeln_x    TYPE vbeln_va,
        vbeln_y    TYPE vbeln_va,
        kunnr      TYPE kunnr,
        posnr      TYPE posnr_va,
        matnr      TYPE vbap-matnr,
        kschl      TYPE c LENGTH 12,   " condition type or compared field
        status     TYPE c LENGTH 10,
        rate_old   TYPE ty_amount,
        rate_new   TYPE ty_amount,
        rate_diff  TYPE ty_amount,
        waers      TYPE waers,
        kpein_old  TYPE kpein,
        kpein_new  TYPE kpein,
        kmein_old  TYPE kmein,
        kmein_new  TYPE kmein,
        kwert_old  TYPE ty_amount,
        kwert_new  TYPE ty_amount,
        kwert_diff TYPE ty_amount,
        prsdt      TYPE prsdt,         " pricing date used for order Y
        remark     TYPE c LENGTH 200,
        color      TYPE lvc_t_scol,
      END OF ty_result,

      BEGIN OF ty_stat,
        orders     TYPE i,
        errors     TYPE i,
        compared   TYPE i,
        ok         TYPE i,
        mismatch   TYPE i,
        missing    TYPE i,
        new_in_s4  TYPE i,
        manual     TYPE i,
        crit       TYPE i,   " red rows = real differences to investigate
        warn       TYPE i,   " yellow rows = informational only
      END OF ty_stat,

      " one row per order X for the overview ALV (screen 1)
      BEGIN OF ty_summary,
        kunnr      TYPE kunnr,
        vbeln_x    TYPE vbeln_va,
        vbeln_y    TYPE vbeln_va,
        items      TYPE i,
        netwr_x    TYPE ty_amount,
        netwr_y    TYPE ty_amount,
        netwr_diff TYPE ty_amount,
        waers      TYPE waers,
        checks     TYPE i,
        ok         TYPE i,
        crit       TYPE i,
        warn       TYPE i,
        status     TYPE c LENGTH 10,   " ALL OK / CHECK / ERROR
        remark     TYPE c LENGTH 200,
        color      TYPE lvc_t_scol,
      END OF ty_summary.

    CONSTANTS:
      c_ok      TYPE c LENGTH 10 VALUE 'OK',
      c_diff    TYPE c LENGTH 10 VALUE 'MISMATCH',
      c_miss    TYPE c LENGTH 10 VALUE 'MISSING_S4',
      c_new     TYPE c LENGTH 10 VALUE 'NEW_IN_S4',
      c_manual  TYPE c LENGTH 10 VALUE 'MANUAL',
      c_error   TYPE c LENGTH 10 VALUE 'ERROR',
      c_allok   TYPE c LENGTH 10 VALUE 'ALL OK',    " order verdict
      c_check   TYPE c LENGTH 10 VALUE 'CHECK',     " order verdict
      c_percent TYPE c LENGTH 1  VALUE 'A'.

    DATA: mt_result  TYPE STANDARD TABLE OF ty_result,
          mt_detail  TYPE STANDARD TABLE OF ty_result,
          mt_summary TYPE STANDARD TABLE OF ty_summary,
          ms_stat    TYPE ty_stat,
          mv_info    TYPE string,
          mt_tcurx   TYPE HASHED TABLE OF tcurx
                          WITH UNIQUE KEY currkey.

    METHODS process_order
      IMPORTING is_vbak TYPE ty_vbak.

    METHODS get_prcd_conditions
      IMPORTING iv_knumv       TYPE vbak-knumv
                iv_waerk       TYPE waerk
      RETURNING VALUE(rt_cond) TYPE ty_t_cond.

    METHODS create_order_y
      IMPORTING is_vbak  TYPE ty_vbak
                it_vbap  TYPE ty_t_vbap
                iv_prsdt TYPE prsdt
      EXPORTING ev_vbeln_y TYPE vbeln_va
                et_cond    TYPE ty_t_cond
                et_vbap_y  TYPE ty_t_vbap
                ev_netwr_y TYPE vbak-netwr
                ev_waerk_y TYPE vbak-waerk
                ev_error   TYPE string.

    METHODS compare_conditions
      IMPORTING is_vbak    TYPE ty_vbak
                iv_vbeln_y TYPE vbeln_va
                it_vbap    TYPE ty_t_vbap
                it_x       TYPE ty_t_cond
                it_y       TYPE ty_t_cond
                it_vbap_y  TYPE ty_t_vbap
                iv_netwr_y TYPE vbak-netwr
                iv_waerk_y TYPE vbak-waerk
                iv_prsdt   TYPE prsdt.

    METHODS compare_fields
      IMPORTING is_vbak    TYPE ty_vbak
                iv_vbeln_y TYPE vbeln_va
                it_vbap    TYPE ty_t_vbap
                it_vbap_y  TYPE ty_t_vbap
                iv_netwr_y TYPE vbak-netwr
                iv_waerk_y TYPE vbak-waerk
                iv_prsdt   TYPE prsdt.

    METHODS set_occurrence
      CHANGING ct_cond TYPE ty_t_cond.

    METHODS to_external
      IMPORTING iv_amount     TYPE ty_amount
                iv_waers      TYPE waers
                iv_krech      TYPE clike OPTIONAL
      RETURNING VALUE(rv_ext) TYPE ty_amount.

    METHODS collect_messages
      IMPORTING it_return     TYPE bapiret2_t
      RETURNING VALUE(rv_msg) TYPE string.

    METHODS add_result
      IMPORTING is_result TYPE ty_result.

    METHODS display.

    METHODS display_detail
      IMPORTING iv_vbeln_x TYPE vbeln_va.

    METHODS on_double_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.

    METHODS on_detail_click
      FOR EVENT double_click OF cl_salv_events_table
      IMPORTING row column.

ENDCLASS.


CLASS lcl_app IMPLEMENTATION.

  METHOD run.

    DATA: lt_vbak TYPE ty_t_vbak,
          lv_cust TYPE i.

    " ------- mode-dependent input validation --------------------------
    IF p_auto = abap_true.
      IF s_auart[] IS INITIAL OR s_erdat[] IS INITIAL.
        MESSAGE 'Document type and date range are mandatory for automatic selection'(m04)
          TYPE 'E'.
      ENDIF.
      IF p_topn < 1.
        MESSAGE 'Number of orders to check must be at least 1'(m03) TYPE 'E'.
      ENDIF.
    ELSE.
      IF s_vbeln[] IS INITIAL.
        MESSAGE 'Enter at least one sales order number'(m05) TYPE 'E'.
      ENDIF.
    ENDIF.

    " ------- R2: user-specified order numbers -------------------------
    IF p_list = abap_true.
      SELECT vbeln, auart, vkorg, vtweg, spart, knumv, waerk,
             erdat, kunnr, netwr
        FROM vbak
        WHERE vbeln IN @s_vbeln
        ORDER BY vbeln
        INTO CORRESPONDING FIELDS OF TABLE @lt_vbak.

      mv_info = |Order list: { lines( lt_vbak ) } order(s) selected|.

    " ------- R1: pick the top-N highest-value orders ------------------
    "  - without customer restriction: N orders overall
    "  - with customer range: N orders PER customer
    " all further details of X are read from VBAK/VBAP/VBPA/VBKD
    ELSEIF s_kunnr[] IS INITIAL.
      SELECT vbeln, auart, vkorg, vtweg, spart, knumv, waerk,
             erdat, kunnr, netwr
        FROM vbak
        WHERE auart IN @s_auart
          AND erdat IN @s_erdat
        ORDER BY netwr DESCENDING, vbeln
        INTO CORRESPONDING FIELDS OF TABLE @lt_vbak
        UP TO @p_topn ROWS.
    ELSE.
      DATA lt_kunnr TYPE STANDARD TABLE OF vbak-kunnr.
      SELECT DISTINCT kunnr
        FROM vbak
        WHERE auart IN @s_auart
          AND erdat IN @s_erdat
          AND kunnr IN @s_kunnr
        ORDER BY kunnr
        INTO TABLE @lt_kunnr.
      lv_cust = lines( lt_kunnr ).

      LOOP AT lt_kunnr INTO DATA(lv_kunnr).
        SELECT vbeln, auart, vkorg, vtweg, spart, knumv, waerk,
               erdat, kunnr, netwr
          FROM vbak
          WHERE auart IN @s_auart
            AND erdat IN @s_erdat
            AND kunnr = @lv_kunnr
          ORDER BY netwr DESCENDING, vbeln
          APPENDING CORRESPONDING FIELDS OF TABLE @lt_vbak
          UP TO @p_topn ROWS.
      ENDLOOP.
    ENDIF.

    IF lt_vbak IS INITIAL.
      MESSAGE 'No sales orders found for the selection'(m02) TYPE 'S'
        DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF p_auto = abap_true.
      IF s_kunnr[] IS INITIAL.
        mv_info = |Top { p_topn } order(s) by net value: | &
                  |{ lines( lt_vbak ) } order(s) selected|.
      ELSE.
        mv_info = |Top { p_topn } order(s) per customer for { lv_cust } | &
                  |customer(s): { lines( lt_vbak ) } order(s) selected|.
      ENDIF.
    ENDIF.

    LOOP AT lt_vbak INTO DATA(ls_vbak).
      ms_stat-orders = ms_stat-orders + 1.
      process_order( ls_vbak ).
    ENDLOOP.

    display( ).

  ENDMETHOD.


  METHOD process_order.

    DATA: lt_cond_y  TYPE ty_t_cond,
          lt_vbap_y  TYPE ty_t_vbap,
          lv_netwr_y TYPE vbak-netwr,
          lv_waerk_y TYPE vbak-waerk,
          lv_vbeln_y TYPE vbeln_va,
          lv_error   TYPE string.

    " fresh work area per order (method runs once per selected order)
    CLEAR: lt_cond_y[], lt_vbap_y[], lv_netwr_y, lv_waerk_y,
           lv_vbeln_y, lv_error.

    " ------- items of X (skip fully rejected items) -------------------
    DATA lt_vbap TYPE ty_t_vbap.
    CLEAR lt_vbap[].
    SELECT posnr, matnr, werks, kwmeng, vrkme, abgru,
           netwr, netpr, sktof, wavwr,
           kzwi1, kzwi2, kzwi3, kzwi4, kzwi5, kzwi6, mwsbp
      FROM vbap
      WHERE vbeln = @is_vbak-vbeln
      ORDER BY posnr
      INTO CORRESPONDING FIELDS OF TABLE @lt_vbap.

    DELETE lt_vbap WHERE abgru IS NOT INITIAL.
    IF lt_vbap IS INITIAL.
      RETURN.
    ENDIF.

    " ------- pricing date = X's ORIGINAL pricing date -----------------
    " Y must be priced on the same date as X so that identical
    " condition-record validity periods apply (exact replication)
    DATA lv_prsdt TYPE prsdt.
    SELECT SINGLE prsdt FROM vbkd
      WHERE vbeln = @is_vbak-vbeln
        AND posnr = '000000'
      INTO @lv_prsdt.
    IF lv_prsdt IS INITIAL.
      SELECT SINGLE prsdt FROM vbkd
        WHERE vbeln = @is_vbak-vbeln
        INTO @lv_prsdt.
    ENDIF.
    IF lv_prsdt IS INITIAL.
      lv_prsdt = is_vbak-erdat.
    ENDIF.

    " ------- original (migrated ECC) pricing result of X --------------
    DATA(lt_cond_x) = get_prcd_conditions( iv_knumv = is_vbak-knumv
                                           iv_waerk = is_vbak-waerk ).

    " counters before this order - used for the per-order verdict
    DATA(ls_stat0) = ms_stat.

    " ------- re-derive pricing on copy order Y ------------------------
    create_order_y( EXPORTING is_vbak    = is_vbak
                              it_vbap    = lt_vbap
                              iv_prsdt   = lv_prsdt
                    IMPORTING ev_vbeln_y = lv_vbeln_y
                              et_cond    = lt_cond_y
                              et_vbap_y  = lt_vbap_y
                              ev_netwr_y = lv_netwr_y
                              ev_waerk_y = lv_waerk_y
                              ev_error   = lv_error ).

    DATA(ls_sum) = VALUE ty_summary(
        kunnr   = is_vbak-kunnr
        vbeln_x = is_vbak-vbeln
        vbeln_y = lv_vbeln_y
        items   = lines( lt_vbap )
        waers   = is_vbak-waerk
        netwr_x = to_external( iv_amount = CONV #( is_vbak-netwr )
                               iv_waers  = is_vbak-waerk ) ).

    IF lv_error IS NOT INITIAL.
      ms_stat-errors = ms_stat-errors + 1.
      add_result( VALUE #( vbeln_x = is_vbak-vbeln
                           vbeln_y = lv_vbeln_y
                           kunnr   = is_vbak-kunnr
                           kschl   = space
                           prsdt   = lv_prsdt
                           status  = c_error
                           remark  = lv_error ) ).
      ls_sum-status = c_error.
      ls_sum-remark = lv_error.
      ls_sum-color  = VALUE #( ( fname = 'STATUS'
                                 color-col = col_negative
                                 color-int = 1 ) ).
      APPEND ls_sum TO mt_summary.
      RETURN.
    ENDIF.

    " ------- compare X vs Y -------------------------------------------
    compare_conditions( is_vbak    = is_vbak
                        iv_vbeln_y = lv_vbeln_y
                        it_vbap    = lt_vbap
                        it_x       = lt_cond_x
                        it_y       = lt_cond_y
                        it_vbap_y  = lt_vbap_y
                        iv_netwr_y = lv_netwr_y
                        iv_waerk_y = lv_waerk_y
                        iv_prsdt   = lv_prsdt ).

    " ------- per-order verdict for the overview ALV -------------------
    ls_sum-netwr_y    = to_external( iv_amount = CONV #( lv_netwr_y )
                                     iv_waers  = lv_waerk_y ).
    ls_sum-netwr_diff = ls_sum-netwr_y - ls_sum-netwr_x.
    ls_sum-ok     = ms_stat-ok   - ls_stat0-ok.
    ls_sum-crit   = ms_stat-crit - ls_stat0-crit.
    ls_sum-warn   = ms_stat-warn - ls_stat0-warn.
    ls_sum-checks = ls_sum-ok + ls_sum-crit + ls_sum-warn.

    IF ls_sum-crit > 0.
      ls_sum-status = c_check.
      ls_sum-remark = |{ ls_sum-crit } difference(s) - | &
                      |double-click for pricing detail|.
      ls_sum-color  = VALUE #( ( fname = 'STATUS'
                                 color-col = col_negative
                                 color-int = 1 ) ).
    ELSE.
      ls_sum-status = c_allok.
      ls_sum-remark = COND #(
        WHEN ls_sum-warn > 0
        THEN |Pricing matches ({ ls_sum-warn } informational warning(s))|
        ELSE |Pricing matches - S/4 reproduces the ECC result| ).
      ls_sum-color  = VALUE #( ( fname = 'STATUS'
                                 color-col = col_positive ) ).
    ENDIF.
    APPEND ls_sum TO mt_summary.

  ENDMETHOD.


  METHOD get_prcd_conditions.

    " KONV is obsolete in S/4HANA - document conditions are stored in
    " PRCD_ELEMENTS (alternative access: CDS view V_KONV).
    SELECT kposn, stunr, zaehk, kschl, krech, kbetr, waers,
           kpein, kmein, kwert, kstat, kinak, kherk, kmprs
      FROM prcd_elements
      WHERE knumv = @iv_knumv
        AND kposn <> '000000'
      ORDER BY kposn, stunr, zaehk
      INTO TABLE @DATA(lt_prcd).

    LOOP AT lt_prcd INTO DATA(ls_prcd).
      DATA(lv_waers) = COND waers( WHEN ls_prcd-waers IS NOT INITIAL
                                   THEN ls_prcd-waers
                                   ELSE iv_waerk ).
      APPEND VALUE ty_cond(
          posnr = ls_prcd-kposn
          kschl = ls_prcd-kschl
          krech = ls_prcd-krech
          rate  = to_external( iv_amount = CONV #( ls_prcd-kbetr )
                               iv_waers  = lv_waers
                               iv_krech  = ls_prcd-krech )
          waers = lv_waers
          kpein = ls_prcd-kpein
          kmein = ls_prcd-kmein
          kwert = to_external( iv_amount = CONV #( ls_prcd-kwert )
                               iv_waers  = iv_waerk )
          kstat = ls_prcd-kstat
          kinak = ls_prcd-kinak
          kherk = ls_prcd-kherk
          kmprs = ls_prcd-kmprs ) TO rt_cond.
    ENDLOOP.

    set_occurrence( CHANGING ct_cond = rt_cond ).

  ENDMETHOD.


  METHOD create_order_y.

    DATA: ls_hdr TYPE bapisdhd1,
          ls_ls  TYPE bapisdls,
          lt_itm TYPE STANDARD TABLE OF bapisditm,
          lt_prt TYPE STANDARD TABLE OF bapiparnr,
          lt_sch TYPE STANDARD TABLE OF bapischdl,
          lt_ret TYPE bapiret2_t.

    " the method runs once per order X - clear every BAPI input/output
    " and every exporting parameter explicitly so nothing carries over
    " from the previous order in the loop
    CLEAR: ev_vbeln_y, et_cond, et_vbap_y, ev_netwr_y, ev_waerk_y,
           ev_error, ls_hdr, ls_ls.
    CLEAR: lt_itm[], lt_prt[], lt_sch[], lt_ret[].

    " ------- header: copy org data of X, force new pricing ------------
    ls_hdr-doc_type   = is_vbak-auart.
    ls_hdr-sales_org  = is_vbak-vkorg.
    ls_hdr-distr_chan = is_vbak-vtweg.
    ls_hdr-division   = is_vbak-spart.
    ls_hdr-price_date = iv_prsdt.
    ls_hdr-purch_no_c = |PRCVAL-{ is_vbak-vbeln }|.

    ls_ls-pricing = 'B'.               " carry out new pricing

    " ------- partners from X (header level) ---------------------------
    SELECT parvw, kunnr FROM vbpa
      WHERE vbeln = @is_vbak-vbeln
        AND posnr = '000000'
        AND parvw IN ('AG','WE')
      INTO TABLE @DATA(lt_vbpa).
    LOOP AT lt_vbpa INTO DATA(ls_vbpa).
      APPEND VALUE bapiparnr( partn_role = ls_vbpa-parvw
                              partn_numb = ls_vbpa-kunnr ) TO lt_prt.
    ENDLOOP.

    " ------- items + schedule lines from X ----------------------------
    LOOP AT it_vbap INTO DATA(ls_vbap).
      APPEND INITIAL LINE TO lt_itm ASSIGNING FIELD-SYMBOL(<ls_itm>).
      <ls_itm>-itm_number = ls_vbap-posnr.
      <ls_itm>-material   = ls_vbap-matnr.
      <ls_itm>-plant      = ls_vbap-werks.
      <ls_itm>-target_qty = ls_vbap-kwmeng.
      <ls_itm>-target_qu  = ls_vbap-vrkme.
      " 40-char material number (field exists in S/4 BAPI structures)
      FIELD-SYMBOLS <lv_matl> TYPE any.
      ASSIGN COMPONENT 'MATERIAL_LONG' OF STRUCTURE <ls_itm> TO <lv_matl>.
      IF sy-subrc = 0.
        <lv_matl> = ls_vbap-matnr.
      ENDIF.

      APPEND VALUE bapischdl( itm_number = ls_vbap-posnr
                              sched_line = '0001'
                              req_qty    = ls_vbap-kwmeng
                              req_date   = sy-datum ) TO lt_sch.
    ENDLOOP.

    " ------- create Y -------------------------------------------------
    CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2'
      EXPORTING
        order_header_in    = ls_hdr
        logic_switch       = ls_ls
      IMPORTING
        salesdocument      = ev_vbeln_y
      TABLES
        return             = lt_ret
        order_items_in     = lt_itm
        order_partners     = lt_prt
        order_schedules_in = lt_sch.

    IF ev_vbeln_y IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ev_error = |Create failed: { collect_messages( lt_ret ) }|.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    " ------- read the freshly priced result of Y ----------------------
    SELECT SINGLE knumv, waerk, netwr FROM vbak
      WHERE vbeln = @ev_vbeln_y
      INTO @DATA(ls_vbak_y).
    IF sy-subrc <> 0.
      ev_error = |Order Y { ev_vbeln_y } not found after commit|.
      RETURN.
    ENDIF.
    ev_netwr_y = ls_vbak_y-netwr.
    ev_waerk_y = ls_vbak_y-waerk.

    et_cond = get_prcd_conditions( iv_knumv = ls_vbak_y-knumv
                                   iv_waerk = ls_vbak_y-waerk ).

    " item value fields of Y for the ECC-vs-S/4 field comparison
    SELECT posnr, matnr, werks, kwmeng, vrkme, abgru,
           netwr, netpr, sktof, wavwr,
           kzwi1, kzwi2, kzwi3, kzwi4, kzwi5, kzwi6, mwsbp
      FROM vbap
      WHERE vbeln = @ev_vbeln_y
      ORDER BY posnr
      INTO CORRESPONDING FIELDS OF TABLE @et_vbap_y.

  ENDMETHOD.


  METHOD compare_conditions.

    DATA lt_y TYPE ty_t_cond.
    lt_y = it_y.

    LOOP AT it_x INTO DATA(ls_x).

      " ignore inactive and statistical lines
      IF ls_x-kinak IS NOT INITIAL
         OR ls_x-kstat = 'X'
         OR ls_x-kschl IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(ls_res) = VALUE ty_result(
          vbeln_x   = is_vbak-vbeln
          vbeln_y   = iv_vbeln_y
          kunnr     = is_vbak-kunnr
          prsdt     = iv_prsdt
          posnr     = ls_x-posnr
          matnr     = VALUE #( it_vbap[ posnr = ls_x-posnr ]-matnr
                               OPTIONAL )
          kschl     = ls_x-kschl
          rate_old  = ls_x-rate
          waers     = ls_x-waers
          kpein_old = ls_x-kpein
          kmein_old = ls_x-kmein
          kwert_old = ls_x-kwert ).

      " manually entered conditions cannot be re-derived by repricing
      IF ls_x-kherk = 'C' OR ls_x-kmprs = 'X'.
        ms_stat-manual = ms_stat-manual + 1.
        ls_res-status = c_manual.
        ls_res-remark = 'Manual condition on X - not re-priced on Y'(r01).
        add_result( ls_res ).
        CONTINUE.
      ENDIF.

      READ TABLE lt_y ASSIGNING FIELD-SYMBOL(<ls_y>)
           WITH KEY posnr = ls_x-posnr
                    kschl = ls_x-kschl
                    occ   = ls_x-occ.
      IF sy-subrc <> 0.
        ms_stat-missing = ms_stat-missing + 1.
        ls_res-status = c_miss.
        IF ls_x-rate = 0 AND ls_x-kwert = 0.
          " zero-value condition: no impact on the pricing outcome
          ls_res-remark =
            'Not determined in S/4, but zero value on X - no impact'(r07).
        ELSE.
          ls_res-remark =
            'Condition not determined in S/4 - check condition record/access'(r02).
        ENDIF.
        add_result( ls_res ).
        CONTINUE.
      ENDIF.

      <ls_y>-used = abap_true.
      ms_stat-compared = ms_stat-compared + 1.

      ls_res-rate_new   = <ls_y>-rate.
      ls_res-rate_diff  = <ls_y>-rate - ls_x-rate.
      ls_res-kpein_new  = <ls_y>-kpein.
      ls_res-kmein_new  = <ls_y>-kmein.
      ls_res-kwert_new  = <ls_y>-kwert.
      ls_res-kwert_diff = <ls_y>-kwert - ls_x-kwert.

      IF ls_res-rate_diff <> 0
         OR ls_res-kwert_diff <> 0
         OR ls_x-kpein <> <ls_y>-kpein
         OR ls_x-kmein <> <ls_y>-kmein.
        ms_stat-mismatch = ms_stat-mismatch + 1.
        ls_res-status = c_diff.
        IF ls_x-krech = c_percent
           AND ( ls_res-rate_new = ls_res-rate_old * 10
                 OR ls_res-rate_old = ls_res-rate_new * 10 ).
          ls_res-remark = 'Factor-10 delta on % condition - see KBA 2333377'(r03).
        ELSEIF ls_x-kherk = 'D'.
          ls_res-remark = 'Header condition - check header distribution'(r04).
        ELSEIF ls_x-waers <> <ls_y>-waers.
          ls_res-remark = |Currency differs: { ls_x-waers } vs { <ls_y>-waers }|.
        ENDIF.
      ELSE.
        ms_stat-ok = ms_stat-ok + 1.
        ls_res-status = c_ok.
      ENDIF.

      add_result( ls_res ).

    ENDLOOP.

    " conditions that only exist on the re-priced order Y
    LOOP AT lt_y INTO DATA(ls_y) WHERE used = abap_false.
      IF ls_y-kinak IS NOT INITIAL
         OR ls_y-kstat = 'X'
         OR ls_y-kschl IS INITIAL.
        CONTINUE.
      ENDIF.
      ms_stat-new_in_s4 = ms_stat-new_in_s4 + 1.
      add_result( VALUE #(
          vbeln_x   = is_vbak-vbeln
          vbeln_y   = iv_vbeln_y
          kunnr     = is_vbak-kunnr
          prsdt     = iv_prsdt
          posnr     = ls_y-posnr
          matnr     = VALUE #( it_vbap[ posnr = ls_y-posnr ]-matnr
                               OPTIONAL )
          kschl     = ls_y-kschl
          status    = c_new
          rate_new  = ls_y-rate
          waers     = ls_y-waers
          kpein_new = ls_y-kpein
          kmein_new = ls_y-kmein
          kwert_new = ls_y-kwert
          remark    = 'Determined in S/4 only - not present on ECC order'(r05) ) ).
    ENDLOOP.

    " header + item value fields (ECC vs S/4 field comparison)
    compare_fields( is_vbak    = is_vbak
                    iv_vbeln_y = iv_vbeln_y
                    it_vbap    = it_vbap
                    it_vbap_y  = it_vbap_y
                    iv_netwr_y = iv_netwr_y
                    iv_waerk_y = iv_waerk_y
                    iv_prsdt   = iv_prsdt ).

  ENDMETHOD.


  METHOD compare_fields.

    " compare stored value fields of the migrated ECC order X against
    " the freshly priced copy Y (customer requirement):
    "   VBAP: NETWR NETPR SKTOF WAVWR KZWI1..KZWI6 MWSBP
    "   VBAK: NETWR
    " amount fields (type P) are converted with the TCURX decimal
    " shift; other fields (e.g. SKTOF flag) are compared as raw values
    CONSTANTS lc_fields TYPE string
      VALUE 'NETWR NETPR SKTOF WAVWR KZWI1 KZWI2 KZWI3 KZWI4 KZWI5 KZWI6 MWSBP'.

    DATA: lv_type TYPE c LENGTH 1,
          ls_res  TYPE ty_result.
    FIELD-SYMBOLS: <lv_x> TYPE any,
                   <lv_y> TYPE any.

    SPLIT lc_fields AT space INTO TABLE DATA(lt_fields).

    LOOP AT it_vbap INTO DATA(ls_vbap).

      READ TABLE it_vbap_y INTO DATA(ls_vbap_y)
           WITH KEY posnr = ls_vbap-posnr.
      IF sy-subrc <> 0.
        ms_stat-mismatch = ms_stat-mismatch + 1.
        add_result( VALUE #( vbeln_x = is_vbak-vbeln
                             vbeln_y = iv_vbeln_y
                             kunnr   = is_vbak-kunnr
                             prsdt   = iv_prsdt
                             posnr   = ls_vbap-posnr
                             matnr   = ls_vbap-matnr
                             kschl   = 'ITEM'
                             status  = c_diff
                             remark  = 'Item missing on copy order Y'(r08) ) ).
        CONTINUE.
      ENDIF.

      LOOP AT lt_fields INTO DATA(lv_field).
        ASSIGN COMPONENT lv_field OF STRUCTURE ls_vbap   TO <lv_x>.
        CHECK sy-subrc = 0.
        ASSIGN COMPONENT lv_field OF STRUCTURE ls_vbap_y TO <lv_y>.
        CHECK sy-subrc = 0.

        CLEAR ls_res.
        ls_res-vbeln_x = is_vbak-vbeln.
        ls_res-vbeln_y = iv_vbeln_y.
        ls_res-kunnr   = is_vbak-kunnr.
        ls_res-prsdt   = iv_prsdt.
        ls_res-posnr   = ls_vbap-posnr.
        ls_res-matnr   = ls_vbap-matnr.
        ls_res-kschl   = lv_field.
        ls_res-remark  = |Item field VBAP-{ lv_field }|.

        DESCRIBE FIELD <lv_x> TYPE lv_type.
        IF lv_type = 'P'.        " amount field -> external format
          ls_res-waers     = is_vbak-waerk.
          ls_res-rate_old  = to_external( iv_amount = CONV #( <lv_x> )
                                          iv_waers  = is_vbak-waerk ).
          ls_res-rate_new  = to_external( iv_amount = CONV #( <lv_y> )
                                          iv_waers  = iv_waerk_y ).
          ls_res-rate_diff = ls_res-rate_new - ls_res-rate_old.
          IF ls_res-rate_diff <> 0.
            ls_res-status = c_diff.
          ELSE.
            ls_res-status = c_ok.
          ENDIF.
        ELSE.                    " flag / character field -> raw compare
          IF <lv_x> <> <lv_y>.
            ls_res-status = c_diff.
            ls_res-remark = |VBAP-{ lv_field }: X = '{ <lv_x> }', | &
                            |Y = '{ <lv_y> }'|.
          ELSE.
            ls_res-status = c_ok.
            ls_res-remark = |VBAP-{ lv_field }: '{ <lv_x> }' (identical)|.
          ENDIF.
        ENDIF.

        IF ls_res-status = c_diff.
          ms_stat-mismatch = ms_stat-mismatch + 1.
        ELSE.
          ms_stat-ok = ms_stat-ok + 1.
        ENDIF.
        add_result( ls_res ).
      ENDLOOP.

    ENDLOOP.

    " ------- header net value VBAK-NETWR ------------------------------
    CLEAR ls_res.
    ls_res-vbeln_x  = is_vbak-vbeln.
    ls_res-vbeln_y  = iv_vbeln_y.
    ls_res-kunnr    = is_vbak-kunnr.
    ls_res-prsdt    = iv_prsdt.
    ls_res-posnr    = '000000'.
    ls_res-kschl    = 'VBAK-NETWR'.
    ls_res-waers    = is_vbak-waerk.
    ls_res-remark   = 'Header net value VBAK-NETWR'(r09).
    ls_res-rate_old = to_external( iv_amount = CONV #( is_vbak-netwr )
                                   iv_waers  = is_vbak-waerk ).
    ls_res-rate_new = to_external( iv_amount = CONV #( iv_netwr_y )
                                   iv_waers  = iv_waerk_y ).
    ls_res-rate_diff = ls_res-rate_new - ls_res-rate_old.
    IF ls_res-rate_diff <> 0.
      ms_stat-mismatch = ms_stat-mismatch + 1.
      ls_res-status = c_diff.
    ELSE.
      ms_stat-ok = ms_stat-ok + 1.
      ls_res-status = c_ok.
    ENDIF.
    IF is_vbak-waerk <> iv_waerk_y.
      ls_res-remark = |Currency differs: X { is_vbak-waerk } vs | &
                      |Y { iv_waerk_y }|.
    ENDIF.
    add_result( ls_res ).

  ENDMETHOD.


  METHOD set_occurrence.

    " occurrence index: n-th appearance of a condition type within an
    " item, in pricing procedure order - used as matching key so that
    " condition types appearing twice are compared pairwise
    DATA: lv_posnr TYPE posnr_va,
          lv_kschl TYPE kscha,
          lv_occ   TYPE i.

    LOOP AT ct_cond ASSIGNING FIELD-SYMBOL(<ls_cond>).
      IF <ls_cond>-posnr <> lv_posnr.
        CLEAR: lv_kschl, lv_occ.
        lv_posnr = <ls_cond>-posnr.
      ENDIF.
      IF <ls_cond>-kschl = lv_kschl.
        lv_occ = lv_occ + 1.
      ELSE.
        lv_occ = 1.
        lv_kschl = <ls_cond>-kschl.
      ENDIF.
      <ls_cond>-occ = lv_occ.
    ENDLOOP.

  ENDMETHOD.


  METHOD to_external.

    " percentage conditions: KBETR is stored with one implied extra
    " decimal (KBETR 100.00 = 10.000 %)
    IF iv_krech = c_percent.
      rv_ext = iv_amount / 10.
      RETURN.
    ENDIF.

    " currency decimal shift: internal CURR fields always carry two
    " decimals; TCURX defines the real number of decimals per currency
    " (JPY = 0 -> real amount = stored amount * 100)
    READ TABLE mt_tcurx INTO DATA(ls_tcurx)
         WITH TABLE KEY currkey = iv_waers.
    IF sy-subrc <> 0.
      SELECT SINGLE * FROM tcurx
        WHERE currkey = @iv_waers
        INTO @ls_tcurx.
      IF sy-subrc <> 0.
        ls_tcurx-currkey = iv_waers.
        ls_tcurx-currdec = 2.
      ENDIF.
      INSERT ls_tcurx INTO TABLE mt_tcurx.
    ENDIF.

    DATA(lv_dec) = CONV i( ls_tcurx-currdec ).
    rv_ext = iv_amount * ( 10 ** ( 2 - lv_dec ) ).

  ENDMETHOD.


  METHOD collect_messages.
    LOOP AT it_return INTO DATA(ls_ret) WHERE type CA 'EAX'.
      IF rv_msg IS NOT INITIAL.
        rv_msg = |{ rv_msg }; |.
      ENDIF.
      rv_msg = |{ rv_msg }{ ls_ret-message }|.
    ENDLOOP.
    IF rv_msg IS INITIAL AND it_return IS NOT INITIAL.
      rv_msg = VALUE #( it_return[ 1 ]-message OPTIONAL ).
    ENDIF.
  ENDMETHOD.


  METHOD add_result.

    DATA(ls_result) = is_result.

    " red only when the pricing outcome is affected:
    "  - MISMATCH / ERROR
    "  - condition with a real value missing in S/4
    "  - new S/4 condition carrying a real value
    " zero-value missing/new conditions are warnings (yellow) only
    CASE ls_result-status.
      WHEN c_diff OR c_error.
        ms_stat-crit = ms_stat-crit + 1.
        ls_result-color = VALUE #( ( fname = 'STATUS'
                                     color-col = col_negative
                                     color-int = 1 ) ).
      WHEN c_miss.
        IF ls_result-rate_old = 0 AND ls_result-kwert_old = 0.
          ms_stat-warn = ms_stat-warn + 1.
          ls_result-color = VALUE #( ( fname = 'STATUS'
                                       color-col = col_total ) ).
        ELSE.
          ms_stat-crit = ms_stat-crit + 1.
          ls_result-color = VALUE #( ( fname = 'STATUS'
                                       color-col = col_negative
                                       color-int = 1 ) ).
        ENDIF.
      WHEN c_new.
        IF ls_result-rate_new = 0 AND ls_result-kwert_new = 0.
          ms_stat-warn = ms_stat-warn + 1.
          ls_result-color = VALUE #( ( fname = 'STATUS'
                                       color-col = col_total ) ).
        ELSE.
          ms_stat-crit = ms_stat-crit + 1.
          ls_result-color = VALUE #( ( fname = 'STATUS'
                                       color-col = col_negative
                                       color-int = 1 ) ).
        ENDIF.
      WHEN c_manual.
        ms_stat-warn = ms_stat-warn + 1.
        ls_result-color = VALUE #( ( fname = 'STATUS'
                                     color-col = col_total ) ).
      WHEN c_ok.
        ls_result-color = VALUE #( ( fname = 'STATUS'
                                     color-col = col_positive ) ).
    ENDCASE.

    APPEND ls_result TO mt_result.

  ENDMETHOD.


  METHOD display.

    " screen 1: one row per order with the overall verdict -
    " double-click a row to open the pricing detail (screen 2)
    DATA lo_alv TYPE REF TO cl_salv_table.

    SORT mt_summary BY kunnr vbeln_x.
    SORT mt_result  BY kunnr vbeln_x posnr kschl.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING  t_table      = mt_summary ).

        lo_alv->get_functions( )->set_all( ).
        lo_alv->get_columns( )->set_optimize( ).
        lo_alv->get_columns( )->set_color_column( 'COLOR' ).
        lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
        lo_alv->get_layout( )->set_key(
          VALUE salv_s_layout_key( report = sy-repid ) ).
        lo_alv->get_layout( )->set_save_restriction(
          if_salv_c_layout=>restrict_none ).

        DATA(lo_cols) = lo_alv->get_columns( ).
        TRY.
            DATA(lo_col) = lo_cols->get_column( 'VBELN_X' ).
            lo_col->set_short_text( 'SO Old' ).
            lo_col->set_medium_text( 'Sales Order Old' ).
            lo_col->set_long_text( 'Sales Order Old (ECC)' ).
            lo_col = lo_cols->get_column( 'VBELN_Y' ).
            lo_col->set_short_text( 'SO New' ).
            lo_col->set_medium_text( 'Sales Order New' ).
            lo_col->set_long_text( 'Sales Order New (S/4)' ).
            lo_cols->get_column( 'ITEMS' )->set_medium_text( 'Items' ).
            lo_cols->get_column( 'NETWR_X' )->set_medium_text( 'Net Value X (ECC)' ).
            lo_cols->get_column( 'NETWR_Y' )->set_medium_text( 'Net Value Y (S/4)' ).
            lo_cols->get_column( 'NETWR_DIFF' )->set_medium_text( 'Net delta' ).
            lo_cols->get_column( 'CHECKS' )->set_medium_text( 'Checks' ).
            lo_cols->get_column( 'OK' )->set_medium_text( 'OK' ).
            lo_cols->get_column( 'CRIT' )->set_medium_text( 'Differences' ).
            lo_cols->get_column( 'WARN' )->set_medium_text( 'Warnings' ).
            lo_cols->get_column( 'STATUS' )->set_medium_text( 'Result' ).
            lo_cols->get_column( 'REMARK' )->set_medium_text( 'Remark' ).
          CATCH cx_salv_not_found.
        ENDTRY.

        " overall verdict header
        DATA(lo_grid) = NEW cl_salv_form_layout_grid( ).
        lo_grid->create_label( row = 1 column = 1
          text = 'Pricing validation ECC -> S/4HANA (copy order, original pricing date)' ).
        lo_grid->create_label( row = 2 column = 1
          text = COND string(
            WHEN ms_stat-crit = 0 AND ms_stat-errors = 0
            THEN |RESULT: ALL OK - S/4HANA reproduces the ECC pricing|
            ELSE |RESULT: { ms_stat-crit } difference(s), | &
                 |{ ms_stat-errors } error(s) - check red rows| ) ).
        lo_grid->create_flow( row = 3 column = 1 )->create_text(
          text = mv_info ).
        lo_grid->create_flow( row = 4 column = 1 )->create_text(
          text = |Orders: { ms_stat-orders }  | &
                 |Checks OK: { ms_stat-ok }  | &
                 |Differences: { ms_stat-crit }  | &
                 |Warnings: { ms_stat-warn }  | &
                 |Errors: { ms_stat-errors }| ).
        lo_grid->create_flow( row = 5 column = 1 )->create_text(
          text = 'Double-click an order row to see the pricing comparison in detail' ).
        lo_alv->set_top_of_list( lo_grid ).

        " double-click -> pricing detail of the selected order
        SET HANDLER on_double_click FOR lo_alv->get_event( ).

        lo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv).
        MESSAGE lx_salv->get_text( ) TYPE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD on_double_click.

    READ TABLE mt_summary INTO DATA(ls_sum) INDEX row.
    IF sy-subrc = 0.
      display_detail( ls_sum-vbeln_x ).
    ENDIF.

  ENDMETHOD.


  METHOD on_detail_click.

    " third drill level: jump into the condition record maintenance
    " (VK13) for the double-clicked condition type - the record valid
    " on the order's pricing date (PRSDT column) is the one to check
    READ TABLE mt_detail INTO DATA(ls_det) INDEX row.
    IF sy-subrc <> 0 OR ls_det-kschl IS INITIAL.
      RETURN.
    ENDIF.

    " only real pricing condition types can be displayed in VK13 -
    " field rows (NETPR, KZWI1, VBAK-NETWR, ...) have no record
    DATA lv_kschl TYPE kscha.
    lv_kschl = ls_det-kschl.
    SELECT SINGLE kschl FROM t685
      WHERE kvewe = 'A'
        AND kappl = 'V'
        AND kschl = @lv_kschl
      INTO @lv_kschl.
    IF sy-subrc <> 0.
      MESSAGE |{ ls_det-kschl } is a value field - no condition record (VK13) behind it|
        TYPE 'S'.
      RETURN.
    ENDIF.

    SET PARAMETER ID 'VKS' FIELD lv_kschl.
    TRY.
        CALL TRANSACTION 'VK13' WITH AUTHORITY-CHECK.
      CATCH cx_sy_authorization_error.
        MESSAGE 'No authorization for transaction VK13'(m07)
          TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD display_detail.

    " screen 2: condition-by-condition + field-by-field comparison of
    " one order, shown as a popup over the overview ALV
    DATA lo_alv TYPE REF TO cl_salv_table.

    CLEAR mt_detail.
    LOOP AT mt_result INTO DATA(ls_result)
         WHERE vbeln_x = iv_vbeln_x.
      APPEND ls_result TO mt_detail.
    ENDLOOP.
    IF mt_detail IS INITIAL.
      MESSAGE 'No detail rows for this order'(m06) TYPE 'S'.
      RETURN.
    ENDIF.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING  t_table      = mt_detail ).

        lo_alv->get_functions( )->set_all( ).
        lo_alv->get_columns( )->set_optimize( ).
        lo_alv->get_columns( )->set_color_column( 'COLOR' ).
        lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).

        DATA(lo_cols) = lo_alv->get_columns( ).
        TRY.
            DATA(lo_col) = lo_cols->get_column( 'VBELN_X' ).
            lo_col->set_short_text( 'SO Old' ).
            lo_col->set_medium_text( 'Sales Order Old' ).
            lo_col->set_long_text( 'Sales Order Old (ECC)' ).
            lo_col = lo_cols->get_column( 'VBELN_Y' ).
            lo_col->set_short_text( 'SO New' ).
            lo_col->set_medium_text( 'Sales Order New' ).
            lo_col->set_long_text( 'Sales Order New (S/4)' ).
            lo_col = lo_cols->get_column( 'KSCHL' ).
            lo_col->set_short_text( 'CTyp/Field' ).
            lo_col->set_medium_text( 'Cond.Type / Field' ).
            lo_col->set_long_text( 'Condition Type / Compared Field' ).
            lo_cols->get_column( 'STATUS' )->set_medium_text( 'Status' ).
            lo_cols->get_column( 'RATE_OLD' )->set_medium_text( 'Rate/Amt X (ECC)' ).
            lo_cols->get_column( 'RATE_NEW' )->set_medium_text( 'Rate/Amt Y (S/4)' ).
            lo_cols->get_column( 'RATE_DIFF' )->set_medium_text( 'Delta' ).
            lo_cols->get_column( 'KPEIN_OLD' )->set_medium_text( 'Per X' ).
            lo_cols->get_column( 'KPEIN_NEW' )->set_medium_text( 'Per Y' ).
            lo_cols->get_column( 'KMEIN_OLD' )->set_medium_text( 'UoM X' ).
            lo_cols->get_column( 'KMEIN_NEW' )->set_medium_text( 'UoM Y' ).
            lo_cols->get_column( 'KWERT_OLD' )->set_medium_text( 'Value X (ECC)' ).
            lo_cols->get_column( 'KWERT_NEW' )->set_medium_text( 'Value Y (S/4)' ).
            lo_cols->get_column( 'KWERT_DIFF' )->set_medium_text( 'Value delta' ).
            lo_cols->get_column( 'PRSDT' )->set_medium_text( 'Pricing date' ).
            lo_cols->get_column( 'REMARK' )->set_medium_text( 'Remark' ).
          CATCH cx_salv_not_found.
        ENDTRY.

        " double-click a condition row -> VK13 for that condition type
        SET HANDLER on_detail_click FOR lo_alv->get_event( ).

        lo_alv->set_screen_popup( start_column = 5
                                  end_column   = 170
                                  start_line   = 2
                                  end_line     = 27 ).
        lo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv).
        MESSAGE lx_salv->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
START-OF-SELECTION.
  NEW lcl_app( )->run( ).
