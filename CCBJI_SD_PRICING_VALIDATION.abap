*&---------------------------------------------------------------------*
*& Report  /CCBJI/SD_PRICING_VALIDATION
*&---------------------------------------------------------------------*
*& CCBJI - Pricing regression validation after ECC -> S/4HANA migration
*&
*& PURPOSE
*& -------
*& Sales orders migrated from ECC carry the pricing result calculated
*& by the ECC pricing engine (now stored in PRCD_ELEMENTS after the
*& KONV -> PRCD_ELEMENTS conversion). To prove that the S/4HANA pricing
*& configuration (pricing procedure, condition records, VOFM routines)
*& reproduces the ECC result, this program takes each sales order X of
*& the selected document type and lets S/4HANA re-derive the pricing
*& from scratch on a copy order Y:
*&
*&   Mode 1 (default) - CREATE:   Y is really created with
*&            BAPI_SALESORDER_CREATEFROMDAT2, LOGIC_SWITCH-PRICING = 'B'
*&            (carry out new pricing). Y's conditions are read back from
*&            PRCD_ELEMENTS and compared with X's conditions. Afterwards
*&            all items of Y are rejected (reason P_ABGRU) via
*&            BAPI_SALESORDER_CHANGE, unless P_NOREJ is set.
*&            Y's PO number field is stamped 'PRCVAL-<X>' for tracing.
*&
*&   Mode 2 - SIMULATE:  BAPI_SALESORDER_SIMULATE re-prices without
*&            saving anything (no number range consumption, no ATP /
*&            credit / output side effects). Y's conditions come from
*&            ORDER_CONDITION_EX. Recommended for mass runs.
*&
*& COMPARISON
*& ----------
*& Condition lines are matched per item + condition type + occurrence
*& and compared on rate (KBETR), pricing unit (KPEIN), condition unit
*& (KMEIN) and - in create mode - condition value (KWERT). One extra
*& row per item compares the item net value (NETWR).
*&
*& Amounts are normalised to external format before comparison, i.e.
*& the TCURX decimal shift is applied (JPY has 0 decimals -> internal
*& value * 100) and percentage rates (KRECH = 'A') are divided by 10.
*& BAPI output amounts are already external (cf. SAP KBA 2333377).
*&
*& Classification:
*&   OK          rate/value identical within tolerance P_TOL
*&   MISMATCH    values differ -> S/4 pricing deviates from ECC result
*&   MISSING_S4  condition exists on X but was not re-determined on Y
*&               (missing/wrong condition record or access sequence)
*&   NEW_IN_S4   condition determined on Y but absent on X
*&   MANUAL      manually entered condition on X (KHERK 'C' / KMPRS) -
*&               not re-derivable by repricing, reported for info only
*&   ERROR       order could not be copied/simulated (BAPI messages)
*&
*& Inactive condition lines (KINAK <> space) are ignored. Statistical
*& lines (KSTAT = 'X') are ignored unless P_STAT is set. Fully rejected
*& items of X (ABGRU <> space) are skipped.
*&
*& TEXT ELEMENTS (maintain in SE38 -> Goto -> Text elements)
*&   TEXT-001  Sales order selection (source order X)
*&   TEXT-002  Processing mode (copy order Y)
*&   TEXT-003  Pricing date for order Y
*&   TEXT-004  Comparison settings
*&---------------------------------------------------------------------*
REPORT /ccbji/sd_pricing_validation.

TABLES: vbak.

DATA gv_kschl TYPE kscha.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_auart FOR vbak-auart OBLIGATORY,
                  s_vkorg FOR vbak-vkorg,
                  s_vtweg FOR vbak-vtweg,
                  s_spart FOR vbak-spart,
                  s_vbeln FOR vbak-vbeln,
                  s_erdat FOR vbak-erdat.
  PARAMETERS p_maxdoc TYPE i DEFAULT 100.        " max. no. of orders
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_crt RADIOBUTTON GROUP md DEFAULT 'X',  " create Y + reject
              p_sim RADIOBUTTON GROUP md.              " simulate only
  PARAMETERS: p_abgru TYPE abgru_va,             " rejection reason for Y
              p_norej AS CHECKBOX.               " keep Y (do not reject)
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_dtold RADIOBUTTON GROUP dt DEFAULT 'X', " X's pricing date
              p_dttod RADIOBUTTON GROUP dt.             " today's date
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-004.
  SELECT-OPTIONS s_kschl FOR gv_kschl.           " restrict condition types
  PARAMETERS: p_tol    TYPE p LENGTH 15 DECIMALS 3 DEFAULT 0, " tolerance
              p_stat   AS CHECKBOX,              " include statistical
              p_onlyer AS CHECKBOX DEFAULT 'X'.  " show differences only
SELECTION-SCREEN END OF BLOCK b4.

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
      END OF ty_vbak,

      BEGIN OF ty_vbap,
        posnr  TYPE vbap-posnr,
        matnr  TYPE vbap-matnr,
        werks  TYPE vbap-werks,
        kwmeng TYPE vbap-kwmeng,
        vrkme  TYPE vbap-vrkme,
        netwr  TYPE vbap-netwr,
        abgru  TYPE vbap-abgru,
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
        kwert_valid TYPE abap_bool,   " KWERT filled (create mode only)
        kstat  TYPE prcd_elements-kstat,
        kinak  TYPE prcd_elements-kinak,
        kherk  TYPE prcd_elements-kherk,
        kmprs  TYPE prcd_elements-kmprs,
        used   TYPE abap_bool,
      END OF ty_cond,
      ty_t_cond TYPE STANDARD TABLE OF ty_cond WITH DEFAULT KEY,

      BEGIN OF ty_net,
        posnr TYPE posnr_va,
        netwr TYPE ty_amount,          " external format
      END OF ty_net,
      ty_t_net TYPE STANDARD TABLE OF ty_net WITH DEFAULT KEY,

      BEGIN OF ty_result,
        vbeln_x    TYPE vbeln_va,
        vbeln_y    TYPE vbeln_va,
        posnr      TYPE posnr_va,
        matnr      TYPE vbap-matnr,
        kschl      TYPE kscha,
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
        remark     TYPE c LENGTH 100,
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
      END OF ty_stat.

    CONSTANTS:
      c_ok      TYPE c LENGTH 10 VALUE 'OK',
      c_diff    TYPE c LENGTH 10 VALUE 'MISMATCH',
      c_miss    TYPE c LENGTH 10 VALUE 'MISSING_S4',
      c_new     TYPE c LENGTH 10 VALUE 'NEW_IN_S4',
      c_manual  TYPE c LENGTH 10 VALUE 'MANUAL',
      c_error   TYPE c LENGTH 10 VALUE 'ERROR',
      c_netrow  TYPE kscha       VALUE '*NET',
      c_percent TYPE c LENGTH 1  VALUE 'A'.

    DATA: mt_result TYPE STANDARD TABLE OF ty_result,
          ms_stat   TYPE ty_stat,
          mt_tcurx  TYPE HASHED TABLE OF tcurx
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
                et_net     TYPE ty_t_net
                ev_error   TYPE string.

    METHODS reject_order_y
      IMPORTING iv_vbeln_y TYPE vbeln_va
                it_vbap    TYPE ty_t_vbap
      RETURNING VALUE(rv_msg) TYPE string.

    METHODS simulate_order_y
      IMPORTING is_vbak  TYPE ty_vbak
                it_vbap  TYPE ty_t_vbap
                iv_prsdt TYPE prsdt
      EXPORTING et_cond  TYPE ty_t_cond
                et_net   TYPE ty_t_net
                ev_error TYPE string.

    METHODS compare_conditions
      IMPORTING is_vbak    TYPE ty_vbak
                iv_vbeln_y TYPE vbeln_va
                it_vbap    TYPE ty_t_vbap
                it_x       TYPE ty_t_cond
                it_y       TYPE ty_t_cond
                it_net_y   TYPE ty_t_net.

    METHODS set_occurrence
      CHANGING ct_cond TYPE ty_t_cond.

    METHODS to_external
      IMPORTING iv_amount     TYPE ty_amount
                iv_waers      TYPE waers
                iv_krech      TYPE clike OPTIONAL
      RETURNING VALUE(rv_ext) TYPE ty_amount.

    METHODS get_num_component
      IMPORTING is_struc      TYPE any
                iv_name       TYPE string
      RETURNING VALUE(rv_val) TYPE ty_amount.

    METHODS get_chr_component
      IMPORTING is_struc      TYPE any
                iv_name       TYPE string
      RETURNING VALUE(rv_val) TYPE string.

    METHODS collect_messages
      IMPORTING it_return     TYPE bapiret2_t
      RETURNING VALUE(rv_msg) TYPE string.

    METHODS add_result
      IMPORTING is_result TYPE ty_result.

    METHODS display.

ENDCLASS.


CLASS lcl_app IMPLEMENTATION.

  METHOD run.

    " create mode needs a rejection reason (unless Y is kept on purpose)
    IF p_crt = abap_true AND p_abgru IS INITIAL AND p_norej IS INITIAL.
      MESSAGE 'Enter a rejection reason for order Y or tick "keep Y"'(m01)
        TYPE 'E'.
    ENDIF.

    SELECT vbeln, auart, vkorg, vtweg, spart, knumv, waerk, erdat
      FROM vbak
      WHERE auart IN @s_auart
        AND vkorg IN @s_vkorg
        AND vtweg IN @s_vtweg
        AND spart IN @s_spart
        AND vbeln IN @s_vbeln
        AND erdat IN @s_erdat
      ORDER BY vbeln
      INTO TABLE @DATA(lt_vbak)
      UP TO @p_maxdoc ROWS.

    IF lt_vbak IS INITIAL.
      MESSAGE 'No sales orders found for the selection'(m02) TYPE 'S'
        DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    LOOP AT lt_vbak INTO DATA(ls_vbak).
      ms_stat-orders = ms_stat-orders + 1.
      process_order( CORRESPONDING #( ls_vbak ) ).
    ENDLOOP.

    display( ).

  ENDMETHOD.


  METHOD process_order.

    DATA: lt_cond_y  TYPE ty_t_cond,
          lt_net_y   TYPE ty_t_net,
          lv_vbeln_y TYPE vbeln_va,
          lv_error   TYPE string.

    " ------- items of X (skip fully rejected items) -------------------
    SELECT posnr, matnr, werks, kwmeng, vrkme, netwr, abgru
      FROM vbap
      WHERE vbeln = @is_vbak-vbeln
      ORDER BY posnr
      INTO TABLE @DATA(lt_vbap).

    DELETE lt_vbap WHERE abgru IS NOT INITIAL.
    IF lt_vbap IS INITIAL.
      RETURN.
    ENDIF.

    " ------- pricing date of X (VBKD header record) -------------------
    DATA lv_prsdt TYPE prsdt.
    IF p_dtold = abap_true.
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
    ELSE.
      lv_prsdt = sy-datum.
    ENDIF.

    " ------- original (migrated ECC) pricing result of X --------------
    DATA(lt_cond_x) = get_prcd_conditions( iv_knumv = is_vbak-knumv
                                           iv_waerk = is_vbak-waerk ).

    " ------- re-derive pricing on copy order Y ------------------------
    IF p_crt = abap_true.
      create_order_y( EXPORTING is_vbak    = is_vbak
                                it_vbap    = lt_vbap
                                iv_prsdt   = lv_prsdt
                      IMPORTING ev_vbeln_y = lv_vbeln_y
                                et_cond    = lt_cond_y
                                et_net     = lt_net_y
                                ev_error   = lv_error ).
    ELSE.
      simulate_order_y( EXPORTING is_vbak  = is_vbak
                                  it_vbap  = lt_vbap
                                  iv_prsdt = lv_prsdt
                        IMPORTING et_cond  = lt_cond_y
                                  et_net   = lt_net_y
                                  ev_error = lv_error ).
    ENDIF.

    IF lv_error IS NOT INITIAL.
      ms_stat-errors = ms_stat-errors + 1.
      add_result( VALUE #( vbeln_x = is_vbak-vbeln
                           vbeln_y = lv_vbeln_y
                           kschl   = space
                           status  = c_error
                           remark  = lv_error ) ).
      RETURN.
    ENDIF.

    " ------- compare X vs Y -------------------------------------------
    compare_conditions( is_vbak    = is_vbak
                        iv_vbeln_y = lv_vbeln_y
                        it_vbap    = lt_vbap
                        it_x       = lt_cond_x
                        it_y       = lt_cond_y
                        it_net_y   = lt_net_y ).

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
          kwert_valid = abap_true
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

    CLEAR: ev_vbeln_y, et_cond, et_net, ev_error.

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
    SELECT SINGLE knumv, waerk FROM vbak
      WHERE vbeln = @ev_vbeln_y
      INTO @DATA(ls_vbak_y).
    IF sy-subrc <> 0.
      ev_error = |Order Y { ev_vbeln_y } not found after commit|.
      RETURN.
    ENDIF.

    et_cond = get_prcd_conditions( iv_knumv = ls_vbak_y-knumv
                                   iv_waerk = ls_vbak_y-waerk ).

    SELECT posnr, netwr FROM vbap
      WHERE vbeln = @ev_vbeln_y
      ORDER BY posnr
      INTO TABLE @DATA(lt_vbap_y).
    LOOP AT lt_vbap_y INTO DATA(ls_vbap_y).
      APPEND VALUE ty_net(
          posnr = ls_vbap_y-posnr
          netwr = to_external( iv_amount = CONV #( ls_vbap_y-netwr )
                               iv_waers  = ls_vbak_y-waerk ) ) TO et_net.
    ENDLOOP.

    " ------- reject Y so it does not stay open ------------------------
    IF p_norej IS INITIAL.
      DATA(lv_rejmsg) = reject_order_y( iv_vbeln_y = ev_vbeln_y
                                        it_vbap    = it_vbap ).
      IF lv_rejmsg IS NOT INITIAL.
        add_result( VALUE #( vbeln_x = is_vbak-vbeln
                             vbeln_y = ev_vbeln_y
                             status  = c_error
                             remark  = |Y NOT rejected - clean up manually: | &
                                       |{ lv_rejmsg }| ) ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD reject_order_y.

    DATA: ls_hdx    TYPE bapisdh1x,
          lt_itm    TYPE STANDARD TABLE OF bapisditm,
          lt_itmx   TYPE STANDARD TABLE OF bapisditmx,
          lt_ret    TYPE bapiret2_t.

    ls_hdx-updateflag = 'U'.

    LOOP AT it_vbap INTO DATA(ls_vbap).
      APPEND VALUE bapisditm( itm_number = ls_vbap-posnr
                              reason_rej = p_abgru ) TO lt_itm.
      APPEND VALUE bapisditmx( itm_number = ls_vbap-posnr
                               updateflag = 'U'
                               reason_rej = 'X' ) TO lt_itmx.
    ENDLOOP.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
      EXPORTING
        salesdocument    = iv_vbeln_y
        order_header_inx = ls_hdx
      TABLES
        return           = lt_ret
        order_item_in    = lt_itm
        order_item_inx   = lt_itmx.

    LOOP AT lt_ret TRANSPORTING NO FIELDS WHERE type CA 'EA'.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      rv_msg = collect_messages( lt_ret ).
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

  ENDMETHOD.


  METHOD simulate_order_y.

    DATA: ls_hdr     TYPE bapisdhead,
          lt_itm     TYPE STANDARD TABLE OF bapiitemin,
          lt_prt     TYPE STANDARD TABLE OF bapipartnr,
          lt_itm_out TYPE STANDARD TABLE OF bapiitemex,
          lt_cond_ex TYPE STANDARD TABLE OF bapicond,
          lt_msg     TYPE STANDARD TABLE OF bapiret2.

    CLEAR: et_cond, et_net, ev_error.

    ls_hdr-doc_type   = is_vbak-auart.
    ls_hdr-sales_org  = is_vbak-vkorg.
    ls_hdr-distr_chan = is_vbak-vtweg.
    ls_hdr-division   = is_vbak-spart.
    ls_hdr-price_date = iv_prsdt.
    ls_hdr-purch_no   = |PRCVAL-{ is_vbak-vbeln }|.

    SELECT parvw, kunnr FROM vbpa
      WHERE vbeln = @is_vbak-vbeln
        AND posnr = '000000'
        AND parvw IN ('AG','WE')
      INTO TABLE @DATA(lt_vbpa).
    LOOP AT lt_vbpa INTO DATA(ls_vbpa).
      APPEND VALUE bapipartnr( partn_role = ls_vbpa-parvw
                               partn_numb = ls_vbpa-kunnr ) TO lt_prt.
    ENDLOOP.

    LOOP AT it_vbap INTO DATA(ls_vbap).
      APPEND INITIAL LINE TO lt_itm ASSIGNING FIELD-SYMBOL(<ls_itm>).
      <ls_itm>-itm_number = ls_vbap-posnr.
      <ls_itm>-material   = ls_vbap-matnr.
      <ls_itm>-plant      = ls_vbap-werks.
      <ls_itm>-req_qty    = ls_vbap-kwmeng.
      FIELD-SYMBOLS <lv_matl> TYPE any.
      ASSIGN COMPONENT 'MATERIAL_LONG' OF STRUCTURE <ls_itm> TO <lv_matl>.
      IF sy-subrc = 0.
        <lv_matl> = ls_vbap-matnr.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'BAPI_SALESORDER_SIMULATE'
      EXPORTING
        order_header_in    = ls_hdr
      TABLES
        order_items_in     = lt_itm
        order_partners     = lt_prt
        order_items_out    = lt_itm_out
        order_condition_ex = lt_cond_ex
        messagetable       = lt_msg.

    LOOP AT lt_msg TRANSPORTING NO FIELDS WHERE type CA 'EA'.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      ev_error = |Simulation failed: { collect_messages( CONV #( lt_msg ) ) }|.
      RETURN.
    ENDIF.

    " BAPI output is already in external format (KBA 2333377:
    " percentage rates are returned divided by 10, i.e. as true percent)
    SORT lt_cond_ex BY itm_number cond_st_no cond_count.
    LOOP AT lt_cond_ex INTO DATA(ls_cond).
      APPEND VALUE ty_cond(
          posnr = ls_cond-itm_number
          kschl = ls_cond-cond_type
          rate  = ls_cond-cond_value
          waers = ls_cond-currency
          kpein = ls_cond-cond_p_unt
          kmein = ls_cond-cond_unit
          kwert_valid = abap_false
          kinak = get_chr_component( is_struc = ls_cond
                                     iv_name  = 'COND_INACT' )
          kstat = get_chr_component( is_struc = ls_cond
                                     iv_name  = 'COND_ISSTA' ) ) TO et_cond.
    ENDLOOP.

    set_occurrence( CHANGING ct_cond = et_cond ).

    " item net value (field name differs between releases -> dynamic)
    LOOP AT lt_itm_out INTO DATA(ls_itm_out).
      DATA(lv_net) = get_num_component( is_struc = ls_itm_out
                                        iv_name  = 'NET_VALUE1' ).
      IF lv_net IS INITIAL.
        lv_net = get_num_component( is_struc = ls_itm_out
                                    iv_name  = 'NET_VALUE' ).
      ENDIF.
      IF lv_net IS NOT INITIAL.
        APPEND VALUE ty_net( posnr = ls_itm_out-itm_number
                             netwr = lv_net ) TO et_net.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD compare_conditions.

    DATA lt_y TYPE ty_t_cond.
    lt_y = it_y.

    LOOP AT it_x INTO DATA(ls_x).

      " ignore inactive lines and (optionally) statistical lines
      IF ls_x-kinak IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      IF ls_x-kstat = 'X' AND p_stat IS INITIAL.
        CONTINUE.
      ENDIF.
      IF ls_x-kschl NOT IN s_kschl OR ls_x-kschl IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(ls_res) = VALUE ty_result(
          vbeln_x   = is_vbak-vbeln
          vbeln_y   = iv_vbeln_y
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
        ls_res-remark =
          'Condition not determined in S/4 - check condition record/access'(r02).
        add_result( ls_res ).
        CONTINUE.
      ENDIF.

      <ls_y>-used = abap_true.
      ms_stat-compared = ms_stat-compared + 1.

      ls_res-rate_new  = <ls_y>-rate.
      ls_res-rate_diff = <ls_y>-rate - ls_x-rate.
      ls_res-kpein_new = <ls_y>-kpein.
      ls_res-kmein_new = <ls_y>-kmein.
      IF <ls_y>-kwert_valid = abap_true.
        ls_res-kwert_new  = <ls_y>-kwert.
        ls_res-kwert_diff = <ls_y>-kwert - ls_x-kwert.
      ENDIF.

      IF abs( ls_res-rate_diff ) > p_tol
         OR ( <ls_y>-kwert_valid = abap_true
              AND abs( ls_res-kwert_diff ) > p_tol )
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
      IF ls_y-kinak IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      IF ls_y-kstat = 'X' AND p_stat IS INITIAL.
        CONTINUE.
      ENDIF.
      IF ls_y-kschl NOT IN s_kschl OR ls_y-kschl IS INITIAL.
        CONTINUE.
      ENDIF.
      ms_stat-new_in_s4 = ms_stat-new_in_s4 + 1.
      add_result( VALUE #(
          vbeln_x   = is_vbak-vbeln
          vbeln_y   = iv_vbeln_y
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

    " item net value comparison
    LOOP AT it_vbap INTO DATA(ls_vbap).
      READ TABLE it_net_y INTO DATA(ls_net_y)
           WITH KEY posnr = ls_vbap-posnr.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      DATA(lv_net_old) = to_external( iv_amount = CONV #( ls_vbap-netwr )
                                      iv_waers  = is_vbak-waerk ).
      DATA(ls_netres) = VALUE ty_result(
          vbeln_x   = is_vbak-vbeln
          vbeln_y   = iv_vbeln_y
          posnr     = ls_vbap-posnr
          matnr     = ls_vbap-matnr
          kschl     = c_netrow
          waers     = is_vbak-waerk
          rate_old  = lv_net_old
          rate_new  = ls_net_y-netwr
          rate_diff = ls_net_y-netwr - lv_net_old
          remark    = 'Item net value (NETWR)'(r06) ).
      IF abs( ls_netres-rate_diff ) > p_tol.
        ms_stat-mismatch = ms_stat-mismatch + 1.
        ls_netres-status = c_diff.
      ELSE.
        ms_stat-ok = ms_stat-ok + 1.
        ls_netres-status = c_ok.
      ENDIF.
      add_result( ls_netres ).
    ENDLOOP.

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


  METHOD get_num_component.
    FIELD-SYMBOLS <lv_any> TYPE any.
    ASSIGN COMPONENT iv_name OF STRUCTURE is_struc TO <lv_any>.
    IF sy-subrc = 0.
      TRY.
          rv_val = <lv_any>.
        CATCH cx_sy_conversion_error.
          CLEAR rv_val.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD get_chr_component.
    FIELD-SYMBOLS <lv_any> TYPE any.
    ASSIGN COMPONENT iv_name OF STRUCTURE is_struc TO <lv_any>.
    IF sy-subrc = 0.
      rv_val = <lv_any>.
    ENDIF.
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

    CASE ls_result-status.
      WHEN c_diff OR c_miss OR c_error.
        ls_result-color = VALUE #( ( fname = 'STATUS'
                                     color-col = col_negative
                                     color-int = 1 ) ).
      WHEN c_new OR c_manual.
        ls_result-color = VALUE #( ( fname = 'STATUS'
                                     color-col = col_total ) ).
      WHEN c_ok.
        ls_result-color = VALUE #( ( fname = 'STATUS'
                                     color-col = col_positive ) ).
    ENDCASE.

    " with "differences only" suppress OK lines
    IF p_onlyer = abap_true AND ls_result-status = c_ok.
      RETURN.
    ENDIF.

    APPEND ls_result TO mt_result.

  ENDMETHOD.


  METHOD display.

    DATA lo_alv TYPE REF TO cl_salv_table.

    SORT mt_result BY vbeln_x posnr kschl.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING  t_table      = mt_result ).

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
            lo_cols->get_column( 'VBELN_X' )->set_medium_text( 'Order X (ECC)' ).
            lo_cols->get_column( 'VBELN_Y' )->set_medium_text( 'Order Y (S/4)' ).
            lo_cols->get_column( 'STATUS' )->set_medium_text( 'Status' ).
            lo_cols->get_column( 'RATE_OLD' )->set_medium_text( 'Rate X (ECC)' ).
            lo_cols->get_column( 'RATE_NEW' )->set_medium_text( 'Rate Y (S/4)' ).
            lo_cols->get_column( 'RATE_DIFF' )->set_medium_text( 'Rate delta' ).
            lo_cols->get_column( 'KPEIN_OLD' )->set_medium_text( 'Per X' ).
            lo_cols->get_column( 'KPEIN_NEW' )->set_medium_text( 'Per Y' ).
            lo_cols->get_column( 'KMEIN_OLD' )->set_medium_text( 'UoM X' ).
            lo_cols->get_column( 'KMEIN_NEW' )->set_medium_text( 'UoM Y' ).
            lo_cols->get_column( 'KWERT_OLD' )->set_medium_text( 'Value X (ECC)' ).
            lo_cols->get_column( 'KWERT_NEW' )->set_medium_text( 'Value Y (S/4)' ).
            lo_cols->get_column( 'KWERT_DIFF' )->set_medium_text( 'Value delta' ).
            lo_cols->get_column( 'REMARK' )->set_medium_text( 'Remark' ).
          CATCH cx_salv_not_found.
        ENDTRY.

        " summary header
        DATA(lo_grid) = NEW cl_salv_form_layout_grid( ).
        lo_grid->create_label( row = 1 column = 1
          text = |Pricing validation ECC -> S/4HANA  ({ COND string(
                   WHEN p_crt = abap_true THEN 'create & reject Y'
                   ELSE 'simulate Y' ) })| ).
        lo_grid->create_flow( row = 2 column = 1 )->create_text(
          text = |Orders: { ms_stat-orders }  Errors: { ms_stat-errors }| ).
        lo_grid->create_flow( row = 3 column = 1 )->create_text(
          text = |Conditions compared: { ms_stat-compared }  | &
                 |OK: { ms_stat-ok }  Mismatch: { ms_stat-mismatch }  | &
                 |Missing in S/4: { ms_stat-missing }  | &
                 |New in S/4: { ms_stat-new_in_s4 }  | &
                 |Manual: { ms_stat-manual }| ).
        lo_alv->set_top_of_list( lo_grid ).

        lo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lx_salv).
        MESSAGE lx_salv->get_text( ) TYPE 'E'.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
START-OF-SELECTION.
  NEW lcl_app( )->run( ).
