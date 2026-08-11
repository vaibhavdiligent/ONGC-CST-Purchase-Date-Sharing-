*&---------------------------------------------------------------------*
*&  Include           ZJV_TRANSFER_POSTING_F01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Form Include
*----------------------------------------------------------------------*

FORM get_data.

* S/4HANA: table JVSO1 replaced by CDS view JV_JVSO1_ACDOCA
* (ABAP SQL name CJVSO1_ACD) as per SAP Note 3082514.
* The view returns ACDOCA data in the JVSO1 structure without a
* ledger split - ledger 4C amounts are in columns HSL_4C / KSL_4C.
  SELECT rjvnam
         rrecin
         racct
         rcntr
         rordnr
         rprojk
         rprctr
         tsl
         hsl
         ksl
         hsl_4c
         ksl_4c
         budat
    INTO TABLE gt_jvso1
    FROM cjvso1_acd
   WHERE rrcty = '0'
     AND rvers = '1'
     AND ryear = p_gjahr
     AND poper = p_poper.

ENDFORM.

*----------------------------------------------------------------------*

FORM process_data.

  LOOP AT gt_jvso1 INTO gs_jvso1.

* Last day of month
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = gs_jvso1-budat
      IMPORTING
        last_day_of_month = gv_postdate.

    gv_docdate = gv_postdate.

  ENDLOOP.

ENDFORM.

*----------------------------------------------------------------------*

FORM post_document.

  DATA: lv_itemno TYPE bapiacgl09-itemno_acc VALUE '1'.

  LOOP AT gt_jvso1 INTO gs_jvso1.

    CLEAR gs_accountgl.

    gs_accountgl-itemno_acc  = lv_itemno.
    gs_accountgl-gl_account  = gs_jvso1-racct.
    gs_accountgl-costcenter  = gs_jvso1-rcntr.
    gs_accountgl-orderid     = gs_jvso1-rordnr.
    gs_accountgl-wbs_element = gs_jvso1-rprojk.
    gs_accountgl-profit_ctr  = gs_jvso1-rprctr.

    APPEND gs_accountgl TO gt_accountgl.

    CLEAR gs_currency.
    gs_currency-itemno_acc = lv_itemno.
    gs_currency-currency   = 'INR'.
    gs_currency-amt_doccur = gs_jvso1-tsl.

    APPEND gs_currency TO gt_currency.

    ADD 1 TO lv_itemno.

  ENDLOOP.

  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader-username   = sy-uname
      documentheader-comp_code  = p_bukrs
      documentheader-doc_date   = gv_docdate
      documentheader-pstng_date = gv_postdate
      documentheader-doc_type   = 'SA'
    TABLES
      accountgl      = gt_accountgl
      currencyamount = gt_currency
      return         = gt_return.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  MESSAGE 'Document Posted Successfully' TYPE 'S'.

ENDFORM.

*----------------------------------------------------------------------*

FORM display_output.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_structure_name = 'TY_JVSO1'
    TABLES
      t_outtab         = gt_jvso1.

ENDFORM.
