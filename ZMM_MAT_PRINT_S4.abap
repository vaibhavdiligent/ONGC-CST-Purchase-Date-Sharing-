*&---------------------------------------------------------------------*
*& Report ZMM_MAT_PRINT_S4
*&---------------------------------------------------------------------*
*& Title       : Material Document Print (S/4HANA optimized)
*& Successor of: ZMM_MAT_PRINT (includes ZDEC2 / ZSEL12 / ZFETCH12 / ZCALL2)
*&---------------------------------------------------------------------*
*& S/4HANA remediation summary
*& ---------------------------------------------------------------------
*& 1. Custom DB view ZMKPF (MKPF + MSEG + T001W) is ELIMINATED.
*&    In S/4HANA MKPF/MSEG are compatibility views on MATDOC, so the
*&    custom join view is obsolete. It is replaced by the released CDS
*&    views (see ARS_API_SUCCESSOR):
*&      MKPF  -> I_MaterialDocumentHeader_2
*&      MSEG  -> I_MaterialDocumentItem_2
*&      T001W -> I_Plant
*&      T001  -> I_CompanyCode
*&      LFA1  -> I_Supplier
*&      QALS  -> I_InspectionLot (custom append field AR_NO is joined
*&               from QALS because append fields are not part of the
*&               released view)
*&      MAKT  -> I_ProductDescription
*&      T001L -> I_StorageLocation
*&    T156T / T157E / USER_ADDRP have no released successor in the
*&    provided API list and are read directly with new ABAP SQL.
*& 2. All includes merged into one class-based report (no ZDEC2,
*&    ZSEL12, ZFETCH12, ZCALL2).
*& 3. New ABAP SQL syntax (comma separated field lists, host variables,
*&    inline declarations), no SELECT inside loops (the former
*&    SELECT SINGLE on LFA1 per item row is removed - manufacturer
*&    names are buffered once via I_Supplier).
*& 4. Division by zero guard added for the rate calculation.
*& 5. Smartform ZMM_MATERIAL is still called through
*&    SSF_FUNCTION_MODULE_NAME (valid print technology in S/4HANA
*&    on-premise). The header table layout is identical to the former
*&    view structure ZMKPF + BUTXT, so the form interface keeps
*&    working. Once the DDIC view ZMKPF is deleted, retype the form
*&    parameter TA_HEADER with a plain DDIC structure that has the
*&    same field list as TY_HEADER below.
*&---------------------------------------------------------------------*
REPORT zmm_mat_print_s4.

*----------------------------------------------------------------------*
* Selection screen (same parameter names as ZMM_MAT_PRINT so existing
* user habits/variants can be recreated 1:1)
*----------------------------------------------------------------------*
DATA: gv_mblnr TYPE mblnr,
      gv_mjahr TYPE mjahr.

SELECT-OPTIONS: s_mblnr1 FOR gv_mblnr OBLIGATORY,
                s_mjahr1 FOR gv_mjahr OBLIGATORY DEFAULT sy-datum(4).

*----------------------------------------------------------------------*
* Local class
*----------------------------------------------------------------------*
CLASS lcl_mat_doc_print DEFINITION FINAL CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS run.

  PRIVATE SECTION.

    " Replacement of former header structure "INCLUDE STRUCTURE zmkpf"
    " + BUTXT - field list and order identical to the old DB view.
    TYPES: BEGIN OF ty_header,
             mblnr TYPE mblnr,
             mjahr TYPE mjahr,
             bldat TYPE bldat,
             budat TYPE budat,
             xblnr TYPE xblnr1,
             bktxt TYPE bktxt,
             werks TYPE werks_d,
             bukrs TYPE bukrs,
             kostl TYPE kostl,
             cpudt TYPE cpudt,
             cputm TYPE cputm,
             usnam TYPE usnam,
             name1 TYPE name1,
             butxt TYPE butxt,
           END OF ty_header.

    " Item structure for the smartform - unchanged DDIC structure
    TYPES: BEGIN OF ty_item.
             INCLUDE STRUCTURE zmseg2.
    TYPES: END OF ty_item.

    " Created-by information for the smartform - unchanged DDIC structure
    TYPES: BEGIN OF ty_crea.
             INCLUDE STRUCTURE znames.
    TYPES: END OF ty_crea.

    TYPES: BEGIN OF ty_mseg,
             mblnr TYPE mblnr,
             mjahr TYPE mjahr,
             zeile TYPE mblpo,
             bwart TYPE bwart,
             matnr TYPE matnr,
             lgort TYPE lgort_d,
             charg TYPE charg_d,
             lifnr TYPE lifnr,
             menge TYPE menge_d,
             shkzg TYPE shkzg,
             waers TYPE waers,
             dmbtr TYPE dmbtr,
             meins TYPE meins,
             sgtxt TYPE sgtxt,
             grund TYPE mb_grbew,
             vfdat TYPE vfdat,
             hsdat TYPE hsdat,
           END OF ty_mseg.

    TYPES: BEGIN OF ty_qals,
             enstehdat  TYPE qals-enstehdat,
             hersteller TYPE qals-hersteller,
             matnr      TYPE qals-matnr,
             charg      TYPE qals-charg,
             mjahr      TYPE qals-mjahr,
             mblnr      TYPE qals-mblnr,
             ar_no      TYPE qals-ar_no,     "customer append field
           END OF ty_qals.

    TYPES: BEGIN OF ty_supplier_batch,
             matnr TYPE matnr,
             charg TYPE charg_d,
             lifnr TYPE lifnr,
           END OF ty_supplier_batch.

    TYPES: BEGIN OF ty_name1,
             lifnr TYPE lifnr,
             name1 TYPE name1,
           END OF ty_name1.

    TYPES: BEGIN OF ty_user,
             bname     TYPE usr21-bname,
             name_text TYPE user_addrp-name_text,
           END OF ty_user.

    TYPES: BEGIN OF ty_makt,
             matnr TYPE matnr,
             maktx TYPE maktx,
           END OF ty_makt.

    TYPES: BEGIN OF ty_stor,
             lgort TYPE lgort_d,
             lgobe TYPE lgobe,
           END OF ty_stor.

    TYPES: BEGIN OF ty_t156t,
             bwart TYPE bwart,
             btext TYPE btext,
           END OF ty_t156t.

    TYPES: BEGIN OF ty_t157e,
             bwart TYPE bwart,
             grund TYPE mb_grbew,
             grtxt TYPE grtxt,
           END OF ty_t157e.

    CONSTANTS gc_langu_en TYPE spras VALUE 'E'.  "kept from old program ('EN')

    CLASS-DATA: gt_header   TYPE STANDARD TABLE OF ty_header,
                gt_item     TYPE STANDARD TABLE OF ty_item,
                gt_crea     TYPE STANDARD TABLE OF ty_crea,
                gt_mseg     TYPE STANDARD TABLE OF ty_mseg,
                gt_qals     TYPE STANDARD TABLE OF ty_qals,
                gt_lifnr    TYPE STANDARD TABLE OF ty_supplier_batch,
                gt_name1    TYPE STANDARD TABLE OF ty_name1,
                gt_user     TYPE STANDARD TABLE OF ty_user,
                gt_makt     TYPE STANDARD TABLE OF ty_makt,
                gt_stor     TYPE STANDARD TABLE OF ty_stor,
                gt_t156t    TYPE STANDARD TABLE OF ty_t156t,
                gt_t157e    TYPE STANDARD TABLE OF ty_t157e.

    CLASS-METHODS:
      fetch_header,
      check_authority,
      fetch_texts_for_header,
      build_created_by,
      fetch_items,
      fetch_item_texts,
      build_items,
      read_batch_classification
        IMPORTING iv_matnr TYPE matnr
                  iv_charg TYPE charg_d
        CHANGING  cs_item  TYPE ty_item,
      call_smartform.

ENDCLASS.


CLASS lcl_mat_doc_print IMPLEMENTATION.

  METHOD run.
    fetch_header( ).

    IF gt_header IS INITIAL.
      MESSAGE 'No material document found for the selection' TYPE 'S'
              DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    check_authority( ).
    fetch_texts_for_header( ).
    build_created_by( ).
    fetch_items( ).
    fetch_item_texts( ).
    build_items( ).
    call_smartform( ).
  ENDMETHOD.


  METHOD fetch_header.
    " Replaces: SELECT * FROM ZMKPF (custom DB view on MKPF/MSEG/T001W).
    " Released CDS views on MATDOC + I_Plant deliver the same field set.
    " The company code name (formerly a separate T001 select) is read
    " in the same statement via I_CompanyCode.
    SELECT FROM i_materialdocumentheader_2 AS hdr
             INNER JOIN i_materialdocumentitem_2 AS itm
               ON  itm~materialdocument     = hdr~materialdocument
               AND itm~materialdocumentyear = hdr~materialdocumentyear
             INNER JOIN i_plant AS plt
               ON plt~plant = itm~plant
             LEFT OUTER JOIN i_companycode AS coc
               ON coc~companycode = itm~companycode
      FIELDS hdr~materialdocument           AS mblnr,
             hdr~materialdocumentyear       AS mjahr,
             hdr~documentdate               AS bldat,
             hdr~postingdate                AS budat,
             hdr~referencedocument          AS xblnr,
             hdr~materialdocumentheadertext AS bktxt,
             itm~plant                      AS werks,
             itm~companycode                AS bukrs,
             itm~costcenter                 AS kostl,
             hdr~creationdate               AS cpudt,
             hdr~creationtime               AS cputm,
             hdr~createdbyuser              AS usnam,
             plt~plantname                  AS name1,
             coc~companycodename            AS butxt
      WHERE hdr~materialdocument     IN @s_mblnr1
        AND hdr~materialdocumentyear IN @s_mjahr1
      ORDER BY hdr~materialdocument, hdr~materialdocumentyear
      INTO CORRESPONDING FIELDS OF TABLE @gt_header.

    " One header line per material document (as in the old program)
    DELETE ADJACENT DUPLICATES FROM gt_header COMPARING mblnr mjahr.
  ENDMETHOD.


  METHOD check_authority.
    " Same authority object / behaviour as the old program
    LOOP AT gt_header ASSIGNING FIELD-SYMBOL(<ls_header>)
         GROUP BY <ls_header>-werks.
      AUTHORITY-CHECK OBJECT 'M_MSEG_WMB'
        ID 'ACTVT' FIELD '04'
        ID 'WERKS' FIELD <ls_header>-werks.
      IF sy-subrc <> 0.
        MESSAGE 'User is not authorized for plant' TYPE 'E'.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD fetch_texts_for_header.
    " User full names - no released CDS successor for USER_ADDRP in the
    " API successor list, therefore direct read with new ABAP SQL.
    SELECT bname, name_text
      FROM user_addrp
      FOR ALL ENTRIES IN @gt_header
      WHERE bname = @gt_header-usnam
      INTO TABLE @gt_user.
  ENDMETHOD.


  METHOD build_created_by.
    gt_crea = VALUE #(
      FOR ls_header IN gt_header
      ( mblnr     = ls_header-mblnr
        mjahr     = ls_header-mjahr
        cpudt     = ls_header-cpudt
        cputm     = ls_header-cputm
        usnam     = ls_header-usnam
        name_text = VALUE #( gt_user[ bname = ls_header-usnam ]-name_text
                             OPTIONAL ) ) ).
  ENDMETHOD.


  METHOD fetch_items.
    " Replaces both former MSEG selects (item data + goods receipt
    " vendor per material/batch) by I_MaterialDocumentItem_2.
    SELECT FROM i_materialdocumentitem_2
      FIELDS materialdocument         AS mblnr,
             materialdocumentyear     AS mjahr,
             materialdocumentitem     AS zeile,
             goodsmovementtype        AS bwart,
             material                 AS matnr,
             storagelocation          AS lgort,
             batch                    AS charg,
             supplier                 AS lifnr,
             quantityinbaseunit       AS menge,
             debitcreditcode          AS shkzg,
             companycodecurrency      AS waers,
             totalgoodsmvtamtincccrcy AS dmbtr,
             materialbaseunit         AS meins,
             materialdocumentitemtext AS sgtxt,
             goodsmovementreasoncode  AS grund,
             shelflifeexpirationdate  AS vfdat,
             manufacturedate          AS hsdat
      FOR ALL ENTRIES IN @gt_header
      WHERE materialdocument     = @gt_header-mblnr
        AND materialdocumentyear = @gt_header-mjahr
      INTO TABLE @gt_mseg.

    IF gt_mseg IS INITIAL.
      RETURN.
    ENDIF.

    " Goods receipt (mvt 101) supplier for material/batch
    SELECT FROM i_materialdocumentitem_2
      FIELDS material AS matnr,
             batch    AS charg,
             supplier AS lifnr
      FOR ALL ENTRIES IN @gt_mseg
      WHERE goodsmovementtype = '101'
        AND material          = @gt_mseg-matnr
        AND batch             = @gt_mseg-charg
      INTO TABLE @gt_lifnr.

    " Inspection lots: released view I_InspectionLot; the customer
    " append field AR_NO is not part of the released view and is
    " therefore joined from QALS via the inspection lot key.
    SELECT FROM i_inspectionlot AS lot
             INNER JOIN qals AS qal
               ON qal~prueflos = lot~inspectionlot
      FIELDS lot~insplotcreatedonlocaldate AS enstehdat,
             lot~manufacturer              AS hersteller,
             lot~material                  AS matnr,
             lot~batch                     AS charg,
             lot~materialdocumentyear      AS mjahr,
             lot~materialdocument          AS mblnr,
             qal~ar_no                     AS ar_no
      FOR ALL ENTRIES IN @gt_mseg
      WHERE lot~material = @gt_mseg-matnr
        AND lot~batch    = @gt_mseg-charg
      INTO TABLE @gt_qals.

    " Latest inspection lot first (evaluated against posting date later)
    SORT gt_qals BY enstehdat DESCENDING.
  ENDMETHOD.


  METHOD fetch_item_texts.
    IF gt_mseg IS INITIAL.
      RETURN.
    ENDIF.

    " Movement type text - no released successor for T156T in the API
    " successor list, direct read kept with new ABAP SQL.
    SELECT bwart, btext
      FROM t156t
      FOR ALL ENTRIES IN @gt_mseg
      WHERE spras = @gc_langu_en
        AND bwart = @gt_mseg-bwart
      INTO TABLE @gt_t156t.

    " Movement reason text - no released successor for T157E
    SELECT bwart, grund, grtxt
      FROM t157e
      FOR ALL ENTRIES IN @gt_mseg
      WHERE spras = @gc_langu_en
        AND bwart = @gt_mseg-bwart
        AND grund = @gt_mseg-grund
      INTO TABLE @gt_t157e.                             "#EC CI_NOORDER

    " Material description: MAKT -> released view I_ProductDescription
    SELECT FROM i_productdescription
      FIELDS product            AS matnr,
             productdescription AS maktx
      FOR ALL ENTRIES IN @gt_mseg
      WHERE product  = @gt_mseg-matnr
        AND language = @gc_langu_en
      INTO TABLE @gt_makt.

    " Storage location description: T001L -> released view
    " I_StorageLocation (like the old program the read is done by
    " storage location only, first name found is used)
    SELECT FROM i_storagelocation
      FIELDS DISTINCT
             storagelocation     AS lgort,
             storagelocationname AS lgobe
      FOR ALL ENTRIES IN @gt_mseg
      WHERE storagelocation = @gt_mseg-lgort
      INTO TABLE @gt_stor.

    " Supplier names: LFA1 -> released view I_Supplier
    IF gt_lifnr IS NOT INITIAL.
      SELECT FROM i_supplier
        FIELDS supplier           AS lifnr,
               organizationbpname1 AS name1
        FOR ALL ENTRIES IN @gt_lifnr
        WHERE supplier = @gt_lifnr-lifnr
        INTO TABLE @gt_name1.
    ENDIF.

    " Manufacturer names (buffered here - the old program executed a
    " SELECT SINGLE on LFA1 inside the item loop)
    IF gt_qals IS NOT INITIAL.
      SELECT FROM i_supplier
        FIELDS supplier            AS lifnr,
               organizationbpname1 AS name1
        FOR ALL ENTRIES IN @gt_qals
        WHERE supplier = @gt_qals-hersteller
        APPENDING TABLE @gt_name1.
    ENDIF.

    SORT gt_name1 BY lifnr.
    DELETE ADJACENT DUPLICATES FROM gt_name1 COMPARING lifnr.
  ENDMETHOD.


  METHOD build_items.
    LOOP AT gt_mseg ASSIGNING FIELD-SYMBOL(<ls_mseg>).
      DATA(ls_item) = VALUE ty_item( ).
      MOVE-CORRESPONDING <ls_mseg> TO ls_item.

      " Goods receipt vendor for the batch
      ls_item-lifnr = VALUE #( gt_lifnr[ matnr = <ls_mseg>-matnr
                                         charg = <ls_mseg>-charg ]-lifnr
                               DEFAULT ls_item-lifnr ).

      " AR number: latest inspection lot created on/before posting date
      DATA(lv_budat) = VALUE budat( gt_header[ mblnr = <ls_mseg>-mblnr
                                               mjahr = <ls_mseg>-mjahr ]-budat
                                    OPTIONAL ).
      LOOP AT gt_qals ASSIGNING FIELD-SYMBOL(<ls_qals>)
           WHERE enstehdat <= lv_budat
             AND matnr      = <ls_mseg>-matnr
             AND charg      = <ls_mseg>-charg.
        ls_item-ar_no = <ls_qals>-ar_no.
        EXIT.
      ENDLOOP.

      " Rate with division-by-zero guard (old program dumped on qty 0)
      IF <ls_mseg>-menge IS NOT INITIAL.
        ls_item-rate = <ls_mseg>-dmbtr / <ls_mseg>-menge.
      ENDIF.

      IF <ls_mseg>-shkzg = 'H'.
        ls_item-menge = -1 * <ls_mseg>-menge.
      ENDIF.

      " Batch classification (manufacturer batch / retest date /
      " manufacturer characteristics)
      read_batch_classification( EXPORTING iv_matnr = <ls_mseg>-matnr
                                           iv_charg = <ls_mseg>-charg
                                 CHANGING  cs_item  = ls_item ).

      ls_item-maktx = VALUE #( gt_makt[ matnr = <ls_mseg>-matnr ]-maktx
                               OPTIONAL ).
      ls_item-lgobe = VALUE #( gt_stor[ lgort = <ls_mseg>-lgort ]-lgobe
                               OPTIONAL ).
      ls_item-btext = VALUE #( gt_t156t[ bwart = <ls_mseg>-bwart ]-btext
                               OPTIONAL ).
      ls_item-btext1 = VALUE #( gt_t157e[ bwart = <ls_mseg>-bwart
                                          grund = <ls_mseg>-grund ]-grtxt
                                OPTIONAL ).

      " Vendor and manufacturer names from the I_Supplier buffer
      ls_item-name  = VALUE #( gt_name1[ lifnr = ls_item-lifnr ]-name1
                               OPTIONAL ).
      ls_item-name1 = VALUE #( gt_name1[ lifnr = ls_item-hersteller ]-name1
                               OPTIONAL ).

      APPEND ls_item TO gt_item.
    ENDLOOP.
  ENDMETHOD.


  METHOD read_batch_classification.
    DATA lt_batch TYPE STANDARD TABLE OF clbatch.

    " VB_BATCH_GET_DETAIL is still the supported way to read batch
    " classification data in S/4HANA on-premise.
    CALL FUNCTION 'VB_BATCH_GET_DETAIL'
      EXPORTING
        matnr              = iv_matnr
        charg              = iv_charg
        get_classification = 'X'
      TABLES
        char_of_batch      = lt_batch
      EXCEPTIONS
        no_material        = 1
        no_batch           = 2
        no_plant           = 3
        material_not_found = 4
        plant_not_found    = 5
        no_authority       = 6
        batch_not_exist    = 7
        lock_on_batch      = 8
        OTHERS             = 9.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    cs_item-atwtb =
      VALUE #( lt_batch[ atnam = 'ZMANUFACTURER_BATCH' ]-atwtb OPTIONAL ).
    " conversion CHAR -> date type as in the ATC-remediated original
    cs_item-zretest_date =
      VALUE #( lt_batch[ atnam = 'ZRETEST_DATE' ]-atwtb OPTIONAL ).
    cs_item-hersteller =
      VALUE #( lt_batch[ atnam = 'ZMANUFACTURER' ]-atwtb OPTIONAL ).
  ENDMETHOD.


  METHOD call_smartform.
    DATA: lv_fm_name  TYPE rs38l_fnam,
          lv_formname TYPE tdsfname VALUE 'ZMM_MATERIAL'.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = lv_formname
      IMPORTING
        fm_name            = lv_fm_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    CALL FUNCTION lv_fm_name
      TABLES
        ta_header        = gt_header
        ta_item          = gt_item
        ta_crea          = gt_crea
      EXCEPTIONS
        formatting_error = 1
        internal_error   = 2
        send_error       = 3
        user_canceled    = 4
        OTHERS           = 5.
    IF sy-subrc <> 0 AND sy-subrc <> 4.   "user cancel is no error
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
START-OF-SELECTION.
  lcl_mat_doc_print=>run( ).
