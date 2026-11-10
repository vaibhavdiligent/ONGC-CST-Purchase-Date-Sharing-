*----------------------------------------------------------------------*
* LCL_CAT - the fields the program knows
*   Built at runtime, not typed in: for every part of the master record
*   the fields of its database table that the API structure also carries.
*   A field added to KNA1 and to the API structure (an append for a new
*   custom field) is therefore picked up without a change here.
*----------------------------------------------------------------------*
CLASS lcl_cat DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS groups RETURNING VALUE(rt) TYPE tt_gdef.
    CLASS-METHODS grp
      IMPORTING iv_grp    TYPE ty_grp
      RETURNING VALUE(rs) TYPE ty_gdef.
    CLASS-METHODS all RETURNING VALUE(rt) TYPE tt_fld.
    CLASS-METHODS by_name
      IMPORTING iv_name   TYPE string
      RETURNING VALUE(rs) TYPE ty_fld.
    " A heading from a file: TABLE-FIELD, a field name alone where it is
    " unique, or one of the names the LSMW files used.
    CLASS-METHODS resolve
      IMPORTING iv_head TYPE string
      EXPORTING es_fld  TYPE ty_fld
                ev_err  TYPE string.
    " The number column: KNA1-KUNNR or LFA1-LIFNR.
    CLASS-METHODS num_name RETURNING VALUE(rv) TYPE string.
    " The headings that identify an entry of a group: its parent's keys
    " and its own (the number column is not included).
    CLASS-METHODS key_names
      IMPORTING iv_grp    TYPE ty_grp
      RETURNING VALUE(rt) TYPE string_table.
    " The group a key belongs to and the groups a repeating group hangs on.
    CLASS-METHODS root_of
      IMPORTING iv_grp    TYPE ty_grp
      RETURNING VALUE(rv) TYPE ty_grp.
  PRIVATE SECTION.
    CLASS-DATA mt_fld  TYPE tt_fld.
    CLASS-DATA mt_gdef TYPE tt_gdef.
    TYPES: BEGIN OF ty_alias,
             alias TYPE string,
             name  TYPE string,
           END OF ty_alias.
    CLASS-DATA mt_alias TYPE SORTED TABLE OF ty_alias WITH UNIQUE KEY alias.

    CLASS-METHODS build.
    CLASS-METHODS add_table IMPORTING is_g TYPE ty_gdef.
    CLASS-METHODS add_addr.
    CLASS-METHODS add_comm.
    CLASS-METHODS add_text.
    CLASS-METHODS add_alias.
    CLASS-METHODS ddic
      IMPORTING iv_tab    TYPE clike
      RETURNING VALUE(rt) TYPE ddfields.
    CLASS-METHODS comps
      IMPORTING iv_struc  TYPE clike
      RETURNING VALUE(rt) TYPE string_table.
    CLASS-METHODS from_dfies
      IMPORTING is_d      TYPE dfies
                iv_grp    TYPE ty_grp
                iv_api    TYPE clike
      RETURNING VALUE(rs) TYPE ty_fld.
    CLASS-METHODS add
      IMPORTING is_f TYPE ty_fld.
ENDCLASS.


CLASS lcl_cat IMPLEMENTATION.

  METHOD groups.
    IF mt_gdef IS INITIAL.
      IF gc_side = 'C'.
        mt_gdef = VALUE #(
          ( grp = 'CENT' tab = 'KNA1' text = 'General data' api = 'CMDS_EI_VMD_CENTRAL_DATA' numfld = 'KUNNR' )
          ( grp = 'ADDR' tab = 'ADRC' text = 'Name and address' )
          ( grp = 'COMM' tab = 'ADR2' text = 'Telephone, fax, e-mail' )
          ( grp = 'TXTC' tab = 'STXH' text = 'Texts - general' )
          ( grp = 'COMP' tab = 'KNB1' text = 'Company code' keys = 'BUKRS' api = 'CMDS_EI_COMPANY_DATA' numfld = 'KUNNR' )
          ( grp = 'TXTB' tab = 'STXH' text = 'Texts - company code' parent = 'COMP' )
          ( grp = 'SALE' tab = 'KNVV' text = 'Sales area' keys = 'VKORG VTWEG SPART' api = 'CMDS_EI_SALES_DATA' numfld = 'KUNNR' )
          ( grp = 'TXTS' tab = 'STXH' text = 'Texts - sales area' parent = 'SALE' )
          ( grp = 'TAXI' tab = 'KNVI' text = 'Tax classification' rep = abap_true keys = 'ALAND TATYP' api = 'CMDS_EI_TAX_IND_DATA' numfld = 'KUNNR' )
          ( grp = 'PART' tab = 'KNVP' text = 'Partner functions' rep = abap_true parent = 'SALE' keys = 'PARVW PARZA' api = 'CMDS_EI_FUNCTIONS_DATA' numfld = 'KUNNR' )
          ( grp = 'UNLD' tab = 'KNVA' text = 'Unloading points' rep = abap_true keys = 'ABLAD' api = 'CMDS_EI_LOADING_DATA' numfld = 'KUNNR' )
          ( grp = 'BANK' tab = 'KNBK' text = 'Bank details' rep = abap_true keys = 'BANKS BANKL BANKN' api = 'CVIS_EI_BANKDETAIL_DATA' numfld = 'KUNNR' )
          ( grp = 'CONT' tab = 'KNVK' text = 'Contact persons' rep = abap_true keys = 'PARNR' api = 'CMDS_EI_CONTACTS_DATA' numfld = 'KUNNR' )
          ( grp = 'DUNN' tab = 'KNB5' text = 'Dunning data' rep = abap_true parent = 'COMP' keys = 'MABER' api = 'CMDS_EI_DUNNING_DATA' numfld = 'KUNNR' )
          ( grp = 'WTAX' tab = 'KNBW' text = 'Withholding tax' rep = abap_true parent = 'COMP' keys = 'WITHT' api = 'CMDS_EI_WTAX_DATA' numfld = 'KUNNR' ) ).
      ELSE.
        mt_gdef = VALUE #(
          ( grp = 'CENT' tab = 'LFA1' text = 'General data' api = 'VMDS_EI_VMD_CENTRAL_DATA' numfld = 'LIFNR' )
          ( grp = 'ADDR' tab = 'ADRC' text = 'Name and address' )
          ( grp = 'COMM' tab = 'ADR2' text = 'Telephone, fax, e-mail' )
          ( grp = 'TXTC' tab = 'STXH' text = 'Texts - general' )
          ( grp = 'COMP' tab = 'LFB1' text = 'Company code' keys = 'BUKRS' api = 'VMDS_EI_COMPANY_DATA' numfld = 'LIFNR' )
          ( grp = 'TXTB' tab = 'STXH' text = 'Texts - company code' parent = 'COMP' )
          ( grp = 'PURC' tab = 'LFM1' text = 'Purchasing organization' keys = 'EKORG' api = 'VMDS_EI_PURCHASING_DATA' numfld = 'LIFNR' )
          ( grp = 'TXTS' tab = 'STXH' text = 'Texts - purchasing organization' parent = 'PURC' )
          ( grp = 'PART' tab = 'WYT3' text = 'Partner functions' rep = abap_true parent = 'PURC' keys = 'LTSNR WERKS PARVW PARZA' api = 'VMDS_EI_FUNCTIONS_DATA' numfld = 'LIFNR' )
          ( grp = 'BANK' tab = 'LFBK' text = 'Bank details' rep = abap_true keys = 'BANKS BANKL BANKN' api = 'CVIS_EI_BANKDETAIL_DATA' numfld = 'LIFNR' )
          ( grp = 'CONT' tab = 'KNVK' text = 'Contact persons' rep = abap_true keys = 'PARNR' api = 'VMDS_EI_CONTACTS_DATA' numfld = 'LIFNR' )
          ( grp = 'DUNN' tab = 'LFB5' text = 'Dunning data' rep = abap_true parent = 'COMP' keys = 'MABER' api = 'VMDS_EI_DUNNING_DATA' numfld = 'LIFNR' )
          ( grp = 'WTAX' tab = 'LFBW' text = 'Withholding tax' rep = abap_true parent = 'COMP' keys = 'WITHT' api = 'VMDS_EI_WTAX_DAT' numfld = 'LIFNR' ) ).
      ENDIF.
    ENDIF.
    rt = mt_gdef.
  ENDMETHOD.

  METHOD grp.
    DATA(lt_g) = groups( ).
    READ TABLE lt_g INTO rs WITH KEY grp = iv_grp.
  ENDMETHOD.

  METHOD root_of.
    " The single-valued part an entry belongs to: a partner function to its
    " sales area, a dunning entry to its company code, everything without a
    " parent to the general data.
    DATA(ls_g) = grp( iv_grp ).
    rv = COND #( WHEN ls_g-parent IS NOT INITIAL THEN ls_g-parent
                 WHEN ls_g-rep = abap_true    THEN 'CENT'
                 ELSE iv_grp ).
  ENDMETHOD.

  METHOD num_name.
    rv = COND #( WHEN gc_side = 'C' THEN `KNA1-KUNNR` ELSE `LFA1-LIFNR` ).
  ENDMETHOD.

  METHOD key_names.
    DATA(ls_g) = grp( iv_grp ).
    IF ls_g-parent IS NOT INITIAL.
      rt = key_names( ls_g-parent ).
    ENDIF.
    IF ls_g-keys IS NOT INITIAL.
      SPLIT ls_g-keys AT ` ` INTO TABLE DATA(lt_k).
      LOOP AT lt_k INTO DATA(lv_k).
        APPEND |{ ls_g-tab }-{ lv_k }| TO rt.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD all.
    IF mt_fld IS INITIAL.
      build( ).
    ENDIF.
    rt = mt_fld.
  ENDMETHOD.

  METHOD by_name.
    IF mt_fld IS INITIAL.
      build( ).
    ENDIF.
    READ TABLE mt_fld INTO rs WITH KEY name = iv_name.
  ENDMETHOD.

  METHOD add.
    IF line_exists( mt_fld[ name = is_f-name ] ).
      RETURN.
    ENDIF.
    APPEND is_f TO mt_fld.
  ENDMETHOD.

  METHOD ddic.
    DATA lo_t TYPE REF TO cl_abap_typedescr.
    cl_abap_typedescr=>describe_by_name(
      EXPORTING  p_name         = iv_tab
      RECEIVING  p_descr_ref    = lo_t
      EXCEPTIONS type_not_found = 1
                 OTHERS         = 2 ).
    IF sy-subrc <> 0 OR lo_t->kind <> cl_abap_typedescr=>kind_struct.
      RETURN.
    ENDIF.
    DATA(lo_s) = CAST cl_abap_structdescr( lo_t ).
    lo_s->get_ddic_field_list(
      EXPORTING  p_langu      = sy-langu
      RECEIVING  p_field_list = rt
      EXCEPTIONS not_found    = 1
                 no_ddic_type = 2
                 OTHERS       = 3 ).
    IF sy-subrc <> 0.
      CLEAR rt.
    ENDIF.
  ENDMETHOD.

  METHOD comps.
    DATA lo_t TYPE REF TO cl_abap_typedescr.
    cl_abap_typedescr=>describe_by_name(
      EXPORTING  p_name         = iv_struc
      RECEIVING  p_descr_ref    = lo_t
      EXCEPTIONS type_not_found = 1
                 OTHERS         = 2 ).
    IF sy-subrc <> 0 OR lo_t->kind <> cl_abap_typedescr=>kind_struct.
      RETURN.
    ENDIF.
    " The included view: the components of an append or include structure
    " are listed as components of their own, which is how a custom field
    " added through an append shows up.
    DATA(lt_c) = CAST cl_abap_structdescr( lo_t )->get_included_view( ).
    LOOP AT lt_c INTO DATA(ls_c).
      APPEND CONV string( ls_c-name ) TO rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD from_dfies.
    rs-name     = |{ is_d-tabname }-{ is_d-fieldname }|.
    rs-grp      = iv_grp.
    rs-tab      = is_d-tabname.
    rs-fld      = is_d-fieldname.
    rs-api      = iv_api.
    rs-text     = COND #( WHEN is_d-scrtext_l IS NOT INITIAL THEN is_d-scrtext_l
                          ELSE is_d-fieldtext ).
    rs-rollname = is_d-rollname.
    rs-convexit = is_d-convexit.
    rs-inttype  = is_d-inttype.
    rs-leng     = is_d-leng.
    rs-decimals = is_d-decimals.
    rs-lower    = xsdbool( is_d-lowercase IS NOT INITIAL ).
    rs-checktab = is_d-checktable.
    rs-domname  = is_d-domname.
    rs-valexi   = xsdbool( is_d-valexi IS NOT INITIAL ).
  ENDMETHOD.

  METHOD add_table.
    DATA(lt_db) = ddic( is_g-tab ).
    IF lt_db IS INITIAL.
      RETURN.
    ENDIF.
    DATA lt_api TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    DATA(lt_c) = comps( is_g-api ).
    LOOP AT lt_c INTO DATA(lv_c).
      INSERT lv_c INTO TABLE lt_api.
    ENDLOOP.

    " ---- keys: the number on the general data, the group's own keys here
    DATA lt_key TYPE SORTED TABLE OF fieldname WITH UNIQUE KEY table_line.
    IF is_g-keys IS NOT INITIAL.
      SPLIT is_g-keys AT ` ` INTO TABLE DATA(lt_k).
      LOOP AT lt_k INTO DATA(lv_k).
        INSERT CONV fieldname( lv_k ) INTO TABLE lt_key.
      ENDLOOP.
    ENDIF.

    LOOP AT lt_db INTO DATA(ls_d) WHERE datatype <> 'CLNT'.
      DATA(ls_f) = from_dfies( is_d = ls_d iv_grp = is_g-grp iv_api = ls_d-fieldname ).
      IF ls_d-fieldname = is_g-numfld.
        IF is_g-grp = 'CENT'.
          ls_f-key  = abap_true.
          ls_f-must = abap_true.
          add( ls_f ).
        ENDIF.
        CONTINUE.
      ENDIF.
      IF line_exists( lt_key[ table_line = ls_d-fieldname ] ).
        ls_f-key  = abap_true.
        ls_f-must = abap_true.
        add( ls_f ).
        CONTINUE.
      ENDIF.
      IF ls_d-keyflag = abap_true.
        " A key the API does not take as data (the vendor partner
        " function's plant, for instance) stays out of the data fields.
        CONTINUE.
      ENDIF.
      IF line_exists( lt_api[ table_line = CONV string( ls_d-fieldname ) ] ).
        add( ls_f ).
      ENDIF.
    ENDLOOP.

    " The customer's partner function names its partner in KUNN2, LIFNR,
    " PERNR or PARNR depending on the function; the API takes one number.
    IF is_g-grp = 'PART' AND gc_side = 'C'.
      DATA(ls_p) = VALUE ty_fld( name = `KNVP-PARTNER` grp = 'PART' tab = 'KNVP'
                                 api = 'PARTNER' rollname = 'GPANR' inttype = 'C' leng = 10
                                 text = `Partner number (customer, supplier, personnel or contact)` ).
      add( ls_p ).
    ENDIF.
  ENDMETHOD.

  METHOD add_addr.
    " ADRC field -> component of the API's address structure (BAPIAD1VL).
    TYPES: BEGIN OF ty_m, adrc TYPE fieldname, api TYPE fieldname, END OF ty_m.
    DATA lt_m TYPE STANDARD TABLE OF ty_m WITH EMPTY KEY.
    lt_m = VALUE #(
      ( adrc = 'NAME1'      api = 'NAME' )       ( adrc = 'NAME2'      api = 'NAME_2' )
      ( adrc = 'NAME3'      api = 'NAME_3' )     ( adrc = 'NAME4'      api = 'NAME_4' )
      ( adrc = 'NAME_CO'    api = 'C_O_NAME' )   ( adrc = 'SORT1'      api = 'SORT1' )
      ( adrc = 'SORT2'      api = 'SORT2' )      ( adrc = 'TITLE'      api = 'TITLE' )
      ( adrc = 'STREET'     api = 'STREET' )     ( adrc = 'HOUSE_NUM1' api = 'HOUSE_NO' )
      ( adrc = 'HOUSE_NUM2' api = 'HOUSE_NO2' )  ( adrc = 'STR_SUPPL1' api = 'STR_SUPPL1' )
      ( adrc = 'STR_SUPPL2' api = 'STR_SUPPL2' ) ( adrc = 'STR_SUPPL3' api = 'STR_SUPPL3' )
      ( adrc = 'LOCATION'   api = 'LOCATION' )   ( adrc = 'BUILDING'   api = 'BUILDING' )
      ( adrc = 'FLOOR'      api = 'FLOOR' )      ( adrc = 'ROOMNUMBER' api = 'ROOM_NO' )
      ( adrc = 'CITY1'      api = 'CITY' )       ( adrc = 'CITY2'      api = 'DISTRICT' )
      ( adrc = 'POST_CODE1' api = 'POSTL_COD1' ) ( adrc = 'POST_CODE2' api = 'POSTL_COD2' )
      ( adrc = 'PO_BOX'     api = 'PO_BOX' )     ( adrc = 'COUNTRY'    api = 'COUNTRY' )
      ( adrc = 'REGION'     api = 'REGION' )     ( adrc = 'LANGU'      api = 'LANGU' )
      ( adrc = 'TRANSPZONE' api = 'TRANSPZONE' ) ( adrc = 'TIME_ZONE'  api = 'TIME_ZONE' )
      ( adrc = 'TAXJURCODE' api = 'TAXJURCODE' ) ( adrc = 'DEFLT_COMM' api = 'COMM_TYPE' )
      ( adrc = 'EXTENSION1' api = 'EXTENS_1' )   ( adrc = 'EXTENSION2' api = 'EXTENS_2' ) ).
    DATA(lt_db) = ddic( 'ADRC' ).
    DATA lt_api TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    DATA(lt_c) = comps( 'BAPIAD1VL' ).
    LOOP AT lt_c INTO DATA(lv_c).
      INSERT lv_c INTO TABLE lt_api.
    ENDLOOP.
    LOOP AT lt_m INTO DATA(ls_m).
      READ TABLE lt_db INTO DATA(ls_d) WITH KEY fieldname = ls_m-adrc.
      IF sy-subrc <> 0 OR NOT line_exists( lt_api[ table_line = CONV string( ls_m-api ) ] ).
        CONTINUE.
      ENDIF.
      add( from_dfies( is_d = ls_d iv_grp = 'ADDR' iv_api = ls_m-api ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD add_comm.
    " The standard telephone, mobile, fax and e-mail. API component names
    " are those of BAPIADTEL, BAPIADFAX and BAPIADSMTP.
    DATA(lt_2) = ddic( 'ADR2' ).
    DATA(lt_3) = ddic( 'ADR3' ).
    DATA(lt_6) = ddic( 'ADR6' ).
    READ TABLE lt_2 INTO DATA(ls_d) WITH KEY fieldname = 'TEL_NUMBER'.
    IF sy-subrc = 0.
      add( from_dfies( is_d = ls_d iv_grp = 'COMM' iv_api = 'TELEPHONE' ) ).
      DATA(ls_mob) = from_dfies( is_d = ls_d iv_grp = 'COMM' iv_api = 'MOBILE' ).
      ls_mob-name = `ADR2-MOB_NUMBER`.
      ls_mob-text = `Mobile number`.
      add( ls_mob ).
    ENDIF.
    READ TABLE lt_2 INTO ls_d WITH KEY fieldname = 'TEL_EXTENS'.
    IF sy-subrc = 0.
      add( from_dfies( is_d = ls_d iv_grp = 'COMM' iv_api = 'EXTENSION' ) ).
    ENDIF.
    READ TABLE lt_3 INTO ls_d WITH KEY fieldname = 'FAX_NUMBER'.
    IF sy-subrc = 0.
      add( from_dfies( is_d = ls_d iv_grp = 'COMM' iv_api = 'FAX' ) ).
    ENDIF.
    READ TABLE lt_3 INTO ls_d WITH KEY fieldname = 'FAX_EXTENS'.
    IF sy-subrc = 0.
      add( from_dfies( is_d = ls_d iv_grp = 'COMM' iv_api = 'EXTENSION' ) ).
    ENDIF.
    READ TABLE lt_6 INTO ls_d WITH KEY fieldname = 'SMTP_ADDR'.
    IF sy-subrc = 0.
      add( from_dfies( is_d = ls_d iv_grp = 'COMM' iv_api = 'E_MAIL' ) ).
    ENDIF.
  ENDMETHOD.

  METHOD add_text.
    " One column per text ID: TEXT-KNVV-Z911 and so on.
    DATA lt_obj TYPE RANGE OF tdobject.
    IF gc_side = 'C'.
      lt_obj = VALUE #( sign = 'I' option = 'EQ' ( low = 'KNA1' ) ( low = 'KNB1' ) ( low = 'KNVV' ) ).
    ELSE.
      lt_obj = VALUE #( sign = 'I' option = 'EQ' ( low = 'LFA1' ) ( low = 'LFB1' ) ( low = 'LFM1' ) ).
    ENDIF.
    SELECT tdobject, tdid FROM ttxid
      WHERE tdobject IN @lt_obj
      ORDER BY tdobject, tdid
      INTO TABLE @DATA(lt_id).
    IF lt_id IS INITIAL.
      RETURN.
    ENDIF.
    SELECT tdobject, tdid, tdspras, tdtext FROM ttxit
      FOR ALL ENTRIES IN @lt_id
      WHERE tdobject = @lt_id-tdobject AND tdid = @lt_id-tdid
      INTO TABLE @DATA(lt_tx).
    LOOP AT lt_id INTO DATA(ls_id).
      DATA(lv_txt) = VALUE string( lt_tx[ tdobject = ls_id-tdobject tdid = ls_id-tdid tdspras = sy-langu ]-tdtext OPTIONAL ).
      IF lv_txt IS INITIAL.
        lv_txt = VALUE #( lt_tx[ tdobject = ls_id-tdobject tdid = ls_id-tdid tdspras = 'E' ]-tdtext OPTIONAL ).
      ENDIF.
      DATA(lv_grp) = COND ty_grp( WHEN ls_id-tdobject = 'KNA1' OR ls_id-tdobject = 'LFA1' THEN 'TXTC'
                                  WHEN ls_id-tdobject = 'KNB1' OR ls_id-tdobject = 'LFB1' THEN 'TXTB'
                                  ELSE 'TXTS' ).
      add( VALUE #( name = |TEXT-{ ls_id-tdobject }-{ ls_id-tdid }| grp = lv_grp
                    tab = 'STXH' inttype = 'g' lower = abap_true leng = 0
                    text = |Text { ls_id-tdid }: { lv_txt }|
                    tdobj = ls_id-tdobject tdid = ls_id-tdid ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD add_alias.
    " The customer's own column names - KNA1 address fields, which in
    " S/4HANA belong to the business partner's address, and the names the
    " LSMW files used.
    DATA(lv_g) = COND string( WHEN gc_side = 'C' THEN `KNA1` ELSE `LFA1` ).
    DATA(lv_b) = COND string( WHEN gc_side = 'C' THEN `KNB1` ELSE `LFB1` ).
    DATA(lv_s) = COND string( WHEN gc_side = 'C' THEN `KNVV` ELSE `LFM1` ).
    DATA lt_a TYPE STANDARD TABLE OF ty_alias WITH EMPTY KEY.
    lt_a = VALUE #(
      ( alias = |{ lv_g }-NAME1| name = `ADRC-NAME1` ) ( alias = |{ lv_g }-NAME2| name = `ADRC-NAME2` )
      ( alias = |{ lv_g }-NAME3| name = `ADRC-NAME3` ) ( alias = |{ lv_g }-NAME4| name = `ADRC-NAME4` )
      ( alias = |{ lv_g }-SORTL| name = `ADRC-SORT1` ) ( alias = |{ lv_g }-STRAS| name = `ADRC-STREET` )
      ( alias = |{ lv_g }-ORT01| name = `ADRC-CITY1` ) ( alias = |{ lv_g }-ORT02| name = `ADRC-CITY2` )
      ( alias = |{ lv_g }-PSTLZ| name = `ADRC-POST_CODE1` ) ( alias = |{ lv_g }-PSTL2| name = `ADRC-POST_CODE2` )
      ( alias = |{ lv_g }-PFACH| name = `ADRC-PO_BOX` ) ( alias = |{ lv_g }-REGIO| name = `ADRC-REGION` )
      ( alias = |{ lv_g }-LAND1| name = `ADRC-COUNTRY` ) ( alias = |{ lv_g }-SPRAS| name = `ADRC-LANGU` )
      ( alias = |{ lv_g }-LZONE| name = `ADRC-TRANSPZONE` ) ( alias = |{ lv_g }-TELF1| name = `ADR2-TEL_NUMBER` )
      ( alias = |{ lv_g }-TELFX| name = `ADR3-FAX_NUMBER` )
      ( alias = `POST` name = `ADRC-POST_CODE1` ) ( alias = `POST_CODE` name = `ADRC-POST_CODE1` )
      ( alias = `SUPPL2` name = `ADRC-STR_SUPPL2` ) ( alias = `SUPPL3` name = `ADRC-STR_SUPPL3` )
      ( alias = `TEL` name = `ADR2-TEL_NUMBER` ) ( alias = `FAX` name = `ADR3-FAX_NUMBER` )
      ( alias = `MOB_NUMBER` name = `ADR2-MOB_NUMBER` )
      ( alias = `ZTERM_B` name = |{ lv_b }-ZTERM| ) ( alias = `ZTERMB` name = |{ lv_b }-ZTERM| )
      ( alias = `ZTERM_V` name = |{ lv_s }-ZTERM| ) ( alias = `ZTERMV` name = |{ lv_s }-ZTERM| )
      ( alias = |{ lv_g }_LOEVM| name = |{ lv_g }-LOEVM| ) ( alias = |{ lv_b }_LOEVM| name = |{ lv_b }-LOEVM| ) ).
    IF gc_side = 'C'.
      APPEND VALUE #( alias = `KTONR` name = `KNVP-PARTNER` ) TO lt_a.
    ENDIF.
    LOOP AT lt_a INTO DATA(ls_a).
      IF line_exists( mt_fld[ name = ls_a-name ] ).
        INSERT ls_a INTO TABLE mt_alias.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD build.
    CLEAR: mt_fld, mt_alias.
    LOOP AT groups( ) INTO DATA(ls_g).
      CASE ls_g-grp.
        WHEN 'ADDR'.
          add_addr( ).
        WHEN 'COMM'.
          add_comm( ).
        WHEN 'TXTC' OR 'TXTB' OR 'TXTS'.
          IF ls_g-grp = 'TXTC'.
            add_text( ).
          ENDIF.
        WHEN OTHERS.
          add_table( ls_g ).
      ENDCASE.
    ENDLOOP.
    add_alias( ).
  ENDMETHOD.

  METHOD resolve.
    CLEAR: es_fld, ev_err.
    IF mt_fld IS INITIAL.
      build( ).
    ENDIF.
    DATA(lv_h) = to_upper( condense( val = iv_head del = ` ` ) ).
    REPLACE ALL OCCURRENCES OF ` ` IN lv_h WITH ``.

    READ TABLE mt_fld INTO es_fld WITH KEY name = lv_h.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.
    READ TABLE mt_alias INTO DATA(ls_a) WITH KEY alias = lv_h.
    IF sy-subrc = 0.
      READ TABLE mt_fld INTO es_fld WITH KEY name = ls_a-name.
      RETURN.
    ENDIF.

    " A field name on its own - only where exactly one field has it.
    IF lv_h NS '-'.
      DATA lv_n   TYPE i.
      DATA lv_alt TYPE string.
      LOOP AT mt_fld INTO DATA(ls_f) WHERE fld = lv_h.
        lv_n = lv_n + 1.
        es_fld = ls_f.
        lv_alt = COND #( WHEN lv_alt IS INITIAL THEN ls_f-name ELSE |{ lv_alt }, { ls_f-name }| ).
      ENDLOOP.
      IF lv_n = 1.
        RETURN.
      ELSEIF lv_n > 1.
        CLEAR es_fld.
        ev_err = |Column "{ iv_head }" is in more than one table - name it as one of: { lv_alt }|.
        RETURN.
      ENDIF.
    ENDIF.
    ev_err = |Column "{ iv_head }" is not a field this program knows|.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* LCL_VAL - a value as the file shows it and as SAP holds it
*   File values are text. Each is converted with the field's own rules
*   from the ABAP Dictionary: its type, its length, its conversion routine
*   (leading zeros, language keys) and whether it may be lower case.
*----------------------------------------------------------------------*
CLASS lcl_val DEFINITION FINAL.
  PUBLIC SECTION.
    " A data object of the field's own type.
    CLASS-METHODS new_data
      IMPORTING is_f      TYPE ty_fld
      RETURNING VALUE(rr) TYPE REF TO data.
    " File text -> value of the field's type. EV_ERR says why it cannot be.
    CLASS-METHODS to_int
      IMPORTING is_f    TYPE ty_fld
                iv_in   TYPE string
      EXPORTING er_val  TYPE REF TO data
                ev_err  TYPE string.
    " Value of the field's type -> file text.
    CLASS-METHODS to_ext
      IMPORTING is_f      TYPE ty_fld
                iv_val    TYPE any
      RETURNING VALUE(rv) TYPE string.
  PRIVATE SECTION.
    CLASS-METHODS to_time
      IMPORTING iv_in     TYPE string
      RETURNING VALUE(rv) TYPE string.
ENDCLASS.


CLASS lcl_val IMPLEMENTATION.

  METHOD new_data.
    TRY.
        IF is_f-inttype = 'g'.
          CREATE DATA rr TYPE string.
        ELSEIF is_f-fld IS NOT INITIAL AND is_f-tab <> 'STXH'.
          DATA(lv_t) = |{ is_f-tab }-{ is_f-fld }|.
          CREATE DATA rr TYPE (lv_t).
        ELSEIF is_f-rollname IS NOT INITIAL.
          CREATE DATA rr TYPE (is_f-rollname).
        ELSE.
          CREATE DATA rr TYPE string.
        ENDIF.
      CATCH cx_sy_create_data_error.
        CREATE DATA rr TYPE string.
    ENDTRY.
  ENDMETHOD.

  METHOD to_time.
    " 9:00, 09:00, 0900, 09:00:00, 090000 - and an Excel time, which is a
    " fraction of a day (0.375 is 09:00).
    DATA(lv) = condense( iv_in ).
    IF lv CO '0123456789.' AND lv CS '.'.
      TRY.
          DATA(lv_f) = CONV decfloat34( lv ).
          IF lv_f < 1.
            DATA(lv_s) = CONV i( round( val = lv_f * 86400 dec = 0 ) ).
            rv = |{ lv_s DIV 3600 WIDTH = 2 ALIGN = RIGHT PAD = '0' }| &&
                 |{ ( lv_s MOD 3600 ) DIV 60 WIDTH = 2 ALIGN = RIGHT PAD = '0' }| &&
                 |{ lv_s MOD 60 WIDTH = 2 ALIGN = RIGHT PAD = '0' }|.
            RETURN.
          ENDIF.
        CATCH cx_sy_conversion_error.
          RETURN.
      ENDTRY.
    ENDIF.
    IF lv CS ':'.
      SPLIT lv AT ':' INTO DATA(lv_h) DATA(lv_m) DATA(lv_x).
    ELSEIF strlen( lv ) = 4 OR strlen( lv ) = 6.
      lv_h = lv(2).
      lv_m = lv+2(2).
      lv_x = COND #( WHEN strlen( lv ) = 6 THEN lv+4(2) ).
    ELSE.
      RETURN.
    ENDIF.
    IF lv_x IS INITIAL.
      lv_x = `00`.
    ENDIF.
    IF lv_h CN '0123456789' OR lv_m CN '0123456789' OR lv_x CN '0123456789'
       OR strlen( lv_h ) > 2 OR strlen( lv_m ) <> 2 OR strlen( lv_x ) <> 2.
      RETURN.
    ENDIF.
    IF CONV i( lv_h ) > 23 OR CONV i( lv_m ) > 59 OR CONV i( lv_x ) > 59.
      RETURN.
    ENDIF.
    rv = |{ lv_h WIDTH = 2 ALIGN = RIGHT PAD = '0' }{ lv_m }{ lv_x }|.
  ENDMETHOD.

  METHOD to_int.
    CLEAR ev_err.
    er_val = new_data( is_f ).
    ASSIGN er_val->* TO FIELD-SYMBOL(<lv>).
    DATA(lv_in) = COND string( WHEN is_f-inttype = 'g' THEN iv_in ELSE condense( iv_in ) ).
    IF lv_in IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        CASE is_f-inttype.
          WHEN 'g'.
            <lv> = lv_in.

          WHEN 'D'.
            DATA(lv_d) = lcl_util=>to_date( lv_in ).
            IF lv_d IS INITIAL.
              ev_err = |"{ lv_in }" is not a date - write it as YYYY/MM/DD|.
              RETURN.
            ENDIF.
            <lv> = lv_d.

          WHEN 'T'.
            DATA(lv_t) = to_time( lv_in ).
            IF lv_t IS INITIAL.
              ev_err = |"{ lv_in }" is not a time - write it as HH:MM|.
              RETURN.
            ENDIF.
            <lv> = lv_t.

          WHEN 'P' OR 'I' OR 'F' OR 'b' OR 's' OR '8' OR 'a' OR 'e'.
            DATA(lv_num) = lv_in.
            REPLACE ALL OCCURRENCES OF `,` IN lv_num WITH ``.
            IF lv_num CN '0123456789.-+ E'.
              ev_err = |"{ lv_in }" is not a number|.
              RETURN.
            ENDIF.
            <lv> = lv_num.

          WHEN 'N'.
            IF lv_in CN '0123456789'.
              ev_err = |"{ lv_in }" may only contain digits|.
              RETURN.
            ENDIF.
            IF is_f-leng > 0 AND strlen( lv_in ) > is_f-leng.
              ev_err = |"{ lv_in }" is longer than { is_f-leng } digits|.
              RETURN.
            ENDIF.
            <lv> = lv_in.

          WHEN OTHERS.
            DATA(lv_c) = lv_in.
            " A word in a one character field is a flag written out in full:
            " Yes, True, X.
            IF is_f-leng = 1 AND strlen( lv_c ) > 1 AND is_f-convexit IS INITIAL.
              lv_c = lcl_util=>flag( lv_c ).
            ENDIF.
            IF is_f-lower = abap_false.
              lv_c = to_upper( lv_c ).
            ENDIF.
            IF is_f-convexit IS NOT INITIAL.
              DATA(lv_fm) = |CONVERSION_EXIT_{ is_f-convexit }_INPUT|.
              TRY.
                  CALL FUNCTION lv_fm
                    EXPORTING  input  = lv_c
                    IMPORTING  output = <lv>
                    EXCEPTIONS OTHERS = 1.
                  IF sy-subrc <> 0.
                    ev_err = |"{ lv_in }" is not a valid value for { is_f-name }|.
                    RETURN.
                  ENDIF.
                CATCH cx_sy_dyn_call_error.
                  <lv> = lv_c.
              ENDTRY.
            ELSE.
              IF is_f-leng > 0 AND strlen( lv_c ) > is_f-leng.
                ev_err = |"{ lv_in }" is longer than the { is_f-leng } characters { is_f-name } holds|.
                RETURN.
              ENDIF.
              <lv> = lv_c.
            ENDIF.
        ENDCASE.
      CATCH cx_sy_conversion_error cx_sy_arithmetic_error.
        ev_err = |"{ lv_in }" does not fit { is_f-name }|.
    ENDTRY.
  ENDMETHOD.

  METHOD to_ext.
    IF iv_val IS INITIAL.
      RETURN.
    ENDIF.
    CASE is_f-inttype.
      WHEN 'g'.
        rv = iv_val.
      WHEN 'D'.
        DATA(lv_d) = CONV d( iv_val ).
        rv = |{ lv_d(4) }/{ lv_d+4(2) }/{ lv_d+6(2) }|.
      WHEN 'T'.
        DATA(lv_t) = CONV t( iv_val ).
        rv = |{ lv_t(2) }:{ lv_t+2(2) }|.
        IF lv_t+4(2) <> '00'.
          rv = |{ rv }:{ lv_t+4(2) }|.
        ENDIF.
      WHEN 'P' OR 'I' OR 'F' OR 'b' OR 's' OR '8' OR 'a' OR 'e'.
        DATA lv_num TYPE decfloat34.
        lv_num = iv_val.
        rv = |{ lv_num NUMBER = RAW }|.
      WHEN OTHERS.
        IF is_f-convexit IS NOT INITIAL.
          DATA(lv_fm) = |CONVERSION_EXIT_{ is_f-convexit }_OUTPUT|.
          DATA lv_out TYPE c LENGTH 255.
          TRY.
              CALL FUNCTION lv_fm
                EXPORTING  input  = iv_val
                IMPORTING  output = lv_out
                EXCEPTIONS OTHERS = 1.
              IF sy-subrc = 0.
                rv = condense( lv_out ).
                RETURN.
              ENDIF.
            CATCH cx_sy_dyn_call_error.
              CLEAR lv_out.
          ENDTRY.
        ENDIF.
        rv = condense( CONV string( iv_val ) ).
    ENDCASE.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* LCL_CHK - is the value allowed?
*   The field's check table, read through its foreign key in the ABAP
*   Dictionary (DD05S) so that a key with more than one field - a region
*   checked together with its country - is checked as SAP checks it; and
*   the fixed values of its domain. Answers are kept, so a value used on a
*   thousand rows is looked up once.
*----------------------------------------------------------------------*
CLASS lcl_chk DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_ctx,
             fld TYPE fieldname,
             val TYPE string,
           END OF ty_ctx,
           tt_ctx TYPE HASHED TABLE OF ty_ctx WITH UNIQUE KEY fld.
    " IT_CTX holds the other fields of the same record (internal values),
    " for foreign keys with more than one field.
    CLASS-METHODS check
      IMPORTING is_f          TYPE ty_fld
                VALUE(iv_val) TYPE string
                it_ctx        TYPE tt_ctx
      RETURNING VALUE(rv) TYPE string.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_fk,
             checktable TYPE tabname,
             checkfield TYPE fieldname,
             fortable   TYPE tabname,
             forkey     TYPE fieldname,
           END OF ty_fk,
           tt_fk TYPE STANDARD TABLE OF ty_fk WITH EMPTY KEY.
    TYPES: BEGIN OF ty_fkc,
             tab TYPE tabname,
             fld TYPE fieldname,
             fk  TYPE tt_fk,
           END OF ty_fkc.
    CLASS-DATA mt_fkc TYPE HASHED TABLE OF ty_fkc WITH UNIQUE KEY tab fld.
    TYPES: BEGIN OF ty_hit,
             key TYPE string,
             ok  TYPE abap_bool,
           END OF ty_hit.
    CLASS-DATA mt_hit TYPE HASHED TABLE OF ty_hit WITH UNIQUE KEY key.
    TYPES: BEGIN OF ty_dv,
             domname TYPE domname,
             low     TYPE string,
             high    TYPE string,
           END OF ty_dv.
    CLASS-DATA mt_dv  TYPE SORTED TABLE OF ty_dv WITH NON-UNIQUE KEY domname.
    CLASS-DATA mt_dvr TYPE SORTED TABLE OF domname WITH UNIQUE KEY table_line.
    CLASS-METHODS fk
      IMPORTING iv_tab    TYPE tabname
                iv_fld    TYPE fieldname
      RETURNING VALUE(rt) TYPE tt_fk.
ENDCLASS.


CLASS lcl_chk IMPLEMENTATION.

  METHOD fk.
    READ TABLE mt_fkc INTO DATA(ls_c) WITH TABLE KEY tab = iv_tab fld = iv_fld.
    IF sy-subrc = 0.
      rt = ls_c-fk.
      RETURN.
    ENDIF.
    " Through the dictionary's own API, not a SELECT on DD05S: the check
    " table is not a column of DD05S on S/4HANA 2025 ("Unknown column name
    " CHECKTABLE"). DD05M, which DDIF_FK_GET fills, carries both sides of
    " every key field pair together with the check table.
    DATA: lv_tab TYPE ddobjname,
          lv_fld TYPE dfies-fieldname,
          lt_05m TYPE STANDARD TABLE OF dd05m,
          lt_08v TYPE STANDARD TABLE OF dd08v.
    lv_tab = iv_tab.
    lv_fld = iv_fld.
    CALL FUNCTION 'DDIF_FK_GET'
      EXPORTING
        tabname       = lv_tab
        fieldname     = lv_fld
        state         = 'A'
      TABLES
        dd08v_tab     = lt_08v
        dd05m_tab     = lt_05m
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc = 0.
      SORT lt_05m BY primpos.
      LOOP AT lt_05m INTO DATA(ls_05m).
        APPEND VALUE #( checktable = ls_05m-checktable checkfield = ls_05m-checkfield
                        fortable   = ls_05m-fortable   forkey     = ls_05m-forkey ) TO rt.
      ENDLOOP.
    ENDIF.
    INSERT VALUE #( tab = iv_tab fld = iv_fld fk = rt ) INTO TABLE mt_fkc.
  ENDMETHOD.

  METHOD check.
    IF iv_val IS INITIAL OR is_f-inttype = 'g'.
      RETURN.
    ENDIF.

    " ---- fixed values of the domain
    IF is_f-valexi = abap_true AND is_f-domname IS NOT INITIAL.
      IF NOT line_exists( mt_dvr[ table_line = is_f-domname ] ).
        SELECT domvalue_l, domvalue_h FROM dd07l
          WHERE domname = @is_f-domname AND as4local = 'A'
          INTO TABLE @DATA(lt_dv).
        LOOP AT lt_dv INTO DATA(ls_dv).
          INSERT VALUE #( domname = is_f-domname low = condense( CONV string( ls_dv-domvalue_l ) )
                          high = condense( CONV string( ls_dv-domvalue_h ) ) ) INTO TABLE mt_dv.
        ENDLOOP.
        INSERT is_f-domname INTO TABLE mt_dvr.
      ENDIF.
      DATA lv_found TYPE abap_bool.
      DATA lv_list  TYPE string.
      DATA(lv_v) = condense( iv_val ).
      LOOP AT mt_dv INTO DATA(ls_v) WHERE domname = is_f-domname.
        IF ( ls_v-high IS INITIAL AND lv_v = ls_v-low )
           OR ( ls_v-high IS NOT INITIAL AND lv_v >= ls_v-low AND lv_v <= ls_v-high ).
          lv_found = abap_true.
          EXIT.
        ENDIF.
        lv_list = COND #( WHEN lv_list IS INITIAL THEN ls_v-low ELSE |{ lv_list }, { ls_v-low }| ).
      ENDLOOP.
      IF lv_found = abap_false AND line_exists( mt_dv[ domname = is_f-domname ] ).
        rv = |"{ lv_v }" is not allowed for { is_f-name } - allowed: { lv_list }|.
        RETURN.
      ENDIF.
    ENDIF.

    " ---- check table, through the foreign key
    IF is_f-checktab IS INITIAL OR is_f-fld IS INITIAL OR is_f-tab = 'STXH'
       OR is_f-inttype NA 'CN'.
      RETURN.
    ENDIF.
    DATA(lt_fk) = fk( iv_tab = is_f-tab iv_fld = is_f-fld ).
    IF lt_fk IS INITIAL.
      RETURN.
    ENDIF.
    DATA lt_where TYPE string_table.
    DATA lv_key   TYPE string.
    LOOP AT lt_fk INTO DATA(ls_fk).
      DATA lv_cv TYPE string.
      CLEAR lv_cv.
      IF ls_fk-checkfield = 'MANDT' OR ls_fk-checkfield = 'CLIENT' OR ls_fk-forkey = 'MANDT'.
        CONTINUE.
      ELSEIF ls_fk-fortable = '*'.
        " A constant in the foreign key, written with its quotes.
        lv_cv = ls_fk-forkey.
        REPLACE ALL OCCURRENCES OF `'` IN lv_cv WITH ``.
      ELSEIF ls_fk-forkey = is_f-fld.
        lv_cv = iv_val.
      ELSE.
        READ TABLE it_ctx INTO DATA(ls_ctx) WITH TABLE KEY fld = ls_fk-forkey.
        IF sy-subrc <> 0.
          " The other half of the key is not known here - SAP checks it
          " when the record is posted.
          RETURN.
        ENDIF.
        lv_cv = ls_ctx-val.
      ENDIF.
      REPLACE ALL OCCURRENCES OF `'` IN lv_cv WITH `''`.
      APPEND |{ ls_fk-checkfield } = '{ lv_cv }'| TO lt_where.
      lv_key = |{ lv_key }/{ ls_fk-checkfield }={ lv_cv }|.
    ENDLOOP.
    IF lt_where IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lv_tab) = lt_fk[ 1 ]-checktable.
    lv_key = |{ lv_tab }{ lv_key }|.
    READ TABLE mt_hit INTO DATA(ls_hit) WITH TABLE KEY key = lv_key.
    IF sy-subrc <> 0.
      DATA lv_cond TYPE string.
      CLEAR lv_cond.
      LOOP AT lt_where INTO DATA(lv_w).
        lv_cond = COND #( WHEN lv_cond IS INITIAL THEN lv_w ELSE |{ lv_cond } AND { lv_w }| ).
      ENDLOOP.
      ls_hit-key = lv_key.
      TRY.
          SELECT SINGLE @abap_true FROM (lv_tab) WHERE (lv_cond) INTO @ls_hit-ok.
        CATCH cx_sy_dynamic_osql_error.
          ls_hit-ok = abap_true.
      ENDTRY.
      INSERT ls_hit INTO TABLE mt_hit.
    ENDIF.
    IF ls_hit-ok = abap_false.
      rv = |"{ iv_val }" is not allowed for { is_f-name } - it is not in { lv_tab }|.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* LCL_CFG - what the system says about a number, a group or a role
*----------------------------------------------------------------------*
CLASS lcl_cfg DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES tt_role TYPE STANDARD TABLE OF bu_role WITH EMPTY KEY.
    CLASS-METHODS guid_of
      IMPORTING VALUE(iv_num) TYPE @@NUMTYPE@@
      RETURNING VALUE(rv)     TYPE bu_partner_guid.
    CLASS-METHODS bp_of
      IMPORTING VALUE(iv_num) TYPE @@NUMTYPE@@
      RETURNING VALUE(rv)     TYPE bu_partner.
    " Business partners given on the screen -> their customer / supplier.
    CLASS-METHODS nums_of_bp
      IMPORTING it_bp         TYPE STANDARD TABLE
      RETURNING VALUE(rt)     TYPE string_table.
    CLASS-METHODS num_by_guid
      IMPORTING VALUE(iv_guid) TYPE bu_partner_guid
      RETURNING VALUE(rv)      TYPE string.
    CLASS-METHODS bp_by_guid
      IMPORTING VALUE(iv_guid) TYPE bu_partner_guid
      RETURNING VALUE(rv)      TYPE bu_partner.
    CLASS-METHODS ok_ktogr
      IMPORTING iv            TYPE clike
      RETURNING VALUE(rv)     TYPE abap_bool.
    " CVI customising: the grouping and the roles an account group creates.
    CLASS-METHODS bp_group
      IMPORTING iv_ktogr      TYPE clike
      RETURNING VALUE(rv)     TYPE bu_group.
    CLASS-METHODS bp_roles
      IMPORTING iv_ktogr      TYPE clike
      RETURNING VALUE(rt)     TYPE tt_role.
    CLASS-METHODS has_role
      IMPORTING VALUE(iv_bp)   TYPE bu_partner
                VALUE(iv_role) TYPE bu_role
      RETURNING VALUE(rv)      TYPE abap_bool.
    " Partner type of a partner function: KU, LI, PE, AP.
    CLASS-METHODS nrart
      IMPORTING VALUE(iv_parvw) TYPE parvw
      RETURNING VALUE(rv)       TYPE nrart.
ENDCLASS.


CLASS lcl_cfg IMPLEMENTATION.

  METHOD guid_of.
    IF gc_side = 'C'.
      SELECT SINGLE partner_guid FROM cvi_cust_link WHERE customer = @iv_num INTO @rv.
    ELSE.
      SELECT SINGLE partner_guid FROM cvi_vend_link WHERE vendor = @iv_num INTO @rv.
    ENDIF.
  ENDMETHOD.

  METHOD bp_of.
    DATA(lv_g) = guid_of( iv_num ).
    IF lv_g IS NOT INITIAL.
      SELECT SINGLE partner FROM but000 WHERE partner_guid = @lv_g INTO @rv.
    ENDIF.
  ENDMETHOD.

  METHOD nums_of_bp.
    IF it_bp IS INITIAL.
      RETURN.
    ENDIF.
    DATA lt_bp TYPE STANDARD TABLE OF bu_partner WITH EMPTY KEY.
    LOOP AT it_bp ASSIGNING FIELD-SYMBOL(<lv_bp>).
      APPEND CONV bu_partner( <lv_bp> ) TO lt_bp.
    ENDLOOP.
    SELECT partner_guid FROM but000
      FOR ALL ENTRIES IN @lt_bp WHERE partner = @lt_bp-table_line
      INTO TABLE @DATA(lt_g).
    IF lt_g IS INITIAL.
      RETURN.
    ENDIF.
    IF gc_side = 'C'.
      " A typed table, not @DATA( ): one column read into an inline table
      " gives a structured line, which a string template cannot take.
      DATA lt_c TYPE STANDARD TABLE OF kunnr WITH EMPTY KEY.
      SELECT customer FROM cvi_cust_link
        FOR ALL ENTRIES IN @lt_g WHERE partner_guid = @lt_g-partner_guid
        INTO TABLE @lt_c.
      LOOP AT lt_c INTO DATA(lv_cust).
        APPEND |{ lv_cust }| TO rt.
      ENDLOOP.
    ELSE.
      DATA lt_v TYPE STANDARD TABLE OF lifnr WITH EMPTY KEY.
      SELECT vendor FROM cvi_vend_link
        FOR ALL ENTRIES IN @lt_g WHERE partner_guid = @lt_g-partner_guid
        INTO TABLE @lt_v.
      LOOP AT lt_v INTO DATA(lv_vend).
        APPEND |{ lv_vend }| TO rt.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD num_by_guid.
    IF gc_side = 'C'.
      SELECT SINGLE customer FROM cvi_cust_link WHERE partner_guid = @iv_guid INTO @DATA(lv_c).
      rv = lv_c.
    ELSE.
      SELECT SINGLE vendor FROM cvi_vend_link WHERE partner_guid = @iv_guid INTO @DATA(lv_v).
      rv = lv_v.
    ENDIF.
  ENDMETHOD.

  METHOD bp_by_guid.
    SELECT SINGLE partner FROM but000 WHERE partner_guid = @iv_guid INTO @rv.
  ENDMETHOD.

  METHOD ok_ktogr.
    IF gc_side = 'C'.
      SELECT SINGLE @abap_true FROM t077d WHERE ktokd = @iv INTO @rv.
    ELSE.
      SELECT SINGLE @abap_true FROM t077k WHERE ktokk = @iv INTO @rv.
    ENDIF.
  ENDMETHOD.

  METHOD bp_group.
    IF gc_side = 'C'.
      SELECT SINGLE grouping FROM cvic_cust_to_bp1 WHERE account_group = @iv_ktogr INTO @rv.
    ELSE.
      SELECT SINGLE grouping FROM cvic_vend_to_bp1 WHERE account_group = @iv_ktogr INTO @rv.
    ENDIF.
  ENDMETHOD.

  METHOD bp_roles.
    IF gc_side = 'C'.
      SELECT role FROM cvic_cust_to_bp2 WHERE account_group = @iv_ktogr INTO TABLE @rt.
    ELSE.
      SELECT role FROM cvic_vend_to_bp2 WHERE account_group = @iv_ktogr INTO TABLE @rt.
    ENDIF.
  ENDMETHOD.

  METHOD has_role.
    IF iv_bp IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE @abap_true FROM but100
      WHERE partner = @iv_bp AND rltyp = @iv_role INTO @rv.
  ENDMETHOD.

  METHOD nrart.
    SELECT SINGLE nrart FROM tpar WHERE parvw = @iv_parvw INTO @rv.
  ENDMETHOD.

ENDCLASS.

