*&---------------------------------------------------------------------*
*& Class  ZCL_OVL_VENDOR_SYNC
*&---------------------------------------------------------------------*
*& Business logic of the ONGC -> OVL Employee Vendor (VEMP) sync
*& (Interface OVL-INT-VEN-001). Called by ZCL_ZOVL_VENDOR_SYNC_DPC for
*& every vendor CPI posts; can also be reused for the initial load.
*&
*& UPSERT, one vendor per call:
*&   - BP / vendor LIFNR does NOT exist in OVL -> CREATE
*&       BP with the SAME number as the ONGC vendor (external number,
*&       BP grouping VEMP), role FLVN00, address, bank details, vendor
*&       node (account group VEMP, PAN) + company code OVL.
*&   - exists                                  -> UPDATE
*&       same nodes, changed values overwrite the OVL values.
*&
*& Built on the CVI pattern already working in OVL (ZGEM_CREATE_VENDOR):
*& one CVIS_EI_EXTERN (partner + vendor node) -> CVI_EI_INBOUND_MAIN.
*& Bank details and address are BP-owned under CVI, so they are set on
*& the partner node; CVI derives LFA1 / LFBK from them.
*&
*& Payment terms are NOT taken from ONGC: OVL default (TVARVC
*& ZOVL_VENSYNC_ZTERM, fallback '0001').
*&
*& Settings (STVARV, type P - all optional, defaults in brackets):
*&   ZOVL_VENSYNC_BUKRS     target company code            ['OVL']
*&   ZOVL_VENSYNC_GROUPING  BP grouping                    ['VEMP']
*&   ZOVL_VENSYNC_KTOKK     vendor account group           ['VEMP']
*&   ZOVL_VENSYNC_ZTERM     default payment terms          ['0001']
*&   ZOVL_VENSYNC_AKONT     recon. account if the ONGC one
*&                          does not exist in OVL          [none]
*&   ZOVL_VENSYNC_BPCAT     BP category if not sent        ['1' person]
*&---------------------------------------------------------------------*
CLASS zcl_ovl_vendor_sync DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS gc_action_create TYPE char10 VALUE 'CREATE'.
    CONSTANTS gc_action_update TYPE char10 VALUE 'UPDATE'.
    CONSTANTS gc_role_vendor   TYPE bu_role VALUE 'FLVN00'.

    TYPES: BEGIN OF ty_result,
             lifnr    TYPE lifnr,
             action   TYPE char10,       " CREATE / UPDATE
             status   TYPE bapi_mtype,   " S / E
             message  TYPE bapi_msg,     " leading message
             messages TYPE bapiret2_t,   " all messages (errors + warnings)
           END OF ty_result.

    METHODS constructor.

    "! Create or update one employee vendor (BP + LFA1/LFB1/LFBK).
    "! Commits on success, rolls back on error - never raises.
    METHODS upsert
      IMPORTING is_vendor        TYPE zcl_zovl_vendor_sync_mpc=>ty_vendor
                it_company       TYPE zcl_zovl_vendor_sync_mpc=>tt_company
                it_bank          TYPE zcl_zovl_vendor_sync_mpc=>tt_bank
      RETURNING VALUE(rs_result) TYPE ty_result.

    "! Write the result to ZOVL_VEN_SYNCLOG (own commit).
    METHODS write_log
      IMPORTING is_result TYPE ty_result.

  PRIVATE SECTION.
    DATA mv_bukrs    TYPE bukrs.
    DATA mv_grouping TYPE bu_group.
    DATA mv_ktokk    TYPE ktokk.
    DATA mv_zterm    TYPE dzterm.
    DATA mv_akont    TYPE akont.
    DATA mv_bpcat    TYPE bu_type.

    DATA mt_messages TYPE bapiret2_t.

    METHODS read_param
      IMPORTING iv_name         TYPE rvari_vnam
                iv_default      TYPE clike
      RETURNING VALUE(rv_value) TYPE rvari_val_255.

    METHODS add_message
      IMPORTING iv_type TYPE bapi_mtype
                iv_text TYPE string.

    METHODS has_error
      RETURNING VALUE(rv_error) TYPE abap_bool.

    METHODS validate
      IMPORTING is_vendor TYPE zcl_zovl_vendor_sync_mpc=>ty_vendor
                it_bank   TYPE zcl_zovl_vendor_sync_mpc=>tt_bank.

    METHODS conv_language
      IMPORTING iv_spras        TYPE clike
      RETURNING VALUE(rv_langu) TYPE spras.

    METHODS get_title_key
      IMPORTING iv_anred        TYPE clike
      RETURNING VALUE(rv_title) TYPE ad_title.

    METHODS get_recon_account
      IMPORTING iv_akont        TYPE clike
      RETURNING VALUE(rv_akont) TYPE akont.

    METHODS fill_partner
      IMPORTING is_vendor   TYPE zcl_zovl_vendor_sync_mpc=>ty_vendor
                it_bank     TYPE zcl_zovl_vendor_sync_mpc=>tt_bank
                iv_partner  TYPE bu_partner
                iv_create   TYPE abap_bool
                iv_bp_guid  TYPE bu_partner_guid
                iv_category TYPE bu_type
      CHANGING  cs_cvis     TYPE cvis_ei_extern.

    METHODS fill_vendor
      IMPORTING is_vendor  TYPE zcl_zovl_vendor_sync_mpc=>ty_vendor
                it_company TYPE zcl_zovl_vendor_sync_mpc=>tt_company
                iv_lifnr   TYPE lifnr
                iv_create  TYPE abap_bool
      CHANGING  cs_cvis    TYPE cvis_ei_extern.
ENDCLASS.


CLASS zcl_ovl_vendor_sync IMPLEMENTATION.

  METHOD constructor.
    mv_bukrs    = read_param( iv_name = 'ZOVL_VENSYNC_BUKRS'    iv_default = 'OVL' ).
    mv_grouping = read_param( iv_name = 'ZOVL_VENSYNC_GROUPING' iv_default = 'VEMP' ).
    mv_ktokk    = read_param( iv_name = 'ZOVL_VENSYNC_KTOKK'    iv_default = 'VEMP' ).
    mv_zterm    = read_param( iv_name = 'ZOVL_VENSYNC_ZTERM'    iv_default = '0001' ).
    mv_bpcat    = read_param( iv_name = 'ZOVL_VENSYNC_BPCAT'    iv_default = '1' ).
    DATA(lv_akont) = read_param( iv_name = 'ZOVL_VENSYNC_AKONT' iv_default = '' ).
    IF lv_akont IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_akont(10)
        IMPORTING
          output = mv_akont.
    ENDIF.
  ENDMETHOD.


  METHOD upsert.
    DATA: lt_cvis     TYPE cvis_ei_extern_t,
          ls_cvis     TYPE cvis_ei_extern,
          lt_return   TYPE bapiretm,
          lv_partner  TYPE bu_partner,
          lv_lifnr    TYPE lifnr,
          lv_bp_guid  TYPE bu_partner_guid,
          lv_category TYPE bu_type,
          lv_actvt    TYPE activ_auth,
          lv_create   TYPE abap_bool.

    CLEAR mt_messages.

    "--------------------------------------------------------------
    " 1. Number: OVL BP / vendor number = ONGC vendor number
    "--------------------------------------------------------------
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = is_vendor-lifnr
      IMPORTING
        output = lv_partner.
    lv_lifnr = lv_partner.
    rs_result-lifnr = lv_lifnr.

    validate( is_vendor = is_vendor it_bank = it_bank ).

    "--------------------------------------------------------------
    " 2. Create or update?
    "--------------------------------------------------------------
    SELECT SINGLE partner_guid, type FROM but000
      WHERE partner = @lv_partner
      INTO (@lv_bp_guid, @lv_category).
    IF sy-subrc <> 0.
      lv_create = abap_true.
      rs_result-action = gc_action_create.
      lv_category = COND #( WHEN is_vendor-bp_category IS NOT INITIAL
                            THEN is_vendor-bp_category ELSE mv_bpcat ).
      IF lv_category <> '1' AND lv_category <> '2'.
        add_message( iv_type = 'E' iv_text = |BP category '{ lv_category }' not supported (1 = person, 2 = organization).| ).
      ENDIF.
      TRY.
          lv_bp_guid = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error.
          add_message( iv_type = 'E' iv_text = |Could not generate the BP GUID.| ).
      ENDTRY.
    ELSE.
      rs_result-action = gc_action_update.
    ENDIF.

    lv_actvt = COND #( WHEN lv_create = abap_true THEN '01' ELSE '02' ).
    AUTHORITY-CHECK OBJECT 'F_LFA1_BUK'
      ID 'ACTVT' FIELD lv_actvt
      ID 'BUKRS' FIELD mv_bukrs.
    IF sy-subrc <> 0.
      add_message( iv_type = 'E' iv_text = |No authorization (F_LFA1_BUK) to maintain vendors in company code { mv_bukrs }.| ).
    ENDIF.

    IF has_error( ) = abap_false.
      "------------------------------------------------------------
      " 3. Build the CVI structure (partner + vendor node)
      "------------------------------------------------------------
      fill_partner( EXPORTING is_vendor   = is_vendor
                              it_bank     = it_bank
                              iv_partner  = lv_partner
                              iv_create   = lv_create
                              iv_bp_guid  = lv_bp_guid
                              iv_category = lv_category
                    CHANGING  cs_cvis     = ls_cvis ).
      fill_vendor( EXPORTING is_vendor  = is_vendor
                             it_company = it_company
                             iv_lifnr   = lv_lifnr
                             iv_create  = lv_create
                   CHANGING  cs_cvis    = ls_cvis ).
    ENDIF.

    IF has_error( ) = abap_false.
      "------------------------------------------------------------
      " 4. Create / change BP + vendor in one call
      "------------------------------------------------------------
      APPEND ls_cvis TO lt_cvis.
      CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
        EXPORTING
          i_data   = lt_cvis
        IMPORTING
          e_return = lt_return.

      LOOP AT lt_return INTO DATA(ls_return).
        LOOP AT ls_return-object_msg INTO DATA(ls_msg)
             WHERE type = 'E' OR type = 'A' OR type = 'W'.
          APPEND CORRESPONDING bapiret2( ls_msg ) TO mt_messages.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    "--------------------------------------------------------------
    " 5. Commit / rollback + result
    "--------------------------------------------------------------
    IF has_error( ) = abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      rs_result-status = 'E'.
      LOOP AT mt_messages INTO DATA(ls_err) WHERE type = 'E' OR type = 'A'.
        rs_result-message = |{ lv_lifnr ALPHA = OUT }: { ls_err-message }|.
        EXIT.
      ENDLOOP.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
      SELECT SINGLE @abap_true FROM lfb1
        WHERE lifnr = @lv_lifnr AND bukrs = @mv_bukrs
        INTO @DATA(lv_exists).
      IF lv_exists = abap_true.
        rs_result-status  = 'S'.
        rs_result-message = |Vendor { lv_lifnr ALPHA = OUT } { COND string( WHEN lv_create = abap_true
                                                                            THEN `created` ELSE `updated` ) } | &&
                            |in company code { mv_bukrs }.|.
      ELSE.
        rs_result-status  = 'E'.
        rs_result-message = |Vendor { lv_lifnr ALPHA = OUT }: no error returned, but LFB1 { mv_bukrs } not found | &&
                            |after commit - check the CVI settings for grouping { mv_grouping }.|.
      ENDIF.
      add_message( iv_type = rs_result-status iv_text = CONV #( rs_result-message ) ).
    ENDIF.

    rs_result-messages = mt_messages.
  ENDMETHOD.


  METHOD validate.
    IF is_vendor-lifnr IS INITIAL.
      add_message( iv_type = 'E' iv_text = |LIFNR (vendor number) is mandatory.| ).
    ENDIF.
    IF is_vendor-name1 IS INITIAL AND is_vendor-name_last IS INITIAL.
      add_message( iv_type = 'E' iv_text = |NAME1 is mandatory.| ).
    ENDIF.
    IF is_vendor-land1 IS INITIAL.
      add_message( iv_type = 'E' iv_text = |LAND1 (country) is mandatory.| ).
    ENDIF.

    " Bank keys must exist in the OVL bank master (FI01), otherwise CVI
    " rejects the whole vendor with a less readable message.
    LOOP AT it_bank INTO DATA(ls_bank) WHERE bankl IS NOT INITIAL.
      SELECT SINGLE @abap_true FROM bnka
        WHERE banks = @ls_bank-banks AND bankl = @ls_bank-bankl
        INTO @DATA(lv_bank_ok).
      IF lv_bank_ok = abap_false.
        add_message( iv_type = 'E'
                     iv_text = |Bank key { ls_bank-banks }/{ ls_bank-bankl } does not exist in OVL (maintain in FI01).| ).
      ENDIF.
      CLEAR lv_bank_ok.
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_partner.
    DATA lv_bkvid TYPE bu_bkvid.

    "--- header -----------------------------------------------------
    cs_cvis-partner-header-object      = 'BusinessPartner'.
    cs_cvis-partner-header-object_task = COND #( WHEN iv_create = abap_true THEN 'I' ELSE 'U' ).
    cs_cvis-partner-header-object_instance-bpartner     = iv_partner.
    cs_cvis-partner-header-object_instance-bpartnerguid = iv_bp_guid.

    "--- central data -----------------------------------------------
    IF iv_create = abap_true.
      " Grouping VEMP must have an EXTERNAL number range so the ONGC
      " number is accepted; CVI gives the vendor the same number.
      cs_cvis-partner-central_data-common-data-bp_control-category = iv_category.
      cs_cvis-partner-central_data-common-data-bp_control-grouping = mv_grouping.
    ENDIF.

    IF iv_category = '1'.
      " Person: first / last name from ONGC BUT000 when sent, else NAME1
      cs_cvis-partner-central_data-common-data-bp_person-firstname  = is_vendor-name_first.
      cs_cvis-partner-central_data-common-data-bp_person-lastname   =
        COND #( WHEN is_vendor-name_last IS NOT INITIAL THEN is_vendor-name_last ELSE is_vendor-name1 ).
      cs_cvis-partner-central_data-common-datax-bp_person-firstname = abap_true.
      cs_cvis-partner-central_data-common-datax-bp_person-lastname  = abap_true.
      IF is_vendor-gbort IS NOT INITIAL.
        cs_cvis-partner-central_data-common-data-bp_person-birthplace  = is_vendor-gbort.
        cs_cvis-partner-central_data-common-datax-bp_person-birthplace = abap_true.
      ENDIF.
      cs_cvis-partner-central_data-common-data-bp_person-correspondlanguage  = conv_language( is_vendor-spras ).
      cs_cvis-partner-central_data-common-datax-bp_person-correspondlanguage = abap_true.
    ELSE.
      cs_cvis-partner-central_data-common-data-bp_organization-name1  = is_vendor-name1.
      cs_cvis-partner-central_data-common-datax-bp_organization-name1 = abap_true.
    ENDIF.

    " Search term -> LFA1-SORTL (required): SORTL, else name
    cs_cvis-partner-central_data-common-data-bp_centraldata-searchterm1 =
      COND #( WHEN is_vendor-sortl IS NOT INITIAL THEN is_vendor-sortl ELSE is_vendor-name1 ).
    cs_cvis-partner-central_data-common-datax-bp_centraldata-searchterm1 = abap_true.

    cs_cvis-partner-central_data-common-data-bp_centraldata-partnerlanguage  = conv_language( is_vendor-spras ).
    cs_cvis-partner-central_data-common-datax-bp_centraldata-partnerlanguage = abap_true.

    DATA(lv_title) = get_title_key( is_vendor-anred ).
    IF lv_title IS NOT INITIAL.
      cs_cvis-partner-central_data-common-data-bp_centraldata-title_key  = lv_title.
      cs_cvis-partner-central_data-common-datax-bp_centraldata-title_key = abap_true.
    ENDIF.

    "--- supplier role FLVN00 (only if missing) ----------------------
    SELECT SINGLE @abap_true FROM but100
      WHERE partner = @iv_partner AND rltyp = @gc_role_vendor
      INTO @DATA(lv_has_role).
    IF lv_has_role = abap_false.
      APPEND INITIAL LINE TO cs_cvis-partner-central_data-role-roles ASSIGNING FIELD-SYMBOL(<ls_role>).
      <ls_role>-task              = 'I'.
      <ls_role>-data_key          = gc_role_vendor.
      <ls_role>-data-rolecategory = gc_role_vendor.
    ENDIF.

    "--- standard address ---------------------------------------------
    DATA lv_addr_guid TYPE bu_address_guid.
    DATA lv_addrnr    TYPE ad_addrnum.
    IF iv_create = abap_false.
      " Address with usage XXDEFAULT, else the first address of the BP
      SELECT addrnumber FROM but021_fs
        WHERE partner = @iv_partner AND adr_kind = 'XXDEFAULT'
        ORDER BY valid_to DESCENDING
        INTO @lv_addrnr
        UP TO 1 ROWS.
      ENDSELECT.
      IF lv_addrnr IS NOT INITIAL.
        SELECT SINGLE address_guid FROM but020
          WHERE partner = @iv_partner AND addrnumber = @lv_addrnr
          INTO @lv_addr_guid.
      ENDIF.
      IF lv_addr_guid IS INITIAL.
        SELECT address_guid FROM but020
          WHERE partner = @iv_partner
          ORDER BY addrnumber
          INTO @lv_addr_guid
          UP TO 1 ROWS.
        ENDSELECT.
      ENDIF.
    ENDIF.

    APPEND INITIAL LINE TO cs_cvis-partner-central_data-address-addresses ASSIGNING FIELD-SYMBOL(<ls_addr>).
    IF lv_addr_guid IS INITIAL.
      <ls_addr>-task = 'I'.
      " Standard address usage XXDEFAULT - required, otherwise the address
      " validity check aborts with R11 243 (same as ZGEM_CREATE_VENDOR).
      APPEND INITIAL LINE TO <ls_addr>-data-addr_usage-addr_usages ASSIGNING FIELD-SYMBOL(<ls_usage>).
      <ls_usage>-task                 = 'I'.
      <ls_usage>-data_key-addresstype = 'XXDEFAULT'.
      <ls_usage>-data_key-valid_to    = '99991231'.
      <ls_usage>-data-valid_from      = sy-datum.
      <ls_usage>-data-standard        = abap_true.
    ELSE.
      <ls_addr>-task          = 'U'.
      <ls_addr>-data_key-guid = lv_addr_guid.
    ENDIF.
    <ls_addr>-data-postal-data-street      = is_vendor-stras.
    <ls_addr>-data-postal-data-city        = is_vendor-ort01.
    <ls_addr>-data-postal-data-district    = is_vendor-ort02.
    <ls_addr>-data-postal-data-postl_cod1  = is_vendor-pstlz.
    <ls_addr>-data-postal-data-region      = is_vendor-regio.
    <ls_addr>-data-postal-data-country     = is_vendor-land1.
    <ls_addr>-data-postal-data-langu       = conv_language( is_vendor-spras ).
    <ls_addr>-data-postal-datax-street     = abap_true.
    <ls_addr>-data-postal-datax-city       = abap_true.
    <ls_addr>-data-postal-datax-district   = abap_true.
    <ls_addr>-data-postal-datax-postl_cod1 = abap_true.
    <ls_addr>-data-postal-datax-region     = abap_true.
    <ls_addr>-data-postal-datax-country    = abap_true.
    <ls_addr>-data-postal-datax-langu      = abap_true.

    " Telephone: current_state = X -> the list sent replaces the OVL list
    IF is_vendor-telf1 IS NOT INITIAL.
      <ls_addr>-data-communication-phone-current_state = abap_true.
      APPEND INITIAL LINE TO <ls_addr>-data-communication-phone-phone ASSIGNING FIELD-SYMBOL(<ls_phone>).
      <ls_phone>-contact-task            = 'I'.
      <ls_phone>-contact-data-country    = is_vendor-land1.
      <ls_phone>-contact-data-telephone  = is_vendor-telf1.
      <ls_phone>-contact-data-std_no     = abap_true.
      <ls_phone>-contact-datax-country   = abap_true.
      <ls_phone>-contact-datax-telephone = abap_true.
      <ls_phone>-contact-datax-std_no    = abap_true.
    ENDIF.

    "--- bank details (BP-owned under CVI -> LFBK) -------------------
    LOOP AT it_bank INTO DATA(ls_bank) WHERE bankn IS NOT INITIAL.
      CLEAR lv_bkvid.
      IF iv_create = abap_false.
        " Same bank ID, else same account -> update, otherwise insert
        IF ls_bank-bvtyp IS NOT INITIAL.
          SELECT SINGLE bkvid FROM but0bk
            WHERE partner = @iv_partner AND bkvid = @ls_bank-bvtyp
            INTO @lv_bkvid.
        ENDIF.
        IF lv_bkvid IS INITIAL.
          SELECT SINGLE bkvid FROM but0bk
            WHERE partner = @iv_partner AND banks = @ls_bank-banks
              AND bankl = @ls_bank-bankl AND bankn = @ls_bank-bankn
            INTO @lv_bkvid.
        ENDIF.
      ENDIF.

      APPEND INITIAL LINE TO cs_cvis-partner-central_data-bankdetail-bankdetails ASSIGNING FIELD-SYMBOL(<ls_bk>).
      IF lv_bkvid IS NOT INITIAL.
        <ls_bk>-task     = 'U'.
        <ls_bk>-data_key = lv_bkvid.
      ELSE.
        <ls_bk>-task     = 'I'.
        <ls_bk>-data_key = ls_bank-bvtyp.   " blank -> CVI assigns the ID
      ENDIF.
      <ls_bk>-data-bank_ctry      = ls_bank-banks.
      <ls_bk>-data-bank_key       = ls_bank-bankl.
      <ls_bk>-data-bank_acct      = ls_bank-bankn.
      <ls_bk>-data-accountholder  = COND #( WHEN ls_bank-koinh IS NOT INITIAL THEN ls_bank-koinh
                                            ELSE is_vendor-name1 ).
      <ls_bk>-datax-bank_ctry     = abap_true.
      <ls_bk>-datax-bank_key      = abap_true.
      <ls_bk>-datax-bank_acct     = abap_true.
      <ls_bk>-datax-accountholder = abap_true.
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_vendor.
    DATA ls_company TYPE vmds_ei_company.

    "--- header: same number as the BP -------------------------------
    SELECT SINGLE @abap_true FROM lfa1 WHERE lifnr = @iv_lifnr INTO @DATA(lv_lfa1_exists).
    cs_cvis-vendor-header-object_task           = COND #( WHEN lv_lfa1_exists = abap_true THEN 'U' ELSE 'I' ).
    cs_cvis-vendor-header-object_instance-lifnr = iv_lifnr.

    "--- central data (LFA1) ------------------------------------------
    IF lv_lfa1_exists = abap_false.
      cs_cvis-vendor-central_data-central-data-ktokk  = mv_ktokk.
      cs_cvis-vendor-central_data-central-datax-ktokk = abap_true.
    ENDIF.
    cs_cvis-vendor-central_data-central-data-j_1ipanno  = is_vendor-j_1ipanno.
    cs_cvis-vendor-central_data-central-datax-j_1ipanno = abap_true.
    cs_cvis-vendor-central_data-central-data-konzs      = is_vendor-konzs.
    cs_cvis-vendor-central_data-central-datax-konzs     = abap_true.

    "--- company code data (LFB1) - OVL only ---------------------------
    READ TABLE it_company INTO DATA(ls_in) WITH KEY bukrs = mv_bukrs.
    IF sy-subrc <> 0.
      READ TABLE it_company INTO ls_in INDEX 1.   " ONGC may send its own code
    ENDIF.

    ls_company-task           = 'M'.               " insert or change
    ls_company-data_key-bukrs = mv_bukrs.

    ls_company-data-akont  = get_recon_account( ls_in-akont ).
    ls_company-datax-akont = abap_true.

    " Payment terms: OVL default, the ONGC value is ignored
    ls_company-data-zterm  = mv_zterm.
    ls_company-datax-zterm = abap_true.

    IF ls_in-zuawa IS NOT INITIAL.
      ls_company-data-zuawa  = ls_in-zuawa.
      ls_company-datax-zuawa = abap_true.
    ENDIF.
    IF ls_in-zwels IS NOT INITIAL.
      ls_company-data-zwels  = ls_in-zwels.
      ls_company-datax-zwels = abap_true.
    ENDIF.
    IF ls_in-kverm IS NOT INITIAL.
      ls_company-data-kverm  = ls_in-kverm.
      ls_company-datax-kverm = abap_true.
    ENDIF.
    " Personnel number only if the employee exists in OVL HR data,
    " otherwise the field check would reject the whole vendor.
    IF ls_in-pernr IS NOT INITIAL.
      DATA lv_pernr TYPE persno.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ls_in-pernr
        IMPORTING
          output = lv_pernr.
      SELECT SINGLE @abap_true FROM pa0003 WHERE pernr = @lv_pernr INTO @DATA(lv_pernr_ok).
      IF lv_pernr_ok = abap_true.
        ls_company-data-pernr  = lv_pernr.
        ls_company-datax-pernr = abap_true.
      ELSE.
        add_message( iv_type = 'W'
                     iv_text = |Personnel number { ls_in-pernr } does not exist in OVL - LFB1-PERNR not set.| ).
      ENDIF.
    ENDIF.

    APPEND ls_company TO cs_cvis-vendor-company_data-company.
  ENDMETHOD.


  METHOD get_recon_account.
    " ONGC recon account if it exists in OVL, else the OVL default
    IF iv_akont IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = iv_akont
        IMPORTING
          output = rv_akont.
      SELECT SINGLE @abap_true FROM skb1
        WHERE bukrs = @mv_bukrs AND saknr = @rv_akont
        INTO @DATA(lv_exists).
      IF lv_exists = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    rv_akont = mv_akont.
    IF rv_akont IS INITIAL.
      add_message( iv_type = 'E'
                   iv_text = |Reconciliation account '{ iv_akont }' does not exist in { mv_bukrs } and no | &&
                             |default is maintained (STVARV ZOVL_VENSYNC_AKONT).| ).
    ENDIF.
  ENDMETHOD.


  METHOD conv_language.
    " ONGC may send the SAP key (E) or ISO code (EN)
    IF strlen( iv_spras ) = 2.
      CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
        EXPORTING
          input            = iv_spras
        IMPORTING
          output           = rv_langu
        EXCEPTIONS
          unknown_language = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        CLEAR rv_langu.
      ENDIF.
    ELSE.
      rv_langu = iv_spras.
    ENDIF.
    IF rv_langu IS INITIAL.
      rv_langu = sy-langu.
    ENDIF.
  ENDMETHOD.


  METHOD get_title_key.
    " LFA1-ANRED is the title TEXT (e.g. 'Mr.'); the BP needs the key
    CHECK iv_anred IS NOT INITIAL.
    SELECT SINGLE title FROM tsad3t
      WHERE langu = @sy-langu AND title_medi = @iv_anred
      INTO @rv_title.
    IF sy-subrc <> 0 AND strlen( iv_anred ) <= 4.
      SELECT SINGLE title FROM tsad3
        WHERE title = @iv_anred
        INTO @rv_title.
    ENDIF.
  ENDMETHOD.


  METHOD read_param.
    SELECT SINGLE low FROM tvarvc
      WHERE name = @iv_name AND type = 'P' AND numb = '0000'
      INTO @rv_value.
    IF sy-subrc <> 0 OR rv_value IS INITIAL.
      rv_value = iv_default.
    ENDIF.
  ENDMETHOD.


  METHOD add_message.
    " Generic message 00/001 (&1&2&3&4) so the Gateway message container
    " rebuilds the same text from the variables - no message class needed.
    DATA lv_text TYPE c LENGTH 200.
    lv_text = iv_text.
    APPEND VALUE #( type       = iv_type
                    id         = '00'
                    number     = '001'
                    message    = iv_text
                    message_v1 = lv_text+0(50)
                    message_v2 = lv_text+50(50)
                    message_v3 = lv_text+100(50)
                    message_v4 = lv_text+150(50) ) TO mt_messages.
  ENDMETHOD.


  METHOD has_error.
    LOOP AT mt_messages TRANSPORTING NO FIELDS WHERE type = 'E' OR type = 'A'.
      rv_error = abap_true.
      EXIT.
    ENDLOOP.
  ENDMETHOD.


  METHOD write_log.
    DATA ls_log TYPE zovl_ven_synclog.

    ls_log-lifnr   = is_result-lifnr.
    ls_log-erdat   = sy-datum.
    ls_log-erzet   = sy-uzeit.
    ls_log-action  = is_result-action.
    ls_log-status  = is_result-status.
    ls_log-message = is_result-message.
    ls_log-ernam   = sy-uname.
    MODIFY zovl_ven_synclog FROM ls_log.
    COMMIT WORK.
  ENDMETHOD.

ENDCLASS.
