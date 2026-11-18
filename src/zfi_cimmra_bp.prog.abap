*&---------------------------------------------------------------------*
*& Include ZFI_CIMMRA_BP
*&---------------------------------------------------------------------*
* Title       : CIMMRA vendor interface - post the vendor as a business
*               partner (S/4HANA)
* Used by     : ZFI_CIMMRA_VENDOR_CREATE, ZFI_CIMMRA_EMPVEND,
*               ZFI_CIMMRA_EMPVEND_UPD, ZFI_CIMMRA_VENDOR_EXT,
*               ZFI_CIMMRA_VENDOR_BLK, ZFI_VEND_HONC (HONC doctors)
* Description : In ECC the three reports called the RFCs
*               ZMM_RFC_VENDOR_CREATE and ZMM_RFC_VENDOR_CHANGE, which
*               maintained the vendor (LFA1/LFB1/LFM1) on its own. In
*               S/4HANA a supplier is a business partner: it is created
*               through CL_MD_BP_MAINTAIN with a BP grouping, the supplier
*               roles and the partner's own address and tax numbers, and
*               CVI writes the supplier from it. LCL_CIMMRA_BP=>CREATE and
*               =>CHANGE take exactly what the reports passed to the RFCs and
*               hand back LT_ERROR / LT_SUCCESS in the RFC's format - the
*               success message is "<vendor>;<address number> <text>" - so
*               the rest of each report, the response file included, is
*               unchanged.
*
*               What moves where in S/4HANA:
*               - name, title, search term, address, telephone, fax, e-mail
*                 -> the business partner (BUT000 / ADRC)
*               - GSTIN (STCD3) and STCD5 -> the partner's tax numbers,
*                 category <country>3 / <country>5 (DFKKBPTAXNUM); CVI
*                 copies them to LFA1. Written into LFA1 directly they are
*                 not shown in BP and the next save in BP can lose them.
*               - account group -> BP grouping (CVIC_VEND_TO_BP1) and the
*                 supplier roles (CVIC_VEND_TO_BP2)
*               - PAN, GST classification, MSME (J_1IPANNO, VEN_CLASS,
*                 J_1ISSIST), bank, company code, purchasing org and
*                 withholding tax -> the supplier part, as before
*               - TAN exemptions -> J_1ITAN_EXEM_SAVE, as before
* Change      : S/4HANA conversion, November 2026
*
* ABAP Cloud  : CL_MD_BP_MAINTAIN, CVI_VEND_LINK and the CVIC_* mapping
*               tables are not released for ABAP Cloud - this is classic
*               ABAP (tier 2). Run ATC with variant ABAP_CLOUD_READINESS
*               and record these as known exemptions.
*----------------------------------------------------------------------*
CLASS lcl_cimmra_bp DEFINITION FINAL.
  PUBLIC SECTION.
    "! A new vendor. IV_LIFNR only for an account group with external
    "! numbering; the business partner then gets the same number.
    "! Options of CREATE and CHANGE (ZFI_VEND_HONC uses them):
    "! IV_INDUSTRY     - the industry key of the file (ECC LFA1-BRSCH). It
    "!                   goes to the supplier as BRSCH when T016 has it and to
    "!                   the business partner as its industry sector (BP >
    "!                   Industries) when one industry system (TB038A) holds
    "!                   it - both when both do.
    "! IV_KEEP_BLANK    - supplier / CIN fields, comma separated, that are sent
    "!                   even when blank, so that a blank in the file clears
    "!                   them (e.g. VEN_CLASS: blank = registered for GST).
    "! IV_BANK_REPLACE  - CHANGE: the file's account replaces the vendor's
    "!                   first bank account, as typing over line 1 in FK02 did.
    CLASS-METHODS create
      IMPORTING iv_lifnr   TYPE lifnr OPTIONAL
                is_central TYPE vmds_ei_vmd_central
                is_postal  TYPE cvis_ei_1vl
                is_company TYPE any
                is_cin     TYPE any
                is_purch   TYPE any
                it_mobile  TYPE ANY TABLE OPTIONAL
                it_fax     TYPE ANY TABLE OPTIONAL
                it_smtp    TYPE ANY TABLE OPTIONAL
                it_bank    TYPE ANY TABLE OPTIONAL
                it_wtax    TYPE ANY TABLE OPTIONAL
                it_tan     TYPE ANY TABLE OPTIONAL
                iv_industry     TYPE clike OPTIONAL
                iv_keep_blank   TYPE string OPTIONAL
                iv_bank_replace TYPE abap_bool DEFAULT abap_false
      EXPORTING et_error   TYPE bapiret2_t
                et_success TYPE bapiret2_t.

    "! An existing vendor. Only filled fields are changed; telephone, fax
    "! and e-mail entries the partner does not have yet are added.
    CLASS-METHODS change
      IMPORTING iv_lifnr   TYPE lifnr
                is_central TYPE vmds_ei_vmd_central
                is_postal  TYPE cvis_ei_1vl
                is_company TYPE any
                is_cin     TYPE any
                is_purch   TYPE any
                it_mobile  TYPE ANY TABLE OPTIONAL
                it_fax     TYPE ANY TABLE OPTIONAL
                it_smtp    TYPE ANY TABLE OPTIONAL
                it_bank    TYPE ANY TABLE OPTIONAL
                it_wtax    TYPE ANY TABLE OPTIONAL
                it_tan     TYPE ANY TABLE OPTIONAL
                iv_industry     TYPE clike OPTIONAL
                iv_keep_blank   TYPE string OPTIONAL
                iv_bank_replace TYPE abap_bool DEFAULT abap_false
      EXPORTING et_error   TYPE bapiret2_t
                et_success TYPE bapiret2_t.

    "! Blocks of an existing vendor (was XK05 by batch input, and the payment
    "! block through VMD_EI_API). As in XK05 the row is the block state: a
    "! blank cell removes that block. The purchasing blocks are set only with a
    "! purchasing organisation, the company code blocks only with a company
    "! code - as the XK05 screen offered them.
    CLASS-METHODS block
      IMPORTING iv_lifnr    TYPE lifnr
                iv_bukrs    TYPE bukrs OPTIONAL
                iv_ekorg    TYPE ekorg OPTIONAL
                iv_sperr    TYPE clike OPTIONAL   " posting block, all company codes
                iv_sperr_cc TYPE clike OPTIONAL   " posting block, the company code
                iv_sperm    TYPE clike OPTIONAL   " purchasing block, all purch. orgs
                iv_sperm_po TYPE clike OPTIONAL   " purchasing block, the purch. org
                iv_sperq    TYPE clike OPTIONAL   " block function
                iv_zahls    TYPE clike OPTIONAL   " payment block, the company code
      EXPORTING et_error    TYPE bapiret2_t
                et_success  TYPE bapiret2_t.

    "! An existing vendor extended to a company code and a purchasing
    "! organisation, copied from a reference company code and purchasing
    "! organisation of the same vendor (was XK01 with reference, then
    "! VMD_EI_API). Blocks and deletion flags are not copied. Supplier roles
    "! the partner lacks for its account group are added.
    CLASS-METHODS extend
      IMPORTING iv_lifnr     TYPE lifnr
                iv_bukrs     TYPE bukrs OPTIONAL
                iv_ekorg     TYPE ekorg OPTIONAL
                iv_ref_bukrs TYPE bukrs OPTIONAL
                iv_ref_ekorg TYPE ekorg OPTIONAL
                iv_akont     TYPE clike OPTIONAL
                iv_zterm     TYPE clike OPTIONAL
                iv_zwels     TYPE clike OPTIONAL
                iv_waers     TYPE clike OPTIONAL
                iv_telf1     TYPE clike OPTIONAL
      EXPORTING et_error     TYPE bapiret2_t
                et_success   TYPE bapiret2_t.

    "! A date from the file - DD.MM.YYYY, DD-MM-YYYY, DD/MM/YYYY, DDMMYYYY
    "! or YYYYMMDD - as a date; initial when it is none of these.
    CLASS-METHODS to_date
      IMPORTING iv_in     TYPE clike
      RETURNING VALUE(rv) TYPE d.

  PRIVATE SECTION.
    CONSTANTS: c_insert TYPE c LENGTH 1 VALUE 'I',
               c_update TYPE c LENGTH 1 VALUE 'U',
               c_delete TYPE c LENGTH 1 VALUE 'D'.

    CLASS-METHODS post
      IMPORTING iv_change  TYPE abap_bool
                iv_lifnr   TYPE lifnr
                is_central TYPE vmds_ei_vmd_central
                is_postal  TYPE cvis_ei_1vl
                is_company TYPE any
                is_cin     TYPE any
                is_purch   TYPE any
                it_mobile  TYPE ANY TABLE OPTIONAL
                it_fax     TYPE ANY TABLE OPTIONAL
                it_smtp    TYPE ANY TABLE OPTIONAL
                it_bank    TYPE ANY TABLE OPTIONAL
                it_wtax    TYPE ANY TABLE OPTIONAL
                it_tan     TYPE ANY TABLE OPTIONAL
                iv_industry     TYPE clike OPTIONAL
                iv_keep_blank   TYPE string OPTIONAL
                iv_bank_replace TYPE abap_bool DEFAULT abap_false
      EXPORTING et_error   TYPE bapiret2_t
                et_success TYPE bapiret2_t.

    "! Moves the filled elementary components of IS_SRC into the
    "! same-named components of CS_DATA and flags them in CS_DATAX. IV_ONLY
    "! and IV_SKIP are comma-separated component names.
    CLASS-METHODS copy
      IMPORTING is_src   TYPE any
                iv_only  TYPE string OPTIONAL
                iv_skip  TYPE string OPTIONAL
      CHANGING  cs_data  TYPE any
                cs_datax TYPE any.

    CLASS-METHODS set
      IMPORTING iv_comp  TYPE clike
                iv_value TYPE any
      CHANGING  cs_data  TYPE any
                cs_datax TYPE any.

    "! One elementary component of a structure as text; empty when the
    "! structure has no such component.
    CLASS-METHODS comp
      IMPORTING is_any    TYPE any
                iv_comp   TYPE clike
      RETURNING VALUE(rv) TYPE string.

    "! The title key (0001, 0003 ...) for a key or a title text.
    CLASS-METHODS title_key
      IMPORTING iv_in     TYPE clike
      RETURNING VALUE(rv) TYPE ad_title.

    CLASS-METHODS add
      IMPORTING iv_type TYPE bapi_mtype DEFAULT 'E'
                iv_text TYPE clike
      CHANGING  ct      TYPE bapiret2_t.

    "! BUP_MEMORY_CENTRAL_INIT: the partner keeps its memory of the LUW
    "! just closed, and the next vendor is refused without a fresh start.
    CLASS-METHODS reset_bp.

    "! Validate, maintain, commit. False when anything stopped it; the
    "! reasons are in CT_ERROR.
    CLASS-METHODS save
      IMPORTING is_data   TYPE cvis_ei_extern
      CHANGING  ct_error  TYPE bapiret2_t
      RETURNING VALUE(rv) TYPE abap_bool.

    "! The headers of a change of an existing vendor: the partner by its
    "! GUID (CVI_VEND_LINK), the supplier by its number.
    CLASS-METHODS change_head
      IMPORTING iv_lifnr  TYPE lifnr
      EXPORTING ev_bp     TYPE bu_partner
      CHANGING  cs_data   TYPE cvis_ei_extern
                ct_error  TYPE bapiret2_t
      RETURNING VALUE(rv) TYPE abap_bool.
ENDCLASS.


CLASS lcl_cimmra_bp IMPLEMENTATION.

  METHOD create.
    post( EXPORTING iv_change  = abap_false
                    iv_lifnr   = iv_lifnr
                    is_central = is_central
                    is_postal  = is_postal
                    is_company = is_company
                    is_cin     = is_cin
                    is_purch   = is_purch
                    it_mobile  = it_mobile
                    it_fax     = it_fax
                    it_smtp    = it_smtp
                    it_bank    = it_bank
                    it_wtax    = it_wtax
                    it_tan     = it_tan
                    iv_industry     = iv_industry
                    iv_keep_blank   = iv_keep_blank
                    iv_bank_replace = iv_bank_replace
          IMPORTING et_error   = et_error
                    et_success = et_success ).
  ENDMETHOD.

  METHOD change.
    post( EXPORTING iv_change  = abap_true
                    iv_lifnr   = iv_lifnr
                    is_central = is_central
                    is_postal  = is_postal
                    is_company = is_company
                    is_cin     = is_cin
                    is_purch   = is_purch
                    it_mobile  = it_mobile
                    it_fax     = it_fax
                    it_smtp    = it_smtp
                    it_bank    = it_bank
                    it_wtax    = it_wtax
                    it_tan     = it_tan
                    iv_industry     = iv_industry
                    iv_keep_blank   = iv_keep_blank
                    iv_bank_replace = iv_bank_replace
          IMPORTING et_error   = et_error
                    et_success = et_success ).
  ENDMETHOD.

  METHOD to_date.
    CLEAR rv.
    DATA(lv) = condense( CONV string( iv_in ) ).
    IF lv IS INITIAL.
      RETURN.
    ENDIF.
    REPLACE ALL OCCURRENCES OF PCRE '[./\-]' IN lv WITH ``.
    IF strlen( lv ) <> 8 OR lv CN '0123456789'.
      RETURN.
    ENDIF.
    " YYYYMMDD when it starts with a year, otherwise DDMMYYYY.
    IF lv(4) >= '1900' AND lv(4) <= '2999'.
      rv = lv.
    ELSE.
      rv = |{ lv+4(4) }{ lv+2(2) }{ lv(2) }|.
    ENDIF.
  ENDMETHOD.

  METHOD add.
    APPEND VALUE bapiret2( type = iv_type message = iv_text ) TO ct.
  ENDMETHOD.

  METHOD reset_bp.
    TRY.
        CALL FUNCTION 'BUP_MEMORY_CENTRAL_INIT'.
      CATCH cx_sy_dyn_call_illegal_func.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD comp.
    CLEAR rv.
    ASSIGN COMPONENT iv_comp OF STRUCTURE is_any TO FIELD-SYMBOL(<lv_v>).
    IF sy-subrc = 0.
      rv = <lv_v>.
      CONDENSE rv.
    ENDIF.
  ENDMETHOD.

  METHOD set.
    ASSIGN COMPONENT iv_comp OF STRUCTURE cs_data TO FIELD-SYMBOL(<lv_t>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    TRY.
        <lv_t> = iv_value.
      CATCH cx_sy_conversion_error.
        RETURN.
    ENDTRY.
    ASSIGN COMPONENT iv_comp OF STRUCTURE cs_datax TO FIELD-SYMBOL(<lv_x>).
    IF sy-subrc = 0.
      <lv_x> = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD copy.
    DATA lo_sd TYPE REF TO cl_abap_structdescr.
    TRY.
        lo_sd ?= cl_abap_typedescr=>describe_by_data( is_src ).
      CATCH cx_sy_move_cast_error.
        RETURN.
    ENDTRY.
    DATA(lv_only) = |,{ to_upper( iv_only ) },|.
    DATA(lv_skip) = |,{ to_upper( iv_skip ) },|.
    LOOP AT lo_sd->components INTO DATA(ls_c).
      IF ls_c-type_kind = cl_abap_typedescr=>typekind_struct1
      OR ls_c-type_kind = cl_abap_typedescr=>typekind_struct2
      OR ls_c-type_kind = cl_abap_typedescr=>typekind_table.
        CONTINUE.
      ENDIF.
      DATA(lv_key) = |,{ ls_c-name },|.
      IF iv_only IS NOT INITIAL AND lv_only NS lv_key.
        CONTINUE.
      ENDIF.
      IF iv_skip IS NOT INITIAL AND lv_skip CS lv_key.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT ls_c-name OF STRUCTURE is_src TO FIELD-SYMBOL(<lv_v>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      IF <lv_v> IS INITIAL.
        CONTINUE.
      ENDIF.
      set( EXPORTING iv_comp = ls_c-name iv_value = <lv_v>
           CHANGING  cs_data = cs_data cs_datax = cs_datax ).
    ENDLOOP.
  ENDMETHOD.

  METHOD title_key.
    CLEAR rv.
    DATA(lv_in) = condense( CONV string( iv_in ) ).
    IF lv_in IS INITIAL.
      RETURN.
    ENDIF.
    IF lv_in CO '0123456789'.
      rv = |{ lv_in ALPHA = IN WIDTH = 4 }|.
      RETURN.
    ENDIF.
    DATA(lv_up) = to_upper( lv_in ).
    SELECT title, title_medi FROM tsad3t
      WHERE langu = @sy-langu INTO TABLE @DATA(lt_t).
    LOOP AT lt_t INTO DATA(ls_t).
      IF to_upper( CONV string( ls_t-title_medi ) ) = lv_up.
        rv = ls_t-title.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD save.
    rv = abap_false.
    DATA lt_map TYPE mdg_bs_bp_msgmap_t.
    TRY.
        cl_md_bp_maintain=>validate_single(
          EXPORTING i_data        = is_data
          IMPORTING et_return_map = lt_map ).
      CATCH cx_root INTO DATA(lx_val).
        add( EXPORTING iv_text = |Validation failed: { lx_val->get_text( ) }|
             CHANGING  ct      = ct_error ).
        RETURN.
    ENDTRY.
    DATA(lv_bad) = abap_false.
    LOOP AT lt_map INTO DATA(ls_map) WHERE type CA 'EAX'.
      APPEND VALUE bapiret2( type = ls_map-type id = ls_map-id number = ls_map-number
                             message = ls_map-message ) TO ct_error.
      lv_bad = abap_true.
    ENDLOOP.
    IF lv_bad = abap_true.
      RETURN.
    ENDIF.

    DATA: lt_in  TYPE cvis_ei_extern_t,
          lt_ret TYPE bapiretm.
    APPEND is_data TO lt_in.
    TRY.
        cl_md_bp_maintain=>maintain(
          EXPORTING i_data   = lt_in
          IMPORTING e_return = lt_ret ).
      CATCH cx_root INTO DATA(lx_mnt).
        ROLLBACK WORK.
        reset_bp( ).
        add( EXPORTING iv_text = |Maintain failed: { lx_mnt->get_text( ) }|
             CHANGING  ct      = ct_error ).
        RETURN.
    ENDTRY.

    " BAPIRETM lines carry a nested message table; read it generically so a
    " release-dependent component name cannot break the program.
    FIELD-SYMBOLS: <lt_sub> TYPE ANY TABLE.
    LOOP AT lt_ret ASSIGNING FIELD-SYMBOL(<ls_ret>).
      ASSIGN COMPONENT 'OBJECT_MSG' OF STRUCTURE <ls_ret> TO <lt_sub>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      LOOP AT <lt_sub> ASSIGNING FIELD-SYMBOL(<ls_sub>).
        DATA ls_r2 TYPE bapiret2.
        CLEAR ls_r2.
        MOVE-CORRESPONDING <ls_sub> TO ls_r2.
        IF ls_r2-type CA 'EAX'.
          APPEND ls_r2 TO ct_error.
          lv_bad = abap_true.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    IF lv_bad = abap_true.
      ROLLBACK WORK.
      reset_bp( ).
      RETURN.
    ENDIF.

    " BAPI_TRANSACTION_COMMIT, not a bare COMMIT WORK: the business partner
    " hangs its end-of-LUW processing off it, and WAIT makes RETURN say
    " whether the update actually ran.
    DATA ls_cret TYPE bapiret2.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait   = abap_true
      IMPORTING return = ls_cret.
    reset_bp( ).
    IF ls_cret-type CA 'EAX'.
      add( EXPORTING iv_text = |The update did not go through - nothing was saved: { ls_cret-message } | &&
                               |(SM13)|
           CHANGING  ct      = ct_error ).
      RETURN.
    ENDIF.
    rv = abap_true.
  ENDMETHOD.

  METHOD change_head.
    rv = abap_false.
    CLEAR ev_bp.
    DATA lv_guid TYPE bu_partner_guid.
    SELECT SINGLE partner_guid FROM cvi_vend_link
      WHERE vendor = @iv_lifnr INTO @lv_guid.
    IF lv_guid IS INITIAL.
      add( EXPORTING iv_text = |Vendor { iv_lifnr ALPHA = OUT } has no business partner (CVI_VEND_LINK)|
           CHANGING  ct      = ct_error ).
      RETURN.
    ENDIF.
    SELECT SINGLE partner FROM but000
      WHERE partner_guid = @lv_guid INTO @ev_bp.
    cs_data-partner-header-object_task                  = c_update.
    cs_data-partner-header-object_instance-bpartnerguid = lv_guid.
    cs_data-vendor-header-object_task                   = c_update.
    cs_data-vendor-header-object_instance-lifnr         = iv_lifnr.
    rv = abap_true.
  ENDMETHOD.

  METHOD block.
    CLEAR: et_error, et_success.
    DATA: ls_data TYPE cvis_ei_extern,
          lv_bp   TYPE bu_partner.
    IF change_head( EXPORTING iv_lifnr = iv_lifnr
                    IMPORTING ev_bp    = lv_bp
                    CHANGING  cs_data  = ls_data
                              ct_error = et_error ) = abap_false.
      RETURN.
    ENDIF.

    " The block function is a two digit code; the file may carry 1 for 01.
    DATA(lv_sperq) = condense( CONV string( iv_sperq ) ).
    IF lv_sperq IS NOT INITIAL AND lv_sperq CO '0123456789'.
      lv_sperq = |{ lv_sperq WIDTH = 2 ALIGN = RIGHT PAD = '0' }|.
    ENDIF.

    " Central blocks - a blank value is sent too: it removes the block.
    set( EXPORTING iv_comp = 'SPERR' iv_value = to_upper( condense( CONV string( iv_sperr ) ) )
         CHANGING  cs_data  = ls_data-vendor-central_data-central-data
                   cs_datax = ls_data-vendor-central_data-central-datax ).
    set( EXPORTING iv_comp = 'SPERQ' iv_value = lv_sperq
         CHANGING  cs_data  = ls_data-vendor-central_data-central-data
                   cs_datax = ls_data-vendor-central_data-central-datax ).
    IF iv_ekorg IS NOT INITIAL.
      set( EXPORTING iv_comp = 'SPERM' iv_value = to_upper( condense( CONV string( iv_sperm ) ) )
           CHANGING  cs_data  = ls_data-vendor-central_data-central-data
                     cs_datax = ls_data-vendor-central_data-central-datax ).
    ENDIF.

    IF iv_bukrs IS NOT INITIAL.
      DATA lv_chave TYPE abap_bool.
      SELECT SINGLE @abap_true FROM lfb1
        WHERE lifnr = @iv_lifnr AND bukrs = @iv_bukrs INTO @lv_chave.
      IF lv_chave = abap_false.
        add( EXPORTING iv_text = |Vendor { iv_lifnr ALPHA = OUT } is not created in company code { iv_bukrs }|
             CHANGING  ct      = et_error ).
        RETURN.
      ENDIF.
      DATA ls_cc TYPE vmds_ei_company.
      ls_cc-task           = c_update.
      ls_cc-data_key-bukrs = iv_bukrs.
      set( EXPORTING iv_comp = 'SPERR' iv_value = to_upper( condense( CONV string( iv_sperr_cc ) ) )
           CHANGING  cs_data = ls_cc-data cs_datax = ls_cc-datax ).
      set( EXPORTING iv_comp = 'ZAHLS' iv_value = to_upper( condense( CONV string( iv_zahls ) ) )
           CHANGING  cs_data = ls_cc-data cs_datax = ls_cc-datax ).
      APPEND ls_cc TO ls_data-vendor-company_data-company.
    ENDIF.

    IF iv_ekorg IS NOT INITIAL.
      DATA lv_phave TYPE abap_bool.
      SELECT SINGLE @abap_true FROM lfm1
        WHERE lifnr = @iv_lifnr AND ekorg = @iv_ekorg INTO @lv_phave.
      IF lv_phave = abap_false.
        add( EXPORTING iv_text = |Vendor { iv_lifnr ALPHA = OUT } is not created in purchasing | &&
                                 |organisation { iv_ekorg }|
             CHANGING  ct      = et_error ).
        RETURN.
      ENDIF.
      DATA ls_po TYPE vmds_ei_purchasing.
      ls_po-task           = c_update.
      ls_po-data_key-ekorg = iv_ekorg.
      set( EXPORTING iv_comp = 'SPERM' iv_value = to_upper( condense( CONV string( iv_sperm_po ) ) )
           CHANGING  cs_data = ls_po-data cs_datax = ls_po-datax ).
      APPEND ls_po TO ls_data-vendor-purchasing_data-purchasing.
    ENDIF.

    IF save( EXPORTING is_data = ls_data CHANGING ct_error = et_error ) = abap_false.
      RETURN.
    ENDIF.
    APPEND VALUE bapiret2( type = 'S'
                           message = |Blocks of vendor { iv_lifnr ALPHA = OUT } updated| &&
                                     |{ COND string( WHEN iv_bukrs IS NOT INITIAL
                                                     THEN | - company code { iv_bukrs }| ) }| &&
                                     |{ COND string( WHEN iv_ekorg IS NOT INITIAL
                                                     THEN | - purchasing org { iv_ekorg }| ) }| )
           TO et_success.
  ENDMETHOD.

  METHOD extend.
    CLEAR: et_error, et_success.
    DATA: ls_data TYPE cvis_ei_extern,
          lv_bp   TYPE bu_partner.
    IF change_head( EXPORTING iv_lifnr = iv_lifnr
                    IMPORTING ev_bp    = lv_bp
                    CHANGING  cs_data  = ls_data
                              ct_error = et_error ) = abap_false.
      RETURN.
    ENDIF.

    " ---- the supplier roles of the account group the partner lacks ------
    " A vendor that never had a purchasing organisation may lack the
    " purchasing role, and the purchasing data is refused without it.
    DATA lv_ktokk TYPE ktokk.
    SELECT SINGLE ktokk FROM lfa1 WHERE lifnr = @iv_lifnr INTO @lv_ktokk.
    SELECT account_group, role FROM cvic_vend_to_bp2
      WHERE account_group = @lv_ktokk INTO TABLE @DATA(lt_role).
    LOOP AT lt_role INTO DATA(ls_role_db).
      DATA lv_rhave TYPE abap_bool.
      CLEAR lv_rhave.
      SELECT SINGLE @abap_true FROM but100
        WHERE partner = @lv_bp AND rltyp = @ls_role_db-role INTO @lv_rhave.
      IF lv_rhave = abap_false.
        DATA ls_role TYPE bus_ei_bupa_roles.
        CLEAR ls_role.
        ls_role-task     = c_insert.
        ls_role-data_key = ls_role_db-role.
        APPEND ls_role TO ls_data-partner-central_data-role-roles.
      ENDIF.
    ENDLOOP.

    DATA lv_done TYPE string.

    " ---- company code ----------------------------------------------------
    IF iv_bukrs IS NOT INITIAL.
      DATA lv_chave TYPE abap_bool.
      SELECT SINGLE @abap_true FROM lfb1
        WHERE lifnr = @iv_lifnr AND bukrs = @iv_bukrs INTO @lv_chave.
      IF lv_chave = abap_false.
        DATA ls_cc TYPE vmds_ei_company.
        ls_cc-task           = c_insert.
        ls_cc-data_key-bukrs = iv_bukrs.
        IF iv_ref_bukrs IS NOT INITIAL.
          SELECT SINGLE * FROM lfb1
            WHERE lifnr = @iv_lifnr AND bukrs = @iv_ref_bukrs INTO @DATA(ls_rlfb1).
          IF sy-subrc <> 0.
            add( EXPORTING iv_text = |Vendor { iv_lifnr ALPHA = OUT } is not created in reference | &&
                                     |company code { iv_ref_bukrs }|
                 CHANGING  ct      = et_error ).
            RETURN.
          ENDIF.
          " What XK01 with reference copied - but not the blocks, the deletion
          " flags or who created the record.
          copy( EXPORTING is_src  = ls_rlfb1
                          iv_skip = `MANDT,LIFNR,BUKRS,ERDAT,ERNAM,LOEVM,SPERR,NODEL,ZAHLS,` &&
                                    `CONFS,UPDAT,UPTIM,CVP_XBLCK_B`
                CHANGING  cs_data  = ls_cc-data
                          cs_datax = ls_cc-datax ).
          " Withholding tax types and codes of the reference company code.
          SELECT witht, wt_withcd, wt_subjct, qsrec FROM lfbw
            WHERE lifnr = @iv_lifnr AND bukrs = @iv_ref_bukrs INTO TABLE @DATA(lt_rw).
          LOOP AT lt_rw INTO DATA(ls_rw).
            DATA ls_wtx TYPE vmds_ei_wtax_type.
            CLEAR ls_wtx.
            ls_wtx-task           = c_insert.
            ls_wtx-data_key-witht = ls_rw-witht.
            copy( EXPORTING is_src  = ls_rw
                            iv_skip = `WITHT`
                  CHANGING  cs_data  = ls_wtx-data
                            cs_datax = ls_wtx-datax ).
            APPEND ls_wtx TO ls_cc-wtax_type-wtax_type.
          ENDLOOP.
        ENDIF.
        " The file's values over the reference's.
        DATA(lv_akont) = condense( CONV string( iv_akont ) ).
        IF lv_akont IS NOT INITIAL.
          IF lv_akont CO '0123456789'.
            lv_akont = |{ lv_akont ALPHA = IN WIDTH = 10 }|.
          ENDIF.
          set( EXPORTING iv_comp = 'AKONT' iv_value = lv_akont
               CHANGING  cs_data = ls_cc-data cs_datax = ls_cc-datax ).
        ENDIF.
        IF iv_zterm IS NOT INITIAL.
          set( EXPORTING iv_comp = 'ZTERM' iv_value = iv_zterm
               CHANGING  cs_data = ls_cc-data cs_datax = ls_cc-datax ).
        ENDIF.
        IF iv_zwels IS NOT INITIAL.
          set( EXPORTING iv_comp = 'ZWELS' iv_value = iv_zwels
               CHANGING  cs_data = ls_cc-data cs_datax = ls_cc-datax ).
        ENDIF.
        set( EXPORTING iv_comp = 'ALTKN' iv_value = iv_lifnr
             CHANGING  cs_data = ls_cc-data cs_datax = ls_cc-datax ).
        set( EXPORTING iv_comp = 'REPRF' iv_value = abap_true
             CHANGING  cs_data = ls_cc-data cs_datax = ls_cc-datax ).
        APPEND ls_cc TO ls_data-vendor-company_data-company.
        lv_done = |company code { iv_bukrs }|.
      ENDIF.
    ENDIF.

    " ---- purchasing organisation ----------------------------------------
    IF iv_ekorg IS NOT INITIAL.
      DATA lv_phave TYPE abap_bool.
      SELECT SINGLE @abap_true FROM lfm1
        WHERE lifnr = @iv_lifnr AND ekorg = @iv_ekorg INTO @lv_phave.
      IF lv_phave = abap_false.
        DATA ls_po TYPE vmds_ei_purchasing.
        ls_po-task           = c_insert.
        ls_po-data_key-ekorg = iv_ekorg.
        IF iv_ref_ekorg IS NOT INITIAL.
          SELECT SINGLE * FROM lfm1
            WHERE lifnr = @iv_lifnr AND ekorg = @iv_ref_ekorg INTO @DATA(ls_rlfm1).
          IF sy-subrc <> 0.
            add( EXPORTING iv_text = |Vendor { iv_lifnr ALPHA = OUT } is not created in reference | &&
                                     |purchasing organisation { iv_ref_ekorg }|
                 CHANGING  ct      = et_error ).
            RETURN.
          ENDIF.
          copy( EXPORTING is_src  = ls_rlfm1
                          iv_skip = `MANDT,LIFNR,EKORG,ERDAT,ERNAM,LOEVM,SPERM`
                CHANGING  cs_data  = ls_po-data
                          cs_datax = ls_po-datax ).
        ENDIF.
        IF iv_waers IS NOT INITIAL.
          set( EXPORTING iv_comp = 'WAERS' iv_value = iv_waers
               CHANGING  cs_data = ls_po-data cs_datax = ls_po-datax ).
        ENDIF.
        IF iv_telf1 IS NOT INITIAL.
          set( EXPORTING iv_comp = 'TELF1' iv_value = iv_telf1
               CHANGING  cs_data = ls_po-data cs_datax = ls_po-datax ).
        ENDIF.
        APPEND ls_po TO ls_data-vendor-purchasing_data-purchasing.
        lv_done = COND #( WHEN lv_done IS INITIAL THEN |purchasing organisation { iv_ekorg }|
                          ELSE |{ lv_done } and purchasing organisation { iv_ekorg }| ).
      ENDIF.
    ENDIF.

    IF lv_done IS INITIAL.
      APPEND VALUE bapiret2( type = 'I' message = 'Vendor already extended' ) TO et_success.
      RETURN.
    ENDIF.

    IF save( EXPORTING is_data = ls_data CHANGING ct_error = et_error ) = abap_false.
      RETURN.
    ENDIF.
    APPEND VALUE bapiret2( type = 'S'
                           message = |Vendor { iv_lifnr ALPHA = OUT } extended to { lv_done }| )
           TO et_success.
  ENDMETHOD.

  METHOD post.
    CLEAR: et_error, et_success.

    DATA: ls_data  TYPE cvis_ei_extern,
          lv_guid  TYPE bu_partner_guid,
          lv_lifnr TYPE lifnr,
          lv_land  TYPE land1,
          lv_adrnr TYPE adrnr,
          lv_bp    TYPE bu_partner.

    lv_lifnr = iv_lifnr.
    DATA(lv_task) = COND char1( WHEN iv_change = abap_true THEN c_update ELSE c_insert ).

    " ---- the partner behind the vendor ----------------------------------
    IF iv_change = abap_true.
      SELECT SINGLE partner_guid FROM cvi_vend_link
        WHERE vendor = @lv_lifnr INTO @lv_guid.
      IF lv_guid IS INITIAL.
        add( EXPORTING iv_text = |Vendor { lv_lifnr ALPHA = OUT } has no business partner (CVI_VEND_LINK)|
             CHANGING  ct      = et_error ).
        RETURN.
      ENDIF.
      SELECT SINGLE land1, adrnr FROM lfa1
        WHERE lifnr = @lv_lifnr INTO (@lv_land, @lv_adrnr).
      SELECT SINGLE partner FROM but000
        WHERE partner_guid = @lv_guid INTO @lv_bp.
    ELSE.
      " A new partner has no number until it is saved; the message names it
      " by a GUID generated here, which becomes its PARTNER_GUID.
      TRY.
          lv_guid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
        CATCH cx_uuid_error.
          add( EXPORTING iv_text = 'No GUID could be generated for the new business partner'
               CHANGING  ct      = et_error ).
          RETURN.
      ENDTRY.
    ENDIF.
    IF is_postal-data-country IS NOT INITIAL.
      lv_land = is_postal-data-country.
    ENDIF.

    ls_data-partner-header-object_task                  = lv_task.
    ls_data-partner-header-object_instance-bpartnerguid = lv_guid.
    ls_data-vendor-header-object_task                   = lv_task.
    ls_data-vendor-header-object_instance-lifnr         = lv_lifnr.
    IF iv_change = abap_false AND lv_lifnr IS NOT INITIAL.
      " External numbering: the partner takes the vendor's number.
      ls_data-partner-header-object_instance-bpartner = lv_lifnr.
    ENDIF.

    " ---- business partner: grouping, roles, name -----------------------
    DATA(lv_ktokk) = is_central-data-ktokk.
    IF iv_change = abap_false.
      ls_data-partner-central_data-common-data-bp_control-category = '2'.  " organisation
      DATA lv_grp TYPE bu_group.
      SELECT SINGLE grouping FROM cvic_vend_to_bp1
        WHERE account_group = @lv_ktokk INTO @lv_grp.
      IF lv_grp IS INITIAL.
        add( EXPORTING iv_text = |Account group { lv_ktokk } has no BP grouping (CVIC_VEND_TO_BP1)|
             CHANGING  ct      = et_error ).
        RETURN.
      ENDIF.
      ls_data-partner-central_data-common-data-bp_control-grouping = lv_grp.

      SELECT account_group, role FROM cvic_vend_to_bp2
        WHERE account_group = @lv_ktokk INTO TABLE @DATA(lt_role).
      IF lt_role IS INITIAL.
        add( EXPORTING iv_text = |Account group { lv_ktokk } has no supplier roles (CVIC_VEND_TO_BP2)|
             CHANGING  ct      = et_error ).
        RETURN.
      ENDIF.
      LOOP AT lt_role INTO DATA(ls_role_db).
        DATA ls_role TYPE bus_ei_bupa_roles.
        CLEAR ls_role.
        ls_role-task     = c_insert.
        ls_role-data_key = ls_role_db-role.
        APPEND ls_role TO ls_data-partner-central_data-role-roles.
      ENDLOOP.
    ENDIF.

    DATA(lv_title) = title_key( is_postal-data-title ).
    IF lv_title IS NOT INITIAL.
      set( EXPORTING iv_comp = 'TITLE_KEY' iv_value = lv_title
           CHANGING  cs_data  = ls_data-partner-central_data-common-data-bp_centraldata
                     cs_datax = ls_data-partner-central_data-common-datax-bp_centraldata ).
    ENDIF.
    IF is_postal-data-sort1 IS NOT INITIAL.
      set( EXPORTING iv_comp = 'SEARCHTERM1' iv_value = is_postal-data-sort1
           CHANGING  cs_data  = ls_data-partner-central_data-common-data-bp_centraldata
                     cs_datax = ls_data-partner-central_data-common-datax-bp_centraldata ).
    ENDIF.
    IF is_postal-data-sort2 IS NOT INITIAL.
      set( EXPORTING iv_comp = 'SEARCHTERM2' iv_value = is_postal-data-sort2
           CHANGING  cs_data  = ls_data-partner-central_data-common-data-bp_centraldata
                     cs_datax = ls_data-partner-central_data-common-datax-bp_centraldata ).
    ENDIF.
    DATA(lt_name) = VALUE string_table( ( |NAME1;NAME| ) ( |NAME2;NAME_2| )
                                        ( |NAME3;NAME_3| ) ( |NAME4;NAME_4| ) ).
    LOOP AT lt_name INTO DATA(lv_np).
      SPLIT lv_np AT ';' INTO DATA(lv_to) DATA(lv_from).
      DATA(lv_nm) = comp( is_any = is_postal-data iv_comp = lv_from ).
      IF lv_nm IS NOT INITIAL.
        set( EXPORTING iv_comp = lv_to iv_value = lv_nm
             CHANGING  cs_data  = ls_data-partner-central_data-common-data-bp_organization
                       cs_datax = ls_data-partner-central_data-common-datax-bp_organization ).
      ENDIF.
    ENDLOOP.

    " ---- business partner: address and communication -------------------
    DATA ls_adr TYPE bus_ei_bupa_address.
    CLEAR ls_adr.
    ls_adr-task = lv_task.
    IF iv_change = abap_true.
      ls_adr-data_key-operation = 'XXDFLT'.      " the standard address
    ENDIF.
    copy( EXPORTING is_src  = is_postal-data
                    iv_only = `CITY,DISTRICT,POSTL_COD1,STREET,HOUSE_NO,STR_SUPPL1,` &&
                              `STR_SUPPL2,STR_SUPPL3,LOCATION,COUNTRY,REGION,LANGU`
          CHANGING  cs_data  = ls_adr-data-postal-data
                    cs_datax = ls_adr-data-postal-datax ).

    " What the partner already has - a change adds only what is new.
    DATA: lt_have_tel  TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line,
          lt_have_fax  TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line,
          lt_have_mail TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    IF iv_change = abap_true AND lv_adrnr IS NOT INITIAL.
      SELECT addrnumber, tel_number FROM adr2
        WHERE addrnumber = @lv_adrnr INTO TABLE @DATA(lt_a2).
      DATA lv_have TYPE string.
      LOOP AT lt_a2 INTO DATA(ls_a2).
        lv_have = condense( CONV string( ls_a2-tel_number ) ).
        INSERT lv_have INTO TABLE lt_have_tel.
      ENDLOOP.
      SELECT addrnumber, fax_number FROM adr3
        WHERE addrnumber = @lv_adrnr INTO TABLE @DATA(lt_a3).
      LOOP AT lt_a3 INTO DATA(ls_a3).
        lv_have = condense( CONV string( ls_a3-fax_number ) ).
        INSERT lv_have INTO TABLE lt_have_fax.
      ENDLOOP.
      SELECT addrnumber, smtp_addr FROM adr6
        WHERE addrnumber = @lv_adrnr INTO TABLE @DATA(lt_a6).
      LOOP AT lt_a6 INTO DATA(ls_a6).
        lv_have = to_upper( condense( CONV string( ls_a6-smtp_addr ) ) ).
        INSERT lv_have INTO TABLE lt_have_mail.
      ENDLOOP.
    ENDIF.

    DATA lv_first TYPE abap_bool.
    lv_first = xsdbool( iv_change = abap_false ).
    IF it_mobile IS SUPPLIED.
      LOOP AT it_mobile ASSIGNING FIELD-SYMBOL(<ls_mob>).
        DATA(lv_tel) = comp( is_any = <ls_mob> iv_comp = 'TELEPHONE' ).
        IF lv_tel IS INITIAL OR line_exists( lt_have_tel[ table_line = lv_tel ] ).
          CONTINUE.
        ENDIF.
        INSERT lv_tel INTO TABLE lt_have_tel.
        DATA ls_tel TYPE bus_ei_bupa_telephone.
        CLEAR ls_tel.
        ls_tel-contact-task           = c_insert.
        ls_tel-contact-data-telephone = lv_tel.
        DATA(lv_ext) = comp( is_any = <ls_mob> iv_comp = 'EXTENSION' ).
        IF lv_ext IS NOT INITIAL.
          ls_tel-contact-data-extension = lv_ext.
        ENDIF.
        " R_3_USER is a code, not a flag: space or 1 a landline, 2 or 3 a
        " mobile - anything else stops the save with MESSAGE_TYPE_X.
        DATA(lv_r3) = comp( is_any = <ls_mob> iv_comp = 'R_3_USER' ).
        IF lv_r3 CA '123'.
          ls_tel-contact-data-r_3_user = lv_r3.
        ENDIF.
        ls_tel-contact-data-std_no = lv_first.
        lv_first = abap_false.
        APPEND ls_tel TO ls_adr-data-communication-phone-phone.
      ENDLOOP.
    ENDIF.

    lv_first = xsdbool( iv_change = abap_false ).
    IF it_fax IS SUPPLIED.
      LOOP AT it_fax ASSIGNING FIELD-SYMBOL(<ls_fx>).
        DATA(lv_fax) = comp( is_any = <ls_fx> iv_comp = 'FAX' ).
        IF lv_fax IS INITIAL OR line_exists( lt_have_fax[ table_line = lv_fax ] ).
          CONTINUE.
        ENDIF.
        INSERT lv_fax INTO TABLE lt_have_fax.
        DATA ls_fax TYPE bus_ei_bupa_fax.
        CLEAR ls_fax.
        ls_fax-contact-task        = c_insert.
        ls_fax-contact-data-fax    = lv_fax.
        ls_fax-contact-data-std_no = lv_first.
        lv_first = abap_false.
        APPEND ls_fax TO ls_adr-data-communication-fax-fax.
      ENDLOOP.
    ENDIF.

    lv_first = xsdbool( iv_change = abap_false ).
    IF it_smtp IS SUPPLIED.
      LOOP AT it_smtp ASSIGNING FIELD-SYMBOL(<ls_sm>).
        DATA(lv_mail) = comp( is_any = <ls_sm> iv_comp = 'E_MAIL' ).
        DATA(lv_mkey) = to_upper( lv_mail ).
        IF lv_mail IS INITIAL OR line_exists( lt_have_mail[ table_line = lv_mkey ] ).
          CONTINUE.
        ENDIF.
        INSERT lv_mkey INTO TABLE lt_have_mail.
        DATA ls_smt TYPE bus_ei_bupa_smtp.
        CLEAR ls_smt.
        ls_smt-contact-task        = c_insert.
        ls_smt-contact-data-e_mail = lv_mail.
        ls_smt-contact-data-std_no = lv_first.
        lv_first = abap_false.
        APPEND ls_smt TO ls_adr-data-communication-smtp-smtp.
      ENDLOOP.
    ENDIF.

    IF iv_change = abap_false
    OR ls_adr-data-postal-datax IS NOT INITIAL
    OR ls_adr-data-communication-phone-phone IS NOT INITIAL
    OR ls_adr-data-communication-fax-fax IS NOT INITIAL
    OR ls_adr-data-communication-smtp-smtp IS NOT INITIAL.
      APPEND ls_adr TO ls_data-partner-central_data-address-addresses.
    ENDIF.

    " ---- business partner: tax numbers ----------------------------------
    " GSTIN (tax number 3) and tax number 5 belong to the partner, filed under
    " category <country>3 / <country>5. CVI copies them to LFA1.
    DATA(lt_txn) = VALUE string_table( ( |3;{ is_central-data-stcd3 }| )
                                       ( |5;{ is_central-data-stcd5 }| ) ).
    LOOP AT lt_txn INTO DATA(lv_tp).
      SPLIT lv_tp AT ';' INTO DATA(lv_digit) DATA(lv_num).
      CONDENSE lv_num.
      IF lv_num IS INITIAL.
        CONTINUE.
      ENDIF.
      IF lv_land IS INITIAL.
        add( EXPORTING iv_text = |Tax number { lv_num } needs the vendor's country - | &&
                                 |the tax number category is chosen by country|
             CHANGING  ct      = et_error ).
        RETURN.
      ENDIF.
      DATA lv_ttype TYPE bptaxtype.
      lv_ttype = |{ lv_land }{ lv_digit }|.
      DATA lv_tok TYPE abap_bool.
      CLEAR lv_tok.
      SELECT SINGLE @abap_true FROM tfktaxnumtype
        WHERE taxtype = @lv_ttype INTO @lv_tok.
      IF lv_tok = abap_false.
        add( EXPORTING iv_text = |Tax number { lv_num }: tax number category { lv_ttype } does not | &&
                                 |exist (Business Partner > Basic Settings > Tax Numbers)|
             CHANGING  ct      = et_error ).
        RETURN.
      ENDIF.
      DATA: lv_old   TYPE bptaxnum,
            lv_oldxl TYPE bptaxnumxl.
      CLEAR: lv_old, lv_oldxl.
      IF lv_bp IS NOT INITIAL.
        SELECT SINGLE taxnum, taxnumxl FROM dfkkbptaxnum
          WHERE partner = @lv_bp AND taxtype = @lv_ttype
          INTO (@lv_old, @lv_oldxl).
      ENDIF.
      DATA(lv_has) = xsdbool( lv_old IS NOT INITIAL OR lv_oldxl IS NOT INITIAL ).
      IF lv_has = abap_true AND ( lv_old = lv_num OR lv_oldxl = lv_num ).
        CONTINUE.                                  " unchanged
      ENDIF.
      DATA ls_tx TYPE bus_ei_bupa_taxnumber.
      CLEAR ls_tx.
      ls_tx-task             = COND #( WHEN lv_has = abap_true THEN c_update ELSE c_insert ).
      ls_tx-data_key-taxtype = lv_ttype.
      IF strlen( lv_num ) > 20.
        ls_tx-data_key-taxnumxl = lv_num.
      ELSE.
        ls_tx-data_key-taxnumber = lv_num.
      ENDIF.
      APPEND ls_tx TO ls_data-partner-central_data-taxnumber-taxnumbers.
    ENDLOOP.

    " ---- supplier: central data -----------------------------------------
    DATA ls_vc TYPE vmds_ei_vmd_central.
    CLEAR ls_vc.
    copy( EXPORTING is_src  = is_central-data
                    iv_skip = COND string( WHEN iv_change = abap_true
                                           THEN `STCD3,STCD5,KTOKK`
                                           ELSE `STCD3,STCD5` )
          CHANGING  cs_data  = ls_vc-data
                    cs_datax = ls_vc-datax ).
    " PAN, GST vendor classification and MSME status are supplier fields.
    copy( EXPORTING is_src  = is_cin
                    iv_only = `J_1IPANNO,VEN_CLASS,J_1ISSIST`
          CHANGING  cs_data  = ls_vc-data
                    cs_datax = ls_vc-datax ).
    " Fields the caller wants sent even when blank: a blank clears them.
    IF iv_keep_blank IS NOT INITIAL.
      DATA lv_keep TYPE string.
      lv_keep = to_upper( iv_keep_blank ).
      SPLIT lv_keep AT ',' INTO TABLE DATA(lt_keep).
      FIELD-SYMBOLS <lv_kv> TYPE any.
      LOOP AT lt_keep INTO DATA(lv_kf).
        CONDENSE lv_kf.
        IF lv_kf IS INITIAL OR lv_kf = 'STCD3' OR lv_kf = 'STCD5' OR lv_kf = 'KTOKK'.
          CONTINUE.
        ENDIF.
        UNASSIGN <lv_kv>.
        ASSIGN COMPONENT lv_kf OF STRUCTURE is_cin TO <lv_kv>.
        IF sy-subrc <> 0.
          ASSIGN COMPONENT lv_kf OF STRUCTURE is_central-data TO <lv_kv>.
        ENDIF.
        IF <lv_kv> IS ASSIGNED AND <lv_kv> IS INITIAL.
          set( EXPORTING iv_comp = lv_kf iv_value = <lv_kv>
               CHANGING  cs_data = ls_vc-data cs_datax = ls_vc-datax ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    " ---- industry key ----------------------------------------------------
    " ECC kept it in LFA1-BRSCH. In S/4HANA the business partner has its own
    " industry sectors (BUT0IS), each filed under an industry system
    " (TB038A); Cipla's vendor template of November 2026 maintains those.
    " The key goes wherever it is defined: the supplier's BRSCH when T016
    " has it, the partner's industry sector when exactly one industry system
    " holds it.
    DATA(lv_ind) = to_upper( condense( CONV string( iv_industry ) ) ).
    IF lv_ind IS NOT INITIAL.
      DATA lv_brsch TYPE brsch.
      lv_brsch = lv_ind.
      DATA lv_t016 TYPE abap_bool.
      CLEAR lv_t016.
      SELECT SINGLE @abap_true FROM t016 WHERE brsch = @lv_brsch INTO @lv_t016.
      IF lv_t016 = abap_true.
        set( EXPORTING iv_comp = 'BRSCH' iv_value = lv_brsch
             CHANGING  cs_data = ls_vc-data cs_datax = ls_vc-datax ).
      ENDIF.
      DATA lv_sec TYPE bu_indsect.
      lv_sec = lv_ind.
      SELECT istype FROM tb038a WHERE ind_sector = @lv_sec
        ORDER BY istype INTO TABLE @DATA(lt_istype).
      IF lines( lt_istype ) = 1.
        DATA(lv_istype) = lt_istype[ 1 ]-istype.
        DATA lv_ihave TYPE abap_bool.
        CLEAR lv_ihave.
        IF lv_bp IS NOT INITIAL.
          SELECT SINGLE @abap_true FROM but0is
            WHERE partner = @lv_bp AND istype = @lv_istype AND ind_sector = @lv_sec
            INTO @lv_ihave.
        ENDIF.
        IF lv_ihave = abap_false.
          DATA ls_is TYPE bus_ei_bupa_industrysector.
          CLEAR ls_is.
          ls_is-task                = c_insert.
          ls_is-data_key-keysystem  = lv_istype.
          ls_is-data_key-ind_sector = lv_sec.
          IF iv_change = abap_false.
            ls_is-data-ind_default  = abap_true.
            ls_is-datax-ind_default = abap_true.
          ENDIF.
          APPEND ls_is TO ls_data-partner-central_data-industry-industries.
        ENDIF.
      ELSEIF lv_t016 = abap_false.
        add( EXPORTING iv_text = COND string(
                         WHEN lt_istype IS INITIAL
                         THEN |Industry key { lv_ind } is neither an industry (T016) nor an | &&
                              |industry sector of an industry system (TB038A)|
                         ELSE |Industry key { lv_ind } is an industry sector of { lines( lt_istype ) } | &&
                              |industry systems (TB038A) and no industry (T016) - it cannot be placed| )
             CHANGING  ct      = et_error ).
        RETURN.
      ENDIF.
    ENDIF.

    " The trading partner has an ALPHA domain (RCOMP, 6).
    DATA(lv_vbund) = comp( is_any = ls_vc-data iv_comp = 'VBUND' ).
    IF lv_vbund IS NOT INITIAL AND lv_vbund CO '0123456789'.
      set( EXPORTING iv_comp = 'VBUND' iv_value = |{ lv_vbund ALPHA = IN WIDTH = 6 }|
           CHANGING  cs_data = ls_vc-data cs_datax = ls_vc-datax ).
    ENDIF.
    ls_data-vendor-central_data-central = ls_vc.

    " ---- supplier: bank details -----------------------------------------
    " FK02 took the file's account into line 1 of the bank details: the
    " vendor's first account (in key order) was replaced, the others stayed.
    IF iv_change = abap_true AND iv_bank_replace = abap_true AND it_bank IS SUPPLIED.
      DATA: lv_nbanks TYPE banks,
            lv_nbankl TYPE bankk,
            lv_nbankn TYPE bankn.
      LOOP AT it_bank ASSIGNING FIELD-SYMBOL(<ls_nb>).
        lv_nbankn = comp( is_any = <ls_nb> iv_comp = 'BANKN' ).
        lv_nbankl = to_upper( comp( is_any = <ls_nb> iv_comp = 'BANKL' ) ).
        lv_nbanks = to_upper( comp( is_any = <ls_nb> iv_comp = 'BANKS' ) ).
        IF lv_nbanks IS INITIAL.
          lv_nbanks = lv_land.
        ENDIF.
        IF lv_nbankn IS NOT INITIAL AND lv_nbankl IS NOT INITIAL.
          EXIT.
        ENDIF.
        CLEAR: lv_nbanks, lv_nbankl, lv_nbankn.
      ENDLOOP.
      IF lv_nbankn IS NOT INITIAL.
        SELECT banks, bankl, bankn FROM lfbk WHERE lifnr = @lv_lifnr
          ORDER BY banks, bankl, bankn INTO TABLE @DATA(lt_oldbk).
        READ TABLE lt_oldbk INTO DATA(ls_oldbk) INDEX 1.
        IF sy-subrc = 0
        AND NOT ( ls_oldbk-banks = lv_nbanks AND ls_oldbk-bankl = lv_nbankl
                  AND ls_oldbk-bankn = lv_nbankn ).
          DATA ls_bkdel TYPE cvis_ei_cvi_bankdetail.
          CLEAR ls_bkdel.
          ls_bkdel-task           = c_delete.
          ls_bkdel-data_key-banks = ls_oldbk-banks.
          ls_bkdel-data_key-bankl = ls_oldbk-bankl.
          ls_bkdel-data_key-bankn = ls_oldbk-bankn.
          APPEND ls_bkdel TO ls_data-vendor-central_data-bankdetail-bankdetails.
        ENDIF.
      ENDIF.
    ENDIF.
    IF it_bank IS SUPPLIED.
      LOOP AT it_bank ASSIGNING FIELD-SYMBOL(<ls_bank>).
        DATA(lv_bankn) = comp( is_any = <ls_bank> iv_comp = 'BANKN' ).
        DATA(lv_bankl) = to_upper( comp( is_any = <ls_bank> iv_comp = 'BANKL' ) ).
        DATA(lv_banks) = to_upper( comp( is_any = <ls_bank> iv_comp = 'BANKS' ) ).
        IF lv_bankn IS INITIAL OR lv_bankl IS INITIAL.
          CONTINUE.
        ENDIF.
        IF lv_banks IS INITIAL.
          lv_banks = lv_land.
        ENDIF.
        DATA lv_bhave TYPE abap_bool.
        CLEAR lv_bhave.
        IF iv_change = abap_true.
          SELECT SINGLE @abap_true FROM lfbk
            WHERE lifnr = @lv_lifnr AND banks = @lv_banks
              AND bankl = @lv_bankl AND bankn = @lv_bankn INTO @lv_bhave.
        ENDIF.
        DATA ls_bk TYPE cvis_ei_cvi_bankdetail.
        CLEAR ls_bk.
        ls_bk-task           = COND #( WHEN lv_bhave = abap_true THEN c_update ELSE c_insert ).
        ls_bk-data_key-banks = lv_banks.
        ls_bk-data_key-bankl = lv_bankl.
        ls_bk-data_key-bankn = lv_bankn.
        DATA(lv_koinh) = comp( is_any = <ls_bank> iv_comp = 'KOINH' ).
        IF lv_koinh IS NOT INITIAL.
          set( EXPORTING iv_comp = 'KOINH' iv_value = lv_koinh
               CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
        ENDIF.
        APPEND ls_bk TO ls_data-vendor-central_data-bankdetail-bankdetails.
      ENDLOOP.
    ENDIF.

    " ---- supplier: company code and withholding tax ---------------------
    DATA lv_bukrs TYPE bukrs.
    ASSIGN COMPONENT 'DATA_KEY' OF STRUCTURE is_company TO FIELD-SYMBOL(<ls_ck>).
    IF sy-subrc = 0.
      lv_bukrs = comp( is_any = <ls_ck> iv_comp = 'BUKRS' ).
    ENDIF.
    IF lv_bukrs IS NOT INITIAL.
      DATA ls_cc TYPE vmds_ei_company.
      CLEAR ls_cc.
      DATA lv_chave TYPE abap_bool.
      CLEAR lv_chave.
      IF iv_change = abap_true.
        SELECT SINGLE @abap_true FROM lfb1
          WHERE lifnr = @lv_lifnr AND bukrs = @lv_bukrs INTO @lv_chave.
      ENDIF.
      ls_cc-task           = COND #( WHEN lv_chave = abap_true THEN c_update ELSE c_insert ).
      ls_cc-data_key-bukrs = lv_bukrs.
      ASSIGN COMPONENT 'DATA' OF STRUCTURE is_company TO FIELD-SYMBOL(<ls_cd>).
      IF sy-subrc = 0.
        copy( EXPORTING is_src  = <ls_cd>
              CHANGING  cs_data  = ls_cc-data
                        cs_datax = ls_cc-datax ).
      ENDIF.
      " Reconciliation account and planning group carry the ALPHA exit.
      DATA(lt_alpha) = VALUE string_table( ( `AKONT` ) ( `FDGRV` ) ).
      LOOP AT lt_alpha INTO DATA(lv_af).
        DATA(lv_av) = comp( is_any = ls_cc-data iv_comp = lv_af ).
        IF lv_av IS NOT INITIAL AND lv_av CO '0123456789'.
          set( EXPORTING iv_comp = lv_af iv_value = |{ lv_av ALPHA = IN WIDTH = 10 }|
               CHANGING  cs_data = ls_cc-data cs_datax = ls_cc-datax ).
        ENDIF.
      ENDLOOP.

      IF it_wtax IS SUPPLIED.
        LOOP AT it_wtax ASSIGNING FIELD-SYMBOL(<ls_wt>).
          DATA(lv_witht) = to_upper( comp( is_any = <ls_wt> iv_comp = 'WITHT' ) ).
          IF lv_witht IS INITIAL.
            CONTINUE.
          ENDIF.
          DATA lv_whave TYPE abap_bool.
          CLEAR lv_whave.
          IF iv_change = abap_true.
            SELECT SINGLE @abap_true FROM lfbw
              WHERE lifnr = @lv_lifnr AND bukrs = @lv_bukrs AND witht = @lv_witht
              INTO @lv_whave.
          ENDIF.
          DATA ls_wtx TYPE vmds_ei_wtax_type.
          CLEAR ls_wtx.
          ls_wtx-task           = COND #( WHEN lv_whave = abap_true THEN c_update ELSE c_insert ).
          ls_wtx-data_key-witht = lv_witht.
          copy( EXPORTING is_src  = <ls_wt>
                          iv_skip = `WITHT`
                CHANGING  cs_data  = ls_wtx-data
                          cs_datax = ls_wtx-datax ).
          APPEND ls_wtx TO ls_cc-wtax_type-wtax_type.
        ENDLOOP.
      ENDIF.
      APPEND ls_cc TO ls_data-vendor-company_data-company.
    ENDIF.

    " ---- supplier: purchasing organisation ------------------------------
    DATA lv_ekorg TYPE ekorg.
    ASSIGN COMPONENT 'DATA_KEY' OF STRUCTURE is_purch TO FIELD-SYMBOL(<ls_pk>).
    IF sy-subrc = 0.
      lv_ekorg = comp( is_any = <ls_pk> iv_comp = 'EKORG' ).
    ENDIF.
    IF lv_ekorg IS NOT INITIAL.
      DATA ls_po TYPE vmds_ei_purchasing.
      CLEAR ls_po.
      DATA lv_phave TYPE abap_bool.
      CLEAR lv_phave.
      IF iv_change = abap_true.
        SELECT SINGLE @abap_true FROM lfm1
          WHERE lifnr = @lv_lifnr AND ekorg = @lv_ekorg INTO @lv_phave.
      ENDIF.
      ls_po-task           = COND #( WHEN lv_phave = abap_true THEN c_update ELSE c_insert ).
      ls_po-data_key-ekorg = lv_ekorg.
      ASSIGN COMPONENT 'DATA' OF STRUCTURE is_purch TO FIELD-SYMBOL(<ls_pd>).
      IF sy-subrc = 0.
        copy( EXPORTING is_src  = <ls_pd>
              CHANGING  cs_data  = ls_po-data
                        cs_datax = ls_po-datax ).
      ENDIF.
      APPEND ls_po TO ls_data-vendor-purchasing_data-purchasing.
    ENDIF.

    " ---- validate, maintain, commit -------------------------------------
    IF save( EXPORTING is_data = ls_data CHANGING ct_error = et_error ) = abap_false.
      RETURN.
    ENDIF.

    " ---- the vendor CVI made ---------------------------------------------
    DATA lv_new TYPE lifnr.
    SELECT SINGLE vendor FROM cvi_vend_link
      WHERE partner_guid = @lv_guid INTO @lv_new.
    SELECT SINGLE partner FROM but000
      WHERE partner_guid = @lv_guid INTO @lv_bp.
    IF lv_new IS INITIAL.
      add( EXPORTING iv_text = COND string(
                       WHEN lv_bp IS INITIAL
                       THEN `Nothing was created: no business partner and no supplier exist for this record`
                       ELSE |Business partner { lv_bp ALPHA = OUT } was saved, but no supplier is | &&
                            |linked to it - check its supplier roles (CVIC_VEND_TO_BP2)| )
           CHANGING  ct      = et_error ).
      RETURN.
    ENDIF.
    SELECT SINGLE adrnr FROM lfa1 WHERE lifnr = @lv_new INTO @lv_adrnr.

    " ---- TAN exemptions (J_1ITAN_EXEM_SAVE, as in ECC) -------------------
    IF it_tan IS SUPPLIED AND lv_bukrs IS NOT INITIAL.
      DATA lt_exem TYPE STANDARD TABLE OF fiwtin_tan_exem WITH EMPTY KEY.
      DATA(lv_pan) = comp( is_any = is_cin iv_comp = 'J_1IPANNO' ).
      LOOP AT it_tan ASSIGNING FIELD-SYMBOL(<ls_tan>).
        DATA(lv_twt) = comp( is_any = <ls_tan> iv_comp = 'WITHT' ).
        IF lv_twt IS INITIAL.
          CONTINUE.
        ENDIF.
        DATA ls_ex TYPE fiwtin_tan_exem.
        CLEAR ls_ex.
        MOVE-CORRESPONDING <ls_tan> TO ls_ex.
        ls_ex-bukrs            = lv_bukrs.
        ls_ex-koart            = 'K'.
        ls_ex-accno            = lv_new.
        ls_ex-fiwtin_tanex_sub = abap_true.
        IF ls_ex-pan_no IS INITIAL.
          ls_ex-pan_no = lv_pan.
        ENDIF.
        APPEND ls_ex TO lt_exem.
      ENDLOOP.
      IF lt_exem IS NOT INITIAL.
        CALL FUNCTION 'J_1ITAN_EXEM_SAVE' IN UPDATE TASK
          TABLES it_tan_exem = lt_exem.
        COMMIT WORK AND WAIT.
        IF sy-subrc <> 0.
          add( EXPORTING iv_type = 'W'
                         iv_text = |Vendor { lv_new ALPHA = OUT } was saved, but the TAN exemption | &&
                                   |update terminated (SM13)|
               CHANGING  ct      = et_error ).
        ENDIF.
      ENDIF.
    ENDIF.

    " The RFC's format: "<vendor>;<address number> <text>".
    APPEND VALUE bapiret2(
      type    = 'S'
      message = |{ lv_new };{ lv_adrnr } Vendor { lv_new ALPHA = OUT } | &&
                |{ COND string( WHEN iv_change = abap_true THEN `changed` ELSE `created` ) } | &&
                |as business partner { lv_bp ALPHA = OUT }| ) TO et_success.
  ENDMETHOD.

ENDCLASS.
