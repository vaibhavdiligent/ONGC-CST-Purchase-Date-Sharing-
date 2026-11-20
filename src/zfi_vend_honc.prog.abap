*&---------------------------------------------------------------------*
*& Report  ZFI_VEND_HONC
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Title     : ZFI_VEND_HONC
* Author            : Shivaji Ingole
* Program Type      : Excutable Program
* Creation Date     : 12-11-2020
* Transaction       : ZFI_VEND_HONC
* Functional design : FS_FI_344_V1.0
* Technical design  : TS_FI_344_V1.0
* Change request    : 8000006708
* Transport Request : CRDK974526
* Description       : Program For Bank Key And Vendor Master Creation
*-----------------------------------------------------------------------*
******Modification log
* Author            : Fatima zia
* Program Type      : Excutable Program
* Change Date       : 27.05.2022
* Transaction       : ZFI_VEND_HONC
* Change request    : 8000007904
* Transport Request : CRDK983367
* Description       :Vendor code generation
*-----------------------------------------------------------------------*
******Modification log
* Author            : Anuj Nandapurkar
* Program Type      : Excutable Program
* Change Date       : 27.05.2022
* Functional design : Pankaj Sawant
* Transaction       : ZFI_VEND_HONC
* Change request    : 8000009272
* Transport Request : CRDK988488
* Description       :Vendor code generation
*-----------------------------------------------------------------------*
******Modification log
* Author            : Gajanan Patil
* Program Type      : Excutable Program
* Change Date       : 15.03.2024
* Functional design : Veera
* Transaction       : ZFI_VEND_HONC
* Change request    : 8000003530
* Transport Request : CRDK990044
* Description       : 3 fields added in incoming file
*-----------------------------------------------------------------------*
******Modification log
* Author            : Gauri Tandel
* Change Date       : 12.06.2024, 21.08.2024
* Functional design : Veera
* Transaction       : ZFI_VEND_HONC
* Change request    : 8000003666, 8000003829
* Transport Request : CRDK991347, CRDK991369,CRDK992195
* Description       : validation added for bank key and Pan no
*                     fields added in BDC and logic for creation of vendor
*-----------------------------------------------------------------------*
******Modification log
* Change Date       : 18.11.2026
* Transaction       : ZFI_VEND_HONC
* Description       : S/4HANA conversion - the vendor is a business
*                     partner. FK01 / FK02 are gone in S/4HANA, so the
*                     batch input that created and changed the vendor is
*                     replaced by local class LCL_HONC_BP in this
*                     program (CL_MD_BP_MAINTAIN): BP grouping and supplier roles
*                     from account group Z001, name / address / mobile /
*                     e-mail on the partner, GSTIN as the partner's tax
*                     number IN3, bank, company code 1000, withholding
*                     tax, PAN and GST classification on the supplier.
*                     The file from AL11, the checks, ZFI_HONC_VEND, the
*                     mails and the archive are unchanged.
*-----------------------------------------------------------------------*

REPORT zfi_vend_honc.

INCLUDE zfi_vend_honc_dd.
INCLUDE zfi_vend_honc_ss.

*----------------------------------------------------------------------*
* S/4HANA: the HONC vendor as a business partner - LCL_HONC_BP
*----------------------------------------------------------------------*
* FK01 and FK02 are gone in S/4HANA: a supplier is a business partner,
* created and changed through CL_MD_BP_MAINTAIN; CVI writes LFA1, LFB1,
* LFBK, LFBW from it. This class is ZFI_VEND_HONC's own - the CIMMRA
* interface has its own in ZFI_CIMMRA_BP, and the two are kept apart.
*
* What goes where:
* - name, title, search term, address, telephone, fax, e-mail -> the
*   business partner; a change updates its standard address, keyed by
*   the address GUID (BUT020), and overwrites the standard numbers by
*   their CONSNUMBER, as FK02 typed over them
* - GSTIN (STCD3) -> the partner's tax number IN3 (DFKKBPTAXNUM)
* - account group -> BP grouping and supplier roles (CVIC_VEND_TO_BP1/2)
* - bank details -> the business partner (BUT0BK); CVI writes LFBK
* - industry key -> LFA1-BRSCH (T016) and/or the partner's industry sector
*   (TB038A); an unchanged key is not sent on a change
* - PAN, GST classification, company code, withholding tax -> supplier
*
* ABAP Cloud: CL_MD_BP_MAINTAIN, CVI_VEND_LINK and the CVIC_* tables are
* not released for ABAP Cloud - classic ABAP (tier 2). Run ATC with
* variant ABAP_CLOUD_READINESS and record them as known exemptions.
*----------------------------------------------------------------------*
CLASS lcl_honc_bp DEFINITION FINAL.
  PUBLIC SECTION.
    "! A new vendor. IV_LIFNR only for an account group with external
    "! numbering; the business partner then gets the same number.
    "! Options of CREATE and CHANGE:
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
    "! IV_COMM_REPLACE  - CHANGE: a new telephone, mobile, fax or e-mail
    "!                   overwrites the partner's standard one of its kind (by
    "!                   its CONSNUMBER), as FK02 did, instead of being added.
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
                iv_comm_replace TYPE abap_bool DEFAULT abap_false
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
                iv_comm_replace TYPE abap_bool DEFAULT abap_false
      EXPORTING et_error   TYPE bapiret2_t
                et_success TYPE bapiret2_t.




  PRIVATE SECTION.
    CONSTANTS: c_insert TYPE c LENGTH 1 VALUE 'I',
               c_update TYPE c LENGTH 1 VALUE 'U'.

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
                iv_comm_replace TYPE abap_bool DEFAULT abap_false
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
      IMPORTING is_data     TYPE cvis_ei_extern
      CHANGING  ct_error    TYPE bapiret2_t
      RETURNING VALUE(rv)   TYPE abap_bool.

    "! Undo what is not committed and refresh the partner's buffers.
    CLASS-METHODS discard.

    "! What the partner looks like where SAP refused the change: its bank
    "! details for R1 228, its industries and the CVI industry assignment
    "! for CVI_MAPPING 002, its telephone numbers for AM 605 - so the log
    "! shows the data the message is about, not only the message.
    CLASS-METHODS diagnose
      IMPORTING iv_bp    TYPE bu_partner
                iv_lifnr TYPE lifnr
                iv_adrnr TYPE adrnr
      CHANGING  ct_error TYPE bapiret2_t.

    "! The rows of a customizing table that hold one of two values, as text.
    CLASS-METHODS rows_with
      IMPORTING iv_tab    TYPE tabname
                iv_v1     TYPE clike
                iv_v2     TYPE clike
      RETURNING VALUE(rv) TYPE string.

ENDCLASS.


CLASS lcl_honc_bp IMPLEMENTATION.

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
                    iv_comm_replace = iv_comm_replace
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
                    iv_comm_replace = iv_comm_replace
          IMPORTING et_error   = et_error
                    et_success = et_success ).
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
    " Every save is validated first. An error SAP finds only at COMMIT WORK
    " cannot come back as a message - the session ends in a short dump:
    " UNCAUGHT_EXCEPTION CX_FSBP_PARTNER_INCONSISTENT "Data for business
    " partner 100071023 contains errors" (CRS, 20.11.2026, when changes were
    " posted without validation). VALIDATE_SINGLE finds it and returns it.
    TRY.
        cl_md_bp_maintain=>validate_single(
          EXPORTING i_data        = is_data
          IMPORTING et_return_map = lt_map ).
      CATCH cx_root INTO DATA(lx_val).
        discard( ).
        add( EXPORTING iv_text = |Validation failed: { lx_val->get_text( ) }|
             CHANGING  ct      = ct_error ).
        RETURN.
    ENDTRY.
    " Whatever the validation left in the partner's buffers goes before the
    " real posting, so that a new bank account or number does not meet itself.
    discard( ).
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
        discard( ).
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
      discard( ).
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




  METHOD discard.
    " Roll back and refresh: BAPI_TRANSACTION_ROLLBACK also refreshes the
    " buffers the business partner functions registered, a bare ROLLBACK WORK
    " does not.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    reset_bp( ).
  ENDMETHOD.

  METHOD rows_with.
    " Read without knowing the field names: every row with one of the two
    " values in any field, fields joined by "/" (the client left out).
    DATA lr_tab TYPE REF TO data.
    FIELD-SYMBOLS: <lt_any> TYPE STANDARD TABLE,
                   <ls_any> TYPE any,
                   <lv_any> TYPE any.
    CLEAR rv.
    DATA(lv_n) = 0.
    TRY.
        CREATE DATA lr_tab TYPE STANDARD TABLE OF (iv_tab).
        ASSIGN lr_tab->* TO <lt_any>.
        SELECT * FROM (iv_tab) INTO TABLE @<lt_any> UP TO 20000 ROWS.
        LOOP AT <lt_any> ASSIGNING <ls_any>.
          DATA(lv_row) = VALUE string( ).
          DATA(lv_hit) = abap_false.
          DO.
            ASSIGN COMPONENT sy-index OF STRUCTURE <ls_any> TO <lv_any>.
            IF sy-subrc <> 0.
              EXIT.
            ENDIF.
            IF sy-index = 1.
              CONTINUE.
            ENDIF.
            DATA(lv_val) = condense( CONV string( <lv_any> ) ).
            IF lv_val IS NOT INITIAL AND ( lv_val = iv_v1 OR lv_val = iv_v2 ).
              lv_hit = abap_true.
            ENDIF.
            lv_row = COND #( WHEN lv_row IS INITIAL THEN lv_val ELSE |{ lv_row }/{ lv_val }| ).
          ENDDO.
          IF lv_hit = abap_true AND lv_n < 6.
            lv_n = lv_n + 1.
            rv = COND #( WHEN rv IS INITIAL THEN lv_row ELSE |{ rv }; { lv_row }| ).
          ENDIF.
        ENDLOOP.
      CATCH cx_root.
        rv = 'not readable'.
        RETURN.
    ENDTRY.
    IF rv IS INITIAL.
      rv = 'no entry'.
    ENDIF.
  ENDMETHOD.

  METHOD diagnose.
    DATA: lv_bank TYPE abap_bool,
          lv_indu TYPE abap_bool,
          lv_tel  TYPE abap_bool.
    LOOP AT ct_error INTO DATA(ls_err).
      IF ( ls_err-id = 'R1' AND ls_err-number = '228' )
      OR ls_err-message CS 'validity period are not unique'.
        lv_bank = abap_true.
      ENDIF.
      IF ( ls_err-id = 'CVI_MAPPING' AND ls_err-number = '002' )
      OR ( ls_err-message CS 'Industry' AND ls_err-message CS 'could not be assigned' ).
        lv_indu = abap_true.
      ENDIF.
      IF ( ls_err-id = 'AM' AND ls_err-number = '605' )
      OR ls_err-message CS 'Assignment of usage'.
        lv_tel = abap_true.
      ENDIF.
    ENDLOOP.

    IF lv_bank = abap_true.
      SELECT bkvid, banks, bankl, bankn FROM but0bk
        WHERE partner = @iv_bp ORDER BY bkvid INTO TABLE @DATA(lt_bk).
      DATA(lv_txt) = VALUE string( ).
      LOOP AT lt_bk INTO DATA(ls_bk).
        lv_txt = |{ lv_txt }{ COND string( WHEN lv_txt IS NOT INITIAL THEN `; ` ) }| &&
                 |{ ls_bk-bkvid } { ls_bk-banks }/{ ls_bk-bankl }/{ ls_bk-bankn }|.
      ENDLOOP.
      APPEND VALUE bapiret2( type = 'E'
        message = |Bank details of business partner { iv_bp ALPHA = OUT } now: | &&
                  |{ COND string( WHEN lv_txt IS INITIAL THEN `none` ELSE lv_txt ) } - an account that | &&
                  |stands there twice has to be removed in BP (Payment Transactions) first| ) TO ct_error.
    ENDIF.

    IF lv_indu = abap_true.
      SELECT istype, ind_sector, isdef FROM but0is
        WHERE partner = @iv_bp ORDER BY istype, ind_sector INTO TABLE @DATA(lt_is).
      DATA: lv_itxt TYPE string,
            lv_std  TYPE string.
      LOOP AT lt_is INTO DATA(ls_is).
        lv_itxt = |{ lv_itxt }{ COND string( WHEN lv_itxt IS NOT INITIAL THEN `, ` ) }| &&
                  |{ ls_is-istype }/{ ls_is-ind_sector }| &&
                  |{ COND string( WHEN ls_is-isdef = abap_true THEN ` (standard)` ) }|.
        IF ls_is-isdef = abap_true.
          lv_std = ls_is-ind_sector.
        ENDIF.
      ENDLOOP.
      DATA lv_brsch TYPE brsch.
      SELECT SINGLE brsch FROM lfa1 WHERE lifnr = @iv_lifnr INTO @lv_brsch.
      APPEND VALUE bapiret2( type = 'E'
        message = |Business partner { iv_bp ALPHA = OUT } has the industries | &&
                  |{ COND string( WHEN lv_itxt IS INITIAL THEN `none` ELSE lv_itxt ) }; its vendor | &&
                  |has the industry key "{ lv_brsch }". SAP refused the save of the partner itself - | &&
                  |no industry was sent| ) TO ct_error.
      APPEND VALUE bapiret2( type = 'E'
        message = |CVI Assign Industries: key to industry (TP038M1) | &&
                  |{ rows_with( iv_tab = 'TP038M1' iv_v1 = lv_brsch iv_v2 = lv_std ) } / industry to key | &&
                  |(TP038M2) { rows_with( iv_tab = 'TP038M2' iv_v1 = lv_brsch iv_v2 = lv_std ) } - the | &&
                  |partner's standard industry needs an entry in both| ) TO ct_error.
    ENDIF.

    IF lv_tel = abap_true AND iv_adrnr IS NOT INITIAL.
      SELECT consnumber, tel_number, r3_user, flgdefault FROM adr2
        WHERE addrnumber = @iv_adrnr ORDER BY consnumber INTO TABLE @DATA(lt_tel).
      DATA lv_ttxt TYPE string.
      LOOP AT lt_tel INTO DATA(ls_tel).
        lv_ttxt = |{ lv_ttxt }{ COND string( WHEN lv_ttxt IS NOT INITIAL THEN `; ` ) }| &&
                  |{ ls_tel-consnumber } { ls_tel-tel_number } type "{ ls_tel-r3_user }"| &&
                  |{ COND string( WHEN ls_tel-flgdefault = abap_true THEN ` default` ) }|.
      ENDLOOP.
      APPEND VALUE bapiret2( type = 'E'
        message = |Telephone numbers of business partner { iv_bp ALPHA = OUT } now: | &&
                  |{ COND string( WHEN lv_ttxt IS INITIAL THEN `none` ELSE lv_ttxt ) }| ) TO ct_error.
    ENDIF.
  ENDMETHOD.

  METHOD post.
    CLEAR: et_error, et_success.

    DATA: ls_data  TYPE cvis_ei_extern,
          lv_guid  TYPE bu_partner_guid,
          lv_lifnr TYPE lifnr,
          lv_land  TYPE land1,
          lv_adrnr TYPE adrnr,
          lv_bp    TYPE bu_partner,
          lv_bptype TYPE bu_type,
          lt_note  TYPE bapiret2_t.

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
      SELECT SINGLE partner, type FROM but000
        WHERE partner_guid = @lv_guid INTO (@lv_bp, @lv_bptype).
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
    " An existing partner that is a person (category 1) has first and last
    " name, not NAME1-4: its name is left as it is, and the log says so.
    DATA(lt_name) = VALUE string_table( ( |NAME1;NAME| ) ( |NAME2;NAME_2| )
                                        ( |NAME3;NAME_3| ) ( |NAME4;NAME_4| ) ).
    IF lv_bptype = '1'.
      IF is_postal-data-name IS NOT INITIAL.
        APPEND VALUE bapiret2( type = 'I'
                               message = |Business partner { lv_bp ALPHA = OUT } is a person - | &&
                                         |its name was not changed (change it in BP)| ) TO lt_note.
      ENDIF.
      CLEAR lt_name.
    ENDIF.
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
    " A change of an existing address is keyed by the address GUID. With the
    " usage XXDFLT alone BP looks for an address under a GUID of its own and
    " refuses the change: "An address with GUID ... does not exist for
    " business partner ...". The GUID is that of the partner's standard
    " address - the vendor's (LFA1-ADRNR), else the one with usage XXDEFAULT.
    DATA: lv_aguid TYPE bu_address_guid,
          lv_bpadr TYPE adrnr.
    IF iv_change = abap_true.
      ls_adr-data_key-operation = 'XXDFLT'.      " the standard address
      IF lv_bp IS NOT INITIAL AND lv_adrnr IS NOT INITIAL.
        SELECT SINGLE address_guid FROM but020
          WHERE partner = @lv_bp AND addrnumber = @lv_adrnr INTO @lv_aguid.
      ENDIF.
      IF lv_aguid IS INITIAL AND lv_bp IS NOT INITIAL.
        SELECT SINGLE addrnumber FROM but021_fs
          WHERE partner = @lv_bp AND adr_kind = 'XXDEFAULT' INTO @DATA(lv_stdadr).
        IF lv_stdadr IS NOT INITIAL.
          SELECT SINGLE address_guid FROM but020
            WHERE partner = @lv_bp AND addrnumber = @lv_stdadr INTO @lv_aguid.
        ENDIF.
      ENDIF.
      ls_adr-data_key-guid = lv_aguid.
      " The numbers and e-mails are read from that address - the partner's,
      " not the supplier's copy of it (a person's supplier has one of its own).
      lv_bpadr = COND #( WHEN lv_stdadr IS NOT INITIAL THEN lv_stdadr ELSE lv_adrnr ).
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
    " The standard entry comes first: with IV_COMM_REPLACE it is the one a new
    " value overwrites, as FK02 did.
    DATA lt_used TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    " No restriction to an empty person number: a partner that is a person has
    " its numbers under its person number, and they were not seen - the mobile
    " was inserted next to the one it has (AM 605, usage AD_MBDEFAU).
    IF iv_change = abap_true AND lv_bpadr IS NOT INITIAL.
      " R_3_USER 3 is the default mobile, 1 the default landline (SAP Help).
      SELECT addrnumber, consnumber, tel_number, flgdefault, r3_user FROM adr2
        WHERE addrnumber = @lv_bpadr
        ORDER BY flgdefault DESCENDING, r3_user DESCENDING, consnumber INTO TABLE @DATA(lt_a2).
      DATA lv_have TYPE string.
      LOOP AT lt_a2 INTO DATA(ls_a2).
        lv_have = condense( CONV string( ls_a2-tel_number ) ).
        INSERT lv_have INTO TABLE lt_have_tel.
      ENDLOOP.
      SELECT addrnumber, consnumber, fax_number, flgdefault FROM adr3
        WHERE addrnumber = @lv_bpadr
        ORDER BY flgdefault DESCENDING, consnumber INTO TABLE @DATA(lt_a3).
      LOOP AT lt_a3 INTO DATA(ls_a3).
        lv_have = condense( CONV string( ls_a3-fax_number ) ).
        INSERT lv_have INTO TABLE lt_have_fax.
      ENDLOOP.
      SELECT addrnumber, consnumber, smtp_addr, flgdefault FROM adr6
        WHERE addrnumber = @lv_bpadr
        ORDER BY flgdefault DESCENDING, consnumber INTO TABLE @DATA(lt_a6).
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
        " Replace: the partner's standard number of the same kind (mobile =
        " R_3_USER 2 or 3) gets the new number; its flags stay as they are.
        IF iv_change = abap_true AND iv_comm_replace = abap_true.
          DATA(lv_ismob) = xsdbool( lv_r3 CA '23' ).
          LOOP AT lt_a2 INTO DATA(ls_o2).
            DATA(lv_ocons) = CONV string( ls_o2-consnumber ).
            IF xsdbool( ls_o2-r3_user CA '23' ) <> lv_ismob
            OR line_exists( lt_used[ table_line = lv_ocons ] ).
              CONTINUE.
            ENDIF.
            INSERT lv_ocons INTO TABLE lt_used.
            CLEAR ls_tel.
            ls_tel-contact-task            = c_update.
            ls_tel-contact-data-consnumber = ls_o2-consnumber.
            set( EXPORTING iv_comp = 'TELEPHONE' iv_value = lv_tel
                 CHANGING  cs_data = ls_tel-contact-data cs_datax = ls_tel-contact-datax ).
            IF lv_ext IS NOT INITIAL.
              set( EXPORTING iv_comp = 'EXTENSION' iv_value = lv_ext
                   CHANGING  cs_data = ls_tel-contact-data cs_datax = ls_tel-contact-datax ).
            ENDIF.
            EXIT.
          ENDLOOP.
        ENDIF.
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
        IF iv_change = abap_true AND iv_comm_replace = abap_true.
          LOOP AT lt_a3 INTO DATA(ls_o3).
            DATA(lv_fcons) = |F{ ls_o3-consnumber }|.
            IF line_exists( lt_used[ table_line = lv_fcons ] ).
              CONTINUE.
            ENDIF.
            INSERT lv_fcons INTO TABLE lt_used.
            CLEAR ls_fax.
            ls_fax-contact-task            = c_update.
            ls_fax-contact-data-consnumber = ls_o3-consnumber.
            set( EXPORTING iv_comp = 'FAX' iv_value = lv_fax
                 CHANGING  cs_data = ls_fax-contact-data cs_datax = ls_fax-contact-datax ).
            EXIT.
          ENDLOOP.
        ENDIF.
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
        IF iv_change = abap_true AND iv_comm_replace = abap_true.
          LOOP AT lt_a6 INTO DATA(ls_o6).
            DATA(lv_scons) = |S{ ls_o6-consnumber }|.
            IF line_exists( lt_used[ table_line = lv_scons ] ).
              CONTINUE.
            ENDIF.
            INSERT lv_scons INTO TABLE lt_used.
            CLEAR ls_smt.
            ls_smt-contact-task            = c_update.
            ls_smt-contact-data-consnumber = ls_o6-consnumber.
            set( EXPORTING iv_comp = 'E_MAIL' iv_value = lv_mail
                 CHANGING  cs_data = ls_smt-contact-data cs_datax = ls_smt-contact-datax ).
            EXIT.
          ENDLOOP.
        ENDIF.
        APPEND ls_smt TO ls_adr-data-communication-smtp-smtp.
      ENDLOOP.
    ENDIF.

    IF iv_change = abap_false
    OR ls_adr-data-postal-datax IS NOT INITIAL
    OR ls_adr-data-communication-phone-phone IS NOT INITIAL
    OR ls_adr-data-communication-fax-fax IS NOT INITIAL
    OR ls_adr-data-communication-smtp-smtp IS NOT INITIAL.
      IF iv_change = abap_true AND lv_aguid IS INITIAL.
        " No standard address found - the rest of the change still goes in.
        APPEND VALUE bapiret2( type = 'I'
                               message = |Business partner { lv_bp ALPHA = OUT } has no standard address | &&
                                         |(BUT020 / BUT021_FS) - address, telephone and e-mail not changed| )
               TO lt_note.
      ELSE.
        APPEND ls_adr TO ls_data-partner-central_data-address-addresses.
      ENDIF.
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
    DATA(lv_ind_file) = lv_ind.
    " A partner that already has the file's industry (BUT0IS, any industry
    " system) gets nothing sent: Cipla keeps the industry on the partner
    " (e.g. 0009 in Standard Industry System 0001), while CVI maps the
    " supplier's industry key 0009 to industry 92 and cannot put that next
    " to it - CVI_MAPPING 002 "Industry 92 for object Business Partner with
    " ID ... could not be assigned" (CRS, BP 100092735, 20.11.2026).
    " More than that: a partner that has ANY industry keeps it on a change.
    " In S/4HANA the supplier's industry key is derived from the partner's
    " standard industry (CVI, Assign Industries); it cannot be set through the
    " supplier (SAP KBA 3581473 - "Industry not updated with ...
    " CL_MD_BP_MAINTAIN", CVI_MAPPING 002). Only a partner without any
    " industry gets the file's one, as its standard industry.
    DATA lv_bp_has_ind TYPE abap_bool.
    CLEAR lv_bp_has_ind.
    IF lv_ind IS NOT INITIAL AND iv_change = abap_true AND lv_bp IS NOT INITIAL.
      SELECT SINGLE @abap_true FROM but0is
        WHERE partner = @lv_bp INTO @lv_bp_has_ind.
      IF lv_bp_has_ind = abap_true.
        CLEAR lv_ind.
      ENDIF.
    ENDIF.
    IF lv_ind IS NOT INITIAL.
      DATA lv_brsch TYPE brsch.
      lv_brsch = lv_ind.
      DATA lv_t016 TYPE abap_bool.
      CLEAR lv_t016.
      SELECT SINGLE @abap_true FROM t016 WHERE brsch = @lv_brsch INTO @lv_t016.
      IF lv_t016 = abap_true AND iv_change = abap_false.
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
          " The partner's first industry: its standard one.
          ls_is-data-ind_default  = abap_true.
          ls_is-datax-ind_default = abap_true.
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

    " An unchanged industry key is not sent on a change. CVI maps BRSCH to
    " the partner's industry (customizing "Assign industries") and inserts
    " it again whenever BRSCH is sent - the partner has it already, and the
    " whole change fails with CVI_MAPPING 002 "Industry 92 for object
    " Business Partner with ID ... could not be assigned" (CRS, 19.11.2026).
    IF iv_change = abap_true.
      ASSIGN COMPONENT 'BRSCH' OF STRUCTURE ls_vc-datax TO FIELD-SYMBOL(<lv_brx>).
      ASSIGN COMPONENT 'BRSCH' OF STRUCTURE ls_vc-data  TO FIELD-SYMBOL(<lv_brv>).
      IF <lv_brx> IS ASSIGNED AND <lv_brv> IS ASSIGNED AND <lv_brx> = abap_true.
        DATA lv_brsch_now TYPE brsch.
        SELECT SINGLE brsch FROM lfa1 WHERE lifnr = @lv_lifnr INTO @lv_brsch_now.
        IF lv_brsch_now = <lv_brv>.
          CLEAR: <lv_brv>, <lv_brx>.
        ENDIF.
      ENDIF.
    ENDIF.

    " The trading partner has an ALPHA domain (RCOMP, 6).
    DATA(lv_vbund) = comp( is_any = ls_vc-data iv_comp = 'VBUND' ).
    IF lv_vbund IS NOT INITIAL AND lv_vbund CO '0123456789'.
      set( EXPORTING iv_comp = 'VBUND' iv_value = |{ lv_vbund ALPHA = IN WIDTH = 6 }|
           CHANGING  cs_data = ls_vc-data cs_datax = ls_vc-datax ).
    ENDIF.
    " On a change only what differs from the supplier as it is is sent. A
    " supplier field in the request - even an unchanged one (KONZS NONFIELD,
    " the GST class), as FK02 always typed them - makes CVI derive the
    " partner's industry from the supplier's industry key again: Cipla's CVI
    " mapping turns 0009 into 92, which cannot sit next to the partner's own
    " 0009 - CVI_MAPPING 002 "Industry 92 for object Business Partner with ID
    " ... could not be assigned" (CRS, BP 100096289, 20.11.2026).
    IF iv_change = abap_true AND ls_vc-datax IS NOT INITIAL.
      SELECT SINGLE * FROM lfa1 WHERE lifnr = @lv_lifnr INTO @DATA(ls_lfa1_now).
      IF sy-subrc = 0.
        DATA lo_vxd TYPE REF TO cl_abap_structdescr.
        lo_vxd ?= cl_abap_typedescr=>describe_by_data( ls_vc-datax ).
        FIELD-SYMBOLS: <lv_vx> TYPE any,
                       <lv_vd> TYPE any,
                       <lv_vo> TYPE any.
        " The included view: the India fields (GST class, PAN) sit in includes.
        DATA(lt_vxc) = lo_vxd->get_included_view( ).
        LOOP AT lt_vxc INTO DATA(ls_vxc).
          IF ls_vxc-type->type_kind <> cl_abap_typedescr=>typekind_char.
            CONTINUE.
          ENDIF.
          UNASSIGN: <lv_vx>, <lv_vd>, <lv_vo>.
          ASSIGN COMPONENT ls_vxc-name OF STRUCTURE ls_vc-datax TO <lv_vx>.
          ASSIGN COMPONENT ls_vxc-name OF STRUCTURE ls_vc-data  TO <lv_vd>.
          ASSIGN COMPONENT ls_vxc-name OF STRUCTURE ls_lfa1_now TO <lv_vo>.
          IF <lv_vx> IS ASSIGNED AND <lv_vd> IS ASSIGNED AND <lv_vo> IS ASSIGNED.
            IF <lv_vx> = abap_true AND <lv_vd> = <lv_vo>.
              CLEAR: <lv_vd>, <lv_vx>.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
    ls_data-vendor-central_data-central = ls_vc.

    " ---- bank details: on the business partner ---------------------------
    " In S/4HANA the business partner's bank details (BUT0BK) lead and CVI
    " writes the supplier's (LFBK) from them - CVI_EI004 "Bank data is
    " provided from the business partner". Each line is keyed by its bank
    " details ID; a new one takes the next free number.
    IF it_bank IS SUPPLIED.
      TYPES: BEGIN OF lty_bk,
               bkvid TYPE bu_bkvid,
               banks TYPE banks,
               bankl TYPE bankk,
               bankn TYPE bankn,
               koinh TYPE koinh_fi,
             END OF lty_bk.
      DATA: lt_bk_old TYPE STANDARD TABLE OF lty_bk WITH EMPTY KEY,
            lt_bk_new TYPE STANDARD TABLE OF lty_bk WITH EMPTY KEY,
            lv_bk_max TYPE i.
      IF lv_bp IS NOT INITIAL.
        SELECT bkvid, banks, bankl, bankn, koinh FROM but0bk
          WHERE partner = @lv_bp
          ORDER BY bkvid
          INTO CORRESPONDING FIELDS OF TABLE @lt_bk_old.
      ENDIF.
      LOOP AT lt_bk_old INTO DATA(ls_bk_old).
        IF ls_bk_old-bkvid CO '0123456789'.
          IF CONV i( ls_bk_old-bkvid ) > lv_bk_max.
            lv_bk_max = CONV i( ls_bk_old-bkvid ).
          ENDIF.
        ENDIF.
      ENDLOOP.

      " The file's accounts, complete ones only.
      LOOP AT it_bank ASSIGNING FIELD-SYMBOL(<ls_bank>).
        DATA ls_bk_in TYPE lty_bk.
        CLEAR ls_bk_in.
        ls_bk_in-bankn = comp( is_any = <ls_bank> iv_comp = 'BANKN' ).
        ls_bk_in-bankl = to_upper( comp( is_any = <ls_bank> iv_comp = 'BANKL' ) ).
        ls_bk_in-banks = to_upper( comp( is_any = <ls_bank> iv_comp = 'BANKS' ) ).
        IF ls_bk_in-banks IS INITIAL.
          ls_bk_in-banks = lv_land.
        ENDIF.
        IF ls_bk_in-bankn IS INITIAL OR ls_bk_in-bankl IS INITIAL.
          CONTINUE.
        ENDIF.
        DATA ls_bk TYPE bus_ei_bupa_bankdetail.
        CLEAR ls_bk.
        READ TABLE lt_bk_old INTO DATA(ls_bk_same)
          WITH KEY banks = ls_bk_in-banks bankl = ls_bk_in-bankl bankn = ls_bk_in-bankn.
        IF sy-subrc = 0.
          " The partner has this account: nothing is sent for it, unless the
          " account holder differs. Sending it again only made SAP check the
          " partner's bank details once more (R1 228).
          ls_bk-task     = c_update.
          ls_bk-data_key = ls_bk_same-bkvid.
          DATA(lv_koinh_in) = comp( is_any = <ls_bank> iv_comp = 'KOINH' ).
          IF lv_koinh_in IS INITIAL
          OR to_upper( lv_koinh_in ) = to_upper( condense( CONV string( ls_bk_same-koinh ) ) ).
            CONTINUE.
          ENDIF.
        ELSEIF iv_change = abap_true AND iv_bank_replace = abap_true
           AND lt_bk_old IS NOT INITIAL AND lt_bk_new IS INITIAL.
          " FK02 typed the file's account over line 1 of the bank details: the
          " vendor's first bank details ID takes the new account
          " in place. Inserting a new ID and deleting the old one left both on
          " the partner - bank details are time-dependent, the delete only ends
          " the old one - and the account showed twice (CRS, 20.11.2026).
          ls_bk-task     = c_update.
          ls_bk-data_key = lt_bk_old[ 1 ]-bkvid.
          set( EXPORTING iv_comp = 'BANK_CTRY' iv_value = ls_bk_in-banks
               CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
          set( EXPORTING iv_comp = 'BANK_KEY' iv_value = ls_bk_in-bankl
               CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
          set( EXPORTING iv_comp = 'BANK_ACCT' iv_value = ls_bk_in-bankn
               CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
          APPEND ls_bk_in TO lt_bk_new.
        ELSE.
          lv_bk_max += 1.
          ls_bk-task     = c_insert.
          ls_bk-data_key = |{ lv_bk_max WIDTH = 4 ALIGN = RIGHT PAD = '0' }|.
          set( EXPORTING iv_comp = 'BANK_CTRY' iv_value = ls_bk_in-banks
               CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
          set( EXPORTING iv_comp = 'BANK_KEY' iv_value = ls_bk_in-bankl
               CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
          set( EXPORTING iv_comp = 'BANK_ACCT' iv_value = ls_bk_in-bankn
               CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
          APPEND ls_bk_in TO lt_bk_new.
        ENDIF.
        DATA(lv_koinh) = comp( is_any = <ls_bank> iv_comp = 'KOINH' ).
        IF lv_koinh IS NOT INITIAL.
          set( EXPORTING iv_comp = 'ACCOUNTHOLDER' iv_value = lv_koinh
               CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
        ENDIF.
        IF ls_bk-task = c_insert OR ls_bk-datax IS NOT INITIAL.
          APPEND ls_bk TO ls_data-partner-central_data-bankdetail-bankdetails.
        ENDIF.
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
    " A change is saved in two steps. The supplier's own fields (corporate
    " group, GST class, industry key) go in a save of their own, after the
    " rest: any of them makes CVI derive the partner's industry from the
    " supplier's industry key again, and with Cipla's mapping (0009 -> 92)
    " that can be refused - CVI_MAPPING 002 "Industry 92 for object Business
    " Partner with ID ... could not be assigned". Saved with the rest, that
    " refusal lost the bank, address and tax number; saved again after it,
    " the partner's buffers still held the first attempt's bank details and
    " the account was written twice (CRS, 20.11.2026). Now the bank, address,
    " communication and tax numbers are saved once, and a refusal of the
    " supplier fields only leaves those unchanged, with a warning.
    DATA ls_vc_later TYPE vmds_ei_vmd_central.
    IF iv_change = abap_true.
      IF ls_data-vendor-central_data-central-datax IS NOT INITIAL.
        ls_vc_later = ls_data-vendor-central_data-central.
      ENDIF.
      " The supplier header stays in the first step: with it SAP runs the
      " partner-to-supplier synchronisation inside the call and reports what
      " it refuses as a message - without it that happened at COMMIT WORK.
      CLEAR ls_data-vendor-central_data-central.
    ENDIF.

    DATA(lv_saved) = save( EXPORTING is_data  = ls_data
                           CHANGING  ct_error = et_error ).

    " ---- the partner's standard industry has no vendor industry key ------
    " SAP derives the supplier's industry key from the partner's STANDARD
    " industry at every save (CVI, Assign Industries). A partner whose
    " standard industry has no key assigned - 92 "Pharmaceutical Trade" on
    " Cipla's doctors, whose vendors have key 0009 - is refused whatever is
    " sent: CVI_MAPPING 002 "Industry 92 for object Business Partner with ID
    " ... could not be assigned" (CRS, BP 100113814, 20.11.2026). The file
    " names the vendor's industry key; the request is then posted with that
    " industry as the partner's standard one - what "industry key 0009" means
    " in S/4HANA. The former industry stays on the partner, no longer as the
    " standard one. SAP validates the request first, nothing is forced.
    IF lv_saved = abap_false AND iv_change = abap_true
    AND lv_ind_file IS NOT INITIAL AND lv_bp IS NOT INITIAL.
      DATA(lv_only_ind) = abap_true.
      LOOP AT et_error INTO DATA(ls_e1).
        IF NOT ( ( ls_e1-id = 'CVI_MAPPING' AND ls_e1-number = '002' )
              OR ( ls_e1-message CS 'Industry' AND ls_e1-message CS 'could not be assigned' ) ).
          lv_only_ind = abap_false.
        ENDIF.
      ENDLOOP.
      IF lv_only_ind = abap_true AND et_error IS NOT INITIAL.
        DATA lv_fsec TYPE bu_indsect.
        lv_fsec = lv_ind_file.
        SELECT istype, ind_sector, isdef FROM but0is
          WHERE partner = @lv_bp ORDER BY istype, ind_sector INTO TABLE @DATA(lt_bpis).
        " The industry system: the one of the partner's standard industry
        " when it holds the file's industry, else the only one that does.
        DATA: lv_fsys   TYPE bu_istype,
              lv_oldstd TYPE bu_indsect.
        CLEAR: lv_fsys, lv_oldstd.
        LOOP AT lt_bpis INTO DATA(ls_bpis) WHERE isdef = abap_true.
          DATA lv_in_sys TYPE abap_bool.
          CLEAR lv_in_sys.
          SELECT SINGLE @abap_true FROM tb038a
            WHERE istype = @ls_bpis-istype AND ind_sector = @lv_fsec INTO @lv_in_sys.
          IF lv_in_sys = abap_true.
            lv_fsys   = ls_bpis-istype.
            lv_oldstd = ls_bpis-ind_sector.
            EXIT.
          ENDIF.
        ENDLOOP.
        IF lv_fsys IS INITIAL.
          SELECT istype FROM tb038a WHERE ind_sector = @lv_fsec
            ORDER BY istype INTO TABLE @DATA(lt_fsys).
          IF lines( lt_fsys ) = 1.
            lv_fsys = lt_fsys[ 1 ]-istype.
            LOOP AT lt_bpis INTO ls_bpis WHERE istype = lv_fsys AND isdef = abap_true.
              lv_oldstd = ls_bpis-ind_sector.
            ENDLOOP.
          ENDIF.
        ENDIF.
        IF lv_fsys IS NOT INITIAL AND lv_oldstd <> lv_fsec.
          DATA ls_alt TYPE cvis_ei_extern.
          ls_alt = ls_data.
          CLEAR ls_alt-partner-central_data-industry-industries.
          DATA ls_isx TYPE bus_ei_bupa_industrysector.
          IF lv_oldstd IS NOT INITIAL.
            CLEAR ls_isx.
            ls_isx-task                = c_update.
            ls_isx-data_key-keysystem  = lv_fsys.
            ls_isx-data_key-ind_sector = lv_oldstd.
            ls_isx-data-ind_default    = abap_false.
            ls_isx-datax-ind_default   = abap_true.
            APPEND ls_isx TO ls_alt-partner-central_data-industry-industries.
          ENDIF.
          CLEAR ls_isx.
          ls_isx-task                = COND #( WHEN line_exists( lt_bpis[ istype     = lv_fsys
                                                                          ind_sector = lv_fsec ] )
                                               THEN c_update ELSE c_insert ).
          ls_isx-data_key-keysystem  = lv_fsys.
          ls_isx-data_key-ind_sector = lv_fsec.
          ls_isx-data-ind_default    = abap_true.
          ls_isx-datax-ind_default   = abap_true.
          APPEND ls_isx TO ls_alt-partner-central_data-industry-industries.

          DATA lt_err_alt TYPE bapiret2_t.
          IF save( EXPORTING is_data  = ls_alt
                   CHANGING  ct_error = lt_err_alt ) = abap_true.
            lv_saved = abap_true.
            CLEAR et_error.
            APPEND VALUE bapiret2(
              type    = 'W'
              message = |Standard industry of business partner { lv_bp ALPHA = OUT } changed | &&
                        |{ COND string( WHEN lv_oldstd IS NOT INITIAL THEN |from { lv_oldstd } | ) }| &&
                        |to { lv_fsec }, the file's industry key: SAP has no vendor industry key | &&
                        |assigned to the former one (CVI, Assign Industries) and refused the partner | &&
                        |with it| ) TO lt_note.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_saved = abap_false.
      IF iv_change = abap_true.
        diagnose( EXPORTING iv_bp    = lv_bp
                            iv_lifnr = lv_lifnr
                            iv_adrnr = lv_bpadr
                  CHANGING  ct_error = et_error ).
      ENDIF.
      RETURN.
    ENDIF.

    IF ls_vc_later-datax IS NOT INITIAL.
      DATA: ls_data2  TYPE cvis_ei_extern,
            lt_err2   TYPE bapiret2_t.
      ls_data2-partner-header-object_task                  = c_update.
      ls_data2-partner-header-object_instance-bpartnerguid = lv_guid.
      ls_data2-vendor-header-object_task                   = c_update.
      ls_data2-vendor-header-object_instance-lifnr         = lv_lifnr.
      ls_data2-vendor-central_data-central                 = ls_vc_later.
      IF save( EXPORTING is_data  = ls_data2
               CHANGING  ct_error = lt_err2 ) = abap_false.
        DATA(lv_err2) = VALUE string( ).
        LOOP AT lt_err2 INTO DATA(ls_e2).
          lv_err2 = COND #( WHEN lv_err2 IS INITIAL THEN CONV string( ls_e2-message )
                            ELSE |{ lv_err2 } / { ls_e2-message }| ).
        ENDLOOP.
        APPEND VALUE bapiret2(
          type    = 'W'
          message = |Bank, address and tax data saved; supplier control data (corporate group, | &&
                    |GST class) not changed: { lv_err2 }| )
          TO lt_note.
      ENDIF.
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
    APPEND LINES OF lt_note TO et_success.
  ENDMETHOD.

ENDCLASS.

* The forms come after the class: they call LCL_HONC_BP.
INCLUDE zfi_vend_honc_fd.

START-OF-SELECTION.

  IF rb_mail IS NOT INITIAL.
    PERFORM goto_tmg.
    LEAVE TO CURRENT TRANSACTION.

*SOC BY FATIMA FOR 8000007904 Vendor code generation.
  ELSE.
*  ELSEIF rb_vend IS NOT INITIAL.
    PERFORM get_files.

    LOOP AT ta_files INTO wa_files.
      gv_file = wa_files-name.
      CONCATENATE gv_dir gv_file INTO gv_file.

      PERFORM get_file_data.
      PERFORM fetch_data.
      PERFORM validate_data.
      IF rb_vend IS NOT INITIAL.
*      IF ta_noifsc IS INITIAL.
        IF ta_chvend IS NOT INITIAL.
          PERFORM change_vendor.
        ENDIF.

        IF ta_crvend IS NOT INITIAL .
          PERFORM create_vendor.
        ENDIF.

*        IF ta_noifsc IS NOT INITIAL OR ta_nullifsc IS NOT INITIAL.             "added by fatima for 800007904
*          PERFORM missing_ifsc_mail.
*        ENDIF.
        PERFORM update_log.                                                     "added by Gauri Tandel - 8000003666 -17.06.2024
        " S/4HANA conversion: a file without vendor rows (only the header) gave
        " no log line and still mailed the users an empty list - "Mail
        " Successfully Sent" and nothing created (CRS, 19.11.2026). It is now
        " named in the log and no mail goes out for it.
        IF ta_data_tmp IS INITIAL.
          CLEAR wa_log.
          wa_log-file = gv_file.
          wa_log-msg  = 'The file has no vendor rows - nothing created or changed, no mail sent'.
          APPEND wa_log TO ta_log.
          CLEAR wa_log.
        ELSE.
          PERFORM send_det_mail.
        ENDIF.
        gv_source_file = gv_file.
        gv_target_path = gv_file.
        REPLACE 'INCOMING' WITH 'ARCHIVE' INTO gv_target_path.
        CALL FUNCTION 'ARCHIVFILE_SERVER_TO_SERVER'
          EXPORTING
            sourcepath       = gv_source_file
            targetpath       = gv_target_path
          EXCEPTIONS
            error_file       = 1
            no_authorization = 2
            OTHERS           = 3.
        IF sy-subrc EQ 0.
* delete source file
          DELETE DATASET gv_file.
        ENDIF.
        PERFORM refresh.
      ELSEIF rb_ifsc IS NOT INITIAL.
*        IF ta_noifsc IS NOT INITIAL OR ta_nullifsc IS NOT INITIAL."Commented by Anuj Nandapurkar
        IF ta_noifsc IS NOT INITIAL OR ta_data1 IS NOT INITIAL OR ta_nullifsc IS NOT INITIAL."Added by Anuj Nandapurkar as part of CR8000009272
          PERFORM missing_ifsc_mail.
        ENDIF.
*        ENDIF.
        CLEAR : wa_files, gv_file.
        REFRESH: ta_noifsc,ta_nullifsc.
*        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

*EOC BY FATIMA FOR 8000007904 Vendor code generation.
  IF ta_log IS NOT INITIAL.
    PERFORM display_output.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_DATA_OLD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM validate_data_old .
*
**Start of changes by Anuj Nandapurkar as part of CR8000009272 dated 16-10-2023
*  LOOP AT ta_data INTO wa_data.
*    IF wa_data-flag = 'A'. "Added by Gajanan - 15.03.2024 - CR#8000003035
*      IF line_exists( ta_movend[ j_1ipanno = wa_data-pan ] ).
*        APPEND wa_data TO ta_movend1.
*      ENDIF.
*      IF ta_movend1 IS NOT INITIAL.
*        READ TABLE ta_movend1 INTO wa_movend1 WITH KEY pan = wa_data-pan.
*        IF sy-subrc = 0.
*          wa_data1-name1 = wa_movend1-name1.
*          wa_data1-pan = wa_movend1-pan.
*          wa_data1-bankn = wa_movend1-bankn.
*          wa_data1-bankl = wa_movend1-bankl.
*          wa_data1-remarks = 'Vendor already exists' .
*          APPEND wa_data1 TO ta_data1.
*        ENDIF.
**      APPEND wa_data1 TO ta_data1.
*
*      ENDIF.
*
*      IF line_exists( ta_lfbk[ bankl = wa_data-bankl bankn = wa_data-bankn ] ).
*        APPEND wa_data TO ta_lfbk1 .
*      ENDIF.
*      IF ta_lfbk1 IS NOT INITIAL.
*        READ TABLE ta_lfbk1 INTO wa_lfbk1 WITH KEY bankl = wa_data-bankl.
*        IF sy-subrc = 0.
*          wa_data1-name1 = wa_lfbk1-name1.
*          wa_data1-pan = wa_lfbk1-pan.
*          wa_data1-bankn = wa_lfbk1-bankn.
*          wa_data1-bankl = wa_lfbk1-bankl.
*          wa_data1-remarks = 'Vendor has the same bank account maintained' .
*          APPEND wa_data1 TO ta_data1.
*        ENDIF.
*      ENDIF.
*
*    ENDIF.
*
*    IF strlen( wa_data-pan ) NE 10.
*      APPEND wa_data TO ta_pan.
*    ENDIF.
*    IF ta_pan IS NOT INITIAL.
*      READ TABLE ta_pan INTO wa_pan WITH KEY pan = wa_data-pan.
*      IF sy-subrc = 0.
*        wa_data1-name1 = wa_pan-name1.
*        wa_data1-pan = wa_pan-pan.
*        wa_data1-bankn = wa_pan-bankn.
*        wa_data1-bankl = wa_pan-bankl.
*        wa_data1-remarks = 'Incorrect PAN details' .
*        APPEND wa_data1 TO ta_data1.
*      ENDIF.
*    ENDIF.
**End of changes by Anuj Nandapurkar as part of CR8000009272 dated 16-10-2023
*
**  LOOP AT ta_data INTO wa_data.
*    IF line_exists( ta_bnka[ bankl = wa_data-bankl ] ).
*      IF line_exists( ta_movend[ j_1ipanno = wa_data-pan ] ).
*        APPEND wa_data TO ta_chvend.
*        IF ta_chvend IS NOT INITIAL.
*          DELETE ta_chvend WHERE name1 = wa_data1-name1.
*        ENDIF.
*      ELSE.
*        APPEND wa_data TO ta_crvend.
*        IF ta_crvend IS NOT INITIAL.
*          DELETE ta_crvend WHERE name1 = wa_data1-name1.
*        ENDIF.
*      ENDIF.
*    ELSE.
**SOC BY FATIMA FOR 8000007904 Vendor code generation.
*      IF wa_data-bankl = ' '.
*        APPEND wa_data TO ta_nullifsc.
*      ELSE.
*        APPEND wa_data TO ta_noifsc.
*      ENDIF.
**EOC BY FATIMA FOR 8000007904 Vendor code generation.
*    ENDIF.
*
*    """ Begin of commented section done by Sushil as on 10.03.2021
****    IF line_exists( ta_lfbk[ bankl = wa_data-bankl ] ).
****      IF line_exists( ta_movend[ j_1ipanno = wa_data-pan ] ).
****        APPEND wa_data TO ta_chvend.
****      ELSE.
****        APPEND wa_data TO ta_crvend.
****      ENDIF.
****    ELSE.
****      APPEND wa_data TO ta_noifsc.
****    ENDIF.
*    """ End of commented section done by Sushil as on 10.03.2021
**    CLEAR wa_data.
*    CLEAR: wa_data1, wa_data, wa_pan, wa_lfbk1, wa_movend1. "Added by Anuj Nandapurkar as part of CR8000009272
*    DELETE ADJACENT DUPLICATES FROM ta_data1 COMPARING ALL FIELDS."Added by Anuj as part of CR8000009272
*    REFRESH: ta_movend1,ta_lfbk1,ta_pan. "Added by Gajanan - 15.03.2024 - CR#8000003035
*  ENDLOOP.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  UPDATE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_log .
  LOOP AT ta_data1 INTO DATA(wa_tmp).
    wa_log-file = gv_file.
    READ TABLE ta_data_tmp INTO wa_data WITH KEY pan = wa_tmp-pan BINARY SEARCH.
    IF sy-subrc = 0.
      wa_log-name1 = wa_data-name1.
      wa_log-bankl = wa_data-bankl.
      wa_log-pan = wa_data-pan.
      wa_log-msg = wa_tmp-remarks.
      APPEND wa_log TO ta_log.
      CLEAR : wa_log.
    ENDIF.
  ENDLOOP.
ENDFORM.
