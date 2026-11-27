*&---------------------------------------------------------------------*
*& Report  ZMMS_BP_INDUSTRY_UPD
*&---------------------------------------------------------------------*
*& Title   : Business partner industry - mass update from Excel
*& Module  : MM (supplier master)            Client : Cipla
*&
*& What it does
*&   Replaces the industry of business partners: one row per partner,
*&   the industry it has now and the one it is to have. The layout is
*&   Cipla's "Industry Key Update in Business Partner.xlsx":
*&
*&     Business Partner | Old Industry Sector | New Industry Sector
*&     100000001        | 92                  | 0008
*&
*& Why it is needed
*&   In S/4HANA the supplier's industry key (LFA1-BRSCH) is not kept on its
*&   own: at every save SAP derives it from the partner's STANDARD industry
*&   through the CVI customising "Assign Industries" (SAP KBA 3581473). The
*&   conversion gave Cipla's vendor partners industry 92, which has no key
*&   assigned, so every save of those suppliers is refused - CVI_MAPPING 002
*&   "Industry 92 for object Business Partner ... could not be assigned"
*&   (MASS, HONC, 11-2026). The fix is on the partner, so it is made there.
*&
*& Per row
*&   - the partner must have the old industry; it is looked for in every
*&     industry system, rows of TB038A without a system are ignored;
*&   - the new industry must exist in the same industry system (TB038A);
*&     a key Excel shortened ("8" for "0008") is matched to the full key;
*&   - the new industry takes the old one's place: it becomes the standard
*&     industry when the old one was, and the old one is removed (or only
*&     loses the standard flag - checkbox "Remove the old industry");
*&   - a partner that already has the new industry as its standard one and
*&     not the old one is "No change".
*&
*& How it is saved - what Cipla's go-live taught
*&   CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE, then BAPI_TRANSACTION_ROLLBACK to
*&   clear what the validation left in the buffers, then MAINTAIN, then
*&   BAPI_TRANSACTION_COMMIT with WAIT and its RETURN read. The supplier
*&   (and customer) header goes with the partner, so SAP runs the
*&   synchronisation inside the call and reports what it refuses. After the
*&   save the partner's industries and the supplier's industry key are read
*&   back and shown. No table is updated directly. A test run (default)
*&   validates and changes nothing.
*&---------------------------------------------------------------------*
REPORT zmms_bp_industry_upd.

TYPES: BEGIN OF ty_row,
         row   TYPE i,
         cells TYPE string_table,
       END OF ty_row,
       tt_row TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

" One line of the result list.
TYPES: BEGIN OF ty_out,
         status  TYPE icon_d,
         row     TYPE i,
         partner TYPE bu_partner,
         old_ind TYPE bu_indsect,
         new_ind TYPE bu_indsect,
         istype  TYPE bu_istype,
         result  TYPE c LENGTH 10,
         brsch   TYPE brsch,
         message TYPE c LENGTH 255,
       END OF ty_out,
       tt_out TYPE STANDARD TABLE OF ty_out WITH EMPTY KEY.

CONSTANTS: gc_ins TYPE c LENGTH 1 VALUE 'I',
           gc_upd TYPE c LENGTH 1 VALUE 'U',
           gc_del TYPE c LENGTH 1 VALUE 'D'.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_file TYPE string LOWER CASE,
            p_test AS CHECKBOX DEFAULT 'X',
            p_del  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* LCX_IND - a problem with the file
*----------------------------------------------------------------------*
CLASS lcx_ind DEFINITION INHERITING FROM cx_static_check FINAL.
  PUBLIC SECTION.
    DATA text TYPE string.
    METHODS constructor IMPORTING iv_text TYPE string.
    METHODS get_text REDEFINITION.
ENDCLASS.

CLASS lcx_ind IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    text = iv_text.
  ENDMETHOD.
  METHOD get_text.
    result = text.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* LCL_FILE - the workbook on the PC
*----------------------------------------------------------------------*
CLASS lcl_file DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS f4.
    "! The first worksheet's rows, heading line included.
    CLASS-METHODS read
      IMPORTING iv_file   TYPE string
      RETURNING VALUE(rt) TYPE tt_row
      RAISING   lcx_ind.
ENDCLASS.

CLASS lcl_file IMPLEMENTATION.

  METHOD f4.
    DATA: lt_ft TYPE filetable,
          lv_rc TYPE i,
          lv_ua TYPE i.
    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING  window_title = 'Industry key update - select the Excel file'
                 file_filter  = 'Excel workbook (*.xlsx)|*.xlsx|'
      CHANGING   file_table   = lt_ft
                 rc           = lv_rc
                 user_action  = lv_ua
      EXCEPTIONS OTHERS       = 1 ).
    IF sy-subrc = 0 AND lv_ua = cl_gui_frontend_services=>action_ok AND lv_rc >= 1.
      p_file = lt_ft[ 1 ]-filename.
    ENDIF.
  ENDMETHOD.

  METHOD read.
    DATA: lt_bin TYPE solix_tab,
          lv_len TYPE i.
    cl_gui_frontend_services=>gui_upload(
      EXPORTING  filename   = iv_file
                 filetype   = 'BIN'
      IMPORTING  filelength = lv_len
      CHANGING   data_tab   = lt_bin
      EXCEPTIONS OTHERS     = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW lcx_ind( |Cannot read the file from the PC: { iv_file }| ).
    ENDIF.
    DATA(lv_x) = cl_bcs_convert=>solix_to_xstring( it_solix = lt_bin iv_size = lv_len ).

    DATA lo_xl TYPE REF TO cl_fdt_xl_spreadsheet.
    TRY.
        lo_xl = NEW cl_fdt_xl_spreadsheet( document_name = iv_file
                                           xdocument     = lv_x ).
      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION NEW lcx_ind( |The file is not a readable .xlsx workbook: { lx->get_text( ) }| ).
    ENDTRY.
    lo_xl->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = DATA(lt_ws) ).
    IF lt_ws IS INITIAL.
      RAISE EXCEPTION NEW lcx_ind( |{ iv_file } contains no worksheet| ).
    ENDIF.

    DATA lo_ref TYPE REF TO data.
    TRY.
        lo_ref = lo_xl->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_ws[ 1 ] ).
      CATCH cx_root INTO DATA(lx2).
        RAISE EXCEPTION NEW lcx_ind( |Worksheet "{ lt_ws[ 1 ] }" could not be read: { lx2->get_text( ) }| ).
    ENDTRY.
    FIELD-SYMBOLS: <lt_tab> TYPE STANDARD TABLE,
                   <ls_lin> TYPE any,
                   <lv_val> TYPE any.
    ASSIGN lo_ref->* TO <lt_tab>.
    IF <lt_tab> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    DATA lv_r TYPE i.
    LOOP AT <lt_tab> ASSIGNING <ls_lin>.
      lv_r = lv_r + 1.
      DATA ls_row TYPE ty_row.
      CLEAR ls_row.
      ls_row-row = lv_r.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <ls_lin> TO <lv_val>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        APPEND condense( CONV string( <lv_val> ) ) TO ls_row-cells.
      ENDDO.
      APPEND ls_row TO rt.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_IND - one partner's industry replaced
*----------------------------------------------------------------------*
CLASS lcl_ind DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS run.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_is,
             istype     TYPE bu_istype,
             ind_sector TYPE bu_indsect,
             isdef      TYPE bu_isdef,
           END OF ty_is,
           tt_is TYPE STANDARD TABLE OF ty_is WITH EMPTY KEY.
    CLASS-DATA mt_out TYPE tt_out.

    CLASS-METHODS row
      IMPORTING is_row TYPE ty_row
                iv_cbp TYPE i
                iv_cold TYPE i
                iv_cnew TYPE i.
    "! The industry key as the system has it: Excel turns "0008" into 8.
    CLASS-METHODS sector
      IMPORTING iv_in     TYPE string
                iv_istype TYPE bu_istype OPTIONAL
      RETURNING VALUE(rv) TYPE bu_indsect.
    CLASS-METHODS systems_of
      IMPORTING iv_sector TYPE bu_indsect
      RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS industries
      IMPORTING iv_bp     TYPE bu_partner
      RETURNING VALUE(rt) TYPE tt_is.
    CLASS-METHODS list
      IMPORTING it_is     TYPE tt_is
      RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS save
      IMPORTING is_data   TYPE cvis_ei_extern
      EXPORTING et_msg    TYPE bapiret2_t
      RETURNING VALUE(rv) TYPE abap_bool.
    CLASS-METHODS discard.
    CLASS-METHODS reset_bp.
    CLASS-METHODS add
      IMPORTING is_out TYPE ty_out
                iv_ty  TYPE c
                iv_msg TYPE clike.
    CLASS-METHODS show.
    CLASS-METHODS col
      IMPORTING it_head   TYPE string_table
                iv_key    TYPE string
                iv_def    TYPE i
      RETURNING VALUE(rv) TYPE i.
ENDCLASS.

CLASS lcl_ind IMPLEMENTATION.

  METHOD run.
    CLEAR mt_out.
    IF p_file IS INITIAL.
      MESSAGE 'Give the Excel file - F4 opens the file dialog' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    DATA lt_rows TYPE tt_row.
    TRY.
        lt_rows = lcl_file=>read( p_file ).
      CATCH lcx_ind INTO DATA(lx).
        DATA(lv_xt) = lx->get_text( ).
        MESSAGE lv_xt TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    IF lines( lt_rows ) < 2.
      MESSAGE 'The file has no data rows - row 1 holds the headings, the data starts in row 2'
              TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " The columns by their headings; in the order of the template otherwise.
    DATA(lt_head) = lt_rows[ 1 ]-cells.
    DATA(lv_cbp)  = col( it_head = lt_head iv_key = `BUSINESSPARTNER`   iv_def = 1 ).
    DATA(lv_cold) = col( it_head = lt_head iv_key = `OLDINDUSTRYSECTOR` iv_def = 2 ).
    DATA(lv_cnew) = col( it_head = lt_head iv_key = `NEWINDUSTRYSECTOR` iv_def = 3 ).

    LOOP AT lt_rows INTO DATA(ls_row) FROM 2.
      row( is_row = ls_row iv_cbp = lv_cbp iv_cold = lv_cold iv_cnew = lv_cnew ).
    ENDLOOP.
    show( ).
  ENDMETHOD.

  METHOD col.
    rv = iv_def.
    LOOP AT it_head INTO DATA(lv_h).
      DATA(lv_k) = to_upper( lv_h ).
      REPLACE ALL OCCURRENCES OF PCRE '[^A-Z0-9]' IN lv_k WITH ``.
      IF lv_k = iv_key.
        rv = sy-tabix.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD row.
    DATA ls_out TYPE ty_out.
    ls_out-row = is_row-row.
    DATA(lv_bpin) = to_upper( condense( VALUE string( is_row-cells[ iv_cbp ] OPTIONAL ) ) ).
    DATA(lv_oldin) = to_upper( condense( VALUE string( is_row-cells[ iv_cold ] OPTIONAL ) ) ).
    DATA(lv_newin) = to_upper( condense( VALUE string( is_row-cells[ iv_cnew ] OPTIONAL ) ) ).
    IF lv_bpin IS INITIAL AND lv_oldin IS INITIAL AND lv_newin IS INITIAL.
      RETURN.                                   " an empty line
    ENDIF.
    IF lv_bpin IS INITIAL OR lv_newin IS INITIAL.
      add( is_out = ls_out iv_ty = 'E'
           iv_msg = 'Business partner and new industry sector are both needed' ).
      RETURN.
    ENDIF.

    " ---- the partner
    ls_out-partner = COND #( WHEN lv_bpin CO '0123456789' THEN |{ lv_bpin ALPHA = IN WIDTH = 10 }|
                             ELSE lv_bpin ).
    SELECT SINGLE partner, partner_guid FROM but000
      WHERE partner = @ls_out-partner INTO @DATA(ls_bp).
    IF ls_bp-partner IS INITIAL.
      add( is_out = ls_out iv_ty = 'E' iv_msg = |Business partner { lv_bpin } does not exist| ).
      RETURN.
    ENDIF.
    DATA(lt_is) = industries( ls_out-partner ).

    " ---- the old industry, as the partner has it
    DATA ls_old TYPE ty_is.
    IF lv_oldin IS NOT INITIAL.
      ls_out-old_ind = sector( lv_oldin ).
      " The standard one first, where the partner has it in two systems.
      LOOP AT lt_is INTO DATA(ls_i) WHERE ind_sector = ls_out-old_ind.
        IF ls_old IS INITIAL OR ls_i-isdef = abap_true.
          ls_old = ls_i.
        ENDIF.
      ENDLOOP.
      IF ls_old IS INITIAL.
        " Excel may have shortened the old key as well: match it against
        " what the partner has.
        LOOP AT lt_is INTO ls_i.
          IF ls_i-ind_sector = lv_oldin
          OR ( lv_oldin CO '0123456789' AND ls_i-ind_sector CO '0123456789 '
               AND CONV i( ls_i-ind_sector ) = CONV i( lv_oldin ) ).
            ls_old = ls_i.
            ls_out-old_ind = ls_i-ind_sector.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      " No old industry given: the partner's standard one is replaced.
      READ TABLE lt_is INTO ls_old WITH KEY isdef = abap_true.
      ls_out-old_ind = ls_old-ind_sector.
    ENDIF.

    " ---- the industry system, and the new industry in it
    DATA lv_sys TYPE bu_istype.
    lv_sys = ls_old-istype.
    IF lv_sys IS INITIAL.
      READ TABLE lt_is INTO DATA(ls_std) WITH KEY isdef = abap_true.
      lv_sys = ls_std-istype.
    ENDIF.
    ls_out-new_ind = sector( iv_in = lv_newin iv_istype = lv_sys ).
    IF ls_out-new_ind IS INITIAL.
      add( is_out = ls_out iv_ty = 'E'
           iv_msg = |Industry sector "{ lv_newin }" does not exist | &&
                    |{ COND string( WHEN lv_sys IS NOT INITIAL THEN |in industry system { lv_sys } | ) }| &&
                    |(TB038A){ systems_of( CONV bu_indsect( lv_newin ) ) }| ).
      RETURN.
    ENDIF.
    IF lv_sys IS INITIAL.
      " A partner without any industry: the system that holds the new one.
      DATA lt_sys TYPE STANDARD TABLE OF bu_istype WITH EMPTY KEY.
      SELECT istype FROM tb038a WHERE ind_sector = @ls_out-new_ind AND istype <> @space
        ORDER BY istype INTO TABLE @lt_sys.
      IF lines( lt_sys ) <> 1.
        add( is_out = ls_out iv_ty = 'E'
             iv_msg = |Partner has no industry, and industry { ls_out-new_ind } is in | &&
                      |{ COND string( WHEN lt_sys IS INITIAL THEN `no` ELSE `more than one` ) } industry system| ).
        RETURN.
      ENDIF.
      lv_sys = lt_sys[ 1 ].
    ENDIF.
    ls_out-istype = lv_sys.

    " ---- already done?
    DATA(lv_has_new) = xsdbool( line_exists( lt_is[ istype = lv_sys ind_sector = ls_out-new_ind ] ) ).
    DATA(lv_new_std) = xsdbool( line_exists( lt_is[ istype = lv_sys ind_sector = ls_out-new_ind isdef = abap_true ] ) ).
    IF ls_old IS INITIAL AND lv_oldin IS NOT INITIAL.
      IF lv_new_std = abap_true.
        add( is_out = ls_out iv_ty = 'I'
             iv_msg = |No change - { ls_out-new_ind } is the standard industry already, | &&
                      |{ ls_out-old_ind } is not on the partner| ).
      ELSE.
        add( is_out = ls_out iv_ty = 'E'
             iv_msg = |The partner does not have industry { lv_oldin } - it has: { list( lt_is ) }| ).
      ENDIF.
      RETURN.
    ENDIF.
    IF ls_old-ind_sector = ls_out-new_ind AND ls_old-istype = lv_sys.
      add( is_out = ls_out iv_ty = 'I' iv_msg = 'No change - old and new industry are the same' ).
      RETURN.
    ENDIF.

    " ---- the request: the partner, with its supplier / customer header
    DATA ls_data TYPE cvis_ei_extern.
    ls_data-partner-header-object_task                  = gc_upd.
    ls_data-partner-header-object_instance-bpartnerguid = ls_bp-partner_guid.
    SELECT SINGLE vendor FROM cvi_vend_link WHERE partner_guid = @ls_bp-partner_guid INTO @DATA(lv_lifnr).
    IF lv_lifnr IS NOT INITIAL.
      ls_data-vendor-header-object_task           = gc_upd.
      ls_data-vendor-header-object_instance-lifnr = lv_lifnr.
    ENDIF.
    SELECT SINGLE customer FROM cvi_cust_link WHERE partner_guid = @ls_bp-partner_guid INTO @DATA(lv_kunnr).
    IF lv_kunnr IS NOT INITIAL.
      ls_data-customer-header-object_task           = gc_upd.
      ls_data-customer-header-object_instance-kunnr = lv_kunnr.
    ENDIF.

    " The new industry takes the old one's place - the standard flag with
    " it, or the standard flag alone when the partner has no old industry.
    DATA(lv_make_std) = xsdbool( ls_old-isdef = abap_true OR ls_old IS INITIAL ).
    DATA ls_new TYPE bus_ei_bupa_industrysector.
    ls_new-task                = COND #( WHEN lv_has_new = abap_true THEN gc_upd ELSE gc_ins ).
    ls_new-data_key-keysystem  = lv_sys.
    ls_new-data_key-ind_sector = ls_out-new_ind.
    ls_new-data-ind_default    = lv_make_std.
    ls_new-datax-ind_default   = abap_true.
    APPEND ls_new TO ls_data-partner-central_data-industry-industries.
    IF ls_old IS NOT INITIAL.
      DATA ls_o TYPE bus_ei_bupa_industrysector.
      ls_o-data_key-keysystem  = ls_old-istype.
      ls_o-data_key-ind_sector = ls_old-ind_sector.
      IF p_del = abap_true.
        ls_o-task = gc_del.
      ELSE.
        ls_o-task              = gc_upd.
        ls_o-data-ind_default  = abap_false.
        ls_o-datax-ind_default = abap_true.
      ENDIF.
      IF p_del = abap_true OR ls_old-isdef = abap_true.
        APPEND ls_o TO ls_data-partner-central_data-industry-industries.
      ENDIF.
    ENDIF.

    " ---- save
    DATA lt_msg TYPE bapiret2_t.
    IF save( EXPORTING is_data = ls_data IMPORTING et_msg = lt_msg ) = abap_false.
      LOOP AT lt_msg INTO DATA(ls_m) WHERE type CA 'EAX'.
        DATA(lv_hint) = COND string(
          WHEN ls_m-id = 'CVI_MAPPING' AND ls_m-number = '002'
          THEN | - industry { ls_out-new_ind } needs a vendor industry key in CVI "Assign Industries"| &&
               | (direction business partner to vendor)| ).
        add( is_out = ls_out iv_ty = 'E' iv_msg = |{ ls_m-message }{ lv_hint }| ).
      ENDLOOP.
      IF NOT line_exists( lt_msg[ type = 'E' ] ) AND NOT line_exists( lt_msg[ type = 'A' ] )
         AND NOT line_exists( lt_msg[ type = 'X' ] ).
        add( is_out = ls_out iv_ty = 'E' iv_msg = 'Not saved - SAP returned no message' ).
      ENDIF.
      RETURN.
    ENDIF.

    IF p_test = abap_true.
      add( is_out = ls_out iv_ty = 'S'
           iv_msg = |Test run OK - { ls_out-old_ind } would be replaced by { ls_out-new_ind } | &&
                    |(industry system { lv_sys })| ).
      RETURN.
    ENDIF.

    " ---- read back
    DATA(lt_after) = industries( ls_out-partner ).
    IF lv_lifnr IS NOT INITIAL.
      SELECT SINGLE brsch FROM lfa1 WHERE lifnr = @lv_lifnr INTO @ls_out-brsch.
    ENDIF.
    IF NOT line_exists( lt_after[ istype = lv_sys ind_sector = ls_out-new_ind isdef = lv_make_std ] ).
      add( is_out = ls_out iv_ty = 'E'
           iv_msg = |Saved, but the partner shows: { list( lt_after ) }| ).
      RETURN.
    ENDIF.
    add( is_out = ls_out iv_ty = 'S'
         iv_msg = |Industry { ls_out-old_ind } replaced by { ls_out-new_ind }| &&
                  |{ COND string( WHEN lv_make_std = abap_true THEN ` (standard industry)` ) }| &&
                  |{ COND string( WHEN lv_lifnr IS NOT INITIAL
                                  THEN | - supplier { lv_lifnr ALPHA = OUT } industry key { ls_out-brsch }| ) }| ).
  ENDMETHOD.

  METHOD sector.
    " The key as the file has it, if the industry system holds it; else the
    " key Excel shortened ("8" for "0008"), if exactly one matches.
    DATA lt_sec TYPE STANDARD TABLE OF bu_indsect WITH EMPTY KEY.
    DATA(lv_in) = CONV bu_indsect( iv_in ).
    IF iv_istype IS NOT INITIAL.
      SELECT ind_sector FROM tb038a WHERE istype = @iv_istype INTO TABLE @lt_sec.
    ELSE.
      SELECT ind_sector FROM tb038a WHERE istype <> @space INTO TABLE @lt_sec.
    ENDIF.
    IF line_exists( lt_sec[ table_line = lv_in ] ).
      rv = lv_in.
      RETURN.
    ENDIF.
    IF iv_in CN '0123456789'.
      RETURN.
    ENDIF.
    DATA lv_n TYPE i.
    LOOP AT lt_sec INTO DATA(lv_s).
      IF lv_s CO '0123456789 ' AND lv_s IS NOT INITIAL AND CONV i( lv_s ) = CONV i( iv_in ).
        lv_n = lv_n + 1.
        rv = lv_s.
      ENDIF.
    ENDLOOP.
    IF lv_n <> 1.
      CLEAR rv.
    ENDIF.
  ENDMETHOD.

  METHOD systems_of.
    DATA lt_sys TYPE STANDARD TABLE OF bu_istype WITH EMPTY KEY.
    SELECT istype FROM tb038a WHERE ind_sector = @iv_sector AND istype <> @space
      ORDER BY istype INTO TABLE @lt_sys.
    LOOP AT lt_sys INTO DATA(lv_s).
      rv = COND #( WHEN rv IS INITIAL THEN | - it is in system { lv_s }|
                   ELSE |{ rv }, { lv_s }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD industries.
    SELECT istype, ind_sector, isdef FROM but0is
      WHERE partner = @iv_bp ORDER BY istype, ind_sector INTO TABLE @rt.
  ENDMETHOD.

  METHOD list.
    LOOP AT it_is INTO DATA(ls_i).
      DATA(lv_one) = |{ ls_i-ind_sector } (system { ls_i-istype }{ COND string( WHEN ls_i-isdef = abap_true
                                                                             THEN `, standard` ) })|.
      rv = COND #( WHEN rv IS INITIAL THEN lv_one ELSE |{ rv }, { lv_one }| ).
    ENDLOOP.
    IF rv IS INITIAL.
      rv = `no industry`.
    ENDIF.
  ENDMETHOD.

  METHOD save.
    rv = abap_false.
    CLEAR et_msg.
    DATA lt_map TYPE mdg_bs_bp_msgmap_t.
    TRY.
        cl_md_bp_maintain=>validate_single( EXPORTING i_data        = is_data
                                            IMPORTING et_return_map = lt_map ).
      CATCH cx_root INTO DATA(lx1).
        discard( ).
        APPEND VALUE #( type = 'E' message = |Validation failed: { lx1->get_text( ) }| ) TO et_msg.
        RETURN.
    ENDTRY.
    " What the validation left in the buffers goes before the real posting
    " (Cipla, HONC 20.11.2026 and ZMMS_BP_MASS_UPLOAD 27.11.2026).
    discard( ).
    LOOP AT lt_map INTO DATA(ls_map) WHERE type CA 'EAX'.
      APPEND VALUE #( type = ls_map-type id = ls_map-id number = ls_map-number
                      message = ls_map-message ) TO et_msg.
    ENDLOOP.
    IF et_msg IS NOT INITIAL.
      RETURN.
    ENDIF.

    DATA: lt_data TYPE cvis_ei_extern_t,
          lt_ret  TYPE bapiretm.
    APPEND is_data TO lt_data.
    TRY.
        cl_md_bp_maintain=>maintain( EXPORTING i_data     = lt_data
                                               i_test_run = p_test
                                     IMPORTING e_return   = lt_ret ).
      CATCH cx_root INTO DATA(lx2).
        discard( ).
        APPEND VALUE #( type = 'E' message = |Maintain failed: { lx2->get_text( ) }| ) TO et_msg.
        RETURN.
    ENDTRY.
    FIELD-SYMBOLS: <lt_sub> TYPE ANY TABLE,
                   <ls_sub> TYPE any.
    DATA(lv_bad) = abap_false.
    LOOP AT lt_ret ASSIGNING FIELD-SYMBOL(<ls_ret>).
      ASSIGN COMPONENT 'OBJECT_MSG' OF STRUCTURE <ls_ret> TO <lt_sub>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      LOOP AT <lt_sub> ASSIGNING <ls_sub>.
        DATA ls_r2 TYPE bapiret2.
        CLEAR ls_r2.
        MOVE-CORRESPONDING <ls_sub> TO ls_r2.
        IF ls_r2-type CA 'EAX'.
          APPEND ls_r2 TO et_msg.
          lv_bad = abap_true.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    IF lv_bad = abap_true OR p_test = abap_true.
      discard( ).
      rv = xsdbool( lv_bad = abap_false ).
      RETURN.
    ENDIF.

    " BAPI_TRANSACTION_COMMIT with WAIT: its RETURN says whether the update ran.
    DATA ls_cret TYPE bapiret2.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait   = abap_true
      IMPORTING return = ls_cret.
    reset_bp( ).
    IF ls_cret-type CA 'EAX'.
      APPEND VALUE #( type = 'E' message = |The update did not go through: { ls_cret-message }| ) TO et_msg.
      RETURN.
    ENDIF.
    rv = abap_true.
  ENDMETHOD.

  METHOD discard.
    " BAPI_TRANSACTION_ROLLBACK refreshes the buffers the business partner
    " functions registered; a bare ROLLBACK WORK does not.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    reset_bp( ).
  ENDMETHOD.

  METHOD reset_bp.
    TRY.
        CALL FUNCTION 'BUP_MEMORY_CENTRAL_INIT'.
      CATCH cx_sy_dyn_call_illegal_func.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD add.
    DATA(ls_o) = is_out.
    ls_o-status  = SWITCH #( iv_ty WHEN 'E' THEN icon_red_light
                                   WHEN 'W' THEN icon_yellow_light
                                   WHEN 'I' THEN icon_light_out
                                   ELSE icon_green_light ).
    ls_o-result  = SWITCH #( iv_ty WHEN 'E' THEN 'Failed'
                                   WHEN 'W' THEN 'Warning'
                                   WHEN 'I' THEN 'No change'
                                   ELSE COND #( WHEN p_test = abap_true THEN 'Test OK' ELSE 'Updated' ) ).
    ls_o-message = iv_msg.
    APPEND ls_o TO mt_out.
  ENDMETHOD.

  METHOD show.
    IF mt_out IS INITIAL.
      MESSAGE 'Nothing to process in the file' TYPE 'S'.
      RETURN.
    ENDIF.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                                CHANGING  t_table      = mt_out ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->get_columns( )->set_optimize( abap_true ).
        DATA(lt_t) = VALUE string_table( ( `STATUS:Status` ) ( `ROW:Excel row` ) ( `PARTNER:Business partner` )
                                         ( `OLD_IND:Old industry` ) ( `NEW_IND:New industry` )
                                         ( `ISTYPE:Industry system` ) ( `RESULT:Result` )
                                         ( `BRSCH:Vendor industry key` ) ( `MESSAGE:Message` ) ).
        LOOP AT lt_t INTO DATA(lv_t).
          SPLIT lv_t AT ':' INTO DATA(lv_c) DATA(lv_x).
          DATA(lo_c) = lo_alv->get_columns( )->get_column( CONV lvc_fname( lv_c ) ).
          lo_c->set_short_text( CONV scrtext_s( lv_x ) ).
          lo_c->set_medium_text( CONV scrtext_m( lv_x ) ).
          lo_c->set_long_text( CONV scrtext_l( lv_x ) ).
        ENDLOOP.
        DATA(lv_ok)  = REDUCE i( INIT n = 0 FOR o IN mt_out WHERE ( result = 'Updated' OR result = 'Test OK' ) NEXT n = n + 1 ).
        DATA(lv_bad) = REDUCE i( INIT n = 0 FOR o IN mt_out WHERE ( result = 'Failed' ) NEXT n = n + 1 ).
        lo_alv->get_display_settings( )->set_list_header(
          CONV lvc_title( |{ COND string( WHEN p_test = abap_true THEN `TEST RUN - ` ) }| &&
                          |{ lv_ok } { COND string( WHEN p_test = abap_true THEN `would be updated` ELSE `updated` ) }, | &&
                          |{ lv_bad } failed| ) ).
        lo_alv->display( ).
      CATCH cx_salv_error INTO DATA(lx).
        DATA(lv_m) = lx->get_text( ).
        MESSAGE lv_m TYPE 'S' DISPLAY LIKE 'E'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Events
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_file=>f4( ).

START-OF-SELECTION.
  lcl_ind=>run( ).
