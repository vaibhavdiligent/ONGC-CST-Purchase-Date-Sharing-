*----------------------------------------------------------------------*
* LCL_UL - the upload
*   1 read the file and match every heading to a field
*   2 convert and check every value, row by row
*   3 change: compare with SAP; create: take what is filled
*   4 one popup for the cells that would clear a field
*   5 build the API's structures with only what changes, and post - one
*     business partner at a time, so one bad record stops nobody else
*----------------------------------------------------------------------*
CLASS lcl_ul DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS run.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_col,
             idx TYPE i,
             fld TYPE ty_fld,
           END OF ty_col,
           tt_col TYPE STANDARD TABLE OF ty_col WITH EMPTY KEY.
    " One value going to SAP.
    TYPES: BEGIN OF ty_chg,
             row   TYPE i,
             grp   TYPE ty_grp,
             key   TYPE string,
             fld   TYPE ty_fld,
             new   TYPE REF TO data,
             oldx  TYPE string,
             newx  TYPE string,
             clear TYPE abap_bool,
           END OF ty_chg,
           tt_chg TYPE STANDARD TABLE OF ty_chg WITH EMPTY KEY.
    " One company code, sales area or repeating entry the record touches.
    TYPES: BEGIN OF ty_ent,
             grp  TYPE ty_grp,
             key  TYPE string,
             task TYPE c LENGTH 1,
             row  TYPE i,
           END OF ty_ent,
           tt_ent TYPE STANDARD TABLE OF ty_ent WITH EMPTY KEY.
    TYPES: BEGIN OF ty_obj,
             tag    TYPE string,
             num    TYPE string,
             row    TYPE i,
             exists TYPE abap_bool,
             err    TYPE abap_bool,
             rows   TYPE tt_row,
             chg    TYPE tt_chg,
             ent    TYPE tt_ent,
           END OF ty_obj,
           tt_obj TYPE STANDARD TABLE OF ty_obj WITH EMPTY KEY.
    " A line already placed in a table of the API structure.
    TYPES: BEGIN OF ty_line,
             key TYPE string,
             ref TYPE REF TO data,
           END OF ty_line.

    CLASS-DATA mo_log  TYPE REF TO lcl_log.
    CLASS-DATA mt_col  TYPE tt_col.
    CLASS-DATA mv_act  TYPE i.
    CLASS-DATA mv_rep  TYPE ty_grp.
    CLASS-DATA mo_db   TYPE REF TO lcl_db.
    CLASS-DATA mt_line TYPE HASHED TABLE OF ty_line WITH UNIQUE KEY key.

    CLASS-METHODS map_cols
      IMPORTING it_head   TYPE tt_cell
                it_rows   TYPE tt_row
      RETURNING VALUE(rv) TYPE abap_bool.
    CLASS-METHODS check_cols RETURNING VALUE(rv) TYPE abap_bool.
    CLASS-METHODS col_of
      IMPORTING iv_name   TYPE string
      RETURNING VALUE(rv) TYPE i.
    CLASS-METHODS cell
      IMPORTING is_row    TYPE ty_row
                iv_idx    TYPE i
      RETURNING VALUE(rv) TYPE string.
    " The key of part IV_GRP in this row, as LCL_DB files it.
    CLASS-METHODS row_key
      IMPORTING is_row    TYPE ty_row
                iv_grp    TYPE ty_grp
      RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS gather
      IMPORTING it_rows   TYPE tt_row
      RETURNING VALUE(rt) TYPE tt_obj.
    CLASS-METHODS compare CHANGING cs_obj TYPE ty_obj.
    CLASS-METHODS add_chg
      IMPORTING is_chg TYPE ty_chg
      CHANGING  cs_obj TYPE ty_obj.
    CLASS-METHODS validate CHANGING cs_obj TYPE ty_obj.
    CLASS-METHODS confirm_clears
      IMPORTING it_obj    TYPE tt_obj
      RETURNING VALUE(rv) TYPE abap_bool.
    CLASS-METHODS build
      IMPORTING is_obj  TYPE ty_obj
      EXPORTING es_ext  TYPE cvis_ei_extern
                ev_guid TYPE bu_partner_guid.
    CLASS-METHODS post
      IMPORTING is_ext    TYPE cvis_ei_extern
                is_obj    TYPE ty_obj
      RETURNING VALUE(rv) TYPE abap_bool.
    CLASS-METHODS reset_bp.

    " ---- generic access to the API structure
    CLASS-METHODS sub
      IMPORTING ir        TYPE REF TO data
                iv_path   TYPE string
      RETURNING VALUE(rr) TYPE REF TO data.
    CLASS-METHODS put
      IMPORTING ir      TYPE REF TO data
                iv_path TYPE string
                iv_val  TYPE any.
    CLASS-METHODS set_field
      IMPORTING ir_line TYPE REF TO data
                is_chg  TYPE ty_chg.
    CLASS-METHODS new_line
      IMPORTING ir_tab    TYPE REF TO data
      RETURNING VALUE(rr) TYPE REF TO data.
    " The line of a company code / sales area / repeating entry, created on
    " first use.
    CLASS-METHODS line_of
      IMPORTING ir_obj    TYPE REF TO data
                is_ent    TYPE ty_ent
      RETURNING VALUE(rr) TYPE REF TO data.
    CLASS-METHODS text_lines
      IMPORTING iv_text   TYPE string
      RETURNING VALUE(rt) TYPE tline_tab.
ENDCLASS.


CLASS lcl_ul IMPLEMENTATION.

  METHOD cell.
    READ TABLE is_row-cells INTO rv INDEX iv_idx.
    IF sy-subrc <> 0.
      CLEAR rv.
    ENDIF.
  ENDMETHOD.

  METHOD col_of.
    READ TABLE mt_col INTO DATA(ls_c) WITH KEY fld-name = iv_name.
    IF sy-subrc = 0.
      rv = ls_c-idx.
    ENDIF.
  ENDMETHOD.

  METHOD map_cols.
    rv = abap_true.
    CLEAR: mt_col, mv_act.
    LOOP AT it_head INTO DATA(lv_h).
      DATA(lv_i) = sy-tabix.
      DATA(lv_hc) = condense( lv_h ).
      IF lv_hc IS INITIAL.
        " A column without a heading is only a problem if it has data.
        LOOP AT it_rows INTO DATA(ls_r).
          IF condense( cell( is_row = ls_r iv_idx = lv_i ) ) IS NOT INITIAL.
            mo_log->add( iv_row = ls_r-row iv_type = 'E'
                         iv_msg = |Column { lcl_util=>col_letter( lv_i ) } has data but no field name in row 1| ).
            rv = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.
        CONTINUE.
      ENDIF.
      IF to_upper( lv_hc ) = gc_action.
        mv_act = lv_i.
        CONTINUE.
      ENDIF.
      DATA ls_f TYPE ty_fld.
      DATA lv_err TYPE string.
      lcl_cat=>resolve( EXPORTING iv_head = lv_hc IMPORTING es_fld = ls_f ev_err = lv_err ).
      IF lv_err IS NOT INITIAL.
        mo_log->add( iv_field = lv_hc iv_type = 'E' iv_msg = lv_err ).
        rv = abap_false.
        CONTINUE.
      ENDIF.
      IF line_exists( mt_col[ fld-name = ls_f-name ] ).
        mo_log->add( iv_field = lv_hc iv_type = 'E'
                     iv_msg = |{ ls_f-name } is in the file twice - columns { lcl_util=>col_letter( col_of( ls_f-name ) ) } | &&
                              |and { lcl_util=>col_letter( lv_i ) }| ).
        rv = abap_false.
        CONTINUE.
      ENDIF.
      APPEND VALUE #( idx = lv_i fld = ls_f ) TO mt_col.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_cols.
    rv = abap_true.
    DATA(lv_num) = lcl_cat=>num_name( ).
    IF p_chg = abap_true AND col_of( lv_num ) = 0.
      mo_log->add( iv_field = lv_num iv_type = 'E'
                   iv_msg = |A change needs the number column { lv_num }| ).
      rv = abap_false.
    ENDIF.

    DATA lt_fld TYPE tt_fld.
    LOOP AT mt_col INTO DATA(ls_c).
      APPEND ls_c-fld TO lt_fld.
    ENDLOOP.

    " ---- one repeating part at most, and only next to what it belongs to
    DATA lt_names TYPE string_table.
    LOOP AT lt_fld INTO DATA(ls_f) WHERE key = abap_false.
      APPEND ls_f-name TO lt_names.
    ENDLOOP.
    DATA lt_dummy TYPE tt_fld.
    DATA lv_dact TYPE abap_bool.
    DATA lv_err TYPE string.
    lcl_sel=>complete( EXPORTING it_names  = lt_names
                                 iv_create = p_crt
                       IMPORTING et_fld    = lt_dummy
                                 ev_action = lv_dact
                                 ev_err    = lv_err ).
    IF lv_err IS NOT INITIAL.
      mo_log->add( iv_type = 'E' iv_msg = lv_err ).
      rv = abap_false.
      RETURN.
    ENDIF.
    mv_rep = lcl_sel=>rep_of( lt_dummy ).

    " ---- every column the system needs must be there
    LOOP AT lt_dummy INTO ls_f WHERE must = abap_true.
      IF ls_f-name = lv_num.
        CONTINUE.
      ENDIF.
      IF col_of( ls_f-name ) = 0.
        mo_log->add( iv_field = ls_f-name iv_type = 'E'
                     iv_msg = |Column { ls_f-name } is missing - { COND string( WHEN ls_f-key = abap_true
                                THEN `it identifies the record` ELSE `a new record needs it` ) }| ).
        rv = abap_false.
      ENDIF.
    ENDLOOP.
    IF mv_rep IS INITIAL AND mv_act > 0.
      mo_log->add( iv_field = gc_action iv_type = 'W'
                   iv_msg = 'The ACTION column is only used with a repeating part - it is ignored' ).
    ENDIF.
  ENDMETHOD.

  METHOD row_key.
    DATA(lt_k) = lcl_cat=>key_names( iv_grp ).
    DATA lv_n TYPE i.
    LOOP AT lt_k INTO DATA(lv_k).
      lv_n = lv_n + 1.
      DATA(lv_i) = col_of( lv_k ).
      DATA lv_v TYPE string.
      CLEAR lv_v.
      IF lv_i > 0.
        DATA(ls_f) = lcl_cat=>by_name( lv_k ).
        DATA lr_v TYPE REF TO data.
        DATA lv_e TYPE string.
        lcl_val=>to_int( EXPORTING is_f = ls_f iv_in = cell( is_row = is_row iv_idx = lv_i )
                         IMPORTING er_val = lr_v ev_err = lv_e ).
        IF lr_v IS BOUND.
          ASSIGN lr_v->* TO FIELD-SYMBOL(<lv>).
          lv_v = condense( CONV string( <lv> ) ).
        ENDIF.
      ENDIF.
      rv = COND #( WHEN lv_n = 1 THEN lv_v ELSE |{ rv }\|{ lv_v }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD gather.
    " Rows are grouped by number. On a creation a row with no number is a
    " record of its own.
    DATA(lv_ncol) = col_of( lcl_cat=>num_name( ) ).
    DATA(ls_nf)   = lcl_cat=>by_name( lcl_cat=>num_name( ) ).
    LOOP AT it_rows INTO DATA(ls_r).
      DATA lv_num TYPE string.
      CLEAR lv_num.
      IF lv_ncol > 0.
        DATA(lv_raw) = condense( cell( is_row = ls_r iv_idx = lv_ncol ) ).
        IF lv_raw IS NOT INITIAL.
          DATA lr_n TYPE REF TO data.
          DATA lv_e TYPE string.
          lcl_val=>to_int( EXPORTING is_f = ls_nf iv_in = lv_raw IMPORTING er_val = lr_n ev_err = lv_e ).
          IF lv_e IS NOT INITIAL.
            mo_log->add( iv_row = ls_r-row iv_field = ls_nf-name iv_new = lv_raw iv_type = 'E' iv_msg = lv_e ).
            CONTINUE.
          ENDIF.
          ASSIGN lr_n->* TO FIELD-SYMBOL(<lv_n>).
          lv_num = <lv_n>.
        ENDIF.
      ENDIF.
      IF lv_num IS INITIAL AND p_chg = abap_true.
        mo_log->add( iv_row = ls_r-row iv_field = ls_nf-name iv_type = 'E'
                     iv_msg = 'No number in this row - a change needs one' ).
        CONTINUE.
      ENDIF.
      DATA(lv_tag) = COND string( WHEN lv_num IS INITIAL THEN |#NEW{ ls_r-row }| ELSE lv_num ).
      READ TABLE rt ASSIGNING FIELD-SYMBOL(<ls_o>) WITH KEY tag = lv_tag.
      IF sy-subrc <> 0.
        APPEND VALUE #( tag = lv_tag num = lv_num row = ls_r-row ) TO rt ASSIGNING <ls_o>.
      ENDIF.
      APPEND ls_r TO <ls_o>-rows.
    ENDLOOP.
  ENDMETHOD.

  METHOD add_chg.
    " The same value from two rows of one record is fine; two different
    " values are a contradiction the program cannot settle.
    READ TABLE cs_obj-chg INTO DATA(ls_old)
         WITH KEY grp = is_chg-grp key = is_chg-key fld-name = is_chg-fld-name.
    IF sy-subrc = 0.
      IF ls_old-newx <> is_chg-newx.
        mo_log->add( iv_row = is_chg-row iv_obj = cs_obj-tag iv_field = is_chg-fld-name
                     iv_old = ls_old-newx iv_new = is_chg-newx iv_type = 'E'
                     iv_msg = |Row { ls_old-row } gives "{ ls_old-newx }" for the same field| ).
        cs_obj-err = abap_true.
      ENDIF.
      RETURN.
    ENDIF.
    APPEND is_chg TO cs_obj-chg.
  ENDMETHOD.

  METHOD compare.
    DATA(lv_create) = p_crt.
    LOOP AT cs_obj-rows INTO DATA(ls_r).
      " ---- the entries this row stands for
      DATA lv_act TYPE c LENGTH 1.
      CLEAR lv_act.
      IF mv_act > 0 AND mv_rep IS NOT INITIAL.
        lv_act = to_upper( condense( cell( is_row = ls_r iv_idx = mv_act ) ) ).
        IF lv_act NA ' IUD'.
          mo_log->add( iv_row = ls_r-row iv_obj = cs_obj-tag iv_field = gc_action iv_new = lv_act
                       iv_type = 'E' iv_msg = 'ACTION must be I (insert), U (update), D (delete) or empty' ).
          cs_obj-err = abap_true.
          CONTINUE.
        ENDIF.
      ENDIF.

      DATA lt_grp TYPE SORTED TABLE OF ty_grp WITH UNIQUE KEY table_line.
      CLEAR lt_grp.
      LOOP AT mt_col INTO DATA(ls_c).
        INSERT ls_c-fld-grp INTO TABLE lt_grp.
        DATA(lv_root) = lcl_cat=>root_of( ls_c-fld-grp ).
        INSERT lv_root INTO TABLE lt_grp.
      ENDLOOP.

      LOOP AT lt_grp INTO DATA(lv_g).
        DATA(ls_g) = lcl_cat=>grp( lv_g ).
        IF ls_g-keys IS INITIAL AND ls_g-rep = abap_false.
          CONTINUE.
        ENDIF.
        DATA(lv_k) = row_key( is_row = ls_r iv_grp = lv_g ).
        IF lv_k CO '|'.
          " no key in this row for this part
          CONTINUE.
        ENDIF.
        DATA(lv_have) = xsdbool( cs_obj-exists = abap_true AND mo_db IS BOUND
                                 AND mo_db->rec( iv_grp = lv_g iv_num = cs_obj-num iv_key = lv_k ) IS BOUND ).
        DATA lv_task TYPE c LENGTH 1.
        IF ls_g-rep = abap_true.
          lv_task = lv_act.
          IF lv_task = space.
            lv_task = COND #( WHEN lv_have = abap_true THEN gc_upd ELSE gc_ins ).
          ENDIF.
          IF ( lv_task = gc_upd OR lv_task = gc_del ) AND lv_have = abap_false.
            mo_log->add( iv_row = ls_r-row iv_obj = cs_obj-tag iv_part = ls_g-text iv_type = 'E'
                         iv_msg = |Entry { lv_k } does not exist - it cannot be { COND string( WHEN lv_task = gc_del
                                  THEN `deleted` ELSE `updated` ) }| ).
            cs_obj-err = abap_true.
            CONTINUE.
          ENDIF.
          IF lv_task = gc_ins AND lv_have = abap_true.
            mo_log->add( iv_row = ls_r-row iv_obj = cs_obj-tag iv_part = ls_g-text iv_type = 'E'
                         iv_msg = |Entry { lv_k } exists already - use U to change it| ).
            cs_obj-err = abap_true.
            CONTINUE.
          ENDIF.
          APPEND VALUE #( grp = lv_g key = lv_k task = lv_task row = ls_r-row ) TO cs_obj-ent.
        ELSEIF NOT line_exists( cs_obj-ent[ grp = lv_g key = lv_k ] ).
          APPEND VALUE #( grp = lv_g key = lv_k row = ls_r-row
                          task = COND #( WHEN lv_have = abap_true THEN gc_upd ELSE gc_ins ) ) TO cs_obj-ent.
        ENDIF.
      ENDLOOP.

      " ---- the values
      LOOP AT mt_col INTO ls_c WHERE fld-key = abap_false.
        DATA(ls_f) = ls_c-fld.
        DATA(lv_raw) = cell( is_row = ls_r iv_idx = ls_c-idx ).
        IF condense( lv_raw ) = gc_keep.
          CONTINUE.
        ENDIF.
        DATA(lv_rg) = lcl_cat=>root_of( ls_f-grp ).
        DATA(lv_key) = COND string( WHEN lcl_cat=>grp( ls_f-grp )-rep = abap_true
                                    THEN row_key( is_row = ls_r iv_grp = ls_f-grp )
                                    WHEN lv_rg = 'CENT' THEN ``
                                    ELSE row_key( is_row = ls_r iv_grp = lv_rg ) ).
        " A deleted entry takes no values.
        IF lcl_cat=>grp( ls_f-grp )-rep = abap_true
           AND line_exists( cs_obj-ent[ grp = ls_f-grp key = lv_key task = gc_del ] ).
          CONTINUE.
        ENDIF.

        DATA lr_new TYPE REF TO data.
        DATA lv_err TYPE string.
        lcl_val=>to_int( EXPORTING is_f = ls_f iv_in = lv_raw IMPORTING er_val = lr_new ev_err = lv_err ).
        IF lv_err IS NOT INITIAL.
          mo_log->add( iv_row = ls_r-row iv_obj = cs_obj-tag iv_field = ls_f-name iv_new = lv_raw
                       iv_type = 'E' iv_msg = lv_err ).
          cs_obj-err = abap_true.
          CONTINUE.
        ENDIF.
        ASSIGN lr_new->* TO FIELD-SYMBOL(<lv_new>).

        DATA lr_cur TYPE REF TO data.
        CLEAR lr_cur.
        IF lv_create = abap_false AND mo_db IS BOUND.
          lr_cur = mo_db->value( is_f = ls_f iv_num = cs_obj-num iv_key = lv_key ).
        ENDIF.
        DATA(lv_oldx) = ``.
        DATA(lv_same) = abap_false.
        IF lr_cur IS BOUND.
          ASSIGN lr_cur->* TO FIELD-SYMBOL(<lv_cur>).
          lv_oldx = lcl_val=>to_ext( is_f = ls_f iv_val = <lv_cur> ).
          lv_same = xsdbool( <lv_new> = <lv_cur> ).
        ELSE.
          lv_same = xsdbool( <lv_new> IS INITIAL ).
        ENDIF.
        IF lv_same = abap_true.
          CONTINUE.
        ENDIF.
        add_chg( EXPORTING is_chg = VALUE #( row = ls_r-row grp = ls_f-grp key = lv_key fld = ls_f new = lr_new
                                             oldx = lv_oldx newx = lcl_val=>to_ext( is_f = ls_f iv_val = <lv_new> )
                                             clear = xsdbool( <lv_new> IS INITIAL ) )
                 CHANGING  cs_obj = cs_obj ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD validate.
    " Every value that goes to SAP is checked against its check table and
    " fixed values, with the other fields of the same record as context.
    LOOP AT cs_obj-chg INTO DATA(ls_c) WHERE clear = abap_false.
      DATA lt_ctx TYPE lcl_chk=>tt_ctx.
      CLEAR lt_ctx.
      " the record as it is
      IF mo_db IS BOUND AND cs_obj-exists = abap_true.
        DATA lr_rec TYPE REF TO data.
        lr_rec = mo_db->rec( iv_grp = ls_c-fld-grp iv_num = cs_obj-num iv_key = ls_c-key ).
        IF lr_rec IS BOUND.
          ASSIGN lr_rec->* TO FIELD-SYMBOL(<ls_rec>).
          DATA(lo_s) = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( <ls_rec> ) ).
          LOOP AT lo_s->components INTO DATA(ls_comp).
            ASSIGN COMPONENT ls_comp-name OF STRUCTURE <ls_rec> TO FIELD-SYMBOL(<lv_rc>).
            IF sy-subrc = 0 AND ls_comp-type_kind CA 'CNDTg'.
              INSERT VALUE #( fld = ls_comp-name val = condense( CONV string( <lv_rc> ) ) ) INTO TABLE lt_ctx.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
      " what the file sends, on top
      LOOP AT cs_obj-chg INTO DATA(ls_o) WHERE grp = ls_c-grp AND key = ls_c-key.
        ASSIGN ls_o-new->* TO FIELD-SYMBOL(<lv_o>).
        DELETE lt_ctx WHERE fld = ls_o-fld-fld.
        INSERT VALUE #( fld = ls_o-fld-fld val = condense( CONV string( <lv_o> ) ) ) INTO TABLE lt_ctx.
      ENDLOOP.
      " and the keys of the record
      DATA(lt_kn) = lcl_cat=>key_names( ls_c-grp ).
      SPLIT ls_c-key AT `|` INTO TABLE DATA(lt_kv).
      DATA lv_kx TYPE i.
      CLEAR lv_kx.
      LOOP AT lt_kn INTO DATA(lv_kn).
        lv_kx = lv_kx + 1.
        DATA(lv_kf) = CONV fieldname( substring_after( val = lv_kn sub = `-` ) ).
        IF NOT line_exists( lt_ctx[ fld = lv_kf ] ).
          INSERT VALUE #( fld = lv_kf val = VALUE #( lt_kv[ lv_kx ] OPTIONAL ) ) INTO TABLE lt_ctx.
        ENDIF.
      ENDLOOP.

      ASSIGN ls_c-new->* TO FIELD-SYMBOL(<lv_v>).
      DATA(lv_msg) = lcl_chk=>check( is_f = ls_c-fld iv_val = condense( CONV string( <lv_v> ) ) it_ctx = lt_ctx ).
      IF lv_msg IS NOT INITIAL.
        mo_log->add( iv_row = ls_c-row iv_obj = cs_obj-tag iv_field = ls_c-fld-name iv_new = ls_c-newx
                     iv_type = 'E' iv_msg = lv_msg ).
        cs_obj-err = abap_true.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD confirm_clears.
    rv = abap_true.
    DATA lv_n TYPE i.
    LOOP AT it_obj INTO DATA(ls_o) WHERE err = abap_false.
      LOOP AT ls_o-chg TRANSPORTING NO FIELDS WHERE clear = abap_true.
        lv_n = lv_n + 1.
      ENDLOOP.
    ENDLOOP.
    IF lv_n = 0.
      RETURN.
    ENDIF.
    DATA lv_ans TYPE c LENGTH 1.
    DATA lv_q   TYPE c LENGTH 400.
    lv_q = |{ lv_n } field(s) are empty in the file but filled in SAP. Empty cells clear the field. | &&
           |Clear them? To leave a field as it is, put # in its cell instead.|.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING  titlebar              = 'Empty cells'
                 text_question         = lv_q
                 text_button_1         = 'Clear them'
                 icon_button_1         = 'ICON_DELETE'
                 text_button_2         = 'Cancel'
                 icon_button_2         = 'ICON_CANCEL'
                 default_button        = '2'
                 display_cancel_button = ' '
      IMPORTING  answer                = lv_ans
      EXCEPTIONS OTHERS                = 1.
    rv = xsdbool( sy-subrc = 0 AND lv_ans = '1' ).
  ENDMETHOD.

  " ---- generic access to the API structure -----------------------------

  METHOD sub.
    IF ir IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN ir->* TO FIELD-SYMBOL(<ls_s>).
    SPLIT iv_path AT `-` INTO TABLE DATA(lt_p).
    LOOP AT lt_p INTO DATA(lv_p).
      ASSIGN COMPONENT lv_p OF STRUCTURE <ls_s> TO FIELD-SYMBOL(<lv_c>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
      ASSIGN <lv_c> TO <ls_s>.
    ENDLOOP.
    GET REFERENCE OF <ls_s> INTO rr.
  ENDMETHOD.

  METHOD put.
    DATA(lr) = sub( ir = ir iv_path = iv_path ).
    IF lr IS BOUND.
      ASSIGN lr->* TO FIELD-SYMBOL(<lv>).
      <lv> = iv_val.
    ENDIF.
  ENDMETHOD.

  METHOD new_line.
    FIELD-SYMBOLS <lt> TYPE STANDARD TABLE.
    ASSIGN ir_tab->* TO <lt>.
    IF sy-subrc = 0.
      DATA lr_new TYPE REF TO data.
      APPEND INITIAL LINE TO <lt> REFERENCE INTO lr_new.
      rr = lr_new.
    ENDIF.
  ENDMETHOD.

  METHOD set_field.
    " DATA-<component> takes the value, DATAX-<component> says "change it".
    DATA(lr_d) = sub( ir = ir_line iv_path = |DATA-{ is_chg-fld-api }| ).
    IF lr_d IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN lr_d->* TO FIELD-SYMBOL(<lv_d>).
    ASSIGN is_chg-new->* TO FIELD-SYMBOL(<lv_v>).
    TRY.
        <lv_d> = <lv_v>.
      CATCH cx_sy_conversion_error.
        RETURN.
    ENDTRY.
    put( ir = ir_line iv_path = |DATAX-{ is_chg-fld-api }| iv_val = abap_true ).
  ENDMETHOD.

  METHOD line_of.
    DATA(ls_g) = lcl_cat=>grp( is_ent-grp ).
    DATA(lv_id) = |{ is_ent-grp }/{ is_ent-key }|.
    READ TABLE mt_line INTO DATA(ls_l) WITH TABLE KEY key = lv_id.
    IF sy-subrc = 0.
      rr = ls_l-ref.
      RETURN.
    ENDIF.

    " Where the part's table sits: on the record itself, or inside the line
    " of its company code / sales area / purchasing organisation.
    DATA lr_owner TYPE REF TO data.
    DATA lv_path TYPE string.
    lr_owner = ir_obj.
    CASE is_ent-grp.
      WHEN 'COMP'. lv_path = `COMPANY_DATA-COMPANY`.
      WHEN 'SALE'. lv_path = `SALES_DATA-SALES`.
      WHEN 'PURC'. lv_path = `PURCHASING_DATA-PURCHASING`.
      WHEN 'TAXI'. lv_path = `CENTRAL_DATA-TAX_IND-TAX_IND`.
      WHEN 'UNLD'. lv_path = `CENTRAL_DATA-LOADING-LOADING`.
      WHEN 'BANK'. lv_path = `CENTRAL_DATA-BANKDETAIL-BANKDETAILS`.
      WHEN 'CONT'. lv_path = `CENTRAL_DATA-CONTACT-CONTACTS`.
      WHEN 'PART' OR 'DUNN' OR 'WTAX'.
        " Inside the parent's line.
        DATA(lv_pn) = lines( lcl_cat=>key_names( ls_g-parent ) ).
        SPLIT is_ent-key AT `|` INTO TABLE DATA(lt_kp).
        DATA lv_pk TYPE string.
        CLEAR lv_pk.
        LOOP AT lt_kp INTO DATA(lv_kp) TO lv_pn.
          lv_pk = COND #( WHEN sy-tabix = 1 THEN lv_kp ELSE |{ lv_pk }\|{ lv_kp }| ).
        ENDLOOP.
        " The parent's own entry sets its real task when it is placed.
        lr_owner = line_of( ir_obj = ir_obj is_ent = VALUE #( grp = ls_g-parent key = lv_pk task = gc_upd ) ).
        lv_path = SWITCH #( is_ent-grp WHEN 'PART' THEN `FUNCTIONS-FUNCTIONS`
                                       WHEN 'DUNN' THEN `DUNNING-DUNNING`
                                       ELSE `WTAX_TYPE-WTAX_TYPE` ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    rr = new_line( sub( ir = lr_owner iv_path = lv_path ) ).
    IF rr IS NOT BOUND.
      RETURN.
    ENDIF.
    put( ir = rr iv_path = `TASK` iv_val = is_ent-task ).
    " DATA_KEY: the part's own keys, the last parts of the entry key.
    SPLIT ls_g-keys AT ` ` INTO TABLE DATA(lt_own).
    SPLIT is_ent-key AT `|` INTO TABLE DATA(lt_kv).
    DATA(lv_off) = lines( lt_kv ) - lines( lt_own ).
    DATA lv_ki TYPE i.
    LOOP AT lt_own INTO DATA(lv_own).
      lv_ki = lv_off + sy-tabix.
      DATA(lv_val) = VALUE string( lt_kv[ lv_ki ] OPTIONAL ).
      put( ir = rr iv_path = |DATA_KEY-{ lv_own }| iv_val = lv_val ).
    ENDLOOP.
    INSERT VALUE #( key = lv_id ref = rr ) INTO TABLE mt_line.
  ENDMETHOD.

  METHOD text_lines.
    " A cell may hold several lines; a line longer than a text line is
    " carried on in the next.
    DATA(lv) = iv_text.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv WITH cl_abap_char_utilities=>newline.
    SPLIT lv AT cl_abap_char_utilities=>newline INTO TABLE DATA(lt_p).
    LOOP AT lt_p INTO DATA(lv_p).
      DATA(lv_first) = abap_true.
      WHILE lv_first = abap_true OR lv_p IS NOT INITIAL.
        DATA ls_l TYPE tline.
        ls_l-tdformat = COND #( WHEN lv_first = abap_true THEN '*' ELSE '=' ).
        ls_l-tdline   = lv_p.
        APPEND ls_l TO rt.
        lv_first = abap_false.
        lv_p = COND #( WHEN strlen( lv_p ) > 132 THEN substring( val = lv_p off = 132 ) ELSE `` ).
      ENDWHILE.
    ENDLOOP.
  ENDMETHOD.

  METHOD build.
    CLEAR: es_ext, ev_guid, mt_line.
    DATA(lv_create) = xsdbool( is_obj-exists = abap_false ).
    DATA(lv_task) = COND #( WHEN lv_create = abap_true THEN gc_ins ELSE gc_upd ).
    DATA lr_ext TYPE REF TO data.
    GET REFERENCE OF es_ext INTO lr_ext.
    DATA(lr_obj) = sub( ir = lr_ext iv_path = COND #( WHEN gc_side = 'C' THEN `CUSTOMER` ELSE `VENDOR` ) ).

    " ---- the customer / supplier
    put( ir = lr_obj iv_path = `HEADER-OBJECT_TASK` iv_val = lv_task ).
    IF is_obj-num IS NOT INITIAL.
      put( ir = lr_obj iv_path = |HEADER-OBJECT_INSTANCE-{ lcl_cat=>grp( 'CENT' )-numfld }| iv_val = is_obj-num ).
    ENDIF.
    DATA(lr_cd)  = sub( ir = lr_obj iv_path = `CENTRAL_DATA-CENTRAL` ).
    IF lv_create = abap_true.
      DATA(lv_ktf) = COND string( WHEN gc_side = 'C' THEN `KTOKD` ELSE `KTOKK` ).
      put( ir = lr_cd iv_path = |DATA-{ lv_ktf }| iv_val = p_ktogr ).
      put( ir = lr_cd iv_path = |DATAX-{ lv_ktf }| iv_val = abap_true ).
    ENDIF.

    " ---- the entries: company codes, sales areas, repeating entries
    LOOP AT is_obj-ent INTO DATA(ls_e).
      DATA(lr_l) = line_of( ir_obj = lr_obj is_ent = ls_e ).
      " The task of a line created as a parent is the parent's own.
      IF lr_l IS BOUND.
        put( ir = lr_l iv_path = `TASK` iv_val = ls_e-task ).
      ENDIF.
    ENDLOOP.

    " ---- the values
    DATA lv_addr TYPE abap_bool.
    TYPES: BEGIN OF ty_com,
             kind TYPE string,
             cons TYPE ad_consnum,
             val  TYPE string,
             ext  TYPE string,
             xval TYPE abap_bool,
             xext TYPE abap_bool,
             del  TYPE abap_bool,
           END OF ty_com.
    DATA lt_com TYPE STANDARD TABLE OF ty_com WITH EMPTY KEY.
    LOOP AT is_obj-chg INTO DATA(ls_c).
      ASSIGN ls_c-new->* TO FIELD-SYMBOL(<lv_new>).
      CASE ls_c-fld-grp.
        WHEN 'CENT'.
          set_field( ir_line = lr_cd is_chg = ls_c ).

        WHEN 'ADDR'.
          set_field( ir_line = sub( ir = lr_obj iv_path = `CENTRAL_DATA-ADDRESS-POSTAL` ) is_chg = ls_c ).
          lv_addr = abap_true.

        WHEN 'COMM'.
          " One line per kind of number: the standard telephone, the
          " mobile, the fax, the e-mail.
          DATA(lv_kind) = COND string( WHEN ls_c-fld-api = 'MOBILE' THEN `MOB`
                                       WHEN ls_c-fld-tab = 'ADR2'   THEN `TEL`
                                       WHEN ls_c-fld-tab = 'ADR3'   THEN `FAX`
                                       ELSE `SMTP` ).
          READ TABLE lt_com ASSIGNING FIELD-SYMBOL(<ls_com>) WITH KEY kind = lv_kind.
          IF sy-subrc <> 0.
            APPEND VALUE #( kind = lv_kind cons = COND #( WHEN mo_db IS BOUND
                                                          THEN mo_db->consnumber( is_f = ls_c-fld iv_num = is_obj-num ) ) )
                   TO lt_com ASSIGNING <ls_com>.
          ENDIF.
          IF ls_c-fld-api = 'EXTENSION'.
            <ls_com>-ext  = <lv_new>.
            <ls_com>-xext = abap_true.
          ELSE.
            <ls_com>-val  = <lv_new>.
            <ls_com>-xval = abap_true.
            <ls_com>-del  = ls_c-clear.
          ENDIF.
          lv_addr = abap_true.

        WHEN 'TXTC' OR 'TXTB' OR 'TXTS'.
          DATA lr_tt TYPE REF TO data.
          IF ls_c-fld-grp = 'TXTC'.
            lr_tt = sub( ir = lr_obj iv_path = `CENTRAL_DATA-TEXT-TEXTS` ).
          ELSE.
            DATA(lv_pg) = lcl_cat=>grp( ls_c-fld-grp )-parent.
            DATA(lr_pl) = line_of( ir_obj = lr_obj is_ent = VALUE #( grp = lv_pg key = ls_c-key
                                     task = VALUE #( is_obj-ent[ grp = lv_pg key = ls_c-key ]-task DEFAULT gc_upd ) ) ).
            lr_tt = sub( ir = lr_pl iv_path = `TEXTS-TEXTS` ).
          ENDIF.
          DATA(lr_tl) = new_line( lr_tt ).
          IF lr_tl IS BOUND.
            DATA lr_old TYPE REF TO data.
            CLEAR lr_old.
            IF mo_db IS BOUND.
              lr_old = mo_db->value( is_f = ls_c-fld iv_num = is_obj-num iv_key = ls_c-key ).
            ENDIF.
            DATA(lv_had) = abap_false.
            IF lr_old IS BOUND.
              ASSIGN lr_old->* TO FIELD-SYMBOL(<lv_old>).
              lv_had = xsdbool( <lv_old> IS NOT INITIAL ).
            ENDIF.
            put( ir = lr_tl iv_path = `TASK`
                 iv_val = COND string( WHEN ls_c-clear = abap_true THEN gc_del
                                       WHEN lv_had = abap_true    THEN gc_upd
                                       ELSE gc_ins ) ).
            put( ir = lr_tl iv_path = `DATA_KEY-TEXT_ID` iv_val = ls_c-fld-tdid ).
            put( ir = lr_tl iv_path = `DATA_KEY-LANGU`   iv_val = p_tlang ).
            IF ls_c-clear = abap_false.
              put( ir = lr_tl iv_path = `DATA` iv_val = text_lines( CONV string( <lv_new> ) ) ).
            ENDIF.
          ENDIF.

        WHEN OTHERS.
          " A company code, a sales area or a repeating entry.
          READ TABLE is_obj-ent INTO DATA(ls_en) WITH KEY grp = ls_c-fld-grp key = ls_c-key.
          IF sy-subrc <> 0.
            CONTINUE.
          ENDIF.
          DATA(lr_el) = line_of( ir_obj = lr_obj is_ent = ls_en ).
          IF ls_c-fld-name = `KNVP-PARTNER`.
            " The partner number has the length its partner type uses:
            " a personnel number has 8 digits, the others 10.
            DATA(lv_parvw) = CONV parvw( segment( val = ls_c-key index = lines( lcl_cat=>key_names( ls_c-fld-grp ) ) - 1 sep = `|` ) ).
            DATA(lv_pv) = condense( CONV string( <lv_new> ) ).
            IF lv_pv CO '0123456789'.
              DATA(lv_len) = COND i( WHEN lcl_cfg=>nrart( lv_parvw ) = 'PE' THEN 8 ELSE 10 ).
              lv_pv = lcl_util=>alpha( iv_in = lv_pv iv_len = lv_len ).
            ENDIF.
            put( ir = lr_el iv_path = `DATA-PARTNER`  iv_val = lv_pv ).
            put( ir = lr_el iv_path = `DATAX-PARTNER` iv_val = abap_true ).
          ELSE.
            set_field( ir_line = lr_el is_chg = ls_c ).
          ENDIF.
      ENDCASE.
    ENDLOOP.

    " ---- telephone, mobile, fax, e-mail
    LOOP AT lt_com INTO DATA(ls_cm).
      DATA(lv_tpath) = SWITCH string( ls_cm-kind WHEN `TEL` OR `MOB` THEN `CENTRAL_DATA-ADDRESS-COMMUNICATION-PHONE-PHONE`
                                                 WHEN `FAX`          THEN `CENTRAL_DATA-ADDRESS-COMMUNICATION-FAX-FAX`
                                                 ELSE                     `CENTRAL_DATA-ADDRESS-COMMUNICATION-SMTP-SMTP` ).
      DATA(lv_vfld) = SWITCH string( ls_cm-kind WHEN `TEL` OR `MOB` THEN `TELEPHONE`
                                                WHEN `FAX`          THEN `FAX`
                                                ELSE                     `E_MAIL` ).
      DATA(lr_cl) = new_line( sub( ir = lr_obj iv_path = lv_tpath ) ).
      IF lr_cl IS NOT BOUND.
        CONTINUE.
      ENDIF.
      DATA(lv_ctask) = COND string( WHEN ls_cm-cons IS INITIAL                 THEN gc_ins
                                    WHEN ls_cm-del = abap_true                 THEN gc_del
                                    ELSE gc_upd ).
      put( ir = lr_cl iv_path = `CONTACT-TASK` iv_val = lv_ctask ).
      IF ls_cm-cons IS NOT INITIAL.
        put( ir = lr_cl iv_path = `CONTACT-DATA-CONSNUMBER`  iv_val = ls_cm-cons ).
        put( ir = lr_cl iv_path = `CONTACT-DATAX-CONSNUMBER` iv_val = abap_true ).
      ELSE.
        put( ir = lr_cl iv_path = `CONTACT-DATA-STD_NO`  iv_val = abap_true ).
        put( ir = lr_cl iv_path = `CONTACT-DATAX-STD_NO` iv_val = abap_true ).
      ENDIF.
      IF ls_cm-xval = abap_true.
        put( ir = lr_cl iv_path = |CONTACT-DATA-{ lv_vfld }|  iv_val = ls_cm-val ).
        put( ir = lr_cl iv_path = |CONTACT-DATAX-{ lv_vfld }| iv_val = abap_true ).
      ENDIF.
      IF ls_cm-xext = abap_true.
        put( ir = lr_cl iv_path = `CONTACT-DATA-EXTENSION`  iv_val = ls_cm-ext ).
        put( ir = lr_cl iv_path = `CONTACT-DATAX-EXTENSION` iv_val = abap_true ).
      ENDIF.
      IF ls_cm-kind = `MOB`.
        put( ir = lr_cl iv_path = `CONTACT-DATA-R_3_USER`  iv_val = gc_mobile ).
        put( ir = lr_cl iv_path = `CONTACT-DATAX-R_3_USER` iv_val = abap_true ).
      ENDIF.
    ENDLOOP.

    IF lv_addr = abap_true.
      put( ir = lr_obj iv_path = `CENTRAL_DATA-ADDRESS-TASK` iv_val = lv_task ).
      " A postal code is checked against its country: a row that changes
      " the one without the other sends the country SAP already has.
      DATA(lr_px) = sub( ir = lr_obj iv_path = `CENTRAL_DATA-ADDRESS-POSTAL-DATAX` ).
      FIELD-SYMBOLS: <lv_xp> TYPE any,
                     <lv_xc> TYPE any.
      IF lr_px IS BOUND.
        ASSIGN lr_px->* TO FIELD-SYMBOL(<ls_px>).
        ASSIGN COMPONENT 'POSTL_COD1' OF STRUCTURE <ls_px> TO <lv_xp>.
        ASSIGN COMPONENT 'COUNTRY' OF STRUCTURE <ls_px> TO <lv_xc>.
      ENDIF.
      IF <lv_xp> IS ASSIGNED AND <lv_xc> IS ASSIGNED AND <lv_xp> = abap_true AND <lv_xc> <> abap_true
         AND mo_db IS BOUND.
        DATA(lr_cty) = mo_db->value( is_f = lcl_cat=>by_name( `ADRC-COUNTRY` ) iv_num = is_obj-num iv_key = `` ).
        IF lr_cty IS BOUND.
          ASSIGN lr_cty->* TO FIELD-SYMBOL(<lv_cty>).
          put( ir = lr_obj iv_path = `CENTRAL_DATA-ADDRESS-POSTAL-DATA-COUNTRY` iv_val = <lv_cty> ).
          <lv_xc> = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

    " ---- the business partner
    es_ext-partner-header-object_task = lv_task.
    IF lv_create = abap_true.
      TRY.
          ev_guid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
        CATCH cx_uuid_error.
          CLEAR ev_guid.
      ENDTRY.
    ELSE.
      ev_guid = lcl_cfg=>guid_of( CONV @@NUMTYPE@@( is_obj-num ) ).
    ENDIF.
    es_ext-partner-header-object_instance-bpartnerguid = ev_guid.

    DATA(lv_bp) = COND bu_partner( WHEN lv_create = abap_false
                                   THEN lcl_cfg=>bp_of( CONV @@NUMTYPE@@( is_obj-num ) ) ).
    DATA lt_role TYPE lcl_cfg=>tt_role.
    DATA(lv_area) = abap_false.
    LOOP AT is_obj-ent INTO ls_e WHERE grp = 'SALE' OR grp = 'PURC'.
      lv_area = abap_true.
    ENDLOOP.
    IF lv_create = abap_true.
      es_ext-partner-central_data-common-data-bp_control-category = gc_org.
      es_ext-partner-central_data-common-data-bp_control-grouping =
        COND #( WHEN p_bpgrp IS NOT INITIAL THEN p_bpgrp ELSE lcl_cfg=>bp_group( p_ktogr ) ).
      lt_role = lcl_cfg=>bp_roles( p_ktogr ).
      IF lt_role IS INITIAL.
        APPEND COND bu_role( WHEN gc_side = 'C' THEN 'FLCU00' ELSE 'FLVN00' ) TO lt_role.
      ENDIF.
    ENDIF.
    IF lv_area = abap_true.
      DATA(lv_arole) = COND bu_role( WHEN gc_side = 'C' THEN 'FLCU01' ELSE 'FLVN01' ).
      IF NOT line_exists( lt_role[ table_line = lv_arole ] ).
        APPEND lv_arole TO lt_role.
      ENDIF.
    ENDIF.
    LOOP AT lt_role INTO DATA(lv_role).
      IF lv_create = abap_false AND lcl_cfg=>has_role( iv_bp = lv_bp iv_role = lv_role ) = abap_true.
        CONTINUE.
      ENDIF.
      APPEND VALUE bus_ei_bupa_roles( task = gc_ins data_key = lv_role ) TO es_ext-partner-central_data-role-roles.
    ENDLOOP.

    " A new partner needs its name and address of its own; on a change CVI
    " keeps the partner in step with the customer / supplier.
    IF lv_create = abap_true.
      DATA(lr_pd) = sub( ir = lr_obj iv_path = `CENTRAL_DATA-ADDRESS-POSTAL-DATA` ).
      DATA(lr_pdx) = sub( ir = lr_obj iv_path = `CENTRAL_DATA-ADDRESS-POSTAL-DATAX` ).
      ASSIGN lr_pd->* TO FIELD-SYMBOL(<ls_pd>).
      ASSIGN lr_pdx->* TO FIELD-SYMBOL(<ls_pdx>).
      DATA(lt_nm) = VALUE string_table( ( `NAME` ) ( `NAME1` ) ( `NAME_2` ) ( `NAME2` ) ( `NAME_3` ) ( `NAME3` )
                                        ( `NAME_4` ) ( `NAME4` ) ).
      DATA lv_i TYPE i VALUE 1.
      DATA lv_from TYPE string.
      DATA lv_to   TYPE string.
      WHILE lv_i < lines( lt_nm ).
        lv_from = lt_nm[ lv_i ].
        lv_to   = lt_nm[ lv_i + 1 ].
        ASSIGN COMPONENT lv_from OF STRUCTURE <ls_pd> TO FIELD-SYMBOL(<lv_nm>).
        IF sy-subrc = 0 AND <lv_nm> IS NOT INITIAL.
          ASSIGN COMPONENT lv_to OF STRUCTURE es_ext-partner-central_data-common-data-bp_organization
                 TO FIELD-SYMBOL(<lv_bn>).
          IF sy-subrc = 0.
            <lv_bn> = <lv_nm>.
            ASSIGN COMPONENT lv_to OF STRUCTURE es_ext-partner-central_data-common-datax-bp_organization
                   TO FIELD-SYMBOL(<lv_bx>).
            IF sy-subrc = 0.
              <lv_bx> = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
        lv_i = lv_i + 2.
      ENDWHILE.
      ASSIGN COMPONENT 'SORT1' OF STRUCTURE <ls_pd> TO FIELD-SYMBOL(<lv_s1>).
      IF sy-subrc = 0 AND <lv_s1> IS NOT INITIAL.
        es_ext-partner-central_data-common-data-bp_centraldata-searchterm1  = <lv_s1>.
        es_ext-partner-central_data-common-datax-bp_centraldata-searchterm1 = abap_true.
      ENDIF.
      DATA ls_adr TYPE bus_ei_bupa_address.
      CLEAR ls_adr.
      ls_adr-task = gc_ins.
      lcl_util=>copy_like( EXPORTING is_from  = <ls_pd>
                                     is_fromx = <ls_pdx>
                           CHANGING  cs_to    = ls_adr-data-postal-data
                                     cs_tox   = ls_adr-data-postal-datax ).
      IF ls_adr-data-postal-datax IS NOT INITIAL.
        APPEND ls_adr TO es_ext-partner-central_data-address-addresses.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD reset_bp.
    " The business partner keeps a global memory for the unit of work just
    " closed - the save mode included - and a COMMIT does not clear it.
    " Without this the next record is refused with "Parameter IV_X_SAVE".
    TRY.
        CALL FUNCTION 'BUP_MEMORY_CENTRAL_INIT'.
      CATCH cx_sy_dyn_call_illegal_func.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD post.
    rv = abap_true.
    DATA(lv_obj) = COND string( WHEN is_obj-num IS NOT INITIAL THEN is_obj-num ELSE |new (row { is_obj-row })| ).

    DATA lt_map TYPE mdg_bs_bp_msgmap_t.
    TRY.
        cl_md_bp_maintain=>validate_single( EXPORTING i_data        = is_ext
                                            IMPORTING et_return_map = lt_map ).
      CATCH cx_root INTO DATA(lx1).
        mo_log->add( iv_row = is_obj-row iv_obj = lv_obj iv_type = 'E'
                     iv_msg = |Validation failed: { lx1->get_text( ) }| ).
        rv = abap_false.
        RETURN.
    ENDTRY.
    LOOP AT lt_map INTO DATA(ls_map) WHERE type CA 'EAX'.
      mo_log->add( iv_row = is_obj-row iv_obj = lv_obj iv_type = 'E'
                   iv_field = |{ ls_map-bapistrucname }-{ ls_map-bapifldnm }| iv_msg = ls_map-message ).
      rv = abap_false.
    ENDLOOP.
    IF rv = abap_false.
      RETURN.
    ENDIF.

    DATA: lt_data TYPE cvis_ei_extern_t,
          lt_ret  TYPE bapiretm.
    APPEND is_ext TO lt_data.
    TRY.
        cl_md_bp_maintain=>maintain( EXPORTING i_data     = lt_data
                                               i_test_run = p_test
                                     IMPORTING e_return   = lt_ret ).
      CATCH cx_root INTO DATA(lx2).
        ROLLBACK WORK.
        reset_bp( ).
        mo_log->add( iv_row = is_obj-row iv_obj = lv_obj iv_type = 'E'
                     iv_msg = |Maintain failed: { lx2->get_text( ) }| ).
        rv = abap_false.
        RETURN.
    ENDTRY.

    FIELD-SYMBOLS: <lt_sub> TYPE ANY TABLE,
                   <ls_sub> TYPE any.
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
          mo_log->add( iv_row = is_obj-row iv_obj = lv_obj iv_type = 'E'
                       iv_field = ls_r2-field iv_msg = ls_r2-message ).
          rv = abap_false.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    IF rv = abap_false OR p_test = abap_true.
      ROLLBACK WORK.
      reset_bp( ).
      RETURN.
    ENDIF.

    " BAPI_TRANSACTION_COMMIT, not a bare COMMIT WORK: the business partner
    " hangs its own end of the unit of work on it, and its RETURN is the only
    " thing that says whether the update actually ran.
    DATA ls_cret TYPE bapiret2.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait   = abap_true
      IMPORTING return = ls_cret.
    reset_bp( ).
    IF ls_cret-type CA 'EAX'.
      mo_log->add( iv_row = is_obj-row iv_obj = lv_obj iv_type = 'E'
                   iv_msg = |The update did not go through - nothing was saved: { ls_cret-message }| ).
      rv = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD run.
    mo_log = NEW lcl_log( ).
    CLEAR mo_db.

    DATA lt_head TYPE tt_cell.
    DATA lt_rows TYPE tt_row.
    TRY.
        lcl_file=>read( IMPORTING et_head = lt_head et_rows = lt_rows ).
      CATCH lcx_dl INTO DATA(lx).
        DATA(lv_xt) = lx->get_text( ).
        MESSAGE lv_xt TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    IF lt_head IS INITIAL OR lt_rows IS INITIAL.
      MESSAGE 'The file has no data rows - row 1 holds the field names, the data starts in row 2'
              TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " ---- 1. headings
    IF map_cols( it_head = lt_head it_rows = lt_rows ) = abap_false OR check_cols( ) = abap_false.
      mo_log->show( 'The file cannot be read - nothing was posted' ).
      RETURN.
    ENDIF.

    " ---- 2. records
    DATA(lt_obj) = gather( lt_rows ).
    DATA lt_num TYPE string_table.
    LOOP AT lt_obj INTO DATA(ls_o) WHERE num IS NOT INITIAL.
      APPEND ls_o-num TO lt_num.
    ENDLOOP.
    IF lt_num IS NOT INITIAL.
      DATA lt_g TYPE string_table.
      LOOP AT mt_col INTO DATA(ls_c).
        IF NOT line_exists( lt_g[ table_line = CONV string( ls_c-fld-grp ) ] ).
          APPEND CONV string( ls_c-fld-grp ) TO lt_g.
        ENDIF.
        DATA(lv_rt) = lcl_cat=>root_of( ls_c-fld-grp ).
        IF NOT line_exists( lt_g[ table_line = CONV string( lv_rt ) ] ).
          APPEND CONV string( lv_rt ) TO lt_g.
        ENDIF.
      ENDLOOP.
      " The general data always, to know whether the record exists.
      IF NOT line_exists( lt_g[ table_line = `CENT` ] ).
        APPEND `CENT` TO lt_g.
      ENDIF.
      mo_db = NEW lcl_db( it_num = lt_num it_grp = lt_g ).
    ENDIF.

    LOOP AT lt_obj ASSIGNING FIELD-SYMBOL(<ls_o>).
      <ls_o>-exists = xsdbool( <ls_o>-num IS NOT INITIAL AND mo_db IS BOUND
                               AND mo_db->rec( iv_grp = 'CENT' iv_num = <ls_o>-num iv_key = `` ) IS BOUND ).
      IF p_chg = abap_true AND <ls_o>-exists = abap_false.
        mo_log->add( iv_row = <ls_o>-row iv_obj = <ls_o>-tag iv_type = 'E'
                     iv_msg = |{ <ls_o>-num } does not exist - choose Create to create it| ).
        <ls_o>-err = abap_true.
        CONTINUE.
      ENDIF.
      IF p_crt = abap_true AND <ls_o>-exists = abap_true.
        mo_log->add( iv_row = <ls_o>-row iv_obj = <ls_o>-tag iv_type = 'E'
                     iv_msg = |{ <ls_o>-num } exists already - choose Change to change it| ).
        <ls_o>-err = abap_true.
        CONTINUE.
      ENDIF.
      compare( CHANGING cs_obj = <ls_o> ).
      validate( CHANGING cs_obj = <ls_o> ).
    ENDLOOP.

    " ---- 3. one question for the cells that clear a field
    IF p_chg = abap_true AND confirm_clears( lt_obj ) = abap_false.
      mo_log->add( iv_type = 'W' iv_msg = 'Cancelled - nothing was posted. Put # in the cells to leave unchanged.' ).
      mo_log->show( 'Cancelled' ).
      RETURN.
    ENDIF.

    " ---- 4. post
    DATA lv_ok TYPE i.
    DATA lv_bad TYPE i.
    DATA lv_none TYPE i.
    LOOP AT lt_obj INTO ls_o.
      DATA(lv_obj) = COND string( WHEN ls_o-num IS NOT INITIAL THEN ls_o-num ELSE |new (row { ls_o-row })| ).
      IF ls_o-err = abap_true.
        lv_bad = lv_bad + 1.
        CONTINUE.
      ENDIF.
      IF ls_o-chg IS INITIAL AND NOT line_exists( ls_o-ent[ task = gc_del ] )
         AND NOT line_exists( ls_o-ent[ task = gc_ins ] ) AND ls_o-exists = abap_true.
        mo_log->add( iv_row = ls_o-row iv_obj = lv_obj iv_type = 'I' iv_msg = 'No change - the file matches SAP' ).
        lv_none = lv_none + 1.
        CONTINUE.
      ENDIF.

      DATA ls_ext TYPE cvis_ei_extern.
      DATA lv_guid TYPE bu_partner_guid.
      build( EXPORTING is_obj = ls_o IMPORTING es_ext = ls_ext ev_guid = lv_guid ).
      DATA(lv_posted) = post( is_ext = ls_ext is_obj = ls_o ).

      " The fields, with what they were and what they become.
      LOOP AT ls_o-chg INTO DATA(ls_ch).
        mo_log->add( iv_row = ls_ch-row iv_obj = lv_obj iv_part = lcl_cat=>grp( ls_ch-grp )-text
                     iv_field = ls_ch-fld-name iv_old = ls_ch-oldx iv_new = ls_ch-newx
                     iv_type = COND #( WHEN lv_posted = abap_false THEN 'W'
                                       WHEN p_test = abap_true    THEN 'I'
                                       ELSE 'S' )
                     iv_msg = COND string( WHEN lv_posted = abap_false THEN `Not saved - see the error above`
                                           WHEN p_test = abap_true    THEN `Would change (test run)`
                                           WHEN ls_ch-clear = abap_true THEN `Cleared`
                                           ELSE `Changed` ) ).
      ENDLOOP.
      LOOP AT ls_o-ent INTO DATA(ls_en) WHERE task = gc_del OR ( task = gc_ins AND grp <> 'COMP' AND grp <> 'SALE' AND grp <> 'PURC' ).
        mo_log->add( iv_row = ls_en-row iv_obj = lv_obj iv_part = lcl_cat=>grp( ls_en-grp )-text
                     iv_field = ls_en-key iv_type = COND #( WHEN lv_posted = abap_true THEN 'I' ELSE 'W' )
                     iv_msg = COND string( WHEN ls_en-task = gc_del THEN `Entry deleted` ELSE `Entry added` ) ).
      ENDLOOP.

      IF lv_posted = abap_false.
        lv_bad = lv_bad + 1.
        CONTINUE.
      ENDIF.
      lv_ok = lv_ok + 1.
      IF p_test = abap_true.
        mo_log->add( iv_row = ls_o-row iv_obj = lv_obj iv_type = 'S'
                     iv_msg = 'Test run OK - nothing was posted' ).
      ELSEIF ls_o-exists = abap_false.
        DATA(lv_new) = lcl_cfg=>num_by_guid( lv_guid ).
        mo_log->add( iv_row = ls_o-row iv_obj = lv_new iv_type = 'S'
                     iv_msg = |Created { lv_new } - business partner { lcl_cfg=>bp_by_guid( lv_guid ) }| ).
      ELSE.
        mo_log->add( iv_row = ls_o-row iv_obj = lv_obj iv_type = 'S' iv_msg = 'Saved' ).
      ENDIF.
    ENDLOOP.

    mo_log->show( |{ COND string( WHEN p_test = abap_true THEN `TEST RUN - ` ) }| &&
                  |{ lv_ok } record(s) { COND string( WHEN p_test = abap_true THEN `would be posted` ELSE `posted` ) }, | &&
                  |{ lv_bad } with errors, { lv_none } unchanged| ).
  ENDMETHOD.

ENDCLASS.

