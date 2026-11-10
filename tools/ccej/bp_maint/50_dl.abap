*----------------------------------------------------------------------*
* LCL_SEL - which fields go into the file
*   The user ticks fields in a list; the keys and the fields the system
*   needs are added. The choice lives in S_FLD, so a screen variant keeps
*   it.
*----------------------------------------------------------------------*
CLASS lcl_sel DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS choose.
    CLASS-METHODS chosen RETURNING VALUE(rt) TYPE string_table.
    " The complete list of columns, in file order: the number, the keys,
    " ACTION where a repeating part is chosen, then the data fields.
    " EV_ERR when the choice cannot go into one file.
    CLASS-METHODS complete
      IMPORTING it_names  TYPE string_table
                iv_create TYPE abap_bool
      EXPORTING et_fld    TYPE tt_fld
                ev_action TYPE abap_bool
                ev_err    TYPE string.
    " The one repeating part among these fields, if there is one.
    CLASS-METHODS rep_of
      IMPORTING it_fld    TYPE tt_fld
      RETURNING VALUE(rv) TYPE ty_grp.
    " Parts that may stand in a file next to a repeating part.
    CLASS-METHODS allowed_with
      IMPORTING iv_rep    TYPE ty_grp
      RETURNING VALUE(rt) TYPE string_table.
ENDCLASS.


CLASS lcl_sel IMPLEMENTATION.

  METHOD chosen.
    LOOP AT s_fld INTO DATA(ls_s) WHERE sign = 'I' AND option = 'EQ'.
      APPEND condense( CONV string( ls_s-low ) ) TO rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD choose.
    TYPES: BEGIN OF ty_pick,
             sel  TYPE c LENGTH 1,
             part TYPE c LENGTH 30,
             name TYPE c LENGTH 60,
             text TYPE c LENGTH 70,
           END OF ty_pick.
    DATA lt_pick TYPE STANDARD TABLE OF ty_pick WITH EMPTY KEY.
    DATA(lt_now) = chosen( ).
    DATA(lt_all) = lcl_cat=>all( ).
    LOOP AT lt_all INTO DATA(ls_f) WHERE key = abap_false.
      APPEND VALUE #( sel  = COND #( WHEN line_exists( lt_now[ table_line = ls_f-name ] ) THEN 'X' )
                      part = lcl_cat=>grp( ls_f-grp )-text
                      name = ls_f-name
                      text = ls_f-text ) TO lt_pick.
    ENDLOOP.

    DATA lt_fcat TYPE slis_t_fieldcat_alv.
    lt_fcat = VALUE #( ( fieldname = 'PART' tabname = '1' seltext_l = 'Part'        outputlen = 26 )
                       ( fieldname = 'NAME' tabname = '1' seltext_l = 'Field'       outputlen = 34 )
                       ( fieldname = 'TEXT' tabname = '1' seltext_l = 'Description' outputlen = 50 ) ).
    DATA lv_exit TYPE c LENGTH 1.
    CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
      EXPORTING  i_title              = 'Choose the fields for the file'
                 i_selection          = 'X'
                 i_zebra              = 'X'
                 i_checkbox_fieldname = 'SEL'
                 i_tabname            = '1'
                 it_fieldcat          = lt_fcat
                 i_screen_start_column = 5
                 i_screen_start_line   = 3
                 i_screen_end_column   = 125
                 i_screen_end_line     = 25
      IMPORTING  e_exit               = lv_exit
      TABLES     t_outtab             = lt_pick
      EXCEPTIONS OTHERS               = 1.
    IF sy-subrc <> 0 OR lv_exit = 'X'.
      RETURN.
    ENDIF.
    CLEAR s_fld[].
    LOOP AT lt_pick INTO DATA(ls_p) WHERE sel = 'X'.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_p-name ) TO s_fld.
    ENDLOOP.
  ENDMETHOD.

  METHOD rep_of.
    LOOP AT it_fld INTO DATA(ls_f).
      IF lcl_cat=>grp( ls_f-grp )-rep = abap_true.
        rv = ls_f-grp.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD allowed_with.
    rt = VALUE #( ( `CENT` ) ( `ADDR` ) ( `COMM` ) ( `TXTC` ) ( CONV string( iv_rep ) ) ).
    DATA(lv_p) = lcl_cat=>grp( iv_rep )-parent.
    IF lv_p IS NOT INITIAL.
      APPEND CONV string( lv_p ) TO rt.
      " the parent's texts
      LOOP AT lcl_cat=>groups( ) INTO DATA(ls_g) WHERE parent = lv_p AND rep = abap_false.
        APPEND CONV string( ls_g-grp ) TO rt.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD complete.
    CLEAR: et_fld, ev_action, ev_err.
    DATA(lt_all) = lcl_cat=>all( ).
    DATA lt_pick TYPE tt_fld.
    LOOP AT it_names INTO DATA(lv_n).
      READ TABLE lt_all INTO DATA(ls_f) WITH KEY name = lv_n.
      IF sy-subrc = 0.
        APPEND ls_f TO lt_pick.
      ENDIF.
    ENDLOOP.

    " ---- at most one repeating part, and only what belongs next to it
    DATA lt_rep TYPE SORTED TABLE OF ty_grp WITH UNIQUE KEY table_line.
    LOOP AT lt_pick INTO ls_f.
      IF lcl_cat=>grp( ls_f-grp )-rep = abap_true.
        INSERT ls_f-grp INTO TABLE lt_rep.
      ENDIF.
    ENDLOOP.
    IF lines( lt_rep ) > 1.
      DATA lv_r TYPE string.
      LOOP AT lt_rep INTO DATA(lv_g).
        lv_r = COND #( WHEN lv_r IS INITIAL THEN lcl_cat=>grp( lv_g )-text
                       ELSE |{ lv_r }, { lcl_cat=>grp( lv_g )-text }| ).
      ENDLOOP.
      ev_err = |Fields of one repeating part per file only - this choice has { lv_r }|.
      RETURN.
    ENDIF.
    DATA(lv_rep) = VALUE ty_grp( lt_rep[ 1 ] OPTIONAL ).
    IF lv_rep IS NOT INITIAL.
      DATA(lt_ok) = allowed_with( lv_rep ).
      LOOP AT lt_pick INTO ls_f.
        IF NOT line_exists( lt_ok[ table_line = CONV string( ls_f-grp ) ] ).
          ev_err = |{ lcl_cat=>grp( lv_rep )-text } can only be combined with general data | &&
                   |and { lcl_cat=>grp( lcl_cat=>root_of( lv_rep ) )-text } - remove { ls_f-name }|.
          RETURN.
        ENDIF.
      ENDLOOP.
      ev_action = abap_true.
    ENDIF.

    " ---- the columns the system needs
    DATA lt_need TYPE string_table.
    APPEND lcl_cat=>num_name( ) TO lt_need.
    DATA lt_grp TYPE SORTED TABLE OF ty_grp WITH UNIQUE KEY table_line.
    LOOP AT lt_pick INTO ls_f.
      INSERT ls_f-grp INTO TABLE lt_grp.
    ENDLOOP.
    LOOP AT lt_grp INTO lv_g.
      APPEND LINES OF lcl_cat=>key_names( lv_g ) TO lt_need.
      " A text of a company code or a sales area needs that key too.
      DATA(lv_par) = lcl_cat=>grp( lv_g )-parent.
      IF lv_par IS NOT INITIAL.
        APPEND LINES OF lcl_cat=>key_names( lv_par ) TO lt_need.
      ENDIF.
    ENDLOOP.
    IF iv_create = abap_true.
      APPEND LINES OF VALUE string_table( ( `ADRC-NAME1` ) ( `ADRC-SORT1` )
                                          ( `ADRC-COUNTRY` ) ( `ADRC-LANGU` ) ) TO lt_need.
      DATA(lv_b) = COND string( WHEN gc_side = 'C' THEN `KNB1` ELSE `LFB1` ).
      IF line_exists( lt_grp[ table_line = 'COMP' ] ) OR line_exists( lt_grp[ table_line = 'DUNN' ] )
         OR line_exists( lt_grp[ table_line = 'WTAX' ] ) OR line_exists( lt_grp[ table_line = 'TXTB' ] ).
        APPEND |{ lv_b }-AKONT| TO lt_need.
      ENDIF.
      IF line_exists( lt_grp[ table_line = 'SALE' ] ) OR line_exists( lt_grp[ table_line = 'PURC' ] )
         OR line_exists( lt_grp[ table_line = 'PART' ] ) OR line_exists( lt_grp[ table_line = 'TXTS' ] ).
        APPEND COND string( WHEN gc_side = 'C' THEN `KNVV-WAERS` ELSE `LFM1-WAERS` ) TO lt_need.
      ENDIF.
    ENDIF.

    " ---- the number, then keys in part order, then the data fields
    DATA lt_names TYPE string_table.
    LOOP AT lt_need INTO lv_n.
      IF NOT line_exists( lt_names[ table_line = lv_n ] ) AND line_exists( lt_all[ name = lv_n ] )
         AND lt_all[ name = lv_n ]-key = abap_true.
        APPEND lv_n TO lt_names.
      ENDIF.
    ENDLOOP.
    LOOP AT lt_need INTO lv_n.
      IF NOT line_exists( lt_names[ table_line = lv_n ] ) AND line_exists( lt_all[ name = lv_n ] ).
        APPEND lv_n TO lt_names.
      ENDIF.
    ENDLOOP.
    LOOP AT lt_all INTO ls_f.
      IF line_exists( lt_pick[ name = ls_f-name ] ) AND NOT line_exists( lt_names[ table_line = ls_f-name ] ).
        APPEND ls_f-name TO lt_names.
      ENDIF.
    ENDLOOP.
    LOOP AT lt_names INTO lv_n.
      READ TABLE lt_all INTO ls_f WITH KEY name = lv_n.
      IF line_exists( lt_need[ table_line = lv_n ] ).
        ls_f-must = abap_true.
      ENDIF.
      APPEND ls_f TO et_fld.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* LCL_DL - the download
*----------------------------------------------------------------------*
CLASS lcl_dl DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS run.
  PRIVATE SECTION.
    CLASS-METHODS numbers RETURNING VALUE(rt) TYPE string_table.
    " The first N parts of a key: "0801|Z1|Z0|ZR|000", 3 -> "0801|Z1|Z0".
    CLASS-METHODS prefix
      IMPORTING iv_key    TYPE string
                iv_n      TYPE i
      RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS in_scope
      IMPORTING iv_grp    TYPE ty_grp
                iv_key    TYPE string
      RETURNING VALUE(rv) TYPE abap_bool.
ENDCLASS.


CLASS lcl_dl IMPLEMENTATION.

  METHOD numbers.
    DATA lt_k TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    IF s_num[] IS NOT INITIAL.
      IF gc_side = 'C'.
        " Typed tables: one column read into an inline table gives a
        " structured line, which a string template cannot take.
        DATA lt_c TYPE STANDARD TABLE OF kunnr WITH EMPTY KEY.
        SELECT kunnr FROM kna1 WHERE kunnr IN @s_num ORDER BY kunnr
          INTO TABLE @lt_c UP TO @p_max ROWS.
        LOOP AT lt_c INTO DATA(lv_cust).
          INSERT |{ lv_cust }| INTO TABLE lt_k.
        ENDLOOP.
      ELSE.
        DATA lt_v TYPE STANDARD TABLE OF lifnr WITH EMPTY KEY.
        SELECT lifnr FROM lfa1 WHERE lifnr IN @s_num ORDER BY lifnr
          INTO TABLE @lt_v UP TO @p_max ROWS.
        LOOP AT lt_v INTO DATA(lv_vend).
          INSERT |{ lv_vend }| INTO TABLE lt_k.
        ENDLOOP.
      ENDIF.
    ENDIF.
    IF s_bp[] IS NOT INITIAL.
      DATA lt_bp TYPE STANDARD TABLE OF bu_partner WITH EMPTY KEY.
      SELECT partner FROM but000 WHERE partner IN @s_bp
        INTO TABLE @lt_bp UP TO @p_max ROWS.
      DATA(lt_n) = lcl_cfg=>nums_of_bp( lt_bp ).
      LOOP AT lt_n INTO DATA(lv_n).
        INSERT lv_n INTO TABLE lt_k.
      ENDLOOP.
    ENDIF.
    LOOP AT lt_k INTO DATA(lv_k).
      IF lines( rt ) >= p_max.
        EXIT.
      ENDIF.
      APPEND lv_k TO rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD prefix.
    SPLIT iv_key AT `|` INTO TABLE DATA(lt_p).
    LOOP AT lt_p INTO DATA(lv_p) TO iv_n.
      rv = COND #( WHEN sy-tabix = 1 THEN lv_p ELSE |{ rv }\|{ lv_p }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD in_scope.
    " The company code and sales area / purchasing organisation ranges on
    " the screen limit which entries are written.
    DATA(lv_root) = lcl_cat=>root_of( iv_grp ).
    SPLIT iv_key AT `|` INTO TABLE DATA(lt_p).
    " The first key part, moved into a field of the range's own type. Not
    " VALUE bukrs( lt_p[ 1 ] OPTIONAL ): the lines of LT_P are strings, and
    " a VALUE with a table expression takes only a compatible type.
    DATA: lv_first TYPE string,
          lv_bukrs TYPE bukrs,
          lv_org   TYPE @@ORGTYPE@@.
    READ TABLE lt_p INTO lv_first INDEX 1.
    rv = abap_true.
    CASE lv_root.
      WHEN 'COMP'.
        IF s_bukrs[] IS NOT INITIAL.
          lv_bukrs = lv_first.
          rv = xsdbool( lv_bukrs IN s_bukrs ).
        ENDIF.
      WHEN 'SALE' OR 'PURC'.
        IF s_org[] IS NOT INITIAL.
          lv_org = lv_first.
          rv = xsdbool( lv_org IN s_org ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD run.
    DATA lt_fld TYPE tt_fld.
    DATA lv_act TYPE abap_bool.
    DATA lv_err TYPE string.
    lcl_sel=>complete( EXPORTING it_names  = lcl_sel=>chosen( )
                                 iv_create = p_crt
                       IMPORTING et_fld    = lt_fld
                                 ev_action = lv_act
                                 ev_err    = lv_err ).
    IF lv_err IS NOT INITIAL.
      MESSAGE lv_err TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    " ---- headings
    DATA lt_head TYPE tt_cell.
    DATA(lv_num) = lcl_cat=>num_name( ).
    DATA(lv_rep) = lcl_sel=>rep_of( lt_fld ).
    " ACTION follows the last key column.
    DATA lv_last_key TYPE i.
    LOOP AT lt_fld INTO DATA(ls_f) WHERE key = abap_true.
      lv_last_key = sy-tabix.
    ENDLOOP.
    DATA lv_ix TYPE i.
    LOOP AT lt_fld INTO ls_f.
      lv_ix = lv_ix + 1.
      APPEND ls_f-name TO lt_head.
      IF lv_act = abap_true AND lv_ix = lv_last_key.
        APPEND gc_action TO lt_head.
      ENDIF.
    ENDLOOP.

    " ---- rows: none without numbers, the records of each number otherwise
    DATA lt_rows TYPE tt_row.
    DATA(lt_nums) = numbers( ).
    IF lt_nums IS NOT INITIAL.
      DATA lt_g TYPE string_table.
      LOOP AT lt_fld INTO ls_f.
        IF NOT line_exists( lt_g[ table_line = CONV string( ls_f-grp ) ] ).
          APPEND CONV string( ls_f-grp ) TO lt_g.
        ENDIF.
        DATA(lv_root) = lcl_cat=>root_of( ls_f-grp ).
        IF NOT line_exists( lt_g[ table_line = CONV string( lv_root ) ] ).
          APPEND CONV string( lv_root ) TO lt_g.
        ENDIF.
      ENDLOOP.
      DATA(lo_db) = NEW lcl_db( it_num = lt_nums it_grp = lt_g ).

      " Which parts carry keys: a company code part, a sales part.
      DATA(lv_cgrp) = VALUE ty_grp( ).
      DATA(lv_sgrp) = VALUE ty_grp( ).
      LOOP AT lt_fld INTO ls_f.
        DATA(lv_r) = lcl_cat=>root_of( ls_f-grp ).
        IF lv_r = 'COMP'.
          lv_cgrp = 'COMP'.
        ELSEIF lv_r = 'SALE' OR lv_r = 'PURC'.
          lv_sgrp = lv_r.
        ENDIF.
      ENDLOOP.

      LOOP AT lt_nums INTO DATA(lv_n).
        " The entries one row stands for.
        TYPES: BEGIN OF ty_slot,
                 ckey TYPE string,   " company code key
                 skey TYPE string,   " sales / purchasing key
                 rkey TYPE string,   " repeating entry key
               END OF ty_slot.
        DATA lt_slot TYPE STANDARD TABLE OF ty_slot WITH EMPTY KEY.
        CLEAR lt_slot.
        IF lv_rep IS NOT INITIAL.
          DATA(lv_pn) = lines( lcl_cat=>key_names( lcl_cat=>grp( lv_rep )-parent ) ).
          LOOP AT lo_db->records( iv_grp = lv_rep iv_num = lv_n ) INTO DATA(ls_e).
            IF in_scope( iv_grp = lv_rep iv_key = ls_e-key ) = abap_false.
              CONTINUE.
            ENDIF.
            DATA(lv_pk) = COND string( WHEN lv_pn > 0 THEN prefix( iv_key = ls_e-key iv_n = lv_pn ) ).
            APPEND VALUE #( ckey = COND #( WHEN lv_cgrp IS NOT INITIAL THEN lv_pk )
                            skey = COND #( WHEN lv_sgrp IS NOT INITIAL THEN lv_pk )
                            rkey = ls_e-key ) TO lt_slot.
          ENDLOOP.
        ELSE.
          DATA lt_ck TYPE string_table.
          DATA lt_sk TYPE string_table.
          CLEAR: lt_ck, lt_sk.
          IF lv_cgrp IS NOT INITIAL.
            LOOP AT lo_db->records( iv_grp = lv_cgrp iv_num = lv_n ) INTO ls_e.
              IF in_scope( iv_grp = lv_cgrp iv_key = ls_e-key ) = abap_true.
                APPEND ls_e-key TO lt_ck.
              ENDIF.
            ENDLOOP.
          ENDIF.
          IF lv_sgrp IS NOT INITIAL.
            LOOP AT lo_db->records( iv_grp = lv_sgrp iv_num = lv_n ) INTO ls_e.
              IF in_scope( iv_grp = lv_sgrp iv_key = ls_e-key ) = abap_true.
                APPEND ls_e-key TO lt_sk.
              ENDIF.
            ENDLOOP.
          ENDIF.
          IF lt_ck IS INITIAL.
            APPEND `` TO lt_ck.
          ENDIF.
          IF lt_sk IS INITIAL.
            APPEND `` TO lt_sk.
          ENDIF.
          LOOP AT lt_ck INTO DATA(lv_ck).
            LOOP AT lt_sk INTO DATA(lv_sk).
              APPEND VALUE #( ckey = lv_ck skey = lv_sk ) TO lt_slot.
            ENDLOOP.
          ENDLOOP.
        ENDIF.
        IF lt_slot IS INITIAL.
          APPEND INITIAL LINE TO lt_slot.
        ENDIF.

        LOOP AT lt_slot INTO DATA(ls_slot).
          DATA ls_row TYPE ty_row.
          CLEAR ls_row.
          CLEAR lv_ix.
          LOOP AT lt_fld INTO ls_f.
            lv_ix = lv_ix + 1.
            DATA lv_cell TYPE string.
            CLEAR lv_cell.
            IF ls_f-name = lv_num.
              " A new record gets its number when it is created.
              lv_cell = COND #( WHEN p_crt = abap_false THEN lcl_val=>to_ext( is_f = ls_f iv_val = CONV @@NUMTYPE@@( lv_n ) ) ).
            ELSE.
              DATA(lv_g2) = lcl_cat=>root_of( ls_f-grp ).
              DATA(lv_key) = COND string(
                WHEN lcl_cat=>grp( ls_f-grp )-rep = abap_true THEN ls_slot-rkey
                WHEN lv_g2 = 'COMP'                           THEN ls_slot-ckey
                WHEN lv_g2 = 'SALE' OR lv_g2 = 'PURC'         THEN ls_slot-skey
                ELSE `` ).
              DATA(lr_v) = lo_db->value( is_f = ls_f iv_num = lv_n iv_key = lv_key ).
              IF lr_v IS BOUND.
                ASSIGN lr_v->* TO FIELD-SYMBOL(<lv_v>).
                lv_cell = lcl_val=>to_ext( is_f = ls_f iv_val = <lv_v> ).
              ENDIF.
            ENDIF.
            APPEND lv_cell TO ls_row-cells.
            IF lv_act = abap_true AND lv_ix = lv_last_key.
              APPEND `` TO ls_row-cells.
            ENDIF.
          ENDLOOP.
          ls_row-row = lines( lt_rows ) + 2.
          APPEND ls_row TO lt_rows.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    TRY.
        lcl_file=>write( it_head = lt_head it_rows = lt_rows ).
      CATCH lcx_dl INTO DATA(lx).
        DATA(lv_xt) = lx->get_text( ).
        MESSAGE lv_xt TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.
    IF lt_rows IS INITIAL.
      MESSAGE |Template with { lines( lt_head ) } columns written to { p_file }| TYPE 'S'.
    ELSE.
      MESSAGE |{ lines( lt_rows ) } rows written to { p_file }| TYPE 'S'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

