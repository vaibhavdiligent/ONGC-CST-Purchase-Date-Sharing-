*----------------------------------------------------------------------*
* LCL_DB - what SAP holds today
*   Every record of every part asked for, read for all numbers at once
*   (in packages), and kept under the key the file uses: the parent's keys
*   followed by the part's own - a partner function of sales area
*   0801/Z1/Z0 is found under "0801|Z1|Z0|ZR|000".
*----------------------------------------------------------------------*
CLASS lcl_db DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_rec,
             grp TYPE ty_grp,
             num TYPE string,
             key TYPE string,
             rec TYPE REF TO data,
           END OF ty_rec,
           tt_rec TYPE SORTED TABLE OF ty_rec WITH NON-UNIQUE KEY grp num key.

    " IT_NUM: internal numbers (with their leading zeros).
    METHODS constructor
      IMPORTING it_num TYPE string_table
                it_grp TYPE string_table.
    METHODS records
      IMPORTING iv_grp    TYPE ty_grp
                iv_num    TYPE string
      RETURNING VALUE(rt) TYPE tt_rec.
    METHODS rec
      IMPORTING iv_grp    TYPE ty_grp
                iv_num    TYPE string
                iv_key    TYPE string
      RETURNING VALUE(rr) TYPE REF TO data.
    " The current value of a field, typed as the field. Unbound when the
    " record does not exist.
    METHODS value
      IMPORTING is_f      TYPE ty_fld
                iv_num    TYPE string
                iv_key    TYPE string
      RETURNING VALUE(rr) TYPE REF TO data.
    " The consecutive number of the communication entry a value lives in -
    " a change has to name it.
    METHODS consnumber
      IMPORTING is_f      TYPE ty_fld
                iv_num    TYPE string
      RETURNING VALUE(rv) TYPE ad_consnum.
    " The key a record is filed under, built from the record itself.
    CLASS-METHODS key_of
      IMPORTING iv_grp    TYPE ty_grp
                is_rec    TYPE any
      RETURNING VALUE(rv) TYPE string.
    " Text name (TDNAME) of a text object for a number and a key.
    CLASS-METHODS tdname
      IMPORTING iv_obj    TYPE tdobject
                iv_num    TYPE string
                iv_key    TYPE string
      RETURNING VALUE(rv) TYPE tdobname.
  PRIVATE SECTION.
    DATA mt_rec TYPE tt_rec.
    TYPES: BEGIN OF ty_txt,
             obj  TYPE tdobject,
             name TYPE tdobname,
             id   TYPE tdid,
             text TYPE string,
           END OF ty_txt.
    DATA mt_txt TYPE HASHED TABLE OF ty_txt WITH UNIQUE KEY obj name id.
    METHODS load_tab
      IMPORTING is_g   TYPE ty_gdef
                it_num TYPE string_table.
    METHODS load_addr
      IMPORTING it_num TYPE string_table.
    METHODS put
      IMPORTING iv_grp TYPE ty_grp
                iv_num TYPE string
                iv_key TYPE string
                ir_rec TYPE REF TO data.
    METHODS text
      IMPORTING iv_obj    TYPE tdobject
                iv_name   TYPE tdobname
                iv_id     TYPE tdid
      RETURNING VALUE(rv) TYPE string.
ENDCLASS.


CLASS lcl_db IMPLEMENTATION.

  METHOD constructor.
    IF it_num IS INITIAL.
      RETURN.
    ENDIF.
    LOOP AT lcl_cat=>groups( ) INTO DATA(ls_g).
      IF NOT line_exists( it_grp[ table_line = CONV string( ls_g-grp ) ] ).
        CONTINUE.
      ENDIF.
      CASE ls_g-grp.
        WHEN 'ADDR' OR 'COMM'.
          " Read together, below.
        WHEN 'TXTC' OR 'TXTB' OR 'TXTS'.
          " Read when asked for.
        WHEN OTHERS.
          load_tab( is_g = ls_g it_num = it_num ).
      ENDCASE.
    ENDLOOP.
    IF line_exists( it_grp[ table_line = `ADDR` ] ) OR line_exists( it_grp[ table_line = `COMM` ] ).
      load_addr( it_num ).
    ENDIF.
  ENDMETHOD.

  METHOD put.
    INSERT VALUE #( grp = iv_grp num = iv_num key = iv_key rec = ir_rec ) INTO TABLE mt_rec.
  ENDMETHOD.

  METHOD key_of.
    DATA(lt_k) = lcl_cat=>key_names( iv_grp ).
    DATA lv_n TYPE i.
    LOOP AT lt_k INTO DATA(lv_k).
      lv_n = lv_n + 1.
      DATA(lv_fld) = substring_after( val = lv_k sub = `-` ).
      ASSIGN COMPONENT lv_fld OF STRUCTURE is_rec TO FIELD-SYMBOL(<lv>).
      DATA(lv_v) = COND string( WHEN sy-subrc = 0 THEN condense( CONV string( <lv> ) ) ).
      rv = COND #( WHEN lv_n = 1 THEN lv_v ELSE |{ rv }\|{ lv_v }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD load_tab.
    " Read in packages of numbers: FOR ALL ENTRIES with a few thousand
    " entries is split by the database interface anyway, and a package
    " keeps one statement from holding too much in memory.
    CONSTANTS lc_pack TYPE i VALUE 500.
    DATA lr_tab TYPE REF TO data.
    FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.
    DATA lt_pack TYPE STANDARD TABLE OF string WITH EMPTY KEY.
    DATA(lv_cond) = |{ is_g-numfld } = @LT_PACK-TABLE_LINE|.

    DATA lv_from TYPE i.
    DATA lv_to   TYPE i.
    DATA(lv_i) = 0.
    WHILE lv_i < lines( it_num ).
      CLEAR lt_pack.
      lv_from = lv_i + 1.
      lv_to   = lv_i + lc_pack.
      LOOP AT it_num INTO DATA(lv_n) FROM lv_from TO lv_to.
        APPEND lv_n TO lt_pack.
      ENDLOOP.
      lv_i = lv_to.

      CREATE DATA lr_tab TYPE STANDARD TABLE OF (is_g-tab).
      ASSIGN lr_tab->* TO <lt_tab>.
      TRY.
          SELECT * FROM (is_g-tab)
            FOR ALL ENTRIES IN @lt_pack
            WHERE (lv_cond)
            INTO TABLE @<lt_tab>.
        CATCH cx_sy_dynamic_osql_error.
          RETURN.
      ENDTRY.
      LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_r>).
        ASSIGN COMPONENT is_g-numfld OF STRUCTURE <ls_r> TO FIELD-SYMBOL(<lv_num>).
        IF sy-subrc <> 0 OR <lv_num> IS INITIAL.
          CONTINUE.
        ENDIF.
        DATA lr_r TYPE REF TO data.
        CREATE DATA lr_r LIKE <ls_r>.
        ASSIGN lr_r->* TO FIELD-SYMBOL(<ls_copy>).
        <ls_copy> = <ls_r>.
        put( iv_grp = is_g-grp iv_num = CONV string( <lv_num> )
             iv_key = key_of( iv_grp = is_g-grp is_rec = <ls_r> ) ir_rec = lr_r ).
      ENDLOOP.
    ENDWHILE.
  ENDMETHOD.

  METHOD load_addr.
    " The address number comes from the general data. Read it here when the
    " general data itself was not asked for.
    TYPES: BEGIN OF ty_an,
             num   TYPE string,
             adrnr TYPE ad_addrnum,
           END OF ty_an.
    DATA lt_an TYPE STANDARD TABLE OF ty_an WITH EMPTY KEY.
    DATA lt_nr TYPE STANDARD TABLE OF ad_addrnum WITH EMPTY KEY.
    DATA lt_k  TYPE STANDARD TABLE OF @@NUMTYPE@@ WITH EMPTY KEY.
    LOOP AT it_num INTO DATA(lv_n).
      APPEND CONV @@NUMTYPE@@( lv_n ) TO lt_k.
    ENDLOOP.
    IF gc_side = 'C'.
      SELECT kunnr AS num, adrnr FROM kna1
        FOR ALL ENTRIES IN @lt_k WHERE kunnr = @lt_k-table_line
        INTO CORRESPONDING FIELDS OF TABLE @lt_an.
    ELSE.
      SELECT lifnr AS num, adrnr FROM lfa1
        FOR ALL ENTRIES IN @lt_k WHERE lifnr = @lt_k-table_line
        INTO CORRESPONDING FIELDS OF TABLE @lt_an.
    ENDIF.
    DELETE lt_an WHERE adrnr IS INITIAL.
    IF lt_an IS INITIAL.
      RETURN.
    ENDIF.
    LOOP AT lt_an INTO DATA(ls_an).
      APPEND ls_an-adrnr TO lt_nr.
    ENDLOOP.

    SELECT * FROM adrc FOR ALL ENTRIES IN @lt_nr
      WHERE addrnumber = @lt_nr-table_line AND nation = @space
      INTO TABLE @DATA(lt_adrc).
    SELECT * FROM adr2 FOR ALL ENTRIES IN @lt_nr
      WHERE addrnumber = @lt_nr-table_line AND persnumber = @space
      INTO TABLE @DATA(lt_adr2).
    SELECT * FROM adr3 FOR ALL ENTRIES IN @lt_nr
      WHERE addrnumber = @lt_nr-table_line AND persnumber = @space
      INTO TABLE @DATA(lt_adr3).
    SELECT * FROM adr6 FOR ALL ENTRIES IN @lt_nr
      WHERE addrnumber = @lt_nr-table_line AND persnumber = @space
      INTO TABLE @DATA(lt_adr6).
    " The standard entry first, so the first one found is the one shown.
    SORT lt_adr2 BY addrnumber flgdefault DESCENDING consnumber.
    SORT lt_adr3 BY addrnumber flgdefault DESCENDING consnumber.
    SORT lt_adr6 BY addrnumber flgdefault DESCENDING consnumber.

    LOOP AT lt_an INTO ls_an.
      " The address valid today.
      LOOP AT lt_adrc INTO DATA(ls_adrc) WHERE addrnumber = ls_an-adrnr
                                           AND date_from <= sy-datum
                                           AND date_to   >= sy-datum.
        put( iv_grp = 'ADDR' iv_num = ls_an-num iv_key = `` ir_rec = NEW adrc( ls_adrc ) ).
        EXIT.
      ENDLOOP.
      " Landline: R_3_USER space or 1. Mobile: 2 or 3.
      LOOP AT lt_adr2 INTO DATA(ls_2) WHERE addrnumber = ls_an-adrnr AND ( r3_user = space OR r3_user = '1' ).
        put( iv_grp = 'COMM' iv_num = ls_an-num iv_key = `ADR2` ir_rec = NEW adr2( ls_2 ) ).
        EXIT.
      ENDLOOP.
      LOOP AT lt_adr2 INTO ls_2 WHERE addrnumber = ls_an-adrnr AND ( r3_user = '2' OR r3_user = '3' ).
        put( iv_grp = 'COMM' iv_num = ls_an-num iv_key = `MOB` ir_rec = NEW adr2( ls_2 ) ).
        EXIT.
      ENDLOOP.
      LOOP AT lt_adr3 INTO DATA(ls_3) WHERE addrnumber = ls_an-adrnr.
        put( iv_grp = 'COMM' iv_num = ls_an-num iv_key = `ADR3` ir_rec = NEW adr3( ls_3 ) ).
        EXIT.
      ENDLOOP.
      LOOP AT lt_adr6 INTO DATA(ls_6) WHERE addrnumber = ls_an-adrnr.
        put( iv_grp = 'COMM' iv_num = ls_an-num iv_key = `ADR6` ir_rec = NEW adr6( ls_6 ) ).
        EXIT.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD records.
    LOOP AT mt_rec INTO DATA(ls_r) WHERE grp = iv_grp AND num = iv_num.
      INSERT ls_r INTO TABLE rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD rec.
    READ TABLE mt_rec INTO DATA(ls_r) WITH KEY grp = iv_grp num = iv_num key = iv_key.
    IF sy-subrc = 0.
      rr = ls_r-rec.
    ENDIF.
  ENDMETHOD.

  METHOD tdname.
    " KNA1: the number. KNB1: number + company code. KNVV: number + sales
    " area. The same for the supplier objects.
    DATA(lv_num) = iv_num.
    IF strlen( lv_num ) < 10.
      lv_num = |{ lv_num WIDTH = 10 }|.
    ENDIF.
    DATA(lv_key) = iv_key.
    REPLACE ALL OCCURRENCES OF `|` IN lv_key WITH ``.
    CASE iv_obj.
      WHEN 'KNA1' OR 'LFA1'.
        rv = lv_num.
      WHEN OTHERS.
        rv = |{ lv_num }{ lv_key }|.
    ENDCASE.
  ENDMETHOD.

  METHOD text.
    READ TABLE mt_txt INTO DATA(ls_t) WITH TABLE KEY obj = iv_obj name = iv_name id = iv_id.
    IF sy-subrc = 0.
      rv = ls_t-text.
      RETURN.
    ENDIF.
    DATA lt_line TYPE STANDARD TABLE OF tline WITH EMPTY KEY.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING  id       = iv_id
                 language = p_tlang
                 name     = iv_name
                 object   = iv_obj
      TABLES     lines    = lt_line
      EXCEPTIONS OTHERS   = 1.
    IF sy-subrc = 0.
      LOOP AT lt_line INTO DATA(ls_l).
        rv = COND #( WHEN sy-tabix = 1 THEN CONV string( ls_l-tdline )
                     ELSE |{ rv }{ cl_abap_char_utilities=>newline }{ ls_l-tdline }| ).
      ENDLOOP.
    ENDIF.
    INSERT VALUE #( obj = iv_obj name = iv_name id = iv_id text = rv ) INTO TABLE mt_txt.
  ENDMETHOD.

  METHOD value.
    IF is_f-grp = 'TXTC' OR is_f-grp = 'TXTB' OR is_f-grp = 'TXTS'.
      rr = NEW string( text( iv_obj  = is_f-tdobj
                             iv_name = tdname( iv_obj = is_f-tdobj iv_num = iv_num iv_key = iv_key )
                             iv_id   = is_f-tdid ) ).
      RETURN.
    ENDIF.

    DATA lr_rec TYPE REF TO data.
    DATA lv_fld TYPE fieldname.
    lv_fld = is_f-fld.
    CASE is_f-grp.
      WHEN 'COMM'.
        DATA(lv_ck) = COND string( WHEN is_f-api = 'MOBILE' THEN `MOB` ELSE CONV string( is_f-tab ) ).
        lr_rec = rec( iv_grp = 'COMM' iv_num = iv_num iv_key = lv_ck ).
      WHEN OTHERS.
        lr_rec = rec( iv_grp = is_f-grp iv_num = iv_num iv_key = iv_key ).
    ENDCASE.
    IF lr_rec IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN lr_rec->* TO FIELD-SYMBOL(<ls_rec>).

    rr = lcl_val=>new_data( is_f ).
    ASSIGN rr->* TO FIELD-SYMBOL(<lv_out>).

    " The customer's partner function holds its partner in the field its
    " partner type uses.
    IF is_f-name = `KNVP-PARTNER`.
      DATA(lt_pf) = VALUE string_table( ( `KUNN2` ) ( `LIFNR` ) ( `PERNR` ) ( `PARNR` ) ).
      LOOP AT lt_pf INTO DATA(lv_pf).
        ASSIGN COMPONENT lv_pf OF STRUCTURE <ls_rec> TO FIELD-SYMBOL(<lv_pv>).
        IF sy-subrc = 0 AND <lv_pv> IS NOT INITIAL.
          <lv_out> = <lv_pv>.
          RETURN.
        ENDIF.
      ENDLOOP.
      RETURN.
    ENDIF.

    ASSIGN COMPONENT lv_fld OF STRUCTURE <ls_rec> TO FIELD-SYMBOL(<lv_in>).
    IF sy-subrc = 0.
      <lv_out> = <lv_in>.
    ENDIF.
  ENDMETHOD.

  METHOD consnumber.
    DATA(lv_ck) = COND string( WHEN is_f-api = 'MOBILE' THEN `MOB` ELSE CONV string( is_f-tab ) ).
    DATA(lr_rec) = rec( iv_grp = 'COMM' iv_num = iv_num iv_key = lv_ck ).
    IF lr_rec IS NOT BOUND.
      RETURN.
    ENDIF.
    ASSIGN lr_rec->* TO FIELD-SYMBOL(<ls_rec>).
    ASSIGN COMPONENT 'CONSNUMBER' OF STRUCTURE <ls_rec> TO FIELD-SYMBOL(<lv_c>).
    IF sy-subrc = 0.
      rv = <lv_c>.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* LCL_FILE - the file on the PC: .xlsx, or text separated by tabs
*----------------------------------------------------------------------*
CLASS lcl_file DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS f4.
    CLASS-METHODS write
      IMPORTING it_head TYPE tt_cell
                it_rows TYPE tt_row
      RAISING   lcx_dl.
    CLASS-METHODS read
      EXPORTING et_head TYPE tt_cell
                et_rows TYPE tt_row
      RAISING   lcx_dl.
  PRIVATE SECTION.
    CLASS-METHODS save_bin
      IMPORTING iv_x TYPE xstring
      RAISING   lcx_dl.
    CLASS-METHODS load_bin
      RETURNING VALUE(rv) TYPE xstring
      RAISING   lcx_dl.
    CLASS-METHODS decode
      IMPORTING iv_x      TYPE xstring
      RETURNING VALUE(rv) TYPE string
      RAISING   lcx_dl.
ENDCLASS.


CLASS lcl_file IMPLEMENTATION.

  METHOD f4.
    DATA(lv_filter) = COND string( WHEN p_txt = abap_true
                                   THEN `Text, tab-separated (*.txt)|*.txt|`
                                   ELSE `Excel workbook (*.xlsx)|*.xlsx|` ).
    IF p_up = abap_true.
      DATA lt_ft TYPE filetable.
      DATA lv_rc TYPE i.
      DATA lv_ua TYPE i.
      cl_gui_frontend_services=>file_open_dialog(
        EXPORTING  window_title = 'Select the file to upload'
                   file_filter  = lv_filter
        CHANGING   file_table   = lt_ft
                   rc           = lv_rc
                   user_action  = lv_ua
        EXCEPTIONS OTHERS       = 1 ).
      IF sy-subrc = 0 AND lv_ua = cl_gui_frontend_services=>action_ok AND lv_rc >= 1.
        DATA(lv_pick) = lt_ft[ 1 ]-filename.
        IF strlen( lv_pick ) > 255.
          MESSAGE 'That path is longer than 255 characters - move the file to a shorter path'
                  TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          p_file = lv_pick.
        ENDIF.
      ENDIF.
      RETURN.
    ENDIF.

    DATA: lv_name TYPE string,
          lv_path TYPE string,
          lv_full TYPE string.
    DATA(lv_ext) = COND string( WHEN p_txt = abap_true THEN `txt` ELSE `xlsx` ).
    DATA(lv_def) = |@@PROGU@@_{ sy-datum }.{ lv_ext }|.
    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING  window_title      = 'Save the file'
                 default_extension = lv_ext
                 default_file_name = lv_def
                 file_filter       = lv_filter
      CHANGING   filename          = lv_name
                 path              = lv_path
                 fullpath          = lv_full
      EXCEPTIONS OTHERS            = 1 ).
    IF sy-subrc = 0 AND lv_full IS NOT INITIAL.
      IF strlen( lv_full ) > 255.
        MESSAGE 'That path is longer than 255 characters - pick a shorter folder'
                TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        p_file = lv_full.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD save_bin.
    DATA(lt_bin) = cl_bcs_convert=>xstring_to_solix( iv_x ).
    DATA(lv_len) = xstrlen( iv_x ).
    DATA(lv_file) = CONV string( p_file ).
    cl_gui_frontend_services=>gui_download(
      EXPORTING  bin_filesize = lv_len
                 filename     = lv_file
                 filetype     = 'BIN'
      CHANGING   data_tab     = lt_bin
      EXCEPTIONS OTHERS       = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW lcx_dl( |The file { p_file } could not be written - is it open in Excel?| ).
    ENDIF.
  ENDMETHOD.

  METHOD load_bin.
    DATA lt_bin TYPE solix_tab.
    DATA lv_len TYPE i.
    DATA(lv_file) = CONV string( p_file ).
    cl_gui_frontend_services=>gui_upload(
      EXPORTING  filename   = lv_file
                 filetype   = 'BIN'
      IMPORTING  filelength = lv_len
      CHANGING   data_tab   = lt_bin
      EXCEPTIONS OTHERS     = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW lcx_dl( |The file { p_file } could not be read| ).
    ENDIF.
    rv = cl_bcs_convert=>solix_to_xstring( it_solix = lt_bin iv_size = lv_len ).
  ENDMETHOD.

  METHOD decode.
    " A byte order mark says how the text is encoded. Without one, UTF-8 is
    " tried first and then Shift-JIS, the usual encoding of a Japanese
    " Windows text file.
    DATA lv_cp TYPE string.
    DATA lv_x  TYPE xstring.
    lv_x = iv_x.
    IF xstrlen( lv_x ) >= 3 AND lv_x(3) = 'EFBBBF'.
      lv_cp = `UTF-8`.
      lv_x = lv_x+3.
    ELSEIF xstrlen( lv_x ) >= 2 AND lv_x(2) = 'FFFE'.
      lv_cp = `UTF-16LE`.
      lv_x = lv_x+2.
    ELSEIF xstrlen( lv_x ) >= 2 AND lv_x(2) = 'FEFF'.
      lv_cp = `UTF-16BE`.
      lv_x = lv_x+2.
    ENDIF.
    IF lv_cp IS NOT INITIAL.
      TRY.
          rv = cl_abap_conv_codepage=>create_in( codepage = lv_cp )->convert( lv_x ).
          RETURN.
        CATCH cx_root INTO DATA(lx1).
          RAISE EXCEPTION NEW lcx_dl( |The file could not be read as { lv_cp }: { lx1->get_text( ) }| ).
      ENDTRY.
    ENDIF.
    TRY.
        rv = cl_abap_conv_codepage=>create_in( codepage = `UTF-8` )->convert( lv_x ).
      CATCH cx_root.
        TRY.
            rv = cl_abap_conv_codepage=>create_in( codepage = `Shift_JIS` )->convert( lv_x ).
          CATCH cx_root INTO DATA(lx2).
            RAISE EXCEPTION NEW lcx_dl( |The file's encoding is not recognised - save it as | &&
                                        |"Unicode Text" or UTF-8 ({ lx2->get_text( ) })| ).
        ENDTRY.
    ENDTRY.
  ENDMETHOD.

  METHOD write.
    IF p_xlsx = abap_true.
      save_bin( lcl_xlsx=>build( iv_sheet = 'Data' it_head = it_head it_row = it_rows ) ).
      RETURN.
    ENDIF.
    " Text: one line per row, cells separated by tabs. A line break inside
    " a cell - a text of several lines - is written as \n.
    DATA(lv_tab) = cl_abap_char_utilities=>horizontal_tab.
    DATA(lv_nl)  = cl_abap_char_utilities=>cr_lf.
    DATA(lv_out) = concat_lines_of( table = it_head sep = lv_tab ) && lv_nl.
    LOOP AT it_rows INTO DATA(ls_r).
      DATA lt_c TYPE tt_cell.
      lt_c = ls_r-cells.
      LOOP AT lt_c ASSIGNING FIELD-SYMBOL(<lv_c>).
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN <lv_c> WITH `\n`.
        REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN <lv_c> WITH `\n`.
        REPLACE ALL OCCURRENCES OF lv_tab IN <lv_c> WITH ` `.
      ENDLOOP.
      lv_out = lv_out && concat_lines_of( table = lt_c sep = lv_tab ) && lv_nl.
    ENDLOOP.
    DATA lv_x TYPE xstring.
    TRY.
        lv_x = cl_abap_conv_codepage=>create_out( codepage = `UTF-8` )->convert( lv_out ).
      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION NEW lcx_dl( |The text file could not be encoded: { lx->get_text( ) }| ).
    ENDTRY.
    " The byte order mark makes Excel open the file as UTF-8.
    DATA lv_bom TYPE xstring VALUE 'EFBBBF'.
    CONCATENATE lv_bom lv_x INTO lv_x IN BYTE MODE.
    save_bin( lv_x ).
  ENDMETHOD.

  METHOD read.
    CLEAR: et_head, et_rows.
    DATA(lv_low) = to_lower( CONV string( p_file ) ).
    IF lv_low CP '*.xlsx'.
      DATA(lo_x) = NEW lcl_excel( ).
      DATA lv_sheet TYPE string.
      lo_x->read( EXPORTING iv_file    = CONV string( p_file )
                            iv_from_pc = abap_true
                            iv_sheet   = `Data`
                            iv_skip    = 1
                  IMPORTING et_head    = et_head
                            et_row     = et_rows
                            ev_sheet   = lv_sheet ).
      RETURN.
    ENDIF.

    DATA(lv_txt) = decode( load_bin( ) ).
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_txt WITH cl_abap_char_utilities=>newline.
    SPLIT lv_txt AT cl_abap_char_utilities=>newline INTO TABLE DATA(lt_lines).
    LOOP AT lt_lines INTO DATA(lv_line).
      DATA(lv_no) = sy-tabix.
      SPLIT lv_line AT cl_abap_char_utilities=>horizontal_tab INTO TABLE DATA(lt_c).
      LOOP AT lt_c ASSIGNING FIELD-SYMBOL(<lv_c>).
        REPLACE ALL OCCURRENCES OF `\n` IN <lv_c> WITH cl_abap_char_utilities=>newline.
      ENDLOOP.
      IF lv_no = 1.
        et_head = lt_c.
        CONTINUE.
      ENDIF.
      IF condense( lv_line ) IS INITIAL.
        CONTINUE.
      ENDIF.
      APPEND VALUE #( row = lv_no cells = lt_c ) TO et_rows.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* LCL_LOG - the result list
*----------------------------------------------------------------------*
CLASS lcl_log DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS add
      IMPORTING iv_row   TYPE i DEFAULT 0
                iv_obj   TYPE clike OPTIONAL
                iv_part  TYPE clike OPTIONAL
                iv_field TYPE clike OPTIONAL
                iv_old   TYPE clike OPTIONAL
                iv_new   TYPE clike OPTIONAL
                iv_type  TYPE ty_msgty
                iv_msg   TYPE clike.
    METHODS show
      IMPORTING iv_title TYPE clike.
  PRIVATE SECTION.
    DATA mt_log TYPE tt_log.
ENDCLASS.


CLASS lcl_log IMPLEMENTATION.

  METHOD add.
    DATA(lv_icon) = SWITCH icon_d( iv_type
                                   WHEN 'E' THEN icon_red_light
                                   WHEN 'W' THEN icon_yellow_light
                                   WHEN 'S' THEN icon_green_light
                                   ELSE icon_information ).
    APPEND VALUE #( status = lv_icon row = iv_row obj = iv_obj part = iv_part field = iv_field
                    old = iv_old new = iv_new type = iv_type msg = iv_msg ) TO mt_log.
  ENDMETHOD.

  METHOD show.
    IF mt_log IS INITIAL.
      MESSAGE 'Nothing to report' TYPE 'S'.
      RETURN.
    ENDIF.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                                CHANGING  t_table      = mt_log ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->get_columns( )->set_optimize( abap_true ).
        DATA(lo_cols) = lo_alv->get_columns( ).
        DATA(lt_txt) = VALUE string_table( ( `STATUS` ) ( `Status` ) ( `ROW` ) ( `Row` )
                                           ( `OBJ` ) ( `Number` ) ( `PART` ) ( `Part` )
                                           ( `FIELD` ) ( `Field` ) ( `OLD` ) ( `Value in SAP` )
                                           ( `NEW` ) ( `Value in file` ) ( `TYPE` ) ( `Type` )
                                           ( `MSG` ) ( `Message` ) ).
        DATA lv_i TYPE i VALUE 1.
        WHILE lv_i < lines( lt_txt ).
          TRY.
              DATA(lo_c) = lo_cols->get_column( CONV lvc_fname( lt_txt[ lv_i ] ) ).
              DATA(lv_h) = lt_txt[ lv_i + 1 ].
              lo_c->set_long_text( CONV scrtext_l( lv_h ) ).
              lo_c->set_medium_text( CONV scrtext_m( lv_h ) ).
              lo_c->set_short_text( CONV scrtext_s( lv_h ) ).
            CATCH cx_salv_not_found.
              CLEAR lv_h.
          ENDTRY.
          lv_i = lv_i + 2.
        ENDWHILE.
        lo_alv->get_display_settings( )->set_list_header( CONV lvc_title( iv_title ) ).
        lo_alv->display( ).
      CATCH cx_salv_msg INTO DATA(lx).
        MESSAGE lx TYPE 'I'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

