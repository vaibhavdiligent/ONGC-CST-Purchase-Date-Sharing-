*&---------------------------------------------------------------------*
*& Report  ZSDS_CUST_CREDIT_UPLOAD
*&---------------------------------------------------------------------*
*& Title       : Customer Credit Limit Upload (SAP Credit Management)
*& Module      : SD / FIN-FSCM-CR
*&
*& What it does
*&   Reads Cipla's credit limit workbook - the FD32 LSMW layout "Customer
*&   change Credit limit" - and writes it to SAP Credit Management, which
*&   replaces the ECC credit tables KNKK / KNKA in S/4HANA:
*&
*&     KKBER  credit control area   -> credit segment (UKM_KKBER2SGM)
*&     KLIMK  customer's credit limit, or KLIME  ->  credit segment limit
*&     KLIMG  total across all areas            ->  main credit segment
*&     WAERS  currency                          ->  checked against the
*&                                                  segment's currency
*&     CTLPC  risk category                     ->  risk class (credit
*&                                                  profile of the BP)
*&     CRBLB  blocked                           ->  blocked in credit mgmt
*&     SBGRP  credit representative group       ->  see LCL_ROW=>SBGRP
*&     Payment terms, customer group 3, interest indicator (and cycle)
*&                                              ->  customer master, BP API
*&
*&   A cell that is empty, or reads NO CHANGE / TBU, leaves the field as
*&   it is. Everything goes through SAP's own interfaces - CL_UKM_FACADE
*&   for credit data, CL_MD_BP_MAINTAIN for the customer master. No table
*&   is written directly. After a productive run the stored limit is read
*&   back: where a limit change needs approval SAP keeps the old one, and
*&   the row says so instead of claiming success.
*&
*& File
*&   .xlsx (save the .xlsb as Excel Workbook). Row 1 holds the technical
*&   names, row 2 the descriptions, row 3 the field types; data starts on
*&   the first row whose first cell is a customer number. Columns are found
*&   by their names, so an inserted or moved column does not matter.
*&
*& Clean core positioning (ABAP_CLOUD_READINESS)
*&   Classic ABAP: CL_UKM_FACADE and CL_MD_BP_MAINTAIN are not released for
*&   ABAP Cloud and no released local API maintains credit segment data
*&   from a GUI upload (the remote one is OData API_CRDTMBUSINESSPARTNER).
*&   Reads are confined to LCL_CFG.
*&---------------------------------------------------------------------*
REPORT zsds_cust_credit_upload.

TABLES sscrfields.

TYPE-POOLS icon.

TYPES ty_path TYPE c LENGTH 255.
TYPES ty_dec  TYPE p LENGTH 15 DECIMALS 2.

TYPES: BEGIN OF ty_row,
         row   TYPE i,
         cells TYPE string_table,
       END OF ty_row,
       tt_row TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

TYPES: BEGIN OF ty_log,
         status  TYPE icon_d,
         xlsrow  TYPE i,
         kunnr   TYPE kunnr,
         partner TYPE bu_partner,
         sgmnt   TYPE ukm_credit_sgmnt,
         field   TYPE string,
         old     TYPE string,
         new     TYPE string,
         type    TYPE bapi_mtype,
         msg     TYPE string,
       END OF ty_log,
       tt_log TYPE STANDARD TABLE OF ty_log WITH EMPTY KEY.

CONSTANTS:
  gc_u     TYPE cmd_ei_object_task VALUE 'U',
  " The main credit segment: where the old "total limit across all credit
  " control areas" (KNKA-KLIMG) lives in SAP Credit Management.
  gc_main  TYPE ukm_credit_sgmnt   VALUE '0000'.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS p_file TYPE ty_path LOWER CASE.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS p_test AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Exception
*----------------------------------------------------------------------*
CLASS lcx_up DEFINITION INHERITING FROM cx_static_check FINAL.
  PUBLIC SECTION.
    DATA text TYPE string.
    METHODS constructor IMPORTING iv_text TYPE string.
    METHODS get_text REDEFINITION.
ENDCLASS.

CLASS lcx_up IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    text = iv_text.
  ENDMETHOD.
  METHOD get_text.
    result = text.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* LCL_UTIL
*----------------------------------------------------------------------*
CLASS lcl_util DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS cell
      IMPORTING is_row    TYPE ty_row
                iv_col    TYPE i
      RETURNING VALUE(rv) TYPE string.
    " Letters and digits only, upper case - "Payment Terms" -> PAYMENTTERMS.
    CLASS-METHODS squash
      IMPORTING iv_in     TYPE clike
      RETURNING VALUE(rv) TYPE string.
    " A cell that asks for nothing to be changed: empty, NO CHANGE, TBU.
    CLASS-METHODS no_change
      IMPORTING iv_in     TYPE string
      RETURNING VALUE(rv) TYPE abap_bool.
    " An amount. EV_OK is false for a cell that is not a number (the KLIMG
    " column of the template holds X on every row).
    CLASS-METHODS to_dec
      IMPORTING iv_in  TYPE string
      EXPORTING ev_dec TYPE ty_dec
                ev_ok  TYPE abap_bool.
ENDCLASS.

CLASS lcl_util IMPLEMENTATION.

  METHOD cell.
    READ TABLE is_row-cells INTO rv INDEX iv_col.
    IF sy-subrc <> 0.
      CLEAR rv.
    ENDIF.
    rv = condense( rv ).
  ENDMETHOD.

  METHOD squash.
    rv = to_upper( CONV string( iv_in ) ).
    REPLACE ALL OCCURRENCES OF PCRE '[^A-Z0-9]' IN rv WITH ``.
  ENDMETHOD.

  METHOD no_change.
    DATA(lv_k) = squash( iv_in ).
    rv = xsdbool( lv_k IS INITIAL OR lv_k = 'NOCHANGE' OR lv_k = 'TBU' ).
  ENDMETHOD.

  METHOD to_dec.
    CLEAR: ev_dec, ev_ok.
    DATA(lv) = condense( iv_in ).
    " Indian grouping and a currency sign are written, not meant.
    REPLACE ALL OCCURRENCES OF `,` IN lv WITH ``.
    REPLACE ALL OCCURRENCES OF ` ` IN lv WITH ``.
    IF lv IS INITIAL OR lv CN '0123456789.-E+e'.
      RETURN.
    ENDIF.
    TRY.
        DATA lv_f TYPE decfloat34.
        lv_f = lv.
        ev_dec = round( val = lv_f dec = 2 ).
        ev_ok  = abap_true.
      CATCH cx_sy_conversion_error cx_sy_arithmetic_error.
        CLEAR ev_dec.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_LOG
*----------------------------------------------------------------------*
CLASS lcl_log DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS add
      IMPORTING iv_row     TYPE i OPTIONAL
                iv_kunnr   TYPE kunnr OPTIONAL
                iv_partner TYPE bu_partner OPTIONAL
                iv_sgmnt   TYPE ukm_credit_sgmnt OPTIONAL
                iv_field   TYPE clike OPTIONAL
                iv_old     TYPE clike OPTIONAL
                iv_new     TYPE clike OPTIONAL
                iv_type    TYPE bapi_mtype
                iv_msg     TYPE clike.
    METHODS has_error
      IMPORTING iv_row    TYPE i
      RETURNING VALUE(rv) TYPE abap_bool.
    " Rows done, rows with an error, rows passed over with nothing done.
    " Lines written against row 0 are about the run, not about a row.
    METHODS counts
      EXPORTING ev_ok   TYPE i
                ev_err  TYPE i
                ev_skip TYPE i.
    METHODS show IMPORTING iv_title TYPE string.
  PRIVATE SECTION.
    DATA mt_log TYPE tt_log.
ENDCLASS.

CLASS lcl_log IMPLEMENTATION.

  METHOD add.
    " A green light is an outcome and nothing else; a remark made on the way
    " gets the information icon.
    DATA lv_icon TYPE icon_d.
    lv_icon = COND #( WHEN iv_type = 'E' OR iv_type = 'A' THEN icon_red_light
                      WHEN iv_type = 'W'                  THEN icon_yellow_light
                      WHEN iv_type = 'S'                  THEN icon_green_light
                      ELSE                                     icon_information )
    .
    APPEND VALUE #( status  = lv_icon
                    xlsrow  = iv_row
                    kunnr   = iv_kunnr
                    partner = iv_partner
                    sgmnt   = iv_sgmnt
                    field   = iv_field
                    old     = iv_old
                    new     = iv_new
                    type    = iv_type
                    msg     = iv_msg ) TO mt_log.
  ENDMETHOD.

  METHOD has_error.
    rv = xsdbool( line_exists( mt_log[ xlsrow = iv_row type = 'E' ] )
               OR line_exists( mt_log[ xlsrow = iv_row type = 'A' ] ) ).
  ENDMETHOD.

  METHOD counts.
    CLEAR: ev_ok, ev_err, ev_skip.
    DATA lt_rows TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    LOOP AT mt_log INTO DATA(ls_l) WHERE xlsrow > 0.
      INSERT ls_l-xlsrow INTO TABLE lt_rows.
    ENDLOOP.
    LOOP AT lt_rows INTO DATA(lv_r).
      IF has_error( lv_r ) = abap_true.
        ev_err = ev_err + 1.
      ELSEIF line_exists( mt_log[ xlsrow = lv_r type = 'S' ] ).
        ev_ok = ev_ok + 1.
      ELSE.
        ev_skip = ev_skip + 1.
      ENDIF.
    ENDLOOP.
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
        DATA(lo_cols) = lo_alv->get_columns( ).
        lo_cols->set_optimize( abap_true ).
        DATA(lt_txt) = VALUE string_table(
          ( `STATUS` ) ( `Status` ) ( `XLSROW` ) ( `Row` ) ( `KUNNR` ) ( `Customer` )
          ( `PARTNER` ) ( `Business partner` ) ( `SGMNT` ) ( `Credit segment` )
          ( `FIELD` ) ( `Field` ) ( `OLD` ) ( `Value in SAP` ) ( `NEW` ) ( `Value in file` )
          ( `TYPE` ) ( `Type` ) ( `MSG` ) ( `Message` ) ).
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

*----------------------------------------------------------------------*
* LCL_CFG - every read of the database
*----------------------------------------------------------------------*
CLASS lcl_cfg DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES tt_bukrs TYPE STANDARD TABLE OF bukrs WITH EMPTY KEY.
    TYPES: BEGIN OF ty_sarea,
             vkorg TYPE vkorg,
             vtweg TYPE vtweg,
             spart TYPE spart,
           END OF ty_sarea,
           tt_sarea TYPE STANDARD TABLE OF ty_sarea WITH EMPTY KEY.

    CLASS-METHODS get RETURNING VALUE(ro) TYPE REF TO lcl_cfg.

    " The customer a cell names: a customer number, or the number of the
    " business partner behind one.
    METHODS customer_of
      IMPORTING iv_in     TYPE string
      RETURNING VALUE(rv) TYPE kunnr.
    METHODS bp_of      IMPORTING VALUE(iv_kunnr) TYPE kunnr
                       RETURNING VALUE(rv)       TYPE bu_partner.
    METHODS guid_of    IMPORTING VALUE(iv_kunnr) TYPE kunnr
                       RETURNING VALUE(rv)       TYPE bu_partner_guid.

    " Credit control area -> credit segment, and the segment's currency.
    METHODS segment_of   IMPORTING VALUE(iv_kkber) TYPE kkber
                         RETURNING VALUE(rv)       TYPE ukm_credit_sgmnt.
    METHODS segment_curr IMPORTING VALUE(iv_sgmnt) TYPE ukm_credit_sgmnt
                         RETURNING VALUE(rv)       TYPE waers.
    " What SAP holds for a partner and segment today.
    METHODS stored_limit
      IMPORTING VALUE(iv_partner) TYPE bu_partner
                VALUE(iv_sgmnt)   TYPE ukm_credit_sgmnt
      EXPORTING ev_limit          TYPE ty_dec
                ev_block          TYPE abap_bool
                ev_found          TYPE abap_bool
                ev_requested      TYPE ty_dec.
    METHODS stored_risk
      IMPORTING VALUE(iv_partner) TYPE bu_partner
      RETURNING VALUE(rv)         TYPE string.

    " A value against the check table of TAB-FLD as the dictionary names it
    " - risk class, payment terms, customer group 3, interest indicator. No
    " check table, or one the dynamic read cannot use: accepted.
    METHODS value_ok
      IMPORTING iv_tab    TYPE clike
                iv_fld    TYPE clike
                iv_val    TYPE string
      RETURNING VALUE(rv) TYPE abap_bool.

    METHODS kkber_bukrs IMPORTING VALUE(iv_kkber) TYPE kkber
                        RETURNING VALUE(rt)       TYPE tt_bukrs.
    METHODS cust_bukrs  IMPORTING VALUE(iv_kunnr) TYPE kunnr
                        RETURNING VALUE(rt)       TYPE tt_bukrs.
    METHODS cust_sales  IMPORTING VALUE(iv_kunnr) TYPE kunnr
                        RETURNING VALUE(rt)       TYPE tt_sarea.
  PRIVATE SECTION.
    CLASS-DATA mo TYPE REF TO lcl_cfg.
ENDCLASS.

CLASS lcl_cfg IMPLEMENTATION.

  METHOD get.
    IF mo IS INITIAL.
      mo = NEW lcl_cfg( ).
    ENDIF.
    ro = mo.
  ENDMETHOD.

  METHOD customer_of.
    DATA: lv_n  TYPE kunnr,
          lv_10 TYPE c LENGTH 10.
    IF iv_in IS INITIAL OR iv_in CN '0123456789' OR strlen( iv_in ) > 10.
      RETURN.
    ENDIF.
    " A classic function module takes no STRING - the cell goes through a
    " character field first.
    lv_10 = iv_in.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = lv_10
      IMPORTING output = lv_n.
    SELECT SINGLE kunnr FROM kna1 WHERE kunnr = @lv_n INTO @rv.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.
    " Not a customer number - perhaps the business partner's.
    DATA lv_bp TYPE bu_partner.
    lv_bp = lv_n.
    SELECT SINGLE l~customer FROM but000 AS b
      INNER JOIN cvi_cust_link AS l ON l~partner_guid = b~partner_guid
      WHERE b~partner = @lv_bp INTO @rv.
  ENDMETHOD.

  METHOD bp_of.
    SELECT SINGLE b~partner FROM cvi_cust_link AS l
      INNER JOIN but000 AS b ON b~partner_guid = l~partner_guid
      WHERE l~customer = @iv_kunnr INTO @rv.
  ENDMETHOD.

  METHOD guid_of.
    SELECT SINGLE partner_guid FROM cvi_cust_link WHERE customer = @iv_kunnr INTO @rv.
  ENDMETHOD.

  METHOD segment_of.
    SELECT SINGLE credit_sgmnt FROM ukm_kkber2sgm WHERE kkber = @iv_kkber INTO @rv.
  ENDMETHOD.

  METHOD segment_curr.
    SELECT SINGLE currency FROM ukmcred_sgm0c WHERE credit_sgmnt = @iv_sgmnt INTO @rv.
  ENDMETHOD.

  METHOD stored_limit.
    CLEAR: ev_limit, ev_block, ev_found, ev_requested.
    " CRED_LIM_REQ is where SAP keeps a limit that waits for approval.
    SELECT SINGLE credit_limit, xblocked, cred_lim_req FROM ukmbp_cms_sgm
      WHERE partner = @iv_partner AND credit_sgmnt = @iv_sgmnt
      INTO (@ev_limit, @ev_block, @ev_requested).
    ev_found = xsdbool( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD stored_risk.
    DATA lv_r TYPE ukm_s_bp_cms-risk_class.
    SELECT SINGLE risk_class FROM ukmbp_cms WHERE partner = @iv_partner INTO @lv_r.
    rv = lv_r.
  ENDMETHOD.

  METHOD value_ok.
    rv = abap_true.
    IF iv_val IS INITIAL.
      RETURN.
    ENDIF.
    DATA: lv_obj   TYPE ddobjname,
          lv_lfld  TYPE dfies-lfieldname,
          ls_dfies TYPE dfies.
    lv_obj  = iv_tab.
    lv_lfld = iv_fld.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname    = lv_obj
        lfieldname = lv_lfld
      IMPORTING
        dfies_wa   = ls_dfies
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0 OR ls_dfies-checktable IS INITIAL OR ls_dfies-checktable = '*'.
      RETURN.
    ENDIF.
    DATA(lv_ct) = CONV string( ls_dfies-checktable ).
    DATA(lv_v) = iv_val.
    REPLACE ALL OCCURRENCES OF `'` IN lv_v WITH `''`.
    " The check table's key field carries the field's own name (T052-ZTERM,
    " TVV3-KVGR3, T056-VZSKZ); where it does not, the read fails and the
    " value is let through for the API to judge.
    DATA(lv_w) = |{ ls_dfies-fieldname } = '{ lv_v }'|.
    DATA lv_hit TYPE abap_bool.
    TRY.
        SELECT SINGLE @abap_true FROM (lv_ct) WHERE (lv_w) INTO @lv_hit.
        rv = lv_hit.
      CATCH cx_sy_dynamic_osql_error.
        rv = abap_true.
    ENDTRY.
  ENDMETHOD.

  METHOD kkber_bukrs.
    SELECT bukrs FROM t001 WHERE kkber = @iv_kkber ORDER BY bukrs INTO TABLE @rt.
  ENDMETHOD.

  METHOD cust_bukrs.
    SELECT bukrs FROM knb1 WHERE kunnr = @iv_kunnr ORDER BY bukrs INTO TABLE @rt.
  ENDMETHOD.

  METHOD cust_sales.
    SELECT vkorg, vtweg, spart FROM knvv WHERE kunnr = @iv_kunnr
      ORDER BY vkorg, vtweg, spart INTO TABLE @rt.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_FILE - the workbook
*----------------------------------------------------------------------*
CLASS lcl_file DEFINITION FINAL.
  PUBLIC SECTION.
    " Every row of the tab that carries the credit layout (KKBER in a
    " heading row), heading rows included.
    CLASS-METHODS read
      IMPORTING iv_file   TYPE string
      RETURNING VALUE(rt) TYPE tt_row
      RAISING   lcx_up.
ENDCLASS.

CLASS lcl_file IMPLEMENTATION.

  METHOD read.
    DATA: lt_bin TYPE solix_tab,
          lv_len TYPE i,
          lv_x   TYPE xstring.
    cl_gui_frontend_services=>gui_upload(
      EXPORTING  filename   = iv_file
                 filetype   = 'BIN'
      IMPORTING  filelength = lv_len
      CHANGING   data_tab   = lt_bin
      EXCEPTIONS OTHERS     = 1 ).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_up EXPORTING iv_text = |Cannot read { iv_file }|.
    ENDIF.
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING  input_length = lv_len
      IMPORTING  buffer       = lv_x
      TABLES     binary_tab   = lt_bin
      EXCEPTIONS OTHERS       = 1.
    IF sy-subrc <> 0 OR lv_x IS INITIAL.
      RAISE EXCEPTION TYPE lcx_up EXPORTING iv_text = |{ iv_file } is empty or cannot be converted|.
    ENDIF.

    DATA lo_xl TYPE REF TO cl_fdt_xl_spreadsheet.
    TRY.
        lo_xl = NEW cl_fdt_xl_spreadsheet( document_name = iv_file xdocument = lv_x ).
      CATCH cx_root INTO DATA(lx).
        DATA(lv_xt) = lx->get_text( ).
        RAISE EXCEPTION TYPE lcx_up
          EXPORTING iv_text = |The workbook cannot be read - save it as "Excel Workbook (*.xlsx)" | &&
                              |({ lv_xt })|.
    ENDTRY.

    DATA lt_names TYPE if_fdt_doc_spreadsheet=>t_worksheet_names.
    lo_xl->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = lt_names ).

    " The tab with KKBER in one of its first rows; the first tab otherwise.
    LOOP AT lt_names INTO DATA(lv_name).
      DATA(lo_data) = lo_xl->if_fdt_doc_spreadsheet~get_itab_from_worksheet( worksheet_name = lv_name ).
      FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.
      ASSIGN lo_data->* TO <lt_tab>.
      IF <lt_tab> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      DATA lt_rows TYPE tt_row.
      CLEAR lt_rows.
      DATA lv_no TYPE i.
      DATA lv_hit TYPE abap_bool.
      CLEAR: lv_no, lv_hit.
      LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_line>).
        lv_no = lv_no + 1.
        DATA ls_r TYPE ty_row.
        CLEAR ls_r.
        ls_r-row = lv_no.
        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_c>).
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          DATA lv_cell TYPE string.
          lv_cell = <lv_c>.
          APPEND lv_cell TO ls_r-cells.
          IF lv_no <= 5 AND lcl_util=>squash( lv_cell ) = 'KKBER'.
            lv_hit = abap_true.
          ENDIF.
        ENDDO.
        APPEND ls_r TO lt_rows.
      ENDLOOP.
      IF lv_hit = abap_true OR rt IS INITIAL.
        rt = lt_rows.
      ENDIF.
      IF lv_hit = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_UP - the upload
*----------------------------------------------------------------------*
CLASS lcl_up DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS run.
  PRIVATE SECTION.
    " Column of each field in this file, 0 where the file has none.
    DATA: mv_kunnr TYPE i,
          mv_kkber TYPE i,
          mv_klimg TYPE i,
          mv_klime TYPE i,
          mv_waers TYPE i,
          mv_klimk TYPE i,
          mv_ctlpc TYPE i,
          mv_crblb TYPE i,
          mv_sbgrp TYPE i,
          mv_zterm TYPE i,
          mv_kvgr3 TYPE i,
          mv_vzskz TYPE i.
    DATA mo_log TYPE REF TO lcl_log.
    DATA mo_cfg TYPE REF TO lcl_cfg.

    " The risk class each partner has been given by this file so far. In
    " ECC the risk category was per credit control area (KNKK); in SAP Credit
    " Management the risk class is one per business partner (UKMBP_CMS).
    TYPES: BEGIN OF ty_risk,
             partner TYPE bu_partner,
             risk    TYPE string,
             row     TYPE i,
           END OF ty_risk.
    DATA mt_risk TYPE HASHED TABLE OF ty_risk WITH UNIQUE KEY partner.

    METHODS columns IMPORTING it_rows TYPE tt_row.
    METHODS one     IMPORTING is_row  TYPE ty_row.

    " Limit, blocked flag and risk class through SAP Credit Management.
    METHODS credit
      IMPORTING iv_row     TYPE i
                iv_kunnr   TYPE kunnr
                iv_partner TYPE bu_partner
                iv_sgmnt   TYPE ukm_credit_sgmnt
                iv_limit   TYPE ty_dec
                iv_set_lim TYPE abap_bool
                iv_block   TYPE string
                iv_risk    TYPE string.

    " Payment terms, customer group 3 and the interest indicator, which are
    " customer master fields, through the business partner interface.
    METHODS master
      IMPORTING iv_row   TYPE i
                iv_kunnr TYPE kunnr
                iv_kkber TYPE kkber
                iv_zterm TYPE string
                iv_kvgr3 TYPE string
                iv_vzskz TYPE string.

    " The business partner keeps a memory of the closed unit of work, the
    " save mode included; the next row is refused unless it is cleared.
    METHODS reset_bp.
ENDCLASS.

CLASS lcl_up IMPLEMENTATION.

  METHOD constructor.
    mo_log = NEW lcl_log( ).
    mo_cfg = lcl_cfg=>get( ).
  ENDMETHOD.

  METHOD reset_bp.
    TRY.
        CALL FUNCTION 'BUP_MEMORY_CENTRAL_INIT'.
      CATCH cx_sy_dyn_call_illegal_func.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD columns.
    " The template's own places, used where a heading is not found.
    mv_kunnr = 1.  mv_kkber = 2.  mv_klimg = 8.  mv_klime = 9.
    mv_waers = 10. mv_klimk = 11. mv_ctlpc = 12. mv_crblb = 13.
    mv_sbgrp = 14. mv_zterm = 16. mv_kvgr3 = 17. mv_vzskz = 18.
    " Then by name, over the first five rows: the technical names of row 1
    " and the descriptions of row 2.
    LOOP AT it_rows INTO DATA(ls_r) WHERE row <= 5.
      DATA lv_c TYPE i.
      CLEAR lv_c.
      LOOP AT ls_r-cells INTO DATA(lv_h).
        lv_c = lv_c + 1.
        DATA(lv_hk) = lcl_util=>squash( lv_h ).
        CASE lv_hk.
          WHEN 'KUNNR' OR 'CUSTOMERCODE'.            mv_kunnr = lv_c.
          WHEN 'KKBER' OR 'CREDITCONTROLAREA'.       mv_kkber = lv_c.
          WHEN 'KLIMG'.                              mv_klimg = lv_c.
          WHEN 'KLIME'.                              mv_klime = lv_c.
          WHEN 'WAERS' OR 'CURRENCYKEY'.             mv_waers = lv_c.
          WHEN 'KLIMK' OR 'CUSTOMERSCREDITLIMIT'.    mv_klimk = lv_c.
          WHEN 'CTLPC' OR 'RISKCATEGORY' OR 'RISKCLASS'. mv_ctlpc = lv_c.
          WHEN 'CRBLB'.                              mv_crblb = lv_c.
          WHEN 'SBGRP'.                              mv_sbgrp = lv_c.
          WHEN 'ZTERM' OR 'PAYMENTTERMS'.            mv_zterm = lv_c.
          WHEN 'KVGR3' OR 'CUSTGRP3' OR 'CUSTOMERGROUP3'. mv_kvgr3 = lv_c.
          WHEN 'VZSKZ' OR 'Z1INTERESTINDICATORINTERESTCYCLE'. mv_vzskz = lv_c.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD run.
    DATA lt_rows TYPE tt_row.
    TRY.
        lt_rows = lcl_file=>read( CONV string( p_file ) ).
      CATCH lcx_up INTO DATA(lx).
        DATA(lv_xt) = lx->get_text( ).
        MESSAGE lv_xt TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    columns( lt_rows ).

    " A data row is one whose customer cell holds a number. The heading,
    " description and type rows of the template hold words there.
    DATA lv_n TYPE i.
    LOOP AT lt_rows INTO DATA(ls_row).
      DATA(lv_k) = lcl_util=>cell( is_row = ls_row iv_col = mv_kunnr ).
      IF lv_k IS INITIAL OR lv_k CN '0123456789'.
        CONTINUE.
      ENDIF.
      lv_n = lv_n + 1.
      one( ls_row ).
    ENDLOOP.

    IF lv_n = 0.
      MESSAGE 'No data rows found - the customer number goes in the first column, data from row 4'
              TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    DATA: lv_ok   TYPE i,
          lv_err  TYPE i,
          lv_skip TYPE i.
    mo_log->counts( IMPORTING ev_ok = lv_ok ev_err = lv_err ev_skip = lv_skip ).
    mo_log->show( |{ COND string( WHEN p_test = abap_true THEN `TEST RUN - nothing posted - ` ) }| &&
                  |{ lv_n } row(s) read: { lv_ok } OK, { lv_err } with errors, { lv_skip } with nothing to change| ).
  ENDMETHOD.

  METHOD one.
    DATA(lv_row) = is_row-row.
    DATA(lv_in)  = lcl_util=>cell( is_row = is_row iv_col = mv_kunnr ).
    DATA(lv_kunnr) = mo_cfg->customer_of( lv_in ).
    IF lv_kunnr IS INITIAL.
      mo_log->add( iv_row = lv_row iv_type = 'E'
                   iv_msg = |{ lv_in } is neither a customer nor a business partner of one| ).
      RETURN.
    ENDIF.
    DATA(lv_bp) = mo_cfg->bp_of( lv_kunnr ).
    IF lv_bp IS INITIAL.
      mo_log->add( iv_row = lv_row iv_kunnr = lv_kunnr iv_type = 'E'
                   iv_msg = 'The customer has no business partner (CVI_CUST_LINK) - credit data belongs to the partner' ).
      RETURN.
    ENDIF.

    " ---- credit control area -> segment
    DATA lv_kkber TYPE kkber.
    lv_kkber = lcl_util=>cell( is_row = is_row iv_col = mv_kkber ).
    DATA(lv_sgmnt) = COND ukm_credit_sgmnt( WHEN lv_kkber IS NOT INITIAL
                                            THEN mo_cfg->segment_of( lv_kkber ) ).
    IF lv_kkber IS INITIAL.
      mo_log->add( iv_row = lv_row iv_kunnr = lv_kunnr iv_partner = lv_bp iv_type = 'E'
                   iv_field = 'KKBER' iv_msg = 'No credit control area in this row' ).
      RETURN.
    ELSEIF lv_sgmnt IS INITIAL.
      mo_log->add( iv_row = lv_row iv_kunnr = lv_kunnr iv_partner = lv_bp iv_type = 'E'
                   iv_field = 'KKBER' iv_new = lv_kkber
                   iv_msg = |Credit control area { lv_kkber } has no credit segment (UKM_KKBER2SGM)| ).
      RETURN.
    ENDIF.

    " ---- the segment limit: KLIMK, or KLIME where KLIMK is empty
    DATA: lv_lim   TYPE ty_dec,
          lv_ok    TYPE abap_bool,
          lv_lim2  TYPE ty_dec,
          lv_ok2   TYPE abap_bool,
          lv_setl  TYPE abap_bool.
    DATA(lv_klimk) = lcl_util=>cell( is_row = is_row iv_col = mv_klimk ).
    DATA(lv_klime) = lcl_util=>cell( is_row = is_row iv_col = mv_klime ).
    IF lcl_util=>no_change( lv_klimk ) = abap_false.
      lcl_util=>to_dec( EXPORTING iv_in = lv_klimk IMPORTING ev_dec = lv_lim ev_ok = lv_ok ).
      IF lv_ok = abap_false.
        mo_log->add( iv_row = lv_row iv_kunnr = lv_kunnr iv_partner = lv_bp iv_sgmnt = lv_sgmnt
                     iv_type = 'E' iv_field = 'KLIMK' iv_new = lv_klimk
                     iv_msg = |"{ lv_klimk }" is not an amount| ).
      ELSE.
        lv_setl = abap_true.
      ENDIF.
    ENDIF.
    IF lcl_util=>no_change( lv_klime ) = abap_false.
      lcl_util=>to_dec( EXPORTING iv_in = lv_klime IMPORTING ev_dec = lv_lim2 ev_ok = lv_ok2 ).
      IF lv_ok2 = abap_false.
        mo_log->add( iv_row = lv_row iv_kunnr = lv_kunnr iv_partner = lv_bp iv_sgmnt = lv_sgmnt
                     iv_type = 'E' iv_field = 'KLIME' iv_new = lv_klime
                     iv_msg = |"{ lv_klime }" is not an amount| ).
      ELSEIF lv_setl = abap_false.
        lv_lim  = lv_lim2.
        lv_setl = abap_true.
      ELSEIF lv_lim2 <> lv_lim.
        " SAP Credit Management keeps one limit per segment.
        mo_log->add( iv_row = lv_row iv_kunnr = lv_kunnr iv_partner = lv_bp iv_sgmnt = lv_sgmnt
                     iv_type = 'W' iv_field = 'KLIME' iv_new = lv_klime
                     iv_msg = |KLIMK and KLIME differ - the segment has one limit; KLIMK { lv_lim } is used| ).
      ENDIF.
    ENDIF.

    " ---- the total across all areas -> the main segment. The template's
    " KLIMG holds X on every row: a flag of the old recording, not an amount.
    DATA: lv_tot  TYPE ty_dec,
          lv_okt  TYPE abap_bool.
    DATA(lv_klimg) = lcl_util=>cell( is_row = is_row iv_col = mv_klimg ).
    IF lcl_util=>no_change( lv_klimg ) = abap_false.
      lcl_util=>to_dec( EXPORTING iv_in = lv_klimg IMPORTING ev_dec = lv_tot ev_ok = lv_okt ).
      IF lv_okt = abap_false.
        mo_log->add( iv_row = lv_row iv_kunnr = lv_kunnr iv_partner = lv_bp iv_sgmnt = gc_main
                     iv_type = 'I' iv_field = 'KLIMG' iv_new = lv_klimg
                     iv_msg = |"{ lv_klimg }" in KLIMG is not an amount - the main segment's limit is left as it is| ).
      ELSE.
        " The main segment has a currency of its own (EUR at Cipla) and the
        " template's amounts are in the credit control area's (INR for
        " 1000). An amount is never written across currencies.
        DATA(lv_mcur) = mo_cfg->segment_curr( gc_main ).
        DATA(lv_rcur) = mo_cfg->segment_curr( lv_sgmnt ).
        IF lv_mcur <> lv_rcur.
          mo_log->add( iv_row = lv_row iv_kunnr = lv_kunnr iv_partner = lv_bp iv_sgmnt = gc_main
                       iv_type = 'E' iv_field = 'KLIMG' iv_new = lv_klimg
                       iv_msg = |The main segment { gc_main } is held in { lv_mcur }, this row's segment | &&
                                |{ lv_sgmnt } in { lv_rcur } - leave KLIMG empty, or give the total in { lv_mcur }| ).
        ENDIF.
      ENDIF.
    ENDIF.

    " ---- currency: the limit is held in the segment's currency
    DATA lv_waers TYPE waers.
    lv_waers = to_upper( lcl_util=>cell( is_row = is_row iv_col = mv_waers ) ).
    IF lv_waers IS NOT INITIAL.
      DATA(lv_scur) = mo_cfg->segment_curr( lv_sgmnt ).
      IF lv_scur IS NOT INITIAL AND lv_scur <> lv_waers.
        mo_log->add( iv_row = lv_row iv_kunnr = lv_kunnr iv_partner = lv_bp iv_sgmnt = lv_sgmnt
                     iv_type = 'E' iv_field = 'WAERS' iv_new = lv_waers
                     iv_msg = |Currency { lv_waers } - segment { lv_sgmnt } is held in { lv_scur }| ).
      ENDIF.
    ENDIF.

    " ---- risk class and blocked
    DATA(lv_risk) = to_upper( lcl_util=>cell( is_row = is_row iv_col = mv_ctlpc ) ).
    IF lcl_util=>no_change( lv_risk ) = abap_true.
      CLEAR lv_risk.
    ELSEIF mo_cfg->value_ok( iv_tab = 'UKMBP_CMS' iv_fld = 'RISK_CLASS' iv_val = lv_risk ) = abap_false.
      mo_log->add( iv_row = lv_row iv_kunnr = lv_kunnr iv_partner = lv_bp
                   iv_type = 'E' iv_field = 'CTLPC' iv_new = lv_risk
                   iv_msg = |Risk class { lv_risk } does not exist in SAP Credit Management - | &&
                            |the ECC risk category has to be one of its risk classes| ).
    ELSE.
      " One risk class per partner: two rows of one customer (two credit
      " control areas) with different risk categories cannot both be kept.
      READ TABLE mt_risk INTO DATA(ls_rk) WITH TABLE KEY partner = lv_bp.
      IF sy-subrc = 0 AND ls_rk-risk <> lv_risk.
        mo_log->add( iv_row = lv_row iv_kunnr = lv_kunnr iv_partner = lv_bp
                     iv_type = 'W' iv_field = 'CTLPC' iv_new = lv_risk
                     iv_msg = |The risk class is one per business partner - row { ls_rk-row } gives | &&
                              |{ ls_rk-risk }, so { lv_risk } from this row is not used| ).
        CLEAR lv_risk.
      ELSEIF sy-subrc <> 0.
        INSERT VALUE #( partner = lv_bp risk = lv_risk row = lv_row ) INTO TABLE mt_risk.
      ENDIF.
    ENDIF.
    DATA(lv_block) = to_upper( lcl_util=>cell( is_row = is_row iv_col = mv_crblb ) ).
    IF lcl_util=>no_change( lv_block ) = abap_true.
      CLEAR lv_block.
    ENDIF.

    " ---- credit representative group: no field in SAP Credit Management.
    DATA(lv_sbgrp) = lcl_util=>cell( is_row = is_row iv_col = mv_sbgrp ).
    IF lcl_util=>no_change( lv_sbgrp ) = abap_false.
      mo_log->add( iv_row = lv_row iv_kunnr = lv_kunnr iv_partner = lv_bp
                   iv_type = 'W' iv_field = 'SBGRP' iv_new = lv_sbgrp
                   iv_msg = 'Credit representative group not loaded - SAP Credit Management has no such field; it is to be agreed where it goes' ).
    ENDIF.

    " Nothing is posted for a row the program has already found fault with.
    IF mo_log->has_error( lv_row ) = abap_true.
      mo_log->add( iv_row = lv_row iv_kunnr = lv_kunnr iv_partner = lv_bp iv_type = 'E'
                   iv_msg = 'Row not posted - correct the errors above and load it again' ).
      RETURN.
    ENDIF.

    IF lv_setl = abap_true OR lv_block IS NOT INITIAL OR lv_risk IS NOT INITIAL.
      credit( iv_row = lv_row iv_kunnr = lv_kunnr iv_partner = lv_bp iv_sgmnt = lv_sgmnt
              iv_limit = lv_lim iv_set_lim = lv_setl iv_block = lv_block iv_risk = lv_risk ).
    ENDIF.
    IF lv_okt = abap_true.
      credit( iv_row = lv_row iv_kunnr = lv_kunnr iv_partner = lv_bp iv_sgmnt = gc_main
              iv_limit = lv_tot iv_set_lim = abap_true iv_block = `` iv_risk = `` ).
    ENDIF.

    master( iv_row   = lv_row
            iv_kunnr = lv_kunnr
            iv_kkber = lv_kkber
            iv_zterm = lcl_util=>cell( is_row = is_row iv_col = mv_zterm )
            iv_kvgr3 = lcl_util=>cell( is_row = is_row iv_col = mv_kvgr3 )
            iv_vzskz = lcl_util=>cell( is_row = is_row iv_col = mv_vzskz ) ).
  ENDMETHOD.

  METHOD credit.
    " What SAP holds now - for the list, and to see afterwards whether the
    " change was taken.
    DATA: lv_old   TYPE ty_dec,
          lv_oblk  TYPE abap_bool,
          lv_found TYPE abap_bool,
          lv_oreq  TYPE ty_dec.
    mo_cfg->stored_limit( EXPORTING iv_partner = iv_partner iv_sgmnt = iv_sgmnt
                          IMPORTING ev_limit = lv_old ev_block = lv_oblk ev_found = lv_found
                                    ev_requested = lv_oreq ).
    DATA(lv_orisk) = mo_cfg->stored_risk( iv_partner ).
    IF lv_oreq IS NOT INITIAL AND lv_oreq <> lv_old.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_partner = iv_partner iv_sgmnt = iv_sgmnt
                   iv_type = 'I' iv_field = 'Requested limit' iv_old = |{ lv_oreq }|
                   iv_msg = 'A limit request is already waiting for approval in this segment' ).
    ENDIF.

    TRY.
        " CL_UKM_FACADE is built through CREATE. BP_MAINTENANCE is the
        " activity for maintaining credit master data.
        DATA(lo_facade)  = cl_ukm_facade=>create( i_activity = cl_ukm_cnst_eventing=>bp_maintenance ).
        DATA(lo_factory) = lo_facade->get_bupa_factory( ).

        " The partner is the business partner behind the customer, read
        " from CVI_CUST_LINK - not the customer number, which is the same
        " only where the grouping uses the same number.
        IF iv_set_lim = abap_true OR iv_block IS NOT INITIAL.
          DATA(lo_account) = lo_factory->get_credit_account( i_partner      = iv_partner
                                                             i_credit_sgmnt = iv_sgmnt ).
          DATA ls_sgm TYPE ukm_s_bp_cms_sgm.
          lo_account->get_bp_cms_sgm( IMPORTING es_bp_cms_sgm = ls_sgm ).
          IF iv_set_lim = abap_true.
            ls_sgm-credit_limit   = iv_limit.
            ls_sgm-limit_chg_date = sy-datum.
          ENDIF.
          IF iv_block IS NOT INITIAL.
            ls_sgm-xblocked = xsdbool( iv_block = 'X' OR iv_block = 'Y' OR iv_block = 'YES' ).
          ENDIF.
          lo_account->set_bp_cms_sgm( is_bp_cms_sgm = ls_sgm ).
        ENDIF.

        IF iv_risk IS NOT INITIAL.
          DATA(lo_bp) = lo_factory->get_business_partner( i_partner = iv_partner ).
          DATA ls_cms TYPE ukm_s_bp_cms.
          lo_bp->get_bp_cms( IMPORTING es_bp_cms = ls_cms ).
          ls_cms-risk_class = iv_risk.
          lo_bp->set_bp_cms( is_bp_cms = ls_cms ).
        ENDIF.

        lo_factory->save_all( i_testrun = p_test i_upd_task = abap_false ).
      CATCH cx_root INTO DATA(lx).
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        reset_bp( ).
        DATA(lv_xt) = lx->get_text( ).
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_partner = iv_partner iv_sgmnt = iv_sgmnt
                     iv_type = 'E' iv_msg = |Credit data not saved: { lv_xt }| ).
        RETURN.
    ENDTRY.

    IF p_test = abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      reset_bp( ).
      IF iv_set_lim = abap_true.
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_partner = iv_partner iv_sgmnt = iv_sgmnt
                     iv_type = 'I' iv_field = 'Credit limit'
                     iv_old = COND string( WHEN lv_found = abap_true THEN |{ lv_old }| ELSE `(no segment yet)` )
                     iv_new = |{ iv_limit }| iv_msg = 'Would change (test run)' ).
      ENDIF.
      IF iv_risk IS NOT INITIAL.
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_partner = iv_partner
                     iv_type = 'I' iv_field = 'Risk class' iv_old = lv_orisk iv_new = iv_risk
                     iv_msg = 'Would change (test run)' ).
      ENDIF.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_partner = iv_partner iv_sgmnt = iv_sgmnt
                   iv_type = 'S' iv_msg = 'Test run OK - nothing was posted' ).
      RETURN.
    ENDIF.

    " SAVE_ALL does not commit; the BAPI commit does, and its RETURN says
    " whether the update ran.
    DATA ls_ret TYPE bapiret2.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait   = abap_true
      IMPORTING return = ls_ret.
    reset_bp( ).
    IF ls_ret-type CA 'EAX'.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_partner = iv_partner iv_sgmnt = iv_sgmnt
                   iv_type = 'E' iv_msg = |The update did not go through - nothing was saved: { ls_ret-message }| ).
      RETURN.
    ENDIF.

    " Read back. Where a credit limit change has to be approved, SAP keeps
    " the old limit and the new one waits - the row must not say "saved".
    IF iv_set_lim = abap_true.
      DATA: lv_now   TYPE ty_dec,
            lv_nblk  TYPE abap_bool,
            lv_nfnd  TYPE abap_bool,
            lv_req   TYPE ty_dec.
      mo_cfg->stored_limit( EXPORTING iv_partner = iv_partner iv_sgmnt = iv_sgmnt
                            IMPORTING ev_limit = lv_now ev_block = lv_nblk ev_found = lv_nfnd
                                      ev_requested = lv_req ).
      IF lv_nfnd = abap_true AND lv_now <> iv_limit AND lv_req = iv_limit.
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_partner = iv_partner iv_sgmnt = iv_sgmnt
                     iv_type = 'W' iv_field = 'Credit limit'
                     iv_old = |{ lv_old }| iv_new = |{ iv_limit }|
                     iv_msg = |Requested - the limit stays { lv_now } until the request is approved| ).
        RETURN.
      ELSEIF lv_nfnd = abap_false OR lv_now <> iv_limit.
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_partner = iv_partner iv_sgmnt = iv_sgmnt
                     iv_type = 'E' iv_field = 'Credit limit'
                     iv_old = |{ lv_old }| iv_new = |{ iv_limit }|
                     iv_msg = |SAP holds { lv_now } after the save - the new limit was not taken| ).
        RETURN.
      ENDIF.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_partner = iv_partner iv_sgmnt = iv_sgmnt
                   iv_type = 'S' iv_field = 'Credit limit' iv_old = |{ lv_old }| iv_new = |{ iv_limit }|
                   iv_msg = 'Changed' ).
    ENDIF.
    IF iv_risk IS NOT INITIAL.
      DATA(lv_nrisk) = mo_cfg->stored_risk( iv_partner ).
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_partner = iv_partner
                   iv_type = COND #( WHEN lv_nrisk = iv_risk THEN 'S' ELSE 'E' )
                   iv_field = 'Risk class' iv_old = lv_orisk iv_new = iv_risk
                   iv_msg = COND string( WHEN lv_nrisk = iv_risk THEN `Changed`
                                         ELSE |SAP holds { lv_nrisk } after the save| ) ).
    ENDIF.
    IF iv_block IS NOT INITIAL.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_partner = iv_partner iv_sgmnt = iv_sgmnt
                   iv_type = 'S' iv_field = 'Blocked' iv_old = |{ lv_oblk }| iv_new = iv_block
                   iv_msg = 'Saved' ).
    ENDIF.
  ENDMETHOD.

  METHOD master.
    DATA(lv_zterm) = to_upper( iv_zterm ).
    DATA(lv_kvgr3) = to_upper( iv_kvgr3 ).
    DATA(lv_int)   = to_upper( iv_vzskz ).
    IF lcl_util=>no_change( lv_zterm ) = abap_true.
      CLEAR lv_zterm.
    ENDIF.
    IF lcl_util=>no_change( lv_kvgr3 ) = abap_true.
      CLEAR lv_kvgr3.
    ENDIF.
    IF lcl_util=>no_change( lv_int ) = abap_true.
      CLEAR lv_int.
    ENDIF.
    IF lv_zterm IS INITIAL AND lv_kvgr3 IS INITIAL AND lv_int IS INITIAL.
      RETURN.
    ENDIF.

    " The interest column holds the indicator and, on some files, the
    " calculation cycle behind it ("Z1 3", "Z1/3"): the cycle is KNB1-ZINRT.
    DATA: lv_ind TYPE string,
          lv_cyc TYPE string.
    IF lv_int IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF `/` IN lv_int WITH ` `.
      REPLACE ALL OCCURRENCES OF `-` IN lv_int WITH ` `.
      CONDENSE lv_int.
      SPLIT lv_int AT ` ` INTO lv_ind lv_cyc.
    ENDIF.

    DATA lv_bad TYPE abap_bool.
    IF lv_zterm IS NOT INITIAL AND mo_cfg->value_ok( iv_tab = 'KNB1' iv_fld = 'ZTERM' iv_val = lv_zterm ) = abap_false.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E' iv_field = 'ZTERM' iv_new = lv_zterm
                   iv_msg = |Payment terms { lv_zterm } do not exist| ).
      lv_bad = abap_true.
    ENDIF.
    IF lv_kvgr3 IS NOT INITIAL AND mo_cfg->value_ok( iv_tab = 'KNVV' iv_fld = 'KVGR3' iv_val = lv_kvgr3 ) = abap_false.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E' iv_field = 'KVGR3' iv_new = lv_kvgr3
                   iv_msg = |Customer group 3 { lv_kvgr3 } does not exist| ).
      lv_bad = abap_true.
    ENDIF.
    IF lv_ind IS NOT INITIAL AND mo_cfg->value_ok( iv_tab = 'KNB1' iv_fld = 'VZSKZ' iv_val = lv_ind ) = abap_false.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E' iv_field = 'VZSKZ' iv_new = lv_ind
                   iv_msg = |Interest indicator { lv_ind } does not exist| ).
      lv_bad = abap_true.
    ENDIF.
    IF lv_bad = abap_true.
      RETURN.
    ENDIF.

    DATA ls_cust TYPE cmds_ei_extern.
    ls_cust-header-object_instance-kunnr = iv_kunnr.
    ls_cust-header-object_task           = gc_u.

    " ---- the company code: the customer's one inside the credit control area
    IF lv_zterm IS NOT INITIAL OR lv_ind IS NOT INITIAL.
      DATA(lt_b) = mo_cfg->cust_bukrs( iv_kunnr ).
      DATA(lt_k) = mo_cfg->kkber_bukrs( iv_kkber ).
      DATA lt_n TYPE lcl_cfg=>tt_bukrs.
      LOOP AT lt_b INTO DATA(lv_b).
        IF line_exists( lt_k[ table_line = lv_b ] ).
          APPEND lv_b TO lt_n.
        ENDIF.
      ENDLOOP.
      IF lines( lt_n ) <> 1.
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E' iv_field = 'ZTERM / VZSKZ'
                     iv_msg = |Payment terms and interest not written - the customer is in { lines( lt_n ) } | &&
                              |company code(s) of credit control area { iv_kkber }, and the file names none| ).
      ELSE.
        DATA ls_comp TYPE cmds_ei_company.
        ls_comp-task           = gc_u.
        ls_comp-data_key-bukrs = lt_n[ 1 ].
        IF lv_zterm IS NOT INITIAL.
          ls_comp-data-zterm  = lv_zterm.
          ls_comp-datax-zterm = abap_true.
        ENDIF.
        IF lv_ind IS NOT INITIAL.
          ls_comp-data-vzskz  = lv_ind.
          ls_comp-datax-vzskz = abap_true.
          IF lv_cyc IS NOT INITIAL AND lv_cyc CO '0123456789'.
            ls_comp-data-zinrt  = lv_cyc.
            ls_comp-datax-zinrt = abap_true.
          ENDIF.
        ENDIF.
        APPEND ls_comp TO ls_cust-company_data-company.
      ENDIF.
    ENDIF.

    " ---- the sales area: the customer's only one
    IF lv_kvgr3 IS NOT INITIAL.
      DATA(lt_s) = mo_cfg->cust_sales( iv_kunnr ).
      IF lines( lt_s ) <> 1.
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E' iv_field = 'KVGR3'
                     iv_msg = |Customer group 3 not written - the customer has { lines( lt_s ) } sales | &&
                              |area(s), and the file names none| ).
      ELSE.
        DATA ls_sale TYPE cmds_ei_sales.
        ls_sale-task           = gc_u.
        ls_sale-data_key-vkorg = lt_s[ 1 ]-vkorg.
        ls_sale-data_key-vtweg = lt_s[ 1 ]-vtweg.
        ls_sale-data_key-spart = lt_s[ 1 ]-spart.
        ls_sale-data-kvgr3     = lv_kvgr3.
        ls_sale-datax-kvgr3    = abap_true.
        APPEND ls_sale TO ls_cust-sales_data-sales.
      ENDIF.
    ENDIF.

    IF ls_cust-company_data-company IS INITIAL AND ls_cust-sales_data-sales IS INITIAL.
      RETURN.
    ENDIF.

    DATA ls_cvis TYPE cvis_ei_extern.
    ls_cvis-partner-header-object_task                  = gc_u.
    ls_cvis-partner-header-object_instance-bpartnerguid = mo_cfg->guid_of( iv_kunnr ).
    ls_cvis-customer = ls_cust.

    " Validate, then maintain - the API honours the test run itself.
    DATA lt_map TYPE mdg_bs_bp_msgmap_t.
    TRY.
        cl_md_bp_maintain=>validate_single( EXPORTING i_data = ls_cvis IMPORTING et_return_map = lt_map ).
      CATCH cx_root INTO DATA(lx1).
        DATA(lv_t1) = lx1->get_text( ).
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E' iv_msg = |Validation failed: { lv_t1 }| ).
        RETURN.
    ENDTRY.
    DATA lv_err TYPE abap_bool.
    LOOP AT lt_map INTO DATA(ls_map) WHERE type CA 'EAX'.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                   iv_field = |{ ls_map-bapistrucname }-{ ls_map-bapifldnm }| iv_msg = ls_map-message ).
      lv_err = abap_true.
    ENDLOOP.
    IF lv_err = abap_true.
      RETURN.
    ENDIF.

    DATA: lt_data TYPE cvis_ei_extern_t,
          lt_ret  TYPE bapiretm.
    APPEND ls_cvis TO lt_data.
    TRY.
        cl_md_bp_maintain=>maintain( EXPORTING i_data = lt_data i_test_run = p_test
                                     IMPORTING e_return = lt_ret ).
      CATCH cx_root INTO DATA(lx2).
        ROLLBACK WORK.
        reset_bp( ).
        DATA(lv_t2) = lx2->get_text( ).
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E' iv_msg = |Maintain failed: { lv_t2 }| ).
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
          mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E' iv_msg = ls_r2-message ).
          lv_err = abap_true.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
    IF lv_err = abap_true OR p_test = abap_true.
      ROLLBACK WORK.
      reset_bp( ).
      IF lv_err = abap_false.
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'S'
                     iv_msg = 'Test run OK - payment terms / interest / customer group 3 would be changed' ).
      ENDIF.
      RETURN.
    ENDIF.
    DATA ls_cret TYPE bapiret2.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait   = abap_true
      IMPORTING return = ls_cret.
    reset_bp( ).
    IF ls_cret-type CA 'EAX'.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                   iv_msg = |The update did not go through - nothing was saved: { ls_cret-message }| ).
    ELSE.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'S'
                   iv_msg = 'Payment terms / interest / customer group 3 saved' ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Events
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA gt_files TYPE filetable.
  DATA gv_rc    TYPE i.
  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING  file_filter = |Excel workbook (*.xlsx)\|*.xlsx\||
    CHANGING   file_table  = gt_files
               rc          = gv_rc
    EXCEPTIONS OTHERS      = 1 ).
  IF sy-subrc = 0 AND gt_files IS NOT INITIAL.
    IF strlen( gt_files[ 1 ]-filename ) > 255.
      MESSAGE 'That path is longer than 255 characters - move the file to a shorter path' TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      p_file = gt_files[ 1 ]-filename.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN.
  " CL_FDT_XL_SPREADSHEET reads .xlsx only - the .xlsb of the template has
  " to be saved as "Excel Workbook" first.
  IF sscrfields-ucomm = 'ONLI'.
    DATA(gv_name) = to_upper( CONV string( p_file ) ).
    IF gv_name IS INITIAL.
      MESSAGE 'Give the file - F4 opens the file dialog' TYPE 'E'.
    ELSEIF gv_name NP '*.XLSX'.
      MESSAGE 'This program reads .xlsx only - open the file in Excel and save it as "Excel Workbook (*.xlsx)"' TYPE 'E'.
    ENDIF.
  ENDIF.

START-OF-SELECTION.
  DATA(go_up) = NEW lcl_up( ).
  go_up->run( ).
