*&---------------------------------------------------------------------*
*& Report  ZSDS_CUST_MASS_UPLOAD
*&---------------------------------------------------------------------*
*& Title       : Business Partner / Customer Master Mass Upload
*& Module      : SD (customer master) incl. FI company-code and FSCM credit
*& Package     : ZSDS_BP_UPLOAD          Transaction : ZSDS_CUSTUPL
*& Source book : "customer master LSMW -  with format.xlsx" - one tab per
*&               scenario, chosen by radio button.
*&
*& Purpose
*&   One program for every customer/BP mass create and change scenario in the
*&   customer's workbook. Column layouts are the customer's existing templates
*&   so nothing has to be re-keyed.
*&
*& Why the engine changed
*&   The templates descend from ECC LSMW/BDC recordings against XD01
*&   (projects ZSD_DOM_MOROCCO / ZSD_SAGA_CUS, recordings SAGACUST, AGA_CUST,
*&   SAGA_CUST, SAGA_DOM - all transaction XD01). Under the S/4HANA Business
*&   Partner approach (SAP Note 2265093) XD01 is redirected to BP, so the
*&   recordings cannot run. The layouts are kept; the engine is the CVI/BP API.
*&
*& APIs
*&   CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE  field-level validation (message map)
*&   CL_MD_BP_MAINTAIN=>MAINTAIN         post, native I_TEST_RUN
*&   CL_UKM_FACADE / CL_UKM_ACCOUNT      FSCM credit limits and risk class
*&   Reads use SELECT only.
*&
*& The one direct table write, and why
*&   ZSD_LICENSE_CHK (transparent, 79 fields, key MANDT + KUNNR, change
*&   logging ON) holds the licence, bank-guarantee and routing data. It is a
*&   customer-owned Z table with no standard API; under BP the old
*&   CUSTOMER_ADD_DATA_CS screen exit that used to fill it is not called.
*&   Writing it directly after the BP is created was explicitly authorised.
*&   Everything else in this program goes through an API.
*&
*&   Because the templates carry 26 of the table's 79 columns, the write is
*&   READ - MERGE - MODIFY. A blank template cell must never blank a field the
*&   template does not carry (VATNO, TIN, LEGL_NAME, PYMNT_MODE, NPI_NO ...).
*&   See LCL_LIC=>SAVE.
*&
*& Data starts on row 2 of every tab; row 1 is the heading row.
*&
*& Naming convention
*&   Z<MODULE>_ pattern from Cipla_Checklist Part 1.1.
*&
*& Clean core positioning (S/4HANA 2502 / ABAP Cloud)
*&   Deliberately TIER 2. CL_MD_BP_MAINTAIN is "Not released" and no released
*&   ABAP API exists for customer master maintenance; the tier-1 alternative
*&   (OData API_BUSINESS_PARTNER) cannot sensibly drive a GUI Excel upload.
*&   Reads are confined to LCL_CFG so they can be swapped for released CDS
*&   views (I_Customer and friends) without touching the engine.
*&   Run ATC with variant ABAP_CLOUD_READINESS and record the tier-2
*&   exemptions above, plus the authorised ZSD_LICENSE_CHK write.
*&
*& Everything below was verified against DD03L from system CRS (release 816),
*& extract dd03l_new_2.xlsx - 577 objects, 20 753 rows. No field is assumed.
*&   CMDS_EI_EXTERN-HEADER-OBJECT_INSTANCE-KUNNR / -OBJECT_TASK
*&   CMDS_EI_EXTERN-CENTRAL_DATA-CENTRAL-DATA / -DATAX
*&     (CMDS_EI_VMD_CENTRAL_DATA / _XFLAG - SAP's own name, "VMD" not a typo)
*&   CMDS_EI_EXTERN-CENTRAL_DATA-ADDRESS-POSTAL-DATA (BAPIAD1VL) / -DATAX
*&   CMDS_EI_EXTERN-CENTRAL_DATA-ADDRESS-COMMUNICATION-PHONE / -FAX / -SMTP
*&     line CVIS_EI_PHONE-DATA-TELEPHONE, CVIS_EI_FAX-DATA-FAX,
*&     line CVIS_EI_SMTP-DATA-E_MAIL
*&   CMDS_EI_EXTERN-CENTRAL_DATA-TAX_IND, line CMDS_EI_TAX_IND:
*&     DATA_KEY-ALAND / -TATYP, DATA-TAXKD, DATAX-TAXKD
*&   CMDS_EI_EXTERN-COMPANY_DATA-COMPANY, line CMDS_EI_COMPANY:
*&     DATA_KEY-BUKRS, DATA / DATAX (CMDS_EI_COMPANY_DATA / _DATAX)
*&   CMDS_EI_EXTERN-SALES_DATA-SALES, line CMDS_EI_SALES:
*&     DATA_KEY-VKORG / -VTWEG / -SPART, DATA / DATAX
*&   CVIS_EI_EXTERN-PARTNER (BUS_EI_EXTERN) / -CUSTOMER (CMDS_EI_EXTERN)
*&   BUS_EI_EXTERN-BP_CONTROL-CATEGORY / -GROUPING
*&   BUS_EI_EXTERN-CENTRAL_DATA-ROLE-ROLES, line BUS_EI_BUPA_ROLES:
*&     DATA_KEY is a CHAR element (BU_ROLE), NOT a structure
*&   BUS_EI_EXTERN-CENTRAL_DATA-IDENT_NUMBER-IDENT_NUMBERS,
*&     line BUS_EI_BUPA_IDENTIFICATION: DATA_KEY-IDENTIFICATIONCATEGORY /
*&     -IDENTIFICATIONNUMBER (a structure here, unlike ROLES)
*&   ZSD_LICENSE_CHK: MANDT + KUNNR key; WERKS is NOT part of the key, so a
*&     customer has exactly one licence record
*&   UKMBP_CMS_SGM: CLIENT + PARTNER + CREDIT_SGMNT, CREDIT_LIMIT / XBLOCKED
*&   UKMBP_CMS: CLIENT + PARTNER, RISK_CLASS
*&
*& Configuration verified against the client's own tables
*&   UKM_KKBER2SGM  credit segment = credit control area, 1:1, 24 entries
*&   UKMCRED_SGM0C  segment 0000 = "Main Credit Segment"; per-area segments
*&                  carry the currency the limit is stored in
*&   TSTL           tax categories per country - IN: JTX1 JTX2 JTX3 JTX4
*&                  JOCG JTC1 | US: UTXJ UTX2 UTX3 MWST | ES: MWST | MA: ZMVT
*&   TB039A         17 BP identification types; X90003 = Aadhaar is created
*&                  by the client (X90001 NABP / X90002 NPI already exist)
*&---------------------------------------------------------------------*
REPORT zsds_cust_mass_upload.

" ICON_* constants live in the ICON type pool. TYPE-POOLS is classic ABAP
" and is one of the tier-2 exemptions recorded in the header.
TYPE-POOLS icon.

*----------------------------------------------------------------------*
* Types and constants
*----------------------------------------------------------------------*
" A RETURNING parameter must be fully typed, so the packed type used for
" credit limits is declared here rather than inline.
TYPES ty_dec TYPE p LENGTH 15 DECIMALS 2.

TYPES: BEGIN OF ty_row,
         row   TYPE i,
         cells TYPE string_table,
       END OF ty_row,
       tt_row TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

" One line per template column. Generated from the verified column analysis,
" not hand-typed - see the header comment of LCL_MAP=>BUILD.
TYPES: BEGIN OF ty_map,
         scen TYPE char2,
         col  TYPE i,
         node TYPE char1,
         fld  TYPE char30,
         cnv  TYPE char2,
       END OF ty_map,
       tt_map TYPE STANDARD TABLE OF ty_map WITH EMPTY KEY.

TYPES: BEGIN OF ty_msg,
         icon    TYPE icon_d,
         xlsrow  TYPE i,
         kunnr   TYPE char16,
         bukrs   TYPE char4,
         vkorg   TYPE char4,
         msgty   TYPE bapi_mtype,
         msgid   TYPE symsgid,
         msgno   TYPE symsgno,
         struc   TYPE char30,
         fldnm   TYPE char30,
         message TYPE bapi_msg,
       END OF ty_msg,
       tt_msg TYPE STANDARD TABLE OF ty_msg WITH EMPTY KEY.

" Node codes used by the column map.
"   K key   A address   M communication   C KNA1   B KNB1   S KNVV
"   T KNVI  Z ZSD_LICENSE_CHK   I BP identification   U FSCM credit
CONSTANTS:
  gc_n_key  TYPE char1 VALUE 'K',
  gc_n_addr TYPE char1 VALUE 'A',
  gc_n_comm TYPE char1 VALUE 'M',
  gc_n_cent TYPE char1 VALUE 'C',
  gc_n_comp TYPE char1 VALUE 'B',
  gc_n_sale TYPE char1 VALUE 'S',
  gc_n_tax  TYPE char1 VALUE 'T',
  gc_n_lic  TYPE char1 VALUE 'Z',
  gc_n_iden TYPE char1 VALUE 'I',
  gc_n_cred TYPE char1 VALUE 'U'.

CONSTANTS:
  gc_i     TYPE cmd_ei_object_task VALUE 'I',   " insert
  gc_u     TYPE cmd_ei_object_task VALUE 'U',   " update
  gc_m     TYPE cmd_ei_object_task VALUE 'M',   " modify
  gc_clear TYPE string             VALUE '#BLANK#'.

" Roles: FI customer and SD customer.
CONSTANTS:
  gc_role_fi TYPE bu_role VALUE 'FLCU00',
  gc_role_sd TYPE bu_role VALUE 'FLCU01',
  gc_org     TYPE bu_type VALUE '2'.

" Aadhaar identification type - created by the client, see header.
CONSTANTS gc_id_aadhaar TYPE bu_id_category VALUE 'X90003'.

" Main credit segment, from UKMCRED_SGM0C (MAIN_CRED_SGMNT = 'X').
CONSTANTS gc_sgm_main TYPE char10 VALUE '0000'.

" Tab names in "customer master LSMW -  with format.xlsx".
" The trailing blank in the Morocco tab name is the customer's, not a typo.
CONSTANTS:
  gc_sh_ind    TYPE string VALUE 'domestic customer IND',
  gc_sh_exp    TYPE string VALUE 'Export customer',
  gc_sh_mar    TYPE string VALUE 'Morocco customer ',
  gc_sh_saga   TYPE string VALUE 'SAGA customer',
  gc_sh_cred   TYPE string VALUE 'credit Limit',
  gc_sh_us     TYPE string VALUE 'domestic customer US',
  gc_sh_ship   TYPE string VALUE 'ship to party US'.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_r1 RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND uc,
            p_r2 RADIOBUTTON GROUP g1,
            p_r3 RADIOBUTTON GROUP g1,
            p_r4 RADIOBUTTON GROUP g1,
            p_r5 RADIOBUTTON GROUP g1,
            p_r6 RADIOBUTTON GROUP g1,
            p_r7 RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY,
            p_pc   RADIOBUTTON GROUP g2 DEFAULT 'X',
            p_srv  RADIOBUTTON GROUP g2.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_test  AS CHECKBOX DEFAULT 'X',
            p_stop  AS CHECKBOX,
            p_bpgrp TYPE bu_group.
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* Exception
*----------------------------------------------------------------------*
CLASS lcx_upl DEFINITION INHERITING FROM cx_static_check FINAL.
  PUBLIC SECTION.
    DATA text TYPE string.
    METHODS constructor IMPORTING iv_text TYPE string.
    METHODS get_text REDEFINITION.
ENDCLASS.

CLASS lcx_upl IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    text = iv_text.
  ENDMETHOD.
  METHOD get_text.
    result = text.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* LCL_UTIL - value conversion
*----------------------------------------------------------------------*
CLASS lcl_util DEFINITION FINAL.
  PUBLIC SECTION.
    " Reads cell IV_COL of IS_ROW. Returns an empty string when the column
    " is beyond the end of the row, which is normal for short rows.
    CLASS-METHODS cell
      IMPORTING is_row    TYPE ty_row
                iv_col    TYPE i
      RETURNING VALUE(rv) TYPE string.

    " Accepts DD.MM.YYYY, DD/MM/YYYY, DD-MM-YYYY and YYYYMMDD. Returns
    " initial for anything it cannot parse - the caller decides whether an
    " unparsable date is an error.
    CLASS-METHODS to_date
      IMPORTING iv_in     TYPE string
      RETURNING VALUE(rv) TYPE d.

    CLASS-METHODS to_dec
      IMPORTING iv_in     TYPE string
      RETURNING VALUE(rv) TYPE ty_dec.

    CLASS-METHODS to_int
      IMPORTING iv_in     TYPE string
      RETURNING VALUE(rv) TYPE i.

    " Conversion exits live on the DOMAIN, not on the field, so they are
    " applied here by target field rather than read from DD03L-CONROUT.
    "   KUNNR / LIFNR / VBUND -> ALPHA        AKONT -> ALPHA (SAKNR)
    "   FDGRV -> ALPHA (domain FDGRP)
    CLASS-METHODS alpha
      IMPORTING iv_in     TYPE string
      RETURNING VALUE(rv) TYPE string.

    CLASS-METHODS is_empty
      IMPORTING is_row    TYPE ty_row
      RETURNING VALUE(rv) TYPE abap_bool.
ENDCLASS.

CLASS lcl_util IMPLEMENTATION.

  METHOD cell.
    IF iv_col > 0 AND iv_col <= lines( is_row-cells ).
      rv = condense( is_row-cells[ iv_col ] ).
    ENDIF.
  ENDMETHOD.

  METHOD to_date.
    DATA(lv) = condense( iv_in ).
    IF lv IS INITIAL.
      RETURN.
    ENDIF.
    REPLACE ALL OCCURRENCES OF '/' IN lv WITH '.'.
    REPLACE ALL OCCURRENCES OF '-' IN lv WITH '.'.
    IF lv CS '.'.
      SPLIT lv AT '.' INTO DATA(lv_d) DATA(lv_m) DATA(lv_y).
      IF strlen( lv_y ) = 2.
        " Two-digit years in these templates are always this century.
        lv_y = |20{ lv_y }|.
      ENDIF.
      IF lv_d CO '0123456789' AND lv_m CO '0123456789' AND lv_y CO '0123456789'
         AND lv_d IS NOT INITIAL AND lv_m IS NOT INITIAL AND strlen( lv_y ) = 4.
        rv = |{ lv_y }{ lv_m ALPHA = IN WIDTH = 2 }{ lv_d ALPHA = IN WIDTH = 2 }|.
      ENDIF.
    ELSEIF strlen( lv ) = 8 AND lv CO '0123456789'.
      rv = lv.
    ENDIF.
    " Guard against 20260231 and friends without depending on a helper
    " class signature: a real date survives a round trip through a date
    " field, an invalid one does not.
    IF rv IS NOT INITIAL.
      DATA lv_chk TYPE d.
      DATA lv_days TYPE i.
      lv_chk = rv.
      lv_days = lv_chk - 1.
      lv_chk = lv_days + 1.
      IF lv_chk <> rv.
        CLEAR rv.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD to_dec.
    DATA(lv) = condense( iv_in ).
    IF lv IS INITIAL.
      RETURN.
    ENDIF.
    REPLACE ALL OCCURRENCES OF ',' IN lv WITH ''.
    REPLACE ALL OCCURRENCES OF ' ' IN lv WITH ''.
    TRY.
        rv = lv.
      CATCH cx_sy_conversion_no_number.
        CLEAR rv.
    ENDTRY.
  ENDMETHOD.

  METHOD to_int.
    DATA(lv_p) = to_dec( iv_in ).
    rv = round( val = lv_p dec = 0 ).
  ENDMETHOD.

  METHOD alpha.
    rv = condense( iv_in ).
    IF rv IS INITIAL OR rv = gc_clear.
      RETURN.
    ENDIF.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING input  = rv
      IMPORTING output = rv.
  ENDMETHOD.

  METHOD is_empty.
    " IS INITIAL takes a data object, not an expression, so the result of
    " CONDENSE is put in a variable first.
    DATA lv_c TYPE string.
    rv = abap_true.
    LOOP AT is_row-cells INTO DATA(lv).
      lv_c = condense( lv ).
      IF lv_c IS NOT INITIAL.
        rv = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_EXCEL - reads one tab of the .xlsx workbook
*----------------------------------------------------------------------*
CLASS lcl_excel DEFINITION FINAL.
  PUBLIC SECTION.
    " Returns the rows of IV_SHEET from row 2 onwards. Row 1 is the heading
    " row on every tab of this workbook.
    METHODS read
      IMPORTING iv_file      TYPE rlgrap-filename
                iv_from_pc   TYPE abap_bool
                iv_sheet     TYPE string
      RETURNING VALUE(rt)    TYPE tt_row
      RAISING   lcx_upl.
  PRIVATE SECTION.
    METHODS load_bin
      IMPORTING iv_file    TYPE rlgrap-filename
                iv_from_pc TYPE abap_bool
      RETURNING VALUE(rv)  TYPE xstring
      RAISING   lcx_upl.
ENDCLASS.

CLASS lcl_excel IMPLEMENTATION.

  METHOD load_bin.
    DATA lt_bin  TYPE solix_tab.
    DATA lv_len  TYPE i.

    IF iv_from_pc = abap_true.
      cl_gui_frontend_services=>gui_upload(
        EXPORTING filename   = CONV string( iv_file )
                  filetype   = 'BIN'
        IMPORTING filelength = lv_len
        CHANGING  data_tab   = lt_bin
        EXCEPTIONS OTHERS    = 1 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_upl
          EXPORTING iv_text = |Cannot read { iv_file } from the PC|.
      ENDIF.
      " SCMS_BINARY_TO_XSTRING rather than a utility class, so no method
      " signature outside this program has to be right for it to compile.
      CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
        EXPORTING input_length = lv_len
        IMPORTING buffer       = rv
        TABLES    binary_tab   = lt_bin
        EXCEPTIONS failed      = 1
                   OTHERS      = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_upl
          EXPORTING iv_text = |{ iv_file } could not be converted|.
      ENDIF.
    ELSE.
      DATA lv_msg TYPE string.
      DATA lv_x   TYPE xstring.
      " MESSAGE addition is mandatory - without it a failed open dumps.
      OPEN DATASET iv_file FOR INPUT IN BINARY MODE MESSAGE lv_msg.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_upl
          EXPORTING iv_text = |Cannot open { iv_file } on the server: { lv_msg }|.
      ENDIF.
      READ DATASET iv_file INTO lv_x.
      CLOSE DATASET iv_file.
      rv = lv_x.
    ENDIF.

    IF rv IS INITIAL.
      RAISE EXCEPTION TYPE lcx_upl EXPORTING iv_text = |{ iv_file } is empty|.
    ENDIF.
  ENDMETHOD.

  METHOD read.
    DATA(lv_bin) = load_bin( iv_file = iv_file iv_from_pc = iv_from_pc ).

    DATA lo_xl TYPE REF TO cl_fdt_xl_spreadsheet.
    TRY.
        lo_xl = NEW cl_fdt_xl_spreadsheet(
                      document_name = CONV string( iv_file )
                      xdocument     = lv_bin ).
      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION TYPE lcx_upl
          EXPORTING iv_text = |Workbook cannot be parsed: { lx->get_text( ) }|.
    ENDTRY.

    DATA lt_names TYPE if_fdt_doc_spreadsheet=>t_worksheet_names.
    lo_xl->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = lt_names ).

    " Tab names in this workbook carry trailing blanks, so compare trimmed.
    DATA lv_hit TYPE string.
    LOOP AT lt_names INTO DATA(lv_nm).
      IF to_upper( condense( CONV string( lv_nm ) ) ) = to_upper( condense( iv_sheet ) ).
        lv_hit = lv_nm.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF lv_hit IS INITIAL.
      RAISE EXCEPTION TYPE lcx_upl
        EXPORTING iv_text = |Tab "{ iv_sheet }" not found in the workbook|.
    ENDIF.

    DATA(lo_data) = lo_xl->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                      worksheet_name = CONV #( lv_hit ) ).
    FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.
    ASSIGN lo_data->* TO <lt_tab>.
    IF <lt_tab> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE lcx_upl
        EXPORTING iv_text = |Tab "{ iv_sheet }" could not be converted|.
    ENDIF.

    " CL_FDT_XL_SPREADSHEET returns the heading row as the first line, so
    " skipping index 1 leaves exactly "data from row 2 onwards".
    DATA lv_idx TYPE i.
    LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_line>).
      lv_idx = sy-tabix.
      IF lv_idx = 1.
        CONTINUE.
      ENDIF.
      DATA ls_row TYPE ty_row.
      CLEAR ls_row.
      ls_row-row = lv_idx.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_c>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        APPEND CONV string( <lv_c> ) TO ls_row-cells.
      ENDDO.
      IF lcl_util=>is_empty( ls_row ) = abap_false.
        APPEND ls_row TO rt.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_LOG - collects messages, shows them in an ALV
*----------------------------------------------------------------------*
CLASS lcl_log DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS add
      IMPORTING iv_row   TYPE i
                iv_kunnr TYPE clike OPTIONAL
                iv_bukrs TYPE clike OPTIONAL
                iv_vkorg TYPE clike OPTIONAL
                iv_type  TYPE bapi_mtype
                iv_text  TYPE clike
                iv_struc TYPE clike OPTIONAL
                iv_fld   TYPE clike OPTIONAL.

    METHODS add_msgmap
      IMPORTING iv_row   TYPE i
                iv_kunnr TYPE clike OPTIONAL
                it_map   TYPE mdg_bs_bp_msgmap_t.

    METHODS has_error
      IMPORTING iv_row    TYPE i
      RETURNING VALUE(rv) TYPE abap_bool.

    METHODS counts
      EXPORTING ev_ok  TYPE i
                ev_err TYPE i.

    METHODS display.
  PRIVATE SECTION.
    DATA mt_msg TYPE tt_msg.
ENDCLASS.

CLASS lcl_log IMPLEMENTATION.

  METHOD add.
    APPEND VALUE ty_msg(
      icon    = COND #( WHEN iv_type = 'E' OR iv_type = 'A' THEN icon_red_light
                        WHEN iv_type = 'W'                  THEN icon_yellow_light
                        ELSE                                     icon_green_light )
      xlsrow  = iv_row
      kunnr   = iv_kunnr
      bukrs   = iv_bukrs
      vkorg   = iv_vkorg
      msgty   = iv_type
      struc   = iv_struc
      fldnm   = iv_fld
      message = iv_text ) TO mt_msg.
  ENDMETHOD.

  METHOD add_msgmap.
    " MDG_BS_BP_MSGMAP carries BAPISTRUCNAME / BAPIFLDNM, which is what lets
    " us point the user at a column instead of relaying a generic message.
    LOOP AT it_map INTO DATA(ls_m).
      add( iv_row   = iv_row
           iv_kunnr = iv_kunnr
           iv_type  = ls_m-type
           iv_text  = ls_m-message
           iv_struc = ls_m-bapistrucname
           iv_fld   = ls_m-bapifldnm ).
    ENDLOOP.
  ENDMETHOD.

  METHOD has_error.
    rv = xsdbool( line_exists( mt_msg[ xlsrow = iv_row msgty = 'E' ] )
               OR line_exists( mt_msg[ xlsrow = iv_row msgty = 'A' ] ) ).
  ENDMETHOD.

  METHOD counts.
    DATA lt_bad TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    DATA lt_all TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    LOOP AT mt_msg INTO DATA(ls).
      INSERT ls-xlsrow INTO TABLE lt_all.
      IF ls-msgty = 'E' OR ls-msgty = 'A'.
        INSERT ls-xlsrow INTO TABLE lt_bad.
      ENDIF.
    ENDLOOP.
    ev_err = lines( lt_bad ).
    ev_ok  = lines( lt_all ) - ev_err.
  ENDMETHOD.

  METHOD display.
    IF mt_msg IS INITIAL.
      MESSAGE 'Nothing was processed' TYPE 'I'.
      RETURN.
    ENDIF.
    DATA lo_alv TYPE REF TO cl_salv_table.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING  t_table      = mt_msg ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->get_columns( )->set_optimize( abap_true ).
        DATA(lo_cols) = lo_alv->get_columns( ).
        lo_cols->get_column( 'ICON' )->set_short_text( 'Status' ).
        lo_cols->get_column( 'XLSROW' )->set_short_text( 'Excel row' ).
        lo_cols->get_column( 'STRUC' )->set_short_text( 'Structure' ).
        lo_cols->get_column( 'FLDNM' )->set_short_text( 'Field' ).
        lo_alv->display( ).
      CATCH cx_salv_msg cx_salv_not_found.
        LOOP AT mt_msg INTO DATA(ls_m).
          WRITE: / ls_m-xlsrow, ls_m-msgty, ls_m-message.
        ENDLOOP.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_CFG - buffered configuration and master-data reads
*   Every SELECT in this program lives here, so the reads can be swapped
*   for released CDS views later without touching the engine.
*----------------------------------------------------------------------*
CLASS lcl_cfg DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS get RETURNING VALUE(ro) TYPE REF TO lcl_cfg.

    METHODS cust_exists IMPORTING iv_kunnr  TYPE kunnr
                        RETURNING VALUE(rv) TYPE abap_bool.

    " Title text -> title key (ADRC-TITLE). The templates carry the text
    " ("Company", "Mr."), the API wants the key.
    METHODS title_key   IMPORTING iv_text   TYPE clike
                        RETURNING VALUE(rv) TYPE ad_title.

    " Departure country for the tax classification. KNVI-ALAND is the
    " country of the sales organisation's company code, not the customer's.
    METHODS aland_of    IMPORTING iv_vkorg  TYPE vkorg
                        RETURNING VALUE(rv) TYPE land1.

    " Nth configured tax category for a country, in TSTL-LFDNR order. This
    " is what TAXKD_01..TAXKD_05 mean on the positional tabs.
    METHODS tax_cat_nth IMPORTING iv_aland  TYPE land1
                                  iv_nth    TYPE i
                        RETURNING VALUE(rv) TYPE tatyp.

    METHODS tax_cat_ok  IMPORTING iv_aland  TYPE land1
                                  iv_tatyp  TYPE tatyp
                        RETURNING VALUE(rv) TYPE abap_bool.

    " Credit control area -> credit segment (UKM_KKBER2SGM, 1:1 here).
    METHODS segment_of  IMPORTING iv_kkber  TYPE kkber
                        RETURNING VALUE(rv) TYPE char10.

    METHODS segment_curr IMPORTING iv_sgmnt  TYPE char10
                         RETURNING VALUE(rv) TYPE waers.

    METHODS ok_kdgrp    IMPORTING iv        TYPE clike
                        RETURNING VALUE(rv) TYPE abap_bool.
    METHODS ok_waers    IMPORTING iv        TYPE clike
                        RETURNING VALUE(rv) TYPE abap_bool.
    METHODS ok_werks    IMPORTING iv        TYPE clike
                        RETURNING VALUE(rv) TYPE abap_bool.
    METHODS ok_ktokd    IMPORTING iv        TYPE clike
                        RETURNING VALUE(rv) TYPE abap_bool.

  PRIVATE SECTION.
    CLASS-DATA mo TYPE REF TO lcl_cfg.

    TYPES: BEGIN OF ty_tstl,
             talnd TYPE land1,
             lfdnr TYPE n LENGTH 3,
             tatyp TYPE tatyp,
           END OF ty_tstl.
    TYPES: BEGIN OF ty_sgm,
             kkber TYPE kkber,
             sgmnt TYPE char10,
           END OF ty_sgm.
    TYPES: BEGIN OF ty_cur,
             sgmnt TYPE char10,
             waers TYPE waers,
           END OF ty_cur.

    DATA mt_tstl  TYPE SORTED TABLE OF ty_tstl
                       WITH NON-UNIQUE KEY talnd lfdnr.
    DATA mt_sgm   TYPE HASHED TABLE OF ty_sgm WITH UNIQUE KEY kkber.
    DATA mt_cur   TYPE HASHED TABLE OF ty_cur WITH UNIQUE KEY sgmnt.
    DATA mt_kdgrp TYPE SORTED TABLE OF kdgrp WITH UNIQUE KEY table_line.
    DATA mt_waers TYPE SORTED TABLE OF waers WITH UNIQUE KEY table_line.
    DATA mt_werks TYPE SORTED TABLE OF werks_d WITH UNIQUE KEY table_line.
    DATA mt_ktokd TYPE SORTED TABLE OF ktokd WITH UNIQUE KEY table_line.

    METHODS constructor.
ENDCLASS.

CLASS lcl_cfg IMPLEMENTATION.

  METHOD get.
    IF mo IS INITIAL.
      mo = NEW lcl_cfg( ).
    ENDIF.
    ro = mo.
  ENDMETHOD.

  METHOD constructor.
    SELECT talnd, lfdnr, tatyp FROM tstl
      INTO CORRESPONDING FIELDS OF TABLE @mt_tstl.

    SELECT kkber, credit_sgmnt AS sgmnt FROM ukm_kkber2sgm
      INTO CORRESPONDING FIELDS OF TABLE @mt_sgm.

    SELECT credit_sgmnt AS sgmnt, currency AS waers FROM ukmcred_sgm0c
      INTO CORRESPONDING FIELDS OF TABLE @mt_cur.

    SELECT kdgrp FROM t151  INTO TABLE @mt_kdgrp.
    SELECT waers FROM tcurc INTO TABLE @mt_waers.
    SELECT werks FROM t001w INTO TABLE @mt_werks.
    SELECT ktokd FROM t077d INTO TABLE @mt_ktokd.
  ENDMETHOD.

  METHOD cust_exists.
    SELECT SINGLE @abap_true FROM kna1 WHERE kunnr = @iv_kunnr INTO @rv.
  ENDMETHOD.

  METHOD title_key.
    DATA(lv_t) = to_upper( condense( CONV string( iv_text ) ) ).
    IF lv_t IS INITIAL.
      RETURN.
    ENDIF.
    " Already a key (numeric, e.g. 0003) - take it as given.
    IF lv_t CO '0123456789'.
      rv = |{ lv_t ALPHA = IN WIDTH = 4 }|.
      RETURN.
    ENDIF.
    SELECT SINGLE title FROM tsad3t
      WHERE langu = @sy-langu AND title_medi = @lv_t
      INTO @rv.
    IF sy-subrc <> 0.
      SELECT SINGLE title FROM tsad3t
        WHERE title_medi = @lv_t
        INTO @rv.
    ENDIF.
  ENDMETHOD.

  METHOD aland_of.
    DATA lv_bukrs TYPE bukrs.
    SELECT SINGLE bukrs FROM tvko WHERE vkorg = @iv_vkorg INTO @lv_bukrs.
    IF sy-subrc = 0.
      SELECT SINGLE land1 FROM t001 WHERE bukrs = @lv_bukrs INTO @rv.
    ENDIF.
  ENDMETHOD.

  METHOD tax_cat_nth.
    DATA lv_n TYPE i.
    LOOP AT mt_tstl INTO DATA(ls) WHERE talnd = iv_aland.
      lv_n = lv_n + 1.
      IF lv_n = iv_nth.
        rv = ls-tatyp.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD tax_cat_ok.
    rv = xsdbool( line_exists( mt_tstl[ talnd = iv_aland tatyp = iv_tatyp ] ) ).
  ENDMETHOD.

  METHOD segment_of.
    rv = VALUE #( mt_sgm[ kkber = iv_kkber ]-sgmnt OPTIONAL ).
  ENDMETHOD.

  METHOD segment_curr.
    rv = VALUE #( mt_cur[ sgmnt = iv_sgmnt ]-waers OPTIONAL ).
  ENDMETHOD.

  METHOD ok_kdgrp.
    rv = xsdbool( line_exists( mt_kdgrp[ table_line = iv ] ) ).
  ENDMETHOD.
  METHOD ok_waers.
    rv = xsdbool( line_exists( mt_waers[ table_line = iv ] ) ).
  ENDMETHOD.
  METHOD ok_werks.
    rv = xsdbool( line_exists( mt_werks[ table_line = iv ] ) ).
  ENDMETHOD.
  METHOD ok_ktokd.
    rv = xsdbool( line_exists( mt_ktokd[ table_line = iv ] ) ).
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_MAP - the column map
*   Generated from the verified analysis of the workbook against DD03L,
*   the two LSMW project exports (ZSD_DOM_MOROCCO, ZSD_SAGA_CUS) and the
*   ZSD_LICENSE_CHK dictionary listing. 568 columns across 7 tabs.
*
*   Corrections applied to the templates, and why:
*     R1 col 77  heading reads JOIG (Integrated GST) but describes Central
*                GST; TSTL configures JOCG for IN, and no JOIG exists.
*     R3 cols 61-64, R4 cols 72-75  TAXKD_02..05 dropped: MA has one tax
*                category (ZMVT) and ES has one (MWST), so there is nothing
*                for the other four columns to write to.
*     R3 cols 112-116  label remnants describing cols 107-111.
*     R4 col 36  blank heading; it is LIFNR - both the AGA_CUST recording
*                and the IND tab run KATR3, KATR4, LIFNR, VBUND, KONZS.
*     R4 cols 39-41  the three Spanish DIR3 codes for FACe. Allocated
*                STCD3 = Accounting Office, STCD4 = Managing Office,
*                STCD5 = Processing Unit (SAP maps tax type ES5 -> STCD5,
*                and the original AGA_CUST recording used STCD5).
*     R4 col 44  reads STCD3 a second time but is annotated 2270001, a
*                reconciliation account; loaded as AKONT.
*     R5 col 14  SBGRP dropped - T014-SBGRP is empty for every credit
*                control area, so the concept is not configured here.
*----------------------------------------------------------------------*
CLASS lcl_map DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS for
      IMPORTING iv_scen   TYPE char2
      RETURNING VALUE(rt) TYPE tt_map.
  PRIVATE SECTION.
    CLASS-DATA mt TYPE tt_map.
    CLASS-METHODS build RETURNING VALUE(rt) TYPE tt_map.
ENDCLASS.

CLASS lcl_map IMPLEMENTATION.

  METHOD for.
    IF mt IS INITIAL.
      mt = build( ).
    ENDIF.
    rt = VALUE #( FOR ls IN mt WHERE ( scen = iv_scen ) ( ls ) ).
  ENDMETHOD.

  METHOD build.
    " R1 - domestic customer IND (136 columns, 129 mapped)
    APPEND LINES OF VALUE tt_map(
      ( scen = 'R1' col = 2    node = 'K' fld = 'KUNNR' cnv = 'AL' )  " New Customer Code
      ( scen = 'R1' col = 3    node = 'K' fld = 'BUKRS' cnv = '' )  " Company Code
      ( scen = 'R1' col = 4    node = 'K' fld = 'VKORG' cnv = '' )  " Sales Organization
      ( scen = 'R1' col = 5    node = 'K' fld = 'VTWEG' cnv = '' )  " Distribution Channel
      ( scen = 'R1' col = 6    node = 'K' fld = 'SPART' cnv = '' )  " Division
      ( scen = 'R1' col = 7    node = 'K' fld = 'KTOKD' cnv = '' )  " Customer Account Group
      ( scen = 'R1' col = 14   node = 'A' fld = 'TITLE' cnv = 'TT' )  " Title text
      ( scen = 'R1' col = 15   node = 'A' fld = 'NAME' cnv = '' )  " Name 1
      ( scen = 'R1' col = 16   node = 'A' fld = 'NAME_2' cnv = '' )  " Name 2
      ( scen = 'R1' col = 17   node = 'A' fld = 'NAME_3' cnv = '' )  " Name 3
      ( scen = 'R1' col = 18   node = 'A' fld = 'NAME_4' cnv = '' )  " Name 4
      ( scen = 'R1' col = 19   node = 'A' fld = 'SORT1' cnv = '' )  " Search Term 1
      ( scen = 'R1' col = 20   node = 'A' fld = 'SORT2' cnv = '' )  " Search Term 2
      ( scen = 'R1' col = 21   node = 'A' fld = 'C_O_NAME' cnv = '' )  " c/o name
      ( scen = 'R1' col = 22   node = 'A' fld = 'STR_SUPPL1' cnv = '' )  " Street 2
      ( scen = 'R1' col = 23   node = 'A' fld = 'STR_SUPPL2' cnv = '' )  " Street 3
      ( scen = 'R1' col = 24   node = 'A' fld = 'STREET' cnv = '' )  " Street
      ( scen = 'R1' col = 25   node = 'A' fld = 'HOUSE_NO' cnv = '' )  " House Number
      ( scen = 'R1' col = 26   node = 'A' fld = 'STR_SUPPL3' cnv = '' )  " Street 4
      ( scen = 'R1' col = 27   node = 'A' fld = 'LOCATION' cnv = '' )  " Street 5
      ( scen = 'R1' col = 28   node = 'A' fld = 'DISTRICT' cnv = '' )  " District
      ( scen = 'R1' col = 29   node = 'A' fld = 'POSTL_COD1' cnv = '' )  " City postal code
      ( scen = 'R1' col = 30   node = 'A' fld = 'CITY' cnv = '' )  " City
      ( scen = 'R1' col = 31   node = 'A' fld = 'COUNTRY' cnv = '' )  " Country Key
      ( scen = 'R1' col = 32   node = 'A' fld = 'REGION' cnv = '' )  " Region (State, Province, County)
      ( scen = 'R1' col = 33   node = 'A' fld = 'LANGU' cnv = '' )  " Language Key
      ( scen = 'R1' col = 34   node = 'M' fld = 'TEL' cnv = '' )  " First telephone no.: dialling code+number
      ( scen = 'R1' col = 35   node = 'M' fld = 'MOB' cnv = '' )  " First Mobile Telephone No.: Dialing Code + Number
      ( scen = 'R1' col = 36   node = 'M' fld = 'FAX' cnv = '' )  " First fax no.: dialling code+number
      ( scen = 'R1' col = 37   node = 'M' fld = 'SMT' cnv = '' )  " E-Mail Address
      ( scen = 'R1' col = 38   node = 'C' fld = 'KATR1' cnv = '' )  " Attribute 1
      ( scen = 'R1' col = 39   node = 'C' fld = 'KATR3' cnv = '' )  " Attribute 3
      ( scen = 'R1' col = 40   node = 'C' fld = 'KATR4' cnv = '' )  " Attribute 4
      ( scen = 'R1' col = 41   node = 'C' fld = 'LIFNR' cnv = 'AL' )  " Account Number of Vendor or Creditor
      ( scen = 'R1' col = 42   node = 'C' fld = 'VBUND' cnv = 'AL' )  " Company ID of Trading Partner
      ( scen = 'R1' col = 43   node = 'C' fld = 'KONZS' cnv = '' )  " Group key
      ( scen = 'R1' col = 44   node = 'C' fld = 'STCD3' cnv = '' )  " Tax Number 3 ( GST Number)
      ( scen = 'R1' col = 45   node = 'C' fld = 'J_1IPANNO' cnv = '' )  " Permanent Account Number
      ( scen = 'R1' col = 46   node = 'C' fld = 'GST_TDS' cnv = '' )  " GST TDS Registration
      ( scen = 'R1' col = 47   node = 'I' fld = 'X90003' cnv = '' )  " Aadhaar Number
      ( scen = 'R1' col = 48   node = 'B' fld = 'AKONT' cnv = 'GL' )  " Reconciliation Account in General Ledger
      ( scen = 'R1' col = 49   node = 'B' fld = 'ZUAWA' cnv = '' )  " Key for sorting according to assignment numbers
      ( scen = 'R1' col = 50   node = 'B' fld = 'FDGRV' cnv = 'AL' )  " Planning group
      ( scen = 'R1' col = 51   node = 'B' fld = 'VZSKZ' cnv = '' )  " Interest calculation indicator
      ( scen = 'R1' col = 52   node = 'B' fld = 'ZINRT' cnv = '' )  " Interest calculation frequency in months
      ( scen = 'R1' col = 53   node = 'B' fld = 'ALTKN' cnv = '' )  " Previous Master Record Number
      ( scen = 'R1' col = 54   node = 'B' fld = 'ZTERM' cnv = '' )  " Terms of Payment Key
      ( scen = 'R1' col = 55   node = 'B' fld = 'TOGRU' cnv = '' )  " Tolerance group for the business partner/G/L account
      ( scen = 'R1' col = 56   node = 'B' fld = 'XZVER' cnv = '' )  " Indicator: Record Payment History ?
      ( scen = 'R1' col = 57   node = 'B' fld = 'ZWELS' cnv = '' )  " List of the Payment Methods to be Considered
      ( scen = 'R1' col = 58   node = 'B' fld = 'ZAHLS' cnv = '' )  " Block Key for Payment
      ( scen = 'R1' col = 59   node = 'S' fld = 'BZIRK' cnv = '' )  " Sales district
      ( scen = 'R1' col = 60   node = 'S' fld = 'VKBUR' cnv = '' )  " Sales Office
      ( scen = 'R1' col = 61   node = 'S' fld = 'VKGRP' cnv = '' )  " Sales Group
      ( scen = 'R1' col = 62   node = 'S' fld = 'KDGRP' cnv = '' )  " Customer group
      ( scen = 'R1' col = 63   node = 'S' fld = 'KLABC' cnv = '' )  " Customer classification (ABC analysis)
      ( scen = 'R1' col = 64   node = 'S' fld = 'WAERS' cnv = '' )  " Currency
      ( scen = 'R1' col = 65   node = 'S' fld = 'KONDA' cnv = '' )  " Price group (customer)
      ( scen = 'R1' col = 66   node = 'S' fld = 'KALKS' cnv = '' )  " Pricing procedure assigned to this customer
      ( scen = 'R1' col = 67   node = 'S' fld = 'VERSG' cnv = '' )  " Customer Statistics Group
      ( scen = 'R1' col = 68   node = 'S' fld = 'LPRIO' cnv = '' )  " Delivery Priority
      ( scen = 'R1' col = 69   node = 'S' fld = 'KZAZU' cnv = '' )  " Order Combination Indicator
      ( scen = 'R1' col = 70   node = 'S' fld = 'VSBED' cnv = '' )  " Shipping Conditions
      ( scen = 'R1' col = 71   node = 'S' fld = 'VWERK' cnv = '' )  " Delivering Plant (Own or External)
      ( scen = 'R1' col = 72   node = 'S' fld = 'ANTLF' cnv = '' )  " Maximum Number of Partial Deliveries Allowed Per Item
      ( scen = 'R1' col = 73   node = 'S' fld = 'INCO1' cnv = '' )  " Incoterms (Part 1)
      ( scen = 'R1' col = 74   node = 'S' fld = 'INCO2' cnv = '' )  " Incoterms (Part 2)
      ( scen = 'R1' col = 75   node = 'B' fld = 'ZTERM' cnv = '' )  " Terms of Payment Key
      ( scen = 'R1' col = 76   node = 'S' fld = 'KTGRD' cnv = '' )  " Account Assignment Group for Customer
      ( scen = 'R1' col = 77   node = 'T' fld = 'JOCG' cnv = '' )  " JOIG IN:Central GST - OP
      ( scen = 'R1' col = 78   node = 'T' fld = 'JTC1' cnv = '' )  " JTC1 IN: 206C(1H) Goods
      ( scen = 'R1' col = 79   node = 'T' fld = 'JTX1' cnv = '' )  " JTX1 Tax Jurisdict.Code d
      ( scen = 'R1' col = 80   node = 'T' fld = 'JTX2' cnv = '' )  " JTX2 Tax Jurisdict.Code d
      ( scen = 'R1' col = 81   node = 'T' fld = 'JTX3' cnv = '' )  " JTX3 Tax Jurisdict.Code d
      ( scen = 'R1' col = 82   node = 'T' fld = 'JTX4' cnv = '' )  " JTX4 Tax Jurisdict.Code d
      ( scen = 'R1' col = 83   node = 'S' fld = 'KVGR1' cnv = '' )  " Customer group 1
      ( scen = 'R1' col = 84   node = 'S' fld = 'KVGR2' cnv = '' )  " Customer group 2
      ( scen = 'R1' col = 85   node = 'S' fld = 'KVGR3' cnv = '' )  " Customer group 3
      ( scen = 'R1' col = 86   node = 'S' fld = 'KVGR4' cnv = '' )  " Customer group 4
      ( scen = 'R1' col = 87   node = 'S' fld = 'KVGR5' cnv = '' )  " Customer group 5
      ( scen = 'R1' col = 88   node = 'Z' fld = 'WERKS' cnv = '' )  " Plant
      ( scen = 'R1' col = 89   node = 'Z' fld = 'CUST_TRNST_DAYS' cnv = 'NM' )  " Transit Day
      ( scen = 'R1' col = 90   node = 'Z' fld = 'KMSUM' cnv = 'NM' )  " Distance in kms.
      ( scen = 'R1' col = 91   node = 'Z' fld = 'DRUGLICENSE1' cnv = '' )  " 20B. Lic. No
      ( scen = 'R1' col = 92   node = 'Z' fld = 'DEA_EXEMPT' cnv = '' )  " DEA_exempt
      ( scen = 'R1' col = 93   node = 'Z' fld = 'DRUGLICENSE2' cnv = '' )  " 21B. Lic. No
      ( scen = 'R1' col = 94   node = 'Z' fld = 'SL_EXEMPT' cnv = '' )  " SL_EXEMPT
      ( scen = 'R1' col = 95   node = 'Z' fld = 'DL1_DL2_VALIDDT' cnv = 'DT' )  " 20B and 21B Expiry Date
      ( scen = 'R1' col = 96   node = 'Z' fld = 'FOODSLICENSE' cnv = '' )  " Food Lic
      ( scen = 'R1' col = 97   node = 'Z' fld = 'FL_VALIDDT' cnv = 'DT' )  " Food Lic Valid Date
      ( scen = 'R1' col = 98   node = 'Z' fld = 'SCHXNO' cnv = '' )  " Sch. X Wh.Sale Lic No
      ( scen = 'R1' col = 99   node = 'Z' fld = 'SCHX_VALIDDT' cnv = 'DT' )  " Schedule-X Wh.Sale Lic. Exp. Date
      ( scen = 'R1' col = 100  node = 'Z' fld = 'SCHXRNO' cnv = '' )  " Sch. X Retail Lic No
      ( scen = 'R1' col = 101  node = 'Z' fld = 'SCHXR_VALIDDT' cnv = 'DT' )  " Sch. X Retail Lic Exp. Date
      ( scen = 'R1' col = 102  node = 'Z' fld = 'RETAIL_LIC_NO' cnv = '' )  " Retails Lic No (20 and 21 )
      ( scen = 'R1' col = 103  node = 'Z' fld = 'SC_EXEMPT' cnv = '' )  " SC_EXEMPT
      ( scen = 'R1' col = 104  node = 'Z' fld = 'RETAIL_EXP' cnv = 'DT' )  " Retails Lic Exp date
      ( scen = 'R1' col = 105  node = 'Z' fld = 'MFGLIC1NO' cnv = '' )  " Mfg License (Gen) Number
      ( scen = 'R1' col = 106  node = 'Z' fld = 'MFGLIC2NO' cnv = '' )  " Mfg License (Nar) Number
      ( scen = 'R1' col = 107  node = 'Z' fld = 'MFGLIC3NO' cnv = '' )  " Mfg License (CC) Number
      ( scen = 'R1' col = 108  node = 'Z' fld = 'BGYN' cnv = '' )  " Bank Guarantee(Y/N)
      ( scen = 'R1' col = 109  node = 'Z' fld = 'BG_NO' cnv = '' )  " Bank Guarantee No
      ( scen = 'R1' col = 110  node = 'Z' fld = 'BG_AMT' cnv = 'NM' )  " BG Amount
      ( scen = 'R1' col = 111  node = 'Z' fld = 'CURRENCY' cnv = '' )  " SD Document Currency
      ( scen = 'R1' col = 112  node = 'Z' fld = 'BG_ISS_DT' cnv = 'DT' )  " BG Issue Date
      ( scen = 'R1' col = 113  node = 'Z' fld = 'BG_EXP_DT' cnv = 'DT' )  " BG Expiry Date
      ( scen = 'R1' col = 114  node = 'Z' fld = 'BG_ISS_BANK' cnv = '' )  " BG Issuing Bank
      ( scen = 'R1' col = 115  node = 'Z' fld = 'AGGR_EXPDT' cnv = 'DT' )  " Agreement Expiry Date
      ( scen = 'R1' col = 116  node = 'Z' fld = 'APPOINT_DT' cnv = 'DT' )  " Appointment Date
      ( scen = 'R1' col = 117  node = 'S' fld = 'KDGRP' cnv = '' )  " Customer group
      ( scen = 'R1' col = 118  node = 'Z' fld = 'AIOCD_CODE' cnv = '' )  " AIOCD Code
      ( scen = 'R1' col = 119  node = 'Z' fld = 'CUST_BNK_NAME' cnv = '' )  " Customer Bank Name
      ( scen = 'R1' col = 120  node = 'Z' fld = 'DST_BOOKING' cnv = '' )  " Destination of Booking
      ( scen = 'R1' col = 121  node = 'Z' fld = 'ZTROUT' cnv = '' )  " Route Code
      ( scen = 'R1' col = 122  node = 'Z' fld = 'EXTENSION' cnv = '' )  " Extension
      ( scen = 'R1' col = 123  node = 'Z' fld = 'ZCROUT' cnv = '' )  " Route
      ( scen = 'R1' col = 124  node = 'Z' fld = 'GLN_URI_FORMAT' cnv = '' )  " GLN URI Format
      ( scen = 'R1' col = 125  node = 'Z' fld = 'DUNS_NUMBER' cnv = '' )  " DUNS_Number
      ( scen = 'R1' col = 126  node = 'Z' fld = 'DEA_FROM_DATE' cnv = 'DT' )  " DEA From Date
      ( scen = 'R1' col = 127  node = 'Z' fld = 'DEA_TO_DATE' cnv = 'DT' )  " DEA To Date
      ( scen = 'R1' col = 128  node = 'Z' fld = 'STATE_FROM_DATE' cnv = 'DT' )  " State From Date
      ( scen = 'R1' col = 129  node = 'Z' fld = 'STATE_TO_DATE' cnv = 'DT' )  " State To Date
      ( scen = 'R1' col = 130  node = 'Z' fld = 'ZIMP_LIC_MIA' cnv = '' )  " Import_License/MIA
      ( scen = 'R1' col = 131  node = 'Z' fld = 'ZIMP_FROMDT_MIA' cnv = 'DT' )  " IMPL/MIA_From_Date
      ( scen = 'R1' col = 132  node = 'Z' fld = 'ZIMP_VALIDDT_MIA' cnv = 'DT' )  " IMPL/MIA_Valid_Date
      ( scen = 'R1' col = 133  node = 'Z' fld = 'CHECK_DIGIT' cnv = '' )  " Check Digit
      ( scen = 'R1' col = 134  node = 'Z' fld = 'GLOBAL_COM' cnv = '' )  " Global Company Prefix
      ( scen = 'R1' col = 135  node = 'Z' fld = 'BO_DAYS' cnv = '' )  " Backorder Days
      ( scen = 'R1' col = 136  node = 'Z' fld = 'LOCATION_NUMBER' cnv = '' )  " Location Number
    ) TO rt.

    " R2 - Export customer (67 columns, 64 mapped)
    APPEND LINES OF VALUE tt_map(
      ( scen = 'R2' col = 2    node = 'K' fld = 'BUKRS' cnv = '' )  " BUKRS
      ( scen = 'R2' col = 3    node = 'K' fld = 'VKORG' cnv = '' )  " VKORG
      ( scen = 'R2' col = 4    node = 'K' fld = 'VTWEG' cnv = '' )  " VTWEG
      ( scen = 'R2' col = 5    node = 'K' fld = 'SPART' cnv = '' )  " SPART
      ( scen = 'R2' col = 6    node = 'K' fld = 'KTOKD' cnv = '' )  " KTOKD
      ( scen = 'R2' col = 8    node = 'A' fld = 'TITLE' cnv = 'TT' )  " TITLE_MEDI
      ( scen = 'R2' col = 9    node = 'A' fld = 'NAME' cnv = '' )  " NAME1
      ( scen = 'R2' col = 10   node = 'A' fld = 'NAME_2' cnv = '' )  " NAME2
      ( scen = 'R2' col = 11   node = 'A' fld = 'NAME_3' cnv = '' )  " NAME3
      ( scen = 'R2' col = 12   node = 'A' fld = 'NAME_4' cnv = '' )  " NAME4
      ( scen = 'R2' col = 13   node = 'A' fld = 'SORT1' cnv = '' )  " SORT1
      ( scen = 'R2' col = 14   node = 'A' fld = 'SORT2' cnv = '' )  " SORT2
      ( scen = 'R2' col = 15   node = 'A' fld = 'C_O_NAME' cnv = '' )  " NAME_CO
      ( scen = 'R2' col = 16   node = 'A' fld = 'STR_SUPPL1' cnv = '' )  " STR_SUPPL1
      ( scen = 'R2' col = 17   node = 'A' fld = 'STR_SUPPL2' cnv = '' )  " STR_SUPPL2
      ( scen = 'R2' col = 18   node = 'A' fld = 'STREET' cnv = '' )  " STREET
      ( scen = 'R2' col = 19   node = 'A' fld = 'STR_SUPPL3' cnv = '' )  " STR_SUPPL3
      ( scen = 'R2' col = 20   node = 'A' fld = 'LOCATION' cnv = '' )  " LOCATION
      ( scen = 'R2' col = 21   node = 'A' fld = 'DISTRICT' cnv = '' )  " CITY2
      ( scen = 'R2' col = 22   node = 'A' fld = 'POSTL_COD1' cnv = '' )  " POST_CODE1
      ( scen = 'R2' col = 23   node = 'A' fld = 'CITY' cnv = '' )  " CITY1
      ( scen = 'R2' col = 24   node = 'A' fld = 'COUNTRY' cnv = '' )  " COUNTRY
      ( scen = 'R2' col = 25   node = 'A' fld = 'REGION' cnv = '' )  " REGION
      ( scen = 'R2' col = 26   node = 'A' fld = 'LANGU' cnv = '' )  " LANGU
      ( scen = 'R2' col = 27   node = 'M' fld = 'TEL' cnv = '' )  " TEL_NUMBER
      ( scen = 'R2' col = 28   node = 'M' fld = 'MOB' cnv = '' )  " MOB_NUMBER
      ( scen = 'R2' col = 29   node = 'M' fld = 'FAX' cnv = '' )  " FAX_NUMBER
      ( scen = 'R2' col = 30   node = 'M' fld = 'SMT' cnv = '' )  " SMTP_ADDR
      ( scen = 'R2' col = 32   node = 'C' fld = 'LIFNR' cnv = 'AL' )  " LIFNR
      ( scen = 'R2' col = 33   node = 'C' fld = 'KUKLA' cnv = '' )  " KUKLA
      ( scen = 'R2' col = 34   node = 'C' fld = 'UMSA1' cnv = '' )  " UMSA1
      ( scen = 'R2' col = 35   node = 'C' fld = 'UWAER' cnv = '' )  " UWAER
      ( scen = 'R2' col = 36   node = 'C' fld = 'UMJAH' cnv = '' )  " UMJAH
      ( scen = 'R2' col = 37   node = 'B' fld = 'AKONT' cnv = 'GL' )  " AKONT
      ( scen = 'R2' col = 38   node = 'B' fld = 'ZUAWA' cnv = '' )  " ZUAWA
      ( scen = 'R2' col = 39   node = 'B' fld = 'XZVER' cnv = '' )  " XZVER
      ( scen = 'R2' col = 40   node = 'S' fld = 'BZIRK' cnv = '' )  " BZIRK
      ( scen = 'R2' col = 41   node = 'S' fld = 'AWAHR' cnv = '' )  " AWAHR
      ( scen = 'R2' col = 42   node = 'S' fld = 'VKBUR' cnv = '' )  " VKBUR
      ( scen = 'R2' col = 43   node = 'S' fld = 'VKGRP' cnv = '' )  " VKGRP
      ( scen = 'R2' col = 44   node = 'S' fld = 'KDGRP' cnv = '' )  " KDGRP
      ( scen = 'R2' col = 45   node = 'S' fld = 'KLABC' cnv = '' )  " KLABC
      ( scen = 'R2' col = 46   node = 'S' fld = 'WAERS' cnv = '' )  " WAERS
      ( scen = 'R2' col = 47   node = 'S' fld = 'KURST' cnv = '' )  " KURST
      ( scen = 'R2' col = 48   node = 'S' fld = 'KALKS' cnv = '' )  " KALKS
      ( scen = 'R2' col = 49   node = 'S' fld = 'VERSG' cnv = '' )  " VERSG
      ( scen = 'R2' col = 50   node = 'S' fld = 'LPRIO' cnv = '' )  " LPRIO
      ( scen = 'R2' col = 51   node = 'S' fld = 'KZAZU' cnv = '' )  " KZAZU
      ( scen = 'R2' col = 52   node = 'S' fld = 'VSBED' cnv = '' )  " VSBED
      ( scen = 'R2' col = 53   node = 'S' fld = 'VWERK' cnv = '' )  " VWERK
      ( scen = 'R2' col = 54   node = 'S' fld = 'ANTLF' cnv = '' )  " ANTLF
      ( scen = 'R2' col = 55   node = 'S' fld = 'INCO1' cnv = '' )  " INCO1
      ( scen = 'R2' col = 56   node = 'S' fld = 'INCO2' cnv = '' )  " INCO2
      ( scen = 'R2' col = 57   node = 'B' fld = 'ZTERM' cnv = '' )  " ZTERM
      ( scen = 'R2' col = 58   node = 'S' fld = 'KTGRD' cnv = '' )  " KTGRD
      ( scen = 'R2' col = 59   node = 'T' fld = '#1' cnv = '' )  " TAXKD_01
      ( scen = 'R2' col = 60   node = 'T' fld = '#2' cnv = '' )  " TAXKD_02
      ( scen = 'R2' col = 61   node = 'T' fld = '#3' cnv = '' )  " TAXKD_03
      ( scen = 'R2' col = 62   node = 'T' fld = '#4' cnv = '' )  " TAXKD_04
      ( scen = 'R2' col = 63   node = 'S' fld = 'KVGR1' cnv = '' )  " KVGR1
      ( scen = 'R2' col = 64   node = 'S' fld = 'KVGR3' cnv = '' )  " KVGR3
      ( scen = 'R2' col = 65   node = 'S' fld = 'KVGR4' cnv = '' )  " KVGR4
      ( scen = 'R2' col = 66   node = 'S' fld = 'KVGR5' cnv = '' )  " KVGR5
      ( scen = 'R2' col = 67   node = 'C' fld = 'J_1IPANNO' cnv = '' )  " PAN No
    ) TO rt.

    " R3 - Morocco customer (116 columns, 104 mapped)
    APPEND LINES OF VALUE tt_map(
      ( scen = 'R3' col = 3    node = 'K' fld = 'BUKRS' cnv = '' )  " BUKRS
      ( scen = 'R3' col = 4    node = 'K' fld = 'VKORG' cnv = '' )  " VKORG
      ( scen = 'R3' col = 5    node = 'K' fld = 'VTWEG' cnv = '' )  " VTWEG
      ( scen = 'R3' col = 6    node = 'K' fld = 'SPART' cnv = '' )  " SPART
      ( scen = 'R3' col = 7    node = 'K' fld = 'KTOKD' cnv = '' )  " KTOKD
      ( scen = 'R3' col = 9    node = 'A' fld = 'TITLE' cnv = 'TT' )  " TITLE_MEDI
      ( scen = 'R3' col = 10   node = 'A' fld = 'NAME' cnv = '' )  " NAME1
      ( scen = 'R3' col = 11   node = 'A' fld = 'NAME_2' cnv = '' )  " NAME2
      ( scen = 'R3' col = 12   node = 'A' fld = 'NAME_3' cnv = '' )  " NAME3
      ( scen = 'R3' col = 13   node = 'A' fld = 'NAME_4' cnv = '' )  " NAME4
      ( scen = 'R3' col = 14   node = 'A' fld = 'SORT1' cnv = '' )  " SORT1
      ( scen = 'R3' col = 15   node = 'A' fld = 'SORT2' cnv = '' )  " SORT2
      ( scen = 'R3' col = 16   node = 'A' fld = 'C_O_NAME' cnv = '' )  " NAME_CO
      ( scen = 'R3' col = 17   node = 'A' fld = 'STR_SUPPL1' cnv = '' )  " STR_SUPPL1
      ( scen = 'R3' col = 18   node = 'A' fld = 'STR_SUPPL2' cnv = '' )  " STR_SUPPL2
      ( scen = 'R3' col = 19   node = 'A' fld = 'STREET' cnv = '' )  " STREET
      ( scen = 'R3' col = 20   node = 'A' fld = 'STR_SUPPL3' cnv = '' )  " STR_SUPPL3
      ( scen = 'R3' col = 21   node = 'A' fld = 'LOCATION' cnv = '' )  " LOCATION
      ( scen = 'R3' col = 22   node = 'A' fld = 'DISTRICT' cnv = '' )  " CITY2
      ( scen = 'R3' col = 23   node = 'A' fld = 'POSTL_COD1' cnv = '' )  " POST_CODE1
      ( scen = 'R3' col = 24   node = 'A' fld = 'CITY' cnv = '' )  " CITY1
      ( scen = 'R3' col = 25   node = 'A' fld = 'COUNTRY' cnv = '' )  " COUNTRY
      ( scen = 'R3' col = 26   node = 'A' fld = 'REGION' cnv = '' )  " REGION
      ( scen = 'R3' col = 27   node = 'A' fld = 'LANGU' cnv = '' )  " LANGU
      ( scen = 'R3' col = 28   node = 'M' fld = 'TEL' cnv = '' )  " TEL_NUMBER
      ( scen = 'R3' col = 29   node = 'M' fld = 'MOB' cnv = '' )  " MOB_NUMBER
      ( scen = 'R3' col = 30   node = 'M' fld = 'FAX' cnv = '' )  " FAX_NUMBER
      ( scen = 'R3' col = 31   node = 'M' fld = 'SMT' cnv = '' )  " SMTP_ADDR
      ( scen = 'R3' col = 32   node = 'C' fld = 'KATR3' cnv = '' )  " KATR3
      ( scen = 'R3' col = 33   node = 'A' fld = 'TIME_ZONE' cnv = '' )  " TIME_ZONE
      ( scen = 'R3' col = 34   node = 'C' fld = 'J_1IPANNO' cnv = '' )  " J_1IPANNO
      ( scen = 'R3' col = 35   node = 'C' fld = 'STCD3' cnv = '' )  " STCD3
      ( scen = 'R3' col = 36   node = 'B' fld = 'AKONT' cnv = 'GL' )  " AKONT
      ( scen = 'R3' col = 37   node = 'B' fld = 'ZUAWA' cnv = '' )  " ZUAWA
      ( scen = 'R3' col = 38   node = 'B' fld = 'FDGRV' cnv = 'AL' )  " FDGRV
      ( scen = 'R3' col = 39   node = 'B' fld = 'VZSKZ' cnv = '' )  " VZSKZ
      ( scen = 'R3' col = 40   node = 'B' fld = 'ZINRT' cnv = '' )  " ZINRT
      ( scen = 'R3' col = 41   node = 'B' fld = 'ZTERM' cnv = '' )  " ZTERM
      ( scen = 'R3' col = 42   node = 'B' fld = 'XZVER' cnv = '' )  " XZVER
      ( scen = 'R3' col = 43   node = 'B' fld = 'ZWELS' cnv = '' )  " ZWELS
      ( scen = 'R3' col = 44   node = 'S' fld = 'BZIRK' cnv = '' )  " BZIRK
      ( scen = 'R3' col = 45   node = 'S' fld = 'VKBUR' cnv = '' )  " VKBUR
      ( scen = 'R3' col = 46   node = 'S' fld = 'VKGRP' cnv = '' )  " VKGRP
      ( scen = 'R3' col = 47   node = 'S' fld = 'KDGRP' cnv = '' )  " KDGRP
      ( scen = 'R3' col = 48   node = 'S' fld = 'KLABC' cnv = '' )  " KLABC
      ( scen = 'R3' col = 49   node = 'S' fld = 'WAERS' cnv = '' )  " WAERS
      ( scen = 'R3' col = 50   node = 'S' fld = 'KONDA' cnv = '' )  " KONDA
      ( scen = 'R3' col = 51   node = 'S' fld = 'KALKS' cnv = '' )  " KALKS
      ( scen = 'R3' col = 52   node = 'S' fld = 'VERSG' cnv = '' )  " VERSG
      ( scen = 'R3' col = 53   node = 'S' fld = 'LPRIO' cnv = '' )  " LPRIO
      ( scen = 'R3' col = 54   node = 'S' fld = 'KZAZU' cnv = '' )  " KZAZU
      ( scen = 'R3' col = 55   node = 'S' fld = 'VSBED' cnv = '' )  " VSBED
      ( scen = 'R3' col = 56   node = 'S' fld = 'VWERK' cnv = '' )  " VWERK
      ( scen = 'R3' col = 57   node = 'S' fld = 'ANTLF' cnv = '' )  " ANTLF
      ( scen = 'R3' col = 58   node = 'S' fld = 'ZTERM' cnv = '' )  " ZTERM1
      ( scen = 'R3' col = 59   node = 'S' fld = 'KTGRD' cnv = '' )  " KTGRD
      ( scen = 'R3' col = 60   node = 'T' fld = '#1' cnv = '' )  " TAXKD_01
      ( scen = 'R3' col = 65   node = 'S' fld = 'KVGR1' cnv = '' )  " KVGR1
      ( scen = 'R3' col = 66   node = 'S' fld = 'KVGR2' cnv = '' )  " KVGR2
      ( scen = 'R3' col = 67   node = 'S' fld = 'KVGR3' cnv = '' )  " KVGR3
      ( scen = 'R3' col = 68   node = 'S' fld = 'KVGR4' cnv = '' )  " KVGR4
      ( scen = 'R3' col = 69   node = 'S' fld = 'KVGR5' cnv = '' )  " KVGR5
      ( scen = 'R3' col = 70   node = 'Z' fld = 'WERKS' cnv = '' )  " WERKS
      ( scen = 'R3' col = 71   node = 'Z' fld = 'CUST_TRNST_DAYS' cnv = 'NM' )  " CUST_TRNST_DAYS
      ( scen = 'R3' col = 72   node = 'Z' fld = 'KMSUM' cnv = 'NM' )  " KMSUM
      ( scen = 'R3' col = 73   node = 'Z' fld = 'DRUGLICENSE1' cnv = '' )  " DRUGLICENSE1
      ( scen = 'R3' col = 74   node = 'Z' fld = 'DRUGLICENSE2' cnv = '' )  " DRUGLICENSE2
      ( scen = 'R3' col = 75   node = 'Z' fld = 'DL1_DL2_VALIDDT' cnv = 'DT' )  " DL1_DL2_VALIDDT
      ( scen = 'R3' col = 76   node = 'Z' fld = 'FOODSLICENSE' cnv = '' )  " FOODSLICENSE
      ( scen = 'R3' col = 77   node = 'Z' fld = 'FL_VALIDDT' cnv = 'DT' )  " FL_VALIDDT
      ( scen = 'R3' col = 78   node = 'Z' fld = 'SCHXNO' cnv = '' )  " SCHXNO
      ( scen = 'R3' col = 79   node = 'Z' fld = 'SCHX_VALIDDT' cnv = 'DT' )  " SCHX_VALIDDT
      ( scen = 'R3' col = 80   node = 'Z' fld = 'SCHXRNO' cnv = '' )  " SCHXRNO
      ( scen = 'R3' col = 81   node = 'Z' fld = 'SCHXR_VALIDDT' cnv = 'DT' )  " SCHXR_VALIDDT
      ( scen = 'R3' col = 82   node = 'Z' fld = 'RETAIL_LIC_NO' cnv = '' )  " RETAIL_LIC_NO
      ( scen = 'R3' col = 83   node = 'Z' fld = 'RETAIL_EXP' cnv = 'DT' )  " RETAIL_EXP
      ( scen = 'R3' col = 84   node = 'Z' fld = 'MFGLIC1NO' cnv = '' )  " MFGLIC1NO
      ( scen = 'R3' col = 85   node = 'Z' fld = 'MFGLIC2NO' cnv = '' )  " MFGLIC2NO
      ( scen = 'R3' col = 86   node = 'Z' fld = 'MFGLIC3NO' cnv = '' )  " MFGLIC3NO
      ( scen = 'R3' col = 87   node = 'Z' fld = 'BGYN' cnv = '' )  " BGYN
      ( scen = 'R3' col = 88   node = 'Z' fld = 'BG_NO' cnv = '' )  " BG_NO
      ( scen = 'R3' col = 89   node = 'Z' fld = 'BG_AMT' cnv = 'NM' )  " BG_AMT
      ( scen = 'R3' col = 90   node = 'Z' fld = 'CURRENCY' cnv = '' )  " CURRENCY
      ( scen = 'R3' col = 91   node = 'Z' fld = 'BG_ISS_DT' cnv = 'DT' )  " BG_ISS_DT
      ( scen = 'R3' col = 92   node = 'Z' fld = 'BG_EXP_DT' cnv = 'DT' )  " BG_EXP_DT
      ( scen = 'R3' col = 93   node = 'Z' fld = 'BG_ISS_BANK' cnv = '' )  " BG_ISS_BANK
      ( scen = 'R3' col = 94   node = 'Z' fld = 'AGGR_EXPDT' cnv = 'DT' )  " AGGR_EXPDT
      ( scen = 'R3' col = 95   node = 'Z' fld = 'APPOINT_DT' cnv = 'DT' )  " APPOINT_DT
      ( scen = 'R3' col = 96   node = 'Z' fld = 'KDGRP' cnv = '' )  " KDGRP1
      ( scen = 'R3' col = 97   node = 'Z' fld = 'AIOCD_CODE' cnv = '' )  " AIOCD_CODE
      ( scen = 'R3' col = 98   node = 'Z' fld = 'CUST_BNK_NAME' cnv = '' )  " CUST_BNK_NAME
      ( scen = 'R3' col = 99   node = 'Z' fld = 'DST_BOOKING' cnv = '' )  " DST_BOOKING
      ( scen = 'R3' col = 100  node = 'Z' fld = 'ZTROUT' cnv = '' )  " ZTROUT
      ( scen = 'R3' col = 101  node = 'Z' fld = 'EXTENSION' cnv = '' )  " EXTENSION
      ( scen = 'R3' col = 102  node = 'Z' fld = 'ZCROUT' cnv = '' )  " ZCROUT
      ( scen = 'R3' col = 103  node = 'Z' fld = 'GLN_URI_FORMAT' cnv = '' )  " GLN_URI_FORMAT
      ( scen = 'R3' col = 104  node = 'Z' fld = 'DUNS_NUMBER' cnv = '' )  " DUNS_NUMBER
      ( scen = 'R3' col = 105  node = 'Z' fld = 'DEA_FROM_DATE' cnv = 'DT' )  " DEA_FROM_DATE
      ( scen = 'R3' col = 106  node = 'Z' fld = 'DEA_TO_DATE' cnv = 'DT' )  " DEA_TO_DATE
      ( scen = 'R3' col = 107  node = 'Z' fld = 'ZIMP_LIC_MIA' cnv = '' )  " ZIMP_LIC_MIA
      ( scen = 'R3' col = 108  node = 'Z' fld = 'STATE_FROM_DATE' cnv = 'DT' )  " STATE_FROM_DATE
      ( scen = 'R3' col = 109  node = 'Z' fld = 'STATE_TO_DATE' cnv = 'DT' )  " STATE_TO_DATE
      ( scen = 'R3' col = 110  node = 'Z' fld = 'ZIMP_FROMDT_MIA' cnv = 'DT' )  " ZIMP_FROMDT_MIA
      ( scen = 'R3' col = 111  node = 'Z' fld = 'ZIMP_VALIDDT_MIA' cnv = 'DT' )  " ZIMP_VALIDDT_MIA
    ) TO rt.

    " R4 - SAGA customer (122 columns, 114 mapped)
    APPEND LINES OF VALUE tt_map(
      ( scen = 'R4' col = 4    node = 'K' fld = 'BUKRS' cnv = '' )  " BUKRS
      ( scen = 'R4' col = 5    node = 'K' fld = 'VKORG' cnv = '' )  " VKORG
      ( scen = 'R4' col = 6    node = 'K' fld = 'VTWEG' cnv = '' )  " VTWEG
      ( scen = 'R4' col = 7    node = 'K' fld = 'SPART' cnv = '' )  " SPART
      ( scen = 'R4' col = 8    node = 'K' fld = 'KTOKD' cnv = '' )  " KTOKD
      ( scen = 'R4' col = 10   node = 'A' fld = 'TITLE' cnv = 'TT' )  " TITLE_MEDI
      ( scen = 'R4' col = 11   node = 'A' fld = 'NAME' cnv = '' )  " NAME1
      ( scen = 'R4' col = 12   node = 'A' fld = 'NAME_2' cnv = '' )  " NAME2
      ( scen = 'R4' col = 13   node = 'A' fld = 'NAME_3' cnv = '' )  " NAME3
      ( scen = 'R4' col = 14   node = 'A' fld = 'NAME_4' cnv = '' )  " NAME4
      ( scen = 'R4' col = 15   node = 'A' fld = 'SORT1' cnv = '' )  " SORT1
      ( scen = 'R4' col = 16   node = 'A' fld = 'SORT2' cnv = '' )  " SORT2
      ( scen = 'R4' col = 17   node = 'A' fld = 'C_O_NAME' cnv = '' )  " NAME_CO
      ( scen = 'R4' col = 18   node = 'A' fld = 'STR_SUPPL1' cnv = '' )  " STR_SUPPL1
      ( scen = 'R4' col = 19   node = 'A' fld = 'STR_SUPPL2' cnv = '' )  " STR_SUPPL2
      ( scen = 'R4' col = 20   node = 'A' fld = 'STREET' cnv = '' )  " STREET
      ( scen = 'R4' col = 21   node = 'A' fld = 'STR_SUPPL3' cnv = '' )  " STR_SUPPL3
      ( scen = 'R4' col = 22   node = 'A' fld = 'LOCATION' cnv = '' )  " LOCATION
      ( scen = 'R4' col = 23   node = 'A' fld = 'DISTRICT' cnv = '' )  " CITY2
      ( scen = 'R4' col = 24   node = 'A' fld = 'POSTL_COD1' cnv = '' )  " POST_CODE1
      ( scen = 'R4' col = 25   node = 'A' fld = 'CITY' cnv = '' )  " CITY1
      ( scen = 'R4' col = 26   node = 'A' fld = 'COUNTRY' cnv = '' )  " COUNTRY
      ( scen = 'R4' col = 27   node = 'A' fld = 'REGION' cnv = '' )  " REGION
      ( scen = 'R4' col = 28   node = 'A' fld = 'TIME_ZONE' cnv = '' )  " TIME_ZONE
      ( scen = 'R4' col = 29   node = 'A' fld = 'LANGU' cnv = '' )  " LANGU
      ( scen = 'R4' col = 30   node = 'M' fld = 'TEL' cnv = '' )  " TEL_NUMBER
      ( scen = 'R4' col = 31   node = 'M' fld = 'MOB' cnv = '' )  " MOB_NUMBER
      ( scen = 'R4' col = 32   node = 'M' fld = 'FAX' cnv = '' )  " FAX_NUMBER
      ( scen = 'R4' col = 33   node = 'M' fld = 'SMT' cnv = '' )  " SMTP_ADDR
      ( scen = 'R4' col = 34   node = 'C' fld = 'KATR3' cnv = '' )  " KATR3
      ( scen = 'R4' col = 35   node = 'C' fld = 'KATR4' cnv = '' )  " KATR4
      ( scen = 'R4' col = 36   node = 'C' fld = 'LIFNR' cnv = 'AL' )  " LIFNR
      ( scen = 'R4' col = 37   node = 'C' fld = 'VBUND' cnv = 'AL' )  " VBUND
      ( scen = 'R4' col = 38   node = 'C' fld = 'KONZS' cnv = '' )  " KONZS
      ( scen = 'R4' col = 39   node = 'C' fld = 'STCD3' cnv = '' )  " STCD3
      ( scen = 'R4' col = 40   node = 'C' fld = 'STCD4' cnv = '' )  " STCD4
      ( scen = 'R4' col = 41   node = 'C' fld = 'STCD5' cnv = '' )  " STCD5
      ( scen = 'R4' col = 42   node = 'C' fld = 'STCEG' cnv = '' )  " STCEG
      ( scen = 'R4' col = 43   node = 'C' fld = 'J_1IPANNO' cnv = '' )  " J_1IPANNO
      ( scen = 'R4' col = 44   node = 'B' fld = 'AKONT' cnv = 'GL' )  " AKONT
      ( scen = 'R4' col = 45   node = 'B' fld = 'AKONT' cnv = 'GL' )  " AKONT
      ( scen = 'R4' col = 46   node = 'B' fld = 'ZUAWA' cnv = '' )  " ZUAWA
      ( scen = 'R4' col = 47   node = 'B' fld = 'VZSKZ' cnv = '' )  " VZSKZ
      ( scen = 'R4' col = 48   node = 'B' fld = 'ZINRT' cnv = '' )  " ZINRT
      ( scen = 'R4' col = 49   node = 'B' fld = 'ZTERM' cnv = '' )  " ZTERM
      ( scen = 'R4' col = 50   node = 'B' fld = 'XZVER' cnv = '' )  " XZVER
      ( scen = 'R4' col = 51   node = 'B' fld = 'ZWELS' cnv = '' )  " ZWELS
      ( scen = 'R4' col = 52   node = 'S' fld = 'BZIRK' cnv = '' )  " BZIRK
      ( scen = 'R4' col = 53   node = 'S' fld = 'AWAHR' cnv = '' )  " AWAHR
      ( scen = 'R4' col = 54   node = 'S' fld = 'VKBUR' cnv = '' )  " VKBUR
      ( scen = 'R4' col = 55   node = 'S' fld = 'VKGRP' cnv = '' )  " VKGRP
      ( scen = 'R4' col = 56   node = 'S' fld = 'KDGRP' cnv = '' )  " KDGRP
      ( scen = 'R4' col = 57   node = 'S' fld = 'KLABC' cnv = '' )  " KLABC
      ( scen = 'R4' col = 58   node = 'S' fld = 'WAERS' cnv = '' )  " WAERS
      ( scen = 'R4' col = 59   node = 'S' fld = 'KURST' cnv = '' )  " KURST
      ( scen = 'R4' col = 60   node = 'S' fld = 'KALKS' cnv = '' )  " KALKS
      ( scen = 'R4' col = 61   node = 'S' fld = 'VERSG' cnv = '' )  " VERSG
      ( scen = 'R4' col = 62   node = 'S' fld = 'LPRIO' cnv = '' )  " LPRIO
      ( scen = 'R4' col = 63   node = 'S' fld = 'KZAZU' cnv = '' )  " KZAZU
      ( scen = 'R4' col = 64   node = 'S' fld = 'VSBED' cnv = '' )  " VSBED
      ( scen = 'R4' col = 65   node = 'S' fld = 'VWERK' cnv = '' )  " VWERK
      ( scen = 'R4' col = 66   node = 'S' fld = 'ANTLF' cnv = '' )  " ANTLF
      ( scen = 'R4' col = 67   node = 'S' fld = 'INCO1' cnv = '' )  " INCO1
      ( scen = 'R4' col = 68   node = 'S' fld = 'INCO2' cnv = '' )  " INCO2
      ( scen = 'R4' col = 69   node = 'S' fld = 'ZTERM' cnv = '' )  " ZTERM1
      ( scen = 'R4' col = 70   node = 'S' fld = 'KTGRD' cnv = '' )  " KTGRD
      ( scen = 'R4' col = 71   node = 'T' fld = '#1' cnv = '' )  " TAXKD_01
      ( scen = 'R4' col = 76   node = 'S' fld = 'KVGR1' cnv = '' )  " KVGR1
      ( scen = 'R4' col = 77   node = 'S' fld = 'KVGR2' cnv = '' )  " KVGR2
      ( scen = 'R4' col = 78   node = 'S' fld = 'KVGR3' cnv = '' )  " KVGR3
      ( scen = 'R4' col = 79   node = 'S' fld = 'KVGR4' cnv = '' )  " KVGR4
      ( scen = 'R4' col = 80   node = 'S' fld = 'KVGR5' cnv = '' )  " KVGR5
      ( scen = 'R4' col = 81   node = 'Z' fld = 'WERKS' cnv = '' )  " WERKS
      ( scen = 'R4' col = 82   node = 'Z' fld = 'CUST_TRNST_DAYS' cnv = 'NM' )  " CUST_TRNST_DAYS
      ( scen = 'R4' col = 83   node = 'Z' fld = 'KMSUM' cnv = 'NM' )  " KMSUM
      ( scen = 'R4' col = 84   node = 'Z' fld = 'DRUGLICENSE1' cnv = '' )  " DRUGLICENSE1
      ( scen = 'R4' col = 85   node = 'Z' fld = 'DRUGLICENSE2' cnv = '' )  " DRUGLICENSE2
      ( scen = 'R4' col = 86   node = 'Z' fld = 'DL1_DL2_VALIDDT' cnv = 'DT' )  " DL1_DL2_VALIDDT
      ( scen = 'R4' col = 87   node = 'Z' fld = 'FOODSLICENSE' cnv = '' )  " FOODSLICENSE
      ( scen = 'R4' col = 88   node = 'Z' fld = 'FL_VALIDDT' cnv = 'DT' )  " FL_VALIDDT
      ( scen = 'R4' col = 89   node = 'Z' fld = 'SCHXNO' cnv = '' )  " SCHXNO
      ( scen = 'R4' col = 90   node = 'Z' fld = 'SCHX_VALIDDT' cnv = 'DT' )  " SCHX_VALIDDT
      ( scen = 'R4' col = 91   node = 'Z' fld = 'SCHXRNO' cnv = '' )  " SCHXRNO
      ( scen = 'R4' col = 92   node = 'Z' fld = 'SCHXR_VALIDDT' cnv = 'DT' )  " SCHXR_VALIDDT
      ( scen = 'R4' col = 93   node = 'Z' fld = 'RETAIL_LIC_NO' cnv = '' )  " RETAIL_LIC_NO
      ( scen = 'R4' col = 94   node = 'Z' fld = 'RETAIL_EXP' cnv = 'DT' )  " RETAIL_EXP
      ( scen = 'R4' col = 95   node = 'Z' fld = 'MFGLIC1NO' cnv = '' )  " MFGLIC1NO
      ( scen = 'R4' col = 96   node = 'Z' fld = 'MFGLIC2NO' cnv = '' )  " MFGLIC2NO
      ( scen = 'R4' col = 97   node = 'Z' fld = 'MFGLIC3NO' cnv = '' )  " MFGLIC3NO
      ( scen = 'R4' col = 98   node = 'Z' fld = 'BGYN' cnv = '' )  " BGYN
      ( scen = 'R4' col = 99   node = 'Z' fld = 'BG_NO' cnv = '' )  " BG_NO
      ( scen = 'R4' col = 100  node = 'Z' fld = 'BG_AMT' cnv = 'NM' )  " BG_AMT
      ( scen = 'R4' col = 101  node = 'Z' fld = 'CURRENCY' cnv = '' )  " CURRENCY
      ( scen = 'R4' col = 102  node = 'Z' fld = 'BG_ISS_DT' cnv = 'DT' )  " BG_ISS_DT
      ( scen = 'R4' col = 103  node = 'Z' fld = 'BG_EXP_DT' cnv = 'DT' )  " BG_EXP_DT
      ( scen = 'R4' col = 104  node = 'Z' fld = 'BG_ISS_BANK' cnv = '' )  " BG_ISS_BANK
      ( scen = 'R4' col = 105  node = 'Z' fld = 'AGGR_EXPDT' cnv = 'DT' )  " AGGR_EXPDT
      ( scen = 'R4' col = 106  node = 'Z' fld = 'APPOINT_DT' cnv = 'DT' )  " APPOINT_DT
      ( scen = 'R4' col = 107  node = 'Z' fld = 'KDGRP' cnv = '' )  " KDGRP1
      ( scen = 'R4' col = 108  node = 'Z' fld = 'AIOCD_CODE' cnv = '' )  " AIOCD_CODE
      ( scen = 'R4' col = 109  node = 'Z' fld = 'CUST_BNK_NAME' cnv = '' )  " CUST_BNK_NAME
      ( scen = 'R4' col = 110  node = 'Z' fld = 'DST_BOOKING' cnv = '' )  " DST_BOOKING
      ( scen = 'R4' col = 111  node = 'Z' fld = 'ZTROUT' cnv = '' )  " ZTROUT
      ( scen = 'R4' col = 112  node = 'Z' fld = 'EXTENSION' cnv = '' )  " EXTENSION
      ( scen = 'R4' col = 113  node = 'Z' fld = 'ZCROUT' cnv = '' )  " ZCROUT
      ( scen = 'R4' col = 114  node = 'Z' fld = 'GLN_URI_FORMAT' cnv = '' )  " GLN_URI_FORMAT
      ( scen = 'R4' col = 115  node = 'Z' fld = 'DUNS_NUMBER' cnv = '' )  " DUNS_NUMBER
      ( scen = 'R4' col = 116  node = 'Z' fld = 'DEA_FROM_DATE' cnv = 'DT' )  " DEA_FROM_DATE
      ( scen = 'R4' col = 117  node = 'Z' fld = 'DEA_TO_DATE' cnv = 'DT' )  " DEA_TO_DATE
      ( scen = 'R4' col = 118  node = 'Z' fld = 'ZIMP_LIC_MIA' cnv = '' )  " ZIMP_LIC_MIA
      ( scen = 'R4' col = 119  node = 'Z' fld = 'STATE_FROM_DATE' cnv = 'DT' )  " STATE_FROM_DATE
      ( scen = 'R4' col = 120  node = 'Z' fld = 'STATE_TO_DATE' cnv = 'DT' )  " STATE_TO_DATE
      ( scen = 'R4' col = 121  node = 'Z' fld = 'ZIMP_FROMDT_MIA' cnv = 'DT' )  " ZIMP_FROMDT_MIA
      ( scen = 'R4' col = 122  node = 'Z' fld = 'ZIMP_VALIDDT_MIA' cnv = 'DT' )  " ZIMP_VALIDDT_MIA
    ) TO rt.

    " R5 - credit Limit (18 columns, 11 mapped)
    APPEND LINES OF VALUE tt_map(
      ( scen = 'R5' col = 1    node = 'K' fld = 'KUNNR' cnv = 'AL' )  " Customer code
      ( scen = 'R5' col = 2    node = 'U' fld = 'SEGMENT' cnv = '' )  " KKBER
      ( scen = 'R5' col = 8    node = 'U' fld = 'LIMIT_MAIN' cnv = 'NM' )  " KLIMG
      ( scen = 'R5' col = 9    node = 'U' fld = 'LIMIT_SGM' cnv = 'NM' )  " KLIME
      ( scen = 'R5' col = 10   node = 'U' fld = 'CURRENCY' cnv = '' )  " WAERS
      ( scen = 'R5' col = 11   node = 'U' fld = 'LIMIT_SGM' cnv = 'NM' )  " KLIMK
      ( scen = 'R5' col = 12   node = 'U' fld = 'RISK_CLASS' cnv = '' )  " CTLPC
      ( scen = 'R5' col = 13   node = 'U' fld = 'XBLOCKED' cnv = '' )  " CRBLB
      ( scen = 'R5' col = 16   node = 'B' fld = 'ZTERM' cnv = '' )  " Payment Terms
      ( scen = 'R5' col = 17   node = 'S' fld = 'KVGR3' cnv = '' )  " Cust Grp 3
      ( scen = 'R5' col = 18   node = 'B' fld = 'VZSKZ' cnv = '' )  " Z1 Interest Indicator (cycle -> ZINRT, see handler)
    ) TO rt.

    " R6 - domestic customer US (75 columns, 73 mapped)
    APPEND LINES OF VALUE tt_map(
      ( scen = 'R6' col = 1    node = 'K' fld = 'KUNNR' cnv = 'AL' )  " Customer code
      ( scen = 'R6' col = 3    node = 'K' fld = 'BUKRS' cnv = '' )  " Company Code
      ( scen = 'R6' col = 4    node = 'K' fld = 'VKORG' cnv = '' )  " Sales Organization
      ( scen = 'R6' col = 5    node = 'K' fld = 'VTWEG' cnv = '' )  " Distribution Channel
      ( scen = 'R6' col = 6    node = 'K' fld = 'SPART' cnv = '' )  " Division
      ( scen = 'R6' col = 7    node = 'K' fld = 'KTOKD' cnv = '' )  " Customer Account Group
      ( scen = 'R6' col = 9    node = 'A' fld = 'TITLE' cnv = 'TT' )  " Title text
      ( scen = 'R6' col = 10   node = 'A' fld = 'NAME' cnv = '' )  " Name 1
      ( scen = 'R6' col = 11   node = 'A' fld = 'NAME_2' cnv = '' )  " Name 2
      ( scen = 'R6' col = 12   node = 'A' fld = 'NAME_3' cnv = '' )  " Name 3
      ( scen = 'R6' col = 13   node = 'A' fld = 'NAME_4' cnv = '' )  " Name 4
      ( scen = 'R6' col = 14   node = 'A' fld = 'SORT1' cnv = '' )  " Search Term 1
      ( scen = 'R6' col = 15   node = 'A' fld = 'SORT2' cnv = '' )  " Search Term 2
      ( scen = 'R6' col = 16   node = 'A' fld = 'STR_SUPPL1' cnv = '' )  " Street 2
      ( scen = 'R6' col = 17   node = 'A' fld = 'STR_SUPPL2' cnv = '' )  " Street 3
      ( scen = 'R6' col = 18   node = 'A' fld = 'STREET' cnv = '' )  " Street
      ( scen = 'R6' col = 19   node = 'A' fld = 'STR_SUPPL3' cnv = '' )  " Street 4
      ( scen = 'R6' col = 20   node = 'A' fld = 'LOCATION' cnv = '' )  " Street 5
      ( scen = 'R6' col = 21   node = 'A' fld = 'DISTRICT' cnv = '' )  " District
      ( scen = 'R6' col = 22   node = 'A' fld = 'POSTL_COD1' cnv = '' )  " City postal code
      ( scen = 'R6' col = 23   node = 'A' fld = 'CITY' cnv = '' )  " City
      ( scen = 'R6' col = 24   node = 'A' fld = 'COUNTRY' cnv = '' )  " Country Key
      ( scen = 'R6' col = 25   node = 'A' fld = 'REGION' cnv = '' )  " Region (State, Province, County)
      ( scen = 'R6' col = 26   node = 'A' fld = 'LANGU' cnv = '' )  " Language Key
      ( scen = 'R6' col = 27   node = 'M' fld = 'TEL' cnv = '' )  " First telephone no.: dialling cod
      ( scen = 'R6' col = 28   node = 'M' fld = 'MOB' cnv = '' )  " First Mobile Telephone No.: Diali
      ( scen = 'R6' col = 29   node = 'M' fld = 'FAX' cnv = '' )  " First fax no.: dialling code+numb
      ( scen = 'R6' col = 30   node = 'M' fld = 'SMT' cnv = '' )  " E-Mail Address
      ( scen = 'R6' col = 31   node = 'C' fld = 'KONZS' cnv = '' )  " Group key
      ( scen = 'R6' col = 32   node = 'C' fld = 'KATR3' cnv = '' )  " Attribute 3
      ( scen = 'R6' col = 33   node = 'C' fld = 'KATR4' cnv = '' )  " Attribute 4
      ( scen = 'R6' col = 34   node = 'C' fld = 'CIVVE' cnv = '' )  " ID for mainly non-military use
      ( scen = 'R6' col = 35   node = 'B' fld = 'AKONT' cnv = 'GL' )  " Reconciliation Account in General
      ( scen = 'R6' col = 36   node = 'B' fld = 'ZUAWA' cnv = '' )  " Key for sorting according to assi
      ( scen = 'R6' col = 37   node = 'B' fld = 'FDGRV' cnv = 'AL' )  " Planning group
      ( scen = 'R6' col = 38   node = 'B' fld = 'VZSKZ' cnv = '' )  " Interest calculation indicator
      ( scen = 'R6' col = 39   node = 'B' fld = 'ZINRT' cnv = '' )  " Interest calculation frequency in
      ( scen = 'R6' col = 40   node = 'B' fld = 'ZTERM' cnv = '' )  " Terms of Payment Key
      ( scen = 'R6' col = 41   node = 'B' fld = 'XZVER' cnv = '' )  " Indicator: Record Payment History
      ( scen = 'R6' col = 42   node = 'B' fld = 'ZWELS' cnv = '' )  " List of the Payment Methods to be
      ( scen = 'R6' col = 43   node = 'S' fld = 'BZIRK' cnv = '' )  " Sales district
      ( scen = 'R6' col = 44   node = 'S' fld = 'AWAHR' cnv = '' )  " Order probability of the item
      ( scen = 'R6' col = 45   node = 'S' fld = 'VKBUR' cnv = '' )  " Sales Office
      ( scen = 'R6' col = 46   node = 'S' fld = 'VKGRP' cnv = '' )  " Sales Group
      ( scen = 'R6' col = 47   node = 'S' fld = 'KDGRP' cnv = '' )  " Customer group
      ( scen = 'R6' col = 48   node = 'S' fld = 'KLABC' cnv = '' )  " Customer classification (ABC anal
      ( scen = 'R6' col = 49   node = 'S' fld = 'WAERS' cnv = '' )  " Currency
      ( scen = 'R6' col = 50   node = 'S' fld = 'KALKS' cnv = '' )  " Pricing procedure assigned to thi
      ( scen = 'R6' col = 51   node = 'S' fld = 'VERSG' cnv = '' )  " Customer Statistics Group
      ( scen = 'R6' col = 52   node = 'S' fld = 'LPRIO' cnv = '' )  " Delivery Priority
      ( scen = 'R6' col = 53   node = 'S' fld = 'KZAZU' cnv = '' )  " Order Combination Indicator
      ( scen = 'R6' col = 54   node = 'S' fld = 'VSBED' cnv = '' )  " Shipping Conditions
      ( scen = 'R6' col = 55   node = 'S' fld = 'VWERK' cnv = '' )  " Delivering Plant (Own or External
      ( scen = 'R6' col = 56   node = 'S' fld = 'ANTLF' cnv = '' )  " Maximum Number of Partial Deliver
      ( scen = 'R6' col = 57   node = 'S' fld = 'INCO1' cnv = '' )  " Incoterms (Part 1)
      ( scen = 'R6' col = 58   node = 'S' fld = 'INCO2' cnv = '' )  " Incoterms (Part 2)
      ( scen = 'R6' col = 59   node = 'B' fld = 'ZTERM' cnv = '' )  " Terms of Payment Key
      ( scen = 'R6' col = 60   node = 'S' fld = 'KTGRD' cnv = '' )  " Customer Account Assignment Group
      ( scen = 'R6' col = 61   node = 'T' fld = 'UTXJ' cnv = '' )  " Tax classification for customer
      ( scen = 'R6' col = 62   node = 'T' fld = 'UTX2' cnv = '' )  " Tax classification for customer
      ( scen = 'R6' col = 63   node = 'T' fld = 'UTX3' cnv = '' )  " Tax classification for customer
      ( scen = 'R6' col = 64   node = 'T' fld = 'MWST' cnv = '' )  " Tax classification for customer
      ( scen = 'R6' col = 65   node = 'S' fld = 'KVGR1' cnv = '' )  " Customer group 1
      ( scen = 'R6' col = 66   node = 'S' fld = 'KVGR2' cnv = '' )  " Customer group 2
      ( scen = 'R6' col = 67   node = 'S' fld = 'KVGR3' cnv = '' )  " Customer group 3
      ( scen = 'R6' col = 68   node = 'S' fld = 'KVGR4' cnv = '' )  " Customer group 4
      ( scen = 'R6' col = 69   node = 'S' fld = 'KVGR5' cnv = '' )  " Customer group 5
      ( scen = 'R6' col = 70   node = 'Z' fld = 'WERKS' cnv = '' )  " Plant
      ( scen = 'R6' col = 71   node = 'Z' fld = 'DRUGLICENSE1' cnv = '' )  " 20B. Lic. No
      ( scen = 'R6' col = 72   node = 'Z' fld = 'DL1_ISSUEDT' cnv = 'DT' )  " 20B Issue Date
      ( scen = 'R6' col = 73   node = 'Z' fld = 'DL1_VALIDDT' cnv = 'DT' )  " 20B Expiry Date
      ( scen = 'R6' col = 74   node = 'Z' fld = 'DEA_FROM_DATE' cnv = 'DT' )  " DEA From Date
      ( scen = 'R6' col = 75   node = 'Z' fld = 'DEA_TO_DATE' cnv = 'DT' )  " DEA To Date
    ) TO rt.

    " R7 - ship to party US (75 columns, 73 mapped)
    APPEND LINES OF VALUE tt_map(
      ( scen = 'R7' col = 1    node = 'K' fld = 'KUNNR' cnv = 'AL' )  " Customer code
      ( scen = 'R7' col = 3    node = 'K' fld = 'BUKRS' cnv = '' )  " Company Code
      ( scen = 'R7' col = 4    node = 'K' fld = 'VKORG' cnv = '' )  " Sales Organization
      ( scen = 'R7' col = 5    node = 'K' fld = 'VTWEG' cnv = '' )  " Distribution Channel
      ( scen = 'R7' col = 6    node = 'K' fld = 'SPART' cnv = '' )  " Division
      ( scen = 'R7' col = 7    node = 'K' fld = 'KTOKD' cnv = '' )  " Customer Account Group
      ( scen = 'R7' col = 9    node = 'A' fld = 'TITLE' cnv = 'TT' )  " Title text
      ( scen = 'R7' col = 10   node = 'A' fld = 'NAME' cnv = '' )  " Name 1
      ( scen = 'R7' col = 11   node = 'A' fld = 'NAME_2' cnv = '' )  " Name 2
      ( scen = 'R7' col = 12   node = 'A' fld = 'NAME_3' cnv = '' )  " Name 3
      ( scen = 'R7' col = 13   node = 'A' fld = 'NAME_4' cnv = '' )  " Name 4
      ( scen = 'R7' col = 14   node = 'A' fld = 'SORT1' cnv = '' )  " Search Term 1
      ( scen = 'R7' col = 15   node = 'A' fld = 'SORT2' cnv = '' )  " Search Term 2
      ( scen = 'R7' col = 16   node = 'A' fld = 'STR_SUPPL1' cnv = '' )  " Street 2
      ( scen = 'R7' col = 17   node = 'A' fld = 'STR_SUPPL2' cnv = '' )  " Street 3
      ( scen = 'R7' col = 18   node = 'A' fld = 'STREET' cnv = '' )  " Street
      ( scen = 'R7' col = 19   node = 'A' fld = 'STR_SUPPL3' cnv = '' )  " Street 4
      ( scen = 'R7' col = 20   node = 'A' fld = 'LOCATION' cnv = '' )  " Street 5
      ( scen = 'R7' col = 21   node = 'A' fld = 'DISTRICT' cnv = '' )  " District
      ( scen = 'R7' col = 22   node = 'A' fld = 'POSTL_COD1' cnv = '' )  " City postal code
      ( scen = 'R7' col = 23   node = 'A' fld = 'CITY' cnv = '' )  " City
      ( scen = 'R7' col = 24   node = 'A' fld = 'COUNTRY' cnv = '' )  " Country Key
      ( scen = 'R7' col = 25   node = 'A' fld = 'REGION' cnv = '' )  " Region (State, Province, County)
      ( scen = 'R7' col = 26   node = 'A' fld = 'LANGU' cnv = '' )  " Language Key
      ( scen = 'R7' col = 27   node = 'M' fld = 'TEL' cnv = '' )  " First telephone no.: dialling cod
      ( scen = 'R7' col = 28   node = 'M' fld = 'MOB' cnv = '' )  " First Mobile Telephone No.: Diali
      ( scen = 'R7' col = 29   node = 'M' fld = 'FAX' cnv = '' )  " First fax no.: dialling code+numb
      ( scen = 'R7' col = 30   node = 'M' fld = 'SMT' cnv = '' )  " E-Mail Address
      ( scen = 'R7' col = 31   node = 'C' fld = 'KONZS' cnv = '' )  " Group key
      ( scen = 'R7' col = 32   node = 'C' fld = 'KATR3' cnv = '' )  " Attribute 3
      ( scen = 'R7' col = 33   node = 'C' fld = 'KATR4' cnv = '' )  " Attribute 4
      ( scen = 'R7' col = 34   node = 'C' fld = 'CIVVE' cnv = '' )  " ID for mainly non-military use
      ( scen = 'R7' col = 35   node = 'B' fld = 'AKONT' cnv = 'GL' )  " Reconciliation Account in General
      ( scen = 'R7' col = 36   node = 'B' fld = 'ZUAWA' cnv = '' )  " Key for sorting according to assi
      ( scen = 'R7' col = 37   node = 'B' fld = 'FDGRV' cnv = 'AL' )  " Planning group
      ( scen = 'R7' col = 38   node = 'B' fld = 'VZSKZ' cnv = '' )  " Interest calculation indicator
      ( scen = 'R7' col = 39   node = 'B' fld = 'ZINRT' cnv = '' )  " Interest calculation frequency in
      ( scen = 'R7' col = 40   node = 'B' fld = 'ZTERM' cnv = '' )  " Terms of Payment Key
      ( scen = 'R7' col = 41   node = 'B' fld = 'XZVER' cnv = '' )  " Indicator: Record Payment History
      ( scen = 'R7' col = 42   node = 'B' fld = 'ZWELS' cnv = '' )  " List of the Payment Methods to be
      ( scen = 'R7' col = 43   node = 'S' fld = 'BZIRK' cnv = '' )  " Sales district
      ( scen = 'R7' col = 44   node = 'S' fld = 'AWAHR' cnv = '' )  " Order probability of the item
      ( scen = 'R7' col = 45   node = 'S' fld = 'VKBUR' cnv = '' )  " Sales Office
      ( scen = 'R7' col = 46   node = 'S' fld = 'VKGRP' cnv = '' )  " Sales Group
      ( scen = 'R7' col = 47   node = 'S' fld = 'KDGRP' cnv = '' )  " Customer group
      ( scen = 'R7' col = 48   node = 'S' fld = 'KLABC' cnv = '' )  " Customer classification (ABC anal
      ( scen = 'R7' col = 49   node = 'S' fld = 'WAERS' cnv = '' )  " Currency
      ( scen = 'R7' col = 50   node = 'S' fld = 'KALKS' cnv = '' )  " Pricing procedure assigned to thi
      ( scen = 'R7' col = 51   node = 'S' fld = 'VERSG' cnv = '' )  " Customer Statistics Group
      ( scen = 'R7' col = 52   node = 'S' fld = 'LPRIO' cnv = '' )  " Delivery Priority
      ( scen = 'R7' col = 53   node = 'S' fld = 'KZAZU' cnv = '' )  " Order Combination Indicator
      ( scen = 'R7' col = 54   node = 'S' fld = 'VSBED' cnv = '' )  " Shipping Conditions
      ( scen = 'R7' col = 55   node = 'S' fld = 'VWERK' cnv = '' )  " Delivering Plant (Own or External
      ( scen = 'R7' col = 56   node = 'S' fld = 'ANTLF' cnv = '' )  " Maximum Number of Partial Deliver
      ( scen = 'R7' col = 57   node = 'S' fld = 'INCO1' cnv = '' )  " Incoterms (Part 1)
      ( scen = 'R7' col = 58   node = 'S' fld = 'INCO2' cnv = '' )  " Incoterms (Part 2)
      ( scen = 'R7' col = 59   node = 'B' fld = 'ZTERM' cnv = '' )  " Terms of Payment Key
      ( scen = 'R7' col = 60   node = 'S' fld = 'KTGRD' cnv = '' )  " Customer Account Assignment Group
      ( scen = 'R7' col = 61   node = 'T' fld = 'UTXJ' cnv = '' )  " Tax classification for customer
      ( scen = 'R7' col = 62   node = 'T' fld = 'UTX2' cnv = '' )  " Tax classification for customer
      ( scen = 'R7' col = 63   node = 'T' fld = 'UTX3' cnv = '' )  " Tax classification for customer
      ( scen = 'R7' col = 64   node = 'T' fld = 'MWST' cnv = '' )  " Tax classification for customer
      ( scen = 'R7' col = 65   node = 'S' fld = 'KVGR1' cnv = '' )  " Customer group 1
      ( scen = 'R7' col = 66   node = 'S' fld = 'KVGR2' cnv = '' )  " Customer group 2
      ( scen = 'R7' col = 67   node = 'S' fld = 'KVGR3' cnv = '' )  " Customer group 3
      ( scen = 'R7' col = 68   node = 'S' fld = 'KVGR4' cnv = '' )  " Customer group 4
      ( scen = 'R7' col = 69   node = 'S' fld = 'KVGR5' cnv = '' )  " Customer group 5
      ( scen = 'R7' col = 70   node = 'Z' fld = 'WERKS' cnv = '' )  " Plant
      ( scen = 'R7' col = 71   node = 'Z' fld = 'DRUGLICENSE1' cnv = '' )  " 20B. Lic. No
      ( scen = 'R7' col = 72   node = 'Z' fld = 'DL1_ISSUEDT' cnv = 'DT' )  " 20B Issue Date
      ( scen = 'R7' col = 73   node = 'Z' fld = 'DL1_VALIDDT' cnv = 'DT' )  " 20B Expiry Date
      ( scen = 'R7' col = 74   node = 'Z' fld = 'DEA_FROM_DATE' cnv = 'DT' )  " DEA From Date
      ( scen = 'R7' col = 75   node = 'Z' fld = 'DEA_TO_DATE' cnv = 'DT' )  " DEA To Date
    ) TO rt.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_CVIS - the CVI/BP API call
*----------------------------------------------------------------------*
CLASS lcl_cvis DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_log TYPE REF TO lcl_log.
    METHODS post
      IMPORTING is_data   TYPE cvis_ei_extern
                iv_row    TYPE i
                iv_kunnr  TYPE clike
      RETURNING VALUE(rv) TYPE abap_bool.
  PRIVATE SECTION.
    DATA mo_log TYPE REF TO lcl_log.
ENDCLASS.

CLASS lcl_cvis IMPLEMENTATION.

  METHOD constructor.
    mo_log = io_log.
  ENDMETHOD.

  METHOD post.
    rv = abap_true.

    " ---- 1. validate. ET_RETURN_MAP carries BAPISTRUCNAME / BAPIFLDNM so
    "         the user can be pointed at the offending template column.
    DATA lt_map TYPE mdg_bs_bp_msgmap_t.
    TRY.
        cl_md_bp_maintain=>validate_single(
          EXPORTING i_data        = is_data
          IMPORTING et_return_map = lt_map ).
      CATCH cx_root INTO DATA(lx1).
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                     iv_text = |Validation failed: { lx1->get_text( ) }| ).
        rv = abap_false.
        RETURN.
    ENDTRY.

    LOOP AT lt_map INTO DATA(ls_map) WHERE type CA 'EAX'.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr
                   iv_type  = ls_map-type
                   iv_text  = ls_map-message
                   iv_struc = ls_map-bapistrucname
                   iv_fld   = ls_map-bapifldnm ).
      rv = abap_false.
    ENDLOOP.
    IF rv = abap_false.
      RETURN.
    ENDIF.

    " ---- 2. maintain. I_TEST_RUN is honoured by the API itself. --------
    DATA: lt_data TYPE cvis_ei_extern_t,
          lt_ret  TYPE bapiretm.
    APPEND is_data TO lt_data.

    TRY.
        cl_md_bp_maintain=>maintain(
          EXPORTING i_data     = lt_data
                    i_test_run = p_test
          IMPORTING e_return   = lt_ret ).
      CATCH cx_root INTO DATA(lx2).
        ROLLBACK WORK.
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                     iv_text = |Maintain failed: { lx2->get_text( ) }| ).
        rv = abap_false.
        RETURN.
    ENDTRY.

    " BAPIRETM lines carry a nested message table. Read it generically so a
    " release-dependent component name cannot break the program.
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
          mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr
                       iv_type = ls_r2-type iv_text = ls_r2-message
                       iv_fld  = ls_r2-field ).
          rv = abap_false.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF rv = abap_false.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    IF p_test = abap_true.
      ROLLBACK WORK.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'S'
                   iv_text = 'Test run OK - customer would be posted' ).
    ELSE.
      COMMIT WORK AND WAIT.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'S'
                   iv_text = 'Customer posted' ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_LIC - the authorised direct write to ZSD_LICENSE_CHK
*
*   Key is MANDT + KUNNR only. WERKS is a normal field, so one customer
*   has exactly one licence record.
*
*   The table has 79 fields; the templates carry 26. The row is therefore
*   READ first and only the columns the template actually carries are
*   overlaid. A blank template cell leaves the stored value alone - it does
*   not blank it. Use #BLANK# in a cell to clear a field on purpose.
*   Change logging is on for this table, so a wrong write is permanent and
*   visible in the change log; this merge is what stops that happening.
*----------------------------------------------------------------------*
CLASS lcl_lic DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_log TYPE REF TO lcl_log.

    METHODS set
      IMPORTING iv_fld  TYPE clike
                iv_val  TYPE clike
                iv_cnv  TYPE clike
                iv_row  TYPE i.

    METHODS save
      IMPORTING iv_kunnr TYPE kunnr
                iv_row   TYPE i.

    METHODS reset.
    METHODS touched RETURNING VALUE(rv) TYPE abap_bool.
  PRIVATE SECTION.
    DATA mo_log    TYPE REF TO lcl_log.
    DATA ms_new    TYPE zsd_license_chk.
    DATA mt_fld    TYPE SORTED TABLE OF fieldname WITH UNIQUE KEY table_line.
ENDCLASS.

CLASS lcl_lic IMPLEMENTATION.

  METHOD constructor.
    mo_log = io_log.
  ENDMETHOD.

  METHOD reset.
    CLEAR ms_new.
    CLEAR mt_fld.
  ENDMETHOD.

  METHOD touched.
    rv = xsdbool( mt_fld IS NOT INITIAL ).
  ENDMETHOD.

  METHOD set.
    ASSIGN COMPONENT iv_fld OF STRUCTURE ms_new TO FIELD-SYMBOL(<lv>).
    IF sy-subrc <> 0.
      mo_log->add( iv_row = iv_row iv_type = 'E'
                   iv_struc = 'ZSD_LICENSE_CHK' iv_fld = iv_fld
                   iv_text = |Field { iv_fld } does not exist on ZSD_LICENSE_CHK| ).
      RETURN.
    ENDIF.

    DATA(lv_in) = condense( CONV string( iv_val ) ).
    IF lv_in = gc_clear.
      CLEAR <lv>.
      INSERT CONV fieldname( iv_fld ) INTO TABLE mt_fld.
      RETURN.
    ENDIF.
    IF lv_in IS INITIAL.
      RETURN.
    ENDIF.

    CASE iv_cnv.
      WHEN 'DT'.
        DATA(lv_d) = lcl_util=>to_date( lv_in ).
        IF lv_d IS INITIAL.
          mo_log->add( iv_row = iv_row iv_type = 'E'
                       iv_struc = 'ZSD_LICENSE_CHK' iv_fld = iv_fld
                       iv_text = |"{ lv_in }" is not a valid date| ).
          RETURN.
        ENDIF.
        <lv> = lv_d.
      WHEN 'NM'.
        <lv> = lcl_util=>to_int( lv_in ).
      WHEN OTHERS.
        <lv> = lv_in.
    ENDCASE.
    INSERT CONV fieldname( iv_fld ) INTO TABLE mt_fld.
  ENDMETHOD.

  METHOD save.
    IF touched( ) = abap_false.
      RETURN.
    ENDIF.

    " ---- read the current row -----------------------------------------
    DATA ls_db TYPE zsd_license_chk.
    SELECT SINGLE * FROM zsd_license_chk
      WHERE kunnr = @iv_kunnr
      INTO @ls_db.
    DATA(lv_exists) = xsdbool( sy-subrc = 0 ).

    " ---- merge: only the columns this template carries -----------------
    ls_db-kunnr = iv_kunnr.
    LOOP AT mt_fld INTO DATA(lv_f).
      ASSIGN COMPONENT lv_f OF STRUCTURE ms_new TO FIELD-SYMBOL(<lv_s>).
      CHECK sy-subrc = 0.
      ASSIGN COMPONENT lv_f OF STRUCTURE ls_db  TO FIELD-SYMBOL(<lv_t>).
      CHECK sy-subrc = 0.
      <lv_t> = <lv_s>.
    ENDLOOP.

    IF p_test = abap_true.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'S'
                   iv_struc = 'ZSD_LICENSE_CHK'
                   iv_text = COND string(
                     WHEN lv_exists = abap_true
                     THEN |Test run OK - would update { lines( mt_fld ) } licence field(s)|
                     ELSE |Test run OK - would create the licence record| ) ).
      RETURN.
    ENDIF.

    " Authorised direct write - see the header comment of this report.
    MODIFY zsd_license_chk FROM ls_db.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'S'
                   iv_struc = 'ZSD_LICENSE_CHK'
                   iv_text = COND string(
                     WHEN lv_exists = abap_true
                     THEN |Licence record updated ({ lines( mt_fld ) } field(s))|
                     ELSE 'Licence record created' ) ).
    ELSE.
      ROLLBACK WORK.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                   iv_struc = 'ZSD_LICENSE_CHK'
                   iv_text = 'Licence record could not be written' ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_CREDIT - FSCM credit limits
*
*   KNKK/KNKA are obsolete under S/4. The limit lives on UKMBP_CMS_SGM per
*   credit segment, the risk class on UKMBP_CMS per business partner.
*   UKM_KKBER2SGM maps credit control area -> segment 1:1 in this system,
*   and segment 0000 is the main segment, which is where the old
*   "total limit across all control areas" (KNKA-KLIMG) belongs.
*
*   Signatures verified against the class documentation:
*     CL_UKM_FACADE=>CREATE( i_activity ) RETURNING ro_facade      (static)
*     ->GET_BUPA_FACTORY( ) RETURNING ro_bupa_factory
*     CL_UKM_BUPA_FACTORY->GET_CREDIT_ACCOUNT( i_partner i_credit_sgmnt )
*     CL_UKM_ACCOUNT->GET_BP_CMS_SGM( IMPORTING es_bp_cms_sgm )
*     CL_UKM_ACCOUNT->SET_BP_CMS_SGM( is_bp_cms_sgm )
*     CL_UKM_BUSINESS_PARTNER->GET_BP_CMS / ->SET_BP_CMS
*     CL_UKM_BUPA_FACTORY->SAVE_ALL( i_testrun i_upd_task ... )
*----------------------------------------------------------------------*
CLASS lcl_credit DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_log TYPE REF TO lcl_log.
    METHODS apply
      IMPORTING iv_kunnr   TYPE kunnr
                iv_sgmnt   TYPE char10
                iv_limit   TYPE ty_dec
                iv_has_lim TYPE abap_bool
                iv_block   TYPE clike
                iv_risk    TYPE clike
                iv_row     TYPE i.
  PRIVATE SECTION.
    DATA mo_log TYPE REF TO lcl_log.
ENDCLASS.

CLASS lcl_credit IMPLEMENTATION.

  METHOD constructor.
    mo_log = io_log.
  ENDMETHOD.

  METHOD apply.
    IF iv_sgmnt IS INITIAL.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                   iv_text = 'No credit segment for this credit control area' ).
      RETURN.
    ENDIF.

    TRY.
        " CL_UKM_FACADE is a singleton built through CREATE, not GET_INSTANCE.
        " I_ACTIVITY drives locking and eventing; BP_MAINTENANCE ('MAINTAIN')
        " is the activity for maintaining credit master data.
        DATA(lo_facade)  = cl_ukm_facade=>create(
                             i_activity = cl_ukm_cnst_eventing=>bp_maintenance ).
        DATA(lo_factory) = lo_facade->get_bupa_factory( ).

        DATA(lo_account) = lo_factory->get_credit_account(
                             i_partner      = CONV bu_partner( iv_kunnr )
                             i_credit_sgmnt = CONV ukm_credit_sgmnt( iv_sgmnt ) ).

        DATA ls_sgm TYPE ukm_s_bp_cms_sgm.
        lo_account->get_bp_cms_sgm( IMPORTING es_bp_cms_sgm = ls_sgm ).

        IF iv_has_lim = abap_true.
          ls_sgm-credit_limit   = iv_limit.
          ls_sgm-limit_chg_date = sy-datum.
        ENDIF.
        IF iv_block IS NOT INITIAL.
          ls_sgm-xblocked = COND #( WHEN iv_block = 'X' OR iv_block = 'Y'
                                    THEN abap_true ELSE abap_false ).
        ENDIF.
        lo_account->set_bp_cms_sgm( EXPORTING is_bp_cms_sgm = ls_sgm ).

        IF iv_risk IS NOT INITIAL.
          DATA(lo_bp) = lo_factory->get_business_partner(
                          i_partner = CONV bu_partner( iv_kunnr ) ).
          DATA ls_cms TYPE ukm_s_bp_cms.
          lo_bp->get_bp_cms( IMPORTING es_bp_cms = ls_cms ).
          ls_cms-risk_class = iv_risk.
          lo_bp->set_bp_cms( EXPORTING is_bp_cms = ls_cms ).
        ENDIF.

        " SAVE_ALL has its own I_TESTRUN, so a test run still goes through
        " the API and still validates - it simply does not persist.
        lo_factory->save_all( i_testrun  = p_test
                              i_upd_task = abap_false ).

        IF p_test = abap_true.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'S'
                       iv_text = |Test run OK - segment { iv_sgmnt } would be updated| ).
        ELSE.
          " SAVE_ALL alone does not commit - the BAPI commit is required.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = abap_true.
          mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'S'
                       iv_text = |Credit data updated for segment { iv_sgmnt }| ).
        ENDIF.

      CATCH cx_root INTO DATA(lx).
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                     iv_text = |Credit update failed: { lx->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_ENGINE - one generic engine driven by the column map
*
*   The seven tabs differ only in which column holds which field, so a
*   metadata-driven engine is used instead of seven hand-written handlers.
*   Adding a column is one line in LCL_MAP, not new code.
*----------------------------------------------------------------------*
CLASS lcl_engine DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_scen TYPE char2
                io_log  TYPE REF TO lcl_log.
    METHODS sheet RETURNING VALUE(rv) TYPE string.
    METHODS run   IMPORTING it_row TYPE tt_row.

  PRIVATE SECTION.
    DATA mv_scen  TYPE char2.
    DATA mo_log   TYPE REF TO lcl_log.
    DATA mo_cvis  TYPE REF TO lcl_cvis.
    DATA mo_lic   TYPE REF TO lcl_lic.
    DATA mo_cred  TYPE REF TO lcl_credit.
    DATA mt_map   TYPE tt_map.

    " Moves IV_VAL into component IV_FLD of CS_DATA and flags the matching
    " component of CS_DATAX. RETURNING cannot be combined with CHANGING, so
    " this reports problems through the log instead of a return code.
    METHODS set_comp
      IMPORTING iv_fld   TYPE clike
                iv_val   TYPE clike
                iv_cnv   TYPE clike
                iv_row   TYPE i
                iv_struc TYPE clike
      CHANGING  cs_data  TYPE any
                cs_datax TYPE any.

    METHODS master IMPORTING is_row TYPE ty_row.
    METHODS credit IMPORTING is_row TYPE ty_row.
ENDCLASS.

CLASS lcl_engine IMPLEMENTATION.

  METHOD constructor.
    mv_scen = iv_scen.
    mo_log  = io_log.
    mt_map  = lcl_map=>for( iv_scen ).
    mo_cvis = NEW lcl_cvis( io_log ).
    mo_lic  = NEW lcl_lic( io_log ).
    mo_cred = NEW lcl_credit( io_log ).
  ENDMETHOD.

  METHOD sheet.
    rv = SWITCH string( mv_scen
           WHEN 'R1' THEN gc_sh_ind
           WHEN 'R2' THEN gc_sh_exp
           WHEN 'R3' THEN gc_sh_mar
           WHEN 'R4' THEN gc_sh_saga
           WHEN 'R5' THEN gc_sh_cred
           WHEN 'R6' THEN gc_sh_us
           WHEN 'R7' THEN gc_sh_ship
           ELSE           gc_sh_ind ).
  ENDMETHOD.

  METHOD run.
    LOOP AT it_row INTO DATA(ls_row).
      IF mv_scen = 'R5'.
        credit( ls_row ).
      ELSE.
        master( ls_row ).
      ENDIF.
      IF p_stop = abap_true AND mo_log->has_error( ls_row-row ) = abap_true.
        mo_log->add( iv_row = ls_row-row iv_type = 'W'
                     iv_text = 'Stopped at the first faulty row' ).
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_comp.
    ASSIGN COMPONENT iv_fld OF STRUCTURE cs_data TO FIELD-SYMBOL(<lv_t>).
    IF sy-subrc <> 0.
      mo_log->add( iv_row = iv_row iv_type = 'E'
                   iv_struc = iv_struc iv_fld = iv_fld
                   iv_text = |Field { iv_fld } does not exist on { iv_struc }| ).
      RETURN.
    ENDIF.

    DATA(lv_in) = condense( CONV string( iv_val ) ).

    IF lv_in = gc_clear.
      CLEAR <lv_t>.
    ELSEIF lv_in IS INITIAL.
      " A blank cell means "leave alone", so no DATAX flag is set.
      RETURN.
    ELSE.
      CASE iv_cnv.
        WHEN 'AL'.
          <lv_t> = lcl_util=>alpha( lv_in ).
        WHEN 'DT'.
          DATA(lv_d) = lcl_util=>to_date( lv_in ).
          IF lv_d IS INITIAL.
            mo_log->add( iv_row = iv_row iv_type = 'E'
                         iv_struc = iv_struc iv_fld = iv_fld
                         iv_text = |"{ lv_in }" is not a valid date| ).
            RETURN.
          ENDIF.
          <lv_t> = lv_d.
        WHEN 'NM'.
          <lv_t> = lcl_util=>to_int( lv_in ).
        WHEN 'TT'.
          <lv_t> = lcl_cfg=>get( )->title_key( lv_in ).
        WHEN OTHERS.
          <lv_t> = lv_in.
      ENDCASE.
    ENDIF.

    " DATAX carries the same component names as DATA.
    ASSIGN COMPONENT iv_fld OF STRUCTURE cs_datax TO FIELD-SYMBOL(<lv_x>).
    IF sy-subrc = 0.
      <lv_x> = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD master.
    DATA(lo_cfg) = lcl_cfg=>get( ).
    mo_lic->reset( ).

    " ---- 1. keys -------------------------------------------------------
    DATA: lv_kunnr TYPE kunnr,
          lv_bukrs TYPE bukrs,
          lv_vkorg TYPE vkorg,
          lv_vtweg TYPE vtweg,
          lv_spart TYPE spart,
          lv_ktokd TYPE ktokd.
    DATA: lv_kt   TYPE string,
          lv_code TYPE string,
          lv_rest TYPE string.

    LOOP AT mt_map INTO DATA(ls_k) WHERE node = gc_n_key.
      DATA(lv_v) = lcl_util=>cell( is_row = is_row iv_col = ls_k-col ).
      CASE ls_k-fld.
        WHEN 'KUNNR'. lv_kunnr = lcl_util=>alpha( lv_v ).
        WHEN 'BUKRS'. lv_bukrs = lv_v.
        WHEN 'VKORG'. lv_vkorg = lv_v.
        WHEN 'VTWEG'. lv_vtweg = lv_v.
        WHEN 'SPART'. lv_spart = lv_v.
        WHEN 'KTOKD'.
          " Some tabs put a picking list in this cell, for example
          " "ZDOM - Sold-to Customer" followed by more lines. Keep only the
          " code in front, and do it on the string before the value is
          " truncated into a CHAR4 field.
          lv_kt = lv_v.
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
                  IN lv_kt WITH ` `.
          REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf
                  IN lv_kt WITH ` `.
          IF lv_kt CA ` `.
            SPLIT lv_kt AT ` ` INTO lv_code lv_rest.
            lv_kt = lv_code.
          ENDIF.
          lv_ktokd = lv_kt.
      ENDCASE.
    ENDLOOP.

    IF lv_ktokd IS NOT INITIAL AND lo_cfg->ok_ktokd( lv_ktokd ) = abap_false.
      mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'E'
                   iv_fld = 'KTOKD'
                   iv_text = |Account group { lv_ktokd } does not exist| ).
      RETURN.
    ENDIF.

    DATA(lv_exists) = COND abap_bool(
      WHEN lv_kunnr IS INITIAL THEN abap_false
      ELSE lo_cfg->cust_exists( lv_kunnr ) ).
    DATA(lv_task) = COND cmd_ei_object_task(
      WHEN lv_exists = abap_true THEN gc_u ELSE gc_i ).

    IF lv_kunnr IS INITIAL AND lv_ktokd IS INITIAL.
      mo_log->add( iv_row = is_row-row iv_type = 'E'
                   iv_text = 'Neither a customer number nor an account group is given' ).
      RETURN.
    ENDIF.

    " ---- 2. build the customer node ------------------------------------
    DATA ls_cust TYPE cmds_ei_extern.
    CLEAR ls_cust.
    ls_cust-header-object_instance-kunnr = lv_kunnr.
    ls_cust-header-object_task           = lv_task.
    ls_cust-central_data-central-data-ktokd  = lv_ktokd.
    ls_cust-central_data-central-datax-ktokd = abap_true.
    ls_cust-central_data-address-task        = lv_task.

    DATA ls_comp TYPE cmds_ei_company.
    DATA ls_sale TYPE cmds_ei_sales.
    CLEAR: ls_comp, ls_sale.
    ls_comp-task = gc_m.
    ls_comp-data_key-bukrs = lv_bukrs.
    ls_sale-task = gc_m.
    ls_sale-data_key-vkorg = lv_vkorg.
    ls_sale-data_key-vtweg = lv_vtweg.
    ls_sale-data_key-spart = lv_spart.

    DATA(lv_aland) = lo_cfg->aland_of( lv_vkorg ).

    DATA: lv_tel TYPE string,
          lv_mob TYPE string,
          lv_fax TYPE string,
          lv_smt TYPE string,
          lv_adh TYPE string.

    LOOP AT mt_map INTO DATA(ls_m) WHERE node <> gc_n_key.
      DATA(lv_cell) = lcl_util=>cell( is_row = is_row iv_col = ls_m-col ).
      IF lv_cell IS INITIAL.
        CONTINUE.
      ENDIF.

      CASE ls_m-node.

        WHEN gc_n_cent.
          set_comp( EXPORTING iv_fld = ls_m-fld iv_val = lv_cell
                              iv_cnv = ls_m-cnv iv_row = is_row-row
                              iv_struc = 'KNA1'
                    CHANGING  cs_data  = ls_cust-central_data-central-data
                              cs_datax = ls_cust-central_data-central-datax ).

        WHEN gc_n_addr.
          set_comp( EXPORTING iv_fld = ls_m-fld iv_val = lv_cell
                              iv_cnv = ls_m-cnv iv_row = is_row-row
                              iv_struc = 'ADDRESS'
                    CHANGING  cs_data  = ls_cust-central_data-address-postal-data
                              cs_datax = ls_cust-central_data-address-postal-datax ).

        WHEN gc_n_comp.
          set_comp( EXPORTING iv_fld = ls_m-fld iv_val = lv_cell
                              iv_cnv = ls_m-cnv iv_row = is_row-row
                              iv_struc = 'KNB1'
                    CHANGING  cs_data  = ls_comp-data
                              cs_datax = ls_comp-datax ).

        WHEN gc_n_sale.
          set_comp( EXPORTING iv_fld = ls_m-fld iv_val = lv_cell
                              iv_cnv = ls_m-cnv iv_row = is_row-row
                              iv_struc = 'KNVV'
                    CHANGING  cs_data  = ls_sale-data
                              cs_datax = ls_sale-datax ).

        WHEN gc_n_comm.
          CASE ls_m-fld.
            WHEN 'TEL'. lv_tel = lv_cell.
            WHEN 'MOB'. lv_mob = lv_cell.
            WHEN 'FAX'. lv_fax = lv_cell.
            WHEN 'SMT'. lv_smt = lv_cell.
          ENDCASE.

        WHEN gc_n_tax.
          " The named tabs give the tax category outright; the positional
          " tabs give an ordinal (#1 .. #5) resolved against TSTL.
          DATA lv_tatyp TYPE tatyp.
          CLEAR lv_tatyp.
          IF ls_m-fld(1) = '#'.
            lv_tatyp = lo_cfg->tax_cat_nth(
                         iv_aland = lv_aland
                         iv_nth   = CONV i( ls_m-fld+1 ) ).
          ELSE.
            lv_tatyp = ls_m-fld.
          ENDIF.
          IF lv_tatyp IS INITIAL.
            mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'W'
                         iv_struc = 'KNVI' iv_fld = ls_m-fld
                         iv_text = |No tax category configured for country { lv_aland } - column { ls_m-col } skipped| ).
            CONTINUE.
          ENDIF.
          IF lo_cfg->tax_cat_ok( iv_aland = lv_aland iv_tatyp = lv_tatyp ) = abap_false.
            mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'E'
                         iv_struc = 'KNVI' iv_fld = lv_tatyp
                         iv_text = |Tax category { lv_tatyp } is not configured for country { lv_aland }| ).
            CONTINUE.
          ENDIF.
          APPEND VALUE cmds_ei_tax_ind(
            task              = gc_m
            data_key-aland    = lv_aland
            data_key-tatyp    = lv_tatyp
            data-taxkd        = lv_cell
            datax-taxkd       = abap_true ) TO ls_cust-central_data-tax_ind.

        WHEN gc_n_lic.
          mo_lic->set( iv_fld = ls_m-fld iv_val = lv_cell
                       iv_cnv = ls_m-cnv iv_row = is_row-row ).

        WHEN gc_n_iden.
          lv_adh = lv_cell.

      ENDCASE.
    ENDLOOP.

    " ---- 3. communication ----------------------------------------------
    IF lv_tel IS NOT INITIAL.
      APPEND VALUE cvis_ei_phone( task = gc_m
                                  data-telephone  = lv_tel
                                  datax-telephone = abap_true
                                ) TO ls_cust-central_data-address-communication-phone.
    ENDIF.
    IF lv_mob IS NOT INITIAL.
      " A mobile number is a telephone entry flagged as a mobile number.
      APPEND VALUE cvis_ei_phone( task = gc_m
                                  data-telephone  = lv_mob
                                  datax-telephone = abap_true
                                ) TO ls_cust-central_data-address-communication-phone.
    ENDIF.
    IF lv_fax IS NOT INITIAL.
      APPEND VALUE cvis_ei_fax( task = gc_m
                                data-fax  = lv_fax
                                datax-fax = abap_true
                              ) TO ls_cust-central_data-address-communication-fax.
    ENDIF.
    IF lv_smt IS NOT INITIAL.
      APPEND VALUE cvis_ei_smtp( task = gc_m
                                 data-e_mail  = lv_smt
                                 datax-e_mail = abap_true
                               ) TO ls_cust-central_data-address-communication-smtp.
    ENDIF.

    " ---- 4. company code and sales area --------------------------------
    IF lv_bukrs IS NOT INITIAL.
      APPEND ls_comp TO ls_cust-company_data-company.
    ENDIF.
    IF lv_vkorg IS NOT INITIAL.
      APPEND ls_sale TO ls_cust-sales_data-sales.
    ENDIF.

    " ---- 5. the BP node: category, grouping, roles, Aadhaar ------------
    DATA ls_bp TYPE bus_ei_extern.
    CLEAR ls_bp.
    ls_bp-header-object_task     = lv_task.
    ls_bp-central_data-common-data-bp_control-category = gc_org.
    " Grouping is derived by CL_MD_BP_MAINTAIN from the account group
    " unless the user overrides it on the selection screen.
    IF p_bpgrp IS NOT INITIAL.
      ls_bp-central_data-common-data-bp_control-grouping = p_bpgrp.
    ENDIF.

    APPEND VALUE bus_ei_bupa_roles( task = gc_m data_key = gc_role_fi )
           TO ls_bp-central_data-role-roles.
    IF lv_vkorg IS NOT INITIAL.
      APPEND VALUE bus_ei_bupa_roles( task = gc_m data_key = gc_role_sd )
             TO ls_bp-central_data-role-roles.
    ENDIF.

    IF lv_adh IS NOT INITIAL.
      APPEND VALUE bus_ei_bupa_identification(
        task                            = gc_m
        data_key-identificationcategory = gc_id_aadhaar
        data_key-identificationnumber   = lv_adh
      ) TO ls_bp-central_data-ident_number-ident_numbers.
    ENDIF.

    " ---- 6. post --------------------------------------------------------
    DATA ls_cvis TYPE cvis_ei_extern.
    CLEAR ls_cvis.
    ls_cvis-partner  = ls_bp.
    ls_cvis-customer = ls_cust.

    DATA(lv_ok) = mo_cvis->post( is_data  = ls_cvis
                                 iv_row   = is_row-row
                                 iv_kunnr = lv_kunnr ).

    " ---- 7. the licence record, only once the BP is safely in ----------
    IF lv_ok = abap_true AND mo_lic->touched( ) = abap_true.
      IF lv_kunnr IS INITIAL.
        mo_log->add( iv_row = is_row-row iv_type = 'W'
                     iv_struc = 'ZSD_LICENSE_CHK'
                     iv_text = 'Licence data skipped - the customer number is assigned internally, so re-run this row in change mode' ).
      ELSE.
        mo_lic->save( iv_kunnr = lv_kunnr iv_row = is_row-row ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD credit.
    DATA(lo_cfg) = lcl_cfg=>get( ).

    DATA: lv_kunnr TYPE kunnr,
          lv_kkber TYPE kkber,
          lv_curr  TYPE waers,
          lv_risk  TYPE string,
          lv_block TYPE string,
          lv_main  TYPE ty_dec,
          lv_sgm   TYPE ty_dec.
    DATA: lv_has_main TYPE abap_bool,
          lv_has_sgm  TYPE abap_bool.

    LOOP AT mt_map INTO DATA(ls_m).
      DATA(lv_cell) = lcl_util=>cell( is_row = is_row iv_col = ls_m-col ).
      IF lv_cell IS INITIAL.
        CONTINUE.
      ENDIF.
      IF ls_m-node = gc_n_key AND ls_m-fld = 'KUNNR'.
        lv_kunnr = lcl_util=>alpha( lv_cell ).
        CONTINUE.
      ENDIF.
      CHECK ls_m-node = gc_n_cred.
      CASE ls_m-fld.
        WHEN 'SEGMENT'.    lv_kkber = lv_cell.
        WHEN 'LIMIT_MAIN'. lv_main  = lcl_util=>to_dec( lv_cell ). lv_has_main = abap_true.
        WHEN 'LIMIT_SGM'.  lv_sgm   = lcl_util=>to_dec( lv_cell ). lv_has_sgm  = abap_true.
        WHEN 'CURRENCY'.   lv_curr  = lv_cell.
        WHEN 'RISK_CLASS'. lv_risk  = lv_cell.
        WHEN 'XBLOCKED'.   lv_block = lv_cell.
      ENDCASE.
    ENDLOOP.

    IF lv_kunnr IS INITIAL.
      mo_log->add( iv_row = is_row-row iv_type = 'E'
                   iv_text = 'No customer number in this row' ).
      RETURN.
    ENDIF.
    IF lo_cfg->cust_exists( lv_kunnr ) = abap_false.
      mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'E'
                   iv_text = |Customer { lv_kunnr } does not exist| ).
      RETURN.
    ENDIF.

    DATA(lv_sgmnt) = lo_cfg->segment_of( lv_kkber ).
    IF lv_sgmnt IS INITIAL.
      mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'E'
                   iv_fld = 'KKBER'
                   iv_text = |Credit control area { lv_kkber } has no credit segment in UKM_KKBER2SGM| ).
      RETURN.
    ENDIF.

    " The limit is stored in the segment's own currency; the template's
    " currency column can only be checked against it, never written.
    IF lv_curr IS NOT INITIAL.
      DATA(lv_scur) = lo_cfg->segment_curr( lv_sgmnt ).
      IF lv_scur IS NOT INITIAL AND lv_scur <> lv_curr.
        mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'E'
                     iv_fld = 'WAERS'
                     iv_text = |Currency { lv_curr } does not match segment { lv_sgmnt }, which is held in { lv_scur }| ).
        RETURN.
      ENDIF.
    ENDIF.

    " Limit for this credit control area.
    IF lv_has_sgm = abap_true.
      mo_cred->apply( iv_kunnr   = lv_kunnr
                      iv_sgmnt   = lv_sgmnt
                      iv_limit   = lv_sgm
                      iv_has_lim = abap_true
                      iv_block   = lv_block
                      iv_risk    = lv_risk
                      iv_row     = is_row-row ).
    ELSE.
      mo_cred->apply( iv_kunnr   = lv_kunnr
                      iv_sgmnt   = lv_sgmnt
                      iv_limit   = 0
                      iv_has_lim = abap_false
                      iv_block   = lv_block
                      iv_risk    = lv_risk
                      iv_row     = is_row-row ).
    ENDIF.

    " Total across all control areas -> the main segment.
    IF lv_has_main = abap_true.
      mo_cred->apply( iv_kunnr   = lv_kunnr
                      iv_sgmnt   = gc_sgm_main
                      iv_limit   = lv_main
                      iv_has_lim = abap_true
                      iv_block   = space
                      iv_risk    = space
                      iv_row     = is_row-row ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Selection-screen services
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA lt_files TYPE filetable.
  DATA lv_rc    TYPE i.
  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING file_filter = |Excel workbook (*.xlsx)\|*.xlsx\|{ cl_gui_frontend_services=>filetype_all }|
    CHANGING  file_table  = lt_files
              rc          = lv_rc
    EXCEPTIONS OTHERS     = 1 ).
  IF sy-subrc = 0 AND lt_files IS NOT INITIAL.
    p_file = lt_files[ 1 ]-filename.
  ENDIF.

*----------------------------------------------------------------------*
* Main
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA(gv_scen) = COND char2(
    WHEN p_r1 = abap_true THEN 'R1'
    WHEN p_r2 = abap_true THEN 'R2'
    WHEN p_r3 = abap_true THEN 'R3'
    WHEN p_r4 = abap_true THEN 'R4'
    WHEN p_r5 = abap_true THEN 'R5'
    WHEN p_r6 = abap_true THEN 'R6'
    ELSE                       'R7' ).

  DATA(go_log)    = NEW lcl_log( ).
  DATA(go_engine) = NEW lcl_engine( iv_scen = gv_scen io_log = go_log ).

  TRY.
      DATA(gt_row) = NEW lcl_excel( )->read( iv_file    = p_file
                                             iv_from_pc = p_pc
                                             iv_sheet   = go_engine->sheet( ) ).
    CATCH lcx_upl INTO DATA(gx).
      " MESSAGE takes a data object, not an expression.
      DATA(gv_txt) = gx->get_text( ).
      MESSAGE gv_txt TYPE 'E'.
  ENDTRY.

  IF gt_row IS INITIAL.
    DATA gv_none TYPE string.
    gv_none = |Tab "{ go_engine->sheet( ) }" holds no data from row 2 onwards|.
    MESSAGE gv_none TYPE 'I'.
    RETURN.
  ENDIF.

  go_engine->run( gt_row ).

END-OF-SELECTION.

  go_log->counts( IMPORTING ev_ok = DATA(gv_ok) ev_err = DATA(gv_err) ).
  DATA gv_sum TYPE string.
  gv_sum = |{ lines( gt_row ) } row(s) read, { gv_ok } processed, { gv_err } with errors|.
  MESSAGE gv_sum TYPE 'S'.
  go_log->display( ).
