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
*&   CMDS_EI_EXTERN-CENTRAL_DATA-ADDRESS-COMMUNICATION-PHONE-PHONE
*&     and -FAX-FAX, -SMTP-SMTP. Each of PHONE / FAX / SMTP is a WRAPPER
*&     STRUCTURE (CVIS_EI_CVI_PHONE etc.) holding CURRENT_STATE plus a
*&     table of the same name - the table is one level deeper than the
*&     component name suggests. Line structures:
*&     The LINE TYPE of CVIS_EI_PHONE_T is CVIS_EI_PHONE_STR, not
*&     CVIS_EI_PHONE - the _STR wraps it in a component called CONTACT
*&     (plus REMARK), so every field goes through CONTACT-:
*&       CVIS_EI_PHONE_STR-CONTACT-DATA-TELEPHONE
*&         (mobile: -CONTACT-DATA-R_3_USER, data element AD_FLGMOB)
*&       CVIS_EI_FAX_STR-CONTACT-DATA-FAX
*&       CVIS_EI_SMTP_STR-CONTACT-DATA-E_MAIL
*&   CMDS_EI_EXTERN-CENTRAL_DATA-TAX_IND-TAX_IND - same wrapper pattern
*&     (CMDS_EI_CMD_TAX_IND). Line CMDS_EI_TAX_IND:
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
         " The column's heading, upper-cased with everything but letters
         " and digits removed. Columns are located by this rather than by
         " position, so inserting, deleting or reordering a column in the
         " workbook does not silently load values into the wrong fields.
         " Empty where the heading is duplicated within the tab and so
         " cannot identify a column - those stay positional.
         hdr  TYPE char40,
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
            p_bpgrp TYPE bu_group,
            p_skip  TYPE i DEFAULT 1.
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
    " IV_LEN is the length of the field the value is going into. Without it
    " nothing is padded - see the comment in the implementation.
    " The character length of a field, or 0 when it has none. DESCRIBE
    " FIELD ... IN CHARACTER MODE only takes a character-like operand: a
    " packed field such as KNVV-ANTLF, an integer, or a STRING terminates
    " the program with OBJECTS_NOT_CHAR, and a dynamically assigned field
    " symbol can be any of those.
    CLASS-METHODS char_len
      IMPORTING iv_any    TYPE any
      RETURNING VALUE(rv) TYPE i.

    CLASS-METHODS alpha
      IMPORTING iv_in     TYPE string
                iv_len    TYPE i DEFAULT 0
      RETURNING VALUE(rv) TYPE string.

    " Upper-cases and strips everything except letters and digits, so tab
    " names and column headings can be compared without being defeated by
    " spacing, punctuation or capitalisation.
    CLASS-METHODS squash
      IMPORTING iv_in     TYPE clike
      RETURNING VALUE(rv) TYPE string.

    " A one character flag written as a word. Excel turns a tick into TRUE
    " and some files carry YES or 1, all of which would land in a CHAR 1
    " field as its first letter - T, Y, 1 - none of which SAP reads as set.
    CLASS-METHODS flag
      IMPORTING iv_in     TYPE clike
      RETURNING VALUE(rv) TYPE string.

    CLASS-METHODS is_empty
      IMPORTING is_row    TYPE ty_row
      RETURNING VALUE(rv) TYPE abap_bool.

    " Copies the components that were actually filled - those whose flag is
    " set in IS_FROMX - into the like-named components of CS_TO, raising the
    " same flags there. Used to give the business partner the address the
    " customer was given, without writing the mapping out twice.
    CLASS-METHODS copy_like
      IMPORTING is_from  TYPE any
                is_fromx TYPE any
      CHANGING  cs_to    TYPE any
                cs_tox   TYPE any.
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

    " Excel hands a date over in whatever shape the cell had: with a time
    " behind it, with slashes or dashes, the year in front, or - when the
    " cell was a real date and the format was lost - as its serial number.
    IF lv CS ' '.
      SPLIT lv AT ` ` INTO lv DATA(lv_rest).
    ENDIF.
    REPLACE ALL OCCURRENCES OF '/' IN lv WITH '.'.
    REPLACE ALL OCCURRENCES OF '-' IN lv WITH '.'.

    IF lv CS '.'.
      SPLIT lv AT '.' INTO DATA(lv_1) DATA(lv_2) DATA(lv_3).
      IF lv_1 IS INITIAL OR lv_2 IS INITIAL OR lv_3 IS INITIAL
         OR lv_1 CN '0123456789' OR lv_2 CN '0123456789' OR lv_3 CN '0123456789'.
        RETURN.
      ENDIF.

      DATA: lv_d TYPE string,
            lv_m TYPE string,
            lv_y TYPE string.
      IF strlen( lv_1 ) = 4.
        lv_y = lv_1. lv_m = lv_2. lv_d = lv_3.        " 2026-12-31
      ELSEIF CONV i( lv_1 ) > 12 OR CONV i( lv_2 ) <= 12.
        lv_d = lv_1. lv_m = lv_2. lv_y = lv_3.        " 31.12.2026
      ELSE.
        lv_m = lv_1. lv_d = lv_2. lv_y = lv_3.        " 12/31/2026
      ENDIF.
      IF strlen( lv_y ) = 2.
        " Two-digit years in these templates are always this century.
        lv_y = |20{ lv_y }|.
      ENDIF.
      IF strlen( lv_y ) <> 4.
        RETURN.
      ENDIF.
      rv = |{ lv_y }{ lv_m ALPHA = IN WIDTH = 2 }{ lv_d ALPHA = IN WIDTH = 2 }|.

    ELSEIF strlen( lv ) = 8 AND lv CO '0123456789'.
      rv = lv.

    ELSEIF strlen( lv ) BETWEEN 4 AND 6 AND lv CO '0123456789'.
      " A spreadsheet serial: day 1 is 01.01.1900, and the sheet counts a
      " 29.02.1900 that never existed, which is why the epoch is the 30th
      " of December 1899. Only a sensible range is taken.
      DATA(lv_ser) = CONV i( lv ).
      IF lv_ser >= 20000 AND lv_ser <= 80000.
        DATA lv_base TYPE d VALUE '18991230'.
        DATA lv_dat  TYPE d.
        lv_dat = lv_base + lv_ser.
        rv = lv_dat.
      ENDIF.
    ENDIF.

    " Guard against 20260231 and friends: a real date survives a round trip
    " through a date field, an invalid one does not.
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
    CONDENSE lv NO-GAPS.

    " A trailing minus is how SAP writes a negative number, and Excel hands
    " it over that way too.
    DATA(lv_neg) = abap_false.
    IF substring( val = lv off = strlen( lv ) - 1 len = 1 ) = '-'.
      lv_neg = abap_true.
      lv = substring( val = lv len = strlen( lv ) - 1 ).
    ENDIF.

    " Which separator is the decimal one: the LAST of the two. 500,000.00
    " and 500.000,00 are the same number written in two conventions, and
    " taking the comma out of both would turn the second into 500.00.
    DATA(lv_dot) = 0.
    DATA(lv_com) = 0.
    FIND ALL OCCURRENCES OF '.' IN lv MATCH COUNT DATA(lv_ndot).
    FIND ALL OCCURRENCES OF ',' IN lv MATCH COUNT DATA(lv_ncom).
    IF lv_ndot > 0.
      FIND ALL OCCURRENCES OF '.' IN lv RESULTS DATA(lt_dot).
      lv_dot = lt_dot[ lines( lt_dot ) ]-offset + 1.
    ENDIF.
    IF lv_ncom > 0.
      FIND ALL OCCURRENCES OF ',' IN lv RESULTS DATA(lt_com).
      lv_com = lt_com[ lines( lt_com ) ]-offset + 1.
    ENDIF.

    IF lv_dot > 0 AND lv_com > 0.
      IF lv_com > lv_dot.
        REPLACE ALL OCCURRENCES OF '.' IN lv WITH ``.
        REPLACE ALL OCCURRENCES OF ',' IN lv WITH '.'.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv WITH ``.
      ENDIF.
    ELSEIF lv_ncom = 1.
      " One comma and only two digits behind it is a decimal comma;
      " anything else is a thousands separator.
      IF strlen( lv ) - lv_com = 2.
        REPLACE ALL OCCURRENCES OF ',' IN lv WITH '.'.
      ELSE.
        REPLACE ALL OCCURRENCES OF ',' IN lv WITH ``.
      ENDIF.
    ELSEIF lv_ncom > 1.
      REPLACE ALL OCCURRENCES OF ',' IN lv WITH ``.
    ENDIF.

    TRY.
        rv = lv.
      CATCH cx_sy_conversion_error.
        " The superclass, so an overflow is caught as well as a value that
        " is not a number at all.
        CLEAR rv.
        RETURN.
    ENDTRY.
    IF lv_neg = abap_true.
      rv = rv * -1.
    ENDIF.
  ENDMETHOD.

  METHOD to_int.
    DATA(lv_p) = to_dec( iv_in ).
    rv = round( val = lv_p dec = 0 ).
  ENDMETHOD.

  METHOD char_len.
    rv = 0.
    DATA(lv_kind) = cl_abap_typedescr=>describe_by_data( iv_any )->type_kind.
    IF lv_kind = cl_abap_typedescr=>typekind_char
    OR lv_kind = cl_abap_typedescr=>typekind_num
    OR lv_kind = cl_abap_typedescr=>typekind_date
    OR lv_kind = cl_abap_typedescr=>typekind_time.
      DESCRIBE FIELD iv_any LENGTH rv IN CHARACTER MODE.
    ENDIF.
  ENDMETHOD.

  METHOD alpha.
    " Leading-zero conversion, done here rather than through
    " CONVERSION_EXIT_ALPHA_INPUT.
    "
    " That function module pads to the length of its OUTPUT parameter. This
    " method used to hand it a STRING, which has no fixed length, so it had
    " nothing to pad to and the zeros were never added: a reconciliation
    " account keyed as 1120001 stayed 1120001 instead of becoming
    " 0001120001, and every SKB1 lookup missed.
    "
    " Padding to IV_LEN here removes the dependency on that behaviour.
    " Only purely numeric values are padded, which is what ALPHA does.
    CLEAR rv.
    rv = condense( iv_in ).
    IF rv IS INITIAL OR rv = gc_clear.
      RETURN.
    ENDIF.
    IF iv_len > 0 AND rv CO '0123456789' AND strlen( rv ) < iv_len.
      rv = repeat( val = '0' occ = iv_len - strlen( rv ) ) && rv.
    ENDIF.
  ENDMETHOD.

  METHOD squash.
    rv = to_upper( CONV string( iv_in ) ).
    REPLACE ALL OCCURRENCES OF PCRE '[^A-Z0-9]' IN rv WITH ''.
    " The key is kept in a 40 character field, so a longer heading has to be
    " cut to the same length here - otherwise the file's key is 41 characters
    " long, the map's is 40, and a column with a long heading could never
    " match. "Key for sorting according to assignment numbers" is one.
    IF strlen( rv ) > 40.
      rv = rv(40).
    ENDIF.
  ENDMETHOD.

  METHOD copy_like.
    FIELD-SYMBOLS: <lv_fx> TYPE any, <lv_f>  TYPE any,
                   <lv_tx> TYPE any, <lv_t>  TYPE any.
    DATA lo_str TYPE REF TO cl_abap_structdescr.
    lo_str ?= cl_abap_typedescr=>describe_by_data( is_fromx ).
    LOOP AT lo_str->components INTO DATA(ls_cmp).
      ASSIGN COMPONENT ls_cmp-name OF STRUCTURE is_fromx TO <lv_fx>.
      IF sy-subrc <> 0 OR <lv_fx> IS INITIAL.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT ls_cmp-name OF STRUCTURE cs_tox TO <lv_tx>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT ls_cmp-name OF STRUCTURE is_from TO <lv_f>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT ls_cmp-name OF STRUCTURE cs_to TO <lv_t>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      <lv_t>  = <lv_f>.
      <lv_tx> = <lv_fx>.
    ENDLOOP.
  ENDMETHOD.

  METHOD flag.
    DATA(lv) = to_upper( condense( CONV string( iv_in ) ) ).
    rv = lv.
    IF strlen( lv ) <= 1.
      RETURN.
    ENDIF.
    CASE lv.
      WHEN 'TRUE' OR 'YES' OR 'JA' OR 'Y' OR 'J' OR '1' OR 'SET' OR 'CHECKED'.
        rv = 'X'.
      WHEN 'FALSE' OR 'NO' OR 'NEIN' OR 'N' OR '0' OR 'UNCHECKED'.
        CLEAR rv.
    ENDCASE.
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
    " Returns the data rows of the tab that carries the columns of this
    " scenario. IT_WANT holds the headings the scenario expects; the tab is
    " picked by how many of them it has, so the tab NAME does not matter -
    " a renamed tab, a single-sheet copy or the master workbook all load the
    " same way. IV_SHEET is only the tie-breaker and the fallback.
    METHODS read
      IMPORTING iv_file    TYPE rlgrap-filename
                iv_from_pc TYPE abap_bool
                iv_sheet   TYPE string
                iv_skip    TYPE i DEFAULT 1
                it_want    TYPE string_table OPTIONAL
      EXPORTING et_head    TYPE string_table
                et_row     TYPE tt_row
                ev_sheet   TYPE string
      RAISING   lcx_upl.
  PRIVATE SECTION.
    METHODS load_bin
      IMPORTING iv_file    TYPE rlgrap-filename
                iv_from_pc TYPE abap_bool
      RETURNING VALUE(rv)  TYPE xstring
      RAISING   lcx_upl.

    " One worksheet as a table of rows, heading rows included.
    METHODS sheet_rows
      IMPORTING io_xl     TYPE REF TO cl_fdt_xl_spreadsheet
                iv_name   TYPE string
      RETURNING VALUE(rt) TYPE tt_row
      RAISING   lcx_upl.

    " How many of the expected headings this heading row carries.
    METHODS score
      IMPORTING it_head   TYPE string_table
                it_want   TYPE string_table
      RETURNING VALUE(rv) TYPE i.
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

  METHOD sheet_rows.
    DATA(lo_data) = io_xl->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
                      worksheet_name = CONV #( iv_name ) ).
    FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.
    ASSIGN lo_data->* TO <lt_tab>.
    IF <lt_tab> IS NOT ASSIGNED.
      RAISE EXCEPTION TYPE lcx_upl
        EXPORTING iv_text = |Tab "{ iv_name }" could not be converted|.
    ENDIF.

    DATA lv_idx TYPE i.
    LOOP AT <lt_tab> ASSIGNING FIELD-SYMBOL(<ls_line>).
      lv_idx = sy-tabix.
      DATA ls_row TYPE ty_row.
      CLEAR ls_row.
      ls_row-row = lv_idx.
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_c>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        DATA lv_cellv TYPE string.
        lv_cellv = <lv_c>.
        APPEND lv_cellv TO ls_row-cells.
      ENDDO.
      APPEND ls_row TO rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD score.
    DATA lt_k TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line.
    LOOP AT it_head INTO DATA(lv_h).
      DATA(lv_k) = lcl_util=>squash( lv_h ).
      IF lv_k IS NOT INITIAL.
        INSERT lv_k INTO TABLE lt_k.
      ENDIF.
    ENDLOOP.
    LOOP AT it_want INTO DATA(lv_w).
      IF line_exists( lt_k[ table_line = lv_w ] ).
        rv = rv + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD read.
    CLEAR: et_head, et_row, ev_sheet.
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
    IF lt_names IS INITIAL.
      RAISE EXCEPTION TYPE lcx_upl
        EXPORTING iv_text = |{ iv_file } contains no worksheet|.
    ENDIF.

    " The tab whose name matches, if there is one. Names are compared on
    " letters and digits only, so trailing blanks, capitalisation and
    " spaces against underscores make no difference.
    DATA lv_named TYPE string.
    DATA(lv_wnm) = lcl_util=>squash( iv_sheet ).
    LOOP AT lt_names INTO DATA(lv_nm).
      IF lcl_util=>squash( CONV string( lv_nm ) ) = lv_wnm.
        lv_named = lv_nm.
        EXIT.
      ENDIF.
    ENDLOOP.

    " What actually decides is the heading row: over every tab, and over the
    " first few lines of each, the line carrying the most of the columns this
    " scenario expects wins. So the tab name does not matter, and neither
    " does a title line above the headings. Where two tabs are equally good
    " the one named for the scenario is taken.
    CONSTANTS lc_scan TYPE i VALUE 10.
    DATA lv_hit  TYPE string.
    DATA lt_hit  TYPE tt_row.
    DATA lv_best TYPE i.
    DATA lv_hrow TYPE i.
    IF it_want IS NOT INITIAL.
      LOOP AT lt_names INTO DATA(lv_n2).
        DATA(lt_r) = sheet_rows( io_xl = lo_xl iv_name = CONV string( lv_n2 ) ).
        DATA(lv_max) = COND i( WHEN lines( lt_r ) < lc_scan THEN lines( lt_r )
                               ELSE lc_scan ).
        DO lv_max TIMES.
          DATA(lv_r)  = sy-index.
          DATA(lv_sc) = score( it_head = lt_r[ lv_r ]-cells it_want = it_want ).
          IF lv_sc > lv_best
          OR ( lv_sc > 0 AND lv_sc = lv_best AND lv_n2 = lv_named AND lv_hit <> lv_named ).
            lv_best = lv_sc.
            lv_hit  = lv_n2.
            lt_hit  = lt_r.
            lv_hrow = lv_r.
          ENDIF.
        ENDDO.
      ENDLOOP.
    ENDIF.

    " No tab recognisable by its headings - fall back to the name, then to
    " the only tab there is, and to IV_SKIP for the heading row.
    IF lv_best = 0.
      CLEAR lt_hit.
      lv_hit  = lv_named.
      lv_hrow = iv_skip.
      IF lv_hit IS INITIAL AND lines( lt_names ) = 1.
        lv_hit = lt_names[ 1 ].
      ENDIF.
    ENDIF.

    IF lv_hit IS INITIAL.
      DATA lv_have TYPE string.
      LOOP AT lt_names INTO DATA(lv_n3).
        lv_have = COND string( WHEN lv_have IS INITIAL THEN lv_n3
                               ELSE |{ lv_have }, { lv_n3 }| ).
      ENDLOOP.
      RAISE EXCEPTION TYPE lcx_upl
        EXPORTING iv_text = |No tab in this workbook carries the columns of "{ iv_sheet }". | &&
                            |Tabs found: { lv_have }|.
    ENDIF.

    IF lt_hit IS INITIAL.
      lt_hit = sheet_rows( io_xl = lo_xl iv_name = lv_hit ).
    ENDIF.
    ev_sheet = lv_hit.

    " Everything down to and including the heading line is dropped; the
    " heading line itself is handed back, because it is what the columns are
    " matched on. Whether CL_FDT_XL_SPREADSHEET returns the heading row as
    " its first line or consumes it as the column names is release-dependent,
    " which is exactly why the heading is located rather than assumed.
    LOOP AT lt_hit INTO DATA(ls_l).
      IF ls_l-row < lv_hrow.
        CONTINUE.
      ELSEIF ls_l-row = lv_hrow.
        et_head = ls_l-cells.
        CONTINUE.
      ENDIF.
      IF ls_l-row = lv_hrow + 1.
        " Some tabs spread the headings over two lines - the credit tab
        " carries the technical names on one line and, for the columns that
        " have no technical name, the description on the next. A blank
        " heading is therefore filled from the neighbouring line, but only
        " from a line that is itself part of the heading block: a line that
        " carries none of this scenario's headings is data and is left
        " alone.
        IF score( it_head = ls_l-cells it_want = it_want ) > 0.
          DATA lv_hc TYPE i.
          LOOP AT ls_l-cells INTO DATA(lv_fill).
            lv_hc = sy-tabix.
            IF lv_fill IS INITIAL.
              CONTINUE.
            ENDIF.
            IF lv_hc > lines( et_head ).
              APPEND INITIAL LINE TO et_head.
            ENDIF.
            READ TABLE et_head ASSIGNING FIELD-SYMBOL(<lv_hd>) INDEX lv_hc.
            IF sy-subrc = 0 AND <lv_hd> IS INITIAL.
              <lv_hd> = lv_fill.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
      IF lcl_util=>is_empty( ls_l ) = abap_false.
        APPEND ls_l TO et_row.
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

    " A creation is logged before its number exists; this puts the number
    " on the lines already written for that row so the list shows it.
    METHODS set_key
      IMPORTING iv_row   TYPE i
                iv_kunnr TYPE clike.

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

  METHOD set_key.
    IF iv_kunnr IS INITIAL.
      RETURN.
    ENDIF.
    LOOP AT mt_msg ASSIGNING FIELD-SYMBOL(<ls_m>) WHERE xlsrow = iv_row.
      IF <ls_m>-kunnr IS INITIAL.
        <ls_m>-kunnr = iv_kunnr.
      ENDIF.
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

    TYPES tt_bukrs TYPE STANDARD TABLE OF bukrs WITH EMPTY KEY.
    TYPES: BEGIN OF ty_sarea,
             vkorg TYPE vkorg,
             vtweg TYPE vtweg,
             spart TYPE spart,
           END OF ty_sarea,
           tt_sarea TYPE STANDARD TABLE OF ty_sarea WITH EMPTY KEY.

    METHODS cust_exists IMPORTING VALUE(iv_kunnr) TYPE kunnr
                        RETURNING VALUE(rv)       TYPE abap_bool.

    " The Business Partner behind a customer, through the CVI link. A change
    " has to name the partner it is changing, otherwise the API reads the
    " request as a creation and asks for a number.
    METHODS cust_guid   IMPORTING VALUE(iv_kunnr) TYPE kunnr
                        RETURNING VALUE(rv)       TYPE bu_partner_guid.
    METHODS cust_bp     IMPORTING VALUE(iv_kunnr) TYPE kunnr
                        RETURNING VALUE(rv)       TYPE bu_partner.

    " CVI customising: which business partner grouping and which BP roles a
    " customer account group creates. Maintained with SM30, views
    " CVIV_CUST_TO_BP1 and CVIV_CUST_TO_BP2. A creation must state the
    " grouping - it is what gives the new partner its number range.
    " The customer number that ended up behind a GUID we created.
    METHODS cust_by_guid IMPORTING VALUE(iv_guid) TYPE bu_partner_guid
                         RETURNING VALUE(rv)      TYPE kunnr.
    " The business partner behind the same GUID. Unless the grouping is
    " flagged for the same number in CVIC_CUST_TO_BP1, the partner is
    " numbered from its own range and does not match the customer - so the
    " run has to say which one it is.
    METHODS bp_by_guid   IMPORTING VALUE(iv_guid) TYPE bu_partner_guid
                         RETURNING VALUE(rv)      TYPE bu_partner.

    " The key column asks for a customer, but a user working in BP sees the
    " partner number and the two are only the same where the grouping is
    " flagged for it. A number that is not a customer is therefore tried as
    " a partner, and the customer behind it is what the row is applied to.
    METHODS cust_of
      IMPORTING VALUE(iv_in) TYPE kunnr
      EXPORTING ev_kunnr     TYPE kunnr
                ev_from_bp   TYPE bu_partner.

    METHODS bp_group    IMPORTING iv_ktokd  TYPE clike
                        RETURNING VALUE(rv) TYPE bu_group.
    TYPES tt_role TYPE STANDARD TABLE OF bu_role WITH EMPTY KEY.
    METHODS bp_roles    IMPORTING iv_ktokd  TYPE clike
                        RETURNING VALUE(rt) TYPE tt_role.

    " The API does not take a "modify" task on the customer side, so every
    " node has to say insert or update. These answer which one it is.
    " By value, not by reference: a by-reference parameter demands an actual
    " parameter of exactly the same type, and these are called with whatever
    " the row happened to give.
    METHODS has_knb1    IMPORTING VALUE(iv_kunnr) TYPE kunnr
                                  VALUE(iv_bukrs) TYPE bukrs
                        RETURNING VALUE(rv)       TYPE abap_bool.
    METHODS has_knvv    IMPORTING VALUE(iv_kunnr) TYPE kunnr
                                  VALUE(iv_vkorg) TYPE vkorg
                                  VALUE(iv_vtweg) TYPE vtweg
                                  VALUE(iv_spart) TYPE spart
                        RETURNING VALUE(rv)       TYPE abap_bool.
    METHODS has_knvi    IMPORTING VALUE(iv_kunnr) TYPE kunnr
                                  VALUE(iv_aland) TYPE land1
                                  VALUE(iv_tatyp) TYPE tatyp
                        RETURNING VALUE(rv)       TYPE abap_bool.
    METHODS has_role    IMPORTING VALUE(iv_partner) TYPE bu_partner
                                  VALUE(iv_role)    TYPE bu_role
                        RETURNING VALUE(rv)         TYPE abap_bool.
    METHODS has_ident   IMPORTING VALUE(iv_partner) TYPE bu_partner
                                  VALUE(iv_cat)     TYPE bu_id_type
                        RETURNING VALUE(rv)         TYPE abap_bool.

    " The credit tab carries no company code and no sales area, so the ones
    " the customer already has are what the payment terms and the customer
    " group can be written to.
    METHODS cust_bukrs  IMPORTING VALUE(iv_kunnr) TYPE kunnr
                        RETURNING VALUE(rt)       TYPE tt_bukrs.
    METHODS cust_sales  IMPORTING VALUE(iv_kunnr) TYPE kunnr
                        RETURNING VALUE(rt)       TYPE tt_sarea.
    " Company codes belonging to a credit control area (T001-KKBER).
    METHODS kkber_bukrs IMPORTING VALUE(iv_kkber) TYPE kkber
                        RETURNING VALUE(rt)       TYPE tt_bukrs.

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

    " Check tables of the three customer-master fields the credit tab
    " carries. The API reports a failed check as a bare "Entry X does not
    " exist in TVV3", which says neither which field nor where the value
    " came from - so the values are checked here first.
    METHODS ok_kvgr3    IMPORTING iv        TYPE clike
                        RETURNING VALUE(rv) TYPE abap_bool.
    METHODS ok_zterm    IMPORTING iv        TYPE clike
                        RETURNING VALUE(rv) TYPE abap_bool.
    METHODS ok_vzskz    IMPORTING iv        TYPE clike
                        RETURNING VALUE(rv) TYPE abap_bool.

    " Sales areas of a customer whose STORED customer group 3 is no longer
    " in TVV3. The API validates the whole customer, so one such row blocks
    " every update of that customer until it is corrected.
    TYPES: BEGIN OF ty_bad_sa,
             vkorg TYPE vkorg,
             vtweg TYPE vtweg,
             spart TYPE spart,
             kvgr3 TYPE kvgr3,
           END OF ty_bad_sa,
           tt_bad_sa TYPE STANDARD TABLE OF ty_bad_sa WITH EMPTY KEY.
    METHODS bad_kvgr3   IMPORTING VALUE(iv_kunnr) TYPE kunnr
                        RETURNING VALUE(rt)       TYPE tt_bad_sa.

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
    DATA mt_kvgr3 TYPE SORTED TABLE OF kvgr3 WITH UNIQUE KEY table_line.
    DATA mt_zterm TYPE SORTED TABLE OF dzterm WITH UNIQUE KEY table_line.
    DATA mt_vzskz TYPE SORTED TABLE OF vzskz WITH UNIQUE KEY table_line.

    TYPES: BEGIN OF ty_g2b, ktokd TYPE ktokd, grouping TYPE bu_group, END OF ty_g2b,
           BEGIN OF ty_r2b, ktokd TYPE ktokd, role     TYPE bu_role,  END OF ty_r2b.
    DATA mt_g2b TYPE SORTED TABLE OF ty_g2b WITH UNIQUE KEY ktokd.
    DATA mt_r2b TYPE SORTED TABLE OF ty_r2b WITH NON-UNIQUE KEY ktokd.

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
    " MT_TSTL is NON-UNIQUE, so it can be filled directly.
    SELECT talnd, lfdnr, tatyp FROM tstl
      INTO CORRESPONDING FIELDS OF TABLE @mt_tstl.

    " Everything below is declared WITH UNIQUE KEY. Filling such a table
    " from a result set that contains duplicates raises ITAB_DUPLICATE_KEY,
    " which is a short dump and not catchable - so duplicates are removed
    " before the move, never after.
    "
    " These four config tables are keyed on the code today, so DISTINCT is
    " belt and braces. The supplier program dumped on exactly this pattern
    " against T052, which does hold several rows per code.
    SELECT DISTINCT kdgrp FROM t151  INTO TABLE @mt_kdgrp.
    SELECT DISTINCT waers FROM tcurc INTO TABLE @mt_waers.
    SELECT DISTINCT werks FROM t001w INTO TABLE @mt_werks.
    SELECT DISTINCT ktokd FROM t077d INTO TABLE @mt_ktokd.
    SELECT DISTINCT kvgr3 FROM tvv3 INTO TABLE @mt_kvgr3.
    " T052 holds one row per instalment, so the payment terms repeat.
    SELECT DISTINCT zterm FROM t052 INTO TABLE @mt_zterm.
    SELECT DISTINCT vzskz FROM t056 INTO TABLE @mt_vzskz.

    " Keyed on the account group alone. Should the customising ever map one
    " account group to two groupings, INSERT reports it with SY-SUBRC 4 and
    " the first wins, instead of dumping on a duplicate key.
    SELECT account_group AS ktokd, grouping
      FROM cvic_cust_to_bp1 INTO TABLE @DATA(lt_g2b).
    LOOP AT lt_g2b INTO DATA(ls_g2b).
      INSERT VALUE ty_g2b( ktokd    = ls_g2b-ktokd
                           grouping = ls_g2b-grouping ) INTO TABLE mt_g2b.
    ENDLOOP.

    SELECT account_group AS ktokd, role
      FROM cvic_cust_to_bp2 INTO CORRESPONDING FIELDS OF TABLE @mt_r2b.

    " Two columns each, keyed on the first, so INSERT is used instead:
    " a duplicate sets SY-SUBRC 4 and the first entry wins.
    SELECT kkber, credit_sgmnt AS sgmnt FROM ukm_kkber2sgm
      INTO TABLE @DATA(lt_sgm).
    LOOP AT lt_sgm INTO DATA(ls_sgm).
      INSERT VALUE ty_sgm( kkber = ls_sgm-kkber
                           sgmnt = ls_sgm-sgmnt ) INTO TABLE mt_sgm.
    ENDLOOP.

    SELECT credit_sgmnt AS sgmnt, currency AS waers FROM ukmcred_sgm0c
      INTO TABLE @DATA(lt_cur).
    LOOP AT lt_cur INTO DATA(ls_cur).
      INSERT VALUE ty_cur( sgmnt = ls_cur-sgmnt
                           waers = ls_cur-waers ) INTO TABLE mt_cur.
    ENDLOOP.
  ENDMETHOD.

  METHOD cust_exists.
    SELECT SINGLE @abap_true FROM kna1 WHERE kunnr = @iv_kunnr INTO @rv.
  ENDMETHOD.

  METHOD cust_guid.
    SELECT SINGLE partner_guid FROM cvi_cust_link
      WHERE customer = @iv_kunnr INTO @rv.
  ENDMETHOD.

  METHOD bp_by_guid.
    IF iv_guid IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE partner FROM but000
      WHERE partner_guid = @iv_guid INTO @rv.
  ENDMETHOD.

  METHOD cust_of.
    CLEAR: ev_kunnr, ev_from_bp.
    IF iv_in IS INITIAL.
      RETURN.
    ENDIF.

    " A customer number wins - that is what the column asks for, and the
    " same digits can be a customer and, separately, someone else's partner.
    IF cust_exists( iv_in ) = abap_true.
      ev_kunnr = iv_in.
      RETURN.
    ENDIF.

    " BUT000-PARTNER and KNA1-KUNNR are both CHAR 10 with the ALPHA exit,
    " so the number as it stands can be looked up either way round.
    SELECT SINGLE partner_guid FROM but000
      WHERE partner = @iv_in INTO @DATA(lv_guid).
    IF sy-subrc <> 0 OR lv_guid IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE customer FROM cvi_cust_link
      WHERE partner_guid = @lv_guid INTO @ev_kunnr.
    IF ev_kunnr IS NOT INITIAL.
      ev_from_bp = iv_in.
    ENDIF.
  ENDMETHOD.

  METHOD cust_bp.
    SELECT SINGLE b~partner FROM but000 AS b
      INNER JOIN cvi_cust_link AS l ON l~partner_guid = b~partner_guid
      WHERE l~customer = @iv_kunnr INTO @rv.
  ENDMETHOD.

  METHOD has_knb1.
    SELECT SINGLE @abap_true FROM knb1
      WHERE kunnr = @iv_kunnr AND bukrs = @iv_bukrs INTO @rv.
  ENDMETHOD.

  METHOD has_knvv.
    SELECT SINGLE @abap_true FROM knvv
      WHERE kunnr = @iv_kunnr AND vkorg = @iv_vkorg
        AND vtweg = @iv_vtweg AND spart = @iv_spart INTO @rv.
  ENDMETHOD.

  METHOD has_knvi.
    SELECT SINGLE @abap_true FROM knvi
      WHERE kunnr = @iv_kunnr AND aland = @iv_aland AND tatyp = @iv_tatyp INTO @rv.
  ENDMETHOD.

  METHOD has_role.
    IF iv_partner IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE @abap_true FROM but100
      WHERE partner = @iv_partner AND rltyp = @iv_role INTO @rv.
  ENDMETHOD.

  METHOD has_ident.
    IF iv_partner IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE @abap_true FROM but0id
      WHERE partner = @iv_partner AND type = @iv_cat INTO @rv.
  ENDMETHOD.

  METHOD cust_bukrs.
    SELECT bukrs FROM knb1 WHERE kunnr = @iv_kunnr
      ORDER BY bukrs INTO TABLE @rt.
  ENDMETHOD.

  METHOD cust_sales.
    SELECT vkorg, vtweg, spart FROM knvv WHERE kunnr = @iv_kunnr
      ORDER BY vkorg, vtweg, spart
      INTO CORRESPONDING FIELDS OF TABLE @rt.
  ENDMETHOD.

  METHOD kkber_bukrs.
    SELECT bukrs FROM t001 WHERE kkber = @iv_kkber
      ORDER BY bukrs INTO TABLE @rt.
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

  METHOD cust_by_guid.
    IF iv_guid IS INITIAL.
      RETURN.
    ENDIF.
    SELECT SINGLE customer FROM cvi_cust_link
      WHERE partner_guid = @iv_guid INTO @rv.
  ENDMETHOD.

  METHOD bp_group.
    rv = VALUE #( mt_g2b[ ktokd = CONV ktokd( iv_ktokd ) ]-grouping OPTIONAL ).
  ENDMETHOD.

  METHOD bp_roles.
    LOOP AT mt_r2b INTO DATA(ls_r) WHERE ktokd = iv_ktokd.
      APPEND ls_r-role TO rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD ok_kvgr3.
    rv = xsdbool( line_exists( mt_kvgr3[ table_line = iv ] ) ).
  ENDMETHOD.
  METHOD ok_zterm.
    rv = xsdbool( line_exists( mt_zterm[ table_line = iv ] ) ).
  ENDMETHOD.
  METHOD ok_vzskz.
    rv = xsdbool( line_exists( mt_vzskz[ table_line = iv ] ) ).
  ENDMETHOD.

  METHOD bad_kvgr3.
    SELECT vkorg, vtweg, spart, kvgr3 FROM knvv
      WHERE kunnr = @iv_kunnr AND kvgr3 <> @space
      INTO TABLE @DATA(lt_sa).
    LOOP AT lt_sa INTO DATA(ls_sa).
      IF ok_kvgr3( ls_sa-kvgr3 ) = abap_false.
        APPEND CORRESPONDING #( ls_sa ) TO rt.
      ENDIF.
    ENDLOOP.
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
      ( scen = 'R1' col = 2    node = 'K' fld = 'KUNNR' cnv = 'AL' hdr = 'NEWCUSTOMERCODE' )  " New Customer Code
      ( scen = 'R1' col = 3    node = 'K' fld = 'BUKRS' cnv = '' hdr = 'COMPANYCODE' )  " Company Code
      ( scen = 'R1' col = 4    node = 'K' fld = 'VKORG' cnv = '' hdr = 'SALESORGANIZATION' )  " Sales Organization
      ( scen = 'R1' col = 5    node = 'K' fld = 'VTWEG' cnv = '' hdr = 'DISTRIBUTIONCHANNEL' )  " Distribution Channel
      ( scen = 'R1' col = 6    node = 'K' fld = 'SPART' cnv = '' hdr = 'DIVISION' )  " Division
      ( scen = 'R1' col = 7    node = 'K' fld = 'KTOKD' cnv = '' hdr = 'CUSTOMERACCOUNTGROUP' )  " Customer Account Group
      ( scen = 'R1' col = 14   node = 'A' fld = 'TITLE' cnv = 'TT' hdr = 'TITLETEXT' )  " Title text
      ( scen = 'R1' col = 15   node = 'A' fld = 'NAME' cnv = '' hdr = 'NAME1' )  " Name 1
      ( scen = 'R1' col = 16   node = 'A' fld = 'NAME_2' cnv = '' hdr = 'NAME2' )  " Name 2
      ( scen = 'R1' col = 17   node = 'A' fld = 'NAME_3' cnv = '' hdr = 'NAME3' )  " Name 3
      ( scen = 'R1' col = 18   node = 'A' fld = 'NAME_4' cnv = '' hdr = 'NAME4' )  " Name 4
      ( scen = 'R1' col = 19   node = 'A' fld = 'SORT1' cnv = '' hdr = 'SEARCHTERM1' )  " Search Term 1
      ( scen = 'R1' col = 20   node = 'A' fld = 'SORT2' cnv = '' hdr = 'SEARCHTERM2' )  " Search Term 2
      ( scen = 'R1' col = 21   node = 'A' fld = 'C_O_NAME' cnv = '' hdr = 'CONAME' )  " c/o name
      ( scen = 'R1' col = 22   node = 'A' fld = 'STR_SUPPL1' cnv = '' hdr = 'STREET2' )  " Street 2
      ( scen = 'R1' col = 23   node = 'A' fld = 'STR_SUPPL2' cnv = '' hdr = 'STREET3' )  " Street 3
      ( scen = 'R1' col = 24   node = 'A' fld = 'STREET' cnv = '' hdr = 'STREET' )  " Street
      ( scen = 'R1' col = 25   node = 'A' fld = 'HOUSE_NO' cnv = '' hdr = 'HOUSENUMBER' )  " House Number
      ( scen = 'R1' col = 26   node = 'A' fld = 'STR_SUPPL3' cnv = '' hdr = 'STREET4' )  " Street 4
      ( scen = 'R1' col = 27   node = 'A' fld = 'LOCATION' cnv = '' hdr = 'STREET5' )  " Street 5
      ( scen = 'R1' col = 28   node = 'A' fld = 'DISTRICT' cnv = '' hdr = 'DISTRICT' )  " District
      ( scen = 'R1' col = 29   node = 'A' fld = 'POSTL_COD1' cnv = '' hdr = 'CITYPOSTALCODE' )  " City postal code
      ( scen = 'R1' col = 30   node = 'A' fld = 'CITY' cnv = '' hdr = 'CITY' )  " City
      ( scen = 'R1' col = 31   node = 'A' fld = 'COUNTRY' cnv = '' hdr = 'COUNTRYKEY' )  " Country Key
      ( scen = 'R1' col = 32   node = 'A' fld = 'REGION' cnv = '' hdr = 'REGIONSTATEPROVINCECOUNTY' )  " Region (State, Province, County)
      ( scen = 'R1' col = 33   node = 'A' fld = 'LANGU' cnv = '' hdr = 'LANGUAGEKEY' )  " Language Key
      ( scen = 'R1' col = 34   node = 'M' fld = 'TEL' cnv = '' hdr = 'FIRSTTELEPHONENODIALLINGCODENUMBER' )  " First telephone no.: dialling code+number
      ( scen = 'R1' col = 35   node = 'M' fld = 'MOB' cnv = '' hdr = 'FIRSTMOBILETELEPHONENODIALINGCODENUMBER' )  " First Mobile Telephone No.: Dialing Code + Number
      ( scen = 'R1' col = 36   node = 'M' fld = 'FAX' cnv = '' hdr = 'FIRSTFAXNODIALLINGCODENUMBER' )  " First fax no.: dialling code+number
      ( scen = 'R1' col = 37   node = 'M' fld = 'SMT' cnv = '' hdr = 'EMAILADDRESS' )  " E-Mail Address
      ( scen = 'R1' col = 38   node = 'C' fld = 'KATR1' cnv = '' hdr = 'ATTRIBUTE1' )  " Attribute 1
      ( scen = 'R1' col = 39   node = 'C' fld = 'KATR3' cnv = '' hdr = 'ATTRIBUTE3' )  " Attribute 3
      ( scen = 'R1' col = 40   node = 'C' fld = 'KATR4' cnv = '' hdr = 'ATTRIBUTE4' )  " Attribute 4
      ( scen = 'R1' col = 41   node = 'C' fld = 'LIFNR' cnv = 'AL' hdr = 'ACCOUNTNUMBEROFVENDORORCREDITOR' )  " Account Number of Vendor or Creditor
      ( scen = 'R1' col = 42   node = 'C' fld = 'VBUND' cnv = 'AL' hdr = 'COMPANYIDOFTRADINGPARTNER' )  " Company ID of Trading Partner
      ( scen = 'R1' col = 43   node = 'C' fld = 'KONZS' cnv = '' hdr = 'GROUPKEY' )  " Group key
      ( scen = 'R1' col = 44   node = 'C' fld = 'STCD3' cnv = '' hdr = 'TAXNUMBER3GSTNUMBER' )  " Tax Number 3 ( GST Number)
      ( scen = 'R1' col = 45   node = 'C' fld = 'J_1IPANNO' cnv = '' hdr = 'PERMANENTACCOUNTNUMBER' )  " Permanent Account Number
      ( scen = 'R1' col = 46   node = 'C' fld = 'GST_TDS' cnv = '' hdr = 'GSTTDSREGISTRATION' )  " GST TDS Registration
      ( scen = 'R1' col = 47   node = 'I' fld = 'X90003' cnv = '' hdr = 'AADHAARNUMBER' )  " Aadhaar Number
      ( scen = 'R1' col = 48   node = 'B' fld = 'AKONT' cnv = 'GL' hdr = 'RECONCILIATIONACCOUNTINGENERALLEDGER' )  " Reconciliation Account in General Ledger
      ( scen = 'R1' col = 49   node = 'B' fld = 'ZUAWA' cnv = '' hdr = 'KEYFORSORTINGACCORDINGTOASSIGNMENTNUMBER' )  " Key for sorting according to assignment numbers
      ( scen = 'R1' col = 50   node = 'B' fld = 'FDGRV' cnv = 'AL' hdr = 'PLANNINGGROUP' )  " Planning group
      ( scen = 'R1' col = 51   node = 'B' fld = 'VZSKZ' cnv = '' hdr = 'INTERESTCALCULATIONINDICATOR' )  " Interest calculation indicator
      ( scen = 'R1' col = 52   node = 'B' fld = 'ZINRT' cnv = '' hdr = 'INTERESTCALCULATIONFREQUENCYINMONTHS' )  " Interest calculation frequency in months
      ( scen = 'R1' col = 53   node = 'B' fld = 'ALTKN' cnv = '' hdr = 'PREVIOUSMASTERRECORDNUMBER' )  " Previous Master Record Number
      ( scen = 'R1' col = 54   node = 'B' fld = 'ZTERM' cnv = '' hdr = 'TERMSOFPAYMENTKEY' )  " Terms of Payment Key
      ( scen = 'R1' col = 55   node = 'B' fld = 'TOGRU' cnv = '' hdr = 'TOLERANCEGROUPFORTHEBUSINESSPARTNERGLACC' )  " Tolerance group for the business partner/G/L account
      ( scen = 'R1' col = 56   node = 'B' fld = 'XZVER' cnv = '' hdr = 'INDICATORRECORDPAYMENTHISTORY' )  " Indicator: Record Payment History ?
      ( scen = 'R1' col = 57   node = 'B' fld = 'ZWELS' cnv = '' hdr = 'LISTOFTHEPAYMENTMETHODSTOBECONSIDERED' )  " List of the Payment Methods to be Considered
      ( scen = 'R1' col = 58   node = 'B' fld = 'ZAHLS' cnv = '' hdr = 'BLOCKKEYFORPAYMENT' )  " Block Key for Payment
      ( scen = 'R1' col = 59   node = 'S' fld = 'BZIRK' cnv = '' hdr = 'SALESDISTRICT' )  " Sales district
      ( scen = 'R1' col = 60   node = 'S' fld = 'VKBUR' cnv = '' hdr = 'SALESOFFICE' )  " Sales Office
      ( scen = 'R1' col = 61   node = 'S' fld = 'VKGRP' cnv = '' hdr = 'SALESGROUP' )  " Sales Group
      ( scen = 'R1' col = 62   node = 'S' fld = 'KDGRP' cnv = '' hdr = 'CUSTOMERGROUP' )  " Customer group
      ( scen = 'R1' col = 63   node = 'S' fld = 'KLABC' cnv = '' hdr = 'CUSTOMERCLASSIFICATIONABCANALYSIS' )  " Customer classification (ABC analysis)
      ( scen = 'R1' col = 64   node = 'S' fld = 'WAERS' cnv = '' hdr = 'CURRENCY' )  " Currency
      ( scen = 'R1' col = 65   node = 'S' fld = 'KONDA' cnv = '' hdr = 'PRICEGROUPCUSTOMER' )  " Price group (customer)
      ( scen = 'R1' col = 66   node = 'S' fld = 'KALKS' cnv = '' hdr = 'PRICINGPROCEDUREASSIGNEDTOTHISCUSTOMER' )  " Pricing procedure assigned to this customer
      ( scen = 'R1' col = 67   node = 'S' fld = 'VERSG' cnv = '' hdr = 'CUSTOMERSTATISTICSGROUP' )  " Customer Statistics Group
      ( scen = 'R1' col = 68   node = 'S' fld = 'LPRIO' cnv = '' hdr = 'DELIVERYPRIORITY' )  " Delivery Priority
      ( scen = 'R1' col = 69   node = 'S' fld = 'KZAZU' cnv = '' hdr = 'ORDERCOMBINATIONINDICATOR' )  " Order Combination Indicator
      ( scen = 'R1' col = 70   node = 'S' fld = 'VSBED' cnv = '' hdr = 'SHIPPINGCONDITIONS' )  " Shipping Conditions
      ( scen = 'R1' col = 71   node = 'S' fld = 'VWERK' cnv = '' hdr = 'DELIVERINGPLANTOWNOREXTERNAL' )  " Delivering Plant (Own or External)
      ( scen = 'R1' col = 72   node = 'S' fld = 'ANTLF' cnv = '' hdr = 'MAXIMUMNUMBEROFPARTIALDELIVERIESALLOWEDP' )  " Maximum Number of Partial Deliveries Allowed Per Item
      ( scen = 'R1' col = 73   node = 'S' fld = 'INCO1' cnv = '' hdr = 'INCOTERMSPART1' )  " Incoterms (Part 1)
      ( scen = 'R1' col = 74   node = 'S' fld = 'INCO2' cnv = '' hdr = 'INCOTERMSPART2' )  " Incoterms (Part 2)
      ( scen = 'R1' col = 75   node = 'S' fld = 'ZTERM' cnv = '' hdr = 'TERMSOFPAYMENTKEY' )  " Terms of Payment Key - sales area, KNVV
      ( scen = 'R1' col = 76   node = 'S' fld = 'KTGRD' cnv = '' hdr = 'ACCOUNTASSIGNMENTGROUPFORCUSTOMER' )  " Account Assignment Group for Customer
      ( scen = 'R1' col = 77   node = 'T' fld = 'JOCG' cnv = '' hdr = 'JOIGINCENTRALGSTOP' )  " JOIG IN:Central GST - OP
      ( scen = 'R1' col = 78   node = 'T' fld = 'JTC1' cnv = '' hdr = 'JTC1IN206C1HGOODS' )  " JTC1 IN: 206C(1H) Goods
      ( scen = 'R1' col = 79   node = 'T' fld = 'JTX1' cnv = '' hdr = 'JTX1TAXJURISDICTCODED' )  " JTX1 Tax Jurisdict.Code d
      ( scen = 'R1' col = 80   node = 'T' fld = 'JTX2' cnv = '' hdr = 'JTX2TAXJURISDICTCODED' )  " JTX2 Tax Jurisdict.Code d
      ( scen = 'R1' col = 81   node = 'T' fld = 'JTX3' cnv = '' hdr = 'JTX3TAXJURISDICTCODED' )  " JTX3 Tax Jurisdict.Code d
      ( scen = 'R1' col = 82   node = 'T' fld = 'JTX4' cnv = '' hdr = 'JTX4TAXJURISDICTCODED' )  " JTX4 Tax Jurisdict.Code d
      ( scen = 'R1' col = 83   node = 'S' fld = 'KVGR1' cnv = '' hdr = 'CUSTOMERGROUP1' )  " Customer group 1
      ( scen = 'R1' col = 84   node = 'S' fld = 'KVGR2' cnv = '' hdr = 'CUSTOMERGROUP2' )  " Customer group 2
      ( scen = 'R1' col = 85   node = 'S' fld = 'KVGR3' cnv = '' hdr = 'CUSTOMERGROUP3' )  " Customer group 3
      ( scen = 'R1' col = 86   node = 'S' fld = 'KVGR4' cnv = '' hdr = 'CUSTOMERGROUP4' )  " Customer group 4
      ( scen = 'R1' col = 87   node = 'S' fld = 'KVGR5' cnv = '' hdr = 'CUSTOMERGROUP5' )  " Customer group 5
      ( scen = 'R1' col = 88   node = 'Z' fld = 'WERKS' cnv = '' hdr = 'PLANT' )  " Plant
      ( scen = 'R1' col = 89   node = 'Z' fld = 'CUST_TRNST_DAYS' cnv = 'NM' hdr = 'TRANSITDAY' )  " Transit Day
      ( scen = 'R1' col = 90   node = 'Z' fld = 'KMSUM' cnv = 'NM' hdr = 'DISTANCEINKMS' )  " Distance in kms.
      ( scen = 'R1' col = 91   node = 'Z' fld = 'DRUGLICENSE1' cnv = '' hdr = '20BLICNO' )  " 20B. Lic. No
      ( scen = 'R1' col = 92   node = 'Z' fld = 'DEA_EXEMPT' cnv = '' hdr = 'DEAEXEMPT' )  " DEA_exempt
      ( scen = 'R1' col = 93   node = 'Z' fld = 'DRUGLICENSE2' cnv = '' hdr = '21BLICNO' )  " 21B. Lic. No
      ( scen = 'R1' col = 94   node = 'Z' fld = 'SL_EXEMPT' cnv = '' hdr = 'SLEXEMPT' )  " SL_EXEMPT
      ( scen = 'R1' col = 95   node = 'Z' fld = 'DL1_DL2_VALIDDT' cnv = 'DT' hdr = '20BAND21BEXPIRYDATE' )  " 20B and 21B Expiry Date
      ( scen = 'R1' col = 96   node = 'Z' fld = 'FOODSLICENSE' cnv = '' hdr = 'FOODLIC' )  " Food Lic
      ( scen = 'R1' col = 97   node = 'Z' fld = 'FL_VALIDDT' cnv = 'DT' hdr = 'FOODLICVALIDDATE' )  " Food Lic Valid Date
      ( scen = 'R1' col = 98   node = 'Z' fld = 'SCHXNO' cnv = '' hdr = 'SCHXWHSALELICNO' )  " Sch. X Wh.Sale Lic No
      ( scen = 'R1' col = 99   node = 'Z' fld = 'SCHX_VALIDDT' cnv = 'DT' hdr = 'SCHEDULEXWHSALELICEXPDATE' )  " Schedule-X Wh.Sale Lic. Exp. Date
      ( scen = 'R1' col = 100  node = 'Z' fld = 'SCHXRNO' cnv = '' hdr = 'SCHXRETAILLICNO' )  " Sch. X Retail Lic No
      ( scen = 'R1' col = 101  node = 'Z' fld = 'SCHXR_VALIDDT' cnv = 'DT' hdr = 'SCHXRETAILLICEXPDATE' )  " Sch. X Retail Lic Exp. Date
      ( scen = 'R1' col = 102  node = 'Z' fld = 'RETAIL_LIC_NO' cnv = '' hdr = 'RETAILSLICNO20AND21' )  " Retails Lic No (20 and 21 )
      ( scen = 'R1' col = 103  node = 'Z' fld = 'SC_EXEMPT' cnv = '' hdr = 'SCEXEMPT' )  " SC_EXEMPT
      ( scen = 'R1' col = 104  node = 'Z' fld = 'RETAIL_EXP' cnv = 'DT' hdr = 'RETAILSLICEXPDATE' )  " Retails Lic Exp date
      ( scen = 'R1' col = 105  node = 'Z' fld = 'MFGLIC1NO' cnv = '' hdr = 'MFGLICENSEGENNUMBER' )  " Mfg License (Gen) Number
      ( scen = 'R1' col = 106  node = 'Z' fld = 'MFGLIC2NO' cnv = '' hdr = 'MFGLICENSENARNUMBER' )  " Mfg License (Nar) Number
      ( scen = 'R1' col = 107  node = 'Z' fld = 'MFGLIC3NO' cnv = '' hdr = 'MFGLICENSECCNUMBER' )  " Mfg License (CC) Number
      ( scen = 'R1' col = 108  node = 'Z' fld = 'BGYN' cnv = '' hdr = 'BANKGUARANTEEYN' )  " Bank Guarantee(Y/N)
      ( scen = 'R1' col = 109  node = 'Z' fld = 'BG_NO' cnv = '' hdr = 'BANKGUARANTEENO' )  " Bank Guarantee No
      ( scen = 'R1' col = 110  node = 'Z' fld = 'BG_AMT' cnv = 'NM' hdr = 'BGAMOUNT' )  " BG Amount
      ( scen = 'R1' col = 111  node = 'Z' fld = 'CURRENCY' cnv = '' hdr = 'SDDOCUMENTCURRENCY' )  " SD Document Currency
      ( scen = 'R1' col = 112  node = 'Z' fld = 'BG_ISS_DT' cnv = 'DT' hdr = 'BGISSUEDATE' )  " BG Issue Date
      ( scen = 'R1' col = 113  node = 'Z' fld = 'BG_EXP_DT' cnv = 'DT' hdr = 'BGEXPIRYDATE' )  " BG Expiry Date
      ( scen = 'R1' col = 114  node = 'Z' fld = 'BG_ISS_BANK' cnv = '' hdr = 'BGISSUINGBANK' )  " BG Issuing Bank
      ( scen = 'R1' col = 115  node = 'Z' fld = 'AGGR_EXPDT' cnv = 'DT' hdr = 'AGREEMENTEXPIRYDATE' )  " Agreement Expiry Date
      ( scen = 'R1' col = 116  node = 'Z' fld = 'APPOINT_DT' cnv = 'DT' hdr = 'APPOINTMENTDATE' )  " Appointment Date
      ( scen = 'R1' col = 117  node = 'Z' fld = 'KDGRP' cnv = '' hdr = 'CUSTOMERGROUP' )  " Customer group - the licence record's, as on the Morocco and SAGA tabs
      ( scen = 'R1' col = 118  node = 'Z' fld = 'AIOCD_CODE' cnv = '' hdr = 'AIOCDCODE' )  " AIOCD Code
      ( scen = 'R1' col = 119  node = 'Z' fld = 'CUST_BNK_NAME' cnv = '' hdr = 'CUSTOMERBANKNAME' )  " Customer Bank Name
      ( scen = 'R1' col = 120  node = 'Z' fld = 'DST_BOOKING' cnv = '' hdr = 'DESTINATIONOFBOOKING' )  " Destination of Booking
      ( scen = 'R1' col = 121  node = 'Z' fld = 'ZTROUT' cnv = '' hdr = 'ROUTECODE' )  " Route Code
      ( scen = 'R1' col = 122  node = 'Z' fld = 'EXTENSION' cnv = '' hdr = 'EXTENSION' )  " Extension
      ( scen = 'R1' col = 123  node = 'Z' fld = 'ZCROUT' cnv = '' hdr = 'ROUTE' )  " Route
      ( scen = 'R1' col = 124  node = 'Z' fld = 'GLN_URI_FORMAT' cnv = '' hdr = 'GLNURIFORMAT' )  " GLN URI Format
      ( scen = 'R1' col = 125  node = 'Z' fld = 'DUNS_NUMBER' cnv = '' hdr = 'DUNSNUMBER' )  " DUNS_Number
      ( scen = 'R1' col = 126  node = 'Z' fld = 'DEA_FROM_DATE' cnv = 'DT' hdr = 'DEAFROMDATE' )  " DEA From Date
      ( scen = 'R1' col = 127  node = 'Z' fld = 'DEA_TO_DATE' cnv = 'DT' hdr = 'DEATODATE' )  " DEA To Date
      ( scen = 'R1' col = 128  node = 'Z' fld = 'STATE_FROM_DATE' cnv = 'DT' hdr = 'STATEFROMDATE' )  " State From Date
      ( scen = 'R1' col = 129  node = 'Z' fld = 'STATE_TO_DATE' cnv = 'DT' hdr = 'STATETODATE' )  " State To Date
      ( scen = 'R1' col = 130  node = 'Z' fld = 'ZIMP_LIC_MIA' cnv = '' hdr = 'IMPORTLICENSEMIA' )  " Import_License/MIA
      ( scen = 'R1' col = 131  node = 'Z' fld = 'ZIMP_FROMDT_MIA' cnv = 'DT' hdr = 'IMPLMIAFROMDATE' )  " IMPL/MIA_From_Date
      ( scen = 'R1' col = 132  node = 'Z' fld = 'ZIMP_VALIDDT_MIA' cnv = 'DT' hdr = 'IMPLMIAVALIDDATE' )  " IMPL/MIA_Valid_Date
      ( scen = 'R1' col = 133  node = 'Z' fld = 'CHECK_DIGIT' cnv = '' hdr = 'CHECKDIGIT' )  " Check Digit
      ( scen = 'R1' col = 134  node = 'Z' fld = 'GLOBAL_COM' cnv = '' hdr = 'GLOBALCOMPANYPREFIX' )  " Global Company Prefix
      ( scen = 'R1' col = 135  node = 'Z' fld = 'BO_DAYS' cnv = '' hdr = 'BACKORDERDAYS' )  " Backorder Days
      ( scen = 'R1' col = 136  node = 'Z' fld = 'LOCATION_NUMBER' cnv = '' hdr = 'LOCATIONNUMBER' )  " Location Number
    ) TO rt.

    " R2 - Export customer (67 columns, 64 mapped)
    APPEND LINES OF VALUE tt_map(
      ( scen = 'R2' col = 2    node = 'K' fld = 'BUKRS' cnv = '' hdr = 'BUKRS' )  " BUKRS
      ( scen = 'R2' col = 3    node = 'K' fld = 'VKORG' cnv = '' hdr = 'VKORG' )  " VKORG
      ( scen = 'R2' col = 4    node = 'K' fld = 'VTWEG' cnv = '' hdr = 'VTWEG' )  " VTWEG
      ( scen = 'R2' col = 5    node = 'K' fld = 'SPART' cnv = '' hdr = 'SPART' )  " SPART
      ( scen = 'R2' col = 6    node = 'K' fld = 'KTOKD' cnv = '' hdr = 'KTOKD' )  " KTOKD
      ( scen = 'R2' col = 8    node = 'A' fld = 'TITLE' cnv = 'TT' hdr = 'TITLEMEDI' )  " TITLE_MEDI
      ( scen = 'R2' col = 9    node = 'A' fld = 'NAME' cnv = '' hdr = 'NAME1' )  " NAME1
      ( scen = 'R2' col = 10   node = 'A' fld = 'NAME_2' cnv = '' hdr = 'NAME2' )  " NAME2
      ( scen = 'R2' col = 11   node = 'A' fld = 'NAME_3' cnv = '' hdr = 'NAME3' )  " NAME3
      ( scen = 'R2' col = 12   node = 'A' fld = 'NAME_4' cnv = '' hdr = 'NAME4' )  " NAME4
      ( scen = 'R2' col = 13   node = 'A' fld = 'SORT1' cnv = '' hdr = 'SORT1' )  " SORT1
      ( scen = 'R2' col = 14   node = 'A' fld = 'SORT2' cnv = '' hdr = 'SORT2' )  " SORT2
      ( scen = 'R2' col = 15   node = 'A' fld = 'C_O_NAME' cnv = '' hdr = 'NAMECO' )  " NAME_CO
      ( scen = 'R2' col = 16   node = 'A' fld = 'STR_SUPPL1' cnv = '' hdr = 'STRSUPPL1' )  " STR_SUPPL1
      ( scen = 'R2' col = 17   node = 'A' fld = 'STR_SUPPL2' cnv = '' hdr = 'STRSUPPL2' )  " STR_SUPPL2
      ( scen = 'R2' col = 18   node = 'A' fld = 'STREET' cnv = '' hdr = 'STREET' )  " STREET
      ( scen = 'R2' col = 19   node = 'A' fld = 'STR_SUPPL3' cnv = '' hdr = 'STRSUPPL3' )  " STR_SUPPL3
      ( scen = 'R2' col = 20   node = 'A' fld = 'LOCATION' cnv = '' hdr = 'LOCATION' )  " LOCATION
      ( scen = 'R2' col = 21   node = 'A' fld = 'DISTRICT' cnv = '' hdr = 'CITY2' )  " CITY2
      ( scen = 'R2' col = 22   node = 'A' fld = 'POSTL_COD1' cnv = '' hdr = 'POSTCODE1' )  " POST_CODE1
      ( scen = 'R2' col = 23   node = 'A' fld = 'CITY' cnv = '' hdr = 'CITY1' )  " CITY1
      ( scen = 'R2' col = 24   node = 'A' fld = 'COUNTRY' cnv = '' hdr = 'COUNTRY' )  " COUNTRY
      ( scen = 'R2' col = 25   node = 'A' fld = 'REGION' cnv = '' hdr = 'REGION' )  " REGION
      ( scen = 'R2' col = 26   node = 'A' fld = 'LANGU' cnv = '' hdr = 'LANGU' )  " LANGU
      ( scen = 'R2' col = 27   node = 'M' fld = 'TEL' cnv = '' hdr = 'TELNUMBER' )  " TEL_NUMBER
      ( scen = 'R2' col = 28   node = 'M' fld = 'MOB' cnv = '' hdr = 'MOBNUMBER' )  " MOB_NUMBER
      ( scen = 'R2' col = 29   node = 'M' fld = 'FAX' cnv = '' hdr = 'FAXNUMBER' )  " FAX_NUMBER
      ( scen = 'R2' col = 30   node = 'M' fld = 'SMT' cnv = '' hdr = 'SMTPADDR' )  " SMTP_ADDR
      ( scen = 'R2' col = 32   node = 'C' fld = 'LIFNR' cnv = 'AL' hdr = 'LIFNR' )  " LIFNR
      ( scen = 'R2' col = 33   node = 'C' fld = 'KUKLA' cnv = '' hdr = 'KUKLA' )  " KUKLA
      ( scen = 'R2' col = 34   node = 'C' fld = 'UMSA1' cnv = '' hdr = 'UMSA1' )  " UMSA1
      ( scen = 'R2' col = 35   node = 'C' fld = 'UWAER' cnv = '' hdr = 'UWAER' )  " UWAER
      ( scen = 'R2' col = 36   node = 'C' fld = 'UMJAH' cnv = '' hdr = 'UMJAH' )  " UMJAH
      ( scen = 'R2' col = 37   node = 'B' fld = 'AKONT' cnv = 'GL' hdr = 'AKONT' )  " AKONT
      ( scen = 'R2' col = 38   node = 'B' fld = 'ZUAWA' cnv = '' hdr = 'ZUAWA' )  " ZUAWA
      ( scen = 'R2' col = 39   node = 'B' fld = 'XZVER' cnv = '' hdr = 'XZVER' )  " XZVER
      ( scen = 'R2' col = 40   node = 'S' fld = 'BZIRK' cnv = '' hdr = 'BZIRK' )  " BZIRK
      ( scen = 'R2' col = 41   node = 'S' fld = 'AWAHR' cnv = '' hdr = 'AWAHR' )  " AWAHR
      ( scen = 'R2' col = 42   node = 'S' fld = 'VKBUR' cnv = '' hdr = 'VKBUR' )  " VKBUR
      ( scen = 'R2' col = 43   node = 'S' fld = 'VKGRP' cnv = '' hdr = 'VKGRP' )  " VKGRP
      ( scen = 'R2' col = 44   node = 'S' fld = 'KDGRP' cnv = '' hdr = 'KDGRP' )  " KDGRP
      ( scen = 'R2' col = 45   node = 'S' fld = 'KLABC' cnv = '' hdr = 'KLABC' )  " KLABC
      ( scen = 'R2' col = 46   node = 'S' fld = 'WAERS' cnv = '' hdr = 'WAERS' )  " WAERS
      ( scen = 'R2' col = 47   node = 'S' fld = 'KURST' cnv = '' hdr = 'KURST' )  " KURST
      ( scen = 'R2' col = 48   node = 'S' fld = 'KALKS' cnv = '' hdr = 'KALKS' )  " KALKS
      ( scen = 'R2' col = 49   node = 'S' fld = 'VERSG' cnv = '' hdr = 'VERSG' )  " VERSG
      ( scen = 'R2' col = 50   node = 'S' fld = 'LPRIO' cnv = '' hdr = 'LPRIO' )  " LPRIO
      ( scen = 'R2' col = 51   node = 'S' fld = 'KZAZU' cnv = '' hdr = 'KZAZU' )  " KZAZU
      ( scen = 'R2' col = 52   node = 'S' fld = 'VSBED' cnv = '' hdr = 'VSBED' )  " VSBED
      ( scen = 'R2' col = 53   node = 'S' fld = 'VWERK' cnv = '' hdr = 'VWERK' )  " VWERK
      ( scen = 'R2' col = 54   node = 'S' fld = 'ANTLF' cnv = '' hdr = 'ANTLF' )  " ANTLF
      ( scen = 'R2' col = 55   node = 'S' fld = 'INCO1' cnv = '' hdr = 'INCO1' )  " INCO1
      ( scen = 'R2' col = 56   node = 'S' fld = 'INCO2' cnv = '' hdr = 'INCO2' )  " INCO2
      ( scen = 'R2' col = 57   node = 'S' fld = 'ZTERM' cnv = '' hdr = 'ZTERM' )  " ZTERM - sales area, KNVV; this tab has no company code payment term
      ( scen = 'R2' col = 58   node = 'S' fld = 'KTGRD' cnv = '' hdr = 'KTGRD' )  " KTGRD
      ( scen = 'R2' col = 59   node = 'T' fld = '#1' cnv = '' hdr = 'TAXKD01' )  " TAXKD_01
      ( scen = 'R2' col = 60   node = 'T' fld = '#2' cnv = '' hdr = 'TAXKD02' )  " TAXKD_02
      ( scen = 'R2' col = 61   node = 'T' fld = '#3' cnv = '' hdr = 'TAXKD03' )  " TAXKD_03
      ( scen = 'R2' col = 62   node = 'T' fld = '#4' cnv = '' hdr = 'TAXKD04' )  " TAXKD_04
      ( scen = 'R2' col = 63   node = 'S' fld = 'KVGR1' cnv = '' hdr = 'KVGR1' )  " KVGR1
      ( scen = 'R2' col = 64   node = 'S' fld = 'KVGR3' cnv = '' hdr = 'KVGR3' )  " KVGR3
      ( scen = 'R2' col = 65   node = 'S' fld = 'KVGR4' cnv = '' hdr = 'KVGR4' )  " KVGR4
      ( scen = 'R2' col = 66   node = 'S' fld = 'KVGR5' cnv = '' hdr = 'KVGR5' )  " KVGR5
      ( scen = 'R2' col = 67   node = 'C' fld = 'J_1IPANNO' cnv = '' hdr = 'PANNO' )  " PAN No
    ) TO rt.

    " R3 - Morocco customer (116 columns, 104 mapped)
    APPEND LINES OF VALUE tt_map(
      ( scen = 'R3' col = 3    node = 'K' fld = 'BUKRS' cnv = '' hdr = 'BUKRS' )  " BUKRS
      ( scen = 'R3' col = 4    node = 'K' fld = 'VKORG' cnv = '' hdr = 'VKORG' )  " VKORG
      ( scen = 'R3' col = 5    node = 'K' fld = 'VTWEG' cnv = '' hdr = 'VTWEG' )  " VTWEG
      ( scen = 'R3' col = 6    node = 'K' fld = 'SPART' cnv = '' hdr = 'SPART' )  " SPART
      ( scen = 'R3' col = 7    node = 'K' fld = 'KTOKD' cnv = '' hdr = 'KTOKD' )  " KTOKD
      ( scen = 'R3' col = 9    node = 'A' fld = 'TITLE' cnv = 'TT' hdr = 'TITLEMEDI' )  " TITLE_MEDI
      ( scen = 'R3' col = 10   node = 'A' fld = 'NAME' cnv = '' hdr = 'NAME1' )  " NAME1
      ( scen = 'R3' col = 11   node = 'A' fld = 'NAME_2' cnv = '' hdr = 'NAME2' )  " NAME2
      ( scen = 'R3' col = 12   node = 'A' fld = 'NAME_3' cnv = '' hdr = 'NAME3' )  " NAME3
      ( scen = 'R3' col = 13   node = 'A' fld = 'NAME_4' cnv = '' hdr = 'NAME4' )  " NAME4
      ( scen = 'R3' col = 14   node = 'A' fld = 'SORT1' cnv = '' hdr = 'SORT1' )  " SORT1
      ( scen = 'R3' col = 15   node = 'A' fld = 'SORT2' cnv = '' hdr = 'SORT2' )  " SORT2
      ( scen = 'R3' col = 16   node = 'A' fld = 'C_O_NAME' cnv = '' hdr = 'NAMECO' )  " NAME_CO
      ( scen = 'R3' col = 17   node = 'A' fld = 'STR_SUPPL1' cnv = '' hdr = 'STRSUPPL1' )  " STR_SUPPL1
      ( scen = 'R3' col = 18   node = 'A' fld = 'STR_SUPPL2' cnv = '' hdr = 'STRSUPPL2' )  " STR_SUPPL2
      ( scen = 'R3' col = 19   node = 'A' fld = 'STREET' cnv = '' hdr = 'STREET' )  " STREET
      ( scen = 'R3' col = 20   node = 'A' fld = 'STR_SUPPL3' cnv = '' hdr = 'STRSUPPL3' )  " STR_SUPPL3
      ( scen = 'R3' col = 21   node = 'A' fld = 'LOCATION' cnv = '' hdr = 'LOCATION' )  " LOCATION
      ( scen = 'R3' col = 22   node = 'A' fld = 'DISTRICT' cnv = '' hdr = 'CITY2' )  " CITY2
      ( scen = 'R3' col = 23   node = 'A' fld = 'POSTL_COD1' cnv = '' hdr = 'POSTCODE1' )  " POST_CODE1
      ( scen = 'R3' col = 24   node = 'A' fld = 'CITY' cnv = '' hdr = 'CITY1' )  " CITY1
      ( scen = 'R3' col = 25   node = 'A' fld = 'COUNTRY' cnv = '' hdr = 'COUNTRY' )  " COUNTRY
      ( scen = 'R3' col = 26   node = 'A' fld = 'REGION' cnv = '' hdr = 'REGION' )  " REGION
      ( scen = 'R3' col = 27   node = 'A' fld = 'LANGU' cnv = '' hdr = 'LANGU' )  " LANGU
      ( scen = 'R3' col = 28   node = 'M' fld = 'TEL' cnv = '' hdr = 'TELNUMBER' )  " TEL_NUMBER
      ( scen = 'R3' col = 29   node = 'M' fld = 'MOB' cnv = '' hdr = 'MOBNUMBER' )  " MOB_NUMBER
      ( scen = 'R3' col = 30   node = 'M' fld = 'FAX' cnv = '' hdr = 'FAXNUMBER' )  " FAX_NUMBER
      ( scen = 'R3' col = 31   node = 'M' fld = 'SMT' cnv = '' hdr = 'SMTPADDR' )  " SMTP_ADDR
      ( scen = 'R3' col = 32   node = 'C' fld = 'KATR3' cnv = '' hdr = 'KATR3' )  " KATR3
      ( scen = 'R3' col = 33   node = 'A' fld = 'TIME_ZONE' cnv = '' hdr = 'TIMEZONE' )  " TIME_ZONE
      ( scen = 'R3' col = 34   node = 'C' fld = 'J_1IPANNO' cnv = '' hdr = 'J1IPANNO' )  " J_1IPANNO
      ( scen = 'R3' col = 35   node = 'C' fld = 'STCD3' cnv = '' hdr = 'STCD3' )  " STCD3
      ( scen = 'R3' col = 36   node = 'B' fld = 'AKONT' cnv = 'GL' hdr = 'AKONT' )  " AKONT
      ( scen = 'R3' col = 37   node = 'B' fld = 'ZUAWA' cnv = '' hdr = 'ZUAWA' )  " ZUAWA
      ( scen = 'R3' col = 38   node = 'B' fld = 'FDGRV' cnv = 'AL' hdr = 'FDGRV' )  " FDGRV
      ( scen = 'R3' col = 39   node = 'B' fld = 'VZSKZ' cnv = '' hdr = 'VZSKZ' )  " VZSKZ
      ( scen = 'R3' col = 40   node = 'B' fld = 'ZINRT' cnv = '' hdr = 'ZINRT' )  " ZINRT
      ( scen = 'R3' col = 41   node = 'B' fld = 'ZTERM' cnv = '' hdr = 'ZTERM' )  " ZTERM
      ( scen = 'R3' col = 42   node = 'B' fld = 'XZVER' cnv = '' hdr = 'XZVER' )  " XZVER
      ( scen = 'R3' col = 43   node = 'B' fld = 'ZWELS' cnv = '' hdr = 'ZWELS' )  " ZWELS
      ( scen = 'R3' col = 44   node = 'S' fld = 'BZIRK' cnv = '' hdr = 'BZIRK' )  " BZIRK
      ( scen = 'R3' col = 45   node = 'S' fld = 'VKBUR' cnv = '' hdr = 'VKBUR' )  " VKBUR
      ( scen = 'R3' col = 46   node = 'S' fld = 'VKGRP' cnv = '' hdr = 'VKGRP' )  " VKGRP
      ( scen = 'R3' col = 47   node = 'S' fld = 'KDGRP' cnv = '' hdr = 'KDGRP' )  " KDGRP
      ( scen = 'R3' col = 48   node = 'S' fld = 'KLABC' cnv = '' hdr = 'KLABC' )  " KLABC
      ( scen = 'R3' col = 49   node = 'S' fld = 'WAERS' cnv = '' hdr = 'WAERS' )  " WAERS
      ( scen = 'R3' col = 50   node = 'S' fld = 'KONDA' cnv = '' hdr = 'KONDA' )  " KONDA
      ( scen = 'R3' col = 51   node = 'S' fld = 'KALKS' cnv = '' hdr = 'KALKS' )  " KALKS
      ( scen = 'R3' col = 52   node = 'S' fld = 'VERSG' cnv = '' hdr = 'VERSG' )  " VERSG
      ( scen = 'R3' col = 53   node = 'S' fld = 'LPRIO' cnv = '' hdr = 'LPRIO' )  " LPRIO
      ( scen = 'R3' col = 54   node = 'S' fld = 'KZAZU' cnv = '' hdr = 'KZAZU' )  " KZAZU
      ( scen = 'R3' col = 55   node = 'S' fld = 'VSBED' cnv = '' hdr = 'VSBED' )  " VSBED
      ( scen = 'R3' col = 56   node = 'S' fld = 'VWERK' cnv = '' hdr = 'VWERK' )  " VWERK
      ( scen = 'R3' col = 57   node = 'S' fld = 'ANTLF' cnv = '' hdr = 'ANTLF' )  " ANTLF
      ( scen = 'R3' col = 58   node = 'S' fld = 'ZTERM' cnv = '' hdr = 'ZTERM1' )  " ZTERM1
      ( scen = 'R3' col = 59   node = 'S' fld = 'KTGRD' cnv = '' hdr = 'KTGRD' )  " KTGRD
      ( scen = 'R3' col = 60   node = 'T' fld = '#1' cnv = '' hdr = 'TAXKD01' )  " TAXKD_01
      ( scen = 'R3' col = 65   node = 'S' fld = 'KVGR1' cnv = '' hdr = 'KVGR1' )  " KVGR1
      ( scen = 'R3' col = 66   node = 'S' fld = 'KVGR2' cnv = '' hdr = 'KVGR2' )  " KVGR2
      ( scen = 'R3' col = 67   node = 'S' fld = 'KVGR3' cnv = '' hdr = 'KVGR3' )  " KVGR3
      ( scen = 'R3' col = 68   node = 'S' fld = 'KVGR4' cnv = '' hdr = 'KVGR4' )  " KVGR4
      ( scen = 'R3' col = 69   node = 'S' fld = 'KVGR5' cnv = '' hdr = 'KVGR5' )  " KVGR5
      ( scen = 'R3' col = 70   node = 'Z' fld = 'WERKS' cnv = '' hdr = 'WERKS' )  " WERKS
      ( scen = 'R3' col = 71   node = 'Z' fld = 'CUST_TRNST_DAYS' cnv = 'NM' hdr = 'CUSTTRNSTDAYS' )  " CUST_TRNST_DAYS
      ( scen = 'R3' col = 72   node = 'Z' fld = 'KMSUM' cnv = 'NM' hdr = 'KMSUM' )  " KMSUM
      ( scen = 'R3' col = 73   node = 'Z' fld = 'DRUGLICENSE1' cnv = '' hdr = 'DRUGLICENSE1' )  " DRUGLICENSE1
      ( scen = 'R3' col = 74   node = 'Z' fld = 'DRUGLICENSE2' cnv = '' hdr = 'DRUGLICENSE2' )  " DRUGLICENSE2
      ( scen = 'R3' col = 75   node = 'Z' fld = 'DL1_DL2_VALIDDT' cnv = 'DT' hdr = 'DL1DL2VALIDDT' )  " DL1_DL2_VALIDDT
      ( scen = 'R3' col = 76   node = 'Z' fld = 'FOODSLICENSE' cnv = '' hdr = 'FOODSLICENSE' )  " FOODSLICENSE
      ( scen = 'R3' col = 77   node = 'Z' fld = 'FL_VALIDDT' cnv = 'DT' hdr = 'FLVALIDDT' )  " FL_VALIDDT
      ( scen = 'R3' col = 78   node = 'Z' fld = 'SCHXNO' cnv = '' hdr = 'SCHXNO' )  " SCHXNO
      ( scen = 'R3' col = 79   node = 'Z' fld = 'SCHX_VALIDDT' cnv = 'DT' hdr = 'SCHXVALIDDT' )  " SCHX_VALIDDT
      ( scen = 'R3' col = 80   node = 'Z' fld = 'SCHXRNO' cnv = '' hdr = 'SCHXRNO' )  " SCHXRNO
      ( scen = 'R3' col = 81   node = 'Z' fld = 'SCHXR_VALIDDT' cnv = 'DT' hdr = 'SCHXRVALIDDT' )  " SCHXR_VALIDDT
      ( scen = 'R3' col = 82   node = 'Z' fld = 'RETAIL_LIC_NO' cnv = '' hdr = 'RETAILLICNO' )  " RETAIL_LIC_NO
      ( scen = 'R3' col = 83   node = 'Z' fld = 'RETAIL_EXP' cnv = 'DT' hdr = 'RETAILEXP' )  " RETAIL_EXP
      ( scen = 'R3' col = 84   node = 'Z' fld = 'MFGLIC1NO' cnv = '' hdr = 'MFGLIC1NO' )  " MFGLIC1NO
      ( scen = 'R3' col = 85   node = 'Z' fld = 'MFGLIC2NO' cnv = '' hdr = 'MFGLIC2NO' )  " MFGLIC2NO
      ( scen = 'R3' col = 86   node = 'Z' fld = 'MFGLIC3NO' cnv = '' hdr = 'MFGLIC3NO' )  " MFGLIC3NO
      ( scen = 'R3' col = 87   node = 'Z' fld = 'BGYN' cnv = '' hdr = 'BGYN' )  " BGYN
      ( scen = 'R3' col = 88   node = 'Z' fld = 'BG_NO' cnv = '' hdr = 'BGNO' )  " BG_NO
      ( scen = 'R3' col = 89   node = 'Z' fld = 'BG_AMT' cnv = 'NM' hdr = 'BGAMT' )  " BG_AMT
      ( scen = 'R3' col = 90   node = 'Z' fld = 'CURRENCY' cnv = '' hdr = 'CURRENCY' )  " CURRENCY
      ( scen = 'R3' col = 91   node = 'Z' fld = 'BG_ISS_DT' cnv = 'DT' hdr = 'BGISSDT' )  " BG_ISS_DT
      ( scen = 'R3' col = 92   node = 'Z' fld = 'BG_EXP_DT' cnv = 'DT' hdr = 'BGEXPDT' )  " BG_EXP_DT
      ( scen = 'R3' col = 93   node = 'Z' fld = 'BG_ISS_BANK' cnv = '' hdr = 'BGISSBANK' )  " BG_ISS_BANK
      ( scen = 'R3' col = 94   node = 'Z' fld = 'AGGR_EXPDT' cnv = 'DT' hdr = 'AGGREXPDT' )  " AGGR_EXPDT
      ( scen = 'R3' col = 95   node = 'Z' fld = 'APPOINT_DT' cnv = 'DT' hdr = 'APPOINTDT' )  " APPOINT_DT
      ( scen = 'R3' col = 96   node = 'Z' fld = 'KDGRP' cnv = '' hdr = 'KDGRP1' )  " KDGRP1
      ( scen = 'R3' col = 97   node = 'Z' fld = 'AIOCD_CODE' cnv = '' hdr = 'AIOCDCODE' )  " AIOCD_CODE
      ( scen = 'R3' col = 98   node = 'Z' fld = 'CUST_BNK_NAME' cnv = '' hdr = 'CUSTBNKNAME' )  " CUST_BNK_NAME
      ( scen = 'R3' col = 99   node = 'Z' fld = 'DST_BOOKING' cnv = '' hdr = 'DSTBOOKING' )  " DST_BOOKING
      ( scen = 'R3' col = 100  node = 'Z' fld = 'ZTROUT' cnv = '' hdr = 'ZTROUT' )  " ZTROUT
      ( scen = 'R3' col = 101  node = 'Z' fld = 'EXTENSION' cnv = '' hdr = 'EXTENSION' )  " EXTENSION
      ( scen = 'R3' col = 102  node = 'Z' fld = 'ZCROUT' cnv = '' hdr = 'ZCROUT' )  " ZCROUT
      ( scen = 'R3' col = 103  node = 'Z' fld = 'GLN_URI_FORMAT' cnv = '' hdr = 'GLNURIFORMAT' )  " GLN_URI_FORMAT
      ( scen = 'R3' col = 104  node = 'Z' fld = 'DUNS_NUMBER' cnv = '' hdr = 'DUNSNUMBER' )  " DUNS_NUMBER
      ( scen = 'R3' col = 105  node = 'Z' fld = 'DEA_FROM_DATE' cnv = 'DT' hdr = 'DEAFROMDATE' )  " DEA_FROM_DATE
      ( scen = 'R3' col = 106  node = 'Z' fld = 'DEA_TO_DATE' cnv = 'DT' hdr = 'DEATODATE' )  " DEA_TO_DATE
      ( scen = 'R3' col = 107  node = 'Z' fld = 'ZIMP_LIC_MIA' cnv = '' hdr = 'ZIMPLICMIA' )  " ZIMP_LIC_MIA
      ( scen = 'R3' col = 108  node = 'Z' fld = 'STATE_FROM_DATE' cnv = 'DT' hdr = 'STATEFROMDATE' )  " STATE_FROM_DATE
      ( scen = 'R3' col = 109  node = 'Z' fld = 'STATE_TO_DATE' cnv = 'DT' hdr = 'STATETODATE' )  " STATE_TO_DATE
      ( scen = 'R3' col = 110  node = 'Z' fld = 'ZIMP_FROMDT_MIA' cnv = 'DT' hdr = 'ZIMPFROMDTMIA' )  " ZIMP_FROMDT_MIA
      ( scen = 'R3' col = 111  node = 'Z' fld = 'ZIMP_VALIDDT_MIA' cnv = 'DT' hdr = 'ZIMPVALIDDTMIA' )  " ZIMP_VALIDDT_MIA
    ) TO rt.

    " R4 - SAGA customer (122 columns, 114 mapped)
    APPEND LINES OF VALUE tt_map(
      ( scen = 'R4' col = 4    node = 'K' fld = 'BUKRS' cnv = '' hdr = 'BUKRS' )  " BUKRS
      ( scen = 'R4' col = 5    node = 'K' fld = 'VKORG' cnv = '' hdr = 'VKORG' )  " VKORG
      ( scen = 'R4' col = 6    node = 'K' fld = 'VTWEG' cnv = '' hdr = 'VTWEG' )  " VTWEG
      ( scen = 'R4' col = 7    node = 'K' fld = 'SPART' cnv = '' hdr = 'SPART' )  " SPART
      ( scen = 'R4' col = 8    node = 'K' fld = 'KTOKD' cnv = '' hdr = 'KTOKD' )  " KTOKD
      ( scen = 'R4' col = 10   node = 'A' fld = 'TITLE' cnv = 'TT' hdr = 'TITLEMEDI' )  " TITLE_MEDI
      ( scen = 'R4' col = 11   node = 'A' fld = 'NAME' cnv = '' hdr = 'NAME1' )  " NAME1
      ( scen = 'R4' col = 12   node = 'A' fld = 'NAME_2' cnv = '' hdr = 'NAME2' )  " NAME2
      ( scen = 'R4' col = 13   node = 'A' fld = 'NAME_3' cnv = '' hdr = 'NAME3' )  " NAME3
      ( scen = 'R4' col = 14   node = 'A' fld = 'NAME_4' cnv = '' hdr = 'NAME4' )  " NAME4
      ( scen = 'R4' col = 15   node = 'A' fld = 'SORT1' cnv = '' hdr = 'SORT1' )  " SORT1
      ( scen = 'R4' col = 16   node = 'A' fld = 'SORT2' cnv = '' hdr = 'SORT2' )  " SORT2
      ( scen = 'R4' col = 17   node = 'A' fld = 'C_O_NAME' cnv = '' hdr = 'NAMECO' )  " NAME_CO
      ( scen = 'R4' col = 18   node = 'A' fld = 'STR_SUPPL1' cnv = '' hdr = 'STRSUPPL1' )  " STR_SUPPL1
      ( scen = 'R4' col = 19   node = 'A' fld = 'STR_SUPPL2' cnv = '' hdr = 'STRSUPPL2' )  " STR_SUPPL2
      ( scen = 'R4' col = 20   node = 'A' fld = 'STREET' cnv = '' hdr = 'STREET' )  " STREET
      ( scen = 'R4' col = 21   node = 'A' fld = 'STR_SUPPL3' cnv = '' hdr = 'STRSUPPL3' )  " STR_SUPPL3
      ( scen = 'R4' col = 22   node = 'A' fld = 'LOCATION' cnv = '' hdr = 'LOCATION' )  " LOCATION
      ( scen = 'R4' col = 23   node = 'A' fld = 'DISTRICT' cnv = '' hdr = 'CITY2' )  " CITY2
      ( scen = 'R4' col = 24   node = 'A' fld = 'POSTL_COD1' cnv = '' hdr = 'POSTCODE1' )  " POST_CODE1
      ( scen = 'R4' col = 25   node = 'A' fld = 'CITY' cnv = '' hdr = 'CITY1' )  " CITY1
      ( scen = 'R4' col = 26   node = 'A' fld = 'COUNTRY' cnv = '' hdr = 'COUNTRY' )  " COUNTRY
      ( scen = 'R4' col = 27   node = 'A' fld = 'REGION' cnv = '' hdr = 'REGION' )  " REGION
      ( scen = 'R4' col = 28   node = 'A' fld = 'TIME_ZONE' cnv = '' hdr = 'TIMEZONE' )  " TIME_ZONE
      ( scen = 'R4' col = 29   node = 'A' fld = 'LANGU' cnv = '' hdr = 'LANGU' )  " LANGU
      ( scen = 'R4' col = 30   node = 'M' fld = 'TEL' cnv = '' hdr = 'TELNUMBER' )  " TEL_NUMBER
      ( scen = 'R4' col = 31   node = 'M' fld = 'MOB' cnv = '' hdr = 'MOBNUMBER' )  " MOB_NUMBER
      ( scen = 'R4' col = 32   node = 'M' fld = 'FAX' cnv = '' hdr = 'FAXNUMBER' )  " FAX_NUMBER
      ( scen = 'R4' col = 33   node = 'M' fld = 'SMT' cnv = '' hdr = 'SMTPADDR' )  " SMTP_ADDR
      ( scen = 'R4' col = 34   node = 'C' fld = 'KATR3' cnv = '' hdr = 'KATR3' )  " KATR3
      ( scen = 'R4' col = 35   node = 'C' fld = 'KATR4' cnv = '' hdr = 'KATR4' )  " KATR4
      ( scen = 'R4' col = 36   node = 'C' fld = 'LIFNR' cnv = 'AL' hdr = '' )
      ( scen = 'R4' col = 37   node = 'C' fld = 'VBUND' cnv = 'AL' hdr = 'VBUND' )  " VBUND
      ( scen = 'R4' col = 38   node = 'C' fld = 'KONZS' cnv = '' hdr = 'KONZS' )  " KONZS
      ( scen = 'R4' col = 39   node = 'C' fld = 'STCD3' cnv = '' hdr = 'STCD3' )  " STCD3
      ( scen = 'R4' col = 40   node = 'C' fld = 'STCD4' cnv = '' hdr = 'STCD4' )  " STCD4
      ( scen = 'R4' col = 41   node = 'C' fld = 'STCD5' cnv = '' hdr = 'STCD4' )  " STCD4
      ( scen = 'R4' col = 42   node = 'C' fld = 'STCEG' cnv = '' hdr = 'STCEG' )  " STCEG
      ( scen = 'R4' col = 43   node = 'C' fld = 'J_1IPANNO' cnv = '' hdr = 'J1IPANNO' )  " J_1IPANNO
      " Column 44 (AR) is headed STCD3, which column 39 already is. It was
      " mapped to the reconciliation account, which column 45 then wrote
      " over, so whatever it holds went nowhere. It is one of the SAGA
      " columns AM-AV still to be confirmed with the customer, so it is read
      " by nothing until they say what it is - a value sent to the wrong
      " field is worse than a value not sent.
      ( scen = 'R4' col = 45   node = 'B' fld = 'AKONT' cnv = 'GL' hdr = 'AKONT' )  " AKONT
      ( scen = 'R4' col = 46   node = 'B' fld = 'ZUAWA' cnv = '' hdr = 'ZUAWA' )  " ZUAWA
      ( scen = 'R4' col = 47   node = 'B' fld = 'VZSKZ' cnv = '' hdr = 'VZSKZ' )  " VZSKZ
      ( scen = 'R4' col = 48   node = 'B' fld = 'ZINRT' cnv = '' hdr = 'ZINRT' )  " ZINRT
      ( scen = 'R4' col = 49   node = 'B' fld = 'ZTERM' cnv = '' hdr = 'ZTERM' )  " ZTERM
      ( scen = 'R4' col = 50   node = 'B' fld = 'XZVER' cnv = '' hdr = 'XZVER' )  " XZVER
      ( scen = 'R4' col = 51   node = 'B' fld = 'ZWELS' cnv = '' hdr = 'ZWELS' )  " ZWELS
      ( scen = 'R4' col = 52   node = 'S' fld = 'BZIRK' cnv = '' hdr = 'BZIRK' )  " BZIRK
      ( scen = 'R4' col = 53   node = 'S' fld = 'AWAHR' cnv = '' hdr = 'AWAHR' )  " AWAHR
      ( scen = 'R4' col = 54   node = 'S' fld = 'VKBUR' cnv = '' hdr = 'VKBUR' )  " VKBUR
      ( scen = 'R4' col = 55   node = 'S' fld = 'VKGRP' cnv = '' hdr = 'VKGRP' )  " VKGRP
      ( scen = 'R4' col = 56   node = 'S' fld = 'KDGRP' cnv = '' hdr = 'KDGRP' )  " KDGRP
      ( scen = 'R4' col = 57   node = 'S' fld = 'KLABC' cnv = '' hdr = 'KLABC' )  " KLABC
      ( scen = 'R4' col = 58   node = 'S' fld = 'WAERS' cnv = '' hdr = 'WAERS' )  " WAERS
      ( scen = 'R4' col = 59   node = 'S' fld = 'KURST' cnv = '' hdr = 'KURST' )  " KURST
      ( scen = 'R4' col = 60   node = 'S' fld = 'KALKS' cnv = '' hdr = 'KALKS' )  " KALKS
      ( scen = 'R4' col = 61   node = 'S' fld = 'VERSG' cnv = '' hdr = 'VERSG' )  " VERSG
      ( scen = 'R4' col = 62   node = 'S' fld = 'LPRIO' cnv = '' hdr = 'LPRIO' )  " LPRIO
      ( scen = 'R4' col = 63   node = 'S' fld = 'KZAZU' cnv = '' hdr = 'KZAZU' )  " KZAZU
      ( scen = 'R4' col = 64   node = 'S' fld = 'VSBED' cnv = '' hdr = 'VSBED' )  " VSBED
      ( scen = 'R4' col = 65   node = 'S' fld = 'VWERK' cnv = '' hdr = 'VWERK' )  " VWERK
      ( scen = 'R4' col = 66   node = 'S' fld = 'ANTLF' cnv = '' hdr = 'ANTLF' )  " ANTLF
      ( scen = 'R4' col = 67   node = 'S' fld = 'INCO1' cnv = '' hdr = 'INCO1' )  " INCO1
      ( scen = 'R4' col = 68   node = 'S' fld = 'INCO2' cnv = '' hdr = 'INCO2' )  " INCO2
      ( scen = 'R4' col = 69   node = 'S' fld = 'ZTERM' cnv = '' hdr = 'ZTERM1' )  " ZTERM1
      ( scen = 'R4' col = 70   node = 'S' fld = 'KTGRD' cnv = '' hdr = 'KTGRD' )  " KTGRD
      ( scen = 'R4' col = 71   node = 'T' fld = '#1' cnv = '' hdr = 'TAXKD01' )  " TAXKD_01
      ( scen = 'R4' col = 76   node = 'S' fld = 'KVGR1' cnv = '' hdr = 'KVGR1' )  " KVGR1
      ( scen = 'R4' col = 77   node = 'S' fld = 'KVGR2' cnv = '' hdr = 'KVGR2' )  " KVGR2
      ( scen = 'R4' col = 78   node = 'S' fld = 'KVGR3' cnv = '' hdr = 'KVGR3' )  " KVGR3
      ( scen = 'R4' col = 79   node = 'S' fld = 'KVGR4' cnv = '' hdr = 'KVGR4' )  " KVGR4
      ( scen = 'R4' col = 80   node = 'S' fld = 'KVGR5' cnv = '' hdr = 'KVGR5' )  " KVGR5
      ( scen = 'R4' col = 81   node = 'Z' fld = 'WERKS' cnv = '' hdr = 'WERKS' )  " WERKS
      ( scen = 'R4' col = 82   node = 'Z' fld = 'CUST_TRNST_DAYS' cnv = 'NM' hdr = 'CUSTTRNSTDAYS' )  " CUST_TRNST_DAYS
      ( scen = 'R4' col = 83   node = 'Z' fld = 'KMSUM' cnv = 'NM' hdr = 'KMSUM' )  " KMSUM
      ( scen = 'R4' col = 84   node = 'Z' fld = 'DRUGLICENSE1' cnv = '' hdr = 'DRUGLICENSE1' )  " DRUGLICENSE1
      ( scen = 'R4' col = 85   node = 'Z' fld = 'DRUGLICENSE2' cnv = '' hdr = 'DRUGLICENSE2' )  " DRUGLICENSE2
      ( scen = 'R4' col = 86   node = 'Z' fld = 'DL1_DL2_VALIDDT' cnv = 'DT' hdr = 'DL1DL2VALIDDT' )  " DL1_DL2_VALIDDT
      ( scen = 'R4' col = 87   node = 'Z' fld = 'FOODSLICENSE' cnv = '' hdr = 'FOODSLICENSE' )  " FOODSLICENSE
      ( scen = 'R4' col = 88   node = 'Z' fld = 'FL_VALIDDT' cnv = 'DT' hdr = 'FLVALIDDT' )  " FL_VALIDDT
      ( scen = 'R4' col = 89   node = 'Z' fld = 'SCHXNO' cnv = '' hdr = 'SCHXNO' )  " SCHXNO
      ( scen = 'R4' col = 90   node = 'Z' fld = 'SCHX_VALIDDT' cnv = 'DT' hdr = 'SCHXVALIDDT' )  " SCHX_VALIDDT
      ( scen = 'R4' col = 91   node = 'Z' fld = 'SCHXRNO' cnv = '' hdr = 'SCHXRNO' )  " SCHXRNO
      ( scen = 'R4' col = 92   node = 'Z' fld = 'SCHXR_VALIDDT' cnv = 'DT' hdr = 'SCHXRVALIDDT' )  " SCHXR_VALIDDT
      ( scen = 'R4' col = 93   node = 'Z' fld = 'RETAIL_LIC_NO' cnv = '' hdr = 'RETAILLICNO' )  " RETAIL_LIC_NO
      ( scen = 'R4' col = 94   node = 'Z' fld = 'RETAIL_EXP' cnv = 'DT' hdr = 'RETAILEXP' )  " RETAIL_EXP
      ( scen = 'R4' col = 95   node = 'Z' fld = 'MFGLIC1NO' cnv = '' hdr = 'MFGLIC1NO' )  " MFGLIC1NO
      ( scen = 'R4' col = 96   node = 'Z' fld = 'MFGLIC2NO' cnv = '' hdr = 'MFGLIC2NO' )  " MFGLIC2NO
      ( scen = 'R4' col = 97   node = 'Z' fld = 'MFGLIC3NO' cnv = '' hdr = 'MFGLIC3NO' )  " MFGLIC3NO
      ( scen = 'R4' col = 98   node = 'Z' fld = 'BGYN' cnv = '' hdr = 'BGYN' )  " BGYN
      ( scen = 'R4' col = 99   node = 'Z' fld = 'BG_NO' cnv = '' hdr = 'BGNO' )  " BG_NO
      ( scen = 'R4' col = 100  node = 'Z' fld = 'BG_AMT' cnv = 'NM' hdr = 'BGAMT' )  " BG_AMT
      ( scen = 'R4' col = 101  node = 'Z' fld = 'CURRENCY' cnv = '' hdr = 'CURRENCY' )  " CURRENCY
      ( scen = 'R4' col = 102  node = 'Z' fld = 'BG_ISS_DT' cnv = 'DT' hdr = 'BGISSDT' )  " BG_ISS_DT
      ( scen = 'R4' col = 103  node = 'Z' fld = 'BG_EXP_DT' cnv = 'DT' hdr = 'BGEXPDT' )  " BG_EXP_DT
      ( scen = 'R4' col = 104  node = 'Z' fld = 'BG_ISS_BANK' cnv = '' hdr = 'BGISSBANK' )  " BG_ISS_BANK
      ( scen = 'R4' col = 105  node = 'Z' fld = 'AGGR_EXPDT' cnv = 'DT' hdr = 'AGGREXPDT' )  " AGGR_EXPDT
      ( scen = 'R4' col = 106  node = 'Z' fld = 'APPOINT_DT' cnv = 'DT' hdr = 'APPOINTDT' )  " APPOINT_DT
      ( scen = 'R4' col = 107  node = 'Z' fld = 'KDGRP' cnv = '' hdr = 'KDGRP1' )  " KDGRP1
      ( scen = 'R4' col = 108  node = 'Z' fld = 'AIOCD_CODE' cnv = '' hdr = 'AIOCDCODE' )  " AIOCD_CODE
      ( scen = 'R4' col = 109  node = 'Z' fld = 'CUST_BNK_NAME' cnv = '' hdr = 'CUSTBNKNAME' )  " CUST_BNK_NAME
      ( scen = 'R4' col = 110  node = 'Z' fld = 'DST_BOOKING' cnv = '' hdr = 'DSTBOOKING' )  " DST_BOOKING
      ( scen = 'R4' col = 111  node = 'Z' fld = 'ZTROUT' cnv = '' hdr = 'ZTROUT' )  " ZTROUT
      ( scen = 'R4' col = 112  node = 'Z' fld = 'EXTENSION' cnv = '' hdr = 'EXTENSION' )  " EXTENSION
      ( scen = 'R4' col = 113  node = 'Z' fld = 'ZCROUT' cnv = '' hdr = 'ZCROUT' )  " ZCROUT
      ( scen = 'R4' col = 114  node = 'Z' fld = 'GLN_URI_FORMAT' cnv = '' hdr = 'GLNURIFORMAT' )  " GLN_URI_FORMAT
      ( scen = 'R4' col = 115  node = 'Z' fld = 'DUNS_NUMBER' cnv = '' hdr = 'DUNSNUMBER' )  " DUNS_NUMBER
      ( scen = 'R4' col = 116  node = 'Z' fld = 'DEA_FROM_DATE' cnv = 'DT' hdr = 'DEAFROMDATE' )  " DEA_FROM_DATE
      ( scen = 'R4' col = 117  node = 'Z' fld = 'DEA_TO_DATE' cnv = 'DT' hdr = 'DEATODATE' )  " DEA_TO_DATE
      ( scen = 'R4' col = 118  node = 'Z' fld = 'ZIMP_LIC_MIA' cnv = '' hdr = 'ZIMPLICMIA' )  " ZIMP_LIC_MIA
      ( scen = 'R4' col = 119  node = 'Z' fld = 'STATE_FROM_DATE' cnv = 'DT' hdr = 'STATEFROMDATE' )  " STATE_FROM_DATE
      ( scen = 'R4' col = 120  node = 'Z' fld = 'STATE_TO_DATE' cnv = 'DT' hdr = 'STATETODATE' )  " STATE_TO_DATE
      ( scen = 'R4' col = 121  node = 'Z' fld = 'ZIMP_FROMDT_MIA' cnv = 'DT' hdr = 'ZIMPFROMDTMIA' )  " ZIMP_FROMDT_MIA
      ( scen = 'R4' col = 122  node = 'Z' fld = 'ZIMP_VALIDDT_MIA' cnv = 'DT' hdr = 'ZIMPVALIDDTMIA' )  " ZIMP_VALIDDT_MIA
    ) TO rt.

    " R5 - credit Limit (18 columns, 11 mapped)
    APPEND LINES OF VALUE tt_map(
      ( scen = 'R5' col = 1    node = 'K' fld = 'KUNNR' cnv = 'AL' hdr = 'CUSTOMERCODE' )  " Customer code
      ( scen = 'R5' col = 2    node = 'U' fld = 'SEGMENT' cnv = '' hdr = 'KKBER' )  " KKBER
      ( scen = 'R5' col = 8    node = 'U' fld = 'LIMIT_MAIN' cnv = 'NM' hdr = 'KLIMG' )  " KLIMG
      ( scen = 'R5' col = 9    node = 'U' fld = 'LIMIT_SGM' cnv = 'NM' hdr = 'KLIME' )  " KLIME
      ( scen = 'R5' col = 10   node = 'U' fld = 'CURRENCY' cnv = '' hdr = 'WAERS' )  " WAERS
      ( scen = 'R5' col = 11   node = 'U' fld = 'LIMIT_SGM' cnv = 'NM' hdr = 'KLIMK' )  " KLIMK
      ( scen = 'R5' col = 12   node = 'U' fld = 'RISK_CLASS' cnv = '' hdr = 'CTLPC' )  " CTLPC
      ( scen = 'R5' col = 13   node = 'U' fld = 'XBLOCKED' cnv = '' hdr = 'CRBLB' )  " CRBLB
      ( scen = 'R5' col = 16   node = 'B' fld = 'ZTERM' cnv = '' hdr = 'PAYMENTTERMS' )  " Payment Terms
      ( scen = 'R5' col = 17   node = 'S' fld = 'KVGR3' cnv = '' hdr = 'CUSTGRP3' )  " Cust Grp 3
      ( scen = 'R5' col = 18   node = 'B' fld = 'VZSKZ' cnv = '' hdr = 'Z1INTERESTINDICATORCYCLEZINRTSEEHANDLER' )  " Z1 Interest Indicator (cycle -> ZINRT, see handler)
    ) TO rt.

    " R6 - domestic customer US (75 columns, 73 mapped)
    APPEND LINES OF VALUE tt_map(
      ( scen = 'R6' col = 1    node = 'K' fld = 'KUNNR' cnv = 'AL' hdr = 'CUSTOMERCODE' )  " Customer code
      ( scen = 'R6' col = 3    node = 'K' fld = 'BUKRS' cnv = '' hdr = 'COMPANYCODE' )  " Company Code
      ( scen = 'R6' col = 4    node = 'K' fld = 'VKORG' cnv = '' hdr = 'SALESORGANIZATION' )  " Sales Organization
      ( scen = 'R6' col = 5    node = 'K' fld = 'VTWEG' cnv = '' hdr = 'DISTRIBUTIONCHANNEL' )  " Distribution Channel
      ( scen = 'R6' col = 6    node = 'K' fld = 'SPART' cnv = '' hdr = 'DIVISION' )  " Division
      ( scen = 'R6' col = 7    node = 'K' fld = 'KTOKD' cnv = '' hdr = 'CUSTOMERACCOUNTGROUP' )  " Customer Account Group
      ( scen = 'R6' col = 9    node = 'A' fld = 'TITLE' cnv = 'TT' hdr = 'TITLETEXT' )  " Title text
      ( scen = 'R6' col = 10   node = 'A' fld = 'NAME' cnv = '' hdr = 'NAME1' )  " Name 1
      ( scen = 'R6' col = 11   node = 'A' fld = 'NAME_2' cnv = '' hdr = 'NAME2' )  " Name 2
      ( scen = 'R6' col = 12   node = 'A' fld = 'NAME_3' cnv = '' hdr = 'NAME3' )  " Name 3
      ( scen = 'R6' col = 13   node = 'A' fld = 'NAME_4' cnv = '' hdr = 'NAME4' )  " Name 4
      ( scen = 'R6' col = 14   node = 'A' fld = 'SORT1' cnv = '' hdr = 'SEARCHTERM1' )  " Search Term 1
      ( scen = 'R6' col = 15   node = 'A' fld = 'SORT2' cnv = '' hdr = 'SEARCHTERM2' )  " Search Term 2
      ( scen = 'R6' col = 16   node = 'A' fld = 'STR_SUPPL1' cnv = '' hdr = 'STREET2' )  " Street 2
      ( scen = 'R6' col = 17   node = 'A' fld = 'STR_SUPPL2' cnv = '' hdr = 'STREET3' )  " Street 3
      ( scen = 'R6' col = 18   node = 'A' fld = 'STREET' cnv = '' hdr = 'STREET' )  " Street
      ( scen = 'R6' col = 19   node = 'A' fld = 'STR_SUPPL3' cnv = '' hdr = 'STREET4' )  " Street 4
      ( scen = 'R6' col = 20   node = 'A' fld = 'LOCATION' cnv = '' hdr = 'STREET5' )  " Street 5
      ( scen = 'R6' col = 21   node = 'A' fld = 'DISTRICT' cnv = '' hdr = 'DISTRICT' )  " District
      ( scen = 'R6' col = 22   node = 'A' fld = 'POSTL_COD1' cnv = '' hdr = 'CITYPOSTALCODE' )  " City postal code
      ( scen = 'R6' col = 23   node = 'A' fld = 'CITY' cnv = '' hdr = 'CITY' )  " City
      ( scen = 'R6' col = 24   node = 'A' fld = 'COUNTRY' cnv = '' hdr = 'COUNTRYKEY' )  " Country Key
      ( scen = 'R6' col = 25   node = 'A' fld = 'REGION' cnv = '' hdr = 'REGIONSTATEPROVINCECOUNTY' )  " Region (State, Province, County)
      ( scen = 'R6' col = 26   node = 'A' fld = 'LANGU' cnv = '' hdr = 'LANGUAGEKEY' )  " Language Key
      ( scen = 'R6' col = 27   node = 'M' fld = 'TEL' cnv = '' hdr = 'FIRSTTELEPHONENODIALLINGCOD' )  " First telephone no.: dialling cod
      ( scen = 'R6' col = 28   node = 'M' fld = 'MOB' cnv = '' hdr = 'FIRSTMOBILETELEPHONENODIALI' )  " First Mobile Telephone No.: Diali
      ( scen = 'R6' col = 29   node = 'M' fld = 'FAX' cnv = '' hdr = 'FIRSTFAXNODIALLINGCODENUMB' )  " First fax no.: dialling code+numb
      ( scen = 'R6' col = 30   node = 'M' fld = 'SMT' cnv = '' hdr = 'EMAILADDRESS' )  " E-Mail Address
      ( scen = 'R6' col = 31   node = 'C' fld = 'KONZS' cnv = '' hdr = 'GROUPKEY' )  " Group key
      ( scen = 'R6' col = 32   node = 'C' fld = 'KATR3' cnv = '' hdr = 'ATTRIBUTE3' )  " Attribute 3
      ( scen = 'R6' col = 33   node = 'C' fld = 'KATR4' cnv = '' hdr = 'ATTRIBUTE4' )  " Attribute 4
      ( scen = 'R6' col = 34   node = 'C' fld = 'CIVVE' cnv = '' hdr = 'IDFORMAINLYNONMILITARYUSE' )  " ID for mainly non-military use
      ( scen = 'R6' col = 35   node = 'B' fld = 'AKONT' cnv = 'GL' hdr = 'RECONCILIATIONACCOUNTINGENERAL' )  " Reconciliation Account in General
      ( scen = 'R6' col = 36   node = 'B' fld = 'ZUAWA' cnv = '' hdr = 'KEYFORSORTINGACCORDINGTOASSI' )  " Key for sorting according to assi
      ( scen = 'R6' col = 37   node = 'B' fld = 'FDGRV' cnv = 'AL' hdr = 'PLANNINGGROUP' )  " Planning group
      ( scen = 'R6' col = 38   node = 'B' fld = 'VZSKZ' cnv = '' hdr = 'INTERESTCALCULATIONINDICATOR' )  " Interest calculation indicator
      ( scen = 'R6' col = 39   node = 'B' fld = 'ZINRT' cnv = '' hdr = 'INTERESTCALCULATIONFREQUENCYIN' )  " Interest calculation frequency in
      ( scen = 'R6' col = 40   node = 'B' fld = 'ZTERM' cnv = '' hdr = 'TERMSOFPAYMENTKEY' )  " Terms of Payment Key
      ( scen = 'R6' col = 41   node = 'B' fld = 'XZVER' cnv = '' hdr = 'INDICATORRECORDPAYMENTHISTORY' )  " Indicator: Record Payment History
      ( scen = 'R6' col = 42   node = 'B' fld = 'ZWELS' cnv = '' hdr = 'LISTOFTHEPAYMENTMETHODSTOBE' )  " List of the Payment Methods to be
      ( scen = 'R6' col = 43   node = 'S' fld = 'BZIRK' cnv = '' hdr = 'SALESDISTRICT' )  " Sales district
      ( scen = 'R6' col = 44   node = 'S' fld = 'AWAHR' cnv = '' hdr = 'ORDERPROBABILITYOFTHEITEM' )  " Order probability of the item
      ( scen = 'R6' col = 45   node = 'S' fld = 'VKBUR' cnv = '' hdr = 'SALESOFFICE' )  " Sales Office
      ( scen = 'R6' col = 46   node = 'S' fld = 'VKGRP' cnv = '' hdr = 'SALESGROUP' )  " Sales Group
      ( scen = 'R6' col = 47   node = 'S' fld = 'KDGRP' cnv = '' hdr = 'CUSTOMERGROUP' )  " Customer group
      ( scen = 'R6' col = 48   node = 'S' fld = 'KLABC' cnv = '' hdr = 'CUSTOMERCLASSIFICATIONABCANAL' )  " Customer classification (ABC anal
      ( scen = 'R6' col = 49   node = 'S' fld = 'WAERS' cnv = '' hdr = 'CURRENCY' )  " Currency
      ( scen = 'R6' col = 50   node = 'S' fld = 'KALKS' cnv = '' hdr = 'PRICINGPROCEDUREASSIGNEDTOTHI' )  " Pricing procedure assigned to thi
      ( scen = 'R6' col = 51   node = 'S' fld = 'VERSG' cnv = '' hdr = 'CUSTOMERSTATISTICSGROUP' )  " Customer Statistics Group
      ( scen = 'R6' col = 52   node = 'S' fld = 'LPRIO' cnv = '' hdr = 'DELIVERYPRIORITY' )  " Delivery Priority
      ( scen = 'R6' col = 53   node = 'S' fld = 'KZAZU' cnv = '' hdr = 'ORDERCOMBINATIONINDICATOR' )  " Order Combination Indicator
      ( scen = 'R6' col = 54   node = 'S' fld = 'VSBED' cnv = '' hdr = 'SHIPPINGCONDITIONS' )  " Shipping Conditions
      ( scen = 'R6' col = 55   node = 'S' fld = 'VWERK' cnv = '' hdr = 'DELIVERINGPLANTOWNOREXTERNAL' )  " Delivering Plant (Own or External
      ( scen = 'R6' col = 56   node = 'S' fld = 'ANTLF' cnv = '' hdr = 'MAXIMUMNUMBEROFPARTIALDELIVER' )  " Maximum Number of Partial Deliver
      ( scen = 'R6' col = 57   node = 'S' fld = 'INCO1' cnv = '' hdr = 'INCOTERMSPART1' )  " Incoterms (Part 1)
      ( scen = 'R6' col = 58   node = 'S' fld = 'INCO2' cnv = '' hdr = 'INCOTERMSPART2' )  " Incoterms (Part 2)
      ( scen = 'R6' col = 59   node = 'S' fld = 'ZTERM' cnv = '' hdr = 'TERMSOFPAYMENTKEY' )  " Terms of Payment Key - sales area, KNVV
      ( scen = 'R6' col = 60   node = 'S' fld = 'KTGRD' cnv = '' hdr = 'CUSTOMERACCOUNTASSIGNMENTGROUP' )  " Customer Account Assignment Group
      ( scen = 'R6' col = 61   node = 'T' fld = 'UTXJ' cnv = '' hdr = 'TAXCLASSIFICATIONFORCUSTOMER' )  " Tax classification for customer
      ( scen = 'R6' col = 62   node = 'T' fld = 'UTX2' cnv = '' hdr = 'TAXCLASSIFICATIONFORCUSTOMER' )  " Tax classification for customer
      ( scen = 'R6' col = 63   node = 'T' fld = 'UTX3' cnv = '' hdr = 'TAXCLASSIFICATIONFORCUSTOMER' )  " Tax classification for customer
      ( scen = 'R6' col = 64   node = 'T' fld = 'MWST' cnv = '' hdr = 'TAXCLASSIFICATIONFORCUSTOMER' )  " Tax classification for customer
      ( scen = 'R6' col = 65   node = 'S' fld = 'KVGR1' cnv = '' hdr = 'CUSTOMERGROUP1' )  " Customer group 1
      ( scen = 'R6' col = 66   node = 'S' fld = 'KVGR2' cnv = '' hdr = 'CUSTOMERGROUP2' )  " Customer group 2
      ( scen = 'R6' col = 67   node = 'S' fld = 'KVGR3' cnv = '' hdr = 'CUSTOMERGROUP3' )  " Customer group 3
      ( scen = 'R6' col = 68   node = 'S' fld = 'KVGR4' cnv = '' hdr = 'CUSTOMERGROUP4' )  " Customer group 4
      ( scen = 'R6' col = 69   node = 'S' fld = 'KVGR5' cnv = '' hdr = 'CUSTOMERGROUP5' )  " Customer group 5
      ( scen = 'R6' col = 70   node = 'Z' fld = 'WERKS' cnv = '' hdr = 'PLANT' )  " Plant
      ( scen = 'R6' col = 71   node = 'Z' fld = 'DRUGLICENSE1' cnv = '' hdr = '20BLICNO' )  " 20B. Lic. No
      ( scen = 'R6' col = 72   node = 'Z' fld = 'DL1_ISSUEDT' cnv = 'DT' hdr = '20BISSUEDATE' )  " 20B Issue Date
      ( scen = 'R6' col = 73   node = 'Z' fld = 'DL1_VALIDDT' cnv = 'DT' hdr = '20BEXPIRYDATE' )  " 20B Expiry Date
      ( scen = 'R6' col = 74   node = 'Z' fld = 'DEA_FROM_DATE' cnv = 'DT' hdr = 'DEAFROMDATE' )  " DEA From Date
      ( scen = 'R6' col = 75   node = 'Z' fld = 'DEA_TO_DATE' cnv = 'DT' hdr = 'DEATODATE' )  " DEA To Date
    ) TO rt.

    " R7 - ship to party US (75 columns, 73 mapped)
    APPEND LINES OF VALUE tt_map(
      ( scen = 'R7' col = 1    node = 'K' fld = 'KUNNR' cnv = 'AL' hdr = 'CUSTOMERCODE' )  " Customer code
      ( scen = 'R7' col = 3    node = 'K' fld = 'BUKRS' cnv = '' hdr = 'COMPANYCODE' )  " Company Code
      ( scen = 'R7' col = 4    node = 'K' fld = 'VKORG' cnv = '' hdr = 'SALESORGANIZATION' )  " Sales Organization
      ( scen = 'R7' col = 5    node = 'K' fld = 'VTWEG' cnv = '' hdr = 'DISTRIBUTIONCHANNEL' )  " Distribution Channel
      ( scen = 'R7' col = 6    node = 'K' fld = 'SPART' cnv = '' hdr = 'DIVISION' )  " Division
      ( scen = 'R7' col = 7    node = 'K' fld = 'KTOKD' cnv = '' hdr = 'CUSTOMERACCOUNTGROUP' )  " Customer Account Group
      ( scen = 'R7' col = 9    node = 'A' fld = 'TITLE' cnv = 'TT' hdr = 'TITLETEXT' )  " Title text
      ( scen = 'R7' col = 10   node = 'A' fld = 'NAME' cnv = '' hdr = 'NAME1' )  " Name 1
      ( scen = 'R7' col = 11   node = 'A' fld = 'NAME_2' cnv = '' hdr = 'NAME2' )  " Name 2
      ( scen = 'R7' col = 12   node = 'A' fld = 'NAME_3' cnv = '' hdr = 'NAME3' )  " Name 3
      ( scen = 'R7' col = 13   node = 'A' fld = 'NAME_4' cnv = '' hdr = 'NAME4' )  " Name 4
      ( scen = 'R7' col = 14   node = 'A' fld = 'SORT1' cnv = '' hdr = 'SEARCHTERM1' )  " Search Term 1
      ( scen = 'R7' col = 15   node = 'A' fld = 'SORT2' cnv = '' hdr = 'SEARCHTERM2' )  " Search Term 2
      ( scen = 'R7' col = 16   node = 'A' fld = 'STR_SUPPL1' cnv = '' hdr = 'STREET2' )  " Street 2
      ( scen = 'R7' col = 17   node = 'A' fld = 'STR_SUPPL2' cnv = '' hdr = 'STREET3' )  " Street 3
      ( scen = 'R7' col = 18   node = 'A' fld = 'STREET' cnv = '' hdr = 'STREET' )  " Street
      ( scen = 'R7' col = 19   node = 'A' fld = 'STR_SUPPL3' cnv = '' hdr = 'STREET4' )  " Street 4
      ( scen = 'R7' col = 20   node = 'A' fld = 'LOCATION' cnv = '' hdr = 'STREET5' )  " Street 5
      ( scen = 'R7' col = 21   node = 'A' fld = 'DISTRICT' cnv = '' hdr = 'DISTRICT' )  " District
      ( scen = 'R7' col = 22   node = 'A' fld = 'POSTL_COD1' cnv = '' hdr = 'CITYPOSTALCODE' )  " City postal code
      ( scen = 'R7' col = 23   node = 'A' fld = 'CITY' cnv = '' hdr = 'CITY' )  " City
      ( scen = 'R7' col = 24   node = 'A' fld = 'COUNTRY' cnv = '' hdr = 'COUNTRYKEY' )  " Country Key
      ( scen = 'R7' col = 25   node = 'A' fld = 'REGION' cnv = '' hdr = 'REGIONSTATEPROVINCECOUNTY' )  " Region (State, Province, County)
      ( scen = 'R7' col = 26   node = 'A' fld = 'LANGU' cnv = '' hdr = 'LANGUAGEKEY' )  " Language Key
      ( scen = 'R7' col = 27   node = 'M' fld = 'TEL' cnv = '' hdr = 'FIRSTTELEPHONENODIALLINGCOD' )  " First telephone no.: dialling cod
      ( scen = 'R7' col = 28   node = 'M' fld = 'MOB' cnv = '' hdr = 'FIRSTMOBILETELEPHONENODIALI' )  " First Mobile Telephone No.: Diali
      ( scen = 'R7' col = 29   node = 'M' fld = 'FAX' cnv = '' hdr = 'FIRSTFAXNODIALLINGCODENUMB' )  " First fax no.: dialling code+numb
      ( scen = 'R7' col = 30   node = 'M' fld = 'SMT' cnv = '' hdr = 'EMAILADDRESS' )  " E-Mail Address
      ( scen = 'R7' col = 31   node = 'C' fld = 'KONZS' cnv = '' hdr = 'GROUPKEY' )  " Group key
      ( scen = 'R7' col = 32   node = 'C' fld = 'KATR3' cnv = '' hdr = 'ATTRIBUTE3' )  " Attribute 3
      ( scen = 'R7' col = 33   node = 'C' fld = 'KATR4' cnv = '' hdr = 'ATTRIBUTE4' )  " Attribute 4
      ( scen = 'R7' col = 34   node = 'C' fld = 'CIVVE' cnv = '' hdr = 'IDFORMAINLYNONMILITARYUSE' )  " ID for mainly non-military use
      ( scen = 'R7' col = 35   node = 'B' fld = 'AKONT' cnv = 'GL' hdr = 'RECONCILIATIONACCOUNTINGENERAL' )  " Reconciliation Account in General
      ( scen = 'R7' col = 36   node = 'B' fld = 'ZUAWA' cnv = '' hdr = 'KEYFORSORTINGACCORDINGTOASSI' )  " Key for sorting according to assi
      ( scen = 'R7' col = 37   node = 'B' fld = 'FDGRV' cnv = 'AL' hdr = 'PLANNINGGROUP' )  " Planning group
      ( scen = 'R7' col = 38   node = 'B' fld = 'VZSKZ' cnv = '' hdr = 'INTERESTCALCULATIONINDICATOR' )  " Interest calculation indicator
      ( scen = 'R7' col = 39   node = 'B' fld = 'ZINRT' cnv = '' hdr = 'INTERESTCALCULATIONFREQUENCYIN' )  " Interest calculation frequency in
      ( scen = 'R7' col = 40   node = 'B' fld = 'ZTERM' cnv = '' hdr = 'TERMSOFPAYMENTKEY' )  " Terms of Payment Key
      ( scen = 'R7' col = 41   node = 'B' fld = 'XZVER' cnv = '' hdr = 'INDICATORRECORDPAYMENTHISTORY' )  " Indicator: Record Payment History
      ( scen = 'R7' col = 42   node = 'B' fld = 'ZWELS' cnv = '' hdr = 'LISTOFTHEPAYMENTMETHODSTOBE' )  " List of the Payment Methods to be
      ( scen = 'R7' col = 43   node = 'S' fld = 'BZIRK' cnv = '' hdr = 'SALESDISTRICT' )  " Sales district
      ( scen = 'R7' col = 44   node = 'S' fld = 'AWAHR' cnv = '' hdr = 'ORDERPROBABILITYOFTHEITEM' )  " Order probability of the item
      ( scen = 'R7' col = 45   node = 'S' fld = 'VKBUR' cnv = '' hdr = 'SALESOFFICE' )  " Sales Office
      ( scen = 'R7' col = 46   node = 'S' fld = 'VKGRP' cnv = '' hdr = 'SALESGROUP' )  " Sales Group
      ( scen = 'R7' col = 47   node = 'S' fld = 'KDGRP' cnv = '' hdr = 'CUSTOMERGROUP' )  " Customer group
      ( scen = 'R7' col = 48   node = 'S' fld = 'KLABC' cnv = '' hdr = 'CUSTOMERCLASSIFICATIONABCANAL' )  " Customer classification (ABC anal
      ( scen = 'R7' col = 49   node = 'S' fld = 'WAERS' cnv = '' hdr = 'CURRENCY' )  " Currency
      ( scen = 'R7' col = 50   node = 'S' fld = 'KALKS' cnv = '' hdr = 'PRICINGPROCEDUREASSIGNEDTOTHI' )  " Pricing procedure assigned to thi
      ( scen = 'R7' col = 51   node = 'S' fld = 'VERSG' cnv = '' hdr = 'CUSTOMERSTATISTICSGROUP' )  " Customer Statistics Group
      ( scen = 'R7' col = 52   node = 'S' fld = 'LPRIO' cnv = '' hdr = 'DELIVERYPRIORITY' )  " Delivery Priority
      ( scen = 'R7' col = 53   node = 'S' fld = 'KZAZU' cnv = '' hdr = 'ORDERCOMBINATIONINDICATOR' )  " Order Combination Indicator
      ( scen = 'R7' col = 54   node = 'S' fld = 'VSBED' cnv = '' hdr = 'SHIPPINGCONDITIONS' )  " Shipping Conditions
      ( scen = 'R7' col = 55   node = 'S' fld = 'VWERK' cnv = '' hdr = 'DELIVERINGPLANTOWNOREXTERNAL' )  " Delivering Plant (Own or External
      ( scen = 'R7' col = 56   node = 'S' fld = 'ANTLF' cnv = '' hdr = 'MAXIMUMNUMBEROFPARTIALDELIVER' )  " Maximum Number of Partial Deliver
      ( scen = 'R7' col = 57   node = 'S' fld = 'INCO1' cnv = '' hdr = 'INCOTERMSPART1' )  " Incoterms (Part 1)
      ( scen = 'R7' col = 58   node = 'S' fld = 'INCO2' cnv = '' hdr = 'INCOTERMSPART2' )  " Incoterms (Part 2)
      ( scen = 'R7' col = 59   node = 'S' fld = 'ZTERM' cnv = '' hdr = 'TERMSOFPAYMENTKEY' )  " Terms of Payment Key - sales area, KNVV
      ( scen = 'R7' col = 60   node = 'S' fld = 'KTGRD' cnv = '' hdr = 'CUSTOMERACCOUNTASSIGNMENTGROUP' )  " Customer Account Assignment Group
      ( scen = 'R7' col = 61   node = 'T' fld = 'UTXJ' cnv = '' hdr = 'TAXCLASSIFICATIONFORCUSTOMER' )  " Tax classification for customer
      ( scen = 'R7' col = 62   node = 'T' fld = 'UTX2' cnv = '' hdr = 'TAXCLASSIFICATIONFORCUSTOMER' )  " Tax classification for customer
      ( scen = 'R7' col = 63   node = 'T' fld = 'UTX3' cnv = '' hdr = 'TAXCLASSIFICATIONFORCUSTOMER' )  " Tax classification for customer
      ( scen = 'R7' col = 64   node = 'T' fld = 'MWST' cnv = '' hdr = 'TAXCLASSIFICATIONFORCUSTOMER' )  " Tax classification for customer
      ( scen = 'R7' col = 65   node = 'S' fld = 'KVGR1' cnv = '' hdr = 'CUSTOMERGROUP1' )  " Customer group 1
      ( scen = 'R7' col = 66   node = 'S' fld = 'KVGR2' cnv = '' hdr = 'CUSTOMERGROUP2' )  " Customer group 2
      ( scen = 'R7' col = 67   node = 'S' fld = 'KVGR3' cnv = '' hdr = 'CUSTOMERGROUP3' )  " Customer group 3
      ( scen = 'R7' col = 68   node = 'S' fld = 'KVGR4' cnv = '' hdr = 'CUSTOMERGROUP4' )  " Customer group 4
      ( scen = 'R7' col = 69   node = 'S' fld = 'KVGR5' cnv = '' hdr = 'CUSTOMERGROUP5' )  " Customer group 5
      ( scen = 'R7' col = 70   node = 'Z' fld = 'WERKS' cnv = '' hdr = 'PLANT' )  " Plant
      ( scen = 'R7' col = 71   node = 'Z' fld = 'DRUGLICENSE1' cnv = '' hdr = '20BLICNO' )  " 20B. Lic. No
      ( scen = 'R7' col = 72   node = 'Z' fld = 'DL1_ISSUEDT' cnv = 'DT' hdr = '20BISSUEDATE' )  " 20B Issue Date
      ( scen = 'R7' col = 73   node = 'Z' fld = 'DL1_VALIDDT' cnv = 'DT' hdr = '20BEXPIRYDATE' )  " 20B Expiry Date
      ( scen = 'R7' col = 74   node = 'Z' fld = 'DEA_FROM_DATE' cnv = 'DT' hdr = 'DEAFROMDATE' )  " DEA From Date
      ( scen = 'R7' col = 75   node = 'Z' fld = 'DEA_TO_DATE' cnv = 'DT' hdr = 'DEATODATE' )  " DEA To Date
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

    " The business partner keeps a global memory for the logical unit of work
    " that has just been closed - including the save mode. A COMMIT does not
    " clear it, and the next row is then refused with "Parameter IV_X_SAVE is
    " ' ' for FM BUPA_CREATE_FROM_DATA. It should be 'A'". Initialising the
    " memory gives every row a clean start.
    METHODS reset_bp.
ENDCLASS.

CLASS lcl_cvis IMPLEMENTATION.

  METHOD constructor.
    mo_log = io_log.
  ENDMETHOD.

  METHOD reset_bp.
    TRY.
        CALL FUNCTION 'BUP_MEMORY_CENTRAL_INIT'.
      CATCH cx_sy_dyn_call_illegal_func.
        RETURN.
    ENDTRY.
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
        reset_bp( ).
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
      reset_bp( ).
      RETURN.
    ENDIF.

    IF p_test = abap_true.
      ROLLBACK WORK.
      reset_bp( ).
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'S'
                   iv_text = 'Test run OK - customer would be posted' ).
    ELSE.
      " BAPI_TRANSACTION_COMMIT, not a bare COMMIT WORK: the business
      " partner hangs its own end-of-LUW processing off it.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = abap_true.
      reset_bp( ).
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

    " The licence table has amounts and day counts in it, so the same guard
    " applies here: a cell that will not convert is logged, not dumped.
    TRY.
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
      CATCH cx_sy_conversion_error.
        mo_log->add( iv_row = iv_row iv_type = 'E'
                     iv_struc = 'ZSD_LICENSE_CHK' iv_fld = iv_fld
                     iv_text = |"{ lv_in }" does not fit { iv_fld }| ).
        RETURN.
    ENDTRY.
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
    " Same reason as in LCL_CVIS: the partner memory has to be initialised
    " once the unit of work is closed, or the next row is refused.
    METHODS reset_bp.
ENDCLASS.

CLASS lcl_credit IMPLEMENTATION.

  METHOD reset_bp.
    TRY.
        CALL FUNCTION 'BUP_MEMORY_CENTRAL_INIT'.
      CATCH cx_sy_dyn_call_illegal_func.
        RETURN.
    ENDTRY.
  ENDMETHOD.

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
          reset_bp( ).
          mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'S'
                       iv_text = |Test run OK - segment { iv_sgmnt } would be updated| ).
        ELSE.
          " SAVE_ALL alone does not commit - the BAPI commit is required.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = abap_true.
          reset_bp( ).
          mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'S'
                       iv_text = |Credit data updated for segment { iv_sgmnt }| ).
        ENDIF.

      CATCH cx_root INTO DATA(lx).
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        reset_bp( ).
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

    " The headings this scenario expects, as matching keys. Used to find the
    " right tab whatever it is called.
    METHODS headings RETURNING VALUE(rt) TYPE string_table.

    " Re-points every map entry at the column that actually carries its
    " heading in this file. Entries whose heading is blank, duplicated or
    " absent keep the position they were built with.
    METHODS bind_columns IMPORTING it_head TYPE string_table.

  PRIVATE SECTION.
    DATA mv_scen  TYPE char2.
    DATA mo_log   TYPE REF TO lcl_log.
    DATA mo_cvis  TYPE REF TO lcl_cvis.
    DATA mo_lic   TYPE REF TO lcl_lic.
    DATA mo_cred  TYPE REF TO lcl_credit.
    DATA mt_map   TYPE tt_map.

    " The customer a key cell names - see the method for what it resolves.
    " TYPE STRING, not CLIKE: LCL_UTIL=>ALPHA takes a string by reference
    " and a generically typed actual cannot reach it. Both callers read the
    " cell with LCL_UTIL=>CELL( ), which returns a string.
    METHODS key_kunnr
      IMPORTING iv_cell   TYPE string
                iv_row    TYPE i
      RETURNING VALUE(rv) TYPE kunnr.

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

    " Moves one component of the customer's address into a differently
    " named component of the business partner, when it was filled.
    METHODS bp_copy
      IMPORTING iv_from  TYPE clike
                iv_to    TYPE clike
                iv_row   TYPE i
                is_post  TYPE any
      CHANGING  cs_data  TYPE any
                cs_datax TYPE any.

    METHODS master IMPORTING is_row TYPE ty_row.
    METHODS credit IMPORTING is_row TYPE ty_row.

    " The credit tab also carries three customer-master fields - payment
    " terms, interest indicator and customer group 3. They are written
    " through the same Business Partner API as everything else; the company
    " code and the sales area they belong to are taken from the ones the
    " customer already has, because the tab does not carry them.
    METHODS credit_master
      IMPORTING iv_kunnr TYPE kunnr
                iv_kkber TYPE kkber
                iv_row   TYPE i
                is_comp  TYPE cmds_ei_company
                is_sale  TYPE cmds_ei_sales.

    " The API validates the customer as a whole, so a value already stored
    " against one of its sales areas can reject an update that has nothing
    " to do with it. This says which one, instead of leaving the user with
    " a bare "Entry X does not exist in TVV3".
    METHODS warn_stored
      IMPORTING iv_kunnr TYPE kunnr
                iv_row   TYPE i
                is_sale  TYPE cmds_ei_sales.

    " Technical field names that occur only once in this scenario, and so
    " can identify a column on a file headed with field names.
    METHODS fld_keys RETURNING VALUE(rt) TYPE string_table.
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

  METHOD headings.
    " Both spellings count: the heading the template carries ("Company
    " Code") and the technical field name ("BUKRS"). Files arrive headed
    " either way, and a field name is only usable where it occurs once.
    DATA lt_k TYPE SORTED TABLE OF string WITH UNIQUE KEY table_line.
    LOOP AT mt_map INTO DATA(ls_m).
      IF ls_m-hdr IS NOT INITIAL.
        INSERT CONV string( ls_m-hdr ) INTO TABLE lt_k.
      ENDIF.
    ENDLOOP.
    DATA(lt_fk) = fld_keys( ).
    LOOP AT lt_fk INTO DATA(lv_f).
      INSERT lv_f INTO TABLE lt_k.
    ENDLOOP.
    rt = VALUE #( FOR lv IN lt_k ( lv ) ).
  ENDMETHOD.

  METHOD fld_keys.
    TYPES: BEGIN OF ty_f, key TYPE string, n TYPE i, END OF ty_f.
    DATA lt_f TYPE SORTED TABLE OF ty_f WITH UNIQUE KEY key.
    LOOP AT mt_map INTO DATA(ls_m).
      DATA(lv_k) = lcl_util=>squash( ls_m-fld ).
      IF lv_k IS INITIAL.
        CONTINUE.
      ENDIF.
      READ TABLE lt_f ASSIGNING FIELD-SYMBOL(<ls_f>) WITH KEY key = lv_k.
      IF sy-subrc = 0.
        <ls_f>-n = <ls_f>-n + 1.
      ELSE.
        INSERT VALUE ty_f( key = lv_k n = 1 ) INTO TABLE lt_f.
      ENDIF.
    ENDLOOP.
    LOOP AT lt_f INTO DATA(ls_f) WHERE n = 1.
      APPEND ls_f-key TO rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD bind_columns.
    IF it_head IS INITIAL.
      RETURN.
    ENDIF.

    " ---- what the file's heading line holds ---------------------------
    " Every occurrence of every heading, in file order. A heading that
    " appears more than once is not thrown away: the second "Terms of
    " payment key" in the file belongs to the second one in the template.
    TYPES: BEGIN OF ty_h,   key TYPE string, col TYPE i, n TYPE i, END OF ty_h.
    TYPES: BEGIN OF ty_occ, key TYPE string, seq TYPE i, col TYPE i, END OF ty_occ.
    TYPES: BEGIN OF ty_bc,  col TYPE i,      key TYPE string, END OF ty_bc.
    DATA lt_cnt   TYPE SORTED TABLE OF ty_h   WITH UNIQUE KEY key.
    DATA lt_occ   TYPE SORTED TABLE OF ty_occ WITH UNIQUE KEY key seq.
    DATA lt_bycol TYPE SORTED TABLE OF ty_bc  WITH UNIQUE KEY col.

    LOOP AT it_head INTO DATA(lv_h).
      " The column number has to be taken here, before anything else runs.
      " READ TABLE on a sorted table is a binary search, and SAP sets
      " SY-TABIX to the position the key WOULD be inserted at when it finds
      " nothing - so reading SY-TABIX after it gives the heading's place in
      " the alphabet instead of its place in the file.
      DATA(lv_col) = sy-tabix.
      DATA(lv_k)   = lcl_util=>squash( lv_h ).
      IF lv_k IS INITIAL.
        CONTINUE.
      ENDIF.
      DATA lv_seq TYPE i.
      READ TABLE lt_cnt ASSIGNING FIELD-SYMBOL(<ls_c>) WITH KEY key = lv_k.
      IF sy-subrc = 0.
        <ls_c>-n = <ls_c>-n + 1.
        lv_seq   = <ls_c>-n.
      ELSE.
        INSERT VALUE ty_h( key = lv_k col = lv_col n = 1 ) INTO TABLE lt_cnt.
        lv_seq = 1.
      ENDIF.
      INSERT VALUE ty_occ( key = lv_k seq = lv_seq col = lv_col ) INTO TABLE lt_occ.
      INSERT VALUE ty_bc( col = lv_col key = lv_k ) INTO TABLE lt_bycol.
    ENDLOOP.

    " ---- and how often the template uses each heading ------------------
    DATA lt_mcnt TYPE SORTED TABLE OF ty_h WITH UNIQUE KEY key.
    LOOP AT mt_map INTO DATA(ls_c1) WHERE hdr IS NOT INITIAL.
      DATA(lv_ck) = CONV string( ls_c1-hdr ).
      READ TABLE lt_mcnt ASSIGNING FIELD-SYMBOL(<ls_mc>) WITH KEY key = lv_ck.
      IF sy-subrc = 0.
        <ls_mc>-n = <ls_mc>-n + 1.
      ELSE.
        INSERT VALUE ty_h( key = lv_ck n = 1 ) INTO TABLE lt_mcnt.
      ENDIF.
    ENDLOOP.

    DATA lt_done TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY table_line.
    DATA lt_used TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY table_line.
    DATA lt_seen TYPE SORTED TABLE OF ty_h WITH UNIQUE KEY key.
    DATA lv_moved TYPE i.

    " ---- first pass: the heading the template carries above the column --
    " A repeated heading is matched by its occurrence, and only when the
    " file repeats it exactly as often as the template does - otherwise
    " there is no way to tell which is which and the column stays put.
    LOOP AT mt_map ASSIGNING FIELD-SYMBOL(<ls_m>) WHERE hdr IS NOT INITIAL.
      DATA(lv_ix) = sy-tabix.
      DATA(lv_key) = CONV string( <ls_m>-hdr ).

      DATA lv_mseq TYPE i.
      READ TABLE lt_seen ASSIGNING FIELD-SYMBOL(<ls_s>) WITH KEY key = lv_key.
      IF sy-subrc = 0.
        <ls_s>-n = <ls_s>-n + 1.
        lv_mseq  = <ls_s>-n.
      ELSE.
        INSERT VALUE ty_h( key = lv_key n = 1 ) INTO TABLE lt_seen.
        lv_mseq = 1.
      ENDIF.

      READ TABLE lt_cnt  INTO DATA(ls_fc) WITH KEY key = lv_key.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      READ TABLE lt_mcnt INTO DATA(ls_mc) WITH KEY key = lv_key.
      " The file must repeat the heading at least as often as the template
      " does; the nth in the template is then the nth in the file. Fewer in
      " the file than in the template means there is no telling which is
      " which, so those columns stay where they are.
      IF sy-subrc <> 0 OR ls_fc-n < ls_mc-n.
        CONTINUE.
      ENDIF.
      READ TABLE lt_occ INTO DATA(ls_o) WITH KEY key = lv_key seq = lv_mseq.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF ls_o-col <> <ls_m>-col.
        lv_moved = lv_moved + 1.
      ENDIF.
      <ls_m>-col = ls_o-col.
      INSERT lv_ix   INTO TABLE lt_done.
      INSERT ls_o-col INTO TABLE lt_used.
    ENDLOOP.

    " ---- second pass: the technical field name -------------------------
    " For files headed with field names rather than the template wording.
    " Only names that occur once in this scenario, once in the file, and
    " only columns no heading has already claimed.
    DATA(lt_fk) = fld_keys( ).
    LOOP AT mt_map ASSIGNING <ls_m>.
      DATA(lv_ix2) = sy-tabix.
      IF line_exists( lt_done[ table_line = lv_ix2 ] ).
        CONTINUE.
      ENDIF.
      DATA(lv_fk) = lcl_util=>squash( <ls_m>-fld ).
      IF lv_fk IS INITIAL OR NOT line_exists( lt_fk[ table_line = lv_fk ] ).
        CONTINUE.
      ENDIF.
      READ TABLE lt_cnt INTO ls_fc WITH KEY key = lv_fk.
      IF sy-subrc <> 0 OR ls_fc-n <> 1
         OR line_exists( lt_used[ table_line = ls_fc-col ] ).
        CONTINUE.
      ENDIF.
      IF ls_fc-col <> <ls_m>-col.
        lv_moved = lv_moved + 1.
      ENDIF.
      <ls_m>-col = ls_fc-col.
      INSERT lv_ix2   INTO TABLE lt_done.
      INSERT ls_fc-col INTO TABLE lt_used.
    ENDLOOP.

    IF lt_done IS INITIAL.
      " No heading in this file was recognised at all, so there is nothing
      " to say and nothing to protect against - the file is read exactly as
      " the template is laid out.
      RETURN.
    ENDIF.

    " ---- what is left is read by position ------------------------------
    " And a position another field has already been found at cannot be
    " read: on a file with a column inserted or removed it holds the
    " neighbour's value, and a wrong value is worse than none.
    DATA lv_miss  TYPE string.
    DATA lv_nmiss TYPE i.
    DATA lv_nblank TYPE i.
    LOOP AT mt_map ASSIGNING <ls_m>.
      DATA(lv_ix3) = sy-tabix.
      IF line_exists( lt_done[ table_line = lv_ix3 ] ).
        CONTINUE.
      ENDIF.
      lv_nmiss = lv_nmiss + 1.
      IF lv_nmiss <= 12.
        lv_miss = COND string( WHEN lv_miss IS INITIAL
                               THEN |{ <ls_m>-fld }({ <ls_m>-col })|
                               ELSE |{ lv_miss }, { <ls_m>-fld }({ <ls_m>-col })| ).
      ENDIF.
      " Two reasons not to read the position after all. Either another
      " field has already been found there, or the heading sitting there is
      " one this scenario knows and it belongs to a different field. Both
      " mean the column has moved and the position now holds someone else's
      " value, which is worse than none.
      DATA(lv_blank) = xsdbool( line_exists( lt_used[ table_line = <ls_m>-col ] ) ).
      IF lv_blank = abap_false.
        READ TABLE lt_bycol INTO DATA(ls_bc) WITH KEY col = <ls_m>-col.
        IF sy-subrc = 0
           AND ls_bc-key <> CONV string( <ls_m>-hdr )
           AND ls_bc-key <> lcl_util=>squash( <ls_m>-fld )
           AND ( line_exists( lt_mcnt[ key = ls_bc-key ] )
              OR line_exists( lt_fk[ table_line = ls_bc-key ] ) ).
          lv_blank = abap_true.
        ENDIF.
      ENDIF.
      IF lv_blank = abap_true.
        <ls_m>-col = 0.
        lv_nblank = lv_nblank + 1.
      ENDIF.
    ENDLOOP.

    " One line each, not one per column.
    IF lv_moved > 0.
      mo_log->add( iv_row = 0 iv_type = 'I'
                   iv_text = |{ lv_moved } column(s) sit elsewhere in this file than in | &&
                             |the template - each was read from where its heading is| ).
    ENDIF.
    IF lv_nmiss > 0.
      mo_log->add( iv_row = 0 iv_type = 'I'
                   iv_text = |{ lv_nmiss } column(s) carry no heading this program recognises | &&
                             |and were read by position: { lv_miss }| ).
    ENDIF.
    IF lv_nblank > 0.
      mo_log->add( iv_row = 0 iv_type = 'W'
                   iv_text = |{ lv_nblank } of those sit where another field was found, so they | &&
                             |were left empty rather than loaded with a neighbour's value - | &&
                             |give those columns their template heading| ).
    ENDIF.
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

  METHOD key_kunnr.
    " The customer a key cell names, whether it holds the customer number
    " or the business partner number. An unknown number is handed back as
    " it stands, so the caller's own "does not exist" check still speaks.
    rv = lcl_util=>alpha( iv_in = iv_cell iv_len = 10 ).
    IF rv IS INITIAL.
      RETURN.
    ENDIF.

    DATA: lv_kunnr TYPE kunnr,
          lv_bp    TYPE bu_partner.
    lcl_cfg=>get( )->cust_of( EXPORTING iv_in      = rv
                              IMPORTING ev_kunnr   = lv_kunnr
                                        ev_from_bp = lv_bp ).
    IF lv_bp IS INITIAL OR lv_kunnr IS INITIAL.
      RETURN.                            " a customer number, or neither
    ENDIF.

    mo_log->add( iv_row = iv_row iv_kunnr = lv_kunnr iv_type = 'S'
                 iv_text = |{ rv ALPHA = OUT } is business partner | &&
                           |{ lv_bp ALPHA = OUT } - customer | &&
                           |{ lv_kunnr ALPHA = OUT } is used| ).
    rv = lv_kunnr.
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
      " Every branch below writes into a field the caller named at runtime,
      " so the target can be a number or a date. A cell that is not one
      " belongs in the log, not in a short dump.
      TRY.
        CASE iv_cnv.
          WHEN 'AL' OR 'GL'.
            " GL was never handled here, so the seven AKONT columns fell
            " through to WHEN OTHERS and reached the API with no conversion
            " at all. Both codes now take the same path.
            "
            " The padding length is read from the target field itself, so it
            " is always the real DDIC length - 10 for KUNNR, LIFNR, AKONT and
            " FDGRV, 6 for VBUND - and cannot drift out of step with a table.
            DATA(lv_len) = lcl_util=>char_len( <lv_t> ).
            IF lv_len > 0.
              <lv_t> = lcl_util=>alpha( iv_in = lv_in iv_len = lv_len ).
            ELSE.
              " not a character field, so there is nothing to pad
              <lv_t> = lv_in.
            ENDIF.
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
            " A word in a one character field is a flag written out in full.
            DATA(lv_w) = lcl_util=>char_len( <lv_t> ).
            IF lv_w = 1 AND strlen( lv_in ) > 1.
              <lv_t> = lcl_util=>flag( lv_in ).
            ELSE.
              <lv_t> = lv_in.
            ENDIF.
        ENDCASE.
        CATCH cx_sy_conversion_error.
          mo_log->add( iv_row = iv_row iv_type = 'E'
                       iv_struc = iv_struc iv_fld = iv_fld
                       iv_text = |"{ lv_in }" does not fit { iv_fld }| ).
          RETURN.
      ENDTRY.
    ENDIF.

    " DATAX carries the same component names as DATA.
    ASSIGN COMPONENT iv_fld OF STRUCTURE cs_datax TO FIELD-SYMBOL(<lv_x>).
    IF sy-subrc = 0.
      <lv_x> = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD bp_copy.
    FIELD-SYMBOLS <lv_v> TYPE any.
    ASSIGN COMPONENT iv_from OF STRUCTURE is_post TO <lv_v>.
    IF sy-subrc <> 0 OR <lv_v> IS INITIAL.
      RETURN.
    ENDIF.
    set_comp( EXPORTING iv_fld = iv_to iv_val = <lv_v>
                        iv_cnv = '' iv_row = iv_row iv_struc = 'BP'
              CHANGING  cs_data  = cs_data
                        cs_datax = cs_datax ).
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
        WHEN 'KUNNR'. lv_kunnr = key_kunnr( iv_cell = lv_v iv_row = is_row-row ).
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
    " "Modify" is not a task the customer API accepts - every node has to
    " say whether it is an insert or an update, so each one is asked for.
    ls_comp-task = COND #( WHEN lv_exists = abap_true
                            AND lo_cfg->has_knb1( iv_kunnr = lv_kunnr
                                                  iv_bukrs = lv_bukrs ) = abap_true
                           THEN gc_u ELSE gc_i ).
    ls_comp-data_key-bukrs = lv_bukrs.
    ls_sale-task = COND #( WHEN lv_exists = abap_true
                            AND lo_cfg->has_knvv( iv_kunnr = lv_kunnr
                                                  iv_vkorg = lv_vkorg
                                                  iv_vtweg = lv_vtweg
                                                  iv_spart = lv_spart ) = abap_true
                           THEN gc_u ELSE gc_i ).
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
          IF ls_m-fld = 'KVGR3' AND lo_cfg->ok_kvgr3( lv_cell ) = abap_false.
            mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'E'
                         iv_struc = 'KNVV' iv_fld = 'KVGR3'
                         iv_text = |Customer group 3 "{ lv_cell }" (column { ls_m-col }) is not in TVV3 | &&
                                   |- maintain it with SM30 view V_TVV3 or correct the file| ).
            CONTINUE.
          ENDIF.
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
          DATA lv_ttask TYPE cmd_ei_object_task.
          lv_ttask = gc_i.
          IF lv_exists = abap_true
             AND lo_cfg->has_knvi( iv_kunnr = lv_kunnr
                                   iv_aland = lv_aland
                                   iv_tatyp = lv_tatyp ) = abap_true.
            lv_ttask = gc_u.
          ENDIF.
          APPEND VALUE cmds_ei_tax_ind(
            task              = lv_ttask
            data_key-aland    = lv_aland
            data_key-tatyp    = lv_tatyp
            data-taxkd        = lv_cell
            datax-taxkd       = abap_true ) TO ls_cust-central_data-tax_ind-tax_ind.

        WHEN gc_n_lic.
          mo_lic->set( iv_fld = ls_m-fld iv_val = lv_cell
                       iv_cnv = ls_m-cnv iv_row = is_row-row ).

        WHEN gc_n_iden.
          lv_adh = lv_cell.

      ENDCASE.
    ENDLOOP.

    " ---- 3. communication ----------------------------------------------
    " The line type of CVIS_EI_PHONE_T is CVIS_EI_PHONE_STR, which wraps
    " CVIS_EI_PHONE in a component called CONTACT (plus a REMARK). The same
    " holds for fax and e-mail, so every field goes through CONTACT-.
    IF lv_tel IS NOT INITIAL.
      APPEND VALUE cvis_ei_phone_str(
               contact-task            = gc_i
               contact-data-telephone  = lv_tel
               contact-datax-telephone = abap_true
             ) TO ls_cust-central_data-address-communication-phone-phone.
    ENDIF.
    IF lv_mob IS NOT INITIAL.
      " A mobile number is a telephone entry flagged as mobile. The flag is
      " BAPIADTEL-R_3_USER, whose data element is AD_FLGMOB.
      APPEND VALUE cvis_ei_phone_str(
               contact-task            = gc_i
               contact-data-telephone  = lv_mob
               contact-data-r_3_user   = abap_true
               contact-datax-telephone = abap_true
               contact-datax-r_3_user  = abap_true
             ) TO ls_cust-central_data-address-communication-phone-phone.
    ENDIF.
    IF lv_fax IS NOT INITIAL.
      APPEND VALUE cvis_ei_fax_str(
               contact-task      = gc_i
               contact-data-fax  = lv_fax
               contact-datax-fax = abap_true
             ) TO ls_cust-central_data-address-communication-fax-fax.
    ENDIF.
    IF lv_smt IS NOT INITIAL.
      APPEND VALUE cvis_ei_smtp_str(
               contact-task         = gc_i
               contact-data-e_mail  = lv_smt
               contact-datax-e_mail = abap_true
             ) TO ls_cust-central_data-address-communication-smtp-smtp.
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

    " The partner has to be identified in the message either way - the API
    " answers "Specify at least one number for the business partner"
    " otherwise (message R11 123). A change names the partner it changes by
    " its GUID. A creation has no number yet, because the number comes from
    " the grouping's range, so it is identified by a GUID generated here:
    " that GUID becomes the new partner's PARTNER_GUID.
    DATA lv_guid TYPE bu_partner_guid.
    IF lv_task = gc_i.
      TRY.
          lv_guid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
        CATCH cx_uuid_error INTO DATA(lx_uuid).
          DATA(lv_ut) = lx_uuid->get_text( ).
          mo_log->add( iv_row = is_row-row iv_type = 'E' iv_text = lv_ut ).
          RETURN.
      ENDTRY.
    ELSEIF lv_kunnr IS NOT INITIAL.
      lv_guid = lo_cfg->cust_guid( lv_kunnr ).
    ENDIF.
    IF lv_guid IS NOT INITIAL.
      ls_bp-header-object_instance-bpartnerguid = lv_guid.
    ENDIF.
    ls_bp-central_data-common-data-bp_control-category = gc_org.

    " A creation has to state the grouping: it is what gives the new partner
    " its number range, and without it the API answers "Specify at least one
    " number for the business partner". It is not derived from the account
    " group on its own - the mapping is CVI customising, SM30 view
    " CVIV_CUST_TO_BP1. The selection screen overrides it.
    IF lv_task = gc_i.
      DATA lv_grp TYPE bu_group.
      lv_grp = p_bpgrp.
      IF lv_grp IS INITIAL.
        lv_grp = lo_cfg->bp_group( lv_ktokd ).
      ENDIF.
      IF lv_grp IS INITIAL.
        mo_log->add( iv_row = is_row-row iv_type = 'E' iv_fld = 'KTOKD'
                     iv_text = |Account group { lv_ktokd } has no business partner grouping in | &&
                               |CVIC_CUST_TO_BP1 - maintain it with SM30 view CVIV_CUST_TO_BP1, | &&
                               |or give a grouping on the selection screen| ).
        RETURN.
      ENDIF.
      ls_bp-central_data-common-data-bp_control-grouping = lv_grp.
    ELSEIF p_bpgrp IS NOT INITIAL.
      ls_bp-central_data-common-data-bp_control-grouping = p_bpgrp.
    ENDIF.

    " The roles the account group creates are CVI customising too, SM30 view
    " CVIV_CUST_TO_BP2. Where that is not maintained, the two standard
    " customer roles are used: FI, plus SD when the row carries a sales area.
    DATA(lv_bp) = COND bu_partner( WHEN lv_task <> gc_i AND lv_kunnr IS NOT INITIAL
                                   THEN lo_cfg->cust_bp( lv_kunnr ) ).
    DATA(lt_roles) = lo_cfg->bp_roles( lv_ktokd ).
    IF lt_roles IS INITIAL.
      APPEND gc_role_fi TO lt_roles.
      IF lv_vkorg IS NOT INITIAL.
        APPEND gc_role_sd TO lt_roles.
      ENDIF.
    ENDIF.

    " A role the partner already has is an update, a new one an insert.
    DATA lv_rtask TYPE cmd_ei_object_task.
    LOOP AT lt_roles INTO DATA(lv_role).
      lv_rtask = COND #( WHEN lo_cfg->has_role( iv_partner = lv_bp
                                                iv_role    = lv_role ) = abap_true
                         THEN gc_u ELSE gc_i ).
      APPEND VALUE bus_ei_bupa_roles(
        task     = lv_rtask
        data_key = lv_role ) TO ls_bp-central_data-role-roles.
    ENDLOOP.

    " A new business partner needs a name and an address of its own. The
    " customer's are what they are, so they are copied across rather than
    " mapped a second time. On a change the partner already has both, and
    " CVI keeps them in step with the customer.
    IF lv_task = gc_i.
      " NAME -> NAME1 and so on: the customer address structure and the BP
      " organisation structure name the same things differently.
      bp_copy( EXPORTING iv_from = 'NAME'   iv_to = 'NAME1' iv_row = is_row-row
                         is_post = ls_cust-central_data-address-postal-data
               CHANGING  cs_data = ls_bp-central_data-common-data-bp_organization
                         cs_datax = ls_bp-central_data-common-datax-bp_organization ).
      bp_copy( EXPORTING iv_from = 'NAME_2' iv_to = 'NAME2' iv_row = is_row-row
                         is_post = ls_cust-central_data-address-postal-data
               CHANGING  cs_data = ls_bp-central_data-common-data-bp_organization
                         cs_datax = ls_bp-central_data-common-datax-bp_organization ).
      bp_copy( EXPORTING iv_from = 'NAME_3' iv_to = 'NAME3' iv_row = is_row-row
                         is_post = ls_cust-central_data-address-postal-data
               CHANGING  cs_data = ls_bp-central_data-common-data-bp_organization
                         cs_datax = ls_bp-central_data-common-datax-bp_organization ).
      bp_copy( EXPORTING iv_from = 'NAME_4' iv_to = 'NAME4' iv_row = is_row-row
                         is_post = ls_cust-central_data-address-postal-data
               CHANGING  cs_data = ls_bp-central_data-common-data-bp_organization
                         cs_datax = ls_bp-central_data-common-datax-bp_organization ).
      bp_copy( EXPORTING iv_from = 'SORT1' iv_to = 'SEARCHTERM1' iv_row = is_row-row
                         is_post = ls_cust-central_data-address-postal-data
               CHANGING  cs_data = ls_bp-central_data-common-data-bp_centraldata
                         cs_datax = ls_bp-central_data-common-datax-bp_centraldata ).
      bp_copy( EXPORTING iv_from = 'SORT2' iv_to = 'SEARCHTERM2' iv_row = is_row-row
                         is_post = ls_cust-central_data-address-postal-data
               CHANGING  cs_data = ls_bp-central_data-common-data-bp_centraldata
                         cs_datax = ls_bp-central_data-common-datax-bp_centraldata ).

      DATA ls_adr TYPE bus_ei_bupa_address.
      CLEAR ls_adr.
      ls_adr-task = gc_i.
      lcl_util=>copy_like(
        EXPORTING is_from  = ls_cust-central_data-address-postal-data
                  is_fromx = ls_cust-central_data-address-postal-datax
        CHANGING  cs_to    = ls_adr-data-postal-data
                  cs_tox   = ls_adr-data-postal-datax ).
      IF ls_adr-data-postal-datax IS NOT INITIAL.
        APPEND ls_adr TO ls_bp-central_data-address-addresses.
      ENDIF.
    ENDIF.

    IF lv_adh IS NOT INITIAL.
      DATA lv_itask TYPE cmd_ei_object_task.
      lv_itask = COND #( WHEN lo_cfg->has_ident( iv_partner = lv_bp
                                                 iv_cat     = CONV bu_id_type( gc_id_aadhaar ) ) = abap_true
                         THEN gc_u ELSE gc_i ).
      APPEND VALUE bus_ei_bupa_identification(
        task                            = lv_itask
        data_key-identificationcategory = gc_id_aadhaar
        data_key-identificationnumber   = lv_adh
      ) TO ls_bp-central_data-ident_number-ident_numbers.
    ENDIF.

    " ---- 6. post --------------------------------------------------------
    DATA ls_cvis TYPE cvis_ei_extern.
    CLEAR ls_cvis.
    ls_cvis-partner  = ls_bp.
    ls_cvis-customer = ls_cust.

    IF lv_exists = abap_true.
      warn_stored( iv_kunnr = lv_kunnr iv_row = is_row-row is_sale = ls_sale ).
    ENDIF.

    DATA(lv_ok) = mo_cvis->post( is_data  = ls_cvis
                                 iv_row   = is_row-row
                                 iv_kunnr = lv_kunnr ).

    " ---- 7. the licence record, only once the BP is safely in ----------
    " A customer created with internal numbering has its number only after
    " the save, and the GUID is what leads to it.
    IF lv_ok = abap_true AND lv_task = gc_i AND lv_kunnr IS INITIAL
       AND p_test = abap_false.
      lv_kunnr = lo_cfg->cust_by_guid( lv_guid ).
      IF lv_kunnr IS INITIAL.
        mo_log->add( iv_row = is_row-row iv_type = 'W'
                     iv_text = 'Posted, but the new customer number could not be read back from CVI_CUST_LINK' ).
      ELSE.
        mo_log->set_key( iv_row = is_row-row iv_kunnr = lv_kunnr ).
        DATA(lv_bp2)  = lo_cfg->bp_by_guid( lv_guid ).
        DATA(lv_made) = |Customer { lv_kunnr ALPHA = OUT } created|.
        IF lv_bp2 IS INITIAL.
          lv_made = lv_made && ' - but BUT000 holds no partner for its GUID; check the CVI link'.
        ELSE.
          lv_made = lv_made && | as business partner { lv_bp2 ALPHA = OUT }|.
        ENDIF.
        mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr
                     iv_type = COND #( WHEN lv_bp2 IS INITIAL THEN 'W' ELSE 'S' )
                     iv_text = lv_made ).
      ENDIF.
    ENDIF.

    IF lv_ok = abap_true AND mo_lic->touched( ) = abap_true.
      IF lv_kunnr IS INITIAL.
        mo_log->add( iv_row = is_row-row iv_type = 'W'
                     iv_struc = 'ZSD_LICENSE_CHK'
                     iv_text = 'Licence data skipped - the customer number is assigned internally and is not known in a test run; the productive run writes it' ).
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
    DATA: ls_comp TYPE cmds_ei_company,
          ls_sale TYPE cmds_ei_sales.

    LOOP AT mt_map INTO DATA(ls_m).
      DATA(lv_cell) = lcl_util=>cell( is_row = is_row iv_col = ls_m-col ).
      IF lv_cell IS INITIAL.
        CONTINUE.
      ENDIF.
      IF ls_m-node = gc_n_key AND ls_m-fld = 'KUNNR'.
        lv_kunnr = key_kunnr( iv_cell = lv_cell iv_row = is_row-row ).
        CONTINUE.
      ENDIF.

      CASE ls_m-node.
        WHEN gc_n_cred.
          CASE ls_m-fld.
            WHEN 'SEGMENT'.    lv_kkber = lv_cell.
            WHEN 'LIMIT_MAIN'. lv_main  = lcl_util=>to_dec( lv_cell ). lv_has_main = abap_true.
            WHEN 'LIMIT_SGM'.
              " The credit tab carries the classic KLIME and KLIMK side by
              " side and FSCM holds one limit per segment, so the two write
              " the same field. The first filled column is the one used;
              " the second only speaks up when it disagrees.
              DATA(lv_sgm2) = lcl_util=>to_dec( lv_cell ).
              IF lv_has_sgm = abap_true.
                IF lv_sgm2 <> lv_sgm.
                  mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'W'
                               iv_fld = 'LIMIT_SGM'
                               iv_text = |Two segment limits in this row, { lv_sgm } and { lv_sgm2 } - the first is used| ).
                ENDIF.
              ELSE.
                lv_sgm      = lv_sgm2.
                lv_has_sgm  = abap_true.
              ENDIF.
            WHEN 'CURRENCY'.   lv_curr  = lv_cell.
            WHEN 'RISK_CLASS'. lv_risk  = lv_cell.
            WHEN 'XBLOCKED'.   lv_block = lv_cell.
          ENDCASE.

        WHEN gc_n_comp.
          IF ls_m-fld = 'ZTERM' AND lo_cfg->ok_zterm( lv_cell ) = abap_false.
            mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'E'
                         iv_struc = 'KNB1' iv_fld = 'ZTERM'
                         iv_text = |Payment terms "{ lv_cell }" (column { ls_m-col }) are not in T052| ).
            CONTINUE.
          ENDIF.
          IF ls_m-fld = 'VZSKZ'.
            " The template's interest column holds the indicator and, on
            " some files, the calculation cycle behind it ("Z1 3", "Z1/3").
            " The indicator is the first token; a number after it is the
            " cycle in months, KNB1-ZINRT.
            DATA(lv_int) = condense( lv_cell ).
            " Backquoted literals: a quoted ' ' loses its blank, which would
            " glue "Z1/3" into "Z13" and make the separator empty.
            REPLACE ALL OCCURRENCES OF '/' IN lv_int WITH ` `.
            REPLACE ALL OCCURRENCES OF '-' IN lv_int WITH ` `.
            CONDENSE lv_int.
            SPLIT lv_int AT ` ` INTO DATA(lv_ind) DATA(lv_cyc).
            IF lo_cfg->ok_vzskz( lv_ind ) = abap_false.
              mo_log->add( iv_row = is_row-row iv_kunnr = lv_kunnr iv_type = 'E'
                           iv_struc = 'KNB1' iv_fld = 'VZSKZ'
                           iv_text = |Interest indicator "{ lv_ind }" (column { ls_m-col }) is not in T056| ).
              CONTINUE.
            ENDIF.
            set_comp( EXPORTING iv_fld = 'VZSKZ' iv_val = lv_ind
                                iv_cnv = ls_m-cnv iv_row = is_row-row
                                iv_struc = 'KNB1'
                      CHANGING  cs_data  = ls_comp-data
                                cs_datax = ls_comp-datax ).
            lv_cyc = condense( lv_cyc ).
            IF lv_cyc IS NOT INITIAL AND lv_cyc CO '0123456789'.
              set_comp( EXPORTING iv_fld = 'ZINRT' iv_val = lv_cyc
                                  iv_cnv = 'NM' iv_row = is_row-row
                                  iv_struc = 'KNB1'
                        CHANGING  cs_data  = ls_comp-data
                                  cs_datax = ls_comp-datax ).
            ENDIF.
          ELSE.
            set_comp( EXPORTING iv_fld = ls_m-fld iv_val = lv_cell
                                iv_cnv = ls_m-cnv iv_row = is_row-row
                                iv_struc = 'KNB1'
                      CHANGING  cs_data  = ls_comp-data
                                cs_datax = ls_comp-datax ).
          ENDIF.

        WHEN gc_n_sale.
          set_comp( EXPORTING iv_fld = ls_m-fld iv_val = lv_cell
                              iv_cnv = ls_m-cnv iv_row = is_row-row
                              iv_struc = 'KNVV'
                    CHANGING  cs_data  = ls_sale-data
                              cs_datax = ls_sale-datax ).
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

    " Payment terms, interest indicator and customer group 3 are customer
    " master fields, not credit data - they go through the BP API.
    IF ls_comp-datax IS NOT INITIAL OR ls_sale-datax IS NOT INITIAL.
      credit_master( iv_kunnr = lv_kunnr
                     iv_kkber = lv_kkber
                     iv_row   = is_row-row
                     is_comp  = ls_comp
                     is_sale  = ls_sale ).
    ENDIF.
  ENDMETHOD.

  METHOD warn_stored.
    DATA(lt_bad) = lcl_cfg=>get( )->bad_kvgr3( iv_kunnr ).
    LOOP AT lt_bad INTO DATA(ls_bad).
      " Corrected by this run if the file writes that same sales area.
      IF is_sale-datax-kvgr3     = abap_true
     AND is_sale-data_key-vkorg = ls_bad-vkorg
     AND is_sale-data_key-vtweg = ls_bad-vtweg
     AND is_sale-data_key-spart = ls_bad-spart.
        CONTINUE.
      ENDIF.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'W'
                   iv_struc = 'KNVV' iv_fld = 'KVGR3'
                   iv_text = |Sales area { ls_bad-vkorg }/{ ls_bad-vtweg }/{ ls_bad-spart } of this | &&
                             |customer already holds Customer group 3 "{ ls_bad-kvgr3 }", which is not | &&
                             |in TVV3. The API checks the whole customer, so it rejects the update until | &&
                             |that value is maintained in SM30 view V_TVV3 or replaced from the file| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD credit_master.
    DATA(lo_cfg) = lcl_cfg=>get( ).
    DATA ls_comp TYPE cmds_ei_company.
    DATA ls_sale TYPE cmds_ei_sales.
    ls_comp = is_comp.
    ls_sale = is_sale.

    DATA ls_cust TYPE cmds_ei_extern.
    CLEAR ls_cust.
    ls_cust-header-object_instance-kunnr = iv_kunnr.
    " The customer exists - this is an update. "Modify" is not a task the
    " customer API accepts.
    ls_cust-header-object_task           = gc_u.

    " ---- company code for the KNB1 fields ------------------------------
    IF ls_comp-datax IS NOT INITIAL.
      DATA(lt_b) = lo_cfg->cust_bukrs( iv_kunnr ).
      IF lines( lt_b ) > 1 AND iv_kkber IS NOT INITIAL.
        " More than one company code: the credit control area decides.
        DATA(lt_k) = lo_cfg->kkber_bukrs( iv_kkber ).
        DATA lt_n TYPE lcl_cfg=>tt_bukrs.
        LOOP AT lt_b INTO DATA(lv_b).
          IF line_exists( lt_k[ table_line = lv_b ] ).
            APPEND lv_b TO lt_n.
          ENDIF.
        ENDLOOP.
        IF lines( lt_n ) = 1.
          lt_b = lt_n.
        ENDIF.
      ENDIF.

      IF lines( lt_b ) = 0.
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                     iv_struc = 'KNB1'
                     iv_text = |Payment terms and interest indicator not written - customer { iv_kunnr } | &&
                               |is not extended to any company code| ).
      ELSEIF lines( lt_b ) > 1.
        DATA lv_list TYPE string.
        CLEAR lv_list.
        LOOP AT lt_b INTO DATA(lv_b2).
          lv_list = COND string( WHEN lv_list IS INITIAL THEN lv_b2 ELSE |{ lv_list }, { lv_b2 }| ).
        ENDLOOP.
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                     iv_struc = 'KNB1'
                     iv_text = |Payment terms and interest indicator not written - the customer is in | &&
                               |company codes { lv_list } and this tab has no company code column| ).
      ELSE.
        " The company code was read from KNB1, so the row is there already.
        ls_comp-task = gc_u.
        ls_comp-data_key-bukrs = lt_b[ 1 ].
        APPEND ls_comp TO ls_cust-company_data-company.
      ENDIF.
    ENDIF.

    " ---- sales area for the KNVV fields --------------------------------
    IF ls_sale-datax IS NOT INITIAL.
      DATA(lt_s) = lo_cfg->cust_sales( iv_kunnr ).
      IF lines( lt_s ) = 0.
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                     iv_struc = 'KNVV'
                     iv_text = |Customer group 3 not written - customer { iv_kunnr } has no sales area| ).
      ELSEIF lines( lt_s ) > 1.
        mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                     iv_struc = 'KNVV'
                     iv_text = |Customer group 3 not written - the customer has { lines( lt_s ) } sales | &&
                               |areas and this tab has no sales area columns| ).
      ELSE.
        " Likewise the sales area comes from KNVV.
        ls_sale-task = gc_u.
        ls_sale-data_key-vkorg = lt_s[ 1 ]-vkorg.
        ls_sale-data_key-vtweg = lt_s[ 1 ]-vtweg.
        ls_sale-data_key-spart = lt_s[ 1 ]-spart.
        APPEND ls_sale TO ls_cust-sales_data-sales.
      ENDIF.
    ENDIF.

    IF ls_cust-company_data-company IS INITIAL AND ls_cust-sales_data-sales IS INITIAL.
      RETURN.
    ENDIF.

    " Anything already stored against this customer that no longer passes
    " its check table will reject the update, whatever this file sends.
    warn_stored( iv_kunnr = iv_kunnr iv_row = iv_row is_sale = ls_sale ).

    " The partner node has to name the business partner being changed. With
    " an empty partner header the API reads the request as a creation and
    " answers "Specify at least one number for the business partner".
    DATA(lv_guid) = lo_cfg->cust_guid( iv_kunnr ).
    IF lv_guid IS INITIAL.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'E'
                   iv_text = |Customer { iv_kunnr } has no business partner link (CVI_CUST_LINK) - | &&
                             |payment terms, interest indicator and customer group were not written| ).
      RETURN.
    ENDIF.

    DATA ls_bp TYPE bus_ei_extern.
    CLEAR ls_bp.
    ls_bp-header-object_task                  = gc_u.
    ls_bp-header-object_instance-bpartnerguid = lv_guid.

    DATA ls_cvis TYPE cvis_ei_extern.
    CLEAR ls_cvis.
    ls_cvis-partner  = ls_bp.
    ls_cvis-customer = ls_cust.
    IF mo_cvis->post( is_data  = ls_cvis
                      iv_row   = iv_row
                      iv_kunnr = iv_kunnr ) = abap_true.
      mo_log->add( iv_row = iv_row iv_kunnr = iv_kunnr iv_type = 'S'
                   iv_text = 'Payment terms / interest indicator / customer group updated' ).
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

  DATA gt_row  TYPE tt_row.
  DATA gt_head TYPE string_table.
  DATA gv_sheet TYPE string.
  TRY.
      NEW lcl_excel( )->read(
        EXPORTING iv_file    = p_file
                  iv_from_pc = p_pc
                  iv_sheet   = go_engine->sheet( )
                  iv_skip    = p_skip
                  it_want    = go_engine->headings( )
        IMPORTING et_head    = gt_head
                  et_row     = gt_row
                  ev_sheet   = gv_sheet ).
    CATCH lcx_upl INTO DATA(gx).
      " MESSAGE takes a data object, not an expression.
      DATA(gv_txt) = gx->get_text( ).
      MESSAGE gv_txt TYPE 'E'.
  ENDTRY.

  IF gt_row IS INITIAL.
    DATA gv_none TYPE string.
    gv_none = |Tab "{ gv_sheet }" holds no data below its heading row|.
    MESSAGE gv_none TYPE 'I'.
    RETURN.
  ENDIF.

  go_engine->bind_columns( gt_head ).
  go_engine->run( gt_row ).

END-OF-SELECTION.

  DATA gv_ok  TYPE i.
  DATA gv_err TYPE i.
  go_log->counts( IMPORTING ev_ok = gv_ok ev_err = gv_err ).
  DATA gv_sum TYPE string.
  gv_sum = |{ lines( gt_row ) } row(s) read, { gv_ok } processed, { gv_err } with errors|.
  MESSAGE gv_sum TYPE 'S'.
  go_log->display( ).
