*&---------------------------------------------------------------------*
*& Report  ZMMS_BP_MASS_UPLOAD
*&---------------------------------------------------------------------*
*& Title       : Business Partner / Supplier Master Mass Upload
*& Module      : MM (supplier master) incl. FI company-code data
*& Package     : ZMMS_BP_UPLOAD          Transaction : ZMMS_BPUPL
*& Source book : "Vendor LSMW with Template.xlsx" - one tab per scenario
*&
*& Purpose
*&   One program for every vendor/BP mass create and change scenario. The
*&   scenario is chosen by radio button and drives which tab of the customer
*&   workbook is read. Column layouts are exactly the customer's existing
*&   templates, so no re-keying is needed.
*&
*& Why the engine changed
*&   The templates are ECC-era LSMW/BDC recordings (ZSD_XK05, ZXD01_PFADD_VND,
*&   ZXK02_CIN, ZLSMW_BANK ...) against XK01/XK02/XK05/XD01. Under the S/4HANA
*&   Business Partner approach (SAP Note 2265093) those transactions are
*&   redirected to BP or removed, so the recordings cannot run. The layouts are
*&   kept; the execution engine is the CVI/BP API.
*&
*& APIs - no INSERT/UPDATE/MODIFY on any table anywhere in this program
*&   CL_MD_BP_MAINTAIN=>VALIDATE_SINGLE   field-level validation (message map)
*&   CL_MD_BP_MAINTAIN=>MAINTAIN          post, native I_TEST_RUN
*&   BAPI_BANK_CREATE / BAPI_BANK_CHANGE  bank master (not a BP object)
*&   J_1ITAN_EXEM_SAVE                    India TAN exemption (update module)
*&   Reads use SELECT / BAPI_BUPA_* only.
*&
*& Gross-segment rule
*&   VALIDATE_SINGLE expects GROSS data per segment: a partial list for bank
*&   details, withholding tax or partner functions DELETES what was left out.
*&   Those segments are therefore read from the database and merged before
*&   they are sent. See the merge_* methods.
*&
*& Naming convention
*&   Z<MODULE>_ pattern from Cipla_Checklist Part 1.1. That checklist is a
*&   request-for-inputs, not a filled-in standard - per its own Part 1 this
*&   draft convention still needs client approval.
*&
*& Clean core positioning (SAP S/4HANA 2502 / ABAP Cloud)
*&   ABAP Cloud is SAP's recommended model but is NOT mandatory for
*&   S/4HANA private cloud and on-premise; classic ABAP extensibility
*&   remains supported. This program is deliberately TIER 2:
*&     - CL_MD_BP_MAINTAIN is flagged "Not released", so a strict tier-1
*&       build cannot use it, and no released ABAP API exists for supplier
*&       master maintenance. The tier-1 alternative is the OData service
*&       API_BUSINESS_PARTNER, which cannot sensibly be driven from a
*&       GUI report doing an Excel mass upload.
*&     - Reads are confined to LCL_CFG and the MERGE_* methods so they can
*&       be swapped for released CDS views (I_Supplier and friends) without
*&       touching any handler.
*&   Everything else follows clean-core practice: modern ABAP syntax, local
*&   classes behind an interface, no obsolete statements, and no INSERT /
*&   UPDATE / MODIFY on any table.
*&   Run ATC with variant ABAP_CLOUD_READINESS and record the tier-2
*&   exemptions above in the review checklist.
*&
*& Structure paths verified against DD03L from system CRS
*&   BUS_EI_EXTERN-CENTRAL_DATA-COMMON / -ADDRESS / -ROLE / -BANKDETAIL
*&   BUS_EI_BUPA_CENTRAL-DATA-BP_CONTROL-CATEGORY / -GROUPING
*&   BUS_EI_STRUC_CENTRAL-TITLE_KEY / -SEARCHTERM1 / -SEARCHTERM2
*&   BUS_EI_BUPA_ADDRESS-DATA-POSTAL-DATA (BUS_EI_STRUC_ADDRESS) / -DATAX
*&   BUS_EI_BUPA_ADDRESS-DATA-COMMUNICATION-PHONE-PHONE / -SMTP-SMTP / -FAX-FAX
*&   BUS_EI_BUPA_TELEPHONE|SMTP|FAX-CONTACT-TASK / -CONTACT-DATA-*
*&     telephone TELEPHONE/EXTENSION/STD_NO/R_3_USER, e-mail E_MAIL, fax FAX
*&   BUS_EI_BUPA_ROLES-TASK / -DATA_KEY (BU_ROLE element, NOT a structure)
*&   CVIS_EI_BANKDETAIL-BANKDETAILS (CVIS_EI_BANKDETAIL_T)
*&     line CVIS_EI_CVI_BANKDETAIL: TASK / DATA_KEY (BANKS,BANKL,BANKN) / DATA / DATAX
*&   VMDS_EI_COMPANY-WTAX_TYPE-WTAX_TYPE, VMDS_EI_PURCHASING-FUNCTIONS-FUNCTIONS
*&   VMDS_EI_FUNCTIONS-DATA-PARTNER (not LIFN2); LTSNR/WERKS live in DATA_KEY
*&---------------------------------------------------------------------*
REPORT zmms_bp_mass_upload.

*----------------------------------------------------------------------*
* Types and constants
*----------------------------------------------------------------------*
" RETURNING parameters must be fully typed, so the packed type used for
" exemption rates and threshold amounts is declared here rather than inline.
TYPES ty_dec TYPE p LENGTH 13 DECIMALS 2.

" Local table of BP roles - avoids depending on a DDIC table type name.
TYPES ty_roles TYPE STANDARD TABLE OF bu_role WITH EMPTY KEY.

TYPES: tt_cell TYPE STANDARD TABLE OF string WITH EMPTY KEY.

TYPES: BEGIN OF ty_row,
         row   TYPE i,
         cells TYPE tt_cell,
       END OF ty_row,
       tt_row TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

" The heading that belongs above each column this program reads. It is what
" lets a file be loaded whose columns have been moved, added or removed - and
" what lets the right tab be found whatever the tab is called.
TYPES: BEGIN OF ty_hdr,
         scen TYPE char2,
         col  TYPE i,
         hdr  TYPE char40,
       END OF ty_hdr,
       tt_hdr TYPE STANDARD TABLE OF ty_hdr WITH EMPTY KEY.

TYPES: BEGIN OF ty_msg,
         icon    TYPE icon_d,
         xlsrow  TYPE i,
         key1    TYPE char20,
         key2    TYPE char10,
         key3    TYPE char10,
         msgty   TYPE bapi_mtype,
         msgid   TYPE symsgid,
         msgno   TYPE symsgno,
         struc   TYPE char30,
         fldnm   TYPE char30,
         message TYPE bapi_msg,
       END OF ty_msg,
       tt_msg TYPE STANDARD TABLE OF ty_msg WITH EMPTY KEY.

CONSTANTS:
  gc_i     TYPE cmd_ei_object_task VALUE 'I',   " insert
  gc_u     TYPE cmd_ei_object_task VALUE 'U',   " update
  gc_m     TYPE cmd_ei_object_task VALUE 'M',   " modify
  gc_clear TYPE string             VALUE '#BLANK#'.

" Tab names in "Vendor LSMW with Template.xlsx"
CONSTANTS:
  gc_sh_create TYPE string VALUE 'Vendor creation for All CC',
  gc_sh_tds    TYPE string VALUE 'TDS upload',
  gc_sh_tan    TYPE string VALUE 'TAN details',
  gc_sh_bkey   TYPE string VALUE 'BANK Key creation',
  gc_sh_bank   TYPE string VALUE 'Bank details update',
  gc_sh_ext    TYPE string VALUE 'Vendor extension',
  gc_sh_cin    TYPE string VALUE 'CIN details',
  gc_sh_pfn    TYPE string VALUE 'Patner function',   " sic - customer spelling
  gc_sh_blk    TYPE string VALUE 'Block_Unblocked'.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS:
  p_r1 RADIOBUTTON GROUP g1 DEFAULT 'X',  " Vendor / BP creation - all CC
  p_r2 RADIOBUTTON GROUP g1,              " Withholding tax / TDS
  p_r3 RADIOBUTTON GROUP g1,              " TAN exemption details
  p_r4 RADIOBUTTON GROUP g1,              " Bank key creation
  p_r5 RADIOBUTTON GROUP g1,              " Vendor bank details
  p_r6 RADIOBUTTON GROUP g1,              " Vendor extension
  p_r7 RADIOBUTTON GROUP g1,              " CIN details
  p_r8 RADIOBUTTON GROUP g1,              " Partner functions
  p_r9 RADIOBUTTON GROUP g1.              " Block / unblock
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_file TYPE rlgrap-filename OBLIGATORY,
            p_pc   RADIOBUTTON GROUP g2 DEFAULT 'X',   " file on the PC
            p_srv  RADIOBUTTON GROUP g2.               " file on the app server
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X',   " simulate - nothing is posted
            p_stop AS CHECKBOX,               " stop at the first faulty row
            p_skip TYPE i DEFAULT 1.          " leading lines treated as heading
SELECTION-SCREEN END OF BLOCK b3.

*----------------------------------------------------------------------*
* Exception
*----------------------------------------------------------------------*
CLASS lcx_upl DEFINITION INHERITING FROM cx_static_check FINAL.
  PUBLIC SECTION.
    DATA mv_text TYPE string.
    METHODS constructor IMPORTING iv_text TYPE string.
    METHODS get_text REDEFINITION.
ENDCLASS.

CLASS lcx_upl IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_text = iv_text.
  ENDMETHOD.
  METHOD get_text.
    result = mv_text.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* Utility
*----------------------------------------------------------------------*
CLASS lcl_util DEFINITION FINAL.
  PUBLIC SECTION.
    "! Write a component into DATA and raise the matching DATAX flag.
    "! Empty value  -> ignored, so untouched template columns stay untouched.
    "! gc_clear     -> clears the field (DATA = space, DATAX = 'X').
    CLASS-METHODS set
      IMPORTING iv_comp  TYPE string
                iv_value TYPE string
                iv_force TYPE abap_bool DEFAULT abap_false
      CHANGING  cs_data  TYPE any
                cs_datax TYPE any.

    CLASS-METHODS cell    IMPORTING is_row TYPE ty_row iv_col TYPE i RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS to_date IMPORTING iv_in  TYPE string RETURNING VALUE(rv) TYPE d.
    CLASS-METHODS to_dec  IMPORTING iv_in  TYPE string RETURNING VALUE(rv) TYPE ty_dec.
    "! Generic ALPHA conversion for fields whose domain carries the exit.
    "! IV_LEN is the length of the field the value is going into. Without it
    "! nothing is padded - see the comment in the implementation.
    CLASS-METHODS alpha   IMPORTING iv_in     TYPE string
                                    iv_len    TYPE i DEFAULT 0
                          RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS lifnr   IMPORTING iv_in  TYPE string RETURNING VALUE(rv) TYPE lifnr.
    CLASS-METHODS gl      IMPORTING iv_in  TYPE string RETURNING VALUE(rv) TYPE saknr.
    "! A one character flag written as a word. Excel turns a tick into TRUE
    "! and some files carry YES or 1, all of which would land in a CHAR 1
    "! field as its first letter - T, Y, 1 - none of which SAP reads as set.
    CLASS-METHODS flag      IMPORTING iv_in  TYPE clike RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS is_empty  IMPORTING is_row TYPE ty_row RETURNING VALUE(rv) TYPE abap_bool.
    "! TRUE for one of the template's own descriptive header lines
    "! (field type / length / mandatory / guideline / LSMW project ...).
    "! Central row filter. Data begins in row 2 on every tab; anything above
    "! that, blank rows, sample rows and leftover header lines are skipped.
    CLASS-METHODS skip_row  IMPORTING is_row TYPE ty_row RETURNING VALUE(rv) TYPE abap_bool.
    "! Heading text reduced to letters and digits in upper case, so that
    "! "Vendor  code", "vendor_code" and "VENDOR CODE" are one and the same.
    CLASS-METHODS squash    IMPORTING iv_in  TYPE clike RETURNING VALUE(rv) TYPE string.
ENDCLASS.

CLASS lcl_util IMPLEMENTATION.

  METHOD set.
    FIELD-SYMBOLS: <lv_d> TYPE any, <lv_x> TYPE any.

    DATA(lv_comp) = to_upper( iv_comp ).
    ASSIGN COMPONENT lv_comp OF STRUCTURE cs_data TO <lv_d>.
    IF sy-subrc <> 0.
      RETURN.                                    " field not present in this release
    ENDIF.

    IF iv_value IS INITIAL AND iv_force = abap_false.
      RETURN.                                    " column not supplied
    ENDIF.

    IF iv_value = gc_clear.
      CLEAR <lv_d>.
    ELSE.
      " A word in a one character field is a flag written out in full.
      DATA lv_w TYPE i.
      DESCRIBE FIELD <lv_d> LENGTH lv_w IN CHARACTER MODE.
      DATA(lv_v) = CONV string( iv_value ).
      IF lv_w = 1 AND strlen( condense( lv_v ) ) > 1.
        lv_v = flag( lv_v ).
      ENDIF.
      TRY.
          <lv_d> = lv_v.
        CATCH cx_sy_conversion_error.
          RETURN.
      ENDTRY.
    ENDIF.

    ASSIGN COMPONENT lv_comp OF STRUCTURE cs_datax TO <lv_x>.
    IF sy-subrc = 0.
      <lv_x> = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD cell.
    CLEAR rv.
    IF iv_col < 1.
      RETURN.
    ENDIF.
    READ TABLE is_row-cells INTO rv INDEX iv_col.
    IF sy-subrc <> 0.
      CLEAR rv.
    ENDIF.
    rv = condense( rv ).
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
    IF rv IS INITIAL.
      RETURN.
    ENDIF.
    IF iv_len > 0 AND rv CO '0123456789' AND strlen( rv ) < iv_len.
      rv = repeat( val = '0' occ = iv_len - strlen( rv ) ) && rv.
    ENDIF.
  ENDMETHOD.

  METHOD lifnr.
    " LFA1-LIFNR, WYT3-LIFN2 and VMDS_EI_FUNCTIONS_DATA-PARTNER (domain KTONR)
    " all carry the ALPHA exit.
    CLEAR rv.
    DATA(lv) = to_upper( condense( iv_in ) ).
    IF lv IS INITIAL.
      RETURN.
    ENDIF.
    rv = alpha( iv_in = lv iv_len = 10 ).
  ENDMETHOD.

  METHOD gl.
    " AKONT / SAKNR - domain SAKNR carries the ALPHA exit, and SAKNR is
    " CHAR 10, so 1120001 has to become 0001120001.
    CLEAR rv.
    DATA(lv) = condense( iv_in ).
    IF lv IS INITIAL.
      RETURN.
    ENDIF.
    rv = alpha( iv_in = lv iv_len = 10 ).
  ENDMETHOD.


  METHOD is_empty.
    rv = abap_true.
    LOOP AT is_row-cells INTO DATA(lv).
      IF lv IS NOT INITIAL.
        rv = abap_false.
        RETURN.
      ENDIF.
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

  METHOD skip_row.
    " Which rows are data is decided by POSITION alone - the heading rows
    " are dropped by the reader according to P_SKIP. Nothing in the row's
    " content causes it to be skipped.
    "
    " Column A of these templates is a label column carrying "Field Tech
    " name", "Sample data" and so on. It used to be read as a marker, which
    " meant a file copied from the sample tab and edited - the way people
    " actually work - loaded nothing at all. It is ignored entirely now.
    rv = is_empty( is_row ).
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Excel reader
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* LCL_HDR - the heading that belongs above each column
*   Taken from the customer workbook, one entry per column this program
*   reads, reduced to letters and digits. Two things are built on it:
*     - the right tab is the one whose heading line carries most of these,
*       so the tab NAME does not decide anything;
*     - each column is then read from wherever its heading actually is, so
*       inserted, deleted or reordered columns load correctly.
*   Columns whose heading is blank or appears twice on the same tab are not
*   listed - those keep their position.
*----------------------------------------------------------------------*
CLASS lcl_hdr DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS for  IMPORTING iv_scen   TYPE char2
                       RETURNING VALUE(rt) TYPE tt_hdr.
  PRIVATE SECTION.
    CLASS-DATA mt TYPE tt_hdr.
    CLASS-METHODS build RETURNING VALUE(rt) TYPE tt_hdr.
ENDCLASS.

CLASS lcl_hdr IMPLEMENTATION.

  METHOD for.
    IF mt IS INITIAL.
      mt = build( ).
    ENDIF.
    rt = VALUE #( FOR ls IN mt WHERE ( scen = iv_scen ) ( ls ) ).
  ENDMETHOD.

  METHOD build.
    " R1 - Vendor creation for All CC (64 identifiable headings)
    APPEND LINES OF VALUE tt_hdr(
      ( scen = 'R1' col = 1    hdr = 'FIELDTECHNAME' )
      ( scen = 'R1' col = 2    hdr = 'LIFNR' )
      ( scen = 'R1' col = 3    hdr = 'BUKRS' )
      ( scen = 'R1' col = 4    hdr = 'EKORG' )
      ( scen = 'R1' col = 5    hdr = 'KTOKK' )
      ( scen = 'R1' col = 6    hdr = 'TITLEMEDI' )
      ( scen = 'R1' col = 7    hdr = 'NAME1' )
      ( scen = 'R1' col = 8    hdr = 'NAME2' )
      ( scen = 'R1' col = 9    hdr = 'NAME3' )
      ( scen = 'R1' col = 10   hdr = 'NAME4' )
      ( scen = 'R1' col = 11   hdr = 'SORT1' )
      ( scen = 'R1' col = 12   hdr = 'SORT2' )
      ( scen = 'R1' col = 13   hdr = 'STRSUPPL1' )
      ( scen = 'R1' col = 14   hdr = 'STRSUPPL2' )
      ( scen = 'R1' col = 15   hdr = 'STREET' )
      ( scen = 'R1' col = 16   hdr = 'STRSUPPL3' )
      ( scen = 'R1' col = 17   hdr = 'CITY2' )
      ( scen = 'R1' col = 18   hdr = 'POSTCODE1' )
      ( scen = 'R1' col = 19   hdr = 'CITY1' )
      ( scen = 'R1' col = 20   hdr = 'COUNTRY' )
      ( scen = 'R1' col = 21   hdr = 'REGION' )
      ( scen = 'R1' col = 22   hdr = 'LANGU' )
      ( scen = 'R1' col = 23   hdr = 'TELNUMBER' )
      ( scen = 'R1' col = 24   hdr = 'TELEXTENS' )
      ( scen = 'R1' col = 25   hdr = 'TELNUMBER2' )
      ( scen = 'R1' col = 26   hdr = 'TELEXTENS2' )
      ( scen = 'R1' col = 27   hdr = 'MOBNUMBER' )
      ( scen = 'R1' col = 28   hdr = 'MOBNUMBER2' )
      ( scen = 'R1' col = 29   hdr = 'FAXNUMBER' )
      ( scen = 'R1' col = 30   hdr = 'SMTPADDR' )
      ( scen = 'R1' col = 31   hdr = 'SMTPADDR2' )
      ( scen = 'R1' col = 32   hdr = 'KUNNR' )
      ( scen = 'R1' col = 33   hdr = 'VBUND' )
      ( scen = 'R1' col = 34   hdr = 'KONZS' )
      ( scen = 'R1' col = 35   hdr = 'STCD3' )
      ( scen = 'R1' col = 36   hdr = 'STCD5' )
      ( scen = 'R1' col = 37   hdr = 'STCEG' )
      ( scen = 'R1' col = 38   hdr = 'J1KFTBUS' )
      ( scen = 'R1' col = 39   hdr = 'STENR' )
      ( scen = 'R1' col = 40   hdr = 'BRSCH' )
      ( scen = 'R1' col = 41   hdr = 'BANKS01' )
      ( scen = 'R1' col = 42   hdr = 'BANKL01' )
      ( scen = 'R1' col = 43   hdr = 'BANKN01' )
      ( scen = 'R1' col = 44   hdr = 'KOINH01' )
      ( scen = 'R1' col = 45   hdr = 'BKONT' )
      ( scen = 'R1' col = 46   hdr = 'IBAN' )
      ( scen = 'R1' col = 47   hdr = 'AKONT' )
      ( scen = 'R1' col = 48   hdr = 'FDGRV' )
      ( scen = 'R1' col = 49   hdr = 'ALTKN' )
      ( scen = 'R1' col = 51   hdr = 'REPRF' )
      ( scen = 'R1' col = 52   hdr = 'ZWELS' )
      ( scen = 'R1' col = 53   hdr = 'ZAHLS' )
      ( scen = 'R1' col = 54   hdr = 'HBKID' )
      ( scen = 'R1' col = 55   hdr = 'VENCLASS' )
      ( scen = 'R1' col = 56   hdr = 'J1ISSIST' )
      ( scen = 'R1' col = 57   hdr = 'J1IPANNO' )
      ( scen = 'R1' col = 58   hdr = 'QLAND' )
      ( scen = 'R1' col = 59   hdr = 'WITHT' )
      ( scen = 'R1' col = 60   hdr = 'WTWITHCD' )
      ( scen = 'R1' col = 61   hdr = 'WAERS' )
      ( scen = 'R1' col = 63   hdr = 'KALSK' )
      ( scen = 'R1' col = 64   hdr = 'WEBRE' )
      ( scen = 'R1' col = 65   hdr = 'INCO1' )
      ( scen = 'R1' col = 66   hdr = 'INCO2' )
    ) TO rt.

    " R2 - TDS upload (64 identifiable headings)
    APPEND LINES OF VALUE tt_hdr(
      ( scen = 'R2' col = 2    hdr = 'LIFNR' )
      ( scen = 'R2' col = 3    hdr = 'BUKRS' )
      ( scen = 'R2' col = 4    hdr = 'D0610' )
      ( scen = 'R2' col = 5    hdr = 'QLAND' )
      ( scen = 'R2' col = 6    hdr = 'WITHT01' )
      ( scen = 'R2' col = 7    hdr = 'WITHT02' )
      ( scen = 'R2' col = 8    hdr = 'WITHT03' )
      ( scen = 'R2' col = 9    hdr = 'WITHT04' )
      ( scen = 'R2' col = 10   hdr = 'WITHT05' )
      ( scen = 'R2' col = 11   hdr = 'WITHT06' )
      ( scen = 'R2' col = 12   hdr = 'WTWITHCD01' )
      ( scen = 'R2' col = 13   hdr = 'WTWITHCD02' )
      ( scen = 'R2' col = 14   hdr = 'WTWITHCD03' )
      ( scen = 'R2' col = 15   hdr = 'WTWITHCD04' )
      ( scen = 'R2' col = 16   hdr = 'WTWITHCD05' )
      ( scen = 'R2' col = 17   hdr = 'WTWITHCD06' )
      ( scen = 'R2' col = 18   hdr = 'WTSUBJCT01' )
      ( scen = 'R2' col = 19   hdr = 'WTSUBJCT02' )
      ( scen = 'R2' col = 20   hdr = 'WTSUBJCT03' )
      ( scen = 'R2' col = 21   hdr = 'WTSUBJCT04' )
      ( scen = 'R2' col = 22   hdr = 'WTSUBJCT05' )
      ( scen = 'R2' col = 23   hdr = 'WTSUBJCT06' )
      ( scen = 'R2' col = 24   hdr = 'QSREC01' )
      ( scen = 'R2' col = 25   hdr = 'QSREC02' )
      ( scen = 'R2' col = 26   hdr = 'QSREC03' )
      ( scen = 'R2' col = 27   hdr = 'QSREC04' )
      ( scen = 'R2' col = 28   hdr = 'QSREC05' )
      ( scen = 'R2' col = 29   hdr = 'QSREC06' )
      ( scen = 'R2' col = 30   hdr = 'WTWTSTCD01' )
      ( scen = 'R2' col = 31   hdr = 'WTWTSTCD02' )
      ( scen = 'R2' col = 32   hdr = 'WTWTSTCD03' )
      ( scen = 'R2' col = 33   hdr = 'WTWTSTCD04' )
      ( scen = 'R2' col = 34   hdr = 'WTWTSTCD05' )
      ( scen = 'R2' col = 35   hdr = 'WTWTSTCD06' )
      ( scen = 'R2' col = 36   hdr = 'WTEXNR01' )
      ( scen = 'R2' col = 37   hdr = 'WTEXNR02' )
      ( scen = 'R2' col = 38   hdr = 'WTEXNR03' )
      ( scen = 'R2' col = 39   hdr = 'WTEXNR04' )
      ( scen = 'R2' col = 40   hdr = 'WTEXNR05' )
      ( scen = 'R2' col = 41   hdr = 'WTEXNR06' )
      ( scen = 'R2' col = 42   hdr = 'WTEXRT01' )
      ( scen = 'R2' col = 43   hdr = 'WTEXRT02' )
      ( scen = 'R2' col = 44   hdr = 'WTEXRT03' )
      ( scen = 'R2' col = 45   hdr = 'WTEXRT04' )
      ( scen = 'R2' col = 46   hdr = 'WTEXRT05' )
      ( scen = 'R2' col = 47   hdr = 'WTEXRT06' )
      ( scen = 'R2' col = 48   hdr = 'WTWTEXRS01' )
      ( scen = 'R2' col = 49   hdr = 'WTWTEXRS02' )
      ( scen = 'R2' col = 50   hdr = 'WTWTEXRS03' )
      ( scen = 'R2' col = 51   hdr = 'WTWTEXRS04' )
      ( scen = 'R2' col = 52   hdr = 'WTWTEXRS05' )
      ( scen = 'R2' col = 53   hdr = 'WTWTEXRS06' )
      ( scen = 'R2' col = 54   hdr = 'WTEXDF01' )
      ( scen = 'R2' col = 55   hdr = 'WTEXDF02' )
      ( scen = 'R2' col = 56   hdr = 'WTEXDF03' )
      ( scen = 'R2' col = 57   hdr = 'WTEXDF04' )
      ( scen = 'R2' col = 58   hdr = 'WTEXDF05' )
      ( scen = 'R2' col = 59   hdr = 'WTEXDF06' )
      ( scen = 'R2' col = 60   hdr = 'WTEXDT01' )
      ( scen = 'R2' col = 61   hdr = 'WTEXDT02' )
      ( scen = 'R2' col = 62   hdr = 'WTEXDT03' )
      ( scen = 'R2' col = 63   hdr = 'WTEXDT04' )
      ( scen = 'R2' col = 64   hdr = 'WTEXDT05' )
      ( scen = 'R2' col = 65   hdr = 'WTEXDT06' )
    ) TO rt.

    " R3 - TAN details (21 identifiable headings)
    APPEND LINES OF VALUE tt_hdr(
      ( scen = 'R3' col = 1    hdr = 'VENDOR' )
      ( scen = 'R3' col = 2    hdr = 'COMPANY' )
      ( scen = 'R3' col = 3    hdr = 'ADDRESS' )
      ( scen = 'R3' col = 4    hdr = 'SECTIONCODE1' )
      ( scen = 'R3' col = 5    hdr = 'SECTIONCODE2' )
      ( scen = 'R3' col = 6    hdr = 'CERTIFICATE1' )
      ( scen = 'R3' col = 7    hdr = 'CERTIFICATE2' )
      ( scen = 'R3' col = 8    hdr = 'EXEMPTIONRATE1' )
      ( scen = 'R3' col = 9    hdr = 'EXEMPTIONRATE2' )
      ( scen = 'R3' col = 10   hdr = 'VALIDFROM1' )
      ( scen = 'R3' col = 11   hdr = 'VALIDFROM2' )
      ( scen = 'R3' col = 12   hdr = 'VALIDTO1' )
      ( scen = 'R3' col = 13   hdr = 'VALIDTO2' )
      ( scen = 'R3' col = 14   hdr = 'TAXTYPE1' )
      ( scen = 'R3' col = 15   hdr = 'TAXTYPE2' )
      ( scen = 'R3' col = 16   hdr = 'TAXCODE1' )
      ( scen = 'R3' col = 17   hdr = 'TAXCODE2' )
      ( scen = 'R3' col = 18   hdr = 'THRESHOLD1' )
      ( scen = 'R3' col = 19   hdr = 'THRESHOLD2' )
      ( scen = 'R3' col = 20   hdr = 'CURRENCY1' )
      ( scen = 'R3' col = 21   hdr = 'CURRENCY2' )
    ) TO rt.

    " R4 - BANK Key creation (9 identifiable headings)
    APPEND LINES OF VALUE tt_hdr(
      ( scen = 'R4' col = 1    hdr = 'FIELDTECHNICALNAME' )
      ( scen = 'R4' col = 2    hdr = 'BANKS' )
      ( scen = 'R4' col = 3    hdr = 'BANKL' )
      ( scen = 'R4' col = 4    hdr = 'BANKA' )
      ( scen = 'R4' col = 5    hdr = 'PROVZ' )
      ( scen = 'R4' col = 6    hdr = 'STRAS' )
      ( scen = 'R4' col = 7    hdr = 'ORT01' )
      ( scen = 'R4' col = 8    hdr = 'BRNCH' )
      ( scen = 'R4' col = 9    hdr = 'SWIFT' )
    ) TO rt.

    " R5 - Bank details update (8 identifiable headings)
    APPEND LINES OF VALUE tt_hdr(
      ( scen = 'R5' col = 1    hdr = 'FIELDTECHNICALNAME' )
      ( scen = 'R5' col = 2    hdr = 'LIFNR' )
      ( scen = 'R5' col = 3    hdr = 'BUKRS' )
      ( scen = 'R5' col = 4    hdr = 'BANKS' )
      ( scen = 'R5' col = 5    hdr = 'BANKL' )
      ( scen = 'R5' col = 6    hdr = 'BANKN' )
      ( scen = 'R5' col = 7    hdr = 'KOINH' )
      ( scen = 'R5' col = 8    hdr = 'IBAN' )
    ) TO rt.

    " R6 - Vendor extension (5 identifiable headings)
    APPEND LINES OF VALUE tt_hdr(
      ( scen = 'R6' col = 1    hdr = 'FIELDTECHNICALNAME' )
      ( scen = 'R6' col = 9    hdr = 'AKONT' )
      ( scen = 'R6' col = 12   hdr = 'WAERS' )
      ( scen = 'R6' col = 13   hdr = 'KALSK' )
      ( scen = 'R6' col = 14   hdr = 'WEBRE' )
    ) TO rt.

    " R7 - CIN details (15 identifiable headings)
    APPEND LINES OF VALUE tt_hdr(
      ( scen = 'R7' col = 1    hdr = 'VENDORACCOUNTNUMBER' )
      ( scen = 'R7' col = 2    hdr = 'COMPANYCODE' )
      ( scen = 'R7' col = 3    hdr = 'ADDRESSVIEW' )
      ( scen = 'R7' col = 4    hdr = 'ECCNUMBER' )
      ( scen = 'R7' col = 5    hdr = 'EXCISEREGISTRATIONNUMBER' )
      ( scen = 'R7' col = 6    hdr = 'EXCISERANGE' )
      ( scen = 'R7' col = 7    hdr = 'EXCISEDIVISION' )
      ( scen = 'R7' col = 8    hdr = 'EXCISECOMMISSIONERATE' )
      ( scen = 'R7' col = 9    hdr = 'CENTRALSALESTAXNUMBER' )
      ( scen = 'R7' col = 10   hdr = 'LOCALSALESTAXNUMBER' )
      ( scen = 'R7' col = 11   hdr = 'SERVICETAXREGISTRATIONNUMBER' )
      ( scen = 'R7' col = 12   hdr = 'PERMANENTACCOUNTNUMBER' )
      ( scen = 'R7' col = 13   hdr = 'SSISTATUS' )
      ( scen = 'R7' col = 14   hdr = 'EXCTAXINDVENDOR' )
      ( scen = 'R7' col = 15   hdr = 'TYPEOFVENDOR' )
    ) TO rt.

    " R8 - Patner function (35 identifiable headings)
    APPEND LINES OF VALUE tt_hdr(
      ( scen = 'R8' col = 1    hdr = 'LIFNR' )
      ( scen = 'R8' col = 2    hdr = 'BUKRS' )
      ( scen = 'R8' col = 3    hdr = 'EKORG' )
      ( scen = 'R8' col = 4    hdr = 'D0320' )
      ( scen = 'R8' col = 5    hdr = 'USEZAV' )
      ( scen = 'R8' col = 6    hdr = 'PARVW05' )
      ( scen = 'R8' col = 7    hdr = 'PARVW06' )
      ( scen = 'R8' col = 8    hdr = 'PARVW07' )
      ( scen = 'R8' col = 9    hdr = 'PARVW08' )
      ( scen = 'R8' col = 10   hdr = 'PARVW09' )
      ( scen = 'R8' col = 11   hdr = 'PARVW10' )
      ( scen = 'R8' col = 12   hdr = 'PARVW11' )
      ( scen = 'R8' col = 13   hdr = 'PARVW12' )
      ( scen = 'R8' col = 14   hdr = 'PARVW13' )
      ( scen = 'R8' col = 15   hdr = 'PARVW14' )
      ( scen = 'R8' col = 16   hdr = 'PARVW15' )
      ( scen = 'R8' col = 17   hdr = 'GPARN05' )
      ( scen = 'R8' col = 18   hdr = 'GPARN06' )
      ( scen = 'R8' col = 19   hdr = 'GPARN07' )
      ( scen = 'R8' col = 20   hdr = 'GPARN08' )
      ( scen = 'R8' col = 21   hdr = 'GPARN09' )
      ( scen = 'R8' col = 22   hdr = 'GPARN10' )
      ( scen = 'R8' col = 23   hdr = 'GPARN11' )
      ( scen = 'R8' col = 24   hdr = 'GPARN12' )
      ( scen = 'R8' col = 25   hdr = 'GPARN13' )
      ( scen = 'R8' col = 26   hdr = 'GPARN14' )
      ( scen = 'R8' col = 27   hdr = 'GPARN15' )
      ( scen = 'R8' col = 28   hdr = 'PARVW01' )
      ( scen = 'R8' col = 29   hdr = 'PARVW02' )
      ( scen = 'R8' col = 30   hdr = 'PARVW03' )
      ( scen = 'R8' col = 31   hdr = 'PARVW04' )
      ( scen = 'R8' col = 32   hdr = 'GPARN01' )
      ( scen = 'R8' col = 33   hdr = 'GPARN02' )
      ( scen = 'R8' col = 34   hdr = 'GPARN03' )
      ( scen = 'R8' col = 35   hdr = 'GPARN04' )
    ) TO rt.

    " R9 - Block_Unblocked (9 identifiable headings)
    APPEND LINES OF VALUE tt_hdr(
      ( scen = 'R9' col = 1    hdr = 'TECHNAME' )
      ( scen = 'R9' col = 2    hdr = 'LIFNR' )
      ( scen = 'R9' col = 3    hdr = 'BUKRS' )
      ( scen = 'R9' col = 4    hdr = 'EKORG' )
      ( scen = 'R9' col = 5    hdr = 'SPERR' )
      ( scen = 'R9' col = 6    hdr = 'SPERR1' )
      ( scen = 'R9' col = 7    hdr = 'SPERM' )
      ( scen = 'R9' col = 8    hdr = 'SPERM1' )
      ( scen = 'R9' col = 9    hdr = 'SPERQ' )
    ) TO rt.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_excel DEFINITION FINAL.
  PUBLIC SECTION.
    "! Returns the data rows of the tab that carries this scenario's columns.
    "! IT_HDR says which heading belongs above which column: the tab is chosen
    "! by how many of those headings its heading line has - so the tab NAME
    "! does not matter - and every cell is then taken from the column that
    "! actually carries its heading, whatever position that is.
    "! IV_SHEET is only the tie-breaker and the fallback.
    METHODS read
      IMPORTING iv_file       TYPE rlgrap-filename
                iv_sheet      TYPE string
                iv_from_pc    TYPE abap_bool
                it_hdr        TYPE tt_hdr OPTIONAL
      EXPORTING et_row        TYPE tt_row
                ev_sheet      TYPE string
                ev_moved      TYPE i
      RAISING   lcx_upl.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_pos,
             tgt TYPE i,
             src TYPE i,
           END OF ty_pos.

    METHODS load_bin IMPORTING iv_file TYPE rlgrap-filename iv_from_pc TYPE abap_bool
                     RETURNING VALUE(rv) TYPE xstring RAISING lcx_upl.

    "! One worksheet as a table of rows, heading lines included.
    METHODS sheet_rows
      IMPORTING io_xl     TYPE REF TO cl_fdt_xl_spreadsheet
                iv_name   TYPE string
      RETURNING VALUE(rt) TYPE tt_row
      RAISING   lcx_upl.

    "! How many of the scenario's headings this line carries.
    METHODS score
      IMPORTING it_head   TYPE tt_cell
                it_hdr    TYPE tt_hdr
      RETURNING VALUE(rv) TYPE i.
ENDCLASS.

CLASS lcl_excel IMPLEMENTATION.

  METHOD load_bin.
    IF iv_from_pc = abap_true.
      DATA: lt_bin TYPE solix_tab,
            lv_len TYPE i.
      cl_gui_frontend_services=>gui_upload(
        EXPORTING filename   = CONV string( iv_file )
                  filetype   = 'BIN'
        IMPORTING filelength = lv_len
        CHANGING  data_tab   = lt_bin
        EXCEPTIONS OTHERS    = 1 ).
      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW lcx_upl( |Cannot read the file from the PC: { iv_file }| ).
      ENDIF.
      rv = cl_bcs_convert=>solix_to_xstring( it_solix = lt_bin iv_size = lv_len ).
    ELSE.
      TRY.
          DATA lv_osmsg TYPE string.
          OPEN DATASET iv_file FOR INPUT IN BINARY MODE MESSAGE lv_osmsg.
          IF sy-subrc <> 0.
            RAISE EXCEPTION NEW lcx_upl( |Cannot open the server file { iv_file }: { lv_osmsg }| ).
          ENDIF.
          READ DATASET iv_file INTO rv.
          CLOSE DATASET iv_file.
        CATCH cx_sy_file_open cx_sy_file_authority.
          RAISE EXCEPTION NEW lcx_upl( |No authorisation for the server file: { iv_file }| ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD sheet_rows.
    DATA lo_ref TYPE REF TO data.
    TRY.
        lo_ref = io_xl->if_fdt_doc_spreadsheet~get_itab_from_worksheet( CONV #( iv_name ) ).
      CATCH cx_root INTO DATA(lx2).
        RAISE EXCEPTION NEW lcx_upl(
          |Tab "{ iv_name }" could not be converted: { lx2->get_text( ) }| ).
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
      DATA lv_c TYPE i.
      CLEAR lv_c.
      DO.
        lv_c = lv_c + 1.
        ASSIGN COMPONENT lv_c OF STRUCTURE <ls_lin> TO <lv_val>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        APPEND condense( CONV string( <lv_val> ) ) TO ls_row-cells.
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
    LOOP AT it_hdr INTO DATA(ls_w).
      IF line_exists( lt_k[ table_line = CONV string( ls_w-hdr ) ] ).
        rv = rv + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD read.
    CLEAR: et_row, ev_sheet, ev_moved.
    DATA(lv_x) = load_bin( iv_file = iv_file iv_from_pc = iv_from_pc ).

    DATA lo_xl TYPE REF TO cl_fdt_xl_spreadsheet.
    TRY.
        lo_xl = NEW cl_fdt_xl_spreadsheet( document_name = CONV string( iv_file )
                                           xdocument     = lv_x ).
      CATCH cx_root INTO DATA(lx).
        RAISE EXCEPTION NEW lcx_upl( |The file is not a readable .xlsx workbook: { lx->get_text( ) }| ).
    ENDTRY.

    lo_xl->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = DATA(lt_ws) ).
    IF lt_ws IS INITIAL.
      RAISE EXCEPTION NEW lcx_upl( |{ iv_file } contains no worksheet.| ).
    ENDIF.

    " The tab whose name matches, if there is one. Names are compared on
    " letters and digits only, so trailing blanks, capitalisation and spaces
    " against underscores make no difference.
    DATA lv_named TYPE string.
    DATA(lv_want) = lcl_util=>squash( iv_sheet ).
    LOOP AT lt_ws INTO DATA(lv_ws).
      IF lcl_util=>squash( CONV string( lv_ws ) ) = lv_want.
        lv_named = lv_ws.
        EXIT.
      ENDIF.
    ENDLOOP.

    " What decides is the heading line: over every tab, and over the first
    " few lines of each, the line carrying most of this scenario's headings
    " wins. That way the tab name does not matter, and neither does a title
    " line sitting above the headings. Equally good tabs go to the one named
    " for the scenario.
    CONSTANTS lc_scan TYPE i VALUE 10.
    DATA lv_use  TYPE string.
    DATA lt_hit  TYPE tt_row.
    DATA lv_best TYPE i.
    DATA lv_hrow TYPE i.
    IF it_hdr IS NOT INITIAL.
      LOOP AT lt_ws INTO DATA(lv_w2).
        DATA(lt_r) = sheet_rows( io_xl = lo_xl iv_name = CONV string( lv_w2 ) ).
        DATA(lv_max) = COND i( WHEN lines( lt_r ) < lc_scan THEN lines( lt_r )
                               ELSE lc_scan ).
        DO lv_max TIMES.
          DATA(lv_i)  = sy-index.
          DATA(lv_sc) = score( it_head = lt_r[ lv_i ]-cells it_hdr = it_hdr ).
          IF lv_sc > lv_best
          OR ( lv_sc > 0 AND lv_sc = lv_best AND lv_w2 = lv_named AND lv_use <> lv_named ).
            lv_best = lv_sc.
            lv_use  = lv_w2.
            lt_hit  = lt_r.
            lv_hrow = lv_i.
          ENDIF.
        ENDDO.
      ENDLOOP.
    ENDIF.

    " Nothing recognisable - fall back to the name, then to the only tab
    " there is, and to P_SKIP for the number of heading lines.
    IF lv_best = 0.
      CLEAR lt_hit.
      lv_use  = lv_named.
      lv_hrow = p_skip.
      IF lv_use IS INITIAL AND lines( lt_ws ) = 1.
        lv_use = lt_ws[ 1 ].
      ENDIF.
    ENDIF.

    IF lv_use IS INITIAL.
      DATA lv_have TYPE string.
      LOOP AT lt_ws INTO DATA(lv_n).
        lv_have = COND string( WHEN lv_have IS INITIAL THEN lv_n ELSE |{ lv_have }, { lv_n }| ).
      ENDLOOP.
      RAISE EXCEPTION NEW lcx_upl(
        |No tab in this workbook carries the columns of "{ iv_sheet }". Tabs found: { lv_have }| ).
    ENDIF.

    IF lt_hit IS INITIAL.
      lt_hit = sheet_rows( io_xl = lo_xl iv_name = lv_use ).
    ENDIF.
    ev_sheet = lv_use.
    IF lt_hit IS INITIAL.
      RAISE EXCEPTION NEW lcx_upl( |Tab "{ lv_use }" is empty.| ).
    ENDIF.

    " Bind each column to the position where its heading really is. A heading
    " that appears twice on the tab is ambiguous and is left alone, as is one
    " the file does not have at all - those columns keep their position.
    TYPES: BEGIN OF ty_h, key TYPE string, col TYPE i, n TYPE i, END OF ty_h.
    DATA lt_h   TYPE SORTED TABLE OF ty_h WITH UNIQUE KEY key.
    DATA lt_pos TYPE SORTED TABLE OF ty_pos WITH UNIQUE KEY tgt.
    DATA lt_src TYPE SORTED TABLE OF i WITH NON-UNIQUE KEY table_line.
    IF lv_hrow > 0 AND lv_hrow <= lines( lt_hit ) AND it_hdr IS NOT INITIAL.
      DATA lv_hc TYPE i.
      DATA(lt_head) = lt_hit[ lv_hrow ]-cells.
      " A tab may spread its headings over two lines: a blank heading is
      " filled from the next line, but only when that line is itself part of
      " the heading block - a line carrying none of this scenario's headings
      " is data and is left alone.
      IF lv_hrow < lines( lt_hit ).
        DATA(lt_next) = lt_hit[ lv_hrow + 1 ]-cells.
        IF score( it_head = lt_next it_hdr = it_hdr ) > 0.
          DATA lv_fc TYPE i.
          LOOP AT lt_next INTO DATA(lv_fill).
            lv_fc = sy-tabix.
            IF lv_fill IS INITIAL.
              CONTINUE.
            ENDIF.
            IF lv_fc > lines( lt_head ).
              APPEND INITIAL LINE TO lt_head.
            ENDIF.
            READ TABLE lt_head ASSIGNING FIELD-SYMBOL(<lv_hd>) INDEX lv_fc.
            IF sy-subrc = 0 AND <lv_hd> IS INITIAL.
              <lv_hd> = lv_fill.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
      LOOP AT lt_head INTO DATA(lv_ht).
        lv_hc = sy-tabix.
        DATA(lv_key) = lcl_util=>squash( lv_ht ).
        IF lv_key IS INITIAL.
          CONTINUE.
        ENDIF.
        READ TABLE lt_h ASSIGNING FIELD-SYMBOL(<ls_h>) WITH KEY key = lv_key.
        IF sy-subrc = 0.
          <ls_h>-n = <ls_h>-n + 1.
        ELSE.
          INSERT VALUE ty_h( key = lv_key col = lv_hc n = 1 ) INTO TABLE lt_h.
        ENDIF.
      ENDLOOP.
      LOOP AT it_hdr INTO DATA(ls_w).
        READ TABLE lt_h INTO DATA(ls_h) WITH KEY key = CONV string( ls_w-hdr ).
        IF sy-subrc = 0 AND ls_h-n = 1.
          INSERT VALUE ty_pos( tgt = ls_w-col src = ls_h-col ) INTO TABLE lt_pos.
          INSERT ls_h-col INTO TABLE lt_src.
          IF ls_h-col <> ls_w-col.
            ev_moved = ev_moved + 1.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    " Widest position the handlers may ask for.
    DATA lv_wide TYPE i.
    LOOP AT lt_pos INTO DATA(ls_p).
      IF ls_p-tgt > lv_wide.
        lv_wide = ls_p-tgt.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_hit INTO DATA(ls_src).
      IF ls_src-row <= lv_hrow.
        CONTINUE.
      ENDIF.

      DATA ls_out TYPE ty_row.
      CLEAR ls_out.
      ls_out-row = ls_src-row.
      IF lt_pos IS INITIAL.
        ls_out-cells = ls_src-cells.
      ELSE.
        DATA(lv_n2) = COND i( WHEN lines( ls_src-cells ) > lv_wide
                              THEN lines( ls_src-cells ) ELSE lv_wide ).
        DO lv_n2 TIMES.
          DATA(lv_t) = sy-index.
          DATA lv_s TYPE i.
          READ TABLE lt_pos INTO ls_p WITH KEY tgt = lv_t.
          IF sy-subrc = 0.
            lv_s = ls_p-src.
          ELSEIF line_exists( lt_src[ table_line = lv_t ] ).
            " This position has no heading of its own and the column sitting
            " there belongs to another field - so there is nothing to read.
            lv_s = 0.
          ELSE.
            lv_s = lv_t.
          ENDIF.
          DATA lv_v TYPE string.
          CLEAR lv_v.
          IF lv_s > 0 AND lv_s <= lines( ls_src-cells ).
            lv_v = ls_src-cells[ lv_s ].
          ENDIF.
          APPEND lv_v TO ls_out-cells.
        ENDDO.
      ENDIF.

      APPEND ls_out TO et_row.
    ENDLOOP.

    IF et_row IS INITIAL.
      RAISE EXCEPTION NEW lcx_upl( |Tab "{ lv_use }" contains no rows below its heading.| ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Log
*----------------------------------------------------------------------*
CLASS lcl_log DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS add
      IMPORTING iv_row TYPE i
                iv_k1  TYPE clike OPTIONAL
                iv_k2  TYPE clike OPTIONAL
                iv_k3  TYPE clike OPTIONAL
                iv_ty  TYPE bapi_mtype
                iv_txt TYPE clike
                iv_id  TYPE symsgid OPTIONAL
                iv_no  TYPE symsgno OPTIONAL
                iv_st  TYPE clike OPTIONAL
                iv_fl  TYPE clike OPTIONAL.
    METHODS add_ret
      IMPORTING iv_row TYPE i
                iv_k1  TYPE clike OPTIONAL
                iv_k2  TYPE clike OPTIONAL
                iv_k3  TYPE clike OPTIONAL
                is_ret TYPE bapiret2.
    METHODS has_error IMPORTING iv_row TYPE i RETURNING VALUE(rv) TYPE abap_bool.
    METHODS display.
  PRIVATE SECTION.
    DATA mt_msg TYPE tt_msg.
ENDCLASS.

CLASS lcl_log IMPLEMENTATION.

  METHOD add.
    APPEND VALUE #(
      icon    = COND #( WHEN iv_ty CA 'EAX' THEN icon_red_light
                        WHEN iv_ty = 'W'    THEN icon_yellow_light
                        ELSE                     icon_green_light )
      xlsrow  = iv_row
      key1    = iv_k1
      key2    = iv_k2
      key3    = iv_k3
      msgty   = iv_ty
      msgid   = iv_id
      msgno   = iv_no
      struc   = iv_st
      fldnm   = iv_fl
      message = iv_txt ) TO mt_msg.
  ENDMETHOD.

  METHOD add_ret.
    IF is_ret-type IS INITIAL AND is_ret-message IS INITIAL.
      RETURN.
    ENDIF.
    add( iv_row = iv_row iv_k1 = iv_k1 iv_k2 = iv_k2 iv_k3 = iv_k3
         iv_ty  = is_ret-type iv_id = is_ret-id iv_no = is_ret-number
         iv_fl  = is_ret-field iv_txt = is_ret-message ).
  ENDMETHOD.

  METHOD has_error.
    rv = xsdbool( line_exists( mt_msg[ xlsrow = iv_row msgty = 'E' ] )
               OR line_exists( mt_msg[ xlsrow = iv_row msgty = 'A' ] ) ).
  ENDMETHOD.

  METHOD display.
    IF mt_msg IS INITIAL.
      MESSAGE 'No data rows were found to process.' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

    " count distinct rows
    DATA lt_r TYPE SORTED TABLE OF i WITH UNIQUE KEY table_line.
    LOOP AT mt_msg INTO DATA(ls).
      INSERT ls-xlsrow INTO TABLE lt_r.
    ENDLOOP.
    DATA: lv_ok TYPE i, lv_er TYPE i.
    LOOP AT lt_r INTO DATA(lv_rr).
      IF has_error( lv_rr ) = abap_true.
        lv_er = lv_er + 1.
      ELSE.
        lv_ok = lv_ok + 1.
      ENDIF.
    ENDLOOP.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                                CHANGING  t_table      = mt_msg ).
        lo_alv->get_functions( )->set_all( abap_true ).
        lo_alv->get_columns( )->set_optimize( abap_true ).

        DATA(lo_cols) = lo_alv->get_columns( ).
        DATA(lt_lbl) = VALUE string_table(
          ( |ICON;Status| ) ( |XLSROW;Excel row| ) ( |KEY1;Vendor / key| )
          ( |KEY2;Comp.code| ) ( |KEY3;Purch.org| ) ( |MSGTY;Type| )
          ( |MSGID;MsgID| ) ( |MSGNO;MsgNo| ) ( |STRUC;API structure| )
          ( |FLDNM;API field| ) ( |MESSAGE;Message| ) ).
        LOOP AT lt_lbl INTO DATA(lv_pair).
          SPLIT lv_pair AT ';' INTO DATA(lv_c) DATA(lv_t).
          TRY.
              DATA(lo_col) = lo_cols->get_column( CONV lvc_fname( lv_c ) ).
              lo_col->set_short_text( CONV scrtext_s( lv_t ) ).
              lo_col->set_medium_text( CONV scrtext_m( lv_t ) ).
              lo_col->set_long_text( CONV scrtext_l( lv_t ) ).
            CATCH cx_salv_not_found.
          ENDTRY.
        ENDLOOP.

        DATA(lv_hdr) = |{ COND string( WHEN p_test = abap_true
                                       THEN 'TEST RUN - nothing was posted'
                                       ELSE 'PRODUCTIVE RUN' ) }| &&
                       |    Rows OK: { lv_ok }    Rows with errors: { lv_er }|.
        lo_alv->get_display_settings( )->set_list_header( CONV lvc_title( lv_hdr ) ).
        lo_alv->display( ).

      CATCH cx_salv_msg INTO DATA(lx).
        " MESSAGE takes a data object, not an expression.
        DATA(lv_err) = lx->get_text( ).
        MESSAGE lv_err TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Configuration buffer
*----------------------------------------------------------------------*
CLASS lcl_cfg DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS get RETURNING VALUE(ro) TYPE REF TO lcl_cfg.

    METHODS ok_bukrs IMPORTING iv TYPE clike RETURNING VALUE(rv) TYPE abap_bool.
    METHODS ok_ekorg IMPORTING iv TYPE clike RETURNING VALUE(rv) TYPE abap_bool.
    METHODS ok_ktokk IMPORTING iv TYPE clike RETURNING VALUE(rv) TYPE abap_bool.
    METHODS ok_zterm IMPORTING iv TYPE clike RETURNING VALUE(rv) TYPE abap_bool.
    METHODS ok_parvw IMPORTING iv TYPE clike RETURNING VALUE(rv) TYPE abap_bool.
    METHODS ok_land1 IMPORTING iv TYPE clike RETURNING VALUE(rv) TYPE abap_bool.
    METHODS ok_bankl IMPORTING iv_banks TYPE clike iv_bankl TYPE clike RETURNING VALUE(rv) TYPE abap_bool.
    METHODS ok_akont IMPORTING iv_bukrs TYPE clike iv_saknr TYPE clike RETURNING VALUE(rv) TYPE abap_bool.
    METHODS ok_witht IMPORTING iv_land1 TYPE clike iv_witht TYPE clike RETURNING VALUE(rv) TYPE abap_bool.
    METHODS ok_wcode IMPORTING iv_land1 TYPE clike iv_witht TYPE clike iv_wcd TYPE clike
                     RETURNING VALUE(rv) TYPE abap_bool.

    "! BP grouping for an account group. NOT identity - CVIC_VEND_TO_BP1 maps
    "! e.g. Z002->Z0X2, Z003->Z0X3, Z009->Z019, Z012->Z022, Z007->ZPLN.
    METHODS bp_group  IMPORTING iv_ktokk TYPE clike RETURNING VALUE(rv) TYPE bu_group.
    METHODS bp_roles  IMPORTING iv_ktokk TYPE clike RETURNING VALUE(rt) TYPE ty_roles.
    METHODS title_key IMPORTING iv_text  TYPE clike RETURNING VALUE(rv) TYPE tsad3t-title.

    METHODS vend_exists IMPORTING iv_lifnr TYPE lifnr RETURNING VALUE(rv) TYPE abap_bool.

    "! The API takes no "modify" task, so every node has to say insert or
    "! update. These answer which one it is.
    METHODS has_lfb1 IMPORTING VALUE(iv_lifnr) TYPE lifnr
                               VALUE(iv_bukrs) TYPE bukrs
                     RETURNING VALUE(rv)       TYPE abap_bool.
    METHODS has_lfm1 IMPORTING VALUE(iv_lifnr) TYPE lifnr
                               VALUE(iv_ekorg) TYPE ekorg
                     RETURNING VALUE(rv)       TYPE abap_bool.
    METHODS has_lfbw IMPORTING VALUE(iv_lifnr) TYPE lifnr
                               VALUE(iv_bukrs) TYPE bukrs
                               VALUE(iv_witht) TYPE witht
                     RETURNING VALUE(rv)       TYPE abap_bool.
    METHODS has_lfbk IMPORTING VALUE(iv_lifnr) TYPE lifnr
                               VALUE(iv_banks) TYPE banks
                               VALUE(iv_bankl) TYPE bankk
                               VALUE(iv_bankn) TYPE bankn
                     RETURNING VALUE(rv)       TYPE abap_bool.
    METHODS has_wyt3 IMPORTING VALUE(iv_lifnr) TYPE lifnr
                               VALUE(iv_ekorg) TYPE ekorg
                               VALUE(iv_parvw) TYPE parvw
                     RETURNING VALUE(rv)       TYPE abap_bool.
    METHODS vend_land1  IMPORTING iv_lifnr TYPE lifnr RETURNING VALUE(rv) TYPE land1.
    METHODS vend_pan    IMPORTING iv_lifnr TYPE lifnr RETURNING VALUE(rv) TYPE j_1ipanno.
    METHODS vend_guid   IMPORTING iv_lifnr TYPE lifnr RETURNING VALUE(rv) TYPE bu_partner_guid.

  PRIVATE SECTION.
    CLASS-DATA go TYPE REF TO lcl_cfg.
    METHODS constructor.

    TYPES: BEGIN OF ty_g2b, ktokk TYPE ktokk, grouping TYPE bu_group, END OF ty_g2b,
           BEGIN OF ty_r2b, ktokk TYPE ktokk, role     TYPE bu_role,       END OF ty_r2b,
           BEGIN OF ty_ttl, txt   TYPE tsad3t-title_medi, key TYPE tsad3t-title, END OF ty_ttl.

    DATA: mt_bukrs TYPE SORTED TABLE OF bukrs  WITH UNIQUE KEY table_line,
          mt_ekorg TYPE SORTED TABLE OF ekorg  WITH UNIQUE KEY table_line,
          mt_ktokk TYPE SORTED TABLE OF ktokk  WITH UNIQUE KEY table_line,
          mt_zterm TYPE SORTED TABLE OF dzterm WITH UNIQUE KEY table_line,
          mt_parvw TYPE SORTED TABLE OF parvw  WITH UNIQUE KEY table_line,
          mt_land1 TYPE SORTED TABLE OF land1  WITH UNIQUE KEY table_line,
          mt_g2b   TYPE SORTED TABLE OF ty_g2b WITH UNIQUE KEY ktokk,
          mt_r2b   TYPE SORTED TABLE OF ty_r2b WITH NON-UNIQUE KEY ktokk,
          mt_ttl   TYPE STANDARD TABLE OF ty_ttl WITH EMPTY KEY.
ENDCLASS.

CLASS lcl_cfg IMPLEMENTATION.

  METHOD get.
    IF go IS INITIAL.
      go = NEW #( ).
    ENDIF.
    ro = go.
  ENDMETHOD.

  METHOD constructor.
    " Every one of these targets is declared WITH UNIQUE KEY, and moving a
    " result set that contains duplicates into such a table raises
    " ITAB_DUPLICATE_KEY - a short dump, not a catchable error.
    "
    " T052 is the one that bites: it holds one row per instalment, so a
    " payment term with three instalments appears three times. T005 has a
    " row per country, but the same applies the moment any of these tables
    " is configured with more than one row per code. DISTINCT removes the
    " duplicates in the database, so the move can never fail.
    SELECT DISTINCT bukrs FROM t001  INTO TABLE @DATA(lt1). mt_bukrs = lt1.
    SELECT DISTINCT ekorg FROM t024e INTO TABLE @DATA(lt2). mt_ekorg = lt2.
    SELECT DISTINCT ktokk FROM t077k INTO TABLE @DATA(lt3). mt_ktokk = lt3.
    SELECT DISTINCT zterm FROM t052  INTO TABLE @DATA(lt4). mt_zterm = lt4.
    SELECT DISTINCT parvw FROM tpar  INTO TABLE @DATA(lt5). mt_parvw = lt5.
    SELECT DISTINCT land1 FROM t005  INTO TABLE @DATA(lt6). mt_land1 = lt6.

    " MT_G2B is keyed on the account group alone. If the customising ever
    " maps one account group to more than one BP grouping, INSERT reports
    " it with SY-SUBRC 4 and the first entry wins, instead of dumping.
    SELECT account_group AS ktokk, grouping
      FROM cvic_vend_to_bp1 INTO TABLE @DATA(lt_g2b).
    LOOP AT lt_g2b INTO DATA(ls_g2b).
      INSERT VALUE ty_g2b( ktokk    = ls_g2b-ktokk
                           grouping = ls_g2b-grouping ) INTO TABLE mt_g2b.
    ENDLOOP.

    SELECT account_group AS ktokk, role
      FROM cvic_vend_to_bp2 INTO CORRESPONDING FIELDS OF TABLE @mt_r2b.

    SELECT title_medi AS txt, title AS key
      FROM tsad3t WHERE langu = @sy-langu
      INTO CORRESPONDING FIELDS OF TABLE @mt_ttl.
  ENDMETHOD.

  METHOD ok_bukrs.
    rv = xsdbool( line_exists( mt_bukrs[ table_line = CONV bukrs( iv ) ] ) ).
  ENDMETHOD.
  METHOD ok_ekorg.
    rv = xsdbool( line_exists( mt_ekorg[ table_line = CONV ekorg( iv ) ] ) ).
  ENDMETHOD.
  METHOD ok_ktokk.
    rv = xsdbool( line_exists( mt_ktokk[ table_line = CONV ktokk( iv ) ] ) ).
  ENDMETHOD.
  METHOD ok_zterm.
    rv = xsdbool( line_exists( mt_zterm[ table_line = CONV dzterm( iv ) ] ) ).
  ENDMETHOD.
  METHOD ok_parvw.
    rv = xsdbool( line_exists( mt_parvw[ table_line = CONV parvw( iv ) ] ) ).
  ENDMETHOD.
  METHOD ok_land1.
    rv = xsdbool( line_exists( mt_land1[ table_line = CONV land1( iv ) ] ) ).
  ENDMETHOD.

  METHOD ok_bankl.
    DATA lv TYPE abap_bool.
    SELECT SINGLE @abap_true FROM bnka
      WHERE banks = @iv_banks AND bankl = @iv_bankl AND loevm = @space
      INTO @lv.
    rv = xsdbool( lv = abap_true ).
  ENDMETHOD.

  METHOD ok_akont.
    DATA lv TYPE abap_bool.
    SELECT SINGLE @abap_true FROM skb1
      WHERE bukrs = @iv_bukrs AND saknr = @iv_saknr AND mitkz = 'K'
      INTO @lv.
    rv = xsdbool( lv = abap_true ).
  ENDMETHOD.

  METHOD ok_witht.
    DATA lv TYPE abap_bool.
    SELECT SINGLE @abap_true FROM t059p
      WHERE land1 = @iv_land1 AND witht = @iv_witht INTO @lv.
    rv = xsdbool( lv = abap_true ).
  ENDMETHOD.

  METHOD ok_wcode.
    DATA lv TYPE abap_bool.
    SELECT SINGLE @abap_true FROM t059z
      WHERE land1 = @iv_land1 AND witht = @iv_witht AND wt_withcd = @iv_wcd
      INTO @lv.
    rv = xsdbool( lv = abap_true ).
  ENDMETHOD.

  METHOD bp_group.
    CLEAR rv.
    TRY.
        rv = mt_g2b[ ktokk = CONV ktokk( iv_ktokk ) ]-grouping.
      CATCH cx_sy_itab_line_not_found.
        CLEAR rv.
    ENDTRY.
  ENDMETHOD.

  METHOD bp_roles.
    LOOP AT mt_r2b INTO DATA(ls) WHERE ktokk = iv_ktokk.
      APPEND ls-role TO rt.
    ENDLOOP.
  ENDMETHOD.

  METHOD title_key.
    CLEAR rv.
    IF iv_text IS INITIAL.
      RETURN.
    ENDIF.
    " Templates hold the title TEXT ("Company"); the API needs the key (0003).
    LOOP AT mt_ttl INTO DATA(ls).
      IF to_upper( CONV string( ls-txt ) ) = to_upper( CONV string( iv_text ) ).
        rv = ls-key.
        RETURN.
      ENDIF.
    ENDLOOP.
    IF iv_text CO '0123456789 '.
      rv = iv_text.
    ENDIF.
  ENDMETHOD.

  METHOD has_lfb1.
    SELECT SINGLE @abap_true FROM lfb1
      WHERE lifnr = @iv_lifnr AND bukrs = @iv_bukrs INTO @rv.
  ENDMETHOD.

  METHOD has_lfm1.
    SELECT SINGLE @abap_true FROM lfm1
      WHERE lifnr = @iv_lifnr AND ekorg = @iv_ekorg INTO @rv.
  ENDMETHOD.

  METHOD has_lfbw.
    SELECT SINGLE @abap_true FROM lfbw
      WHERE lifnr = @iv_lifnr AND bukrs = @iv_bukrs AND witht = @iv_witht INTO @rv.
  ENDMETHOD.

  METHOD has_lfbk.
    SELECT SINGLE @abap_true FROM lfbk
      WHERE lifnr = @iv_lifnr AND banks = @iv_banks
        AND bankl = @iv_bankl AND bankn = @iv_bankn INTO @rv.
  ENDMETHOD.

  METHOD has_wyt3.
    SELECT SINGLE @abap_true FROM wyt3
      WHERE lifnr = @iv_lifnr AND ekorg = @iv_ekorg AND parvw = @iv_parvw INTO @rv.
  ENDMETHOD.

  METHOD vend_exists.
    DATA lv TYPE abap_bool.
    SELECT SINGLE @abap_true FROM lfa1 WHERE lifnr = @iv_lifnr INTO @lv.
    rv = xsdbool( lv = abap_true ).
  ENDMETHOD.

  METHOD vend_land1.
    SELECT SINGLE land1 FROM lfa1 WHERE lifnr = @iv_lifnr INTO @rv.
  ENDMETHOD.

  METHOD vend_pan.
    SELECT SINGLE j_1ipanno FROM lfa1 WHERE lifnr = @iv_lifnr INTO @rv.
  ENDMETHOD.

  METHOD vend_guid.
    SELECT SINGLE partner_guid FROM cvi_vend_link WHERE vendor = @iv_lifnr INTO @rv.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* CVI poster - VALIDATE_SINGLE then MAINTAIN
*----------------------------------------------------------------------*
CLASS lcl_cvis DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS constructor IMPORTING io_log TYPE REF TO lcl_log.
    METHODS post
      IMPORTING iv_row  TYPE i
                iv_k1   TYPE clike OPTIONAL
                iv_k2   TYPE clike OPTIONAL
                iv_k3   TYPE clike OPTIONAL
                is_data TYPE cvis_ei_extern
      RETURNING VALUE(rv_ok) TYPE abap_bool.
  PRIVATE SECTION.
    " The business partner keeps a global memory for the logical unit of work
    " that has just been closed - including the save mode. A COMMIT does not
    " clear it, and the next row is then refused with "Parameter IV_X_SAVE is
    " ' ' for FM BUPA_CREATE_FROM_DATA. It should be 'A'". Initialising the
    " memory gives every row a clean start.
    METHODS reset_bp.
    DATA mo_log TYPE REF TO lcl_log.
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
    rv_ok = abap_true.

    " ---- 1. validation. Runs in test AND productive mode. --------------
    "      ET_RETURN_MAP carries BAPISTRUCNAME / BAPIFLDNM so the user can be
    "      pointed at the offending template column.
    DATA lt_map TYPE mdg_bs_bp_msgmap_t.
    TRY.
        cl_md_bp_maintain=>validate_single(
          EXPORTING i_data        = is_data
          IMPORTING et_return_map = lt_map ).
      CATCH cx_root INTO DATA(lx1).
        mo_log->add( iv_row = iv_row iv_k1 = iv_k1 iv_k2 = iv_k2 iv_k3 = iv_k3
                     iv_ty = 'E' iv_txt = |Validation failed: { lx1->get_text( ) }| ).
        rv_ok = abap_false.
        RETURN.
    ENDTRY.

    LOOP AT lt_map INTO DATA(ls_map) WHERE type CA 'EAX'.
      mo_log->add( iv_row = iv_row iv_k1 = iv_k1 iv_k2 = iv_k2 iv_k3 = iv_k3
                   iv_ty  = ls_map-type iv_id = ls_map-id iv_no = ls_map-number
                   iv_st  = ls_map-bapistrucname iv_fl = ls_map-bapifldnm
                   iv_txt = ls_map-message ).
      rv_ok = abap_false.
    ENDLOOP.
    IF rv_ok = abap_false.
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
        mo_log->add( iv_row = iv_row iv_k1 = iv_k1 iv_k2 = iv_k2 iv_k3 = iv_k3
                     iv_ty = 'E' iv_txt = |Maintain failed: { lx2->get_text( ) }| ).
        rv_ok = abap_false.
        RETURN.
    ENDTRY.

    " BAPIRETM lines carry a nested message table; read it generically so a
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
          mo_log->add( iv_row = iv_row iv_k1 = iv_k1 iv_k2 = iv_k2 iv_k3 = iv_k3
                       iv_ty  = ls_r2-type iv_id = ls_r2-id iv_no = ls_r2-number
                       iv_fl  = ls_r2-field iv_txt = ls_r2-message ).
          rv_ok = abap_false.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF rv_ok = abap_false.
      ROLLBACK WORK.
      reset_bp( ).
      RETURN.
    ENDIF.

    IF p_test = abap_true.
      ROLLBACK WORK.
      reset_bp( ).
      mo_log->add( iv_row = iv_row iv_k1 = iv_k1 iv_k2 = iv_k2 iv_k3 = iv_k3
                   iv_ty = 'S' iv_txt = 'Test run OK - would post' ).
    ELSE.
      " BAPI_TRANSACTION_COMMIT, not a bare COMMIT WORK: the business partner
      " hangs its own end-of-LUW processing off it.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = abap_true.
      reset_bp( ).
      mo_log->add( iv_row = iv_row iv_k1 = iv_k1 iv_k2 = iv_k2 iv_k3 = iv_k3
                   iv_ty = 'S' iv_txt = 'Posted successfully' ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Handler interface and base
*----------------------------------------------------------------------*
INTERFACE lif_h.
  METHODS sheet     RETURNING VALUE(rv) TYPE string.
  METHODS first_row RETURNING VALUE(rv) TYPE i.
  METHODS run       IMPORTING it_row TYPE tt_row.
ENDINTERFACE.

CLASS lcl_base DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_h ABSTRACT METHODS sheet first_row run.
    METHODS constructor IMPORTING io_log TYPE REF TO lcl_log.
  PROTECTED SECTION.
    DATA: mo_log  TYPE REF TO lcl_log,
          mo_cfg  TYPE REF TO lcl_cfg,
          mo_cvis TYPE REF TO lcl_cvis.

    METHODS header
      IMPORTING iv_lifnr TYPE lifnr
                iv_task  TYPE cmd_ei_object_task
      CHANGING  cs_data  TYPE cvis_ei_extern.

    "! Gross merge - existing LFBK rows that are not in the file must be sent
    "! back or the API deletes them.
    METHODS merge_banks
      IMPORTING iv_lifnr TYPE lifnr
      CHANGING  ct_bank  TYPE cvis_ei_bankdetail_t.

    "! Gross merge for LFBW.
    METHODS merge_wtax
      IMPORTING iv_lifnr TYPE lifnr
                iv_bukrs TYPE bukrs
      CHANGING  ct_wtax  TYPE vmds_ei_wtax_type_t.

    "! Gross merge for WYT3. Account groups auto-create LF/RS/WL pointing at
    "! the vendor itself - those must survive.
    METHODS merge_funcs
      IMPORTING iv_lifnr TYPE lifnr
                iv_ekorg TYPE ekorg
      CHANGING  ct_func  TYPE vmds_ei_functions_t.
ENDCLASS.

CLASS lcl_base IMPLEMENTATION.

  METHOD constructor.
    mo_log  = io_log.
    mo_cfg  = lcl_cfg=>get( ).
    mo_cvis = NEW lcl_cvis( io_log ).
  ENDMETHOD.

  METHOD header.
    cs_data-vendor-header-object_instance-lifnr = iv_lifnr.
    cs_data-vendor-header-object_task           = iv_task.
    cs_data-partner-header-object_task          = iv_task.

    " The partner has to be identified in the message either way, or the API
    " answers "Specify at least one number for the business partner"
    " (message R11 123). A change names the partner by its GUID; a creation
    " has no number yet - it comes from the grouping's range - so it is
    " identified by a GUID generated here, which becomes the new partner's
    " PARTNER_GUID.
    DATA lv_guid TYPE bu_partner_guid.
    IF iv_task = gc_i.
      TRY.
          lv_guid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
        CATCH cx_uuid_error.
          CLEAR lv_guid.
      ENDTRY.
    ELSE.
      lv_guid = mo_cfg->vend_guid( iv_lifnr ).
    ENDIF.
    IF lv_guid IS NOT INITIAL.
      cs_data-partner-header-object_instance-bpartnerguid = lv_guid.
    ENDIF.
  ENDMETHOD.

  METHOD merge_banks.
    SELECT banks, bankl, bankn, bkont, bvtyp, koinh
      FROM lfbk WHERE lifnr = @iv_lifnr
      INTO TABLE @DATA(lt_db).

    LOOP AT lt_db INTO DATA(ls_db).
      IF line_exists( ct_bank[ data_key-banks = ls_db-banks
                               data_key-bankl = ls_db-bankl
                               data_key-bankn = ls_db-bankn ] ).
        CONTINUE.                                " file overrides this one
      ENDIF.
      DATA ls_keep TYPE cvis_ei_cvi_bankdetail.
      CLEAR ls_keep.
      ls_keep-task           = gc_u.
      ls_keep-data_key-banks = ls_db-banks.
      ls_keep-data_key-bankl = ls_db-bankl.
      ls_keep-data_key-bankn = ls_db-bankn.
      lcl_util=>set( EXPORTING iv_comp = 'KOINH' iv_value = CONV string( ls_db-koinh )
                     CHANGING  cs_data = ls_keep-data cs_datax = ls_keep-datax ).
      lcl_util=>set( EXPORTING iv_comp = 'BKONT' iv_value = CONV string( ls_db-bkont )
                     CHANGING  cs_data = ls_keep-data cs_datax = ls_keep-datax ).
      lcl_util=>set( EXPORTING iv_comp = 'BVTYP' iv_value = CONV string( ls_db-bvtyp )
                     CHANGING  cs_data = ls_keep-data cs_datax = ls_keep-datax ).
      APPEND ls_keep TO ct_bank.
    ENDLOOP.
  ENDMETHOD.

  METHOD merge_wtax.
    SELECT witht, wt_withcd, wt_subjct, qsrec, wt_wtstcd,
           wt_exnr, wt_exrt, wt_wtexrs, wt_exdf, wt_exdt
      FROM lfbw WHERE lifnr = @iv_lifnr AND bukrs = @iv_bukrs
      INTO TABLE @DATA(lt_db).

    LOOP AT lt_db INTO DATA(ls_db).
      IF line_exists( ct_wtax[ data_key-witht = ls_db-witht ] ).
        CONTINUE.
      ENDIF.
      DATA ls_keep TYPE vmds_ei_wtax_type.
      CLEAR ls_keep.
      ls_keep-task           = gc_u.
      ls_keep-data_key-witht = ls_db-witht.
      lcl_util=>set( EXPORTING iv_comp = 'WT_WITHCD' iv_value = CONV string( ls_db-wt_withcd )
                     CHANGING cs_data = ls_keep-data cs_datax = ls_keep-datax ).
      lcl_util=>set( EXPORTING iv_comp = 'WT_SUBJCT' iv_value = CONV string( ls_db-wt_subjct )
                     CHANGING cs_data = ls_keep-data cs_datax = ls_keep-datax ).
      lcl_util=>set( EXPORTING iv_comp = 'QSREC' iv_value = CONV string( ls_db-qsrec )
                     CHANGING cs_data = ls_keep-data cs_datax = ls_keep-datax ).
      lcl_util=>set( EXPORTING iv_comp = 'WT_WTSTCD' iv_value = CONV string( ls_db-wt_wtstcd )
                     CHANGING cs_data = ls_keep-data cs_datax = ls_keep-datax ).
      lcl_util=>set( EXPORTING iv_comp = 'WT_EXNR' iv_value = CONV string( ls_db-wt_exnr )
                     CHANGING cs_data = ls_keep-data cs_datax = ls_keep-datax ).
      APPEND ls_keep TO ct_wtax.
    ENDLOOP.
  ENDMETHOD.

  METHOD merge_funcs.
    SELECT parvw, parza, lifn2, defpa
      FROM wyt3 WHERE lifnr = @iv_lifnr AND ekorg = @iv_ekorg
      INTO TABLE @DATA(lt_db).

    LOOP AT lt_db INTO DATA(ls_db).
      IF line_exists( ct_func[ data_key-parvw = ls_db-parvw
                               data_key-parza = ls_db-parza ] ).
        CONTINUE.
      ENDIF.
      DATA ls_keep TYPE vmds_ei_functions.
      CLEAR ls_keep.
      ls_keep-task           = gc_u.
      ls_keep-data_key-parvw = ls_db-parvw.
      ls_keep-data_key-parza = ls_db-parza.
      lcl_util=>set( EXPORTING iv_comp = 'PARTNER' iv_value = CONV string( ls_db-lifn2 )
                     CHANGING cs_data = ls_keep-data cs_datax = ls_keep-datax ).
      lcl_util=>set( EXPORTING iv_comp = 'DEFPA' iv_value = CONV string( ls_db-defpa )
                     CHANGING cs_data = ls_keep-data cs_datax = ls_keep-datax ).
      APPEND ls_keep TO ct_func.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Tab 1 - Vendor creation for All CC  (66 columns, data from row 4)
*----------------------------------------------------------------------*
CLASS lcl_h_create DEFINITION INHERITING FROM lcl_base FINAL.
  PUBLIC SECTION.
    METHODS lif_h~sheet     REDEFINITION.
    METHODS lif_h~first_row REDEFINITION.
    METHODS lif_h~run       REDEFINITION.
  PRIVATE SECTION.
    METHODS fill_partner IMPORTING is_row TYPE ty_row iv_ktokk TYPE ktokk
                         CHANGING  cs_data TYPE cvis_ei_extern.
    METHODS fill_central IMPORTING is_row TYPE ty_row
                         CHANGING  cs_data TYPE cvis_ei_extern.
ENDCLASS.

CLASS lcl_h_create IMPLEMENTATION.

  METHOD lif_h~sheet.     rv = gc_sh_create. ENDMETHOD.
  METHOD lif_h~first_row. rv = 2. ENDMETHOD.

  METHOD fill_partner.
    " --- BP central ------------------------------------------------------
    " BP_CONTROL holds the control fields CATEGORY and GROUPING. It has no
    " DATAX counterpart - BUS_EI_BUPA_CENTRAL_DATA_XFLAG contains only
    " BP_CENTRALDATA, BP_PERSON and BP_ORGANIZATION - so these are simply set.
    cs_data-partner-central_data-common-data-bp_control-category = '2'.  " organisation
    cs_data-partner-central_data-common-data-bp_control-grouping = mo_cfg->bp_group( iv_ktokk ).

    DATA(lv_title) = mo_cfg->title_key( lcl_util=>cell( is_row = is_row iv_col = 6 ) ).

    lcl_util=>set( EXPORTING iv_comp = 'TITLE_KEY' iv_value = CONV string( lv_title )
                   CHANGING  cs_data  = cs_data-partner-central_data-common-data-bp_centraldata
                             cs_datax = cs_data-partner-central_data-common-datax-bp_centraldata ).
    lcl_util=>set( EXPORTING iv_comp = 'SEARCHTERM1' iv_value = lcl_util=>cell( is_row = is_row iv_col = 11 )
                   CHANGING  cs_data  = cs_data-partner-central_data-common-data-bp_centraldata
                             cs_datax = cs_data-partner-central_data-common-datax-bp_centraldata ).
    lcl_util=>set( EXPORTING iv_comp = 'SEARCHTERM2' iv_value = lcl_util=>cell( is_row = is_row iv_col = 12 )
                   CHANGING  cs_data  = cs_data-partner-central_data-common-data-bp_centraldata
                             cs_datax = cs_data-partner-central_data-common-datax-bp_centraldata ).

    DATA(lt_name) = VALUE string_table( ( |NAME1;7| ) ( |NAME2;8| ) ( |NAME3;9| ) ( |NAME4;10| ) ).
    LOOP AT lt_name INTO DATA(lv_p).
      SPLIT lv_p AT ';' INTO DATA(lv_f) DATA(lv_c).
      lcl_util=>set( EXPORTING iv_comp = lv_f iv_value = lcl_util=>cell( is_row = is_row iv_col = CONV i( lv_c ) )
                     CHANGING  cs_data  = cs_data-partner-central_data-common-data-bp_organization
                               cs_datax = cs_data-partner-central_data-common-datax-bp_organization ).
    ENDLOOP.

    " --- roles, from CVIC_VEND_TO_BP2 (every group maps to FLVN00+FLVN01) --
    "     DATA_KEY is an element of type BU_ROLE, not a structure.
    DATA ls_role TYPE bus_ei_bupa_roles.
    LOOP AT mo_cfg->bp_roles( iv_ktokk ) INTO DATA(lv_role).
      CLEAR ls_role.
      ls_role-task     = gc_i.
      ls_role-data_key = lv_role.
      APPEND ls_role TO cs_data-partner-central_data-role-roles.
    ENDLOOP.

    " --- address ---------------------------------------------------------
    DATA ls_adr TYPE bus_ei_bupa_address.
    CLEAR ls_adr.
    ls_adr-task = gc_i.

    DATA(lt_post) = VALUE string_table(
      ( |STR_SUPPL1;13| ) ( |STR_SUPPL2;14| ) ( |STREET;15| ) ( |STR_SUPPL3;16| )
      ( |DISTRICT;17| )   ( |POSTL_COD1;18| ) ( |CITY;19| )   ( |COUNTRY;20| )
      ( |REGION;21| )     ( |LANGU;22| ) ).
    LOOP AT lt_post INTO DATA(lv_pp).
      SPLIT lv_pp AT ';' INTO DATA(lv_pf) DATA(lv_pc).
      lcl_util=>set( EXPORTING iv_comp = lv_pf iv_value = lcl_util=>cell( is_row = is_row iv_col = CONV i( lv_pc ) )
                     CHANGING  cs_data  = ls_adr-data-postal-data
                               cs_datax = ls_adr-data-postal-datax ).
    ENDLOOP.

    " telephone / mobile - "number column;extension column;mobile flag"
    DATA: ls_tel TYPE bus_ei_bupa_telephone,
          ls_fax TYPE bus_ei_bupa_fax,
          ls_smt TYPE bus_ei_bupa_smtp,
          lt_tel TYPE string_table.

    lt_tel = VALUE #( ( `23;24;` ) ( `25;26;` ) ( `27;;3` ) ( `28;;3` ) ).

    LOOP AT lt_tel INTO DATA(lv_tp).
      SPLIT lv_tp AT ';' INTO DATA(lv_n) DATA(lv_x) DATA(lv_u).
      DATA(lv_num) = lcl_util=>cell( is_row = is_row iv_col = CONV i( lv_n ) ).
      IF lv_num IS INITIAL.
        CONTINUE.
      ENDIF.
      CLEAR ls_tel.
      ls_tel-contact-task           = gc_i.
      ls_tel-contact-data-telephone = lv_num.
      IF lv_x IS NOT INITIAL.
        ls_tel-contact-data-extension = lcl_util=>cell( is_row = is_row iv_col = CONV i( lv_x ) ).
      ENDIF.
      ls_tel-contact-data-r_3_user = lv_u.
      IF lv_n = '23'.
        ls_tel-contact-data-std_no = abap_true.
      ENDIF.
      APPEND ls_tel TO ls_adr-data-communication-phone-phone.
    ENDLOOP.

    DATA(lv_fax) = lcl_util=>cell( is_row = is_row iv_col = 29 ).
    IF lv_fax IS NOT INITIAL.
      CLEAR ls_fax.
      ls_fax-contact-task        = gc_i.
      ls_fax-contact-data-fax    = lv_fax.
      ls_fax-contact-data-std_no = abap_true.
      APPEND ls_fax TO ls_adr-data-communication-fax-fax.
    ENDIF.

    DO 2 TIMES.
      DATA lv_mc TYPE i.
      lv_mc = COND #( WHEN sy-index = 1 THEN 30 ELSE 31 ).
      DATA(lv_mail) = lcl_util=>cell( is_row = is_row iv_col = lv_mc ).
      IF lv_mail IS INITIAL.
        CONTINUE.
      ENDIF.
      CLEAR ls_smt.
      ls_smt-contact-task        = gc_i.
      ls_smt-contact-data-e_mail = lv_mail.
      IF sy-index = 1.
        ls_smt-contact-data-std_no = abap_true.
      ENDIF.
      APPEND ls_smt TO ls_adr-data-communication-smtp-smtp.
    ENDDO.

    APPEND ls_adr TO cs_data-partner-central_data-address-addresses.
  ENDMETHOD.

  METHOD fill_central.
    " "FIELD;column;length" - a length means the field's domain carries the
    " ALPHA exit, so a numeric value is zero-padded to that length before it
    " goes to the API. The LENGTH matters, not just the fact of the exit:
    " KUNNR is CHAR 10 and VBUND is CHAR 6, so they pad differently.
    "   KUNNR  domain KUNNR  - ALPHA, 10
    "   VBUND  domain RCOMP  - ALPHA, 6
    " KONZS, STCD*, BRSCH, J_1I* and VEN_CLASS have no conversion exit and are
    " passed through unchanged.
    DATA(lt_map) = VALUE string_table(
      ( |KTOKK;5;|  ) ( |KUNNR;32;10| ) ( |VBUND;33;6| ) ( |KONZS;34;| )
      ( |STCD3;35;| ) ( |STCD5;36;|  ) ( |STCEG;37;|  ) ( |J_1KFTBUS;38;| )
      ( |STENR;39;| ) ( |BRSCH;40;|  )
      " CIN - LFA1 fields, confirmed present in VMDS_EI_VMD_CENTRAL
      ( |VEN_CLASS;55;| ) ( |J_1ISSIST;56;| ) ( |J_1IPANNO;57;| ) ).

    LOOP AT lt_map INTO DATA(lv_p).
      SPLIT lv_p AT ';' INTO DATA(lv_f) DATA(lv_c) DATA(lv_a).
      DATA(lv_v) = lcl_util=>cell( is_row = is_row iv_col = CONV i( lv_c ) ).
      IF lv_a IS NOT INITIAL.
        lv_v = lcl_util=>alpha( iv_in = lv_v iv_len = CONV i( lv_a ) ).
      ENDIF.
      lcl_util=>set( EXPORTING iv_comp = lv_f iv_value = lv_v
                     CHANGING  cs_data  = cs_data-vendor-central_data-central-data
                               cs_datax = cs_data-vendor-central_data-central-datax ).
    ENDLOOP.
  ENDMETHOD.

  METHOD lif_h~run.
    LOOP AT it_row INTO DATA(ls_row).
      IF lcl_util=>skip_row( ls_row ) = abap_true.
        CONTINUE.
      ENDIF.

      DATA(lv_lifnr) = lcl_util=>lifnr( lcl_util=>cell( is_row = ls_row iv_col = 2 ) ).
      DATA(lv_bukrs) = CONV bukrs( lcl_util=>cell( is_row = ls_row iv_col = 3 ) ).
      DATA(lv_ekorg) = CONV ekorg( lcl_util=>cell( is_row = ls_row iv_col = 4 ) ).
      DATA(lv_ktokk) = CONV ktokk( to_upper( lcl_util=>cell( is_row = ls_row iv_col = 5 ) ) ).

      IF lv_ktokk IS INITIAL AND lv_bukrs IS INITIAL.
        CONTINUE.
      ENDIF.

      " ---- validation before anything is built ----
      DATA lv_bad TYPE abap_bool.
      CLEAR lv_bad.

      " IS INITIAL takes a data object, not an expression, so the derived
      " BP grouping is read into a variable first.
      DATA lv_grp TYPE bu_group.
      CLEAR lv_grp.
      IF lv_ktokk IS NOT INITIAL.
        lv_grp = mo_cfg->bp_group( lv_ktokk ).
      ENDIF.

      IF mo_cfg->ok_ktokk( lv_ktokk ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_ty = 'E'
                     iv_txt = |Account group { lv_ktokk } does not exist (column 5, KTOKK)| ).
        lv_bad = abap_true.
      ELSEIF lv_grp IS INITIAL.
        mo_log->add( iv_row = ls_row-row iv_ty = 'E'
                     iv_txt = |Account group { lv_ktokk } has no BP grouping in CVIC_VEND_TO_BP1| ).
        lv_bad = abap_true.
      ENDIF.
      IF mo_cfg->ok_bukrs( lv_bukrs ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_k2 = lv_bukrs iv_ty = 'E'
                     iv_txt = |Company code { lv_bukrs } does not exist (column 3, BUKRS)| ).
        lv_bad = abap_true.
      ENDIF.
      IF lv_ekorg IS NOT INITIAL AND mo_cfg->ok_ekorg( lv_ekorg ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_k3 = lv_ekorg iv_ty = 'E'
                     iv_txt = |Purchasing organisation { lv_ekorg } does not exist (column 4, EKORG)| ).
        lv_bad = abap_true.
      ENDIF.

      DATA(lv_akont) = lcl_util=>gl( lcl_util=>cell( is_row = ls_row iv_col = 47 ) ).
      IF lv_akont IS INITIAL.
        mo_log->add( iv_row = ls_row-row iv_ty = 'E'
                     iv_txt = 'Reconciliation account is mandatory (column 47, AKONT)' ).
        lv_bad = abap_true.
      ELSEIF mo_cfg->ok_akont( iv_bukrs = lv_bukrs iv_saknr = lv_akont ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_k2 = lv_bukrs iv_ty = 'E'
                     iv_txt = |{ lv_akont } is not a vendor reconciliation account in { lv_bukrs } (column 47)| ).
        lv_bad = abap_true.
      ENDIF.

      DATA(lv_banks) = to_upper( lcl_util=>cell( is_row = ls_row iv_col = 41 ) ).
      DATA(lv_bankl) = lcl_util=>cell( is_row = ls_row iv_col = 42 ).
      DATA(lv_bankn) = lcl_util=>cell( is_row = ls_row iv_col = 43 ).
      IF lv_bankl IS NOT INITIAL
         AND mo_cfg->ok_bankl( iv_banks = lv_banks iv_bankl = lv_bankl ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_ty = 'E'
                     iv_txt = |Bank key { lv_banks }/{ lv_bankl } does not exist - create it first (tab "BANK Key creation")| ).
        lv_bad = abap_true.
      ENDIF.

      IF lv_bad = abap_true.
        IF p_stop = abap_true.
          EXIT.
        ENDIF.
        CONTINUE.
      ENDIF.

      " ---- build ----
      DATA ls_data TYPE cvis_ei_extern.
      CLEAR ls_data.
      header( EXPORTING iv_lifnr = lv_lifnr
                        iv_task  = COND #( WHEN lv_lifnr IS INITIAL THEN gc_i ELSE gc_u )
              CHANGING  cs_data  = ls_data ).

      fill_partner( EXPORTING is_row = ls_row iv_ktokk = lv_ktokk CHANGING cs_data = ls_data ).
      fill_central( EXPORTING is_row = ls_row CHANGING cs_data = ls_data ).

      " bank details
      IF lv_bankl IS NOT INITIAL AND lv_bankn IS NOT INITIAL.
        DATA ls_bk TYPE cvis_ei_cvi_bankdetail.
        CLEAR ls_bk.
        ls_bk-task           = gc_i.
        ls_bk-data_key-banks = lv_banks.
        ls_bk-data_key-bankl = lv_bankl.
        ls_bk-data_key-bankn = lv_bankn.
        lcl_util=>set( EXPORTING iv_comp = 'KOINH' iv_value = lcl_util=>cell( is_row = ls_row iv_col = 44 )
                       CHANGING cs_data = ls_bk-data cs_datax = ls_bk-datax ).
        lcl_util=>set( EXPORTING iv_comp = 'BKONT' iv_value = lcl_util=>cell( is_row = ls_row iv_col = 45 )
                       CHANGING cs_data = ls_bk-data cs_datax = ls_bk-datax ).
        APPEND ls_bk TO ls_data-vendor-central_data-bankdetail-bankdetails.
      ENDIF.

      " company code data
      DATA ls_cc TYPE vmds_ei_company.
      CLEAR ls_cc.
      ls_cc-task           = COND #( WHEN lv_lifnr IS NOT INITIAL
                                      AND mo_cfg->has_lfb1( iv_lifnr = CONV lifnr( lv_lifnr ) iv_bukrs = CONV bukrs( lv_bukrs ) ) = abap_true
                                     THEN gc_u ELSE gc_i ).
      ls_cc-data_key-bukrs = lv_bukrs.
      " "FIELD;column;length" - same convention as FILL_CENTRAL: a length
      " means the domain carries the ALPHA exit and the value is padded to
      " that length. Confirmed from DD01L: SAKNR (AKONT) and FDGRP (FDGRV)
      " carry it, both CHAR 10; ZTERM, REPRF, ZWELS, ZAHLS, HBKID, QLAND
      " and CHAR10 (ALTKN) do not.
      DATA(lt_cc) = VALUE string_table(
        ( |AKONT;47;10| ) ( |FDGRV;48;10| ) ( |ALTKN;49;| ) ( |ZTERM;50;| )
        ( |REPRF;51;|   ) ( |ZWELS;52;|   ) ( |ZAHLS;53;| ) ( |HBKID;54;| )
        ( |QLAND;58;|   ) ).
      LOOP AT lt_cc INTO DATA(lv_cp).
        SPLIT lv_cp AT ';' INTO DATA(lv_cf) DATA(lv_cn) DATA(lv_ca).
        DATA(lv_cv) = lcl_util=>cell( is_row = ls_row iv_col = CONV i( lv_cn ) ).
        IF lv_cf = 'AKONT'.
          lv_cv = lv_akont.                      " already ALPHA-converted
        ELSEIF lv_ca IS NOT INITIAL.
          lv_cv = lcl_util=>alpha( iv_in = lv_cv iv_len = CONV i( lv_ca ) ).
        ENDIF.
        lcl_util=>set( EXPORTING iv_comp = lv_cf iv_value = lv_cv
                       CHANGING cs_data = ls_cc-data cs_datax = ls_cc-datax ).
      ENDLOOP.

      " withholding tax on the creation tab (single type/code pair)
      DATA(lv_witht) = to_upper( lcl_util=>cell( is_row = ls_row iv_col = 59 ) ).
      IF lv_witht IS NOT INITIAL.
        DATA ls_wt TYPE vmds_ei_wtax_type.
        CLEAR ls_wt.
        ls_wt-task           = gc_i.
        ls_wt-data_key-witht = lv_witht.
        lcl_util=>set( EXPORTING iv_comp = 'WT_WITHCD' iv_value = to_upper( lcl_util=>cell( is_row = ls_row iv_col = 60 ) )
                       CHANGING cs_data = ls_wt-data cs_datax = ls_wt-datax ).
        lcl_util=>set( EXPORTING iv_comp = 'WT_SUBJCT' iv_value = 'X'
                       CHANGING cs_data = ls_wt-data cs_datax = ls_wt-datax ).
        APPEND ls_wt TO ls_cc-wtax_type-wtax_type.
      ENDIF.
      APPEND ls_cc TO ls_data-vendor-company_data-company.

      " purchasing data
      IF lv_ekorg IS NOT INITIAL.
        DATA ls_po TYPE vmds_ei_purchasing.
        CLEAR ls_po.
        ls_po-task           = COND #( WHEN lv_lifnr IS NOT INITIAL
                                        AND mo_cfg->has_lfm1( iv_lifnr = CONV lifnr( lv_lifnr ) iv_ekorg = CONV ekorg( lv_ekorg ) ) = abap_true
                                       THEN gc_u ELSE gc_i ).
        ls_po-data_key-ekorg = lv_ekorg.
        DATA(lt_po) = VALUE string_table(
          ( |WAERS;61| ) ( |ZTERM;62| ) ( |KALSK;63| )
          ( |WEBRE;64| ) ( |INCO1;65| ) ( |INCO2;66| ) ).
        LOOP AT lt_po INTO DATA(lv_op).
          SPLIT lv_op AT ';' INTO DATA(lv_of) DATA(lv_on).
          lcl_util=>set( EXPORTING iv_comp = lv_of iv_value = lcl_util=>cell( is_row = ls_row iv_col = CONV i( lv_on ) )
                         CHANGING cs_data = ls_po-data cs_datax = ls_po-datax ).
        ENDLOOP.
        APPEND ls_po TO ls_data-vendor-purchasing_data-purchasing.
      ENDIF.

      mo_cvis->post( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_k3 = lv_ekorg
                     is_data = ls_data ).

      IF p_stop = abap_true AND mo_log->has_error( ls_row-row ) = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Tab 2 - TDS upload  (65 columns, six repeating blocks, data from row 3)
*----------------------------------------------------------------------*
CLASS lcl_h_tds DEFINITION INHERITING FROM lcl_base FINAL.
  PUBLIC SECTION.
    METHODS lif_h~sheet     REDEFINITION.
    METHODS lif_h~first_row REDEFINITION.
    METHODS lif_h~run       REDEFINITION.
ENDCLASS.

CLASS lcl_h_tds IMPLEMENTATION.

  METHOD lif_h~sheet.     rv = gc_sh_tds. ENDMETHOD.
  METHOD lif_h~first_row. rv = 2. ENDMETHOD.

  METHOD lif_h~run.
    " Column bases for the six blocks _01.._06 (offset = block - 1)
    CONSTANTS: lc_witht  TYPE i VALUE 6,   lc_withcd TYPE i VALUE 12,
               lc_subjct TYPE i VALUE 18,  lc_qsrec  TYPE i VALUE 24,
               lc_wtstcd TYPE i VALUE 30,  lc_exnr   TYPE i VALUE 36,
               lc_exrt   TYPE i VALUE 42,  lc_wtexrs TYPE i VALUE 48,
               lc_exdf   TYPE i VALUE 54,  lc_exdt   TYPE i VALUE 60.

    LOOP AT it_row INTO DATA(ls_row).
      IF lcl_util=>skip_row( ls_row ) = abap_true.
        CONTINUE.
      ENDIF.

      DATA(lv_lifnr) = lcl_util=>lifnr( lcl_util=>cell( is_row = ls_row iv_col = 2 ) ).
      DATA(lv_bukrs) = CONV bukrs( lcl_util=>cell( is_row = ls_row iv_col = 3 ) ).
      DATA(lv_qland) = to_upper( lcl_util=>cell( is_row = ls_row iv_col = 5 ) ).
      " column 4 = D0610, an XK02 screen flag - deliberately ignored

      IF lv_lifnr IS INITIAL.
        CONTINUE.
      ENDIF.
      IF mo_cfg->vend_exists( lv_lifnr ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_ty = 'E'
                     iv_txt = |Vendor { lv_lifnr } does not exist| ).
        CONTINUE.
      ENDIF.
      IF mo_cfg->ok_bukrs( lv_bukrs ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_ty = 'E'
                     iv_txt = |Company code { lv_bukrs } does not exist| ).
        CONTINUE.
      ENDIF.

      DATA(lv_land) = COND land1( WHEN lv_qland IS NOT INITIAL THEN lv_qland
                                  ELSE mo_cfg->vend_land1( lv_lifnr ) ).

      DATA lt_wt TYPE vmds_ei_wtax_type_t.
      CLEAR lt_wt.
      DATA lv_bad TYPE abap_bool.
      CLEAR lv_bad.

      DO 6 TIMES.
        DATA(lv_o)  = sy-index - 1.
        DATA(lv_wt) = to_upper( lcl_util=>cell( is_row = ls_row iv_col = lc_witht + lv_o ) ).
        IF lv_wt IS INITIAL.
          CONTINUE.
        ENDIF.

        DATA(lv_cd) = to_upper( lcl_util=>cell( is_row = ls_row iv_col = lc_withcd + lv_o ) ).

        IF mo_cfg->ok_witht( iv_land1 = lv_land iv_witht = lv_wt ) = abap_false.
          mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_ty = 'E'
                       iv_txt = |Block { sy-index }: tax type { lv_wt } is not defined for country { lv_land }| ).
          lv_bad = abap_true.
          CONTINUE.
        ENDIF.
        IF lv_cd IS NOT INITIAL
           AND mo_cfg->ok_wcode( iv_land1 = lv_land iv_witht = lv_wt iv_wcd = lv_cd ) = abap_false.
          mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_ty = 'E'
                       iv_txt = |Block { sy-index }: tax code { lv_cd } is not valid for type { lv_wt }| ).
          lv_bad = abap_true.
          CONTINUE.
        ENDIF.

        DATA ls_wt TYPE vmds_ei_wtax_type.
        CLEAR ls_wt.
        " CONV at the call: the cell was read as a STRING, and a parameter
        " typed with a dictionary type does not take one as it stands.
        ls_wt-task           = COND #( WHEN mo_cfg->has_lfbw( iv_lifnr = lv_lifnr
                                                              iv_bukrs = lv_bukrs
                                                              iv_witht = CONV witht( lv_wt ) ) = abap_true
                                       THEN gc_u ELSE gc_i ).
        ls_wt-data_key-witht = lv_wt.

        lcl_util=>set( EXPORTING iv_comp = 'WT_WITHCD' iv_value = lv_cd
                       CHANGING cs_data = ls_wt-data cs_datax = ls_wt-datax ).
        lcl_util=>set( EXPORTING iv_comp = 'WT_SUBJCT'
                       iv_value = to_upper( lcl_util=>cell( is_row = ls_row iv_col = lc_subjct + lv_o ) )
                       CHANGING cs_data = ls_wt-data cs_datax = ls_wt-datax ).
        lcl_util=>set( EXPORTING iv_comp = 'QSREC'
                       iv_value = to_upper( lcl_util=>cell( is_row = ls_row iv_col = lc_qsrec + lv_o ) )
                       CHANGING cs_data = ls_wt-data cs_datax = ls_wt-datax ).
        lcl_util=>set( EXPORTING iv_comp = 'WT_WTSTCD'
                       iv_value = lcl_util=>cell( is_row = ls_row iv_col = lc_wtstcd + lv_o )
                       CHANGING cs_data = ls_wt-data cs_datax = ls_wt-datax ).
        lcl_util=>set( EXPORTING iv_comp = 'WT_EXNR'
                       iv_value = lcl_util=>cell( is_row = ls_row iv_col = lc_exnr + lv_o )
                       CHANGING cs_data = ls_wt-data cs_datax = ls_wt-datax ).
        lcl_util=>set( EXPORTING iv_comp = 'WT_EXRT'
                       iv_value = lcl_util=>cell( is_row = ls_row iv_col = lc_exrt + lv_o )
                       CHANGING cs_data = ls_wt-data cs_datax = ls_wt-datax ).
        lcl_util=>set( EXPORTING iv_comp = 'WT_WTEXRS'
                       iv_value = to_upper( lcl_util=>cell( is_row = ls_row iv_col = lc_wtexrs + lv_o ) )
                       CHANGING cs_data = ls_wt-data cs_datax = ls_wt-datax ).

        DATA(lv_df) = lcl_util=>to_date( lcl_util=>cell( is_row = ls_row iv_col = lc_exdf + lv_o ) ).
        DATA(lv_dt) = lcl_util=>to_date( lcl_util=>cell( is_row = ls_row iv_col = lc_exdt + lv_o ) ).
        lcl_util=>set( EXPORTING iv_comp = 'WT_EXDF' iv_value = CONV string( lv_df )
                       CHANGING cs_data = ls_wt-data cs_datax = ls_wt-datax ).
        lcl_util=>set( EXPORTING iv_comp = 'WT_EXDT' iv_value = CONV string( lv_dt )
                       CHANGING cs_data = ls_wt-data cs_datax = ls_wt-datax ).

        APPEND ls_wt TO lt_wt.
      ENDDO.

      IF lv_bad = abap_true.
        IF p_stop = abap_true.
          EXIT.
        ENDIF.
        CONTINUE.
      ENDIF.
      IF lt_wt IS INITIAL.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_ty = 'W'
                     iv_txt = 'No withholding tax type supplied - row skipped' ).
        CONTINUE.
      ENDIF.

      " gross segment: keep the types already on the vendor
      merge_wtax( EXPORTING iv_lifnr = lv_lifnr iv_bukrs = lv_bukrs CHANGING ct_wtax = lt_wt ).

      DATA ls_data TYPE cvis_ei_extern.
      CLEAR ls_data.
      header( EXPORTING iv_lifnr = lv_lifnr iv_task = gc_u CHANGING cs_data = ls_data ).

      DATA ls_cc TYPE vmds_ei_company.
      CLEAR ls_cc.
      ls_cc-task           = COND #( WHEN mo_cfg->has_lfb1( iv_lifnr = CONV lifnr( lv_lifnr ) iv_bukrs = CONV bukrs( lv_bukrs ) ) = abap_true
                                     THEN gc_u ELSE gc_i ).
      ls_cc-data_key-bukrs = lv_bukrs.
      lcl_util=>set( EXPORTING iv_comp = 'QLAND' iv_value = lv_qland
                     CHANGING cs_data = ls_cc-data cs_datax = ls_cc-datax ).
      ls_cc-wtax_type-wtax_type = lt_wt.
      APPEND ls_cc TO ls_data-vendor-company_data-company.

      mo_cvis->post( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs is_data = ls_data ).

      IF p_stop = abap_true AND mo_log->has_error( ls_row-row ) = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Tab 3 - TAN details -> FIWTIN_TAN_EXEM via J_1ITAN_EXEM_SAVE
*
*   The FM is an UPDATE MODULE: it returns no messages, so everything is
*   validated here and the call is simply skipped in a test run.
*   PAN_NO is part of the primary key but is NOT in the template - it is
*   read from LFA1-J_1IPANNO.
*----------------------------------------------------------------------*
CLASS lcl_h_tan DEFINITION INHERITING FROM lcl_base FINAL.
  PUBLIC SECTION.
    METHODS lif_h~sheet     REDEFINITION.
    METHODS lif_h~first_row REDEFINITION.
    METHODS lif_h~run       REDEFINITION.
ENDCLASS.

CLASS lcl_h_tan IMPLEMENTATION.

  METHOD lif_h~sheet.     rv = gc_sh_tan. ENDMETHOD.
  METHOD lif_h~first_row. rv = 2. ENDMETHOD.

  METHOD lif_h~run.
    DATA lt_exem TYPE STANDARD TABLE OF fiwtin_tan_exem.

    LOOP AT it_row INTO DATA(ls_row).
      IF lcl_util=>skip_row( ls_row ) = abap_true.
        CONTINUE.
      ENDIF.

      DATA(lv_lifnr) = lcl_util=>lifnr( lcl_util=>cell( is_row = ls_row iv_col = 1 ) ).
      DATA(lv_bukrs) = CONV bukrs( lcl_util=>cell( is_row = ls_row iv_col = 2 ) ).
      IF lv_lifnr IS INITIAL.
        CONTINUE.
      ENDIF.

      IF mo_cfg->vend_exists( lv_lifnr ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_ty = 'E'
                     iv_txt = |Vendor { lv_lifnr } does not exist| ).
        CONTINUE.
      ENDIF.
      IF mo_cfg->ok_bukrs( lv_bukrs ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_ty = 'E'
                     iv_txt = |Company code { lv_bukrs } does not exist| ).
        CONTINUE.
      ENDIF.

      DATA(lv_pan) = mo_cfg->vend_pan( lv_lifnr ).
      IF lv_pan IS INITIAL.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_ty = 'E'
                     iv_txt = 'PAN_NO belongs to the key but LFA1-J_1IPANNO is empty - maintain PAN first' ).
        CONTINUE.
      ENDIF.

      DATA(lv_land) = mo_cfg->vend_land1( lv_lifnr ).

      DO 2 TIMES.
        DATA(lv_o) = sy-index - 1.

        DATA(lv_wt) = to_upper( lcl_util=>cell( is_row = ls_row iv_col = 14 + lv_o ) ).
        IF lv_wt IS INITIAL.
          CONTINUE.
        ENDIF.
        DATA(lv_cd) = to_upper( lcl_util=>cell( is_row = ls_row iv_col = 16 + lv_o ) ).
        DATA(lv_df) = lcl_util=>to_date( lcl_util=>cell( is_row = ls_row iv_col = 10 + lv_o ) ).

        IF lv_df IS INITIAL.
          mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_ty = 'E'
                       iv_txt = |Block { sy-index }: "valid from" is a key field and must be filled| ).
          CONTINUE.
        ENDIF.
        IF mo_cfg->ok_witht( iv_land1 = lv_land iv_witht = lv_wt ) = abap_false.
          mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_ty = 'E'
                       iv_txt = |Block { sy-index }: tax type { lv_wt } is not defined for country { lv_land }| ).
          CONTINUE.
        ENDIF.

        DATA ls_ex TYPE fiwtin_tan_exem.
        CLEAR ls_ex.
        ls_ex-bukrs            = lv_bukrs.
        ls_ex-koart            = 'K'.
        ls_ex-accno            = lv_lifnr.
        ls_ex-fiwtin_tanex_sub = abap_true.
        ls_ex-seccode          = lcl_util=>cell( is_row = ls_row iv_col = 4 + lv_o ).
        ls_ex-witht            = lv_wt.
        ls_ex-wt_withcd        = lv_cd.
        ls_ex-wt_exdf          = lv_df.
        ls_ex-pan_no           = lv_pan.
        ls_ex-wt_exdt          = lcl_util=>to_date( lcl_util=>cell( is_row = ls_row iv_col = 12 + lv_o ) ).
        ls_ex-wt_exnr          = lcl_util=>cell( is_row = ls_row iv_col = 6 + lv_o ).
        ls_ex-wt_exrt          = lcl_util=>to_dec( lcl_util=>cell( is_row = ls_row iv_col = 8 + lv_o ) ).
        ls_ex-fiwtin_exem_thr  = lcl_util=>to_dec( lcl_util=>cell( is_row = ls_row iv_col = 18 + lv_o ) ).
        ls_ex-waers            = to_upper( lcl_util=>cell( is_row = ls_row iv_col = 20 + lv_o ) ).

        APPEND ls_ex TO lt_exem.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_ty = 'S'
                     iv_txt = |Block { sy-index } ({ lv_wt }/{ lv_cd }, from { lv_df DATE = USER }) | &&
                              |{ COND string( WHEN p_test = abap_true THEN 'validated - would be saved'
                                              ELSE 'validated' ) }| ).
      ENDDO.

      IF p_stop = abap_true AND mo_log->has_error( ls_row-row ) = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lt_exem IS INITIAL.
      RETURN.
    ENDIF.

    IF p_test = abap_true.
      mo_log->add( iv_row = 0 iv_ty = 'S'
                   iv_txt = |Test run - { lines( lt_exem ) } TAN exemption rows validated, nothing saved| ).
    ELSE.
      CALL FUNCTION 'J_1ITAN_EXEM_SAVE' IN UPDATE TASK
        TABLES it_tan_exem = lt_exem.
      COMMIT WORK AND WAIT.
      mo_log->add( iv_row = 0 iv_ty = 'S'
                   iv_txt = |{ lines( lt_exem ) } TAN exemption rows sent to J_1ITAN_EXEM_SAVE| ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Tab 4 - BANK Key creation  (BAPI_BANK_CREATE / _CHANGE, data from row 7)
*----------------------------------------------------------------------*
CLASS lcl_h_bkey DEFINITION INHERITING FROM lcl_base FINAL.
  PUBLIC SECTION.
    METHODS lif_h~sheet     REDEFINITION.
    METHODS lif_h~first_row REDEFINITION.
    METHODS lif_h~run       REDEFINITION.
ENDCLASS.

CLASS lcl_h_bkey IMPLEMENTATION.

  METHOD lif_h~sheet.     rv = gc_sh_bkey. ENDMETHOD.
  METHOD lif_h~first_row. rv = 2. ENDMETHOD.

  METHOD lif_h~run.
    LOOP AT it_row INTO DATA(ls_row).
      IF lcl_util=>skip_row( ls_row ) = abap_true.
        CONTINUE.
      ENDIF.

      DATA(lv_banks) = CONV banks( to_upper( lcl_util=>cell( is_row = ls_row iv_col = 2 ) ) ).
      DATA(lv_bankl) = CONV bankk( lcl_util=>cell( is_row = ls_row iv_col = 3 ) ).
      DATA(lv_key)   = |{ lv_banks }/{ lv_bankl }|.

      IF lv_banks IS INITIAL OR lv_bankl IS INITIAL.
        CONTINUE.
      ENDIF.
      IF mo_cfg->ok_land1( lv_banks ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_key iv_ty = 'E'
                     iv_txt = |Bank country { lv_banks } does not exist (column 2, BANKS)| ).
        CONTINUE.
      ENDIF.

      DATA ls_adr TYPE bapi1011_address.
      CLEAR ls_adr.
      ls_adr-bank_name   = lcl_util=>cell( is_row = ls_row iv_col = 4 ).
      ls_adr-region      = lcl_util=>cell( is_row = ls_row iv_col = 5 ).
      ls_adr-street      = lcl_util=>cell( is_row = ls_row iv_col = 6 ).
      ls_adr-city        = lcl_util=>cell( is_row = ls_row iv_col = 7 ).
      ls_adr-bank_branch = lcl_util=>cell( is_row = ls_row iv_col = 8 ).
      ls_adr-swift_code  = lcl_util=>cell( is_row = ls_row iv_col = 9 ).

      IF ls_adr-bank_name IS INITIAL.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_key iv_ty = 'E'
                     iv_txt = 'Bank name is mandatory (column 4, BANKA)' ).
        CONTINUE.
      ENDIF.

      DATA(lv_exists) = mo_cfg->ok_bankl( iv_banks = lv_banks iv_bankl = lv_bankl ).
      DATA ls_ret TYPE bapiret2.
      CLEAR ls_ret.

      IF lv_exists = abap_true.
        " BAPI_BANK_CHANGE uses BANKCOUNTRY / BANKKEY
        CALL FUNCTION 'BAPI_BANK_CHANGE'
          EXPORTING bankcountry         = lv_banks
                    bankkey             = lv_bankl
                    bank_address        = ls_adr
                    bank_addressx       = VALUE bapi1011_addressx(
                                            bank_name   = abap_true
                                            region      = abap_true
                                            street      = abap_true
                                            city        = abap_true
                                            bank_branch = abap_true
                                            swift_code  = abap_true )
                    i_check_before_save = p_test
          IMPORTING return              = ls_ret.
      ELSE.
        " BAPI_BANK_CREATE uses BANK_CTRY / BANK_KEY - different names
        CALL FUNCTION 'BAPI_BANK_CREATE'
          EXPORTING bank_ctry           = lv_banks
                    bank_key            = lv_bankl
                    bank_address        = ls_adr
                    i_check_before_save = p_test
                    i_no_overwrite      = abap_true
          IMPORTING return              = ls_ret.
      ENDIF.

      IF ls_ret-type CA 'EAX'.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        mo_log->add_ret( iv_row = ls_row-row iv_k1 = lv_key is_ret = ls_ret ).
      ELSEIF p_test = abap_true.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_key iv_ty = 'S'
                     iv_txt = COND string( WHEN lv_exists = abap_true THEN 'Test run OK - would change the bank'
                                                                 ELSE 'Test run OK - would create the bank' ) ).
      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = abap_true.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_key iv_ty = 'S'
                     iv_txt = COND string( WHEN lv_exists = abap_true THEN 'Bank changed' ELSE 'Bank created' ) ).
      ENDIF.

      IF p_stop = abap_true AND mo_log->has_error( ls_row-row ) = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Tab 5 - Bank details update  (8 columns, data from row 7)
*   Rows are grouped per vendor so that all of a vendor's accounts go in
*   one call, then merged with what is already on the vendor.
*----------------------------------------------------------------------*
CLASS lcl_h_bank DEFINITION INHERITING FROM lcl_base FINAL.
  PUBLIC SECTION.
    METHODS lif_h~sheet     REDEFINITION.
    METHODS lif_h~first_row REDEFINITION.
    METHODS lif_h~run       REDEFINITION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_in,
             row   TYPE i,
             lifnr TYPE lifnr,
             bukrs TYPE bukrs,
             banks TYPE banks,
             bankl TYPE bankk,
             bankn TYPE bankn,
             koinh TYPE koinh_fi,
             iban  TYPE iban,
           END OF ty_in,
           tt_in TYPE STANDARD TABLE OF ty_in WITH EMPTY KEY.
    METHODS flush IMPORTING iv_lifnr TYPE lifnr it_grp TYPE tt_in.
ENDCLASS.

CLASS lcl_h_bank IMPLEMENTATION.

  METHOD lif_h~sheet.     rv = gc_sh_bank. ENDMETHOD.
  METHOD lif_h~first_row. rv = 2. ENDMETHOD.

  METHOD flush.
    DATA: lv_row TYPE i,
          lv_cc  TYPE bukrs.
    lv_row = VALUE #( it_grp[ 1 ]-row   OPTIONAL ).
    lv_cc  = VALUE #( it_grp[ 1 ]-bukrs OPTIONAL ).

    IF mo_cfg->vend_exists( iv_lifnr ) = abap_false.
      mo_log->add( iv_row = lv_row iv_k1 = iv_lifnr iv_ty = 'E'
                   iv_txt = |Vendor { iv_lifnr } does not exist| ).
      RETURN.
    ENDIF.

    DATA lt_bank TYPE cvis_ei_bankdetail_t.
    CLEAR lt_bank.
    DATA lv_bad TYPE abap_bool.
    CLEAR lv_bad.

    LOOP AT it_grp INTO DATA(ls_in).
      IF ls_in-bankl IS INITIAL OR ls_in-bankn IS INITIAL.
        mo_log->add( iv_row = ls_in-row iv_k1 = iv_lifnr iv_ty = 'E'
                     iv_txt = 'Bank key and bank account number are both mandatory' ).
        lv_bad = abap_true.
        CONTINUE.
      ENDIF.
      IF mo_cfg->ok_bankl( iv_banks = ls_in-banks iv_bankl = ls_in-bankl ) = abap_false.
        mo_log->add( iv_row = ls_in-row iv_k1 = iv_lifnr iv_ty = 'E'
                     iv_txt = |Bank key { ls_in-banks }/{ ls_in-bankl } does not exist - create it first| ).
        lv_bad = abap_true.
        CONTINUE.
      ENDIF.

      DATA ls_bk TYPE cvis_ei_cvi_bankdetail.
      CLEAR ls_bk.
      ls_bk-task           = COND #( WHEN mo_cfg->has_lfbk( iv_lifnr = CONV lifnr( iv_lifnr )
                                                            iv_banks = CONV banks( ls_in-banks )
                                                            iv_bankl = CONV bankk( ls_in-bankl )
                                                            iv_bankn = CONV bankn( ls_in-bankn ) ) = abap_true
                                     THEN gc_u ELSE gc_i ).
      ls_bk-data_key-banks = ls_in-banks.
      ls_bk-data_key-bankl = ls_in-bankl.
      ls_bk-data_key-bankn = ls_in-bankn.
      lcl_util=>set( EXPORTING iv_comp = 'KOINH' iv_value = CONV string( ls_in-koinh )
                     CHANGING cs_data = ls_bk-data cs_datax = ls_bk-datax ).
      APPEND ls_bk TO lt_bank.
    ENDLOOP.

    IF lv_bad = abap_true OR lt_bank IS INITIAL.
      RETURN.
    ENDIF.

    " keep the accounts that are not in the file - the API works on gross data
    DATA(lv_before) = lines( lt_bank ).
    merge_banks( EXPORTING iv_lifnr = iv_lifnr CHANGING ct_bank = lt_bank ).
    IF lines( lt_bank ) > lv_before.
      mo_log->add( iv_row = lv_row iv_k1 = iv_lifnr iv_ty = 'S'
                   iv_txt = |{ lines( lt_bank ) - lv_before } existing bank account(s) retained| ).
    ENDIF.

    DATA ls_data TYPE cvis_ei_extern.
    CLEAR ls_data.
    header( EXPORTING iv_lifnr = iv_lifnr iv_task = gc_u CHANGING cs_data = ls_data ).
    ls_data-vendor-central_data-bankdetail-bankdetails = lt_bank.

    mo_cvis->post( iv_row = lv_row iv_k1 = iv_lifnr iv_k2 = lv_cc is_data = ls_data ).
  ENDMETHOD.

  METHOD lif_h~run.
    DATA lt_in TYPE tt_in.

    LOOP AT it_row INTO DATA(ls_row).
      IF lcl_util=>skip_row( ls_row ) = abap_true.
        CONTINUE.
      ENDIF.
      DATA(lv_l) = lcl_util=>lifnr( lcl_util=>cell( is_row = ls_row iv_col = 2 ) ).
      IF lv_l IS INITIAL.
        CONTINUE.
      ENDIF.
      APPEND VALUE ty_in(
        row   = ls_row-row
        lifnr = lv_l
        bukrs = lcl_util=>cell( is_row = ls_row iv_col = 3 )
        banks = to_upper( lcl_util=>cell( is_row = ls_row iv_col = 4 ) )
        bankl = lcl_util=>cell( is_row = ls_row iv_col = 5 )
        bankn = lcl_util=>cell( is_row = ls_row iv_col = 6 )
        koinh = lcl_util=>cell( is_row = ls_row iv_col = 7 )
        iban  = lcl_util=>cell( is_row = ls_row iv_col = 8 ) ) TO lt_in.
    ENDLOOP.

    SORT lt_in BY lifnr row.

    DATA: lv_prev TYPE lifnr,
          lt_grp  TYPE tt_in.
    LOOP AT lt_in INTO DATA(ls_in).
      IF lv_prev IS NOT INITIAL AND ls_in-lifnr <> lv_prev.
        DATA lv_frow TYPE i.
        lv_frow = VALUE #( lt_grp[ 1 ]-row OPTIONAL ).
        flush( iv_lifnr = lv_prev it_grp = lt_grp ).
        CLEAR lt_grp.
        IF p_stop = abap_true AND mo_log->has_error( lv_frow ) = abap_true.
          RETURN.
        ENDIF.
      ENDIF.
      APPEND ls_in TO lt_grp.
      lv_prev = ls_in-lifnr.
    ENDLOOP.
    IF lt_grp IS NOT INITIAL.
      flush( iv_lifnr = lv_prev it_grp = lt_grp ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Tab 6 - Vendor extension  (14 columns, data from row 7)
*   cols 2-4 target LIFNR/BUKRS/EKORG, cols 5-7 the reference org units,
*   col 8 "always X" is an LSMW artifact and is ignored.
*----------------------------------------------------------------------*
CLASS lcl_h_ext DEFINITION INHERITING FROM lcl_base FINAL.
  PUBLIC SECTION.
    METHODS lif_h~sheet     REDEFINITION.
    METHODS lif_h~first_row REDEFINITION.
    METHODS lif_h~run       REDEFINITION.
ENDCLASS.

CLASS lcl_h_ext IMPLEMENTATION.

  METHOD lif_h~sheet.     rv = gc_sh_ext. ENDMETHOD.
  METHOD lif_h~first_row. rv = 2. ENDMETHOD.

  METHOD lif_h~run.
    LOOP AT it_row INTO DATA(ls_row).
      IF lcl_util=>skip_row( ls_row ) = abap_true.
        CONTINUE.
      ENDIF.

      DATA(lv_lifnr) = lcl_util=>lifnr( lcl_util=>cell( is_row = ls_row iv_col = 2 ) ).
      DATA(lv_bukrs) = CONV bukrs( lcl_util=>cell( is_row = ls_row iv_col = 3 ) ).
      DATA(lv_ekorg) = CONV ekorg( lcl_util=>cell( is_row = ls_row iv_col = 4 ) ).
      DATA(lv_rbuk)  = CONV bukrs( lcl_util=>cell( is_row = ls_row iv_col = 6 ) ).
      DATA(lv_reko)  = CONV ekorg( lcl_util=>cell( is_row = ls_row iv_col = 7 ) ).

      IF lv_lifnr IS INITIAL.
        CONTINUE.
      ENDIF.
      IF mo_cfg->vend_exists( lv_lifnr ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_ty = 'E'
                     iv_txt = |Vendor { lv_lifnr } does not exist| ).
        CONTINUE.
      ENDIF.
      IF mo_cfg->ok_bukrs( lv_bukrs ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_ty = 'E'
                     iv_txt = |Target company code { lv_bukrs } does not exist| ).
        CONTINUE.
      ENDIF.
      IF lv_ekorg IS NOT INITIAL AND mo_cfg->ok_ekorg( lv_ekorg ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k3 = lv_ekorg iv_ty = 'E'
                     iv_txt = |Target purchasing organisation { lv_ekorg } does not exist| ).
        CONTINUE.
      ENDIF.

      " already extended?
      SELECT SINGLE @abap_true FROM lfb1
        WHERE lifnr = @lv_lifnr AND bukrs = @lv_bukrs INTO @DATA(lv_has_cc).
      IF lv_has_cc = abap_true.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_ty = 'W'
                     iv_txt = |Vendor is already extended to { lv_bukrs } - existing data will be updated| ).
      ENDIF.

      DATA ls_data TYPE cvis_ei_extern.
      CLEAR ls_data.
      header( EXPORTING iv_lifnr = lv_lifnr iv_task = gc_u CHANGING cs_data = ls_data ).

      " ---- company code: copy from the reference, then overlay ----------
      DATA ls_cc TYPE vmds_ei_company.
      CLEAR ls_cc.
      ls_cc-task           = COND #( WHEN mo_cfg->has_lfb1( iv_lifnr = CONV lifnr( lv_lifnr ) iv_bukrs = CONV bukrs( lv_bukrs ) ) = abap_true
                                     THEN gc_u ELSE gc_i ).
      ls_cc-data_key-bukrs = lv_bukrs.

      IF lv_rbuk IS NOT INITIAL.
        SELECT SINGLE akont, zterm, zwels, reprf, fdgrv
          FROM lfb1 WHERE lifnr = @lv_lifnr AND bukrs = @lv_rbuk
          INTO @DATA(ls_ref).
        IF sy-subrc = 0.
          lcl_util=>set( EXPORTING iv_comp = 'AKONT' iv_value = CONV string( ls_ref-akont )
                         CHANGING cs_data = ls_cc-data cs_datax = ls_cc-datax ).
          lcl_util=>set( EXPORTING iv_comp = 'ZTERM' iv_value = CONV string( ls_ref-zterm )
                         CHANGING cs_data = ls_cc-data cs_datax = ls_cc-datax ).
          lcl_util=>set( EXPORTING iv_comp = 'ZWELS' iv_value = CONV string( ls_ref-zwels )
                         CHANGING cs_data = ls_cc-data cs_datax = ls_cc-datax ).
          lcl_util=>set( EXPORTING iv_comp = 'REPRF' iv_value = CONV string( ls_ref-reprf )
                         CHANGING cs_data = ls_cc-data cs_datax = ls_cc-datax ).
          lcl_util=>set( EXPORTING iv_comp = 'FDGRV' iv_value = CONV string( ls_ref-fdgrv )
                         CHANGING cs_data = ls_cc-data cs_datax = ls_cc-datax ).
        ELSE.
          mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_ty = 'W'
                       iv_txt = |Reference company code { lv_rbuk } has no data for this vendor| ).
        ENDIF.
      ENDIF.

      " overlay: col 9 AKONT, col 10 payment method, col 11 double-invoice check
      DATA(lv_akont) = lcl_util=>gl( lcl_util=>cell( is_row = ls_row iv_col = 9 ) ).
      IF lv_akont IS NOT INITIAL.
        IF mo_cfg->ok_akont( iv_bukrs = lv_bukrs iv_saknr = lv_akont ) = abap_false.
          mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_ty = 'E'
                       iv_txt = |{ lv_akont } is not a vendor reconciliation account in { lv_bukrs }| ).
          CONTINUE.
        ENDIF.
        lcl_util=>set( EXPORTING iv_comp = 'AKONT' iv_value = CONV string( lv_akont )
                       CHANGING cs_data = ls_cc-data cs_datax = ls_cc-datax ).
      ENDIF.
      lcl_util=>set( EXPORTING iv_comp = 'ZWELS' iv_value = lcl_util=>cell( is_row = ls_row iv_col = 10 )
                     CHANGING cs_data = ls_cc-data cs_datax = ls_cc-datax ).
      lcl_util=>set( EXPORTING iv_comp = 'REPRF' iv_value = lcl_util=>cell( is_row = ls_row iv_col = 11 )
                     CHANGING cs_data = ls_cc-data cs_datax = ls_cc-datax ).
      APPEND ls_cc TO ls_data-vendor-company_data-company.

      " ---- purchasing org: copy from the reference, then overlay --------
      IF lv_ekorg IS NOT INITIAL.
        DATA ls_po TYPE vmds_ei_purchasing.
        CLEAR ls_po.
        ls_po-task           = COND #( WHEN mo_cfg->has_lfm1( iv_lifnr = CONV lifnr( lv_lifnr ) iv_ekorg = CONV ekorg( lv_ekorg ) ) = abap_true
                                       THEN gc_u ELSE gc_i ).
        ls_po-data_key-ekorg = lv_ekorg.

        IF lv_reko IS NOT INITIAL.
          SELECT SINGLE waers, zterm, kalsk, webre, inco1, inco2
            FROM lfm1 WHERE lifnr = @lv_lifnr AND ekorg = @lv_reko
            INTO @DATA(ls_rp).
          IF sy-subrc = 0.
            lcl_util=>set( EXPORTING iv_comp = 'WAERS' iv_value = CONV string( ls_rp-waers )
                           CHANGING cs_data = ls_po-data cs_datax = ls_po-datax ).
            lcl_util=>set( EXPORTING iv_comp = 'ZTERM' iv_value = CONV string( ls_rp-zterm )
                           CHANGING cs_data = ls_po-data cs_datax = ls_po-datax ).
            lcl_util=>set( EXPORTING iv_comp = 'KALSK' iv_value = CONV string( ls_rp-kalsk )
                           CHANGING cs_data = ls_po-data cs_datax = ls_po-datax ).
            lcl_util=>set( EXPORTING iv_comp = 'WEBRE' iv_value = CONV string( ls_rp-webre )
                           CHANGING cs_data = ls_po-data cs_datax = ls_po-datax ).
            lcl_util=>set( EXPORTING iv_comp = 'INCO1' iv_value = CONV string( ls_rp-inco1 )
                           CHANGING cs_data = ls_po-data cs_datax = ls_po-datax ).
            lcl_util=>set( EXPORTING iv_comp = 'INCO2' iv_value = CONV string( ls_rp-inco2 )
                           CHANGING cs_data = ls_po-data cs_datax = ls_po-datax ).
          ENDIF.
        ENDIF.

        lcl_util=>set( EXPORTING iv_comp = 'WAERS' iv_value = to_upper( lcl_util=>cell( is_row = ls_row iv_col = 12 ) )
                       CHANGING cs_data = ls_po-data cs_datax = ls_po-datax ).
        lcl_util=>set( EXPORTING iv_comp = 'KALSK' iv_value = lcl_util=>cell( is_row = ls_row iv_col = 13 )
                       CHANGING cs_data = ls_po-data cs_datax = ls_po-datax ).
        lcl_util=>set( EXPORTING iv_comp = 'WEBRE' iv_value = lcl_util=>cell( is_row = ls_row iv_col = 14 )
                       CHANGING cs_data = ls_po-data cs_datax = ls_po-datax ).
        APPEND ls_po TO ls_data-vendor-purchasing_data-purchasing.
      ENDIF.

      mo_cvis->post( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_k3 = lv_ekorg
                     is_data = ls_data ).

      IF p_stop = abap_true AND mo_log->has_error( ls_row-row ) = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Tab 7 - CIN details  (15 columns, data from row 2)
*   All fields live on LFA1 and are reachable through VMDS_EI_VMD_CENTRAL.
*   Column 3 "Address View" is an XK02 screen flag and is ignored.
*----------------------------------------------------------------------*
CLASS lcl_h_cin DEFINITION INHERITING FROM lcl_base FINAL.
  PUBLIC SECTION.
    METHODS lif_h~sheet     REDEFINITION.
    METHODS lif_h~first_row REDEFINITION.
    METHODS lif_h~run       REDEFINITION.
ENDCLASS.

CLASS lcl_h_cin IMPLEMENTATION.

  METHOD lif_h~sheet.     rv = gc_sh_cin. ENDMETHOD.
  METHOD lif_h~first_row. rv = 2. ENDMETHOD.

  METHOD lif_h~run.
    " column -> LFA1 field
    DATA(lt_map) = VALUE string_table(
      ( |J_1IEXCD;4|   ) ( |J_1IEXRN;5|   ) ( |J_1IEXRG;6|  ) ( |J_1IEXDI;7| )
      ( |J_1IEXCO;8|   ) ( |J_1ICSTNO;9|  ) ( |J_1ILSTNO;10| ) ( |J_1ISERN;11| )
      ( |J_1IPANNO;12| ) ( |J_1ISSIST;13| ) ( |J_1IEXCIVE;14| ) ( |J_1IVTYP;15| ) ).

    LOOP AT it_row INTO DATA(ls_row).
      IF lcl_util=>skip_row( ls_row ) = abap_true.
        CONTINUE.
      ENDIF.

      DATA(lv_lifnr) = lcl_util=>lifnr( lcl_util=>cell( is_row = ls_row iv_col = 1 ) ).
      DATA(lv_bukrs) = CONV bukrs( lcl_util=>cell( is_row = ls_row iv_col = 2 ) ).
      IF lv_lifnr IS INITIAL.
        CONTINUE.
      ENDIF.
      IF mo_cfg->vend_exists( lv_lifnr ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_ty = 'E'
                     iv_txt = |Vendor { lv_lifnr } does not exist| ).
        CONTINUE.
      ENDIF.

      DATA ls_data TYPE cvis_ei_extern.
      CLEAR ls_data.
      header( EXPORTING iv_lifnr = lv_lifnr iv_task = gc_u CHANGING cs_data = ls_data ).

      DATA lv_any TYPE abap_bool.
      CLEAR lv_any.
      LOOP AT lt_map INTO DATA(lv_p).
        SPLIT lv_p AT ';' INTO DATA(lv_f) DATA(lv_c).
        DATA(lv_v) = lcl_util=>cell( is_row = ls_row iv_col = CONV i( lv_c ) ).
        IF lv_v IS INITIAL.
          CONTINUE.
        ENDIF.
        lv_any = abap_true.
        lcl_util=>set( EXPORTING iv_comp = lv_f iv_value = lv_v
                       CHANGING  cs_data  = ls_data-vendor-central_data-central-data
                                 cs_datax = ls_data-vendor-central_data-central-datax ).
      ENDLOOP.

      IF lv_any = abap_false.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_ty = 'W'
                     iv_txt = 'No CIN value supplied - row skipped' ).
        CONTINUE.
      ENDIF.

      mo_cvis->post( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs is_data = ls_data ).

      IF p_stop = abap_true AND mo_log->has_error( ls_row-row ) = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Tab 8 - Partner function  (35 columns, data from row 10)
*   Note the column order: PARVW_05..15 come BEFORE PARVW_01..04, so the
*   pairs are addressed explicitly rather than by a simple offset.
*   Columns 4 (D0320) and 5 (USE_ZAV) are LSMW artifacts and are ignored.
*----------------------------------------------------------------------*
CLASS lcl_h_pfn DEFINITION INHERITING FROM lcl_base FINAL.
  PUBLIC SECTION.
    METHODS lif_h~sheet     REDEFINITION.
    METHODS lif_h~first_row REDEFINITION.
    METHODS lif_h~run       REDEFINITION.
ENDCLASS.

CLASS lcl_h_pfn IMPLEMENTATION.

  METHOD lif_h~sheet.     rv = gc_sh_pfn. ENDMETHOD.
  METHOD lif_h~first_row. rv = 2. ENDMETHOD.

  METHOD lif_h~run.
    " "PARVW column;GPARN column" for the 15 slots, in template order
    DATA(lt_pair) = VALUE string_table(
      ( |6;17|  ) ( |7;18|  ) ( |8;19|  ) ( |9;20|  ) ( |10;21| )
      ( |11;22| ) ( |12;23| ) ( |13;24| ) ( |14;25| ) ( |15;26| ) ( |16;27| )
      ( |28;32| ) ( |29;33| ) ( |30;34| ) ( |31;35| ) ).

    LOOP AT it_row INTO DATA(ls_row).
      IF lcl_util=>skip_row( ls_row ) = abap_true.
        CONTINUE.
      ENDIF.

      DATA(lv_lifnr) = lcl_util=>lifnr( lcl_util=>cell( is_row = ls_row iv_col = 1 ) ).
      DATA(lv_bukrs) = CONV bukrs( lcl_util=>cell( is_row = ls_row iv_col = 2 ) ).
      DATA(lv_ekorg) = CONV ekorg( lcl_util=>cell( is_row = ls_row iv_col = 3 ) ).

      IF lv_lifnr IS INITIAL.
        CONTINUE.
      ENDIF.
      IF mo_cfg->vend_exists( lv_lifnr ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_ty = 'E'
                     iv_txt = |Vendor { lv_lifnr } does not exist| ).
        CONTINUE.
      ENDIF.
      IF mo_cfg->ok_ekorg( lv_ekorg ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k3 = lv_ekorg iv_ty = 'E'
                     iv_txt = |Purchasing organisation { lv_ekorg } does not exist| ).
        CONTINUE.
      ENDIF.

      DATA lt_fn TYPE vmds_ei_functions_t.
      CLEAR lt_fn.
      DATA lv_bad TYPE abap_bool.
      CLEAR lv_bad.

      LOOP AT lt_pair INTO DATA(lv_pair).
        SPLIT lv_pair AT ';' INTO DATA(lv_pc) DATA(lv_gc).
        DATA(lv_parvw) = CONV parvw( to_upper( lcl_util=>cell( is_row = ls_row iv_col = CONV i( lv_pc ) ) ) ).
        DATA(lv_partn) = lcl_util=>lifnr( lcl_util=>cell( is_row = ls_row iv_col = CONV i( lv_gc ) ) ).

        IF lv_parvw IS INITIAL AND lv_partn IS INITIAL.
          CONTINUE.
        ENDIF.
        IF lv_parvw IS INITIAL OR lv_partn IS INITIAL.
          mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k3 = lv_ekorg iv_ty = 'E'
                       iv_txt = |Columns { lv_pc }/{ lv_gc }: partner function and partner number must both be filled| ).
          lv_bad = abap_true.
          CONTINUE.
        ENDIF.
        IF mo_cfg->ok_parvw( lv_parvw ) = abap_false.
          mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k3 = lv_ekorg iv_ty = 'E'
                       iv_txt = |Partner function { lv_parvw } does not exist (column { lv_pc })| ).
          lv_bad = abap_true.
          CONTINUE.
        ENDIF.
        IF mo_cfg->vend_exists( lv_partn ) = abap_false.
          mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k3 = lv_ekorg iv_ty = 'E'
                       iv_txt = |Partner vendor { lv_partn } does not exist (column { lv_gc })| ).
          lv_bad = abap_true.
          CONTINUE.
        ENDIF.

        DATA ls_fn TYPE vmds_ei_functions.
        CLEAR ls_fn.
        ls_fn-task           = COND #( WHEN mo_cfg->has_wyt3( iv_lifnr = CONV lifnr( lv_lifnr )
                                                              iv_ekorg = CONV ekorg( lv_ekorg )
                                                              iv_parvw = CONV parvw( lv_parvw ) ) = abap_true
                                       THEN gc_u ELSE gc_i ).
        ls_fn-data_key-parvw = lv_parvw.
        ls_fn-data_key-parza = '000'.
        lcl_util=>set( EXPORTING iv_comp = 'PARTNER' iv_value = CONV string( lv_partn )
                       CHANGING cs_data = ls_fn-data cs_datax = ls_fn-datax ).
        APPEND ls_fn TO lt_fn.
      ENDLOOP.

      IF lv_bad = abap_true.
        IF p_stop = abap_true.
          EXIT.
        ENDIF.
        CONTINUE.
      ENDIF.
      IF lt_fn IS INITIAL.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_ty = 'W'
                     iv_txt = 'No partner function supplied - row skipped' ).
        CONTINUE.
      ENDIF.

      " Account groups auto-create LF/RS/WL pointing at the vendor itself;
      " those must be sent back or the API removes them.
      merge_funcs( EXPORTING iv_lifnr = lv_lifnr iv_ekorg = lv_ekorg CHANGING ct_func = lt_fn ).

      DATA ls_data TYPE cvis_ei_extern.
      CLEAR ls_data.
      header( EXPORTING iv_lifnr = lv_lifnr iv_task = gc_u CHANGING cs_data = ls_data ).

      DATA ls_po TYPE vmds_ei_purchasing.
      CLEAR ls_po.
      ls_po-task              = COND #( WHEN mo_cfg->has_lfm1( iv_lifnr = CONV lifnr( lv_lifnr ) iv_ekorg = CONV ekorg( lv_ekorg ) ) = abap_true
                                        THEN gc_u ELSE gc_i ).
      ls_po-data_key-ekorg    = lv_ekorg.
      ls_po-functions-functions = lt_fn.
      APPEND ls_po TO ls_data-vendor-purchasing_data-purchasing.

      mo_cvis->post( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_k3 = lv_ekorg
                     is_data = ls_data ).

      IF p_stop = abap_true AND mo_log->has_error( ls_row-row ) = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Tab 9 - Block_Unblocked  (9 columns, data from row 9)
*   col1 transaction code (LSMW artifact, ignored) | col2 LIFNR
*   col3 BUKRS | col4 EKORG | col5 SPERR | col6 SPERR_1
*   col7 SPERM | col8 SPERM_1 | col9 SPERQ
*
*   A blank cell means "leave this flag alone". To CLEAR a flag the cell
*   must contain UNBLOCK (or #BLANK#) - without that marker an unblock
*   cannot be expressed at all. This is the agreed template addition.
*----------------------------------------------------------------------*
CLASS lcl_h_blk DEFINITION INHERITING FROM lcl_base FINAL.
  PUBLIC SECTION.
    METHODS lif_h~sheet     REDEFINITION.
    METHODS lif_h~first_row REDEFINITION.
    METHODS lif_h~run       REDEFINITION.
  PRIVATE SECTION.
    METHODS flag IMPORTING iv_cell TYPE string RETURNING VALUE(rv) TYPE string.
ENDCLASS.

CLASS lcl_h_blk IMPLEMENTATION.

  METHOD lif_h~sheet.     rv = gc_sh_blk. ENDMETHOD.
  METHOD lif_h~first_row. rv = 2. ENDMETHOD.

  METHOD flag.
    DATA(lv) = to_upper( condense( iv_cell ) ).
    CASE lv.
      WHEN ''.                              CLEAR rv.
      WHEN 'UNBLOCK' OR 'U' OR gc_clear.    rv = gc_clear.
      WHEN 'BLOCK'.                         rv = 'X'.
      WHEN OTHERS.                          rv = lv.
    ENDCASE.
  ENDMETHOD.

  METHOD lif_h~run.
    LOOP AT it_row INTO DATA(ls_row).
      IF lcl_util=>skip_row( ls_row ) = abap_true.
        CONTINUE.
      ENDIF.

      DATA(lv_lifnr) = lcl_util=>lifnr( lcl_util=>cell( is_row = ls_row iv_col = 2 ) ).
      DATA(lv_bukrs) = CONV bukrs( lcl_util=>cell( is_row = ls_row iv_col = 3 ) ).
      DATA(lv_ekorg) = CONV ekorg( lcl_util=>cell( is_row = ls_row iv_col = 4 ) ).

      IF lv_lifnr IS INITIAL.
        CONTINUE.
      ENDIF.
      IF mo_cfg->vend_exists( lv_lifnr ) = abap_false.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_ty = 'E'
                     iv_txt = |Vendor { lv_lifnr } does not exist| ).
        CONTINUE.
      ENDIF.

      DATA(lv_s)   = flag( lcl_util=>cell( is_row = ls_row iv_col = 5 ) ).   " central posting
      DATA(lv_s1)  = flag( lcl_util=>cell( is_row = ls_row iv_col = 6 ) ).   " CC posting
      DATA(lv_m)   = flag( lcl_util=>cell( is_row = ls_row iv_col = 7 ) ).   " central purchasing
      DATA(lv_m1)  = flag( lcl_util=>cell( is_row = ls_row iv_col = 8 ) ).   " POrg purchasing
      DATA(lv_q)   = flag( lcl_util=>cell( is_row = ls_row iv_col = 9 ) ).   " function block

      " template's own rules
      IF lv_s1 IS NOT INITIAL AND lv_bukrs IS INITIAL.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_ty = 'E'
                     iv_txt = 'SPERR_1 (company-code block) requires a company code in column 3' ).
        CONTINUE.
      ENDIF.
      IF lv_m1 IS NOT INITIAL AND lv_ekorg IS INITIAL.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_ty = 'E'
                     iv_txt = 'SPERM_1 (purch.org block) requires a purchasing organisation in column 4' ).
        CONTINUE.
      ENDIF.
      IF lv_q IS NOT INITIAL AND lv_q <> gc_clear
         AND ( lv_s1 IS NOT INITIAL OR lv_m1 IS NOT INITIAL ).
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_ty = 'E'
                     iv_txt = 'SPERQ must stay blank when a company-code or purch.org block is applied' ).
        CONTINUE.
      ENDIF.
      IF lv_s IS INITIAL AND lv_s1 IS INITIAL AND lv_m IS INITIAL
         AND lv_m1 IS INITIAL AND lv_q IS INITIAL.
        mo_log->add( iv_row = ls_row-row iv_k1 = lv_lifnr iv_ty = 'W'
                     iv_txt = 'No block indicator supplied - row skipped' ).
        CONTINUE.
      ENDIF.

      DATA ls_data TYPE cvis_ei_extern.
      CLEAR ls_data.
      header( EXPORTING iv_lifnr = lv_lifnr iv_task = gc_u CHANGING cs_data = ls_data ).

      lcl_util=>set( EXPORTING iv_comp = 'SPERR' iv_value = lv_s iv_force = xsdbool( lv_s = gc_clear )
                     CHANGING  cs_data  = ls_data-vendor-central_data-central-data
                               cs_datax = ls_data-vendor-central_data-central-datax ).
      lcl_util=>set( EXPORTING iv_comp = 'SPERM' iv_value = lv_m iv_force = xsdbool( lv_m = gc_clear )
                     CHANGING  cs_data  = ls_data-vendor-central_data-central-data
                               cs_datax = ls_data-vendor-central_data-central-datax ).
      lcl_util=>set( EXPORTING iv_comp = 'SPERQ' iv_value = lv_q iv_force = xsdbool( lv_q = gc_clear )
                     CHANGING  cs_data  = ls_data-vendor-central_data-central-data
                               cs_datax = ls_data-vendor-central_data-central-datax ).

      IF lv_s1 IS NOT INITIAL.
        DATA ls_cc TYPE vmds_ei_company.
        CLEAR ls_cc.
        ls_cc-task           = COND #( WHEN mo_cfg->has_lfb1( iv_lifnr = CONV lifnr( lv_lifnr ) iv_bukrs = CONV bukrs( lv_bukrs ) ) = abap_true
                                     THEN gc_u ELSE gc_i ).
        ls_cc-data_key-bukrs = lv_bukrs.
        lcl_util=>set( EXPORTING iv_comp = 'SPERR' iv_value = lv_s1 iv_force = abap_true
                       CHANGING cs_data = ls_cc-data cs_datax = ls_cc-datax ).
        APPEND ls_cc TO ls_data-vendor-company_data-company.
      ENDIF.

      IF lv_m1 IS NOT INITIAL.
        DATA ls_po TYPE vmds_ei_purchasing.
        CLEAR ls_po.
          ls_po-task           = COND #( WHEN mo_cfg->has_lfm1( iv_lifnr = CONV lifnr( lv_lifnr ) iv_ekorg = CONV ekorg( lv_ekorg ) ) = abap_true
                                       THEN gc_u ELSE gc_i ).
        ls_po-data_key-ekorg = lv_ekorg.
        lcl_util=>set( EXPORTING iv_comp = 'SPERM' iv_value = lv_m1 iv_force = abap_true
                       CHANGING cs_data = ls_po-data cs_datax = ls_po-datax ).
        APPEND ls_po TO ls_data-vendor-purchasing_data-purchasing.
      ENDIF.

      mo_cvis->post( iv_row = ls_row-row iv_k1 = lv_lifnr iv_k2 = lv_bukrs iv_k3 = lv_ekorg
                     is_data = ls_data ).

      IF p_stop = abap_true AND mo_log->has_error( ls_row-row ) = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Factory
*----------------------------------------------------------------------*
CLASS lcl_factory DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS create IMPORTING io_log TYPE REF TO lcl_log
                         RETURNING VALUE(ro) TYPE REF TO lif_h.
ENDCLASS.

CLASS lcl_factory IMPLEMENTATION.
  METHOD create.
    CASE abap_true.
      WHEN p_r1. ro = NEW lcl_h_create( io_log ).
      WHEN p_r2. ro = NEW lcl_h_tds( io_log ).
      WHEN p_r3. ro = NEW lcl_h_tan( io_log ).
      WHEN p_r4. ro = NEW lcl_h_bkey( io_log ).
      WHEN p_r5. ro = NEW lcl_h_bank( io_log ).
      WHEN p_r6. ro = NEW lcl_h_ext( io_log ).
      WHEN p_r7. ro = NEW lcl_h_cin( io_log ).
      WHEN p_r8. ro = NEW lcl_h_pfn( io_log ).
      WHEN p_r9. ro = NEW lcl_h_blk( io_log ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* Selection-screen events
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: lt_ft TYPE filetable,
        lv_rc TYPE i,
        lv_ua TYPE i.
  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING window_title = 'Select the vendor upload workbook'
              file_filter  = 'Excel workbook (*.xlsx)|*.xlsx|All files (*.*)|*.*'
    CHANGING  file_table   = lt_ft
              rc           = lv_rc
              user_action  = lv_ua ).
  IF lv_ua = cl_gui_frontend_services=>action_ok AND lv_rc >= 1.
    READ TABLE lt_ft INTO DATA(ls_ft) INDEX 1.
    p_file = ls_ft-filename.
  ENDIF.

AT SELECTION-SCREEN.
  IF to_upper( CONV string( p_file ) ) NS '.XLSX'.
    MESSAGE 'The customer workbook is .xlsx - .xls is not supported.' TYPE 'E'.
  ENDIF.

*----------------------------------------------------------------------*
* Main
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA(go_log) = NEW lcl_log( ).
  DATA(go_h)   = lcl_factory=>create( go_log ).

  IF go_h IS INITIAL.
    MESSAGE 'No scenario selected.' TYPE 'E'.
  ENDIF.

  " Up-front check for a clean message. CL_MD_BP_MAINTAIN performs its own
  " checks per company code and purchasing organisation as well.
  AUTHORITY-CHECK OBJECT 'F_LFA1_BUK' ID 'BUKRS' DUMMY ID 'ACTVT' FIELD '02'.
  IF sy-subrc <> 0.
    MESSAGE 'No authorisation to change supplier master data (F_LFA1_BUK).' TYPE 'E'.
  ENDIF.
  IF p_test = abap_false.
    AUTHORITY-CHECK OBJECT 'F_LFA1_BUK' ID 'BUKRS' DUMMY ID 'ACTVT' FIELD '01'.
    IF sy-subrc <> 0.
      MESSAGE 'No authorisation for a productive run - please use the test run.' TYPE 'E'.
    ENDIF.
  ENDIF.

  DATA(gv_scen) = COND char2(
    WHEN p_r1 = abap_true THEN 'R1'
    WHEN p_r2 = abap_true THEN 'R2'
    WHEN p_r3 = abap_true THEN 'R3'
    WHEN p_r4 = abap_true THEN 'R4'
    WHEN p_r5 = abap_true THEN 'R5'
    WHEN p_r6 = abap_true THEN 'R6'
    WHEN p_r7 = abap_true THEN 'R7'
    WHEN p_r8 = abap_true THEN 'R8'
    ELSE                       'R9' ).

  DATA lt_rows  TYPE tt_row.
  DATA gv_sheet TYPE string.
  DATA gv_moved TYPE i.
  TRY.
      NEW lcl_excel( )->read(
        EXPORTING iv_file    = p_file
                  iv_sheet   = go_h->sheet( )
                  iv_from_pc = p_pc
                  it_hdr     = lcl_hdr=>for( gv_scen )
        IMPORTING et_row     = lt_rows
                  ev_sheet   = gv_sheet
                  ev_moved   = gv_moved ).
    CATCH lcx_upl INTO DATA(gx).
      " MESSAGE takes a data object, not an expression.
      DATA(gv_txt) = gx->get_text( ).
      MESSAGE gv_txt TYPE 'E'.
  ENDTRY.

  IF lt_rows IS INITIAL.
    DATA gv_none TYPE string.
    gv_none = |Tab "{ gv_sheet }" holds no data rows below its heading|.
    MESSAGE gv_none TYPE 'I'.
  ENDIF.

  " Say so when the file's columns are not where the template has them - the
  " data is read correctly either way, but it is worth knowing.
  IF gv_moved > 0.
    go_log->add( iv_row = 0 iv_ty = 'I'
                 iv_txt = |{ gv_moved } column(s) sit elsewhere in this file than in the | &&
                          |template - each was read from where its heading is| ).
  ENDIF.

  go_h->run( lt_rows ).

END-OF-SELECTION.
  go_log->display( ).
