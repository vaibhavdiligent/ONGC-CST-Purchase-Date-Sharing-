*&---------------------------------------------------------------------*
*& Report  @@PROGU@@
*&---------------------------------------------------------------------*
*& Title       : @@TITLE@@
*& Module      : @@MODULE@@                       Transaction : @@TCODE@@
*&
*& What it does
*&   One program for every change to, and every creation of, a
*&   @@WHAT1@@ - in place of the LSMW objects that did this one layout at a
*&   time. The user chooses the fields, the program writes a file with
*&   exactly those fields, and reads the same file back:
*&
*&   Download  Row 1 holds the field names (TABLE-FIELD, e.g. KNVV-KDGRP),
*&             row 2 onwards the data. With no numbers given the file has
*&             the headings only; with numbers, one row per record holding
*&             what SAP has today. The fields the system needs are added
*&             to the user's choice automatically.
*&   Upload    Change: each row is compared with SAP and only the fields
*&             that differ are sent. Create: a business partner is created
*&             with the @@WHAT1@@ role and the data in its rows.
*&
*&   Every value is checked before anything is posted: type, length,
*&   check table and fixed values from the ABAP Dictionary. Everything is
*&   written through the Business Partner API (CL_MD_BP_MAINTAIN) - no
*&   table is updated directly. A test run shows what would change.
*&
*& Clean core positioning (SAP S/4HANA 2025 / ABAP_CLOUD_READINESS)
*&   Classic ABAP, deliberately: CL_MD_BP_MAINTAIN is not released for
*&   ABAP Cloud and no released API maintains the customer and supplier
*&   parts of a business partner from a GUI mass upload. Reads of the
*&   master tables, the ABAP Dictionary (DDIF_FK_GET, DD07L) and the text IDs
*&   (TTXID) are confined to LCL_CAT, LCL_CHK, LCL_CFG and LCL_DB.
*&
*& Generated - do not edit. Source: tools/ccej/bp_maint/*.abap,
*& generator tools/ccej/gen_bp_maint.py.
*&---------------------------------------------------------------------*
REPORT @@PROG@@.

" C = customer program, V = supplier program. The two programs are one
" source; this constant is what tells them apart at runtime.
CONSTANTS gc_side TYPE c LENGTH 1 VALUE '@@SIDE@@'.

TABLES sscrfields.

" 255 is the widest a screen field goes; the file dialog returns a STRING
" and a shorter field would cut a long path short.
TYPES ty_path  TYPE c LENGTH 255.
" A field name in the file: TABLE-FIELD, e.g. KNVV-/CCEJ/KNVV_ZKZBLCREKBN.
TYPES ty_fname TYPE c LENGTH 60.
TYPES ty_dec   TYPE p LENGTH 15 DECIMALS 2.
TYPES ty_grp   TYPE c LENGTH 4.
TYPES ty_msgty TYPE c LENGTH 1.

TYPES: tt_cell TYPE string_table.
TYPES: BEGIN OF ty_row,
         row   TYPE i,
         cells TYPE tt_cell,
       END OF ty_row,
       tt_row TYPE STANDARD TABLE OF ty_row WITH EMPTY KEY.

" One field the program can download and upload.
TYPES: BEGIN OF ty_fld,
         name     TYPE string,     " heading in the file: TABLE-FIELD
         grp      TYPE ty_grp,     " which part of the master record
         tab      TYPE tabname,    " table the current value is read from
         fld      TYPE fieldname,  " field in that table
         api      TYPE fieldname,  " component in the API structure
         key      TYPE abap_bool,  " key of its group
         must     TYPE abap_bool,  " always in the file (key / required)
         text     TYPE string,     " description
         rollname TYPE rollname,
         convexit TYPE convexit,
         inttype  TYPE inttype,
         leng     TYPE i,
         decimals TYPE i,
         lower    TYPE abap_bool,
         checktab TYPE tabname,
         domname  TYPE domname,
         valexi   TYPE abap_bool,
         tdobj    TYPE tdobject,   " texts only
         tdid     TYPE tdid,       " texts only
       END OF ty_fld,
       tt_fld TYPE STANDARD TABLE OF ty_fld WITH EMPTY KEY.

" One part of the master record: the general data, a company code, a sales
" area, the partner functions of a sales area and so on.
"   REP     the part repeats - one row in the file per entry, with ACTION
"   PARENT  the part it hangs under (a partner function under a sales area)
"   KEYS    its own key fields, blank-separated
"   API     the ABAP Dictionary structure the API takes its data in
TYPES: BEGIN OF ty_gdef,
         grp    TYPE ty_grp,
         tab    TYPE tabname,
         text   TYPE string,
         rep    TYPE abap_bool,
         parent TYPE ty_grp,
         keys   TYPE string,
         api    TYPE string,
         numfld TYPE fieldname,
       END OF ty_gdef,
       tt_gdef TYPE STANDARD TABLE OF ty_gdef WITH EMPTY KEY.

" One line of the result list.
TYPES: BEGIN OF ty_log,
         status TYPE icon_d,
         row    TYPE i,
         obj    TYPE string,
         part   TYPE string,
         field  TYPE string,
         old    TYPE string,
         new    TYPE string,
         type   TYPE ty_msgty,
         msg    TYPE string,
       END OF ty_log,
       tt_log TYPE STANDARD TABLE OF ty_log WITH EMPTY KEY.

CONSTANTS:
  " A cell holding this is left as it is in SAP.
  gc_keep   TYPE string VALUE '#',
  " Used by the reused helpers for "nothing in the cell".
  gc_clear  TYPE string VALUE '#BLANK#',
  " Column that says what to do with a repeating entry: I, U or D.
  gc_action TYPE string VALUE 'ACTION',
  " The API's tasks.
  gc_ins    TYPE c LENGTH 1 VALUE 'I',
  gc_upd    TYPE c LENGTH 1 VALUE 'U',
  gc_del    TYPE c LENGTH 1 VALUE 'D',
  " A mobile number is a telephone entry with R_3_USER 3. It is not a flag:
  " anything but space, 1, 2 or 3 ends the transaction with a type X message.
  gc_mobile TYPE c LENGTH 1 VALUE '3',
  " Business partner category: organisation.
  gc_org    TYPE bu_type VALUE '2'.

" Data objects the SELECT-OPTIONS are built over.
DATA: gv_num   TYPE @@NUMTYPE@@,
      gv_bp    TYPE bu_partner,
      gv_bukrs TYPE bukrs,
      gv_org   TYPE @@ORGTYPE@@,
      gv_fname TYPE ty_fname.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_crt RADIOBUTTON GROUP g0 USER-COMMAND md,
            p_chg RADIOBUTTON GROUP g0 DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_down RADIOBUTTON GROUP g1 USER-COMMAND ac DEFAULT 'X',
            p_up   RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.

" The account group decides the number range and which fields a new record
" needs. Only creation asks for it.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_ktogr TYPE @@KTOTYPE@@,
            p_bpgrp TYPE bu_group.
SELECTION-SCREEN END OF BLOCK b2.

" The fields the user picked. S_FLD is not on the screen but it is part of
" it, so a variant saved with Goto > Variants keeps the choice and brings it
" back.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-004.
SELECTION-SCREEN PUSHBUTTON /1(24) bt_fld USER-COMMAND fld.
SELECTION-SCREEN COMMENT 27(50) cm_fld.
SELECT-OPTIONS s_fld FOR gv_fname NO-DISPLAY.
PARAMETERS p_tlang TYPE spras DEFAULT sy-langu.
SELECTION-SCREEN END OF BLOCK b3.

" Which records a download fills the file with. Left empty, the file has
" the headings only.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-005.
SELECT-OPTIONS: s_num   FOR gv_num,
                s_bp    FOR gv_bp,
                s_bukrs FOR gv_bukrs,
                s_org   FOR gv_org.
PARAMETERS:     p_max   TYPE i DEFAULT 1000.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-006.
PARAMETERS p_test AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b5.

" Not OBLIGATORY: a required field left empty stops every radio button click
" with "Fill in all required entry fields". It is checked on execution.
SELECTION-SCREEN BEGIN OF BLOCK b6 WITH FRAME TITLE TEXT-007.
PARAMETERS: p_file TYPE ty_path LOWER CASE,
            p_xlsx RADIOBUTTON GROUP g2 USER-COMMAND ft DEFAULT 'X',
            p_txt  RADIOBUTTON GROUP g2.
SELECTION-SCREEN END OF BLOCK b6.

*----------------------------------------------------------------------*
* Exception
*----------------------------------------------------------------------*
CLASS lcx_dl DEFINITION INHERITING FROM cx_static_check FINAL.
  PUBLIC SECTION.
    DATA text TYPE string.
    METHODS constructor IMPORTING iv_text TYPE string.
    METHODS get_text REDEFINITION.
ENDCLASS.

CLASS lcx_dl IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    text = iv_text.
  ENDMETHOD.
  METHOD get_text.
    result = text.
  ENDMETHOD.
ENDCLASS.

