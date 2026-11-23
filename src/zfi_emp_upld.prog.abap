*----------------------------------------------------------------------*
* Program
* Title             : ZFI_EMP_UPLD
* Author            : Rahul Magdum
* Program Type      : BDC Upload Program with file using interface
* Creation Date     : <23-03-2018>
* Transaction       :
* Functional design : <FI/089>
* Technical design  :
* Change request    : 8000004225
* Transport number  : CRDK953268
* Description       : Vendor Master Upload
*----------------------------------------------------------------------*
* Modification Information
* Date            : 07.01.2025
* Author          : Shubham Mishra
* Change request  : 8000003767
* Transport number: CRDK994198
* Description     : Error log via emai functionality .
*----------------------------------------------------------------------*
* Modification Information
* Date            : 04.04.2025 14.04.2025 16.04.2025 24.04.2025
*                   25.04.2025
* Author          : Aishwarya Patil
* Change request  : 8000003767
* Transport number: CRDK99539, CRDK995540, CRDK995566 CRDK995694
*                   CRDK995728
* Description     : Employee Vendor Creation
*----------------------------------------------------------------------*
* Modification Information
* Date            : 17.03.2026
* Author          : Prashanti Pandaram
* Change request  : 8000003767
* Transport number: CRDK9A0094
* Description     : Bank Key Creation
*----------------------------------------------------------------------*
* Modification Information
* Date            : 19.11.2026
* Description     : S/4HANA conversion. XK01 and FK02 are gone in
*                   S/4HANA: the employee vendor is created as a
*                   business partner - a PERSON, with first, middle and
*                   last name - through CL_MD_BP_MAINTAIN (local class
*                   LCL_EMP_BP), and a changed bank account is put on
*                   the partner. The active employee file is read in
*                   HRMS's layout (Employee vendor creation - mapping
*                   HRMS.xlsx) by its column headings; the 50-column
*                   layout is still read. Files, DPR / tax invoice
*                   check, archive and mails are unchanged.
*----------------------------------------------------------------------*
* Modification Information
* Date            : 22.11.2026
* Description     : From the HONC test in CRS: a changed bank account
*                   overwrites the first bank details ID (no second
*                   account left on the partner), and SAP's partner
*                   buffers are discarded after the validation and after
*                   a failed employee.
*----------------------------------------------------------------------*


REPORT zfi_emp_upld NO STANDARD PAGE HEADING LINE-SIZE 255.

TYPE-POOLS: truxs, slis.
TABLES: adr6.

*----------------------------------------------------------------------*
*  T Y P E S                                                           *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_file,
         lifnr      TYPE rf02k-lifnr,
         bukrs      TYPE rf02k-bukrs,
         ekorg      TYPE rf02k-ekorg,
         ktokk      TYPE rf02k-ktokk,
         title_medi TYPE sza1_d0100-title_medi,
         name1      TYPE addr1_data-name1,
         name2      TYPE addr1_data-name2,
         sort1      TYPE addr1_data-sort1,
         str_suppl1 TYPE addr1_data-str_suppl1,
         str_suppl2 TYPE addr1_data-str_suppl2,
         street     TYPE addr1_data-street,
         city2      TYPE addr1_data-city2,
         post_code1 TYPE addr1_data-post_code1,
         city1      TYPE addr1_data-city1,
         country    TYPE addr1_data-country,
         region     TYPE addr1_data-region,
         langu      TYPE addr1_data-langu,
         tel_number TYPE sza1_d0100-tel_number,
         mob_number TYPE sza1_d0100-mob_number,
         fax_number TYPE sza1_d0100-fax_number,
         smtp_addr  TYPE sza1_d0100-smtp_addr,  "21
         kunnr      TYPE lfa1-kunnr,
         vbund      TYPE lfa1-vbund,
         konzs      TYPE lfa1-konzs,
         fiskn      TYPE lfa1-fiskn,
         lnrza      TYPE lfa1-lnrza,
         banks      TYPE lfbk-banks,
         bankl      TYPE lfbk-bankl,
         bankn      TYPE lfbk-bankn,
         koinh      TYPE lfbk-koinh,
         akont      TYPE lfb1-akont,
         fdgrv      TYPE lfb1-fdgrv,
         altkn      TYPE lfb1-altkn,
         pernr      TYPE lfb1-pernr,
         zterm      TYPE lfb1-zterm,
         reprf      TYPE lfb1-reprf,
         zwels      TYPE lfb1-zwels,
         zahls      TYPE lfb1-zahls,
         hbkid      TYPE lfb1-hbkid,
         zgrup      TYPE lfb1-zgrup,
         waers      TYPE lfm1-waers,
         inco1      TYPE lfm1-inco1,                    "LFM1-ZTERM,
         inco2      TYPE lfm1-inco2,
         kalsk      TYPE lfm1-kalsk,
         webre      TYPE lfm1-webre,
         lebre      TYPE lfm1-lebre,
         ekgrp      TYPE lfm1-ekgrp,
         plifz      TYPE lfm1-plifz,
         j_1icstno  TYPE j_1icstno,
         j_1ilstno  TYPE j_1ilstno,
         " S/4HANA: the person's name, from HRMS (fname / mname / lname)
         fname      TYPE bu_namep_f,
         mname      TYPE bu_namemid,
         lname      TYPE bu_namep_l,
       END OF ty_file,

*   BOC BY Aishwarya - 8000003767 - 07.04.2025
       BEGIN OF ty_final_dpr,
         lifnr(10) TYPE c,
       END OF ty_final_dpr,

       BEGIN OF ty_file_dpr,
         buzei     TYPE string,
         bldat     TYPE string,
         blart     TYPE string,
         bukrs     TYPE string,
         event_dt  TYPE string,
         apr_dt    TYPE string,
         inv_sb_dt TYPE string,
         budat     TYPE string,
         waers     TYPE string,
         bktxt     TYPE string,
         kursf     TYPE string,
         xblnr     TYPE string,
         umskz     TYPE string,
         lifnr(10) TYPE c,
         dmbtr     TYPE string,
         zfbdt     TYPE string,
         prctr     TYPE string,
         zuonr     TYPE string,
         sgtxt     TYPE string,
         bupla     TYPE string,
         secoo     TYPE string,
         zlspr     TYPE string,
         zlsch     TYPE string,
         xref1     TYPE string,
         xref3     TYPE string,
         qsshb     TYPE string,
         witht     TYPE string,
         qsskz     TYPE string,
       END OF ty_file_dpr,

       BEGIN OF ty_file_tax,
         bukrs     TYPE string,
         bktxt     TYPE string,
         blart     TYPE string,
         xblnr     TYPE string,
         event_dt  TYPE string,
         apr_dt    TYPE string,
         inv_sb_dt TYPE string,
         bldat     TYPE string,
         budat     TYPE string,
         lifnr(10) TYPE c,
         bupla     TYPE string,
         secco     TYPE string,
         zterm     TYPE string,
         zlspr     TYPE string,
         zlsch     TYPE string,
         dmbtr     TYPE string,
         waers     TYPE string,
         xmwst     TYPE string,
         bstat     TYPE string,
         name1     TYPE string,
         rebzg     TYPE string,
         buzei     TYPE string,
         sgtxt     TYPE string,
         wrbtr     TYPE string,
         hsn_sac   TYPE string,
         mwskz     TYPE string,
         kschl     TYPE string,
         mwsts     TYPE string,
         wmwst     TYPE string,
         kostl     TYPE string,
         hkont     TYPE string,
         qsshb     TYPE string,
         witht     TYPE string,
         qsskz     TYPE string,
         prctr     TYPE string,
         zuonr     TYPE string,
       END OF ty_file_tax,

       BEGIN OF ty_final_tax,
         lifnr(10) TYPE c,
       END OF ty_final_tax,

       BEGIN OF ty_lfbk,
         lifnr TYPE	lifnr,
         banks TYPE banks,
         bankl TYPE bankl,
         bankn TYPE string,
       END OF ty_lfbk,

       BEGIN OF ty_lifnr,
         lifnr TYPE	lifnr,
         bukrs TYPE bukrs,
       END OF ty_lifnr,

       BEGIN OF ty_final,
         lifnr TYPE	lifnr,
       END OF ty_final,

*       BEGIN OF ty_panno,
*         lifnr     TYPE  lifnr,
*         j_1ipanno TYPE j_1ipanno,
*       END OF ty_panno,

*       BEGIN OF ty_msg,
*         emp_no  TYPE string,
*         message TYPE string,
*       END OF ty_msg.

       BEGIN OF ty_msg,
         emp_no   TYPE string,
         status   TYPE string,
         emp_name TYPE string,
*         pan_no   TYPE string,
         remarks  TYPE string,
       END OF ty_msg,

       " S/4HANA: the column of each HRMS field, from the file's header
       BEGIN OF ty_hrms_col,
         emp   TYPE i,
         sal   TYPE i,
         fname TYPE i,
         mname TYPE i,
         lname TYPE i,
         name  TYPE i,
         acct  TYPE i,
         ifsc  TYPE i,
         bctry TYPE i,
         mob   TYPE i,
         func  TYPE i,
         mail  TYPE i,
         mail2 TYPE i,
       END OF ty_hrms_col.
*   EOC BY Aishwarya - 8000003767 - 07.04.2025

*----------------------------------------------------------------------*
*  D A T A                                                             *
*----------------------------------------------------------------------*
* S/4HANA: no batch input (BDCDATA, BDCMSGCOLL, CTU_PARAMS) any more.
DATA: ta_msg            TYPE TABLE OF ty_msg,
      wa_msg            TYPE ty_msg,
*   BOC BY Aishwarya - 8000003767 - 07.04.2025
      ta_msg_update     TYPE TABLE OF ty_msg,
      ta_msg_new        TYPE TABLE OF ty_msg,
      wa_msg_new        TYPE ty_msg.
*   EOC BY Aishwarya - 8000003767 - 07.04.2025

* S/4HANA: the layout of the file being read
" S/4HANA conversion: the run's log, also printed in the job's spool -
" the mail can fail to go out (SOST status red, message 826, 23.11.2026).
DATA: gt_log       TYPE TABLE OF ty_msg,
      gt_run       TYPE string_table,
      gv_to_create TYPE i,
      gv_to_update TYPE i.
DATA: gs_col    TYPE ty_hrms_col,
      gv_layout TYPE c LENGTH 1,
      gv_sep    TYPE string.

DATA: ta_file         TYPE TABLE OF ty_file,
      wa_file         TYPE ty_file,
*   BOC BY Aishwarya - 8000003767 - 07.04.2025
      ta_file_dpr     TYPE TABLE OF ty_file_dpr,
      wa_file_dpr     TYPE ty_file_dpr,
      ta_file_tax     TYPE TABLE OF ty_file_tax,
      wa_file_tax     TYPE ty_file_tax,
      ta_tax_update   TYPE TABLE OF ty_final_tax,
      wa_tax_update   TYPE ty_final_tax,
      wa_tax_create   TYPE ty_final_tax,
      ta_tax_create   TYPE TABLE OF ty_final_tax,
      ta_dpr_update   TYPE TABLE OF ty_final_dpr,
      wa_dpr_update   TYPE ty_final_dpr,
      ta_dpr_create   TYPE TABLE OF ty_final_dpr,
      wa_dpr_create   TYPE ty_final_dpr,
      ta_update       TYPE TABLE OF ty_final,
      wa_update       TYPE ty_final,
      ta_create       TYPE TABLE OF ty_final,
      wa_create       TYPE ty_final,
*      ta_panno        TYPE TABLE OF ty_panno,
*      wa_panno        TYPE  ty_panno,
      ta_lfbk         TYPE TABLE OF ty_lfbk,
      ta_lifnr        TYPE TABLE OF ty_lifnr,
      lv_status       TYPE string,
      lv_bankl        TYPE string,
      lv_banka        TYPE string,
*   EOC BY Aishwarya - 8000003767 - 07.04.2025

      ta_contents_bin TYPE STANDARD TABLE OF solisti1 WITH HEADER LINE.

DATA: gv_cnt     TYPE p.

DATA: ta_fc TYPE slis_t_fieldcat_alv,
      wa_fc TYPE slis_fieldcat_alv,
      wa_lo TYPE slis_layout_alv.

*----------------------------------------------------------------------*
*  C O N S T A N T S                                                   *
*----------------------------------------------------------------------*
CONSTANTS: co_msgtyp    TYPE char1  VALUE 'S',
           co_id_path   TYPE char20 VALUE 'ZFI_EMPEXP',
           co_id_num1   TYPE numc3  VALUE 001,
           co_id_num2   TYPE numc3  VALUE 002,
           co_file      TYPE char40 VALUE 'Cipla_Active_Emp*',
           co_id_em     TYPE char20 VALUE 'ZFI_EMPEM4',
           co_pfile     TYPE char10 VALUE 'P_FILE',
           co_create(3) VALUE 'New',
           co_update(8) VALUE 'Modified',
           co_failed(6) VALUE 'Failed'.

* S/4HANA: the fixed values of the HRMS layout - sheet "expected output
* file for upload" of "Employee vendor creation - mapping HRMS.xlsx".
CONSTANTS: co_lay_hrms   TYPE c LENGTH 1 VALUE 'H',
           co_lay_sap    TYPE c LENGTH 1 VALUE 'S',
           co_hrms_bukrs TYPE bukrs      VALUE '1000',
           co_hrms_ktokk TYPE ktokk      VALUE 'Z008',
           co_hrms_land  TYPE land1      VALUE 'IN',
           co_hrms_langu TYPE c LENGTH 2 VALUE 'EN',
           co_hrms_akont TYPE akont      VALUE '1200013',
           co_hrms_zwels TYPE dzwels     VALUE 'CNT'.

*----------------------------------------------------------------------*
*  S E L E C T - O P T I O N S  &  P A R A M E T E R S                 *
*----------------------------------------------------------------------*
PARAMETERS: p_file TYPE rlgrap-filename DEFAULT text-003. "OBLIGATORY.

*   BOC BY Aishwarya - 8000003767 - 07.04.2025
SELECTION-SCREEN: BEGIN OF BLOCK b1.
*PARAMETERS: p_to TYPE string,
*            p_cc TYPE string.

SELECT-OPTIONS: s_to FOR adr6-smtp_addr NO INTERVALS,
                s_cc FOR adr6-smtp_addr NO INTERVALS.
"SOA by Prashanti for CRDK9A0094 on 17.03.2026
PARAMETERS: p_bkey  RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND toggle,
            p_empcd RADIOBUTTON GROUP rad1.
"EOA by Prashanti for CRDK9A0094 on 17.03.2026

SELECTION-SCREEN: END OF BLOCK b1.
*   EOC BY Aishwarya - 8000003767 - 07.04.2025
*----------------------------------------------------------------------*
*  S/4HANA: THE EMPLOYEE VENDOR AS A BUSINESS PARTNER                  *
*----------------------------------------------------------------------*
* XK01 and FK02 are gone in S/4HANA: a supplier is a business partner,
* created through CL_MD_BP_MAINTAIN, and CVI writes LFA1/LFB1/LFBK/LFM1
* from it. An employee is a natural person, so the partner is created as
* a PERSON (category 1) with first, middle and last name - not as an
* organisation. BP grouping and supplier roles come from the account
* group (CVIC_VEND_TO_BP1 / CVIC_VEND_TO_BP2), the partner takes the
* employee code as its number, as RF02K-LIFNR did in XK01. Bank details
* go on the business partner (BUT0BK), from which CVI writes LFBK.
*
* ABAP Cloud: CL_MD_BP_MAINTAIN, CVI_VEND_LINK and the CVIC_* tables are
* not released for ABAP Cloud - classic ABAP (tier 2). Run ATC with
* variant ABAP_CLOUD_READINESS and record them as known exemptions.
*----------------------------------------------------------------------*
CLASS lcl_emp_bp DEFINITION FINAL.
  PUBLIC SECTION.
    "! A new employee vendor, number IV_LIFNR (the employee code).
    CLASS-METHODS create
      IMPORTING is_file  TYPE ty_file
                iv_lifnr TYPE lifnr
      EXPORTING et_error TYPE bapiret2_t
                et_ok    TYPE bapiret2_t.
    "! FK02: the file's bank account into line 1 of the bank details - the
    "! vendor's first account is replaced, the others stay.
    CLASS-METHODS change_bank
      IMPORTING is_file  TYPE ty_file
                iv_lifnr TYPE lifnr
      EXPORTING et_error TYPE bapiret2_t
                et_ok    TYPE bapiret2_t.
  PRIVATE SECTION.
    CONSTANTS: c_insert TYPE c LENGTH 1 VALUE 'I',
               c_update TYPE c LENGTH 1 VALUE 'U'.
    CLASS-METHODS set
      IMPORTING iv_comp  TYPE clike
                iv_value TYPE any
      CHANGING  cs_data  TYPE any
                cs_datax TYPE any.
    CLASS-METHODS alpha
      IMPORTING iv_in     TYPE clike
                iv_width  TYPE i
      RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS title_key
      IMPORTING iv_in     TYPE clike
      RETURNING VALUE(rv) TYPE ad_title.
    CLASS-METHODS save
      IMPORTING is_data   TYPE cvis_ei_extern
      CHANGING  ct_error  TYPE bapiret2_t
      RETURNING VALUE(rv) TYPE abap_bool.
    CLASS-METHODS add
      IMPORTING iv_text TYPE clike
      CHANGING  ct      TYPE bapiret2_t.
    CLASS-METHODS reset_bp.
    "! Undo what is not committed and refresh the partner's buffers.
    CLASS-METHODS discard.
ENDCLASS.


CLASS lcl_emp_bp IMPLEMENTATION.

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

  METHOD alpha.
    rv = condense( CONV string( iv_in ) ).
    IF rv IS NOT INITIAL AND rv CO '0123456789'.
      rv = |{ rv ALPHA = IN WIDTH = iv_width }|.
    ENDIF.
  ENDMETHOD.

  METHOD title_key.
    " The file has the text ("Mr."); the partner takes its key (TSAD3T).
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
      IF to_upper( condense( CONV string( ls_t-title_medi ) ) ) = lv_up.
        rv = ls_t-title.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD add.
    APPEND VALUE bapiret2( type = 'E' message = iv_text ) TO ct.
  ENDMETHOD.

  METHOD discard.
    " BAPI_TRANSACTION_ROLLBACK also refreshes the buffers the business
    " partner functions registered; a bare ROLLBACK WORK does not.
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

  METHOD save.
    rv = abap_false.
    DATA lt_map TYPE mdg_bs_bp_msgmap_t.
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
    " What the validation left in the partner's buffers goes before the real
    " posting, and a failed employee leaves nothing behind for the next one -
    " in the HONC run a failed row's partner came up at a later row's COMMIT
    " WORK and ended the job in a short dump (CRS, 20.11.2026).
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
    " BAPIRETM lines carry a nested message table, read generically.
    FIELD-SYMBOLS <lt_sub> TYPE ANY TABLE.
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
    " BAPI_TRANSACTION_COMMIT with WAIT: RETURN says whether the update ran.
    DATA ls_cret TYPE bapiret2.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait   = abap_true
      IMPORTING return = ls_cret.
    reset_bp( ).
    IF ls_cret-type CA 'EAX'.
      add( EXPORTING iv_text = |The update did not go through - nothing was saved: { ls_cret-message } (SM13)|
           CHANGING  ct      = ct_error ).
      RETURN.
    ENDIF.
    rv = abap_true.
  ENDMETHOD.

  METHOD create.
    CLEAR: et_error, et_ok.
    DATA: ls_data TYPE cvis_ei_extern,
          lv_guid TYPE bu_partner_guid.

    " ---- BP grouping and supplier roles of the account group -------------
    DATA lv_grp TYPE bu_group.
    SELECT SINGLE grouping FROM cvic_vend_to_bp1
      WHERE account_group = @is_file-ktokk INTO @lv_grp.
    IF lv_grp IS INITIAL.
      add( EXPORTING iv_text = |Account group { is_file-ktokk } has no BP grouping (CVIC_VEND_TO_BP1)|
           CHANGING  ct      = et_error ).
      RETURN.
    ENDIF.
    SELECT account_group, role FROM cvic_vend_to_bp2
      WHERE account_group = @is_file-ktokk INTO TABLE @DATA(lt_role).
    IF lt_role IS INITIAL.
      add( EXPORTING iv_text = |Account group { is_file-ktokk } has no supplier roles (CVIC_VEND_TO_BP2)|
           CHANGING  ct      = et_error ).
      RETURN.
    ENDIF.
    TRY.
        lv_guid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
      CATCH cx_uuid_error.
        add( EXPORTING iv_text = 'No GUID could be generated for the new business partner'
             CHANGING  ct      = et_error ).
        RETURN.
    ENDTRY.

    ls_data-partner-header-object_task                  = c_insert.
    ls_data-partner-header-object_instance-bpartnerguid = lv_guid.
    ls_data-partner-header-object_instance-bpartner     = iv_lifnr.  " the employee code
    ls_data-vendor-header-object_task                   = c_insert.
    ls_data-vendor-header-object_instance-lifnr         = iv_lifnr.

    ls_data-partner-central_data-common-data-bp_control-category = '1'.  " person
    ls_data-partner-central_data-common-data-bp_control-grouping = lv_grp.
    LOOP AT lt_role INTO DATA(ls_role_db).
      DATA ls_role TYPE bus_ei_bupa_roles.
      CLEAR ls_role.
      ls_role-task     = c_insert.
      ls_role-data_key = ls_role_db-role.
      APPEND ls_role TO ls_data-partner-central_data-role-roles.
    ENDLOOP.

    " ---- the person: name, title, search term, language ------------------
    " First / middle / last name from HRMS; a file with the full name only
    " has its last word taken as the last name.
    DATA: lv_first TYPE string,
          lv_mid   TYPE string,
          lv_last  TYPE string.
    lv_first = condense( CONV string( is_file-fname ) ).
    lv_mid   = condense( CONV string( is_file-mname ) ).
    lv_last  = condense( CONV string( is_file-lname ) ).
    IF lv_last IS INITIAL.
      DATA(lv_full) = condense( CONV string( is_file-name1 ) ).
      IF lv_full IS INITIAL.
        add( EXPORTING iv_text = 'The employee has no name - a person needs a last name'
             CHANGING  ct      = et_error ).
        RETURN.
      ENDIF.
      SPLIT lv_full AT space INTO TABLE DATA(lt_word).
      DELETE lt_word WHERE table_line IS INITIAL.
      lv_last = lt_word[ lines( lt_word ) ].
      DELETE lt_word INDEX lines( lt_word ).
      IF lv_first IS INITIAL.
        lv_first = concat_lines_of( table = lt_word sep = ` ` ).
        CLEAR lv_mid.
      ENDIF.
    ENDIF.
    set( EXPORTING iv_comp = 'FIRSTNAME' iv_value = lv_first
         CHANGING  cs_data  = ls_data-partner-central_data-common-data-bp_person
                   cs_datax = ls_data-partner-central_data-common-datax-bp_person ).
    IF lv_mid IS NOT INITIAL.
      set( EXPORTING iv_comp = 'MIDDLENAME' iv_value = lv_mid
           CHANGING  cs_data  = ls_data-partner-central_data-common-data-bp_person
                     cs_datax = ls_data-partner-central_data-common-datax-bp_person ).
    ENDIF.
    set( EXPORTING iv_comp = 'LASTNAME' iv_value = lv_last
         CHANGING  cs_data  = ls_data-partner-central_data-common-data-bp_person
                   cs_datax = ls_data-partner-central_data-common-datax-bp_person ).

    " Language: the file has the ISO code (EN), the partner the SAP one (E).
    DATA lv_langu TYPE spras.
    IF strlen( condense( CONV string( is_file-langu ) ) ) = 2.
      CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
        EXPORTING  input            = is_file-langu
        IMPORTING  output           = lv_langu
        EXCEPTIONS unknown_language = 1
                   OTHERS           = 2.
      IF sy-subrc <> 0.
        CLEAR lv_langu.
      ENDIF.
    ELSE.
      lv_langu = is_file-langu.
    ENDIF.
    IF lv_langu IS NOT INITIAL.
      set( EXPORTING iv_comp = 'CORRESPONDLANGUAGE' iv_value = lv_langu
           CHANGING  cs_data  = ls_data-partner-central_data-common-data-bp_person
                     cs_datax = ls_data-partner-central_data-common-datax-bp_person ).
      set( EXPORTING iv_comp = 'PARTNERLANGUAGE' iv_value = lv_langu
           CHANGING  cs_data  = ls_data-partner-central_data-common-data-bp_centraldata
                     cs_datax = ls_data-partner-central_data-common-datax-bp_centraldata ).
    ENDIF.
    DATA(lv_title) = title_key( is_file-title_medi ).
    IF lv_title IS NOT INITIAL.
      set( EXPORTING iv_comp = 'TITLE_KEY' iv_value = lv_title
           CHANGING  cs_data  = ls_data-partner-central_data-common-data-bp_centraldata
                     cs_datax = ls_data-partner-central_data-common-datax-bp_centraldata ).
    ENDIF.
    IF is_file-sort1 IS NOT INITIAL.
      set( EXPORTING iv_comp = 'SEARCHTERM1' iv_value = is_file-sort1
           CHANGING  cs_data  = ls_data-partner-central_data-common-data-bp_centraldata
                     cs_datax = ls_data-partner-central_data-common-datax-bp_centraldata ).
    ENDIF.

    " ---- address and communication (XK01 screen 0111) --------------------
    DATA ls_adr TYPE bus_ei_bupa_address.
    ls_adr-task = c_insert.
    DATA(lt_pf) = VALUE string_table( ( |COUNTRY;{ is_file-country }| ) ( |REGION;{ is_file-region }| )
                                      ( |CITY;{ is_file-city1 }| ) ( |DISTRICT;{ is_file-city2 }| )
                                      ( |POSTL_COD1;{ is_file-post_code1 }| ) ( |STREET;{ is_file-street }| )
                                      ( |STR_SUPPL1;{ is_file-str_suppl1 }| )
                                      ( |STR_SUPPL2;{ is_file-str_suppl2 }| ) ( |LANGU;{ lv_langu }| ) ).
    LOOP AT lt_pf INTO DATA(lv_pf).
      SPLIT lv_pf AT ';' INTO DATA(lv_pname) DATA(lv_pval).
      CONDENSE lv_pval.
      IF lv_pval IS NOT INITIAL.
        set( EXPORTING iv_comp = lv_pname iv_value = lv_pval
             CHANGING  cs_data  = ls_adr-data-postal-data
                       cs_datax = ls_adr-data-postal-datax ).
      ENDIF.
    ENDLOOP.
    " Telephone (LFA1-TELF1) and mobile in ADR2: the first is the standard
    " number; R_3_USER 3 marks the standard mobile (SAP Help).
    DATA lv_std TYPE abap_bool VALUE abap_true.
    IF is_file-tel_number IS NOT INITIAL.
      DATA ls_tel TYPE bus_ei_bupa_telephone.
      CLEAR ls_tel.
      ls_tel-contact-task           = c_insert.
      ls_tel-contact-data-telephone = is_file-tel_number.
      ls_tel-contact-data-std_no    = lv_std.
      lv_std = abap_false.
      APPEND ls_tel TO ls_adr-data-communication-phone-phone.
    ENDIF.
    IF is_file-mob_number IS NOT INITIAL.
      CLEAR ls_tel.
      ls_tel-contact-task           = c_insert.
      ls_tel-contact-data-telephone = is_file-mob_number.
      ls_tel-contact-data-r_3_user  = '3'.
      ls_tel-contact-data-std_no    = lv_std.
      APPEND ls_tel TO ls_adr-data-communication-phone-phone.
    ENDIF.
    IF is_file-fax_number IS NOT INITIAL.
      DATA ls_fax TYPE bus_ei_bupa_fax.
      ls_fax-contact-task        = c_insert.
      ls_fax-contact-data-fax    = is_file-fax_number.
      ls_fax-contact-data-std_no = abap_true.
      APPEND ls_fax TO ls_adr-data-communication-fax-fax.
    ENDIF.
    IF is_file-smtp_addr IS NOT INITIAL.
      DATA ls_smt TYPE bus_ei_bupa_smtp.
      ls_smt-contact-task        = c_insert.
      ls_smt-contact-data-e_mail = is_file-smtp_addr.
      ls_smt-contact-data-std_no = abap_true.
      APPEND ls_smt TO ls_adr-data-communication-smtp-smtp.
    ENDIF.
    APPEND ls_adr TO ls_data-partner-central_data-address-addresses.

    " ---- bank details: on the business partner (XK01 screen 0130) ------
    IF is_file-bankl IS NOT INITIAL AND is_file-bankn IS NOT INITIAL.
      DATA ls_bk TYPE bus_ei_bupa_bankdetail.
      ls_bk-task     = c_insert.
      ls_bk-data_key = '0001'.
      set( EXPORTING iv_comp = 'BANK_CTRY'
                     iv_value = COND banks( WHEN is_file-banks IS NOT INITIAL
                                            THEN to_upper( is_file-banks ) ELSE is_file-country )
           CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
      set( EXPORTING iv_comp = 'BANK_KEY' iv_value = to_upper( is_file-bankl )
           CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
      set( EXPORTING iv_comp = 'BANK_ACCT' iv_value = is_file-bankn
           CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
      IF is_file-koinh IS NOT INITIAL.
        set( EXPORTING iv_comp = 'ACCOUNTHOLDER' iv_value = is_file-koinh
             CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
      ENDIF.
      APPEND ls_bk TO ls_data-partner-central_data-bankdetail-bankdetails.
    ENDIF.

    " ---- supplier: control data (XK01 screen 0120 / 0130) -------------
    DATA ls_vc TYPE vmds_ei_vmd_central.
    set( EXPORTING iv_comp = 'KTOKK' iv_value = is_file-ktokk
         CHANGING  cs_data = ls_vc-data cs_datax = ls_vc-datax ).
    IF is_file-konzs IS NOT INITIAL.
      set( EXPORTING iv_comp = 'KONZS' iv_value = to_upper( is_file-konzs )
           CHANGING  cs_data = ls_vc-data cs_datax = ls_vc-datax ).
    ENDIF.
    IF is_file-kunnr IS NOT INITIAL.
      set( EXPORTING iv_comp = 'KUNNR' iv_value = alpha( iv_in = is_file-kunnr iv_width = 10 )
           CHANGING  cs_data = ls_vc-data cs_datax = ls_vc-datax ).
    ENDIF.
    IF is_file-vbund IS NOT INITIAL.
      set( EXPORTING iv_comp = 'VBUND' iv_value = alpha( iv_in = is_file-vbund iv_width = 6 )
           CHANGING  cs_data = ls_vc-data cs_datax = ls_vc-datax ).
    ENDIF.
    IF is_file-fiskn IS NOT INITIAL.
      set( EXPORTING iv_comp = 'FISKN' iv_value = alpha( iv_in = is_file-fiskn iv_width = 10 )
           CHANGING  cs_data = ls_vc-data cs_datax = ls_vc-datax ).
    ENDIF.
    IF is_file-lnrza IS NOT INITIAL.
      set( EXPORTING iv_comp = 'LNRZA' iv_value = alpha( iv_in = is_file-lnrza iv_width = 10 )
           CHANGING  cs_data = ls_vc-data cs_datax = ls_vc-datax ).
    ENDIF.
    ls_data-vendor-central_data-central = ls_vc.

    " ---- supplier: company code (XK01 screens 0210 / 0215) -------------
    IF is_file-bukrs IS NOT INITIAL.
      DATA ls_cc TYPE vmds_ei_company.
      ls_cc-task           = c_insert.
      ls_cc-data_key-bukrs = is_file-bukrs.
      DATA(lt_cf) = VALUE string_table(
        ( |AKONT;{ alpha( iv_in = is_file-akont iv_width = 10 ) }| )
        ( |FDGRV;{ alpha( iv_in = is_file-fdgrv iv_width = 10 ) }| )
        ( |ALTKN;{ is_file-altkn }| ) ( |PERNR;{ is_file-pernr }| ) ( |ZTERM;{ is_file-zterm }| )
        ( |REPRF;{ is_file-reprf }| ) ( |ZWELS;{ is_file-zwels }| ) ( |ZAHLS;{ is_file-zahls }| )
        ( |HBKID;{ is_file-hbkid }| ) ( |ZGRUP;{ is_file-zgrup }| ) ).
      LOOP AT lt_cf INTO DATA(lv_cf).
        SPLIT lv_cf AT ';' INTO DATA(lv_cname) DATA(lv_cval).
        CONDENSE lv_cval.
        IF lv_cval IS NOT INITIAL AND NOT ( lv_cname = 'PERNR' AND lv_cval CO '0' ).
          set( EXPORTING iv_comp = lv_cname iv_value = lv_cval
               CHANGING  cs_data = ls_cc-data cs_datax = ls_cc-datax ).
        ENDIF.
      ENDLOOP.
      APPEND ls_cc TO ls_data-vendor-company_data-company.
    ENDIF.

    " ---- supplier: purchasing organisation (XK01 screen 0310) ----------
    IF is_file-ekorg IS NOT INITIAL.
      DATA ls_po TYPE vmds_ei_purchasing.
      ls_po-task           = c_insert.
      ls_po-data_key-ekorg = is_file-ekorg.
      DATA(lt_mf) = VALUE string_table(
        ( |WAERS;{ is_file-waers }| ) ( |ZTERM;{ is_file-zterm }| ) ( |INCO1;{ is_file-inco1 }| )
        ( |INCO2;{ is_file-inco2 }| ) ( |KALSK;{ is_file-kalsk }| ) ( |WEBRE;{ is_file-webre }| )
        ( |LEBRE;{ is_file-lebre }| ) ( |EKGRP;{ is_file-ekgrp }| ) ( |PLIFZ;{ is_file-plifz }| ) ).
      LOOP AT lt_mf INTO DATA(lv_mf).
        SPLIT lv_mf AT ';' INTO DATA(lv_mname) DATA(lv_mval).
        CONDENSE lv_mval.
        IF lv_mval IS NOT INITIAL AND NOT ( lv_mname = 'PLIFZ' AND lv_mval CO '0' ).
          set( EXPORTING iv_comp = lv_mname iv_value = lv_mval
               CHANGING  cs_data = ls_po-data cs_datax = ls_po-datax ).
        ENDIF.
      ENDLOOP.
      APPEND ls_po TO ls_data-vendor-purchasing_data-purchasing.
    ENDIF.

    " ---- validate, maintain, commit ---------------------------------------
    IF save( EXPORTING is_data = ls_data CHANGING ct_error = et_error ) = abap_false.
      RETURN.
    ENDIF.
    DATA: lv_new TYPE lifnr,
          lv_bp  TYPE bu_partner.
    SELECT SINGLE vendor FROM cvi_vend_link WHERE partner_guid = @lv_guid INTO @lv_new.
    SELECT SINGLE partner FROM but000 WHERE partner_guid = @lv_guid INTO @lv_bp.
    IF lv_new IS INITIAL.
      add( EXPORTING iv_text = COND string(
                       WHEN lv_bp IS INITIAL
                       THEN `Nothing was created: no business partner and no supplier exist for this employee`
                       ELSE |Business partner { lv_bp ALPHA = OUT } was saved, but no supplier is linked | &&
                            |to it - check the supplier roles of { is_file-ktokk } (CVIC_VEND_TO_BP2)| )
           CHANGING  ct      = et_error ).
      RETURN.
    ENDIF.
    APPEND VALUE bapiret2( type = 'S'
                           message = |Employee vendor { lv_new ALPHA = OUT } created as business | &&
                                     |partner { lv_bp ALPHA = OUT } (person)| ) TO et_ok.
  ENDMETHOD.

  METHOD change_bank.
    CLEAR: et_error, et_ok.
    DATA: ls_data TYPE cvis_ei_extern,
          lv_guid TYPE bu_partner_guid,
          lv_bp   TYPE bu_partner.
    SELECT SINGLE partner_guid FROM cvi_vend_link WHERE vendor = @iv_lifnr INTO @lv_guid.
    IF lv_guid IS INITIAL.
      add( EXPORTING iv_text = |Vendor { iv_lifnr ALPHA = OUT } has no business partner (CVI_VEND_LINK)|
           CHANGING  ct      = et_error ).
      RETURN.
    ENDIF.
    SELECT SINGLE partner FROM but000 WHERE partner_guid = @lv_guid INTO @lv_bp.
    ls_data-partner-header-object_task                  = c_update.
    ls_data-partner-header-object_instance-bpartnerguid = lv_guid.
    ls_data-vendor-header-object_task                   = c_update.
    ls_data-vendor-header-object_instance-lifnr         = iv_lifnr.

    DATA: lv_banks TYPE banks,
          lv_bankl TYPE bankk,
          lv_bankn TYPE bankn.
    lv_banks = to_upper( is_file-banks ).
    lv_bankl = to_upper( is_file-bankl ).
    lv_bankn = is_file-bankn.
    IF lv_banks IS INITIAL.
      SELECT SINGLE land1 FROM lfa1 WHERE lifnr = @iv_lifnr INTO @lv_banks.
    ENDIF.
    IF lv_bankl IS INITIAL OR lv_bankn IS INITIAL.
      add( EXPORTING iv_text = 'The file has no complete bank account (IFSC and account number)'
           CHANGING  ct      = et_error ).
      RETURN.
    ENDIF.

    " The partner's bank details. FK02 typed over line 1; here the currently
    " valid line with the lowest ID takes the HRMS account in place.
    " Bank details are time-dependent in this system (BUT0BK in V_TB056):
    " a change sent without its validity is applied as of today - SAP ends
    " the line and opens a new ID with the new bank, and the partner shows
    " two lines (60451: 0001 HDFC0009209 and 0002 HDFC0009229, CRQ,
    " 23.11.2026). So the change is sent as time-dependent, for the line's
    " own validity period, and lines that are no longer valid are left out.
    DATA lv_now TYPE timestamp.
    GET TIME STAMP FIELD lv_now.
    SELECT bkvid, banks, bankl, bankn, bk_valid_from, bk_valid_to FROM but0bk
      WHERE partner = @lv_bp
      ORDER BY bkvid INTO TABLE @DATA(lt_old).
    DELETE lt_old WHERE ( bk_valid_from IS NOT INITIAL AND bk_valid_from > lv_now )
                     OR ( bk_valid_to   IS NOT INITIAL AND bk_valid_to   < lv_now ).
    IF line_exists( lt_old[ banks = lv_banks bankl = lv_bankl bankn = lv_bankn ] ).
      APPEND VALUE bapiret2( type = 'I' message = 'The bank account is already maintained' ) TO et_ok.
      RETURN.
    ENDIF.

    DATA ls_bk TYPE bus_ei_bupa_bankdetail.
    DATA lv_bkvid TYPE bu_bkvid.
    IF lt_old IS NOT INITIAL.
      DATA(ls_line) = lt_old[ 1 ].
      lv_bkvid       = ls_line-bkvid.
      ls_bk-task     = c_update.
      ls_bk-data_key = lv_bkvid.
      ls_data-partner-central_data-bankdetail-time_dependent = abap_true.
      DATA lv_date TYPE d.
      IF ls_line-bk_valid_from IS NOT INITIAL.
        CONVERT TIME STAMP ls_line-bk_valid_from TIME ZONE 'UTC' INTO DATE lv_date.
        IF lv_date IS NOT INITIAL.
          set( EXPORTING iv_comp = 'BANKDETAILVALIDFROM' iv_value = lv_date
               CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
        ENDIF.
      ENDIF.
      IF ls_line-bk_valid_to IS NOT INITIAL.
        CLEAR lv_date.
        CONVERT TIME STAMP ls_line-bk_valid_to TIME ZONE 'UTC' INTO DATE lv_date.
        IF lv_date IS NOT INITIAL.
          set( EXPORTING iv_comp = 'BANKDETAILVALIDTO' iv_value = lv_date
               CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
        ENDIF.
      ENDIF.
    ELSE.
      " No valid bank line: a new one after the highest ID in use.
      SELECT MAX( bkvid ) FROM but0bk WHERE partner = @lv_bp INTO @DATA(lv_max).
      lv_bkvid       = COND #( WHEN lv_max IS INITIAL OR lv_max CN '0123456789' THEN '0001'
                               ELSE |{ CONV i( lv_max ) + 1 WIDTH = 4 ALIGN = RIGHT PAD = '0' }| ).
      ls_bk-task     = c_insert.
      ls_bk-data_key = lv_bkvid.
    ENDIF.
    set( EXPORTING iv_comp = 'BANK_CTRY' iv_value = lv_banks
         CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
    set( EXPORTING iv_comp = 'BANK_KEY' iv_value = lv_bankl
         CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
    set( EXPORTING iv_comp = 'BANK_ACCT' iv_value = lv_bankn
         CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
    IF is_file-koinh IS NOT INITIAL.
      set( EXPORTING iv_comp = 'ACCOUNTHOLDER' iv_value = is_file-koinh
           CHANGING  cs_data = ls_bk-data cs_datax = ls_bk-datax ).
    ENDIF.
    APPEND ls_bk TO ls_data-partner-central_data-bankdetail-bankdetails.

    IF save( EXPORTING is_data = ls_data CHANGING ct_error = et_error ) = abap_false.
      RETURN.
    ENDIF.

    " Read back: the account must now be on that bank details ID. If SAP
    " opened another ID instead, the log says so with both lines.
    GET TIME STAMP FIELD lv_now.
    SELECT bkvid, banks, bankl, bankn, bk_valid_from, bk_valid_to FROM but0bk
      WHERE partner = @lv_bp
      ORDER BY bkvid INTO TABLE @DATA(lt_new).
    IF NOT line_exists( lt_new[ bkvid = lv_bkvid banks = lv_banks bankl = lv_bankl bankn = lv_bankn ] ).
      DATA(lv_lines) = ``.
      LOOP AT lt_new INTO DATA(ls_new).
        lv_lines = lv_lines && | { ls_new-bkvid } { ls_new-bankl } { ls_new-bankn }| &&
                   COND string( WHEN ls_new-bk_valid_to IS NOT INITIAL AND ls_new-bk_valid_to < lv_now
                                THEN ` (no longer valid);` ELSE `;` ).
      ENDLOOP.
      APPEND VALUE bapiret2( type = 'S'
                             message = |Bank account saved, but not on bank details ID { lv_bkvid } - | &&
                                       |partner { lv_bp ALPHA = OUT } has:{ lv_lines }| ) TO et_ok.
      RETURN.
    ENDIF.
    APPEND VALUE bapiret2( type = 'S'
                           message = 'Employee vendor Bank details are updated as per HRMS data' ) TO et_ok.
  ENDMETHOD.

ENDCLASS.

*---------------------------------------------------------------------*
* I N I T I A L I Z A T I O N                                         *
*---------------------------------------------------------------------*
INITIALIZATION.
  PERFORM hide_sel_opt_interval.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name = co_pfile.
      screen-input = 0.
      screen-output = 1.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*---------------------------------------------------------------------*
* S T A R T - O F - S E L E C T I O N                                 *
*---------------------------------------------------------------------*
START-OF-SELECTION.
  IF p_bkey EQ abap_true.
    SUBMIT ZFFI_BANKKEY_CREATE WITH s_to = s_to
                               WITH s_cc = s_cc
                               AND RETURN.
  ELSE.
    PERFORM fetch_files.                        "Added by Aishwarya - 8000003767 - 04.04.2025
    PERFORM f_read_n_run.
    PERFORM f_write_log.                        " S/4HANA conversion: log in the spool
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_READ_N_RUN
*&---------------------------------------------------------------------*
*       text TO FETCH THE DATA FROM FILE ON APPLICATION SERVER AND
*            EXECUTE BDC
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_read_n_run.
  DATA: lwa_line       TYPE string,
        lv_path        TYPE rcgiedial-iefile,
        lv_file        TYPE rcgiedial-iefile,
        lv_filename    TYPE rcgiedial-iefile,
        lv_filecount   TYPE p,
        lv_arcpath     TYPE rcgiedial-iefile,
        lta_files      TYPE TABLE OF epsfili,
        lwa_files      TYPE epsfili,
        lv_len         TYPE p,
        lv_header_skip TYPE abap_bool VALUE '0'. " Flag to skip the header line

  CONSTANTS:
    lc_ext     TYPE char4 VALUE '.csv',                  "'.csv',
    lc_extstr1 TYPE char3 VALUE 'csv',                   "'csv',
    lc_extstr2 TYPE char3 VALUE 'CSV'.                   "'CSV'.
*  break abap4. "added by aasif  on 07.01.2025
  IF sy-batch = abap_false.  " commented by SM for CR3767
    MESSAGE text-004 TYPE co_msgtyp.
    LEAVE LIST-PROCESSING.
  ELSE.

    "" FETCH FILE PATH FROM PARAM TABLE
    SELECT SINGLE param_name FROM zparam_table INTO lv_path WHERE id_name EQ co_id_path AND id_number EQ co_id_num1.
    IF sy-subrc = 0 AND lv_path IS NOT INITIAL.

      CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
        EXPORTING
          dir_name               = CONV epsf-epsdirnam( lv_path )
        TABLES
          dir_list               = lta_files
        EXCEPTIONS
          invalid_eps_subdir     = 1
          sapgparam_failed       = 2
          build_directory_failed = 3
          no_authorization       = 4
          read_directory_failed  = 5
          too_many_read_errors   = 6
          empty_directory_list   = 7
          OTHERS                 = 8.

      LOOP AT lta_files INTO lwa_files.         " FOR MULTIPLE FILES TO BE PROCESSED INDIVIDUALLY
        IF NOT ( lwa_files-name CP co_file ).
          CONTINUE.
        ENDIF.

        lv_filecount = lv_filecount + 1.

        "" REFRESH DATA FOR NEW LOOP PASS
        CLEAR: lwa_line, lv_file, lv_filename, lv_arcpath, lv_len, ta_msg, ta_file.
        lv_header_skip = '0'.          " S/4HANA conversion: every file has its header

        "" CHECK THE EXTENSION AND ALLOW VALID EXTENSION FILES
        SPLIT lwa_files-name AT '.' INTO DATA(lv_1) DATA(lv_2).

        IF lv_2 IS NOT INITIAL.

          "" SKIP THE FILE IF NOT TEXT FILE
          IF lv_2 NE lc_extstr1 AND lv_2 NE lc_extstr2.
            CONTINUE.
          ENDIF.

        ELSE.
          "" SKIP THE FILE IF NO EXTENSION
          CONTINUE.
        ENDIF.

*        LV_LEN = STRLEN( LV_FILE ) - 3.
*        IF LV_FILE+LV_LEN(3) NE LC_EXTSTR1 AND LV_FILE+LV_LEN(3) NE LC_EXTSTR2.                   "" IF NO EXTENSION THEN ADD IT
*          CONCATENATE LV_FILE LC_EXT INTO LV_FILE.
*        ENDIF.

        lv_file = lwa_files-name.

        lv_filename = lv_file.
        CONCATENATE lv_path lv_file INTO lv_file.

        OPEN DATASET lv_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.
        IF sy-subrc = 0.
          DO.
            READ DATASET lv_file INTO lwa_line.
            IF sy-subrc EQ 0.
*   BOC BY Aishwarya - 8000003767 - 08.04.2025
              " S/4HANA conversion: no carriage return at the line end (a
              " Windows file on the Linux server), no empty lines.
              REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf(1) IN lwa_line WITH ``.
              IF lwa_line IS INITIAL.
                CONTINUE.
              ENDIF.
              " Skip the first line (header) if not already skipped
              IF lv_header_skip EQ '0'.
                lv_header_skip = '1'. " Set flag to skip the next line
                PERFORM f_header USING lwa_line.   " which layout the file has
                CONTINUE. " Skip processing for header line
              ENDIF.
              IF gv_layout = co_lay_hrms.
                PERFORM f_convert_hrms USING lwa_line CHANGING wa_file.
              ELSE.
                PERFORM f_convert_wa USING lwa_line CHANGING wa_file.
              ENDIF.
*   EOC BY Aishwarya - 8000003767 - 08.04.2025
              APPEND wa_file TO ta_file.
            ELSE.
              EXIT.
            ENDIF.
          ENDDO.

          "" CLOSE THE FILE
          CLOSE DATASET lv_file.
          APPEND |File { lv_file }: { lines( ta_file ) } employee rows, layout | &&
                 COND string( WHEN gv_layout = co_lay_hrms THEN `HRMS` ELSE `50 columns` ) TO gt_run.

          "" ARCHIVE FILE
          lv_arcpath = lv_path.
          PERFORM f_get_archive_path CHANGING lv_arcpath.

          CONCATENATE lv_arcpath lv_filename INTO lv_arcpath.

          PERFORM f_archive USING lv_file lv_arcpath.

        ELSE.
          wa_msg-remarks = text-005.
          APPEND wa_msg TO ta_msg.
          CLEAR: wa_msg.
        ENDIF.

        IF ta_msg IS INITIAL.
          PERFORM f_runbdc.

          IF ta_msg IS NOT INITIAL.
            PERFORM f_mail_log USING lv_filename.
          ENDIF.
        ELSE.
          APPEND LINES OF ta_msg TO gt_log.
          PERFORM f_mail_log USING lv_filename.
        ENDIF.
      ENDLOOP.
      IF lv_filecount = 0.
        APPEND |No employee file ({ co_file }) in { lv_path }| TO gt_run.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_RUNBDC
*&---------------------------------------------------------------------*
*       text TO EXECUTE BDC WITH DATA FROM FILE READ
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_runbdc.
  DATA lv_key TYPE lifnr.
  LOOP AT ta_file INTO wa_file.
    " S/4HANA conversion: the DPR / tax invoice vendors in TA_CREATE and
    " TA_UPDATE have no leading zeros; HRMS writes some employee codes with
    " them (01047) - compare without, or such an employee is never found.
    lv_key = wa_file-lifnr.
    SHIFT lv_key LEFT DELETING LEADING '0'.

*   BOC BY Aishwarya - 8000003767 - 07.04.2025
    " S/4HANA conversion: no batch input tables to refresh any more.

    IF ta_create IS NOT INITIAL OR ta_update IS NOT INITIAL.
      READ TABLE ta_create INTO wa_create WITH KEY lifnr = lv_key BINARY SEARCH.
      IF sy-subrc = 0.
        gv_to_create = gv_to_create + 1.
        wa_file-lifnr = wa_create-lifnr.
        lv_status = co_create.
*   EOC BY Aishwarya - 8000003767 - 07.04.2025

        " S/4HANA: was CALL TRANSACTION 'XK01' - the employee vendor is created
        " as a business partner (a person).
        PERFORM f_bp_create.

*   BOC BY Aishwarya - 8000003767 - 09.04.2025
      ELSEIF ta_update IS NOT INITIAL OR ta_create IS NOT INITIAL.
        READ TABLE ta_update INTO wa_update WITH KEY lifnr = lv_key BINARY SEARCH.
        wa_update-lifnr = |{ wa_update-lifnr ALPHA = IN }|.
        IF sy-subrc = 0.
          gv_to_update = gv_to_update + 1.
          READ TABLE ta_lfbk INTO DATA(wa_lfbk) WITH KEY lifnr = wa_update-lifnr banks = wa_file-banks bankl = wa_file-bankl bankn = wa_file-bankn BINARY SEARCH.
          IF sy-subrc <> 0.
            lv_status = co_update.

*            lv_bankl = wa_file-bankl.
*            lv_banka = lv_bankl(4).

*   BOC BY Aishwarya - 8000003767 - 16.04.2025
            " S/4HANA: was CALL TRANSACTION 'FK02' - the bank account is put on
            " the business partner.
            PERFORM f_bp_update USING wa_update-lifnr.

*          PERFORM send_email.                                                               "Commented BY Aishwarya - 8000003767 - 07.04.2025
*          REFRESH: ta_bdcdata[], ta_mess[], ta_mess_update[], ta_bdcdata_update[].
            CLEAR : wa_create, wa_lfbk, wa_update, lv_status, lv_bankl, lv_banka.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

  " S/4HANA conversion: once after the loop - TA_MSG / TA_MSG_UPDATE gather
  " the whole run; copied on every one of ~26,800 rows before.
  APPEND LINES OF ta_msg TO ta_msg_new.
  APPEND LINES OF ta_msg_update TO ta_msg_new.
  APPEND LINES OF ta_msg TO gt_log.
  APPEND LINES OF ta_msg_update TO gt_log.

  SORT ta_msg_new BY emp_no.
  DELETE ADJACENT DUPLICATES FROM ta_msg_new COMPARING emp_no.

  IF ta_msg_new IS NOT INITIAL.
    PERFORM send_email_new.
  ENDIF.

*   EOC BY Aishwarya - 8000003767 - 07.04.2025

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_CONVERT_WA
*&---------------------------------------------------------------------*
*       text MAP APPLICATION SERVER FILE ROW IN WORK AREA FIELDS
*----------------------------------------------------------------------*
FORM f_convert_wa USING uwa_line CHANGING cwa_file LIKE wa_file.

  DATA: lv_val1  TYPE char100,
        lv_val2  TYPE char100,
        lv_val3  TYPE char100,
        lv_val4  TYPE char100,
        lv_val5  TYPE char100,
        lv_val6  TYPE char100,
        lv_val7  TYPE char100,
        lv_val8  TYPE char100,
        lv_val9  TYPE char100,
        lv_val10 TYPE char100,
        lv_val11 TYPE char100,
        lv_val12 TYPE char100,
        lv_val13 TYPE char100,
        lv_val14 TYPE char100,
        lv_val15 TYPE char100,
        lv_val16 TYPE char100,
        lv_val17 TYPE char100,
        lv_val18 TYPE char100,
        lv_val19 TYPE char100,
        lv_val20 TYPE char100,
        lv_val21 TYPE char100,
        lv_val22 TYPE char100,
        lv_val23 TYPE char100,
        lv_val24 TYPE char100,
        lv_val25 TYPE char100,
        lv_val26 TYPE char100,
        lv_val27 TYPE char100,
        lv_val28 TYPE char100,
        lv_val29 TYPE char100,
        lv_val30 TYPE char100,
        lv_val31 TYPE char100,
        lv_val32 TYPE char100,
        lv_val33 TYPE char100,
        lv_val34 TYPE char100,
        lv_val35 TYPE char100,
        lv_val36 TYPE char100,
        lv_val37 TYPE char100,
        lv_val38 TYPE char100,
        lv_val39 TYPE char100,
        lv_val40 TYPE char100,
        lv_val41 TYPE char100,
        lv_val42 TYPE char100,
        lv_val43 TYPE char100,
        lv_val44 TYPE char100,
        lv_val45 TYPE char100,
        lv_val46 TYPE char100,
        lv_val47 TYPE char100,
        lv_val48 TYPE char100,
        lv_val49 TYPE char100,
        lv_val50 TYPE char100.

  SPLIT uwa_line AT '#'                                                                           "Changed BY Aishwarya - 8000003767 - 24.04.2025
  INTO lv_val1  lv_val2  lv_val3  lv_val4  lv_val5  lv_val6  lv_val7  lv_val8  lv_val9  lv_val10
       lv_val11 lv_val12 lv_val13 lv_val14 lv_val15 lv_val16 lv_val17 lv_val18 lv_val19 lv_val20
       lv_val21 lv_val22 lv_val23 lv_val24 lv_val25 lv_val26 lv_val27 lv_val28 lv_val29 lv_val30
       lv_val31 lv_val32 lv_val33 lv_val34 lv_val35 lv_val36 lv_val37 lv_val38 lv_val39 lv_val40
       lv_val41 lv_val42 lv_val43 lv_val44 lv_val45 lv_val46 lv_val47 lv_val48 lv_val49 lv_val50.

  cwa_file-lifnr      = lv_val1  .
  cwa_file-bukrs      = lv_val2  .
  cwa_file-ekorg      = lv_val3  .
  cwa_file-ktokk      = lv_val4  .
  cwa_file-title_medi = lv_val5  .
  cwa_file-name1      = lv_val6  .
  cwa_file-name2      = lv_val7  .
  cwa_file-sort1      = lv_val8  .
  cwa_file-str_suppl1 = lv_val9  .
  cwa_file-str_suppl2 = lv_val10 .
  cwa_file-street     = lv_val11 .
  cwa_file-city2      = lv_val12 .
  cwa_file-post_code1 = lv_val13 .
  cwa_file-city1      = lv_val14 .
  cwa_file-country    = lv_val15 .
  cwa_file-region     = lv_val16 .
  cwa_file-langu      = lv_val17 .
  cwa_file-tel_number = lv_val18 .
  cwa_file-mob_number = lv_val19 .
  cwa_file-fax_number = lv_val20 .
  cwa_file-smtp_addr  = lv_val21 .
  cwa_file-kunnr      = lv_val22 .
  cwa_file-vbund      = lv_val23 .
  cwa_file-konzs      = lv_val24 .
  cwa_file-fiskn      = lv_val25 .
  cwa_file-lnrza      = lv_val26 .
  cwa_file-banks      = lv_val27 .
  cwa_file-bankl      = lv_val28 .
  cwa_file-bankn      = lv_val29 .
  cwa_file-koinh      = lv_val30 .
  cwa_file-akont      = lv_val31 .
  cwa_file-fdgrv      = lv_val32 .
  cwa_file-altkn      = lv_val33 .
  cwa_file-pernr      = lv_val34 .
  cwa_file-zterm      = lv_val35 .
  cwa_file-reprf      = lv_val36 .
* cwa_file-zwels      = lv_val37 .
  " S/4HANA conversion: payment method CNT for every employee vendor, as
  " the mapping sheet "expected output file for upload" has it - the file
  " sends only C (CRQ, 23.11.2026).
  cwa_file-zwels      = co_hrms_zwels.
  cwa_file-zahls      = lv_val38 .
  cwa_file-hbkid      = lv_val39 .
  cwa_file-zgrup      = lv_val40 .
  cwa_file-waers      = lv_val41 .
  cwa_file-inco1      = lv_val42 .
  cwa_file-inco2      = lv_val43 .
  cwa_file-kalsk      = lv_val44 .
  cwa_file-webre      = lv_val45 .
  cwa_file-lebre      = lv_val46 .
  cwa_file-ekgrp      = lv_val47 .
  cwa_file-plifz      = lv_val48 .
  cwa_file-j_1icstno  = lv_val49 .
  cwa_file-j_1ilstno  = lv_val50 .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form F_GET_ARCHIVE_PATH
*&---------------------------------------------------------------------*
*       text TO GET THE ARCHIVE FOLDER PATH ON APPLICATION SEREVR
*----------------------------------------------------------------------*
FORM f_get_archive_path CHANGING c_arcpath.
  " Code Remediation S4 AL11 file path change **BEGIN OF CHANGE BY ABAP7 25.10.2026
* CONSTANTS: lc_archive_folder TYPE char20 VALUE '\Archive\'.
* DATA: lv_cnt   TYPE p,
*       lv_whcnt TYPE p,
*       lv_len   TYPE p,
*       lv_level TYPE p,
*       lv_check TYPE p,
*       lv_str   TYPE char100 VALUE '0\'.
*
* lv_len = strlen( c_arcpath ).
*
* WHILE ( lv_whcnt <> lv_len ).
*   IF c_arcpath+lv_whcnt(1) EQ '\'.
*     lv_level = lv_level + 1.
*   ENDIF.
  CONSTANTS: lc_archive_folder TYPE char20 VALUE '/Archive/'.
  DATA: lv_cnt   TYPE p,
        lv_whcnt TYPE p,
        lv_len   TYPE p,
        lv_level TYPE p,
        lv_check TYPE p,
        lv_str   TYPE char100 VALUE '0/'.

  lv_len = strlen( c_arcpath ).

  WHILE ( lv_whcnt <> lv_len ).
    IF c_arcpath+lv_whcnt(1) EQ '/'.
      lv_level = lv_level + 1.
    ENDIF.
  " Code Remediation S4 AL11 file path change **END OF CHANGE BY ABAP7 25.10.2026

    lv_whcnt = lv_whcnt + 1.
  ENDWHILE.

  lv_level = lv_level - 1.

  CLEAR: lv_whcnt.
  WHILE ( lv_whcnt <> lv_len ).
    " Code Remediation S4 AL11 file path change **BEGIN OF CHANGE BY ABAP7 25.10.2026
*   IF c_arcpath+lv_whcnt(1) EQ '\'.
*     lv_check = lv_check + 1.
*
*     IF lv_level = lv_check.
*       CONCATENATE c_arcpath+0(lv_whcnt) lc_archive_folder INTO lv_str.
*       EXIT.
*     ENDIF.
*   ENDIF.
    IF c_arcpath+lv_whcnt(1) EQ '/'.
      lv_check = lv_check + 1.

      IF lv_level = lv_check.
        CONCATENATE c_arcpath+0(lv_whcnt) lc_archive_folder INTO lv_str.
        EXIT.
      ENDIF.
    ENDIF.
    " Code Remediation S4 AL11 file path change **END OF CHANGE BY ABAP7 25.10.2026

    lv_whcnt = lv_whcnt + 1.
  ENDWHILE.

  "" PASS ARCHIVE DIRECTORY PATH
  c_arcpath = lv_str.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form F_GET_ARCHIVE_PATH
*&---------------------------------------------------------------------*
*       text TO ARCHIVE THE FILE(ON APPL. SERVER) ONCE USED IN PROGRAM
*----------------------------------------------------------------------*
FORM f_archive USING u_file u_arcpath.
  CALL FUNCTION 'ARCHIVFILE_SERVER_TO_SERVER'
    EXPORTING
      sourcepath       = CONV sapb-sappfad( u_file )
      targetpath       = CONV sapb-sappfad( u_arcpath )
    EXCEPTIONS
      error_file       = 1
      no_authorization = 2
      OTHERS           = 3.

  DELETE DATASET u_file.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MAIL_LOG
*&---------------------------------------------------------------------*
*       text MAIL MESSAGE LOG TO EMAIL
*----------------------------------------------------------------------*
FORM f_mail_log USING u_subject.

  DATA: lwa_vl_doc       TYPE sodocchgi1,
        lta_packing_list TYPE STANDARD TABLE OF sopcklsti1 WITH HEADER LINE,
        lta_receivers    TYPE STANDARD TABLE OF somlreci1 WITH HEADER LINE,
        lta_body         TYPE TABLE OF solisti1 WITH HEADER LINE,
        lv_email         TYPE char100,
        lv_cc            TYPE char100,
        lv_param         TYPE char100,
        lv_str           TYPE char20,
        lv_body_cnt1     TYPE p,
        lv_start         TYPE p,
        lv_end           TYPE p.

  CONSTANTS: lc_dc_htm      TYPE sopcklsti1-doc_type    VALUE 'HTM',
             lc_dc_xls      TYPE sopcklsti1-doc_type    VALUE 'XLS',
             lc_dc_txt      TYPE sopcklsti1-doc_type    VALUE 'TXT',              "CSV',     "'
             lc_vl_obj      TYPE sodocchgi1-obj_name    VALUE 'SAPRPT',
             lc_sensi       TYPE sodocchgi1-sensitivty  VALUE 'F',
             lc_pl_obj1     TYPE sopcklsti1-obj_name    VALUE 'INPUT_FILE',
             lc_pl_objdesc1 TYPE sopcklsti1-obj_descr   VALUE 'Input File',
             lc_pl_obj2     TYPE sopcklsti1-obj_name    VALUE 'MESSAGELOG',
             lc_pl_objdesc2 TYPE sopcklsti1-obj_descr   VALUE 'Message Log',
             lc_rectype     TYPE somlreci1-rec_type     VALUE 'U',
             lc_comtype     TYPE somlreci1-com_type     VALUE 'INT',
             lc_field1      TYPE char20                 VALUE 'Emp No.',
             lc_field2      TYPE char20                 VALUE 'Message'.

** START OF MESSAGE LOG ATTACHMENT PROCESSING
  CLEAR: ta_contents_bin, ta_contents_bin[].

  "" INPUT FILE
  LOOP AT ta_file INTO wa_file.
    PERFORM f_file_item USING sy-tabix.
  ENDLOOP.

  "" MESSAGE LOF FILE
  CONCATENATE lc_field1
              lc_field2
  INTO ta_contents_bin-line SEPARATED BY cl_abap_char_utilities=>horizontal_tab.            "SEPARATED BY TAB

  CONCATENATE ta_contents_bin cl_abap_char_utilities=>cr_lf INTO ta_contents_bin.           "LINE ENDED BY ENTER
  APPEND ta_contents_bin.

  LOOP AT ta_msg INTO wa_msg.
    CONCATENATE wa_msg-emp_no
                wa_msg-remarks
    INTO ta_contents_bin-line SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

    CONCATENATE ta_contents_bin cl_abap_char_utilities=>cr_lf INTO ta_contents_bin.
    APPEND ta_contents_bin.
  ENDLOOP.

** END OF MESSAGE LOG ATTACHMENT PROCESSING

** MAIL BODY
  lta_body-line = text-ml1.
  APPEND lta_body. CLEAR: lta_body.

  lta_body-line = text-ml2.
  APPEND lta_body. CLEAR: lta_body.

  lta_body-line = text-ml3.
  APPEND lta_body. CLEAR: lta_body.

** DOCUMENT DATA & SUBJECT
  lwa_vl_doc-doc_size     = 5000.
  lwa_vl_doc-obj_langu    = sy-langu.
  lwa_vl_doc-obj_name     = lc_vl_obj.
  lwa_vl_doc-sensitivty   = lc_sensi.
  lwa_vl_doc-obj_descr    = u_subject.

** PACKING LIST
  DESCRIBE TABLE lta_body LINES lta_packing_list-body_num.
  lta_packing_list-doc_type = lc_dc_htm.
  lta_packing_list-transf_bin = space.
  lta_packing_list-head_start = 1.
  lta_packing_list-body_start = 1.
  lta_packing_list-head_num = 0.
  APPEND lta_packing_list.
  CLEAR: lta_packing_list.

  IF ta_contents_bin[] IS NOT INITIAL.
    IF ta_file IS NOT INITIAL.
      DESCRIBE TABLE ta_file LINES lv_body_cnt1.

      lta_packing_list-doc_type   = lc_dc_txt.
      lta_packing_list-transf_bin = abap_true.
      lta_packing_list-head_start = 1.
      lta_packing_list-head_num   = 0.
      lta_packing_list-body_start = 1.
      lta_packing_list-body_num   = lv_body_cnt1.
      lta_packing_list-obj_name   = lc_pl_obj1.
      lta_packing_list-obj_descr  = u_subject.
      APPEND lta_packing_list.
      CLEAR: lta_packing_list.
    ENDIF.

    IF ta_msg IS NOT INITIAL.
      lv_start = lv_body_cnt1 + 1.
      DESCRIBE TABLE ta_contents_bin LINES lv_end.

      lta_packing_list-doc_type   = lc_dc_xls.
      lta_packing_list-transf_bin = abap_true.
      lta_packing_list-head_start = 1.
      lta_packing_list-head_num   = 0.
      lta_packing_list-body_start = lv_start.
      lta_packing_list-body_num   = lv_end.
      lta_packing_list-obj_name   = lc_pl_obj2.
      lta_packing_list-obj_descr  = lc_pl_objdesc2.
      APPEND lta_packing_list.
      CLEAR: lta_packing_list.
    ENDIF.
  ENDIF.

** RECEIVERS EMAIL
  SELECT SINGLE param_name FROM zparam_table INTO lv_email WHERE id_name EQ co_id_em AND id_number EQ co_id_num1.
  IF sy-subrc = 0 AND lv_email IS NOT INITIAL.
    lta_receivers-receiver = lv_email.
    lta_receivers-rec_type = lc_rectype.
    lta_receivers-com_type = lc_comtype.
    APPEND lta_receivers.
    CLEAR: lta_receivers.
  ENDIF.

  SELECT SINGLE param_name FROM zparam_table INTO lv_cc WHERE id_name EQ co_id_em AND id_number EQ co_id_num2.
  IF sy-subrc = 0 AND lv_cc IS NOT INITIAL.
    lta_receivers-receiver = lv_cc.
    lta_receivers-rec_type = lc_rectype.
    lta_receivers-com_type = lc_comtype.
    lta_receivers-copy     = abap_true.
    APPEND lta_receivers.
    CLEAR: lta_receivers.
  ENDIF.

  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = lwa_vl_doc
      put_in_outbox              = abap_true
      sender_address             = CONV soextreci1-receiver( sy-uname )
      sender_address_type        = CONV soextreci1-adr_typ( lc_rectype )                           "'B'
      commit_work                = abap_true
    TABLES
      packing_list               = lta_packing_list
      contents_bin               = ta_contents_bin               "LTA_TEXT                      "
      contents_txt               = lta_body
      receivers                  = lta_receivers
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc <> 0.
    MESSAGE text-006 TYPE co_msgtyp.
  ENDIF.
ENDFORM.

FORM f_file_item USING u_tabix.
  CONSTANTS: lc_sep TYPE char1 VALUE '#'.         "','.

  DATA: lv_cnt      TYPE p,
        lv_str      TYPE char30,
        lv_datatype TYPE char30.

  FIELD-SYMBOLS: <fs_wa>  LIKE wa_file,
                 <fs_val>.

  CLEAR: ta_contents_bin.

  DO.
    lv_cnt = lv_cnt + 1.

    ASSIGN COMPONENT lv_cnt OF STRUCTURE wa_file TO <fs_val>.
    IF <fs_val> IS ASSIGNED.
      lv_str = CONV #( <fs_val> ). CONDENSE lv_str.
      CONCATENATE ta_contents_bin lv_str INTO ta_contents_bin SEPARATED BY lc_sep.
    ELSE.
      SHIFT ta_contents_bin LEFT DELETING LEADING lc_sep.
      IF u_tabix NE 1.
        CONCATENATE cl_abap_char_utilities=>cr_lf ta_contents_bin INTO ta_contents_bin.
      ENDIF.
      CONDENSE ta_contents_bin.
      APPEND ta_contents_bin.
      EXIT.
    ENDIF.

    UNASSIGN: <fs_val>.
  ENDDO.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM send_email.
*  IF ta_msg IS NOT INITIAL.
*
*    CLASS cl_abap_char_utilities DEFINITION LOAD.
*
*    CONSTANTS: con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
*               con_cret TYPE c VALUE cl_abap_char_utilities=>cr_lf.
*
*    DATA:   it_attach TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
*                    WITH HEADER LINE.
*
*    DATA:   it_message TYPE STANDARD TABLE OF solisti1 INITIAL SIZE 0
*                    WITH HEADER LINE.
*
*    DATA: t_packing_list  LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
*          t_contents      LIKE solisti1 OCCURS 0 WITH HEADER LINE,
*          t_receivers     LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
*          t_attachment    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
*          t_object_header LIKE solisti1 OCCURS 0 WITH HEADER LINE,
*          w_cnt           TYPE i,
*          w_sent_all(1)   TYPE c,
*          w_doc_data      LIKE sodocchgi1,
*          gd_error        TYPE sy-subrc,
*          gd_reciever     TYPE sy-subrc.
*
*    DATA : lv_to TYPE string,
*           lv_cc TYPE string.
*
*    TYPES : BEGIN OF ty_zparam_table,
*              id_name    TYPE zparam_table-id_name,
*              id_number  TYPE zparam_table-id_number,
*              param_name TYPE zparam_table-param_name,
*            END OF ty_zparam_table.
*
*    DATA : ta_param TYPE TABLE OF ty_zparam_table,
*           wa_param TYPE          ty_zparam_table.
*
*    DATA : ta_param1 TYPE TABLE OF ty_zparam_table,
*           wa_param1 TYPE          ty_zparam_table.
*
*    CONSTANTS : c_emp1 TYPE zparam_table-id_name VALUE 'ZFI_EMPEM1',
*                c_emp2 TYPE zparam_table-id_name VALUE 'ZFI_EMPEM2'.
*
*
*    """Load Heading in EXCEL Sheet
*    it_attach-line = |{ it_attach-line }Employee No{ con_tab }|.
*    it_attach-line = |{ it_attach-line }Message Description{ con_tab }|.
*    it_attach-line = |{ con_cret }{ it_attach-line }|.
*    APPEND it_attach.
*    CLEAR it_attach.
*
*    """Load Items in Excel Sheet.
*    LOOP AT ta_msg INTO wa_msg.
*      it_attach-line = |{ it_attach-line }{ wa_msg-emp_no }{ con_tab }|.
*      it_attach-line = |{ it_attach-line }{ wa_msg-remarks }{ con_tab }|.
*
*      it_attach-line = |{ con_cret }{ it_attach-line }|.
*      APPEND it_attach.
*      CLEAR it_attach.
*    ENDLOOP.
*
*    """Mail Body Context
*    it_message = 'Dear Sir/Madam'.
*    APPEND it_message.
*
*    it_message = ''.
*    APPEND it_message.
*
*    it_message = 'Please Find Log message.'.
*    APPEND it_message.
*
*    it_message = ''.
*    APPEND it_message.
*
*    it_message = ''.
*    APPEND it_message.
*
*    it_message = 'Thanks & Regards,'.
*    APPEND it_message.
*
*    it_message = ''.
*    APPEND it_message.
*
** Fill the document data.
*    w_doc_data-doc_size = 1.
*
** Populate the subject/generic message attributes
*    w_doc_data-obj_langu = sy-langu.
*    w_doc_data-obj_name  = 'SAPRPT'.
**    w_doc_data-obj_descr = 'EMP UPLD Report'.                "Commented By Aishwarya - 8000003767 - 07.04.2025
*    w_doc_data-obj_descr = 'Employee Vendor Daily Log'.       "Added By Aishwarya - 8000003767 - 07.04.2025
*    w_doc_data-sensitivty = 'F'.
*
** Fill the document data and get size of attachment
*    CLEAR w_doc_data.
*    READ TABLE it_attach INDEX w_cnt.
*    w_doc_data-doc_size = ( w_cnt - 1 ) * 255 + strlen( it_attach ).
*    w_doc_data-obj_langu  = sy-langu.
*    w_doc_data-obj_name   = 'SAPRPT'.
*    w_doc_data-obj_descr  = 'Error Log for ZFI_EMPMAST_UPD'.
*    w_doc_data-sensitivty = 'F'.
*    CLEAR t_attachment.
*    REFRESH t_attachment.
*    t_attachment[] = it_attach[].
*
** Describe the body of the message
*    CLEAR t_packing_list.
*    REFRESH t_packing_list.
*    t_packing_list-transf_bin = space.
*    t_packing_list-head_start = 1.
*    t_packing_list-head_num = 0.
*    t_packing_list-body_start = 1.
*    DESCRIBE TABLE it_message LINES t_packing_list-body_num.
*    t_packing_list-doc_type = 'RAW'.
*    APPEND t_packing_list.
*
** Create attachment notification
*    t_packing_list-transf_bin = 'X'.
*    t_packing_list-head_start = 1.
*    t_packing_list-head_num   = 1.
*    t_packing_list-body_start = 1.
*
*    DESCRIBE TABLE t_attachment LINES t_packing_list-body_num.
*    t_packing_list-doc_type   = 'XLS'.
*    t_packing_list-obj_descr  = |ZFI_EMPMAST_UPD log{ sy-datum }|.
*    t_packing_list-obj_name   = 'TEST'.
*    t_packing_list-doc_size   =  t_packing_list-body_num * 255.
*    APPEND t_packing_list.
*
*** RECEIVERS EMAIL
*    SELECT id_name
*           id_number
*           param_name
*       FROM zparam_table INTO TABLE ta_param WHERE id_name EQ c_emp1.
*
*    LOOP AT ta_param INTO wa_param .
*      IF sy-subrc = 0 AND ta_param IS NOT INITIAL.
**        REFRESH t_receivers.
*        t_receivers-receiver = wa_param-param_name.
*        t_receivers-rec_type = 'U'.
*        t_receivers-com_type = 'INT'.
*        t_receivers-notif_del = 'X'.
*        t_receivers-notif_ndel = 'X'.
*        APPEND t_receivers.
*      ENDIF.
*    ENDLOOP.
*    CLEAR: t_receivers.
*    CLEAR wa_param.
*
*    SELECT id_name
*           id_number
*           param_name
*       FROM zparam_table INTO TABLE ta_param1 WHERE id_name EQ c_emp2.
*
*    LOOP AT ta_param1 INTO wa_param1 .
*      IF sy-subrc = 0 AND ta_param1 IS NOT INITIAL.
*        t_receivers-receiver = wa_param1-param_name.
*        t_receivers-rec_type = 'U'.
*        t_receivers-com_type = 'INT'.
*        t_receivers-notif_del = 'X'.
*        t_receivers-copy = 'X'.
*        t_receivers-notif_ndel = 'X'.
*        APPEND t_receivers.
*      ENDIF.
*    ENDLOOP.
*    CLEAR: t_receivers.
*    CLEAR wa_param1.
*
*  ENDIF.
*
*  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
*    EXPORTING
*      document_data              = w_doc_data
*      put_in_outbox              = 'X'
*      sender_address             = ' '
*      sender_address_type        = ' '
*      commit_work                = 'X'
*    IMPORTING
*      sent_to_all                = w_sent_all
*    TABLES
*      packing_list               = t_packing_list
**     contents_bin               = t_attachment
*      contents_txt               = it_message
*      receivers                  = t_receivers
*    EXCEPTIONS
*      too_many_receivers         = 1
*      document_not_sent          = 2
*      document_type_not_exist    = 3
*      operation_no_authorization = 4
*      parameter_error            = 5
*      x_error                    = 6
*      enqueue_error              = 7
*      OTHERS                     = 8.
*
**  IF sy-subrc = 0.
**    WAIT UP TO 2 SECONDS.
**    SUBMIT rsconn01 WITH mode = 'INT'
**                  WITH output = 'X'
**                  AND RETURN.
**  ENDIF.
*
**ENDIF.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FETCH_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fetch_files .

  DATA: lwa_line           TYPE string,
        lv_line            TYPE string,
        lv_file            TYPE rcgiedial-iefile,
        lv_filename        TYPE rcgiedial-iefile,
        lv_source          TYPE rcgiedial-iefile,
        lv_target          TYPE rcgiedial-iefile,
        lv_arcpath         TYPE rcgiedial-iefile,
        lv_len             TYPE p,
        lta_file           TYPE TABLE OF eps2fili,
        lwa_file           TYPE eps2fili,
        lv_message         TYPE string,
        lv_counter         TYPE i,
        gv_dir             TYPE eps2filnam,
        lv_header_skip_tax TYPE abap_bool VALUE '0', " Flag to skip the header line
        lv_header_skip_dpr TYPE abap_bool VALUE '0'. " Flag to skip the header line

  CONSTANTS : lc_ext     TYPE char4 VALUE '.csv',
              lc_extstr1 TYPE char3 VALUE 'csv',
              lc_extstr2 TYPE char3 VALUE 'CSV'.

  CONSTANTS : co_param      TYPE string VALUE 'HRMS_IN',
              co_param1     TYPE string VALUE 'HRMS_OUT',
              co_file_name  TYPE string VALUE 'TAXINVOICE*',
              co_file_name1 TYPE string VALUE 'DPR*'.

  CONSTANTS : co_s(1) VALUE 'S',
              co_e(1) VALUE 'E'.

  SELECT SINGLE * FROM zparam_table INTO @DATA(lv_path) WHERE id_name = @co_param.  "#EC CI_NOORDER
  IF sy-subrc = 0 AND lv_path-param_name IS NOT INITIAL.
    CLEAR gv_dir.
    gv_dir = lv_path-param_name.

    CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
      EXPORTING
        iv_dir_name            = gv_dir
      TABLES
        dir_list               = lta_file
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 8.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF NOT lta_file IS INITIAL.
      CLEAR: lwa_file.

      LOOP AT lta_file INTO lwa_file.                  " FOR MULTIPLE FILES TO BE PROCESSED INDIVIDUALLY
        IF NOT ( lwa_file-name CP co_file_name1 ).
          CONTINUE.
        ENDIF.

        lv_counter = lv_counter + 1.

        "" REFRESH DATA FOR NEW LOOP PASS
        CLEAR: lwa_line, lv_file, lv_filename, lv_arcpath, lv_len, ta_msg, ta_file_dpr.

        "" CHECK THE EXTENSION AND ALLOW VALID EXTENSION FILES
        SPLIT lwa_file-name AT '.' INTO DATA(lv_1) DATA(lv_2).

        IF lv_2 IS NOT INITIAL.
          "" SKIP THE FILE IF NOT TEXT FILE
          IF lv_2 NE lc_extstr1 AND lv_2 NE lc_extstr2.
            CONTINUE.
          ENDIF.
        ELSE.
          "" SKIP THE FILE IF NO EXTENSION
          CONTINUE.
        ENDIF.

        lv_file = lwa_file-name.
        lv_filename = lv_file.
        CONCATENATE gv_dir lv_file INTO lv_file.

        OPEN DATASET lv_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.
        IF sy-subrc = 0.
          DO.
            READ DATASET lv_file INTO lwa_line.
            IF sy-subrc EQ 0.
              " Skip the first line (header) if not already skipped
              IF lv_header_skip_dpr EQ '0'.
                lv_header_skip_dpr = '1'. " Set flag to skip the next line
                CONTINUE. " Skip processing for header line
              ENDIF.
              PERFORM f_convert_dpr USING lwa_line CHANGING wa_file_dpr.
              APPEND wa_file_dpr TO ta_file_dpr.
            ELSE.
              EXIT.
            ENDIF.
          ENDDO.

          "" CLOSE THE FILE
          CLOSE DATASET lv_file.
        ENDIF.
      ENDLOOP.

      CLEAR: lwa_file.
      LOOP AT lta_file INTO lwa_file.         " FOR MULTIPLE FILES TO BE PROCESSED INDIVIDUALLY
        IF NOT ( lwa_file-name CP co_file_name ).
          CONTINUE.
        ENDIF.

        lv_counter = lv_counter + 1.

        "" REFRESH DATA FOR NEW LOOP PASS
        CLEAR: lwa_line, lv_file, lv_filename, lv_arcpath, lv_len, ta_msg, ta_file_tax.

        "" CHECK THE EXTENSION AND ALLOW VALID EXTENSION FILES
        SPLIT lwa_file-name AT '.' INTO lv_1 lv_2.
        IF lv_2 IS NOT INITIAL.
          "" SKIP THE FILE IF NOT TEXT FILE
          IF lv_2 NE lc_extstr1 AND lv_2 NE lc_extstr2.
            CONTINUE.
          ENDIF.
        ELSE.
          "" SKIP THE FILE IF NO EXTENSION
          CONTINUE.
        ENDIF.

        lv_file = lwa_file-name.
        lv_filename = lv_file.
        CONCATENATE gv_dir lv_file INTO lv_file.

        OPEN DATASET lv_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.
        IF sy-subrc = 0.
          DO.
            READ DATASET lv_file INTO lwa_line.
            IF sy-subrc EQ 0.
              " Skip the first line (header) if not already skipped
              IF lv_header_skip_tax EQ '0'.
                lv_header_skip_tax = '1'. " Set flag to skip the next line
                CONTINUE. " Skip processing for header line
              ENDIF.
              PERFORM f_convert_tax USING lwa_line CHANGING wa_file_tax.
              APPEND wa_file_tax TO ta_file_tax.
            ELSE.
              EXIT.
            ENDIF.
          ENDDO.

          "" CLOSE THE FILE
          CLOSE DATASET lv_file.
        ENDIF.
      ENDLOOP.

    ELSE.
      MESSAGE text-032 TYPE co_s DISPLAY LIKE co_e.
      LEAVE LIST-PROCESSING.
    ENDIF.

*    SELECT lifnr FROM lfa1 INTO TABLE ta_lifnr.                                       "Commented by Aishwarya - 8000003767 - 25.04.2025
    SELECT lifnr bukrs FROM lfb1 INTO TABLE ta_lifnr WHERE bukrs = '1000'. "#EC CI_NOFIRST             "Added by Aishwarya - 8000003767 - 25.04.2025
    SORT ta_lifnr BY lifnr.

    SELECT lifnr banks bankl bankn FROM lfbk INTO TABLE ta_lfbk. "#EC CI_NOWHERE

    SORT ta_lfbk BY lifnr banks bankl bankn.

    LOOP AT ta_file_tax INTO wa_file_tax.

      wa_file_tax-lifnr = |{ wa_file_tax-lifnr ALPHA = IN }|.

      READ TABLE ta_lifnr INTO DATA(wa_lifnr) WITH KEY lifnr = wa_file_tax-lifnr BINARY SEARCH.
      IF sy-subrc = 0.
        SHIFT wa_lifnr-lifnr LEFT DELETING LEADING '0'.
        wa_tax_update-lifnr = wa_lifnr-lifnr.
        APPEND wa_tax_update TO ta_tax_update.
      ELSE.
        SHIFT wa_file_tax-lifnr LEFT DELETING LEADING '0'.
        wa_tax_create-lifnr = wa_file_tax-lifnr.
        APPEND wa_tax_create TO ta_tax_create.
      ENDIF.

      CLEAR : wa_file_tax, wa_tax_update, wa_tax_create, wa_lifnr.
    ENDLOOP.

    LOOP AT ta_file_dpr INTO wa_file_dpr.

      wa_file_dpr-lifnr = |{ wa_file_dpr-lifnr ALPHA = IN }|.

      READ TABLE ta_lifnr INTO wa_lifnr WITH KEY lifnr = wa_file_dpr-lifnr BINARY SEARCH.
      IF sy-subrc = 0.
        SHIFT wa_lifnr-lifnr LEFT DELETING LEADING '0'.
        wa_dpr_update-lifnr = wa_lifnr-lifnr.
        APPEND wa_dpr_update TO ta_dpr_update.
      ELSE.
        SHIFT wa_file_dpr-lifnr LEFT DELETING LEADING '0'.
        wa_dpr_create-lifnr = wa_file_dpr-lifnr.
        APPEND wa_dpr_create TO ta_dpr_create.
      ENDIF.

      CLEAR : wa_file_dpr, wa_dpr_create, wa_dpr_update, wa_lifnr.
    ENDLOOP.

* Merge ta_final_tax and ta_final_dpr into ta_final_combined
    APPEND LINES OF ta_tax_update TO ta_update.                      "If LIFNR Present in lfa1 table
    APPEND LINES OF ta_dpr_update TO ta_update.

    SORT ta_update BY lifnr.
    DELETE ADJACENT DUPLICATES FROM ta_update.

* Merge ta_final_tax and ta_final_dpr into ta_final_combined
    APPEND LINES OF ta_dpr_create TO ta_create.                       "If LIFNR is not Present in lfa1 table
    APPEND LINES OF ta_tax_create TO ta_create.

    SORT ta_create BY lifnr.
    DELETE ADJACENT DUPLICATES FROM ta_create.

  ELSE.
    MESSAGE text-031 TYPE co_e.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SELECT SINGLE * FROM zparam_table INTO @DATA(lv_hrms) WHERE id_name = @co_param1.  "#EC CI_NOORDER
  IF sy-subrc = 0 AND lv_hrms-param_name IS NOT INITIAL..

    IF lta_file IS NOT INITIAL.
      LOOP AT lta_file INTO lwa_file.

        CLEAR: lv_filename, lv_source, lv_target, lv_line.

        lv_file = lwa_file-name.
        lv_filename = lv_file.
        CONCATENATE lv_hrms-param_name lv_filename INTO lv_target.

        lv_file = lwa_file-name.
        lv_filename = lv_file.
        CONCATENATE gv_dir lv_filename INTO lv_source.

        OPEN DATASET lv_source FOR INPUT IN TEXT MODE ENCODING DEFAULT.
        IF sy-subrc <> 0.
          MESSAGE text-034 TYPE co_s DISPLAY LIKE co_e.
          CONTINUE.
        ENDIF.

        OPEN DATASET lv_target FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
        IF sy-subrc <> 0.
          MESSAGE text-035 TYPE co_s DISPLAY LIKE co_e.
          CLOSE DATASET lv_source.
          CONTINUE.
        ENDIF.

        WHILE sy-subrc = 0.
          READ DATASET lv_source INTO lv_line.
          IF sy-subrc = 0.
            TRANSFER lv_line TO lv_target.
          ENDIF.
        ENDWHILE.

        DELETE DATASET lv_source.

        CLOSE DATASET lv_source.
        CLOSE DATASET lv_target.

      ENDLOOP.
    ELSE.
      MESSAGE text-032 TYPE co_s DISPLAY LIKE co_e.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ELSE.
    MESSAGE text-031 TYPE co_e.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONVERT_WA1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_LINE  text
*      <--P_WA_FILES  text
*----------------------------------------------------------------------*
FORM f_convert_dpr  USING uwa_line_dpr CHANGING cwa_file_dpr LIKE wa_file_dpr.

  DATA: lv_val1  TYPE char100,
        lv_val2  TYPE char100,
        lv_val3  TYPE char100,
        lv_val4  TYPE char100,
        lv_val5  TYPE char100,
        lv_val6  TYPE char100,
        lv_val7  TYPE char100,
        lv_val8  TYPE char100,
        lv_val9  TYPE char100,
        lv_val10 TYPE char100,
        lv_val11 TYPE char100,
        lv_val12 TYPE char100,
        lv_val13 TYPE char100,
        lv_val14 TYPE char100,
        lv_val15 TYPE char100,
        lv_val16 TYPE char100,
        lv_val17 TYPE char100,
        lv_val18 TYPE char100,
        lv_val19 TYPE char100,
        lv_val20 TYPE char100,
        lv_val21 TYPE char100,
        lv_val22 TYPE char100,
        lv_val23 TYPE char100,
        lv_val24 TYPE char100,
        lv_val25 TYPE char100,
        lv_val26 TYPE char100,
        lv_val27 TYPE char100,
        lv_val28 TYPE char100.

*  SPLIT uwa_line_dpr AT ','    "'#'                                                                "CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
  SPLIT uwa_line_dpr AT '|'               "Added By vema for Cr8000003767  on 03.02.2026
  INTO lv_val1  lv_val2  lv_val3  lv_val4  lv_val5  lv_val6  lv_val7  lv_val8  lv_val9  lv_val10
       lv_val11 lv_val12 lv_val13 lv_val14 lv_val15 lv_val16 lv_val17 lv_val18 lv_val19 lv_val20
       lv_val21 lv_val22 lv_val23 lv_val24 lv_val25 lv_val26 lv_val27 lv_val28.

  cwa_file_dpr-buzei      = lv_val1  .
  cwa_file_dpr-bldat      = lv_val2  .
  cwa_file_dpr-blart      = lv_val3  .
  cwa_file_dpr-bukrs      = lv_val4  .
  cwa_file_dpr-event_dt   = lv_val5  .
  cwa_file_dpr-apr_dt     = lv_val6  .
  cwa_file_dpr-inv_sb_dt  = lv_val7  .
  cwa_file_dpr-budat      = lv_val8  .
  cwa_file_dpr-waers      = lv_val9  .
  cwa_file_dpr-bktxt      = lv_val10 .
  cwa_file_dpr-kursf      = lv_val11 .
  cwa_file_dpr-xblnr      = lv_val12  .
  cwa_file_dpr-umskz      = lv_val13 .
  cwa_file_dpr-lifnr      = lv_val14 .
  cwa_file_dpr-dmbtr      = lv_val15 .
  cwa_file_dpr-zfbdt      = lv_val16 .
  cwa_file_dpr-prctr      = lv_val17 .
  cwa_file_dpr-zuonr      = lv_val18 .
  cwa_file_dpr-sgtxt      = lv_val19 .
  cwa_file_dpr-bupla      = lv_val20 .
  cwa_file_dpr-secoo      = lv_val21 .
  cwa_file_dpr-zlspr      = lv_val22 .
  cwa_file_dpr-zlsch      = lv_val23 .
  cwa_file_dpr-xref1      = lv_val24 .
  cwa_file_dpr-xref3      = lv_val25 .
  cwa_file_dpr-qsshb      = lv_val26 .
  cwa_file_dpr-witht      = lv_val27 .
  cwa_file_dpr-qsskz      = lv_val28 .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONVERT_TAX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LWA_LINE  text
*      <--P_WA_FILES  text
*----------------------------------------------------------------------*
FORM f_convert_tax  USING uwa_line_tax CHANGING cwa_file_tax LIKE wa_file_tax.

  DATA: lv_val1  TYPE char100,
        lv_val2  TYPE char100,
        lv_val3  TYPE char100,
        lv_val4  TYPE char100,
        lv_val5  TYPE char100,
        lv_val6  TYPE char100,
        lv_val7  TYPE char100,
        lv_val8  TYPE char100,
        lv_val9  TYPE char100,
        lv_val10 TYPE char100,
        lv_val11 TYPE char100,
        lv_val12 TYPE char100,
        lv_val13 TYPE char100,
        lv_val14 TYPE char100,
        lv_val15 TYPE char100,
        lv_val16 TYPE char100,
        lv_val17 TYPE char100,
        lv_val18 TYPE char100,
        lv_val19 TYPE char100,
        lv_val20 TYPE char100,
        lv_val21 TYPE char100,
        lv_val22 TYPE char100,
        lv_val23 TYPE char100,
        lv_val24 TYPE char100,
        lv_val25 TYPE char100,
        lv_val26 TYPE char100,
        lv_val27 TYPE char100,
        lv_val28 TYPE char100,
        lv_val29 TYPE char100,
        lv_val30 TYPE char100,
        lv_val31 TYPE char100,
        lv_val32 TYPE char100,
        lv_val33 TYPE char100,
        lv_val34 TYPE char100,
        lv_val35 TYPE char100,
        lv_val36 TYPE char100.

*  SPLIT uwa_line_tax AT ','                                                                "CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
  SPLIT uwa_line_tax AT '|'                      "Added By vema for Cr8000003767  on 03.02.2026
  INTO lv_val1  lv_val2  lv_val3  lv_val4  lv_val5  lv_val6  lv_val7  lv_val8  lv_val9  lv_val10
       lv_val11 lv_val12 lv_val13 lv_val14 lv_val15 lv_val16 lv_val17 lv_val18 lv_val19 lv_val20
       lv_val21 lv_val22 lv_val23 lv_val24 lv_val25 lv_val26 lv_val27 lv_val28 lv_val29 lv_val30
       lv_val31 lv_val32 lv_val33 lv_val34 lv_val35 lv_val36.

  cwa_file_tax-bukrs      = lv_val1  .
  cwa_file_tax-bktxt      = lv_val2  .
  cwa_file_tax-blart      = lv_val3  .
  cwa_file_tax-xblnr      = lv_val4  .
  cwa_file_tax-event_dt   = lv_val5  .
  cwa_file_tax-apr_dt     = lv_val6  .
  cwa_file_tax-inv_sb_dt  = lv_val7  .
  cwa_file_tax-bldat      = lv_val8  .
  cwa_file_tax-budat      = lv_val9  .
  cwa_file_tax-lifnr      = lv_val10  .
  cwa_file_tax-bupla      = lv_val11 .
  cwa_file_tax-secco      = lv_val12 .
  cwa_file_tax-zterm      = lv_val13 .
  cwa_file_tax-zlspr      = lv_val14 .
  cwa_file_tax-zlsch      = lv_val15 .
  cwa_file_tax-dmbtr      = lv_val16 .
  cwa_file_tax-waers      = lv_val17 .
  cwa_file_tax-xmwst      = lv_val18 .
  cwa_file_tax-bstat      = lv_val19 .
  cwa_file_tax-name1      = lv_val20 .
  cwa_file_tax-rebzg      = lv_val21 .
  cwa_file_tax-buzei      = lv_val22 .
  cwa_file_tax-sgtxt      = lv_val23 .
  cwa_file_tax-wrbtr      = lv_val24 .
  cwa_file_tax-hsn_sac    = lv_val25 .
  cwa_file_tax-mwskz      = lv_val26 .
  cwa_file_tax-kschl      = lv_val27 .
  cwa_file_tax-mwsts      = lv_val28 .
  cwa_file_tax-wmwst      = lv_val29 .
  cwa_file_tax-kostl      = lv_val30 .
  cwa_file_tax-hkont      = lv_val31 .
  cwa_file_tax-qsshb      = lv_val32 .
  cwa_file_tax-witht      = lv_val33 .
  cwa_file_tax-qsskz      = lv_val34 .
  cwa_file_tax-prctr      = lv_val35 .
  cwa_file_tax-zuonr      = lv_val36 .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL_NEW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email_new .

  DATA : lta_message            TYPE STANDARD TABLE OF solisti1,
         lwa_message            TYPE solisti1,
         lv_subject             LIKE sodocchgi1-obj_descr,
*         lv_srno                TYPE c VALUE 0,   " S/4HANA: C(1) - '*' at 10, CONVT_NO_NUMBER at 11
         lv_srno                TYPE i,
         lwa_doc_data           LIKE sodocchgi1,
         lta_packing_list       TYPE TABLE OF sopcklsti1,
         lwa_packing_list       TYPE sopcklsti1,
         lta_receivers          TYPE TABLE OF somlreci1,
         lwa_receivers          TYPE somlreci1,
         lv_send_address        TYPE so_rec_ext,
         lv_sender_address_type LIKE soextreci1-adr_typ,
         lv_sent_all            TYPE c,
         lv_srno1               TYPE i.

  CONSTANTS: co_s(1) VALUE 'S',
             co_e(1) VALUE 'E'.

  CLEAR: lwa_message.

  lwa_message-line = 'Dear Sir/Madam,'.
  APPEND lwa_message TO lta_message.
  CLEAR: lwa_message.

  lwa_message = '<br />'.
  APPEND lwa_message TO lta_message.
  CLEAR lwa_message.

  lwa_message = '<br />'.
  APPEND lwa_message TO lta_message.
  CLEAR lwa_message.

  lwa_message = 'Please find log of creation/modification of Employee Vendor Codes in Z008 vendor account group of the General Vendor Master in SAP'.
  APPEND lwa_message TO lta_message.
  CLEAR: lwa_message.

  lwa_message = '<br />'.
  APPEND lwa_message TO lta_message.
  CLEAR lwa_message.

  lwa_message = '<br />'.
  APPEND lwa_message TO lta_message.
  CLEAR lwa_message.

**Table.
*   table display
  CLEAR lwa_message.
  lwa_message-line = '<table style="MARGIN: 10px" bordercolor="#90EE90" '.
  APPEND lwa_message TO lta_message.

  CLEAR lwa_message.
  lwa_message-line = ' cellspacing="0" cellpadding="3" width="95%"'.
  APPEND lwa_message TO lta_message.

  CLEAR lwa_message.
  lwa_message-line = ' border="1"><tbody><tr>'.
  APPEND lwa_message TO lta_message.

*   table header   Sr No, Vendor Code, Status, Name of Employee, PAN No Of Employee, Remarks
  CLEAR lwa_message.
  lwa_message-line = '<th bgcolor=#eeeeee>Sr No</th>'.
  APPEND lwa_message TO lta_message.

  CLEAR lwa_message.
  lwa_message-line = '<th bgcolor=#eeeeee>Vendor Code</th>'.
  APPEND lwa_message TO lta_message.

  CLEAR lwa_message.
  lwa_message-line = '<th bgcolor=#eeeeee>Status</th>'.
  APPEND lwa_message TO lta_message.

  CLEAR lwa_message.
  lwa_message-line = '<th bgcolor=#eeeeee>Name of Employee</th>'.
  APPEND lwa_message TO lta_message.

*  CLEAR lwa_message.
*  lwa_message-line = '<th bgcolor=#eeeeee>PAN No Of Employee</th>'.
*  APPEND lwa_message TO lta_message.

  CLEAR lwa_message.
  lwa_message-line = '<th bgcolor=#eeeeee>Remarks</th>'.
  APPEND lwa_message TO lta_message.

  DESCRIBE TABLE ta_msg_new LINES DATA(lv_line).

  LOOP AT ta_msg_new INTO wa_msg_new. " WHERE emp_no = wa_file-lifnr.

    lv_srno = lv_srno + 1.
    DATA(wa_msg_temp) = wa_msg_new.

    CLEAR lwa_message.
    lwa_message-line = '<tr style="text-align: center;">'.
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = |<td>{ lv_srno }</td>|.
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    CONCATENATE '<td>' wa_msg_temp-emp_no'</td>' INTO lwa_message-line.
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    CONCATENATE '<td>' wa_msg_temp-status'</td>' INTO lwa_message-line.
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    CONCATENATE '<td>' wa_msg_temp-emp_name'</td>' INTO lwa_message-line.
    APPEND lwa_message TO lta_message.

*    CLEAR lwa_message.
*    CONCATENATE '<td>' wa_msg_temp-pan_no'</td>' INTO lwa_message-line.
*    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    CONCATENATE '<td>' wa_msg_temp-remarks'</td>' INTO lwa_message-line.
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message = '</tr>'.
    APPEND lwa_message TO lta_message.

    CLEAR : wa_msg_new, lwa_message, wa_msg_temp.

  ENDLOOP.

*Table close
  CLEAR lwa_message.
  lwa_message-line = '</tbody> </table>'.
  APPEND lwa_message TO lta_message.

  CLEAR lwa_message.
  lwa_message = '<br />'.
  APPEND lwa_message TO lta_message.

  lwa_message = '<br />'.
  APPEND lwa_message TO lta_message.
  CLEAR lwa_message.

  DESCRIBE TABLE lta_message LINES lv_line.
  READ TABLE lta_message INTO lwa_message INDEX lv_line.
  lwa_doc_data-doc_size = ( lv_line - 1 ) * 255 + strlen( lwa_message ).

  MODIFY lta_packing_list FROM lwa_packing_list INDEX 1 TRANSPORTING body_num.
  CLEAR  lwa_packing_list.
  lwa_packing_list-transf_bin = space.
  lwa_packing_list-head_start = 1.
  lwa_packing_list-head_num   = 0.
  lwa_packing_list-body_start = 1.
  DESCRIBE TABLE lta_message LINES  lwa_packing_list-body_num.
  lwa_packing_list-doc_type = 'HTM'.
  APPEND  lwa_packing_list TO lta_packing_list .

  lv_subject = 'Employee Vendor Daily Log'.
  lwa_doc_data-obj_langu = sy-langu.
  lwa_doc_data-obj_name  = 'SAPRPT'.
  lwa_doc_data-obj_descr = lv_subject .
  lwa_doc_data-sensitivty = 'F'.

  lv_sender_address_type = 'INT'.
  lv_send_address = 'Do_Not_Reply@Cipla.com'.

  " Define recipient

  IF NOT s_to[] IS INITIAL.
    LOOP AT s_to INTO DATA(lwa_to).
      lwa_receivers-receiver   = lwa_to-low.
      lwa_receivers-rec_type   = 'U'. " User Email
      lwa_receivers-com_type   = 'INT'.
      lwa_receivers-notif_del  = ''.
      lwa_receivers-notif_ndel = ''.
      APPEND lwa_receivers TO lta_receivers.
      CLEAR: lwa_to,lwa_receivers.
    ENDLOOP.
  ENDIF.

  IF NOT s_cc[] IS INITIAL.
    LOOP AT s_cc INTO DATA(lwa_cc).
      lwa_receivers-receiver   = lwa_cc-low.
      lwa_receivers-rec_type   = 'U'.
      lwa_receivers-com_type   = 'INT'.
      lwa_receivers-notif_del  = ''.
      lwa_receivers-notif_ndel = ''.
      lwa_receivers-copy       = 'X'.
      APPEND lwa_receivers  TO lta_receivers .
      CLEAR: lwa_cc,lwa_receivers.
    ENDLOOP.
  ENDIF.

  IF lta_receivers IS NOT INITIAL.

    CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
      EXPORTING
        document_data              = lwa_doc_data
*       put_in_outbox              = 'X'
        sender_address             = lv_send_address
        sender_address_type        = lv_sender_address_type
        commit_work                = 'X'
      IMPORTING
        sent_to_all                = lv_sent_all
      TABLES
        packing_list               = lta_packing_list
*       contents_bin               = lta_attachment
*       contents_hex               = lta_content_hex
        contents_txt               = lta_message
        receivers                  = lta_receivers
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.

    IF sy-subrc = 0.
      MESSAGE text-040 TYPE co_s.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  HIDE_SEL_OPT_INTERVAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM hide_sel_opt_interval.

*-- Variable Declarations
  DATA: restrict TYPE sscr_restrict,
        opt_list TYPE sscr_opt_list_tab WITH HEADER LINE,
        ass_tab  TYPE sscr_ass_tab WITH HEADER LINE.

*-- Constants Declarations
  CONSTANTS:
    c_name_all TYPE rsrestrict-optionlist VALUE 'ALL',
    c_kind_s   TYPE rsscr-kind            VALUE 'S',
    c_s_to     TYPE rsrestrict-objectname VALUE 'S_TO',
    c_s_cc     TYPE rsrestrict-objectname VALUE 'S_CC',
    c_main_i   TYPE rsrestrict-sign       VALUE 'I'.

*-- Options fill
  opt_list-name       = c_name_all.
  opt_list-options-eq = abap_true.
  APPEND opt_list TO restrict-opt_list_tab.

*-- Hide ranges
  CLEAR ass_tab.
  ass_tab-kind    = c_kind_s.
  ass_tab-name    = c_s_to.
  ass_tab-sg_main = c_main_i.
  ass_tab-sg_addy = space.
  ass_tab-op_main = c_name_all.
  APPEND ass_tab TO restrict-ass_tab.

  CLEAR ass_tab.
  ass_tab-kind    = c_kind_s.
  ass_tab-name    = c_s_cc.
  ass_tab-sg_main = c_main_i.
  ass_tab-sg_addy = space.
  ass_tab-op_main = c_name_all.
  APPEND ass_tab TO restrict-ass_tab.

*-- Interval restrictions
  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      restriction            = restrict
    EXCEPTIONS
      too_late               = 1
      repeated               = 2
      selopt_without_options = 3
      selopt_without_signs   = 4
      invalid_sign           = 5
      empty_option_list      = 6
      invalid_kind           = 7
      repeated_kind_a        = 8
      OTHERS                 = 9.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_HEADER
*&---------------------------------------------------------------------*
* S/4HANA conversion: which layout the file has, from its header line.
* HRMS's own layout ("Employee vendor creation - mapping HRMS.xlsx") is
* found by its column headings, in any order and with either spelling
* (Employee_Code / Emp_Code, Bank_AcctNo / Bank_AccNo, ...); any other
* file is the 50-column layout F_CONVERT_WA has always read.
* The 50-column file's own header line is a template row that also says
* "Emp_Code" ("Emp_Code#1000##Z008#Salutation#fname mname lname#...") - it
* was taken for the HRMS layout, refused for a missing name column, and no
* employee vendor was created (CRQ, 23.11.2026). A header with 40 or more
* columns, or without an employee name heading, is the 50-column layout.
*----------------------------------------------------------------------*
FORM f_header USING uv_line TYPE string.
  DATA: lt_col  TYPE string_table,
        lt_sep  TYPE string_table,
        lv_best TYPE i,
        lv_n    TYPE i.
  CLEAR: gs_col, gv_layout, gv_sep.
  gv_layout = co_lay_sap.
  gv_sep    = '#'.
  " The separator: the one the header line has most of.
  lt_sep = VALUE #( ( `#` ) ( `,` ) ( `|` ) ( `;` ) ( |{ cl_abap_char_utilities=>horizontal_tab }| ) ).
  LOOP AT lt_sep INTO DATA(lv_sep).
    FIND ALL OCCURRENCES OF lv_sep IN uv_line MATCH COUNT lv_n.
    IF lv_n > lv_best.
      lv_best = lv_n.
      gv_sep  = lv_sep.
    ENDIF.
  ENDLOOP.
  PERFORM f_split USING uv_line gv_sep CHANGING lt_col.
  LOOP AT lt_col INTO DATA(lv_col).
    DATA(lv_ix) = sy-tabix.
    DATA(lv_key) = to_upper( lv_col ).
    REPLACE ALL OCCURRENCES OF PCRE '[^A-Z0-9]' IN lv_key WITH ``.
    CASE lv_key.
      WHEN 'EMPLOYEECODE' OR 'EMPCODE'.     gs_col-emp   = lv_ix.
      WHEN 'SALUTATION'.                    gs_col-sal   = lv_ix.
      WHEN 'FNAME' OR 'FIRSTNAME'.          gs_col-fname = lv_ix.
      WHEN 'MNAME' OR 'MIDDLENAME'.         gs_col-mname = lv_ix.
      WHEN 'LNAME' OR 'LASTNAME'.           gs_col-lname = lv_ix.
      WHEN 'EMPLOYEENAME' OR 'EMPNAME'.     gs_col-name  = lv_ix.
      WHEN 'BANKACCTNO' OR 'BANKACCNO'.     gs_col-acct  = lv_ix.
      WHEN 'BANKIFSC'.                      gs_col-ifsc  = lv_ix.
      WHEN 'BANKCOUNTRY'.                   gs_col-bctry = lv_ix.
      WHEN 'MOBILENO'.                      gs_col-mob   = lv_ix.
      WHEN 'FUNCTION'.                      gs_col-func  = lv_ix.
      WHEN 'EMAILADDRESS'.                  gs_col-mail  = lv_ix.
      WHEN 'EMAIL'.                         gs_col-mail2 = lv_ix.
    ENDCASE.
  ENDLOOP.
  IF gs_col-emp IS INITIAL
     OR lines( lt_col ) >= 40
     OR ( gs_col-name IS INITIAL AND gs_col-lname IS INITIAL ).
    CLEAR gs_col.                         " the 50-column layout
    gv_sep = '#'.
    RETURN.
  ENDIF.
  gv_layout = co_lay_hrms.
  " What a vendor cannot be created without.
  DATA(lt_need) = VALUE string_table(
    ( COND #( WHEN gs_col-acct IS INITIAL THEN `Bank_AcctNo` ) )
    ( COND #( WHEN gs_col-ifsc IS INITIAL THEN `Bank IFSC` ) ) ).
  DELETE lt_need WHERE table_line IS INITIAL.
  IF lt_need IS NOT INITIAL.
    wa_msg-status  = co_failed.
    wa_msg-remarks = |HRMS file has no column { concat_lines_of( table = lt_need sep = `, ` ) } - | &&
                     |see "Employee vendor creation - mapping HRMS"|.
    APPEND wa_msg TO ta_msg.
    CLEAR wa_msg.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SPLIT
*&---------------------------------------------------------------------*
* One line into its columns. A column in double quotes may hold the
* separator ("Lower Parel, Mumbai"); "" inside quotes is a quote.
*----------------------------------------------------------------------*
FORM f_split USING uv_line TYPE string uv_sep TYPE string CHANGING ct_col TYPE string_table.
  CLEAR ct_col.
  IF uv_line NS '"'.
    SPLIT uv_line AT uv_sep INTO TABLE ct_col.
    RETURN.
  ENDIF.
  DATA: lv_cur TYPE string,
        lv_q   TYPE abap_bool,
        lv_i   TYPE i,
        lv_c   TYPE string.
  DATA(lv_len) = strlen( uv_line ).
  WHILE lv_i < lv_len.
    lv_c = substring( val = uv_line off = lv_i len = 1 ).
    IF lv_c = '"'.
      IF lv_q = abap_true AND lv_i + 1 < lv_len.
        IF substring( val = uv_line off = lv_i + 1 len = 1 ) = '"'.
          lv_cur = lv_cur && '"'.
          lv_i = lv_i + 2.
          CONTINUE.
        ENDIF.
      ENDIF.
      lv_q = xsdbool( lv_q = abap_false ).
    ELSEIF lv_c = uv_sep AND lv_q = abap_false.
      APPEND lv_cur TO ct_col.
      CLEAR lv_cur.
    ELSE.
      lv_cur = lv_cur && lv_c.
    ENDIF.
    lv_i = lv_i + 1.
  ENDWHILE.
  APPEND lv_cur TO ct_col.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_COL
*&---------------------------------------------------------------------*
FORM f_col USING it_col TYPE string_table uv_ix TYPE i CHANGING cv_val TYPE any.
  CLEAR cv_val.
  IF uv_ix > 0.
    READ TABLE it_col INTO DATA(lv_v) INDEX uv_ix.
    IF sy-subrc = 0.
      cv_val = condense( lv_v ).
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONVERT_HRMS
*&---------------------------------------------------------------------*
* One line of the HRMS file into the vendor record, as sheet "expected
* output file for upload" of "Employee vendor creation - mapping
* HRMS.xlsx" lays it down: the columns HRMS sends, the rest fixed.
*----------------------------------------------------------------------*
FORM f_convert_hrms USING uv_line TYPE string CHANGING cwa_file LIKE wa_file.
  DATA lt_col TYPE string_table.
  CLEAR cwa_file.
  PERFORM f_split USING uv_line gv_sep CHANGING lt_col.
  PERFORM f_col USING lt_col gs_col-emp   CHANGING cwa_file-lifnr.      " LFA1 / LFB1 - LIFNR
  PERFORM f_col USING lt_col gs_col-sal   CHANGING cwa_file-title_medi. " LFA1 - ANRED
  PERFORM f_col USING lt_col gs_col-fname CHANGING cwa_file-fname.      " person: first name
  PERFORM f_col USING lt_col gs_col-mname CHANGING cwa_file-mname.      "         middle name
  PERFORM f_col USING lt_col gs_col-lname CHANGING cwa_file-lname.      "         last name
  PERFORM f_col USING lt_col gs_col-name  CHANGING cwa_file-name1.      " LFA1 - NAME1
  IF cwa_file-name1 IS INITIAL.
    cwa_file-name1 = condense( |{ cwa_file-fname } { cwa_file-mname } { cwa_file-lname }| ).
  ENDIF.
  cwa_file-sort1      = cwa_file-name1.                                 " LFA1 - SORTL
  cwa_file-bukrs      = co_hrms_bukrs.                                  " LFB1 - BUKRS
  cwa_file-ktokk      = co_hrms_ktokk.                                  " LFA1 - KTOKK
  cwa_file-country    = co_hrms_land.                                   " LFA1 - LAND1
  cwa_file-langu      = co_hrms_langu.                                  " LFA1 - SPRAS
  PERFORM f_col USING lt_col gs_col-mob   CHANGING cwa_file-tel_number. " LFA1 - TELF1
  PERFORM f_col USING lt_col gs_col-func  CHANGING cwa_file-konzs.      " LFA1 - KONZS
  cwa_file-konzs      = to_upper( cwa_file-konzs ).
  cwa_file-akont      = co_hrms_akont.                                  " LFB1 - AKONT
  cwa_file-altkn      = cwa_file-lifnr.                                 " LFB1 - ALTKN
  cwa_file-reprf      = abap_true.                                      " LFB1 - REPRF
  cwa_file-zwels      = co_hrms_zwels.                                  " LFB1 - ZWELS
  PERFORM f_col USING lt_col gs_col-mail  CHANGING cwa_file-smtp_addr.  " ADR6 - SMTP_ADDR
  IF cwa_file-smtp_addr IS INITIAL.
    PERFORM f_col USING lt_col gs_col-mail2 CHANGING cwa_file-smtp_addr.
  ENDIF.
  PERFORM f_col USING lt_col gs_col-bctry CHANGING cwa_file-banks.      " LFBK - BANKS
  IF cwa_file-banks IS INITIAL.
    cwa_file-banks    = co_hrms_land.
  ENDIF.
  PERFORM f_col USING lt_col gs_col-ifsc  CHANGING cwa_file-bankl.      " LFBK - BANKL
  cwa_file-bankl      = to_upper( cwa_file-bankl ).
  PERFORM f_col USING lt_col gs_col-acct  CHANGING cwa_file-bankn.      " LFBK - BANKN
  cwa_file-koinh      = cwa_file-name1.                                 " LFBK - KOINH
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BP_CREATE
*&---------------------------------------------------------------------*
* S/4HANA: was the XK01 batch input - the employee vendor is created as
* a business partner (a person) through LCL_EMP_BP.
*----------------------------------------------------------------------*
FORM f_bp_create.
  DATA: lt_error TYPE bapiret2_t,
        lt_ok    TYPE bapiret2_t,
        lv_lifnr TYPE lifnr.
  lv_lifnr = |{ wa_file-lifnr ALPHA = IN }|.
  lcl_emp_bp=>create( EXPORTING is_file  = wa_file
                                iv_lifnr = lv_lifnr
                      IMPORTING et_error = lt_error
                                et_ok    = lt_ok ).
  PERFORM f_bp_log USING co_create lt_error lt_ok CHANGING ta_msg.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BP_UPDATE
*&---------------------------------------------------------------------*
* S/4HANA: was the FK02 batch input - the HRMS bank account replaces the
* employee vendor's first one, on its business partner.
*----------------------------------------------------------------------*
FORM f_bp_update USING uv_lifnr TYPE lifnr.
  DATA: lt_error TYPE bapiret2_t,
        lt_ok    TYPE bapiret2_t.
  lcl_emp_bp=>change_bank( EXPORTING is_file  = wa_file
                                     iv_lifnr = uv_lifnr
                           IMPORTING et_error = lt_error
                                     et_ok    = lt_ok ).
  PERFORM f_bp_log USING co_update lt_error lt_ok CHANGING ta_msg_update.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BP_LOG
*&---------------------------------------------------------------------*
* The messages into the mail log, as F_MESSAGE_LOG(_UPDATE) put the
* batch input's: status New / Modified when saved, Failed otherwise.
*----------------------------------------------------------------------*
FORM f_bp_log USING    uv_status TYPE clike
                       it_error  TYPE bapiret2_t
                       it_ok     TYPE bapiret2_t
              CHANGING ct_msg    LIKE ta_msg.
  DATA ls_msg TYPE ty_msg.
  ls_msg-emp_no   = wa_file-lifnr.
  ls_msg-emp_name = wa_file-name1.
  IF it_error IS INITIAL AND it_ok IS NOT INITIAL.
    LOOP AT it_ok INTO DATA(ls_ok).
      ls_msg-status  = uv_status.
      ls_msg-remarks = ls_ok-message.
      APPEND ls_msg TO ct_msg.
    ENDLOOP.
    RETURN.
  ENDIF.
  IF it_error IS INITIAL.
    ls_msg-status  = co_failed.
    ls_msg-remarks = 'Not saved - no message was returned'.
    APPEND ls_msg TO ct_msg.
    RETURN.
  ENDIF.
  LOOP AT it_error INTO DATA(ls_err).
    ls_msg-status  = co_failed.
    ls_msg-remarks = ls_err-message.
    IF ls_msg-remarks IS INITIAL AND ls_err-id IS NOT INITIAL.
      MESSAGE ID ls_err-id TYPE 'S' NUMBER ls_err-number
              WITH ls_err-message_v1 ls_err-message_v2 ls_err-message_v3 ls_err-message_v4
              INTO ls_msg-remarks.
    ENDIF.
    APPEND ls_msg TO ct_msg.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_WRITE_LOG
*&---------------------------------------------------------------------*
* S/4HANA conversion: the run's log in the job's spool (SM37 - Spool), so
* it can be read when the log mail does not go out.
*----------------------------------------------------------------------*
FORM f_write_log.
  DATA lv_text TYPE string.
  WRITE: / 'Employee vendor upload - log'.
  ULINE.
  lv_text = |Vendors of the DPR / tax invoice files: { lines( ta_create ) } not yet in company code 1000, | &&
            |{ lines( ta_update ) } already there|.
  WRITE: / lv_text.
  LOOP AT gt_run INTO lv_text.
    WRITE: / lv_text.
  ENDLOOP.
  lv_text = |Employee rows found among them: { gv_to_create } to create, { gv_to_update } to check / change bank|.
  WRITE: / lv_text.
  IF gv_to_create = 0.
    WRITE: / 'No employee vendor to create - every employee of the files is already a vendor in 1000,',
           / 'or is not in the DPR / tax invoice files.'.
  ENDIF.
  ULINE.
  SORT gt_log BY emp_no status remarks.
  DELETE ADJACENT DUPLICATES FROM gt_log COMPARING emp_no status remarks.
  LOOP AT gt_log INTO DATA(ls_log).
    lv_text = |{ ls_log-emp_no } { ls_log-status } { ls_log-emp_name }: { ls_log-remarks }|.
    WRITE: / lv_text.
  ENDLOOP.
  IF gt_log IS INITIAL.
    WRITE: / 'No messages.'.
  ENDIF.
ENDFORM.
