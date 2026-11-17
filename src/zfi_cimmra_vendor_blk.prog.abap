*&---------------------------------------------------------------------*
*& Report  ZFI_CIMMRA_VENDOR_BLK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
* Program
* Title             : Cimmra vendor block
* Author            : Uday Reddy
* Program Type      : <Report>
* Creation Date     : 30-03-2026
* Transaction       : NA
* Functional design : Arijit podder
* Change request    : 8000004921
* Transport request : CRDK9A00I0
* Description       : Cimmra vendor blocking using BDC of tcode XK05 transaction and XK02 transaction.

*----------------------------------------------------------------------*
* Modification Information
* Date             : 17.11.2026
* Author           :
* Change request   :
* Transport number :
* Description      : S/4HANA - blocks through the business partner. CALL TRANSACTION
*                    'XK05' (batch input - XK05 is replaced by BP in S/4HANA)
*                    and the payment block through VMD_EI_API are replaced by
*                    LCL_CIMMRA_BP=>BLOCK (include ZFI_CIMMRA_BP), one change
*                    of the business partner through CL_MD_BP_MAINTAIN with
*                    the same fields. The AL11 file and the response file are
*                    unchanged; the transaction code column says BP.
*----------------------------------------------------------------------*
REPORT zfi_cimmra_vendor_blk
       NO STANDARD PAGE HEADING LINE-SIZE 255.

TYPES : BEGIN OF ty_data,
          id     TYPE lfa1-emnfr,       "Id
          lifnr  TYPE lfa1-lifnr,       "VendorAccountNumber
          bukrs  TYPE lfb1-bukrs,       "CompanyCode
          ekorg  TYPE lfm1-ekorg,       "PurchasingOrganization
          sperr  TYPE lfa1-sperr,       "PostingBlockAllCC
          sperr1 TYPE lfa1-sperr,       "PostingBlockCC
          sperm  TYPE lfa1-sperm,       "PurchaseBlockAllPOrg
          sperm1 TYPE lfa1-sperm,       "PurchaseBlockPOrg
          sperq  TYPE lfa1-sperq,       "Block function
          zahls  TYPE lfb1-zahls,       "PaymentBlock
        END OF ty_data.

TYPES : BEGIN OF ty_tab,
          rec(2000),
        END OF ty_tab.

TYPES: BEGIN OF ty_log,
         lifnr TYPE lifnr,
         tcode TYPE tcode,
         msg   TYPE string,
       END OF ty_log.

TYPES : BEGIN OF ty_sft,
          vendor     TYPE lfa1-lifnr,
          vendortype TYPE string,
          t_code     TYPE tcode,
          date       TYPE char10,
          message    TYPE string,
        END OF ty_sft.

DATA : gv_inc       TYPE zparam_table-param_name,
       gv_arc       TYPE zparam_table-param_name,
       gv_dir_name  TYPE eps2filnam,
       ta_file      TYPE TABLE OF eps2fili,
       wa_file      TYPE eps2fili,
       gv_inc_file  TYPE localfile,
       gv_arc_file  TYPE localfile,
       wa_tab       TYPE ty_tab,
       gv_string    TYPE string,
       ta_al11      TYPE TABLE OF ty_data,
       wa_al11      TYPE ty_data,
       ta_response  TYPE TABLE OF ty_sft,
       wa_response  TYPE ty_sft,
       htab(1)      TYPE c,
       gv_al11_path TYPE string,
       lv_date      TYPE sy-datum,
       lv_ddmmyyyy  TYPE char10,
       lv_string    TYPE string.

DATA: it_log TYPE TABLE OF ty_log,
      wa_log TYPE ty_log.


CONSTANTS : co_inc_file     TYPE zparam_table-id_name VALUE 'CIM_VNCRT',
            co_arc_file     TYPE zparam_table-id_name VALUE 'ZFICIM_ARC',
            co_pattern(20)  VALUE 'CIM_VEND_BLK*',
            co_filename(20) VALUE 'CIM_VEND_BLK_RESP',
            co_comma(1)     VALUE ','.


INCLUDE zfi_cimmra_bp.

START-OF-SELECTION.
PERFORM get_file.
PERFORM fetchdata_al11.
PERFORM process_data.
PERFORM send_data.


*&---------------------------------------------------------------------*
*&      Form  GET_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_file .

  CLEAR : gv_inc, gv_arc.

  "To get the AL11 file path.
" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 26.06.2026  FOR ATC
*  SELECT SINGLE param_name FROM zparam_table INTO gv_inc
*    WHERE id_name = co_inc_file.
 SELECT PARAM_NAME FROM ZPARAM_TABLE   UP TO 1 ROWS INTO GV_INC WHERE
ID_NAME = CO_INC_FILE  ORDER BY  ID_NAME ID_NUMBER .   ENDSELECT.
" Code Remediation changes S4 2025_A Conversion * *END OF CHANGE BY ABAP7 26.06.2026 FOR ATC

  "to get the AL11 archive path.
" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 26.06.2026  FOR ATC
*  SELECT SINGLE param_name FROM zparam_table INTO gv_arc
*    WHERE id_name = co_arc_file.
 SELECT PARAM_NAME FROM ZPARAM_TABLE   UP TO 1 ROWS INTO GV_ARC WHERE
ID_NAME = CO_ARC_FILE  ORDER BY  ID_NAME ID_NUMBER .   ENDSELECT.
" Code Remediation changes S4 2025_A Conversion * *END OF CHANGE BY ABAP7 26.06.2026 FOR ATC

  IF sy-subrc = 0 AND gv_inc IS NOT INITIAL AND gv_arc IS NOT INITIAL.

    CLEAR : gv_dir_name.
    gv_dir_name = gv_inc.

    CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
      EXPORTING
        iv_dir_name            = gv_dir_name
*       FILE_MASK              = ' '
*         IMPORTING
*       DIR_NAME               =
*       FILE_COUNTER           =
*       ERROR_COUNTER          =
      TABLES
        dir_list               = ta_file
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

  ELSE.
    MESSAGE text-001 TYPE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FETCHDATA_AL11
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fetchdata_al11 .

  IF ta_file IS NOT INITIAL.

    DELETE ta_file WHERE name NP co_pattern.

    CLEAR : wa_file.

    LOOP AT ta_file INTO wa_file.

      CLEAR : gv_inc_file, gv_arc_file.

      " Code Remediation S4 AL11 file path change **BEGIN OF CHANGE BY ABAP7 25.10.2026
*     CONCATENATE gv_inc wa_file-name INTO gv_inc_file SEPARATED BY '\'.
*     CONCATENATE gv_arc wa_file-name INTO gv_arc_file SEPARATED BY '\'.
      CONCATENATE gv_inc wa_file-name INTO gv_inc_file SEPARATED BY '/'.
      CONCATENATE gv_arc wa_file-name INTO gv_arc_file SEPARATED BY '/'.
      " Code Remediation S4 AL11 file path change **END OF CHANGE BY ABAP7 25.10.2026

      OPEN DATASET gv_inc_file FOR INPUT IN TEXT MODE ENCODING DEFAULT.

      OPEN DATASET gv_arc_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc = 0.
        DO.
          READ DATASET gv_inc_file INTO wa_tab.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          gv_string = wa_tab.
          WHILE gv_string <> space.
            IF wa_tab+0(3) CA 'id'.
              CLEAR : gv_string.
              TRANSFER wa_tab TO gv_arc_file.
            ELSE.
              SPLIT wa_tab AT '|' INTO wa_al11-id
                                       wa_al11-lifnr
                                       wa_al11-bukrs
                                       wa_al11-ekorg
                                       wa_al11-sperr
                                       wa_al11-sperr1
                                       wa_al11-sperm
                                       wa_al11-sperm1
                                       wa_al11-sperq
                                       wa_al11-zahls.
              APPEND wa_al11 TO ta_al11.
              CLEAR : wa_al11, gv_string.
            ENDIF.
          ENDWHILE.
          CLEAR : wa_tab.
        ENDDO.
      ELSE.
        MESSAGE text-003 TYPE 'E'.
      ENDIF.

      DELETE DATASET gv_inc_file.
      CLOSE DATASET gv_inc_file.
      CLOSE DATASET gv_arc_file.
      CLEAR : wa_file.
    ENDLOOP.
    CLEAR : gv_inc_file, gv_arc_file, gv_string.
  ELSE.
    MESSAGE text-002 TYPE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.
  CLEAR wa_al11.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data .

  DATA: lt_error   TYPE bapiret2_t,
        lt_success TYPE bapiret2_t,
        lv_lifnr   TYPE lifnr.

  LOOP AT ta_al11 INTO wa_al11.
    " S/4HANA: XK05 (batch input) and the payment block through VMD_EI_API
    " are replaced by one change of the business partner - ZFI_CIMMRA_BP.
    lv_lifnr = |{ wa_al11-lifnr ALPHA = IN }|.
    lcl_cimmra_bp=>block(
      EXPORTING iv_lifnr    = lv_lifnr
                iv_bukrs    = wa_al11-bukrs
                iv_ekorg    = wa_al11-ekorg
                iv_sperr    = wa_al11-sperr
                iv_sperr_cc = wa_al11-sperr1
                iv_sperm    = wa_al11-sperm
                iv_sperm_po = wa_al11-sperm1
                iv_sperq    = wa_al11-sperq
                iv_zahls    = wa_al11-zahls
      IMPORTING et_error    = lt_error
                et_success  = lt_success ).

    LOOP AT lt_error INTO DATA(ls_error).
      CLEAR wa_log.
      wa_log-lifnr = lv_lifnr.
      wa_log-tcode = 'BP'.
      wa_log-msg   = |ERROR: { ls_error-message }|.
      APPEND wa_log TO it_log.
    ENDLOOP.
    LOOP AT lt_success INTO DATA(ls_success).
      CLEAR wa_log.
      wa_log-lifnr = lv_lifnr.
      wa_log-tcode = 'BP'.
      wa_log-msg   = |SUCCESS: { ls_success-message }|.
      APPEND wa_log TO it_log.
    ENDLOOP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEND_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_data .

" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 26.06.2026  FOR ATC
*  SELECT SINGLE * FROM zparam_table INTO @DATA(wa_zparam)
*     WHERE id_name = 'CIM_VNRSP' AND param_name NE ''.
 SELECT * FROM ZPARAM_TABLE   UP TO 1 ROWS INTO @DATA(WA_ZPARAM) WHERE
ID_NAME = 'CIM_VNRSP' AND PARAM_NAME NE ''  ORDER BY  ID_NAME,ID_NUMBER
.   ENDSELECT.
" Code Remediation changes S4 2025_A Conversion * *END OF CHANGE BY ABAP7 26.06.2026 FOR ATC

  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Path for AL11 not maintained' TYPE 'I'.
  ENDIF.
  IF it_log IS NOT INITIAL.

    wa_response-vendor = 'VendorAccountNumber'.
    wa_response-vendortype = 'VendorType'.
    wa_response-t_code = 'TransactionCode'.
    wa_response-date   = 'CreatedOnDate'.
    wa_response-message = 'Message'.
    APPEND wa_response TO ta_response.
    CLEAR wa_response.

    LOOP AT it_log INTO wa_log.
      wa_response-vendor = wa_log-lifnr.
      wa_response-vendortype = 'VendorBlock'.
      wa_response-t_code = wa_log-tcode.

      CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum+0(4) INTO wa_response-date SEPARATED BY '/'.

      wa_response-message = wa_log-msg.

      APPEND wa_response TO ta_response.
      CLEAR wa_response.
    ENDLOOP.
  ENDIF.

  htab = cl_abap_char_utilities=>horizontal_tab.

  DATA(lv_count) = lines( ta_response ).

  IF ta_response[] IS NOT INITIAL AND lv_count > 1.

    DATA(lv_variant) = syst-slset.

    CLEAR : gv_al11_path.

    lv_date = sy-datlo.

    CONCATENATE lv_date+6(2) lv_date+4(2) lv_date+0(4) INTO lv_ddmmyyyy.

    " Code Remediation S4 AL11 file path change **BEGIN OF CHANGE BY ABAP7 25.10.2026
*   IF lv_variant IS INITIAL.
*     gv_al11_path = |{ wa_zparam-param_name }{ '\' }{ co_filename }{ lv_ddmmyyyy }{ sy-uzeit }{ '.csv' }|.
*   ELSE.
*     gv_al11_path = |{ wa_zparam-param_name }{ '\' }{ co_filename }{ lv_ddmmyyyy }{ sy-uzeit }{ '.csv' }|.
*   ENDIF.
    IF lv_variant IS INITIAL.
      gv_al11_path = |{ wa_zparam-param_name }{ '/' }{ co_filename }{ lv_ddmmyyyy }{ sy-uzeit }{ '.csv' }|.
    ELSE.
      gv_al11_path = |{ wa_zparam-param_name }{ '/' }{ co_filename }{ lv_ddmmyyyy }{ sy-uzeit }{ '.csv' }|.
    ENDIF.
    " Code Remediation S4 AL11 file path change **END OF CHANGE BY ABAP7 25.10.2026

    OPEN DATASET gv_al11_path IN TEXT MODE FOR OUTPUT ENCODING UTF-8.

    IF sy-subrc = 0.
      LOOP AT ta_response INTO wa_response.
        CONCATENATE
      wa_response-vendor
      wa_response-vendortype
      wa_response-t_code
      wa_response-date
      wa_response-message
      INTO lv_string SEPARATED BY co_comma.

        TRANSFER lv_string TO gv_al11_path.
        CLEAR : lv_string, wa_response.
      ENDLOOP.
    ENDIF.
    CLOSE DATASET gv_al11_path.
    MESSAGE text-004 TYPE 'S'.

  ELSE.
    MESSAGE text-005 TYPE 'I'.
  ENDIF.

ENDFORM.