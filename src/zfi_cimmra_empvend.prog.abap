*&---------------------------------------------------------------------*
*& Report  ZFI_CIMMRA_EMPVEND
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
* Program
* Title           : Cimmra Employee vendor creation and change
* Author          : Uday Reddy
* Program Type    : <Report>
* Creation Date   : 05-02-2026
* Transaction     :
* Functional design: Arijit podder
* Change request : 8000004648
* Transport request: CRDK999690
* Description     : Vendor creation and change using
*                   RFCs ZMM_RFC_VENDOR_CREATE and ZMM_RFC_VENDOR_CHANGE.
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Modification Information
* Date             : 17.11.2026
* Author           :
* Change request   :
* Transport number :
* Description      : S/4HANA - the vendor is created / changed as a business
*                    partner. The calls to RFC ZMM_RFC_VENDOR_CREATE and
*                    ZMM_RFC_VENDOR_CHANGE are replaced by
*                    LCL_CIMMRA_BP=>CREATE / =>CHANGE (include ZFI_CIMMRA_BP),
*                    which post through CL_MD_BP_MAINTAIN: BP grouping and
*                    supplier roles from the account group, GSTIN as the
*                    partner's tax number. Same parameters, same LT_ERROR /
*                    LT_SUCCESS - the response file is unchanged.
*----------------------------------------------------------------------*
REPORT zfi_cimmra_empvend.

TYPES : BEGIN OF ty_data,
          id                 TYPE lfa1-emnfr,       "Id
          lifnr	             TYPE lfa1-lifnr,       "VendorAccountNumber
          bukrs	             TYPE lfb1-bukrs,       "CompanyCode
          ekorg              TYPE lfm1-ekorg,       "PurchasingOrganization
          ktokk              TYPE lfa1-ktokk,       "VendorAccountGroup
          alwaysx            TYPE char1,
          title_text         TYPE string,  "LFA1          "TitleText
          name1              TYPE lfa1-name1,       "Name1
          name2              TYPE lfa1-name1,      "Name2
          search_term1       TYPE lfa1-name1,       "SearchTerm1
          street2            TYPE lfa1-stras,                  "Street2
          street3            TYPE lfa1-stras,                  "Street3
          stras              TYPE lfa1-stras,       "Street
          ort02              TYPE lfa1-ort02,       "District
          pstlz              TYPE lfa1-pstlz,       "CityPostalCode
          ort01              TYPE lfa1-ort01,       "City
          land1              TYPE lfa1-land1,       "CountryKey
          regio              TYPE lfa1-regio,       "Region
          spras              TYPE lfa1-spras,       "Languagekey
          first_telephone_no TYPE string,           "FirstTelephoneNo
          telf1              TYPE lfa1-telf1,       "FirstMobileTelephoneNo
          firstfaxno         TYPE string,                  "FirstFaxNo
          email              TYPE string,                        "Email
          taxnumber3         TYPE string,                        "TaxNumber3
          brsch              TYPE lfa1-brsch,               "IndustryKey
" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 29.06.2026 FOR ATC
*          j_1ipanno          TYPE j_1imovend-j_1ipanno,     "PermanentAccountNumber
          j_1ipanno          TYPE lfa1-j_1ipanno,     "PermanentAccountNumber
" Code Remediation changes S4 2025_A Conversion **END OF CHANGE BY ABAP7 29.06.2026 FOR ATC
" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 29.06.2026 FOR ATC
*          ven_class          TYPE j_1imovend-ven_class, "VendorClassificationForGST
          ven_class          TYPE lfa1-ven_class, "VendorClassificationForGST
" Code Remediation changes S4 2025_A Conversion **END OF CHANGE BY ABAP7 29.06.2026 FOR ATC
          banks              TYPE lfbk-banks,        "BankCountryKey
          bankl              TYPE lfbk-bankl,        "BankKeys
          bankn              TYPE lfbk-bankn,        "BankAccountNumber
          koinh              TYPE lfbk-koinh,        "AccountHolderName
          akont              TYPE lfb1-akont,        "ReconciliationAccountInGeneralLedger
          fdgrv              TYPE lfb1-fdgrv,        "PlanningGroup
          altkn              TYPE lfb1-altkn,        "PreviousMasterRecordNumber
          reprf              TYPE lfb1-reprf,        "CheckFlagForDoubleInvoicesOrCreditMemos
          zwels              TYPE lfb1-zwels,        "ListOfThePaymentMethodsToBeConsidered
          waers              TYPE lfm1-waers,        "PurchaseOrderCurrency
          kalsk              TYPE lfm1-kalsk,        "GroupForCalculationSchema
          webre              TYPE lfm1-webre,        "IndicatorGRBased
          gst_code           TYPE string,            "VendorClassificationForGSTCode
        END OF ty_data.


TYPES : BEGIN OF ty_tab,
          rec(2000),
        END OF ty_tab.

TYPES : BEGIN OF ty_sft,
          id         TYPE lfa1-emnfr,
          vendor     TYPE lfa1-lifnr,
          vendortype TYPE string,
          message    TYPE string,
          date       TYPE char10,
          adrc       TYPE string,
          msg        TYPE string,
        END OF ty_sft.

TYPES: BEGIN OF ty_al11_data,
         id         TYPE lfa1-emnfr,
         vendor     TYPE lfa1-lifnr,
         vendortype TYPE string,
         date       TYPE char10,
         plant      TYPE char5,
         part_func  TYPE string,
         part_count TYPE string,
         ref        TYPE string,
         message    TYPE string,
       END OF ty_al11_data.

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
       ta_sft       TYPE TABLE OF ty_sft,
       wa_sft       TYPE ty_sft,
       lr_setnam    TYPE RANGE OF setleaf-setname,
       lr_sys       TYPE sy-datum,
       lr_dat       TYPE char10,
       ta_sftp1     TYPE TABLE OF ty_al11_data,
       wa_sftp1     TYPE ty_al11_data,
       htab(1)      TYPE c,
       gv_al11_path TYPE string,
       lv_date      TYPE sy-datum,
       lv_ddmmyyyy  TYPE char10,
       lv_string    TYPE string.

DATA :  lv_lifnr           TYPE lifnr,
        ls_central         TYPE  vmds_ei_vmd_central,
        ls_postal_addr     TYPE  cvis_ei_1vl,
        ls_company_data    TYPE  zkrp_vmds_ei_company,
        ls_cin             TYPE  zkrp_cin_det_s,
        ls_purchasing_data TYPE  zkrp_vmds_ei_purchasing.

DATA : lt_bank        TYPE zkrp_bank_det,
       ls_bank        TYPE zkrp_ven_bank,
       lt_mobile_addr TYPE  zkrp_mobile_addr,
       ls_mobile_addr TYPE  zkrp_mobile_s,
       lt_smtp_addr   TYPE  zkrp_smtp_addr,
       ls_smtp_addr   TYPE  zkrp_smtp_s,
       lt_error       TYPE TABLE OF bapiret2,
       lt_success     TYPE TABLE OF bapiret2.


CONSTANTS : co_inc_file      TYPE zparam_table-id_name VALUE 'CIM_VNCRT',
            co_arc_file      TYPE zparam_table-id_name VALUE 'ZFICIM_ARC',
            co_dom(20)       VALUE 'DOM_VENDORS',
            co_imp(20)       VALUE 'IMPORT_VENDORS',
            co_imp_mrp(20)   VALUE 'IMPORT_MRP_VENDORS',
            co_dom_price(25) VALUE 'DOMESTIC_PRICING_VENDORS',
            co_sez(20)       VALUE 'SEZ_VENDORS',
            co_filename(20)  VALUE 'CIM_EMPVEND_RESP',
            co_comma(1)      VALUE ',',
            co_pattern(20)   VALUE 'CIM_EMPVEND*'.


INCLUDE zfi_cimmra_bp.

START-OF-SELECTION.
PERFORM get_file.
PERFORM fetchdata_al11.
PERFORM calling_rfc.
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
                                       wa_al11-ktokk
                                       wa_al11-alwaysx
                                       wa_al11-title_text
                                       wa_al11-name1
                                       wa_al11-name2
                                       wa_al11-search_term1
                                       wa_al11-street2
                                       wa_al11-street3
                                       wa_al11-stras
                                       wa_al11-ort02
                                       wa_al11-pstlz
                                       wa_al11-ort01
                                       wa_al11-land1
                                       wa_al11-regio
                                       wa_al11-spras
                                       wa_al11-first_telephone_no
                                       wa_al11-telf1
                                       wa_al11-firstfaxno
                                       wa_al11-email
                                       wa_al11-taxnumber3
                                       wa_al11-brsch
                                       wa_al11-j_1ipanno
                                       wa_al11-ven_class
                                       wa_al11-banks
                                       wa_al11-bankl
                                       wa_al11-bankn
                                       wa_al11-koinh
                                       wa_al11-akont
                                       wa_al11-fdgrv
                                       wa_al11-altkn
                                       wa_al11-reprf
                                       wa_al11-zwels
                                       wa_al11-waers
                                       wa_al11-kalsk
                                       wa_al11-webre
                                       wa_al11-gst_code.

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
*&      Form  CALLING_RFC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calling_rfc .

  DATA(lt_string_rows) = VALUE string_table( ).

  SELECT lifnr,erdat,ktokk,emnfr FROM lfa1 INTO TABLE @DATA(lta_lfa1)
  FOR ALL ENTRIES IN @ta_al11
  WHERE emnfr = @ta_al11-id.                            "#EC CI_NOFIELD

***SOC by Uday Reddy CR#8000004921 on 13.03.2026.
  SELECT * FROM setleaf INTO TABLE @DATA(ta_setleaf)
    WHERE setname = 'CIMM_EMP_VEN'.
***EOC by Uday Reddy CR#8000004921 on 13.03.2026.

  SELECT * FROM tsad3t INTO TABLE @DATA(lta_title).

  SELECT land1, waers FROM t500w INTO TABLE @DATA(ta_t500w)
     FOR ALL ENTRIES IN @ta_al11
     WHERE land1 = @ta_al11-land1.


  "schema grp vendor (kalsk)
  REFRESH: lr_setnam.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'DOM_VENDORS' ) TO lr_setnam.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'IMPORT_VENDORS' ) TO lr_setnam.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'IMPORT_MRP_VENDORS' ) TO lr_setnam.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'DOMESTIC_PRICING_VENDORS' ) TO lr_setnam.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = 'SEZ_VENDORS' ) TO lr_setnam.

  SELECT * FROM setleaf INTO TABLE @DATA(ta_set)
   WHERE setname IN @lr_setnam.

  LOOP AT ta_al11 INTO wa_al11.
***SOC by Uday Reddy CR#8000004921 on 17.03.2026.
    READ TABLE ta_setleaf INTO DATA(wa_setleaf) WITH KEY valfrom = wa_al11-ktokk.
    IF sy-subrc = 0.
***EOC by Uday Reddy CR#8000004921 on 17.03.2026.
      READ TABLE lta_lfa1 INTO DATA(lwa_lfa1) WITH KEY emnfr = wa_al11-id
                                                       ktokk = wa_setleaf-valfrom.
      IF sy-subrc = 0.
        wa_sft-vendortype = 'EMPVEND'.
        CONCATENATE 'Id '  wa_al11-id  ' is duplicate' INTO wa_sft-message.
        APPEND wa_sft TO ta_sft.
        CLEAR wa_sft.
        CONTINUE.
      ENDIF.
    ENDIF.        "Added by Uday Reddy CR#8000004921 on 17.03.2026.
*    ELSE.
    ls_central-data-emnfr  = wa_al11-id.
    ls_central-data-brsch = wa_al11-brsch.

    ls_central-data-brsch = |{ ls_central-data-brsch ALPHA = IN }|.

    ls_central-data-ktokk = wa_al11-ktokk.
    ls_central-data-konzs = 'EMPLY_VEND'.
    ls_central-data-taxbs = '0'.
    ls_cin-lifnr = wa_al11-lifnr.
    ls_cin-j_1ipanno = wa_al11-j_1ipanno.
    ls_cin-ven_class = wa_al11-gst_code.


    ls_company_data-data_key-bukrs = wa_al11-bukrs.
    ls_company_data-data-akont = wa_al11-akont.
    ls_company_data-data-zwels = wa_al11-zwels.
    ls_company_data-data-reprf = 'X'.
    ls_company_data-data-altkn = wa_al11-lifnr.
    ls_company_data-data-fdgrv = 'VE'.

    ls_purchasing_data-data_key-ekorg = wa_al11-ekorg.       "Added by Uday Reddy CR#8000004921 on 02.04.2026.


    READ TABLE lta_title INTO DATA(wa_title) WITH KEY title_medi = wa_al11-title_text langu = 'E'.
    IF sy-subrc = 0.
      ls_postal_addr-data-title = wa_title-title.
    ENDIF.
    ls_postal_addr-data-name = wa_al11-name1.
    ls_postal_addr-data-city = wa_al11-ort01.
    ls_postal_addr-data-district = wa_al11-ort02.
    ls_postal_addr-data-postl_cod1 = wa_al11-pstlz.
    ls_postal_addr-data-country = wa_al11-land1.
    ls_postal_addr-data-langu = wa_al11-spras.
    ls_postal_addr-data-sort1 = wa_al11-name1.


    READ TABLE ta_t500w INTO DATA(wa_t500w) WITH KEY land1 = wa_al11-land1.
    IF sy-subrc = 0.
      ls_purchasing_data-data-waers = wa_t500w-waers.
    ENDIF.

    ls_purchasing_data-data-webre = 'X'.
    ls_purchasing_data-data-kzaut = 'X'.

    READ TABLE ta_set INTO DATA(wa_set) WITH KEY valfrom = wa_al11-ktokk.
    IF sy-subrc = 0.
      IF wa_set-setname = co_dom.
        ls_purchasing_data-data-kalsk = '01'.
      ELSEIF wa_set-setname = co_imp.
        ls_purchasing_data-data-kalsk = '02'.
      ELSEIF wa_set-setname = co_imp_mrp.
        ls_purchasing_data-data-kalsk = '03'.
      ELSEIF wa_set-setname = co_dom_price.
        ls_purchasing_data-data-kalsk = '04'.
      ELSEIF wa_set-setname =  co_sez.
        ls_purchasing_data-data-kalsk = '05'.
      ENDIF.
    ELSE.
      ls_purchasing_data-data-kalsk = ''.  "incase no account grp maintained
    ENDIF.


    ls_bank-banks = wa_al11-banks.
    ls_bank-bankl = wa_al11-bankl.
    ls_bank-bankn = wa_al11-bankn.
    ls_bank-koinh = wa_al11-koinh.
    APPEND ls_bank TO lt_bank.


    CLEAR lt_string_rows.
    SPLIT wa_al11-telf1 AT ';' INTO TABLE lt_string_rows.
    READ TABLE lt_string_rows INTO DATA(lwa_telf1) INDEX 1.

    DATA(lv_cons) = 001.
    LOOP AT lt_string_rows ASSIGNING FIELD-SYMBOL(<fs_rows>).
      CLEAR:ls_mobile_addr.
      ls_mobile_addr-telephone = <fs_rows>.
      ls_mobile_addr-caller_no = <fs_rows>.
      ls_mobile_addr-r_3_user = '2'.
      ls_mobile_addr-consnumber = lv_cons.
      lv_cons = lv_cons + 001.
      APPEND ls_mobile_addr TO lt_mobile_addr.
    ENDLOOP.


    CLEAR lt_string_rows.
    SPLIT wa_al11-email AT ';' INTO TABLE lt_string_rows.
    DATA(lv_consnum) = 001.
    LOOP AT lt_string_rows ASSIGNING FIELD-SYMBOL(<fs_string_rows>).
      CLEAR: ls_smtp_addr.

      ls_smtp_addr-e_mail = <fs_string_rows>.
      ls_smtp_addr-consnumber = lv_consnum.
      lv_consnum = lv_consnum + 001.
      APPEND ls_smtp_addr TO lt_smtp_addr.
    ENDLOOP.

***SOC by Uday Reddy CR#8000004921 on 02.04.2026.
*    lv_lifnr = wa_al11-lifnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_al11-lifnr
      IMPORTING
        output = lv_lifnr.
***EOC by Uday Reddy CR#8000004921 on 02.04.2026.

    " S/4HANA: the vendor is posted as a business partner - include ZFI_CIMMRA_BP.
    " (was CALL FUNCTION 'ZMM_RFC_VENDOR_CREATE')
    lcl_cimmra_bp=>create(
      EXPORTING iv_lifnr   = lv_lifnr
                is_central = ls_central
                is_postal  = ls_postal_addr
                is_company = ls_company_data
                is_cin     = ls_cin
                is_purch   = ls_purchasing_data
                it_mobile  = lt_mobile_addr
                it_smtp    = lt_smtp_addr
                it_bank    = lt_bank
      IMPORTING et_error   = lt_error
                et_success = lt_success ).

    IF lt_success IS NOT INITIAL.
      READ TABLE lt_success INTO DATA(wa_success) INDEX 1.
      IF sy-subrc = 0.
        SPLIT wa_success-message AT ';' INTO wa_sft-vendor wa_sft-msg.
        SPLIT wa_sft-msg AT ' ' INTO wa_sft-adrc wa_sft-message.
        lr_sys = sy-datum.
        CONCATENATE lr_sys+6(2) '.' lr_sys+4(2) '.' lr_sys+0(4) INTO lr_dat.

        wa_sft-date = lr_dat.
        wa_sft-id = wa_al11-id.
        wa_sft-vendortype = 'EMPVEND'.
        APPEND wa_sft TO ta_sft.
      ENDIF.
      CLEAR: wa_sft,wa_success.
    ENDIF.


    IF lt_error IS NOT INITIAL.
      READ TABLE lt_error INTO DATA(wa_error) INDEX 1.
      IF sy-subrc = 0.
        wa_sft-id = wa_al11-id.
        wa_sft-vendortype = 'EMPVEND'.
        wa_sft-date = sy-datum.
        wa_sft-message = wa_error-message.
        APPEND wa_sft TO ta_sft.
      ENDIF.
      CLEAR: wa_sft,wa_error.
    ENDIF.
*    ENDIF.
    CLEAR : wa_al11, lv_lifnr, ls_central, ls_cin, ls_company_data, ls_postal_addr, ls_purchasing_data.
    CLEAR : ls_mobile_addr, lt_mobile_addr, ls_smtp_addr, lt_smtp_addr,ls_bank, lt_bank, lt_error, lt_success.
    CLEAR : wa_set, wa_t500w, wa_title, lwa_lfa1.

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
ID_NAME = 'CIM_VNRSP' AND PARAM_NAME NE ''  ORDER BY  ID_NAME ,ID_NUMBER
.   ENDSELECT.
" Code Remediation changes S4 2025_A Conversion * *END OF CHANGE BY ABAP7 26.06.2026 FOR ATC

  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Path for AL11 not maintained' TYPE 'I'.
  ENDIF.
  IF ta_sft IS NOT INITIAL.

    SELECT lifnr,ekorg,werks,parvw,parza,lifn2 FROM wyt3
      INTO TABLE @DATA(ta_partner)
      FOR ALL ENTRIES IN @ta_sft
      WHERE lifnr = @ta_sft-vendor.

*    PERFORM fill_header.
    wa_sftp1-id     = 'Id' .
    wa_sftp1-vendor  = 'SAPVendorNo'.
    wa_sftp1-vendortype = 'VendorType'.
    wa_sftp1-date   = 'CreatedOnDate'.
    wa_sftp1-plant  = 'Plant'.
    wa_sftp1-part_func = 'PartnerFunction'.
    wa_sftp1-part_count = 'PartnerCounter'.
    wa_sftp1-ref = 'ReftoOthervVendor'.
    wa_sftp1-message = 'Message'.
    APPEND wa_sftp1 TO ta_sftp1.
    CLEAR wa_sftp1.

    LOOP AT ta_sft INTO wa_sft.
      wa_sftp1-id    =  wa_sft-id.
      wa_sftp1-vendor = wa_sft-vendor.
      wa_sftp1-vendortype = wa_sft-vendortype.
      wa_sftp1-date   = wa_sft-date.
      READ TABLE ta_partner INTO DATA(wa_partner) WITH KEY lifnr = wa_sft-vendor.
      IF sy-subrc = 0.
        "S.O.A by Vema Cr8000004648 on 27.02.2026.
        DATA(lv_index) = sy-tabix.
" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 26.06.2026  FOR ATC
        LOOP AT ta_partner INTO DATA(wa_partner2) FROM lv_index. "#EC CI_NOORDER
" Code Remediation changes S4 2025_A Conversion * *END OF CHANGE BY ABAP7 26.06.2026  FOR ATC
          IF wa_partner-lifnr <> wa_partner2-lifn2.
            EXIT.
          ELSE.
            wa_sftp1-part_func = |{  wa_sftp1-part_func }-{ wa_partner-parvw }|.
          ENDIF.
        ENDLOOP.
        CLEAR : lv_index,wa_partner2.
        "E.O.A by Vema Cr8000004648 on 27.02.2026.

        wa_sftp1-plant = wa_partner-werks.
*        wa_sftp1-part_func = wa_partner-parvw.
        wa_sftp1-part_count = wa_partner-parza.
        wa_sftp1-ref = wa_partner-lifn2.
      ENDIF.

      wa_sftp1-message = wa_sft-message.
      APPEND wa_sftp1 TO ta_sftp1.
      CLEAR: wa_sftp1,wa_sft.
    ENDLOOP.
  ENDIF.

  htab = cl_abap_char_utilities=>horizontal_tab.
  DATA(lv_count) = lines( ta_sftp1 ).
  IF ta_sftp1[] IS NOT INITIAL AND lv_count > 1.

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
      LOOP AT ta_sftp1 INTO wa_sftp1.

        CONCATENATE
          wa_sftp1-id
          wa_sftp1-vendor
          wa_sftp1-vendortype
          wa_sftp1-date
          wa_sftp1-plant
          wa_sftp1-part_func
          wa_sftp1-part_count
          wa_sftp1-ref
          wa_sftp1-message
         INTO lv_string SEPARATED BY co_comma.

        TRANSFER lv_string TO gv_al11_path.
        CLEAR: lv_string, wa_sftp1.
      ENDLOOP.
    ENDIF.
    CLOSE DATASET gv_al11_path.
    MESSAGE text-004 TYPE 'S'.

  ELSE.
    MESSAGE text-005 TYPE 'I'.
  ENDIF.

ENDFORM.