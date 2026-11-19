*&---------------------------------------------------------------------*
*&  Include           ZFI_VEND_HONC_DD
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Title     : ZFI_VEND_HONC_DD
* Author            : Shivaji Ingole
* Program Type      : Include Program
* Creation Date     : 12-11-2020
* Transaction       : ZFI_VEND_HONC
* Functional design : FS_FI_344_V1.0
* Technical design  : TS_FI_344_V1.0
* Change request    : 8000006708
* Transport Request : CRDK974526, CRDK976286
* Description       : Include For Bank Key And Vendor Master Creation
*                     trasnsaction.
*-----------------------------------------------------------------------*
******Modification log
* Author            : Anuj Nandapurkar
* Program Type      : Excutable Program
* Change Date       : 27.05.2022
* Functional design : Pankaj Sawant
* Transaction       : ZFI_VEND_HONC
* Change request    : 8000009272
* Transport Request : CRDK988488
* Description       :Vendor code generation
*-----------------------------------------------------------------------*
*-----------------------------------------------------------------------*
******Modification log
* Author            : Mary Roopini A
* Program Type      : Excutable Program
* Change Date       : 22.11.2024, 25.11.2024
* Functional design : Veerabhadraroa
* Transaction       : ZFI_VEND_HONC
* Change request    : 8000004012
* Transport Request : CRDK993578
* Description       : Vendor Code - PAN combination to reflect in every CR
*-----------------------------------------------------------------------*
******Modification log
* Author            : Meghna Lekhwal
* Program Type      : Excutable Program
* Change Date       : 07.04.2026
* Functional design : Veerabhadraroa
* Transaction       : ZFI_VEND_HONC
* Change request    : 8000004166
* Transport Request : CRDK9A00F9
* Description       : changing uniqueid
*-----------------------------------------------------------------------*
******Modification log
* Change Date       : 18.11.2026
* Transaction       : ZFI_VEND_HONC
* Description       : S/4HANA conversion - the vendor is a business
*                     partner. FK01 / FK02 are gone in S/4HANA, so the
*                     batch input that created and changed the vendor is
*                     replaced by program ZFI_VEND_HONC (LCL_HONC_BP,
*                     CL_MD_BP_MAINTAIN): BP grouping and supplier roles
*                     from account group Z001, name / address / mobile /
*                     e-mail on the partner, GSTIN as the partner's tax
*                     number IN3, bank, company code 1000, withholding
*                     tax, PAN and GST classification on the supplier.
*                     The file from AL11, the checks, ZFI_HONC_VEND, the
*                     mails and the archive are unchanged.
*-----------------------------------------------------------------------*

*-----------------------------------------------------------------------*
****************************Type Declaration*****************************
*-----------------------------------------------------------------------*
TYPES : BEGIN OF ty_vendor,
          title(5)    TYPE c,
          vendor      TYPE lifnr,         "added by AC:CR-8000003939:09.10.2024
          name1       TYPE lfa1-name1,
          sort1       TYPE ad_sort1ul,
          street      TYPE ad_street,
          str_suppl1  TYPE ad_strspp1,
          str_suppl2  TYPE ad_strspp2,
          post_code1  TYPE ad_pstcd1,
          city1       TYPE ad_city1,
          country     TYPE land1,
*          region      TYPE bezei20, "Commented by Gajanan - 21.03.2024 - CR#8000003530
          region      TYPE regio, "Added by Gajanan - 21.03.2024 - CR#8000003530
          tel_number1 TYPE ad_tlnmbr1,
          tel_extens1 TYPE ad_tlxtns1,
          tel_number2 TYPE ad_tlnmbr1,
          tel_extens2 TYPE ad_tlxtns1,
          mob_number1 TYPE ad_mbnmbr1,
          mob_number2 TYPE ad_mbnmbr1,
          fax_number  TYPE ad_fxnmbr1,
          smtp_addr   TYPE ad_smtpadr,
          banks       TYPE banks,
          bankl       TYPE bankk,
          bankn       TYPE bankn,
          koinh       TYPE koinh_fi,
          curren      TYPE waers,
          pan         TYPE j_1ipanno,
***SOA by Gajanan - 15.03.2024 - CR#8000003530
          gst         TYPE stcd3,
"Soc atc correction abap7-22.09.2026
*          gstvenclass TYPE j_1imovend-ven_class, "added by meghna 05.04.2026 8000004166
          gstvenclass TYPE lfa1-ven_class, "added by meghna 05.04.2026 8000004166
"Eoc atc correction abap7-22.09.2026
          unique_id   TYPE zde_unique_id,
          flag(1),
          ind_key     TYPE brsch, "Added Lalitha CR#8000004166 21.02.2025
*          gstvenclass TYPE j_1imovend-ven_class, "Added Lalitha CR#8000004166 21.02.2025 "commented by meghna 05.04.2026 8000004166
***EOA by Gajanan - 15.03.2024 - CR#8000003530
          delete(1),     "added by Gauri Tandel - 8000003666 -11.06.2024
        END OF ty_vendor.

TYPES : BEGIN OF ty_data,
          rec(1000) TYPE c,
        END OF ty_data.

TYPES : BEGIN OF ty_lfbk,
          lifnr         TYPE lfbk-lifnr,
          banks         TYPE lfbk-banks,
          bankl         TYPE lfbk-bankl,
          bankn         TYPE lfbk-bankn,
          koinh         TYPE lfbk-koinh,
          ebpp_accname  TYPE lfbk-ebpp_accname,
          ebpp_bvstatus TYPE lfbk-ebpp_bvstatus,
        END OF ty_lfbk.
TYPES : BEGIN OF ty_movend,
"Soc atc correction abap7-22.09.2026
***          lifnr     TYPE j_1imovend-lifnr,
***          j_1ipanno TYPE j_1imovend-j_1ipanno,
          lifnr     TYPE lfa1-lifnr,
          j_1ipanno TYPE lfa1-j_1ipanno,
"Eoc atc correction abap7-22.09.2026
        END OF ty_movend.

TYPES : BEGIN OF ty_log,
          file  TYPE char128,
          name1 TYPE lfa1-name1,
          bankl TYPE bankk,
          pan   TYPE j_1ipanno,
          msg   TYPE char255,
        END OF ty_log,

        "" BOC done by Sushil as on 10.03.2021
        BEGIN OF ty_bnka,
          banks TYPE bnka-banks,
          bankl TYPE bnka-bankl,
          banka TYPE bnka-banka,
          provz TYPE bnka-provz,
        END OF ty_bnka.
"" EOC done by Sushil as on 10.03.2021

TYPES: BEGIN OF ty_lfb1, "added by Gauri Tandel - 8000003666 -11.06.2024
         lifnr TYPE lifnr,
         bukrs TYPE bukrs,
         loevm TYPE loevm,
       END OF ty_lfb1.

*******************************SOC by AC:CR-8000003939:15.10.2024***************************************
TYPES: BEGIN OF ty_lfa1,
         lifnr TYPE lifnr,
         loevm TYPE loevm,
       END OF ty_lfa1.
*******************************SOC by AC:CR-8000003939:15.10.2024***************************************
*-----------------------------------------------------------------------*
****************************Data Declaration*****************************
*-----------------------------------------------------------------------*
DATA : ta_log TYPE TABLE OF ty_log,
       wa_log TYPE ty_log.
DATA : ta_movend TYPE TABLE OF ty_movend,
       wa_movend TYPE ty_movend.
DATA : ta_t005u TYPE TABLE OF t005u.
DATA : ta_lfbk TYPE TABLE OF ty_lfbk,
       wa_lfbk TYPE ty_lfbk.             "BOC by Gauri Tandel - 8000003666 -11.06.2024
DATA: ta_lfb1       TYPE TABLE OF ty_lfb1,
      wa_lfb1       TYPE ty_lfb1,
      ta_lfb1_pan   TYPE TABLE OF ty_lfb1,
      ta_lfb1_pan_n TYPE TABLE OF ty_lfb1,
      wa_lfb1_pan   TYPE ty_lfb1.          "EOC by Gauri Tandel - 8000003666 -11.06.2024
DATA : ta_data     TYPE TABLE OF ty_vendor,
       ta_data_tmp TYPE TABLE OF ty_vendor.
DATA : ta_bnka TYPE TABLE OF ty_bnka, " LOC added by Sushil as on 10.03.2021
       wa_bnka TYPE ty_bnka.
DATA : gv_file TYPE eps2filnam,
       wa_data TYPE ty_vendor.
* S/4HANA: no batch input any more (BDCDATA, BDCMSGCOLL) - the vendor is
* posted through LCL_HONC_BP, which takes these:
TYPES: BEGIN OF ty_bp_cin,                 " supplier: PAN, GST class
         j_1ipanno TYPE lfa1-j_1ipanno,
         ven_class TYPE lfa1-ven_class,
       END OF ty_bp_cin,
       BEGIN OF ty_bp_ckey,
         bukrs TYPE bukrs,
       END OF ty_bp_ckey,
       BEGIN OF ty_bp_cdata,                " supplier: company code
         akont TYPE lfb1-akont,
         zterm TYPE lfb1-zterm,
         reprf TYPE lfb1-reprf,
         zwels TYPE lfb1-zwels,
         qland TYPE lfb1-qland,
       END OF ty_bp_cdata,
       BEGIN OF ty_bp_comp,
         data_key TYPE ty_bp_ckey,
         data     TYPE ty_bp_cdata,
       END OF ty_bp_comp,
       BEGIN OF ty_bp_none,                 " no purchasing organisation
         none TYPE c LENGTH 1,
       END OF ty_bp_none,
       BEGIN OF ty_bp_tel,
         telephone TYPE ad_tlnmbr1,
         extension TYPE ad_tlxtns1,
         r_3_user  TYPE c LENGTH 1,         " space landline, 3 mobile
       END OF ty_bp_tel,
       ty_bp_tel_t TYPE STANDARD TABLE OF ty_bp_tel WITH EMPTY KEY,
       BEGIN OF ty_bp_fax,
         fax TYPE ad_fxnmbr1,
       END OF ty_bp_fax,
       ty_bp_fax_t TYPE STANDARD TABLE OF ty_bp_fax WITH EMPTY KEY,
       BEGIN OF ty_bp_mail,
         e_mail TYPE ad_smtpadr,
       END OF ty_bp_mail,
       ty_bp_mail_t TYPE STANDARD TABLE OF ty_bp_mail WITH EMPTY KEY,
       BEGIN OF ty_bp_bank,
         banks TYPE banks,
         bankl TYPE bankk,
         bankn TYPE bankn,
         koinh TYPE koinh_fi,
       END OF ty_bp_bank,
       ty_bp_bank_t TYPE STANDARD TABLE OF ty_bp_bank WITH EMPTY KEY,
       BEGIN OF ty_bp_wtax,
         witht     TYPE lfbw-witht,
         wt_withcd TYPE lfbw-wt_withcd,
         wt_subjct TYPE lfbw-wt_subjct,
       END OF ty_bp_wtax,
       ty_bp_wtax_t TYPE STANDARD TABLE OF ty_bp_wtax WITH EMPTY KEY.
DATA : ta_noifsc   TYPE TABLE OF ty_vendor,
       ta_nullifsc TYPE TABLE OF ty_vendor, "BY FATIMA FOR 8000007904
       ta_crvend   TYPE TABLE OF ty_vendor,
       ta_chvend   TYPE TABLE OF ty_vendor.
DATA : ta_email TYPE TABLE OF zfi_vend_email,
       wa_email TYPE zfi_vend_email.
DATA : ta_records TYPE TABLE OF ty_data.
DATA : wa_records TYPE ty_data.
DATA : ta_files TYPE TABLE OF eps2fili.
DATA : gv_dir TYPE eps2filnam.
DATA : gv_akont TYPE lfb1-akont.
DATA : wa_files TYPE eps2fili.
DATA : gv_source_file TYPE sapb-sappfad.
DATA : gv_target_path TYPE sapb-sappfad.
DATA : gv_witht TYPE zparam_table-param_name."By Shivaji 8000006708 CRDK976286
DATA : gv_withcd TYPE zparam_table-param_name."By Shivaji 8000006708 CRDK976286
DATA : gv_payment_term TYPE zparam_table-param_name."By Shivaji 8000006708 CRDK976286

*Start of changes by Anuj Nandapurkar as part of CR8000009272 dated 16-10-2023
DATA: ta_lfbk1   TYPE TABLE OF ty_vendor,
      wa_lfbk1   TYPE ty_vendor,
      ta_movend1 TYPE TABLE OF ty_vendor,
      wa_movend1 TYPE ty_vendor,
      ta_pan     TYPE TABLE OF ty_vendor,
      wa_pan     TYPE ty_vendor,
      ta_delete  TYPE TABLE OF ty_vendor. "added by Gauri Tandel - 8000003666 -11.06.2024

TYPES: BEGIN OF ty_data1,
         name1   TYPE lfa1-name1,
         sort1   TYPE ad_sort1ul,
         bankl   TYPE bankk,
         bankn   TYPE bankn,
         pan     TYPE j_1ipanno,
         remarks TYPE string,
         vendor  TYPE lifnr, "added by Mary Roopini A - 8000004012- 22.11.2024
       END OF ty_data1.

DATA: ta_data1 TYPE TABLE OF ty_data1,
      wa_data1 TYPE ty_data1.
*End of changes by Anuj Nandapurkar as part of CR8000009272 dated 16-10-2023

DATA: ta_honc     TYPE TABLE OF zfi_honc_vend,
      ta_honc_tmp TYPE TABLE OF zfi_honc_vend,  "added by Gauri Tandel - 8000003666 -17.06.2024
      wa_honc     TYPE zfi_honc_vend.

DATA:lv_string TYPE string.


*******************************SOC by AC:CR-8000003939:15.10.2024***************************************
DATA : ta_lfa1   TYPE TABLE OF ty_lfa1,
       ta_lfa1_n TYPE TABLE OF ty_lfa1,
       wa_lfa1   TYPE ty_lfa1,
       wa_lfa1_n TYPE ty_lfa1.
*******************************EOC by AC:CR-8000003939:15.10.2024***************************************

*-----------------------------------------------------------------------*
************************Constant Declaration*****************************
*-----------------------------------------------------------------------*
CONSTANTS : co_s      TYPE c VALUE 'S',
            co_e      TYPE c VALUE 'E',
            co_remark TYPE string VALUE 'Bank Key does not Exists in SAP'.   "added by Gauri Tandel - 8000003829 -13.08.2024

DATA : lv_index  TYPE sy-index,
       lv_index1 TYPE sy-index.
