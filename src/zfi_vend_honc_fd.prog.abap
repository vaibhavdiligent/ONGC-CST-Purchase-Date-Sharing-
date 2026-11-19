*&---------------------------------------------------------------------*
*&  Include           ZFI_VEND_HONC_FD
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Program Title     : ZFI_VEND_HONC_DD
* Author            : Shivaji Ingole, Sushil
* Program Type      : Include Program
* Creation Date     : 12-11-2020
* Transaction       : ZFI_VEND_HONC
* Functional design : FS_FI_344_V1.0
* Technical design  : TS_FI_344_V1.0
* Change request    : 8000006708
* Transport Request : CRDK974526, CRDK976287, CRDK976845
* Description       : Include For Bank Key And Vendor Master Creation
*                     trasnsaction.
*-----------------------------------------------------------------------*
******Modification log
* Author            : Fatima zia
* Program Type      : Excutable Program
* Change Date       : 27.05.2022
* Transaction       : ZFI_VEND_HONC
* Change request    : 8000007904
* Transport Request : CRDK983367
* Description       :Vendor code generation
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
* Change Date       : 22.11.2024, 25.11.2024, 26.11.2024
* Functional design : Veerabhadraroa
* Transaction       : ZFI_VEND_HONC
* Change request    : 8000004012
* Transport Request : CRDK993578
* Description       : Vendor Code - PAN combination to reflect in every CR
*-----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GOTO_TMG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM goto_tmg .
  CONSTANTS : lc_viewname TYPE dd02v-tabname VALUE 'ZFI_VEND_EMAIL'.

  CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
    EXPORTING
      action                       = 'U'
      view_name                    = lc_viewname
    EXCEPTIONS
      client_reference             = 1
      foreign_lock                 = 2
      invalid_action               = 3
      no_clientindependent_auth    = 4
      no_database_function         = 5
      no_editor_function           = 6
      no_show_auth                 = 7
      no_tvdir_entry               = 8
      no_upd_auth                  = 9
      only_show_allowed            = 10
      system_failure               = 11
      unknown_field_in_dba_sellist = 12
      view_not_found               = 13
      maintenance_prohibited       = 14
      OTHERS                       = 15.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_FILES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_files .
  CONSTANTS lc_id TYPE zparam_table-id_name VALUE 'ZFI_VEND_H'.
  CONSTANTS lc_id1 TYPE zparam_table-id_name VALUE 'ZFI_VEND_G'.

  ##WARN_OK
  SELECT SINGLE param_name  "#EC CI_NOORDER
      FROM zparam_table
      INTO gv_akont
      WHERE id_name = lc_id1.

  ##WARN_OK
  " Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 09.09.2026 FOR ATC
*  SELECT SINGLE param_name
*    FROM zparam_table
*    INTO @DATA(lv_dir)
*    WHERE id_name = @lc_id.
  SELECT param_name
    FROM zparam_table
    INTO @DATA(lv_dir)
    UP TO 1 ROWS
    WHERE id_name = @lc_id
    ORDER BY PRIMARY KEY.
   endselect.
 " Code Remediation changes S4 2025_A Conversion **END OF CHANGE BY ABAP7 09.09.2026 FOR ATC
  IF sy-subrc = 0.
    gv_dir = lv_dir.
    CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
      EXPORTING
        iv_dir_name            = gv_dir
*       FILE_MASK              = ' '
*     IMPORTING
*       DIR_NAME               =
*       FILE_COUNTER           =
*       ERROR_COUNTER          =
      TABLES
        dir_list               = ta_files
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
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.
    MESSAGE 'Please Maintain Directory Path Parameter'(003) TYPE co_s DISPLAY LIKE co_e.
    LEAVE TO CURRENT TRANSACTION.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FETCH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fetch_data .
  CONSTANTS : lc_in TYPE t005u-land1 VALUE 'IN'.
  CONSTANTS : lc_withcd TYPE zparam_table-id_name VALUE 'EAS_TDS_HN'. "By Shivaji 8000006708 CRDK976286
  CONSTANTS : lc_witht TYPE zparam_table-id_name VALUE 'EAS_TDS1_H'."By Shivaji 8000006708 CRDK976286
  CONSTANTS : lc_payment_term TYPE zparam_table-id_name VALUE 'EAS_PT_HN'."By Shivaji 8000006708 CRDK976286

  IF ta_data IS NOT INITIAL.
    SELECT lifnr banks bankl bankn koinh ebpp_accname ebpp_bvstatus
      FROM lfbk
      INTO TABLE ta_lfbk
      FOR ALL ENTRIES IN ta_data
*      WHERE bankl = ta_data-bankl.              "commented by Amol Chavan  on 24.10.2024 15:43:04"
      WHERE bankn = ta_data-bankn.              "Added by Amol Chavan CR  on 24.10.2024 15:43:04"
    IF sy-subrc = 0.
*      SORT ta_lfbk BY lifnr.
      SORT ta_lfbk BY bankl bankn.
    ENDIF.

    SELECT
      banks bankl banka provz FROM bnka INTO TABLE ta_bnka
            FOR  ALL ENTRIES IN ta_data
            WHERE banks = ta_data-banks AND
                  bankl = ta_data-bankl.

    SORT ta_bnka BY bankl.

  ENDIF.

  SELECT mandt email mail cc
    FROM zfi_vend_email
    INTO TABLE ta_email.

  IF ta_data IS NOT INITIAL.
    SELECT lifnr j_1ipanno
"Soc atc correction abap7-22.09.2026
***      FROM j_1imovend
      FROM lfa1
"Eoc atc correction abap7-22.09.2026
      INTO TABLE ta_movend
      FOR ALL ENTRIES IN ta_data
      WHERE j_1ipanno = ta_data-pan.
    IF sy-subrc = 0.
      SORT ta_movend BY j_1ipanno.
    ENDIF.
  ENDIF.

  SELECT mandt spras land1 bland bezei
    FROM t005u
    INTO TABLE ta_t005u
    WHERE spras = 'E'
      AND land1 = lc_in.
  IF sy-subrc = 0.
    LOOP AT ta_t005u ASSIGNING FIELD-SYMBOL(<fs_t005u>).
      TRANSLATE <fs_t005u>-bezei TO UPPER CASE.
    ENDLOOP.
  ENDIF.

* Start By Shivaji 8000006708 CRDK976286
  SELECT SINGLE param_name
    FROM zparam_table
    INTO gv_witht
    WHERE id_name = lc_witht.

  SELECT SINGLE param_name
    FROM zparam_table
    INTO gv_withcd
    WHERE id_name = lc_withcd.

  SELECT SINGLE param_name
  FROM zparam_table
  INTO gv_payment_term
  WHERE id_name = lc_payment_term.
* End By Shivaji 8000006708 CRDK976286

  "BOC by Gauri Tandel - 8000003666 -11.06.2024
  IF ta_lfbk IS NOT INITIAL.
    SELECT lifnr bukrs loevm                          "check bank key and bank account deletion indicator
      FROM lfb1
      INTO TABLE ta_lfb1
      FOR ALL ENTRIES IN ta_lfbk
      WHERE lifnr = ta_lfbk-lifnr
      AND   bukrs = '1000'.                           "added by Amol Chavan CR 8000003939 - on 25.10.2024 12:06:52"

  ENDIF.

  IF ta_movend IS NOT INITIAL.
    SELECT lifnr loevm  "bukrs                    "check bank key and bank account deletion indicator
*    FROM lfb1                                          "commented by AC:CR-8000003939:15.10.2024
     FROM lfa1                                          "added by AC:CR-8000003939:15.10.2024
    INTO TABLE ta_lfa1
    FOR ALL ENTRIES IN ta_movend
    WHERE lifnr = ta_movend-lifnr.
  ENDIF.
  "EOC by Gauri Tandel - 8000003602 -11.06.2024

*  SOC by AC : CR-8000003939 on 15.10.2024
  DATA(ta_data_vend) = ta_data.
  DELETE ta_data_vend WHERE flag = 'A'.


  IF ta_data_vend IS NOT INITIAL.

    LOOP AT ta_data_vend ASSIGNING FIELD-SYMBOL(<fs_data_vend>).
      <fs_data_vend>-vendor = |{ <fs_data_vend>-vendor ALPHA = IN }|.
    ENDLOOP.

    SELECT lifnr loevm
    FROM lfa1
    INTO TABLE ta_lfa1_n
    FOR ALL ENTRIES IN ta_data_vend
    WHERE lifnr = ta_data_vend-vendor.
  ENDIF.
* EOC by AC CR-8000003939 on 15.10.2024
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_data .
*  perform validate_data_old.   "old code for validation
  CLEAR: ta_data1, ta_honc.
*  SORT ta_data BY pan.              "Commented by Amol Chavan CR 8000003939 on 25.10.2024 12:39:46"
  SORT ta_data BY pan bankn .        "Added by Amol Chavan CR - 8000003939 on 25.10.2024 12:40:14"
  "Soc atc correction abap7-22.09.2026
  SORT: ta_lfb1 BY lifnr bukrs,
        ta_lfa1 BY lifnr.
  "Eoc atc correction abap7-22.09.2026
  " Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 09.09.2026 FOR ATC
  SORT ta_lfbk BY bankn.
  SORT ta_lfb1 BY lifnr.
  " Code Remediation changes S4 2025_A Conversion **END OF CHANGE BY ABAP7 09.09.2026 FOR ATC
  LOOP AT ta_data INTO wa_data.
    IF wa_data-flag = 'A'. "Added by Gajanan - 15.03.2024 - CR#8000003035

      "Bank Key doesnt exists
      READ TABLE ta_bnka INTO wa_bnka WITH KEY bankl = wa_data-bankl BINARY SEARCH.
      IF sy-subrc <> 0.
        IF wa_data-bankl = ' '.
          APPEND wa_data TO ta_nullifsc.
        ELSE.
          APPEND wa_data TO ta_noifsc.
        ENDIF.

        wa_data1-name1 = wa_data-name1.
        wa_data1-pan = wa_data-pan.
        wa_data1-bankn = wa_data-bankn.
        wa_data1-bankl = wa_data-bankl.
*        wa_data1-remarks = 'Bank Key does not Exists in SAP'.               "Commented by AC:CR:8000003939 on 08.10.2024
        wa_data1-remarks = 'IFSC Code does not exist in the system, please resubmit request with correct IFSC code'.               "addeed by AC:CR:8000003939 on 08.10.2024

        APPEND wa_data1 TO ta_data1.
        CLEAR wa_data1."added by Mary Roopini A - 8000004012 on 26.11.2024
      ELSE.

        "check if Vendor already exists
        READ TABLE ta_movend INTO wa_movend WITH KEY j_1ipanno = wa_data-pan BINARY SEARCH.                                        "Parallel cursor Added by Amol Chavan CR - 8000003939  on 25.10.2024 13:51:07"
        IF sy-subrc = 0.
          lv_index = sy-tabix.
        ENDIF.
        LOOP AT ta_movend INTO wa_movend FROM lv_index.
          IF wa_data-pan <> wa_movend-j_1ipanno.
            EXIT.
          ENDIF.                                                        "Added by Amol Chavan CR - on 24.10.2024 15:36:36"
*        IF sy-subrc = 0.                                                                                                       Commented by AC:CR-8000003939:15.10.2024
*          READ TABLE ta_lfb1_pan INTO wa_lfb1_pan WITH KEY lifnr = wa_movend-lifnr loevm = ' ' BINARY SEARCH.                  Commented by AC:CR-8000003939:15.10.2024
          READ TABLE ta_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_movend-lifnr loevm = ' ' BINARY SEARCH.                           "added by AC:CR-8000003939:15.10.2024
          IF sy-subrc = 0.
            DATA(lv_vendor1) = wa_lfa1-lifnr.
            wa_data-delete = abap_true.
            APPEND wa_data TO ta_movend1.
          ENDIF.
*        ENDIF.                                                                                                                 Commented by AC:CR-8000003939:15.10.2024
        ENDLOOP.


        IF ta_movend1 IS NOT INITIAL.
          READ TABLE ta_movend1 INTO wa_movend1 WITH KEY pan = wa_data-pan.
          IF sy-subrc = 0.
            wa_data1-name1 = wa_movend1-name1.
            wa_data1-pan = wa_movend1-pan.
            wa_data1-bankn = wa_movend1-bankn.
            wa_data1-bankl = wa_movend1-bankl.
*          wa_data1-remarks = 'Vendor already exists' .
*            READ TABLE ta_movend INTO wa_movend WITH KEY j_1ipanno = wa_data-pan BINARY SEARCH.                                 "Commented by Amol Chavan CR - on 25.10.2024 11:26:11"
*            IF sy-subrc = 0.                                                                                                    "Commented by Amol Chavan CR - on 25.10.2024 11:26:11"
            CONCATENATE 'Vendor_' lv_vendor1 ' already exists with the same PAN Number.' INTO lv_string.                         "wa_movend-vendor
            wa_data1-remarks = lv_string.
*            ENDIF.                                                                                                              "Commented by Amol Chavan CR - on 25.10.2024 11:26:11"
            APPEND wa_data1 TO ta_data1.
            CLEAR lv_vendor1.
            CLEAR wa_data1."added by Mary Roopini A - 8000004012 on 26.11.2024
          ENDIF.
        ENDIF.

        "Vendor has same Bank Account
        CLEAR: wa_lfbk, wa_lfb1,lv_string, wa_lfa1.
        READ TABLE ta_lfbk INTO wa_lfbk WITH KEY  bankn = wa_data-bankn BINARY SEARCH.                               "Parallel cursor Added by Amol Chavan CR - 8000003939  on 25.10.2024 13:51:07"
        IF sy-subrc = 0.
          lv_index1 = sy-tabix.
        ENDIF.
        LOOP AT ta_lfbk INTO wa_lfbk FROM lv_index1.
          IF wa_data-bankn <> wa_lfbk-bankn.
            EXIT.
          ENDIF.                                                                                                     " Added by Amol Chavan  on 24.10.2024 15:46:18"
*        IF sy-subrc = 0.                                                                                            Commented by AC:CR-8000003939:15.10.2024
          READ TABLE ta_lfb1 INTO wa_lfb1 WITH KEY lifnr = wa_lfbk-lifnr loevm = ' ' BINARY SEARCH.                  "commented by Amol Chavan CR - 8000003939 on 25.10.2024 11:37:42"
          IF sy-subrc = 0.
            DATA(lv_vendor)  = wa_lfb1-lifnr.
            wa_data-delete = abap_true.
            APPEND wa_data TO ta_lfbk1 .
          ENDIF.
*        ENDIF.                                                                                                     Commented by AC:CR-8000003939:15.10.2024
        ENDLOOP.

        IF ta_lfbk1 IS NOT INITIAL.
          READ TABLE ta_lfbk1 INTO wa_lfbk1 WITH KEY bankl = wa_data-bankl bankn = wa_data-bankn BINARY SEARCH..
          IF sy-subrc = 0.
            wa_data1-name1 = wa_lfbk1-name1.
            wa_data1-pan = wa_lfbk1-pan.
            wa_data1-bankn = wa_lfbk1-bankn.
            wa_data1-bankl = wa_lfbk1-bankl.
*          wa_data1-remarks = 'Vendor has the same bank account maintained' .
*            READ TABLE ta_lfbk INTO wa_lfbk WITH KEY bankl = wa_data-bankl bankn = wa_data-bankn BINARY SEARCH.  "Commented by Amol Chavan CR - 8000003939 on 25.10.2024 11:20:28"
*            IF sy-subrc = 0.                                                                                     "Commented by Amol Chavan CR - 8000003939 on 25.10.2024 11:20:28"
            CONCATENATE 'Vendor_' lv_vendor ' has the same bank account maintained' INTO lv_string .                      "wa_lfbk-vendor
            wa_data1-remarks = lv_string.
*            ENDIF.                                                                                               "Commented by Amol Chavan CR - 8000003939 on 25.10.2024 11:20:28"
            APPEND wa_data1 TO ta_data1.
            CLEAR lv_vendor.
            CLEAR wa_data1."added by Mary Roopini A - 8000004012 on 26.11.2024
          ENDIF.
        ENDIF.

        "PAN Details are Incorrect
        IF strlen( wa_data-pan ) NE 10.
          APPEND wa_data TO ta_pan.
        ENDIF.
        IF ta_pan IS NOT INITIAL.
          READ TABLE ta_pan INTO wa_pan WITH KEY pan = wa_data-pan.
          IF sy-subrc = 0.
            wa_data1-name1 = wa_pan-name1.
            wa_data1-pan = wa_pan-pan.
            wa_data1-bankn = wa_pan-bankn.
            wa_data1-bankl = wa_pan-bankl.
            wa_data1-remarks = 'Incorrect PAN details' .
            APPEND wa_data1 TO ta_data1.
            CLEAR wa_data1."added by Mary Roopini A - 8000004012 on 26.11.2024
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR : wa_lfb1, lv_index, lv_index1, lv_vendor, lv_vendor1.

    IF wa_data-flag = 'U'.

      "Bank Key doesnt exists
      READ TABLE ta_bnka INTO wa_bnka WITH KEY bankl = wa_data-bankl BINARY SEARCH.
      IF sy-subrc <> 0.
        IF wa_data-bankl = ' '.
          APPEND wa_data TO ta_nullifsc.
        ELSE.
          APPEND wa_data TO ta_noifsc.
        ENDIF.

        wa_data1-name1 = wa_data-name1.
        wa_data1-pan = wa_data-pan.
        wa_data1-bankn = wa_data-bankn.
        wa_data1-bankl = wa_data-bankl.
        wa_data1-vendor = wa_data-vendor. "added by Mary Roopini A - 8000004012 on 26.11.2024
*        wa_data1-remarks = 'Bank Key does not Exists in SAP'.                 "Commented by AC:CR:8000003939 on 08.10.2024
        wa_data1-remarks = 'IFSC code does not exist in the system, please resubmit request with correct IFSC code'.                  "Added by AC:CR: 8000003939 on 08.10.2024

        APPEND wa_data1 TO ta_data1.
        CLEAR wa_data1."added by Mary Roopini A - 8000004012 on 26.11.2024
      ELSE.

        "Vendor has same bank Account or Not
        SORT ta_lfbk1 BY bankl bankn.
        IF ta_lfbk1 IS NOT INITIAL.
          READ TABLE ta_lfbk1 INTO wa_lfbk1 WITH KEY bankl = wa_data-bankl bankn = wa_data-bankn BINARY SEARCH..
          IF sy-subrc = 0.
            wa_data1-name1 = wa_lfbk1-name1.
            wa_data1-pan = wa_lfbk1-pan.
            wa_data1-vendor = wa_data-vendor. "added by Mary Roopini A - 8000004012 on 26.11.2024
            wa_data1-bankn = wa_lfbk1-bankn.
            wa_data1-bankl = wa_lfbk1-bankl.
*          wa_data1-remarks = 'Vendor has the same bank account maintained' .
            READ TABLE ta_lfbk INTO wa_lfbk WITH KEY bankl = wa_data-bankl bankn = wa_data-bankn BINARY SEARCH.
            IF sy-subrc = 0.
              CONCATENATE 'Vendor   ' wa_lfbk-lifnr ' has the same bank account maintained' INTO lv_string .
              wa_data1-remarks = lv_string.
            ENDIF.
            APPEND wa_data1 TO ta_data1.
            CLEAR wa_data1."added by Mary Roopini A - 8000004012 on 26.11.2024
          ENDIF.
        ENDIF.

        "check if Vendor already exists
*        READ TABLE ta_movend INTO wa_movend WITH KEY j_1ipanno = wa_data-pan BINARY SEARCH.                           "Commented by AC:CR - 8000003939 on 15.10.2024
*        IF sy-subrc = 0.                                                                                              "Commented by AC:CR - 8000003939 on 15.10.2024
*          READ TABLE ta_lfb1_pan INTO wa_lfb1_pan WITH KEY lifnr = wa_movend-lifnr loevm = ' ' BINARY SEARCH.         "commented by AC:CR-8000003939 on 15.10.2024

        wa_data-vendor = |{ wa_data-vendor ALPHA = IN }|.                                                             "added by AC:CR-8000003939: on 15.10.2024
        READ TABLE ta_lfa1_n INTO wa_lfa1_n WITH KEY lifnr = wa_data-vendor."  loevm = ' '."commented by Mary Roopini A - 8000004012 on 22.11.2024
        IF sy-subrc = 0.
          IF wa_lfa1_n-loevm = ' '."added by Mary Roopini A - 8000004012 on 22.11.2024
            SORT ta_data1 BY bankl bankn pan.
            CLEAR wa_data1.
            READ TABLE ta_data1 INTO wa_data1 WITH KEY bankl = wa_data-bankl bankn = wa_data-bankn pan =  wa_data-pan BINARY SEARCH.
            IF sy-subrc <> 0.
              APPEND wa_data TO ta_chvend.
            ENDIF.
          ELSE."added by Mary Roopini A - 8000004012 on 22.11.2024
            wa_data1-name1 = wa_data-name1."added by Mary Roopini A - 8000004012 on 25.11.2024
            wa_data1-pan = wa_data-pan."added by Mary Roopini A - 8000004012 on 25.11.2024
            wa_data1-bankn = wa_data-bankn."added by Mary Roopini A - 8000004012 on 25.11.2024
            wa_data1-bankl = wa_data-bankl."added by Mary Roopini A - 8000004012 on 25.11.2024
            wa_data1-vendor = wa_data-vendor. "added by Mary Roopini A - 8000004012 on 26.11.2024
            wa_data1-remarks = 'Vendor is marked for deletion'."added by Mary Roopini A - 8000004012 on 22.11.2024
            APPEND wa_data1 TO ta_data1."added by Mary Roopini A - 8000004012 on 25.11.2024
            CLEAR wa_data1."added by Mary Roopini A - 8000004012 on 26.11.2024
          ENDIF."added by Mary Roopini A - 8000004012 on 22.11.2024


        ELSE.
          wa_data1-name1 = wa_data-name1.
          wa_data1-pan = wa_data-pan.
          wa_data1-bankn = wa_data-bankn.
          wa_data1-bankl = wa_data-bankl.
          wa_data1-vendor = wa_data-vendor. "added by Mary Roopini A - 8000004012 on 22.11.2024
          wa_data1-remarks = 'Vendor does not Exists in SAP'.
          APPEND wa_data1 TO ta_data1.
          CLEAR wa_data1."added by Mary Roopini A - 8000004012 on 26.11.2024
        ENDIF.
*        ELSE.
*          wa_data1-name1 = wa_data-name1.
*          wa_data1-pan = wa_data-pan.
*          wa_data1-bankn = wa_data-bankn.
*          wa_data1-bankl = wa_data-bankl.
*          wa_data1-remarks = 'Vendor does not Exists in SAP'.
*          APPEND wa_data1 TO ta_data1.
*        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  "delete error entries from TA_DATA to proceed for vendor creation/change

  ta_data_tmp = ta_data.
  CLEAR: wa_data1.
  LOOP AT ta_data INTO wa_data.
    IF wa_data-flag = 'A'.
      " S/4HANA conversion: no BINARY SEARCH - TA_DATA1 is sorted by bank key
      " once a change record is checked, so a binary search by PAN missed
      " rejected records, and a rejected new vendor (one whose PAN already has
      " a vendor) went on to be created.
      READ TABLE ta_data1 INTO wa_data1 WITH KEY pan = wa_data-pan.
      IF sy-subrc = 0.
        DELETE ta_data WHERE pan = wa_data1-pan.
      ENDIF.
    ENDIF.
  ENDLOOP.

  "vendor creation/change
  IF ta_data IS NOT INITIAL.
    LOOP AT ta_data INTO wa_data.
      IF wa_data-flag = 'A'.
        IF line_exists( ta_bnka[ bankl = wa_data-bankl ] ).             "--------- CREATION/CHANGE IN VENDOR
          IF line_exists( ta_movend[ j_1ipanno = wa_data-pan ] ).
            IF line_exists( ta_lfb1[ lifnr = wa_movend-lifnr bukrs = '1000' loevm = ' ' ] ).
              APPEND wa_data TO ta_chvend.
              IF ta_chvend IS NOT INITIAL.
                DELETE ta_chvend WHERE name1 = wa_data1-name1 AND pan = wa_data1-pan.                   "PAN Added by Amol Chavan CR - 8000003939 on 25.10.2024 15:34:19"
              ENDIF.
            ELSE.
              APPEND wa_data TO ta_crvend.
              IF ta_crvend IS NOT INITIAL.
                DELETE ta_crvend WHERE name1 = wa_data1-name1.
              ENDIF.
            ENDIF.
          ELSE.
            APPEND wa_data TO ta_crvend.
            IF ta_crvend IS NOT INITIAL.
              DELETE ta_crvend WHERE name1 = wa_data1-name1  AND pan = wa_data1-pan.                    "Pan Added by Amol Chavan CR - 8000003939 on 25.10.2024 15:34:57"
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "BOC by Gauri Tandel - 8000003666 -11.06.2024
  IF rb_vend IS NOT INITIAL.
    IF ta_data1 IS NOT INITIAL.
      CLEAR: wa_data1.

      LOOP AT ta_data1 INTO wa_data1.
        CLEAR: wa_data.
        READ TABLE ta_data_tmp INTO wa_data WITH KEY pan = wa_data1-pan BINARY SEARCH.
        IF sy-subrc = 0.
          wa_honc-unique_id = wa_data-unique_id.
        ENDIF.
*        READ TABLE ta_movend INTO wa_movend WITH KEY j_1ipanno = wa_data-pan BINARY SEARCH.
*        IF sy-subrc = 0.
*          wa_honc-vendor_code = wa_movend-lifnr.
*        ELSE.
*          READ TABLE ta_lfbk INTO wa_lfbk WITH KEY bankl = wa_data-bankl bankn = wa_data-bankn BINARY SEARCH.
*          IF sy-subrc = 0.
*            wa_honc-vendor_code = wa_lfbk-lifnr.
*          ENDIF.
*        ENDIF.                               "commented by Gauri Tandel - 8000003829 -13.08.2024

        wa_honc-pan_no = wa_data1-pan.
        wa_honc-status = 'FAILURE'.
        wa_honc-message = wa_data1-remarks.
        wa_honc-update_date = sy-datlo.
        wa_honc-update_time = sy-timlo.
        wa_honc-vendor_code = wa_data1-vendor."added by Mary Roopini A - 8000004012 on 22.11.2024
        wa_honc-update_id = sy-uname.
        APPEND wa_honc TO ta_honc.
        CLEAR wa_data1.

*        APPEND wa_honc TO ta_honc_tmp.          "added by Gauri Tandel - 8000003666 -17.06.2024

      ENDLOOP.
*      WAIT UP TO 2 SECONDS .
      MODIFY zfi_honc_vend FROM TABLE ta_honc.

      CLEAR: wa_honc.
    ENDIF.
  ENDIF.
  "EOC by Gauri Tandel - 8000003666 -11.06.2024

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MISSING_IFSC_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM missing_ifsc_mail .
  DATA : lv_send_address TYPE so_rec_ext,
         lwa_doc_data    TYPE sodocchgi1,
         lv_lines        TYPE i,
         lv_subject      TYPE so_obj_des,
         lta_receiver    TYPE TABLE OF somlreci1,
         lwa_receiver    TYPE somlreci1,
         lta_message     TYPE TABLE OF solisti1,
         lwa_packing     TYPE sopcklsti1,
         lta_packing     TYPE TABLE OF sopcklsti1,
         lwa_message     TYPE solisti1.
  DATA : lv_date(10) TYPE c.
  CONSTANTS : lc_b TYPE c VALUE 'B'.

  CONCATENATE sy-datum+6(2) '/' sy-datum+4(2) '/' sy-datum+0(4) INTO lv_date.
***********Build Message************
  CLEAR lwa_message.
  lwa_message-line = 'Dear Rashmi,<br/>'(006).
  APPEND lwa_message TO lta_message.

*SOC by fatima for 8000007904 Vendor code generation.
  "email to be send for null ifsc code in file
  IF ta_nullifsc IS NOT INITIAL.
    CLEAR lwa_message.
    lwa_message-line = '<br/>Below Bank Key Are Missing : <br/>'(007).
    APPEND  lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<html> <body>'(036).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<table style="MARGIN: 10px" '(037).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = ' cellspacing="0" cellpadding="3" width="400"'(010).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = ' border="1"><tbody><tr>'(011).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Bank Country</th>'(040).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>IFSC No</th>'(024).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>State</th>'(041).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Name Of HCP</th>'(042).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>PAN No Of HCP</th></tr>'(043).
    APPEND lwa_message TO lta_message.

    LOOP AT ta_nullifsc INTO wa_data.
*   table Contents
      TRY .
          DATA(lwa_t005u) = ta_t005u[ bezei = wa_data-region ].
          ##NO_HANDLER
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
      CLEAR lwa_message.
      lwa_message-line = '<tr>'.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-banks '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-bankl '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' lwa_t005u-bland '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-name1 '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-pan '</td></tr>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR : wa_data,lwa_t005u.
    ENDLOOP.
*   table close
    lwa_message-line = '</tbody> </table>'(014).
    APPEND lwa_message TO lta_message.

*   HTML close
    lwa_message-line = '</body> </html> '(018).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message = '<br/>Thanks & Regards,<br/>'(029).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message = 'Cipla SAP.<br/>'(015).
    APPEND lwa_message TO lta_message.
***********Build Message************

    CLEAR lwa_packing.
    REFRESH lta_packing.
    lwa_packing-transf_bin = space.
    lwa_packing-head_start = 1.
    lwa_packing-head_num = 0.
    lwa_packing-body_start = 1.
    DESCRIBE TABLE lta_message LINES lwa_packing-body_num.
    lwa_packing-doc_type = 'HTM'.
    APPEND lwa_packing TO lta_packing.

    REFRESH lta_receiver.
    LOOP AT ta_email INTO wa_email WHERE mail = lc_b.
      lwa_receiver-receiver = wa_email-email.
      lwa_receiver-rec_type = 'U'.
      lwa_receiver-com_type = 'INT'.
      lwa_receiver-notif_del = ''.
      lwa_receiver-notif_ndel = ''.
      IF wa_email-cc = abap_true.
        lwa_receiver-copy = abap_true.
      ENDIF.
      APPEND lwa_receiver TO lta_receiver.
      CLEAR : wa_email, lwa_receiver.
    ENDLOOP.

    lv_subject = 'Blank IFSC code found'.   "added by fatima for 8000007904

    CLEAR lwa_doc_data.
    lwa_doc_data-obj_langu  = sy-langu.
    lwa_doc_data-obj_name   = 'SAPRPT'.
    lwa_doc_data-obj_descr  = lv_subject .
    lwa_doc_data-sensitivty = 'F'.

    CLEAR : lv_lines, lwa_message.
    DESCRIBE TABLE lta_message LINES lv_lines.
    READ TABLE lta_message INTO lwa_message INDEX lv_lines.
    lwa_doc_data-doc_size = ( lv_lines - 1 ) * 255 + strlen( lwa_message ).

    lv_send_address = 'Medline@cipla.com'(038).
    CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
      EXPORTING
        document_data              = lwa_doc_data
        sender_address             = lv_send_address
        sender_address_type        = 'INT'
        commit_work                = 'X'
      TABLES
        packing_list               = lta_packing
        contents_txt               = lta_message
        receivers                  = lta_receiver
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
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      MESSAGE 'Mail Sent For Misssing IFSC Details.'(017) TYPE co_s.
    ENDIF.
  ENDIF.

  IF ta_noifsc IS NOT INITIAL.
    REFRESH: lta_message.
    CLEAR lwa_message.
    lwa_message-line = '<br/>Please verify and share details in attached format for creation of Bank Key<br/>'(039).
    APPEND  lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<html> <body>'(036).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<table style="MARGIN: 10px" '(037).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = ' cellspacing="0" cellpadding="3" width="400"'(010).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = ' border="1"><tbody><tr>'(011).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Bank Country</th>'(040).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>IFSC No</th>'(024).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>State</th>'(041).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Name Of HCP</th>'(042).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>PAN No Of HCP</th></tr>'(043).
    APPEND lwa_message TO lta_message.

    LOOP AT ta_noifsc INTO wa_data.
*   table Contents
      TRY .
          CLEAR lwa_t005u.
          lwa_t005u = ta_t005u[ bezei = wa_data-region ].
          ##NO_HANDLER
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
      CLEAR lwa_message.
      lwa_message-line = '<tr>'.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-banks '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-bankl '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' lwa_t005u-bland '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-name1 '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-pan '</td></tr>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.
      CLEAR : wa_data,lwa_t005u.
    ENDLOOP.
*     table close
    lwa_message-line = '</tbody> </table>'(014).
    APPEND lwa_message TO lta_message.

*   HTML close
    lwa_message-line = '</body> </html> '(018).
    APPEND lwa_message TO lta_message.
  ENDIF.

  IF ta_data1 IS NOT INITIAL.
*Start of changes by Anuj Nandapurkar as part of 8000009272 dated 16-10-2023
    CLEAR lwa_message.
    lwa_message-line = '<br/>Please find below table for errors<br/>'.
    APPEND  lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<table style="MARGIN: 10px" '(037).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = ' cellspacing="0" cellpadding="3" width="400"'(010).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = ' border="1"><tbody><tr>'(011).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Doctor Name</th>'(023).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>PAN Card No</th>'(034).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Bank A/C No</th>'(032).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>IFSC No</th>'(024).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Remarks</th></tr>'(044).
    APPEND lwa_message TO lta_message.

    LOOP AT ta_data1 INTO wa_data1.

      CLEAR lwa_message.
      lwa_message-line = '<tr>'.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data1-name1 '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data1-pan '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data1-bankn '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data1-bankl '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.
      CLEAR lwa_message.

      CONCATENATE '<td>' wa_data1-remarks '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.
      CLEAR : wa_data1, wa_lfbk1, wa_pan, wa_movend1, wa_data.
      DELETE ta_data1 WHERE name1 IS INITIAL.
    ENDLOOP.

    lwa_message-line = '</tbody> </table>'(014).
    APPEND lwa_message TO lta_message.

    lwa_message-line = '</body> </html> '(018).
    APPEND lwa_message TO lta_message.
  ENDIF.
*End of changes by Anuj Nandapurkar as part of CR8000009272 dated 16-10-2023

  CLEAR lwa_message.
  lwa_message = '<br/>Thanks & Regards,<br/>'(029).
  APPEND lwa_message TO lta_message.

  CLEAR lwa_message.
  lwa_message = 'Cipla SAP.<br/>'(015).
  APPEND lwa_message TO lta_message.
***********Build Message************

  CLEAR lwa_packing.
  REFRESH lta_packing.
  lwa_packing-transf_bin = space.
  lwa_packing-head_start = 1.
  lwa_packing-head_num = 0.
  lwa_packing-body_start = 1.
  DESCRIBE TABLE lta_message LINES lwa_packing-body_num.
  lwa_packing-doc_type = 'HTM'.
  APPEND lwa_packing TO lta_packing.

  REFRESH lta_receiver.
  LOOP AT ta_email INTO wa_email WHERE mail = lc_b.
    lwa_receiver-receiver = wa_email-email.
    lwa_receiver-rec_type = 'U'.
    lwa_receiver-com_type = 'INT'.
    lwa_receiver-notif_del = ''.
    lwa_receiver-notif_ndel = ''.
    IF wa_email-cc = abap_true.
      lwa_receiver-copy = abap_true.
    ENDIF.
    APPEND lwa_receiver TO lta_receiver.
    CLEAR : wa_email, lwa_receiver.
  ENDLOOP.

  CONCATENATE 'Bank Key Creation Date ('(048) lv_date ')' INTO lv_subject.

  CLEAR lwa_doc_data.
  lwa_doc_data-obj_langu  = sy-langu.
  lwa_doc_data-obj_name   = 'SAPRPT'.
  lwa_doc_data-obj_descr  = lv_subject .
  lwa_doc_data-sensitivty = 'F'.

  CLEAR : lv_lines, lwa_message.
  DESCRIBE TABLE lta_message LINES lv_lines.
  READ TABLE lta_message INTO lwa_message INDEX lv_lines.
  lwa_doc_data-doc_size = ( lv_lines - 1 ) * 255 + strlen( lwa_message ).

  lv_send_address = 'Medline@cipla.com'(038).
  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = lwa_doc_data
      sender_address             = lv_send_address
      sender_address_type        = 'INT'
      commit_work                = 'X'
    TABLES
      packing_list               = lta_packing
      contents_txt               = lta_message
      receivers                  = lta_receiver
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
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    MESSAGE 'Mail Sent For Misssing IFSC Details.'(017) TYPE co_s.
  ENDIF.
*    ENDIF.
*EOC by fatima for 8000007904 Vendor code generation.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHANGE_VENDOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM change_vendor .
* S/4HANA: FK02 is gone - the vendor is changed through its business
* partner (LCL_HONC_BP=>CHANGE, program ZFI_VEND_HONC). It changes what
* the FK02 batch input did: name, search term, address, telephone,
* mobile, fax, e-mail (a new one overwrites the standard one, as FK02
* did), GSTIN, corporate group NONFIELD, industry key, the bank account
* (the file's replaces the first one, as typing over line 1 in FK02 did)
* and the GST vendor classification.
  DATA : lv_stat TYPE char255,
         lv_vend TYPE lfa1-lifnr.
  DATA : ls_central TYPE vmds_ei_vmd_central,
         ls_postal  TYPE cvis_ei_1vl,
         ls_cin     TYPE ty_bp_cin,
         ls_comp    TYPE ty_bp_comp,
         ls_none    TYPE ty_bp_none,
         lt_tel     TYPE ty_bp_tel_t,
         lt_fax     TYPE ty_bp_fax_t,
         lt_mail    TYPE ty_bp_mail_t,
         lt_bank    TYPE ty_bp_bank_t,
         lt_wtax    TYPE ty_bp_wtax_t,
         lt_error   TYPE bapiret2_t,
         lt_success TYPE bapiret2_t.

  LOOP AT ta_chvend INTO wa_data.
    IF wa_data-flag = 'U'.              "added by Gauri Tandel - 8000003666 -17.06.2024
      PERFORM bp_data USING    wa_data abap_true
                      CHANGING ls_central ls_postal ls_cin ls_comp
                               lt_tel lt_fax lt_mail lt_bank lt_wtax.

      lcl_honc_bp=>change(
        EXPORTING iv_lifnr        = wa_data-vendor
                  is_central      = ls_central
                  is_postal       = ls_postal
                  is_company      = ls_none          " FK02 changed general data only
                  is_cin          = ls_cin
                  is_purch        = ls_none
                  it_mobile       = lt_tel
                  it_fax          = lt_fax
                  it_smtp         = lt_mail
                  it_bank         = lt_bank
                  iv_industry     = wa_data-ind_key
                  iv_keep_blank   = `VEN_CLASS`      " blank = registered for GST
                  iv_bank_replace = abap_true
                  iv_comm_replace = abap_true        " FK02 typed over the numbers
        IMPORTING et_error        = lt_error
                  et_success      = lt_success ).

      PERFORM bp_result USING    wa_data lt_error lt_success
                        CHANGING lv_stat lv_vend.
      IF lv_vend IS NOT INITIAL.
        lv_vend = wa_data-vendor.       " Vendor Number - as for F2 056 in ECC
      ENDIF.

      "Update table ZFI_HONC_VEND
      CLEAR: wa_honc.
      wa_honc-mandt = sy-mandt.
      wa_honc-unique_id = wa_data-unique_id.
      wa_honc-pan_no = wa_data-pan.
      IF lv_vend IS NOT INITIAL .
        wa_honc-vendor_code = lv_vend.
        wa_honc-status = 'SUCCESS'.
      ELSE.
        wa_honc-vendor_code = wa_data-vendor."added by Mary Roopini - 8000004012 - 22.11.2024
        wa_honc-status = 'FAILURE'.
      ENDIF.
      wa_honc-message = lv_stat.
      wa_honc-update_date = sy-datum.
      wa_honc-update_time = sy-uzeit.
      wa_honc-update_id = sy-uname.
      APPEND wa_honc TO ta_honc.
      APPEND wa_honc TO ta_honc_tmp.          "added by Gauri Tandel - 8000003666 -17.06.2024
      MODIFY zfi_honc_vend FROM TABLE ta_honc.
      CLEAR: wa_honc.
      REFRESH: ta_honc.
      CLEAR : wa_data, lv_stat, lv_vend, lt_error, lt_success.
    ENDIF.
  ENDLOOP.
  WAIT UP TO 2 SECONDS.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_VENDOR
*&---------------------------------------------------------------------*
* S/4HANA: FK01 is gone - the vendor is created as a business partner
* (LCL_HONC_BP=>CREATE, program ZFI_VEND_HONC) with what the FK01 batch
* input entered: account group Z001 (BP grouping and supplier roles from
* CVIC_VEND_TO_BP1 / 2), title, name, search term, address, language EN,
* mobile, e-mail, GSTIN, industry key, bank account, company code 1000
* (reconciliation account ZFI_VEND_G, payment terms EAS_PT_HN, payment
* method CNT, double-invoice check, WHT country IN), withholding tax type
* EAS_TDS1_H with code EAS_TDS_HN, PAN and GST vendor classification.
*----------------------------------------------------------------------*
FORM create_vendor .
  DATA : lv_stat TYPE char255,
         lv_vend TYPE lfa1-lifnr.
  DATA : ls_central TYPE vmds_ei_vmd_central,
         ls_postal  TYPE cvis_ei_1vl,
         ls_cin     TYPE ty_bp_cin,
         ls_comp    TYPE ty_bp_comp,
         ls_none    TYPE ty_bp_none,
         lt_tel     TYPE ty_bp_tel_t,
         lt_fax     TYPE ty_bp_fax_t,
         lt_mail    TYPE ty_bp_mail_t,
         lt_bank    TYPE ty_bp_bank_t,
         lt_wtax    TYPE ty_bp_wtax_t,
         lt_error   TYPE bapiret2_t,
         lt_success TYPE bapiret2_t.
  "BOC by Gauri Tandel - 8000003666 -11.06.2024
  DELETE ta_crvend WHERE delete = abap_true.
  "EOC by Gauri Tandel - 8000003666 -11.06.2024
  LOOP AT ta_crvend INTO wa_data.
    PERFORM bp_data USING    wa_data abap_false
                    CHANGING ls_central ls_postal ls_cin ls_comp
                             lt_tel lt_fax lt_mail lt_bank lt_wtax.

    lcl_honc_bp=>create(
      EXPORTING is_central  = ls_central
                is_postal   = ls_postal
                is_company  = ls_comp
                is_cin      = ls_cin
                is_purch    = ls_none            " FK01 had no purchasing org
                it_mobile   = lt_tel
                it_fax      = lt_fax
                it_smtp     = lt_mail
                it_bank     = lt_bank
                it_wtax     = lt_wtax
                iv_industry = wa_data-ind_key
      IMPORTING et_error    = lt_error
                et_success  = lt_success ).

    " The new vendor number - F2 271 in ECC.
    PERFORM bp_result USING    wa_data lt_error lt_success
                      CHANGING lv_stat lv_vend.

    "Update table ZFI_HONC_VEND
    CLEAR: wa_honc.
    wa_honc-mandt = sy-mandt.
    wa_honc-unique_id = wa_data-unique_id.
    wa_honc-pan_no = wa_data-pan.
    IF lv_vend IS NOT INITIAL .
      wa_honc-vendor_code = lv_vend.
      wa_honc-status = 'SUCCESS'.
    ELSE.
      wa_honc-vendor_code = wa_data-vendor."added by Mary Roopini - 8000004012 - 22.11.2024
      wa_honc-status = 'FAILURE'.
    ENDIF.
    wa_honc-message = lv_stat.
    wa_honc-create_date = sy-datum.
    wa_honc-create_time = sy-uzeit.
    wa_honc-create_id = sy-uname.
    APPEND wa_honc TO ta_honc.
    MODIFY zfi_honc_vend FROM TABLE ta_honc.
    CLEAR: wa_honc.
    REFRESH: ta_honc.
    CLEAR : wa_data, lv_stat, lv_vend, lt_error, lt_success.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BP_DATA
*&---------------------------------------------------------------------*
* S/4HANA: one record of the file as LCL_HONC_BP takes it - what the
* FK01 (IV_CHANGE = false) and FK02 (true) batch input entered.
*----------------------------------------------------------------------*
FORM bp_data USING    is_data    TYPE ty_vendor
                      iv_change  TYPE abap_bool
             CHANGING cs_central TYPE vmds_ei_vmd_central
                      cs_postal  TYPE cvis_ei_1vl
                      cs_cin     TYPE ty_bp_cin
                      cs_comp    TYPE ty_bp_comp
                      ct_tel     TYPE ty_bp_tel_t
                      ct_fax     TYPE ty_bp_fax_t
                      ct_mail    TYPE ty_bp_mail_t
                      ct_bank    TYPE ty_bp_bank_t
                      ct_wtax    TYPE ty_bp_wtax_t.
  CLEAR: cs_central, cs_postal, cs_cin, cs_comp, ct_tel, ct_fax, ct_mail,
         ct_bank, ct_wtax.

  " ---- general data: name and address (FK01 / FK02 screen 0111) --------
  cs_postal-data-title      = is_data-title.
  IF iv_change = abap_true AND cs_postal-data-title IS INITIAL.
    cs_postal-data-title    = 'Dr.'.            " FK02 always entered Dr.
  ENDIF.
  cs_postal-data-name       = is_data-name1.
  cs_postal-data-sort1      = is_data-sort1.
  cs_postal-data-street     = is_data-street+0(23). "added by Gauri Tandel - 8000003829 - 13.08.2024
  cs_postal-data-str_suppl1 = is_data-str_suppl1.
  cs_postal-data-str_suppl2 = is_data-str_suppl2.
  cs_postal-data-postl_cod1 = is_data-post_code1.
  cs_postal-data-city       = is_data-city1.
  cs_postal-data-country    = is_data-country.
  IF iv_change = abap_true.
    cs_postal-data-region   = is_data-region.
  ELSE.
    " FK01 took the region from T005U - blank when India has no such region.
    TRY.
        cs_postal-data-region = ta_t005u[ bland = is_data-region ]-bland. "Added by Gajanan - 21.03.2024 - CR#8000003530
        ##NO_HANDLER
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
    cs_postal-data-langu    = 'E'.              " FK01: ADDR1_DATA-LANGU EN
  ENDIF.

  IF is_data-tel_number1 IS NOT INITIAL.
    APPEND VALUE #( telephone = is_data-tel_number1
                    extension = is_data-tel_extens1 ) TO ct_tel.
  ENDIF.
  IF is_data-mob_number1 IS NOT INITIAL.
    " R_3_USER 3: the mobile number, as SZA1_D0100-MOB_NUMBER stored it.
    APPEND VALUE #( telephone = is_data-mob_number1 r_3_user = '3' ) TO ct_tel.
  ENDIF.
  IF is_data-fax_number IS NOT INITIAL.
    APPEND VALUE #( fax = is_data-fax_number ) TO ct_fax.
  ENDIF.
  IF is_data-smtp_addr IS NOT INITIAL.
    APPEND VALUE #( e_mail = is_data-smtp_addr ) TO ct_mail.
  ENDIF.

  " ---- control data (screen 0120) ---------------------------------------
  " GSTIN: LCL_HONC_BP files it as the partner's tax number IN3.
  cs_central-data-stcd3 = is_data-gst.
  IF iv_change = abap_true.
    cs_central-data-konzs = 'NONFIELD'.
  ELSE.
    cs_central-data-ktokk = 'Z001'.
  ENDIF.

  " ---- bank (screen 0130) -----------------------------------------------
  IF is_data-bankl IS NOT INITIAL AND is_data-bankn IS NOT INITIAL.
    APPEND VALUE #( banks = is_data-banks
                    bankl = is_data-bankl
                    bankn = is_data-bankn
                    koinh = is_data-koinh ) TO ct_bank.
  ENDIF.

  " ---- CIN details: PAN on create, GST vendor classification ----------
  IF iv_change = abap_false.
    cs_cin-j_1ipanno = is_data-pan.
  ENDIF.
  cs_cin-ven_class = is_data-gstvenclass.

  " ---- company code 1000 and withholding tax (FK01 only) ---------------
  IF iv_change = abap_false.
    cs_comp-data_key-bukrs = '1000'.
    cs_comp-data-akont     = gv_akont.
    cs_comp-data-zterm     = gv_payment_term.
    cs_comp-data-reprf     = abap_true.
    cs_comp-data-zwels     = 'CNT'.
    cs_comp-data-qland     = 'IN'.
    IF gv_witht IS NOT INITIAL.
      APPEND VALUE #( witht     = gv_witht
                      wt_withcd = gv_withcd
                      wt_subjct = abap_true ) TO ct_wtax.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BP_RESULT
*&---------------------------------------------------------------------*
* S/4HANA: the messages of LCL_HONC_BP into the log and into the text
* for ZFI_HONC_VEND, as the batch input's messages were. CV_VEND is the
* vendor saved - LCL_HONC_BP's success line is
* "<vendor>;<address number> <text>".
*----------------------------------------------------------------------*
FORM bp_result USING    is_data    TYPE ty_vendor
                        it_error   TYPE bapiret2_t
                        it_success TYPE bapiret2_t
               CHANGING cv_stat    TYPE char255
                        cv_vend    TYPE lifnr.
  DATA : lt_all TYPE bapiret2_t,
         lv_msg TYPE char255.
  CLEAR : cv_stat, cv_vend.
  APPEND LINES OF it_error TO lt_all.
  APPEND LINES OF it_success TO lt_all.
  LOOP AT lt_all INTO DATA(ls_ret).
    CLEAR lv_msg.
    IF ls_ret-type = 'S' AND ls_ret-message CA ';'.
      SPLIT ls_ret-message AT ';' INTO DATA(lv_lifnr) DATA(lv_rest).
      cv_vend = lv_lifnr.
      SPLIT lv_rest AT ' ' INTO DATA(lv_adrnr) DATA(lv_text).
      lv_msg = lv_text.
    ELSE.
      lv_msg = ls_ret-message.
    ENDIF.
    IF lv_msg IS INITIAL AND ls_ret-id IS NOT INITIAL.
      MESSAGE ID ls_ret-id TYPE 'S' NUMBER ls_ret-number
              WITH ls_ret-message_v1 ls_ret-message_v2
                   ls_ret-message_v3 ls_ret-message_v4 INTO lv_msg.
    ENDIF.
    IF cv_stat IS INITIAL.
      cv_stat = lv_msg.
    ELSE.
      CONCATENATE cv_stat lv_msg INTO cv_stat SEPARATED BY '/'.
    ENDIF.
    wa_log-file = gv_file.
    wa_log-name1 = is_data-name1.
    wa_log-bankl = is_data-bankl.
    wa_log-pan = is_data-pan.
    wa_log-msg = lv_msg.
    APPEND wa_log TO ta_log.
    CLEAR : wa_log, lv_msg, lv_adrnr.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SEND_DET_MAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_det_mail .
  DATA : lv_send_address TYPE so_rec_ext,
         lwa_doc_data    TYPE sodocchgi1,
         lv_lines        TYPE i,
         lv_subject      TYPE so_obj_des,
         lta_receiver    TYPE TABLE OF somlreci1,
         lwa_receiver    TYPE somlreci1,
         lta_message     TYPE TABLE OF solisti1,
         lwa_packing     TYPE sopcklsti1,
         lta_packing     TYPE TABLE OF sopcklsti1,
         lwa_message     TYPE solisti1,
         lv_date(10)     TYPE c.
  DATA : lv_sr_no(4) TYPE c.

  CONSTANTS : lc_v TYPE c VALUE 'V'.

  WAIT UP TO 2 SECONDS.

  SELECT lifnr, j_1ipanno
"Soc atc correction abap7-22.09.2026
***   FROM j_1imovend
   FROM lfa1
"Eoc atc correction abap7-22.09.2026
   INTO TABLE @DATA(lta_vend)
   FOR ALL ENTRIES IN @ta_data
   WHERE j_1ipanno = @ta_data-pan.
  IF sy-subrc = 0.
    SORT lta_vend BY j_1ipanno.
  ENDIF.

  CONCATENATE sy-datum+6(2) '/' sy-datum+4(2) '/' sy-datum+0(4) INTO lv_date.
***********Build Message************
  REFRESH lta_message.
  CLEAR lwa_message.
  lwa_message-line = 'Dear All,<br/>'(026).
  APPEND lwa_message TO lta_message.
*SOC By fatima for 8000007904 on 30.05.2022.

  "email to be send for null ifsc code in file
  IF ta_nullifsc IS NOT INITIAL.
    CLEAR lwa_message.
    lwa_message-line = '<br/>Below Bank Key Are Missing : <br/>'(007).
    APPEND  lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<html> <body>'(036).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<table style="MARGIN: 10px" '(037).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = ' cellspacing="0" cellpadding="3" width="400"'(010).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = ' border="1"><tbody><tr>'(011).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Bank Country</th>'(040).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>IFSC No</th>'(024).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>State</th>'(041).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Name Of HCP</th>'(042).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>PAN No Of HCP</th></tr>'(043).
    APPEND lwa_message TO lta_message.

    LOOP AT ta_nullifsc INTO wa_data.
*   table Contents
      TRY .
*          DATA(lwa_t005u) = ta_t005u[ bezei = wa_data-region ]. "Commented by Gajanan - 21.03.2024 - CR#8000003530
          DATA(lwa_t005u) = ta_t005u[ bland = wa_data-region ]. "Added by Gajanan - 21.03.2024 - CR#8000003530
          ##NO_HANDLER
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
      CLEAR lwa_message.
      lwa_message-line = '<tr>'.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-banks '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-bankl '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' lwa_t005u-bland '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-name1 '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-pan '</td></tr>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR : wa_data,lwa_t005u.
    ENDLOOP.
*   table close
    lwa_message-line = '</tbody> </table>'(014).
    APPEND lwa_message TO lta_message.

*   HTML close
    lwa_message-line = '</body> </html> '(018).
    APPEND lwa_message TO lta_message.
  ENDIF.
  IF ta_noifsc IS NOT INITIAL.
    REFRESH: lta_message.
    CLEAR lwa_message.
    lwa_message-line = '<br/>Please verify and share details in attached format for creation of Bank Key<br/>'(039).
    APPEND  lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<html> <body>'(036).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<table style="MARGIN: 10px" '(037).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = ' cellspacing="0" cellpadding="3" width="400"'(010).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = ' border="1"><tbody><tr>'(011).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Bank Country</th>'(040).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>IFSC No</th>'(024).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>State</th>'(041).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Name Of HCP</th>'(042).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>PAN No Of HCP</th></tr>'(043).
    APPEND lwa_message TO lta_message.

    LOOP AT ta_noifsc INTO wa_data.
*   table Contents

      TRY .
          CLEAR lwa_t005u.
*          lwa_t005u = ta_t005u[ bezei = wa_data-region ]. "Commented by Gajanan - 21.03.2024 - CR#8000003530
          lwa_t005u = ta_t005u[ bland = wa_data-region ]. "Added by Gajanan - 21.03.2024 - CR#8000003530
          ##NO_HANDLER
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
      CLEAR lwa_message.
      lwa_message-line = '<tr>'.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-banks '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-bankl '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' lwa_t005u-bland '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-name1 '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-pan '</td></tr>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR : wa_data,lwa_t005u.
    ENDLOOP.
*   table close
    lwa_message-line = '</tbody> </table>'(014).
    APPEND lwa_message TO lta_message.
  ENDIF.

  IF ta_crvend IS NOT INITIAL OR ta_chvend IS NOT INITIAL.
    CLEAR lwa_message.
    CONCATENATE '<br/>Please find below the list of vendor codes created as on'(027) lv_date
      'for your information and necessary action.<br/>'(028) INTO lwa_message-line SEPARATED BY space.
    APPEND  lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<html> <body>'(036).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<table style="MARGIN: 10px"'(037).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = ' cellspacing="0" cellpadding="3" width="400"'(010).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = ' border="1"><tbody><tr>'(011).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Sr No.</th>'(046).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Doctor Name</th>'(023).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>PAN Card No</th>'(034).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Bank A/C No</th>'(032).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>IFSC No</th>'(024).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Vendor Code</th>'(031).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Remarks</th></tr>'(044).
    APPEND lwa_message TO lta_message.

    CLEAR wa_data.
    CLEAR lv_sr_no.
    lv_sr_no = 0.
    LOOP AT ta_chvend INTO wa_data.
      TRY .
          DATA(lwa_vend) = lta_vend[ j_1ipanno = wa_data-pan ].
          ##NO_HANDLER
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      lv_sr_no = lv_sr_no + 1.
*   table Contents
      CLEAR lwa_message.
      lwa_message-line = '<tr>'.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' lv_sr_no '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-name1 '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-pan '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-bankn '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-bankl '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' lwa_vend-lifnr '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' 'Vendor Changed'(021) '</td></tr>'(020) INTO lwa_message-line.
      APPEND lwa_message TO lta_message.
      CLEAR : wa_data, lwa_vend.
    ENDLOOP.
    LOOP AT ta_crvend INTO wa_data.
      TRY .
          DATA(lwa_vend1) = lta_vend[ j_1ipanno = wa_data-pan ].
        CATCH cx_sy_itab_line_not_found.
          ##NO_HANDLER
      ENDTRY.

      lv_sr_no = lv_sr_no + 1.
*   table Contents
      CLEAR lwa_message.
      lwa_message-line = '<tr>'.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' lv_sr_no '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-name1 '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-pan '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-bankn '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data-bankl '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' lwa_vend1-lifnr '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      IF lwa_vend1-lifnr IS NOT INITIAL.
        CONCATENATE '<td>' 'Vendor Created'(019) '</td></tr>'(020) INTO lwa_message-line.
      ELSE.
        CONCATENATE '<td>' 'Error In File Data'(045) '</td></tr>'(020) INTO lwa_message-line.
      ENDIF.
      APPEND lwa_message TO lta_message.
      CLEAR : wa_data, lwa_vend1.
    ENDLOOP.
*   table close
    lwa_message-line = '</tbody> </table>'(014).
    APPEND lwa_message TO lta_message.

*   HTML close
    lwa_message-line = '</body> </html> '(018).
    APPEND lwa_message TO lta_message.
  ENDIF.
  IF ta_data1 IS NOT INITIAL.
*Start of changes by Anuj Nandapurkar as part of 8000009272 dated 16-10-2023
    CLEAR lwa_message.
    lwa_message-line = '<br/>Please find below table for errors<br/>'.
    APPEND  lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<table style="MARGIN: 10px" '(037).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = ' cellspacing="0" cellpadding="3" width="400"'(010).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = ' border="1"><tbody><tr>'(011).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Doctor Name</th>'(023).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>PAN Card No</th>'(034).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Bank A/C No</th>'(032).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>IFSC No</th>'(024).
    APPEND lwa_message TO lta_message.

    CLEAR lwa_message.
    lwa_message-line = '<th>Remarks</th></tr>'(044).
    APPEND lwa_message TO lta_message.

    LOOP AT ta_data1 INTO wa_data1.

      CLEAR lwa_message.
      lwa_message-line = '<tr>'.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data1-name1 '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data1-pan '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data1-bankn '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.

      CLEAR lwa_message.
      CONCATENATE '<td>' wa_data1-bankl '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.
      CLEAR lwa_message.

      CONCATENATE '<td>' wa_data1-remarks '</td>' INTO lwa_message-line.
      APPEND lwa_message TO lta_message.
      CLEAR : wa_data1, wa_lfbk1, wa_pan, wa_movend1, wa_data.
      DELETE ta_data1 WHERE name1 IS INITIAL.
    ENDLOOP.


    lwa_message-line = '</tbody> </table>'(014).
    APPEND lwa_message TO lta_message.

    lwa_message-line = '</body> </html> '(018).
    APPEND lwa_message TO lta_message.
  ENDIF.
*End of changes by Anuj Nandapurkar as part of CR8000009272 dated 16-10-2023

  CLEAR lwa_message.
  lwa_message = '<br/>Thanks & Regards,<br/>'(029).
  APPEND lwa_message TO lta_message.

  CLEAR lwa_message.
  lwa_message = 'Cipla SAP.<br/>'(015).
  APPEND lwa_message TO lta_message.
***********Build Message************

  CLEAR lwa_packing.
  REFRESH lta_packing.
  lwa_packing-transf_bin = space.
  lwa_packing-head_start = 1.
  lwa_packing-head_num = 0.
  lwa_packing-body_start = 1.
  DESCRIBE TABLE lta_message LINES lwa_packing-body_num.
  lwa_packing-doc_type = 'HTM'.
  APPEND lwa_packing TO lta_packing.

  REFRESH lta_receiver.
  LOOP AT ta_email INTO wa_email WHERE mail = lc_v.
    lwa_receiver-receiver = wa_email-email.
    lwa_receiver-rec_type = 'U'.
    lwa_receiver-com_type = 'INT'.
    lwa_receiver-notif_del = ''.
    lwa_receiver-notif_ndel = ''.
    IF wa_email-cc = abap_true.
      lwa_receiver-copy = abap_true.
    ENDIF.
    APPEND lwa_receiver TO lta_receiver.
    CLEAR : wa_email, lwa_receiver.
  ENDLOOP.

  CONCATENATE 'Created Vendor Code List ('(050) lv_date ')' INTO lv_subject.

  CLEAR lwa_doc_data.
  lwa_doc_data-obj_langu  = sy-langu.
  lwa_doc_data-obj_name   = 'SAPRPT'.
  lwa_doc_data-obj_descr  = lv_subject .
  lwa_doc_data-sensitivty = 'F'.

  CLEAR : lv_lines, lwa_message.
  DESCRIBE TABLE lta_message LINES lv_lines.
  READ TABLE lta_message INTO lwa_message INDEX lv_lines.
  lwa_doc_data-doc_size = ( lv_lines - 1 ) * 255 + strlen( lwa_message ).

  lv_send_address = 'Medline@cipla.com'(038).
  CALL FUNCTION 'SO_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = lwa_doc_data
      sender_address             = lv_send_address
      sender_address_type        = 'INT'
      commit_work                = 'X'
    TABLES
      packing_list               = lta_packing
      contents_txt               = lta_message
      receivers                  = lta_receiver
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
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF ta_log IS INITIAL.
      MESSAGE 'Mail Successfully Sent With Vendor Change/Creation Details.'(005) TYPE co_s.
    ENDIF.
  ENDIF.
  REFRESH lta_vend.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_FILE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_file_data .
  DATA : lwa_tab(1000) TYPE c.
  DATA: lv_pan TYPE sf_searchw.

  IF gv_file IS NOT INITIAL.
    OPEN DATASET gv_file FOR INPUT IN TEXT MODE ENCODING NON-UNICODE.
    IF sy-subrc = 0.
      DO.
        READ DATASET gv_file INTO lwa_tab.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        wa_records-rec = lwa_tab.
        APPEND wa_records TO ta_records.
        CLEAR : wa_records, lwa_tab.
      ENDDO.
    ENDIF.
    CLOSE DATASET gv_file.
  ENDIF.
  IF ta_records IS INITIAL.
    MESSAGE 'No Data Found in file'(002) TYPE co_s DISPLAY LIKE co_e.
    LEAVE TO CURRENT TRANSACTION.
  ENDIF.

  LOOP AT ta_records INTO wa_records.
    SPLIT wa_records-rec AT '|' INTO wa_data-title wa_data-vendor wa_data-name1 wa_data-sort1 wa_data-street
          wa_data-str_suppl1 wa_data-str_suppl2 wa_data-post_code1 wa_data-city1 wa_data-country
          wa_data-region wa_data-tel_number1 wa_data-tel_extens1 wa_data-tel_number2 wa_data-tel_extens2
          wa_data-mob_number1 wa_data-mob_number2 wa_data-fax_number wa_data-smtp_addr wa_data-banks
          wa_data-bankl wa_data-bankn wa_data-koinh wa_data-curren wa_data-pan
          wa_data-gst  wa_data-gstvenclass wa_data-unique_id wa_data-flag wa_data-ind_key . "added by meghna on 05.04.2026 8000004166
*          wa_data-gstvenclass. "Added by Gajanan - 15.03.2024 - CR#8000003035 "commented by meghna on 05.04.2026 8000004166
    "Added ind_key by lalitha - 21.02.2024 - CR#8000004166
    TRANSLATE wa_data-region TO UPPER CASE.

    "Delete trailing commas while uploading .csv file. Added by Anuj Nandapurkar as part of CR8000009272
    lv_pan = wa_data-pan.
    CALL FUNCTION 'SF_SPECIALCHAR_DELETE'
      EXPORTING
        with_specialchar    = lv_pan
      IMPORTING
        without_specialchar = wa_data-pan
      EXCEPTIONS
        result_word_empty   = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    APPEND wa_data TO ta_data.
    CLEAR : wa_data, wa_records.
  ENDLOOP.
  DELETE ta_records INDEX 1.
  DELETE ta_data INDEX 1.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh .
  REFRESH : ta_records, ta_lfbk, ta_data, ta_email, ta_movend, ta_t005u,
            ta_noifsc, ta_chvend, ta_crvend,ta_nullifsc.                      "added by fatima for 8000007904
  CLEAR gv_file.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_output .
  DATA lo_column TYPE REF TO cl_salv_column_table.
  DATA: lwa_key TYPE salv_s_layout_key.
  DATA : lo_alv TYPE REF TO cl_salv_table.

  CONSTANTS : lc_f01 TYPE lvc_fname VALUE 'FILE',
              lc_f02 TYPE lvc_fname VALUE 'NAME1',
              lc_f03 TYPE lvc_fname VALUE 'BANKL',
              lc_f04 TYPE lvc_fname VALUE 'PAN',
              lc_f05 TYPE lvc_fname VALUE 'MSG'.

  CONSTANTS : lc_s01 TYPE char10 VALUE 'File',
              lc_s02 TYPE char10 VALUE 'Vendor',
              lc_s03 TYPE char10 VALUE 'Bank Key',
              lc_s04 TYPE char10 VALUE 'PAN No',
              lc_s05 TYPE char10 VALUE 'Message'.

  CONSTANTS : lc_m01 TYPE char20 VALUE 'File Name',
              lc_m02 TYPE char20 VALUE 'Vendor Name',
              lc_m03 TYPE char20 VALUE 'Bank Key',
              lc_m04 TYPE char20 VALUE 'PAN Number',
              lc_m05 TYPE char20 VALUE 'Message'.

  CONSTANTS : lc_l01 TYPE char40 VALUE 'File Name',
              lc_l02 TYPE char40 VALUE 'Vendor Name',
              lc_l03 TYPE char40 VALUE 'Bank Key',
              lc_l04 TYPE char40 VALUE 'PAN Number',
              lc_l05 TYPE char40 VALUE 'Message'.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = ta_log.

    CATCH cx_salv_msg INTO DATA(lo_alv_msg).
      MESSAGE lo_alv_msg TYPE if_mdg_constants=>c_msgtype_error.
  ENDTRY.

  " Change, Save, Choose layout of ALV output list
  DATA(lo_layout) = lo_alv->get_layout( ).
  lwa_key-report = sy-repid.
  lo_layout->set_key( lwa_key ).
  lo_layout->set_save_restriction( cl_salv_layout=>restrict_none ).

  lo_layout->set_default( abap_true ).


  DATA(lo_columns) = lo_alv->get_columns( ).

  "Files
  TRY.
      lo_column ?= lo_columns->get_column( lc_f01 ).
      lo_column->set_short_text( lc_s01 ).
      lo_column->set_medium_text( lc_m01 ).
      lo_column->set_long_text( lc_l01 ).

    CATCH cx_salv_not_found INTO DATA(lo_salv_not_found).
      MESSAGE lo_salv_not_found TYPE if_mdg_constants=>c_msgtype_error.
  ENDTRY.

  "Vendor Name
  TRY.
      lo_column ?= lo_columns->get_column( lc_f02 ).
      lo_column->set_short_text( lc_s02 ).
      lo_column->set_medium_text( lc_m02 ).
      lo_column->set_long_text( lc_l02 ).

    CATCH cx_salv_not_found INTO lo_salv_not_found.
      MESSAGE lo_salv_not_found TYPE if_mdg_constants=>c_msgtype_error.
  ENDTRY.

  " Bank Key
  TRY.
      lo_column ?= lo_columns->get_column( lc_f03 ).
      lo_column->set_short_text( lc_s03 ).
      lo_column->set_medium_text( lc_m03 ).
      lo_column->set_long_text( lc_l03 ).

    CATCH cx_salv_not_found INTO lo_salv_not_found.
      MESSAGE lo_salv_not_found TYPE if_mdg_constants=>c_msgtype_error.
  ENDTRY.

  " PAN No
  TRY.
      lo_column ?= lo_columns->get_column( lc_f04 ).
      lo_column->set_short_text( lc_s04 ).
      lo_column->set_medium_text( lc_m04 ).
      lo_column->set_long_text( lc_l04 ).

    CATCH cx_salv_not_found INTO lo_salv_not_found.
      MESSAGE lo_salv_not_found TYPE if_mdg_constants=>c_msgtype_error.
  ENDTRY.

  "Message
  TRY.
      lo_column ?= lo_columns->get_column( lc_f05 ).
      lo_column->set_short_text( lc_s05 ).
      lo_column->set_medium_text( lc_m05 ).
      lo_column->set_long_text( lc_l05 ).

    CATCH cx_salv_not_found INTO lo_salv_not_found.
      MESSAGE lo_salv_not_found TYPE if_mdg_constants=>c_msgtype_error.
  ENDTRY.

  lo_columns->set_optimize( 'X' ).

  "Display output in ALV format
  CALL METHOD lo_alv->display( ).

ENDFORM.
