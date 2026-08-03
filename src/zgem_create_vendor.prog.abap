*&---------------------------------------------------------------------*
*& Report  ZGEM_CREATE_VENDOR
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zgem_create_vendor.

TABLES: msg_log.

TYPES: BEGIN OF t_msg_mail,
         msgtxt TYPE string,
       END OF t_msg_mail.
DATA: ist_bdcdata   LIKE bdcdata  OCCURS 0  WITH HEADER LINE,
      ist_xmessages TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      street(120)   TYPE c,
      v_vendor      TYPE lfa1-lifnr,
"Code Remediation changes S4 2025 Conversion Begin of change SAP_ABAP on 13/06/2026
*      v_panno       TYPE j_1imovend-j_1ipanno,
      v_panno       TYPE j_1ipanno,
"Code Remediation changes S4 2025 Conversion End of change SAP_ABAP on 13/06/2026
      wa_vend       TYPE lfa1,
*      wa_lfa1       TYPE lfa1,
      v_regio       TYPE t005u-bland,
      v_state_name  TYPE t005u-bezei,
      v_stcd3       TYPE lfa1-stcd3,
      msg_text_exp  TYPE  msg_text,
      message(100)  TYPE c,
      len           TYPE i.
DATA : mode(1) TYPE c VALUE 'N'.
DATA : ist_msg_text LIKE STANDARD TABLE OF msg_text WITH HEADER LINE,
       it_msg_mail  TYPE TABLE OF t_msg_mail,
       wa_msg_mail  TYPE t_msg_mail.
*PARAMETERS : p_gem TYPE zgem_orderdet-order_id.

*SELECT * FROM  zgem_orderdet
*      INTO TABLE @DATA(it_order)
*      WHERE lifnr EQ ' '.
**      WHERE ORDER_ID = @P_GEM.

IF sy-uname = 'BGJOB_ADM' ."OR SY-UNAME = 'ABAPUSER01'.
  SELECT * FROM  zgem_orderdet
  INTO TABLE @DATA(it_order)
  WHERE lifnr EQ ' '
  AND supply_order_date NOT BETWEEN '2022-03-03T00:00:00' AND '2022-03-18T00:00:00'.
  IF sy-subrc <> 0.
    MESSAGE 'Data from 03/03/2022 to 18/04/2022 can only be created manually' TYPE 'E'.
*      CONTINUE.
  ENDIF.

  SELECT * FROM  zgem_orderdet
   INTO TABLE @DATA(it_order2)
   WHERE lifnr EQ ' '
   AND supply_order_date BETWEEN '2022-03-03T00:00:00' AND '2022-03-18T00:00:00'.
  IF sy-subrc = 0.
" Code Remediation changes S4 2025_1_A Conversion **BEGIN OF CHANGE BY SAP_ABAP 05.06.2026  FOR ATC
" BY TESTING **BEGIN OF CHANGE BY SAPOSS 15052026 FOR ATC .
" Code Remediation changes S4 2025_1_A Conversion * *END OF CHANGE BY SAP_ABAP 05.06.2026  FOR ATC
" testing **BEGIN OF CHANGE BY SAPOSS 15.05.2026  FOR ATC
 SORT IT_ORDER2 BY ORDER_ID.
" testing * *END OF CHANGE BY SAPOSS 15.05.2026  FOR ATC
    DELETE ADJACENT DUPLICATES FROM it_order2 COMPARING order_id.
*      LOOP AT IT_ORDER2 INTO DATA(wa_order2) WHERE SUPPLY_ORDER_DATE NOT BETWEEN '2022-03-03T00:00:00' AND '2022-03-18T00:00:00'.
**        CONTINUE.
*        CONCATENATE 'Data from 03/03/2022 to 18/04/2022 can only be created manually for PO:-' wa_order2-order_id
**        MESSAGE 'Data from 03/03/2022 to 18/04/2022 can only be created manually for PO:-' TYPE 'E'.
*        CONTINUE.
*      ENDLOOP.
  ENDIF.
ELSE.
  SELECT * FROM  zgem_orderdet
   INTO TABLE it_order
   WHERE lifnr EQ ' '.
*      WHERE ORDER_ID = @P_GEM.
ENDIF.


*WHILE 1 =
IF it_order[] IS NOT INITIAL.
  SORT it_order BY order_id.
  DELETE ADJACENT DUPLICATES FROM it_order COMPARING order_id.
*  sort it_order by vendor_gstn.
*  DELETE ADJACENT DUPLICATES FROM it_order COMPARING vendor_gstn.
  LOOP AT it_order INTO DATA(wa_order).
"Code Remediation changes S4 2025 Conversion Begin of change SAP_ABAP on 13/06/2026
*    SELECT l~lifnr, l~regio, l~ktokk, l~stcd3, j~j_1ipanno INTO TABLE @DATA(it_lfa1)
*    FROM lfa1 AS l INNER JOIN j_1imovend AS j
*    ON l~lifnr = j~lifnr
*    WHERE ktokk = 'GEMV'
*    AND ( l~sperr NE 'X'
*    AND
*   l~sperm NE 'X').
    SELECT l~lifnr, l~regio, l~ktokk, l~stcd3, l~j_1ipanno INTO TABLE @DATA(it_lfa1)
    FROM lfa1 AS l
    WHERE ktokk = 'GEMV'
    AND ( l~sperr NE 'X'
    AND
   l~sperm NE 'X').
"Code Remediation changes S4 2025 Conversion End of change SAP_ABAP on 13/06/2026
    IF wa_order-lifnr IS NOT INITIAL.
      MESSAGE 'Vendor already created' TYPE 'E'.
      CONTINUE.
    ENDIF.
    CLEAR:  v_stcd3 , v_regio ,  v_panno  .


    TRANSLATE wa_order-vendor_pan TO UPPER CASE.
    TRANSLATE wa_order-vendor_gstn TO UPPER CASE.

    street = wa_order-vendor_address.
    MOVE wa_order-vendor_gstn TO v_stcd3.
    MOVE wa_order-vendor_pan TO v_panno.

    IF wa_order-vendor_gstn IS NOT INITIAL.
      READ TABLE it_lfa1 INTO DATA(wa_lfa1) WITH KEY stcd3 = v_stcd3.
      IF sy-subrc = 0.
        UPDATE zgem_orderdet SET lifnr = wa_lfa1-lifnr
             WHERE vendor_gstn = v_stcd3.
        COMMIT WORK.
        PERFORM change_bnkdetails.
        MESSAGE 'Vendor with GST No. already Exist' TYPE 'E'.
        CONTINUE.
      ENDIF.
    ENDIF.

    CLEAR:v_state_name, v_regio, len.
    IF wa_order-vendor_state IS NOT INITIAL.
      TRANSLATE wa_order-vendor_state TO UPPER CASE.
" Code Remediation changes S4 2025_1_A Conversion **BEGIN OF CHANGE BY SAP_ABAP 05.06.2026  FOR ATC
*      v_state_name = wa_order-vendor_state.
      TRY.
 V_STATE_NAME = CONV #( WA_ORDER-VENDOR_STATE ).
 CATCH CX_SY_CONVERSION_OVERFLOW CX_SY_ARITHMETIC_OVERFLOW.
 ENDTRY.
" Code Remediation changes S4 2025_1_A Conversion * *END OF CHANGE BY SAP_ABAP 05.06.2026 FOR ATC

      REPLACE ALL OCCURRENCES OF REGEX '[ ]+' IN v_state_name WITH '%'.
" testing **BEGIN OF CHANGE BY SAPOSS 15.05.2026  FOR ATC
*      SELECT SINGLE bland FROM t005u
*        INTO @v_regio
*        WHERE spras = @sy-langu
*        AND   land1 = 'IN'
*        AND   bezei LIKE @v_state_name.
 SELECT BLAND FROM T005U   UP TO 1 ROWS INTO @V_REGIO WHERE SPRAS =
@SY-LANGU AND LAND1 = 'IN' AND BEZEI LIKE @V_STATE_NAME  ORDER BY
PRIMARY KEY.   ENDSELECT.
" testing * *END OF CHANGE BY SAPOSS 15.05.2026 FOR ATC
    ENDIF.

    IF v_regio IS NOT INITIAL AND v_panno IS NOT INITIAL.
      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY regio = v_regio
                                               j_1ipanno = v_panno.
      IF sy-subrc = 0 AND v_stcd3 IS INITIAL.
        UPDATE zgem_orderdet SET lifnr = wa_lfa1-lifnr
           WHERE vendor_pan = v_panno.
        PERFORM change_bnkdetails.
        COMMIT WORK.
        MESSAGE 'Vendor with Pan No. already Exist' TYPE 'E'.
        CONTINUE.
      ENDIF.

    ENDIF.





    """"""""""" call XK01 BDC""""""""""""""""""""""'


    REFRESH: ist_bdcdata, ist_xmessages.

" **begin of change by SAP_ABAP 28.04.2026
" Replace CALL TRANSACTION 'XK01' with VMD_EI_API=>MAINTAIN_BAPI

" --- Original block (lines 146-364) commented out ---
*    PERFORM bdc_dynpro      USING 'SAPMF02K' '0100'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'USE_ZAV'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '/00'.
*    PERFORM bdc_field       USING 'RF02K-BUKRS'
*                                  'OVL'.
************    added by aparna on 11/03/2025
*    IF wa_order-offering_type = 'GOODS' OR wa_order-offering_type = 'goods'.
*
*      PERFORM bdc_field       USING 'RF02K-EKORG'
*                                    'PMAT'.
*    ELSEIF wa_order-offering_type = 'SERVICES' OR  wa_order-offering_type = 'services'.
*
*      PERFORM bdc_field       USING 'RF02K-EKORG'
*                                    'PSRV'.
*    ENDIF.
**    ***********    ended by aparna on 11/03/2025
*    PERFORM bdc_field       USING 'RF02K-KTOKK'
*                                  'GEMV'.
*    PERFORM bdc_field       USING 'USE_ZAV'
*                                  'X'.
*    PERFORM bdc_dynpro      USING 'SAPMF02K' '0111'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '=$2OC'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'ADDR1_DATA-REGION'.
*    PERFORM bdc_field       USING 'ADDR1_DATA-NAME1'
*                                  wa_order-vendor_name.
**      PERFORM bdc_field       USING 'ADDR1_DATA-NAME2'
**                                    ist_vend-vend-name2.
*    PERFORM bdc_field       USING 'ADDR1_DATA-SORT1'
*                                  wa_order-vendor_code.
*    PERFORM bdc_field       USING 'ADDR1_DATA-POST_CODE1'
*                                  wa_order-vendor_pin_code.
*    PERFORM bdc_field       USING 'ADDR1_DATA-CITY1'
*                                  wa_order-vendor_state.
*    PERFORM bdc_field       USING 'ADDR1_DATA-COUNTRY'
*                                  'IN'.
*
*
*    PERFORM bdc_field       USING 'ADDR1_DATA-REGION'
*                                   v_regio.
**      PERFORM bdc_field       USING 'SZA1_D0100-TEL_NUMBER'
**                                     ist_vend-vend-telf1.
**+SP013 - Mobile Number
**      PERFORM bdc_field       USING 'SZA1_D0100-MOB_NUMBER'
**                                        ist_vend-vend-mob_number.
***+SP013 - End
**      PERFORM bdc_field       USING 'SZA1_D0100-FAX_NUMBER'
**                                    ist_vend-vend-telfx+0(26).
**      PERFORM bdc_field       USING 'SZA1_D0100-SMTP_ADDR'
**                                        ist_vend-vend-email.
**---
*    PERFORM bdc_dynpro      USING 'SAPMF02K' '0111'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '/00'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'ADDR1_DATA-LOCATION'.
*    PERFORM bdc_field       USING 'ADDR1_DATA-STR_SUPPL1'
*                                  street+40(40).
**      PERFORM bdc_field       USING 'ADDR1_DATA-STR_SUPPL2'
**                                    ist_vend-vend-suppl2.
*    PERFORM bdc_field       USING 'ADDR1_DATA-STREET'
*                                  street+0(40).
**      PERFORM bdc_field       USING 'ADDR1_DATA-STR_SUPPL3'
**                                    ist_vend-vend-suppl3.
**      PERFORM bdc_field       USING 'ADDR1_DATA-LOCATION'
**                                    ist_vend-vend-ort02.
*
*    PERFORM bdc_dynpro      USING 'SAPMF02K' '0120'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'LFA1-BRSCH'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '/00'.
*
*    PERFORM bdc_field       USING 'LFA1-STCD3'
*                                   wa_order-vendor_gstn.
*
*    TRANSLATE wa_order-is_msme_verified TO UPPER CASE.
*    TRANSLATE wa_order-mse_gender TO UPPER CASE.
*    TRANSLATE wa_order-mse_socail_category TO UPPER CASE.
*
*
*    IF wa_order-is_msme_verified = 'FALSE'.
*      PERFORM bdc_field       USING 'LFA1-BRSCH'
*                                       'VOTH'.
*    ENDIF.
*
*
*    IF wa_order-is_msme_verified = 'TRUE'.
*      IF wa_order-mse_gender = 'MALE'  OR  wa_order-mse_gender = ' ' . " SOC BY ROHIT ON 24/06/2024
*        IF wa_order-mse_socail_category IS INITIAL.
*          PERFORM bdc_field       USING 'LFA1-BRSCH'
*                                         'Z031'.
*        ENDIF.
*
**      ELSEIF wa_order-mse_gender = 'MALE' OR  wa_order-mse_gender = ' '.
**        IF wa_order-mse_socail_category IS NOT INITIAL.
**          PERFORM bdc_field       USING 'LFA1-BRSCH'
**                                         'Z033'.
**        ENDIF.
*
*      ELSEIF wa_order-mse_gender = 'FEMALE' AND wa_order-mse_socail_category IS NOT INITIAL.   " SOC BY ROHIT ON 20-06-2024
**        IF wa_order-mse_socail_category IS NOT INITIAL.
*        PERFORM bdc_field       USING 'LFA1-BRSCH'
*                                       'Z034'.
**        ENDIF.
*
*      ELSEIF wa_order-mse_gender = 'FEMALE' AND wa_order-mse_socail_category IS INITIAL.
**        IF wa_order-mse_socail_category IS INITIAL.
*        PERFORM bdc_field       USING 'LFA1-BRSCH'
*                                       'Z032'.
**        ENDIF.                                                                                "EOC BY ROHIT ON 20-06-2024
*
*      ENDIF.
*
*    ENDIF.
*
*    IF wa_order-is_msme_verified = 'TRUE'.                                             " SOC BY ROHIT ON 20-06-2024
*      IF wa_order-mse_gender = 'MALE' OR  wa_order-mse_gender = ' '.
*        IF wa_order-mse_socail_category IS NOT INITIAL.
*          PERFORM bdc_field       USING 'LFA1-BRSCH'
*                                           'Z033'.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*    PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'LFA1-XZEMP'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '=ENTR'.
*    PERFORM bdc_field       USING 'LFA1-XZEMP'
*                                  'X'.
*    PERFORM bdc_field       USING 'LFBK-BANKS(01)'
*                                   'IN'.
*    PERFORM bdc_field       USING 'LFBK-BANKL(01)'
*                                    wa_order-vendor_bank_ifsc_code.
*    PERFORM bdc_field       USING 'LFBK-BANKN(01)'
*                                    wa_order-vendor_bank_account_no.
*    PERFORM bdc_field       USING 'LFBK-KOINH(01)'
*                                    wa_order-vendor_name.
*    PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'LFBK-BANKS(01)'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '=ENTR'.
*    PERFORM bdc_field       USING 'LFA1-XZEMP'
*                                  'X'.
*    PERFORM bdc_dynpro      USING 'SAPMF02K' '0380'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '=ENTR'.
**End of <RD1K960611>.
*    PERFORM bdc_dynpro      USING 'SAPMF02K' '0210'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'LFB1-FDGRV'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '/00'.
*    PERFORM bdc_field       USING 'LFB1-AKONT'
*                                  '190101'.
*    PERFORM bdc_field       USING 'LFB1-ZUAWA'
*                                    '000'.
*    PERFORM bdc_field       USING 'LFB1-FDGRV'
*                                   'DL'.
*    PERFORM bdc_dynpro      USING 'SAPMF02K' '0215'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'LFB1-REPRF'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '/00'.
*    PERFORM bdc_field       USING 'LFB1-ZTERM'
*                                  '0001'.
*    PERFORM bdc_field       USING 'LFB1-REPRF'
*                                  'X'.
*    IF wa_order-vendor_bank_ifsc_code IS NOT INITIAL.
*      CONDENSE wa_order-vendor_bank_ifsc_code.
*      IF wa_order-vendor_bank_ifsc_code+0(3) = 'SBI'.
*        PERFORM bdc_field       USING 'LFB1-ZWELS'
*                                         '9'.
*      ELSE.
*        PERFORM bdc_field       USING 'LFB1-ZWELS'
*                                       '8'.
*      ENDIF.
*
*
*    ENDIF.
*
*
*
*    PERFORM bdc_dynpro      USING 'SAPMF02K' '0220'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'LFB1-EIKTO'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '/00'.
*    PERFORM bdc_dynpro      USING 'SAPMF02K' '0610'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '/00'.
**    PERFORM bdc_field       USING 'BDC_CURSOR'
**                                  'LFB1-QLAND'.
*    PERFORM bdc_field       USING 'LFB1-QLAND'
*                                       'IN'.                 "ADDED BY ROHIT ON 15-05-2024
*
*    PERFORM bdc_dynpro      USING 'SAPMF02K' '0310'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'LFM1-WEBRE'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '/00'.
*    PERFORM bdc_field       USING 'LFM1-WAERS'
*                                  'INR'.
*    PERFORM bdc_field       USING 'LFM1-KALSK'
*                                  '01'.
*    PERFORM bdc_field       USING 'LFM1-WEBRE'
*                                   'X'.
*    PERFORM bdc_dynpro      USING 'SAPMF02K' '0320'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'RF02K-LIFNR'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '=UPDA'.
*    CALL TRANSACTION 'XK01' USING ist_bdcdata MODE mode

" --- BP-led create via CVI_EI_INBOUND_MAIN (Business Partner + vendor) ----
*   GEMV vendor number range 21 is EXTERNAL (same number as the Business
*   Partner), so the vendor cannot draw its own number - the BP must be
*   created first (internal number from its grouping) and CVI copies that
*   number to the vendor. We therefore build BOTH the partner (BP) node and
*   the vendor node in one CVIS_EI_EXTERN and call CVI_EI_INBOUND_MAIN.
    DATA ls_company     TYPE vmds_ei_company.
    DATA ls_purchasing  TYPE vmds_ei_purchasing.
    DATA ls_bankdetails TYPE cvis_ei_cvi_bankdetail.
    DATA ls_wtax        TYPE vmds_ei_wtax_type.
    DATA lv_brsch       TYPE brsch.
    DATA lt_cvis        TYPE cvis_ei_extern_t.
    DATA ls_cvis        TYPE cvis_ei_extern.
    DATA lt_return      TYPE bapiretm.
    DATA lv_bp_guid     TYPE bu_partner_guid.
    DATA lv_bp          TYPE bu_partner.
    DATA lv_err         TYPE flag.

*   GeM supplier BP grouping (TB001: GEMV = "Account Group - GeM Vendors",
*   BP number range 21). The BP number range must be INTERNAL so CVI copies
*   that number to the external GEMV (KREDITOR) vendor - confirm in BUCF.
    CONSTANTS c_bp_group TYPE bu_group VALUE 'GEMV'.
    CONSTANTS c_bp_categ TYPE bu_type  VALUE '2'.   " 2 = Organization
    CONSTANTS c_industry TYPE lfa1-j_1kftind VALUE 'GEM'. " Type of Industry (functional: default 'GEM')

    CLEAR: lt_cvis, ls_cvis, lt_return, ls_company, ls_purchasing,
           ls_bankdetails, ls_wtax, lv_brsch, lv_bp_guid, lv_bp, lv_err.

    TRY.
        lv_bp_guid = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
    ENDTRY.

*   ===== Business Partner (partner) node - created first, INTERNAL number ==
    ls_cvis-partner-header-object      = 'BusinessPartner'.
    ls_cvis-partner-header-object_task = 'I'.
    ls_cvis-partner-header-object_instance-bpartnerguid = lv_bp_guid.

    ls_cvis-partner-central_data-common-data-bp_control-category   = c_bp_categ.
    ls_cvis-partner-central_data-common-data-bp_control-grouping   = c_bp_group.
    ls_cvis-partner-central_data-common-data-bp_organization-name1 = wa_order-vendor_name.
*   Search term on the BP -> CVI maps it to LFA1-SORTL (required entry field).
*   Guarantee a non-blank value: vendor code, else vendor name, else 'GEM'.
    IF wa_order-vendor_code IS NOT INITIAL.
      ls_cvis-partner-central_data-common-data-bp_centraldata-searchterm1 = wa_order-vendor_code.
    ELSEIF wa_order-vendor_name IS NOT INITIAL.
      ls_cvis-partner-central_data-common-data-bp_centraldata-searchterm1 = wa_order-vendor_name.
    ELSE.
      ls_cvis-partner-central_data-common-data-bp_centraldata-searchterm1 = 'GEM'.
    ENDIF.

*   supplier role FLVN00
    APPEND INITIAL LINE TO ls_cvis-partner-central_data-role-roles
      ASSIGNING FIELD-SYMBOL(<fs_role>).
    <fs_role>-task              = 'I'.
    <fs_role>-data_key          = 'FLVN00'.
    <fs_role>-data-rolecategory = 'FLVN00'.

*   BP address
    APPEND INITIAL LINE TO ls_cvis-partner-central_data-address-addresses
      ASSIGNING FIELD-SYMBOL(<fs_addr>).
    <fs_addr>-task = 'I'.
    <fs_addr>-data-postal-data-street     = street+0(40).
    <fs_addr>-data-postal-data-str_suppl1 = street+40(40).
    <fs_addr>-data-postal-data-city       = wa_order-vendor_state.
    <fs_addr>-data-postal-data-postl_cod1 = wa_order-vendor_pin_code.
    <fs_addr>-data-postal-data-region     = v_regio.
    <fs_addr>-data-postal-data-country    = 'IN'.
    <fs_addr>-data-postal-data-langu      = sy-langu.
*   Standard address usage (XXDEFAULT) - required, otherwise the BP address
*   time-dependency check (BUA_CHECK_ADDRESS_VALIDITY_ALL) aborts with
*   R11 243 "check table is missing".
    APPEND INITIAL LINE TO <fs_addr>-data-addr_usage-addr_usages
      ASSIGNING FIELD-SYMBOL(<fs_ausage>).
    <fs_ausage>-task                 = 'I'.
    <fs_ausage>-data_key-addresstype = 'XXDEFAULT'.
    <fs_ausage>-data_key-valid_to    = '99991231'.
    <fs_ausage>-data-valid_from      = sy-datum.
    <fs_ausage>-data-standard        = abap_true.

*   ===== Vendor node (LFA1 data) - CVI gives it the same number as the BP ==
    ls_cvis-vendor-header-object_task = 'I'.

    ls_cvis-vendor-central_data-central-data-ktokk      = 'GEMV'.
    ls_cvis-vendor-central_data-central-datax-ktokk     = 'X'.
    ls_cvis-vendor-central_data-central-data-stcd3      = wa_order-vendor_gstn.
    ls_cvis-vendor-central_data-central-datax-stcd3     = 'X'.
    ls_cvis-vendor-central_data-central-data-j_1ipanno  = wa_order-vendor_pan.
    ls_cvis-vendor-central_data-central-datax-j_1ipanno = 'X'.
*   Type of Industry (J_1KFTIND) is mandatory for GeM vendors -
*   functional confirmed defaulting it to 'GEM'.
    ls_cvis-vendor-central_data-central-data-j_1kftind  = c_industry.
    ls_cvis-vendor-central_data-central-datax-j_1kftind = 'X'.
    ls_cvis-vendor-central_data-central-data-xzemp      = 'X'.
    ls_cvis-vendor-central_data-central-datax-xzemp     = 'X'.
    ls_cvis-vendor-central_data-address-postal-data-sort1  = wa_order-vendor_code.
    ls_cvis-vendor-central_data-address-postal-datax-sort1 = 'X'.

*   Industry key (BRSCH) by MSME status / gender / social category
*   (restores the conditional logic of the original XK01 BDC).
    TRANSLATE wa_order-is_msme_verified    TO UPPER CASE.
    TRANSLATE wa_order-mse_gender          TO UPPER CASE.
    TRANSLATE wa_order-mse_socail_category TO UPPER CASE.
    IF wa_order-is_msme_verified = 'FALSE'.
      lv_brsch = 'VOTH'.
    ELSEIF wa_order-is_msme_verified = 'TRUE'.
      IF wa_order-mse_gender = 'MALE' OR wa_order-mse_gender = ' '.
        IF wa_order-mse_socail_category IS INITIAL.
          lv_brsch = 'Z031'.
        ELSE.
          lv_brsch = 'Z033'.
        ENDIF.
      ELSEIF wa_order-mse_gender = 'FEMALE'.
        IF wa_order-mse_socail_category IS NOT INITIAL.
          lv_brsch = 'Z034'.
        ELSE.
          lv_brsch = 'Z032'.
        ENDIF.
      ENDIF.
    ENDIF.
    IF lv_brsch IS NOT INITIAL.
      ls_cvis-vendor-central_data-central-data-brsch  = lv_brsch.
      ls_cvis-vendor-central_data-central-datax-brsch = 'X'.
    ENDIF.

*   company code data (OVL)
    ls_company-task           = 'M'.
    ls_company-data_key-bukrs = 'OVL'.
*   Recon account must be zero-padded (SKB1 stores SAKNR ALPHA, e.g. 0000190101)
*   otherwise the foreign-key check fails with "190101 does not exist in OVL".
    ls_company-data-akont     = |{ '190101' ALPHA = IN }|.
    ls_company-datax-akont    = 'X'.
    ls_company-data-zuawa     = '000'.
    ls_company-datax-zuawa    = 'X'.
    ls_company-data-fdgrv     = 'DL'.
    ls_company-datax-fdgrv    = 'X'.
    ls_company-data-zterm     = '0001'.
    ls_company-datax-zterm    = 'X'.
    ls_company-data-reprf     = 'X'.
    ls_company-datax-reprf    = 'X'.
    ls_company-data-qland     = 'IN'.
    ls_company-datax-qland    = 'X'.
*   Payment method: '9' for SBI IFSC, else '8'
    IF wa_order-vendor_bank_ifsc_code IS NOT INITIAL.
      CONDENSE wa_order-vendor_bank_ifsc_code.
      IF wa_order-vendor_bank_ifsc_code+0(3) = 'SBI'.
        ls_company-data-zwels = '9'.
      ELSE.
        ls_company-data-zwels = '8'.
      ENDIF.
      ls_company-datax-zwels = 'X'.
    ENDIF.
*   Withholding tax type I0 (was the separate LFBW / J_1IMOVEND updates)
    ls_wtax-task            = 'M'.
    ls_wtax-data_key-witht  = 'I0'.
    ls_wtax-data-wt_subjct  = 'X'.
    ls_wtax-data-qsrec      = '00'.
    ls_wtax-datax-wt_subjct = abap_true.
    ls_wtax-datax-qsrec     = abap_true.
    APPEND ls_wtax TO ls_company-wtax_type-wtax_type.
    APPEND ls_company TO ls_cvis-vendor-company_data-company.

*   purchasing org data: PMAT (goods) / PSRV (services)
    ls_purchasing-task = 'M'.
    IF wa_order-offering_type = 'GOODS' OR wa_order-offering_type = 'goods'.
      ls_purchasing-data_key-ekorg = 'PMAT'.
    ELSEIF wa_order-offering_type = 'SERVICES' OR wa_order-offering_type = 'services'.
      ls_purchasing-data_key-ekorg = 'PSRV'.
    ENDIF.
    ls_purchasing-data-waers  = 'INR'.
    ls_purchasing-datax-waers = 'X'.
    ls_purchasing-data-kalsk  = '01'.
    ls_purchasing-datax-kalsk = 'X'.
    ls_purchasing-data-webre  = 'X'.
    ls_purchasing-datax-webre = 'X'.
    APPEND ls_purchasing TO ls_cvis-vendor-purchasing_data-purchasing.

*   bank details
    IF wa_order-vendor_bank_account_no IS NOT INITIAL.
      ls_bankdetails-task           = 'M'.
      ls_bankdetails-data_key-banks = 'IN'.
      ls_bankdetails-data_key-bankl = wa_order-vendor_bank_ifsc_code.
      ls_bankdetails-data_key-bankn = wa_order-vendor_bank_account_no.
      ls_bankdetails-data-koinh     = wa_order-vendor_name.
      ls_bankdetails-datax-koinh    = 'X'.
      APPEND ls_bankdetails TO ls_cvis-vendor-central_data-bankdetail-bankdetails.
    ENDIF.

    APPEND ls_cvis TO lt_cvis.

*   ===== create BP + vendor (same number) =================================
    CALL FUNCTION 'CVI_EI_INBOUND_MAIN'
      EXPORTING
        i_data   = lt_cvis
      IMPORTING
        e_return = lt_return.

    REFRESH it_msg_mail.
    CLEAR: wa_msg_mail, v_vendor, lv_err.

    LOOP AT lt_return INTO DATA(ls_return).
      LOOP AT ls_return-object_msg INTO DATA(ls_ob_msg).
        IF ls_ob_msg-type = 'E' OR ls_ob_msg-type = 'A'.
          lv_err = 'X'.
          WRITE :/ ls_ob_msg-message.
          wa_msg_mail-msgtxt = ls_ob_msg-message.
          APPEND wa_msg_mail TO it_msg_mail.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    IF lv_err IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
*     BP number = vendor number (same-number CVI); read it back by GUID.
      SELECT SINGLE partner FROM but000 INTO lv_bp
        WHERE partner_guid = lv_bp_guid.
      v_vendor = lv_bp.
*     Fallback: read the created GEMV vendor by GSTIN / PAN.
      IF v_vendor IS INITIAL.
        IF wa_order-vendor_gstn IS NOT INITIAL.
          SELECT SINGLE lifnr FROM lfa1 INTO v_vendor
            WHERE ktokk = 'GEMV' AND stcd3 = wa_order-vendor_gstn.
        ELSEIF wa_order-vendor_pan IS NOT INITIAL.
          SELECT SINGLE lifnr FROM lfa1 INTO v_vendor
            WHERE ktokk = 'GEMV' AND j_1ipanno = wa_order-vendor_pan.
        ENDIF.
      ENDIF.

      IF v_vendor IS NOT INITIAL.
        v_panno = wa_order-vendor_pan.
        IF wa_order-vendor_gstn IS NOT INITIAL.
          UPDATE zgem_orderdet SET lifnr = v_vendor
            WHERE vendor_gstn = wa_order-vendor_gstn.
        ELSEIF v_panno IS NOT INITIAL.
          UPDATE zgem_orderdet SET lifnr = v_vendor
            WHERE vendor_pan = v_panno.
        ENDIF.
        COMMIT WORK.

        CONCATENATE 'Vendor' v_vendor 'created.' INTO message SEPARATED BY space.
        WRITE : / message.
        PERFORM vcs_mail.
      ENDIF.
    ELSE.
*     Errors -> roll back; the collected messages are mailed at the loop end.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

    IF it_order2 IS NOT INITIAL.
      LOOP AT it_order2 INTO DATA(wa_order2) WHERE supply_order_date BETWEEN '2022-03-03T00:00:00' AND '2022-03-18T00:00:00'.
*        CONTINUE.
        DATA(msg_sodt) = 'Data from 03/03/2022 to 18/04/2022 can only be created manually for PO:-'.
        WRITE :/ msg_sodt , wa_order2-order_id .
        wa_msg_mail-msgtxt = msg_sodt.
        APPEND wa_msg_mail TO it_msg_mail.
        CLEAR : msg_sodt,wa_order2.
      ENDLOOP.
    ENDIF.

    IF it_msg_mail IS NOT INITIAL.
      PERFORM err_mail.
    ENDIF.



    CLEAR wa_lfa1.
    WAIT UP TO 10 SECONDS.
    CLEAR : v_vendor.
    REFRESH: it_lfa1[].
  ENDLOOP.
ENDIF.


*FORM bdc_dynpro USING l_program l_dynpro.
*  CLEAR ist_bdcdata.
" **begin of change by SAP_ABAP 28.04.2026
" Replace CALL TRANSACTION 'XK02' with VMD_EI_API=>MAINTAIN_BAPI

" --- Original block (lines 475-1058) commented out ---
*  ist_bdcdata-program  = l_program.
*  ist_bdcdata-dynpro   = l_dynpro.
*  ist_bdcdata-dynbegin = 'X'.
*  APPEND ist_bdcdata.
*ENDFORM.                    " bdc_dynpro
***&---------------------------------------------------------------------
**
***       BDC FIELD
***----------------------------------------------------------------------
**
***      -->P_0501   text
***      -->P_0502   text
***----------------------------------------------------------------------
**
*FORM bdc_field USING l_fnam l_fval.
*
*  CLEAR ist_bdcdata.
*  ist_bdcdata-fnam = l_fnam.
*  ist_bdcdata-fval = l_fval.
*  SHIFT ist_bdcdata-fval LEFT DELETING LEADING space.
*  APPEND ist_bdcdata.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  VCS_MAIL
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
FORM vcs_mail .

  DATA :   i_text          TYPE bcsy_text. "Table for body
  DATA : w_text          LIKE LINE OF i_text.
  DATA :i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE,
        i_record    LIKE solisti1 OCCURS 0 WITH HEADER LINE.
  DATA : lo_document     TYPE REF TO cl_document_bcs VALUE IS INITIAL. "document object

  DATA : p_sub TYPE char50. "email subject
  DATA : lo_send_request TYPE REF TO cl_bcs VALUE IS INITIAL.
  DATA :lv_data_string TYPE string.
  DATA : lv_len_in    LIKE sood-objlen.
  DATA : lv_filesize TYPE so_obj_len.

  CONSTANTS:
    con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
    con_cret TYPE c VALUE cl_abap_char_utilities=>cr_lf.
  DATA : l_attsubject TYPE sood-objdes.
  DATA : lo_sender       TYPE REF TO if_sender_bcs VALUE IS INITIAL. "sender
  DATA : lo_recipient    TYPE REF TO if_recipient_bcs VALUE IS INITIAL. "recipient
  DATA : p_email TYPE adr6-smtp_addr.

  DATA:
    t_header  TYPE STANDARD TABLE OF w3head WITH HEADER LINE, "Header
    t_fields  TYPE STANDARD TABLE OF w3fields WITH HEADER LINE, "Fields
    t_html    TYPE STANDARD TABLE OF w3html WITH HEADER LINE, "Html
    t_html1   TYPE STANDARD TABLE OF w3html WITH HEADER LINE, "Html
    wa_header TYPE w3head,
    w_head    TYPE w3head.

  SELECT *
  FROM zovl_email
  INTO TABLE @DATA(it_email)
  WHERE tcode = 'ZGEM_VENDOR'.

  p_sub = 'New GeM Vendor created'.
  lo_send_request = cl_bcs=>create_persistent( ).
*



  t_html-line = '<table>'.
  APPEND t_html.
  CLEAR t_html.
*t_html-line = '<thead>'.
* APPEND t_html.
* CLEAR t_html.
  t_html-line = '<tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<td></td></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<td><p>Please find below the GeM Vendor bank details for updation with SBI<p></td>'.
* CONCATENATE  t_html-line v_date into t_html-line.
*    CONCATENATE '<tr><td>Previous Date</td>'  INTO t_html-line SEPARATED BY space .
*    APPEND t_html.
*    CLEAR t_html.
*   t_html-line = '<td><p>Please do not reply on this mail, this is only for your information<p></td>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr></tr></table>'.
  APPEND t_html.
  CLEAR t_html.
* t_html-line = '</thead>'.
* APPEND t_html.
* CLEAR t_html.
  t_html-line = '<table border = "1">'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr>'.
  APPEND t_html.
  CLEAR t_html.
*    LOOP AT it_fcat.
  CONCATENATE '<th bgcolor = "#F0D086" fgcolor = "black">'
  'Vendor Code'
  '</th>' INTO t_html-line.
  APPEND t_html.
  CLEAR t_html.

  CONCATENATE '<th bgcolor = "#F0D086" fgcolor = "black">'
  'Vendor Name'
  '</th>' INTO t_html-line.
  APPEND t_html.
  CLEAR t_html.

  CONCATENATE '<th bgcolor = "#F0D086" fgcolor = "black">'
  'Vendor PAN'
  '</th>' INTO t_html-line.
  APPEND t_html.
  CLEAR t_html.

  CONCATENATE '<th bgcolor = "#F0D086" fgcolor = "black">'
  'Vendor GSTN'
  '</th>' INTO t_html-line.
  APPEND t_html.
  CLEAR t_html.

  CONCATENATE '<th bgcolor = "#F0D086" fgcolor = "black">'
  'Vendor Bank Account'
  '</th>' INTO t_html-line.
  APPEND t_html.
  CLEAR t_html.

  CONCATENATE '<th bgcolor = "#F0D086" fgcolor = "black">'
  'Vendor Bank IFSC'
  '</th>' INTO t_html-line.
  APPEND t_html.
  CLEAR t_html.
*    ENDLOOP.
  t_html-line = '</tr>'.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '<tr>'.
  APPEND t_html.
  CLEAR t_html.


  t_html-line = '<td>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = v_vendor.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '</td>'.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '<td>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = wa_order-vendor_name.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '</td>'.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '<td>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = wa_order-vendor_pan.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '</td>'.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '<td>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = wa_order-vendor_gstn.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '</td>'.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '<td>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = wa_order-vendor_bank_account_no.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '</td>'.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '<td>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = wa_order-vendor_bank_ifsc_code.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '</td>'.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '</tr>'.
  APPEND t_html.
  CLEAR t_html.
*   ENDLOOP.

  t_html-line = '</table>'.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '<table>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = ' <tr></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr><p>Regards<p></tr>'.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '<tr><p>SAP Team - ONGC Videsh<p></tr>'.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '<tr></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr><p>*This is a SAP-system generated e-mail for your information, please do not reply to this message.*<p></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr><td></td></tr></table>'.
  APPEND t_html.
  CLEAR t_html.
  REFRESH i_text[].
  APPEND LINES OF t_html TO i_text.
  lo_document = cl_document_bcs=>create_document( "create document
  i_type = 'HTM' "Type of document HTM, TXT etc
  i_text =  i_text "email body internal table
  i_subject = p_sub ). "email subject here p_sub input parameter
* Pass the document to send request
  lo_send_request->set_document( lo_document ).
*
*  LV_DATA_STRING = I_RECORD.
*  LV_FILESIZE    = V_BIN_FILESIZE. " LV_LEN_IN.
*  CONDENSE LV_FILESIZE.
*
*  CLEAR : L_ATTSUBJECT.
*  L_ATTSUBJECT = 'Bank Signatory Details'.
*
*  TRY.
*      LO_DOCUMENT->ADD_ATTACHMENT( EXPORTING
*                                      I_ATTACHMENT_TYPE = 'PDF' "'XLS'
*                                      I_ATTACHMENT_SIZE   = LV_FILESIZE
*                                      I_ATTACHMENT_SUBJECT = L_ATTSUBJECT
*                                      I_ATT_CONTENT_TEXT  = I_RECORD[] ).
**                                      i_att_content_hex = lit_binary_content  ).
**          CATCH cx_document_bcs INTO lx_document_bcs.
*  ENDTRY.


  TRY.
      lo_sender = cl_sapuser_bcs=>create( sy-uname ). "sender is the logged in user
* Set sender to send request
      lo_send_request->set_sender(
      EXPORTING
      i_sender = lo_sender ).
*    CATCH CX_ADDRESS_BCS.
****Catch exception here
  ENDTRY.
**Set recipient

  LOOP AT it_email INTO DATA(wa_email).
    p_email = wa_email-smtp_addr.
    IF sy-tabix = 1.
      lo_recipient = cl_cam_address_bcs=>create_internet_address( p_email ). "Here Recipient is email input p_email
      TRY.
          lo_send_request->add_recipient(
              EXPORTING
              i_recipient = lo_recipient
              i_express = 'X' ).
*  CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
**Catch exception here
      ENDTRY.
      CLEAR : p_email.
    ELSE.
      p_email = wa_email-smtp_addr.
      lo_recipient = cl_cam_address_bcs=>create_internet_address( p_email ). "cc
      TRY.
          lo_send_request->add_recipient(
          EXPORTING
          i_recipient = lo_recipient
          i_express = 'X'
          i_copy = abap_true ).
      ENDTRY.
    ENDIF.
  ENDLOOP.
  TRY.
      CALL METHOD lo_send_request->set_send_immediately
        EXPORTING
          i_send_immediately = 'X'.  "p_send. "here selection screen input p_send
*    CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
**Catch exception here
  ENDTRY.
  TRY.
** Send email
      lo_send_request->send(
      EXPORTING
      i_with_error_screen = 'X' ).
      COMMIT WORK.
      IF sy-subrc = 0. "mail sent successfully
*        CLEAR : WA_FINAL.
        MESSAGE 'Mail Sent Successfully' TYPE 'S'.
      ENDIF.
*    CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
*catch exception here
  ENDTRY.
ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  ERR_MAIL
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
FORM err_mail .

  DATA :   i_text          TYPE bcsy_text. "Table for body
  DATA : w_text          LIKE LINE OF i_text.
  DATA :i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE,
        i_record    LIKE solisti1 OCCURS 0 WITH HEADER LINE.
  DATA : lo_document     TYPE REF TO cl_document_bcs VALUE IS INITIAL. "document object

  DATA : p_sub TYPE char50. "email subject
  DATA : lo_send_request TYPE REF TO cl_bcs VALUE IS INITIAL.
  DATA :lv_data_string TYPE string.
  DATA : lv_len_in    LIKE sood-objlen.
  DATA : lv_filesize TYPE so_obj_len.

  CONSTANTS:
    con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
    con_cret TYPE c VALUE cl_abap_char_utilities=>cr_lf.
  DATA : l_attsubject TYPE sood-objdes.
  DATA : lo_sender       TYPE REF TO if_sender_bcs VALUE IS INITIAL. "sender
  DATA : lo_recipient    TYPE REF TO if_recipient_bcs VALUE IS INITIAL. "recipient
  DATA : p_email TYPE adr6-smtp_addr.

  DATA:
    t_header  TYPE STANDARD TABLE OF w3head WITH HEADER LINE, "Header
    t_fields  TYPE STANDARD TABLE OF w3fields WITH HEADER LINE, "Fields
    t_html    TYPE STANDARD TABLE OF w3html WITH HEADER LINE, "Html
    t_html1   TYPE STANDARD TABLE OF w3html WITH HEADER LINE, "Html
    wa_header TYPE w3head,
    w_head    TYPE w3head.

  SELECT *
  FROM zovl_email
  INTO TABLE @DATA(it_email)
  WHERE tcode = 'ZGEM_ERR'.

  p_sub = 'Error in GeM Vendor creation'.
  lo_send_request = cl_bcs=>create_persistent( ).


  t_html-line = '<table>'.
  APPEND t_html.
  CLEAR t_html.
*t_html-line = '<thead>'.
* APPEND t_html.
* CLEAR t_html.
  t_html-line = '<tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<td></td></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr><p>Errors encountered during GeM vendor creation, please find details below:<p></tr>'.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '<tr></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr></tr>'.
  APPEND t_html.
  CLEAR t_html.
  CONCATENATE '<tr><p>' 'Vendor -' wa_order-vendor_code '-' wa_order-vendor_name '<p></tr>' INTO t_html-line SEPARATED BY space.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr></tr>'.
  APPEND t_html.
  CLEAR t_html.
  LOOP AT it_msg_mail INTO wa_msg_mail.
    CONCATENATE '<tr><p>' '-' wa_msg_mail-msgtxt '<p></tr>' INTO t_html-line SEPARATED BY space.
    APPEND t_html.
    CLEAR t_html.
  ENDLOOP.
  t_html-line = '<tr></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr><p>Regards<p></tr>'.
  APPEND t_html.
  CLEAR t_html.

  t_html-line = '<tr><p>SAP Team - ONGC Videsh<p></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr><p>*This is a SAP-system generated e-mail for your information, please do not reply to this message.*<p></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr></tr>'.
  APPEND t_html.
  CLEAR t_html.
  t_html-line = '<tr><td></td></tr></table>'.
  APPEND t_html.
  CLEAR t_html.

  REFRESH i_text[].
  APPEND LINES OF t_html TO i_text.

  lo_document = cl_document_bcs=>create_document( "create document
  i_type = 'HTM' "Type of document HTM, TXT etc
  i_text =  i_text "email body internal table
  i_subject = p_sub ). "email subject here p_sub input parameter
* Pass the document to send request
  lo_send_request->set_document( lo_document ).
*
*  LV_DATA_STRING = I_RECORD.
*  LV_FILESIZE    = V_BIN_FILESIZE. " LV_LEN_IN.
*  CONDENSE LV_FILESIZE.
*
*  CLEAR : L_ATTSUBJECT.
*  L_ATTSUBJECT = 'Bank Signatory Details'.
*
*  TRY.
*      LO_DOCUMENT->ADD_ATTACHMENT( EXPORTING
*                                      I_ATTACHMENT_TYPE = 'PDF' "'XLS'
*                                      I_ATTACHMENT_SIZE   = LV_FILESIZE
*                                      I_ATTACHMENT_SUBJECT = L_ATTSUBJECT
*                                      I_ATT_CONTENT_TEXT  = I_RECORD[] ).
**                                      i_att_content_hex = lit_binary_content  ).
**          CATCH cx_document_bcs INTO lx_document_bcs.
*  ENDTRY.


  TRY.
      lo_sender = cl_sapuser_bcs=>create( sy-uname ). "sender is the logged in user
* Set sender to send request
      lo_send_request->set_sender(
      EXPORTING
      i_sender = lo_sender ).
*    CATCH CX_ADDRESS_BCS.
****Catch exception here
  ENDTRY.
**Set recipient

  LOOP AT it_email INTO DATA(wa_email).
    p_email = wa_email-smtp_addr.
    IF sy-tabix = 1.
      lo_recipient = cl_cam_address_bcs=>create_internet_address( p_email ). "Here Recipient is email input p_email
      TRY.
          lo_send_request->add_recipient(
              EXPORTING
              i_recipient = lo_recipient
              i_express = 'X' ).
*  CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
**Catch exception here
      ENDTRY.
      CLEAR : p_email.
    ELSE.
      p_email = wa_email-smtp_addr.
      lo_recipient = cl_cam_address_bcs=>create_internet_address( p_email ). "cc
      TRY.
          lo_send_request->add_recipient(
          EXPORTING
          i_recipient = lo_recipient
          i_express = 'X'
          i_copy = abap_true ).
      ENDTRY.
    ENDIF.
  ENDLOOP.
  TRY.
      CALL METHOD lo_send_request->set_send_immediately
        EXPORTING
          i_send_immediately = 'X'.  "p_send. "here selection screen input p_send
*    CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
**Catch exception here
  ENDTRY.
  TRY.
** Send email
      lo_send_request->send(
      EXPORTING
      i_with_error_screen = 'X' ).
      COMMIT WORK.
      IF sy-subrc = 0. "mail sent successfully
*        CLEAR : WA_FINAL.
        MESSAGE 'Mail Sent Successfully' TYPE 'S'.
      ENDIF.
*    CATCH CX_SEND_REQ_BCS INTO BCS_EXCEPTION .
*catch exception here
  ENDTRY.
ENDFORM.
**&---------------------------------------------------------------------*
**&      Form  CHANGE_BNKDETAILS
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**  -->  p1        text
**  <--  p2        text
**----------------------------------------------------------------------*
FORM change_bnkdetails .
*
*
*  PERFORM bdc_dynpro      USING 'SAPMF02K' '0101'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'RF02K-BUKRS'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '/00'.
*  PERFORM bdc_field       USING 'RF02K-LIFNR'
*                                 wa_lfa1-lifnr.
*  PERFORM bdc_field       USING 'RF02K-BUKRS'
*                                'OVL'.
*  PERFORM bdc_field       USING 'RF02K-D0110'
*                                'X'.
*  PERFORM bdc_field       USING 'RF02K-D0130'
*                                'X'.
*  PERFORM bdc_field       USING 'USE_ZAV'
*                                'X'.
*  PERFORM bdc_dynpro      USING 'SAPMF02K' '0111'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=VW'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'ADDR1_DATA-STR_SUPPL1'.
**perform bdc_field       using 'ADDR1_DATA-NAME1'
**                              'UTILITYP'.
**perform bdc_field       using 'ADDR1_DATA-SORT1'
**                              'GEM0001577'.
*  PERFORM bdc_field       USING 'ADDR1_DATA-STR_SUPPL1'
*                                street+55(50).
*  PERFORM bdc_field       USING 'ADDR1_DATA-STREET'
*                                street+0(55).
**perform bdc_field       using 'ADDR1_DATA-CITY1'
**                              'DELHI'.
**perform bdc_field       using 'ADDR1_DATA-COUNTRY'
**                              'IN'.
**perform bdc_field       using 'ADDR1_DATA-REGION'
**                              '07'.
**perform bdc_field       using 'ADDR1_DATA-LANGU'
**                              'EN'.
*  PERFORM bdc_dynpro      USING 'SAPMF02K' '0130'.
*  PERFORM bdc_field       USING 'BDC_CURSOR'
*                                'LFBK-BANKN(01)'.
*  PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=UPDA'.
*  PERFORM bdc_field       USING 'LFA1-XZEMP'
*                                'X'.
*  PERFORM bdc_field       USING 'LFBK-BANKL(01)'
*                                 wa_order-vendor_bank_ifsc_code.
*  PERFORM bdc_field       USING 'LFBK-BANKN(01)'
*                                wa_order-vendor_bank_account_no.
*  PERFORM bdc_field       USING 'LFBK-KOINH(01)'
*                              wa_order-vendor_name.
*  CALL TRANSACTION 'XK02' USING ist_bdcdata MODE mode

" --- S/4 vendor bank/address change via VMD_EI_API=>MAINTAIN_BAPI ---
  DATA ls_is_master_data           TYPE vmds_ei_main.
  DATA ls_es_master_data_correct   TYPE vmds_ei_main.
  DATA ls_es_message_correct       TYPE cvis_message.
  DATA ls_es_master_data_defective TYPE vmds_ei_main.
  DATA ls_es_message_defective     TYPE cvis_message.
  DATA ls_vendors                  TYPE vmds_ei_extern.
  DATA ls_bankdetails              TYPE cvis_ei_cvi_bankdetail.

  CLEAR: ls_is_master_data, ls_es_master_data_correct, ls_es_message_correct,
         ls_es_master_data_defective, ls_es_message_defective,
         ls_vendors, ls_bankdetails.

*   header: U = update the existing vendor (LIFNR = wa_lfa1-lifnr)
  ls_vendors-header-object_instance-lifnr = wa_lfa1-lifnr.
  ls_vendors-header-object_task           = 'U'.

*   updated street / address
  ls_vendors-central_data-address-postal-data-str_suppl1  = street+55(50).
  ls_vendors-central_data-address-postal-datax-str_suppl1 = 'X'.
  ls_vendors-central_data-address-postal-data-street      = street+0(55).
  ls_vendors-central_data-address-postal-datax-street     = 'X'.
  ls_vendors-central_data-central-data-xzemp   = 'X'.
  ls_vendors-central_data-central-datax-xzemp  = 'X'.

*   bank details (M = insert or update line 01)
  ls_bankdetails-task           = 'M'.
  ls_bankdetails-data_key-banks = 'IN'.
  ls_bankdetails-data_key-bankl = wa_order-vendor_bank_ifsc_code.
  ls_bankdetails-data_key-bankn = wa_order-vendor_bank_account_no.
  ls_bankdetails-data-koinh     = wa_order-vendor_name.
  ls_bankdetails-datax-koinh    = 'X'.
  APPEND ls_bankdetails TO ls_vendors-central_data-bankdetail-bankdetails.

  APPEND ls_vendors TO ls_is_master_data-vendors.

  CALL METHOD vmd_ei_api=>maintain_bapi
    EXPORTING
      iv_test_run              = abap_false
      iv_collect_messages      = abap_true
      is_master_data           = ls_is_master_data
    IMPORTING
      es_master_data_correct   = ls_es_master_data_correct
      es_message_correct       = ls_es_message_correct
      es_master_data_defective = ls_es_master_data_defective
      es_message_defective     = ls_es_message_defective.

  REFRESH it_msg_mail.
  CLEAR wa_msg_mail.
  IF ls_es_master_data_defective-vendors IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    LOOP AT ls_es_message_defective-messages INTO DATA(ls_msg_d).
      WRITE :/ ls_msg_d-message.
      wa_msg_mail-msgtxt = ls_msg_d-message.
      APPEND wa_msg_mail TO it_msg_mail.
    ENDLOOP.
  ENDIF.
  IF it_msg_mail IS NOT INITIAL.
    PERFORM err_mail.
  ENDIF.

ENDFORM.
