*&---------------------------------------------------------------------*
*& Report  YRVG004_QAIS_EXECUTE
*&
*&---------------------------------------------------------------------*
*& Title   : CIS Execute
*& Type    : 1 Reporting
*& Status  : K Customer Program
*& Package : YV01
*& Author  : AB_SACHIN
*& Created : 12.09.2013
*&---------------------------------------------------------------------*
REPORT yrvg004_qais_execute MESSAGE-ID yv01.

*Top include.
INCLUDE yrvg004_qais_execute_top.
*Selection Screen and Screen validations.
INCLUDE yrvg004_qais_execute_sel.
*Form Definition.
INCLUDE yrvg004_qais_execute_f01.

INITIALIZATION.
  GET PARAMETER ID 'ZFL' FIELD lv_siml.
  IF lv_siml EQ 'X'.
    MESSAGE 'This option is for getting a snapshot of CIS status as on date when performed. Please use this option diligently.' TYPE 'I'.
  ENDIF.

* START OF SELECTION ---------------------------------------------------*
START-OF-SELECTION.
*Checking Authorization
  LOOP AT s_vkbur.
    AUTHORITY-CHECK OBJECT 'YV_VKBUR'
             ID 'VKBUR' FIELD s_vkbur-low .
    IF sy-subrc <> 0 .
      MESSAGE e081 WITH s_vkbur-low.
    ENDIF.
  ENDLOOP.
  SET PF-STATUS 'STANDARD'.
*Screen level validations.
  PERFORM validation.
*fetch respective data records.
  PERFORM get_data.
* get the description of sale order
  PERFORM qais_remarks.
*format Data
  IF r_quater = 'X'.
    PERFORM format_data.
  ENDIF.

  IF r_month = 'X' OR r_month1 EQ 'X' OR r_rpd EQ 'X' OR r_rhd EQ 'X' OR
     r_rlld EQ 'X' OR c_maint EQ 'X' OR c_maint1 EQ 'X' .
    IF ls_psdq = 'X'.
      REFRESH: it_yrva_qais_data_n[], it_yrva_qais_data_n_temp[].
      it_yrva_qais_data_n[]      = it_yrva_qais_data[].
      it_yrva_qais_data_n_temp[] = it_yrva_qais_data_temp[].
    ENDIF.
    PERFORM format_data_month.
    IF ls_psdq = 'X'.
      REFRESH: it_yrva_qais_data2[], it_yrva_qais_data_temp2[].
      it_yrva_qais_data2[]      = it_yrva_qais_data[].
      it_yrva_qais_data_temp2[] = it_yrva_qais_data_temp[].
      REFRESH: it_s922_newq[].
      it_s922_newq[] = it_s922[].
      it_s922 = it_s922_n.
      REFRESH: it_yrva_qais_data[], it_yrva_qais_data_temp[].
      it_yrva_qais_data[]      = it_yrva_qais_data_n[].
      it_yrva_qais_data_temp[] = it_yrva_qais_data_n_temp[].
      PERFORM format_data_month.
      REFRESH: it_s922[].
      it_s922[] = it_s922_newq[].
      REFRESH: it_yrva_qais_data_n[], it_yrva_qais_data_n_temp[].
      it_yrva_qais_data_n[]      = it_yrva_qais_data[].
      it_yrva_qais_data_n_temp[] = it_yrva_qais_data_temp[].
      REFRESH: it_yrva_qais_data[], it_yrva_qais_data_temp[].
      it_yrva_qais_data[]      = it_yrva_qais_data2[].
      it_yrva_qais_data_temp[] = it_yrva_qais_data_temp2[].
    ENDIF.
  ENDIF.
*Calculate the discount for respective customers.
  PERFORM calculate_discount.
*Fetch the customer names.
  PERFORM get_cust_name.

END-OF-SELECTION.
*generate field catlofs.
  PERFORM create_field_catalog.
*Display the final records.
  PERFORM display_list.
