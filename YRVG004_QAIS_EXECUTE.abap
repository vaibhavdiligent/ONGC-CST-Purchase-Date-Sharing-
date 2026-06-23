*&                                                                     *
*& Report  YRVG004_QAIS_EXECUTE
*&
*&                                                                     *
*&
*&
*&                                                                     *
REPORT  yrvg004_qais_execute MESSAGE-ID yv01.

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
* START OF SELECTION                                                   *
START-OF-SELECTION.
*Checking Authorization
  LOOP AT s_vkbur.
    AUTHORITY-CHECK OBJECT 'YV_VKBUR'
             ID 'VKBUR' FIELD s_vkbur-low .
    IF sy-subrc <> 0  .
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
* SOC Commeneted by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000008188
  IF r_quater = 'X'.
    PERFORM format_data. "Uncommented by Adarsh/Archana against charm:4000006427 TR:DVRK9A12BV
  ENDIF.
** EOC Commeneted by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000008188
**SOC by ujjjwal/priyanka on charm 4000002906 on 10-10-2020 to create new additional MQAIS link discount


*  IF r_month = 'X' OR r_month1 EQ 'X' OR r_rpd EQ 'X' OR r_rhd EQ 'X' OR r_rlld EQ 'X'.

***SOC by Abhinav/Archna/Vishal on Charm  4000006149, TR DVRK9A112V
*      IF r_month = 'X' OR r_month1 EQ 'X' OR r_rpd EQ 'X' OR r_rhd EQ 'X' OR r_rlld EQ 'X' or c_maint EQ 'X'." SOC Commented by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007222
  IF r_month = 'X' OR r_month1 EQ 'X' OR r_rpd EQ 'X' OR r_rhd EQ 'X' OR r_rlld EQ 'X' OR c_maint EQ 'X' OR c_maint1 EQ 'X' ." SOC by Chilukuri
***EOC by Abhinav/Archna/Vishal on Charm  4000006149, TR DVRK9A112V
** SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000008707
    IF ls_psdq = 'X'.
      REFRESH: it_yrva_qais_data_n[], it_yrva_qais_data_n_temp[].
      it_yrva_qais_data_n[]      = it_yrva_qais_data[].
      it_yrva_qais_data_n_temp[] = it_yrva_qais_data_temp[].
    ENDIF.
** EOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000008707
*    IF r_month = 'X' OR r_month1 EQ 'X' OR r_rpd EQ 'X'.
**EOC by ujjjwal/priyanka on charm 4000002906 on 10-10-2020 to create new additional MQAIS link discount
    PERFORM format_data_month.
** SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000008707
    IF ls_psdq = 'X'.
      REFRESH: it_yrva_qais_data2[], it_yrva_qais_data_temp2[].

      it_yrva_qais_data2[]      = it_yrva_qais_data[].
      it_yrva_qais_data_temp2[] = it_yrva_qais_data_temp[].
      REFRESH: it_s922_newq[].
      it_s922_newq[] = it_s922[].
**      APPEND LINES OF it_s922_n TO it_s922.
      it_s922 = it_s922_n.
      REFRESH: it_yrva_qais_data[], it_yrva_qais_data_temp[].
      it_yrva_qais_data[]      =  it_yrva_qais_data_n[].
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
** EOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000008707
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
