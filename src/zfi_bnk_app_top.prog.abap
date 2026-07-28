*&---------------------------------------------------------------------*
*& Include          ZFI_BNK_APP_TOP
*&---------------------------------------------------------------------*


TYPES : BEGIN OF ty_final,
          sel              TYPE c,
          guid             TYPE c LENGTH 45,        "Holds concatenated REGUT primary key (ZBUKR+BANKS+LAUFD+LAUFI+XVORL+DTKEY+LFDNR = 41 chars) - unique key in place of BATCH_NO
          batch_no         TYPE bnk_com_btch_no,
          rule_id          TYPE bnk_com_btch_rule_id,
          item_cnt         TYPE bnk_com_btch_ctr,
          laufd            TYPE bnk_com_btch_mrge_dat,
          laufi            TYPE bnk_com_btch_mrge_id,
          xvorl            TYPE xvorl,
          laufd_f          TYPE bnk_com_btch_file_dat,
          laufi_f          TYPE bnk_com_btch_file_id,
          sent_flag        TYPE c,
          error_flag       TYPE c,
          rec_flag         TYPE c,
          batch_sum        TYPE bnk_com_btch_amount,
          batch_curr       TYPE bnk_com_btch_curr,
          max_pay_amt      TYPE bnk_com_max_paymnt_amount,
          status           TYPE epic_regut_status,   "REGUT-STATUS(CHAR3) - avoids MOVE-CORRESPONDING type clash
          crusr            TYPE bnk_com_create_user,
          crtime           TYPE bnk_com_create_time,
          crdate           TYPE bnk_com_create_date,
          chusr            TYPE bnk_com_change_user,
          chtime           TYPE bnk_com_change_time,
          chdate           TYPE bnk_com_change_date,
          cur_processor    TYPE bnk_com_cur_processor,
          archive_status   TYPE bnk_com_archive_status,
          zbukr            TYPE dzbukr,
          hbkid            TYPE hbkid,
          tot_btch_amt     TYPE bnk_com_btch_amt_in_rule_curr,
          maxpayamt_rulecu TYPE bnk_com_max_pymntamt_in_rulcur,
          grp_field1_value TYPE bnk_com_grp_fld_val1,
          grp_field2_value TYPE bnk_com_grp_fld_val2,
          raw_data         TYPE xstring,
          file_data_sent   TYPE xstring,
          file_data_received TYPE xstring,
          pending_with     TYPE sy-uname,
          file_name        TYPE zfi_paym_file-file_name ,
          received_filename TYPE zfi_paym_file-received_filename,

        END OF ty_final.

DATA : gt_paym          TYPE STANDARD TABLE OF zfi_paym_file,
       gt_batch_header  TYPE STANDARD TABLE OF regut,   "Replaced BNK_BATCH_HEADER with REGUT
       gt_batch_sign    TYPE STANDARD TABLE OF zfi_batch_sign ,
       gt_final         TYPE STANDARD TABLE OF ty_final,


       gs_paym          TYPE zfi_paym_file,
       gs_batch_header  TYPE regut,   "Replaced BNK_BATCH_HEADER with REGUT
       gs_batch_sign    TYPE zfi_batch_sign ,
       gs_final         TYPE ty_final,

       c_ccont          TYPE REF TO cl_gui_custom_container, "Custom cont
       c_alvgd          TYPE REF TO cl_gui_alv_grid,         "ALV grid
       it_fcat          TYPE lvc_t_fcat,                     " Fieldcat
       it_layout        TYPE lvc_s_layo,                     "Layout
       c_dwnld          TYPE REF TO cl_gui_frontend_services.


DATA: gt_selected_rows TYPE lvc_t_row,                       "Selected Rows
      gs_selected_rows TYPE lvc_s_row.

DATA : gv_laufd     TYPE zfi_paym_file-laufd,
       gv_laufi     TYPE zfi_paym_file-laufi,
       gv_sent_date TYPE zfi_paym_file-sent_date.
