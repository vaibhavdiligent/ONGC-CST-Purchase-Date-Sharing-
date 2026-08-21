*&---------------------------------------------------------------------*
*& Include ZFI_BNK_APP1_TOP                                  Module Pool      ZFI_BNK_APP1
*&
*&---------------------------------------------------------------------*

PROGRAM ZFI_BNK_APP1.

TYPES : BEGIN OF ty_final,
          sel              type c,
          guid             TYPE c LENGTH 45,        "Holds concatenated REGUT primary key (ZBUKR+BANKS+LAUFD+LAUFI+XVORL+DTKEY+LFDNR = 41 chars) - unique key in place of BATCH_NO
          batch_no         TYPE bnk_com_btch_no,
          rule_id          TYPE bnk_com_btch_rule_id,
          item_cnt         TYPE bnk_com_btch_ctr,
          laufd            TYPE bnk_com_btch_mrge_dat,
          laufi            TYPE bnk_com_btch_mrge_id,
          xvorl            TYPE xvorl,
          laufd_f          TYPE bnk_com_btch_file_dat,
          laufi_f          TYPE bnk_com_btch_file_id,
          Error_flag       type c,
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
          raw_data         TYPE XSTRING,
          file_data_sent   TYPE XSTRING,

        end of ty_final.

DATA : gt_paym          TYPE STANDARD TABLE OF zfi_paym_file,
       gt_batch_header  TYPE STANDARD TABLE OF regut,   "Replaced BNK_BATCH_HEADER with REGUT
       gt_batch_sign    TYPE STANDARD TABLE OF zfi_batch_sign ,
       gt_paym2         TYPE STANDARD TABLE OF zfi_paym_file,
       gt_batch_header2 TYPE STANDARD TABLE OF regut,   "Replaced BNK_BATCH_HEADER with REGUT
       gt_batch_sign2   TYPE STANDARD TABLE OF zfi_batch_sign ,
       gt_paym3         TYPE STANDARD TABLE OF zfi_paym_file,
       gt_batch_header3 TYPE STANDARD TABLE OF regut,   "Replaced BNK_BATCH_HEADER with REGUT
       gt_batch_sign3   TYPE STANDARD TABLE OF zfi_batch_sign ,
       gt_final         TYPE STANDARD TABLE OF ty_final,
       gt_final2        TYPE STANDARD TABLE OF ty_final,
       gt_final3        type STANDARD TABLE OF ty_final,
       gt_reguhm        TYPE STANDARD TABLE OF reguhm.    "for BATCHNO exclusion check

DATA: gs_paym          LIKE LINE OF gt_paym,
      gs_batch_header  LIKE LINE OF gt_batch_header,
      gs_batch_sign    LIKE LINE OF gt_batch_sign,
      gs_final         LIKE LINE OF gt_final,
      gs_paym2         LIKE LINE OF gt_paym2,
      gs_batch_header2 LIKE LINE OF gt_batch_header2,
      gs_batch_sign2   LIKE LINE OF gt_batch_sign2,
      gs_final2        LIKE LINE OF gt_final2,
      gs_paym3         LIKE LINE OF gt_paym3,
      gs_batch_header3 LIKE LINE OF gt_batch_header3,
      gs_batch_sign3   LIKE LINE OF gt_batch_sign3,
      gs_final3        type ty_final.


*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TABSTRIP'
CONSTANTS: BEGIN OF C_TABSTRIP,
             TAB1 LIKE SY-UCOMM VALUE 'TABSTRIP_FC1',
             TAB2 LIKE SY-UCOMM VALUE 'TABSTRIP_FC2',
             TAB3 LIKE SY-UCOMM VALUE 'TABSTRIP_FC3',
           END OF C_TABSTRIP.
*&SPWIZARD: DATA FOR TABSTRIP 'TABSTRIP'
CONTROLS:  TABSTRIP TYPE TABSTRIP.
DATA:      BEGIN OF G_TABSTRIP,
             SUBSCREEN   LIKE SY-DYNNR,
             PROG        LIKE SY-REPID VALUE 'ZFI_BNK_APP1',
             PRESSED_TAB LIKE SY-UCOMM VALUE C_TABSTRIP-TAB1,
           END OF G_TABSTRIP.
DATA:      OK_CODE LIKE SY-UCOMM.

DATA: c_ccont1   TYPE REF TO cl_gui_custom_container,  "Custom cont
      c_alvgd1   TYPE REF TO cl_gui_alv_grid,          "ALV grid
      it_fcat1   TYPE lvc_t_fcat,                      " Fieldcat
      it_layout1 TYPE lvc_s_layo.                      "Layout

DATA: c_ccont2   TYPE REF TO cl_gui_custom_container,  "Custom cont
      c_alvgd2   TYPE REF TO cl_gui_alv_grid,          "ALV grid
      it_fcat2   TYPE lvc_t_fcat,                      " Fieldcat
      it_layout2 TYPE lvc_s_layo.

DATA: c_ccont3   TYPE REF TO cl_gui_custom_container,  "Custom cont
      c_alvgd3   TYPE REF TO cl_gui_alv_grid,          "ALV grid
      it_fcat3   TYPE lvc_t_fcat,                      " Fieldcat
      it_layout3 TYPE lvc_s_layo.

data: gt_selected_rows1 type lvc_t_row,"Selected Rows
      gs_selected_rows1 type lvc_s_row.

data: gt_selected_rows2 type lvc_t_row,"Selected Rows
      gs_selected_rows2 type lvc_s_row.

data: gt_selected_rows3 type lvc_t_row,"Selected Rows
      gs_selected_rows3 type lvc_s_row.
