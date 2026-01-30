*&---------------------------------------------------------------------*
*& Include YGMS_CST_PURCHASE_TOP
*& Description: Global Data Declarations
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Type Pools
*----------------------------------------------------------------------*
TYPE-POOLS: icon.

*----------------------------------------------------------------------*
* Tables
*----------------------------------------------------------------------*
TABLES: ygms_cst_pur.

*----------------------------------------------------------------------*
* Global Data Objects
*----------------------------------------------------------------------*
DATA: go_controller   TYPE REF TO ygms_cl_cst_controller,
      go_alv_handler  TYPE REF TO ygms_cl_cst_alv_handler,
      gt_messages     TYPE bapiret2_t,
      gt_allocation   TYPE ygms_tt_allocation,
      gt_purchase     TYPE ygms_tt_purchase,
      gv_gail_id      TYPE ygms_gail_id.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*
CONSTANTS: gc_mode_upload     TYPE char1 VALUE 'U',
           gc_mode_allocate   TYPE char1 VALUE 'A',
           gc_mode_view       TYPE char1 VALUE 'V',
           gc_mode_send       TYPE char1 VALUE 'S',
           gc_mode_download   TYPE char1 VALUE 'D',
           gc_mode_nomination TYPE char1 VALUE 'N'.
