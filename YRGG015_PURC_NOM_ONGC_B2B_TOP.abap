*&---------------------------------------------------------------------*
*& Include YRGG015_PURC_NOM_ONGC_B2B_TOP
*& Types, Data Declarations, Class Definitions
*&---------------------------------------------------------------------*

TABLES: oijnomi.

*----------------------------------------------------------------------*
* Type Definitions
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_pur,
         gas_day     TYPE aedat,
         location_id TYPE char10,
         material    TYPE matnr,
         state_code  TYPE char2,
         qty_scm     TYPE p LENGTH 13 DECIMALS 3,
         gail_id     TYPE char20,
         deleted     TYPE char1,
       END OF ty_pur.

TYPES: BEGIN OF ty_main,   " Matches YRXR036_PURC_NOM_G1 ty_main
         tsyst  TYPE char4,
         vbeln  TYPE ebeln,    " Outline Agreement
         date   TYPE aedat,
         locid  TYPE oij_locid,
         matnr  TYPE matnr,
         menge  TYPE p LENGTH 13 DECIMALS 3,
         unit   TYPE meins,
         charg  TYPE charg_d,
         rank   TYPE i,
         ancv   TYPE char10,
         agcv   TYPE char10,
         nomtk  TYPE char20,
         nomit  TYPE char10,
         msg    TYPE char220,
         msgty  TYPE msgty,
       END OF ty_main.

TYPES: BEGIN OF ty_display,
         sel        TYPE char1,           " Checkbox
         gas_day    TYPE aedat,
         location_id TYPE char10,
         material   TYPE matnr,
         state_code TYPE char2,
         qty_scm    TYPE p LENGTH 13 DECIMALS 3,
         gail_id    TYPE char20,
         outline_agr TYPE ebeln,          " Outline Agreement (OA)
         charg      TYPE charg_d,         " Batch
         oa_missing TYPE char1,           " Flag: OA not found
         celltab    TYPE lvc_t_styl,      " Cell style for editability
         rowcolor   TYPE lvc_fname,       " Row color field name placeholder
         t_color    TYPE lvc_t_scol,      " Cell color table
       END OF ty_display.

TYPES: BEGIN OF ty_loc_map,
         location_id TYPE char10,
       END OF ty_loc_map.

TYPES: BEGIN OF ty_batch_vals,
         charg   TYPE charg_d,
         matnr   TYPE matnr,
         werks   TYPE werks_d,
         ersda   TYPE ersda,
       END OF ty_batch_vals.

TYPES: BEGIN OF ty_oa,
         vbeln   TYPE ebeln,
         matnr   TYPE matnr,
         location_id TYPE char10,
         bedat   TYPE bedat,
       END OF ty_oa.

TYPES: tt_main    TYPE STANDARD TABLE OF ty_main.
TYPES: tt_display TYPE STANDARD TABLE OF ty_display.

*----------------------------------------------------------------------*
* Global Variables
*----------------------------------------------------------------------*
DATA: gt_pur       TYPE STANDARD TABLE OF ty_pur,
      gt_display   TYPE tt_display,
      gt_main      TYPE tt_main,
      go_alv       TYPE REF TO cl_gui_alv_grid,
      go_container TYPE REF TO cl_gui_custom_container,
      gs_layout    TYPE lvc_s_layo,
      gt_fcat      TYPE lvc_t_fcat,
      gv_okcode    TYPE syucomm,
      gv_repid     TYPE syrepid,
      gv_auth_bg   TYPE char1.             " Authority for background

CONSTANTS: gc_memory_id  TYPE char30  VALUE 'YRGG015_NOM_DATA',
           gc_err_mem_id TYPE char30  VALUE 'YRGG015_NOM_ERRORS',
           gc_role_core  TYPE string  VALUE 'ZC_GMS_CORE_TEAM',
           gc_excl_state TYPE char2   VALUE 'GJ',
           gc_deleted    TYPE char1   VALUE 'X',
           gc_sm3        TYPE meins   VALUE 'SM3'.

*----------------------------------------------------------------------*
* Event Handler Class Definition
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      on_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed,

      on_hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,

      on_onf4
        FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_rowno es_row_no e_ucomm
                  er_event_data.
ENDCLASS.

DATA: go_handler TYPE REF TO lcl_event_handler.

*----------------------------------------------------------------------*
* Class Implementation (in F03)
*----------------------------------------------------------------------*
