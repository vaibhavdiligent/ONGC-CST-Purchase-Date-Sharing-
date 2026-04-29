*&---------------------------------------------------------------------*
*& Include YRGG015_PURC_NOM_ONGC_B2B_F04
*& Create Nomination (SUBMIT YRXR036_PURC_NOM_G1), Batch Mass Change,
*& Background Job Scheduling
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& FORM handle_create_nomination
*& Builds i_main from selected rows, exports to memory, submits YRGR040
*&---------------------------------------------------------------------*
FORM handle_create_nomination.
  DATA: lt_selected TYPE tt_display,
        ls_disp     TYPE ty_display,
        ls_main     TYPE ty_main,
        lt_main     TYPE tt_main,
        lt_errors   TYPE tt_main.

  " Collect selected rows
  LOOP AT gt_display INTO ls_disp WHERE sel = abap_true.
    APPEND ls_disp TO lt_selected.
  ENDLOOP.

  IF lt_selected IS INITIAL.
    MESSAGE 'Please select at least one row to create nomination.' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  " Validate: all selected rows must have an Outline Agreement
  LOOP AT lt_selected INTO ls_disp.
    IF ls_disp-outline_agr IS INITIAL.
      MESSAGE 'One or more selected rows have no Outline Agreement. Cannot create nomination.' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
    IF ls_disp-charg IS INITIAL.
      MESSAGE |Batch is missing for material { ls_disp-material } at location { ls_disp-location_id }. Please assign batch before creating nomination.| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDLOOP.

  " Build i_main in the format expected by YRXR036_PURC_NOM_G1
  " Sequence: TSYST(blank), OA(vbeln), Date, LocID, Material, Qty, UoM=SM3, Batch, Rank=1
  DATA: lv_rank TYPE i VALUE 1.
  LOOP AT lt_selected INTO ls_disp.
    CLEAR ls_main.
    ls_main-tsyst = ''.             " Transport system - filled by BATCH_VALIDATE in YRGR040
    ls_main-vbeln = ls_disp-outline_agr.
    ls_main-date  = ls_disp-gas_day.
    ls_main-locid = ls_disp-location_id.
    ls_main-matnr = ls_disp-material.
    ls_main-menge = ls_disp-qty_scm.
    ls_main-unit  = gc_sm3.
    ls_main-charg = ls_disp-charg.
    ls_main-rank  = lv_rank.
    APPEND ls_main TO lt_main.
    lv_rank = lv_rank + 1.
  ENDLOOP.

  IF lt_main IS INITIAL.
    MESSAGE 'No valid data to submit for nomination creation.' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  " Export data to memory for YRXR036_PURC_NOM_G1 to import
  EXPORT lt_main TO MEMORY ID gc_memory_id.

  " Submit YRGR040 (YRXR036_PURC_NOM_G1) with first radio button (create nomination)
  " The program reads memory, runs BATCH_VALIDATE then CREATEFROMDATA, then exports errors
  SUBMIT yrxr036_purc_nom_g1
    WITH r_create = abap_true    " First radio button = Create Nomination
    AND RETURN.

  " Import any errors returned by YRGR040
  IMPORT lt_errors FROM MEMORY ID gc_err_mem_id.

  " Clear memory
  FREE MEMORY ID gc_memory_id.
  FREE MEMORY ID gc_err_mem_id.

  IF lt_errors IS NOT INITIAL.
    " Show errors inline in a separate ALV and return to input screen
    PERFORM display_nomination_errors USING lt_errors.
  ELSE.
    MESSAGE 'Nominations created successfully.' TYPE 'S'.
    " Refresh the ALV grid
    go_alv->refresh_table_display( ).
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM display_nomination_errors
*& Displays YRGR040 errors in an ALV popup, then returns to input screen
*&---------------------------------------------------------------------*
FORM display_nomination_errors USING it_errors TYPE tt_main.
  DATA: lo_popup    TYPE REF TO cl_gui_dialogbox_container,
        lo_alv_err  TYPE REF TO cl_gui_alv_grid,
        lt_fcat     TYPE lvc_t_fcat,
        ls_fcat     TYPE lvc_s_fcat,
        ls_layout   TYPE lvc_s_layo.

  " Build simple fieldcat for error display
  ls_fcat-fieldname = 'LOCID'.  ls_fcat-coltext = 'Location'. APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  ls_fcat-fieldname = 'MATNR'.  ls_fcat-coltext = 'Material'. APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  ls_fcat-fieldname = 'DATE'.   ls_fcat-coltext = 'Date'.     APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  ls_fcat-fieldname = 'VBELN'.  ls_fcat-coltext = 'OA'.       APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.
  ls_fcat-fieldname = 'MSG'.    ls_fcat-coltext = 'Message'.  ls_fcat-outputlen = 80. APPEND ls_fcat TO lt_fcat. CLEAR ls_fcat.

  ls_layout-cwidth_opt = abap_true.

  " Create popup container
  CREATE OBJECT lo_popup
    EXPORTING
      caption    = 'Nomination Errors'
      top        = 10
      left       = 10
      width      = 500
      height     = 300
    EXCEPTIONS
      OTHERS     = 1.

  IF sy-subrc <> 0.
    " Fallback: show as message list
    LOOP AT it_errors INTO DATA(ls_err) WHERE msgty = 'E' OR msgty = 'A'.
      MESSAGE ls_err-msg TYPE 'S' DISPLAY LIKE 'E'.
    ENDLOOP.
    RETURN.
  ENDIF.

  CREATE OBJECT lo_alv_err
    EXPORTING
      i_parent = lo_popup
    EXCEPTIONS
      OTHERS   = 1.

  lo_alv_err->set_table_for_first_display(
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_outtab       = it_errors
      it_fieldcatalog = lt_fcat
    EXCEPTIONS
      OTHERS          = 1 ).

  " Leave the popup open until user closes; return to selection screen
  MESSAGE 'Nomination creation had errors. See error log above.' TYPE 'S' DISPLAY LIKE 'W'.
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM handle_batch_mass_change
*& Popup: list batch-managed materials in selected rows,
*&        allow user to pick one batch for each material → apply to all rows
*&---------------------------------------------------------------------*
FORM handle_batch_mass_change.
  DATA: ls_disp   TYPE ty_display,
        lt_sel    TYPE tt_display,
        lt_matnrs TYPE STANDARD TABLE OF matnr,
        lv_matnr  TYPE matnr,
        lv_charg  TYPE charg_d,
        lt_batches TYPE STANDARD TABLE OF ty_batch_vals,
        ls_batch  TYPE ty_batch_vals,
        lt_f4     TYPE STANDARD TABLE OF ddshretval,
        ls_f4     TYPE ddshretval.

  " Collect selected rows
  LOOP AT gt_display INTO ls_disp WHERE sel = abap_true.
    APPEND ls_disp TO lt_sel.
  ENDLOOP.

  IF lt_sel IS INITIAL.
    MESSAGE 'Select rows first to use Batch Change in Mass.' TYPE 'S' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  " Collect distinct materials from selected rows
  LOOP AT lt_sel INTO ls_disp.
    READ TABLE lt_matnrs WITH KEY table_line = ls_disp-material TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND ls_disp-material TO lt_matnrs.
    ENDIF.
  ENDLOOP.

  " For each unique material, popup an F4 to pick batch then apply to all matching rows
  LOOP AT lt_matnrs INTO lv_matnr.
    REFRESH lt_batches.
    PERFORM get_valid_batches_for_material
      USING    lv_matnr
      CHANGING lt_batches.

    IF lt_batches IS INITIAL.
      CONTINUE.
    ENDIF.

    " Build value list
    REFRESH lt_f4.
    LOOP AT lt_batches INTO ls_batch.
      ls_f4-fieldname = 'CHARG'.
      ls_f4-fieldval  = ls_batch-charg.
      APPEND ls_f4 TO lt_f4.
      CLEAR ls_f4.
    ENDLOOP.

    CLEAR lv_charg.
    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING
        endpos_col  = 60
        endpos_row  = 15
        startpos_col = 5
        startpos_row = 5
        titletext   = |Select Batch for Material { lv_matnr }|
      IMPORTING
        RETURNCODE  = DATA(lv_rc)
      TABLES
        valuetab    = lt_f4
      EXCEPTIONS
        OTHERS      = 1.

    IF sy-subrc = 0 AND lv_rc = ''.
      READ TABLE lt_f4 INDEX 1 INTO ls_f4.
      IF sy-subrc = 0.
        lv_charg = ls_f4-fieldval.
      ENDIF.
    ENDIF.

    IF lv_charg IS INITIAL.
      CONTINUE.
    ENDIF.

    " Apply selected batch to all selected rows of this material
    LOOP AT gt_display INTO ls_disp WHERE sel = abap_true AND material = lv_matnr.
      ls_disp-charg = lv_charg.
      MODIFY gt_display FROM ls_disp.
    ENDLOOP.
  ENDLOOP.

  " Refresh grid
  go_alv->refresh_table_display( ).
ENDFORM.

*&---------------------------------------------------------------------*
*& FORM schedule_background_job
*& Schedules YRGG015 as a background job (ZC_GMS_CORE_TEAM only)
*&---------------------------------------------------------------------*
FORM schedule_background_job.
  DATA: lv_jobname  TYPE tbtcjob-jobname,
        lv_jobcount TYPE tbtcjob-jobcount,
        ls_submit   TYPE btcssubmit.

  lv_jobname = 'YRGG015_PURC_NOM_ONGC_B2B'.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = lv_jobname
    IMPORTING
      jobcount         = lv_jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
    MESSAGE 'Error creating background job.' TYPE 'E'.
    RETURN.
  ENDIF.

  " Submit this report as the job step
  SUBMIT yrgg015_purc_nom_ongc_b2b
    WITH s_date  IN s_date
    WITH s_locid IN s_locid
    VIA JOB lv_jobname NUMBER lv_jobcount
    AND RETURN.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount             = lv_jobcount
      jobname              = lv_jobname
      strtimmed            = abap_true      " Start immediately
    EXCEPTIONS
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      job_nosteps          = 5
      OTHERS               = 6.

  IF sy-subrc = 0.
    MESSAGE |Background job { lv_jobname } scheduled successfully.| TYPE 'S'.
  ELSE.
    MESSAGE 'Error scheduling background job.' TYPE 'E'.
  ENDIF.
ENDFORM.
