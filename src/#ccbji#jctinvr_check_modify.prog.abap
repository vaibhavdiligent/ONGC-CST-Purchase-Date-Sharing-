*&---------------------------------------------------------------------*
*& Report  /CCBJI/JCTINVR_CHECK_MODIFY
*&---------------------------------------------------------------------*
*& National Tax Agency (NTA) - Qualified Invoice Issuer Registration
*& Number : status check and modify.
*&
*& S/4HANA re-implementation of the legacy Ab Initio / PI graph
*&   05228MD_NationalTaxAgency_C1_InvoiceNumber_CheckModify
*& which read the DB2 table "SAPCOK"//CCBJI/T_JCTINVR, decided the
*& delta indicator from the revocation / expiration dates against the
*& process date and wrote the row back through PI.
*&
*& In S/4HANA the table /CCBJI/T_JCTINVR is a local transparent table,
*& so the whole graph collapses into one report that reads and updates
*& the table directly - no PI, no DB2, no file.
*&
*& Logic (per invoice registration number, MANDT + INVOICE_CD = key)
*& ----------------------------------------------------------------
*&   A registration is "no longer valid" on the process date when
*&     REVOCATION_DATE <> 00000000 AND REVOCATION_DATE <= p_date, or
*&     EXPIRATION_DATE <> 00000000 AND EXPIRATION_DATE <= p_date.
*&
*&   1) no longer valid  AND ZUPDIND <> 'D'          -> set ZUPDIND = 'D'
*&      (legacy branch "Mark as Deleted")
*&   2) still valid      AND ZUPDIND  = 'D'
*&      AND at least one of the two dates is filled  -> set ZUPDIND = 'U'
*&      (legacy branch "Mark as Updated/Active"; a row flagged 'D'
*&       without any date was deleted by another process and is left
*&       untouched)
*&
*&   For every changed row the delta/audit fields are refreshed exactly
*&   like the legacy reformat component did:
*&     ZUPDIND = 'D' / 'U'
*&     ZAENAM  = changed-by  (legacy constant 'AbInitio', now the job
*&                            user - overridable on the selection screen)
*&     ZUPDAT  = system date
*&     ZUPTIM  = system time
*&   MANDT is not set explicitly (the legacy graph forced 100); Open SQL
*&   works in the logon client of the job.
*&
*& Scheduling : daily background job, legacy slot 10:00 JST.
*&---------------------------------------------------------------------*
REPORT /ccbji/jctinvr_check_modify.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_cand,
         invoice_cd      TYPE /ccbji/t_jctinvr-invoice_cd,
         revocation_date TYPE /ccbji/t_jctinvr-revocation_date,
         expiration_date TYPE /ccbji/t_jctinvr-expiration_date,
         zupdind         TYPE /ccbji/t_jctinvr-zupdind,
       END OF ty_cand.

TYPES: BEGIN OF ty_log,
         invoice_cd      TYPE /ccbji/t_jctinvr-invoice_cd,
         old_ind         TYPE /ccbji/t_jctinvr-zupdind,
         new_ind         TYPE /ccbji/t_jctinvr-zupdind,
         revocation_date TYPE /ccbji/t_jctinvr-revocation_date,
         expiration_date TYPE /ccbji/t_jctinvr-expiration_date,
         action          TYPE c LENGTH 25,
         status          TYPE c LENGTH 40,
       END OF ty_log.

*----------------------------------------------------------------------*
* CONSTANTS
*----------------------------------------------------------------------*
CONSTANTS: gc_ind_delete TYPE /ccbji/t_jctinvr-zupdind VALUE 'D',
           gc_ind_update TYPE /ccbji/t_jctinvr-zupdind VALUE 'U',
           gc_date_init  TYPE /ccbji/t_jctinvr-revocation_date VALUE '00000000'.

*----------------------------------------------------------------------*
* GLOBAL DATA
*----------------------------------------------------------------------*
DATA: gs_invr TYPE /ccbji/t_jctinvr.

DATA: gt_cand TYPE STANDARD TABLE OF ty_cand,
      gt_log  TYPE STANDARD TABLE OF ty_log,
      go_alv  TYPE REF TO cl_salv_table.

DATA: gv_read     TYPE i,
      gv_mark_del TYPE i,
      gv_mark_upd TYPE i,
      gv_updated  TYPE i,
      gv_error    TYPE i.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  PARAMETERS     p_date  TYPE sy-datum OBLIGATORY DEFAULT sy-datum.
  SELECT-OPTIONS s_invcd FOR gs_invr-invoice_cd.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-b02.
  PARAMETERS: p_chgby TYPE /ccbji/t_jctinvr-zaenam DEFAULT sy-uname,
              p_test  AS CHECKBOX DEFAULT 'X',
              p_cmtsz TYPE i DEFAULT 5000,
              p_alv   AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b02.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  IF p_cmtsz <= 0.
    MESSAGE 'Commit size must be greater than zero' TYPE 'E'.
  ENDIF.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM read_candidates.
  PERFORM classify_records.
  PERFORM update_database.

END-OF-SELECTION.

  PERFORM display_result.

*&---------------------------------------------------------------------*
*&      Form  READ_CANDIDATES
*&---------------------------------------------------------------------*
*&  Only rows carrying a revocation or an expiration date can ever
*&  change their status, so the rest of the table is never transferred.
*&---------------------------------------------------------------------*
FORM read_candidates.

  SELECT invoice_cd,
         revocation_date,
         expiration_date,
         zupdind
    FROM /ccbji/t_jctinvr
   WHERE invoice_cd IN @s_invcd
     AND ( revocation_date <> @gc_date_init
        OR expiration_date <> @gc_date_init )
    INTO TABLE @gt_cand.

  gv_read = lines( gt_cand ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CLASSIFY_RECORDS
*&---------------------------------------------------------------------*
*&  Legacy branches "Mark as Deleted (D)" and "Mark as Updated (U)".
*&---------------------------------------------------------------------*
FORM classify_records.

  DATA: lv_invalid TYPE abap_bool,
        ls_log     TYPE ty_log.

  LOOP AT gt_cand ASSIGNING FIELD-SYMBOL(<ls_cand>).

    CLEAR: lv_invalid, ls_log.

*   Revocation or expiration reached on / before the process date
    IF ( <ls_cand>-revocation_date <> gc_date_init
     AND <ls_cand>-revocation_date <= p_date )
    OR ( <ls_cand>-expiration_date <> gc_date_init
     AND <ls_cand>-expiration_date <= p_date ).
      lv_invalid = abap_true.
    ENDIF.

    IF lv_invalid = abap_true.
*     Branch 1 - registration number is no longer valid
      IF <ls_cand>-zupdind = gc_ind_delete.
        CONTINUE.                     "already flagged as deleted
      ENDIF.
      ls_log-new_ind = gc_ind_delete.
      ls_log-action  = 'Mark as deleted'.
      gv_mark_del    = gv_mark_del + 1.
    ELSE.
*     Branch 2 - registration number is (again) valid
      IF <ls_cand>-zupdind <> gc_ind_delete.
        CONTINUE.                     "nothing to correct
      ENDIF.
      ls_log-new_ind = gc_ind_update.
      ls_log-action  = 'Mark as updated/active'.
      gv_mark_upd    = gv_mark_upd + 1.
    ENDIF.

    ls_log-invoice_cd      = <ls_cand>-invoice_cd.
    ls_log-old_ind         = <ls_cand>-zupdind.
    ls_log-revocation_date = <ls_cand>-revocation_date.
    ls_log-expiration_date = <ls_cand>-expiration_date.
    APPEND ls_log TO gt_log.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  UPDATE_DATABASE
*&---------------------------------------------------------------------*
*&  Legacy "Update DB2 target" component: key MANDT + INVOICE_CD,
*&  only ZUPDIND, ZAENAM, ZUPDAT and ZUPTIM are written.
*&---------------------------------------------------------------------*
FORM update_database.

  DATA: lv_since_commit TYPE i,
        lv_updat        TYPE /ccbji/t_jctinvr-zupdat,
        lv_uptim        TYPE /ccbji/t_jctinvr-zuptim.

  IF gt_log IS INITIAL.
    RETURN.
  ENDIF.

  lv_updat = sy-datum.
  lv_uptim = sy-uzeit.

  LOOP AT gt_log ASSIGNING FIELD-SYMBOL(<ls_log>).

    IF p_test = abap_true.
      <ls_log>-status = 'Test run - no database update'.
      CONTINUE.
    ENDIF.

    UPDATE /ccbji/t_jctinvr
       SET zupdind = @<ls_log>-new_ind,
           zaenam  = @p_chgby,
           zupdat  = @lv_updat,
           zuptim  = @lv_uptim
     WHERE invoice_cd = @<ls_log>-invoice_cd.

    IF sy-subrc = 0.
      <ls_log>-status = 'Updated'.
      gv_updated      = gv_updated + 1.
    ELSE.
      <ls_log>-status = 'Update failed - record not found'.
      gv_error        = gv_error + 1.
    ENDIF.

    lv_since_commit = lv_since_commit + 1.
    IF lv_since_commit >= p_cmtsz.
      COMMIT WORK AND WAIT.
      CLEAR lv_since_commit.
    ENDIF.

  ENDLOOP.

  IF p_test = abap_false.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULT
*&---------------------------------------------------------------------*
*&  ALV in dialog, classic list (spool) in background.
*&---------------------------------------------------------------------*
FORM display_result.

  IF p_alv = abap_true AND sy-batch IS INITIAL.
    PERFORM display_alv.
  ELSE.
    PERFORM write_summary.
    PERFORM write_detail.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  WRITE_SUMMARY
*&---------------------------------------------------------------------*
FORM write_summary.

  WRITE: / 'NTA invoice registration number - check and modify'.
  ULINE.
  WRITE: / 'Process date                 :', p_date.
  WRITE: / 'Changed by (ZAENAM)          :', p_chgby.
  IF p_test = abap_true.
    WRITE: / 'Mode                         : Test run (no update)'.
  ELSE.
    WRITE: / 'Mode                         : Update run'.
  ENDIF.
  ULINE.
  WRITE: / 'Records read (dated entries) :', gv_read     LEFT-JUSTIFIED.
  WRITE: / 'Flagged as deleted       (D) :', gv_mark_del LEFT-JUSTIFIED.
  WRITE: / 'Flagged as updated       (U) :', gv_mark_upd LEFT-JUSTIFIED.
  WRITE: / 'Rows updated on database     :', gv_updated  LEFT-JUSTIFIED.
  WRITE: / 'Rows in error                :', gv_error    LEFT-JUSTIFIED.
  ULINE.

  IF gt_log IS INITIAL.
    WRITE: / 'No invoice registration number required a status change'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  WRITE_DETAIL
*&---------------------------------------------------------------------*
*&  Plain list for background execution (spool).
*&---------------------------------------------------------------------*
FORM write_detail.

  IF gt_log IS INITIAL.
    RETURN.
  ENDIF.

  SKIP.
  WRITE: /  'Invoice number', 20 'Old', 26 'New', 32 'Revocation',
         45 'Expiration', 58 'Action', 86 'Status'.
  ULINE.

  LOOP AT gt_log ASSIGNING FIELD-SYMBOL(<ls_out>).
    WRITE: /  <ls_out>-invoice_cd,
           20 <ls_out>-old_ind,
           26 <ls_out>-new_ind,
           32 <ls_out>-revocation_date,
           45 <ls_out>-expiration_date,
           58 <ls_out>-action,
           86 <ls_out>-status.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM display_alv.

  DATA: lo_columns TYPE REF TO cl_salv_columns_table,
        lo_column  TYPE REF TO cl_salv_column_table,
        lo_exc     TYPE REF TO cx_root,
        lv_title   TYPE lvc_title,
        lv_msg     TYPE string.

  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = go_alv
        CHANGING  t_table      = gt_log ).

      go_alv->get_functions( )->set_all( abap_true ).

      lv_title = 'Invoice registration number - status changes'.
      go_alv->get_display_settings( )->set_list_header( lv_title ).
      go_alv->get_display_settings( )->set_striped_pattern( abap_true ).

      PERFORM build_header CHANGING go_alv.

      lo_columns = go_alv->get_columns( ).
      lo_columns->set_optimize( abap_true ).

      lo_column ?= lo_columns->get_column( 'INVOICE_CD' ).
      lo_column->set_short_text( 'Invoice' ).
      lo_column->set_medium_text( 'Invoice number' ).
      lo_column->set_long_text( 'Invoice registration number' ).

      lo_column ?= lo_columns->get_column( 'OLD_IND' ).
      lo_column->set_short_text( 'Old ind.' ).
      lo_column->set_medium_text( 'Old indicator' ).
      lo_column->set_long_text( 'Update indicator before' ).

      lo_column ?= lo_columns->get_column( 'NEW_IND' ).
      lo_column->set_short_text( 'New ind.' ).
      lo_column->set_medium_text( 'New indicator' ).
      lo_column->set_long_text( 'Update indicator after' ).

      lo_column ?= lo_columns->get_column( 'REVOCATION_DATE' ).
      lo_column->set_short_text( 'Revoc.' ).
      lo_column->set_medium_text( 'Revocation date' ).
      lo_column->set_long_text( 'Revocation date' ).

      lo_column ?= lo_columns->get_column( 'EXPIRATION_DATE' ).
      lo_column->set_short_text( 'Expiry' ).
      lo_column->set_medium_text( 'Expiration date' ).
      lo_column->set_long_text( 'Expiration date' ).

      lo_column ?= lo_columns->get_column( 'ACTION' ).
      lo_column->set_short_text( 'Action' ).
      lo_column->set_medium_text( 'Action' ).
      lo_column->set_long_text( 'Action taken' ).

      lo_column ?= lo_columns->get_column( 'STATUS' ).
      lo_column->set_short_text( 'Status' ).
      lo_column->set_medium_text( 'Status' ).
      lo_column->set_long_text( 'Status' ).

      go_alv->display( ).

    CATCH cx_salv_msg cx_salv_not_found cx_salv_existing INTO lo_exc.
      lv_msg = lo_exc->get_text( ).
      MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'W'.
      PERFORM write_summary.
      PERFORM write_detail.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BUILD_HEADER
*&---------------------------------------------------------------------*
*&  Run parameters and counters as ALV top-of-page.
*&---------------------------------------------------------------------*
FORM build_header CHANGING co_alv TYPE REF TO cl_salv_table.

  DATA: lo_grid TYPE REF TO cl_salv_form_layout_grid,
        lv_text TYPE string,
        lv_num  TYPE string.

  CREATE OBJECT lo_grid.

  lv_text = |{ p_date DATE = USER }|.
  lo_grid->create_label( row = 1 column = 1 text = 'Process date' ).
  lo_grid->create_text(  row = 1 column = 2 text = lv_text ).

  IF p_test = abap_true.
    lv_text = 'Test run - no database update'.
  ELSE.
    lv_text = 'Update run'.
  ENDIF.
  lo_grid->create_label( row = 1 column = 3 text = 'Mode' ).
  lo_grid->create_text(  row = 1 column = 4 text = lv_text ).

  lv_num = |{ gv_read }|.
  lo_grid->create_label( row = 2 column = 1 text = 'Records read' ).
  lo_grid->create_text(  row = 2 column = 2 text = lv_num ).

  lv_num = |{ gv_mark_del } / { gv_mark_upd }|.
  lo_grid->create_label( row = 2 column = 3 text = 'Deleted (D) / Updated (U)' ).
  lo_grid->create_text(  row = 2 column = 4 text = lv_num ).

  lv_num = |{ gv_updated }|.
  lo_grid->create_label( row = 3 column = 1 text = 'Rows updated' ).
  lo_grid->create_text(  row = 3 column = 2 text = lv_num ).

  lv_num = |{ gv_error }|.
  lo_grid->create_label( row = 3 column = 3 text = 'Rows in error' ).
  lo_grid->create_text(  row = 3 column = 4 text = lv_num ).

  co_alv->set_top_of_list( lo_grid ).

ENDFORM.
