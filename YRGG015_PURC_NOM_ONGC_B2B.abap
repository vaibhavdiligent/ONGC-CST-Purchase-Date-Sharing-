*&---------------------------------------------------------------------*
*& Report  YRGG015_PURC_NOM_ONGC_B2B
*& Purchase Nomination Creation - ONGC B2B
*& T-Code: YRGG015
*&---------------------------------------------------------------------*
REPORT yrgg015_purc_nom_ongc_b2b MESSAGE-ID oo
                                  LINE-SIZE 255
                                  NO STANDARD PAGE HEADING.

INCLUDE yrgg015_purc_nom_ongc_b2b_top.    " Types, Data, Class definitions
INCLUDE yrgg015_purc_nom_ongc_b2b_f01.    " FN logic, Validation, Fetch
INCLUDE yrgg015_purc_nom_ongc_b2b_f02.    " OA derivation, Batch derivation
INCLUDE yrgg015_purc_nom_ongc_b2b_f03.    " ALV display, Event handler
INCLUDE yrgg015_purc_nom_ongc_b2b_f04.    " Create nomination, Batch mass, BG job

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_locid FOR oijnomi-locid NO INTERVALS,
                  s_date  FOR oijnomi-nom_date.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_bgrun AS CHECKBOX MODIF ID bg.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
* Initialization - Default previous fortnight dates
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM set_default_fn_dates.
  TEXT-001 = 'Selection Criteria'.
  TEXT-002 = 'Background Processing'.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT - Control field visibility
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM control_screen_fields.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN - Input validation
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM validate_selection_screen.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM fetch_pur_data.
  IF gt_display IS INITIAL.
    MESSAGE 'No data found for the given selection criteria.' TYPE 'S' DISPLAY LIKE 'W'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_bgrun = abap_true.
    PERFORM schedule_background_job.
  ELSE.
    PERFORM display_alv_grid.
  ENDIF.
