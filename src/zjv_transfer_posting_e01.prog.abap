*&---------------------------------------------------------------------*
*&  Include           ZJV_TRANSFER_POSTING_E01
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Events Include
*----------------------------------------------------------------------*


START-OF-SELECTION.

  PERFORM get_data.

  IF gt_jvso1 IS INITIAL.
    MESSAGE 'No data found' TYPE 'I'.
    EXIT.
  ENDIF.

  PERFORM process_data.

  IF p_test = 'X'.
    PERFORM display_output.
  ELSE.
    PERFORM post_document.
  ENDIF.
