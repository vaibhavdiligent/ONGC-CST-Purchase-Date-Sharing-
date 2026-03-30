*&---------------------------------------------------------------------*
*& Report YRG025_PENDING_IMB_POSTING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT yrg025_pending_imb_posting.
INCLUDE:yrg025_pending_imb_post_sel.
INCLUDE:yrg025_pending_imb_post_val.


START-OF-SELECTION.

*  PERFORM:validation.
  PERFORM:get_data.
*SOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbalance TR:DVRK9A1POQ
  IF p_auto EQ 'X'.
    PERFORM:customers_with_imbalance.
  ENDIF.
*EOC BY Gaurav/Pratibha ON 07.02.2026 Auto posting of fortnightly imbalance TR:DVRK9A1POQ
  PERFORM:comment_build USING gt_list_top_of_page[].
  PERFORM:get_fieldcat.
  PERFORM:alv_display.
