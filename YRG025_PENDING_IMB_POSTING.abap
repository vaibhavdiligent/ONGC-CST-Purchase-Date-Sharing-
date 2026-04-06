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
  PERFORM:comment_build USING gt_list_top_of_page[].
  PERFORM:get_fieldcat.
  PERFORM:alv_display.
