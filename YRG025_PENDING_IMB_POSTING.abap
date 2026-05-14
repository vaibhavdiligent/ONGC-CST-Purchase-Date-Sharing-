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
*SOC Email Pending Postings
  IF p_email EQ 'X'.
    PERFORM:email_pending_postings.
    MESSAGE 'Email sent successfully.' TYPE 'S'.
  ELSE.
    PERFORM:comment_build USING gt_list_top_of_page[].
    PERFORM:get_fieldcat.
    PERFORM:alv_display.
  ENDIF.
*EOC Email Pending Postings
