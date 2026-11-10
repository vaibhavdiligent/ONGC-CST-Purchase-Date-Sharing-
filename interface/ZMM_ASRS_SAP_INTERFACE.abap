*&                                                               *
*& Report   ZMM_ASRS_SAP_INTERFACE
*&
*&                                                               *
*&
*&
*&                                                               *

REPORT zmm_asrs_sap_interface.


TABLES: zmm_asrs.

TYPES: BEGIN OF ty_asrs,
        msg_src TYPE zmsg_src,
        msg_rec_id TYPE zmsg_rec_id,
        msg_trans_type TYPE zmsg_trans_type,
        msg_action TYPE zmsg_action,
        msg_ret_src TYPE zmsg_ret_src,
        msg_ret_rec_id TYPE zmsg_ret_rec_id,
        msg_ret_trans_id TYPE zmsg_ret_trans_id,
        msg_dt_def TYPE zdates,
        gr_no TYPE mblnr,
        req_id TYPE zreq_id,
        req_type TYPE zreq_type,
        mat_code TYPE matnr,
        sap_batch TYPE charg_d,
        qty TYPE erfmg,
        status TYPE zstat,
        old_status TYPE zold_status,
        total_pack TYPE qanzgeb,
        mfg_date TYPE zmfg_date,
        manufacturer TYPE zmanufacturer,
        mfg_batch TYPE zmfg_batch,
        line_item TYPE mblpo,
        plant TYPE werks_d,
        sent TYPE icon_d,
        rec TYPE icon_d,
        proc TYPE icon_d,
      END OF ty_asrs.

*SELECT-OPTIONS: s_werks FOR zmm_asrs-plant.
*PARAMETERS p_date TYPE datum OBLIGATORY.

PARAMETERS s_werks TYPE zmm_asrs-plant OBLIGATORY.
SELECT-OPTIONS: p_date FOR sy-datum OBLIGATORY.


DATA: gt_asrs TYPE STANDARD TABLE OF zmm_asrs,
      gt_con TYPE STANDARD TABLE OF zmm_dbcon_asrs,
      gs_con TYPE zmm_dbcon_asrs,
      gt_final TYPE STANDARD TABLE OF ty_asrs.
*
*AT SELECTION-SCREEN ON p_date.
* IF p_date-high IS INITIAL.
*    MESSAGE 'Fill all required fields' TYPE 'E'.
* ENDIF.


START-OF-SELECTION.
  PERFORM get_data.
  PERFORM process_data.

END-OF-SELECTION.
   PERFORM display_alv.
*&                                                                   *
*&       Form GET_DATA
*&                                                                   *
*        text
*                                                                    *
*     > p1         text
* <      p2        text
*                                                                    *
FORM get_data .
   SELECT msg_src
         msg_rec_id
         msg_trans_type
         msg_action
         msg_dt_def
         msg_stat
         gr_no
         req_id
         req_type
         mat_code
         sap_batch
         qty
         status
         old_status
         total_pack
         mfg_date
         manufacturer
         mfg_batch
         line_item
         plant
         trf_status
     FROM zmm_asrs
     INTO CORRESPONDING FIELDS OF TABLE gt_asrs
     WHERE msg_dt_def IN p_date
   AND plant = s_werks.

  SELECT *
    FROM zmm_dbcon_asrs
    INTO TABLE gt_con
  WHERE werks = s_werks.

ENDFORM.                     " GET_DATA
*&                                                                   *
*&       Form PROCESS_DATA
*&                                                                   *
*        text
*                                                                    *
*     > p1         text
* <      p2        text
*                                                                    *
FORM process_data .
   DATA: ls_asrs TYPE zmm_asrs,
         con(20) TYPE c,
         ls_final TYPE ty_asrs.
   DATA: exc_ref    TYPE REF TO cx_sy_native_sql_error,
       error_text TYPE string.
   SORT gt_asrs BY plant.


 LOOP AT gt_asrs INTO ls_asrs.
   MOVE-CORRESPONDING ls_asrs TO ls_final.
   ON CHANGE OF ls_asrs-plant.
     READ TABLE gt_con INTO gs_con WITH KEY werks = ls_asrs-plant.
     IF sy-subrc <> 0.
       CONTINUE.
     ENDIF.
     TRY.
          IF con IS NOT INITIAL.
            EXEC SQL.
              DISCONNECT :con
            ENDEXEC.
          ENDIF.
          CLEAR con.
          EXEC SQL. "#EC CI_EXECSQL
            CONNECT TO :gs_con-DBCON
          ENDEXEC.
          EXEC SQL. "#EC CI_EXECSQL
            SET CONNECTION :gs_con-DBCON
          ENDEXEC.
       CATCH cx_sy_native_sql_error INTO exc_ref.
     ENDTRY.

   ENDON.
   TRY.
        EXEC SQL.
          OPEN dbcur FOR
          SELECT MSG_ERR, MSG_STAT
            FROM HOST_TO_WMS
            WHERE MSG_REC_ID = :ls_asrs-MSG_REC_ID
        ENDEXEC.
        EXEC SQL. "#EC CI_EXECSQL
          FETCH NEXT dbcur INTO :ls_asrs-MSG_ERR, :ls_asrs-MSG_STAT
        ENDEXEC.
        EXEC SQL. "#EC CI_EXECSQL
          close dbcur
        ENDEXEC.
     CATCH cx_sy_native_sql_error INTO exc_ref.

   ENDTRY.
   IF sy-subrc <> 0.
     ls_final-rec = '@F1@'.
     ls_final-proc = '@KB@'.
   ELSE.
     ls_final-rec = '@DF@'.
     IF ls_asrs-msg_stat = 'TRM'.
       ls_final-proc = '@C9@'.
     ELSEIF ls_asrs-msg_stat = 'DEF'.
       ls_final-proc = '@7C@'.
     ELSE.
       ls_final-proc = '@AG@'.
     ENDIF.
   ENDIF.

   IF ls_asrs-trf_status = 'Y'.
     ls_final-sent = '@B4@'.
   ELSE.
     ls_final-sent = '@B2@'.
   ENDIF.

    APPEND ls_final TO gt_final.
    CLEAR: ls_final, ls_asrs.
  ENDLOOP.

  IF con IS NOT INITIAL.
    EXEC SQL.
      DISCONNECT :con
    ENDEXEC.
  ENDIF.

ENDFORM.                      " PROCESS_DATA
*&                                                                      *
*&       Form   DISPLAY_ALV
*&                                                                      *
*        text
*                                                                       *
*    > p1          text
* <     p2         text
*                                                                       *

FORM display_alv .

  DATA: go_alv TYPE REF TO cl_salv_table,
      go_columns TYPE REF TO cl_salv_columns,
      go_column TYPE REF TO cl_salv_column,
      go_func TYPE REF TO cl_salv_functions,
      go_layout TYPE REF TO cl_salv_layout.

  DATA: lv_small TYPE scrtext_s,
        lv_large TYPE scrtext_l,
        lv_med TYPE scrtext_m.

  DATA : ls_ref TYPE salv_s_ddic_reference.
  DATA : lo_filters TYPE REF TO cl_salv_filters.
  DATA : lv_edit TYPE lvc_edtmsk.
  TRY.
       CALL METHOD cl_salv_table=>factory
*                            EXPORTING
*                              list_display   = IF_SALV_C_BOOL_SAP=>FALSE
*                              r_container    =
*                              container_name =
         IMPORTING
           r_salv_table = go_alv
         CHANGING
           t_table      = gt_final.

       CALL METHOD go_alv->get_columns
         RECEIVING
            value = go_columns.
       TRY.
            CALL METHOD go_columns->get_column
              EXPORTING
                columnname = 'SENT'
              RECEIVING
                value      = go_column.
            lv_large = lv_med = lv_small = 'Sent to ASRS'.
            go_column->set_long_text( lv_large ).
            go_column->set_medium_text( lv_med ).
            go_column->set_short_text( lv_small ).

           CALL METHOD go_columns->get_column
             EXPORTING
               columnname = 'REC'
             RECEIVING
               value      = go_column.
           lv_large = lv_med = lv_small = 'Recieved to ASRS'.
           go_column->set_long_text( lv_large ).
           go_column->set_medium_text( lv_med ).
           go_column->set_short_text( lv_small ).

           CALL METHOD go_columns->get_column
             EXPORTING
               columnname = 'PROC'
             RECEIVING
               value      = go_column.
           lv_large = lv_med = lv_small = 'Processed by ASRS'.
           go_column->set_long_text( lv_large ).
           go_column->set_medium_text( lv_med ).
           go_column->set_short_text( lv_small ).

**SOA by Vrushali on 17-03-2017 for ticket : 8000003027
          CALL METHOD go_columns->get_column
            EXPORTING
              columnname = 'MAT_CODE'
            RECEIVING
              value      = go_column.
          CLEAR ls_ref.
          ls_ref-field = 'MAT_CODE'.
          ls_ref-table = 'ZMM_ASRS'.
          go_column->set_ddic_reference( ls_ref ).
          go_column->set_edit_mask( ' ' ).
**EOA by Vrushali on 17.03.2017
        CATCH cx_salv_not_found .
      ENDTRY.

      CALL METHOD go_alv->get_functions
        RECEIVING
          value = go_func.
      go_func->set_all( abap_true ).
      go_alv->display( ).
    CATCH cx_salv_msg .
  ENDTRY.


ENDFORM.                     " DISPLAY_ALV
