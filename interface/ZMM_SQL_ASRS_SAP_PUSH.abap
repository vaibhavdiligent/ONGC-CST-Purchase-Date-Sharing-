*&                                                                         *
*& Report   ZDEMO_SQL_ASRS_SAP
*&
*&                                                                         *
*&
*&
*&                                                                         *

REPORT zmm_sql_asrs_sap_push.

TABLES: zmm_asrs.

SELECT-OPTIONS: s_type for zmm_asrs-msg_trans_type.

DATA: gs_asrs TYPE zmm_asrs,
      gt_asrs TYPE STANDARD TABLE OF zmm_asrs,
      gt_con TYPE STANDARD TABLE OF zmm_dbcon_asrs,
      gs_con TYPE zmm_dbcon_asrs.
FIELD-SYMBOLS <fs_asrs> TYPE zmm_asrs.
DATA: dbs TYPE dbcon-con_name.
DATA: con(20) TYPE c.


START-OF-SELECTION.

  SELECT *
    FROM zmm_dbcon_asrs
    INTO TABLE gt_con.







*WRITE sy-subrc.



  DATA: lv_date TYPE char10.
  DATA: lv_date1 TYPE char10.
  DATA: lv_date2 TYPE char10.
  SELECT *
    FROM zmm_asrs
    INTO CORRESPONDING FIELDS OF TABLE gt_asrs
    WHERE msg_trans_type in s_type
    AND trf_status = 'N'.
* lv_date = sy-datum.
* exec SQL.
*    SELECT MSG_REC_ID    FROM HOST_TO_WMS_TEST INTO :gs_asrs-msg_rec_id
* ENDEXEC.
  DATA: exc_ref    TYPE REF TO cx_sy_native_sql_error,
        error_text TYPE string.

  SORT gt_asrs BY plant.
  LOOP AT gt_asrs ASSIGNING <fs_asrs>.
    ON CHANGE OF <fs_asrs>-plant.
      READ TABLE gt_con INTO gs_con WITH KEY werks = <fs_asrs>-plant.
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
            GET CONNECTION :CON
          ENDEXEC.
          EXEC SQL.
            SET CONNECTION :gs_con-DBCON
          ENDEXEC.
       CATCH cx_sy_native_sql_error INTO exc_ref.
     ENDTRY.
   ENDON.
   gs_asrs = <fs_asrs>.

   lv_date = gs_asrs-msg_dt_def.
   IF gs_asrs-msg_dt_trm IS NOT INITIAL.
     lv_date1 = gs_asrs-msg_dt_trm.
   ENDIF.
   IF gs_asrs-mfg_date IS NOT INITIAL.
     lv_date2 = gs_asrs-mfg_date.
   ENDIF.
   TRY.
        EXEC SQL.
          INSERT INTO host_to_wms (MSG_SRC,
                                       MSG_REC_ID,
                                       MSG_TRANS_TYPE,
                                       MSG_ACTION,
                                       MSG_RET_SRC,
                                       MSG_RET_REC_ID,
                                       MSG_RET_TRANS_ID,
                                       MSG_DT_DEF,
                                       MSG_DT_TRM,
                                       MSG_ERR,
                                       MSG_ERR_DESC,
                                       MSG_STAT,
                                       GR_NO,
                                       REQ_ID,
                                       REQ_TYPE,
                                       MAT_CODE,
                                       DESCRIPTION,
                                       UOM,
                                       ITEM_TYPE,
                                       SAP_BATCH,
                                       QTY,
                                       STATUS,
                                       TOTAL_PACK,
                                       MFG_DATE,
                                       MANUFACTURER,
                                       MFG_BATCH,
                                       LINE_ITEM,
                                       PLANT,
                                       OLD_STATUS)
                  VALUES (             :gs_asrs-MSG_SRC ,
                                       :gs_asrs-MSG_REC_ID   ,
                                          :gs_asrs-MSG_TRANS_TYPE ,
                                          :gs_asrs-MSG_ACTION ,
                                          :gs_asrs-MSG_RET_SRC ,
                                          :gs_asrs-MSG_RET_REC_ID ,
                                          :gs_asrs-MSG_RET_TRANS_ID ,
                                          :lv_date,
                                          :lv_date1,
                                          :gs_asrs-MSG_ERR ,
                                          :gs_asrs-MSG_ERR_DESC ,
                                          :gs_asrs-MSG_STAT ,
                                          :gs_asrs-GR_NO ,
                                          :gs_asrs-REQ_ID ,
                                          :gs_asrs-REQ_TYPE ,
                                          :gs_asrs-MAT_CODE ,
                                          :gs_asrs-DESCRIPTION ,
                                          :gs_asrs-UOM ,
                                          :gs_asrs-ITEM_TYPE ,
                                          :gs_asrs-SAP_BATCH ,
                                          :gs_asrs-QTY ,
                                          :gs_asrs-STATUS ,
                                          :gs_asrs-TOTAL_PACK ,
                                          :lv_date2,
                                          :gs_asrs-MANUFACTURER ,
                                          :gs_asrs-MFG_BATCH ,
                                          :gs_asrs-LINE_ITEM ,
                                          :gs_asrs-PLANT,
                                          :gs_asrs-OLD_STATUS     )
          ENDEXEC.

        CATCH cx_sy_native_sql_error INTO exc_ref.
          error_text = exc_ref->get_text( ).

          CONTINUE.
      ENDTRY.

      COMMIT WORK AND WAIT .

*    EXEC SQL.
*      DISCONNECT :con
*    ENDEXEC.
      <fs_asrs>-trf_status   = 'Y'.
    ENDLOOP.

    IF con IS NOT INITIAL.
      EXEC SQL.
        DISCONNECT :con
      ENDEXEC.
    ENDIF.
    MODIFY zmm_asrs FROM TABLE gt_asrs.
