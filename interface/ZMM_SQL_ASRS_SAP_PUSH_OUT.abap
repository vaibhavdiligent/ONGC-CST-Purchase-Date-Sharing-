*&                                                                        *
*& Report ZDEMO_SQL_ASRS_SAP
*&
*&                                                                        *
* MODIFICATION INFORMATION
* DATE              : 02.11.2018
* AUTHOR            : Sushil Pandey
* CHANGE REQUEST    : <8000004873>
* TRANSPORT REQUEST : <CRDK958605>
* DESCRIPTION       : conectn -ASRS
*&                                                                        *

REPORT zmm_sql_asrs_sap_push_out.

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

"""" SOC done by Sushil against ticket number 8000004873- Error conectn-ASRS - TR: CRDK958605
        DATA: lv_asrs_usr(25),
              lv_asrs_werk TYPE werks.
          CONSTANTS: lc_asrs_mem(10) VALUE 'ASRS_MEMO'.

           lv_asrs_usr = { sy-uname }{ lc_asrs_mem } .
           import lv_asrs_werk FROM MEMORY ID lv_asrs_usr.
        """" EOC done by Sushil against ticket number 8000004873- Errorconectn -ASRS - TR: CRDK958605
IF sy-subrc eq 0. " LOC added by Sushil against ticket number 8000004873
  SELECT *
    FROM zmm_dbcon_asrs
    INTO TABLE gt_con
    WHERE WERKS = LV_ASRS_WERK. " WERKS has been added by Sushil againstticket number 8000004873 to fetch connection for specific client.


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
*      SELECT MSG_REC_ID   FROM HOST_TO_WMS_TEST INTO :gs_asrs-msg_rec_id
*    ENDEXEC.
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
                                          MSG_DT_DEF,
                                          MSG_STAT,
                                          REQ_ID,
                                          REQ_TYPE,
                                          MAT_CODE,
                                          SAP_BATCH,
                                          QTY,
                                          STATUS,
                                          LINE_ITEM,
                                          PLANT)
                     VALUES (             :gs_asrs-MSG_SRC ,
                                          :gs_asrs-MSG_REC_ID ,
                                          :gs_asrs-MSG_TRANS_TYPE   ,
                                          :gs_asrs-MSG_ACTION ,
                                          :lv_date,
                                          :gs_asrs-MSG_STAT ,
                                          :gs_asrs-REQ_ID ,
                                          :gs_asrs-REQ_TYPE ,
                                          :gs_asrs-MAT_CODE ,
                                          :gs_asrs-SAP_BATCH ,
                                          :gs_asrs-QTY ,
                                          :gs_asrs-STATUS ,
                                          :gs_asrs-LINE_ITEM ,
                                          :gs_asrs-PLANT)
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



    ENDIF. " LOC added by Sushil against ticket number 8000004873
