*&                                                                     *
*& Report ZQM_TRACKWISE
*&
*&                                                                     *
*&
*&
*&                                                                     *
* Program
* Title             : Track Wise System Report
* Author            : Phaneendra Reddy
* Program Type      : Executable Program
* Creation Date     : 24-11-2016
* Transaction       : ZTW_DEV_STAT_RPT
* Functional design : FS_QM_113_SAP-TRACKWISE data Migration
* Technical design : TS_QM_113_SAP-TRACKWISE data Migration
* Ticket Number     : 8000002067
* Transport request : CRDK940574
* Description       : Transfer data from SAP to Trackwise system for processing
*                     of deviations
*&                                                                     *
* Modification Information..
* Date              :
* Author            :
* Ticket Number     :
* Transport number :
* Description       :
*&                                                                     *

REPORT zqm_trackwise.
TYPES:BEGIN OF ty_plant,
  plant_code TYPE werks,
  plant_name TYPE name1,
END OF ty_plant.
DATA: gs_prod_det TYPE ztw_prod_det,
      gt_prod_det TYPE STANDARD TABLE OF ztw_prod_det,
      gs_mat_det TYPE ztw_mat_det,
      gt_mat_det TYPE STANDARD TABLE OF ztw_mat_det,
      gt_con TYPE STANDARD TABLE OF zcon_mdm,
      gs_con TYPE zcon_mdm,
      flag TYPE c,
      gs_plnt_comb TYPE ztw_plnt_comb,
      wa_plant TYPE ty_plant.
DATA:gv_name TYPE t001w-name1.
DATA:it_ztw_plnt_det TYPE TABLE OF ztw_plnt_comb.
DATA: lv_date TYPE char10.
DATA: lv_date1 TYPE char10.
DATA: lv_date2 TYPE char10.

FIELD-SYMBOLS: <fs_asrs> TYPE ztw_prod_det,
               <fs_asrs1> TYPE ztw_mat_det.
DATA: exc_ref    TYPE REF TO cx_sy_native_sql_error,
       error_text TYPE string.

DATA: dbs TYPE dbcon-con_name.
DATA: con(20) TYPE c.
DATA:gv_qmel TYPE viqmel.
DATA:wa_plnt_comb TYPE ztw_plnt_comb.

START-OF-SELECTION.
  PERFORM send_plant USING gv_qmel wa_plnt_comb.
   PERFORM send_trackwise.
*&                                                                     *
*&       Form SEND_TRACKWISE
*&                                                                     *
*        text
*                                                                      *
*     > p1         text
* <      p2        text
*                                                                      *
FORM send_trackwise .
   SELECT *
     FROM zcon_mdm CLIENT SPECIFIED
     INTO TABLE gt_con
   WHERE mandt = sy-mandt.
   IF gt_con IS NOT INITIAL.
     CLEAR gs_con.
" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 26.06.2026 FOR ATC
     READ TABLE gt_con INTO gs_con INDEX 1. "#EC CI_NOORDER
" Code Remediation changes S4 2025_A Conversion * *END OF CHANGE BY ABAP7 26.06.2026 FOR ATC

    IMPORT flag TO flag FROM MEMORY ID 'QM/19_FLAG'.
    IF flag = 'P'.
" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 26.06.2026 FOR ATC
*        IMPORT ta_prod_det TO gt_prod_det FROM MEMORY ID 'PRODUCT'.
IMPORT ta_prod_det TO gt_prod_det FROM MEMORY ID 'PRODUCT' ACCEPTING PADDING .
" Code Remediation changes S4 2025_A Conversion * *END OF CHANGE BY ABAP7 26.06.2026 FOR ATC
       PERFORM date_condition1.
       IF gt_prod_det IS NOT INITIAL .
         INSERT ztw_prod_det FROM TABLE gt_prod_det.
       ENDIF.
       IF con IS NOT INITIAL.
         EXEC SQL.
           DISCONNECT :con
         ENDEXEC.
       ENDIF.
    ELSEIF flag = 'M'.
" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 26.06.2026 FOR ATC
*        IMPORT ta_mat_det TO gt_mat_det FROM MEMORY ID 'MATERIAL'.
IMPORT ta_mat_det TO gt_mat_det FROM MEMORY ID 'MATERIAL' ACCEPTING PADDING .
" Code Remediation changes S4 2025_A Conversion * *END OF CHANGE BY ABAP7 26.06.2026 FOR ATC
       if gt_mat_det is not INITIAL.
       LOOP AT gt_mat_det ASSIGNING <fs_asrs1>.
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
        gs_mat_det = <fs_asrs1>.
        CLEAR:lv_date.
        IF gs_mat_det-date_of_supply IS NOT INITIAL."CN '00000000'.
*           lv_date = gs_mat_det-date_of_supply.
*           write gs_mat_det-date_of_supply to lv_date DD/MM/YYYY.
          CONCATENATE gs_mat_det-date_of_supply+6(2) gs_mat_det-date_of_supply+4(2)
          gs_mat_det-date_of_supply+0(4) into lv_date SEPARATED BY '-'.
        ENDIF.
        IF lv_date IS NOT INITIAL.
          TRY.
               EXEC SQL.
                 INSERT INTO ZTW_MAT_DET (Mat_Code,
                                            Mat_Desc,
                                            Batch_No,
                                            Plant_Code,
                                            Mfg_Batch_No,
                                            Ins_Lot_No,
                                            AR_No,
                                            Vendor_Code,
                                            Vendor_Name,
                                            Mfg_Code,
                                            Mfg_Name,
                                            Date_of_Supply,
                                            Qty_Supplied,
                                            UoM,
                                            Impacted_Prod_Code,
                                            Impa_Prod_Batch_No,
                                            Impacted_Prod_Name,
                                            Impa_Prod_Ver_No,
                                            Impacted_Ref_Doc_No,
                                            Impa_Mfg_Batch_No)
                         VALUES (              :gs_mat_det-Mat_Code,
                                               :gs_mat_det-Mat_Desc,
                                               :gs_mat_det-Batch_No,
                                               :gs_mat_det-Plant_Code,
                                               :gs_mat_det-Mfg_Batch_No,
                                               :gs_mat_det-Ins_Lot_No,
                                               :gs_mat_det-AR_No,
                                               :gs_mat_det-Vendor_Code,
                                               :gs_mat_det-Vendor_Name,
                                               :gs_mat_det-Mfg_Code,
                                               :gs_mat_det-Mfg_Name,
                                               :lv_date,
                                               :gs_mat_det-Qty_Supplied,
                                               :gs_mat_det-UoM,
                                               :gs_mat_det-Impacted_Prod_Code,
                                               :gs_mat_det-Impa_Prod_Batch_No,
                                               :gs_mat_det-Impacted_Prod_Name,
                                               :gs_mat_det-Impa_Prod_Ver_No,
                                               :gs_mat_det-Impacted_Ref_Doc_No,
                                               :gs_mat_det-Impa_Mfg_Batch_No)

               ENDEXEC.

             CATCH cx_sy_native_sql_error INTO exc_ref.
                error_text = exc_ref->get_text( ).
            MESSAGE error_text TYPE 'I'.
                CONTINUE.
           ENDTRY.
         ELSEIF lv_date IS INITIAL.
           TRY.
                EXEC SQL.
                  INSERT INTO ZTW_MAT_DET (Mat_Code,
                                             Mat_Desc,
                                             Batch_No,
                                             Plant_Code,
                                             Mfg_Batch_No,
                                             Ins_Lot_No,
                                             AR_No,
                                             Vendor_Code,
                                             Vendor_Name,
                                             Mfg_Code,
                                             Mfg_Name,
                                             Qty_Supplied,
                                             UoM,
                                             Impacted_Prod_Code,
                                             Impa_Prod_Batch_No,
                                             Impacted_Prod_Name,
                                             Impa_Prod_Ver_No,
                                             Impacted_Ref_Doc_No,
                                             Impa_Mfg_Batch_No)
                          VALUES (              :gs_mat_det-Mat_Code,
                                                :gs_mat_det-Mat_Desc,
                                                :gs_mat_det-Batch_No,
                                                :gs_mat_det-Plant_Code,
                                                :gs_mat_det-Mfg_Batch_No,
                                                :gs_mat_det-Ins_Lot_No,
                                                :gs_mat_det-AR_No,
                                                :gs_mat_det-Vendor_Code,
                                                :gs_mat_det-Vendor_Name,
                                                :gs_mat_det-Mfg_Code,
                                                :gs_mat_det-Mfg_Name,
                                                :gs_mat_det-Qty_Supplied,
                                                :gs_mat_det-UoM,
                                                :gs_mat_det-Impacted_Prod_Code,
                                               :gs_mat_det-Impa_Prod_Batch_No,
                                               :gs_mat_det-Impacted_Prod_Name,
                                               :gs_mat_det-Impa_Prod_Ver_No,
                                               :gs_mat_det-Impacted_Ref_Doc_No,
                                               :gs_mat_det-Impa_Mfg_Batch_No)

               ENDEXEC.

            CATCH cx_sy_native_sql_error INTO exc_ref.
               error_text = exc_ref->get_text( ).
             MESSAGE error_text TYPE 'I'.
               CONTINUE.
          ENDTRY.
        ENDIF.
        COMMIT WORK AND WAIT .

*   EXEC SQL.
*     DISCONNECT :con
*   ENDEXEC.
          <fs_asrs1>-status = 'Y'.
       ENDLOOP.
       endif.
*"             EXEC SQL.
*                SELECT   Plant_Name
*                    INTO :lv_werks
*                    FROM ZTW_PLNT_DET
*                    WHERE werks = 1015.
*              ENDEXEC.
       IF gt_mat_det IS NOT INITIAL.
          INSERT ztw_mat_det FROM TABLE gt_mat_det.
       ENDIF.
       IF con IS NOT INITIAL.
          EXEC SQL.
            DISCONNECT :con
          ENDEXEC.
       ENDIF.
     ENDIF.
   ENDIF.
ENDFORM.                      " SEND_TRACKWISE
*&                                                                     *
*&        Form SEND_PLANT
*&                                                                     *
*         text
*                                                                      *
*          >P_GV_QMEL text
*                                                                      *
FORM send_plant USING       p_gv_qmel TYPE viqmel
                          p_plnt_comb TYPE ztw_plnt_comb.

  CLEAR:gt_con.
  SELECT *
    FROM zcon_mdm CLIENT SPECIFIED
    INTO TABLE gt_con
  WHERE mandt = sy-mandt.
  IF gt_con IS NOT INITIAL.
    CLEAR gs_con.
" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 26.06.2026 FOR ATC
    READ TABLE gt_con INTO gs_con INDEX 1. "#EC CI_NOORDER
" Code Remediation changes S4 2025_A Conversion * *END OF CHANGE BY ABAP7 26.06.2026 FOR ATC
    IF sy-subrc = 0.
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
             error_text = exc_ref->get_text( ).
             MESSAGE error_text TYPE 'I'.
        ENDTRY.
        "Checking for entry is existing Or not.
        TRY.
             CLEAR:wa_plant.
             DATA:gv_plant TYPE ztw_prod_det-plant_code."werks.
             CLEAR:gv_plant.
             gv_plant = p_plnt_comb-werks.
             EXEC SQL.
               SELECT   Plant_Code, Plant_Name
                   INTO :wa_plant
                   FROM ZTW_PLNT_DET
                   WHERE Plant_Code = :gv_plant
             ENDEXEC.

        CATCH cx_sy_native_sql_error INTO exc_ref.
          error_text = exc_ref->get_text( ).
          MESSAGE error_text TYPE 'I'.
      ENDTRY.
      IF wa_plant IS INITIAL.
        gs_plnt_comb-werks = p_plnt_comb-werks.
        IF p_gv_qmel-mawerk IS NOT INITIAL.
          CLEAR:gv_name.
          SELECT SINGLE name1 FROM t001w INTO gv_name WHERE werks = p_gv_qmel-mawerk.
        ENDIF.
        TRY.
             EXEC SQL.
               INSERT INTO ZTW_PLNT_DET (Plant_Code,
                                          Plant_Name)
                       VALUES (              :gs_plnt_comb-werks,
                                             :gv_Name)

              ENDEXEC.


            CATCH cx_sy_native_sql_error INTO exc_ref.
              error_text = exc_ref->get_text( ).
              MESSAGE error_text TYPE 'I'.
*              CONTINUE.
          ENDTRY.

           COMMIT WORK AND WAIT .
        ENDIF.
      ENDIF.
    ENDIF.
    IF con IS NOT INITIAL.
      EXEC SQL.
        DISCONNECT :con
     ENDEXEC.
   ENDIF.
   CLEAR:con.
ENDFORM.                       " SEND_PLANT
*&                                                                         *
*&        Form DATE_CONDITION1
*&                                                                         *
*         text
*                                                                          *
*     > p1           text
* <      p2          text
*                                                                          *
FORM date_condition1 .
   if gt_prod_det is not INITIAL.
   LOOP AT gt_prod_det ASSIGNING <fs_asrs>.
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
     gs_prod_det = <fs_asrs>.
     CLEAR:lv_date,lv_date1,lv_date2.
*          if gs_prod_det-mfg_date CN '00000000'.

      IF gs_prod_det-mfg_date IS NOT INITIAL.
*        lv_date = gs_prod_det-mfg_date.
*        WRITE gs_prod_det-mfg_date to lv_date DD/MM/YYYY.
        CONCATENATE gs_prod_det-mfg_date+6(2) gs_prod_det-mfg_date+4(2)
             gs_prod_det-mfg_date+0(4) into lv_date SEPARATED BY '-'.
      ENDIF.
*          if gs_prod_det-expiry_date CN '00000000'.
      IF gs_prod_det-expiry_date IS NOT INITIAL.
*        lv_date1 = gs_prod_det-expiry_date.
*        WRITE gs_prod_det-expiry_date to lv_date1 DD/MM/YYYY.
        CONCATENATE gs_prod_det-expiry_date+6(2) gs_prod_det-expiry_date+4(2)
            gs_prod_det-expiry_date+0(4) into lv_date1 SEPARATED BY '-'.
      ENDIF.

      IF gs_prod_det-retest_date IS NOT INITIAL.
*        IF gs_prod_det-retest_date NE '00.00.0000'.
*          CALL FUNCTION 'CONVERSION_EXIT_SDATE_INPUT'
*            EXPORTING
*               input = gs_prod_det-retest_date
*            IMPORTING
*               output = lv_date2.
*        ENDIF.
**        lv_date2 = gs_prod_det-retest_date.
*         WRITE gs_prod_det-retest_date to lv_date2 DD/MM/YYYY.
        CONCATENATE gs_prod_det-retest_date+0(2) gs_prod_det-retest_date+3(2)
             gs_prod_det-retest_date+6(4) into lv_date2 SEPARATED BY '-'.
      ENDIF.
      DATA:lv_werks TYPE char30."werks.

    IF lv_date IS INITIAL AND lv_date1 IS NOT INITIAL AND lv_date2 IS NOT INITIAL.
      TRY.
           EXEC SQL.
             INSERT INTO ZTW_PROD_DET (Prod_Code,
                                        Prod_Name,
                                        Prod_Batch_No,
                                        Plant_Code,
                                        Production_Ver_No,
                                        Ref_Doc_No,
                                        Mfg_Batch_No,
                                        Expiry_Date,
                                        Retest_Date,
                                        Impacted_Mat_Code,
                                        Impacted_Mat_Desc,
                                        Impacted_Batch_No,
                                        Impa_Mat_Insp_Lot,
                                        Impa_Mfg_Batch_No,
                                        Impacted_Mat_AR_No)
                     VALUES (             :gs_prod_det-Prod_Code,
                                          :gs_prod_det-Prod_Name,
                                          :gs_prod_det-Prod_Batch_No,
                                          :gs_prod_det-Plant_Code,
                                          :gs_prod_det-Production_Ver_No,
                                          :gs_prod_det-Ref_Doc_No,
                                          :gs_prod_det-Mfg_Batch_No,
                                          :lv_date1,
                                          :lv_date2,
                                          :gs_prod_det-Impacted_Mat_Code,
                                          :gs_prod_det-Impacted_Mat_Desc,
                                          :gs_prod_det-Impacted_Batch_No,
                                          :gs_prod_det-Impa_Mat_Insp_Lot,
                                          :gs_prod_det-Impa_Mfg_Batch_No,
                                          :gs_prod_det-Impacted_Mat_AR_No)

           ENDEXEC.
        CATCH cx_sy_native_sql_error INTO exc_ref.
           error_text = exc_ref->get_text( ).
           MESSAGE error_text TYPE 'I'.
           CONTINUE.
      ENDTRY.
    ELSEIF lv_date IS INITIAL AND lv_date1 IS INITIAL AND lv_date2 IS NOT INITIAL.
      TRY.
           EXEC SQL.
             INSERT INTO ZTW_PROD_DET (Prod_Code,
                                         Prod_Name,
                                       Prod_Batch_No,
                                       Plant_Code,
                                       Production_Ver_No,
                                       Ref_Doc_No,
                                       Mfg_Batch_No,
                                       Retest_Date,
                                       Impacted_Mat_Code,
                                       Impacted_Mat_Desc,
                                       Impacted_Batch_No,
                                       Impa_Mat_Insp_Lot,
                                       Impa_Mfg_Batch_No,
                                       Impacted_Mat_AR_No)
                    VALUES (             :gs_prod_det-Prod_Code,
                                         :gs_prod_det-Prod_Name,
                                         :gs_prod_det-Prod_Batch_No,
                                         :gs_prod_det-Plant_Code,
                                         :gs_prod_det-Production_Ver_No,
                                         :gs_prod_det-Ref_Doc_No,
                                         :gs_prod_det-Mfg_Batch_No,
                                         :lv_date2,
                                         :gs_prod_det-Impacted_Mat_Code,
                                         :gs_prod_det-Impacted_Mat_Desc,
                                         :gs_prod_det-Impacted_Batch_No,
                                         :gs_prod_det-Impa_Mat_Insp_Lot,
                                         :gs_prod_det-Impa_Mfg_Batch_No,
                                         :gs_prod_det-Impacted_Mat_AR_No)

            ENDEXEC.
         CATCH cx_sy_native_sql_error INTO exc_ref.
            error_text = exc_ref->get_text( ).
            MESSAGE error_text TYPE 'I'.
            CONTINUE.
       ENDTRY.
    ELSEIF lv_date IS INITIAL AND lv_date1 IS INITIAL AND lv_date2 IS INITIAL.
       TRY.
            EXEC SQL.
              INSERT INTO ZTW_PROD_DET (Prod_Code,
                                          Prod_Name,
                                          Prod_Batch_No,
                                          Plant_Code,
                                          Production_Ver_No,
                                          Ref_Doc_No,
                                          Mfg_Batch_No,
                                          Impacted_Mat_Code,
                                          Impacted_Mat_Desc,
                                          Impacted_Batch_No,
                                          Impa_Mat_Insp_Lot,
                                          Impa_Mfg_Batch_No,
                                          Impacted_Mat_AR_No)
                      VALUES (              :gs_prod_det-Prod_Code,
                                            :gs_prod_det-Prod_Name,
                                            :gs_prod_det-Prod_Batch_No,
                                            :gs_prod_det-Plant_Code,
                                           :gs_prod_det-Production_Ver_No,
                                           :gs_prod_det-Ref_Doc_No,
                                           :gs_prod_det-Mfg_Batch_No,
                                           :gs_prod_det-Impacted_Mat_Code,
                                           :gs_prod_det-Impacted_Mat_Desc,
                                           :gs_prod_det-Impacted_Batch_No,
                                           :gs_prod_det-Impa_Mat_Insp_Lot,
                                           :gs_prod_det-Impa_Mfg_Batch_No,
                                           :gs_prod_det-Impacted_Mat_AR_No)

           ENDEXEC.
         CATCH cx_sy_native_sql_error INTO exc_ref.
           error_text = exc_ref->get_text( ).
           MESSAGE error_text TYPE 'I'.
           CONTINUE.
       ENDTRY.

    ELSEIF lv_date IS NOT INITIAL AND lv_date1 IS INITIAL AND lv_date2 IS INITIAL.
      TRY.
           EXEC SQL.
             INSERT INTO ZTW_PROD_DET (Prod_Code,
                                        Prod_Name,
                                        Prod_Batch_No,
                                        Plant_Code,
                                        Production_Ver_No,
                                        Ref_Doc_No,
                                        Mfg_Batch_No,
                                        Mfg_Date,
                                        Impacted_Mat_Code,
                                        Impacted_Mat_Desc,
                                        Impacted_Batch_No,
                                        Impa_Mat_Insp_Lot,
                                        Impa_Mfg_Batch_No,
                                        Impacted_Mat_AR_No)
                     VALUES (             :gs_prod_det-Prod_Code,
                                          :gs_prod_det-Prod_Name,
                                          :gs_prod_det-Prod_Batch_No,
                                          :gs_prod_det-Plant_Code,
                                          :gs_prod_det-Production_Ver_No,
                                          :gs_prod_det-Ref_Doc_No,
                                          :gs_prod_det-Mfg_Batch_No,
                                          :lv_date,
                                          :gs_prod_det-Impacted_Mat_Code,
                                          :gs_prod_det-Impacted_Mat_Desc,
                                          :gs_prod_det-Impacted_Batch_No,
                                          :gs_prod_det-Impa_Mat_Insp_Lot,
                                          :gs_prod_det-Impa_Mfg_Batch_No,
                                          :gs_prod_det-Impacted_Mat_AR_No)

           ENDEXEC.
        CATCH cx_sy_native_sql_error INTO exc_ref.
           error_text = exc_ref->get_text( ).
           MESSAGE error_text TYPE 'I'.
           CONTINUE.
      ENDTRY.
    ELSEIF lv_date IS NOT INITIAL AND lv_date1 IS NOT INITIAL AND lv_date2 IS INITIAL.
      TRY.
           EXEC SQL.
             INSERT INTO ZTW_PROD_DET (Prod_Code,
                                         Prod_Name,
                                         Prod_Batch_No,
                                         Plant_Code,
                                         Production_Ver_No,
                                         Ref_Doc_No,
                                         Mfg_Batch_No,
                                         Mfg_Date,
                                         Expiry_Date,
                                         Impacted_Mat_Code,
                                         Impacted_Mat_Desc,
                                         Impacted_Batch_No,
                                         Impa_Mat_Insp_Lot,
                                         Impa_Mfg_Batch_No,
                                         Impacted_Mat_AR_No)
                     VALUES (              :gs_prod_det-Prod_Code,
                                           :gs_prod_det-Prod_Name,
                                           :gs_prod_det-Prod_Batch_No,
                                           :gs_prod_det-Plant_Code,
                                           :gs_prod_det-Production_Ver_No,
                                           :gs_prod_det-Ref_Doc_No,
                                           :gs_prod_det-Mfg_Batch_No,
                                           :lv_date,
                                           :lv_date1,
                                           :gs_prod_det-Impacted_Mat_Code,
                                           :gs_prod_det-Impacted_Mat_Desc,
                                           :gs_prod_det-Impacted_Batch_No,
                                           :gs_prod_det-Impa_Mat_Insp_Lot,
                                           :gs_prod_det-Impa_Mfg_Batch_No,
                                           :gs_prod_det-Impacted_Mat_AR_No)

           ENDEXEC.
        CATCH cx_sy_native_sql_error INTO exc_ref.
           error_text = exc_ref->get_text( ).
           MESSAGE error_text TYPE 'I'.
           CONTINUE.
      ENDTRY.
    ELSEIF lv_date IS NOT INITIAL AND lv_date1 IS NOT INITIAL AND lv_date2 IS NOT INITIAL.
      TRY.
           EXEC SQL.
            INSERT INTO   ZTW_PROD_DET   (Prod_Code,
                                          Prod_Name,
                                          Prod_Batch_No,
                                          Plant_Code,
                                          Production_Ver_No,
                                          Ref_Doc_No,
                                          Mfg_Batch_No,
                                          Mfg_Date,
                                          Expiry_Date,
                                          Retest_Date,
                                          Impacted_Mat_Code,
                                          Impacted_Mat_Desc,
                                          Impacted_Batch_No,
                                          Impa_Mat_Insp_Lot,
                                          Impa_Mfg_Batch_No,
                                          Impacted_Mat_AR_No)
                    VALUES (                :gs_prod_det-Prod_Code,
                                            :gs_prod_det-Prod_Name,
                                            :gs_prod_det-Prod_Batch_No,
                                            :gs_prod_det-Plant_Code,
                                            :gs_prod_det-Production_Ver_No,
                                           :gs_prod_det-Ref_Doc_No,
                                           :gs_prod_det-Mfg_Batch_No,
                                           :lv_date,
                                           :lv_date1,
                                           :lv_date2,
                                           :gs_prod_det-Impacted_Mat_Code,
                                           :gs_prod_det-Impacted_Mat_Desc,
                                           :gs_prod_det-Impacted_Batch_No,
                                           :gs_prod_det-Impa_Mat_Insp_Lot,
                                           :gs_prod_det-Impa_Mfg_Batch_No,
                                           :gs_prod_det-Impacted_Mat_AR_No)

           ENDEXEC.
        CATCH cx_sy_native_sql_error INTO exc_ref.
           error_text = exc_ref->get_text( ).
           MESSAGE error_text TYPE 'I'.
           CONTINUE.
      ENDTRY.
    ELSEIF lv_date IS INITIAL AND lv_date1 IS NOT INITIAL AND lv_date2 IS INITIAL.
      TRY.
           EXEC SQL.
             INSERT INTO ZTW_PROD_DET (Prod_Code,
                                         Prod_Name,
                                         Prod_Batch_No,
                                         Plant_Code,
                                         Production_Ver_No,
                                         Ref_Doc_No,
                                         Mfg_Batch_No,
                                         Expiry_Date,
                                         Impacted_Mat_Code,
                                         Impacted_Mat_Desc,
                                         Impacted_Batch_No,
                                        Impa_Mat_Insp_Lot,
                                        Impa_Mfg_Batch_No,
                                        Impacted_Mat_AR_No)
                    VALUES (              :gs_prod_det-Prod_Code,
                                          :gs_prod_det-Prod_Name,
                                          :gs_prod_det-Prod_Batch_No,
                                          :gs_prod_det-Plant_Code,
                                          :gs_prod_det-Production_Ver_No,
                                          :gs_prod_det-Ref_Doc_No,
                                          :gs_prod_det-Mfg_Batch_No,
                                          :lv_date1,
                                          :gs_prod_det-Impacted_Mat_Code,
                                          :gs_prod_det-Impacted_Mat_Desc,
                                          :gs_prod_det-Impacted_Batch_No,
                                          :gs_prod_det-Impa_Mat_Insp_Lot,
                                          :gs_prod_det-Impa_Mfg_Batch_No,
                                          :gs_prod_det-Impacted_Mat_AR_No)

           ENDEXEC.
        CATCH cx_sy_native_sql_error INTO exc_ref.
           error_text = exc_ref->get_text( ).
           MESSAGE error_text TYPE 'I'.
           CONTINUE.
      ENDTRY.
    ELSEIF lv_date IS NOT INITIAL AND lv_date1 IS INITIAL AND lv_date2 IS NOT INITIAL.
      TRY.
           EXEC SQL.
             INSERT INTO ZTW_PROD_DET (Prod_Code,
                                         Prod_Name,
                                         Prod_Batch_No,
                                         Plant_Code,
                                         Production_Ver_No,
                                         Ref_Doc_No,
                                         Mfg_Batch_No,
                                         Mfg_Date,
                                         Retest_Date,
                                         Impacted_Mat_Code,
                                         Impacted_Mat_Desc,
                                         Impacted_Batch_No,
                                         Impa_Mat_Insp_Lot,
                                         Impa_Mfg_Batch_No,
                                         Impacted_Mat_AR_No)
                     VALUES (              :gs_prod_det-Prod_Code,
                                           :gs_prod_det-Prod_Name,
                                           :gs_prod_det-Prod_Batch_No,
                                           :gs_prod_det-Plant_Code,
                                           :gs_prod_det-Production_Ver_No,
                                           :gs_prod_det-Ref_Doc_No,
                                           :gs_prod_det-Mfg_Batch_No,
                                           :lv_date,
                                           :lv_date2,
                                           :gs_prod_det-Impacted_Mat_Code,
                                          :gs_prod_det-Impacted_Mat_Desc,
                                          :gs_prod_det-Impacted_Batch_No,
                                          :gs_prod_det-Impa_Mat_Insp_Lot,
                                          :gs_prod_det-Impa_Mfg_Batch_No,
                                          :gs_prod_det-Impacted_Mat_AR_No)

           ENDEXEC.
         CATCH cx_sy_native_sql_error INTO exc_ref.
           error_text = exc_ref->get_text( ).
           MESSAGE error_text TYPE 'I'.
           CONTINUE.
       ENDTRY.
    ENDIF.
    COMMIT WORK AND WAIT .
    <fs_asrs>-status = 'Y'.
  ENDLOOP.
endif.
ENDFORM.                     " DATE_CONDITION1
