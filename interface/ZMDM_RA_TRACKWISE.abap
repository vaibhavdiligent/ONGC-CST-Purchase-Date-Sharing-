*&                                                                     *
*& Report   ZMDM_RA_TRACKWISE
*&
*&                                                                     *
*&
*&
*&                                                                     *

REPORT zmdm_ra_trackwise.

TABLES: mara.

DATA: gs_mara TYPE mara,
      gt_mara TYPE STANDARD TABLE OF mara,
      gt_con TYPE STANDARD TABLE OF zcon_mdm,
      gs_con TYPE zcon_mdm,
      g_field1 TYPE zmm_param-field1." WITH HEADER LINE.

DATA: exc_ref    TYPE REF TO cx_sy_native_sql_error,
      error_text TYPE string.

FIELD-SYMBOLS <fs_mara> TYPE mara.
DATA: dbs TYPE dbcon-con_name.
DATA: con(20) TYPE c.

DATA: l_cnt(3) TYPE n.
*
SELECT-OPTIONS: s_ersda FOR mara-ersda .
*

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM process_data.

*&                                                                     *
*&       Form GET_DATA
*&                                                                     *
*        text
*                                                                      *
*     > p1         text
* <      p2        text
*                                                                      *
FORM get_data .
   CLEAR g_field1.
   REFRESH: gt_mara[], gt_con[].

  SELECT *
    FROM zcon_mdm CLIENT SPECIFIED
    INTO TABLE gt_con
    WHERE mandt = sy-mandt.

" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 26.06.2026 FOR ATC
* SELECT SINGLE field1 FROM zmm_param CLIENT SPECIFIED
*     INTO g_field1
*          WHERE mandt        = sy-mandt
*          AND    param_type = 'ZZ9'.
  SELECT FIELD1 FROM ZMM_PARAM CLIENT SPECIFIED  UP TO 1 ROWS INTO
G_FIELD1 WHERE MANDT = SY-MANDT AND PARAM_TYPE = 'ZZ9' ORDER BY SRNO .
    ENDSELECT.
" Code Remediation changes S4 2025_A Conversion * *END OF CHANGE BY ABAP7 26.06.2026 FOR ATC

   SELECT *
     FROM mara
     INTO TABLE gt_mara
     WHERE ersda IN s_ersda
     AND mtart ='ZAPI'
*     AND lvorm NE 'X'    "not require on 27.10.2016
     AND mstae NOT IN (g_field1).
ENDFORM.                      " GET_DATA
*&                                                                       *
*&       Form PROCESS_DATA
*&                                                                       *
*        text
*                                                                        *
*     > p1         text
* <      p2        text
*                                                                        *
FORM process_data .
   SORT gt_mara BY matnr.

  CLEAR gs_con.
" Code Remediation changes S4 2025_A Conversion **BEGIN OF CHANGE BY ABAP7 26.06.2026 FOR ATC
  READ TABLE gt_con INTO gs_con INDEX 1. "#EC CI_NOORDER
" Code Remediation changes S4 2025_A Conversion * *END OF CHANGE BY ABAP7 26.06.2026 FOR ATC
  IF sy-subrc = 0.

    CLEAR l_cnt.
    LOOP AT gt_mara INTO gs_mara.   "ASSIGNING <fs_mara>.
*    ON CHANGE OF <fs_mara>-plant.
*      READ TABLE gt_con INTO gs_con WITH KEY werks = <fs_mara>-plant.
*      IF sy-subrc <> 0.
*        CONTINUE.
*      ENDIF.
      TRY.
           IF con IS NOT INITIAL.
             EXEC SQL.
               DISCONNECT :con
             ENDEXEC.
           ENDIF.

            CLEAR con.
            EXEC SQL.
              CONNECT TO :gs_con-DBCON
            ENDEXEC.

            EXEC SQL.
              GET CONNECTION :CON
            ENDEXEC.

            EXEC SQL.
              SET CONNECTION :gs_con-DBCON
            ENDEXEC.

          CATCH cx_sy_native_sql_error INTO exc_ref.
        ENDTRY.
*      ENDON.

*              UPDATE mara
*SET
*                INSERT INTO mara (matnr)
*                        VALUES (gs_mara-matnr)

        SHIFT gs_mara-matnr LEFT DELETING LEADING '0'.

        TRY.
            EXEC SQL.
              INSERT INTO    MARA (MATNR)
                                    VALUES ( :gs_mara-matnr)

            ENDEXEC.

          CATCH cx_sy_native_sql_error INTO exc_ref.
            error_text = exc_ref->get_text( ).

            l_cnt = l_cnt + 1.
            CONTINUE.
        ENDTRY.

        COMMIT WORK AND WAIT .
      ENDLOOP.

      IF con IS NOT INITIAL.
        EXEC SQL.
          DISCONNECT :con
        ENDEXEC.
        DATA: l_str TYPE string.


*        CONCATENATE '' 'Records inserted successfully' INTO l_str.
*        MESSAGE l_str TYPE 'S'.

        CONCATENATE '' 'Data inserted successfully' INTO l_str.
        MESSAGE l_str TYPE 'S'.
      ENDIF.

    ELSE.
      MESSAGE 'Connection not maintained.' TYPE    'E'.
    ENDIF.

* MODIFY zmara FROM TABLE gt_mara.
ENDFORM.                   " PROCESS_DATA
