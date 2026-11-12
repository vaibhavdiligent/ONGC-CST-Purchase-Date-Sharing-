"! <p class="shorttext synchronized">Custom Data Monitor task: reverse docs of a document type</p>
"!
"! Implementation of BAdI BADI_FINCS_CUST_DATAMON
"! (enhancement implementation FINCS_CUSTOM_TASK_DATA_MONITOR).
"! BAdI filter: CONSOLIDATIONTASK = <your custom task>
"!
"! Flow: EXECUTE (called by the Data Monitor) -> RUN_REVERSAL
"!       -> ZCL_FINCS_GJE_REVERSAL->EXECUTE -> SUBMIT FICRVS00 (CXA0)
CLASS zcl_fincs_cdm_reverse_badi DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_badi_interface.
    INTERFACES if_badi_fincs_cust_datamon.

    "-- Customer settings for this task
    CONSTANTS:
      gc_doctype_to_reverse TYPE acdocu-docty  VALUE 'XX',  " <-- doc type to reverse
      gc_ledger             TYPE acdocu-rldnr  VALUE 'Y1',  " <-- group reporting ledger
      gc_dimension          TYPE acdocu-rdimen VALUE 'Y1',  " <-- consolidation dimension
      "-- Fixed values of domain FC_STATUS (SE11 -> domain FC_STATUS -> Value Range)
      gc_status_ok          TYPE fc_status     VALUE '9',   " Task: No errors
      gc_status_error       TYPE fc_status     VALUE '8'.   " Task: Error(s)

    "! Business logic, independent of the BAdI signature
    CLASS-METHODS run_reversal
      IMPORTING iv_rvers     TYPE acdocu-rvers
                iv_ryear     TYPE acdocu-ryear
                iv_poper     TYPE acdocu-poper
                it_rbunit    TYPE zcl_fincs_gje_reversal=>ty_selection-rbunit
                iv_test_run  TYPE abap_bool
      EXPORTING et_messages  TYPE bapiret2_t
      RETURNING VALUE(rv_ok) TYPE abap_bool.
ENDCLASS.



CLASS zcl_fincs_cdm_reverse_badi IMPLEMENTATION.

  METHOD if_badi_fincs_cust_datamon~execute.
    DATA lt_bunit TYPE zcl_fincs_gje_reversal=>ty_selection-rbunit.

    "-- 1. Consolidation units of this run ----------------------------------
    "   The monitor passes either a list (CONSOLIDATIONUNITTAB) or one unit.
    "   FINCS_T_UNIT may be a table of FC_BUNIT or of a structure; both work.
    LOOP AT consolidationunittab ASSIGNING FIELD-SYMBOL(<ls_unit>).
      ASSIGN COMPONENT 'CONSOLIDATIONUNIT' OF STRUCTURE <ls_unit> TO FIELD-SYMBOL(<lv_unit>).
      IF sy-subrc <> 0.
        ASSIGN COMPONENT 'BUNIT' OF STRUCTURE <ls_unit> TO <lv_unit>.
      ENDIF.
      IF sy-subrc <> 0.
        ASSIGN <ls_unit> TO <lv_unit>.          " elementary line type FC_BUNIT
      ENDIF.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <lv_unit> ) TO lt_bunit.
    ENDLOOP.

    IF lt_bunit IS INITIAL AND consolidationunit IS NOT INITIAL.
      lt_bunit = VALUE #( ( sign = 'I' option = 'EQ' low = consolidationunit ) ).
    ENDIF.

    "-- 2. Reverse (via CXA0 / FICRVS00) -----------------------------------
    "   Called once per version (base + extension versions); the selection is
    "   by CONSOLIDATIONVERSION, so each call only reverses its own documents.
    DATA(lv_ok) = run_reversal(
      EXPORTING iv_rvers    = consolidationversion
                iv_ryear    = fiscalyear
                iv_poper    = CONV #( fiscalperiod )
                it_rbunit   = lt_bunit
                iv_test_run = xsdbool( isconsolidationtasktestrun IS NOT INITIAL )
      IMPORTING et_messages = DATA(lt_messages) ).

    "-- 3. Task status (CHANGING, MANDATORY on every call) ------------------
    consolidationtaskstatus = COND #( WHEN lv_ok = abap_true
                                      THEN gc_status_ok
                                      ELSE gc_status_error ).

    "-- 4. Messages back to the monitor (CHANGING, optional) -----------------
    "   BAPIRET2 -> FINCS_SYSTEMMESSAGE_S (MSGTY/MSGID/MSGNR/MSGV1-4)
    LOOP AT lt_messages INTO DATA(ls_msg).
      APPEND VALUE #( systemmessagetype           = ls_msg-type
                      systemmessageidentification = ls_msg-id
                      systemmessagenumber         = ls_msg-number
                      systemmessagevariable1      = ls_msg-message_v1
                      systemmessagevariable2      = ls_msg-message_v2
                      systemmessagevariable3      = ls_msg-message_v3
                      systemmessagevariable4      = ls_msg-message_v4 ) TO systemmessage.
    ENDLOOP.
  ENDMETHOD.


  METHOD run_reversal.
    DATA(lo_rev) = NEW zcl_fincs_gje_reversal(
      is_selection = VALUE #( rldnr    = gc_ledger
                              rdimen   = gc_dimension
                              rvers    = iv_rvers
                              ryear    = iv_ryear
                              poper    = iv_poper
                              rbunit   = it_rbunit
                              docty    = gc_doctype_to_reverse
                              test_run = iv_test_run ) ).

    rv_ok = lo_rev->execute( IMPORTING et_messages = et_messages ).
  ENDMETHOD.

ENDCLASS.
