"! <p class="shorttext synchronized">Custom Data Monitor task: reverse docs of a document type</p>
"!
"! Implementation of BAdI BADI_FINCS_CUST_DATAMON
"! (enhancement spot / implementation FINCS_CUSTOM_TASK_DATA_MONITOR).
"! BAdI filter: CONSOLIDATIONTASK = <your custom task, e.g. 1Z10>
"!
"! ---------------------------------------------------------------------------
"! HOW TO FIT THIS TO YOUR SYSTEM
"!  The interface/method/parameter names of this BAdI are not published
"!  outside the system and differ by release. Do this once:
"!   1. SE19 -> your implementation -> double-click the implementing class.
"!      SE24 already generated the correct INTERFACES statement and method
"!      stub. Keep the generated stub.
"!   2. Paste the body of method RUN_REVERSAL below into the class and call
"!      it from the generated method, mapping the importing parameters
"!      (version, fiscal year, period, cons. units, test run, ...) and
"!      setting the exporting task status + messages (see the mapping block).
"! ---------------------------------------------------------------------------
CLASS zcl_fincs_cdm_reverse_badi DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_badi_interface.
    " >>> Replace by the interface generated for BADI_FINCS_CUST_DATAMON <<<
    " INTERFACES if_fincs_cust_datamon.

    "-- Customer settings for this task
    CONSTANTS:
      gc_doctype_to_reverse TYPE acdocu-docty  VALUE 'XX',  " <-- doc type to reverse
      gc_ledger             TYPE acdocu-rldnr  VALUE 'Y1',  " <-- group reporting ledger
      gc_dimension          TYPE acdocu-rdimen VALUE 'Y1'.  " <-- consolidation dimension

    "! Business logic, independent of the exact BAdI signature
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


  " ---------------------------------------------------------------------------
  " MAPPING BLOCK - put this into the generated BAdI method.
  " Parameter names (iv_*, is_*, ev_*, et_*) are illustrative: use the ones
  " shown in the generated signature.
  " ---------------------------------------------------------------------------
  " METHOD if_fincs_cust_datamon~execute.      " <-- generated method name
  "
  "   " BAdI is called once for the base version AND once per extension
  "   " version - reverse only for the version that owns the document.
  "
  "   DATA(lt_bunit) = VALUE zcl_fincs_gje_reversal=>ty_selection-rbunit(
  "     FOR <u> IN it_consolidation_units                  " cons. units of the run
  "     ( sign = 'I' option = 'EQ' low = <u>-consolidationunit ) ).
  "
  "   DATA(lv_ok) = run_reversal(
  "     EXPORTING iv_rvers    = is_parameters-consolidationversion
  "               iv_ryear    = is_parameters-fiscalyear
  "               iv_poper    = is_parameters-fiscalperiod
  "               it_rbunit   = lt_bunit
  "               iv_test_run = is_parameters-testrun
  "     IMPORTING et_messages = DATA(lt_msg) ).
  "
  "   " Task status is MANDATORY for every BAdI call (else task stays open)
  "   ev_consolidationtaskstatus = COND #( WHEN lv_ok = abap_true
  "                                        THEN <status 'executed OK'>    " see value help / domain
  "                                        ELSE <status 'with errors'> ).
  "   et_messages = lt_msg.                  " if the signature offers a message table
  " ENDMETHOD.

ENDCLASS.
