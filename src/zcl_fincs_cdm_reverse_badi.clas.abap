"! <p class="shorttext synchronized">Custom Data Monitor task: post inverse of ZR as ZS</p>
* Implementation of BAdI BADI_FINCS_CUST_DATAMON
* BAdI filter: CONSOLIDATIONTASK = your custom task
* Rule: all ZR documents (PL01) of the unit/period are posted inverted
*       in the same period as ZS documents (PL10).
CLASS zcl_fincs_cdm_reverse_badi DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_badi_interface.
    INTERFACES if_badi_fincs_cust_datamon.

    CONSTANTS:
      gc_doctype_source     TYPE acdocu-docty  VALUE 'ZR',  " Std-alone Re-groupings (PL01)
      gc_doctype_target     TYPE acdocu-docty  VALUE 'ZS',  " Rev of Std. Re-Grouping (PL10)
      gc_plevl_target       TYPE acdocu-plevl  VALUE '10',  " posting level of ZS
      gc_ledger             TYPE acdocu-rldnr  VALUE '1G',  " group reporting ledger
      gc_dimension          TYPE acdocu-rdimen VALUE 'Y1',  " consolidation dimension
      gc_status_ok          TYPE fc_status     VALUE '9',   " Task: No errors
      gc_status_error       TYPE fc_status     VALUE '8'.   " Task: Error(s)

    CLASS-METHODS run_reversal
      IMPORTING iv_rvers        TYPE acdocu-rvers
                iv_ryear        TYPE acdocu-ryear
                iv_poper        TYPE acdocu-poper
                iv_ritclg       TYPE fc_itclg
                iv_rcongr       TYPE fc_congr OPTIONAL
                iv_docty_target TYPE acdocu-docty
                it_rbunit       TYPE zcl_fincs_gje_reversal=>ty_selection-rbunit
                iv_test_run     TYPE abap_bool
      EXPORTING et_messages     TYPE bapiret2_t
      RETURNING VALUE(rv_ok)    TYPE abap_bool.
ENDCLASS.



CLASS zcl_fincs_cdm_reverse_badi IMPLEMENTATION.

  METHOD if_badi_fincs_cust_datamon~execute.
    DATA lt_bunit    TYPE zcl_fincs_gje_reversal=>ty_selection-rbunit.
    DATA lt_messages TYPE bapiret2_t.

    "-- 1. Consolidation units of this run
    LOOP AT consolidationunittab INTO DATA(lv_unit).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_unit ) TO lt_bunit.
    ENDLOOP.

    IF lt_bunit IS INITIAL AND consolidationunit IS NOT INITIAL.
      lt_bunit = VALUE #( ( sign = 'I' option = 'EQ' low = consolidationunit ) ).
    ENDIF.

    "-- 2. Post inverse of ZR as ZS (target type: from task customizing, else ZS)
    DATA(lv_ok) = run_reversal(
      EXPORTING iv_rvers        = consolidationversion
                iv_ryear        = fiscalyear
                iv_poper        = fiscalperiod
                iv_ritclg       = consolidationchartofaccounts
                iv_rcongr       = consolidationgroup
                iv_docty_target = COND #( WHEN consolidationdocumenttype IS NOT INITIAL
                                          THEN consolidationdocumenttype
                                          ELSE gc_doctype_target )
                it_rbunit       = lt_bunit
                iv_test_run     = xsdbool( isconsolidationtasktestrun IS NOT INITIAL )
      IMPORTING et_messages     = lt_messages ).

    "-- 3. Task status (mandatory)
    consolidationtaskstatus = COND #( WHEN lv_ok = abap_true
                                      THEN gc_status_ok
                                      ELSE gc_status_error ).

    "-- 4. Messages to the task log
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
      is_selection = VALUE #( rldnr        = gc_ledger
                              rdimen       = gc_dimension
                              rvers        = iv_rvers
                              ryear        = iv_ryear
                              poper        = iv_poper
                              ritclg       = iv_ritclg
                              rcongr       = iv_rcongr
                              rbunit       = it_rbunit
                              docty        = gc_doctype_source
                              docty_target = iv_docty_target
                              plevl_target = gc_plevl_target
                              test_run     = iv_test_run ) ).

    rv_ok = lo_rev->execute( IMPORTING et_messages = et_messages ).
  ENDMETHOD.

ENDCLASS.
