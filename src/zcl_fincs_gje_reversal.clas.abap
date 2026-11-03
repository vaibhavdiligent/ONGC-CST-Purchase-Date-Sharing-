"! <p class="shorttext synchronized">Group journal entry reversal helper (used by custom DM task)</p>
"!
"! Reverses group journal entries (ACDOCU) of a given consolidation document type
"! for one version / fiscal year / period / consolidation unit selection.
"!
"! Reversal is delegated to the SAP standard mass-reversal program FICRVS00
"! (transaction CXA0), so all standard checks, number ranges, reversal flags
"! and document links are applied exactly as in CXA0.
"!
"! ---------------------------------------------------------------------------
"! BEFORE ACTIVATING - verify in YOUR system (release dependent):
"!  1. ACDOCU field names used in GET_DOCUMENTS (SE11 -> ACDOCU).
"!  2. Selection-screen names of FICRVS00 (SE38 -> FICRVS00 -> Display ->
"!     Goto -> Text elements -> Selection texts, or F1 -> Technical info on
"!     each CXA0 field). Put them into the GC_SEL constants below.
"! ---------------------------------------------------------------------------
CLASS zcl_fincs_gje_reversal DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_selection,
        rldnr      TYPE acdocu-rldnr,     " Consolidation ledger
        rdimen     TYPE acdocu-rdimen,    " Consolidation dimension (Y1)
        rvers      TYPE acdocu-rvers,     " Consolidation version
        ryear      TYPE acdocu-ryear,     " Fiscal year
        poper      TYPE acdocu-poper,     " Posting period
        rbunit     TYPE RANGE OF acdocu-rbunit,  " Consolidation unit(s)
        docty      TYPE acdocu-docty,     " Document type to be reversed
        test_run   TYPE abap_bool,
      END OF ty_selection,

      BEGIN OF ty_doc,
        rldnr  TYPE acdocu-rldnr,
        rdimen TYPE acdocu-rdimen,
        ryear  TYPE acdocu-ryear,
        docct  TYPE acdocu-docct,
        docnr  TYPE acdocu-docnr,
        rvers  TYPE acdocu-rvers,
        poper  TYPE acdocu-poper,
        rbunit TYPE acdocu-rbunit,
        docty  TYPE acdocu-docty,
      END OF ty_doc,
      tt_doc TYPE SORTED TABLE OF ty_doc
             WITH UNIQUE KEY rldnr rdimen ryear docct docnr.

    METHODS constructor
      IMPORTING is_selection TYPE ty_selection.

    "! Selects the documents and reverses them.
    "! @parameter et_messages | messages for the task log
    "! @parameter rv_ok       | abap_true = no error
    METHODS execute
      EXPORTING et_messages  TYPE bapiret2_t
      RETURNING VALUE(rv_ok) TYPE abap_bool.

    METHODS get_documents
      RETURNING VALUE(rt_doc) TYPE tt_doc.

  PRIVATE SECTION.
    "-- Selection-screen element names of FICRVS00 (CXA0).
    "-- !!! VERIFY each name in SE38 for your release !!!
    CONSTANTS:
      BEGIN OF gc_sel,
        version  TYPE rsscr_name VALUE 'P_RVERS',   " Version
        ryear    TYPE rsscr_name VALUE 'P_RYEAR',   " Fiscal year
        poper    TYPE rsscr_name VALUE 'P_POPER',   " Period
        dimen    TYPE rsscr_name VALUE 'P_RDIMEN',  " Dimension
        cons_unit TYPE rsscr_name VALUE 'S_RBUNIT', " Cons. unit (select-option)
        doc_type TYPE rsscr_name VALUE 'S_DOCTY',   " Document type (select-option)
        doc_nr   TYPE rsscr_name VALUE 'S_DOCNR',   " Document number (select-option)
        test_run TYPE rsscr_name VALUE 'P_TEST',    " Test run flag
      END OF gc_sel,
      gc_report TYPE syrepid VALUE 'FICRVS00'.

    DATA ms_sel TYPE ty_selection.

    METHODS build_seltab
      IMPORTING it_doc           TYPE tt_doc
      RETURNING VALUE(rt_seltab) TYPE rsparams_tt.

    METHODS submit_mass_reversal
      IMPORTING it_seltab   TYPE rsparams_tt
      CHANGING  ct_messages TYPE bapiret2_t
      RETURNING VALUE(rv_ok) TYPE abap_bool.

    METHODS add_msg
      IMPORTING iv_type     TYPE symsgty
                iv_text     TYPE csequence
      CHANGING  ct_messages TYPE bapiret2_t.
ENDCLASS.



CLASS zcl_fincs_gje_reversal IMPLEMENTATION.

  METHOD constructor.
    ms_sel = is_selection.
  ENDMETHOD.


  METHOD execute.
    CLEAR et_messages.

    DATA(lt_doc) = get_documents( ).

    IF lt_doc IS INITIAL.
      add_msg( EXPORTING iv_type = 'S'
                         iv_text = |No unreversed documents of type { ms_sel-docty } found|
               CHANGING  ct_messages = et_messages ).
      rv_ok = abap_true.
      RETURN.
    ENDIF.

    add_msg( EXPORTING iv_type = 'I'
                       iv_text = |{ lines( lt_doc ) } document(s) of type { ms_sel-docty } selected for reversal|
             CHANGING  ct_messages = et_messages ).

    rv_ok = submit_mass_reversal( EXPORTING it_seltab   = build_seltab( lt_doc )
                                  CHANGING  ct_messages = et_messages ).
  ENDMETHOD.


  METHOD get_documents.
    " Original (not yet reversed, not itself a reversal) documents of the
    " given document type.
    " !!! VERIFY in SE11: field names for "reversed" / "reversal of" in ACDOCU
    " !!! (e.g. REVERSED / REVDOCNR / REFDOCNR). Adjust the WHERE clause.
    SELECT DISTINCT rldnr, rdimen, ryear, docct, docnr,
                    rvers, poper, rbunit, docty
      FROM acdocu
      WHERE rldnr  =  @ms_sel-rldnr
        AND rdimen =  @ms_sel-rdimen
        AND rvers  =  @ms_sel-rvers
        AND ryear  =  @ms_sel-ryear
        AND poper  =  @ms_sel-poper
        AND rbunit IN @ms_sel-rbunit
        AND docty  =  @ms_sel-docty
*       AND reversed = @abap_false          " <-- uncomment / rename after SE11 check
*       AND revdocnr = @space               " <-- doc is not itself a reversal
      INTO CORRESPONDING FIELDS OF TABLE @rt_doc.
  ENDMETHOD.


  METHOD build_seltab.
    rt_seltab = VALUE #(
      ( selname = gc_sel-version kind = 'P' low = ms_sel-rvers )
      ( selname = gc_sel-ryear   kind = 'P' low = ms_sel-ryear )
      ( selname = gc_sel-poper   kind = 'P' low = ms_sel-poper )
      ( selname = gc_sel-dimen   kind = 'P' low = ms_sel-rdimen )
      ( selname = gc_sel-test_run kind = 'P' low = ms_sel-test_run )
      ( selname = gc_sel-doc_type kind = 'S' sign = 'I' option = 'EQ' low = ms_sel-docty ) ).

    LOOP AT ms_sel-rbunit INTO DATA(ls_bunit).
      APPEND VALUE #( selname = gc_sel-cons_unit kind = 'S'
                      sign = ls_bunit-sign option = ls_bunit-option
                      low  = ls_bunit-low  high   = ls_bunit-high ) TO rt_seltab.
    ENDLOOP.

    " Pass the exact document numbers, so only what we selected is reversed
    LOOP AT it_doc INTO DATA(ls_doc).
      APPEND VALUE #( selname = gc_sel-doc_nr kind = 'S'
                      sign = 'I' option = 'EQ' low = ls_doc-docnr ) TO rt_seltab.
    ENDLOOP.
  ENDMETHOD.


  METHOD submit_mass_reversal.
    DATA: lt_list  TYPE STANDARD TABLE OF abaplist,
          lt_ascii TYPE STANDARD TABLE OF char255.

    rv_ok = abap_true.

    " Run CXA0 logic and capture its output list instead of displaying it
    SUBMIT (gc_report)
      WITH SELECTION-TABLE it_seltab
      EXPORTING LIST TO MEMORY
      AND RETURN.

    CALL FUNCTION 'LIST_FROM_MEMORY'
      TABLES     listobject = lt_list
      EXCEPTIONS not_found  = 1
                 OTHERS     = 2.
    IF sy-subrc = 0.
      CALL FUNCTION 'LIST_TO_ASCI'
        TABLES     listasci   = lt_ascii
                   listobject = lt_list
        EXCEPTIONS OTHERS     = 1.
      CALL FUNCTION 'LIST_FREE_MEMORY'
        TABLES listobject = lt_list.

      " Copy the CXA0 result list into the task log
      LOOP AT lt_ascii INTO DATA(lv_line) WHERE table_line IS NOT INITIAL.
        add_msg( EXPORTING iv_type = 'I' iv_text = condense( lv_line )
                 CHANGING  ct_messages = ct_messages ).
      ENDLOOP.
    ENDIF.

    " Re-check: anything of this doc type still unreversed?
    IF ms_sel-test_run = abap_false AND get_documents( ) IS NOT INITIAL.
      rv_ok = abap_false.
      add_msg( EXPORTING iv_type = 'E'
                         iv_text = |Some documents of type { ms_sel-docty } could not be reversed - check CXA0 log|
               CHANGING  ct_messages = ct_messages ).
    ELSE.
      add_msg( EXPORTING iv_type = 'S'
                         iv_text = COND #( WHEN ms_sel-test_run = abap_true
                                           THEN |Test run completed, nothing posted|
                                           ELSE |Reversal of document type { ms_sel-docty } completed| )
               CHANGING  ct_messages = ct_messages ).
    ENDIF.
  ENDMETHOD.


  METHOD add_msg.
    DATA(lv_text) = CONV bapi_msg( iv_text ).
    APPEND VALUE #( type       = iv_type
                    id         = '00'
                    number     = '001'
                    message    = lv_text
                    message_v1 = lv_text+0(50)
                    message_v2 = lv_text+50(50)
                    message_v3 = lv_text+100(50)
                    message_v4 = lv_text+150(50) ) TO ct_messages.
  ENDMETHOD.

ENDCLASS.
