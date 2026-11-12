"! <p class="shorttext synchronized">Group journal entry reversal helper (used by custom DM task)</p>
*
* Reverses group journal entries (ACDOCU) of a given consolidation document type
* for one version / fiscal year / period / consolidation unit selection.
*
* Reversal is delegated to the SAP standard mass-reversal program FICRVS00
* (transaction CXA0), so all standard checks, number ranges, reversal flags
* and document links are applied exactly as in CXA0.
*
* FICRVS00 selection names taken from the system (SE38 - Selection Texts).
* FICRVS00 has NO test-run flag: in test mode this class only lists the
* documents and does not call CXA0.
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
        ritclg     TYPE fc_itclg,         " Consolidation chart of accounts
        rcongr     TYPE fc_congr,         " Consolidation group (optional)
        rbunit     TYPE RANGE OF acdocu-rbunit,  " Consolidation unit(s)
        docty      TYPE acdocu-docty,     " Document type to be reversed
        rvs_ryear  TYPE acdocu-ryear,     " Reversal year   (initial = RYEAR)
        rvs_poper  TYPE acdocu-poper,     " Reversal period (initial = POPER)
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
    "-- Selection-screen element names of FICRVS00 (CXA0), from SE38
    CONSTANTS:
      BEGIN OF gc_sel,
        version     TYPE rsscr_name VALUE 'PA_VERS',   " P  Version
        dimen       TYPE rsscr_name VALUE 'PA_DIMEN',  " P  Dimension
        coa         TYPE rsscr_name VALUE 'PA_ITCLG',  " P  Consolidation COA
        congr       TYPE rsscr_name VALUE 'PA_CONGR',  " P  Consolidation group
        rvs_year    TYPE rsscr_name VALUE 'PA_RVSYR',  " P  Reversal year
        rvs_period  TYPE rsscr_name VALUE 'PA_RVSPE',  " P  Reversal period
        no_reversed TYPE rsscr_name VALUE 'PA_NORVD',  " P  No reversed documents
        no_reversal TYPE rsscr_name VALUE 'PA_NORVL',  " P  No reversal documents
        ryear       TYPE rsscr_name VALUE 'SO_YEAR',   " S  Fiscal year
        period      TYPE rsscr_name VALUE 'SO_PERID',  " S  Period
        cons_unit   TYPE rsscr_name VALUE 'SO_BUNIT',  " S  Consolidation unit
        doc_type    TYPE rsscr_name VALUE 'SO_DOCTY',  " S  Document type
        doc_nr      TYPE rsscr_name VALUE 'SO_DOCNR',  " S  Document number
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

    "! Safety check of GC_SEL against FICRVS00's real selection screen.
    METHODS check_selnames
      CHANGING  ct_messages  TYPE bapiret2_t
      RETURNING VALUE(rv_ok) TYPE abap_bool.

    METHODS add_msg
      IMPORTING iv_type     TYPE symsgty
                iv_text     TYPE csequence
      CHANGING  ct_messages TYPE bapiret2_t.
ENDCLASS.



CLASS zcl_fincs_gje_reversal IMPLEMENTATION.

  METHOD constructor.
    ms_sel = is_selection.
    " Default: reverse into the same year / period as the original document
    IF ms_sel-rvs_ryear IS INITIAL.
      ms_sel-rvs_ryear = ms_sel-ryear.
    ENDIF.
    IF ms_sel-rvs_poper IS INITIAL.
      ms_sel-rvs_poper = ms_sel-poper.
    ENDIF.
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

    "-- Test run: CXA0 has no test flag -> only list, do not post
    IF ms_sel-test_run = abap_true.
      LOOP AT lt_doc INTO DATA(ls_doc).
        add_msg( EXPORTING iv_type = 'I'
                           iv_text = |Would reverse: unit { ls_doc-rbunit } doc { ls_doc-docnr } ({ ls_doc-ryear }/{ ls_doc-poper })|
                 CHANGING  ct_messages = et_messages ).
      ENDLOOP.
      add_msg( EXPORTING iv_type = 'S'
                         iv_text = |Test run - nothing posted|
               CHANGING  ct_messages = et_messages ).
      rv_ok = abap_true.
      RETURN.
    ENDIF.

    IF check_selnames( CHANGING ct_messages = et_messages ) = abap_false.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    rv_ok = submit_mass_reversal( EXPORTING it_seltab   = build_seltab( lt_doc )
                                  CHANGING  ct_messages = et_messages ).
  ENDMETHOD.


  METHOD get_documents.
    " Original (not yet reversed, not itself a reversal) documents of the
    " given document type.
    "   RVSDOCNR = number of the reversal document (filled => already reversed)
    "   ORNDOCNR = number of the reversed document (filled => this IS a reversal)
    SELECT DISTINCT rldnr, rdimen, ryear, docct, docnr,
                    rvers, poper, rbunit, docty
      FROM acdocu
      WHERE rldnr    =  @ms_sel-rldnr
        AND rdimen   =  @ms_sel-rdimen
        AND rvers    =  @ms_sel-rvers
        AND ryear    =  @ms_sel-ryear
        AND poper    =  @ms_sel-poper
        AND rbunit   IN @ms_sel-rbunit
        AND docty    =  @ms_sel-docty
        AND rvsdocnr =  @space
        AND orndocnr =  @space
      INTO CORRESPONDING FIELDS OF TABLE @rt_doc.
  ENDMETHOD.


  METHOD build_seltab.
    "-- Parameters (KIND = 'P')
    rt_seltab = VALUE #(
      ( selname = gc_sel-version     kind = 'P' low = ms_sel-rvers )
      ( selname = gc_sel-dimen       kind = 'P' low = ms_sel-rdimen )
      ( selname = gc_sel-coa         kind = 'P' low = ms_sel-ritclg )
      ( selname = gc_sel-rvs_year    kind = 'P' low = ms_sel-rvs_ryear )
      ( selname = gc_sel-rvs_period  kind = 'P' low = ms_sel-rvs_poper )
      ( selname = gc_sel-no_reversed kind = 'P' low = abap_true )   " skip already reversed
      ( selname = gc_sel-no_reversal kind = 'P' low = abap_true ) ).  " skip reversal docs

    IF ms_sel-rcongr IS NOT INITIAL.
      APPEND VALUE #( selname = gc_sel-congr kind = 'P' low = ms_sel-rcongr ) TO rt_seltab.
    ENDIF.

    "-- Select-options (KIND = 'S')
    APPEND VALUE #( selname = gc_sel-ryear    kind = 'S' sign = 'I' option = 'EQ' low = ms_sel-ryear ) TO rt_seltab.
    APPEND VALUE #( selname = gc_sel-period   kind = 'S' sign = 'I' option = 'EQ' low = ms_sel-poper ) TO rt_seltab.
    APPEND VALUE #( selname = gc_sel-doc_type kind = 'S' sign = 'I' option = 'EQ' low = ms_sel-docty ) TO rt_seltab.

    LOOP AT ms_sel-rbunit INTO DATA(ls_bunit).
      APPEND VALUE #( selname = gc_sel-cons_unit kind = 'S'
                      sign = ls_bunit-sign option = ls_bunit-option
                      low  = ls_bunit-low  high   = ls_bunit-high ) TO rt_seltab.
    ENDLOOP.

    " Exact document numbers, so only what we selected is reversed
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
    IF get_documents( ) IS NOT INITIAL.
      rv_ok = abap_false.
      add_msg( EXPORTING iv_type = 'E'
                         iv_text = |Some documents of type { ms_sel-docty } could not be reversed - check CXA0 log|
               CHANGING  ct_messages = ct_messages ).
    ELSE.
      add_msg( EXPORTING iv_type = 'S'
                         iv_text = |Reversal of document type { ms_sel-docty } completed|
               CHANGING  ct_messages = ct_messages ).
    ENDIF.
  ENDMETHOD.


  METHOD check_selnames.
    DATA lt_info TYPE STANDARD TABLE OF rsel_info.

    rv_ok = abap_true.

    CALL FUNCTION 'RS_REPORTSELECTIONS_INFO'
      EXPORTING  report              = gc_report
      TABLES     field_info          = lt_info
      EXCEPTIONS no_selections       = 1
                 report_not_existent = 2
                 subroutine_pool     = 3
                 OTHERS              = 4.
    IF sy-subrc <> 0.
      " Cannot verify - names are taken from SE38, so continue
      RETURN.
    ENDIF.

    DATA(lt_wanted) = VALUE string_table(
      ( CONV #( gc_sel-version ) )     ( CONV #( gc_sel-dimen ) )
      ( CONV #( gc_sel-coa ) )         ( CONV #( gc_sel-rvs_year ) )
      ( CONV #( gc_sel-rvs_period ) )  ( CONV #( gc_sel-no_reversed ) )
      ( CONV #( gc_sel-no_reversal ) ) ( CONV #( gc_sel-ryear ) )
      ( CONV #( gc_sel-period ) )      ( CONV #( gc_sel-cons_unit ) )
      ( CONV #( gc_sel-doc_type ) )    ( CONV #( gc_sel-doc_nr ) ) ).

    LOOP AT lt_wanted INTO DATA(lv_name).
      IF NOT line_exists( lt_info[ name = lv_name ] ).
        rv_ok = abap_false.
        add_msg( EXPORTING iv_type = 'E'
                           iv_text = |{ lv_name } is not a selection field of { gc_report }|
                 CHANGING  ct_messages = ct_messages ).
      ENDIF.
    ENDLOOP.
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
