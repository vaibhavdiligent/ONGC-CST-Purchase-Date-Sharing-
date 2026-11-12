"! <p class="shorttext synchronized">Post inverse of ZR docs as ZS docs (custom DM task)</p>
* Rule: pick all documents of the source type (ZR, PL01) of a consolidation
* unit / version / year / period and post the inverse of each of them in the
* SAME period with the target type (ZS, PL10). The ZR documents stay as they are.
*
* Re-run safe: ZS documents already posted for the same selection are first
* reversed via CXA0 (FICRVS00), then all ZS documents are posted again.
*
* Test run: nothing is reversed or posted, the log lists what would happen.
*
* POST_DOCUMENT is the only place that posts. It must call the group journal
* entry posting API of the system (the one used by app Post Group Journal
* Entries / OData FINCS_JRNLENTR_POST_SRV) - see the TODO there.
CLASS zcl_fincs_gje_reversal DEFINITION
  PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_selection,
        rldnr        TYPE acdocu-rldnr,     " Consolidation ledger
        rdimen       TYPE acdocu-rdimen,    " Consolidation dimension
        rvers        TYPE acdocu-rvers,     " Consolidation version
        ryear        TYPE acdocu-ryear,     " Fiscal year
        poper        TYPE acdocu-poper,     " Period
        ritclg       TYPE fc_itclg,         " Consolidation chart of accounts
        rcongr       TYPE fc_congr,         " Consolidation group (optional)
        rbunit       TYPE RANGE OF acdocu-rbunit,  " Consolidation unit(s)
        docty        TYPE acdocu-docty,     " Source document type    (ZR)
        docty_target TYPE acdocu-docty,     " Target document type    (ZS)
        plevl_target TYPE acdocu-plevl,     " Target posting level    (10)
        test_run     TYPE abap_bool,
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
      tt_doc  TYPE SORTED TABLE OF ty_doc
              WITH UNIQUE KEY rldnr rdimen ryear docct docnr,
      tt_line TYPE STANDARD TABLE OF acdocu WITH EMPTY KEY.

    METHODS constructor
      IMPORTING is_selection TYPE ty_selection.

    "! @parameter et_messages | messages for the task log
    "! @parameter rv_ok       | abap_true = no error
    METHODS execute
      EXPORTING et_messages  TYPE bapiret2_t
      RETURNING VALUE(rv_ok) TYPE abap_bool.

  PRIVATE SECTION.
    "-- Selection-screen element names of FICRVS00 (CXA0), from SE38
    CONSTANTS:
      BEGIN OF gc_sel,
        version     TYPE rsscr_name VALUE 'PA_VERS',
        dimen       TYPE rsscr_name VALUE 'PA_DIMEN',
        coa         TYPE rsscr_name VALUE 'PA_ITCLG',
        congr       TYPE rsscr_name VALUE 'PA_CONGR',
        rvs_year    TYPE rsscr_name VALUE 'PA_RVSYR',
        rvs_period  TYPE rsscr_name VALUE 'PA_RVSPE',
        no_reversed TYPE rsscr_name VALUE 'PA_NORVD',
        no_reversal TYPE rsscr_name VALUE 'PA_NORVL',
        ryear       TYPE rsscr_name VALUE 'SO_YEAR',
        period      TYPE rsscr_name VALUE 'SO_PERID',
        cons_unit   TYPE rsscr_name VALUE 'SO_BUNIT',
        doc_type    TYPE rsscr_name VALUE 'SO_DOCTY',
        doc_nr      TYPE rsscr_name VALUE 'SO_DOCNR',
      END OF gc_sel,
      gc_report TYPE syrepid VALUE 'FICRVS00'.

    DATA ms_sel TYPE ty_selection.

    "! Not reversed, not-a-reversal documents of a document type
    METHODS get_documents
      IMPORTING iv_docty      TYPE acdocu-docty
      RETURNING VALUE(rt_doc) TYPE tt_doc.

    "! Lines of one source document, inverted and changed to target type/level
    METHODS build_target_lines
      IMPORTING is_doc         TYPE ty_doc
      RETURNING VALUE(rt_line) TYPE tt_line.

    "! Posts one group journal entry
    METHODS post_document
      IMPORTING is_source   TYPE ty_doc
                it_line     TYPE tt_line
      CHANGING  ct_messages TYPE bapiret2_t
      RETURNING VALUE(rv_ok) TYPE abap_bool.

    "! Reverses documents via CXA0
    METHODS reverse_via_cxa0
      IMPORTING it_doc      TYPE tt_doc
                iv_docty    TYPE acdocu-docty
      CHANGING  ct_messages TYPE bapiret2_t
      RETURNING VALUE(rv_ok) TYPE abap_bool.

    METHODS build_seltab
      IMPORTING it_doc           TYPE tt_doc
                iv_docty         TYPE acdocu-docty
      RETURNING VALUE(rt_seltab) TYPE rsparams_tt.

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
    rv_ok = abap_true.

    DATA(lt_src) = get_documents( ms_sel-docty ).          " ZR documents
    DATA(lt_old) = get_documents( ms_sel-docty_target ).   " ZS already posted

    add_msg( EXPORTING iv_type = 'I'
                       iv_text = |{ lines( lt_src ) } { ms_sel-docty } doc(s) found, | &&
                                 |{ lines( lt_old ) } existing { ms_sel-docty_target } doc(s)|
             CHANGING  ct_messages = et_messages ).

    "-- Test run: only list ------------------------------------------------
    IF ms_sel-test_run = abap_true.
      LOOP AT lt_old INTO DATA(ls_old).
        add_msg( EXPORTING iv_type = 'I'
                           iv_text = |Would reverse { ms_sel-docty_target } doc { ls_old-docnr } (unit { ls_old-rbunit })|
                 CHANGING  ct_messages = et_messages ).
      ENDLOOP.
      LOOP AT lt_src INTO DATA(ls_src).
        add_msg( EXPORTING iv_type = 'I'
                           iv_text = |Would post { ms_sel-docty_target } for { ms_sel-docty } doc { ls_src-docnr } | &&
                                     |(unit { ls_src-rbunit }, { lines( build_target_lines( ls_src ) ) } lines)|
                 CHANGING  ct_messages = et_messages ).
      ENDLOOP.
      add_msg( EXPORTING iv_type = 'S' iv_text = |Test run - nothing posted|
               CHANGING  ct_messages = et_messages ).
      RETURN.
    ENDIF.

    "-- 1. Re-run: reverse ZS documents posted earlier ----------------------
    IF lt_old IS NOT INITIAL.
      IF reverse_via_cxa0( EXPORTING it_doc   = lt_old
                                     iv_docty = ms_sel-docty_target
                           CHANGING  ct_messages = et_messages ) = abap_false.
        rv_ok = abap_false.
        RETURN.                           " do not post twice
      ENDIF.
    ENDIF.

    "-- 2. Post one ZS document per ZR document ----------------------------
    LOOP AT lt_src INTO ls_src.
      DATA(lt_line) = build_target_lines( ls_src ).
      IF lt_line IS INITIAL.
        CONTINUE.
      ENDIF.
      IF post_document( EXPORTING is_source   = ls_src
                                  it_line     = lt_line
                        CHANGING  ct_messages = et_messages ) = abap_false.
        rv_ok = abap_false.
      ENDIF.
    ENDLOOP.

    add_msg( EXPORTING iv_type = COND #( WHEN rv_ok = abap_true THEN 'S' ELSE 'E' )
                       iv_text = COND #( WHEN rv_ok = abap_true
                                         THEN |{ ms_sel-docty_target } posting completed|
                                         ELSE |{ ms_sel-docty_target } posting completed with errors| )
             CHANGING  ct_messages = et_messages ).
  ENDMETHOD.


  METHOD get_documents.
    "   RVSDOCNR filled => already reversed, ORNDOCNR filled => is a reversal
    SELECT DISTINCT rldnr, rdimen, ryear, docct, docnr,
                    rvers, poper, rbunit, docty
      FROM acdocu
      WHERE rldnr    =  @ms_sel-rldnr
        AND rdimen   =  @ms_sel-rdimen
        AND rvers    =  @ms_sel-rvers
        AND ryear    =  @ms_sel-ryear
        AND poper    =  @ms_sel-poper
        AND rbunit   IN @ms_sel-rbunit
        AND docty    =  @iv_docty
        AND rvsdocnr =  @space
        AND orndocnr =  @space
      INTO CORRESPONDING FIELDS OF TABLE @rt_doc.
  ENDMETHOD.


  METHOD build_target_lines.
    SELECT * FROM acdocu
      WHERE rldnr  = @is_doc-rldnr
        AND rdimen = @is_doc-rdimen
        AND ryear  = @is_doc-ryear
        AND docct  = @is_doc-docct
        AND docnr  = @is_doc-docnr
      INTO TABLE @rt_line.

    LOOP AT rt_line ASSIGNING FIELD-SYMBOL(<ls_line>).
      " same unit, partner, FS item, subitem, period ... only type/level/sign change
      <ls_line>-docty = ms_sel-docty_target.
      <ls_line>-plevl = ms_sel-plevl_target.
      " invert amounts: transaction / local / group currency / quantity
      <ls_line>-tsl = - <ls_line>-tsl.
      <ls_line>-hsl = - <ls_line>-hsl.
      <ls_line>-ksl = - <ls_line>-ksl.
      <ls_line>-msl = - <ls_line>-msl.
      " new document: no number / reversal links yet
      CLEAR: <ls_line>-docnr, <ls_line>-rvsdocnr, <ls_line>-orndocnr, <ls_line>-revyear.
    ENDLOOP.
  ENDMETHOD.


  METHOD post_document.
    "======================================================================
    " TODO - connect the group journal entry posting API here.
    "   IT_LINE = the ZS lines (ACDOCU layout, amounts already inverted).
    "   Map them to the API's header/item structure:
    "     header: RLDNR, RDIMEN, RVERS, RYEAR, POPER, DOCTY (ZS), BUDAT, text
    "     items : RBUNIT, RITCLG, RACCT (FS item), RBPTR (partner), SITYP/SUBIT
    "             (subitem), RTCUR, TSL, HSL, KSL, MSL, ...
    "   Document text suggestion: |Rev { is_source-docty } { is_source-docnr }|
    "   On success: add_msg( 'S', 'ZS doc <no> posted for ZR doc <no>' ),
    "   rv_ok = abap_true.  On error: copy the API messages, rv_ok = abap_false.
    "======================================================================
    add_msg( EXPORTING iv_type = 'E'
                       iv_text = |Posting API not connected yet - { ms_sel-docty } doc { is_source-docnr } not posted|
             CHANGING  ct_messages = ct_messages ).
    rv_ok = abap_false.
  ENDMETHOD.


  METHOD reverse_via_cxa0.
    DATA: lt_list  TYPE STANDARD TABLE OF abaplist,
          lt_ascii TYPE STANDARD TABLE OF char255.

    DATA(lt_seltab) = build_seltab( it_doc = it_doc iv_docty = iv_docty ).

    SUBMIT (gc_report)
      WITH SELECTION-TABLE lt_seltab
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
      LOOP AT lt_ascii INTO DATA(lv_line) WHERE table_line IS NOT INITIAL.
        add_msg( EXPORTING iv_type = 'I' iv_text = condense( lv_line )
                 CHANGING  ct_messages = ct_messages ).
      ENDLOOP.
    ENDIF.

    " Check: are the old documents reversed now?
    rv_ok = xsdbool( get_documents( iv_docty ) IS INITIAL ).
    IF rv_ok = abap_false.
      add_msg( EXPORTING iv_type = 'E'
                         iv_text = |Old { iv_docty } documents could not be reversed (CXA0) - nothing posted|
               CHANGING  ct_messages = ct_messages ).
    ENDIF.
  ENDMETHOD.


  METHOD build_seltab.
    rt_seltab = VALUE #(
      ( selname = gc_sel-version     kind = 'P' low = ms_sel-rvers )
      ( selname = gc_sel-dimen       kind = 'P' low = ms_sel-rdimen )
      ( selname = gc_sel-coa         kind = 'P' low = ms_sel-ritclg )
      ( selname = gc_sel-rvs_year    kind = 'P' low = ms_sel-ryear )
      ( selname = gc_sel-rvs_period  kind = 'P' low = ms_sel-poper )
      ( selname = gc_sel-no_reversed kind = 'P' low = abap_true )
      ( selname = gc_sel-no_reversal kind = 'P' low = abap_true )
      ( selname = gc_sel-ryear    kind = 'S' sign = 'I' option = 'EQ' low = ms_sel-ryear )
      ( selname = gc_sel-period   kind = 'S' sign = 'I' option = 'EQ' low = ms_sel-poper )
      ( selname = gc_sel-doc_type kind = 'S' sign = 'I' option = 'EQ' low = iv_docty ) ).

    IF ms_sel-rcongr IS NOT INITIAL.
      APPEND VALUE #( selname = gc_sel-congr kind = 'P' low = ms_sel-rcongr ) TO rt_seltab.
    ENDIF.

    LOOP AT ms_sel-rbunit INTO DATA(ls_bunit).
      APPEND VALUE #( selname = gc_sel-cons_unit kind = 'S'
                      sign = ls_bunit-sign option = ls_bunit-option
                      low  = ls_bunit-low  high   = ls_bunit-high ) TO rt_seltab.
    ENDLOOP.

    LOOP AT it_doc INTO DATA(ls_doc).
      APPEND VALUE #( selname = gc_sel-doc_nr kind = 'S'
                      sign = 'I' option = 'EQ' low = ls_doc-docnr ) TO rt_seltab.
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
