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
* Posting: SAP consolidation posting logic, function group FC15
*   FC_DOCUMENT_ANALYZE2 (check / derive)  ->  FC_DOCUMENT_POST2 (post)
* Line structure FC05_S_PDOC has the same field names as ACDOCU
* (RBUNIT, RBUPTR, RITEM, SITYP, SUBIT, RTCUR, TSL, HSL, KSL, MSL, ...).
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
      tt_line TYPE fc05_t_pdoc.

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

    "! Posts one group journal entry (FC_DOCUMENT_ANALYZE2 + FC_DOCUMENT_POST2)
    METHODS post_document
      IMPORTING is_source   TYPE ty_doc
                it_line     TYPE tt_line
      CHANGING  ct_messages TYPE bapiret2_t
      RETURNING VALUE(rv_ok) TYPE abap_bool.

    "! Copies FC05 messages to the log; false if an error message is included
    METHODS collect_fc_messages
      IMPORTING it_msg      TYPE fc05_t_message
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
    DATA lt_acdocu TYPE STANDARD TABLE OF acdocu WITH EMPTY KEY.

    SELECT * FROM acdocu
      WHERE rldnr  = @is_doc-rldnr
        AND rdimen = @is_doc-rdimen
        AND ryear  = @is_doc-ryear
        AND docct  = @is_doc-docct
        AND docnr  = @is_doc-docnr
      ORDER BY docln
      INTO TABLE @lt_acdocu.

    " Same unit, partner, FS item, subitem, currency ... (copied by name),
    " amounts inverted. Document type / posting level come from the header.
    LOOP AT lt_acdocu INTO DATA(ls_acdocu).
      DATA(ls_line) = CORRESPONDING fc05_s_pdoc( ls_acdocu ).
      ls_line-tsl = - ls_line-tsl.
      ls_line-hsl = - ls_line-hsl.
      ls_line-ksl = - ls_line-ksl.
      ls_line-msl = - ls_line-msl.
      APPEND ls_line TO rt_line.
    ENDLOOP.
  ENDMETHOD.


  METHOD post_document.
    DATA: lt_doc       TYPE fc05_t_pdoc,
          lt_head      TYPE fc05_t_pdoc_head,
          lt_msg       TYPE fc05_t_message,
          ls_container TYPE fc05_s_pdoc_container,
          ls_control   TYPE fc05_s_da_control.

    rv_ok  = abap_true.
    lt_doc = it_line.

    "-- 1. Check / derive the document lines --------------------------------
    CALL FUNCTION 'FC_DOCUMENT_ANALYZE2'
      EXPORTING
        e_rldnr      = ms_sel-rldnr
        e_itclg      = ms_sel-ritclg
        e_dimen      = ms_sel-rdimen
        e_docty      = ms_sel-docty_target
        e_rvers      = ms_sel-rvers
        e_ryear      = ms_sel-ryear
        e_perid      = ms_sel-poper
        es_control   = ls_control
      IMPORTING
        it_message   = lt_msg
        is_container = ls_container
      CHANGING
        ct_doc       = lt_doc.

    IF collect_fc_messages( EXPORTING it_msg = lt_msg CHANGING ct_messages = ct_messages ) = abap_false
       OR lt_doc IS INITIAL.
      add_msg( EXPORTING iv_type = 'E'
                         iv_text = |{ ms_sel-docty_target } for { ms_sel-docty } doc { is_source-docnr }: check failed, not posted|
               CHANGING  ct_messages = ct_messages ).
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    "-- 2. Header: one document = all lines ---------------------------------
    APPEND VALUE #( docty  = ms_sel-docty_target
                    s_cont = ls_container
                    rcongr = ms_sel-rcongr
                    sgtxt  = |Rev { ms_sel-docty } { is_source-docnr }|
                    from   = 1
                    to     = lines( lt_doc ) ) TO lt_head.

    "-- 3. Post ---------------------------------------------------------------
    CLEAR lt_msg.
    CALL FUNCTION 'FC_DOCUMENT_POST2'
      EXPORTING
        e_rldnr    = ms_sel-rldnr
        e_itclg    = ms_sel-ritclg
        e_dimen    = ms_sel-rdimen
        e_rvers    = ms_sel-rvers
        e_ryear    = ms_sel-ryear
        e_perid    = ms_sel-poper
        es_control = ls_control
        et_doc     = lt_doc
      IMPORTING
        it_message = lt_msg
      CHANGING
        ct_head    = lt_head.

    IF collect_fc_messages( EXPORTING it_msg = lt_msg CHANGING ct_messages = ct_messages ) = abap_false.
      ROLLBACK WORK.
      add_msg( EXPORTING iv_type = 'E'
                         iv_text = |{ ms_sel-docty_target } for { ms_sel-docty } doc { is_source-docnr }: posting failed|
               CHANGING  ct_messages = ct_messages ).
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    COMMIT WORK AND WAIT.

    DATA(lv_new_docnr) = VALUE belnr_d( lt_head[ 1 ]-s_docnr-docnr OPTIONAL ).
    add_msg( EXPORTING iv_type = 'S'
                       iv_text = |{ ms_sel-docty_target } doc { lv_new_docnr } posted for { ms_sel-docty } doc { is_source-docnr }|
             CHANGING  ct_messages = ct_messages ).
  ENDMETHOD.


  METHOD collect_fc_messages.
    " FC05_T_MESSAGE line layout is read generically (MSGTY/MSGID/MSGNO/MSGV1-4)
    rv_ok = abap_true.

    LOOP AT it_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).
      ASSIGN COMPONENT 'MSGTY' OF STRUCTURE <ls_msg> TO FIELD-SYMBOL(<lv_ty>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT 'MSGID' OF STRUCTURE <ls_msg> TO FIELD-SYMBOL(<lv_id>).
      ASSIGN COMPONENT 'MSGNO' OF STRUCTURE <ls_msg> TO FIELD-SYMBOL(<lv_no>).
      ASSIGN COMPONENT 'MSGV1' OF STRUCTURE <ls_msg> TO FIELD-SYMBOL(<lv_v1>).
      ASSIGN COMPONENT 'MSGV2' OF STRUCTURE <ls_msg> TO FIELD-SYMBOL(<lv_v2>).
      ASSIGN COMPONENT 'MSGV3' OF STRUCTURE <ls_msg> TO FIELD-SYMBOL(<lv_v3>).
      ASSIGN COMPONENT 'MSGV4' OF STRUCTURE <ls_msg> TO FIELD-SYMBOL(<lv_v4>).
      IF <lv_id> IS NOT ASSIGNED OR <lv_no> IS NOT ASSIGNED OR <lv_v1> IS NOT ASSIGNED
         OR <lv_v2> IS NOT ASSIGNED OR <lv_v3> IS NOT ASSIGNED OR <lv_v4> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      APPEND VALUE #( type       = <lv_ty>
                      id         = <lv_id>
                      number     = <lv_no>
                      message_v1 = <lv_v1>
                      message_v2 = <lv_v2>
                      message_v3 = <lv_v3>
                      message_v4 = <lv_v4> ) TO ct_messages.

      IF <lv_ty> CA 'EAX'.
        rv_ok = abap_false.
      ENDIF.
    ENDLOOP.
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
