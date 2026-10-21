*&---------------------------------------------------------------------*
*& Report  ZJVA_ACDOCA_JV_FIELDS_UPD
*&---------------------------------------------------------------------*
*& Program Title : Update JV fields (VNAME / EGRUP / RECID) in ACDOCA
*&                 from an Excel upload
*&
*& Description   : Fills or corrects the Joint Venture fields of
*&                 existing ACDOCA lines. It covers lines that GJMIGACC
*&                 does not correct, e.g. document-splitting balancing
*&                 lines and asset detail lines with AWITEM = 0.
*&
*&                 Input file ("Upload Format.xlsx", first worksheet):
*&                   A Company Code      -> ACDOCA-RBUKRS
*&                   B Fiscal Year       -> ACDOCA-GJAHR
*&                   C Document Number   -> ACDOCA-BELNR
*&                   D G/L Line Item     -> ACDOCA-DOCLN
*&                   E Joint venture     -> ACDOCA-VNAME (new value)
*&                   F Equity group      -> ACDOCA-EGRUP (new value)
*&                   G Recovery Indicator-> ACDOCA-RECID (new value)
*&                 Row 1 is a header row (checkbox P_HDR).
*&                 Lines are selected with RLDNR = P_RLDNR (default 0L)
*&                 and RRCTY = '0'.
*&
*&                 Test run (default): no update. The ALV shows for every
*&                 file row: Co.Code, Fiscal Year, Document, DOCLN,
*&                 RACCT, PRCTR, WSL, HSL, KSL, the current and the
*&                 proposed JV fields, and the result of the checks.
*&                 Update run: the same checks, then UPDATE ACDOCA per
*&                 line, with a BKPF lock (EFBKPF) and COMMIT WORK per
*&                 document.
*&
*& Rules
*&   - A blank cell in columns E-G means "do not change this field".
*&   - Without P_OVERWR only blank ACDOCA fields are filled. A field
*&     that already has a different value is kept and the line is
*&     flagged with a warning.
*&   - With P_ETYPE the equity type (ETYPE) is set when EGRUP changes.
*&     It is taken from existing ACDOCA lines of the same company code
*&     with the same VNAME + EGRUP (most frequent value).
*&   - With P_CHECK the values are checked against the JV master data
*&     tables named in the constants GC_TAB_* (dynamic SELECT):
*&     unknown VNAME / EGRUP / RECID = error. VNAME+EGRUP not found
*&     together = warning only. If a table or field does not exist in
*&     the system, a warning is issued instead of a check.
*&     Confirm the table names in SE11 (OCP) before go-live.
*&   - Authority: F_BKPF_BUK, activity 03 (test) or 02 (update).
*&
*& IMPORTANT: This program changes ACDOCA directly. It only updates
*& ACDOCA, not BSEG or other tables. Use it only after the functional
*& team has signed off the upload file. Keep the ALV of the update run
*& (or run it in background to get a spool) as the audit log.
*&
*& Selection texts (maintain in SE38 -> Text elements):
*&   P_FILE   Upload file (.xlsx)       P_RLDNR  Ledger
*&   P_HDR    First row is header       P_OVERWR Overwrite existing values
*&   P_ETYPE  Derive equity type        P_TEST   Test run (no update)
*&   P_CHECK  Validate JV master data
*& Text symbols: B01 File, B02 Processing options
*&---------------------------------------------------------------------*
REPORT zjva_acdoca_jv_fields_upd.

*----------------------------------------------------------------------*
* Types
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_input,
         xlrow  TYPE i,
         rbukrs TYPE acdoca-rbukrs,
         gjahr  TYPE acdoca-gjahr,
         belnr  TYPE acdoca-belnr,
         docln  TYPE acdoca-docln,
         vname  TYPE acdoca-vname,
         egrup  TYPE acdoca-egrup,
         recid  TYPE acdoca-recid,
       END OF ty_input,
       ty_t_input TYPE STANDARD TABLE OF ty_input WITH EMPTY KEY.

TYPES: BEGIN OF ty_acd,
         rbukrs TYPE acdoca-rbukrs,
         gjahr  TYPE acdoca-gjahr,
         belnr  TYPE acdoca-belnr,
         docln  TYPE acdoca-docln,
         racct  TYPE acdoca-racct,
         prctr  TYPE acdoca-prctr,
         wsl    TYPE acdoca-wsl,
         rwcur  TYPE acdoca-rwcur,
         hsl    TYPE acdoca-hsl,
         rhcur  TYPE acdoca-rhcur,
         ksl    TYPE acdoca-ksl,
         rkcur  TYPE acdoca-rkcur,
         vname  TYPE acdoca-vname,
         egrup  TYPE acdoca-egrup,
         recid  TYPE acdoca-recid,
         etype  TYPE acdoca-etype,
         buzei  TYPE acdoca-buzei,
         awtyp  TYPE acdoca-awtyp,
       END OF ty_acd.

TYPES: BEGIN OF ty_out,
         light     TYPE c LENGTH 1,          " 1 red, 2 yellow, 3 green
         xlrow     TYPE i,
         rbukrs    TYPE acdoca-rbukrs,
         gjahr     TYPE acdoca-gjahr,
         belnr     TYPE acdoca-belnr,
         docln     TYPE acdoca-docln,
         racct     TYPE acdoca-racct,
         prctr     TYPE acdoca-prctr,
         wsl       TYPE acdoca-wsl,
         rwcur     TYPE acdoca-rwcur,
         hsl       TYPE acdoca-hsl,
         rhcur     TYPE acdoca-rhcur,
         ksl       TYPE acdoca-ksl,
         rkcur     TYPE acdoca-rkcur,
         vname_old TYPE acdoca-vname,
         egrup_old TYPE acdoca-egrup,
         recid_old TYPE acdoca-recid,
         etype_old TYPE acdoca-etype,
         vname_new TYPE acdoca-vname,
         egrup_new TYPE acdoca-egrup,
         recid_new TYPE acdoca-recid,
         etype_new TYPE acdoca-etype,
         changed   TYPE c LENGTH 30,         " fields that will change
         buzei     TYPE acdoca-buzei,
         awtyp     TYPE acdoca-awtyp,
         msgty     TYPE bapi_mtype,
         message   TYPE bapi_msg,
       END OF ty_out,
       ty_t_out TYPE STANDARD TABLE OF ty_out WITH EMPTY KEY.

TYPES: BEGIN OF ty_check,
         tabname TYPE tabname,
         key     TYPE string,
         result  TYPE c LENGTH 1,             " X found, - not found, ? not checkable
       END OF ty_check.

*----------------------------------------------------------------------*
* Constants - master data tables used for the value checks
*----------------------------------------------------------------------*
CONSTANTS: gc_tab_vname TYPE tabname VALUE 'T8JV',   " Joint ventures
           gc_tab_egrup TYPE tabname VALUE 'T8JF',   " Equity groups
           gc_tab_veg   TYPE tabname VALUE 'T8JG',   " Venture / equity group
           gc_tab_recid TYPE tabname VALUE 'T8JJ',   " Recovery indicators
           gc_rrcty     TYPE acdoca-rrcty VALUE '0'.

*----------------------------------------------------------------------*
* Global data
*----------------------------------------------------------------------*
DATA: gt_input  TYPE ty_t_input,
      gt_out    TYPE ty_t_out,
      gt_check  TYPE HASHED TABLE OF ty_check WITH UNIQUE KEY tabname key,
      gt_authok TYPE HASHED TABLE OF acdoca-rbukrs WITH UNIQUE KEY table_line,
      gt_authno TYPE HASHED TABLE OF acdoca-rbukrs WITH UNIQUE KEY table_line.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-b01.
  PARAMETERS: p_file  TYPE localfile OBLIGATORY,
              p_hdr   AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-b02.
  PARAMETERS: p_rldnr  TYPE acdoca-rldnr DEFAULT '0L' OBLIGATORY,
              p_overwr AS CHECKBOX DEFAULT ' ',
              p_etype  AS CHECKBOX DEFAULT 'X',
              p_check  AS CHECKBOX DEFAULT 'X',
              p_test   AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file CHANGING p_file.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  PERFORM read_excel CHANGING gt_input.
  IF gt_input IS INITIAL.
    MESSAGE 'No data lines found in the upload file' TYPE 'S' DISPLAY LIKE 'E' ##NO_TEXT.
    RETURN.
  ENDIF.

  PERFORM check_lines.

  IF p_test IS INITIAL.
    IF sy-batch IS INITIAL.
      DATA: lv_answer   TYPE c LENGTH 1,
            lv_question TYPE c LENGTH 400.
      DATA(lv_count) = REDUCE i( INIT n = 0 FOR o IN gt_out WHERE ( light = '3' ) NEXT n = n + 1 ).
      lv_question = |{ lv_count } ACDOCA line(s) will be changed. Continue?|.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Update ACDOCA'(p01)
          text_question         = lv_question
          text_button_1         = 'Yes'(p02)
          text_button_2         = 'No'(p03)
          default_button        = '2'
          display_cancel_button = space
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          OTHERS                = 1.
      IF sy-subrc <> 0 OR lv_answer <> '1'.
        MESSAGE 'Update cancelled - no data changed' TYPE 'S' ##NO_TEXT.
        RETURN.
      ENDIF.
    ENDIF.
    PERFORM update_acdoca.
  ENDIF.

  PERFORM display_alv.

*&---------------------------------------------------------------------*
*& Form READ_EXCEL - read the first worksheet of the .xlsx file
*&---------------------------------------------------------------------*
FORM read_excel CHANGING ct_input TYPE ty_t_input.
  DATA: lt_bin    TYPE solix_tab,
        lv_len    TYPE i,
        lv_xfile  TYPE xstring,
        lv_fname  TYPE string,
        lv_value  TYPE string,
        ls_input  TYPE ty_input.
  FIELD-SYMBOLS: <lt_sheet> TYPE STANDARD TABLE,
                 <ls_row>   TYPE any,
                 <lv_cell>  TYPE any.

  lv_fname = p_file.
  cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename   = lv_fname
      filetype   = 'BIN'
    IMPORTING
      filelength = lv_len
    CHANGING
      data_tab   = lt_bin
    EXCEPTIONS
      OTHERS     = 1 ).
  IF sy-subrc <> 0.
    MESSAGE |File { lv_fname } could not be read| TYPE 'E' ##NO_TEXT.
  ENDIF.
  lv_xfile = cl_bcs_convert=>solix_to_xstring( it_solix = lt_bin iv_size = lv_len ).

  TRY.
      DATA(lo_xl) = NEW cl_fdt_xl_spreadsheet( document_name = lv_fname
                                               xdocument     = lv_xfile ).
      lo_xl->if_fdt_doc_spreadsheet~get_worksheet_names(
        IMPORTING worksheet_names = DATA(lt_ws) ).
      DATA(lr_sheet) = lo_xl->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lt_ws[ 1 ] ).
    CATCH cx_root INTO DATA(lx).
      MESSAGE |Excel file cannot be processed: { lx->get_text( ) }| TYPE 'E' ##NO_TEXT.
  ENDTRY.
  ASSIGN lr_sheet->* TO <lt_sheet>.

  LOOP AT <lt_sheet> ASSIGNING <ls_row>.
    DATA(lv_row) = sy-tabix.
    IF lv_row = 1 AND p_hdr = abap_true.
      CONTINUE.
    ENDIF.
    CLEAR ls_input.
    ls_input-xlrow = lv_row.
    DO 7 TIMES.
      ASSIGN COMPONENT sy-index OF STRUCTURE <ls_row> TO <lv_cell>.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      lv_value = <lv_cell>.
      CONDENSE lv_value NO-GAPS.
      TRANSLATE lv_value TO UPPER CASE.
      CASE sy-index.
        WHEN 1. ls_input-rbukrs = lv_value.
        WHEN 2. ls_input-gjahr  = lv_value.
        WHEN 3. ls_input-belnr  = lv_value.
                ls_input-belnr  = |{ ls_input-belnr ALPHA = IN }|.
        WHEN 4. SHIFT lv_value LEFT DELETING LEADING '0'.
                ls_input-docln  = |{ lv_value WIDTH = 6 ALIGN = RIGHT PAD = '0' }|.
        WHEN 5. ls_input-vname  = lv_value.
        WHEN 6. ls_input-egrup  = lv_value.
        WHEN 7. ls_input-recid  = lv_value.
      ENDCASE.
    ENDDO.
    IF ls_input-rbukrs IS INITIAL AND ls_input-belnr IS INITIAL.
      CONTINUE.                                   " empty row
    ENDIF.
    APPEND ls_input TO ct_input.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_LINES - read ACDOCA, validate, build proposal
*&---------------------------------------------------------------------*
FORM check_lines.
  DATA: lt_acd   TYPE SORTED TABLE OF ty_acd
                   WITH UNIQUE KEY rbukrs gjahr belnr docln,
        lt_seen  TYPE HASHED TABLE OF ty_input
                   WITH UNIQUE KEY rbukrs gjahr belnr docln,
        ls_out   TYPE ty_out,
        lt_warn  TYPE string_table,
        lv_found TYPE c LENGTH 1,
        lv_msg   TYPE string,
        lv_where TYPE string,
        lv_bukrs TYPE string.

  DATA(lt_keys) = gt_input.
  SORT lt_keys BY rbukrs gjahr belnr docln.
  DELETE ADJACENT DUPLICATES FROM lt_keys COMPARING rbukrs gjahr belnr docln.

  IF lt_keys IS NOT INITIAL.
    SELECT rbukrs, gjahr, belnr, docln, racct, prctr,
           wsl, rwcur, hsl, rhcur, ksl, rkcur,
           vname, egrup, recid, etype, buzei, awtyp
      FROM acdoca
      FOR ALL ENTRIES IN @lt_keys
      WHERE rldnr  = @p_rldnr
        AND rbukrs = @lt_keys-rbukrs
        AND gjahr  = @lt_keys-gjahr
        AND belnr  = @lt_keys-belnr
        AND docln  = @lt_keys-docln
        AND rrcty  = @gc_rrcty
      INTO CORRESPONDING FIELDS OF TABLE @lt_acd.
  ENDIF.

  LOOP AT gt_input INTO DATA(ls_in).
    CLEAR: ls_out, lt_warn.
    ls_out-xlrow  = ls_in-xlrow.
    ls_out-rbukrs = ls_in-rbukrs.
    ls_out-gjahr  = ls_in-gjahr.
    ls_out-belnr  = ls_in-belnr.
    ls_out-docln  = ls_in-docln.

    "--- Duplicate key in the file
    INSERT ls_in INTO TABLE lt_seen.
    IF sy-subrc <> 0.
      lv_msg = |Duplicate line in file (first at row { lt_seen[ rbukrs = ls_in-rbukrs gjahr = ls_in-gjahr belnr = ls_in-belnr docln = ls_in-docln ]-xlrow })|.
      PERFORM set_result USING 'E' lv_msg CHANGING ls_out.
      APPEND ls_out TO gt_out.
      CONTINUE.
    ENDIF.

    "--- ACDOCA line
    READ TABLE lt_acd INTO DATA(ls_acd)
         WITH TABLE KEY rbukrs = ls_in-rbukrs gjahr = ls_in-gjahr
                        belnr  = ls_in-belnr  docln = ls_in-docln.
    IF sy-subrc <> 0.
      lv_msg = |Line not found in ACDOCA (ledger { p_rldnr }, record type 0)|.
      PERFORM set_result USING 'E' lv_msg CHANGING ls_out.
      APPEND ls_out TO gt_out.
      CONTINUE.
    ENDIF.
    ls_out-racct = ls_acd-racct.  ls_out-prctr = ls_acd-prctr.
    ls_out-wsl   = ls_acd-wsl.    ls_out-rwcur = ls_acd-rwcur.
    ls_out-hsl   = ls_acd-hsl.    ls_out-rhcur = ls_acd-rhcur.
    ls_out-ksl   = ls_acd-ksl.    ls_out-rkcur = ls_acd-rkcur.
    ls_out-buzei = ls_acd-buzei.  ls_out-awtyp = ls_acd-awtyp.
    ls_out-vname_old = ls_acd-vname. ls_out-egrup_old = ls_acd-egrup.
    ls_out-recid_old = ls_acd-recid. ls_out-etype_old = ls_acd-etype.
    ls_out-vname_new = ls_acd-vname. ls_out-egrup_new = ls_acd-egrup.
    ls_out-recid_new = ls_acd-recid. ls_out-etype_new = ls_acd-etype.

    "--- Authority
    PERFORM check_authority USING ls_in-rbukrs CHANGING lv_found.
    IF lv_found <> abap_true.
      lv_msg = |No authorization F_BKPF_BUK for company code { ls_in-rbukrs }|.
      PERFORM set_result USING 'E' lv_msg CHANGING ls_out.
      APPEND ls_out TO gt_out.
      CONTINUE.
    ENDIF.

    IF ls_in-vname IS INITIAL AND ls_in-egrup IS INITIAL AND ls_in-recid IS INITIAL.
      PERFORM set_result USING 'E' 'No JV values given in the file' CHANGING ls_out ##NO_TEXT.
      APPEND ls_out TO gt_out.
      CONTINUE.
    ENDIF.

    "--- Proposed values: blank cell = keep; existing value only with P_OVERWR
    PERFORM apply_field USING 'VNAME' ls_in-vname ls_acd-vname CHANGING ls_out-vname_new lt_warn.
    PERFORM apply_field USING 'EGRUP' ls_in-egrup ls_acd-egrup CHANGING ls_out-egrup_new lt_warn.
    PERFORM apply_field USING 'RECID' ls_in-recid ls_acd-recid CHANGING ls_out-recid_new lt_warn.

    "--- Master data checks on the resulting combination
    DATA(lv_error) = VALUE string( ).
    lv_bukrs = cl_abap_dyn_prg=>quote( CONV string( ls_in-rbukrs ) ).
    IF p_check = abap_true AND ls_out-vname_new IS NOT INITIAL.
      lv_where = |BUKRS = { lv_bukrs } AND VNAME = { cl_abap_dyn_prg=>quote( CONV string( ls_out-vname_new ) ) }|.
      PERFORM check_exists USING gc_tab_vname lv_where CHANGING lv_found.
      CASE lv_found.
        WHEN '-'. lv_error = |Joint venture { ls_out-vname_new } not defined ({ gc_tab_vname })|.
        WHEN '?'. APPEND |VNAME not validated ({ gc_tab_vname } not readable)| TO lt_warn.
      ENDCASE.
    ENDIF.
    IF p_check = abap_true AND lv_error IS INITIAL AND ls_out-egrup_new IS NOT INITIAL.
      lv_where = |BUKRS = { lv_bukrs } AND EGRUP = { cl_abap_dyn_prg=>quote( CONV string( ls_out-egrup_new ) ) }|.
      PERFORM check_exists USING gc_tab_egrup lv_where CHANGING lv_found.
      CASE lv_found.
        WHEN '-'. lv_error = |Equity group { ls_out-egrup_new } not defined ({ gc_tab_egrup })|.
        WHEN '?'. APPEND |EGRUP not validated ({ gc_tab_egrup } not readable)| TO lt_warn.
      ENDCASE.
      IF lv_error IS INITIAL AND ls_out-vname_new IS NOT INITIAL.
        lv_where = |BUKRS = { lv_bukrs } AND VNAME = { cl_abap_dyn_prg=>quote( CONV string( ls_out-vname_new ) ) }| &&
                   | AND EGRUP = { cl_abap_dyn_prg=>quote( CONV string( ls_out-egrup_new ) ) }|.
        PERFORM check_exists USING gc_tab_veg lv_where CHANGING lv_found.
        CASE lv_found.
          WHEN '-'. APPEND |Equity group { ls_out-egrup_new } not found for venture { ls_out-vname_new } ({ gc_tab_veg })| TO lt_warn.
          WHEN '?'. APPEND |VNAME/EGRUP combination not validated ({ gc_tab_veg })| TO lt_warn.
        ENDCASE.
      ENDIF.
    ENDIF.
    IF p_check = abap_true AND lv_error IS INITIAL AND ls_out-recid_new IS NOT INITIAL.
      lv_where = |BUKRS = { lv_bukrs } AND RECID = { cl_abap_dyn_prg=>quote( CONV string( ls_out-recid_new ) ) }|.
      PERFORM check_exists USING gc_tab_recid lv_where CHANGING lv_found.
      CASE lv_found.
        WHEN '-'. lv_error = |Recovery indicator { ls_out-recid_new } not defined ({ gc_tab_recid })|.
        WHEN '?'. APPEND |RECID not validated ({ gc_tab_recid } not readable)| TO lt_warn.
      ENDCASE.
    ENDIF.
    IF lv_error IS NOT INITIAL.
      PERFORM set_result USING 'E' lv_error CHANGING ls_out.
      APPEND ls_out TO gt_out.
      CONTINUE.
    ENDIF.

    "--- Equity type follows the equity group
    IF p_etype = abap_true AND ls_out-egrup_new <> ls_acd-egrup.
      PERFORM derive_etype USING ls_in-rbukrs ls_out-vname_new ls_out-egrup_new
                           CHANGING ls_out-etype_new.
      IF ls_out-etype_new IS INITIAL.
        ls_out-etype_new = ls_acd-etype.
        APPEND 'Equity type could not be derived - kept' TO lt_warn ##NO_TEXT.
      ENDIF.
    ENDIF.

    "--- Changed fields
    IF ls_out-vname_new <> ls_acd-vname. ls_out-changed = |{ ls_out-changed } VNAME|. ENDIF.
    IF ls_out-egrup_new <> ls_acd-egrup. ls_out-changed = |{ ls_out-changed } EGRUP|. ENDIF.
    IF ls_out-recid_new <> ls_acd-recid. ls_out-changed = |{ ls_out-changed } RECID|. ENDIF.
    IF ls_out-etype_new <> ls_acd-etype. ls_out-changed = |{ ls_out-changed } ETYPE|. ENDIF.
    CONDENSE ls_out-changed.

    IF ls_out-changed IS INITIAL.
      INSERT `No change - values already in ACDOCA` INTO lt_warn INDEX 1.
      lv_msg = concat_lines_of( table = lt_warn sep = `; ` ).
      PERFORM set_result USING 'W' lv_msg CHANGING ls_out.
    ELSE.
      INSERT COND string( WHEN p_test = abap_true THEN `Line will be updated`
                          ELSE `Line selected for update` ) INTO lt_warn INDEX 1.
      lv_msg = concat_lines_of( table = lt_warn sep = `; ` ).
      PERFORM set_result USING 'S' lv_msg CHANGING ls_out.
    ENDIF.
    APPEND ls_out TO gt_out.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form APPLY_FIELD - decide the new value of one JV field
*&---------------------------------------------------------------------*
FORM apply_field USING    iv_name TYPE csequence
                          iv_file TYPE any
                          iv_acd  TYPE any
                 CHANGING cv_new  TYPE any
                          ct_warn TYPE string_table.
  IF iv_file IS INITIAL OR iv_file = iv_acd.
    RETURN.                                    " keep current value
  ENDIF.
  IF iv_acd IS INITIAL OR p_overwr = abap_true.
    cv_new = iv_file.
  ELSE.
    APPEND |{ iv_name } kept: ACDOCA has { iv_acd }, file has { iv_file } (set "Overwrite")| TO ct_warn.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_EXISTS - dynamic existence check with buffer
*&   cv_result: X found, - not found, ? table/field not readable
*&---------------------------------------------------------------------*
FORM check_exists USING    iv_tab    TYPE tabname
                           iv_where  TYPE string
                  CHANGING cv_result TYPE c.
  READ TABLE gt_check INTO DATA(ls_check)
       WITH TABLE KEY tabname = iv_tab key = iv_where.
  IF sy-subrc = 0.
    cv_result = ls_check-result.
    RETURN.
  ENDIF.
  TRY.
      SELECT SINGLE @abap_true FROM (iv_tab) WHERE (iv_where) INTO @DATA(lv_exists).
      cv_result = COND #( WHEN sy-subrc = 0 AND lv_exists = abap_true THEN 'X' ELSE '-' ).
    CATCH cx_sy_dynamic_osql_error cx_sy_open_sql_db.
      cv_result = '?'.
  ENDTRY.
  INSERT VALUE #( tabname = iv_tab key = iv_where result = cv_result ) INTO TABLE gt_check.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DERIVE_ETYPE - most frequent ETYPE for company code/VNAME/EGRUP
*&---------------------------------------------------------------------*
FORM derive_etype USING    iv_bukrs TYPE acdoca-rbukrs
                           iv_vname TYPE acdoca-vname
                           iv_egrup TYPE acdoca-egrup
                  CHANGING cv_etype TYPE acdoca-etype.
  STATICS st_etype TYPE HASHED TABLE OF ty_acd WITH UNIQUE KEY rbukrs vname egrup.
  CLEAR cv_etype.
  IF iv_egrup IS INITIAL.
    RETURN.
  ENDIF.
  READ TABLE st_etype INTO DATA(ls_buf)
       WITH TABLE KEY rbukrs = iv_bukrs vname = iv_vname egrup = iv_egrup.
  IF sy-subrc = 0.
    cv_etype = ls_buf-etype.
    RETURN.
  ENDIF.
  SELECT etype, COUNT(*) AS cnt
    FROM acdoca
    WHERE rldnr  = @p_rldnr
      AND rbukrs = @iv_bukrs
      AND vname  = @iv_vname
      AND egrup  = @iv_egrup
      AND etype  <> @space
    GROUP BY etype
    ORDER BY cnt DESCENDING
    INTO TABLE @DATA(lt_etype)
    UP TO 1 ROWS.
  IF lt_etype IS NOT INITIAL.
    cv_etype = lt_etype[ 1 ]-etype.
  ENDIF.
  INSERT VALUE #( rbukrs = iv_bukrs vname = iv_vname egrup = iv_egrup etype = cv_etype )
         INTO TABLE st_etype.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CHECK_AUTHORITY - F_BKPF_BUK, 02 for update / 03 for test
*&---------------------------------------------------------------------*
FORM check_authority USING    iv_bukrs TYPE acdoca-rbukrs
                     CHANGING cv_ok    TYPE c.
  cv_ok = abap_false.
  IF line_exists( gt_authok[ table_line = iv_bukrs ] ).
    cv_ok = abap_true.
    RETURN.
  ELSEIF line_exists( gt_authno[ table_line = iv_bukrs ] ).
    RETURN.
  ENDIF.
  DATA(lv_actvt) = COND activ_auth( WHEN p_test = abap_true THEN '03' ELSE '02' ).
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
    ID 'BUKRS' FIELD iv_bukrs
    ID 'ACTVT' FIELD lv_actvt.
  IF sy-subrc = 0.
    INSERT iv_bukrs INTO TABLE gt_authok.
    cv_ok = abap_true.
  ELSE.
    INSERT iv_bukrs INTO TABLE gt_authno.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form UPDATE_ACDOCA - one LUW per document, with BKPF lock
*&---------------------------------------------------------------------*
FORM update_acdoca.
  DATA lv_ok TYPE abap_bool.

  SORT gt_out BY rbukrs gjahr belnr docln.
  LOOP AT gt_out INTO DATA(ls_doc) WHERE light = '3'
       GROUP BY ( rbukrs = ls_doc-rbukrs gjahr = ls_doc-gjahr belnr = ls_doc-belnr )
       ASSIGNING FIELD-SYMBOL(<ls_grp>).

    CALL FUNCTION 'ENQUEUE_EFBKPF'
      EXPORTING
        bukrs          = <ls_grp>-rbukrs
        belnr          = <ls_grp>-belnr
        gjahr          = <ls_grp>-gjahr
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      DATA(lv_lockmsg) = CONV bapi_msg( |Document locked by { sy-msgv1 } - not updated| ).
      LOOP AT GROUP <ls_grp> ASSIGNING FIELD-SYMBOL(<ls_l>).
        <ls_l>-light = '1'. <ls_l>-msgty = 'E'. <ls_l>-message = lv_lockmsg.
      ENDLOOP.
      CONTINUE.
    ENDIF.

    lv_ok = abap_true.
    LOOP AT GROUP <ls_grp> ASSIGNING FIELD-SYMBOL(<ls_line>).
      UPDATE acdoca
         SET vname = @<ls_line>-vname_new,
             egrup = @<ls_line>-egrup_new,
             recid = @<ls_line>-recid_new,
             etype = @<ls_line>-etype_new
       WHERE rldnr  = @p_rldnr
         AND rbukrs = @<ls_line>-rbukrs
         AND gjahr  = @<ls_line>-gjahr
         AND belnr  = @<ls_line>-belnr
         AND docln  = @<ls_line>-docln
         AND rrcty  = @gc_rrcty.
      IF sy-subrc <> 0 OR sy-dbcnt <> 1.
        lv_ok = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF lv_ok = abap_true.
      COMMIT WORK AND WAIT.
    ELSE.
      ROLLBACK WORK.
    ENDIF.
    CALL FUNCTION 'DEQUEUE_EFBKPF'
      EXPORTING
        bukrs = <ls_grp>-rbukrs
        belnr = <ls_grp>-belnr
        gjahr = <ls_grp>-gjahr.

    LOOP AT GROUP <ls_grp> ASSIGNING <ls_l>.
      IF lv_ok = abap_true.
        <ls_l>-light = '3'. <ls_l>-msgty = 'S'.
        <ls_l>-message = |Updated: { <ls_l>-changed }|.
      ELSE.
        <ls_l>-light = '1'. <ls_l>-msgty = 'E'.
        <ls_l>-message = 'Update failed - whole document rolled back' ##NO_TEXT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  SORT gt_out BY xlrow.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_RESULT
*&---------------------------------------------------------------------*
FORM set_result USING    iv_type TYPE bapi_mtype
                         iv_text TYPE csequence
                CHANGING cs_out  TYPE ty_out.
  cs_out-msgty   = iv_type.
  cs_out-message = iv_text.
  cs_out-light   = SWITCH #( iv_type WHEN 'E' THEN '1' WHEN 'W' THEN '2' ELSE '3' ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form DISPLAY_ALV
*&---------------------------------------------------------------------*
FORM display_alv.
  DATA: lo_alv  TYPE REF TO cl_salv_table,
        lo_cols TYPE REF TO cl_salv_columns_table,
        lo_col  TYPE REF TO cl_salv_column_table.

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                              CHANGING  t_table      = gt_out ).
    CATCH cx_salv_msg.
      RETURN.
  ENDTRY.

  lo_alv->get_functions( )->set_all( abap_true ).
  lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).
  lo_cols = lo_alv->get_columns( ).
  lo_cols->set_optimize( abap_true ).
  TRY.
      lo_cols->set_exception_column( 'LIGHT' ).
    CATCH cx_salv_data_error.
  ENDTRY.

  DATA(lt_text) = VALUE string_table(
    ( `XLROW|File row` )
    ( `VNAME_OLD|Current Joint venture` )      ( `EGRUP_OLD|Current Equity group` )
    ( `RECID_OLD|Current Recovery ind.` )      ( `ETYPE_OLD|Current Equity type` )
    ( `VNAME_NEW|Proposed Joint venture` )     ( `EGRUP_NEW|Proposed Equity group` )
    ( `RECID_NEW|Proposed Recovery ind.` )     ( `ETYPE_NEW|Proposed Equity type` )
    ( `CHANGED|Fields changed` )               ( `MSGTY|Type` )
    ( `MESSAGE|Result` ) ) ##NO_TEXT.
  LOOP AT lt_text INTO DATA(lv_text).
    SPLIT lv_text AT '|' INTO DATA(lv_col) DATA(lv_label).
    TRY.
        lo_col ?= lo_cols->get_column( CONV lvc_fname( lv_col ) ).
        lo_col->set_short_text( CONV scrtext_s( lv_label ) ).
        lo_col->set_medium_text( CONV scrtext_m( lv_label ) ).
        lo_col->set_long_text( CONV scrtext_l( lv_label ) ).
      CATCH cx_salv_not_found.
    ENDTRY.
  ENDLOOP.

  TRY.
      lo_col ?= lo_cols->get_column( 'WSL' ). lo_col->set_currency_column( 'RWCUR' ).
      lo_col ?= lo_cols->get_column( 'HSL' ). lo_col->set_currency_column( 'RHCUR' ).
      lo_col ?= lo_cols->get_column( 'KSL' ). lo_col->set_currency_column( 'RKCUR' ).
    CATCH cx_salv_not_found cx_salv_data_error.
  ENDTRY.

  "Header
  DATA(lo_grid) = NEW cl_salv_form_layout_grid( ).
  DATA(lv_err) = REDUCE i( INIT n = 0 FOR o IN gt_out WHERE ( light = '1' ) NEXT n = n + 1 ).
  DATA(lv_wrn) = REDUCE i( INIT n = 0 FOR o IN gt_out WHERE ( light = '2' ) NEXT n = n + 1 ).
  DATA(lv_ok)  = REDUCE i( INIT n = 0 FOR o IN gt_out WHERE ( light = '3' ) NEXT n = n + 1 ).
  lo_grid->create_header_information( row = 1 column = 1
    text = COND string( WHEN p_test = abap_true
                        THEN 'TEST RUN - ACDOCA not changed'
                        ELSE 'UPDATE RUN - ACDOCA changed' ) ) ##NO_TEXT.
  lo_grid->create_label( row = 2 column = 1 text = 'File / Ledger' ) ##NO_TEXT.
  lo_grid->create_text(  row = 2 column = 2 text = |{ p_file } / { p_rldnr }| ).
  lo_grid->create_label( row = 3 column = 1 text = 'Rows OK / Warning / Error' ) ##NO_TEXT.
  lo_grid->create_text(  row = 3 column = 2 text = |{ lv_ok } / { lv_wrn } / { lv_err }| ).
  lo_alv->set_top_of_list( lo_grid ).
  lo_alv->set_top_of_list_print( lo_grid ).

  lo_alv->display( ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form F4_FILE
*&---------------------------------------------------------------------*
FORM f4_file CHANGING cv_file TYPE localfile.
  DATA: lt_files TYPE filetable,
        lv_rc    TYPE i.
  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      default_extension = 'xlsx'
      file_filter       = 'Excel (*.xlsx)|*.xlsx'
    CHANGING
      file_table        = lt_files
      rc                = lv_rc
    EXCEPTIONS
      OTHERS            = 1 ) ##NO_TEXT.
  IF sy-subrc = 0 AND lv_rc = 1.
    cv_file = lt_files[ 1 ]-filename.
  ENDIF.
ENDFORM.
