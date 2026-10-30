*&---------------------------------------------------------------------*
*& Report  ZMMS_VEND_BP_MAINT
*&---------------------------------------------------------------------*
*& Title       : Supplier Master Download / Upload (Business Partner)
*& Module      : MM                       Transaction : ZMMS_VENDMNT
*&
*& STATUS - SELECTION SCREEN PROTOTYPE
*&   This version is the selection screen only, for the users to check
*&   its look and feel. Every field, dropdown and value help works, and
*&   executing lists what was chosen. Nothing is read from the system,
*&   nothing is posted, and no file is opened or written.
*&
*& The program this screen belongs to
*&   Replaces the 22 LSMW objects that maintain supplier master data with
*&   one program. Each LSMW layout becomes a scenario, picked in two
*&   steps - an area, then a scenario of that area - and each scenario is
*&   both downloaded (existing suppliers written into the layout) and
*&   uploaded (a filled layout posted through the Business Partner API).
*&   LSMW objects with identical layouts share one scenario, which names
*&   every object it replaces: 22 objects, 19 scenarios.
*&---------------------------------------------------------------------*
REPORT zmms_vend_bp_maint.

" The selection screen's own function code, so a click on a radio button
" or a dropdown can be told apart from the user asking to run.
TABLES sscrfields.

" The file path. 255 is the widest a screen field goes; the file dialog
" hands back a STRING, and a shorter field would cut a long path short.
TYPES ty_path TYPE c LENGTH 255.
TYPES ty_area_id TYPE c LENGTH 2.
TYPES ty_scen_id TYPE c LENGTH 3.

TYPES: BEGIN OF ty_area,
         id   TYPE c LENGTH 2,
         text TYPE c LENGTH 40,
       END OF ty_area,
       tt_area TYPE STANDARD TABLE OF ty_area WITH EMPTY KEY.

" One scenario: the area it belongs to, its key, what it maintains, the
" LSMW objects it replaces, and the file name proposed for a download.
TYPES: BEGIN OF ty_scen,
         area TYPE c LENGTH 2,
         id   TYPE c LENGTH 3,
         text TYPE c LENGTH 50,
         lsmw TYPE c LENGTH 80,
         file TYPE c LENGTH 40,
       END OF ty_scen,
       tt_scen TYPE STANDARD TABLE OF ty_scen WITH EMPTY KEY.

" Data objects the SELECT-OPTIONS are built over.
DATA: gv_lifnr TYPE lifnr,
      gv_bp    TYPE bu_partner.

*----------------------------------------------------------------------*
* Selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_down RADIOBUTTON GROUP g0 USER-COMMAND md DEFAULT 'X',
            p_up   RADIOBUTTON GROUP g0.
SELECTION-SCREEN END OF BLOCK b0.

" Two dropdowns: the area, then the scenarios of that area. Both carry a
" USER-COMMAND, so a choice comes straight back and the second list, the
" LSMW line and the file name follow it at once.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) TEXT-006 FOR FIELD p_area.
PARAMETERS p_area TYPE ty_area_id AS LISTBOX VISIBLE LENGTH 40 USER-COMMAND ar.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(10) TEXT-007 FOR FIELD p_scen.
PARAMETERS p_scen TYPE ty_scen_id AS LISTBOX VISIBLE LENGTH 64 USER-COMMAND sc.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN COMMENT /1(75) cm_lsmw.
SELECTION-SCREEN END OF BLOCK b1.

" Which suppliers a download writes. Ignored on an upload.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS: s_lifnr FOR gv_lifnr,
                s_bp    FOR gv_bp.
PARAMETERS:     p_bukrs TYPE bukrs,
                p_ekorg TYPE ekorg,
                p_max   TYPE i DEFAULT 100.
SELECTION-SCREEN END OF BLOCK b2.

" What an upload does with what it reads. Ignored on a download.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-004.
PARAMETERS: p_test AS CHECKBOX DEFAULT 'X',
            p_stop AS CHECKBOX,
            p_skip TYPE i DEFAULT 1.
SELECTION-SCREEN END OF BLOCK b3.

" Not OBLIGATORY: a required field that is empty stops every radio button
" and dropdown click with "Fill in all required entry fields". It is
" checked when the program is run instead.
SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-005.
PARAMETERS: p_file  TYPE ty_path LOWER CASE,
            p_xlsx  RADIOBUTTON GROUP g2 USER-COMMAND ft DEFAULT 'X',
            p_txt   RADIOBUTTON GROUP g2,
            p_empty AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b4.

*----------------------------------------------------------------------*
* LCL_CATALOG - the areas and scenarios
*   One scenario per LSMW layout. Where several LSMW objects share a
*   layout, the scenario names them all.
*----------------------------------------------------------------------*
CLASS lcl_catalog DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS areas
      RETURNING VALUE(rt) TYPE tt_area.
    CLASS-METHODS scenarios
      RETURNING VALUE(rt) TYPE tt_scen.
    CLASS-METHODS scenario
      IMPORTING VALUE(iv_id) TYPE ty_scen_id
      RETURNING VALUE(rs)    TYPE ty_scen.
ENDCLASS.

CLASS lcl_catalog IMPLEMENTATION.

  METHOD areas.
    rt = VALUE #(
      ( id = 'CR' text = 'Supplier creation' )
      ( id = 'AD' text = 'Name, address and communication' )
      ( id = 'CC' text = 'Company code and payment' )
      ( id = 'BK' text = 'Bank' )
      ( id = 'DL' text = 'Deletion flags' ) ).
  ENDMETHOD.

  METHOD scenarios.
    rt = VALUE #(
      " ---- supplier creation
      ( area = 'CR' id = 'V01' text = 'Supplier general data'
        lsmw = 'BIG_XK01_VENGEN (BIG and CCEJ)' file = 'BIG_XK01_VENGEN' )
      ( area = 'CR' id = 'V02' text = 'Supplier company code data'
        lsmw = 'BIG_XK01_VENCOM (BIG and CCEJ)' file = 'BIG_XK01_VENCOM' )
      ( area = 'CR' id = 'V03' text = 'Supplier purchasing data'
        lsmw = 'BIG_XK01_VEN_PU (BIG and CCEJ)' file = 'BIG_XK01_VEN_PU' )
      ( area = 'CR' id = 'V04' text = 'Company code extension'
        lsmw = 'CRT_COMPANY' file = 'CRT_COMPANY' )
      ( area = 'CR' id = 'V05' text = 'Depot supplier'
        lsmw = 'CRT_DEP' file = 'CRT_DEP' )
      " ---- name, address and communication
      ( area = 'AD' id = 'V06' text = 'Names'
        lsmw = 'XK02_UPD_NAME9' file = 'XK02_UPD_NAME9' )
      ( area = 'AD' id = 'V07' text = 'Phone, fax and e-mail'
        lsmw = 'XK02_ADD_COMM' file = 'XK02_ADD_COMM' )
      ( area = 'AD' id = 'V08' text = 'Address'
        lsmw = 'UPD_LFA1_ADRC' file = 'UPD_LFA1_ADRC' )
      ( area = 'AD' id = 'V09' text = 'E-mail address'
        lsmw = 'UPD_XK02_TEST' file = 'UPD_XK02_TEST' )
      ( area = 'AD' id = 'V10' text = 'Tax fields'
        lsmw = 'UPD_LFA1' file = 'UPD_LFA1' )
      " ---- company code and payment
      ( area = 'CC' id = 'V11' text = 'Payment terms and payment methods'
        lsmw = 'XK02_ZTERM_PAYT' file = 'XK02_ZTERM_PAYT' )
      ( area = 'CC' id = 'V12' text = 'Company code data'
        lsmw = 'UPD_COM' file = 'UPD_COM' )
      ( area = 'CC' id = 'V13' text = 'Depot supplier change'
        lsmw = 'UPD_DEP' file = 'UPD_DEP' )
      ( area = 'CC' id = 'V14' text = 'Tax number and our account number'
        lsmw = 'UPD_VEND (MIGRATION)' file = 'UPD_VEND_MIGRATION' )
      " ---- bank
      ( area = 'BK' id = 'V15' text = 'Bank details'
        lsmw = 'UPD_LFBK_BANK (DUAL)' file = 'UPD_LFBK_BANK_DUAL' )
      ( area = 'BK' id = 'V16' text = 'Bank details - short layout'
        lsmw = 'UPD_LFBK_BANK (MAINTENANCE)' file = 'UPD_LFBK_BANK_MAINT' )
      ( area = 'BK' id = 'V17' text = 'West DME data'
        lsmw = 'UPD_VEND (WEST_DME)' file = 'UPD_VEND_WEST_DME' )
      " ---- deletion flags
      ( area = 'DL' id = 'V18' text = 'Deletion flags'
        lsmw = 'XK06_DELETE_VEN' file = 'XK06_DELETE_VEN' )
      ( area = 'DL' id = 'V19' text = 'Deletion flags - central and company code'
        lsmw = 'XK06_DEL2' file = 'XK06_DEL2' ) ).
  ENDMETHOD.

  METHOD scenario.
    DATA(lt_all) = scenarios( ).
    READ TABLE lt_all INTO rs WITH KEY id = iv_id.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* LCL_SCREEN - the selection screen and the prototype's output
*----------------------------------------------------------------------*
CLASS lcl_screen DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS init.
    CLASS-METHODS output.
    CLASS-METHODS f4_file.
    CLASS-METHODS validate.
    CLASS-METHODS run.
  PRIVATE SECTION.
    " The file name last proposed for a download, and what it was
    " proposed for - so a name the user picked is never overwritten.
    CLASS-DATA mv_file TYPE ty_path.
    CLASS-DATA mv_scen TYPE ty_scen_id.
    CLASS-DATA mv_ext  TYPE string.

    CLASS-METHODS set_list
      IMPORTING VALUE(iv_id)     TYPE vrm_id
                VALUE(it_values) TYPE vrm_values.
    CLASS-METHODS propose_file.
    CLASS-METHODS ext
      RETURNING VALUE(rv) TYPE string.
    CLASS-METHODS screen_val
      IMPORTING VALUE(iv_field) TYPE dynpread-fieldname
      RETURNING VALUE(rv)       TYPE string.
    CLASS-METHODS show
      IMPORTING VALUE(iv_label) TYPE string
                VALUE(iv_value) TYPE string.
ENDCLASS.

CLASS lcl_screen IMPLEMENTATION.

  METHOD init.
    p_area = 'CR'.
    p_scen = 'V01'.
  ENDMETHOD.

  METHOD output.
    " ---- the area list
    DATA lt_area TYPE vrm_values.
    DATA(lt_a) = lcl_catalog=>areas( ).
    LOOP AT lt_a INTO DATA(ls_a).
      APPEND VALUE #( key = ls_a-id text = ls_a-text ) TO lt_area.
    ENDLOOP.
    set_list( iv_id = 'P_AREA' it_values = lt_area ).
    IF p_area IS INITIAL.
      p_area = 'CR'.
    ENDIF.

    " ---- the scenarios of that area; a scenario of another area gives
    " way to the first of this one
    DATA lt_scen TYPE vrm_values.
    DATA lv_first TYPE ty_scen_id.
    DATA lv_kept TYPE abap_bool.
    DATA(lt_s) = lcl_catalog=>scenarios( ).
    LOOP AT lt_s INTO DATA(ls_s) WHERE area = p_area.
      APPEND VALUE #( key = ls_s-id text = |{ ls_s-id }  { ls_s-text }| ) TO lt_scen.
      IF lv_first IS INITIAL.
        lv_first = ls_s-id.
      ENDIF.
      IF ls_s-id = p_scen.
        lv_kept = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_kept = abap_false.
      p_scen = lv_first.
    ENDIF.
    set_list( iv_id = 'P_SCEN' it_values = lt_scen ).

    DATA(ls_cur) = lcl_catalog=>scenario( p_scen ).
    cm_lsmw = |Replaces LSMW: { ls_cur-lsmw }|.

    " ---- which suppliers to write is a download question; how to post
    " what is read is an upload one. Each block is closed while the other
    " direction is chosen, so nobody fills in options that do nothing.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN 'S_LIFNR-LOW' OR 'S_LIFNR-HIGH' OR 'S_BP-LOW' OR 'S_BP-HIGH'
          OR '%_S_LIFNR_%_APP_%-VALU_PUSH' OR '%_S_BP_%_APP_%-VALU_PUSH'
          OR 'P_BUKRS' OR 'P_EKORG' OR 'P_MAX' OR 'P_EMPTY'.
          screen-input = COND #( WHEN p_down = abap_true THEN '1' ELSE '0' ).
          MODIFY SCREEN.
        WHEN 'P_TEST' OR 'P_STOP' OR 'P_SKIP'.
          screen-input = COND #( WHEN p_up = abap_true THEN '1' ELSE '0' ).
          MODIFY SCREEN.
      ENDCASE.
    ENDLOOP.

    propose_file( ).
  ENDMETHOD.

  METHOD set_list.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = iv_id
        values          = it_values
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE |The list for { iv_id } could not be filled| TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD ext.
    rv = COND #( WHEN p_txt = abap_true THEN `.txt` ELSE `.xlsx` ).
  ENDMETHOD.

  METHOD screen_val.
    " A value help runs before the screen has been handed to the program, so
    " the program variable still holds whatever the last round trip left in
    " it. The value has to be read off the screen itself.
    DATA lt_dynp TYPE TABLE OF dynpread.
    DATA ls_dynp TYPE dynpread.
    ls_dynp-fieldname = iv_field.
    APPEND ls_dynp TO lt_dynp.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING  dyname             = sy-repid
                 dynumb             = sy-dynnr
                 translate_to_upper = abap_true
      TABLES     dynpfields         = lt_dynp
      EXCEPTIONS OTHERS             = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    READ TABLE lt_dynp INTO ls_dynp INDEX 1.
    IF sy-subrc = 0.
      rv = condense( ls_dynp-fieldvalue ).
    ENDIF.
  ENDMETHOD.

  METHOD propose_file.
    " An upload reads a file that is already there; its name is the
    " user's. The name proposed for a download is not one anyone picked
    " to upload, so it is cleared rather than read by accident.
    IF p_up = abap_true.
      IF p_file = mv_file.
        CLEAR: p_file, mv_file, mv_scen, mv_ext.
      ENDIF.
      RETURN.
    ENDIF.

    " The name follows the scenario and the format. A name the user typed
    " or picked is kept until the scenario or the format changes.
    DATA(lv_ext) = ext( ).
    IF p_file IS NOT INITIAL AND p_file <> mv_file
       AND p_scen = mv_scen AND lv_ext = mv_ext.
      RETURN.
    ENDIF.

    " Whatever stands in front of the last separator is the folder.
    DATA(lv_old) = CONV string( p_file ).
    DATA(lv_dir) = `C:\temp\`.
    DATA(lv_i) = nmax( val1 = find( val = lv_old sub = `\` occ = -1 )
                       val2 = find( val = lv_old sub = `/` occ = -1 ) ).
    IF lv_i >= 0.
      lv_dir = substring( val = lv_old len = lv_i + 1 ).
    ENDIF.

    DATA(ls_s) = lcl_catalog=>scenario( p_scen ).
    p_file  = |{ lv_dir }{ ls_s-file }{ lv_ext }|.
    mv_file = p_file.
    mv_scen = p_scen.
    mv_ext  = lv_ext.
  ENDMETHOD.

  METHOD f4_file.
    DATA(lv_filter) = COND string( WHEN p_txt = abap_true
                                   THEN `Text, tab-separated (*.txt)|*.txt|`
                                   ELSE `Excel workbook (*.xlsx)|*.xlsx|` ).
    " An upload reads a file that is already there, so it is picked; a
    " download writes one, so it is named.
    IF p_up = abap_true.
      DATA lt_ft TYPE filetable.
      DATA lv_rc TYPE i.
      DATA lv_ua TYPE i.
      cl_gui_frontend_services=>file_open_dialog(
        EXPORTING  window_title = 'Select the file to upload'
                   file_filter  = lv_filter
        CHANGING   file_table   = lt_ft
                   rc           = lv_rc
                   user_action  = lv_ua
        EXCEPTIONS OTHERS       = 1 ).
      IF sy-subrc = 0 AND lv_ua = cl_gui_frontend_services=>action_ok AND lv_rc >= 1.
        DATA(lv_pick) = lt_ft[ 1 ]-filename.
        " A path longer than the field is refused rather than cut short.
        IF strlen( lv_pick ) > 255.
          MESSAGE 'That path is longer than 255 characters - move the file to a shorter path'
                  TYPE 'S' DISPLAY LIKE 'E'.
        ELSE.
          p_file = lv_pick.
        ENDIF.
      ENDIF.
      RETURN.
    ENDIF.

    DATA: lv_name TYPE string,
          lv_path TYPE string,
          lv_full TYPE string.
    DATA lv_scen TYPE ty_scen_id.
    lv_scen = screen_val( 'P_SCEN' ).
    IF lv_scen IS INITIAL.
      lv_scen = p_scen.
    ENDIF.
    DATA(ls_s) = lcl_catalog=>scenario( lv_scen ).
    DATA(lv_def) = |{ ls_s-file }{ ext( ) }|.
    DATA(lv_dext) = COND string( WHEN p_txt = abap_true THEN `txt` ELSE `xlsx` ).
    cl_gui_frontend_services=>file_save_dialog(
      EXPORTING  window_title      = 'Save the file'
                 default_extension = lv_dext
                 default_file_name = lv_def
                 file_filter       = lv_filter
      CHANGING   filename          = lv_name
                 path              = lv_path
                 fullpath          = lv_full
      EXCEPTIONS OTHERS            = 1 ).
    IF sy-subrc = 0 AND lv_full IS NOT INITIAL.
      IF strlen( lv_full ) > 255.
        MESSAGE 'That path is longer than 255 characters - pick a shorter folder'
                TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        p_file = lv_full.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD validate.
    IF p_scen IS INITIAL.
      MESSAGE 'Choose an area and a scenario' TYPE 'E'.
    ENDIF.
    IF p_down = abap_true.
      IF p_empty = abap_false AND s_lifnr[] IS INITIAL AND s_bp[] IS INITIAL.
        MESSAGE 'Give a supplier or a business partner, or tick Headings only' TYPE 'E'.
      ENDIF.
      IF p_max < 1.
        MESSAGE 'Rows at most must be 1 or more' TYPE 'E'.
      ENDIF.
    ELSEIF p_skip < 1.
      MESSAGE 'Heading rows to skip must be 1 or more' TYPE 'E'.
    ENDIF.
    IF p_file IS INITIAL.
      MESSAGE 'Give a file name - F4 opens the file dialog' TYPE 'E'.
    ENDIF.
    " The end of the name decides, not whether the extension appears
    " somewhere in it.
    DATA(lv_name) = to_upper( CONV string( p_file ) ).
    IF p_xlsx = abap_true AND lv_name NP '*.XLSX'.
      MESSAGE 'Excel chosen - the file name must end in .xlsx' TYPE 'E'.
    ELSEIF p_txt = abap_true AND lv_name NP '*.TXT'.
      MESSAGE 'Text chosen - the file name must end in .txt' TYPE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD show.
    WRITE: / iv_label, 32 iv_value.
  ENDMETHOD.

  METHOD run.
    DATA(ls_s) = lcl_catalog=>scenario( p_scen ).
    DATA(lt_a) = lcl_catalog=>areas( ).
    DATA lv_area TYPE c LENGTH 40.
    lv_area = VALUE #( lt_a[ id = p_area ]-text OPTIONAL ).

    FORMAT COLOR COL_NEGATIVE.
    WRITE: / 'PROTOTYPE - selection screen only. Nothing was read, written or posted.'.
    FORMAT COLOR OFF.
    ULINE.
    show( iv_label = `Direction`
          iv_value = COND #( WHEN p_down = abap_true THEN `Download a workbook`
                             ELSE `Upload a filled workbook` ) ).
    show( iv_label = `Area`          iv_value = CONV #( lv_area ) ).
    show( iv_label = `Scenario`      iv_value = |{ ls_s-id } { ls_s-text }| ).
    show( iv_label = `Replaces LSMW` iv_value = CONV #( ls_s-lsmw ) ).
    IF p_down = abap_true.
      show( iv_label = `Suppliers`
            iv_value = COND #( WHEN s_lifnr[] IS INITIAL THEN `-`
                               ELSE |{ lines( s_lifnr[] ) } selection line(s)| ) ).
      show( iv_label = `Business partners`
            iv_value = COND #( WHEN s_bp[] IS INITIAL THEN `-`
                               ELSE |{ lines( s_bp[] ) } selection line(s)| ) ).
      show( iv_label = `Company code`       iv_value = CONV #( p_bukrs ) ).
      show( iv_label = `Purchasing org.`    iv_value = CONV #( p_ekorg ) ).
      show( iv_label = `Rows at most`       iv_value = |{ p_max }| ).
      show( iv_label = `Headings only`
            iv_value = COND #( WHEN p_empty = abap_true THEN `Yes` ELSE `No` ) ).
    ELSE.
      show( iv_label = `Test run`
            iv_value = COND #( WHEN p_test = abap_true THEN `Yes - nothing would be posted`
                               ELSE `No - rows would be posted` ) ).
      show( iv_label = `Stop at first faulty row`
            iv_value = COND #( WHEN p_stop = abap_true THEN `Yes` ELSE `No` ) ).
      show( iv_label = `Heading rows to skip` iv_value = |{ p_skip }| ).
    ENDIF.
    show( iv_label = `File (on the PC)` iv_value = CONV #( p_file ) ).
    show( iv_label = `Format`
          iv_value = COND #( WHEN p_xlsx = abap_true THEN `Excel (.xlsx)`
                             ELSE `Text, tab-separated (.txt)` ) ).
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* Events
*----------------------------------------------------------------------*
INITIALIZATION.
  lcl_screen=>init( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_screen=>output( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  lcl_screen=>f4_file( ).

AT SELECTION-SCREEN.
  " Only a run is checked. A click on a radio button or a dropdown, or
  " Enter while the screen is still being filled in, is not a request to
  " run, so there is nothing to complain about yet.
  CHECK sscrfields-ucomm = 'ONLI' OR sscrfields-ucomm = 'PRIN' OR sscrfields-ucomm = 'SJOB'.
  lcl_screen=>validate( ).

START-OF-SELECTION.
  lcl_screen=>run( ).
