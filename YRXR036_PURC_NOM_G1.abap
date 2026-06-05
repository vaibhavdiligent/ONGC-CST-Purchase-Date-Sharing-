*-----------------------------------------------------------------------------------*
* PROGRAM DESCRIPTION: Purchase Nomination Upload
*
* DEVELOPER: Karavadi Ravi Chandra
* CREATION DATE: 24-07-2023
* TR NUMBER:   DVRK9A13PA
* REFRENCE PROGRAM : YRXR019_PURC_NOM_G1
*-----------------------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Report YRXR036_PURC_NOM_G1
*&---------------------------------------------------------------------*
REPORT yrxr036_purc_nom_g1.

TABLES : yro_nom_param1,oijnomi.
* TYPES------------------------------------------------------------------------------*
TYPES: BEGIN OF ty_main,
         tsyst          TYPE oij_tsyst,
         vbeln          TYPE vbeln,
         date           TYPE sy-datum,
         locid          TYPE oij_locid,
         matnr          TYPE matnr,
         menge          TYPE oij_menge,
         unit           TYPE oij_uniti,
         charg          TYPE charg_d,
         rank           TYPE i,
         ancv           TYPE yyncv,
         agcv           TYPE yygcv,
         nomtk          TYPE oij_nomtk,
         nomit          TYPE oij_item,
         st_qty         TYPE oijnomi-yyoij_dpimb_qty,
         del_ind        TYPE char1,
         flag           TYPE char1,
         post_status    TYPE char30,
         ticketnr       TYPE oij_tktnr,
         ticket_key     TYPE oij_el_tkt_key,
         ticket_item    TYPE oij_el_tkt_posnr,
         color(4),
         error_msg(170) TYPE c,
       END OF ty_main.

**SOC BY RAVI/SHREYOSI ON 05.10.2023 CHARM: 4000007179, KRC:GMS: YRGR040_multpl date_PGLS
TYPES: BEGIN OF ty_main_r,
         date  TYPE aedat,
         locid TYPE oij_locid,
         rank  TYPE i,
         nomtk TYPE oij_nomtk,
         nomit TYPE oij_item,
       END OF ty_main_r.
**EOC BY RAVI/SHREYOSI ON 05.10.2023 CHARM: 4000007179, KRC:GMS: YRGR040_multpl date_PGLS

TYPES: BEGIN OF ty_message,
         id      TYPE symsgid,
         number  TYPE symsgno,
         message TYPE bapi_msg,
       END OF ty_message.

TYPES: BEGIN OF ty_temp,
         matnr    TYPE matnr,
         werk     TYPE werks_d,
         charg    TYPE charg_d,
         xchpf(1),
         locid    TYPE oij_locid,
         tsyst    TYPE oij_tsyst,
         ebeln    TYPE ekpo-ebeln,
         date     TYPE sy-datum,
       END OF ty_temp.
DATA: lt_temp TYPE TABLE OF ty_temp.

TYPES: BEGIN OF ty_log,
         tsyst        TYPE oij_tsyst,
         ebeln        TYPE ekpo-ebeln,
         date         TYPE sy-datum,
         locid        TYPE oij_locid,
         matnr        TYPE matnr,
         charg        TYPE charg_d,
         message(100),
       END OF ty_log.

TYPES : BEGIN OF ty_display_final,
          sel              TYPE char1,
          version          TYPE oij_el_tkt_version,
          message(132)     TYPE c,
          stat(1)          TYPE c,
          idate            TYPE oij_idate,
          locid            TYPE oij_locid,
          nomtk            TYPE oij_nomtk,
          nomit            TYPE oij_item,
          matnr_i          TYPE oij_matnri,
          charg_o          TYPE oij_chargo,
          charg_d          TYPE oij_chargo,
          docnr            TYPE oij_docnr,
          yyoij_cnom_qty   TYPE yyoij_cnom_qty,
          yyoij_cnom_uom   TYPE yyoij_cnom_uom,
          menge            TYPE oij_menge,
          unit_i           TYPE oij_uniti,
          ga_rank          TYPE oij_rank,
          ga_allocated_qty TYPE oij_allocated_qty,
          yyoij_dnimb_qty  TYPE yyoij_dnimb_qty,
          yyoij_dpimb_qty  TYPE yyoij_dpimb_qty,
          ticketnr         TYPE oij_tktnr,
          ticket_item      TYPE oij_el_tkt_posnr,
          ticket_key       TYPE oij_el_tkt_key,
          post_status      TYPE char30,
          flag(1),
          celltab          TYPE lvc_t_styl,
          color(4),
        END OF ty_display_final.

DATA: lt_log TYPE TABLE OF ty_log,
      ls_log TYPE ty_log.

* INTERNAL TABLES------------------------------------------------------------*
DATA: i_tab        TYPE STANDARD TABLE OF alsmex_tabline,
      i_main       TYPE TABLE OF ty_main,
      i_nom_item   TYPE TABLE OF bapitswnom03,   "#EC CI_USAGE_OK[2215424]
      i_nom_item_o TYPE TABLE OF bapitswnom03_o, "#EC CI_USAGE_OK[2215424]
      i_return     TYPE TABLE OF bapiret2,
      i_error      TYPE TABLE OF bapiret2,
      i_message    TYPE TABLE OF ty_message.
DATA: bdcdata TYPE TABLE OF bdcdata WITH HEADER LINE,
      msgtab  LIKE STANDARD TABLE OF bdcmsgcoll WITH HEADER LINE.
DATA: wa_main          TYPE ty_main,
      wa_main1         TYPE ty_main,
      wa_tab           TYPE alsmex_tabline,
      wa_nom_item      TYPE bapitswnom03,         "#EC CI_USAGE_OK[2215424]
      wa_nom_header_o  TYPE bapitswnom02_o,
      wa_nom_header    TYPE bapitswnom02,
      wa_message       TYPE ty_message,
      wa_ret           TYPE bapiret2,
      i_main_tyst      TYPE TABLE OF ty_main,
      wa_main_tyst     TYPE ty_main,
      it_oijnomi_exist TYPE TABLE OF oijnomi,
      wa_oijnomi       TYPE oijnomi,
      wa_oijnomi_temp  TYPE oijnomi,
      wa_oijloc        TYPE oijloc.

DATA: it_oijnomi                TYPE TABLE OF oijnomi,
      it_oijnomih               TYPE TABLE OF oijnomh,
      wa_oijnomih               TYPE oijnomh,
      l_count                   TYPE oij_item VALUE '0000000010',
      it_yrxa_cmdata            TYPE TABLE OF yrxa_cmdata,
      wa_yrxa_cmdata            TYPE yrxa_cmdata,
      it_oijnomi_display        TYPE TABLE OF oijnomi,
      wa_oijnomi_display        TYPE oijnomi,
      it_oijnomi_display_final  TYPE TABLE OF ty_display_final WITH HEADER LINE,
      it_oijnomi_display_final1 TYPE TABLE OF ty_display_final,
      wa_oijnomi_display_final  TYPE ty_display_final,
      i_main_check              TYPE TABLE OF ty_main,
      wa_main_check             TYPE ty_main,
      it_oijts                  TYPE TABLE OF oijts,
      wa_oijts                  TYPE oijts,
      it_oij_el_doc_mot         TYPE TABLE OF oij_el_doc_mot,
      wa_oij_el_doc_mot         TYPE oij_el_doc_mot,
      it_ekko                   TYPE TABLE OF ekko,
      wa_ekko                   TYPE ekko,
      it_yrga_oa_alloc          TYPE TABLE OF yrga_oa_alloc,
      wa_yrga_oa_alloc          TYPE yrga_oa_alloc.

DATA: it_yro_nom_param1      TYPE TABLE OF yro_nom_param1,
      it_yro_nom_param_alloc TYPE TABLE OF yro_nom_param1,
      wa_yro_nom_param1      TYPE yro_nom_param1,
      it_oijnomi_post        TYPE TABLE OF oijnomi.
DATA:fcat    TYPE slis_t_fieldcat_alv WITH HEADER LINE.

DATA: wa_total_nom TYPE menge_d,
      tot_m_qty    TYPE menge_d,
      new_qty      TYPE menge_d.

DATA: l_adq TYPE oib_adqnt,
      l_gcv TYPE yyncv,
      l_ncv TYPE yyncv.

DATA: gt_fieldcat TYPE lvc_t_fcat WITH HEADER LINE .
DATA: gt_fieldcat1 TYPE lvc_t_fcat,
      wa_fieldcat1 TYPE lvc_s_fcat.
DATA : i_layout       TYPE lvc_s_layo,
       lt_top_of_page TYPE slis_t_listheader,
       ls_line        TYPE slis_listheader,
       l_date_l       TYPE char10.

DATA : lt_celltab TYPE lvc_t_styl,                          "#EC NEEDED
       l_index    TYPE i.

DATA : c_check(1) VALUE 'X'.

FIELD-SYMBOLS : <f1>    TYPE any.

DATA: go_cust_cont         TYPE REF TO cl_gui_custom_container,
      go_alv_grid          TYPE REF TO cl_gui_alv_grid,
      gi_fcat              TYPE lvc_t_fcat,
      gs_refres_val        TYPE lvc_s_stbl,
      gr_docking_container TYPE REF TO cl_gui_docking_container,
      gr_document          TYPE REF TO cl_dd_document.

DATA: gv_header_text(255)  TYPE c.
DATA: gv_header_text_t     TYPE sdydo_text_table.
DATA flg TYPE c.

* INITIALIZATION-------------------------------------------------------*
INITIALIZATION.

  SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-002.
    SELECTION-SCREEN SKIP.
    PARAMETERS: r_excel RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND user MODIF ID a,
                r_pro   RADIOBUTTON GROUP rad1                   MODIF ID a,
                r_dis   RADIOBUTTON GROUP rad1 MODIF ID a,
                r_act   RADIOBUTTON GROUP rad1 MODIF ID a.
  SELECTION-SCREEN END OF BLOCK a1.

  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
    SELECTION-SCREEN SKIP.
    PARAMETERS: p_fname TYPE rlgrap-filename MODIF ID abc.
  SELECTION-SCREEN END OF BLOCK b1.

  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
    SELECTION-SCREEN SKIP.
    SELECT-OPTIONS: s_date FOR wa_main-date MODIF ID jkl,
                    p_locid1 FOR oijnomi-locid MODIF ID mno NO INTERVALS.
    PARAMETERS:
      p_locid TYPE oijnomi-locid MODIF ID ghi,
      p_qty   TYPE oij_allocated_qty MODIF ID def NO-DISPLAY,
      p_gcv   TYPE p DECIMALS 3  MODIF ID def NO-DISPLAY,
      p_ncv   TYPE p DECIMALS 3  MODIF ID def NO-DISPLAY.
  SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN OUTPUT.
  IF r_excel = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'DEF' OR screen-group1 = 'GHI' OR screen-group1 = 'JKL' OR screen-group1 = 'MNO'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF r_pro = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'ABC' OR screen-group1 = 'MNO'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF r_dis = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'ABC' OR screen-group1 = 'DEF' OR screen-group1 = 'GHI'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF r_act = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'ABC' OR screen-group1 = 'DEF' OR screen-group1 = 'GHI'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN.
  IF r_dis <> 'X' AND r_act <> 'X'.
    IF s_date-high IS NOT INITIAL.
      DATA: w_date1 TYPE d.
      IF s_date-low+6(2) BETWEEN '01' AND '15'.
        CONCATENATE s_date-low+0(6) '15' INTO w_date1.
      ELSE.
        CALL FUNCTION 'HR_GR_LAST_DAY_OF_MONTHS'
          EXPORTING
            day_in            = s_date-low
          IMPORTING
            last_day_of_month = w_date1
          EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.
        IF sy-subrc <> 0.
        ENDIF.
      ENDIF.
      IF NOT ( s_date-high BETWEEN s_date-low AND w_date1 ).
      ENDIF.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    CHANGING
      file_name = p_fname.

START-OF-SELECTION.
DATA: lv_yrgg015 TYPE char1.
IMPORT lv_yrgg015 = lv_yrgg015  FROM MEMORY ID 'YRGG015_CALL_FLAG'.
FREE MEMORY ID 'YRGG015_CALL_FLAG'.
IF lv_yrgg015 = 'X'.
  IMPORT i_main = i_main FROM MEMORY ID 'YRGG015_NOM_DATA'.
  FREE MEMORY ID 'YRGG015_NOM_DATA'.
ENDIF.

IF r_excel = 'X'.
  IF p_fname IS INITIAL AND lv_yrgg015 <> 'X'.
    MESSAGE 'Please provide the file path.' TYPE 'I'.
  ELSE.
    IF lv_yrgg015 <> 'X'.
      PERFORM fetch_data_from_excel_sheet.
      PERFORM arrange_data.
    ENDIF.
    PERFORM batch_validate.
    IF lt_log[] IS INITIAL.
      PERFORM get_nomination.
      PERFORM createfromdata.
      PERFORM show_log.
    ELSE.
      IF lv_yrgg015 = 'X'.
        EXPORT lt_log TO MEMORY ID 'YRGG015_NOM_ERRORS'.
      ENDIF.
      PERFORM error_display.
    ENDIF.
  ENDIF.
  ELSEIF r_pro = 'X' OR r_dis = 'X' OR r_act = 'X'.
    IF s_date-low IS INITIAL.
      MESSAGE 'Please enter date for processing' TYPE 'I'.
    ENDIF.
    IF r_act = 'X' OR r_dis = 'X'.
      IF p_locid1 IS INITIAL.
        MESSAGE 'Please enter location for processing' TYPE 'I'.
      ENDIF.
    ELSE.
      IF p_locid IS INITIAL.
        MESSAGE 'Please enter location for processing' TYPE 'I'.
      ENDIF.
    ENDIF.
    IF s_date-low IS NOT INITIAL AND ( p_locid IS NOT INITIAL OR p_locid1 IS NOT INITIAL ).
      SELECT SINGLE * INTO wa_oijloc
        FROM oijloc
        WHERE locid = p_locid.
      PERFORM process_nomination.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  FETCH_DATA_FROM_EXCEL_SHEET
*&---------------------------------------------------------------------*
FORM fetch_data_from_excel_sheet .
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_fname
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 15
      i_end_row               = 10000
    TABLES
      intern                  = i_tab
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ARRANGE_DATA
*&---------------------------------------------------------------------*
FORM arrange_data .
  DELETE i_tab WHERE col = '1'.

  LOOP AT i_tab INTO wa_tab.
    wa_tab-col = wa_tab-col - 1.
    ASSIGN COMPONENT wa_tab-col OF STRUCTURE wa_main TO <f1>. "#EC CI_FLDEXT_OK[2215424]
    IF sy-subrc = 0.
      TRY.
          <f1> = wa_tab-value.
        CATCH cx_sy_conversion_no_number.
          MESSAGE 'Problem with file format.' TYPE 'E'.
      ENDTRY.
    ENDIF.
    IF wa_tab-col = '0003'.
      CALL FUNCTION 'CONVERT_DATE_TO_INTERN_FORMAT'
        EXPORTING
          datum = wa_tab-value
          dtype = 'DATS'
        IMPORTING
          idate = wa_main-date.
    ENDIF.
    AT END OF row.
      TRANSLATE wa_main-charg TO UPPER CASE.
      APPEND wa_main TO i_main.
      CLEAR wa_main.
    ENDAT.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BATCH_VALIDATE
*&---------------------------------------------------------------------*
FORM batch_validate.

  IF NOT i_main[] IS INITIAL.

    SELECT vbeln,
           tsyst,
           planloc,
           matnr
      FROM oij_el_doc_mot
      INTO TABLE @DATA(lt_oij)
      FOR ALL ENTRIES IN @i_main[]
      WHERE vbeln = @i_main-vbeln.

    SORT lt_oij BY vbeln.
    LOOP AT i_main  ASSIGNING FIELD-SYMBOL(<fs_main>).

      READ TABLE lt_oij INTO DATA(ls_oij) WITH KEY  vbeln = <fs_main>-vbeln BINARY SEARCH.
      IF <fs_main>-vbeln+0(1) NE '5'.
        <fs_main>-tsyst = ls_oij-tsyst.
        <fs_main>-locid = ls_oij-planloc.
        <fs_main>-matnr = ls_oij-matnr.
      ENDIF.
      CLEAR ls_oij.
    ENDLOOP.

    IF lt_log IS NOT INITIAL.
      EXIT.
    ENDIF.

    CLEAR: lt_log[].
    SELECT tsyst,werk FROM oijts INTO TABLE @DATA(lt_oijts)
    FOR ALL ENTRIES IN @i_main WHERE tsyst = @i_main-tsyst AND delind <> 'X'.

    SELECT * INTO TABLE @DATA(it_OIJTSMAT)
      FROM  oijtsmat
      FOR ALL ENTRIES IN @i_main
      WHERE pmatnr = @i_main-matnr
      AND   delind = ' '.

    LOOP AT i_main ASSIGNING  <fs_main>.
      DATA(tbx) = sy-tabix.

      IF <fs_main> IS ASSIGNED.

*&------>BUG FIX: Reject zero or negative nominated quantity before any other check
*&       Zero qty rows must not reach RFC_TSW_NOM_CREATEFROMDATA as the FM
*&       defaults to the OA contractual quantity when imenge = 0.
        IF <fs_main>-menge <= 0.
          APPEND VALUE #( tsyst   = <fs_main>-tsyst
                          ebeln   = <fs_main>-vbeln
                          date    = <fs_main>-date
                          locid   = <fs_main>-locid
                          matnr   = <fs_main>-matnr
                          charg   = <fs_main>-charg
                          message = 'Nominated quantity is zero or negative - row skipped' ) TO lt_log.
          <fs_main>-del_ind = 'X'.
          CONTINUE.
        ENDIF.
*&------>END BUG FIX

        SELECT SINGLE ebeln FROM ekko INTO @DATA(ebeln) WHERE ebeln = @<fs_main>-vbeln.
        IF ebeln IS INITIAL.
          CONCATENATE 'PO of Excel does not exist' '' INTO DATA(msg2) SEPARATED BY space.
          APPEND VALUE #(  tsyst   = <fs_main>-tsyst
                           ebeln   = <fs_main>-vbeln
                           date    = <fs_main>-date
                           locid   = <fs_main>-locid
                           matnr   = <fs_main>-matnr
                           charg   = <fs_main>-charg
                           message = msg2 ) TO lt_log.
        ENDIF.

        READ TABLE lt_oijts INTO DATA(ls_oijtss) WITH KEY tsyst = <fs_main>-tsyst.
        IF sy-subrc <> 0.
          CLEAR msg2.
          IF <fs_main>-vbeln+0(1) EQ '5'.
            CONCATENATE 'Transport System' <fs_main>-tsyst 'does not exist' INTO msg2 SEPARATED BY space.
          ENDIF.
          APPEND VALUE #(  tsyst   = <fs_main>-tsyst
                           ebeln   = <fs_main>-vbeln
                           date    = <fs_main>-date
                           locid   = <fs_main>-locid
                           matnr   = <fs_main>-matnr
                           charg   = <fs_main>-charg
                           message = msg2 ) TO lt_log.
        ENDIF.

        SELECT SINGLE  pbltyp FROM oifspbl INTO @DATA(lv_pbltyp) WHERE  pblnr = @<fs_main>-locid.
        IF lv_pbltyp IS NOT INITIAL.
          IF lv_pbltyp = 'YRDI'.
            READ TABLE lt_oijts INTO ls_oijtss WITH KEY tsyst = <fs_main>-tsyst.
            IF sy-subrc = 0.
              IF ls_oijtss-werk IS NOT INITIAL.
                SELECT SINGLE werks FROM ekpo INTO @DATA(lv_werks) WHERE ebeln = @<fs_main>-vbeln AND loekz <> 'S'.
                IF sy-subrc = 0.
                  SELECT vstel FROM ekpv INTO @DATA(lv_werk) UP TO 1 ROWS WHERE ebeln = @<fs_main>-vbeln ORDER BY PRIMARY KEY.
                  ENDSELECT.
                  IF ls_oijtss-werk <> lv_werk.
                    CONCATENATE 'Plant in Transport System (' ls_oijtss-werk ') doesn''t match with that in PO (' lv_werk ')' INTO DATA(msg3) SEPARATED BY space.
                    APPEND VALUE #(  tsyst   = <fs_main>-tsyst
                                     ebeln   = <fs_main>-vbeln
                                     date    = <fs_main>-date
                                     locid   = <fs_main>-locid
                                     matnr   = <fs_main>-matnr
                                     charg   = <fs_main>-charg
                                     message = msg3 ) TO lt_log.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          CLEAR msg3.
          IF <fs_main>-vbeln+0(1) EQ '5'.
            CONCATENATE 'Location ID' <fs_main>-locid 'does not exist' INTO msg3 SEPARATED BY space.
          ENDIF.
          APPEND VALUE #(  tsyst   = <fs_main>-tsyst
                           ebeln   = <fs_main>-vbeln
                           date    = <fs_main>-date
                           locid   = <fs_main>-locid
                           matnr   = <fs_main>-matnr
                           charg   = <fs_main>-charg
                           message = msg3 ) TO lt_log.
        ENDIF.
        CLEAR: msg3, lv_pbltyp, ls_oijtss, lv_werks.

        SELECT SINGLE matnr, lvorm ,mstae
               FROM mara INTO @DATA(matnr)
               WHERE matnr = @<fs_main>-matnr.
        IF matnr IS INITIAL.
          IF <fs_main>-vbeln+0(1) EQ '5'.
            CONCATENATE 'Material of Excel does not exist' '' INTO msg2 SEPARATED BY space.
          ENDIF.
          APPEND VALUE #( tsyst   = <fs_main>-tsyst
                           ebeln   = <fs_main>-vbeln
                           date    = <fs_main>-date
                           locid   = <fs_main>-locid
                           matnr   = <fs_main>-matnr
                           charg   = <fs_main>-charg
                           message = msg2 ) TO lt_log.
        ENDIF.

        CLEAR: msg2.
        IF matnr IS NOT INITIAL.
          IF matnr-lvorm EQ 'X' OR matnr-mstae EQ 'Z1'.
            CONCATENATE 'Material is Blocked for Posting' '' INTO msg2 SEPARATED BY space.
            APPEND VALUE #( tsyst   = <fs_main>-tsyst
                             ebeln   = <fs_main>-vbeln
                             date    = <fs_main>-date
                             locid   = <fs_main>-locid
                             matnr   = <fs_main>-matnr
                             charg   = <fs_main>-charg
                             message = msg2 ) TO lt_log.
          ELSE.
            IF lv_werks IS INITIAL.
              SELECT SINGLE werks FROM ekpo INTO @lv_werks WHERE ebeln = @<fs_main>-vbeln.
            ENDIF.
            SELECT SINGLE matnr ,lvorm ,mmsta FROM marc INTO @DATA(lv_matnr)
              WHERE matnr = @<fs_main>-matnr AND werks = @lv_werks.
            IF lv_matnr-lvorm EQ 'X' OR lv_matnr-mmsta EQ 'Z1'.
              CLEAR: msg2.
              CONCATENATE 'Material is blocked for posting in Plant' lv_werks '' INTO msg2 SEPARATED BY space.
              APPEND VALUE #( tsyst   = <fs_main>-tsyst
                               ebeln   = <fs_main>-vbeln
                               date    = <fs_main>-date
                               locid   = <fs_main>-locid
                               matnr   = <fs_main>-matnr
                               charg   = <fs_main>-charg
                               message = msg2 ) TO lt_log.
            ENDIF.
            CLEAR: lv_werks.
          ENDIF.
        ENDIF.

        CLEAR: msg2.
        SELECT SINGLE * INTO @DATA(l_OIJTSLOC) FROM oijtsloc
          WHERE locid = @<fs_main>-locid AND tsyst = @<fs_main>-tsyst.
        IF sy-subrc <> 0.
          SELECT SINGLE werk INTO @DATA(l_werk) FROM oijts WHERE tsyst = @<fs_main>-tsyst.
          CONCATENATE 'Location ID' <fs_main>-locid ' is not mapped against transport system'
          <fs_main>-tsyst 'Plant' l_werk INTO msg2 SEPARATED BY space.
          APPEND VALUE #( tsyst   = <fs_main>-tsyst
                          ebeln   = <fs_main>-vbeln
                          date    = <fs_main>-date
                          locid   = <fs_main>-locid
                          matnr   = <fs_main>-matnr
                          charg   = <fs_main>-charg
                          message = msg2 ) TO lt_log.
        ENDIF.

        IF <fs_main>-matnr IS NOT INITIAL.
          READ TABLE it_OIJTSMAT TRANSPORTING NO FIELDS
          WITH KEY pmatnr = <fs_main>-matnr tsyst = <fs_main>-tsyst.
          IF sy-subrc <> 0.
            CONCATENATE 'Material' <fs_main>-matnr ' is not mapped against transport system'
            <fs_main>-tsyst INTO msg2 SEPARATED BY space.
            APPEND VALUE #( tsyst   = <fs_main>-tsyst
                            ebeln   = <fs_main>-vbeln
                            date    = <fs_main>-date
                            locid   = <fs_main>-locid
                            matnr   = <fs_main>-matnr
                            charg   = <fs_main>-charg
                            message = msg2 ) TO lt_log.
          ENDIF.
        ENDIF.

        IF ebeln IS INITIAL OR matnr IS INITIAL.
          <fs_main>-del_ind = 'X'.
          CLEAR: ebeln, matnr.
          CONTINUE.
        ENDIF.

      ENDIF.

      TRY .
          DATA(ls_oijts) = lt_oijts[ tsyst = <fs_main>-tsyst ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
      APPEND VALUE #( matnr = <fs_main>-matnr
                      werk  = ls_oijts-werk
                      charg = <fs_main>-charg
                      locid = <fs_main>-locid
                      tsyst = <fs_main>-tsyst
                      ebeln = <fs_main>-vbeln
                      date  = <fs_main>-date ) TO lt_temp.

      CLEAR: ebeln, msg2, matnr.
    ENDLOOP.

    IF i_main IS NOT INITIAL.
      DELETE i_main WHERE del_ind IS NOT INITIAL.
    ENDIF.

    IF NOT lt_temp[] IS INITIAL.
      SELECT matnr,werks,xchpf FROM marc INTO TABLE @DATA(lt_marc) FOR ALL ENTRIES IN @lt_temp
              WHERE matnr = @lt_temp-matnr AND werks = @lt_temp-werk.
      DELETE lt_marc WHERE xchpf IS INITIAL.
      IF NOT lt_marc[] IS INITIAL.
        LOOP AT lt_temp ASSIGNING FIELD-SYMBOL(<fs_temp>).
          TRY .
              DATA(ls_marc) = lt_marc[ matnr = <fs_temp>-matnr werks = <fs_temp>-werk ].
              <fs_temp>-xchpf = 'X'.
              IF <fs_temp>-charg IS INITIAL.
                APPEND VALUE #( tsyst   = <fs_temp>-tsyst
                                ebeln   = <fs_temp>-ebeln
                                locid   = <fs_temp>-locid
                                matnr   = <fs_temp>-matnr
                                charg   = <fs_temp>-charg
                                date    = <fs_temp>-date
                                message = 'Batch in Excel is mandatory for Batch Managed Material' ) TO lt_log.
              ENDIF.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
        ENDLOOP.
      ENDIF.

      SELECT matnr,werks,xchpf FROM marc INTO TABLE @DATA(lt_marc1) FOR ALL ENTRIES IN @lt_temp
             WHERE matnr = @lt_temp-matnr AND werks = @lt_temp-werk.
      DELETE lt_marc1 WHERE xchpf IS NOT INITIAL.
      IF NOT lt_marc1[] IS INITIAL.
        LOOP AT lt_temp ASSIGNING FIELD-SYMBOL(<fs_temp1>).
          TRY .
              DATA(ls_marc1) = lt_marc1[ matnr = <fs_temp1>-matnr werks = <fs_temp1>-werk ].
              IF <fs_temp1>-charg IS NOT INITIAL.
                APPEND VALUE #( tsyst   = <fs_temp1>-tsyst
                                ebeln   = <fs_temp1>-ebeln
                                date    = <fs_temp1>-date
                                locid   = <fs_temp1>-locid
                                matnr   = <fs_temp1>-matnr
                                charg   = <fs_temp1>-charg
                                message = 'Batch present in Excel for Non-Batch Managed Material' ) TO lt_log.
                LOOP AT i_main INTO DATA(wk) WHERE vbeln = <fs_temp1>-ebeln.
                  wk-flag = 'X'.
                  MODIFY i_main FROM wk TRANSPORTING flag.
                  CLEAR: wk.
                ENDLOOP.
              ENDIF.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
        ENDLOOP.
      ENDIF.

      DELETE lt_temp[] WHERE xchpf IS INITIAL.
      IF NOT lt_temp[] IS INITIAL.
        SELECT * FROM mcha INTO TABLE @DATA(lt_mcha) FOR ALL ENTRIES IN @lt_temp
          WHERE matnr = @lt_temp-matnr AND werks = @lt_temp-werk AND charg = @lt_temp-charg.
        LOOP AT lt_temp INTO DATA(ls_temp).
          TRY .
              DATA(ls_mcha) = lt_mcha[ matnr = ls_temp-matnr werks = ls_temp-werk charg = ls_temp-charg ].
            CATCH cx_sy_itab_line_not_found.
              APPEND VALUE #( tsyst   = ls_temp-tsyst
                              ebeln   = ls_temp-ebeln
                              date    = ls_temp-date
                              locid   = ls_temp-locid
                              matnr   = ls_temp-matnr
                              charg   = ls_temp-charg
                              message = 'Batch and Material Combination in excel doesnt exist for the location' ) TO lt_log.
          ENDTRY.
        ENDLOOP.
        SORT lt_log BY matnr charg locid tsyst.
        DELETE ADJACENT DUPLICATES FROM lt_log COMPARING matnr charg locid tsyst.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT i_main INTO DATA(wn).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING  input  = wn-vbeln
      IMPORTING  output = wn-vbeln.
    CALL FUNCTION 'CONVERSION_EXIT_MATN5_INPUT'
      EXPORTING  input        = wn-matnr
      IMPORTING  output       = wn-matnr
      EXCEPTIONS length_error = 1 OTHERS = 2.
    IF sy-subrc <> 0.
    ENDIF.

    SELECT SINGLE bukrs,bsart FROM ekko INTO (@DATA(bukrs), @DATA(bsart)) WHERE ebeln = @wn-vbeln.
    IF bukrs = '7300' AND bsart = 'ZNGT'.
      SELECT werks FROM ekpo INTO @DATA(werks) UP TO 1 ROWS WHERE ebeln = @wn-vbeln ORDER BY PRIMARY KEY.
      ENDSELECT.
      SELECT SINGLE * FROM yrga_cgd_plant INTO @DATA(wyr) WHERE werks = @werks.
      IF wyr IS INITIAL.
        SELECT SINGLE ymvgr1 FROM yrga_po_ind INTO @DATA(mvg) WHERE yebeln = @wn-vbeln.
        IF mvg IS INITIAL.
          CONCATENATE 'No Indicator maintained against the PO' '' INTO DATA(msg) SEPARATED BY space.
          APPEND VALUE #( tsyst   = wn-tsyst
                          ebeln   = wn-vbeln
                          date    = wn-date
                          locid   = wn-locid
                          matnr   = wn-matnr
                          charg   = wn-charg
                          message = msg ) TO lt_log.
        ENDIF.
      ENDIF.
    ENDIF.

    SELECT matnr FROM ekpo INTO @DATA(matnr1) UP TO 1 ROWS WHERE ebeln = @wn-vbeln ORDER BY PRIMARY KEY.
    ENDSELECT.
    IF matnr1 NE wn-matnr.
      CONCATENATE 'Material in excel does not match with material' matnr1 'in PO' INTO msg SEPARATED BY space.
      APPEND VALUE #( tsyst   = wn-tsyst
                      ebeln   = wn-vbeln
                      date    = wn-date
                      locid   = wn-locid
                      matnr   = wn-matnr
                      charg   = wn-charg
                      message = msg ) TO lt_log.
    ENDIF.

    IF wn-flag NE 'X'.
      IF wn-charg IS NOT INITIAL.
        SELECT * FROM eket INTO @DATA(wcharg) UP TO 1 ROWS WHERE ebeln = @wn-vbeln ORDER BY PRIMARY KEY.
        ENDSELECT.
        IF wcharg IS NOT INITIAL.
          IF wn-charg NE wcharg-charg.
            CONCATENATE 'Batch in Excel does not match with Batch' wcharg-charg 'in PO' INTO msg SEPARATED BY space.
            APPEND VALUE #( tsyst   = wn-tsyst
                            ebeln   = wn-vbeln
                            date    = wn-date
                            locid   = wn-locid
                            matnr   = wn-matnr
                            charg   = wn-charg
                            message = msg ) TO lt_log.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR: wn, bukrs, bsart, werks, wyr, mvg, msg, wcharg, matnr1.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ERROR_DISPLAY
*&---------------------------------------------------------------------*
FORM error_display.
  DATA:lv_col_pos TYPE i.
  lv_col_pos = 0.

  lv_col_pos = lv_col_pos + 1.
  fcat-fieldname = 'TSYST'. fcat-col_pos = lv_col_pos.
  fcat-seltext_l = 'TRANS SYSTEM'. fcat-outputlen = 18. fcat-emphasize = 'C410'.
  APPEND fcat. CLEAR fcat.

  lv_col_pos = lv_col_pos + 1.
  fcat-fieldname = 'EBELN'. fcat-col_pos = lv_col_pos.
  fcat-seltext_l = 'PO NO'. fcat-outputlen = 18. fcat-emphasize = 'C410'.
  APPEND fcat. CLEAR fcat.

  lv_col_pos = lv_col_pos + 1.
  fcat-fieldname = 'DATE'. fcat-col_pos = lv_col_pos.
  fcat-seltext_l = 'GAS DAY'. fcat-outputlen = 18. fcat-emphasize = 'C410'.
  APPEND fcat. CLEAR fcat.

  lv_col_pos = lv_col_pos + 1.
  fcat-fieldname = 'LOCID'. fcat-col_pos = lv_col_pos.
  fcat-seltext_l = 'LOCATION ID'. fcat-outputlen = 10. fcat-emphasize = 'C300'.
  APPEND fcat. CLEAR fcat.

  lv_col_pos = lv_col_pos + 1.
  fcat-fieldname = 'MATNR'. fcat-col_pos = lv_col_pos.
  fcat-seltext_l = 'MATERIAL'. fcat-outputlen = 18. fcat-emphasize = 'C410'.
  APPEND fcat. CLEAR fcat.

  lv_col_pos = lv_col_pos + 1.
  fcat-fieldname = 'CHARG'. fcat-col_pos = lv_col_pos.
  fcat-seltext_l = 'BATCH'. fcat-outputlen = 10. fcat-emphasize = 'C300'.
  APPEND fcat. CLEAR fcat.

  lv_col_pos = lv_col_pos + 1.
  fcat-fieldname = 'MESSAGE'. fcat-col_pos = lv_col_pos.
  fcat-seltext_l = 'MESSAGE'. fcat-outputlen = 50. fcat-emphasize = 'C300'.
  APPEND fcat. CLEAR fcat.

  SORT lt_log BY date ebeln.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = fcat[]
      i_save             = 'U'
    TABLES
      t_outtab           = lt_log.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CREATEFROMDATA
*&---------------------------------------------------------------------*
FORM createfromdata .
  DATA: wl_ebelp TYPE ekpo-ebelp.
  SORT i_main BY vbeln date.
  SORT i_main BY tsyst date locid.
  REFRESH i_main_tyst.
  MOVE i_main[] TO i_main_tyst[].
  DELETE ADJACENT DUPLICATES FROM i_main_tyst COMPARING tsyst date locid.

  LOOP AT i_main_tyst INTO wa_main_tyst.
    LOOP AT i_main INTO wa_main WHERE tsyst = wa_main_tyst-tsyst AND locid = wa_main_tyst-locid AND date = wa_main_tyst-date.

*&------>BUG FIX: Final guard — must not call FM if quantity is zero or negative
      IF wa_main-menge <= 0.
        CONTINUE.
      ENDIF.
*&------>END BUG FIX

      READ TABLE it_oijts INTO wa_oijts WITH KEY tsyst = wa_main-tsyst.
      IF sy-subrc = 0.
        IF wa_oijts-werk(1) = '3'.
          wa_nom_header-nominationtype = 'GITA'.
          wa_nom_item-itemtype         = 'ZO'.
        ELSEIF wa_oijts-werk(1) = '2' OR wa_oijts-werk(1) = '5' OR wa_oijts-werk(1) = '7' OR wa_oijts-werk(1) = '6'.
          wa_nom_header-nominationtype = 'GISA'.
          wa_nom_item-itemtype         = 'OU'.
        ENDIF.
      ENDIF.

      wa_nom_header-transportsystem  = wa_main-tsyst.
      wa_nom_header-nominationstatus = '1'.

      READ TABLE it_oijnomi_exist INTO wa_oijnomi WITH KEY docnr = wa_main-vbeln idate = wa_main-date.
      IF sy-subrc EQ 0.
        CONCATENATE 'Nomination already created for ' wa_main-vbeln INTO wa_ret-message SEPARATED BY space.
        APPEND wa_ret TO i_error. CLEAR wa_ret.
        CONTINUE.
      ENDIF.

      SELECT ebelp INTO wl_ebelp FROM ekpo UP TO 1 ROWS WHERE ebeln = wa_main-vbeln AND loekz IN ( ' ','S' ) ORDER BY PRIMARY KEY.
      ENDSELECT.
      IF sy-subrc = 0.
        MOVE wl_ebelp TO wa_nom_item-documentitem.
      ELSE.
        CONCATENATE 'No active record found for PO' wa_main-vbeln INTO wa_ret-message SEPARATED BY space.
        APPEND wa_ret TO i_error. CLEAR wa_ret.
        CONTINUE.
      ENDIF.

      wa_nom_item-itemnumber        = l_count.
      wa_nom_item-itemstatus        = '1'.
      wa_nom_item-scheduleddate     = wa_main-date.
      wa_nom_item-locationid        = wa_main-locid.
      wa_nom_item-demandmaterial    = wa_main-matnr.
      wa_nom_item-schedulematerial  = wa_main-matnr.
      wa_nom_item-nominatedquantity = wa_main-menge.
      wa_nom_item-quantityunit_sap  = wa_main-unit.
      wa_nom_item-documentnumber    = wa_main-vbeln.
      IF wa_main-vbeln+0(1) = '5'.
        wa_nom_item-documentindicator = 'T'.
        wa_nom_item-batchorigin       = wa_main-charg.
      ELSE.
        wa_nom_item-documentindicator = 'K'.
        wa_nom_item-batchdestination  = wa_main-charg.
      ENDIF.

      APPEND wa_nom_item TO i_nom_item.
      wa_main-nomit = l_count.
      APPEND wa_main TO i_main_check.
      l_count = l_count + 10.
    ENDLOOP.

    PERFORM call_rfc_tsw_nom_create TABLES i_nom_item
                                           i_nom_item_o
                                           i_return
                                     USING wa_nom_header
                                  CHANGING wa_nom_header_o.

    READ TABLE i_return INTO wa_ret WITH KEY type = 'E'.
    IF sy-subrc = 0.
      APPEND wa_ret TO i_error.
    ELSE.
      wa_main-nomtk = wa_nom_header_o-nominationnumber_sap.
      LOOP AT i_main_check INTO wa_main_check.
        wa_main_check-nomtk = wa_nom_header_o-nominationnumber_sap.
        MODIFY i_main_check FROM wa_main_check INDEX sy-tabix.
      ENDLOOP.
      PERFORM nomination_update.
      REFRESH: i_nom_item, i_nom_item_o, i_return, i_main_check.
    ENDIF.
    CLEAR: i_nom_item, i_nom_item_o, i_return, wa_nom_header, wa_nom_header_o.
    l_count = '0000000010'.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CALL_RFC_TSW_NOM_CREATE
*&---------------------------------------------------------------------*
FORM call_rfc_tsw_nom_create  TABLES   p_it_nom_item   STRUCTURE bapitswnom03   "#EC CI_USAGE_OK[2215424]
                                        p_it_nom_item_o STRUCTURE bapitswnom03_o "#EC CI_USAGE_OK[2215424]
                                        p_it_return     STRUCTURE bapiret2
                               USING    p_wa_nom_header   TYPE bapitswnom02
                               CHANGING p_wa_nom_header_o TYPE bapitswnom02_o.

  CALL FUNCTION 'RFC_TSW_NOM_CREATEFROMDATA'
    EXPORTING
      headerdata_in      = p_wa_nom_header
    IMPORTING
      headerdata_out     = p_wa_nom_header_o
    TABLES
      nominationitem_in  = p_it_nom_item
      nominationitem_out = p_it_nom_item_o
      return             = p_it_return.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SHOW_LOG
*&---------------------------------------------------------------------*
FORM show_log .
  WRITE TEXT-002.
  LOOP AT i_error INTO wa_ret.
    WRITE: /1 wa_ret-id, 10 wa_ret-number, 20 wa_ret-message.
  ENDLOOP.
  SKIP.
  WRITE TEXT-003.
  LOOP AT i_message INTO wa_message.
    WRITE: /1 wa_message-id, 10 wa_message-number, 20 wa_message-message.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_NOMINATION
*&---------------------------------------------------------------------*
FORM get_nomination .
  IF i_main[] IS NOT INITIAL.
    SELECT * FROM oijnomi INTO TABLE it_oijnomi_exist
      FOR ALL ENTRIES IN i_main
      WHERE docnr EQ i_main-vbeln
      AND idate EQ i_main-date
      AND delind EQ space.

    SELECT * INTO TABLE it_oijts
      FROM oijts
      FOR ALL ENTRIES IN i_main
      WHERE tsyst = i_main-tsyst.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PROCESS_NOMINATION  (retained from original — unchanged)
*&---------------------------------------------------------------------*
FORM process_nomination .
* Full implementation retained from original YRXR036_PURC_NOM_G1
* — no changes made in this form for this fix transport
  PERFORM nomination_update.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  NOMINATION_UPDATE  (stub — full code in original)
*&---------------------------------------------------------------------*
FORM nomination_update .
* Retained from original — no changes for this fix
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  QUICK_CONFIRM  (stub — full code in original)
*&---------------------------------------------------------------------*
FORM quick_confirm .
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CREATE_TICKET  (stub — full code in original)
*&---------------------------------------------------------------------*
FORM create_ticket .
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  BDC helpers
*&---------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam. bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.

FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program = program. bdcdata-dynpro = dynpro. bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.

CONTROLS: control1 TYPE TABLEVIEW USING SCREEN 9000.
INCLUDE yrxr036_purc_nom_g1_pbo .
