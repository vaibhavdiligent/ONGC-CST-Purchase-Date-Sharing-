*-----------------------------------------------------------------------------------*
* PROGRAM DESCRIPTION: Purchase Nomination Upload
*
* DEVELOPER: Karavadi Ravi Chandra
* CREATION DATE: 24-07-2023
* TR NUMBER: 	DVRK9A13PA
* REFRENCE PROGRAM : YRXR019_PURC_NOM_G1
*-----------------------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Report YRXR036_PURC_NOM_G1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT yrxr036_purc_nom_g1.

TABLES : yro_nom_param1,oijnomi.
* TYPES------------------------------------------------------------------------------*
TYPES: BEGIN OF ty_main,
*          desc type
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
**Column Order of Date, Locid, Rank is changed to facilitate AT END OF RANK.
TYPES: BEGIN OF ty_main_r,
         date  TYPE aedat,
         locid TYPE oij_locid,
         rank  TYPE i,
         nomtk TYPE oij_nomtk,
         nomit TYPE oij_item,
       END OF ty_main_r.
**EOC BY RAVI/SHREYOSI ON 05.10.2023 CHARM: 4000007179, KRC:GMS: YRGR040_multpl date_PGLS

TYPES: BEGIN OF ty_message,
         id      TYPE	symsgid,
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
*soc by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024
          sel              TYPE char1,
          version          TYPE oij_el_tkt_version,
          message(132)     TYPE c,
          stat(1)          TYPE c,
*eoc by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024
          idate	           TYPE oij_idate,
          locid            TYPE oij_locid,
          nomtk	           TYPE oij_nomtk,
          nomit	           TYPE oij_item,
          matnr_i	         TYPE oij_matnri,
          charg_o          TYPE oij_chargo,
          charg_d          TYPE oij_chargo,
          docnr	           TYPE oij_docnr,
          yyoij_cnom_qty   TYPE yyoij_cnom_qty,
          yyoij_cnom_uom   TYPE yyoij_cnom_uom,
          menge	           TYPE oij_menge,
          unit_i           TYPE oij_uniti,
          ga_rank	         TYPE oij_rank,
          ga_allocated_qty TYPE oij_allocated_qty,
          yyoij_dnimb_qty	 TYPE yyoij_dnimb_qty,
          yyoij_dpimb_qty	 TYPE yyoij_dpimb_qty,
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
** -> Begin of changes by of Aditi on 11.12.2024 16:30:21 for ATC
      i_nom_item   TYPE TABLE OF bapitswnom03, "#EC CI_USAGE_OK[2215424]
      i_nom_item_o TYPE TABLE OF bapitswnom03_o, "#EC CI_USAGE_OK[2215424]
** <- End changes by of Aditi on 11.12.2024 16:30:25 for ATC
      i_return     TYPE TABLE OF bapiret2,
      i_error      TYPE TABLE OF bapiret2,
      i_message    TYPE TABLE OF ty_message.
DATA: bdcdata TYPE TABLE OF bdcdata WITH HEADER LINE,
      msgtab  LIKE STANDARD TABLE OF bdcmsgcoll WITH HEADER LINE.
DATA: wa_main          TYPE ty_main,
      wa_main1         TYPE ty_main,
      wa_tab           TYPE alsmex_tabline,
** -> Begin of changes by of Aditi on 11.12.2024 16:30:48 for ATC
      wa_nom_item      TYPE bapitswnom03,     "#EC CI_USAGE_OK[2215424]
** <- End changes by of Aditi on 11.12.2024 16:30:52 for ATC
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
*soc by MANMOHAN/SHREYOSI Charm:4000008514 date01.08.2024
      it_oijnomi_display_final1 TYPE TABLE OF ty_display_final,
*eoc by MANMOHAN/SHREYOSI Charm:4000008514 date:01.08.2024
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

* VARIABLES

DATA: wa_total_nom TYPE menge_d,
      tot_m_qty    TYPE menge_d,
      new_qty      TYPE menge_d.

DATA: l_adq TYPE oib_adqnt,
      l_gcv TYPE yyncv,
      l_ncv TYPE yyncv.

DATA: gt_fieldcat TYPE lvc_t_fcat WITH HEADER LINE .
*soc by MANMOHAN/SHREYOSI Charm:4000008514 date01.08.2024
DATA: gt_fieldcat1 TYPE lvc_t_fcat,
      wa_fieldcat1 TYPE lvc_s_fcat.
*Eoc by MANMOHAN/SHREYOSI Charm:4000008514 date01.08.2024
DATA : i_layout       TYPE lvc_s_layo,
       lt_top_of_page TYPE slis_t_listheader,
       ls_line        TYPE slis_listheader,
       l_date_l       TYPE char10.

DATA : lt_celltab TYPE lvc_t_styl,                          "#EC NEEDED
       l_index    TYPE i.

DATA : c_check(1) VALUE 'X'.

FIELD-SYMBOLS : <f1>    TYPE any.

*soc by MANMOHAN/SHREYOSI Charm:4000008514 date01.08.2024
DATA: go_cust_cont         TYPE REF TO cl_gui_custom_container,
      go_alv_grid          TYPE REF TO cl_gui_alv_grid,
      gi_fcat              TYPE lvc_t_fcat,
      gs_refres_val        TYPE lvc_s_stbl,
      gr_docking_container TYPE REF TO cl_gui_docking_container,
      gr_document          TYPE REF TO cl_dd_document.

DATA: gv_header_text(255)  TYPE c.

DATA: gv_header_text_t     TYPE sdydo_text_table.

DATA flg TYPE c.
*eoc by MANMOHAN/SHREYOSI Charm:4000008514 date01.08.2024

* INITIALIZATION-------------------------------------------------------*
INITIALIZATION.


  SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-002.
    SELECTION-SCREEN SKIP.
    PARAMETERS: r_excel RADIOBUTTON GROUP rad1 DEFAULT 'X' USER-COMMAND user MODIF ID a,
                r_pro   RADIOBUTTON GROUP rad1                   MODIF ID a,
                r_dis   RADIOBUTTON GROUP rad1 MODIF ID a,
*soc by MANMOHAN/SHREYOSI Charm:4000008514 date01.08.2024
                r_act   RADIOBUTTON GROUP rad1 MODIF ID a.
*eoc by MANMOHAN/SHREYOSI Charm:4000008514 date01.08.2024
  SELECTION-SCREEN END OF BLOCK a1.

*SELECTION-SCREEN
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
    SELECTION-SCREEN SKIP.
    PARAMETERS: p_fname TYPE rlgrap-filename MODIF ID abc.
  SELECTION-SCREEN END OF BLOCK b1.


  SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
    SELECTION-SCREEN SKIP.
    SELECT-OPTIONS: s_date FOR wa_main-date MODIF ID jkl,
*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024
                    p_locid1 FOR oijnomi-locid MODIF ID mno NO INTERVALS.
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024
    PARAMETERS:
      p_locid TYPE oijnomi-locid MODIF ID ghi,
      p_qty   TYPE oij_allocated_qty MODIF ID def NO-DISPLAY,
      p_gcv   TYPE p DECIMALS 3  MODIF ID def NO-DISPLAY,
      p_ncv   TYPE p DECIMALS 3  MODIF ID def NO-DISPLAY.
  SELECTION-SCREEN END OF BLOCK b2.



AT SELECTION-SCREEN OUTPUT.
  IF r_excel = 'X'.
    LOOP AT SCREEN.
*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024
      IF screen-group1 = 'DEF' OR screen-group1 = 'GHI' OR screen-group1 = 'JKL' OR screen-group1 = 'MNO'.
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSEIF r_pro = 'X'.
    LOOP AT SCREEN.
*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024
      IF screen-group1 = 'ABC' OR screen-group1 = 'MNO'.
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSEIF r_dis = 'X'.
    LOOP AT SCREEN.
*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024
      IF screen-group1 = 'ABC' OR screen-group1 = 'DEF' OR screen-group1 = 'GHI'.
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024

  ELSEIF r_act = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'ABC' OR screen-group1 = 'DEF' OR screen-group1 = 'GHI'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024


  ENDIF.


AT SELECTION-SCREEN.

  IF r_dis <> 'X' AND r_act <> 'X'.           "++ by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024
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


*&---------------------------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST (Fetch the filename)     *
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    CHANGING
      file_name = p_fname.

* START OF SELECTION---------------------------------------------------*
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
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024
    IF s_date-low IS INITIAL.
      MESSAGE 'Please enter date for processing' TYPE 'I'.
    ENDIF.
*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024
    IF r_act = 'X' OR r_dis = 'X'.
      IF p_locid1 IS INITIAL.
        MESSAGE 'Please enter location for processing' TYPE 'I'.
      ENDIF.
    ELSE.
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024
      IF p_locid IS INITIAL.
        MESSAGE 'Please enter location for processing' TYPE 'I'.
      ENDIF.
*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024
    ENDIF.
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024
    IF s_date-low IS NOT INITIAL AND ( p_locid IS NOT INITIAL OR p_locid1 IS NOT INITIAL ).
      SELECT SINGLE * INTO wa_oijloc
        FROM oijloc
        WHERE
        locid = p_locid.
      PERFORM process_nomination.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  FETCH_DATA_FROM_EXCEL_SHEET
*&---------------------------------------------------------------------*
*       fetch Data from Excel
*----------------------------------------------------------------------*
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
ENDFORM.                    " FETCH_DATA_FROM_EXCEL_SHEET

*SOC BY YASHU/SHREYOSI : CHARM: 4000004403: YS:YRGR040_batch check
FORM batch_validate.


  IF NOT i_main[] IS INITIAL.
*&------>SOC CHARM ID :4000008742  |TECHICAL : RAVINDER SINGH| FUNCTIONAL : SHREYOSHI |DT:10.10.2024
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
*&------>EOC CHARM ID :4000008742  |TECHICAL : RAVINDER SINGH| FUNCTIONAL : SHREYOSHI |DT:06.09.2024

    IF lt_log IS NOT INITIAL.
      EXIT.
    ENDIF.

    CLEAR: lt_log[].
    SELECT tsyst,werk FROM oijts INTO TABLE @DATA(lt_oijts)
    FOR ALL ENTRIES IN @i_main WHERE tsyst = @i_main-tsyst AND delind <> 'X'.

    SELECT * INTO TABLE @DATA(it_OIJTSMAT)
      FROM  oijtsmat
      FOR ALL ENTRIES IN @i_main
      WHERE
      pmatnr = @i_main-matnr
      AND
      delind = ' '.

    LOOP AT i_main ASSIGNING  <fs_main>.
      DATA(tbx) = sy-tabix.

      IF <fs_main> IS ASSIGNED.


        SELECT SINGLE ebeln FROM ekko INTO @DATA(ebeln) WHERE ebeln = @<fs_main>-vbeln.
        IF ebeln IS INITIAL.

          CONCATENATE 'PO of Excel does not exist' '' INTO DATA(msg2) SEPARATED BY space.
          APPEND VALUE #(  tsyst = <fs_main>-tsyst
                           ebeln = <fs_main>-vbeln
                           date  = <fs_main>-date
                           locid = <fs_main>-locid
                           matnr = <fs_main>-matnr
                           charg = <fs_main>-charg
                           message = msg2 ) TO lt_log.

        ENDIF.

        READ TABLE lt_oijts INTO DATA(ls_oijtss) WITH KEY tsyst = <fs_main>-tsyst.
        IF sy-subrc <> 0.
          CLEAR msg2.
**&------>SOC CHARM ID :4000008742  |TECHICAL : RAVINDER SINGH| FUNCTIONAL : SHREYOSHI |DT:06.09.2024
          IF <fs_main>-vbeln+0(1) EQ '5'.
            CONCATENATE 'Transport System' <fs_main>-tsyst 'does not exist' INTO msg2 SEPARATED BY space.
          ENDIF.
**&------>SOC CHARM ID :4000008742  |TECHICAL : RAVINDER SINGH| FUNCTIONAL : SHREYOSHI |DT:06.09.2024

          APPEND VALUE #(  tsyst = <fs_main>-tsyst
                           ebeln = <fs_main>-vbeln
                           date  = <fs_main>-date
                           locid = <fs_main>-locid
                           matnr = <fs_main>-matnr
                           charg = <fs_main>-charg
                           message = msg2 ) TO lt_log.

        ENDIF.

*&------> SOC BY ABID HUSSAIN/SHREYOSI DE: TR:DVRK9A1A9K(4000008425) FOR PLANT VALIDATION.

        SELECT SINGLE  pbltyp FROM oifspbl INTO @DATA(lv_pbltyp) WHERE  pblnr = @<fs_main>-locid.
        IF lv_pbltyp IS NOT INITIAL.
          IF lv_pbltyp = 'YRDI'.
            READ TABLE lt_oijts INTO ls_oijtss
             WITH KEY tsyst = <fs_main>-tsyst.
            IF sy-subrc = 0.
              IF ls_oijtss-werk IS NOT INITIAL.
                SELECT SINGLE werks FROM ekpo INTO @DATA(lv_werks) WHERE ebeln = @<fs_main>-vbeln AND loekz <> 'S'.
                IF sy-subrc = 0.
** -> Begin of changes by of Aditi on 03.12.2024 17:23:21 for ATC
                  SELECT vstel FROM ekpv INTO @DATA(lv_werk) UP TO 1 ROWS WHERE ebeln = @<fs_main>-vbeln ORDER BY PRIMARY KEY.
                  ENDSELECT.
** <- End changes by of Aditi on 03.12.2024 17:23:27 for ATC
                  IF ls_oijtss-werk <> lv_werk.

                    CONCATENATE 'Plant in Transport System (' ls_oijtss-werk ') doesn''t match with that in PO (' lv_werk ')' INTO DATA(msg3) SEPARATED BY space.

                    APPEND VALUE #(  tsyst = <fs_main>-tsyst
                                     ebeln = <fs_main>-vbeln
                                     date  = <fs_main>-date
                                     locid = <fs_main>-locid
                                     matnr = <fs_main>-matnr
                                     charg = <fs_main>-charg
                                     message = msg3 ) TO lt_log.
                  ENDIF.
                ENDIF.
              ENDIF.



            ENDIF.
          ENDIF.
        ELSE.

          CLEAR msg3.
**&------>SOC CHARM ID :4000008742  |TECHICAL : RAVINDER SINGH| FUNCTIONAL : SHREYOSHI |DT:06.09.2024
          IF <fs_main>-vbeln+0(1) EQ '5'.
            CONCATENATE 'Location ID' <fs_main>-locid 'does not exist' INTO msg3 SEPARATED BY space.
          ENDIF.
**&------>SOC CHARM ID :4000008742  |TECHICAL : RAVINDER SINGH| FUNCTIONAL : SHREYOSHI |DT:06.09.2024
          APPEND VALUE #(  tsyst = <fs_main>-tsyst
                           ebeln = <fs_main>-vbeln
                           date  = <fs_main>-date
                           locid = <fs_main>-locid
                           matnr = <fs_main>-matnr
                           charg = <fs_main>-charg
                           message = msg3 ) TO lt_log.
        ENDIF.
        CLEAR: msg3,  lv_pbltyp, ls_oijtss,lv_werks.

*&------> EOC BY ABID HUSSAIN/SHREYOSI DE: TR:DVRK9A1A9K(4000008425) FOR PLANT VALIDATION.


        SELECT SINGLE matnr, lvorm ,mstae
               FROM mara INTO @DATA(matnr)
               WHERE matnr = @<fs_main>-matnr.
        IF matnr IS INITIAL.
          IF <fs_main>-vbeln+0(1) EQ '5'.
            CONCATENATE 'Material of Excel does not exist' '' INTO msg2 SEPARATED BY space.
          ENDIF.
          APPEND VALUE #( tsyst = <fs_main>-tsyst
                           ebeln = <fs_main>-vbeln
                           date  = <fs_main>-date
                           locid = <fs_main>-locid
                           matnr = <fs_main>-matnr
                           charg = <fs_main>-charg
                           message = msg2 ) TO lt_log.


        ENDIF.

        CLEAR: msg2.
        IF matnr IS NOT INITIAL .
          IF matnr-lvorm EQ 'X' OR matnr-mstae EQ 'Z1'.
            CONCATENATE 'Material is Blocked for Posting' '' INTO msg2 SEPARATED BY space.

            APPEND VALUE #( tsyst = <fs_main>-tsyst
                                     ebeln = <fs_main>-vbeln
                                     date  = <fs_main>-date
                                     locid = <fs_main>-locid
                                     matnr = <fs_main>-matnr
                                     charg = <fs_main>-charg
                                     message = msg2 ) TO lt_log.

          ELSE.
            IF lv_werks IS INITIAL .
              SELECT SINGLE werks
                    FROM ekpo INTO @lv_werks
                    WHERE ebeln = @<fs_main>-vbeln.
            ENDIF.

            SELECT SINGLE  matnr ,lvorm ,mmsta
                FROM marc INTO @DATA(lv_matnr)
                WHERE matnr = @<fs_main>-matnr
                AND werks = @lv_werks.
            IF lv_matnr-lvorm EQ 'X' OR lv_matnr-mmsta EQ 'Z1'.
              CLEAR: msg2.
              CONCATENATE 'Material is blocked for posting in Plant'  lv_werks'' INTO msg2 SEPARATED BY space.

              APPEND VALUE #( tsyst = <fs_main>-tsyst
                               ebeln = <fs_main>-vbeln
                               date  = <fs_main>-date
                               locid = <fs_main>-locid
                               matnr = <fs_main>-matnr
                               charg = <fs_main>-charg
                               message = msg2 ) TO lt_log.

            ENDIF.
            CLEAR :lv_werks.

          ENDIF.
        ENDIF.
        CLEAR: msg2.
        SELECT SINGLE * INTO @DATA(l_OIJTSLOC)
          FROM oijtsloc
          WHERE
          locid = @<fs_main>-locid
          AND
          tsyst = @<fs_main>-tsyst.
        IF sy-subrc <> 0.
          SELECT SINGLE werk INTO @DATA(l_werk)
            FROM oijts
            WHERE
            tsyst = @<fs_main>-tsyst.
          CONCATENATE 'Location ID' <fs_main>-locid ' is not mapped against transport system'
          <fs_main>-tsyst 'Plant' l_werk
          INTO msg2 SEPARATED BY space.
          APPEND VALUE #( tsyst = <fs_main>-tsyst
                                  ebeln = <fs_main>-vbeln
                                  date  = <fs_main>-date
                                  locid = <fs_main>-locid
                                  matnr = <fs_main>-matnr
                                  charg = <fs_main>-charg
                                  message = msg2 ) TO lt_log.

        ENDIF.

        IF <fs_main>-matnr IS NOT INITIAL.
          READ TABLE it_OIJTSMAT TRANSPORTING NO FIELDS
          WITH KEY pmatnr = <fs_main>-matnr tsyst = <fs_main>-tsyst.
          IF sy-subrc <> 0.
            CONCATENATE 'Material' <fs_main>-matnr ' is not mapped against transport system'
          <fs_main>-tsyst
          INTO msg2 SEPARATED BY space.
            APPEND VALUE #( tsyst = <fs_main>-tsyst
                                    ebeln = <fs_main>-vbeln
                                    date  = <fs_main>-date
                                    locid = <fs_main>-locid
                                    matnr = <fs_main>-matnr
                                    charg = <fs_main>-charg
                                    message = msg2 ) TO lt_log.
          ENDIF.
        ENDIF.

        IF ebeln IS INITIAL OR matnr IS INITIAL.

          <fs_main>-del_ind = 'X'.


          CLEAR: ebeln , matnr."

          CONTINUE.
        ENDIF.


      ENDIF.


      TRY .
          DATA(ls_oijts) = lt_oijts[ tsyst = <fs_main>-tsyst ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
      APPEND VALUE #( matnr = <fs_main>-matnr
                      werk = ls_oijts-werk
                      charg = <fs_main>-charg
                      locid =  <fs_main>-locid
                      tsyst =  <fs_main>-tsyst
                      ebeln =  <fs_main>-vbeln
                      date  =  <fs_main>-date  ) TO lt_temp.

      CLEAR:ebeln,msg2, msg2, matnr.
    ENDLOOP.

    IF i_main IS NOT INITIAL.
      DELETE i_main WHERE del_ind IS NOT INITIAL.
    ENDIF.


    IF NOT lt_temp[] IS INITIAL.
      SELECT matnr,werks,xchpf FROM marc INTO TABLE @DATA(lt_marc) FOR ALL ENTRIES IN @lt_temp
              WHERE matnr = @lt_temp-matnr AND werks = @lt_temp-werk.

      DELETE lt_marc WHERE xchpf IS INITIAL.
      IF NOT lt_marc[] IS INITIAL .
        LOOP AT lt_temp ASSIGNING FIELD-SYMBOL(<fs_temp>).
          TRY .
              DATA(ls_marc) = lt_marc[ matnr = <fs_temp>-matnr werks = <fs_temp>-werk ].
              <fs_temp>-xchpf = 'X'.
              IF <fs_temp>-charg IS INITIAL .
                APPEND VALUE #(  tsyst = <fs_temp>-tsyst
                           ebeln = <fs_temp>-ebeln
                           locid = <fs_temp>-locid
                           matnr = <fs_temp>-matnr
                           charg = <fs_temp>-charg
                           date  = <fs_temp>-date
                           message = 'Batch in Excel is mandatory for Batch Managed Material' ) TO lt_log.
              ENDIF.

            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
        ENDLOOP.
      ENDIF.



      SELECT matnr,werks,xchpf FROM marc INTO TABLE @DATA(lt_marc1) FOR ALL ENTRIES IN @lt_temp
             WHERE matnr = @lt_temp-matnr AND werks = @lt_temp-werk.

      DELETE lt_marc1 WHERE xchpf IS NOT INITIAL.

      IF NOT lt_marc1[] IS INITIAL .
        LOOP AT lt_temp ASSIGNING FIELD-SYMBOL(<fs_temp1>).
          TRY .
              DATA(ls_marc1) = lt_marc1[ matnr = <fs_temp1>-matnr werks = <fs_temp1>-werk ].
              IF <fs_temp1>-charg IS NOT INITIAL .
                APPEND VALUE #( tsyst = <fs_temp1>-tsyst
                           ebeln = <fs_temp1>-ebeln
                           date  = <fs_temp1>-date
                           locid = <fs_temp1>-locid
                           matnr = <fs_temp1>-matnr
                           charg = <fs_temp1>-charg

                             message = 'Batch  present in Excel for Non-Batch Managed Material' ) TO lt_log.
                LOOP AT i_main INTO DATA(wk) WHERE vbeln = <fs_temp1>-ebeln.
                  wk-flag = 'X'.
                  MODIFY i_main FROM wk TRANSPORTING flag.
                  CLEAR:wk.
                ENDLOOP.
              ENDIF.


            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
        ENDLOOP.
      ENDIF.


      DELETE lt_temp[] WHERE xchpf IS INITIAL.
      IF NOT lt_temp[] IS INITIAL .
        SELECT * FROM mcha INTO TABLE @DATA(lt_mcha) FOR ALL ENTRIES IN @lt_temp
          WHERE matnr = @lt_temp-matnr AND werks = @lt_temp-werk AND charg = @lt_temp-charg.
        LOOP AT lt_temp INTO DATA(ls_temp).
          TRY .
              DATA(ls_mcha) = lt_mcha[ matnr = ls_temp-matnr werks = ls_temp-werk charg = ls_temp-charg ].
            CATCH cx_sy_itab_line_not_found.
              APPEND VALUE #( tsyst = ls_temp-tsyst
                         ebeln = ls_temp-ebeln
                         date  = ls_temp-date
                         locid = ls_temp-locid
                         matnr = ls_temp-matnr
                         charg = ls_temp-charg
                        message = 'Batch and Material Combination in excel doesnt exist for the location' ) TO lt_log.

          ENDTRY.
        ENDLOOP.
        SORT lt_log BY matnr charg locid tsyst .
        DELETE ADJACENT DUPLICATES FROM lt_log COMPARING matnr charg locid tsyst.
      ENDIF.
    ENDIF.
  ENDIF.



  LOOP AT i_main INTO DATA(wn).
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wn-vbeln
      IMPORTING
        output = wn-vbeln.

** -> Begin of changes by of Aditi on 12.12.2024 2:57:03 PM for ATC
    CALL FUNCTION 'CONVERSION_EXIT_MATN5_INPUT'
      EXPORTING
        input        = wn-matnr
      IMPORTING
        output       = wn-matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
** <- End changes by of Aditi on 12.12.2024 2:57:08 PM for ATC
    IF sy-subrc <> 0.
    ENDIF.



    SELECT SINGLE bukrs,bsart FROM ekko INTO (@DATA(bukrs) , @DATA(bsart)) WHERE ebeln = @wn-vbeln.
    IF bukrs = '7300' AND bsart = 'ZNGT'.
** -> Begin of changes by of Aditi on 03.12.2024 17:21:22 for ATC
      SELECT werks FROM ekpo INTO  @DATA(werks) UP TO 1 ROWS WHERE ebeln = @wn-vbeln ORDER BY PRIMARY KEY.
      ENDSELECT.
** <- End changes by of Aditi on 03.12.2024 17:21:31 for ATC
      SELECT SINGLE * FROM yrga_cgd_plant  INTO  @DATA(wyr) WHERE werks = @werks.
      IF wyr IS INITIAL.
        SELECT SINGLE ymvgr1 FROM yrga_po_ind INTO @DATA(mvg) WHERE yebeln = @wn-vbeln.
        IF mvg IS INITIAL.
          CONCATENATE 'No Indicator maintained against the PO' '' INTO DATA(msg) SEPARATED BY space.
          APPEND VALUE #(     tsyst   = wn-tsyst
                              ebeln   = wn-vbeln
                              date    = wn-date
                              locid   = wn-locid
                              matnr   = wn-matnr
                              charg   = wn-charg
                              message = msg ) TO lt_log.



        ENDIF.
      ENDIF.
    ENDIF.



    """"BOC 4000005410 TECHNICAL ANKUR PRASHAR FUNCTIONAL SHREYOSI DE ON 23.09.2022""


** -> Begin of changes by of Aditi on 03.12.2024 17:24:16 for ATC
    SELECT matnr FROM ekpo INTO @DATA(matnr1) UP TO 1 ROWS WHERE ebeln =  @wn-vbeln ORDER BY PRIMARY KEY.
    ENDSELECT.
** <- End changes by of Aditi on 03.12.2024 17:24:18 for ATC
    IF matnr1 NE wn-matnr.

      CONCATENATE 'Material in excel does not match with material' matnr1   'in PO'    INTO msg SEPARATED BY space.
      APPEND VALUE #(      tsyst  = wn-tsyst
                           ebeln  = wn-vbeln
                           date   = wn-date
                           locid  = wn-locid
                           matnr  = wn-matnr
                           charg  = wn-charg
                          message = msg ) TO lt_log.

    ENDIF.




    IF wn-flag NE 'X'.
      IF wn-charg IS NOT INITIAL.
** -> Begin of changes by of Aditi on 03.12.2024 17:20:07 for ATC
        SELECT  * FROM eket INTO @DATA(wcharg) UP TO 1 ROWS WHERE ebeln = @wn-vbeln ORDER BY PRIMARY KEY.
        ENDSELECT.
** <- End changes by of Aditi on 03.12.2024 17:20:10 for ATC
        IF wcharg IS NOT INITIAL.
          IF wn-charg NE wcharg-charg.
            CONCATENATE 'Batch in Excel does not match with Batch' wcharg-charg 'in PO'    INTO msg SEPARATED BY space.
            APPEND VALUE #(    tsyst   = wn-tsyst
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

    """"BOC 4000005410 TECHNICAL ANKUR PRASHAR FUNCTIONAL SHREYOSI DE ON 23.09.2022""

    CLEAR:wn,bukrs, bsart, werks, wyr, mvg,msg,wcharg, matnr1.
  ENDLOOP.


ENDFORM.

FORM error_display.




  DATA:lv_col_pos TYPE i.
  lv_col_pos = 0.

  lv_col_pos = lv_col_pos + 1.
  fcat-fieldname    = 'TSYST'.
  fcat-col_pos      =  lv_col_pos.
  fcat-seltext_l    = 'TRANS SYSTEM'.
  fcat-outputlen      =  18.
  fcat-emphasize = 'C410'.
  APPEND fcat.
  CLEAR fcat.


  lv_col_pos = lv_col_pos + 1.
  fcat-fieldname    = 'EBELN'.
  fcat-col_pos      =  lv_col_pos.
  fcat-seltext_l    = 'PO NO'.
  fcat-outputlen      =  18.
  fcat-emphasize = 'C410'.
  APPEND fcat.
  CLEAR fcat.

  lv_col_pos = lv_col_pos + 1.
  fcat-fieldname    = 'DATE'.
  fcat-col_pos      =  lv_col_pos.
  fcat-seltext_l    = 'GAS DAY'.
  fcat-outputlen      =  18.
  fcat-emphasize = 'C410'.
  APPEND fcat.
  CLEAR fcat.

  lv_col_pos = lv_col_pos + 1.
  fcat-fieldname    = 'LOCID'.
  fcat-col_pos      =  lv_col_pos.
  fcat-seltext_l    = 'LOCATION ID'.
  fcat-outputlen      =  10.
  fcat-emphasize = 'C300'.
  APPEND fcat.
  CLEAR fcat.


  lv_col_pos = lv_col_pos + 1.
  fcat-fieldname    = 'MATNR'.
  fcat-col_pos      =  lv_col_pos.
  fcat-seltext_l    = 'MATERIAL'.
  fcat-outputlen      =  18.
  fcat-emphasize = 'C410'.
  APPEND fcat.
  CLEAR fcat.


  lv_col_pos = lv_col_pos + 1.
  fcat-fieldname    = 'CHARG'.
  fcat-col_pos      =  lv_col_pos.
  fcat-seltext_l    = 'BATCH'.
  fcat-outputlen      =  10.
  fcat-emphasize = 'C300'.
  APPEND fcat.
  CLEAR fcat.

  lv_col_pos = lv_col_pos + 1.
  fcat-fieldname    = 'MESSAGE'.
  fcat-col_pos      =  lv_col_pos.
  fcat-seltext_l    = 'MESSAGE'.
  fcat-outputlen      =  50.
  fcat-emphasize = 'C300'.
  APPEND fcat.
  CLEAR fcat.

  SORT lt_log BY date ebeln.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      it_fieldcat        = fcat[]
      i_save             = 'U'
    TABLES
      t_outtab           = lt_log.
ENDFORM.
*EOC BY YASHU/SHREYOSI : CHARM: 4000004403: YS:YRGR040_batch check
*&---------------------------------------------------------------------*
*&      Form  ARRANGE_DATA
*&---------------------------------------------------------------------*
*       Arrange Data
*----------------------------------------------------------------------*
FORM arrange_data .
  DELETE i_tab WHERE col = '1'.

  LOOP AT i_tab INTO wa_tab.
    wa_tab-col = wa_tab-col - 1.
** -> Begin of changes by of Aditi on 03.12.2024 15:49:39 for ATC
    ASSIGN COMPONENT wa_tab-col OF STRUCTURE wa_main TO <f1>. "#EC CI_FLDEXT_OK[2215424]
** <- End changes by of Aditi on 03.12.2024 15:49:43 for ATC
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
*SOC BY YASHU/SHREYOSI CHARM 4000004403: YS:YRGR040_batch check
      TRANSLATE wa_main-charg TO UPPER CASE.
*EOC BY YASHU/SHREYOSI CHARM 4000004403: YS:YRGR040_batch check
      APPEND wa_main TO i_main.
      CLEAR wa_main.
    ENDAT.
  ENDLOOP.


ENDFORM.                    " ARRANGE_DATA
*&---------------------------------------------------------------------*
*&      Form  CREATEFROMDATA
*&---------------------------------------------------------------------*
*       Set date for creating nomination
*----------------------------------------------------------------------*
FORM createfromdata .
  DATA: wl_ebelp TYPE ekpo-ebelp.
  SORT i_main BY vbeln date.

  SORT i_main BY tsyst date locid .
  REFRESH i_main_tyst.
  MOVE i_main[] TO i_main_tyst[].
  DELETE ADJACENT DUPLICATES FROM i_main_tyst COMPARING tsyst date locid .


  LOOP AT i_main_tyst INTO wa_main_tyst .

    LOOP AT  i_main INTO wa_main WHERE tsyst = wa_main_tyst-tsyst AND locid = wa_main_tyst-locid AND date = wa_main_tyst-date.

      READ TABLE it_oijts INTO wa_oijts WITH KEY tsyst = wa_main-tsyst.
      IF sy-subrc = 0.

        IF wa_oijts-werk(1) = '3'.
          wa_nom_header-nominationtype       = 'GITA'.
          wa_nom_item-itemtype               = 'ZO'.
        ELSEIF wa_oijts-werk(1) = '2' OR wa_oijts-werk(1) = '5' OR wa_oijts-werk(1) = '7' OR wa_oijts-werk(1) = '6'.
          wa_nom_header-nominationtype       = 'GISA'.
          wa_nom_item-itemtype               = 'OU'.

        ENDIF.
      ENDIF.
***Fill Header for 'GISA'
      wa_nom_header-transportsystem      = wa_main-tsyst.
      wa_nom_header-nominationstatus     = '1'.
*    CHECK ALREADY CREATED NOMINATION
      READ TABLE it_oijnomi_exist INTO wa_oijnomi WITH KEY docnr = wa_main-vbeln
          idate = wa_main-date.
      IF sy-subrc EQ 0 .
        CONCATENATE 'Nomination already created for ' wa_main-vbeln
        INTO wa_ret-message SEPARATED BY space.
        APPEND wa_ret TO i_error. CLEAR wa_ret.
        CONTINUE.
      ENDIF.
***Fill 1st line item for 'GISA'
** -> Begin of changes by of Aditi on 03.12.2024 17:22:30 for ATC
      SELECT ebelp INTO wl_ebelp FROM ekpo UP TO 1 ROWS WHERE ebeln = wa_main-vbeln AND loekz IN ( ' ','S' ) ORDER BY PRIMARY KEY.
      ENDSELECT.
** <- End changes by of Aditi on 03.12.2024 17:22:35 for ATC
      IF sy-subrc = 0.
        MOVE wl_ebelp TO wa_nom_item-documentitem.
      ELSE.
        CONCATENATE 'No active record found for PO' wa_main-vbeln
        INTO wa_ret-message SEPARATED BY space.
        APPEND wa_ret TO i_error. CLEAR wa_ret.
        CONTINUE.
      ENDIF.
      wa_nom_item-itemnumber             = l_count.
      wa_nom_item-itemstatus             = '1'.
      wa_nom_item-scheduleddate          = wa_main-date.
      wa_nom_item-locationid             = wa_main-locid.
      wa_nom_item-demandmaterial         = wa_main-matnr.
      wa_nom_item-schedulematerial       = wa_main-matnr.
      wa_nom_item-nominatedquantity      = wa_main-menge.
      wa_nom_item-quantityunit_sap       = wa_main-unit.
      wa_nom_item-documentnumber         = wa_main-vbeln.
      IF wa_main-vbeln+0(1) = '5'.
        wa_nom_item-documentindicator      = 'T'.
        wa_nom_item-batchorigin = wa_main-charg.
      ELSE.
        wa_nom_item-documentindicator      = 'K'.
        wa_nom_item-batchdestination = wa_main-charg.
      ENDIF.

      APPEND wa_nom_item TO i_nom_item.

      wa_main-nomit = l_count.
      APPEND wa_main TO i_main_check.

      l_count = l_count + 10.

    ENDLOOP.
    PERFORM call_rfc_tsw_nom_create     TABLES i_nom_item
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
    CLEAR  : i_nom_item, i_nom_item_o, i_return, wa_nom_header,
                                                 wa_nom_header_o.
    l_count = '0000000010'.


  ENDLOOP.



ENDFORM.                    " CREATEFROMDATA
*&---------------------------------------------------------------------*
*&      Form  CALL_RFC_TSW_NOM_CREATE
*&---------------------------------------------------------------------*
** -> Begin of changes by of Aditi on 11.12.2024 16:32:39 for ATC
FORM call_rfc_tsw_nom_create  TABLES   p_it_nom_item     STRUCTURE bapitswnom03 "#EC CI_USAGE_OK[2215424]
                                       p_it_nom_item_o   STRUCTURE bapitswnom03_o "#EC CI_USAGE_OK[2215424]
** <- End changes by of Aditi on 11.12.2024 16:32:42 for ATC
                                       p_it_return       STRUCTURE bapiret2
                              USING    p_wa_nom_header   TYPE      bapitswnom02
                              CHANGING p_wa_nom_header_o TYPE      bapitswnom02_o.


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
ENDFORM.                    " CALL_RFC_TSW_NOM_CREATE
*&---------------------------------------------------------------------*
*&      Form  CREATE_TICKET
*&---------------------------------------------------------------------*
FORM create_ticket .

  DATA ls_oijnomi TYPE oijnomi.
  DATA lt_oijnomi TYPE TABLE OF oijnomi.
  DATA l_enq_flag TYPE flag.

  DATA: w_ticket_number(50),
        w_qty_char(15),
        w_gcv_char(15),
        w_ncv_char(15),
        w_stdate(10),
        w_endate(10).
  DATA: opt          TYPE ctu_params,
        c_numobj     TYPE  nrobj VALUE 'YTICKET',
        w_number(10) TYPE c,
        retco        TYPE  nrreturn,
        l_tab        TYPE i.

  DATA : it_enq TYPE TABLE OF seqg3.
  DATA l_enq TYPE eqegraarg.

  DO 2 TIMES .

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        guname = '*'
      TABLES
        enq    = it_enq.
    IF sy-subrc <> 0.
    ENDIF.

    CLEAR l_enq.
    CLEAR l_enq_flag.
    CONCATENATE sy-mandt wa_main-nomtk INTO l_enq.
    LOOP AT it_enq INTO DATA(wa_enq) WHERE gname(3) = 'OIJ' AND garg(23) = l_enq.
      l_enq_flag = 'X'.
      EXIT .
    ENDLOOP.




    IF l_enq_flag IS INITIAL.
      EXIT.
    ELSE.
      WAIT UP TO 5 SECONDS.
    ENDIF.
  ENDDO.

  IF l_enq_flag = 'X'.
    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        guname = '*'
      TABLES
        enq    = it_enq.
    IF sy-subrc <> 0.
    ENDIF.

    CLEAR l_enq.
    CLEAR l_enq_flag.
    CONCATENATE sy-mandt wa_main-nomtk INTO l_enq.
    LOOP AT it_enq INTO wa_enq WHERE gname(3) = 'OIJ' AND garg(23) = l_enq.
      l_enq_flag = 'X'.
      EXIT .
    ENDLOOP.


    IF l_enq_flag = 'X'.
      DATA l_error TYPE char100.
      CONCATENATE 'Nomination' wa_main-nomtk 'cannot be locked and actuals actualqty ' 'might not be updated'
      INTO l_error.
      MESSAGE l_error TYPE 'I'.
    ENDIF.
  ENDIF.

  CLEAR ls_oijnomi.
  SELECT SINGLE * INTO ls_oijnomi FROM oijnomi
    WHERE
    nomtk = wa_main-nomtk
    AND
    nomit = wa_main-nomit
    AND delind = ' '.

  SELECT * INTO TABLE lt_oijnomi FROM oijnomi
    WHERE
    nomtk = wa_main-nomtk
    AND delind = ' '.


  DESCRIBE TABLE lt_oijnomi LINES l_tab.

  TYPES : BEGIN OF ty_flag,
            item TYPE oij_item,
          END OF ty_flag.
  DATA : it_flag TYPE TABLE OF ty_flag,
         wa_flag TYPE ty_flag.
  IF l_tab > 1.
    wa_flag-item = wa_main-nomit.
    APPEND wa_flag TO it_flag.
    EXPORT it_flag TO MEMORY ID 'V1'.
  ENDIF.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = c_numobj
    IMPORTING
      number                  = w_number
      returncode              = retco
    EXCEPTIONS
      interval_not_found      = 01
      number_range_not_intern = 02
      object_not_found        = 03
      interval_overflow       = 04.

  CONCATENATE 'C' sy-datum w_number INTO w_ticket_number.

  PERFORM bdc_dynpro USING 'SAPMOIJTN'             '0101'.
  PERFORM bdc_field  USING 'BDC_CURSOR'              'ROIJTIC_IO-TICKETNR'.
  PERFORM bdc_field  USING 'BDC_OKCODE'              '=ENT1'.
  PERFORM bdc_field  USING 'ROIJTIC_IO-TICKETNR'     w_ticket_number .
  PERFORM bdc_field  USING 'ROIJTIC_IO-TICKET_TYPE'  'C'.
  PERFORM bdc_field  USING 'ROIJTIC_IO-NOMNR'        wa_main-nomtk.
  PERFORM bdc_field  USING 'ROIJTIC_IO-NOMTK'        wa_main-nomtk.

  w_qty_char = ls_oijnomi-ga_allocated_qty.
  w_ncv_char = wa_main-ancv.
  w_gcv_char = wa_main-agcv.
  CONDENSE: w_qty_char,w_gcv_char,w_ncv_char.

  WRITE wa_main-date TO w_stdate.
  WRITE wa_main-date TO w_endate.

  IF l_tab > 1.
    PERFORM bdc_dynpro      USING 'SAPLOIJT_IO' '0500'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'OIJNOMH-NOMNR'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=TC_OIJNOMI_MARK'.
    PERFORM bdc_dynpro      USING 'SAPLOIJT_IO' '0500'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'OIJNOMH-NOMNR'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=COPY'.

  ENDIF.


  DATA w_date TYPE char10.
  CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4) INTO w_date.


  PERFORM bdc_dynpro     USING  'SAPMOIJTN'  '0301' .
  PERFORM bdc_field USING  'BDC_OKCODE' '/EENTE' .
  PERFORM bdc_field USING  'BDC_SUBSCR' 'SAPMOIJTN'.
  PERFORM bdc_field USING  'BDC_SUBSCR' 'SAPMOIJTN'.
  PERFORM bdc_field USING  'BDC_CURSOR' 'ROIJTIC_IO-NET_QUANTITY'.
  PERFORM bdc_field USING  'ROIJTIC_IO-NET_QUANTITY' w_qty_char.
  PERFORM bdc_field USING  'ROIJTIC_IO-UOM'         'SM3'.
  PERFORM bdc_field USING  'ROIJTIC_IO-END_DATE'    w_endate.
  PERFORM bdc_field USING  'ROIJTIC_IO-END_TIME'    '00:00:00'.
  PERFORM bdc_field USING  'ROIJTIC_IO-START_DATE'  w_stdate.
  PERFORM bdc_field USING  'ROIJTIC_IO-START_TIME'  '00:00:00'.
  PERFORM bdc_field USING  'ROIJTIC_IO-BUDAT'      w_stdate .
  PERFORM bdc_field USING  'ROIJTIC_IO-OIB_BLTIME'  '00:00:00'.
  PERFORM bdc_field USING  'ROIJTIC_IO-UOM'        wa_main-unit.

  PERFORM bdc_dynpro     USING  'SAPMOIJTN'  '0301' .
  PERFORM bdc_field USING  'BDC_CURSOR' 'ROIJTIC_IO-TIC_ITEM(01)'.
  PERFORM bdc_field USING  'BDC_OKCODE' '=IOTAB4' .
  PERFORM bdc_field USING  'BDC_SUBSCR' 'SAPMOIJTN'.
  PERFORM bdc_field USING  'BDC_SUBSCR' 'SAPMOIJTN'.
  PERFORM bdc_field USING  'ROIJTIC_IO-NET_QUANTITY' w_qty_char.
  PERFORM bdc_field USING  'ROIJTIC_IO-UOM'         'SM3'.
  PERFORM bdc_field USING  'ROIJTIC_IO-START_DATE'  w_stdate.
  PERFORM bdc_field USING  'ROIJTIC_IO-START_TIME'  '00:00:00'.
  PERFORM bdc_field USING  'ROIJTIC_IO-BUDAT'      w_stdate .
  PERFORM bdc_field USING  'ROIJTIC_IO-OIB_BLTIME'  '00:00:00'.
  PERFORM bdc_field USING  'ROIJTIC_IO-END_DATE'    w_endate.
  PERFORM bdc_field USING  'ROIJTIC_IO-END_TIME'    '00:00:00'.
  PERFORM bdc_field USING  'ROIJTIC_IO-UOM'        wa_main-unit.

  PERFORM bdc_dynpro USING  'SAPMOIJTN'  '0301' .
  PERFORM bdc_field  USING 'BDC_OKCODE' '/EENTE'.
  PERFORM bdc_field USING  'BDC_SUBSCR' 'SAPMOIJTN'.
  PERFORM bdc_field USING  'BDC_SUBSCR' 'SAPMOIJTN'.
  PERFORM bdc_field USING  'BDC_SUBSCR' 'SAPLOIB_QCI_SUB_SCREEN'.
  PERFORM bdc_field USING  'BDC_CURSOR'   'GT_PARAM_SCREEN-VALUE(02)'.
  PERFORM bdc_field USING  'GT_PARAM_SCREEN-VALUE(01)'     w_ncv_char.
  PERFORM bdc_field USING  'GT_PARAM_SCREEN-VALUE(02)'     w_gcv_char.

  PERFORM bdc_dynpro     USING  'SAPMOIJTN'  '0301' .
  PERFORM bdc_field USING  'BDC_OKCODE' '=ACTL' .
  PERFORM bdc_field USING  'BDC_SUBSCR' 'SAPMOIJTN'.
  PERFORM bdc_field USING  'BDC_SUBSCR' 'SAPMOIJTN'.
  PERFORM bdc_field USING  'BDC_SUBSCR' 'SAPLOIB_QCI_SUB_SCREEN'.

  opt-dismode = 'E'.
  opt-defsize = 'X'.
  opt-racommit = 'X'.

  REFRESH : msgtab.
  CALL  TRANSACTION 'O4TEN'
              USING bdcdata
            OPTIONS FROM opt
           MESSAGES INTO msgtab.
  COMMIT WORK.
*******   BOC charm 4000001118 technial : vikram bajaj : functional pratibha dangwal and aroghya mondal Dt 16.12.2019
  WAIT UP TO 2 SECONDS.
*******   EOC charm 4000001118 technial : vikram bajaj : functional pratibha dangwal and aroghya mondal Dt 16.12.2019

  LOOP AT msgtab.
    wa_message-id = msgtab-msgid.
    wa_message-number = msgtab-msgnr.
    wa_message-message = msgtab-msgv1.
    APPEND wa_message TO i_message.
  ENDLOOP.
***  BOC CHARM 4000001873 TECH VIKRAM BAJAJ FUNC PRATIBHA DANGWAL DT 26.03.2020
  DATA : ls_oij_el_ticket_i TYPE oij_el_ticket_i .
  DATA : lt_oij_el_ticket_i TYPE TABLE OF oij_el_ticket_i,
         lt_oij_el_ticket_h TYPE TABLE OF oij_el_ticket_h.
  DATA : lv_ind(1) .

  SELECT * FROM oij_el_ticket_h INTO TABLE lt_oij_el_ticket_h
  WHERE ticketnr = w_ticket_number AND ticket_purpose = '1'.

  IF sy-subrc = 0.
** -> Begin of changes by of Aditi on 03.12.2024 17:17:51 for ATC
    READ TABLE lt_oij_el_ticket_h INTO DATA(wa_oij_el_ticket_h) INDEX 1. "#EC CI_NOORDER
** <- End changes by of Aditi on 03.12.2024 17:17:53 for ATC
    SELECT * FROM oij_el_ticket_i INTO TABLE lt_oij_el_ticket_i
    WHERE ticket_key = wa_oij_el_ticket_h-ticket_key AND status = 'C' AND substatus = '6' .

    IF sy-subrc = 0.
      SELECT * FROM oijnomi INTO TABLE lt_oijnomi FOR ALL ENTRIES IN lt_oij_el_ticket_i WHERE nomtk = lt_oij_el_ticket_i-nomtk AND nomit = lt_oij_el_ticket_i-nomit
        AND delind = ' '.
      IF sy-subrc = 0 .
        LOOP AT lt_oijnomi INTO ls_oijnomi .
          IF ls_oijnomi-actuals IS INITIAL .
            ls_oijnomi-actuals = 'X' .
            lv_ind = 'X' .
          ENDIF .
          IF ls_oijnomi-actualqty EQ 0.
            ls_oijnomi-actualqty = ls_oijnomi-ga_allocated_qty.
            lv_ind = 'X'.
          ENDIF.
          IF lv_ind IS NOT INITIAL .
            MODIFY lt_oijnomi FROM ls_oijnomi TRANSPORTING actuals actualqty .
          ENDIF .
          CLEAR: ls_oijnomi ,  ls_oij_el_ticket_i .
        ENDLOOP .

        IF lv_ind IS NOT INITIAL .
          MODIFY oijnomi FROM TABLE lt_oijnomi .
        ENDIF .
      ENDIF .
    ENDIF.
  ENDIF.

***Checking the ticket status and display the error if ticket is not posted completely

  DATA: lt_oij_el_ticket_h_temp TYPE TABLE OF oij_el_ticket_h,
        lt_oij_el_ticket_i_temp TYPE TABLE OF oij_el_ticket_i.


  SELECT * FROM oij_el_ticket_h INTO TABLE lt_oij_el_ticket_h_temp
  WHERE ticketnr = w_ticket_number AND ticket_purpose = '1'.

  IF sy-subrc = 0.
** -> Begin of changes by of Aditi on 03.12.2024 17:18:15 for ATC
    READ TABLE lt_oij_el_ticket_h_temp INTO DATA(wa_oij_el_ticket_h_temp) INDEX 1. "#EC CI_NOORDER
** <- End changes by of Aditi on 03.12.2024 17:18:18 for ATC
    SELECT * FROM oij_el_ticket_i INTO TABLE lt_oij_el_ticket_i_temp
    WHERE ticket_key = wa_oij_el_ticket_h_temp-ticket_key AND ticket_purpose = '1' AND nomtk = wa_main-nomtk AND nomit = wa_main-nomit.

    IF sy-subrc = 0.
** -> Begin of changes by of Aditi on 03.12.2024 17:18:33 for ATC
      READ TABLE lt_oij_el_ticket_i_temp INTO DATA(wa_oij_el_ticket_i_temp) INDEX 1. "#EC CI_NOORDER
** <- End changes by of Aditi on 03.12.2024 17:18:35 for ATC
      IF wa_oij_el_ticket_i_temp-status = 'C' AND wa_oij_el_ticket_i_temp-substatus = '6'.

        wa_main-post_status = 'Completely Posted'.
        wa_main-ticketnr = wa_oij_el_ticket_h_temp-ticketnr.
        wa_main-ticket_key = wa_oij_el_ticket_h_temp-ticket_key.
        wa_main-ticket_item =  wa_oij_el_ticket_i_temp-ticket_item.

      ELSE.
** -> Begin of changes by of Aditi on 03.12.2024 17:19:13 for ATC
        SELECT  * FROM oij_el_error_log INTO @DATA(oij_el) UP TO 1 ROWS WHERE ticket_key   = @wa_oij_el_ticket_i_temp-ticket_key
                                                            AND   ticket_item  = @wa_oij_el_ticket_i_temp-ticket_item
                                                            AND   msgid        NE '' ORDER BY PRIMARY KEY.
        ENDSELECT.
** <- End changes by of Aditi on 03.12.2024 17:19:15 for ATC
        IF oij_el IS NOT INITIAL.
          MESSAGE ID oij_el-msgid TYPE oij_el-msgty NUMBER oij_el-msgno
          WITH oij_el-msgv1 oij_el-msgv2 oij_el-msgv3 oij_el-msgv4 INTO wa_main-error_msg.
        ENDIF.
        wa_main-color = 'C601'.
        wa_main-post_status = 'Partially Posted'.
        wa_main-ticketnr = wa_oij_el_ticket_h_temp-ticketnr.
        wa_main-ticket_key = wa_oij_el_ticket_h_temp-ticket_key.
        wa_main-ticket_item =  wa_oij_el_ticket_i_temp-ticket_item.
      ENDIF.
    ELSE.
      wa_main-post_status = ''.
      wa_main-ticketnr = ''.
      wa_main-ticket_key = ''.
      wa_main-ticket_item =  ''.
    ENDIF.
  ELSE.
    wa_main-post_status = ''.
    wa_main-ticketnr = ''.
    wa_main-ticket_key = ''.
    wa_main-ticket_item =  ''.
  ENDIF.

***  EOC CHARM 4000001873 TECH VIKRAM BAJAJ FUNC PRATIBHA DANGWAL DT 26.03.2020
  REFRESH: bdcdata.
  CLEAR:   w_qty_char,w_gcv_char,w_ncv_char,
           w_stdate,w_endate,wa_message.
ENDFORM.                    " CREATE_TICKET
*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  QUICK_CONFIRM
*&---------------------------------------------------------------------*
FORM quick_confirm .
  DATA: lt_oijnomh_key TYPE TABLE OF roijnomh_key,
        lt_oijnomh     TYPE TABLE OF oijnomh,
        lt_oijnomi     TYPE TABLE OF oijnomi,
        lt_nom_item    TYPE TABLE OF roijnomiio,
        lt_nom_header  TYPE roijnomhio_t,
        lt_return      TYPE bapiret2_t,
        ls_nom_header  TYPE LINE OF roijnomhio_t,
        ls_oijnomh_key TYPE roijnomh_key,
        ls_oijnomh     TYPE oijnomh,
        ls_oijnomi     TYPE oijnomi,
        ls_nom_item    LIKE roijnomiio.

  ls_oijnomh_key-nomtk = wa_main-nomtk.
  APPEND ls_oijnomh_key TO lt_oijnomh_key.

  CALL FUNCTION 'OIJNOMH_ARRAY_READ'
    EXPORTING
      it_oijnomh_key        = lt_oijnomh_key
    IMPORTING
      et_oijnomh            = lt_oijnomh
    EXCEPTIONS
      records_not_found     = 1
      records_not_requested = 2
      OTHERS                = 3.

  CALL FUNCTION 'OIJNOMI_ARRAY_READ'
    EXPORTING
      it_oijnomh_key        = lt_oijnomh_key
    IMPORTING
      et_oijnomi            = lt_oijnomi
    EXCEPTIONS
      records_not_found     = 1
      records_not_requested = 2
      OTHERS                = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  LOOP AT lt_oijnomh INTO ls_oijnomh.
    MOVE-CORRESPONDING ls_oijnomh TO ls_nom_header.
    APPEND ls_nom_header TO lt_nom_header.
  ENDLOOP.
  LOOP AT lt_oijnomi INTO ls_oijnomi WHERE nomtk = wa_main-nomtk.
    MOVE-CORRESPONDING ls_oijnomi TO ls_nom_item.
    IF ls_nom_item-quickconf <> 'X'.
      ls_nom_item-quickconf = 'X'.
      ls_nom_item-updkz     = 'U'.
      APPEND ls_nom_item TO lt_nom_item.
    ENDIF.
  ENDLOOP.
  IF lt_nom_item IS NOT INITIAL.
    CALL FUNCTION 'OIJ_NOM_MAINTAIN_MULTIPLE'
      EXPORTING
        it_nom_header     = lt_nom_header
        it_nom_item       = lt_nom_item
        iv_internal       = 'X'
        iv_lock           = ' '
      IMPORTING
        et_nom_header     = lt_nom_header
        et_nom_item       = lt_nom_item
        et_return         = lt_return
      EXCEPTIONS
        nomination_locked = 1
        error_phase_01    = 2
        error_phase_02    = 3
        OTHERS            = 4.

    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.                    " QUICK_CONFIRM
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
ENDFORM.                    " SHOW_LOG
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
      WHERE
      tsyst = i_main-tsyst.

  ENDIF.

ENDFORM.                    " GET_NOMINATION
*&---------------------------------------------------------------------*
*&      Form  NOMINATION_UPDATE
*&---------------------------------------------------------------------*
FORM nomination_update .

  DATA:lt_oijnomh     TYPE TABLE OF oijnomh,
       lt_oijnomi     TYPE TABLE OF oijnomi,
       ls_nom_header  TYPE LINE OF roijnomhio_t,
       ls_oijnomi_key TYPE roijnomi_key,
       ls_oijnomh     TYPE oijnomh,
       ls_oijnomi     TYPE oijnomi,
       lt_ret         TYPE bapiret2_t,
       lt_nom_header  TYPE roijnomhio_t,
       ls_nom_item    LIKE  roijnomiio,
       ls_nom_item1   LIKE  roijnomiio,
       lt_nom_item    LIKE TABLE OF ls_nom_item,
       lt_nom_item1   LIKE TABLE OF ls_nom_item,
       ls_nom_old     LIKE  roijnomiio,
       lt_nom_old     LIKE TABLE OF ls_nom_item,
       lt_nom_key     TYPE TABLE OF roijnomh_key,
       wa_ret         TYPE bapiret2,
       l_bstnk        TYPE bstnk,
       wa_nomh        TYPE roijnomh_key.


  DATA : i_rspartab  TYPE TABLE OF rsparams,
         wa_rspartab LIKE LINE OF i_rspartab,
         w_agcv      TYPE char24,
         w_ancv      TYPE char24.

  DATA l_tabix TYPE sy-tabix.

  IF i_main_check[] IS INITIAL.
    MOVE i_main[] TO i_main_check[].
  ENDIF.

  LOOP AT i_main_check INTO wa_main.
    REFRESH : lt_oijnomh,lt_oijnomi,lt_nom_item,lt_nom_old.
    CLEAR: ls_nom_header,ls_oijnomi_key,ls_oijnomh,ls_oijnomi,lt_ret,lt_nom_header,ls_nom_item,ls_nom_item1,ls_nom_old,wa_ret,l_bstnk.
    IF r_pro <> 'X'.
      REFRESH lt_nom_key.
      wa_nomh-nomtk = wa_main-nomtk.
      APPEND wa_nomh TO lt_nom_key.

      CALL FUNCTION 'OIJNOMH_ARRAY_READ'
        EXPORTING
          it_oijnomh_key        = lt_nom_key
        IMPORTING
          et_oijnomh            = lt_oijnomh
        EXCEPTIONS
          records_not_found     = 1
          records_not_requested = 2
          OTHERS                = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CALL FUNCTION 'OIJNOMI_ARRAY_READ'
        EXPORTING
          it_oijnomh_key        = lt_nom_key
        IMPORTING
          et_oijnomi            = lt_oijnomi
        EXCEPTIONS
          records_not_found     = 1
          records_not_requested = 2
          OTHERS                = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      LOOP AT lt_oijnomh INTO ls_oijnomh.
        MOVE-CORRESPONDING ls_oijnomh TO ls_nom_header.
        APPEND ls_nom_header TO lt_nom_header.
      ENDLOOP.

      LOOP AT lt_oijnomi INTO ls_oijnomi WHERE nomit = wa_main-nomit..
        MOVE-CORRESPONDING ls_oijnomi TO ls_nom_item.
        APPEND ls_nom_item TO lt_nom_item.
      ENDLOOP.

      CALL FUNCTION 'OIJ_NOM_DEFAULTING'
        EXPORTING
          it_nom_header     = lt_nom_header
          it_nom_item       = lt_nom_item
        IMPORTING
          et_nom_header     = lt_nom_header
          et_nom_item       = lt_nom_item
        EXCEPTIONS
          defaulting_error  = 1
          no_records_passed = 2
          OTHERS            = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      REFRESH   lt_nom_item1.
      MOVE lt_nom_item[] TO lt_nom_item1[].

      CLEAR l_tabix.
      LOOP AT lt_nom_item INTO ls_nom_item.
        l_tabix = sy-tabix.
*&------>BUG FIX: Use original Excel qty (wa_main-menge) not the SAP-returned menge.
*&       RFC_TSW_NOM_CREATEFROMDATA defaults to OA contractual qty when nominatedquantity=0,
*&       so ls_nom_item-menge read back from SAP would be wrong for zero-qty rows.
*&       wa_main-menge is preserved from i_main_check and always holds the Excel value.
        ls_nom_item-yyoij_cnom_qty = wa_main-menge.
*&------>END BUG FIX
        ls_nom_item-yyoij_cnom_uom = ls_nom_item-unit_i.
        ls_nom_item-ga_rank = wa_main-rank.
        ls_nom_item-menge = '1.000'.
        ls_nom_item-unit_i = 'SM3'.
        ls_nom_item-updkz = 'U'.
        MODIFY lt_nom_item FROM ls_nom_item INDEX l_tabix.
      ENDLOOP.

      CALL FUNCTION 'OIJ_NOM_MAINTAIN_MULTIPLE'
        EXPORTING
          it_nom_header     = lt_nom_header
          it_nom_item       = lt_nom_item
          iv_internal       = 'X'
          iv_lock           = ' '
        IMPORTING
          et_nom_header     = lt_nom_header
          et_nom_item       = lt_nom_item
          et_return         = lt_ret
        EXCEPTIONS
          nomination_locked = 1
          error_phase_01    = 2
          error_phase_02    = 3
          OTHERS            = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.
    IF r_pro = 'X'.
      CLEAR : w_agcv , w_ancv.
      REFRESH i_rspartab.
      w_agcv = wa_main-agcv.
      w_ancv = wa_main-ancv.


      wa_rspartab-selname = 'S_NOMTK'.
      wa_rspartab-kind    = 'S'.
      wa_rspartab-sign    = 'I'.
      wa_rspartab-option  = 'EQ'.
      wa_rspartab-low     = wa_main-nomtk.
      APPEND wa_rspartab TO i_rspartab.
      CLEAR wa_rspartab.

      wa_rspartab-selname = 'S_LOCID'.
      wa_rspartab-kind    = 'S'.
      wa_rspartab-sign    = 'I'.
      wa_rspartab-option  = 'EQ'.
      wa_rspartab-low     = wa_main-locid.
      APPEND wa_rspartab TO i_rspartab.
      CLEAR wa_rspartab.

      wa_rspartab-selname = 'S_IDATE'.
      wa_rspartab-kind    = 'S'.
      wa_rspartab-sign    = 'I'.
      wa_rspartab-option  = 'EQ'.
      wa_rspartab-low     =  wa_main-date.
      APPEND wa_rspartab TO i_rspartab.
      CLEAR wa_rspartab.

      wa_rspartab-selname = 'P_ANCV'.
      wa_rspartab-kind    = 'P'.
      wa_rspartab-low     =  w_ancv.
      APPEND wa_rspartab TO i_rspartab.
      CLEAR wa_rspartab.

      wa_rspartab-selname = 'P_AGCV'.
      wa_rspartab-kind    = 'P'.
      wa_rspartab-low     =  w_agcv.
      APPEND wa_rspartab TO i_rspartab.
      CLEAR wa_rspartab.

      SUBMIT yrxr008_nomination_corr_copy USING SELECTION-SCREEN '1000'
             WITH SELECTION-TABLE i_rspartab AND RETURN.
      IMPORT i_return[] FROM MEMORY ID 'NOMI_CORRECTION_YRXU003'.


      CLEAR wa_yro_nom_param1.
      SELECT SINGLE * FROM yro_nom_param1 INTO wa_yro_nom_param1
        WHERE nomtk = wa_main-nomtk
        AND nomit = wa_main-nomit.

      IF sy-subrc = 0.

        wa_yro_nom_param1-nomit = wa_main-nomit.
        wa_yro_nom_param1-nomtk = wa_main-nomtk.
        wa_yro_nom_param1-ancv = wa_main-ancv.
        wa_yro_nom_param1-agcv = wa_main-agcv.
        wa_yro_nom_param1-createdby = sy-uname.
        wa_yro_nom_param1-createdon = sy-datum.
        wa_yro_nom_param1-createtime = sy-uzeit.
        UPDATE yro_nom_param1 FROM wa_yro_nom_param1.

      ELSE.
        wa_yro_nom_param1-nomit = wa_main-nomit.
        wa_yro_nom_param1-nomtk = wa_main-nomtk.
        wa_yro_nom_param1-ancv = wa_main-ancv.
        wa_yro_nom_param1-agcv = wa_main-agcv.
        wa_yro_nom_param1-createdby = sy-uname.
        wa_yro_nom_param1-createdon = sy-datum.
        wa_yro_nom_param1-createtime = sy-uzeit.
        INSERT yro_nom_param1 FROM wa_yro_nom_param1.
      ENDIF.
      COMMIT WORK.
    ENDIF.
  ENDLOOP.

  IF r_pro  = 'X'.

    DATA: it_oijnomi  TYPE TABLE OF oijnomi,
          i_main1     TYPE TABLE OF ty_main,
          i_main2     TYPE TABLE OF ty_main,
          i_main3     TYPE TABLE OF ty_main,
          wa_main2    TYPE ty_main,

          w_total     TYPE oij_allocated_qty,
          w_qty       TYPE oij_allocated_qty,
          w_qty_alloc TYPE oij_allocated_qty,
          w_qty1      TYPE oij_allocated_qty,
          w_qty2      TYPE oij_allocated_qty,
          w_qty3      TYPE oij_allocated_qty,
          w_qty4      TYPE oij_allocated_qty,
          w_val       TYPE i,
          w_rank      TYPE i,
          w_rank1     TYPE i,
          w_qty_tot   TYPE oij_allocated_qty,
          l_tabix1    TYPE sy-tabix.


    SELECT * INTO TABLE it_yrxa_cmdata
      FROM yrxa_cmdata
      FOR ALL ENTRIES IN i_main
      WHERE
      yybus_location = i_main-locid
      AND
      yydate = i_main-date.
    IF sy-subrc = 0.
      SORT it_yrxa_cmdata BY yybus_location yydate yytimestamp DESCENDING.
      DELETE ADJACENT DUPLICATES FROM it_yrxa_cmdata COMPARING yybus_location yydate .
    ENDIF.


    IF p_qty IS NOT INITIAL.
      LOOP AT it_yrxa_cmdata INTO wa_yrxa_cmdata.
        wa_yrxa_cmdata-yycum_vol = p_qty.
        MODIFY it_yrxa_cmdata FROM wa_yrxa_cmdata INDEX sy-tabix.
      ENDLOOP.

    ENDIF.

    SELECT * INTO TABLE it_oijnomi
      FROM oijnomi
      FOR ALL ENTRIES IN i_main
      WHERE
      nomtk = i_main-nomtk
      AND delind = ' '.
    IF sy-subrc = 0.
      MOVE i_main[] TO i_main2[].
      SORT i_main2 BY locid date.
      DELETE ADJACENT DUPLICATES FROM i_main2 COMPARING locid date.
      LOOP AT i_main2 INTO wa_main2.
        REFRESH i_main1.
        LOOP AT i_main INTO wa_main WHERE locid = wa_main2-locid AND date = wa_main2-date.
          APPEND wa_main TO i_main1.
        ENDLOOP.



        CLEAR: w_val,w_qty.
        DESCRIBE TABLE i_main1 LINES w_val.
        SORT i_main1 BY rank ASCENDING.

        READ TABLE i_main1 INTO wa_main1 INDEX w_val.
        IF sy-subrc = 0.
          w_rank1 = wa_main1-rank.
        ENDIF.

        READ TABLE it_yrxa_cmdata INTO wa_yrxa_cmdata WITH KEY yybus_location = wa_main-locid yydate = wa_main-date.
        IF sy-subrc = 0.
          w_qty = wa_yrxa_cmdata-yycum_vol .
        ELSE.
          w_qty = p_qty.
        ENDIF.
        CLEAR w_rank.
        DATA l_alloc_abs TYPE oij_allocated_qty.
        LOOP AT i_main1 INTO wa_main .
          CLEAR: w_qty2..
          l_tabix = sy-tabix.
          READ TABLE it_oijnomi INTO wa_oijnomi WITH KEY nomtk = wa_main-nomtk  nomit = wa_main-nomit.
          IF sy-subrc = 0.
            l_tabix1 = sy-tabix.
            IF w_rank <> wa_main-rank.
              CLEAR w_qty1.
              LOOP AT i_main1 INTO wa_main1 WHERE rank = wa_main-rank.
                READ TABLE it_oijnomi INTO wa_oijnomi_temp WITH KEY nomtk = wa_main1-nomtk  nomit = wa_main1-nomit.
                IF sy-subrc = 0.
                  w_qty1 = w_qty1 + wa_oijnomi_temp-menge.
                ENDIF.
                w_rank = wa_main1-rank.
              ENDLOOP.
              w_qty3 = w_qty.
            ENDIF.
            IF w_qty1 = wa_oijnomi-menge.
              IF l_tabix <> w_val.
                IF wa_oijnomi-menge <= w_qty.
                  wa_oijnomi-ga_allocated_qty = wa_oijnomi-menge.
                ELSE.
                  wa_oijnomi-ga_allocated_qty = w_qty.
                ENDIF.
                w_qty = w_qty - wa_oijnomi-menge.
                IF w_qty < 0 .
                  CLEAR w_qty.
                ENDIF.
                MODIFY it_oijnomi FROM wa_oijnomi INDEX l_tabix1 TRANSPORTING ga_allocated_qty.
              ELSE.
                wa_oijnomi-ga_allocated_qty = w_qty.
                MODIFY it_oijnomi FROM wa_oijnomi INDEX l_tabix1 TRANSPORTING ga_allocated_qty.
              ENDIF.
            ELSE.

              IF l_tabix = w_val AND w_rank1 <> wa_main1-rank.
                wa_oijnomi-ga_allocated_qty = w_qty.
                MODIFY it_oijnomi FROM wa_oijnomi INDEX l_tabix1 TRANSPORTING ga_allocated_qty.

              ELSEIF w_rank1 = wa_main1-rank.
                IF w_qty3 > 0.
                  w_qty2 =  ( wa_oijnomi-menge * w_qty3 ) / w_qty1.
                  wa_oijnomi-ga_allocated_qty = w_qty2.
                  w_qty = w_qty - w_qty2.
                  MODIFY it_oijnomi FROM wa_oijnomi INDEX l_tabix1 TRANSPORTING ga_allocated_qty.
                ELSE.
                  wa_oijnomi-ga_allocated_qty = 0.
                  MODIFY it_oijnomi FROM wa_oijnomi INDEX l_tabix1 TRANSPORTING ga_allocated_qty.
                ENDIF.
              ELSE.
                IF w_qty1 > w_qty3 OR w_qty3 IS INITIAL.

                  IF w_qty3 > 0.
                    w_qty2 =  ( wa_oijnomi-menge * w_qty3 ) / w_qty1.
                  ELSE.
                    w_qty2  = 0.
                  ENDIF.
                  wa_oijnomi-ga_allocated_qty = w_qty2.
                  w_qty = w_qty - w_qty2.
                  MODIFY it_oijnomi FROM wa_oijnomi INDEX l_tabix1 TRANSPORTING ga_allocated_qty.
                ELSE.
                  wa_oijnomi-ga_allocated_qty = wa_oijnomi-menge.
                  w_qty = w_qty - wa_oijnomi-menge.
                  MODIFY it_oijnomi FROM wa_oijnomi INDEX l_tabix1 TRANSPORTING ga_allocated_qty.
                ENDIF.

              ENDIF.

            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      DATA : it_main_r TYPE TABLE OF ty_main_r,
             wa_main_r TYPE ty_main_r.
      REFRESH it_main_r.

      LOOP AT i_main INTO wa_main.
        wa_main_r-nomtk = wa_main-nomtk.
        wa_main_r-nomit = wa_main-nomit.
        wa_main_r-rank = wa_main-rank.
        wa_main_r-date = wa_main-date.
        wa_main_r-locid = wa_main-locid.
        APPEND wa_main_r TO it_main_r.
      ENDLOOP.

      SORT it_main_r BY date ASCENDING locid ASCENDING rank ASCENDING.

      DATA l_ind TYPE sy-tabix.
      DATA l_ind1 TYPE sy-tabix.

      SORT i_main BY rank nomtk nomit.
      DATA l_rank TYPE i.
      CLEAR :w_qty , w_qty1,w_qty2,w_qty3,l_rank,w_qty4,w_qty_alloc.
      DATA : l_tab   TYPE sy-tabix,l_tab1 TYPE sy-tabix, l_tab_u TYPE sy-tabix.

      LOOP AT i_main2 INTO wa_main2.
        READ TABLE it_yrxa_cmdata INTO wa_yrxa_cmdata WITH KEY yybus_location = wa_main2-locid yydate = wa_main2-date.
        IF sy-subrc = 0.
          w_qty = wa_yrxa_cmdata-yycum_vol.
        ENDIF.
        LOOP AT it_main_r INTO wa_main_r WHERE date = wa_main2-date AND locid = wa_main2-locid.
          READ TABLE i_main INTO wa_main WITH KEY
          nomtk = wa_main_r-nomtk nomit = wa_main_r-nomit.
          l_rank = wa_main-rank.
          READ TABLE it_oijnomi INTO wa_oijnomi WITH KEY nomtk = wa_main-nomtk nomit = wa_main-nomit.
          IF sy-subrc = 0.
            l_ind1 = sy-tabix.
            l_tab = wa_oijnomi-ga_rank .
            l_tab_u = wa_oijnomi-ga_rank.
            w_qty1 = w_qty1 + wa_oijnomi-menge.
            w_qty2 = w_qty2 + wa_oijnomi-ga_allocated_qty.

          ENDIF.

          AT END OF rank.
            IF w_qty1 = w_qty2.
              CLEAR : w_qty1,w_qty2.
            ELSE.
              l_rank = l_rank + 1.
              READ TABLE i_main INTO wa_main2 WITH KEY rank = l_rank date = wa_main2-date locid = wa_main2-locid.
              IF sy-subrc = 0.
                CLEAR wa_oijnomi_temp.
                READ TABLE it_oijnomi INTO wa_oijnomi_temp WITH KEY nomtk = wa_main2-nomtk nomit = wa_main2-nomit.
                IF sy-subrc = 0.
                  l_ind = sy-tabix.
                  l_tab1 = wa_oijnomi_temp-ga_rank.
                ENDIF.
                IF wa_oijnomi_temp-ga_allocated_qty IS INITIAL.
                  CLEAR : w_qty1,w_qty2.
                  CONTINUE.
                ELSEIF wa_oijnomi_temp-ga_allocated_qty IS NOT INITIAL.
                  w_qty2 = w_qty1 - w_qty2.
                  w_qty4 = wa_oijnomi_temp-ga_allocated_qty - w_qty4.
                  IF w_qty4 < 0.
                    w_qty4 = wa_oijnomi_temp-ga_allocated_qty.
                    CLEAR wa_oijnomi_temp-ga_allocated_qty.
                  ELSE.
                    wa_oijnomi_temp-ga_allocated_qty = wa_oijnomi_temp-ga_allocated_qty - w_qty4.
                  ENDIF.
                  wa_oijnomi-ga_allocated_qty = wa_oijnomi-ga_allocated_qty + w_qty4.
                  MODIFY it_oijnomi FROM wa_oijnomi  INDEX l_ind1.
                  MODIFY it_oijnomi FROM wa_oijnomi_temp INDEX l_ind.
                ENDIF.
              ELSE.
                IF abs( w_qty2 ) <= '0.009' AND w_qty2 IS NOT INITIAL.
                  l_tab = l_tab - 1.
                  IF l_tab = 0.
                    EXIT.
                  ENDIF.
                  LOOP AT it_oijnomi INTO wa_oijnomi_temp WHERE ga_rank = l_tab AND
                    ga_allocated_qty >  abs( w_qty2 )..
                    l_ind = sy-tabix.
                    EXIT.                               "#EC CI_NOORDER
                  ENDLOOP.
                  IF sy-subrc = 0.

                    IF ( wa_oijnomi_temp-ga_allocated_qty < wa_oijnomi_temp-menge ) OR w_qty2 < 0.
                      wa_oijnomi_temp-ga_allocated_qty = wa_oijnomi_temp-ga_allocated_qty + w_qty2.
                      MODIFY it_oijnomi FROM wa_oijnomi_temp INDEX l_ind.
                      l_tab = l_tab + 1.
                      CLEAR wa_oijnomi-ga_allocated_qty.
                      MODIFY it_oijnomi FROM wa_oijnomi INDEX l_ind1.
                    ENDIF..
                  ENDIF..

                ENDIF.
              ENDIF.
            ENDIF.
            CLEAR : w_qty1,w_qty2.
          ENDAT.

        ENDLOOP.

        CLEAR: w_qty_alloc,w_qty4.

        LOOP AT it_oijnomi INTO wa_oijnomi WHERE idate = wa_main2-date AND locid = wa_main2-locid.
          w_qty_alloc = w_qty_alloc + wa_oijnomi-ga_allocated_qty.
        ENDLOOP.

        IF w_qty <> w_qty_alloc.
          w_qty_alloc = w_qty - w_qty_alloc.

          READ TABLE it_oijnomi INTO wa_oijnomi WITH KEY  nomtk = wa_main-nomtk nomit = wa_main-nomit.
          IF sy-subrc = 0.

            IF   wa_oijnomi-ga_allocated_qty > 0.
              wa_oijnomi-ga_allocated_qty = wa_oijnomi-ga_allocated_qty + w_qty_alloc.
              MODIFY it_oijnomi FROM wa_oijnomi INDEX sy-tabix.
            ELSE.
              CLEAR l_tab1.
              LOOP AT  it_oijnomi INTO wa_oijnomi WHERE  ga_allocated_qty > 0 AND idate = wa_main2-date AND locid = wa_main2-locid.
                l_tab1 = sy-tabix.
                EXIT .                                  "#EC CI_NOORDER
              ENDLOOP.
              IF l_tab1 > 0.
                wa_oijnomi-ga_allocated_qty = wa_oijnomi-ga_allocated_qty + w_qty_alloc.
                MODIFY it_oijnomi FROM wa_oijnomi INDEX l_tab1 TRANSPORTING ga_allocated_qty.
              ENDIF.
              CLEAR l_tab1.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

************************************final update.
      LOOP AT it_oijnomi INTO wa_oijnomi.
        REFRESH : lt_oijnomh,lt_oijnomi,lt_nom_item,lt_nom_old.
        CLEAR: ls_nom_header,ls_oijnomi_key,ls_oijnomh,ls_oijnomi,lt_ret,lt_nom_header,ls_nom_item,ls_nom_item1,ls_nom_old,wa_ret,l_bstnk.

        REFRESH lt_nom_key.
        wa_nomh-nomtk = wa_oijnomi-nomtk.
        APPEND wa_nomh TO lt_nom_key.

        CALL FUNCTION 'OIJNOMH_ARRAY_READ'
          EXPORTING
            it_oijnomh_key        = lt_nom_key
          IMPORTING
            et_oijnomh            = lt_oijnomh
          EXCEPTIONS
            records_not_found     = 1
            records_not_requested = 2
            OTHERS                = 3.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        CALL FUNCTION 'OIJNOMI_ARRAY_READ'
          EXPORTING
            it_oijnomh_key        = lt_nom_key
          IMPORTING
            et_oijnomi            = lt_oijnomi
          EXCEPTIONS
            records_not_found     = 1
            records_not_requested = 2
            OTHERS                = 3.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        LOOP AT lt_oijnomh INTO ls_oijnomh.
          MOVE-CORRESPONDING ls_oijnomh TO ls_nom_header.
          APPEND ls_nom_header TO lt_nom_header.
        ENDLOOP.

        LOOP AT lt_oijnomi INTO ls_oijnomi WHERE nomit = wa_oijnomi-nomit.
          MOVE-CORRESPONDING ls_oijnomi TO ls_nom_item.
          APPEND ls_nom_item TO lt_nom_item.
        ENDLOOP.

        CALL FUNCTION 'OIJ_NOM_DEFAULTING'
          EXPORTING
            it_nom_header     = lt_nom_header
            it_nom_item       = lt_nom_item
          IMPORTING
            et_nom_header     = lt_nom_header
            et_nom_item       = lt_nom_item
          EXCEPTIONS
            defaulting_error  = 1
            no_records_passed = 2
            OTHERS            = 3.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        REFRESH   lt_nom_item1.
        MOVE lt_nom_item[] TO lt_nom_item1[].

        CLEAR l_tabix.
        LOOP AT lt_nom_item INTO ls_nom_item.
          l_tabix = sy-tabix.
          ls_nom_item-ga_allocated_qty = wa_oijnomi-ga_allocated_qty.

**SOC BY RAVI/PRATIBHA ON 09.11.2023 CHARM: 4000007316, GMS: Modify YRGR040_NON-GAIL PUR alloc
          DATA: qty_a          TYPE oib_adqnt,
                qty_b          TYPE oib_adqnt,
                final_quantity TYPE oijnomi-ga_allocated_qty,
                return         TYPE tdspbapiret2,
                quant_temp     TYPE oib_adqnt.
          CLEAR wa_yrxa_cmdata.
          READ TABLE it_yrxa_cmdata INTO wa_yrxa_cmdata WITH KEY yybus_location = wa_oijnomi-locid yydate = wa_oijnomi-idate.

          IF sy-subrc = 0.
            CLEAR: quant_temp.
            quant_temp = wa_oijnomi-ga_allocated_qty.
            CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
              EXPORTING
                i_trqty   = quant_temp
                i_truom   = 'SM3'
                i_tguom   = ls_nom_item-yyoij_cnom_uom
                lv_gcv    = wa_yrxa_cmdata-yyavg_gcv
                lv_ncv    = wa_yrxa_cmdata-yyavg_ncv
              CHANGING
                c_tgqty   = qty_a
                ct_return = return.

          ENDIF.

          CLEAR: wa_oij_el_doc_mot,wa_ekko,wa_yrga_oa_alloc.

          READ TABLE it_oij_el_doc_mot INTO wa_oij_el_doc_mot WITH KEY vbeln = wa_oijnomi-docnr.
          IF sy-subrc EQ 0.
            READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_oij_el_doc_mot-refvbeln.
            IF sy-subrc EQ 0.
              READ TABLE it_yrga_oa_alloc INTO wa_yrga_oa_alloc WITH KEY ebeln = wa_ekko-ebeln gas_day = wa_oijnomi-idate.
              IF sy-subrc EQ 0.

                CLEAR return.
                CLEAR: quant_temp.
                quant_temp = wa_yrga_oa_alloc-allocation.
                CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
                  EXPORTING
                    i_trqty   = quant_temp
                    i_truom   = wa_yrga_oa_alloc-uom
                    i_tguom   = ls_nom_item-yyoij_cnom_uom
                    lv_gcv    = wa_yrga_oa_alloc-agcv
                    lv_ncv    = wa_yrga_oa_alloc-ancv
                  CHANGING
                    c_tgqty   = qty_b
                    ct_return = return.

                CLEAR final_quantity.
                final_quantity = qty_b - qty_a.
                IF final_quantity > 0.
                  ls_nom_item-yyoij_dpimb_qty = final_quantity.
                  CLEAR ls_nom_item-yyoij_dnimb_qty.
                ELSE.
                  ls_nom_item-yyoij_dnimb_qty = final_quantity.
                  CLEAR ls_nom_item-yyoij_dpimb_qty.
                ENDIF.

              ENDIF.
            ENDIF.
          ENDIF.
**EOC BY RAVI/PRATIBHA ON 09.11.2023 CHARM: 4000007316, GMS: Modify YRGR040_NON-GAIL PUR alloc

          ls_nom_item-updkz = 'U'.
          MODIFY lt_nom_item FROM ls_nom_item INDEX l_tabix.
        ENDLOOP.

        CALL FUNCTION 'OIJ_NOM_MAINTAIN_MULTIPLE'
          EXPORTING
            it_nom_header     = lt_nom_header
            it_nom_item       = lt_nom_item
            iv_internal       = 'X'
            iv_lock           = ' '
          IMPORTING
            et_nom_header     = lt_nom_header
            et_nom_item       = lt_nom_item
            et_return         = lt_ret
          EXCEPTIONS
            nomination_locked = 1
            error_phase_01    = 2
            error_phase_02    = 3
            OTHERS            = 4.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = 'X'.
      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.                    " NOMINATION_UPDATE
*&---------------------------------------------------------------------*
*&      Form  PROCESS_NOMINATION
*&---------------------------------------------------------------------*
FORM process_nomination .
  REFRESH it_oijnomi.

  IF r_dis <> 'X' AND r_act <> 'X'.                 "++ by MANMOHAN/SHREYOSI Charm:4000008514 date:23.07.2024

    SELECT * FROM oijnomi INTO TABLE it_oijnomi
      WHERE
      idate IN s_date
      AND
      locid EQ p_locid AND
      delind EQ space.

    IF it_oijnomi[] IS NOT INITIAL.
**SOC BY RAVI/PRATIBHA ON 09.11.2023 CHARM: 4000007316, GMS: Modify YRGR040_NON-GAIL PUR alloc
      SELECT vbeln refvbeln FROM oij_el_doc_mot INTO CORRESPONDING FIELDS OF TABLE it_oij_el_doc_mot
      FOR ALL ENTRIES IN it_oijnomi WHERE vbeln = it_oijnomi-docnr
        AND delind = ' '.

      DELETE it_oij_el_doc_mot[] WHERE vbeln+0(1) NE 4.

      DELETE it_oij_el_doc_mot[] WHERE refvbeln+0(1) NE 4.

      IF it_oij_el_doc_mot[] IS NOT INITIAL.

        SELECT ebeln FROM ekko INTO CORRESPONDING FIELDS OF TABLE it_ekko
        FOR ALL ENTRIES IN it_oij_el_doc_mot WHERE ebeln = it_oij_el_doc_mot-refvbeln.

        IF it_ekko[] IS NOT INITIAL.

          SELECT ebeln gas_day time_stamp allocation uom agcv ancv FROM yrga_oa_alloc INTO CORRESPONDING FIELDS OF TABLE it_yrga_oa_alloc
          FOR ALL ENTRIES IN it_ekko WHERE ebeln = it_ekko-ebeln AND gas_day IN s_date.

          SORT it_yrga_oa_alloc BY ebeln DESCENDING gas_day DESCENDING time_stamp DESCENDING.
          DELETE ADJACENT DUPLICATES FROM it_yrga_oa_alloc COMPARING ebeln gas_day.
        ENDIF.

      ENDIF.
**EOC BY RAVI/PRATIBHA ON 09.11.2023 CHARM: 4000007316, GMS: Modify YRGR040_NON-GAIL PUR alloc

      SELECT * INTO TABLE it_oijnomih
        FROM oijnomh
        FOR ALL ENTRIES IN it_oijnomi
        WHERE
        nomtk = it_oijnomi-nomtk
        AND delind = ' '.

      SELECT * INTO TABLE it_yrxa_cmdata
        FROM yrxa_cmdata
        FOR ALL ENTRIES IN it_oijnomi
        WHERE
        yybus_location = it_oijnomi-locid
        AND
        yydate = it_oijnomi-idate.

      IF sy-subrc = 0.
        SORT it_yrxa_cmdata BY yybus_location yydate yytimestamp DESCENDING.
        DELETE ADJACENT DUPLICATES FROM it_yrxa_cmdata COMPARING yybus_location yydate .
      ENDIF.


********************check for ticket completed or not
      REFRESH it_oijnomi_post.

      DATA : it_tic  TYPE TABLE OF  oij_el_ticket_i,
             wa_tic  TYPE  oij_el_ticket_i,
             wa_tic1 TYPE  oij_el_ticket_i.

      DATA:
        it_oijnomi_post1 TYPE oijnomi OCCURS 0,
        it_oijnomi_tkt   TYPE oijnomi OCCURS 0.

      SORT it_oijnomi BY nomtk nomit.
      MOVE it_oijnomi[] TO it_oijnomi_tkt[].
      SELECT * INTO TABLE it_tic FROM  oij_el_ticket_i
        FOR ALL ENTRIES IN it_oijnomi
        WHERE
        nomtk = it_oijnomi-nomtk
        AND
        nomit = it_oijnomi-nomit.
      IF sy-subrc = 0.
        SORT it_oijnomi BY nomtk nomit.
        SORT it_tic BY nomtk nomit locid ticket_purpose.
        MOVE it_oijnomi[] TO it_oijnomi_post1[].

        LOOP AT it_oijnomi_post1 INTO wa_oijnomi.
          READ TABLE it_tic INTO wa_tic WITH KEY
          nomtk = wa_oijnomi-nomtk
          nomit = wa_oijnomi-nomit
          locid = wa_oijnomi-locid
          ticket_purpose = '1'.
          IF sy-subrc = 0.
            LOOP AT it_tic INTO wa_tic WHERE
               nomtk = wa_oijnomi-nomtk AND
            nomit = wa_oijnomi-nomit AND
            locid = wa_oijnomi-locid AND
            ticket_purpose = '1'.

              READ TABLE it_tic INTO wa_tic1 WITH KEY
               ticket_key = wa_tic-ticket_key
               ticket_purpose = '5'
               status         = 'C'
               substatus      = '6'.

              IF sy-subrc <> 0.
                DELETE it_oijnomi WHERE idate = wa_oijnomi-idate AND locid = wa_oijnomi-locid.
                APPEND wa_oijnomi TO it_oijnomi_post.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDLOOP.
      ENDIF.

***Contract reference outline agreement validation
**SOC BY RAVI/PRATIBHA ON 09.11.2023 CHARM: 4000007316, GMS: Modify YRGR040_NON-GAIL PUR alloc
      DATA: it_oijnomi_temp1 TYPE STANDARD TABLE OF oijnomi,
            wa_oijnomi_temp1 TYPE oijnomi,
            temp_date1       TYPE char10,
            error_msg3       TYPE char255,
            reference_check  TYPE char1.


      MOVE it_oijnomi[] TO it_oijnomi_temp1[].
      CLEAR: error_msg3, reference_check.

      LOOP AT it_oijnomi_temp1 INTO wa_oijnomi_temp1.
        READ TABLE it_oij_el_doc_mot INTO wa_oij_el_doc_mot WITH KEY vbeln = wa_oijnomi_temp1-docnr.
        IF sy-subrc EQ 0.
          IF wa_oij_el_doc_mot-refvbeln IS NOT INITIAL.
            READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_oij_el_doc_mot-refvbeln.
            IF sy-subrc EQ 0.
              READ TABLE it_yrga_oa_alloc INTO wa_yrga_oa_alloc WITH KEY ebeln = wa_ekko-ebeln gas_day = wa_oijnomi_temp1-idate.
              IF sy-subrc NE 0.
                DELETE it_oijnomi WHERE idate = wa_oijnomi_temp1-idate.
                CLEAR temp_date1.
                CONCATENATE wa_oijnomi_temp1-idate+6(2) '.' wa_oijnomi_temp1-idate+4(2) '.' wa_oijnomi_temp1-idate+0(4) INTO temp_date1.
                CONCATENATE 'Allocation is not found for reference purchase contract' wa_ekko-ebeln 'for gas day' temp_date1 INTO error_msg3 SEPARATED BY space.
                MESSAGE: error_msg3 TYPE 'I' DISPLAY LIKE 'E'.
                reference_check = 'X'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

**EOC BY RAVI/PRATIBHA ON 09.11.2023 CHARM: 4000007316, GMS: Modify YRGR040_NON-GAIL PUR alloc

      REFRESH i_main.
      LOOP AT it_oijnomi INTO wa_oijnomi.
        CLEAR wa_yrxa_cmdata .

        READ TABLE it_oijnomih INTO wa_oijnomih WITH KEY nomtk = wa_oijnomi-nomtk.
        IF sy-subrc = 0.
          wa_main-tsyst   = wa_oijnomih-tsyst.
        ENDIF.
        wa_main-date    = wa_oijnomi-idate.
        wa_main-locid = wa_oijnomi-locid.
        wa_main-matnr = wa_oijnomi-matnr_i.
        wa_main-menge   = wa_oijnomi-menge.
        wa_main-unit    = wa_oijnomi-unit_i.
        wa_main-rank = wa_oijnomi-ga_rank.
        READ TABLE it_yrxa_cmdata INTO wa_yrxa_cmdata WITH KEY yybus_location = wa_main-locid yydate = wa_main-date.
        IF p_gcv IS INITIAL.
          wa_main-agcv = wa_yrxa_cmdata-yyavg_gcv.
        ELSE.
          wa_main-agcv = p_gcv.
        ENDIF.
        IF p_ncv IS INITIAL.
          wa_main-ancv = wa_yrxa_cmdata-yyavg_ncv.
        ELSE.
          wa_main-ancv = p_ncv.

        ENDIF.
        wa_main-nomtk = wa_oijnomi-nomtk.
        wa_main-nomit = wa_oijnomi-nomit.
        APPEND wa_main TO i_main.
      ENDLOOP.

      IF p_locid IS NOT INITIAL AND s_date-low IS NOT INITIAL.

        SELECT yybus_location,
               yydate,
               yytimestamp,
               yycum_vol FROM yrxa_cmdata INTO TABLE @DATA(it_qchk1)
                                               WHERE yybus_location = @p_locid
                                               AND   yydate IN @s_date.

        IF sy-subrc = 0.
          SORT  it_qchk1 BY yybus_location yydate yytimestamp DESCENDING.
          DELETE ADJACENT DUPLICATES FROM it_qchk1 COMPARING yybus_location yydate .
        ENDIF.

        CLEAR tot_m_qty.

        LOOP AT it_qchk1 INTO DATA(wchk1).
          READ TABLE it_oijnomi WITH KEY idate = wchk1-yydate TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            tot_m_qty = tot_m_qty + wchk1-yycum_vol.
          ENDIF.
        ENDLOOP.

      ENDIF.

      IF i_main[] IS NOT  INITIAL .
        IF  it_oijnomi_post[] IS INITIAL OR it_oijnomi[] IS NOT INITIAL.
          REFRESH it_oijnomi_post.
          PERFORM nomination_update.
        ENDIF.
        REFRESH it_oijnomi_display.
        SELECT * INTO TABLE it_oijnomi_display
          FROM oijnomi
          FOR ALL ENTRIES IN i_main
          WHERE
          nomtk = i_main-nomtk
          AND
          nomit = i_main-nomit
          AND delind = ' '.
        IF sy-subrc = 0.
          LOOP AT it_oijnomi_display INTO wa_oijnomi.
            wa_oijnomi-yyoij_dpimb_qty = wa_oijnomi-ga_allocated_qty.
            MODIFY it_oijnomi_display FROM wa_oijnomi INDEX sy-tabix.
          ENDLOOP.
        ENDIF.
      ENDIF.

      IF it_oijnomi_display[] IS NOT INITIAL AND it_oijnomi_post[] IS  INITIAL.

        CLEAR: wa_total_nom,
               p_qty.

        SELECT * INTO TABLE it_yro_nom_param_alloc
                FROM yro_nom_param1
                FOR ALL ENTRIES IN it_oijnomi_display
                WHERE
                nomtk = it_oijnomi_display-nomtk
                AND
                nomit = it_oijnomi_display-nomit.

        LOOP AT it_oijnomi_display INTO wa_oijnomi_display.
          DATA(tbx) = sy-tabix.
          CLEAR : l_adq,l_gcv,l_ncv,wa_yro_nom_param1.
          p_qty = p_qty + wa_oijnomi_display-ga_allocated_qty.
          new_qty = new_qty + wa_oijnomi_display-yyoij_dpimb_qty.

          READ TABLE it_yro_nom_param_alloc INTO wa_yro_nom_param1 WITH KEY nomtk = wa_oijnomi_display-nomtk
                                        nomit = wa_oijnomi_display-nomit.
          IF sy-subrc = 0.
            IF p_gcv IS INITIAL.
              l_gcv = wa_yro_nom_param1-agcv.
              l_ncv = wa_yro_nom_param1-ancv.
            ELSE.
              l_gcv = p_gcv.
              l_ncv = p_ncv.
            ENDIF.
          ENDIF.

          wa_total_nom = wa_total_nom + wa_oijnomi_display-menge.
          l_adq = wa_oijnomi_display-ga_allocated_qty.
          CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
            EXPORTING
              i_trqty = l_adq
              i_truom = 'SM3'
              i_tguom = wa_oijnomi_display-yyoij_cnom_uom
              lv_gcv  = l_gcv
              lv_ncv  = l_ncv
            CHANGING
              c_tgqty = l_adq.

          wa_oijnomi_display-yyoij_dnimb_qty = l_adq.
          MODIFY it_oijnomi_display FROM wa_oijnomi_display INDEX tbx
          TRANSPORTING yyoij_dnimb_qty.
        ENDLOOP.
        CLEAR:tbx.

        PERFORM display_alv.

      ELSE.
        IF reference_check NE 'X'.
          MESSAGE 'Ticket already generated Please select display mode' TYPE 'I'.
        ENDIF.
      ENDIF.
    ENDIF.

  ELSE.
*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
    CLEAR: wa_total_nom, new_qty, tot_m_qty.
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
    c_check = ''.
    SELECT * FROM oijnomi INTO TABLE it_oijnomi_post
    WHERE idate IN s_date AND
*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
    locid IN p_locid1 AND
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
    delind EQ space.
*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
    IF p_locid1 IS NOT INITIAL AND s_date-low IS NOT INITIAL.
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
      SELECT yybus_location,
      yydate,
      yytimestamp,
      yycum_vol FROM yrxa_cmdata INTO TABLE @DATA(it_qchk2)
*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
                                      WHERE yybus_location IN @p_locid1
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
                                      AND   yydate IN @s_date.

      IF sy-subrc = 0.
        SORT it_qchk2 BY yybus_location yydate yytimestamp DESCENDING.
        DELETE ADJACENT DUPLICATES FROM it_qchk2 COMPARING yybus_location yydate .
      ENDIF.

      CLEAR tot_m_qty.
      LOOP AT it_qchk2 INTO DATA(wchk2).
        tot_m_qty = tot_m_qty + wchk2-yycum_vol.
      ENDLOOP.

    ENDIF.

    IF it_oijnomi_post[] IS NOT INITIAL.
      SELECT * INTO TABLE it_yro_nom_param1
        FROM yro_nom_param1
        FOR ALL ENTRIES IN it_oijnomi_post
        WHERE
        nomtk = it_oijnomi_post-nomtk
        AND
        nomit = it_oijnomi_post-nomit.

    ENDIF.

    LOOP AT it_oijnomi_post INTO wa_oijnomi.
      APPEND wa_oijnomi TO it_oijnomi_display.
    ENDLOOP.


    IF it_oijnomi_display[] IS NOT INITIAL.
      DATA l_tabix TYPE sy-tabix.
      CLEAR: wa_total_nom,p_qty.
      LOOP AT it_oijnomi_display INTO wa_oijnomi_display.
        l_tabix = sy-tabix.
        wa_total_nom = wa_total_nom + wa_oijnomi_display-menge.
        p_qty = p_qty +  wa_oijnomi_display-ga_allocated_qty.
        new_qty = new_qty + wa_oijnomi_display-ga_allocated_qty.

        CLEAR : l_adq,l_gcv,l_ncv.

        READ TABLE it_yro_nom_param1 INTO wa_yro_nom_param1 WITH KEY nomtk = wa_oijnomi_display-nomtk
                                        nomit = wa_oijnomi_display-nomit.
        IF sy-subrc = 0.
          l_gcv = wa_yro_nom_param1-agcv.
          l_ncv = wa_yro_nom_param1-ancv.
        ENDIF.
        l_adq = wa_oijnomi_display-ga_allocated_qty.
        CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
          EXPORTING
            i_trqty = l_adq
            i_truom = 'SM3'
            i_tguom = wa_oijnomi_display-yyoij_cnom_uom
            lv_gcv  = l_gcv
            lv_ncv  = l_ncv
          CHANGING
            c_tgqty = l_adq.
        wa_oijnomi_display-yyoij_dnimb_qty = l_adq.
        MODIFY it_oijnomi_display FROM wa_oijnomi_display INDEX l_tabix
        TRANSPORTING yyoij_dnimb_qty.
      ENDLOOP.
*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
      IF r_act IS NOT INITIAL.

        DATA : lt_oij_el_ticket_i TYPE TABLE OF oij_el_ticket_i.
        DATA: v_repid LIKE sy-repid.

        v_repid = sy-repid .

        i_layout-zebra      = 'X'.
        i_layout-no_rowmark = 'X'.

        REFRESH it_oijnomi_display_final.

        it_oijnomi_display_final[] = CORRESPONDING #( it_oijnomi_display[] ).

        SORT it_oijnomi_display_final BY idate ASCENDING.

        PERFORM ticket_details.

        DELETE it_oijnomi_display_final WHERE stat IS INITIAL.

        SORT it_oijnomi_display_final BY idate locid.
        CALL SCREEN 9000.
      ELSE.
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
        PERFORM display_alv.
*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
      ENDIF.
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
    ENDIF.
  ENDIF.
  REFRESH i_main.


ENDFORM.                    " PROCESS_NOMINATION

FORM display_alv.

  PERFORM create_fieldcat.

  DATA: v_repid LIKE sy-repid.

  v_repid = sy-repid .
  i_layout-zebra   = 'X' .
  i_layout-cwidth_opt = 'X' .
  i_layout-stylefname = 'CELLTAB'.
  i_layout-info_fname = ''.

  REFRESH it_oijnomi_display_final.

  it_oijnomi_display_final[] = CORRESPONDING #( it_oijnomi_display[] ).
*soc by MANMOHAN/SHREYOSI Charm:4000008514 date:20.08.2024
  SORT it_oijnomi_display_final BY idate locid nomtk nomit.
*eoc by MANMOHAN/SHREYOSI Charm:4000008514 date:20.08.2024
  IF r_dis EQ 'X'.
    i_layout-info_fname = 'COLOR'.
    PERFORM ticket_details.
  ENDIF.

  LOOP AT it_oijnomi_display_final ASSIGNING FIELD-SYMBOL(<fs_oijnomi>).
    IF <fs_oijnomi>-charg_o IS INITIAL AND <fs_oijnomi>-charg_d IS NOT INITIAL.
      <fs_oijnomi>-charg_o = <fs_oijnomi>-charg_d.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = v_repid
      i_callback_pf_status_set = 'PF'
      i_callback_user_command  = 'USER_COMMAND'
      i_callback_top_of_page   = 'TOP_PAGE'
      is_layout_lvc            = i_layout
      it_fieldcat_lvc          = gt_fieldcat[]
    TABLES
      t_outtab                 = it_oijnomi_display_final
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.

*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024

FORM display_act_alv.

  PERFORM create_act_fieldcat.
  DATA : lt_oij_el_ticket_i TYPE TABLE OF oij_el_ticket_i.
  DATA: v_repid LIKE sy-repid.

  v_repid = sy-repid .

  i_layout-zebra      = 'X'.
  i_layout-no_rowmark = 'X'.

  REFRESH it_oijnomi_display_final.

  it_oijnomi_display_final[] = CORRESPONDING #( it_oijnomi_display[] ).

  SORT it_oijnomi_display_final BY idate ASCENDING.

  SELECT * FROM oij_el_ticket_i INTO TABLE lt_oij_el_ticket_i
    FOR ALL ENTRIES IN it_oijnomi_display_final
    WHERE ticket_key = it_oijnomi_display_final-ticket_key
    AND   locid      = it_oijnomi_display_final-locid.

  LOOP AT it_oijnomi_display_final ASSIGNING FIELD-SYMBOL(<fs_symbol1>).

    READ TABLE lt_oij_el_ticket_i INTO DATA(ls_oij_el_ticket_i) WITH KEY ticket_key = <fs_symbol1>-ticket_key.

    IF sy-subrc EQ 0.
      <fs_symbol1>-version = ls_oij_el_ticket_i-ticket_version.
    ENDIF.

  ENDLOOP.

  PERFORM ticket_details.

  DELETE it_oijnomi_display_final WHERE stat IS INITIAL.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program       = v_repid
      i_callback_pf_status_set = 'PF'
      i_callback_user_command  = 'USER_COMMAND1'
      i_callback_top_of_page   = 'TOP_PAGE'
      is_layout_lvc            = i_layout
      it_fieldcat_lvc          = gt_fieldcat[]
    TABLES
      t_outtab                 = it_oijnomi_display_final
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.


FORM user_command1 USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.

  DATA : lv_lines        TYPE sytabix,
         lv_index        TYPE syindex,
         lt_message      TYPE tab_bdcmsgcoll,
         ls_message      TYPE bdcmsgcoll,
         lv_check        TYPE char1,
         lv_count        TYPE i,
         lv_message(132) TYPE c.

  IF ( r_ucomm = 'BACK' OR r_ucomm = 'CANCEL' OR r_ucomm = 'EXIT' ).

    LEAVE TO TRANSACTION 'YRGR040'.

  ELSEIF r_ucomm = 'SEL_ALL'.

    LOOP AT it_oijnomi_display_final ASSIGNING FIELD-SYMBOL(<fs_final>).
      <fs_final>-sel = 'X'.
    ENDLOOP.

  ELSEIF r_ucomm = 'DESEL_ALL'.

    LOOP AT it_oijnomi_display_final ASSIGNING <fs_final>.
      CLEAR <fs_final>-sel.
    ENDLOOP.

  ELSEIF r_ucomm = 'REFRESH'.

    REFRESH it_oijnomi_display.
    PERFORM process_nomination..

  ELSEIF r_ucomm = 'SUBMIT'.

    DATA : lo_ref_grid TYPE REF TO cl_gui_alv_grid.
    IF lo_ref_grid IS INITIAL.
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = lo_ref_grid.
    ENDIF.

    IF lo_ref_grid IS NOT INITIAL.
      CALL METHOD lo_ref_grid->check_changed_data.
    ENDIF.

    READ TABLE it_oijnomi_display_final TRANSPORTING NO FIELDS WITH KEY sel = 'X'.
    IF sy-subrc NE 0.
      MESSAGE | Please select an entry for Actualization | TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ELSE.

      DATA(gt_final_tmp) = it_oijnomi_display_final[].
      DELETE gt_final_tmp WHERE sel IS INITIAL.


      LOOP AT gt_final_tmp INTO DATA(wa_final).
        LOOP AT it_oijnomi_display_final ASSIGNING <fs_final>  WHERE idate EQ wa_final-idate AND locid EQ wa_final-locid.
          <fs_final>-sel = 'X'.
        ENDLOOP.
      ENDLOOP.

      REFRESH gt_final_tmp.

      gt_final_tmp[] = it_oijnomi_display_final[].
      DELETE gt_final_tmp WHERE sel IS INITIAL.
      DESCRIBE TABLE gt_final_tmp LINES lv_lines.

      CLEAR lv_index.
      LOOP AT it_oijnomi_display_final ASSIGNING <fs_final> WHERE sel EQ 'X'.
        lv_index = lv_index + 1.

        cl_progress_indicator=>progress_indicate(
          EXPORTING
           i_text               = | Actualizing Ticket - { lv_index  } / { lv_lines } |
           i_processed          = lv_index
           i_total              = lv_lines
           i_output_immediately = abap_true ).

        CLEAR lt_message.
        CALL FUNCTION 'YRX_TICKET_ACTUAL_BACKGROND'
          EXPORTING
            i_ticketnr = <fs_final>-ticketnr
            i_item     = <fs_final>-ticket_item
            i_choose   = lv_check
            i_version  = <fs_final>-version
          TABLES
            et_message = lt_message.

        CLEAR : ls_message,
                lv_message.
        LOOP AT lt_message INTO ls_message WHERE msgid = 'OD'
                                           AND ( msgnr = '792'
                                              OR msgnr = '721'
                                              OR msgnr = '800').
          EXIT.
        ENDLOOP.
        IF sy-subrc NE 0.
          DESCRIBE TABLE lt_message LINES DATA(lv_tab_lines).
          READ TABLE lt_message INTO ls_message INDEX lv_tab_lines.
        ENDIF.
        IF ls_message IS NOT INITIAL.
          MESSAGE ID ls_message-msgid
                TYPE ls_message-msgtyp
              NUMBER ls_message-msgnr
                WITH ls_message-msgv1
                     ls_message-msgv2
                     ls_message-msgv3
                     ls_message-msgv4
                INTO lv_message.
          IF sy-subrc EQ 0.
            <fs_final>-message = lv_message.
          ENDIF.
        ENDIF.
      ENDLOOP.

      MESSAGE | Selected Ticket(s) have been Processed. Please check Message Column for Status | TYPE 'I'.

    ENDIF.
  ENDIF.

  rs_selfield-refresh = 'X'.

ENDFORM.

*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024

FORM user_command USING lv_okcode  LIKE sy-ucomm
                        l_selfield TYPE slis_selfield.

  DATA : ref_grid TYPE REF TO cl_gui_alv_grid.
  IF ref_grid IS INITIAL.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        e_grid = ref_grid.
  ENDIF.
  IF NOT ref_grid IS INITIAL.
    CALL METHOD ref_grid->check_changed_data.
  ENDIF.

  CASE lv_okcode.
    WHEN 'EXE'.
      READ TABLE it_oijnomi_display_final WITH KEY flag = 'X' TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        MESSAGE 'Please Select at least one line item to proceed' TYPE 'E' DISPLAY LIKE 'E'.
      ENDIF.
      PERFORM qty_check.
      PERFORM correct_nomi.
      l_selfield-refresh = c_check.
      LEAVE  TO CURRENT TRANSACTION.

    WHEN 'BACK'.
      LEAVE  TO CURRENT TRANSACTION.

    WHEN '&ALLSEL'.
      LOOP AT it_oijnomi_display_final INTO wa_oijnomi_display_final.
        wa_oijnomi_display_final-flag = 'X'.
        MODIFY it_oijnomi_display_final FROM wa_oijnomi_display_final.
      ENDLOOP.
      l_selfield-refresh = c_check.

    WHEN '&SALSEL'.
      LOOP AT it_oijnomi_display_final INTO wa_oijnomi_display_final.
        wa_oijnomi_display_final-flag = ''.
        MODIFY it_oijnomi_display_final FROM wa_oijnomi_display_final.
      ENDLOOP.
      l_selfield-refresh = c_check.


  ENDCASE.

ENDFORM.

FORM pf USING rt_extab TYPE slis_t_extab.

  IF r_dis <> 'X'.
    SET PF-STATUS 'DIS_ALV_SCREEN'.
  ELSE.
    SET PF-STATUS 'DIS_ALV_SCREEN' EXCLUDING 'EXE'.
  ENDIF.

  IF r_act EQ 'X'.
    SET PF-STATUS 'YACT'.
  ENDIF.

ENDFORM.

FORM ticket_details.

  DATA: lt_oij_el_ticket_h_temp1 TYPE TABLE OF oij_el_ticket_h,
        lt_oij_el_ticket_i_temp1 TYPE TABLE OF oij_el_ticket_i.


  SELECT * FROM oij_el_ticket_i INTO TABLE lt_oij_el_ticket_i_temp1 FOR ALL ENTRIES IN it_oijnomi_display_final
  WHERE nomtk = it_oijnomi_display_final-nomtk AND nomit = it_oijnomi_display_final-nomit.

  IF sy-subrc = 0.
    SORT lt_oij_el_ticket_i_temp1 BY erdat DESCENDING erzeit DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_oij_el_ticket_i_temp1 COMPARING nomtk nomit.

    SELECT * FROM oij_el_ticket_h INTO TABLE lt_oij_el_ticket_h_temp1 FOR ALL ENTRIES IN lt_oij_el_ticket_i_temp1
    WHERE ticket_key = lt_oij_el_ticket_i_temp1-ticket_key.
    IF sy-subrc = 0.

      LOOP AT it_oijnomi_display_final INTO wa_oijnomi_display_final.
        READ TABLE lt_oij_el_ticket_i_temp1 INTO DATA(wa_oij_el_ticket_i_temp1) WITH KEY nomtk = wa_oijnomi_display_final-nomtk nomit = wa_oijnomi_display_final-nomit.
        IF sy-subrc = 0.
          READ TABLE lt_oij_el_ticket_h_temp1 INTO DATA(wa_oij_el_ticket_h_temp1) WITH KEY ticket_key = wa_oij_el_ticket_i_temp1-ticket_key.
          IF sy-subrc = 0.
            wa_oijnomi_display_final-ticketnr = wa_oij_el_ticket_h_temp1-ticketnr.
            wa_oijnomi_display_final-ticket_item = wa_oij_el_ticket_i_temp1-ticket_item.
            wa_oijnomi_display_final-ticket_key = wa_oij_el_ticket_i_temp1-ticket_key.
*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.09.2024
            wa_oijnomi_display_final-version = wa_oij_el_ticket_i_temp1-ticket_version.
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:23.09.2024
            IF wa_oij_el_ticket_i_temp1-ticket_purpose EQ '1' AND wa_oij_el_ticket_i_temp1-status EQ 'C' AND wa_oij_el_ticket_i_temp1-substatus EQ '6' AND wa_oij_el_ticket_i_temp1-tktsubrc NE '1A'.
              wa_oijnomi_display_final-post_status = 'Completely Posted'.
            ELSEIF wa_oij_el_ticket_i_temp1-ticket_purpose EQ '1' AND ( wa_oij_el_ticket_i_temp1-status NE 'C' OR wa_oij_el_ticket_i_temp1-substatus NE '6' ).
              wa_oijnomi_display_final-post_status = 'Partially Posted'.
*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
              wa_oijnomi_display_final-stat = '1'.
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
              wa_main-color = 'C601'.
            ELSEIF wa_oij_el_ticket_i_temp1-ticket_purpose EQ '1' AND wa_oij_el_ticket_i_temp1-status EQ 'C' AND wa_oij_el_ticket_i_temp1-substatus EQ '6' AND wa_oij_el_ticket_i_temp1-tktsubrc EQ '1A'.
              wa_oijnomi_display_final-post_status = 'Posted Ticket Corrupted'.
            ELSEIF wa_oij_el_ticket_i_temp1-ticket_purpose EQ '5' AND wa_oij_el_ticket_i_temp1-status EQ 'C' AND wa_oij_el_ticket_i_temp1-substatus EQ '6' AND wa_oij_el_ticket_i_temp1-tktsubrc NE '1A'.
              wa_oijnomi_display_final-post_status = 'Completely Reversed'.
            ELSEIF wa_oij_el_ticket_i_temp1-ticket_purpose EQ '5' AND ( wa_oij_el_ticket_i_temp1-status NE 'C' OR wa_oij_el_ticket_i_temp1-substatus NE '6' ).
              wa_oijnomi_display_final-post_status = 'Partially Reversed'.
*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
              wa_oijnomi_display_final-stat = '2'.
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
              wa_main-color = 'C601'.
            ELSEIF wa_oij_el_ticket_i_temp1-ticket_purpose EQ '5' AND wa_oij_el_ticket_i_temp1-status EQ 'C' AND wa_oij_el_ticket_i_temp1-substatus EQ '6' AND wa_oij_el_ticket_i_temp1-tktsubrc EQ '1A'.
              wa_oijnomi_display_final-post_status = 'Reversed Ticket Corrupted.'.
            ENDIF.
            MODIFY it_oijnomi_display_final FROM wa_oijnomi_display_final.
          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDIF.


ENDFORM.

FORM create_fieldcat.

  DATA: w_sr TYPE i.
  w_sr = 1 .
  REFRESH: gt_fieldcat.

*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
  gt_fieldcat-fieldname ='sel'.
  gt_fieldcat-no_out = 'X'.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024

  gt_fieldcat-fieldname = 'FLAG'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  10.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = ''.
  gt_fieldcat-scrtext_m    = ''.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  gt_fieldcat-checkbox = c_check.
  gt_fieldcat-edit = c_check.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'IDATE'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  10.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Gas Day'.
  gt_fieldcat-scrtext_m    = 'Gas Day'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:01.08.2024
  gt_fieldcat-fieldname = 'locid'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  10.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Location'.
  gt_fieldcat-scrtext_m    = 'Location'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:01.08.2024

  gt_fieldcat-fieldname = 'NOMTK'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  20.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Nomination Number'.
  gt_fieldcat-scrtext_m    = 'Nomination Number'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'NOMIT'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  10.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Nomination Line Item'.
  gt_fieldcat-scrtext_m    = 'Nomination Line Item'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'MATNR_I'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  18.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Material'.
  gt_fieldcat-scrtext_m    = 'Material'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'CHARG_O'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  18.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Batch'.
  gt_fieldcat-scrtext_m    = 'Batch'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'DOCNR'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  10.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Contract No'.
  gt_fieldcat-scrtext_m    = 'Contract No'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'YYOIJ_CNOM_QTY'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  13.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Nominated Qty'.
  gt_fieldcat-scrtext_m    = 'Nominated Qty'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'YYOIJ_CNOM_UOM'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  3.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'UOM'.
  gt_fieldcat-scrtext_m    = 'UOM'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'MENGE'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  13.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Corrected Nomination'.
  gt_fieldcat-scrtext_m    = 'Corrected Nomination'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'UNIT_I'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  3.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'UOM'.
  gt_fieldcat-scrtext_m    = 'UOM'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'GA_RANK'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  3.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Priority'.
  gt_fieldcat-scrtext_m    = 'Priority'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'GA_ALLOCATED_QTY'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  13.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Allocated_Qty(sys)'.
  gt_fieldcat-scrtext_m    = 'Allocated_Qty(sys)'.
  gt_fieldcat-scrtext_l    = 'Allocated_Qty(sys)'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'YYOIJ_DNIMB_QTY'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  13.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Allocated_Qty(C.UM)'.
  gt_fieldcat-scrtext_m    = 'Allocated_Qty(C.UM)'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  IF r_dis EQ 'X'.

    gt_fieldcat-fieldname = 'TICKETNR'.
    gt_fieldcat-key       = 'X' .
    gt_fieldcat-outputlen    =  20.
    gt_fieldcat-just         =  'L' .
    gt_fieldcat-col_pos      =  w_sr.
    gt_fieldcat-scrtext_s    = 'Ticket External No'.
    gt_fieldcat-scrtext_m    = 'Ticket External No'.
    gt_fieldcat-selddictxt      = 'M'.
    gt_fieldcat-do_sum       = space.
    APPEND gt_fieldcat.
    CLEAR gt_fieldcat.
    w_sr = w_sr  + 1.

    gt_fieldcat-fieldname = 'TICKET_KEY'.
    gt_fieldcat-key       = 'X' .
    gt_fieldcat-outputlen    =  20.
    gt_fieldcat-just         =  'L' .
    gt_fieldcat-col_pos      =  w_sr.
    gt_fieldcat-scrtext_s    = 'Ticket Key'.
    gt_fieldcat-scrtext_m    = 'Ticket Key'.
    gt_fieldcat-selddictxt      = 'M'.
    gt_fieldcat-do_sum       = space.
    APPEND gt_fieldcat.
    CLEAR gt_fieldcat.
    w_sr = w_sr  + 1.

    gt_fieldcat-fieldname = 'TICKET_ITEM'.
    gt_fieldcat-key       = 'X' .
    gt_fieldcat-outputlen    =  6.
    gt_fieldcat-just         =  'L' .
    gt_fieldcat-col_pos      =  w_sr.
    gt_fieldcat-scrtext_s    = 'Ticket Item No'.
    gt_fieldcat-scrtext_m    = 'Ticket Item No'.
    gt_fieldcat-selddictxt      = 'M'.
    gt_fieldcat-do_sum       = space.
    APPEND gt_fieldcat.
    CLEAR gt_fieldcat.
    w_sr = w_sr  + 1.

    gt_fieldcat-fieldname = 'POST_STATUS'.
    gt_fieldcat-key       = 'X' .
    gt_fieldcat-outputlen    =  30.
    gt_fieldcat-just         =  'L' .
    gt_fieldcat-col_pos      =  w_sr.
    gt_fieldcat-scrtext_s    = 'Ticket Post Status'.
    gt_fieldcat-scrtext_m    = 'Ticket Post Status'.
    gt_fieldcat-selddictxt      = 'M'.
    gt_fieldcat-do_sum       = space.
    APPEND gt_fieldcat.
    CLEAR gt_fieldcat.
    w_sr = w_sr  + 1.

  ENDIF.

ENDFORM.
*SOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024
FORM create_act_fieldcat.

  DATA: w_sr TYPE i.
  w_sr = 1 .
  REFRESH: gt_fieldcat.

  gt_fieldcat-fieldname = 'sel'.
  gt_fieldcat-checkbox  = 'X'.
  gt_fieldcat-edit      = 'X'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  5.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Sel'.
  gt_fieldcat-scrtext_m    = 'Select'.
  gt_fieldcat-scrtext_l    = 'Select'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'IDATE'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  10.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Gas Day'.
  gt_fieldcat-scrtext_m    = 'Gas Day'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'locid'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  10.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Location'.
  gt_fieldcat-scrtext_m    = 'Location'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'NOMTK'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  20.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Nomination Number'.
  gt_fieldcat-scrtext_m    = 'Nomination Number'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'NOMIT'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  10.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Nomination Line Item'.
  gt_fieldcat-scrtext_m    = 'Nomination Line Item'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'MATNR_I'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  18.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Material'.
  gt_fieldcat-scrtext_m    = 'Material'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'DOCNR'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  10.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Contract No'.
  gt_fieldcat-scrtext_m    = 'Contract No'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'YYOIJ_CNOM_QTY'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  13.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Nominated Qty'.
  gt_fieldcat-scrtext_m    = 'Nominated Qty'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'YYOIJ_CNOM_UOM'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  3.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'UOM'.
  gt_fieldcat-scrtext_m    = 'UOM'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'MENGE'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  13.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Corrected Nomination'.
  gt_fieldcat-scrtext_m    = 'Corrected Nomination'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'UNIT_I'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  3.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'UOM'.
  gt_fieldcat-scrtext_m    = 'UOM'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'GA_RANK'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  3.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Priority'.
  gt_fieldcat-scrtext_m    = 'Priority'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'GA_ALLOCATED_QTY'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  13.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Allocated_Qty(sys)'.
  gt_fieldcat-scrtext_m    = 'Allocated_Qty(sys)'.
  gt_fieldcat-scrtext_l    = 'Allocated_Qty(sys)'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'YYOIJ_DNIMB_QTY'.
  gt_fieldcat-key       = 'X'.
  gt_fieldcat-outputlen    =  13.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Allocated_Qty(C.UM)'.
  gt_fieldcat-scrtext_m    = 'Allocated_Qty(C.UM)'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'TICKETNR'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  20.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Ticket External No'.
  gt_fieldcat-scrtext_m    = 'Ticket External No'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1.

  gt_fieldcat-fieldname = 'TICKET_KEY'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  20.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Ticket Key'.
  gt_fieldcat-scrtext_m    = 'Ticket Key'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1.

  gt_fieldcat-fieldname = 'TICKET_ITEM'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  6.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Ticket Item No'.
  gt_fieldcat-scrtext_m    = 'Ticket Item No'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1.

  gt_fieldcat-fieldname = 'POST_STATUS'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  30.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Ticket Post Status'.
  gt_fieldcat-scrtext_m    = 'Ticket Post Status'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1.

  gt_fieldcat-fieldname = 'MESSAGE'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  132.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Processed Message'.
  gt_fieldcat-scrtext_m    = 'Processed Message'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1.

ENDFORM.
*EOC by MANMOHAN/SHREYOSI Charm:4000008514 date:24.07.2024

FORM checkbox_top_page USING l_selfield TYPE slis_selfield.

  IF l_selfield-value = '0'.

    CLEAR wa_oijnomi_display_final.
    READ TABLE it_oijnomi_display_final INTO wa_oijnomi_display_final INDEX l_selfield-tabindex.
    wa_oijnomi_display_final-flag = 'X'.
    MODIFY it_oijnomi_display_final FROM wa_oijnomi_display_final INDEX l_selfield-tabindex.

    LOOP AT it_oijnomi_display_final INTO wa_oijnomi_display_final WHERE idate = wa_oijnomi_display_final-idate AND flag NE 'X'.
      wa_oijnomi_display_final-flag = 'X'.
      PERFORM fill_celltab USING 'RO' 'FLAG' CHANGING lt_celltab.
      wa_oijnomi_display_final-celltab = lt_celltab.
      MODIFY it_oijnomi_display_final FROM wa_oijnomi_display_final INDEX sy-tabix.
    ENDLOOP.

  ELSE.

    CLEAR wa_oijnomi_display_final.
    READ TABLE it_oijnomi_display_final INTO wa_oijnomi_display_final INDEX l_selfield-tabindex.
    wa_oijnomi_display_final-flag = ''.
    MODIFY it_oijnomi_display_final FROM wa_oijnomi_display_final INDEX l_selfield-tabindex.

    LOOP AT it_oijnomi_display_final INTO wa_oijnomi_display_final WHERE idate = wa_oijnomi_display_final-idate AND flag EQ 'X'.
      wa_oijnomi_display_final-flag = ''.
      PERFORM fill_celltab USING 'RW' 'FLAG' CHANGING lt_celltab.
      wa_oijnomi_display_final-celltab = lt_celltab.
      MODIFY it_oijnomi_display_final FROM wa_oijnomi_display_final INDEX sy-tabix.

    ENDLOOP.

  ENDIF.

  DATA: l_date_h TYPE char10.
  REFRESH lt_top_of_page.
  CLEAR ls_line.

  ls_line-typ  = 'H'.
  CONCATENATE s_date-low+6(2) '.' s_date-low+4(2) '.' s_date-low+0(4) INTO l_date_l.
  CONCATENATE 'Purchase Side Allocation for' l_date_l INTO ls_line-info SEPARATED BY space.

  IF s_date-high IS NOT INITIAL.
    CONCATENATE s_date-high+6(2) '.' s_date-high+4(2) '.' s_date-high+0(4) INTO l_date_h.
    CONCATENATE ls_line-info 'to' l_date_h INTO ls_line-info SEPARATED BY space.
  ENDIF.

  APPEND ls_line TO lt_top_of_page.

  ls_line-typ  = 'A'.
  ls_line-info = 'For Selected Entries:'.
  APPEND ls_line TO lt_top_of_page.
  CLEAR:ls_line.

  ls_line-typ  = 'A'.
  CONCATENATE 'Total_Nominated_Quantity:' '28' INTO ls_line-info SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.
  CLEAR:ls_line.

  ls_line-typ  = 'A'.
  CONCATENATE 'Total_Allocated_Quantity:' '28' INTO ls_line-info SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.
  CLEAR:ls_line.

  ls_line-typ  = 'A'.
  CONCATENATE 'Total_Measurement_Quantity:' '28' INTO ls_line-info SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.
  CLEAR:ls_line.

  DATA : text2(40),
         text1(40).

  SET TITLEBAR 'TITLE1' OF PROGRAM 'YRXR036_PURC_NOM_G1' WITH text2.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_top_of_page.

ENDFORM.

FORM top_page.


  DATA: l_date_h      TYPE char10,
        temp_quantity TYPE char30.
  REFRESH lt_top_of_page.
  CLEAR ls_line.


  IF r_dis <> 'X'.
    ls_line-typ  = 'H'.
    CONCATENATE s_date-low+6(2) '.' s_date-low+4(2) '.' s_date-low+0(4) INTO l_date_l.
    CONCATENATE 'Purchase Side Allocation for' l_date_l INTO ls_line-info SEPARATED BY space.
  ELSE.
    ls_line-typ  = 'H'.
    CONCATENATE s_date-low+6(2) '.' s_date-low+4(2) '.' s_date-low+0(4) INTO l_date_l.
    CONCATENATE 'Purchase Allocation Display for' l_date_l INTO ls_line-info SEPARATED BY space.
  ENDIF.

  IF s_date-high IS NOT INITIAL.
    CONCATENATE s_date-high+6(2) '.' s_date-high+4(2) '.' s_date-high+0(4) INTO l_date_h.
    CONCATENATE ls_line-info 'to' l_date_h INTO ls_line-info SEPARATED BY space.
  ENDIF.

  APPEND ls_line TO lt_top_of_page.

  CLEAR:ls_line.
  ls_line-typ  = 'H'.
  APPEND ls_line TO lt_top_of_page.

  CLEAR:ls_line.
  ls_line-typ  = 'S'.
  ls_line-info = 'For Below Entries:'.
  APPEND ls_line TO lt_top_of_page.
  CLEAR:ls_line.

  ls_line-typ  = 'S'.
  CLEAR temp_quantity.
  temp_quantity = wa_total_nom.
  CONDENSE temp_quantity.
  CONCATENATE 'Total Nominated Quantity:' temp_quantity INTO ls_line-info SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.
  CLEAR:ls_line.

  ls_line-typ  = 'S'.
  CLEAR temp_quantity.
  temp_quantity = new_qty.
  CONDENSE temp_quantity.
  CONCATENATE 'Total Allocated Quantity:' temp_quantity INTO ls_line-info SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.
  CLEAR:ls_line.

  ls_line-typ  = 'S'.
  CLEAR temp_quantity.
  temp_quantity = tot_m_qty.
  CONDENSE temp_quantity.
  CONCATENATE 'Total Measurement Quantity:' temp_quantity INTO ls_line-info SEPARATED BY space.
  APPEND ls_line TO lt_top_of_page.
  CLEAR:ls_line.

  IF r_dis <> 'X'.
    ls_line-typ  = 'A'.
    ls_line-info = 'Note: On selecting one line item for a gas day,tickets will'.
    APPEND ls_line TO lt_top_of_page.
    CLEAR:ls_line.

    ls_line-typ  = 'A'.
    ls_line-info = 'get posted for all line items against the same gas day.'.
    APPEND ls_line TO lt_top_of_page.
    CLEAR:ls_line.
  ENDIF.


  DATA : text2(40),
         text1(40).

  SET TITLEBAR 'TITLE1' OF PROGRAM 'YRXR036_PURC_NOM_G1' WITH text2.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_top_of_page.

ENDFORM.

FORM fill_celltab USING VALUE(p_mode) e_fieldname TYPE lvc_fname
                  CHANGING pt_celltab TYPE lvc_t_styl.

  DATA: ls_celltab TYPE lvc_s_styl,
        l_mode     TYPE raw4.
  CLEAR pt_celltab.

  IF p_mode EQ 'RW'.
    l_mode = cl_gui_alv_grid=>mc_style_enabled.
  ELSE.
    l_mode = cl_gui_alv_grid=>mc_style_disabled.
  ENDIF.

  ls_celltab-fieldname = e_fieldname.
  ls_celltab-style = l_mode.
  INSERT ls_celltab INTO TABLE pt_celltab.

ENDFORM.


*&SPWIZARD: DECLARATION OF TABLECONTROL 'CONTROL1' ITSELF
CONTROLS: control1 TYPE TABLEVIEW USING SCREEN 9000.

*&SPWizard: Include inserted by SP Wizard. DO NOT CHANGE THIS LINE!
INCLUDE yrxr036_purc_nom_g1_pbo .
*&---------------------------------------------------------------------*
*&      Form  CORRECT_NOMI
*&---------------------------------------------------------------------*
FORM correct_nomi .

  DATA:lt_oijnomh     TYPE TABLE OF oijnomh,
       lt_oijnomi     TYPE TABLE OF oijnomi,
       ls_nom_header  TYPE LINE OF roijnomhio_t,
       ls_oijnomi_key TYPE roijnomi_key,
       ls_oijnomh     TYPE oijnomh,
       ls_oijnomi     TYPE oijnomi,
       lt_ret         TYPE bapiret2_t,
       lt_nom_header  TYPE roijnomhio_t,
       ls_nom_item    LIKE  roijnomiio,
       ls_nom_item1   LIKE  roijnomiio,
       lt_nom_item    LIKE TABLE OF ls_nom_item,
       lt_nom_item1   LIKE TABLE OF ls_nom_item,
       ls_nom_old     LIKE  roijnomiio,
       lt_nom_old     LIKE TABLE OF ls_nom_item,
       lt_nom_key     TYPE TABLE OF roijnomh_key,
       wa_ret         TYPE bapiret2,
       l_bstnk        TYPE bstnk,
       wa_nomh        TYPE roijnomh_key,
       l_tabix        TYPE sy-tabix.




  LOOP AT it_oijnomi_display_final INTO wa_oijnomi_display_final WHERE flag = 'X'.
    REFRESH : lt_oijnomh,lt_oijnomi,lt_nom_item,lt_nom_old.
    CLEAR: ls_nom_header,ls_oijnomi_key,ls_oijnomh,ls_oijnomi,lt_ret,lt_nom_header,ls_nom_item,ls_nom_item1,ls_nom_old,wa_ret,l_bstnk.

    READ TABLE it_oijnomi_post INTO wa_oijnomi_temp WITH KEY nomtk = wa_oijnomi_display_final-nomtk nomit = wa_oijnomi_display_final-nomit.
    IF sy-subrc <> 0.
      REFRESH lt_nom_key.
      wa_nomh-nomtk = wa_oijnomi_display_final-nomtk.
      APPEND wa_nomh TO lt_nom_key.

      CALL FUNCTION 'OIJNOMH_ARRAY_READ'
        EXPORTING
          it_oijnomh_key        = lt_nom_key
        IMPORTING
          et_oijnomh            = lt_oijnomh
        EXCEPTIONS
          records_not_found     = 1
          records_not_requested = 2
          OTHERS                = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CALL FUNCTION 'OIJNOMI_ARRAY_READ'
        EXPORTING
          it_oijnomh_key        = lt_nom_key
        IMPORTING
          et_oijnomi            = lt_oijnomi
        EXCEPTIONS
          records_not_found     = 1
          records_not_requested = 2
          OTHERS                = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      LOOP AT lt_oijnomh INTO ls_oijnomh.
        MOVE-CORRESPONDING ls_oijnomh TO ls_nom_header.
        APPEND ls_nom_header TO lt_nom_header.
      ENDLOOP.

      LOOP AT lt_oijnomi INTO ls_oijnomi WHERE nomit = wa_oijnomi_display_final-nomit..
        MOVE-CORRESPONDING ls_oijnomi TO ls_nom_item.
        APPEND ls_nom_item TO lt_nom_item.
      ENDLOOP.

      CALL FUNCTION 'OIJ_NOM_DEFAULTING'
        EXPORTING
          it_nom_header     = lt_nom_header
          it_nom_item       = lt_nom_item
        IMPORTING
          et_nom_header     = lt_nom_header
          et_nom_item       = lt_nom_item
        EXCEPTIONS
          defaulting_error  = 1
          no_records_passed = 2
          OTHERS            = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      REFRESH   lt_nom_item1.
      MOVE lt_nom_item[] TO lt_nom_item1[].

      CLEAR l_tabix.
      LOOP AT lt_nom_item INTO ls_nom_item.
        l_tabix = sy-tabix.
        ls_nom_item-ga_allocated_qty = wa_oijnomi_display_final-yyoij_dpimb_qty.
        ls_nom_item-updkz = 'U'.
        MODIFY lt_nom_item FROM ls_nom_item INDEX l_tabix.
      ENDLOOP.

      CALL FUNCTION 'OIJ_NOM_MAINTAIN_MULTIPLE'
        EXPORTING
          it_nom_header     = lt_nom_header
          it_nom_item       = lt_nom_item
          iv_internal       = 'X'
          iv_lock           = ' '
        IMPORTING
          et_nom_header     = lt_nom_header
          et_nom_item       = lt_nom_item
          et_return         = lt_ret
        EXCEPTIONS
          nomination_locked = 1
          error_phase_01    = 2
          error_phase_02    = 3
          OTHERS            = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.
  ENDLOOP.

  REFRESH: i_main ,it_yrxa_cmdata.
  SELECT * INTO TABLE it_yrxa_cmdata
    FROM yrxa_cmdata
    FOR ALL ENTRIES IN it_oijnomi
    WHERE
    yybus_location = it_oijnomi-locid
    AND
    yydate = it_oijnomi-idate.
  IF sy-subrc = 0.
    SORT it_yrxa_cmdata BY yybus_location yydate yytimestamp DESCENDING.
    DELETE ADJACENT DUPLICATES FROM it_yrxa_cmdata COMPARING yybus_location yydate .
  ENDIF.

  LOOP AT it_oijnomi_display_final INTO wa_oijnomi_display_final WHERE flag = 'X'.
    READ TABLE it_oijnomi_post INTO wa_oijnomi_temp WITH KEY nomtk = wa_oijnomi_display_final-nomtk nomit = wa_oijnomi_display_final-nomit.
    IF sy-subrc <> 0.
      CLEAR wa_yrxa_cmdata .
      wa_main-date    = wa_oijnomi_display_final-idate.
      wa_main-locid = wa_oijnomi_display_final-locid.
      wa_main-matnr = wa_oijnomi_display_final-matnr_i.
      wa_main-menge   = wa_oijnomi_display_final-menge.
      wa_main-unit    = wa_oijnomi_display_final-unit_i.
      wa_main-rank = wa_oijnomi_display_final-ga_rank.
      READ TABLE it_yrxa_cmdata INTO wa_yrxa_cmdata WITH KEY yybus_location = wa_main-locid yydate = wa_main-date.
      IF p_gcv IS INITIAL.
        wa_main-agcv = wa_yrxa_cmdata-yyavg_gcv.
      ELSE.
        wa_main-agcv = p_gcv.
      ENDIF.
      IF p_ncv IS INITIAL.
        wa_main-ancv = wa_yrxa_cmdata-yyavg_ncv.
      ELSE.
        wa_main-ancv = p_ncv.

      ENDIF.
      wa_main-nomtk  = wa_oijnomi_display_final-nomtk.
      wa_main-nomit  = wa_oijnomi_display_final-nomit.
      wa_main-st_qty = wa_oijnomi_display_final-yyoij_dpimb_qty.
      APPEND wa_main TO i_main.
    ENDIF.

    CLEAR: wa_oijnomi_display_final.
  ENDLOOP.


  LOOP AT i_main INTO wa_main WHERE nomtk IS NOT INITIAL.
    IF wa_main-st_qty IS NOT INITIAL.
      PERFORM quick_confirm.
      PERFORM create_ticket.
      MODIFY i_main FROM wa_main.
    ENDIF.

    IF wa_main-post_status IS INITIAL.
      wa_main-post_status = ''.
      wa_main-ticketnr = ''.
      wa_main-ticket_key = ''.
      wa_main-ticket_item =  ''.
      MODIFY i_main FROM wa_main.
    ENDIF.
    CLEAR wa_main.
  ENDLOOP.

  PERFORM display_alv_post.

  REFRESH i_main.
ENDFORM.


FORM display_alv_post.

  REFRESH : gt_fieldcat[].
  CLEAR i_layout.
  DATA: w_sr TYPE i.
  w_sr = 1 .


  gt_fieldcat-fieldname = 'DATE'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  10.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Gas Day'.
  gt_fieldcat-scrtext_m    = 'Gas Day'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1  .

  gt_fieldcat-fieldname = 'NOMTK'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  20.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Nomination Number'.
  gt_fieldcat-scrtext_m    = 'Nomination Number'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1.

  gt_fieldcat-fieldname = 'NOMIT'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  10.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Nomination'.
  gt_fieldcat-scrtext_m    = 'Nomination'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1.

  gt_fieldcat-fieldname = 'ST_QTY'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  10.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Allocated Quantity'.
  gt_fieldcat-scrtext_m    = 'Allocated Quantity'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1.

  gt_fieldcat-fieldname = 'TICKETNR'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  20.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Ticket External No'.
  gt_fieldcat-scrtext_m    = 'Ticket External No'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1.

  gt_fieldcat-fieldname = 'TICKET_KEY'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  20.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Ticket Key'.
  gt_fieldcat-scrtext_m    = 'Ticket Key'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1.

  gt_fieldcat-fieldname = 'TICKET_ITEM'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  6.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Ticket Item No'.
  gt_fieldcat-scrtext_m    = 'Ticket Item No'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1.

  gt_fieldcat-fieldname = 'POST_STATUS'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  30.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Ticket Post Status'.
  gt_fieldcat-scrtext_m    = 'Ticket Post Status'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1.

  gt_fieldcat-fieldname = 'ERROR_MSG'.
  gt_fieldcat-key       = 'X' .
  gt_fieldcat-outputlen    =  170.
  gt_fieldcat-just         =  'L' .
  gt_fieldcat-col_pos      =  w_sr.
  gt_fieldcat-scrtext_s    = 'Error Message'.
  gt_fieldcat-scrtext_m    = 'Error Message'.
  gt_fieldcat-selddictxt      = 'M'.
  gt_fieldcat-do_sum       = space.
  APPEND gt_fieldcat.
  CLEAR gt_fieldcat.
  w_sr = w_sr  + 1.


  DATA: v_repid LIKE sy-repid.

  v_repid = sy-repid .
  i_layout-zebra   = 'X' .
  i_layout-cwidth_opt = 'X' .
  i_layout-info_fname = 'COLOR'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = v_repid
      is_layout_lvc      = i_layout
      it_fieldcat_lvc    = gt_fieldcat[]
    TABLES
      t_outtab           = i_main
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  QTY_CHECK
*&---------------------------------------------------------------------*
FORM qty_check .

  DATA: misqty1           TYPE p DECIMALS 3 VALUE'0.000',
        misqty2           TYPE p DECIMALS 3 VALUE'0.060',
        gv_msg4           TYPE string,
        wa_oijnomi_modify TYPE ty_display_final,
        it_oijnomi_modify TYPE STANDARD TABLE OF ty_display_final WITH HEADER LINE,
        error_msg         TYPE char255,
        error_msg1        TYPE char255,
        error_msg2        TYPE char255,
        quantity_check    TYPE char1,
        allocation_check  TYPE char1,
        measurement_check TYPE char1,
        temp_date         TYPE char10.



  IF p_locid IS NOT INITIAL AND s_date-low IS NOT INITIAL.

    SELECT yybus_location,
        yydate,
        yytimestamp,
        yycum_vol FROM yrxa_cmdata INTO TABLE @DATA(it_qchk)
                                        WHERE yybus_location = @p_locid
                                        AND   yydate IN @s_date.
    IF sy-subrc = 0.
      SORT  it_qchk BY yybus_location yydate yytimestamp DESCENDING.
      DELETE ADJACENT DUPLICATES FROM it_qchk COMPARING yybus_location yydate .
    ENDIF.

    it_oijnomi_modify[] = it_oijnomi_display_final[].
    DELETE ADJACENT DUPLICATES FROM it_oijnomi_modify COMPARING idate.
    SORT it_oijnomi_modify BY idate ASCENDING.

    LOOP AT it_oijnomi_modify INTO wa_oijnomi_modify.
      READ TABLE it_oijnomi_display_final WITH KEY idate = wa_oijnomi_modify-idate flag = 'X' TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        LOOP AT it_oijnomi_display_final INTO wa_oijnomi_display_final WHERE idate = wa_oijnomi_modify-idate AND flag NE 'X'.
          wa_oijnomi_display_final-flag = 'X'.
          MODIFY it_oijnomi_display_final FROM wa_oijnomi_display_final TRANSPORTING flag.
          CLEAR wa_oijnomi_display_final.
        ENDLOOP.
        wa_oijnomi_modify-flag = 'X'.
        MODIFY it_oijnomi_modify FROM wa_oijnomi_modify.
      ELSE.
        DELETE it_qchk WHERE yydate = wa_oijnomi_modify-idate.
      ENDIF.
      CLEAR wa_oijnomi_modify.
    ENDLOOP.

    IF it_qchk IS INITIAL.
      MESSAGE 'No measurement data available for all the dates' TYPE 'E'.
    ENDIF.

    DATA: fqty TYPE p DECIMALS 3.

    DELETE it_oijnomi_modify WHERE flag NE 'X'.

    CLEAR: quantity_check,measurement_check.
    error_msg = 'Total allocation quantity is not equal to the cumulative measurement for'.
    error_msg2 = 'No measurement data available for'.

    DATA: lv_index   TYPE numc3,
          line_count TYPE numc3.

    LOOP AT it_oijnomi_modify INTO wa_oijnomi_modify.
      lv_index = sy-tabix.
      CLEAR: fqty.
      LOOP AT it_qchk INTO DATA(wchk) WHERE yydate = wa_oijnomi_modify-idate.
        fqty = fqty + wchk-yycum_vol.
      ENDLOOP.

      IF fqty EQ 0.
        CLEAR temp_date.
        CONCATENATE wa_oijnomi_modify-idate+6(2) '.' wa_oijnomi_modify-idate+4(2) '.' wa_oijnomi_modify-idate+0(4) INTO temp_date.
        CONCATENATE error_msg2 temp_date INTO error_msg2 SEPARATED BY space.
        CONCATENATE error_msg2 ',' INTO error_msg2.
        measurement_check = 'X'.
        DELETE it_oijnomi_display_final WHERE idate = wa_oijnomi_modify-idate.
        DELETE it_qchk WHERE yydate = wa_oijnomi_modify-idate.

        IF it_qchk[] IS INITIAL.
          MESSAGE 'No measurement data available for all the dates' TYPE 'E'.
        ENDIF.

        CONTINUE.
      ENDIF.

      CLEAR new_qty.
      LOOP AT it_oijnomi_display_final INTO wa_oijnomi_display_final WHERE idate = wa_oijnomi_modify-idate.
        new_qty = new_qty + wa_oijnomi_display_final-ga_allocated_qty.
      ENDLOOP.

      IF new_qty NE fqty.
        CLEAR temp_date.
        CONCATENATE wa_oijnomi_modify-idate+6(2) '.' wa_oijnomi_modify-idate+4(2) '.' wa_oijnomi_modify-idate+0(4) INTO temp_date.
        CONCATENATE error_msg temp_date INTO error_msg SEPARATED BY space.
        CONCATENATE error_msg ',' INTO error_msg.
        quantity_check = 'X'.
        DELETE it_oijnomi_display_final WHERE idate = wa_oijnomi_modify-idate.
      ENDIF.

      IF it_oijnomi_display_final[] IS INITIAL.
        MESSAGE 'Total allocation quantity is not equal to the cumulative measurement for all the dates' TYPE 'E'.
      ENDIF.

    ENDLOOP.

    IF measurement_check = 'X'.
      MESSAGE: error_msg2 TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.


    IF quantity_check = 'X'.
      MESSAGE: error_msg TYPE 'I' DISPLAY LIKE 'E'.
    ENDIF.

  ENDIF.

  CLEAR: allocation_check.
  error_msg1 = 'Allocation against some PO/Agreements is less than 0.060 for'.

  CLEAR: line_count,lv_index.
  DESCRIBE TABLE it_oijnomi_modify LINES line_count.

  LOOP AT it_oijnomi_display_final INTO DATA(woi) WHERE flag = 'X'.
    lv_index = sy-tabix.
    IF  woi-yyoij_dpimb_qty < misqty2 AND woi-yyoij_dpimb_qty > misqty1.
      CLEAR temp_date.
      CONCATENATE woi-idate+6(2) '.' woi-idate+4(2) '.' woi-idate+0(4) INTO temp_date.
      CONCATENATE error_msg1 temp_date INTO error_msg1 SEPARATED BY space.
      CONCATENATE error_msg1 ',' INTO error_msg1.
      allocation_check = 'X'.
      DELETE it_oijnomi_display_final WHERE idate = woi-idate.
    ENDIF.
    CLEAR: woi.
  ENDLOOP.

  IF it_oijnomi_display_final[] IS INITIAL.
    MESSAGE 'Allocation against some PO/Agreements is less than 0.060 for all the dates' TYPE 'E'.
  ENDIF.

*&------>SOC CHARM ID :4000008742  |TECHICAL : RAVINDER SINGH| FUNCTIONAL : SHREYOSHI |DT:25.10.2024
  SELECT ebeln,loekz FROM ekpo INTO TABLE @DATA(lt_ekpo) FOR ALL ENTRIES IN @it_oijnomi_display_final
    WHERE ebeln = @it_oijnomi_display_final-docnr.

  LOOP AT lt_ekpo INTO DATA(ls_ekpo).
    IF ls_ekpo-loekz NE ''.
      DELETE it_oijnomi_display_final WHERE docnr = ls_ekpo-ebeln AND flag = 'X'.
    ENDIF.
    CLEAR ls_ekpo.
  ENDLOOP.

  IF it_oijnomi_display_final[] IS INITIAL.
    MESSAGE 'Allocation complete. As POs/ Outline Agreements are locked, ticket posting is not required.' TYPE 'E'.
  ENDIF.

*&------>EOC CHARM ID :4000008742  |TECHICAL : RAVINDER SINGH| FUNCTIONAL : SHREYOSHI |DT:25.10.2024

  IF allocation_check = 'X'.
    MESSAGE: error_msg1 TYPE 'I' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  QTY_ADD
*&---------------------------------------------------------------------*
FORM qty_add .


  CLEAR: new_qty.
  LOOP AT it_oijnomi_display_final INTO DATA(wqty).

    new_qty =  new_qty + wqty-yyoij_dpimb_qty.

    CLEAR:wqty.

  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_9000
*&---------------------------------------------------------------------*
FORM display_9000 .

  gs_refres_val = VALUE #( col = abap_true row = abap_true ).
  PERFORM create_act_fieldcat.

  DATA(ls_variant) = VALUE disvariant( report = sy-repid
                                         username = sy-uname ).

  DATA(ls_layout) = VALUE lvc_s_layo( stylefname = 'CELLTAB'
                                      no_rowins = abap_true
                                      cwidth_opt = abap_true ).

  IF gr_docking_container IS NOT BOUND.

    CREATE OBJECT gr_docking_container
      EXPORTING
        repid = sy-repid
        dynnr = '9000'
        side  = gr_docking_container->dock_at_top
        ratio = 10.

    IF sy-subrc <> 0.
    ENDIF.

    CREATE OBJECT gr_document.

    gv_header_text = 'Note: On Selecting one line item for a gas day, tickets will get posted for all line items against the same gas day'.

    CALL METHOD gr_document->add_text
      EXPORTING
        text         = gv_header_text
        sap_fontsize = cl_dd_document=>medium
        sap_emphasis = cl_dd_document=>emphasis.

    CALL METHOD gr_document->display_document
      EXPORTING
        parent = gr_docking_container.

  ENDIF.

  IF go_cust_cont IS NOT BOUND.

    CREATE OBJECT go_cust_cont
      EXPORTING
        container_name = 'CUST_CONT'.

    CREATE OBJECT go_alv_grid
      EXPORTING
        i_parent = go_cust_cont.

    CALL METHOD go_alv_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    LOOP AT it_oijnomi_display_final INTO DATA(ls_final).
      wa_oijnomi_display_final = CORRESPONDING #( ls_final ).
      APPEND wa_oijnomi_display_final TO it_oijnomi_display_final1.
      CLEAR  wa_oijnomi_display_final.
    ENDLOOP.

    LOOP AT gt_fieldcat INTO DATA(ls_fieldcat).
      wa_fieldcat1 = CORRESPONDING #( ls_fieldcat ).
      APPEND wa_fieldcat1 TO gt_fieldcat1.
      CLEAR wa_fieldcat1.
    ENDLOOP.

    SORT it_oijnomi_display_final1 BY idate locid nomtk nomit.

    CALL METHOD go_alv_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = ls_layout
        is_variant                    = ls_variant
        i_save                        = 'A'
      CHANGING
        it_outtab                     = it_oijnomi_display_final1
        it_fieldcatalog               = gt_fieldcat1
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD go_alv_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    go_alv_grid->set_frontend_fieldcatalog( it_fieldcatalog = gt_fieldcat1 ).

    go_alv_grid->check_changed_data( ).
    go_alv_grid->refresh_table_display( is_stable = gs_refres_val ).

  ENDIF.


ENDFORM.
