*&---------------------------------------------------------------------*
*& Report ZRADVVDO2
*&---------------------------------------------------------------------*
*& Z Copy of Standard Program RADVVDO2
*& Title: Version Display/Comparison of Domains
*& Original Author: SAP
*& Development Class: SDBT
*&---------------------------------------------------------------------*
REPORT zradvvdo2
  MESSAGE-ID da
  NO STANDARD PAGE HEADING.

***********************************************************************
*       V E R S I O N S V E R G L E I C H E R
*       für Domänen
***********************************************************************

INCLUDE zradvvhdr.
"Parameter-Definition für alle Vergleichsreports

* Makro, der zu eine Struktur X die Tabellen X_OBJ1, X_OBJ2, X_LIST
* X_BLOCK definiert.
DEFINE_VERSTABLES: DD01VD, DD07VD.
DEFINE_VERSTABLES: RSVRSHEAD.

*
DATA rc LIKE sy-subrc.
DATA tech_fields TYPE seuvs_t_fieldnames.  " technical fields
" Technische Felder gehen weder in Vergleich noch in Anzeige ein
DATA key_fields TYPE seuvs_t_fieldnames.   " key fields
DATA cb_fields TYPE seuvs_t_fieldnames.    " checkbox fields
DATA opt_fields TYPE seuvs_t_fieldnames.   " optional fields (displayed
                                           " via variant)

START-OF-SELECTION.

* Authority-Check auf Anzeigeberechtigung
  PERFORM auth_check(radvvutl) USING 'DOMA' objname objnam2.

* Die nicht versorgten Parameter füllen.
  PERFORM fill_emtpy_par(radvvutl)
    CHANGING objname objnam2
             versno1 versno2
             log_dest compare.

* Zuerst die beiden Objekte lesen.
  PERFORM obj_read
    TABLES dd01vd_obj1 dd07vd_obj1
    USING objname versno1 SPACE CHANGING rc.

  IF compare = 'X'.
    PERFORM obj_read
      TABLES dd01vd_obj2 dd07vd_obj2
      USING objnam2 versno2 log_dest CHANGING rc.
  ELSE.
    dd01vd_obj2[] = dd01vd_obj1[].
    dd07vd_obj2[] = dd07vd_obj1[].
  ENDIF.

  DO.
    CALL FUNCTION 'RS_VERS_DISPLAY_INIT'
      EXCEPTIONS
        OTHERS = 0.

    CALL FUNCTION 'RS_VERS_TITEL_DEF'
      EXPORTING
        objname1 = objname
        objname2 = objnam2
        versno1  = versno1
        versno2  = versno2
        infoln1b = infoln1b
        infoln2b = infoln2b
        log_dest = log_dest
        rem_syst = rem_syst
      EXCEPTIONS
        OTHERS   = 0.

***********************************************************************
*   Header
***********************************************************************
    CLEAR: block_info.
    REFRESH: key_fields, tech_fields, cb_fields, opt_fields.

    " technical fields
    SPLIT 'VALEXI' AT ' ' INTO TABLE tech_fields.
    " OUTPUTSTYLETXT is displayed instead of OUTPUTSTYLE:
    APPEND 'OUTPUTSTYLE' TO tech_fields.

    " checkbox fields
    SPLIT 'LOWERCASE SIGNFLAG PROXYTYPE AMPMFORMAT' AT ' '
      INTO TABLE cb_fields.

    block_info-typ = 'A'.
    block_info-title = 'Allgemeine Eigenschaften'(s01).
    block_info-tabname = 'DD01VD'.

    CALL FUNCTION 'RS_VERS_BLOCK_DEF'
      EXPORTING
        block_info       = block_info
      TABLES
        obj1_tab         = dd01vd_obj1
        obj2_tab         = dd01vd_obj2
        display_tab      = dd01vd_list
        key_fields       = key_fields
        tech_fields      = tech_fields
        checkbox_fields  = cb_fields
        opt_fields       = opt_fields
      EXCEPTIONS
        OTHERS           = 0.

***********************************************************************
*   Festwerte
***********************************************************************
    READ TABLE dd01vd_obj1 INDEX 1.
    READ TABLE dd01vd_obj2 INDEX 1.

    IF dd01vd_obj1-valexi <> space OR dd01vd_obj2-valexi <> space.

      CLEAR block_info.
      REFRESH: key_fields, tech_fields, cb_fields, opt_fields.

      block_info-typ = 'T'.
      block_info-title = 'Festwerte:'(s02).
      block_info-tabname = 'DD07VD'.

      CALL FUNCTION 'RS_VERS_BLOCK_DEF'
        EXPORTING
          block_info       = block_info
        TABLES
          obj1_tab         = dd07vd_obj1
          obj2_tab         = dd07vd_obj2
          display_tab      = dd07vd_list
          key_fields       = key_fields
          tech_fields      = tech_fields
          checkbox_fields  = cb_fields
          opt_fields       = opt_fields
        EXCEPTIONS
          OTHERS           = 0.

    ENDIF.

    CALL FUNCTION 'RS_VERS_DISPLAY_PROCESS'
      EXPORTING
        print   = ' '
        compare = compare
      IMPORTING
        repeat  = repeat_display
      EXCEPTIONS
        OTHERS  = 0.

    IF repeat_display = space.
      EXIT.
    ENDIF.
  ENDDO.

*----------------------------------------------------------------------*
*  FORM OBJ_READ
*----------------------------------------------------------------------*
*  Read domain version data
*----------------------------------------------------------------------*
*  --> DD01VD_TAB  Domain header data
*  --> DD07VD_TAB  Domain fixed values data
*  --> OBJNAME     Object name
*  --> VERSNO      Version number
*  --> RC          Return code
*----------------------------------------------------------------------*
FORM obj_read
  TABLES dd01vd_tab STRUCTURE dd01vd
         dd07vd_tab STRUCTURE dd07vd
  USING  objname versno log_dest
  CHANGING rc.

  DATA: dd07v_tab  LIKE dd07v  OCCURS 0 WITH HEADER LINE.
  DATA: dd01v_tab  LIKE dd01v  OCCURS 0 WITH HEADER LINE.
  DATA: dd07tv_tab LIKE dd07tv OCCURS 0 WITH HEADER LINE.
  DATA: dd01tv_tab LIKE dd01tv OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'SVRS_GET_VERSION_DOMD_40'
    EXPORTING
      destination  = log_dest
      object_name  = objname
      versno       = versno
    TABLES
      dd01v_tab    = dd01v_tab
      dd07v_tab    = dd07v_tab
      dd01tv_tab   = dd01tv_tab
      dd07tv_tab   = dd07tv_tab
    EXCEPTIONS
      no_version   = 01.

  IF sy-subrc <> 0.
* MSG DA401: Version & von & nicht gefunden
    MESSAGE e401 WITH versno objname.
  ENDIF.

  fill_sy_langu_text1 01 domname.
  fill_sy_langu_text2 07 domname domvalue_l.

  READ TABLE dd01v_tab INDEX 1.
  IF dd01v_tab-valexi = space.
    REFRESH dd07v_tab.
  ENDIF.

  LOOP AT dd01v_tab.
    CLEAR dd01vd_tab.
    MOVE-CORRESPONDING dd01v_tab TO dd01vd_tab.
    " convert OUTPUTSTYLE into text for new DEC-types
    IF dd01v_tab-datatype(3) = 'D34'
    OR dd01v_tab-datatype(3) = 'D16'.
      PERFORM get_fv_text(radvvutl)
        USING
          'OUTPUTSTYLE' dd01vd_tab-outputstyle
        CHANGING dd01vd_tab-outputstyletxt rc.
    ENDIF.
    APPEND dd01vd_tab.
  ENDLOOP.

  LOOP AT dd07v_tab.
    MOVE-CORRESPONDING dd07v_tab TO dd07vd_tab.
    APPEND dd07vd_tab.
  ENDLOOP.

ENDFORM.                    "OBJ_READ
