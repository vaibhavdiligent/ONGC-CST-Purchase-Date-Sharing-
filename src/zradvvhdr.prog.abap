*&---------------------------------------------------------------------*
*& Include ZRADVVHDR
*&---------------------------------------------------------------------*
*& Z Copy of Standard Include RADVVHDR
*& Parameter definitions for all version comparison reports
*&---------------------------------------------------------------------*

PARAMETERS: objname  LIKE vrsd-objname DEFAULT SPACE,
            objnam2  LIKE vrsd-objname DEFAULT SPACE,
            versno1  LIKE vrsd-versno  DEFAULT '00000',
            versno2  LIKE vrsd-versno  DEFAULT '00000',
            infoln1a LIKE vrsinfolna,  "Infozeile 1. Teil zu VERSNO1
            infoln1b LIKE vrsinfolnb,  "Infozeile 2. Teil
            infoln2a LIKE vrsinfolna,  "Infozeile 1. Teil zu VERSNO2
            infoln2b LIKE vrsinfolnb.  "Infozeile 2. Teil

* Neue Parameter für Remote-Vergleich:
PARAMETERS: log_dest LIKE rfcdes-rfcdest DEFAULT ' ', " log. Destinat.
            rem_syst LIKE tadir-srcsystem DEFAULT ' '. " REMOTE-System

TYPE-POOLS: seuvs.

DATA: compare.
* Erneute Anzeige bei Funktion "tech. Darstellung"
DATA: repeat_display.
DATA: block_info TYPE seuvs_block_info.

* Makro, der zu eine Struktur X die Tabellen X_OBJ1, X_OBJ2, und X_LIST
* definiert.
INCLUDE zlsveumac.

* Beim Lesen von Versionen enthalten die Headertabellen (DDxxV_TAB)
* die Sprache, in der Versioniert wurde. Für eine einfache Handhabung
* der Versionsanzeige, sollte aber die Anmeldesprache sofort in den
* Headertabellen zur Verfügung stehen. Das leistet der folgende
* Makro.
* Wenn in DDxxV_TAB nicht die Sprache SY-LANGU vorgefunden wird,
* dann wird der Text (bzw. die Texte) aus der DDxxTV_TAB geholt.
**********************************************************************
* Anwendung:
* FILL_SY_LANGU_TEXT 08 TABNAME FIELDNAME.
* Damit werden die Texte der DD08V_TAB aus der Tabelle DD08TV_TAB
* gefüllt. Die DD08V_TAB hat die Schlüsselfelder TABNAME und FIELDNAME.
* Bei Tabellen mit nur einem Schlüssel, muß zweimal der gleiche
* Feldname angegeben werden.
**********************************************************************

DATA: g_save_sy_subrc LIKE sy-subrc.

DEFINE fill_sy_langu_text1.
  "Mit einem Sortierfeld
  g_save_sy_subrc = sy-subrc.
  LOOP AT dd&1v_tab WHERE ddlanguage <> sy-langu.
    READ TABLE dd&1tv_tab WITH KEY
      &2 = dd&1v_tab-&2
      ddlanguage = sy-langu.
    CHECK sy-subrc = 0.
    MOVE-CORRESPONDING dd&1tv_tab TO dd&1v_tab.
    MODIFY dd&1v_tab.
  ENDLOOP.
  sy-subrc = g_save_sy_subrc.
END-OF-DEFINITION.

DEFINE fill_sy_langu_text2.
  "Mit zwei Sortierfeldern
  g_save_sy_subrc = sy-subrc.
  LOOP AT dd&1v_tab WHERE ddlanguage <> sy-langu.
    READ TABLE dd&1tv_tab WITH KEY
      &2 = dd&1v_tab-&2
      &3 = dd&1v_tab-&3
      ddlanguage = sy-langu.
    CHECK sy-subrc = 0.
    MOVE-CORRESPONDING dd&1tv_tab TO dd&1v_tab.
    MODIFY dd&1v_tab.
  ENDLOOP.
  sy-subrc = g_save_sy_subrc.
END-OF-DEFINITION.
