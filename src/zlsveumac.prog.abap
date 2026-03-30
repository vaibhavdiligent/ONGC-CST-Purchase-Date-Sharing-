*&---------------------------------------------------------------------*
*& Include ZLSVEUMAC
*&---------------------------------------------------------------------*
*& Z Copy of Standard Include LSVEUMAC
*& MACRO: DEFINE_VERSTABLES
*& Makro, der zu eine Struktur X die Tabellen X_OBJ1, X_OBJ2, und X_LIST
*& definiert.
*&---------------------------------------------------------------------*

DEFINE DEFINE_VERSTABLES.
  DATA: &1_OBJ1 LIKE TABLE OF &1 WITH HEADER LINE.
  DATA: &1_OBJ2 LIKE TABLE OF &1 WITH HEADER LINE.
  DATA: BEGIN OF &1_LIST OCCURS 0,
          DATA LIKE &1,
          DAT2 LIKE &1,
          ATTR TYPE SEUVS_ATTR,
        END OF &1_LIST.
END-OF-DEFINITION.
* Achtung: Da manche Aufrufer den Bezug auf intern definierte Variablen
* benutzen, muß LIKE stehen bleiben.
