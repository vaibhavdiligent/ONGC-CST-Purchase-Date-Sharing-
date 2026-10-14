*&---------------------------------------------------------------------*
*& Report  ZJV_OVC_CORP
*&---------------------------------------------------------------------*
*& Corporate - Colombia : entry point for transaction ZJV_OVC_CORP.
*&   1. Maintain mapping table            -> ZFI_COL_GLMAP (SM30 dialog)
*&   2. Execute Corporate Bank Statement  -> report ZFI_COL_OVL_POSTING
*&
*& Prerequisite for option 1 : table maintenance generated for
*& ZFI_COL_GLMAP (SE54). No text elements needed - all texts are set
*& at INITIALIZATION.
*&---------------------------------------------------------------------*
REPORT  zjv_ovc_corp.

CONSTANTS: gc_map_table TYPE tabname  VALUE 'ZFI_COL_GLMAP',
           gc_post_prog TYPE progname VALUE 'ZFI_COL_OVL_POSTING'.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE gv_tit.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS r_map RADIOBUTTON GROUP g1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT 4(60) gv_c1 FOR FIELD r_map.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS r_post RADIOBUTTON GROUP g1.
SELECTION-SCREEN COMMENT 4(60) gv_c2 FOR FIELD r_post.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  gv_tit = 'Corporate - Colombia (OVC -> OVL)'.
  gv_c1  = '1. Maintain Mapping Table'.
  gv_c2  = '2. Execute Corporate Bank Statement'.

START-OF-SELECTION.
  IF r_map = abap_true.
*   Table maintenance dialog (same as SM30 -> Maintain); authorization
*   (S_TABU_DIS / S_TABU_NAM) is checked by the standard function.
    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action    = 'U'
        view_name = gc_map_table
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      MESSAGE |Maintenance of { gc_map_table } not possible | &&
              |(no maintenance dialog, no authorization or locked)|
              TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ELSE.
*   Posting program with its own selection screen
    SUBMIT (gc_post_prog) VIA SELECTION-SCREEN AND RETURN.
  ENDIF.
