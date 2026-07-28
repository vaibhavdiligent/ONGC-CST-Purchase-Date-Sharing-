*&---------------------------------------------------------------------*
*& Report  ZFI_BNK_APP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zfi_bnk_app.

* Top Include
INCLUDE zfi_bnk_app_top.
* Selection Screen
INCLUDE zfi_bnk_app_s01.
* Include for sub routines
INCLUDE zfi_bnk_app_f.
** Include for output
INCLUDE zfi_bnk_app_outputo01.
** Include for input
INCLUDE zfi_bnk_app_i01.

START-OF-SELECTION.
  CALL SCREEN 100.
