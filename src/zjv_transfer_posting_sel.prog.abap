*&---------------------------------------------------------------------*
*&  Include           ZJV_TRANSFER_POSTING_SEL
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Selection Screen Include
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: p_bukrs TYPE bukrs OBLIGATORY,
            p_gjahr TYPE gjahr OBLIGATORY,
            p_poper TYPE monat OBLIGATORY.

PARAMETERS: p_test AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK b1.
