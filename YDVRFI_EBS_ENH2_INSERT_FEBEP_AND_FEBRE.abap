*&=====================================================================*
*&  ENHANCEMENT SPOT : YDVRFI_EBS   (ENHANCEMENT 2)
*&  PROGRAM          : RFEKA400
*&  FORM             : INSERT_FEBEP_AND_FEBRE   (spot (1))
*&  PURPOSE          : Export the whole XFEBEP table to ABAP memory
*&                     'YRFEKA400_XFEBEP[]' so ZXF01U01 can read the
*&                     NSTO / ZUONR values later.
*&=====================================================================*


*&---------------------------------------------------------------------*
*&  ORIGINAL CODE (as currently active in the system)
*&---------------------------------------------------------------------*
ENHANCEMENT 2  YDVRFI_EBS.    "active version

*******************************************************************************
* Developed By      :   Man Mohan Acharya
* Suggested By      :   Pankaj Gupta
* Date              :   26.04.2019 17:04:14
* Purpose           :   To Pass The XFEBEP Table Data To memory which are updated in Database and later
*                       ZUONR is modified Based ON NSTO of TEXTS field as chect field length is less than Zuonr
*****************************************************************************
DATA(lt_xfebep) = xfebep[].
** -> Begin of changes by  KRITIKA on 24.10.2024 for ATC
EXPORT lt_xfebep TO MEMORY ID 'YRFEKA400_XFEBEP[]'. "#EC CI_FLDEXT_OK[2610650]
** <- End of changes by KRITIKA on 24.10.2024 for ATC
****End Of Development on 26.04.2019 17:04:14********************
ENDENHANCEMENT.


*&---------------------------------------------------------------------*
*&  MODIFIED CODE
*&---------------------------------------------------------------------*
*&  NO CHANGE REQUIRED.
*&
*&  This spot is a straight EXPORT of the current statement's XFEBEP to
*&  memory. It does not contain any per-line-item gate/CHECK/EXIT, so it
*&  is not a source of the "logic skipped" behaviour.
*&
*&  Note (context only, not a defect here): the consumer of this memory
*&  ID in ZXF01U01 reads it with WITH KEY KUKEY/ESNUM - that read is only
*&  reliable if the exported XFEBEP still belongs to the statement being
*&  processed. That is guaranteed here because the export happens per
*&  statement inside INSERT_FEBEP_AND_FEBRE, so no fix is needed on this
*&  side.
*&---------------------------------------------------------------------*
