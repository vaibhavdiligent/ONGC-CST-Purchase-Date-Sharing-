*&---------------------------------------------------------------------*
*& Interface: YGMS_IF_CST_PROCESSOR
*& Package: YGMS
*& Description: Processing Interface for ONGC CST Purchase Data Sharing
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

INTERFACE ygms_if_cst_processor PUBLIC.

  "! <p class="shorttext synchronized" lang="en">Process data based on mode</p>
  "! @parameter iv_mode | Processing mode (U=Upload, A=Allocate, V=View, S=Send, D=Download, N=Nomination)
  "! @parameter et_messages | Return messages (BAPI format)
  "! @raising ygms_cx_cst_error | Processing error occurred
  METHODS process
    IMPORTING
      iv_mode     TYPE char1
    EXPORTING
      et_messages TYPE bapiret2_t
    RAISING
      ygms_cx_cst_error.

  "! <p class="shorttext synchronized" lang="en">Validate allocation data</p>
  "! @parameter it_data | Allocation data to validate
  "! @parameter rv_valid | Returns ABAP_TRUE if data is valid
  "! @raising ygms_cx_cst_validation | Validation error occurred
  METHODS validate
    IMPORTING
      it_data         TYPE ygms_tt_allocation
    RETURNING
      VALUE(rv_valid) TYPE abap_bool
    RAISING
      ygms_cx_cst_validation.

  "! <p class="shorttext synchronized" lang="en">Get processing results</p>
  "! @parameter rt_results | Purchase data results
  METHODS get_results
    RETURNING
      VALUE(rt_results) TYPE ygms_tt_purchase.

ENDINTERFACE.
