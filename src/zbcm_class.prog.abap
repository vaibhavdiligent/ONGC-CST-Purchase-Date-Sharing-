*&---------------------------------------------------------------------*
*& Include          ZBCM_CLASS
*&---------------------------------------------------------------------*
CLASS lcl_file_verifier DEFINITION CREATE PRIVATE FINAL.

  PUBLIC SECTION.

    CLASS-METHODS:
      get_instance
        RETURNING
          value(ro_obj) TYPE REF TO lcl_file_verifier.

*    METHODS:
*      get_file_info   " get the file data, type and other information
*        IMPORTING
*          value(iv_filename)  TYPE string
*        EXPORTING
*          value(ev_signature) TYPE xstring.

    METHODS:
      verify_signature
        IMPORTING
          value(iv_signature)     TYPE xstring
        EXPORTING
          value(ev_owner)         TYPE string
          value(ev_email)         TYPE string
          value(ev_serial)        TYPE string
          value(ev_thumbprint)    TYPE string
          value(ev_validfrom)     TYPE string
          value(ev_validto)       TYPE string
          value(ev_issuer)        TYPE string
          value(ev_bindocument_out) TYPE xstring,

      get_filename
        EXPORTING
          value(ev_filename_save) TYPE string
          value(ev_file_type)     TYPE char10.

*  PRIVATE SECTION.

    CLASS-DATA: go_object TYPE REF TO lcl_file_verifier.

    METHODS:
      get_signer_info
        IMPORTING
          value(is_signerlist) TYPE ssfsigner
        EXPORTING
          value(ev_owner)      TYPE string
          value(ev_email)      TYPE string
          value(ev_serial)     TYPE string
          value(ev_thumbprint) TYPE string
          value(ev_validfrom)  TYPE string
          value(ev_validto)    TYPE string
          value(ev_issuer)     TYPE string
          value(e_int)         type i.

    DATA: mv_filename_save TYPE string,
          mv_file_type     TYPE char10.

ENDCLASS.                    "lcl_file_verifier DEFINITION
