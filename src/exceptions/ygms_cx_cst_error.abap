*&---------------------------------------------------------------------*
*& Exception Class: YGMS_CX_CST_ERROR
*& Package: YGMS
*& Description: Base Exception Class for ONGC CST Purchase Data Sharing
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

CLASS ygms_cx_cst_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_message.
    INTERFACES if_t100_dyn_msg.

    "! Upload failed exception
    CONSTANTS:
      BEGIN OF upload_failed,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF upload_failed.

    "! Multiple fortnight exception
    CONSTANTS:
      BEGIN OF multi_fortnight,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF multi_fortnight.

    "! Mapping not found exception
    CONSTANTS:
      BEGIN OF mapping_not_found,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE 'MV_PARAM2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF mapping_not_found.

    "! Material mapping not found exception
    CONSTANTS:
      BEGIN OF material_mapping_not_found,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE 'MV_PARAM2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF material_mapping_not_found.

    "! GCV/NCV missing exception
    CONSTANTS:
      BEGIN OF gcv_ncv_missing,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE 'MV_PARAM2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gcv_ncv_missing.

    "! Validation failed exception
    CONSTANTS:
      BEGIN OF validation_failed,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE 'MV_PARAM2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF validation_failed.

    "! Save failed exception
    CONSTANTS:
      BEGIN OF save_failed,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '010',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF save_failed.

    "! Authorization failed exception
    CONSTANTS:
      BEGIN OF auth_failed,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '011',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF auth_failed.

    "! State authorization failed exception
    CONSTANTS:
      BEGIN OF state_auth_failed,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '012',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF state_auth_failed.

    "! Email transmission failed exception
    CONSTANTS:
      BEGIN OF email_failed,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '014',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF email_failed.

    "! Mapping expired exception
    CONSTANTS:
      BEGIN OF mapping_expired,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '017',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE 'MV_PARAM2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF mapping_expired.

    "! Exception parameters
    DATA: mv_param1 TYPE string READ-ONLY,
          mv_param2 TYPE string READ-ONLY.

    "! <p class="shorttext synchronized" lang="en">Constructor</p>
    "! @parameter textid | Text ID for message
    "! @parameter previous | Previous exception
    "! @parameter mv_param1 | First message parameter
    "! @parameter mv_param2 | Second message parameter
    METHODS constructor
      IMPORTING
        textid    LIKE if_t100_message=>t100key OPTIONAL
        previous  LIKE previous OPTIONAL
        mv_param1 TYPE string OPTIONAL
        mv_param2 TYPE string OPTIONAL.

ENDCLASS.


CLASS ygms_cx_cst_error IMPLEMENTATION.

  METHOD constructor.
    super->constructor( previous = previous ).
    me->mv_param1 = mv_param1.
    me->mv_param2 = mv_param2.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
