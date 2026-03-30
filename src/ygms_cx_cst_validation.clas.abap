CLASS ygms_cx_cst_validation DEFINITION
  PUBLIC
  INHERITING FROM ygms_cx_cst_error
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! Allocation mismatch exception
    CONSTANTS:
      BEGIN OF allocation_mismatch,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '009',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE 'MV_PARAM2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF allocation_mismatch.

    "! Invalid date range exception
    CONSTANTS:
      BEGIN OF invalid_date_range,
        msgid TYPE symsgid VALUE 'YGMS_MSG',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'MV_PARAM1',
        attr2 TYPE scx_attrname VALUE 'MV_PARAM2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF invalid_date_range.

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


CLASS ygms_cx_cst_validation IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(
      textid    = textid
      previous  = previous
      mv_param1 = mv_param1
      mv_param2 = mv_param2
    ).
  ENDMETHOD.

ENDCLASS.
