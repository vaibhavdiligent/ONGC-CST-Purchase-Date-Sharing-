*&---------------------------------------------------------------------*
*& Interface: YGMS_IF_CST_CONSTANTS
*& Package: YGMS
*& Description: Constants Interface for ONGC CST Purchase Data Sharing
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

INTERFACE ygms_if_cst_constants PUBLIC.

  "! Tax Type Constants
  CONSTANTS:
    co_tax_cst TYPE char3 VALUE 'CST',  " Central Sales Tax
    co_tax_vat TYPE char3 VALUE 'VAT'.  " Value Added Tax

  "! State Code Constants
  CONSTANTS:
    co_state_gujarat     TYPE char4 VALUE 'GJ',   " Gujarat
    co_state_maharashtra TYPE char4 VALUE 'MH',   " Maharashtra
    co_state_madhya_prd  TYPE char4 VALUE 'MP',   " Madhya Pradesh
    co_state_rajasthan   TYPE char4 VALUE 'RJ',   " Rajasthan
    co_state_delhi       TYPE char4 VALUE 'DL',   " Delhi
    co_state_uttar_prd   TYPE char4 VALUE 'UP',   " Uttar Pradesh
    co_state_haryana     TYPE char4 VALUE 'HR'.   " Haryana

  "! Validation Status Constants
  CONSTANTS:
    co_status_valid TYPE char2 VALUE 'VS',  " Valid
    co_status_error TYPE char2 VALUE 'ER'.  " Error

  "! Flag Constants
  CONSTANTS:
    co_flag_true  TYPE char1 VALUE 'X',
    co_flag_false TYPE char1 VALUE ''.

  "! Number Range Object
  CONSTANTS:
    co_nr_object TYPE nrobj VALUE 'YGMS'.

  "! Message Class
  CONSTANTS:
    co_msg_class TYPE arbgb VALUE 'YGMS_MSG'.

  "! Audit Operation Constants (NEW in v1.2)
  CONSTANTS:
    co_audit_insert TYPE char1 VALUE 'I',  " Insert operation
    co_audit_update TYPE char1 VALUE 'U',  " Update operation
    co_audit_delete TYPE char1 VALUE 'D'.  " Delete operation

  "! Processing Mode Constants
  CONSTANTS:
    co_mode_upload     TYPE char1 VALUE 'U',  " Upload mode
    co_mode_allocate   TYPE char1 VALUE 'A',  " Allocate mode
    co_mode_view       TYPE char1 VALUE 'V',  " View mode
    co_mode_send       TYPE char1 VALUE 'S',  " Send mode
    co_mode_download   TYPE char1 VALUE 'D',  " Download mode
    co_mode_nomination TYPE char1 VALUE 'N'.  " Nomination mode

  "! Activity Constants for Authorization
  CONSTANTS:
    co_activity_create  TYPE activ_auth VALUE '01',  " Create
    co_activity_change  TYPE activ_auth VALUE '02',  " Change
    co_activity_display TYPE activ_auth VALUE '03'.  " Display

  "! ALV Status Icons
  CONSTANTS:
    co_icon_success TYPE icon_d VALUE '@08@',  " Green light
    co_icon_warning TYPE icon_d VALUE '@09@',  " Yellow light
    co_icon_error   TYPE icon_d VALUE '@0A@'.  " Red light

  "! Default validity dates
  CONSTANTS:
    co_valid_from_default TYPE datum VALUE '19000101',
    co_valid_to_default   TYPE datum VALUE '99991231'.

  "! Fortnight determination
  TYPES: BEGIN OF ty_fortnight,
           from_date TYPE datum,
           to_date   TYPE datum,
         END OF ty_fortnight.

  "! Conversion factors
  CONSTANTS:
    co_mmbtu_to_scm TYPE p DECIMALS 6 VALUE '1.055060'.  " MMBTU to SCM conversion factor

ENDINTERFACE.
