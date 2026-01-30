*&---------------------------------------------------------------------*
*& ABAP Data Dictionary - Domain Definitions
*& Package: YGMS
*& Description: Domain definitions for ONGC CST Purchase Data Sharing
*&---------------------------------------------------------------------*
*& Domain: YGMS_CTP_ID
*& Data Type: CHAR
*& Length: 20
*& Description: ONGC CTP/Terminal ID
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Domain: YGMS_LOC_ID
*& Data Type: CHAR
*& Length: 10
*& Description: GAIL Location ID
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Domain: YGMS_ONGC_MAT
*& Data Type: CHAR
*& Length: 20
*& Description: ONGC Material Code
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Domain: YGMS_QTY_SCM
*& Data Type: DEC
*& Length: 15
*& Decimals: 3
*& Description: Quantity in SCM (Standard Cubic Meters)
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Domain: YGMS_QTY_MBG
*& Data Type: DEC
*& Length: 15
*& Decimals: 3
*& Description: Quantity in MMBTU
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Domain: YGMS_GCV
*& Data Type: DEC
*& Length: 13
*& Decimals: 3
*& Description: Gross Calorific Value (kcal/SCM)
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Domain: YGMS_NCV
*& Data Type: DEC
*& Length: 13
*& Decimals: 3
*& Description: Net Calorific Value (kcal/SCM)
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Domain: YGMS_ONGC_ID
*& Data Type: CHAR
*& Length: 9
*& Description: ONGC Transaction ID
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Domain: YGMS_GAIL_ID
*& Data Type: CHAR
*& Length: 16
*& Description: GAIL Transaction ID
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Domain: YGMS_STATE_CD
*& Data Type: CHAR
*& Length: 4
*& Description: State Code
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Domain: YGMS_STATE
*& Data Type: CHAR
*& Length: 40
*& Description: State Name
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Domain: YGMS_TAX_TYPE
*& Data Type: CHAR
*& Length: 3
*& Fixed Values: CST, VAT
*& Description: Tax Type (CST=Central Sales Tax, VAT=Value Added Tax)
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Domain: YGMS_FLAG
*& Data Type: CHAR
*& Length: 1
*& Fixed Values: X, (blank)
*& Description: Generic Flag
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Domain: YGMS_LOG_ID
*& Data Type: CHAR
*& Length: 32
*& Description: Audit Log Entry ID (GUID)
*&---------------------------------------------------------------------*
