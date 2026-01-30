*&---------------------------------------------------------------------*
*& Database Table: YGMS_CST_AUDIT_LOG
*& Description: Central Audit Log Table
*& Delivery Class: A (Application Table)
*& Package: YGMS
*& Version: 1.2 (NEW)
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Table Definition
*----------------------------------------------------------------------*
* DEFINE TABLE ygms_cst_audit_log.
*   "Table Description: Central Audit Log for all data changes
*   "Delivery Class: A (Application table, master and transaction data)
*   "Data Class: APPL0
*   "Size Category: 4

*----------------------------------------------------------------------*
* Field Definitions
*----------------------------------------------------------------------*
*   FIELD mandt      TYPE mandt      KEY "Client
*   FIELD log_id     TYPE ygms_log_id KEY "Unique Log Entry ID (GUID)
*   FIELD table_name TYPE tabname        "Table Being Changed
*   FIELD operation  TYPE char1          "Operation (I=Insert, U=Update, D=Delete)
*   FIELD key_field1 TYPE char100        "Primary Key Value 1
*   FIELD key_field2 TYPE char100        "Primary Key Value 2
*   FIELD key_field3 TYPE char100        "Primary Key Value 3
*   FIELD field_name TYPE fieldname      "Changed Field Name
*   FIELD old_value  TYPE char255        "Old Value
*   FIELD new_value  TYPE char255        "New Value
*   FIELD changed_by TYPE ernam          "Changed By User
*   FIELD changed_on TYPE erdat          "Change Date
*   FIELD changed_at TYPE erzet          "Change Time

* ENDDEFINE.
*----------------------------------------------------------------------*

DEFINE ygms_cst_audit_log.
  DEFINE mandt      TYPE mandt.
  DEFINE log_id     TYPE ygms_log_id.
  DEFINE table_name TYPE tabname.
  DEFINE operation  TYPE char1.
  DEFINE key_field1 TYPE char100.
  DEFINE key_field2 TYPE char100.
  DEFINE key_field3 TYPE char100.
  DEFINE field_name TYPE fieldname.
  DEFINE old_value  TYPE char255.
  DEFINE new_value  TYPE char255.
  DEFINE changed_by TYPE ernam.
  DEFINE changed_on TYPE erdat.
  DEFINE changed_at TYPE erzet.
ENDDEFINE.

*----------------------------------------------------------------------*
* Table DDL Statement (for reference)
*----------------------------------------------------------------------*
* @EndUserText.label : 'Central Audit Log for all data changes'
* @AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
* @AbapCatalog.tableCategory : #TRANSPARENT
* @AbapCatalog.deliveryClass : #A
* @AbapCatalog.dataMaintenance : #RESTRICTED
* define table ygms_cst_audit_log {
*   key client     : mandt not null;
*   key log_id     : ygms_log_id not null;
*   table_name     : tabname;
*   operation      : char1;
*   key_field1     : char100;
*   key_field2     : char100;
*   key_field3     : char100;
*   field_name     : fieldname;
*   old_value      : char255;
*   new_value      : char255;
*   changed_by     : ernam;
*   changed_on     : erdat;
*   changed_at     : erzet;
* }
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Secondary Index Definition (for performance)
*----------------------------------------------------------------------*
* Index YGMS_CST_AUDIT_LOG~001
* Fields: TABLE_NAME, CHANGED_ON, CHANGED_AT
* Description: Index for audit log queries by table and date
*----------------------------------------------------------------------*
