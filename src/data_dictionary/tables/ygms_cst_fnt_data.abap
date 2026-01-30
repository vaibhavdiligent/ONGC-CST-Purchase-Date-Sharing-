*&---------------------------------------------------------------------*
*& Database Table: YGMS_CST_FNT_DATA
*& Description: Fortnightly Aggregate Data with Audit
*& Delivery Class: A (Application Table)
*& Package: YGMS
*&---------------------------------------------------------------------*
*& Change History:
*& Date        Author      Description
*& ----------  ----------  --------------------------------------------
*& DD.MM.YYYY  [Name]      Initial development (TSD v1.2)
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Table Definition
*----------------------------------------------------------------------*
* DEFINE TABLE ygms_cst_fnt_data.
*   "Table Description: Fortnightly Aggregate Data
*   "Delivery Class: A (Application table, master and transaction data)
*   "Data Class: APPL0
*   "Size Category: 2

*----------------------------------------------------------------------*
* Field Definitions
*----------------------------------------------------------------------*
*   FIELD mandt         TYPE mandt         KEY "Client
*   FIELD from_date     TYPE datum         KEY "Fortnight Start Date
*   FIELD to_date       TYPE datum         KEY "Fortnight End Date
*   FIELD location_id   TYPE ygms_loc_id   KEY "GAIL Location ID
*   FIELD material      TYPE matnr         KEY "GAIL Material Code
*   FIELD state_code    TYPE ygms_state_cd KEY "State Code
*   FIELD total_qty_mbg TYPE ygms_qty_mbg      "Total Quantity in MMBTU
*   FIELD avg_gcv       TYPE ygms_gcv          "Weighted Average GCV
*   FIELD avg_ncv       TYPE ygms_ncv          "Weighted Average NCV
*   FIELD total_qty_scm TYPE ygms_qty_scm      "Total Quantity in SCM
*   FIELD gail_id       TYPE ygms_gail_id      "GAIL Transaction ID
*   FIELD created_by    TYPE ernam             "Created By User
*   FIELD created_on    TYPE erdat             "Creation Date
*   FIELD created_at    TYPE erzet             "Creation Time
*   FIELD changed_by    TYPE aenam             "Last Changed By
*   FIELD changed_on    TYPE aedat             "Last Change Date
*   FIELD changed_at    TYPE aezet             "Last Change Time

* ENDDEFINE.
*----------------------------------------------------------------------*

DEFINE ygms_cst_fnt_data.
  DEFINE mandt         TYPE mandt.
  DEFINE from_date     TYPE datum.
  DEFINE to_date       TYPE datum.
  DEFINE location_id   TYPE ygms_loc_id.
  DEFINE material      TYPE matnr.
  DEFINE state_code    TYPE ygms_state_cd.
  DEFINE total_qty_mbg TYPE ygms_qty_mbg.
  DEFINE avg_gcv       TYPE ygms_gcv.
  DEFINE avg_ncv       TYPE ygms_ncv.
  DEFINE total_qty_scm TYPE ygms_qty_scm.
  DEFINE gail_id       TYPE ygms_gail_id.
  DEFINE created_by    TYPE ernam.
  DEFINE created_on    TYPE erdat.
  DEFINE created_at    TYPE erzet.
  DEFINE changed_by    TYPE aenam.
  DEFINE changed_on    TYPE aedat.
  DEFINE changed_at    TYPE aezet.
ENDDEFINE.

*----------------------------------------------------------------------*
* Table DDL Statement (for reference)
*----------------------------------------------------------------------*
* @EndUserText.label : 'Fortnightly Aggregate Data'
* @AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
* @AbapCatalog.tableCategory : #TRANSPARENT
* @AbapCatalog.deliveryClass : #A
* @AbapCatalog.dataMaintenance : #RESTRICTED
* define table ygms_cst_fnt_data {
*   key client        : mandt not null;
*   key from_date     : datum not null;
*   key to_date       : datum not null;
*   key location_id   : ygms_loc_id not null;
*   key material      : matnr not null;
*   key state_code    : ygms_state_cd not null;
*   total_qty_mbg     : ygms_qty_mbg;
*   avg_gcv           : ygms_gcv;
*   avg_ncv           : ygms_ncv;
*   total_qty_scm     : ygms_qty_scm;
*   gail_id           : ygms_gail_id;
*   created_by        : ernam;
*   created_on        : erdat;
*   created_at        : erzet;
*   changed_by        : aenam;
*   changed_on        : aedat;
*   changed_at        : aezet;
* }
*----------------------------------------------------------------------*
