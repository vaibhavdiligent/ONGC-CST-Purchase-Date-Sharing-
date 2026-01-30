*&---------------------------------------------------------------------*
*& Database Table: YGMS_CST_B2B_1
*& Description: Receipt Data Storage with Audit
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
* DEFINE TABLE ygms_cst_b2b_1.
*   "Table Description: Receipt Data from ONGC (B2B Interface Data)
*   "Delivery Class: A (Application table, master and transaction data)
*   "Data Class: APPL0
*   "Size Category: 2

*----------------------------------------------------------------------*
* Field Definitions
*----------------------------------------------------------------------*
*   FIELD mandt         TYPE mandt        KEY "Client
*   FIELD gas_day       TYPE ygms_gas_day KEY "Gas Receipt Date
*   FIELD ctp_id        TYPE ygms_ctp_id  KEY "ONGC Terminal ID
*   FIELD ongc_material TYPE ygms_ongc_mat KEY "ONGC Material Code
*   FIELD location_id   TYPE ygms_loc_id      "GAIL Location ID (Mapped)
*   FIELD material      TYPE matnr            "GAIL Material Code (Mapped)
*   FIELD qty_scm       TYPE ygms_qty_scm     "Quantity in SCM
*   FIELD qty_mbg       TYPE ygms_qty_mbg     "Quantity in MMBTU
*   FIELD ongc_id       TYPE ygms_ongc_id     "ONGC Transaction ID
*   FIELD created_by    TYPE ernam            "Created By User
*   FIELD created_on    TYPE erdat            "Creation Date
*   FIELD created_at    TYPE erzet            "Creation Time
*   FIELD changed_by    TYPE aenam            "Last Changed By
*   FIELD changed_on    TYPE aedat            "Last Change Date
*   FIELD changed_at    TYPE aezet            "Last Change Time

* ENDDEFINE.
*----------------------------------------------------------------------*

DEFINE ygms_cst_b2b_1.
  DEFINE mandt         TYPE mandt.
  DEFINE gas_day       TYPE ygms_gas_day.
  DEFINE ctp_id        TYPE ygms_ctp_id.
  DEFINE ongc_material TYPE ygms_ongc_mat.
  DEFINE location_id   TYPE ygms_loc_id.
  DEFINE material      TYPE matnr.
  DEFINE qty_scm       TYPE ygms_qty_scm.
  DEFINE qty_mbg       TYPE ygms_qty_mbg.
  DEFINE ongc_id       TYPE ygms_ongc_id.
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
* @EndUserText.label : 'Receipt Data from ONGC (B2B Interface Data)'
* @AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
* @AbapCatalog.tableCategory : #TRANSPARENT
* @AbapCatalog.deliveryClass : #A
* @AbapCatalog.dataMaintenance : #RESTRICTED
* define table ygms_cst_b2b_1 {
*   key client        : mandt not null;
*   key gas_day       : ygms_gas_day not null;
*   key ctp_id        : ygms_ctp_id not null;
*   key ongc_material : ygms_ongc_mat not null;
*   location_id       : ygms_loc_id;
*   material          : matnr;
*   qty_scm           : ygms_qty_scm;
*   qty_mbg           : ygms_qty_mbg;
*   ongc_id           : ygms_ongc_id;
*   created_by        : ernam;
*   created_on        : erdat;
*   created_at        : erzet;
*   changed_by        : aenam;
*   changed_on        : aedat;
*   changed_at        : aezet;
* }
*----------------------------------------------------------------------*
