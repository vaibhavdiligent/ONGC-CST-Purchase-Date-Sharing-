*&---------------------------------------------------------------------*
*& Database Table: YGMS_CST_MAT_MAP
*& Description: Material Mapping with Time-Dependent Validity
*& Delivery Class: C (Customizing Table)
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
* DEFINE TABLE ygms_cst_mat_map.
*   "Table Description: Material Mapping - ONGC Material to GAIL Material
*   "Delivery Class: C (Customizing, maintenance by customer only)
*   "Data Class: APPL1
*   "Size Category: 0

*----------------------------------------------------------------------*
* Field Definitions
*----------------------------------------------------------------------*
*   FIELD mandt         TYPE mandt        KEY "Client
*   FIELD ongc_material TYPE ygms_ongc_mat KEY "ONGC Material Code
*   FIELD location_id   TYPE ygms_loc_id  KEY "GAIL Location ID
*   FIELD valid_from    TYPE datab        KEY "Valid From Date
*   FIELD valid_to      TYPE datbi            "Valid To Date
*   FIELD gail_material TYPE matnr            "GAIL Material Code
*   FIELD created_by    TYPE ernam            "Created By User
*   FIELD created_on    TYPE erdat            "Creation Date
*   FIELD created_at    TYPE erzet            "Creation Time
*   FIELD changed_by    TYPE aenam            "Last Changed By
*   FIELD changed_on    TYPE aedat            "Last Change Date
*   FIELD changed_at    TYPE aezet            "Last Change Time

* ENDDEFINE.
*----------------------------------------------------------------------*

DEFINE ygms_cst_mat_map.
  DEFINE mandt         TYPE mandt.
  DEFINE ongc_material TYPE ygms_ongc_mat.
  DEFINE location_id   TYPE ygms_loc_id.
  DEFINE valid_from    TYPE datab.
  DEFINE valid_to      TYPE datbi.
  DEFINE gail_material TYPE matnr.
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
* @EndUserText.label : 'Material Mapping - ONGC Material to GAIL Material'
* @AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
* @AbapCatalog.tableCategory : #TRANSPARENT
* @AbapCatalog.deliveryClass : #C
* @AbapCatalog.dataMaintenance : #RESTRICTED
* define table ygms_cst_mat_map {
*   key client        : mandt not null;
*   key ongc_material : ygms_ongc_mat not null;
*   key location_id   : ygms_loc_id not null;
*   key valid_from    : datab not null;
*   valid_to          : datbi;
*   gail_material     : matnr;
*   created_by        : ernam;
*   created_on        : erdat;
*   created_at        : erzet;
*   changed_by        : aenam;
*   changed_on        : aedat;
*   changed_at        : aezet;
* }
*----------------------------------------------------------------------*
