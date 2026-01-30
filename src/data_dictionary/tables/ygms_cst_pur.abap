*&---------------------------------------------------------------------*
*& Database Table: YGMS_CST_PUR
*& Description: Daily Purchase Data with Audit
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
* DEFINE TABLE ygms_cst_pur.
*   "Table Description: Daily Purchase Data - State-wise Allocation
*   "Delivery Class: A (Application table, master and transaction data)
*   "Data Class: APPL0
*   "Size Category: 3

*----------------------------------------------------------------------*
* Field Definitions
*----------------------------------------------------------------------*
*   FIELD mandt         TYPE mandt         KEY "Client
*   FIELD gas_day       TYPE ygms_gas_day  KEY "Gas Receipt Date
*   FIELD location_id   TYPE ygms_loc_id   KEY "GAIL Location ID
*   FIELD material      TYPE matnr         KEY "GAIL Material Code
*   FIELD state_code    TYPE ygms_state_cd KEY "State Code
*   FIELD state         TYPE ygms_state        "State Name
*   FIELD ctp_id        TYPE ygms_ctp_id       "ONGC Terminal ID
*   FIELD ongc_material TYPE ygms_ongc_mat     "ONGC Material Code
*   FIELD qty_mbg       TYPE ygms_qty_mbg      "Quantity in MMBTU
*   FIELD gcv           TYPE ygms_gcv          "Gross Calorific Value
*   FIELD ncv           TYPE ygms_ncv          "Net Calorific Value
*   FIELD qty_scm       TYPE ygms_qty_scm      "Quantity in SCM
*   FIELD gail_id       TYPE ygms_gail_id      "GAIL Transaction ID
*   FIELD tax_type      TYPE ygms_tax_type     "CST/VAT Indicator
*   FIELD excluded      TYPE ygms_excl_flg     "Exclusion Flag
*   FIELD sent          TYPE ygms_sent_flg     "Sent Flag
*   FIELD created_by    TYPE ernam             "Created By User
*   FIELD created_on    TYPE erdat             "Creation Date
*   FIELD created_at    TYPE erzet             "Creation Time
*   FIELD changed_by    TYPE aenam             "Last Changed By
*   FIELD changed_on    TYPE aedat             "Last Change Date
*   FIELD changed_at    TYPE aezet             "Last Change Time

* ENDDEFINE.
*----------------------------------------------------------------------*

DEFINE ygms_cst_pur.
  DEFINE mandt         TYPE mandt.
  DEFINE gas_day       TYPE ygms_gas_day.
  DEFINE location_id   TYPE ygms_loc_id.
  DEFINE material      TYPE matnr.
  DEFINE state_code    TYPE ygms_state_cd.
  DEFINE state         TYPE ygms_state.
  DEFINE ctp_id        TYPE ygms_ctp_id.
  DEFINE ongc_material TYPE ygms_ongc_mat.
  DEFINE qty_mbg       TYPE ygms_qty_mbg.
  DEFINE gcv           TYPE ygms_gcv.
  DEFINE ncv           TYPE ygms_ncv.
  DEFINE qty_scm       TYPE ygms_qty_scm.
  DEFINE gail_id       TYPE ygms_gail_id.
  DEFINE tax_type      TYPE ygms_tax_type.
  DEFINE excluded      TYPE ygms_excl_flg.
  DEFINE sent          TYPE ygms_sent_flg.
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
* @EndUserText.label : 'Daily Purchase Data - State-wise Allocation'
* @AbapCatalog.enhancement.category : #NOT_EXTENSIBLE
* @AbapCatalog.tableCategory : #TRANSPARENT
* @AbapCatalog.deliveryClass : #A
* @AbapCatalog.dataMaintenance : #RESTRICTED
* define table ygms_cst_pur {
*   key client        : mandt not null;
*   key gas_day       : ygms_gas_day not null;
*   key location_id   : ygms_loc_id not null;
*   key material      : matnr not null;
*   key state_code    : ygms_state_cd not null;
*   state             : ygms_state;
*   ctp_id            : ygms_ctp_id;
*   ongc_material     : ygms_ongc_mat;
*   qty_mbg           : ygms_qty_mbg;
*   gcv               : ygms_gcv;
*   ncv               : ygms_ncv;
*   qty_scm           : ygms_qty_scm;
*   gail_id           : ygms_gail_id;
*   tax_type          : ygms_tax_type;
*   excluded          : ygms_excl_flg;
*   sent              : ygms_sent_flg;
*   created_by        : ernam;
*   created_on        : erdat;
*   created_at        : erzet;
*   changed_by        : aenam;
*   changed_on        : aedat;
*   changed_at        : aezet;
* }
*----------------------------------------------------------------------*
