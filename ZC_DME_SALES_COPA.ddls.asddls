@AbapCatalog.sqlViewName: 'ZCDMESALESCOPA'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DME Month-End CO-PA Sales Source (CE2/ACDOCA)'
@VDM.viewType: #COMPOSITE
*&---------------------------------------------------------------------*
*& CDS View  ZC_DME_SALES_COPA
*&---------------------------------------------------------------------*
*& Purpose : Single normalised sales/profitability source for the DME
*&           month-end CO-PA program (ZRDME_MONTH_END_COPA).
*&           Implements the FS source switch (section 3 / 4.2.4):
*&             - CE2JP00  : periods up to Nov-2027  (parallel-run window)
*&             - ACDOCA   : periods from Dec-2027   (target state)
*&           The two branches are UNIONed and selected by PERIO so the
*&           consuming program stays source-agnostic.
*&
*& NOTE    : The ACDOCA branch must read the operating-concern
*&           profitability characteristics. Where ACDOCA does not carry a
*&           CE characteristic natively, the corresponding CI_ACDOCA
*&           extension / append field is used (FS 9.2, 9.4). The field
*&           list below reflects the FS mapping; adjust extension field
*&           names to the customer ACDOCA append at build time.
*&           Operating concern : JP00   (currency type / PALEDGER 01)
*&---------------------------------------------------------------------*

// ---- Branch 1: CE2JP00 (summarised plan/expense table) -------------
define view ZC_DME_SALES_COPA
  as select from ce2jp00 as ce
{
  key ce.gjahr                       as gjahr,
  key cast( ce.perbl as abap.numc(3) ) as perde,
      // YYYY0PP key used by the program's SELECT
      ce.perbl                       as perio,
      ce.bukrs                       as bukrs,
      ce.kndnr                       as kndnr,
      ce.artnr                       as artnr,
      ce.werks                       as werks,
      ce.vkorg                       as vkorg,
      ce.vtweg                       as vtweg,
      ce.spart                       as spart,
      ce.prctr                       as prctr,
      ce.kunwe                       as kunwe,
      ce.kmvkbu                      as kmvkbu,
      ce.ww207                       as ww207,
      ce.ww214                       as ww214,
      ce.vkaus                       as vkaus,
      ce.ww228                       as ww228,
      ce.ww229                       as ww229,
      ce.erlos                       as erlos,
      ce.vv506                       as vv506,
      ce.vv507                       as vv507
}
where ce.paledger = '01'
  and ce.vrgar    = '5'                 // plan record type (expense, AS-IS)
  and ce.versi    = '101'
  and ce.perbl   <= '2027011'           // up to Nov-2027

union all

// ---- Branch 2: ACDOCA (actual line items) - target state ----------
select from acdoca as ad
{
  key ad.gjahr                       as gjahr,
  key cast( ad.poper as abap.numc(3) ) as perde,
      concat( ad.gjahr, concat( '0', substring( ad.poper, 2, 2 ) ) ) as perio,
      ad.rbukrs                      as bukrs,
      ad.kunnr                       as kndnr,
      ad.matnr                       as artnr,
      ad.werks                       as werks,
      ad.vkorg                       as vkorg,
      ad.vtweg                       as vtweg,
      ad.spart                       as spart,
      ad.prctr                       as prctr,
      ad.kunwe                       as kunwe,
      ad.vkbur                       as kmvkbu,
      // CO-PA / CI_ACDOCA characteristics (FS 9.2)
      ad.ww207                       as ww207,
      ad.ww214                       as ww214,
      ad.vkaus                       as vkaus,
      ad.ww228                       as ww228,
      ad.ww229                       as ww229,
      ad.ksl                         as erlos,
      ad.vv506                       as vv506,
      ad.vv507                       as vv507
}
where ad.rldnr  = '0L'                  // leading ledger
  and ad.poper >= '012'                 // (combined with year filter below)
  and ad.gjahr >= '2027'                // from Dec-2027 onwards
