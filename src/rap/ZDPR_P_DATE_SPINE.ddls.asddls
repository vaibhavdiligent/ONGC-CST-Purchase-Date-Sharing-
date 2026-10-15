@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Production Dates (date spine)'
@Metadata.ignorePropagatedAnnotations: true

/* ── Date spine ────────────────────────────────────────────────────────────
 * One row per production date present in ZPRA_T_DLY_PRD, with its fiscal
 * year (April..March). Used to spread the annual BE target over every
 * reporting day so the "BE Target" line of the graph is flat and present on
 * each date, independent of which assets reported that day (the classic
 * report writes the same annual figure on every chart row -
 * fill_dynamic_table_sec5a).
 * ─────────────────────────────────────────────────────────────────────────── */
define view entity ZDPR_P_DATE_SPINE
  as select from zpra_t_dly_prd as D

{
  key D.production_date                               as ProductionDate,

      cast( substring( dats_add_months( D.production_date, -3, 'INITIAL' ), 1, 4 )
            as abap.numc(4) )                         as FiscalYear,

      cast( substring( dats_add_months( D.production_date, -3, 'INITIAL' ), 5, 2 )
            as abap.numc(2) )                         as FiscalPeriod
}
group by
  D.production_date
