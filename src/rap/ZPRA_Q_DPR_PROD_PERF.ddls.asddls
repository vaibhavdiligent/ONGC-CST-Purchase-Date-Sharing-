@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Production Performance (Excel tab 3)'
@Metadata.ignorePropagatedAnnotations: true

/* ── The Production Performance table of the DPR Excel (tab 3) ──────────────
 * One row per (Scope, Product group):
 *   Scope 'YTD'    -> Actual & BE Target per-day figures for the report window
 *   Scope 'ANNUAL' -> BE Target per-day figure only (Actual = 0, Excel "-")
 * Columns mirror the Excel blocks:
 *   Oil group  : BOPD        Gas group : MMSCMD       Boepd : Total (O+OEG)
 * AchievementPct = YTD "% Achv w.r.t. BE Target" (0 for ANNUAL rows, where the
 * Excel leaves the cell blank). Operates on <= 4 pre-aggregated rows - the
 * division cost is negligible; all scanning happened in ZPRA_P_DPR_PERF_AGG.
 * ─────────────────────────────────────────────────────────────────────────── */
@OData.entityType.name: 'DPRProdPerfQueryType'

define view entity ZPRA_Q_DPR_PROD_PERF
  with parameters
    P_DateFrom   : zpra_t_dly_prd-production_date,
    P_DateTo     : zpra_t_dly_prd-production_date,
    P_FiscalYear : zpra_t_prd_tar-gjahr

  as select from ZPRA_P_DPR_PERF_AGG(
                   P_DateFrom   : $parameters.P_DateFrom,
                   P_DateTo     : $parameters.P_DateTo,
                   P_FiscalYear : $parameters.P_FiscalYear )

{
  key ScopeType,
  key ProductGroup,

      @EndUserText.label: 'Scope'
      case ScopeType
        when 'YTD'    then 'YTD'
        when 'ANNUAL' then 'Annual'
        else               ScopeType
      end                                             as ScopeText,

      @EndUserText.label: 'Product Group'
      case ProductGroup
        when 'GAS' then 'Gas ( MMSCMD )'
        else            'Oil, LNG & Condensate ( BOPD )'
      end                                             as ProductGroupText,

      /* ── Actual (per-day average over the window; 0 on ANNUAL rows) ──── */
      @EndUserText.label: 'Actual (BOPD / MMSCMD)'
      cast( case when Divisor > 0
                 then SumActualQty / Divisor
                 else cast( 0 as abap.dec( 23, 7 ) )
            end as abap.dec( 23, 7 ) )                as ActualPerDay,

      @EndUserText.label: 'Actual Total (BOEPD)'
      cast( case when Divisor > 0
                 then SumActualBoepd / Divisor
                 else cast( 0 as abap.dec( 23, 3 ) )
            end as abap.dec( 23, 3 ) )                as ActualBoepdPerDay,

      /* ── BE Target (per-day rate) ────────────────────────────────────── */
      @EndUserText.label: 'BE Target (BOPD / MMSCMD)'
      cast( case when Divisor > 0
                 then SumTargetQty / Divisor
                 else cast( 0 as abap.dec( 23, 7 ) )
            end as abap.dec( 23, 7 ) )                as TargetPerDay,

      @EndUserText.label: 'BE Target Total (BOEPD)'
      cast( case when Divisor > 0
                 then SumTargetBoepd / Divisor
                 else cast( 0 as abap.dec( 23, 3 ) )
            end as abap.dec( 23, 3 ) )                as TargetBoepdPerDay,

      /* ── % Achievement w.r.t. BE Target (YTD only; 0 -> blank/Annual) ── */
      @EndUserText.label: '% Achv w.r.t. BE Target'
      cast( case when ScopeType = 'YTD' and SumTargetBoepd > 0
                 then SumActualBoepd * cast( 100 as abap.dec( 4, 0 ) )
                      / SumTargetBoepd
                 else cast( 0 as abap.dec( 10, 2 ) )
            end as abap.dec( 10, 2 ) )                as AchievementPct,

      /* UI criticality for the % cell: 3=green >=100, 2=amber >=90, 1=red,
         0=neutral (ANNUAL rows - no actual, Excel shows "-") */
      @EndUserText.label: 'Achievement Criticality'
      cast( case
              when ScopeType <> 'YTD' or SumTargetBoepd <= 0        then 0
              when SumActualBoepd >= SumTargetBoepd                 then 3
              when SumActualBoepd * cast( 100 as abap.dec( 4, 0 ) )
                   >= SumTargetBoepd * cast( 90 as abap.dec( 4, 0 ) ) then 2
              else 1
            end as abap.int1 )                        as AchievementCriticality
}
