@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Production Performance Aggregates'
@Metadata.ignorePropagatedAnnotations: true

/* ── Inner aggregation for the Production Performance query (Excel tab 3) ──
 * Emits at most 4 rows: (OIL|GAS) x (YTD|ANNUAL). All heavy lifting (sums,
 * distinct counts) is grouped and pushed to HANA here; the outer query only
 * divides these tiny aggregates - the pattern needed because CDS cannot
 * divide two aggregates in a single view.
 *   YTD    : actual + target sums over the report window [P_DateFrom..P_DateTo],
 *            Divisor = number of production days in the window
 *            -> per-day figures == the Excel "YTD Actual/Target" row values
 *   ANNUAL : BE-target sums over the whole fiscal year P_FiscalYear,
 *            Divisor = number of fiscal months found (normally 12)
 *            -> per-day annual target rate; no actuals (Excel shows "-")
 * ─────────────────────────────────────────────────────────────────────────── */
define view entity ZDPR_P_PERF_AGG
  with parameters
    P_DateFrom   : abap.dats,
    P_DateTo     : abap.dats,
    P_FiscalYear : gjahr

  as select from ZDPR_C_BOEPD_DAY

{
  key cast( 'YTD' as abap.char( 6 ) )                 as ScopeType,
  key ProductGroup                                    as ProductGroup,

      cast( sum( ActualQtyOvl )   as abap.dec( 23, 7 ) ) as SumActualQty,
      cast( sum( ActualBoepdOvl ) as abap.dec( 23, 3 ) ) as SumActualBoepd,
      cast( sum( TargetQty )      as abap.dec( 23, 7 ) ) as SumTargetQty,
      cast( sum( TargetBoepd )    as abap.dec( 23, 3 ) ) as SumTargetBoepd,

      /* production days in the window */
      cast( count( distinct ProductionDate ) as abap.dec( 10, 0 ) ) as Divisor
}
where ProductionDate >= $parameters.P_DateFrom
  and ProductionDate <= $parameters.P_DateTo
  and BusinessUnit <> 'OTHER'   /* screen out test/garbage asset codes */
group by ProductGroup

union all

  /* ZDPR_I_TARGET pre-computes ProductGroup and TargetBoepd as plain
     columns, so this branch groups and sums plain fields only (no CASE
     in GROUP BY / aggregates - rejected by the target release). */
  select from ZDPR_I_TARGET as Tar
{
  key cast( 'ANNUAL' as abap.char( 6 ) )              as ScopeType,
  key Tar.ProductGroup                                as ProductGroup,

      /* no actuals at annual level - Excel shows "-" */
      cast( 0 as abap.dec( 23, 7 ) )                  as SumActualQty,
      cast( 0 as abap.dec( 23, 3 ) )                  as SumActualBoepd,

      cast( sum( Tar.TargetQty )   as abap.dec( 23, 7 ) ) as SumTargetQty,

      cast( sum( Tar.TargetBoepd ) as abap.dec( 23, 3 ) ) as SumTargetBoepd,

      /* fiscal months carrying a target (normally 12) */
      cast( count( distinct Tar.FiscalPeriod ) as abap.dec( 10, 0 ) ) as Divisor
}
where Tar.FiscalYear = $parameters.P_FiscalYear
  and Tar.TargetCode = 'TAR_BE'
  and Tar.VolumeType = 'NET_PROD'
group by Tar.ProductGroup
