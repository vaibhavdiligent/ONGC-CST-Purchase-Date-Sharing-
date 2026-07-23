@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Production Performance - aggregation layer'
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
define view entity ZPRA_P_DPR_PERF_AGG
  with parameters
    P_DateFrom   : zpra_t_dly_prd-production_date,
    P_DateTo     : zpra_t_dly_prd-production_date,
    P_FiscalYear : zpra_t_prd_tar-gjahr

  as select from ZPRA_C_DPR_BOEPD_DAY

{
  key 'YTD'                                           as ScopeType,
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
group by ProductGroup

union all

  select from zpra_t_prd_tar as Tar
{
  key 'ANNUAL'                                        as ScopeType,
  key case Tar.product
        when '722000004' then 'GAS'
        else                  'OIL'
      end                                             as ProductGroup,

      /* no actuals at annual level - Excel shows "-" */
      cast( 0 as abap.dec( 23, 7 ) )                  as SumActualQty,
      cast( 0 as abap.dec( 23, 3 ) )                  as SumActualBoepd,

      cast( sum( Tar.tar_qty2 ) as abap.dec( 23, 7 ) ) as SumTargetQty,

      cast( sum( case Tar.product
                   when '722000004' then Tar.tar_qty2 * cast( 6290 as abap.dec( 5, 0 ) )
                   else                  Tar.tar_qty2
                 end ) as abap.dec( 23, 3 ) )         as SumTargetBoepd,

      /* fiscal months carrying a target (normally 12) */
      cast( count( distinct Tar.monat ) as abap.dec( 10, 0 ) ) as Divisor
}
where Tar.gjahr           = $parameters.P_FiscalYear
  and Tar.tar_code        = 'TAR_BE'
  and Tar.prod_vl_type_cd = 'NET_PROD'
group by case Tar.product
           when '722000004' then 'GAS'
           else                  'OIL'
         end
