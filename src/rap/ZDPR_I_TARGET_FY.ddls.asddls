@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Annual Target per Asset (FY)'
@Metadata.ignorePropagatedAnnotations: true

/* ── Annual target per fiscal year / asset / block / product ────────────────
 * Replicates section 2b ("Target : YYYY-YY") of the classic DPR report:
 *   - all fiscal months of the year are summed (SELECT ... WHERE gjahr EQ
 *     gv_current_gjahr - no month restriction)
 *   - all three volume types NET_PROD / GROSS_PROD / GAS_INJ are included
 *     (r_prd_vl_type[] of the classic report; gas targets are NOT stored as
 *     NET_PROD, which is why a NET_PROD-only join returned 0 for gas)
 *   - volumes are the scaled figures of ZDPR_I_TARGET (barrels / MMSCM, BOE)
 * The consumer divides the annual volume by DaysInFiscalYear to obtain the
 * flat "BE Target" daily rate (fill_dynamic_table_sec2b: value / lv_days).
 * CDS cannot divide two aggregates in one view, so the division happens in
 * ZDPR_P_TARGET_DAY.
 * ─────────────────────────────────────────────────────────────────────────── */
define view entity ZDPR_I_TARGET_FY
  as select from ZDPR_I_TARGET as Tar

{
  key Tar.TargetCode                                  as TargetCode,
  key Tar.FiscalYear                                  as FiscalYear,
  key Tar.Asset                                       as Asset,
  key Tar.Block                                       as Block,
  key Tar.Product                                     as Product,

      Tar.ProductGroup                                as ProductGroup,
      Tar.FiscalYearStart                             as FiscalYearStart,
      Tar.DaysInFiscalYear                            as DaysInFiscalYear,

      /* annual target volume: barrels (oil family) / MMSCM (gas) */
      cast( sum( Tar.TargetVolume ) as abap.dec( 23, 3 ) ) as AnnualTargetVolume,

      /* annual target in barrels of oil equivalent */
      cast( sum( Tar.TargetBoe )    as abap.dec( 23, 3 ) ) as AnnualTargetBoe,

      /* fiscal months carrying a target (normally 12) */
      cast( count( distinct Tar.FiscalPeriod ) as abap.dec( 3, 0 ) ) as TargetMonths
}
where
     Tar.VolumeType = 'NET_PROD'
  or Tar.VolumeType = 'GROSS_PROD'
  or Tar.VolumeType = 'GAS_INJ'
group by
  Tar.TargetCode,
  Tar.FiscalYear,
  Tar.Asset,
  Tar.Block,
  Tar.Product,
  Tar.ProductGroup,
  Tar.FiscalYearStart,
  Tar.DaysInFiscalYear
