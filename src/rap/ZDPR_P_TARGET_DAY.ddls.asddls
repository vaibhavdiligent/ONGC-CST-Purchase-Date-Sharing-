@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR BE Target daily rate per date/asset'
@Metadata.ignorePropagatedAnnotations: true

/* ── BE target daily rate on every production date ─────────────────────────
 * Date spine x annual BE target of the date's fiscal year:
 *   TargetQty   = AnnualTargetVolume / DaysInFiscalYear  (BOPD / MMSCMD)
 *   TargetBoepd = AnnualTargetBoe    / DaysInFiscalYear  (BOEPD)
 * Summed over assets/products for one date this equals the GRAND-TOTAL of
 * the classic "Target : YYYY-YY" line in BOPD/MMSCMD mode - the flat "BE
 * Target" line of the DPR graph. Targets are at OVL (share) level, like the
 * "Actual Production" line (ActualBoepdOvl). Business unit and product group
 * are derived exactly as in ZDPR_P_DAY_BASE so both row kinds share the same
 * dimension values in the cube.
 * ─────────────────────────────────────────────────────────────────────────── */
define view entity ZDPR_P_TARGET_DAY
  as select from ZDPR_P_DATE_SPINE as Cal

  inner join ZDPR_I_TARGET_FY as Tar
    on  Tar.FiscalYear = Cal.FiscalYear
    and Tar.TargetCode = 'TAR_BE'

{
  key Cal.ProductionDate                              as ProductionDate,
  key Tar.Asset                                       as Asset,
  key Tar.Block                                       as Block,
  key Tar.Product                                     as Product,

      Tar.ProductGroup                                as ProductGroup,

      case substring( Tar.Asset, 1, 3 )
        when 'RUS' then 'BU-RUSSIA'
        when 'BRA' then 'BU-LAC'
        when 'COL' then 'BU-LAC'
        when 'VEN' then 'BU-LAC'
        when 'MMR' then 'BU-ASIA PACIFIC'
        when 'VNM' then 'BU-ASIA PACIFIC'
        when 'AZE' then 'BU-MENA CIS'
        when 'SSU' then 'BU-MENA CIS'
        when 'SUD' then 'BU-MENA CIS'
        when 'UAE' then 'BU-MENA CIS'
        else            'OTHER'
      end                                             as BusinessUnit,

      Cal.FiscalYear                                  as FiscalYear,
      Cal.FiscalPeriod                                as FiscalPeriod,

      /* daily target rate in native units (BOPD / MMSCMD) */
      cast( case when Tar.DaysInFiscalYear > 0
                 then division( Tar.AnnualTargetVolume, Tar.DaysInFiscalYear, 7 )
                 else 0
            end as abap.dec( 23, 7 ) )                as TargetQty,

      /* daily target rate in BOEPD */
      cast( case when Tar.DaysInFiscalYear > 0
                 then division( Tar.AnnualTargetBoe, Tar.DaysInFiscalYear, 3 )
                 else 0
            end as abap.dec( 23, 3 ) )                as TargetBoepd
}
