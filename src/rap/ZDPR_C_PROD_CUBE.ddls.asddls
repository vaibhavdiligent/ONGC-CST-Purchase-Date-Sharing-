@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Production - Analytical Cube'
@Metadata.ignorePropagatedAnnotations: true

/* ── Analytical Cube annotations ─────────────────────────────────────────── */
@Analytics.dataCategory: #CUBE
@Analytics.internalName: #LOCAL

/* ── OData exposure ──────────────────────────────────────────────────────── */
@OData.entityType.name: 'DPRProductionCubeType'

define view entity ZDPR_C_PROD_CUBE
  as select from ZDPR_I_DAILY as Daily

  /* Asset name text */
  left outer join zoiu_pr_dn as AssetTxt
    on Daily.Asset = AssetTxt.dn_no

  /* PI percentage — join on asset/block, filter in query */
  left outer join zpra_t_prd_pi as PI
    on  Daily.Asset           = PI.asset
    and Daily.Block           = PI.block
    and Daily.ProductionDate >= PI.vld_frm
    and Daily.ProductionDate <= PI.vld_to

{
  /* ── Dimensions ──────────────────────────────────────────────────────── */

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Production Date'
  key Daily.ProductionDate                        as ProductionDate,

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Calendar Year'
  key Daily.CalendarYear                          as CalendarYear,

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Calendar Month'
  key Daily.CalendarMonth                         as CalendarMonth,

  @AnalyticsDetails.query.axis: #ROWS
  @EndUserText.label: 'Product Code'
  @ObjectModel.text.element: ['ProductDescription']
  key Daily.Product                               as Product,

  @EndUserText.label: 'Product'
  Daily.ProductDescription                        as ProductDescription,

  @AnalyticsDetails.query.axis: #ROWS
  @EndUserText.label: 'Asset'
  @ObjectModel.text.element: ['AssetDescription']
  key Daily.Asset                                 as Asset,

  @EndUserText.label: 'Asset Description'
  AssetTxt.dn_de                                  as AssetDescription,

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Block'
  key Daily.Block                                 as Block,

  @AnalyticsDetails.query.axis: #FREE
  @EndUserText.label: 'Volume Type'
  @ObjectModel.text.element: ['VolumeTypeDescription']
  key Daily.VolumeType                            as VolumeType,

  @EndUserText.label: 'Volume Type Description'
  Daily.VolumeTypeDescription                     as VolumeTypeDescription,

  /* ── Measures (signed: gas GAS_INJ counts negative, so SUM() nets) ───── */

  @EndUserText.label: 'Production Qty (Primary UoM)'
  @Aggregation.default: #SUM
  @Semantics.quantity.unitOfMeasure: 'ProdUom1'
  cast( Daily.ProdQty1
        * ( case Daily.VolumeType
              when 'GAS_INJ' then ( cast( 0 as abap.dec( 2, 0 ) ) - cast( 1 as abap.dec( 2, 0 ) ) )
              else                cast(  1 as abap.dec( 2, 0 ) )
            end )
        as abap.dec( 23, 3 ) )                    as ProdQty1,

  @EndUserText.label: 'Primary UoM'
  Daily.ProdUom1                                  as ProdUom1,

  @EndUserText.label: 'Production Qty (Secondary UoM)'
  @Aggregation.default: #SUM
  @Semantics.quantity.unitOfMeasure: 'ProdUom2'
  cast( Daily.ProdQty2
        * ( case Daily.VolumeType
              when 'GAS_INJ' then ( cast( 0 as abap.dec( 2, 0 ) ) - cast( 1 as abap.dec( 2, 0 ) ) )
              else                cast(  1 as abap.dec( 2, 0 ) )
            end )
        as abap.dec( 23, 3 ) )                    as ProdQty2,

  @EndUserText.label: 'Secondary UoM'
  Daily.ProdUom2                                  as ProdUom2,

  /* ── PI (Participating Interest) ─────────────────────────────────────── */
  @EndUserText.label: 'Participating Interest %'
  @Aggregation.default: #NOP
  PI.pi                                           as ParticipatingInterest,

  /* OVL share = signed ProdQty1 × PI / 100 */
  @EndUserText.label: 'OVL Share Qty (Primary UoM)'
  @Aggregation.default: #SUM
  @Semantics.quantity.unitOfMeasure: 'ProdUom1'
  cast(
    Daily.ProdQty1
    * ( case Daily.VolumeType
          when 'GAS_INJ' then ( cast( 0 as abap.dec( 2, 0 ) ) - cast( 1 as abap.dec( 2, 0 ) ) )
          else                cast(  1 as abap.dec( 2, 0 ) )
        end )
    * PI.pi / cast( 100 as abap.dec(5,2) )
    as abap.dec(23,3)
  )                                               as OvlShareQty1,

  @EndUserText.label: 'OVL Share Qty (Secondary UoM)'
  @Aggregation.default: #SUM
  @Semantics.quantity.unitOfMeasure: 'ProdUom2'
  cast(
    Daily.ProdQty2
    * ( case Daily.VolumeType
          when 'GAS_INJ' then ( cast( 0 as abap.dec( 2, 0 ) ) - cast( 1 as abap.dec( 2, 0 ) ) )
          else                cast(  1 as abap.dec( 2, 0 ) )
        end )
    * PI.pi / cast( 100 as abap.dec(5,2) )
    as abap.dec(23,3)
  )                                               as OvlShareQty2,

  /* ── Gas in MMSCMD (port of convert_gas_units_to_mmscm, signed) ───────── */
  /* MCM -> as-is (MCM == MMSCM here), MCF -> /35.3, M3 -> /1,000,000.       */
  @EndUserText.label: 'Gas (MMSCMD)'
  @Aggregation.default: #SUM
  cast(
    ( case Daily.Product
        when '722000004' then
          case Daily.ProdUom1
            when 'MCF' then Daily.ProdQty1 / cast( '35.3' as abap.dec( 4, 1 ) )
            when 'M3'  then Daily.ProdQty1 / cast( 1000000 as abap.dec( 10, 0 ) )
            else            Daily.ProdQty1
          end
        else cast( 0 as abap.dec( 23, 7 ) )
      end )
    * ( case Daily.VolumeType
          when 'GAS_INJ' then ( cast( 0 as abap.dec( 2, 0 ) ) - cast( 1 as abap.dec( 2, 0 ) ) )
          else                cast(  1 as abap.dec( 2, 0 ) )
        end )
    as abap.dec( 23, 7 )
  )                                               as GasMmscmd,

  /* ── Total (O+OEG) BOEPD: oil/cond BOPD + gas MMSCMD * 6290, signed ───── */
  @EndUserText.label: 'Total O+OEG (BOEPD)'
  @Aggregation.default: #SUM
  cast(
    ( case Daily.Product
        when '722000004' then
          ( case Daily.ProdUom1
              when 'MCF' then Daily.ProdQty1 / cast( '35.3' as abap.dec( 4, 1 ) )
              when 'M3'  then Daily.ProdQty1 / cast( 1000000 as abap.dec( 10, 0 ) )
              else            Daily.ProdQty1
            end ) * cast( 6290 as abap.dec( 5, 0 ) )
        else Daily.ProdQty1
      end )
    * ( case Daily.VolumeType
          when 'GAS_INJ' then ( cast( 0 as abap.dec( 2, 0 ) ) - cast( 1 as abap.dec( 2, 0 ) ) )
          else                cast(  1 as abap.dec( 2, 0 ) )
        end )
    as abap.dec( 23, 3 )
  )                                               as BoepdQty
}
/* Oil family: NET_PROD. Gas has NO NET_PROD rows in ZPRA_T_DLY_PRD -
   net gas = GROSS_PROD - GAS_INJ, as in the classic DPR program. */
where
     (  Daily.Product <> '722000004'
    and Daily.VolumeType = 'NET_PROD' )
  or (  Daily.Product =  '722000004'
    and Daily.VolumeType = 'GROSS_PROD' )
  or (  Daily.Product =  '722000004'
    and Daily.VolumeType = 'GAS_INJ' )
