@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Daily Production - Interface View'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType: {
  serviceQuality: #A,
  sizeCategory:   #L,
  dataClass:      #TRANSACTIONAL
}

define view entity ZPRA_I_DPR_DAILY
  as select from zpra_t_dly_prd as DlyPrd

  /* Asset description */
  association [0..1] to zoiu_pr_dn as _AssetText
    on $projection.Asset = _AssetText.dn_no

  /* PI validity for the production date */
  association [0..*] to zpra_t_prd_pi as _PrdPI
    on  $projection.Asset >= _PrdPI.asset
    and $projection.Block >= _PrdPI.block

  /* DPR profile config */
  association [0..1] to zpra_c_dpr_prof as _DprProf
    on  $projection.Product = _DprProf.product
    and $projection.Asset   = _DprProf.asset

{
  key DlyPrd.production_date              as ProductionDate,
  key DlyPrd.product                      as Product,
  key DlyPrd.asset                        as Asset,
  key DlyPrd.block                        as Block,
  key DlyPrd.prd_vl_type                  as VolumeType,

      /* Quantities */
      @Semantics.quantity.unitOfMeasure: 'ProdUom1'
      DlyPrd.prod_vl_qty1                 as ProdQty1,
      @Semantics.unitOfMeasure: true
      DlyPrd.prod_vl_uom1                 as ProdUom1,

      @Semantics.quantity.unitOfMeasure: 'ProdUom2'
      DlyPrd.prod_vl_qty2                 as ProdQty2,
      @Semantics.unitOfMeasure: true
      DlyPrd.prod_vl_uom2                 as ProdUom2,

      /* Calendar derivations */
      year(  DlyPrd.production_date )     as CalendarYear,
      month( DlyPrd.production_date )     as CalendarMonth,

      /* Product description (inline) */
      case DlyPrd.product
        when '722000001' then 'Oil'
        when '722000003' then 'Condensate'
        when '722000004' then 'Gas'
        when '722000005' then 'LNG'
        else                  'Other'
      end                                 as ProductDescription,

      /* Volume type description */
      case DlyPrd.prd_vl_type
        when 'NET_PROD'   then 'Net Production'
        when 'GROSS_PROD' then 'Gross Production'
        when 'GAS_INJ'    then 'Gas Injection'
        else DlyPrd.prd_vl_type
      end                                 as VolumeTypeDescription,

      /* Associations */
      _AssetText,
      _PrdPI,
      _DprProf
}
