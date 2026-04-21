@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Monthly Reconciled Production - Interface View'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType: {
  serviceQuality: #A,
  sizeCategory:   #L,
  dataClass:      #TRANSACTIONAL
}

define view entity ZPRA_I_DPR_MONTHLY
  as select from zpra_t_mrec_prd as MrecPrd

  association [0..1] to zoiu_pr_dn as _AssetText
    on $projection.Asset = _AssetText.dn_no

{
  key MrecPrd.gjahr                       as FiscalYear,
  key MrecPrd.monat                       as FiscalPeriod,
  key MrecPrd.asset                       as Asset,
  key MrecPrd.block                       as Block,
  key MrecPrd.product                     as Product,
  key MrecPrd.prd_vl_type                 as VolumeType,

      @Semantics.quantity.unitOfMeasure: 'ProdUom1'
      MrecPrd.prod_vl_qty1                as ProdQty1,
      @Semantics.unitOfMeasure: true
      MrecPrd.prod_vl_uom1                as ProdUom1,

      @Semantics.quantity.unitOfMeasure: 'ProdUom2'
      MrecPrd.prod_vl_qty2                as ProdQty2,
      @Semantics.unitOfMeasure: true
      MrecPrd.prod_vl_uom2                as ProdUom2,

      case MrecPrd.product
        when '722000001' then 'Oil'
        when '722000003' then 'Condensate'
        when '722000004' then 'Gas'
        when '722000005' then 'LNG'
        else                  'Other'
      end                                 as ProductDescription,

      case MrecPrd.prd_vl_type
        when 'NET_PROD'   then 'Net Production'
        when 'GROSS_PROD' then 'Gross Production'
        when 'GAS_INJ'    then 'Gas Injection'
        else MrecPrd.prd_vl_type
      end                                 as VolumeTypeDescription,

      _AssetText
}
