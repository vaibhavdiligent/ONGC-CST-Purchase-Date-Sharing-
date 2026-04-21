@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DPR Production Targets - Interface View'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType: {
  serviceQuality: #A,
  sizeCategory:   #M,
  dataClass:      #MASTER
}

define view entity ZPRA_I_DPR_TARGET
  as select from zpra_t_prd_tar as PrdTar

  association [0..1] to zoiu_pr_dn as _AssetText
    on $projection.Asset = _AssetText.dn_no

{
  key PrdTar.tar_code                     as TargetCode,
  key PrdTar.gjahr                        as FiscalYear,
  key PrdTar.monat                        as FiscalPeriod,
  key PrdTar.asset                        as Asset,
  key PrdTar.block                        as Block,
  key PrdTar.product                      as Product,
  key PrdTar.prod_vl_type_cd              as VolumeType,

      @Semantics.quantity.unitOfMeasure: 'TargetUom'
      PrdTar.tar_qty                      as TargetQty,
      @Semantics.unitOfMeasure: true
      PrdTar.uom                          as TargetUom,

      @Semantics.quantity.unitOfMeasure: 'TargetUom'
      PrdTar.tar_qty2                     as TargetQty2,

      /* Target type description */
      case PrdTar.tar_code
        when 'TAR_BE' then 'Budget Estimate'
        when 'TAR_IN' then 'Internal Target'
        when 'TAR_EX' then 'MOU Excellent'
        when 'TAR_VG' then 'MOU Very Good'
        when 'TAR_PC' then 'Physical Control'
        when 'TAR_RE' then 'Revised Estimate'
        else PrdTar.tar_code
      end                                 as TargetTypeDescription,

      case PrdTar.product
        when '722000001' then 'Oil'
        when '722000003' then 'Condensate'
        when '722000004' then 'Gas'
        when '722000005' then 'LNG'
        else                  'Other'
      end                                 as ProductDescription,

      _AssetText
}
