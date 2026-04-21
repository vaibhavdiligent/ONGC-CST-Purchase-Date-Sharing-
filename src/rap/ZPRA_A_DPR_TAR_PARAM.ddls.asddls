@EndUserText.label: 'DPR Target Download - Input Parameters'
@MappingRole: true

define abstract entity ZPRA_A_DPR_TAR_PARAM
{
  @EndUserText.label: 'Fiscal Year'
  fiscal_year  : abap.numc(4);

  @EndUserText.label: 'Target Code'
  @Consumption.valueHelp: '_TargetCodeVH'
  target_code  : abap.char(10);
}
