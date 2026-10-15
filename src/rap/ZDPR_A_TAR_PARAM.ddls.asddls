@EndUserText.label: 'DPR Target Download - Input Parameters'
define abstract entity ZDPR_A_TAR_PARAM
{
  @EndUserText.label: 'Fiscal Year'
  fiscal_year  : abap.numc(4);

  @EndUserText.label: 'Target Code'
  target_code  : abap.char(10);
}
