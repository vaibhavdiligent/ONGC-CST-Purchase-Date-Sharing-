@EndUserText.label: 'DPR Production Download Parameters'
define abstract entity ZDPR_A_PROD_PARAM
{
  @EndUserText.label: 'Date From'
  date_from : abap.dats;

  @EndUserText.label: 'Date To'
  date_to   : abap.dats;
}
