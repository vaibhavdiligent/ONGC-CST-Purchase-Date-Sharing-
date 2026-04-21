@EndUserText.label: 'DPR Production Download - Input Parameters'
@MappingRole: true

define abstract entity ZPRA_A_DPR_PROD_PARAM
{
  @EndUserText.label: 'Date From'
  date_from : abap.dats;

  @EndUserText.label: 'Date To'
  date_to   : abap.dats;
}
