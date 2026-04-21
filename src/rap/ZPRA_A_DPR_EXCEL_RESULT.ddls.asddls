@EndUserText.label: 'DPR Excel Download - Action Result'
@MappingRole: true

define abstract entity ZPRA_A_DPR_EXCEL_RESULT
{
  @EndUserText.label: 'Excel File (Base64 encoded)'
  excel_base64 : abap.string(0);

  @EndUserText.label: 'File Name'
  file_name    : abap.char(100);

  @EndUserText.label: 'MIME Type'
  mime_type    : abap.char(100);

  @EndUserText.label: 'Message'
  message      : abap.char(200);
}
