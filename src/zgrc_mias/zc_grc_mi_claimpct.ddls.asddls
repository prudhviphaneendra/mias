@Metadata.allowExtensions: true
@EndUserText.label: 'MIAS Claim Percentages Application'
@AccessControl.authorizationCheck: #CHECK
@ObjectModel.sapObjectNodeType.name: 'ZGRC_MI_CLAIMPCT'
define root view entity ZC_GRC_MI_CLAIMPCT
  provider contract transactional_query
  as projection on ZR_GRC_MI_CLAIMPCT
{
  key ClaimPctUuid,
  @ObjectModel.text.element:  ['MIASProgram']
      @UI.textArrangement: #TEXT_ONLY
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZR_MIAS_PROG_PL', element: 'Id' } }]
  ProgramName,
  _Program_PL.MIASProgram as MIASProgram,
   @ObjectModel.text.element:  ['Description']
      @UI.textArrangement: #TEXT_ONLY
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZR_MIAS_PROGDESC_PL', element: 'Id' } }]
  ProgramDesc,
  _Description_PL.Description as Description,
  ProgramYear,
  Percentage,
   @ObjectModel.text.element:  ['OwnerName']
      @UI.textArrangement: #TEXT_ONLY
     @Consumption.valueHelpDefinition: [{ entity: { name: 'ZR_GRC_USER_MASTER', element: 'UserID' } }]
  Owner,
  _Owner.UserDescription as OwnerName,
  StartDate,
  EndDate,
  Createdby,
  Createdat,
  Lastchangedby,
  Lastchangedat,
    _Program_PL,
  _Description_PL,
    _Owner
  
}
