@AccessControl.authorizationCheck: #CHECK
@Metadata.allowExtensions: true
@EndUserText.label: '###GENERATED Core Data Service Entity'
@ObjectModel.sapObjectNodeType.name: 'ZGRC_MI_CLAIMPCT'
define root view entity ZR_GRC_MI_CLAIMPCT
  as select from zgrc_mi_claimpct
  
   association[0..1] to ZR_MIAS_PROG_PL as _Program_PL on
  $projection.ProgramName = _Program_PL.Id
   association[0..1] to ZR_MIAS_PROGDESC_PL as _Description_PL on
  $projection.ProgramDesc = _Description_PL.Id
  association[0..1] to ZR_GRC_USER_MASTER as _Owner
    on $projection.Owner = _Owner.UserID
{
  key claim_pct_uuid as ClaimPctUuid,
  program_name as ProgramName,
  program_desc as ProgramDesc,
  program_year as ProgramYear,
  percentage as Percentage,
  owner as Owner,
  start_date as StartDate,
  end_date as EndDate,
  @Semantics.user.createdBy: true
  createdby as Createdby,
  @Semantics.systemDateTime.createdAt: true
  createdat as Createdat,
  @Semantics.user.localInstanceLastChangedBy: true
  lastchangedby as Lastchangedby,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  lastchangedat as Lastchangedat,
  _Program_PL,
  _Description_PL,
  _Owner
  
}
