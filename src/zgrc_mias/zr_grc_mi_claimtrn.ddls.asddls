@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@EndUserText.label: 'Claim Transaction'
define view entity ZR_GRC_MI_CLAIMTRN
  as select from zgrc_mi_claimtrn as claim_transaction
  association to parent ZR_GRC_MI_CLAIMREG as _claim_register on $projection.ClaimRegUuid = _claim_register.ClaimRegUuid
  association[0..1] to ZR_MIAS_TYPE_PL as _Type_PL on
  $projection.Type = _Type_PL.Id
{
  key claim_reg_uuid     as ClaimRegUuid,
  key claim_tran_uuid    as ClaimTranUuid,
      claim_tran_ext_id  as ClaimTranExtId,
      accounting_date    as AccountingDate,
      accounting_month   as AccountingMonth,
      accounting_quarter as AccountingQuarter,
      amount             as Amount,
      r_i_1_comment      as RI1Comment,
      mias_r_i_y_n       as MiasRIYN,
      subtype            as Subtype,
      description        as Description,
      r_i_amount_1       as RIAmount1,
      r_i_amount_2       as RIAmount2,
      r_i_amount_3       as RIAmount3,
      r_i_amount_4       as RIAmount4,
      r_i_amount_5       as RIAmount5,
      r_i_percent_1      as RIPercent1,
      r_i_percent_2      as RIPercent2,
      r_i_percent_3      as RIPercent3,
      r_i_percent_4      as RIPercent4,
      r_i_percent_5      as RIPercent5,
      r_i_total_amount   as RITotalAmount,
      reinsurance_share  as ReinsuranceShare,
      type               as Type,
      @Semantics.user.createdBy: true
      createdby          as Createdby,
      @Semantics.systemDateTime.createdAt: true
      createdat          as Createdat,
      @Semantics.user.localInstanceLastChangedBy: true
      lastchangedby      as Lastchangedby,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      lastchangedat      as Lastchangedat,
      _claim_register,
      _Type_PL

}
