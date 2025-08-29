@Metadata.allowExtensions: true
@EndUserText.label: 'MIAS Claim Register'
@AccessControl.authorizationCheck: #CHECK
define root view entity ZC_GRC_MI_CLAIMREG
  provider contract transactional_query
  as projection on ZR_GRC_MI_CLAIMREG
{
  key     ClaimRegUuid,
   @ObjectModel.text.element:  ['RecType']
      @UI.textArrangement: #TEXT_ONLY
      @Consumption.valueHelpDefinition: [{ entity: { name: 'ZR_MIAS_RECTYPE_PL', element: 'Id' } }]
          recordtype,
          _RecType_PL.Record_Type as RecType,
          @UI.hidden: #(Hiderecordtypedesc)
          recordtypedesc,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual Hiderecordtypedesc             :abap_boolean,

          @UI.hidden: #(HideClaimRegExtId)
          ClaimRegExtId,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideClaimRegExtId              :abap_boolean,

          @UI.hidden: #(HideAssetLocation)
          AssetLocation,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideAssetLocation              :abap_boolean,

          @UI.hidden: #(HideBrandBu)
          @ObjectModel.text.element:  ['Brand_PL']
          @UI.textArrangement: #TEXT_ONLY
          @Consumption.valueHelpDefinition: [{ entity: { name: 'ZR_MIAS_BRAND_PL', element: 'Id' } }]
          BrandBu,
          _Brand_PL.Brand             as Brand_PL,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideBrandBu                    :abap_boolean,

          @UI.hidden: #(HideCloseDate)
          CloseDate,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideCloseDate                  :abap_boolean,

          @UI.hidden: #(HideDateOfLoss)
          DateOfLoss,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideDateOfLoss                 :abap_boolean,

          @UI.hidden: #(HideIbnerFormula)
          IbnerFormula,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideIbnerFormula               :abap_boolean,

          @UI.hidden: #(HideIbnerAdjustment)
          IbnerAdjustment,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideIbnerAdjustment            :abap_boolean,

          @UI.hidden: #(HideIbner)
          Ibner,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideIbner                      :abap_boolean,

          @UI.hidden: #(HideInsurerClaimNumber)
          InsurerClaimNumber,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideInsurerClaimNumber         :abap_boolean,

          @UI.hidden: #(HideInsurerClaimNumber2)
          InsurerClaimNumber2,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideInsurerClaimNumber2        :abap_boolean,

          @UI.hidden: #(HideLossDescription)
          LossDescription,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideLossDescription            :abap_boolean,

          @UI.hidden: #(HideMiasGrossIncurredFormula)
          MiasGrossIncurredFormula,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideMiasGrossIncurredFormula   :abap_boolean,

          @UI.hidden: #(HideMiasGrossIncurred)
          MiasGrossIncurred,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideMiasGrossIncurred          :abap_boolean,

          @UI.hidden: #(HideMiasPaid)
          MiasPaid,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideMiasPaid                   :abap_boolean,

          @UI.hidden: #(HideMiasGrossReservedExclIbner)
          MiasGrossReservedExclIbner,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideMiasGrossReservedExclIbner :abap_boolean,

          @UI.hidden: #(HideMiasNetReservedExclIbner)
          MiasNetReservedExclIbner,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideMiasNetReservedExclIbner   :abap_boolean,

          @UI.hidden: #(HideMiasNetReservedInclIbnerF)
          MiasNetReservedInclIbnerF,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideMiasNetReservedInclIbnerF  :abap_boolean,

          @UI.hidden: #(HideMiasPolicyYear)
          MiasPolicyYear,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideMiasPolicyYear             :abap_boolean,

          @UI.hidden: #(HideMiasShare)
          MiasShare,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideMiasShare                  :abap_boolean,

          @UI.hidden: #(HideSubtype)
          @ObjectModel.text.element:  ['MIASProgram']
          @UI.textArrangement: #TEXT_ONLY
          @Consumption.valueHelpDefinition: [{ entity: { name: 'ZR_MIAS_PROG_PL', element: 'Id' } }]
          Subtype,
          _Program_PL.MIASProgram         as MIASProgram,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideSubtype                    :abap_boolean,

          @UI.hidden: #(HideDescription)
          @ObjectModel.text.element:  ['Desc_PL']
          @UI.textArrangement: #TEXT_ONLY
          @Consumption.valueHelpDefinition: [{ entity: { name: 'ZR_MIAS_PROGDESC_PL', element: 'Id' } }]
          Description,
          _Description_PL.Description as Desc_PL,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideDescription                :abap_boolean,

          @UI.hidden: #(HideRI1stBracket)
          RI1stBracket,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideRI1stBracket               :abap_boolean,

          @UI.hidden: #(HideRITotalRecieved)
          RITotalRecieved,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideRITotalRecieved            :abap_boolean,

          @UI.hidden: #(HideRITotalShare)
          RITotalShare,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideRITotalShare               :abap_boolean,

          @UI.hidden: #(HideRIShareOfGrossReserve)
          RIShareOfGrossReserve,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideRIShareOfGrossReserve      :abap_boolean,

          @UI.hidden: #(HideReinsuranceReceivables)
          ReinsuranceReceivables,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideReinsuranceReceivables     :abap_boolean,

          @UI.hidden: #(HideStatus)
          @ObjectModel.text.element:  ['Status_PL']
          @UI.textArrangement: #TEXT_ONLY
          @Consumption.valueHelpDefinition: [{ entity: { name: 'ZR_MIAS_STATUS_PL', element: 'Id' } }]
          Status,
          _Status_PL.Status           as Status_PL,
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:ZCL_GRC_MIAS_CONTROL_FIELDS'
  virtual HideStatus                     :abap_boolean,

          Createdby,
          Createdat,
          Lastchangedby,
          Lastchangedat,
          _claim_transaction : redirected to composition child ZC_GRC_MI_CLAIMTRN,
          _Brand_PL,
          _Description_PL,
          _Status_PL,
          _RecType_PL,
          _Program_PL
}
