module Handler.Patient where

import Import
import CustomDBDataTypes
import Handler.FormUtils

patientForm :: Form Patient
patientForm = renderDivs $ Patient
  <$> areq textField "Name" Nothing
  <*> areq (selectField optionsEnum) "Gender" (Just Male)
  <*> aopt intField "Phone" Nothing
  <*> aopt intField "Phone2" Nothing
  <*> aopt emailField "Email" Nothing
  <*> aopt textareaField "Address" Nothing
  <*> aopt textareaField "Remarks" Nothing

getAddPatientR :: Handler Html
getAddPatientR = getAddRecordForm patientForm AddPatientR

postAddPatientR :: Handler Html
postAddPatientR = postAddRecordForm patientForm AddPatientR