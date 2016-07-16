module Handler.Nurse where

import Import
import CustomDBDataTypes
import Handler.FormUtils

nurseForm :: Maybe Nurse -> Form Nurse
nurseForm inp = renderDivs $ Nurse
  <$> areq textField "Name" (nurseName <$> inp)
  <*> areq (selectField optionsEnum) "Gender" (Just Female)
  <*> aopt textField "Position" (nursePosition <$> inp)
  <*> aopt textareaField "Remarks" (nurseRemarks <$> inp)
  <*> areq (selectField optionsEnum) "Active" (nurseActive <$> inp)

getAddNurseR :: Handler Html
getAddNurseR = getAddRecordForm (nurseForm Nothing) AddNurseR

postAddNurseR :: Handler Html
postAddNurseR = postAddRecordForm Nothing (nurseForm Nothing) AddNurseR

perPage = 10

getListNurseR :: Handler Html
getListNurseR = redirect $ ListNursePageR 1

getListNursePageR :: Integer -> Handler Html
getListNursePageR pageNumber = do
  dbData <- runDB $ selectList ([] :: [Filter Nurse])
    [ Asc NurseName
    , LimitTo perPage
    , OffsetBy $ ((fromIntegral pageNumber) - 1) * perPage]

  defaultLayout
    [whamlet|
      <ul>
        $forall Entity objId objData <- dbData
          <li>#{nurseName objData}
      <div>
        <p>
          <a href=@{AddNurseR}>Add new nurse record
    |]

getEditNurseR :: NurseId -> Handler Html
getEditNurseR idVal = do
  val <- runDB $ get $ idVal
  getAddRecordForm (nurseForm val) (EditNurseR idVal)

postEditNurseR :: NurseId -> Handler Html
postEditNurseR idVal =
  postAddRecordForm (Just idVal) (nurseForm Nothing) (EditNurseR idVal)

getNurseR :: NurseId -> Handler Html
getNurseAppointmentsR :: NurseId -> Handler Html
getNurseAdmissionsR :: NurseId -> Handler Html

getNurseR = undefined
getNurseAppointmentsR = undefined
getNurseAdmissionsR = undefined
