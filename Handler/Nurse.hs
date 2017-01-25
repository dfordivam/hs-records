module Handler.Nurse where

import Import
import CustomDBDataTypes
import Handler.FormUtils

getAddNurseR :: Handler Html
getAddNurseR = getAddRecordForm (nurseForm Nothing) AddNurseR

postAddNurseR :: Handler Html
postAddNurseR = postAddRecordForm Nothing (nurseForm Nothing) AddNurseR

perPage = 10

getListNurseR :: Handler Html
getListNurseR = redirect $ ListNursePageR 1

getListNursePageR :: Integer -> Handler Html
getListNursePageR pageNumber = do
  dbData <- runDB $ selectList ([ProfessionalRole ==. Nurse] :: [Filter Professional])
    [ Asc ProfessionalName
    , LimitTo perPage
    , OffsetBy $ ((fromIntegral pageNumber) - 1) * perPage]

  defaultLayout
    [whamlet|
      <ul>
        $forall Entity objId objData <- dbData
          <li>#{professionalName objData}
      <div>
        <p>
          <a href=@{AddNurseR}>Add new nurse record
    |]

getEditNurseR :: ProfessionalId -> Handler Html
getEditNurseR idVal = do
  val <- runDB $ get $ idVal
  getAddRecordForm (nurseForm val) (EditNurseR idVal)

postEditNurseR :: ProfessionalId -> Handler Html
postEditNurseR idVal =
  postAddRecordForm (Just idVal) (nurseForm Nothing) (EditNurseR idVal)

getNurseR :: ProfessionalId -> Handler Html
getNurseAppointmentsR :: ProfessionalId -> Handler Html
getNurseAdmissionsR :: ProfessionalId -> Handler Html

getNurseR = undefined
getNurseAppointmentsR = undefined
getNurseAdmissionsR = undefined
