module Handler.Physician where

import Import
import CustomDBDataTypes
import Handler.FormUtils

physicianForm :: Maybe Physician -> Form Physician
physicianForm inp = renderDivs $ Physician
  <$> areq textField "Name" (physicianName <$> inp)
  <*> areq (selectField optionsEnum) "Gender" (physicianGender <$> inp)
  <*> areq textField "Position" (physicianPosition <$> inp)
  <*> aopt textareaField "Remarks" (physicianRemarks <$> inp)

getAddPhysicianR :: Handler Html
getAddPhysicianR = getAddRecordForm (physicianForm Nothing) AddPhysicianR

postAddPhysicianR :: Handler Html
postAddPhysicianR = postAddRecordForm Nothing (physicianForm Nothing) AddPhysicianR

perPage = 10

getListPhysicianR :: Handler Html
getListPhysicianR = redirect $ ListPhysicianPageR 1

getListPhysicianPageR :: Integer -> Handler Html
getListPhysicianPageR pageNumber = do
  dbData <- runDB $ selectList ([] :: [Filter Physician])
    [ Asc PhysicianName
    , LimitTo perPage
    , OffsetBy $ ((fromIntegral pageNumber) - 1) * perPage]

  defaultLayout
    [whamlet|
      <ul>
        $forall Entity physId physician <- dbData
          <li>#{physicianName physician}
          <li>
            <a href=@{EditPhysicianR physId}>edit
      <div>
        <p>
          <a href=@{AddPhysicianR}>Add new physician
    |]

getEditPhysicianR :: PhysicianId -> Handler Html
getEditPhysicianR idVal = do
  val <- runDB $ get $ idVal
  getAddRecordForm (physicianForm val) (EditPhysicianR idVal)

postEditPhysicianR :: PhysicianId -> Handler Html
postEditPhysicianR idVal = 
  postAddRecordForm (Just idVal) (physicianForm Nothing) (EditPhysicianR idVal)

getPhysicianR :: PhysicianId -> Handler Html
getPhysicianAppointmentsR :: PhysicianId -> Handler Html
getPhysicianAdmissionsR :: PhysicianId -> Handler Html

getPhysicianR = undefined
getPhysicianAppointmentsR = undefined
getPhysicianAdmissionsR = undefined
