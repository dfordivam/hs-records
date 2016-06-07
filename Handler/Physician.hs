module Handler.Physician where

import Import
import CustomDBDataTypes
import Handler.FormUtils

physicianForm :: Form Physician
physicianForm = renderDivs $ Physician
  <$> areq textField "Name" Nothing
  <*> areq (selectField optionsEnum) "Gender" (Just Male)
  <*> areq textField "Position" Nothing
  <*> aopt textareaField "Remarks" Nothing

getAddPhysicianR :: Handler Html
getAddPhysicianR = getAddRecordForm physicianForm AddPhysicianR

postAddPhysicianR :: Handler Html
postAddPhysicianR = postAddRecordForm physicianForm AddPhysicianR

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
      <div>
        <p>
          <a href=@{AddPhysicianR}>Add new physician
    |]

getPhysicianR :: PhysicianId -> Handler Html
getPhysicianAppointmentsR :: PhysicianId -> Handler Html
getPhysicianAdmissionsR :: PhysicianId -> Handler Html
getEditPhysicianR :: PhysicianId -> Handler Html
postEditPhysicianR :: PhysicianId -> Handler Html

getPhysicianR = undefined
getPhysicianAppointmentsR = undefined
getPhysicianAdmissionsR = undefined
getEditPhysicianR = undefined
postEditPhysicianR = undefined
