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

getListPhysicianR :: Handler Html
getListPhysicianR = do
  dbData <- runDB $ selectList ([] :: [Filter Physician]) [LimitTo 10]

  defaultLayout
    [whamlet|
      <ul>
        $forall Entity physId physician <- dbData
          <li>#{physicianName physician}
    |]

getListPhysicianPageR :: Integer -> Handler Html
getPhysicianR :: PhysicianId -> Handler Html
getPhysicianAppointmentsR :: PhysicianId -> Handler Html
getPhysicianAdmissionsR :: PhysicianId -> Handler Html
getEditPhysicianR :: PhysicianId -> Handler Html
postEditPhysicianR :: PhysicianId -> Handler Html

getListPhysicianPageR = undefined
getPhysicianR = undefined
getPhysicianAppointmentsR = undefined
getPhysicianAdmissionsR = undefined
getEditPhysicianR = undefined
postEditPhysicianR = undefined
