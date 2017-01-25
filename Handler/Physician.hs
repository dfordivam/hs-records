{-# LANGUAGE RankNTypes #-}
module Handler.Physician where

import Import
import CustomDBDataTypes
import Handler.FormUtils

getAddPhysicianR :: Handler Html
getAddPhysicianR = getAddRecordForm (physicianForm Nothing) AddPhysicianR

postAddPhysicianR :: Handler Html
postAddPhysicianR = postAddRecordForm Nothing (physicianForm Nothing) AddPhysicianR

perPage :: forall a. Num a => a
perPage = 10

getListPhysicianR :: Handler Html
getListPhysicianR = redirect $ ListPhysicianPageR 1

getListPhysicianPageR :: Integer -> Handler Html
getListPhysicianPageR pageNumber = do
  dbData <- runDB $ selectList ([ProfessionalRole ==. Physician] :: [Filter Professional])
    [ Asc ProfessionalName
    , LimitTo perPage
    , OffsetBy $ ((fromIntegral pageNumber) - 1) * perPage]

  defaultLayout
    [whamlet|
      <ul>
        $forall Entity physId physician <- dbData
          <li>#{professionalName physician}
          <li>
            <a href=@{EditPhysicianR physId}>edit
      <div>
        <p>
          <a href=@{AddPhysicianR}>Add new physician
    |]

getEditPhysicianR :: ProfessionalId -> Handler Html
getEditPhysicianR idVal = do
  val <- runDB $ get $ idVal
  getAddRecordForm (physicianForm val) (EditPhysicianR idVal)

postEditPhysicianR :: ProfessionalId -> Handler Html
postEditPhysicianR idVal =
  postAddRecordForm (Just idVal) (physicianForm Nothing) (EditPhysicianR idVal)

getPhysicianR :: ProfessionalId -> Handler Html
getPhysicianAppointmentsR :: ProfessionalId -> Handler Html
getPhysicianAdmissionsR :: ProfessionalId -> Handler Html

getPhysicianR = undefined
getPhysicianAppointmentsR = undefined
getPhysicianAdmissionsR = undefined
