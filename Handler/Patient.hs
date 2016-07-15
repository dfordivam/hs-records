module Handler.Patient where

import Import
import CustomDBDataTypes
import Handler.FormUtils
import Database.Persist.MongoDB
-- import System.Random
-- import qualified Data.Vector as V

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
postAddPatientR = postAddRecordForm Nothing patientForm AddPatientR

getPatientR :: PatientId -> Handler Html
getPatientAppointmentsR :: PatientId -> Handler Html
getPatientAdmissionsR :: PatientId -> Handler Html
getEditPatientR :: PatientId -> Handler Html
postEditPatientR :: PatientId -> Handler Html

perPage = 20

-- addRandomNames :: Int -> Handler ()
-- addRandomNames count = do
--   lastNames <- liftIO $ readFile "CSV_Database_of_Last_Names.csv"
--   firstNames <- liftIO $ readFile "CSV_Database_of_First_Names.csv"
--
--   let lastNameVec = V.fromList $ lines lastNames
--       firstNameVec = V.fromList $ lines firstNames
--       ln = length lastNameVec
--       fn = length firstNameVec
--
--       addOne :: Handler ()
--       addOne = do
--
--         i1 <- liftIO $ randomRIO (0, ln - 1)
--         i2 <- liftIO $ randomRIO (0, fn - 1)
--
--         let patName = pack $ firstName ++ " " ++ lastName
--             lastName = lastNameVec V.! i1
--             firstName = firstNameVec V.! i2
--             gender = if even i1 then Male else Female
--             pat = Patient patName gender
--               Nothing Nothing Nothing Nothing Nothing
--         runDB $ insert pat
--         return ()
--
--   replicateM_ count addOne
--
--   return ()

getListPatientR :: Handler Html
getListPatientR = redirect $ ListPatientPageR 1

getListPatientPageR :: Integer -> Handler Html
getListPatientPageR pageNumber = do
  dbData <- runDB $ selectList ([] :: [Filter Patient])
    [ Asc PatientName
    , LimitTo perPage
    , OffsetBy $ ((fromIntegral pageNumber) - 1) * perPage]

  defaultLayout
    [whamlet|
      <ul>
        $forall Entity patId patData <- dbData
          <li>#{patientName patData}
      <div>
        <p>
          <a href=@{AddPatientR}>Add new patient record
    |]

getPatientSearchR :: Handler Html
getPatientSearchR = do
  maybeName <- lookupGetParam "name"

  dbData <- case maybeName of
    Just name ->
      runDB $ selectList ([PatientName =~. (name, "i")] :: [Filter Patient])
        [ Asc PatientName]
    Nothing -> pure []

  defaultLayout
    [whamlet|
      <form method=get action=@{PatientSearchR}>
        <input type="text" name="name" >
        <button>Submit
        <p>
      <ul>
        $forall Entity patId patData <- dbData
          <li>#{patientName patData}
    |]

getPatientR = undefined
getPatientAppointmentsR = undefined
getPatientAdmissionsR = undefined
getEditPatientR = undefined
postEditPatientR = undefined
