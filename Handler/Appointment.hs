module Handler.Appointment where

import Import
import CustomDBDataTypes
import Handler.FormUtils

appointmentStartTimeFormTag :: Text
appointmentStartTimeFormTag = "appointmentStartTimeFormTag"

appointmentForm :: Maybe Appointment ->
  PatientId -> [(Text, ProfessionalId)] -> [(Text, ProfessionalId)] -> UTCTime
  -> Form Appointment
appointmentForm inp patId physicianList nurseList startTime =
  renderDivs $ Appointment
  <$> pure patId
  <*> areq (selectFieldList physicianList) "Physician"
    (appointmentPhysician <$> inp)
  <*> aopt (selectFieldList nurseList) "Nurse"
    (appointmentNurse <$> inp)
  <*> areq textareaField "Diagnosis"
    (appointmentDiagnosis <$> inp)
  <*> areq textareaField "Comments"
    (appointmentComments <$> inp)
  <*> pure startTime
  --  (appointmentStartTime <$> inp)
  <*> pure Nothing
  -- <*> aopt textField "End Time"
  --  (appointmentEndTime <$> inp)

getAddAppointmentR :: PatientId -> Handler Html
getAddAppointmentR patId = do
  physList <- runDB $ selectList ([ProfessionalRole ==. Physician] :: [Filter Professional]) 
    [Asc ProfessionalName]
  nurseList <- runDB $ selectList ([ProfessionalRole ==. Nurse] :: [Filter Professional])
    [Asc ProfessionalName]

  curTime <- lift (liftIO getCurrentTime)
  let physNameList = map 
        (\(Entity objId obj) -> (professionalName obj, objId)) physList
      nurseNameList = map
        (\(Entity objId obj) -> (professionalName obj, objId)) nurseList 

  -- Generate the form to be displayed
  (widget, enctype) <- generateFormPost
    (appointmentForm Nothing patId physNameList nurseNameList curTime)

  let addRoute = (AddAppointmentR patId)

  defaultLayout $ do
    aDomId <- newIdent
    $(widgetFile "add-appointment")

postAddAppointmentR :: PatientId -> Handler Html
postAddAppointmentR patId = do
  stime <- lookupPostParam appointmentStartTimeFormTag

  curTime <- lift (liftIO getCurrentTime)

  let startTime = fromMaybe curTime $
        join (parseTime defaultTimeLocale "%FT%R" <$>
          (unpack <$> stime))

  physList <- runDB $ selectList ([ProfessionalRole ==. Physician] :: [Filter Professional]) 
    [Asc ProfessionalName]
  nurseList <- runDB $ selectList ([ProfessionalRole ==. Nurse] :: [Filter Professional])
    [Asc ProfessionalName]

  curTime <- lift (liftIO getCurrentTime)
  let physNameList = map 
        (\(Entity objId obj) -> (professionalName obj, objId)) physList
      nurseNameList = map
        (\(Entity objId obj) -> (professionalName obj, objId)) nurseList 

  postAddRecordForm Nothing 
    (appointmentForm Nothing patId physNameList nurseNameList startTime)
    (AddAppointmentR patId)

getListAppointmentR :: Handler Html
getListAppointmentPageR :: Integer -> Handler Html
getListUpcomingAppointmentR :: Handler Html
getListUpcomingAppointmentPageR :: Integer -> Handler Html
getAppointmentR :: AppointmentId -> Handler Html
getEditAppointmentR :: AppointmentId -> Handler Html
postEditAppointmentR :: AppointmentId -> Handler Html

getListAppointmentR = undefined
getListAppointmentPageR = undefined
getListUpcomingAppointmentR = undefined
getListUpcomingAppointmentPageR = undefined
getAppointmentR = undefined
getEditAppointmentR = undefined
postEditAppointmentR = undefined
