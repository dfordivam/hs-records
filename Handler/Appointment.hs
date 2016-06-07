module Handler.Appointment where

import Import
import CustomDBDataTypes
import Handler.FormUtils

-- appointmentForm :: PatientId -> Form Appointment
-- appointmentForm patId = renderDivs $ Appointment
--   <$> areq textField "Name" Nothing
--   <*> areq (selectField optionsEnum) "Gender" (Just Male)
--   <*> aopt intField "Phone" Nothing
--   <*> aopt intField "Phone2" Nothing
--   <*> aopt emailField "Email" Nothing
--   <*> aopt textareaField "Address" Nothing
--   <*> aopt textareaField "Remarks" Nothing

getAddAppointmentR :: PatientId -> Handler Html
getAddAppointmentR patId = do
  print patId
  defaultLayout $ return ()

postAddAppointmentR :: PatientId -> Handler Html
postAddAppointmentR = undefined 

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
