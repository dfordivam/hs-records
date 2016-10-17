module Handler.AppointmentSpec (spec) where

import TestImport
import CustomDBDataTypes
import Handler.TestUtils

spec :: Spec
spec = withApp $ do
  it "loads a form to add a new appointment" $ do
    -- Init
    runDB $ deleteWhere ([] :: [Filter Appointment])
    runDB $ deleteWhere ([] :: [Filter User])

    addPatients 1
    addPhysicians 1

    Just (Entity objId _ ) <- runDB $ selectFirst ([] :: [Filter Patient]) []

    -- Test start

    get $ AddAppointmentR objId
    statusIs 303

    withRestrictedUserLogin $ do
    get $ AddAppointmentR objId
    statusIs 200
    
    request $ do
      setMethod "POST"
      setUrl $ AddAppointmentR objId
      addToken

      byLabel "Physician" "1"
      byLabel "Diagnosis" "Some Diagnosis"
      byLabel "Comments" "Some Comments"

    statusIs 200

    (Entity _ obj3:_) <- runDB $ selectList ([] :: [Filter Appointment]) []
    assertEqual "Should have patient info " (appointmentPatient obj3) objId
