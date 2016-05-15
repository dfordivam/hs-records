module Handler.PatientSpec (spec) where

import TestImport
import CustomDBDataTypes

spec :: Spec
spec = withApp $ do
  it "loads a form to add a new patient record" $ do
    get AddPatientR
    statusIs 200
    -- htmlAllContain "h1" "Add new physician information"

    let name = "Patient's Name"
        gender = Male

    request $ do
      setMethod "POST"
      setUrl AddPatientR
      addToken

      byLabel "Name" name
      byLabel "Gender" "2"

    -- printBody
    statusIs 200

    (Entity _id obj:_) <- runDB $ selectList [PatientName ==. name] []
    assertEqual "Should have " obj 
      (Patient name gender Nothing Nothing Nothing Nothing Nothing)

