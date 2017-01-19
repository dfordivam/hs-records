module Handler.PatientSpec (spec) where

import TestImport
import CustomDBDataTypes
import Handler.TestUtils

spec :: Spec
spec = withApp $ do
  it "loads a form to add a new patient record" $ do
    get AddPatientR
    statusIs 303

    withRestrictedUserLogin ( do
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
    )

  it "get list of patients" $ do
    runDB $ deleteWhere ([] :: [Filter Patient])
    addPatients 25

    withRestrictedUserLogin ( do
    get ListPatientR
    followRedirect
    statusIs 200

    -- Has 20 in this page
    bodyContains "Pat_1"
    bodyContains "Pat_10"
    bodyContains "Pat_20"

    bodyContains "Add new patient record"

    -- Check 2nd page
    get $ ListPatientPageR 2
    statusIs 200

    bodyContains "Pat_6"
    bodyContains "Pat_9"

    bodyContains "Add new patient record"
    )

  -- it "edit the record" $ do
  --
  it "Search for patients" $ do
    runDB $ deleteWhere ([] :: [Filter Patient])

    -- Add some patients
    addPatients 25

    -- Do Lookup
    -- Valid name lookup should return the list of names
    withRestrictedUserLogin ( do
    get (PatientSearchR , [("name", "pat")])
    statusIs 200

    bodyContains "Pat_6"
    bodyContains "Pat_9"
    bodyContains "Pat_1"
    bodyContains "Pat_10"
    bodyContains "Pat_20"

    get (PatientSearchR , [("name", "2")])
    statusIs 200

    bodyContains "Pat_20"
    bodyContains "Pat_2"
    bodyContains "Pat_21"
    bodyContains "Pat_22"
    bodyContains "Pat_23"
    bodyContains "Pat_24"
    bodyContains "Pat_25"

    htmlNoneContain "li" "Pat_13"

    get (PatientSearchR , [("name", "1")])
    statusIs 200

    bodyContains "Pat_1"
    bodyContains "Pat_11"
    bodyContains "Pat_10"

    htmlNoneContain "li" "Pat_23"

    -- Invalid name lookup should not return any names
    get (PatientSearchR , [("name", "s")])
    statusIs 200

    htmlNoneContain "li" "Pat"
    )
