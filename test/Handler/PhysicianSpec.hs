module Handler.PhysicianSpec (spec) where

import TestImport
import CustomDBDataTypes

spec :: Spec
spec = withApp $ do
  it "loads a form to add a new physician" $ do
    get AddPhysicianR
    statusIs 200
    -- htmlAllContain "h1" "Add new physician information"

    let name = "Physician's Name"
        title = "Physician's Title"
        gender = Female

    request $ do
      setMethod "POST"
      setUrl AddPhysicianR
      addToken

      byLabel "Name" name
      byLabel "Gender" "1"
      byLabel "Position" title

    -- printBody
    statusIs 200

    (Entity _id obj:_) <- runDB $ selectList [PhysicianName ==. name] []
    assertEqual "Should have " obj (Physician name gender title Nothing)

