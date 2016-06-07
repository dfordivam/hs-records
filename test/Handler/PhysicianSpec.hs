module Handler.PhysicianSpec (spec) where

import TestImport
import CustomDBDataTypes

spec :: Spec
spec = withApp $ do
  it "loads a form to add a new physician" $ do
    runDB $ deleteWhere ([] :: [Filter Physician])

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

  it "displays a list of physicians which has link to detail page" $ do
    runDB $ deleteWhere ([] :: [Filter Physician])
    addPhysicians 25

    get ListPhysicianR
    statusIs 200

    printBody

    -- Has 10 in this page
    bodyContains "Phys_1"
    bodyContains "Phys_10"

    -- Has link to 
    -- Has a link to add new physician
    bodyContains "Add new physician"

    get $ ListPhysicianPageR 2
    statusIs 200
    bodyContains "Phys_11"
    bodyContains "Phys_20"

  it "displays a message when no physicians are present in database" $ do
    get ListPhysicianR
    statusIs 200

    -- Has a link to add new physician
    --
  -- it "details page shows physician details, list of upcoming appointments\
  --   \ past appointments, link to edit details" $ do
  --   get ListPhysicianR
  --   statusIs 200

addPhysicians countN = do
  unless (countN < 2) $ addPhysicians (countN - 1)

  let physician = Physician name gender position remarks
      name = pack $ "Phys_" ++ (show countN)
      gender = if even countN then Male else Female
      position = pack $ "Pos_" ++ (show countN)
      remarks = Nothing
      -- Just $ Textarea $ pack $ "Remarks for Phys_" ++ (show count)

  runDB $ insert physician
  return ()
