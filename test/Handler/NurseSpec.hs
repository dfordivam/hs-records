module Handler.NurseSpec (spec) where

import TestImport
import CustomDBDataTypes
import Handler.TestUtils

spec :: Spec
spec = withApp $ do
  it "loads a form to add a new nurse" $ do
    runDB $ deleteWhere ([] :: [Filter Nurse])

    withFullUserLogin ( do
    get AddNurseR
    statusIs 403
    )

    withAdminLogin ( do
    get AddNurseR
    statusIs 200
    -- htmlAllContain "h1" "Add new nurse information"

    let name = "Nurse's Name"
        title = "Nurse's Title"
        gender = Female

    request $ do
      setMethod "POST"
      setUrl AddNurseR
      addToken

      byLabel "Name" name
      byLabel "Gender" "1"
      byLabel "Position" title
      byLabel "Active" "1"

    -- printBody
    statusIs 200

    (Entity _ obj:_) <- runDB $ selectList [NurseName ==. name] []
    assertEqual "Should have " obj (Nurse name gender (Just title) Nothing False)
    )

  it "displays a list of nurses which has link to detail page" $ do
    runDB $ deleteWhere ([] :: [Filter Nurse])
    addNurses 25

    withRestrictedUserLogin ( do
    get ListNurseR
    statusIs 403
    )

    withFullUserLogin ( do
    get ListNurseR
    followRedirect
    statusIs 200

    -- Has 10 in this page
    bodyContains "Nurse_1"
    bodyContains "Nurse_10"

    bodyContains "Add new nurse"

    -- Check 2nd page
    get $ ListNursePageR 2
    statusIs 200
    bodyContains "Nurse_20"

    bodyContains "Add new nurse"
    )

  it "edit the record" $ do
    addNurses 25
    Just (Entity objId obj) <- runDB $ selectFirst ([] :: [Filter Nurse]) []

    withFullUserLogin ( do
    get $ EditNurseR objId
    statusIs 403
    )

    withAdminLogin ( do
    get $ EditNurseR objId
    statusIs 200

    let oldName = nurseName obj
        oldRemarks = nurseRemarks obj
        oldPosition = fromMaybe "" (nursePosition obj)

    bodyContains (unpack oldName)

    let newName = oldName ++ "_newname"
        newRemarks = "some new remark" :: Text

    request $ do
      setMethod "POST"
      setUrl $ EditNurseR objId
      addToken

      byLabel "Name" newName
      byLabel "Gender" "1"
      byLabel "Remarks" newRemarks
      byLabel "Position" oldPosition
      byLabel "Active" "1"

    statusIs 200

    Just (Entity _ newObj) <- runDB $  selectFirst ([] :: [Filter Nurse]) []
    --getBy $ objId

    assertEqual "Name should be" (nurseName newObj) (newName)
    assertEqual "Position should be" (nursePosition newObj) (nursePosition obj)
    -- assertEqual "Remarks should be" (nurseRemarks newObj) (Just newRemarks)
    )

  it "displays a message when no nurses are present in database" $ do
    get ListNurseR
    followRedirect
    statusIs 200

    -- Has a link to add new nurse
    --
  -- it "details page shows nurse details, list of upcoming appointments\
  --   \ past appointments, link to edit details" $ do
  --   get ListNurseR
  --   statusIs 200

