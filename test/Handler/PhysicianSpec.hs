module Handler.PhysicianSpec (spec) where

import TestImport
import CustomDBDataTypes
import Handler.TestUtils
import Yesod.Form.Fields (unTextarea)

spec :: Spec
spec = withApp $ do
  describe "Physician Handler" $ do
    describe "Basic CRUD operations" $ do
      it "Creates/adds a new physician entry" $ do
        runDB $ deleteWhere ([] :: [Filter Physician])

        withFullUserLogin ( do
        get AddPhysicianR
        statusIs 403
        )

        withAdminLogin ( do
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
          byLabel "Active" "1"

        -- printBody
        statusIs 200

        (Entity _ obj:_) <- runDB $ selectList [PhysicianName ==. name] []
        assertEqual "Should have " obj (Physician name gender title Nothing False)
        )

      it "edits/updates the record" $ do
        addPhysicians 25
        Just (Entity objId obj) <- runDB $ selectFirst ([] :: [Filter Physician]) []

        withFullUserLogin ( do
        get $ EditPhysicianR objId
        statusIs 403
        )

        withAdminLogin ( do
        get $ EditPhysicianR objId
        statusIs 200

        let oldName = physicianName obj
            oldRemarks = physicianRemarks obj

        bodyContains (unpack oldName)

        let newName = oldName ++ "_newname"
            newRemarks = "some new remark" :: Text

        request $ do
          setMethod "POST"
          setUrl $ EditPhysicianR objId
          addToken

          byLabel "Name" newName
          byLabel "Gender" "1"
          byLabel "Position" (physicianPosition obj)
          byLabel "Remarks" newRemarks
          byLabel "Active" "1"

        statusIs 200

        Just (Entity _ newObj) <- runDB $  selectFirst ([] :: [Filter Physician]) []
        --getBy $ objId

        assertEqual "Name" (physicianName newObj) (newName)
        assertEqual "Position" (physicianPosition newObj) (physicianPosition obj)
        assertEqual "Remarks" (fmap unTextarea (physicianRemarks newObj)) (Just newRemarks)

        Just (Entity _ hist) <- runDB $  selectFirst ([] :: [Filter PhysicianHistory]) []
        assertEqual "History" (objId) (physicianHistoryPhysician hist)
        assertEqual "Name" (Just oldName) (physicianHistoryName hist)
        assertEqual "Position" (Nothing) (physicianHistoryPosition hist)
        assertEqual "Remarks" (oldRemarks, True) (physicianHistoryRemarks hist)
        )

      -- it "details page shows physician details, list of upcoming appointments\
      --   \ past appointments, link to edit details" $ do
      --   get ListPhysicianR
      --   statusIs 200

    describe "Pagination" $ do
      it "displays a list of physicians which has link to detail page" $ do
        runDB $ deleteWhere ([] :: [Filter Physician])
        addPhysicians 25

        withRestrictedUserLogin ( do
        get ListPhysicianR
        statusIs 403
        )
        -- This require Full priviledges

        withFullUserLogin ( do
        get ListPhysicianR
        followRedirect
        statusIs 200

        -- Has 10 in this page
        bodyContains "Phys_1"
        bodyContains "Phys_10"

        bodyContains "Add new physician"

        -- Check 2nd page
        get $ ListPhysicianPageR 2
        statusIs 200
        bodyContains "Phys_20"

        bodyContains "Add new physician"
        )


      -- it "displays a message when no physicians are present in database" $ do
      --   get ListPhysicianR
      --   followRedirect
      --   statusIs 200

      --   -- Has a link to add new physician
        --
