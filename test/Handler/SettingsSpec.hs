module Handler.SettingsSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do
  it "Should load without login if admin is not present" $ do
    runDB $ deleteWhere ([] :: [Filter User])

    get SettingsR
    statusIs 200

  it "Add admin should create a new admin account" $ do
    -- This will add the admin account
    -- Assuming that currently there is no admin
    get SettingsAddAdminR
    statusIs 303
    followRedirect

    -- Redirect to login
    statusIs 303
    followRedirect

  it "Should require admin login if admin is present" $ do
    get SettingsR
    statusIs 303
    followRedirect

    -- Do login with admin account
    request $ do
      setMethod "POST"
      -- Fix this, use AuthR LoginR
      setUrl ("/auth/page/account/login" :: Text)

      byLabel "Username" adminUserName
      byLabel "Password" adminUserPassword

    statusIs 303
    followRedirect

    statusIs 200
    bodyContains "Add Admin"
