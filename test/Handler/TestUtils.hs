{-# LANGUAGE LambdaCase #-}

module Handler.TestUtils (
  withAdminLogin,
  withFullUserLogin,
  withRestrictedUserLogin,
  addPhysicians,
  addNurses,
  addPatients,
  doPrint
  )
  where

import TestImport
import CustomDBDataTypes
import Yesod.Auth.Account
import System.IO

withAdminLogin f = doSettingsInit >>
  doAdminLogin >> f >> doLogout

withFullUserLogin f = doSettingsInit >>
  doFullAccessLogin >> f >> doLogout

withRestrictedUserLogin f = doSettingsInit >>
  doRestrictedAccessLogin >> f >> doLogout

doPrint str = liftIO $ hPutStr stderr str

addPhysicians = addProfessionalData Physician
addNurses = addProfessionalData Nurse

addProfessionalData role countN = do
  unless (countN < 2) $ addPhysicians (countN - 1)

  let physician = Professional name gender position remarks True role
      name = pack $ prefix ++ (show countN)
      gender = if even countN then Male else Female
      position = pack $ "Pos_" ++ (show countN)
      remarks = Nothing
      prefix = case role of
                Physician -> "Phys_"
                Nurse -> "Nurse_"
      -- Just $ Textarea $ pack $ "Remarks for Phys_" ++ (show count)

  runDB $ insert physician
  return ()


addPatients countN = do
  unless (countN < 2) $ addPatients (countN - 1)

  let patient = Patient name gender
        Nothing Nothing (Just email) Nothing Nothing
      name = pack $ "Pat_" ++ (show countN)
      gender = if even countN then Male else Female
      email = name ++ "@gmail.com"

  runDB $ insert patient
  return ()

fullAccessUserName = "userfullaccess"
fullAccessPassword = "passfullaccess"

restrictedAccessUserName = "restrictedAccessUserName"
restrictedAccessPassword = "restrictedAccessPassword"

authrLoginUrl = ("/auth/page/account/login" :: Text)
authNewUserUrl = ("/auth/page/account/newaccount" :: Text)

doSettingsInit = do
  maybeAdmin <- runDB $ selectFirst [UserUsername ==. adminUserName] []
  case maybeAdmin of
    Just _ -> return ()
    Nothing -> do
      runDB $ deleteWhere ([] :: [Filter User])
      get SettingsAddAdminR

      addUsers
      doLogout

doLogin user pass = do
  request $ do
    setMethod "POST"
    -- Fix this, use AuthR LoginR
    setUrl authrLoginUrl

    addPostParam "f1" user
    addPostParam "f2" pass
  
doLogout =
  post $ ("/auth/logout" :: Text)

doAdminLogin = doLogin adminUserName adminUserPassword

doFullAccessLogin = doLogin fullAccessUserName fullAccessPassword 

doRestrictedAccessLogin = doLogin restrictedAccessUserName restrictedAccessPassword

addUsers = do
  addUser fullAccessUserName fullAccessPassword FullAccess
  addUser restrictedAccessUserName restrictedAccessPassword RestrictedAccess

-- Adds a verified user
addUser username password access = do
  get authNewUserUrl
  request $ do
    setMethod "POST"
    -- Fix this, use AuthR
    setUrl authNewUserUrl
    addToken

    addPostParam "f1" username
    addPostParam "f2" "a@d.com"
    addPostParam "f3" password
    addPostParam "f4" password

  followRedirect

  doAdminLogin
  let params = if access == FullAccess then [("access", "full")] else [] :: [(Text,Text)]
  get (SettingsVerifyUserR username, params)
  doLogout
