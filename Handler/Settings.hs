{-# LANGUAGE LambdaCase #-}
module Handler.Settings where

import Import
import Yesod.Auth.Account
import CustomDBDataTypes

getSettingsR :: Handler Html
getSettingsR = do
  defaultLayout $ do
    $(widgetFile "settings")

getSettingsAddAdminR :: Handler Html
getSettingsAddAdminR = do
  maybeUser <- runAccountPersistDB $ loadUser adminUserName
  case maybeUser of
    Just _ -> redirect SettingsR
    Nothing -> do
      pass <- hashPassword adminUserPassword
      Entity userId _ <- runAccountDB $ do
        Right user <- addNewUser adminUserName "" "" pass -- Point of failure
        verifyAccount user
        return user
      runDB $ update userId [UserAuthLevel =. AdminAccess]
      redirect SettingsR

getSettingsVerifyUserR :: Text -> Handler Html
getSettingsVerifyUserR userName = do
  access <- lookupGetParam "access" >>=
            \case
              Nothing -> return RestrictedAccess
              Just _ -> return FullAccess
  runAccountDB $ loadUser userName >>= mapM verifyAccount
  runDB $ selectFirst [UserUsername ==. userName] [] >>= 
    mapM (\(Entity uid _) -> update uid [UserAuthLevel =. access])
  redirect SettingsR
