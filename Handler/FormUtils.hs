{-# LANGUAGE QuasiQuotes           #-}

module Handler.FormUtils where

import Import
import CustomDBDataTypes
import Database.Persist.CDC

getAddRecordForm form addRoute = do
  -- Generate the form to be displayed
  (widget, enctype) <- generateFormPost form

  defaultLayout
    [whamlet|
      <p>
      <form method=post action=@{addRoute} enctype=#{enctype}>
        ^{widget}
        <button>Submit
    |]

postAddRecordForm maybeId form addRoute = do
  ((result, widget), enctype) <- runFormPost form

  case result of
    FormSuccess obj -> do
      case maybeId of
        Nothing -> runDB $ insertEntity obj >> return ()
        Just objId -> do
          mu <- maybeAuthId
          case mu of
            Nothing -> return ()
            Just uname -> runDB $ do
              Just (Entity userId _) <- selectFirst [UserUsername ==. uname] []
              replaceWithCDC userId objId obj
      defaultLayout [whamlet|<p>#{show obj}|]
    _ -> defaultLayout
      [whamlet|
        <p>Invalid input, please try again.
        <form method=post action=@{addRoute} enctype=#{enctype}>
          ^{widget}
          <button>Submit
        |]

physicianForm :: Maybe Professional -> Form Professional
physicianForm = commonProfessionalForm Physician

nurseForm :: Maybe Professional -> Form Professional
nurseForm = commonProfessionalForm Nurse

commonProfessionalForm role inp = renderDivs $ Professional
  <$> areq textField "Name" (professionalName <$> inp)
  <*> areq (selectField optionsEnum) "Gender" (professionalGender <$> inp)
  <*> areq textField "Position" (professionalPosition <$> inp)
  <*> aopt textareaField "Remarks" (professionalRemarks <$> inp)
  <*> areq (selectField optionsEnum) "Active" (professionalActive <$> inp)
  <*> pure role

