{-# LANGUAGE QuasiQuotes           #-}

module Handler.Physician where

import Import
import CustomDBDataTypes

physicianForm :: Form Physician
physicianForm = renderDivs $ Physician
  <$> areq textField "Name" Nothing
  <*> areq (selectField optionsEnum) "Gender" (Just Male)
  <*> areq textField "Position" Nothing
  <*> aopt textareaField "Remarks" Nothing

getAddPhysicianR :: Handler Html
getAddPhysicianR = do
  -- Generate the form to be displayed
  (widget, enctype) <- generateFormPost physicianForm

  defaultLayout
    [whamlet|
      <p>
      <form method=post action=@{AddPhysicianR} enctype=#{enctype}>
        ^{widget}
        <button>Submit
    |]

postAddPhysicianR :: Handler Html
postAddPhysicianR = do
  ((result, widget), enctype) <- runFormPost physicianForm

  case result of
    FormSuccess person -> do
      _ <- runDB $ insertEntity person
      defaultLayout [whamlet|<p>#{show person}|]
    _ -> defaultLayout
      [whamlet|
        <p>Invalid input, please try again.
        <form method=post action=@{AddPhysicianR} enctype=#{enctype}>
          ^{widget}
          <button>Submit
        |]
