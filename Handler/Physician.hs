{-# LANGUAGE QuasiQuotes           #-}

module Handler.Physician where

import Import
import CustomDBDataTypes

physicianForm :: Form Physician
physicianForm = renderDivs $ Physician
  <$> areq textField "Name" Nothing
  <*> areq (selectField optionsEnum) "Gender" (Just Male)
  <*> areq textField "Position" Nothing
  <*> aopt textField "Remarks" Nothing

getAddPhysicianR :: Handler Html
getAddPhysicianR = do
  -- Generate the form to be displayed
  (widget, enctype) <- generateFormPost physicianForm

  defaultLayout
    [whamlet|
      <p>
        The widget generated contains only the contents
        of the form, not the form tag itself. So...
      <form method=post action=@{AddPhysicianR} enctype=#{enctype}>
        ^{widget}
        <p>It also doesn't include the submit button.
        <button>Submit
    |]

postAddPhysicianR :: Handler Html
postAddPhysicianR = do
  ((result, widget), enctype) <- runFormPost physicianForm

  case result of
    FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
    _ -> defaultLayout
      [whamlet|
        <p>Invalid input, let's try again.
        <form method=post action=@{AddPhysicianR} enctype=#{enctype}>
          ^{widget}
          <button>Submit
        |]
