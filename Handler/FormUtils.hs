{-# LANGUAGE QuasiQuotes           #-}

module Handler.FormUtils where

import Import

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

postAddRecordForm form addRoute = do
  ((result, widget), enctype) <- runFormPost form

  case result of
    FormSuccess obj -> do
      _ <- runDB $ insertEntity obj
      defaultLayout [whamlet|<p>#{show obj}|]
    _ -> defaultLayout
      [whamlet|
        <p>Invalid input, please try again.
        <form method=post action=@{addRoute} enctype=#{enctype}>
          ^{widget}
          <button>Submit
        |]
