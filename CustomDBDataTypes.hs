{-# LANGUAGE TemplateHaskell #-}

module CustomDBDataTypes where

import ClassyPrelude.Yesod
import Database.Persist.TH()

data Gender = Female | Male
  deriving (Show, Read, Eq)

derivePersistField "Gender"

