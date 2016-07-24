{-# LANGUAGE TemplateHaskell #-}

module CustomDBDataTypes where

import ClassyPrelude.Yesod
import Database.Persist.TH()

data Gender = Female | Male
  deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "Gender"

data AuthorisationLevel = AdminAccess | FullAccess | RestrictedAccess
  deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "AuthorisationLevel"
