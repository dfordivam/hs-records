{-# LANGUAGE TemplateHaskell #-}

module CustomDBDataTypes where

import ClassyPrelude.Yesod
import Database.Persist.TH()

data RoleType = Physician | Nurse
  deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "RoleType"

data Gender = Female | Male
  deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "Gender"

data AuthorisationLevel = AdminAccess | FullAccess | RestrictedAccess
  deriving (Show, Read, Eq, Enum, Bounded)

derivePersistField "AuthorisationLevel"
