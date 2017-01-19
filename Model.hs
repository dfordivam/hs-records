{-# LANGUAGE FlexibleInstances #-}

-- Need to figure out if this can be avoided
{-# LANGUAGE UndecidableInstances #-}

module Model where

import ClassyPrelude.Yesod hiding (share)
import Database.Persist.Quasi
#ifdef USE_MONGODB
import Database.Persist.MongoDB hiding (master)
#else
#endif
import Language.Haskell.TH.Syntax
import CustomDBDataTypes
import Database.Persist.CDC.TH

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
#ifdef USE_MONGODB
let mongoSettings = (mkPersistSettings (ConT ''MongoContext))
 in share "User" [mkPersist mongoSettings]
    $(persistFileWith lowerCaseSettings "config/models")
#else
share "User" [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")
#endif
