{-# LANGUAGE FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
#ifdef USE_MONGODB
import Database.Persist.MongoDB hiding (master)
#else
#endif
import Language.Haskell.TH.Syntax
import CustomDBDataTypes

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
#ifdef USE_MONGODB
let mongoSettings = (mkPersistSettings (ConT ''MongoContext))
 in share [mkPersist mongoSettings]
    $(persistFileWith lowerCaseSettings "config/models")
#else
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models")
#endif
