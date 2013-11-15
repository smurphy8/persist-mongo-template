{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Persist.Mongo.Settings where


import Persist.Mongo.Lens
import Yesod hiding (runDB)
-- import Data.Text (Text)
-- import Database.Persist.Quasi
-- import Data.Aeson (withText,(.:?),(.!=))
-- import Control.Applicative ((<$>),(<*>),pure)
-- import Yesod.Auth.HashDB (HashDBUser(..))
-- import Data.Time
import GHC.Generics
-- import Database.Persist.MongoDB hiding (master)
-- import WidgetTypes
-- import Permissions
-- import ContentCfgTypes
-- import Language.Haskell.TH.Syntax

-- import Data.Typeable (Typeable)

import Yesod.Core (MonadIO,MonadBaseControl)
import Data.Text (Text, pack)
import Database.Persist 
import Database.Persist.MongoDB
import Database.Persist.Quasi (lowerCaseSettings)
import Network (PortID (PortNumber))
import Database.Persist.TH
import Language.Haskell.TH.Syntax
import Data.Time

import ContentCfgTypes
import WidgetTypes
-- share [mkPersist (mkPersistSettings (ConT ''MongoBackend)) { mpsGeneric = False }, mkMigrate "migrateAll"][persistLowerCase|
-- Questionnaire
--   desc Text Maybe
--   questions [Question]
--   deriving Show Eq Read 
-- Question
--   formulation Text
--   deriving Show Eq Read 
-- |]

share [mkPersist (mkPersistSettings (ConT ''MongoBackend)) { mpsGeneric = False }, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "modelsMongo")




{-===========================================================================-}
{-                                 runDB                                     -}
{-===========================================================================-}

runDB :: forall (m :: * -> *) b.(MonadIO m ,MonadBaseControl IO m) =>
               Action m b -> m b
runDB a = withMongoDBConn "onping_production" "localhost" (PortNumber 27017) Nothing 2000 $ \pool -> do 
  (runMongoDBPool master a )  pool




persistMakeClassy ''ContentObj

persistMakeClassy ''ContentArray

persistMakeClassy ''MenuPanel

persistMakeClassy ''Dashboard


