{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

module Persist.Mongo.Settings where


import Persist.Mongo.Lens
import Yesod hiding (runDB)
import GHC.Generics

import Yesod.Core (MonadIO,MonadBaseControl)
import Data.Text (Text, pack)
import Database.Persist 
import Database.Persist.MongoDB
import Database.Persist.Quasi (lowerCaseSettings)
import Network (PortID (PortNumber))
import Control.Lens.Lens
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



{-|
data Entity entity =
    Entity { entityKey :: Key entity
           , entityVal :: entity }
    deriving (Eq, Ord, Show, Read)
|-}

-- | lensEntity :: Lens (Entity a) (Entity b) a b
lensEntityVal :: Functor f => (a -> f a) -> Entity a -> f (Entity a)
lensEntityVal  f (Entity k v) = fmap (Entity k) (f v)

persistMakeClassy ''SplineConfigObj

persistMakeClassy ''ContentConfig

persistMakeClassy ''ContentObj

persistMakeClassy ''ContentArray

persistMakeClassy ''MenuPanel

persistMakeClassy ''Dashboard



