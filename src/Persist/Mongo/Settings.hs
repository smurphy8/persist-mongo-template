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
import Data.Text (Text, pack, unpack)
import Database.Persist 
import Database.Persist.MongoDB
import Database.Persist.Quasi (lowerCaseSettings)
import Network (PortID (PortNumber))
import Control.Lens.Lens
import Database.Persist.TH
import Language.Haskell.TH.Syntax
import Data.Time
import qualified  Data.Yaml as Y
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Control.Applicative  ((<$>),(<*>))

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


data MongoDBConf =  MongoDBConf {
     host :: Text
    ,db   :: Text
    ,port :: Int
    }
   deriving (Read, Show,Eq)
instance FromJSON MongoDBConf where
    parseJSON (Object tObj) = MongoDBConf <$>
                          tObj .: "host" <*>
                          tObj .: "db" <*>
                          tObj .: "port"

    parseJSON _ = fail "Rule: Expecting MongoDB Config Object Received, Other"




share [mkPersist (mkPersistSettings (ConT ''MongoBackend)) { mpsGeneric = False }, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "modelsMongo")




{-===========================================================================-}
{-                                 runDB                                     -}
{-===========================================================================-}

runDB :: forall (m :: * -> *) b.(MonadIO m ,MonadBaseControl IO m) =>
               Action m b -> m b
runDB a = withMongoDBConn "onping_production" "localhost" (PortNumber 27017) Nothing 2000 $ \pool -> do 
  (runMongoDBPool master a )  pool


runDBConf :: forall (m :: * -> *) b.(MonadIO m ,MonadBaseControl IO m) =>
               MongoDBConf -> Action m b -> m b
runDBConf (MongoDBConf host db port) a = withMongoDBConn db (unpack host) (PortNumber $ fromIntegral port) Nothing 2000 $ \pool -> do 
  (runMongoDBPool master a )  pool

readDBConf :: FilePath -> IO (Either String MongoDBConf)
readDBConf fPath = do
	fCont <- BS.readFile fPath
	return $ Y.decodeEither $ fCont


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