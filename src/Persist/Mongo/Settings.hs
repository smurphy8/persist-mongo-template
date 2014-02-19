{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Persist.Mongo.Settings where

import Data.Typeable (Typeable)
import Persist.Mongo.Lens
import Yesod hiding (runDB)
import GHC.Generics

-- import Yesod.Core (MonadIO,MonadBaseControl)
import Data.Text (Text,unpack)
-- import Database.Persist 
import Database.Persist.MongoDB
import Database.Persist.Quasi (lowerCaseSettings)
import Network (PortID (PortNumber))
-- import Control.Lens.Lens
-- import Database.Persist.TH
import Language.Haskell.TH.Syntax hiding (location)
import Data.Time
import Data.ByteString hiding (unpack,group)
import qualified  Data.Yaml as Y
-- import qualified Data.Aeson as A
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
   deriving (Read, Show,Eq,Typeable)
instance FromJSON MongoDBConf where
    parseJSON (Object tObj) = MongoDBConf <$>
                          tObj .: "host" <*>
                          tObj .: "db" <*>
                          tObj .: "port"

    parseJSON _ = fail "Rule: Expecting MongoDB Config Object Received, Other"



instance ToJSON MongoDBConf where
    toJSON (MongoDBConf {..} ) = object [
                 "host" .= host,
                 "db"   .= db,
                 "port" .= port]                 


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



instance ToJSON RollingReportConfig where
instance FromJSON RollingReportConfig where

instance ToJSON RollingReportConfigEntry where
instance FromJSON RollingReportConfigEntry  where

instance ToJSON RollingReportPid where
instance FromJSON RollingReportPid  where



instance ToJSON OnpingTagHistory where 
    toJSON (OnpingTagHistory {..}) = object 
                                      [
                                        "pid"	              .= onpingTagHistoryPid			 
                                      , "time"	              .= onpingTagHistoryTime			 
                                      , "val"	              .= onpingTagHistoryVal                     
                                      ]


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

persistMakeClassy ''SubObject
persistMakeClassy ''SubMenuJoin
persistMakeClassy ''ContentArrayJoin
persistMakeClassy ''OnpingAlarmCombined
persistMakeClassy ''OnpingTagCombined
persistMakeClassy ''OnpingTagHistory
persistMakeClassy ''ParameterHistory
persistMakeClassy ''TestCollection
persistMakeClassy ''Company

persistMakeClassy ''Site

persistMakeClassy ''Location 
persistMakeClassy ''Robot

persistMakeClassy ''Unit 
persistMakeClassy ''LocationTableWidget
persistMakeClassy ''MultiLocationTableWidget
persistMakeClassy ''PDFTableWidget
persistMakeClassy ''CalendarWidget
persistMakeClassy ''Prospective 
persistMakeClassy ''ReportRow 
persistMakeClassy ''AutoReport
persistMakeClassy ''MaskScript 
persistMakeClassy ''MaskTypeJoin
persistMakeClassy ''MaskDataStore 
persistMakeClassy ''MaskType 
persistMakeClassy ''TableByMultiLocConfigObj
persistMakeClassy ''TableByLocConfigObj 
persistMakeClassy ''CustomTableConfigObj
persistMakeClassy ''CustomTableIdConfigObj
persistMakeClassy ''RollingReportConfigIdObj
