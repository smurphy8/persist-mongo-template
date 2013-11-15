{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,DeriveGeneric, MultiParamTypeClasses, FlexibleInstances  #-}
module ContentCfgTypes.Util where
import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)


import Yesod 
--import Text.Julius
import qualified WidgetTypes as W
import qualified Database.MongoDB as MDB
import Data.Aeson.Types
import System.Locale
import Data.Time

import Data.Text
--import Yesod


-- | Helper functions to explicity do type conversion 

intVal :: Text -> Value
intVal = toJSON.intRead
  where
    intRead :: Text -> Int
    intRead = read.unpack

intListVal :: Text -> Value
intListVal = toJSON.intListRead
  where
    intListRead :: Text -> [Int]
    intListRead = read.unpack

boolVal :: Text -> Value
boolVal = toJSON.boolRead
  where
    boolRead :: Text -> Bool
    boolRead = read.unpack

textVal :: Text -> Value
textVal = toJSON

utcVal :: Text -> Value
utcVal = toJSON.timeRead
    where 
      timeRead :: Text -> UTCTime
      timeRead = (readTime defaultTimeLocale (unpack W.stdTimeFormat)).unpack

-- | A Table object transformer on a get parameter string
-- | "o51ffc5907671f95ad8000000"
locVal :: Text -> Value
locVal = toJSON . cnvServe.cnv
    where 
        cnv :: Text -> Value
        cnv = toJSON.unpack
        cnvServe :: (Value -> Maybe PersistValue)
        cnvServe t = case fromJSON t of
                       (Success l) -> Just l
                       (Error _ ) -> Nothing 


text2PersistVal :: Text -> Value
text2PersistVal = toJSON . cnvServe.cnv
    where 
        cnv :: Text -> Value
        cnv = toJSON.unpack
        cnvServe :: (Value -> Maybe PersistValue)
        cnvServe t = case fromJSON t of
                       (Success l) -> Just l
                       (Error _ ) -> Nothing 



