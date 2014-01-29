{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,DeriveGeneric, MultiParamTypeClasses, FlexibleInstances  #-}
module ContentCfgTypes.TableByMultiLocConfigObj where

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)
-- import qualified Data.ByteString as BS
import Control.Applicative ((<$>))
import Yesod 
-- import Data.Aeson.Types
import Data.Text
import ContentCfgTypes.Util


data TableByMultiLocConfigObj = TableByMultiLocConfigObj {
     mlcoTableId :: PersistValue
    }
   deriving (Read, Show,Eq)

instance FromJSON TableByMultiLocConfigObj where
    parseJSON (Object tObj) = TableByMultiLocConfigObj <$>
                          tObj .: "tableid"
    parseJSON _ = fail "Rule: Expecting Test Object Received, Other"

instance ToJSON TableByMultiLocConfigObj where 
    toJSON (TableByMultiLocConfigObj {..}) = object
                                             [ "tableid" .= mlcoTableId   ]



runTableByMultiLocConfigObj :: (Text,Text) -> (Text, Value)
runTableByMultiLocConfigObj (t,v)
  | t == "tableid" = (t .=  locVal v)
  | otherwise = (t .= toJSON v)

