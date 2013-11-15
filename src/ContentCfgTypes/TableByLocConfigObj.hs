{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,DeriveGeneric, MultiParamTypeClasses, FlexibleInstances  #-}
module ContentCfgTypes.TableByLocConfigObj where

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)
import Control.Applicative ((<$>))

import Yesod 

import Data.Text
import ContentCfgTypes.Util

-- Table stuff

data TableByLocConfigObj =  TableByLocConfigObj {
     tblcoTableId :: PersistValue
    }
   deriving (Read, Show,Eq)

instance FromJSON TableByLocConfigObj where
    parseJSON (Object tObj) = TableByLocConfigObj <$>
                          tObj .: "tableid"

    parseJSON _ = fail "Rule: Expecting Location Table Id Received, Other"

instance ToJSON TableByLocConfigObj where 
    toJSON (TableByLocConfigObj {..}) = object
                        [
                         "tableid" .= tblcoTableId
                         ]

-- | A Table object transformer on a get parameter string

runTableConfigObj :: (Text,Text) -> (Text, Value)
runTableConfigObj (t,v)
  | t == "tableid" = (t .=  locVal v)
  | otherwise = (t .= toJSON v)
