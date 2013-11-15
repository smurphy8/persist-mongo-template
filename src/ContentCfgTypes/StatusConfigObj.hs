{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,DeriveGeneric, MultiParamTypeClasses, FlexibleInstances  #-}
module ContentCfgTypes.StatusConfigObj where

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)

import Control.Applicative ((<$>), (<*>))
import Yesod 

import Data.Text
import ContentCfgTypes.Util


data StatusConfigObj =  StatusConfigObj {
      statusWidth :: Int
      ,statusTrueText :: Text
      ,statusFalseText :: Text
      ,statusPidList :: Text
      ,statusDescriptionList :: Text
      ,statusLocationList :: Text
      ,statusSetValues :: Text

    }
   deriving (Read, Show,Eq)

instance FromJSON StatusConfigObj where 
    parseJSON (Object tObj) = StatusConfigObj <$>
                          tObj .: "width"  <*>
                          tObj .: "trueText"  <*>
                          tObj .: "falseText"  <*>
                          tObj .: "pidList" <*>
                          tObj .: "descriptionList" <*>
                          tObj .: "locationList" <*>
                          tObj .: "setValues"


    parseJSON _ = fail "Rule: Expecting Test Object Received, Other"

instance ToJSON StatusConfigObj where 
    toJSON (StatusConfigObj {..}) = object 
                        [
                         "width" .= statusWidth
                         ,"trueText" .= statusTrueText
                         ,"falseText" .= statusFalseText
                         ,"pidList" .= statusPidList
                         ,"descriptionList" .= statusDescriptionList
                         ,"locationList" .= statusLocationList
                         ,"setValues" .= statusSetValues
                         ]


-- | A Status object transformer on a get parameter string

runStatusConfigObj :: (Text,Text) -> (Text, Value)
runStatusConfigObj (t,v)
  | t == "width" = (t .= intVal v)
  | t == "trueText" = (t .= textVal v)
  | t == "falseText" = (t .= textVal v)
  | t == "pidList" = (t .= textVal v)
  | t == "descriptionList" = (t .= textVal v)
  | t == "locationList" = (t .= textVal v)
  | t == "setValues" = (t .= textVal v)
  | otherwise = (t .= toJSON v)


defaultStCO :: StatusConfigObj 
defaultStCO = StatusConfigObj 200 "" "" "" "" "" ""
