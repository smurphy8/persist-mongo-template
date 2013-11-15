{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,DeriveGeneric, MultiParamTypeClasses, FlexibleInstances  #-}
module ContentCfgTypes.AutoReportConfigObj where

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)

import Control.Applicative ((<$>), (<*>))
import Yesod 

import Data.Text
import ContentCfgTypes.Util

-- | ==================================================
-- | Autoreport Config 
-- | ================================================== 

data AutoReportConfigObj  =  AutoReportConfigObj { 
      autoReportWidth :: Int
     ,autoReportTitle :: Text
    }
   deriving (Read,Show,Eq)

-- instance ToJavascript Value where 
--     toJavascript = fromValue


instance FromJSON AutoReportConfigObj where 
    parseJSON (Object tObj) = AutoReportConfigObj <$> 
                          tObj .: "width" <*> 
                          tObj .: "title"

    parseJSON _ = fail "Rule: Expecting Test Object Received, Other"

instance ToJSON AutoReportConfigObj where 
    toJSON (AutoReportConfigObj {..}) = object 
                        [ 
                         "width" .= autoReportWidth 
                         ,"title" .= autoReportTitle 
                         ]


-- | A autoReport object transformer on a get parameter string

runAutoReportConfigObj :: (Text,Text) -> (Text, Value)
runAutoReportConfigObj (t,v)
  | t == "width" = (t .= intVal v)
  | t == "title" = (t .=  textVal v)
  | otherwise = (t .= toJSON v)


