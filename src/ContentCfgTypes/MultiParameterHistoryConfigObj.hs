{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,DeriveGeneric, MultiParamTypeClasses, FlexibleInstances  #-}
module ContentCfgTypes.MultiParameterHistoryConfigObj where

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)

import Control.Applicative ((<$>), (<*>))
import Yesod 


import Data.Time

import Data.Text
import ContentCfgTypes.Util

-- | ==================================================
-- | MultiParameterHistoryConfigObj config 
-- | ================================================== 

data MultiParameterHistoryConfigObj =  MultiParameterHistoryConfigObj { 
      mhistoryStep :: Double
     ,mhistoryDelta :: Double
     ,mhistoryStart :: UTCTime
     ,mhistoryEnd   :: UTCTime
     ,mhistoryPIDList :: Text

    }
   deriving (Read, Show, Eq)

instance FromJSON MultiParameterHistoryConfigObj where 
    parseJSON (Object tObj) = MultiParameterHistoryConfigObj <$> 
                          tObj .: "step"  <*>
                          tObj .: "dt" <*>
                          tObj .: "startDate" <*>
                          tObj .: "endDate"   <*>
                          tObj .: "pidList"
                           

    parseJSON _ = fail "Rule: Expecting multi parameter history object object received, other"

instance ToJSON MultiParameterHistoryConfigObj where 
    toJSON (MultiParameterHistoryConfigObj {..}) = object 
                        [ 
                         "step"  .= mhistoryStep 
                         ,"dt" .= mhistoryDelta
                         ,"startDate" .= mhistoryStart
                         ,"endDate"   .= mhistoryEnd
                         ,"pidList" .=   mhistoryPIDList

                         ]


-- | A Tank object transformer on a get parameter string

runMultiParameterHistoryConfigObj :: (Text,Text) -> (Text, Value)
runMultiParameterHistoryConfigObj (t,v)
  | t == "step"  = (t .= intVal v)
  | t == "dt" = (t .= intVal v)
  | t == "startDate" = (t .= utcVal v)
  | t == "endDate"   = (t .= utcVal v)
  | t == "pidList"   = (t .= textVal v)
  | otherwise    = (t .= toJSON v)

