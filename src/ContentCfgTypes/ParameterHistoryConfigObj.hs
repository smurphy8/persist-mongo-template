{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,DeriveGeneric, MultiParamTypeClasses, FlexibleInstances  #-}
module ContentCfgTypes.ParameterHistoryConfigObj where

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)

import Control.Applicative ((<$>), (<*>))
import Yesod 


import Data.Time

import Data.Text
import ContentCfgTypes.Util

-- | ==================================================
-- | ParameterHistoryConfigObj Config 
-- | ================================================== 




data ParameterHistoryConfigObj =  ParameterHistoryConfigObj { 
      phistoryStep :: Double
     ,phistoryDelta :: Double
     ,phistoryStart :: UTCTime
     ,phistoryEnd   :: UTCTime
     ,phistoryPID :: Text

    }
   deriving (Read, Show,Eq)

instance FromJSON ParameterHistoryConfigObj where 
    parseJSON (Object tObj) = ParameterHistoryConfigObj <$> 
                          tObj .: "step"  <*>
                          tObj .: "delta" <*>
                          tObj .: "start" <*>
                          tObj .: "end"   <*>
                          tObj .: "pid"
                           

    parseJSON _ = fail "Rule: Expecting Parameter History Object Object Received, Other"

instance ToJSON ParameterHistoryConfigObj where 
    toJSON (ParameterHistoryConfigObj {..}) = object 
                        [ 
                          "start" .= phistoryStart
                         ,"end"   .= phistoryEnd
                         ,"step"  .= phistoryStep
                         ,"delta" .= phistoryDelta
                         ,"pid" .=   phistoryPID

                         ]


-- | A Tank object transformer on a get parameter string

runParameterHistoryConfigObj :: (Text,Text) -> (Text, Value)
runParameterHistoryConfigObj (t,v)
  | t == "start" = (t .= utcVal v)
  | t == "end"   = (t .= utcVal v) 
  | t == "step"  = (t .= intVal v)
  | t == "delta" = (t .= intVal v)
  | t == "pid"   = (t .= textVal v)
  | otherwise    = (t .= toJSON v)

