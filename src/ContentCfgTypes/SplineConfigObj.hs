{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,DeriveGeneric, MultiParamTypeClasses, FlexibleInstances  #-}
module ContentCfgTypes.SplineConfigObj where

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)
import Control.Applicative ((<$>), (<*>))
import Yesod
import Data.Text
import ContentCfgTypes.Util

data SplineConfigObj =  SplineConfigObj { 
     splineStep :: Int
     ,splineTitle :: Text
     ,splineParamIds :: Text
     ,splineTime :: Int
     ,splineTimeUnit :: Text
     ,splineEndDate :: Text
     ,splineLegend :: Int
     ,splineDescriptionList :: Text
     ,splineLocationList :: Text
     ,splineGraphList :: Text
--     ,splineSecondYAxisList :: Text
    }
   deriving (Read, Show,Eq)






instance FromJSON SplineConfigObj where 
    parseJSON (Object tObj) = SplineConfigObj <$>  
                          tObj .: "step"  <*>
                          tObj .: "title" <*>
                          tObj .: "pidlist" <*>
                          tObj .: "time" <*>
                          tObj .: "timeUnit" <*>
                          tObj .: "endDate" <*>
                          tObj .: "legend" <*>
                          tObj .: "descriptionList" <*>
                          tObj .: "locationList" <*>
                          tObj .: "graphList"   --  <*>
--                          tObj .: "secondYAxisList"


    parseJSON _ = fail "Rule: Expecting Test Object Received, Other"

instance ToJSON SplineConfigObj where 
    toJSON (SplineConfigObj {..}) = object 
                        [ 
                         "step"  .= splineStep
                         ,"title" .= splineTitle
                         ,"pidlist" .= splineParamIds
                         ,"time" .= splineTime
                         ,"timeUnit" .= splineTimeUnit
                         ,"endDate" .= splineEndDate
                         ,"legend" .= splineLegend
                         ,"descriptionList" .= splineDescriptionList
                         ,"locationList" .= splineLocationList
                         ,"graphList" .= splineGraphList
--                         ,"secondYAxisList" .= splineSecondYAxisList
                         ]


-- | A Spline object transformer on a get parameter string

runSplineConfigObj :: (Text,Text) -> (Text, Value)
runSplineConfigObj (t,v) 
  | t == "step"  = (t .= intVal v)
  | t == "title" = (t .=  textVal v)
  | t == "pidlist" = (t .= textVal v)
  | t == "time" = (t .= intVal v)
  | t == "timeUnit" = (t .= textVal v)
  | t == "endDate" = (t .= textVal v)
  | t == "legend" = (t .= intVal v)
  | t == "descriptionList" = (t .= textVal v)
  | t == "locationList" = (t .= textVal v)
  | t == "graphList" = (t .= textVal v)
--  | t == "secondYAxisList" = (t .= textVal v)
  | otherwise = (t .= toJSON v)

defaultSCO :: SplineConfigObj
defaultSCO = SplineConfigObj 600 "Enter Title Here" "" 3 "hour" "" 1 "" "" "" -- ""