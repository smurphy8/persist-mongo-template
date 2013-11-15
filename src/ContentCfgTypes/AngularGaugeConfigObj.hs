{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,DeriveGeneric, MultiParamTypeClasses, FlexibleInstances  #-}
module ContentCfgTypes.AngularGaugeConfigObj where

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)

import Control.Applicative ((<$>), (<*>))
import Yesod 

import Data.Text
import ContentCfgTypes.Util


-- AngularGauge stuf

data AngularGaugeConfigObj =  AngularGaugeConfigObj {
      angularGaugeWidth :: Int
     ,angularGaugeHeight :: Int
     ,angularGaugeStacking :: Text
     ,angularGaugeUnits :: Text
     ,angularGaugeColors :: Text
     ,angularGaugeLineColors :: Text
     ,angularGaugeLineParams :: Text
     ,angularGaugeWacLevels :: Text
     ,angularGaugeDescriptions :: Text
     ,angularGaugeLocations :: Text
     ,angularGaugeParamIds :: Text
    }
   deriving (Read, Show,Eq)

instance FromJSON AngularGaugeConfigObj where
    parseJSON (Object tObj) = AngularGaugeConfigObj <$>
                          tObj .: "width" <*>
                          tObj .: "height" <*>
                          tObj .: "stacking" <*>
                          tObj .: "units" <*>
                          tObj .: "colors" <*>
                          tObj .: "linecolors" <*>
                          tObj .: "lineparams" <*>
                          tObj .: "waclevels" <*>
                          tObj .: "descriptionlist" <*>
                          tObj .: "locationlist" <*>
                          tObj .: "pidlist"

    parseJSON _ = fail "Rule: Expecting Test Object Received, Other"

instance ToJSON AngularGaugeConfigObj where 
    toJSON (AngularGaugeConfigObj {..}) = object
                        [
                         "width" .= angularGaugeWidth
                         ,"height" .= angularGaugeHeight
                         ,"stacking" .= angularGaugeStacking
                         ,"units" .= angularGaugeUnits
                         ,"colors" .= angularGaugeColors
                         ,"linecolors" .= angularGaugeLineColors
                         ,"lineparams" .= angularGaugeLineParams
                         ,"waclevels" .= angularGaugeWacLevels
                         ,"descriptionlist" .= angularGaugeDescriptions
                         ,"locationlist" .= angularGaugeLocations
                         ,"pidlist" .= angularGaugeParamIds
                         ]


-- | A AngularGauge object transformer on a get parameter string

runAngularGaugeConfigObj :: (Text,Text) -> (Text, Value)
runAngularGaugeConfigObj (t,v)
  | t == "width" = (t .=  intVal v)
  | t == "height" = (t .=  intVal v)
  | t == "units" = (t .=  textVal v)
  | t == "stacking" = (t .=  textVal v)
  | t == "colors" = (t .=  textVal v)
  | t == "linecolors" = (t .=  textVal v)
  | t == "lineparams" = (t .=  textVal v)
  | t == "waclevels" = (t .=  textVal v)
  | t == "descriptionlist" = (t .=  textVal v)
  | t == "locationlist" = (t .=  textVal v)
  | t == "pidlist" = (t .=  textVal v)
  | otherwise = (t .= toJSON v)


defaultAGCO :: AngularGaugeConfigObj
defaultAGCO = AngularGaugeConfigObj 250 250 "normal" "" "" "" "" "" "" "" ""
