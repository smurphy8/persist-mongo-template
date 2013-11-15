{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,DeriveGeneric, MultiParamTypeClasses, FlexibleInstances  #-}
module ContentCfgTypes.TankGaugeConfigObj where

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)

import Control.Applicative ((<$>), (<*>))
import Yesod 

import Data.Text
import ContentCfgTypes.Util


-- TankGauge stuf

data TankGaugeConfigObj =  TankGaugeConfigObj {
      tankGaugeWidth :: Int
     ,tankGaugeHeight :: Int
     ,tankGaugeStacking :: Text
     ,tankGaugeUnits :: Text
     ,tankGaugeColors :: Text
     ,tankGaugeLineColors :: Text
     ,tankGaugeLineParams :: Text
     ,tankGaugeWacLevels :: Text
     ,tankGaugeDescriptions :: Text
     ,tankGaugeLocations :: Text
     ,tankGaugeParamIds :: Text
    }
   deriving (Read, Show,Eq)

instance FromJSON TankGaugeConfigObj where
    parseJSON (Object tObj) = TankGaugeConfigObj <$>
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

instance ToJSON TankGaugeConfigObj where 
    toJSON (TankGaugeConfigObj {..}) = object
                        [
                         "width" .= tankGaugeWidth
                         ,"height" .= tankGaugeHeight
                         ,"stacking" .= tankGaugeStacking
                         ,"units" .= tankGaugeUnits
                         ,"colors" .= tankGaugeColors
                         ,"linecolors" .= tankGaugeLineColors
                         ,"lineparams" .= tankGaugeLineParams
                         ,"waclevels" .= tankGaugeWacLevels
                         ,"descriptionlist" .= tankGaugeDescriptions
                         ,"locationlist" .= tankGaugeLocations
                         ,"pidlist" .= tankGaugeParamIds
                         ]


-- | A TankGauge object transformer on a get parameter string

runTankGaugeConfigObj :: (Text,Text) -> (Text, Value)
runTankGaugeConfigObj (t,v)
  | t == "width" = (t .=  intVal v)
  | t == "height" = (t .=  intVal v)
  | t == "stacking" = (t .=  textVal v)
  | t == "units" = (t .=  textVal v)
  | t == "colors" = (t .=  textVal v)
  | t == "linecolors" = (t .=  textVal v)
  | t == "lineparams" = (t .=  textVal v)
  | t == "waclevels" = (t .=  textVal v)
  | t == "descriptionlist" = (t .=  textVal v)
  | t == "locationlist" = (t .=  textVal v)
  | t == "pidlist" = (t .=  textVal v)
  | otherwise = (t .= toJSON v)


defaultTGCO :: TankGaugeConfigObj
defaultTGCO = TankGaugeConfigObj 1221 400 "normal" "" "" "" "" "" "" "" ""
