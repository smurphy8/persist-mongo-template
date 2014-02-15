{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,DeriveGeneric, MultiParamTypeClasses, FlexibleInstances  #-}

module ContentCfgTypes
       ( module ContentCfgTypes
       ) where
import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)
import GHC.Generics
import Control.Applicative ((<$>), (<*>),(<|>))
import Yesod 
--import Text.Julius
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H

import Data.Text

import ContentCfgTypes.Util 



import ContentCfgTypes.TankGaugeConfigObj as ContentCfgTypes
import ContentCfgTypes.SplineConfigObj as ContentCfgTypes
import ContentCfgTypes.TankConfigObj as ContentCfgTypes
import ContentCfgTypes.AngularGaugeConfigObj as ContentCfgTypes
import ContentCfgTypes.AutoReportConfigObj as ContentCfgTypes 
import ContentCfgTypes.StatusConfigObj as ContentCfgTypes
import ContentCfgTypes.ParameterHistoryConfigObj as ContentCfgTypes
import ContentCfgTypes.MultiParameterHistoryReportConfigObj as ContentCfgTypes
import ContentCfgTypes.MultiParameterHistoryConfigObj as ContentCfgTypes
import ContentCfgTypes.TableByMultiLocConfigObj as ContentCfgTypes
import ContentCfgTypes.TableByLocConfigObj as ContentCfgTypes
import ContentCfgTypes.CustomTableConfigObj as ContentCfgTypes
import ContentCfgTypes.CustomTableIdConfigObj as ContentCfgTypes
import ContentCfgTypes.RollingReportConfigIdObj as ContentCfgTypes
--import Yesod

{-| Content Types are the configuration object and helpers for them 
    for all the different content widgets possible in onping2.0.

    Because they are defined for use in a database context they have to be strictly 
    defined.  
|-} 
-- MUST BE THE LAST LINE!!!!

-- | Every ContentConfig type has to have a constructor here 
-- This is what is put into the content array

type MaybeConfig a = Maybe (A.Result a)


data ContentConfig = AutoReportConfig NullConfigObj
                   | TankGaugeConfig !TankGaugeConfigObj
                   | SplineConfig !SplineConfigObj
                   | AngularGaugeConfig !AngularGaugeConfigObj
                   | StatusConfig !StatusConfigObj
                   | EditMode !EditModeObj
                   | TableByMultiLocConfig !TableByMultiLocConfigObj
                   | TableByLocConfig !TableByLocConfigObj
                   | AutoTableByLocationConfig NullConfigObj
                   | CustomTableConfig !CustomTableConfigObj
                   | CustomTableIdConfig !CustomTableIdConfigObj
                   | RollingReportIdConfig !RollingReportConfigIdObj
                     deriving (Read,Show,Eq,Generic) 





data NullConfigObj = NullConfigObj
  deriving (Read,Show,Eq)

instance FromJSON NullConfigObj where
  parseJSON (Object tObj)
   |H.null tObj = return NullConfigObj
   |otherwise = fail "Expected empty Object"
  parseJSON _ = fail "Expected empty Object recieved other"

instance ToJSON NullConfigObj where 
  toJSON NullConfigObj = A.Object H.empty 




-- parseContentList :: Text-> Object -> Parser (Maybe Value)




instance FromJSON ContentConfig where
     parseJSON  (Object o) = do 
       ((o .: "AutoReportConfig") >>= (\x -> return $ (AutoReportConfig x))) 
       <|> ((o .: "TankGaugeConfig") >>= (\x -> return $ (TankGaugeConfig x)))
       <|> ((o .: "SplineConfig") >>= (\x -> return $ (SplineConfig x)))
       <|> ((o .: "AngularGaugeConfig") >>= (\x -> return $ (AngularGaugeConfig x)))
       <|> ((o .: "StatusConfig") >>= (\x -> return $ (StatusConfig x)))
       <|> ((o .: "EditMode") >>= (\x -> return $ (EditMode x)))
       <|> ((o .: "TableByMultiLocConfig") >>= (\x -> return $ (TableByMultiLocConfig x)))
       <|> ((o .: "TableByLocConfig") >>= (\x -> return $ (TableByLocConfig x)))
       <|> ((o .: "AutoTableByLocationConfig") >>= (\x -> return $ AutoTableByLocationConfig x))
       <|> ((o .: "CustomTableConfig") >>= (\x -> return $ CustomTableConfig x))
       <|> ((o .: "CustomTableIdConfig") >>= (\x -> return $ CustomTableIdConfig x))
       <|> ((o .: "RollingReportIdConfig") >>= (\x -> return $ RollingReportIdConfig x))
       <|> fail "Could not parse ContentConfig"
     parseJSON (Array  _) = fail "Whoops it was a: Array"
     parseJSON (String _) = fail "Whoops it was a: String"
     parseJSON (Number _) = fail "Whoops it was a: Number"
     parseJSON (Bool   _) = fail "Whoops it was a: Bool"
     parseJSON (Null    ) = fail "Whoops it was a: Null"
      

instance ToJSON ContentConfig where
    toJSON (AutoReportConfig       x) = object [ ("AutoReportConfig"          .= x)]
    toJSON (TankGaugeConfig        x) = object [ ("TankGaugeConfig"           .= x)] 
    toJSON (SplineConfig           x) = object [ ("SplineConfig"              .= x)] 
    toJSON (AngularGaugeConfig     x) = object [ ("AngularGaugeConfig"        .= x)] 
    toJSON (StatusConfig           x) = object [ ("StatusConfig"              .= x)] 
    toJSON (EditMode               x) = object [ ("EditMode"                  .= x)] 
    toJSON (TableByMultiLocConfig  x) = object [ ("TableByMultiLocConfig"     .= x)] 
    toJSON (TableByLocConfig       x) = object [ ("TableByLocConfig"          .= x)]
    toJSON (AutoTableByLocationConfig x)  = object [ ("AutoTableByLocationConfig" .= x)]
    toJSON (CustomTableConfig      x) = object [ ("CustomTableConfig"         .= x)]
    toJSON (CustomTableIdConfig x) = object [ ("CustomTableIdConfig"          .= x)]
    toJSON (RollingReportIdConfig x) = object [ ("RollingReportIdConfig"          .= x)]


-- | EditMode is a special config object that is available in any widget to be picked up
--  it allows the toggling of the wrench config options to be communicated to the internal
-- representation of the widget

data EditModeObj = EditModeObj { cfgMode :: Bool }
  deriving (Read,Show,Eq)

-- | using hand derive for clarity 
instance (FromJSON EditModeObj) where
  parseJSON (Object tObj) = EditModeObj <$> tObj .: "cfgMode"
  parseJSON _ = fail "Rule: Expecting Edit Mode Object Received, Other"


instance ToJSON EditModeObj where
  toJSON (EditModeObj {..} ) = object [ "cfgMode" .= cfgMode]



runEditModeObj :: (Text,Text) -> (Text,Value)
runEditModeObj (t,v)
  | t == "cfgMode" = (t .= boolVal v)
  | otherwise = (t .= toJSON v) 

-- | Generated JSON example: 

-- |Make this type accessible in the Database

derivePersistField "ContentConfig"





              
lookupContent ::(A.FromJSON a , MonadHandler m) => ( (Text,Text)  -> (Text,Value) ) -> m (Maybe (A.Result a))
lookupContent objTransformer = do 
  req <- getRequest
  case reqGetParams req of 
    [] -> return Nothing
    lst -> return $ Just (A.fromJSON.object $ objTransformer <$> lst )


{-| Lookup content will take a list of values and parse them
    with a transformer into the right values.  Then will map them
    into a JSON Object.
    To do this the final Data type must be an Instance of FromJSON and
    the transormer provided must convert (Text,Text) into a (Text,Val) type
    according to whatever Transformation rule you want |-}

--Example stuff
 
data TestObj =  TestObj { testWidth :: Int
                          ,testTitle :: Text 
                          ,testStep :: Int 
                        }
             deriving (Read, Show,Eq)

instance FromJSON TestObj where 
    parseJSON (Object tObj) = TestObj <$> 
                          tObj .: "width" <*> 
                          tObj .: "title" <*> 
                          tObj .: "step" 

    parseJSON _ = fail "Rule: Expecting Test Object Received, Other"

instance ToJSON TestObj where 
    toJSON (TestObj {..}) = object 
                        [ 
                         "width" .= testWidth 
                         ,"step" .= testStep
                         ,"title" .= testTitle 
                         ]



-- | An example transformer on a get parameter string

exTransformObj :: (Text,Text) -> (Text, Value)
exTransformObj (t,v)
  | t == "width" = (t .= intVal v)
  | t == "title" = (t .= textVal v)
  | otherwise = (t .= toJSON v)

