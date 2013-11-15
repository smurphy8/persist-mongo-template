{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             MultiParamTypeClasses, FlexibleInstances  #-}

module WidgetTypes where

import Prelude 
import Data.Aeson ((.:?),(.!=))
import Control.Applicative ( (<$>),(<*>))

import Data.Text
import Yesod

-- | Time Conventions

timeFormat::Text
timeFormat="%Y_%m_%d_%H_%M_%S%z"

inputTimeFormat :: Text
inputTimeFormat="%Y_%m_%d_%H_%M_%Q"

stdTimeFormat :: Text
stdTimeFormat = "\"%Y-%m-%dT%H:%M:%S%QZ\"" 

stdTimeFormat' :: Text
stdTimeFormat' = "%Y-%m-%dT%H:%M:%S%QZ" 


-- | Logo Convention
data Logo = Logo {
      logoStaticRoute :: Text
    , logoString :: String
    }
  deriving (Read,Show,Eq)
instance FromJSON Logo where
    parseJSON (Object l) = Logo <$> 
                           (l .: "route")  <*>
                           (l .: "string") 

    parseJSON _ = fail "Rule: Expected Object Logo"


data MenuElement a = MenuElement a
  deriving (Read,Show,Eq) 

type HeaderMenuElements  = String

data HeaderWidget = HeaderWidget {
      headerWidgetLogo :: Logo
     , headerMenuElements :: HeaderMenuElements
    }
  deriving (Read,Show,Eq)


instance FromJSON HeaderWidget where 
    parseJSON (Object h) = HeaderWidget <$>
                           (h .: "route") <*>
                           (h .: "hme")   
    parseJSON Null = fail "Rule: Expecting Object HeaderWidget, Received Null"
    parseJSON _ = fail "Rule: Expecting Object HeaderWidget, Received Other"




type BranchIndex = [Int]
data MConfigObject = 
    MConfigObject { -- The edit field should be added on client side??
              mConfigIcon :: Text 
              ,mConfigPanelType :: Text
              ,mConfigText :: Text
    }
    deriving (Read,Show,Eq)


-- | MConfig Definitions for JSON Parsing
instance ToJSON MConfigObject where
    toJSON (MConfigObject {..}) = object 
                              [
                               "icon" .= mConfigIcon
                             , "panelType" .= mConfigPanelType
                             , "text" .= mConfigText
                              ]

instance FromJSON MConfigObject where 
    parseJSON (Object p ) = MConfigObject <$>
                            p .: "icon"      <*>
                            p .:? "panelType" .!= "Add"<*>
                            p .: "text"      
    parseJSON Null = fail "Rule: Expecting Object MConfig, recieved Null"
     -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = fail "Rule: Expecting Object Mconfig recieved other "
-- ==================================================

-- |Not permissions that Users have but instead permissions others have on a user

derivePersistField "HeaderWidget"

derivePersistField "MConfigObject"

{-| Content Types and config settings 
 |-}




type ContentProperties = [(Text,Text)]
type CArr = [Int] -- This must get turned into ContentWidgetId or something


-- | routines to get Onping Models instead of just lookupGetParams over and over

