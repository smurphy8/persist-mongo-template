{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,DeriveGeneric, MultiParamTypeClasses, FlexibleInstances  #-}
module ContentCfgTypes.CustomTableConfigObj where

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)
import Control.Applicative ((<$>), (<*>))
import Yesod
import Data.Text
import ContentCfgTypes.Util
import GHC.Generics

data CTCell =  CPid CellPid
              | CText Text
      deriving (Read, Show, Generic, Eq)
instance FromJSON CTCell where
instance ToJSON CTCell where


data CellPid = CellPid { cPid :: Int }
    deriving(Read, Show, Eq)
instance FromJSON CellPid where
  parseJSON (Object cObj) = CellPid <$>
                        cObj .: "pid"
  parseJSON _ = fail "Rule: Expecting CellPid object received other"

instance ToJSON CellPid where
  toJSON (CellPid {..}) = object
              [
                "pid" .= cPid
              ]

data CustomTableConfigObj =  CustomTableConfigObj { 
     cTableTitle :: Text
     ,cTableHeaders :: [Text]
     ,cTableCells :: [[CTCell]]
    }
   deriving (Read, Show,Eq)


instance FromJSON CustomTableConfigObj where 
    parseJSON (Object cObj) = CustomTableConfigObj <$>  
                          cObj .: "title"  <*>
                          cObj .: "headers" <*>
                          cObj .: "cells"
    parseJSON _ = fail "Rule: Expecting Table Config Object Received, Other"

instance ToJSON CustomTableConfigObj where 
    toJSON (CustomTableConfigObj {..}) = object 
                        [ 
                         "title"  .= cTableTitle
                         ,"headers" .= cTableHeaders
                         ,"cells" .= cTableCells
                         ]


-- | A Custom Table object transformer on a get parameter string

runCustomTableConfigObj :: (Text,Text) -> (Text, Value)
runCustomTableConfigObj (t,v) 
  | t == "title"  = (t .= textVal v)
  | t == "headers" = (t .=  textVal v)
  | t == "cells" = (t .= textVal v)
  | otherwise = (t .= toJSON v)

--defaultCTO :: CustomTableConfigObj
--defaultCTO = CustomTableConfigObj "Title" ["Column 1", "Column 2","Column 3","Column 4"] [[CText "Cell 1,1",CText "Cell 1,2",CText "Cell 1,3"],[CPid (CellPid 299),CPid (CellPid 299),CPid (CellPid 300)],[CText "Cell 3,1",CText "Cell 3,2",CText "Cell 3,3"],[CText "Cell 4,1",CText "Cell 4,2",CText "Cell 4,3"]]