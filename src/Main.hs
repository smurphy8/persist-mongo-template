{-# LANGUAGE NoImplicitPrelude,OverloadedStrings #-}
module Main where
import Persist.Mongo.Settings
import Prelude (print,($),(.), (()), IO )
import Database.Persist
-- import Database.Persist.Class

import Data.Either
import Control.Applicative ((<$>))

 
main :: IO () 
main = do 
  res <- runDB $ selectList [] [Asc DashboardId]
  print $ dashboardDefault.entityVal <$> res
  mConf <- readDBConf "config.yml"
  case mConf of
  	(Left s) -> print s
  	(Right conf) -> do
  		newRes <- runDBConf conf $ selectList [] [Asc DashboardId]
  		print $ dashboardDefault . entityVal <$> newRes
