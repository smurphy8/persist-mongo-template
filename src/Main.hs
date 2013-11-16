{-# LANGUAGE NoImplicitPrelude,OverloadedStrings #-}
module Main where
import Persist.Mongo.Settings
import Prelude (print,($),(.))
import Database.Persist
import Database.Persist.Class
import Control.Applicative ((<$>), (<*>), liftA2, Applicative)

 

main = do 
  res <- runDB $ selectList [] [Asc DashboardId]
  print $ dashboardDefault.entityVal <$> res 
