{-# LANGUAGE NoImplicitPrelude,OverloadedStrings #-}
module Main where
import Persist.Mongo.Settings
import Prelude (($))
import Database.Persist
import Database.Persist.Class

main = runDB $ selectList [] [Asc QuestionnaireId]
