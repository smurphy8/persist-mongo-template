module Persist.Mongo.SettingsSpec (main, spec) where

-- import Data.List ()
import Persist.Mongo.Settings 
import Database.Persist
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "testFetchLocation" $ do
    it "should access the database for stuff" $ do
      l <- runDB $ selectList [] [Asc DashboardId]
      (null (entityVal `fmap` l)) `shouldBe` False
