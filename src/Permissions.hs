{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses, DeriveGeneric,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Permissions where 

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)
import Data.Text
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Yesod hiding (runDB)

type UserPermissionsList = [UserPermissions]

type GroupPermissionsList  = [UserPermissions]




data UserPermissions = All | Read | Write | Edit | Delete | None 
   deriving (Show,Read,Eq,Enum,Bounded,Ord,Generic)
permCount :: Int 
permCount = 5


eIList :: [Int]
eIList = []

instance ToJSON UserPermissions where 
    toJSON All    = object [("All" .= eIList)]
    toJSON Read   = object [ ("Read" .= eIList)]
    toJSON Write  = object [ ("Write" .= eIList)]
    toJSON Edit   = object [ ("Edit" .= eIList)]
    toJSON Delete = object [ ("Delete" .= eIList)]
    toJSON None   = object [ ("None" .= eIList)]



listPermissions :: [UserPermissions]
listPermissions = listPermissions' minBound

listPermissions' :: UserPermissions -> [UserPermissions]
listPermissions' x | (x == minBound) = x:(listPermissions' $ succ x)
                   | (x <  maxBound) = x:(listPermissions' $ succ x )
                   | (x == maxBound) = x:[]
                   | otherwise = []
                  

testUPInstance :: Result UserPermissions
testUPInstance = fromJSON (toJSON All) :: Result UserPermissions
 
instance FromJSON UserPermissions where
    parseJSON (Object p) = do       
      let possiblePermissions :: [UserPermissions]
          possiblePermissions = (Prelude.take permCount $ iterate (succ) All)
          possiblePermissionsText :: [Text]
          possiblePermissionsText = (pack.show <$> possiblePermissions)
          parseList :: Text -> Object -> Parser (Maybe Value)
          parseList t o =  (o .:? t) >>= (\x -> return x)       
          parsedPermissions = parseList <$> possiblePermissionsText
      checkPerms <- sequence [ pFcn p | pFcn <- parsedPermissions]
      let paired = possiblePermissions `Prelude.zip` checkPerms
      case [pR | (pR,c) <- paired  , c /= Nothing] of 
        [] -> fail "Rule: Missing Permission"
        x:_ -> return x

    parseJSON (Array  _) = fail "Whoops it was a: Array"
    parseJSON (String _) = fail "Whoops it was a: String"
    parseJSON (Number _) = fail "Whoops it was a: Number"
    parseJSON (Bool   _) = fail "Whoops it was a: Bool"
    parseJSON (Null    ) = fail "Whoops it was a: Null"


      -- checked <- sequence $ permCheckList <*> pure p 

      -- case [fst pc | pc <- pairUpPandC , ( snd pc /= (Nothing) )] of 
      --   [] -> fail "Rule: Expecting a Permission"
      --   x:xs  -> return x
     
      
      
    -- parseJSON = withText "UserPermissions" $ \x -> 
    --                               case (readMay.unpack $ x) of
    --                                 (Just k) -> pure k
    --                                 Nothing -> fail "Rule: Expected Valid User Permissions"
                            


derivePersistField "UserPermissions"

