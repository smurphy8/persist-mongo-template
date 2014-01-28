{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Mask.BuiltIns where 

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)

import Language.StructuredScript.Parsers
--import Text.Julius
--import qualified Data.Aeson as A



testCompile :: String
testCompile = "tst"

-- | Add your builtIn Functions here and in Mask.hs and in Types.hs




  
divBy10 :: Const -> Either String Const
divBy10 (ConstDouble x) = Right $ ConstDouble $ x / 10.0
divBy10 _ = Left $ "Expected Double"

multBy10 :: Const -> Either String Const
multBy10 (ConstDouble x) = Right $ ConstDouble $ x * 10.0
multBy10 _ = Left $ "Expected Double"

