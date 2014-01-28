{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SafeCopy where 
import Data.Serialize
import Data.SafeCopy
import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Language.StructuredScript.Parsers 
import Data.ByteString
import Mask.Types
--import Text.Julius
--import qualified Data.Aeson as A


instance (Hashable a,SafeCopy a, SafeCopy b, Ord a) => SafeCopy (M.HashMap a b) where
    getCopy = contain $ fmap M.fromList safeGet
    putCopy = contain . safePut . M.toList
    errorTypeName = (\_ -> "Hashmap messed up")



{-| When there are Types that don't really fit 
into Yesod's Persist DB engine, a Safecopy is serialized here.

Then the appropriate thing to do is wrap the serialized Bytestring in a type generated 
in the model file.  

That will allow the information to still maintain some type safety in addition to 
the new version information.
|-}



data TestSafeCopy = TestSafeCopy { unTest :: Int } 
      deriving (Read,Eq,Show)

$(deriveSafeCopy 0 'base ''TestSafeCopy)

testSafeCopyPut :: ByteString
testSafeCopyPut = runPut $ safePut $ TestSafeCopy 3

testSafeCopyGet :: Either String TestSafeCopy  
testSafeCopyGet = runGet safeGet testSafeCopyPut 


-- |Mask Specific
$(deriveSafeCopy 0 'base ''TagType)
$(deriveSafeCopy 0 'base ''TagTarget)
$(deriveSafeCopy 0 'base ''BuiltInId)
$(deriveSafeCopy 0 'base ''UserMask)
$(deriveSafeCopy 0 'base ''MaskData)


-- | Structured Text Specific
$(deriveSafeCopy 0 'base ''Expr)
$(deriveSafeCopy 0 'base ''Stmt)
$(deriveSafeCopy 0 'base ''Duop)
$(deriveSafeCopy 0 'base ''Unop)
$(deriveSafeCopy 0 'base ''Const)
