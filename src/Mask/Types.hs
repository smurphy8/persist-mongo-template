{-# LANGUAGE BangPatterns,RankNTypes,OverloadedStrings #-}
{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,DeriveGeneric,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Mask.Types where 

import Prelude hiding (head, init, last
                      ,readFile, tail, writeFile)


import GHC.Generics
import qualified Data.HashMap.Strict as M
import qualified Data.Hashable as H
import Language.StructuredScript.Parsers 
import Yesod 
--import Text.Julius
--import qualified Data.Aeson as A
import Data.Text
import Data.String (IsString)

testCompile :: String
testCompile = "tst"

-- | /mask/editor MaskEditorR T
-- | CR -> "MaskEditorCR"


-- | Pick either a Tag result from Onping Tag History
-- | Or OnpingTagCombined

data TagType = TagHistory | TagCombined
  deriving (Show,Eq)
           
-- | TargetId is left as a generic Type variable in case I decide the best
--   way to store it is by storing the query itself (the middle part of a selectFirst or selectList)
data TagTarget a = TagTarget { targetId :: a , targetType :: TagType}
    deriving (Show, Eq)

-- | This is the input map which is keyed by whatever is indexing
-- It is transformed into the destination target by TagTransform
-- Tag transform will represent a class of functions that can
-- take a TagImap to some f (varTable)

type TagIMap a = (Show a, Eq a) => M.HashMap Text (TagTarget  a)


sampleTagIMap :: forall k a.
                       (Eq k, Num a, Data.String.IsString k, H.Hashable k) =>
                       M.HashMap k (TagTarget a)
sampleTagIMap = M.insert "input1" (TagTarget 299 TagHistory) M.empty


-- |User Mask is envisioned to work like this on run ...
{-| get The 'UserMaskID', and Retrieve this type
    if the 'VarTable' is empty build a new VTable with the
    'TagTarget' , using a 'TagTransform' Function
    Then, a function is constructed that takes input
    and runs it against the VarTable and the Stmt Tree
   This is the output of the Tag Transform and can then be applicatively
   mapped with your input parameter.
   Some Notes,
   * All Parameters BUT THE FIRST are looked up using OnpingTagCombined
     +This may change for now but it is currently beyond the scope
      of SST to handle mapping and pairing times with inputs
   
    
|-}
  
data UserMask = UserMask {   getStmtTree :: Stmt ,
                             getInputLookups :: M.HashMap Text (TagTarget Int)
                           }  -- ^ TagIMap Int
    deriving (Eq,Show)

emptyUserMask :: UserMask
emptyUserMask = UserMask Nop M.empty

-- (TagTargetTransform a b)               
-- (TagTarget a -> TagTargetTransform a b)
-- type TagTargetTransform a b = forall m .(Monad m, Functor m) => TagTarget a ->  ( b -> m b)
                       
data MaskFcn      = OneVar  (Const -> Either String Const)
                  | UserDef (Const -> Either String Const)  -- Tag


-- | Add your builtIn Functions here and in Mask.hs and in Mask.hs
-- | Always leave UserDefined as the last in BuiltInId type so the getBuilInIdR can strip it off.
data BuiltInId = DivByTen | UserDefined
    deriving (Eq,Show,Ord,Bounded,Enum,Generic)


-- | Add Built In Masks that you have created to this list so they may be created in 
listBuiltInMasks :: [BuiltInId]
listBuiltInMasks = [DivByTen]


instance ToJSON BuiltInId
instance FromJSON BuiltInId


-- | Stored in the DB as MaskDataStore
data MaskData = MaskData { getBuiltInId :: BuiltInId , userMask :: UserMask}
                    deriving (Eq,Show)
